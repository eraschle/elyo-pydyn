;;; elyo-convert.el --- Dynamo BIM package -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides functionality to change Python code in a Dynamo file.
;;
;;; Code:
(require 'elyo-json)
(require 'elyo-path)
(require 'elyo-utils)

(require 'evil-lion)

(defvar-local node-uuid nil "Unique node id in the source file.")
(put 'node-uuid 'safe-local-variable (lambda (_) t))

(defvar-local node-engine nil "Python engine to use for code.")
(put 'node-engine 'safe-local-variable (lambda (_) t))

(defvar-local node-path nil "Source path of the source file.")
(put 'node-path 'safe-local-variable (lambda (_) t))

(defvar-local node-os nil "Line ending in python code.")
(put 'node-os 'safe-local-variable (lambda (_) t))

(defvar-local node-coding nil "Coding system of the file.")
(put 'node-coding 'safe-local-variable (lambda (_) t))


(defcustom elyo-convert-py2-engine  "IronPython2"
  "Extension of a python code file."
  :type 'string
  :group 'elyo-dynpy)


(defcustom elyo-convert-py3-engine "CPython3"
  "Extension of a python code file."
  :type 'string
  :group 'elyo-dynpy)


(defvar :dynamo (make-symbol "dynamo")
  "Symbol for value in Dynamo file.")
(defvar :dyn-idx (make-symbol "dyn-idx")
  "Symbol for value in Dynamo file.")
(defvar :python (make-symbol "python")
  "Symbol for value in Python file.")
(defvar :py-idx (make-symbol "py-idx")
  "Symbol for value in Python file.")
(defvar :py-short (make-symbol "py-short")
  "Symbol for python abbrev.")
(defvar :py-engine (make-symbol "py-engine")
  "Symbol for python abbrev.")
(defvar :os (make-symbol "os")
  "Symbol for value in Python file.")


(defvar elyo--convert-code-map (list (list :dynamo "\\\\\\\\" :dyn-idx 0 :python "\\\\"             :py-idx 0)
                                     (list :dynamo "\\\""     :dyn-idx 3 :python "\""               :py-idx 4)
                                     (list :dynamo "\\t"      :dyn-idx 1 :python "\t"               :py-idx 1 :py-short "py2" :py-engine "IronPython2")
                                     (list :dynamo "\\t"      :dyn-idx 2 :python (make-string 4 ? ) :py-idx 2 :py-short "py3" :py-engine "CPython3")
                                     (list :dynamo "\\r\\n"   :dyn-idx 4 :python "\n"               :py-idx 5 :os "linux")
                                     (list :dynamo "\\n"      :dyn-idx 5 :python "\n"               :py-idx 6 :os "dos")
                                     (list :dynamo "\\\\n"    :dyn-idx 6 :python "\\n"              :py-idx 3))
  "List with values to transfer code between Emacs und Dynamo.")


(defun elyo-switch-to-source ()
  "Switch to source file unless `node-path' is nil."
  (switch-to-buffer (elyo-buffer-by node-path)))


(defun elyo-is-py2-engine? (&optional engine)
  "Return non-nil ENGINE is for python 2.x engine."
  (let ((engine (or engine node-engine)))
    (and engine (string-equal engine elyo-convert-py2-engine))))


(defun elyo-is-py3-engine? (&optional engine)
  "Return non-nil ENGINE is for CPython 3.x engine."
  (let ((engine (or engine node-engine)))
    (and engine (string-equal engine elyo-convert-py3-engine))))


(defun elyo--convert-map-by (property &optional value)
  "Return convert-map with existing PROPERTY and equal VALUE if VALUE non-nil."
  (catch 'found-it
    (dolist (convert-map elyo--convert-code-map)
      (when (plist-member convert-map property)
        (unless value
          (throw 'found-it convert-map))
        (when (string-equal value (plist-get convert-map property))
          (throw 'found-it convert-map))))))


(defun elyo--convert (convert-map node-info from-sym to-sym)
  "Return code from NODE-INFO after replace FROM-SYM with TO-SYM from CONVERT-MAP.
Check if there is no other \\ before match string of last search."
  (with-temp-buffer
    (insert (plist-get node-info :code))
    (goto-char (point-min))
    (while (search-forward (plist-get convert-map from-sym) (point-max) 't)
      (if (and (or (not (char-before (match-beginning 0)))
                   (not (char-equal ?\\ (char-before (match-beginning 0)))))
               (string-equal (elyo--buffer-substring (match-beginning 0) (match-end 0))
                             (plist-get convert-map from-sym)))
          (replace-string-in-region (plist-get convert-map from-sym)
                                    (plist-get convert-map to-sym)
                                    (match-beginning 0) (match-end 0)))
      (goto-char (match-beginning 0))
      (forward-char (seq-length (plist-get convert-map to-sym))))
    (buffer-string)))


(defun elyo--convert-new-line (convert-map node-info from-sym to-sym)
  "Change code from NODE-INFO with FROM-SYM and TO-SYM if os exist in CONVERT-MAP."
  (when (plist-member convert-map :os)
    (when (or (and node-os (string-equal (plist-get convert-map :os) node-os)) ;; Py to Dyn > check local var
              (and (not node-os) (s-contains-p (plist-get convert-map from-sym)
                                               (plist-get node-info :code))))  ;; Dyn to Py > check code
      (unless (plist-member node-info :os)
        (plist-put node-info :os (plist-get convert-map :os)))   ;; Dyn to Py > Store value for later use
      (plist-put node-info :code (elyo--convert convert-map node-info
                                                from-sym to-sym)))))


(defun elyo--convert-engine (convert-map node-info from-sym to-sym)
  "Change code of NODE-INFO with FROM-SYM & TO-SYM CONVERT-MAP if is non-nil."
  (when (plist-member convert-map :py-engine)
    (let ((engine (plist-get convert-map :py-engine))
          (src-engine (plist-get node-info :engine)))
      (when (and src-engine (s-equals? engine src-engine))
        (plist-put node-info :code (elyo--convert convert-map node-info
                                                  from-sym to-sym))))))


(defun elyo--convert-others (convert-map node-info from-sym to-sym)
  "Convert code with NODE-INFO, FROM-SYM, TO-SYM and CONVERT-MAP if NOT engine or os."
  (unless (or (plist-member convert-map :py-engine)
              (plist-member convert-map :os))
    (plist-put node-info :code (elyo--convert convert-map node-info from-sym to-sym))))


(defun elyo--convert-sorted-map (sort-key)
  "Sort `elyo--convert-code-map' with SORT-KEY."
  (seq-sort (lambda (elem other)
              (< (plist-get elem sort-key) (plist-get other sort-key)))
            elyo--convert-code-map))


(defun elyo--convert-to-python (node-info)
  "Convert code from dynamo to python and store it in NODE-INFO."
  (dolist (conv-map (elyo--convert-sorted-map :dyn-idx))
    (dolist (callback (list 'elyo--convert-new-line
                            'elyo--convert-engine
                            'elyo--convert-others))
      (funcall callback conv-map node-info :dynamo :python)))
  (plist-get node-info :code))


(defun elyo--convert-to-dynamo (node-info)
  "Convert code from python to dynamo and store it in NODE-INFO."
  (dolist (conv-map (elyo--convert-sorted-map :py-idx))
    (dolist (callback (list 'elyo--convert-new-line
                            'elyo--convert-engine
                            'elyo--convert-others))
      (funcall callback conv-map node-info :python :dynamo)))
  (plist-get node-info :code))


(defun elyo-convert-export-path (node-info)
  "Return export python file path for NODE-INFO."
  (concat (elyo-path-export-create-of (plist-get node-info :path))
          (elyo-path-export-file-name-for
           node-info elyo--convert-code-map)))


(defun elyo-convert-export-buffer (node-info)
  "Return export buffer for NODE-INFO."
  (elyo-buffer-by (elyo-convert-export-path node-info)))


(defun elyo--convert-delete-line ()
  "Return current line in the current buffer if search has found something."
  (save-excursion
    (delete-line)))


(defun elyo--convert-delete-in-python ()
  "Search for value in code and this delete line."
  (dolist (search-for (list "# Load the Python Standard"
                            "Phython-Standard- und DesignScript-Bibliotheken laden"
                            "# The inputs to this node will be stored"
                            "Die Eingaben für diesen Block werden in Form einer Liste in den IN-Variablen gespeichert."
                            "dataEnteringNode = IN"
                            "Assign your output to the OUT variable."
                            "Weisen Sie Ihre Ausgabe der OUT-Variablen zu."))
    (elyo--while-search search-for 'elyo--convert-delete-line)))


(defun elyo-convert-type-ignore-add ()
  "Execute all add type-ignore functions."
  (dolist (regex (list elyo-dynamo-input-regex
                       "import [a-zA-z ,]*Enum"
                       "UnwrapElement(.*)"
                       "clr.Reference\[[a-zA-Z]+\]\(\)"
                       "List\[[a-zA-Z]+\]\(.*\)"))
    (elyo--while-regex regex 'elyo-python-ignore-add))
  (dolist (search-for (list "dataEnteringNode = IN"
                            "clr.ImportExtensions(Revit.Elements)"
                            "clr.ImportExtensions(Revit.GeometryConversion)"
                            "Application.DocumentManager.MdiActiveDocument"
                            "TransactionManager.Instance."
                            "LabelUtils.GetLabelFor"
                            "basestring"))
    (elyo--while-search search-for 'elyo-python-ignore-add)))


(defun elyo--convert-code-trim-lines ()
  "Add type ignore to known errors in Dynamo python code."
  (goto-char (point-min))
  (while (not (eobp))
    (unless (string-empty-p (elyo--current-line))
      (replace-string-in-region (elyo--current-line)
                                (string-trim-right (elyo--current-line))
                                (pos-bol) (pos-eol)))
    (forward-line 1)))


(defun elyo-convert-local-var? ()
  "Return non-nil if buffer-local line exist."
  ;; # -*- elyo-convert-beg: 12716;
  (s-starts-with-p "# -*-" (string-trim-left (elyo--buffer-substring (point-min) 10))))


(defun elyo--convert-set-local-vars (node-info)
  "Add file local variable with value of NODE-INFO in current.buffer."
  (goto-char (point-min))
  (unless (elyo-convert-local-var?)
    (open-line 1))
  (add-file-local-variable-prop-line 'node-coding 'utf-8-dos)
  (add-file-local-variable-prop-line 'node-uuid   (plist-get node-info :node-id))
  (add-file-local-variable-prop-line 'node-engine (plist-get node-info :engine))
  (add-file-local-variable-prop-line 'node-os     (plist-get node-info :os ))
  (add-file-local-variable-prop-line 'node-path   (plist-get node-info :path)))


(defun elyo--convert-buffer-local-set (node-info)
  "Set buffer-local variable with value of NODE-INFO in current.buffer."
  (setq node-coding 'utf-8-dos)
  (setq node-uuid   (plist-get node-info :node-id))
  (setq node-engine (plist-get node-info :engine))
  (setq node-os     (plist-get node-info :os))
  (setq node-path   (plist-get node-info :path)))


(defun elyo--convert-code-add-to (node-info)
  "Clear, convert code based on NEW-LINE add code from NODE-INFO in current buffer."
  (let ((code (elyo--convert-to-python node-info)))
    (delete-region (point-min) (point-max))
    (insert code)))


(defun elyo-convert-buffer (node-info)
  "Create python file with NODE-INFO."
  (let ((buffer (elyo-convert-export-buffer node-info)))
    (set-buffer buffer)
    (elyo--convert-code-add-to node-info)
    (elyo--convert-buffer-local-set node-info)
    (elyo--convert-set-local-vars node-info)
    (elyo--convert-delete-in-python)
    (elyo--convert-code-trim-lines)
    (elyo-convert-type-ignore-add)
    (goto-char (point-min))
    (save-buffer 0)))


(defun elyo-convert-to-python (file-path &optional kill-buffer)
  "Return converted code from nodes in FILE-PATH, if KILL-BUFFER is non-nil buffer."
  (let ((node-infos (elyo-json-python-infos-in file-path)))
    (unless (seq-empty-p node-infos)
      (dolist (node-info node-infos)
        (elyo-convert-buffer node-info)
        (if kill-buffer (kill-buffer))))))


(defun elyo-convert-code-info (code)
  "Return plist with Pyhton CODE to convert into Dynamo."
  (list :code code
        :engine node-engine
        :os node-os))


(defun elyo--convert-code (code engine)
  "Return CODE with indent-char based on ENGINE."
  (with-temp-buffer
    (insert code)
    (if (elyo-is-py2-engine? engine)
        (elyo-buffer-tabify)
      (elyo-buffer-untabify))
    (buffer-string)))


(defun elyo-convert-py-to-dyn (code)
  "Return python code from CODE to convert it to Dynamo."
  (let ((code-info (elyo-convert-code-info
                    (elyo--convert-code code node-engine)))
        (convert-map (elyo--convert-map-by :os node-os)))
    (plist-put code-info :code
               (string-trim (elyo--convert-to-dynamo code-info)
                            (format "[%s]+" (plist-get convert-map :dynamo))
                            (format "[%s]+" (plist-get convert-map :dynamo))))
    (plist-get code-info :code)))


(defun elyo-convert-to-dynamo (code)
  "Convert the given CODE from Python to Dynamo."
  (elyo-convert-py-to-dyn code))


(defun elyo-convert-python-to-dynamo (code)
  "Replace python CODE in source Dynamo Python NODE."
  (when (elyo-is-dynamo? node-path)
    (let ((code (elyo-convert-py-to-dyn code))
          (node-id node-uuid))
      (set-buffer (elyo-buffer-by node-path))
      (elyo-json-code-replace node-id code))))


(defun elyo-convert-node-name ()
  "Return dynamo node name of current python code."
  (when (elyo-is-dynamo? node-path)
    (let ((node-id node-uuid))
      (with-current-buffer (elyo-buffer-by node-path)
        (plist-get (elyo-node-plist-by node-id) :name)))))


(defun elyo-convert-node-rename (new-name)
  "Rename dynamo node of current python code to NEW-NAME."
  (when (elyo-is-dynamo? node-path)
    (let ((node-id node-uuid))
      (set-buffer (elyo-buffer-by node-path))
      (elyo-json-node-rename node-id new-name))))


(defun elyo-convert-node-geometry-set (show)
  "Set show geometry for node of current buffer to SHOW."
  (when (elyo-is-dynamo? node-path)
    (let ((node-id node-uuid))
      (set-buffer (elyo-buffer-by node-path))
      (elyo-json-node-geometry-set node-id show))))


(defun elyo-convert-goto-code (code)
  "Select CODE in source and move cursor as far as possible to to left."
  (let ((node-id node-uuid))
    (with-current-buffer (elyo-buffer-by node-path)
      (elyo-json-select-code-of node-id code)
      (elyo--cursor-to-left-border))))


(defun elyo-convert-is-custom? ()
  "Return non-nil python is from Dynamo Custom Node."
  (and node-path (elyo-is-dynamo-custom? node-path)))


(defun elyo-convert-dynamo-file-info ()
  "Return file info of source Dynamo Script or Custom Node file."
  (elyo-json-file-info node-path))


(provide 'elyo-convert)
;;; elyo-convert.el ends here
