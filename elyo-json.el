;;; elyo-json.el -- Dynamo BIM package -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides functionality to change Python code in a Dynamo file.
;;
;;; Code:
(require 'elyo-utils)

(require 'json)
(require 'smartparens)

(defun elyo--search-value (value &optional key)
  "Return search string for VALUE to find and ':' if KEY is non-nil."
  (if key (format "\"%s\":" value) (format "\"%s\"" value)))

(defun elyo--json-object-read ()
  "Return json-object at point or nil if an error occurs."
  (save-excursion
    (sp-beginning-of-sexp)
    (let ((json-object-type 'hash-table)
          (json-array-type  'alist)
          (json-key-type    'string)
          (content nil))
      (condition-case ex
          (setq content (json-read-object))
        ('error
         (message (format "JSON read error %s (%s)" buffer-file-name ex))))
      content)))

(defun elyo--script-node-? (node)
  "Return non-nil if NODE is python script node."
  (and (string-equal (gethash "ConcreteType" node)
                     "PythonNodeModels.PythonNode, PythonNodeModels")
       (string-equal (gethash "NodeType" node)
                     "PythonScriptNode")))

(defun elyo--goto-key-at-point (key-name)
  "Move to KEY-NAME start at current point."
  (save-restriction
    (beginning-of-line 0)
    (sp-narrow-to-sexp 1)
    (goto-char (point-min))
    (search-forward (elyo--search-value key-name t) nil nil 1)
    (goto-char (match-end 0))))

(defun elyo--key-value-start (key-name)
  "Return start position of value from KEY-NAME."
  (save-excursion
    (elyo--goto-key-at-point key-name)
    (+ (point) 2)))

(defun elyo--key-bool-start (key-name)
  "Return start position of BOOLEAN value from KEY-NAME."
  (1- (elyo--key-value-start key-name)))

(defun elyo--key-value-end (key-name)
  "Return end position of value from KEY-NAME."
  (save-excursion
    (elyo--goto-key-at-point key-name)
    (- (pos-eol) 2)))

(defun elyo--key-bool-end (key-name)
  "Return end position of BOOLEAN value from KEY-NAME."
  (1+ (elyo--key-value-end key-name)))

(defun elyo--key-bool-value (bool-value)
  "Return non-nil if BOOL-VALUE is true."
  (if (string-equal "true" bool-value) t nil))

(defun elyo--json-narrow-key (json-key)
  "Narrow buffer to content of JSON-KEY."
  (goto-char (point-min))
  (search-forward (elyo--search-value json-key t) nil t 1)
  ;; (goto-char (match-end 0))
  (forward-line 1)
  (sp-narrow-to-sexp 1))

(defun elyo--json-object-goto (node-id json-key)
  "Move point to NODE-ID in JSON-KEY."
  (when (buffer-narrowed-p)
    (widen))
  (elyo--json-narrow-key json-key)
  (goto-char (point-min))
  (search-forward (elyo--search-value node-id) nil nil nil)
  (beginning-of-line))

(defun elyo--json-node-view-of (node-id)
  "Move point to NODE-ID in NodeViews."
  (elyo--json-object-goto node-id "NodeViews"))

(defun elyo--json-node-view-object-by (node-id)
  "Return the json-object of NODE-ID in NodeViews."
  (save-restriction
    (elyo--json-node-view-of node-id)
    (elyo--json-object-read)))

(defun elyo--json-node-name-by (node-id)
  "Return node name node the given NODE-ID."
  (save-restriction
    (save-excursion
      (let ((node-info (elyo--json-node-view-object-by node-id)))
        (if (or (not (gethash "Name" node-info)) (string-blank-p (gethash "Name" node-info)))
            "NO_NODE_NAME"
          (gethash "Name" node-info))))))

(defvar :path (make-symbol "path")
  "PATH property of node PLIST.")
(defvar :node-id (make-symbol "node-id")
  "NODE-ID property of node PLIST.")
(defvar :name (make-symbol "name")
  "NAME property of node PLIST.")
(defvar :code (make-symbol "code")
  "CODE property of node PLIST.")
(defvar :start (make-symbol "code-start")
  "CODE-START property of node PLIST.")
(defvar :end (make-symbol "code-end")
  "CODE-END property of node PLIST.")
(defvar :engine (make-symbol "engine")
  "ENGINE property of node PLIST.")

(defvar :is-custom (make-symbol "is-custom")
  "IS_CUSTOM property of node PLIST.")
(defvar :category (make-symbol "category")
  "CATEGORY property of node PLIST.")


(defun elyo--node-plist (node)
  "Return NODE information as a PLIST."
  (save-excursion
    (let ((node-id (gethash "Id" node))
          (start (elyo--key-value-start "Code"))
          (end (elyo--key-value-end "Code")))
      (list :node-id node-id
            :name (elyo--json-node-name-by node-id)
            :code (elyo--buffer-substring start end)
            :start start
            :end end
            :engine (gethash "Engine" node "IronPython2")
            :path (buffer-file-name)))))


(defun elyo-node-plist-by (node-id)
  "Return node info by NODE-ID or nil."
  (catch 'found-it
    (dolist (node-info (elyo-json-python-infos))
      (when (string-equal (plist-get node-info :node-id) node-id)
        (throw 'found-it node-info)))))


(defun elyo-json-select-code-of (node-id code)
  "Select CODE snippet in code from NODE-ID."
  (unwind-protect
      (let ((node-info (elyo-node-plist-by node-id)))
        (narrow-to-region (plist-get node-info :start)
                          (plist-get node-info :end))
        (goto-char (point-min))
        (search-forward code)
        (goto-char (match-beginning 0)))
    (widen)))


(defun elyo-json-code-replace (node-id code)
  "Replace CODE in python code node with NODE-ID."
  (unless (buffer-modified-p)
    (revert-buffer-with-coding-system 'utf-8-dos))
  (let* ((node-info (elyo-node-plist-by node-id))
         (start (plist-get node-info :start))
         (end (plist-get node-info :end)))
    (goto-char start)
    (replace-string-in-region (elyo--buffer-substring start end)
                              code (pos-bol) (pos-eol))
    (goto-char start)))


(defun elyo-json-node-rename (node-id new-name)
  "Rename Python node with NODE-ID to NEW-NAME."
  (unless (buffer-modified-p)
    (revert-buffer-with-coding-system 'utf-8-dos))
  (unwind-protect
      (progn
        (elyo--json-node-view-of node-id)
        (sp-narrow-to-sexp 1)
        (replace-string-in-region (format "\"%s\"" (elyo--json-node-name-by node-id))
                                  (format "\"%s\"" new-name)
                                  (point-min) (point-max))
        (goto-char (point-min))
        (search-forward new-name)
        (goto-char (match-beginning 0)))
    (widen)))


(defun elyo-json-node-geometry-set (node-id show)
  "Set Show geometry of Python node with NODE-ID to SHOW"
  (unless (buffer-modified-p)
    (revert-buffer-with-coding-system 'utf-8-dos))
  (unwind-protect
      (progn
        (elyo--json-node-view-of node-id)
        (sp-narrow-to-sexp 1)
        (let ((start (elyo--key-bool-start "ShowGeometry"))
              (end (elyo--key-bool-end "ShowGeometry")))
          (goto-char start)
          (replace-string-in-region (elyo--buffer-substring start end)
                                    (if show "true" "false")
                                    (point-min) (point-max))
          (goto-char (pos-bol))))
    (widen)))


(defun elyo-json-python-at-point ()
  "Return plist of all python script nodes in current buffer."
  (let ((start (elyo--key-value-start "Id"))
        (end (elyo--key-value-end "Id")))
    (elyo-node-plist-by (elyo--buffer-substring start end))))

(defun elyo-json-python-infos ()
  "Return plist of all python script nodes in current buffer."
  (let ((nodes (list)))
    (goto-char (point-min))
    (while (search-forward (elyo--search-value "PythonScriptNode") nil t)
      (beginning-of-line)
      (let ((node (elyo--json-object-read)))
        (when (elyo--script-node-? node)
          (push (elyo--node-plist node) nodes)))
      (sp-end-of-sexp))
    (seq-reverse nodes)))

(defun elyo-json-python-infos-in (file-path)
  "Return plist of all python script nodes in FILE-PATH."
  (let ((buffer (find-file-noselect file-path))
        (nodes (list)))
    (with-current-buffer buffer
      (unless (buffer-modified-p)
        (revert-buffer-with-coding-system 'utf-8-dos))
      (setq nodes (elyo-json-python-infos)))
    (kill-buffer buffer)
    nodes))

(defun elyo--json-file-plist ()
  "Return NODE information of the file in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((uuid-start (elyo--key-value-start "UUID"))
          (uuid-end (elyo--key-value-end "UUID"))
          (name-start (elyo--key-value-start "Name"))
          (name-end (elyo--key-value-end "Name"))
          (custom-start (elyo--key-bool-start "IsCustomNode"))
          (custom-end (elyo--key-bool-end "IsCustomNode"))
          (category-start (elyo--key-value-start "Category"))
          (category-end (elyo--key-value-end "Category")))
      (list :node-id (elyo--buffer-substring uuid-start uuid-end)
            :is-custom (elyo--key-bool-value (elyo--buffer-substring
                                              custom-start custom-end))
            :category (elyo--buffer-substring category-start category-end)
            :name (elyo--buffer-substring name-start name-end)
            :path (buffer-file-name)))))

(defun elyo-json-file-info (file-path)
  "Return plist of all python script nodes in FILE-PATH."
  (with-current-buffer (find-file-noselect file-path)
    (unless (buffer-modified-p)
      (revert-buffer-with-coding-system 'utf-8-dos))
    (elyo--json-file-plist)))

(provide 'elyo-json)
;;; elyo-json.el ends here
