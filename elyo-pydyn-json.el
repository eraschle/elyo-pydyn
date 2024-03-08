;;; elyo-pydyn-json.el -- DOOM Dynamo package -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Erich Raschle
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; This module provides functionality for read and write Dynamo file content.
;;
;;; Code:
(require 'elyo-pydyn-utils)
(require 'elyo-pydyn-path)

(require 'json)
(require 'smartparens)

(defun elyo-pydyn--search-value (value &optional key)
  "Return search string for VALUE to find and ':' if KEY is non-nil.
Otherwise search value for value is return."
  (if key (format "\"%s\":" value)
    (if (and (string-prefix-p "\"" value) (string-suffix-p "\"" value))
        value
      (format "\"%s\"" value))))


(defun elyo-pydyn--goto-key (key-name &optional from-point bound check-narrow)
  "Return end position of KEY-NAME.
If FROM-POINT is non-nil searching begin from point-min
and BOUND is searching boundery. No error is thrown when buffer is not narrow,
if CHECK-NARROW is non-nil."
  (when (and check-narrow (not (buffer-narrowed-p)))
    (error "Expect buffer is narrowed to node content"))
  (unless from-point
    (goto-char (point-min)))
  (search-forward (elyo-pydyn--search-value key-name t) bound nil 1)
  (goto-char (match-end 0)))


(defun elyo-pydyn--key-value-start (key-name &optional from-point bound as-string)
  "Return start position of macht-result for KEY-NAME.
If FROM-POINT is non-nil searching begin from point-min
and BOUND is searching boundery.
If AS-STRING is non-nil value contain with surounding \"."
  (save-excursion
    (elyo-pydyn--goto-key key-name from-point bound)
    (+ (match-end 0) (if as-string 1 2))))


(defun elyo-pydyn--key-bool-start (key-name &optional from-point bound)
  "Return start position of boolean value from KEY-NAME.
If FROM-POINT is non-nil point move to point-min. BOUND is searching boundery."
  (1- (elyo-pydyn--key-value-start key-name from-point bound)))


(defun elyo-pydyn--key-value-end (key-name &optional from-point bound as-string)
  "Return end position of value from KEY-NAME.
If FROM-POINT is non-nil searching begin from point-min
and BOUND is searching boundery.
If AS-STRING is non-nil value contain with surounding \"."
  (save-excursion
    (elyo-pydyn--goto-key key-name from-point bound)
    (goto-char (match-end 0))
    (- (pos-eol) (if as-string 1 2))))


(defun elyo-pydyn--key-bool-end (key-name &optional from-point bound)
  "Return end position of boolean value from KEY-NAME.
If FROM-POINT is non-nil point move to point-min. BOUND is searching boundery."
  (1+ (elyo-pydyn--key-value-end key-name from-point bound)))


(defun elyo-pydyn--key-bool-value (bool-value)
  "Return non-nil if BOOL-VALUE is true."
  (if (string-equal "true" bool-value) t nil))


(defun elyo-pydyn--json-prop-get (prop start end &optional default as-string)
  "Return PROP value or DEFAULT between START and END.
If AS-STRING is non-nil value contain with surounding \"."
  (goto-char start)
  (condition-case _ex
      (let ((value-start (elyo-pydyn--key-value-start prop t end as-string))
            (value-end (elyo-pydyn--key-value-end prop t end as-string)))
        (let ((value (elyo-pydyn-buffer-substring value-start value-end)))
          (if value value default)))
    ('error default)))


(defun elyo-pydyn-dynamo-decode (code)
  "Return CODE decoded from Json / Dynamo."
  (with-temp-buffer
    (insert code)
    (goto-char (point-min))
    (json-read-string)))


(defun elyo-pydyn-dynamo-encode (code)
  "Return CODE encoded for Json / Dynamo."
  (json-encode-string code))


(defvar :node-id (make-symbol "node-id")
  "NODE-ID of node.")
(defvar :node-info (make-symbol "node-info")
  "NODE-ID of node.")
(defvar :name (make-symbol "name")
  "NAME of node.")
(defvar :code (make-symbol "code")
  "CODE string in node.")
(defvar :code-line (make-symbol "code-line")
  "LINE NUMBER of code in node.")
(defvar :engine (make-symbol "engine")
  "ENGINE for code in node.")
(defvar :node-start (make-symbol "node-start")
  "START LINE NUMBER of NODE in Dynamo document.")
(defvar :node-end (make-symbol "node-end")
  "END LINE NUMBER of NODE in Dynamo document.")
(defvar :path (make-symbol "path")
  "Dynamo file PATH of node.")


(defun elyo-pydyn--json-node-info-read ()
  "Return JSON-object at point or nil if an error occurs."
  (save-excursion
    (let ((start (progn (sp-beginning-of-sexp) (point)))
          (end (progn (sp-end-of-sexp) (point))))
      (goto-char start)
      (let ((code-start (elyo-pydyn--key-value-start "Code" t end t))
            (code-end (elyo-pydyn--key-value-end "Code" t end t)))
        (list :node-id (elyo-pydyn--json-prop-get "Id" start end)
              :code (elyo-pydyn-buffer-substring code-start code-end)
              :code-line (1- (line-number-at-pos code-start t))
              :engine (elyo-pydyn--json-prop-get "Engine" start end
                                                 elyo-pydyn-python-2-engine)
              :path (buffer-file-name)
              :node-start (1- (line-number-at-pos start t))
              :node-end (1- (line-number-at-pos end t)))))))


(defun elyo-pydyn--json-node-view-read ()
  "Return JSON-object at point or nil if an error occurs."
  (save-excursion
    (let ((start (progn (sp-beginning-of-sexp) (point)))
          (end (progn (sp-end-of-sexp) (point))))
      (goto-char start)
      (list :node-id (elyo-pydyn--json-prop-get "Id" start end)
            :name (elyo-pydyn--json-prop-get "Name" start end)))))


(defun elyo-pydyn--script-node-? (node)
  "Return non-nil if NODE is python script node."
  (and (string-equal (gethash "ConcreteType" node)
                     "PythonNodeModels.PythonNode, PythonNodeModels")
       (string-equal (gethash "NodeType" node)
                     "PythonScriptNode")))


(defun elyo-pydyn--json-narrow-key (json-key)
  "Narrow buffer to content of JSON-KEY."
  (goto-char (point-min))
  (search-forward (elyo-pydyn--search-value json-key t) nil t 1)
  (forward-line 1)
  (sp-narrow-to-sexp 1)
  (goto-char (point-min)))


(defun elyo-pydyn--json-node-view-goto (node-id &optional narrow)
  "Move point to NODE VIEW with NODE-ID and NARROW buffer if non-nil."
  (goto-char (point-min))
  (search-forward (elyo-pydyn--search-value node-id))
  (beginning-of-line)
  (when narrow
    (sp-narrow-to-sexp 1)))


(defun elyo-pydyn--json-node-view-by (node-id &optional narrow)
  "Return NODE VIEW with NODE-ID and NARROW buffer if non-nil."
  (elyo-pydyn--json-node-view-goto node-id narrow)
  (elyo-pydyn--json-node-view-read))


(defun elyo-pydyn--json-node-name-by (node-id &optional narrow)
  "Return name of node with NODE-ID, NARROW buffer if non-nil."
  (save-excursion
    (let ((view-info (elyo-pydyn--json-node-view-by
                      node-id narrow)))
      (let ((name (plist-get view-info :name)))
        (if name name "NO_NODE_NAME")))))


(defvar buffer-cache (make-hash-table) "Cached node-info.")


(defun elyo-pydyn--json-buffer-cache-add (file-path node-cache)
  "Add NODE-CACHE with key FILE-PATH to buffer-cache."
  (unless buffer-cache
    (setq buffer-cache (make-hash-table)))
  (puthash file-path node-cache buffer-cache))


(defun elyo-pydyn-buffer-cache-reset ()
  "Reset global buffer cache."
  (interactive)
  (setq buffer-cache nil))


(defun elyo-pydyn-node-cache-update (file-path)
  "Update node cache of FILE-PATH in buffer-cache."
  (elyo-pydyn-node-cache-reset file-path)
  (elyo-pydyn--json-node-infos file-path))


(defun elyo-pydyn-node-cache-reset (file-path)
  "Reset node cache of FILE-PATH in buffer-cache."
  (when buffer-cache
    (remhash file-path buffer-cache)))


(defun elyo-pydyn--node-cache-add-names (nodes-cache)
  "Add name into node in NODES-CACHE."
  (elyo-pydyn--json-narrow-key "NodeViews")
  (dolist (node-info nodes-cache)
    (let* ((node-id (plist-get node-info :node-id))
           (view-info (elyo-pydyn--json-node-view-by node-id)))
      (plist-put node-info :name (plist-get view-info :name)))))


(defun elyo-pydyn--json-node-infos-create (file-path)
  "Return PLIST of all python nodes in FILE-PATH."
  (with-current-buffer (elyo-pydyn-buffer-by file-path)
    (let ((node-cache nil))
      (save-excursion
        (save-restriction
          (elyo-pydyn--json-narrow-key "Nodes")
          (forward-line)
          (while (search-forward (elyo-pydyn--search-value "PythonScriptNode") nil t)
            (beginning-of-line)
            (let ((node-info (elyo-pydyn--json-node-info-read)))
              (when node-info
                (push node-info node-cache)))
            (sp-end-of-sexp)))
        (save-restriction
          (elyo-pydyn--node-cache-add-names node-cache)))
      (elyo-pydyn--json-buffer-cache-add file-path node-cache))))


(defun elyo-pydyn--json-node-infos (file-path)
  "Return PLIST of all python nodes in FILE-PATH."
  (elyo-pydyn-is-dynamo-or-error file-path)
  (unless buffer-cache
    (setq buffer-cache (make-hash-table)))
  ;; (unless (assoc file-path buffer-cache)
  (unless (gethash file-path buffer-cache)
    (message "Create node info cashe %s" (file-name-base file-path))
    (elyo-pydyn--json-node-infos-create file-path))
  (gethash file-path buffer-cache))


(defun elyo-pydyn-json-node-info-by (file-path node-id)
  "Return node info of node with NODE-ID or nil from FILE-PATH."
  (seq-find (lambda (node) (equal (plist-get node :node-id) node-id))
            (elyo-pydyn--json-node-infos file-path)))


(defun elyo-pydyn--json-node-info-update (node-id prop value)
  "Update node with NODE-ID in cache with PROP VALUE."
  (let ((node-info (elyo-pydyn-json-node-info-by buffer-file-name node-id)))
    (plist-put node-info prop value)))

(defun elyo-pydyn-json-goto-line (node-id property)
  "Goto line of PROPERTY value in node with NODE-ID."
  (when (buffer-narrowed-p)
    (widen))
  (goto-char (point-min))
  (let ((node-info (elyo-pydyn-json-node-info-by buffer-file-name node-id)))
    (forward-line (plist-get node-info property))))


(defun elyo-pydyn-json-select-code-of (node-id code)
  "Select CODE in code value of node with NODE-ID."
  (let ((node-info (elyo-pydyn-json-node-info-by buffer-file-name node-id)))
    (goto-char (point-min))
    (forward-line (plist-get node-info :code-line)))
  (let ((start (elyo-pydyn--key-value-start "Code" t))
        (end (elyo-pydyn--key-value-end "Code" t))
        (case-fold-search t))
    (goto-char start)
    (if (search-forward code end t)
        (goto-char (match-beginning 0))
      (goto-char start))))


(defun elyo-pydyn-json-code-replace (node-id code)
  "Replace CODE in python node with NODE-ID in FILE-PATH."
  (save-restriction
    (let ((node-info (elyo-pydyn--json-narrow-node-by node-id)))
      (unless (string-equal (plist-get node-info :code) code)
        (let ((start (elyo-pydyn--key-value-start "Code" nil nil t))
              (end (elyo-pydyn--key-value-end "Code" nil nil t)))
          (replace-string-in-region
           (plist-get node-info :code) code start end)
          (elyo-pydyn--json-node-info-update node-id :code code)))
      (goto-char (elyo-pydyn--key-value-start "Code")))))


(defun elyo-pydyn-json-node-rename (node-id new-name)
  "Rename Python node with NODE-ID to NEW-NAME."
  (save-restriction
    (elyo-pydyn--json-narrow-key "NodeViews")
    (elyo-pydyn--json-node-view-goto node-id t)
    (replace-string-in-region (format "%S" (elyo-pydyn--json-node-name-by node-id))
                              (format "%S" new-name))
    (goto-char (point-min))
    (search-forward new-name)
    (goto-char (match-beginning 0))
    (elyo-pydyn--json-node-info-update  node-id :name new-name)))


(defun elyo-pydyn-json-node-geometry-set (node-id show)
  "Set show geometry to SHOW for Python node with NODE-ID."
  (save-restriction
    (elyo-pydyn--json-narrow-key "NodeViews")
    (elyo-pydyn--json-node-view-goto node-id t)
    (let ((start (elyo-pydyn--key-bool-start "ShowGeometry"))
          (end (elyo-pydyn--key-bool-end "ShowGeometry")))
      (goto-char start)
      (replace-string-in-region (elyo-pydyn-buffer-substring start end)
                                (if show "true" "false")
                                (point-min) (point-max))
      (goto-char (match-beginning 0)))))


(defun elyo-pydyn-python-node-get ()
  "Return PLIST of python node at point in current buffer."
  (let ((line (line-number-at-pos (point) t)))
    (seq-find (lambda (node)
                (and (>= line (plist-get node :node-start))
                     (<= line (plist-get node :node-end))))
              (elyo-pydyn--json-node-infos buffer-file-name))))


(defun elyo-pydyn--json-narrow-node-by (node-id)
  "Move point to beginning of node with NODE-ID in current buffer."
  (if (buffer-narrowed-p)
      (widen))
  (let ((node-info (elyo-pydyn-json-node-info-by buffer-file-name node-id)))
    (unless node-info
      (error "Node info with %s does not exist in %s"
             node-id buffer-file-name))
    (narrow-to-region (save-excursion
                        (goto-char (point-min))
                        (forward-line (plist-get node-info :node-start))
                        (pos-bol))
                      (save-excursion
                        (goto-char (point-min))
                        (forward-line (plist-get node-info :node-end))
                        (pos-eol)))
    (goto-char (point-min))
    node-info))


(defun elyo-pydyn-python-nodes-get (&optional sort-prop)
  "Return PLIST of python nodes in current buffer sorted by SORT-PROP."
  (let ((sort-prop (or sort-prop :code-line)))
    (seq-sort (lambda (node other)
                (if (stringp (plist-get node sort-prop))
                    (string-greaterp (plist-get node sort-prop)
                                     (plist-get other sort-prop))
                  (> (plist-get node sort-prop)
                     (plist-get other sort-prop))))
              (elyo-pydyn--json-node-infos buffer-file-name))))


(defun elyo-pydyn-python-nodes-in (file-path &optional sort-prop)
  "Return plist of python nodes in FILE-PATH sorted by SORT-PROP."
  (with-current-buffer (elyo-pydyn-buffer-by file-path)
    (elyo-pydyn-python-nodes-get sort-prop)))


(defvar :is-custom (make-symbol "is-custom")
  "IS_CUSTOM property of custom or script node info PLIST.")
(defvar :category (make-symbol "category")
  "CATEGORY property of custom or script node info PLIST.")


(defun elyo-pydyn--dynamo-file-plist ()
  "Return NODE info of SCRIPT or CUSTOM BLOCK in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((uuid-start (elyo-pydyn--key-value-start "UUID"))
          (uuid-end (elyo-pydyn--key-value-end "UUID"))
          (name-start (elyo-pydyn--key-value-start "Name"))
          (name-end (elyo-pydyn--key-value-end "Name"))
          (custom-start (elyo-pydyn--key-bool-start "IsCustomNode"))
          (custom-end (elyo-pydyn--key-bool-end "IsCustomNode"))
          (category-start (elyo-pydyn--key-value-start "Category"))
          (category-end (elyo-pydyn--key-value-end "Category")))
      (list :node-id (elyo-pydyn-buffer-substring uuid-start uuid-end)
            :is-custom (elyo-pydyn--key-bool-value (elyo-pydyn-buffer-substring
                                                    custom-start custom-end))
            :category (elyo-pydyn-buffer-substring category-start category-end)
            :name (elyo-pydyn-buffer-substring name-start name-end)
            :path (buffer-file-name)))))


(defun elyo-pydyn-dynamo-file-info-of (file-path)
  "Return PLIST with file info from SCRIPT oder CUSTOM NODE at FILE-PATH."
  (with-current-buffer (find-file-noselect file-path)
    (elyo-pydyn--dynamo-file-plist)))

(provide 'elyo-pydyn-json)
;;; elyo-pydyn-json.el ends here
