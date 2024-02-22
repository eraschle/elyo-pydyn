;;; elyo-convert.el --- Dynamo BIM package -*- lexical-binding: t; -*-

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
;; This module provides functionality to convert code between python and dynamo
;;
;;; Code:
(require 'elyo-json)
(require 'elyo-path)
(require 'elyo-utils)

(require 'ws-butler)
(require 'evil-lion)
(require 'json)

(defvar-local node-uuid nil "Unique node id in the source file.")
(put 'node-uuid 'safe-local-variable (lambda (_) t))

(defvar-local node-engine nil "Python engine to use for code.")
(put 'node-engine 'safe-local-variable (lambda (_) t))

(defvar-local node-path nil "Source path of the source file.")
(put 'node-path 'safe-local-variable (lambda (_) t))

(defvar-local node-export nil "Export path for preview buffers.")
(put 'node-export 'safe-local-variable (lambda (_) t))

(defvar :py-short (make-symbol "py-short")
  "Symbol for python abbrev.")
(defvar :py-engine (make-symbol "py-engine")
  "Symbol for python abbrev.")

(defvar elyo--python-abbrev (list (list :py-short "py2" :py-engine "IronPython2")
                                  (list :py-short "py3" :py-engine "CPython3"))
  "List with values to transfer code between Emacs und Dynamo.")


(defun elyo-convert-local-var? ()
  "Return non-nil if buffer-local line exist."
  ;; # -*- elyo-convert-beg: 12716;
  (s-starts-with-p "# -*-" (elyo--buffer-substring (point-min) 10)))


(defun elyo--convert-file-local-set (node-info)
  "Add file local variable with value of NODE-INFO in current.buffer."
  (goto-char (point-min))
  (unless (elyo-convert-local-var?)
    (open-line 1))
  (add-file-local-variable-prop-line 'node-uuid   (plist-get node-info :node-id))
  (add-file-local-variable-prop-line 'node-engine (plist-get node-info :engine))
  (add-file-local-variable-prop-line 'node-path   (plist-get node-info :path)))


(defun elyo--convert-buffer-local-set (node-info &optional export-path)
  "Set buffer-local variable with value of NODE-INFO and EXPORT-PATH."
  (setq-local node-uuid   (plist-get node-info :node-id))
  (setq-local node-engine (plist-get node-info :engine))
  (setq-local node-path   (plist-get node-info :path))
  (when (and export-path (not (file-exists-p export-path)))
    (setq-local node-export export-path)))


(defun elyo-convert-export-path (node-info)
  "Return export python file path for NODE-INFO."
  (concat (elyo-path-export-folder (plist-get node-info :path))
          (elyo-path-export-file-name node-info elyo--python-abbrev)))


(defun elyo-convert-export-path-all (file-path)
  "Return all export path of FILE-PATH or nil if FILE-PATH is not dynamo-source."
  (when (elyo-is-dynamo-source? file-path)
    (seq-map (lambda (node-info)
               (elyo-convert-export-path node-info))
             (elyo-json-python-infos-in file-path))))


(defun elyo--convert-decode-code (node-info)
  "Return decoded code from NODE-INFO."
  (with-temp-buffer
    (insert (plist-get node-info :code))
    (goto-char (point-min))
    (json-read-string)))


(defun elyo-convert-code-in-buffer (buffer node-info &optional callback)
  "Return BUFFER with cleaned code from NODE-INFO.
CALLBACK is applied to clean exported code."
  (let ((engine (plist-get node-info :engine))
        (code (elyo--convert-decode-code node-info)))
    (with-current-buffer buffer
      (let ((coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8)
            (delete-trailing-lines t)
            (require-final-newline nil))
        (read-only-mode -1)
        (coding-system-change-eol-conversion 'utf-8 'unix)
        (erase-buffer)
        (insert code)
        (when callback
          (funcall callback))
        (when (elyo-is-python-2? engine)
          (elyo-buffer-tabify))
        (when (elyo-is-python-3? engine)
          (elyo-buffer-untabify))
        (delete-trailing-whitespace (point-min)
                                    (point-max))
        ;; Go to first line of code
        (goto-char (point-min))
        (while (string-blank-p (elyo--current-line))
          (forward-line)))
      (current-buffer))))


(defun elyo-convert--preview-get (export-path node-info)
  "Return convert code from NODE-INFO with CALLBACK at EXPORT-PATH if exist."
  (let ((buffer (if (file-exists-p export-path)
                    (elyo-buffer-by export-path)
                  (elyo-buffer-preview-get
                   (file-name-base export-path)))))
    (elyo-convert-code-in-buffer buffer node-info)))


(defun elyo-convert-preview-buffer (node-info)
  "Return BUFFER with converted code from NODE-INFO.
CALLBACK is applied to clean exported code."
  (let* ((export (elyo-convert-export-path node-info))
         (export-exist (file-exists-p export)))
    (with-current-buffer (elyo-convert--preview-get export node-info)
      (when export-exist
        (elyo--convert-file-local-set node-info))
      (elyo--convert-buffer-local-set node-info export)
      ;; Go to first line of code
      (goto-char (point-min))
      (when export-exist
        ;; Beecause of file local vars, start at second line
        (forward-line))
      (while (string-blank-p (elyo--current-line))
        (forward-line))
      (current-buffer))))


(defun elyo-convert-node-to-python (node-info callback)
  "Return BUFFER with converted code from NODE-INFO.
CALLBACK is applied to clean exported code."
  (let ((export-path (elyo-convert-export-path node-info)))
    (with-current-buffer (elyo-convert-code-in-buffer
                          (elyo-buffer-by export-path) node-info callback )
      (elyo--convert-file-local-set node-info)
      (elyo--convert-buffer-local-set node-info nil)
      ;; Go to first line of code
      (goto-char (point-min))
      ;; Beecause of file local vars, start at second line
      (forward-line)
      (while (string-blank-p (elyo--current-line))
        (forward-line))
      (current-buffer))))


(defun elyo-convert-to-python (file-path clean-cb)
  "Return buffer of last exported python node in FILE-PATH.
CLEAN-CB is applied to clean exported code. Unless last buffer,
  buffer will be saved and killed"
  (let ((last-export nil))
    (dolist (node-info (elyo-json-python-infos-in file-path :name))
      (when last-export
        (elyo-save-buffer-of last-export nil nil t))
      (setq last-export (elyo-convert-node-to-python
                         node-info clean-cb)))
    last-export))


(defun elyo-convert-to-dynamo (code)
  "Return CODE converted into Dynamo format."
  (with-temp-buffer
    (let ((delete-trailing-lines t)
          (require-final-newline nil))
      (insert code)
      (goto-char (point-min))
      (when (elyo-convert-local-var?)
        (goto-char (point-min))
        (delete-line))
      (delete-trailing-whitespace (point-min) (point-max))
      ;; Dynamo use always tabs and \n in JSON string...
      (elyo-buffer-tabify)
      (json-encode-string
       (string-trim (elyo--buffer-substring (point-min) (point-max))
                    "[\n]*" "[\n]*")))))


(defun elyo-convert-python-to-dynamo (code)
  "Replace python CODE in Dynamo Python NODE."
  (when (elyo-is-dynamo? node-path)
    (let ((code (elyo-convert-to-dynamo code))
          (node-id node-uuid))
      (save-current-buffer
        (set-buffer (elyo-buffer-by node-path))
        (elyo-json-code-replace node-id code)))
    node-path))


(defun elyo-convert-node-name ()
  "Return Dynamo node name of Python file in current buffer."
  (when (elyo-is-dynamo? node-path)
    (let ((node-id node-uuid))
      (with-current-buffer (elyo-buffer-by node-path)
        (plist-get (elyo-json-node-info-by
                    node-path node-id)
                   :name)))))


(defun elyo-convert-node-rename (new-name)
  "Rename Dynamo node to NEW-NAME of Python file in current buffer."
  (when (elyo-is-dynamo? node-path)
    (let ((node-id node-uuid))
      (set-buffer (elyo-buffer-by node-path))
      (elyo-json-node-rename node-id new-name))))


(defun elyo-convert-node-geometry-set (show)
  "Set show geometry to SHOW for node of Python file in current buffer."
  (when (elyo-is-dynamo? node-path)
    (let ((node-id node-uuid))
      (set-buffer (elyo-buffer-by node-path))
      (elyo-json-node-geometry-set node-id show))))


(defun elyo-convert-goto-code (code)
  "Select CODE in node of Python file in current buffer."
  (let ((node-id node-uuid)
        (buffer (elyo-buffer-by node-path))
        (trim "[ \\t\\n\\r\"]+"))
    (switch-to-buffer buffer)
    (elyo-json-select-code-of
     node-id (string-trim code trim trim))
    (elyo--cursor-to-left-border)))


(defun elyo-convert-dynamo-file-info ()
  "Return file info from Dynamo SCRIPT or CUSTOM NODE."
  (elyo-json-file-info node-path))


(provide 'elyo-convert)
;;; elyo-convert.el ends here
