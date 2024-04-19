;;; elyo-pydyn-convert.el --- Dynamo BIM package -*- lexical-binding: t; -*-

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
(require 'elyo-pydyn-json)
(require 'elyo-pydyn-path)
(require 'elyo-pydyn-utils)

(defvar-local node-uuid nil "Unique node id in the source file.")
(put 'node-uuid 'safe-local-variable (lambda (_) t))

(defvar-local node-engine nil "Python engine to use for code.")
(put 'node-engine 'safe-local-variable (lambda (_) t))

(defvar-local node-path nil "Source path of the source file.")
(put 'node-path 'safe-local-variable (lambda (_) t))

(defvar-local node-export nil "Export path for preview buffers.")
(put 'node-export 'safe-local-variable (lambda (_) t))


(defun elyo-pydyn-local-var? ()
  "Return non-nil if buffer-local line exist."
  ;; # -*- elyo-pydyn-beg: 12716;
  (let* ((local-value "# -*-")
         (value-count (1+ (seq-length local-value))))
    (unless (<= (point-max) value-count)
      (s-starts-with-p local-value (elyo-pydyn-buffer-substring
                                    (point-min) value-count)))))


(defun elyo-pydyn--convert-file-local-set (node-info)
  "Add file local variable with value of NODE-INFO in current.buffer."
  (goto-char (point-min))
  (unless (elyo-pydyn-local-var?)
    (open-line 1))
  (add-file-local-variable-prop-line 'node-uuid   (plist-get node-info :node-id))
  (add-file-local-variable-prop-line 'node-engine (plist-get node-info :engine))
  (add-file-local-variable-prop-line 'node-path   (plist-get node-info :path)))


(defun elyo-pydyn--convert-buffer-local-set (node-info &optional export-path)
  "Set buffer-local variable with value of NODE-INFO and EXPORT-PATH."
  (setq-local node-uuid   (plist-get node-info :node-id))
  (setq-local node-engine (plist-get node-info :engine))
  (setq-local node-path   (plist-get node-info :path))
  (when (and export-path (not (file-exists-p export-path)))
    (setq-local node-export export-path)))


(defun elyo-pydyn-export-path (node-info)
  "Return export python file path for NODE-INFO."
  (concat (elyo-pydyn-path-export-folder (plist-get node-info :path))
          (elyo-pydyn-path-export-file-name node-info)))


(defun elyo-pydyn-export-path-all (file-path)
  "Return all export path of FILE-PATH or nil if FILE-PATH is not dynamo-source."
  (when (elyo-pydyn-is-dynamo-source? file-path)
    (seq-map (lambda (node-info)
               (elyo-pydyn-export-path node-info))
             (elyo-pydyn-python-nodes-in file-path))))


(defun elyo-pydyn-code-in-buffer (buffer node-info &optional callback)
  "Return BUFFER with cleaned code from NODE-INFO.
CALLBACK is applied to clean exported code."
  (let ((engine (plist-get node-info :engine))
        (code (elyo-pydyn-dynamo-decode
               (plist-get node-info :code))))
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
        (when (elyo-pydyn-is-python-2? engine)
          (elyo-pydyn-buffer-tabify))
        (when (elyo-pydyn-is-python-3? engine)
          (elyo-pydyn-buffer-untabify))
        (delete-trailing-whitespace (point-min)
                                    (point-max))
        ;; Go to first line of code
        (goto-char (point-min))
        (while (string-blank-p (elyo-pydyn-current-line))
          (forward-line)))
      (current-buffer))))


(defun elyo-pydyn--preview-get (export-path node-info)
  "Return convert code from NODE-INFO with CALLBACK at EXPORT-PATH if exist."
  (let ((buffer (if (file-exists-p export-path)
                    (elyo-pydyn-buffer-by export-path)
                  (elyo-pydyn-buffer-preview-get
                   (file-name-base export-path)))))
    (elyo-pydyn-code-in-buffer buffer node-info)))


(defun elyo-pydyn-preview-buffer (node-info)
  "Return BUFFER with converted code from NODE-INFO.
CALLBACK is applied to clean exported code."
  (let* ((export (elyo-pydyn-export-path node-info))
         (export-exist (file-exists-p export)))
    (with-current-buffer (elyo-pydyn--preview-get export node-info)
      (when export-exist
        (elyo-pydyn--convert-file-local-set node-info))
      (elyo-pydyn--convert-buffer-local-set node-info export)
      ;; Go to first line of code
      (goto-char (point-min))
      (when export-exist
        ;; Because of file local vars, start at second line
        (forward-line))
      (while (string-blank-p (elyo-pydyn-current-line))
        (forward-line))
      (current-buffer))))


(defun elyo-pydyn-convert-node-to-python (node-info callback)
  "Return BUFFER with converted code from NODE-INFO.
CALLBACK is applied to clean exported code."
  (let ((export-path (elyo-pydyn-export-path node-info)))
    (with-current-buffer (elyo-pydyn-code-in-buffer
                          (elyo-pydyn-buffer-by export-path) node-info callback )
      (elyo-pydyn--convert-file-local-set node-info)
      (elyo-pydyn--convert-buffer-local-set node-info nil)
      ;; Go to first line of code
      (goto-char (point-min))
      ;; Because of file local vars, start at second line
      (forward-line)
      (while (string-blank-p (elyo-pydyn-current-line))
        (forward-line))
      (current-buffer))))


(defun elyo-pydyn-convert-to-python (file-path clean-cb)
  "Return buffer of last exported python node in FILE-PATH.
CLEAN-CB is applied to clean exported code. Unless last buffer,
  buffer will be saved and killed"
  (let ((last-export nil))
    (dolist (node-info (elyo-pydyn-python-nodes-in file-path :name))
      (when last-export
        (elyo-pydyn-buffer-save last-export nil nil t))
      (setq last-export (elyo-pydyn-convert-node-to-python
                         node-info clean-cb)))
    last-export))


(defun elyo-pydyn-convert-to-dynamo (code)
  "Return CODE converted into Dynamo format."
  (with-temp-buffer
    (let ((delete-trailing-lines t)
          (require-final-newline nil))
      (insert code)
      (goto-char (point-min))
      (when (elyo-pydyn-local-var?)
        (goto-char (point-min))
        (delete-line))
      (delete-trailing-whitespace (point-min) (point-max))
      ;; Dynamo use always tabs and \n in JSON string...
      (elyo-pydyn-buffer-tabify)
      (elyo-pydyn-dynamo-encode
       (string-trim (elyo-pydyn-buffer-substring (point-min) (point-max))
                    "[\n]+" "[\n]+")))))


(defun elyo-pydyn-convert-python-to-dynamo (code)
  "Replace python CODE in Dynamo Python NODE."
  (when (elyo-pydyn-is-dynamo? node-path)
    (let ((code (elyo-pydyn-convert-to-dynamo code))
          (node-id node-uuid))
      (save-current-buffer
        (set-buffer (elyo-pydyn-buffer-by node-path))
        (elyo-pydyn-json-code-replace node-id code)))
    node-path))


(defun elyo-pydyn-dynamo-node-name ()
  "Return Dynamo node name of Python file in current buffer."
  (when (elyo-pydyn-is-dynamo? node-path)
    (let ((node-id node-uuid))
      (with-current-buffer (elyo-pydyn-buffer-by node-path)
        (plist-get (elyo-pydyn-json-node-info-by
                    node-path node-id)
                   :name)))))


(defun elyo-pydyn-node-rename (new-name)
  "Rename Dynamo node to NEW-NAME of Python file in current buffer."
  (when (elyo-pydyn-is-dynamo? node-path)
    (let ((node-id node-uuid))
      (set-buffer (elyo-pydyn-buffer-by node-path))
      (elyo-pydyn-json-node-rename node-id new-name))))


(defun elyo-pydyn-node-geometry-set (show)
  "Set show geometry to SHOW for node of Python file in current buffer."
  (when (elyo-pydyn-is-dynamo? node-path)
    (let ((node-id node-uuid))
      (set-buffer (elyo-pydyn-buffer-by node-path))
      (elyo-pydyn-json-node-geometry-set node-id show))))


(defun elyo-pydyn-goto-code (code)
  "Select CODE in node of Python file in current buffer."
  (let ((node-id node-uuid)
        (buffer (elyo-pydyn-buffer-by node-path))
        (trim "[ \\t\\n\\r\"]+"))
    (switch-to-buffer buffer)
    (elyo-pydyn-json-select-code-of
     node-id (string-trim code trim trim))
    (elyo-pydyn--cursor-to-left-border)))


(defun elyo-pydyn--cursor-to-left-border ()
  "Scroll the screen that the cursor is close to left border."
  (if (= 0 (window-left-column))
      (scroll-left (1- (current-column)))
    (scroll-left (1- (- (current-column) (window-left-column)))))
  (scroll-right 5)
  (recenter))


(defun elyo-pydyn-dynamo-file-info ()
  "Return file info from Dynamo SCRIPT or CUSTOM NODE."
  (elyo-pydyn-dynamo-file-info-of node-path))


(provide 'elyo-pydyn-convert)
;;; elyo-pydyn-convert.el ends here
