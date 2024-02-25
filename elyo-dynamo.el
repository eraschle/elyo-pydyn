;;; elyo-dynamo.el --- Dynamo BIM Emacs package -*- lexical-binding: t; -*-

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
;; This module provides minor mode `elyo-dynamo'
;;
;;; Code:

(require 'elyo-utils)
(require 'elyo-python)

(require 'view)

(defcustom elyo-dynamo-keymap-prefix "C-d"
  "The prefix for elyo-dynamo-mode key bindings."
  :type 'string
  :group 'elyo-pydyn)


(defun elyo-dynamo-key (key)
  "Return Emacs key representation of KEY."
  (elyo-pydyn-key elyo-dynamo-keymap-prefix key))


(defun elyo-dynamo--mode-map-create ()
  "Define mode keymap."
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (elyo-dynamo-key "m") #'elyo-dynamo-mode-on)
    (define-key key-map (elyo-dynamo-key "M") #'elyo-dynamo-mode-off)
    (define-key key-map (elyo-dynamo-key "n") #'elyo-dynamo-at-point-to-python)
    (define-key key-map (elyo-dynamo-key "s") #'elyo-dynamo-script-to-python)
    (define-key key-map (elyo-dynamo-key "S") #'elyo-dynamo-folder-to-python)
    (define-key key-map (elyo-dynamo-key "j") #'elyo-dynamo-jump-to-node)
    (define-key key-map (elyo-dynamo-key "g") #'elyo-dynamo-goto-python)
    (define-key key-map (elyo-dynamo-key "p") #'elyo-dynamo-python-code-preview)
    (define-key key-map (elyo-dynamo-key "o") #'elyo-dynamo-clean-orphan-code-file)
    (define-key key-map (elyo-dynamo-key "O") #'elyo-dynamo-clean-orphan-code-folder)
    (define-key key-map (elyo-dynamo-key "x") #'elyo-dynamo-file-cache-reset)
    (define-key key-map (elyo-dynamo-key "X") #'elyo-dynamo-buffer-cache-reset)
    key-map))


(defvar elyo-dynamo-mode-map (elyo-dynamo--mode-map-create)
  "Keymap for dynpy minor mode.")

(add-to-list 'minor-mode-alist '(elyo-dynamo-mode " elyo-dynamo"))
(add-to-list 'minor-mode-map-alist (cons 'elyo-dynamo-mode elyo-dynamo-mode-map));;


(defcustom elyo-dynamo-indent-width nil
  "Indent width for `json-mode' in open Dynamo file."
  :type 'integer
  :group 'elyo-pydyn)


(defun elyo-dynamo-indent-width-setup ()
  "Set indent with in current buffer."
  (when elyo-dynamo-indent-width
    (elyo-indent-width-setup elyo-dynamo-indent-width)))


(defun elyo-dynamo--node-info-or-error ()
  "Switch to Python file of code at current point if exists."
  (elyo-is-dynamo-or-error)
  (let ((node-info (elyo-json-python-at-point)))
    (unless node-info
      (user-error "Current point is NOT inside a PYTHON node!!!"))
    node-info))


;;;###autoload
(defun elyo-dynamo-goto-python ()
  "Switch to Python file of code at current point if exists."
  (interactive)
  (let ((path (elyo-convert-export-path
               (elyo-dynamo--node-info-or-error))))
    (unless (file-exists-p path)
      (user-error "File %s does not exists in %s"
                  (file-name-base path)
                  (file-name-parent-directory path)))
    (switch-to-buffer-other-window (elyo-buffer-by path))))


;;;###autoload
(defun elyo-dynamo-python-code-preview ()
  "Jump to selected node in current buffer."
  (interactive)
  (let ((buffer (elyo-convert-preview-buffer
                 (elyo-dynamo--node-info-or-error))))
    (switch-to-buffer-other-window
     (with-current-buffer buffer
       (view-mode-exit)
       (if (not node-export)
           (revert-buffer nil t t)
         (python-mode)
         (ensure-empty-lines 1))
       (view-mode-enter)
       (current-buffer)))))


;;;###autoload
(defun elyo-dynamo-at-point-to-python (switch-or-kill)
  "Export python code at point and SWITCH-OR-KILL export buffer."
  (interactive (list (elyo-choose-switch-or-kill "Python")))
  (unwind-protect
      (let ((node-info (elyo-dynamo--node-info-or-error))
            (switch (elyo--is-switch switch-or-kill))
            (other-win (elyo--is-switch-other switch-or-kill))
            (kill (elyo--is-kill switch-or-kill)))
        (elyo-disable-lsp-clients)
        (elyo-save-buffer-of (elyo-convert-node-to-python
                              node-info 'elyo-python-convert-clean)
                             switch other-win kill))
    (elyo-enable-lsp-clients)
    (when (or (elyo--is-switch switch-or-kill)
              (elyo--is-switch-other switch-or-kill))
      (elyo-python-mode-on))))


;;;###autoload
(defun elyo-dynamo-script-to-python (file-path switch-or-kill)
  "Export python node in FILE-PATH and SWITCH-OR-KILL to export buffer."
  (interactive (list (elyo-selection-get
                      (elyo-dynamo-files-in elyo-source-root t)
                      "Select Dynamo file:" elyo-source-root)
                     (elyo-choose-switch-or-kill "Python")))
  (elyo-is-dynamo-or-error file-path)
  (unwind-protect
      (progn
        (elyo-disable-lsp-clients)
        (let ((buffer (elyo-convert-to-python
                       file-path 'elyo-python-convert-clean))
              (switch (elyo--is-switch switch-or-kill))
              (other-win (elyo--is-switch-other switch-or-kill))
              (kill (elyo--is-kill switch-or-kill)))
          (elyo-save-buffer-of buffer switch other-win kill)))
    (elyo-enable-lsp-clients)
    (when (or (elyo--is-switch switch-or-kill)
              (elyo--is-switch-other switch-or-kill))
      (elyo-python-mode-on))))


;;;###autoload
(defun elyo-dynamo-folder-to-python (&optional directory switch-or-kill)
  "Export all python nodes of Dynamo files in DIRECTORY.
SWITCH-OR-KILL last export buffer afterwards."
  (interactive (list (read-directory-name
                      "Export python to directory? "
                      elyo-source-root)
                     (elyo-choose-switch-or-kill "Python")))
  (elyo-is-source-or-error directory)
  (unwind-protect
      (let ((buffer nil)
            (switch (elyo--is-switch switch-or-kill))
            (other-win (elyo--is-switch-other switch-or-kill))
            (kill (elyo--is-kill switch-or-kill)))
        (elyo-disable-lsp-clients)
        (dolist (file-path (elyo-dynamo-files-in directory))
          (when (elyo-is-dynamo? file-path)
            (when buffer (elyo-save-buffer-of buffer nil nil t))
            (setq buffer (elyo-convert-to-python
                          file-path 'elyo-python-convert-clean))))
        (elyo-save-buffer-of buffer switch other-win kill))
    (elyo-enable-lsp-clients)
    (when (or (elyo--is-switch switch-or-kill)
              (elyo--is-switch-other switch-or-kill))
      (elyo-python-mode-on))))


(defun elyo-dynamo--node-select-of (node-info)
  "Return NODE-INFO value used for `completing-read'."
  (let ((uuid (plist-get node-info :node-id))
        (name (plist-get node-info :name)))
    (format "%-50s %s" name
            (propertize uuid 'face 'marginalia-documentation))))


(defun elyo-dynamo--node-selections (node-infos)
  "Return list with name and node-id for all nodes in NODE-INFOS."
  (seq-map 'elyo-dynamo--node-select-of node-infos))


(defun elyo-dynamo--select-node ()
  "Return node-info selected by the user."
  (let* ((node-infos (elyo-json-python-infos :name))
         (completions-format 'vertical)
         (completions-sort 'alphabetical)
         (selected (completing-read
                    "Jump to: "
                    (elyo-dynamo--node-selections node-infos)
                    nil t)))
    (catch 'found-it
      (dolist (node node-infos)
        (when (string-equal (elyo-dynamo--node-select-of node) selected)
          (throw 'found-it node))))))


;;;###autoload
(defun elyo-dynamo-jump-to-node ()
  "Jump to selected node in current buffer."
  (interactive)
  (elyo-is-dynamo-or-error)
  (let ((node-info (elyo-dynamo--select-node)))
    (elyo-json-goto-line (plist-get node-info :node-id)
                         :code-line)))


(defun elyo-dynamo--clean-orphan (file-path)
  "Delete python file where source node in Dynamo FILE-PATH does not exist anymore."
  (let ((export-path (elyo-python-files-in
                      (elyo-path-export-folder file-path)))
        (node-paths (elyo-convert-export-path-all file-path)))
    (dolist (path-wo-src (seq-difference export-path node-paths))
      (when (file-exists-p path-wo-src)
        (delete-file path-wo-src nil)))))


;;;###autoload
(defun elyo-dynamo-clean-orphan-code-file (&optional file-path)
  "Delete python files of not existing nodes of Dynamo FILE-PATH."
  (interactive (list (elyo-selection-get
                      (elyo-dynamo-files-in elyo-source-root t)
                      "Select Dynamo file:" elyo-source-root)))
  (elyo-is-source-or-error file-path)
  (when (yes-or-no-p "Are you sure to delete python-files??")
    (unwind-protect
        (progn (elyo-disable-lsp-clients)
               (elyo-dynamo--clean-orphan file-path))
      (elyo-enable-lsp-clients))))


;;;###autoload
(defun elyo-dynamo-clean-orphan-code-folder (&optional directory)
  "Delete all python files of existing nodes from Dynamo files in DIRECTORY."
  (interactive (list (read-directory-name
                      "Delete python files without node in? "
                      elyo-source-root)))
  (elyo-is-source-or-error directory)
  (unwind-protect
      (progn
        (elyo-disable-lsp-clients)
        (dolist (file-path (elyo-dynamo-files-in directory t))
          (elyo-dynamo--clean-orphan file-path)))
    (elyo-enable-lsp-clients)))


;;;###autoload
(defun elyo-dynamo-buffer-cache-reset ()
  "Reset global buffer cache."
  (interactive)
  (elyo-json-buffer-cache-reset))


;;;###autoload
(defun elyo-dynamo-file-cache-reset ()
  "Reset node cache of FILE-PATH in buffer-cache."
  (interactive)
  (elyo-json-node-cache-reset buffer-file-name))


(defun elyo-dynamo--cache-reset-h ()
  "Reset node cache for current file."
  (when (elyo-is-dynamo?)
    (elyo-json-node-cache-reset buffer-file-name)))


(defun elyo-dynamo--cache-remove-h ()
  "Reset node cache for current file."
  (when (elyo-is-dynamo-source?)
    (elyo-json-node-cache-reset buffer-file-name))
  (unless (seq-some
           (lambda (buf)
             (elyo-is-dynamo-source? (buffer-file-name buf)))
           (buffer-list))
    (elyo-json-buffer-cache-reset)))


(define-minor-mode elyo-dynamo-mode
  "Toggles elyo-dynamo-mode."
  :global nil
  :group 'elyo-pydyn
  :lighter " elyo-dynamo"
  :keymap elyo-dynamo-mode-map

  (add-hook 'after-revert-hook 'elyo-dynamo--cache-reset-h)
  (add-hook 'kill-buffer-hook 'elyo-dynamo--cache-remove-h)

  (when (and elyo-dynamo-mode
             (elyo-not-converting?))
    (elyo-dynamo-indent-width-setup)
    (message "ELYO DYNAMO")))


;;;###autoload
(defun elyo-is-json-mode? ()
  "Return non-nil when active major mode is `json-mode'."
  (or (equal major-mode 'json-mode)
      (derived-mode-p 'json-mode)))


;;;###autoload
(defun elyo-dynamo-json-config ()
  "Setup JSON file to work for minor modes."
  (when (and (elyo-is-json-mode?)
             (elyo-is-dynamo?))
    (setq-local require-final-newline nil
                so-long--inhibited t)))


;;;###autoload
(defun elyo-dynamo-mode-activate ()
  "Activate and config `elyo-dynamo-mode' if possible."
  (elyo-dynamo-json-config)
  (when (and (elyo-is-json-mode?)
             (elyo-is-dynamo-source?))
    (elyo-dynamo-mode 1)))


;;;###autoload
(defun elyo-dynamo-mode-on ()
  "Activate `elyo-dynamo-mode' if possible."
  (interactive)
  (elyo-dynamo-mode-activate))


;;;###autoload
(defun elyo-dynamo-mode-off ()
  "Deaktivert `elyo-dynamo-mode'."
  (interactive)
  (elyo-dynamo-mode -1))


(provide 'elyo-dynamo)
;;; elyo-dynamo.el ends here
