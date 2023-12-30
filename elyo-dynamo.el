;;; elyo-dynamo.el --- Dynamo BIM package -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides functionality to change Python code in a Dynamo file.
;;
;;; Code:

(require 'elyo-utils)
(require 'elyo-convert)

(defcustom elyo-dynamo-keymap-prefix "C-d"
  "The prefix for elyo-mode key bindings."
  :type 'string
  :group 'elyo-dynamo)


(defun elyo-dynamo-key (key)
  "Return Emacs key representation of KEY."
  (elyo-pydyn-key elyo-dynamo-keymap-prefix key))


(defun elyo-dynamo--mode-map-create ()
  "Define mode keymap."
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (elyo-dynamo-key "p") #'elyo-dynamo-at-point-to-python)
    (define-key key-map (elyo-dynamo-key "P") #'elyo-dynamo-script-to-python)
    (define-key key-map (elyo-dynamo-key "d") #'elyo-dynamo-folder-to-python)
    (define-key key-map (elyo-dynamo-key "g") #'elyo-dynamo-goto-python)
    key-map))


(defvar elyo-dynamo-mode-map (elyo-dynamo--mode-map-create)
  "Keymap for dynpy minor mode.")

(add-to-list 'minor-mode-alist '(elyo-dynamo-mode " elyo-dynamo"))
(add-to-list 'minor-mode-map-alist (cons 'elyo-dynamo-mode elyo-dynamo-mode-map));;


(defvar elyo--dynamo-hooks (list 'json-mode-hook
                                 )
  "Hooks to register when minor mode is enabled.")


(defun elyo--dynamo-add-hooks ()
  "Register hooks of the mode."
  (dolist (hook elyo--dynamo-hooks)
    (add-hook hook #'elyo--dynamo-mode-hook)))


(defun elyo--dynamo-remove-hooks ()
  "Unregister hooks of the mode."
  (dolist (hook elyo--dynamo-hooks)
    (remove-hook hook #'elyo--dynamo-mode-hook)))


(defun elyo--dynamo-mode-hook ()

  )


;;;###autoload
(defun elyo-dynamo-goto-python ()
  "Switch to Python file if exists."
  (interactive)
  (when (elyo-is-dynamo?)
    (let ((node-info (elyo-json-python-at-point)))
      (if (not node-info)
          (message "Current point is NOT inside a PYTHON node!!!")
        (let ((path (elyo-convert-export-path node-info)))
          (if (not (file-exists-p path))
              (message "File %s does not exists in %s"
                       (file-name-base path)
                       (file-name-parent-directory path))
            (switch-to-buffer (elyo-buffer-by path))))))))


;;;###autoload
(defun elyo-dynamo-at-point-to-python ()
  "Export python of dynamo node at current point."
  (interactive)
  (when (elyo-is-dynamo?)
    (save-excursion
      (let ((node-info (elyo-json-python-at-point)))
        (elyo-convert-buffer node-info)))
    (switch-to-buffer (last-buffer))))


;;;###autoload
(defun elyo-dynamo-script-to-python (&optional file-path)
  "Export python code of every python node in FILE-PATH and KILL-BUFFER if not-nil."
  (interactive (elyo-selection-get (elyo-dynamo-files-in elyo-source-root t)
                                   "Select Dynamo file:" elyo-source-root))
  (when (elyo-is-dynamo? file-path)
    (unwind-protect
        (progn (elyo-disable-lsp-clients python-mode-hook)
               (elyo-convert-to-python file-path t)
               (switch-to-buffer (last-buffer)))
      (elyo-enable-lsp-clients python-mode-hook))))


(defun elyo-dynamo-folder-to-python (&optional directory)
  "Export every python-node of any Dynamo files to python files in DIRECTORY."
  (interactive "D")
  (unwind-protect
      (progn (elyo-disable-lsp-clients python-mode-hook)
             (dolist (file-path (elyo-dynamo-files-in directory))
               (when (elyo-is-dynamo? file-path)
                 (elyo-convert-to-python file-path t)))
             (switch-to-buffer (last-buffer)))
    (elyo-enable-lsp-clients python-mode-hook)))


(define-minor-mode elyo-dynamo-mode
  "Toggles global elyo-mode."
  :global t
  :group 'elyo-dynamo
  :lighter " elyo-dynamo"
  :keymap elyo-dynamo-mode-map
  (if elyo-dynamo-mode
      (elyo--dynamo-add-hooks)
    (elyo--dynamo-remove-hooks)))


(provide 'elyo-dynamo)
;;; elyo-dynamo.el ends here
