;;; elyo-python.el --- Dynamo BIM package -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides functionality to change Python code in a Dynamo file.
;;
;;; Code:

(require 'elyo-path)
(require 'elyo-python-env)
(require 'elyo-python-format)
(require 'elyo-utils)


(defcustom elyo-python-keymap-prefix "C-p"
  "The prefix for elyo-mode key bindings."
  :type 'string
  :group 'elyo-python)


(defun elyo-python-key (key)
  "Return Emacs key representation of KEY."
  (kbd (elyo-pydyn-key elyo-python-keymap-prefix key)))


(defun elyo--python-mode-map-create ()
  "Define dynamo keymap."
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (elyo-python-key "i") #'elyo-python-ignore-to-inputs)
    (define-key key-map (elyo-python-key "I") #'elyo-python-ignore-to-errors)
    (define-key key-map (elyo-python-key "f") #'elyo-python-formatter-disable)
    (define-key key-map (elyo-python-key "F") #'elyo-python-formatter-enable)
    (define-key key-map (elyo-python-key "g") #'elyo-python-goto-dynamo-node)
    (define-key key-map (elyo-python-key "n") #'elyo-python-to-dynamo-node)
    (define-key key-map (elyo-python-key "s") #'elyo-python-to-dynamo-script)
    (define-key key-map (elyo-python-key "S") #'elyo-python-to-dynamo-folder)
    (define-key key-map (elyo-python-key "t") #'elyo-buffer-tabify)
    (define-key key-map (elyo-python-key "T") #'elyo-buffer-untabify)
    (define-key key-map (elyo-python-key "r") #'elyo-python-node-rename)
    (define-key key-map (elyo-python-key "v") #'elyo-python-node-geometry-set)
    key-map))


(defvar elyo-python-mode-map (elyo--python-mode-map-create)
  "The keymap for elyo-basic-mode.")

(add-to-list 'minor-mode-alist '(elyo-python-mode " elyo-python"))
(add-to-list 'minor-mode-map-alist (cons 'elyo-python-mode elyo-python-mode-map));;


;;;###autoload
(defun elyo-python-ignore-to-inputs ()
  "Add type: ignore to all Dynamo Node inputs."
  (interactive)
  (when (elyo-is-python?)
    (elyo--while-regex elyo-dynamo-input-regex
                       'elyo-python-ignore-add)
    (if (and (called-interactively-p 'interactive)
             (buffer-modified-p))
        (save-buffer))))


;;;###autoload
(defun elyo-python-ignore-to-errors ()
  "Search for known errors and add type ignore to it."
  (interactive)
  (elyo-convert-type-ignore-add))


(defun elyo-python-ignore-clean-buffer ()
  "Clean type: ignore in current buffer."
  (interactive)
  (save-excursion
    (elyo--while-regex
     (elyo-python-command-regex elyo-python-type-ignore)
     'elyo--python-ignore-clean)))


;;;###autoload
(defun elyo-python-buffer-clean ()
  "Remove special python comments in current buffer."
  (interactive)
  (elyo-python-ignore-clean-buffer)
  (elyo-python-formatter-clean-buffer))


;;;###autoload
(defun elyo-python-goto-dynamo-node ()
  "Search for code at point in source Dynamo file and select it."
  (interactive)
  (when (elyo-is-python-export?)
    (elyo-convert-goto-code (elyo-convert-to-dynamo
                             (elyo--current-line)))))


(defun elyo--python-code-clean ()
  "Return code of current buffer with removed python comments."
  (let ((code (buffer-string)))
    (with-temp-buffer
      (insert code)
      (when (elyo-convert-local-var?)
        (goto-char (point-min))
        (delete-line))
      (elyo-python-buffer-clean)
      (buffer-string))))


(defun elyo--python-to-dyn-node (file-path)
  "Replace python code of Dynamo node in FILE-PATH."
  (when (elyo-is-python-export? file-path)
    (with-current-buffer (elyo-buffer-by file-path)
      (elyo-convert-python-to-dynamo (elyo--python-code-clean)))))


;;;###autoload
(defun elyo-python-to-dynamo-node (&optional file-path)
  "Replace python code in FILE-PATH in Dynamo node."
  (interactive (list buffer-file-name))
  (elyo--python-to-dyn-node file-path)
  (elyo-switch-to-source))


;;;###autoload
(defun elyo-python-to-dynamo-script (&optional file-path)
  "Replace all python code in directory of FILE-PATH in Dynamo script."
  (interactive (list buffer-file-name))
  (require 'json-mode)
  (unwind-protect
      (progn (elyo-disable-lsp-clients json-mode-hook)
             (when (elyo-is-python? file-path)
               (let ((directory (file-name-parent-directory file-path)))
                 (dolist (python-path (elyo-python-files-in directory))
                   (elyo--python-to-dyn-node python-path)))
               (elyo-switch-to-source)))
    (elyo-enable-lsp-clients json-mode-hook)))


;;;###autoload
(defun elyo-python-to-dynamo-folder (&optional directory)
  "Replace all python code in DIRECTORY and replace code in the Dynamo files."
  (interactive "D")
  (require 'json-mode)
  (unwind-protect
      (progn (elyo-disable-lsp-clients json-mode-hook)
             (dolist (file-path (elyo-python-files-in directory t))
               (elyo--python-to-dyn-node file-path)))
    (elyo-enable-lsp-clients json-mode-hook)))


;;;###autoload
(defun elyo-python-node-geometry-set ()
  "Toggle show geometry value in dynamo source node."
  (interactive)
  (when (elyo-is-python-export? buffer-file-name)
    (let ((show-geom (read-answer "Show geometry? "
                                  '(("yes"  ?y "Do show geometry")
                                    ("no"   ?n "Do not show geometry")
                                    ("quit" ?q "exit")))))
      (let ((show (s-equals? show-geom "yes")))
        (elyo-convert-node-geometry-set show)
        (elyo-switch-to-source)
        (when (buffer-modified-p)
          (save-buffer))))))


;;;###autoload
(defun elyo-python-node-rename ()
  "Rename Dynamo python node in source file."
  (interactive)
  (when (elyo-is-python-export? buffer-file-name)
    (let* ((current-name (elyo-convert-node-name))
           (new-name (read-string "New Dynamo name: " current-name)))
      (unless (s-equals? new-name current-name)
        (save-excursion (elyo-convert-node-rename new-name))
        (elyo-switch-to-source)
        (when (buffer-modified-p)
          (save-buffer))))))


(define-minor-mode elyo-python-mode
  "Toggles global elyo python mode."
  :global t
  :group 'elyo-python
  :lighter " elyo-python"
  :keymap elyo-python-mode-map

  (defvar elyo--first-hook nil)
  (unless elyo--first-hook
    (elyo-python-add-hooks)
    (elyo-python-formatter-config)
    (setq elyo--first-hook t))

  (unless elyo-convert-process-running
    (message "elyo python hook %s" elyo-python-mode)
    (if elyo-python-mode
        (progn (message "Activate ELYO PYTHON (State Changed)")
               (elyo--python-setup-mode)
               (setq-local apheleia-formatter 'autopep8))
      (message "Deactivate ELYO PYTHON (State Changed)"))))

(defvar elyo--python-hooks (list 'doom-switch-buffer-hook
                                 'doom-switch-window-hook)
  "Hooks to register when minor mode is enabled.")

(defun elyo-python-add-hooks ()
  "Register hooks of the mode."
  (dolist (hook elyo--python-hooks)
    (add-hook hook #'elyo--python-mode-hook 99)))


(defvar elyo--python-local-hooks (list 'python-mode-local-vars-hook
                                       'revert-buffer-internal-hook)
  "Hooks to register when minor mode is enabled.")


(defun elyo-python-add-local-hooks ()
  "Register hooks of the mode."
  ;; (add-hook 'apheleia-post-format-hook #'elyo-python-tabify-after-formatter)
  (dolist (hook elyo--python-local-hooks)
    (add-hook hook #'elyo--python-mode-hook 99)))


(defun elyo--python-active? ()
  "Hook open a python file."
  (and elyo-python-mode
       (not elyo-convert-process-running)))


(defun elyo--python-mode-hook ()
  "Hook open a python file."
  (when (elyo--python-active?)
    (when (elyo-is-python-export?)
      (setq-local apheleia-formatters-respect-indent-level t)
      (if (elyo-is-py2-engine?)
          (elyo-python2-env-config)
        (if (elyo-is-py3-engine?)
            (elyo-python3-env-config))))))


(defun elyo--python-setup-mode ()
  "Configure elyo python minor mode."
  (when (elyo--python-active?)
    (elyo-python-formatter-config)
    (elyo-python-env-general-config)
    (elyo-python-env-pyright-config)
    (elyo-python-env-flycheck-config)
    (elyo-python-add-local-hooks)))


(defun elyo-major-mode-changed ()
  "Toggle Minor-Mode on/off based major mode."
  (unless elyo-convert-process-running
    (if (equal major-mode 'python-mode)
        (unless elyo-python-mode
          (message "Activate ELYO PYTHON (Major Mode Changed)")
          (elyo-python-mode 1))
      (unless (not elyo-python-mode)
        (message "Deactivate ELYO PYTHON (Major Mode Changed)")
        (elyo-python-mode -1)))))

(add-hook 'after-change-major-mode-hook #'elyo-major-mode-changed)

(defun elyo-python-activate-hook ()
  "Hook open a python file."
  (unless elyo-convert-process-running
    (if (or (equal major-mode 'python-mode)
            (derived-mode-p 'python-mode))
        (progn (message "Activate ELYO PYTHON (Major Mode Changed)")
               (setq elyo-python-mode 1))
      (progn (message "Deactivate ELYO PYTHON (Major Mode Changed)")
             (etq elyo-python-mode -1)))))


(provide 'elyo-python)
;;; elyo-python.el ends here
