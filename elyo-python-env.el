;;; elyo-python-env.el --- Dynamo BIM package -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides functionality to change Python code in a Dynamo file.
;;
;;; Code:
(require 'elyo-convert)
(require 'elyo-path)
(require 'elyo-utils)

(require 's)
(require 'flycheck)
(require 'lsp-pyright)
(require 'mode-local)
(require 'vertico)


(defcustom elyo-python2-version "2.7"
  "Path to the python stubs folder."
  :type 'string
  :group 'elyo-python)


(defcustom elyo-python3-version "3.8.10"
  "Path to the python stubs folder."
  :type 'string
  :group 'elyo-python)


(defcustom elyo-python-line-length 80
  "Max allowed line length."
  :type 'string
  :group 'elyo-python)


(defun elyo-python-env-general-config ()
  "Set python version independent settings."
  (setq-local tab-width 4
              python-indent-offset 4
              python-indent-def-block-scale 1 ;; Fix of indent width of 8
              python-indent-guess-indent-offset nil
              fill-column elyo-python-line-length
              apheleia-formatters-respect-fill-column t
              apheleia-formatter 'autopep8))


(defcustom elyo-python-error-levels '(("^E9.*$" . error)
                                      ("^F82.*$" . error)
                                      ("^F83.*$" . error)
                                      ("^D.*$" . info)
                                      ("^N.*$" . info)
                                      ("^W19.*$" . info))
  "Mapping from error numbers to error level in IDE."
  :type 'list
  :group 'elyo-python)


(defcustom elyo-python-line-length 80
  "Max allowed line length."
  :type 'integer
  :group 'elyo-python)


(defun elyo-python-env-flycheck-config ()
  "Setup flycheck python settings."
  (setq-local flycheck-flake8-maximum-line-length elyo-python-line-length
              flycheck-flake8-error-level-alist elyo-python-error-levels))


(defcustom elyo-python-stubs-path nil
  "Path to the python stubs folder."
  :type 'string
  :group 'elyo-python)


(defun elyo-python-env-pyright-config ()
  "Setup flycheck python settings."
  (setq-local lsp-pyright-disable-organize-imports t
              lsp-pyright-stub-path elyo-python-stubs-path))


(defun elyo--python-exe (py-version)
  "Return python executable based on PY-VERSION."
  (concat "python" (seq-first (string-split py-version "[\.]"))))


(defun elyo--python-set-version (py-version)
  "Set python version inkl. some executable based on PY-VERSION."
  ;; (setq-local +pyenv--version py-version)
  (unless (string-prefix-p py-version (getenv "PYENV_VERSION"))
    (message "Set PYENV %s" py-version)
    ;; (let ((py-exe (elyo--python-exe py-version)))
    ;;   (setq-mode-local python-mode
    ;;                    pythonic-interpreter py-exe
    ;;                    python-interpreter py-exe
    ;;                    python-shell-interpreter py-exe)
    ;; )
    (setq-local +pyenv--version elyo-python2-version)
    (pyenv-mode-set py-version)))


(defun elyo-python2-env-config ()
  "Configuration for python 2.x."
  (message "Set python 2")
  (elyo--python-set-version elyo-python2-version)
  (unless indent-tabs-mode
    (indent-tabs-mode 1)
    (setq-local indent-tabs-mode t
                tab-width 4)))


(defun elyo-python3-env-config ()
  "Configuration for python 3.x."
  (message "Set python 3")
  (elyo--python-set-version elyo-python3-version)
  (indent-tabs-mode -1))


;;;###autoload
(defun elyo-file-find-uuid-in-library ()
  "Return preview completion with references of custom node."
  (interactive)
  (when (and (elyo-is-python-export?) (elyo-convert-is-custom?))
    (let ((file-info (elyo-convert-dynamo-file-info)))
      (+vertico-file-search
        :query (concat "\"FunctionSignature\": \""
                       (plist-get file-info :node-id) "\",")
        :in elyo-source-root :all-files nil))))


(provide 'elyo-python-env)
;;; elyo-python-env.el ends here
