;; MIT License

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

(require 's)
(require 'apheleia)
(require 'lsp-pyright)
(require 'pyvenv)



(defvar pydyn-ignore-values (list "E402" "E226" "W1" "W5" "W6")
  "Command option of `apheleia-formatter'.")


(defvar pydyn-formatter
  '(elyo . ("autopep8"
            (apheleia-formatters-fill-column "--max-line-length")
            (list "--ignore" (s-join "," pydyn-ignore-values))
            "-"))
  "`apheleia-formatter' configuration.")


(defun pydyn-formatter-maybe-inhibit-h ()
  elyo-pydyn-processing)

;;;###autoload
(defun pydyn-formatter-config-add ()
  "Add formatter to `apheleia-formatters'."
  (add-to-list 'apheleia-formatters pydyn-formatter)
  (add-to-list 'apheleia-inhibit-functions 'pydyn-formatter-maybe-inhibit-h))


;;;###autoload
(defun pydyn-formatter-config ()
  "Setup aphelia python formatter settings."
  (setq-local apheleia-formatters-respect-fill-column t
              apheleia-formatter 'elyo))


(defvar pydyn-venv-path ""
  "Path to python virtual enviroment.")


(defvar pydyn-stub-path ""
  "Path to root directory of Python stub files")


(defvar pydyn-typeshed-path []
  "Paths to look for typeshed modules.")


;;;###autoload
(defun pydyn-py-activate-venv ()
  "Activate python virtual env."
  (when (file-exists-p venv-path)
    (pyvenv-activate pydyn-venv-path)))


;;;###autoload
(defun pydyn-pyright-config ()
  "Setup LSP-PYRIGHT."
  (setq-local lsp-pyright-disable-organize-imports t
              lsp-pyright-multi-root nil
              ;; lsp-pyright-use-library-code-for-types nil
              lsp-pyright-diagnostic-mode "openFilesOnly"
              lsp-pyright-typeshed-paths pydyn-typeshed-path)
  (let ((stub-path (file-truename pydyn-stub-path))
        (venv-path (file-truename pydyn-venv-path)))
    (when (file-exists-p stub-path)
      (setq-local lsp-pyright-stub-path stub-path))
    (when (file-exists-p venv-path)
      (setq-local lsp-pyright-venv-path venv-path))))


(defun pydyn-py2-config ()
  "Configuration for python 2.x."
  (unless indent-tabs-mode
    (indent-tabs-mode t)))


(defun pydyn-py3-config ()
  "Configuration for python 3.x."
  (when indent-tabs-mode
    (indent-tabs-mode t)))
