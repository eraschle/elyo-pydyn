;;; elyo/dynamo/config.el -*- lexical-binding: t; -*-

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

(require 'python)
(require 'json)


;;;###autoload
(defun pydyn-python-mode-setup ()
  "Hook to setup ELYO PYTHON MINOR MODE."
  (when (elyo-not-converting?)
    (pydyn-py-activate-venv)
    (pydyn-formatter-config)
    (pydyn-pyright-config)
    (when (or (elyo-is-python-2? node-engine)
              (elyo-is-python-intern?))
      (pydyn-py2-config))
    (when (elyo-is-python-3? node-engine)
      (pydyn-py3-config))
    (message "ELYO-PYTHON setup")))


;;;###autoload
(defun elyo-file-find-uuid-in-library ()
  "Return preview completion with references of custom node."
  (interactive)
  (when (and (elyo-is-python-export?) (elyo-dynamo-is-custom?))
    (let ((file-info (elyo-convert-dynamo-file-info)))
      (+vertico-file-search
        :query (concat "\"FunctionSignature\": \""
                       (plist-get file-info :node-id) "\",")
        :in elyo-source-root
        :all-files nil))))

(defvar pydyn--python-disable-hook nil
  "Variable to store value of other mode hook.")

(defun pydyn-python-convert-start-h ()
  "Hooks run before convert process has started."
  (setq pydyn--python-disable-hook json-mode-hook
        json-mode-hook nil)
  ;; Will be overwriten/deleted after convert file
  (add-hook 'json-mode-hook #'elyo-dynamo-json-config))

(defun pydyn-python-convert-end-h ()
  "Hooks run after convert process is finish."
  (when pydyn--python-disable-hook
    (setq json-mode-hook pydyn--python-disable-hook
          pydyn--python-disable-hook nil)))

(defvar pydyn-source "<Pfad Hauptordner Skripts>")
(defvar pydyn-export "<Pfad Hauptordner Python Export KEIN UNTERORDNER VON PYDYN-SOURCE>")


(use-package! elyo-python
  :hook (python-mode . elyo-python-mode-activate)
  :init
  (add-hook 'elyo-python-mode-hook 'pydyn-python-mode-setup)
  (add-hook 'elyo-convert-start-hook 'pydyn-python-convert-start-h)
  (add-hook 'elyo-convert-end-hook 'pydyn-python-convert-end-h)
  (pydyn-formatter-config-add)
  :config
  (setq pydyn-venv-path "<Pfad zur VENV-Ordner>"
        pydyn-stub-path "<Pfad zum Stubs-Ordner>"
        elyo-source-root (file-truename pydyn-source)
        elyo-source-root  (file-truename pydyn-export)
        elyo-python-formatter-off "# autopep8: off"
        elyo-python-formatter-on "# autopep8: on"
        elyo-python-type-ignore "# type: ignore"
        elyo-python-indent-width 4
        elyo-python-line-length 110

        ;; if a row of exported python code contains a value in this list will be deleted
        elyo-python-delete-contain (list "# Load the Python Standard"
                                         "Phython-Standard- und DesignScript-Bibliotheken laden"
                                         "# The inputs to this node will be stored"
                                         "Die Eingaben für diesen Block werden in Form einer Liste in den IN-Variablen gespeichert."
                                         "dataEnteringNode = IN"
                                         "Assign your output to the OUT variable."
                                         "Weisen Sie Ihre Ausgabe der OUT-Variablen zu.")

        ;; A tyoe: ignore is added if one regex match
        elyo-python-type-ignore-regex (list elyo-dynamo-input-regex
                                            "import [a-zA-z ,]*Enum"
                                            "UnwrapElement(.*)"
                                            "clr.Reference\[[a-zA-Z]+\]\(\)"
                                            "List\[[a-zA-Z]+\]\(.*\)")

        ;; A tyoe: ignore is added if a row contain a item in this list
        elyo-python-type-ignore-contain (list "dataEnteringNode = IN"
                                              "clr.ImportExtensions(Revit.Elements)"
                                              "clr.ImportExtensions(Revit.GeometryConversion)"
                                              "Application.DocumentManager.MdiActiveDocument"
                                              "TransactionManager.Instance."
                                              "LabelUtils.GetLabelFor"
                                              "basestring"))

  (map! :map python-mode-map
        :localleader
        (:prefix ("y" . "elyo BIM")
         :desc "Add IGNORE to IN"              :n "i" #'elyo-python-ignore-to-inputs
         :desc "Add IGNORE to Errors"          :n "I" #'elyo-python-ignore-to-errors
         :desc "Remove bracket if-statement"   :n "b" #'elyo-python-if-remove-bracket
         :desc "Clean buffer comments"         :n "c" #'elyo-python-buffer-clean
         :desc "Formatter disable in region"   :n "f" #'elyo-python-formatter-disable
         :desc "Formatter enable"              :n "F" #'elyo-python-formatter-enable
         :desc "Goto Dynamo script node"       :n "g" #'elyo-python-goto-dynamo-node
         :desc "Replace python in Node"        :n "n" #'elyo-python-to-dynamo-node
         :desc "Replace python in Script"      :n "s" #'elyo-python-to-dynamo-script
         :desc "Replace python in Folder"      :n "S" #'elyo-python-to-dynamo-folder
         :desc "Tabify Buffer"                 :n "t" #'elyo-buffer-tabify
         :desc "Untabify"                      :n "T" #'elyo-buffer-untabify
         :desc "Toggle type: ignore"           :ni "y" #'elyo-python-ignore-toggle
         )))


(defun pydyn-dynamo-format-maybe-inhibit-h ()
  (elyo-is-dynamo?))

(defvar pydyn--dynamo-disable-hook nil
  "Variable to store value of other mode hook.")

(defun pydyn-dynamo-convert-start-h ()
  "Hook to disable other mode hooks before convert process."
  (setq pydyn--dynamo-disable-hook python-mode-hook
        python-mode-hook nil))

(defun pydyn-dynamo-convert-end-h ()
  "Hook to enable other mode hooks after convert process."
  (when pydyn--dynamo-disable-hook
    (setq python-mode-hook pydyn--dynamo-disable-hook
          pydyn--dynamo-disable-hook nil)))

(defun pydyn-dynamo-mode-setup ()
  "Hook to setup ELYO DYNAMO MINOR MODE."
  (message "SETUP ELYO-DYNAMO"))


(use-package! elyo-dynamo
  :hook (json-mode . elyo-dynamo-mode-activate)
  :config
  (setq elyo-source-root (file-truename pydyn-source)
        elyo-export-root (file-truename pydyn-export))

  (add-to-list 'apheleia-inhibit-functions 'pydyn-dynamo-format-maybe-inhibit-h)
  (add-hook 'elyo-dynamo-mode-hook 'pydyn-dynamo-mode-setup)
  (add-hook 'elyo-convert-start-hook 'pydyn-dynamo-convert-start-h)
  (add-hook 'elyo-convert-end-hook 'pydyn-dynamo-convert-end-h)

  (map! :map json-mode-map
        :localleader
        (:prefix ("y" . "elyo BIM")
         :desc "Script to Pyhton"              :n "s" #'elyo-dynamo-script-to-python
         :desc "Folder To Python"              :n "f" #'elyo-dynamo-folder-to-python
         :desc "Jump to node"                  :n "j" #'elyo-dynamo-jump-to-node
         :desc "Preview node"                  :n "p" #'elyo-dynamo-python-code-preview
         :desc "Goto Python File"              :n "g" #'elyo-dynamo-goto-python
         :desc "To Python Code at point"       :n "n" #'elyo-dynamo-at-point-to-python
         :desc "Script: Delete orphan code"    :n "S" #'elyo-dynamo-clean-orphan-code-file
         :desc "Folder: Delete orphan code"    :n "F" #'elyo-dynamo-clean-orphan-code-folder)))
