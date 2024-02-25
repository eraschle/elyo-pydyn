;;; elyo-pydyn-utils.el --- DOOM Dynamo package -*- lexical-binding: t; -*-

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
;;
;;; Commentary:
;;
;; This module provides everything else
;;
;;; Code:

(require 'lsp)
(require 'lsp-headerline)

(defcustom elyo-pydyn-keymap-prefix "C-c C-y"
  "Prefix for `elyo-dynamo' and `elyo-python' minor mode bindings."
  :type 'string
  :group 'elyo-pydyn)


(defun elyo-pydyn-key (&rest keys)
  "Return Emacs key representation of KEYS."
  (kbd (string-join
        (append (ensure-list elyo-pydyn-keymap-prefix)
                (ensure-list keys))
        " ")))


(defcustom elyo-pydyn-dynamo-input-regex "IN[^ -][^A-Za-z]\\(\[[0-9]+\]\\)?"
  "Regex for IN[0] variable in Dynamo Python scripts."
  :type 'string
  :group 'elyo-pydyn)


(defcustom elyo-pydyn-buffer-preview-prefix "Preview"
  "Prefix for preview buffer name."
  :type 'string
  :group 'elyo-pydyn)


(defun elyo-pydyn-buffer-preview-name? ()
  "Return non-nil if current buffer is a preview buffer."
  (and (string-prefix-p (format "** %s " elyo-pydyn-buffer-preview-prefix)
                        (buffer-name))
       (string-suffix-p " **" (buffer-name))))


(defun elyo-pydyn-buffer-preview-name (name)
  "Return preview buffer NAME with `elyo-pydyn-buffer-preview'."
  (concat (format "** %s " elyo-pydyn-buffer-preview-prefix)
          (format "%s ** " name)))


(defun elyo-pydyn-buffer-preview-get (name)
  "Return preview buffer with NAME."
  (let ((prev-name (elyo-pydyn-buffer-preview-name name)))
    (or (get-buffer prev-name)
        (get-buffer-create prev-name))))


;;;###autoload
(defun elyo-pydyn-buffer-by (path)
  "Return or create buffer of PATH."
  (or (get-file-buffer path)
      (find-file-noselect path t)
      (create-file-buffer path)))


;;;###autoload
(defun elyo-pydyn-save-buffer (buffer-or-path &optional switch other-windows kill)
  "Save BUFFER-OR-PATH if modified and SWITCH, OTHER-WINDOWS or KILL it if non-nil."
  (let ((buffer (if (bufferp buffer-or-path)
                    buffer-or-path
                  (elyo-pydyn-buffer-by buffer-or-path))))
    (when (buffer-modified-p buffer)
      (with-current-buffer buffer
        (save-buffer 1)))
    (cond (switch (switch-to-buffer buffer))
          (other-windows (switch-to-buffer-other-window buffer))
          (kill (kill-buffer buffer))
          (t nil))))


(defun elyo-pydyn-buffer-substring (start end &optional with-properties)
  "Return buffer substring between START and END.
WITH-PROPERTIES control if the substring contains properties or not."
  (if with-properties
      (buffer-substring start end)
    (buffer-substring-no-properties start end)))


(defun elyo-pydyn-current-line (&optional with-properties)
  "Return current line at point of the current buffer.
WITH-PROPERTIES control if the substring contains properties or not."
  (elyo-pydyn-buffer-substring (pos-bol) (pos-eol) with-properties))


(defun elyo-pydyn-while-search (search-for action-cb &optional do-action-cb ignore-case)
  "SEARCH-FOR and apply ACTION-CB & DO-ACTION-CB, IGNORE-CASE to match."
  (let ((case-fold-search ignore-case))
    (goto-char (point-min))
    (while (search-forward search-for (point-max) t)
      ;; (goto-char (match-beginning 0))
      (if (and do-action-cb (funcall do-action-cb))
          (funcall action-cb)
        (unless do-action-cb
          (funcall action-cb)))
      (end-of-line))))


(defun elyo-pydyn-while-regex (rx action-cb &optional do-action-cb ignore-case)
  "SEARCH-FOR RX and apply ACTION-CB & DO-ACTION-CB, IGNORE-CASE to match."
  (let ((case-fold-search ignore-case))
    (goto-char (point-min))
    (while (re-search-forward rx (point-max) t)
      (goto-char (match-beginning 0))
      (if (and do-action-cb (funcall do-action-cb))
          (funcall action-cb)
        (unless do-action-cb
          (funcall action-cb)))
      (end-of-line))))


(defun elyo-pydyn--name-get (path &optional prefix)
  "Return PATH without PREFIX if non-nil otherwise file name."
  (if prefix
      (string-remove-prefix prefix path)
    (file-name-base path)))


(defun elyo-pydyn--name-path-list (paths &optional prefix)
  "Return list with PREFIX free name and path of PATHS."
  (seq-map (lambda (path)
             (list (elyo-pydyn--name-get path prefix) path))
           paths))


(defun elyo-pydyn-selection-get (paths prompt &optional prefix)
  "Return user path selection from PATHS. PROMPT is show to user.
PREFIX will be removed from PATHS."
  (let* ((name-and-path (elyo-pydyn--name-path-list (-flatten paths) prefix))
         (selected (elyo-pydyn-choose-get name-and-path prompt)))
    (catch 'found-it
      (dolist (name-path name-and-path)
        (when (string-equal (seq-first name-path) selected)
          (throw 'found-it (seq-elt name-path 1)))))))


(defun elyo-pydyn-choose-get (choose-list prompt &optional initial-input)
  "Return user selection from CHOOSE-LIST.
PROMPT is show to user and INITIAL-INPUT is pre selected if non-nil."
  (let ((completions-format 'vertical)
        (completions-sort 'alphabetical))
    (completing-read prompt choose-list nil t initial-input)))


(defun elyo-pydyn-choose-switch-or-kill (name)
  "Return User selection buffer of NAME (Python/Dynamo)."
  (let ((kill (format "Kill %s buffer." name))
        (switch (format "Switch to %s buffer (same window)." name))
        (switch-other (format "Switch to %s buffer (other window)." name)))
    (let ((selected (elyo-pydyn-choose-get (list switch-other switch kill)
                                           "Choose action?: ")))
      (cond ((equal selected kill) 'kill)
            ((equal selected switch) 'switch)
            ((equal selected switch-other) 'switch-other)
            (t nil)))))


(defun elyo-pydyn-is-switch (result)
  "Return non-nil when SWITCH buffer is equal RESULT."
  (equal result 'switch))


(defun elyo-pydyn-is-switch-other (result)
  "Return non-nil when SWITCH-OTHER is equal RESULT."
  (equal result 'switch-other))


(defun elyo-pydyn-is-kill (result)
  "Return non-nil if KILL buffer is equal RESULT."
  (equal result 'kill))


(defvar elyo-pydyn-process-running nil
  "Is non-nil during convert process.")


;;;###autoload
(defun elyo-pydyn-not-converting? ()
  "Return non-nil when no convert process is running."
  (not elyo-pydyn-process-running))


(defcustom elyo-pydyn-disabled-lsp-client
  '(('json-mode   . (list json-ls
                          json-ls-tramp
                          json-rpc))
    ('python-mode . (list lsp-pyright
                          pylsp
                          pyls
                          ruff-lsp-tramp
                          ruff-lsp
                          json-rpc)))
  "Toggle to CONVERSATION configuration LSP client are blocked for execution.
They are blocked the syntax of the code can also later be checked.
Otherwise they are really slow down the process. See `lsp-disabled-clients'"
  :type 'list
  :group 'elyo-pydyn)


(defcustom elyo-pydyn-start-hook nil
  "Hooks called before convert process starts."
  :type 'list
  :group 'elyo-pydyn)


(defcustom elyo-pydyn-end-hook nil
  "Hooks called after convert process is finish."
  :type 'list
  :group 'elyo-pydyn)


;;;###autoload
(defun elyo-pydyn-disable-lsp-clients ()
  "Disable lsp clients in `elyo-pydyn-disabled-lsp-client'."
  (setq lsp-disabled-clients elyo-pydyn-disabled-lsp-client)
  (run-hooks 'elyo-pydyn-start-hook)
  (setq elyo-pydyn-process-running t))


;;;###autoload
(defun elyo-pydyn-enable-lsp-clients ()
  "Enable LSP-client and restore attributes."
  (setq elyo-pydyn-process-running nil)
  (run-hooks 'elyo-pydyn-end-hook)
  (setq lsp-disabled-clients nil))


;;;###autoload
(defun elyo-pydyn-indent-width-setup (width)
  "Setup line indent WIDTH for the current buffer."
  (setq-local tab-width width
              standard-indent width
              evil-shift-width width))


;;;###autoload
(defcustom elyo-pydyn-python-2-name  "IronPython2"
  "Name of python 2 code / engine."
  :type 'string
  :group 'elyo-pydyn)


;;;###autoload
(defcustom elyo-pydyn-python-3-name "CPython3"
  "Name of python 3 code / engine."
  :type 'string
  :group 'elyo-pydyn)


;;;###autoload
(defun elyo-pydyn-is-python-2? (engine)
  "Return non-nil when ENGINE is PYTHON 2 engine."
  (and engine (equal engine elyo-pydyn-python-2-name)))


;;;###autoload
(defun elyo-pydyn-is-python-3? (engine)
  "Return non-nil when ENGINE is CPython 3 engine."
  (and engine (equal engine elyo-pydyn-python-3-name)))


;;;###autoload
(defun elyo-pydyn-buffer-tabify ()
  "Convert buffer from SPACE to TABS indentation."
  (interactive)
  (unless indent-tabs-mode
    (indent-tabs-mode 1))
  (tabify (point-min) (point-max)))


;;;###autoload
(defun elyo-pydyn-buffer-untabify ()
  "Convert buffer from TAB to SPACE indentation."
  (interactive)
  (when indent-tabs-mode
    (indent-tabs-mode -1))
  (untabify (point-min) (point-max)))


(defun elyo-pydyn-buffer-breadcrumb-on ()
  "Convert buffer from TAB to SPACE indentation."
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-enable-symbol-numbers t))


(defun elyo-pydyn-buffer-breadcrumb-off ()
  "Convert buffer from TAB to SPACE indentation."
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-enable-symbol-numbers nil))


(provide 'elyo-pydyn-utils)
;;; elyo-pydyn-utils.el ends here
