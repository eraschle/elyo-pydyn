;;; elyo-utils.el --- Dynamo BIM package -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides functionality to change Python code in a Dynamo file.
;;
;;; Code:

(require 'dash)
(require 'python)
(require 's)
(require 'subr-x)
(require 'undo-fu-session)
(require 'lsp)

(defcustom elyo-pydyn-keymap-prefix "C-c C-y"
  "Prefix for `elyo-dynamo' and `elyo-python' minor mode bindings."
  :type 'string
  :group 'elyo-dynamo)


(defun elyo-pydyn-key (&rest keys)
  "Return Emacs key representation of KEYS."
  (kbd (s-join " " (append (ensure-list elyo-pydyn-keymap-prefix)
                           (ensure-list keys)))))


(defcustom elyo-dynamo-input-regex "IN[^ -][^A-Za-z]\\(\[[0-9]+\]\\)?"
  "Regex for IN[0] variable in Dynamo Python scripts."
  :type 'list
  :group 'elyo-dynpy)


(defun elyo-buffer-by (path)
  "Return or create buffer of PATH."
  (or (get-file-buffer path)
      (find-file-noselect path)
      (create-file-buffer path)))


(defun elyo--buffer-substring (start end &optional with-properties)
  "Return the buffer substring between START and END, WITH-PROPERTIES if non-nil."
  (if with-properties
      (buffer-substring start end)
    (buffer-substring-no-properties start end)))


(defun elyo--current-line (&optional with-properties)
  "Return the current line of buffer and WITH-PROPERTIES if non-nil."
  (elyo--buffer-substring (pos-bol) (pos-eol) with-properties))


(defun elyo--cursor-to-left-border ()
  "Scroll the screen to move cursor as close as possible to left border."
  (if (= 0 (window-left-column))
      (scroll-left (1- (current-column)))
    (scroll-left (1- (- (current-column) (window-left-column)))))
  (recenter))


(defun elyo--while-search (search-for action-cb &optional do-action-cb ignore-case)
  "Search SEARCH-FOR and apply ACTION-CB & DO-ACTION-CB, IGNORE-CASE every match."
  (let ((case-fold-search ignore-case))
    (goto-char (point-min))
    (while (search-forward search-for (point-max) t)
      ;; (goto-char (match-beginning 0))
      (if (and do-action-cb (funcall do-action-cb))
          (funcall action-cb)
        (unless do-action-cb
          (funcall action-cb)))
      (end-of-line))))


(defun elyo--while-regex (rx action-cb &optional do-action-cb ignore-case)
  "Find while is at end for RX and apply ACTION-CB & DO-ACTION-CB, IGNORE-CASE."
  (let ((case-fold-search ignore-case))
    (goto-char (point-min))
    (while (re-search-forward rx (point-max) t)
      (goto-char (match-beginning 0))
      (if (and do-action-cb (funcall do-action-cb))
          (funcall action-cb)
        (unless do-action-cb
          (funcall action-cb)))
      (end-of-line))))


(defun elyo--name-get (path &optional prefix)
  "Return a PATH free PREFIX if PATH is non-nil otherwise file name."
  (if prefix
      (string-remove-prefix prefix path)
    (file-name-base path)))


(defun elyo--name-path-list (paths &optional prefix)
  "Return PATH free PREFIX and path of all PATHS."
  (seq-map (lambda (path)
             (list (elyo--name-get path prefix) path))
           paths))


(defun elyo-selection-get (paths prompt &optional prefix)
  "Create with PATHS, PROMPT and PREFIX completing read for a user selection."
  (setq completions-format 'vertical)
  (setq completions-sort 'alphabetical)
  (let* ((name-and-path (elyo--name-path-list (-flatten paths) prefix))
         (selected (completing-read prompt name-and-path nil t)))
    (catch 'found-it
      (dolist (name-path name-and-path)
        (when (string-equal (seq-first name-path) selected)
          (throw 'found-it (seq-rest name-path)))))))


(defvar elyo-convert-process-running nil
  "Flag non-nil during convert process.")

(defvar elyo-destination-mode-hook nil
  "Destination mode hooks to remove during process.")

(defcustom elyo-convert-disabled-lsp-client '((json-mode   . (json-rpc json-ls-tramp jq-lsp))
                                              (python-mode . (lsp-pyright pylsp pyls jq-lsp)))
  "Toggle to CONVERSATION configuration LSP client are blocked for execution.
They are blocked the syntax of the code can also later be checked.
Otherwise they are really slow down the process. See `lsp-disabled-clients'"
  :type 'list
  :group 'elyo-dynpy)


(defun elyo-disable-lsp-clients (dest-hook)
  "Function to disable lsp clients and remove DEST-MODE.
DO NOT forget to enable them after conversation."
  (unwind-protect
      (setq lsp-disabled-clients elyo-convert-disabled-lsp-client
            elyo-destination-mode-hook dest-hook
            dest-hook nil)
    (setq elyo-convert-process-running t)))


(defun elyo-enable-lsp-clients (dest-hook)
  "Toggle to NORMAL configuration (LSP client allowed & DEST-HOOK restored)."
  (unwind-protect
      (setq lsp-disabled-clients nil
            dest-hook elyo-destination-mode-hook
            elyo-destination-mode-hook nil)
    (setq elyo-convert-process-running nil)))


(defun elyo-buffer-tabify ()
  "Convert all SPACE into TABS indentation."
  (interactive)
  (tabify (point-min) (point-max)))


(defun elyo-buffer-untabify ()
  "Convert all TAB into SPACE indentation."
  (interactive)
  (untabify (point-min) (point-max)))


(provide 'elyo-utils)
;;; elyo-utils.el ends here
