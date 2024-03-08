;;; elyo-python.el --- DOOM Dynamo package -*- lexical-binding: t; -*-

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
;; This module provides `elyo-python-mode'
;;
;;; Code:

(require 'elyo-pydyn-path)
(require 'elyo-pydyn-convert)
(require 'elyo-pydyn-utils)

(require 's)
(require 'rect)

(defcustom elyo-python-keymap-prefix "C-p"
  "The prefix for elyo-python-mode key bindings."
  :type 'string
  :group 'elyo-pydyn)


(defun elyo-python-key (key)
  "Return Emacs key representation of KEY."
  (kbd (elyo-pydyn-key elyo-python-keymap-prefix key)))


(defun elyo-python--mode-map-create ()
  "Define python keymap."
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (elyo-python-key "m") #'elyo-python-mode-on)
    (define-key key-map (elyo-python-key "M") #'elyo-python-mode-off)
    (define-key key-map (elyo-python-key "b") #'elyo-python-if-remove-bracket)
    (define-key key-map (elyo-python-key "b") #'elyo-python-backslash-ensure)
    (define-key key-map (elyo-python-key "f") #'elyo-python-formatter-disable)
    (define-key key-map (elyo-python-key "F") #'elyo-python-formatter-enable)
    (define-key key-map (elyo-python-key "g") #'elyo-python-goto-dynamo-node)
    (define-key key-map (elyo-python-key "i") #'elyo-python-ignore-to-inputs)
    (define-key key-map (elyo-python-key "I") #'elyo-python-ignore-to-errors)
    (define-key key-map (elyo-python-key "h") #'elyo-python-highlight-regex)
    (define-key key-map (elyo-python-key "H") #'elyo-python-unhighlight-regex)
    (define-key key-map (elyo-python-key "n") #'elyo-python-to-dynamo-node)
    (define-key key-map (elyo-python-key "r") #'elyo-python-node-rename)
    (define-key key-map (elyo-python-key "s") #'elyo-python-to-dynamo-script)
    (define-key key-map (elyo-python-key "S") #'elyo-python-to-dynamo-folder)
    (define-key key-map (elyo-python-key "t") #'elyo-pydyn-buffer-tabify)
    (define-key key-map (elyo-python-key "T") #'elyo-pydyn-buffer-untabify)
    (define-key key-map (elyo-python-key "v") #'elyo-python-node-geometry-set)
    (define-key key-map (elyo-python-key "y") #'elyo-python-ignore-toggle)
    key-map))


(defvar elyo-python-mode-map (elyo-python--mode-map-create)
  "The keymap for elyo-python-mode.")

(add-to-list 'minor-mode-alist '(elyo-python-mode " elyo-python"))
(add-to-list 'minor-mode-map-alist (cons 'elyo-python-mode elyo-python-mode-map));;

(defun elyo-python-command-regex-get(name-and-value)
  "Return regex for NAME-AND-VALUE of comment, like type: ignore."
  (format "#[ ]?%s[ ]?:[ ]?%s"
          (s-trim (car name-and-value))
          (s-trim (cadr name-and-value))))


(defun elyo-python-command-regex(comment)
  "Return regex for COMMENT, like type: ignore."
  (let ((wo-hash (s-trim (string-replace "#" "" comment))))
    (elyo-python-command-regex-get (s-split ":" wo-hash))))


(defcustom elyo-python-indent-width nil
  "Indent width of spaces in `python-mode'."
  :type 'integer
  :group 'elyo-pydyn)


(defun elyo-python-indent-width-setup ()
  "Set `elyo-python-indent-width' in current buffer."
  (when elyo-python-indent-width
    (elyo-pydyn-indent-width-set elyo-python-indent-width)))


(defcustom elyo-python-line-length nil
  "Maximal line length for python code line."
  :type 'integer
  :group 'elyo-pydyn)


(defun elyo-python-line-length-setup ()
  "Set `elyo-python-line-length' in current buffer."
  (when elyo-python-line-length
    (setq fill-column elyo-python-line-length)))


(defcustom elyo-python-type-ignore nil
  "Comment to supress type checker error."
  :type 'string
  :group 'elyo-pydyn)


(defun elyo-python-is-type-ignore? ()
  "Return non-nil if current line contain `elyo-python-type-ignore'."
  (when elyo-python-type-ignore
    (s-matches? (elyo-python-command-regex
                 elyo-python-type-ignore)
                (elyo-pydyn-current-line))))


(defun elyo-python--ignore-add ()
  "Return current line with append `elyo-python-type-ignore'."
  (when elyo-python-type-ignore
    (concat (s-trim-right (elyo-pydyn-current-line))
            (make-string 2 ? )
            elyo-python-type-ignore)))


;;;###autoload
(defun elyo-python-ignore-add()
  "Add `elyo-python-type-ignore' if not exist already."
  (interactive)
  (unless (elyo-python-is-type-ignore?)
    (replace-string-in-region (elyo-pydyn-current-line)
                              (elyo-python--ignore-add)
                              (pos-bol) (pos-eol))))


;;;###autoload
(defun elyo-python-ignore-toggle()
  "Toggle `elyo-python-type-ignore' in current line."
  (interactive)
  (when (and elyo-python-type-ignore
             (elyo-python-is-type-ignore?))
    (save-excursion
      (goto-char (pos-bol))
      (when (re-search-forward
             (elyo-python-command-regex elyo-python-type-ignore)
             (pos-eol) t 1)
        (elyo-python-ignore-remove-match))
      (elyo-python-ignore-add))))


(defun elyo-python--match-replaced(matched)
  "Return current line with MATCHED replaced."
  (s-trim-right (s-replace matched "" (elyo-pydyn-current-line))))


(defun elyo-python-ignore-remove-match ()
  "Return current line with last search match replaced."
  (let ((matched (elyo-pydyn-buffer-substring (match-beginning 0)
                                              (match-end 0))))
    (replace-string-in-region (elyo-pydyn-current-line)
                              (elyo-python--match-replaced matched)
                              (pos-bol) (pos-eol))))


;;;###autoload
(defun elyo-python-ignore-clean-buffer ()
  "Remove `elyo-python-type-ignore' in current buffer."
  (interactive)
  (when elyo-python-type-ignore
    (save-excursion
      (elyo-pydyn-while-regex (elyo-python-command-regex elyo-python-type-ignore)
                              'elyo-python-ignore-remove-match))))


(defcustom elyo-python-formatter-on nil
  "Comment for enable formatter in upcoming lines."
  :type 'string
  :group 'elyo-pydyn)


(defun elyo-python-formatter-add-on (end-point)
  "Return point of added `elyo-python-formatter-on' at END-POINT."
  (when elyo-python-formatter-on
    (save-excursion
      (let ((end-point (progn (goto-char end-point)
                              (pos-bol))))
        ;; First insert the end value
        (goto-char end-point)
        (if (s-blank? (elyo-pydyn-current-line))
            (progn (insert elyo-python-formatter-on)
                   (open-line 1))
          (ensure-empty-lines 1)
          (forward-line 1)
          (insert elyo-python-formatter-on)))
      (point))))


(defcustom elyo-python-formatter-off nil
  "Comment to disable formatter until `elyo-python-formatter-off'."
  :type 'string
  :group 'elyo-pydyn)


(defun elyo-python-formatter-add-off (start-point)
  "Return point of added `elyo-python-formatter-off' at START-POINT."
  (when elyo-python-formatter-off
    (save-excursion
      (let ((start-point (progn (goto-char start-point)
                                (pos-bol))))
        ;; Otherwise start would change the end position
        (goto-char start-point)
        (if (s-blank? (elyo-pydyn-current-line))
            (progn (insert elyo-python-formatter-off)
                   (ensure-empty-lines 1))
          (open-line 1)
          (insert elyo-python-formatter-off)))
      (point))))


;;;###autoload
(defun elyo-python-formatter-disable (start end)
  "Insert ON / OFF FORMATTER comment at START and END."
  (interactive "r")
  ;; First insert the end value
  (let ((end-point (elyo-python-formatter-add-on end))
        ;; Otherwise start would change the end position
        (start-point (elyo-python-formatter-add-off start)))
    (rectangle-forward-char (- end-point end))
    (rectangle-backward-char (- start start-point))))


(defun elyo-python--comment-next-search (comment)
  "Search for the next COMMENT from current point.
Return point of match or nil."
  (re-search-forward (elyo-python-command-regex comment)
                     (point-max) t 1))

(defun elyo-python--formatter-next (comment)
  "Return next position of COMMENT."
  (save-excursion
    (elyo-python--comment-next-search comment)
    (match-beginning 0)))


(defun elyo-python--formatter-previous (comment)
  "Return previous position of COMMENT."
  (save-excursion
    (re-search-backward (elyo-python-command-regex comment)
                        (point-min) t 1)
    (match-beginning 0)))


(defun elyo-python-formatter-on-pos ()
  "Return previous and next position of `elyo-python-formatter-on'."
  (let ((comment elyo-python-formatter-on))
    (cons (elyo-python--formatter-previous comment)
          (elyo-python--formatter-next comment))))


(defun elyo-python-formatter-off-pos ()
  "Return previous and next position of `elyo-python-formatter-off'."
  (let ((comment elyo-python-formatter-off))
    (cons (elyo-python--formatter-previous comment)
          (elyo-python--formatter-next comment))))


(defun elyo-python-formatter-is-inside? ()
  "Return non-nil if point is between ON / OFF comment."
  (let ((off-pos (elyo-python-formatter-off-pos))
        (on-pos (elyo-python-formatter-on-pos)))
    (< (car off-pos) (cdr on-pos))))


;;;###autoload
(defun elyo-python-formatter-enable ()
  "Remove formatter comment if point is between OFF / ON comment."
  (interactive)
  (when (elyo-python-formatter-is-inside?)
    (save-excursion
      (goto-char (car (elyo-python-formatter-off-pos)))
      (delete-line))
    (save-excursion
      (goto-char (cdr (elyo-python-formatter-on-pos)))
      (delete-line))))


;;;###autoload
(defun elyo-python-formatter-clean-buffer ()
  "Remove formatter comment in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (elyo-python--comment-next-search
           elyo-python-formatter-off)
      (goto-char (point-min))
      (while (not (eobp))
        (forward-line)
        (elyo-python-formatter-enable)))))


(defcustom elyo-python-delete-contain nil
  "Remove if any value of this list contain in current line."
  :type 'list
  :group 'elyo-pydyn)


(defun elyo-python-delete-comment-lines ()
  "Remove lines contain any value in `elyo-python-delete-contain'."
  (when elyo-python-delete-contain
    (dolist (search-for elyo-python-delete-contain)
      (elyo-pydyn-while-search search-for 'delete-line))))


(defcustom elyo-python-type-ignore-regex nil
  "Regex to add `elyo-python-type-ignore' if any regex matches."
  :type 'list
  :group 'elyo-pydyn)


;;;###autoload
(defun elyo-python-highlight-regex ()
  "Highlight `elyo-dynamo-input-regex' in current buffer."
  (interactive)
  (let ((hi-lock-auto-select-face t))
    (dolist (regex (completing-read-multiple
                    "Choose regex to highlight: "
                    elyo-python-type-ignore-regex))
      (highlight-regexp regex))))


;;;###autoload
(defun elyo-python-unhighlight-regex ()
  "Unhighlight all `elyo-dynamo-input-regex' in current buffer."
  (interactive)
  (dolist (regex elyo-python-type-ignore-regex)
    (unhighlight-regexp regex)))


(defcustom elyo-python-type-ignore-contain nil
  "Add `elyo-python-type-ignore' if current line contain any value in this."
  :type 'list
  :group 'elyo-pydyn)


;;;###autoload
(defun elyo-python-ignore-to-errors ()
  "Add `elyo-python-type-ignore' to known type checker errors."
  (interactive)
  (when elyo-python-type-ignore-regex
    (save-excursion
      (dolist (regex elyo-python-type-ignore-regex)
        (elyo-pydyn-while-regex regex 'elyo-python-ignore-add))))
  (when elyo-python-type-ignore-contain
    (save-excursion
      (dolist (search-for elyo-python-type-ignore-contain)
        (elyo-pydyn-while-search search-for 'elyo-python-ignore-add)))))


(defun elyo-python-convert-clean ()
  "Remove any python comment in current buffer."
  (elyo-python-delete-comment-lines)
  (elyo-python-ignore-to-errors))


;;;###autoload
(defun elyo-python-ignore-to-inputs ()
  "Add `elyo-python-type-ignore' to Dynamo Input (IN)."
  (interactive)
  (elyo-pydyn-is-python-export-or-error)
  (save-excursion
    (elyo-pydyn-while-regex elyo-dynamo-input-regex
                            'elyo-python-ignore-add))
  (if (and (called-interactively-p 'interactive)
           (buffer-modified-p))
      (save-buffer)))


;;;###autoload
(defun elyo-python-buffer-clean ()
  "Remove special python comments in current buffer."
  (interactive)
  (elyo-python-ignore-clean-buffer)
  (elyo-python-formatter-clean-buffer))


(defcustom elyo-python-inside-bracket-regex "(\\(.*?\\))"
  "Regex to find value inside bracket (group 1)."
  :type 'string
  :group 'elyo-pydyn)


(defun elyo-python-backslash-values (match-value)
  "Return biggest backslash string found in MATCH-VALUE."
  (let ((long-slashes "\\\\\\\\")
        (one-slash "\\"))
    (when (s-contains? one-slash match-value)
      (while (and one-slash (> (seq-length long-slashes) (seq-length one-slash)))
        (if (s-contains? long-slashes match-value)
            (setq one-slash nil)
          (setq long-slashes
                (string-limit long-slashes (- (seq-length long-slashes)
                                              (seq-length one-slash)))))))
    long-slashes))


(defun elyo-python--backslash-check ()
  "Replace `\\' with `\\\\' in region  of group 1."
  (let* ((value (match-string-no-properties 1))
         (slash (elyo-python-backslash-values value))
         (start-r? (s-starts-with? "r" value)))
    (if (s-contains? slash value)
        (replace-string-in-region slash (if start-r? "\\" "\\\\")
                                  (match-beginning 1) (pos-eol)))))


(defun elyo-python--backslash-contain ()
  "Return non-nil when last search contain backslashes."
  (s-contains? "\\" (match-string-no-properties 1)))


;;;###autoload
(defun elyo-python-backslash-ensure ()
  "Start `query-replace-regexp' process to replace brackets in if-statement."
  (interactive)
  (when elyo-python-inside-bracket-regex
    (save-excursion
      (elyo-pydyn-while-regex elyo-python-inside-bracket-regex
                              'elyo-python--backslash-check
                              'elyo-python--backslash-contain)
      (unhighlight-regexp elyo-python-inside-bracket-regex))))


(defcustom elyo-python-if-bracket-regex "if[ ]?(\\(.*\\)):"
  "Regex to search and replace brackets in if statements."
  :type 'string
  :group 'elyo-pydyn)


;;;###autoload
(defun elyo-python-if-remove-bracket ()
  "Start `query-replace-regexp' process to replace brackets in if-statement."
  (interactive)
  (when elyo-python-if-bracket-regex
    (save-excursion
      (goto-char (point-min))
      (query-replace-regexp elyo-python-if-bracket-regex "if \\1:"))))


;;;###autoload
(defun elyo-python-goto-dynamo-node ()
  "Goto to source file and try to select code at point in source."
  (interactive)
  (elyo-pydyn-is-python-export-or-error)
  (elyo-pydyn-goto-code (elyo-pydyn-convert-to-dynamo
                         (elyo-pydyn-current-line))))


(defun elyo-python--code-clean ()
  "Return code of current buffer with removed python comments."
  (let ((code (buffer-string)))
    (with-temp-buffer
      (insert code)
      (goto-char (point-min))
      (elyo-python-buffer-clean)
      (buffer-string))))


(defun elyo-python--to-dynamo-node ()
  "Replace python code of current buffer in Dynamo node."
  (elyo-pydyn-convert-python-to-dynamo (elyo-python--code-clean)))


;;;###autoload
(defun elyo-python-to-dynamo-node (file-path switch-or-kill)
  "Replace code from FILE-PATH in source. SWITCH-OR-KILL Dynamo buffer afterwarts."
  (interactive (list buffer-file-name
                     (elyo-pydyn-choose-switch-or-kill "Dynamo")))
  (elyo-pydyn-is-python-export-or-error file-path)
  (with-current-buffer (elyo-pydyn-buffer-by file-path)
    (elyo-pydyn-buffer-save (elyo-python--to-dynamo-node)
                            (elyo-pydyn-is-switch switch-or-kill)
                            (elyo-pydyn-is-switch-other switch-or-kill)
                            (elyo-pydyn-is-kill switch-or-kill))))


;;;###autoload
(defun elyo-python-to-dynamo-script (file-path switch-or-kill)
  "Replace code from FILE-PATH of all Dynamo nodes, SWITCH-OR-KILL buffer."
  (interactive (list (if (elyo-pydyn-is-python-export? buffer-file-name)
                         (buffer-file-name)
                       (elyo-selection-get (elyo-pydyn-python-files-in elyo-export-root t)
                                           "Select python file: " elyo-export-root))
                     (elyo-pydyn-choose-switch-or-kill "Dynamo")))
  (elyo-pydyn-is-python-export-or-error file-path)
  (unwind-protect
      (progn
        (elyo-pydyn-disable-lsp-clients)
        (let ((directory (file-name-directory file-path))
              (buffer-before (current-buffer))
              (switch (elyo-pydyn-is-switch switch-or-kill))
              (other-win (elyo-pydyn-is-switch-other switch-or-kill))
              (kill (elyo-pydyn-is-kill switch-or-kill))
              (dyn-path nil))
          (dolist (python-path (elyo-pydyn-python-files-in directory))
            (let ((buffer (elyo-pydyn-buffer-by python-path)))
              (with-current-buffer buffer
                (setq dyn-path (elyo-python--to-dynamo-node)))
              (unless (equal buffer buffer-before)
                (kill-buffer-if-not-modified buffer))))
          (elyo-pydyn-buffer-save dyn-path switch other-win kill)
          (message "Dynamo '%s' updated" (string-remove-prefix
                                          elyo-source-root dyn-path))))
    (elyo-pydyn-enable-lsp-clients)))


;;;###autoload
(defun elyo-python-to-dynamo-folder (directory switch-or-kill)
  "Replace code in Dynamo of python files in DIRECTORY, SWITCH-OR-KILL last buffer."
  (interactive (list (read-directory-name "Replace code in Dynamo source of all python files in? "
                                          elyo-export-root)
                     (elyo-pydyn-choose-switch-or-kill "Dynamo")))
  (elyo-pydyn-is-export-or-error directory)
  (unwind-protect
      (let ((dyn-path nil)
            (buffer-before (current-buffer))
            (switch (elyo-pydyn-is-switch switch-or-kill))
            (other-win (elyo-pydyn-is-switch-other switch-or-kill))
            (kill (elyo-pydyn-is-kill switch-or-kill)))
        (elyo-pydyn-disable-lsp-clients)
        (dolist (file-path (elyo-pydyn-python-files-in directory t))
          (let ((buffer (elyo-pydyn-buffer-by file-path)))
            (with-current-buffer buffer
              (let ((current-dyn (elyo-python--to-dyn-node)))
                (unless (or dyn-path (s-equals? current-dyn dyn-path))
                  (when (and dyn-path (not (s-equals? current-dyn dyn-path)))
                    (elyo-pydyn-buffer-save dyn-path nil t)
                    (message "Dynamo '%s' updated"
                             (string-remove-prefix
                              elyo-source-root dyn-path)))
                  (setq dyn-path current-dyn))
                (when (not (equal buffer buffer-before))
                  (kill-buffer-if-not-modified buffer))))))
        (elyo-pydyn-buffer-save dyn-path switch other-win kill))
    (elyo-pydyn-enable-lsp-clients)))


;;;###autoload
(defun elyo-python-node-geometry-set (switch-or-kill)
  "Set show geometry in dynamo node and SWITCH-OR-KILL buffer afterwards."
  (interactive (list (elyo-pydyn-choose-switch-or-kill "Dynamo")))
  (elyo-pydyn-is-python-export-or-error buffer-file-name)
  (let ((dyn-path node-path)
        (show (s-equals? "yes" (read-answer
                                "Show geometry? "
                                '(("yes"  ?y "Do show geometry")
                                  ("no"   ?n "Do not show geometry")
                                  ("quit" ?q "exit"))) ))
        (switch (elyo-pydyn-is-switch switch-or-kill))
        (other-win (elyo-pydyn-is-switch-other switch-or-kill))
        (kill (elyo-pydyn-is-kill switch-or-kill)))
    (elyo-pydyn-node-geometry-set show)
    (elyo-pydyn-buffer-save dyn-path switch other-win kill)))


;;;###autoload
(defun elyo-python-node-rename (switch-or-kill)
  "Rename Dynamo node in source file and SWITCH-OR-KILL buffer afterwards."
  (interactive (list (elyo-pydyn-choose-switch-or-kill "Dynamo")))
  (elyo-pydyn-is-python-export-or-error buffer-file-name)
  (let* ((dyn-path node-path)
         (current-name (elyo-pydyn-dynamo-node-name))
         (new-name (read-string "New Dynamo name: " current-name))
         (switch (elyo-pydyn-is-switch switch-or-kill))
         (other-win (elyo-pydyn-is-switch-other switch-or-kill))
         (kill (elyo-pydyn-is-kill switch-or-kill)))
    (unless (s-equals? new-name current-name)
      (save-excursion (elyo-pydyn-node-rename new-name)))
    (when (buffer-modified-p (elyo-pydyn-buffer-by dyn-path))
      (elyo-pydyn-buffer-save dyn-path switch other-win kill))))


(define-minor-mode elyo-python-mode
  "Toggles elyo-python-mode."
  :global nil
  :group 'elyo-pydyn
  :lighter " elyo-python"
  :keymap elyo-python-mode-map
  (cond
   ((and elyo-python-mode (elyo-pydyn-not-processing?))
    (elyo-python-indent-width-setup)
    (elyo-python-line-length-setup)
    (elyo-pydyn-buffer-breadcrump-on)
    (message "ELYO PYTHON on"))
   ((and elyo-python-mode (not (elyo-pydyn-not-processing?)))
    (setq elyo-python-mode nil)
    (message "CONVERT running"))
   (t
    (setq elyo-python-mode nil)
    (message "ELYO PYTHON off"))))


;;;###autoload
(defun elyo-pydyn-is-python-mode? ()
  "Return non-nil if current mode is `python-mode'."
  (or (equal major-mode 'python-mode)
      (derived-mode-p 'python-mode)))


;;;###autoload
(defun elyo-python-mode-activate ()
  "Function to activate `elyo-python-mode'."
  (if (and (elyo-pydyn-is-python-mode?)
           (or (elyo-pydyn-is-python-export?)
               (elyo-pydyn-is-python-source?)))
      (progn (elyo-python-indent-width-setup)
             (elyo-python-line-length-setup)
             (if (elyo-pydyn-not-processing?)
                 (elyo-python-mode-on)
               (elyo-python-mode-off)))
    (elyo-python-mode-off)))


;;;###autoload
(defun elyo-python-mode-on ()
  "Akticate `elyo-dynamo-mode'."
  (interactive)
  (elyo-python-mode 1))


;;;###autoload
(defun elyo-python-mode-off ()
  "Deaktivert `elyo-dynamo-mode'."
  (interactive)
  (elyo-python-mode -1))


(provide 'elyo-python)
;;; elyo-python.el ends here
