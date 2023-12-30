;;; elyo-python-format.el --- Dynamo BIM package -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides functionality to change Python code in a Dynamo file.
;;
;;; Code:
(require 'elyo-utils)

(require 's)
(require 'rect)
(require 'apheleia)
(require 'doom-modeline)
(require 'mode-local)
(require 'rect)
(require 's)


(defun elyo-python-command-regex(comment)
  "Return regex for COMMENT, like type: ignore."
  (let* ((without-comment (s-trim (string-replace "#" "" comment)))
         (name-and-value (s-split ":" without-comment)))
    (format "\[# \]*%s\[: \]*%s"
            (string-trim (seq-first name-and-value))
            (string-trim (seq-first (cdr name-and-value))))))


(defcustom elyo-python-type-ignore "# type: ignore"
  "Value for type ignore command."
  :type 'string
  :group 'elyo-dynpy)


(defun elyo-python-ignore? ()
  "Return non-nil if line contain type: ignore."
  (s-matches? (elyo-python-command-regex elyo-python-type-ignore)
              (elyo--current-line)))


(defun elyo--python-line-append-ignore()
  "Return current line with append type: ignore comment."
  (concat (s-trim-right (elyo--current-line)) "   " elyo-python-type-ignore))


(defun elyo-python-ignore-add()
  "Disable/ignore error from type checker."
  (interactive)
  (unless (elyo-python-ignore?)
    (replace-string-in-region (elyo--current-line)
                              (elyo--python-line-append-ignore)
                              (pos-bol) (pos-eol))))


(defun elyo--match-replaced(matched)
  "Return current line with replaced MATCHED value."
  (s-trim-right (s-replace matched "" (elyo--current-line))))


(defun elyo--python-ignore-clean()
  "Return line with replaced last search for type ignore comment."
  (let ((matched-value (elyo--buffer-substring (match-beginning 0) (match-end 0))))
    (replace-string-in-region (elyo--current-line)
                              (elyo--match-replaced matched-value)
                              (pos-bol) (pos-eol))))


;; (defun elyo-format-align-revit-doc ()
;;   "Align revit standart variables (doc, uiApp, app) to =."
;;   (let ((start (point-max))
;;         (end (point-min)))
;;     (dolist (search-for (list "DocumentManager.Instance.CurrentDBDocument"
;;                               "DocumentManager.Instance.CurrentUIApplication"
;;                               "DocumentManager.Instance.CurrentUIApplication.Application"
;;                               "uiApp.Application"))
;;       (let ((found-pos (progn (goto-char (point-min))
;;                               (search-forward search-for (point-max) t)
;;                               (goto-char (match-beginning 0))
;;                               (list (pos-bol)
;;                                     (pos-eol)))))
;;         (when found-pos
;;           (setq start (min start (or (seq-elt found-pos 0) (point-max))))
;;           (setq end (max end (or (seq-elt found-pos 1) (point-min)))))))
;;     (when ( and ( > start (point-min)) ( < end (point-max)) ( < start end))
;;       (evil-lion-left 0 start end ?=))))


;; (defun elyo-format-align-ignore ()
;;   "Align all Dynamo python inputs to = and #type: ignore."
;;   (let ((start nil)
;;         (end nil))
;;     (goto-char (point-min))
;;     (while (re-search-forward "[= \(]+IN\\[.*ignore"  (point-max) t)
;;       (goto-char (match-end 0))
;;       (if start (setq end (pos-eol)) (setq start (pos-bol))))
;;     (when ( and start end ( < start end))
;;       (evil-lion-left 0 start end ?=)
;;       (evil-lion-left 0 start end ?  # ))))


;; (defun elyo-format-align-code ()
;;   "Execute all code align functions."
;;   (elyo--convert-align-type-ignore)
;;   (elyo--convert-align-revit-doc)
;;   )


(defcustom elyo-python-formatter-max-length "--max-line-length"
  "Command option for max line length."
  :type 'string
  :group 'elyo-dynpy)


(defcustom elyo-python-ignore-option "--ignore="
  "Command option pass to formatter for execution."
  :type 'string
  :group 'elyo-dynpy)


(defcustom elyo-python-ignore-values (list "E2" "E4" "W1" "W5" "W6")
  "Command option pass to formatter for execution."
  :type 'list
  :group 'elyo-dynpy)

(defun elyo-python-formatter-options()
  "Return command for python formatter."
  (list elyo-python-ignore-option (s-join "," elyo-python-ignore-values)))


(defcustom elyo-python-formatter
  '(autopep8 . ("autopep8"
                (apheleia-formatters-fill-column
                 elyo-python-formatter-max-length)
                (elyo-python-formatter-options)
                "-"))
  "Formatter configuration for python buffers."
  :type 'list
  :group 'elyo-dynpy)


(defun elyo-python-formatter-config ()
  "Return `apheleia' formatter used in this minor mode."
  (add-to-list 'apheleia-formatters elyo-python-formatter))


(defcustom elyo-python-formatter-off "# autopep8: off"
  "Command to disable formatter until `elyo-python-formatter-on'."
  :type 'string
  :group 'elyo-dynpy)


(defcustom elyo-python-formatter-on "# autopep8: on"
  "Command to enable the formatter in upcoming lines."
  :type 'string
  :group 'elyo-dynpy)


(defun elyo-python-formatter-add-on (end-point)
  "Add formatter off comment for END-POINT and return point."
  (save-excursion
    (let ((end-point (progn (goto-char end-point)
                            (pos-bol))))
      ;; First insert the end value
      (goto-char end-point)
      (if (s-blank? (elyo--current-line))
          (progn (insert elyo-python-formatter-on)
                 (open-line 1))
        (ensure-empty-lines 1)
        (forward-line 1)
        (insert elyo-python-formatter-on)))
    (point))
  )

(defun elyo-python-formatter-add-off (start-point)
  "Add formatter on comment START-POINT and return point."
  (save-excursion
    (let ((start-point (progn (goto-char start-point)
                              (pos-bol))))
      ;; Otherwise start would change the end position
      (goto-char start-point)
      (if (s-blank? (elyo--current-line))
          (progn (insert elyo-python-formatter-off)
                 (ensure-empty-lines 1))
        (open-line 1)
        (insert elyo-python-formatter-off)))
    (point)))


;;;###autoload
(defun elyo-python-formatter-disable (start end)
  "Insert value at START and END comments to disable formatter."
  (interactive "r")
  ;; First insert the end value
  (let ((end-point (elyo-python-formatter-add-off end))
        ;; Otherwise start would change the end position
        (start-point (elyo-python-formatter-add-off start)))
    (rectangle-forward-char (- end-point end))
    (rectangle-backward-char (- start start-point))))


(defun elyo--python-formatter-next (comment)
  "Return next position of COMMENT."
  (save-excursion
    (re-search-forward (elyo-python-command-regex comment)
                       (point-max) nil 1)
    (match-beginning 0)))


(defun elyo--python-formatter-previous (comment)
  "Return previous position of COMMENT."
  (save-excursion
    (re-search-backward (elyo-python-command-regex comment)
                        (point-min) nil 1)
    (match-beginning 0)))


(defun elyo-python-formatter-on-pos ()
  "Return previous and next position of ON comment."
  (let ((comment elyo-python-formatter-on))
    (cons (elyo--python-formatter-previous comment)
          (elyo--python-formatter-next comment))))


(defun elyo-python-formatter-off-pos ()
  "Return previous and next position of OFF comment."
  (let ((comment elyo-python-formatter-off))
    (cons (elyo--python-formatter-previous comment)
          (elyo--python-formatter-next comment))))


(defun elyo-python-formatter-is-inside? ()
  "Return non-nil current point if formatter is inside."
  (< (car (elyo-python-formatter-off-pos))
     (cadr (elyo-python-formatter-on-pos))))


;;;###autoload
(defun elyo-python-formatter-enable ()
  "Remove formatter comment if point is disabled."
  (interactive)
  (when (elyo-python-formatter-is-inside?)
    (save-excursion
      (goto-char (car (elyo-python-formatter-off-pos)))
      (delete-line))
    (save-excursion
      (goto-char (cadr (elyo-python-formatter-on-pos)))
      (delete-line))))


;;;###autoload
(defun elyo-python-formatter-clean-buffer ()
  "Clean buffer from formatter comment's."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-line)
      (elyo-python-formatter-enable))))


(provide 'elyo-python-format)
;;; elyo-python-format.el ends here
