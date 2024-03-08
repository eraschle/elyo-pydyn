;;; elyo-path.el --- DOOM Dynamo package -*- lexical-binding: t; -*-

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
;; create, handle and check path.
;;
;;; Code:

(require 'elyo-utils)

(defcustom elyo-python-extension "py"
  "Extension of a python code file."
  :type 'string
  :group 'elyo-pydyn)


(defcustom elyo-dynamo-script-ext "dyn"
  "Extension of dynamo script."
  :type 'list
  :group 'elyo-pydyn)


(defcustom elyo-dynamo-custom-ext "dyf"
  "Extension of dynamo custom node."
  :type 'list
  :group 'elyo-pydyn)


(defcustom elyo-source-root nil
  "Source root path of Dynamo library."
  :type 'string
  :group 'elyo-pydyn)


(defcustom elyo-export-root nil
  "Export root path of Python files."
  :type 'string
  :group 'elyo-pydyn)


(defcustom elyo-python-intern-path nil
  "Root path of Python files used in scripts."
  :type 'string
  :group 'elyo-pydyn)


(defun elyo-path-or-cub (&optional file-path)
  "Return FILE-PATH buffer if non-nil, otherwise path of current buffer."
  (or file-path buffer-file-name))


(defun elyo-is-export? (path)
  "Return non-nil if PATH is an `elyo-source-root'."
  (s-starts-with? elyo-export-root path))


(defun elyo-is-export-or-error (path)
  "Throw user error if PATH is not a subpath of `elyo-export-root'."
  (unless (elyo-is-export? path)
    (user-error "%s is NOT sub-directory of %s" path elyo-export-root)))


(defun elyo-is-source? (path)
  "Return non-nil if PATH is an `elyo-export-root'."
  (s-starts-with? elyo-source-root path))


(defun elyo-is-source-or-error (path)
  "Throw user error if PATH is not a subpath of `elyo-source-root'."
  (unless (elyo-is-source? path)
    (user-error "%s is NOT sub-directory of %s" path elyo-source-root)))


(defun elyo--path-is-ext? (file-path extensions)
  "Return non-nil if FILE-PATH extension is in EXTENSIONS."
  (when file-path
    (let ((file-ext (file-name-extension file-path)))
      (seq-some (lambda (ext) (progn (s-ends-with? ext file-ext)))
                (ensure-list extensions)))))


;;;###autoload
(defun elyo-dynamo-is-script? (&optional file-path)
  "Return non-nil when FILE-PATH is Dynamo SCRIPT."
  (elyo--path-is-ext? (elyo-path-or-cub file-path)
                      elyo-dynamo-script-ext))


;;;###autoload
(defun elyo-dynamo-is-custom? (&optional file-path)
  "Return non-nil when FILE-PATH is Dynamo CUSTOM NODE."
  (elyo--path-is-ext? (elyo-path-or-cub file-path)
                      elyo-dynamo-custom-ext))


(defun elyo-is-dynamo? (&optional file-path)
  "Return non-nil when FILE-PATH is Dynamo SCRIPT or CUSTOM NODE."
  (or (elyo-dynamo-is-script? file-path)
      (elyo-dynamo-is-custom? file-path)))


;;;###autoload
(defun elyo-is-dynamo-or-error (&optional file-path)
  "Throw user error if FILE-PATH is not a subpath of `elyo-source-root'."
  (let ((file-path (elyo-path-or-cub file-path)))
    (unless (elyo-is-dynamo? file-path)
      (user-error "%s is NOT a Dynamo file" (file-name-base file-path)))))


(defun elyo-is-dynamo-source? (&optional file-path)
  "Return non-nil when FILE-PATH is inside of `elyo-source-root'."
  (let ((file-path (elyo-path-or-cub file-path)))
    (and (elyo-is-source? file-path) (elyo-is-dynamo? file-path))))


(defun elyo-is-python? (&optional file-path)
  "Return non-nil when FILE-PATH or current buffer is PYTHON."
  (elyo--path-is-ext? (elyo-path-or-cub file-path)
                      elyo-python-extension))


(defun elyo-is-python-export-or-error (&optional file-path)
  "Throw user error if FILE-PATH or current buffer is not a python-file."
  (let ((file-path (elyo-path-or-cub file-path)))
    (elyo-is-export-or-error file-path)
    (unless (elyo-is-python-export? file-path)
      (user-error "%s is NOT a Python file" (file-name-base file-path)))))


;;;###autoload
(defun elyo-is-python-intern? (&optional file-path)
  "Return non-nil when FILE-PATH or current buffer is PYTHON internal package."
  (and (elyo-is-python? file-path)
       (not (s-blank? elyo-python-intern-path))
       (s-contains? elyo-python-intern-path
                    (elyo-path-or-cub file-path) t)))


;;;###autoload
(defun elyo-is-python-export? (&optional file-path)
  "Return non-nil when FILE-PATH is inside of `elyo-export-root'."
  (let ((file-path (elyo-path-or-cub file-path)))
    (and (elyo-is-export? file-path)
         (elyo-is-python? file-path))))


;;;###autoload
(defun elyo-is-python-source? (&optional file-path)
  "Return non-nil when FILE-PATH is inside of `elyo-export-root'."
  (let ((file-path (elyo-path-or-cub file-path)))
    (and (elyo-is-source? file-path)
         (elyo-is-python? file-path))))


(defun elyo--files-in-directory (directory extension &optional recursive)
  "Return files of EXTENSION in DIRECTORY, RECURSIVE search if non-nil."
  (let ((files (ensure-list (list))))
    (when (file-exists-p directory)
      (dolist (ext (ensure-list extension))
        (push (directory-files-recursively
               directory (format "\.%s" ext) recursive)
              files)))
    (seq-reverse (flatten-list files))))


(defun elyo--path-export-folder-for-source (path)
  "Return translated EXPORT directory of PATH."
  (concat elyo-export-root
          (string-replace elyo-source-root ""
                          (file-name-directory path))
          (file-name-base path) "/"))


(defun elyo--path-export-folder-for-export (path)
  "Return export path with added file-name as directory for PATH."
  (if (file-directory-p path) path (file-name-directory path)))


(defun elyo--path-export-folder-for (file-path)
  "Return export path for FILE-PATH."
  (if (elyo-is-export? file-path)
      (elyo--path-export-folder-for-export file-path)
    (elyo--path-export-folder-for-source file-path)))


(defun elyo-python-files-in (node-path &optional recursive)
  "Return python files in NODE-PATH, RECURSIVE search if non-nil."
  (elyo--files-in-directory (elyo--path-export-folder-for node-path)
                            elyo-python-extension
                            recursive))


(defun elyo-dynamo-files-in (directory &optional recursive)
  "Return Dynamo files in DIRECTORY, RECURSIVE search if non-nil."
  (elyo--files-in-directory directory
                            (list elyo-dynamo-custom-ext
                                  elyo-dynamo-script-ext)
                            recursive))


(defun elyo-path-export-folder (node-path)
  "Return export directory path for NODE-PATH. Create directory if not exist."
  (let ((export-dir (elyo--path-export-folder-for node-path)))
    (unless (file-exists-p export-dir)
      (make-directory export-dir t))
    export-dir))


(defvar elyo-path-clean-lookup (list " " "<" ">" "?" "|" "*" "/" "\\" "\"")
  "Not allowed characters in for directory or file path.")


(defvar elyo-path-name-separator "_"
  "Character to separate names and not allowed names.")


(defun elyo--path-py-abbrev-of (node-info python-maps)
  "Return abbrev from PYTHON-MAPS of python engine in NODE-INFO."
  (catch 'found-it
    (dolist (py-map python-maps)
      (when (equal (plist-get py-map :py-engine)
                   (plist-get node-info :engine))
        (throw 'found-it (plist-get py-map :py-short))))))


(defun elyo--path-clean-name (value)
  "Return cleaned VALUE with all `elyo-path-clean-lookup' replaced."
  (dolist (replace-value elyo-path-clean-lookup)
    (setq value (string-replace replace-value
                                elyo-path-name-separator
                                value)))
  ;; Because of multiple replacements is it
  ;; possible to have more then connected.
  (replace-regexp-in-string
   "[_]+" "_" (replace-regexp-in-string
               "[__]+" "_" value)))


(defun elyo-path-export-name (node-info python-maps)
  "Return export name created from NODE-INFO and PYTHON-MAPS."
  (s-join elyo-path-name-separator ;; join names together with _
          (list (elyo--path-clean-name (plist-get node-info :name))
                (elyo--path-py-abbrev-of node-info python-maps)
                (elyo--path-clean-name (plist-get node-info :node-id)))))


(defun elyo-path-export-file-name (node-info python-maps)
  "Return export file name created from NODE-INFO and PYTHON-MAPS."
  (let ((file-name (elyo-path-export-name node-info python-maps)))
    (s-downcase (concat file-name "." elyo-python-extension))))


(provide 'elyo-path)
;;; elyo-path.el ends here
