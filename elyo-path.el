;;; elyo-path.el --- Dynamo BIM package -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides functionality to change Python code in a Dynamo file.
;;
;;; Code:

(require 'elyo-utils)

(defcustom elyo-python-extension "py"
  "Extension of a python code file."
  :type 'string
  :group 'elyo-dynpy)


(defcustom elyo-dynamo-script-ext "dyn"
  "Extension of dynamo script."
  :type 'list
  :group 'elyo-dynpy)


(defcustom elyo-dynamo-custom-ext "dyf"
  "Extension of dynamo custom node."
  :type 'list
  :group 'elyo-dynpy)


(defcustom elyo-source-root nil
  "Source root path of the Dynamo library."
  :type 'string
  :group 'elyo-dynpy)


(defcustom elyo-export-root nil
  "Export root path of the Python files."
  :type 'string
  :group 'elyo-dynpy)


;; (defcustom elyo-export-to-source nil
;;   "When t, automatically tangle Org files on save."
;;   :type 'boolean
;;   :group 'elyo-dynpy)


(defcustom elyo-python-intern-path nil
  "Export root path of the Python files."
  :type 'string
  :group 'elyo-dynpy)


(defun elyo-path-or-cub (&optional file-path)
  "Return if FILE-PATH is non-nil file-path, otherwise path of current buffer."
  (or file-path buffer-file-name))


(defun elyo-is-export? (path)
  "Return non-nil if PATH is an export path."
  (s-starts-with? elyo-export-root path))


(defun elyo-is-source? (path)
  "Return non-nil if PATH is an source path."
  (s-starts-with? elyo-source-root path))


(defun elyo--path-is-ext? (file-path extensions)
  "Return non-nil if FILE-PATH has one of the given EXTENSIONS."
  (when file-path
    (let ((file-ext (file-name-extension file-path)))
      (seq-some (lambda (ext) (progn (s-ends-with? ext file-ext)))
                (ensure-list extensions)))))


(defun elyo-is-dynamo-script? (&optional file-path)
  "Return non-nil if FILE-PATH is a DYNAMO script file."
  (and (elyo-is-source? file-path)
       (elyo--path-is-ext? (elyo-path-or-cub file-path)
                           elyo-dynamo-script-ext)))


(defun elyo-is-dynamo-custom? (&optional file-path)
  "Return non-nil if FILE-PATH is a DYNAMO Custom node."
  (and (elyo-is-source? file-path)
       (elyo--path-is-ext? (elyo-path-or-cub file-path)
                           elyo-dynamo-custom-ext)))


(defun elyo-is-dynamo? (&optional file-path)
  "Return non-nil if FILE-PATH is a DYNAMO Script/Custom node."
  (or (elyo-is-dynamo-script? file-path)
      (elyo-is-dynamo-custom? file-path)))


(defun elyo-is-python? (&optional file-path)
  "Return non-nil if FILE-PATH or current buffer is a PYTHON file."
  (elyo--path-is-ext? (elyo-path-or-cub file-path)
                      elyo-python-extension))


(defun elyo-is-python-intern? (&optional file-path)
  "Return non-nil if FILE-PATH or current buffer is PYTHON file internal package."
  (and (elyo-is-python? file-path)
       (s-contains? elyo-python-intern-path (elyo-path-or-cub file-path) t)))


(defun elyo-is-python-export? (&optional file-path)
  "Return non-nil FILE-PATH is a file inside of `elyo-export-root'."
  (let ((file-path (elyo-path-or-cub file-path)))
    (and (elyo-is-python? file-path) (elyo-is-export? file-path))))


(defun elyo--files-in-directory (directory extension &optional recursive)
  "Return all files of EXTENSION in DIRECTORY, RECURSIVE if non-nil."
  (let ((files (ensure-list (list))))
    (when (file-exists-p directory)
      (dolist (ext (ensure-list extension))
        (push (directory-files-recursively
               directory (format "\.%s" ext) recursive)
              files)))
    (seq-reverse (flatten-list files))))


(defun elyo--path-export-folder-for (file-path)
  "Return export directory path for FILE-PATH."
  (concat elyo-export-root
          (string-replace elyo-source-root ""
                          (file-name-directory file-path))
          (file-name-base file-path) "/"))


(defun elyo-python-files-in (node-path &optional recursive)
  "Return all python files in export dir of NODE-PATH, RECURSIVE if non-nil."
  (elyo--files-in-directory (elyo--path-export-folder-for node-path )
                            elyo-python-extension
                            recursive))


(defun elyo-dynamo-files-in (directory &optional recursive)
  "Return all Dynamo files in DIRECTORY, RECURSIVE if non-nil."
  (elyo--files-in-directory directory
                            (list elyo-dynamo-custom-ext
                                  elyo-dynamo-script-ext)
                            recursive))


(defun elyo-path-export-clean-for (node-path)
  "Delete all existing python files in the export directory of NODE-PATH."
  (let ((export-path (elyo--path-export-folder-for node-path)))
    (when (file-exists-p export-path)
      (dolist (file-path (elyo-python-files-in node-path))
        (delete-file file-path))
      ;; (delete-directory export-path nil nil)
      )))


(defun elyo-path-export-create-of (node-path)
  "Return export directory path for NODE-PATH."
  (let ((export-dir (elyo--path-export-folder-for node-path)))
    (unless (file-exists-p export-dir)
      (make-directory export-dir t))
    export-dir))



(defcustom elyo-convert-py2-engine  "IronPython2"
  "Extension of a python code file."
  :type 'string
  :group 'elyo-dynpy)

(defvar elyo-path-clean-lookup (list " " "<" ">" "?" "|" "*" "/" "\\" "\"")
  "Replace characters lookup, to replace with `elyo-path-name-separator'.")


(defvar elyo-path-name-separator "_"
  "Character to separate names and not allowed names.")


(defun elyo--path-py-abbrev-of (node-info convert-maps)
  "Return abbrev value of NODE-INFO if one engine in CONVERT-MAPS is equals."
  (catch 'found-it
    (dolist (convert convert-maps)
      (when (equal (plist-get convert :py-engine)
                   (plist-get node-info :engine))
        (throw 'found-it (plist-get convert :py-short))))))


(defun elyo--path-clean-name (value)
  "Return cleaned VALUE, where all `elyo-path-clean-lookup' are replaced."
  (dolist (replace-value elyo-path-clean-lookup)
    (setq value (string-replace replace-value
                                elyo-path-name-separator
                                value)))
  ;; Because of multiple replacements is it
  ;; possible to have more then connected.
  (replace-regexp-in-string "[_]+" "_" value))


(defun elyo-path-join-file-name (node-info convert-maps)
  "Return export python file name created with NODE-INFO and CONVERT-MAPS."
  (s-join elyo-path-name-separator ;; join names together with _
          (list (elyo--path-clean-name (plist-get node-info :name))
                (elyo--path-py-abbrev-of node-info convert-maps)
                (elyo--path-clean-name (plist-get node-info :node-id)))))


(defun elyo-path-export-file-name-for (node-info convert-maps)
  "Return export python file name created from NODE-INFO and CONVERT-MAPS."
  (let ((file-name (elyo-path-join-file-name node-info convert-maps)))
    (s-downcase (concat file-name "." elyo-python-extension))))


(provide 'elyo-path)
;;; elyo-path.el ends here
