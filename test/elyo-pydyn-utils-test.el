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
;; test for `elyo-pydyn-utils`
;;
;;; Code:

(require 'ert)
(require 'elyo-pydyn-utils)

(ert-deftest elyo-pydyn-buffer-preview-name-test ()
  (should
   (string= "** Preview FILENAME **"
            (elyo-pydyn-buffer-preview-name "FILENAME")))
  (should
   (let ((elyo-pydyn-buffer-preview-prefix "VORSCHAU"))
     (string= "** VORSCHAU FILENAME **"
              (elyo-pydyn-buffer-preview-name "FILENAME"))))
  )

(ert-deftest elyo-pydyn-buffer-preview-get-test ())
(let ((preview (elyo-pydyn-buffer-preview-get "/home/user/path/to/file")))
  (should (bufferp preview))
  (should (string= (buffer-name preview)
                   "** Preview /home/user/path/to/file **")))

(ert-deftest elyo-pydyn-buffer-preview-name?-test ()
  ;; Current buffer is not a preview buffer
  (should-not (elyo-pydyn-buffer-preview-name?))
  (with-current-buffer (elyo-pydyn-buffer-preview-get "preview file")
    ;; Buffer from `elyo-pydyn-buffer-preview-get' must be a preview buffer
    (should (elyo-pydyn-buffer-preview-name?))))

(ert-deftest elyo-pydyn-save-buffer-test ()
  ;; TODO: How to test `elyo-pydyn-save-buffer'
  )

(ert-deftest elyo-pydyn-buffer-substring-test ()
  ;; TODO: How to test `elyo-pydyn-buffer-substring' useful
  )

(ert-deftest elyo-pydyn-while-search-test ()
  ;; TODO: How to test `elyo-pydyn-while-search' useful
  )

(ert-deftest elyo-pydyn-while-regex-test ()
  ;; TODO: How to test `elyo-pydyn-while-regex' useful
  )

(ert-deftest elyo-pydyn--name-get-test ()
  (let* ((dir-path "/some/path/of/directory/")
         (file-name "some-file-name.el")
         (file-path (concat dir-path file-name)))
    (should (equal (elyo-pydyn--name-get file-path)
                   (elyo-pydyn--name-get file-path
                                         dir-path)))))

(ert-deftest elyo-pydyn-not-processing-test ()
  ;; non-nil if process not is running
  (should (elyo-pydyn-not-processing?))
  ;; nil if process not is running
  (let ((elyo-pydyn-processing t))
    (should-not (elyo-pydyn-not-processing?))))
