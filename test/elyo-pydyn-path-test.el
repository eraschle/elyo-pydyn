;;; elyo-pydyn-path-test.el --- DOOM Dynamo package -*- lexical-binding: t; -*-

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
;; test for elyo-pydyn-path`
;;
;;; Code:

(require 'ert)
(require 'elyo-pydyn-path)


(ert-deftest elyo-pydyn-is-python-2-test ()
  (should-not (elyo-pydyn-is-python-2? nil))
  (let ((elyo-pydyn-python-2-engine "py2"))
    (should (elyo-pydyn-is-python-2? "py2"))))

(ert-deftest elyo-pydyn-is-python-3-test ()
  (should-not (elyo-pydyn-is-python-3? nil))
  (let ((elyo-pydyn-python-3-engine "py3"))
    (should (elyo-pydyn-is-python-3? "py3"))))
