;;; pyim-tests.el ---  unit tests for pyim -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2019-2021 Free Software Foundation, Inc.

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; Maintainer: Chen Bin <chenbin.sh@gmail.com>
;;             Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/pyim
;; Keywords: convenience, Chinese, pinyin, input-method

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;  pyim test case.

;;; Code:
;; * 代码                                                                 :code:
(require 'ert)
(require 'pyim)
(require 'pyim-dregcache)

(defun pyim-test-get-dicts ()
  "当前目录下的词库."
  (let* ((files (directory-files-recursively default-directory "\.pyim$")))
    (mapcar (lambda (f)
              (list :name (file-name-base f) :file f))
            files)))

(setq default-input-method "pyim")
(setq pyim-dicts (pyim-test-get-dicts))

(ert-deftest pyim-test-dregcache-backend ()
  (let* ((pyim-dcache-backend 'pyim-dregcache)
         words)
    (should (eq (length pyim-dregcache-cache) 0))
    ;; load dictionary
    (pyim-dcache-update-code2word t)
    ;; cache is filled
    (should (> (length pyim-dregcache-cache) 0))

    ;; test dregcache api
    (setq words (pyim-dcache-get "zun-bei"))
    (should (eq (length words) 1))
    (should (string= (nth 0 words) "尊卑"))
    (setq words (pyim-dcache-get "zun"))
    (should (string= (nth 0 words) "尊"))
    (should (eq (length words) 43))))

(ert-run-tests-batch-and-exit)
;; * Footer
;;; pyim-tests.el ends here
