;;; pyim-codes.el --- codes lib for pyim.        -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
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

;;; Code:
;; * 代码                                                           :code:
(require 'cl-lib)
(require 'pyim-scheme)
(require 'pyim-imobjs)
(require 'pyim-dcache)

(defun pyim-codes-create (imobj scheme-name &optional first-n)
  "按照 SCHEME-NAME 对应的输入法方案，从一个 IMOBJ 创建一个列表 codes, 这个列表
包含一个或者多个 code 字符串，这些 code 字符串用于从词库中搜索词条."
  (let ((class (pyim-scheme-get-option scheme-name :class)))
    (when class
      (funcall (intern (format "pyim-codes-create:%S" class))
               imobj scheme-name first-n))))

(defun pyim-codes-create:quanpin (imobj _scheme-name &optional first-n)
  "从IMOBJ 创建一个 code 列表：codes.

列表 codes 中包含一个或者多个 code 字符串，这些 code 字符串用于从
词库中搜索相关词条。

    (pyim-codes-create '((\"w\" \"o\" \"w\" \"o\") (\"\" \"ai\" \"\" \"ai\") (\"m\" \"ei\" \"m\" \"ei\") (\"n\"  \"v\" \"n\"  \"v\")) 'quanpin)

结果为:

   (\"wo\" \"ai\" \"mei\" \"nv\")"
  (mapcar
   (lambda (w)
     (let ((py (replace-regexp-in-string ;去掉分隔符，在词库中搜索候选词不需要分隔符
                "'" "" (concat (nth 0 w) (nth 1 w)))))
       (if (numberp first-n)
           (substring py 0 (min first-n (length py)))
         py)))
   imobj))

(defun pyim-codes-create:shuangpin (imobj _scheme-name &optional first-n)
  (pyim-codes-create:quanpin imobj 'quanpin first-n))

(defun pyim-codes-create:xingma (imobj scheme-name &optional first-n)
  (when scheme-name
    (let ((code-prefix (pyim-scheme-get-option scheme-name :code-prefix)))
      (mapcar
       (lambda (x)
         (concat (or code-prefix "")
                 (if (numberp first-n)
                     (substring x 0 (min first-n (length x)))
                   x)))
       imobj))))

;; * Footer
(provide 'pyim-codes)

;;; pyim-imobjs.el ends here
