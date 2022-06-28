;;; pyim-entered.el --- page lib for pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-common)

(defgroup pyim-entered nil
  "Entered tools for pyim."
  :group 'pyim)

(defvar pyim-entered--buffer " *pyim-entered-buffer*"
  "一个 buffer，用来处理用户已经输入的字符串： entered。

用户 *已经* 输入的字符组成的字符串，在 pyim 里面，叫做 entered,
说白了就是 input, 选择 entered 而不选择 input 的原因是：

1. input 太常见了， 和其它词语组和起来容易产生歧义，比如：
   pyim-entered-output 就比 pyim-input-output 更加容易理解。
2. entered 这个词语很少见，只要明白它代表的概念，就不容易产生混乱。

pyim 使用一个 buffer 来处理 entered, 以实现 “用户输入字符串” 编
辑等高级功能：

1. 如果输入的字符串有错误，可以修改，不用取消重新输入；
2. 如果光标不在行首，pyim 只使用光标前的字符串来查找词条，
   如果词条上屏，词条对应的输入就从 buffer 中清除，然后
   继续处理后面的输入，这种方式方便长词的输入；
3. 如果光标在行首，则处理整行。")

(defmacro pyim-entered-with-entered-buffer (&rest forms)
  (declare (indent 0) (debug t))
  `(with-current-buffer (get-buffer-create pyim-entered--buffer)
     ,@forms))

(defun pyim-entered-get (&optional type)
  "从 `pyim-entered--buffer' 中获取拼音字符串.

默认返回 entered buffer 中的全部字符串。如果 TYPE 取值为
point-before, 返回 entered buffer 中 point 之前的字符串，如果
TYPE 取值为 point-after, 返回 entered buffer 中 point 之后的字符
串。"
  (pyim-entered-with-entered-buffer
    (cond
     ((bobp) (buffer-string))
     ((eq type 'point-before)
      (buffer-substring-no-properties (point-min) (point)))
     ((eq type 'point-after)
      (buffer-substring-no-properties (point) (point-max)))
     (t (buffer-string)))))

(defun pyim-entered-erase-buffer ()
  "清除 `pyim-entered--buffer' 的内容"
  (pyim-entered-with-entered-buffer
    (erase-buffer)))

(defun pyim-entered-in-the-middle-of-entered-p ()
  "判断 entered buffer 中，光标是否在 entered 字符串中间。"
  (pyim-entered-with-entered-buffer
    (and (> (point) 1)
         (< (point) (point-max)))))

;; * Footer
(provide 'pyim-entered)

;;; pyim-entered.el ends here
