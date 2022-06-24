;;; pyim-autoselector.el --- autoselector for pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-process)

(defgroup pyim-autoselector nil
  "Autoselector for pyim."
  :group 'pyim)

(defun pyim-autoselector-xingma (&rest _args)
  "适用于型码输入法的自动上屏器.

比如：五笔等型码输入法，重码率很低，90%以上的情况都是选择第一个词
条，自动选择可以减少按空格强制选词的机会。"
  (let* ((scheme (pyim-scheme-current))
         (split-length (pyim-scheme-xingma-code-split-length scheme))
         (entered (pyim-process-get-entered 'point-before))
         (candidates (pyim-process-get-candidates))
         (last-candidates (pyim-process-get-last-candidates)))
    (when (pyim-scheme-xingma-p scheme)
      (pyim-autoselector--xingma
       split-length entered candidates last-candidates))))

(defun pyim-autoselector--xingma (split-length entered candidates last-candidates)
  "`pyim-autoselector-xingma' 内部使用的函数。"
  (cond
   ((and (= (length entered) split-length)
         (= (length candidates) 1)
         ;; 如果没有候选词，pyim 默认将用户输入当做候选词，这时不能自动上屏，
         ;; 因为这种情况往往是用户输入有误，自动上屏之后，调整输入就变得麻烦了。
         (not (equal entered (car candidates))))
    '(:select current))
   ((and (> (length entered) split-length)
         (equal (substring entered 0 split-length)
                (car last-candidates)))
    ;; 自动清除错误输入模式，类似微软五笔：敲第五个字母的时候，前面四个字母自
    ;; 动清除。
    '(:select last :replace-with ""))
   ((> (length entered) split-length)
    '(:select last))
   (t nil)))

(cl-pushnew #'pyim-autoselector-xingma pyim-process-autoselector)

;; * Footer
(provide 'pyim-autoselector)

;;; pyim-autoselector.el ends here
