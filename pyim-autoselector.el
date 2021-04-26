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
(require 'pyim-entered)
(require 'pyim-candidates)

(defgroup pyim-autoselector nil
  "Autoselector for pyim."
  :group 'pyim)

(defcustom pyim-autoselector '(pyim-autoselector-xingma)
  "已经启用的自动上屏器.

自动上屏器是一个函数。假设用户已经输入 \"nihao\", 并按下 \"m\" 键，
那么当前entered 就是 \"nihaom\". 上次 entered 是 \"nihao\". 那么
返回值有3种情况（优先级按照下面的顺序）：

1. (:select last :replace-with \"xxx\") 自动上屏上次
entered (nihao) 的第一个候选词，m 键下一轮处理。

2. (:select current :replace-with \"xxx\") 自动上屏当前
entered (nihaom) 的第一个候选词。

3. nil  不自动上屏。

如果 :replace-with 设置为一个字符串，则选择最终会被这个字符串替代。

注意：多个 autoselector 函数运行时，最好不要相互影响，如果相互有
影响，需要用户自己管理。"
  :type '(choice (const nil)
                 (repeat function)))

(defun pyim-autoselector-xingma (&rest _args)
  "适用于型码输入法的自动上屏器.

比如：五笔等型码输入法，重码率很低，90%以上的情况都是选择第一个词
条，自动选择可以减少按空格强制选词的机会。"
  (let* ((scheme-name (pyim-scheme-name))
         (class (pyim-scheme-get-option scheme-name :class))
         (n (pyim-scheme-get-option scheme-name :code-split-length))
         (entered (pyim-entered-get 'point-before)))
    (when (eq class 'xingma)
      (cond
       ((and (= (length entered) n)
             (= (length pyim-candidates) 1)
             ;; 如果没有候选词，pyim 默认将用户输入当做候选词，这时不能自动上屏，
             ;; 因为这种情况往往是用户输入有误，自动上屏之后，调整输入就变得麻烦了。
             (not (equal entered (car pyim-candidates))))
        '(:select current))
       ((and (> (length entered) n)
             (equal (substring entered 0 n)
                    (car pyim-candidates-last)))
        ;; 自动清除错误输入模式，类似微软五笔：敲第五个字母的时候，前面四个字母自
        ;; 动清除。
        '(:select last :replace-with ""))
       ((> (length entered) n)
        '(:select last))
       (t nil)))))


;; * Footer
(provide 'pyim-autoselector)

;;; pyim-autoselector.el ends here
