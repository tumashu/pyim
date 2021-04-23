;;; pyim-common.el --- common utilities for pyim    -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2015-2021 Free Software Foundation, Inc.

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

;; * 说明文档                                                              :doc:
;; This file has common utilities used by pyim

;;; Code:
;; * 代码                                                                 :code:

(defgroup pyim-common nil
  "pyim common."
  :group 'pyim)

(defvar pyim-debug nil)

(defun pyim-string-match-p (regexp string &optional start)
  "与 `string-match-p' 类似，如果 REGEXP 和 STRING 是非字符串时，
不会报错。"
  (and (stringp regexp)
       (stringp string)
       (string-match-p regexp string start)))

(defun pyim-dline-parse (&optional seperaters)
  "解析词库文件当前行的信息，SEPERATERS 为词库使用的分隔符。"
  (let* ((begin (line-beginning-position))
         (end (line-end-position))
         (items (split-string
                 (buffer-substring-no-properties begin end)
                 seperaters)))
    items))

(defun pyim-permutate-list (list)
  "使用排列组合的方式重新排列 LIST.
这个函数由 ‘二中’ 提供，`pyim-hanzi2pinyin' 没有使用这个函数
(速度稍微有点慢)。"
  (let ((list-head (car list))
        (list-tail (cdr list)))
    (cond ((null list-tail)
           (cl-loop for element0 in list-head
                    append (cons (cons element0 nil) nil)))
          (t (cl-loop for element in list-head
                      append (mapcar (lambda (l) (cons element l))
                                     (pyim-permutate-list list-tail)))))))

(defun pyim-permutate-list2 (list)
  "使用排列组合的方式重新排列 LIST.
这个函数由 ’翀/ty‘ 提供，`pyim-hanzi2pinyin' 默认使用这个函数。"
  (if (= (length list) 1)
      (mapcar #'list (car list))
    (pyim-permutate-list2-internal (car list) (cdr list))))

(defun pyim-permutate-list2-internal (one two)
  "`pyim-permutate-list2' 的内部函数。"
  (let (return)
    (if (null (car two))
        one
      (dolist (x1 one)
        (dolist (x2 (car two))
          (push (if (listp x1)
                    (append x1 (list x2))
                  (list x1 x2))
                return)))
      (setq one return)
      (pyim-permutate-list2-internal one (cdr two)))))

(defun pyim-list-merge (a b)
  "Join list A and B to a new list, then delete dups."
  (let ((a (if (listp a)
               a
             (list a)))
        (b (if (listp b)
               b
             (list b))))
    (delete-dups `(,@a ,@b))))

(defun pyim-char-before-to-string (num)
  "得到光标前第 `num' 个字符，并将其转换为字符串。"
  (let* ((point (point))
         (point-before (- point num)))
    (when (and (> point-before 0)
               (char-before point-before))
      (char-to-string (char-before point-before)))))

(defun pyim-char-after-to-string (num)
  "得到光标后第 `num' 个字符，并将其转换为字符串。"
  (let* ((point (point))
         (point-after (+ point num)))
    (when (char-after point-after)
      (char-to-string (char-after point-after)))))

(defun pyim-exwm-enable-p ()
  "测试当前是否是 exwm 环境。

FIXME: This seem to be not a good approach, the
better way is let exwm provide a test function.
for example: https://github.com/ch11ng/exwm/pull/831"
  (string-match-p " \\*temp\\*" (buffer-name)))


;; * Footer
(provide 'pyim-common)

;;; pyim-common.el ends here

