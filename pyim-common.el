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
(require 'cl-lib)
(require 'subr-x)

(defgroup pyim-common nil
  "pyim common."
  :group 'pyim)

(defvar pyim-debug nil)

(defvar pyim--local-variable-list nil
  "A list of buffer local variable.")

(defun pyim-register-local-variables (vars)
  "Recode variables VARS to `pyim--local-variable-list'."
  (dolist (var vars)
    (cl-pushnew var pyim--local-variable-list)
    (make-variable-buffer-local var)
    (put var 'permanent-local t))
  pyim--local-variable-list)

(defun pyim-kill-local-variables ()
  "Kill all local variables in `pyim--local-variable-list'."
  (mapc #'kill-local-variable pyim--local-variable-list))

(defun pyim-recreate-local-variables ()
  "Kill then make all variables in `pyim--local-variable-list'."
  (mapc #'kill-local-variable pyim--local-variable-list)
  (mapc #'make-local-variable pyim--local-variable-list))

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
这个函数由 \"二中\" 提供。"
  (let ((list-head (car list))
        (list-tail (cdr list)))
    (cond ((null list-tail)
           (cl-loop for element0 in list-head
                    append (cons (cons element0 nil) nil)))
          (t (cl-loop for element in list-head
                      append (mapcar (lambda (l) (cons element l))
                                     (pyim-permutate-list list-tail)))))))

(defun pyim-zip (lists &optional care-first-one)
  "Zip LISTS and delete dups: ((a b c) (d e)) => (a d b e c).
When CARE-FIRST-ONE is no-nil, ((a b c) (d e)) => (a d)."
  (when care-first-one
    (setq lists
          (mapcar (lambda (x)
                    (list (car x)))
                  lists)))
  (setq lists (remove nil lists))
  (if (< (length lists) 2)
      (car lists)
    (let* ((n (apply #'max (mapcar #'length lists)))
           (lists (mapcar
                   (lambda (x)
                     (append x (make-list (- n (length x)) nil)))
                   lists)))
      (delete-dups
       (pyim-flatten-tree
        (apply #'cl-mapcar
               #'list lists))))))

(defun pyim-flatten-tree (tree)
  "Take TREE and \"flatten\" it."
  (if (fboundp 'flatten-tree)
      (flatten-tree tree)
    (let (elems)
      (while (consp tree)
        (let ((elem (pop tree)))
          (while (consp elem)
            (push (cdr elem) tree)
            (setq elem (car elem)))
          (if elem (push elem elems))))
      (if tree (push tree elems))
      (nreverse elems))))

(defun pyim-subconcat (list &optional sep)
  "Concat sublist of LIST with SEP: (a b c d) => (abcd abc ab)."
  (let ((n (length list))
        output)
    (dotimes (i (- n 1))
      (let ((list (cl-subseq list 0 (- n i))))
        (push (string-join list (or sep "")) output)))
    (nreverse output)))

(defun pyim-split-list (list separator)
  "Split LIST into sublists bounded by equal SEPARATOR."
  (let (group result)
    (dolist (x (append list (list separator)))
      (if (not (equal x separator))
          (push x group)
        (push (nreverse group) result)
        (setq group nil)))
    (reverse result)))

(defun pyim-char-before-to-string (num)
  "得到光标前第 NUM 个字符，并将其转换为字符串。"
  (let* ((point (point))
         (point-before (- point num)))
    (when (and (> point-before 0)
               (char-before point-before))
      (char-to-string (char-before point-before)))))

(defun pyim-char-after-to-string (num)
  "得到光标后第 NUM 个字符，并将其转换为字符串。"
  (let* ((point (point))
         (point-after (+ point num)))
    (when (char-after point-after)
      (char-to-string (char-after point-after)))))

(defun pyim-char-before-to-number (num)
  (string-to-number
   (pyim-char-before-to-string num)))

(if (fboundp 'string-distance)
    (defalias 'pyim-string-distance 'string-distance)
  (defun pyim-string-distance (s1 s2)
    "Return the edit (levenshtein) distance between strings S1 S2."
    (let* ((l1 (length s1))
	       (l2 (length s2))
	       (dist (vconcat (mapcar (lambda (_) (make-vector (1+ l2) nil))
				                  (number-sequence 1 (1+ l1)))))
	       (in (lambda (i j) (aref (aref dist i) j))))
      (setf (aref (aref dist 0) 0) 0)
      (dolist (j (number-sequence 1 l2))
        (setf (aref (aref dist 0) j) j))
      (dolist (i (number-sequence 1 l1))
        (setf (aref (aref dist i) 0) i)
        (dolist (j (number-sequence 1 l2))
	      (setf (aref (aref dist i) j)
	            (min
	             (1+ (funcall in (1- i) j))
	             (1+ (funcall in i (1- j)))
	             (+ (if (equal (aref s1 (1- i)) (aref s2 (1- j))) 0 1)
		            (funcall in (1- i) (1- j)))))))
      (funcall in l1 l2))))

(defun pyim-proportion (nums)
  "计算 NUMS 所占比例。"
  (let ((sum (float (apply #'+ nums))))
    (mapcar
     (lambda (n)
       (/ n sum))
     nums)))

(defun pyim-numbers> (a b)
  "比较数字列表 A 和 B."
  (if (and (car a) (car b)
           (equal (car a) (car b)))
      (pyim-numbers> (cdr a) (cdr b))
    (> (or (car a) 0)
       (or (car b) 0))))

(defun pyim-add-unread-command-events (key &optional reset)
  "This function is a fork of `quail-add-unread-command-events'."
  (when reset
    (setq unread-command-events nil))
  (setq unread-command-events
        (if (characterp key)
            (cons (cons 'no-record key) unread-command-events)
          (append (cl-mapcan
                   (lambda (e)
                     (list (cons 'no-record e)))
                   (append key nil))
                  unread-command-events))))

;; Fork from `company-dabbrev--time-limit-while' in company-mode."
(defmacro pyim-time-limit-while (test limit &rest body)
  "If TEST non-nil and time consumption < LIMIT, repeat eval BODY."
  (declare (indent 2) (debug t))
  (let ((start (make-symbol "start")))
    `(let ((,start (current-time)))
       (catch 'done
         (while ,test
           ,@body
           (and ,limit
                (> (float-time (time-since ,start)) ,limit)
                (throw 'done 'pyim-time-out)))))))

(defvar exwm-xim-buffer-p)
(defun pyim-exwm-xim-environment-p ()
  "判断当前环境是否是 exwm-xim 环境。"
  (bound-and-true-p exwm-xim-buffer-p))

;; * Footer
(provide 'pyim-common)

;;; pyim-common.el ends here

