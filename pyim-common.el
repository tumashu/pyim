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

(defvar pyim-local-variable-list nil
  "A list of buffer local variable.")

(defun pyim-register-local-variables (vars)
  "Recode variables VARS to `pyim-local-variable-list'."
  (dolist (var vars)
    (cl-pushnew var pyim-local-variable-list)
    (make-variable-buffer-local var)
    (put var 'permanent-local t))
  pyim-local-variable-list)

(defun pyim-kill-local-variables ()
  "Kill all local variables in `pyim-local-variable-list'."
  (mapc #'kill-local-variable pyim-local-variable-list))

(defun pyim-recreate-local-variables ()
  "Kill then make all variables in `pyim-local-variable-list'."
  (mapc #'kill-local-variable pyim-local-variable-list)
  (mapc #'make-local-variable pyim-local-variable-list))

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
这个函数由 ‘二中’ 提供，`pyim-cstring-to-pinyin' 没有使用这个函数
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
这个函数由 ’翀/ty‘ 提供，`pyim-cstring-to-pinyin' 默认使用这个函数。"
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

(defun pyim-add-unread-command-events (key &optional reset)
  "Add KEY to `unread-command-events', ensuring that it is not recorded.

If KEY is a character, it is prepended to `unread-command-events'
as a cons cell of the form (no-record . KEY).

If KEY is a vector of events, the events in the vector are
prepended to `unread-command-events', after converting each event
to a cons cell of the form (no-record . EVENT).

Pyim puts keys back in `unread-command-events' to be handled
again, and when it does this these keys have already been
recorded in the recent keys and in the keyboard macro being
defined, which means that recording them again creates
duplicates.  When RESET is non-nil, the events in
`unread-command-events' are first discarded.

This function is a fork of `quail-add-unread-command-events'."
  (let ((ssh-run-p (or (getenv "SSH_CLIENT")
                       (getenv "SSH_TTY"))))
    (when reset
      (setq unread-command-events nil))
    (setq unread-command-events
          (if (characterp key)
              (cons
               (if ssh-run-p
                   ;; FIXME: When user use Xshell or MobaXTerm, error "<no-record>
                   ;; is undefined" will be exist, this may be not a pyim's bug.
                   ;; but I do not know how to solve this problem, so I do this ugly
                   ;; hack, and wait others help ...
                   ;; 1. https://github.com/tumashu/pyim/issues/402
                   ;; 2. https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=bd5c7404195e45f11946b4e0933a1f8b697d8b87x
                   key
                 (cons 'no-record key))
               unread-command-events)
            (append (mapcan (lambda (e)
                              (list (if ssh-run-p
                                        e
                                      (cons 'no-record e))))
                            (append key nil))
                    unread-command-events)))))

;; * Footer
(provide 'pyim-common)

;;; pyim-common.el ends here

