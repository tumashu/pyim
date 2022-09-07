;;; pyim-cregexp-utils.el --- Chinese regexp tools for pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-cregexp)
(require 'pyim-dhashcache)

(defgroup pyim-cregexp nil
  "Chinese regexp tools for pyim."
  :group 'pyim)

(defcustom pyim-cregexp-convert-at-point-function
  #'pyim-cregexp-convert-at-point-function
  "`pyim-cregexp-convert-at-point' 使用的函数。

此函数有一个参数 cregexp, 表示生成的 cregexp. 其返回值会插入当前
buffer."
  :type 'function)

;;;###autoload
(defun pyim-cregexp-convert-at-point (&optional insert-only)
  "将光标前的字符串按拼音的规则转换为一个搜索中文的 regexp.
用于实现拼音搜索中文的功能。

在 minibuffer 中，这个命令默认会自动运行 `exit-minibuffer'.
这个可以使用 INSERT-ONLY 参数控制。"
  (interactive "P")
  (pyim-pymap-cache-create)
  (let* ((string (if mark-active
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (buffer-substring
                    (point)
                    (save-excursion
                      (skip-syntax-backward "w")
                      (point)))))
         (length (length string))
         (cregexp (pyim-cregexp-build string)))
    (delete-char (- 0 length))
    (insert (funcall pyim-cregexp-convert-at-point-function cregexp))
    (when (and (not insert-only)
               (window-minibuffer-p))
      (exit-minibuffer))))

(defun pyim-cregexp-convert-at-point-function (cregexp)
  "这个函数是变量 `pyim-cregexp-convert-at-point-function' 的默认取值。"
  (cond
   ;; Deal with `org-search-view'
   ((and (window-minibuffer-p)
         (string-match-p
          (regexp-quote "[+-]Word/{Regexp}")
          (buffer-substring (point-min) (point-max))))
    (format "{%s}" cregexp))
   (t cregexp)))

;; 让 isearch 支持用 code 搜索中文功能
(declare-function isearch-search-fun "isearch")
(defvar isearch-forward)

;;;###autoload
(define-minor-mode pyim-isearch-mode
  "这个 mode 为 isearch 添加拼音搜索功能."
  :global t
  :require 'pyim
  :lighter " pyim-isearch"
  (if pyim-isearch-mode
      (progn
        (advice-add 'isearch-search-fun :override #'pyim-isearch--search-fun)
        (message "PYIM: `pyim-isearch-mode' 已经激活，激活后，一些 isearch 扩展包有可能失效。"))
    (advice-remove 'isearch-search-fun #'pyim-isearch--search-fun)))

(defun pyim-isearch--search-fun ()
  "这个函数为 isearch 相关命令添加中文拼音搜索功能，
做为 `isearch-search-fun' 函数的 advice 使用。"
  (funcall
   (lambda ()
     `(lambda (string &optional bound noerror count)
        (funcall (if ,isearch-forward
                     're-search-forward
                   're-search-backward)
                 ;; FIXME: 搜索字符串中的 '\' 不太好处理，如果遇到, 目前的做法是
                 ;; 不做任何处理，这个是需要改进的地方。
                 (if (string-match-p "\\\\" string)
                     (regexp-quote string)
                   (pyim-cregexp-build string))
                 bound noerror count)))))

;; 让 ivy 支持 code 搜索。
(declare-function ivy--regex-plus "ivy")

(defun pyim-cregexp-ivy (str)
  "Let ivy support search Chinese with pinyin feature."
  (let ((x (ivy--regex-plus str))
        (case-fold-search nil))
    (if (listp x)
        (mapcar (lambda (y)
                  (if (cdr y)
                      (list (if (equal (car y) "")
                                ""
                              (pyim-cregexp-build (car y)))
                            (cdr y))
                    (list (pyim-cregexp-build (car y)))))
                x)
      (pyim-cregexp-build x))))

;; * Footer
(provide 'pyim-cregexp-utils)

;;; pyim-cregexp-utils.el ends here
