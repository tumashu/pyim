;;; pyim-cregexp.el --- Chinese regexp tools for pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-pymap)
(require 'xr)
(require 'rx)

(defgroup pyim-cregexp nil
  "Chinese regexp tools for pyim."
  :group 'pyim)

(defcustom pyim-cregexp-fallback-scheme 'quanpin
  "`pyim-cregexp-build' 使用的 Fallback scheme.

如果 `pyim-cregexp-build' 无法支持用户正在使用的 scheme 时，
将使用这个 scheme."
  :type 'symbol)

(defun pyim-cregexp-build (string &optional char-level-num)
  "根据 STRING 构建一个中文 regexp, 用于 \"拼音搜索汉字\".
比如：\"nihao\" -> \"[你呢...][好号...] \\| nihao\""
  ;; NOTE: (rx-to-string "") will return "\\(?:\\)",
  ;; While I want (pyim-cregexp-build "") return just "".
  (if (equal string "")
      string
    (let* ((char-level-num (or char-level-num 3))
           (rx-string
            (if (= char-level-num 0)
                string
              (ignore-errors
                (rx-to-string
                 (pyim-cregexp-build-from-rx
                  (lambda (x)
                    (if (stringp x)
                        (xr (pyim-cregexp-build-1 x char-level-num))
                      x))
                  (xr string)))))))
      (if (and rx-string (stringp rx-string))
          (if (pyim-cregexp-valid-p rx-string)
              rx-string
            (pyim-cregexp-build string (- char-level-num 1)))
        string))))

(defun pyim-cregexp-valid-p (cregexp)
  "Return t when cregexp is a valid regexp."
  (and cregexp
       (stringp cregexp)
       (condition-case nil
           (progn (string-match-p cregexp "") t)
         ;; FIXME: Emacs can't handle regexps whose length is too big :-(
         (error nil))))

(defun pyim-cregexp-build-from-rx (fn rx-form)
  (pcase rx-form
    ('nil nil)
    (`(,form) (funcall fn form))
    (`(any . ,_) rx-form)
    (`(,_ . ,_)
     (mapcar (lambda (x)
               (pyim-cregexp-build-from-rx fn x))
             rx-form))
    (_ (funcall fn rx-form))))

(defun pyim-cregexp-build-1 (str &optional char-level-num)
  (let* ((scheme-name (pyim-scheme-name))
         (class (pyim-scheme-get-option scheme-name :class))
         (code-prefix (pyim-scheme-get-option scheme-name :code-prefix))
         (sep "#####&&&&#####")
         (lst (remove "" (split-string
                          (replace-regexp-in-string
                           "\\([a-z]+'*\\)" (concat sep "\\1" sep) str)
                          sep)))
         (char-level-num
          (cond
           ((and char-level-num (> char-level-num 3)) 3)
           ((and char-level-num (< char-level-num 1)) 1)
           (t char-level-num))))
    ;; 确保 pyim 词库加载
    (pyim-dcache-init-variables)
    ;; pyim 暂时只支持全拼和双拼搜索
    (when (not (member class '(quanpin shuangpin xingma)))
      (setq scheme-name pyim-cregexp-fallback-scheme))
    (mapconcat
     (lambda (string)
       (if (or (pyim-string-match-p "[^a-z']+" string)
               (equal string ""))
           string
         (let* ((string1 (replace-regexp-in-string "'" "" string))
                (imobjs (pyim-imobjs-create string1 scheme-name))
                (regexp-list
                 (mapcar
                  (lambda (imobj)
                    (if (eq class 'xingma)
                        (pyim-cregexp-build:xingma imobj nil nil nil code-prefix)
                      (pyim-cregexp-build:quanpin imobj nil nil nil char-level-num)))
                  imobjs))
                (regexp
                 (when regexp-list
                   (mapconcat #'identity
                              (delq nil regexp-list)
                              "\\|")))
                (regexp
                 (if (> (length regexp) 0)
                     (if (equal string string1)
                         (concat string "\\|" regexp)
                       (concat string "\\|" string1 "\\|" regexp))
                   string)))
           (format "\\(?:%s\\)" regexp))))
     lst "")))

(defun pyim-cregexp-build:quanpin (imobj &optional match-beginning
                                         first-equal all-equal char-level-num)
  "从 IMOBJ 创建一个搜索中文的 regexp."
  (let* ((imobj
          (mapcar (lambda (x)
                    (concat (nth 0 x) (nth 1 x)))
                  imobj))
         (cchar-list
          (let ((n 0) results)
            (dolist (py imobj)
              (let* ((equal-match
                      (or all-equal
                          (and first-equal (= n 0))))
                     (cchars
                      ;; 只取常用字，不常用的汉字忽略，防止生成的
                      ;; regexp 太长而无法搜索
                      (mapconcat (lambda (x)
                                   (mapconcat #'identity
                                              (cl-subseq (split-string x "|") 0 char-level-num)
                                              ""))
                                 (pyim-pymap-py2cchar-get py equal-match nil t) "")))
                (push cchars results))
              (setq n (+ 1 n)))
            (nreverse results)))
         (regexp
          (mapconcat (lambda (x)
                       (when (pyim-string-match-p "\\cc" x)
                         (format "[%s]" x)))
                     cchar-list "")))
    (unless (equal regexp "")
      (concat (if match-beginning "^" "") regexp))))

(defun pyim-cregexp-build:xingma (imobj &optional match-beginning
                                        first-equal _all-equal code-prefix)
  "从 IMOBJ 创建一个搜索中文的 regexp."
  (cl-flet ((build-regexp
             (list)
             (let* ((n (apply #'max (mapcar #'length list)))
                    results)
               (dotimes (i n)
                 (push (format "[%s]%s"
                               (mapconcat
                                (lambda (x)
                                  (if (> i (- (length x) 1))
                                      ""
                                    (char-to-string
                                     (elt x i))))
                                list "")
                               (if (> i 0) "?" ""))
                       results))
               (mapconcat #'identity (reverse results) ""))))
    (let ((regexp (mapconcat
                   (lambda (x)
                     (let ((code (concat (or code-prefix "")
                                         (if first-equal
                                             (substring x 0 1)
                                           x))))
                       (build-regexp (pyim-dcache-get code))))
                   imobj "")))
      (unless (equal regexp "")
        (concat (if match-beginning "^" "") regexp)))))

(define-obsolete-function-alias 'pyim-convert-cregexp-at-point 'pyim-cregexp-convert-at-point "4.0")
(defun pyim-cregexp-convert-at-point (&optional insert-only)
  "将光标前的字符串按拼音的规则转换为一个搜索中文的 regexp.
用于实现拼音搜索中文的功能。

在 minibuffer 中，这个命令默认会自动运行 `exit-minibuffer'.
这个可以使用 INSERT-ONLY 参数控制。"
  (interactive "P")
  (unless (equal input-method-function 'pyim-input-method)
    (activate-input-method 'pyim))
  (let* ((buffer-string
          (buffer-substring (point-min) (point-max)))
         (string (if mark-active
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
    (cond
     ;; Deal with `org-search-view'
     ((and (window-minibuffer-p)
           (string-match-p
            (regexp-quote "[+-]Word/{Regexp}") buffer-string))
      (insert (format "{%s}" cregexp)))
     (t (insert cregexp)))
    (when (and (not insert-only)
               (window-minibuffer-p))
      (exit-minibuffer))))

(defun pyim-cregexp-isearch-search-fun ()
  "这个函数为 isearch 相关命令添加中文拼音搜索功能，
做为 `isearch-search-fun' 函数的 advice 使用。"
  (funcall
   (lambda ()
     `(lambda (string &optional bound noerror count)
        (funcall (if ,isearch-forward
                     're-search-forward
                   're-search-backward)
                 (pyim-cregexp-build string) bound noerror count)))))

;;;###autoload
(define-minor-mode pyim-isearch-mode
  "这个 mode 为 isearch 添加拼音搜索功能."
  :global t
  :require 'pyim
  :lighter " pyim-isearch"
  (if pyim-isearch-mode
      (progn
        (advice-add 'isearch-search-fun :override #'pyim-cregexp-isearch-search-fun)
        (message "PYIM: `pyim-isearch-mode' 已经激活，激活后，一些 isearch 扩展包有可能失效。"))
    (advice-remove 'isearch-search-fun #'pyim-cregexp-isearch-search-fun)))

(declare-function ivy--regex-plus "ivy")

(define-obsolete-function-alias 'pyim-ivy-cregexp 'pyim-cregexp-ivy "4.0")
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
(provide 'pyim-cregexp)

;;; pyim-cregexp.el ends here
