;;; pyim-cregexp.el --- Chinese regexp core tools for pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-dcache)
(require 'pyim-imobjs)
(require 'pyim-pymap)
(require 'pyim-scheme)
(require 'rx)
(require 'xr)

(defgroup pyim-cregexp nil
  "Chinese regexp tools for pyim."
  :group 'pyim)

(defcustom pyim-cregexp-fallback-scheme 'quanpin
  "`pyim-cregexp-build' 使用的 Fallback scheme.

如果 `pyim-cregexp-build' 无法支持用户正在使用的 scheme 时，
将使用这个 scheme."
  :type 'pyim-scheme)

(defun pyim-cregexp-build (string &optional char-level-num chinese-only)
  "根据 STRING 构建一个中文 regexp.

这个函数的功能和 `pyim-cregexp-build' 类似，大多数参数也相同，不
同点是这个函数没有 scheme 参数，它会根据 `pyim-default-scheme' 和
`pyim-cregexp-fallback-scheme' 等信息动态的获取 scheme."
  (let ((scheme (pyim-cregexp--scheme)))
    (pyim-cregexp-create string scheme char-level-num chinese-only)))

(defun pyim-cregexp--scheme (&optional scheme)
  "返回一个支持 cregexp 的 scheme.

这个函数同时考虑 SCHEME, `pyim-default-scheme' 和
`pyim-cregexp-fallback-scheme'."
  (or (pyim-cregexp--find-scheme scheme)
      (pyim-cregexp--find-scheme pyim-default-scheme)
      (pyim-cregexp--find-scheme pyim-cregexp-fallback-scheme)
      (pyim-cregexp--find-scheme 'quanpin)))

(defun pyim-cregexp--find-scheme (scheme-or-name)
  "如果 SCHEME-OR-NAME 支持 cregexp 功能，就返回对应的 scheme."
  (let ((scheme (if (pyim-scheme-p scheme-or-name)
                    scheme-or-name
                  (pyim-scheme-get scheme-or-name))))
    (when (and (pyim-scheme-p scheme)
               (pyim-scheme-cregexp-support-p scheme))
      scheme)))

(defun pyim-cregexp-create (string scheme &optional char-level-num chinese-only)
  "根据 STRING 构建一个中文 regexp, 用于 \"拼音搜索汉字\".

比如：\"nihao\" -> \"[你呢...][好号...] \\| nihao\"


CHAR-LEVEL-NUM 代表汉字常用级别，pyim 中根据汉字的使用频率，将汉
字分为4个级别：1级最常用，4级别最不常用，1-3级汉字大概8000左右，
如果这个参数设置为3, 那么代表在构建 regexp 是，只使用常用级别小于
等于3的汉字。

如果 CHINESE-ONLY 为真，那么生成的 regexp 只能搜索汉字。

注意事项：如果生成的 regexp 太长，Emacs 无法处理，那么，这个命令
会抛弃一些不常用的汉字，重新生成，知道生成一个 Emacs 可以处理的
regexp, 所以搜索单字的时候一般可以搜到生僻字，但搜索句子的时候，
就无法搜索生僻字了。"
  (if (and string scheme
           (stringp string)
           (> (length string) 0)
           (pyim-scheme-p scheme)
           (pyim-scheme-cregexp-support-p scheme))
      (pyim-cregexp--create-valid-cregexp-from-string
       string scheme char-level-num chinese-only)
    string))

(defun pyim-cregexp--create-valid-cregexp-from-string
    (string scheme &optional char-level-num chinese-only)
  "从 STRING 创建一个有效的搜索中文的 regexp."
  (let ((char-level-num
         (pyim-cregexp--char-level-num char-level-num))
        rx-string)
    (while (and (not (pyim-cregexp--valid-p rx-string))
                (> char-level-num 0))
      (setq rx-string
            (pyim-cregexp--create-beautiful-cregexp-from-string
             string scheme char-level-num chinese-only))
      (setq char-level-num (1- char-level-num)))
    rx-string))

(defun pyim-cregexp--char-level-num (num)
  "根据 NUM 返回一个有效的常用汉字级别。"
  (if (numberp num)
      (max (min num 4) 1)
    4))

(defun pyim-cregexp--valid-p (cregexp)
  "Return t when cregexp is a valid regexp."
  (and cregexp
       (stringp cregexp)
       (not (pyim-cregexp--match-error-p cregexp))))

(defun pyim-cregexp--match-error-p (cregexp)
  "Return t when an match error is signaled.

Emacs can't handle regexps whose length is too big :-("
  (equal (condition-case nil
             (string-match-p cregexp "")
           (error 'error))
         'error))

(defun pyim-cregexp--create-beautiful-cregexp-from-string
    (string scheme &optional char-level-num chinese-only)
  "使用 rx 和 xr, 从 STRING 生成一个漂亮的搜索中文的 regexp.

这个 regexp 可能正常使用，也可能长度超出 emacs 的限制。"
  (or (ignore-errors
        (rx-to-string
         (pyim-cregexp--create-cregexp-from-rx
          (lambda (x)
            (if (stringp x)
                (xr (pyim-cregexp--create-cregexp-from-string
                     x scheme char-level-num chinese-only))
              x))
          (xr string))))
      string))

(defun pyim-cregexp--create-cregexp-from-rx (fn rx-form)
  (pcase rx-form
    ('nil nil)
    (`(,form) (funcall fn form))
    (`(any . ,_) rx-form)
    (`(,_ . ,_)
     (mapcar (lambda (x)
               (pyim-cregexp--create-cregexp-from-rx fn x))
             rx-form))
    (_ (funcall fn rx-form))))

(defun pyim-cregexp--create-cregexp-from-string
    (string scheme &optional char-level-num chinese-only)
  (let* ((char-level-num (pyim-cregexp--char-level-num char-level-num))
         (string-list (pyim-cregexp--split-string string)))
    ;; 确保 pyim 词库加载
    (pyim-dcache-init-variables)
    (pyim-cregexp--create-cregexp-from-string-list
     string-list scheme char-level-num chinese-only)))

(defun pyim-cregexp--split-string (string)
  (let ((sep "#####&&&&#####"))
    (remove "" (split-string
                (replace-regexp-in-string
                 "\\([a-z]+'*\\)" (concat sep "\\1" sep) string)
                sep))))

(defun pyim-cregexp--create-cregexp-from-string-list
    (string-list scheme &optional char-level-num chinese-only)
  (mapconcat
   (lambda (string)
     (if (or (pyim-string-match-p "[^a-z']+" string)
             (equal string ""))
         string
       (let* ((string1 (replace-regexp-in-string "'" "" string))
              (imobjs (pyim-imobjs-create string1 scheme))
              (regexp-list
               (mapcar (lambda (imobj)
                         (pyim-cregexp-create-from-imobj
                          imobj scheme nil nil nil char-level-num))
                       imobjs))
              (regexp
               (when regexp-list
                 (string-join (delq nil regexp-list) "\\|")))
              (regexp
               (if chinese-only
                   regexp
                 (if (> (length regexp) 0)
                     (if (equal string string1)
                         (concat string "\\|" regexp)
                       (concat string "\\|" string1 "\\|" regexp))
                   string))))
         (format "\\(?:%s\\)" regexp))))
   string-list ""))

(cl-defgeneric pyim-cregexp-create-from-imobj
    (imobj _scheme &optional match-beginning
           first-equal all-equal char-level-num)
  "从 IMOBJ 创建一个搜索中文的 regexp.")

(cl-defmethod pyim-cregexp-create-from-imobj
  (imobj (_scheme pyim-scheme-quanpin)
         &optional match-beginning first-equal all-equal char-level-num)
  "从 IMOBJ 创建一个搜索中文的 regexp, 适用于全拼输入法。"
  (let* ((num (pyim-cregexp--char-level-num char-level-num))
         (pinyin-list (pyim-cregexp--quanpin-get-pinyin-list imobj))
         (cchars-list
          (pyim-cregexp--quanpin-get-cchars-from-pinyin-list
           pinyin-list all-equal first-equal num))
         (regexp
          (mapconcat (lambda (x)
                       (when (pyim-string-match-p "\\cc" x)
                         (format "[%s]" x)))
                     cchars-list "")))
    (unless (equal regexp "")
      (concat (if match-beginning "^" "") regexp))))

(defun pyim-cregexp--quanpin-get-pinyin-list (imobj)
  "从 IMOBJ 生成类似 (\"ni\" \"hao\") 的拼音列表。"
  (mapcar (lambda (x)
            (concat (nth 0 x) (nth 1 x)))
          imobj))

(defun pyim-cregexp--quanpin-get-cchars-from-pinyin-list
    (pinyin-list all-equal first-equal char-level-num)
  "(\"ni\" \"hao\") => (\"你 ... 蔫 ... 鸟 ... 宁 ...\" \"好号毫\")"
  (let ((num (pyim-cregexp--char-level-num char-level-num))
        (n 0)
        results)
    (dolist (py pinyin-list)
      (let* ((equal-match
              (or all-equal
                  (and first-equal (= n 0))))
             (cchars (mapconcat
                      (lambda (x)
                        (mapconcat #'identity
                                   (let* ((list (split-string x "|"))
                                          (length (length list)))
                                     (cl-subseq list 0 (min num length)))
                                   ""))
                      (pyim-pymap-py2cchar-get py equal-match nil t) "")))
        (push cchars results))
      (setq n (+ 1 n)))
    (nreverse results)))

(cl-defmethod pyim-cregexp-create-from-imobj
  (imobj (scheme pyim-scheme-xingma)
         &optional match-beginning first-equal _all-equal _char-level-num)
  "从 IMOBJ 创建一个搜索中文的 regexp, 适用于形码输入法。"
  (let* ((code-prefix (pyim-scheme-code-prefix scheme))
         (regexp (mapconcat
                  (lambda (x)
                    (let ((code (concat (or code-prefix "")
                                        (if first-equal
                                            (substring x 0 1)
                                          x))))
                      (pyim-cregexp--build-xingma-regexp-from-words
                       (pyim-dcache-get code '(code2word)))))
                  imobj "")))
    (unless (equal regexp "")
      (concat (if match-beginning "^" "") regexp))))

(defun pyim-cregexp--build-xingma-regexp-from-words (words)
  "根据 WORDS, 创建一个可以搜索这些 WORDS 的 regexp.

比如：工, 恭恭敬敬 => [工恭][恭]?[敬]?[敬]?

通过 \"[工恭][恭]?[敬]?[敬]?\" 可以搜索 \"工\" 和 \"恭恭敬敬\"."
  (let ((n (apply #'max (mapcar #'length words)))
        results)
    (dotimes (i n)
      (push (format "[%s]%s"
                    (mapconcat
                     (lambda (x)
                       (if (> i (- (length x) 1))
                           ""
                         (char-to-string
                          (elt x i))))
                     words "")
                    (if (> i 0) "?" ""))
            results))
    (string-join (reverse results))))

;; * Footer
(provide 'pyim-cregexp)

;;; pyim-cregexp.el ends here
