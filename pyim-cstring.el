;;; pyim-cstring.el --- Chinese string core tools for pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-dcache)
(require 'pyim-scheme)
(require 'pyim-pymap)

(defgroup pyim-cstring nil
  "Chinese string tools for pyim."
  :group 'pyim)

(defun pyim-cstring--substrings (cstring &optional max-length number)
  "找出 CSTRING 中所有长度不超过 MAX-LENGTH 的子字符串，生成一个 alist。

这个 alist 中的每个元素为：(子字符串 开始位置 结束位置), 参数
NUMBER 用于递归，表示子字符串在 CSTRING 中的位置。"
  (let ((number (or number 0)))
    (cond
     ((= (length cstring) 0) nil)
     (t (append (pyim-cstring--substrings-1 cstring max-length number)
                (pyim-cstring--substrings (substring cstring 1)
                                          max-length (1+ number)))))))

(defun pyim-cstring--substrings-1 (cstring max-length number)
  "`pyim-cstring--substrings' 的内部函数。"
  (cond
   ((< (length cstring) 2) nil)
   (t (append
       (let ((length (length cstring)))
         (when (<= length (or max-length 6))
           (list (list cstring number (+ number length)))))
       (pyim-cstring--substrings-1
        (substring cstring 0 -1)
        max-length number)))))

;; ** 获取光标处中文字符串
(defun pyim-cstring-at-point (&optional number)
  "获取光标一个中文字符串，字符数量为：NUMBER."
  (save-excursion
    (let* ((point (point))
           (begin (- point number))
           (begin (if (> begin 0)
                      begin
                    (point-min)))
           (string (buffer-substring-no-properties
                    point begin)))
      (when (and (stringp string)
                 (= (length string) number)
                 (not (pyim-string-match-p "\\CC" string)))
        string))))

;; ** 中文字符串到拼音的转换工具
;;;###autoload
(defun pyim-cstring-to-pinyin (string &optional shou-zi-mu separator
                                      return-list ignore-duo-yin-zi _)
  "将汉字字符串转换为对应的拼音字符串的工具.

如果 SHOU-ZI-MU 设置为 t, 转换仅得到拼音首字母字符串。当
RETURN-LIST 设置为 t 时，返回一个拼音列表，这个列表包含词条的一个
或者多个拼音（词条包含多音字时）；如果 IGNORE-DUO-YIN-ZI 设置为
t, 遇到多音字时，只使用第一个拼音，其它拼音忽略。

BUG: 当 STRING 中包含其它标点符号，并且设置 SEPERATER 时，结果会
包含多余的连接符：比如： \"你=好\" --> \"ni-=-hao\""
  (if (not (pyim-string-match-p "\\cc" string))
      (if return-list (list string) string)
    (let* ((pinyins-list
            (or (pyim-cstring-to-pinyin--from-dcache string)
                (pyim-pymap-str2py-get string)))
           (list (mapcar (lambda (x)
                           (mapconcat (lambda (str)
                                        (if shou-zi-mu
                                            (substring str 0 1)
                                          str))
                                      x separator))
                         (if ignore-duo-yin-zi
                             (list (car pinyins-list))
                           pinyins-list))))
      ;; 返回拼音字符串或者拼音列表
      (if return-list
          list
        (string-join list " ")))))

(defun pyim-cstring-to-pinyin--from-dcache (cstring)
  "从 Dcache 中搜索 CSTRING 对应的拼音。"
  (let* ((pinyins-list
          (mapcar #'pyim-cstring--get-pinyin-code
                  (pyim-pymap-split-string cstring))))
    (unless (member nil pinyins-list)
      (list (apply #'append pinyins-list)))))

(defun pyim-cstring--get-pinyin-code (str)
  "从 Dcache 中获取中文字符串 STR 对应的拼音。

如果 STR 不包含中文，不做特殊处理。"
  (if (pyim-string-match-p "\\cc" str)
      (when-let ((code (cl-find-if-not
                        (lambda (c)
                          ;; 注意：Pinyin 词库中不包含 "/" 字符。
                          (string-match-p c "/"))
                        (pyim-dcache-get str '(word2code)))))
        (split-string code "-"))
    (list str)))

;;;###autoload
(defun pyim-cstring-to-pinyin-simple (string &optional shou-zi-mu separator return-list)
  "简化版的 `pyim-cstring-to-pinyin', 不处理多音字。"
  (pyim-cstring-to-pinyin string shou-zi-mu separator return-list t))

;; ** 中文字符串到形码的转换工具
(cl-defgeneric pyim-cstring-to-xingma (string scheme &optional return-list)
  "将中文 STRING 转换为 SCHEME 方案对应的形码。")

(cl-defmethod pyim-cstring-to-xingma (string (scheme pyim-scheme-xingma)
                                             &optional return-list)
  "将中文 STRING 转换为 SCHEME 方案对应的形码。

返回的形码不包括 code-prefix。当 RETURN-LIST 设置为 t 时，返回一
个形码 list。"
  (when (string-match-p "^\\cc+\\'" string)
    (let* ((prefix (pyim-scheme-code-prefix scheme))
           (dcache-codes
            (mapcar (lambda (x)
                      (when (string-prefix-p prefix x)
                        (string-remove-prefix prefix x)))
                    (sort (cl-copy-list (pyim-dcache-get string '(word2code)))
                          (lambda (a b)
                            (> (length a) (length b))))))
           (codes (remove nil dcache-codes)))
      (when codes
        (if return-list
            codes
          ;; 如果要返回一个字符串，那就返回第一个，也就是最长的那个编码。
          (car codes))))))

(cl-defmethod pyim-cstring-to-xingma (string (scheme pyim-scheme-wubi)
                                             &optional return-list)
  "将中文 STRING 转换为五笔编码。

得到的五笔编码包括 code-prefix。当 RETURN-LIST 设置为 t 时，返回
一个五笔编码 list。"
  (or (cl-call-next-method)
      (let ((length (length string))
            (string (split-string string "" t))
            code)
        (cond
         ;; 双字词，分别取两个字的前两个编码
         ((eq length 2)
          (let ((s1 (pyim-cstring-to-xingma (nth 0 string) scheme))
                (s2 (pyim-cstring-to-xingma (nth 1 string) scheme)))
            (when (and s1 s2)
              (setq code (concat (substring s1 0 2)
                                 (substring s2 0 2))))))
         ;; 三字词，取前二字的首编码，及第三个字的前两个编码
         ((eq length 3)
          (let ((s1 (pyim-cstring-to-xingma (nth 0 string) scheme))
                (s2 (pyim-cstring-to-xingma (nth 1 string) scheme))
                (s3 (pyim-cstring-to-xingma (nth 2 string) scheme)))
            (when (and s1 s2 s3)
              (setq code (concat (substring s1 0 1)
                                 (substring s2 0 1)
                                 (substring s3 0 2))))))
         ;; 四字词及以上，分别前三个字及最后一个字的首编码
         ((> length 3)
          (let ((s1 (pyim-cstring-to-xingma (nth 0 string) scheme))
                (s2 (pyim-cstring-to-xingma (nth 1 string) scheme))
                (s3 (pyim-cstring-to-xingma (nth 2 string) scheme))
                (s4 (pyim-cstring-to-xingma (nth (1- length) string) scheme)))
            (when (and s1 s2 s3 s4)
              (setq code (concat (substring s1 0 1)
                                 (substring s2 0 1)
                                 (substring s3 0 1)
                                 (substring s4 0 1))))))
         (t nil))
        (when code
          (if return-list
              (list code)
            code)))))

(cl-defgeneric pyim-cstring-to-codes (string scheme &optional criteria)
  "将 STRING 转换为 SCHEME 对应的 codes.")

(cl-defmethod pyim-cstring-to-codes (string (scheme pyim-scheme-xingma) &optional _)
  "将中文字符串 STRING 转换为对应的形码."
  (pyim-cstring-to-xingma string scheme t))

(cl-defmethod pyim-cstring-to-codes (string (_scheme pyim-scheme-quanpin) &optional criteria)
  "将中文字符串 STRING 转换为对应的拼音。

如果用户提供 CRITERIA 字符串，那么检索到的所有 codes 会和这个字符
串进行字符串相似度比较，然后选择一个相似度最高的 code 作为输出，
这种处理方式适合拼音输入法，形码输入法一般不需要类似的操作。

CRITERIA 字符串一般是通过 imobjs 构建的，它保留了用户原始的输入信
息。"
  (let ((codes (pyim-cstring-to-pinyin string nil "-" t nil t))
        codes-sorted)
    (if (< (length criteria) 1)
        codes
      ;; 将 所有 codes 与 criteria 字符串比对，选取相似度最高的一个
      ;; code. 这种处理方式适合拼音输入法。
      (setq codes-sorted
            (sort codes
                  (lambda (a b)
                    (< (pyim-string-distance a criteria)
                       (pyim-string-distance b criteria)))))
      (list (car codes-sorted)))))

;; PYIM 重构以前使用的一些函数名称，alias 一下，便于兼容。
(defalias 'pyim-hanzi2pinyin-simple #'pyim-cstring-to-pinyin-simple)
(defalias 'pyim-hanzi2pinyin #'pyim-cstring-to-pinyin)
(defalias 'pyim-hanzi2xingma #'pyim-cstring-to-xingma)

;; * Footer
(provide 'pyim-cstring)

;;; pyim-cstring.el ends here
