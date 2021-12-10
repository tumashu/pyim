;;; pyim-candidates.el --- candidates lib for pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-codes)
(require 'pyim-pymap)

(defgroup pyim-candidates nil
  "Candidates of pyim."
  :group 'pyim)

(defcustom pyim-enable-shortcode t
  "启用输入联想词功能."
  :type 'boolean)

(defvar pyim-candidates nil
  "所有备选词条组成的列表.")

(defvar pyim-candidates-last nil
  "上一轮备选词条列表，这个变量主要用于 autoselector 机制.")

(defvar pyim-candidate-position nil
  "当前选择的词条在 ‘pyim-candidates’ 中的位置.

细节信息请参考 `pyim-page-refresh' 的 docstring.")

(pyim-register-local-variables
 '(pyim-candidates pyim-candidate-position))

;; ** 获取备选词列表
(defun pyim-candidates-create (imobjs scheme-name &optional async)
  "按照 SCHEME-NAME 对应的输入法方案， 从输入法内部对象列表:
IMOBJS 获得候选词条。"
  (when imobjs
    (let ((class (pyim-scheme-get-option scheme-name :class)))
      (when class
        (funcall (intern (format "pyim-candidates-create:%S" class))
                 imobjs scheme-name async)))))

(defun pyim-candidates-create:xingma (imobjs scheme-name &optional async)
  "`pyim-candidates-create' 处理五笔仓颉等形码输入法的函数."
  (unless async
    (let (result)
      (dolist (imobj imobjs)
        (let* ((codes (reverse (pyim-codes-create imobj scheme-name)))
               (output1 (car codes))
               (output2 (reverse (cdr codes)))
               output3 str)

          (when output2
            (setq str (mapconcat
                       (lambda (code)
                         (car (pyim-dcache-get code)))
                       output2 "")))
          (setq output3
                (remove "" (or (mapcar (lambda (x)
                                         (concat str x))
                                       (pyim-dcache-get output1 '(icode2word code2word shortcode2word)))
                               (list str))))
          (setq result (append result output3))))
      (when (car result)
        result))))

(defun pyim-candidates-create:quanpin (imobjs scheme-name &optional async)
  "`pyim-candidates-create' 处理全拼输入法的函数."
  (if async
      ;; 使用当前的 entered 构建一个搜索中文的正则表达式, 然后使用这个正则表达式
      ;; 在当前 buffer 中搜索词条。
      (let ((str (pyim-entered-get)))
        (if (< (length str) 1)
            pyim-candidates
          ;; NOTE: 让第一个词保持不变是不是合理，有待进一步的观察。
          `(,(car pyim-candidates)
            ,@(pyim-candidates-search-buffer
               (pyim-cregexp-build str 3 t))
            ,@(cdr pyim-candidates))))
    ;; 这段代码主要实现以下功能：假如用户输入 nihaomazheshi, 但词库里面找不到对
    ;; 应的词条，那么输入法自动用 nihaoma 和 zheshi 的第一个词条："你好吗" 和 "
    ;; 这是" 连接成一个新的字符串 "你好吗这是" 做为第一个候选词。
    (let* ((candidates (pyim-candidates-create-quanpin imobjs scheme-name))
           (n (length (car candidates)))
           output)
      (push (car candidates) output)
      (while (and (> n 0)
                  (car (setq imobjs
                             (mapcar (lambda (imobj)
                                       (nthcdr n imobj))
                                     imobjs))))
        (let ((candidates (pyim-candidates-create-quanpin imobjs scheme-name)))
          (push (car (pyim-candidates-create-quanpin imobjs scheme-name t)) output)
          (setq n (length (car candidates)))))
      (append (pyim-subconcat (nreverse output) "")
              candidates))))

(defun pyim-candidates-search-buffer (regexp)
  "在当前 buffer 中使用 REGEXP 搜索词条。"
  (save-excursion
    (let ((start (current-time))
          words)
      (goto-char (point-min))
      ;; Search after pos.
      (pyim-time-limit-while (and (not (input-pending-p))
                                  (re-search-forward regexp nil t))
          start 0.1 25
          (let ((match (match-string-no-properties 0)))
            ;; NOTE: 单个汉字我觉得不值得收集。
            (when (>= (length match) 2)
              (cl-pushnew match words :test #'equal))))
      words)))

(defun pyim-candidates-create-quanpin (imobjs scheme-name &optional fast-search)
  "`pyim-candidates-create:quanpin' 内部使用的函数。"
  (let (jianpin-words znabc-words personal-words common-words pinyin-chars-1 pinyin-chars-2)
    ;; 智能ABC模式，得到尽可能的拼音组合，查询这些组合，得到的词条做为联想词。
    (let ((codes (mapcar (lambda (x)
                           (pyim-subconcat x "-"))
                         (mapcar (lambda (imobj)
                                   (pyim-codes-create imobj scheme-name))
                                 imobjs))))
      (setq znabc-words
            (pyim-zip (mapcar #'pyim-dcache-get
                              (pyim-zip codes))
                      fast-search)))

    ;; 假如输入 "nih" ，那么搜索 code 为 "n-h" 的词条，然后筛选出所有拼音匹配
    ;; "ni-h" 或者 "ni[^-]*-h" 的词条。
    (when (and pyim-enable-shortcode
               (> (length (car imobjs)) 1))
      (dolist (imobj imobjs)
        (let* ((w (pyim-dcache-get
                   (mapconcat #'identity
                              (pyim-codes-create imobj scheme-name 1)
                              "-")
                   '(ishortcode2word)))
               (regexp1 (mapconcat #'identity
                                   (pyim-codes-create imobj scheme-name)
                                   "-"))
               (regexp2 (mapconcat #'identity
                                   (pyim-codes-create imobj scheme-name)
                                   "[^-]*-"))
               (w1 (cl-remove-if-not
                    (lambda (cstr)
                      (let ((py (pyim-cstring-to-pinyin cstr nil "-")))
                        (or (string-match-p regexp1 py)
                            (string-match-p regexp2 py))))
                    w))
               (w2 (cl-remove-if-not
                    (lambda (cstr)
                      (string-match-p regexp1 (pyim-cstring-to-pinyin cstr nil "-")))
                    w1)))
          (push (append w2 w1) jianpin-words))))

    ;; 获取个人词条，词库词条和第一汉字列表。
    (dolist (imobj imobjs)
      (let* (;; 个人词条
             (w1 (pyim-dcache-get
                  (mapconcat #'identity
                             (pyim-codes-create imobj scheme-name)
                             "-")
                  (if pyim-enable-shortcode
                      '(icode2word ishortcode2word)
                    '(icode2word))))
             ;; 词库词条
             (w2 (pyim-dcache-get
                  (mapconcat #'identity
                             (pyim-codes-create imobj scheme-name)
                             "-")
                  (if pyim-enable-shortcode
                      '(code2word shortcode2word)
                    '(code2word))))
             ;; 第一个汉字
             (w3 (pyim-dcache-get
                  (car (pyim-codes-create imobj scheme-name))))
             ;; 如果 w3 找不到第一个拼音对应的汉字，那就进一步使用
             ;; `pyim-pymap-py2cchar-get' 来查找，这个函数支持声母搜索。可以得到
             ;; 更多的词条。
             (w4 (unless w3
                   (mapcar #'char-to-string
                           (pyim-zip
                            (mapcar (lambda (x)
                                      ;; NOTE: 这里只取最常用的汉字，太多的汉字会带来后续处理压力，可能拖慢输入法。不过
                                      ;; 这个结论只是猜测。
                                      (car (split-string x "|")))
                                    (pyim-pymap-py2cchar-get
                                     (car (pyim-codes-create imobj scheme-name)))))))))
        (push w1 personal-words)
        (push w2 common-words)
        (push w3 pinyin-chars-1)
        (push w4 pinyin-chars-2)))

    (setq jianpin-words (pyim-zip (nreverse jianpin-words) fast-search))
    (setq personal-words (pyim-zip (nreverse personal-words) fast-search))
    (setq common-words (pyim-zip (nreverse common-words) fast-search))
    (setq pinyin-chars-1 (pyim-zip (nreverse pinyin-chars-1) fast-search))
    (setq pinyin-chars-2 (pyim-zip (nreverse pinyin-chars-2) fast-search))

    ;; 个人词条排序：使用词频信息对个人词库得到的候选词排序，第一个词条的位置
    ;; 比较特殊，不参与排序，具体原因请参考 `pyim-page-select-word' 中的
    ;; comment.
    (setq personal-words
          `(,(car personal-words)
            ,@(pyim-dcache-call-api
               'sort-words (cdr personal-words))))

    ;; 调试输出
    (when pyim-debug
      (print (list :imobjs imobjs
                   :personal-words personal-words
                   :common-words common-words
                   :jianpin-words jianpin-words
                   :znabc-words znabc-words
                   :pinyin-chars-1
                   (cl-subseq pinyin-chars-1
                              0 (min (length pinyin-chars-1) 5))
                   :pinyin-chars-2
                   (cl-subseq pinyin-chars-2
                              0 (min (length pinyin-chars-2) 5)))))

    (delete-dups
     (delq nil
           `(,@personal-words
             ,@jianpin-words
             ,@common-words
             ,@znabc-words
             ,@pinyin-chars-1
             ,@pinyin-chars-2
             )))))

(defun pyim-candidates-create:shuangpin (imobjs _scheme-name &optional async)
  "`pyim-candidates-create' 处理双拼输入法的函数."
  (pyim-candidates-create:quanpin imobjs 'quanpin async))

;; * Footer
(provide 'pyim-candidates)

;;; pyim-candidates.el ends here
