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
(defun pyim-candidates-sort (candidates)
  "对 CANDIDATES 进行排序。"
  (pyim-dcache-call-api 'sort-words candidates))

(defun pyim-candidates-create (imobjs scheme-name &optional async)
  "按照 SCHEME-NAME 对应的输入法方案， 从输入法内部对象列表:
IMOBJS 获得候选词条。"
  (when imobjs
    (let ((class (pyim-scheme-get-option scheme-name :class)))
      (when class
        (funcall (intern (format "pyim-candidates-create:%S" class))
                 imobjs scheme-name async)))))

(defun pyim-candidates-get-chief (scheme-name &optional personal-words common-words)
  "选取第一位候选词。"
  (let ((class (pyim-scheme-get-option scheme-name :class)))
    (cond ((equal class 'xingma)
           (or
            ;; 如果从公共词库里面获取到的第一个词条是汉字，就选择它。
            (when (= (length (car common-words)) 1)
              (car common-words))
            ;; 从个人词库里面按排列的先后顺序，获取一个汉字。
            (cl-find-if
             (lambda (word)
               (= (length word) 1))
             personal-words)))
          (t (or
              ;; 最近输入的10个不同的词中出现一次以上。
              (cl-find-if
               (lambda (word)
                 (> (or (car (pyim-dcache-get word 'iword2count-recent1)) 0) 1))
               personal-words)
              ;; 最近输入的50个不同的词中出现过三次以上。
              (cl-find-if
               (lambda (word)
                 (> (or (car (pyim-dcache-get word 'iword2count-recent2)) 0) 3))
               personal-words)
              ;; 个人词条中的第一个词。
              (car personal-words))))))

(defun pyim-candidates-create:xingma (imobjs scheme-name &optional async)
  "`pyim-candidates-create' 处理五笔仓颉等形码输入法的函数."
  (unless async
    (let (result)
      (dolist (imobj imobjs)
        (let* ((codes (pyim-codes-create imobj scheme-name))
               (last-code (car (last codes)))
               (other-codes (remove last-code codes))
               output prefix)

          ;; 如果 wubi/aaaa -> 工 㠭；wubi/bbbb -> 子 子子孙孙；wubi/cccc 又 叕；
          ;; 用户输入为： aaaabbbbcccc

          ;; 那么：
          ;; 1. codes       =>   ("wubi/aaaa" "wubi/bbbb" "wubi/cccc")
          ;; 2. last-code   =>   "wubi/cccc"
          ;; 3. other-codes =>   ("wubi/aaaa" "wubi/bbbb")
          ;; 4. prefix      =>   工子
          (when other-codes
            (setq prefix (mapconcat
                          (lambda (code)
                            (pyim-candidates-get-chief
                             scheme-name
                             (pyim-dcache-get code '(icode2word))
                             (pyim-dcache-get code '(code2word))))
                          other-codes "")))

          ;; 5. output => 工子又 工子叕
          (setq output
                ;; NOTE: 下面这种策略是否合理？
                ;; 1. 第一个词选择公共词库中的第一个词。
                ;; 2. 剩下的分成常用字和词，常用字优先排，字和词各按 count 大小排序。
                (let* ((personal-words (pyim-dcache-get last-code '(icode2word)))
                       (personal-words (pyim-candidates-sort personal-words))
                       (common-words (pyim-dcache-get last-code '(code2word)))
                       (chief-word (pyim-candidates-get-chief scheme-name personal-words common-words))
                       (common-words (pyim-candidates-sort common-words))
                       (other-words (pyim-dcache-get last-code '(shortcode2word))))
                  (mapcar (lambda (word)
                            (concat prefix word))
                          `(,chief-word
                            ,@personal-words
                            ,@common-words
                            ,@other-words))))
          (setq output (remove "" (or output (list prefix))))
          (setq result (append result output))))
      (when (car result)
        (delete-dups result)))))

(defun pyim-candidates-create:quanpin (imobjs scheme-name &optional async)
  "`pyim-candidates-create' 处理全拼输入法的函数."
  (if async
      ;; 构建一个搜索中文的正则表达式, 然后使用这个正则表达式在当前 buffer 中搜
      ;; 索词条。
      (let ((str (string-join (pyim-codes-create (car imobjs) scheme-name))))
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
    (let ((counts (make-hash-table :test #'equal))
          (time-limit 0.1)
          words)
      (goto-char (point-min))
      (pyim-time-limit-while (and (not (input-pending-p))
                                  (re-search-forward regexp nil t)) time-limit
        (let ((match (match-string-no-properties 0)))
          ;; NOTE: 单个汉字我觉得不值得收集。
          (when (>= (length match) 2)
            (if (member match words)
                (cl-incf (gethash match counts))
              (push match words)
              (puthash match 1 counts)))))
      (sort words (lambda (a b)
                    (> (or (gethash a counts) 0)
                       (or (gethash b counts) 0)))))))

(defun pyim-candidates-create-quanpin (imobjs scheme-name &optional fast-search)
  "`pyim-candidates-create:quanpin' 内部使用的函数。"
  (let (;; Let indent beautiful.
        jianpin-words znabc-words
        personal-words common-words
        pinyin-chars-1 pinyin-chars-2
        chief-word)
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
                   (string-join (pyim-codes-create imobj scheme-name 1) "-")
                   '(ishortcode2word)))
               (regexp1 (string-join
                         (pyim-codes-create imobj scheme-name)
                         "-"))
               (regexp2 (string-join
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
                  (string-join (pyim-codes-create imobj scheme-name) "-")
                  (if pyim-enable-shortcode
                      '(icode2word ishortcode2word)
                    '(icode2word))))
             ;; 词库词条
             (w2 (pyim-dcache-get
                  (string-join (pyim-codes-create imobj scheme-name) "-")
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
    (setq personal-words (pyim-candidates-sort personal-words))
    (setq chief-word (pyim-candidates-get-chief scheme-name personal-words))

    ;; 调试输出
    (when pyim-debug
      (print (list :imobjs imobjs
                   :chief-word chief-word
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
           `(,chief-word
             ,@personal-words
             ,@jianpin-words
             ,@common-words
             ,@znabc-words
             ,@pinyin-chars-1
             ,@pinyin-chars-2
             )))))

(defun pyim-candidates-create:shuangpin (imobjs scheme-name &optional async)
  "`pyim-candidates-create' 处理双拼输入法的函数."
  (if async
      ;; 构建一个搜索中文的正则表达式, 然后使用这个正则表达式在当前 buffer 中搜
      ;; 索词条。
      (let ((str (string-join (pyim-codes-create (car imobjs) scheme-name))))
        (if (< (length str) 1)
            pyim-candidates
          ;; NOTE: 让第一个词保持不变是不是合理，有待进一步的观察。
          `(,(car pyim-candidates)
            ,@(pyim-candidates-search-buffer
               ;; 按照 pyim 的内部设计，这里得到的 str 其实是全拼，所以要按照全
               ;; 拼的规则来生成 cregexp.
               (let ((pyim-default-scheme 'quanpin))
                 (pyim-cregexp-build str 3 t)))
            ,@(cdr pyim-candidates))))
    (pyim-candidates-create:quanpin imobjs 'quanpin async)))

;; * Footer
(provide 'pyim-candidates)

;;; pyim-candidates.el ends here
