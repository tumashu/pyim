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
(require 'pyim-cregexp)
(require 'pyim-cstring)

(defgroup pyim-candidates nil
  "Candidates of pyim."
  :group 'pyim)

(defcustom pyim-enable-shortcode t
  "启用输入联想词功能."
  :type 'boolean)

(defcustom pyim-candidates-search-buffer-p t
  "是否从当前 buffer 搜索词条。

这个功能很好用，但偶尔会导致 pyim 卡顿。"
  :type 'boolean)

(defcustom pyim-candidates-xingma-words-function
  #'pyim-candidates-xingma-words-default
  "形码输入法候选词列表生成函数。

如果形码输入法用户需要微调候选词词频，可以自定义这个函数。"
  :type 'function)

;; ** 获取备选词列表
(defun pyim-candidates--sort (candidates)
  "对 CANDIDATES 进行排序。"
  (pyim-dcache-sort-words candidates))

(cl-defgeneric pyim-candidates-get-chief (scheme &optional personal-words common-words)
  "PYIM 输入法第一位候选词的获取策略。")

(cl-defmethod pyim-candidates-get-chief ((_scheme pyim-scheme-quanpin)
                                         &optional personal-words _common-words)
  "PYIM 输入法第一位候选词的获取通用策略。"
  (or
   ;; 最近输入的10个不同的词中出现一次以上。
   (cl-find-if
    (lambda (word)
      (> (or (car (pyim-dcache-get word '(iword2count-recent-10-words))) 0) 1))
    personal-words)
   ;; 最近输入的50个不同的词中出现过三次以上。
   (cl-find-if
    (lambda (word)
      (> (or (car (pyim-dcache-get word '(iword2count-recent-50-words))) 0) 3))
    personal-words)
   ;; 个人词条中的第一个词。
   (car personal-words)))

(cl-defgeneric pyim-candidates-create (imobjs scheme)
  "按照 SCHEME, 从 IMOBJS 获得候选词条。")

(cl-defmethod pyim-candidates-create (imobjs (scheme pyim-scheme-xingma))
  "按照 SCHEME, 从 IMOBJS 获得候选词条，用于五笔仓颉等形码输入法。"
  (let (result)
    (dolist (imobj imobjs)
      (let* ((codes (pyim-codes-create imobj scheme))
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
                          (car (pyim-candidates-xingma-words code)))
                        other-codes "")))

        ;; 5. output => 工子又 工子叕
        (setq output
              (mapcar (lambda (word)
                        (concat prefix word))
                      (pyim-candidates-xingma-words last-code)))
        (setq output (remove "" (or output (list prefix))))
        (setq result (append result output))))
    (when (car result)
      (delete-dups result))))

(defun pyim-candidates-xingma-words (code)
  "搜索形码 CODE, 得到相应的词条列表。"
  (and (functionp pyim-candidates-xingma-words-function)
       (funcall pyim-candidates-xingma-words-function code)))

(defun pyim-candidates-xingma-words-default (code)
  "搜索形码 CODE, 得到相应的词条列表。

当前的词条的构建规则是：
1. 先排公共词库中的字。
2. 然后再排所有词库中的词，词会按词频动态调整。"
  (let* ((common-words (pyim-dcache-get code '(code2word)))
         (common-chars (pyim-candidates--get-chars common-words))
         (personal-words (pyim-dcache-get code '(icode2word)))
         (other-words (pyim-dcache-get code '(shortcode2word)))
         (words-without-chars
          (pyim-candidates--sort
           (pyim-candidates--remove-chars
            (delete-dups
             `(,@personal-words
               ,@common-words
               ,@other-words))))))
    `(,@common-chars
      ,@words-without-chars)))

(defun pyim-candidates--get-chars (words)
  "从 WORDS 中获取字。"
  (cl-remove-if (lambda (x)
                  (> (length x) 1))
                words))

(defun pyim-candidates--remove-chars (words)
  "把 WORDS 中的字删除。"
  (cl-remove-if (lambda (x)
                  (< (length x) 2))
                words))

(cl-defmethod pyim-candidates-create (imobjs (scheme pyim-scheme-quanpin))
  "按照 SCHEME, 从 IMOBJS 获得候选词条，用于全拼输入法。"
  ;; 这段代码主要实现以下功能：假如用户输入 nihaomazheshi, 但词库里面找不到对
  ;; 应的词条，那么输入法自动用 nihaoma 和 zheshi 的第一个词条："你好吗" 和 "
  ;; 这是" 连接成一个新的字符串 "你好吗这是" 做为第一个候选词。
  (let* ((candidates (pyim-candidates--quanpin imobjs scheme))
         (n (length (car candidates)))
         output)
    (push (car candidates) output)
    (while (and (> n 0)
                (car (setq imobjs
                           (mapcar (lambda (imobj)
                                     (nthcdr n imobj))
                                   imobjs))))
      (let ((candidates (pyim-candidates--quanpin imobjs scheme)))
        (push (car (pyim-candidates--quanpin imobjs scheme t)) output)
        (setq n (length (car candidates)))))
    (append (pyim-subconcat (nreverse output) "")
            candidates)))

(defun pyim-candidates--quanpin (imobjs scheme &optional fast-search)
  "用于全拼输入法的 `pyim-candidates-create' 方法内部使用的函数。"
  (let* ((znabc-words (pyim-candidates--znabc-words imobjs scheme fast-search))
         (jianpin-words (pyim-candidates--jianpin-words imobjs scheme fast-search))
         (quanpin-words (pyim-candidates--quanpin-words imobjs scheme fast-search))
         (personal-words (pyim-candidates--sort (nth 0 quanpin-words)))
         (common-words (nth 1 quanpin-words))
         (chief-word (pyim-candidates-get-chief scheme personal-words))
         (quanpin-chars (pyim-candidates--quanpin-first-chars imobjs scheme fast-search))
         (matched-chars (nth 0 quanpin-chars))
         (possible-chars (nth 1 quanpin-chars))
         (words `( :chief-word ,chief-word
                   :personal-words ,@personal-words
                   :jianpin-words ,@jianpin-words
                   :common-words ,@common-words
                   :znabc-words ,@znabc-words
                   :matched-chars ,@matched-chars
                   :possible-chars ,@possible-chars)))
    (when pyim-debug (print words))
    (delete-dups (cl-remove-if-not #'stringp words))))

(defun pyim-candidates--znabc-words (imobjs scheme &optional fast-search)
  "智能ABC模式，得到尽可能的拼音组合，查询这些组合，得到的词条做为联想词。"
  (let ((codes (mapcar (lambda (x)
                         (pyim-subconcat x "-"))
                       (mapcar (lambda (imobj)
                                 (pyim-codes-create imobj scheme))
                               imobjs))))
    (pyim-zip (mapcar #'pyim-dcache-get
                      (pyim-zip codes))
              fast-search)))

(defun pyim-candidates--jianpin-words (imobjs scheme &optional fast-search)
  "获取简拼词语。

假如输入 \"nih\" ，那么搜索 code 为 \"n-h\" 的词条，然后筛选出所
有拼音匹配\"ni-h\" 或者 \"ni[^-]*-h\" 的词条。"
  (when (and pyim-enable-shortcode
             (> (length (car imobjs)) 1))
    (let (jianpin-words)
      (dolist (imobj imobjs)
        (let* ((w (pyim-dcache-get
                   (string-join (pyim-codes-create imobj scheme 1) "-")
                   '(ishortcode2word)))
               (regexp1 (string-join
                         (pyim-codes-create imobj scheme)
                         "-"))
               (regexp2 (string-join
                         (pyim-codes-create imobj scheme)
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
          (push (delete-dups (append w2 w1)) jianpin-words)))
      (pyim-zip (nreverse jianpin-words) fast-search))))

(defun pyim-candidates--quanpin-words (imobjs scheme &optional fast-search)
  "从 dcache 获取个人词条，词库词条。"
  (let (personal-words common-words)
    (dolist (imobj imobjs)
      (let* ((w1 (pyim-candidates--quanpin-personal-words imobj scheme))
             (w2 (pyim-candidates--quanpin-common-words imobj scheme)))
        (push w1 personal-words)
        (push w2 common-words)))
    (setq personal-words (pyim-zip (nreverse personal-words) fast-search))
    (setq common-words (pyim-zip (nreverse common-words) fast-search))
    (list personal-words common-words)))

(defun pyim-candidates--quanpin-personal-words (imobj scheme)
  (pyim-dcache-get
   (string-join (pyim-codes-create imobj scheme) "-")
   (if pyim-enable-shortcode
       '(icode2word ishortcode2word)
     '(icode2word))))

(defun pyim-candidates--quanpin-common-words (imobj scheme)
  (pyim-dcache-get
   (string-join (pyim-codes-create imobj scheme) "-")
   (if pyim-enable-shortcode
       '(code2word shortcode2word)
     '(code2word))))

(defun pyim-candidates--quanpin-first-chars (imobjs scheme &optional fast-search)
  "获取词条第一汉字列表。"
  (let (matched-chars possible-chars)
    (dolist (imobj imobjs)
      (let* ((w1 (pyim-candidates--quanpin-first-matched-chars imobj scheme))
             (w2 (unless w1
                   (pyim-candidates--quanpin-first-possible-chars imobj scheme))))
        (push w1 matched-chars)
        (push w2 possible-chars)))
    (setq matched-chars (pyim-zip (nreverse matched-chars) fast-search))
    (setq possible-chars (pyim-zip (nreverse possible-chars) fast-search))
    (list matched-chars possible-chars)))

(defun pyim-candidates--quanpin-first-matched-chars (imobj scheme)
  "获取输入的全拼对应的第一个汉字。

假如用户输入 nihao 时，获取 ni 对应的汉字。"
  (let ((code (car (pyim-codes-create imobj scheme))))
    (delete-dups
     `(,@(pyim-dcache-get code '(icode2word code2word))
       ,@(pyim-pymap-py2cchar-get code t t)))))

(defun pyim-candidates--quanpin-first-possible-chars (imobj scheme)
  "获取输入的全拼对应的第一个可能的常用汉字。

假如用户输入 ni 时，获取拼音匹配 ni.* 的常用汉字，比如：ni niao
ning niu 等等。"
  (let ((pinyin (car (pyim-codes-create imobj scheme))))
    (mapcar #'char-to-string
            (pyim-zip
             (mapcar (lambda (x)
                       ;; NOTE: 这里只取最常用的汉字，太多的汉字会带
                       ;; 来后续处理压力，可能拖慢输入法。不过这个结
                       ;; 论只是猜测。
                       (car (split-string x "|")))
                     (pyim-pymap-py2cchar-get pinyin nil 1))))))

(cl-defgeneric pyim-candidates-create-limit-time (_imobjs _scheme)
  "按照 SCHEME, 使用限时运行的方式从 IMOBJS 获得候选词条。

1. 这个函数是同步运行。
2. 这个函数运行有时间限制，运行超过某个时间后，无论有没有结果，必须结束。
3. 这个函数需要探测用户是否输入，如果用户开始输入，这个函数运行必须结束。"
  nil)

(cl-defmethod pyim-candidates-create-limit-time (imobjs (scheme pyim-scheme-quanpin))
  "按照 SCHEME, 用限时运行的方式从 IMOBJS 获得候选词条，用于全拼输入法。"
  ;; 构建一个搜索中文的正则表达式, 然后使用这个正则表达式在当前 buffer 中搜
  ;; 索词条。
  (let ((str (string-join (pyim-codes-create (car imobjs) scheme))))
    (when (and pyim-candidates-search-buffer-p
               (> (length str) 0))
      (pyim-candidates--search-buffer
       (pyim-cregexp-create str scheme 3 t)))))

(defun pyim-candidates--search-buffer (regexp)
  "在当前 buffer 中使用 REGEXP 搜索词条。"
  (when (not (input-pending-p)) ;只有在用户输入停顿的时候才搜索 buffer.
    (save-excursion
      (let ((counts (make-hash-table :test #'equal))
            (time-limit 0.1)
            words)
        (goto-char (point-min))
        (pyim-time-limit-while (and (not (input-pending-p)) ;如果用户继续输入，就停止 buffer 搜索。
                                    (re-search-forward regexp nil t)) time-limit
          (let* ((match (match-string-no-properties 0))
                 (word (propertize match :comment "(buf)")))
            ;; NOTE: 单个汉字我觉得不值得收集。
            (when (>= (length word) 2)
              (if (member word words)
                  (cl-incf (gethash word counts))
                (push word words)
                (puthash word 1 counts)))))
        (sort words (lambda (a b)
                      (> (or (gethash a counts) 0)
                         (or (gethash b counts) 0))))))))

(cl-defmethod pyim-candidates-create-limit-time (imobjs (_scheme pyim-scheme-shuangpin))
  "按照 SCHEME, 用限时运行的方式从 IMOBJS 获得候选词条，用于双拼输入法。"
  ;; 注意：pyim 支持的双拼输入法，内部使用全拼的 imobjs, 所以这里直接调用全拼的
  ;; `pyim-candidates-create-limit-time' 方法来处理 imobjs。
  (pyim-candidates-create-limit-time imobjs (pyim-scheme-get 'quanpin)))

(cl-defgeneric pyim-candidates-create-async (_imobjs _scheme _callback)
  "按照 SCHEME, 使用异步的方式从 IMOBJS 获得候选词条。

获取到的词条后，需要将其做为参数，调用 CALLBACK 函数。"
  nil)

;; * Footer
(provide 'pyim-candidates)

;;; pyim-candidates.el ends here
