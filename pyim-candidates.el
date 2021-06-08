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
  (unless async
    (let* (;; 如果输入 "ni-hao" ，搜索 code 为 "n-h" 的词条做为联想词。
           ;; 搜索首字母得到的联想词太多，这里限制联想词要大于两个汉字并且只搜索
           ;; 个人文件。
           (jianpin-words
            (when (and (> (length (car imobjs)) 1) pyim-enable-shortcode)
              (pyim-dcache-get
               (mapconcat #'identity
                          (pyim-codes-create (car imobjs) scheme-name 1)
                          "-")
               '(ishortcode2word))))
           znabc-words
           pinyin-chars
           personal-words
           common-words)

      ;; 智能ABC模式，得到尽可能的拼音组合，查询这些组合，得到的词条做
      ;; 为联想词。
      (let* ((codes (pyim-codes-create (car imobjs) scheme-name))
             (n (- (length codes) 1))
             output)
        (dotimes (i (- n 1))
          (let ((lst (cl-subseq codes 0 (- n i))))
            (push (mapconcat #'identity lst "-") output)))
        (dolist (code (reverse output))
          (setq znabc-words (append znabc-words (pyim-dcache-get code)))))

      (dolist (imobj imobjs)
        (setq personal-words
              (append personal-words
                      (pyim-dcache-get
                       (mapconcat #'identity
                                  (pyim-codes-create imobj scheme-name)
                                  "-")
                       (if pyim-enable-shortcode
                           '(icode2word ishortcode2word)
                         '(icode2word)))))

        (setq common-words (delete-dups common-words))
        (setq common-words
              (let* ((cands (pyim-dcache-get
                             (mapconcat #'identity
                                        (pyim-codes-create imobj scheme-name)
                                        "-")
                             (if pyim-enable-shortcode
                                 '(code2word shortcode2word)
                               '(code2word)))))
                (cond
                 ((and (> (length cands) 0)
                       (> (length common-words) 0)
                       (or (eq 1 (length imobj))
                           (eq 2 (length imobj))))
                  ;; 两个单字或者两字词序列合并,确保常用字词在前面
                  (let* ((size (min (length cands) (length common-words)))
                         new-common-words
                         (i 0))
                    ;; 两个序列轮流取出一个元素输入新序列
                    (while (< i size)
                      (push (nth i common-words) new-common-words)
                      (push (nth i cands) new-common-words)
                      (setq i (1+ i)))
                    ;; 较长序列的剩余元素加入新序列
                    (append (nreverse new-common-words)
                            (nthcdr size (cond
                                          ((< size (length cands))
                                           cands)
                                          ((< size (length common-words))
                                           common-words))))))
                 (t
                  (append common-words cands)))))

        (setq pinyin-chars
              (append pinyin-chars
                      (pyim-dcache-get
                       (car (pyim-codes-create imobj scheme-name))))))

      ;; 使用词频信息，对个人词库得到的候选词排序，
      ;; 第一个词的位置比较特殊，不参与排序，
      ;; 具体原因请参考 `pyim-page-select-word' 中的 comment.
      (setq personal-words
            `(,(car personal-words)
              ,@(pyim-dcache-call-api
                 'sort-words (cdr personal-words))))

      ;; Debug
      (when pyim-debug
        (princ (list :imobjs imobjs
                     :personal-words personal-words
                     :common-words common-words
                     :jianpin-words jianpin-words
                     :znabc-words znabc-words
                     :pinyin-chars pinyin-chars)))

      (delete-dups
       (delq nil
             `(,@personal-words
               ,@common-words
               ,@jianpin-words
               ,@znabc-words
               ,@pinyin-chars))))))

(defun pyim-candidates-create:shuangpin (imobjs _scheme-name &optional async)
  "`pyim-candidates-create' 处理双拼输入法的函数."
  (pyim-candidates-create:quanpin imobjs 'quanpin async))

(defun pyim-candidate-parse (candidate)
  (let ((output
         (if (consp candidate)
             (car candidate)
           candidate)))
    (if (stringp output)
        ;; 注：五笔支持 comments 遗留下来的代码，现在作为兼容而保留，
        ;; 等用户的 dcache 都升级之后，这个就可以删除了。
        (car (split-string output ":"))
      output)))

;; * Footer
(provide 'pyim-candidates)

;;; pyim-candidates.el ends here
