;;; pyim-cstring-utils.el --- Chinese string tools for pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-cstring)
(require 'pyim-dhashcache)

(defgroup pyim-cstring nil
  "Chinese string tools for pyim."
  :group 'pyim)

;; ** 中文字符串分词相关功能
(defun pyim-cstring-split-to-list (chinese-string
                                   &optional
                                   max-word-length
                                   delete-dups
                                   prefer-short-word)
  "一个基于 pyim 的中文分词函数。这个函数可以将中文字符串
CHINESE-STRING 分词，得到一个词条 alist，这个 alist 的元素都是列
表，其中第一个元素为分词得到的词条，第二个元素为词条相对于字符串
中的起始位置，第三个元素为结束位置。分词时，默认词条不超过6个字符，
用户可以通过 MAX-WORD-LENGTH 来自定义，但值得注意的是：这个值设置
越大，分词速度越慢。

如果 DELETE-DUPS 设置为 non-nil, 一个中文字符串只保留一种分割方式。
比如：

     我爱北京天安门 => 我爱 北京 天安门

如果 PREFER-SHORT-WORD 为 non-nil, 去重的时候则优先保留较短的词。

注意事项：

1. 这个工具使用暴力匹配模式来分词，*不能检测出* pyim 词库中不存在
   的中文词条。

2. 这个函数的分词速度比较慢，仅仅适用于中文短句的分词，不适用于文
   章分词。根据评估，20个汉字组成的字符串需要大约0.3s， 40个汉字
   消耗1s，随着字符串长度的增大消耗的时间呈几何倍数增加。"
  ;; 如果 pyim 词库没有加载，加载 pyim 词库，确保 `pyim-dcache-get' 可以正常运行。
  (pyim-dcache-init-variables)

  (let (result)
    (dolist (string-list (pyim-cstring--substrings chinese-string max-word-length))
      (let ((pinyin-list (pyim-cstring-to-pinyin (car string-list) nil "-" t)))
        (dolist (pinyin pinyin-list)
          (let ((words (pyim-dcache-get pinyin '(code2word)))) ; 忽略个人词库可以提高速度
            (dolist (word words)
              (when (equal word (car string-list))
                (push string-list result)))))))

    (if delete-dups
        ;;  判断两个词条在字符串中的位置是否冲突，如果冲突，仅保留一个。
        (cl-delete-duplicates
         result
         :test (lambda (x1 x2)
                 (let ((begin1 (nth 1 x1))
                       (begin2 (nth 1 x2))
                       (end1 (nth 2 x1))
                       (end2 (nth 2 x2)))
                   (not (or (<= end1 begin2)
                            (<= end2 begin1)))))
         :from-end prefer-short-word)
      result)))

(defun pyim-cstring-split-to-string (string
                                     &optional
                                     prefer-short-word
                                     separator
                                     max-word-length)
  "将中文字符串 STRING 分词.

在分词的位置插入空格或者自定义分隔符 SEPERATERS，默认情况下较长的
词条优先使用，如果 PREFER-SHORT-WORD 设置为 t，则优先使用较短的词
条。默认最长词条不超过6个字符，用户可以通 MAX-WORD-LENGTH 来自定
义词条的最大长度，但值得注意的是，这个值设置越大，分词速度越慢。"
  (mapconcat (lambda (str)
               (when (> (length str) 0)
                 (if (not (pyim-string-match-p "\\CC" str))
                     (pyim-cstring--split-to-string
                      str prefer-short-word separator max-word-length)
                   str)))
             (pyim-pymap-split-string string) (or separator " ")))

(defun pyim-cstring--split-to-string (chinese-string
                                      &optional
                                      prefer-short-word
                                      separator
                                      max-word-length)
  "`pyim-cstring-split-to-string' 内部函数。"
  (let ((str-length (length chinese-string))
        (word-list (pyim-cstring-split-to-list
                    chinese-string max-word-length t prefer-short-word))
        position-list result)

    ;; 提取词条相对于字符串的位置信息。
    (dolist (word word-list)
      (push (nth 1 word) position-list)
      (push (nth 2 word) position-list))

    ;; 将位置信息由小到大排序。
    (setq position-list
          (cl-delete-duplicates (sort position-list #'<)))

    ;; 在分词的位置插入空格或者用户指定的分隔符。
    (dotimes (i str-length)
      (when (and (> i 0) (member i position-list))
        (push (or separator " ") result))
      (push (substring chinese-string i (1+ i)) result))
    (setq result (nreverse result))
    (string-join result)))

(defun pyim-cstring-split-buffer ()
  "将一个 buffer 中的中文文章，进行分词操作。"
  (interactive)
  (message "分词开始！")
  (goto-char (point-min))
  (while (not (eobp))
    (let ((string (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
      (delete-region (line-beginning-position)
                     (min (+ (line-end-position) 1) (point-max)))
      (insert (pyim-cstring-split-to-string string))
      (insert "\n")))
  (goto-char (point-min))
  (message "分词完成！"))

;; ** 获取光标处中文词条的功能
(defun pyim-cstring-words-at-point (&optional end-of-point)
  "获取光标当前的词条列表，当 END-OF-POINT 设置为 t 时，获取光标后的
词条列表。词条列表的每一个元素都是列表，这些列表的第一个元素为词
条，第二个元素为光标处到词条头部的距离，第三个元素为光标处到词条
尾部的距离。

其工作原理是：

1. 使用 `thing-at-point' 获取当前光标处的一个字符串，一般而言：英
   文会得到一个单词，中文会得到一个句子。

2. 英文单词直接返回这个单词的列表。

3. 中文句子首先用 `pyim-cstring-split-to-list' 分词，然后根据光标
   在中文句子中的位置，筛选出符合要求的中文词条。得到并返回 *一个*
   或者 *多个* 词条的列表。"
  ;;
  ;;                                光标到词 光标到词
  ;;                                首的距离 尾的距离
  ;;                                       | |
  ;; 获取光标当前的词<I>条列表 -> (("的词" 2 0) ("词条" 1 1))
  ;;
  (let* ((case-fold-search t)
         (current-pos (point))
         (current-char
          (if end-of-point
              (string (following-char))
            (string (preceding-char))))
         (str (thing-at-point 'word t))
         (str-length (length str))
         (str-boundary (bounds-of-thing-at-point 'word))
         (str-beginning-pos (when str-boundary
                              (car str-boundary)))
         (str-end-pos (when str-boundary
                        (cdr str-boundary)))
         (str-offset
          (when (and str-beginning-pos str-end-pos)
            (if (= current-pos str-end-pos)
                (- str-end-pos str-beginning-pos)
              (- current-pos str-beginning-pos))))
         str-offset-adjusted words-alist results)

    ;; 当字符串长度太长时， `pyim-cstring-split-to-list'
    ;; 的速度比较慢，这里确保待分词的字符串长度不超过10.
    (when (and str (not (pyim-string-match-p "\\CC" str)))
      (if (> str-offset 5)
          (progn (setq str-offset-adjusted 5)
                 (setq str (substring str
                                      (- str-offset 5)
                                      (min (+ str-offset 5) str-length))))
        (setq str-offset-adjusted str-offset)
        (setq str (substring str 0 (min 9 str-length)))))

    (cond
     (;; FIXME: 在光标在 buffer 开头或者行首时，直接返回 nil.
      ;; 也许这块应该更好的处理。
      (or (bobp) (eq (point) (line-beginning-position))) nil)
     ((and str (not (pyim-string-match-p "\\CC" str)))
      (setq words-alist
            (pyim-cstring-split-to-list str))
      (dolist (word-list words-alist)
        (let ((word-begin (nth 1 word-list))
              (word-end (nth 2 word-list)))
          (if (if end-of-point
                  (and (< str-offset-adjusted word-end)
                       (>= str-offset-adjusted word-begin))
                (and (<= str-offset-adjusted word-end)
                     (> str-offset-adjusted word-begin)))
              (push (list (car word-list)
                          (- str-offset-adjusted word-begin) ;; 例如： ("你好" 1 1)
                          (- word-end str-offset-adjusted))
                    results))))
      (or results
          (list (if end-of-point
                    (list current-char 0 1)
                  (list current-char 1 0)))))
     (str (list (list str
                      (- current-pos str-beginning-pos)
                      (- str-end-pos current-pos)))))))

;; ** 让 forward/backward 支持中文
(defun pyim-cstring-forward-word (&optional arg)
  "向前移动 ARG 英文或者中文词，向前移动时基于 *最长* 的词移动。"
  (interactive "P")
  (or arg (setq arg 1))
  (dotimes (_ arg)
    (let* ((words (pyim-cstring-words-at-point t))
           (max-length
            (cl-reduce #'max
                       (cons 0 (mapcar (lambda (word)
                                         (nth 2 word))
                                       words))))
           (max-length (max (or max-length 1) 1)))
      (forward-char max-length))))

(defun pyim-cstring-backward-word (&optional arg)
  "向后移动 ARG 个英文或者中文词，向后移动时基于 *最长* 的词移动。"
  (interactive "P")
  (or arg (setq arg 1))
  (dotimes (_ arg)
    (let* ((words (pyim-cstring-words-at-point))
           (max-length
            (cl-reduce #'max
                       (cons 0 (mapcar (lambda (word)
                                         (nth 1 word))
                                       words))))
           (max-length (max (or max-length 1) 1)))
      (backward-char max-length))))

(defalias 'pyim-cwords-at-point #'pyim-cstring-words-at-point)
(defalias 'pyim-forward-word #'pyim-cstring-forward-word)
(defalias 'pyim-backward-word #'pyim-cstring-backward-word)

;; * Footer
(provide 'pyim-cstring-utils)

;;; pyim-cstring-utils.el ends here

