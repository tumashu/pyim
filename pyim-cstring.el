;;; pyim-cstring.el --- Chinese string tools for pyim.        -*- lexical-binding: t; -*-

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

(defvar pyim-cstring-to-code-criteria nil
  "用于 code 选取的基准字符串。

`pyim-cstring-to-codes' 获取到一个词条的多个 codes 时，会将所有的
codes 与这个字符串进行比较，然后选择一个最相似的 code 输出.

这个字符串主要用于全拼和双拼输入法的多音字矫正，一般使用用户输入
生成的 imobjs 转换得到，保留了用户原始输入的许多信息。")

(defun pyim-cstring-partition (string &optional to-cchar)
  "STRING partition.

1. Hello你好 -> (\"Hello\" \"你\" \"好\"), when TO-CCHAR is non-nil.
2. Hello你好 -> (\"Hello\" \"你好\"), when TO-CCHAR is nil."
  ;; NOTE: 使用5个\0作为分割符有没有其它副作用？有待观察。
  (let ((sep (make-string 5 ?\0)))
    (if (pyim-string-match-p "\\CC" string)
        ;; 处理中英文混合的情况
        (remove "" (split-string
                    (replace-regexp-in-string
                     (if to-cchar "\\(\\cc\\)" "\\(\\cc+\\)")
                     (concat sep "\\1" sep) string)
                    sep))
      (if to-cchar
          (cl-mapcar #'char-to-string string)
        (list string)))))

(defun pyim-cstring-substrings (cstring &optional max-length number)
  "找出 CSTRING 中所有长度不超过 MAX-LENGTH 的子字符串，生成一个 alist。

这个 alist 中的每个元素为：(子字符串 开始位置 结束位置), 参数
NUMBER 用于递归，表示子字符串在 CSTRING 中的位置。"
  (let ((number (or number 0)))
    (cond
     ((= (length cstring) 0) nil)
     (t (append (pyim-cstring-substrings-1 cstring max-length number)
                (pyim-cstring-substrings (substring cstring 1)
                                         max-length (1+ number)))))))

(defun pyim-cstring-substrings-1 (cstring max-length number)
  "`pyim-cstring-substrings' 的内部函数。"
  (cond
   ((< (length cstring) 2) nil)
   (t (append
       (let ((length (length cstring)))
         (when (<= length (or max-length 6))
           (list (list cstring number (+ number length)))))
       (pyim-cstring-substrings-1
        (substring cstring 0 -1)
        max-length number)))))

;; ** 中文字符串分词相关功能
(defun pyim-cstring-split-to-list (chinese-string &optional max-word-length delete-dups prefer-short-word)
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
章分词。根据评估，20个汉字组成的字符串需要大约0.3s， 40个汉字消耗
1s，随着字符串长度的增大消耗的时间呈几何倍数增加。"
  ;; 如果 pyim 词库没有加载，加载 pyim 词库，确保 `pyim-dcache-get' 可以正常运行。
  (pyim-dcache-init-variables)

  (let (result)
    (dolist (string-list (pyim-cstring-substrings chinese-string max-word-length))
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

(defun pyim-cstring-split-to-string (string &optional prefer-short-word
                                            separator max-word-length)
  "将中文字符串 STRING 分词.

在分词的位置插入空格或者自定义分隔符 SEPERATERS，默认情况下较长的
词条优先使用，如果 PREFER-SHORT-WORD 设置为 t，则优先使用较短的
词条。默认最长词条不超过6个字符，用户可以通 MAX-WORD-LENGTH 来
自定义词条的最大长度，但值得注意的是，这个值设置越大，分词速度越
慢。"
  (mapconcat (lambda (str)
               (when (> (length str) 0)
                 (if (not (pyim-string-match-p "\\CC" str))
                     (pyim-cstring-split-to-string-1
                      str prefer-short-word separator max-word-length)
                   str)))
             (pyim-cstring-partition string) (or separator " ")))

(defun pyim-cstring-split-to-string-1 (chinese-string &optional prefer-short-word
                                                      separator max-word-length)
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

;; ** 中文字符串到拼音的转换工具
;;;###autoload
(defun pyim-cstring-to-pinyin (string &optional shou-zi-mu separator
                                      return-list ignore-duo-yin-zi adjust-duo-yin-zi)
  "将汉字字符串转换为对应的拼音字符串的工具.

如果 SHOU-ZI-MU 设置为 t, 转换仅得到拼音首字母字符串。当
RETURN-LIST 设置为 t 时，返回一个拼音列表，这个列表包含词条的一个
或者多个拼音（词条包含多音字时）；如果 IGNORE-DUO-YIN-ZI 设置为
t, 遇到多音字时，只使用第一个拼音，其它拼音忽略；当
ADJUST-DUO-YIN-Zi 设置为 t 时, `pyim-cstring-to-pinyin' 会使用 pyim 已
安装的词库来校正多音字，但这个功能有一定的限制:

1. pyim 普通词库中不存在的词条不能较正
2. 多音字校正速度比较慢，实时转换会产生卡顿。

BUG: 当 STRING 中包含其它标点符号，并且设置 SEPERATER 时，结果会
包含多余的连接符：比如： '你=好' --> 'ni-=-hao'"
  (if (not (pyim-string-match-p "\\cc" string))
      (if return-list
          (list string)
        string)
    (let (pinyins-list pinyins-list-adjusted)
      ;; ("Hello" "银" "行") -> (("Hello") ("yin") ("hang" "xing"))
      (setq pinyins-list
            (mapcar (lambda (str)
                      (if (pyim-string-match-p "\\cc" str)
                          (pyim-pymap-cchar2py-get str)
                        (list str)))
                    (pyim-cstring-partition string t)))

      ;; 通过排列组合的方式, 重排 pinyins-list。
      ;; 比如：(("Hello") ("yin") ("hang" "xing")) -> (("Hello" "yin" "hang") ("Hello" "yin" "xing"))
      (setq pinyins-list
            (pyim-permutate-list pinyins-list))

      ;; 使用 pyim 的安装的词库来校正多音字。
      ;; FIXME：如果 string 包含非中文的字符，那么多音字矫正将不起作用。
      (when adjust-duo-yin-zi
        (pyim-dcache-init-variables)
        (dolist (pylist pinyins-list)
          (let* ((py-str (mapconcat #'identity pylist "-"))
                 (words-from-dicts
                  (pyim-dcache-get py-str '(code2word))))
            (when (member string words-from-dicts)
              (push pylist pinyins-list-adjusted))))
        (setq pinyins-list-adjusted
              (nreverse pinyins-list-adjusted)))

      ;; 返回拼音字符串或者拼音列表
      (let* ((pinyins-list
              (or pinyins-list-adjusted
                  pinyins-list))
             (list (mapcar (lambda (x)
                             (mapconcat (lambda (str)
                                          (if shou-zi-mu
                                              (substring str 0 1)
                                            str))
                                        x separator))
                           (if ignore-duo-yin-zi
                               (list (car pinyins-list))
                             pinyins-list))))
        (if return-list
            list
          (string-join list " "))))))

;;;###autoload
(defun pyim-cstring-to-pinyin-simple (string &optional shou-zi-mu separator return-list)
  "简化版的 `pyim-cstring-to-pinyin', 不处理多音字。"
  (pyim-cstring-to-pinyin string shou-zi-mu separator return-list t))

;; ** 中文字符串到形码的转换工具
(defun pyim-cstring-to-xingma (string scheme-name &optional return-list)
  "返回汉字 STRING 对应形码方案 SCHEME-NAME 的 code (不包括
code-prefix)。当RETURN-LIST 设置为 t 时，返回一个 code list。"
  (when (string-match-p "^\\cc+\\'" string)
    (let* ((prefix (pyim-scheme-get-option scheme-name :code-prefix))
           (func (intern (concat "pyim-cstring-to-xingma:" (symbol-name scheme-name))))
           (dcache-codes (mapcar (lambda (x)
                                   (when (string-prefix-p prefix x)
                                     (string-remove-prefix prefix x)))
                                 (sort (cl-copy-list (pyim-dcache-call-api 'search-word-code string))
                                       (lambda (a b) (> (length a) (length b))))))
           (codes (or (remove nil dcache-codes)
                      (and (functionp func)
                           (funcall func string scheme-name)))))
      (when codes
        (if return-list
            codes
          ;; 如果要返回一个字符串，那就返回第一个，也就是最长的那个编码。
          (car codes))))))

(defun pyim-cstring-to-xingma:wubi (string &optional scheme-name)
  "返回汉字 STRING 的五笔编码 (不包括 code-prefix) 编码列表。"
  (let ((length (length string))
        (string (split-string string "" t)))
    (cond
     ;; 双字词，分别取两个字的前两个编码
     ((eq length 2)
      (let ((s1 (pyim-cstring-to-xingma (nth 0 string) scheme-name))
            (s2 (pyim-cstring-to-xingma (nth 1 string) scheme-name)))
        (when (and s1 s2)
          (list (concat (substring s1 0 2)
                        (substring s2 0 2))))))
     ;; 三字词，取前二字的首编码，及第三个字的前两个编码
     ((eq length 3)
      (let ((s1 (pyim-cstring-to-xingma (nth 0 string) scheme-name))
            (s2 (pyim-cstring-to-xingma (nth 1 string) scheme-name))
            (s3 (pyim-cstring-to-xingma (nth 2 string) scheme-name)))
        (when (and s1 s2 s3)
          (list (concat (substring s1 0 1)
                        (substring s2 0 1)
                        (substring s3 0 2))))))
     ;; 四字词及以上，分别前三个字及最后一个字的首编码
     ((> length 3)
      (let ((s1 (pyim-cstring-to-xingma (nth 0 string) scheme-name))
            (s2 (pyim-cstring-to-xingma (nth 1 string) scheme-name))
            (s3 (pyim-cstring-to-xingma (nth 2 string) scheme-name))
            (s4 (pyim-cstring-to-xingma (nth (1- length) string) scheme-name)))
        (when (and s1 s2 s3 s4)
          (list (concat (substring s1 0 1)
                        (substring s2 0 1)
                        (substring s3 0 1)
                        (substring s4 0 1))))))
     (t nil))))

(defun pyim-cstring-to-codes (string scheme-name &optional criteria)
  "将 STRING 转换为 SCHEME-NAME 对应的 codes.

当 pyim class 为拼音，并且提供 CRITERIA 字符串时，检索到的所有
codes 会和这个字符串进行比较，然后选择一个相似度最高的 code 作为
输出，这种处理方式适合拼音输入法，形码输入法一般不需要类似的操作。

CRITERIA 字符串一般是通过 imobjs 构建的，它保留了用户原始的输入信
息。"
  (let ((class (pyim-scheme-get-option scheme-name :class)))
    (cond ((eq class 'xingma)
           (pyim-cstring-to-xingma string scheme-name t))
          ;;拼音使用了多音字校正
          (t (let ((codes (pyim-cstring-to-pinyin string nil "-" t nil t))
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
                 (list (car codes-sorted))))))))

;; ** 获取光标处中文字符串或者中文词条的功能
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

(defun pyim-cstring-words-at-point (&optional end-of-point)
  "获取光标当前的词条列表，当 END-OF-POINT 设置为 t 时，获取光标后的词条列表。
词条列表的每一个元素都是列表，这些列表的第一个元素为词条，第二个元素为光标处到词条
头部的距离，第三个元素为光标处到词条尾部的距离。

其工作原理是：

1. 使用 `thing-at-point' 获取当前光标处的一个字符串，一般而言：英文会得到
   一个单词，中文会得到一个句子。
2. 英文单词直接返回这个单词的列表。
3. 中文句子首先用 `pyim-cstring-split-to-list' 分词，然后根据光标在中文句子
   中的位置，筛选出符合要求的中文词条。得到并返回 *一个* 或者 *多个* 词条
   的列表。"
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
     ((or (bobp) (eq (point) (line-beginning-position))) nil)
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

;; * Footer
(provide 'pyim-cstring)

;;; pyim-cstring.el ends here
