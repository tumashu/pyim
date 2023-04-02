;;; pyim-outcome.el --- outcome handle tools for pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-scheme)

(defgroup pyim-outcome nil
  "Outcome tools for pyim."
  :group 'pyim)

(defvaralias 'pyim-magic-converter 'pyim-outcome-magic-converter)

(defcustom pyim-outcome-magic-converter nil
  "将 “待选词条” 在 “上屏” 之前自动转换为其他字符串.
这个功能可以实现“简转繁”，“输入中文得到英文”之类的功能。"
  :type '(choice (const nil)
                 function))

(defcustom pyim-outcome-trigger "v"
  "用于触发特殊操作的字符，相当与单字快捷键.

输入中文的时候，我们需要快速频繁的执行一些特定的命令，最直接的方
法就是将上述命令绑定到一个容易按的快捷键上，但遗憾的是 emacs 大多
数容易按的快捷键都 *名花有主* 了，甚至找一个 “Ctrl＋单字符”的快
捷键都不太容易，特殊功能触发字符，可以帮助我们实现“单字符”快捷
键，类似 org-mode 的 speed key。

默认情况下，我们可以使用特殊功能触发字符执行下面几个操作（假设触
发字符为 v）：

1. 快速切换中英文标点符号的样式：当光标前的字符是一个标点符号时，
   按 \"v\" 可以切换这个标点的样式。比如：光标在A处的时候，按
   \"v\" 可以将A前面的全角逗号转换为半角逗号。

        你好，-A-

   按 \"v\" 后

        你好,-A-

2. 快速将光标前的词条添加到词库：当光标前的字符是中文字符时，按
   \"num\" + \"v\" 可以将光标前 num 个中文汉字组成的词条添加到个
   人词频文件中，比如：当光标在A处时，按\"4v\"可以将“的红烧肉”
   这个词条加入个人词频文件，默认num不超过9。

        我爱吃美味的红烧肉-A-

值得注意的是，这种方式如果添加的功能太多，会造成许多潜在的冲突。

用户可以使用变量 `pyim-outcome-trigger' 来设置触发字符，默
认的触发字符是：\"v\", 选择这个字符的理由基于全拼输入法的：

1. \"v\" 不是有效的声母，不会对中文输入造成太大的影响。
2. \"v\" 字符很容易按。

pyim 使用函数 `pyim-process-select-handle-char' 来处理特殊功能触发字符。当待输入的
字符是触发字符时，`pyim-process-select-handle-char' 根据光标前的字符的不同来调用不
同的功能，具体见 `pyim-process-select-handle-char' ：

单字快捷键受到输入法方案的限制，比如：全拼输入法可以将其设置为v,
但双拼输入法下设置 v 可能就不行，所以，pyim 首先会检查当前输入法
方案下，这个快捷键设置是否合理有效，如果不是一个合理的设置，则使
用拼音方案默认的 :prefer-triggers 。

具体请参考 `pyim-outcome--get-trigger' 。"
  :type '(choice (const nil) string))

(defcustom pyim-outcome-trigger-function #'pyim-outcome-trigger-function-default
  "可以使用 `pyim-outcome-trigger' 激活的函数。

这个函数与『单字快捷键配合使用』，当光标前面的字符为汉字字符时，
按 `pyim-outcome-trigger' 对应字符，可以调用这个函数来处理
光标前面的文字内容。"
  :type 'function)

(defvar pyim-outcome--history nil
  "记录 pyim outcome 的变化的历史

在 pyim 中 outcome 代表用户通过输入法选择，并最终插入到 buffer
的字符串。

“一次确认就生成的词条” , 当前变量一般只有一个元素，比如：
1. 输入： nihao
2. 输出： 你好
2. 变量取值为： (\"你好\")

“多次确认才能生成词条” , 当前变量记录了选择的历史，比如：

1. 输入： yiersansi
2. 输出： 一二三四
3. 第一次选择：一二
4. 第二次选择：三
5. 第三次选择：四
6. 变量取值为： (\"一二三四\" \"一二三\" \"一二\")")

(defvar pyim-outcome--magic-convert-cache nil
  "用来临时保存 `pyim-outcome-magic-convert' 的结果.
从而加快同一个字符串第二次的转换速度。")

(pyim-register-local-variables '(pyim-outcome--history))

;; ** 选词框相关函数
(defun pyim-outcome-get ()
  "获取 outcome"
  (pyim-outcome--get 0))

(defun pyim-outcome--get (n)
  "获取 outcome"
  (nth n pyim-outcome--history))

(defun pyim-outcome-add (outcome)
  "添加 OUTCOME."
  (push outcome pyim-outcome--history))

(defun pyim-outcome-diff ()
  "OUTCOME 的变化。"
  (string-remove-prefix
   (or (pyim-outcome--get 1) "") (pyim-outcome--get 0)))

(defun pyim-outcome-erase ()
  "清除 OUTCOME."
  (setq pyim-outcome--history nil))

(defun pyim-outcome-magic-convert (str)
  "用于处理 `pyim-outcome-magic-converter' 的函数。"
  (if (functionp pyim-outcome-magic-converter)
      (or (cdr (assoc str pyim-outcome--magic-convert-cache))
          (let ((result (funcall pyim-outcome-magic-converter str)))
            (setq pyim-outcome--magic-convert-cache
                  `((,str . ,result)))
            result))
    str))

(defun pyim-outcome-trigger-p (str)
  "判断 STR 是否是一个 trigger."
  (equal (pyim-outcome--get-trigger) str))

(defun pyim-outcome--get-trigger ()
  "检查 `pyim-outcome-trigger' 是否为一个合理的 trigger char 。

pyim 的 translate-trigger-char 要占用一个键位，为了防止用户
自定义设置与输入法冲突，这里需要检查一下这个键位设置的是否合理，
如果不合理，就返回输入法默认设定。"
  (let* ((user-trigger pyim-outcome-trigger)
         (user-trigger
          (if (characterp user-trigger)
              (char-to-string user-trigger)
            (when (= (length user-trigger) 1)
              user-trigger)))
         (first-char (pyim-scheme-first-chars (pyim-scheme-current)))
         (prefer-triggers (pyim-scheme-prefer-triggers
                           (pyim-scheme-current))))
    (if (and (> (length user-trigger) 0)
             (pyim-string-match-p (regexp-quote user-trigger) first-char))
        (car prefer-triggers)
      user-trigger)))

(defun pyim-outcome-call-trigger-function ()
  (when (functionp pyim-outcome-trigger-function)
    (funcall pyim-outcome-trigger-function)
    (message "PYIM: 运行 `pyim-outcome-trigger-function' 函数。")))

(defun pyim-outcome-trigger-function-default (&optional no-space)
  "默认的 `pyim-outcome-trigger-function'.

这个函数可以清理当前行的内容，比如：删除不必要的空格，等。"
  (interactive)
  (let* ((begin (line-beginning-position))
         (end (point))
         (string (buffer-substring-no-properties begin end))
         (sep (if no-space "" " "))
         new-string)
    (when (> (length string) 0)
      (delete-region begin end)
      (with-temp-buffer
        (insert string)
        (dolist (x `(;; 中文标点与英文之间的空格
                     ("\\([，。；？！；、）】]\\) *\\([[:ascii:]]\\)" . "")
                     ;; 英文与中文标点之间的空格
                     ("\\([[:ascii:]]\\) *\\([（【]\\)" . "")
                     ;; 英文与汉字之间的空格
                     ("\\([a-zA-Z]\\) *\\(\\cc\\)" . ,sep)
                     ;; 汉字与英文之间的空格
                     ("\\(\\cc\\) *\\([a-zA-Z]\\)" . ,sep)
                     ;; 汉字与汉字之间的空格
                     ("\\(\\cc\\) +\\(\\cc\\)" . "")))
          (dotimes (_ 3) ;NOTE. 数字3是一个经验数字。
            (goto-char (point-min))
            (while (re-search-forward (car x) nil t)
              (replace-match
               (concat (match-string 1)
                       (cdr x)
                       (match-string 2))
               nil t))))
        (setq new-string (buffer-string)))
      (insert new-string))))

;; * Footer
(provide 'pyim-outcome)

;;; pyim-outcome.el ends here
