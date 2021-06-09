;;; pyim-punctuation.el --- punctuation tools for pyim.        -*- lexical-binding: t; -*-

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

(defgroup pyim-punctuation nil
  "Punctuation libs for pyim."
  :group 'pyim)

(defcustom pyim-punctuation-dict
  '(("'" "‘" "’")
    ("\"" "“" "”")
    ("_" "——")
    ("^" "…")
    ("]" "】")
    ("[" "【")
    ("@" "◎")
    ("?" "？")
    (">" "》")
    ("=" "＝")
    ("<" "《")
    (";" "；")
    (":" "：")
    ("/" "、")
    ("." "。")
    ("-" "－")
    ("," "，")
    ("+" "＋")
    ("*" "×")
    (")" "）")
    ("(" "（")
    ("&" "※")
    ("%" "％")
    ("$" "￥")
    ("#" "＃")
    ("!" "！")
    ("`" "・")
    ("~" "～")
    ("}" "』")
    ("|" "÷")
    ("{" "『"))
  "标点符号表."
  :type 'list)

(defcustom pyim-punctuation-half-width-functions nil
  "让 pyim 输入半角标点.

取值为一个函数列表，这个函数列表中的任意一个函数的运行结果为 t 时，
pyim 输入半角标点，函数列表中每个函数都有一个参数：char ，表示
最后输入的一个字符，具体见: `pyim-process-outcome-handle-char' 。"
  :type '(choice (const nil)
                 (repeat function)))

(defvar pyim-punctuation-translate-p '(auto yes no)
  "这个变量的第一个元素的取值用于控制标点符号全角半角模式切换.

1. 当第一个元素为 'yes 时，输入全角标点。
2. 当第一个元素为 'no 时，输入半角标点。
3. 当第一个元素为 'auto 时，根据中英文环境，自动切换。")

(defvar pyim-punctuation-pair-status
  '(("\"" nil) ("'" nil))
  "成对标点符号切换状态.")

(defvar pyim-punctuation-escape-list (number-sequence ?0 ?9)
  "Punctuation will not insert after this characters.

If you don't like this function, set the variable to nil")

(pyim-register-local-variables
 '(pyim-punctuation-translate-p
   pyim-punctuation-pair-status
   pyim-punctuation-escape-list
   pyim-punctuation-half-width-functions))

;; ** 切换中英文标点符号
(defun pyim-punctuation-toggle ()
  "Pyim 标点符号全角半角模式切换命令.

每次运行 `pyim-punctuation-toggle' 命令，都会调整变量
`pyim-punctuation-translate-p' 的取值，`pyim-process-outcome-handle-char' 根据
`pyim-process-punctuation-full-width-p' 函数的返回值，来决定是否转换标点
符号：

1. 当返回值为 'yes 时，`pyim-process-outcome-handle-char' 转换标点符号，从而输入全角标点。
2. 当返回值为 'no 时，`pyim-process-outcome-handle-char' 忽略转换，从而输入半角标点。
3. 当返回值为 'auto 时，根据中英文环境，自动切换。"
  (interactive)
  (setq pyim-punctuation-translate-p
        `(,@(cdr pyim-punctuation-translate-p)
          ,(car pyim-punctuation-translate-p)))
  (message
   (cl-case (car pyim-punctuation-translate-p)
     (yes "开启全角标点输入模式。")
     (no "开启半角标点输入模式。")
     (auto "开启全半角标点自动转换模式。"))))

(defun pyim-punctuation-translate-at-point ()
  "切换光标处标点的样式(全角 or 半角).

用户也可以使用命令 `pyim-punctuation-translate-at-point' 来切换
 *光标前* 标点符号的样式。"
  (interactive)
  (let* ((current-char (char-to-string (preceding-char)))
         (punc-list
          (cl-some (lambda (x)
                     (when (member current-char x) x))
                   pyim-punctuation-dict)))
    (when punc-list
      (delete-char -1)
      (if (equal current-char (car punc-list))
          (insert (pyim-punctuation-return-proper-punct punc-list))
        (insert (car punc-list))))))

(defun pyim-punctuation-translate (&optional punct-style)
  "将光标前1个或前后连续成对的n个标点符号进行全角/半角转换.

当 PUNCT-STYLE 设置为 'full-width 时，所有的标点符号转换为全角符
号，设置为 'half-width 时，转换为半角符号。"
  (interactive)
  (let ((punc-list (flatten-tree pyim-punctuation-dict))
        (punct-style
         (or punct-style
             (intern (completing-read
                      "将光标处的标点转换为" '("full-width" "half-width")))))
        ;; lnum : puncts on the left (before point)
        (lnum 0)
        ;; rnum : puncts on the right (after point)
        (rnum 0)
        (point (point))
        last-puncts result)
    (catch 'break
      (while t
        (let ((str (pyim-char-after-to-string rnum)))
          (if (member str punc-list)
              (cl-incf rnum)
            (throw 'break nil)))))
    (catch 'break
      (while (<= lnum rnum)
        (let ((str (pyim-char-before-to-string lnum)))
          (if (member str punc-list)
              (cl-incf lnum)
            (throw 'break nil)))))
    ;; 右侧与左侧成对匹配
    (setq rnum (min lnum rnum))
    (setq last-puncts (buffer-substring (- point lnum) (+ point rnum)))
    ;; 删除旧的标点符号
    (delete-char rnum)
    (delete-char (- 0 lnum))
    (dolist (punct (split-string last-puncts ""))
      (dolist (puncts pyim-punctuation-dict)
        (let ((position (cl-position punct puncts :test #'equal)))
          (when position
            (cond
             ((eq punct-style 'full-width)
              (if (= position 0)
                  (push (pyim-punctuation-return-proper-punct puncts) result)
                (push punct result)))
             ((eq punct-style 'half-width)
              (if (= position 0)
                  (push punct result)
                (push (car puncts) result))))))))
    (insert (mapconcat #'identity (reverse result) ""))
    (backward-char rnum)))

(defun pyim-punctuation-return-proper-punct (punc-list &optional before)
  "返回合适的标点符号，PUNCT-LIST 为标点符号列表.

这个函数用于处理成对的全角标点符号，简单来说：如果第一次输入的标
点是：（‘）时，那么下一次输入的标点就是（’）。

PUNCT-LIST 格式类似：

   `(\",\" \"，\") 或者：`(\"'\" \"‘\" \"’\")

当 BEFORE 为 t 时，只返回切换之前的结果，这个用来获取切换之前的
标点符号。

函数 `pyim-punctuation-return-proper-punct' 内部，我们使用变量
`pyim-punctuation-pair-status' 来记录 “成对” 中文标点符号的状态。"
  (let* ((str (car punc-list))
         (punc (cdr punc-list))
         (switch-p (cdr (assoc str pyim-punctuation-pair-status))))
    (if (= (safe-length punc) 1)
        (car punc)
      (if before
          (setq switch-p (not switch-p))
        (setf (cdr (assoc str pyim-punctuation-pair-status))
              (not switch-p)))
      (if switch-p
          (car punc)
        (nth 1 punc)))))


;; * Footer
(provide 'pyim-punctuation)

;;; pyim-punctuation.el ends here
