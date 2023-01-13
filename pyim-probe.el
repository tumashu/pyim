;;; pyim-probe.el --- Auto-Switch-to-English-Input probes for pyim  -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2015-2021 Free Software Foundation, Inc.

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

;; * 使用说明                                                              :doc:
;; ** 简介
;; 这个文件包含了许多探针函数，用于实现两个功能：

;; 1. 根据环境自动切换到英文输入模式
;; 2. 根据环境自动切换到半角标点输入模式

;; ** 设置
;; *** 根据环境自动切换到英文输入模式
;; 用户将探针函数添加到 `pyim-english-input-switch-functions' 后, 就可以激活这个
;; 探针函数，比如：

;; #+BEGIN_EXAMPLE
;; (setq-default pyim-english-input-switch-functions
;;               '(pyim-probe-dynamic-english
;;                 pyim-probe-isearch-mode
;;                 pyim-probe-program-mode))
;; #+END_EXAMPLE

;; 只要上述 *函数列表* 中，任意一个函数返回值为 t, pyim 就自动切换到
;; 英文输入模式。

;; *** 根据环境自动切换到半角标点输入模式
;; 用户将探针函数添加到 `pyim-punctuation-half-width-functions' 后, 就可以激活这个
;; 探针函数。

;;; Code:
;; * 代码                                                                 :code:
(require 'pyim-common)
(require 'pyim-process)

;; ** 根据环境自动切换到英文输入模式
(defun pyim-probe-program-mode ()
  "激活这个 pyim 探针函数后，只能在字符串和 comment 中输入中文。
注：仅仅影响 `prog-mode' 衍生的 mode 。

用于：`pyim-english-input-switch-functions' 。"
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (let* ((pos (point))
           (ppss (syntax-ppss pos)))
      (not
       (or (car (setq ppss (nthcdr 3 ppss)))
           (car (setq ppss (cdr ppss)))
           (nth 3 ppss))))))

(defvar org-heading-regexp)
(defvar org-use-speed-commands)
(defvar pyim-isearch-mode)
(defvar isearch-mode)

(defun pyim-probe-org-speed-commands ()
  "激活这个 pyim 探针函数后，可以解决 org-speed-commands 与 pyim 冲突问题。

用于：`pyim-english-input-switch-functions' 。"
  (and (string= major-mode "org-mode")
       (bolp)
       (looking-at org-heading-regexp)
       org-use-speed-commands))

(defun pyim-probe-isearch-mode ()
  "激活这个 pyim 探针函数后，使用 isearch 搜索时，禁用中文输入，强制英文输入。

用于：`pyim-english-input-switch-functions' 。"
  (and pyim-isearch-mode
       ;; isearch 启动的时候，会设置一个 buffer variable: `isearch-mode'
       ;; 检测所有 buffer 中 `isearch-mode' 的取值，如果任何一个
       ;; 取值为 t, 就说明 isearch 已经启动。
       (cl-some (lambda (buf)
                  (buffer-local-value 'isearch-mode buf))
                (buffer-list))))

(defun pyim-probe-org-structure-template ()
  "激活这个 pyim 探针函数后，输入 org-structure-template 时，不会开启中文输入。

用于：`pyim-english-input-switch-functions' 。"
  (when (eq major-mode 'org-mode)
    (let ((line-string (buffer-substring (line-beginning-position) (point))))
      (and (looking-at "[ \t]*$")
           (string-match "^[ \t]*<\\([a-zA-Z]*\\)$" line-string)))))

(defun pyim-probe-dynamic-english ()
  "激活这个 pyim 探针函数后，使用下面的规则动态切换中英文输入：

1. 从光标往前找第一个非数字的字符，为中文字符时，输入下一个字符时默认开启中文输入
2. 从光标往前找第一个非数字的字符，为其他字符时，输入下一个字符时默认开启英文输入
3. 使用 `pyim-convert-string-at-point' 可以将光标前的字符串转换为中文，
   所以用户需要给 `pyim-convert-string-at-point' 绑定一个快捷键，比如：

   (global-set-key (kbd \"M-i\") #\\='pyim-convert-string-at-point)

这个函数用于：`pyim-english-input-switch-functions' 。"
  (let* ((offset 0)
         (non-digit-str-before-1 (pyim-char-before-to-string offset)))
    (while (and non-digit-str-before-1
                (cl-search non-digit-str-before-1 "0123456789"))
      (cl-incf offset)
      (setq non-digit-str-before-1 (pyim-char-before-to-string offset)))
    (if (<= (point) (save-excursion (back-to-indentation)
                                    (point)))
        (not (or (pyim-string-match-p
                  "\\cc"
                  (save-excursion
                    ;; 查找前一个非空格字符。
                    (if (re-search-backward "[^[:space:]\n]" nil t)
                        (char-to-string (char-after (point))))))
                 (> (length (pyim-process-get-entered 'point-before)) 0)))
      (not (or (pyim-string-match-p "\\cc" non-digit-str-before-1)
               (> (length (pyim-process-get-entered 'point-before)) 0))))))

(defun pyim-probe-auto-english ()
  "激活这个 pyim 探针函数后，使用下面的规则自动切换中英文输入：

1. 当前字符为英文字符（不包括空格）时，输入下一个字符为英文字符
2. 当前字符为中文字符或输入字符为行首字符时，输入的字符为中文字符
3. 以单个空格为界，自动切换中文和英文字符
   即，形如 `我使用 emacs 编辑此函数' 的句子全程自动切换中英输入法

这个函数用于：`pyim-english-input-switch-functions' 。"
  (let ((str-before-1 (pyim-char-before-to-string 0))
        (str-before-2 (pyim-char-before-to-string 1)))
    (if (> (point) (save-excursion (back-to-indentation)
                                   (point)))
        (or (if (pyim-string-match-p " " str-before-1)
                (pyim-string-match-p "\\cc" str-before-2)
              (and (not (pyim-string-match-p "\\cc" str-before-1))
                   (= (length (pyim-process-get-entered 'point-before)) 0)))))))

(declare-function evil-normal-state-p "evil")
(defun pyim-probe-evil-normal-mode ()
  "判断是否是evil的normal模式，如果是则返回true.
这个函数用于：`pyim-english-input-switch-functions'."
  (evil-normal-state-p))

;; ** 根据环境自动切换到半角标点输入模式
(defvar pyim-punctuation-dict)

(defun pyim-probe-punctuation-line-beginning (char)
  "激活这个 pyim 探针函数后，行首输入标点时，强制输入半角标点。

用于：`pyim-punctuation-half-width-functions' 。"
  (let ((line-string (buffer-substring (line-beginning-position) (point))))
    (and (member (char-to-string char)
                 (mapcar #'car pyim-punctuation-dict))
         (string-match "^[ \t]*$" line-string))))

(declare-function org-inside-LaTeX-fragment-p "org")
(declare-function org-inside-latex-macro-p "org")

(defun pyim-probe-punctuation-after-punctuation (char)
  "激活这个 pyim 探针函数后，半角标点后再输入一个标点符号时，强制输入半角标点。

用于：`pyim-punctuation-half-width-functions' 。"
  (let ((str-before-1 (pyim-char-before-to-string 0))
        (puncts (mapcar #'car pyim-punctuation-dict)))
    (and (member str-before-1 puncts)
         (member (char-to-string char) puncts))))

(defun pyim-probe-org-latex-mode ()
  "org-mode 中的 latex fragment 和 latex 宏指令中自动切换到英文输入."
  (and (eq major-mode 'org-mode)
       (or (org-inside-LaTeX-fragment-p)
           (org-inside-latex-macro-p))))

(defun pyim-probe-exwm-xim-environment ()
  "测试当前是否是 exwm-xim 输入法环境。

这个探针主要用于： `pyim-force-input-chinese-functions'"
  (pyim-exwm-xim-environment-p))

(defvar xwidget-webkit-isearch--read-string-buffer)
(defun pyim-probe-xwidget-webkit-environment ()
  "测试当前是否是 xwidget-webkit 运行环境。

这个探针主要用于： `pyim-force-input-chinese-functions'."
  (or (eq this-original-command
          'xwidget-webkit-pass-command-event-with-input-method)
      (bound-and-true-p xwidget-webkit-isearch--read-string-buffer)))

(cl-pushnew #'pyim-probe-exwm-xim-environment pyim-force-input-chinese-functions)
(cl-pushnew #'pyim-probe-xwidget-webkit-environment pyim-force-input-chinese-functions)

;; * Footer
(provide 'pyim-probe)

;;; pyim-probe.el ends here
