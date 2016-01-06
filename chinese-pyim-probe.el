;;; chinese-pyim-probe.el --- Auto-Switch-to-English-Input probes for Chinese-pyim

;; * Header
;; Copyright 2015 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/chinese-pyim

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; * 介绍
;; 这个文件包含了许多探针函数，用于实现 *根据环境* “自动切换到英文输入模式” 功能。

;; * 设置
;; 用户将某个探针函数添加到 `pyim-english-input-switch-function' 后, 就可以激活这个
;; 探针函数，比如：

;; #+BEGIN_EXAMPLE
;; (setq-default pyim-english-input-switch-function
;;               '(pyim-probe-dynamic-english
;;                 pyim-probe-isearch-mode
;;                 pyim-probe-program-mode))
;; #+END_EXAMPLE

;; 只要上述 *函数列表* 中，任意一个函数返回值为 t, Chinese-pyim 就自动切换到
;; 英文输入模式。

;;; Code:
;; * 代码                                                                 :code:
;; #+BEGIN_SRC emacs-lisp

(defun pyim-probe-program-mode ()
  "激活这个 Chinese-pyim 探针函数后，只能在字符串和 comment 中输入中文。
注：仅仅影响 `prog-mode' 衍生的 mode 。"
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (let* ((pos (point))
           (ppss (syntax-ppss pos)))
      (not
       (or (car (setq ppss (nthcdr 3 ppss)))
           (car (setq ppss (cdr ppss)))
           (nth 3 ppss))))))

(defun pyim-probe-org-speed-commands ()
  "激活这个 Chinese-pyim 探针函数后，可以解决 org-speed-commands 与 Chinese-pyim 冲突问题。"
  (and (string= major-mode "org-mode")
       (bolp)
       (looking-at org-heading-regexp)
       org-use-speed-commands))

(defun pyim-probe-isearch-mode ()
  "激活这个 Chinese-pyim 探针函数后，使用 isearch 搜索时，禁用中文输入，强制英文输入。"
  (and pyim-isearch-enable-pinyin-search
       ;; isearch 启动的时候，会设置一个 buffer variable: `isearch-mode'
       ;; 检测所有 buffer 中 `isearch-mode' 的取值，如果任何一个
       ;; 取值为 t, 就说明 isearch 已经启动。
       (cl-some #'(lambda (buf)
                    (buffer-local-value 'isearch-mode buf))
                (buffer-list))))

(defun pyim-probe-dynamic-english ()
  "激活这个 Chinese-pyim 探针函数后，使用下面的规则动态切换中英文输入：

1. 当前字符为英文字符时，输入下一个字符时默认开启英文输入
2. 当前字符为中文字符时，输入下一个字符时默认开启中文输入
3. 无论当前是什么输入模式，当输入1个空格后，自动切换到中文输入模式"
  (let ((str-before-1 (pyim-char-before-to-string 0))
        (str-before-2 (pyim-char-before-to-string 1))
        (regexp-chinese "\\cc")
        (regexp-alpha "[a-zA-Z]")
        ;; ascii puncts: !\"#$%&'()*+,-./:;<=>?@\^_`{|}~
        ;; NOTE: "-" must put the end of [].
        (regexp-punct "[@`+=_~&-]"))
    (cond ((and (stringp str-before-1)
                (stringp str-before-2)
                (equal str-before-1 " "))
           (pyim-toggle-full-width-punctuation 1 t) ;; 使用全角标点
           nil)
          ((and (stringp str-before-1)
                (or (pyim-string-match-p regexp-alpha str-before-1)
                    (pyim-string-match-p regexp-punct str-before-1))
                (= (length pyim-guidance-str) 0))
           (pyim-toggle-full-width-punctuation -1 t) ;; 使用半角标点
           t)
          ((pyim-string-match-p regexp-chinese str-before-1)
           (pyim-toggle-full-width-punctuation 1 t)  ;; 使用全角标点
           nil))))
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'chinese-pyim-probe)

;;; chinese-pyim-probe.el ends here
;; #+END_SRC
