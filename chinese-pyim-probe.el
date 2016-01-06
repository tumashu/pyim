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

;; 这个文件包含了许多 “自动切换到英文” 探测函数，如果用户将某个探测函数添加到
;; `pyim-english-input-switch-function' , 只要这个函数返回值为 t, Chinese-pyim
;; 就自动切换到英文输入模式。

;;; Code:
;; * 代码                                                                 :code:
;; #+BEGIN_SRC emacs-lisp

(defun pyim-probe-outside-string-or-comment-p ()
  "如果当前 POS 不在字符串或者 comment 中时，返回 t, 否则，返回 nil 。
激活这个探针后，只能在字符串和 comment 中输入中文。"
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (let* ((pos (point))
           (ppss (syntax-ppss pos)))
      (not
       (or (car (setq ppss (nthcdr 3 ppss)))
           (car (setq ppss (cdr ppss)))
           (nth 3 ppss))))))

(defun pyim-probe-org-speed-commands-active-p ()
  "激活这个探针后，可以解决 org-speed-commands 与 Chinese-pyim 冲突问题。"
  (and (string= major-mode "org-mode")
       (bolp)
       (looking-at org-heading-regexp)
       org-use-speed-commands))

(defun pyim-probe-isearch-force-english-input ()
  "激活这个探针后，使用 isearch 搜索时，禁用中文输入，强制英文输入。"
  (and pyim-isearch-enable-pinyin-search
       ;; isearch 启动的时候，会设置一个 buffer variable: `isearch-mode'
       ;; 检测所有 buffer 中 `isearch-mode' 的取值，如果任何一个
       ;; 取值为 t, 就说明 isearch 已经启动。
       (cl-some #'(lambda (buf)
                    (buffer-local-value 'isearch-mode buf))
                (buffer-list))))
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'chinese-pyim-probe)

;;; chinese-pyim-probe.el ends here
;; #+END_SRC
