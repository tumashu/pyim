;;; chinese-pyim-company.el --- Integrate company-mode with Chinese-pyim

;; * Header
;; Copyright  2015 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/chinese-pyim
;; Version: 0.0.1
;; Keywords: convenience, Chinese, pinyin, input-method, complete

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

;; * 说明文档                                                           :doc:
;; 这个文件包含了两个 company 补全后端，与 Chinese-pyim 配合使用
;; 可以比较方便的补全中文词条。


;; #+BEGIN_EXAMPLE
;; (require 'chinese-pyim-company)

;; (setq company-idle-delay  0.1)
;; (setq company-minimum-prefix-length 2)
;; (setq company-selection-wrap-around t)
;; (setq company-show-numbers t)
;; (setq company-dabbrev-downcase nil)
;; (setq company-dabbrev-ignore-case nil)
;; (setq company-require-match nil)

;; #+END_EXAMPLE

;;; Code:

;; * 代码                                                               :code:
;; ** require
;; #+BEGIN_SRC emacs-lisp
(require 'chinese-pyim)
(require 'company)
(require 'company-dabbrev)

(defun pyim-company-chinese-complete-p ()
  (let ((string (pyim-char-before-to-string 0)))
    (string-match-p "\\cc" string)))

(defun pyim-company-grab-word (orig-fun)
  (if (pyim-company-chinese-complete-p)
      (pyim-grab-chinese-word
       0 (pyim-char-before-to-string 0))
    (funcall orig-fun)))

(defun pyim-company-dabbrev--make-regexp (orig-fun prefix)
  (if (pyim-company-chinese-complete-p)
      (format "%s[^[:punct:][:blank:]\n]\\{1,6\\}" prefix)
    (funcall orig-fun prefix)))

(defun pyim-company-dabbrev--search (orig-fun regexp &optional limit other-buffer-modes
                                              ignore-comments)
  (let ((words (funcall orig-fun regexp limit other-buffer-modes ignore-comments)))
    (if (pyim-company-chinese-complete-p)
        (mapcar
         #'(lambda (x)
             (concat (company-grab-word)
                     (car (split-string (pyim-split-chinese-string2string
                                         (substring x 2)))))) words)
      words)))

(advice-add 'company-grab-word :around #'pyim-company-grab-word)
(advice-add 'company-dabbrev--make-regexp :around #'pyim-company-dabbrev--make-regexp)
(advice-add 'company-dabbrev--search :around #'pyim-company-dabbrev--search)
;; #+END_SRC

;; * Footer

;; #+BEGIN_SRC emacs-lisp
(provide 'chinese-pyim-company)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; chinese-pyim.el ends here
;; #+END_SRC
