;;; chinese-pyim-devtools.el --- Tools for Chinese-pyim developers

;;; Header:
;; Copyright 2015 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/chinese-pyim
;; Version: 0.0.1
;; Keywords: convenience, Chinese, pinyin, input-method

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
;; 这个文件包含开发 Chinese-pyim 时需要的函数和命令。

;;; Code:

;; ** 加载必要的库文件
;; #+BEGIN_SRC emacs-lisp
(require 'org)
(require 'ox)
(require 'ox-gfm)
;; #+END_SRC

;; ** 定义一个 org 导出过滤器，处理中文文档中的多余空格
;; 当本文档导出为 README 文档时，中文与中文之间的回车符会转化为空格符，对于中文而言，
;; 这些空格这是多余的，这里定义了一个导出过滤器，当 org 文件导出为 html 以及 markdown
;; 格式时，自动删除中文与中文之间不必要的空格。

;; #+BEGIN_SRC emacs-lisp
(defun pyim-devtools-org-clean-space (text backend info)
  "在export为HTML时，删除中文之间不必要的空格"
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]")
          (string text))
      ;; org默认将一个换行符转换为空格，但中文不需要这个空格，删除。
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
             "\\1\\2" string))
      ;; 删除粗体之前的空格
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) +\\(<\\)" regexp)
             "\\1\\2" string))
      ;; 删除粗体之后的空格
      (setq string
            (replace-regexp-in-string
             (format "\\(>\\) +\\(%s\\)" regexp)
             "\\1\\2" string))
      string)))

;; #+END_SRC

;; ** 自定义 tangle  代码和 export 文档的命令
;; 添加一个 emacs 命令，用来 tangle 代码和导出 README。这里将当前文档
;; 导出为 marokdown 格式和 ascii 格式。前者用于 github，后者用于 emacs
;; 包管理器。

;; #+BEGIN_SRC emacs-lisp
;;;###autoload
(defun pyim-devtools-tangle-and-export ()
  "专门用于 Chinese-pyim 文档导出和代码 tango 的命令。"
  (interactive)
  (let ((org-export-select-tags '("README" "readme"))
        (org-export-filter-paragraph-functions '(pyim-devtools-org-clean-space))
        (readme-md "README.md")
        (readme-ascii "README.txt")
        ;; 导出时用空格代替TAB键
        (indent-tabs-mode nil)
        (tab-width 4))
    (unless (file-exists-p readme-md)
      (write-region "" nil readme-md))
    (unless (file-exists-p readme-ascii)
      (write-region "" nil readme-ascii))
    (org-export-to-file 'gfm "README.md")
    (org-export-to-file 'ascii "README.txt")
    (org-babel-tangle)))
;; #+END_SRC

;; ** 通过 noweb 功能，将 README 文档的内容添加到代码文件中。
;; 这里首先定义一个函数，用于返回一个文件的内容，其作用类似sh命令：cat
;; #+BEGIN_SRC emacs-lisp
(defun pyim-devtools-return-file-content (file-name)
  "返回一个文件的内容，作用类似sh命令：cat"
  (let ((file (expand-file-name file-name)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))
;; #+END_SRC

;;; Footer:
;; #+BEGIN_SRC emacs-lisp
(provide 'chinese-pyim-devtools)

;; Local Variables:
;; coding: utf-8-unix
;; tab-width: 4
;; indent-tabs-mode: nil
;; lentic-init: lentic-orgel-org-init
;; End:

;;; chinese-pyim-devtools.el ends here
;; #+END_SRC
