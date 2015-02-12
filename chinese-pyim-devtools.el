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

;; ** 自定义生成 README 的命令
;; 添加一个 emacs 命令，用来导出 README，用于 Github。
;; #+BEGIN_SRC emacs-lisp
;;;###autoload
(defun pyim-devtools-generate-readme ()
  "生成 Chinese-pyim README。"
  (interactive)
  (let ((org-export-select-tags '("README"))
        (org-export-filter-paragraph-functions '(pyim-devtools-org-clean-space))
        (indent-tabs-mode nil)
        (tab-width 4))
    (org-export-to-file 'gfm "README.md")))
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
