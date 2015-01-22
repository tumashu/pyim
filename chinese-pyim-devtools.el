;;; chinese-pyim-devtools.el --- Tools for Chinese-pyim developers

;; Copyright 2015 Feng Shu
;;
;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/chinese-pyim
;; Version: 0.0.1
;; Keywords: convenience, Chinese, pinyin, input-method

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; 注意: ** 不要手动编辑这个文件（这个文件是 tangle chinese-pyim.org 文件得到的） **
;;
;; 这个文件包含开发 Chinese-pyim 时需要的函数和命令。
;;
;;; Code:

(require 'org)
(require 'ox)
(require 'ox-gfm)

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

;;;###autoload
(defun pyim-devtools-tangle-and-export ()
  "专门用于 Chinese-pyim 文档导出和代码 tango 的命令。"
  (interactive)
  (let ((org-export-select-tags '("README" "readme"))
        (org-export-filter-paragraph-functions '(pyim-devtools-org-clean-space))
        (readme-md "README.md")
        (readme-ascii "README.txt"))
    (unless (file-exists-p readme-md)
      (write-region "" nil readme-md))
    (unless (file-exists-p readme-ascii)
      (write-region "" nil readme-ascii))
    (org-export-to-file 'gfm "README.md")
    (org-export-to-file 'ascii "README.txt")
    (org-babel-tangle)))

(defun pyim-devtools-return-file-content (file-name)
  "返回一个文件的内容，作用类似sh命令：cat"
  (let ((file (expand-file-name file-name)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(provide 'chinese-pyim-devtools)

;; Local Variables:
;; coding: utf-8-unix
;; End:
;;; chinese-pyim-devtools.el ends here
