;;; pyim-devtools.el --- Tools for pyim developers  -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

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

;; * 说明文档                                                              :doc:
;; 这个文件包含了与 pyim 开发相关的配置或者工具，比如：

;; 1. org-webpage config for pyim

;;; Code:

;; * 代码                                                                 :code:
(require 'org2web)

(defvar pyim-website-repository-directory
  "~/project/emacs-packages/pyim/")

(org2web-add-project
 '("pyim"
   :repository-directory (:eval pyim-website-repository-directory)
   :remote (git "https://github.com/tumashu/pyim.git" "gh-pages")
   :site-domain "https://tumashu.github.io/pyim"
   :site-main-title "pyim"
   :site-sub-title "(一个 emacs 环境下的中文拼音输入法)"
   :default-category "documents"
   :theme (worg killjs)
   :force-absolute-url t
   :source-browse-url ("GitHub" "https://github.com/tumashu/pyim")
   :personal-avatar "/media/img/horse.jpg"
   :personal-duoshuo-shortname "tumashu-website"
   :preparation-function org2web-el2org-preparation-function
   :org-export-function org2web-el2org-org-export-function
   :el2org-doc-sources ("pyim.*\\.el$")
   :el2org-readme-sources ("pyim.el")
   :el2org-index-sources ("pyim.el")
   :web-server-port 9876))

;; * Footer

(provide 'pyim-devtools)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; pyim-devtools.el ends here
