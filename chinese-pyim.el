;;; chinese-pyim.el --- Please install 'pyim' package instead.

;; * Header
;; Copyright 2015-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/chinese-pyim
;; Version: 1.5.2
;; Package-Requires: ((pyim "1.5.2"))
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

;; chinese-pyim is renamed to "pyim", please use "pyim" package instead,
;; chinese-pyim package will be removed in the future.

;;; Code:
;; * 代码                                                                 :code:
(require 'pyim)

;; For compatible
(register-input-method "chinese-pyim" "euc-cn" 'pyim-start pyim-title)

(message "

------------------------------------------------------------------
|                   Chinese-pyim 重要更新                        |
|                                                                |
|  chinese-pyim 已经重命名为 pyim, 请同学们安装 pyim 并按照      |
|  下面的例子修改自己的配置，然后删除 chinese-pyim 包。          |
|                                                                |
|    (require 'pyim)                                             |
|    (require 'pyim-basedict)                                    |
|    (pyim-basedict-enable)                                      |
|    (setq default-input-method \"pyim\")                        |
|                                                                |
|  给大家带来的不便我深表歉意，感谢大家的支持和理解。            |
------------------------------------------------------------------

")


;; * Footer
(provide 'chinese-pyim)

;;; chinese-pyim.el ends here
