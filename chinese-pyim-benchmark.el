;;; chinese-pyim-benchmark.el --- benchmark tools for Chinese-pyim

;; * Header
;; Copyright 2015-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/chinese-pyim
;; Version: 1.5.2
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

;; * 说明文档                                                              :doc:
;; 这个文件包含了一些调试 Chinese-pyim 性能的工具

;;; Code:

;; * 代码                                                                 :code:
(require 'chinese-pyim)

(defun pyim-benchmark ()
  (interactive)
  (let ((pylist (car (pyim-code-split:quanpin "ni-hao" 'quanpin)))
        (time 1000))
    (message "=================================================")
    (message "## Benchmark `pyim-get-sm' ...")
    (benchmark time '(pyim-pinyin-get-sm "ni"))

    (message "## Benchmark `pyim-get-ym' ...")
    (benchmark time '(pyim-pinyin-get-ym "iao"))

    (message "## Benchmark `pyim-get-charpy' ...")
    (benchmark time '(pyim-pinyin-get-charpy "ni"))

    (message "## Benchmark `pyim-code-split:quanpin' ...")
    (benchmark time '(pyim-code-split:quanpin "ni-hao" 'default))

    (message "## Benchmark `pyim-scode-join' ...")
    (benchmark time '(pyim-scode-join pylist nil 'default))

    (message "## Benchmark `pyim-choices-get' ...")
    (benchmark time '(pyim-choices-get (list pylist)'quanpin))

    (message "## Benchmark `pyim-choices-get:dcache-personal' ...")
    (benchmark time '(pyim-choices-get:dcache-personal pylist 'quanpin))

    (message "## Benchmark `pyim-choices-get:dcache-common' ...")
    (benchmark time '(pyim-choices-get:dcache-common pylist 'quanpin))

    (message "## Benchmark `pyim-choices-get:pinyin-char' ...")
    (benchmark time '(pyim-choices-get:pinyin-chars pylist 'quanpin))

    (message "## Benchmark `pyim-hanzi2pinyin' ...")
    (benchmark time '(pyim-hanzi2pinyin "你好" nil "-" t))

    (message "## Benchmark `pyim-cchar2pinyin-get' ...")
    (benchmark time '(pyim-cchar2pinyin-get ?你))

    (message "## Benchmark `pyim-pinyin2cchar-get' ...")
    (benchmark time '(pyim-pinyin2cchar-get "ni"))

    (message "## Benchmark `pyim-build-chinese-regexp-for-pylist' ...")
    (benchmark time '(pyim-spinyin-build-chinese-regexp pylist nil nil t))

    (message "## Benchmark `pyim-dcache-get' ...")
    (benchmark time '(pyim-dcache-get "ni-hao"))
    (message "=================================================")
    (message "")))


;; * Footer

(provide 'chinese-pyim-benchmark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; chinese-pyim-benchmark.el ends here
