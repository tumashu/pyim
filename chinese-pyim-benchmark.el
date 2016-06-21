;;; chinese-pyim-benchmark.el --- benchmark tools for Chinese-pyim

;; * Header
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

;; * 说明文档                                                              :doc:
;; 这个文件包含了一些调试 Chinese-pyim 性能的工具

;;; Code:

;; * 代码                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(require 'chinese-pyim)

(defun pyim-benchmark ()
  (interactive)
  (let ((pylist (car (pyim-split-string:quanpin "ni-hao" 'default)))
        (time 1000))
    (message "=================================================")
    (message "## Benchmark `pyim-get-sm' ...")
    (benchmark time '(pyim-get-sm "ni"))

    (message "## Benchmark `pyim-get-ym' ...")
    (benchmark time '(pyim-get-ym "iao"))

    (message "## Benchmark `pyim-get-charpy' ...")
    (benchmark time '(pyim-get-charpy "ni"))

    (message "## Benchmark `pyim-split-string:quanpin' ...")
    (benchmark time '(pyim-split-string:quanpin "ni-hao" 'default))

    (message "## Benchmark `pyim-pylist-to-string' ...")
    (benchmark time '(pyim-pylist-to-string pylist nil 'default))

    (message "## Benchmark `pyim-get-choices' ...")
    (benchmark time '(pyim-get-choices (list pylist)))

    (message "## Benchmark `pyim-get-choices:personal-file' ...")
    (benchmark time '(pyim-get-choices:personal-file pylist))

    (message "## Benchmark `pyim-get-choices:pinyin-dict' ...")
    (benchmark time '(pyim-get-choices:pinyin-dict pylist))

    (message "## Benchmark `pyim-get-choices:char' ...")
    (benchmark time '(pyim-get-choices:chars pylist))

    (message "## Benchmark `pyim-hanzi2pinyin' ...")
    (benchmark time '(pyim-hanzi2pinyin "你好" nil "-" t))

    (message "## Benchmark `pyim-cchar2pinyin-get' ...")
    (benchmark time '(pyim-cchar2pinyin-get ?你))

    (message "## Benchmark `pyim-pinyin2cchar-get' ...")
    (benchmark time '(pyim-pinyin2cchar-get "ni"))

    (message "## Benchmark `pyim-build-chinese-regexp-for-pylist' ...")
    (benchmark time '(pyim-build-chinese-regexp-for-pylist pylist nil nil t))

    (message "## Benchmark `pyim-get' ...")
    (benchmark time '(pyim-get "ni-hao" '(pinyin-dict)))
    (message "=================================================")
    (message "")))

;; #+END_SRC

;; * Footer

;; #+BEGIN_SRC emacs-lisp
(provide 'chinese-pyim-benchmark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; chinese-pyim-benchmark.el ends here
;; #+END_SRC
