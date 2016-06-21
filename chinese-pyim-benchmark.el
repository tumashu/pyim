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

    (message "## Benchmark `pyim-get-choices:guess-words' ...")
    (benchmark time '(pyim-get-choices:guess-words pylist))

    (message "## Benchmark `pyim-get-choices:dabbrev' ...")
    (benchmark time '(pyim-get-choices:dabbrev pylist))

    (message "## Benchmark `pyim-get-dabbrev' 1 ...")
    (benchmark time '(save-excursion
                       (pyim-get-dabbrev
                        (pyim-build-chinese-regexp-for-pylist pylist nil nil t)
                        pyim-dabbrev-time-limit)))

    (message "## Benchmark `pyim-get-dabbrev' 2  ...")
    (benchmark time '(save-excursion
                       (pyim-get-dabbrev
                        "[你呢尼泥逆倪匿拟腻妮霓昵溺旎睨鲵坭猊怩伲祢慝铌年念廿粘碾捻蔫撵拈黏鲶鲇辇埝娘酿鸟尿袅嬲茑脲捏涅聂孽蹑嗫啮镊镍乜陧颞臬蘖您恁宁凝拧泞咛狞柠佞聍苎甯牛纽扭妞钮拗忸狃][好号毫豪浩耗皓嚎昊郝壕蒿貉灏镐嗥嚆薅濠蚝颢]"
                        pyim-dabbrev-time-limit)))

    (message "## Benchmark `pyim-get-dabbrev-time-limit-while' ...")
    (benchmark time '(let ((tmp-end (point))
                           (start (current-time))
                           symbols)
                       (pyim-get-dabbrev-time-limit-while (> tmp-end (point-min))
                           start pyim-dabbrev-time-limit 1
                           (ignore-errors
                             (forward-char -10000))
                           (forward-line 0)
                           (save-excursion
                             ;; Before, we used backward search, but it matches non-greedily, and
                             ;; that forced us to use the "beginning/end of word" anchors in
                             ;; search regexp.
                             (while (re-search-forward " " tmp-end t)
                               (let ((match (match-string-no-properties 0)))
                                 (when (>= (length match) 2)
                                   (push match symbols)))))
                           (setq tmp-end (point)))))

    (message "## Benchmark `re-search-forward' chinese chars ...")
    (benchmark time '(save-excursion
                       (re-search-forward
                        (pyim-build-chinese-regexp-for-pylist pylist nil nil t)
                        nil t)))

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
