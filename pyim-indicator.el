;;; pyim-indicator.el --- pyim indicator for pyim.        -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2021 Free Software Foundation, Inc.

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

;;; Code:
;; * 代码                                                           :code:
(require 'cl-lib)
(require 'pyim-common)

(defgroup pyim-indicator nil
  "Indicator for pyim."
  :group 'pyim)

(defcustom pyim-indicator #'pyim-indicator-default
  "PYIM 当前使用的 indicator.
Indicator 用于显示输入法当前输入状态（英文还是中文）。"
  :type 'function)

(defvar pyim-indicator-cursor-color (list "white" "green")
  "`pyim-indicator-default' 使用的 cursor 颜色。")

(defvar pyim-indicator-modeline-string (list "PYIM " "PYIM-EN ")
  "`pyim-indicator-default' 使用的 modeline 字符串。")

(defvar pyim-indicator-timer nil
  "`pyim-indicator-daemon' 使用的 timer.")

(defvar pyim-indicator-timer-repeat 0.2)

(defun pyim-indicator-daemon (func)
  "Indicator daemon, 用于实时显示输入法当前输入状态。"
  (pyim-indicator-daemon-stop)
  (setq pyim-indicator-timer
        (run-with-timer
         nil pyim-indicator-timer-repeat
         #'pyim-indicator-daemon-function func)))

(defun pyim-indicator-daemon-stop ()
  "Stop indicator daemon."
  (interactive)
  (when (timerp pyim-indicator-timer)
    (cancel-timer pyim-indicator-timer)))

(defun pyim-indicator-daemon-function (func)
  "`pyim-indicator-daemon' 内部使用的函数。"
  (let ((chinese-input-p
         (and (functionp func)
              (funcall func))))
    (funcall pyim-indicator chinese-input-p)))

(defun pyim-indicator-default (chinese-input-p)
  "Pyim 默认使用的 indicator, 主要通过光标颜色和 mode-line 来显示输入状态。"
  (if chinese-input-p
      (progn
        (setq current-input-method-title (nth 1 pyim-indicator-modeline-string))
        (set-cursor-color (nth 1 pyim-indicator-cursor-color)))
    (setq current-input-method-title (nth 0 pyim-indicator-modeline-string))
    (set-cursor-color (nth 0 pyim-indicator-cursor-color))))

;; * Footer
(provide 'pyim-indicator)

;;; pyim-indicator.el ends here
