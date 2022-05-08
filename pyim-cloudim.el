;;; pyim-cloudim.el --- cloud input method support for pyim.        -*- lexical-binding: t; -*-

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
(require 'url)
(require 'json)

(defgroup pyim-cloudim nil
  "Cloud input method for pyim."
  :group 'pyim)

(defcustom pyim-cloudim nil
  "设置 pyim 使用的云输入法。"
  :type '(choice
          (const :tag "Do not use cloud input method." nil)
          (const :tag "Use baidu cloud input method." baidu)
          (const :tag "Use google cloud input method." google)))

(defun pyim-cloudim (string input-method)
  "使用云 INPUT-METHOD 输入法引擎搜索 STRING 获取词条列表.
云输入法由 `pyim-cloudim' 设置。"
  (when (and pyim-cloudim (symbolp pyim-cloudim))
    (let ((func (intern (format "pyim-cloudim:%s" pyim-cloudim))))
      (when (functionp func)
        (funcall func string input-method)))))

(setq pyim-candidates-cloud-search-function #'pyim-cloudim)

(defun pyim-cloudim:baidu (string input-method)
  "使用 baidu 云 INPUT-METHOD 输入法引擎搜索 STRING, 获取词条列表。"
  (when (and (equal input-method 'pinyin)
             (functionp 'json-parse-buffer))
    (with-current-buffer (url-retrieve-synchronously
                          (format "https://olime.baidu.com/py?py=%s" string)
                          t nil 0.5)
      (pyim-cloudim-parse-baidu-buffer))))

(defun pyim-cloudim-parse-baidu-buffer ()
  "解析 `url-retrieve-synchronously' 返回的 baidu buffer."
  (goto-char (point-min))
  (search-forward "\n\n" nil t)
  (delete-region (point-min) (point))
  (let ((data (funcall 'json-parse-buffer)))
    (list (elt (elt (elt (gethash "0" data) 0) 0) 0))))

(defun pyim-cloudim:google (string input-method)
  "使用 google 云 INPUT-METHOD 输入法引擎搜索 STRING, 获取词条列表。"
  (when (and (eq input-method 'pinyin)
             (functionp 'json-parse-buffer))
    (with-current-buffer (url-retrieve-synchronously
                          (format "https://www.google.cn/inputtools/request?ime=pinyin&text=%s" string)
                          t nil 0.5)
      (pyim-cloudim-parse-google-buffer))))

(defun pyim-cloudim-parse-google-buffer ()
  "解析 `url-retrieve-synchronously' 返回的 google buffer."
  (goto-char (point-min))
  (search-forward "\n\n" nil t)
  (delete-region (point-min) (point))
  (let ((data (funcall 'json-parse-buffer)))
    (list (elt (elt (elt (elt data 1) 0) 1) 0))))

;; * Footer
(provide 'pyim-cloudim)

;;; pyim-cloudim.el ends here
