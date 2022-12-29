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
(require 'pyim-candidates)

(defgroup pyim-cloudim nil
  "Cloud input method for pyim."
  :group 'pyim)

(defcustom pyim-cloudim nil
  "设置 pyim 使用的云输入法。"
  :type '(choice
          (const :tag "Do not use cloud input method." nil)
          (const :tag "Use baidu cloud input method." baidu)
          (const :tag "Use google cloud input method." google)))

(cl-defmethod pyim-candidates-create-async
  (imobjs (scheme pyim-scheme-quanpin) callback
          &context (pyim-cloudim (eql baidu)))
  "按照 SCHEME, 使用异步的方式从 IMOBJS 获得候选词条，用于全拼输入法。
这里使用 baidu 提供的云输入法接口获取词条。"
  (let ((str (string-join (pyim-codes-create (car imobjs) scheme))))
    (unless (< (length str) 1)
      (url-retrieve
       (format "https://olime.baidu.com/py?py=%s" str)
       (lambda (_)
         (funcall callback (cons imobjs (pyim-cloudim--parse-baidu-buffer)))
         (kill-buffer))
       nil t))))

(defun pyim-cloudim--parse-baidu-buffer ()
  "解析 `pyim-cloudim-url-retrieve-sync' 返回的 baidu buffer."
  ;; NOTE: 以前这个函数使用 `json-parse-buffer' 来处理返回的结果，但因为旧版本
  ;; Emacs 没有 `json-parse-buffer' 函数，所以现在改用这种简单粗暴的方式，虽然没
  ;; 有使用 json 得到的结果精确，但应该适用于大多数情况，同时也减少了一个包依赖。
  (let ((words (pyim-cloudim--parse-baidu-buffer-string (buffer-string))))
    (when (> (length words) 0)
      (mapcar (lambda (word)
                (propertize word :comment "(云)"))
              words))))

(defun pyim-cloudim--parse-baidu-buffer-string (string)
  "从 baidu buffer STRING 中抓取中文词条，返回对应的词条列表."
  (let ((string (decode-coding-string string 'utf-8))
        (sep "丨"))
    (cl-remove-if-not
     (lambda (x)
       (> (length x) 0))
     (split-string
      (replace-regexp-in-string
       "\\CC" ""
       (replace-regexp-in-string "," sep string))
      (format "[%s]+" sep)))))

(cl-defmethod pyim-candidates-create-async
  (imobjs (scheme pyim-scheme-quanpin) callback
          &context (pyim-cloudim (eql google)))
  "按照 SCHEME, 使用异步的方式从 IMOBJS 获得候选词条，用于全拼输入法。
这里使用 google 提供的云输入法接口获取词条。"
  (let ((str (string-join (pyim-codes-create (car imobjs) scheme))))
    (unless (< (length str) 1)
      (url-retrieve
       (format "https://www.google.cn/inputtools/request?ime=pinyin&text=%s" str)
       (lambda (_)
         (funcall callback (cons imobjs (pyim-cloudim--parse-google-buffer)))
         (kill-buffer))
       nil t))))

(defun pyim-cloudim--parse-google-buffer ()
  "解析 `pyim-cloudim-url-retrieve-sync' 返回的 google buffer."
  (pyim-cloudim--parse-baidu-buffer))

;; * Footer
(provide 'pyim-cloudim)

;;; pyim-cloudim.el ends here
