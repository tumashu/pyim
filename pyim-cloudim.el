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

(defun pyim-cloudim (string scheme-name)
  "使用云输入法引擎搜索 STRING 获取词条列表.
云输入法由 `pyim-cloudim' 设置。"
  (when (and pyim-cloudim (symbolp pyim-cloudim))
    (let ((func (intern (format "pyim-cloudim:%s" pyim-cloudim))))
      (when (functionp func)
        (funcall func string scheme-name)))))

(setq pyim-candidates-cloud-search-function #'pyim-cloudim)

(defun pyim-cloudim:baidu (string scheme-name)
  "使用 baidu 云输入法引擎搜索 STRING, 获取词条列表。"
  (when (equal scheme-name 'quanpin)
    (let ((buffer (pyim-cloudim-url-retrieve-sync
                   (format "https://olime.baidu.com/py?py=%s" string)
                   t nil 0.2)))
      (when (bufferp buffer)
        (with-current-buffer buffer
          (pyim-cloudim-parse-baidu-buffer))))))

(defun pyim-cloudim-url-retrieve-sync (url &optional silent inhibit-cookies timeout)
  "Pyim 版本的 `url-retrieve-synchronously'.

只有在用户输入停顿的时候，才能运行这个函数，用户如果再次输入，这
个函数马上停止执行。"
  (when (not (input-pending-p)) ;只有在用户输入停顿的时候才搜索 buffer.
    (url-do-setup)
    (let* (url-asynchronous
           data-buffer
           (callback (lambda (&rest _args)
                       (setq data-buffer (current-buffer))
                       (url-debug 'retrieval
                                  "Synchronous fetching done (%S)"
                                  data-buffer)))
           (start-time (current-time))
           (proc-buffer (url-retrieve url callback nil silent
                                      inhibit-cookies)))
      (if (not proc-buffer)
          (url-debug 'retrieval "Synchronous fetching unnecessary %s" url)
        (unwind-protect
            (catch 'done
              (while (and (not (input-pending-p)) ;如果用户继续输入，就停止云搜索。
                          (not data-buffer))
                (when (and timeout (time-less-p timeout
                                                (time-since start-time)))
                  (url-debug 'retrieval "Timed out %s (after %ss)" url
                             (float-time (time-since start-time)))
                  (throw 'done 'timeout))
	            (url-debug 'retrieval
		                   "Spinning in pyim-cloudim-url-retrieve-sync: nil (%S)"
		                   proc-buffer)
                (when-let ((redirect-buffer
                            (buffer-local-value 'url-redirect-buffer
                                                proc-buffer)))
                  (unless (eq redirect-buffer proc-buffer)
                    (url-debug
                     'retrieval "Redirect in pyim-cloudim-url-retrieve-sync: %S -> %S"
		             proc-buffer redirect-buffer)
                    (let (kill-buffer-query-functions)
                      (kill-buffer proc-buffer))
                    ;; Accommodate hack in commit 55d1d8b.
                    (setq proc-buffer redirect-buffer)))
                (when-let ((proc (get-buffer-process proc-buffer)))
                  (when (memq (process-status proc)
                              '(closed exit signal failed))
                    ;; Process sentinel vagaries occasionally cause
                    ;; url-retrieve to fail calling callback.
                    (unless data-buffer
                      (url-debug 'retrieval "Dead process %s" url)
		              (throw 'done 'exception))))
                ;; Querying over consumer internet in the US takes 100
                ;; ms, so split the difference.
                (accept-process-output nil 0.05)))
          (unless (eq data-buffer proc-buffer)
            (let (kill-buffer-query-functions)
              (kill-buffer proc-buffer)))))
      data-buffer)))

(defun pyim-cloudim-parse-baidu-buffer ()
  "解析 `pyim-cloudim-url-retrieve-sync' 返回的 baidu buffer."
  ;; NOTE: 以前这个函数使用 `json-parse-buffer' 来处理返回的结果，但因为旧版本
  ;; Emacs 没有 `json-parse-buffer' 函数，所以现在改用这种简单粗暴的方式，虽然没
  ;; 有使用 json 得到的结果精确，但应该适用于大多数情况，同时也减少了一个包依赖。
  (let ((word (replace-regexp-in-string
               "\\CC" ""
               (decode-coding-string
                (buffer-string)
                'utf-8))))
    (kill-buffer)
    (when (> (length word) 0)
      (list word))))

(defun pyim-cloudim:google (string scheme-name)
  "使用 google 云输入法引擎搜索 STRING, 获取词条列表。"
  (when (eq scheme-name 'quanpin)
    (let ((buffer (pyim-cloudim-url-retrieve-sync
                   (format "https://www.google.cn/inputtools/request?ime=pinyin&text=%s" string)
                   t nil 0.2)))
      (when (bufferp buffer)
        (with-current-buffer buffer
          (pyim-cloudim-parse-google-buffer))))))

(defun pyim-cloudim-parse-google-buffer ()
  "解析 `pyim-cloudim-url-retrieve-sync' 返回的 google buffer."
  (pyim-cloudim-parse-baidu-buffer))

;; * Footer
(provide 'pyim-cloudim)

;;; pyim-cloudim.el ends here
