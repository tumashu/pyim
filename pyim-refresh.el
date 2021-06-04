;;; pyim-refresh.el --- Refresh function of pyim.        -*- lexical-binding: t; -*-

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

(defvar pyim-refresh-timer nil
  "异步处理 intered 时时，使用的 timer.")

(defun pyim-refresh (&optional no-delay)
  "延迟 `pyim-entered-exhibit-delay-ms' 显示备选词等待用户选择。"
  (if (= (length (pyim-entered-get 'point-before)) 0)
      (pyim-refresh-terminate)
    (when pyim-entered--exhibit-timer
      (cancel-timer pyim-entered--exhibit-timer))
    (cond
     ((or no-delay
          (not pyim-entered-exhibit-delay-ms)
          (eq pyim-entered-exhibit-delay-ms 0))
      (pyim-refresh-1))
     (t (setq pyim-entered--exhibit-timer
              (run-with-timer (/ pyim-entered-exhibit-delay-ms 1000.0)
                              nil
                              #'pyim-refresh-1))))))

;; 没有这一行，native-compilation 会出现奇怪的问题，pyim-outcome-handle 会获取到
;; 错误的 pyim-candidates 取值。原因未知。
(defvar pyim-candidates)

(defun pyim-refresh-1 ()
  "查询 `pyim-entered-buffer' 光标前的拼音字符串（如果光标在行首则为光标后的）, 显示备选词等待用户选择。"
  (let* ((scheme-name (pyim-scheme-name))
         entered-to-translate)
    (setq entered-to-translate
          (pyim-entered-get 'point-before))
    (setq pyim-imobjs (pyim-imobjs-create entered-to-translate scheme-name))
    (setq pyim-candidates
          (or (delete-dups (pyim-candidates-create pyim-imobjs scheme-name))
              (list entered-to-translate)))
    (pyim-refresh-timer-reset)
    ;; 延迟1秒异步处理 entered, pyim 内置的输入法目前不使用异步获取
    ;; 词条的方式，主要用于 pyim-liberime 支持。
    (setq pyim-refresh-timer
          (run-with-timer
           1 nil
           (lambda ()
             (if (functionp 'make-thread)
                 (make-thread #'pyim-refresh-with-thread
                              "pyim-refresh-with-thread")
               (pyim-refresh-with-thread)))))
    ;; 自动上屏功能
    (let ((autoselector-results
           (mapcar (lambda (x)
                     (when (functionp x)
                       (ignore-errors
                         (funcall x))))
                   (cl-remove-duplicates pyim-autoselector :from-end t)))
          result)
      (cond
       ;; 假如用户输入 "nihao", 然后按了 "m" 键, 那么当前的 entered
       ;; 就是"nihaom", 如果 autoselector 返回 list: (:select last),
       ;; 那么，“nihao” 对应的第一个候选词将上屏，m键下一轮继续处理。
       ;; 这是一种 "踩雷确认模式".
       ((and
         ;; autoselector 功能会影响手动连续选择功能，所以这里做了一些限制，
         ;; 只有在输入的时候才能够触发 autoselector 机制。
         (eq this-command 'pyim-self-insert-command)
         (cl-find-if (lambda (x)
                       (setq result x)
                       (equal (plist-get x :select) 'last))
                     autoselector-results))
        (let* ((str (plist-get result :replace-with))
               (pyim-candidates
                (if (and str (stringp str))
                    (list str)
                  pyim-candidates-last)))
          (pyim-outcome-handle 'candidate))
        ;; autoselector 机制已经触发的时候，如果发现 entered buffer 中
        ;; point 后面还有未处理的输入，就将其转到下一轮处理，这种情况
        ;; 很少出现，一般是型码输入法，entered 编辑的时候有可能触发。
        (pyim-add-unread-command-events
         (listify-key-sequence (pyim-entered-get 'point-after)))
        (pyim-add-unread-command-events last-command-event)
        (pyim-refresh-terminate))
       ;; 假设用户已经输入 "niha", 然后按了 "o" 键，那么，当前
       ;; entered 就是 "nihao". 如果 autoselector 函数返回一个 list:
       ;; (:select current), 那么就直接将 "nihao" 对应的第一个候选词
       ;; 上屏幕。
       ((and (eq this-command 'pyim-self-insert-command)
             (cl-find-if (lambda (x)
                           (setq result x)
                           (equal (plist-get x :select) 'current))
                         autoselector-results))
        (let* ((str (plist-get result :replace-with))
               (pyim-candidates
                (if (and str (stringp str))
                    (list str)
                  pyim-candidates)))
          (pyim-outcome-handle 'candidate))
        (pyim-add-unread-command-events
         (listify-key-sequence (pyim-entered-get 'point-after)))
        (pyim-refresh-terminate))
       (t (setq pyim-candidate-position 1)
          (pyim-preview-refresh)
          (pyim-page-refresh))))))

(defun pyim-refresh-with-thread ()
  "Function used by `pyim-refresh-timer'"
  (let* ((scheme-name (pyim-scheme-name))
         (words (delete-dups (pyim-candidates-create pyim-imobjs scheme-name t))))
    (when words
      (setq pyim-candidates words)
      (pyim-preview-refresh)
      ;; NEED HELP: 目前只有 posframe 和 minibufer 可以正确处理异步刷新 page
      (when (and (member pyim-page-tooltip '(posframe minibuffer))
                 (not (eq (selected-window) (minibuffer-window))))
        (pyim-page-refresh)))))

(defun pyim-refresh-timer-reset ()
  "Reset `pyim-refresh-timer'."
  (when pyim-refresh-timer
    (cancel-timer pyim-refresh-timer)
    (setq pyim-refresh-timer nil)))

(defalias 'pyim-terminate-translation #'pyim-refresh-terminate)
(defun pyim-refresh-terminate ()
  "Terminate the translation of the current key."
  (setq pyim-translating nil)
  (pyim-preview-delete-string)
  (setq pyim-candidates nil)
  (setq pyim-candidates-last nil)
  (setq pyim-force-input-chinese nil)
  (pyim-page-hide)
  (pyim-entered-erase-buffer)
  (setq pyim-cstring-to-code-criteria nil)
  (pyim-refresh-timer-reset)
  (let* ((class (pyim-scheme-get-option (pyim-scheme-name) :class))
         (func (intern (format "pyim-refresh-terminate:%S" class)))
         ;; `pyim-refresh-terminate' 以前叫 pyim-terminate-translation, 兼容以前的名称。
         (func-old (intern (format "pyim-terminate-translation:%S" class))))
    (cond ((and class (functionp func))
           (funcall func))
          ((and class (functionp func-old))
           (funcall func))
          (t nil))))

;; * Footer
(provide 'pyim-refresh)

;;; pyim-refresh.el ends here
