;;; pyim-entered.el --- page lib for pyim.        -*- lexical-binding: t; -*-

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

(defgroup pyim-entered nil
  "Entered tools for pyim."
  :group 'pyim)

(define-obsolete-variable-alias 'pyim-exhibit-delay-ms 'pyim-entered-exhibit-delay-ms "")
(defcustom pyim-entered-exhibit-delay-ms 0
  "输入或者删除拼音字符后等待多少毫秒后才显示可选词
当用户快速输入连续的拼音时可提升用户体验.
如果为 0 或者 nil, 则不等待立刻显示可选词."
  :type 'integer)

(defvar pyim-entered--exhibit-timer nil)

(defvar pyim-entered-refresh-timer nil
  "异步处理 intered 时时，使用的 timer.")

(defvar pyim-entered-buffer " *pyim-entered-buffer*"
  "一个 buffer，用来处理用户已经输入的字符串： entered。

用户 *已经* 输入的字符组成的字符串，在 pyim 里面，叫做 entered,
说白了就是 input, 选择 entered 而不选择 input 的原因是：

1. input 太常见了， 和其它词语组和起来容易产生歧义，比如：
   pyim-entered-output 就比 pyim-input-output 更加容易理解。
2. entered 这个词语很少见，只要明白它代表的概念，就不容易产生混乱。

pyim 使用一个 buffer 来处理 entered, 以实现 “用户输入字符串” 编
辑等高级功能：

1. 如果输入的字符串有错误，可以修改，不用取消重新输入；
2. 如果光标不在行首，pyim 只使用光标前的字符串来查找词条，
   如果词条上屏，词条对应的输入就从 buffer 中清除，然后
   继续处理后面的输入，这种方式方便长词的输入；
3. 如果光标在行首，则处理整行。")

(defvar pyim-entered-longest nil
  "记录用户在连续选词之前的最长输入，用于全拼输入法多音字矫正。")

(defmacro pyim-with-entered-buffer (&rest forms)
  (declare (indent 0) (debug t))
  `(with-current-buffer (get-buffer-create pyim-entered-buffer)
     ,@forms))

(defun pyim-entered-get (&optional type)
  "从 `pyim-entered-buffer' 中获取拼音字符串.

默认返回 entered buffer 中的全部字符串。如果 TYPE 取值为
point-before, 返回 entered buffer 中 point 之前的字符串，如果
TYPE 取值为 point-after, 返回 entered buffer 中 point 之后的字符
串。"
  (pyim-with-entered-buffer
    (cond
     ((bobp) (buffer-string))
     ((eq type 'point-before)
      (buffer-substring-no-properties (point-min) (point)))
     ((eq type 'point-after)
      (buffer-substring-no-properties (point) (point-max)))
     (t (buffer-string)))))

(defun pyim-entered-erase-buffer ()
  "清除 `pyim-entered-buffer' 的内容"
  (pyim-with-entered-buffer
    (erase-buffer)))

;; pyim-entered-buffer 中进行光标移动的函数
;; point move function in `pyim-entered-buffer'
(defun pyim-entered-forward-point ()
  "`pyim-entered-buffer' 中光标前移"
  (interactive)
  (pyim-with-entered-buffer
    (ignore-errors
      (forward-char)))
  (pyim-entered-refresh t))

(defun pyim-entered-backward-point ()
  "`pyim-entered-buffer' 中光标后移"
  (interactive)
  (pyim-with-entered-buffer
    (ignore-errors
      (backward-char)))
  (pyim-entered-refresh t))

(defun pyim-entered-backward-imelem (&optional search-forward)
  "`pyim-entered-buffer’ 中光标向后移动一个 imelem 对应的字符串

在全拼输入法中，就是向前移动一个拼音"
  (interactive)
  (let* ((position (pyim-entered-next-imelem-position 1 search-forward)))
    (pyim-with-entered-buffer
      (goto-char position))
    (pyim-entered-refresh t)))

(defun pyim-entered-forward-imelem ()
  "`pyim-entered-buffer’ 中光标向前移动一个 imelem 对应的字符串"
  (interactive)
  (pyim-entered-backward-imelem t))

(defun pyim-entered-end-of-line ()
  "`pyim-entered-buffer' 中光标移至行尾"
  (interactive)
  (pyim-with-entered-buffer
    (end-of-line))
  (pyim-entered-refresh t))

(defun pyim-entered-beginning-of-line ()
  "`pyim-entered-buffer' 中光标移至行首"
  (interactive)
  (pyim-with-entered-buffer
    (beginning-of-line))
  (pyim-entered-refresh t))

(defun pyim-entered-next-imelem-position (num &optional search-forward start)
  "从 `pyim-entered-buffer' 的当前位置，找到下一个或者下 NUM 个 imelem 对应的位置

如果 SEARCH-FORWARD 为 t, 则向前搜索，反之，向后搜索。"
  (pyim-with-entered-buffer
    (let* ((scheme-name (pyim-scheme-name))
           (start (or start (point)))
           (end-position start)
           (string (buffer-substring-no-properties (point-min) start))
           (orig-imobj-len (length (car (pyim-imobjs-create string scheme-name))))
           imobj pos)
      (if search-forward
          ;; "ni|haoshijie" -> "nihao|shijie"
          (progn
            (setq pos (point-max))
            (while (and (> pos start) (= end-position start))
              (setq string (buffer-substring-no-properties (point-min) pos)
                    imobj (car (pyim-imobjs-create string scheme-name)))
              (if (>= (+ orig-imobj-len num) (length imobj))
                  (setq end-position pos)
                (cl-decf pos))))
        ;; "nihao|shijie" -> "ni|haoshijie"
        (if (<= orig-imobj-len num)
            (setq end-position (point-min))
          (setq pos start)
          (while (and (>= pos (point-min)) (= end-position start))
            (setq string (buffer-substring-no-properties (point-min) pos)
                  imobj (car (pyim-imobjs-create string scheme-name)))
            (if (= (- orig-imobj-len num) (length imobj))
                (setq end-position pos)
              (cl-decf pos)))))
      end-position)))

(declare-function pyim-terminate-translation "pyim")
(declare-function pyim-convert-string-at-point "pyim")

(defun pyim-entered-refresh (&optional no-delay)
  "延迟 `pyim-entered-exhibit-delay-ms' 显示备选词等待用户选择。"
  (if (= (length (pyim-entered-get 'point-before)) 0)
      (pyim-terminate-translation)
    (when pyim-entered--exhibit-timer
      (cancel-timer pyim-entered--exhibit-timer))
    (cond
     ((or no-delay
          (not pyim-entered-exhibit-delay-ms)
          (eq pyim-entered-exhibit-delay-ms 0))
      (pyim-entered-refresh-1))
     (t (setq pyim-entered--exhibit-timer
              (run-with-timer (/ pyim-entered-exhibit-delay-ms 1000.0)
                              nil
                              #'pyim-entered-refresh-1))))))

;; 没有这一行，native-compilation 会出现奇怪的问题，pyim-outcome-handle 会获取到
;; 错误的 pyim-candidates 取值。原因未知。
(defvar pyim-candidates)

(defun pyim-entered-refresh-1 ()
  "查询 `pyim-entered-buffer' 光标前的拼音字符串（如果光标在行首则为光标后的）, 显示备选词等待用户选择。"
  (let* ((scheme-name (pyim-scheme-name))
         entered-to-translate)
    (setq entered-to-translate
          (pyim-entered-get 'point-before))
    (setq pyim-imobjs (pyim-imobjs-create entered-to-translate scheme-name))
    (setq pyim-candidates
          (or (delete-dups (pyim-candidates-create pyim-imobjs scheme-name))
              (list entered-to-translate)))
    (pyim-entered-refresh-timer-reset)
    ;; 延迟1秒异步处理 entered, pyim 内置的输入法目前不使用异步获取
    ;; 词条的方式，主要用于 pyim-liberime 支持。
    (setq pyim-entered-refresh-timer
          (run-with-timer
           1 nil
           (lambda ()
             (if (functionp 'make-thread)
                 (make-thread #'pyim-entered-refresh-with-thread
                              "pyim-entered-refresh-with-thread")
               (pyim-entered-refresh-with-thread)))))
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
        (setq unread-command-events
              (append (listify-key-sequence (pyim-entered-get 'point-after))
                      unread-command-events))
        (push last-command-event unread-command-events)
        (pyim-terminate-translation))
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
        (setq unread-command-events
              (append (listify-key-sequence (pyim-entered-get 'point-after))
                      unread-command-events))
        (pyim-terminate-translation))
       (t (setq pyim-candidate-position 1)
          (pyim-preview-refresh)
          (pyim-page-refresh))))))

(defun pyim-entered-refresh-with-thread ()
  "Function used by `pyim-entered-refresh-timer'"
  (let* ((scheme-name (pyim-scheme-name))
         (words (delete-dups (pyim-candidates-create pyim-imobjs scheme-name t))))
    (when words
      (setq pyim-candidates words)
      (pyim-preview-refresh)
      ;; NEED HELP: 目前只有 posframe 和 minibufer 可以正确处理异步刷新 page
      (when (and (member pyim-page-tooltip '(posframe minibuffer))
                 (not (eq (selected-window) (minibuffer-window))))
        (pyim-page-refresh)))))

(defun pyim-entered-refresh-timer-reset ()
  "Reset `pyim-entered-refresh-timer'."
  (when pyim-entered-refresh-timer
    (cancel-timer pyim-entered-refresh-timer)
    (setq pyim-entered-refresh-timer nil)))

;; ** 与拼音输入相关的用户命令
(defun pyim-entered-delete-backward-char (&optional n)
  "在pyim-entered-buffer中向后删除1个字符"
  (interactive)
  (pyim-with-entered-buffer
    (ignore-errors
      (delete-char (- 0 (or n 1)))))
  (if (> (length (pyim-entered-get 'point-before)) 0)
      (pyim-entered-refresh t)
    (pyim-outcome-handle "")
    (pyim-terminate-translation)))

(defun pyim-entered-delete-forward-char ()
  "在pyim-entered-buffer中向前删除1个字符"
  (interactive)
  (pyim-entered-delete-backward-char -1))

(defun pyim-entered-delete-backward-imelem (&optional search-forward)
  "`pyim-entered-buffer’ 中向后删除一个 imelem 对应的字符串"
  (interactive)
  (let ((position (pyim-entered-next-imelem-position 1 search-forward)))
    (pyim-with-entered-buffer
      (delete-region (point) position))
    (pyim-entered-refresh t)))

(defun pyim-entered-delete-forward-imelem ()
  "`pyim-entered-buffer’ 中向前删除一个 imelem 对应的字符串"
  (interactive)
  (pyim-entered-delete-backward-imelem t))

;; * Footer
(provide 'pyim-entered)

;;; pyim-entered.el ends here
