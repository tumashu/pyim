;;; pyim-liberime.el --- Rime support for pyim.        -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/pyim
;; Version: 3.2
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

;; * pyim-liberime 使用说明                                          :README:doc:
;; 1. 安裝配置 liberime 和 pyim, 方式见：[[https://github.com/merrickluo/liberime][liberime]].
;; 2. 使用 rime 全拼输入法的用户，也可以使用 rime-quanpin scheme,
;;    这个 scheme 是专门针对 rime 全拼输入法定制的，支持全拼v快捷键。
;;    #+BEGIN_EXAMPLE
;;    (require 'pyim-liberime)
;;    (setq pyim-default-scheme 'rime-quanpin)
;;    #+END_EXAMPLE
;; 3. 如果通过 rime 使用微软双拼，可以用以下设置：
;;    #+BEGIN_EXAMPLE
;;    (liberime-select-schema "double_pinyin_mspy")
;;    (setq pyim-default-scheme 'rime-microsoft-shuangpin)
;;    #+END_EXAMPLE
;;    默认是用繁体中文，想要改成简体中文的话，可以参考 [[https://github.com/rime/home/wiki/CustomizationGuide#%E4%B8%80%E4%BE%8B%E5%AE%9A%E8%A3%BD%E7%B0%A1%E5%8C%96%E5%AD%97%E8%BC%B8%E5%87%BA][rime wiki]]，或者[[http://wenshanren.org/?p=1070#orgc7dbd8e][这篇博客]]

;;; Code:
;; * 代码                                                           :code:
(require 'pyim)
(require 'liberime nil t)

(pyim-scheme-add
 '(rime
   :document
   "rime 输入法。

这个 scheme 适用于 librime 支持的所有输入法，通用性较好，但无法支
持 trigger, 所以类似 pyim 全拼支持的v快捷键将无法使用。"
   :class rime
   :code-prefix "rime/"
   :code-prefix-history ("&")
   :first-chars "abcdefghijklmnopqrstuvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz'-a"
   :prefer-triggers nil))

(pyim-scheme-add
 '(rime-quanpin
   :document
   "rime 全拼输入法。

这个 scheme 专门用于 librime 全拼输入法，同时支持 trigger,
也就是 v 快捷键，使用 rime 全拼的朋友建议使用这个 scheme。"
   :class rime
   :code-prefix "rime/"
   :code-prefix-history ("&")
   :first-chars "abcdefghjklmnopqrstwxyz"
   :rest-chars "vmpfwckzyjqdltxuognbhsrei'-a"
   :prefer-triggers ("v")))

(pyim-scheme-add
 '(rime-microsoft-shuangpin
   :document "rime 微软双拼输入法。"
   :class rime
   :code-prefix "rime/"
   :code-prefix-history ("&")
   :first-chars "abcdefghijklmnopqrstuvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz;"
   :prefer-triggers nil))

(declare-function liberime-get-commit "liberime")
(declare-function liberime-get-context "liberime")
(declare-function liberime-clear-commit "liberime")
(declare-function liberime-clear-composition "liberime")
(declare-function liberime-search "liberime" (string limit))
(declare-function liberime-get-preedit "liberime")
(declare-function liberime-get-status "liberime")
(declare-function liberime-process-key "liberime" (keycode &optional mask))
(declare-function liberime-select-candidate "liberime" (num))

(defun pyim-liberime-scheme-name (orig_func &optional default)
  "Advice function of `pyim-scheme-name'."
  (let* ((scheme-name (funcall orig_func default))
         (class (pyim-scheme-get-option scheme-name :class)))
    (if (eq class 'rime)
        (if (featurep 'liberime-core)
            scheme-name
          'quanpin)
      scheme-name)))

(advice-add 'pyim-scheme-name :around #'pyim-liberime-scheme-name)

(defun pyim-imobjs-create:rime (entered &optional _)
  (list (list entered)))

(defun pyim-codes-create:rime (imobj scheme-name &optional first-n)
  (pyim-codes-create:xingma imobj scheme-name first-n))

(defun pyim-candidates-create:rime (imobjs scheme-name &optional async)
  "`pyim-candidates-create' 处理 rime 输入法的函数."
  (let* ((code (car (pyim-codes-create (car imobjs) scheme-name)))
         (code-prefix (pyim-scheme-get-option scheme-name :code-prefix))
         (s (replace-regexp-in-string "-" "" code))
         ;; `liberime-search' 搜索的时候不需要 code-prefix, 去除。
         (s (if code-prefix
                (string-remove-prefix code-prefix s)
              s))
         (words (liberime-search s (if async
                                       nil
                                     (* pyim-page-length 2)))))
    words))

(defun pyim-page-preview-create:rime (&optional separator)
  (let* ((preedit (or (liberime-get-preedit)
                      (pyim-entered-get 'point-before))))
    (pyim-process-with-entered-buffer
     (if (equal 1 (point))
         (concat "|" preedit)
       (concat (replace-regexp-in-string (concat separator "'") "'" preedit)
               " |" (buffer-substring-no-properties (point) (point-max)))))))

(defvar pyim-liberime-code-log nil)
(defvar pyim-liberime-word-log nil)
(defun pyim-select-word:rime ()
  "从选词框中选择当前词条，然后删除该词条对应拼音。"
  (interactive)
  (pyim-process-outcome-handle 'candidate)
  (let* ((entered (pyim-entered-get 'point-before))
         (word (string-remove-prefix
                (or (pyim-outcome-get 1) "") (pyim-outcome-get)))
         (code (pyim-liberime-get-code word entered))
         (to-be-translated
          (string-remove-prefix code entered)))
    (push code pyim-liberime-code-log)
    (push word pyim-liberime-word-log)
    (if (or (> (length to-be-translated) 0) ;是否有光标前未转换的字符串
            (> (length (pyim-entered-get 'point-after)) 0)) ;是否有光标后字符串
        (progn
          (pyim-process-with-entered-buffer
            (delete-region (point-min) (point))
            (insert to-be-translated)
            (goto-char (point-max)))
          (pyim-process-run))
      ;; 在 rime 后端造词和调整瓷瓶词频
      (pyim-liberime-create-word
       (reverse pyim-liberime-code-log)
       (reverse pyim-liberime-word-log))
      ;; 使用 rime 的同时，也附带的优化 quanpin 的词库。
      (let ((pyim-default-scheme 'quanpin))
        (if (member (pyim-outcome-get) pyim-candidates)
            (pyim-process-create-word (pyim-outcome-get) t)
          (pyim-process-create-word (pyim-outcome-get))))
      (setq pyim-liberime-code-log nil)
      (setq pyim-liberime-word-log nil)
      (pyim-process-terminate)
      ;; pyim 使用这个 hook 来处理联想词。
      (run-hooks 'pyim-select-finish-hook))))

(defun pyim-autoselector-rime (&rest _args)
  "适用于RIME的自动上屏器."
  (let* ((scheme-name (pyim-scheme-name))
         (class (pyim-scheme-get-option scheme-name :class)))
    (when (eq class 'rime)
      (let* ((commit (liberime-get-commit))
             (context (liberime-get-context))
             (composition (alist-get 'composition context))
             (length (alist-get 'length composition)))
        (cond
         ;; 有新输入的顶屏模式
         ((and commit (eq length 1))
          `(:select last :replace-with ,commit))
         ;; 无新输入的顶屏模式
         (commit
          `(:select current :replace-with ,commit))
         (t nil))))))

(add-to-list 'pyim-autoselector 'pyim-autoselector-rime)

(defun pyim-liberime-create-word (codes words)
  "通过 CODES 和 WORDS 的信息，在 rime 后端重新造词和调整词频。
比如：

1. CODES -> (\"nihao\" \"ma\")
2. WORDS -> (\"你好\" \"吗\")

在 rime 后端将生成 “你好吗” 这个词条。"
  (when (and (listp codes)
             (listp words)
             (not (cl-find-if-not #'stringp codes))
             (not (cl-find-if-not #'stringp words)))
    (liberime-clear-composition)
    (dolist (key (string-to-list (mapconcat #'identity codes "")))
      (liberime-process-key key))
    (let (word)
      (while (setq word (pop words))
        (let ((status t))
          (while status
            (let* ((context (liberime-get-context))
                   (menu (alist-get 'menu context))
                   (last-page-p (alist-get 'last-page-p menu))
                   (candidates (alist-get 'candidates menu))
                   (pos (cl-position word candidates :test #'equal)))
              (cond
               (pos (liberime-select-candidate pos)
                    (setq status nil))
               ((or last-page-p
                    (not menu))
                (setq status nil)
                (setq words nil))
               (t (liberime-process-key 65366))))))))))

(defun pyim-liberime-process-create-word (word &optional _prepend _wordcount-handler)
  "Create WORD at current rime backend.
ONlY works with quanpin."
  ;; 判断当前 rime 环境是否支持全拼，如果支持，就添加词条。
  (ignore-errors
    (let ((codes (pyim-cstring-to-codes word 'quanpin)))
      (when (member "你好" (liberime-search "nihao" 10))
        (dolist (code codes)
          (unless (pyim-string-match-p "[^ a-z-]" code)
            (pyim-liberime-create-word
             (split-string code "-")
             (remove "" (split-string word "")))
            (pyim-process-terminate:rime)))))))

(advice-add 'pyim-process-create-word :after #'pyim-liberime-process-create-word)

(defun pyim-liberime-get-code (word input &optional _limit)
  "Get the code of WORD from the beginning of INPUT.
`liberime-search' with LIMIT argument is used internal.

NOTE: This is a hacky approach, the better way is let librime
provide an API.

Please see: https://github.com/rime/librime/issues/349"
  (cond
   ;; 处理基于语音的输入法，比如：拼音，这类输入法 preedit 一般用空格
   ;; 分隔，与汉字一一对应。
   ((string-match-p
     (mapconcat #'identity
                '("pinyin" "luna" "terra" "bopomofo" "stenotype"
                  "jyut6ping3" "wugniu" "soutzoe" "zyenpheng"
                  "sampheng" "clover")
                "\\|")
     (alist-get 'schema_id (liberime-get-status)))
    (unless (liberime-get-preedit)
      (liberime-search input 1))
    (let* ((n (length word))
           (preedit (split-string (liberime-get-preedit) "[ ']+"))
           (preedit-list (cl-subseq preedit 0 (min n (length preedit))))
           (i (min (length input) (* n 5)))
           str)
      (while (> i 0)
        (setq str (substring input 0 i))
        (liberime-search str 1)
        (if (equal preedit-list (split-string (liberime-get-preedit) "[ ']+"))
            (setq i 0)
          (setq i (- i 1))))
      str))
   ((string-match-p
     (mapconcat #'identity
                '("wubi86" "wubi98")
                "\\|")
     (alist-get 'schema_id (liberime-get-status)))
    (let ((lst (split-string (liberime-get-preedit) "[ ']+"))
          (str "")
          words)
      (while lst
        (setq str (concat str (pop lst)))
        (setq words (liberime-search str 20))
        (when (member word words)
          (setq lst nil)))
      (or str input)))
   ;; 找不到通用的处理方式的话就不做截取处理。
   (t input)))

(defun pyim-process-terminate:rime ()
  (liberime-clear-commit)
  (liberime-clear-composition))

;; * Footer
(provide 'pyim-liberime)

;;; pyim-librime.el ends here
