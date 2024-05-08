;;; pyim.el --- A Chinese input method support quanpin, shuangpin, wubi, cangjie and rime.        -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2015-2021 Free Software Foundation, Inc.

;; Author: Ye Wenbin <wenbinye@163.com>
;;         Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/pyim
;; Version: 5.3.4
;; Keywords: convenience, Chinese, pinyin, input-method
;; Package-Requires: ((emacs "27.1") (async "1.6") (xr "1.13"))

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

;; * 核心代码                                                           :code:
;; ** require + defcustom + defvar
(require 'cl-lib)
(require 'help-mode)
(require 'pyim-autoselector)
(require 'pyim-common)
(require 'pyim-cstring)
(require 'pyim-dhashcache)
(require 'pyim-indicator)
(require 'pyim-page)
(require 'pyim-preview)
(require 'pyim-process)
(require 'pyim-scheme)
(require 'subr-x)

(defgroup pyim nil
  "Pyim is a Chinese input method support quanpin, shuangpin, wubi and cangjie."
  :group 'leim)

(defcustom pyim-select-word-by-number t
  "使用数字键来选择词条.

如果设置为 nil, 将直接输入数字，适用于使用数字做为
编码的输入法。"
  :type 'boolean)

;;;###autoload
(defvar pyim-title "PYIM ")

(defvar pyim-load-hook nil)
(defvar pyim-activate-hook nil)
(defvar pyim-deactivate-hook nil)

(defvar pyim-mode-map
  (let ((map (make-sparse-keymap))
        (i ?\ ))
    (while (< i 127)
      (define-key map (char-to-string i) #'pyim-self-insert-command)
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 256)
      (define-key map (vector i) #'pyim-self-insert-command)
      (setq i (1+ i)))
    (dolist (i (number-sequence 0 9))
      (define-key map (kbd (number-to-string i))
                  (lambda ()
                    (interactive)
                    (pyim-select-word-by-number i))))
    (dolist (x '(("<f1>" . 1)
                 ("<f2>" . 2)
                 ("<f3>" . 3)
                 ("<f4>" . 4)))
      (define-key map (kbd (car x))
                  (lambda ()
                    (interactive)
                    (pyim-select-char-in-word-by-number (cdr x)))))
    (define-key map " " #'pyim-select-word)
    (define-key map (kbd "C-SPC") #'pyim-select-word-simple)
    (define-key map [backspace] #'pyim-delete-backward-char)
    (define-key map [delete] #'pyim-delete-forward-char)
    (define-key map "\C-d" #'pyim-delete-forward-char)
    (define-key map [M-backspace] #'pyim-delete-backward-imelem)
    (define-key map [M-delete] #'pyim-delete-forward-imelem)
    (define-key map [C-backspace] #'pyim-delete-backward-imelem)
    (define-key map [C-delete] #'pyim-delete-forward-imelem)
    (define-key map [?\t]      #'pyim-toggle-assistant-scheme)
    (define-key map (kbd "TAB") #'pyim-toggle-assistant-scheme)
    (define-key map "\177" #'pyim-delete-backward-char)
    (define-key map "\C-f" #'pyim-forward-point)
    (define-key map "\C-b" #'pyim-backward-point)
    (define-key map "\M-f" #'pyim-forward-imelem)
    (define-key map "\M-b" #'pyim-backward-imelem)
    (define-key map "\C-e" #'pyim-end-of-line)
    (define-key map "\C-a" #'pyim-beginning-of-line)
    (define-key map "=" #'pyim-next-page)
    (define-key map "-" #'pyim-previous-page)
    (define-key map "\C-n" #'pyim-next-word)
    (define-key map "\C-p" #'pyim-previous-word)
    (define-key map "\M-n" #'pyim-next-page)
    (define-key map "\M-p" #'pyim-previous-page)
    (define-key map "\C-m" #'pyim-quit-no-clear)
    (define-key map [return] #'pyim-quit-no-clear)
    (define-key map "\C-c" #'pyim-quit-clear)
    (define-key map "\C-g" #'pyim-quit-clear)
    map)
  "Pyim 的 Keymap.")

(cl-defmethod pyim-process-get-mode-map ()
  pyim-mode-map)

;; ** pyim 输入法定义
(defun pyim-input-method (key)
  "得到需要插入到 buffer 的字符串, 并将其插入到待输入 buffer.

这个函数会处理用户输入的字符，并最终的得到需要插入 buffer 的字符
串。这个字符串会被分解为 event list, 通过 emacs 低层函数
`read-event' 来将这些 list 插入到 *待输入buffer*。"
  (if (or
       ;; NOTE: 在 widget 输入框存在的情况下，即使 buffer 是只读的，widget 输入
       ;; 框也有可能要输入文本，EWW 就存在类似情况。
       ;;
       ;; buffer-read-only
       overriding-terminal-local-map
       overriding-local-map)
      (list key)
    ;; (message "call with key: %S" key-or-string)
    (with-silent-modifications
      (unwind-protect
          (let ((input-string (pyim-process-input-method key)))
            ;; (message "input-string: %s" input-string)
            (when (and (stringp input-string)
                       (> (length input-string) 0))
              (if input-method-exit-on-first-char
                  (list (aref input-string 0))
                (mapcar #'identity input-string))))
        (pyim-process-terminate)))))

;; ** Pyim 输入法注册
;;;###autoload
(register-input-method "pyim" "UTF-8" #'pyim-activate pyim-title "")

;; ** PYim 输入法启动功能
;;;###autoload
(defun pyim-activate (&optional _args)
  "pyim 启动函数.

pyim 是使用 `pyim-activate' 来启动输入法，这个命令主要做如下工作：
1. 重置所有的 local 变量。
2. 创建汉字到拼音和拼音到汉字的 hash table。
3. 创建词库缓存 dcache.
4. 运行 hook： `pyim-load-hook'。
5. 将 `pyim--kill-emacs-hook-function' 命令添加到 `kill-emacs-hook' , emacs 关闭
之前将用户选择过的词生成的缓存和词频缓存保存到文件，供以后使用。
6. 设定变量：
   1. `input-method-function'
   2. `deactivate-current-input-method-function'
7. 运行 `pyim-activate-hook'

pyim 使用函数 `pyim-activate' 启动输入法的时候，会将变量
`input-method-function' 设置为 `pyim-input-method' ，这个变量会影
响 `read-event' 的行为。

当输入字符时，`read-event' 会被调用，`read-event' 调用的过程中，
会执行 `pyim-input-method' 这个函数。"
  (interactive)

  ;; 启动 pyim 需要的 daemon
  (pyim-process-start-daemon)

  ;; 初始化 dcache.
  (pyim-process-init-dcaches)

  ;; 启动或者重启的时候，退出辅助输入法。
  (pyim-scheme-disable-assistant)

  (run-hooks 'pyim-load-hook)
  ;; Make sure personal or other dcache are saved to file before kill emacs.
  (add-hook 'kill-emacs-hook #'pyim--kill-emacs-hook-function)

  (setq deactivate-current-input-method-function #'pyim-deactivate)
  ;; If we are in minibuffer, turn off the current input method
  ;; before exiting.
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook #'pyim-exit-from-minibuffer))
  (run-hooks 'pyim-activate-hook)
  (setq-local input-method-function #'pyim-input-method)
  nil)

(defun pyim--kill-emacs-hook-function ()
  "Pyim function which is used in `kill-emacs-hook'."
  (pyim-process-save-dcaches t)
  t)

;; ** 取消激活功能
(defun pyim-deactivate ()
  "取消 pyim 的激活状态."
  (interactive)
  (pyim-kill-local-variables)
  (kill-local-variable 'input-method-function)
  (pyim-process-stop-daemon)
  (run-hooks 'pyim-deactivate-hook))

;; ** pyim 从 minibuffer 退出功能
(defun pyim-exit-from-minibuffer ()
  "Pyim 从 minibuffer 退出."
  (deactivate-input-method)
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'pyim-exit-from-minibuffer)))

;; ** pyim 重启功能
(defun pyim-restart ()
  "重启 pyim，不建议用于编程环境.

这个函数用于重启 pyim，其过程和 `pyim-activate' 类似，只是在输入法重
启之前，询问用户，是否保存个人词频信息。"
  (interactive)
  (let ((save-personal-dcache
         (yes-or-no-p "重启 pyim 前，需要保存个人词频信息吗？ ")))
    (pyim-restart-1 save-personal-dcache)))

(defun pyim-restart-1 (&optional save-personal-dcache _refresh-common-dcache)
  "重启 pyim，用于编程环境.

当 SAVE-PERSONAL-DCACHE 是 non-nil 时，保存个人词库文件。

REFRESH-COMMON-DCACHE 已经废弃，不要再使用了。"
  (pyim-process-save-dcaches save-personal-dcache)
  (pyim-process-init-dcaches :force))

;; ** 升级功能
(defun pyim-upgrade ()
  "升级 pyim 功能。"
  (interactive)
  (pyim-dcache-upgrade))

;; ** 键盘输入处理功能
(defun pyim-self-insert-command ()
  "Pyim 默认的 self-insert-command."
  (interactive "*")
  (cond
   ((pyim-process-input-chinese-p)
    (pyim-process-with-entered-buffer
      ;; 一定要注意，point 可能不在 point-min, 或者 point-max. 因为用
      ;; 户可能通过命令移动了 entered 中的 point。
      (insert (char-to-string last-command-event)))
    (pyim-process-run))
   ((pyim-process-get-candidates)
    (pyim-process-select-word-and-last-char))
   (t (pyim-process-select-last-char))))

(pyim-process-register-self-insert-command 'pyim-self-insert-command)

;; ** 加词功能
(defalias 'pyim-create-word-at-point
  #'pyim-process-create-word-at-point)

(defun pyim-create-2cchar-word-at-point ()
  "将光标前2个中文字符组成的字符串加入个人词库。"
  (interactive)
  (pyim-create-word-at-point 2))

(defun pyim-create-3cchar-word-at-point ()
  "将光标前3个中文字符组成的字符串加入个人词库。"
  (interactive)
  (pyim-create-word-at-point 3))

(defun pyim-create-4cchar-word-at-point ()
  "将光标前4个中文字符组成的字符串加入个人词库。"
  (interactive)
  (pyim-create-word-at-point 4))

(defun pyim-create-word-from-selection ()
  "Add the selected text as a Chinese word into the personal dictionary."
  (interactive)
  (when (region-active-p)
    (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
      (if (> (length string) 6)
          (error "PYIM: 所选词条太长。")
        (if (not (string-match-p "^\\cc+\\'" string))
            (error "PYIM: 所选词条包含非中文字符。")
          (pyim-process-create-word string)
          (message "PYIM: 将词条 %S 加入词库。" string)))
      (deactivate-mark)
      ;; NOTE: 这里必须返回 t, 因为这个函数的返回结果会被用来做为判断条件。
      t)))

;; ** 导入词条功能
(defun pyim-import-words-and-counts (file &optional merge-method silent)
  "从 FILE 中导入词条以及词条对应的词频信息。

导入的文件结构类似：

  ;;; -*- coding: utf-8-unix -*-
  ;; 词条 计数 对应编码(可选)
  你好 247
  这是 312

MERGE-METHOD 是一个函数，这个函数需要两个数字参数，代表词条在词频
缓存中的词频和待导入文件中的词频，函数返回值做为合并后的词频使用，
默认方式是：取两个词频的最大值。"
  (interactive "F导入词条和词频信息文件: ")
  ;; 导入词条和词频之前需要加载 dcaches.
  (when (or silent
            (yes-or-no-p "PYIM 词条导入注意事项：
1. 这个命令对多音字处理比较粗糙，可能会导入一些不合常理的词条记录，
   (比如：ying-xing 银行），但不影响 PYIM 正常使用。
2. 这个命令也可以用于形码输入法，比如：五笔，不过需要形码输入法有
   编码反查功能。
=> 确定继续导入吗？"))
    (pyim-process-init-dcaches)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents file))
      (goto-char (point-min))
      (forward-line 1)
      (while (not (eobp))
        (let* ((content (pyim-dline-parse))
               (word (car content))
               (count (string-to-number
                       (or (car (cdr content)) "0")))
               (criteria (car (cdr (cdr content)))))
          (pyim-process-create-word
           word nil
           (lambda (x)
             (funcall (or merge-method #'max)
                      (or x 0)
                      count))
           criteria)
          (unless silent
            (message "* 导入 %S" word)))
        (forward-line 1)))
    ;; 保存一下用户选择过的词生成的缓存和词频缓存，
    ;; 因为使用 async 机制更新 dcache 时，需要从 dcache 文件
    ;; 中读取变量值, 然后再对用户选择过的词生成的缓存排序，如果没
    ;; 有这一步骤，导入的词条就会被覆盖。
    (pyim-process-save-dcaches t)
    ;; 更新相关的 dcache
    (pyim-process-update t)

    (message "PYIM: 词条和词频信息导入完成！")))

;; ** 导出功能
(defun pyim-export-words-and-counts (file &optional confirm ignore-counts)
  "将个人词条以及词条对应的词频信息导出到文件 FILE.

如果 FILE 为 nil, 提示用户指定导出文件位置, 如果 CONFIRM 为
non-nil，文件存在时将会提示用户是否覆盖，默认为覆盖模式"
  (interactive "F将词条和词频信息导出到文件: ")
  (pyim-dcache-init-variables)
  (pyim-dcache-export-words-and-counts file confirm ignore-counts)
  (message "PYIM: 词条和词频信息导出完成。"))

(defun pyim-export-personal-words (file &optional confirm)
  "将用户的个人词条导出为 pyim 词库文件.

如果 FILE 为 nil, 提示用户指定导出文件位置, 如果 CONFIRM 为 non-nil，
文件存在时将会提示用户是否覆盖，默认为覆盖模式。"
  (interactive "F将个人词条导出到文件：")
  (pyim-dcache-init-variables)
  (pyim-dcache-export-personal-words file confirm)
  (message "PYIM: 个人词条导出完成。"))

;; ** 删词功能
(defun pyim-delete-words-in-file (file)
  "从个人词库缓存中批量删除 FILE 文件中列出的词条.

FILE 的格式与 `pyim-dcache-export' 生成的文件格式相同，
另外这个命令也可以识别没有词频的行，比如：

   ;;; -*- coding: utf-8-unix -*-
   词条1
   词条2"
  (interactive "F记录待删词条的文件: ")
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (goto-char (point-min))
    (forward-line 1)
    (while (not (eobp))
      (let ((word (car (pyim-dline-parse))))
        (when (and word (not (pyim-string-match-p "\\CC" word)))
          (pyim-process-delete-word word)))
      (forward-line 1)))
  (message "PYIM: 批量删词完成！"))

(defun pyim-delete-last-word ()
  "从个人词库中删除最新创建的词条。"
  (interactive)
  (let ((word (pyim-process-last-created-word)))
    (when word
      (pyim-process-delete-word word)
      (message "PYIM: 将词条 %S 从个人词库中删除！" word))))

(defalias 'pyim-delete-word-at-point
  #'pyim-process-delete-word-at-point)

(defun pyim-delete-word ()
  "从个人词库中删除词条。"
  (interactive)
  (cond
   (mark-active
    (let ((string (buffer-substring-no-properties
                   (region-beginning) (region-end))))
      (when (and (< (length string) 6)
                 (> (length string) 0))
        (pyim-process-delete-word string)
        (message "将词条 %S 从个人词库中删除。" string)
        (deactivate-mark))))
   (t (let ((words (completing-read-multiple
                    "请选择需要删除的词条(可多选): "
                    (pyim-process-last-created-words))))
        (dolist (word words)
          (pyim-process-delete-word word)
          (message "将词条 %S 从个人词库中删除。" word))))))

;; ** 选词功能
(defun pyim-select-word-simple ()
  "从选词框中选择当前词条.
这个函数与 `pyim-select-word' 的区别是：
这个函数不会将选择的词条加入个人词库，主要的使用场景是：
当用户需要输入一个生僻字时，输入包含该字的一个词条，
然后再删除不需要的字，由于这个词条不是常用词条，所以
不需要保存到个人词库。"
  (interactive)
  (if (pyim-process-get-candidates)
      (pyim-process-select-word-without-save)
    (pyim-process-select-last-char)))

(defun pyim-select-word ()
  "从选词框中选择当前词条，然后删除该词条对应拼音。"
  (interactive)
  (if (pyim-process-get-candidates)
      (pyim-process-select-word (pyim-scheme-current))
    ;; 如果没有选项，输入空格
    (pyim-process-select-last-char)))

(defun pyim-select-word-by-number (&optional num)
  "使用数字编号来选择对应的词条。"
  (interactive)
  (if (or pyim-select-word-by-number num)
      (pyim-select-word-by-number-1 num)
    ;; 有些输入法使用数字键编码，这种情况下，数字键就
    ;; 不能用来选词了。
    (call-interactively #'pyim-self-insert-command)))

(defun pyim-select-word-by-number-1 (num)
  (if (and (pyim-process-get-candidates)
           (pyim-page-plan-to-select-word num))
      (pyim-process-select-word (pyim-scheme-current))
    (pyim-process-select-last-char)))

(defun pyim-select-char-in-word-by-number (&optional n)
  "以词定字功能。"
  (interactive)
  (pyim-process-plan-to-select-char-in-word (1- (or n 1)))
  (pyim-process-select-word (pyim-scheme-current)))

;; ** 翻页和翻词功能
(defalias 'pyim-previous-page #'pyim-page-previous-page)
(defalias 'pyim-next-page #'pyim-page-next-page)
(defalias 'pyim-previous-word #'pyim-page-previous-word)
(defalias 'pyim-next-word #'pyim-page-next-word)

;; ** 取消当前输入功能
(defun pyim-quit-clear ()
  "取消当前输入的命令."
  (interactive)
  (pyim-process-select-nothing))

;; ** 字母上屏功能
(defun pyim-quit-no-clear ()
  "字母上屏命令."
  (interactive)
  (pyim-process-select-entered))

;; ** 中英文输入模式切换
(defalias 'pyim-toggle-input-ascii #'pyim-process-toggle-input-ascii)

;; ** 主辅输入法切换功能
(defun pyim-toggle-assistant-scheme ()
  "临时切换到辅助输入法.

这个功能一般用于五笔等形码输入法，在忘记编码的时候临时用拼音输入
中文。"
  (interactive)
  (if (pyim-process-without-entered-p)
      (pyim-process-select-last-char)
    (pyim-scheme-toggle-assistant)
    (pyim-process-run)))

;; ** PYIM 输入操作命令
(defun pyim-forward-point ()
  "光标前移"
  (interactive)
  (pyim-process-with-entered-buffer
    (forward-char))
  (pyim-process-run))

(defun pyim-backward-point ()
  "光标后移"
  (interactive)
  (pyim-process-with-entered-buffer
    (backward-char))
  (pyim-process-run))

(defun pyim-backward-imelem (&optional search-forward)
  "光标向后移动一个 imelem 对应的字符串

在全拼输入法中，就是向前移动一个拼音"
  (interactive)
  (let ((position (pyim-process-next-imelem-position 1 search-forward)))
    (pyim-process-with-entered-buffer
      (goto-char position))
    (pyim-process-run)))

(defun pyim-forward-imelem ()
  "光标向前移动一个 imelem 对应的字符串"
  (interactive)
  (pyim-backward-imelem t))

(defun pyim-end-of-line ()
  "光标移至行尾"
  (interactive)
  (pyim-process-with-entered-buffer
    (end-of-line))
  (pyim-process-run))

(defun pyim-beginning-of-line ()
  "光标移至行首"
  (interactive)
  (pyim-process-with-entered-buffer
    (beginning-of-line))
  (pyim-process-run))

(defun pyim-delete-backward-char (&optional n)
  "向后删除1个字符"
  (interactive)
  (pyim-process-with-entered-buffer
    (delete-char (- 0 (or n 1))))
  (if (pyim-process-without-entered-p)
      (pyim-process-select-nothing)
    (pyim-process-run)))

(defun pyim-delete-forward-char ()
  "向前删除1个字符"
  (interactive)
  (pyim-delete-backward-char -1))

(defun pyim-delete-backward-imelem (&optional search-forward)
  "向后删除一个 imelem 对应的字符串"
  (interactive)
  (let ((position (pyim-process-next-imelem-position 1 search-forward)))
    (pyim-process-with-entered-buffer
      (delete-region (point) position))
    (pyim-process-run)))

(defun pyim-delete-forward-imelem ()
  "向前删除一个 imelem 对应的字符串"
  (interactive)
  (pyim-delete-backward-imelem t))

;; ** 金手指功能
;;;###autoload
(defun pyim-convert-string-at-point (&optional _)
  "将光标前的用户输入的字符串转换为中文."
  (interactive "P")
  (pyim--activate-pyim)
  (or (pyim-create-word-from-selection)
      (pyim-process-trigger-feature-run-p)
      (pyim-process-feed-entered-at-point-into-pyim)
      (message "PYIM: 命令 `pyim-convert-string-at-point' 没有起作用！")))

(defun pyim--activate-pyim ()
  "如果当前输入法设置为 pyim, 就激活它。"
  (unless (equal input-method-function 'pyim-input-method)
    (activate-input-method 'pyim)))

;; ** 编码反查功能
(defun pyim-search-word-code ()
  "选择词条，然后反查它的 code。"
  (interactive)
  (when (region-active-p)
    (let* ((string (buffer-substring-no-properties (region-beginning) (region-end)))
           codes)
      (if (not (string-match-p "^\\cc+\\'" string))
          (error "PYIM: 不是纯中文字符串。")
        (setq codes (pyim-cstring-to-codes string (pyim-scheme-current)))
        (if codes
            (message "PYIM (%S): %S -> %S" pyim-default-scheme string codes)
          (message "PYIM: 没有找到 %S 对应的编码。" string)))
      (deactivate-mark))))

;; ** pyim 探针
(require 'pyim-probe)

;; ** pyim 云输入法
(require 'pyim-cloudim)

;; * Footer
(provide 'pyim)

;;; pyim.el ends here
