;;; pyim-process.el --- Process of pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-scheme)
(require 'pyim-dcache)
(require 'pyim-entered)
(require 'pyim-imobjs)
(require 'pyim-codes)
(require 'pyim-candidates)
(require 'pyim-outcome)
(require 'pyim-punctuation)
(require 'pyim-cstring)

(defgroup pyim-process nil
  "Process for pyim."
  :group 'pyim)

(defcustom pyim-english-input-switch-functions nil
  "让 pyim 开启英文输入功能.

这个变量的取值为一个函数列表，这个函数列表中的任意一个函数的
运行结果为 t 时，pyim 开启英文输入功能。"
  :type '(repeat function))

(defcustom pyim-force-input-chinese-functions nil
  "让 pyim 强制输入中文.

这个变量的取值为一个函数列表，这个函数列表中的任意一个函数的运行
结果为 t 时，pyim 将强制输入中文功能,无视
`pyim-english-input-switch-functions' 的设置."
  :type '(repeat function))

(defvaralias 'pyim-autoselector 'pyim-process-autoselector)
(defcustom pyim-process-autoselector nil
  "当前启用的自动上屏器列表.

自动上屏器是一个函数。假设用户已经输入 \"nihao\", 并按下 \"m\" 键，
那么当前entered 就是 \"nihaom\". 上次 entered 是 \"nihao\". 那么
返回值有3种情况（优先级按照下面的顺序）：

1. (:select last :replace-with \"xxx\") 自动上屏上次
entered (nihao) 的第一个候选词，m 键下一轮处理。

2. (:select current :replace-with \"xxx\") 自动上屏当前
entered (nihaom) 的第一个候选词。

3. nil  不自动上屏。

如果 :replace-with 设置为一个字符串，则选择最终会被这个字符串替代。

注意：多个 autoselector 函数运行时，最好不要相互影响，如果相互有
影响，需要用户自己管理。"
  :type '(repeat function))

(defcustom pyim-process-run-delay 0.5
  "延迟多少秒开始延迟获取词条。"
  :type 'number)

(defcustom pyim-select-finish-hook nil
  "Pyim 选词完成时运行的 hook."
  :type 'hook)

(defvar pyim-process-ui-init-hook nil
  "Hook used to run ui init functions.")

(defvar pyim-process-ui-refresh-hook nil
  "Hook used to run ui refresh functions.")

(defvar pyim-process-ui-hide-hook nil
  "Hook used to run ui hide functions.")

(defvar pyim-process-start-daemon-hook nil
  "Pyim start daemon hook.")

(defvar pyim-process-stop-daemon-hook nil
  "Pyim stop daemon hook.")

(defvar pyim-process--self-insert-commands nil
  "保存所有的 self insert command.")

(defvar pyim-process--input-ascii nil
  "是否开启 pyim 英文输入模式.")

(defvar pyim-process--force-input-chinese nil
  "是否强制开启中文输入模式.

这个变量只用于 `pyim-convert-string-at-point', 不要
在其它地方使用。")

(defvar pyim-process--translating nil
  "记录是否在转换状态.")

(defvar pyim-process--imobjs nil
  "Imobj (Input method object) 组成的 list.

imobj 在 pyim 里面的概念，类似与编译器里面的语法树，
它代表 pyim 输入的字符串 entered 解析得到的一个结构化对象，
以全拼输入法的为例：

1. entered: nihaoma
2. imobj: ((\"n\" \"i\" \"n\" \"i\") (\"h\" \"ao\" \"h\" \"ao\") (\"m\" \"a\" \"m\" \"a\"))

而 imobjs 是 imobj 组成的一个列表，因为有模糊音等概念的存在，一个
entered 需要以多种方式或者多步骤解析，得到多种可能的 imobj, 这些
imobj 组合构成在一起，构成了 imobjs 这个概念。比如：

1. entered: guafeng (设置了模糊音 en -> eng)
2. imobj-1: ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"en\" \"f\" \"eng\"))
3. imobj-2: ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"eng\" \"f\" \"eng\"))
4. imobjs:  (((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"en\" \"f\" \"eng\"))
             ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"eng\" \"f\" \"eng\")))

这个变量用来保存解析得到的 imobjs。

解析完成之后，pyim 会为每一个 imobj 创建对应 code 字符串, 然后在词库
中搜索 code 字符串来得到所需要的词条，最后使用特定的方式将得到的
词条组合成一个候选词列表：`pyim-candidates' 并通过 pyim-page 相关
功能来显示选词框，供用户选择词条，比如：

1. imobj: ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"en\" \"f\" \"en\"))
2. code: gua-fen

从上面的说明可以看出，imobj 本身也是有结构的：

1. imobj: ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"en\" \"f\" \"en\"))

我们将 (\"g\" \"ua\" \"g\" \"ua\") 这些子结构，叫做 imelem (IM element), *大
多数情况下*, 一个 imelem 能够代表一个汉字，这个概念在编辑 entered
的时候，非常有用。

另外要注意的是，不同的输入法， imelem 的内部结构是不一样的，比如：
1. quanping:  (\"g\" \"ua\" \"g\" \"ua\")
2. shuangpin: (\"h\" \"ao\" \"h\" \"c\")
3. wubi:      (\"aaaa\")")

(defvar pyim-process--code-criteria nil
  "用于 code 选取的基准字符串。

当获取到一个词条的多个 codes 时， pyim 会将所有的 codes 与这个字
符串进行比较，然后选择一个与这个字符串最相似的 code.

这个变量主要用于全拼和双拼输入法的多音字矫正，其取值一般使用用户
输入生成的 imobjs 转换得到，保留了用户原始输入的许多信息。")

(defvar pyim-process--candidates nil
  "所有备选词条组成的列表.")

(defvar pyim-process--last-candidates nil
  "上一轮备选词条列表，这个变量主要用于 autoselector 机制.")

(defvar pyim-process--word-position nil
  "当前选择的词条在 `pyim-candidates’ 中的位置.

细节信息请参考 `pyim-page--refresh' 的 docstring.")

(defvar pyim-process--char-position-in-word nil
  "“以词定字”功能中“字”在“词”中的位置.")

(defvar pyim-process--run-delay-timer nil
  "异步处理 entered 时，使用的 timer.")

(defvar pyim-process--last-created-words nil
  "记录最近创建的词条， 用于实现快捷删词功能： `pyim-delete-last-word' .")

(pyim-register-local-variables
 '(pyim-process--input-ascii
   pyim-process--translating
   pyim-process--imobjs
   pyim-process--candidates
   pyim-process--word-position))

;; ** pyim-input-method 核心函数
(defun pyim-process-input-method (key)
  "`pyim-process-input-method' 是 `pyim-input-method' 内部使用的函数。

这个函数比较复杂，作许多低层工作，但它的一个重要流程是：

1. 使用函数 `read-key-sequence' 得到 key-sequence
2. 使用函数 `lookup-key' 查询 `pyim-mode-map' 中，与上述 key-sequence 对应
   的命令。
3. 如果查询得到的命令是 self-insert-command 时，调用这个函数。
4. 这个函数最终会返回需要插入到 buffer 的字符串。

这个部份的代码涉及许多 emacs 低层函数，相对复杂，不容易理解，有兴
趣的朋友可以参考 elisp 手册相关章节:
1. Invoking the Input Method
2. Input Methods
3. Miscellaneous Event Input Features
4. Reading One Event"
  ;; Check the possibility of translating KEY.
  ;; If KEY is nil, we can anyway start translation.
  (pyim-process-ui-init)
  (if (or (integerp key) (null key))
      ;; OK, we can start translation.
      (let* ((echo-keystrokes 0)
             (help-char nil)
             (mode-map (pyim-process-get-mode-map))
             (overriding-terminal-local-map mode-map)
             (input-method-function nil)
             (input-method-use-echo-area nil)
             (modified-p (buffer-modified-p))
             last-command-event last-command this-command)

        (pyim-process--init-cleanup)

        (when key
          (pyim-add-unread-command-events key))

        (while (pyim-process--translating-p)
          (set-buffer-modified-p modified-p)
          (let* ((keyseq (read-key-sequence nil nil nil t))
                 (cmd (lookup-key mode-map keyseq)))
            ;; (message "key: %s, cmd:%s\nlcmd: %s, lcmdv: %s, tcmd: %s"
            ;;          key cmd last-command last-command-event this-command)
            (if (if key
                    (commandp cmd)
                  (pyim-process-self-insert-command-p cmd))
                (progn
                  ;; (message "keyseq: %s" keyseq)
                  (setq last-command-event (aref keyseq (1- (length keyseq)))
                        last-command this-command
                        this-command cmd)
                  (setq key t)
                  (condition-case-unless-debug err
                      (call-interactively cmd)
                    (error (message "pyim 出现错误: %S , 开启 debug-on-error 后可以了解详细情况。" err)
                           (beep))))
              ;; KEYSEQ is not defined in the translation keymap.
              ;; Let's return the event(s) to the caller.
              (pyim-add-unread-command-events (this-single-command-raw-keys) t)
              ;; (message "unread-command-events: %s" unread-command-events)
              (pyim-process-terminate))))
        ;; (message "return: %s" (pyim-process-get-select-result))
        (pyim-process-get-select-result))
    ;; Since KEY doesn't start any translation, just return it.
    ;; But translate KEY if necessary.
    (char-to-string key)))

(cl-defgeneric pyim-process-get-mode-map ()
  "获取 pyim mode map.")

(defun pyim-process--init-cleanup ()
  (pyim-entered-erase-buffer)
  (pyim-process--set-translating-flag t)
  (setq pyim-process--char-position-in-word nil)
  (pyim-outcome-erase))

(defun pyim-process--set-translating-flag (value)
  (setq pyim-process--translating value))

(defun pyim-process--translating-p ()
  pyim-process--translating)

(defun pyim-process-self-insert-command-p (cmd)
  "测试 CMD 是否是一个 pyim self insert command."
  (member cmd pyim-process--self-insert-commands))

(defun pyim-process-register-self-insert-command (command)
  (cl-pushnew command pyim-process--self-insert-commands))

(defun pyim-process-terminate ()
  "Terminate the translation of the current key."
  (pyim-process-terminate-really (pyim-scheme-current)))

(cl-defgeneric pyim-process-terminate-really (scheme)
  "Terminate the translation of the current key.")

(cl-defmethod pyim-process-terminate-really (_scheme)
  (pyim-process--set-translating-flag nil)
  (pyim-entered-erase-buffer)
  (setq pyim-process--code-criteria nil)
  (setq pyim-process--force-input-chinese nil)
  (setq pyim-process--candidates nil)
  (setq pyim-process--last-candidates nil)
  (pyim-process--run-delay-timer-reset)
  (pyim-process-ui-hide))

;; ** Dcache，UI 和 daemon 相关
(defun pyim-process-ui-init ()
  "初始化 pyim 相关 UI."
  (run-hooks 'pyim-process-ui-init-hook))

(cl-defgeneric pyim-process-ui-position ()
  "返回选词框等 UI 放置的位置。"
  (point))

(defun pyim-process-init-dcaches (&optional force)
  "PYIM 流程，词库相关的初始化工作。"
  (pyim-recreate-local-variables)
  (pyim-pymap-cache-create)
  (pyim-dcache-init-variables)
  (pyim-dcache-update force))

(defun pyim-process-save-dcaches (&optional force)
  "PYIM 流程，保存 dcache."
  (when force
    (pyim-dcache-save-caches)))

(defun pyim-process-update (&optional force)
  (when pyim-dcache-auto-update
    (pyim-dcache-update force)))

(defun pyim-process-start-daemon ()
  "启动 pyim 流程需要的 daemon."
  (run-hooks 'pyim-process-start-daemon-hook))

(defun pyim-process-stop-daemon ()
  "关闭 pyim 流程已经启动的 daemon."
  (interactive)
  (run-hooks 'pyim-process-stop-daemon-hook))

;; ** 输入相关
(defmacro pyim-process-with-entered-buffer (&rest forms)
  "PYIM 流程的输入保存在一个 buffer 中，使用 FORMS 处理这个 buffer
中的信息。"
  (declare (indent 0) (debug t))
  `(pyim-entered-with-entered-buffer
     (ignore-errors
       ,@forms)))

(defun pyim-process-next-imelem-position (num &optional search-forward start)
  "从 `pyim-entered--buffer' 的当前位置，找到下一个或者下 NUM 个 imelem 对应的位置

如果 SEARCH-FORWARD 为 t, 则向前搜索，反之，向后搜索。"
  (pyim-entered-with-entered-buffer
    (let* ((scheme (pyim-scheme-current))
           (start (or start (point)))
           (end-position start)
           (string (buffer-substring-no-properties (point-min) start))
           (orig-imobj-len (length (car (pyim-imobjs-create string scheme))))
           imobj pos)
      (if search-forward
          ;; "ni|haoshijie" -> "nihao|shijie"
          (progn
            (setq pos (point-max))
            (while (and (> pos start) (= end-position start))
              (setq string (buffer-substring-no-properties (point-min) pos)
                    imobj (car (pyim-imobjs-create string scheme)))
              (if (>= (+ orig-imobj-len num) (length imobj))
                  (setq end-position pos)
                (cl-decf pos))))
        ;; "nihao|shijie" -> "ni|haoshijie"
        (if (<= orig-imobj-len num)
            (setq end-position (point-min))
          (setq pos start)
          (while (and (>= pos (point-min)) (= end-position start))
            (setq string (buffer-substring-no-properties (point-min) pos)
                  imobj (car (pyim-imobjs-create string scheme)))
            (if (= (- orig-imobj-len num) (length imobj))
                (setq end-position pos)
              (cl-decf pos)))))
      end-position)))

(defun pyim-process--string-at-region-or-point ()
  (if mark-active
      (buffer-substring-no-properties
       (region-beginning) (region-end))
    (buffer-substring (point) (line-beginning-position))))

(defun pyim-process-feed-entered-at-point-into-pyim ()
  (let* ((entered-info (pyim-process--find-entered-at-point))
         (entered (nth 0 entered-info))
         (char-num-need-delete (nth 1 entered-info)))
    (when entered-info
      (pyim-process--delete-region-or-chars char-num-need-delete)
      (pyim-process--feed-entered-into-pyim entered)
      ;; NOTE: 这里必须返回 t, 因为这个函数的返回结果会被作为判断条件使用。
      t)))

(defun pyim-process--find-entered-at-point ()
  "从光标处提取一个有效的 entered 字符串."
  (let* ((case-fold-search nil)
         (scheme (pyim-scheme-current))
         (first-chars (pyim-scheme-first-chars scheme))
         (rest-chars (pyim-scheme-rest-chars scheme))
         (regexp-used-to-extract-entered
          (format "[%s]+ *$"
                  (cl-delete-duplicates
                   (concat first-chars rest-chars "'-"))))
         (string (pyim-process--string-at-region-or-point)))
    (when (string-match regexp-used-to-extract-entered string)
      (let* ((entered (match-string 0 string))
             ;; 一些编程语言使用单引号做为字符串的标记，这里需要特殊处理。
             (entered (replace-regexp-in-string "^[-']" "" entered))
             (backward-delete-char-number (length entered))
             (entered (replace-regexp-in-string " +" "" entered)))
        (list entered backward-delete-char-number)))))

(defun pyim-process--delete-region-or-chars (&optional num)
  "删除 region 或者光标之前 NUM 个字符。"
  (if mark-active
      (delete-region
       (region-beginning) (region-end))
    (when (and (numberp num) (> num 0))
      (delete-char (- 0 num)))))

(defun pyim-process--feed-entered-into-pyim (entered)
  (when (and (stringp entered) (> (length entered) 0))
    (pyim-add-unread-command-events entered)
    (pyim-process--force-input-chinese)))

(defun pyim-process--force-input-chinese ()
  "让 pyim 强制输入中文，忽略所有探针函数。"
  (setq pyim-process--force-input-chinese t))

;; ** 中英文切换相关
(defun pyim-process-toggle-input-ascii ()
  "pyim 切换中英文输入模式, 同时调整标点符号样式。"
  (interactive)
  (setq pyim-process--input-ascii
        (not pyim-process--input-ascii)))

(defun pyim-process-input-chinese-p ()
  "确定 pyim 是否需要启动中文输入模式."
  (let* ((scheme (pyim-scheme-current))
         (first-chars (pyim-scheme-first-chars scheme))
         (rest-chars (pyim-scheme-rest-chars scheme))
         (entered (pyim-entered-get 'point-before)))
    (and (pyim-process--input-chinese-predicate-1)
         (pyim-process--input-chinese-predicate-2
          last-command-event entered first-chars rest-chars))))

(defun pyim-process--input-chinese-predicate-1 ()
  "`pyim-process-input-chinese-p' 内部函数，测试环境。"
  (or (pyim-process--force-input-chinese-p)
      (and (not pyim-process--input-ascii)
           (not (pyim-process-auto-switch-english-input-p)))))

(defun pyim-process--force-input-chinese-p ()
  "判断是否强制输入中文，这个函数主要处理变量：
`pyim-force-input-chinese-functions'."
  (let ((func-or-list pyim-force-input-chinese-functions))
    (or pyim-process--force-input-chinese
        (cl-some (lambda (x)
                   (when (functionp x)
                     (funcall x)))
                 (cond ((functionp func-or-list) (list func-or-list))
                       ((listp func-or-list) func-or-list)
                       (t nil))))))

(defun pyim-process-auto-switch-english-input-p ()
  "判断是否 *根据环境自动切换* 为英文输入模式，这个函数处理变量：
`pyim-english-input-switch-functions'"
  (let ((func-or-list pyim-english-input-switch-functions))
    (cl-some (lambda (x)
               (when (functionp x)
                 (funcall x)))
             (cond ((functionp func-or-list) (list func-or-list))
                   ((listp func-or-list) func-or-list)
                   (t nil)))))

(defun pyim-process--input-chinese-predicate-2 (event entered first-chars rest-chars)
  "`pyim-process-input-chinese-p' 内部函数，测试输入。"
  (if (not (string< "" entered))
      (member event (mapcar #'identity first-chars))
    (member event (mapcar #'identity rest-chars))))

(defun pyim-process-indicator-function ()
  "Indicator function."
  (pyim-process--input-chinese-predicate-1))

;; ** 解析输入 -> 获取词条 -> 自动选词
(defun pyim-process-run ()
  "查询 entered 字符串, 显示备选词等待用户选择。"
  (if (pyim-process-without-entered-p)
      (pyim-process-terminate)
    (let* ((scheme (pyim-scheme-current))
           entered-to-translate)
      (setq entered-to-translate
            (pyim-entered-get 'point-before))
      (setq pyim-process--imobjs
            (pyim-imobjs-create entered-to-translate scheme))
      (setq pyim-process--last-candidates
            pyim-process--candidates)
      (setq pyim-process--candidates
            (or (delete-dups
                 (pyim-candidates-create
                  pyim-process--imobjs scheme))
                (list entered-to-translate)))
      (unless (eq (pyim-process--auto-select) 'auto-select-success)
        (pyim-process-plan-to-select-word 0)
        (pyim-process-ui-refresh)
        (pyim-process--run-delay)))))

(defun pyim-process-without-entered-p ()
  (= (length (pyim-process-get-entered 'point-before)) 0))

(defun pyim-process-get-entered (&optional type)
  (pyim-entered-get type))

(defun pyim-process-ui-hide ()
  "隐藏 pyim 相关 UI."
  (run-hooks 'pyim-process-ui-hide-hook))

(defun pyim-process--auto-select ()
  "自动上屏操作流程。

1. (:select last) 模式：

假如用户输入 \"nihao\", 然后按了 \"m\" 键, 那么当前的 entered 就
是 \"nihaom\", 如果 autoselector 返回 list: (:select last), 那么，
\"nihao\" 对应的第一个候选词将上屏，m 键下一轮继续处理，这是一种
\"踩雷确认模式\".

2. (:select current) 模式：

假设用户已经输入 \"niha\", 然后按了 \"o\" 键，那么，当前 entered
就是 \"nihao\". 如果 autoselector 函数返回一个 list:
(:select current), 那么就直接将 \"nihao\" 对应的第一个候选词上屏
幕。

3. 如果返回的 list 中包含 :replace-with \"xxx\" 信息，那么
\"xxx\" 上屏。"
  (let* ((results
          ;; autoselector 功能会影响手动连续选择功能，所以这里做了一些限制，
          ;; 只有在输入的时候才能够触发 autoselector 机制。
          (when (pyim-process-self-insert-command-p this-command)
            (pyim-process--autoselector-results)))
         (select-last-word
          (pyim-process--autoselector-find-result results 'last))
         (select-current-word
          (pyim-process--autoselector-find-result results 'current)))
    (when (or select-last-word select-current-word)
      (pyim-process--auto-select-word select-last-word select-current-word)
      ;; autoselector 机制已经触发的时候，如果发现 entered buffer 中
      ;; point 后面还有未处理的输入，就将其转到下一轮处理，这种情况
      ;; 很少出现，一般是型码输入法，entered 编辑的时候有可能触发。
      (pyim-add-unread-command-events
       (pyim-entered-get 'point-after))
      (when select-last-word
        (pyim-add-unread-command-events last-command-event))
      (pyim-process-terminate)
      ;; 如果自动上屏操作成功完成，就返回 'auto-select-success,
      ;; pyim 后续操作会检测这个返回值。
      'auto-select-success)))

(defun pyim-process--autoselector-results ()
  "运行所有 autoselectors, 返回结果列表。"
  (mapcar (lambda (x)
            (when (functionp x)
              (ignore-errors
                (funcall x))))
          (cl-remove-duplicates
           pyim-process-autoselector
           :from-end t
           :test #'equal)))

(defun pyim-process--autoselector-find-result (results type)
  "从所有 autoselector 运行结果中，寻找返回类型为 TYPE 的结果。"
  (cl-find-if (lambda (x)
                (equal (plist-get x :select) type))
              results))

(defun pyim-process--auto-select-word (select-last-word select-current-word)
  (let ((pyim-process--candidates
         (pyim-process--get-autoselect-candidates
          select-last-word select-current-word)))
    (pyim-process-select-word-without-save 'do-not-terminate)
    (pyim-process-create-word (pyim-process-get-select-result) t)))

(defun pyim-process--get-autoselect-candidates (select-last-word select-current-word)
  (let* ((str (plist-get (if select-last-word
                             select-last-word
                           select-current-word)
                         :replace-with))
         (candidates
          (if select-last-word
              (pyim-process-get-last-candidates)
            (pyim-process-get-candidates))))
    (if (and str (stringp str))
        (list str)
      candidates)))

(defun pyim-process-get-last-candidates ()
  pyim-process--last-candidates)

(defun pyim-process-get-candidates ()
  pyim-process--candidates)

(defun pyim-process-ui-refresh (&optional hightlight-current)
  "刷新 pyim 相关 UI."
  (run-hook-with-args 'pyim-process-ui-refresh-hook hightlight-current))

(defun pyim-process--run-delay ()
  "运行延迟获取候选词流程。

当用户输入停顿时间超过 `pyim-process--run-delay' 这个阈值时，就激
活延迟获取候选词流程，目前，延迟获取候选词有两种处理模式：

1. 同步+限时+用户抢断模式：比如：搜索 buffer 词条等。
2. 异步模式：比如：调用云输入法接口等。

注意：按理说，两种模式的延时阈值应该单独设置的，但当前 pyim 没有
将其分开，因为这样做在满足当前需求的同时，可以简化代码，如果以后
有新功能必须将其分开时，再做考虑。"
  (pyim-process--run-delay-timer-reset)
  (setq pyim-process--run-delay-timer
        (run-with-timer
         pyim-process-run-delay
         nil #'pyim-process--run-delay-timer-function)))

(defun pyim-process--run-delay-timer-reset ()
  "Reset `pyim-process--run-delay-timer'."
  (when pyim-process--run-delay-timer
    (cancel-timer pyim-process--run-delay-timer)
    (setq pyim-process--run-delay-timer nil)))

(defun pyim-process--run-delay-timer-function ()
  "Function used by `pyim-process--run-delay-timer'"
  (pyim-process--handle-candidates-async)
  (pyim-process--handle-candidates-limit-time))

(defun pyim-process--handle-candidates-limit-time ()
  "使用限时的方式获取候选词。"
  (let* ((scheme (pyim-scheme-current))
         (words (pyim-candidates-create-limit-time
                 pyim-process--imobjs scheme)))
    (when words
      (setq pyim-process--candidates
            (pyim-process--merge-candidates words pyim-process--candidates))
      (pyim-process-ui-refresh))))

(defun pyim-process--merge-candidates (new-candidates old-candidates)
  "将 OLD-CANDIDATES 和 NEW-CANDIDATES 合并的默认策略。"
  (remove nil (delete-dups
               `(,(car old-candidates)
                 ,@new-candidates
                 ,@(cdr old-candidates)))))

(defun pyim-process--handle-candidates-async ()
  "使用异步的方式获取候选词条词条。"
  (let ((scheme (pyim-scheme-current))
        (buffer (current-buffer)))
    (pyim-candidates-create-async
     pyim-process--imobjs scheme
     (lambda (async-return)
       (with-current-buffer buffer
         (when (and (pyim-process--translating-p)
                    (not (input-pending-p))
                    (equal (car async-return) pyim-process--imobjs))
           (setq pyim-process--candidates
                 (pyim-process--merge-candidates (cdr async-return) pyim-process--candidates))
           (pyim-process-ui-refresh)))))))

;; ** 预选词条相关
(defun pyim-process-plan-to-select-word (word-position)
  "预选 candidates 列表中 WORD-POSITION 位置的词条。"
  (setq pyim-process--word-position word-position))

(defun pyim-process-plan-to-select-char-in-word (char-position)
  "以词定字功能中，通过 CHAR-POSITION 预选词条中的汉字。"
  (setq pyim-process--char-position-in-word char-position))

(defun pyim-process-next-word-position (n)
  "返回已选词条后面地 N 个词条对应的位置。"
  (let* ((new (+ (pyim-process-word-position) n))
         (max (1- (pyim-process-candidates-length)))
         (pos (if (>= max new)
                  (if (< new 0) max new)
                0)))
    pos))

(defun pyim-process-word-position (&optional position)
  "返回已选词条的位置。"
  (min (- (pyim-process-candidates-length) 1)
       (if (integerp position)
           position
         pyim-process--word-position)))

(defun pyim-process-candidates-length ()
  "返回候选词条列表长度。"
  (length pyim-process--candidates))

;; ** 选词造词相关
(defun pyim-process-select-nothing ()
  "不选择任何东西。"
  (pyim-outcome-erase)
  (pyim-process-terminate))

(defun pyim-process-select-entered ()
  (pyim-outcome-add (pyim-entered-get 'point-before))
  (pyim-process-terminate))

(cl-defgeneric pyim-process-select-word (scheme)
  "按照 SCHEME 对应的规则，对预选词条进行选词操作。")

(cl-defmethod pyim-process-select-word :after (_scheme)
  "运行 `pyim-select-finish-hook'."
  (run-hooks 'pyim-select-finish-hook))

(cl-defmethod pyim-process-select-word ((_scheme pyim-scheme-quanpin))
  "按照全拼规则，对预选词条进行选词操作。"
  (pyim-process--create-code-criteria)
  (pyim-process-select-word-without-save 'do-not-terminate)
  (if (pyim-process--multi-step-select-word-p)
      (pyim-process--select-word-in-next-step)
    (pyim-process-create-word (pyim-process-get-select-result) t)
    (pyim-process-terminate)))

(defun pyim-process--create-code-criteria ()
  "创建 `pyim-process--code-criteria'."
  (setq pyim-process--code-criteria
        (let ((str (string-join
                    (pyim-codes-create (pyim-process-get-first-imobj)
                                       (pyim-scheme-current)))))
          (if (> (length pyim-process--code-criteria)
                 (length str))
              pyim-process--code-criteria
            str))))

(defun pyim-process-select-word-without-save (&optional do-not-terminate)
  "选择词条但不保存词条。"
  (let ((word (nth pyim-process--word-position
                   pyim-process--candidates)))
    (pyim-outcome-add (concat (pyim-outcome-get) word))
    (unless do-not-terminate
      (pyim-process-terminate))))

(defun pyim-process--multi-step-select-word-p ()
  "判断是否使用连续选词模式。"
  (and (not (pyim-process--select-char-in-word-p)) ;以词定字的时候，不连续选择，处理起来太复杂。
       (or (pyim-process--entered-to-be-translated)
           (> (length (pyim-process-get-entered 'point-after)) 0))))

(defun pyim-process--select-char-in-word-p ()
  pyim-process--char-position-in-word)

(defun pyim-process-get-first-imobj ()
  (car pyim-process--imobjs))

(defun pyim-process--entered-to-be-translated ()
  "连续选择时，获取 entered 未转换的一部分.

大体来说，entered 字符串可以分解为三个部分：

1. 光标前字符串
   1. 光标前已经转换的字符串
   2. 光标前还没有转换的字符串。
2. 光标后字符串

对 entered 字符串的处理思路是：截取已经转换的字符串，把未转换的字
符串和光标后的字符串合并后下一轮递归的处理。

比如：entered 为 xiaolifeidao, 本次选择 “小李” 之后，需要将
entered 截断，“小李” 这个词条长度为 2, 就将 entered 从头开始缩减
2 个 imelem 对应的字符，变成 feidao, 为下一次选择 “飞” 做准备。

在连续选择时，当前选择的词条和 outcome 是不一致的，比如：
xiaolifeidao

第一次选择：小李， outcome = 小李
第二次选择：飞，   outcome = 小李飞
第三次选择：刀，   outcome = 小李飞刀

注意事项： 这里有一个假设前提是： 一个 imelem 对应一个汉字，
在全拼输入法中，这个假设大多数情况是成立的，但在型码输入法
中，比如五笔输入法，就不成立，好在型码输入法一般不需要多次
选择。"
  (let* ((imobj (pyim-process-get-first-imobj))
         (length-selected-word-in-this-step
          (length (pyim-outcome-diff))))
    (when (< length-selected-word-in-this-step (length imobj))
      (string-join
       (mapcar (lambda (w)
                 (concat (nth 2 w) (nth 3 w)))
               (nthcdr length-selected-word-in-this-step imobj))))))

(defun pyim-process--select-word-in-next-step ()
  "在连续选词模式下，下一轮需要进行的选词操作。"
  (let ((entered-to-be-translated
         (pyim-process--entered-to-be-translated)))
    (pyim-process-with-entered-buffer
      ;; 把光标前已转换的 entered 字符串, 从 entered 字符串里面去掉，保留未
      ;; 转换的字符串和光标之后的字符串。
      (delete-region (point-min) (point))
      (insert entered-to-be-translated)
      ;; 为下一次选词作准备，一般情况下词库里面的词条不会超过20个汉字，所以
      ;; 这里光标向前移动不超过20个 imelem. 从而让下一轮处理时的 “光标前字符
      ;; 串” 比较长，这种方式可能比逐字选择更加好用。
      (goto-char (pyim-process-next-imelem-position 20 t 1)))
    (pyim-process-run)))

(defun pyim-process-create-word (word &optional prepend wordcount-handler criteria)
  "将中文词条 WORD 添加编码后，保存到用户选择过的词生成的缓存中。

词条 WORD 默认会追加到已有词条的后面，如果 PREPEND 设置为 t,
词条就会放到已有词条的最前面。

这是函数会调用 `pyim-cstring-to-codes' 来获取中文词条对应的编码。

WORDCOUNT-HANDLER 可以是一个数字，代表将此数字设置为 WORD 的新词频，
WORDCOUNT-HANDLER 也可以是一个函数，其返回值将设置为 WORD 的新词频，
而这个函数的参数则表示 WORD 当前词频，这个功能用于：`pyim-dcache-import',
如果 WORDCOUNT-HANDLER 设置为其他, 则表示让 WORD 当前词频加1.

如果 CRITERIA 是一个字符串，在多音字矫正时，将使用这个字符串来矫
正多音字。

BUG：拼音无法有效地处理多音字。"
  (when (pyim-process--create-word-p word)
    (pyim-process--add-last-created-word word)
    (pyim-process--add-word-to-dcache word prepend wordcount-handler criteria)))

(defun pyim-process--create-word-p (word)
  (and (> (length word) 0)
       ;; NOTE: 十二个汉字及以上的词条，加到个人词库里面用处不大，这是很主观的一
       ;; 个数字，也许应该添加一个配置选项？
       (< (length word) 12)
       (not (pyim-string-match-p "\\CC" word))))

(defun pyim-process--add-last-created-word (word)
  (setq pyim-process--last-created-words
        (cons word (remove word pyim-process--last-created-words))))

(defun pyim-process--add-word-to-dcache (word prepend wordcount-handler criteria)
  (let* (;; PYIM 有些功能会用到 text property, 保存词条之前将 text property 去除，防
         ;; 止不必要的数据进入 cache.
         (word (substring-no-properties word))
         (scheme (pyim-scheme-current))
         (code-prefix (pyim-scheme-code-prefix scheme))
         (codes (pyim-cstring-to-codes
                 word scheme
                 (or criteria pyim-process--code-criteria))))
    ;; 保存对应词条的词频
    (when (> (length word) 0)
      (pyim-dcache-update-wordcount word (or wordcount-handler #'1+)))
    ;; 添加词条到个人缓存
    (dolist (code codes)
      (unless (pyim-string-match-p "[^ a-z-]" code)
        (pyim-dcache-insert-word
         (if (and (> (length word) 1)
                  (> (length codes) 1))
             ;; 如果 word 超过一个汉字，并且得到多个 codes，那么大概率说明没有
             ;; 正确处理多音字，这里设置一下 :noexport 属性，在导出词条的时候
             ;; 不导出这些带标记的词。
             (propertize word :noexport t)
           word)
         (concat (or code-prefix "") code) prepend)))))

(defun pyim-process-get-select-result ()
  "返回 PYIM 选择操作的结果。"
  (pyim-process-magic-convert
   (pyim-outcome-get)))

(defun pyim-process-magic-convert (string)
  "返回 STRING 以词定字和魔术转换后的新字符串."
  (pyim-outcome-magic-convert
   (pyim-process--char-in-word string)))

(defun pyim-process--char-in-word (word)
  (let ((pos pyim-process--char-position-in-word)
        (length (length word)))
    (if (and (integerp pos)
             (< pos length))
        (substring word pos (1+ pos))
      word)))

(cl-defmethod pyim-process-select-word ((_scheme pyim-scheme-xingma))
  "按照形码规则，对预选词条进行选词操作。"
  (pyim-process-select-word-without-save 'do-not-terminate)
  (if (pyim-entered-in-the-middle-of-entered-p)
      (progn
        (pyim-process-with-entered-buffer
          (delete-region (point-min) (point)))
        (pyim-process-run))
    (pyim-process-create-word (pyim-process-get-select-result) t)
    (pyim-process-terminate)))

(defun pyim-process-select-last-char ()
  "选择上一个输入的字符。"
  (pyim-outcome-add
   (concat (pyim-outcome-get)
           (pyim-process-select-handle-char last-command-event)))
  (pyim-process-terminate))

(defun pyim-process-select-handle-char (char)
  "Pyim 字符转换函数，CHAR 代表 *待输入* 的字符。"
  (let ((str (char-to-string char)))
    (cond
     ((pyim-process--invalid-char-p char) "")
     ((and (pyim-outcome-trigger-p (char-to-string char))
           (pyim-process-trigger-feature-run-p))
      "")
     ((pyim-process--punctuation-half-width-p char) str)
     ((pyim-punctuation-p char)
      (pyim-punctuation-return-proper-punct char))
     (t str))))

(defun pyim-process--invalid-char-p (char)
  "当 CHAR 是空格前面的字符时，返回 t."
  (< char ? ))

(defun pyim-process-trigger-feature-run-p ()
  (not (eq (pyim-process--trigger-feature-run)
           'without-trigger-feature)))

(defun pyim-process--trigger-feature-run ()
  (cond
   ((pyim-process--trigger-delete-word-p)
    (let ((number-before-2 (pyim-char-before-to-number 1)))
      (delete-char -2)
      (pyim-process-delete-word-at-point number-before-2)))

   ((pyim-process--trigger-create-word-p)
    (let ((number-before-1 (pyim-char-before-to-number 0)))
      (delete-char -1)
      (pyim-process-create-word-at-point number-before-1)))

   ((pyim-process--trigger-call-function-p)
    (pyim-outcome-call-trigger-function))

   ((pyim-process--trigger-punctuation-to-full-width-p)
    (pyim-punctuation-translate 'full-width))

   ((pyim-process--trigger-punctuation-to-half-width-p)
    (pyim-punctuation-translate 'half-width))

   (t 'without-trigger-feature)))

(defun pyim-process--trigger-delete-word-p ()
  "当光标之前的字符串类似 “<中文>[1-9]-” 时，比如 “你好2-” ，返回 t."
  (let* ((str-before-2 (pyim-char-before-to-string 1))
         (str-before-3 (pyim-char-before-to-string 2)))
    (and (eq (char-before) ?-)
         (pyim-string-match-p "[0-9]" str-before-2)
         (pyim-string-match-p "\\cc" str-before-3))))

(defun pyim-process-delete-word-at-point (&optional number silent)
  "将光标前字符数为 NUMBER 的中文字符串从个人词库中删除
当 SILENT 设置为 t 是，不显示提醒信息。"
  (let ((string (pyim-cstring-at-point (or number 2))))
    (when string
      (pyim-process-delete-word string)
      (unless silent
        (message "词条: \"%s\" 已经从个人词库缓冲中删除。" string)))))

(defun pyim-process--trigger-create-word-p ()
  "当光标之前的字符串类似 “<中文>[2-9]” 时，比如 “你好2” ，返回 t."
  (let ((str-before-2 (pyim-char-before-to-string 1)))
    (and (member (char-before) (number-sequence ?2 ?9))
         (pyim-string-match-p "\\cc" str-before-2))))

(defun pyim-process-create-word-at-point (&optional number silent)
  "将光标前字符数为 NUMBER 的中文字符串添加到个人词库中，当
SILENT 设置为 t 是，不显示提醒信息。"
  (when-let* ((string (pyim-cstring-at-point (or number 2))))
    (pyim-process-create-word string)
    (unless silent
      (message "PYIM: 将词条 %S 加入词库。" string))))

(defun pyim-process--trigger-call-function-p ()
  "当光标之前是中文但不是标点符号时，返回 t."
  (let ((str-before-1 (pyim-char-before-to-string 0)))
    (and (not (pyim-punctuation-position str-before-1))
         (pyim-string-match-p "\\cc" str-before-1)
         (functionp pyim-outcome-trigger-function))))

(defun pyim-process--trigger-punctuation-to-full-width-p ()
  "当光标前面是半角标点时，返回 t."
  (let* ((str-before-1 (pyim-char-before-to-string 0))
         (punc-posit-before-1 (pyim-punctuation-position str-before-1)))
    (and (numberp punc-posit-before-1)
         (= punc-posit-before-1 0))))

(defun pyim-process--trigger-punctuation-to-half-width-p ()
  "当光标前面是全角标点时，返回 t."
  (let* ((str-before-1 (pyim-char-before-to-string 0))
         (punc-posit-before-1 (pyim-punctuation-position str-before-1)))
    (and (numberp punc-posit-before-1)
         (> punc-posit-before-1 0))))

(defun pyim-process--punctuation-half-width-p (char)
  "根据 CHAR 和环境信息，判断是否输入半角符号。"
  (or (not (pyim-process--punctuation-full-width-p))
      (pyim-punctuation-auto-half-width-p char)
      (pyim-punctuation-escape-p (char-before))))

(defun pyim-process--punctuation-full-width-p ()
  "判断是否需要切换到全角标点输入模式

输入标点的样式的改变（全角或者半角）受三个方面影响：

1. 用户是否手动切换了标点样式？
2  用户是否手动切换到英文输入模式？
3. pyim 是否根据环境自动切换到英文输入模式？

三方面的综合结果为： 只要当前的输入模式是英文输入模式，那么输入的
标点符号 *必定* 是半角标点，如果当前输入模式是中文输入模式，那么，
输入标点的样式用户可以使用 `pyim-punctuation-toggle'手动控制，具
体请参考 `pyim-process--punctuation-full-width-p'。"
  (cl-case (car pyim-punctuation-translate-p)
    (yes t)
    (no nil)
    (auto
     ;; 如果用户手动或者根据环境自动切换为英文输入模式，
     ;; 那么标点符号也要切换为半角模式。
     (and (not pyim-process--input-ascii)
          (not (pyim-process-auto-switch-english-input-p))))))

(defun pyim-process-select-word-and-last-char ()
  "选择预选词条和上一次输入的字符。"
  (let ((word (nth (1- pyim-process--word-position)
                   pyim-process--candidates)))
    (pyim-outcome-add
     (concat (pyim-outcome-get) word
             (pyim-process-select-handle-char last-command-event)))
    (pyim-process-terminate)))

;; ** 删词相关
(defun pyim-process-delete-word (word)
  (pyim-dcache-delete-word word)
  (pyim-process-remove-last-created-word word))

(defun pyim-process-remove-last-created-word (word)
  (setq pyim-process--last-created-words
        (remove word pyim-process--last-created-words)))

(defun pyim-process-last-created-word ()
  (car pyim-process--last-created-words))

(defun pyim-process-last-created-words ()
  pyim-process--last-created-words)


;; * Footer
(provide 'pyim-process)

;;; pyim-process.el ends here
