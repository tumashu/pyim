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
  :type 'symbol)

(defcustom pyim-force-input-chinese-functions nil
  "让 pyim 强制输入中文.

这个变量的取值为一个函数列表，这个函数列表中的任意一个函数的运行
结果为 t 时，pyim 将强制输入中文功能,无视
`pyim-english-input-switch-functions' 的设置."
  :type 'symbol)

(defvaralias 'pyim-autoselector 'pyim-process-autoselector)
(defcustom pyim-process-autoselector nil
  "已经启用的自动上屏器.

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
  :type '(choice (const nil)
                 (repeat function)))

(define-obsolete-variable-alias
  'pyim-process-async-delay 'pyim-process-run-delay "5.0")

(defcustom pyim-process-run-delay 0.5
  "延迟多少秒开始延迟获取词条。"
  :type 'integer)

(defvar pyim-process-input-ascii nil
  "是否开启 pyim 英文输入模式.")

(defvar pyim-process-force-input-chinese nil
  "是否强制开启中文输入模式.

这个变量只用于 `pyim-convert-string-at-point', 不要
在其它地方使用。")

(defvar pyim-process-translating nil
  "记录是否在转换状态.")

(defvar pyim-process-last-created-words nil
  "记录最近创建的词条， 用于实现快捷删词功能： `pyim-delete-last-word' .")

(defvar pyim-process-code-criteria nil
  "用于 code 选取的基准字符串。

当获取到一个词条的多个 codes 时， pyim 会将所有的 codes 与这个字
符串进行比较，然后选择一个与这个字符串最相似的 code.

这个变量主要用于全拼和双拼输入法的多音字矫正，其取值一般使用用户
输入生成的 imobjs 转换得到，保留了用户原始输入的许多信息。")

(defvar pyim-process-run-delay-timer nil
  "异步处理 entered 时，使用的 timer.")

(defvar pyim-process-self-insert-commands nil
  "保存所有的 self insert command.")

(defvar pyim-process-ui-init-hook nil
  "Hook used to run ui init functions.")

(defvar pyim-process-ui-refresh-hook nil
  "Hook used to run ui refresh functions.")

(defvar pyim-process-ui-hide-hook nil
  "Hook used to run ui hide functions.")

(defvar pyim-process-ui-position-function #'point
  "The value is a function returned a position where ui place.")

(defvar pyim-process-start-daemon-hook nil
  "Pyim start daemon hook.")

(defvar pyim-process-stop-daemon-hook nil
  "Pyim stop daemon hook.")

(defvar pyim-process-imobjs nil
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

(defvar pyim-process-candidates nil
  "所有备选词条组成的列表.")

(defvar pyim-process-candidates-last nil
  "上一轮备选词条列表，这个变量主要用于 autoselector 机制.")

(defvar pyim-process-candidate-position nil
  "当前选择的词条在 `pyim-candidates’ 中的位置.

细节信息请参考 `pyim-page-refresh' 的 docstring.")

(pyim-register-local-variables
 '(pyim-process-input-ascii
   pyim-process-translating
   pyim-process-imobjs
   pyim-process-candidates
   pyim-process-candidate-position))

(defun pyim-process-ui-init ()
  "初始化 pyim 相关 UI."
  (run-hooks 'pyim-process-ui-init-hook))

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

(defmacro pyim-process-with-entered-buffer (&rest forms)
  "PYIM 流程的输入保存在一个 buffer 中，使用 FORMS 处理这个 buffer
中的信息。"
  (declare (indent 0) (debug t))
  `(pyim-entered-with-entered-buffer
     ,@forms))

(defun pyim-process-get-entered (&optional type)
  (pyim-entered-get type))

(defun pyim-process-next-imelem-position (num &optional search-forward start)
  "从 `pyim-entered-buffer' 的当前位置，找到下一个或者下 NUM 个 imelem 对应的位置

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

(defun pyim-process-force-input-chinese-p ()
  "判断是否强制输入中文，这个函数主要处理变量：
`pyim-force-input-chinese-functions'."
  (let ((func-or-list pyim-force-input-chinese-functions))
    (or pyim-process-force-input-chinese
        (cl-some (lambda (x)
                   (when (functionp x)
                     (funcall x)))
                 (cond ((functionp func-or-list) (list func-or-list))
                       ((listp func-or-list) func-or-list)
                       (t nil))))))

(defun pyim-process-input-chinese-p ()
  "确定 pyim 是否需要启动中文输入模式."
  (let* ((scheme (pyim-scheme-current))
         (first-chars (pyim-scheme-first-chars scheme))
         (rest-chars (pyim-scheme-rest-chars scheme))
         (entered (pyim-entered-get 'point-before)))
    (and (pyim-process-input-chinese-predicate-1)
         (pyim-process-input-chinese-predicate-2
          last-command-event entered first-chars rest-chars))))

(defun pyim-process-input-chinese-predicate-1 ()
  "`pyim-process-input-chinese-p' 内部函数，测试环境。"
  (or (pyim-process-force-input-chinese-p)
      (and (not pyim-process-input-ascii)
           (not (pyim-process-auto-switch-english-input-p)))))

(defun pyim-process-input-chinese-predicate-2 (event entered first-chars rest-chars)
  "`pyim-process-input-chinese-p' 内部函数，测试输入。"
  (if (not (string< "" entered))
      (member event (mapcar #'identity first-chars))
    (member event (mapcar #'identity rest-chars))))

(defun pyim-process-indicator-function ()
  "Indicator function."
  (pyim-process-input-chinese-predicate-1))

(defun pyim-process-run ()
  "查询 entered 字符串, 显示备选词等待用户选择。"
  (if (= (length (pyim-entered-get 'point-before)) 0)
      (pyim-process-terminate)
    (let* ((scheme (pyim-scheme-current))
           entered-to-translate)
      (setq entered-to-translate
            (pyim-entered-get 'point-before))
      (setq pyim-process-imobjs (pyim-imobjs-create entered-to-translate scheme))
      (setq pyim-process-candidates
            (or (delete-dups (pyim-candidates-create pyim-process-imobjs scheme))
                (list entered-to-translate)))
      (unless (eq (pyim-process-auto-select) 'auto-select-success)
        (setq pyim-process-candidate-position 1)
        (pyim-process-ui-refresh)
        (pyim-process-run-delay)))))

(defun pyim-process-auto-select ()
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
            (pyim-process-autoselector-results)))
         (select-last-word
          (pyim-process-autoselector-find-result results 'last))
         (select-current-word
          (pyim-process-autoselector-find-result results 'current)))
    (when (or select-last-word select-current-word)
      (let* ((str (plist-get (if select-last-word
                                 select-last-word
                               select-current-word)
                             :replace-with))
             (candidates (if select-last-word
                             pyim-process-candidates-last
                           pyim-process-candidates))
             (pyim-process-candidates
              (if (and str (stringp str))
                  (list str)
                candidates)))
        (pyim-process-outcome-handle 'candidate)
        (pyim-process-create-word (pyim-process-get-outcome) t))
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

(defun pyim-process-autoselector-results ()
  "运行所有 autoselectors, 返回结果列表。"
  (mapcar (lambda (x)
            (when (functionp x)
              (ignore-errors
                (funcall x))))
          (cl-remove-duplicates
           pyim-process-autoselector
           :from-end t
           :test #'equal)))

(defun pyim-process-self-insert-command-p (cmd)
  "测试 CMD 是否是一个 pyim self insert command."
  (member cmd pyim-process-self-insert-commands))

(defun pyim-process-autoselector-find-result (results type)
  "从所有 autoselector 运行结果中，寻找返回类型为 TYPE 的结果。"
  (cl-find-if (lambda (x)
                (equal (plist-get x :select) type))
              results))

(defun pyim-process-ui-refresh (&optional hightlight-current)
  "刷新 pyim 相关 UI."
  (run-hook-with-args 'pyim-process-ui-refresh-hook hightlight-current))

(defun pyim-process-run-delay ()
  "运行延迟获取候选词流程。

当用户输入停顿时间超过 `pyim-process-run-delay' 这个阈值时，就激
活延迟获取候选词流程，目前，延迟获取候选词有两种处理模式：

1. 同步+限时+用户抢断模式：比如：搜索 buffer 词条等。
2. 异步模式：比如：调用云输入法接口等。

注意：按理说，两种模式的延时阈值应该单独设置的，但当前 pyim 没有
将其分开，因为这样做在满足当前需求的同时，可以简化代码，如果以后
有新功能必须将其分开时，再做考虑。"
  (pyim-process-run-delay-timer-reset)
  (setq pyim-process-run-delay-timer
        (run-with-timer
         pyim-process-run-delay
         nil #'pyim-process-run-delay-timer-function)))

(defun pyim-process-run-delay-timer-reset ()
  "Reset `pyim-process-run-delay-timer'."
  (when pyim-process-run-delay-timer
    (cancel-timer pyim-process-run-delay-timer)
    (setq pyim-process-run-delay-timer nil)))

(defun pyim-process-run-delay-timer-function ()
  "Function used by `pyim-process-run-delay-timer'"
  (pyim-process-handle-candidates-async)
  (pyim-process-handle-candidates-limit-time))

(defun pyim-process-handle-candidates-limit-time ()
  "使用限时的方式获取候选词。"
  (let* ((scheme (pyim-scheme-current))
         (words (pyim-candidates-create-limit-time
                 pyim-process-imobjs scheme)))
    (when words
      (setq pyim-process-candidates
            (pyim-process-merge-candidates words pyim-process-candidates))
      (pyim-process-ui-refresh))))

(defun pyim-process-merge-candidates (new-candidates old-candidates)
  "将 OLD-CANDIDATES 和 NEW-CANDIDATES 合并的默认策略。"
  (remove nil (delete-dups
               `(,(car old-candidates)
                 ,@new-candidates
                 ,@(cdr old-candidates)))))

(defun pyim-process-handle-candidates-async ()
  "使用异步的方式获取候选词条词条。"
  (let ((scheme (pyim-scheme-current))
        (buffer (current-buffer)))
    (pyim-candidates-create-async
     pyim-process-imobjs scheme
     (lambda (async-return)
       (with-current-buffer buffer
         (when (and pyim-process-translating
                    (not (input-pending-p))
                    (equal (car async-return) pyim-process-imobjs))
           (setq pyim-process-candidates
                 (pyim-process-merge-candidates (cdr async-return) pyim-process-candidates))
           (pyim-process-ui-refresh)))))))

(defun pyim-process-get-candidates ()
  pyim-process-candidates)

(defun pyim-process-get-last-candidates ()
  pyim-process-candidates-last)

(defun pyim-process-update-last-candidates ()
  (setq pyim-process-candidates-last pyim-process-candidates))

(defun pyim-process-get-candidate-position ()
  pyim-process-candidate-position)

(defun pyim-process-candidates-length ()
  (length pyim-process-candidates))

(defun pyim-process-set-candidate-position (n)
  (setq pyim-process-candidate-position n))

(defun pyim-process-get-first-imobj ()
  (car pyim-process-imobjs))

(defun pyim-process-select-subword-p ()
  pyim-outcome-subword-info)

(defun pyim-process-get-outcome-subword-info ()
  pyim-outcome-subword-info)

(defun pyim-process-toggle-set-subword-info (n)
  (if (member n pyim-outcome-subword-info)
      (setq pyim-outcome-subword-info
            (remove n pyim-outcome-subword-info))
    (push n pyim-outcome-subword-info)))

(defun pyim-process-get-outcome (&optional n magic-convert use-subword)
  "PYIM 流程的输出"
  (let ((str (pyim-outcome-get n)))
    (when use-subword
      (setq str (pyim-outcome-get-subword str))
      (setq pyim-outcome-subword-info nil))
    (when magic-convert
      (setq str (pyim-outcome-magic-convert str)))
    str))

(defun pyim-process-subword-and-magic-convert (string)
  "返回 STRING 以词定字和魔术转换后的新字符串."
  (pyim-outcome-magic-convert
   (pyim-outcome-get-subword string)))

(defun pyim-process-outcome-handle (type)
  "依照 TYPE, 获取 pyim 的 outcome，并将其加入 `pyim-outcome-history'."
  (cond ((not enable-multibyte-characters)
         (pyim-entered-erase-buffer)
         (setq pyim-outcome-history nil)
         (error "Can't input characters in current unibyte buffer"))
        ((equal type "")
         (setq pyim-outcome-history nil))
        ((eq type 'last-char)
         (push
          (concat (pyim-outcome-get)
                  (pyim-process-outcome-handle-char last-command-event))
          pyim-outcome-history))
        ((eq type 'candidate)
         (let ((candidate
                (nth (1- pyim-process-candidate-position)
                     pyim-process-candidates)))
           (push
            (concat (pyim-outcome-get) candidate)
            pyim-outcome-history)))
        ((eq type 'candidate-and-last-char)
         (let ((candidate
                (nth (1- pyim-process-candidate-position)
                     pyim-process-candidates)))
           (push
            (concat (pyim-outcome-get)
                    candidate
                    (pyim-process-outcome-handle-char last-command-event))
            pyim-outcome-history)))
        ((eq type 'pyim-entered)
         (push (pyim-entered-get 'point-before) pyim-outcome-history))
        (t (error "Pyim: invalid outcome"))))

;; Fix compile warn.
(declare-function pyim-create-word-at-point "pyim")
(declare-function pyim-delete-word-at-point "pyim")

(defun pyim-process-outcome-handle-char (char)
  "Pyim 字符转换函数，主要用于处理标点符号.

pyim 在运行过程中调用这个函数来进行标点符号格式的转换。

常用的标点符号数量不多，所以 pyim 没有使用文件而是使用一个变量
`pyim-punctuation-dict' 来设置标点符号对应表，这个变量是一个
alist 列表。"
  (let* ((str (char-to-string char))
         ;; 注意：`str' 是 *待输入* 的字符对应的字符串。
         (str-before-1 (pyim-char-before-to-string 0))
         (str-before-2 (pyim-char-before-to-string 1))
         (str-before-3 (pyim-char-before-to-string 2))
         ;; 从标点词库中搜索与 `str' 对应的标点列表。
         (punc-list (assoc str pyim-punctuation-dict))
         ;; 从标点词库中搜索与 `str-before-1' 对应的标点列表。
         (punc-list-before-1
          (cl-some (lambda (x)
                     (when (member str-before-1 x) x))
                   pyim-punctuation-dict))
         ;; `str-before-1' 在其对应的标点列表中的位置。
         (punc-posit-before-1
          (cl-position str-before-1 punc-list-before-1
                       :test #'equal))
         (trigger (pyim-outcome-get-trigger)))
    (cond
     ;; 空格之前的字符什么也不输入。
     ((< char ? ) "")

     ;; 这个部份与标点符号处理无关，主要用来快速删除用户自定义词条。
     ;; 比如：在一个中文字符串后输入 2-v，可以将光标前两个中文字符
     ;; 组成的字符串，从个人词库删除。
     ((and (eq (char-before) ?-)
           (pyim-string-match-p "[0-9]" str-before-2)
           (pyim-string-match-p "\\cc" str-before-3)
           (equal str trigger))
      (delete-char -2)
      (pyim-delete-word-at-point
       (string-to-number str-before-2))
      "")
     ;; 这个部份与标点符号处理无关，主要用来快速保存用户自定义词条。
     ;; 比如：在一个中文字符串后输入 2v，可以将光标前两个中文字符
     ;; 组成的字符串，保存到个人词库。
     ((and (member (char-before) (number-sequence ?2 ?9))
           (pyim-string-match-p "\\cc" str-before-2)
           (equal str trigger))
      (delete-char -1)
      (pyim-create-word-at-point
       (string-to-number str-before-1))
      "")

     ;; 光标前面的字符为中文字符时，按 v 清洗当前行的内容。
     ((and (not (numberp punc-posit-before-1))
           (pyim-string-match-p "\\cc" str-before-1)
           (equal str trigger)
           (functionp pyim-outcome-trigger-function))
      (funcall pyim-outcome-trigger-function)
      "")

     ;; 关闭标点转换功能时，只插入英文标点。
     ((not (pyim-process-punctuation-full-width-p))
      str)

     ;; 当用户使用 org-mode 以及 markdown 等轻量级标记语言撰写文档时，
     ;; 常常需要输入数字列表，比如：

     ;; 1. item1
     ;; 2. item2
     ;; 3. item3

     ;; 在这种情况下，数字后面输入句号必须是半角句号而不是全角句号，
     ;; pyim 调用 `pyim-process-outcome-handle-char' 时，会检测光标前面的字符，如果这个
     ;; 字符属于 `pyim-punctuation-escape-list' ，pyim 将输入半角标点，
     ;; 具体细节见：`pyim-process-outcome-handle-char'
     ((member (char-before)
              pyim-punctuation-escape-list)
      str)

     ;; 当 `pyim-punctuation-half-width-functions' 中
     ;; 任意一个函数返回值为 t 时，插入英文标点。
     ((cl-some (lambda (x)
                 (if (functionp x)
                     (funcall x char)
                   nil))
               pyim-punctuation-half-width-functions)
      str)

     ;; 当光标前面为英文标点时， 按 `pyim-outcome-trigger'
     ;; 对应的字符后， 自动将其转换为对应的中文标点。
     ((and (numberp punc-posit-before-1)
           (= punc-posit-before-1 0)
           (equal str trigger))
      (pyim-punctuation-translate 'full-width)
      "")

     ;; 当光标前面为中文标点时， 按 `pyim-outcome-trigger'
     ;; 对应的字符后， 自动将其转换为对应的英文标点。
     ((and (numberp punc-posit-before-1)
           (> punc-posit-before-1 0)
           (equal str trigger))
      (pyim-punctuation-translate 'half-width)
      "")

     ;; 正常输入标点符号。
     (punc-list
      (pyim-punctuation-return-proper-punct punc-list))

     ;; 当输入的字符不是标点符号时，原样插入。
     (t str))))

(defun pyim-process-punctuation-full-width-p ()
  "判断是否需要切换到全角标点输入模式

输入标点的样式的改变（全角或者半角）受三个方面影响：

1. 用户是否手动切换了标点样式？
2  用户是否手动切换到英文输入模式？
3. pyim 是否根据环境自动切换到英文输入模式？

三方面的综合结果为： 只要当前的输入模式是英文输入模式，那么输入的
标点符号 *必定* 是半角标点，如果当前输入模式是中文输入模式，那么，
输入标点的样式用户可以使用 `pyim-punctuation-toggle'手动控制，具
体请参考 `pyim-process-punctuation-full-width-p'。"
  (cl-case (car pyim-punctuation-translate-p)
    (yes t)
    (no nil)
    (auto
     ;; 如果用户手动或者根据环境自动切换为英文输入模式，
     ;; 那么标点符号也要切换为半角模式。
     (and (not pyim-process-input-ascii)
          (not (pyim-process-auto-switch-english-input-p))))))

(defun pyim-process-create-code-criteria ()
  "创建 `pyim-process-code-criteria'."
  (setq pyim-process-code-criteria
        (let ((str (string-join
                    (pyim-codes-create (pyim-process-get-first-imobj)
                                       (pyim-scheme-current)))))
          (if (> (length pyim-process-code-criteria)
                 (length str))
              pyim-process-code-criteria
            str))))

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
  (when (and (> (length word) 0)
             ;; NOTE: 十二个汉字及以上的词条，加到个人词库里面用处不大，这是很主
             ;; 观的一个数字，也许应该添加一个配置选项？
             (< (length word) 12)
             (not (pyim-string-match-p "\\CC" word)))
    ;; PYIM 有些功能（比如：以词定字功能）会用到 text property, 保存词条之前将
    ;; text property 去除，防止不必要的数据进入 cache.
    (setq word (substring-no-properties word))
    ;; 以词定字功能使用时，保存的词条应该是定字后的词条。
    (when (pyim-process-select-subword-p)
      (setq word (pyim-outcome-get-subword word)))
    ;; 记录最近创建的词条，用于快速删词功能。
    (setq pyim-process-last-created-words
          (cons word (remove word pyim-process-last-created-words)))
    (let* ((scheme (pyim-scheme-current))
           (code-prefix (pyim-scheme-code-prefix scheme))
           (codes (pyim-cstring-to-codes
                   word scheme
                   (or criteria pyim-process-code-criteria))))
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
           (concat (or code-prefix "") code) prepend)))
      ;; TODO, 排序个人词库?
      ;; 返回 codes 和 word, 用于 message 命令。
      (mapconcat (lambda (code)
                   (format "%s -> %s" (concat (or code-prefix "") code) word))
                 codes "; "))))

(defun pyim-process-delete-word (word)
  (pyim-dcache-delete-word word))

(defun pyim-process-cleanup-input-output ()
  (pyim-entered-erase-buffer)
  (pyim-process-outcome-handle ""))

(defun pyim-process-terminate ()
  "Terminate the translation of the current key."
  (pyim-process-terminate-really (pyim-scheme-current)))

(cl-defgeneric pyim-process-terminate-really (scheme)
  "Terminate the translation of the current key.")

(cl-defmethod pyim-process-terminate-really (_scheme)
  (setq pyim-process-translating nil)
  (pyim-entered-erase-buffer)
  (setq pyim-process-code-criteria nil)
  (setq pyim-process-force-input-chinese nil)
  (setq pyim-process-candidates nil)
  (setq pyim-process-candidates-last nil)
  (pyim-process-run-delay-timer-reset)
  (pyim-process-ui-hide))

(defun pyim-process-ui-hide ()
  "隐藏 pyim 相关 UI."
  (run-hooks 'pyim-process-ui-hide-hook))

;; * Footer
(provide 'pyim-process)

;;; pyim-process.el ends here
