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
(require 'pyim-page)
(require 'pyim-candidates)
(require 'pyim-preview)
(require 'pyim-indicator)
(require 'pyim-outcome)
(require 'pyim-punctuation)
(require 'pyim-autoselector)
(require 'pyim-cstring)
(require 'pyim-magic)

(defcustom pyim-english-input-switch-functions nil
  "让 pyim 开启英文输入功能.

这个变量的取值为一个函数列表，这个函数列表中的任意一个函数的
运行结果为 t 时，pyim 开启英文输入功能。"
  :type 'symbol)

(defcustom pyim-exhibit-delay-ms 0
  "输入或者删除拼音字符后等待多少毫秒后才显示可选词
当用户快速输入连续的拼音时可提升用户体验.
如果为 0 或者 nil, 则不等待立刻显示可选词."
  :type 'integer)

(defvar pyim-process-input-ascii nil
  "是否开启 pyim 英文输入模式.")

(defvar pyim-process-force-input-chinese nil
  "是否强制开启中文输入模式.

这个变量只用于 `pyim-convert-string-at-point', 不要
在其它地方使用。")

(defvar pyim-process-translating nil
  "记录是否在转换状态.")

(defvar pyim-process-last-created-word nil
  "记录最近一次创建的词条， 用于实现快捷删词功能： `pyim-delete-last-word' .")

(defvar pyim-process-run-async-timer nil
  "异步处理 entered 时，使用的 timer.")

(defvar pyim-process-self-insert-commands nil
  "保存所有的 self insert command.")

(defvar pyim-process-run-exhibit-timer nil)

(pyim-register-local-variables
 '(pyim-process-input-ascii
   pyim-process-translating
   pyim-process-last-created-word))

(defun pyim-process-init-dcaches (&optional force save-caches)
  "PYIM 流程，词库相关的初始化工作。"
  (pyim-recreate-local-variables)
  (pyim-pymap-cache-create)
  (pyim-dcache-update force))

(defun pyim-process-save-dcaches (&optional force)
  "PYIM 流程，保存 dcache."
  (when force
    (pyim-dcache-save-caches)))

(defun pyim-process-update-personal-words ()
  (pyim-dcache-call-api 'update-personal-words t))

(defun pyim-process-init-ui ()
  "PYIM 流程，用户界面相关的初始化工作。"
  (pyim-preview-setup-overlay))

(defun pyim-process-start-daemon ()
  "启动 pyim 流程需要的相关 daemon."
  (pyim-indicator-start-daemon #'pyim-process-indicator-function))

(defun pyim-process-stop-daemon ()
  "关闭 pyim 流程已经启动的 daemon."
  (interactive)
  ;; 只有其它的 buffer 中没有启动 pyim 时，才停止 daemon.
  ;; 因为 daemon 是服务所有 buffer 的。
  (unless (cl-find-if
           (lambda (buf)
             (buffer-local-value 'current-input-method buf))
           (remove (current-buffer) (buffer-list)))
    (pyim-indicator-stop-daemon)))

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

(defun pyim-process-auto-switch-english-input-p ()
  "判断是否 *根据环境自动切换* 为英文输入模式，这个函数处理变量：
`pyim-english-input-switch-functions'"
  (let* ((func-or-list pyim-english-input-switch-functions))
    (and (cl-some (lambda (x)
                    (if (functionp x)
                        (funcall x)
                      nil))
                  (cond ((functionp func-or-list) (list func-or-list))
                        ((listp func-or-list) func-or-list)
                        (t nil))))))

(defun pyim-process-input-chinese-p ()
  "确定 pyim 是否需要启动中文输入模式."
  (let* ((scheme-name (pyim-scheme-name))
         (first-chars (pyim-scheme-get-option scheme-name :first-chars))
         (rest-chars (pyim-scheme-get-option scheme-name :rest-chars)))
    (and (or pyim-process-force-input-chinese
             (and (not pyim-process-input-ascii)
                  (not (pyim-process-auto-switch-english-input-p))))
         (if (not (string< "" (pyim-entered-get 'point-before)))
             (member last-command-event
                     (mapcar #'identity first-chars))
           (member last-command-event
                   (mapcar #'identity rest-chars))))))

(defun pyim-process-indicator-function ()
  "Indicator function."
  (or pyim-process-force-input-chinese
      (and (not pyim-process-input-ascii)
           (not (pyim-process-auto-switch-english-input-p)))))

(defun pyim-process-run (&optional no-delay)
  "延迟 `pyim-exhibit-delay-ms' 显示备选词等待用户选择。"
  (if (= (length (pyim-entered-get 'point-before)) 0)
      (pyim-process-terminate)
    (when pyim-process-run-exhibit-timer
      (cancel-timer pyim-process-run-exhibit-timer))
    (cond
     ((or no-delay
          (not pyim-exhibit-delay-ms)
          (eq pyim-exhibit-delay-ms 0))
      (pyim-process-run-1))
     (t (setq pyim-process-run-exhibit-timer
              (run-with-timer (/ pyim-exhibit-delay-ms 1000.0)
                              nil
                              #'pyim-process-run-1))))))

(defun pyim-process-run-1 ()
  "查询 `pyim-entered-buffer' 光标前的拼音字符串（如果光标在行首则为光标后的）, 显示备选词等待用户选择。"
  (let* ((scheme-name (pyim-scheme-name))
         entered-to-translate)
    (setq entered-to-translate
          (pyim-entered-get 'point-before))
    (setq pyim-imobjs (pyim-imobjs-create entered-to-translate scheme-name))
    (setq pyim-candidates
          (or (delete-dups (pyim-candidates-create pyim-imobjs scheme-name))
              (list entered-to-translate)))
    (pyim-process-run-async-timer-reset)
    ;; 延迟1秒异步处理 entered, pyim 内置的输入法目前不使用异步获取
    ;; 词条的方式，主要用于 pyim-liberime 支持。
    (setq pyim-process-run-async-timer
          (run-with-timer
           1 nil
           (lambda ()
             (if (functionp 'make-thread)
                 (make-thread #'pyim-process-run-with-thread
                              "pyim-process-run-with-thread")
               (pyim-process-run-with-thread)))))
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
         (pyim-process-self-insert-command-p this-command)
         (cl-find-if (lambda (x)
                       (setq result x)
                       (equal (plist-get x :select) 'last))
                     autoselector-results))
        (let* ((str (plist-get result :replace-with))
               (pyim-candidates
                (if (and str (stringp str))
                    (list str)
                  pyim-candidates-last)))
          (pyim-process-outcome-handle 'candidate))
        ;; autoselector 机制已经触发的时候，如果发现 entered buffer 中
        ;; point 后面还有未处理的输入，就将其转到下一轮处理，这种情况
        ;; 很少出现，一般是型码输入法，entered 编辑的时候有可能触发。
        (pyim-add-unread-command-events
         (pyim-entered-get 'point-after))
        (pyim-add-unread-command-events last-command-event)
        (pyim-process-terminate))
       ;; 假设用户已经输入 "niha", 然后按了 "o" 键，那么，当前
       ;; entered 就是 "nihao". 如果 autoselector 函数返回一个 list:
       ;; (:select current), 那么就直接将 "nihao" 对应的第一个候选词
       ;; 上屏幕。
       ((and (pyim-process-self-insert-command-p this-command)
             (cl-find-if (lambda (x)
                           (setq result x)
                           (equal (plist-get x :select) 'current))
                         autoselector-results))
        (let* ((str (plist-get result :replace-with))
               (pyim-candidates
                (if (and str (stringp str))
                    (list str)
                  pyim-candidates)))
          (pyim-process-outcome-handle 'candidate))
        (pyim-add-unread-command-events
         (pyim-entered-get 'point-after))
        (pyim-process-terminate))
       (t (setq pyim-candidate-position 1)
          (pyim-preview-refresh)
          (pyim-page-refresh))))))

(defun pyim-process-self-insert-command-p (cmd)
  "测试 CMD 是否是一个 pyim self insert command."
  (member cmd pyim-process-self-insert-commands))

(defun pyim-process-run-with-thread ()
  "Function used by `pyim-process-run-async-timer'"
  (let* ((scheme-name (pyim-scheme-name))
         (words (delete-dups (pyim-candidates-create pyim-imobjs scheme-name t))))
    (when words
      (setq pyim-candidates words)
      (pyim-preview-refresh)
      ;; NEED HELP: 目前只有 posframe 和 minibufer 可以正确处理异步刷新 page
      (when (and (member pyim-page-tooltip '(posframe minibuffer))
                 (not (eq (selected-window) (minibuffer-window))))
        (pyim-page-refresh)))))

(defun pyim-process-run-async-timer-reset ()
  "Reset `pyim-process-run-async-timer'."
  (when pyim-process-run-async-timer
    (cancel-timer pyim-process-run-async-timer)
    (setq pyim-process-run-async-timer nil)))

(defun pyim-process-get-candidates ()
  pyim-candidates)

(defun pyim-process-set-candidate-position (n)
  (setq pyim-candidate-position n))

(defun pyim-process-get-imobjs ()
  pyim-imobjs)

(defun pyim-process-get-outcome (&optional n magic-convert)
  "PYIM 流程的输出"
  (if magic-convert
      (pyim-magic-convert (pyim-outcome-get n))
    (pyim-outcome-get n)))

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
         (let* ((candidate
                 (pyim-candidate-parse
                  (nth (1- pyim-candidate-position)
                       pyim-candidates))))
           (push
            (concat (pyim-outcome-get) candidate)
            pyim-outcome-history)))
        ((eq type 'candidate-and-last-char)
         (let* ((candidate
                 (pyim-candidate-parse
                  (nth (1- pyim-candidate-position)
                       pyim-candidates))))
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
  "创建 `pyim-cstring-to-code-criteria'."
  (setq pyim-cstring-to-code-criteria
        (let ((str (mapconcat #'identity
                              (pyim-codes-create (car (pyim-process-get-imobjs)) (pyim-scheme-name))
                              "")))
          (if (> (length pyim-cstring-to-code-criteria)
                 (length str))
              pyim-cstring-to-code-criteria
            str))))

(defun pyim-process-create-word (word &optional prepend wordcount-handler)
  "将中文词条 WORD 添加编码后，保存到用户选择过的词生成的缓存中。

词条 WORD 默认会追加到已有词条的后面，如果 PREPEND 设置为 t,
词条就会放到已有词条的最前面。

这是函数会调用 `pyim-cstring-to-codes' 来获取中文词条对应的编码。

WORDCOUNT-HANDLER 可以是一个数字，代表将此数字设置为 WORD 的新词频，
WORDCOUNT-HANDLER 也可以是一个函数，其返回值将设置为 WORD 的新词频，
而这个函数的参数则表示 WORD 当前词频，这个功能用于：`pyim-dcache-import',
如果 WORDCOUNT-HANDLER 设置为其他, 则表示让 WORD 当前词频加1.

BUG：拼音无法有效地处理多音字。"
  (when (and (> (length word) 0)
             (< (length word) 11) ;十个汉字以上的词条，加到个人词库里面用处不大，忽略。
             (not (pyim-string-match-p "\\CC" word)))
    ;; 记录最近创建的词条，用于快速删词功能。
    (setq pyim-process-last-created-word word)
    (let* ((scheme-name (pyim-scheme-name))
           (code-prefix (pyim-scheme-get-option scheme-name :code-prefix))
           (codes (pyim-cstring-to-codes word scheme-name pyim-cstring-to-code-criteria)))
      ;; 保存对应词条的词频
      (when (> (length word) 0)
        (pyim-dcache-update-iword2count word prepend wordcount-handler))
      ;; 添加词条到个人缓存
      (dolist (code codes)
        (unless (pyim-string-match-p "[^ a-z-]" code)
          (pyim-dcache-insert-icode2word
           word (concat (or code-prefix "") code) prepend)))
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

(defalias 'pyim-terminate-translation #'pyim-process-terminate)
(defun pyim-process-terminate ()
  "Terminate the translation of the current key."
  (setq pyim-process-translating nil)
  (pyim-entered-erase-buffer)
  (setq pyim-process-force-input-chinese nil)
  (setq pyim-candidates nil)
  (setq pyim-candidates-last nil)
  (pyim-preview-delete-string)
  (pyim-page-hide)
  (setq pyim-cstring-to-code-criteria nil)
  (pyim-process-run-async-timer-reset)
  (let* ((class (pyim-scheme-get-option (pyim-scheme-name) :class))
         (func (intern (format "pyim-process-terminate:%S" class)))
         ;; `pyim-process-terminate' 以前叫 pyim-terminate-translation, 兼容以前的名称。
         (func-old (intern (format "pyim-terminate-translation:%S" class))))
    (cond ((and class (functionp func))
           (funcall func))
          ((and class (functionp func-old))
           (funcall func))
          (t nil))))

;; * Footer
(provide 'pyim-process)

;;; pyim-process.el ends here
