;;; pyim-page.el --- page lib for pyim.        -*- lexical-binding: t; -*-

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
(require 'posframe nil t)
(require 'pyim-common)
(require 'pyim-process)

(defgroup pyim-page nil
  "Page tools for pyim."
  :group 'pyim)

(defcustom pyim-page-length 5
  "每页显示的词条数目.

细节信息请参考 `pyim-page--refresh' 的 docstring."
  :type 'number)

(define-widget 'pyim-page-tooltip-1 'lazy nil
  :type '(choice (const posframe)
                 (const popup)
                 (const popon)
                 (const minibuffer)))

(define-widget 'pyim-page-tooltip 'lazy
  "如何绘制 pyim 选词框，详见变量 `pyim-page-tooltip'。"
  :type '(choice (pyim-page-tooltip-1 :tag "单一选词框")
                 (repeat :tag "依次选择可用选词框" pyim-page-tooltip-1)))

(defcustom pyim-page-tooltip '(posframe popup minibuffer)
  "如何绘制 pyim 选词框.

1. 当这个变量取值为 posframe 时，使用 posframe 包来绘制选词框，
   使用 Emacs 26 图形版的用户推荐使用这个选项。
2. 当这个变量取值为 popup 时，使用 popup-el 包来绘制选词框，
   这个选项可以在 Emacs 图形版和终端版使用，速度没有 posframe 快，
   偶尔会遇到选词框错位的问题。
3. 当这个变量取值为 popon 时，使用 popon 包来绘制选词框，这个选项
   效果类似 popup。
4. 当这个变量取值为 minibuffer 时，minibuffer 将做为选词框，
   这个选项也作为其他选项不可用时的 fallback.
5. 当这个变量的取值是为一个 list 时，pyim 将按照优先顺序动态
   选择一个当前环境可用的 tooltip."
  :type 'pyim-page-tooltip)

(define-widget 'pyim-page-style 'lazy "选词框的格式"
  :type '(choice (const :tag "单行选词框" two-lines)
                 (const :tag "双行选词框" one-line)
                 (const :tag "垂直选词框" vertical)
                 (const :tag "单行选词框 (minibuffer)" minibuffer)))

(defcustom pyim-page-style 'two-lines
  "这个变量用来控制选词框的格式.

pyim 内建的有四种选词框格式：

1. one-line    单行选词框
2. two-lines   双行选词框
3. vertical    垂直选词框
4. minibuffer  单行选词框 (minibuffer 中专用)"
  :type 'pyim-page-style)

(defcustom pyim-page-tooltip-style-alist
  '((minibuffer . minibuffer))
  "pyim page tooltip 专用 page style 绑定设置表。

这个表是一个 alist, 每个元素的 car 代表 tooltip, cdr 代表对应的
page style."
  :type '(alist :key-type pyim-page-tooltip-1
                :value-type pyim-page-style))

(defcustom pyim-page-posframe-border-width 0
  "posframe的内间距。
只有当用户使用 posframe 来显示候选词时才有效。"
  :type 'integer)

(defcustom pyim-page-posframe-min-width (* pyim-page-length 7)
  "使用 posframe 做为选词框时，设置选词框的最小宽度."
  :type 'integer)

(defcustom pyim-page-minibuffer-separator nil
  "在 minibuffer 中使用 pyim 时，preview 和 page 之间的分割字符串。"
  :type '(choice (const :tag "No user defined separator" nil) string))

(defface pyim-page
  '((t (:inherit default :background "#333333" :foreground "#dcdccc")))
  "Face used for the pyim page.")

(defface pyim-page-border
  '((t (:inherit pyim-page :background "gray")))
  "Face used for the pyim page border.
Only useful when use posframe.")

(defface pyim-page-selection
  '((t (:background "gray44")))
  "选词框中已选词条的 face.")

(defvar pyim-page-tooltip-infos
  '((posframe   :package posframe :test posframe-workable-p)
    (popup      :package popup)
    (popon      :package popon)
    (minibuffer :package minibuffer))
  "pyim-page tooltip 相关信息。

用于函数 `pyim-page--tooltip-valid-p'.")

(defun pyim-page--refresh (&optional hightlight-current)
  "刷新 page 页面的函数.

这个函数主要用来处理选词框选词后，刷新显示问题，

\"nihao\" 对应的 *待选词列表* 类似：

     (\"你好\" \"倪皓\" \"泥\" \"你\"  ...  \"慝\")

*待选词列表* 一般都很长，不可能在一页中完全显示，所以 pyim 使用了
page 的概念，比如，上面的 “nihao” 的 *待选词列表* 就可以逻辑的
分成5页：


  第1页   你好  倪皓  泥  你  呢  拟  逆  腻  妮
  第2页   怩    溺    尼  禰  齯  麑  鲵  蜺  衵
  第3页   薿    旎    睨  铌  昵  匿  倪  霓  暱
  第4页   柅    猊    郳  輗  坭  惄  堄  儗  伲
  第5页   祢    慝

`pyim-process-word-position' 的返回值以及 `pyim-page-length' 的设
定值（默认设置为9），共同决定了 pyim 需要显示哪一页，比如：

  第1页  你好  倪皓   泥    你  呢  拟  逆  腻  妮
  第2页  怩    溺     尼    禰  齯  麑  鲵  蜺  衵
  第3页  薿[B] 旎     睨[A] 铌  昵  匿  倪  霓  暱[E]
  第4页  柅    猊     郳    輗  坭  惄  堄  儗  伲
  第5页  祢    慝

假设当前选择的词条为 \"睨\", 那么 `pyim-process-word-position'
的返回值为 A 所在的位置。那么：

1. 函数 `pyim-page--current-page' 返回值为3， 说明当前 page 为第3页。
2. 函数 `pyim-page--total-page'  返回值为5，说明 page 共有5页。
3. 函数 `pyim-page--start' 返回 B 所在的位置。
4. 函数 `pyim-page--end' 返回 E 所在的位置。
5. 函数 `pyim-page--refresh' 会从待选词条列表中提取一个 sublist:

     (\"薿\" \"旎\" \"睨\" \"铌\" \"昵\" \"匿\" \"倪\" \"霓\" \"暱\")

这个 sublist 的起点为 `pyim-page--start' 的返回值，终点为
`pyim-page--end' 的返回值。并保存到一个 plist 的 :candidates 对应
的位置，这个 plist 最终会做为参数传递给 `pyim-page-style' 相关的
函数，用于生成用于在选词框中显示的字符串。"
  (let* ((candidates-showed (pyim-page--get-showed-candidates))
         (position (pyim-page--word-position-in-current-page))
         (tooltip (pyim-page--get-valid-tooltip))
         (style (pyim-page--get-page-style tooltip))
         (page-info
          (list :scheme (pyim-scheme-current)
                :current-page (pyim-page--current-page)
                :total-page (pyim-page--total-page)
                :candidates candidates-showed
                :position position
                :hightlight-current hightlight-current
                :assistant-enable (pyim-scheme-assistant-enable-p))))
    ;; Show page.
    (when (and (null unread-command-events)
               (null unread-post-input-method-events))
      (pyim-page-show
       (pyim-page-info-format style page-info)
       (pyim-process-ui-position)
       tooltip))))

(add-hook 'pyim-process-ui-refresh-hook #'pyim-page--refresh)

(defun pyim-page--get-showed-candidates ()
  "从 CANDIDATES 中获取当前 page 显示需要显示的部分内容。"
  (mapcar (lambda (x)
            (let ((comment (get-text-property 0 :comment x)))
              (if comment
                  (concat x comment)
                x)))
          (cl-subseq (pyim-process-get-candidates)
                     (pyim-page--start)
                     (pyim-page--end))))

(defun pyim-page--start (&optional position)
  "计算当前所在页的第一个词条的位置.

细节信息请参考 `pyim-page--refresh' 的 docstring."
  (* (/ (pyim-process-word-position position) pyim-page-length)
     pyim-page-length))

(defun pyim-page--end ()
  "计算当前所在页的最后一个词条的位置，

细节信息请参考 `pyim-page--refresh' 的 docstring."
  (let* ((whole (pyim-process-candidates-length))
         (len pyim-page-length)
         (pos (pyim-process-word-position))
         (last (* (/ (+ pos len) len) len)))
    (if (< last whole)
        last
      whole)))

(defun pyim-page--word-position-in-current-page ()
  "获取当前选择的词条在在当前 page 中的位置。"
  (- (pyim-process-word-position)
     (pyim-page--start)))

(defun pyim-page--get-valid-tooltip ()
  "根据当前环境，获取一个可用的 tooltip."
  (cond
   ;; NOTE: 以前在 minibuffer 中试用过 posframe, linux 环境下运行效果还不错，但
   ;; 在 windows 环境下，似乎有很严重的性能问题，原因未知。
   ((eq (selected-window) (minibuffer-window)) 'minibuffer)
   ;; 在 exwm-xim 环境下输入中文时，只能使用 minibuffer, 因为应用窗口遮挡的缘故，
   ;; 其它方式不可用。
   ((pyim-exwm-xim-environment-p) 'minibuffer)
   (t (or (cl-find-if #'pyim-page--tooltip-valid-p
                      (if (listp pyim-page-tooltip)
                          pyim-page-tooltip
                        (list pyim-page-tooltip)))
          'minibuffer))))

(defun pyim-page--tooltip-valid-p (tooltip)
  "测试 TOOLTIP 当前是否可用。"
  (let* ((info (alist-get tooltip pyim-page-tooltip-infos))
         (package (plist-get info :package))
         (test-func (plist-get info :test)))
    (cond
     ((not (featurep package)) nil)
     ((not (functionp test-func)) t)
     ((and (functionp test-func)
           (funcall test-func)) t)
     (t nil))))

(defun pyim-page--get-page-style (tooltip)
  "依照 TOOLTIP 和 `pyim-page-style', 得到一个 page style."
  (or (cdr (assoc tooltip pyim-page-tooltip-style-alist))
      pyim-page-style))

(defun pyim-page--current-page ()
  "计算当前选择的词条在第几页面.

细节信息请参考 `pyim-page--refresh' 的 docstring."
  (1+ (/ (pyim-process-word-position) pyim-page-length)))

(defun pyim-page--total-page ()
  "计算 page 总共有多少页.

细节信息请参考 `pyim-page--refresh' 的 docstring."
  (let* ((length (pyim-process-candidates-length))
         (n (/ length pyim-page-length)))
    (if (> length (* n pyim-page-length))
        (1+ n)
      n)))

(cl-defgeneric pyim-page-show (string position tooltip)
  "在 POSITION 位置，使用 TOOLTIP 显示字符串 STRING.

注意事项： pyim-page 将背景颜色设置功能放置在 `pyim-page-show' ，
因为在 `pyim-page-info-format' 中实现此功能，需要计算字符串宽度和
补充空格，添加 text propertize, 过程相当啰嗦，而背景颜色设置不是
pyim-page 的核心的功能，为此增加代码的复杂度和测试的难度感觉得不
偿失。

所以我们的选择是：尽量选择支持背景颜色设置的 tooltip, 如果不支持，
就放弃这个功能。")

(defvar pyim-page--posframe-buffer " *pyim-page--posframe-buffer*"
  "这个变量用来保存做为 page tooltip 的 posframe 的 buffer.")

(cl-defmethod pyim-page-show (string position (_tooltip (eql posframe)))
  "在 POSITION 位置，使用 posframe STRING."
  (posframe-show pyim-page--posframe-buffer
                 :string string
                 :position position
                 :min-width pyim-page-posframe-min-width
                 :background-color (face-attribute 'pyim-page :background)
                 :foreground-color (face-attribute 'pyim-page :foreground)
                 :border-width pyim-page-posframe-border-width
                 :border-color (face-attribute 'pyim-page-border :background)))

(defvar pyim-page--minibuffer-string nil
  "函数 `pyim-page-show-with-minibuffer' 上一次处理的消息字符串。")

(cl-defmethod pyim-page-show (string _position (_tooltip (eql minibuffer)))
  "使用 minibuffer 来显示 STRING。"
  (let ((max-mini-window-height (+ pyim-page-length 2))
        (message-log-max nil))
    (if (not (eq (selected-window) (minibuffer-window)))
        (message string)
      (message nil)
      ;; 在类似 vertico-posframe 这样的环境中，posframe window-point 同步问题不
      ;; 太好处理，这里使用一个简单粗暴的方式：在输入过程中，隐藏真实的 cursor
      ;; 并显示一个伪 cursor, 输入完成之后再恢复。
      (setq-local cursor-type nil)

      ;; 异步获取词条的时候，上一次的 page 字符串可能还在 Minibuffer 中，所以首
      ;; 先要将其去除，否则会出现两个 page.
      (delete-char (length pyim-page--minibuffer-string))
      (save-excursion
        (insert
         (setq pyim-page--minibuffer-string
               (concat
                ;; 显示一个伪 cursor.
                (propertize " " 'face 'cursor)
                (or pyim-page-minibuffer-separator
                    (let* ((width (string-width (buffer-string)))
                           (n (- (* 20 (+ 1 (/ width 20))) width)))
                      (make-string n ?\ )))
                string)))))))

(declare-function popup-tip "popup")
(declare-function popup-delete "popup")
(defvar popup-version)

(defvar pyim-page--popup nil
  "这个变量用来保存做为 page tooltip 的 popup.")

(cl-defmethod pyim-page-show (string position (_tooltip (eql popup)))
  "Show STRING at POSITION with the help of popup-el."
  (when pyim-page--popup
    ;; 延迟获取词条的时候，如果不把已经存在的 popup 删除，就会出现两个 page.
    (popup-delete pyim-page--popup))
  (setq pyim-page--popup
        (apply #'popup-tip string
               :point position :around t :nowait t :nostrip t
               ;; popup v0.5.9 以后才支持 face 参数
               (unless (version<= popup-version "0.5.8")
                 (list :face 'pyim-page)))))

(declare-function popon-create "popon")
(declare-function popon-kill "popon")
(declare-function popon-x-y-at-pos "popon")

(defvar pyim-page--popon nil
  "这个变量用来保存做为 page tooltip 的 popon.")

(cl-defmethod pyim-page-show (string position (_tooltip (eql popon)))
  "Show STRING at POSITION with the help of popon."
  (when pyim-page--popon
    ;; 延迟获取词条的时候，如果不把已经存在的 popon 删除，就会出现两个 page.
    (popon-kill pyim-page--popon))
  (let* ((x-y (popon-x-y-at-pos position))
         (x (car x-y))
         (y (cdr x-y)))
    (setq pyim-page--popon
          (popon-create
           (pyim-page--add-default-page-face
            (pyim-page--align-lines string))
           (cons x (+ y 1))))))

(defun pyim-page--add-default-page-face (string)
  "为 STRING 添加默认 page face."
  (with-temp-buffer
    (insert string)
    (add-face-text-property
     (point-min) (point-max) 'pyim-page t)
    (buffer-string)))

(defun pyim-page--align-lines (string)
  "用空格将 STRING 的每一行都对齐。"
  (let* ((lines (split-string string "\n"))
         (widths (mapcar #'string-width lines))
         (max-width (apply #'max widths))
         (new-lines
          (mapcar (lambda (line)
                    (concat line
                            (make-string
                             (- max-width (string-width line))
                             ?\ )))
                  lines)))
    (string-join new-lines "\n")))

(cl-defgeneric pyim-page-info-format (style page-info)
  "将 PAGE-INFO 按照 STYLE 格式化为选词框中显示的字符串。")

(cl-defmethod pyim-page-info-format (_style page-info)
  "将 PAGE-INFO 格式化为选词框中显示的字符串.

样式类似：

+----------------------------+
| ni hao [1/9]               |
| 1.你好 2.你号 ...          |
+----------------------------+"
  (format "=> %s%s [%s/%s]: \n%s"
          (pyim-page-preview-create
           (plist-get page-info :scheme))
          (if (plist-get page-info :assistant-enable) " (辅)" "")
          (plist-get page-info :current-page)
          (plist-get page-info :total-page)
          (pyim-page-menu-create
           (plist-get page-info :candidates)
           (plist-get page-info :position)
           nil
           (plist-get page-info :hightlight-current))))

(cl-defmethod pyim-page-info-format ((_style (eql one-line)) page-info)
  "将 PAGE-INFO 格式化为选词框中显示的字符串.

样式类似：

+-----------------------------------+
| [ni hao]: 1.你好 2.你号 ... (1/9) |
+-----------------------------------+"
  (format "[%s%s]: %s(%s/%s)"
          (pyim-page-preview-create
           (plist-get page-info :scheme) " ")
          (if (plist-get page-info :assistant-enable) " (辅)" "")
          (pyim-page-menu-create
           (plist-get page-info :candidates)
           (plist-get page-info :position)
           nil
           (plist-get page-info :hightlight-current))
          (plist-get page-info :current-page)
          (plist-get page-info :total-page)))

(cl-defmethod pyim-page-info-format ((_style (eql vertical)) page-info)
  "将 PAGE-INFO 格式化为选词框中显示的字符串.

样式类似：

+--------------+
| ni hao [1/9] |
| 1.你好       |
| 2.你号 ...   |
+--------------+"
  (format "=> %s%s [%s/%s]: \n%s"
          (pyim-page-preview-create
           (plist-get page-info :scheme))
          (if (plist-get page-info :assistant-enable) " (辅)" "")
          (plist-get page-info :current-page)
          (plist-get page-info :total-page)
          (pyim-page-menu-create
           (plist-get page-info :candidates)
           (plist-get page-info :position)
           "\n"
           (plist-get page-info :hightlight-current))))

(cl-defmethod pyim-page-info-format ((_style (eql minibuffer)) page-info)
  "将 PAGE-INFO 格式化为选词框中显示的字符串.

样式类似：

+------------------------------------+
| [ni hao]: 1.你好 2.你号 ...  (1/9) |
+------------------------------------+"
  ;; 在 minibuffer 中显示 page 的时候，page 字符串直接插入到 minibuffer 现有的内
  ;; 容中, 为了便于区分，在 page 后面添加一个显眼的字符。
  (format "[%-15s%s]: %s(%s/%s) $ "
          (pyim-page-preview-create
           (plist-get page-info :scheme))
          (if (plist-get page-info :assistant-enable) " (辅)" "")
          (pyim-page-menu-create
           (plist-get page-info :candidates)
           (plist-get page-info :position)
           nil
           (plist-get page-info :hightlight-current))
          (plist-get page-info :current-page)
          (plist-get page-info :total-page)))

(cl-defgeneric pyim-page-preview-create (scheme &optional separator)
  "这个函数用于创建在 page 中显示的预览字符串。

这个预览是在 page 中显示，而 `pyim-preview--refresh' 对应的预览
是在 buffer 光标处显示，两者要做区别。")

(cl-defmethod pyim-page-preview-create ((_scheme pyim-scheme-quanpin) &optional separator)
  (let* ((separator (or separator " "))
         (translated (string-join (mapcar (lambda (w)
                                            (concat (nth 0 w) (nth 1 w)))
                                          (pyim-process-get-first-imobj))
                                  separator)))
    (concat
     ;; | 显示光标位置的字符
     (pyim-process-with-entered-buffer
       (if (equal 1 (point))
           (concat "|" translated)
         (concat (replace-regexp-in-string (concat separator "'") "'" translated)
                 " |" (buffer-substring-no-properties (point) (point-max)))))
     ;; 使用辅助输入法时，在 page 中提示默认输入法的 code, 这个功能对形码用户挺
     ;; 有用。
     (pyim-page--code-hint-of-default-scheme))))

(defun pyim-page--code-hint-of-default-scheme ()
  "获取当前词条在默认输入法下的 code 提示."
  (when (pyim-scheme-assistant-enable-p)
    (let* ((word (nth (pyim-process-word-position)
                      (pyim-process-get-candidates)))
           (codes (sort (pyim-cstring-to-codes
                         word (pyim-scheme-get pyim-default-scheme))
                        (lambda (a b)
                          (< (length a) (length b)))))
           (hint (string-join codes " ")))
      (if (> (length hint) 0)
          (format " [%s]" hint)
        " "))))

(cl-defmethod pyim-page-preview-create ((scheme pyim-scheme-shuangpin) &optional separator)
  (let ((keymaps (pyim-scheme-shuangpin-keymaps scheme))
        result)
    (dolist (w (pyim-process-get-first-imobj))
      (let ((sm (nth 0 w))
            (ym (nth 1 w)))
        (if (equal sm "")
            (push (car (rassoc (list ym) keymaps)) result)
          (push
           (concat (cl-some
                    (lambda (x)
                      (when (equal sm (nth 1 x))
                        (car x)))
                    keymaps)
                   (cl-some
                    (lambda (x)
                      (when (or (equal ym (nth 2 x))
                                (equal ym (nth 3 x)))
                        (car x)))
                    keymaps))
           result))))
    (string-join (reverse result) (or separator " "))))

(cl-defmethod pyim-page-preview-create ((_scheme pyim-scheme-xingma) &optional _separator)
  ;; | 显示光标位置的字符
  (pyim-process-with-entered-buffer
    (if (equal (point) (point-max))
        (buffer-substring-no-properties (point-min) (point-max))
      (concat (buffer-substring-no-properties (point-min) (point))
              "| "
              (buffer-substring-no-properties (point) (point-max))))))

(defun pyim-page-menu-create (candidates position &optional separator hightlight-current)
  "这个函数用于创建在 page 中显示的备选词条菜单。"
  (let ((i 0) result)
    (dolist (candidate candidates)
      (let ((str (substring-no-properties
                  (if (consp candidate)
                      (concat (car candidate) (cdr candidate))
                    candidate))))
        (setq i (1+ i))
        ;; 高亮当前选择的词条，用于 `pyim-page-next-word'
        (push
         (if (and hightlight-current
                  (= i (+ position 1)))
             (format "%d%s" i
                     (propertize
                      (format "[%s]" str)
                      'face 'pyim-page-selection))
           (format "%d.%s " i str))
         result)))
    (string-join (nreverse result) (or separator ""))))

(defun pyim-page-plan-to-select-word (num-key)
  "按照 NUM-KEY 预选词条，如果预选不成功，则返回 nil."
  (let ((index (if (numberp num-key)
                   (- num-key 1)
                 0))
        (end (pyim-page--end)))
    (when (= index -1) (setq index 9))
    (when (<= (+ index (pyim-page--start)) end)
      (pyim-process-plan-to-select-word
       (+ (pyim-page--start) index)))))

(defun pyim-page-next-page (arg)
  "Pyim page 翻页命令."
  (interactive "p")
  (if (pyim-process-without-entered-p)
      (pyim-process-select-last-char)
    (pyim-process-plan-to-select-word
     (pyim-page--start
      (pyim-process-next-word-position
       (* pyim-page-length arg))))
    (pyim-process-ui-refresh)))

(defun pyim-page-previous-page (arg)
  (interactive "p")
  (pyim-page-next-page (- arg)))

(defun pyim-page-next-word (arg)
  (interactive "p")
  (if (pyim-process-without-entered-p)
      (pyim-process-select-last-char)
    (pyim-process-plan-to-select-word
     (pyim-process-next-word-position arg))
    (pyim-process-ui-refresh 'hightlight-current)))

(defun pyim-page-previous-word (arg)
  (interactive "p")
  (pyim-page-next-word (- arg)))

(defun pyim-page--hide ()
  "Hide pyim page."
  (pyim-page-hide-tooltip (pyim-page--get-valid-tooltip)))

(cl-defgeneric pyim-page-hide-tooltip (tooltip)
  "Hide TOOLTIP.")

(cl-defmethod pyim-page-hide-tooltip ((_tooltip (eql popup)))
  "Hide popup tooltip."
  (popup-delete pyim-page--popup))

(cl-defmethod pyim-page-hide-tooltip ((_tooltip (eql popon)))
  "Hide popon tooltip."
  (popon-kill pyim-page--popon))

(cl-defmethod pyim-page-hide-tooltip ((_tooltip (eql posframe)))
  "Hide posframe tooltip."
  (posframe-hide pyim-page--posframe-buffer))

(cl-defmethod pyim-page-hide-tooltip ((_tooltip (eql minibuffer)))
  "Hide minibuffer tooltip."
  (when (eq (selected-window) (minibuffer-window))
    ;; 从 minibuffer 中删除 page 字符串。
    (delete-char (length pyim-page--minibuffer-string))
    ;; 在类似 vertico-posframe 这样的环境中，posframe window-point 同步问题
    ;; 不太好处理，这里使用一个简单粗暴的方式：在输入过程中，隐藏真实的
    ;; cursor 并显示一个伪 cursor, 输入完成之后再恢复。
    (setq-local cursor-type t))
  (setq pyim-page--minibuffer-string nil))

(add-hook 'pyim-process-ui-hide-hook #'pyim-page--hide)

;; * Footer
(provide 'pyim-page)

;;; pyim-page.el ends here
