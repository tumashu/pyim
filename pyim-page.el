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
;; Emacs 26.1 之前的版本无法安装 posframe.
(require 'posframe nil t)
;; popup 不是 GNU ELPA 包，所以 pyim 不能强制依赖它。
(require 'popup nil t)
(require 'pyim-common)
(require 'pyim-preview)

(eval-when-compile
  (require 'pyim-entered))

(defgroup pyim-page nil
  "Page tools for pyim."
  :group 'pyim)

(defcustom pyim-page-length 5
  "每页显示的词条数目.

细节信息请参考 `pyim-page-refresh' 的 docstring."
  :type 'number)

(defcustom pyim-page-tooltip '(posframe popup minibuffer)
  "如何绘制 pyim 选词框.

1. 当这个变量取值为 posframe 时，使用 posframe 包来绘制选词框，
   使用 emacs 26 图形版的用户推荐使用这个选项。
2. 当这个变量取值为 popup 时，使用 popup-el 包来绘制选词框，
   这个选项可以在 emacs 图形版和终端版使用，速度没有 posframe 快，
   偶尔会遇到选词框错位的问题。
3. 当这个变量取值为 minibuffer 时，minibuffer 将做为选词框，
   这个选项也作为其他选项不可用时的 fallback.
4. 当这个变量的取值是为一个 list 时，pyim 将按照优先顺序动态
   选择一个当前环境可用的 tooltip."
  :type '(choice (repeat (choice (const posframe)
                                 (const popup)
                                 (const minibuffer)))
                 (const posframe)
                 (const popup)
                 (const minibuffer)))

(defcustom pyim-page-style 'two-lines
  "这个变量用来控制选词框的格式.

pyim 内建的有四种选词框格式：

1. one-line    单行选词框
2. two-lines   双行选词框
3. vertical    垂直选词框
4. minibuffer  单行选词框 (minibuffer 中专用)"
  :type '(choice (const two-lines)
                 (const one-line)
                 (const vertical)
                 (const minibuffer)))

(defcustom pyim-page-tooltip-style-alist
  '((minibuffer . minibuffer))
  "pyim page tooltip 专用 page style 绑定设置表。

这个表是一个 alist, 每个元素的 car 代表 tooltip, cdr 代表对应的
page style."
  :type '(alist :key-type symbol
                :value-type symbol))

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
  '((t (:inherit pyim-page)))
  "Face used for the pyim page border.
Only useful when use posframe.")

(defface pyim-page-selection
  '((t (:background "gray44")))
  "选词框中已选词条的 face.")

(defface pyim-page-subword
  '((t (:background "gray44")))
  "使用以词选字功能时，选择的汉字所使用的 face.")

(defvar pyim-page-posframe-buffer " *pyim-page-posframe-buffer*"
  "这个变量用来保存做为 page tooltip 的 posframe 的 buffer.")

(defvar pyim-page-last-popup nil
  "这个变量用来保存做为 page tooltip 的 popup.")

(defvar pyim-page-last-minibuffer-string nil
  "函数 `pyim-page-show-with-minibuffer' 上一次处理的消息字符串。")

(defun pyim-page-current-page ()
  "计算当前选择的词条在第几页面.

细节信息请参考 `pyim-page-refresh' 的 docstring."
  (1+ (/ (1- pyim-candidate-position) pyim-page-length)))

(defun pyim-page-total-page ()
  "计算 page 总共有多少页.

细节信息请参考 `pyim-page-refresh' 的 docstring."
  (1+ (/ (1- (length pyim-candidates)) pyim-page-length)))

(defun pyim-page-start ()
  "计算当前所在页的第一个词条的位置.

细节信息请参考 `pyim-page-refresh' 的 docstring."
  (let ((pos (min (length pyim-candidates) pyim-candidate-position)))
    (1+ (* (/ (1- pos) pyim-page-length) pyim-page-length))))

(defun pyim-page-end (&optional finish)
  "计算当前所在页的最后一个词条的位置，

如果 pyim-candidates 用完，则检查是否有补全。如果 FINISH 为
non-nil，说明，补全已经用完了.

细节信息请参考 `pyim-page-refresh' 的 docstring."
  (let* ((whole (length pyim-candidates))
         (len pyim-page-length)
         (pos pyim-candidate-position)
         (last (* (/ (+ (1- pos) len) len) len)))
    (if (< last whole)
        last
      (if finish
          whole
        (pyim-page-end t)))))

(defun pyim-page-refresh (&optional hightlight-current)
  "刷新 page 页面的函数.

这个函数主要用来处理选词框选词后，刷新显示问题，pyim 使用
`pyim-candidates' 来保存 *待选词列表* ，\"nihao\" 对应的
`pyim-candidates' 的值类似：

     (\"你好\" \"倪皓\" \"泥\" \"你\"  ...  \"慝\")

*待选词列表* 一般都很长，不可能在一页中完全显示，所以 pyim 使用了
page 的概念，比如，上面的 “nihao” 的 *待选词列表* 就可以逻辑的
分成5页：


  第1页   你好  倪皓  泥  你  呢  拟  逆  腻  妮
  第2页   怩    溺    尼  禰  齯  麑  鲵  蜺  衵
  第3页   薿    旎    睨  铌  昵  匿  倪  霓  暱
  第4页   柅    猊    郳  輗  坭  惄  堄  儗  伲
  第5页   祢    慝

`pyim-candidate-position' 的取值以及 `pyim-page-length' 的设定值
（默认设置为9），共同决定了 pyim 需要显示哪一页，比如：

  第1页  你好  倪皓   泥    你  呢  拟  逆  腻  妮
  第2页  怩    溺     尼    禰  齯  麑  鲵  蜺  衵
  第3页  薿[B] 旎     睨[A] 铌  昵  匿  倪  霓  暱[E]
  第4页  柅    猊     郳    輗  坭  惄  堄  儗  伲
  第5页  祢    慝

假设当前选择的词条为 \"睨\", 那么 `pyim-candidate-position' 的取
值为 A 所在的位置。那么：

1. 函数 `pyim-page-current-page' 返回值为3， 说明当前 page 为第3页。
2. 函数 `pyim-page-total-page'  返回值为5，说明 page 共有5页。
3. 函数 `pyim-page-start' 返回 B 所在的位置。
4. 函数 `pyim-page-end' 返回 E 所在的位置。
5. 函数 `pyim-page-refresh' 会从 `pyim-candidates' 中提取一个 sublist:

     (\"薿\" \"旎\" \"睨\" \"铌\" \"昵\" \"匿\" \"倪\" \"霓\" \"暱\")

   这个 sublist 的起点为 `pyim-page-start' 的返回值，终点为
   `pyim-page-end' 的返回值。并保存到一个 hashtable 的
   :candidates 关键字对应的位置，这个 hastable 最终会做为参数传递
   给 `pyim-page-style' 相关的函数，用于生成用于在选词框中显示的
   字符串。"
  (let* ((start (1- (pyim-page-start)))
         (end (pyim-page-end))
         (candidates pyim-candidates)
         (candidate-showed
          (mapcar (lambda (x)
                    (let ((comment (get-text-property 0 :comment x)))
                      (if comment
                          (concat x comment)
                        x)))
                  (cl-subseq candidates start end)))
         (pos (- (min pyim-candidate-position (length candidates)) start))
         (page-info (make-hash-table))
         (tooltip (pyim-page-get-valid-tooltip)))
    (puthash :current-page (pyim-page-current-page) page-info)
    (puthash :total-page (pyim-page-total-page) page-info)
    (puthash :candidates candidate-showed page-info)
    (puthash :position pos page-info)
    (puthash :hightlight-current hightlight-current page-info)
    ;; Show page.
    (when (and (null unread-command-events)
               (null unread-post-input-method-events))
      (pyim-page-show
       (pyim-page-info-format page-info tooltip)
       (pyim-preview-start-point)
       tooltip))))

(declare-function pyim-process-terminate "pyim-process")

(defun pyim-page-next-page (arg)
  "Pyim page 翻页命令.

原理是：改变 `pyim-candidate-position' 的取值，假设一次只翻一页，
那么这个函数所做的工作就是：
1. 首先将 `pyim-candidate-position' 增加 `pyim-page-length' ，确
   保其指定的位置在下一页。
2. 然后将 `pyim-candidate-position' 的值设定为 `pyim-page-start'
   的返回值，确保 `pyim-candidate-position' 的取值为下一页第一个
   词条的位置。
3. 最后调用 `pyim-page-refresh' 来重新刷新页面。"
  (interactive "p")
  (if (= (length (pyim-entered-get 'point-before)) 0)
      (progn
        (pyim-process-outcome-handle 'last-char)
        (pyim-process-terminate))
    (let ((new (+ pyim-candidate-position (* pyim-page-length arg) 1))
          maxpos)
      (setq maxpos (+ 1 (length pyim-candidates)))
      (setq pyim-candidate-position
            (if (> new 0)
                (if (> new maxpos) 1 new)
              maxpos)
            pyim-candidate-position (pyim-page-start))
      (pyim-preview-refresh)
      (pyim-page-refresh))))

(defun pyim-page-previous-page (arg)
  (interactive "p")
  (pyim-page-next-page (- arg)))

(defun pyim-page-next-word (arg)
  (interactive "p")
  (if (= (length (pyim-entered-get 'point-before)) 0)
      (progn
        (pyim-process-outcome-handle 'last-char)
        (pyim-process-terminate))
    (let ((new (+ pyim-candidate-position arg))
          len)
      (setq len (length pyim-candidates))
      (setq pyim-candidate-position
            (if (>= len new)
                (if (> new 0) new len)
              1))
      (pyim-preview-refresh)
      (pyim-page-refresh t))))

(defun pyim-page-previous-word (arg)
  (interactive "p")
  (pyim-page-next-word (- arg)))

(defun pyim-page-preview-create (&optional separator)
  "这个函数用于创建在 page 中显示的预览字符串。

这个预览是在 page 中显示，而 `pyim-preview-refresh' 对应的预览
是在 buffer 光标处显示，两者要做区别。"
  (let* ((scheme-name (pyim-scheme-name))
         (class (pyim-scheme-get-option scheme-name :class)))
    (when class
      (funcall (intern (format "pyim-page-preview-create:%S" class)) separator))))

(declare-function 'pyim-entered-with-entered-buffer "pyim-entered")

(defun pyim-page-preview-create:quanpin (&optional separator)
  (let* ((separator (or separator " "))
         (translated (string-join (mapcar (lambda (w)
                                            (concat (nth 0 w) (nth 1 w)))
                                          (car pyim-imobjs))
                                  separator)))
    (concat
     ;; | 显示光标位置的字符
     (pyim-entered-with-entered-buffer
       (if (equal 1 (point))
           (concat "|" translated)
         (concat (replace-regexp-in-string (concat separator "'") "'" translated)
                 " |" (buffer-substring-no-properties (point) (point-max)))))
     ;; 用于标记辅助输入法
     (when (and (eq pyim-assistant-scheme 'quanpin)
                (eq pyim-assistant-scheme-enable t))
       (let ((code (pyim-cstring-to-xingma
                    (nth (1- pyim-candidate-position)
                         pyim-candidates)
                    (pyim-scheme-name 'default))))
         (if (> (length code) 0)
             (format " [%s](辅)" code)
           " (辅)"))))))

(defun pyim-page-preview-create:shuangpin (&optional separator)
  (let ((keymaps (pyim-scheme-get-option (pyim-scheme-name) :keymaps))
        result)
    (dolist (w (car pyim-imobjs))
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

(defun pyim-page-preview-create:xingma (&optional separator)
  (let* ((scheme-name (pyim-scheme-name)))
    (cl-flet* ((segment (x)
                 (string-join
                  (car (pyim-imobjs-create x scheme-name))
                  (or separator " ")))
               (fmt (x)
                 (mapconcat #'segment
                            (split-string x "'")
                            "'")))
      ;; | 显示光标位置的字符
      (pyim-process-with-entered-buffer
        (if (equal (point) (point-max))
            (fmt (buffer-substring-no-properties (point-min) (point-max)))
          (concat (fmt (buffer-substring-no-properties (point-min) (point)))
                  "| "
                  (fmt (buffer-substring-no-properties (point) (point-max)))))))))

(defun pyim-page-menu-create (candidates position &optional separator hightlight-current)
  "这个函数用于创建在 page 中显示的备选词条菜单。"
  (let ((i 0) result)
    (dolist (candidate candidates)
      (let ((str (substring-no-properties
                  (if (consp candidate)
                      (concat (car candidate) (cdr candidate))
                    candidate))))
        (dolist (n pyim-outcome-subword-info)
          (when (<= n (length str))
            (set-text-properties (- n 1) n '(face pyim-page-subword) str)))
        (setq i (1+ i))
        ;; 高亮当前选择的词条，用于 `pyim-page-next-word'
        (push
         (if (and hightlight-current
                  (= i position))
             (format "%d%s" i
                     (propertize
                      (format "[%s]" str)
                      'face 'pyim-page-selection))
           (format "%d.%s " i str))
         result)))
    (string-join (nreverse result) (or separator ""))))

(defun pyim-page-info-format (page-info tooltip)
  "将 PAGE-INFO 按照 `pyim-page-style' 格式化为选词框中显示的字符串。"
  (let ((style (or (cdr (assoc tooltip pyim-page-tooltip-style-alist))
                   pyim-page-style)))
    (let ((func (intern (format "pyim-page-style:%S" style))))
      (if (functionp func)
          (funcall func page-info)
        (pyim-page-style:two-lines page-info)))))

(defun pyim-page-style:two-lines (page-info)
  "将 PAGE-INFO 格式化为选词框中显示的字符串.

样式类似：

+----------------------------+
| ni hao [1/9]               |
| 1.你好 2.你号 ...          |
+----------------------------+"
  (format "=> %s [%s/%s]: \n%s"
          (pyim-page-preview-create)
          (gethash :current-page page-info)
          (gethash :total-page page-info)
          (pyim-page-menu-create
           (gethash :candidates page-info)
           (gethash :position page-info)
           nil
           (gethash :hightlight-current page-info))))

(defun pyim-page-style:one-line (page-info)
  "将 PAGE-INFO 格式化为选词框中显示的字符串.

样式类似：

+-----------------------------------+
| [ni hao]: 1.你好 2.你号 ... (1/9) |
+-----------------------------------+"
  (format "[%s]: %s(%s/%s)"
          (pyim-page-preview-create " ")
          (pyim-page-menu-create
           (gethash :candidates page-info)
           (gethash :position page-info)
           nil
           (gethash :hightlight-current page-info))
          (gethash :current-page page-info)
          (gethash :total-page page-info)))

(defun pyim-page-style:vertical (page-info)
  "将 PAGE-INFO 格式化为选词框中显示的字符串.

样式类似：

+--------------+
| ni hao [1/9] |
| 1.你好       |
| 2.你号 ...   |
+--------------+"
  (format "=> %s [%s/%s]: \n%s"
          (pyim-page-preview-create)
          (gethash :current-page page-info)
          (gethash :total-page page-info)
          (pyim-page-menu-create
           (gethash :candidates page-info)
           (gethash :position page-info)
           "\n"
           (gethash :hightlight-current page-info))))

(defun pyim-page-style:minibuffer (page-info)
  "将 PAGE-INFO 格式化为选词框中显示的字符串.

样式类似：

+------------------------------------+
| [ni hao]: 1.你好 2.你号 ...  (1/9) |
+------------------------------------+"
  ;; 在 minibuffer 中显示 page 的时候，page 字符串直接插入到 minibuffer 现有的内
  ;; 容中, 为了便于区分，在 page 后面添加一个显眼的字符。
  (format "[%-15s]: %s(%s/%s) $ "
          (pyim-page-preview-create)
          (pyim-page-menu-create
           (gethash :candidates page-info)
           (gethash :position page-info)
           nil
           (gethash :hightlight-current page-info))
          (gethash :current-page page-info)
          (gethash :total-page page-info)))

(defun pyim-page-get-valid-tooltip ()
  "获取一个可用的 tooltip."
  (cond
   ;; NOTE: 以前在 minibuffer 中试用过 posframe, linux 环境下运行效果还不错，但
   ;; 在 windows 环境下，似乎有很严重的性能问题，原因未知。
   ((eq (selected-window) (minibuffer-window)) 'minibuffer)
   ;; 在 exwm-xim 环境下输入中文时，只能使用 minibuffer, 因为应用窗口遮挡的缘故，
   ;; 其它方式不可用。
   ((pyim-exwm-xim-environment-p) 'minibuffer)
   (t (or (cl-find-if (lambda (tp)
                        (or (and (eq tp 'posframe)
                                 (functionp 'posframe-workable-p)
                                 (posframe-workable-p))
                            (and (eq tp 'popup)
                                 (featurep 'popup))
                            (eq tp 'minibuffer)))
                      (if (listp pyim-page-tooltip)
                          pyim-page-tooltip
                        (list pyim-page-tooltip)))
          'minibuffer))))

(defun pyim-page-show (string position tooltip)
  "在 POSITION 位置，使用 posframe 或者 popup 显示字符串 STRING."
  (cond ((eq tooltip 'posframe)
         (posframe-show pyim-page-posframe-buffer
                        :string string
                        :position position
                        :min-width pyim-page-posframe-min-width
                        :background-color (face-attribute 'pyim-page :background)
                        :foreground-color (face-attribute 'pyim-page :foreground)
                        :border-width pyim-page-posframe-border-width
                        :border-color (face-attribute 'pyim-page-border :background)))
        ((eq tooltip 'popup)
         (pyim-page-show-with-popup :string string
                                    :position position))
        (t (pyim-page-show-with-minibuffer string))))

(defun pyim-page-show-with-minibuffer (string)
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
      (delete-char (length pyim-page-last-minibuffer-string))
      (save-excursion
        (insert
         (setq pyim-page-last-minibuffer-string
               (concat
                ;; 显示一个伪 cursor.
                (propertize " " 'face 'cursor)
                (or pyim-page-minibuffer-separator
                    (let* ((width (string-width (buffer-string)))
                           (n (- (* 20 (+ 1 (/ width 20))) width)))
                      (make-string n ?\ )))
                string)))))))

(declare-function 'popup-tip "popup")
(declare-function 'popup-delete "popup")
(defvar popup-version)

(cl-defun pyim-page-show-with-popup (&key string position)
  "Show STRING at POSITION with the help of popup-el."
  (when pyim-page-last-popup
    ;; 异步获取词条的时候，如果不把已经存在的 popup 删除，就会出现两个 page.
    (popup-delete pyim-page-last-popup))
  (setq pyim-page-last-popup
        (apply #'popup-tip string
               :point position :around t :nowait t :nostrip t
               ;; popup v0.5.9 以后才支持 face 参数
               (unless (version<= popup-version "0.5.8")
                 (list :face 'pyim-page)))))

(defun pyim-page-hide ()
  "Hide pyim page."
  (let ((tooltip (pyim-page-get-valid-tooltip)))
    (cond
     ((eq tooltip 'popup)
      (popup-delete pyim-page-last-popup))
     ((eq tooltip 'posframe)
      (posframe-hide pyim-page-posframe-buffer))
     (t (when (eq (selected-window) (minibuffer-window))
          ;; 从 minibuffer 中删除 page 字符串。
          (delete-char (length pyim-page-last-minibuffer-string))
          ;; 在类似 vertico-posframe 这样的环境中，posframe window-point 同步问题
          ;; 不太好处理，这里使用一个简单粗暴的方式：在输入过程中，隐藏真实的
          ;; cursor 并显示一个伪 cursor, 输入完成之后再恢复。
          (setq-local cursor-type t))
        (setq pyim-page-last-minibuffer-string nil)))))

;; * Footer
(provide 'pyim-page)

;;; pyim-page.el ends here
