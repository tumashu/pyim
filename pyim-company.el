;;; pyim-company --- company-mode completion backend for pyim     -*- lexical-binding: t; -*-

;; * Header
;; Copyright 2020 Levin Du

;; Author: Levin Du <zslevin@gmail.com>

;; This file implements a simple company-mode completion backend for pyim.

;;; Commentary:

;; * 说明文档                                                              :doc:
;; 这个文件通过 company-mode 来实现联想词的选择。

;; 因为联想词比较多，所以默认仅列出三字词及以上。如果需要列出二字词，可以做以下设定：

;; (setq pyim-company-prefix-min 1)

;; 为了减少联想词的数量，补全时会使用 Pyim 输入的长度来过滤。例如用户输入“武器”一词
;; 后，提示的联想词列表就是“武器”开头的词组，而忽略 “器”开头的。

;; 另外就是优先考虑长的联想词，例如在输入“白”，之后输入“日”，系统就会提示“白日”开头
;; 的词组，而忽略“日”开头的。

;; 补全的来源目前有三个，可以修改 `pyim-company-match-functions' 来设定，默认全部启用。

;; (setq pyim-company-match-functions '(pyim-company-match-phrase-file
;;                                      pyim-company-match-dhashcache
;;                                      pyim-company-match-buffer))

;; 排在前面的函数所返回的结果会有更高的展示优先级。

;;  - `pyim-company-match-phrase-file': 使用 `pyim-company-phrase-file' 指定的词组
;;    文件来做全行匹配。例如，若该文件含有“白日依山进，黄河入海流。”一行，则在输入
;;    “白日”后会将原样的整行作为候选项供用户选择，所有空白标点均会保留。要编辑该词
;;    组文件，调用 `pyim-company-edit-phrase-file' 即可。

;; - `pyim-company-match-dhashcache': 搜索 Pyim 的 dhashcache 里的词组。

;;  - `pyim-company-match-buffer': 搜索所有 buffer 来匹配中文句子（包括标点），要
;;    求该 buffer 不在 `pyim-company-match-buffer-ignore' 里，主模式是跟当前 buffer 
;;    相同，或派生于 `pyim-company-match-buffer-modes' 列表中。

;; Code:

;; * 代码                                                                 :code:

(require 'pyim)
(require 'pyim-dhashcache)
(require 'company)
(require 'cl-lib)

;; ** 变量

(defcustom pyim-company-prefix-min 2
  "匹配前缀的最小长度。

默认是 2，即至少截取光标前两个汉字去匹配三字及以上的联想词。如果
需要二字联想词，则设为 1。"
  :group 'pyim
  :type 'integer)

(defcustom pyim-company-prefix-max 4
  "匹配前缀的最大长度。

默认是 4，即至多截取光标前四个汉字去匹配联想词，主要用来减少匹配
范围，从而提高速度。"
  :group 'pyim
  :type 'integer)

(defcustom pyim-company-match-buffer-ignore '("^ \\*.*\\*$" dired-mode)
  "指定要忽略的 buffer。

可以包含正则表达式，或主模式。"
  :group 'pyim
  :type '(repeat (choice regexp (symbol :tag "Major Mode"))))

(defcustom pyim-company-match-buffer-modes '(text-mode)
  "指定要匹配的 buffer。

如果 buffer 的主模式派生于此列表之一，则列入匹配范围。 `org-mode'
是 `text-mode' 的派生模式，仅需指定 `text-mode' 即可搜索
`org-mode' 等 buffer 。"
  :group 'pyim
  :type '(repeat (symbol :tag "Major Mode")))

(defcustom pyim-company-phrase-file (concat (file-name-as-directory pyim-dcache-directory)
                                            "phrase.org")
  "设定 `pyim-company-match-phrase-file' 所使用的词组文件的路径。"
  :group 'pyim
  :type 'file)

(defcustom pyim-company-match-functions '(pyim-company-match-phrase-file
                                          pyim-company-match-dhashcache
                                          pyim-company-match-buffer)
  "匹配函数列表，每个函数接受一个 prefix 参数，并返回联想词列表。"
  :group 'pyim
  :type 'list)

(defvar pyim-company-cache nil)

;; ** Company 整合

;;;###autoload
(defun pyim-company (command &optional arg &rest ignore)
  "`company-mode' completion backend for pyim."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'pyim-company))
    (prefix (pyim-company-prefix))
    (candidates (pyim-company-filter arg))
    (sorted t)
    (duplicates t)))

(defun pyim-company-prefix ()
  "Return the max prefix with matches available."
  (let* ((prefix-min (max pyim-company-prefix-min
                          (length (car pyim-outcome-history)))))
    (cl-loop for num from pyim-company-prefix-max downto prefix-min
             for prefix = (pyim-cstring-at-point num)
             when (and (stringp prefix) (pyim-company-filter prefix))
             return prefix)))

(defun pyim-company-filter (prefix)
  "Return all candidates that begin with PREFIX (cached version)."
  (unless (string= (car pyim-company-cache) prefix)
    (setq pyim-company-cache
          (cons prefix
                (pyim-company-matches prefix))))
  (cdr pyim-company-cache))

(defun pyim-company-matches (prefix)
  "Return all candidates that begin with PREFIX (no cache)."
  (let (result)
    (dolist (fn pyim-company-match-functions)
      (setq result (append result (apply fn (list prefix)))))
    (delete prefix result)))

;; ** 通用工具代码

(defun pyim-company-gather (pattern)
  "在当前 buffer 中收集所有匹配 PATTERN 的表达式。"
  (let (result)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward pattern nil t)
          (setq result (cons (buffer-substring-no-properties (match-beginning 0)
						             (match-end 0))
                             result)))))
    result))

(defun pyim-company-pattern-sentence (prefix)
  "返回匹配 PREFIX 开始，以标点或行尾结束的正则表达式。"
  (let ((punct ",，.。!！?？:：;；、…"))
    (concat (regexp-quote prefix)
            "[^" punct "\n\r\t]+?"
            "\\(?:[" punct "]\\|$\\)")))

(defun pyim-company-pattern-whole-line (prefix)
  "返回匹配 PREFIX 开头的整行的正则表达式。"
  (concat "^" (regexp-quote prefix) ".*$"))

;; shameless copied from he-buffer-remember from hippie-exp
(defun pyim-company-buffer-member (lst)
  (or (memq major-mode lst)
      (progn
	(while (and lst
		    (or (not (stringp (car lst)))
			(not (string-match (car lst) (buffer-name)))))
	  (setq lst (cdr lst)))
	lst)))

;; ** 匹配 Pyim 的 dhashcache

(defun pyim-company-match-dhashcache (prefix)
  (append (all-completions prefix pyim-dhashcache-word2code)
          (all-completions prefix pyim-dhashcache-iword2count)))

;; ** 匹配 buffer

(defun pyim-company-match-buffer (prefix)
  "从所有 buffer 中匹配 PREFIX 开头的句子。

先从当前 buffer 匹配，然后再匹配其它主模式与当前 buffer 相同、或
派生于 `pyim-company-match-buffer-modes' 所列出的主模式的 buffer。

返回的是匹配项的列表，匹配项不含结束标点。"

  (let* ((this-buf (current-buffer))
         (mode major-mode)
         (pattern (pyim-company-pattern-sentence prefix))
         ;; current-buffer first
         (result (pyim-company-gather pattern)))
    (catch 'done
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (not (equal buf this-buf))
                     (not (pyim-company-buffer-member pyim-company-match-buffer-ignore))
                     (apply 'derived-mode-p mode pyim-company-match-buffer-modes))
              (setq result (remove-duplicates (append result (pyim-company-gather pattern))
                                              :test 'string=))
              (when (> (length result) 20)
                (throw 'done nil))))))
    result))

;; ** 匹配词组文件

(defun pyim-company-match-phrase-file (prefix)
  "返回 `pyim-company-phrase-file' 词组文件所有匹配 PREFIX 开头的行。"
  (with-current-buffer (find-file-noselect pyim-company-phrase-file)
    (pyim-company-gather (pyim-company-pattern-whole-line prefix))))

(defun pyim-company-edit-phrase-file ()
  "编辑 `pyim-company-phrase-file' 所指定的词组文件。"
  (interactive)
  (find-file-other-window pyim-company-phrase-file))

;; ** Pyim 整合

(defun pyim-company-init ()
  "Pyim 激活时所要执行的钩子函数，激活 `pyim-company'。

当 `company-mode' 打开，将 `pyim-company' 作为第一个 backend 插入，
激活联想词功能。"
  (when (bound-and-true-p company-mode)
    ;; make sure pyim-company is the first one in backend list
    (setq company-backends (delete 'pyim-company company-backends))
    (add-to-list 'company-backends 'pyim-company)
    (make-local-variable 'company-minimum-prefix-length)
    (setq company-minimum-prefix-length pyim-company-prefix-min)))

(defun pyim-company-deinit ()
  "Pyim 关闭时所要执行的钩子函数。

当 `company-mode' 打开，将 `pyim-company' 移除出 backend 列表，并
做清理工作。"
  (when (bound-and-true-p company-mode)
    (setq company-backends (delete 'pyim-company company-backends))
    (kill-local-variable 'company-minimum-prefix-length)))

(add-hook 'pyim-active-hook 'pyim-company-init)
(add-hook 'pyim-inactive-hook 'pyim-company-deinit)

;; * Footer
(provide 'pyim-company)

;;; pyim-company.el ends here
