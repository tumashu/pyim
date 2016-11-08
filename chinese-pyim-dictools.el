;;; chinese-pyim-dictools.el --- Tools for Chinese-pyim pinyin dict

;; * Header
;; Copyright 2015-2016 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/chinese-pyim
;; Version: 1.5
;; Keywords: convenience, Chinese, pinyin, input-method

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; * 说明文档                                                              :doc:
;; 这个文件主要包含与词库文件管理相关的一些命令和函数，比如：
;; 1. 词库管理器
;; 2. 汉字转拼音
;; 3. 其它


;;; Code:

;; * 代码                                                                 :code:
;; ** Require
;; #+BEGIN_SRC emacs-lisp
(require 'cl-lib)
(require 'chinese-pyim-pymap)
(require 'chinese-pyim-core)
;; #+END_SRC

;; ** 汉字到拼音的转换工具
;; `pyim-hanzi2pinyin' 和 `pyim-hanzi2pinyin-simple' 可以将一个中文字符串转换为拼音字符串
;; 或者拼音列表，也可以将一个中文字符串转换为拼音首字母字符串或者首字母列表。

;; 在转换的过程中，`pyim-hanzi2pinyin' 考虑多音字，所以适用于制作词库，Chinese-pyim 使用
;; 这个函数来制作词库。而 `pyim-hanzi2pinyin-simple' 不考虑多音字，可以用于添加拼音索引。

;; 例如：
;; #+BEGIN_SRC emacs-lisp
;; (list (pyim-hanzi2pinyin "银行")
;;       (pyim-hanzi2pinyin "银行" t)
;;       (pyim-hanzi2pinyin "银行" nil "-")
;;       (pyim-hanzi2pinyin "银行" nil "-" t)
;;       (pyim-hanzi2pinyin "银行" t "-" t))
;; #+END_SRC

;; #+RESULTS:
;; : ("yinhang yinxing" "yh yx" "yin-hang yin-xing" ("yin-hang" "yin-xing") ("y-h" "y-x"))

;; #+BEGIN_SRC emacs-lisp
;; (list (pyim-hanzi2pinyin-simple "行长")
;;       (pyim-hanzi2pinyin-simple "行长" t)
;;       (pyim-hanzi2pinyin-simple "行长" nil "-")
;;       (pyim-hanzi2pinyin-simple "行长" nil "-" t)
;;       (pyim-hanzi2pinyin-simple "行长" t "-" t))
;; #+END_SRC

;; #+RESULTS:
;; : ("hangchang" "hc" "hang-chang" ("hang-chang") ("h-c"))


;; `pyim-hanzi2pinyin' 函数使用 “排列组合” 函数 `pyim-permutate-list' 来处理多音字，
;; 这个函数所做工作类似：

;; #+BEGIN_EXAMPLE
;; (cl-prettyprint
;;  (pyim-permutate-list
;;   '(("a" "b")
;;     ("c" "d")
;;     ("e" "f" "g"))))

;; OUTPUT:

;; (("a" "c" "e")
;;  ("a" "c" "f")
;;  ("a" "c" "g")
;;  ("a" "d" "e")
;;  ("a" "d" "f")
;;  ("a" "d" "g")
;;  ("b" "c" "e")
;;  ("b" "c" "f")
;;  ("b" "c" "g")
;;  ("b" "d" "e")
;;  ("b" "d" "f")
;;  ("b" "d" "g"))
;; #+END_EXAMPLE


;; #+BEGIN_SRC emacs-lisp
;;;###autoload
(defun pyim-hanzi2pinyin (string &optional shou-zi-mu separator
                                 return-list ignore-duo-yin-zi adjuct-duo-yin-zi)
  "将汉字字符串转换为对应的拼音字符串, 如果 `shou-zi-mu' 设置为t,转换仅得到拼音
首字母字符串。当 `return-list' 设置为 t 时，返回一个拼音列表，这个列表包含词条的一个
或者多个拼音（词条包含多音字时）；如果 `ignore-duo-yin-zi' 设置为t, 遇到多音字时，
只使用第一个拼音，其它拼音忽略；当 `adjuct-duo-yin-zi' 设置为t时，pyim-hanzi2pinyin
会使用 Chinese-pyim 已安装的词库来校正多音字，但这个功能有一定的限制:

1. Chinese-pyim 普通词库中不存在的词条不能较正
2. 多音字校正速度比较慢，实时转换会产生卡顿。

BUG: 当 `string' 中包含其它标点符号，并且设置 `separator' 时，结果会包含多余的连接符：
比如： '你=好' --> 'ni-=-hao'"
  (if (not (pyim-string-match-p "\\cc" string))
      string
    (let (string-list pinyins-list pinyins-list-permutated pinyins-list-adjusted)

      ;; 将汉字字符串转换为字符list，英文原样输出。
      ;; 比如： “Hello银行” -> ("Hello" "银" "行")
      (setq string-list
            (if (pyim-string-match-p "\\CC" string)
                ;; 处理中英文混合的情况
                (split-string
                 (replace-regexp-in-string
                  "\\(\\cc\\)" "@@@@\\1@@@@" string)
                 "@@@@")
              ;; 如果词条只包含中文，使用`string-to-vector'
              ;; 这样处理速度比较快。
              (string-to-vector string)))

      ;; 将上述汉字字符串里面的所有汉字转换为与之对应的拼音list。
      ;; 比如： ("Hello" "银" "行") -> (("Hello") ("yin") ("hang" "xing"))
      (mapc
       #'(lambda (str)
           ;; `string-to-vector' 得到的是 char vector, 需要将其转换为 string。
           (when (numberp str)
             (setq str (char-to-string str)))
           (cond
            ((> (length str) 1)
             (push (list str) pinyins-list))
            ((and (> (length str) 0)
                  (pyim-string-match-p "\\cc" str))
             (push (or (pyim-cchar2pinyin-get (string-to-char str))
                       (list str))
                   pinyins-list))
            ((> (length str) 0)
             (push (list str) pinyins-list))))
       string-list)
      (setq pinyins-list (nreverse pinyins-list))

      ;; 通过排列组合的方式, 重排 pinyins-list。
      ;; 比如：(("Hello") ("yin") ("hang" "xing")) -> (("Hello" "yin" "hang") ("Hello" "yin" "xing"))
      (setq pinyins-list-permutated (pyim-permutate-list2 pinyins-list))

      ;; 使用 Chinese-pyim 的安装的词库来校正多音字。
      (when adjuct-duo-yin-zi
        ;; 确保 pyim 词库加载
        (pyim-dcache-init-variables)
        (dolist (pinyin-list pinyins-list-permutated)
          (let* ((py-str (mapconcat #'identity pinyin-list "-"))
                 (words-from-dicts
                  ;; pyim-buffer-list 中第一个 buffer 对应的是个人词库文件
                  ;; 个人词库文件中的词条，极有可能存在 *多音字污染*。
                  ;; 这是由 Chinese-pyim 保存词条的机制决定的。
                  (pyim-dcache-get py-str pyim-dcache-code2word)))
            (when (member string words-from-dicts)
              (push pinyin-list pinyins-list-adjusted))))
        (setq pinyins-list-adjusted
              (nreverse pinyins-list-adjusted)))

      ;; 返回拼音字符串或者拼音列表
      (let* ((pinyins-list
              (or pinyins-list-adjusted
                  pinyins-list-permutated))
             (list (mapcar
                    #'(lambda (x)
                        (mapconcat
                         #'(lambda (str)
                             (if shou-zi-mu
                                 (substring str 0 1)
                               str))
                         x separator))
                    (if ignore-duo-yin-zi
                        (list (car pinyins-list))
                      pinyins-list))))
        (if return-list
            list
          (mapconcat #'identity list " "))))))

(defun pyim-permutate-list (list)
  "使用排列组合的方式重新排列 `list'，这个函数由 ‘二中’ 提供。
注：`pyim-hanzi2pinyin' 没有使用这个函数(速度稍微有点慢)。"
  (let ((list-head (car list))
        (list-tail (cdr list)))
    (cond ((null list-tail)
           (cl-loop for element0 in list-head
                    append (cons (cons element0 nil) nil)))
          (t (cl-loop for element in list-head
                      append (mapcar (lambda (l) (cons element l))
                                     (pyim-permutate-list list-tail)))))))

(defun pyim-permutate-list2 (list)
  "使用排列组合的方式重新排列 `list'，这个函数由 ’翀/ty‘ 提供。
`pyim-hanzi2pinyin' 默认使用这个函数。"
  (if (= (length list) 1)
      (mapcar #'list (car list))
    (pyim-permutate-list2-internal (car list) (cdr list))))

(defun pyim-permutate-list2-internal (one two)
  "`pyim-permutate-list2' 的内部函数。"
  (let (return)
    (if (null (car two))
        one
      (dolist (x1 one)
        (dolist (x2 (car two))
          (push (if (listp x1)
                    (append x1 (list x2))
                  (list x1 x2))
                return)))
      (setq one return)
      (pyim-permutate-list2-internal one (cdr two)))))

;;;###autoload
(defun pyim-hanzi2pinyin-simple (string &optional shou-zi-mu separator return-list)
  "简化版的 `pyim-hanzi2pinyin', 不处理多音字。"
  (pyim-hanzi2pinyin string shou-zi-mu separator return-list t))
;; #+END_SRC

;; ** Chinese-pyim 词库管理工具
;; 为 Chinese-pyim 添加一个简单的词库管理工具 `pyim-dicts-manager' ，可以方便的执行下列命令：
;; 1. 添加词库。
;; 2. 删除词库。
;; 3. 向上和向下移动词库。
;; 4. 保存词库设置。
;; 5. 重启输入法。

;; #+BEGIN_SRC emacs-lisp
(defvar pyim-dm-buffer "*pyim-dict-manager*")

(defun pyim-dm-refresh ()
  "Refresh the contents of the *pyim-dict-manager* buffer."
  (interactive)
  (with-current-buffer pyim-dm-buffer
    (let ((inhibit-read-only t)
          (dicts-list pyim-dicts)
          (format-string "%-4s %-4s %-60s\n")
          (face-attr '((foreground-color . "DarkOrange2")
                       (bold . t)))
          (i 1))
      (erase-buffer)
      (insert (propertize (format format-string "序号" "启用" "词库文件")
                          'face face-attr))
      (insert (propertize (format format-string
                                  "----" "----"
                                  "----------------------------------------------------------------------\n")
                          'face face-attr))
      (if (not pyim-dicts)
          (insert "拼音词库是 Chinese-pyim 使用顺手与否的关键。根据经验估计：

1. 当词库词条超过100万时 (词库文件>20M)，Chinese-pyim 选词频率大大降低。
2. 当词库词条超过100万时，Chinese-pyim 中文输入体验可以达到搜狗输入法的 80%。

想快速体验 Chinese-pyim 输入法的用户, 可以使用 chinese-pyim-basedict：

     (require 'chinese-pyim-basedict)
     (chinese-pyim-basedict-enable)

如果用户的计算机性能比较好，建议从 Melpa 安装 chinese-pyim-greatdict 包,
这个词库包有 300 多万词条，是一个 *大而全* 的词库。

喜欢折腾的用户可以从下面几个途径获得 Chinese-pyim 更详细的信息。
1. 使用 `C-h v pyim-dicts' 了解 `Chinese-pyim' 词库文件格式。
2. 了解如何导入其它输入法的词库。
   1. 使用 package 管理器查看 Chinese-pyim 包的简介
   2. 阅读 chinese-pyim.el 文件 Commentary
   3. 查看 Chinese-pyim 在线 README：https://github.com/tumashu/chinese-pyim\n")
        (dolist (dict dicts-list)
          (let ((disable (plist-get dict :disable))
                (file (plist-get dict :file)))
            (insert (propertize (format format-string
                                        i (if disable "NO" "YES") file)
                                'id i 'disable disable 'file file)))
          (setq i (1+ i))))
      (insert (propertize "
操作命令：[A] 添加词库  [D] 删除词库   [P] 向上移动   [N] 向下移动  [g] 刷新页面
          [s] 保存配置  [R] 重启输入法 [C-c C-c] 禁用/启用当前词库"
                          'face face-attr)))))

(defun pyim-dm-toggle-dict (&optional enable)
  "启用当前行对应的词库。"
  (interactive)
  (when (equal (buffer-name) pyim-dm-buffer)
    (let* ((id (get-text-property (point) 'id))
           (disable (get-text-property (point) 'disable))
           (dict (cl-copy-list (nth (1- id) pyim-dicts)))
           (disable (plist-get dict :disable))
           (line (line-number-at-pos)))
      (setf (nth (1- id) pyim-dicts) (plist-put dict :disable (not disable)))
      (if (not disable)
          (message "禁用当前词库")
        (message "启用当前词库"))
      (pyim-dm-refresh)
      (goto-char (point-min))
      (forward-line (- line 1)))))

(defun pyim-dm-delete-dict ()
  "从 `pyim-dicts' 中删除当前行对应的词库信息。"
  (interactive)
  (when (equal (buffer-name) pyim-dm-buffer)
    (let ((id (get-text-property (point) 'id))
          (file (get-text-property (point) 'file))
          (line (line-number-at-pos)))
      (when (yes-or-no-p "确定要删除这条词库信息吗? ")
        (setq pyim-dicts (delq (nth (1- id) pyim-dicts) pyim-dicts))
        (pyim-dm-refresh)
        (goto-char (point-min))
        (forward-line (- line 1))))))

(defun pyim-dm-dict-position-up ()
  "向上移动词库。"
  (interactive)
  (when (equal (buffer-name) pyim-dm-buffer)
    (let* ((id (get-text-property (point) 'id))
           (dict1 (nth (- id 1) pyim-dicts))
           (dict2 (nth (- id 2) pyim-dicts))
           (length (length pyim-dicts))
           (line (line-number-at-pos)))
      (when (> id 1)
        (setf (nth (- id 1) pyim-dicts) dict2)
        (setf (nth (- id 2) pyim-dicts) dict1)
        (pyim-dm-refresh)
        (goto-char (point-min))
        (forward-line (- line 2))))))

(defun pyim-dm-dict-position-down ()
  "向下移动词库。"
  (interactive)
  (when (equal (buffer-name) pyim-dm-buffer)
    (let* ((id (get-text-property (point) 'id))
           (dict1 (nth (- id 1) pyim-dicts))
           (dict2 (nth id pyim-dicts))
           (length (length pyim-dicts))
           (line (line-number-at-pos)))
      (when (< id length)
        (setf (nth (1- id) pyim-dicts) dict2)
        (setf (nth id pyim-dicts) dict1)
        (pyim-dm-refresh)
        (goto-char (point-min))
        (forward-line line)))))

(defun pyim-dm-save-dict-info ()
  "使用 `customize-save-variable' 函数将 `pyim-dicts' 保存到 ~/.emacs 文件中。"
  (interactive)
  ;; 将`pyim-dict'的设置保存到emacs配置文件中。
  (customize-save-variable 'pyim-dicts pyim-dicts)
  (message "将 Chinese-pyim 词库配置信息保存到 ~/.emacs 文件。"))

(defun pyim-dm-add-dict ()
  "为 `pyim-dicts' 添加词库信息。"
  (interactive)
  (when (equal (buffer-name) pyim-dm-buffer)
    (let ((line (line-number-at-pos))
          dict name file coding first-used dict-type)
      (setq name (read-from-minibuffer "请输入词库名称： "))
      (setq file (read-file-name "请选择词库文件： " "~/"))
      (setq first-used  (yes-or-no-p "是否让 Chinese-pyim 优先使用词库？ "))
      (setq dict `(:name ,name :file ,file))
      (if first-used
          (add-to-list 'pyim-dicts dict)
        (add-to-list 'pyim-dicts dict t))
      (pyim-dm-refresh)
      (goto-char (point-min))
      (forward-line (- line 1)))))

(define-derived-mode pyim-dm-mode special-mode "pyim-dicts-manager"
  "Major mode for managing Chinese-pyim dicts"
  (read-only-mode)
  (define-key pyim-dm-mode-map (kbd "D") 'pyim-dm-delete-dict)
  (define-key pyim-dm-mode-map (kbd "g") 'pyim-dm-refresh)
  (define-key pyim-dm-mode-map (kbd "A") 'pyim-dm-add-dict)
  (define-key pyim-dm-mode-map (kbd "N") 'pyim-dm-dict-position-down)
  (define-key pyim-dm-mode-map (kbd "P") 'pyim-dm-dict-position-up)
  (define-key pyim-dm-mode-map (kbd "s") 'pyim-dm-save-dict-info)
  (define-key pyim-dm-mode-map (kbd "C-c C-c") 'pyim-dm-toggle-dict)
  (define-key pyim-dm-mode-map (kbd "R") 'pyim-restart))

;;;###autoload
(defun pyim-dicts-manager ()
  "Chinese-pyim 词库管理器。"
  (interactive)
  (let ((buffer (get-buffer-create pyim-dm-buffer)))
    (pyim-dm-refresh)
    (switch-to-buffer buffer)
    (pyim-dm-mode)
    (setq truncate-lines t)))

(defun pyim-extra-dicts-add-dict (new-dict)
  "添加 `new-dict' 到 `pyim-dicts', 其中 `new-dict' 的格式为：

   (:name \"XXX\" :file \"/path/to/XXX.pyim\")

这个函数用于制作 elpa 格式的词库 ，不建议普通用户使用。"
  (let (replace result)
    (dolist (dict pyim-extra-dicts)
      (if (equal (plist-get dict :name)
                 (plist-get new-dict :name))
          (progn (push new-dict result)
                 (setq replace t))
        (push dict result)))
    (setq result (reverse result))
    (setq pyim-extra-dicts
          (if replace result `(,@result ,new-dict)))
    (message "Add Chinese-pyim dict %S to `pyim-extra-dicts'。" (plist-get new-dict :name))
    t))

(defun pyim-dict-name-available-p (dict-name)
  "查询 `pyim-dicts' 中 `:name' 为 `dict-name' 的词库信息是否存在。
  这个函数主要用于词库 package。"
  (cl-some (lambda (x)
             (let ((name (plist-get x :name)))
               (equal name dict-name)))
           pyim-dicts))

(defun pyim-dict-file-available-p (dict-file)
  "查询 `pyim-dicts' 中 `:file' 为 `dict-file' 的词库信息是否存在。
  这个函数主要用于词库 package。"
  (cl-some (lambda (x)
             (let ((file (plist-get x :file)))
               (equal (expand-file-name file)
                      (expand-file-name dict-file))))
           pyim-dicts))
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'chinese-pyim-dictools)

;;; chinese-pyim-dictools.el ends here
;; #+END_SRC
