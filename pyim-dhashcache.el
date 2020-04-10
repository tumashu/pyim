;;; pyim-dhashcache --- uses hash table to cache and search dictionaries   -*- lexical-binding: t; -*-

;; * Header
;; This file uses hash table to cache and search dictionaries.

;;; Commentary:

;; * 说明文档                                                              :doc:
;; 这个文件为词典建立散列表(Hash Table)结构缓存,提供基于散列表的辞典搜索算法.
;; 搜索速度极快,消耗内存较多.
;;
;; 可以 (setq pyim-dcache-backend 'pyim-dhashcache) 然后重启输入法启用此引擎

;;; Code:
;; * 代码                                                                 :code:
(require 'pyim-common)
(require 'async nil t)

;; Pyim 词库缓存文件，注意：变量名称中不能出现 ":" 等，不能作为文件名称的字符。
(defvar pyim-dhashcache-code2word nil)
(defvar pyim-dhashcache-code2word-md5 nil)
(defvar pyim-dhashcache-word2code nil)
(defvar pyim-dhashcache-iword2count nil)
(defvar pyim-dhashcache-shortcode2word nil)
(defvar pyim-dhashcache-icode2word nil)
(defvar pyim-dhashcache-ishortcode2word nil)
(defvar pyim-dhashcache-update-shortcode2word nil)
(defvar pyim-dhashcache-update-ishortcode2word nil)
(defvar pyim-dhashcache-update-icode2word-p nil)

(defun pyim-dhashcache-sort-words (words-list)
  "对 WORDS-LIST 排序，词频大的排在前面.

排序使用 `pyim-dhashcache-iword2count' 中记录的词频信息"
  (sort words-list
        #'(lambda (a b)
            (> (or (gethash a pyim-dhashcache-iword2count) 0)
               (or (gethash b pyim-dhashcache-iword2count) 0)))))

(defun pyim-dhashcache-get-shortcode (code)
  "获取一个 CODE 的所有简写.

比如：.nihao -> .nihao .niha .nih .ni .n"
  (when (and (> (length code) 0)
             (not (string-match-p "-" code))
             (pyim-string-match-p "^[[:punct:]]" code))
    (let* ((code1 (substring code 1))
           (prefix (substring code 0 1))
           (n (length code1))
           results)
      (dotimes (i n)
        (when (> i 1)
          (push (concat prefix (substring code1 0 i)) results)))
      results)))

(defun pyim-dhashcache-update-ishortcode2word (&optional force)
  "读取 ‘pyim-dhashcache-icode2word’ 中的词库，创建 *简拼* 缓存，然后加载这个缓存.

如果 FORCE 为真，强制加载缓存。"
  (interactive)
  (when (or force (not pyim-dhashcache-update-ishortcode2word))
    (if (pyim-use-emacs-thread-p)
        (make-thread
         `(lambda ()
            (maphash
             #'(lambda (key value)
                 (let ((newkey (mapconcat
                                #'(lambda (x)
                                    (substring x 0 1))
                                (split-string key "-") "-")))
                   (puthash newkey
                            (delete-dups
                             `(,@value
                               ,@(gethash newkey pyim-dhashcache-ishortcode2word)))
                            pyim-dhashcache-ishortcode2word)))
             pyim-dhashcache-icode2word)
            (maphash
             #'(lambda (key value)
                 (puthash key (pyim-dhashcache-sort-words value)
                          pyim-dhashcache-ishortcode2word))
             pyim-dhashcache-ishortcode2word)
            (pyim-dcache-save-variable 'pyim-dhashcache-ishortcode2word)
            (setq pyim-dhashcache-update-ishortcode2word t)))
      (async-start
       `(lambda ()
          ,(async-inject-variables "^load-path$")
          ,(async-inject-variables "^exec-path$")
          ,(async-inject-variables "^pyim-.+?directory$")
          (require 'pyim-dhashcache)
          (pyim-dcache-set-variable 'pyim-dhashcache-icode2word)
          (pyim-dcache-set-variable 'pyim-dhashcache-iword2count)
          (setq pyim-dhashcache-ishortcode2word
                (make-hash-table :test #'equal))
          (maphash
           #'(lambda (key value)
               (when (and (> (length key) 0)
                          (not (string-match-p "[^a-z-]" key)))
                 (let* ((newkey (mapconcat
                                 #'(lambda (x)
                                     (substring x 0 1))
                                 (split-string key "-") "-")))
                   (puthash newkey
                            (delete-dups
                             `(,@value
                               ,@(gethash newkey pyim-dhashcache-ishortcode2word)))
                            pyim-dhashcache-ishortcode2word))))
           pyim-dhashcache-icode2word)
          (maphash
           #'(lambda (key value)
               (puthash key (pyim-dhashcache-sort-words value)
                        pyim-dhashcache-ishortcode2word))
           pyim-dhashcache-ishortcode2word)
          (pyim-dcache-save-variable 'pyim-dhashcache-ishortcode2word))
       `(lambda (result)
          (setq pyim-dhashcache-update-ishortcode2word t)
          (pyim-dcache-set-variable 'pyim-dhashcache-ishortcode2word t))))))

(defun pyim-dhashcache-update-shortcode2word (&optional force)
  "使用 `pyim-dhashcache-code2word' 中的词条，创建简写 code 词库缓存并加载.

如果 FORCE 为真，强制运行。"
  (interactive)
  (when (or force (not pyim-dhashcache-update-shortcode2word))
    (if (pyim-use-emacs-thread-p)
        (make-thread
         `(lambda ()
            (maphash
             #'(lambda (key value)
                 (dolist (x (pyim-dhashcache-get-shortcode key))
                   (puthash x
                            (mapcar
                             #'(lambda (word)
                                 (if (get-text-property 0 :comment word)
                                     word
                                   (propertize word :comment (substring key (length x)))))
                             (delete-dups `(,@value ,@(gethash x pyim-dhashcache-shortcode2word))))
                            pyim-dhashcache-shortcode2word)))
             pyim-dhashcache-code2word)
            (maphash
             #'(lambda (key value)
                 (puthash key (pyim-dhashcache-sort-words value)
                          pyim-dhashcache-shortcode2word))
             pyim-dhashcache-shortcode2word)
            (pyim-dcache-save-variable 'pyim-dhashcache-shortcode2word)
            (setq pyim-dhashcache-update-shortcode2word t)))
      (async-start
       `(lambda ()
          ,(async-inject-variables "^load-path$")
          ,(async-inject-variables "^exec-path$")
          ,(async-inject-variables "^pyim-.+?directory$")
          (require 'pyim-dhashcache)
          (pyim-dcache-set-variable 'pyim-dhashcache-code2word)
          (pyim-dcache-set-variable 'pyim-dhashcache-iword2count)
          (setq pyim-dhashcache-shortcode2word
                (make-hash-table :test #'equal))
          (maphash
           #'(lambda (key value)
               (dolist (x (pyim-dhashcache-get-shortcode key))
                 (puthash x
                          (mapcar
                           #'(lambda (word)
                               ;; 这个地方的代码用于实现五笔 code 自动提示功能，
                               ;; 比如输入 'aa' 后得到选词框：
                               ;; ----------------------
                               ;; | 1. 莁aa 2.匶wv ... |
                               ;; ----------------------
                               (if (get-text-property 0 :comment word)
                                   word
                                 (propertize word :comment (substring key (length x)))))
                           (delete-dups `(,@value ,@(gethash x pyim-dhashcache-shortcode2word))))
                          pyim-dhashcache-shortcode2word)))
           pyim-dhashcache-code2word)
          (maphash
           #'(lambda (key value)
               (puthash key (pyim-dhashcache-sort-words value)
                        pyim-dhashcache-shortcode2word))
           pyim-dhashcache-shortcode2word)
          (pyim-dcache-save-variable 'pyim-dhashcache-shortcode2word)
          nil)
       `(lambda (result)
          (setq pyim-dhashcache-update-shortcode2word t)
          (pyim-dcache-set-variable 'pyim-dhashcache-shortcode2word t))))))

(defun pyim-dhashcache-get-path (variable)
  "获取保存 VARIABLE 取值的文件的路径."
  (when (symbolp variable)
    (concat (file-name-as-directory pyim-dcache-directory)
            (symbol-name variable))))

(defun pyim-dhashcache-generate-dcache-file (dict-files dcache-file)
  "读取词库文件列表：DICT-FILES, 生成一个词库缓冲文件 DCACHE-FILE.

pyim 使用的词库文件是简单的文本文件，编码 *强制* 为 'utf-8-unix,
其结构类似：

  ni-bu-hao 你不好
  ni-hao  你好 妮好 你豪

第一个空白字符之前的内容为 code，空白字符之后为中文词条列表。词库
*不处理* 中文标点符号。"
  (let ((hashtable (make-hash-table :size 1000000 :test #'equal)))
    (dolist (file dict-files)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents file))
        (goto-char (point-min))
        (forward-line 1)
        (while (not (eobp))
          (let* ((content (pyim-dline-parse))
                 (code (car content))
                 (words (cdr content)))
            (when (and code words)
              (puthash code
                       (delete-dups `(,@(gethash code hashtable) ,@words))
                       hashtable)))
          (forward-line 1))))
    (pyim-dcache-save-value-to-file hashtable dcache-file)
    hashtable))

(defun pyim-dhashcache-generate-word2code-dcache-file (dcache file)
  "从 DCACHE 生成一个 word -> code 的反向查询表.
DCACHE 是一个 code -> words 的 hashtable.
并将生成的表保存到 FILE 中."
  (when (hash-table-p dcache)
    (let ((hashtable (make-hash-table :size 1000000 :test #'equal)))
      (maphash
       #'(lambda (code words)
           (unless (pyim-string-match-p "-" code)
             (dolist (word words)
               (let ((value (gethash word hashtable)))
                 (puthash word
                          (if value
                              `(,code ,@value)
                            (list code))
                          hashtable)))))
       dcache)
      (pyim-dcache-save-value-to-file hashtable file))))

(defun pyim-dhashcache-update-code2word (dict-files dicts-md5 &optional force)
  "读取并加载词库.

读取 `pyim-dicts' 和 `pyim-extra-dicts' 里面的词库文件，生成对应的
词库缓冲文件，然后加载词库缓存。

如果 FORCE 为真，强制加载。"
  (interactive)
  (let* ((code2word-file (pyim-dhashcache-get-path 'pyim-dhashcache-code2word))
         (word2code-file (pyim-dhashcache-get-path 'pyim-dhashcache-word2code))
         (code2word-md5-file (pyim-dhashcache-get-path 'pyim-dhashcache-code2word-md5)))
    (when (or force (not (equal dicts-md5 (pyim-dcache-get-value-from-file code2word-md5-file))))
      ;; use hashtable
      (if (pyim-use-emacs-thread-p)
          (make-thread
           `(lambda ()
              (let ((dcache (pyim-dhashcache-generate-dcache-file ',dict-files ,code2word-file)))
                (pyim-dhashcache-generate-word2code-dcache-file dcache ,word2code-file))
              (pyim-dcache-save-value-to-file ',dicts-md5 ,code2word-md5-file)
              (pyim-dcache-set-variable 'pyim-dhashcache-code2word t)
              (pyim-dcache-set-variable 'pyim-dhashcache-word2code t)))
        (async-start
         `(lambda ()
            ,(async-inject-variables "^load-path$")
            ,(async-inject-variables "^exec-path$")
            ,(async-inject-variables "^pyim-.+?directory$")
            (require 'pyim-dhashcache)
            (let ((dcache (pyim-dhashcache-generate-dcache-file ',dict-files ,code2word-file)))
              (pyim-dhashcache-generate-word2code-dcache-file dcache ,word2code-file))
            (pyim-dcache-save-value-to-file ',dicts-md5 ,code2word-md5-file))
         `(lambda (result)
            (pyim-dcache-set-variable 'pyim-dhashcache-code2word t)
            (pyim-dcache-set-variable 'pyim-dhashcache-word2code t)))))))

(defun pyim-dhashcache-export (dcache file &optional confirm)
  "将一个 pyim DCACHE 导出为文件 FILE.

如果 CONFIRM 为 non-nil，文件存在时将会提示用户是否覆盖，
默认为覆盖模式"
  (with-temp-buffer
    (insert ";;; -*- coding: utf-8-unix -*-\n")
    (maphash
     #'(lambda (key value)
         (insert (format "%s %s\n"
                         key
                         (if (listp value)
                             (mapconcat #'identity value " ")
                           value))))
     dcache)
    (pyim-dcache-write-file file confirm)))

(declare-function pyim-pinyin2cchar-get "pyim" (pinyin &optional equal-match return-list include-seperator))

(defun pyim-dhashcache-get (code &optional from)
  "从 FROM 对应的 dcaches 中搜索 CODE, 得到对应的词条.

当词库文件加载完成后，pyim 就可以用这个函数从词库缓存中搜索某个
code 对应的中文词条了。

如果 FROM 为 nil, 则默认搜索 `pyim-dhashcache-icode2word' 和
`pyim-dhashcache-code2word' 两个 dcache."
  (let* ((caches (mapcar (lambda (x)
                           (intern (concat "pyim-dhashcache-" (symbol-name x))))
                         (or (and from
                                  (if (listp from)
                                      from
                                    (list from)))
                             '(icode2word code2word))))
         result)
    (dolist (cache caches)
      (let* ((cache (ignore-errors (symbol-value cache)))
             (value (and cache (gethash code cache))))
        (when value
          (setq result (append result value)))))
    `(,@result ,@(pyim-pinyin2cchar-get code t t))))

(defun pyim-dhashcache-update-icode2word (&optional force)
  "对 personal 缓存中的词条进行排序，加载排序后的结果.

在这个过程中使用了 `pyim-dhashcache-iword2count' 中记录的词频信息。
如果 FORCE 为真，强制排序。"
  (interactive)
  (when (or force (not pyim-dhashcache-update-icode2word-p))
    (if (pyim-use-emacs-thread-p)
        (make-thread
         `(lambda ()
            (maphash
             #'(lambda (key value)
                 (puthash key (pyim-dhashcache-sort-words value)
                          pyim-dhashcache-icode2word))
             pyim-dhashcache-icode2word)
            (pyim-dcache-save-variable 'pyim-dhashcache-icode2word)
            (setq pyim-dhashcache-update-icode2word-p t)))
      (async-start
       `(lambda ()
          ,(async-inject-variables "^load-path$")
          ,(async-inject-variables "^exec-path$")
          ,(async-inject-variables "^pyim-.+?directory$")
          (require 'pyim-dhashcache)
          (pyim-dcache-set-variable 'pyim-dhashcache-icode2word)
          (pyim-dcache-set-variable 'pyim-dhashcache-iword2count)
          (maphash
           #'(lambda (key value)
               (puthash key (pyim-dhashcache-sort-words value)
                        pyim-dhashcache-icode2word))
           pyim-dhashcache-icode2word)
          (pyim-dcache-save-variable 'pyim-dhashcache-icode2word)
          nil)
       `(lambda (result)
          (setq pyim-dhashcache-update-icode2word-p t)
          (pyim-dcache-set-variable 'pyim-dhashcache-icode2word t))))))

(defun pyim-dhashcache-update-personal-words (&optional force)
  (pyim-dhashcache-update-icode2word force)
  (pyim-dhashcache-update-ishortcode2word force))

(defun pyim-dhashcache-init-variables ()
  "初始化 dcache 缓存相关变量."
  (pyim-dcache-set-variable
   'pyim-dhashcache-iword2count nil
   ;; 添加 dregcache 后端之后， 原来的 dcache 更名为 dhashcache,
   ;; 升级迁移
   (pyim-dcache-get-variable 'pyim-dcache-iword2count))
  (pyim-dcache-set-variable 'pyim-dhashcache-code2word)
  (pyim-dcache-set-variable 'pyim-dhashcache-word2code)
  (pyim-dcache-set-variable 'pyim-dhashcache-shortcode2word)
  (pyim-dcache-set-variable
   'pyim-dhashcache-icode2word nil
   ;; 添加 dregcache 后端之后， 原来的 dcache 更名为 dhashcache,
   ;; 升级迁移
   (pyim-dcache-get-variable 'pyim-dcache-icode2word))
  (pyim-dcache-set-variable
   'pyim-dhashcache-ishortcode2word nil
   ;; 添加 dregcache 后端之后， 原来的 dcache 更名为 dhashcache,
   ;; 升级迁移
   (pyim-dcache-get-variable 'pyim-dcache-ishortcode2word)))

(defun pyim-dhashcache-save-personal-dcache-to-file ()
  ;; 用户选择过的词
  (pyim-dcache-save-variable 'pyim-dhashcache-icode2word)
  ;; 词频
  (pyim-dcache-save-variable 'pyim-dhashcache-iword2count))

(defun pyim-dhashcache-insert-export-content ()
  (maphash
   #'(lambda (key value)
       (insert (format "%s %s\n" key value)))
   pyim-dhashcache-iword2count)
  ;; 在默认情况下，用户选择过的词生成的缓存中存在的词条，
  ;; `pyim-dhashcache-iword2count' 中也一定存在，但如果用户
  ;; 使用了特殊的方式给用户选择过的词生成的缓存中添加了
  ;; 词条，那么就需要将这些词条也导出，且设置词频为 0
  (maphash
   #'(lambda (_ words)
       (dolist (word words)
         (unless (gethash word pyim-dhashcache-iword2count)
           (insert (format "%s %s\n" word 0)))))
   pyim-dhashcache-icode2word))

(defmacro pyim-dhashcache-put (cache code &rest body)
  "这个用于保存词条，删除词条以及调整词条位置."
  (declare (indent 0))
  (let ((key (make-symbol "key"))
        (table (make-symbol "table"))
        (new-value (make-symbol "new-value")))
    `(let* ((,key ,code)
            (,table ,cache)
            (orig-value (gethash ,key ,table))
            ,new-value)
       (setq ,new-value (progn ,@body))
       (unless (equal orig-value ,new-value)
         (puthash ,key ,new-value ,table)))))

(defun pyim-dhashcache-update-iword2count (word &optional _prepend wordcount-handler)
  "保存词频到缓存."
  (pyim-dhashcache-put
    pyim-dhashcache-iword2count word
    (cond
     ((functionp wordcount-handler)
      (funcall wordcount-handler orig-value))
     ((numberp wordcount-handler)
      wordcount-handler)
     (t (+ (or orig-value 0) 1)))))

(defun pyim-dhashcache-delete-word (word)
  "将中文词条 WORD 从个人词库中删除"
  (maphash
   (lambda (key value)
     (when (member word value)
       (puthash key (remove word value)
                pyim-dhashcache-icode2word)))
   pyim-dhashcache-icode2word)
  (remhash word pyim-dhashcache-iword2count))

(declare-function pyim-list-merge "pyim" (a b))

(defun pyim-dhashcache-insert-word-into-icode2word (word pinyin prepend)
  "保存个人词到缓存."
  (pyim-dhashcache-put pyim-dhashcache-icode2word
                       pinyin
                       (if prepend (pyim-list-merge word orig-value)
                         (pyim-list-merge orig-value word))))

(defun pyim-dhashcache-search-word-code (string)
  (gethash string pyim-dhashcache-word2code))

(defun pyim-dhashcache-export-personal-words (file &optional confirm)
  "导出个人词库到 FILE."
  (pyim-dhashcache-export pyim-dhashcache-icode2word file confirm))

;; * Footer

(provide 'pyim-dhashcache)
;;; pyim-dhashcache.el ends here
