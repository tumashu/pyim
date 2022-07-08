;;; pyim-dregcache --- map dictionary to plain cache and use regular expression to search     -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2019-2021 Free Software Foundation, Inc.

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; Maintainer: Chen Bin <chenbin.sh@gmail.com>
;;             Feng Shu <tumashu@163.com>
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

;; * 说明文档                                                              :doc:
;; 这个文件将词典直接映射到内存.使用正则表达式(Regular Expression)搜索词库.
;; 搜索速度较快,消耗内存极少.
;;
;; 可以 (setq pyim-dcache-backend 'pyim-dregcache) 然后重启输入法启用此引擎

;;; Code:
;; * 代码                                                                 :code:
(require 'pyim-common)
(require 'pyim-dict)
(require 'subr-x)
(require 'pyim-dcache)

(defvar pyim-dregcache-cache nil)
(defvar pyim-dregcache-loaded-dict-files nil)
(defvar pyim-dregcache-icode2word nil)
(defvar pyim-dregcache-iword2count nil)
(defvar pyim-dregcache-dicts-md5 nil)

;; ** 获取当前可用后端
(cl-defmethod pyim-dcache-backend
  (&context (pyim-dcache-backend (eql pyim-dregcache)))
  "返回当前可用的 dcache backend."
  (if (and (featurep 'pyim-dregcache)
           ;; pyim-dregcache 后端目前只支持全拼或者双拼。
           (pyim-scheme-quanpin-p (pyim-scheme-current)))
      'pyim-dregcache
    'pyim-dhashcache))

;; ** 初始化 dregcache 相关函数
(cl-defmethod pyim-dcache-init-variables
  (&context ((pyim-dcache-backend) (eql pyim-dregcache)))
  "初始化 cache 缓存相关变量."
  (pyim-dcache-init-variable
   pyim-dregcache-iword2count
   ;; dregcache 引擎也需要词频信息，第一次使用 dregcache 引擎的时候，
   ;; 自动导入 dhashcache 引擎的词频信息，以后两个引擎的词频信息就
   ;; 完全分开了。
   (pyim-dcache-get-value 'pyim-dhashcache-iword2count))
  (unless pyim-dregcache-icode2word
    (pyim-dregcache--update-personal-words t)))

;; ** 从 dregcache 搜索词条相关函数
(cl-defmethod pyim-dcache-get
  (key &context ((pyim-dcache-backend) (eql pyim-dregcache))
       &optional from)
  "从 FROM 中搜索 KEY, 得到对应的结果。

用于 pyim-dregcache 类型的 dcache 后端。"
  (setq from (or from '(icode2word code2word)))
  (when key
    (let (value result)
      (dolist (x from)
        (setq value (pyim-dregcache--get-key-from-dcache-type key x))
        ;; 处理 iword2count.
        (unless (listp value)
          (setq value (list value)))
        (when value
          (setq result (append result value))))
      result)))

(defun pyim-dregcache--get-key-from-dcache-type (key dcache-type)
  (cond ((memq dcache-type '(icode2word ishortcode2word))
         (pyim-dregcache--get-icode2word-ishortcode2word key))
        ((memq dcache-type '(code2word shortcode2word))
         (pyim-dregcache--get-code2word-shortcode2word key))
        ((memq dcache-type '(iword2count))
         (gethash key pyim-dregcache-iword2count))
        ;; FIXME: pyim-dregcache 暂时不支持 iword2count-recent-10-words 和
        ;; iword2count-recent-50-words.
        ((memq dcache-type '(iword2count-recent-10-words iword2count-recent-50-words))
         nil)
        ;; pyim-dregcache 目前只能用于全拼输入法，而 pyim 中全拼输入法代码反查
        ;; 功能是通过 pymap 相关函数实现的，不使用 word2code.
        ((memq dcache-type '(word2code))
         nil)
        (t nil)))

(defun pyim-dregcache--get-code2word-shortcode2word (code)
  (let ((dict-files (pyim-dregcache-cached-dict-files))
        result)

    (when pyim-debug (message "pyim-dregcache--get-code2word-shortcode2word called. code=%s dict-files=%s"
                              code
                              dict-files))
    (when pyim-debug (message "pyim-dregcache--get is called. code=%s" code))
    (when dict-files
      (dolist (file dict-files)
        ;; please note file is a symbol, not string
        (let* ((file-info (plist-get pyim-dregcache-cache file))
               (content (pyim-dregcache--get-content code file-info)))
          (setq result (append (pyim-dregcache--get-1 content code) result)))))
    ;; `push' plus `nreverse' is more efficient than `add-to-list'
    ;; Many examples exist in Emacs' own code
    (nreverse result)))

(defun pyim-dregcache--get-icode2word-ishortcode2word (code)
  "以 CODE 搜索个人词和个人联想词.  正则表达式搜索词库,不需要为联想词开单独缓存."
  (when pyim-debug (message "pyim-dregcache--get-icode2word-ishortcode2word called => %s" code))
  (when pyim-dregcache-icode2word
    (nreverse (pyim-dregcache--get-1 pyim-dregcache-icode2word code))))

(defmacro pyim-dregcache--match-line (code)
  `(concat "^" (pyim-dregcache--code2regexp ,code) " \\(.+\\)"))

(defun pyim-dregcache--get-1 (content code)
  (let ((case-fold-search t)
        (start 0)
        (pattern (pyim-dregcache--match-line code))
        (content-length (length content))
        word
        output)
    (while (and (< start content-length)
                (setq start (string-match pattern content start)))
      ;; 提取词
      (setq word (match-string-no-properties 1 content))
      (when word
        (cond
         ((string-match "^[^ ]+$" word)
          ;; 单个词
          (push word output))
         (t
          ;; 多个字
          (setq output (append (nreverse (split-string word " +")) output)))))
      ;; 继续搜索
      (setq start (+ start 2 (length code) (length word))))
    output))

(defmacro pyim-dregcache--is-shenmu (code)
  "判断CODE 是否是一个声母."
  `(and (eq (length ,code) 1)
        (not (string-match ,code "aeo"))))

(defmacro pyim-dregcache--shenmu2regexp (char)
  "将声母 CHAR 转换为通用正则表达式匹配所有以该声母开头的汉字."
  `(concat ,char "[a-z]*"))

(defun pyim-dregcache--code2regexp (code)
  "将 CODE 转换成正则表达式用来搜索辞典缓存中的匹配项目.
单个声母会匹配所有以此生母开头的单个汉字."
  (let* (arr)
    (cond
     ((pyim-dregcache--is-shenmu code)
      ;; 用户输入单个声母如 "w", "y", 将该声母转换为
      ;; 通用正则表达式匹配所有以该声母开头的汉字
      (pyim-dregcache--shenmu2regexp code))

     ((eq (length (setq arr (split-string code "-"))) 1)
      ;; 用户输入单个汉字的声母和韵母如 "wan", "dan", 不做任何处理
      code)

     (t
      ;; 用户输入多字词的code,如果某个字只提供了首个声母,将该声母转换为
      ;; 通用正则表达式匹配所有以该声母开头的汉字
      (let* ((s (mapconcat (lambda (e)
                             (if (pyim-dregcache--is-shenmu e)
                                 (pyim-dregcache--shenmu2regexp e)
                               e))
                           arr
                           "-")))
        ;; 当输入3字拼音时,也搜索前3字包含此拼音的更长的词.
        ;; 三字词很少(可 grep -rEsoh "^[a-z]+(-[a-z]+){2}( [^ ]+){2,6}" 验证)
        ;; 用户通常输入4字或更多字的词.
        ;; 例如符合 bei-jin-tian 的词很少, "北京天安门"是更可能匹配的词
        ;; `pyim-dregcache' 搜索算法可以保证更长的词在靠后的位置
        (cond
         ((< (length arr) 3)
          s)
         ((eq ?* (elt s (1- (length s))))
          ;; 简化正则表达式, tian-an-m[a-z]* => tian-an-m[a-z]?[a-z-]*
          (concat (substring s 0 (1- (length s))) "?[a-z-]*"))
         (t
          ;; tian-an-men => tian-an-men[a-z-]*
          (concat s "[a-z-]*"))))))))

(defun pyim-dregcache-cached-dict-files ()
  "所有词典文件."
  pyim-dregcache-loaded-dict-files)

(defun pyim-dregcache--get-content (code file-info)
  "找到 CODE 对应的字典子缓冲区.  FILE-INFO 是字典文件的所有信息."
  (let* ((rlt (plist-get file-info :content))
         idx
         (ch (elt code 0)))
    (cond
     ((< ch ?i)
      (setq idx (- ch ?a)))
     ((< ch ?u)
      ;; 'i' could not be first character of pinyin code
      (setq idx (- ch ?a 1)))
     (t
      ;; 'i', 'u', 'v' could not be first character of pinyin code
      (setq idx (- ch ?a 3))))
    ;; fetch segment using the first character of pinyin code
    (nth idx rlt)))

;; ** 给 dregcache 添加词条相关函数
(cl-defmethod pyim-dcache-insert-word
  (word code prepend
        &context ((pyim-dcache-backend) (eql pyim-dregcache)))
  "将词条 WORD 插入到 `pyim-dregcache-icode2word'."
  (pyim-dregcache--insert-word-into-icode2word word code prepend))

(defun pyim-dregcache--insert-word-into-icode2word (word code prepend)
  "保存个人词到缓存,和其他词库格式一样以共享正则搜索算法."
  (when pyim-debug
    (message "pyim-dregcache--insert-word-into-icode2word called => %s %s %s"
             word
             code
             prepend))
  (with-temp-buffer
    (when pyim-dregcache-icode2word
      (insert pyim-dregcache-icode2word))
    (goto-char (point-min))
    (let* ((case-fold-search t)
           substring replace-string beg end old-word-list)
      (if (re-search-forward (concat "^" code " \\(.*\\)") nil t)
          (progn
            (setq beg (match-beginning 0))
            (setq end (match-end 0))
            (setq substring (match-string-no-properties 1))
            (delete-region beg end)
            ;; 这里不进行排序，在pyim-dregcache--update-personal-words排序
            (setq old-word-list (pyim-dregcache--sort-words (split-string substring " ")))
            (setq replace-string (concat code " " (string-join (delete-dups `(,@old-word-list ,word)) " "))))
        (setq replace-string (concat code " " (or replace-string word) "\n")))
      (goto-char (or beg (point-max)))
      (insert replace-string))
    (setq pyim-dregcache-icode2word
          (buffer-string))))

;; ** 从 dregcache 删除词条相关函数
(cl-defmethod pyim-dcache-delete-word
  (word &context ((pyim-dcache-backend) (eql pyim-dregcache)))
  "将中文词条 WORD 从个人词库中删除."
  (with-temp-buffer
    (insert pyim-dregcache-icode2word)
    (goto-char (point-min))
    (let* ((case-fold-search t)
           substring beg end)
      (while (re-search-forward (concat "^\\([a-z-]+\\) \\(.*\\)" word "\\(.*\\)$") nil t)
        (setq beg (match-beginning 0))
        (setq end (match-end 0))
        (setq substring (concat (match-string-no-properties 1)
                                (match-string-no-properties 2)
                                (match-string-no-properties 3)))

        ;; delete string and the newline char
        (delete-region beg (+ 1 end))
        (when (> (length (split-string substring " ")) 1)
          (goto-char beg)
          (insert substring)))
      (setq pyim-dregcache-icode2word
            (buffer-string))))
  ;; 删除对应词条的词频
  (remhash word pyim-dregcache-iword2count))

;; ** 更新 dregcache 相关函数
(cl-defmethod pyim-dcache-update
  (&context ((pyim-dcache-backend) (eql pyim-dregcache)) &optional force)
  "读取并加载所有相关词库 dcache.

如果 FORCE 为真，强制加载。"
  (pyim-dregcache--update-personal-words force)
  (let* ((dict-files (pyim-dict-get-enabled-dict-files))
         (dicts-md5 (pyim-dcache-create-files-md5 dict-files)))
    (when pyim-debug
      (message "pyim-dregcache--update: pyim-dicts=%s pyim-extra-dicts=%s dict-files=%s"
               pyim-dicts
               pyim-extra-dicts
               dict-files))
    (pyim-dregcache--update-code2word dict-files dicts-md5 force)))

(defun pyim-dregcache--update-personal-words (&optional force)
  "合并 `pyim-dregcache-icode2word' 磁盘文件. 加载排序后的结果.

在这个过程中使用了 `pyim-dregcache-iword2count' 中记录的词频信息。
如果 FORCE 为真，强制排序"
  (let* ((content (pyim-dregcache--load-variable 'pyim-dregcache-icode2word)))
    (when content
      (with-temp-buffer
        (let* (prev-record prev-code record code prev-point)
          (when pyim-dregcache-icode2word
            (insert pyim-dregcache-icode2word))
          (insert content)
          (sort-lines nil (point-min) (point-max))
          (delete-duplicate-lines (point-min) (point-max) nil t nil)
          (goto-char (point-min))
          (unless (re-search-forward "utf-8-unix" (line-end-position) t)
            (insert "## -*- coding: utf-8-unix -*-\n"))
          (goto-char (point-min))
          (while (not (eobp))
            ;; initiate prev-point and prev-line
            (goto-char (line-beginning-position))
            (setq prev-point (point))
            (setq prev-record (pyim-dline-parse))
            (setq prev-code (car prev-record))
            ;; 词库在创建时已经保证1个code只有1行，因此合并词库文件一般只有2行需要合并
            ;; 所以这里没有对2行以上的合并进行优化
            (when (= (forward-line 1) 0)
              (setq record (pyim-dline-parse))
              (setq code (car record))
              (when (string-equal prev-code code)
                ;; line-end-position donesn't contain "\n"
                (progn (delete-region prev-point (line-end-position))
                       (insert (string-join (delete-dups `(,@prev-record ,@record)) " ")))))))
        (setq pyim-dregcache-icode2word (buffer-string)))))
  (when (and force pyim-dregcache-icode2word)
    (pyim-dregcache--sort-icode2word)))

(defun pyim-dregcache--load-variable (variable)
  "载入 VARIABLE 对应的文件内容."
  (let* ((file (pyim-dregcache--variable-file variable)))
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun pyim-dregcache--variable-file (variable)
  "Get VARIABLE dcache file path."
  (concat (file-name-as-directory pyim-dcache-directory)
          (symbol-name variable)))

(defun pyim-dregcache--update-code2word (dict-files dicts-md5 &optional force)
  "读取并加载词库.

读取词库文件 DICT-FILES，生成对应的词库缓冲文件，然后加载词库缓存。

DICT-FILES 是词库文件列表. DICTS-MD5 是词库的MD5校验码.

如果 FORCE 为真，强制加载。"
  (interactive)
  (when (or force (not (equal dicts-md5 pyim-dregcache-dicts-md5)))
    ;; no hashtable i file mapping algorithm
    (dolist (file dict-files)
      (pyim-dregcache--load-dictionary-file file))
    (setq pyim-dregcache-dicts-md5 dicts-md5)))

(defun pyim-dregcache-reset-cache ()
  "Reset dregcache backend's cache."
  (setq pyim-dregcache-cache nil)
  (setq pyim-dregcache-loaded-dict-files nil))

(defun pyim-dregcache--load-dictionary-file (dict-file)
  "READ from DICT-FILE."
  (let* ((raw-content (with-temp-buffer
                        (insert-file-contents dict-file)
                        (buffer-string)))
         (file (intern (file-truename dict-file))))
    (unless (memq file pyim-dregcache-loaded-dict-files)
      (push file pyim-dregcache-loaded-dict-files))
    (setq pyim-dregcache-cache
          (plist-put pyim-dregcache-cache
                     file
                     (pyim-dregcache--create-cache-content raw-content)))))

(defun pyim-dregcache--create-cache-content (raw-content)
  "将 RAW-CONTENT 划分成可以更高效搜索的缓冲区."
  (let ((chars "bcdefghjklmnopqrstwxyz")
        (i 0)
        content-segments
        (start (string-match "^a" raw-content))
        chunk
        end)
    ;; 将字典缓存划分成多个"子搜索区域"
    (while (< i (length chars))
      (when (setq end (string-match (string ?^ (elt chars i))
                                    raw-content
                                    start))
        (setq chunk (substring-no-properties raw-content start end))
        (push chunk content-segments)
        (setq start end))
      (setq i (1+ i)))

    ;; last chunk
    (setq chunk (substring-no-properties raw-content end (length raw-content)))
    (push chunk content-segments)
    (list :content (nreverse content-segments))))

;; ** 更新 dregcache 词频功能。
(cl-defmethod pyim-dcache-update-wordcount
  (word &context ((pyim-dcache-backend) (eql pyim-dregcache))
        &optional wordcount-handler)
  (pyim-dregcache--update-iword2count word wordcount-handler))

(defun pyim-dregcache--update-iword2count (word &optional wordcount-handler)
  "保存词频到缓存."
  (when pyim-debug (message "pyim-dregcache--update-iword2count. word=%s" word))
  (let* ((orig-value
          (or (gethash word pyim-dregcache-iword2count) 0))
         (new-value (cond
                     ((functionp wordcount-handler)
                      (funcall wordcount-handler orig-value))
                     ((numberp wordcount-handler)
                      wordcount-handler)
                     (t (+ orig-value 1)))))
    (unless (equal orig-value new-value)
      (puthash word new-value pyim-dregcache-iword2count))))

;; ** 升级 dregcache 相关函数
(cl-defmethod pyim-dcache-upgrade (&context ((pyim-dcache-backend) (eql pyim-dregcache)))
  "升级词库缓存.

当前已有的功能：
1. 基于 :code-prefix-history 信息，升级为新的 code-prefix。"
  (pyim-dregcache--upgrade-icode2word))

(defun pyim-dregcache--upgrade-icode2word ()
  "升级 icode2word 缓存。

dregcache 只支持全拼和双拼，不能用于五笔之类的型码输入法，而
update-icode2word 目前只要是用于更新型码输入法的 code-prefix, 所
以不需要具体实现细节。")

;; ** 根据 dregcache 信息对词条进行排序
(defun pyim-dregcache--sort-words (words-list)
  "对 WORDS-LIST 排序，词频大的排在前面."
  (let ((iword2count pyim-dregcache-iword2count))
    (sort words-list
          (lambda (a b)
            (let ((a (car (split-string a ":")))
                  (b (car (split-string b ":"))))
              (> (or (gethash a iword2count) 0)
                 (or (gethash b iword2count) 0)))))))

;; ** 保存 dregcache 相关函数
(cl-defmethod pyim-dcache-save-caches
  (&context ((pyim-dcache-backend) (eql pyim-dregcache)))
  (pyim-dregcache--save-personal-dcache-to-file))

(defun pyim-dregcache--save-personal-dcache-to-file ()
  "保存缓存内容到默认目录."
  (when pyim-debug (message "pyim-dregcache--save-personal-dcache-to-file called"))
  ;; 用户选择过的词存为标准辞典格式保存
  (when pyim-dregcache-icode2word
    (pyim-dregcache--save-variable
     'pyim-dregcache-icode2word
     pyim-dregcache-icode2word))
  ;; 词频
  (pyim-dcache-save-variable
   'pyim-dregcache-iword2count
   pyim-dregcache-iword2count))

(defun pyim-dregcache--save-variable (variable value)
  "Save VARIABLE with its VALUE."
  (let* ((file (pyim-dregcache--variable-file variable))
         (save-silently t))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (insert value)
      (pyim-dcache-write-file file))))

;; ** 导出 dregcache 相关函数
(cl-defmethod pyim-dcache-export-words-and-counts
  (_file &context ((pyim-dcache-backend) (eql pyim-dregcache))
         &optional _confirm)
  "TODO")

(cl-defmethod pyim-dcache-export-personal-words
  (file &context ((pyim-dcache-backend) (eql pyim-dregcache))
        &optional confirm)
  "将个人词库存入 FILE."
  (when pyim-dregcache-icode2word
    ;; 按词频排序，把词频信息保存到用户词典
    (pyim-dregcache--sort-icode2word)
    (with-temp-buffer
      (insert pyim-dregcache-icode2word)
      ;; 删除单字词
      (goto-char (point-min))
      (while (re-search-forward "^[a-z]+ [^a-z]*" nil t)
        (replace-match "" nil t))
      ;; 按拼音排序
      (sort-lines nil (point-min) (point-max))
      (pyim-dcache-write-file file confirm))))

(defun pyim-dregcache--sort-icode2word ()
  "对个人词库排序."
  ;; https://github.com/redguardtoo/zhfreq
  (with-temp-buffer
    (dolist (l (split-string pyim-dregcache-icode2word "\n"))
      (cond
       ((string-match "^\\([a-z-]+ \\)\\(.*\\)" l)
        ;; 3字以上词很少，如果只处理单字,2字词,3字词
        ;; ((string-match "^\\([a-z]+ \\|[a-z]+-[a-z]+ \\|[a-z]+-[a-z]+-[a-z]+ \\)\\(.*\\)" l)
        (let* ((pinyin (match-string 1 l))
               (words (pyim-dregcache--sort-words (split-string (match-string 2 l) " "))))
          (insert (format "%s\n" (concat pinyin (string-join words " "))))))
       ;; 其他词
       ((string= l "")
        ;; skip empty line
        )
       (t
        (insert (format "%s\n" l)))))
    (setq pyim-dregcache-icode2word (buffer-string))))


;; * Footer

(provide 'pyim-dregcache)
;;; pyim-dregcache.el ends here
