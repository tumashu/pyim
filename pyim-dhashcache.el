;;; pyim-dhashcache --- uses hash table to cache and search dictionaries

;; * Header
;; This file uses hash table to cache and search dictionaries.

;;; Commentary:

;; * 說明文檔                                                              :doc:
;; 這個文件為詞典建立散列表 (Hash Table) 結構緩存，提供基於散列表的辭典搜索演算法。
;; 搜索速度極快，消耗記憶體較多。
;;
;; 可以 (setq pyim-dcache-backend 'pyim-dhashcache) 然後重啟輸入法啟用此引擎

;;; Code:
;; * 代碼                                                                 :code:
(require 'pyim-common)
(require 'async nil t)

;; Pyim 詞庫緩存文件，注意：變數名稱中不能出現 ":" 等，不能作為文件名稱的字元。
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
  "對 WORDS-LIST 排序，詞頻大的排在前面。

排序使用 `pyim-dhashcache-iword2count' 中記錄的詞頻信息"
  (sort words-list
        #'(lambda (a b)
            (let ((a (car (split-string a ":")))
                  (b (car (split-string b ":"))))
              (> (or (gethash a pyim-dhashcache-iword2count) 0)
                 (or (gethash b pyim-dhashcache-iword2count) 0))))))

(defun pyim-dhashcache-get-shortcode (code)
  "獲取一個 CODE 的所有簡寫。

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
  "讀取 ‘pyim-dhashcache-icode2word’ 中的詞庫，創建 *簡拼* 緩存，然後加載這個緩存。

如果 FORCE 為真，強制加載緩存。"
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
          (pyim-dcache-save-variable 'pyim-dhashcache-ishortcode2word))
       `(lambda (result)
          (setq pyim-dhashcache-update-ishortcode2word t)
          (pyim-dcache-set-variable 'pyim-dhashcache-ishortcode2word t))))))

(defun pyim-dhashcache-update-shortcode2word (&optional force)
  "使用 `pyim-dhashcache-code2word' 中的詞條，創建簡寫 code 詞庫緩存並加載。

如果 FORCE 為真，強制運行。"
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
                                 (if (string-match-p ":"  word)
                                     word
                                   (concat word ":" (substring key (length x)))))
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
                               ;; 這個地方的代碼用於實現五筆 code 自動提示功能，
                               ;; 比如輸入 'aa' 後得到選詞框：
                               ;; ----------------------
                               ;; | 1. 莁aa 2.匶wv ... |
                               ;; ----------------------
                               (if (string-match-p ":"  word)
                                   word
                                 (concat word ":" (substring key (length x)))))
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
  "獲取保存 VARIABLE 取值的文件的路徑。"
  (when (symbolp variable)
    (concat (file-name-as-directory pyim-dcache-directory)
            (symbol-name variable))))

(defun pyim-dhashcache-generate-dcache-file (dict-files dcache-file)
  "讀取詞庫文件列表：DICT-FILES，生成一個詞庫緩沖文件 DCACHE-FILE。

pyim 使用的詞庫文件是簡單的文本文件，編碼 *強制* 為 'utf-8-unix，
其結構類似：

  ni-bu-hao 你不好
  ni-hao  你好 妮好 你豪

第一個空白字元之前的內容為 code，空白字元之後為中文詞條列表。詞庫
*不處理* 中文標點符號。"
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
  "從 DCACHE 生成一個 word -> code 的反向查詢表。
DCACHE 是一個 code -> words 的 hashtable。
並將生成的表保存到 FILE 中。"
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
  "讀取並加載詞庫。

讀取 `pyim-dicts' 和 `pyim-extra-dicts' 裡面的詞庫文件，生成對應的
詞庫緩沖文件，然後加載詞庫緩存。

如果 FORCE 為真，強制加載。"
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
  "將一個 pyim DCACHE 導出為文件 FILE。

如果 CONFIRM 為 non-nil，文件存在時將會提示用戶是否覆蓋，
預設為覆蓋模式"
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

(defun pyim-dhashcache-get (code &optional from)
  "從 FROM 對應的 dcaches 中搜索 CODE，得到對應的詞條。

當詞庫文件加載完成後，pyim 就可以用這個函數從詞庫緩存中搜索某個
code 對應的中文詞條了。

如果 FROM 為 nil，則預設搜索 `pyim-dhashcache-icode2word' 和
`pyim-dhashcache-code2word' 兩個 dcache。"
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
  "對 personal 緩存中的詞條進行排序，加載排序後的結果。

在這個過程中使用了 `pyim-dhashcache-iword2count' 中記錄的詞頻信息。
如果 FORCE 為真，強制排序。"
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
  "初始化 dcache 緩存相關變數。"
  (pyim-dcache-set-variable
   'pyim-dhashcache-iword2count nil
   ;; 添加 dregcache 後端之後，原來的 dcache 更名為 dhashcache，
   ;; 升級遷移
   (pyim-dcache-get-variable 'pyim-dcache-iword2count))
  (pyim-dcache-set-variable 'pyim-dhashcache-code2word)
  (pyim-dcache-set-variable 'pyim-dhashcache-word2code)
  (pyim-dcache-set-variable 'pyim-dhashcache-shortcode2word)
  (pyim-dcache-set-variable
   'pyim-dhashcache-icode2word nil
   ;; 添加 dregcache 後端之後，原來的 dcache 更名為 dhashcache，
   ;; 升級遷移
   (pyim-dcache-get-variable 'pyim-dcache-icode2word))
  (pyim-dcache-set-variable
   'pyim-dhashcache-ishortcode2word nil
   ;; 添加 dregcache 後端之後，原來的 dcache 更名為 dhashcache，
   ;; 升級遷移
   (pyim-dcache-get-variable 'pyim-dcache-ishortcode2word)))

(defun pyim-dhashcache-save-personal-dcache-to-file ()
  ;; 用戶選擇過的詞
  (pyim-dcache-save-variable 'pyim-dhashcache-icode2word)
  ;; 詞頻
  (pyim-dcache-save-variable 'pyim-dhashcache-iword2count))

(defun pyim-dhashcache-insert-export-content ()
  (maphash
   #'(lambda (key value)
       (insert (format "%s %s\n" key value)))
   pyim-dhashcache-iword2count)
  ;; 在預設情況下，用戶選擇過的詞生成的緩存中存在的詞條，
  ;; `pyim-dhashcache-iword2count' 中也一定存在，但如果用戶
  ;; 使用了特殊的方式給用戶選擇過的詞生成的緩存中添加了
  ;; 詞條，那麼就需要將這些詞條也導出，且設置詞頻為 0
  (maphash
   #'(lambda (_ words)
       (dolist (word words)
         (unless (gethash word pyim-dhashcache-iword2count)
           (insert (format "%s %s\n" word 0)))))
   pyim-dhashcache-icode2word))

(defmacro pyim-dhashcache-put (cache code &rest body)
  "這個用於保存詞條，刪除詞條以及調整詞條位置。"
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

(defun pyim-dhashcache-update-iword2count (word &optional prepend wordcount-handler)
  "保存詞頻到緩存。"
  (pyim-dhashcache-put
    pyim-dhashcache-iword2count word
    (cond
     ((functionp wordcount-handler)
      (funcall wordcount-handler orig-value))
     ((numberp wordcount-handler)
      wordcount-handler)
     (t (+ (or orig-value 0) 1)))))

(defun pyim-dhashcache-delete-word (word)
  "將中文詞條 WORD 從個人詞庫中刪除"
  (let* ((pinyins (pyim-hanzi2pinyin word nil "-" t))
         (pinyins-szm (mapcar
                       #'(lambda (pinyin)
                           (mapconcat #'(lambda (x)
                                          (substring x 0 1))
                                      (split-string pinyin "-") "-"))
                       pinyins)))
    (dolist (pinyin pinyins)
      (unless (pyim-string-match-p "[^ a-z-]" pinyin)
        (pyim-dhashcache-put
          pyim-dhashcache-icode2word pinyin
          (remove word orig-value))))
    (dolist (pinyin pinyins-szm)
      (unless (pyim-string-match-p "[^ a-z-]" pinyin)
        (pyim-dhashcache-put
          pyim-dhashcache-icode2word pinyin
          (remove word orig-value))))
    (remhash word pyim-dhashcache-iword2count)))

(defun pyim-dhashcache-insert-word-into-icode2word (word pinyin prepend)
  "保存個人詞到緩存。"
  (pyim-dhashcache-put pyim-dhashcache-icode2word
                       pinyin
                       (if prepend (pyim-list-merge word orig-value)
                         (pyim-list-merge orig-value word))))

(defun pyim-dhashcache-search-word-code (string)
  (gethash string pyim-dhashcache-word2code))

(defun pyim-dhashcache-export-personal-words (file &optional confirm)
  "導出個人詞庫到 FILE。"
  (pyim-dhashcache-export pyim-dhashcache-icode2word file confirm))

;; * Footer

(provide 'pyim-dhashcache)
;;; pyim-dhashcache.el ends here
