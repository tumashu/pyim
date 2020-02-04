;;; pyim-dregcache --- map dictionary to plain cache and use regular expression to search

;; * Header
;; Copyright 2019 Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>

;; This file maps dictionary into plain memory. Regular expression is used to find words.

;;; Commentary:

;; * 說明文檔                                                              :doc:
;; 這個文件將詞典直接映射到記憶體，使用正則表達式 (Regular Expression) 搜索詞庫。
;; 搜索速度較快，消耗記憶體極少。
;;
;; 可以 (setq pyim-dcache-backend 'pyim-dregcache) 然後重啟輸入法啟用此引擎

;;; Code:
;; * 代碼                                                                 :code:
(require 'pyim-common)
(defvar pyim-dregcache-cache nil)
(defvar pyim-dregcache-icode2word nil)
(defvar pyim-dregcache-iword2count nil)
(defvar pyim-dregcache-dicts-md5 nil)

(defun pyim-dregcache-variable-file (variable)
  "Get VARIABLE dcache file path."
  (concat (file-name-as-directory pyim-dcache-directory)
          (symbol-name variable)))

(defun pyim-dregcache-save-variable (variable value)
  "Save VARIABLE with its VALUE."
  (let* ((file (pyim-dregcache-variable-file variable))
         (save-silently t))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (insert value)
      (pyim-dcache-write-file file))))

(defun pyim-dregcache-load-variable (variable)
  "載入 VARIABLE 對應的文件內容。"
  (let* ((file (pyim-dregcache-variable-file variable)))
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun pyim-dregcache-sort-words (words-list)
  "對 WORDS-LIST 排序，詞頻大的排在前面。

排序使用 `pyim-dregcache-iword2count' 中記錄的詞頻信息"
  (sort words-list
        #'(lambda (a b)
            (let ((a (car (split-string a ":")))
                  (b (car (split-string b ":"))))
              (> (or (gethash a pyim-dregcache-iword2count) 0)
                 (or (gethash b pyim-dregcache-iword2count) 0))))))

(defun pyim-dregcache-sort-icode2word ()
  "對個人詞庫排序。"
  ;; https://github.com/redguardtoo/zhfreq
  (with-temp-buffer
    (dolist (l (split-string pyim-dregcache-icode2word "\n"))
      (cond
        ((string-match "^\\([a-z-]+ \\)\\(.*\\)" l)
         ;; 3 字以上詞很少，如果只處理單字，2 字詞，3 字詞
         ;; ((string-match "^\\([a-z]+ \\|[a-z]+-[a-z]+ \\|[a-z]+-[a-z]+-[a-z]+ \\)\\(.*\\)" l)
         (let* ((pinyin (match-string 1 l))
                (words (pyim-dregcache-sort-words (split-string (match-string 2 l) " "))))
           (insert (format "%s\n" (concat pinyin (mapconcat 'identity words " "))))))
        ;; 其他詞
        ((string= l "")
         ;; skip empty line
         )
        (t
         (insert (format "%s\n" l)))))
    (setq pyim-dregcache-icode2word (buffer-string))))

(defun pyim-dregcache-create-cache-content (raw-content)
  "將 RAW-CONTENT 劃分成可以更高效搜索的緩沖區。"
  (let* (rlt)
    (cond
     ;; 小於 1M 的詞庫不用劃分 "子搜索區域"
     ((< (length raw-content) (* 1 1024 1024))
      (setq rlt (list :content raw-content)))
     (t
      (let* ((chars "bcdefghjklmnopqrstwxyz")
             pattern
             (i 0)
             dict-splited
             (content-segments '())
             (start 0)
             end)
        ;; 將字典緩存劃分成多個 "子搜索區域"
        (while (< i (length chars))
          (setq pattern (concat "^" (string (elt chars i))))
          (setq end (string-match pattern raw-content start))
          (when end
            (setq content-segments
                  (add-to-list 'content-segments
                               (substring-no-properties raw-content start end)
                               t))
            (setq dict-splited t)
            ;; 將搜索起始點前移
            (setq start end))
          (setq i (1+ i)))

        (cond
         ;; attach segments
         (dict-splited
          ;; 將剩餘的附後
          (setq content-segments
                (add-to-list 'content-segments
                             (substring-no-properties raw-content start)
                             t))
          (setq rlt (list :content content-segments)))
         (t
          (setq rlt (list :content raw-content)))))))
    rlt))

(defun pyim-dregcache-load-dictionary-file (dict-file)
  "READ from DICT-FILE."
  (let* ((raw-content (with-temp-buffer
                        (insert-file-contents dict-file)
                        (buffer-string))))
    (setq pyim-dregcache-cache
          ;; use string type as key, so have to use `lax-plist-put'
          ;; @see https://www.gnu.org/software/emacs/manual/html_node/elisp/Plist-Access.html#Plist-Access
          (lax-plist-put pyim-dregcache-cache
                         (file-truename dict-file)
                         (pyim-dregcache-create-cache-content raw-content)))))

(defun pyim-dregcache-update-code2word (dict-files dicts-md5 &optional force)
  "讀取並加載詞庫。

讀取 `pyim-dicts' 和 `pyim-extra-dicts' 裡面的詞庫文件，生成對應的
詞庫緩沖文件，然後加載詞庫緩存。

DICT-FILES 是詞庫文件列表，DICTS-MD5 是詞庫的MD5校驗碼。

如果 FORCE 為真，強制加載。"
  (interactive)
  (when (or force (not (equal dicts-md5 pyim-dregcache-dicts-md5)))
    ;; no hashtable i file mapping algorithm
    (dolist (file dict-files)
      (pyim-dregcache-load-dictionary-file file))
    (setq pyim-dregcache-dicts-md5 dicts-md5)))

(defmacro pyim-dregcache-shenmu2regexp (char)
  "將聲母 CHAR 轉換為通用正則表達式匹配所有以該聲母開頭的漢字。"
  `(concat ,char "[a-z]*"))

(defmacro pyim-dregcache-is-shenmu (code)
  "判斷CODE 是否是一個聲母。"
  `(and (eq (length ,code) 1)
        (not (string-match ,code "aeo"))))

(defun pyim-dregcache-code2regexp (code)
  "將 CODE 轉換成正則表達式用來搜索辭典緩存中的匹配項目。
單個聲母會匹配所有以此聲母開頭的單個漢字。"
  (let* (arr)
    (cond
     ((pyim-dregcache-is-shenmu code)
      ;; 用戶輸入單個聲母如 "w", "y", 將該聲母轉換為
      ;; 通用正則表達式匹配所有以該聲母開頭的漢字
      (pyim-dregcache-shenmu2regexp code))

     ((eq (length (setq arr (split-string code "-"))) 1)
      ;; 用戶輸入單個漢字的聲母和韻母如 "wan"、"dan"，不做任何處理
      code)

     (t
      ;; 用戶輸入多字詞的 code，如果某個字只提供了首個聲母，將該聲母轉換為
      ;; 通用正則表達式匹配所有以該聲母開頭的漢字
      (let* ((s (mapconcat (lambda (e)
                             (if (pyim-dregcache-is-shenmu e)
                                 (pyim-dregcache-shenmu2regexp e)
                               e))
                           arr
                           "-")))
        ;; 當輸入 3 字拼音時，也搜索前 3 字包含此拼音的更長的詞。
        ;; 三字詞很少 (可 grep -rEsoh "^[a-z]+(-[a-z]+){2}( [^ ]+){2,6}" 驗證)
        ;; 用戶通常輸入 4 字或更多字的詞。
        ;; 例如符合 bei-jin-tian 的詞很少，"北京天安門" 是更可能匹配的詞
        ;; `pyim-dregcache' 搜索演算法可以保證更長的詞在靠後的位置
        (cond
         ((< (length arr) 3)
          s)
         ((eq ?* (elt s (1- (length s))))
          ;; 簡化正則表達式，tian-an-m[a-z]* => tian-an-m[a-z]?[a-z-]*
          (concat (substring s 0 (1- (length s))) "?[a-z-]*"))
         (t
          ;; tian-an-men => tian-an-men[a-z-]*
          (concat s "[a-z-]*"))))))))

(defmacro pyim-dregcache-match-line (code)
  `(concat "^" (pyim-dregcache-code2regexp ,code) " \\(.+\\)"))

(defun pyim-dregcache-all-dict-files ()
  "所有詞典文件。"
  (let* (rlt)
    (dolist (item pyim-dregcache-cache)
      (when (stringp item)
        (push item rlt)))
    ;; restore user's dictionary order
    (nreverse rlt)))

(defun pyim-dregcache-get-content (code file-info)
  "找到 CODE 對應的字典子緩沖區。FILE-INFO 是字典文件的所有信息。"
  (let* ((rlt (plist-get file-info :content))
         idx
         (ch (elt code 0)))
    (when (listp rlt)
      (cond
       ((<= ch ?h)
        (setq idx (- ch ?a)))
       ((<= ch ?t)
        ;; 'i' could not be first character of pinyin code
        (setq idx (- ch ?a 1)))
       (t
        ;; 'i', 'u', 'v' could not be first character of pinyin code
        (setq idx (- ch ?a 3))))
      ;; fetch segment using the first character of pinyin code
      (setq rlt (nth idx rlt)))
    rlt))

(defun pyim-dregcache-get-1 (content code)
  (let ((case-fold-search t)
        (start 0)
        (pattern (pyim-dregcache-match-line code))
        (content-length (length content))
        word
        output)
    (while (and (< start content-length)
                (setq start (string-match pattern content start)))
      ;; 提取詞
      (setq word (match-string-no-properties 1 content))
      (when word
        (cond
          ((string-match "^[^ ]+$" word)
           ;; 單個詞
           (push word output))
          (t
           ;; 多個字
           (setq output (append (nreverse (split-string word " +")) output)))))
      ;; 繼續搜索
      (setq start (+ start 2 (length code) (length word))))
    output))

(defun pyim-dregcache-get (code &optional from)
  "從 `pyim-dregcache-cache' 搜索 CODE，得到對應的詞條。"
  (if (or (memq 'icode2word from)
          (memq 'ishortcode2word from))
      (pyim-dregcache-get-icode2word-ishortcode2word code)
    (let ((dict-files (pyim-dregcache-all-dict-files))
          result)
      (when pyim-debug (message "pyim-dregcache-get is called. code=%s pattern=%s" code pattern))
      (when dict-files
        (dolist (file dict-files)
                 (let* ((file-info (lax-plist-get pyim-dregcache-cache file))
                        (content (pyim-dregcache-get-content code file-info)))
                   (setq result (append (pyim-dregcache-get-1 content code) result)))))
      ;; `push' plus `nreverse' is more efficient than `add-to-list'
      ;; Many examples exist in Emacs' own code
      (setq result (nreverse result))
      `(,@result ,@(pyim-pinyin2cchar-get code t t)))))

(defun pyim-dregcache-get-icode2word-ishortcode2word (code)
  "以 CODE 搜索個人詞和個人聯想詞。正則表達式搜索詞庫，不需要為聯想詞開單獨緩存。"
  (when pyim-debug (message "pyim-dregcache-get-icode2word-ishortcode2word called => %s" code))
  (when pyim-dregcache-icode2word
    (nreverse (pyim-dregcache-get-1 pyim-dregcache-icode2word code))))

(defun pyim-dregcache-update-personal-words (&optional force)
  "合併 `pyim-dregcache-icode2word' 磁盤文件。加載排序後的結果。

在這個過程中使用了 `pyim-dregcache-iword2count' 中記錄的詞頻信息。
如果 FORCE 為真，強制排序。"
  (let* ((content (pyim-dregcache-load-variable 'pyim-dregcache-icode2word)))
    (when content
      (with-temp-buffer
        (let* (prev-record prev-code record code)
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
            ;; 詞庫在創建時已經保證 1 個 code 只有 1 行，因此合併詞庫文件一般
            ;; 只有 2 行需要合併，所以這裡沒有對 2 行以上的合併進行優化
            (when (= (forward-line 1) 0)
              (setq record (pyim-dline-parse))
              (setq code (car record))
              (when (string-equal prev-code code)
                ;; line-end-position donesn't contain "\n"
                (progn (delete-region prev-point (line-end-position))
                       (insert (string-join (delete-dups `(,@prev-record ,@record)) " ")))))))
        (setq pyim-dregcache-icode2word (buffer-string)))))
  (when (and force pyim-dregcache-icode2word)
    (pyim-dregcache-sort-icode2word)))

(defun pyim-dregcache-init-variables ()
  "初始化 cache 緩存相關變數。"
  (pyim-dcache-set-variable
   'pyim-dregcache-iword2count
   nil
   ;; dregcache 引擎也需要詞頻信息，第一次使用 dregcache 引擎的時候，
   ;; 自動導入 dhashcache 引擎的詞頻信息，以後兩個引擎的詞頻信息就
   ;; 完全分開了。
   (pyim-dcache-get-variable 'pyim-dhashcache-iword2count))
  (unless pyim-dregcache-icode2word
    (pyim-dregcache-update-personal-words t)))

(defun pyim-dregcache-save-personal-dcache-to-file ()
  "保存緩存內容到預設目錄。"
  (when pyim-debug (message "pyim-dregcache-save-personal-dcache-to-file called"))
  ;; 用戶選擇過的詞存為標準辭典格式保存
  (when pyim-dregcache-icode2word
    (pyim-dregcache-save-variable 'pyim-dregcache-icode2word
                                  pyim-dregcache-icode2word))
  ;; 詞頻
  (pyim-dcache-save-variable 'pyim-dregcache-iword2count))

(defun pyim-dregcache-insert-export-content ()
  "TODO"
  )

(defun pyim-dregcache-update-iword2count (word &optional prepend wordcount-handler)
  "保存詞頻到緩存。"
  (when pyim-debug (message "pyim-dregcache-update-iword2count. word=%s" word))
  (let* ((orig-value (gethash word pyim-dregcache-iword2count))
         (new-value (cond
                     ((functionp wordcount-handler)
                      (funcall wordcount-handler orig-value))
                     ((numberp wordcount-handler)
                      wordcount-handler)
                     (t (+ (or orig-value 0) 1)))))
    (unless (equal orig-value new-value)
      (puthash word new-value pyim-dregcache-iword2count))))

(defun pyim-dregcache-delete-word (word)
  "將中文詞條 WORD 從個人詞庫中刪除。"
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
  ;; 刪除對應詞條的詞頻
  (remhash word pyim-dregcache-iword2count))

(defun pyim-dregcache-insert-word-into-icode2word (word code prepend)
  "保存個人詞到緩存，和其他詞庫格式一樣以共用正則搜索演算法。"
  (when pyim-debug
    (message "pyim-dregcache-insert-word-into-icode2word called => %s %s %s"
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
            ;; 這裡不進行排序，在 pyim-dregcache-update-personal-words 排序
            (setq old-word-list (pyim-dregcache-sort-words (split-string substring " ")))
            (setq replace-string (concat code " " (string-join (delete-dups `(,@old-word-list ,word)) " "))))
        (setq replace-string (concat code " " (or replace-string word) "\n")))
      (goto-char (or beg (point-max)))
      (insert replace-string))
    (setq pyim-dregcache-icode2word
            (buffer-string))))

(defun pyim-dregcache-search-word-code-1 (word content)
    (let* ((case-fold-search t)
           (regexp (concat "^\\([a-z-]+\\)\\(.*\\) " "\\(" word " \\|" word "$\\)")))
      (when (string-match regexp content)
        (match-string-no-properties 1 content))))

(defun pyim-dregcache-search-word-code (word)
  "從 `pyim-dregcache-cache' 和 `pyim-dregcache-icode2word' 搜索 word，得到對應的 code。"
  (when pyim-debug (message "pyim-dregcache-search-word-code word=%s" word))
  (when pyim-dregcache-cache
    (catch 'result
      (let ((dict-files (pyim-dregcache-all-dict-files))
            code)
        (when pyim-dregcache-icode2word
          (setq code (pyim-dregcache-search-word-code-1 word pyim-dregcache-icode2word))
          (when code (throw 'result (list code))))
        (dolist (file dict-files)
          (let* ((file-info (lax-plist-get pyim-dregcache-cache file))
                 (contents (lax-plist-get file-info :content)))
            (dolist (content (if (listp contents) contents (list contents)))
              (setq code (pyim-dregcache-search-word-code-1 word content))
              (when code (throw 'result (list code))))))))))

(defun pyim-dregcache-export-personal-words (file &optional confirm)
  "將個人詞庫存入 FILE。"
  (when pyim-dregcache-icode2word
    ;; 按詞頻排序，把詞頻信息保存到用戶詞典
    (pyim-dregcache-sort-icode2word)
    (with-temp-buffer
      (insert pyim-dregcache-icode2word)
      ;; 按拼音排序
      (sort-lines nil (point-min) (point-max))
      (pyim-dcache-write-file file confirm))))

;; * Footer

(provide 'pyim-dregcache)
;;; pyim-dregcache.el ends here
