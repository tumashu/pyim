;;; pyim-sqlite --- uses sqlite to cache and search dictionaries -*- lexical-binding: t; -*-

;; * Header
;; This file uses sqlite3 database to cache and search dictionaries.

;;; Commentary:

;; * 说明文档                                                              :doc:
;; 这个文件把系统和用户词典保存在sqlite database中，提供基于sql的辞典搜索算法.
;; select 查询sqlite database 的速度比从 hash-table get 差一个数量级。0.00005s 和 0.000003s 的区别。
;;
;; 可以 (setq pyim-dcache-backend 'pyim-sqlite) 然后重启输入法启用此引擎

;;; Code:
;; * 代码                                                                 :code:
(require 'pyim-common)

;; (push "e:/workspace/emacs-sqlite3" load-path)
(require 'sqlite3)

(defvar pyim-sqlite-systemdb nil
  "系统词库路径.")

(defvar pyim-sqlite-userdb nil
  "用户输入生成的词库路径")

;; sqlite database的默认目录为 ~/.emacs.d/pyim
(setq pyim-sqlite-systemdb (sqlite3-new (expand-file-name "pyim/pyim_system.db" user-emacs-directory)))
(setq pyim-sqlite-userdb (sqlite3-new (expand-file-name "pyim/pyim_user.db" user-emacs-directory)))

(setq get-iword2count-stmt (sqlite3-prepare pyim-sqlite-userdb
                                            "SELECT count FROM pyim_iword2count where word=?"))

(setq get-icode2word2-stmt (sqlite3-prepare pyim-sqlite-userdb
                                            "SELECT cchars FROM pyim_icode2word WHERE py=?"))

(defmacro with-timer (title &rest forms)
  "Run the given FORMS, counting the elapsed time.
A message including the given TITLE and the corresponding elapsed
time is displayed."
  (declare (indent 1))
  (let ((nowvar (make-symbol "now"))
        (body   `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (prog1 ,body
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s... done (%.7fs)" ,title elapsed))))))

(defun pyim-sqlite--get-iword2count(db word)
  "从数据库获取word的词频."

  ;; (let ((resultset (with-timer (concat "get-iword2count:" word) (sqlite3-execute db
  ;;                                    "SELECT count FROM pyim_iword2count where word=?" (vector word)))))
  ;; (let ((resultset (sqlite3-execute db
  ;;                                   "SELECT count FROM pyim_iword2count where word=?" (vector word))))
  (let ((resultset (sqlite3-execute-with-stmt db
                                              get-iword2count-stmt (vector word) nil)))
    (car (sqlite3-resultset-next resultset))
    ))

(defun pyim-sqlite-sort-words (words-list)
  " 需要修改，从user db里取。
对 WORDS-LIST 排序，词频大的排在前面.

排序使用 `pyim-dhashcache-iword2count' 中记录的词频信息"
  ;; (message "sort-words: %s" words-list)
  (sort words-list
        #'(lambda (a b)
            (let ((a (car (split-string a ":")))
                  (b (car (split-string b ":"))))
              (> (or (pyim-sqlite--get-iword2count pyim-sqlite-userdb a) 0)
                 (or (pyim-sqlite--get-iword2count pyim-sqlite-userdb b) 0))))))

(defun pyim-sqlite-get-shortcode (code)
  "用了dhashcode的代码
获取一个 CODE 的所有简写.

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

(defun pyim-sqlite--get-icode2word(db code)
  "按拼音从user数据库查询用户保存的词.
多个词，按空格分割. 为什么查询不到数据返回nil时，sqlite3-resultset-next 会导致程序segfault."
  ;; (let ((resultset (sqlite3-execute db
  ;;                                   "SELECT cchars FROM pyim_icode2word WHERE py=?" (vector code))))
  (let ((resultset (sqlite3-execute-with-stmt db
                                              get-icode2word2-stmt (vector code) nil)))
    (let ((result (sqlite3-resultset-next resultset)))
      (when result
        (split-string (car result) " ")))))

(defun pyim-sqlite--get-code2word(mydb code)
  "从数据库查询系统词库word. 写错字段，调用sqlite3-resultset-next，emacs居然segfault了.
pyim_code2word按拼音查询，cchars字段中的词用空格分割."
  ;; (let ((resultset (with-timer (concat "pyim-sqlite--get-code2word: " code) (sqlite3-execute mydb
  ;;                                    "SELECT cchars FROM PYIM_CODE2WORD where py=?" (vector code)))))
  (let ((resultset (sqlite3-execute mydb
                                     "SELECT cchars FROM PYIM_CODE2WORD where py=?" (vector code))))
    (let ((result (sqlite3-resultset-next resultset)))
      (when result
        (split-string (car result) " ")))))

(defun pyim-sqlite-get (code &optional from)
  "从数据库查询对应的词条."
  ;; (message "sqlite-get: %s" code)
  (let (result)
    ;; 查询系统词库
    (let ((value (pyim-sqlite--get-code2word pyim-sqlite-systemdb code)))
      (when value
        (setq result (append result value)))
      )
    ;; 查询用户词库
    (let ((value (pyim-sqlite--get-icode2word pyim-sqlite-userdb code)))
      (when value
        (setq result (append result value)))
      )
    `(,@result ,@(pyim-pinyin2cchar-get code t t))
    ))

(defun pyim-sqlite-update-icode2word (&optional force)
  "对 personal 缓存中的词条进行排序，加载排序后的结果.

在这个过程中使用了 `pyim-dhashcache-iword2count' 中记录的词频信息。
如果 FORCE 为真，强制排序。"
  (interactive)
  (message "pyim-sqlite-update-icode2word: %s" force)
  )

(defun pyim-sqlite-init-variables ()
  "初始化 dcache 缓存相关变量. 会调用多次，如果在这里初始化数据库连接要小心些，被覆盖的话，insert or update 时，会lock database."
  ;; (message "pyim-sqlite-init-variables: %s" (current-time-string))
  ;; (setq pyim-sqlite-systemdb (sqlite3-new "e:\\tmp\\pyim_sogou.db"))
  ;; (setq pyim-sqlite-userdb (sqlite3-new "e:\\tmp\\test.db"))
  )

(defun pyim-sqlite-save-personal-dcache-to-file ()
  ;; 用户选择过的词
  ;; (pyim-dcache-save-variable 'pyim-dhashcache-icode2word)
  ;; 词频
  ;; (pyim-dcache-save-variable 'pyim-dhashcache-iword2count)
  )

(defun pyim-sqlite-insert-export-content ()
  "暂时没实现."
  )

(defmacro pyim-sqlite-put (cache code &rest body)
  "这个用于保存词条，删除词条以及调整词条位置."
  )

(defun pyim-sqlite-update-iword2count (word &optional prepend wordcount-handler)
  "保存词频到缓存. 需要判断是否有记录，存在的话update，没有就insert.
测试了一下，不需要手动commit，lock database 是因为多次初始化数据库连接导致。
update pyim_iword2count set count=count+1 where word='字库';"
  ;; (message "pyim-sqlite-update-iword2count: %s" word)
  (let ((count (pyim-sqlite--get-iword2count pyim-sqlite-userdb word))
        (db pyim-sqlite-userdb))
    (if (not (null count))
        (progn ;; (sqlite3-transaction pyim-sqlite-userdb)
          ;; (message "%s: %i" word count)
          (sqlite3-execute-batch db
                                 "update pyim_iword2count set count=count+1 where word=?"
                                 (vector word))
          ;; (sqlite3-commit db)
          )
      (sqlite3-execute-batch db
                             "insert into pyim_iword2count(word, count) values(?,?)"
                             (vector word 1)))))

(defun pyim-sqlite-delete-word (word)
  "将中文词条 WORD 从个人词库中删除"
  )

(defun pyim-sqlite-insert-word-into-icode2word (word pinyin prepend)
  "保存个人词到用户数据库，实现把添加的词放到最前面有点麻烦，暂时不实现."
  ;; (message "pyim-sqlite-insert-word-into-icode2word: %s %s %s" word pinyin prepend)
  (let ((chinese (car (pyim-sqlite--get-icode2word pyim-sqlite-userdb pinyin)))
        (db pyim-sqlite-userdb))
    (if (not (null chinese))
        (progn
          ;; (message "%s: %s" pinyin chinese)
          ;; word不存在，update
          (unless (string-match-p word chinese)
            (sqlite3-execute-batch db
                                   "update pyim_icode2word set cchars=? where py=?"
                                   (vector (concat word " " chinese) pinyin))))
      (sqlite3-execute-batch db
                             "insert into pyim_icode2word(py, cchars) values(?,?)"
                             (vector pinyin word))
      )))

(defun pyim-sqlite-search-word-code (string)
  ;; (gethash string pyim-dhashcache-word2code)
  )

(defun pyim-sqlite-export-personal-words (file &optional confirm)
  "导出个人词库到 FILE."
  )

;; * Footer

(provide 'pyim-sqlite)
;;; pyim-sqlite.el ends here
