;;; pyim-tests.el ---  unit tests for pyim -*- lexical-binding: t; -*-

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
;;  pyim test case.

;;; Code:
;; * 代码                                                                 :code:
(require 'ert)
(require 'pyim)
(require 'pyim-basedict)
(require 'pyim-dregcache)
(require 'pyim-dhashcache)

(defun pyim-tests-make-temp-file (&optional dir-flag)
  (make-temp-file "pyim-tests-temp-" dir-flag))

(defun pyim-tests-noninteractive-init ()
  (setq default-input-method "pyim")
  (setq pyim-dicts nil)
  (setq pyim-extra-dicts nil)
  ;; 设置 pyim-dcache-directory, 防止用户个人词库不小心被覆盖掉。
  (setq pyim-dcache-directory (pyim-tests-make-temp-file t))
  ;; 做测试的时候不保存词库，防止因为误操作导致个人词库损坏。
  (defalias 'pyim-kill-emacs-hook-function #'ignore)

  (pyim-basedict-enable)
  (pyim-dcache-init-variables))

(when noninteractive
  (pyim-tests-noninteractive-init))

;; ** pyim-schemes 相关单元测试
(ert-deftest pyim-tests-pyim-schemes ()
  (let ((pyim-default-scheme 'wubi))
    (should (equal (pyim-scheme-name) 'wubi)))

  (let ((pyim-default-scheme 'wuci))
    (should (equal (pyim-scheme-name) 'quanpin)))

  (let ((pyim-default-scheme 'wubi)
        (pyim-assistant-scheme 'cangjie)
        (pyim-assistant-scheme-enable t))
    (should (equal (pyim-scheme-name) 'cangjie)))

  (let ((pyim-default-scheme 'wubi)
        (pyim-assistant-scheme 'cangjie)
        (pyim-assistant-scheme-enable nil))
    (should (equal (pyim-scheme-name) 'wubi)))

  (should (equal (pyim-scheme-get-option 'quanpin :class) 'quanpin))
  (should (equal (pyim-scheme-get-option 'wubi :class) 'xingma)))

;; ** pyim-common 相关单元测试
(ert-deftest pyim-tests-pyim-char-before/after-to-string ()
  (with-temp-buffer
    (insert "你好世界abc")
    (print (pyim-char-before-to-string 0))
    (should (equal (pyim-char-before-to-string 0) "c"))
    (should (equal (pyim-char-before-to-string 1) "b"))
    (backward-char 5)
    (should (equal (pyim-char-before-to-string 0) "好"))
    (should (equal (pyim-char-before-to-string 1) "你"))
    (should (equal (pyim-char-after-to-string 0) "世"))
    (should (equal (pyim-char-after-to-string 1) "界"))))

(ert-deftest pyim-tests-pyim-dline-parse ()
  (with-temp-buffer
    (insert "ni-hao 你好")
    (should (equal (pyim-dline-parse) '("ni-hao" "你好"))))
  (with-temp-buffer
    (insert "a-b-c-d")
    (should (equal (pyim-dline-parse "-") '("a" "b" "c" "d"))))
  (with-temp-buffer
    (insert "你好 2")
    (should (equal (pyim-dline-parse) '("你好" "2")))))

(ert-deftest pyim-tests-pyim-permutate-list ()
  (should (equal (pyim-permutate-list '((a b) (c d e) (f)))
                 '((a c f)
                   (a d f)
                   (a e f)
                   (b c f)
                   (b d f)
                   (b e f))))
  (should (equal (pyim-permutate-list nil) nil))
  (should (equal (pyim-permutate-list '((a))) '((a)))))

(ert-deftest pyim-tests-pyim-zip ()
  (should (equal (pyim-zip '((a b "d") nil (1 d) (f) nil))
                 '(a 1 f b d "d")))
  (should (equal (pyim-zip nil) nil)))

(ert-deftest pyim-tests-pyim-subconcat ()
  (should (equal (pyim-subconcat '("a" "b" "c" "d"))
                 '("abcd" "abc" "ab")))
  (should (equal (pyim-subconcat '("a" "b" "c" "d") "-")
                 '("a-b-c-d" "a-b-c" "a-b")))
  (should (equal (pyim-subconcat nil) nil)))

(ert-deftest pyim-tests-pyim-string-distance ()
  (should (equal (pyim-string-distance "nihaoma" "nihaoma") 0))
  (should (equal (pyim-string-distance "nihaoma" "nhm") 4))
  (should (equal (pyim-string-distance "nihaoma" "niham") 2))
  (should (equal (pyim-string-distance "nihaoma" "nbm") 5))
  (should (equal (pyim-string-distance "nihaoma" "wbc") 7))
  (should (equal (pyim-string-distance "nihaoma" "ni") 5))
  (should (equal (pyim-string-distance "nihaoma" "ci") 6)))

(ert-deftest pyim-tests-pyim-add-unread-command-events ()
  (let ((unread-command-events nil))
    (pyim-add-unread-command-events ?a)
    (should (equal unread-command-events
                   '((no-record . 97))))
    (pyim-add-unread-command-events "b")
    (should (equal unread-command-events
                   '((no-record . 98)
                     (no-record . 97))))
    (pyim-add-unread-command-events "cd")
    (should (equal unread-command-events
                   '((no-record . 99)
                     (no-record . 100)
                     (no-record . 98)
                     (no-record . 97))))
    (pyim-add-unread-command-events "e" t)
    (should (equal unread-command-events
                   '((no-record . 101))))
    (pyim-add-unread-command-events nil t)
    (should (equal unread-command-events nil))))

(ert-deftest pyim-tests-pyim-time-limit-while ()
  (let ((time (current-time))
        (limit 0.1))
    (pyim-time-limit-while t limit
      t)
    (should (< (float-time (time-since time)) (* limit 2)))))

;; ** pyim-pymap 相关单元测试
(ert-deftest pyim-tests-pyim-pymap ()
  (should-not (cl-find-if-not
               (lambda (x)
                 (= (length (split-string (cadr x) "|")) 4))
               pyim-pymap))
  (should (equal (pyim-pymap-cchar< "的" "成") t))
  (should (equal (pyim-pymap-cchar2py-get "阿")
                 '("e" "a")))
  (should (equal (pyim-pymap-cchar2py-get ?阿)
                 '("e" "a")))
  (should (equal (pyim-pymap-cchar2py-get "你")
                 '("ni")))
  (should (equal (pyim-pymap-cchar2py-get "作")
                 '("zuo")))
  (should (equal (pyim-pymap-py2cchar-get "a" t)
                 '("阿啊呵腌|嗄吖锕||錒")))
  (should (equal (pyim-pymap-py2cchar-get "a" t t)
                 '("阿" "啊" "呵" "腌" "嗄" "吖" "锕" "錒")))
  (should (equal (pyim-pymap-py2cchar-get "a" t t t)
                 '("阿" "啊" "呵" "腌" "|" "嗄" "吖" "锕" "|" "|" "錒")))
  (should (equal (pyim-pymap-py2cchar-get "zhua" t)
                 '("抓挝爪||髽|膼撾檛簻")))
  (should (equal (mapcar (lambda (x)
                           (concat (substring x 0 1)
                                   (substring x -1)))
                         (pyim-pymap-py2cchar-get "a"))
                 '("阿錒" "爱溾" "厂馣" "昂䇦" "奥泑")))
  (should (equal (length (pyim-pymap-py2cchar-get "a")) 5))
  (should (equal (length (pyim-pymap-py2cchar-get "z")) 36)))

;; ** pyim-pinyin 相关单元测试
(ert-deftest pyim-tests-pyim-pinyin ()
  ;; pyim-pinyin-get-shenmu
  (should (equal (pyim-pinyin-get-shenmu "nihao")
                 '("n" . "ihao")))
  (should (equal (pyim-pinyin-get-shenmu "ao")
                 '("" . "ao")))
  (should (equal (pyim-pinyin-get-shenmu "")
                 '(nil . "")))

  ;; pyim-pinyin-valid-charpy-p
  (should (pyim-pinyin-valid-charpy-p "n" "i"))
  (should (pyim-pinyin-valid-charpy-p "" "a"))
  (should (pyim-pinyin-valid-charpy-p "" "ao"))
  (should-not (pyim-pinyin-valid-charpy-p "n" "k"))
  (should-not (pyim-pinyin-valid-charpy-p "a" "k"))

  ;; pyim-pinyin-get-charpy
  (should (equal (pyim-pinyin-get-charpy "nihao")
                 '(("n" "i" "n" "i") . "hao")))
  (should (equal (pyim-pinyin-get-charpy "ao")
                 '(("" "ao" "" "ao") . "")))
  (should (equal (pyim-pinyin-get-charpy "nh")
                 '(("n" "" "n" "") . "h")))

  ;; pyim-pinyin-split
  (should (equal (pyim-pinyin-split "n")
                 '(("n" nil "n" nil))))
  (should (equal (pyim-pinyin-split "ni")
                 '(("n" "i" "n" "i"))))
  (should (equal (pyim-pinyin-split "nih")
                 '(("n" "i" "n" "i")
                   ("h" nil "h" nil))))
  (should (equal (pyim-pinyin-split "nihao")
                 '(("n" "i" "n" "i")
                   ("h" "ao" "h" "ao"))))
  (should (equal (pyim-pinyin-split "a")
                 '(("" "a" "" "a"))))
  (should (equal (pyim-pinyin-split "a")
                 '(("" "a" "" "a"))))
  (should (equal (pyim-pinyin-split "xian")
                 '(("x" "ian" "x" "ian"))))
  (should (equal (pyim-pinyin-split "xi'an")
                 '(("" "xi'an" "" "xi'an"))))
  (should (equal (pyim-pinyin-split "ide")
                 '(("" "ide" "" "ide"))))
  (should (equal (pyim-pinyin-split "ude")
                 '(("" "ude" "" "ude"))))

  ;; pyim-pinyin-find-fuzzy
  (let ((pyim-pinyin-fuzzy-alist
         '(("en" "eng")
           ("f" "h"))))
    (should (equal (pyim-pinyin-find-fuzzy '("f" "en" "f" "en"))
                   '(("f" "en" "f" "en")
                     ("f" "eng" "f" "en")
                     ("h" "en" "f" "en")
                     ("h" "eng" "f" "en"))))
    (should (equal (pyim-pinyin-find-fuzzy '("h" "en" "h" "en"))
                   '(("h" "en" "h" "en")
                     ("h" "eng" "h" "en")
                     ("f" "en" "h" "en")
                     ("f" "eng" "h" "en"))))
    (should (equal (pyim-pinyin-find-fuzzy '("f" "eng" "f" "eng"))
                   '(("f" "eng" "f" "eng")
                     ("f" "en" "f" "eng")
                     ("h" "eng" "f" "eng")
                     ("h" "en" "f" "eng"))))
    (should (equal (pyim-pinyin-find-fuzzy '("h" "eng" "h" "eng"))
                   '(("h" "eng" "h" "eng")
                     ("h" "en" "h" "eng")
                     ("f" "eng" "h" "eng")
                     ("f" "en" "h" "eng")))))
  ;; pyim-pinyin-build-regexp
  (should (equal (pyim-pinyin-build-regexp "ni-hao")
                 "ni[a-z]*-hao[a-z]*"))
  (should (equal (pyim-pinyin-build-regexp "ni-hao" t)
                 "^ni[a-z]*-hao[a-z]*"))
  (should (equal (pyim-pinyin-build-regexp "ni-hao" nil t)
                 "ni-hao[a-z]*"))
  (should (equal (pyim-pinyin-build-regexp "ni-hao" nil nil t)
                 "ni-hao"))
  (should (equal (pyim-pinyin-build-regexp "ni-hao" t t)
                 "^ni-hao[a-z]*")))

;; ** pyim-punctuation 相关单元测试
(ert-deftest pyim-tests-pyim-punctuation ()
  (with-temp-buffer
    (insert ",")
    (pyim-punctuation-translate 'full-width)
    (should (equal (buffer-string) "，"))
    (pyim-punctuation-translate 'half-width)
    (should (equal (buffer-string) ",")))

  (with-temp-buffer
    (insert "[]")
    (backward-char 1)
    (pyim-punctuation-translate 'full-width)
    (should (equal (buffer-string) "【】"))
    (pyim-punctuation-translate 'half-width)
    (should (equal (buffer-string) "[]")))

  (with-temp-buffer
    (let ((pyim-punctuation-pair-status
           '(("\"" nil) ("'" nil))))
      (insert "[{''}]")
      (backward-char 3)
      (pyim-punctuation-translate 'full-width)
      (should (equal (buffer-string) "【『‘’』】"))
      (pyim-punctuation-translate 'half-width)
      (should (equal (buffer-string) "[{''}]"))))

  (with-temp-buffer
    (let ((pyim-punctuation-pair-status
           '(("\"" nil) ("'" nil))))
      (insert "[{''}]")
      (backward-char 3)
      (pyim-punctuation-translate-at-point)
      (should (equal (buffer-string) "【『‘’』】"))
      (pyim-punctuation-translate-at-point)
      (should (equal (buffer-string) "[{''}]"))))

  (let ((pyim-punctuation-pair-status
         '(("\"" nil) ("'" nil))))
    (should (equal (pyim-punctuation-return-proper-punct '("'" "‘" "’")) "‘"))
    (should (equal (pyim-punctuation-return-proper-punct '("'" "‘" "’")) "’"))))

;; ** pyim-entered 相关单元测试
(ert-deftest pyim-tests-pyim-entered ()
  (pyim-entered-with-entered-buffer
    (erase-buffer)
    (insert "nihao")
    (backward-char 3))
  (should (equal (pyim-entered-get) "nihao"))
  (should (equal (pyim-entered-get 'point-before) "ni"))
  (should (equal (pyim-entered-get 'point-after) "hao"))
  (pyim-entered-erase-buffer)
  (should (equal (pyim-entered-get) "")))

;; ** pyim-impobjs 相关单元测试
(ert-deftest pyim-tests-pyim-imobjs ()
  (let ((pyim-pinyin-fuzzy-alist '(("en" "eng")
                                   ("in" "ing")
                                   ("un" "ong"))))
    (should (equal (pyim-imobjs-create "nihao" 'quanpin)
                   '((("n" "i" "n" "i") ("h" "ao" "h" "ao")))))
    (should (equal (pyim-imobjs-create "nh" 'quanpin)
                   '((("n" "" "n" "") ("h" nil "h" nil)))))
    (should (equal (pyim-imobjs-create "xi'an" 'quanpin)
                   '((("x" "i" "x" "i") ("'" "an" "'" "an")))))
    (should (equal (pyim-imobjs-create "xian" 'quanpin)
                   '((("x" "ian" "x" "ian")))))
    (should (equal (pyim-imobjs-create "fenyun" 'quanpin)
                   '((("f" "en" "f" "en") ("y" "un" "y" "un"))
                     (("f" "en" "f" "en") ("y" "ong" "y" "un"))
                     (("f" "eng" "f" "en") ("y" "un" "y" "un"))
                     (("f" "eng" "f" "en") ("y" "ong" "y" "un")))))
    (should (equal (pyim-imobjs-create "xian" 'wubi)
                   '(("xian"))))
    (should (equal (pyim-imobjs-create "xian" 'cangjie)
                   '(("xian"))))
    (should (equal (pyim-imobjs-create "nihc" 'pyim-shuangpin)
                   '((("n" "i" "n" "i") ("h" "ao" "h" "c")))))))

;; ** pyim-codes 相关单元测试
(ert-deftest pyim-tests-pyim-codes ()
  (should (equal (pyim-codes-create
                  (car (pyim-imobjs-create "nihao" 'quanpin))
                  'quanpin)
                 '("ni" "hao")))
  (should (equal (pyim-codes-create
                  (car (pyim-imobjs-create "aaaa" 'wubi))
                  'wubi)
                 '("wubi/aaaa")))
  (should (equal (pyim-codes-create
                  (car (pyim-imobjs-create "aaaa" 'wubi))
                  'wubi 2)
                 '("wubi/aa")))
  (should (equal (pyim-codes-create
                  (car (pyim-imobjs-create "aaaa" 'wubi))
                  'wubi 1)
                 '("wubi/a")))
  (should (equal (pyim-codes-create
                  (car (pyim-imobjs-create "aaaa" 'cangjie))
                  'cangjie)
                 '("cangjie/aaaa"))))

;; ** pyim-candidates 相关单元测试
(ert-deftest pyim-tests-pyim-candidates-search-buffer ()
  (with-temp-buffer
    (insert "你好你好你坏你坏你话牛蛤牛和牛蛤牛蛤牛蛤牛蛤牛蛤")
    (should (equal (pyim-candidates-search-buffer (pyim-cregexp-build "nh" 3 t))
                   '("牛蛤" "你坏" "你好" "牛和" "你话")))))

;; ** pyim-cstring 相关单元测试
(ert-deftest pyim-tests-pyim-cstring-partition ()
  (should (equal (pyim-cstring-partition "你好 hello 你好")
                 '("你好" " hello " "你好")))
  (should (equal (pyim-cstring-partition "你好 hello 你好" t)
                 '("你" "好" " hello " "你" "好")))
  (should (equal (pyim-cstring-partition "你好")
                 '("你好")))
  (should (equal (pyim-cstring-partition "你好" t)
                 '("你" "好")))
  (should (equal (pyim-cstring-partition "hello")
                 '("hello")))
  (should (equal (pyim-cstring-partition "hello" t)
                 '("hello"))))

(ert-deftest pyim-tests-pyim-cstring-substrings ()
  (should (equal (pyim-cstring-substrings "我爱北京")
                 '(("我爱北京" 0 4)
                   ("我爱北" 0 3)
                   ("我爱" 0 2)
                   ("爱北京" 1 4)
                   ("爱北" 1 3)
                   ("北京" 2 4))))
  (should (equal (pyim-cstring-substrings "我爱北京" 3)
                 '(("我爱北" 0 3)
                   ("我爱" 0 2)
                   ("爱北京" 1 4)
                   ("爱北" 1 3)
                   ("北京" 2 4))))
  (let* ((str "我爱北京")
         (alist (pyim-cstring-substrings "我爱北京" 2))
         (key (car (nth 1 alist)))
         (pos (cdr (nth 1 alist))))
    (should (equal (substring str (car pos) (cadr pos)) key))))

(ert-deftest pyim-tests-pyim-cstring-split ()
  (let ((pyim-dhashcache-code2word (make-hash-table :test #'equal))
        (str "我爱北京天安门"))

    ;; Create code2word dcache.
    (puthash "wo-ai" (list "我爱") pyim-dhashcache-code2word)
    (puthash "bei-jing" (list "北京") pyim-dhashcache-code2word)
    (puthash "tian-an" (list "天安") pyim-dhashcache-code2word)
    (puthash "an-men" (list "安门") pyim-dhashcache-code2word)
    (puthash "tian-an-men" (list "天安门") pyim-dhashcache-code2word)

    ;; pyim-cstring-split-to-list
    (should (equal (pyim-cstring-split-to-list str)
                   '(("安门" 5 7) ("天安" 4 6) ("天安门" 4 7)
                     ("北京" 2 4) ("我爱" 0 2))))
    (should (equal (pyim-cstring-split-to-list str 2)
                   '(("安门" 5 7) ("天安" 4 6)
                     ("北京" 2 4) ("我爱" 0 2))))
    (should (equal (pyim-cstring-split-to-list str 2)
                   '(("安门" 5 7) ("天安" 4 6)
                     ("北京" 2 4) ("我爱" 0 2))))
    (should (equal (pyim-cstring-split-to-list str nil t)
                   '(("天安门" 4 7) ("北京" 2 4) ("我爱" 0 2))))
    (should (equal (pyim-cstring-split-to-list str nil t t)
                   '(("安门" 5 7) ("北京" 2 4) ("我爱" 0 2))))
    (should (equal (pyim-cstring-split-to-string "我爱北京天安门")
                   "我爱 北京 天安门"))
    (should (equal (pyim-cstring-split-to-string "haha我爱北京天安门haha")
                   "haha 我爱 北京 天安门 haha"))
    (should (equal (pyim-cstring-split-to-string "haha我爱北京天安门haha" nil "-")
                   "haha-我爱-北京-天安门-haha"))
    (should (equal (pyim-cstring-split-to-string "haha 我爱北京天安门 haha" nil "-")
                   "haha -我爱-北京-天安门- haha"))

    ;; pyim-cstring-split-to-string
    (should (equal (pyim-cstring-split-to-string "我爱北京天安门" t)
                   "我爱 北京 天 安门"))
    (should (equal (pyim-cstring-split-to-string "我爱北京天安门" nil "-")
                   "我爱-北京-天安门"))
    (should (equal (pyim-cstring-split-to-string "我爱北京天安门" nil "-" 2)
                   "我爱-北京-天安-门"))))

(ert-deftest pyim-tests-pyim-cstring-to-pinyin ()
  (let ((pyim-dhashcache-code2word (make-hash-table :test #'equal))
        (str "银行很行"))
    ;; Create code2word dcache.
    (puthash "yin-hang-hen-xing" (list "银行很行") pyim-dhashcache-code2word)
    ;; pyim-cstring-split-to-list
    (should (equal (pyim-cstring-to-pinyin "银行很行")
                   (concat "yinxinghenxing yinxinghenheng yinxinghenhang "
                           "yinhenghenxing yinhenghenheng yinhenghenhang "
                           "yinhanghenxing yinhanghenheng yinhanghenhang")))
    (should (equal (pyim-cstring-to-pinyin "银行很行" t)
                   "yxhx yxhh yxhh yhhx yhhh yhhh yhhx yhhh yhhh"))
    (should (equal (pyim-cstring-to-pinyin "银行很行" nil "-")
                   (concat "yin-xing-hen-xing yin-xing-hen-heng yin-xing-hen-hang "
                           "yin-heng-hen-xing yin-heng-hen-heng yin-heng-hen-hang "
                           "yin-hang-hen-xing yin-hang-hen-heng yin-hang-hen-hang")))
    (should (equal (pyim-cstring-to-pinyin "银行很行" nil "-" t)
                   '("yin-xing-hen-xing" "yin-xing-hen-heng" "yin-xing-hen-hang"
                     "yin-heng-hen-xing" "yin-heng-hen-heng" "yin-heng-hen-hang"
                     "yin-hang-hen-xing" "yin-hang-hen-heng" "yin-hang-hen-hang")))
    (should (equal (pyim-cstring-to-pinyin "银行很行" nil "-" t t)
                   '("yin-xing-hen-xing")))
    (should (equal (pyim-cstring-to-pinyin "银行很行" nil "-" nil nil t)
                   "yin-hang-hen-xing"))
    (should (equal (pyim-cstring-to-pinyin "Hello 银行很行 Hi" nil "-" nil t)
                   "Hello -yin-xing-hen-xing- Hi"))
    ;; FIXME: 这个 test 是不合理的，不过暂时找不到简单的修复方式。
    (should (equal (pyim-cstring-to-pinyin "Hello 银行很行 Hi" nil "-" nil nil t)
                   (concat "Hello -yin-xing-hen-xing- Hi Hello -yin-xing-hen-heng- Hi "
                           "Hello -yin-xing-hen-hang- Hi Hello -yin-heng-hen-xing- Hi "
                           "Hello -yin-heng-hen-heng- Hi Hello -yin-heng-hen-hang- Hi "
                           "Hello -yin-hang-hen-xing- Hi Hello -yin-hang-hen-heng- Hi "
                           "Hello -yin-hang-hen-hang- Hi")))))

(ert-deftest pyim-tests-pyim-cstring-to-xingma ()
  (let ((pyim-dhashcache-word2code (make-hash-table :test #'equal)))
    (puthash "工" (list "wubi/aaaa" "cangjie/mlm" "gong") pyim-dhashcache-word2code)
    (puthash "房" (list "wubi/yny") pyim-dhashcache-word2code)
    (puthash "丛" (list "wubi/wwg") pyim-dhashcache-word2code)
    (should (equal (pyim-cstring-to-xingma "工" 'wubi) "aaaa"))
    (should (equal (pyim-cstring-to-xingma "工房" 'wubi) "aayn"))
    (should (equal (pyim-cstring-to-xingma "工房丛" 'wubi) "ayww"))
    (should (equal (pyim-cstring-to-xingma "工房丛房" 'wubi) "aywy"))
    (should (equal (pyim-cstring-to-xingma "工" 'wubi t) '("aaaa")))))

(ert-deftest pyim-tests-pyim-cstring-words-at-point ()
  (let ((pyim-dhashcache-code2word (make-hash-table :test #'equal)))
    (puthash "tian-an" (list "天安") pyim-dhashcache-code2word)
    (puthash "an-men" (list "安门") pyim-dhashcache-code2word)
    (puthash "tian-an-men" (list "天安门") pyim-dhashcache-code2word)
    (with-temp-buffer
      (insert "天安门abc\n天安门")
      ;; <I>天安门abc
      (goto-char (point-min))
      (should (equal (pyim-cstring-words-at-point) nil))
      ;; 天<I>安门abc
      (forward-char 1)
      (should (equal (pyim-cstring-words-at-point)
                     '(("天安门" 1 2) ("天安" 1 1))))
      ;; 天安门abc<I>
      (forward-char 5)
      (should (equal (pyim-cstring-words-at-point)
                     '(("abc" 3 0))))
      ;; <I>天安门
      (forward-char 1)
      (should (equal (pyim-cstring-words-at-point) nil))
      ;; 天安门<I>
      (goto-char (point-max))
      (should (equal (pyim-cstring-words-at-point)
                     '(("天安门" 3 0) ("安门" 2 0)))))))

;; ** pyim-cregexp 相关单元测试
(ert-deftest pyim-tests-pyim-cregexp ()
  (let ((regexp (pyim-cregexp-build "nihao")))
    (should (string-match-p regexp "nihao"))
    (should (string-match-p regexp "anihaob"))
    (should (string-match-p regexp "你好"))
    (should (string-match-p regexp "哈哈你好吗")))

  (let ((regexp (pyim-cregexp-build "nihao" nil t)))
    (should-not (string-match-p regexp "nihao"))
    (should-not (string-match-p regexp "anihaob"))
    (should (string-match-p regexp "你好"))
    (should (string-match-p regexp "哈哈你好吗")))

  (let ((regexp (pyim-cregexp-build "beng")))
    (should (string-match-p regexp "痭"))
    (should (string-match-p regexp "泵"))
    (should (string-match-p regexp "堋"))
    (should (string-match-p regexp "洴")))

  (let ((regexp (pyim-cregexp-build "ni.*ma")))
    (should (string-match-p regexp "nihaoma"))
    (should (string-match-p regexp "nima"))
    (should (string-match-p regexp "你好吗"))
    (should (string-match-p regexp "你不好吗"))
    (should (string-match-p regexp "哈哈你不好吗")))

  (let ((regexp (pyim-cregexp-build "nh")))
    (should (string-match-p regexp "nh"))
    (should (string-match-p regexp "anhb"))
    (should (string-match-p regexp "你好"))
    (should (string-match-p regexp "牛哈"))
    (should (string-match-p regexp "摄和")))

  (let ((regexp (pyim-cregexp-build "nha")))
    (should (string-match-p regexp "nha"))
    (should (string-match-p regexp "anhab"))
    (should (string-match-p regexp "你哈"))
    (should (string-match-p regexp "牛蛤")))

  (let* ((str (nth 2 (split-string (car (pyim-pymap-py2cchar-get "wang" t)) "|")))
         (regexp1 (pyim-cregexp-build-1 "wang" 3))
         (regexp2 (pyim-cregexp-build-1 "wang" 2)))
    (should (string-match-p regexp1 str))
    (should-not (string-match-p regexp2 str)))

  (let* ((imobj '(("d" "a" "d" "a") ("w" "ang" "w" "ang")))
         (regexp1 (pyim-cregexp-build:quanpin imobj))
         (regexp2 (pyim-cregexp-build:quanpin imobj nil nil t)))
    (should (string-match-p regexp1 "大王"))
    (should (string-match-p regexp1 "当王"))
    (should (string-match-p regexp2 "大王"))
    (should-not (string-match-p regexp2 "当王")))

  (let ((pyim-default-scheme 'wubi)
        (pyim-dhashcache-code2word (make-hash-table :test #'equal)))
    (puthash "wubi/aaaa" (list "工" "恭恭敬敬") pyim-dhashcache-code2word)
    (puthash "wubi/adww" (list "欺" "蒙古人" "其人" "欺人" "斯人" "惹人" "匧" "歁" "莢") pyim-dhashcache-code2word)
    (should (equal (pyim-cregexp-build "aaaa") "\\(?:aaaa\\|[工恭]恭?敬?敬?\\)"))
    (should (equal (pyim-cregexp-build "adww") "\\(?:adww\\|[其匧惹斯欺歁莢蒙][人古]?人?\\)"))
    (should (equal (pyim-cregexp-build "aaaa'aaaa")
                   "\\(?:\\(?:aaaa'\\|aaaa\\|[工恭]恭?敬?敬?\\)\\(?:aaaa\\|[工恭]恭?敬?敬?\\)\\)"))
    (should (equal (pyim-cregexp-build-1 "aaaa'aaaa")
                   "\\(?:aaaa'\\|aaaa\\|[工恭][恭]?[敬]?[敬]?\\)\\(?:aaaa\\|[工恭][恭]?[敬]?[敬]?\\)"))))

;; ** pyim-import 相关单元测试
(ert-deftest pyim-tests-pyim-import-words-and-counts ()
  ;; 这个测试目前主要用于手工测试，在 github 上这个测试无法通过的。
  (when (not noninteractive)
    (let ((pyim-dcache-directory (pyim-tests-make-temp-file t))
          (file (pyim-tests-make-temp-file)))
      ;; 删除测试用词条
      (dolist (x '("测㤅" "测嘊" "测伌"))
        (pyim-process-delete-word x))
      (dolist (x '("测㤅" "测嘊" "测伌"))
        (should-not (member x (pyim-dcache-get "ce-ai" '(icode2word)))))
      (should-not (equal (gethash "测㤅" pyim-dhashcache-iword2count) 76543))
      (should-not (equal (gethash "测嘊" pyim-dhashcache-iword2count) 34567))
      (should-not (equal (gethash "测伌" pyim-dhashcache-iword2count) 0))

      ;; 导入测试用词条
      (with-temp-buffer
        (insert
         ";;; -*- coding: utf-8-unix -*-
测㤅 76543 ce-ai
测嘊 34567
测伌")
        (write-file file))
      (pyim-import-words-and-counts file (lambda (orig-count new-count) new-count) t)

      ;; 测试词条是否存在
      (dolist (x '("测㤅" "测嘊" "测伌"))
        (should (member x (pyim-dcache-get "ce-ai" '(icode2word)))))
      (should (equal (gethash "测㤅" pyim-dhashcache-iword2count) 76543))
      (should (equal (gethash "测嘊" pyim-dhashcache-iword2count) 34567))
      (should (equal (gethash "测伌" pyim-dhashcache-iword2count) 0)))))

;; ** pyim-dcache 相关单元测试
(ert-deftest pyim-tests-pyim-dcache-save/read-variable-value ()
  (let* ((file (pyim-tests-make-temp-file))
         (backup-file (concat file "-backup-" (format-time-string "%Y%m%d%H%M%S")))
         (value (make-hash-table :test #'equal)))
    (puthash "ni-hao" (list "你好") value)
    (pyim-dcache-save-value-to-file value file)
    (should (equal (gethash "ni-hao" (pyim-dcache-get-value-from-file file))
                   '("你好")))
    (should-not (file-exists-p backup-file))
    (pyim-dcache-save-value-to-file "" file 0.8)
    (should (file-exists-p backup-file))
    (should (equal (gethash "ni-hao" (pyim-dcache-get-value-from-file backup-file))
                   '("你好")))))

(ert-deftest pyim-tests-pyim-dcache-handle-variable ()
  (let ((pyim-dcache-directory (pyim-tests-make-temp-file t))
        my/test:1)

    (pyim-dcache-save-variable 'my/test:1 "hello")
    (should (equal (pyim-dcache-get-value 'my/test:1) "hello"))

    (setq my/test:1 "hi")
    (pyim-dcache-reload-variable my/test:1)
    (should (equal my/test:1 "hello"))

    (setq my/test:1 "hi")
    (pyim-dcache-init-variable my/test:1)
    (should (equal my/test:1 "hi"))

    (setq my/test:1 nil)
    (pyim-dcache-init-variable my/test:1)
    (should (equal my/test:1 "hello"))))

(ert-deftest pyim-tests-pyim-dcache-export ()
  (let ((pyim-dhashcache-iword2count (make-hash-table :test #'equal))
        (pyim-dhashcache-icode2word (make-hash-table :test #'equal))
        (file (pyim-tests-make-temp-file)))
    (puthash "你好" 10 pyim-dhashcache-iword2count)
    (puthash "尼耗" 1 pyim-dhashcache-iword2count)
    (puthash "ni-hao" (list "你好" "尼耗") pyim-dhashcache-icode2word)
    (pyim-dcache-export-words-and-counts file)
    (with-temp-buffer
      (insert-file-contents file)
      (should (equal (buffer-string)
                     ";;; -*- coding: utf-8-unix -*-
你好 10
尼耗 1
")))
    (pyim-dcache-export-words-and-counts file nil t)
    (with-temp-buffer
      (insert-file-contents file)
      (should (equal (buffer-string)
                     ";;; -*- coding: utf-8-unix -*-
你好
尼耗
")))
    (pyim-dcache-export-personal-words file)
    (with-temp-buffer
      (insert-file-contents file)
      (should (equal (buffer-string)
                     ";;; -*- coding: utf-8-unix -*-
ni-hao 你好 尼耗
")))))

;; ** pyim-dhashcache 相关单元测试
(ert-deftest pyim-tests-pyim-dhashcache-get-shortcodes ()
  (should (equal (pyim-dhashcache-get-shortcodes ".abcde") nil))
  (should (equal (pyim-dhashcache-get-shortcodes "wubi/abcde")
                 '("wubi/abcd" "wubi/abc" "wubi/ab")))
  (should (equal (pyim-dhashcache-get-shortcodes "abcde") nil))
  (should (equal (pyim-dhashcache-get-shortcodes "ni-hao") nil))
  (should (equal (pyim-dhashcache-get-shortcodes "") nil)))

(ert-deftest pyim-tests-pyim-dhashcache-get-ishortcodes ()
  (should (equal (pyim-dhashcache-get-ishortcodes "ni-hao") '("n-h")))
  (should (equal (pyim-dhashcache-get-ishortcodes "wubi/aaaa") nil))
  (should (equal (pyim-dhashcache-get-ishortcodes "ni") '("n")))
  (should (equal (pyim-dhashcache-get-ishortcodes "") nil)))

(ert-deftest pyim-tests-pyim-dhashcache-get-path ()
  (let* ((dir (pyim-tests-make-temp-file t))
         (pyim-dcache-directory dir))
    (should (equal (pyim-dhashcache-get-path 'hello) (expand-file-name "hello" dir)))
    (should (equal (pyim-dhashcache-get-path "hello") nil))))

(ert-deftest pyim-tests-pyim-dhashcache-generate-file ()
  (let ((dist-file (pyim-tests-make-temp-file))
        (dcache-file (pyim-tests-make-temp-file))
        (word2code-dcache-file (pyim-tests-make-temp-file))
        output1 output2)
    (with-temp-buffer
      (insert ";; -*- coding: utf-8 -*--
a 阿 啊 呵 腌 吖 嗄 锕 錒
a-a 啊啊
zuo-zuo-ye 做作业
zuo-zuo-you-mang 作作有芒")
      (write-region (point-min) (point-max) dist-file))
    (pyim-dhashcache-generate-dcache-file (list dist-file) dcache-file)
    (with-temp-buffer
      (insert-file-contents dcache-file)
      (setq output1 (read (current-buffer)))
      (pyim-dhashcache-generate-word2code-dcache-file output1 word2code-dcache-file))
    (with-temp-buffer
      (insert-file-contents word2code-dcache-file)
      (setq output2 (read (current-buffer))))

    (should (equal (gethash "a" output1) '("阿" "啊" "呵" "腌" "吖" "嗄" "锕" "錒")))
    (should (equal (gethash "a-a" output1) '("啊啊")))
    (should (equal (gethash "zuo-zuo-you-mang" output1) '("作作有芒")))
    (should (equal (gethash "啊" output2) '("a")))
    (should (equal (gethash "啊啊" output2) nil))))

(ert-deftest pyim-tests-pyim-dhashcache-update-shortcode2word ()
  (let ((code2word (make-hash-table :test #'equal))
        (iword2count (make-hash-table :test #'equal))
        (shortcode2word (make-hash-table :test #'equal))
        output)

    (puthash "wubi/a" '("戈") code2word)
    (puthash "wubi/aa" '("式" "藏") code2word)
    (puthash "wubi/aaa" '("工") code2word)
    (puthash "wubi/aaaa" '("工" "㠭") code2word)
    (puthash "wubi/aaab" '("㐂") code2word)
    (puthash "wubi/aaae" '("𧝣") code2word)

    (setq shortcode2word
          (pyim-dhashcache-update-shortcode2word-1 code2word iword2count))

    (should (equal (gethash "wubi/aa" shortcode2word)
                   '(#("工" 0 1 (:comment "a"))
                     #("㠭" 0 1 (:comment "aa"))
                     #("㐂" 0 1 (:comment "ab"))
                     #("𧝣" 0 1 (:comment "ae")))))
    (should (equal (gethash "wubi/aaa" shortcode2word)
                   '(#("工" 0 1 (:comment "a"))
                     #("㠭" 0 1 (:comment "a"))
                     #("㐂" 0 1 (:comment "b"))
                     #("𧝣" 0 1 (:comment "e")))))))

(ert-deftest pyim-tests-pyim-dhashcache-update-ishortcode2word ()
  (let ((icode2word (make-hash-table :test #'equal))
        (iword2count (make-hash-table :test #'equal))
        ishortcode2word)

    (puthash "ni" '("你" "呢") icode2word)
    (puthash "ni-hao" '("你好" "呢耗") icode2word)
    (puthash "ni-huai" '("你坏") icode2word)

    (setq ishortcode2word
          (pyim-dhashcache-update-ishortcode2word-1
           icode2word iword2count))

    (should (equal (gethash "n-h" ishortcode2word)
                   '("你好" "呢耗" "你坏")))
    (should (equal (gethash "n" ishortcode2word)
                   '("你" "呢")))))

(ert-deftest pyim-tests-pyim-dhashcache-put/delete ()
  (let ((pyim-dhashcache-icode2word (make-hash-table :test #'equal)))
    (puthash "ni-hao" '("你好" "呢耗") pyim-dhashcache-icode2word)
    (pyim-dhashcache-put
      pyim-dhashcache-icode2word "ni-hao"
      (cons "呢毫" orig-value))
    (pyim-dhashcache-put
      pyim-dhashcache-icode2word "ni-bu-hao"
      (list "你不好"))
    (should (equal (gethash "ni-hao" pyim-dhashcache-icode2word) '("呢毫" "你好" "呢耗")))
    (should (equal (gethash "ni-bu-hao" pyim-dhashcache-icode2word) '("你不好")))
    (pyim-dhashcache-delete-word "你不好")
    (should (equal (gethash "ni-bu-hao" pyim-dhashcache-icode2word) nil))
    (pyim-dhashcache-delete-word "你好")
    (should (equal (gethash "ni-hao" pyim-dhashcache-icode2word) '("呢毫" "呢耗")))))

(ert-deftest pyim-tests-pyim-dhashcache-update-iword2count ()
  (let ((pyim-dhashcache-iword2count (make-hash-table :test #'equal)))
    (puthash "你好" 1 pyim-dhashcache-iword2count)
    (pyim-dhashcache-update-iword2count "你好")
    (should (equal (gethash "你好" pyim-dhashcache-iword2count) 1))
    (pyim-dhashcache-update-iword2count "你好" #'1+)
    (should (equal (gethash "你好" pyim-dhashcache-iword2count) 2))
    (pyim-dhashcache-update-iword2count "你好" 10)
    (should (equal (gethash "你好" pyim-dhashcache-iword2count) 10))
    (pyim-dhashcache-update-iword2count "你好" (lambda (x) (* x 2)))
    (should (equal (gethash "你好" pyim-dhashcache-iword2count) 20))))

(ert-deftest pyim-tests-pyim-dhashcache-export ()
  (let ((file (pyim-tests-make-temp-file))
        (icode2word (make-hash-table :test #'equal)))
    (puthash "yin-xing"
             (list (propertize "银行" :noexport t)
                   "因行") icode2word)
    (pyim-dhashcache-export icode2word file)
    (with-temp-buffer
      (insert-file-contents file)
      (should (equal(buffer-string) ";;; -*- coding: utf-8-unix -*-
yin-xing 因行
")))))

(ert-deftest pyim-tests-pyim-dhashcache-get ()
  (let ((pyim-dhashcache-code2word (make-hash-table :test #'equal))
        (pyim-dhashcache-icode2word (make-hash-table :test #'equal))
        (pyim-dhashcache-iword2count (make-hash-table :test #'equal)))

    (puthash "ni-hao" '("呢耗") pyim-dhashcache-icode2word)
    (puthash "ni-hao" '("你好") pyim-dhashcache-code2word)
    (puthash "你好" 10 pyim-dhashcache-iword2count)

    (should (equal (pyim-dhashcache-get "ni-hao" '(code2word)) '("你好")))
    (should (equal (pyim-dhashcache-get "ni-hao" '(icode2word)) '("呢耗")))
    (should (equal (pyim-dhashcache-get "你好" '(iword2count)) '(10)))
    (should (equal (pyim-dhashcache-get "ni-hao" '(code2word icode2word)) '("你好" "呢耗")))
    (should (equal (pyim-dhashcache-get "ni-hao") '("呢耗" "你好")))))

(ert-deftest pyim-tests-pyim-dhashcache-insert-word-into-icode2word ()
  (let ((pyim-dhashcache-icode2word (make-hash-table :test #'equal)))
    (pyim-dhashcache-insert-word-into-icode2word "你好" "ni-hao" t)
    (pyim-dhashcache-insert-word-into-icode2word "你耗" "ni-hao" t)
    (pyim-dhashcache-insert-word-into-icode2word "你豪" "ni-hao" nil)
    (should (equal (gethash "ni-hao" pyim-dhashcache-icode2word)
                   '("你耗" "你好" "你豪")))))

(ert-deftest pyim-tests-pyim-dhashcache-insert-word-into-ishortcode2word ()
  (let ((pyim-dhashcache-ishortcode2word (make-hash-table :test #'equal)))
    (pyim-dhashcache-insert-word-into-ishortcode2word "你好" "ni-hao" t)
    (pyim-dhashcache-insert-word-into-ishortcode2word "你慌" "ni-huang" t)
    (pyim-dhashcache-insert-word-into-ishortcode2word "你坏" "ni-huai" nil)
    (should (equal (gethash "n-h" pyim-dhashcache-ishortcode2word)
                   '("你慌" "你好" "你坏")))))

(ert-deftest pyim-tests-pyim-dhashcache-sort-words ()
  (let ((pyim-dhashcache-iword2count (make-hash-table :test #'equal))
        (weight-table (make-hash-table :test #'equal))
        words)
    (puthash "你好" 3 pyim-dhashcache-iword2count)
    (puthash "呢耗" 2 pyim-dhashcache-iword2count)
    (puthash "你豪" 1 pyim-dhashcache-iword2count)

    (puthash "你好" 0.1 weight-table)
    (puthash "呢耗" 0.3 weight-table)
    (puthash "你豪" 5   weight-table)

    (setq words (list "呢耗" "你豪" "你好"))
    (should (equal (pyim-dhashcache-sort-words words)
                   '("你好" "呢耗" "你豪")))

    (setq words (list "呢耗" "你豪" "你好"))
    (should (equal (pyim-dhashcache-sort-words words pyim-dhashcache-iword2count)
                   '("你好" "呢耗" "你豪")))

    (setq words (list "呢耗" "你豪" "你好"))
    (should  (equal (pyim-dhashcache-sort-words words nil weight-table)
                    '("你豪" "呢耗" "你好")))))

;; ** pyim-dregcache 相关单元测试
(ert-deftest pyim-tests-pyim-general ()
  (let ((pyim-dcache-backend 'pyim-dregcache))
    (with-temp-buffer
      (should (not toggle-input-method-active))
      (call-interactively #'toggle-input-method))))

(ert-deftest pyim-tests-pyim-dregcache-backend ()
  (let ((pyim-dcache-backend 'pyim-dregcache)
        words)
    (should (eq (length pyim-dregcache-cache) 0))
    ;; load dictionary
    (pyim-dcache-update t)
    ;; cache is filled
    (should (> (length pyim-dregcache-cache) 0))

    ;; test dregcache api
    (setq words (pyim-dcache-get "zun-bei"))
    (should (eq (length words) 1))
    (should (string= (nth 0 words) "尊卑"))

    (setq words (pyim-dcache-get "zun"))
    (should (string= (nth 0 words) "尊"))
    ;; `pyim-dregcache-get' calls `pyim-pymap-py2cchar-get' before return result
    (should (eq (length words) 51))))

(ert-run-tests-batch-and-exit)
;; * Footer
;;; pyim-tests.el ends here
