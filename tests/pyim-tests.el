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
(require 'pyim-dregcache)
(require 'pyim-dhashcache)

;; ** 单元测试前的准备工作
(defun pyim-tests-add-dict (file-name)
  "搜索文件名称为 FILE-NAME 的词库，并添加到 `pyim-dicts'."
  (let ((file (expand-file-name (concat default-directory "/deps/" file-name))))
    (if (file-exists-p file)
        (cl-pushnew
         (list :name (file-name-base file) :file file)
         pyim-dicts)
      (message "pyim-test: fail to find dict file: '%s'." file))))

(setq default-input-method "pyim")
(pyim-tests-add-dict "pyim-basedict.pyim")
(pyim-dcache-init-variables)

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
                     ("h" "eng" "f" "en")))))
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
    (insert "
一日，正当嗟悼之际，俄见一僧一道远远而来，生得骨格不凡，丰神迥别，说说
笑笑，来至峰下，坐于石边，高谈快论：先是说些云山雾海、神仙玄幻之事，后
便说到红尘中荣华富贵。此石听了，不觉打动凡心，也想要到人间去享一享这荣
华富贵，但自恨粗蠢，不得已，便口吐人言，向那僧道说道：“大师，弟子蠢物，
不能见礼了！适闻二位谈那人世间荣耀繁华，心切慕之。弟子质虽粗蠢，性却稍
通，况见二师仙形道体，定非凡品，必有补天济世之材，利物济人之德。如蒙发
一点慈心，携带弟子得入红尘，在那富贵场中，温柔乡里受享几年，自当永佩洪
恩，万劫不忘也！”二仙师听毕，齐憨笑道：“善哉，善哉！那红尘中有却有些乐
事，但不能永远依恃；况又有‘美中不足，好事多磨’八个字紧相连属，瞬息间则
又乐极悲生，人非物换，究竟是到头一梦，万境归空，倒不如不去的好。”这石
凡心已炽，那里听得进这话去，乃复苦求再四。二仙知不可强制，乃叹道：“此
亦静极思动，无中生有之数也！既如此，我们便携你去受享受享，只是到不得意
时，切莫后悔！”石道：“自然，自然。”那僧又道：“若说你性灵，却又如此质蠢，
并更无奇贵之处。如此也只好踮脚而已。也罢！我如今大施佛法，助你助，待劫
终之日，复还本质，以了此案。你道好否？”石头听了，感谢不尽。那僧便念咒
书符，大展幻术，将一块大石登时变成一块鲜明莹洁的美玉，且又缩成扇坠大小
的可佩可拿。那僧托于掌上，笑道：“形体倒也是个宝物了！还只没有实在的好
处，须得再镌上数字，使人一见便知是奇物方妙。然后好携你到那昌明隆盛之邦、
诗礼簪缨之族、花柳繁华地、温柔富贵乡去安身乐业。”石头听了，喜不能禁，
乃问：“不知赐了弟子那哪几件奇处？又不知携了弟子到何地方？望乞明示，使
弟子不惑。”那僧笑道：“你且莫问，日后自然明白的。”说着，便袖了这石，同
那道人飘然而去，竟不知投奔何方何舍。")
    (should (equal (pyim-candidates-search-buffer (pyim-cregexp-build "hs" 3 t))
                   '("何舍" "幻术" "好事")))))

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
    (should-not (string-match-p regexp2 "当王"))))

;; ** pyim-import 相关单元测试
(ert-deftest pyim-tests-pyim-import-words-and-counts ()
  ;; 这个测试目前主要用于手工测试，在 github 上这个测试无法通过的。
  :expected-result :failed
  (let ((file (make-temp-file "pyim-tests-import")))
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
    (pyim-delete-word)

    ;; 测试词条是否存在
    (dolist (x '("测㤅" "测嘊" "测伌"))
      (should (member x (pyim-dcache-get "ce-ai" '(icode2word)))))
    (should (equal (gethash "测㤅" pyim-dhashcache-iword2count) 76543))
    (should (equal (gethash "测嘊" pyim-dhashcache-iword2count) 34567))
    (should (equal (gethash "测伌" pyim-dhashcache-iword2count) 0))))

;; ** pyim-dhashcache 相关单元测试
(ert-deftest pyim-tests-pyim-dhashcache-get-shortcode ()
  (should (equal (pyim-dhashcache-get-shortcode ".abcde")
                 '(".abcd" ".abc" ".ab")))
  (should (equal (pyim-dhashcache-get-shortcode "wubi/abcde")
                 '("wubi/abcd" "wubi/abc" "wubi/ab")))
  (should (equal (pyim-dhashcache-get-shortcode "abcde")
                 '("abcd" "abc" "ab")))
  (should (equal (pyim-dhashcache-get-shortcode "ni-hao") nil))
  (should (equal (pyim-dhashcache-get-shortcode "") nil)))

(ert-deftest pyim-tests-pyim-dhashcache-get-path ()
  (let ((pyim-dcache-directory "/tmp/dcache"))
    (should (equal (pyim-dhashcache-get-path 'hello) "/tmp/dcache/hello"))
    (should (equal (pyim-dhashcache-get-path "hello") nil))))

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
    (pyim-dcache-update-code2word t)
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
