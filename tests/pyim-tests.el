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
;; pyim test case.
;; 1.  [50%]  pyim-autoselector.el
;; 2.  [90%]  pyim-candidates.el
;; 3.  [95%]  pyim-cloudim.el
;; 4.  DONE   pyim-codes.el
;; 5.  DONE   pyim-common.el
;; 6.  [95%]  pyim-cregexp.el
;; 7.  [95%]  pyim-cregexp-utils.el
;; 8.  DONE   pyim-cstring.el
;; 9.  DONE   pyim-cstring-utils.el
;; 10. DONE   pyim-dcache.el
;; 11. [95%]  pyim-dhashcache.el
;; 12. DONE   pyim-dict.el
;; 13. IGNORE pyim-dict-manager.el
;; 14. [20%]  pyim-dregcache.el
;; 15. TODO   pyim.el
;; 16. DONE   pyim-entered.el
;; 17. DONE   pyim-imobjs.el
;; 18. TODO   pyim-indicator.el
;; 19. IGNORE pyim-liberime.el
;; 20. TODO   pyim-outcome.el
;; 21. [30%]  pyim-page.el
;; 22. DONE   pyim-pinyin.el
;; 23. [30%]  pyim-preview.el
;; 24. [95%]  pyim-probe.el
;; 25. TODO   pyim-process.el
;; 26. DONE   pyim-punctuation.el
;; 27. DONE   pyim-pymap.el
;; 28. IGNORE pyim-pymap-utils.el
;; 29  DONE   pyim-scheme.el

;;; Code:
;; * 代码                                                                 :code:
(require 'ert)
(require 'pyim)
(require 'pyim-basedict) ;pyim-test.el use pyim-basedict v0.5.0.
(require 'pyim-cstring-utils)
(require 'pyim-cregexp-utils)
(require 'pyim-dhashcache)
(require 'pyim-dregcache)
(require 'pyim-pymap-utils)

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
    (should (equal (pyim-scheme-name
                    (pyim-scheme-current))
                   'wubi)))

  (let ((pyim-default-scheme 'wuci))
    (should (equal (pyim-scheme-name
                    (pyim-scheme-current))
                   'quanpin)))

  (let ((pyim-default-scheme 'wubi)
        (pyim-assistant-scheme 'cangjie)
        (pyim-assistant-scheme-enable t))
    (should (equal (pyim-scheme-name
                    (pyim-scheme-current))
                   'cangjie)))

  (let ((pyim-default-scheme 'wubi)
        (pyim-assistant-scheme 'cangjie)
        (pyim-assistant-scheme-enable nil))
    (should (equal (pyim-scheme-name
                    (pyim-scheme-current))
                   'wubi)))

  (should (equal (pyim-scheme-name
                  (pyim-scheme-get 'quanpin))
                 'quanpin))

  (should-not (pyim-scheme-get 'quanpin1))

  (should (equal (pyim-scheme-name
                  (pyim-scheme-get 'wubi))
                 'wubi)))

(ert-deftest pyim-tests-pyim-scheme-add ()
  (let ((pyim-schemes nil))
    (pyim-scheme-add
     '(quanpin
       :document "test1"
       :class quanpin
       :first-chars "abcdefghijklmnopqrstuwxyz"
       :rest-chars "vmpfwckzyjqdltxuognbhsrei'-a"
       :prefer-triggers ("v")))

    (pyim-scheme-add
     '(quanpin
       :document "test2"
       :class quanpin
       :first-chars "abcdefghijklmnopqrstuwxyz"
       :rest-chars "vmpfwckzyjqdltxuognbhsrei'-a"
       :prefer-triggers ("v")))

    (pyim-scheme-add
     '(quanpin1
       :document "test3"
       :class quanpin
       :first-chars "abcdefghijklmnopqrstuwxyz"
       :rest-chars "vmpfwckzyjqdltxuognbhsrei'-a"
       :prefer-triggers ("v")))

    (pyim-scheme-add "error")

    (should (equal (mapcar #'pyim-scheme-document pyim-schemes)
                   '("test2" "test3")))))

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

(ert-deftest pyim-test-pyim-proportion ()
  (should (equal (pyim-proportion '(1 2 3 4))
                 '(0.1 0.2 0.3 0.4))))

(ert-deftest pyim-test-pyim-numbers> ()
  (should-not (pyim-numbers> '(1) '(3)))
  (should (pyim-numbers> '(4) '(3)))
  (should-not (pyim-numbers> '(1 2) '(3 4)))
  (should-not (pyim-numbers> '(1 2) '(1 2)))
  (should (pyim-numbers> '(2 2) '(1 1)))
  (should (pyim-numbers> '(2 2) '(1 3)))
  (should (pyim-numbers> '(2 2) '(1)))
  (should-not (pyim-numbers> '(2 2) '(3)))
  (should-not (pyim-numbers> '(2) '(3 1)))
  (should (pyim-numbers> '(2) '(1 3))))

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
                                   ("un" "ong")))
        (quanpin (pyim-scheme-get 'quanpin))
        (wubi (pyim-scheme-get 'wubi))
        (cangjie (pyim-scheme-get 'cangjie))
        (pyim-shuangpin (pyim-scheme-get 'pyim-shuangpin)))
    (should (equal (pyim-imobjs-create "nihao" quanpin)
                   '((("n" "i" "n" "i") ("h" "ao" "h" "ao")))))
    (should (equal (pyim-imobjs-create "nh" quanpin)
                   '((("n" "" "n" "") ("h" nil "h" nil)))))
    (should (equal (pyim-imobjs-create "xi'an" quanpin)
                   '((("x" "i" "x" "i") ("'" "an" "'" "an")))))
    (should (equal (pyim-imobjs-create "xian" quanpin)
                   '((("x" "ian" "x" "ian")))))
    (should (equal (pyim-imobjs-create "fenyun" quanpin)
                   '((("f" "en" "f" "en") ("y" "un" "y" "un"))
                     (("f" "en" "f" "en") ("y" "ong" "y" "un"))
                     (("f" "eng" "f" "en") ("y" "un" "y" "un"))
                     (("f" "eng" "f" "en") ("y" "ong" "y" "un")))))
    (should (equal (pyim-imobjs-create "xian" wubi)
                   '(("xian"))))
    (should (equal (pyim-imobjs-create "xian" cangjie)
                   '(("xian"))))
    (should (equal (pyim-imobjs-create "nihc" pyim-shuangpin)
                   '((("n" "i" "n" "i") ("h" "ao" "h" "c")))))))

;; ** pyim-codes 相关单元测试
(ert-deftest pyim-tests-pyim-codes ()
  (let ((quanpin (pyim-scheme-get 'quanpin))
        (wubi (pyim-scheme-get 'wubi))
        (cangjie (pyim-scheme-get 'cangjie))
        (pyim-shuangpin (pyim-scheme-get 'pyim-shuangpin)))
    (should (equal (pyim-codes-create
                    (car (pyim-imobjs-create "nihao" quanpin))
                    quanpin)
                   '("ni" "hao")))
    (should (equal (pyim-codes-create
                    (car (pyim-imobjs-create "aaaa" wubi))
                    wubi)
                   '("wubi/aaaa")))
    (should (equal (pyim-codes-create
                    (car (pyim-imobjs-create "aaaa" wubi))
                    wubi 2)
                   '("wubi/aa")))
    (should (equal (pyim-codes-create
                    (car (pyim-imobjs-create "aaaa" wubi))
                    wubi 1)
                   '("wubi/a")))
    (should (equal (pyim-codes-create
                    (car (pyim-imobjs-create "aaaa" cangjie))
                    cangjie)
                   '("cangjie/aaaa")))))

;; ** pyim-candidates 相关单元测试
(ert-deftest pyim-tests-pyim-candidates-get-chief ()
  (let ((wubi (pyim-scheme-get 'wubi))
        (personal-words1 '("呵呵" "天" "恭恭敬敬"))
        (personal-words2 '("呵呵" "恭恭敬敬"))
        (common-words1 '("工" "恭恭敬敬"))
        (common-words2 '("恭恭敬敬" "工")))

    ;; 形码输入法选择第一位词条的规则是：
    ;; 1. 如果从公共词库里面获取到的第一个词条是汉字，就选择它。
    ;; 2. 如果不是，就从个人词库里面按排列的先后顺序，获取一个汉字。
    (should (equal (pyim-candidates-get-chief wubi personal-words1 common-words1)
                   "工"))
    (should (equal (pyim-candidates-get-chief wubi personal-words1 common-words2)
                   "天"))
    (should-not (pyim-candidates-get-chief wubi personal-words2 common-words2)))

  (let ((quanpin (pyim-scheme-get 'quanpin))
        (pyim-dhashcache-iword2count-recent-10-words
         (read "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:all-words (\"就\" \"不是\" \"如果\" \"是\" \"规则\" \"的\" \"词条\" \"第一位\" \"选择\" \"输入法\") \"如果\" 1 \"的\" 1 \"就\" 1 \"输入法\" 2 \"词条\" 1 \"不是\" 1 \"选择\" 1 \"第一位\" 1 \"规则\" 1 \"是\" 1))"))
        (pyim-dhashcache-iword2count-recent-50-words
         (read "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:all-words (\"就\" \"不是\" \"如果\" \"是\" \"规则\" \"的\" \"词条\" \"第一位\" \"选择\" \"输入法\" \"形码\" \"天\" \"网\" \"呵呵\" \"工\" \"你\" \"天空\" \"蓝天\" \"大地\" \"不好\" \"我\" \"你好\") \"你好\" 1 \"你\" 3 \"我\" 1 \"不好\" 1 \"天空\" 2 \"天\" 3 \"大地\" 1 \"蓝天\" 4 \"工\" 2 \"呵呵\" 1 \"网\" 1 \"形码\" 1 \"输入法\" 1 \"选择\" 1 \"第一位\" 1 \"词条\" 1 \"的\" 1 \"规则\" 1 \"是\" 1 \"如果\" 1 \"不是\" 1 \"就\" 1))"))
        (personal-words1 '("输入罚" "输入法"))
        (personal-words2 '("蓝田" "蓝天"))
        (personal-words3 '("美丽" "魅力")))

    ;; 1. 最近输入的10个不同的词中出现一次以上。
    (should (equal (pyim-candidates-get-chief quanpin personal-words1 nil)
                   "输入法"))
    ;; 2. 最近输入的50个不同的词中出现过三次以上。
    (should (equal (pyim-candidates-get-chief quanpin personal-words2 nil)
                   "蓝天"))
    ;; 3. 个人词条中的第一个词。
    (should (equal (pyim-candidates-get-chief quanpin personal-words3 nil)
                   "美丽"))))

(ert-deftest pyim-tests-pyim-candidates-create-xingma ()
  (let ((wubi (pyim-scheme-get 'wubi))
        (pyim-dhashcache-icode2word
         (read "#s(hash-table size 1642 test equal rehash-size 1.5 rehash-threshold 0.8125 data (\"wubi/aaaa\" (\"工\" \"㠭\") \"wubi/bbbb\" (\"子\" \"子子孙孙孙孙\") \"wubi/cccc\" (\"又\" \"叕\")))"))
        (pyim-dhashcache-code2word
         (read "#s(hash-table size 1642 test equal rehash-size 1.5 rehash-threshold 0.8125 data (\"wubi/aaaa\" (\"㠭\") \"wubi/bbbb\" (\"子子孙孙\" \"子\") \"wubi/cccc\" (\"叕\" \"又\")))")))
    (should (equal (pyim-candidates-create
                    (pyim-imobjs-create "aaaa" wubi)
                    wubi)
                   '("㠭" "工")))
    (should (equal (pyim-candidates-create
                    (pyim-imobjs-create "bbbb" wubi)
                    wubi)
                   '("子" "子子孙孙孙孙" "子子孙孙")))
    (should (equal (pyim-candidates-create
                    (pyim-imobjs-create "cccc" wubi)
                    wubi)
                   '("叕" "又")))
    (should (equal (pyim-candidates-create
                    (pyim-imobjs-create "aaaabbbb" wubi)
                    wubi)
                   '("㠭子" "㠭子子孙孙孙孙" "㠭子子孙孙")))
    (should (equal
             (pyim-candidates-create
              (pyim-imobjs-create "aaaabbbbcccc" wubi)
              wubi)
             '("㠭子叕" "㠭子又")))))

(ert-deftest pyim-tests-pyim-candidates-znabc-words ()
  (let* ((pyim-dhashcache-code2word (make-hash-table :test #'equal))
         (pyim-dhashcache-icode2word (make-hash-table :test #'equal))
         (quanpin (pyim-scheme-get 'quanpin))
         (imobjs (pyim-imobjs-create "nihaomapengyou" quanpin)))
    (puthash "ni-hao" (list "你好" "尼耗") pyim-dhashcache-code2word)
    (puthash "ni-hao-ma" (list "你好吗" "你好马") pyim-dhashcache-code2word)
    (puthash "ni-hao-ma-peng-you" (list "你好吗朋友" "你好吗喷油") pyim-dhashcache-code2word)
    (should (equal (pyim-candidates-znabc-words imobjs quanpin)
                   '("你好吗朋友" "你好吗" "你好" "你好吗喷油" "你好马" "尼耗")))
    (should (equal (pyim-candidates-znabc-words imobjs quanpin t)
                   '("你好吗朋友" "你好吗" "你好")))))

(ert-deftest pyim-tests-pyim-candidates-jianpin-words ()
  (let* ((pyim-dhashcache-code2word (make-hash-table :test #'equal))
         (pyim-dhashcache-icode2word (make-hash-table :test #'equal))
         (pyim-dhashcache-ishortcode2word (make-hash-table :test #'equal))
         (quanpin (pyim-scheme-get 'quanpin))
         (imobjs1 (pyim-imobjs-create "nih" quanpin))
         (imobjs2 (pyim-imobjs-create "ni" quanpin)))
    (puthash "n-h" (list "你好" "你坏" "尼耗" "南好" "内核" "内河") pyim-dhashcache-ishortcode2word)
    (should (equal (pyim-candidates-jianpin-words imobjs1 quanpin)
                   '("你好" "你坏" "尼耗")))
    (should-not (pyim-candidates-jianpin-words imobjs2 quanpin))))

(ert-deftest pyim-tests-pyim-candidates-get-dcache-words ()
  (let* ((pyim-dhashcache-code2word (make-hash-table :test #'equal))
         (pyim-dhashcache-icode2word (make-hash-table :test #'equal))
         (pyim-dhashcache-shortcode2word (make-hash-table :test #'equal))
         (pyim-dhashcache-ishortcode2word (make-hash-table :test #'equal))
         (quanpin (pyim-scheme-get 'quanpin))
         (imobjs1 (pyim-imobjs-create "n" quanpin))
         (imobjs2 (pyim-imobjs-create "ni" quanpin))
         (imobjs3 (pyim-imobjs-create "ni-hao" quanpin)))
    (puthash "n" (list "你" "您" "妮") pyim-dhashcache-ishortcode2word)
    (puthash "ni" (list "你" "尼") pyim-dhashcache-icode2word)
    (puthash "ni" (list "尼" "你") pyim-dhashcache-code2word)
    (puthash "ni-hao" (list "你好" "尼耗" "呢耗") pyim-dhashcache-icode2word)
    (puthash "ni-hao" (list "你好" "尼耗") pyim-dhashcache-code2word)
    (puthash "n-h" (list "你好" "你坏" "尼耗" "南好" "内核" "内河") pyim-dhashcache-ishortcode2word)
    (should (equal (pyim-candidates-dcache-quanpin-words imobjs1 quanpin nil 10)
                   '(("你" "您" "妮") nil nil ("南" "乃" "囊" "脑" "呢" "内" "嫩" "能" "你" "年"))))
    (should (equal (pyim-candidates-dcache-quanpin-words imobjs2 quanpin nil 10)
                   '(("你" "尼") ("尼" "你") ("你" "尼" "尼" "你") nil)))
    (should (equal (pyim-candidates-dcache-quanpin-words imobjs3 quanpin nil 10)
                   '(("你好" "尼耗" "呢耗") ("你好" "尼耗") ("你好" "尼耗" "呢耗" "你好" "尼耗") nil)))))

(ert-deftest pyim-tests-pyim-candidates-quanpin-personal-words ()
  (let* ((pyim-dhashcache-icode2word (make-hash-table :test #'equal))
         (pyim-dhashcache-ishortcode2word (make-hash-table :test #'equal))
         (quanpin (pyim-scheme-get 'quanpin))
         (imobjs1 (pyim-imobjs-create "n" quanpin))
         (imobjs2 (pyim-imobjs-create "ni" quanpin))
         (imobjs3 (pyim-imobjs-create "nh" quanpin)))
    (puthash "n" (list "你" "您" "妮") pyim-dhashcache-ishortcode2word)
    (puthash "ni" (list "你" "尼") pyim-dhashcache-icode2word)
    (puthash "n-h" (list "呢耗") pyim-dhashcache-icode2word)
    (puthash "n-h" (list "你好" "你坏" "尼耗") pyim-dhashcache-ishortcode2word)
    (should (equal (pyim-candidates-quanpin-personal-words (car imobjs1) quanpin)
                   '("你" "您" "妮")))
    (should (equal (pyim-candidates-quanpin-personal-words (car imobjs2) quanpin)
                   '("你" "尼")))
    (should (equal (pyim-candidates-quanpin-personal-words (car imobjs3) quanpin)
                   '("呢耗" "你好" "你坏" "尼耗")))))

(ert-deftest pyim-tests-pyim-candidates-quanpin-common-words ()
  (let* ((pyim-dhashcache-code2word (make-hash-table :test #'equal))
         (pyim-dhashcache-shortcode2word (make-hash-table :test #'equal))
         (quanpin (pyim-scheme-get 'quanpin))
         (imobjs1 (pyim-imobjs-create "n" quanpin))
         (imobjs2 (pyim-imobjs-create "ni" quanpin))
         (imobjs3 (pyim-imobjs-create "nh" quanpin)))
    (puthash "n" (list "你" "您" "妮") pyim-dhashcache-shortcode2word)
    (puthash "ni" (list "你" "尼") pyim-dhashcache-code2word)
    (puthash "n-h" (list "呢耗") pyim-dhashcache-code2word)
    (puthash "n-h" (list "你好" "你坏" "尼耗") pyim-dhashcache-shortcode2word)
    (should (equal (pyim-candidates-quanpin-common-words (car imobjs1) quanpin)
                   '("你" "您" "妮")))
    (should (equal (pyim-candidates-quanpin-common-words (car imobjs2) quanpin)
                   '("你" "尼")))
    (should (equal (pyim-candidates-quanpin-common-words (car imobjs3) quanpin)
                   '("呢耗" "你好" "你坏" "尼耗")))))

(ert-deftest pyim-tests-pyim-candidates-quanpin-chars ()
  (let* ((pyim-dhashcache-icode2word (make-hash-table :test #'equal))
         (pyim-dhashcache-code2word (make-hash-table :test #'equal))
         (quanpin (pyim-scheme-get 'quanpin))
         (imobjs (pyim-imobjs-create "nihao" quanpin)))
    (puthash "ni" (list "你" "呢") pyim-dhashcache-icode2word)
    (puthash "ni" (list "你" "尼") pyim-dhashcache-code2word)
    (should (equal (pyim-candidates-quanpin-chars (car imobjs) quanpin)
                   '("你" "呢" "你" "尼")))))

(ert-deftest pyim-tests-pyim-candidates-pymap-chars ()
  (should (equal (pyim-candidates-pymap-chars "ni" 10)
                 '("你" "年" "娘" "鸟" "摄" "您" "宁" "牛" "尼" "念"))))

(ert-deftest pyim-tests-pyim-candidates-search-buffer ()
  (with-temp-buffer
    (insert "你好你好你坏你坏你话牛蛤牛和牛蛤牛蛤牛蛤牛蛤牛蛤")
    (should (equal (pyim-candidates-search-buffer (pyim-cregexp-build "nh" 3 t))
                   '("牛蛤" "你坏" "你好" "牛和" "你话")))
    (let ((words (pyim-candidates-search-buffer (pyim-cregexp-build "nh" 3 t))))
      (should (equal (get-text-property 0 :comment (car words)) "(buf)")))))

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
  (let ((pyim-dhashcache-word2code (make-hash-table :test #'equal))
        (wubi (pyim-scheme-get 'wubi))
        (cangjie (pyim-scheme-get 'cangjie)))
    (puthash "工" (list "wubi/aaaa" "cangjie/mlm" "gong") pyim-dhashcache-word2code)
    (puthash "房" (list "wubi/yny") pyim-dhashcache-word2code)
    (puthash "丛" (list "wubi/wwg") pyim-dhashcache-word2code)
    (should (equal (pyim-cstring-to-xingma "工" cangjie) "mlm"))
    (should-not (pyim-cstring-to-xingma "工房" cangjie))
    (should (equal (pyim-cstring-to-xingma "工" wubi) "aaaa"))
    (should (equal (pyim-cstring-to-xingma "工房" wubi) "aayn"))
    (should (equal (pyim-cstring-to-xingma "工房丛" wubi) "ayww"))
    (should (equal (pyim-cstring-to-xingma "工房丛房" wubi) "aywy"))
    (should (equal (pyim-cstring-to-xingma "工" wubi t) '("aaaa")))
    (should (equal (pyim-cstring-to-xingma "工房" wubi t) '("aayn")))))

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
                     '(("天安门" 3 0) ("安门" 2 0))))

      ;; <I>天安门abc
      (goto-char (point-min))
      (should (equal (pyim-cstring-words-at-point t) nil)))))

(ert-deftest pyim-tests-pyim-cstring-forward-or-backward-word ()
  (with-temp-buffer
    (let ((pyim-dhashcache-code2word (make-hash-table :test #'equal)))
      (puthash "ha-ha" (list "哈哈") pyim-dhashcache-code2word)
      (puthash "wo-ai" (list "我爱") pyim-dhashcache-code2word)
      (puthash "bei-jing" (list "北京") pyim-dhashcache-code2word)
      (puthash "tian-an-men" (list "天安门") pyim-dhashcache-code2word)
      (insert "哈哈我爱北京天安门")
      (goto-char 2)
      (pyim-cstring-forward-word 1)
      (should (equal (buffer-substring (point-min) (point)) "哈哈"))
      (pyim-cstring-forward-word 3)
      (should (equal (buffer-substring (point-min) (point)) "哈哈我爱北京天安门"))
      (pyim-cstring-backward-word 1)
      (should (equal (buffer-substring (point-min) (point)) "哈哈我爱北京"))
      (pyim-cstring-backward-word 2)
      (should (equal (buffer-substring (point-min) (point)) "哈哈")))))

;; ** pyim-cregexp 相关单元测试
(ert-deftest pyim-tests-pyim-cregexp ()
  (let ((wubi (pyim-scheme-get 'wubi))
        (pyim-default-scheme 'quanpin)
        (pyim-cregexp-fallback-scheme 'wubi))
    (should (equal (pyim-scheme-name (pyim-cregexp-scheme)) 'quanpin))
    (should (equal (pyim-scheme-name (pyim-cregexp-scheme wubi)) 'wubi)))

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
         (quanpin (pyim-scheme-get 'quanpin))
         (regexp1 (pyim-cregexp-create-1 "wang" quanpin 3 nil))
         (regexp2 (pyim-cregexp-create-1 "wang" quanpin 2)))
    (should (string-match-p regexp1 str))
    (should-not (string-match-p regexp2 str)))

  (let* ((quanpin (pyim-scheme-get 'quanpin))
         (imobj '(("d" "a" "d" "a") ("w" "ang" "w" "ang")))
         (regexp1 (pyim-cregexp-create-from-imobj imobj quanpin))
         (regexp2 (pyim-cregexp-create-from-imobj imobj quanpin nil nil t)))
    (should (string-match-p regexp1 "大王"))
    (should (string-match-p regexp1 "当王"))
    (should (string-match-p regexp2 "大王"))
    (should-not (string-match-p regexp2 "当王")))

  (let ((pyim-default-scheme 'wubi)
        (wubi (pyim-scheme-get 'wubi))
        (pyim-dhashcache-code2word (make-hash-table :test #'equal)))
    (puthash "wubi/aaaa" (list "工" "恭恭敬敬") pyim-dhashcache-code2word)
    (puthash "wubi/adww" (list "欺" "蒙古人" "其人" "欺人" "斯人" "惹人" "匧" "歁" "莢") pyim-dhashcache-code2word)
    (should (equal (pyim-cregexp-build "aaaa") "\\(?:aaaa\\|[工恭]恭?敬?敬?\\)"))
    (should (equal (pyim-cregexp-build "adww") "\\(?:adww\\|[其匧惹斯欺歁莢蒙][人古]?人?\\)"))
    (should (equal (pyim-cregexp-build "aaaa'aaaa")
                   "\\(?:\\(?:aaaa'\\|aaaa\\|[工恭]恭?敬?敬?\\)\\(?:aaaa\\|[工恭]恭?敬?敬?\\)\\)"))
    (should (equal (pyim-cregexp-create-1 "aaaa'aaaa" wubi)
                   "\\(?:aaaa'\\|aaaa\\|[工恭][恭]?[敬]?[敬]?\\)\\(?:aaaa\\|[工恭][恭]?[敬]?[敬]?\\)")))

  (with-temp-buffer
    (insert "haha nihao")
    (let ((pyim-cregexp-convert-at-point-function
           (lambda (cregexp)
             (concat "XXX: " cregexp)))
          (string "\\(?:nihao\\|[㒟㖏㖕㖖㖻㘈㘝㘨㘿㙞㚔㜤㜦㜵㜸㝕㞋㞙㞾㟧㠜㠡㡪㣇㣷㤛㥾㦐㧱㩘㩶㪒㭤㮆㮏㮞㮟㲡㲰㲻㲽㳮㴪㵫㸎㹸㺲㼭㽱㿦䀑䀔䁥䂇䂼䃵䄒䄭䄹䆨䇣䋴䋻䌜䌰䍲䏔䐁䒜䔭䕥䖆䗿䘌䘦䘽䙚䚓䚾䛏䛘䜆䜓䝚䞕䟢䡾䤔䦊䦵䧇䧔䩞䬯䭃䭢䭲䮍䮗䮘䯀䯅䯵䰯䳖䴴䵑䵒乜伱伲佞你侫倪儗儜儞兒凝匿卄呢咛唸啮喦嗫噛嚀嚙囁囐囓囜圼坭埝埿堄妞妮妳姩娘婗嫋嫟嬝嬢嬣嬲嬺孃孨孴孼孽宁寍寕寗寜寧尼尿屔屰峊嵲嶭巕帇年廿念忸怓怩恁您惄惗愵慝懝扭抐抝抳拈拗拟拧拰捏捻掜揑摂摄摰撚撵擜擬擰攆敜旎昵晲暱杻枿柅柠棿榐槷樢橣檷檸櫱氼氽汼沑泞泥涅涊淣淰湼溓溺濘炄牛牜狃狋狔狞猊獰甯疌疒痆眤睨矃碾祢禰秊秜秥篞簐籋籾粘糱糵紐縌纽聂聍聶聹聻胒脲腝腻膩臡臬臲艌苧苨苶茑茮莥菍蔦蔫薴薿蘖蘗蚭蚴蛪蜺蠥衂衵袅裊褭褹觬誽諗譺讘貎跈跜踂踗蹍蹑蹨躎躡輗輦輾辇辗迡逆郳酿醸釀鈕鈢鈮鉨鉩銸鋷錜鎳鑈鑏鑷钀钮铌镊镍闑陧隉隬霓靵顳颞馜鬡鬤鮎鯓鯢鯰鲇鲵鲶鳥鶂鷊鸋鸟鸮鹝鹢麑黏鼰齞齧齯][㕺㘪㙱㚪㝀㞻㠙㩝㬔㬶㵆䒵䚽䝞䝥䧚䧫䪽䬉䯫侾傐儫勂号呺哠嗥嘷噑嚆嚎壕好峼恏悎昊昦晧暠暤暭曍椃毫浩淏滈澔濠瀥灏灝狢獆獋獔皓皜皞皡皥秏竓籇耗聕茠蒿薃薅薧藃號虠蚝蠔諕譹诐豪貉郝鄗鎬镐顥颢鰝鶴鸮]\\)"))
      (pyim-cregexp-convert-at-point)
      (should (equal (buffer-string) (concat "haha XXX: " string)))))

  (with-temp-buffer
    (insert "hinihao")
    (set-mark 3)
    (let ((pyim-cregexp-convert-at-point-function
           (lambda (cregexp)
             (concat "XXX: " cregexp)))
          (string "\\(?:nihao\\|[㒟㖏㖕㖖㖻㘈㘝㘨㘿㙞㚔㜤㜦㜵㜸㝕㞋㞙㞾㟧㠜㠡㡪㣇㣷㤛㥾㦐㧱㩘㩶㪒㭤㮆㮏㮞㮟㲡㲰㲻㲽㳮㴪㵫㸎㹸㺲㼭㽱㿦䀑䀔䁥䂇䂼䃵䄒䄭䄹䆨䇣䋴䋻䌜䌰䍲䏔䐁䒜䔭䕥䖆䗿䘌䘦䘽䙚䚓䚾䛏䛘䜆䜓䝚䞕䟢䡾䤔䦊䦵䧇䧔䩞䬯䭃䭢䭲䮍䮗䮘䯀䯅䯵䰯䳖䴴䵑䵒乜伱伲佞你侫倪儗儜儞兒凝匿卄呢咛唸啮喦嗫噛嚀嚙囁囐囓囜圼坭埝埿堄妞妮妳姩娘婗嫋嫟嬝嬢嬣嬲嬺孃孨孴孼孽宁寍寕寗寜寧尼尿屔屰峊嵲嶭巕帇年廿念忸怓怩恁您惄惗愵慝懝扭抐抝抳拈拗拟拧拰捏捻掜揑摂摄摰撚撵擜擬擰攆敜旎昵晲暱杻枿柅柠棿榐槷樢橣檷檸櫱氼氽汼沑泞泥涅涊淣淰湼溓溺濘炄牛牜狃狋狔狞猊獰甯疌疒痆眤睨矃碾祢禰秊秜秥篞簐籋籾粘糱糵紐縌纽聂聍聶聹聻胒脲腝腻膩臡臬臲艌苧苨苶茑茮莥菍蔦蔫薴薿蘖蘗蚭蚴蛪蜺蠥衂衵袅裊褭褹觬誽諗譺讘貎跈跜踂踗蹍蹑蹨躎躡輗輦輾辇辗迡逆郳酿醸釀鈕鈢鈮鉨鉩銸鋷錜鎳鑈鑏鑷钀钮铌镊镍闑陧隉隬霓靵顳颞馜鬡鬤鮎鯓鯢鯰鲇鲵鲶鳥鶂鷊鸋鸟鸮鹝鹢麑黏鼰齞齧齯][㕺㘪㙱㚪㝀㞻㠙㩝㬔㬶㵆䒵䚽䝞䝥䧚䧫䪽䬉䯫侾傐儫勂号呺哠嗥嘷噑嚆嚎壕好峼恏悎昊昦晧暠暤暭曍椃毫浩淏滈澔濠瀥灏灝狢獆獋獔皓皜皞皡皥秏竓籇耗聕茠蒿薃薅薧藃號虠蚝蠔諕譹诐豪貉郝鄗鎬镐顥颢鰝鶴鸮]\\)"))
      (pyim-cregexp-convert-at-point)
      (should (equal (buffer-string) (concat "hiXXX: " string))))))

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
(ert-deftest pyim-tests-pyim-dcache-backend ()
  (let ((pyim-dcache-backend 'pyim-dregcache)
        (pyim-default-scheme 'quanpin))
    (should (eq (pyim-dcache-backend) 'pyim-dregcache)))

  (let ((pyim-dcache-backend 'pyim-dregcache)
        (pyim-default-scheme 'wubi))
    (should (eq (pyim-dcache-backend) 'pyim-dhashcache)))

  (let ((pyim-dcache-backend 'pyim-dhashcache1))
    (should (eq (pyim-dcache-backend) 'pyim-dhashcache))))

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
  (let ((pyim-dcache-backend 'pyim-dhashcache)
        (pyim-dhashcache-iword2count (make-hash-table :test #'equal))
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

;; ** pyim-dict 相关单元测试
(ert-deftest pyim-tests-pyim-dict ()
  (let ((pyim-extra-dicts nil)
        (dict1 '(:name "test1" :file "/home/user/test1.pyim"))
        (dict2 '(:name "test2" :file "/home/user/test2.pyim"))
        (dict3 '(:name "test1" :file "/home/user/test3.pyim"))
        (dict4 '(:name "test4" :file "/home/user/test4.pyim" :disable t)))
    (pyim-extra-dicts-add-dict dict1)
    (should (equal pyim-extra-dicts `(,dict1)))
    (pyim-extra-dicts-add-dict dict2)
    (should (equal pyim-extra-dicts `(,dict1 ,dict2)))
    (pyim-extra-dicts-add-dict dict3)
    (should (equal pyim-extra-dicts `(,dict3 ,dict2)))
    (pyim-extra-dicts-add-dict dict4)
    (should (equal (pyim-dict-get-enabled-dict-files)
                   '("/home/user/test3.pyim" "/home/user/test2.pyim")))))

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
pinyin/a 阿 啊 呵 腌 吖 嗄 锕 錒
a-a 啊啊
wo 我
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
    (should (equal (gethash "啊" output2) '("pinyin/a")))
    (should (equal (gethash "我" output2) nil))
    (should (equal (gethash "啊啊" output2) nil))))

(ert-deftest pyim-tests-pyim-dhashcache-update-shortcode2word ()
  (let ((pyim-dhashcache-iword2count (make-hash-table :test #'equal))
        (code2word (make-hash-table :test #'equal))
        (shortcode2word (make-hash-table :test #'equal))
        output)

    (puthash "wubi/a" '("戈") code2word)
    (puthash "wubi/aa" '("式" "藏") code2word)
    (puthash "wubi/aaa" '("工") code2word)
    (puthash "wubi/aaaa" '("工" "㠭") code2word)
    (puthash "wubi/aaab" '("㐂") code2word)
    (puthash "wubi/aaae" '("𧝣") code2word)

    (setq shortcode2word
          (pyim-dhashcache-update-shortcode2word-1 code2word))

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
  (let ((pyim-dhashcache-iword2count (make-hash-table :test #'equal))
        (icode2word (make-hash-table :test #'equal))
        ishortcode2word)

    (puthash "ni" '("你" "呢") icode2word)
    (puthash "ni-hao" '("你好" "呢耗") icode2word)
    (puthash "ni-huai" '("你坏") icode2word)

    (setq ishortcode2word
          (pyim-dhashcache-update-ishortcode2word-1 icode2word))

    (should (equal (gethash "n-h" ishortcode2word)
                   '("你好" "呢耗" "你坏")))
    (should (equal (gethash "n" ishortcode2word)
                   '("你" "呢")))))

(ert-deftest pyim-tests-pyim-dhashcache-put/delete ()
  (let ((pyim-dcache-backend 'pyim-dhashcache)
        (pyim-dhashcache-icode2word (make-hash-table :test #'equal)))
    (puthash "ni-hao" '("你好" "呢耗") pyim-dhashcache-icode2word)
    (pyim-dhashcache-put
      pyim-dhashcache-icode2word "ni-hao"
      (cons "呢毫" orig-value))
    (pyim-dhashcache-put
      pyim-dhashcache-icode2word "ni-bu-hao"
      (list "你不好"))
    (should (equal (gethash "ni-hao" pyim-dhashcache-icode2word) '("呢毫" "你好" "呢耗")))
    (should (equal (gethash "ni-bu-hao" pyim-dhashcache-icode2word) '("你不好")))
    (pyim-dcache-delete-word "你不好")
    (should (equal (gethash "ni-bu-hao" pyim-dhashcache-icode2word) nil))
    (pyim-dcache-delete-word "你好")
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
  (let ((pyim-dcache-backend 'pyim-dhashcache)
        (pyim-dhashcache-code2word (make-hash-table :test #'equal))
        (pyim-dhashcache-icode2word (make-hash-table :test #'equal))
        (pyim-dhashcache-iword2count (make-hash-table :test #'equal)))

    (puthash "ni-hao" '("呢耗") pyim-dhashcache-icode2word)
    (puthash "ni-hao" '("你好") pyim-dhashcache-code2word)
    (puthash "你好" 10 pyim-dhashcache-iword2count)

    (should (equal (pyim-dcache-get "ni-hao" '(code2word)) '("你好")))
    (should (equal (pyim-dcache-get "ni-hao" '(icode2word)) '("呢耗")))
    (should (equal (pyim-dcache-get "你好" '(iword2count)) '(10)))
    (should (equal (pyim-dcache-get "ni-hao" '(code2word icode2word)) '("你好" "呢耗")))
    (should (equal (pyim-dcache-get "ni-hao") '("呢耗" "你好")))))

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

(ert-deftest pyim-tests-pyim-dcache-sort-words ()
  (let ((pyim-dcache-backend 'pyim-dhashcache)
        (pyim-dhashcache-iword2count (make-hash-table :test #'equal))
        words)
    (puthash "你好" 3 pyim-dhashcache-iword2count)
    (puthash "呢耗" 2 pyim-dhashcache-iword2count)
    (puthash "你豪" 1 pyim-dhashcache-iword2count)

    (setq words (list "呢耗" "你豪" "你好"))
    (should (equal (pyim-dcache-sort-words words)
                   '("你好" "呢耗" "你豪")))))

(ert-deftest pyim-tests-pyim-dhashcache-get-counts-from-log ()
  (should (member (pyim-dhashcache-get-counts-from-log
                   '((day :20220107 10
                          :20220106 6
                          :20220104 3
                          :20220103 3))
                   ;; (date-to-time "2022-01-07")
                   '(25047 4608))
                  '(((day 6 0 3 3 0 0 0)) ;Fixme: In github-ci will result this value, why?
                    ((day 10 6 0 3 3 0 0))))))

(ert-deftest pyim-tests-pyim-dhashcache-calculate-priority ()
  (should (equal (pyim-dhashcache-calculate-priority
                  '((day 3 7 6 4 5 9 1)))
                 '(69))))

(ert-deftest pyim-tests-pyim-dhashcache-upgrade-icode2word ()
  (should (equal (pyim-dhashcache-upgrade-icode2word-rulers)
                 '(((".") . "wubi/") (("@") . "cangjie/"))))
  (let ((pyim-dhashcache-icode2word (make-hash-table :test #'equal)))
    (puthash ".aaaa" '("工") pyim-dhashcache-icode2word)
    (puthash "wubi/aaaa" '("㠭" "工") pyim-dhashcache-icode2word)
    (pyim-dhashcache-upgrade-icode2word t)
    (should-not (gethash ".aaaa" pyim-dhashcache-icode2word))
    (should (equal (gethash "wubi/aaaa" pyim-dhashcache-icode2word)
                   '("㠭" "工"))))
  (let ((pyim-dhashcache-icode2word (make-hash-table :test #'equal)))
    (puthash ".aaaa" '("工") pyim-dhashcache-icode2word)
    (puthash "wubi/aaaa" '("㠭" "工") pyim-dhashcache-icode2word)
    (pyim-dhashcache-upgrade-icode2word)
    (should (equal (gethash ".aaaa" pyim-dhashcache-icode2word)
                   '("工")))
    (should (equal (gethash "wubi/aaaa" pyim-dhashcache-icode2word)
                   '("㠭" "工")))))

;; ** pyim-dregcache 相关单元测试
(ert-deftest pyim-tests-pyim-general ()
  (let ((pyim-dcache-backend 'pyim-dregcache))
    (with-temp-buffer
      (should (not toggle-input-method-active))
      (call-interactively #'toggle-input-method))))

(ert-deftest pyim-tests-pyim-dregcache-backend ()
  (let ((pyim-dcache-backend 'pyim-dregcache)
        words
        file-info
        content)
    (setq pyim-dregcache-cache nil)

    ;; load dictionary
    (pyim-dcache-init-variables)
    (pyim-dcache-update t)
    ;; cache is filled
    (should (> (length pyim-dregcache-cache) 0))
    ;; get first dictionary cache
    (setq file-info (lax-plist-get pyim-dregcache-cache
                                   (car (pyim-dregcache-all-dict-files))))

    (setq content (plist-get file-info :content))
    (let ((i 0)
          (chars "abcdefghjklmnopqrstwxyz"))
      (should (eq (length content) (length chars)))
      (while (< i (length chars))
        (should (eq (elt chars i) (elt (nth i content) 0)))
        (setq i (1+ i))))
    (should (string= (pyim-dregcache-get-content "ai" file-info)
                     (pyim-dregcache-get-content "a" file-info)))
    (should (string= (pyim-dregcache-get-content "ba" file-info)
                     (pyim-dregcache-get-content "b" file-info)))
    (should (string= (pyim-dregcache-get-content "ze" file-info)
                     (pyim-dregcache-get-content "z" file-info)))

    ;; test dregcache api
    (setq words (pyim-dcache-get "a"))
    (should (eq (length words) 8))
    (should (string= (nth 0 words) "阿"))

    (setq  words (pyim-dcache-get "za-cao"))
    (should (eq (length words) 1))
    (should (string= (nth 0 words) "杂草"))

    (setq  words (pyim-dcache-get "ba-shi-tian-huan-you-di-qiu"))
    (should (eq (length words) 1))
    (should (string= (nth 0 words) "八十天环游地球"))

    (setq words (pyim-dcache-get "zun-bei"))
    (should (eq (length words) 1))
    (should (string= (nth 0 words) "尊卑"))

    (setq words (pyim-dcache-get "zun"))
    (should (string= (nth 0 words) "尊"))

    (let* ((gc-cons-threshold most-positive-fixnum))
      (message "search by code \"zun-yi\" takes %s seconds" (benchmark-run-compiled 1 (pyim-dcache-get "zun-yi"))))

    ;; `pyim-dregcache-get' calls `pyim-pymap-py2cchar-get' before return result
    (should (eq (length words) 26))))

;; ** pyim-cloudim 相关单元测试
(ert-deftest pyim-tests-pyim-cloudim ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK
Content-Length: 88
Content-Type: text/plain; charset=utf-8
Date: Sun, 08 May 2022 00:56:13 GMT

{\"0\":[[[\"你好\",5,{\"pinyin\":\"ni'hao\",\"type\":\"IMEDICT\"}]]],\"1\":\"ni'hao\",\"result\":[null]}")
    (should (equal (pyim-cloudim-parse-baidu-buffer) '("你好")))
    (should (equal (get-text-property 0 :comment (car (pyim-cloudim-parse-baidu-buffer))) "(云)")))

  (with-temp-buffer
    (insert "HTTP/1.1 200 OK
Date: Sun, 08 May 2022 03:33:56 GMT
Pragma: no-cache
Expires: -1
Cache-Control: no-cache, must-revalidate
Cross-Origin-Resource-Policy: cross-origin
Content-Type: application/json; charset=UTF-8
X-Content-Type-Options: nosniff
Content-Disposition: attachment; filename=\"f.txt\"
Server: Google Input Server/1.0
X-XSS-Protection: 0
X-Frame-Options: SAMEORIGIN
Alt-Svc: h3=\":443\"; ma=2592000,h3-29=\":443\"; ma=2592000,h3-Q050=\":443\"; ma=2592000,h3-Q046=\":443\"; ma=2592000,h3-Q043=\":443\"; ma=2592000,quic=\":443\"; ma=2592000; v=\"46,43\"
Transfer-Encoding: chunked

[\"SUCCESS\",[[\"nihao\",[\"你好\"],[],{\"annotation\":[\"ni hao\"],\"candidate_type\":[0],\"lc\":[\"16 16\"]}]]]")
    (should (equal (pyim-cloudim-parse-google-buffer) '("你好")))
    (should (equal (get-text-property 0 :comment (car (pyim-cloudim-parse-google-buffer))) "(云)"))))

;; ** pyim-probe 相关单元测试
(ert-deftest pyim-tests-pyim-probe-program-mode ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; comment")
    (should-not (pyim-probe-program-mode))
    (insert "\n")
    (should (pyim-probe-program-mode))
    (insert "()")
    (backward-char 1)
    (should (pyim-probe-program-mode))
    (insert "setq test")
    (should (pyim-probe-program-mode))
    (insert " \"\"")
    (backward-char 1)
    (should-not (pyim-probe-program-mode))))

(ert-deftest pyim-tests-pyim-probe-isearch-mode ()
  ;; Isearch mode 不好写测试，这里假设 isearch 命令运行时，至少有一个 buffer 中
  ;; 变量 isearch-mode 取值为 t. 参考了 `isearch-define-mode-toggle'.
  (let ((pyim-isearch-mode t))
    (with-current-buffer (get-buffer-create "test")
      (setq-local isearch-mode t))
    (should (pyim-probe-isearch-mode))))

(ert-deftest pyim-tests-pyim-probe-org-speed-commands ()
  (when (> emacs-major-version 25)
    (with-temp-buffer
      (let ((org-use-speed-commands t))
        (org-mode)
        (insert "* heading")
        (goto-char (line-beginning-position))
        (should (pyim-probe-org-speed-commands))
        (forward-char 1)
        (should-not (pyim-probe-org-speed-commands))
        (forward-char 1)
        (should-not (pyim-probe-org-speed-commands))))))

(ert-deftest pyim-tests-pyim-probe-org-structure-template ()
  (with-temp-buffer
    (org-mode)
    (insert "<")
    (should (pyim-probe-org-structure-template))
    (insert "abcde")
    (should (pyim-probe-org-structure-template))
    (insert " ")
    (should-not (pyim-probe-org-structure-template))

    (erase-buffer)

    (insert "    <")
    (should (pyim-probe-org-structure-template))
    (insert "abcde")
    (should (pyim-probe-org-structure-template))
    (insert " ")
    (should-not (pyim-probe-org-structure-template))))

(ert-deftest pyim-tests-pyim-probe-dynamic-english ()
  (with-temp-buffer
    ;; 从光标往前找第一个非数字的字符，为其他字符时，输入下一个字符时默认开启英文输入
    (insert "english")
    (should (pyim-probe-dynamic-english))

    (insert "123")
    (should (pyim-probe-dynamic-english))

    (insert ",")
    (should (pyim-probe-dynamic-english))

    (insert "  ")
    (should (pyim-probe-dynamic-english))

    ;; 从光标往前找第一个非数字的字符，为中文字符时，输入下一个字符时默认开启中文输入
    (insert "中文")
    (should-not (pyim-probe-dynamic-english))))

(ert-deftest pyim-tests-pyim-probe-auto-english ()
  (with-temp-buffer
    ;; 1. 当前字符为英文字符（不包括空格）时，输入下一个字符为英文字符
    (insert "english")
    (should (pyim-probe-auto-english))
    ;; 当前字符为中文字符或输入字符为行首字符时，输入的字符为中文字符
    (insert "\n")
    (should-not (pyim-probe-auto-english))

    (insert "中文")
    (should-not (pyim-probe-auto-english))
    ;; 以单个空格为界，自动切换中文和英文字符

    (insert " ")
    (should (pyim-probe-auto-english))

    (insert "english ")
    (should-not (pyim-probe-auto-english))

    (insert "中文 ")
    (should (pyim-probe-auto-english))))

(ert-deftest pyim-tests-pyim-probe-evil-normal-mode ()
  (when (featurep 'evil)
    (with-temp-buffer
      (evil-local-mode)
      (should (pyim-probe-evil-normal-mode))
      (evil-local-mode -1)
      (should-not (pyim-probe-evil-normal-mode)))))

(ert-deftest pyim-tests-pyim-probe-punctuation-line-beginning ()
  (with-temp-buffer
    (should (pyim-probe-punctuation-line-beginning ?.))
    (insert "fff")
    (should-not (pyim-probe-punctuation-line-beginning ?.))
    (insert "\n")
    (should (pyim-probe-punctuation-line-beginning ?.))))

(ert-deftest pyim-tests-pyim-probe-punctuation-after-punctuation ()
  (with-temp-buffer
    (should-not (pyim-probe-punctuation-after-punctuation ?.))
    (insert "'")
    (should (pyim-probe-punctuation-after-punctuation ?.))
    (insert "abc")
    (should-not (pyim-probe-punctuation-after-punctuation ?.))
    (insert "123")
    (should-not (pyim-probe-punctuation-after-punctuation ?.))
    (insert "。")
    (should-not (pyim-probe-punctuation-after-punctuation ?.))
    (insert ".")
    (should (pyim-probe-punctuation-after-punctuation ?.))))

(ert-deftest pyim-tests-pyim-probe-org-latex-mode ()
  (when (> emacs-major-version 25)
    (with-temp-buffer
      (org-mode)
      (insert "\\begin{equation}")
      (save-excursion
        (insert "\\end{equation}"))
      (should (pyim-probe-org-latex-mode))

      (erase-buffer)
      (insert "$$")
      (backward-char 1)
      (should (pyim-probe-org-latex-mode))

      (erase-buffer)
      (insert "\\documentclass{article}")
      (should (pyim-probe-org-latex-mode))))

  (when (< emacs-major-version 26)
    (should-not (pyim-probe-org-latex-mode))))

(ert-deftest pyim-tests-pyim-probe-exwm-xim-environment ()
  (with-temp-buffer
    (setq-local exwm-xim-buffer-p t)
    (should (pyim-probe-exwm-xim-environment))
    (setq-local exwm-xim-buffer-p nil)
    (should-not (pyim-probe-exwm-xim-environment))))

(ert-deftest pyim-tests-pyim-probe-xwidget-webkit-environment ()
  ;; TODO
  )

;; ** pyim-probe 相关单元测试
(ert-deftest pyim-tests-pyim-page-get-page-style ()
  (let ((pyim-page-tooltip-style-alist
         '((minibuffer . minibuffer)))
        (pyim-page-style 'test))
    (should (equal (pyim-page-get-page-style 'minibuffer)
                   'minibuffer))
    (should (equal (pyim-page-get-page-style 'test)
                   'test))))

(ert-deftest pyim-tests-pyim-page-info-format ()
  (let ((page-info
         (list :scheme (pyim-scheme-get 'quanpin)
               :current-page 1
               :total-page 26
               :candidates '("你好" "尼耗" "您耗" "您好" "你")
               :position 3
               :hightlight-current 'hightlight-current
               :assistant-enable nil)))

    (should (equal (pyim-page-info-format 'two-lines page-info)
                   "=> | [1/26]: 
1.你好 2.尼耗 3[您耗]4.您好 5.你 "))

    (should (equal (pyim-page-info-format 'no-exist-style page-info)
                   "=> | [1/26]: 
1.你好 2.尼耗 3[您耗]4.您好 5.你 "))

    (should (equal (pyim-page-info-format 'one-line page-info)
                   "[|]: 1.你好 2.尼耗 3[您耗]4.您好 5.你 (1/26)"))
    (should (equal (pyim-page-info-format 'vertical page-info)
                   "=> | [1/26]: 
1.你好 
2.尼耗 
3[您耗]
4.您好 
5.你 "))
    (should (equal (pyim-page-info-format 'minibuffer page-info)
                   "[|              ]: 1.你好 2.尼耗 3[您耗]4.您好 5.你 (1/26) $ "))))

(ert-deftest pyim-tests-pyim-page-menu-create ()
  (should
   (equal (pyim-page-menu-create '("你好" "尼耗" "您耗" "您好" "你") 0 nil t)
          #("1.你好 2.尼耗 3.您耗 4.您好 5.你 " 13 17 (face pyim-page-selection))))
  (should
   (equal (pyim-page-menu-create '("你好" "尼耗" "您耗" "您好" "你") 1 nil t)
          #("1[你好]2.尼耗 3.您耗 4.您好 5.你 " 1 5 (face pyim-page-selection))))
  (should
   (equal (pyim-page-menu-create '("你好" "尼耗" "您耗" "您好" "你") 3 nil t)
          #("1.你好 2.尼耗 3[您耗]4.您好 5.你 " 11 15 (face pyim-page-selection)))))

;; ** pyim-preview 相关单元测试
(ert-deftest pyim-tests-pyim-preview-string ()
  (let ((pyim-process-candidates '("世界" "时节" "使节" "视界" ))
        (pyim-process-candidate-position 1)
        (pyim-outcome-history '("你好"))
        (pyim-process-imobjs '((("sh" "i" "sh" "i") ("j" "ie" "j" "ie"))))
        (scheme (pyim-scheme-get 'quanpin)))
    (should (equal (pyim-preview-string scheme)
                   "你好世界")))

  (let ((pyim-process-candidates '("世界" "时节" "使节" "视界" ))
        (pyim-process-candidate-position 2)
        (pyim-outcome-history nil)
        (pyim-process-imobjs '((("sh" "i" "sh" "i") ("j" "ie" "j" "ie"))))
        (scheme (pyim-scheme-get 'quanpin)))
    (should (equal (pyim-preview-string scheme)
                   "时节")))

  (let ((pyim-process-candidates '("这是" "蛰是" "这时" "真实" "这使" "这事" "这" "者" "着" "折" "哲" "浙" "遮"))
        (pyim-process-candidate-position 10)
        (pyim-outcome-history nil)
        (pyim-process-imobjs '((("zh" "e" "zh" "e") ("sh" "i" "sh" "i"))))
        (scheme (pyim-scheme-get 'quanpin)))
    (should (equal (pyim-preview-string scheme)
                   "折shi")))

  (let ((pyim-process-candidates '("工" "藏匿" "工工" "花花草草" "㠭"))
        (pyim-process-candidate-position 4)
        (pyim-outcome-history nil)
        (pyim-process-imobjs '(("aaaa")))
        (scheme (pyim-scheme-get 'wubi)))
    (should (equal (pyim-preview-string scheme)
                   "花花草草"))))

;; ** pyim-autoselecter 相关单元测试
(ert-deftest pyim-tests-pyim-autoselecter-xingma-1 ()
  (should-not (apply #'pyim-autoselector-xingma-1
                     '(4 "aaaa" ("工" "藏匿" "花花草草" "工工" "㠭") ("工"))))

  (should (equal (apply #'pyim-autoselector-xingma-1
                        '(4 "aaaab" ("工" "藏匿" "花花草草" "工工" "㠭") ("工")))
                 '(:select last)))

  ;; 自动清除错误输入模式，类似微软五笔：敲第五个字母的时候，前面四个字母自动清除
  (should (equal (apply #'pyim-autoselector-xingma-1
                        '(4 "xxxxa" ("工" "戈") ("xxxx")))
                 '(:select last :replace-with "")))

  (should (equal (apply #'pyim-autoselector-xingma-1
                        '(4 "aaaa" ("工") ("工")))
                 '(:select current))))

(ert-run-tests-batch-and-exit)

;; * Footer
;;; pyim-tests.el ends here
