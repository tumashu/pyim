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

(defun pyim-test-get-dicts ()
  "当前目录下的词库."
  (let* ((files (directory-files-recursively default-directory "\.pyim$")))
    (mapcar (lambda (f)
              (list :name (file-name-base f) :file f))
            files)))

(setq default-input-method "pyim")
(setq pyim-dicts (pyim-test-get-dicts))

(ert-deftest pyim-test-pyim-permutate-list ()
  (should (equal (pyim-permutate-list '((a b) (c d e) (f)))
                 '((a c f)
                   (a d f)
                   (a e f)
                   (b c f)
                   (b d f)
                   (b e f))))
  (should (equal (pyim-permutate-list nil) nil))
  (should (equal (pyim-permutate-list '((a))) '((a)))))

(ert-deftest pyim-test-pyim-zip ()
  (should (equal (pyim-zip '((a b "d") nil (1 d) (f) nil))
                 '(a 1 f b d "d")))
  (should (equal (pyim-zip nil) nil)))

(ert-deftest pyim-test-pyim-subconcat ()
  (should (equal (pyim-subconcat '("a" "b" "c" "d"))
                 '("abcd" "abc" "ab")))
  (should (equal (pyim-subconcat '("a" "b" "c" "d") "-")
                 '("a-b-c-d" "a-b-c" "a-b")))
  (should (equal (pyim-subconcat nil) nil)))

(ert-deftest pyim-test-pyim-cstring-partition ()
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

(ert-deftest pyim-test-pyim-cstring-substrings ()
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

(ert-deftest pyim-test-pyim-cstring-split ()
  (pyim-dcache-init-variables)
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

(ert-deftest pyim-test-pyim-cstring-to-pinyin ()
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
                   "yin-hang-hen-xing"))))

(ert-deftest pyim-test-pyim-general ()
  (let ((pyim-dcache-backend 'pyim-dregcache))
    (with-temp-buffer
      (should (not toggle-input-method-active))
      (call-interactively #'toggle-input-method))))

(ert-deftest pyim-test-dregcache-backend ()
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
    (should (eq (length words) 44))))

(ert-run-tests-batch-and-exit)
;; * Footer
;;; pyim-tests.el ends here
