;;; pyim-pinyin.el --- pinyin tools for pyim.        -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
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

;;; Code:
;; * 代码                                                           :code:
(require 'cl-lib)
(require 'pyim-pymap)

(defgroup pyim-pinyin nil
  "Pinyin libs for pyim."
  :group 'pyim)

(defcustom pyim-pinyin-fuzzy-alist
  '(("en" "eng")
    ("in" "ing")
    ("un" "ong"))
  "设定模糊音."
  :type '(alist :key-type string
                :value-type (list string)))

(defvar pyim-pinyin--shengmu
  '("b" "p" "m" "f" "d" "t" "n" "l" "g" "k" "h"
    "j" "q" "x" "z" "c" "s" "zh" "ch" "sh" "r" "y" "w"))

(defvar pyim-pinyin--yunmu
  '("a" "o" "e" "i" "u" "v" "ai" "ei" "ui" "ao" "ou" "iu"
    "ie" "ia" "ua" "ve" "er" "an" "en" "in" "un" "vn" "ang" "iong"
    "eng" "ing" "ong" "uan" "uang" "ian" "iang" "iao" "ue"
    "uai" "uo"))

(defvar pyim-pinyin--valid-yunmu
  '("a" "o" "e" "ai" "ei" "ui" "ao" "ou" "er" "an" "en"
    "ang" "eng"))

(defconst pyim-pinyin--shuangpin-invalid-pinyin-regexp
  (format "^\\(%s\\)$"
          (string-join
           '("[qtghkzcsdn]o"
             "[ypfbmw]uo"
             "[qj]ong"
             "[rtysdghklzcn]iong"
             "[qtypdjlxbnm]uai"
             "[ghk]ing?"
             "[qjlxn]uang"
             "[dgh]iang"
             "[qjlx]ua"
             "[hkg]ia"
             "[rtsdghkzc]v"
             "[jl]ui")
           "\\|"))
  "双拼可能自动产生的无效拼音. 例如输入 kk 得到有效拼音 kuai .
但同时产生了无效拼音 king .  用户手动输入的无效拼音无需考虑.
因为用户有即时界面反馈,不可能连续输入无效拼音.")

;; 分解拼音的相关函数
(defun pyim-pinyin-split (pinyin)
  "将一个代表拼音的字符串 PINYIN, 分解为声母韵母对组成的列表.

这个过程通过循环的调用 `pyim-pinyin--get-charpy' 来实现，整个过程
类似用菜刀切黄瓜片，将一个拼音字符串逐渐切开。"
  (let ((py pinyin)
        charpy spinyin)
    (while (when (string< "" pinyin)
             (setq charpy (pyim-pinyin--get-charpy pinyin))
             (if (and (equal (nth 0 (car charpy)) "")
                      (equal (nth 1 (car charpy)) ""))
                 (progn
                   (setq spinyin nil)
                   (setq pinyin ""))
               (setq spinyin (append spinyin (list (car charpy))))
               (setq pinyin (cdr charpy)))))
    (or spinyin
        ;; 如果无法按照拼音的规则来分解字符串，
        ;; 就将字符串简单的包装一下，然后返回。
        ;; 目前这个功能用于： 以u或者i开头的词库 #226
        ;; https://github.com/tumashu/pyim/issues/226
        (list (list "" py "" py)))))

(defun pyim-pinyin--get-charpy (pinyin)
  "将拼音字符串 PINYIN 分解成声母，韵母和剩余部分."
  (let* ((x (pyim-pinyin--get-shenmu pinyin))
         (shenmu (car x))
         (yunmu-and-rest (cdr x))
         (i (min (length yunmu-and-rest) 5))
         yunmu rest)
    (while (> i 0)
      (setq yunmu (substring yunmu-and-rest 0 i))
      (setq rest (substring yunmu-and-rest i))
      (if (member yunmu pyim-pinyin--yunmu)
          (cond (;; 如果声母和韵母组成的拼音不是一个有效的拼音，
                 ;; 就继续缩短，如果是，就进一步检测。
                 (not (pyim-pinyin--valid-charpy-p shenmu yunmu))
                 (setq i (1- i))
                 (setq yunmu ""))
                ((and (string< "" rest)
                      ;; 截取后剩余的字符串 rest 找不出声母
                      (equal (car (pyim-pinyin--get-shenmu rest)) "")
                      ;; 截取后的韵母最后一个字符是一个有效声母
                      (member (substring yunmu -1) pyim-pinyin--shengmu)
                      ;; 截取得到的韵母如果去掉最后一个字符，还是有效的韵母
                      (member (substring yunmu 0 -1) pyim-pinyin--yunmu))
                 (if (not (pyim-pinyin--valid-charpy-p shenmu (substring yunmu 0 -1)))
                     ;; 如果去掉韵母最后一个字符后，无法组成一个有效的拼音。
                     ;; 就不要缩短了。
                     (setq i 0)
                   (setq i (1- i))
                   (setq yunmu "")))
                (t (setq i 0)))
        (setq i (1- i))
        (setq yunmu "")))
    (cons (list shenmu yunmu shenmu yunmu)
          (substring yunmu-and-rest (length yunmu)))))

(defun pyim-pinyin--get-shenmu (pinyin)
  "从一个拼音字符串 PINYIN 中提出第一个声母。"
  (let ((i (min (length pinyin) 2))
        shenmu)
    (while (> i 0)
      (setq shenmu (substring pinyin 0 i))
      (if (member shenmu pyim-pinyin--shengmu)
          (setq i 0)
        (setq i (1- i))
        (setq shenmu "")))
    (cons shenmu
          (substring pinyin (length shenmu)))))

(defun pyim-pinyin--valid-charpy-p (shenmu yunmu)
  "测试由 SHENMU 和 YUNMU 组成的拼音，是否是一个有效的汉字拼音。
这个函数尊重 `pyim-pinyin-fuzzy-alist' 模糊音设置。"
  (cl-some
   (lambda (char-pinyin)
     (pyim-pymap-py2cchar-get char-pinyin t))
   (mapcar (lambda (x)
             (concat (nth 0 x) (nth 1 x)))
           (pyim-pinyin-find-fuzzy
            (list shenmu yunmu shenmu yunmu)))))

(defun pyim-pinyin-find-fuzzy (info)
  "Find all fuzzy pinyins, INFO is (shenmu yunmu shenmu yunmu).

 (\"f\" \"en\" \"f\" \"en\") -> ((\"f\" \"en\" \"f\" \"en\")
                                 (\"f\" \"eng\" \"f\" \"en\"))"
  (let* ((fuzzy-alist (copy-alist pyim-pinyin-fuzzy-alist))
         (sm (nth 0 info))
         (ym (nth 1 info))
         (sm-list (cl-find-if (lambda (x)
                                (member sm x))
                              fuzzy-alist))
         (ym-list (cl-find-if (lambda (x)
                                (member ym x))
                              fuzzy-alist))
         result)
    (dolist (a (delete-dups (cons sm sm-list)))
      (dolist (b (delete-dups (cons ym ym-list)))
        (push `(,a ,b ,@(nthcdr 2 info)) result)))
    (reverse result)))

(defun pyim-pinyin-valid-shuangpin-p (shuangpin)
  (not (string-match-p
        pyim-pinyin--shuangpin-invalid-pinyin-regexp
        shuangpin)))

;; * Footer
(provide 'pyim-pinyin)

;;; pyim-pinyin.el ends here
