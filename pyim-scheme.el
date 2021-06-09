;;; pyim-scheme.el --- scheme tools for pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-common)

(defgroup pyim-scheme nil
  "Scheme tools for pyim."
  :group 'pyim)

(defcustom pyim-default-scheme 'quanpin
  "设置 pyim 使用哪一种输入法方案，默认使用全拼输入."
  :type 'symbol)

(defcustom pyim-assistant-scheme 'quanpin
  "设置辅助输入法方案.

这个功能主要用于五笔等形码输入法，在忘记编码的情况下，
临时激活某种辅助输入法（比如：拼音输入法）来输入汉字。"
  :type 'symbol)

(defvar pyim-assistant-scheme-enable nil
  "设置临时 scheme, 用于五笔等形码输入法临时拼音输入。")

(pyim-register-local-variables '(pyim-assistant-scheme-enable))

(defvar pyim-schemes nil
  "Pyim 支持的所有拼音方案.")

;;;###autoload
(defun pyim-default-scheme (&optional scheme-name)
  (interactive)
  (let* ((scheme-names (mapcar #'car pyim-schemes))
         (scheme-name
          (or scheme-name
              (intern (completing-read "PYIM: 将 pyim-default-scheme 设置为：" scheme-names)))))
    (if (memq scheme-name scheme-names)
        (progn
          (setq pyim-default-scheme scheme-name)
          (message "PYIM: `pyim-default-scheme' 已经设置为 %s." scheme-name)
          scheme-name)
      (message "PYIM: %s 不是一个有效的 scheme 名称, 继续使用 %s." scheme-name pyim-default-scheme)
      nil)))

(defun pyim-scheme-add (scheme)
  "Add SCHEME to `pyim-schemes'"
  (if (listp scheme)
      (let ((scheme-name (car scheme)))
        (when (symbolp scheme-name)
          (setq pyim-schemes
                (remove (assoc scheme-name pyim-schemes)
                        pyim-schemes)))
        (push scheme pyim-schemes))
    (message "PYIM: Invalid pyim scheme config!")))

(defun pyim-scheme-get (scheme-name)
  "获取名称为 SCHEME-NAME 的输入法方案。"
  (when scheme-name
    (assoc scheme-name pyim-schemes)))

(defun pyim-scheme-name (&optional default)
  "获取输入法 scheme"
  (let (scheme-name)
    (if (and pyim-assistant-scheme-enable
             (not default))
        (setq scheme-name
              (or pyim-assistant-scheme
                  pyim-default-scheme))
      (setq scheme-name pyim-default-scheme))
    (if (assq scheme-name pyim-schemes)
        scheme-name
      'quanpin)))

(defun pyim-scheme-get-option (scheme-name option)
  "获取名称为 SCHEME-NAME 的输入法方案，并提取其属性 OPTION 。"
  (when scheme-name
    (let* ((scheme (pyim-scheme-get scheme-name))
           (scheme-inherit
            (car (pyim-scheme-get
                  (plist-get (cdr scheme) :inherit)))))
      (if (member option (cdr scheme))
          (plist-get (cdr scheme) option)
        (pyim-scheme-get-option scheme-inherit option)))))

(pyim-scheme-add
 '(quanpin
   :document "全拼输入法方案（不可删除）。"
   :class quanpin
   :first-chars "abcdefghijklmnopqrstuwxyz"
   :rest-chars "vmpfwckzyjqdltxuognbhsrei'-a"
   :prefer-triggers ("v")))

(pyim-scheme-add
 '(wubi
   :document "五笔输入法。"
   :class xingma
   :first-chars "abcdefghijklmnopqrstuvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz'"
   :code-prefix "wubi/" ;五笔词库中所有的 code 都以 "wubi/" 开头，防止和其它词库冲突。
   :code-prefix-history (".") ;五笔词库以前使用 "." 做为 code-prefix.
   :code-split-length 4 ;默认将用户输入切成 4 个字符长的 code 列表（不计算 code-prefix）
   :code-maximum-length 4 ;五笔词库中，code 的最大长度（不计算 code-prefix）
   :prefer-triggers nil))

(pyim-scheme-add
 '(cangjie
   :document "倉頡输入法。"
   :class xingma
   :first-chars "abcdefghijklmnopqrstuvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz"
   :code-prefix "cangjie/" ;仓颉输入法词库中所有的 code 都以 "cangjie/" 开头，防止词库冲突。
   :code-prefix-history ("@") ;仓颉输入法词库曾经使用过的 code-prefix
   :code-split-length 5 ;默认将用户输入切成 5 个字符长的 code 列表（不计算 code-prefix）
   :code-maximum-length 5 ;仓颉词库中，code 的最大长度（不计算 code-prefix）
   :prefer-triggers nil))

(pyim-scheme-add
 '(pyim-shuangpin
   :document "与 pyim 配合良好的双拼输入法方案，源自小鹤双拼方案。"
   :class shuangpin
   :first-chars "abcdefghijklmnpqrstuvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz"
   :prefer-triggers ("o")
   :keymaps
   (("a" "a" "a")
    ("b" "b" "in")
    ("c" "c" "ao")
    ("d" "d" "ai")
    ("e" "e" "e")
    ("f" "f" "en")
    ("g" "g" "eng")
    ("h" "h" "ang")
    ("i" "ch" "i")
    ("j" "j" "an")
    ("k" "k" "ing" "uai")
    ("l" "l" "iang" "uang")
    ("m" "m" "ian")
    ("n" "n" "iao")
    ("o" "o" "uo" "o")
    ("p" "p" "ie")
    ("q" "q" "iu")
    ("r" "r" "uan")
    ("s" "s" "iong" "ong")
    ("t" "t" "ue" "ve")
    ("u" "sh" "u")
    ("v" "zh" "v" "ui")
    ("w" "w" "ei")
    ("x" "x" "ia" "ua")
    ("y" "y" "un")
    ("z" "z" "ou")
    ("aa" "a")
    ("aj" "an")
    ("ad" "ai")
    ("ac" "ao")
    ("ah" "ang")
    ("ee" "e")
    ("ew" "ei")
    ("ef" "en")
    ("er" "er")
    ("eg" "eng")
    ("ag" "ng")
    ("ao" "o")
    ("au" "ou"))))

(pyim-scheme-add
 '(microsoft-shuangpin
   :document "微软双拼方案。"
   :class shuangpin
   :first-chars "abcdefghijklmnopqrstuvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz;"
   :prefer-triggers nil
   :keymaps
   (("a" "a" "a")
    ("b" "b" "ou")
    ("c" "c" "iao")
    ("d" "d" "uang" "iang")
    ("e" "e" "e")
    ("f" "f" "en")
    ("g" "g" "eng")
    ("h" "h" "ang")
    ("i" "ch" "i")
    ("j" "j" "an")
    ("k" "k" "ao")
    ("l" "l" "ai")
    ("m" "m" "ian")
    ("n" "n" "in")
    ("o" "o" "uo" "o")
    ("p" "p" "un")
    ("q" "q" "iu")
    ("r" "r" "uan" "er")
    ("s" "s" "iong" "ong")
    ("t" "t" "ue")
    ("u" "sh" "u")
    ("v" "zh" "ve" "ui")
    ("w" "w" "ia" "ua")
    ("x" "x" "ie")
    ("y" "y" "uai" "v")
    ("z" "z" "ei")
    (";" ";" "ing")
    ("oa" "a")
    ("oj" "an")
    ("ol" "ai")
    ("ok" "ao")
    ("oh" "ang")
    ("oe" "e")
    ("oz" "ei")
    ("of" "en")
    ("or" "er")
    ("og" "eng")
    ("oo" "o")
    ("ob" "ou"))))

(pyim-scheme-add
 '(zhinengabc-shuangpin
   :document "智能ABC双拼方案"
   :class shuangpin
   :first-chars "abcdefghjklmnopqrstvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz"
   :prefer-triggers nil
   :keymaps
   (("q" "q" "ei")
    ("w" "w" "ian")
    ("e" "ch" "e")
    ("r" "r" "iu" "er")
    ("t" "t" "uang" "iang")
    ("y" "y" "ing")
    ("u" "u" "u")
    ("i" "i" "i")
    ("o" "o" "o" "uo")
    ("p" "p" "uan" "van")
    ("a" "zh" "a")
    ("s" "s" "ong" "iong")
    ("d" "d" "ua" "ia")
    ("f" "f" "en")
    ("g" "g" "eng")
    ("h" "h" "ang")
    ("j" "j" "an")
    ("k" "k" "ao")
    ("l" "l" "ai")
    ("z" "z" "iao")
    ("x" "x" "ie")
    ("c" "c" "in" "uai")
    ("v" "sh" "v")
    ("b" "b" "ou")
    ("n" "n" "un")
    ("m" "m" "ue" "ui")
    ("oa" "a")
    ("oj" "an")
    ("ol" "ai")
    ("ok" "ao")
    ("oh" "ang")
    ("oe" "e")
    ("oz" "ei")
    ("of" "en")
    ("or" "er")
    ("og" "eng")
    ("oo" "o")
    ("ob" "ou"))))

(pyim-scheme-add
 '(xiaohe-shuangpin
   :document "小鹤双拼输入法方案。"
   :class shuangpin
   :first-chars "abcdefghijklmnopqrstuvwxyz"
   :rest-chars "abcdefghijklmnopqrstuvwxyz"
   :prefer-triggers nil
   :keymaps
   (("a" "a" "a")
    ("b" "b" "in")
    ("c" "c" "ao")
    ("d" "d" "ai")
    ("e" "e" "e")
    ("f" "f" "en")
    ("g" "g" "eng")
    ("h" "h" "ang")
    ("i" "ch" "i")
    ("j" "j" "an")
    ("k" "k" "ing" "uai")
    ("l" "l" "iang" "uang")
    ("m" "m" "ian")
    ("n" "n" "iao")
    ("o" "o" "uo" "o")
    ("p" "p" "ie")
    ("q" "q" "iu")
    ("r" "r" "uan")
    ("s" "s" "iong" "ong")
    ("t" "t" "ue" "ve")
    ("u" "sh" "u")
    ("v" "zh" "v" "ui")
    ("w" "w" "ei")
    ("x" "x" "ia" "ua")
    ("y" "y" "un")
    ("z" "z" "ou")
    ("aa" "a")
    ("an" "an")
    ("ai" "ai")
    ("ao" "ao")
    ("ah" "ang")
    ("ee" "e")
    ("ei" "ei")
    ("en" "en")
    ("er" "er")
    ("eg" "eng")
    ("og" "ng")
    ("oo" "o")
    ("ou" "ou"))))

;; * Footer
(provide 'pyim-scheme)

;;; pyim-scheme.el ends here
