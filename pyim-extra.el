;;; pyim-extra.el --- Extra functions used by Chinese-pyim (chinese pinyin input method)

;; Copyright 2006 Ye Wenbin
;;           2014 Feng Shu

;; Author: Ye Wenbin <wenbinye@163.com>, Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/chinese-pyim
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar pyim-punctuation-list
  '(("'" "‘" "’")
    ("\"" "“" "”")
    ("_" "――")
    ("^" "……")
    ("]" "】")
    ("[" "【")
    ("@" "◎")
    ("?" "？")
    (">" "》")
    ("=" "＝")
    ("<" "《")
    (";" "；")
    (":" "：")
    ("/" "、")
    ("." "。")
    ("-" "－")
    ("," "，")
    ("+" "＋")
    ("*" "×")
    (")" "）")
    ("(" "（")
    ("&" "※")
    ("%" "％")
    ("$" "￥")
    ("#" "＃")
    ("!" "！")
    ("`" "・")
    ("~" "～")
    ("}" "』")
    ("|" "÷")
    ("{" "『")))

(defvar pyim-punc-escape-list
  (number-sequence ?0 ?9)
  "Punctuation will not insert after this characters.
If you don't like this funciton, set the variable to nil")
(defvar pyim-insert-ascii-char (cons ?\; "；")
  "*Key used for `pyim-insert-ascii'.")

(defvar pyim-punc-translate-p t
  "*Non-nil means will translate punctuation.")

(defun pyim-punc-translate (char)
  (if pyim-punc-translate-p
      (cond ((< char ? ) "")
            ((and pyim-insert-ascii-char
                  (= char (car pyim-insert-ascii-char)))
             (char-to-string char))
            (t (let ((str (char-to-string char))
                     punc)
                 (if (and (not (member (char-before) pyim-punc-escape-list))
                          (setq punc (cdr (assoc str pyim-punctuation-list))))
                     (progn
                       (if (= char (char-before))
                           (delete-char -1))
                       (if (= (safe-length punc) 1)
                           (car punc)
                         (setcdr (cdr punc) (not (cddr punc)))
                         (if (cddr punc)
                             (car punc)
                           (nth 1 punc))))
                   str))))
    (char-to-string char)))

(defun pyim-punc-translate-toggle (arg)
  (interactive "P")
  (setq pyim-punc-translate-p
        (if (null arg)
            (not pyim-punc-translate-p)
          (> (prefix-numeric-value arg) 0))))

;;;  一个快速插入英文的命令。按自己的需要绑定到 ";"
(defun pyim-insert-ascii ()
  (interactive)
  (if current-input-method
      (let (c)
        (message (format "自定义输入(直接空格%s, 回车%c): "
                         (cdr pyim-insert-ascii-char)
                         (car pyim-insert-ascii-char)))
        (setq c (read-event))
        (cond ((= c ? ) (insert (cdr pyim-insert-ascii-char)))
              ((= c ?\r) (insert-char (car pyim-insert-ascii-char) 1))
              (t
               (setq unread-command-events (list last-input-event))
               (insert (read-from-minibuffer "自定义输入: ")))))
    (call-interactively 'self-insert-command)))

;;;  load and save history
(defun pyim-load-history (history-file package)
  (let* ((pyim-current-package package)
         (history (pyim-history))
         item)
    (when (file-exists-p history-file)
      (with-current-buffer (find-file-noselect history-file)
        (goto-char (point-min))
        (while (not (eobp))
          (if (and (setq item (pyim-line-content))
                   (= (length item) 2))
              (puthash (car item)
                       `(nil ("pos" . ,(string-to-number (cadr item))))
                       history))
          (forward-line 1))
        (kill-buffer (current-buffer))))))

(defun pyim-save-history (history-file package)
  (interactive)
  (let* ((pyim-current-package package)
         (history (pyim-history)))
    (with-temp-buffer
      (erase-buffer)
      (let (pos)
        (maphash (lambda (key val)
                   (unless (or (pyim-string-emptyp key)
                               (= (setq pos (cdr (assoc "pos" (cdr val)))) 1))
                     (insert key " " (number-to-string pos) "\n")))
                 history))
      (write-file history-file))))

;;;  增加两个快速选择的按键
(defun pyim-quick-select-1 ()
  "如果没有可选项，插入数字，否则选择对应的词条"
  (interactive)
  (if (car pyim-current-choices)
      (let ((index (pyim-page-start))
            (end (pyim-page-end)))
        (if (>= index end)
            (pyim-append-string (pyim-translate last-command-event))
          (pyim-remember-select (1+ index))
          (setq pyim-current-str (pyim-choice (nth index (car pyim-current-choices))))))
    (pyim-append-string (pyim-translate last-command-event)))
  (pyim-terminate-translation))

(defun pyim-quick-select-2 ()
  "如果没有可选项，插入数字，否则选择对应的词条"
  (interactive)
  (if (car pyim-current-choices)
      (let ((index (1+ (pyim-page-start)))
            (end (pyim-page-end)))
        (if (>= index end)
            (pyim-append-string (pyim-translate last-command-event))
          (pyim-remember-select (1+ index))
          (setq pyim-current-str (pyim-choice (nth index (car pyim-current-choices))))))
    (pyim-append-string (pyim-translate last-command-event)))
  (pyim-terminate-translation))

(defun pyim-describe-char (pos package)
  (interactive
   (list (point)
         (if (eq input-method-function 'pyim-input-method)
             (pyim-package-name)
           (let (pyim-current-package)
             (setq pyim-current-package
                   (if (= (length pyim-package-list) 1)
                       (cdar pyim-package-list)
                     (assoc
                      (completing-read "In package: "
                                       pyim-package-list nil t
                                       (caar pyim-package-list))
                      pyim-package-list)))
             (pyim-package-name)))))
  (if (>= pos (point-max))
      (error "No character follows specified position"))
  (let ((char (char-after pos))
        (func (intern-soft (format "%s-get-char-code" package)))
        code)
    (when func
      (setq code (funcall func char))
      (if code
          (message "Type %S to input %c for input method %s"
                   code char package)
        (message "Can't find char code for %c" char)))))

;;;  char table
(defun pyim-make-char-table (chars table)
  "Set `pyim-char-database'"
  (dolist (char chars)
    (let ((code (car char)))
      (dolist (c (cdr char))
        (set (intern c table) code)))))

(defsubst pyim-get-char-code (char table)
  "Get the code of the character CHAR"
  (symbol-value (intern-soft (char-to-string char) table)))


(provide 'pyim-extra)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; pyim-extra.el ends here
