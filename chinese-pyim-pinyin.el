;;; chinese-pyim-pinyin.el --- Pinyin functions used by Chinese-pyim (chinese pinyin input method)

;; Copyright 2006 Ye Wenbin
;;           2014-2015 Feng Shu

;; Author: Ye Wenbin <wenbinye@163.com>, Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/chinese-pyim
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 0.0.1
;; Keywords: convenience, Chinese, pinyin, input-method

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
(require 'cl-lib)

;;;  variable declare
(defvar pyim-pinyin-shenmu
  '("b" "p" "m" "f" "d" "t" "n" "l" "g" "k" "h"
    "j" "q" "x" "z" "c" "s" "zh" "ch" "sh" "r" "y" "w"))
(defvar pyim-pinyin-yunmu
  '("a" "o" "e" "i" "u" "v" "ai" "ei" "ui" "ao" "ou" "iu"
    "ie" "ia" "ua" "ve" "er" "an" "en" "in" "un" "vn" "ang" "iong"
    "eng" "ing" "ong" "uan" "uang" "ian" "iang" "iao" "ue"
    "uai" "uo"))
(defvar pyim-pinyin-valid-yunmu
  '("a" "o" "e" "ai" "ei" "ui" "ao" "ou" "er" "an" "en"
    "ang" "eng"))

(defvar pyim-pinyin-char-table (make-vector 1511 nil))
(defvar pyim-pinyin-pos nil)
(defvar pyim-pinyin-pylist nil)

;;;  handle function
(defun pyim-pinyin-handle-string ()
  (let ((str pyim-current-key)
        userpos wordspy)
    (setq pyim-pinyin-pylist (pyim-pinyin-split-string str)
          pyim-pinyin-pos 0)
    (unless (and (pyim-pinyin-validp pyim-pinyin-pylist)
                 (progn
                   (setq userpos (pyim-pinyin-user-divide-pos str)
                         pyim-current-key (pyim-pinyin-restore-user-divide
                                           (pyim-pinyin-pylist-to-string pyim-pinyin-pylist)
                                           userpos))
                   (setq pyim-current-choices (list (delete-dups (pyim-pinyin-get-choices pyim-pinyin-pylist))))
                   (when  (car pyim-current-choices)
                     (setq pyim-current-pos 1)
                     (pyim-pinyin-format-page)
                     t)))
      (setq pyim-current-str (replace-regexp-in-string "-" "" pyim-current-key))
      (setq pyim-guidance-str (format "%s"
                                      (replace-regexp-in-string
                                       "-" " " pyim-current-key)))
      (pyim-show))))

(defun pyim-pinyin-format-page ()
  "按当前位置，生成候选词条"
  (let* ((end (pyim-page-end))
         (start (1- (pyim-page-start)))
         (choices (car pyim-current-choices))
         (choice (pyim-subseq choices start end))
         (pos (1- (min pyim-current-pos (length choices))))
         (i 0) rest)
    (setq pyim-current-str (concat (substring pyim-current-str 0 pyim-pinyin-pos)
                                   (pyim-choice (nth pos choices)))
          rest (mapconcat (lambda (py)
                            (concat (car py) (cdr py)))
                          (nthcdr (length pyim-current-str) pyim-pinyin-pylist)
                          "'"))
    (if (string< "" rest)
        (setq pyim-current-str (concat pyim-current-str rest)))
    (setq pyim-guidance-str
          (format "%s[%d/%d]: %s"
                  (replace-regexp-in-string "-" " " pyim-current-key)
                  (pyim-current-page) (pyim-total-page)
                  (mapconcat 'identity
                             (mapcar
                              (lambda (c)
                                (format "%d.%s " (setq i (1+ i))
                                        (if (consp c)
                                            (concat (car c) (cdr c))
                                          c)))
                              choice) " ")))
    (pyim-show)))

(defun pyim-pinyin-pylist-to-string (pylist)
  "把分解的拼音合并，以便进行查找"
  (mapconcat 'identity
             (mapcar (lambda (w) (concat (car w) (cdr w))) pylist)
             "-"))

;; 将汉字的拼音分成声母和其它
(defun pyim-pinyin-get-sm (py)
  "从一个拼音字符串中提出第一个声母。"
  (when (and py (string< "" py))
    (let (shenmu yunmu len)
      (if (< (length py) 2)
          (if (member py pyim-pinyin-shenmu)
              (cons py "")
            (cons "" py))
        (setq shenmu (substring py 0 2))
        (if (member shenmu pyim-pinyin-shenmu)
            (setq py (substring py 2))
          (setq shenmu (substring py 0 1))
          (if (member shenmu pyim-pinyin-shenmu)
              (setq py (substring py 1))
            (setq shenmu "")))
        (cons shenmu py)))))

(defun pyim-pinyin-get-ym (py)
  "从一个拼音字符串中提出第一个韵母"
  (when (and py (string< "" py))
    (let (yunmu len)
      (setq len (min (length py) 5))
      (setq yunmu (substring py 0 len))
      (while (and (not (member yunmu pyim-pinyin-yunmu))
                  (> len 0))
        (setq yunmu (substring py 0 (setq len (1- len)))))
      (setq py (substring py len))
      (if (and (string< "" py)
               (not (member (substring py 0 1) pyim-pinyin-shenmu))
               (member (substring yunmu -1) pyim-pinyin-shenmu)
               (member (substring yunmu 0 -1) pyim-pinyin-yunmu))
          (setq py (concat (substring yunmu -1) py)
                yunmu (substring yunmu 0 -1)))
      (cons yunmu py))))

(defun pyim-pinyin-get-charpy (py)
  "分解一个拼音字符串成声母和韵母。"
  (when (and py (string< "" py))
    (let* ((sm (pyim-pinyin-get-sm py))
           (ym (pyim-pinyin-get-ym (cdr sm)))
           (chpy (concat (car sm) (car ym))))
      (if (or (null ym)                 ; 如果韵母为空
              (and (string< "" (car ym)) (not (pyim-pinyin-get chpy)))) ; 错误的拼音
          (cons sm "")
        (cons (cons (car sm) (car ym)) (cdr ym))))))

;;; 处理输入的拼音
(defun pyim-pinyin-split-string (py)
  "把一个拼音字符串分解。如果含有 '，优先在这个位置中断，否则，自动分
解成声母和韵母的组合"
  (when (and py (string< "" py))
    (apply 'append
           (mapcar (lambda (p)
                     (let (chpy pylist)
                       (setq p (replace-regexp-in-string "[ -]" "" p))
                       (while (when (string< "" p)
                                (setq chpy (pyim-pinyin-get-charpy p))
                                (setq pylist (append pylist (list (car chpy))))
                                (setq p (cdr chpy))))
                       pylist))
                   (split-string py "'")))))

(defun pyim-pinyin-validp (pylist)
  "检查得到的拼音是否含有声母为空，而韵母又不正确的拼音"
  (let ((valid t) py)
    (while (progn
             (setq py (car pylist))
             (if (and (not (string< "" (car py)))
                      (not (member (cdr py) pyim-pinyin-valid-yunmu)))
                 (setq valid nil)
               (setq pylist (cdr pylist)))))
    valid))

(defun pyim-pinyin-user-divide-pos (py)
  "检测出用户分割的位置"
  (setq py (replace-regexp-in-string "-" "" py))
  (let (poslist (start 0))
    (while (string-match "'" py start)
      (setq start (match-end 0))
      (setq poslist (append poslist (list (match-beginning 0)))))
    poslist))

(defun pyim-pinyin-restore-user-divide (py pos)
  "按检测出的用户分解的位置，重新设置拼音"
  (let ((i 0) (shift 0) cur)
    (setq cur (car pos)
          pos (cdr pos))
    (while (and cur (< i (length py)))
      (if (= (aref py i) ?-)
          (if (= i (+ cur shift))
              (progn
                (aset py i ?')
                (setq cur (car pos)
                      pos (cdr pos)))
            (setq shift (1+ shift))))
      (setq i (1+ i)))
    (if cur (setq py (concat py "'")))  ; the last char is `''
    py))

;;;  词组选择解析
(defun pyim-pinyin-get-choices (pylist)
  "得到可能的词组和汉字。例如：

 (pyim-pinyin-get-choices  (pyim-pinyin-split-string \"pin-yin\"))
  => (#(\"拼音\" 0 2 (py (\"pin-yin\"))) \"拼\" \"品\" \"贫\" \"苹\" \"聘\" \"频\" \"拚\" \"颦\" \"牝\" \"嫔\" \"姘\" \"嚬\")

 (pyim-pinyin-get-choices  (pyim-pinyin-split-string \"pin-yin\"))
 => (#(\"拼音\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))) #(\"贫铀\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))) #(\"聘用\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))) \"拼\" \"品\" \"贫\" \"苹\" \"聘\" \"频\" \"拚\" \"颦\" \"牝\" \"嫔\" \"姘\" \"嚬\")

"
  (let (choice words chars wordspy choice)
    (setq wordspy (pyim-pinyin-possible-words-py pylist))
    (if wordspy
        (setq words (pyim-pinyin-possible-words wordspy)))
    (setq chars (pyim-pinyin-get (concat (caar pylist) (cdar pylist)))
          choice (append words chars))))

(defun pyim-pinyin-possible-words (wordspy)
  "根据拼音得到可能的词组。例如：
  (pyim-pinyin-possible-words '((\"p-y\" (\"p\" . \"in\") (\"y\" . \"\"))))
    => (#(\"拼音\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))) #(\"贫铀\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))) #(\"聘用\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))))

"
  (let (words)
    (dolist (word (reverse wordspy))
      (if (listp word)
          (setq words (append words (pyim-pinyin-match-word (pyim-pinyin-get (car word))
                                                            (cdr word))))
        (setq words (append words (mapcar (lambda (w)
                                            (propertize w 'py (list word)))
                                          (pyim-pinyin-get word))))))
    words))

(defun pyim-pinyin-possible-words-py (pylist)
  "所有可能的词组拼音。从第一个字开始，每个字断开形成一个拼音。如果是
完整拼音，则给出完整的拼音，如果是给出声母，则为一个 CONS CELL，CAR 是
拼音，CDR 是拼音列表。例如：

 (setq foo-pylist (pyim-pinyin-split-string \"pin-yin-sh-r\"))
  => ((\"p\" . \"in\") (\"y\" . \"in\") (\"sh\" . \"\") (\"r\" . \"\"))

 (pyim-pinyin-possible-words-py foo-pylist)
  => (\"pin-yin\" (\"p-y-sh\" (\"p\" . \"in\") (\"y\" . \"in\") (\"sh\" . \"\")) (\"p-y-sh-r\" (\"p\" . \"in\") (\"y\" . \"in\") (\"sh\" . \"\") (\"r\" . \"\")))
 "
  (let (pys fullpy smpy wordlist (full t))
    (if (string< "" (cdar pylist))
        (setq fullpy (concat (caar pylist) (cdar pylist))
              smpy (pyim-pinyin-essential-py (car pylist)))
      (setq smpy (caar pylist)
            full nil))
    (setq wordlist (list (car pylist)))
    (dolist (py (cdr pylist))
      (setq wordlist (append wordlist (list py)))
      (if (and full (string< "" (cdr py)))
          (setq fullpy (concat fullpy "-" (car py) (cdr py))
                smpy (concat smpy "-" (pyim-pinyin-essential-py py))
                pys (append pys (list fullpy)))
        (setq full nil
              smpy (concat smpy "-" (pyim-pinyin-essential-py py))
              pys (append pys (list (cons smpy wordlist))))))
    ;; (message "%s: %s" pys wordlist))
    pys))

(defun pyim-pinyin-match-word (wordlist wordspy)
  "给出一个词组列表和它的拼音列表，给出所有可能的词组，并加上一个 py
属性。例如：

 (pyim-pinyin-get \"p-y\")
  => (\"拼音\" \"番禺\" \"培养\" \"培育\" \"配药\" \"彭阳\" \"朋友\" \"偏远\" \"便宜\" \"片语\" \"飘扬\" \"漂移\" \"漂游\" \"贫铀\" \"聘用\" \"平阳\" \"平遥\" \"平邑\" \"平阴\" \"平舆\" \"平原\" \"平远\" \"濮阳\")

 (pyim-pinyin-match-word (pyim-pinyin-get \"p-y\") '((\"p\" . \"in\") (\"y\" . \"\")))
  => (#(\"拼音\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))) #(\"贫铀\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))) #(\"聘用\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))))

"
  (let (words)
    (dolist (word wordlist)
      ;;      (message "word: %s" word)
      (let ((match t) py pys (tmppy wordspy))
        (dotimes (i (length wordspy))
          (setq py (car tmppy)
                tmppy (cdr tmppy))
          ;; (message "py: %s" py)
          (when (string< "" (cdr py))
            (let (chmatch)
              (dolist (chpy (pyim-pinyin-get-char-code (aref word i)))
                (if (string= (cdr (pyim-pinyin-get-sm chpy)) (cdr py))
                    (setq chmatch t)))
              (or chmatch (setq match nil)))))
        ;; (message "%d: py: %s, match: %s" i py match))
        (if match
            (setq words (append words (list (propertize word 'py wordspy)))))))
    words))

(defun pyim-pinyin-essential-py (py)
  "一个拼音中的主要部分，如果有声母返回声母，否则返回韵母"
  (if (string< "" (car py))
      (car py)
    (cdr py)))

;;;  create and rearrage
(defun pyim-pinyin-match-py (word pylist)
  (let (sym words fullpy abbpy chpy)
    (when (> (length word) 1)
      (if (stringp (car pylist))        ; if is full pinyin
          (progn (setq fullpy (car pylist))
                 (cons fullpy (mapconcat 'identity
                                         (mapcar 'pyim-pinyin-essential-py
                                                 (pyim-pinyin-split-string (replace-regexp-in-string "-" "'" fullpy)))
                                         "-")))
        (dotimes (i (length word))
          (setq chpy (car pylist)
                pylist (cdr pylist))
          (setq abbpy (concat abbpy "-"
                              (if (string< "" (car chpy))
                                  (car chpy) (cdr chpy))))
          (if (string< "" (cdr chpy))
              (setq fullpy (concat fullpy "-" (car chpy) (cdr chpy)))
            (setq fullpy (concat fullpy "-"
                                 (car (pyim-pinyin-get-char-code (aref word i)))))))
        (cons (substring fullpy 1)
              (substring abbpy 1))))))

(defun pyim-pinyin-intern-word (word py)
  (let((buf (cdr (assoc "buffer" (car (pyim-buffer-list)))))
       words)
    (with-current-buffer buf
      (pyim-bisearch-word py (point-min) (point-max))
      (if (string= (pyim-code-at-point) py)
          (progn
            (setq words (pyim-line-content)
                  words (cons (car words) (delete-dups (append (list word)
                                                               (cdr words)))))
            ;; (message "delete: %s" words))
            (pyim-delete-line))
        (forward-line 1)
        (setq words (list py word)))
      ;;    (message "insert: %s" words)
      (insert (mapconcat 'identity words " ") "\n"))))

(defun pyim-pinyin-create-word (word pylist)
  ;; (message "create: %s, %s" word pylist)
  (let ((py (pyim-pinyin-match-py word pylist))
        words)
    (when py
      (pyim-pinyin-intern-word word (car py))
      (pyim-pinyin-intern-word word (cdr py)))))

(defun pyim-pinyin-rearrange (word pylist)
  ;; (message "rearrage: %s, %s" word pylist)
  (let ((py (pyim-pinyin-match-py word pylist)))
    (when py
      (pyim-pinyin-rearrange-1 word
                               (car py))
      (pyim-pinyin-rearrange-1 word (cdr py)))))

(defun pyim-pinyin-rearrange-1 (word py)
  (pyim-pinyin-intern-word word py))

(defun pyim-pinyin-han-stringp (str)
  "Predicate whether the STR is a pinyin of a chinese character"
  (let ((valid t)
        (i 0))
    (while (and (< i (length str)) valid)
      (if (member (char-to-string (aref str i))
                  (mapcar 'identity "vmpfwckzyjqdltxuognbhsrei'-a"))
          (setq valid nil))
      (setq i (1+ i)))
    valid))

;;;  commands
(defun pyim-pinyin-select-current ()
  (interactive)
  (if (null (car pyim-current-choices))  ; 如果没有选项，输入空格
      (progn
        (setq pyim-current-str (pyim-translate last-command-event))
        (pyim-terminate-translation))
    (let ((str (pyim-choice (nth (1- pyim-current-pos) (car pyim-current-choices))))
          chpy pylist)
      (if (> (length str) 1)            ; 重排
          (pyim-pinyin-rearrange str (get-text-property 0 'py str))
        (setq chpy (nth pyim-pinyin-pos pyim-pinyin-pylist))
        (pyim-pinyin-rearrange-1 str (concat (car chpy) (cdr chpy))))
      (setq pyim-pinyin-pos (+ pyim-pinyin-pos (length str)))
      (if (= pyim-pinyin-pos (length pyim-pinyin-pylist)) ; 如果是最后一个，检查
                                        ; 是不是在文件中，没有的话，创
                                        ; 建这个词
          (progn
            (if (not (member pyim-current-str (car pyim-current-choices)))
                (pyim-pinyin-create-word pyim-current-str pyim-pinyin-pylist))
            (pyim-terminate-translation))
        (setq pylist (nthcdr pyim-pinyin-pos pyim-pinyin-pylist))
        (setq pyim-current-choices (list (pyim-pinyin-get-choices pylist))
              pyim-current-pos 1)
        (pyim-pinyin-format-page)))))

(defun pyim-pinyin-number-select ()
  "如果没有可选项，插入数字，否则选择对应的词条"
  (interactive)
  (if (car pyim-current-choices)
      (let ((index (- last-command-event ?1))
            (end (pyim-page-end)))
        (if (> (+ index (pyim-page-start)) end)
            (pyim-show)
          (setq pyim-current-pos (+ pyim-current-pos index))
          (setq pyim-current-str (concat (substring pyim-current-str 0
                                                    pyim-pinyin-pos)
                                         (pyim-choice
                                          (nth (1- pyim-current-pos)
                                               (car pyim-current-choices)))))
          (pyim-pinyin-select-current)))
    (pyim-append-string (char-to-string last-command-event))
    (pyim-terminate-translation)))

(defun pyim-pinyin-next-page (arg)
  (interactive "p")
  (if (= (length pyim-current-key) 0)
      (progn
        (pyim-append-string (pyim-translate last-command-event))
        (pyim-terminate-translation))
    (let ((new (+ pyim-current-pos (* pyim-page-length arg) 1)))
      (setq pyim-current-pos (if (> new 0) new 1)
            pyim-current-pos (pyim-page-start))
      (pyim-pinyin-format-page))))

(defun pyim-pinyin-previous-page (arg)
  (interactive "p")
  (pyim-pinyin-next-page (- arg)))

(defun pyim-pinyin-quit-no-clear ()
  (interactive)
  (setq pyim-current-str (replace-regexp-in-string "-" ""
                                                   pyim-current-key))
  (pyim-terminate-translation))

(defun pyim-pinyin-backward-kill-py ()
  (interactive)
  (string-match "['-][^'-]+$" pyim-current-key)
  (setq pyim-current-key
        (replace-match "" nil nil pyim-current-key))
  (pyim-pinyin-handle-string))

(defun pyim-pinyin-fuzzy-adjust-1 ()
  (interactive)
  (cond
   ((string-match-p "eng" pyim-current-key)
    (setq pyim-current-key
          (replace-regexp-in-string "eng" "en" pyim-current-key)))
   ((string-match-p "en[^g]*" pyim-current-key)
    (setq pyim-current-key
          (replace-regexp-in-string "en" "eng" pyim-current-key)))
   ((string-match-p "ing" pyim-current-key)
    (setq pyim-current-key
          (replace-regexp-in-string "ing" "in" pyim-current-key)))
   ((string-match-p "in[^g]*" pyim-current-key)
    (setq pyim-current-key
          (replace-regexp-in-string "in" "ing" pyim-current-key)))
   ((string-match-p "un" pyim-current-key)
    (setq pyim-current-key
          (replace-regexp-in-string "un" "ong" pyim-current-key)))
   ((string-match-p "ong" pyim-current-key)
    (setq pyim-current-key
          (replace-regexp-in-string "ong" "un" pyim-current-key))))
  (pyim-pinyin-handle-string))

(defun pyim-pinyin-activate-function ()
  (setq pyim-do-completion nil
        pyim-handle-function 'pyim-pinyin-handle-string
        pyim-translate-function 'pyim-punctuation-translate)
  (make-local-variable 'pyim-pinyin-pylist)
  (make-local-variable 'pyim-pinyin-pos))

;;;  pyim-pinyin-get
(defun pyim-pinyin-get (code)
  (let (words)
    (when (and (stringp code) (string< "" code))
      (dolist (buf (pyim-buffer-list))
        (with-current-buffer (cdr (assoc "buffer" buf))
          (setq words (append words
                              (cdr
                               (pyim-bisearch-word code
                                                   (point-min)
                                                   (point-max)))))))
      (delete-dups words))))

(defun pyim-pinyin-get-char-code (char)
  "Get the code of the character CHAR"
  (symbol-value (intern-soft (char-to-string char) pyim-pinyin-char-table)))

(defun pyim-pinyin-make-char-table-1 (chars)
  (dolist (char chars)
    (let ((code (car char)))
      (dolist (c (cdr char))
        (let* ((s (intern-soft c pyim-pinyin-char-table))
               (py (and s (symbol-value s))))
          (set (intern c pyim-pinyin-char-table) (append py (list code))))))))

(defun pyim-pinyin-make-char-table ()
  "Build pinyin char hashtable from quail/PY.el"
  (interactive)
  (with-temp-buffer
    (insert-file-contents (locate-library "quail/PY.el"))
    (goto-char (point-min))
    (while (re-search-forward
            "^[[:space:]]*([[:space:]]*\"\\([a-z]+\\)\"[[:space:]]*\"\\([^\"]+\\)\"[[:space:]]*)[[:space:]]*$" nil t)
      (let ((pinyin (match-string 1))
            (hanzi-string (substring-no-properties (match-string 2)))
            pinyin-list)
        (setq pinyin-list
              (list
               (append (list pinyin)
                       (split-string
                        (replace-regexp-in-string
                         "_$" ""
                         (replace-regexp-in-string
                          "\\(.\\)" "\\1_" hanzi-string)) "_"))))
        (pyim-pinyin-make-char-table-1 pinyin-list)))))

(defun pyim-hanzi2pinyin (string &optional shou-zi-mu separator ignore-duo-yin-zi)
  "将汉字字符串转换为对应的拼音字符串, 如果 `shou-zi-mu' 设置为t,转换仅得到拼音
首字母字符串。如果 `ignore-duo-yin-zi' 设置为t, 遇到多音字时，只使用第一个拼音。
其它拼音忽略。"
  (let (string-list pinyin-list)
    ;; 确保 `pyim-pinyin-char-table' 已经生成。
    (unless (pyim-pinyin-get-char-code ?文)
      (pyim-pinyin-make-char-table))
    ;; 将汉字字符串转换为字符list，英文原样输出。
    (setq string-list (split-string
                       (replace-regexp-in-string
                        "\\(\\cc\\)" "-\\1-" string)
                       "-"))
    ;; 删除空字符串
    (setq string-list (cl-remove-if #'(lambda (x)
                                        (= (length x) 0)) string-list))
    ;; 将上述汉字字符串里面的所有汉字转换为与之对应的拼音list。
    (setq pinyin-list (mapcar (lambda (str)
                                (cond
                                 ((> (length str) 1) (list str))
                                 ((and (> (length str) 0)
                                       (string-match-p "\\cc" str))
                                  (or (pyim-pinyin-get-char-code (string-to-char str)) (list str)))
                                 ((> (length str) 0) (list str)))) string-list))
    ;; 通过排列组合的方式将 pinyin-list 转化为拼音字符串。
    (if ignore-duo-yin-zi
        (mapconcat 'identity
                   (mapcar
                    (lambda (x)
                      (if shou-zi-mu
                          (substring (car x) 0 1)
                        (car x))) pinyin-list) (or separator ""))

      (mapconcat 'identity
                 (cl-remove-duplicates
                  (let ((result '("")))
                    (cl-loop for i in pinyin-list
                             do (setq result
                                      (cl-loop for j in i
                                               append (cl-loop for k in result
                                                               collect (concat k (if shou-zi-mu (substring j 0 1) j)
                                                                               (or separator "")))))) result)
                  :test (lambda (x y) (or (null y) (equal x y)))
                  :from-end t) " "))))

(defun pyim-hanzi2pinyin-simple (string &optional shou-zi-mu separator)
  "简化版的 `pyim-hanzi2pinyin', 不处理多音字。"
  (pyim-hanzi2pinyin string shou-zi-mu separator t))

(provide 'chinese-pyim-pinyin)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; chinese-pyim-pinyin.el ends here
