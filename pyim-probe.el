;;; pyim-probe.el --- Auto-Switch-to-English-Input probes for pyim

;; * Header
;; Copyright 2015-2017 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/pyim

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; * 使用說明                                                              :doc:
;; ** 簡介
;; 這個文件包含了許多探針函數，用於實現兩個功能：

;; 1. 根據環境自動切換到英文輸入模式
;; 2. 根據環境自動切換到半形標點輸入模式

;; ** 設置
;; *** 根據環境自動切換到英文輸入模式
;; 用戶將探針函數添加到 `pyim-english-input-switch-functions' 後, 就可以激活這個
;; 探針函數，比如：

;; #+BEGIN_EXAMPLE
;; (setq-default pyim-english-input-switch-functions
;;               '(pyim-probe-dynamic-english
;;                 pyim-probe-isearch-mode
;;                 pyim-probe-program-mode))
;; #+END_EXAMPLE

;; 只要上述 *函數列表* 中，任意一個函數返回值為 t, pyim 就自動切換到
;; 英文輸入模式。

;; *** 根據環境自動切換到半形標點輸入模式
;; 用戶將探針函數添加到 `pyim-punctuation-half-width-functions' 後, 就可以激活這個
;; 探針函數。

;;; Code:
;; * 代碼                                                                 :code:
;; ** 根據環境自動切換到英文輸入模式
(defun pyim-probe-program-mode ()
  "激活這個 pyim 探針函數後，只能在字元串和 comment 中輸入中文。
註：僅僅影響 `prog-mode' 衍生的 mode 。

用於：`pyim-english-input-switch-functions' 。"
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (let* ((pos (point))
           (ppss (syntax-ppss pos)))
      (not
       (or (car (setq ppss (nthcdr 3 ppss)))
           (car (setq ppss (cdr ppss)))
           (nth 3 ppss))))))

(defun pyim-probe-org-speed-commands ()
  "激活這個 pyim 探針函數後，可以解決 org-speed-commands 與 pyim 沖突問題。

用於：`pyim-english-input-switch-functions' 。"
  (and (string= major-mode "org-mode")
       (bolp)
       (looking-at org-heading-regexp)
       org-use-speed-commands))

(defun pyim-probe-isearch-mode ()
  "激活這個 pyim 探針函數後，使用 isearch 搜索時，禁用中文輸入，強制英文輸入。

用於：`pyim-english-input-switch-functions' 。"
  (and pyim-isearch-mode
       ;; isearch 啟動的時候，會設置一個 buffer variable: `isearch-mode'
       ;; 檢測所有 buffer 中 `isearch-mode' 的取值，如果任何一個
       ;; 取值為 t, 就說明 isearch 已經啟動。
       (cl-some #'(lambda (buf)
                    (buffer-local-value 'isearch-mode buf))
                (buffer-list))))

(defun pyim-probe-org-structure-template ()
  "激活這個 pyim 探針函數後，輸入 org-structure-template 時，不會開啟中文輸入。

用於：`pyim-english-input-switch-functions' 。"
  (when (eq major-mode 'org-mode)
    (let ((line-string (buffer-substring (point-at-bol) (point))))
      (and (looking-at "[ \t]*$")
           (string-match "^[ \t]*<\\([a-zA-Z]*\\)$" line-string)))))

(defun pyim-probe-dynamic-english ()
  "激活這個 pyim 探針函數後，使用下麵的規則動態切換中英文輸入：

1. 從光標往前找第一個非數字的字元，為中文字元時，輸入下一個字元時默認開啟中文輸入
2. 從光標往前找第一個非數字的字元，為其他字元時，輸入下一個字元時默認開啟英文輸入
3. 使用 `pyim-convert-code-at-point' 可以將光標前的 code 字元串轉換為中文，
   所以用戶需要給 `pyim-convert-code-at-point' 綁定一個快捷鍵，比如：

   (global-set-key (kbd \"M-i\") 'pyim-convert-code-at-point)

這個函數用於：`pyim-english-input-switch-functions' 。"
  (let* ((offset 0)
         (non-digit-str-before-1 (pyim-char-before-to-string offset)))
    (while (and non-digit-str-before-1
                (cl-search non-digit-str-before-1 "0123456789"))
      (cl-incf offset)
      (setq non-digit-str-before-1 (pyim-char-before-to-string offset)))
    (unless (string= (buffer-name) " *temp*") ; Make sure this probe can work with exim of exwm.
      (if (<= (point) (save-excursion (back-to-indentation)
                                      (point)))
          (not (or (pyim-string-match-p
                    "\\cc"
                    (save-excursion
                      ;; 查找前一個非空格字元。
                      (if (re-search-backward "[^[:space:]\n]" nil t)
                          (char-to-string (char-after (point))))))
                   (> (length (pyim-entered-get)) 0)))
        (not (or (pyim-string-match-p "\\cc" non-digit-str-before-1)
                 (> (length (pyim-entered-get)) 0)))))))

(defun pyim-probe-auto-english ()
  "激活這個 pyim 探針函數後，使用下麵的規則自動切換中英文輸入：

1. 當前字元為英文字元（不包括空格）時，輸入下一個字元為英文字元
2. 當前字元為中文字元或輸入字元為行首字元時，輸入的字元為中文字元
3. 以單個空格為界，自動切換中文和英文字元
   即，形如 `我使用 emacs 編輯此函數' 的句子全程自動切換中英輸入法

這個函數用於：`pyim-english-input-switch-functions' 。"
  (let ((str-before-1 (pyim-char-before-to-string 0))
        (str-before-2 (pyim-char-before-to-string 1)))
    (unless (string= (buffer-name) " *temp*")
      (if (> (point) (save-excursion (back-to-indentation)
                                     (point)))
          (or (if (pyim-string-match-p " " str-before-1)
                  (pyim-string-match-p "\\cc" str-before-2)
                (and (not (pyim-string-match-p "\\cc" str-before-1))
                     (= (length (pyim-entered-get)) 0))))))))

(defun pyim-probe-evil-normal-mode ()
  "判斷是否是evil的normal模式，如果是則返回true.
這個函數用於：`pyim-english-input-switch-functions'."
  (evil-normal-state-p))

;; ** 根據環境自動切換到半形標點輸入模式
(defun pyim-probe-punctuation-line-beginning (char)
  "激活這個 pyim 探針函數後，行首輸入標點時，強制輸入半形標點。

用於：`pyim-punctuation-half-width-functions' 。"
  (let ((line-string (buffer-substring (point-at-bol) (point))))
    (unless (string= (buffer-name) " *temp*") ; Make sure this probe can work with exim of exwm.
      (and (member (char-to-string char)
                   (mapcar 'car pyim-punctuation-dict))
           (string-match "^[ \t]*$" line-string)))))

(defun pyim-probe-punctuation-after-punctuation (char)
  "激活這個 pyim 探針函數後，半形標點後再輸入一個標點符號時，強制輸入半形標點。

用於：`pyim-punctuation-half-width-functions' 。"
  (let ((str-before-1 (pyim-char-before-to-string 0))
        (puncts (mapcar 'car pyim-punctuation-dict)))
    (and (member str-before-1 puncts)
         (member (char-to-string char) puncts))))

(defun pyim-probe-org-latex-mode ()
  "org-mode 中的 latex fragment 和 latex 巨集指令中自動切換到英文輸入."
  (when (eq major-mode 'org-mode)
    (or (not (eq (org-inside-LaTeX-fragment-p) nil))
        (not (eq (org-inside-latex-macro-p) nil)))))

;; * Footer
(provide 'pyim-probe)

;;; pyim-probe.el ends here
