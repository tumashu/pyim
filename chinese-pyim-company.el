;;; chinese-pyim-company.el --- Integrate company-mode with Chinese-pyim

;; Copyright  2015 Feng Shu
;;
;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/chinese-pyim
;; Version: 0.0.1
;; Keywords: convenience, Chinese, pinyin, input-method, complete

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

;;; Commentary:
;;
;;  这个包为 `Chinese-pyim' 添加中文词条 *联想* 功能，其使用方式：
;;
;;      (require 'chinese-pyim-company)
;;

;;; Code:
(require 'company)
(require 'company-dabbrev)

(defcustom pyim-company-minimum-prefix-length 1
  "设置输入几个字符时开始搜索联想词条。"
  :group 'chinese-pyim
  :type 'number)

(defcustom pyim-company-predict-words-number 10
  "设置最多可以搜索多少个联想词条，如果设置为 nil
或者 0 时，关闭联想功能。"
  :group 'chinese-pyim
  :type 'number)

;; `company-mode' 开启了 `lexical-binding' ，所以需要特殊的方式
;; 调整其行为，这里保存 company-frontends 默认设定。
(defvar pyim-backup-company-frontends company-frontends)
(defvar pyim-backup-company-idle-delay company-idle-delay)
(defvar pyim-backup-company-minimum-prefix-length company-minimum-prefix-length)
(defvar pyim-backup-company-selection-wrap-around company-selection-wrap-around)
(defvar pyim-backup-company-require-match company-require-match)

(defun pyim-revert-company-default-setting ()
  "恢复 `company-mode' 默认设置的函数。"
  (setq company-frontends pyim-backup-company-frontends
        company-idle-delay pyim-backup-company-idle-delay
        company-minimum-prefix-length pyim-backup-company-minimum-prefix-length
        company-selection-wrap-around pyim-backup-company-selection-wrap-around
        company-require-match pyim-backup-company-require-match))

(defun pyim-company-completion-cancelled-hook (x)
  "`company-mode' 补全取消后，恢复 `company-mode' 原来的设置。"
  (pyim-revert-company-default-setting)
  (remove-hook 'company-completion-cancelled-hook
               'pyim-company-completion-cancelled-hook t))

(defun pyim-company-completion-finished-hook (x)
  "`company-mode' 补全完成后，恢复 `company-mode' 原来的设置。"
  (pyim-revert-company-default-setting)
  (remove-hook 'company-completion-finished-hook
               'pyim-company-completion-finished-hook t))

;;;###autoload
(defun pyim-company-complete ()
  "`company-mode' 补全命令的包装函数，专门用户 `chinese-pyim'"
  (interactive)
  (unless company-mode (company-mode))
  (setq company-backends
        (append '((pyim-company-predict-words
                   pyim-company-dabbrev)) pyim-backup-company-frontends)
        company-minimum-prefix-length pyim-company-minimum-prefix-length
        company-idle-delay 0.1
        company-selection-wrap-around t
        company-require-match nil)
  (add-hook 'company-completion-cancelled-hook
            'pyim-company-completion-cancelled-hook t)
  (add-hook 'company-completion-finished-hook
            'pyim-company-completion-finished-hook t)
  (company-manual-begin))

;; `Chinese-pyim' 选词结束后，调用 `pyim-company-complete'.
(add-hook 'pyim-select-word-finish-hook 'pyim-company-complete)

(defun pyim-company-dabbrev (command &optional arg &rest ignored)
  "`company-mode' dabbrev 补全后端，是 `company-dabbrev'
(包含在 `company-mode'中) 的衍生版本，通过与 Chinese-pyim 配合
来补全中文（忽略非中文）。

`pyim-company-dabbrev' 可以和 `company-dabbrev' 配合使用。具体细节请
参考 Company-mode group backends 相关文档。"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'pyim-company-dabbrev))
    (prefix
     (and (featurep 'chinese-pyim)
          ;; 光标前字符是否时汉字？
          (string-match-p "\\cc" (char-to-string (char-before)))
          (string-match-p "\\cc" pyim-current-str)
           pyim-current-str))
    (candidates
     (let* ((case-fold-search company-dabbrev-ignore-case)
            (words (company-dabbrev--search
                    ;; 最多补全六个中文字符，得到太长的中文字符串用处不大。
                    (format "%s[^[:punct:][:blank:]\n]\\{1,6\\}" arg)
                    company-dabbrev-time-limit
                    (pcase company-dabbrev-other-buffers
                      (`t (list major-mode))
                      (`all `all))))
            (downcase-p
             (if (eq company-dabbrev-downcase 'case-replace)
                 case-replace
               company-dabbrev-downcase)))
       (if downcase-p
           (mapcar 'downcase words)
         words)))
    (ignore-case company-dabbrev-ignore-case)
    (duplicates t)))

(defun pyim-get-predict-words (pinyin word)
  "获取所有词库中以 `word' 开头的中文词条，用于拼音联想输入。
`pinyin' 选项是为了在词库中快速定位，减少搜索时间。"
  (when (and pyim-company-predict-words-number
             (> pyim-company-predict-words-number 0)
             (> (length pinyin) 0)
             (> (length word) 0))
    (let* ((limit pyim-company-predict-words-number)
           (regexp (concat " +\\(" (regexp-quote word) "\\cc+\\)"))
           (count 0)
           predict-words)
      (dolist (buf pyim-buffer-list)
        (with-current-buffer (cdr (assoc "buffer" buf))
          (pyim-bisearch-word pinyin (point-min) (point-max))
          (save-excursion
            (forward-line (- 0 limit))
            (while (and (re-search-forward regexp nil t)
                        (< count (* 2 limit)))
              (setq predict-words (delete-dups
                                   (append predict-words
                                           (list (match-string 1)))))
              (goto-char (match-end 0))
              (setq count (1+ count))))))
      predict-words)))

(defun pyim-company-predict-words (command &optional arg &rest ignore)
  "`company-mode' 补全后端，只用于 Chinese-pyim 联想词补全，无其他
作用。"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'pyim-company-backend))
    (prefix
     (and (featurep 'chinese-pyim)
          ;; 光标前字符是否时汉字？
          (string-match-p "\\cc" (char-to-string (char-before)))
          (string-match-p "\\cc" pyim-current-str)
          pyim-current-key
          pyim-current-str))
    (candidates
     (pyim-get-predict-words pyim-current-key
                             pyim-current-str))))

(provide 'chinese-pyim-company)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; chinese-pyim.el ends here
