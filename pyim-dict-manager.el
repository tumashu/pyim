;;; pyim-dict-manager.el --- Dict manager for pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-dict)

(defgroup pyim-dict nil
  "Dict tools for pyim."
  :group 'pyim)

(defvar pyim-dict-manager--buffer "*pyim-dict-manager*")

(defun pyim-dict-manager-refresh ()
  "Refresh the contents of the *pyim-dict-manager* buffer."
  (interactive)
  (with-current-buffer pyim-dict-manager--buffer
    (let ((inhibit-read-only t)
          (dicts-list pyim-dicts)
          (format-string "%-4s %-4s %-60s\n")
          (face-attr '((foreground-color . "DarkOrange2")
                       (bold . t)))
          (i 1))
      (erase-buffer)
      (insert (propertize (format format-string "序号" "启用" "词库文件")
                          'face face-attr))
      (insert (propertize (format format-string
                                  "----" "----"
                                  "----------------------------------------------------------------------\n")
                          'face face-attr))
      (if (not pyim-dicts)
          (insert "拼音词库是 pyim 使用顺手与否的关键。根据经验估计：

1. 当词库词条超过100万时 (词库文件>20M)，pyim 选词频率大大降低。
2. 当词库词条超过100万时，pyim 中文输入体验可以达到搜狗输入法的 80%。

想快速体验 pyim 输入法的用户, 可以使用 pyim-basedict：

     (require 'pyim-basedict)
     (pyim-basedict-enable)

喜欢折腾的用户可以从下面几个途径获得 pyim 更详细的信息。
1. 使用 `C-h v pyim-dicts' 了解 pyim 词库文件格式。
2. 了解如何导入其它输入法的词库。
   1. 使用 package 管理器查看 pyim 包的简介
   2. 阅读 pyim.el 文件 Commentary
   3. 查看 pyim 在线 README：https://github.com/tumashu/pyim\n")
        (dolist (dict dicts-list)
          (let ((disable (plist-get dict :disable))
                (file (plist-get dict :file)))
            (insert (propertize (format format-string
                                        i (if disable "NO" "YES") file)
                                'id i 'disable disable 'file file)))
          (setq i (1+ i))))
      (insert (propertize "
操作命令：[A] 添加词库  [D] 删除词库   [P] 向上移动   [N] 向下移动  [g] 刷新页面
          [s] 保存配置  [R] 重启输入法 [C-c C-c] 禁用/启用当前词库"
                          'face face-attr)))))

(defun pyim-dict-manager-toggle-dict (&optional _enable)
  "启用当前行对应的词库。"
  (interactive)
  (when (equal (buffer-name) pyim-dict-manager--buffer)
    (let* ((id (get-text-property (point) 'id))
           (dict (cl-copy-list (nth (1- id) pyim-dicts)))
           (disable (plist-get dict :disable))
           (line (line-number-at-pos)))
      (setf (nth (1- id) pyim-dicts) (plist-put dict :disable (not disable)))
      (if (not disable)
          (message "禁用当前词库")
        (message "启用当前词库"))
      (pyim-dict-manager-refresh)
      (goto-char (point-min))
      (forward-line (- line 1)))))

(defun pyim-dict-manager-delete-dict ()
  "从 `pyim-dicts' 中删除当前行对应的词库信息。"
  (interactive)
  (when (equal (buffer-name) pyim-dict-manager--buffer)
    (let ((id (get-text-property (point) 'id))
          (line (line-number-at-pos)))
      (when (yes-or-no-p "确定要删除这条词库信息吗? ")
        (setq pyim-dicts (delq (nth (1- id) pyim-dicts) pyim-dicts))
        (pyim-dict-manager-refresh)
        (goto-char (point-min))
        (forward-line (- line 1))))))

(defun pyim-dict-manager-dict-position-up ()
  "向上移动词库。"
  (interactive)
  (when (equal (buffer-name) pyim-dict-manager--buffer)
    (let* ((id (get-text-property (point) 'id))
           (dict1 (nth (- id 1) pyim-dicts))
           (dict2 (nth (- id 2) pyim-dicts))
           (line (line-number-at-pos)))
      (when (> id 1)
        (setf (nth (- id 1) pyim-dicts) dict2)
        (setf (nth (- id 2) pyim-dicts) dict1)
        (pyim-dict-manager-refresh)
        (goto-char (point-min))
        (forward-line (- line 2))))))

(defun pyim-dict-manager-dict-position-down ()
  "向下移动词库。"
  (interactive)
  (when (equal (buffer-name) pyim-dict-manager--buffer)
    (let* ((id (get-text-property (point) 'id))
           (dict1 (nth (- id 1) pyim-dicts))
           (dict2 (nth id pyim-dicts))
           (length (length pyim-dicts))
           (line (line-number-at-pos)))
      (when (< id length)
        (setf (nth (1- id) pyim-dicts) dict2)
        (setf (nth id pyim-dicts) dict1)
        (pyim-dict-manager-refresh)
        (goto-char (point-min))
        (forward-line line)))))

(defun pyim-dict-manager-save-dict-info ()
  "使用 `customize-save-variable' 函数将 `pyim-dicts' 保存到 '~/.emacs' 文件中。"
  (interactive)
  ;; 将`pyim-dict'的设置保存到emacs配置文件中。
  (customize-save-variable 'pyim-dicts pyim-dicts)
  (message "将 pyim 词库配置信息保存到 ~/.emacs 文件。"))

(defun pyim-dict-manager-add-dict ()
  "为 `pyim-dicts' 添加词库信息。"
  (interactive)
  (when (equal (buffer-name) pyim-dict-manager--buffer)
    (let ((line (line-number-at-pos))
          dict name file first-used)
      (setq name (read-from-minibuffer "请输入词库名称： "))
      (setq file (read-file-name "请选择词库文件： " "~/"))
      (setq first-used  (yes-or-no-p "是否让 pyim 优先使用词库？ "))
      (setq dict `(:name ,name :file ,file))
      (if first-used
          (add-to-list 'pyim-dicts dict)
        (add-to-list 'pyim-dicts dict t))
      (pyim-dict-manager-refresh)
      (goto-char (point-min))
      (forward-line (- line 1)))))

(declare-function pyim-restart "pyim")

(define-derived-mode pyim-dict-manager-mode special-mode "pyim-dicts-manager"
  "Major mode for managing pyim dicts"
  (read-only-mode)
  (define-key pyim-dict-manager-mode-map (kbd "D") #'pyim-dict-manager-delete-dict)
  (define-key pyim-dict-manager-mode-map (kbd "g") #'pyim-dict-manager-refresh)
  (define-key pyim-dict-manager-mode-map (kbd "A") #'pyim-dict-manager-add-dict)
  (define-key pyim-dict-manager-mode-map (kbd "N") #'pyim-dict-manager-dict-position-down)
  (define-key pyim-dict-manager-mode-map (kbd "P") #'pyim-dict-manager-dict-position-up)
  (define-key pyim-dict-manager-mode-map (kbd "s") #'pyim-dict-manager-save-dict-info)
  (define-key pyim-dict-manager-mode-map (kbd "C-c C-c") #'pyim-dict-manager-toggle-dict)
  (define-key pyim-dict-manager-mode-map (kbd "R") #'pyim-restart))

;;;###autoload
(defun pyim-dicts-manager ()
  "pyim 词库管理器。

使用这个词库管理器可以方便的执行下列命令：
1. 添加词库。
2. 删除词库。
3. 向上和向下移动词库。
4. 保存词库设置。
5. 重启输入法。"
  (interactive)
  (let ((buffer (get-buffer-create pyim-dict-manager--buffer)))
    (pyim-dict-manager-refresh)
    (switch-to-buffer buffer)
    (pyim-dict-manager-mode)
    (setq truncate-lines t)))

;; * Footer
(provide 'pyim-dict-manager)

;;; pyim-dict-manager.el ends here
