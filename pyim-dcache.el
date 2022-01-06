;;; pyim-dcache.el --- dcache tools for pyim.        -*- lexical-binding: t; -*-

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
(require 'pyim-dict)
(require 'url-util)

(defgroup pyim-dcache nil
  "Dcache for pyim."
  :group 'pyim)

(defcustom pyim-dcache-directory (locate-user-emacs-file "pyim/dcache/")
  "一个目录，用于保存 pyim 词库对应的 cache 文件."
  :type 'directory
  :group 'pyim)

(defcustom pyim-dcache-backend 'pyim-dhashcache
  "词库后端引擎.负责缓冲词库并提供搜索词的算法.
可选项为 `pyim-dhashcache' 或 `pyim-dregcache'.
前者搜索单词速度很快,消耗内存多.  后者搜索单词速度较快,消耗内存少.

`pyim-dregcache' 速度和词库大小成正比.  当词库接近100M大小时,
在六年历史的笔记本上会有一秒的延迟. 这时建议换用 `pyim-dhashcache'.

注意：`pyim-dregcache' 只支持全拼和双拼输入法，不支持其它型码输入法."
  :type 'symbol)

(defvar pyim-dcache-auto-update t
  "是否自动创建和更新词库对应的 dcache 文件.

这个变量默认设置为 t, 如果有词库文件添加到 `pyim-dicts' 或者
`pyim-extra-dicts' 时，pyim 会自动生成相关的 dcache 文件。

一般不建议将这个变量设置为 nil，除非有以下情况：

1. 用户的词库已经非常稳定，并且想通过禁用这个功能来降低
pyim 对资源的消耗。
2. 自动更新功能无法正常工作，用户通过手工从其他机器上拷贝
dcache 文件的方法让 pyim 正常工作。")

;; ** Dcache API 调用功能
(defun pyim-dcache-call-api (api-name &rest api-args)
  "Get backend API named API-NAME then call it with arguments API-ARGS."
  ;; make sure the backend is load
  (unless (featurep pyim-dcache-backend)
    (require pyim-dcache-backend))
  (let ((func (intern (concat (symbol-name pyim-dcache-backend)
                              "-" (symbol-name api-name)))))
    (if (functionp func)
        (apply func api-args)
      (when pyim-debug
        (message "%S 不是一个有效的 dcache api 函数." (symbol-name func))
        ;; Need to return nil
        nil))))

;; ** Dcache 变量处理相关功能
(defun pyim-dcache-init-variables ()
  "初始化 dcache 缓存相关变量."
  (pyim-dcache-call-api 'init-variables))

(defmacro pyim-dcache-init-variable (variable &optional fallback-value)
  "初始化 VARIABLE.

如果 VARIABLE 的值为 nil, 则使用 ‘pyim-dcache-directory’ 中对应文
件的内容来设置 VARIABLE 变量, 如果此时 VARIABLE 取值还是 nil, 那
么就将 VARIABLE 的值设置为 FALLBACK-VALUE."
  `(when (and (symbolp ',variable) (not ,variable))
     (setq ,variable (or (pyim-dcache-get-value ',variable)
                         ,fallback-value
                         (make-hash-table :test #'equal)))))

(defmacro pyim-dcache-reload-variable (variable)
  "从 `pyim-dcache-directory' 重新读取并设置 VARIABLE 的值."
  `(when (symbolp ',variable)
     (setq ,variable (or (pyim-dcache-get-value ',variable)
                         (make-hash-table :test #'equal)))))

(defun pyim-dcache-get-value (variable)
  "从 `pyim-dcache-directory' 中读取与 VARIABLE 对应的文件中保存的值."
  (let ((file (expand-file-name (url-hexify-string (symbol-name variable))
                                pyim-dcache-directory)))
    (pyim-dcache-get-value-from-file file)))

(defun pyim-dcache-save-variable (variable value &optional auto-backup-threshold)
  "将 VARIABLE 变量的取值保存到 `pyim-dcache-directory' 中对应文件中.

如果 VALUE 的长度小于先前保存值的长度的 AUTO-BACKUP-THRESHOLD 倍,
那么先前保存的值将自动备份到相应的备份文件。"
  (let ((file (expand-file-name (url-hexify-string (symbol-name variable))
                                pyim-dcache-directory)))
    (pyim-dcache-save-value-to-file value file auto-backup-threshold)))

(defun pyim-dcache-value-length (value)
  "获取 VALUE 的某个可以作为长度的值."
  (or (ignore-errors
        (if (hash-table-p value)
            (hash-table-count value)
          (length value)))
      0))

(defun pyim-dcache-save-value-to-file (value file &optional auto-backup-threshold)
  "将 VALUE 保存到 FILE 文件中.

如果 VALUE 的长度小于 FILE 中上次保存值的长度的
AUTO-BACKUP-THRESHOLD 倍, 那么原值将自动备份到 FILE 对应的备份文
件。"
  (make-directory (file-name-directory file) t)
  (let* ((backup-file (concat file "-backup-" (format-time-string "%Y%m%d%H%M%S")))
         (orig-value (pyim-dcache-get-value-from-file file))
         (orig-length (pyim-dcache-value-length orig-value))
         (length (pyim-dcache-value-length value)))
    (when (and (numberp auto-backup-threshold)
               (< length (* auto-backup-threshold orig-length)))
      (pyim-dcache-save-value-to-file orig-value backup-file)
      (message "PYIM: 生成备份文件 %S, 请检查原文件 %S 是否损坏！！！" backup-file file))
    (when value
      (with-temp-buffer
        (insert ";; -*- lisp-data -*-\n")
        (insert ";; Auto generated by `pyim-dhashcache-save-variable-to-file', don't edit it by hand!\n")
        (insert (format ";; Build time: %s\n\n" (current-time-string)))
        (insert (let ((print-level nil)
                      (print-circle t)
                      (print-length nil))
                  (prin1-to-string value)))
        (insert "\n\n")
        (insert ";; Local\sVariables:\n") ;Use \s to avoid a false positive!
        (insert ";; coding: utf-8-unix\n")
        (insert ";; End:")
        (pyim-dcache-write-file file)))))

(defun pyim-dcache-get-value-from-file (file)
  "读取保存到 FILE 里面的 value."
  (when (and (> (length file) 0)
             (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (ignore-errors
        (read (current-buffer))))))

;; ** Dcache 文件处理功能
(defun pyim-dcache-write-file (filename &optional confirm)
  "A helper function to write dcache files."
  (let ((coding-system-for-write 'utf-8-unix)
        (create-lockfiles nil))
    (when (and confirm
               (file-exists-p filename)
               ;; NS does its own confirm dialog.
               (not (and (eq (framep-on-display) 'ns)
                         (listp last-nonmenu-event)
                         use-dialog-box))
               (or (y-or-n-p (format-message
                              "File `%s' exists; overwrite? " filename))
                   (user-error "Canceled"))))
    (write-region (point-min) (point-max) filename nil :silent)
    (message "Saving file %s..." filename)))

(defun pyim-dcache-save-caches ()
  "保存 dcache.

  将用户选择过的词生成的缓存和词频缓存的取值
  保存到它们对应的文件中.

  这个函数默认作为 `kill-emacs-hook' 使用。"
  (interactive)
  (pyim-dcache-call-api 'save-personal-dcache-to-file)
  t)

;; ** Dcache 导出功能
(defun pyim-dcache-export-words-and-counts (file &optional confirm ignore-counts)
  "将个人词条以及词条对应的词频信息导出到文件 FILE.

如果 FILE 为 nil, 提示用户指定导出文件位置, 如果 CONFIRM 为
non-nil，文件存在时将会提示用户是否覆盖，默认为覆盖模式"
  (interactive "F将词条和词频信息导出到文件: ")
  (pyim-dcache-init-variables)
  (pyim-dcache-call-api 'export-words-and-counts file confirm ignore-counts)
  (message "PYIM: 词条和词频信息导出完成。"))

(defun pyim-dcache-export-personal-words (file &optional confirm)
  "将用户的个人词条导出为 pyim 词库文件.

如果 FILE 为 nil, 提示用户指定导出文件位置, 如果 CONFIRM 为 non-nil，
文件存在时将会提示用户是否覆盖，默认为覆盖模式。"
  (interactive "F将个人词条导出到文件：")
  (pyim-dcache-init-variables)
  (pyim-dcache-call-api 'export-personal-words file confirm)
  (message "PYIM: 个人词条导出完成。"))

;; ** Dcache 更新功能
(defun pyim-dcache-update (&optional force)
  "读取并加载所有相关词库 dcache.

如果 FORCE 为真，强制加载。"
  (pyim-dcache-init-variables)
  (when pyim-dcache-auto-update
    (pyim-dcache-call-api 'update-iword2priority force)
    (pyim-dcache-call-api 'update-personal-words force)
    (let* ((dict-files (mapcar (lambda (x)
                                 (unless (plist-get x :disable)
                                   (plist-get x :file)))
                               `(,@pyim-dicts ,@pyim-extra-dicts)))
           (dicts-md5 (pyim-dcache-create-dicts-md5 dict-files)))
      (pyim-dcache-call-api 'update-code2word dict-files dicts-md5 force))))

(defun pyim-dcache-create-dicts-md5 (dict-files)
  "为 DICT-FILES 生成 md5 字符串。"
  ;;当需要强制更新 dict 缓存时，更改这个字符串。
  (let ((version "v1"))
    (md5 (prin1-to-string
          (mapcar (lambda (file)
                    (list version file (nth 5 (file-attributes file 'string))))
                  dict-files)))))

(defun pyim-dcache-update-wordcount (word &optional wordcount-handler)
  "保存 WORD 词频.

1. 如果 WORDCOUNT-HANDLER 是一个函数：那么其返回值将作为词频保存，
   参数为原有词频。
2. 如果 WORDCOUNT-HANDLER 是一个数值：那么这个数值直接作为词频保存。
3. 如果 WORDCOUNT-HANDLER 为其他值：词频不变."
  (pyim-dcache-call-api 'update-iword2count word wordcount-handler))

;; ** Dcache 加词功能
(defun pyim-dcache-insert-word (word code prepend)
  "将词条 WORD 插入到 dcache 中。

如果 PREPEND 为 non-nil, 词条将放到已有词条的最前面。
内部函数会根据 CODE 来确定插入对应的 hash key."
  (pyim-dcache-call-api 'insert-word-into-icode2word word code prepend)
  ;; NOTE: 保存词条到 icode2word 词库缓存的同时，也在 ishortcode2word 词库缓存中
  ;; 临时写入一份，供当前 Emacs session 使用，但退出时 pyim 不会保存
  ;; ishortcode2word 词库缓存到文件，因为下次启动 Emacs 的时候，ishortcode2word
  ;; 词库缓存会从 icode2word 再次重建。
  (pyim-dcache-call-api 'insert-word-into-ishortcode2word word code prepend))

;; ** Dcache 升级功能
(defun pyim-dcache-upgrade ()
  "升级词库缓存.

当前已有的功能：
1. 基于 :code-prefix-history 信息，升级为新的 code-prefix。"
  (interactive)
  (pyim-dcache-call-api 'upgrade-icode2word))

;; ** Dcache 删词功能
(defun pyim-dcache-delete-word (word)
  "将中文词条 WORD 从个人词库中删除"
  (pyim-dcache-call-api 'delete-word word))

;; ** Dcache 检索功能
(defun pyim-dcache-get (code &optional from)
  "从 FROM 对应的 dcache 中搜索 CODE, 得到对应的词条.

当词库文件加载完成后，pyim 就可以用这个函数从词库缓存中搜索某个
code 对应的中文词条了."
  `(,@(pyim-dcache-call-api 'get code from)
    ,@(pyim-pymap-py2cchar-get code t t)))

;; * Footer
(provide 'pyim-dcache)

;;; pyim-dcache.el ends here
