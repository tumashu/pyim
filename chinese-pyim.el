;;; chinese-pyim.el --- Chinese pinyin input method

;; Copyright 2006 Ye Wenbin
;;           2014-2015 Feng Shu
;;
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

;;; Commentary:
;;
;; # 简介 #
;; Chinese-pyim 是 Chinese Pinyin Input Method 的缩写。是emacs环境下的一个中文拼音输入法。
;;
;; # 背景 #
;; Chinese-pyim 的代码源自 emacs-eim。emacs-eim是一个emacs中文输入法框架，使用这
;; 个框架可以自定义输入法，emacs-eim 软件包本身就包含许多不同的中文输入法，比如：
;; 五笔输入法，仓颉输入法以及二笔输入法等等。
;;
;; 虽然 emacs-eim 很优秀。但遗憾的是，emacs-eim 并没有发展起来，因为：中文输入法的
;; 发展(尤其是拼音输入法)特别依赖庞大的用户群体直接或者间接的提供优化输入法的信息。
;; emacs 中文用户群体很难达到这种要求。另外各种平台下的中文输入法的高速发展，比如：
;; Window平台下的搜狗输入法，baidu输入法以及QQ输入法，linux平台下的fcitx，ibus等等。
;; 也间接的抹杀了 emacs-eim 的开发动力，最终的结果就是：chinese-eim 在 2008 年之后
;; 几乎停止了开发。
;;
;; 但是，外部的输入法与emacs配合不够默契，不断的切换输入法极大的损害了emacs那种
;; *行云流水* 的体验。而本人在使用（或者叫折腾） emacs-eim 的过程中发现：
;;
;; 1. *当 emacs-eim 拼音词库词条超过100万时，选词频率大大降低。*
;; 2. *当 emacs-eim 拼音词库词条超过100万时，中文输入体验可以达到搜狗输入法的80%。*
;; 3. *随着使用时间的延长，emacs-eim会越来越好用（个人词库的积累）。*
;;
;; 所以，本人认为将 Chinese-eim 作为一个 *备用* 中文输入法是非常合适的，于是我 fork 了
;; emacs-eim, 简化代码并更改名称为：chinese-pyim。
;;
;; # 目标 #
;; Chinese-pyim 的目标是：*尽最大的努力成为一个好用的 emacs 备用中文拼音输入法*。
;; 具体可表现为三个方面：
;;
;; 1. Fallback:     当外部输入法不能使用时（比如：console，cygwin等），尽最大可能让 emacs 用户不必为输入中文而烦恼。
;; 2. Integration:  尽最大可能减少输入法切换频率，让中文输入不影响 emacs 的体验。
;; 3. Exchange:     尽最大可能简化 Chinese-pyim 使用其他优秀输入法的词库的难度和复杂度。
;;
;; # 特点 #
;; 1. Chinese-pyim 只是一个拼音输入法，安装配置方便快捷，默认只通过添加词库的方式优化输入法。
;; 2. Chinese-pyim 只使用最简单的文本词库格式，可以快速方便的利用其他输入法的词库。
;;
;; # 安装 #
;; 1. 配置melpa源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET chinese-pyim RET
;; 3. 在emacs配置文件中（比如: ~/.emacs）添加如下代码：
;;
;; ```lisp
;; (require 'chinese-pyim)
;; ```
;; # 配置 #
;;
;; ## 添加拼音词库 ##
;; Chinese-pyim 默认没有携带任何拼音词库，如果不配置拼音词库，Chinese-pyim将不能正常工作。
;; 这样做的原因有两个：
;;
;; 1. 防止侵犯其他输入法的版权。
;; 2. 防止自带词库质量太差，影响用户体验。
;;
;; 用户可以使用下面两种方式，简单的获取质量比较好的词库：
;;
;; ### 第一种方式 ###
;;
;; 获取其他 Chinese-pyim 用户的拼音词库，比如，某个同学测试 Chinese-pyim 时创建了一个
;; 中文拼音词库，词条数量大于100万，文件大小大于20M。
;;
;; https://github.com/tumashu/chinese-pyim-bigdict/blob/master/pyim-bigdict.txt?raw=true
;;
;; 其他同学可以下载上述词库来体验一下超大词库为 Chinese-pyim 带来的巨大变化。
;;
;; 下载上述词库后，运行 `pyim-add-dict' ，按照命令提示，将下载得到的词库文件信息添加
;; 到 `pyim-dicts' 中，最后运行命令 `pyim-restart' 或者重启emacs。
;;
;; ### 第二种方式 ###
;;
;; 使用词库转换工具将其他输入法的词库转化为Chinese-pyim使用的词库：这里只介绍windows平
;; 台下的一个词库转换软件：
;;
;; 1. 软件名称： "imewlconverter"
;; 2. 中文名称：“深蓝词库转换”。
;; 3. 下载地址： http://code.google.com/p/imewlconverter/
;; 4. 依赖平台:  "Microsoft .NET Framework 2.0"
;;
;; 首先从其他拼音输入法网站上获取所需词库，使用下述自定义输出格式转换词库文件，然后将转
;; 换得到的内容保存到文件中。
;;
;;         shen,lan,ci,ku 深蓝词库
;;
;; 将文件中所有","替换为"-"，得到的文件每一行都类似：
;;
;;         shen-lan-ci-ku 深蓝词库
;;
;; 最后，使用命令 `pyim-add-dict' ，将转换得到的词库文件的信息添加到 `pyim-dicts' 中，
;; 完成后运行命令 `pyim-restart' 或者重启emacs。
;;
;; 注意：每一个词库文件必须按行排序（准确的说，是按每一行的拼音code排序），
;; 因为`Chinese-pyim' 寻找词条时，使用二分法来优化速度，而二分法工作的前提
;; 就是对文件按行排序。具体细节请参考：`pyim-bisearch-word' 。
;; 所以，当词库排序不正确时（比如：用户手动调整词库文件后），记得运行函数
;; `pyim-update-table' 重新对文件排序。
;;
;; ## 激活 Chinese-pyim ##
;;
;; ```lisp
;; (setq default-input-method "chinese-pyim")
;; (global-set-key (kbd "C-<SPC>") 'toggle-input-method)
;; ;; (global-set-key (kbd "C-;") 'pyim-insert-ascii)
;; ;; (global-set-key (kbd "C-;") 'pyim-toggle-full-width-punctuation)
;; ;; (global-set-key (kbd "C-;") 'pyim-punctuation-translate-at-point)
;;
;; ```
;; 切换全角半角标点符号使用命令: M-x pyim-toggle-full-width-punctuation
;;
;; Chinese-pyim 使用一个比较 *粗糙* 的方法处理 *模糊音*，要了解具体细节，请
;; 运行： C-h v pyim-pinyin-fuzzy-adjust-function
;;
;; # 如何手动安装和管理词库 #
;; 这里假设有两个词库文件：
;;
;; 1. /path/to/pyim-dict1.txt
;; 2. /path/to/pyim-dict2.txt
;;
;; 在~/.emacs文件中添加如下一行配置。
;;
;; ```lisp
;; (setq pyim-dicts
;;       '((:name "dict1" :file "/path/to/pyim-dict1.txt" :coding gbk-dos)
;;         (:name "dict2" :file "/path/to/pyim-dict2.txt" :coding gbk-dos)))
;; ```
;;
;; # 其他 #
;;
;; 1. 了解 Chinese-pyim 个人词频文件设置的细节：C-h v pyim-personal-file
;; 2. 了解 Chinese-pyim 词库设置细节：C-h v pyim-dicts
;; 3. 下面两个函数可以将汉字字符串转换为拼音字符串。
;;    1. `pyim-hanzi2pinyin' （考虑多音字）
;;    2. `pyim-hanzi2pinyin-simple'  （不考虑多音字）
;; 4. 可以定义类似下面的命令来实现快速切换词库的功能。
;; ```lisp
;; (defun pyim-use-dict:bigdict ()
;;   (interactive)
;;   (setq pyim-dicts
;;         '((:name "BigDict"
;;                  :file "/path/to/pyim-bigdict.txt"
;;                  :coding utf-8-unix)))
;;   (pyim-restart-1 t))
;; ```

;;; Code:
(require 'cl-lib)
(require 'help-mode)

(defgroup chinese-pyim nil
  "Chinese pinyin input method"
  :group 'leim)

(defcustom pyim-personal-file (locate-user-emacs-file "pyim/pyim-personal.txt")
  "这个文件用来保存用户曾经输入过的中文词条，和这些词条输入的先后顺序。

随着 `Chinese-pyim' 使用时间的延长，这个文件会保存越来越多的用户个人常用的词条，
相应的 `Chinese-pyim' 也会越来越顺手，所以：建议用户做好备份，同时也提醒用户注
意保护隐私，不要随意将这个文件泄露他人。

这个文件的格式与 `Chinese-pyim' 词库的格式完全一致，`Chinese-pyim' 使用与词库一样
的方法处理这个文件，同时，当输入法搜索词条时，这个文件里的词条最先使用。但是，这个
文件不是让用户添加自定义词库的。因为：emacs关闭之前，`Chinese-pyim' 会自动
更新这个文件，将编辑过的内容覆盖，所以不建议普通用户手动编辑这个文件，
BUG：当用户错误的将这个变量设定为其他重要文件时，也存在文件内容破坏的风险。

如果用户需要手动为 `Chinese-pyim' 添加新词条，请使用其自带的词库功能，具体请参
考变量 `pyim-dicts'。

当这个文件中的词条数量增长到一定程度，用户可以直接将这个文件转换为词库。
"
  :group 'chinese-pyim
  :type 'file)

(defcustom pyim-dicts nil
  "一个列表，用于保存 `Chinese-pyim' 的词库信息，每一个 element 都代表一条词库的信息。
用户可以使用 `pyim-add-dict' 命令来添加词库信息，每一条词库信息都使用一个 plist 来
表示，比如：

    (:name \"100万大词库\"
     :file \"/path/to/pinyin-bigdict.txt\"
     :coding utf-8-unix)

其中：
1. `:name'   代表词库名称，用户可以按照喜好来确定。
2. `:coding' 表示词库文件使用的编码。
3. `:file'   表示词库文件，

每一个词库文件都是简单的文本文件。文件每一行都类似：

    ni-hao 你好 拟好

第一个空格之前的内容为code（拼音），第一个空格之后为中文词条列表。
`Chinese-pyim' 词库不使用其他特殊格式，词库也不处理中文标点符号。

但要注意：词库文件必须按行排序（准确的说，是按每一行的 code 排序），因为
`Chinese-pyim' 寻找词条时，使用二分法来优化速度，而二分法工作的前提就是对
文件按行排序。具体细节请参考：`pyim-bisearch-word' 。当用户手动调整词库文
件后，记得运行 `pyim-update-table' 来对文件排序。"
  :group 'chinese-pyim
  :type 'list)

(defcustom pyim-punctuation-dict
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
    ("{" "『"))
  "标点符号表。"
  :group 'chinese-pyim
  :type 'list)

(defcustom pyim-pinyin-fuzzy-adjust-function
  'pyim-pinyin-fuzzy-adjust-1
  "Chinese-pyim的核心并不能处理模糊音，这里提供了一个比较
 *粗糙* 的方法来处理模糊音。

假如：用户输入了一个错误的拼音“ying-gai”，用户可以通过快
捷键运行一个函数，将“ing” 替换 “in”，得到 “yin-gai”
对应的词语。

这种处理方式能力有限，一次不能处理太多的模糊音，用户需要根据
自己的需要，自定义模糊音处理函数。

模糊音处理函数可以参考：`pyim-pinyin-fuzzy-adjust-1'
"
  :group 'chinese-pyim
  :type 'function)

(defcustom pyim-page-length 9
  "每页显示的词条数目"
  :group 'chinese-pyim
  :type 'number)

(defface pyim-string-face '((t (:underline t)))
  "Face to show current string"
  :group 'chinese-pyim)

;;;  variable declare
(defvar pyim-info (make-vector 9 nil)
  "拼音输入法运行时需要的信息，一个 vector，有五个部分:
1. name
2. buffer-list
   1. 每个 buffer 都是一个 assoc list。
   2. 每一个 assoc list 都包含两个部份：
      1. buffer 词库文件导入时创建的 buffer (用户不可见)。
      2. file:  词库文件的路径。
3. history
4. keymap
5. active-function.
")
(defvar pyim-do-completion t "是否读入可能的补全")

(defvar pyim-current-key "" "已经输入的代码")
(defvar pyim-current-str "" "当前选择的词条")
(defvar pyim-current-choices nil
  "
所有可选的词条，是一个list。
1. CAR 部份是可选的词条，一般是一个字符串列表。
   也可以含有list。但是包含的list第一个元素必须是将要插入的字符串。
2. CDR 部分是一个 Association list。通常含有这样的内容：
   1. pos 上次选择的位置
   2. completion 下一个可能的字母（如果 pyim-do-completion 为 t）
")

(defvar pyim-current-pos nil "当前选择的词条在 pyim-current-choices 中的位置")
(defvar pyim-guidance-str "" "显示可选词条的字符串")
(defvar pyim-translating nil "记录是否在转换状态")
(defvar pyim-overlay nil "显示当前选择词条的 overlay")
(defvar pyim-guidance-frame nil)
(defvar pyim-guidance-buf nil)

(defvar pyim-load-hook nil)
(defvar pyim-active-hook nil)

(defvar pyim-stop-function nil)
(defvar pyim-translate-function nil)
(defvar pyim-add-completion-function nil)
(defvar pyim-format-function 'pyim-format)
(defvar pyim-handle-function 'pyim-handle-string)
(defvar pyim-buffer-name-format " *%s*"
  "buffer 的名字格式，%s 对应 package name")
(defvar pyim-activated-p nil)

(defvar pyim-punctuation-escape-list (number-sequence ?0 ?9)
  "Punctuation will not insert after this characters.
If you don't like this funciton, set the variable to nil")

(defvar pyim-punctuation-translate-p t
  "*Non-nil means will translate punctuation.")

(defvar pyim-local-variable-list
  '(pyim-page-length
    pyim-do-completion

    pyim-current-key
    pyim-current-str
    pyim-current-choices
    pyim-current-pos
    pyim-guidance-str
    pyim-translating
    pyim-overlay
    pyim-guidance-frame
    pyim-guidance-buf

    pyim-load-hook
    pyim-active-hook

    pyim-translate-function
    pyim-format-function
    pyim-handle-function
    pyim-add-completion-function
    pyim-stop-function

    input-method-function
    inactivate-current-input-method-function
    describe-current-input-method-function)
  "A list of buffer local variable")

(dolist (var pyim-local-variable-list)
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

;;;  package contents
(defsubst pyim-name ()
  (aref pyim-info 0))

(defsubst pyim-buffer-list ()
  (aref pyim-info 1))

(defsubst pyim-history ()
  "
保存输入过的词的选择
1. 加快搜索。
2. 处理标点。

每个元素都有这样的格式：

   ((list WORDS) other-properties)

其中：OTHER-PROPERTIES 是一些其它的属性，
比如，上次的位置，用来输入标点等。"
  (aref pyim-info 2))

(defsubst pyim-mode-map ()
  (aref pyim-info 3))

(defsubst pyim-options ()
  (aref pyim-info 4))

(defsubst pyim-active-function ()
  (aref pyim-info 5))

(defsubst pyim-set-name (name)
  (aset pyim-info 0 name))

(defsubst pyim-set-buffer-list (list)
  (aset pyim-info 1 list))

(defsubst pyim-set-history (history)
  (aset pyim-info 2 history))

(defsubst pyim-set-mode-map (map)
  (aset pyim-info 3 map))

(defsubst pyim-set-options (options)
  (aset pyim-info 4 options))

(defsubst pyim-set-active-function (func)
  (aset pyim-info 5 func))

(defun pyim-get-option (option)
  (cdr (assoc option (pyim-options))))

(defun pyim-set-option (option flag)
  (let ((options (pyim-options))
        opt)
    (if (setq opt (assoc option options))
        (setcdr opt flag)
      (push (cons option flag) options)
      (pyim-set-options options))))

(defun pyim-create-template-dict (file)
  "生成模版词库。"
  (condition-case error
      (unless (file-exists-p file)
        (with-temp-buffer
          (erase-buffer)
          (insert ";; -*- coding: utf-8 -*-\n")
          (make-directory (file-name-directory file) t)
          (write-file (expand-file-name file))
          (message "自动创建 Chinese-pyim 文件: %s" file)))
    (error
     (warn "`Chinese-pyim' 模版词库创建失败！" ))))

(defun pyim-dict-name-available-p (dict-name)
  "查询 `pyim-dicts' 中 `:name' 为 `dict-name' 的词库信息是否存在。
这个函数主要用于词库 package。"
  (cl-some (lambda (x)
             (let ((name (plist-get x :name)))
               (string= name dict-name)))
           pyim-dicts))

(defun pyim-dict-file-available-p (dict-file)
  "查询 `pyim-dicts' 中 `:file' 为 `dict-file' 的词库信息是否存在。
这个函数主要用于词库 package。"
  (cl-some (lambda (x)
             (let ((file (plist-get x :file)))
               (string= (expand-file-name file)
                        (expand-file-name dict-file))))
           pyim-dicts))

(defun pyim-add-dict ()
  "为 `pyim-dicts' 添加词库信息，然后 `pyim-dicts' 将通过
`customize-save-variable' 函数保存到用户emacs配置中"
  (interactive)
  (let (dict name file coding first-used)
    (setq name (read-from-minibuffer "请输入词库名称： "))
    (setq file (ido-read-file-name "请选择词库文件： " "~/"))
    (setq coding (ido-completing-read "词库文件编码: " '("utf-8-unix" "cjk-dos")))
    (setq first-used  (yes-or-no-p "是否让 Chinese-pyim 优先使用词库？ "))
    (setq dict `(:name ,name :file ,file :coding ,(intern coding)))
    (if first-used
        (add-to-list 'pyim-dicts dict)
      (add-to-list 'pyim-dicts dict t))
    ;; 将`pyim-dict'的设置保存到emacs配置文件中。
    (customize-save-variable 'pyim-dicts pyim-dicts)
    (message "添加并保存 Chinese-pyim 输入法词库: (%s)，重启 emacs 后生效！" name)))

(defun pyim-show-dict-help ()
  "显示 Chinese-pyim 帮助信息，让用户快速的了解如何安装词库。"
  (let ((buffer-name "*Chinese-pyim-dict-help*"))
    (with-output-to-temp-buffer buffer-name
      (set-buffer buffer-name)
      (when (featurep 'org)
        (org-mode))
      (setq truncate-lines 1)
      (insert "Chinese-pyim 没有可用词库！！！

拼音词库是 Chinese-pyim 使用顺手与否的关键。根据经验估计：
1. 当词库词条超过100万时(词库文件>20M)，Chinese-pyim 选词频率大大降低。
2. 当词库词条超过100万时，Chinese-pyim 中文输入体验可以达到搜狗输入法的80%。

赶时间的朋友可以直接下载其他 Chinese-pyim 用户现成的拼音词库，比如，某个同学
自己使用的词库：BigDict，这个词库词条数量大于100万，文件大小大于20M，可以显著
增强 Chinese-pyim 的输入体验。

  https://github.com/tumashu/chinese-pyim-bigdict/blob/master/pyim-bigdict.txt?raw=true

下载上述拼音词库后，运行 `pyim-add-dict' ，按照命令提示，将词库文件信息添加到
 `pyim-dicts' 中，最后运行命令 `pyim-restart' 或者重启emacs。。

喜欢折腾的用户可以从下面几个途径获得 Chinese-pyim 更详细的信息。
1. 使用 `C-h v pyim-dicts' 了解 `Chinese-pyim' 词库文件格式，
2. 了解如何导入其它输入法的词库。
   1. 使用 package 管理器查看 Chinese-pyim 包的简介
   2. 阅读 chinese-pyim.el 文件 Commentary
   3. 查看 Chinese-pyim 在线 README：https://github.com/tumashu/chinese-pyim")
      (goto-char (point-min)))))

;;;  read file functions
(defun pyim-load-file ()
  "为每一个词库文件创建一个buffer(这些buffer用户不可见)，然后将各个词库文件的内容插入
与之对应的buffer。最后返回一个包含所有buffer对象以及词库文件名的alist。

`pyim-personal-file' 文件最先导入。然后按照先后顺序导入 `pyim-dicts' 中定义的词库
排在最前面的词库首先被加载，相同的词库文件只加载一次。
"
  (let ((personal-file (expand-file-name pyim-personal-file))
        (dicts-list pyim-dicts)
        (bufname (format pyim-buffer-name-format (pyim-name)))
        buflist buf file coding)
    (save-excursion
      (unless (file-exists-p personal-file)
        ;; 如果 `pyim-personal-file' 对应的文件不存在，
        ;; 创建一个模版文件。
        (pyim-create-template-dict personal-file))
      (setq buf (pyim-read-file personal-file bufname))
      (setq buflist (append buflist (list buf)))
      (if dicts-list
          (dolist (dict dicts-list)
            (cond
             ((and (listp dict) (plist-get dict :file))
              (setq file (expand-file-name (plist-get dict :file)))
              (setq coding (plist-get dict :coding))
              (if (and (file-exists-p file)
                       (not (pyim-file-load-p file buflist)))
                  (setq buflist (append buflist (list (pyim-read-file file bufname coding))))
                (message "忽略导入重复的词库文件：%s。" file)))
             ((stringp dict)
              (setq file (expand-file-name dict))
              (if (and (file-exists-p file)
                       (not (pyim-file-load-p file buflist)))
                  (setq buflist (append buflist (list (pyim-read-file file bufname))))
                (message "忽略导入重复的词库文件：%s。" file)))))
        ;; 当用户没有设置词库信息时，弹出帮助信息。
        (pyim-show-dict-help)))
    buflist))

(defun pyim-file-load-p (file buflist)
  "判断 file 是否已经加载"
  (cl-some (lambda (x)
             (rassoc file x))
           buflist))

(defun pyim-read-file (file name &optional coding)
  (with-current-buffer (generate-new-buffer name)
    (if coding
        (let ((coding-system-for-read coding))
          (insert-file-contents file))
      (insert-file-contents file))
    `(("buffer" . ,(current-buffer))
      ("file" . ,file))))

(defun pyim-save-personal-file ()
  "与 `pyim-personal-file' 文件对应的buffer在 `Chinese-pyim' 使用期间不断更新。
这个函数将更新后的内容保存到`pyim-personal-file' 文件中，

这个函数默认作为`kill-emacs-hook'使用。"
  (interactive)
  (let* ((buffer (car (pyim-buffer-list)))
         (file (cdr (assoc "file" buffer))))
    (with-current-buffer (cdr (assoc "buffer" buffer))
      (save-restriction
        (if (file-exists-p file)
            (progn (write-region (point-min) (point-max) file)
                   (message "更新 Chinese-pyim 文件：%s。" file))
          (message "Chinese-pyim 文件：%s 不存在。" file))))))

;;;  common functions
(defsubst pyim-delete-region ()
  "Delete the text in the current translation region of E+."
  (if (overlay-start pyim-overlay)
      (delete-region (overlay-start pyim-overlay)
                     (overlay-end pyim-overlay))))

;;; steal from emms-compat.el. Is this a good idea?
(when (not (fboundp 'emms-delete-if))
  (defun emms-delete-if (predicate seq)
    "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function: it reuses the storage of SEQ
whenever possible."
    ;; remove from car
    (while (when (funcall predicate (car seq))
             (setq seq (cdr seq))))
    ;; remove from cdr
    (let ((ptr seq)
          (next (cdr seq)))
      (while next
        (when (funcall predicate (car next))
          (setcdr ptr (if (consp next)
                          (cdr next)
                        nil)))
        (setq ptr (cdr ptr))
        (setq next (cdr ptr))))
    seq))

(defun pyim-subseq (list from &optional to)
  (if (null to) (nthcdr from list)
    (butlast (nthcdr from list) (- (length list) to))))

(defun pyim-mod (x y)
  "like `mod', but when result is 0, return Y"
  (let ((base (mod x y)))
    (if (= base 0)
        y
      base)))

(defun pyim-string-emptyp (str)
  (not (string< "" str)))

(defun pyim-line-content (&optional seperaters omit-nulls)
  "用 SEPERATERS 分解当前行，所有参数传递给 split-string 函数"
  (let ((items   (split-string
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)) seperaters)))
    (if omit-nulls
        (emms-delete-if 'pyim-string-emptyp items)
      items)))

(defsubst pyim-delete-line ()
  (delete-region (line-beginning-position) (min (+ (line-end-position) 1)
                                                (point-max))))

(defsubst pyim-append-string (str)
  "append STR to pyim-current-str"
  (setq pyim-current-str (concat pyim-current-str str)))

;;;  code search
(defun pyim-get (code)
  (when (and (stringp code) (not (pyim-string-emptyp code)))
    (let ((history (gethash code (pyim-history)))
          pos words completions)
      (if (and (car history) (assoc "completions" (cdr history)))
          history
        (dolist (buf (pyim-buffer-list))
          (with-current-buffer (cdr (assoc "buffer" buf))
            ;; Chinese-pyim 首次运行时，personal-file 文件为空文件，
            ;; 其对应的 buffer 为空，这里确保 buffer 包含有效的词库
            ;; 信息时才搜索所需的词条。
            (when (pyim-dict-buffer-valid-p)
              (setq words (append words
                                  (cdr
                                   (pyim-bisearch-word code
                                                       (point-min)
                                                       (point-max)))))
              (if pyim-do-completion
                  (setq completions (pyim-completions code completions))))))
        (setq words (delete-dups words))
        (puthash code (list words
                            (cons "pos" (or (cdr (assoc "pos" (cdr history))) 1))
                            (cons "completions" completions))
                 (pyim-history))))))

(defun pyim-completions (code completions)
  (let ((maxln 200)
        (cnt 0)
        (len (length code))
        (reg (concat "^" (regexp-quote code))))
    (save-excursion
      (forward-line 1)
      (while (and (looking-at reg)
                  (< cnt maxln))
        (add-to-list 'completions (buffer-substring-no-properties
                                   (+ (point) len)
                                   (+ (point) len 1)))
        (forward-line 1)
        (setq cnt (1+ cnt)))
      completions)))

(defun pyim-dict-buffer-valid-p ()
  "粗略地确定当前 buffer 是否是一个有效的词库产生的 buffer。
确定标准：

1. buffer 必须多于5行。
2. buffer 中间一行必须包含空格或者TAB。
2. buffer 中间一行必须包含中文字符(\\cc)。

BUG: 这个函数需要进一步优化，使其判断更准确。"
  (when (> (count-lines (point-min) (point-max)) 5)
    (save-excursion
      (let ((mid (/ (+ (point-min) (point-max)) 2))
            ccode)
        (goto-char mid)
        (beginning-of-line)
        (and (re-search-forward "[ \t]" (line-end-position) t)
             (re-search-forward "\\cc" (line-end-position) t))))))

(defun pyim-bisearch-word (code start end)
  (let ((mid (/ (+ start end) 2))
        ccode)
    (goto-char mid)
    (beginning-of-line)
    (setq ccode (pyim-code-at-point))
    ;;    (message "%d, %d, %d: %s" start mid end ccode)
    (if (string= ccode code)
        (pyim-line-content)
      (if (> mid start)
          (if (string< ccode code)
              (pyim-bisearch-word code mid end)
            (pyim-bisearch-word code start mid))))))

(defun pyim-code-at-point ()
  "Before calling this function, be sure that the point is at the
beginning of line"
  (save-excursion
    (if (re-search-forward "[ \t]" (line-end-position) t)
        (buffer-substring-no-properties (line-beginning-position) (1- (point)))
      (error "文件类型错误！%s 的第 %d 行没有词条！" (buffer-name) (line-number-at-pos)))))

(defun pyim-delete-duplicate-word ()
  (interactive)
  (let* ((words-list1 (pyim-line-content " "))
         (length (length words-list1))
         (words-list2 (delete-dups words-list1)))
    (when (> length (length words-list2))
      (pyim-delete-line)
      (insert (mapconcat 'identity words-list2 " "))
      (insert "\n")
      (goto-char (line-beginning-position)))))

;;;  interface
(defun pyim-check-buffers ()
  "检查所有的 buffer 是否还存在，如果不存在，重新打开文件，如果文件不
存在，从 buffer-list 中删除这个 buffer"
  (let ((buflist (pyim-buffer-list))
        (bufname (pyim-name))
        buffer file)
    (dolist (buf buflist)
      (setq buffer (assoc "buffer" buf))
      (setq file (cdr (assoc "file" buf)))
      (unless (buffer-live-p (cdr buffer))
        (if (file-exists-p file)
            (with-current-buffer (generate-new-buffer
                                  (format pyim-buffer-name-format bufname))
              (insert-file-contents file)
              (setcdr buffer (current-buffer)))
          (message "%s for %s is not exists!" file bufname)
          (setq buflist (remove buf buflist)))))
    t))

(defun pyim-kill-buffers ()
  "删除所有词库文件对应的 buffer ，用于重启 Chinese-pyim 。"
  (let ((buflist (pyim-buffer-list))
        buffer)
    (dolist (buf buflist)
      (setq buffer (cdr (assoc "buffer" buf)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun pyim-inactivate ()
  (interactive)
  (mapc 'kill-local-variable pyim-local-variable-list))

;;;  page format
(defsubst pyim-choice (choice)
  (if (consp choice)
      (car choice)
    choice))

(defun pyim-add-completion ()
  "注意, pyim-add-completion-function 在没有完补全之前返回 nil, 在加完所
有补全之后一定要返回一个 t"
  (if (functionp pyim-add-completion-function)
      (funcall pyim-add-completion-function)
    t))

(defun pyim-format (key cp tp choice)
  (let ((i 0))
    (format "%s[%d/%d]: %s"
            key  cp tp
            (mapconcat 'identity
                       (mapcar
                        (lambda (c)
                          (format "%d.%s " (setq i (1+ i)) c))
                        choice) " "))))

(defun pyim-format-page ()
  "按当前位置，生成候选词条"
  (let ((end (pyim-page-end)))
    (if (car pyim-current-choices)
        (let* ((start (1- (pyim-page-start)))
               (choices (car pyim-current-choices))
               (choice (pyim-subseq choices start end))
               (pos (1- (min pyim-current-pos (length choices))))
               (i 0))
          (setq pyim-current-str (pyim-choice (nth pos choices)))
          (setq pyim-guidance-str
                (funcall pyim-format-function pyim-current-key (pyim-current-page)
                         (pyim-total-page) choice))
          ;; (message "%d, %s, %s" pos pyim-current-str pyim-guidance-str)
          (pyim-show))
      (setq pyim-current-str pyim-current-key)
      (setq pyim-guidance-str
            (concat pyim-current-key
                    (if (cdr (assoc "completions" (cdr pyim-current-choices)))
                        (format "[%s]: "
                                (mapconcat 'identity
                                           (cdr (assoc
                                                 "completions"
                                                 (cdr pyim-current-choices)))
                                           "")))))
      (pyim-show))))

(defun pyim-current-page ()
  (1+ (/ (1- pyim-current-pos) pyim-page-length)))

(defun pyim-total-page ()
  (1+ (/ (1- (length (car pyim-current-choices))) pyim-page-length)))

(defun pyim-page-start ()
  "计算当前所在页的第一个词条的位置"
  (let ((pos (min (length (car pyim-current-choices)) pyim-current-pos)))
    (1+ (- pos (pyim-mod pos pyim-page-length)))))

(defun pyim-page-end (&optional finish)
  "计算当前所在页的最后一个词条的位置，如果 pyim-current-choices 用
完，则检查是否有补全。如果 FINISH 为 non-nil，说明，补全已经用完了"
  (let* ((whole (length (car pyim-current-choices)))
         (len pyim-page-length)
         (pos pyim-current-pos)
         (last (+ (- pos (pyim-mod pos len)) len)))
    (if (< last whole)
        last
      (if finish
          whole
        (pyim-page-end (pyim-add-completion))))))

;;;  commands
(defun pyim-next-page (arg)
  (interactive "p")
  (if (> (length pyim-current-key) 0)
      (let ((new (+ pyim-current-pos (* pyim-page-length arg) 1)))
        (setq pyim-current-pos (if (> new 0) new 1)
              pyim-current-pos (pyim-page-start))
        (pyim-format-page))
    (message "%c" last-command-event)
    (pyim-append-string (pyim-translate last-command-event))
    (pyim-terminate-translation)))

(defun pyim-previous-page (arg)
  (interactive "p")
  (pyim-next-page (- arg)))

(defun pyim-delete-last-char ()
  (interactive)
  (if (> (length pyim-current-key) 1)
      (progn
        (setq pyim-current-key (substring pyim-current-key 0 -1))
        (funcall pyim-handle-function))
    (setq pyim-current-str "")
    (pyim-terminate-translation)))

(defun pyim-self-insert-command ()
  "如果在 pyim-first-char 列表中，则查找相应的词条，否则停止转换，插入对应的字符"
  (interactive "*")
  ;; (message "%s" (current-buffer))
  (if (if (pyim-string-emptyp pyim-current-key)
          (member last-command-event
                  (mapcar 'identity "abcdefghjklmnopqrstwxyz"))
        (member last-command-event
                (mapcar 'identity "vmpfwckzyjqdltxuognbhsrei'-a")))
      (progn
        (setq pyim-current-key (concat pyim-current-key (char-to-string last-command-event)))
        (funcall pyim-handle-function))
    (pyim-append-string (pyim-translate last-command-event))
    (pyim-terminate-translation)))

(defun pyim-select-current ()
  "如果没有可选项，而且是用空格来绑定这个键，就插入空格，否则选择第一
个词条"
  (interactive)
  (if (null (car pyim-current-choices))
      (setq pyim-current-str
            (if (> (length pyim-current-str) 0)
                ""
              (pyim-translate last-command-event)))
    (pyim-remember-select))
  (pyim-terminate-translation))

(defun pyim-remember-select (&optional pos)
  (let ((rest (emms-delete-if (lambda (p) (string= (car p) "pos"))
                              (cdr pyim-current-choices))))
    (setq rest (append rest (list (cons "pos" (or pos
                                                  pyim-current-pos)))))
    (puthash pyim-current-key (cons (car pyim-current-choices)
                                    rest) (pyim-history))))

(defun pyim-number-select ()
  "如果没有可选项，插入数字，否则选择对应的词条"
  (interactive)
  (if (car pyim-current-choices)
      (let ((index (+ (pyim-page-start) (- last-command-event ?2)))
            (end (pyim-page-end)))
        (if (>= index end)
            (pyim-show)
          (pyim-remember-select (1+ index))
          (setq pyim-current-str (pyim-choice (nth index (car pyim-current-choices))))
          (pyim-terminate-translation)))
    (pyim-append-string (char-to-string last-command-event))
    (pyim-terminate-translation)))

(defun pyim-quit-clear ()
  (interactive)
  (setq pyim-current-str "")
  (pyim-terminate-translation))

(defun pyim-quit-no-clear ()
  (interactive)
  (setq pyim-current-str pyim-current-key)
  (pyim-terminate-translation))

(defun pyim-terminate-translation ()
  "Terminate the translation of the current key."
  (setq pyim-translating nil)
  (pyim-delete-region)
  (setq pyim-current-choices nil)
  (setq pyim-guidance-str ""))

;;;  pyim-handle-string
(defun pyim-handle-string ()
  (if (and (functionp pyim-stop-function)
           (funcall pyim-stop-function))
      (progn
        (setq unread-command-events
              (list (aref pyim-current-key (1- (length pyim-current-key)))))
        (pyim-terminate-translation))
    (setq pyim-current-choices (pyim-get pyim-current-key)
          pyim-current-pos
          (if (pyim-get-option 'record-position)
              (cdr (assoc "pos" (cdr pyim-current-choices)))
            1))
    (pyim-format-page)))

(defun pyim-translate (char)
  (if (functionp pyim-translate-function)
      (funcall pyim-translate-function char)
    (char-to-string char)))

;;;  Core function of input method (stole from quail)
(defun pyim-exit-from-minibuffer ()
  (deactivate-input-method)
  (if (<= (minibuffer-depth) 1)
      (remove-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer)))

(defun pyim-setup-overlays ()
  (let ((pos (point)))
    (if (overlayp pyim-overlay)
        (move-overlay pyim-overlay pos pos)
      (setq pyim-overlay (make-overlay pos pos))
      (if input-method-highlight-flag
          (overlay-put pyim-overlay 'face 'pyim-string-face)))))

(defun pyim-delete-overlays ()
  (if (and (overlayp pyim-overlay) (overlay-start pyim-overlay))
      (delete-overlay pyim-overlay)))

(defun pyim-show ()
  (unless enable-multibyte-characters
    (setq pyim-current-key nil
          pyim-current-str nil)
    (error "Can't input characters in current unibyte buffer"))
  (pyim-delete-region)
  (insert pyim-current-str)
  (move-overlay pyim-overlay (overlay-start pyim-overlay) (point))
  ;; Then, show the guidance.
  (when (and (not input-method-use-echo-area)
             (null unread-command-events)
             (null unread-post-input-method-events))
    (if (eq (selected-window) (minibuffer-window))
        ;; Show the guidance in the next line of the currrent
        ;; minibuffer.
        (pyim-minibuffer-message
         (format "  [%s]\n%s"
                 current-input-method-title pyim-guidance-str))
      ;; Show the guidance in echo area without logging.
      (let ((message-log-max nil))
        (message "%s" pyim-guidance-str)))))

(defun pyim-make-guidance-frame ()
  "Make a new one-line frame for Quail guidance."
  (let* ((fparam (frame-parameters))
         (top (cdr (assq 'top fparam)))
         (border (cdr (assq 'border-width fparam)))
         (internal-border (cdr (assq 'internal-border-width fparam)))
         (newtop (- top
                    (frame-char-height) (* internal-border 2) (* border 2))))
    (if (< newtop 0)
        (setq newtop (+ top (frame-pixel-height) internal-border border)))
    (make-frame (append '((user-position . t) (height . 1)
                          (minibuffer)
                          (menu-bar-lines . 0) (tool-bar-lines . 0))
                        (cons (cons 'top newtop) fparam)))))

(defun pyim-minibuffer-message (string)
  (message nil)
  (let ((point-max (point-max))
        (inhibit-quit t))
    (save-excursion
      (goto-char point-max)
      (insert string))
    (sit-for 1000000)
    (delete-region point-max (point-max))
    (when quit-flag
      (setq quit-flag nil
            unread-command-events '(7)))))

(defun pyim-input-method (key)
  (if (or buffer-read-only
          overriding-terminal-local-map
          overriding-local-map)
      (list key)
    ;; (message "call with key: %c" key)
    (pyim-setup-overlays)
    (let ((modified-p (buffer-modified-p))
          (buffer-undo-list t)
          (inhibit-modification-hooks t))
      (unwind-protect
          (let ((input-string (pyim-start-translation key)))
            ;;   (message "input-string: %s" input-string)
            (setq pyim-guidance-str "")
            (when (and (stringp input-string)
                       (> (length input-string) 0))
              (if input-method-exit-on-first-char
                  (list (aref input-string 0))
                (pyim-input-string-to-events input-string))))
        (pyim-delete-overlays)
        (set-buffer-modified-p modified-p)
        ;; Run this hook only when the current input method doesn't
        ;; require conversion. When conversion is required, the
        ;; conversion function should run this hook at a proper
        ;; timing.
        (run-hooks 'input-method-after-insert-chunk-hook)))))

(defun pyim-start-translation (key)
  "Start translation of the typed character KEY by the current Quail package.
Return the input string."
  ;; Check the possibility of translating KEY.
  ;; If KEY is nil, we can anyway start translation.
  (if (or (integerp key) (null key))
      ;; OK, we can start translation.
      (let* ((echo-keystrokes 0)
             (help-char nil)
             (overriding-terminal-local-map (pyim-mode-map))
             (generated-events nil)
             (input-method-function nil)
             (modified-p (buffer-modified-p))
             last-command-event last-command this-command)
        (setq pyim-current-str ""
              pyim-current-key ""
              pyim-translating t)
        (if key
            (setq unread-command-events
                  (cons key unread-command-events)))
        (while pyim-translating
          (set-buffer-modified-p modified-p)
          (let* ((prompt (if input-method-use-echo-area
                             (format "%s%s %s"
                                     (or input-method-previous-message "")
                                     pyim-current-key
                                     pyim-guidance-str)))
                 (keyseq (read-key-sequence prompt nil nil t))
                 (cmd (lookup-key (pyim-mode-map) keyseq)))
            ;;             (message "key: %s, cmd:%s\nlcmd: %s, lcmdv: %s, tcmd: %s"
            ;;                      key cmd last-command last-command-event this-command)
            (if (if key
                    (commandp cmd)
                  (eq cmd 'pyim-self-insert-command))
                (progn
                  ;; (message "keyseq: %s" keyseq)
                  (setq last-command-event (aref keyseq (1- (length keyseq)))
                        last-command this-command
                        this-command cmd)
                  (setq key t)
                  (condition-case err
                      (call-interactively cmd)
                    (error (message "%s" (cdr err)) (beep))))
              ;; KEYSEQ is not defined in the translation keymap.
              ;; Let's return the event(s) to the caller.
              (setq unread-command-events
                    (string-to-list (this-single-command-raw-keys)))
              ;; (message "unread-command-events: %s" unread-command-events)
              (pyim-terminate-translation))))
        ;;    (1message "return: %s" pyim-current-str)
        pyim-current-str)
    ;; Since KEY doesn't start any translation, just return it.
    ;; But translate KEY if necessary.
    (char-to-string key)))

(defun pyim-input-string-to-events (str)
  (let ((events (mapcar 'identity str)))
    (if (or (get-text-property 0 'advice str)
            (next-single-property-change 0 'advice str))
        (setq events
              (nconc events (list (list 'pyim-advice str)))))
    events))

(defun pyim-advice (args)
  (interactive "e")
  (let* ((string (nth 1 args))
         (func (get-text-property 0 'advice string)))
    (if (functionp func)
        (funcall func string))))

(global-set-key [pyim-advice] 'pyim-advice)

;;; Function dealing with chinese words and punctuations
(require 'chinese-pyim-pinyin)

;; 处理标点符号
(defun pyim-punctuation-translate (char)
  (if pyim-punctuation-translate-p
      (cond ((< char ? ) "")
            (t (let ((str (char-to-string char))
                     punc)
                 (if (and (not (member (char-before) pyim-punctuation-escape-list))
                          (setq punc (cdr (assoc str pyim-punctuation-dict))))
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

;;; 切换光标处标点的样式（全角 or 半角）
(defun pyim-punctuation-translate-at-point ()
  (interactive)
  (let* ((current-char (char-to-string (preceding-char)))
         (punc-list
          (cl-some (lambda (x)
                     (when (member current-char x) x))
                   pyim-punctuation-dict)))
    (when punc-list
      (delete-char -1)
      (if (string= current-char (car punc-list))
          (if (> (length (cdr punc-list)) 1)
              (insert (ido-completing-read "请选择："(cdr punc-list)))
            (insert (car (cdr punc-list))))
        (insert (car punc-list))))))

;;; 切换中英文标点符号
(defun pyim-toggle-full-width-punctuation (arg)
  (interactive "P")
  (setq pyim-punctuation-translate-p
        (if (null arg)
            (not pyim-punctuation-translate-p)
          (> (prefix-numeric-value arg) 0)))
  (if pyim-punctuation-translate-p
      (message "开启标点转换功能（使用全角标点）")
    (message "关闭标点转换功能（使用半角标点）")))

;;;  一个快速插入英文的命令。
(defun pyim-insert-ascii ()
  (interactive)
  (if current-input-method
      (insert (read-no-blanks-input "自定义输入: "))
    (call-interactively 'self-insert-command)))

(defvar pyim-mode-map
  (let ((map (make-sparse-keymap))
        (i ?\ ))
    (while (< i 127)
      (define-key map (char-to-string i) 'pyim-self-insert-command)
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 256)
      (define-key map (vector i) 'pyim-self-insert-command)
      (setq i (1+ i)))
    (dolist (i (number-sequence ?1 ?9))
      (define-key map (char-to-string i) 'pyim-pinyin-number-select))
    (define-key map " " 'pyim-pinyin-select-current)
    (define-key map [backspace] 'pyim-delete-last-char)
    (define-key map (kbd "M-DEL") 'pyim-pinyin-backward-kill-py)
    (define-key map (kbd "M-g") (lambda ()
                                  (interactive)
                                  (funcall pyim-pinyin-fuzzy-adjust-function)))
    (define-key map [delete] 'pyim-delete-last-char)
    (define-key map "\177" 'pyim-delete-last-char)
    (define-key map "\C-n" 'pyim-pinyin-next-page)
    (define-key map "\C-p" 'pyim-pinyin-previous-page)
    (define-key map "=" 'pyim-pinyin-next-page)
    (define-key map "-" 'pyim-pinyin-previous-page)
    (define-key map "\M-n" 'pyim-pinyin-next-page)
    (define-key map "\M-p" 'pyim-pinyin-previous-page)
    (define-key map "\C-m" 'pyim-pinyin-quit-no-clear)
    (define-key map "\C-c" 'pyim-quit-clear)
    (define-key map "\C-g" 'pyim-quit-clear)
    map)
  "Keymap")

(defun pyim-restart ()
  "重启 Chinese-pyim，不建议用于编程环境。"
  (interactive)
  (let ((file-save-p
         (yes-or-no-p "正在重启 Chinese-pyim，需要保存 personal 文件的变动吗？ ")))
    (pyim-restart-1 file-save-p)))

(defun pyim-restart-1 (save-personal-file)
  "重启 Chinese-pyim，用于编程环境。"
  (pyim-start (pyim-name) nil t save-personal-file))

(defun pyim-start (name &optional active-func restart save-personal-file)
  (interactive)
  (mapc 'kill-local-variable pyim-local-variable-list)
  (mapc 'make-local-variable pyim-local-variable-list)
  ;; 重启时，kill 所有已经打开的 buffer。
  (when (and restart save-personal-file)
    (pyim-save-personal-file))
  (when restart
    (pyim-kill-buffers)
    (pyim-set-buffer-list nil))
  (unless (and (pyim-name)
               (pyim-check-buffers)
               (not restart))
    (pyim-set-name name)
    (pyim-set-buffer-list (pyim-load-file))
    (pyim-set-history (make-hash-table :test 'equal))
    (pyim-set-mode-map (let ((map (make-sparse-keymap)))
                         (set-keymap-parent map pyim-mode-map)
                         map))
    (pyim-set-active-function 'pyim-pinyin-activate-function)
    (pyim-pinyin-make-char-table)
    (run-hooks 'pyim-load-hook)
    (message nil))

  (unless (member 'pyim-save-personal-file kill-emacs-hook)
    (add-to-list 'kill-emacs-hook 'pyim-save-personal-file))

  (setq input-method-function 'pyim-input-method)
  (setq deactivate-current-input-method-function 'pyim-inactivate)
  ;; (setq describe-current-input-method-function 'pyim-help)
  ;; If we are in minibuffer, turn off the current input method
  ;; before exiting.
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'pyim-exit-from-minibuffer))
  (run-hooks 'pyim-active-hook)
  (if (functionp (pyim-active-function))
      (funcall (pyim-active-function)))
  (when restart
    (message "Chinese-pyim 重启完成。")))

;;; 注册输入法
(register-input-method "chinese-pyim" "euc-cn" 'pyim-start "[pyim]")

;;;###autoload
(defun pyim-update-table ()
  (interactive)
  (save-restriction
    (let ((lastw "")
          first-char total-char currw)
      (perform-replace "[ \t]+$" "" nil t nil nil nil (point-min) (point-max))
      (sort-regexp-fields nil "^.*$" "[a-z-]+[ ]+"
                          (point-min)
                          (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^[ \t]*$")     ; 如果有空行，删除
            (pyim-delete-line)
          (setq currw (pyim-code-at-point))
          (if (string= currw lastw)
              (delete-region (1- (point)) (+ (point) (length currw))))
          (setq lastw currw)
          (forward-line 1)))
      (goto-char (point-min))
      (while (not (eobp))
        (pyim-delete-duplicate-word)
        (forward-line 1))
      (if (looking-at "^$")
          (delete-char -1)))))

(provide 'chinese-pyim)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; chinese-pyim.el ends here
