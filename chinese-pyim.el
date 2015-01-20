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
;; 中文拼音词库，词条数量大于100万，文件大小大于20M，(注意：请使用另存为，不要直接点击链接)。
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
;; ### 第三种方式 ###
;;
;; 获取中文词条，然后添加拼音code。中文词条的获取途径很多，比如：
;;
;; 1. 从其它输入法中导出。
;; 2. 获取中文文章，通过分词系统分词得到。
;; 3. 中文处理工具自带的dict。
;; 4. 其它。
;;
;; Chinese-pyim 下面两个命令可以为中文词条添加拼音Code，从而生成可用词库：
;;
;; 1. `pyim-article2dict-chars' 将文章中游离汉字字符转换为拼音词库。
;; 2. `pyim-article2dict-words' 将文章中中文词语转换为拼音词库。
;; 3. `pyim-article2dict-misspell-words' 将文章中连续的游离词组成字符串后，转换为拼音词库。
;;
;; 注意：在运行上述两个命令之前，必须确保待转换的文章中，中文词汇已经使
;; 用 *空格* 强制隔开。
;;
;; 最后将生成的词库按上述方法添加到 Chinese-pyim 中就可以了。
;;
;; ## 词库文件编辑后注意事项 ##
;; 每一个词库文件必须按行排序（准确的说，是按每一行的拼音code排序），
;; 因为`Chinese-pyim' 寻找词条时，使用二分法来优化速度，而二分法工作的前提
;; 就是对文件按行排序。具体细节请参考：`pyim-bisearch-word' 。
;; 所以，当词库排序不正确时（比如：用户手动调整词库文件后），记得运行函数
;; `pyim-update-dict-file' 重新对文件排序。
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
;; 运行： C-h v pyim-fuzzy-pinyin-adjust-function
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
;; # 其他 Tips #
;;
;; ## 如何快速切换全角标点与半角标点 ##
;; 1. 第一种方法：使用命令 `pyim-toggle-full-width-punctuation'，全局切换。
;; 2. 第二种方法：使用命令 `pyim-punctuation-translate-at-point' 只切换光标处标点的样式。
;; 3. 第三种方法：设置变量 `pyim-translate-trigger-char'。输入变量设定的字符会切换光标处标点的样式。
;;
;; ## 了解 Chinese-pyim 个人词频文件设置的细节 ##
;;```
;; C-h v pyim-personal-file
;;```
;;
;; ## 了解 Chinese-pyim 词库设置细节 ##
;;```
;;C-h v pyim-dicts
;;```
;;
;; ## 将汉字字符串转换为拼音字符串 ##
;;    1. `pyim-hanzi2pinyin' （考虑多音字）
;;    2. `pyim-hanzi2pinyin-simple'  （不考虑多音字）
;;
;; ## 实现快速切换词库的功能 ##
;; 可以自定义类似的命令：
;; ```lisp
;; (defun pyim-use-dict:bigdict ()
;;   (interactive)
;;   (setq pyim-dicts
;;         '((:name "BigDict"
;;                  :file "/path/to/pyim-bigdict.txt"
;;                  :coding utf-8-unix)))
;;   (pyim-restart-1 t))
;; ```
;;
;; ## Chinese-pyim 开启联想词输入模式 ##
;;
;; `Chinese-pyim' 增加了两个 `company-mode' 补全后端来实现 *联想词* 输入功能：
;;
;; 1. `pyim-company-dabbrev' 是 `company-dabbrev' 的中文优化版，适用于补全其它 buffer 中的中文词语。
;; 2. `pyim-company-predict-words' 可以从 Chinese-pyim 词库中搜索与当前中文词条相近的词条。
;;
;; 安装和使用方式：
;;
;; 1. 安装 `company-mode' 扩展包。
;; 2. 在 emacs 配置中添加如下几行代码：
;; ```lisp
;; (require 'chinese-pyim-company)
;; ;; ;; 从词库中搜索10个联想词。
;; ;; (setq pyim-company-predict-words-number 10)
;; ```
;; ## 如何手动加词和删词 ##
;;
;; 1. `pyim-create-word-without-pinyin' 直接将一个中文词条加入个人词库的函数，用于编程环境。
;; 2. `pyim-create-word-at-point:<N>char' 这是一组命令，从光标前提取N个汉字字符组成字符串，
;;     并将其加入个人词库。
;; 3. `pyim-create-word-from-region' 如果用户已经高亮选择了某个中文字符串，那么这个命令直接
;;     将这个字符串加入个人词库，否则，这个命令会高亮选择光标前两个汉字字符，等待用户调整选区。
;;     建议用户为其设定一个快捷键。
;; 4. `pyim-translate-trigger-char' 以默认设置为例：在“我爱吃红烧肉”后输入“5v” 可以将
;;     “爱吃红烧肉”这个词条保存到用户个人文件。
;; 5. `pyim-automatic-generate-word' 将此选项设置为 t 时，Chinese-pyim 开启自动组词功能。
;;     实验特性，不建议普通用户使用，
;; 6. `pyim-delete-word-from-personal-buffer' 从个人文件对应的 buffer 中删除当前高亮选择的词条。
;;

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
件后，记得运行 `pyim-update-dict-file' 来对文件排序。"
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

(defcustom pyim-translate-trigger-char ?v
  "光标前面的字符为标点符号时，按这个字符可以切换前面的标点
符号的样式（半角/全角）

当光标前面为中文字符串时，输入 <num>v 可以用于保存自定义词条。"
  :group 'chinese-pyim
  :type 'character)

(defcustom pyim-fuzzy-pinyin-adjust-function
  'pyim-fuzzy-pinyin-adjust-1
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

(defcustom pyim-select-word-finish-hook nil
  "Chinese-pyim 选词完成时运行的hook，

Chinese-pyim 使用这个 hook 处理联想词，用户可以使用
这个 hook 调用来调用外部的补全系统等工作。"
  :group 'chinese-pyim
  :type 'hook)

(defcustom pyim-automatic-generate-word nil
  "自动组词功能，这个选项设置为 t 时，Chinese-pyim 会
将当前光标前2个或者3个汉字组成的字符串，加入个人词库。"
  :group 'chinese-pyim
  :type 'boolean)

(defcustom pyim-page-length 9
  "每页显示的词条数目"
  :group 'chinese-pyim
  :type 'number)

(defface pyim-string-face '((t (:underline t)))
  "Face to show current string"
  :group 'chinese-pyim)

;;;  variable declare
(defvar pyim-buffer-name " *Chinese-pyim*")
(defvar pyim-buffer-list nil
  "一个列表，用来保存词库文件与 buffer 的对应信息。
1. 每个元素都是一个 alist。
2. 每一个 alist 都包含两个部份：
   1. buffer 词库文件导入时创建的 buffer (用户不可见)。
   2. file   词库文件的路径。")
(defvar pyim-active-function nil)
(defvar pyim-current-key "" "已经输入的代码")
(defvar pyim-current-str "" "当前选择的词条")

(defvar pyim-separate-char-history nil
  "纪录连续输入的单个汉字，当输入词组后，这个变量设置为 nil。
这个变量主要用于自动组词功能。")

(defvar pyim-current-choices nil
  "所有可选的词条，是一个list。
1. CAR 部份是可选的词条，一般是一个字符串列表。
   也可以含有list。但是包含的list第一个元素必须是将要插入的字符串。
2. CDR 部分是一个 Association list。通常含有这样的内容：
   1. pos 上次选择的位置
   2. completion 下一个可能的字母（如果 pyim-do-completion 为 t）")

(defvar pyim-current-predict-words nil
  "用来纪录联想得到的词条，有利于将联想词和正常词分开处理。")

(defvar pyim-current-pos nil "当前选择的词条在 pyim-current-choices 中的位置")
(defvar pyim-guidance-str "" "显示可选词条的字符串")
(defvar pyim-translating nil "记录是否在转换状态")
(defvar pyim-overlay nil "显示当前选择词条的 overlay")
(defvar pyim-guidance-frame nil)
(defvar pyim-guidance-buf nil)

(defvar pyim-load-hook nil)
(defvar pyim-active-hook nil)

(defvar pyim-stop-function nil)
(defvar pyim-translate-function 'pyim-default-translate)
(defvar pyim-add-completion-function nil)
(defvar pyim-format-function 'pyim-format)
(defvar pyim-handle-function 'pyim-handle-string)

(defvar pyim-punctuation-escape-list (number-sequence ?0 ?9)
  "Punctuation will not insert after this characters.
If you don't like this funciton, set the variable to nil")

(defvar pyim-punctuation-translate-p t
  "*Non-nil means will translate punctuation.")

(defvar pyim-pair-punctuation-status
  '(("\"" nil) ("'" nil))
  "成对标点符号切换状态")

(defvar pyim-shen-mu
  '("b" "p" "m" "f" "d" "t" "n" "l" "g" "k" "h"
    "j" "q" "x" "z" "c" "s" "zh" "ch" "sh" "r" "y" "w"))

(defvar pyim-yun-mu
  '("a" "o" "e" "i" "u" "v" "ai" "ei" "ui" "ao" "ou" "iu"
    "ie" "ia" "ua" "ve" "er" "an" "en" "in" "un" "vn" "ang" "iong"
    "eng" "ing" "ong" "uan" "uang" "ian" "iang" "iao" "ue"
    "uai" "uo"))

(defvar pyim-valid-yun-mu
  '("a" "o" "e" "ai" "ei" "ui" "ao" "ou" "er" "an" "en"
    "ang" "eng"))

(defvar pyim-char-table (make-vector 1511 nil))
(defvar pyim-position nil)
(defvar pyim-pinyin-list nil)

(defvar pyim-dict-help-string
  "Chinese-pyim 没有可用词库！！！

拼音词库是 Chinese-pyim 使用顺手与否的关键。根据经验估计：
1. 当词库词条超过100万时(词库文件>20M)，Chinese-pyim 选词频率大大降低。
2. 当词库词条超过100万时，Chinese-pyim 中文输入体验可以达到搜狗输入法的80%。

赶时间的朋友可以直接下载其他 Chinese-pyim 用户现成的拼音词库，比如，某个同学
自己使用的词库：BigDict，这个词库词条数量大于100万，文件大小大于20M，可以显著
增强 Chinese-pyim 的输入体验，(注意：请使用另存为，不要直接点击链接)。

  https://github.com/tumashu/chinese-pyim-bigdict/blob/master/pyim-bigdict.txt?raw=true

下载上述拼音词库后，运行 `pyim-add-dict' ，按照命令提示，将词库文件信息添加到
 `pyim-dicts' 中，最后运行命令 `pyim-restart' 或者重启emacs。。

喜欢折腾的用户可以从下面几个途径获得 Chinese-pyim 更详细的信息。
1. 使用 `C-h v pyim-dicts' 了解 `Chinese-pyim' 词库文件格式，
2. 了解如何导入其它输入法的词库。
   1. 使用 package 管理器查看 Chinese-pyim 包的简介
   2. 阅读 chinese-pyim.el 文件 Commentary
   3. 查看 Chinese-pyim 在线 README：https://github.com/tumashu/chinese-pyim")

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
      (define-key map (char-to-string i) 'pyim-number-select))
    (define-key map " " 'pyim-select-current)
    (define-key map [backspace] 'pyim-delete-last-char)
    (define-key map (kbd "M-DEL") 'pyim-backward-kill-py)
    (define-key map (kbd "M-g") (lambda ()
                                  (interactive)
                                  (funcall pyim-fuzzy-pinyin-adjust-function)))
    (define-key map [delete] 'pyim-delete-last-char)
    (define-key map "\177" 'pyim-delete-last-char)
    (define-key map "\C-n" 'pyim-next-page)
    (define-key map "\C-p" 'pyim-previous-page)
    (define-key map "=" 'pyim-next-page)
    (define-key map "-" 'pyim-previous-page)
    (define-key map "\M-n" 'pyim-next-page)
    (define-key map "\M-p" 'pyim-previous-page)
    (define-key map "\C-m" 'pyim-quit-no-clear)
    (define-key map "\C-c" 'pyim-quit-clear)
    (define-key map "\C-g" 'pyim-quit-clear)
    map)
  "Keymap")

(defvar pyim-local-variable-list
  '(pyim-page-length

    pyim-current-key
    pyim-current-str
    pyim-current-choices
    pyim-current-pos
    ;; pyim-current-predict-words
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
    describe-current-input-method-function

    pyim-pair-punctuation-status

    pyim-pinyin-list
    pyim-pinyin-position)
  "A list of buffer local variable")

(dolist (var pyim-local-variable-list)
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

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
    (setq file (read-file-name "请选择词库文件： " "~/"))
    (setq coding (completing-read "词库文件编码: "
                                  '("cjk-dos" "gb18030-dos") nil t nil nil "utf-8-unix"))
    (setq first-used  (yes-or-no-p "是否让 Chinese-pyim 优先使用词库？ "))
    (setq dict `(:name ,name :file ,file :coding ,(intern coding)))
    (if first-used
        (add-to-list 'pyim-dicts dict)
      (add-to-list 'pyim-dicts dict t))
    ;; 将`pyim-dict'的设置保存到emacs配置文件中。
    (customize-save-variable 'pyim-dicts pyim-dicts)
    (message "添加并保存 Chinese-pyim 输入法词库: (%s)，运行 `pyim-restart' 命令或者重启 emacs 后生效！" name)))

(defun pyim-show-help (string)
  "显示 Chinese-pyim 帮助信息，让用户快速的了解如何安装词库。"
  (let ((buffer-name "*Chinese-pyim-dict-help*"))
    (with-output-to-temp-buffer buffer-name
      (set-buffer buffer-name)
      (when (featurep 'org)
        (org-mode))
      (setq truncate-lines 1)
      (insert string)
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
        (bufname pyim-buffer-name)
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
        (pyim-show-help pyim-dict-help-string)))
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
  (let* ((element (car pyim-buffer-list))
         (buffer (cdr (assoc "buffer" element)))
         (file (cdr (assoc "file" element))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-restriction
          (if (file-exists-p file)
              (progn (write-region (point-min) (point-max) file)
                     (message "更新 Chinese-pyim 文件：%s。" file))
            (message "Chinese-pyim 文件：%s 不存在。" file)))))))

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

(defun pyim-sort-and-remove-duplicates (words-list)
  "使用分词后的文章来制作拼音词库时，首先按照词条在文章中
出现的频率对词条排序，然后再删除重复词条。"
  (let ((list (cl-remove-duplicates words-list :test #'equal))
        (count-table (make-hash-table :test #'equal)))
    (dolist (x words-list)
      (let ((value (gethash x count-table)))
        (if value
            (puthash x (1+ value) count-table)
          (puthash x 1 count-table))))
    (sort list (lambda (a b) (> (gethash a count-table)
                                (gethash b count-table))))))

(defun pyim-remove-duplicates-word (&optional sort-by-freq)
  "制作拼音词库时，删除当前行重复出现的词条，
当 `sort-by-freq' 为 t 时，首先按照当前行词条出现频率对词条排序，
然后再删除重复词条，用于：从中文文章构建词库。"
  (interactive)
  (let* (words-list length)
    (setq words-list (pyim-line-content " "))
    (setq length (length words-list))
    (setq words-list
          (if sort-by-freq
              (cons (car words-list) ;; 拼音必须排在第一位
                    (pyim-sort-and-remove-duplicates (cdr words-list)))
            (cl-remove-duplicates words-list :test #'equal)))
    (when (> length (length words-list))
      (pyim-delete-line)
      (insert (mapconcat 'identity words-list " "))
      (insert "\n")
      (goto-char (line-beginning-position)))))

;;;  interface
(defun pyim-check-buffers ()
  "检查所有的 buffer 是否还存在，如果不存在，重新打开文件，如果文件不
存在，从 buffer-list 中删除这个 buffer"
  (let ((buflist pyim-buffer-list)
        (bufname pyim-buffer-name)
        buffer file)
    (dolist (buf buflist)
      (setq buffer (assoc "buffer" buf))
      (setq file (cdr (assoc "file" buf)))
      (unless (buffer-live-p (cdr buffer))
        (if (file-exists-p file)
            (with-current-buffer (generate-new-buffer bufname)
              (insert-file-contents file)
              (setcdr buffer (current-buffer)))
          (message "%s for %s is not exists!" file bufname)
          (setq buflist (remove buf buflist)))))
    t))

(defun pyim-kill-buffers ()
  "删除所有词库文件对应的 buffer ，用于重启 Chinese-pyim 。"
  (let ((buflist pyim-buffer-list)
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


;;;  handle function
(defun pyim-handle-string ()
  (let ((str pyim-current-key)
        userpos wordspy)
    (setq pyim-pinyin-list (pyim-split-string str)
          pyim-pinyin-position 0)
    (unless (and (pyim-validp pyim-pinyin-list)
                 (progn
                   (setq userpos (pyim-user-divide-pos str)
                         pyim-current-key (pyim-restore-user-divide
                                           (pyim-pylist-to-string pyim-pinyin-list)
                                           userpos))
                   (setq pyim-current-choices (list (delete-dups (pyim-get-choices pyim-pinyin-list))))
                   (when  (car pyim-current-choices)
                     (setq pyim-current-pos 1)
                     (pyim-format-page)
                     t)))
      (setq pyim-current-str (replace-regexp-in-string "-" "" pyim-current-key))
      (setq pyim-guidance-str (format "%s"
                                      (replace-regexp-in-string
                                       "-" " " pyim-current-key)))
      (pyim-show))))

(defun pyim-format-page ()
  "按当前位置，生成候选词条"
  (let* ((end (pyim-page-end))
         (start (1- (pyim-page-start)))
         (choices (car pyim-current-choices))
         (choice (pyim-subseq choices start end))
         (pos (1- (min pyim-current-pos (length choices))))
         (i 0) rest)
    (setq pyim-current-str (concat (substring pyim-current-str 0 pyim-pinyin-position)
                                   (pyim-choice (nth pos choices)))
          rest (mapconcat (lambda (py)
                            (concat (car py) (cdr py)))
                          (nthcdr (length pyim-current-str) pyim-pinyin-list)
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

(defun pyim-pylist-to-string (pylist)
  "把分解的拼音合并，以便进行查找"
  (mapconcat 'identity
             (mapcar (lambda (w) (concat (car w) (cdr w))) pylist)
             "-"))

;; 将汉字的拼音分成声母和其它
(defun pyim-get-sm (py)
  "从一个拼音字符串中提出第一个声母。"
  (when (and py (string< "" py))
    (let (shenmu yunmu len)
      (if (< (length py) 2)
          (if (member py pyim-shen-mu)
              (cons py "")
            (cons "" py))
        (setq shenmu (substring py 0 2))
        (if (member shenmu pyim-shen-mu)
            (setq py (substring py 2))
          (setq shenmu (substring py 0 1))
          (if (member shenmu pyim-shen-mu)
              (setq py (substring py 1))
            (setq shenmu "")))
        (cons shenmu py)))))

(defun pyim-get-ym (py)
  "从一个拼音字符串中提出第一个韵母"
  (when (and py (string< "" py))
    (let (yunmu len)
      (setq len (min (length py) 5))
      (setq yunmu (substring py 0 len))
      (while (and (not (member yunmu pyim-yun-mu))
                  (> len 0))
        (setq yunmu (substring py 0 (setq len (1- len)))))
      (setq py (substring py len))
      (if (and (string< "" py)
               (not (member (substring py 0 1) pyim-shen-mu))
               (member (substring yunmu -1) pyim-shen-mu)
               (member (substring yunmu 0 -1) pyim-yun-mu))
          (setq py (concat (substring yunmu -1) py)
                yunmu (substring yunmu 0 -1)))
      (cons yunmu py))))

(defun pyim-get-charpy (py)
  "分解一个拼音字符串成声母和韵母。"
  (when (and py (string< "" py))
    (let* ((sm (pyim-get-sm py))
           (ym (pyim-get-ym (cdr sm)))
           (chpy (concat (car sm) (car ym))))
      (if (or (null ym)                 ; 如果韵母为空
              (and (string< "" (car ym)) (not (pyim-get chpy)))) ; 错误的拼音
          (cons sm "")
        (cons (cons (car sm) (car ym)) (cdr ym))))))

;;; 处理输入的拼音
(defun pyim-split-string (py)
  "把一个拼音字符串分解。如果含有 '，优先在这个位置中断，否则，自动分
解成声母和韵母的组合"
  (when (and py (string< "" py))
    (apply 'append
           (mapcar (lambda (p)
                     (let (chpy pylist)
                       (setq p (replace-regexp-in-string "[ -]" "" p))
                       (while (when (string< "" p)
                                (setq chpy (pyim-get-charpy p))
                                (setq pylist (append pylist (list (car chpy))))
                                (setq p (cdr chpy))))
                       pylist))
                   (split-string py "'")))))

(defun pyim-validp (pylist)
  "检查得到的拼音是否含有声母为空，而韵母又不正确的拼音"
  (let ((valid t) py)
    (while (progn
             (setq py (car pylist))
             (if (and (not (string< "" (car py)))
                      (not (member (cdr py) pyim-valid-yun-mu)))
                 (setq valid nil)
               (setq pylist (cdr pylist)))))
    valid))

(defun pyim-user-divide-pos (py)
  "检测出用户分割的位置"
  (setq py (replace-regexp-in-string "-" "" py))
  (let (poslist (start 0))
    (while (string-match "'" py start)
      (setq start (match-end 0))
      (setq poslist (append poslist (list (match-beginning 0)))))
    poslist))

(defun pyim-restore-user-divide (py pos)
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
(defun pyim-get-choices (pylist)
  "得到可能的词组和汉字。例如：

 (pyim-get-choices  (pyim-split-string \"pin-yin\"))
  => (#(\"拼音\" 0 2 (py (\"pin-yin\"))) \"拼\" \"品\" \"贫\" \"苹\" \"聘\" \"频\" \"拚\" \"颦\" \"牝\" \"嫔\" \"姘\" \"嚬\")

 (pyim-get-choices  (pyim-split-string \"pin-yin\"))
 => (#(\"拼音\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))) #(\"贫铀\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))) #(\"聘用\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))) \"拼\" \"品\" \"贫\" \"苹\" \"聘\" \"频\" \"拚\" \"颦\" \"牝\" \"嫔\" \"姘\" \"嚬\")

"
  (let (choice words chars wordspy choice)
    (setq wordspy (pyim-possible-words-py pylist))
    (if wordspy
        (setq words (pyim-possible-words wordspy)))
    (setq chars (pyim-get (concat (caar pylist) (cdar pylist)))
          choice (append words chars))))

(defun pyim-possible-words (wordspy)
  "根据拼音得到可能的词组。例如：
  (pyim-possible-words '((\"p-y\" (\"p\" . \"in\") (\"y\" . \"\"))))
    => (#(\"拼音\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))) #(\"贫铀\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))) #(\"聘用\" 0 2 (py ((\"p\" . \"in\") (\"y\" . \"\")))))

"
  (let (words)
    (dolist (word (reverse wordspy))
      (if (listp word)
          (setq words (append words (pyim-match-word (pyim-get (car word))
                                                     (cdr word))))
        (setq words (append words (mapcar (lambda (w)
                                            (propertize w 'py (list word)))
                                          (pyim-get word))))))
    words))

(defun pyim-possible-words-py (pylist)
  "所有可能的词组拼音。从第一个字开始，每个字断开形成一个拼音。如果是
完整拼音，则给出完整的拼音，如果是给出声母，则为一个 CONS CELL，CAR 是
拼音，CDR 是拼音列表。例如：

 (setq foo-pylist (pyim-split-string \"pin-yin-sh-r\"))
  => ((\"p\" . \"in\") (\"y\" . \"in\") (\"sh\" . \"\") (\"r\" . \"\"))

 (pyim-possible-words-py foo-pylist)
  => (\"pin-yin\" (\"p-y-sh\" (\"p\" . \"in\") (\"y\" . \"in\") (\"sh\" . \"\")) (\"p-y-sh-r\" (\"p\" . \"in\") (\"y\" . \"in\") (\"sh\" . \"\") (\"r\" . \"\")))
 "
  (let (pys fullpy smpy wordlist (full t))
    (if (string< "" (cdar pylist))
        (setq fullpy (concat (caar pylist) (cdar pylist))
              smpy (pyim-essential-py (car pylist)))
      (setq smpy (caar pylist)
            full nil))
    (setq wordlist (list (car pylist)))
    (dolist (py (cdr pylist))
      (setq wordlist (append wordlist (list py)))
      (if (and full (string< "" (cdr py)))
          (setq fullpy (concat fullpy "-" (car py) (cdr py))
                smpy (concat smpy "-" (pyim-essential-py py))
                pys (append pys (list fullpy)))
        (setq full nil
              smpy (concat smpy "-" (pyim-essential-py py))
              pys (append pys (list (cons smpy wordlist))))))
    ;; (message "%s: %s" pys wordlist))
    pys))

(defun pyim-match-word (wordlist wordspy)
  "给出一个词组列表和它的拼音列表，给出所有可能的词组，并加上一个 py
属性。例如：

 (pyim-get \"p-y\")
  => (\"拼音\" \"番禺\" \"培养\" \"培育\" \"配药\" \"彭阳\" \"朋友\" \"偏远\" \"便宜\" \"片语\" \"飘扬\" \"漂移\" \"漂游\" \"贫铀\" \"聘用\" \"平阳\" \"平遥\" \"平邑\" \"平阴\" \"平舆\" \"平原\" \"平远\" \"濮阳\")

 (pyim-match-word (pyim-get \"p-y\") '((\"p\" . \"in\") (\"y\" . \"\")))
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
              (dolist (chpy (pyim-get-char-code (aref word i)))
                (if (string= (cdr (pyim-get-sm chpy)) (cdr py))
                    (setq chmatch t)))
              (or chmatch (setq match nil)))))
        ;; (message "%d: py: %s, match: %s" i py match))
        (if match
            (setq words (append words (list (propertize word 'py wordspy)))))))
    words))

(defun pyim-essential-py (py)
  "一个拼音中的主要部分，如果有声母返回声母，否则返回韵母"
  (if (string< "" (car py))
      (car py)
    (cdr py)))

;;;  create and rearrage
(defun pyim-match-py (word pylist)
  (let (sym words fullpy abbpy chpy)
    (when (> (length word) 1)
      (if (stringp (car pylist))        ; if is full pinyin
          (progn (setq fullpy (car pylist))
                 (cons fullpy (mapconcat 'identity
                                         (mapcar 'pyim-essential-py
                                                 (pyim-split-string (replace-regexp-in-string "-" "'" fullpy)))
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
                                 (car (pyim-get-char-code (aref word i)))))))
        (cons (substring fullpy 1)
              (substring abbpy 1))))))

(defun pyim-intern-word (word py &optional append delete)
  "这个函数用于保存用户词频，将参数拼音 `py' 对应的中文词条 `word'
保存到 personal-file 对应的 buffer。

当 `append' 设置为 t 时，新词追加到已有词的后面。

当`delete' 设置为 t 时，从上述 buffer 中删除参数拼音 `py' 对应
的中文词条 `word'。"
  (let((buf (cdr (assoc "buffer" (car pyim-buffer-list))))
       words)
    (with-current-buffer buf
      (pyim-bisearch-word py (point-min) (point-max))
      (if (string= (pyim-code-at-point) py)
          (progn
            (setq words (pyim-line-content))
            (if delete
                (setq words (remove word words))
              (setq words
                    (cons (car words)
                          (delete-dups
                           (if append
                               (append (cdr words) (list word))
                             (append (list word) (cdr words)))))))
            ;; (message "delete: %s" words))
            (pyim-delete-line))
        (forward-line 1)
        (setq words (list py word)))
      ;;    (message "insert: %s" words)
      (when (> (length words) 1)
        (insert (mapconcat 'identity words " ") "\n")))))

(defun pyim-create-word (word pylist)
  ;; (message "create: %s, %s" word pylist)
  (let ((py (pyim-match-py word pylist))
        words)
    (when py
      (pyim-intern-word word (car py))
      (pyim-intern-word word (cdr py)))))

(defun pyim-create-word-without-pinyin (word)
  "将中文词条 `word' 添加拼音后，保存到 personal-file 对应的
buffer中，当前词条追加到已有词条之后。"
  (mapc (lambda (py)
          (unless (string-match-p "[^ a-z-]" py)
            (pyim-intern-word word py t)))
        (pyim-hanzi2pinyin word nil "-" t)))

(defun pyim-delete-word (word)
  "将中文词条 `word' 从 personal-file 对应的 buffer 中删除"
  (mapc (lambda (py)
          (unless (string-match-p "[^ a-z-]" py)
            (pyim-intern-word word py nil t)))
        (pyim-hanzi2pinyin word nil "-" t)))

(defun pyim-chinese-string-at-point (&optional number)
  "获取光标一个中文字符串，字符数量为：`number'"
  (save-excursion
    (let* ((point (point))
           (begin (- point number))
           (begin (if (> begin 0)
                      begin
                    (point-min)))
           (string (buffer-substring-no-properties
                    point begin)))
      (when (and string
                 (= (length string) number)
                 (not (string-match-p "\\CC" string)))
        string))))

(defun pyim-create-word-at-point (&optional number silent)
  "将光标前字符数为 `number' 的中文字符串添加到个人词库中
当 `silent' 设置为 t 是，不显示提醒信息。"
  (let* ((string (pyim-chinese-string-at-point (or number 2))))
    (when string
      (pyim-create-word-without-pinyin string)
      (unless silent
        (message "将词条: \"%s\" 插入 personal file。" string)))))

(defun pyim-create-word-at-point:2char ()
  "将光标前2个中文字符组成的字符串加入个人词库。"
  (interactive)
  (pyim-create-word-at-point 2))

(defun pyim-create-word-at-point:3char ()
  "将光标前3个中文字符组成的字符串加入个人词库。"
  (interactive)
  (pyim-create-word-at-point 3))

(defun pyim-create-word-at-point:4char ()
  "将光标前4个中文字符组成的字符串加入个人词库。"
  (interactive)
  (pyim-create-word-at-point 4))

(defun pyim-create-word-from-region ()
  "将高亮选择的字符串添加到个人词库，如果当前没有选择任何
字符串，那么选择光标前两个字符。"
  (interactive)
  (if mark-active
      (let ((string (buffer-substring-no-properties
                     (region-beginning) (region-end))))
        (if (and (< (length string) 6)
                 (> (length string) 0))
            (progn
              (pyim-create-word-without-pinyin string)
              (message "将词条: \"%s\" 插入 personal file。" string))
          (message "选择的字符串大于6个汉字，忽略。"))
        (goto-char (region-end))
        (deactivate-mark))
    ;; 激活光标前两个字符大小的一个选区，
    ;; 等待用户调整选区大小。
    (push-mark (point))
    (backward-char 2)
    (setq mark-active t)))

(defun pyim-delete-word-from-personal-buffer ()
  "将高亮选择的字符从 personel-file 对应的 buffer 中删除。"
  (interactive)
  (if mark-active
      (let ((string (buffer-substring-no-properties
                     (region-beginning) (region-end))))
        (when (and (< (length string) 6)
                   (> (length string) 0))
          (pyim-delete-word string)
          (message "将词条: \"%s\" 从 personal file中删除。" string)))
    (message "请首先高亮选择需要删除的词条。")))

(defun pyim-rearrange (word pylist)
  ;; (message "rearrage: %s, %s" word pylist)
  (let ((py (pyim-match-py word pylist)))
    (when py
      (pyim-rearrange-1 word
                        (car py))
      (pyim-rearrange-1 word (cdr py)))))

(defun pyim-rearrange-1 (word py)
  (pyim-intern-word word py))

(defun pyim-han-stringp (str)
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
(defun pyim-select-current ()
  (interactive)
  (if (null (car pyim-current-choices))  ; 如果没有选项，输入空格
      (progn
        (setq pyim-current-str (pyim-translate last-command-event))
        (pyim-terminate-translation))
    (let ((str (pyim-choice (nth (1- pyim-current-pos) (car pyim-current-choices))))
          chpy pylist)
      (if (> (length str) 1)            ; 重排
          (pyim-rearrange str (get-text-property 0 'py str))
        (setq chpy (nth pyim-pinyin-position pyim-pinyin-list))
        (pyim-rearrange-1 str (concat (car chpy) (cdr chpy))))
      (setq pyim-pinyin-position (+ pyim-pinyin-position (length str)))
      (if (= pyim-pinyin-position (length pyim-pinyin-list))
                                        ; 如果是最后一个，检查
                                        ; 是不是在文件中，没有的话，创
                                        ; 建这个词
          (progn
            (if (not (member pyim-current-str (car pyim-current-choices)))
                (pyim-create-word pyim-current-str pyim-pinyin-list))
            (pyim-terminate-translation)
            ;; 纪录连续输入的单个汉字，用于判断是否启动自动组词。
            (push pyim-current-str pyim-separate-char-history)
            ;; 输入词语后，将词语前面的两个汉字机械的组成一个词条，
            ;; 然后保存到个人词库。
            ;; BUG: 这个地方需要进一步优化。
            (when (and pyim-automatic-generate-word
                       (> (length pyim-separate-char-history) 1)
                       (> (length pyim-current-str) 1))
              (pyim-create-word-at-point 2 t)
              (setq pyim-separate-char-history nil))
            ;; Chinese-pyim 使用这个 hook 来处理联想词。
            (run-hooks 'pyim-select-word-finish-hook))
        (setq pylist (nthcdr pyim-pinyin-position pyim-pinyin-list))
        (setq pyim-current-choices (list (pyim-get-choices pylist))
              pyim-current-pos 1)
        (pyim-format-page)))))

(defun pyim-number-select ()
  "如果没有可选项，插入数字，否则选择对应的词条"
  (interactive)
  (if (car pyim-current-choices)
      (let ((index (- last-command-event ?1))
            (end (pyim-page-end)))
        (if (> (+ index (pyim-page-start)) end)
            (pyim-show)
          (setq pyim-current-pos (+ pyim-current-pos index))
          (setq pyim-current-str (concat (substring pyim-current-str 0
                                                    pyim-pinyin-position)
                                         (pyim-choice
                                          (nth (1- pyim-current-pos)
                                               (car pyim-current-choices)))))
          (pyim-select-current)))
    (pyim-append-string (char-to-string last-command-event))
    (pyim-terminate-translation)))

(defun pyim-next-page (arg)
  (interactive "p")
  (if (= (length pyim-current-key) 0)
      (progn
        (pyim-append-string (pyim-translate last-command-event))
        (pyim-terminate-translation))
    (let ((new (+ pyim-current-pos (* pyim-page-length arg) 1)))
      (setq pyim-current-pos (if (> new 0) new 1)
            pyim-current-pos (pyim-page-start))
      (pyim-format-page))))

(defun pyim-previous-page (arg)
  (interactive "p")
  (pyim-next-page (- arg)))

(defun pyim-quit-no-clear ()
  (interactive)
  (setq pyim-current-str (replace-regexp-in-string "-" ""
                                                   pyim-current-key))
  (pyim-terminate-translation))

(defun pyim-backward-kill-py ()
  (interactive)
  (string-match "['-][^'-]+$" pyim-current-key)
  (setq pyim-current-key
        (replace-match "" nil nil pyim-current-key))
  (pyim-handle-string))

(defun pyim-fuzzy-pinyin-adjust-1 ()
  (interactive)
  (cond
   ((string-match-p "eng" pyim-current-key)
    (setq pyim-current-key
          (replace-regexp-in-string "eng" "en" pyim-current-key)))
   ((string-match-p "en[^g]*" pyim-current-key)
    (setq pyim-current-key
          (replace-regexp-in-string "en" "eng" pyim-current-key))))
  (cond
   ((string-match-p "ing" pyim-current-key)
    (setq pyim-current-key
          (replace-regexp-in-string "ing" "in" pyim-current-key)))
   ((string-match-p "in[^g]*" pyim-current-key)
    (setq pyim-current-key
          (replace-regexp-in-string "in" "ing" pyim-current-key))))
  (cond
   ((string-match-p "un" pyim-current-key)
    (setq pyim-current-key
          (replace-regexp-in-string "un" "ong" pyim-current-key)))
   ((string-match-p "ong" pyim-current-key)
    (setq pyim-current-key
          (replace-regexp-in-string "ong" "un" pyim-current-key))))
  (pyim-handle-string))

;;;  pyim-get
(defun pyim-get (code)
  (let (words predict-words)
    (when (and (stringp code) (string< "" code))
      (dolist (buf pyim-buffer-list)
        (with-current-buffer (cdr (assoc "buffer" buf))
          (setq words (append words
                              (cdr
                               (pyim-bisearch-word code
                                                   (point-min)
                                                   (point-max)))))))
      (delete-dups words))))

(defun pyim-get-char-code (char)
  "Get the code of the character CHAR"
  (symbol-value (intern-soft (char-to-string char) pyim-char-table)))

(defun pyim-make-char-table-1 (chars)
  (dolist (char chars)
    (let ((code (car char)))
      (dolist (c (cdr char))
        (let* ((s (intern-soft c pyim-char-table))
               (py (and s (symbol-value s))))
          (set (intern c pyim-char-table) (append py (list code))))))))

(defun pyim-make-char-table ()
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
        (pyim-make-char-table-1 pinyin-list)))))

;;;  commands
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

(defun pyim-quit-clear ()
  (interactive)
  (setq pyim-current-str "")
  (pyim-terminate-translation))

(defun pyim-terminate-translation ()
  "Terminate the translation of the current key."
  (setq pyim-translating nil)
  (pyim-delete-region)
  (setq pyim-current-choices nil)
  (setq pyim-guidance-str ""))

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
             (overriding-terminal-local-map pyim-mode-map)
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
                 (cmd (lookup-key pyim-mode-map keyseq)))
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

;; 处理标点符号
(defun pyim-return-proper-punctuation (punc-list &optional before)
  "返回合适的标点符号，`punc-list'为标点符号列表，其格式类似：
      `(\",\" \"，\") 或者：`(\"'\" \"‘\" \"’\")
当 `before' 为 t 时，只返回切换之前的结果，这个用来获取切换之前
的标点符号。"
  (let* ((str (car punc-list))
         (punc (cdr punc-list))
         (switch-p (cdr (assoc str pyim-pair-punctuation-status))))
    (if (= (safe-length punc) 1)
        (car punc)
      (if before
          (setq switch-p (not switch-p))
        (setf (cdr (assoc str pyim-pair-punctuation-status))
              (not switch-p)))
      (if switch-p
          (car punc)
        (nth 1 punc)))))

(defun pyim-char-before-to-string (num)
  "得到光标前第 `num' 个字符，并将其转换为字符串。"
  (let* ((point (point))
         (point-before (- point num)))
    (when (and (> point-before 0)
               (char-before point-before))
      (char-to-string (char-before point-before)))))

(defun pyim-default-translate (char)
  (let* ((str (char-to-string char))
         ;; 注意：`str' 是 *待输入* 的字符对应的字符串。
         (str-before-1 (pyim-char-before-to-string 0))
         (str-before-2 (pyim-char-before-to-string 1))
         (str-before-3 (pyim-char-before-to-string 2))
         (str-before-4 (pyim-char-before-to-string 3))
         ;; 从标点词库中搜索与 `str' 对应的标点列表。
         (punc-list (assoc str pyim-punctuation-dict))
         ;; 从标点词库中搜索与 `str-before-1' 对应的标点列表。
         (punc-list-before-1
          (cl-some (lambda (x)
                     (when (member str-before-1 x) x))
                   pyim-punctuation-dict))
         ;; `str-before-1' 在其对应的标点列表中的位置。
         (punc-posit-before-1
          (cl-position str-before-1 punc-list-before-1
                       :test #'string=)))
    (cond
     ;; 空格之前的字符什么也不输入。
     ((< char ? ) "")

     ;; 这个部份与标点符号处理无关，主要用来快速保存用户自定义词条。
     ;; 比如：在一个中文字符串后输入 2v，可以将光标前两个中文字符
     ;; 组成的字符串，保存到个人词库。
     ((and (member (char-before) (number-sequence ?2 ?9))
           (string-match-p "\\cc" str-before-2)
           (= char pyim-translate-trigger-char))
      (delete-char -1)
      (pyim-create-word-at-point
       (string-to-number str-before-1))
      "")

     ;; 关闭标点转换功能时，只插入英文标点。
     ((not pyim-punctuation-translate-p) str)

     ;; 当前字符属于 `pyim-punctuation-escape-list'时，
     ;; 插入英文标点。
     ((member (char-before)
              pyim-punctuation-escape-list) str)

     ;; 当光标前面为英文标点时， 按 `pyim-translate-trigger-char'
     ;; 对应的字符后， 自动将其转换为对应的中文标点。
     ((and (numberp punc-posit-before-1)
           (= punc-posit-before-1 0)
           (= char pyim-translate-trigger-char))
      (delete-char -1)
      (pyim-return-proper-punctuation punc-list-before-1 t))

     ;; 当光标前面为中文标点时， 按 `pyim-translate-trigger-char'
     ;; 对应的字符后， 自动将其转换为对应的英文标点。
     ((and (numberp punc-posit-before-1)
           (> punc-posit-before-1 0)
           (= char pyim-translate-trigger-char))
      (delete-char -1)
      (car punc-list-before-1))

     ;; 正常输入标点符号。
     (punc-list (pyim-return-proper-punctuation punc-list))

     ;; 当输入的字符不是标点符号时，原样插入。
     (t str))))

;; 切换光标处标点的样式（全角 or 半角）
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
          (insert (pyim-return-proper-punctuation punc-list t))
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

(defun pyim-restart ()
  "重启 Chinese-pyim，不建议用于编程环境。"
  (interactive)
  (let ((file-save-p
         (yes-or-no-p "正在重启 Chinese-pyim，需要保存 personal 文件的变动吗？ ")))
    (pyim-restart-1 file-save-p)))

(defun pyim-restart-1 (save-personal-file)
  "重启 Chinese-pyim，用于编程环境。"
  (pyim-start "Chinese-pyim" nil t save-personal-file))

(defun pyim-start (name &optional active-func restart save-personal-file)
  (interactive)
  (mapc 'kill-local-variable pyim-local-variable-list)
  (mapc 'make-local-variable pyim-local-variable-list)
  ;; 重启时，kill 所有已经打开的 buffer。
  (when (and restart save-personal-file)
    (pyim-save-personal-file))
  (when restart
    (pyim-kill-buffers)
    (setq pyim-buffer-list nil))
  (unless (and pyim-buffer-list
               (pyim-check-buffers)
               (not restart))
    (setq pyim-buffer-list (pyim-load-file))
    (pyim-make-char-table)
    (run-hooks 'pyim-load-hook)
    (message nil))

  (when pyim-automatic-generate-word
    (message "Chinese-pyim 自动组词功能已经开启，具体细节参考: `pyim-automatic-generate-word'。"))

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
  (if (functionp pyim-active-function)
      (funcall pyim-active-function))
  (when restart
    (message "Chinese-pyim 重启完成。")))

;;; 注册输入法
(register-input-method "chinese-pyim" "euc-cn" 'pyim-start "[pyim]")

;;;###autoload
(defun pyim-hanzi2pinyin (string &optional shou-zi-mu separator return-list ignore-duo-yin-zi)
  "将汉字字符串转换为对应的拼音字符串, 如果 `shou-zi-mu' 设置为t,转换仅得到拼音
首字母字符串。如果 `ignore-duo-yin-zi' 设置为t, 遇到多音字时，只使用第一个拼音。
其它拼音忽略。"
  (let (string-list pinyin-list output)

    ;; 确保 `pyim-char-table' 已经生成。
    (unless (pyim-get-char-code ?文)
      (pyim-make-char-table))

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
                                  (or (pyim-get-char-code (string-to-char str)) (list str)))
                                 ((> (length str) 0) (list str)))) string-list))

    ;; 通过排列组合的方式将 pinyin-list 转化为拼音字符串列表。
    (setq output
          (if ignore-duo-yin-zi
              (list (mapconcat 'identity
                               (mapcar
                                (lambda (x)
                                  (if shou-zi-mu
                                      (substring (car x) 0 1)
                                    (car x))) pinyin-list)
                               (or separator "")))
            (cl-remove-duplicates
             (let ((result '("")))
               (cl-loop for i in pinyin-list
                        do (setq result
                                 (cl-loop for j in i
                                          append (cl-loop for k in result
                                                          collect (concat k (if shou-zi-mu (substring j 0 1) j)
                                                                          (or separator "")))))) result)
             :test (lambda (x y) (or (null y) (equal x y)))
             :from-end t)))

    ;; 清理多余的拼音连接符，这个处理方式有点hack。需要优化。
    (setq output (mapcar (lambda (x)
                           (replace-regexp-in-string
                            "- " " " x)) output))
    (setq output (mapcar (lambda (x)
                           (replace-regexp-in-string
                            "-$" "" x)) output))
    (setq output (mapcar (lambda (x)
                           (replace-regexp-in-string
                            " -" " " x)) output))
    ;; 返回字符串或者列表
    (if return-list
        output
      (mapconcat 'identity output " "))))

;;;###autoload
(defun pyim-hanzi2pinyin-simple (string &optional shou-zi-mu separator return-list)
  "简化版的 `pyim-hanzi2pinyin', 不处理多音字。"
  (pyim-hanzi2pinyin string shou-zi-mu separator t))

;;;###autoload
(defun pyim-update-dict-file (&optional force sort-by-freq)
  "手动调整 Chinese-pyim 词库文件后，执行此命令可以：
1. 按照每行拼音对文件进行排序。
2. 删除重复的词条。

当我们明确无误的知道此命令的使用条件已经符合时。可以将 `force' 设置
为 t ，此时，就不需要用户进一步确认是否执行此命令。

当 `sort-by-freq' 设置位 t 时，删除每一行的重复词条之前，首先将词条按照
词条出现的频率大小排序，这个选项适用于：从文章构建词库，文章中词条出现
频率可以代表此词条的使用频率。"
  (interactive)
  (when (or force
            (yes-or-no-p "注意：当前 buffer *必须* 为词库文件 buffer，是否继续？"))
    (save-restriction
      (let ((lastw "")
            first-char total-char currw)
        (goto-char (point-min))
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
          (pyim-remove-duplicates-word sort-by-freq)
          (forward-line 1))
        (if (looking-at "^$")
            (delete-char -1))))))

(defun pyim-convert-current-line-to-dict-format ()
  "将当前行对应的汉语词条转换为 Chinese-pyim 可以识别的词库格式（ni-hao 你好）。"
  (interactive)
  (let (line-content pinyin-list insert-string)
    (setq line-content (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
    (setq line-content (replace-regexp-in-string "^ +\\| +$" "" line-content))
    (setq pinyin-list (pyim-hanzi2pinyin line-content nil "-" t))
    (delete-region (line-beginning-position) (line-end-position))
    (setq insert-string
          (mapconcat
           (lambda (x)
             ;; 拼音中不能有中文字符。
             ;; 中文词条中必须有中文字符，并且不能有ascii字符。
             (unless (or (string-match-p "[^a-z-]" x)
                         (string-match-p "[:ascii:]" line-content)
                         (not (string-match-p "\\cc" line-content)))
               (format "%s  %s" x line-content))) pinyin-list "\n"))
    (when (> (length insert-string) 1)
      (insert insert-string))))

;;;###autoload
(defun pyim-article2dict-chars ()
  "将一篇中文文章转换为 Chinese-pyim 可以识别的拼音词库。
这个命令只将文章中 *非词语* 中文字符转化为词库。

这个命令可以得到一篇文章中常用单字词语的词频信息。"
  (interactive)
  (pyim-article2dict 'chars))

;;;###autoload
(defun pyim-article2dict-words ()
  "将一篇中文文章转换为 Chinese-pyim 可以识别的拼音词库。
这个命令将文章中 *正确词语*，转化为词库。

这个命令使用频率很低，原因有两点：
1. 寻找准确的中文词条非常容易，一般不需要从一篇文章中通过分词的手段获得。
2. 文章很大时，这个命令运行速度太慢。

这个命令最大的用途就是为没有拼音的中文词库添加拼音code。"
  (interactive)
  (pyim-article2dict 'words))

;;;###autoload
(defun pyim-article2dict-misspell-words ()
  "将一篇中文文章转换为 Chinese-pyim 可以识别的拼音词库。
这个命令将文章中 *连续出现的独立汉字* 组合成中文字符串，
然后将其转化为词库，例如：

   “哪  狗  天”

会被转换为：

   “哪狗天”

有一句话说：“对的都一样，错的万万千”，对用户来说，这个命令可能
最有用处，它可以增加许多新词，也许这些新词毫无意义，但其代表了一种
输入习惯，可以提高输入体验。"
  (interactive)
  (pyim-article2dict 'misspell-words))

(defun pyim-article2dict (object)
  "将一篇中文文章转换为 Chinese-pyim 可以识别的拼音词库。
其步骤为：
1. 清除所有非汉语内容。
2. 使用分词系统将文章分词。
3. 将词条与词条之间用换行符分开。
4. 为每一行的词条添加拼音。"
  (save-excursion
    (pyim-show-help
     "将一篇中文文章转换为 Chinese-pyim 可以识别的拼音词库。
1. 准备材料：准备好所需要的中文文章，比如：一本网络小说，将其转换为文本文件。
2. 分词处理：使用分词工具将上述文件中的中文词语用空格分开，这里只介绍（jieba）结巴分词工具。
   1. 安装教程请参考： https://github.com/fxsjy/jieba
   2. 使用命令： python -m jieba -d \" \" 源文件.txt  > 目标文件.txt
   3. 命令帮助： python -m jieba --help
3. 添加拼音：使用 emacs 打开 “目标文件.txt”，然后运行命令：M-x pyim-build-dict-from-chinese-word
4. 保存文件

另外，使用分词工具的目的是确保中文词语与词语之间用 *空格* 强制隔开。比如：

    \"你好 吃饭 中文\"

分词这个步骤不是必须步骤，如果你获得的文件已经满足上述条件，那么直接运行当前命令就可以了。

注意事项：当文件很大时，这个命令需要执行较长时间，据估计：生成5万词条的词库大约需要15分钟。"))
  (when (yes-or-no-p "您上述准备工作是否已经完成？如果完成，请输入 yes 继续执行命令：")
    (let ((file (read-file-name "请选择需要转换的文本文件：")))
      (with-temp-buffer
        (insert-file-contents file)
        ;; 删除所有英文单词以及标点符号。
        (goto-char (point-min))
        (while (re-search-forward "[[:punct:]a-zA-Z0-9]+" nil t)
          (replace-match "\n"))
        ;; 当 `accuracy' 为 nil 时，`pyim-article2dict' 会将连续出现的
        ;; 单个汉字字符合并成汉字字符串，比如： “哪  狗  天” 会被转换
        ;; 为 “哪狗天”。增加词条量的同时也会产生许多无意义的词汇。
        (cond ((eq object 'chars)
               (goto-char (point-min))
               (while (re-search-forward "\\cc\\cc+" nil t)
                 (replace-match ""))
               ;; 将词条使用换行符隔开。
               (goto-char (point-min))
               (while (re-search-forward "[[:blank:]]+" nil t)
                 (replace-match "\n")))
              ((eq object 'words)
               (goto-char (point-min))
               ;; 删除所有单个汉字字符，单个汉字字符的拼音词库非常容易获得。
               ;; 将其删除后，将极大的减少词库转换时间。
               (while (re-search-forward "\\CC\\cc\\CC" nil t)
                 (replace-match "\n"))
               ;; 将词条使用换行符隔开。
               (goto-char (point-min))
               (while (re-search-forward "[[:blank:]]+" nil t)
                 (replace-match "\n"))
               (goto-char (point-min))
               (while (re-search-forward "\n\\cc\n" nil t)
                 (replace-match "\n")))
              ((eq object 'misspell-words)
               (goto-char (point-min))
               ;; 删除现有词条，只保留单个汉语字符，将单个的汉语字符
               ;; 组成字符串后，有可能得到新的词语，虽然这些词语可能
               ;; 没有实际意义，但可以提升拼音输入法的体验。
               (while (re-search-forward "\\cc\\cc+" nil t)
                 (replace-match "\n"))
               (goto-char (point-min))
               (while (re-search-forward "[[:blank:]]+" nil t)
                 (replace-match ""))
               (goto-char (point-min))
               (while (re-search-forward "[[:blank:]\n]+\\cc[[:blank:]\n]+" nil t)
                 (replace-match ""))
               (goto-char (point-min))
               ;; 删除大于4个字符的中文字符串，没什么用处。
               (while (re-search-forward "\\cc\\{5,\\}" nil t)
                 (replace-match "\n"))))
        ;; 删除多余空白行。
        (goto-char (point-min))
        (while (re-search-forward "\n+" nil t)
          (replace-match "\n"))
        ;; `pyim-article2dict' 处理大文件时运行时间很长
        ;; 分阶段保存内容可以防止数据丢失。
        (pyim-article2dict-write-stage-file file "CleanStage-" t)
        ;; 为每一行的词条添加拼音code
        (goto-char (point-min))
        (while (not (eobp))
          (pyim-convert-current-line-to-dict-format)
          (forward-line 1))
        (pyim-article2dict-write-stage-file file "ConvertStage-" t)
        ;; 将文件按行排序，并删除重复的词条，运行两次。
        (pyim-update-dict-file t t)
        (pyim-article2dict-write-stage-file file "SortStage-" t)
        (pyim-update-dict-file t t)
        (pyim-article2dict-write-stage-file file "FinishStage-" t)))))

(defun pyim-article2dict-write-stage-file (file stage force)
  "将当前 buffer 的内容另存为一个 stage 文件。
用于 `pyim-article2dict' 分阶段保存内容。"
  (let ((file (expand-file-name file))
        stage-file)
    (when (and file stage force)
      (setq stage-file
            (concat (file-name-directory file)
                    (make-temp-name stage) "-"
                    (file-name-nondirectory file)))
      (write-region (point-min) (point-max) stage-file)
      (message "将此阶段转换的结果另存为文件：%s" stage-file))))

(provide 'chinese-pyim)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; chinese-pyim.el ends here
