;;; pyim.el --- A Chinese input method support quanpin, shuangpin, wubi and cangjie.        -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2015-2021 Free Software Foundation, Inc.

;; Author: Ye Wenbin <wenbinye@163.com>
;;         Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/pyim
;; Version: 3.6
;; Keywords: convenience, Chinese, pinyin, input-method
;; Package-Requires: ((emacs "24.4") (async "1.6") (xr "1.13"))

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

;; * pyim 使用说明                                          :README:doc:
;; ** 截图
;; [[./snapshots/pyim-linux-x-with-toolkit.png]]

;; ** 简介
;; pyim 是 Emacs 环境下的一个中文输入法，最初它只支持全拼输入，所以当时
;; "pyim" 代表 "Chinese Pinyin Input Method" 的意思，后来根据同学的提议，
;; 添加了五笔的支持，再叫 “拼音输入法” 就不太合适了，所以你现在可以将它理解
;; 为 “PengYou input method”： 平时像朋友一样帮助你，偶尔也像朋友一样犯二 。。。

;; ** 背景
;; pyim 的代码源自 emacs-eim。

;; emacs-eim 是 Emacs 环境下的一个中文输入法框架， 支持拼音，五笔，仓颉以及二笔等
;; 多种输入法，但遗憾的是，2008 年之后它就停止了开发，我认为主要原因是外部中文输入法快速发展。

;; 虽然外部输入法功能强大，但不能和 Emacs 默契的配合，这一点极大的损害了 Emacs 那种 *行云流水*
;; 的感觉。而本人在使用（或者叫折腾） emacs-eim 的过程中发现：

;; 1. *当 emacs-eim 词库词条超过 100 万时，选词频率大大降低，中文体验增强。*
;; 3. *随着使用时间的延长，emacs-eim 会越来越好用（个人词库的积累）。*

;; 于是我 fork 了 emacs-eim 输入法的部分代码, 创建了一个项目：pyim。

;; ** 目标
;; pyim 的目标是： *尽最大的努力成为一个好用的 Emacs 中文输入法* ，
;; 具体可表现为三个方面：

;; 1. Fallback:     当外部输入法不能使用时，比如在 console 或者 cygwin 环境
;;    下，尽最大可能让 Emacs 用户不必为输入中文而烦恼。
;; 2. Integration:  尽最大可能减少输入法切换频率，让中文输入不影响 Emacs
;;    的体验。
;; 3. Exchange:     尽最大可能简化 pyim 使用其他优秀输入法的词库
;;    的难度和复杂度。

;; ** 特点
;; 1. pyim 支持全拼，双拼，五笔和仓颉，其中对全拼的支持最好。
;; 2. pyim 通过添加词库的方式优化输入法。
;; 3. pyim 使用文本词库格式，方便处理。

;; ** 安装
;; 1. 配置 melpa 源，参考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET pyim RET
;; 3. 在 Emacs 配置文件中（比如: ~/.emacs）添加如下代码：
;;    #+BEGIN_EXAMPLE
;;    (require 'pyim)
;;    (require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
;;    (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
;;    (setq default-input-method "pyim")
;;    #+END_EXAMPLE

;; ** 配置

;; *** 配置实例
;; 对 pyim 感兴趣的同学，可以看看本人的 pyim 配置（总是适用于最新版的 pyim）:

;; #+BEGIN_EXAMPLE
;; (use-package pyim
;;   :ensure nil
;;   :demand t
;;   :config
;;   ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
;;   (use-package pyim-basedict
;;     :ensure nil
;;     :config (pyim-basedict-enable))

;;   (setq default-input-method "pyim")

;;   ;; 我使用全拼
;;   (setq pyim-default-scheme 'quanpin)

;;   ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;;   ;; 我自己使用的中英文动态切换规则是：
;;   ;; 1. 光标只有在注释里面时，才可以输入中文。
;;   ;; 2. 光标前是汉字字符时，才能输入中文。
;;   ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;;   (setq-default pyim-english-input-switch-functions
;;                 '(pyim-probe-dynamic-english
;;                   pyim-probe-isearch-mode
;;                   pyim-probe-program-mode
;;                   pyim-probe-org-structure-template))

;;   (setq-default pyim-punctuation-half-width-functions
;;                 '(pyim-probe-punctuation-line-beginning
;;                   pyim-probe-punctuation-after-punctuation))

;;   ;; 开启拼音搜索功能
;;   (pyim-isearch-mode 1)

;;   ;; 使用 posframe 绘制 page, (需要用户手动安装 posframe 包）。
;;   ;; (setq pyim-page-tooltip 'posframe)

;;   ;; 如果 posframe 不可用，可以试着安装 popup 包，然后设置：
;;   ;; ;; (setq pyim-page-tooltip 'popup)

;;   ;; 选词框显示5个候选词
;;   (setq pyim-page-length 5)

;;   :bind
;;   (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
;;    ("C-;" . pyim-delete-word-from-personal-buffer)))
;; #+END_EXAMPLE

;; *** 添加词库文件
;; pyim 当前的默认的拼音词库是 pyim-basedict, 这个词库的词条量
;; 8 万左右，是一个 *非常小* 的拼音词库，词条的来源有两个：

;; 1. libpinyin 项目的内置词库
;; 2. pyim 用户贡献的个人词库

;; 如果 pyim-basedict 不能满足需求，用户可以使用其他方式为 pyim 添加拼音词库，
;; 具体方式请参考 [[如何添加自定义拼音词库]] 小结。

;; *** 激活 pyim

;; #+BEGIN_EXAMPLE
;; (setq default-input-method "pyim")
;; (global-set-key (kbd "C-\\") 'toggle-input-method)
;; #+END_EXAMPLE

;; ** 使用
;; *** 常用快捷键
;; | 输入法快捷键          | 功能                       |
;; |-----------------------+----------------------------|
;; | C-n 或 M-n 或 + 或 .  | 向下翻页                   |
;; | C-p 或 M-p 或 - 或 ,  | 向上翻页                   |
;; | C-f                   | 选择下一个备选词           |
;; | C-b                   | 选择上一个备选词           |
;; | SPC                   | 确定输入                   |
;; | RET 或 C-m            | 字母上屏                   |
;; | C-c                   | 取消输入                   |
;; | C-g                   | 取消输入并保留已输入的中文 |
;; | TAB                   | 模糊音调整                 |
;; | DEL 或 BACKSPACE      | 删除最后一个字符           |
;; | C-DEL 或  C-BACKSPACE | 删除最后一个拼音           |
;; | M-DEL 或  M-BACKSPACE | 删除最后一个拼音           |

;; *** 使用双拼模式
;; pyim 支持双拼输入模式，用户可以通过变量 `pyim-default-scheme' 来设定：

;; #+BEGIN_EXAMPLE
;; (setq pyim-default-scheme 'pyim-shuangpin)
;; #+END_EXAMPLE

;; 注意：
;; 1. pyim 支持微软双拼（microsoft-shuangpin）和小鹤双拼（xiaohe-shuangpin）。
;; 2. 用户可以使用函数 `pyim-scheme-add' 添加自定义双拼方案。
;; 3. 用户可能需要重新设置 `pyim-translate-trigger-char'。

;; *** 使用 rime 输入法
;; 具体安装和使用方式请查看 pyim-liberime 包的 Commentary 部分。

;; *** 使用五笔输入
;; pyim 支持五笔输入模式，用户可以通过变量 `pyim-default-scheme' 来设定：

;; #+BEGIN_EXAMPLE
;; (setq pyim-default-scheme 'wubi)
;; #+END_EXAMPLE

;; 在使用五笔输入法之前，请用 pyim-dicts-manager 添加一个五笔词库，词库的格式类似：

;; #+BEGIN_EXAMPLE
;;; -*- coding: utf-8-unix -*-
;; .aaaa 工
;; .aad 式
;; .aadk 匿
;; .aadn 慝 葚
;; .aadw 萁
;; .aafd 甙
;; .aaff 苷
;; .aaht 芽
;; .aak 戒
;; #+END_EXAMPLE

;; 最简单的方式是从 melpa 中安装 pyim-wbdict 包，然后根据它的
;; [[https://github.com/tumashu/pyim-wbdict][README]] 来配置。

;; 另外 Ye FeiYu 同学维护着 pyim-wbdict 的一个 fork, 里面包含着极点
;; 五笔和清歌五笔的词库，不做发布，有兴趣的同学可以了解一下：

;;     https://github.com/yefeiyu/pyim-wbdict

;; 如果用户在使用五笔输入法的过程中，忘记了某个字的五笔码，可以按 TAB
;; 键临时切换到辅助输入法来输入，选词完成之后自动退出。辅助输入法可以
;; 通过 `pyim-assistant-scheme' 来设置。

;; *** 使用仓颉输入法
;; pyim 支持仓颉输入法，用户可以通过变量 `pyim-default-scheme' 来设定：

;; #+BEGIN_EXAMPLE
;; (setq pyim-default-scheme 'cangjie)
;; #+END_EXAMPLE

;; 在使用仓颉输入法之前，请用 pyim-dicts-manager 添加一个仓颉词库，词库的格式类似：

;; #+BEGIN_EXAMPLE
;;; -*- coding: utf-8-unix -*-
;; @a 日
;; @a 曰
;; @aa 昌
;; @aa 昍
;; @aaa 晶
;; @aaa 晿
;; @aaah 曑
;; #+END_EXAMPLE

;; 如果用户使用仓颉第五代，最简单的方式是从 melpa 中安装 pyim-cangjie5dict 包，
;; 然后根据它的 [[https://github.com/p1uxtar/pyim-cangjie5dict][README]] 来配置。
;; pyim 支持其它版本的仓颉，但需要用户自己创建词库文件。

;; 用户可以使用命令：`pyim-search-word-code' 来查询当前选择词条的仓颉编码

;; *** 使用三码郑码（至至郑码）输入法
;; 具体细节参考：https://github.com/p1uxtar/pyim-smzmdict

;; *** 让选词框跟随光标
;; 用户可以通过下面的设置让 pyim 在 *光标处* 显示一个选词框：

;; 1. 使用 popup 包来绘制选词框 （emacs overlay 机制）
;;    #+BEGIN_EXAMPLE
;;    (setq pyim-page-tooltip 'popup)
;;    #+END_EXAMPLE
;; 2. 使用 posframe 来绘制选词框
;;    #+BEGIN_EXAMPLE
;;    (setq pyim-page-tooltip 'posframe)
;;    #+END_EXAMPLE
;;    注意：pyim 不会自动安装 posframe, 用户需要手动安装这个包，

;; *** 调整 tooltip 选词框的显示样式
;; pyim 的 tooltip 选词框默认使用 *双行显示* 的样式，在一些特
;; 殊的情况下（比如：popup 显示的菜单错位），用户可以使用 *单行显示*
;; 的样式：

;; #+BEGIN_EXAMPLE
;; (setq pyim-page-style 'one-line)
;; #+END_EXAMPLE

;; 注：用户可以添加函数 pyim-page-style:STYLENAME 来定义自己的选词框格式。

;; *** 设置模糊音
;; 可以通过设置 `pyim-fuzzy-pinyin-alist' 变量来自定义模糊音。

;; *** 使用魔术转换器
;; 用户可以将待选词条作 “特殊处理” 后再 “上屏”，比如 “简体转繁体” 或者
;; “输入中文，上屏英文” 之类的。

;; 用户需要设置 `pyim-magic-converter', 比如：下面这个例子实现，
;; 输入 “二呆”，“一个超级帅的小伙子” 上屏 :-)
;; #+BEGIN_EXAMPLE
;; (defun my-converter (string)
;;   (if (equal string "二呆")
;;       "“一个超级帅的小伙子”"
;;     string))
;; (setq pyim-magic-converter #'my-converter)
;; #+END_EXAMPLE

;; *** 切换全角标点与半角标点

;; 1. 第一种方法：使用命令 `pyim-punctuation-toggle'，全局切换。
;;    这个命令主要用来设置变量： `pyim-punctuation-translate-p', 用户也可以
;;    手动设置这个变量， 比如：
;;    #+BEGIN_EXAMPLE
;;    (setq pyim-punctuation-translate-p '(yes no auto))   ;使用全角标点。
;;    (setq pyim-punctuation-translate-p '(no yes auto))   ;使用半角标点。
;;    (setq pyim-punctuation-translate-p '(auto yes no))   ;中文使用全角标点，英文使用半角标点。
;;    #+END_EXAMPLE
;; 2. 第二种方法：使用命令 `pyim-punctuation-translate-at-point' 只切换光
;;    标处标点的样式。
;; 3. 第三种方法：设置变量 `pyim-translate-trigger-char' ，输入变量设定的
;;    字符会切换光标处标点的样式。

;; *** 手动加词和删词

;; 1. `pyim-create-Ncchar-word-at-point 这是一组命令，从光标前提取N个汉
;;    字字符组成字符串，并将其加入个人词库。
;; 2. `pyim-translate-trigger-char' 以默认设置为例：在“我爱吃红烧肉”后输
;;    入“5v” 可以将“爱吃红烧肉”这个词条保存到用户个人词库。
;; 3. `pyim-create-word-from-selection', 选择一个词条，运行这个命令后，就
;;    可以将这个词条添加到个人词库。
;; 4. `pyim-delete-word' 从个人词库中删除当前高亮选择的词条。

;; *** pyim 高级功能
;; 1. 根据环境自动切换到英文输入模式，使用 pyim-english-input-switch-functions 配置。
;; 2. 根据环境自动切换到半角标点输入模式，使用 pyim-punctuation-half-width-functions 配置。

;; 注意：上述两个功能使用不同的变量设置， *千万不要搞错* 。

;; **** 根据环境自动切换到英文输入模式

;; | 探针函数                          | 功能说明                                                                          |
;; |-----------------------------------+-----------------------------------------------------------------------------------|
;; | pyim-probe-program-mode           | 如果当前的 mode 衍生自 prog-mode，那么仅仅在字符串和 comment 中开启中文输入模式   |
;; |-----------------------------------+-----------------------------------------------------------------------------------|
;; | pyim-probe-org-speed-commands     | 解决 org-speed-commands 与 pyim 冲突问题                                          |
;; | pyim-probe-isearch-mode           | 使用 isearch 搜索时，强制开启英文输入模式                                         |
;; |                                   | 注意：想要使用这个功能，pyim-isearch-mode 必须激活                                |
;; |-----------------------------------+-----------------------------------------------------------------------------------|
;; | pyim-probe-org-structure-template | 使用 org-structure-template 时，关闭中文输入模式                                  |
;; |-----------------------------------+-----------------------------------------------------------------------------------|
;; |                                   | 1. 当前字符为中文字符时，输入下一个字符时默认开启中文输入                         |
;; | pyim-probe-dynamic-english        | 2. 当前字符为其他字符时，输入下一个字符时默认开启英文输入                         |
;; |                                   | 3. 使用命令 pyim-convert-string-at-point 可以将光标前的拼音字符串强制转换为中文。 |
;; |-----------------------------------+-----------------------------------------------------------------------------------|

;; 激活方式：

;; #+BEGIN_EXAMPLE
;; (setq-default pyim-english-input-switch-functions
;;               '(probe-function1 probe-function2 probe-function3))
;; #+END_EXAMPLE

;; 注意事项：
;; 1. 上述函数列表中，任意一个函数的返回值为 t 时，pyim 切换到英文输入模式。
;; 2. [[https://github.com/DogLooksGood/emacs-rime][Emacs-rime]] 和 [[https://github.com/laishulu/emacs-smart-input-source][smart-input-source]]
;;    也有类似探针的功能，其对应函数可以直接或者简单包装后作为 pyim 探针使用，有兴趣的同学可以了解一下。

;; **** 根据环境自动切换到半角标点输入模式

;; | 探针函数                                 | 功能说明                   |
;; |------------------------------------------+----------------------------|
;; | pyim-probe-punctuation-line-beginning    | 行首强制输入半角标点       |
;; |------------------------------------------+----------------------------|
;; | pyim-probe-punctuation-after-punctuation | 半角标点后强制输入半角标点 |
;; |------------------------------------------+----------------------------|

;; 激活方式：

;; #+BEGIN_EXAMPLE
;; (setq-default pyim-punctuation-half-width-functions
;;               '(probe-function4 probe-function5 probe-function6))
;; #+END_EXAMPLE

;; 注：上述函数列表中，任意一个函数的返回值为 t 时，pyim 切换到半角标点输入模式。

;; ** 捐赠
;; 您可以通过小额捐赠的方式支持 pyim 的开发工作，具体方式：

;; 1. 通过支付宝收款账户：tumashu@163.com
;; 2. 通过支付宝钱包扫描：

;;    [[file:snapshots/QR-code-for-author.jpg]]


;; ** Tips

;; *** 关闭输入联想词功能 (默认开启)

;; #+BEGIN_EXAMPLE
;; (setq pyim-enable-shortcode nil)
;; #+END_EXAMPLE

;; *** 如何将个人词条相关信息导入和导出？

;; 1. 导入使用命令： pyim-import
;; 2. 导出使用命令： pyim-export

;; *** pyim 出现错误时，如何开启 debug 模式

;; #+BEGIN_EXAMPLE
;; (setq debug-on-error t)
;; #+END_EXAMPLE

;; *** 如何查看 pyim 文档。
;; pyim 的文档隐藏在 comment 中，如果用户喜欢阅读 html 格式的文档，
;; 可以查看在线文档；

;;   http://tumashu.github.io/pyim/

;; *** 将光标处的拼音或者五笔字符串转换为中文 (与 vimim 的 “点石成金” 功能类似)
;; #+BEGIN_EXAMPLE
;; (global-set-key (kbd "M-i") 'pyim-convert-string-at-point)
;; #+END_EXAMPLE

;; *** 如何使用其它字符翻页
;; #+BEGIN_EXAMPLE
;; (define-key pyim-mode-map "." 'pyim-page-next-page)
;; (define-key pyim-mode-map "," 'pyim-page-previous-page)
;; #+END_EXAMPLE

;; *** 如何用 ";" 来选择第二个候选词
;; #+BEGIN_EXAMPLE
;; (define-key pyim-mode-map ";"
;;   (lambda ()
;;     (interactive)
;;     (pyim-page-select-word-by-number 2)))
;; #+END_EXAMPLE

;; *** 如何添加自定义拼音词库
;; pyim 默认没有携带任何拼音词库，用户可以使用下面几种方式，获取
;; 质量较好的拼音词库：

;; **** 第一种方式 (懒人推荐使用)

;; 获取其他 pyim 用户的拼音词库，比如，某个同学测试 pyim
;; 时创建了一个中文拼音词库，词条数量大约60万。

;;    http://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim.gz

;; 下载上述词库后，运行 `pyim-dicts-manager' ，按照命令提示，将下载得到的词库
;; 文件信息添加到 `pyim-dicts' 中，最后运行命令 `pyim-restart' 或者重启
;; emacs，这个词库使用 `utf-8-unix' 编码。

;; **** 第二种方式 (Windows 用户推荐使用)

;; 使用词库转换工具将其他输入法的词库转化为pyim使用的词库：这里只介绍windows平
;; 台下的一个词库转换软件：

;; 1. 软件名称： imewlconverter
;; 2. 中文名称： 深蓝词库转换
;; 3. 下载地址： https://github.com/studyzy/imewlconverter
;; 4. 依赖平台： Microsoft .NET Framework (>= 3.5)

;; 使用方式：

;; [[file:snapshots/imewlconverter-basic.gif]]

;; 如果生成的词库词频不合理，可以按照下面的方式处理（非常有用的功能）：

;; [[file:snapshots/imewlconverter-wordfreq.gif]]

;; 生成词库后，运行 `pyim-dicts-manager' ，按照命令提示，将转换得到的词库文件的信息添加到 `pyim-dicts' 中，
;; 完成后运行命令 `pyim-restart' 或者重启emacs。

;; **** 第三种方式 (Linux & Unix 用户推荐使用)
;; E-Neo 同学编写了一个词库转换工具: [[https://github.com/E-Neo/scel2pyim][scel2pyim]] ,
;; 可以将一个搜狗词库转换为 pyim 词库。

;; 1. 软件名称： scel2pyim
;; 2. 下载地址： https://github.com/E-Neo/scel2pyim
;; 3. 编写语言： C语言

;; *** 如何手动安装和管理词库
;; 这里假设有两个词库文件：

;; 1. /path/to/pyim-dict1.pyim
;; 2. /path/to/pyim-dict2.pyim

;; 在~/.emacs文件中添加如下一行配置。

;; #+BEGIN_EXAMPLE
;; (setq pyim-dicts
;;       '((:name "dict1" :file "/path/to/pyim-dict1.pyim")
;;         (:name "dict2" :file "/path/to/pyim-dict2.pyim")))
;; #+END_EXAMPLE

;; 注意事项:
;; 1. 只有 :file 是 *必须* 设置的。
;; 2. 必须使用词库文件的绝对路径。
;; 3. 词库文件的编码必须为 utf-8-unix，否则会出现乱码。

;; *** Emacs 启动时加载 pyim 词库

;; #+BEGIN_EXAMPLE
;; (add-hook 'emacs-startup-hook
;;           #'(lambda () (pyim-restart-1 t)))
;; #+END_EXAMPLE

;; *** 将汉字字符串转换为拼音字符串
;; 下面两个函数可以将中文字符串转换的拼音字符串或者列表，用于 emacs-lisp
;; 编程。

;; 1. `pyim-cstring-to-pinyin' （考虑多音字）
;; 2. `pyim-cstring-to-pinyin-simple'  （不考虑多音字）

;; *** 中文分词
;; pyim 包含了一个简单的分词函数：`pyim-cstring-split-to-list', 可以
;; 将一个中文字符串分成一个词条列表，比如：

;; #+BEGIN_EXAMPLE
;;                   (("天安" 5 7)
;; 我爱北京天安门 ->  ("天安门" 5 8)
;;                    ("北京" 3 5)
;;                    ("我爱" 1 3))
;; #+END_EXAMPLE

;; 其中，每一个词条列表中包含三个元素，第一个元素为词条本身，第二个元素为词条
;; 相对于字符串的起始位置，第三个元素为词条结束位置。

;; 另一个分词函数是 `pyim-cstring-split-to-string', 这个函数将生成一个新的字符串，
;; 在这个字符串中，词语之间用空格或者用户自定义的分隔符隔开。

;; 注意，上述两个分词函数使用暴力匹配模式来分词，所以， *不能检测出* pyim
;; 词库中不存在的中文词条。

;; *** 获取光标处的中文词条
;; pyim 包含了一个简单的命令：`pyim-cstring-words-at-point', 这个命令
;; 可以得到光标处的 *英文* 或者 *中文* 词条的 *列表*，这个命令依赖分词函数：
;; `pyim-cstring-split-to-list'。

;; *** 让 `forward-word' 和 `back-backward’ 在中文环境下正常工作
;; 中文词语没有强制用空格分词，所以 Emacs 内置的命令 `forward-word' 和 `backward-word'
;; 在中文环境不能按用户预期的样子执行，而是 forward/backward “句子” ，pyim
;; 自带的两个命令可以在中文环境下正常工作：

;; 1. `pyim-forward-word
;; 2. `pyim-backward-word

;; 用户只需将其绑定到快捷键上就可以了，比如：

;; #+BEGIN_EXAMPLE
;; (global-set-key (kbd "M-f") 'pyim-forward-word)
;; (global-set-key (kbd "M-b") 'pyim-backward-word)
;; #+END_EXAMPLE

;; *** 为 isearch 相关命令添加拼音搜索支持
;; pyim 安装后，可以通过下面的设置开启拼音搜索功能：

;; #+BEGIN_EXAMPLE
;; (pyim-isearch-mode 1)
;; #+END_EXAMPLE

;; 注意：这个功能有一些限制，搜索字符串中只能出现 “a-z” 和 “’”，如果有
;; 其他字符（比如 regexp 操作符），则自动关闭拼音搜索功能。

;; 开启这个功能后，一些 isearch 扩展有可能失效，如果遇到这种问题，
;; 只能禁用这个 Minor-mode，然后联系 pyim 的维护者，看有没有法子实现兼容。

;; 用户激活这个 mode 后，可以使用下面的方式 *强制关闭* isearch 搜索框中文输入
;; （即使在 pyim 激活的时候）。

;; #+BEGIN_EXAMPLE
;; (setq-default pyim-english-input-switch-functions
;;               '(pyim-probe-isearch-mode))
;; #+END_EXAMPLE

;; *** 让 ivy 支持拼音搜索候选项功能
;; #+BEGIN_EXAMPLE
;; (setq ivy-re-builders-alist
;;       '((t . pyim-ivy-cregexp)))
;; #+END_EXAMPLE

;; *** 让 vertico, selectrum 等补全框架，通过 orderless 支持拼音搜索候选项功能。
;; #+BEGIN_EXAMPLE
;; (defun my-orderless-regexp (orig_func component)
;;   (let ((result (funcall orig_func component)))
;;     (pyim-cregexp-build result)))

;; (advice-add 'orderless-regexp :around #'my-orderless-regexp)
;; #+END_EXAMPLE


;;; Code:

;; * 核心代码                                                           :code:
;; ** require + defcustom + defvar
(require 'subr-x)
(require 'cl-lib)
(require 'help-mode)
(require 'popup nil t)
(require 'posframe nil t)
(require 'pyim-pymap)
(require 'pyim-common)
(require 'pyim-pinyin)
(require 'pyim-punctuation)
(require 'pyim-dict)
(require 'pyim-dcache)
(require 'pyim-scheme)
(require 'pyim-page)
(require 'pyim-candidates)

(defgroup pyim nil
  "Pyim is a Chinese input method support quanpin, shuangpin, wubi and cangjie."
  :group 'leim)

(defcustom pyim-enable-shortcode t
  "启用输入联想词功能."
  :type 'boolean)

(defcustom pyim-translate-trigger-char "v"
  "用于触发特殊操作的字符，相当与单字快捷键.

输入中文的时候，我们需要快速频繁的执行一些特定的命令，最直接的方
法就是将上述命令绑定到一个容易按的快捷键上，但遗憾的是 emacs 大多
数容易按的快捷键都 *名花有主* 了，甚至找一个 “Ctrl＋单字符”的快
捷键都不太容易，特殊功能触发字符，可以帮助我们实现“单字符”快捷
键，类似 org-mode 的 speed key。

默认情况下，我们可以使用特殊功能触发字符执行下面几个操作（假设触
发字符为 v）：

1. 快速切换中英文标点符号的样式：当光标前的字符是一个标点符号时，
   按 \"v\" 可以切换这个标点的样式。比如：光标在A处的时候，按
   \"v\" 可以将A前面的全角逗号转换为半角逗号。

        你好，-A-

   按 \"v\" 后

        你好,-A-

2. 快速将光标前的词条添加到词库：当光标前的字符是中文字符时，按
   \"num\" + \"v\" 可以将光标前 num 个中文汉字组成的词条添加到个
   人词频文件中，比如：当光标在A处时，按\"4v\"可以将“的红烧肉”
   这个词条加入个人词频文件，默认num不超过9。

        我爱吃美味的红烧肉-A-

值得注意的是，这种方式如果添加的功能太多，会造成许多潜在的冲突。

用户可以使用变量 `pyim-translate-trigger-char' 来设置触发字符，默
认的触发字符是：\"v\", 选择这个字符的理由基于全拼输入法的：

1. \"v\" 不是有效的声母，不会对中文输入造成太大的影响。
2. \"v\" 字符很容易按。

pyim 使用函数 `pyim-translate' 来处理特殊功能触发字符。当待输入的
字符是触发字符时，`pyim-translate' 根据光标前的字符的不同来调用不
同的功能，具体见 `pyim-translate' ：

单字快捷键受到输入法方案的限制，比如：全拼输入法可以将其设置为v,
但双拼输入法下设置 v 可能就不行，所以，pyim 首先会检查当前输入法
方案下，这个快捷键设置是否合理有效，如果不是一个合理的设置，则使
用拼音方案默认的 :prefer-trigger-chars 。

具体请参考 `pyim-translate-get-trigger-char' 。"
  :type '(choice (const nil) string))

(defcustom pyim-exhibit-delay-ms 0
  "输入或者删除拼音字符后等待多少毫秒后才显示可选词
当用户快速输入连续的拼音时可提升用户体验.
如果为 0 或者 nil, 则不等待立刻显示可选词."
  :type 'integer)

(defcustom pyim-fuzzy-pinyin-alist
  '(("en" "eng")
    ("in" "ing")
    ("un" "ong"))
  "设定模糊音."
  :type 'sexp)

(defface pyim-preview-face '((t (:underline t)))
  "设置光标处预览字符串的 face.")

(defcustom pyim-english-input-switch-functions nil
  "让 pyim 开启英文输入功能.

这个变量的取值为一个函数列表，这个函数列表中的任意一个函数的
运行结果为 t 时，pyim 开启英文输入功能。"
  :type 'symbol)

(defcustom pyim-wash-function 'pyim-wash-current-line-function
  "清洗光标前面的文字内容.
这个函数与『单字快捷键配合使用』，当光标前面的字符为汉字字符时，
按 `pyim-translate-trigger-char' 对应字符，可以调用这个函数来清洗
光标前面的文字内容。"
  :type 'function)

(defcustom pyim-page-select-finish-hook nil
  "Pyim 选词完成时运行的 hook."
  :type 'hook)

(defcustom pyim-page-select-word-by-number t
  "使用数字键来选择词条.

如果设置为 nil, 将直接输入数字，适用于使用数字做为
编码的输入法。"
  :type 'boolean)

(defcustom pyim-magic-converter nil
  "将 “待选词条” 在 “上屏” 之前自动转换为其他字符串.
这个功能可以实现“简转繁”，“输入中文得到英文”之类的功能。"
  :type 'boolean)

(defcustom pyim-autoselector '(pyim-autoselector-xingma)
  "已经启用的自动上屏器.

自动上屏器是一个函数。假设用户已经输入 \"nihao\", 并按下 \"m\" 键，
那么当前entered 就是 \"nihaom\". 上次 entered 是 \"nihao\". 那么
返回值有3种情况（优先级按照下面的顺序）：

1. (:select last :replace-with \"xxx\")    自动上屏上次 entered (nihao) 的第一个候选词，m 键下一轮处理。
3. (:select current :replace-with \"xxx\") 自动上屏当前 entered (nihaom) 的第一个候选词。
4. nil                                     不自动上屏。

如果 :replace-with 设置为一个字符串，则选择最终会被这个字符串替代。

注意：多个 autoselector 函数运行时，最好不要相互影响，如果相互有
影响，需要用户自己管理。"
  :type '(choice (const nil)
                 (repeat function)))

;;;###autoload
(defvar pyim-titles '("PYIM " "PYIM-EN " "PYIM-AU ") "Pyim 在 mode-line 中显示的名称.")

(defvar pyim-entered-buffer " *pyim-entered-buffer*"
  "一个 buffer，用来处理用户已经输入的字符串： entered。

用户 *已经* 输入的字符组成的字符串，在 pyim 里面，叫做 entered,
说白了就是 input, 选择 entered 而不选择 input 的原因是：

1. input 太常见了， 和其它词语组和起来容易产生歧义，比如：
   pyim-entered-output 就比 pyim-input-output 更加容易理解。
2. entered 这个词语很少见，只要明白它代表的概念，就不容易产生混乱。

pyim 使用一个 buffer 来处理 entered, 以实现 “用户输入字符串” 编
辑等高级功能：

1. 如果输入的字符串有错误，可以修改，不用取消重新输入；
2. 如果光标不在行首，pyim 只使用光标前的字符串来查找词条，
   如果词条上屏，词条对应的输入就从 buffer 中清除，然后
   继续处理后面的输入，这种方式方便长词的输入；
3. 如果光标在行首，则处理整行。")

(defvar pyim-imobjs nil
  "Imobj (Input method object) 组成的 list.

imobj 在 pyim 里面的概念，类似与编译器里面的语法树，
它代表 pyim 输入的字符串 entered 解析得到的一个结构化对象，
以全拼输入法的为例：

1. entered: nihaoma
2. imobj: ((\"n\" \"i\" \"n\" \"i\") (\"h\" \"ao\" \"h\" \"ao\") (\"m\" \"a\" \"m\" \"a\"))

而 imobjs 是 imobj 组成的一个列表，因为有模糊音等概念的存在，一个
entered 需要以多种方式或者多步骤解析，得到多种可能的 imobj, 这些
imobj 组合构成在一起，构成了 imobjs 这个概念。比如：

1. entered: guafeng (设置了模糊音 en -> eng)
2. imobj-1: ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"en\" \"f\" \"eng\"))
3. imobj-2: ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"eng\" \"f\" \"eng\"))
4. imobjs:  (((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"en\" \"f\" \"eng\"))
             ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"eng\" \"f\" \"eng\")))

这个变量用来保存解析得到的 imobjs。

解析完成之后，pyim 会为每一个 imobj 创建对应 code 字符串, 然后在词库
中搜索 code 字符串来得到所需要的词条，最后使用特定的方式将得到的
词条组合成一个候选词列表：`pyim-candidates' 并通过 pyim-page 相关
功能来显示选词框，供用户选择词条，比如：

1. imobj: ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"en\" \"f\" \"en\"))
2. code: gua-fen

从上面的说明可以看出，imobj 本身也是有结构的：

1. imobj: ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"en\" \"f\" \"en\"))

我们将 (\"g\" \"ua\" \"g\" \"ua\") 这些子结构，叫做 imelem (IM element), *大
多数情况下*, 一个 imelem 能够代表一个汉字，这个概念在编辑 entered
的时候，非常有用。

另外要注意的是，不同的输入法， imelem 的内部结构是不一样的，比如：
1. quanping:  (\"g\" \"ua\" \"g\" \"ua\")
2. shuangpin: (\"h\" \"ao\" \"h\" \"c\")
3. wubi:      (\"aaaa\")")

(defvar pyim-preview-overlay nil
  "用于保存光标处预览字符串的 overlay.")

(defvar pyim-outcome-history nil
  "记录 pyim outcome 的变化的历史

在 pyim 中 outcome 代表用户通过输入法选择，并最终插入到 buffer
的字符串。

“一次确认就生成的词条” , 当前变量一般只有一个元素，比如：
1. 输入： nihao
2. 输出： 你好
2. 变量取值为： (\"你好\")

“多次确认才能生成词条” , 当前变量记录了选择的历史，比如：

1. 输入： yiersansi
2. 输出： 一二三四
3. 第一次选择：一二
4. 第二次选择：三
5. 第三次选择：四
6. 变量取值为： (\"一二三四\" \"一二三\" \"一二\")")

(defvar pyim-input-ascii nil
  "是否开启 pyim 英文输入模式.")

(defvar pyim-force-input-chinese nil
  "是否强制开启中文输入模式.

这个变量只用于 `pyim-convert-string-at-point', 不要
在其它地方使用。")

(defvar pyim-last-created-word nil
  "记录最近一次创建的词条， 用于实现快捷删词功能： `pyim-delete-last-word' .")

(defvar pyim-translating nil
  "记录是否在转换状态.")

(defvar pyim-magic-convert-cache nil
  "用来临时保存 `pyim-magic-convert' 的结果.
从而加快同一个字符串第二次的转换速度。")

(defvar pyim-load-hook nil)
(defvar pyim-active-hook nil)
(defvar pyim-inactive-hook nil)

(defvar pyim-mode-map
  (let ((map (make-sparse-keymap))
        (i ?\ ))
    (while (< i 127)
      (define-key map (char-to-string i) #'pyim-self-insert-command)
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 256)
      (define-key map (vector i) #'pyim-self-insert-command)
      (setq i (1+ i)))
    (dolist (i (number-sequence ?0 ?9))
      (define-key map (char-to-string i) #'pyim-page-select-word-by-number))
    (define-key map " " #'pyim-page-select-word)
    (define-key map (kbd "C-SPC") #'pyim-page-select-word-simple)
    (define-key map [backspace] #'pyim-entered-delete-backward-char)
    (define-key map [delete] #'pyim-entered-delete-forward-char)
    (define-key map "\C-d" #'pyim-entered-delete-forward-char)
    (define-key map [M-backspace] #'pyim-entered-delete-backward-imelem)
    (define-key map [M-delete] #'pyim-entered-delete-forward-imelem)
    (define-key map [C-backspace] #'pyim-entered-delete-backward-imelem)
    (define-key map [C-delete] #'pyim-entered-delete-forward-imelem)
    (define-key map [?\t]      #'pyim-toggle-assistant-scheme)
    (define-key map (kbd "TAB") #'pyim-toggle-assistant-scheme)
    (define-key map "\177" #'pyim-entered-delete-backward-char)
    (define-key map "\C-f" #'pyim-entered-forward-point)
    (define-key map "\C-b" #'pyim-entered-backward-point)
    (define-key map "\M-f" #'pyim-entered-forward-imelem)
    (define-key map "\M-b" #'pyim-entered-backward-imelem)
    (define-key map "\C-e" #'pyim-entered-end-of-line)
    (define-key map "\C-a" #'pyim-entered-beginning-of-line)
    (define-key map "=" #'pyim-page-next-page)
    (define-key map "-" #'pyim-page-previous-page)
    (define-key map "\C-n" #'pyim-page-next-word)
    (define-key map "\C-p" #'pyim-page-previous-word)
    (define-key map "\M-n" #'pyim-page-next-page)
    (define-key map "\M-p" #'pyim-page-previous-page)
    (define-key map "\C-m" #'pyim-quit-no-clear)
    (define-key map [return] #'pyim-quit-no-clear)
    (define-key map "\C-c" #'pyim-quit-clear)
    map)
  "Pyim 的 Keymap.")

;; ** 将变量转换为 local 变量
(defvar pyim-local-variable-list
  '(pyim-imobjs
    pyim-outcome-history
    pyim-preview-overlay
    pyim-candidates
    pyim-candidate-position
    pyim-input-ascii
    ;; pyim-english-input-switch-functions
    pyim-punctuation-half-width-functions
    pyim-translating
    pyim-last-created-word

    input-method-function
    inactivate-current-input-method-function
    describe-current-input-method-function

    pyim-punctuation-translate-p
    pyim-punctuation-pair-status
    pyim-punctuation-escape-list)
  "A list of buffer local variable.")

(defvar pyim--exhibit-timer nil)

(dolist (var pyim-local-variable-list)
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

(defmacro pyim-with-entered-buffer (&rest forms)
  (declare (indent 0) (debug t))
  `(with-current-buffer (get-buffer-create pyim-entered-buffer)
     ,@forms))

(defun pyim-entered-get (&optional type)
  "从 `pyim-entered-buffer' 中获取拼音字符串.

默认返回 entered buffer 中的全部字符串。如果 TYPE 取值为
point-before, 返回 entered buffer 中 point 之前的字符串，如果
TYPE 取值为 point-after, 返回 entered buffer 中 point 之后的字符
串。"
  (pyim-with-entered-buffer
    (cond
     ((equal 1 (point))
      (buffer-string))
     ((eq type 'point-before)
      (buffer-substring-no-properties 1 (point)))
     ((eq type 'point-after)
      (buffer-substring-no-properties (point) (point-max)))
     (t (buffer-string)))))

(defun pyim-entered-erase-buffer ()
  "清除 `pyim-entered-buffer' 的内容"
  (pyim-with-entered-buffer
    (erase-buffer)))

;; pyim-entered-buffer 中进行光标移动的函数
;; point move function in `pyim-entered-buffer'
(defun pyim-entered-forward-point ()
  "`pyim-entered-buffer' 中光标前移"
  (interactive)
  (pyim-with-entered-buffer
    (ignore-errors
      (forward-char)))
  (pyim-entered-refresh t))

(defun pyim-entered-backward-point ()
  "`pyim-entered-buffer' 中光标后移"
  (interactive)
  (pyim-with-entered-buffer
    (ignore-errors
      (backward-char)))
  (pyim-entered-refresh t))

(defun pyim-entered-backward-imelem (&optional search-forward)
  "`pyim-entered-buffer’ 中光标向后移动一个 imelem 对应的字符串

在全拼输入法中，就是向前移动一个拼音"
  (interactive)
  (let* ((position (pyim-entered-next-imelem-position 1 search-forward)))
    (pyim-with-entered-buffer
      (goto-char position))
    (pyim-entered-refresh t)))

(defun pyim-entered-forward-imelem ()
  "`pyim-entered-buffer’ 中光标向前移动一个 imelem 对应的字符串"
  (interactive)
  (pyim-entered-backward-imelem t))

(defun pyim-entered-end-of-line ()
  "`pyim-entered-buffer' 中光标移至行尾"
  (interactive)
  (pyim-with-entered-buffer
    (end-of-line))
  (pyim-entered-refresh t))

(defun pyim-entered-beginning-of-line ()
  "`pyim-entered-buffer' 中光标移至行首"
  (interactive)
  (pyim-with-entered-buffer
    (beginning-of-line))
  (pyim-entered-refresh t))

(defun pyim-entered-next-imelem-position (num &optional search-forward start)
  "从 `pyim-entered-buffer' 的当前位置，找到下一个或者下 NUM 个 imelem 对应的位置

如果 SEARCH-FORWARD 为 t, 则向前搜索，反之，向后搜索。"
  (pyim-with-entered-buffer
    (let* ((scheme-name (pyim-scheme-name))
           (start (or start (point)))
           (end-position start)
           (string (buffer-substring-no-properties (point-min) start))
           (orig-imobj-len (length (car (pyim-imobjs-create string scheme-name))))
           imobj pos)
      (if search-forward
          ;; "ni|haoshijie" -> "nihao|shijie"
          (progn
            (setq pos (point-max))
            (while (and (> pos start) (= end-position start))
              (setq string (buffer-substring-no-properties (point-min) pos)
                    imobj (car (pyim-imobjs-create string scheme-name)))
              (if (>= (+ orig-imobj-len num) (length imobj))
                  (setq end-position pos)
                (cl-decf pos))))
        ;; "nihao|shijie" -> "ni|haoshijie"
        (if (<= orig-imobj-len num)
            (setq end-position (point-min))
          (setq pos start)
          (while (and (>= pos (point-min)) (= end-position start))
            (setq string (buffer-substring-no-properties (point-min) pos)
                  imobj (car (pyim-imobjs-create string scheme-name)))
            (if (= (- orig-imobj-len num) (length imobj))
                (setq end-position pos)
              (cl-decf pos)))))
      end-position)))

;; ** 注册 Pyim 输入法
;;;###autoload
(register-input-method "pyim" "euc-cn" 'pyim-start (nth 0 pyim-titles))

;;;###autoload
(defun pyim-start (_name &optional _active-func restart save-personal-dcache refresh-common-dcache)
  "pyim 启动函数.
  TODO: Document NAME ACTIVE-FUNC RESTART SAVE-PERSONAL-DCACHE REFRESH-COMMON-DCACHE

pyim 是使用 `pyim-start' 来启动输入法，这个命令主要做如下工作：
1. 重置 `pyim-local-variable-list' 中所有的 local 变量。
2. 使用 `pyim-cchar2pinyin-create-cache' 创建汉字到拼音的 hash table 对应表。
3. 运行hook： `pyim-load-hook'。
4. 将 `pyim-dcache-save-caches' 命令添加到 `kill-emacs-hook' , emacs 关闭
之前将用户选择过的词生成的缓存和词频缓存保存到文件，供以后使用。
5. 设定变量：
1. `input-method-function'
2. `deactivate-current-input-method-function'
6. 运行 `pyim-active-hook'

pyim 使用函数 `pyim-start' 启动输入法的时候，会将变量
`input-method-function' 设置为 `pyim-input-method' ，这个变量会影
响 `read-event' 的行为。

当输入字符时，`read-event' 会被调用，`read-event' 调用的过程中，
会执行 `pyim-input-method' 这个函数。`pyim-input-method' 又调用函
数`pyim-start-translation'."
  (interactive)
  (mapc #'kill-local-variable pyim-local-variable-list)
  (mapc #'make-local-variable pyim-local-variable-list)
  (when (and restart save-personal-dcache)
    (pyim-dcache-save-caches))

  (pyim-dcache-init-variables)

  (when pyim-dcache-auto-update
    (pyim-dcache-call-api 'update-personal-words restart))

  (pyim-cchar2pinyin-cache-create)
  (pyim-pinyin2cchar-cache-create)
  (run-hooks 'pyim-load-hook)

  (when pyim-dcache-auto-update
    (pyim-dcache-update-code2word refresh-common-dcache)
    ;; 这个命令 *当前* 主要用于五笔输入法。
    (pyim-dcache-call-api 'update-shortcode2word restart))

  ;; Make sure personal or other dcache are saved to file before kill emacs.
  (add-hook 'kill-emacs-hook #'pyim-dcache-save-caches)
  (setq input-method-function #'pyim-input-method)
  (setq deactivate-current-input-method-function #'pyim-inactivate)
  ;; (setq describe-current-input-method-function 'pyim-help)
  ;; If we are in minibuffer, turn off the current input method
  ;; before exiting.
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook #'pyim-exit-from-minibuffer))
  (run-hooks 'pyim-active-hook)
  (when (and (eq pyim-page-tooltip 'posframe)
             (not (pyim-posframe-valid-p)))
    (message "PYIM: posframe 没有正确安装或者当前 Emacs 版本不支持 posframe。"))
  (when restart
    (message "pyim 重启完成。"))
  nil)

(declare-function quail-exit-from-minibuffer "quail" ())

(defun pyim-exit-from-minibuffer ()
  "Pyim 从 minibuffer 退出."
  ;; FIXME: `quail-exit-from-minibuffer' removes itself from
  ;; `minibuffer-exit-hook' but it won't remove `pyim-exit-from-minibuffer' from
  ;; that hook.  Is that indeed what we want here?
  (quail-exit-from-minibuffer))

(defun pyim-restart ()
  "重启 pyim，不建议用于编程环境.

这个函数用于重启 pyim，其过程和 `pyim-start' 类似，只是在输入法重
启之前，询问用户，是否保存个人词频信息。"
  (interactive
   (let ((save-personal-dcache
          (yes-or-no-p "重启 pyim 前，需要保存个人词频信息吗？ "))
         (refresh-common-dcache
          (yes-or-no-p "需要强制刷新词库缓存吗？ ")))
     (pyim-restart-1 save-personal-dcache refresh-common-dcache))))

(defun pyim-restart-1 (&optional save-personal-dcache refresh-common-dcache)
  "重启 pyim，用于编程环境.

当 SAVE-PERSONAL-DCACHE 是 non-nil 时，保存个人词库文件。
当 REFRESH-COMMON-DCACHE 是 non-nil 时，强制刷新词库缓存。"
  (pyim-start "pyim" nil t
              save-personal-dcache refresh-common-dcache))

(defun pyim-export (file &optional confirm)
  "将个人词条以及词条对应的词频信息导出到文件 FILE.

  如果 FILE 为 nil, 提示用户指定导出文件位置, 如果 CONFIRM 为 non-nil，
  文件存在时将会提示用户是否覆盖，默认为覆盖模式"
  (interactive "F将词条相关信息导出到文件: ")
  (with-temp-buffer
    (insert ";;; -*- coding: utf-8-unix -*-\n")
    (pyim-dcache-call-api 'insert-export-content)
    (pyim-dcache-write-file file confirm)))

(defun pyim-export-personal-words (file &optional confirm)
  "将用户选择过的词生成的缓存导出为 pyim 词库文件.

如果 FILE 为 nil, 提示用户指定导出文件位置, 如果 CONFIRM 为 non-nil，
文件存在时将会提示用户是否覆盖，默认为覆盖模式。

注： 这个函数的用途是制作 pyim 词库，个人词条导入导出建议使用：
`pyim-import' 和 `pyim-export' ."
  (interactive "F将个人缓存中的词条导出到文件：")
  (pyim-dcache-call-api 'export-personal-words file confirm)
  (message "Pyim export finished."))

(defun pyim-import (file &optional merge-method)
  "从 FILE 中导入词条以及词条对应的词频信息。

MERGE-METHOD 是一个函数，这个函数需要两个数字参数，代表
词条在词频缓存中的词频和待导入文件中的词频，函数返回值做为合并后的词频使用，
默认方式是：取两个词频的最大值。"
  (interactive "F导入词条相关信息文件: ")
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (goto-char (point-min))
    (forward-line 1)
    (while (not (eobp))
      (let* ((content (pyim-dline-parse))
             (word (car content))
             (count (string-to-number
                     (or (car (cdr content)) "0"))))
        (pyim-create-word
         word nil
         (lambda (x)
           (funcall (or merge-method #'max)
                    (or x 0)
                    count))))
      (forward-line 1)))
  ;; 保存一下用户选择过的词生成的缓存和词频缓存，
  ;; 因为使用 async 机制更新 dcache 时，需要从 dcache 文件
  ;; 中读取变量值, 然后再对用户选择过的词生成的缓存排序，如果没
  ;; 有这一步骤，导入的词条就会被覆盖，使用 emacs-thread 机制来更新 dcache
  ;; 不存在此问题。
  (unless pyim-dcache-prefer-emacs-thread
    (pyim-dcache-save-caches))
  ;; 更新相关的 dcache
  (pyim-dcache-call-api 'update-personal-words t)

  (message "pyim: 词条相关信息导入完成！"))

(defun pyim-insert-word-into-icode2word (word pinyin prepend)
  (pyim-dcache-call-api 'insert-word-into-icode2word word pinyin prepend))

(defun pyim-create-word (word &optional prepend wordcount-handler)
  (pyim-create-pyim-word word prepend wordcount-handler))

(defun pyim-create-pyim-word (word &optional prepend wordcount-handler)
  "将中文词条 WORD 添加编码后，保存到用户选择过的词生成的缓存中。

词条 WORD 默认会追加到已有词条的后面，如果 PREPEND 设置为 t,
词条就会放到已有词条的最前面。

根据当前输入法，决定是调用 `pyim-cstring-to-pinyin' 还是
`pyim-hanzi2xingma' 来获取中文词条的编码。

WORDCOUNT-HANDLER 可以是一个数字，代表将此数字设置为 WORD 的新词频，
WORDCOUNT-HANDLER 也可以是一个函数，其返回值将设置为 WORD 的新词频，
而这个函数的参数则表示 WORD 当前词频，这个功能用于：`pyim-import',
如果 WORDCOUNT-HANDLER 设置为其他, 则表示让 WORD 当前词频加1.

BUG：拼音无法有效地处理多音字。"
  (when (and (> (length word) 0)
             (< (length word) 11) ;十个汉字以上的词条，加到个人词库里面用处不大，忽略。
             (not (pyim-string-match-p "\\CC" word)))
    ;; 记录最近创建的词条，用于快速删词功能。
    (setq pyim-last-created-word word)
    (let* ((scheme-name (pyim-scheme-name))
           (class (pyim-scheme-get-option scheme-name :class))
           (code-prefix (pyim-scheme-get-option scheme-name :code-prefix))
           (codes (cond ((eq class 'xingma)
                         (pyim-hanzi2xingma word scheme-name t))
                        ;;拼音使用了多音字校正
                        (t (pyim-cstring-to-pinyin word nil "-" t nil t)))))
      ;; 保存对应词条的词频
      (when (> (length word) 0)
        (pyim-dcache-call-api
         'update-iword2count
         word
         prepend
         wordcount-handler))
      ;; 添加词条到个人缓存
      (dolist (code codes)
        (unless (pyim-string-match-p "[^ a-z-]" code)
          (pyim-insert-word-into-icode2word word
                                            (concat (or code-prefix "") code)
                                            prepend)))
      ;; TODO, 排序个人词库?
      )))

(defun pyim-hanzi2xingma (string scheme-name &optional return-list)
  "返回汉字 STRING 对应形码方案 SCHEME-NAME 的 code (不包括
code-prefix)。当RETURN-LIST 设置为 t 时，返回一个 code list。"
  (let* ((fun (intern (concat "pyim-hanzi2xingma:" (symbol-name scheme-name))))
         (code (and fun (funcall fun string))))
    (when code
      (if return-list
          (list code)
        code))))

(defun pyim-hanzi2xingma:wubi (string)
  "返回汉字 STRING 的五笔编码(不包括 code-prefix)。当RETURN-LIST
设置为 t 时，返回一个编码列表。"
  (when (string-match-p "^\\cc+\\'" string)
    (let ((code (pyim-code-search string 'wubi))
          (len (length string)))
      (when (string-empty-p code)
        (when (= len 1)
          (error "No code found for %s" string))
        (setq string (split-string string "" t)
              code
              (cl-case len
                ;; 双字词，分别取两个字的前两个编码
                (2 (concat (substring (pyim-hanzi2xingma:wubi (nth 0 string)) 0 2)
                           (substring (pyim-hanzi2xingma:wubi (nth 1 string)) 0 2)))
                ;; 三字词，取前二字的首编码，及第三个字的前两个编码
                (3 (concat (substring (pyim-hanzi2xingma:wubi (nth 0 string)) 0 1)
                           (substring (pyim-hanzi2xingma:wubi (nth 1 string)) 0 1)
                           (substring (pyim-hanzi2xingma:wubi (nth 2 string)) 0 2)))
                ;; 四字词及以上，分别前三个字及最后一个字的首编码
                (t (concat (substring (pyim-hanzi2xingma:wubi (nth 0 string)) 0 1)
                           (substring (pyim-hanzi2xingma:wubi (nth 1 string)) 0 1)
                           (substring (pyim-hanzi2xingma:wubi (nth 2 string)) 0 1)
                           (substring (pyim-hanzi2xingma:wubi (nth (1- len) string)) 0 1))))))
      code)))

(defun pyim-create-word-at-point (&optional number silent)
  "将光标前字符数为 NUMBER 的中文字符串添加到个人词库中
当 SILENT 设置为 t 是，不显示提醒信息。"
  (let* ((string (pyim-cstring-at-point (or number 2))))
    (when string
      (pyim-create-word string)
      (unless silent
        (message "将词条: \"%s\" 加入 personal 缓冲。" string)))))

(defun pyim-create-2cchar-word-at-point ()
  "将光标前2个中文字符组成的字符串加入个人词库。"
  (interactive)
  (pyim-create-word-at-point 2))

(defun pyim-create-3cchar-word-at-point ()
  "将光标前3个中文字符组成的字符串加入个人词库。"
  (interactive)
  (pyim-create-word-at-point 3))

(defun pyim-create-4cchar-word-at-point ()
  "将光标前4个中文字符组成的字符串加入个人词库。"
  (interactive)
  (pyim-create-word-at-point 4))

;; ** 删词功能
(defun pyim-create-word-from-selection ()
  "Add the selected text as a Chinese word into the personal dictionary."
  (interactive)
  (when (region-active-p)
    (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
      (if (> (length string) 6)
          (error "词条太长")
        (if (not (string-match-p "^\\cc+\\'" string))
            (error "不是纯中文字符串")
          (pyim-create-word string)
          (message "将词条: %S 插入 personal file。" string))))))

(defun pyim-search-word-code ()
  "选择词条，然后反查它的 code. 这个功能对五笔用户有用。"
  (interactive)
  (when (region-active-p)
    (let* ((string (buffer-substring-no-properties (region-beginning) (region-end)))
           code)
      (if (not (string-match-p "^\\cc+\\'" string))
          (error "不是纯中文字符串")
        (setq code (pyim-dcache-call-api 'search-word-code string))
        (if code
            (message "%S -> %S " string code)
          (message "没有找到 %S 对应的编码。" string))))))

(defun pyim-delete-words-in-file (file)
  "从个人词库缓存中批量删除 FILE 文件中列出的词条.

FILE 的格式与 `pyim-export' 生成的文件格式相同，
另外这个命令也可以识别没有词频的行，比如：

   ;;; -*- coding: utf-8-unix -*-
   词条1
   词条2"
  (interactive "F记录待删词条的文件: ")
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (goto-char (point-min))
    (forward-line 1)
    (while (not (eobp))
      (let ((word (car (pyim-dline-parse))))
        (when (and word (not (pyim-string-match-p "\\CC" word)))
          (pyim-delete-word-1 word)))
      (forward-line 1)))
  (message "pyim: 批量删词完成！"))

(defun pyim-delete-last-word ()
  "从个人词库中删除最新创建的词条。"
  (interactive)
  (when pyim-last-created-word
    (pyim-delete-word-1 pyim-last-created-word)
    (message "pyim: 从个人词库中删除词条 “%s” !" pyim-last-created-word)))

(defun pyim-delete-word-at-point (&optional number silent)
  "将光标前字符数为 NUMBER 的中文字符串从个人词库中删除
当 SILENT 设置为 t 是，不显示提醒信息。"
  (let* ((string (pyim-cstring-at-point (or number 2))))
    (when string
      (pyim-delete-word-1 string)
      (unless silent
        (message "词条: \"%s\" 已经从个人词库缓冲中删除。" string)))))

(defun pyim-delete-word ()
  "将高亮选择的词条从个人词库中删除。"
  (interactive)
  (if mark-active
      (let ((string (buffer-substring-no-properties
                     (region-beginning) (region-end))))
        (when (and (< (length string) 6)
                   (> (length string) 0))
          (pyim-delete-word-1 string)
          (message "将词条: %S 从 personal 缓冲中删除。" string)))
    (message "请首先高亮选择需要删除的词条。")))

(defun pyim-delete-word-1 (word)
  "将中文词条 WORD 从个人词库中删除"
  (pyim-dcache-call-api 'delete-word word))

;; ** 处理用户输入字符的相关函数
(defun pyim-input-method (key)
  "得到需要插入到 buffer 的字符串, 并将其插入到待输入 buffer.

这个函数会处理用户输入的字符，并最终的得到需要插入 buffer 的字符
串。这个字符串会被分解为 event list, 通过 emacs 低层函数
`read-event' 来将这些 list 插入到 *待输入buffer*。"
  (if (or buffer-read-only
          overriding-terminal-local-map
          overriding-local-map)
      (list key)
    ;; (message "call with key: %S" key-or-string)
    (pyim-preview-setup-overlay)
    (with-silent-modifications
      (unwind-protect
          (let ((input-string (pyim-start-translation key)))
            ;; (message "input-string: %s" input-string)
            (when (and (stringp input-string)
                       (> (length input-string) 0))
              (if input-method-exit-on-first-char
                  (list (aref input-string 0))
                (mapcar #'identity input-string))))
        (pyim-preview-delete-overlay)
        (pyim-entered-erase-buffer)))))

(defun pyim-magic-convert (str)
  "用于处理 `pyim-magic-convert' 的函数。"
  (if (functionp pyim-magic-converter)
      (or (cdr (assoc str pyim-magic-convert-cache))
          (let ((result (funcall pyim-magic-converter str)))
            (setq pyim-magic-convert-cache
                  `((,str . ,result)))
            result))
    str))

(defun pyim-start-translation (key)
  "Start translation of the typed character KEY-OR-STRING by pyim.
Return the input string.

`pyim-start-translation' 这个函数较复杂，作许多低层工作，但它的一
个重要流程是：

1. 使用函数 `read-key-sequence' 得到 key-sequence
2. 使用函数 `lookup-key' 查询 `pyim-mode-map' 中，与上述 key-sequence 对应
   的命令。
3. 如果查询得到的命令是 `pyim-self-insert-command' 时，
   `pyim-start-translation' 会调用这个函数。
4. 这个函数最终会返回需要插入到 buffer 的字符串。

这个部份的代码涉及许多 emacs 低层函数，相对复杂，不容易理解，有兴
趣的朋友可以参考：
1. `quail-input-method' 相关函数。
2. elisp 手册相关章节:
   1. Invoking the Input Method
   2. Input Methods
   3. Miscellaneous Event Input Features
   4. Reading One Event"
  ;; Check the possibility of translating KEY.
  ;; If KEY is nil, we can anyway start translation.
  (if (or (integerp key) (null key))
      ;; OK, we can start translation.
      (let* ((echo-keystrokes 0)
             (help-char nil)
             (overriding-terminal-local-map pyim-mode-map)
             ;; (generated-events nil)
             (input-method-function nil)
             ;; Quail package 用这个变量来控制是否在 buffer 中
             ;; 插入 preview string, pyim *强制* 将其设置为 nil
             (input-method-use-echo-area nil)
             (modified-p (buffer-modified-p))
             last-command-event last-command this-command)

        (setq pyim-translating t)
        (pyim-entered-erase-buffer)
        (pyim-outcome-handle "")

        (when key
          (setq unread-command-events
                (cons key unread-command-events)))

        (while pyim-translating
          (set-buffer-modified-p modified-p)
          (let* ((keyseq (read-key-sequence nil nil nil t))
                 (cmd (lookup-key pyim-mode-map keyseq)))
            ;; (message "key: %s, cmd:%s\nlcmd: %s, lcmdv: %s, tcmd: %s"
            ;;          key cmd last-command last-command-event this-command)
            (if (if key
                    (commandp cmd)
                  (eq cmd 'pyim-self-insert-command))
                (progn
                  ;; (message "keyseq: %s" keyseq)
                  (setq last-command-event (aref keyseq (1- (length keyseq)))
                        last-command this-command
                        this-command cmd)
                  (setq key t)
                  (condition-case-unless-debug err
                      (call-interactively cmd)
                    (error (message "pyim 出现错误: %S , 开启 debug-on-error 后可以了解详细情况。" err)
                           (beep))))
              ;; KEYSEQ is not defined in the translation keymap.
              ;; Let's return the event(s) to the caller.
              (setq unread-command-events
                    (string-to-list (this-single-command-raw-keys)))
              ;; (message "unread-command-events: %s" unread-command-events)
              (pyim-terminate-translation))))
        ;; (message "return: %s" (pyim-outcome-get))
        (pyim-magic-convert (pyim-outcome-get)))
    ;; Since KEY doesn't start any translation, just return it.
    ;; But translate KEY if necessary.
    (char-to-string key)))

(defun pyim-auto-switch-english-input-p ()
  "判断是否 *根据环境自动切换* 为英文输入模式，这个函数处理变量：
`pyim-english-input-switch-functions'"
  (let* ((func-or-list pyim-english-input-switch-functions))
    (and (cl-some #'(lambda (x)
                      (if (functionp x)
                          (funcall x)
                        nil))
                  (cond ((functionp func-or-list) (list func-or-list))
                        ((listp func-or-list) func-or-list)
                        (t nil)))
         (setq current-input-method-title
               (if pyim-input-ascii
                   (nth 1 pyim-titles)
                 (nth 2 pyim-titles))))))

(defun pyim-input-chinese-p ()
  "确定 pyim 是否需要启动中文输入模式."
  (let* ((scheme-name (pyim-scheme-name))
         (first-chars (pyim-scheme-get-option scheme-name :first-chars))
         (rest-chars (pyim-scheme-get-option scheme-name :rest-chars)))
    (and (or pyim-force-input-chinese
             (and (not pyim-input-ascii)
                  (not (pyim-auto-switch-english-input-p))))
         (if (not (string< "" (pyim-entered-get 'point-before)))
             (member last-command-event
                     (mapcar #'identity first-chars))
           (member last-command-event
                   (mapcar #'identity rest-chars)))
         (setq current-input-method-title (nth 0 pyim-titles)))))

(defun pyim-autoselector-xingma (&rest _args)
  "适用于型码输入法的自动上屏器.

比如：五笔等型码输入法，重码率很低，90%以上的情况都是选择第一个词
条，自动选择可以减少按空格强制选词的机会。"
  (let* ((scheme-name (pyim-scheme-name))
         (class (pyim-scheme-get-option scheme-name :class))
         (n (pyim-scheme-get-option scheme-name :code-split-length)))
    (when (eq class 'xingma)
      (cond
       ((and (= (length (pyim-entered-get 'point-before)) n)
             (= (length pyim-candidates) 1))
        '(:select current))
       ((> (length (pyim-entered-get 'point-before)) n)
        '(:select last))
       (t nil)))))

(defun pyim-self-insert-command ()
  "Pyim 版本的 self-insert-command."
  (interactive "*")
  (setq pyim-candidates-last pyim-candidates)
  (cond
   ((pyim-input-chinese-p)
    (pyim-with-entered-buffer
      ;; 一定要注意，point 可能不在 point-min, 或者 point-max. 因为用
      ;; 户可能通过命令移动了 entered 中的 point。
      (insert (char-to-string last-command-event)))
    (pyim-entered-refresh))
   (pyim-candidates
    (pyim-outcome-handle 'candidate-and-last-char)
    (pyim-terminate-translation))
   (t
    (pyim-outcome-handle 'last-char)
    (pyim-terminate-translation))))

(defun pyim-entered-refresh-1 ()
  "查询 `pyim-entered-buffer' 光标前的拼音字符串（如果光标在行首则为光标后的）, 显示备选词等待用户选择。"
  (let* ((scheme-name (pyim-scheme-name))
         entered-to-translate)
    (setq entered-to-translate
          (pyim-entered-get 'point-before))
    (setq pyim-imobjs (pyim-imobjs-create entered-to-translate scheme-name))
    (setq pyim-candidates
          (or (delete-dups (pyim-candidates-create pyim-imobjs scheme-name))
              (list entered-to-translate)))
    (when pyim-candidates-create-timer
      (cancel-timer pyim-candidates-create-timer))
    ;; 延迟1秒异步获取 candidates, pyim 内置的输入法目前不使用异步获取
    ;; 词条的方式，主要用于 pyim-liberime 支持。
    (setq pyim-candidates-create-timer
          (run-with-timer
           1 nil
           (lambda ()
             (if (functionp 'make-thread)
                 (make-thread #'pyim-candidates-create-timer-function
                              "pyim-candidates-create")
               (pyim-candidates-create-timer-function)))))
    ;; 自动上屏功能
    (let ((autoselector-results
           (mapcar #'(lambda (x)
                       (when (functionp x)
                         (ignore-errors
                           (funcall x))))
                   (cl-remove-duplicates pyim-autoselector :from-end t)))
          result)
      (cond
       ;; 假如用户输入 "nihao", 然后按了 "m" 键, 那么当前的 entered
       ;; 就是"nihaom", 如果 autoselector 返回 list: (:select last),
       ;; 那么，“nihao” 对应的第一个候选词将上屏，m键下一轮继续处理。
       ;; 这是一种 "踩雷确认模式".
       ((and
         ;; autoselector 功能会影响手动连续选择功能，所以这里做了一些限制，
         ;; 只有在输入的时候才能够触发 autoselector 机制。
         (eq this-command 'pyim-self-insert-command)
         (cl-find-if (lambda (x)
                       (setq result x)
                       (equal (plist-get x :select) 'last))
                     autoselector-results))
        (let* ((str (plist-get result :replace-with))
               (pyim-candidates
                (if (and str (stringp str))
                    (list str)
                  pyim-candidates-last)))
          (pyim-outcome-handle 'candidate))
        ;; autoselector 机制已经触发的时候，如果发现 entered buffer 中
        ;; point 后面还有未处理的输入，就将其转到下一轮处理，这种情况
        ;; 很少出现，一般是型码输入法，entered 编辑的时候有可能触发。
        (setq unread-command-events
              (append (listify-key-sequence (pyim-entered-get 'point-after))
                      unread-command-events))
        (push last-command-event unread-command-events)
        (pyim-terminate-translation))
       ;; 假设用户已经输入 "niha", 然后按了 "o" 键，那么，当前
       ;; entered 就是 "nihao". 如果 autoselector 函数返回一个 list:
       ;; (:select current), 那么就直接将 "nihao" 对应的第一个候选词
       ;; 上屏幕。
       ((and (eq this-command 'pyim-self-insert-command)
             (cl-find-if (lambda (x)
                           (setq result x)
                           (equal (plist-get x :select) 'current))
                         autoselector-results))
        (let* ((str (plist-get result :replace-with))
               (pyim-candidates
                (if (and str (stringp str))
                    (list str)
                  pyim-candidates)))
          (pyim-outcome-handle 'candidate))
        (setq unread-command-events
              (append (listify-key-sequence (pyim-entered-get 'point-after))
                      unread-command-events))
        (pyim-terminate-translation))
       (t (setq pyim-candidate-position 1)
          (pyim-preview-refresh)
          (pyim-page-refresh))))))

(defun pyim-entered-refresh (&optional no-delay)
  "延迟 `pyim-exhibit-delay-ms' 显示备选词等待用户选择。"
  (if (= (length (pyim-entered-get 'point-before)) 0)
      (pyim-terminate-translation)
    (when pyim--exhibit-timer
      (cancel-timer pyim--exhibit-timer))
    (cond
     ((or no-delay
          (not pyim-exhibit-delay-ms)
          (eq pyim-exhibit-delay-ms 0))
      (pyim-entered-refresh-1))
     (t (setq pyim--exhibit-timer
              (run-with-timer (/ pyim-exhibit-delay-ms 1000.0)
                              nil
                              #'pyim-entered-refresh-1))))))

(defun pyim-terminate-translation ()
  "Terminate the translation of the current key."
  (setq pyim-translating nil)
  (pyim-preview-delete-string)
  (setq pyim-candidates nil)
  (setq pyim-candidates-last nil)
  (when pyim-candidates-create-timer
    (cancel-timer pyim-candidates-create-timer)
    (setq pyim-candidates-create-timer nil))
  (setq pyim-assistant-scheme-enable nil)
  (setq pyim-force-input-chinese nil)
  (when (and (eq pyim-page-tooltip 'posframe)
             (pyim-posframe-valid-p))
    (posframe-hide pyim-page-tooltip-posframe-buffer))
  (pyim-entered-erase-buffer)
  (let* ((class (pyim-scheme-get-option (pyim-scheme-name) :class))
         (func (intern (format "pyim-terminate-translation:%S" class))))
    (when (and class (functionp func))
      (funcall func))))

(defun pyim-toggle-assistant-scheme ()
  "临时切换到辅助输入法.

这个功能一般用于五笔等形码输入法，在忘记编码的时候临时用拼音输入
中文。"
  (interactive)
  (if (= (length (pyim-entered-get 'point-before)) 0)
      (progn
        (pyim-outcome-handle 'last-char)
        (pyim-terminate-translation))
    (setq pyim-assistant-scheme-enable
          (not pyim-assistant-scheme-enable))
    (pyim-entered-refresh)))

(defun pyim-imobjs-create (entered &optional scheme-name)
  "按照 SCHEME-NAME 对应的输入法方案，从 ENTERED 字符串中创建一个
或者多个 imobj 组成的列表，不同的输入法，imobj 的结构也是不一样的。"
  (let ((class (pyim-scheme-get-option scheme-name :class)))
    (when class
      (funcall (intern (format "pyim-imobjs-create:%S" class))
               entered scheme-name))))

(defun pyim-imobjs-create:quanpin (entered &optional _)
  "从用户输入的字符串 ENTERED 创建一个输入法内部对象列表: imobjs.

这个 imobjs 可能包含一个 imobj, 也可能包含多个，每个 imobj 都包含
声母和韵母的相关信息，比如：

    (pyim-imobjs-create:quanpin \"woaimeinv\" 'quanpin)

结果为:

    (((\"w\" \"o\" \"w\" \"o\") (\"\" \"ai\" \"\" \"ai\") (\"m\" \"ei\" \"m\" \"ei\") (\"n\" \"v\" \"n\" \"v\")))

如果字符串无法正确处理，则返回 nil, 比如：

   (pyim-imobjs-create \"ua\" 'quanpin)

全拼输入法的 imelem 是四个字符串组成的 list, 类似：

  (\"w\" \"o\" \"w\" \"o\")

代表：

  (声母1, 韵母1, 声母2, 韵母2)

声母1和声母2一般用来生成 code 字符串，用于词库中寻找词条。声母2和
韵母2一般用来反向构建 entered 字符串，用于“多次选择生成词条”这个
功能。

大多数情况，声母1 = 声母2, 韵母1 = 韵母2, 只有在使用模糊音的时候，
才可能出现不一致的情况。"
  (when (and entered (string< "" entered))
    (let* ((str-list (remove "" (split-string entered "'")))
           (n (length str-list))
           output)
      (dotimes (i n)
        (let* ((str (nth i str-list))
               (x (pyim-pinyin-split str)))
          (if (not x)
              (setq output nil)
            (when (> i 0)
              ;; 将强制分割符号附加到封分割符后面的声母开头，
              ;; 类似： ("'n" "i" "n" "i"), 用于 `pyim-page-preview-create' 函数。
              (setf (caar x)
                    (concat "'" (caar x))))
            (setq output (append output x)))))
      (when output
        (pyim-imobjs-find-fuzzy:quanpin (list output))))))

;; "nihc" -> (((\"n\" \"i\" \"n\" \"i\") (\"h\" \"ao\" \"h\" \"c\")))
(defun pyim-imobjs-create:shuangpin (entered &optional scheme-name)
  (let ((keymaps (pyim-scheme-get-option scheme-name :keymaps))
        (list (string-to-list (replace-regexp-in-string "-" "" entered)))
        results)
    (while list
      (let* ((sp-sm (pop list))
             (sp-ym (pop list))
             (sp-sm (when sp-sm (char-to-string sp-sm)))
             (sp-ym (when sp-ym (char-to-string sp-ym)))
             (sm (nth 1 (assoc sp-sm keymaps)))
             (ym (or (cdr (cdr (assoc sp-ym keymaps))) (list "")))
             one-word-pinyins)

        (dolist (x ym)
          (let* ((y (concat sp-sm (or sp-ym " ")))
                 (z (cadr (assoc y keymaps)))
                 (py (if z (list "" z sp-sm sp-ym) (list sm x sp-sm sp-ym))))
            (unless (string-match-p pyim-pinyin-shuangpin-invalid-pinyin-regexp
                                    (concat (nth 0 py) (nth 1 py)))
              (push py one-word-pinyins))))

        (when (and one-word-pinyins (> (length one-word-pinyins) 0))
          (push one-word-pinyins results))))
    (pyim-imobjs-find-fuzzy:quanpin
     (pyim-permutate-list (nreverse results)))))

(defun pyim-imobjs-create:xingma (entered &optional scheme-name)
  (let ((n (pyim-scheme-get-option scheme-name :code-split-length)))
    (let (output)
      (mapc (lambda (x)
              (while (not (string-empty-p x))
                (if (< (length x) n)
                    (progn
                      (push x output)
                      (setq x ""))
                  (push (substring x 0 n) output)
                  (setq x (substring x n)))))
            (split-string entered "'"))
      (list (nreverse output)))))

(defun pyim-imobjs-find-fuzzy:quanpin (imobjs)
  "用于处理模糊音的函数。"
  (let (fuzzy-imobjs result1 result2)
    (dolist (imobj imobjs)
      (setq fuzzy-imobjs
            (pyim-permutate-list
             (mapcar #'pyim-imobjs-find-fuzzy:quanpin-1 imobj)))
      (push (car fuzzy-imobjs) result1)
      (setq result2 (append result2 (cdr fuzzy-imobjs))))
    (append result1 result2)))

;; (\"f\" \"en\" \"f\" \"en\") -> ((\"f\" \"en\" \"f\" \"en\") (\"f\" \"eng\" \"f\" \"en\"))
(defun pyim-imobjs-find-fuzzy:quanpin-1 (imelem)
  "Find all fuzzy pinyins."
  (cl-labels ((find-list (str list)
                         (let (result)
                           (dolist (x list)
                             (when (member str x)
                               (setq list nil)
                               (setq result
                                     (delete-dups
                                      `(,str ,@(cl-copy-list x))))))
                           (or result (list str)))))
    (let* ((fuzzy-alist pyim-fuzzy-pinyin-alist)
           (sm-list (find-list (nth 0 imelem) fuzzy-alist))
           (ym-list (find-list (nth 1 imelem) fuzzy-alist))
           result)
      (dolist (a sm-list)
        (dolist (b ym-list)
          (push `(,a ,b ,@(nthcdr 2 imelem)) result)))
      (reverse result))))

(defun pyim-codes-create (imobj scheme-name &optional first-n)
  "按照 SCHEME-NAME 对应的输入法方案，从一个 IMOBJ 创建一个列表 codes, 这个列表
包含一个或者多个 code 字符串，这些 code 字符串用于从词库中搜索词条."
  (let ((class (pyim-scheme-get-option scheme-name :class)))
    (when class
      (funcall (intern (format "pyim-codes-create:%S" class))
               imobj scheme-name first-n))))

(defun pyim-codes-create:quanpin (imobj _scheme-name &optional first-n)
  "从IMOBJ 创建一个 code 列表：codes.

列表 codes 中包含一个或者多个 code 字符串，这些 code 字符串用于从
词库中搜索相关词条。

    (pyim-codes-create '((\"w\" \"o\" \"w\" \"o\") (\"\" \"ai\" \"\" \"ai\") (\"m\" \"ei\" \"m\" \"ei\") (\"n\"  \"v\" \"n\"  \"v\")) 'quanpin)

结果为:

   (\"wo\" \"ai\" \"mei\" \"nv\")"
  (mapcar
   #'(lambda (w)
       (let ((py (replace-regexp-in-string ;去掉分隔符，在词库中搜索候选词不需要分隔符
                  "'" "" (concat (nth 0 w) (nth 1 w)))))
         (if (numberp first-n)
             (substring py 0 (min first-n (length py)))
           py)))
   imobj))

(defun pyim-codes-create:shuangpin (imobj _scheme-name &optional first-n)
  (pyim-codes-create:quanpin imobj 'quanpin first-n))

(defun pyim-codes-create:xingma (imobj scheme-name &optional first-n)
  (when scheme-name
    (let ((code-prefix (pyim-scheme-get-option scheme-name :code-prefix)))
      (mapcar
       #'(lambda (x)
           (concat (or code-prefix "")
                   (if (numberp first-n)
                       (substring x 0 (min first-n (length x)))
                     x)))
       imobj))))

(defun pyim-code-search (word scheme-name)
  "从 SCHEME-NAME 对应的输入法词库中，搜索 WORD 对应的 code.

返回最长的 code."
  (when (and (stringp word)
             (> (length word) 0))
    (let* ((prefix (pyim-scheme-get-option scheme-name :code-prefix))
           (code
            (cl-find-if
             #'(lambda (x)
                 (equal (substring (or x " ") 0 1) prefix))
             (sort
              (cl-copy-list (pyim-dcache-call-api 'search-word-code word))
              #'(lambda (a b) (> (length a) (length b)))))))
      (substring (or code " ") 1))))

;; ** 待输入字符串预览
(defun pyim-preview-setup-overlay ()
  "设置 pyim 光标处实时预览功能所需要的 overlay.

这个函数会在 `pyim-input-method' 中调用，用于创建 overlay ，并将
其保存到 `pyim-preview-overlay' 变量，overlay 的 face 属性设置为
`pyim-preview-face' ，用户可以使用这个变量来自定义 face"
  (let ((pos (point)))
    (if (overlayp pyim-preview-overlay)
        (move-overlay pyim-preview-overlay pos pos)
      (setq pyim-preview-overlay (make-overlay pos pos))
      (if input-method-highlight-flag
          (overlay-put pyim-preview-overlay 'face 'pyim-preview-face)))))

(defun pyim-preview-delete-overlay ()
  "删除 pyim 光标处实时预览功能所需要的 overlay.

这个函数会在 `pyim-input-method' 中调用，用于删除
`pyim-preview-overlay' 中保存的 overlay。"
  (if (and (overlayp pyim-preview-overlay) (overlay-start pyim-preview-overlay))
      (delete-overlay pyim-preview-overlay)))

(defun pyim-preview-refresh ()
  "刷新光标处预览.

pyim 会使用 Emacs overlay 机制在 *待输入buffer* 光标处高亮显示一
个预览字符串，让用户可以查看将要输入的字符串，这个函数用于更新这
个字符串的内容。"
  (let* ((class (pyim-scheme-get-option (pyim-scheme-name) :class))
         (candidates pyim-candidates)
         (pos (1- (min pyim-candidate-position (length candidates))))
         (preview
          (concat (pyim-outcome-get)
                  (pyim-candidate-parse (nth pos candidates)))))
    (when (memq class '(quanpin))
      (let ((rest (mapconcat
                   #'(lambda (py)
                       (concat (nth 0 py) (nth 1 py)))
                   (nthcdr (length preview) (car pyim-imobjs))
                   "'")))
        (when (string< "" rest)
          (setq preview (concat preview rest)))))
    (setq preview (pyim-magic-convert preview))
    ;; Delete old preview string.
    (pyim-preview-delete-string)
    ;; Insert new preview string.
    (insert preview)
    ;; Highlight new preview string.
    (move-overlay pyim-preview-overlay
                  (overlay-start pyim-preview-overlay) (point))))

(defun pyim-preview-delete-string ()
  "删除已经插入 buffer 的 preview 预览字符串。"
  (if (overlay-start pyim-preview-overlay)
      (delete-region (overlay-start pyim-preview-overlay)
                     (overlay-end pyim-preview-overlay))))

;; ** 选词框相关函数
(defun pyim-candidate-parse (candidate)
  (let ((output
         (if (consp candidate)
             (car candidate)
           candidate)))
    (if (stringp output)
        ;; 注：五笔支持 comments 遗留下来的代码，现在作为兼容而保留，
        ;; 等用户的 dcache 都升级之后，这个就可以删除了。
        (car (split-string output ":"))
      output)))

(defun pyim-outcome-get (&optional n)
  "获取 outcome"
  (nth (or n 0) pyim-outcome-history))

(defun pyim-outcome-handle (type)
  "依照 TYPE, 获取 pyim 的 outcome，并将其加入 `pyim-outcome-history'."
  (cond ((not enable-multibyte-characters)
         (pyim-entered-erase-buffer)
         (setq pyim-outcome-history nil)
         (error "Can't input characters in current unibyte buffer"))
        ((equal type "")
         (setq pyim-outcome-history nil))
        ((eq type 'last-char)
         (push
          (concat (pyim-outcome-get)
                  (pyim-translate last-command-event))
          pyim-outcome-history))
        ((eq type 'candidate)
         (let* ((candidate
                 (pyim-candidate-parse
                  (nth (1- pyim-candidate-position)
                       pyim-candidates))))
           (push
            (concat (pyim-outcome-get) candidate)
            pyim-outcome-history)))
        ((eq type 'candidate-and-last-char)
         (let* ((candidate
                 (pyim-candidate-parse
                  (nth (1- pyim-candidate-position)
                       pyim-candidates))))
           (push
            (concat (pyim-outcome-get)
                    candidate
                    (pyim-translate last-command-event))
            pyim-outcome-history)))
        ((eq type 'pyim-entered)
         (push (pyim-entered-get 'point-before) pyim-outcome-history))
        (t (error "Pyim: invalid outcome"))))

(defun pyim-page-select-word-simple ()
  "从选词框中选择当前词条.
这个函数与 `pyim-page-select-word' 的区别是：
这个函数不会将选择的词条加入个人词库，主要的使用场景是：
当用户需要输入一个生僻字时，输入包含该字的一个词条，
然后再删除不需要的字，由于这个词条不是常用词条，所以
不需要保存到个人词库。"
  (interactive)
  (if (null pyim-candidates)
      (pyim-outcome-handle 'last-char)
    (pyim-outcome-handle 'candidate))
  (pyim-terminate-translation))

(defun pyim-page-select-word ()
  "从选词框中选择当前词条，然后删除该词条对应拼音。"
  (interactive)
  (if (null pyim-candidates)  ; 如果没有选项，输入空格
      (progn
        (pyim-outcome-handle 'last-char)
        (pyim-terminate-translation))
    (let* ((class (pyim-scheme-get-option (pyim-scheme-name) :class))
           (func (intern (format "pyim-page-select-word:%S" class))))
      (if (and class (functionp func))
          (funcall func)
        (call-interactively #'pyim-page-select-word:pinyin)))))

(defun pyim-page-select-word:pinyin ()
  "从选词框中选择当前词条，然后删除该词条对应拼音。"
  (interactive)
  (pyim-outcome-handle 'candidate)
  (let* ((imobj (car pyim-imobjs))
         (length-selected-word
          ;; 获取 *这一次* 选择词条的长度， 在“多次选择词条才能上屏”的情况下，
          ;; 一定要和 outcome 的概念作区别。
          ;; 比如： xiaolifeidao
          ;; 第一次选择：小李， outcome = 小李
          ;; 第二次选择：飞，   outcome = 小李飞
          ;; 第三次选择：刀，   outcome = 小李飞刀
          (- (length (pyim-outcome-get))
             (length (pyim-outcome-get 1))))
         ;; pyim-imobjs 包含 *pyim-entered-buffer* 里面光标前面的字符
         ;; 串，通过与 selected-word 做比较，获取光标前未转换的字符串。
         ;; to-be-translated.
         (to-be-translated (mapconcat #'identity
                                      (mapcar
                                       #'(lambda (w)
                                           (concat (nth 2 w) (nth 3 w)))
                                       (nthcdr length-selected-word imobj))
                                      "")))
    ;; 大体来说，entered 字符串可以分解为三个部分：

    ;; 1. 光标前字符串
    ;;    1. 光标前已经转换的字符串
    ;;    2. 光标前还没有转换的字符串。
    ;; 2. 光标后字符串

    ;; 下面对 entered 字符串的大体思路是：截取已经转换的字符串，把未转
    ;; 换的字符串和光标后的字符串合并后下一轮递归的处理。

    ;; 比如：entered 为 xiaolifeidao, 本次选择 “小李” 之后，需要将
    ;; entered 截断，“小李” 这个词条长度为2, 就将 entered从头开始缩减
    ;; 2 个 imelem 对应的字符，变成 feidao, 为下一次选择 “飞” 做准备。

    ;; 注意事项： 这里有一个假设前提是： 一个 imelem 对应一个汉字，
    ;; 在全拼输入法中，这个假设大多数情况是成立的，但在型码输入法
    ;; 中，比如五笔输入法，就不成立，好在型码输入法一般不需要多次
    ;; 选择。
    (if (or (< length-selected-word (length imobj)) ;是否有未转换的光标前字符串
            (> (length (pyim-entered-get 'point-after)) 0)) ;是否有光标后字符串
        (progn
          (pyim-with-entered-buffer
            ;; 把光标前已转换的 entered 字符串, 从 entered字符串里面剪
            ;; 掉，保留未转换的字符串和光标之后的字符串。
            (delete-region (point-min) (point))
            (insert to-be-translated)
            ;; 为下一次选词作准备，一般情况下词库里面的词条不会超过20
            ;; 个汉字，所以这里光标向前移动不超过20个 imelem. 从而让下
            ;; 一轮处理时的“光标前字符串”比较长，这种方式可能比逐字选
            ;; 择更加好用。
            (goto-char (pyim-entered-next-imelem-position 20 t 1)))
          (pyim-entered-refresh))
      ;; pyim 词频调整策略：
      ;; 1. 如果一个词条是用户在输入过程中，自己新建的词条，那么就将这个词条
      ;;    添加到个人词库的后面（不放置前面是为了减少误输词条的影响）。
      ;; 2. 如果输入的词条，先前已经在候选词列表中，就自动将其放到第一位。
      ;;    这样的话，一个新词要输入两遍之后才可能出现在第一位。
      ;; 3. pyim 在启动的时候，会使用词频信息，对个人词库作一次排序。
      ;;    用作 pyim 下一次使用。
      (if (member (pyim-outcome-get) pyim-candidates)
          (pyim-create-pyim-word (pyim-outcome-get) t)
        (pyim-create-pyim-word (pyim-outcome-get)))

      (pyim-terminate-translation)
      ;; pyim 使用这个 hook 来处理联想词。
      (run-hooks 'pyim-page-select-finish-hook))))

(defun pyim-page-select-word:xingma ()
  "从选词框中选择当前词条，然后删除该词条对应编码。"
  (interactive)
  (pyim-outcome-handle 'candidate)
  (if (pyim-with-entered-buffer
        (and (> (point) 1)
             (< (point) (point-max))))
      (progn
        (pyim-with-entered-buffer
          ;; 把本次已经选择的词条对应的子 entered, 从 entered
          ;; 字符串里面剪掉。
          (delete-region (point-min) (point)))
        (pyim-entered-refresh))
    (when (string-empty-p (pyim-code-search (pyim-outcome-get)
                                            (pyim-scheme-name)))
      (pyim-create-pyim-word (pyim-outcome-get) t))
    (pyim-terminate-translation)
    ;; pyim 使用这个 hook 来处理联想词。
    (run-hooks 'pyim-page-select-finish-hook)))


(defun pyim-page-select-word-by-number (&optional n)
  "使用数字编号来选择对应的词条。"
  (interactive)
  (if (or pyim-page-select-word-by-number n)
      (if (null pyim-candidates)
          (progn
            (pyim-outcome-handle 'last-char)
            (pyim-terminate-translation))
        (let ((index (if (numberp n)
                         (- n 1)
                       (- last-command-event ?1)))
              (end (pyim-page-end)))
          (if (= index -1) (setq index 9) nil)
          (if (> (+ index (pyim-page-start)) end)
              (pyim-page-refresh)
            (setq pyim-candidate-position
                  (+ (pyim-page-start) index))
            (pyim-page-select-word))))
    ;; 有些输入法使用数字键编码，这种情况下，数字键就
    ;; 不能用来选词了。
    (call-interactively #'pyim-self-insert-command)))

;; ** 处理标点符号
(defun pyim-translate-get-trigger-char ()
  "检查 `pyim-translate-trigger-char' 是否为一个合理的 trigger char 。

pyim 的 translate-trigger-char 要占用一个键位，为了防止用户
自定义设置与输入法冲突，这里需要检查一下这个键位设置的是否合理，
如果不合理，就返回输入法默认设定。"
  (let* ((user-trigger-char pyim-translate-trigger-char)
         (user-trigger-char
          (if (characterp user-trigger-char)
              (char-to-string user-trigger-char)
            (when (= (length user-trigger-char) 1)
              user-trigger-char)))
         (first-char (pyim-scheme-get-option
                      (pyim-scheme-name)
                      :first-chars))
         (prefer-trigger-chars (pyim-scheme-get-option
                                (pyim-scheme-name)
                                :prefer-trigger-chars)))
    (if (pyim-string-match-p (regexp-quote user-trigger-char) first-char)
        (progn
          ;; (message "注意：pyim-translate-trigger-char 设置和当前输入法冲突，使用推荐设置：\"%s\""
          ;;          prefer-trigger-chars)
          prefer-trigger-chars)
      user-trigger-char)))

(defun pyim-translate (char)
  "Pyim 字符转换函数，主要用于处理标点符号.

pyim 在运行过程中调用这个函数来进行标点符号格式的转换。

常用的标点符号数量不多，所以 pyim 没有使用文件而是使用一个变量
`pyim-punctuation-dict' 来设置标点符号对应表，这个变量是一个
alist 列表。"
  (let* ((str (char-to-string char))
         ;; 注意：`str' 是 *待输入* 的字符对应的字符串。
         (str-before-1 (pyim-char-before-to-string 0))
         (str-before-2 (pyim-char-before-to-string 1))
         (str-before-3 (pyim-char-before-to-string 2))
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
                       :test #'equal))
         (trigger-str (pyim-translate-get-trigger-char)))
    (cond
     ;; 空格之前的字符什么也不输入。
     ((< char ? ) "")

     ;; 这个部份与标点符号处理无关，主要用来快速删除用户自定义词条。
     ;; 比如：在一个中文字符串后输入 2-v，可以将光标前两个中文字符
     ;; 组成的字符串，从个人词库删除。
     ((and (eq (char-before) ?-)
           (pyim-string-match-p "[0-9]" str-before-2)
           (pyim-string-match-p "\\cc" str-before-3)
           (equal str trigger-str))
      (delete-char -2)
      (pyim-delete-word-at-point
       (string-to-number str-before-2))
      "")
     ;; 这个部份与标点符号处理无关，主要用来快速保存用户自定义词条。
     ;; 比如：在一个中文字符串后输入 2v，可以将光标前两个中文字符
     ;; 组成的字符串，保存到个人词库。
     ((and (member (char-before) (number-sequence ?2 ?9))
           (pyim-string-match-p "\\cc" str-before-2)
           (equal str trigger-str))
      (delete-char -1)
      (pyim-create-word-at-point
       (string-to-number str-before-1))
      "")

     ;; 光标前面的字符为中文字符时，按 v 清洗当前行的内容。
     ((and (not (numberp punc-posit-before-1))
           (pyim-string-match-p "\\cc" str-before-1)
           (equal str trigger-str))
      (funcall pyim-wash-function)
      "")

     ;; 关闭标点转换功能时，只插入英文标点。
     ((not (pyim-punctuation-full-width-p))
      str)

     ;; 当用户使用 org-mode 以及 markdown 等轻量级标记语言撰写文档时，
     ;; 常常需要输入数字列表，比如：

     ;; 1. item1
     ;; 2. item2
     ;; 3. item3

     ;; 在这种情况下，数字后面输入句号必须是半角句号而不是全角句号，
     ;; pyim 调用 `pyim-translate' 时，会检测光标前面的字符，如果这个
     ;; 字符属于 `pyim-punctuation-escape-list' ，pyim 将输入半角标点，
     ;; 具体细节见：`pyim-translate'
     ((member (char-before)
              pyim-punctuation-escape-list)
      str)

     ;; 当 `pyim-punctuation-half-width-functions' 中
     ;; 任意一个函数返回值为 t 时，插入英文标点。
     ((cl-some #'(lambda (x)
                   (if (functionp x)
                       (funcall x char)
                     nil))
               pyim-punctuation-half-width-functions)
      str)

     ;; 当光标前面为英文标点时， 按 `pyim-translate-trigger-char'
     ;; 对应的字符后， 自动将其转换为对应的中文标点。
     ((and (numberp punc-posit-before-1)
           (= punc-posit-before-1 0)
           (equal str trigger-str))
      (pyim-punctuation-translate 'full-width)
      "")

     ;; 当光标前面为中文标点时， 按 `pyim-translate-trigger-char'
     ;; 对应的字符后， 自动将其转换为对应的英文标点。
     ((and (numberp punc-posit-before-1)
           (> punc-posit-before-1 0)
           (equal str trigger-str))
      (pyim-punctuation-translate 'half-width)
      "")

     ;; 正常输入标点符号。
     (punc-list
      (pyim-punctuation-return-proper-punct punc-list))

     ;; 当输入的字符不是标点符号时，原样插入。
     (t str))))

(defun pyim-wash-current-line-function ()
  "清理当前行的内容，比如：删除不必要的空格，等。"
  (interactive)
  (let* ((begin (line-beginning-position))
         (end (point))
         (string (buffer-substring-no-properties begin end))
         new-string)
    (when (> (length string) 0)
      (delete-region begin end)
      (setq new-string
            (with-temp-buffer
              (insert string)
              (goto-char (point-min))
              (while (re-search-forward "\\([，。；？！；、）】]\\)  +\\([[:ascii:]]\\)" nil t)
                (replace-match (concat (match-string 1) (match-string 2))  nil t))
              (goto-char (point-min))
              (while (re-search-forward "\\([[:ascii:]]\\)  +\\([（【]\\)" nil t)
                (replace-match (concat (match-string 1) (match-string 2))  nil t))
              (goto-char (point-min))
              (while (re-search-forward "\\([[:ascii:]]\\)  +\\(\\cc\\)" nil t)
                (replace-match (concat (match-string 1) " " (match-string 2))  nil t))
              (goto-char (point-min))
              (while (re-search-forward "\\(\\cc\\)  +\\([[:ascii:]]\\)" nil t)
                (replace-match (concat (match-string 1) " " (match-string 2))  nil t))
              (buffer-string)))
      (insert new-string))))

;; ** 与拼音输入相关的用户命令
(defun pyim-entered-delete-backward-char (&optional n)
  "在pyim-entered-buffer中向后删除1个字符"
  (interactive)
  (pyim-with-entered-buffer
    (ignore-errors
      (delete-char (- 0 (or n 1)))))
  (if (> (length (pyim-entered-get 'point-before)) 0)
      (pyim-entered-refresh t)
    (pyim-outcome-handle "")
    (pyim-terminate-translation)))

(defun pyim-entered-delete-forward-char ()
  "在pyim-entered-buffer中向前删除1个字符"
  (interactive)
  (pyim-entered-delete-backward-char -1))

(defun pyim-entered-delete-backward-imelem (&optional search-forward)
  "`pyim-entered-buffer’ 中向后删除一个 imelem 对应的字符串"
  (interactive)
  (let ((position (pyim-entered-next-imelem-position 1 search-forward)))
    (pyim-with-entered-buffer
      (delete-region (point) position))
    (pyim-entered-refresh t)))

(defun pyim-entered-delete-forward-imelem ()
  "`pyim-entered-buffer’ 中向前删除一个 imelem 对应的字符串"
  (interactive)
  (pyim-entered-delete-backward-imelem t))

(define-obsolete-function-alias
  'pyim-convert-code-at-point #'pyim-convert-string-at-point "2.0")

;;;###autoload
(defun pyim-convert-string-at-point (&optional return-cregexp)
  "将光标前的用户输入的字符串转换为中文.

如果 RETURN-CREGEXP 为真, pyim 会把用户输入的字符串当作
拼音，依照这个拼音来构建一个 regexp, 用户可以用这个 regexp
搜索拼音对应的汉字。"
  (interactive "P")
  (unless (equal input-method-function 'pyim-input-method)
    (activate-input-method 'pyim))
  (if return-cregexp
      (pyim-convert-cregexp-at-point t)
    (let* ((case-fold-search nil)
           (scheme-name (pyim-scheme-name))
           (first-chars (pyim-scheme-get-option scheme-name :first-chars))
           (rest-chars (pyim-scheme-get-option scheme-name :rest-chars))
           (string (if mark-active
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (buffer-substring (point) (line-beginning-position))))
           code length)
      (cond ((string-match
              ;; 创建一个 regexp, 用于提取出光标处一个适合
              ;; 转换的字符串。
              (format "[%s]+ *$"
                      (cl-delete-duplicates
                       (concat first-chars rest-chars "'-")))
              string)
             (setq code
                   ;; 一些编程语言使用单引号 ' 做为字符串的标记，这里需要特殊处理。
                   (replace-regexp-in-string
                    "^[-']" ""
                    (match-string 0 string)))
             (setq length (length code))
             (setq code (replace-regexp-in-string " +" "" code))
             (when mark-active
               (delete-region
                (region-beginning) (region-end)))
             (when (and (not mark-active) (> length 0))
               (delete-char (- 0 length)))
             (when (> length 0)
               (setq unread-command-events
                     (append (listify-key-sequence code)
                             unread-command-events))
               (setq pyim-force-input-chinese t)))
            ((pyim-string-match-p "[[:punct:]：－]" (pyim-char-before-to-string 0))
             ;; 当光标前的一个字符是标点符号时，半角/全角切换。
             (call-interactively 'pyim-punctuation-translate-at-point))
            ((and nil ;; 暂时还没有准备启用这个功能
                  (eq pyim-default-scheme 'quanpin)
                  (string-match "\\cc *$" string))
             ;; 如果光标处是汉字，就用汉字的拼音来重新启动输入法
             (setq string
                   (if mark-active
                       string
                     (match-string 0 string)))
             (setq length (length string))
             (when mark-active
               (delete-region
                (region-beginning) (region-end)))
             (when (and (not mark-active) (> length 0))
               (delete-char (- 0 length)))
             (setq code (pyim-cstring-to-pinyin
                         (replace-regexp-in-string " " "" string)
                         nil "-" nil t))
             (when (and (> code 0)
                        (> length 0))
               (setq unread-command-events
                     (append (listify-key-sequence code)
                             unread-command-events))
               (setq pyim-force-input-chinese t)))
            (t (message "Pyim: pyim-convert-string-at-point do noting."))))))

(defun pyim-quit-clear ()
  "取消当前输入的命令."
  (interactive)
  (pyim-outcome-handle "")
  (pyim-terminate-translation))

(defun pyim-quit-no-clear ()
  "字母上屏命令."
  (interactive)
  (pyim-outcome-handle 'pyim-entered)
  (pyim-terminate-translation))

(defun pyim-inactivate ()
  "取消 pyim 的激活状态."
  (interactive)
  (mapc #'kill-local-variable pyim-local-variable-list)
  (run-hooks 'pyim-inactive-hook))

(defun pyim-toggle-input-ascii ()
  "pyim 切换中英文输入模式。同时调整标点符号样式。"
  (interactive)
  (setq pyim-input-ascii
        (not pyim-input-ascii)))

;; ** pyim 中文字符串工具
(require 'pyim-cstring)

;; ** pyim 探针程序
(require 'pyim-probe)

;; * Footer
(provide 'pyim)

;;; pyim.el ends here
