# Created 2021-04-23 Fri 09:25
#+TITLE: PYIM 是一个 Emacs 中文输入法，支持全拼，双拼，五笔，仓颉 和 Rime 等
#+AUTHOR: Feng Shu

#+html: <a href="https://github.com/tumashu/pyim/actions/workflows/test.yml"><img alt="Github Action" src="https://github.com/tumashu/pyim/actions/workflows/test.yml/badge.svg"/></a>
#+html: <a href="http://elpa.gnu.org/packages/pyim.html"><img alt="GNU ELPA" src="https://elpa.gnu.org/packages/pyim.svg"/></a>
#+html: <a href="http://elpa.gnu.org/devel/pyim.html"><img alt="GNU-devel ELPA" src="https://elpa.gnu.org/devel/pyim.svg"/></a>
#+html: <a href="https://melpa.org/#/pyim"><img alt="MELPA" src="https://melpa.org/packages/pyim-badge.svg"/></a>

* Changlog

** <2021-04-28 Wed> 五笔输入法和仓颉输入法的不兼容更新

五笔输入法和仓颉输入法的 code-prefix 原来是使用一个标点符号作为 code-prefix, 现
在使用 "wubi/" 和 "cangjie/" 这种形式的 code-prefix, 减少不同输入法误用相同
code-prefix 带来的词库冲突。

五笔输入法的 scheme 设置已经移到 pyim-wbdict 包，仓颉输入法的 scheme 设置已经移
动到 pyim-cangjie5dict 包。

对于使用上述两个包的用户，此次变更不受影响，因为我已经更新两个包，让其使用新的
code-prefix.

受影响的是自己维护词库的五笔和仓颉用户，需要做以下更新：
1. 五笔用户
   1. 需要 (require 'pyim-wbdict), 加载五笔 scheme 设置。
   2. 需要将自己的五笔词库文件中的 code-prefix "." 替换为 "wubi/".
   3. 运行 `pyim-dcache-upgrade-icode2word' 命令，升级 icode2word 词库缓存。
2. 仓颉用户
   1. 需要 (require 'pyim-cangjie5dict), 加载仓颉 scheme 设置。
   2. 需要将自己的五笔词库文件中的 code-prefix "@" 替换为 "cangjie/".

* 截图
[[file:./snapshots/pyim-linux-x-with-toolkit.png]]

* 简介
pyim 是 Emacs 环境下的一个中文输入法，最初它只支持全拼输入，所以当时 "pyim" 代表
"Chinese Pinyin Input Method" 的意思，后来根据同学的提议，添加了五笔等输入法的支
持，再叫 “拼音输入法” 就不太合适了，所以你现在可以叫它 “朋友输入法”

#+begin_center
                            (Peng You input method)
#+end_center


* 背景
pyim 的代码源自 Emacs-eim。

Emacs-eim 是 Emacs 环境下的一个中文输入法框架， 支持拼音，五笔，仓颉以及二笔等多
种输入法，但遗憾的是，2008 年之后它就停止了开发。

虽然外部输入法功能强大，但不能和 Emacs 默契的配合，这一点极大的损害了 Emacs 那种
*行云流水* 的感觉。而本人在使用 Emacs-eim 的过程中发现：

1. *当 Emacs-eim 词库词条很大时，选词频率大大降低，中文体验增强。*
2. *随着使用时间的延长，Emacs-eim 会越来越好用（个人词库的积累）。*

于是我 fork 了 Emacs-eim 输入法的部分代码, 创建了新项目：pyim。

* 目标
pyim 的目标是： *尽最大的努力成为一个好用的 Emacs 中文输入法* ，
具体可表现为三个方面：

1. Fallback: 当外部输入法不能使用时，比如在 console 或者 cygwin 环境下，尽最大可
   能让 Emacs 用户不必为输入中文而烦恼。
2. Integration: 尽最大可能减少输入法切换频率，让中文输入不影响 Emacs的体验。
3. Exchange: 尽最大可能简化 pyim 使用其他优秀输入法的词库的难度和复杂度。

* 特点
1. pyim 支持全拼，双拼，五笔和仓颉等，其中对全拼的支持最好。
2. pyim 通过添加词库的方式优化输入法。
3. pyim 使用文本词库格式，方便处理。
4. pyim 可以作为 rime 的前端使用。

* 安装
1. M-x package-install RET pyim RET
2. 在 Emacs 配置文件中（比如: ~/.emacs）添加如下代码：
   #+begin_example
   (require 'pyim)
   (require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
   (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
   (setq default-input-method "pyim")
   #+end_example

* 配置

** 配置实例
对 pyim 感兴趣的同学，可以看看本人的 pyim 配置，但要注意不要乱抄探针配置。

#+begin_src elisp
(require 'pyim)

(setq default-input-method "pyim")

;; 金手指设置，可以将光标处的编码，比如：拼音字符串，转换为中文。
(global-set-key (kbd "M-j") 'pyim-convert-string-at-point)

;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
(define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)

;; 我使用全拼
(pyim-default-scheme 'quanpin)
;; (pyim-default-scheme 'wubi)
;; (pyim-default-scheme 'cangjie)

;; pyim 探针设置
;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;; 我自己使用的中英文动态切换规则是：
;; 1. 光标只有在注释里面时，才可以输入中文。
;; 2. 光标前是汉字字符时，才能输入中文。
;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
;; (setq-default pyim-english-input-switch-functions
;;               '(pyim-probe-dynamic-english
;;                 pyim-probe-isearch-mode
;;                 pyim-probe-program-mode
;;                 pyim-probe-org-structure-template))

;; (setq-default pyim-punctuation-half-width-functions
;;               '(pyim-probe-punctuation-line-beginning
;;                 pyim-probe-punctuation-after-punctuation))

;; 开启代码搜索中文功能（比如拼音，五笔码等）
(pyim-isearch-mode 1)

;; 设置选词框的绘制方式
(if (posframe-workable-p)
    (setq pyim-page-tooltip 'posframe)
  (setq pyim-page-tooltip 'popup))

;; 显示5个候选词。
(setq pyim-page-length 5)

;; Basedict
(require 'pyim-basedict)
(pyim-basedict-enable)
#+end_src

** 添加词库文件
pyim 当前的默认的拼音词库是 pyim-basedict, 这个词库的词条量8万左右，是一个 *非
常小* 的拼音词库，源于：libpinyin 项目

如果 pyim-basedict 不能满足需求，用户可以使用其他方式为 pyim 添加拼音词库，具体
方式请参考 [[如何添加自定义拼音词库]] 小结。

** 激活 pyim

#+begin_example
(setq default-input-method "pyim")
(global-set-key (kbd "C-\\") 'toggle-input-method)
#+end_example

* 使用
** 常用快捷键
| 输入法快捷键          | 功能                       |
|-----------------------+----------------------------|
| C-n 或 M-n 或 + 或 .  | 向下翻页                   |
| C-p 或 M-p 或 - 或 ,  | 向上翻页                   |
| C-f                   | 选择下一个备选词           |
| C-b                   | 选择上一个备选词           |
| SPC                   | 确定输入                   |
| RET 或 C-m            | 字母上屏                   |
| C-c                   | 取消输入                   |
| C-g                   | 取消输入并保留已输入的中文 |
| TAB                   | 模糊音调整                 |
| DEL 或 BACKSPACE      | 删除最后一个字符           |
| C-DEL 或  C-BACKSPACE | 删除最后一个拼音           |
| M-DEL 或  M-BACKSPACE | 删除最后一个拼音           |

** 使用双拼模式
pyim 支持双拼输入模式，用户可以通过变量 `pyim-default-scheme' 来设定：

#+begin_example
(pyim-default-scheme 'pyim-shuangpin)
#+end_example

注意：
1. pyim 支持微软双拼（microsoft-shuangpin）和小鹤双拼（xiaohe-shuangpin）。
2. 用户可以使用函数 `pyim-scheme-add' 添加自定义双拼方案。
3. 用户可能需要重新设置 `pyim-outcome-trigger'。

** 使用 rime 输入法
具体安装和使用方式请查看 pyim-liberime 包的 Commentary 部分。

** 使用型码输入法
1. 五笔输入法可以参考： https://github.com/tumashu/pyim-wbdict
2. 仓颉输入法可以参考：https://github.com/p1uxtar/pyim-cangjiedict
3. 三码郑码（至至郑码）输入法可以参考： https://github.com/p1uxtar/pyim-smzmdict

如果用户在使用型码输入法的过程中，忘记了某个字的编码，可以按 TAB 键临时切换到辅
助输入法来输入，辅助输入法可以通过 `pyim-assistant-scheme' 来设置。

** 让选词框跟随光标
用户可以通过下面的设置让 pyim 在 *光标处* 显示一个选词框：

1. 使用 popup 包来绘制选词框 （emacs overlay 机制）
   #+begin_example
   (setq pyim-page-tooltip 'popup)
   #+end_example
2. 使用 posframe 来绘制选词框
   #+begin_example
   (setq pyim-page-tooltip 'posframe)
   #+end_example
   注意：pyim 不会自动安装 posframe, 用户需要手动安装这个包，

** 调整 tooltip 选词框的显示样式
pyim 的选词框默认使用 *双行显示* 的样式，在一些特殊的情况下（比如：popup 显示的
菜单错位），用户可以使用 *单行显示*的样式：

#+begin_example
(setq pyim-page-style 'one-line)
#+end_example

注：用户可以添加函数 pyim-page-style:STYLENAME 来定义自己的选词框格式。

** 设置模糊音
可以通过设置 `pyim-pinyin-fuzzy-alist' 变量来自定义模糊音。

** 使用魔术转换器
用户可以将待选词 “特殊处理” 后再 “上屏”，比如 “简体转繁体” 或者 “输入中文，上屏
英文” 之类的。

用户需要设置 `pyim-magic-converter', 比如：下面这个例子实现，输入 “二呆”，“一个
超级帅的小伙子” 上屏 :-)

#+begin_example
(defun my-converter (string)
  (if (equal string "二呆")
      "“一个超级帅的小伙子”"
    string))
(setq pyim-magic-converter #'my-converter)
#+end_example

** 切换全角标点与半角标点

1. 第一种方法：使用命令 `pyim-punctuation-toggle'，全局切换。这个命令主要用来设
   置变量： `pyim-punctuation-translate-p', 用户也可以手动设置这个变量， 比如：
   
   #+begin_example
   (setq pyim-punctuation-translate-p '(yes no auto))   ;使用全角标点。
   (setq pyim-punctuation-translate-p '(no yes auto))   ;使用半角标点。
   (setq pyim-punctuation-translate-p '(auto yes no))   ;中文使用全角标点，英文使用半角标点。
   #+end_example
   
2. 第二种方法：使用命令 `pyim-punctuation-translate-at-point' 只切换光标处标点的
   样式。
3. 第三种方法：设置变量 `pyim-outcome-trigger' ，输入变量设定的字符会切换光标处
   标点的样式。

** 手动加词和删词

1. `pyim-convert-string-at-point' 金手指命令，可以比较方便的添加和删除词条，比如：
   1. 在 "你好" 后面输入2, 然后运行金手指命令，可以将 “你好” 加入个人词库。
   2. 在 “你好” 后面输入2-, 然后运行金手指命令，可以将 “你好” 从个人词库删除。
   3. 如果用户选择了一个词条，则运行金手指命令可以将选择的词条加入个人词库。
   4. 如果用户在汉字后面输入"-", 然后运行金手指命令，可以将最近一次创建的词条删除。
2. `pyim-create-Ncchar-word-at-point' 这是一组命令，从光标前提取N个汉字字符组成字
   符串，并将其加入个人词库。
3. `pyim-outcome-trigger' 以默认设置为例：在 “我爱吃红烧肉” 后输入 “5v”，可以将
   “爱吃红烧肉”这个词条保存到用户个人词库。
4. `pyim-create-word-from-selection', 选择一个词条，运行这个命令后，就可以将这个
   词条添加到个人词库。
5. `pyim-delete-word' 从个人词库中删除当前高亮选择的词条。

** pyim 输入状态指示器
pyim 输入状态指示器可以帮助用户快速了解当前 pyim 是处于英文输入状态还是中文输入
状态，因为 pyim probe 探针功能可以让中英文输入状态动态切换，所以快速了解当前中英
文输入状态有时候显得很重要。

pyim 当前内置三种指示器实现方式：
1. 改变光标颜色： pyim-indicator-with-cursor-color, 用户可以使用变量
   pyim-indicator-cursor-color 来配置两种输入状态对应的光标颜色。
2. 使用 modeline 显示状态字符串：pyim-indicator-with-mode-line, 用户可以使用变量
   pyim-indicator-modeline-string 来配置两种状态对应的显示字符串。
3. 使用 posframe 来显示一个带颜色小点：pyim-indicator-with-posframe

设置默认启用的指示器有两个，用户可以使用下面的变量调整：
#+begin_example
(setq pyim-indicator-list (list #'pyim-indicator-with-cursor-color #'pyim-indicator-with-modeline))
#+end_example

注意：用户切换 emacs 主题之后，最好重启 pyim 一下。

** pyim 高级功能
1. 根据环境自动切换到英文输入模式，使用 pyim-english-input-switch-functions 配置。
2. 根据环境自动切换到半角标点输入模式，使用 pyim-punctuation-half-width-functions 配置。

注意：上述两个功能使用不同的变量设置， *千万不要搞错* 。

*** 根据环境自动切换到英文输入模式

| 探针函数                          | 功能说明                                                                          |
|-----------------------------------+-----------------------------------------------------------------------------------|
| pyim-probe-program-mode           | 如果当前的 mode 衍生自 prog-mode，那么仅仅在字符串和 comment 中开启中文输入模式   |
|-----------------------------------+-----------------------------------------------------------------------------------|
| pyim-probe-org-speed-commands     | 解决 org-speed-commands 与 pyim 冲突问题                                          |
| pyim-probe-isearch-mode           | 使用 isearch 搜索时，强制开启英文输入模式                                         |
|                                   | 注意：想要使用这个功能，pyim-isearch-mode 必须激活                                |
|-----------------------------------+-----------------------------------------------------------------------------------|
| pyim-probe-org-structure-template | 使用 org-structure-template 时，关闭中文输入模式                                  |
|-----------------------------------+-----------------------------------------------------------------------------------|
|                                   | 1. 当前字符为中文字符时，输入下一个字符时默认开启中文输入                         |
| pyim-probe-dynamic-english        | 2. 当前字符为其他字符时，输入下一个字符时默认开启英文输入                         |
|                                   | 3. 使用命令 pyim-convert-string-at-point 可以将光标前的拼音字符串强制转换为中文。 |
|-----------------------------------+-----------------------------------------------------------------------------------|

激活方式：

#+begin_example
(setq-default pyim-english-input-switch-functions
              '(probe-function1 probe-function2 probe-function3))
#+end_example

注意事项：
1. 上述函数列表中，任意一个函数的返回值为 t 时，pyim 切换到英文输入模式。
2. [[https://github.com/DogLooksGood/emacs-rime][Emacs-rime]] 和 [[https://github.com/laishulu/emacs-smart-input-source][smart-input-source]] 也有类似探针的功能，其对应函数可以直接或者简
   单包装后作为 pyim 探针使用，有兴趣的同学可以了解一下。

*** 根据环境自动切换到半角标点输入模式

| 探针函数                                 | 功能说明                   |
|------------------------------------------+----------------------------|
| pyim-probe-punctuation-line-beginning    | 行首强制输入半角标点       |
|------------------------------------------+----------------------------|
| pyim-probe-punctuation-after-punctuation | 半角标点后强制输入半角标点 |
|------------------------------------------+----------------------------|

激活方式：

#+begin_example
(setq-default pyim-punctuation-half-width-functions
              '(probe-function4 probe-function5 probe-function6))
#+end_example

注：上述函数列表中，任意一个函数的返回值为 t 时，pyim 切换到半角标点输入模式。


* 开发
请参考 [[file:Development.org][Development.org]] 文档
* 捐赠
您可以通过小额捐赠的方式支持 pyim 的开发工作，具体方式：

1. 通过支付宝收款账户：tumashu@163.com
2. 通过支付宝钱包扫描：

   [[file:snapshots/QR-code-for-author.jpg]]

* Tips

** 如何快速切换 scheme

可以试试 pyim-default-scheme 命令。

** 关闭输入联想词功能 (默认开启)

#+begin_example
(setq pyim-enable-shortcode nil)
#+end_example

** 如何将个人词条相关信息导入和导出？

1. 导入使用命令： pyim-dcache-import
2. 导出使用命令： pyim-dcache-export

** pyim 出现错误时，如何开启 debug 模式

#+begin_example
(setq debug-on-error t)
#+end_example

** 将光标处的拼音或者五笔字符串转换为中文 (与 vimim 的 “点石成金” 功能类似)

#+begin_example
(global-set-key (kbd "M-i") 'pyim-convert-string-at-point)
#+end_example

** 如何使用其它字符翻页

#+begin_example
(define-key pyim-mode-map "." 'pyim-page-next-page)
(define-key pyim-mode-map "," 'pyim-page-previous-page)
#+end_example

** 如何用 ";" 来选择第二个候选词

#+begin_example
(define-key pyim-mode-map ";"
  (lambda ()
    (interactive)
    (pyim-select-word-by-number 2)))
#+end_example

** 如何添加自定义拼音词库
pyim 默认没有携带任何拼音词库，用户可以使用下面几种方式，获取质量较好的拼音词库：

*** 第－种方式 (Windows 用户推荐使用)

使用词库转换工具将其他输入法的词库转化为 pyim 使用的词库：这里只介绍 windows 平
台下的一个词库转换软件：

1. 软件名称： imewlconverter
2. 中文名称： 深蓝词库转换
3. 下载地址： https://github.com/studyzy/imewlconverter
4. 依赖平台： Microsoft .NET Framework (>= 3.5)

使用方式：

[[file:snapshots/imewlconverter-basic.gif]]

如果生成的词库词频不合理，可以按照下面的方式处理（非常有用的功能）：

[[file:snapshots/imewlconverter-wordfreq.gif]]

生成词库后，运行 `pyim-dicts-manager' ，按照命令提示，将转换得到的词库文件的信息
添加到 `pyim-dicts' 中，完成后运行命令 `pyim-restart' 或者重启emacs。

*** 第二种方式 (Linux & Unix 用户推荐使用)
E-Neo 同学编写了一个词库转换工具: [[https://github.com/E-Neo/scel2pyim][scel2pyim]] , 可以将一个搜狗词库转换为 pyim 词库。

1. 软件名称： scel2pyim
2. 下载地址： https://github.com/E-Neo/scel2pyim
3. 编写语言： C语言

*** 第三种方式

可以了解：https://github.com/redguardtoo/pyim-tsinghua-dict

** 如何手动安装和管理词库
这里假设有两个词库文件：

1. /path/to/pyim-dict1.pyim
2. /path/to/pyim-dict2.pyim

在 ~/.emacs 文件中添加如下一行配置。

#+begin_example
(setq pyim-dicts
      '((:name "dict1" :file "/path/to/pyim-dict1.pyim")
        (:name "dict2" :file "/path/to/pyim-dict2.pyim")))
#+end_example

注意事项:
1. 只有 :file 是 *必须* 设置的。
2. 必须使用词库文件的绝对路径。
3. 词库文件的编码必须为 utf-8-unix，否则会出现乱码。

** Emacs 启动时加载 pyim 词库

#+begin_example
(add-hook 'emacs-startup-hook
          (lambda () (pyim-restart-1 t)))
#+end_example

** 将汉字字符串转换为拼音字符串
下面两个函数可以将中文字符串转换的拼音字符串或者列表，用于 emacs-lisp 编程。

1. `pyim-cstring-to-pinyin' （考虑多音字）
2. `pyim-cstring-to-pinyin-simple'  （不考虑多音字）

** 中文分词
pyim 包含了一个简单的分词函数：`pyim-cstring-split-to-list', 可以将一个中文字符
串分成一个词条列表，比如：

#+begin_example
                  (("天安" 5 7)
我爱北京天安门 ->  ("天安门" 5 8)
                   ("北京" 3 5)
                   ("我爱" 1 3))
#+end_example

其中，每一个词条列表中包含三个元素，第一个元素为词条本身，第二个元素为词条相对于
字符串的起始位置，第三个元素为词条结束位置。

另一个分词函数是 `pyim-cstring-split-to-string', 这个函数将生成一个新的字符串，
在这个字符串中，词语之间用空格或者用户自定义的分隔符隔开。

注意，上述两个分词函数使用暴力匹配模式来分词，所以，*不能检测出* pyim 词库中不存
在的中文词条。

** 获取光标处的中文词条
pyim 包含了一个简单的命令：`pyim-cstring-words-at-point', 这个命令可以得到光标处
的 *英文* 或者 *中文* 词条的 *列表*，这个命令依赖分词函数：
`pyim-cstring-split-to-list'。

** 让 `forward-word' 和 `back-backward’ 在中文环境下正常工作
中文词语没有强制用空格分词，所以 Emacs 内置的命令 `forward-word' 和
`backward-word' 在中文环境不能按用户预期的样子执行，而是 forward/backward “句子”
，pyim自带的两个命令可以在中文环境下正常工作：

1. `pyim-forward-word
2. `pyim-backward-word

用户只需将其绑定到快捷键上就可以了，比如：

#+begin_example
(global-set-key (kbd "M-f") 'pyim-forward-word)
(global-set-key (kbd "M-b") 'pyim-backward-word)
#+end_example

** 为 isearch 相关命令添加拼音搜索支持
pyim 安装后，可以通过下面的设置开启拼音搜索功能：

#+begin_example
(pyim-isearch-mode 1)
#+end_example

注意：这个功能有一些限制，搜索字符串中只能出现 “a-z” 和 “’”，如果有其他字符（比
如 regexp 操作符），则自动关闭拼音搜索功能。

开启这个功能后，一些 isearch 扩展有可能失效，如果遇到这种问题，
只能禁用这个 Minor-mode，然后联系 pyim 的维护者，看有没有法子实现兼容。

用户激活这个 mode 后，可以使用下面的方式 *强制关闭* isearch 搜索框中文输入（即使
在 pyim 激活的时候）。

#+begin_example
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-isearch-mode))
#+end_example

** 创建一个搜索中文的 regexp

#+begin_src emacs-lisp
(pyim-cregexp-build ".*nihao.*")
#+end_src

#+RESULTS:
: \(?:.*\(?:nihao\|[乜伲佞你倪凝匿呢咛啮嗫坭埝妞妮娘嬲孽宁尼尿嵲年廿念忸怩恁您慝扭拈拗拟拧捏捻摄撵旎昵杻柠氽泞泥涅溺牛狃狞猊甯疒睨碾祢粘糵纽聂聍脲腻臬苧茑菍蔫薿蘖蚴袅蹑辇辗逆酿钮铌镊镍陧霓颞鲇鲵鲶鸟鸮鹝鹢麑黏][号嗥嚆嚎壕好昊毫浩淏濠灏皓皞耗蒿薅蚝诐豪貉郝鄗镐颢鸮]\).*\)

** 让 ivy 支持拼音搜索候选项功能

#+begin_example
(setq ivy-re-builders-alist
      '((t . pyim-cregexp-ivy)))
#+end_example

** 让 vertico, selectrum 等补全框架，通过 orderless 支持拼音搜索候选项功能。

#+begin_example
(defun my-orderless-regexp (orig_func component)
  (let ((result (funcall orig_func component)))
    (pyim-cregexp-build result)))

(advice-add 'orderless-regexp :around #'my-orderless-regexp)
#+end_example
