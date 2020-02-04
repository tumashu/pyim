;;; pyim.el --- A Chinese input method support quanpin, shuangpin, wubi and cangjie.

;; * Header
;; Copyright 2006 Ye Wenbin
;;           2014-2019 Feng Shu

;; Author: Ye Wenbin <wenbinye@163.com>
;;         Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/pyim
;; Keywords: convenience, Chinese, pinyin, input-method

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

;; * pyim 使用說明                                          :README:doc:
;; ** 截圖
;; [[./snapshots/pyim-linux-x-with-toolkit.png]]

;; ** 簡介
;; pyim 是 Emacs 環境下的一個中文輸入法，最初它只支持全拼輸入，所以當時
;; "pyim" 代表 "Chinese Pinyin Input Method" 的意思，後來根據同學的提議，
;; 添加了五筆的支持，再叫 “拼音輸入法” 就不太合適了，所以你現在可以將它理解
;; 為 “PengYou input method”：平時像朋友一樣幫助你，偶爾也像朋友一樣犯二 。。。

;; ** 背景
;; pyim 的代碼源自 emacs-eim。

;; emacs-eim 是 Emacs 環境下的一個中文輸入法框架，支持拼音，五筆，倉頡以及二筆等
;; 多種輸入法，但遺憾的是，2008 年之後它就停止了開發，我認為主要原因是外部中文輸入法快速發展。

;; 雖然外部輸入法功能強大，但不能和 Emacs 默契的配合，這一點極大的損害了 Emacs 那種 *行雲流水*
;; 的感覺。而本人在使用（或者叫折騰） emacs-eim 的過程中發現：

;; 1. *當 emacs-eim 詞庫詞條超過 100 萬時，選詞頻率大大降低，中文體驗增強。*
;; 3. *隨著使用時間的延長，emacs-eim 會越來越好用（個人詞庫的積累）。*

;; 於是我 fork 了 emacs-eim 輸入法的部分代碼，創建了一個項目：pyim。

;; ** 目標
;; pyim 的目標是： *盡最大的努力成為一個好用的 Emacs 中文輸入法* ，
;; 具體可表現為三個方面：

;; 1. Fallback:     當外部輸入法不能使用時，比如在 console 或者 cygwin 環境
;;    下，盡最大可能讓 Emacs 用戶不必為輸入中文而煩惱。
;; 2. Integration:  盡最大可能減少輸入法切換頻率，讓中文輸入不影響 Emacs
;;    的體驗。
;; 3. Exchange:     盡最大可能簡化 pyim 使用其他優秀輸入法的詞庫
;;    的難度和復雜度。

;; ** 特點
;; 1. pyim 支持全拼，雙拼，五筆和倉頡，其中對全拼的支持最好。
;; 2. pyim 通過添加詞庫的方式優化輸入法。
;; 3. pyim 使用文本詞庫格式，方便處理。

;; ** 安裝
;; 1. 配置 melpa 源，參考：http://melpa.org/#/getting-started
;; 2. M-x package-install RET pyim RET
;; 3. 在 Emacs 配置文件中（比如: ~/.emacs）添加如下代碼：
;;    #+BEGIN_EXAMPLE
;;    (require 'pyim)
;;    (require 'pyim-basedict) ; 拼音詞庫設置，五筆用戶 *不需要* 此行設置
;;    (pyim-basedict-enable)   ; 拼音詞庫，五筆用戶 *不需要* 此行設置
;;    (setq default-input-method "pyim")
;;    #+END_EXAMPLE

;; ** 配置

;; *** 配置實例
;; 對 pyim 感興趣的同學，可以看看本人的 pyim 配置（總是適用於最新版的 pyim）:

;; #+BEGIN_EXAMPLE
;; (use-package pyim
;;   :ensure nil
;;   :demand t
;;   :config
;;   ;; 激活 basedict 拼音詞庫，五筆用戶請繼續閱讀 README
;;   (use-package pyim-basedict
;;     :ensure nil
;;     :config (pyim-basedict-enable))

;;   (setq default-input-method "pyim")

;;   ;; 我使用全拼
;;   (setq pyim-default-scheme 'quanpin)

;;   ;; 設置 pyim 探針設置，這是 pyim 高級功能設置，可以實現 *無痛* 中英文切換 :-)
;;   ;; 我自己使用的中英文動態切換規則是：
;;   ;; 1. 光標只有在註釋裡面時，才可以輸入中文。
;;   ;; 2. 光標前是漢字字元時，才能輸入中文。
;;   ;; 3. 使用 M-j 快捷鍵，強制將光標前的拼音字元串轉換為中文。
;;   (setq-default pyim-english-input-switch-functions
;;                 '(pyim-probe-dynamic-english
;;                   pyim-probe-isearch-mode
;;                   pyim-probe-program-mode
;;                   pyim-probe-org-structure-template))

;;   (setq-default pyim-punctuation-half-width-functions
;;                 '(pyim-probe-punctuation-line-beginning
;;                   pyim-probe-punctuation-after-punctuation))

;;   ;; 開啟拼音搜索功能
;;   (pyim-isearch-mode 1)

;;   ;; 使用 popup-el 來繪制選詞框，如果用 emacs26，建議設置
;;   ;; 為 'posframe，速度很快並且菜單不會變形，不過需要用戶
;;   ;; 手動安裝 posframe 包。
;;   (setq pyim-page-tooltip 'popup)

;;   ;; 選詞框顯示5個候選詞
;;   (setq pyim-page-length 5)

;;   :bind
;;   (("M-j" . pyim-convert-string-at-point) ;與 pyim-probe-dynamic-english 配合
;;    ("C-;" . pyim-delete-word-from-personal-buffer)))
;; #+END_EXAMPLE

;; *** 添加詞庫文件
;; pyim 當前的預設的拼音詞庫是 pyim-basedict，這個詞庫的詞條量
;; 8 萬左右，是一個 *非常小* 的拼音詞庫，詞條的來源有兩個：

;; 1. libpinyin 項目的內置詞庫
;; 2. pyim 用戶貢獻的個人詞庫

;; 如果 pyim-basedict 不能滿足需求，用戶可以使用其他方式為 pyim 添加拼音詞庫，
;; 具體方式請參考 [[如何添加自定義拼音詞庫]] 小結。

;; *** 激活 pyim

;; #+BEGIN_EXAMPLE
;; (setq default-input-method "pyim")
;; (global-set-key (kbd "C-\\") 'toggle-input-method)
;; #+END_EXAMPLE

;; ** 使用
;; *** 常用快捷鍵
;; | 輸入法快捷鍵          | 功能                       |
;; |-----------------------+----------------------------|
;; | C-n 或 M-n 或 + 或 .  | 向下翻頁                   |
;; | C-p 或 M-p 或 - 或 ,  | 向上翻頁                   |
;; | C-f                   | 選擇下一個備選詞           |
;; | C-b                   | 選擇上一個備選詞           |
;; | SPC                   | 確定輸入                   |
;; | RET 或 C-m            | 字母上屏                   |
;; | C-c                   | 取消輸入                   |
;; | C-g                   | 取消輸入並保留已輸入的中文 |
;; | TAB                   | 模糊音調整                 |
;; | DEL 或 BACKSPACE      | 刪除最後一個字元           |
;; | C-DEL 或  C-BACKSPACE | 刪除最後一個拼音           |
;; | M-DEL 或  M-BACKSPACE | 刪除最後一個拼音           |

;; *** 使用雙拼模式
;; pyim 支持雙拼輸入模式，用戶可以通過變數 `pyim-default-scheme' 來設定：

;; #+BEGIN_EXAMPLE
;; (setq pyim-default-scheme 'pyim-shuangpin)
;; #+END_EXAMPLE

;; 注意：
;; 1. pyim 支持微軟雙拼（microsoft-shuangpin）和小鶴雙拼（xiaohe-shuangpin）。
;; 2. 用戶可以使用變數 `pyim-schemes' 添加自定義雙拼方案。
;; 3. 用戶可能需要重新設置 `pyim-translate-trigger-char'。

;; *** 通過 pyim 來支持 rime 所有輸入法

;; 1. 安裝配置 liberime 和 pyim，方式見：[[https://github.com/merrickluo/liberime][liberime]]。
;; 2. 將 liberime 的 page_size 設置為 100，這樣 pyim 一次可以獲取 100
;;    個候選詞，然後自己處理分頁。用戶可以按 TAB 鍵切換到輔助輸入
;;    法來輸入 100 以後的詞條。

;;    手動設置方式是：在 `liberime-user-data-dir'/default.custom.yaml
;;    文件中添加類似下麵的內容：

;;    #+BEGIN_EXAMPLE
;;    patch:
;;         "menu/page_size": 100
;;         "speller/auto_select": false
;;         "speller/auto_select_unique_candidate": false
;;    #+END_EXAMPLE

;; 3. 使用 rime 全拼輸入法的用戶，也可以使用 rime-quanpin scheme,
;;    這個 scheme 是專門針對 rime 全拼輸入法定製的，支持全拼v快捷鍵。
;;    #+BEGIN_EXAMPLE
;;    (setq pyim-default-scheme 'rime-quanpin)
;;    #+END_EXAMPLE
;; 4. 如果通過 rime 使用微軟雙拼，可以用以下設置：
;;    #+BEGIN_EXAMPLE
;;    (liberime-select-schema "double_pinyin_mspy")
;;    (setq pyim-default-scheme 'rime-microsoft-shuangpin)
;;    #+END_EXAMPLE
;;    預設是用繁體中文，想要改成簡體中文的話，可以參考 [[https://github.com/rime/home/wiki/CustomizationGuide#%E4%B8%80%E4%BE%8B%E5%AE%9A%E8%A3%BD%E7%B0%A1%E5%8C%96%E5%AD%97%E8%BC%B8%E5%87%BA][rime wiki]]，或者[[http://wenshanren.org/?p=1070#orgc7dbd8e][這篇博客]]
;; *** 使用五筆輸入
;; pyim 支持五筆輸入模式，用戶可以通過變數 `pyim-default-scheme' 來設定：

;; #+BEGIN_EXAMPLE
;; (setq pyim-default-scheme 'wubi)
;; #+END_EXAMPLE

;; 在使用五筆輸入法之前，請用 pyim-dicts-manager 添加一個五筆詞庫，詞庫的格式類似：

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

;; 最簡單的方式是從 melpa 中安裝 pyim-wbdict 包，然後根據它的
;; [[https://github.com/tumashu/pyim-wbdict][README]] 來配置。

;; 另外 Ye FeiYu 同學維護著 pyim-wbdict 的一個 fork，裡面包含著極點
;; 五筆和清歌五筆的詞庫，不做發布，有興趣的同學可以瞭解一下：

;;     https://github.com/yefeiyu/pyim-wbdict

;; 如果用戶在使用五筆輸入法的過程中，忘記了某個字的五筆碼，可以按 TAB
;; 鍵臨時切換到輔助輸入法來輸入，選詞完成之後自動退出。輔助輸入法可以
;; 通過 `pyim-assistant-scheme' 來設置。

;; *** 使用倉頡輸入法
;; pyim 支持倉頡輸入法，用戶可以通過變數 `pyim-default-scheme' 來設定：

;; #+BEGIN_EXAMPLE
;; (setq pyim-default-scheme 'cangjie)
;; #+END_EXAMPLE

;; 在使用倉頡輸入法之前，請用 pyim-dicts-manager 添加一個倉頡詞庫，詞庫的格式類似：

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

;; 如果用戶使用倉頡第五代，最簡單的方式是從 melpa 中安裝 pyim-cangjie5dict 包，
;; 然後根據它的 [[https://github.com/erstern/pyim-cangjie5dict][README]] 來配置。
;; pyim 支持其它版本的倉頡，但需要用戶自己創建詞庫文件。

;; 用戶可以使用命令：`pyim-search-word-code' 來查詢當前選擇詞條的倉頡編碼

;; *** 讓選詞框跟隨光標
;; 用戶可以通過下麵的設置讓 pyim 在 *光標處* 顯示一個選詞框：

;; 1. 使用 popup 包來繪制選詞框 （emacs overlay 機制）
;;    #+BEGIN_EXAMPLE
;;    (setq pyim-page-tooltip 'popup)
;;    #+END_EXAMPLE
;; 2. 使用 posframe 來繪制選詞框
;;    #+BEGIN_EXAMPLE
;;    (setq pyim-page-tooltip 'posframe)
;;    #+END_EXAMPLE
;;    注意：pyim 不會自動安裝 posframe，用戶需要手動安裝這個包，

;; *** 調整 tooltip 選詞框的顯示樣式
;; pyim 的 tooltip 選詞框預設使用 *雙行顯示* 的樣式，在一些特
;; 殊的情況下（比如：popup 顯示的菜單錯位），用戶可以使用 *單行顯示*
;; 的樣式：

;; #+BEGIN_EXAMPLE
;; (setq pyim-page-style 'one-line)
;; #+END_EXAMPLE

;; 註：用戶可以添加函數 pyim-page-style:STYLENAME 來定義自己的選詞框格式。

;; *** 設置模糊音
;; 可以通過設置 `pyim-fuzzy-pinyin-alist' 變數來自定義模糊音。

;; *** 使用魔術轉換器
;; 用戶可以將待選詞條作 “特殊處理” 後再 “上屏”，比如 “簡體轉繁體” 或者
;; “輸入中文，上屏英文” 之類的。

;; 用戶需要設置 `pyim-magic-converter'，比如：下麵這個例子實現，
;; 輸入 “二呆”，“一個超級帥的小夥子” 上屏 :-)
;; #+BEGIN_EXAMPLE
;; (defun my-converter (string)
;;   (if (equal string "二呆")
;;       "“一個超級帥的小夥子”"
;;     string))
;; (setq pyim-magic-converter #'my-converter)
;; #+END_EXAMPLE

;; *** 切換全形標點與半形標點

;; 1. 第一種方法：使用命令 `pyim-punctuation-toggle'，全局切換。
;;    這個命令主要用來設置變數：`pyim-punctuation-translate-p'，用戶也可以
;;    手動設置這個變數，比如：
;;    #+BEGIN_EXAMPLE
;;    (setq pyim-punctuation-translate-p '(yes no auto))   ;使用全形標點。
;;    (setq pyim-punctuation-translate-p '(no yes auto))   ;使用半形標點。
;;    (setq pyim-punctuation-translate-p '(auto yes no))   ;中文使用全形標點，英文使用半形標點。
;;    #+END_EXAMPLE
;; 2. 第二種方法：使用命令 `pyim-punctuation-translate-at-point' 只切換光
;;    標處標點的樣式。
;; 3. 第三種方法：設置變數 `pyim-translate-trigger-char' ，輸入變數設定的
;;    字元會切換光標處標點的樣式。

;; *** 手動加詞和刪詞

;; 1. `pyim-create-Ncchar-word-at-point 這是一組命令，從光標前提取N個漢
;;    字字元組成字元串，並將其加入個人詞庫。
;; 2. `pyim-translate-trigger-char' 以預設設置為例：在“我愛吃紅燒肉”後輸
;;    入“5v” 可以將“愛吃紅燒肉”這個詞條保存到用戶個人詞庫。
;; 3. `pyim-create-word-from-selection'，選擇一個詞條，運行這個命令後，就
;;    可以將這個詞條添加到個人詞庫。
;; 4. `pyim-delete-word' 從個人詞庫中刪除當前高亮選擇的詞條。

;; *** pyim 高級功能
;; 1. 根據環境自動切換到英文輸入模式，使用 pyim-english-input-switch-functions 配置。
;; 2. 根據環境自動切換到半形標點輸入模式，使用 pyim-punctuation-half-width-functions 配置。

;; 注意：上述兩個功能使用不同的變數設置， *千萬不要搞錯* 。

;; **** 根據環境自動切換到英文輸入模式

;; | 探針函數                          | 功能說明                                                                          |
;; |-----------------------------------+-----------------------------------------------------------------------------------|
;; | pyim-probe-program-mode           | 如果當前的 mode 衍生自 prog-mode，那麼僅僅在字元串和 comment 中開啟中文輸入模式   |
;; |-----------------------------------+-----------------------------------------------------------------------------------|
;; | pyim-probe-org-speed-commands     | 解決 org-speed-commands 與 pyim 沖突問題                                          |
;; | pyim-probe-isearch-mode           | 使用 isearch 搜索時，強制開啟英文輸入模式                                         |
;; |                                   | 注意：想要使用這個功能，pyim-isearch-mode 必須激活                                |
;; |-----------------------------------+-----------------------------------------------------------------------------------|
;; | pyim-probe-org-structure-template | 使用 org-structure-template 時，關閉中文輸入模式                                  |
;; |-----------------------------------+-----------------------------------------------------------------------------------|
;; |                                   | 1. 當前字元為中文字元時，輸入下一個字元時預設開啟中文輸入                         |
;; | pyim-probe-dynamic-english        | 2. 當前字元為其他字元時，輸入下一個字元時預設開啟英文輸入                         |
;; |                                   | 3. 使用命令 pyim-convert-string-at-point 可以將光標前的拼音字元串強制轉換為中文。   |
;; |-----------------------------------+-----------------------------------------------------------------------------------|

;; 激活方式：

;; #+BEGIN_EXAMPLE
;; (setq-default pyim-english-input-switch-functions
;;               '(probe-function1 probe-function2 probe-function3))
;; #+END_EXAMPLE

;; 註：上述函數列表中，任意一個函數的返回值為 t 時，pyim 切換到英文輸入模式。

;; **** 根據環境自動切換到半形標點輸入模式

;; | 探針函數                                 | 功能說明                   |
;; |------------------------------------------+----------------------------|
;; | pyim-probe-punctuation-line-beginning    | 行首強制輸入半形標點       |
;; |------------------------------------------+----------------------------|
;; | pyim-probe-punctuation-after-punctuation | 半形標點後強制輸入半形標點 |
;; |------------------------------------------+----------------------------|

;; 激活方式：

;; #+BEGIN_EXAMPLE
;; (setq-default pyim-punctuation-half-width-functions
;;               '(probe-function4 probe-function5 probe-function6))
;; #+END_EXAMPLE

;; 註：上述函數列表中，任意一個函數的返回值為 t 時，pyim 切換到半形標點輸入模式。

;; ** 捐贈
;; 您可以通過小額捐贈的方式支持 pyim 的開發工作，具體方式：

;; 1. 通過支付寶收款賬戶：tumashu@163.com
;; 2. 通過支付寶錢包掃描：

;;    [[file:snapshots/QR-code-for-author.jpg]]


;; ** Tips

;; *** 關閉輸入聯想詞功能 (預設開啟)

;; #+BEGIN_EXAMPLE
;; (setq pyim-enable-shortcode nil)
;; #+END_EXAMPLE

;; *** 如何將個人詞條相關信息導入和導出？

;; 1. 導入使用命令：pyim-import
;; 2. 導出使用命令：pyim-export

;; *** pyim 出現錯誤時，如何開啟 debug 模式

;; #+BEGIN_EXAMPLE
;; (setq debug-on-error t)
;; #+END_EXAMPLE

;; *** 如何查看 pyim 文檔。
;; pyim 的文檔隱藏在 comment 中，如果用戶喜歡閱讀 html 格式的文檔，
;; 可以查看在線文檔；

;;   http://tumashu.github.io/pyim/

;; *** 將光標處的拼音或者五筆字元串轉換為中文 (與 vimim 的 “點石成金” 功能類似)
;; #+BEGIN_EXAMPLE
;; (global-set-key (kbd "M-i") 'pyim-convert-string-at-point)
;; #+END_EXAMPLE

;; *** 如何使用其它字元翻頁
;; #+BEGIN_EXAMPLE
;; (define-key pyim-mode-map "." 'pyim-page-next-page)
;; (define-key pyim-mode-map "," 'pyim-page-previous-page)
;; #+END_EXAMPLE

;; *** 如何用 ";" 來選擇第二個候選詞
;; #+BEGIN_EXAMPLE
;; (define-key pyim-mode-map ";"
;;   (lambda ()
;;     (interactive)
;;     (pyim-page-select-word-by-number 2)))
;; #+END_EXAMPLE

;; *** 如何添加自定義拼音詞庫
;; pyim 預設沒有攜帶任何拼音詞庫，用戶可以使用下麵幾種方式，獲取
;; 質量較好的拼音詞庫：

;; **** 第一種方式 (懶人推薦使用)

;; 獲取其他 pyim 用戶的拼音詞庫，比如，某個同學測試 pyim
;; 時創建了一個中文拼音詞庫，詞條數量大約60萬。

;;    http://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim.gz

;; 下載上述詞庫後，運行 `pyim-dicts-manager' ，按照命令提示，將下載得到的詞庫
;; 文件信息添加到 `pyim-dicts' 中，最後運行命令 `pyim-restart' 或者重啟
;; emacs，這個詞庫使用 `utf-8-unix' 編碼。

;; **** 第二種方式 (Windows 用戶推薦使用)

;; 使用詞庫轉換工具將其他輸入法的詞庫轉化為pyim使用的詞庫：這裡只介紹windows平
;; 臺下的一個詞庫轉換軟體：

;; 1. 軟體名稱： imewlconverter
;; 2. 中文名稱： 深藍詞庫轉換
;; 3. 下載地址： https://github.com/studyzy/imewlconverter
;; 4. 依賴平臺： Microsoft .NET Framework (>= 3.5)

;; 使用方式：

;; [[file:snapshots/imewlconverter-basic.gif]]

;; 如果生成的詞庫詞頻不合理，可以按照下麵的方式處理（非常有用的功能）：

;; [[file:snapshots/imewlconverter-wordfreq.gif]]

;; 生成詞庫後，運行 `pyim-dicts-manager' ，按照命令提示，將轉換得到的詞庫文件的信息添加到 `pyim-dicts' 中，
;; 完成後運行命令 `pyim-restart' 或者重啟emacs。

;; **** 第三種方式 (Linux & Unix 用戶推薦使用)
;; E-Neo 同學編寫了一個詞庫轉換工具: [[https://github.com/E-Neo/scel2pyim][scel2pyim]] ,
;; 可以將一個搜狗詞庫轉換為 pyim 詞庫。

;; 1. 軟體名稱： scel2pyim
;; 2. 下載地址： https://github.com/E-Neo/scel2pyim
;; 3. 編寫語言： C語言

;; *** 如何手動安裝和管理詞庫
;; 這裡假設有兩個詞庫文件：

;; 1. /path/to/pyim-dict1.pyim
;; 2. /path/to/pyim-dict2.pyim

;; 在~/.emacs文件中添加如下一行配置。

;; #+BEGIN_EXAMPLE
;; (setq pyim-dicts
;;       '((:name "dict1" :file "/path/to/pyim-dict1.pyim")
;;         (:name "dict2" :file "/path/to/pyim-dict2.pyim")))
;; #+END_EXAMPLE

;; 注意事項:
;; 1. 只有 :file 是 *必須* 設置的。
;; 2. 必須使用詞庫文件的絕對路徑。
;; 3. 詞庫文件的編碼必須為 utf-8-unix，否則會出現亂碼。

;; *** Emacs 啟動時加載 pyim 詞庫

;; #+BEGIN_EXAMPLE
;; (add-hook 'emacs-startup-hook
;;           #'(lambda () (pyim-restart-1 t)))
;; #+END_EXAMPLE

;; *** 將漢字字元串轉換為拼音字元串
;; 下麵兩個函數可以將中文字元串轉換的拼音字元串或者列表，用於 emacs-lisp
;; 編程。

;; 1. `pyim-hanzi2pinyin' （考慮多音字）
;; 2. `pyim-hanzi2pinyin-simple'  （不考慮多音字）

;; *** 中文分詞
;; pyim 包含了一個簡單的分詞函數：`pyim-cstring-split-to-list'，可以
;; 將一個中文字元串分成一個詞條列表，比如：

;; #+BEGIN_EXAMPLE
;;                   (("天安" 5 7)
;; 我愛北京天安門 ->  ("天安門" 5 8)
;;                    ("北京" 3 5)
;;                    ("我愛" 1 3))
;; #+END_EXAMPLE

;; 其中，每一個詞條列表中包含三個元素，第一個元素為詞條本身，第二個元素為詞條
;; 相對於字元串的起始位置，第三個元素為詞條結束位置。

;; 另一個分詞函數是 `pyim-cstring-split-to-string'，這個函數將生成一個新的字元串，
;; 在這個字元串中，詞語之間用空格或者用戶自定義的分隔符隔開。

;; 注意，上述兩個分詞函數使用暴力匹配模式來分詞，所以， *不能檢測出* pyim
;; 詞庫中不存在的中文詞條。

;; *** 獲取光標處的中文詞條
;; pyim 包含了一個簡單的命令：`pyim-cwords-at-point'，這個命令
;; 可以得到光標處的 *英文* 或者 *中文* 詞條的 *列表*，這個命令依賴分詞函數：
;; `pyim-cstring-split-to-list'。

;; *** 讓 `forward-word' 和 `back-backward’ 在中文環境下正常工作
;; 中文詞語沒有強制用空格分詞，所以 Emacs 內置的命令 `forward-word' 和 `backward-word'
;; 在中文環境不能按用戶預期的樣子執行，而是 forward/backward “句子” ，pyim
;; 自帶的兩個命令可以在中文環境下正常工作：

;; 1. `pyim-forward-word
;; 2. `pyim-backward-word

;; 用戶只需將其綁定到快捷鍵上就可以了，比如：

;; #+BEGIN_EXAMPLE
;; (global-set-key (kbd "M-f") 'pyim-forward-word)
;; (global-set-key (kbd "M-b") 'pyim-backward-word)
;; #+END_EXAMPLE

;; *** 為 isearch 相關命令添加拼音搜索支持
;; pyim 安裝後，可以通過下麵的設置開啟拼音搜索功能：

;; #+BEGIN_EXAMPLE
;; (pyim-isearch-mode 1)
;; #+END_EXAMPLE

;; 注意：這個功能有一些限制，搜索字元串中只能出現 “a-z” 和 “’”，如果有
;; 其他字元（比如 regexp 操作符），則自動關閉拼音搜索功能。

;; 開啟這個功能後，一些 isearch 擴展有可能失效，如果遇到這種問題，
;; 只能禁用這個 Minor-mode，然後聯系 pyim 的維護者，看有沒有法子實現相容。

;; 用戶激活這個 mode 後，可以使用下麵的方式 *強制關閉* isearch 搜索框中文輸入
;; （即使在 pyim 激活的時候）。

;; #+BEGIN_EXAMPLE
;; (setq-default pyim-english-input-switch-functions
;;               '(pyim-probe-isearch-mode))
;; #+END_EXAMPLE

;;; Code:

;; * 核心代碼                                                           :code:
;; ** require + defcustom + defvar
(require 'cl-lib)
(require 'help-mode)
(require 'popup nil t)
(require 'posframe nil t)
(require 'pyim-pymap)
(require 'pyim-common)
(require 'xr) ;Used by pyim-cregexp-build
(require 'rx) ;Used by pyim-cregexp-build

(defgroup pyim nil
  "Pyim is a Chinese input method support quanpin, shuangpin, wubi and cangjie."
  :group 'leim)

(defcustom pyim-dicts nil
  "一個列表，用於保存 `pyim' 的詞庫信息。
每一個 element 都代表一條詞庫的信息, 用戶可以使用詞庫管理命令
`pyim-dicts-manager' 來添加詞庫信息，每一條詞庫信息都使用一個
plist 來表示，比如：

    (:name \"100萬大詞庫\" :file \"/path/to/pinyin-bigdict.pyim\")

其中：
1. `:name'      代表詞庫名稱，用戶可以按照喜好來確定（可選項）。
2. `:file'      表示詞庫文件，

另外一個與這個變數功能類似的變數是：`pyim-extra-dicts', 專門
用於和 elpa 格式的詞庫包集成。"
  :group 'pyim
  :type 'list)

(defcustom pyim-enable-shortcode t
  "啟用輸入聯想詞功能。"
  :group 'pyim
  :type 'boolean)

(defcustom pyim-punctuation-dict
  '(("'" "‘" "’")
    ("\"" "“" "”")
    ("_" "——")
    ("^" "…")
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
  "標點符號表。"
  :group 'pyim
  :type 'list)

(defcustom pyim-default-scheme 'quanpin
  "設置 pyim 使用哪一種輸入法方案，預設使用全拼輸入。"
  :group 'pyim)

(defcustom pyim-assistant-scheme 'quanpin
  "設置輔助輸入法方案。

這個功能主要用於五筆等形碼輸入法，在忘記編碼的情況下，
臨時激活某種輔助輸入法（比如：拼音輸入法）來輸入漢字。"
  :group 'pyim)

(defcustom pyim-schemes
  '((quanpin
     :document "全拼輸入法方案（不可刪除）。"
     :class quanpin
     :first-chars "abcdefghijklmnopqrstuwxyz"
     :rest-chars "vmpfwckzyjqdltxuognbhsrei'-a"
     :prefer-trigger-chars "v")
    (rime
     :document
     "rime 輸入法。

這個 scheme 適用於 librime 支持的所有輸入法，通用性較好，但無法支
持 trigger-chars, 所以類似 pyim 全拼支持的v快捷鍵將無法使用。"
     :class rime
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz'-a"
     :prefer-trigger-chars nil)
    (rime-quanpin
     :document
     "rime 全拼輸入法。

這個 scheme 專門用於 librime 全拼輸入法，同時支持 trigger-chars,
也就是v快捷鍵，使用 rime 全拼的朋友建議使用這個 scheme。"
     :class rime
     :first-chars "abcdefghjklmnopqrstwxyz"
     :rest-chars "vmpfwckzyjqdltxuognbhsrei'-a"
     :prefer-trigger-chars "v")
    (rime-microsoft-shuangpin
     :document "rime 微軟雙拼輸入法。"
     :class rime
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz;"
     :prefer-trigger-chars nil)
    (wubi
     :document "五筆輸入法。"
     :class xingma
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz'"
     :code-prefix "." ;五筆詞庫中所有的 code 都以 "." 開頭，防止和拼音詞庫沖突。
     :code-split-length 4 ;預設將用戶輸入切成 4 個字元長的 code 列表（不計算 code-prefix）
     :code-maximum-length 4 ;五筆詞庫中，code 的最大長度（不計算 code-prefix）
     :prefer-trigger-chars nil)
    (cangjie
     :document "倉頡輸入法。"
     :class xingma
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz"
     :code-prefix "@" ;倉頡輸入法詞庫中所有的 code 都以 "@" 開頭，防止詞庫沖突。
     :code-split-length 5 ;預設將用戶輸入切成 5 個字元長的 code 列表（不計算 code-prefix）
     :code-maximum-length 5 ;倉頡詞庫中，code 的最大長度（不計算 code-prefix）
     :prefer-trigger-chars nil)
    (pyim-shuangpin
     :document "與 pyim 配合良好的雙拼輸入法方案，源自小鶴雙拼方案。"
     :class shuangpin
     :first-chars "abcdefghijklmnpqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz"
     :prefer-trigger-chars "o"
     :keymaps
     (("a" "a" "a")
      ("b" "b" "in")
      ("c" "c" "ao")
      ("d" "d" "ai")
      ("e" "e" "e")
      ("f" "f" "en")
      ("g" "g" "eng")
      ("h" "h" "ang")
      ("i" "ch" "i")
      ("j" "j" "an")
      ("k" "k" "ing" "uai")
      ("l" "l" "iang" "uang")
      ("m" "m" "ian")
      ("n" "n" "iao")
      ("o" "o" "uo" "o")
      ("p" "p" "ie")
      ("q" "q" "iu")
      ("r" "r" "uan")
      ("s" "s" "iong" "ong")
      ("t" "t" "ue" "ve")
      ("u" "sh" "u")
      ("v" "zh" "v" "ui")
      ("w" "w" "ei")
      ("x" "x" "ia" "ua")
      ("y" "y" "un")
      ("z" "z" "ou")
      ("aa" "a")
      ("aj" "an")
      ("ad" "ai")
      ("ac" "ao")
      ("ah" "ang")
      ("ee" "e")
      ("ew" "ei")
      ("ef" "en")
      ("er" "er")
      ("eg" "eng")
      ("ag" "ng")
      ("ao" "o")
      ("au" "ou")))
    (ziranma-shuangpin
     :document "自然碼雙拼方案。"
     :class shuangpin
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz"
     :prefer-trigger-chars nil
     :keymaps
     (("a" "a" "a")
      ("b" "b" "ou")
      ("c" "c" "iao")
      ("d" "d" "uang" "iang")
      ("e" "e" "e")
      ("f" "f" "en")
      ("g" "g" "eng")
      ("h" "h" "ang")
      ("i" "ch" "i")
      ("j" "j" "an")
      ("k" "k" "ao")
      ("l" "l" "ai")
      ("m" "m" "ian")
      ("n" "n" "in")
      ("o" "o" "uo" "o")
      ("p" "p" "un")
      ("q" "q" "iu")
      ("r" "r" "uan" "er")
      ("s" "s" "iong" "ong")
      ("t" "t" "ue" "ve")
      ("u" "sh" "u")
      ("v" "zh" "v" "ui")
      ("w" "w" "ia" "ua")
      ("x" "x" "ie")
      ("y" "y" "uai" "ing")
      ("z" "z" "ei")
      ("aa" "a")
      ("an" "an")
      ("ai" "ai")
      ("ao" "ao")
      ("ah" "ang")
      ("ee" "e")
      ("ei" "ei")
      ("en" "en")
      ("er" "er")
      ("eg" "eng")
      ("oo" "o")
      ("ou" "ou")))
    (microsoft-shuangpin
     :document "微軟雙拼方案。"
     :class shuangpin
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz;"
     :prefer-trigger-chars nil
     :keymaps
     (("a" "a" "a")
      ("b" "b" "ou")
      ("c" "c" "iao")
      ("d" "d" "uang" "iang")
      ("e" "e" "e")
      ("f" "f" "en")
      ("g" "g" "eng")
      ("h" "h" "ang")
      ("i" "ch" "i")
      ("j" "j" "an")
      ("k" "k" "ao")
      ("l" "l" "ai")
      ("m" "m" "ian")
      ("n" "n" "in")
      ("o" "o" "uo" "o")
      ("p" "p" "un")
      ("q" "q" "iu")
      ("r" "r" "uan" "er")
      ("s" "s" "iong" "ong")
      ("t" "t" "ue")
      ("u" "sh" "u")
      ("v" "zh" "ve" "ui")
      ("w" "w" "ia" "ua")
      ("x" "x" "ie")
      ("y" "y" "uai" "v")
      ("z" "z" "ei")
      (";" ";" "ing")
      ("oa" "a")
      ("oj" "an")
      ("ol" "ai")
      ("ok" "ao")
      ("oh" "ang")
      ("oe" "e")
      ("oz" "ei")
      ("of" "en")
      ("or" "er")
      ("og" "eng")
      ("oo" "o")
      ("ob" "ou")))
    (zhinengabc-shuangpin
     :document "智能ABC雙拼方案"
     :class shuangpin
     :first-chars "abcdefghjklmnopqrstvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz"
     :prefer-trigger-chars nil
     :keymaps
     (("q" "q" "ei")
      ("w" "w" "ian")
      ("e" "ch" "e")
      ("r" "r" "iu" "er")
      ("t" "t" "uang" "iang")
      ("y" "y" "ing")
      ("u" "u" "u")
      ("i" "i" "i")
      ("o" "o" "o" "uo")
      ("p" "p" "uan" "van")
      ("a" "zh" "a")
      ("s" "s" "ong" "iong")
      ("d" "d" "ua" "ia")
      ("f" "f" "en")
      ("g" "g" "eng")
      ("h" "h" "ang")
      ("j" "j" "an")
      ("k" "k" "ao")
      ("l" "l" "ai")
      ("z" "z" "iao")
      ("x" "x" "ie")
      ("c" "c" "in" "uai")
      ("v" "sh" "v")
      ("b" "b" "ou")
      ("n" "n" "un")
      ("m" "m" "ue" "ui")
      ("oa" "a")
      ("oj" "an")
      ("ol" "ai")
      ("ok" "ao")
      ("oh" "ang")
      ("oe" "e")
      ("oz" "ei")
      ("of" "en")
      ("or" "er")
      ("og" "eng")
      ("oo" "o")
      ("ob" "ou")))
    (xiaohe-shuangpin
     :document "小鶴雙拼輸入法方案。"
     :class shuangpin
     :first-chars "abcdefghijklmnopqrstuvwxyz"
     :rest-chars "abcdefghijklmnopqrstuvwxyz"
     :prefer-trigger-chars nil
     :keymaps
     (("a" "a" "a")
      ("b" "b" "in")
      ("c" "c" "ao")
      ("d" "d" "ai")
      ("e" "e" "e")
      ("f" "f" "en")
      ("g" "g" "eng")
      ("h" "h" "ang")
      ("i" "ch" "i")
      ("j" "j" "an")
      ("k" "k" "ing" "uai")
      ("l" "l" "iang" "uang")
      ("m" "m" "ian")
      ("n" "n" "iao")
      ("o" "o" "uo" "o")
      ("p" "p" "ie")
      ("q" "q" "iu")
      ("r" "r" "uan")
      ("s" "s" "iong" "ong")
      ("t" "t" "ue" "ve")
      ("u" "sh" "u")
      ("v" "zh" "v" "ui")
      ("w" "w" "ei")
      ("x" "x" "ia" "ua")
      ("y" "y" "un")
      ("z" "z" "ou")
      ("aa" "a")
      ("an" "an")
      ("ai" "ai")
      ("ao" "ao")
      ("ah" "ang")
      ("ee" "e")
      ("ei" "ei")
      ("en" "en")
      ("er" "er")
      ("eg" "eng")
      ("og" "ng")
      ("oo" "o")
      ("ou" "ou"))))
  "Pyim 支持的所有拼音方案。")

(defcustom pyim-translate-trigger-char "v"
  "用於觸發特殊操作的字元，相當與單字快捷鍵。

輸入中文的時候，我們需要快速頻繁的執行一些特定的命令，最直接的方
法就是將上述命令綁定到一個容易按的快捷鍵上，但遺憾的是 emacs 大多
數容易按的快捷鍵都 *名花有主* 了，甚至找一個 “Ctrl＋單字元”的快
捷鍵都不太容易，特殊功能觸發字元，可以幫助我們實現“單字元”快捷
鍵，類似 org-mode 的 speed key。

預設情況下，我們可以使用特殊功能觸發字元執行下麵幾個操作（假設觸
發字元為 v）：

1. 快速切換中英文標點符號的樣式：當光標前的字元是一個標點符號時，
   按 \"v\" 可以切換這個標點的樣式。比如：光標在A處的時候，按
   \"v\" 可以將A前面的全形逗號轉換為半形逗號。

        你好，-A-

   按 \"v\" 後

        你好,-A-

2. 快速將光標前的詞條添加到詞庫：當光標前的字元是中文字元時，按
   \"num\" + \"v\" 可以將光標前 num 個中文漢字組成的詞條添加到個
   人詞頻文件中，比如：當光標在A處時，按\"4v\"可以將“的紅燒肉”
   這個詞條加入個人詞頻文件，預設num不超過9。

        我愛吃美味的紅燒肉-A-

值得注意的是，這種方式如果添加的功能太多，會造成許多潛在的沖突。

用戶可以使用變數 `pyim-translate-trigger-char' 來設置觸發字元，默
認的觸發字元是：\"v\"，選擇這個字元的理由基於全拼輸入法的：

1. \"v\" 不是有效的聲母，不會對中文輸入造成太大的影響。
2. \"v\" 字元很容易按。

pyim 使用函數 `pyim-translate' 來處理特殊功能觸發字元。當待輸入的
字元是觸發字元時，`pyim-translate' 根據光標前的字元的不同來調用不
同的功能，具體見 `pyim-translate' ：

單字快捷鍵受到輸入法方案的限制，比如：全拼輸入法可以將其設置為v,
但雙拼輸入法下設置 v 可能就不行，所以，pyim 首先會檢查當前輸入法
方案下，這個快捷鍵設置是否合理有效，如果不是一個合理的設置，則使
用拼音方案預設的 :prefer-trigger-chars 。

具體請參考 `pyim-translate-get-trigger-char' 。"
  :group 'pyim
  :type 'character)

(defcustom pyim-exhibit-delay-ms 0
  "輸入或者刪除拼音字元後等待多少毫秒後才顯示可選詞
當用戶快速輸入連續的拼音時可提升用戶體驗。
如果為 0 或者 nil, 則不等待立刻顯示可選詞。"
  :group 'pyim
  :type 'integer)

(defcustom pyim-fuzzy-pinyin-alist
  '(("en" "eng")
    ("in" "ing")
    ("un" "ong"))
  "設定糢糊音。"
  :group 'pyim)

(defface pyim-preview-face '((t (:underline t)))
  "設置光標處預覽字元串的 face。"
  :group 'pyim)

(defcustom pyim-english-input-switch-functions nil
  "讓 pyim 開啟英文輸入功能。

這個變數的取值為一個函數列表，這個函數列表中的任意一個函數的
運行結果為 t 時，pyim 開啟英文輸入功能。"
  :group 'pyim)

(defcustom pyim-punctuation-half-width-functions nil
  "讓 pyim 輸入半形標點。

取值為一個函數列表，這個函數列表中的任意一個函數的運行結果為 t 時，
pyim 輸入半形標點，函數列表中每個函數都有一個參數：char ，表示
最後輸入的一個字元，具體見: `pyim-translate' 。"
  :group 'pyim)

(defcustom pyim-wash-function 'pyim-wash-current-line-function
  "清洗光標前面的文字內容。
這個函數與『單字快捷鍵配合使用』，當光標前面的字元為漢字字元時，
按 `pyim-translate-trigger-char' 對應字元，可以調用這個函數來清洗
光標前面的文字內容。"
  :group 'pyim
  :type 'function)

(defcustom pyim-page-length 5
  "每頁顯示的詞條數目。

細節信息請參考 `pyim-page-refresh' 的 docstring。"
  :group 'pyim
  :type 'number)

(defcustom pyim-page-tooltip 'popup
  "如何繪制 pyim 選詞框。

1. 當這個變數取值為 posframe 時，使用 posframe 包來繪制選詞框；
2. 當這個變數取值為 minibuffer 時，使用 minibuffer 做為選詞框；
3. 當這個變數取值為 popup 時，使用 popup-el 包來繪制選詞框；"
  :group 'pyim)

(defcustom pyim-page-style 'two-lines
  "這個變數用來控制選詞框的格式。

pyim 內建的有三種選詞框格式：

1. one-line  單行選詞框
2. two-lines 雙行選詞框
3. vertical  垂直選詞框"
  :group 'pyim
  :type 'symbol)

(defcustom pyim-page-select-finish-hook nil
  "Pyim 選詞完成時運行的 hook。"
  :group 'pyim
  :type 'hook)

(defcustom pyim-page-select-word-by-number t
  "使用數字鍵來選擇詞條。

如果設置為 nil, 將直接輸入數字，適用於使用數字做為
編碼的輸入法。"
  :group 'pyim)

(defcustom pyim-magic-converter nil
  "將 “待選詞條” 在 “上屏” 之前自動轉換為其他字元串。
這個功能可以實現“簡轉繁”，“輸入中文得到英文”之類的功能。"
  :group 'pyim)

(defcustom pyim-posframe-border-width 0
  "posframe的內間距。
只有當用戶使用 posframe 來顯示候選詞時才有效。"
  :group 'pyim
  :type 'integer)

(defcustom pyim-autoselector '(pyim-autoselector-xingma)
  "已經啟用的自動上屏器。

自動上屏器是一個函數，如果這個函數返回t, 那麼當前的詞條就會自動
上屏，不需要手動選擇。"
  :group 'pyim)

(defcustom pyim-posframe-min-width (* pyim-page-length 7)
  "使用 posframe 做為選詞框時，設置選詞框的最小寬度。"
  :group 'pyim
  :type 'integer)

(defface pyim-page
  '((t (:inherit default :background "#333333" :foreground "#dcdccc")))
  "Face used for the pyim page."
  :group 'pyim)

(defface pyim-page-border
  '((t (:inherit pyim-page)))
  "Face used for the pyim page border.
Only useful when use posframe."
  :group 'pyim)

(defface pyim-page-selection
  '((t (:background "gray44")))
  "選詞框中已選詞條的 face

注意：當使用 minibuffer 為選詞框時，這個選項才有用處。"
  :group 'pyim)

(defvar pyim-dcache-backend 'pyim-dhashcache
  "詞庫後端引擎，負責緩沖詞庫並提供搜索詞的演算法。
可選項為 `pyim-dhashcache' 或 `pyim-dregcache'。
前者搜索單詞速度很快，消耗記憶體多。後者搜索單詞速度較快，消耗記憶體少。

`pyim-dregcache' 速度和詞庫大小成正比。當詞庫接近 100M 大小時，
在六年歷史的筆記型電腦上會有一秒的延遲，這時建議換用 `pyim-dhashcache'。")

;;;###autoload
(defvar pyim-title "靈通" "Pyim 在 mode-line 中顯示的名稱。")
(defvar pyim-extra-dicts nil "與 `pyim-dicts' 類似，用於和 elpa 格式的詞庫包集成。")

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

(defvar pyim-entered-buffer " *pyim-entered-buffer*"
  "一個 buffer，用來處理用戶已經輸入的字元串：entered。

用戶 *已經* 輸入的字元組成的字元串，在 pyim 裡面，叫做 entered,
說白了就是 input，選擇 entered 而不選擇 input 的原因是：

1. input 太常見了，和其它詞語組和起來容易產生歧義，比如：
   pyim-entered-output 就比 pyim-input-output 更加容易理解。
2. entered 這個詞語很少見，只要明白它代表的概念，就不容易產生混亂。

pyim 使用一個 buffer 來處理 entered，以實現 “用戶輸入字元串” 編
輯等高級功能：

1. 如果輸入的字元串有錯誤，可以修改，不用取消重新輸入；
2. 如果光標不在行首，pyim 只使用光標前的字元串來查找詞條，
   如果詞條上屏，詞條對應的輸入就從 buffer 中清除，然後
   繼續處理後面的輸入，這種方式方便長詞的輸入；
3. 如果光標在行首，則處理整行。")

(defvar pyim-imobjs nil
  "Imobj (Input method object) 組成的 list。

imobj 在 pyim 裡面的概念，類似與編譯器裡面的語法樹，
它代表 pyim 輸入的字元串 entered 解析得到的一個結構化對象，
以全拼輸入法的為例：

1. entered: nihaoma
2. imobj: ((\"n\" \"i\" \"n\" \"i\") (\"h\" \"ao\" \"h\" \"ao\") (\"m\" \"a\" \"m\" \"a\"))

而 imobjs 是 imobj 組成的一個列表，因為有糢糊音等概念的存在，一個
entered 需要以多種方式或者多步驟解析，得到多種可能的 imobj，這些
imobj 組合構成在一起，構成了 imobjs 這個概念。比如：

1. entered: guafeng (設置了糢糊音 en -> eng)
2. imobj-1: ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"en\" \"f\" \"eng\"))
3. imobj-2: ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"eng\" \"f\" \"eng\"))
4. imobjs:  (((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"en\" \"f\" \"eng\"))
             ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"eng\" \"f\" \"eng\")))

這個變數用來保存解析得到的 imobjs。

解析完成之後，pyim 會為每一個 imobj 創建對應 code 字元串，然後在詞庫
中搜索 code 字元串來得到所需要的詞條，最後使用特定的方式將得到的
詞條組合成一個候選詞列表：`pyim-candidates' 並通過 pyim-page 相關
功能來顯示選詞框，供用戶選擇詞條，比如：

1. imobj: ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"en\" \"f\" \"en\"))
2. code: gua-fen

從上面的說明可以看出，imobj 本身也是有結構的：

1. imobj: ((\"g\" \"ua\" \"g\" \"ua\") (\"f\" \"en\" \"f\" \"en\"))

我們將 (\"g\" \"ua\" \"g\" \"ua\") 這些子結構，叫做 imelem (IM element)，*大
多數情況下*，一個 imelem 能夠代表一個漢字，這個概念在編輯 entered
的時候，非常有用。

另外要注意的是，不同的輸入法，imelem 的內部結構是不一樣的，比如：
1. quanping:  (\"g\" \"ua\" \"g\" \"ua\")
2. shuangpin: (\"h\" \"ao\" \"h\" \"c\")
3. wubi:      (\"aaaa\")")

(defvar pyim-candidates nil
  "所有備選詞條組成的列表。")

(defvar pyim-preview-overlay nil
  "用於保存光標處預覽字元串的 overlay。")

(defvar pyim-outcome-history nil
  "記錄 pyim outcome 的變化的歷史

在 pyim 中 outcome 代表用戶通過輸入法選擇，並最終插入到 buffer
的字元串。

“一次確認就生成的詞條”，當前變數一般只有一個元素，比如：
1. 輸入：nihao
2. 輸出：你好
2. 變數取值為：(\"你好\")

“多次確認才能生成詞條”，當前變數記錄了選擇的歷史，比如：

1. 輸入：yiersansi
2. 輸出：一二三四
3. 第一次選擇：一二
4. 第二次選擇：三
5. 第三次選擇：四
6. 變數取值為：(\"一二三四\" \"一二三\" \"一二\")")

(defvar pyim-assistant-scheme-enable nil
  "設置臨時 scheme，用於五筆等形碼輸入法臨時拼音輸入。")

(defvar pyim-input-ascii nil
  "是否開啟 pyim 英文輸入模式。")

(defvar pyim-force-input-chinese nil
  "是否強制開啟中文輸入模式。

這個變數只用於 `pyim-convert-string-at-point'，不要
在其它地方使用。")

(defvar pyim-candidate-position 1
  "當前選擇的詞條在 ‘pyim-candidates’ 中的位置。

細節信息請參考 `pyim-page-refresh' 的 docstring。")

(defvar pyim-last-created-word nil
  "記錄最近一次創建的詞條，用於實現快捷刪詞功能：`pyim-delete-last-word'。")

(defvar pyim-translating nil
  "記錄是否在轉換狀態。")

(defvar pyim-magic-convert-cache nil
  "用來臨時保存 `pyim-magic-convert' 的結果。
從而加快同一個字元串第二次的轉換速度。")

(defvar pyim-load-hook nil)
(defvar pyim-active-hook nil)

(defvar pyim-punctuation-translate-p '(auto yes no)
  "這個變數的第一個元素的取值用於控制標點符號全形半形模式切換。

1. 當第一個元素為 'yes 時，輸入全形標點。
2. 當第一個元素為 'no 時，輸入半形標點。
3. 當第一個元素為 'auto 時，根據中英文環境，自動切換。")

(defvar pyim-punctuation-pair-status
  '(("\"" nil) ("'" nil))
  "成對標點符號切換狀態。")

(defvar pyim-punctuation-escape-list (number-sequence ?0 ?9)
  "Punctuation will not insert after this characters.

If you don't like this function, set the variable to nil")

(defvar pyim-pinyin2cchar-cache1 nil
  "拼音查漢字功能需要的變數。")
(defvar pyim-pinyin2cchar-cache2 nil
  "拼音查漢字功能需要的變數。")
(defvar pyim-pinyin2cchar-cache3 nil
  "拼音查漢字功能需要的變數。")
(defvar pyim-cchar2pinyin-cache nil
  "漢字轉拼音功能需要的變數。")

(defvar pyim-dcache-auto-update t
  "是否自動創建和更新詞庫對應的 dcache 文件。

這個變數預設設置為 t，如果有詞庫文件添加到 `pyim-dicts' 或者
`pyim-extra-dicts' 時，pyim 會自動生成相關的 dcache 文件。

一般不建議將這個變數設置為 nil，除非有以下情況：

1. 用戶的詞庫已經非常穩定，並且想通過禁用這個功能來降低
pyim 對資源的消耗。
2. 自動更新功能無法正常工作，用戶通過手工從其他機器上拷貝
dcache 文件的方法讓 pyim 正常工作。")

(defvar pyim-page-tooltip-posframe-buffer " *pyim-page-tooltip-posframe-buffer*"
  "這個變數用來保存做為 page tooltip 的 posframe 的 buffer。")

(defconst pyim-shuangpin-invalid-pinyin-regexp
  "^\\([qtghkzcsdn]o\\|[ypfbmw]uo\\|[qj]ong\\|[rtysdghklzcn]iong\\|[qtypdjlxbnm]uai\\|[ghk]ing?\\|[qjlxn]uang\\|[dgh]iang\\|[qjlx]ua\\|[hkg]ia\\|[rtsdghkzc]v\\|[jl]ui\\)$"
  "雙拼可能自動產生的無效拼音，例如輸入 kk 得到有效拼音 kuai，
但同時產生了無效拼音 king。用戶手動輸入的無效拼音無需考慮，
因為用戶有即時界面反饋，不可能連續輸入無效拼音。")

(defvar pyim-rime-limit 50
  "當 pyim 使用 `liberime-search' 來獲取詞條時，這個變數用來限制
`liberime-search' 返回詞條的數量。")

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
      (define-key map (char-to-string i) 'pyim-page-select-word-by-number))
    (define-key map " " 'pyim-page-select-word)
    (define-key map (kbd "C-SPC") 'pyim-page-select-word-simple)
    (define-key map [backspace] 'pyim-entered-delete-backward-char)
    (define-key map [delete] 'pyim-entered-delete-forward-char)
    (define-key map "\C-d" 'pyim-entered-delete-forward-char)
    (define-key map [M-backspace] 'pyim-entered-delete-backward-imelem)
    (define-key map [M-delete] 'pyim-entered-delete-forward-imelem)
    (define-key map [C-backspace] 'pyim-entered-delete-backward-imelem)
    (define-key map [C-delete] 'pyim-entered-delete-forward-imelem)
    (define-key map [?\t]      'pyim-toggle-assistant-scheme)
    (define-key map (kbd "TAB") 'pyim-toggle-assistant-scheme)
    (define-key map "\177" 'pyim-entered-delete-backward-char)
    (define-key map "\C-f" 'pyim-entered-forward-point)
    (define-key map "\C-b" 'pyim-entered-backward-point)
    (define-key map "\M-f" 'pyim-entered-forward-imelem)
    (define-key map "\M-b" 'pyim-entered-backward-imelem)
    (define-key map "\C-e" 'pyim-entered-end-of-line)
    (define-key map "\C-a" 'pyim-entered-beginning-of-line)
    (define-key map "=" 'pyim-page-next-page)
    (define-key map "-" 'pyim-page-previous-page)
    (define-key map "\C-n" 'pyim-page-next-word)
    (define-key map "\C-p" 'pyim-page-previous-word)
    (define-key map "\M-n" 'pyim-page-next-page)
    (define-key map "\M-p" 'pyim-page-previous-page)
    (define-key map "\C-m" 'pyim-quit-no-clear)
    (define-key map [return] 'pyim-quit-no-clear)
    (define-key map "\C-c" 'pyim-quit-clear)
    map)
  "Pyim 的 Keymap。")

;; ** 將變數轉換為 local 變數
(defvar pyim-local-variable-list
  '(pyim-imobjs
    pyim-outcome-history
    pyim-preview-overlay
    pyim-candidates
    pyim-candidate-position
    pyim-input-ascii
    pyim-english-input-switch-functions
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

(defun pyim-entered-get ()
  "從 `pyim-entered-buffer' 中獲取拼音字元串"
  (pyim-with-entered-buffer
    (buffer-string)))

(defun pyim-entered-erase-buffer ()
  "清除 `pyim-entered-buffer' 的內容"
  (pyim-with-entered-buffer
    (erase-buffer)))

;; pyim-entered-buffer 中進行光標移動的函數
;; point move function in `pyim-entered-buffer'
(defun pyim-entered-forward-point ()
  "`pyim-entered-buffer' 中光標前移"
  (interactive)
  (pyim-with-entered-buffer
    (ignore-errors
      (forward-char)))
  (pyim-entered-refresh t))

(defun pyim-entered-backward-point ()
  "`pyim-entered-buffer' 中光標後移"
  (interactive)
  (pyim-with-entered-buffer
    (ignore-errors
      (backward-char)))
  (pyim-entered-refresh t))

(defun pyim-entered-backward-imelem (&optional search-forward)
  "`pyim-entered-buffer’ 中光標向後移動一個 imelem 對應的字元串

在全拼輸入法中，就是向前移動一個拼音"
  (interactive)
  (let* ((position (pyim-entered-next-imelem-position 1 search-forward)))
    (pyim-with-entered-buffer
      (goto-char position))
    (pyim-entered-refresh t)))

(defun pyim-entered-forward-imelem ()
  "`pyim-entered-buffer’ 中光標向前移動一個 imelem 對應的字元串"
  (interactive)
  (pyim-entered-backward-imelem t))

(defun pyim-entered-end-of-line ()
  "`pyim-entered-buffer' 中光標移至行尾"
  (interactive)
  (pyim-with-entered-buffer
    (end-of-line))
  (pyim-entered-refresh t))

(defun pyim-entered-beginning-of-line ()
  "`pyim-entered-buffer' 中光標移至行首"
  (interactive)
  (pyim-with-entered-buffer
    (beginning-of-line))
  (pyim-entered-refresh t))

;; ** "漢字 -> 拼音" 以及 "拼音 -> 漢字" 的轉換函數
(defun pyim-pinyin2cchar-cache-create (&optional force)
  "構建 pinyin 到 chinese char 的緩存。

用於加快搜索速度，這個函數將緩存保存到 `pyim-pinyin2cchar-cache' 變數中，
如果 FORCE 設置為 t，強制更新索引。"
  (when (or force (or (not pyim-pinyin2cchar-cache1)
                      (not pyim-pinyin2cchar-cache2)))
    (setq pyim-pinyin2cchar-cache1
          (make-hash-table :size 50000 :test #'equal))
    (setq pyim-pinyin2cchar-cache2
          (make-hash-table :size 50000 :test #'equal))
    (setq pyim-pinyin2cchar-cache3
          (make-hash-table :size 50000 :test #'equal))
    (dolist (x pyim-pymap)
      (let* ((py (car x))
             (cchars (cdr x))
             (n (min (length py) 7)))
        (puthash py cchars pyim-pinyin2cchar-cache1)
        (puthash py (cdr (split-string (car cchars) ""))
                 pyim-pinyin2cchar-cache2)
        (dotimes (i n)
          (let* ((key (substring py 0 (+ i 1)))
                 (orig-value (gethash key pyim-pinyin2cchar-cache3)))
            (puthash key (delete-dups `(,@orig-value ,@cchars))
                     pyim-pinyin2cchar-cache3)))))))

(defun pyim-pinyin2cchar-get (pinyin &optional equal-match return-list include-seperator)
  "獲取拼音與 PINYIN 想匹配的所有漢字。

比如：

“man” -> (\"忙茫盲芒氓莽蟒邙漭硭\" \"滿慢漫曼蠻饅瞞蔓顢謾墁幔蟎鞔鰻縵熳鏝\")

如果 EQUAL-MATCH 是 non-nil，獲取和 PINYIN 完全匹配的漢字。
如果 RETURN-LIST 是 non-nil，返回一個由單個漢字字元串組成的列表。

(\"滿\" \"慢\" \"漫\"  ...)

如果 INCLUDE-SEPERATOR 是 non-nil，返回的列表包含一個 ‘|’ 號，pyim 用這個分隔符
來區分 3500 個常用漢字和生僻字。"
  (pyim-pinyin2cchar-cache-create)
  (when (and pinyin (stringp pinyin))
    (let ((output
           (if equal-match
               (if return-list
                   (gethash pinyin pyim-pinyin2cchar-cache2)
                 (gethash pinyin pyim-pinyin2cchar-cache1))
             (gethash pinyin pyim-pinyin2cchar-cache3))))
      (if include-seperator
          output
        (delq ""
              (mapcar (lambda (x)
                        (replace-regexp-in-string "|" "" x))
                      (or output '())))))))

(defun pyim-cchar2pinyin-get (char-or-str)
  "獲取字元或者字元串 CHAR-OR-STR 對應的拼音 code。

pyim 在特定的時候需要讀取一個漢字的拼音，這個工作由此完成，函數
從 `pyim-cchar2pinyin-cache' 查詢得到一個漢字字元的拼音，例如：

(pyim-cchar2pinyin-get ?我)

結果為:

(\"wo\")"
  (pyim-cchar2pinyin-cache-create)
  (let ((key (if (characterp char-or-str)
                 (char-to-string char-or-str)
               char-or-str)))
    (when (= (length key) 1)
      (gethash key pyim-cchar2pinyin-cache))))

(defun pyim-cchar2pinyin-cache-create (&optional force)
  "Build pinyin cchar->pinyin hashtable from `pyim-pymap'.

If FORCE is non-nil, FORCE build."
  (when (or force (not pyim-cchar2pinyin-cache))
    (setq pyim-cchar2pinyin-cache
          (make-hash-table :size 50000 :test #'equal))
    (dolist (x pyim-pymap)
      (let ((py (car x))
            (cchar-list (string-to-list (car (cdr x)))))
        (dolist (cchar cchar-list)
          (let* ((key (char-to-string cchar))
                 (cache (gethash key pyim-cchar2pinyin-cache)))
            (if cache
                (puthash key (append (list py) cache) pyim-cchar2pinyin-cache)
              (puthash key (list py) pyim-cchar2pinyin-cache))))))))

;; ** 註冊 Pyim 輸入法
;;;###autoload
(register-input-method "pyim" "euc-cn" 'pyim-start pyim-title)

;;;###autoload
(defun pyim-start (name &optional active-func restart save-personal-dcache refresh-common-dcache)
  "pyim 啟動函數。
  TODO: Document NAME ACTIVE-FUNC RESTART SAVE-PERSONAL-DCACHE REFRESH-COMMON-DCACHE

pyim 是使用 `pyim-start' 來啟動輸入法，這個命令主要做如下工作：
1. 重置 `pyim-local-variable-list' 中所有的 local 變數。
2. 使用 `pyim-cchar2pinyin-create-cache' 創建漢字到拼音的 hash table 對應表。
3. 運行hook：`pyim-load-hook'。
4. 將 `pyim-dcache-save-caches' 命令添加到 `kill-emacs-hook'，emacs 關閉
之前將用戶選擇過的詞生成的緩存和詞頻緩存保存到文件，供以後使用。
5. 設定變數：
1. `input-method-function'
2. `deactivate-current-input-method-function'
6. 運行 `pyim-active-hook'

pyim 使用函數 `pyim-start' 啟動輸入法的時候，會將變數
`input-method-function' 設置為 `pyim-input-method' ，這個變數會影
響 `read-event' 的行為。

當輸入字元時，`read-event' 會被調用，`read-event' 調用的過程中，
會執行 `pyim-input-method' 這個函數。`pyim-input-method' 又調用函
數 `pyim-start-translation'。"
  (interactive)
  (mapc 'kill-local-variable pyim-local-variable-list)
  (mapc 'make-local-variable pyim-local-variable-list)
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
    ;; 這個命令 *當前* 主要用於五筆輸入法。
    (pyim-dcache-call-api 'update-shortcode2word restart))

  (unless (member 'pyim-dcache-save-caches kill-emacs-hook)
    (add-to-list 'kill-emacs-hook 'pyim-dcache-save-caches))
  (setq input-method-function 'pyim-input-method)
  (setq deactivate-current-input-method-function 'pyim-inactivate)
  ;; (setq describe-current-input-method-function 'pyim-help)
  ;; If we are in minibuffer, turn off the current input method
  ;; before exiting.
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'pyim-exit-from-minibuffer))
  (run-hooks 'pyim-active-hook)
  (when (and (memq pyim-page-tooltip '(posframe child-frame))
             (not (pyim-posframe-valid-p)))
    (message "PYIM: posframe 沒有正確安裝或者當前 emacs 版本不支持 posframe。"))
  (when restart
    (message "pyim 重啟完成。"))
  nil)

(defun pyim-exit-from-minibuffer ()
  "Pyim 從 minibuffer 退出。"
  (deactivate-input-method)
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer)))

(defun pyim-restart ()
  "重啟 pyim，不建議用於編程環境。

這個函數用於重啟 pyim，其過程和 `pyim-start' 類似，只是在輸入法重
啟之前，詢問用戶，是否保存個人詞頻信息。"
  (interactive
   (let ((save-personal-dcache
          (yes-or-no-p "重啟 pyim 前，需要保存個人詞頻信息嗎？ "))
         (refresh-common-dcache
          (yes-or-no-p "需要強制刷新詞庫緩存嗎？ ")))
     (pyim-restart-1 save-personal-dcache refresh-common-dcache))))

(defun pyim-restart-1 (&optional save-personal-dcache refresh-common-dcache)
  "重啟 pyim，用於編程環境。

當 SAVE-PERSONAL-DCACHE 是 non-nil 時，保存個人詞庫文件。
當 REFRESH-COMMON-DCACHE 是 non-nil 時，強制刷新詞庫緩存。"
  (pyim-start "pyim" nil t
              save-personal-dcache refresh-common-dcache))

(defun pyim-create-dicts-md5 (dict-files)
  (let* ((version "v1") ;當需要強制更新 dict 緩存時，更改這個字元串。
         (dicts-md5 (md5 (prin1-to-string
                          (mapcar #'(lambda (file)
                                      (list version file (nth 5 (file-attributes file 'string))))
                                  dict-files)))))
    dicts-md5))

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
        (message "%S 不是一個有效的 dcache api 函數。" (symbol-name func))
        ;; Need to return nil
        nil))))

(defun pyim-dcache-update-code2word (&optional force)
  "讀取並加載詞庫。

讀取 `pyim-dicts' 和 `pyim-extra-dicts' 裡面的詞庫文件，生成對應的
詞庫緩沖文件，然後加載詞庫緩存。

如果 FORCE 為真，強制加載。"
  (let* ((dict-files (mapcar #'(lambda (x)
                                 (unless (plist-get x :disable)
                                   (plist-get x :file)))
                             `(,@pyim-dicts ,@pyim-extra-dicts)))
         (dicts-md5 (pyim-create-dicts-md5 dict-files)))
    (pyim-dcache-call-api 'update-code2word dict-files dicts-md5 force)))

(defun pyim-dcache-init-variables ()
  "初始化 dcache 緩存相關變數。"
  (pyim-dcache-call-api 'init-variables))

(defun pyim-dcache-save-caches ()
  "保存 dcache。

  將用戶選擇過的詞生成的緩存和詞頻緩存的取值
  保存到它們對應的文件中。

  這個函數預設作為 `kill-emacs-hook' 使用。"
  (interactive)
  (pyim-dcache-call-api 'save-personal-dcache-to-file)
  t)

(defun pyim-export (file &optional confirm)
  "將個人詞條以及詞條對應的詞頻信息導出到文件 FILE。

  如果 FILE 為 nil，提示用戶指定導出文件位置，如果 CONFIRM 為 non-nil，
  文件存在時將會提示用戶是否覆蓋，預設為覆蓋模式"
  (interactive "F將詞條相關信息導出到文件: ")
  (with-temp-buffer
    (insert ";;; -*- coding: utf-8-unix -*-\n")
    (pyim-dcache-call-api 'insert-export-content)
    (pyim-dcache-write-file file confirm)))

(defun pyim-export-personal-words (file &optional confirm)
  "將用戶選擇過的詞生成的緩存導出為 pyim 詞庫文件。

如果 FILE 為 nil，提示用戶指定導出文件位置，如果 CONFIRM 為 non-nil，
文件存在時將會提示用戶是否覆蓋，預設為覆蓋模式。

註：這個函數的用途是製作 pyim 詞庫，個人詞條導入導出建議使用：
`pyim-import' 和 `pyim-export'。"
  (interactive "F將個人緩存中的詞條導出到文件：")
  (pyim-dcache-call-api 'export-personal-words file confirm))

(defun pyim-import (file &optional merge-method)
  "從 FILE 中導入詞條以及詞條對應的詞頻信息。

MERGE-METHOD 是一個函數，這個函數需要兩個數字參數，代表
詞條在詞頻緩存中的詞頻和待導入文件中的詞頻，函數返回值做為合並後的詞頻使用，
預設方式是：取兩個詞頻的最大值。"
  (interactive "F導入詞條相關信息文件: ")
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
  ;; 保存一下用戶選擇過的詞生成的緩存和詞頻緩存，
  ;; 因為使用 async 機制更新 dcache 時，需要從 dcache 文件
  ;; 中讀取變數值，然後再對用戶選擇過的詞生成的緩存排序，如果沒
  ;; 有這一步驟，導入的詞條就會被覆蓋，使用 emacs-thread 機制來更新 dcache
  ;; 不存在此問題。
  (unless pyim-prefer-emacs-thread
    (pyim-dcache-save-caches))
  ;; 更新相關的 dcache
  (pyim-dcache-call-api 'update-personal-words t)

  (message "pyim: 詞條相關信息導入完成！"))

;; ** 從詞庫中搜索中文詞條
(defun pyim-dcache-get (code &optional from)
  "從 FROM 對應的 dcache 中搜索 CODE，得到對應的詞條。

當詞庫文件加載完成後，pyim 就可以用這個函數從詞庫緩存中搜索某個
code 對應的中文詞條了。"
  (pyim-dcache-call-api 'get code from))

(defun pyim-pinyin-build-regexp (pinyin &optional match-beginning first-equal all-equal)
  "從 PINYIN 構建一個 regexp，用於搜索聯想詞，
比如：ni-hao-si-j --> ^ni-hao[a-z]*-si[a-z]*-j[a-z]* , when FIRST-EQUAL set to `t'
                  --> ^ni[a-z]*-hao[a-z]*-si[a-z]*-j[a-z]* , when FIRST-EQUAL set to `nil'"
  (when (and pinyin (stringp pinyin))
    (let ((pinyin-list (split-string pinyin "-"))
          (count 0))
      (concat (if match-beginning "^" "")
              (mapconcat
               #'(lambda (x)
                   (setq count (+ count 1))
                   (if (or (not first-equal) (> count 1))
                       (if all-equal
                           x
                         (concat x "[a-z]*"))
                     x))
               pinyin-list "-")))))

(defun pyim-insert-word-into-icode2word (word pinyin prepend)
  (pyim-dcache-call-api 'insert-word-into-icode2word word pinyin prepend))

(defun pyim-create-word (word &optional prepend wordcount-handler)
  "將中文詞條 WORD 添加編碼後，保存到用戶選擇過的詞生成的緩存中。

詞條 WORD 預設會追加到已有詞條的後面，如果 PREPEND 設置為 t,
詞條就會放到已有詞條的最前面。

根據當前輸入法，決定是調用 `pyim-hanzi2pinyin' 還是
`pyim-hanzi2xingma' 來獲取中文詞條的編碼。

WORDCOUNT-HANDLER 可以是一個數字，代表將此數字設置為 WORD 的新詞頻，
WORDCOUNT-HANDLER 也可以是一個函數，其返回值將設置為 WORD 的新詞頻，
而這個函數的參數則表示 WORD 當前詞頻，這個功能用於：`pyim-import',
如果 WORDCOUNT-HANDLER 設置為其他，則表示讓 WORD 當前詞頻加 1。

BUG：拼音無法有效地處理多音字。
"
  (when (and (> (length word) 0)
             (< (length word) 11) ;十個漢字以上的詞條，加到個人詞庫裡面用處不大，忽略。
             (not (pyim-string-match-p "\\CC" word)))
    ;; 記錄最近創建的詞條，用於快速刪詞功能。
    (setq pyim-last-created-word word)
    (let* ((scheme-name (pyim-scheme-name))
           (class (pyim-scheme-get-option scheme-name :class))
           (code-prefix (pyim-scheme-get-option scheme-name :code-prefix))
           (codes (if (eq class 'xingma)
                      (pyim-hanzi2xingma word scheme-name t)
                    ;;拼音使用了多音字校正
                    (pyim-hanzi2pinyin word nil "-" t nil t))))
      ;; 保存對應詞條的詞頻
      (when (> (length word) 0)
        (pyim-dcache-call-api
         'update-iword2count
         word
         prepend
         wordcount-handler))
      ;; 添加詞條到個人緩存
      (dolist (code codes)
        (unless (pyim-string-match-p "[^ a-z-]" code)
          (pyim-insert-word-into-icode2word word
                                            (concat (or code-prefix "") code)
                                            prepend)))
      ;; TODO，排序個人詞庫？
      )))

(defun pyim-hanzi2xingma (string scheme-name &optional return-list)
  "返回漢字 STRING 對應形碼方案 SCHEME-NAME 的 code (不包括
code-prefix)。當RETURN-LIST 設置為 t 時，返回一個 code list。"
  (let* ((fun (intern (concat "pyim-hanzi2xingma:" (symbol-name scheme-name))))
         (code (and fun (funcall fun string))))
    (when code
      (if return-list
          (list code)
        code))))

(defun pyim-hanzi2xingma:wubi (string)
  "返回漢字 STRING 的五筆編碼(不包括 code-prefix)。當RETURN-LIST
設置為 t 時，返回一個編碼列表。"
  (when (string-match-p "^\\cc+\\'" string)
    (let ((code (pyim-code-search string 'wubi))
          (len (length string)))
      (when (string-empty-p code)
        (when (= len 1)
          (error "No code found for %s" string))
        (setq string (split-string string "" t)
              code
              (cl-case len
                ;; 雙字詞，分別取兩個字的前兩個編碼
                (2 (concat (substring (pyim-hanzi2xingma:wubi (nth 0 string)) 0 2)
                           (substring (pyim-hanzi2xingma:wubi (nth 1 string)) 0 2)))
                ;; 三字詞，取前二字的首編碼，及第三個字的前兩個編碼
                (3 (concat (substring (pyim-hanzi2xingma:wubi (nth 0 string)) 0 1)
                           (substring (pyim-hanzi2xingma:wubi (nth 1 string)) 0 1)
                           (substring (pyim-hanzi2xingma:wubi (nth 2 string)) 0 2)))
                ;; 四字詞及以上，分別前三個字及最後一個字的首編碼
                (t (concat (substring (pyim-hanzi2xingma:wubi (nth 0 string)) 0 1)
                           (substring (pyim-hanzi2xingma:wubi (nth 1 string)) 0 1)
                           (substring (pyim-hanzi2xingma:wubi (nth 2 string)) 0 1)
                           (substring (pyim-hanzi2xingma:wubi (nth (1- len) string)) 0 1))))))
      code)))

(defun pyim-list-merge (a b)
  "Join list A and B to a new list, then delete dups."
  (let ((a (if (listp a)
               a
             (list a)))
        (b (if (listp b)
               b
             (list b))))
    (delete-dups `(,@a ,@b))))

(defun pyim-cstring-at-point (&optional number)
  "獲取光標一個中文字元串，字元數量為：NUMBER。"
  (save-excursion
    (let* ((point (point))
           (begin (- point number))
           (begin (if (> begin 0)
                      begin
                    (point-min)))
           (string (buffer-substring-no-properties
                    point begin)))
      (when (and (stringp string)
                 (= (length string) number)
                 (not (pyim-string-match-p "\\CC" string)))
        string))))

(defun pyim-create-word-at-point (&optional number silent)
  "將光標前字元數為 NUMBER 的中文字元串添加到個人詞庫中
當 SILENT 設置為 t 是，不顯示提醒信息。"
  (let* ((string (pyim-cstring-at-point (or number 2))))
    (when string
      (pyim-create-word string)
      (unless silent
        (message "將詞條: \"%s\" 加入 personal 緩沖。" string)))))

(defun pyim-create-2cchar-word-at-point ()
  "將光標前2個中文字元組成的字元串加入個人詞庫。"
  (interactive)
  (pyim-create-word-at-point 2))

(defun pyim-create-3cchar-word-at-point ()
  "將光標前3個中文字元組成的字元串加入個人詞庫。"
  (interactive)
  (pyim-create-word-at-point 3))

(defun pyim-create-4cchar-word-at-point ()
  "將光標前4個中文字元組成的字元串加入個人詞庫。"
  (interactive)
  (pyim-create-word-at-point 4))

;; ** 刪詞功能
(defun pyim-create-word-from-selection ()
  "Add the selected text as a Chinese word into the personal dictionary."
  (interactive)
  (when (region-active-p)
    (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
      (if (> (length string) 6)
          (error "詞條太長")
        (if (not (string-match-p "^\\cc+\\'" string))
            (error "不是純中文字元串")
          (pyim-create-word string)
          (message "將詞條: %S 插入 personal file。" string))))))

(defun pyim-search-word-code ()
  "選擇詞條，然後反查它的 code. 這個功能對五筆用戶有用。"
  (interactive)
  (when (region-active-p)
    (let* ((string (buffer-substring-no-properties (region-beginning) (region-end)))
           code)
      (if (not (string-match-p "^\\cc+\\'" string))
          (error "不是純中文字元串")
        (setq code (pyim-dcache-call-api 'search-word-code string))
        (if code
            (message "%S -> %S " string code)
          (message "沒有找到 %S 對應的編碼。" string))))))

(defun pyim-delete-words-in-file (file)
  "從個人詞庫緩存中批量刪除 FILE 文件中列出的詞條。

FILE 的格式與 `pyim-export' 生成的文件格式相同，
另外這個命令也可以識別沒有詞頻的行，比如：

   ;;; -*- coding: utf-8-unix -*-
   詞條1
   詞條2"
  (interactive "F記錄待刪詞條的文件: ")
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
  (message "pyim: 批量刪詞完成！"))

(defun pyim-delete-last-word ()
  "從個人詞庫中刪除最新創建的詞條。"
  (interactive)
  (when pyim-last-created-word
    (pyim-delete-word-1 pyim-last-created-word)
    (message "pyim: 從個人詞庫中刪除詞條 “%s” !" pyim-last-created-word)))

(defun pyim-delete-word-at-point (&optional number silent)
  "將光標前字元數為 NUMBER 的中文字元串從個人詞庫中刪除
當 SILENT 設置為 t 是，不顯示提醒信息。"
  (let* ((string (pyim-cstring-at-point (or number 2))))
    (when string
      (pyim-delete-word-1 string)
      (unless silent
        (message "詞條: \"%s\" 已經從個人詞庫緩沖中刪除。" string)))))

(defun pyim-delete-word ()
  "將高亮選擇的詞條從個人詞庫中刪除。"
  (interactive)
  (if mark-active
      (let ((string (buffer-substring-no-properties
                     (region-beginning) (region-end))))
        (when (and (< (length string) 6)
                   (> (length string) 0))
          (pyim-delete-word-1 string)
          (message "將詞條: %S 從 personal 緩沖中刪除。" string)))
    (message "請首先高亮選擇需要刪除的詞條。")))

(defun pyim-delete-word-1 (word)
  "將中文詞條 WORD 從個人詞庫中刪除"
  (pyim-dcache-call-api 'delete-word word))

;; ** 處理用戶輸入字元的相關函數
(defun pyim-input-method (key)
  "得到需要插入到 buffer 的字元串，並將其插入到待輸入 buffer。

這個函數會處理用戶輸入的字元，並最終的得到需要插入 buffer 的字元
串。這個字元串會被分解為 event list，通過 emacs 低層函數
`read-event' 來將這些 list 插入到 *待輸入buffer*。"
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
                (mapcar 'identity input-string))))
        (pyim-preview-delete-overlay)
        (pyim-entered-erase-buffer)))))

(defun pyim-magic-convert (str)
  "用於處理 `pyim-magic-convert' 的函數。"
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

`pyim-start-translation' 這個函數較複雜，作許多低層工作，但它的一
個重要流程是：

1. 使用函數 `read-key-sequence' 得到 key-sequence
2. 使用函數 `lookup-key' 查詢 `pyim-mode-map' 中，與上述 key-sequence 對應
   的命令。
3. 如果查詢得到的命令是 `pyim-self-insert-command' 時，
   `pyim-start-translation' 會調用這個函數。
4. 這個函數最終會返回需要插入到 buffer 的字元串。

這個部份的代碼涉及許多 emacs 低層函數，相對復雜，不容易理解，有興
趣的朋友可以參考：
1. `quail-input-method' 相關函數。
2. elisp 手冊相關章節:
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
             (generated-events nil)
             (input-method-function nil)
             ;; Quail package 用這個變數來控制是否在 buffer 中
             ;; 插入 preview string，pyim *強制* 將其設置為 nil
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
                    (error (message "pyim 出現錯誤: %S，開啟 debug-on-error 後可以瞭解詳細情況。" err)
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
  "判斷是否 *根據環境自動切換* 為英文輸入模式，這個函數處理變數：
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
               (concat pyim-title
                       (if pyim-input-ascii
                           "-英文"
                         "-AU英"))))))

(defun pyim-input-chinese-p ()
  "確定 pyim 是否需要啟動中文輸入模式。"
  (let* ((scheme-name (pyim-scheme-name))
         (first-chars (pyim-scheme-get-option scheme-name :first-chars))
         (rest-chars (pyim-scheme-get-option scheme-name :rest-chars)))
    (and (or pyim-force-input-chinese
             (and (not pyim-input-ascii)
                  (not (pyim-auto-switch-english-input-p))))
         (if (not (string< "" (pyim-entered-get)))
             (member last-command-event
                     (mapcar 'identity first-chars))
           (member last-command-event
                   (mapcar 'identity rest-chars)))
         (setq current-input-method-title pyim-title))))

(defun pyim-autoselector-xingma (&rest args)
  "適用於型碼輸入法的自動上屏器。

比如：五筆等型碼輸入法，重碼率很低，90%以上的情況都是選擇第一個詞
條，自動選擇可以減少按空格強制選詞的機會。"
  (let* ((scheme-name (pyim-scheme-name))
         (class (pyim-scheme-get-option scheme-name :class))
         (n (pyim-scheme-get-option scheme-name :code-split-length)))
    (and (eq class 'xingma)
         (= (length (pyim-entered-get)) n))))

(defun pyim-self-insert-command ()
  "Pyim 版本的 self-insert-command。"
  (interactive "*")
  (cond
   ;; 自動上屏器設置：自動上屏器是一個函數，如果這個函數返回t，就自
   ;; 動上屏。
   ((cl-some #'(lambda (x)
                 (when (functionp x)
                   (funcall x)))
             pyim-autoselector)
    (push last-command-event unread-command-events)
    (unless (equal pyim-candidates (list (pyim-entered-get)))
      (pyim-outcome-handle 'candidate))
    (pyim-terminate-translation))
   ((pyim-input-chinese-p)
    (pyim-with-entered-buffer
      (insert (char-to-string last-command-event)))
    (pyim-entered-refresh))
   (pyim-candidates
    (pyim-outcome-handle 'candidate-and-last-char)
    (pyim-terminate-translation))
   (t
    (pyim-outcome-handle 'last-char)
    (pyim-terminate-translation))))

(defun pyim-entered-refresh-1 ()
  "查詢 `pyim-entered-buffer' 光標前的拼音字元串 (如果光標在行首則為光標後的)，顯示備選詞等待用戶選擇。"
  (let* ((scheme-name (pyim-scheme-name))
         pinyin-to-translate)
    (pyim-with-entered-buffer
      (setq pinyin-to-translate
            (if (equal 1 (point))
                (buffer-string)
              (buffer-substring-no-properties 1 (point)))))
    (setq pyim-imobjs (pyim-imobjs-create pinyin-to-translate scheme-name))
    (setq pyim-candidates
          (or (delete-dups (pyim-candidates-create pyim-imobjs scheme-name))
              (list pinyin-to-translate)))
    (setq pyim-candidate-position 1)
    (pyim-preview-refresh)
    (pyim-page-refresh)))

(defun pyim-entered-refresh (&optional no-delay)
  "延遲 `pyim-exhibit-delay-ms' 顯示備選詞等待用戶選擇。"
  (if (= (length (pyim-entered-get)) 0)
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
  (setq pyim-assistant-scheme-enable nil)
  (setq pyim-force-input-chinese nil)
  (when (and (memq pyim-page-tooltip '(posframe child-frame))
             (pyim-posframe-valid-p))
    (posframe-hide pyim-page-tooltip-posframe-buffer))
  (pyim-entered-erase-buffer))

;; 分解拼音的相關函數
(defun pyim-pinyin-get-shenmu (pinyin)
  "從一個拼音字元串 PINYIN 中提出第一個聲母。"
  (let ((i (min (length pinyin) 2))
        shenmu)
    (while (> i 0)
      (setq shenmu (substring pinyin 0 i))
      (if (member shenmu pyim-pinyin-shenmu)
          (setq i 0)
        (setq i (1- i))
        (setq shenmu "")))
    (cons shenmu
          (substring pinyin (length shenmu)))))

(defun pyim-pinyin-get-charpy (pinyin)
  "將拼音字元串 PINYIN 分解成聲母，韻母和剩餘部分。"
  (let* ((x (pyim-pinyin-get-shenmu pinyin))
         (shenmu (car x))
         (yunmu-and-rest (cdr x))
         (i (min (length yunmu-and-rest) 5))
         yunmu rest result)
    (cl-flet ((pinyin-valid-p
               (shenmu yunmu)
               (cl-some
                #'(lambda (char-pinyin)
                    (pyim-pinyin2cchar-get char-pinyin t))
                (mapcar #'(lambda (x)
                            (concat (nth 0 x) (nth 1 x)))
                        (pyim-imobjs-find-fuzzy:quanpin-1
                         (list shenmu yunmu shenmu yunmu))))))
      (while (> i 0)
        (setq yunmu (substring yunmu-and-rest 0 i))
        (setq rest (substring yunmu-and-rest i))
        (if (member yunmu pyim-pinyin-yunmu)
            (cond (;; 如果聲母和韻母組成的拼音不是一個有效的拼音，
                   ;; 就繼續縮短，如果是，就進一步檢測。
                   (not (pinyin-valid-p shenmu yunmu))
                   (setq i (1- i))
                   (setq yunmu ""))
                  ((and (string< "" rest)
                        ;; 截取後剩餘的字元串 rest 找不出聲母
                        (equal (car (pyim-pinyin-get-shenmu rest)) "")
                        ;; 截取後的韻母最後一個字元是一個有效聲母
                        (member (substring yunmu -1) pyim-pinyin-shenmu)
                        ;; 截取得到的韻母如果去掉最後一個字元，還是有效的韻母
                        (member (substring yunmu 0 -1) pyim-pinyin-yunmu))
                   (if (not (pinyin-valid-p shenmu (substring yunmu 0 -1)))
                       ;; 如果去掉韻母最後一個字元後，無法組成一個有效的拼音。
                       ;; 就不要縮短了。
                       (setq i 0)
                     (setq i (1- i))
                     (setq yunmu "")))
                  (t (setq i 0)))
          (setq i (1- i))
          (setq yunmu ""))))
    (cons (list shenmu yunmu shenmu yunmu)
          (substring yunmu-and-rest (length yunmu)))))

(defun pyim-pinyin-split (pinyin)
  "將一個代表拼音的字元串 PINYIN，分解為聲母韻母對組成的列表。

這個過程通過循環的調用 `pyim-pinyin-get-charpy' 來實現，整個過程
類似用菜刀切黃瓜片，將一個拼音字元串逐漸切開。"
  (let ((py pinyin)
        charpy spinyin)
    (while (when (string< "" pinyin)
             (setq charpy (pyim-pinyin-get-charpy pinyin))
             (if (and (equal (nth 0 (car charpy)) "")
                      (equal (nth 1 (car charpy)) ""))
                 (progn
                   (setq spinyin nil)
                   (setq pinyin ""))
               (setq spinyin (append spinyin (list (car charpy))))
               (setq pinyin (cdr charpy)))))
    (or spinyin
        ;; 如果無法按照拼音的規則來分解字元串，
        ;; 就將字元串簡單的包裝一下，然後返回。
        ;; 目前這個功能用於：以 u 或者 i 開頭的詞庫 #226
        ;; https://github.com/tumashu/pyim/issues/226
        (list (list "" py "" py)))))

(defun pyim-scheme-get (scheme-name)
  "獲取名稱為 SCHEME-NAME 的輸入法方案。"
  (assoc scheme-name pyim-schemes))

(defun pyim-scheme-name (&optional default)
  "獲取輸入法 scheme"
  (let (scheme-name)
    (if (and pyim-assistant-scheme-enable
             (not default))
        (setq scheme-name
              (or pyim-assistant-scheme
                  pyim-default-scheme))
      (setq scheme-name pyim-default-scheme))
    (if (assq scheme-name pyim-schemes)
        scheme-name
      (message "Pyim: invalid scheme, fallback to quanpin scheme.")
      'quanpin)))

(defun pyim-toggle-assistant-scheme ()
  "臨時切換到輔助輸入法。

這個功能一般用於五筆等形碼輸入法，在忘記編碼的時候臨時用拼音輸入
中文。"
  (interactive)
  (if (= (length (pyim-entered-get)) 0)
      (progn
        (pyim-outcome-handle 'last-char)
        (pyim-terminate-translation))
    (setq pyim-assistant-scheme-enable
          (not pyim-assistant-scheme-enable))
    (pyim-entered-refresh)))

(defun pyim-scheme-get-option (scheme-name option)
  "獲取名稱為 SCHEME-NAME 的輸入法方案，並提取其屬性 OPTION 。"
  (let ((scheme (pyim-scheme-get scheme-name)))
    (when scheme
      (plist-get (cdr scheme) option))))

(defun pyim-imobjs-create (entered &optional scheme-name)
  "按照 SCHEME-NAME 對應的輸入法方案，從 ENTERED 字元串中創建一個
或者多個 imobj 組成的列表，不同的輸入法，imobj 的結構也是不一樣的。"
  (let ((class (pyim-scheme-get-option scheme-name :class)))
    (when class
      (funcall (intern (format "pyim-imobjs-create:%S" class))
               entered scheme-name))))

(defun pyim-imobjs-create:quanpin (entered &optional -)
  "從用戶輸入的字元串 ENTERED 創建一個輸入法內部對象列表: imobjs。

這個 imobjs 可能包含一個 imobj，也可能包含多個，每個 imobj 都包含
聲母和韻母的相關信息，比如：

    (pyim-imobjs-create:quanpin \"woaimeinv\" 'quanpin)

結果為:

    (((\"w\" \"o\" \"w\" \"o\") (\"\" \"ai\" \"\" \"ai\") (\"m\" \"ei\" \"m\" \"ei\") (\"n\" \"v\" \"n\" \"v\")))

如果字元串無法正確處理，則返回 nil，比如：

   (pyim-imobjs-create \"ua\" 'quanpin)

全拼輸入法的 imelem 是四個字元串組成的 list，類似：

  (\"w\" \"o\" \"w\" \"o\")

代表：

  (聲母1, 韻母1, 聲母2, 韻母2)

聲母1和聲母2一般用來生成 code 字元串，用於詞庫中尋找詞條。聲母2和
韻母2一般用來反向構建 entered 字元串，用於“多次選擇生成詞條”這個
功能。

大多數情況，聲母1 = 聲母2，韻母1 = 韻母2，只有在使用糢糊音的時候，
才可能出現不一致的情況。"
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
              ;; 將強制分割符號附加到封分割符後面的聲母開頭，
              ;; 類似：("'n" "i" "n" "i")，用於 `pyim-page-preview-create' 函數。
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
            (unless (string-match-p pyim-shuangpin-invalid-pinyin-regexp
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

(defun pyim-imobjs-create:rime (entered &optional -)
  (list (list entered)))

(defun pyim-imobjs-find-fuzzy:quanpin (imobjs)
  "用於處理模糊音的函數。"
  (let (fuzzy-imobjs result1 result2)
    (dolist (imobj imobjs)
      (setq fuzzy-imobjs
            (pyim-permutate-list
             (mapcar 'pyim-imobjs-find-fuzzy:quanpin-1 imobj)))
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

(defun pyim-entered-next-imelem-position (num &optional search-forward start)
  "從 `pyim-entered-buffer' 的當前位置，找到下一個或者下 NUM 個 imelem 對應的位置

如果 SEARCH-FORWARD 為 t，則向前搜索，反之，向後搜索。"
  (pyim-with-entered-buffer
    (let* ((scheme-name (pyim-scheme-name))
           (start (or start (point)))
           (end-position start)
           (string (buffer-substring-no-properties (point-min) start))
           (orig-imobj-len (length (car (pyim-imobjs-create string scheme-name))))
           imobj)
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

(defun pyim-codes-create (imobj scheme-name &optional first-n)
  "按照 SCHEME-NAME 對應的輸入法方案，從一個 IMOBJ 創建一個列表 codes，這個列表
包含一個或者多個 code 字元串，這些 code 字元串用於從詞庫中搜索詞條。"
  (let ((class (pyim-scheme-get-option scheme-name :class)))
    (when class
      (funcall (intern (format "pyim-codes-create:%S" class))
               imobj scheme-name first-n))))

(defun pyim-codes-create:quanpin (imobj scheme-name &optional first-n)
  "從IMOBJ 創建一個 code 列表：codes。

列表 codes 中包含一個或者多個 code 字元串，這些 code 字元串用於從
詞庫中搜索相關詞條。

    (pyim-codes-create '((\"w\" \"o\" \"w\" \"o\") (\"\" \"ai\" \"\" \"ai\") (\"m\" \"ei\" \"m\" \"ei\") (\"n\"  \"v\" \"n\"  \"v\")) 'quanpin)

結果為:

   (\"wo\" \"ai\" \"mei\" \"nv\")"
  (mapcar
   #'(lambda (w)
       (let ((py (replace-regexp-in-string ;去掉分隔符，在詞庫中搜索候選詞不需要分隔符
                  "'" "" (concat (nth 0 w) (nth 1 w)))))
         (if (numberp first-n)
             (substring py 0 (min first-n (length py)))
           py)))
   imobj))

(defun pyim-codes-create:shuangpin (imobj scheme-name &optional first-n)
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

(defun pyim-codes-create:rime (imobj scheme-name &optional _first-n)
  (when scheme-name
    imobj))

(defun pyim-code-search (word scheme-name)
  "從 SCHEME-NAME 對應的輸入法詞庫中，搜索 WORD 對應的 code。

返回最長的 code。"
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

;; ** 獲取備選詞列表
(defun pyim-candidates-create (imobjs scheme-name)
  "按照 SCHEME-NAME 對應的輸入法方案，從輸入法內部對象列表:
IMOBJS 獲得候選詞條。"
  (when imobjs
    (let ((class (pyim-scheme-get-option scheme-name :class)))
      (when class
        (funcall (intern (format "pyim-candidates-create:%S" class))
                 imobjs scheme-name)))))

(defun pyim-candidates-create:xingma (imobjs scheme-name)
  "`pyim-candidates-create' 處理五筆倉頡等形碼輸入法的函數。"
  (let (result)
    (dolist (imobj imobjs)
      (let* ((codes (reverse (pyim-codes-create imobj scheme-name)))
             (output1 (car codes))
             (output2 (reverse (cdr codes)))
             output3 str)

        (when output2
          (setq str (mapconcat
                     #'(lambda (code)
                         (car (pyim-dcache-get code)))
                     output2 "")))
        (setq output3
              (remove "" (or (mapcar #'(lambda (x)
                                         (concat str x))
                                     (pyim-dcache-get output1 '(code2word shortcode2word icode2word)))
                             (list str))))
        (setq result (append result output3))))
    (when (car result)
      result)))

(defun pyim-candidates-create:rime (imobjs scheme-name)
  "`pyim-candidates-create' 處理 rime 輸入法的函數。"
  (when (functionp 'liberime-clear-composition)
    (liberime-clear-composition)
    (let ((s (replace-regexp-in-string
              "-" "" (car (pyim-codes-create (car imobjs) scheme-name)))))
      (dolist (key (string-to-list s))
        (liberime-process-key key))
      (let* ((context (liberime-get-context))
             (menu (alist-get 'menu context))
             (candidates (alist-get 'candidates menu)))
        candidates))))

(defun pyim-candidates-create:quanpin (imobjs scheme-name)
  "`pyim-candidates-create' 處理全拼輸入法的函數。"
  (let* (;; 如果輸入 "ni-hao" ，搜索 code 為 "n-h" 的詞條做為聯想詞。
         ;; 搜索首字母得到的聯想詞太多，這裡限制聯想詞要大於兩個漢字並且只搜索
         ;; 個人文件。
         (jianpin-words
          (when (and (> (length (car imobjs)) 1) pyim-enable-shortcode)
            (pyim-dcache-get
             (mapconcat #'identity
                        (pyim-codes-create (car imobjs) scheme-name 1)
                        "-")
             '(ishortcode2word))))
         znabc-words
         pinyin-chars
         personal-words
         common-words)

    ;; 智能ABC模式，得到盡可能的拼音組合，查詢這些組合，得到的詞條做
    ;; 為聯想詞。
    (let* ((codes (pyim-codes-create (car imobjs) scheme-name))
           (n (- (length codes) 1))
           output)
      (dotimes (i (- n 1))
        (let ((lst (cl-subseq codes 0 (- n i))))
          (push (mapconcat #'identity lst "-") output)))
      (dolist (code (reverse output))
        (setq znabc-words (append znabc-words (pyim-dcache-get code)))))

    (dolist (imobj imobjs)
      (setq personal-words
            (append personal-words
                    (pyim-dcache-get
                     (mapconcat #'identity
                                (pyim-codes-create imobj scheme-name)
                                "-")
                     (if pyim-enable-shortcode
                         '(icode2word ishortcode2word)
                       '(icode2word)))))

      (setq common-words (delete-dups common-words))
      (setq common-words
            (let* ((cands (pyim-dcache-get
                           (mapconcat #'identity
                                      (pyim-codes-create imobj scheme-name)
                                      "-")
                           (if pyim-enable-shortcode
                               '(code2word shortcode2word)
                             '(code2word)))))
              (cond
               ((and (> (length cands) 0)
                     (> (length common-words) 0)
                     (or (eq 1 (length imobj))
                         (eq 2 (length imobj))))
                ;; 兩個單字或者兩字詞序列合並,確保常用字詞在前面
                (let* ((size (min (length cands) (length common-words)))
                       new-common-words
                       (i 0))
                  ;; 兩個序列輪流取出一個元素輸入新序列
                  (while (< i size)
                    (push (nth i common-words) new-common-words)
                    (push (nth i cands) new-common-words)
                    (setq i (1+ i)))
                  ;; 較長序列的剩餘元素加入新序列
                  (append (nreverse new-common-words)
                          (nthcdr size (cond
                                        ((< size (length cands))
                                         cands)
                                        ((< size (length common-words))
                                         common-words))))))
               (t
                (append common-words cands)))))

      (setq pinyin-chars
            (append pinyin-chars
                    (pyim-dcache-get
                     (car (pyim-codes-create imobj scheme-name))))))

    ;; 使用詞頻信息，對個人詞庫得到的候選詞排序，
    ;; 第一個詞的位置比較特殊，不參與排序，
    ;; 具體原因請參考 `pyim-page-select-word' 中的 comment。
    (setq personal-words
          `(,(car personal-words)
            ,@(pyim-dcache-call-api
               'sort-words (cdr personal-words))))

    ;; Debug
    (when pyim-debug
      (princ (list :imobjs imobjs
                   :personal-words personal-words
                   :common-words common-words
                   :jianpin-words jianpin-words
                   :znabc-words znabc-words
                   :pinyin-chars pinyin-chars)))

    (delete-dups
     (delq nil
           `(,@personal-words
             ,@common-words
             ,@jianpin-words
             ,@znabc-words
             ,@pinyin-chars)))))

(defun pyim-candidates-create:shuangpin (imobjs scheme-name)
  "`pyim-candidates-create' 處理雙拼輸入法的函數。"
  (pyim-candidates-create:quanpin imobjs 'quanpin))

;; ** 待輸入字元串預覽
(defun pyim-preview-setup-overlay ()
  "設置 pyim 光標處實時預覽功能所需要的 overlay。

這個函數會在 `pyim-input-method' 中調用，用於創建 overlay ，並將
其保存到 `pyim-preview-overlay' 變數，overlay 的 face 屬性設置為
`pyim-preview-face' ，用戶可以使用這個變數來自定義 face"
  (let ((pos (point)))
    (if (overlayp pyim-preview-overlay)
        (move-overlay pyim-preview-overlay pos pos)
      (setq pyim-preview-overlay (make-overlay pos pos))
      (if input-method-highlight-flag
          (overlay-put pyim-preview-overlay 'face 'pyim-preview-face)))))

(defun pyim-preview-delete-overlay ()
  "刪除 pyim 光標處實時預覽功能所需要的 overlay。

這個函數會在 `pyim-input-method' 中調用，用於刪除
`pyim-preview-overlay' 中保存的 overlay。"
  (if (and (overlayp pyim-preview-overlay) (overlay-start pyim-preview-overlay))
      (delete-overlay pyim-preview-overlay)))

(defun pyim-preview-refresh ()
  "刷新光標處預覽。

pyim 會使用 emacs overlay 機制在 *待輸入buffer* 光標處高亮顯示一
個預覽字元串，讓用戶可以查看將要輸入的字元串，這個函數用於更新這
個字元串的內容。"
  (let* ((class (pyim-scheme-get-option (pyim-scheme-name) :class))
         (end (pyim-page-end))
         (start (1- (pyim-page-start)))
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
  "刪除已經插入 buffer 的 preview 預覽字元串。"
  (if (overlay-start pyim-preview-overlay)
      (delete-region (overlay-start pyim-preview-overlay)
                     (overlay-end pyim-preview-overlay))))

;; ** 選詞框相關函數
(defun pyim-candidate-parse (candidate)
  (let ((output
         (if (consp candidate)
             (car candidate)
           candidate)))
    (if (stringp output)
        (car (split-string output ":"))
      output)))

(defun pyim-page-current-page ()
  "計算當前選擇的詞條在第幾頁面。

細節信息請參考 `pyim-page-refresh' 的 docstring。"
  (1+ (/ (1- pyim-candidate-position) pyim-page-length)))

(defun pyim-page-total-page ()
  "計算 page 總共有多少頁。

細節信息請參考 `pyim-page-refresh' 的 docstring。"
  (1+ (/ (1- (length pyim-candidates)) pyim-page-length)))

(defun pyim-page-start ()
  "計算當前所在頁的第一個詞條的位置。

細節信息請參考 `pyim-page-refresh' 的 docstring。"
  (let ((pos (min (length pyim-candidates) pyim-candidate-position)))
    (1+ (* (/ (1- pos) pyim-page-length) pyim-page-length))))

(defun pyim-page-end (&optional finish)
  "計算當前所在頁的最後一個詞條的位置，

如果 pyim-candidates 用完，則檢查是否有補全。如果 FINISH 為
non-nil，說明，補全已經用完了。

細節信息請參考 `pyim-page-refresh' 的 docstring。"
  (let* ((whole (length pyim-candidates))
         (len pyim-page-length)
         (pos pyim-candidate-position)
         (last (* (/ (+ (1- pos) len) len) len)))
    (if (< last whole)
        last
      (if finish
          whole
        (pyim-page-end t)))))

(defun pyim-page-refresh (&optional hightlight-current)
  "刷新 page 頁面的函數。

這個函數主要用來處理選詞框選詞後，刷新顯示問題，pyim 使用
`pyim-candidates' 來保存 *待選詞列表* ，\"nihao\" 對應的
`pyim-candidates' 的值類似：

     (\"你好\" \"倪皓\" \"泥\" \"你\"  ...  \"慝\")

*待選詞列表* 一般都很長，不可能在一頁中完全顯示，所以 pyim 使用了
page 的概念，比如，上面的 “nihao” 的 *待選詞列表* 就可以邏輯的
分成5頁：


  第1頁   你好  倪皓  泥  你  呢  擬  逆  膩  妮
  第2頁   怩    溺    尼  禰  齯  麑  鯢  蜺  衵
  第3頁   薿    旎    睨  鈮  昵  匿  倪  霓  暱
  第4頁   柅    猊    郳  輗  坭  惄  堄  儗  伲
  第5頁   禰    慝

`pyim-candidate-position' 的取值以及 `pyim-page-length' 的設定值
（預設設置為 5），共同決定了 pyim 需要顯示哪一頁，比如：

  第1頁  你好  倪皓   泥    你  呢  擬  逆  膩  妮
  第2頁  怩    溺     尼    禰  齯  麑  鯢  蜺  衵
  第3頁  薿[B] 旎     睨[A] 鈮  昵  匿  倪  霓  暱[E]
  第4頁  柅    猊     郳    輗  坭  惄  堄  儗  伲
  第5頁  禰    慝

假設當前選擇的詞條為 \"睨\"，那麼 `pyim-candidate-position' 的取
值為 A 所在的位置。那麼：

1. 函數 `pyim-page-current-page' 返回值為 3，說明當前 page 為第 3 頁。
2. 函數 `pyim-page-total-page' 返回值為 5，說明 page 共有 5 頁。
3. 函數 `pyim-page-start' 返回 B 所在的位置。
4. 函數 `pyim-page-end' 返回 E 所在的位置。
5. 函數 `pyim-page-refresh' 會從 `pyim-candidates' 中提取一個 sublist:

     (\"薿\" \"旎\" \"睨\" \"鈮\" \"昵\" \"匿\" \"倪\" \"霓\" \"暱\")

   這個 sublist 的起點為 `pyim-page-start' 的返回值，終點為
   `pyim-page-end' 的返回值。並保存到一個 hashtable 的
   :candidates 關鍵字對應的位置，這個 hastable 最終會做為參數傳遞
   給 `pyim-page-style' 相關的函數，用於生成用於在選詞框中顯示的
   字元串。"
  (let* ((end (pyim-page-end))
         (start (1- (pyim-page-start)))
         (candidates pyim-candidates)
         (candidate-showed
          (mapcar #'(lambda (x)
                      (if (stringp x)
                          (replace-regexp-in-string ":" "" x)
                        x))
                  (cl-subseq candidates start end)))
         (pos (- (min pyim-candidate-position (length candidates)) start))
         (page-info (make-hash-table)))
    (puthash :current-page (pyim-page-current-page) page-info)
    (puthash :total-page (pyim-page-total-page) page-info)
    (puthash :candidates candidate-showed page-info)
    (puthash :position pos page-info)
    ;; Show page.
    (when (and (null unread-command-events)
               (null unread-post-input-method-events))
      (if (eq (selected-window) (minibuffer-window))
          ;; 在 minibuffer 中輸入中文時，使用當前輸入的
          ;; 下一行來顯示候選詞。
          (pyim-minibuffer-message
           (concat "\n" (pyim-page-style:minibuffer page-info)))
        ;; 在普通 buffer 中輸入中文時，使用 `pyim-page-tooltip'
        ;; 指定的方式來顯示候選詞。
        (let ((message-log-max nil))
          (cond
           ((equal (buffer-name) " *temp*")
            ;; when exwm-xim is used, page should be showed
            ;; in minibuffer.
            (message (pyim-page-style:exwm page-info)))
           (pyim-page-tooltip
            (pyim-page-tooltip-show
             (let ((func (intern (format "pyim-page-style:%S" pyim-page-style))))
               (if (functionp func)
                   (funcall func page-info)
                 (pyim-page-style:two-lines page-info)))
             (overlay-start pyim-preview-overlay)))
           (t (message (pyim-page-style:minibuffer page-info)))))))))

(defun pyim-minibuffer-message (string)
  "當在 minibuffer 中使用 pyim 輸入中文時，需要將
minibuffer 原來顯示的信息和 pyim 選詞框整合在一起顯示
這個函數就是作這個工作。"
  (message nil)
  (let ((inhibit-quit t)
        point-1)
    (save-excursion
      (insert string)
      (setq point-1 (point)))
    (sit-for 1000000)
    (delete-region (point) point-1)
    (when quit-flag
      (setq quit-flag nil
            unread-command-events '(7)))))

(defun pyim-page-next-page (arg)
  "Pyim page 翻頁命令。

原理是：改變 `pyim-candidate-position' 的取值，假設一次只翻一頁，
那麼這個函數所做的工作就是：
1. 首先將 `pyim-candidate-position' 增加 `pyim-page-length' ，確
   保其指定的位置在下一頁。
2. 然後將 `pyim-candidate-position' 的值設定為 `pyim-page-start'
   的返回值，確保 `pyim-candidate-position' 的取值為下一頁第一個
   詞條的位置。
3. 最後調用 `pyim-page-refresh' 來重新刷新頁面。"
  (interactive "p")
  (if (= (length (pyim-entered-get)) 0)
      (progn
        (pyim-outcome-handle 'last-char)
        (pyim-terminate-translation))
    (let ((new (+ pyim-candidate-position (* pyim-page-length arg) 1)))
      (setq pyim-candidate-position (if (> new 0) new 1)
            pyim-candidate-position (pyim-page-start))
      (pyim-preview-refresh)
      (pyim-page-refresh))))

(defun pyim-page-previous-page (arg)
  (interactive "p")
  (pyim-page-next-page (- arg)))

(defun pyim-page-next-word (arg)
  (interactive "p")
  (if (= (length (pyim-entered-get)) 0)
      (progn
        (pyim-outcome-handle 'last-char)
        (pyim-terminate-translation))
    (let ((new (+ pyim-candidate-position arg)))
      (setq len (length pyim-candidates))
      (setq pyim-candidate-position
       (if (>= len new)
         (if (> new 0) new len)
         1))
      (pyim-preview-refresh)
      (pyim-page-refresh t))))

(defun pyim-page-previous-word (arg)
  (interactive "p")
  (pyim-page-next-word (- arg)))

(defun pyim-page-preview-create (&optional separator)
  "這個函數用於創建在 page 中顯示的預覽字元串。

這個預覽是在 page 中顯示，而 `pyim-preview-refresh' 對應的預覽
是在 buffer 光標處顯示，兩者要做區別。"
  (let* ((scheme-name (pyim-scheme-name))
         (class (pyim-scheme-get-option scheme-name :class)))
    (when class
      (funcall (intern (format "pyim-page-preview-create:%S" class)) separator))))

(defun pyim-page-preview-create:quanpin (&optional separator)
  (let* ((separator (or separator " "))
         (translated (mapconcat #'identity
                                (mapcar
                                 #'(lambda (w)
                                     (concat (nth 0 w) (nth 1 w)))
                                 (car pyim-imobjs))
                                separator)))
    (concat
     ;; | 顯示光標位置的字元
     (pyim-with-entered-buffer
       (if (equal 1 (point))
           (concat "|" translated)
         (concat (replace-regexp-in-string (concat separator "'") "'" translated)
                 " |" (buffer-substring-no-properties (point) (point-max)))))
     ;; 用於標記輔助輸入法
     (when (and (eq pyim-assistant-scheme 'quanpin)
                (eq pyim-assistant-scheme-enable t))
       (let ((code (pyim-code-search
                    (pyim-candidate-parse
                     (nth (1- pyim-candidate-position)
                          pyim-candidates))
                    (pyim-scheme-name 'default))))
         (if (> (length code) 0)
             (format " [%s](A)" code)
           " (A)"))))))

(defun pyim-page-preview-create:shuangpin (&optional separator)
  (let ((keymaps (pyim-scheme-get-option (pyim-scheme-name) :keymaps))
        result)
    (dolist (w (car pyim-imobjs))
      (let ((sm (nth 0 w))
            (ym (nth 1 w)))
        (if (equal sm "")
            (push (car (rassoc (list ym) keymaps)) result)
          (push
           (concat (cl-some
                    #'(lambda (x)
                        (when (equal sm (nth 1 x))
                          (car x)))
                    keymaps)
                   (cl-some
                    #'(lambda (x)
                        (when (or (equal ym (nth 2 x))
                                  (equal ym (nth 3 x)))
                          (car x)))
                    keymaps))
           result))))
    (mapconcat 'identity
               (reverse result)
               (or separator " "))))

(defun pyim-page-preview-create:rime (&optional _separator)
  (let* ((context (liberime-get-context))
         (composition (alist-get 'composition context))
         (preedit (alist-get 'preedit composition)))
    (or preedit "")))

(defun pyim-page-preview-create:xingma (&optional separator)
  (let* ((scheme-name (pyim-scheme-name)))
    (cl-flet* ((segment (x)
                        (mapconcat #'identity
                                   (car (pyim-imobjs-create x scheme-name))
                                   (or separator " ")))
               (fmt (x)
                    (mapconcat #'segment
                               (split-string x "'")
                               "'")))
      ;; | 顯示光標位置的字元
      (pyim-with-entered-buffer
        (if (equal (point) (point-max))
            (fmt (buffer-substring-no-properties (point-min) (point-max)))
          (concat (fmt (buffer-substring-no-properties (point-min) (point)))
                  "| "
                  (fmt (buffer-substring-no-properties (point) (point-max)))))))))

(defun pyim-page-menu-create (candidates position &optional separator)
  "這個函數用於創建在 page 中顯示的備選詞條菜單。"
  (let ((i 0) result)
    (dolist (candidate candidates)
      (let ((str (if (consp candidate)
                     (concat (car candidate) (cdr candidate))
                   candidate)))
        (setq i (1+ i))
        ;; 高亮當前選擇的詞條，用於 `pyim-page-next-word'
        (push
         (if (and hightlight-current
                  (= i position))
             (format "%d%s" i
                     (propertize
                      (format "[%s]" str)
                      'face 'pyim-page-selection))
           (format "%d.%s " i str))
         result)))
    (mapconcat #'identity
               (reverse result)
               (or separator ""))))

(defun pyim-page-style:two-lines (page-info)
  "將 PAGE-INFO 格式化為選詞框中顯示的字元串。

樣式類似：

+----------------------------+
| ni hao [1/9]               |
| 1.你好 2.你號 ...          |
+----------------------------+"
  (format "=> %s [%s/%s]: \n%s"
          (pyim-page-preview-create)
          (gethash :current-page page-info)
          (gethash :total-page page-info)
          (pyim-page-menu-create
           (gethash :candidates page-info)
           (gethash :position page-info))))

(defun pyim-page-style:one-line (page-info)
  "將 PAGE-INFO 格式化為選詞框中顯示的字元串。

樣式類似：

+-----------------------------------+
| [ni hao]: 1.你好 2.你號 ... (1/9) |
+-----------------------------------+"
  (format "[%s]: %s(%s/%s)"
          (pyim-page-preview-create " ")
          (pyim-page-menu-create
           (gethash :candidates page-info)
           (gethash :position page-info))
          (gethash :current-page page-info)
          (gethash :total-page page-info)))

(defun pyim-page-style:vertical (page-info)
  "將 PAGE-INFO 格式化為選詞框中顯示的字元串。

樣式類似：

+--------------+
| ni hao [1/9] |
| 1.你好       |
| 2.你號 ...   |
+--------------+"
  (format "=> %s [%s/%s]: \n%s"
          (pyim-page-preview-create)
          (gethash :current-page page-info)
          (gethash :total-page page-info)
          (pyim-page-menu-create
           (gethash :candidates page-info)
           (gethash :position page-info)
           "\n")))

(defun pyim-page-style:minibuffer (page-info)
  "將 PAGE-INFO 格式化為選詞框中顯示的字元串。

樣式類似：

+------------------------------------+
| [ni hao]: 1.你好 2.你號 ...  (1/9) |
+------------------------------------+"
  (format "[%s]: %s(%s/%s)"
          (pyim-page-preview-create)
          (pyim-page-menu-create
           (gethash :candidates page-info)
           (gethash :position page-info))
          (gethash :current-page page-info)
          (gethash :total-page page-info)))

(defun pyim-page-style:exwm (page-info)
  "專門用於 exwm 環境的 page style。"
  (format "[%s]: %s(%s/%s)"
          (let ((class (pyim-scheme-get-option (pyim-scheme-name) :class))
                (preview (pyim-outcome-get)))
            (when (memq class '(quanpin))
              (let ((rest (mapconcat
                           #'(lambda (py)
                               (concat (nth 0 py) (nth 1 py)))
                           (nthcdr (length preview) (car pyim-imobjs))
                           " ")))
                (when (string< "" rest)
                  (setq preview (concat preview rest)))))
            preview)
          (pyim-page-menu-create
           (gethash :candidates page-info)
           (gethash :position page-info))
          (gethash :current-page page-info)
          (gethash :total-page page-info)))

(defun pyim-page-tooltip-show (string position)
  "在 POSITION 位置，使用 posframe 或者 popup 顯示字元串 STRING。"
  (let ((frame (window-frame (selected-window)))
        (length (* pyim-page-length 10))
        (tooltip pyim-page-tooltip))
    (cond ((and (memq tooltip '(posframe child-frame))
                (pyim-posframe-valid-p))
           (posframe-show pyim-page-tooltip-posframe-buffer
                          :string string
                          :position position
                          :min-width pyim-posframe-min-width
                          :background-color (face-attribute 'pyim-page :background)
                          :foreground-color (face-attribute 'pyim-page :foreground)
                          :internal-border-width pyim-posframe-border-width
                          :internal-border-color (face-attribute 'pyim-page-border :background)))
          ((eq tooltip 'minibuffer)
           (let ((max-mini-window-height (+ pyim-page-length 2)))
             (message string)))
          (t (popup-tip string :point position :margin 1)))))

(defun pyim-posframe-valid-p ()
  "Test posframe's status."
  (and (>= emacs-major-version 26)
       (featurep 'posframe)
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p))))))

(defun pyim-outcome-get (&optional n)
  "獲取 outcome"
  (nth (or n 0) pyim-outcome-history))

(defun pyim-outcome-handle (type)
  "依照 TYPE，獲取 pyim 的 outcome，並將其加入 `pyim-outcome-history'。"
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
         (push (pyim-entered-get) pyim-outcome-history))
        (t (error "Pyim: invalid outcome"))))

(defun pyim-page-select-word-simple ()
  "從選詞框中選擇當前詞條。
這個函數與 `pyim-page-select-word' 的區別是：
這個函數不會將選擇的詞條加入個人詞庫，主要的使用場景是：
當用戶需要輸入一個生僻字時，輸入包含該字的一個詞條，
然後再刪除不需要的字，由於這個詞條不是常用詞條，所以
不需要保存到個人詞庫。"
  (interactive)
  (if (null pyim-candidates)
      (pyim-outcome-handle 'last-char)
    (pyim-outcome-handle 'candidate))
  (pyim-terminate-translation))

(defun pyim-page-select-word ()
  "從選詞框中選擇當前詞條，然後刪除該詞條對應拼音。"
  (interactive)
  (if (null pyim-candidates)  ; 如果沒有選項，輸入空格
      (progn
        (pyim-outcome-handle 'last-char)
        (pyim-terminate-translation))
    (cl-case (pyim-scheme-get-option (pyim-scheme-name) :class)
      (rime (call-interactively #'pyim-page-select-word:rime))
      (xingma (call-interactively #'pyim-page-select-word:xingma))
      (t (call-interactively #'pyim-page-select-word:pinyin)))))

(defun pyim-page-select-word:pinyin ()
  "從選詞框中選擇當前詞條，然後刪除該詞條對應拼音。"
  (interactive)
  (pyim-outcome-handle 'candidate)
  (let* ((imobj (car pyim-imobjs))
         (length-selected-word
          ;; 獲取 *這一次* 選擇詞條的長度，在 “多次選擇詞條才能上屏” 的情況下，
          ;; 一定要和 outcome 的概念作區別。
          ;; 比如：xiaolifeidao
          ;; 第一次選擇：小李， outcome = 小李
          ;; 第二次選擇：飛，   outcome = 小李飛
          ;; 第三次選擇：刀，   outcome = 小李飛刀
          (- (length (pyim-outcome-get))
             (length (pyim-outcome-get 1))))
         ;; pyim-imobjs 包含 *pyim-entered-buffer* 裡面光標前面的字元串，
         ;; 某些情況只有部分被翻譯，剩餘部分保存在下麵這個變數
         (to-be-translated (mapconcat #'identity
                                      (mapcar
                                       #'(lambda (w)
                                           (concat (nth 2 w) (nth 3 w)))
                                       (nthcdr length-selected-word imobj))
                                      "")))
    ;; 在使用全拼輸入法輸入長詞的時候，可能需要多次選擇，才能夠將
    ;; 這個詞條上屏，這個地方用來判斷是否是 “最後一次選擇”，如果
    ;; 不是最後一次選擇，就需要截斷 entered，準備下一輪的選擇。

    ;; 判斷方法：entered 為 xiaolifeidao，本次選擇 “小李” 之後，
    ;; 需要將 entered 截斷，“小李” 這個詞條長度為 2，就將 entered
    ;; 從頭開始縮減 2 個 imelem 對應的字元，變成 feidao，為下一次
    ;; 選擇 “飛” 做準備。

    ;; 注意事項：這裡有一個假設前提是：一個 imelem 對應一個漢字，
    ;; 在全拼輸入法中，這個假設大多數情況是成立的，但在型碼輸入法
    ;; 中，比如五筆輸入法，就不成立，好在型碼輸入法一般不需要多次
    ;; 選擇。
    (if (or (< length-selected-word (length imobj))
            (pyim-with-entered-buffer (< (point) (point-max))))
        (progn
          (pyim-with-entered-buffer
            ;; 把本次已經選擇的詞條對應的子 entered，從 entered
            ;; 字元串裡面剪掉。
            (delete-region (point-min) (point))
            (insert to-be-translated)
            ;; 為下一次選詞作準備，一般情況下詞庫裡面的詞條不會超過 20
            ;; 個漢字，所以這裡一次遞歸的處理 20 個 imelem. 這種方式
            ;; 可能比逐字選擇更加好用。
            (goto-char (pyim-entered-next-imelem-position 20 t 1)))
          (pyim-entered-refresh))
      ;; pyim 詞頻調整策略：
      ;; 1. 如果一個詞條是用戶在輸入過程中，自己新建的詞條，那麼就將這個詞條
      ;;    添加到個人詞庫的後面（不放置前面是為了減少誤輸詞條的影響）。
      ;; 2. 如果輸入的詞條，先前已經在候選詞列表中，就自動將其放到第一位。
      ;;    這樣的話，一個新詞要輸入兩遍之後才可能出現在第一位。
      ;; 3. pyim 在啟動的時候，會使用詞頻信息，對個人詞庫作一次排序。
      ;;    用作 pyim 下一次使用。
      (if (member (pyim-outcome-get) pyim-candidates)
          (pyim-create-word (pyim-outcome-get) t)
        (pyim-create-word (pyim-outcome-get)))

      (pyim-terminate-translation)
      ;; pyim 使用這個 hook 來處理聯想詞。
      (run-hooks 'pyim-page-select-finish-hook))))

(defun pyim-page-select-word:xingma ()
  "從選詞框中選擇當前詞條，然後刪除該詞條對應編碼。"
  (interactive)
  (pyim-outcome-handle 'candidate)
  (if (pyim-with-entered-buffer
        (and (> (point) 1)
             (< (point) (point-max))))
      (progn
        (pyim-with-entered-buffer
          ;; 把本次已經選擇的詞條對應的子 entered，從 entered
          ;; 字元串裡面剪掉。
          (delete-region (point-min) (point)))
        (pyim-entered-refresh))
    (when (string-empty-p (pyim-code-search (pyim-outcome-get)
                                            (pyim-scheme-name)))
      (pyim-create-word (pyim-outcome-get) t))
    (pyim-terminate-translation)
    ;; pyim 使用這個 hook 來處理聯想詞。
    (run-hooks 'pyim-page-select-finish-hook)))

(defun pyim-page-select-word:rime ()
  "從選詞框中選擇當前詞條，專門用於 rime 輸入法支持。"
  (interactive)
  (if (null pyim-candidates)  ; 如果沒有選項，輸入空格
      (progn
        (pyim-outcome-handle 'last-char)
        (pyim-terminate-translation))
    ;; pyim 告訴 liberime 選擇其他的詞條
    (liberime-select-candidate (- pyim-candidate-position 1))
    (let* ((context (liberime-get-context))
           imobjs)
      (pyim-outcome-handle 'candidate)
      (if (not context)
          (progn
            (unless (member (pyim-outcome-get) pyim-candidates)
              (pyim-create-word (pyim-outcome-get)))
            (pyim-terminate-translation)
            ;; pyim 使用這個 hook 來處理聯想詞。
            (run-hooks 'pyim-page-select-finish-hook))
        ;; BUG: 預設 liberime 得到的 candidate 是分頁的，一頁只包含5個詞條，
        ;; pyim 需要 liberime 不分頁，或者一頁包含盡可能多個詞。
        (setq pyim-candidates
              (let* ((menu (alist-get 'menu context))
                     (candidates (alist-get 'candidates menu)))
                candidates))
        (setq pyim-candidate-position 1)
        (pyim-preview-refresh)
        (pyim-page-refresh)))))

(defun pyim-page-select-word-by-number (&optional n)
  "使用數字編號來選擇對應的詞條。"
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
          (if (> (+ index (pyim-page-start)) end)
              (pyim-page-refresh)
            (setq pyim-candidate-position (+ pyim-candidate-position index))
            (pyim-page-select-word))))
    ;; 有些輸入法使用數字鍵編碼，這種情況下，數字鍵就
    ;; 不能用來選詞了。
    (call-interactively #'pyim-self-insert-command)))

;; ** 處理標點符號
(defun pyim-translate-get-trigger-char ()
  "檢查 `pyim-translate-trigger-char' 是否為一個合理的 trigger char 。

pyim 的 translate-trigger-char 要占用一個鍵位，為了防止用戶
自定義設置與輸入法沖突，這裡需要檢查一下這個鍵位設置的是否合理，
如果不合理，就返回輸入法預設設定。"
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
          ;; (message "注意：pyim-translate-trigger-char 設置和當前輸入法沖突，使用推薦設置：\"%s\""
          ;;          prefer-trigger-chars)
          prefer-trigger-chars)
      user-trigger-char)))

(defun pyim-translate (char)
  "Pyim 字元轉換函數，主要用於處理標點符號。

pyim 在運行過程中調用這個函數來進行標點符號格式的轉換。

常用的標點符號數量不多，所以 pyim 沒有使用文件而是使用一個變數
`pyim-punctuation-dict' 來設置標點符號對應表，這個變數是一個
alist 列表。"
  (let* ((str (char-to-string char))
         ;; 注意：`str' 是 *待輸入* 的字元對應的字元串。
         (str-before-1 (pyim-char-before-to-string 0))
         (str-before-2 (pyim-char-before-to-string 1))
         (str-before-3 (pyim-char-before-to-string 2))
         (str-before-4 (pyim-char-before-to-string 3))
         ;; 從標點詞庫中搜索與 `str' 對應的標點列表。
         (punc-list (assoc str pyim-punctuation-dict))
         ;; 從標點詞庫中搜索與 `str-before-1' 對應的標點列表。
         (punc-list-before-1
          (cl-some (lambda (x)
                     (when (member str-before-1 x) x))
                   pyim-punctuation-dict))
         ;; `str-before-1' 在其對應的標點列表中的位置。
         (punc-posit-before-1
          (cl-position str-before-1 punc-list-before-1
                       :test #'equal))
         (trigger-str (pyim-translate-get-trigger-char)))
    (cond
     ;; 空格之前的字元什麼也不輸入。
     ((< char ? ) "")

     ;; 這個部份與標點符號處理無關，主要用來快速刪除用戶自定義詞條。
     ;; 比如：在一個中文字元串後輸入 2-v，可以將光標前兩個中文字元
     ;; 組成的字元串，從個人詞庫刪除。
     ((and (eq (char-before) ?-)
           (pyim-string-match-p "[0-9]" str-before-2)
           (pyim-string-match-p "\\cc" str-before-3)
           (equal str trigger-str))
      (delete-char -2)
      (pyim-delete-word-at-point
       (string-to-number str-before-2))
      "")
     ;; 這個部份與標點符號處理無關，主要用來快速保存用戶自定義詞條。
     ;; 比如：在一個中文字元串後輸入 2v，可以將光標前兩個中文字元
     ;; 組成的字元串，保存到個人詞庫。
     ((and (member (char-before) (number-sequence ?2 ?9))
           (pyim-string-match-p "\\cc" str-before-2)
           (equal str trigger-str))
      (delete-char -1)
      (pyim-create-word-at-point
       (string-to-number str-before-1))
      "")

     ;; 光標前面的字元為中文字元時，按 v 清洗當前行的內容。
     ((and (not (numberp punc-posit-before-1))
           (pyim-string-match-p "\\cc" str-before-1)
           (equal str trigger-str))
      (funcall pyim-wash-function)
      "")

     ;; 關閉標點轉換功能時，只插入英文標點。
     ((not (pyim-punctuation-full-width-p))
      str)

     ;; 當用戶使用 org-mode 以及 markdown 等輕量級標記語言撰寫文檔時，
     ;; 常常需要輸入數字列表，比如：

     ;; 1. item1
     ;; 2. item2
     ;; 3. item3

     ;; 在這種情況下，數字後面輸入句號必須是半形句號而不是全形句號，
     ;; pyim 調用 `pyim-translate' 時，會檢測光標前面的字元，如果這個
     ;; 字元屬於 `pyim-punctuation-escape-list' ，pyim 將輸入半形標點，
     ;; 具體細節見：`pyim-translate'
     ((member (char-before)
              pyim-punctuation-escape-list)
      str)

     ;; 當 `pyim-punctuation-half-width-functions' 中
     ;; 任意一個函數返回值為 t 時，插入英文標點。
     ((cl-some #'(lambda (x)
                   (if (functionp x)
                       (funcall x char)
                     nil))
               pyim-punctuation-half-width-functions)
      str)

     ;; 當光標前面為英文標點時，按 `pyim-translate-trigger-char'
     ;; 對應的字元後，自動將其轉換為對應的中文標點。
     ((and (numberp punc-posit-before-1)
           (= punc-posit-before-1 0)
           (equal str trigger-str))
      (pyim-punctuation-translate 'full-width)
      "")

     ;; 當光標前面為中文標點時，按 `pyim-translate-trigger-char'
     ;; 對應的字元後，自動將其轉換為對應的英文標點。
     ((and (numberp punc-posit-before-1)
           (> punc-posit-before-1 0)
           (equal str trigger-str))
      (pyim-punctuation-translate 'half-width)
      "")

     ;; 正常輸入標點符號。
     (punc-list
      (pyim-punctuation-return-proper-punct punc-list))

     ;; 當輸入的字元不是標點符號時，原樣插入。
     (t str))))

(defun pyim-char-before-to-string (num)
  "得到光標前第 `num' 個字元，並將其轉換為字元串。"
  (let* ((point (point))
         (point-before (- point num)))
    (when (and (> point-before 0)
               (char-before point-before))
      (char-to-string (char-before point-before)))))

(defun pyim-char-after-to-string (num)
  "得到光標後第 `num' 個字元，並將其轉換為字元串。"
  (let* ((point (point))
         (point-after (+ point num)))
    (when (char-after point-after)
      (char-to-string (char-after point-after)))))

(defun pyim-wash-current-line-function ()
  "清理當前行的內容，比如：刪除不必要的空格，等。"
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

;; ** 切換中英文標點符號
(defun pyim-punctuation-full-width-p ()
  "判斷是否需要切換到全形標點輸入模式

輸入標點的樣式的改變（全形或者半形）受三個方面影響：

1. 用戶是否手動切換了標點樣式？
2  用戶是否手動切換到英文輸入模式？
3. pyim 是否根據環境自動切換到英文輸入模式？

三方面的綜合結果為：只要當前的輸入模式是英文輸入模式，那麼輸入的
標點符號 *必定* 是半形標點，如果當前輸入模式是中文輸入模式，那麼，
輸入標點的樣式用戶可以使用 `pyim-punctuation-toggle'手動控制，具
體請參考 `pyim-punctuation-full-width-p'。"
  (cl-case (car pyim-punctuation-translate-p)
    (yes t)
    (no nil)
    (auto
     ;; 如果用戶手動或者根據環境自動切換為英文輸入模式，
     ;; 那麼標點符號也要切換為半形模式。
     (and (not pyim-input-ascii)
          (not (pyim-auto-switch-english-input-p))))))

(defun pyim-punctuation-toggle ()
  "Pyim 標點符號全形半形模式切換命令。

每次運行 `pyim-punctuation-toggle' 命令，都會調整變數
`pyim-punctuation-translate-p' 的取值，`pyim-translate' 根據
`pyim-punctuation-full-width-p' 函數的返回值，來決定是否轉換標點
符號：

1. 當返回值為 'yes 時，`pyim-translate' 轉換標點符號，從而輸入全形標點。
2. 當返回值為 'no 時，`pyim-translate' 忽略轉換，從而輸入半形標點。
3. 當返回值為 'auto 時，根據中英文環境，自動切換。"
  (interactive)
  (setq pyim-punctuation-translate-p
        `(,@(cdr pyim-punctuation-translate-p)
          ,(car pyim-punctuation-translate-p)))
  (message
   (cl-case (car pyim-punctuation-translate-p)
     (yes "開啟全形標點輸入模式。")
     (no "開啟半形標點輸入模式。")
     (auto "開啟全半形標點自動轉換模式。"))))

(defun pyim-punctuation-translate-at-point ()
  "切換光標處標點的樣式(全形 or 半形)。

用戶也可以使用命令 `pyim-punctuation-translate-at-point' 來切換
 *光標前* 標點符號的樣式。"
  (interactive)
  (let* ((current-char (char-to-string (preceding-char)))
         (punc-list
          (cl-some (lambda (x)
                     (when (member current-char x) x))
                   pyim-punctuation-dict)))
    (when punc-list
      (delete-char -1)
      (if (equal current-char (car punc-list))
          (insert (pyim-punctuation-return-proper-punct punc-list))
        (insert (car punc-list))))))

(defun pyim-flatten-list (my-list)
  (cond
   ((null my-list) nil)
   ((atom my-list) (list my-list))
   (t (append (pyim-flatten-list (car my-list))
              (pyim-flatten-list (cdr my-list))))))

(defun pyim-punctuation-translate (&optional punct-style)
  "將光標前1個或前後連續成對的n個標點符號進行全形/半形轉換。

當 PUNCT-STYLE 設置為 'full-width 時，所有的標點符號轉換為全形符
號，設置為 'half-width 時，轉換為半形符號。"
  (interactive)
  (let ((punc-list (pyim-flatten-list pyim-punctuation-dict))
        (punct-style
         (or punct-style
             (intern (completing-read
                      "將光標處的標點轉換為" '("full-width" "half-width")))))
        ;; lnum : puncts on the left (before point)
        (lnum 0)
        ;; rnum : puncts on the right (after point)
        (rnum 0)
        (point (point))
        last-puncts result)
    (catch 'break
      (while t
        (let ((str (pyim-char-after-to-string rnum)))
          (if (member str punc-list)
              (cl-incf rnum)
            (throw 'break nil)))))
    (catch 'break
      (while (<= lnum rnum)
        (let ((str (pyim-char-before-to-string lnum)))
          (if (member str punc-list)
              (cl-incf lnum)
            (throw 'break nil)))))
    ;; 右側與左側成對匹配
    (setq rnum (min lnum rnum))
    (setq last-puncts (buffer-substring (- point lnum) (+ point rnum)))
    ;; 刪除舊的標點符號
    (delete-char rnum)
    (delete-char (- 0 lnum))
    (dolist (punct (split-string last-puncts ""))
      (dolist (puncts pyim-punctuation-dict)
        (let ((position (cl-position punct puncts :test #'equal)))
          (when position
            (cond
             ((eq punct-style 'full-width)
              (if (= position 0)
                  (push (pyim-punctuation-return-proper-punct puncts) result)
                (push punct result)))
             ((eq punct-style 'half-width)
              (if (= position 0)
                  (push punct result)
                (push (car puncts) result))))))))
    (insert (mapconcat #'identity (reverse result) ""))
    (backward-char rnum)))

(defun pyim-punctuation-return-proper-punct (punc-list &optional before)
  "返回合適的標點符號，PUNCT-LIST 為標點符號列表。

這個函數用於處理成對的全形標點符號，簡單來說：如果第一次輸入的標
點是：（‘）時，那麼下一次輸入的標點就是（’）。

PUNCT-LIST 格式類似：

   `(\",\" \"，\") 或者：`(\"'\" \"‘\" \"’\")

當 BEFORE 為 t 時，只返回切換之前的結果，這個用來獲取切換之前的
標點符號。

函數 `pyim-punctuation-return-proper-punct' 內部，我們使用變數
`pyim-punctuation-pair-status' 來記錄 “成對” 中文標點符號的狀態。"
  (let* ((str (car punc-list))
         (punc (cdr punc-list))
         (switch-p (cdr (assoc str pyim-punctuation-pair-status))))
    (if (= (safe-length punc) 1)
        (car punc)
      (if before
          (setq switch-p (not switch-p))
        (setf (cdr (assoc str pyim-punctuation-pair-status))
              (not switch-p)))
      (if switch-p
          (car punc)
        (nth 1 punc)))))

;; ** 與拼音輸入相關的用戶命令
(defun pyim-entered-delete-backward-char (&optional n)
  "在pyim-entered-buffer中向後刪除1個字元"
  (interactive)
  (pyim-with-entered-buffer
    (ignore-errors
      (delete-char (- 0 (or n 1)))))
  (if (> (length (pyim-entered-get)) 0)
      (pyim-entered-refresh t)
    (pyim-outcome-handle "")
    (pyim-terminate-translation)))

(defun pyim-entered-delete-forward-char ()
  "在pyim-entered-buffer中向前刪除1個字元"
  (interactive)
  (pyim-entered-delete-backward-char -1))

(defun pyim-entered-delete-backward-imelem (&optional search-forward)
  "`pyim-entered-buffer’ 中向後刪除一個 imelem 對應的字元串"
  (interactive)
  (let ((position (pyim-entered-next-imelem-position 1 search-forward)))
    (pyim-with-entered-buffer
      (delete-region (point) position))
    (pyim-entered-refresh t)))

(defun pyim-entered-delete-forward-imelem ()
  "`pyim-entered-buffer’ 中向前刪除一個 imelem 對應的字元串"
  (interactive)
  (pyim-entered-delete-backward-imelem t))

(define-obsolete-function-alias
  'pyim-convert-code-at-point 'pyim-convert-string-at-point)

;;;###autoload
(defun pyim-convert-string-at-point (&optional return-cregexp)
  "將光標前的用戶輸入的字元串轉換為中文。

如果 RETURN-CREGEXP 為真，pyim 會把用戶輸入的字元串當作
拼音，依照這個拼音來構建一個 regexp，用戶可以用這個 regexp
搜索拼音對應的漢字。"
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
           (length 0)
           code length)
      (cond ((string-match
              ;; 創建一個 regexp，用於提取出光標處一個適合
              ;; 轉換的字元串。
              (format "[%s]+ *$"
                      (cl-delete-duplicates
                       (concat first-chars rest-chars "'-")))
              string)
             (setq code
                   ;; 一些編程語言使用單引號 ' 做為字元串的標記，這裡需要特殊處理。
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
             ;; 當光標前的一個字元是標點符號時，半形/全形切換。
             (call-interactively 'pyim-punctuation-translate-at-point))
            ((and nil ;; 暫時還沒有準備啟用這個功能
                  (eq pyim-default-scheme 'quanpin)
                  (string-match "\\cc *$" string))
             ;; 如果光標處是漢字，就用漢字的拼音來重新啟動輸入法
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
             (setq code (pyim-hanzi2pinyin
                         (replace-regexp-in-string " " "" string)
                         nil "-" nil t))
             (when (> length 0)
               (setq unread-command-events
                     (append (listify-key-sequence code)
                             unread-command-events))
               (setq pyim-force-input-chinese t)))
            (t (message "Pyim: pyim-convert-string-at-point do noting."))))))

(defun pyim-quit-clear ()
  "取消當前輸入的命令。"
  (interactive)
  (pyim-outcome-handle "")
  (pyim-terminate-translation))

(defun pyim-quit-no-clear ()
  "字母上屏命令。"
  (interactive)
  (pyim-outcome-handle 'pyim-entered)
  (pyim-terminate-translation))

(defun pyim-inactivate ()
  "取消 pyim 的激活狀態。"
  (interactive)
  (mapc 'kill-local-variable pyim-local-variable-list))

(defun pyim-toggle-input-ascii ()
  "pyim 切換中英文輸入模式。同時調整標點符號樣式。"
  (interactive)
  (setq pyim-input-ascii
        (not pyim-input-ascii)))

;; ** 讓 isearch-mode 通過 pinyin 搜索中文
(defun pyim-cregexp-build (string)
  "根據 STRING 構建一個中文 regexp，用於 \"拼音搜索漢字\"。
比如：\"nihao\" -> \"[你呢...][好號...] \\| nihao\""
  (or (ignore-errors
        (rx-to-string (pyim-cregexp-build-from-rx
                       (lambda (x)
                         (if (stringp x)
                             (xr (pyim-cregexp-build-1 x))
                           x))
                       (xr string))))
      string))

(defun pyim-cregexp-build-from-rx (fn rx-form)
  (cond
   ((not rx-form) nil)
   ((and (listp rx-form)
         (not (listp (cdr rx-form))))
    (funcall fn rx-form))
   ((and (listp rx-form)
         (not (eq 'any (car rx-form))))
    (mapcar (lambda (x)
              (pyim-cregexp-build-from-rx fn x))
            rx-form))
   ((and (listp rx-form)
         (eq 'any (car rx-form)))
    rx-form)
   (t (funcall fn rx-form))))

(defun pyim-cregexp-build-1 (string)
  (let* ((scheme-name (pyim-scheme-name))
         (class (pyim-scheme-get-option scheme-name :class)))
    ;; 確保 pyim 詞庫加載
    (pyim-dcache-init-variables)
    ;; pyim 暫時只支持全拼和雙拼搜索
    (when (not (member class '(quanpin shuangpin)))
      (setq scheme-name 'quanpin))
    (if (or (pyim-string-match-p "[^a-z']+" string))
        string
      (let* ((imobjs
              ;; 如果一個字元串以'結尾,就按照拼音首字母字元串處理。
              (if (pyim-string-match-p "'$" string)
                  (list (mapcar #'(lambda (x)
                                    (list (char-to-string x)))
                                (string-to-list string)))
                ;; Slowly operating, need to improve.
                (pyim-imobjs-create string scheme-name)))
             (regexp-list
              (mapcar
               #'(lambda (imobj)
                   (pyim-cregexp-build:quanpin imobj))
               imobjs))
             (regexp
              (when regexp-list
                (mapconcat #'identity
                           (delq nil regexp-list)
                           "\\|")))
             (regexp
              (if (> (length regexp) 0)
                  (concat string "\\|" regexp)
                string)))
        regexp))))

(defun pyim-cregexp-build:quanpin (imobj &optional match-beginning
                                         first-equal all-equal)
  "從 IMOBJ 創建一個搜索中文的 regexp。"
  (let* ((imobj
          (mapcar #'(lambda (x)
                      (concat (nth 0 x) (nth 1 x)))
                  imobj))
         (cchar-list
          (let ((n 0) results)
            (dolist (py imobj)
              (let* ((equal-match
                      (or all-equal
                          (and first-equal (= n 0))))
                     (cchars
                      ;; 只取常用字，不常用的漢字忽略，防止生成的
                      ;; regexp 太長而無法搜索
                      (mapconcat #'(lambda (x)
                                     (car (split-string x "|")))
                                 (pyim-pinyin2cchar-get py equal-match nil t) "")))
                (push cchars results))
              (setq n (+ 1 n)))
            (nreverse results)))
         (regexp
          (mapconcat #'(lambda (x)
                         (when (pyim-string-match-p "\\cc" x)
                           (format "[%s]" x)))
                     cchar-list "")))
    (unless (equal regexp "")
      (concat (if match-beginning "^" "") regexp))))

(defun pyim-isearch-search-fun ()
  "這個函數為 isearch 相關命令添加中文拼音搜索功能，
做為 `isearch-search-fun' 函數的 advice 使用。"
  (funcall
   (lambda ()
     `(lambda (string &optional bound noerror count)
        (funcall (if ,isearch-forward
                     're-search-forward
                   're-search-backward)
                 (pyim-cregexp-build string) bound noerror count)))))

;;;###autoload
(define-minor-mode pyim-isearch-mode
  "這個 mode 為 isearch 添加拼音搜索功能。"
  :global t
  :group 'pyim
  :require 'pyim
  :lighter " pyim-isearch"
  (if pyim-isearch-mode
      (progn
        (advice-add 'isearch-search-fun :override #'pyim-isearch-search-fun)
        (message "PYIM: `pyim-isearch-mode' 已經激活，激活後，一些 isearch 擴展包有可能失效。"))
    (advice-remove 'isearch-search-fun #'pyim-isearch-search-fun)))

(defun pyim-convert-cregexp-at-point (&optional insert-only)
  "將光標前的字元串按拼音的規則轉換為一個搜索中文的 regexp。
用於實現拼音搜索中文的功能。

在 minibuffer 中，這個命令預設會自動運行 `exit-minibuffer'。
這個可以使用 INSERT-ONLY 參數控制。"
  (interactive "P")
  (unless (equal input-method-function 'pyim-input-method)
    (activate-input-method 'pyim))
  (let* ((buffer-string
          (buffer-substring (point-min) (point-max)))
         (string (if mark-active
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (buffer-substring
                    (point)
                    (save-excursion
                      (skip-syntax-backward "w")
                      (point)))))
         (length (length string))
         (cregexp (pyim-cregexp-build string)))
    (delete-char (- 0 length))
    (cond
     ;; Deal with `org-search-view'
     ((and (window-minibuffer-p)
           (string-match-p
            (regexp-quote "[+-]Word/{Regexp}") buffer-string))
      (insert (format "{%s}" cregexp)))
     (t (insert cregexp)))
    (when (and (not insert-only)
               (window-minibuffer-p))
      (exit-minibuffer))))

;; ** 讓 forward/backward 支持中文
(defun pyim-forward-word (&optional arg)
  "向前移動 ARG 英文或者中文詞，向前移動時基於 *最長* 的詞移動。"
  (interactive "P")
  (or arg (setq arg 1))
  (dotimes (i arg)
    (let* ((words (pyim-cwords-at-point t))
           (max-length
            (cl-reduce #'max
                       (cons 0 (mapcar #'(lambda (word)
                                           (nth 2 word))
                                       words))))
           (max-length (max (or max-length 1) 1)))
      (forward-char max-length))))

(defun pyim-backward-word (&optional arg)
  "向後移動 ARG 個英文或者中文詞，向後移動時基於 *最長* 的詞移動。"
  (interactive "P")
  (or arg (setq arg 1))
  (dotimes (i arg)
    (let* ((words (pyim-cwords-at-point))
           (max-length
            (cl-reduce #'max
                       (cons 0 (mapcar #'(lambda (word)
                                           (nth 1 word))
                                       words))))
           (max-length (max (or max-length 1) 1)))
      (backward-char max-length))))

(defun pyim-cwords-at-point (&optional end-of-point)
  "獲取光標當前的詞條列表，當 END-OF-POINT 設置為 t 時，獲取光標後的詞條列表。
詞條列表的每一個元素都是列表，這些列表的第一個元素為詞條，第二個元素為光標處到詞條
頭部的距離，第三個元素為光標處到詞條尾部的距離。

其工作原理是：

1. 使用 `thing-at-point' 獲取當前光標處的一個字元串，一般而言：英文會得到
   一個單詞，中文會得到一個句子。
2. 英文單詞直接返回這個單詞的列表。
3. 中文句子首先用 `pyim-cstring-split-to-list' 分詞，然後根據光標在中文句子
   中的位置，篩選出符合要求的中文詞條。得到並返回 *一個* 或者 *多個* 詞條
   的列表。"
  ;;
  ;;                                光標到詞 光標到詞
  ;;                                首的距離 尾的距離
  ;;                                       | |
  ;; 獲取光標當前的詞<I>條列表 -> (("的詞" 2 0) ("詞條" 1 1))
  ;;
  (let* ((case-fold-search t)
         (current-pos (point))
         (current-char
          (if end-of-point
              (string (following-char))
            (string (preceding-char))))
         (str (thing-at-point 'word t))
         (str-length (length str))
         (str-boundary (bounds-of-thing-at-point 'word))
         (str-beginning-pos (when str-boundary
                              (car str-boundary)))
         (str-end-pos (when str-boundary
                        (cdr str-boundary)))
         (str-offset
          (when (and str-beginning-pos str-end-pos)
            (if (= current-pos str-end-pos)
                (1+ (- str-end-pos str-beginning-pos))
              (1+ (- current-pos str-beginning-pos)))))
         str-offset-adjusted words-alist results)

    ;; 當字元串長度太長時，`pyim-cstring-split-to-list'
    ;; 的速度比較慢，這裡確保待分詞的字元串長度不超過 10。
    (when (and str (not (pyim-string-match-p "\\CC" str)))
      (if (> str-offset 5)
          (progn (setq str-offset-adjusted 5)
                 (setq str (substring str
                                      (- str-offset 5)
                                      (min (+ str-offset 5) str-length))))
        (setq str-offset-adjusted str-offset)
        (setq str (substring str 0 (min 9 str-length)))))

    (cond
     ((and str (not (pyim-string-match-p "\\CC" str)))
      (setq words-alist
            (pyim-cstring-split-to-list str))
      (dolist (word-list words-alist)
        (let ((word-begin (nth 1 word-list))
              (word-end (nth 2 word-list)))
          (if (if end-of-point
                  (and (< str-offset-adjusted word-end)
                       (>= str-offset-adjusted word-begin))
                (and (<= str-offset-adjusted word-end)
                     (> str-offset-adjusted word-begin)))
              (push (list (car word-list)
                          (- str-offset-adjusted word-begin) ;; 例如：("你好" 1 1)
                          (- word-end str-offset-adjusted))
                    results))))
      (or results
          (list (if end-of-point
                    (list current-char 0 1)
                  (list current-char 1 0)))))
     (str (list (list str
                      (- current-pos str-beginning-pos)
                      (- str-end-pos current-pos)))))))

(defun pyim-cstring-split-to-list (chinese-string &optional max-word-length)
  "一個基於 pyim 的中文分詞函數。這個函數可以將中文字元
串 CHINESE-STRING 分詞，得到一個詞條 alist，這個 alist 的元素
都是列表，其中第一個元素為分詞得到的詞條，第二個元素為詞條相對於
字元串中的起始位置，第三個元素為結束位置。分詞時，預設詞條不超過
6個字元，用戶可以通過 MAX-WORD-LENGTH 來自定義，但值得注意的是：
這個值設置越大，分詞速度越慢。

注意事項：
1. 這個工具使用暴力匹配模式來分詞，*不能檢測出* pyim 詞庫中不存在
   的中文詞條。
2. 這個函數的分詞速度比較慢，僅僅適用於中文短句的分詞，不適用於
   文章分詞。根據評估，20 個漢字組成的字元串需要大約 0.3s，40 個
   漢字消耗 1s，隨著字元串長度的增大消耗的時間呈幾何倍數增加。"
  ;;                   (("天安" 5 7)
  ;; 我愛北京天安門 ->  ("天安門" 5 8)
  ;;                    ("北京" 3 5)
  ;;                    ("我愛" 1 3))
  (cl-labels
      ((get-possible-words-internal
        ;; 內部函數，功能類似：
        ;; ("a" "b" "c" "d") -> ("abcd" "abc" "ab")
        (my-list number)
        (cond
         ((< (length my-list) 2) nil)
         (t (append
             (let* ((str (mapconcat #'identity my-list ""))
                    (length (length str)))
               (when (<= length (or max-word-length 6))
                 (list (list str number (+ number length)))))
             (get-possible-words-internal
              (reverse (cdr (reverse my-list))) number)))))
       (get-possible-words
        ;; 內部函數，功能類似：
        ;; ("a" "b" "c" "d") -> ("abcd" "abc" "ab" "bcd" "bc" "cd")
        (my-list number)
        (cond
         ((null my-list) nil)
         (t (append (get-possible-words-internal my-list number)
                    (get-possible-words (cdr my-list) (1+ number)))))))

    ;; 如果 pyim 詞庫沒有加載，加載 pyim 詞庫，
    ;; 確保 `pyim-dcache-get' 可以正常運行。
    (pyim-dcache-init-variables)

    (let ((string-alist
           (get-possible-words
            (mapcar #'char-to-string
                    (string-to-vector chinese-string)) 1))
          words-list result)
      (dolist (string-list string-alist)
        (let ((pinyin-list (pyim-hanzi2pinyin (car string-list) nil "-" t)))
          (dolist (pinyin pinyin-list)
            (let ((words (pyim-dcache-get pinyin '(code2word)))) ; 忽略個人詞庫可以提高速度
              (dolist (word words)
                (when (equal word (car string-list))
                  (push string-list result)))))))
      result)))

;; (let ((str "醫生隨時都有可能被患者及其家屬反咬一口"))
;;   (benchmark 1 '(pyim-cstring-split-to-list str)))

;; (let ((str "醫生隨時都有可能被患者及其家屬反咬一口"))
;;   (pyim-cstring-split-to-list str))

(defun pyim-cstring-split-to-string (string &optional prefer-short-word
                                            separator max-word-length)
  "將中文字元串 STRING 分詞。

在分詞的位置插入空格或者自定義分隔符 SEPERATERS，預設情況下較長的
詞條優先使用，如果 PREFER-SHORT-WORD 設置為 t，則優先使用較短的
詞條。預設最長詞條不超過6個字元，用戶可以通 MAX-WORD-LENGTH 來
自定義詞條的最大長度，但值得注意的是，這個值設置越大，分詞速度越
慢。"
  (let ((string-list
         (if (pyim-string-match-p "\\CC" string)
             (split-string
              (replace-regexp-in-string
               "\\(\\CC+\\)" "@@@@\\1@@@@" string) "@@@@")
           (list string))))
    (mapconcat
     #'(lambda (str)
         (when (> (length str) 0)
           (if (not (pyim-string-match-p "\\CC" str))
               (pyim-cstring-split-to-string-1
                str prefer-short-word separator max-word-length)
             (concat " " str " "))))
     string-list "")))

(defun pyim-cstring-split-to-string-1 (chinese-string &optional prefer-short-word
                                                      separator max-word-length)
  "`pyim-cstring-split-to-string' 內部函數。"
  (let ((str-length (length chinese-string))
        (word-list (cl-delete-duplicates
                    ;;  判斷兩個詞條在字元串中的位置
                    ;;  是否沖突，如果沖突，僅保留一個，
                    ;;  刪除其它。
                    (pyim-cstring-split-to-list chinese-string max-word-length)
                    :test #'(lambda (x1 x2)
                              (let ((begin1 (nth 1 x1))
                                    (begin2 (nth 1 x2))
                                    (end1 (nth 2 x1))
                                    (end2 (nth 2 x2)))
                                (not (or (<= end1 begin2)
                                         (<= end2 begin1)))))
                    :from-end prefer-short-word))
        position-list result)

    ;; 提取詞條相對於字元串的位置信息。
    (dolist (word word-list)
      (push (nth 1 word) position-list)
      (push (nth 2 word) position-list))

    ;; 將位置信息由小到大排序。
    (setq position-list
          (cl-delete-duplicates (sort position-list #'<)))

    ;; 在分詞的位置插入空格或者用戶指定的分隔符。
    (dotimes (i str-length)
      (when (member (1+ i) position-list)
        (push (or separator " ") result))
      (push (substring chinese-string i (1+ i))  result))
    (setq result (nreverse result))
    (mapconcat #'identity result "")))

(defun pyim-cstring-split-buffer ()
  "將一個 buffer 中的中文文章，進行分詞操作。"
  (interactive)
  (message "分詞開始！")
  (goto-char (point-min))
  (while (not (eobp))
    (let ((string (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
      (delete-region (line-beginning-position)
                     (min (+ (line-end-position) 1) (point-max)))
      (insert (pyim-cstring-split-to-string string))
      (insert "\n")))
  (goto-char (point-min))
  (message "分詞完成！"))

;; ** 漢字到拼音的轉換工具
;;;###autoload
(defun pyim-hanzi2pinyin (string &optional shou-zi-mu separator
                                 return-list ignore-duo-yin-zi adjust-duo-yin-zi)
  "將漢字字元串轉換為對應的拼音字元串的工具。

如果 SHOU-ZI-MU 設置為 t，轉換僅得到拼音首字母字元串。當
RETURN-LIST 設置為 t 時，返回一個拼音列表，這個列表包含詞條的一個
或者多個拼音（詞條包含多音字時）；如果 IGNORE-DUO-YIN-ZI 設置為 t，
遇到多音字時，只使用第一個拼音，其它拼音忽略；當
ADJUST-DUO-YIN-Zi 設置為 t 時，`pyim-hanzi2pinyin' 會使用 pyim 已
安裝的詞庫來校正多音字，但這個功能有一定的限制：

1. pyim 普通詞庫中不存在的詞條不能校正；
2. 多音字校正速度比較慢，實時轉換會產生卡頓。

BUG: 當 STRING 中包含其它標點符號，並且設置 SEPERATER 時，結果會
包含多餘的連接符：比如：'你=好' --> 'ni-=-hao'"
  (if (not (pyim-string-match-p "\\cc" string))
      (if return-list
          (list string)
        string)
    (let (string-list pinyins-list pinyins-list-permutated pinyins-list-adjusted)

      ;; 將漢字字元串轉換為字元list，英文原樣輸出。
      ;; 比如：“Hello銀行” -> ("Hello" "銀" "行")
      (setq string-list
            (if (pyim-string-match-p "\\CC" string)
                ;; 處理中英文混合的情況
                (split-string
                 (replace-regexp-in-string
                  "\\(\\cc\\)" "@@@@\\1@@@@" string)
                 "@@@@")
              ;; 如果詞條只包含中文，使用`string-to-vector'
              ;; 這樣處理速度比較快。
              (string-to-vector string)))

      ;; 將上述漢字字元串裡面的所有漢字轉換為與之對應的拼音 list。
      ;; 比如：("Hello" "銀" "行") -> (("Hello") ("yin") ("hang" "xing"))
      (mapc
       #'(lambda (str)
           ;; `string-to-vector' 得到的是 char vector，需要將其轉換為 string。
           (when (numberp str)
             (setq str (char-to-string str)))
           (cond
            ((> (length str) 1)
             (push (list str) pinyins-list))
            ((and (> (length str) 0)
                  (pyim-string-match-p "\\cc" str))
             (push (or (pyim-cchar2pinyin-get (string-to-char str))
                       (list str))
                   pinyins-list))
            ((> (length str) 0)
             (push (list str) pinyins-list))))
       string-list)
      (setq pinyins-list (nreverse pinyins-list))

      ;; 通過排列組合的方式，重排 pinyins-list。
      ;; 比如：(("Hello") ("yin") ("hang" "xing")) -> (("Hello" "yin" "hang") ("Hello" "yin" "xing"))
      (setq pinyins-list-permutated (pyim-permutate-list2 pinyins-list))

      ;; 使用 pyim 的安裝的詞庫來校正多音字。
      (when adjust-duo-yin-zi
        ;; 確保 pyim 詞庫加載
        (pyim-dcache-init-variables)
        (dolist (pinyin-list pinyins-list-permutated)
          (let* ((py-str (mapconcat #'identity pinyin-list "-"))
                 (words-from-dicts
                  ;; pyim-buffer-list 中第一個 buffer 對應的是個人詞庫文件
                  ;; 個人詞庫文件中的詞條，極有可能存在 *多音字污染*。
                  ;; 這是由 pyim 保存詞條的機制決定的。
                  (pyim-dcache-get py-str '(code2word))))
            (when (member string words-from-dicts)
              (push pinyin-list pinyins-list-adjusted))))
        (setq pinyins-list-adjusted
              (nreverse pinyins-list-adjusted)))

      ;; 返回拼音字元串或者拼音列表
      (let* ((pinyins-list
              (or pinyins-list-adjusted
                  pinyins-list-permutated))
             (list (mapcar
                    #'(lambda (x)
                        (mapconcat
                         #'(lambda (str)
                             (if shou-zi-mu
                                 (substring str 0 1)
                               str))
                         x separator))
                    (if ignore-duo-yin-zi
                        (list (car pinyins-list))
                      pinyins-list))))
        (if return-list
            list
          (mapconcat #'identity list " "))))))

(defun pyim-permutate-list (list)
  "使用排列組合的方式重新排列 LIST。
這個函數由 ‘二中’ 提供，`pyim-hanzi2pinyin' 沒有使用這個函數
(速度稍微有點慢)。"
  (let ((list-head (car list))
        (list-tail (cdr list)))
    (cond ((null list-tail)
           (cl-loop for element0 in list-head
                    append (cons (cons element0 nil) nil)))
          (t (cl-loop for element in list-head
                      append (mapcar (lambda (l) (cons element l))
                                     (pyim-permutate-list list-tail)))))))

(defun pyim-permutate-list2 (list)
  "使用排列組合的方式重新排列 LIST。
這個函數由 ’翀/ty‘ 提供，`pyim-hanzi2pinyin' 預設使用這個函數。"
  (if (= (length list) 1)
      (mapcar #'list (car list))
    (pyim-permutate-list2-internal (car list) (cdr list))))

(defun pyim-permutate-list2-internal (one two)
  "`pyim-permutate-list2' 的內部函數。"
  (let (return)
    (if (null (car two))
        one
      (dolist (x1 one)
        (dolist (x2 (car two))
          (push (if (listp x1)
                    (append x1 (list x2))
                  (list x1 x2))
                return)))
      (setq one return)
      (pyim-permutate-list2-internal one (cdr two)))))

;;;###autoload
(defun pyim-hanzi2pinyin-simple (string &optional shou-zi-mu separator return-list)
  "簡化版的 `pyim-hanzi2pinyin'，不處理多音字。"
  (pyim-hanzi2pinyin string shou-zi-mu separator return-list t))

;; ** pyim 詞庫管理工具
(defvar pyim-dm-buffer "*pyim-dict-manager*")

(defun pyim-dm-refresh ()
  "Refresh the contents of the *pyim-dict-manager* buffer."
  (interactive)
  (with-current-buffer pyim-dm-buffer
    (let ((inhibit-read-only t)
          (dicts-list pyim-dicts)
          (format-string "%-4s %-4s %-60s\n")
          (face-attr '((foreground-color . "DarkOrange2")
                       (bold . t)))
          (i 1))
      (erase-buffer)
      (insert (propertize (format format-string "序號" "啟用" "詞庫文件")
                          'face face-attr))
      (insert (propertize (format format-string
                                  "----" "----"
                                  "----------------------------------------------------------------------\n")
                          'face face-attr))
      (if (not pyim-dicts)
          (insert "拼音詞庫是 pyim 使用順手與否的關鍵。根據經驗估計：

1. 當詞庫詞條超過100萬時 (詞庫文件>20M)，pyim 選詞頻率大大降低。
2. 當詞庫詞條超過100萬時，pyim 中文輸入體驗可以達到搜狗輸入法的 80%。

想快速體驗 pyim 輸入法的用戶，可以使用 pyim-basedict：

     (require 'pyim-basedict)
     (pyim-basedict-enable)

喜歡折騰的用戶可以從下麵幾個途徑獲得 pyim 更詳細的信息。
1. 使用 `C-h v pyim-dicts' 瞭解 pyim 詞庫文件格式。
2. 瞭解如何導入其它輸入法的詞庫。
   1. 使用 package 管理器查看 pyim 包的簡介
   2. 閱讀 pyim.el 文件 Commentary
   3. 查看 pyim 在線 README：https://github.com/tumashu/pyim\n")
        (dolist (dict dicts-list)
          (let ((disable (plist-get dict :disable))
                (file (plist-get dict :file)))
            (insert (propertize (format format-string
                                        i (if disable "NO" "YES") file)
                                'id i 'disable disable 'file file)))
          (setq i (1+ i))))
      (insert (propertize "
操作命令：[A] 添加詞庫  [D] 刪除詞庫   [P] 向上移動   [N] 向下移動  [g] 刷新頁面
          [s] 保存配置  [R] 重啟輸入法 [C-c C-c] 禁用/啟用當前詞庫"
                          'face face-attr)))))

(defun pyim-dm-toggle-dict (&optional enable)
  "啟用當前行對應的詞庫。"
  (interactive)
  (when (equal (buffer-name) pyim-dm-buffer)
    (let* ((id (get-text-property (point) 'id))
           (disable (get-text-property (point) 'disable))
           (dict (cl-copy-list (nth (1- id) pyim-dicts)))
           (disable (plist-get dict :disable))
           (line (line-number-at-pos)))
      (setf (nth (1- id) pyim-dicts) (plist-put dict :disable (not disable)))
      (if (not disable)
          (message "禁用當前詞庫")
        (message "啟用當前詞庫"))
      (pyim-dm-refresh)
      (goto-char (point-min))
      (forward-line (- line 1)))))

(defun pyim-dm-delete-dict ()
  "從 `pyim-dicts' 中刪除當前行對應的詞庫信息。"
  (interactive)
  (when (equal (buffer-name) pyim-dm-buffer)
    (let ((id (get-text-property (point) 'id))
          (file (get-text-property (point) 'file))
          (line (line-number-at-pos)))
      (when (yes-or-no-p "確定要刪除這條詞庫信息嗎? ")
        (setq pyim-dicts (delq (nth (1- id) pyim-dicts) pyim-dicts))
        (pyim-dm-refresh)
        (goto-char (point-min))
        (forward-line (- line 1))))))

(defun pyim-dm-dict-position-up ()
  "向上移動詞庫。"
  (interactive)
  (when (equal (buffer-name) pyim-dm-buffer)
    (let* ((id (get-text-property (point) 'id))
           (dict1 (nth (- id 1) pyim-dicts))
           (dict2 (nth (- id 2) pyim-dicts))
           (length (length pyim-dicts))
           (line (line-number-at-pos)))
      (when (> id 1)
        (setf (nth (- id 1) pyim-dicts) dict2)
        (setf (nth (- id 2) pyim-dicts) dict1)
        (pyim-dm-refresh)
        (goto-char (point-min))
        (forward-line (- line 2))))))

(defun pyim-dm-dict-position-down ()
  "向下移動詞庫。"
  (interactive)
  (when (equal (buffer-name) pyim-dm-buffer)
    (let* ((id (get-text-property (point) 'id))
           (dict1 (nth (- id 1) pyim-dicts))
           (dict2 (nth id pyim-dicts))
           (length (length pyim-dicts))
           (line (line-number-at-pos)))
      (when (< id length)
        (setf (nth (1- id) pyim-dicts) dict2)
        (setf (nth id pyim-dicts) dict1)
        (pyim-dm-refresh)
        (goto-char (point-min))
        (forward-line line)))))

(defun pyim-dm-save-dict-info ()
  "使用 `customize-save-variable' 函數將 `pyim-dicts' 保存到 '~/.emacs' 文件中。"
  (interactive)
  ;; 將`pyim-dict'的設置保存到emacs配置文件中。
  (customize-save-variable 'pyim-dicts pyim-dicts)
  (message "將 pyim 詞庫配置信息保存到 ~/.emacs 文件。"))

(defun pyim-dm-add-dict ()
  "為 `pyim-dicts' 添加詞庫信息。"
  (interactive)
  (when (equal (buffer-name) pyim-dm-buffer)
    (let ((line (line-number-at-pos))
          dict name file coding first-used dict-type)
      (setq name (read-from-minibuffer "請輸入詞庫名稱："))
      (setq file (read-file-name "請選擇詞庫文件：" "~/"))
      (setq first-used  (yes-or-no-p "是否讓 pyim 優先使用詞庫？ "))
      (setq dict `(:name ,name :file ,file))
      (if first-used
          (add-to-list 'pyim-dicts dict)
        (add-to-list 'pyim-dicts dict t))
      (pyim-dm-refresh)
      (goto-char (point-min))
      (forward-line (- line 1)))))

(define-derived-mode pyim-dm-mode special-mode "pyim-dicts-manager"
  "Major mode for managing pyim dicts"
  (read-only-mode)
  (define-key pyim-dm-mode-map (kbd "D") 'pyim-dm-delete-dict)
  (define-key pyim-dm-mode-map (kbd "g") 'pyim-dm-refresh)
  (define-key pyim-dm-mode-map (kbd "A") 'pyim-dm-add-dict)
  (define-key pyim-dm-mode-map (kbd "N") 'pyim-dm-dict-position-down)
  (define-key pyim-dm-mode-map (kbd "P") 'pyim-dm-dict-position-up)
  (define-key pyim-dm-mode-map (kbd "s") 'pyim-dm-save-dict-info)
  (define-key pyim-dm-mode-map (kbd "C-c C-c") 'pyim-dm-toggle-dict)
  (define-key pyim-dm-mode-map (kbd "R") 'pyim-restart))

;;;###autoload
(defun pyim-dicts-manager ()
  "pyim 詞庫管理器。

使用這個詞庫管理器可以方便的執行下列命令：
1. 添加詞庫。
2. 刪除詞庫。
3. 向上和向下移動詞庫。
4. 保存詞庫設置。
5. 重啟輸入法。"
  (interactive)
  (let ((buffer (get-buffer-create pyim-dm-buffer)))
    (pyim-dm-refresh)
    (switch-to-buffer buffer)
    (pyim-dm-mode)
    (setq truncate-lines t)))

(defun pyim-extra-dicts-add-dict (new-dict)
  "添加 `new-dict' 到 `pyim-extra-dicts'。

其中 NEW-DICT 的格式為：

   (:name \"XXX\" :file \"/path/to/XXX.pyim\")

這個函數用於製作 elpa 格式的詞庫 ，不建議普通用戶使用。"
  (let (replace result)
    (dolist (dict pyim-extra-dicts)
      (if (equal (plist-get dict :name)
                 (plist-get new-dict :name))
          (progn (push new-dict result)
                 (setq replace t))
        (push dict result)))
    (setq result (reverse result))
    (setq pyim-extra-dicts
          (if replace result `(,@result ,new-dict)))
    (message "Add pyim dict %S to `pyim-extra-dicts'." (plist-get new-dict :name))
    t))

(defun pyim-dict-name-available-p (dict-name)
  "查詢 `pyim-dicts' 中 `:name' 為 DICT-NAME 的詞庫信息是否存在。
這個函數主要用於詞庫 package。"
  (cl-some (lambda (x)
             (let ((name (plist-get x :name)))
               (equal name dict-name)))
           pyim-dicts))

(defun pyim-dict-file-available-p (dict-file)
  "查詢 `pyim-dicts' 中 `:file' 為 DICT-FILE 的詞庫信息是否存在。
這個函數主要用於詞庫 package。"
  (cl-some (lambda (x)
             (let ((file (plist-get x :file)))
               (equal (expand-file-name file)
                      (expand-file-name dict-file))))
           pyim-dicts))

;; ** pyim 探針程式
(require 'pyim-probe)

;; * Footer
(provide 'pyim)

;;; pyim.el ends here
