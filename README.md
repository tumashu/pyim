Note: this file is auto converted from pyim.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [pyim 使用說明](#org932a3dc)
    1.  [截圖](#orgd5b4684)
    2.  [簡介](#org1504f05)
    3.  [背景](#org2a8bcbb)
    4.  [目標](#orge7bd1f7)
    5.  [特點](#orge835dab)
    6.  [安裝](#org2fd6ffd)
    7.  [配置](#org69a3509)
        1.  [配置實例](#org5972b8d)
        2.  [新增詞庫檔案](#org21b60b9)
        3.  [啟用 pyim](#org99e38a9)
    8.  [使用](#org4e8be12)
        1.  [常用快捷鍵](#org956534d)
        2.  [使用雙拼模式](#orgd80b128)
        3.  [通過 pyim 來支援 rime 所有輸入法](#org9bf91c4)
        4.  [使用五筆輸入](#orgb157452)
        5.  [使用倉頡輸入法](#org1d12f01)
        6.  [讓選詞框跟隨游標](#orgd8518f2)
        7.  [調整 tooltip 選詞框的顯示樣式](#org0c4ca0b)
        8.  [設定模糊音](#orga4d54f5)
        9.  [使用魔術轉換器](#org80398d3)
        10. [切換全形標點與半形標點](#orgafdb56c)
        11. [手動加詞和刪詞](#org6d6e63e)
        12. [pyim 高級功能](#org0e98985)
    9.  [捐贈](#orge593807)
    10. [Tips](#org5a3abc8)
        1.  [關閉輸入聯想詞功能 (預設開啟)](#org801c5c3)
        2.  [如何將個人詞條相關資訊匯入和導出？](#org84c999d)
        3.  [pyim 出現錯誤時，如何開啟 debug 模式](#org40ad362)
        4.  [如何檢視 pyim 文件。](#orgd8fb597)
        5.  [將游標處的拼音或者五筆字串轉換為中文 (與 vimim 的 「點石成金」 功能類似)](#org4f582af)
        6.  [如何使用其它字元翻頁](#org64d0985)
        7.  [如何用 ";" 來選擇第二個候選詞](#org7d7359f)
        8.  [如何新增自定義拼音詞庫](#orgcb568bc)
        9.  [如何手動安裝和管理詞庫](#org9a81531)
        10. [Emacs 啟動時載入 pyim 詞庫](#org408b245)
        11. [將漢字字串轉換為拼音字串](#orgc264606)
        12. [中文分詞](#org1bfca77)
        13. [獲取游標處的中文詞條](#org571b25c)
        14. [讓 \`forward-word' 和 \`back-backward』 在中文環境下正常工作](#org2eb4aea)
        15. [為 isearch 相關命令新增拼音搜索支援](#org2f8f6b4)


<a id="org932a3dc"></a>

# pyim 使用說明


<a id="orgd5b4684"></a>

## 截圖

![img](./snapshots/pyim-linux-x-with-toolkit.png)


<a id="org1504f05"></a>

## 簡介

pyim 是 Emacs 環境下的一個中文輸入法，最初它只支援全拼輸入，所以當時
"pyim" 代表 "Chinese Pinyin Input Method" 的意思，後來根據同學的提議，新增了五筆的支援，再叫 「拼音輸入法」 就不太合適了，所以你現在可以將它理解為 「PengYou input method」： 平時像朋友一樣幫助你，偶爾也像朋友一樣犯二 。。。


<a id="org2a8bcbb"></a>

## 背景

pyim 的程式碼源自 emacs-eim。

emacs-eim 是 Emacs 環境下的一個中文輸入法框架， 支援拼音，五筆，倉頡以及二筆等多種輸入法，但遺憾的是，2008 年之後它就停止了開發，我認為主要原因是外部中文輸入法快速發展。

雖然外部輸入法功能強大，但不能和 Emacs 默契的配合，這一點極大的損害了 Emacs 那種 **行雲流水**
的感覺。而本人在使用（或者叫折騰） emacs-eim 的過程中發現：

1.  **當 emacs-eim 詞庫詞條超過 100 萬時，選詞頻率大大降低，中文體驗增強。**
2.  **隨著使用時間的延長，emacs-eim 會越來越好用（個人詞庫的積累）。**

於是我 fork 了 emacs-eim 輸入法的部分程式碼, 建立了一個專案：pyim。


<a id="orge7bd1f7"></a>

## 目標

pyim 的目標是： **盡最大的努力成為一個好用的 Emacs 中文輸入法** ，具體可表現爲三個方面：

1.  Fallback:     當外部輸入法不能使用時，比如在 console 或者 cygwin 環境下，盡最大可能讓 Emacs 使用者不必為輸入中文而煩惱。
2.  Integration:  盡最大可能減少輸入法切換頻率，讓中文輸入不影響 Emacs
    的體驗。
3.  Exchange:     盡最大可能簡化 pyim 使用其他優秀輸入法的詞庫的難度和複雜度。


<a id="orge835dab"></a>

## 特點

1.  pyim 支援全拼，雙拼，五筆和倉頡，其中對全拼的支援最好。
2.  pyim 通過新增詞庫的方式優化輸入法。
3.  pyim 使用文字詞庫格式，方便處理。


<a id="org2fd6ffd"></a>

## 安裝

1.  配置 melpa 源，參考：<http://melpa.org/#/getting-started>
2.  M-x package-install RET pyim RET
3.  在 Emacs 配置檔案中（比如: ~/.emacs）新增如下程式碼：

        (require 'pyim)
        (require 'pyim-basedict) ; 拼音詞庫設定，五筆使用者 *不需要* 此行設定
        (pyim-basedict-enable)   ; 拼音詞庫，五筆使用者 *不需要* 此行設定
        (setq default-input-method "pyim")


<a id="org69a3509"></a>

## 配置


<a id="org5972b8d"></a>

### 配置實例

對 pyim 感興趣的同學，可以看看本人的 pyim 配置（總是適用於最新版的 pyim）:

    (use-package pyim
      :ensure nil
      :demand t
      :config
      ;; 啟用 basedict 拼音詞庫，五筆使用者請繼續閱讀 README
      (use-package pyim-basedict
        :ensure nil
        :config (pyim-basedict-enable))

      (setq default-input-method "pyim")

      ;; 我使用全拼
      (setq pyim-default-scheme 'quanpin)

      ;; 設定 pyim 探針設定，這是 pyim 高級功能設定，可以實現 *無痛* 中英文切換 :-)
      ;; 我自己使用的中英文動態切換規則是：
      ;; 1. 游標只有在註釋裡面時，才可以輸入中文。
      ;; 2. 游標前是漢字字元時，才能輸入中文。
      ;; 3. 使用 M-j 快捷鍵，強制將游標前的拼音字串轉換為中文。
      (setq-default pyim-english-input-switch-functions
                    '(pyim-probe-dynamic-english
                      pyim-probe-isearch-mode
                      pyim-probe-program-mode
                      pyim-probe-org-structure-template))

      (setq-default pyim-punctuation-half-width-functions
                    '(pyim-probe-punctuation-line-beginning
                      pyim-probe-punctuation-after-punctuation))

      ;; 開啟拼音搜索功能
      (pyim-isearch-mode 1)

      ;; 使用 popup-el 來繪製選詞框, 如果用 emacs26, 建議設定
      ;; 為 'posframe, 速度很快並且菜單不會變形，不過需要使用者
      ;; 手動安裝 posframe 包。
      (setq pyim-page-tooltip 'popup)

      ;; 選詞框顯示5個候選詞
      (setq pyim-page-length 5)

      :bind
      (("M-j" . pyim-convert-string-at-point) ;與 pyim-probe-dynamic-english 配合
       ("C-;" . pyim-delete-word-from-personal-buffer)))


<a id="org21b60b9"></a>

### 新增詞庫檔案

pyim 目前的預設的拼音詞庫是 pyim-basedict, 這個詞庫的詞條量
8 萬左右，是一個 **非常小** 的拼音詞庫，詞條的來源有兩個：

1.  libpinyin 專案的內建詞庫
2.  pyim 使用者貢獻的個人詞庫

如果 pyim-basedict 不能滿足需求，使用者可以使用其他方式為 pyim 新增拼音詞庫，具體方式請參考 [1.10.8](#orgcb568bc) 小結。


<a id="org99e38a9"></a>

### 啟用 pyim

    (setq default-input-method "pyim")
    (global-set-key (kbd "C-\\") 'toggle-input-method)


<a id="org4e8be12"></a>

## 使用


<a id="org956534d"></a>

### 常用快捷鍵

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">輸入法快捷鍵</th>
<th scope="col" class="org-left">功能</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-n 或 M-n 或 + 或 .</td>
<td class="org-left">向下翻頁</td>
</tr>


<tr>
<td class="org-left">C-p 或 M-p 或 - 或 ,</td>
<td class="org-left">向上翻頁</td>
</tr>


<tr>
<td class="org-left">C-f</td>
<td class="org-left">選擇下一個備選詞</td>
</tr>


<tr>
<td class="org-left">C-b</td>
<td class="org-left">選擇上一個備選詞</td>
</tr>


<tr>
<td class="org-left">SPC</td>
<td class="org-left">確定輸入</td>
</tr>


<tr>
<td class="org-left">RET 或 C-m</td>
<td class="org-left">字母上屏</td>
</tr>


<tr>
<td class="org-left">C-c</td>
<td class="org-left">取消輸入</td>
</tr>


<tr>
<td class="org-left">C-g</td>
<td class="org-left">取消輸入並保留已輸入的中文</td>
</tr>


<tr>
<td class="org-left">TAB</td>
<td class="org-left">模糊音調整</td>
</tr>


<tr>
<td class="org-left">DEL 或 BACKSPACE</td>
<td class="org-left">刪除最後一個字元</td>
</tr>


<tr>
<td class="org-left">C-DEL 或  C-BACKSPACE</td>
<td class="org-left">刪除最後一個拼音</td>
</tr>


<tr>
<td class="org-left">M-DEL 或  M-BACKSPACE</td>
<td class="org-left">刪除最後一個拼音</td>
</tr>
</tbody>
</table>


<a id="orgd80b128"></a>

### 使用雙拼模式

pyim 支援雙拼輸入模式，使用者可以通過變數 \`pyim-default-scheme' 來設定：

    (setq pyim-default-scheme 'pyim-shuangpin)

注意：

1.  pyim 支援微軟雙拼（microsoft-shuangpin）和小鶴雙拼（xiaohe-shuangpin）。
2.  使用者可以使用變數 \`pyim-schemes' 新增自定義雙拼方案。
3.  使用者可能需要重新設定 \`pyim-translate-trigger-char'。


<a id="org9bf91c4"></a>

### 通過 pyim 來支援 rime 所有輸入法

1.  安裝配置 liberime 和 pyim, 方式見：[liberime](https://github.com/merrickluo/liberime).
2.  將 liberime 的 page\_size 設定為 100, 這樣 pyim 一次可以獲取 100
    個候選詞，然後自己處理分頁。使用者可以按 TAB 鍵切換到輔助輸入法來輸入 100 以後的詞條。

    手動設定方式是： 在 \`liberime-user-data-dir'/default.custom.yaml
    檔案中新增類似下面的內容：

        patch:
             "menu/page_size": 100
             "speller/auto_select": false
             "speller/auto_select_unique_candidate": false

3.  使用 rime 全拼輸入法的使用者，也可以使用 rime-quanpin scheme,
    這個 scheme 是專門針對 rime 全拼輸入法定製的，支援全拼v快捷鍵。

        (setq pyim-default-scheme 'rime-quanpin)
4.  如果通過 rime 使用微軟雙拼，可以用以下設定：

        (liberime-select-schema "double_pinyin_mspy")
        (setq pyim-default-scheme 'rime-microsoft-shuangpin)

    預設是用繁體中文，想要改成簡體中文的話，可以參考 [rime wiki](https://github.com/rime/home/wiki/CustomizationGuide#%E4%B8%80%E4%BE%8B%E5%AE%9A%E8%A3%BD%E7%B0%A1%E5%8C%96%E5%AD%97%E8%BC%B8%E5%87%BA)，或者[這篇部落格](http://wenshanren.org/?p=1070#orgc7dbd8e)


<a id="orgb157452"></a>

### 使用五筆輸入

pyim 支援五筆輸入模式，使用者可以通過變數 \`pyim-default-scheme' 來設定：

    (setq pyim-default-scheme 'wubi)

在使用五筆輸入法之前，請用 pyim-dicts-manager 新增一個五筆詞庫，詞庫的格式類似：

    # ;;; -*- coding: utf-8-unix -*-
    .aaaa 工
    .aad 式
    .aadk 匿
    .aadn 慝 葚
    .aadw 萁
    .aafd 甙
    .aaff 苷
    .aaht 芽
    .aak 戒

最簡單的方式是從 melpa 中安裝 pyim-wbdict 包，然後根據它的
[README](https://github.com/tumashu/pyim-wbdict) 來配置。

另外 Ye FeiYu 同學維護著 pyim-wbdict 的一個 fork, 裡面包含著極點五筆和清歌五筆的詞庫，不做發佈，有興趣的同學可以瞭解一下：

<https://github.com/yefeiyu/pyim-wbdict>

如果使用者在使用五筆輸入法的過程中，忘記了某個字的五筆碼，可以按 TAB
鍵臨時切換到輔助輸入法來輸入，選詞完成之後自動退出。輔助輸入法可以通過 \`pyim-assistant-scheme' 來設定。


<a id="org1d12f01"></a>

### 使用倉頡輸入法

pyim 支援倉頡輸入法，使用者可以通過變數 \`pyim-default-scheme' 來設定：

    (setq pyim-default-scheme 'cangjie)

在使用倉頡輸入法之前，請用 pyim-dicts-manager 新增一個倉頡詞庫，詞庫的格式類似：

    # ;;; -*- coding: utf-8-unix -*-
    @a 日
    @a 曰
    @aa 昌
    @aa 昍
    @aaa 晶
    @aaa 晿
    @aaah 曑

如果使用者使用倉頡第五代，最簡單的方式是從 melpa 中安裝 pyim-cangjie5dict 包，然後根據它的 [README](https://github.com/erstern/pyim-cangjie5dict) 來配置。
pyim 支援其它版本的倉頡，但需要使用者自己建立詞庫檔案。

使用者可以使用命令：\`pyim-search-word-code' 來查詢目前選擇詞條的倉頡編碼


<a id="orgd8518f2"></a>

### 讓選詞框跟隨游標

使用者可以通過下面的設定讓 pyim 在 **游標處** 顯示一個選詞框：

1.  使用 popup 包來繪製選詞框 （emacs overlay 機制）

        (setq pyim-page-tooltip 'popup)
2.  使用 posframe 來繪製選詞框

        (setq pyim-page-tooltip 'posframe)

    注意：pyim 不會自動安裝 posframe, 使用者需要手動安裝這個包，


<a id="org0c4ca0b"></a>

### 調整 tooltip 選詞框的顯示樣式

pyim 的 tooltip 選詞框預設使用 **雙行顯示** 的樣式，在一些特殊的情況下（比如：popup 顯示的菜單錯位），使用者可以使用 **單行顯示**
的樣式：

    (setq pyim-page-style 'one-line)

註：使用者可以新增函式 pyim-page-style:STYLENAME 來定義自己的選詞框格式。


<a id="orga4d54f5"></a>

### 設定模糊音

可以通過設定 \`pyim-fuzzy-pinyin-alist' 變數來自定義模糊音。


<a id="org80398d3"></a>

### 使用魔術轉換器

使用者可以將待選詞條作 「特殊處理」 后再 「上屏」，比如 「簡體轉繁體」 或者「輸入中文，上屏英文」 之類的。

使用者需要設定 \`pyim-magic-converter', 比如：下面這個例子實現，輸入 「二呆」，「一個超級帥的小夥子」 上屏 :-)

    (defun my-converter (string)
      (if (equal string "二呆")
          "「一個超級帥的小夥子」"
        string))
    (setq pyim-magic-converter #'my-converter)


<a id="orgafdb56c"></a>

### 切換全形標點與半形標點

1.  第一種方法：使用命令 \`pyim-punctuation-toggle'，全域性切換。這個命令主要用來設定變數： \`pyim-punctuation-translate-p', 使用者也可以手動設定這個變數， 比如：

        (setq pyim-punctuation-translate-p '(yes no auto))   ;使用全形標點。
        (setq pyim-punctuation-translate-p '(no yes auto))   ;使用半形標點。
        (setq pyim-punctuation-translate-p '(auto yes no))   ;中文使用全形標點，英文使用半形標點。
2.  第二種方法：使用命令 \`pyim-punctuation-translate-at-point' 只切換游標處標點的樣式。
3.  第三種方法：設定變數 \`pyim-translate-trigger-char' ，輸入變數設定的字元會切換游標處標點的樣式。


<a id="org6d6e63e"></a>

### 手動加詞和刪詞

1.  \`pyim-create-Ncchar-word-at-point 這是一組命令，從游標前提取N個漢字字元組成字串，並將其加入個人詞庫。
2.  \`pyim-translate-trigger-char' 以預設設定為例：在「我愛吃紅燒肉」后輸入「5v」 可以將「愛吃紅燒肉」這個詞條儲存到使用者個人詞庫。
3.  \`pyim-create-word-from-selection', 選擇一個詞條，執行這個命令后，就可以將這個詞條新增到個人詞庫。
4.  \`pyim-delete-word' 從個人詞庫中刪除目前高亮選擇的詞條。


<a id="org0e98985"></a>

### pyim 高級功能

1.  根據環境自動切換到英文輸入模式，使用 pyim-english-input-switch-functions 配置。
2.  根據環境自動切換到半形標點輸入模式，使用 pyim-punctuation-half-width-functions 配置。

注意：上述兩個功能使用不同的變數設定， **千萬不要搞錯** 。

1.  根據環境自動切換到英文輸入模式

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-left">探針函式</th>
    <th scope="col" class="org-left">功能說明</th>
    </tr>
    </thead>

    <tbody>
    <tr>
    <td class="org-left">pyim-probe-program-mode</td>
    <td class="org-left">如果目前的 mode 衍生自 prog-mode，那麼僅僅在字串和 comment 中開啟中文輸入模式</td>
    </tr>
    </tbody>

    <tbody>
    <tr>
    <td class="org-left">pyim-probe-org-speed-commands</td>
    <td class="org-left">解決 org-speed-commands 與 pyim 衝突問題</td>
    </tr>


    <tr>
    <td class="org-left">pyim-probe-isearch-mode</td>
    <td class="org-left">使用 isearch 搜索時，強制開啟英文輸入模式</td>
    </tr>


    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">注意：想要使用這個功能，pyim-isearch-mode 必須啟用</td>
    </tr>
    </tbody>

    <tbody>
    <tr>
    <td class="org-left">pyim-probe-org-structure-template</td>
    <td class="org-left">使用 org-structure-template 時，關閉中文輸入模式</td>
    </tr>
    </tbody>

    <tbody>
    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">1. 目前字元為中文字元時，輸入下一個字元時預設開啟中文輸入</td>
    </tr>


    <tr>
    <td class="org-left">pyim-probe-dynamic-english</td>
    <td class="org-left">2. 目前字元為其他字元時，輸入下一個字元時預設開啟英文輸入</td>
    </tr>


    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">3. 使用命令 pyim-convert-string-at-point 可以將游標前的拼音字串強制轉換為中文。</td>
    </tr>
    </tbody>
    </table>

    啟用方式：

        (setq-default pyim-english-input-switch-functions
                      '(probe-function1 probe-function2 probe-function3))

    註：上述函式列表中，任意一個函式的返回值為 t 時，pyim 切換到英文輸入模式。

2.  根據環境自動切換到半形標點輸入模式

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-left">探針函式</th>
    <th scope="col" class="org-left">功能說明</th>
    </tr>
    </thead>

    <tbody>
    <tr>
    <td class="org-left">pyim-probe-punctuation-line-beginning</td>
    <td class="org-left">行首強制輸入半形標點</td>
    </tr>
    </tbody>

    <tbody>
    <tr>
    <td class="org-left">pyim-probe-punctuation-after-punctuation</td>
    <td class="org-left">半形標點后強制輸入半形標點</td>
    </tr>
    </tbody>
    </table>

    啟用方式：

        (setq-default pyim-punctuation-half-width-functions
                      '(probe-function4 probe-function5 probe-function6))

    註：上述函式列表中，任意一個函式的返回值為 t 時，pyim 切換到半形標點輸入模式。


<a id="orge593807"></a>

## 捐贈

您可以通過小額捐贈的方式支援 pyim 的開發工作，具體方式：

1.  通過支付寶收款賬戶：tumashu@163.com
2.  通過支付寶錢包掃瞄：

    ![img](snapshots/QR-code-for-author.jpg)


<a id="org5a3abc8"></a>

## Tips


<a id="org801c5c3"></a>

### 關閉輸入聯想詞功能 (預設開啟)

    (setq pyim-enable-shortcode nil)


<a id="org84c999d"></a>

### 如何將個人詞條相關資訊匯入和導出？

1.  匯入使用命令： pyim-import
2.  導出使用命令： pyim-export


<a id="org40ad362"></a>

### pyim 出現錯誤時，如何開啟 debug 模式

    (setq debug-on-error t)


<a id="orgd8fb597"></a>

### 如何檢視 pyim 文件。

pyim 的文件隱藏在 comment 中，如果使用者喜歡閱讀 html 格式的文件，可以檢視線上文件；

<http://tumashu.github.io/pyim/>


<a id="org4f582af"></a>

### 將游標處的拼音或者五筆字串轉換為中文 (與 vimim 的 「點石成金」 功能類似)

    (global-set-key (kbd "M-i") 'pyim-convert-string-at-point)


<a id="org64d0985"></a>

### 如何使用其它字元翻頁

    (define-key pyim-mode-map "." 'pyim-page-next-page)
    (define-key pyim-mode-map "," 'pyim-page-previous-page)


<a id="org7d7359f"></a>

### 如何用 ";" 來選擇第二個候選詞

    (define-key pyim-mode-map ";"
      (lambda ()
        (interactive)
        (pyim-page-select-word-by-number 2)))


<a id="orgcb568bc"></a>

### 如何新增自定義拼音詞庫

pyim 預設沒有攜帶任何拼音詞庫，使用者可以使用下面幾種方式，獲取質量較好的拼音詞庫：

1.  第一種方式 (懶人推薦使用)

    獲取其他 pyim 使用者的拼音詞庫，比如，某個同學測試 pyim
    時建立了一個中文拼音詞庫，詞條數量大約60萬。

    <http://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim.gz>

    下載上述詞庫后，執行 \`pyim-dicts-manager' ，按照命令提示，將下載得到的詞庫檔案資訊新增到 \`pyim-dicts' 中，最後執行命令 \`pyim-restart' 或者重啟
    emacs，這個詞庫使用 \`utf-8-unix' 編碼。

2.  第二種方式 (Windows 使用者推薦使用)

    使用詞庫轉換工具將其他輸入法的詞庫轉化為pyim使用的詞庫：這裡只介紹windows平臺下的一個詞庫轉換軟體：

    1.  軟體名稱： imewlconverter
    2.  中文名稱： 深藍詞庫轉換
    3.  下載地址： <https://github.com/studyzy/imewlconverter>
    4.  依賴平臺： Microsoft .NET Framework (>= 3.5)

    使用方式：

    ![img](snapshots/imewlconverter-basic.gif)

    如果產生的詞庫詞頻不合理，可以按照下面的方式處理（非常有用的功能）：

    ![img](snapshots/imewlconverter-wordfreq.gif)

    產生詞庫后，執行 \`pyim-dicts-manager' ，按照命令提示，將轉換得到的詞庫檔案的資訊新增到 \`pyim-dicts' 中，完成後執行命令 \`pyim-restart' 或者重啟emacs。

3.  第三種方式 (Linux & Unix 使用者推薦使用)

    E-Neo 同學編寫了一個詞庫轉換工具: [scel2pyim](https://github.com/E-Neo/scel2pyim) ,
    可以將一個搜狗詞庫轉換為 pyim 詞庫。

    1.  軟體名稱： scel2pyim
    2.  下載地址： <https://github.com/E-Neo/scel2pyim>
    3.  編寫語言： C語言


<a id="org9a81531"></a>

### 如何手動安裝和管理詞庫

這裡假設有兩個詞庫檔案：

1.  /path/to/pyim-dict1.pyim
2.  /path/to/pyim-dict2.pyim

在~/.emacs檔案中新增如下一行配置。

    (setq pyim-dicts
          '((:name "dict1" :file "/path/to/pyim-dict1.pyim")
            (:name "dict2" :file "/path/to/pyim-dict2.pyim")))

注意事項:

1.  只有 :file 是 **必須** 設定的。
2.  必須使用詞庫檔案的絕對路徑。
3.  詞庫檔案的編碼必須為 utf-8-unix，否則會出現亂碼。


<a id="org408b245"></a>

### Emacs 啟動時載入 pyim 詞庫

    (add-hook 'emacs-startup-hook
              #'(lambda () (pyim-restart-1 t)))


<a id="orgc264606"></a>

### 將漢字字串轉換為拼音字串

下面兩個函式可以將中文字串轉換的拼音字串或者列表，用於 emacs-lisp
程式設計。

1.  \`pyim-hanzi2pinyin' （考慮多音字）
2.  \`pyim-hanzi2pinyin-simple'  （不考慮多音字）


<a id="org1bfca77"></a>

### 中文分詞

pyim 包含了一個簡單的分詞函式：\`pyim-cstring-split-to-list', 可以將一個中文字串分成一個詞條列表，比如：

                      (("天安" 5 7)
    我愛北京天安門 ->  ("天安門" 5 8)
                       ("北京" 3 5)
                       ("我愛" 1 3))

其中，每一個詞條列表中包含三個元素，第一個元素為詞條本身，第二個元素為詞條相對於字串的起始位置，第三個元素為詞條結束位置。

另一個分詞函式是 \`pyim-cstring-split-to-string', 這個函式將產生一個新的字串，在這個字串中，詞語之間用空格或者使用者自定義的分隔符隔開。

注意，上述兩個分詞函式使用暴力匹配模式來分詞，所以， **不能檢測出** pyim
詞庫中不存在的中文詞條。


<a id="org571b25c"></a>

### 獲取游標處的中文詞條

pyim 包含了一個簡單的命令：\`pyim-cwords-at-point', 這個命令可以得到游標處的 **英文** 或者 **中文** 詞條的 \*列表\*，這個命令依賴分詞函式：
\`pyim-cstring-split-to-list'。


<a id="org2eb4aea"></a>

### 讓 \`forward-word' 和 \`back-backward』 在中文環境下正常工作

中文詞語沒有強制用空格分詞，所以 Emacs 內建的命令 \`forward-word' 和 \`backward-word'
在中文環境不能按使用者預期的樣子執行，而是 forward/backward 「句子」 ，pyim
自帶的兩個命令可以在中文環境下正常工作：

1.  \`pyim-forward-word
2.  \`pyim-backward-word

使用者只需將其繫結到快捷鍵上就可以了，比如：

    (global-set-key (kbd "M-f") 'pyim-forward-word)
    (global-set-key (kbd "M-b") 'pyim-backward-word)


<a id="org2f8f6b4"></a>

### 為 isearch 相關命令新增拼音搜索支援

pyim 安裝后，可以通過下面的設定開啟拼音搜索功能：

    (pyim-isearch-mode 1)

注意：這個功能有一些限制，搜索字串中只能出現 「a-z」 和 「』」，如果有其他字元（比如 regexp 操作符），則自動關閉拼音搜索功能。

開啟這個功能后，一些 isearch 擴充套件有可能失效，如果遇到這種問題，只能禁用這個 Minor-mode，然後聯繫 pyim 的維護者，看有沒有法子實現相容。

使用者啟用這個 mode 后，可以使用下面的方式 **強制關閉** isearch 搜索框中文輸入（即使在 pyim 啟用的時候）。

    (setq-default pyim-english-input-switch-functions
                  '(pyim-probe-isearch-mode))
