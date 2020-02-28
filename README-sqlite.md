
* 编译emacs-sqlite3 module
编译时需要mingw64。安装 libsqlite 和 开发包。
#+begin_src sh
pacman -S libsqlite libsqlite-devel
#+end_src

clone https://github.com/syohex/emacs-sqlite3，编译emacs-sqlite3。编译后的动态链接库在windows上需要修改扩展名为dll。
#+begin_src sh
git clone https://github.com/syohex/emacs-sqlite3.git
cd emacs-sqlite3 && make && make test
cp sqlite3-core.so sqlite3-core.dll
#+end_src

调用关系：
pyim -> emacs-sqlite3 -> sqlite3-core.dll(emacs的动态链接库) -> libsqlite3.dll(on windows)

* 设置sqlite3的环境变量
由于使用了emacs 的module，需要设置环境变量 LIB_LIBRARY=/path/to/ emacs才能找到sqlite3-core.dll

#+begin_src lisp
(setq pyim-dcache-backend 'pyim-sqlite)
#+end_src

* pyim 使用的数据库

** ~/.emacs.d/pyim/pyim_system.db
*** table
**** pyim_code2word - 系统词库
由pyim-basedict/pyim-basedict.pyim 中的8万多条词汇和通过深蓝转换的搜狗细胞词库（39万条词汇），合成一个39万条词汇的词库。

搜狗细胞词库的单字母的词汇不全，需要用pyim-basedict 中的词汇补全。

#+begin_src sql
CREATE TABLE pyim_code2word(py text primary key, cchars text)
#+end_src
       
+ py 字段 :: 词的拼音，每个字的拼音以 ="-"= 分割。表的主键。sqlite 的表要用主键或者索引，否则会全表扫描，很慢。
+ cchars 字段 :: 中的词以空格分割。

** ~/.emacs.d/pyim/pyim_user.db
*** table
**** pyim_icode2word - 用户词库。由用户输入时产生的数据生成。
#+begin_src sql
CREATE TABLE pyim_icode2word(py text primary key, cchars text)
#+end_src

**** pyim_iword2count - 用户输入的词的词频。
#+begin_src sql
CREATE TABLE pyim_iword2count(word text primary key, count int)       
#+end_src

** 生成pyim_system.db系统词库
#+begin_src sh
python sqlite/db.py
#+end_src

* pyim使用sqlite数据库后的性能
使用sqlite 的性能稍微差一点。

#+begin_src lisp
(dotimes (_ 20)
  (message "hash test1...")
  (with-timer "pyim-dhashcode-code2word" (gethash "tai-shan" pyim-dhashcache-code2word)))
#+end_src
hast-table 平均每次 get 大概0.000003s左右。

#+begin_src lisp
(dotimes (_ 20)
  (message "sql test1...")
  (with-timer "sqlite--get-code2word" (pyim-sqlite--get-code2word pyim-sqlite-systemdb "找不到"))
  (message "sql test2...")
  (with-timer "sqlite--get-code2word" (pyim-sqlite--get-code2word pyim-sqlite-systemdb "tai-shan")))
#+end_src

使用 sqlite 时，pyim-sqlite--get-code2word 函数一次调用是0.00005s，差一个数量级的。当然 pyim-sqlite--get-code2word 函数查询sqlite数据库(系统词库有39条纪录，而hash-table 中只有10万条数据，当然 =hash-table= 只要key不冲突，get 的速度和数据量关系不大) ，而sqlite数据库可以使用内存数据库或者放到 =/dev/shm= 中的方式进行优化，速度应该可以更快一点。

