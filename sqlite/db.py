# -*- coding: utf-8 -*-

import sqlite3
import logging

LOG_FORMAT_STR = "%(levelname)s " \
                 "[%(module)s.%(funcName)s:%(lineno)d] " \
                 "- %(message)s"
logging.basicConfig(level=logging.DEBUG,
                    format=LOG_FORMAT_STR)
log = logging.getLogger('pyim')
log.setLevel(logging.DEBUG)

# sogou词库sqlite数据库路径，327万条数据，大概26MB。
db_path = 'e://tmp//pyim_system.db'


def create_pyim_dict(db_path):
    """创建sogou词库表. py有重复值，没法做为primary key，只能建一般index。中文是否做为
primary key，要考虑pyim输入法查询是怎么使用词库的.

    @params
        str db_path: sqlite数据库文件路径
    """
    conn = sqlite3.connect(db_path)
    log.error("Opened database %s successfully" % db_path)
    c = conn.cursor()

    sql = '''CREATE TABLE pyim_code2word
(py text primary key,
 cchars text
 );'''

    c.execute(sql)

    log.error("Table PYIM_CODE2WORD created successfully")
    conn.commit()
    conn.close()


def load_single_line_dict(dict_path, dicts):
    """将词库load 到dict. 如果有重复的，覆盖掉原来的.
词库格式:py<SPC>中文<SPC>中文

    @params
        str dict_path: 词库文件路径
        dict dicts: 词库dict
    """
    count = 0
    # basedict，拼音无重复，词按空格分割
    with open(dict_path, 'r', encoding='UTF-8') as py_dict:
        for line in py_dict:
            space = line.find(" ")
            py = line[:space]
            # 行末最后的回车要取掉
            cchars = line[space+1:-1]
            if dicts.get(py):
                count = count + 1
            all_dict[py] = cchars

    # log.debug("length of all_dict: %i, %i lines added to dict" % (len(all_dict), count))
    log.debug("%i lines overwrote from %s" % (count, dict_path))
    return dicts


def load_multi_line_dict(dict_path, dicts):
    """sogou和dicts词库按拼音排重合并到一个dict. 
pyim 自带的词库是拼音 + 多个词，而sogou的词库是一行拼音对于一个词，必须在pyim 自带的词库加载后才能加载，否则
不好排重。
sogou词库导入rime的格式: py^I中文^I词频

    @params
        str dict_path: 词库文件路径
        dict dicts: 词库dict
    """
    count = 0

    # sogou词库，在加载了basedict后，拼音有重复值，词也有重复的，要排重
    with open(dict_path, 'r', encoding='UTF-8') as py_dict:
        for line in py_dict:
            values = line.split("\t")
            # 暂时不用把词频入库
            py = values[1].replace(" ", "-")
            cchars = values[0]
            # 是否在basedict中已经加入词库了
            if all_dict.get(py):
                if all_dict.get(py).find(cchars) < 0:
                    # log.debug("all_dict.get(py): " + all_dict.get(py) + ", append py: " + py + ": " + cchars)
                    dicts[py] = dicts[py] + " " + cchars
                    count = count + 1
            else:
                dicts[py] = cchars
                count = count + 1

    log.debug("%i lines loaded from %s" % (count, dict_path))
    return dicts


def insert_data(db_path, all_dict):
    """词库插入sqlite db.

    @params
        str db_path: sqlite数据库文件路径
        str all_dict: 词库字典
    """
    conn = sqlite3.connect(db_path)
    log.error("Opened database %s successfully" % db_path)
    c = conn.cursor()

    c.execute("begin;")
    for (py, cchars) in all_dict.items():
        # log.debug(py, ", ", cchars)
        c.execute('INSERT INTO pyim_code2word(py, cchars) values(?,?);',
                  [py, cchars])

    conn.commit()
    log.error("Records created successfully")
    conn.close()


def convert_single_line_dict(dict_path, new_dict_path):
    """将pyim使用的单行格式词库 转换为 rime使用的多行词库格式。
词库格式:py<SPC>中文<SPC>中文 -> rime格式：中文<tab>pinyin<tab>词频\n

    @params
        str dict_path: 词库文件路径
        str new_dict_path: rime词库路径
    """
    count = 0

    dict_header = '''---
name: luna_pinyin.basedict
version: "2020.03.01"
sort: by_weight
use_preset_vocabulary: true
# 此处为扩充词库（基本）默认链接载入的词库
#import_tables:
#    - luna_pinyin
#    - luna_pinyin.basedict
...

# 自定义词语

'''

    new_dict_file = open(new_dict_path, 'w', encoding='UTF-8')
    new_dict_file.write(dict_header)

    # basedict，拼音无重复，词按空格分割
    with open(dict_path, 'r', encoding='UTF-8') as py_dict:
        for line in py_dict:
            space = line.find(" ")
            # - 改为 空格
            py = line[:space].replace("-", " ")
            # 行末最后的回车要取掉
            words = line[space+1:-1].split(" ")
            for word in words:
                new_dict_file.write(word + "\t" + py + "\t1\n")
                # all_dict[word] = py
                count = count + 1

    new_dict_file.close()
    log.debug("%i word written to %s" % (count, new_dict_path))


# create_pyim_dict(db_path)
# all_dict = {}
# all_dict = load_single_line_dict("e://tmp//pyim-basedict.pyim", all_dict)
# all_dict = load_single_line_dict("e://tmp//pyim-greatdict.pyim", all_dict)
# all_dict = load_multi_line_dict("e://tmp//sogou.txt", all_dict)
# print("dict length: %i", len(all_dict))
# insert_data(db_path, all_dict)

# convert_single_line_dict("e://tmp//pyim-greatdict.pyim", "e://tmp//luna_pinyin.greatdict.dict.yaml")
convert_single_line_dict("e://tmp//pyim-basedict.pyim", "e://tmp//luna_pinyin.basedict.dict.yaml")

# CREATE INDEX idx_py ON PYIM_CODE2WORD_SOGOU (py);
