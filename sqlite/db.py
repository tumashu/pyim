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
db_path = 'e://tmp//pyim_sogou.db'


def create_pyim_dict(db_path):
    """创建sogou词库表. py有重复值，没法做为primary key，只能建一般index。中文是否做为
primary key，要考虑pyim输入法查询是怎么使用词库的.

    @params
        str db_path: sqlite数据库文件路径
    """
    db_path = 'e://tmp//pyim_sogou.db'
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


def sort_data(sogou_path, base_path):
    """sogou和base词库按拼音排重合并到一个dict. 
sogou词库导入rime的格式: py^I中文^I词频
base词库格式:py<SPC>中文<SPC>

    @params
        str sogou_path: sogou词库文件路径
        str base_path: base词库文件路径
    """
    all_dict = dict()

    # basedict，拼音无重复，词按空格分割
    with open(base_path, 'r', encoding='UTF-8') as py_dict:
        for line in py_dict:
            space = line.find(" ")
            py = line[:space]
            # 行末最后的回车要取掉
            cchars = line[space+1:-1]
            all_dict[py] = cchars

    # sogou词库，在加载了basedict后，拼音有重复值，词也有重复的，要排重
    with open(sogou_path, 'r', encoding='UTF-8') as py_dict:
        for line in py_dict:
            values = line.split("\t")
            # 暂时不用把词频入库
            py = values[1].replace(" ", "-")
            cchars = values[0]
            # 是否在basedict中已经加入词库了
            if all_dict.get(py) and all_dict.get(py).find(cchars) < 0:
                # log.debug("all_dict.get(py): " + all_dict.get(py) + ", append py: " + py + ": " + cchars)
                all_dict[py] = all_dict[py] + " " + cchars
            else:
                all_dict[py] = cchars

    log.debug("length of all_dict: %i" % len(all_dict))
    return all_dict


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


create_pyim_dict(db_path)
all_dict = sort_data("e://tmp//sogou.txt", "e://tmp//pyim-basedict.pyim")
insert_data(db_path, all_dict)

# CREATE INDEX idx_py ON PYIM_CODE2WORD_SOGOU (py);
