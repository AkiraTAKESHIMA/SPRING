import os
import sys
import subprocess
import json

import s00_const as const
import s00_util as util
import s00_conf as conf



if __name__ == '__main__':
    step = 6

    cnf = json.load(open('test.json','r'))
    util.adjust_config(cnf)

    for landType in cnf['landType']:
        f_conf, f_report = util.get_f_conf(step, landType)

        print(f_conf)
        fp = open(f_conf,'w')
        fp.write(conf.head(f_report))
        fp.write(conf.makeGridData.block_gs(cnf['RM'], step, landType))
        fp.write(conf.remap.block_options(cnf['options']))
        fp.close()
