import os
import sys
import subprocess
import json

import s00_const as const
import s00_util as util
import s00_conf as conf



if __name__ == '__main__':
    step = 2

    cnf = json.load(open(const.f_cnf,'r'))
    util.adjust_config(cnf)

    f_conf, f_report = util.get_f_conf(step)

    print(f_conf)
    fp = open(f_conf,'w')
    fp.write(conf.head(f_report))
    fp.write(conf.makeGridData.block_gs(cnf['OGCM'], step))
    fp.write(conf.remap.block_options(cnf['options']))
    fp.close()
