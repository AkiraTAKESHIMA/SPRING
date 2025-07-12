import os
import sys
import subprocess
import json

import s00_const as const
import s00_util as util
import s00_conf as conf


def make_conf_OGCM_to_AGCM():
    for landType in ['ocean', 'land']:
        f_conf, f_report, dir_set_this, dir_tmp_this \
          = util.get_f_conf(step, child_name=f'OGCM_{landType}_to_AGCM')

        print(f_conf)
        fp = open(f_conf,'w')
        fp.write(conf.head(f_report))
        fp.write(conf.remap.block_gs(cnf['OGCM']))
        fp.write(conf.remap.block_gs(cnf['AGCM']))
        fp.write(conf.remap.block_remapping(cnf['remapping'], dir_tmp_this))
        fp.write(conf.remap.block_options(cnf['options']))
        fp.close()


def make_conf_RM_to_AGCM():
    for landType in ['river', 'noriv', 'ocean']:
        f_conf, f_report, dir_set_this, dir_tmp_this \
          = util.get_f_conf(step, child_name=f'RM_{landType}_to_AGCM')

        print(f_conf)
        fp = open(f_conf,'w')
        fp.write(conf.head(f_report))
        fp.write(conf.remap.block_gs(cnf['RM'],landType))
        fp.write(conf.remap.block_gs(cnf['AGCM']))
        fp.write(conf.remap.block_remapping(cnf['remapping'], dir_tmp_this))
        fp.write(conf.remap.block_options(cnf['options']))
        fp.close()


if __name__ == '__main__':
    step = 7

    cnf = json.load(open('test.json','r'))
    util.adjust_config(cnf)

    job = const.job[step]
    make_conf_OGCM_to_AGCM()
    make_conf_RM_to_AGCM()
