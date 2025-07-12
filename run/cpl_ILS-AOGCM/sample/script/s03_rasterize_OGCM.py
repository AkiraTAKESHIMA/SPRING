import os
import sys
import subprocess
import json

import s00_util as util
import s00_conf as conf



if __name__ == '__main__':
    step = 3

    cnf = json.load(open('test.json','r'))
    util.adjust_config(cnf)

    f_conf, f_report = util.get_f_conf(step)

    print(f_conf)
    fp = open(f_conf,'w')
    fp.write(conf.head(f_report))
    fp.write(conf.remap.block_gs_polygon(cnf['OGCM']))
    fp.write(conf.rasterize.block_raster(cnf['RM']))
    fp.write(conf.rasterize.block_output(step))
    fp.write(conf.remap.block_options(cnf['options']))
    fp.close()
