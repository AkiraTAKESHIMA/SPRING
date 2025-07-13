import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const

import s00_const as lconst
import s00_util as lutil


def make_rt_untiled(cnf, dataName, landType):
    f_conf = f'{lconst.dir_set[2]}/{dataName}/{landType}/all.conf'
    os.makedirs(os.path.dirname(f_conf), exist_ok=True)
    fp = open(f_conf, 'w')
    fp.write(lutil.conf_remap_latlon(
               None, landType, dataName, 
               cnf[dataName], cnf['MATSIRO'], cnf['options']))
    fp.close()

    exec_remap(f_conf)


def make_rt_tiled(cnf, dataName, tileName, landType):
    f_conf = f'{lconst.dir_set[2]}/{dataName}/{landType}/{tileName}.conf'
    os.makedirs(os.path.dirname(f_conf), exist_ok=True)
    fp = open(f_conf, 'w')

    if dataName in ['GLCNMO', 'GTOPO30', 'HWSD', 'JRA55']:
        fp.write(lutil.conf_remap_latlon(
          tileName, landType, dataName, 
          cnf[dataName], cnf['MATSIRO'], cnf['options']))
    elif dataName in ['MODIS']:
        fp.write(lutil.conf_remap_modis(
          tileName, landType, dataName, 
          cnf[dataName], cnf['MATSIRO'], cnf['options']))
    fp.close()

    exec_remap(f_conf)


def exec_remap(f_conf):
    pc = subprocess.run([const.prog_remap, f_conf], 
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                        encoding='utf-8')
    print('stdout:')
    print(pc.stdout.strip())
    print('stderr:')
    print(pc.stderr.strip()) 


def run():
    lst_dataName = ['GLCNMO', 'GTOPO30', 'HWSD', 'ISLSCP1', 'JRA55', 'MODIS']

    cnf = json.load(open('conf.json','r'))
    lutil.adjust_settings(cnf)

    for dataName in lst_dataName:
        if dataName != 'ISLSCP1': continue
        print(dataName)

        if 'f_list_tiles' in cnf[dataName].keys():
            f_list_tiles = os.path.join(cnf[dataName]['dir'], cnf[dataName]['f_list_tiles'])
            if not os.path.isfile(f_list_tiles):
                raise Exception(f'File not found: {f_list_tiles}'+\
                              '\nCheck the values in the setting file.')
            for landType in cnf['landType']:
                for line in open(f_list_tiles, 'r').readlines():
                    tileName = line.strip()
                    if landType != 'river' or tileName != 'E130N40': continue
                    print(f'{dataName} {landType} {tileName}')
                    make_rt_tiled(cnf, dataName, tileName, landType)

        else:
            for landType in cnf['landType']:
                make_rt_untiled(cnf, dataName, landType)



if __name__ == '__main__':
    run()
