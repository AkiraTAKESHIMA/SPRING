import os
import sys
import subprocess
import json
import argparse

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

    exec_remap(f_conf, dataName, landType, 'all')


def make_rt_tiled(cnf, dataName, landType, tileName):
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

    exec_remap(f_conf, dataName, landType, tileName)


def exec_remap(f_conf, dataName, landType, tileName):
    pc = subprocess.run([const.prog_remap, f_conf], 
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                        encoding='utf-8')
    f_log = f'{lconst.dir_log[2]}/{dataName}/{landType}/{tileName}.out'
    f_err = f'{lconst.dir_log[2]}/{dataName}/{landType}/{tileName}.err'
    os.makedirs(os.path.dirname(f_log), exist_ok=True)
    print(f_log)
    print(f_err)
    with open(f_log, 'w') as fp:
        fp.write(pc.stdout)
    with open(f_err, 'w') as fp:
        fp.write(pc.stderr)


def run_():
    lst_dataName = ['GLCNMO', 'GTOPO30', 'HWSD', 'ISLSCP1', 'JRA55', 'MODIS']

    cnf = json.load(open('conf.json','r'))
    lutil.adjust_settings(cnf)

    for dataName in lst_dataName:
        if dataName != 'MODIS': continue
        print(dataName)

        if 'f_list_tiles' in cnf[dataName].keys():
            f_list_tiles = os.path.join(cnf[dataName]['dir'], cnf[dataName]['f_list_tiles'])
            if not os.path.isfile(f_list_tiles):
                raise Exception(f'File not found: {f_list_tiles}'+\
                              '\nCheck the values in the setting file.')
            for landType in cnf['landType']:
                for line in open(f_list_tiles, 'r').readlines():
                    tileName = line.strip()
                    if landType != 'river' or tileName != 'h17v03': continue
                    print(f'{dataName} {landType} {tileName}')
                    make_rt_tiled(cnf, dataName, landType, tileName)

        else:
            for landType in cnf['landType']:
                make_rt_untiled(cnf, dataName, landType)


def run(dataName_run=None, landType_run=None, tileName_run=None):
    lst_dataName = ['GLCNMO', 'GTOPO30', 'HWSD', 'ISLSCP1', 'JRA55', 'MODIS']

    cnf = json.load(open('conf.json','r'))
    lutil.adjust_settings(cnf)

    if dataName_run is not None and dataName_run not in lst_dataName:
        raise Exception(f'Data name "{dataName}" is invalid.  Valid data names: {lst_dataName}')

    if landType_run is not None and landType_run not in cnf['landType']:
        raise Exception(f'Land type "{landType_run}" is invalid. Check the value of "landType" '\
                        'in the configuration file: conf.json')

    for dataName in lst_dataName:
        if dataName_run is not None and dataName != dataName_run: continue

        if 'f_list_tiles' in cnf[dataName].keys():
            f_list_tiles = os.path.join(cnf[dataName]['dir'], cnf[dataName]['f_list_tiles'])
            if not os.path.isfile(f_list_tiles):
                raise Exception(f'File not found: {f_list_tiles}'+\
                              '\nCheck the values in the setting file.')

            for landType in cnf['landType']:
                if landType_run is not None and landType != landType_run: continue

                lst_tileName = [line.strip() for line in open(f_list_tiles, 'r').readlines()]
                if tileName_run is not None and tileName_run not in lst_tileName:
                    raise Exception(f'Tile name "{tileName_run}" is invalid. '+\
                                    f'Check the list of tiles: {f_list_tiles}')
                for tileName in [line.strip() for line in open(f_list_tiles, 'r').readlines()]:
                    if tileName_run is not None and tileName != tileName_run: continue

                    print(f'{dataName} {landType} {tileName}')
                    make_rt_tiled(cnf, dataName, landType, tileName)

        else:
            if tileName_run is not None:
                raise Exception(f'Tile name was given but the data "{dataName}" are untiled.')

            for landType in cnf['landType']:
                if landType_run is not None and landType != landType_run: continue

                print(f'{dataName} {landType}')
                make_rt_untiled(cnf, dataName, landType)



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--data', help='specify data name')
    parser.add_argument('-l', '--land', help='specify land name')
    parser.add_argument('-t', '--tile', help='specify tile name')
    args = parser.parse_args()

    run(args.data, args.land, args.tile)
