import os
import sys
import subprocess
import json
import argparse

sys.path.append('../../../common')
import const, util

import s00_const as lconst
import s00_util as lutil


def make_rt_untiled(cnf, dataName, landType):
    f_conf = f'{lconst.dir_set[step]}/{dataName}/{landType}/all.conf'
    print(f'config: {f_conf}')
    fp = open(f_conf, 'w')
    fp.write(lutil.conf_remap_latlon(
               None, landType, dataName, 
               cnf[dataName], cnf['MATSIRO'], cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/{dataName}/{landType}/all.out'
    f_err = f'{lconst.dir_log[step]}/{dataName}/{landType}/all.err'
    util.exec_program(const.prog_remap, f_conf, f_log, f_err)


def make_rt_tiled(cnf, dataName, landType, tileName):
    f_conf = f'{lconst.dir_set[step]}/{dataName}/{landType}/{tileName}.conf'
    print(f'config: {f_conf}')
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

    f_log = f'{lconst.dir_log[step]}/{dataName}/{landType}/{tileName}.out'
    f_err = f'{lconst.dir_log[step]}/{dataName}/{landType}/{tileName}.err'
    util.exec_program(const.prog_remap, f_conf, f_log, f_err)


def mkdir(dataName, landType):
    dir_set = f'{lconst.dir_set[step]}/{dataName}'
    dir_tmp = f'{lconst.dir_tmp[step]}/{dataName}'
    dir_log = f'{lconst.dir_log[step]}/{dataName}'
    os.makedirs(dir_set, exist_ok=True)
    os.makedirs(dir_tmp, exist_ok=True)
    os.makedirs(dir_log, exist_ok=True)


def run(dataName_run, landType_run, tileName_run):
    lst_dataName = ['GLCNMO', 'GTOPO30', 'HWSD', 'ISLSCP1', 'JRA55', 'MODIS']

    cnf = json.load(open(lconst.f_cnf, 'r'))
    lutil.adjust_settings(cnf)

    if dataName_run is not None and dataName_run not in lst_dataName:
        raise Exception(f'Data name "{dataName}" is invalid.  Valid data names: {lst_dataName}')

    if landType_run is not None and landType_run not in cnf['landType']:
        raise Exception(f'Land type "{landType_run}" is invalid. Check the value of "landType" '\
                        'in the configuration file: conf.json')

    for dataName in lst_dataName:
        if dataName_run is not None and dataName != dataName_run: continue

        # Make directories
        for landType in cnf['landType']:
            if landType_run is not None and landType != landType_run: continue
            mkdir(dataName, landType)

        # Make remapping tables
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

        # Copy data or make symbolic links
        util.make_slink(f'{lconst.dir_tmp[step]}/{dataName}/rt',
                        f'{const.dir_out}/remapping_table/{dataName}')
            

if __name__ == '__main__':
    step = int(sys.argv[0][1:3])

    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--data', help='data name')
    parser.add_argument('-l', '--land', help='land type')
    parser.add_argument('-t', '--tile', help='tile name')
    args = parser.parse_args()

    run(args.data, args.land, args.tile)
