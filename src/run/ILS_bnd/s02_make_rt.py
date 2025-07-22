import os
import sys
import copy
import subprocess
import json
import argparse

import const, util, conf

import s00_const as lconst
import s00_util as lutil


def make_rt_untiled(cnf, step, dataName, landType):
    tileName = 'global'
    dir_tmp = f'{lconst.dir_tmp[step]}/{dataName}'

    f_conf = f'{lconst.dir_set[step]}/{dataName}/{tileName}_{landType}.conf'
    print(f'config: {f_conf}')
    fp = open(f_conf, 'w')
    fp.write(conf.remap.head(dir_tmp, f'report_{tileName}_{landType}'))
    fp.write(conf.remap.block_gs(cnf[dataName]))
    fp.write(conf.remap.block_gs(cnf[f'MATSIRO_{landType}']))
    fp.write(conf.remap.block_remapping(cnf['remapping'], dir_tmp,
               fname_rt_grid=f'mapping_table_idx_{tileName}_{landType}',
               fname_rt_area=f'mapping_table_area_{tileName}_{landType}',
               fname_rt_coef=f'mapping_table_coef_{tileName}_{landType}'))
    fp.write(conf.remap.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/{dataName}/{tileName}_{landType}.out'
    f_err = f'{lconst.dir_log[step]}/{dataName}/{tileName}_{landType}.err'
    util.exec_program(const.prog_remap, f_conf, f_log, f_err)


def make_rt_tiled(cnf, step, dataName, landType, tileName):
    mat = copy.deepcopy(cnf[f'MATSIRO_{landType}'])
    dat = copy.deepcopy(cnf[dataName])

    dat['name'] = dat['name'] + '_' + tileName
    if dat['type'] == 'latlon':
        west, east, south, north = lutil.get_tile_bbox_latlon(tileName)
        dat['west']  = west
        dat['east']  = east
        dat['south'] = south
        dat['north'] = north
    elif dat['type'] == 'polygon':
        dat['f_lon_vertex']['path'] = dat['f_lon_vertex']['path'].format(tilename=tileName)
        dat['f_lat_vertex']['path'] = dat['f_lat_vertex']['path'].format(tilename=tileName)
        lon = util.read_bin(dat['f_lon_vertex'], dat['dir'], dat['coord_miss'])
        lat = util.read_bin(dat['f_lat_vertex'], dat['dir'], dat['coord_miss'])
        west, east, south, north = lon.min(), lon.max(), lat.min(), lat.max()
    else:
        raise Exception(f'Invalid value in `dat["type"]`: {dat["type"]}')

    dxi, dxf, dyi, dyf = lutil.get_raster_bounds(mat, west, east, south, north)
    mat['dxi'] = dxi
    mat['dxf'] = dxf
    mat['dyi'] = dyi
    mat['dyf'] = dyf

    dir_tmp = f'{lconst.dir_tmp[step]}/{dataName}'

    f_conf = f'{lconst.dir_set[step]}/{dataName}/{tileName}_{landType}.conf'
    print(f'config: {f_conf}')
    fp = open(f_conf, 'w')
    fp.write(conf.remap.head(dir_tmp, f'report_{tileName}_{landType}'))
    fp.write(conf.remap.block_gs(dat))
    fp.write(conf.remap.block_gs(mat))
    fp.write(conf.remap.block_remapping(cnf['remapping'], dir_tmp,
               fname_rt_grid=f'mapping_table_idx_{tileName}_{landType}',
               fname_rt_area=f'mapping_table_area_{tileName}_{landType}',
               fname_rt_coef=f'mapping_table_coef_{tileName}_{landType}'))
    fp.write(conf.remap.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/{dataName}/{tileName}_{landType}.out'
    f_err = f'{lconst.dir_log[step]}/{dataName}/{tileName}_{landType}.err'
    util.exec_program(const.prog_remap, f_conf, f_log, f_err)


def mkdir(step, dataName, landType):
    dir_set = f'{lconst.dir_set[step]}/{dataName}'
    dir_tmp = f'{lconst.dir_tmp[step]}/{dataName}'
    dir_log = f'{lconst.dir_log[step]}/{dataName}'
    os.makedirs(dir_set, exist_ok=True)
    os.makedirs(dir_tmp, exist_ok=True)
    os.makedirs(dir_log, exist_ok=True)


def driv_make_rt(cnf, step, dataName_run, landType_run, tileName_run):
    lst_dataName = ['GLCNMO', 'GTOPO30', 'HWSD', 'ISLSCP1', 'JRA55', 'MODIS']

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
            mkdir(step, dataName, landType)

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
                    make_rt_tiled(cnf, step, dataName, landType, tileName)

        else:
            if tileName_run is not None:
                raise Exception(f'Tile name was given but the data "{dataName}" are untiled.')

            for landType in cnf['landType']:
                if landType_run is not None and landType != landType_run: continue

                print(f'{dataName} {landType}')
                make_rt_untiled(cnf, step, dataName, landType)

        # Copy data or make symbolic links
        util.make_slink(f'{lconst.dir_tmp[step]}/{dataName}',
                        f'{const.dir_out}/remapping_table/{dataName}')
            

def run():
    step = int(__name__.split('.')[-1][1:3])

    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--data', help='data name')
    parser.add_argument('-l', '--land', help='land type')
    parser.add_argument('-t', '--tile', help='tile name')
    args = parser.parse_args()

    cnf = json.load(open(lconst.f_cnf, 'r'))
    lutil.adjust_config(cnf)

    driv_make_rt(cnf, step, args.data, args.land, args.tile)
