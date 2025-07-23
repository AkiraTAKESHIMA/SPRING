import os
import sys
import shutil
import subprocess
import numpy as np

import const


class Job():
    def __init__(self):
        self.job = None

    def put_job(self, job):
        self.job = job

    def get_job(self, job):
        job = self.job

job = Job()


def exec_program(prog, f_conf, f_log, f_err):
    pc = subprocess.run([prog, f_conf],
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                       encoding='utf-8')

    with open(f_log, 'w') as fp:
        fp.write(pc.stdout)
    with open(f_err, 'w') as fp:
        fp.write(pc.stderr)
    print(f'stdout: {f_log}')
    print(f'stderr: {f_err}')

    if pc.returncode != 0:
        #raise Exception(f'Program `{prog}` failed.')
        print(f'*** Program `{prog}` failed.')


def make_slink(org, dst):
    if not (os.path.isfile(org) or os.path.isdir(org)):
        raise Exception(f'File or directory not found: {org}')
    if os.path.isfile(dst) or os.path.islink(dst):
        os.remove(dst)
    elif os.path.isdir(dst):
        shutil.rmtree(dst)

    os.makedirs(os.path.dirname(dst), exist_ok=True)

    cdir = os.getcwd()
    absorg = os.path.join(cdir, org)
    absdst = os.path.join(cdir, dst)
    pc = subprocess.run([const.command_ln] + const.options_ln.strip().split() + \
                        [absorg, absdst],
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                        encoding='utf-8')
    if pc.returncode != 0:
        command = f'{const.command_ln} {const.options_ln} {absorg} {absdst}'
        raise Exception(f'Failed: \n{command}')

    print(f'linked: {dst} -> {org}')


def istep(name):
    if job.job is None:
        raise Exception('`job.job` is undefined.')

    for key in job.job.keys():
        if job.job[key] == name:
            return key
    raise Exception(f'Invalid value in $name: {name}')


def join_topdir(cnf, dir_top):
    for key in cnf.keys():
        if type(cnf[key]) is dict:
            cnf[key] = join_topdir(cnf[key], dir_top)
        elif type(cnf[key]) is str:
            if key == 'dir':
                cnf[key] = os.path.join(dir_top, cnf[key])
            elif key == '_dir':
                if 'dir' in cnf[key].keys():
                    raise Exception('"_dir" and "dir" cannot be given for one data.')
                cnf[key]['dir'] = cnf[key]['_dir']
    return cnf


def key_val_exist(dct, key):
    if key not in dct.keys():
        return False
    elif dct[key] is None:
        return False
    else:
        return True


def get_meshBaseName(meshName):
    if 'landType' in meshName:
        return meshName[:meshName.index('landType')-2]
    else:
        return meshName


def get_tile_bbox_latlon(tileName):
    if tileName[0] not in ['W','E'] or tileName[4] not in ['N','S'] or len(tileName) != 7:
        raise Exception(('Unexpected format of tileName: {}'\
                       '\ntileName must be like "W140N40" or "E010S20".').format(tileName))

    if tileName[0] == 'W':
        west = -int(tileName[1:4])
    else:
        west = int(tileName[1:4])
    east = west + 10

    if tileName[4] == 'N':
        north = int(tileName[5:7])
    else:
        north = -int(tileName[5:7])
    south = north - 10

    return west, east, south, north


def get_raster_bounds(
      west, east, south, north, mx_1deg, my_1deg,
      twest, teast, tsouth, tnorth):
    dxi = int(np.floor((twest-west) * mx_1deg+1))
    dxf = int(np.ceil((teast-west) * mx_1deg))
    dyi = int(np.floor((north-tnorth) * my_1deg+1))
    dyf = int(np.ceil((north-tsouth) * my_1deg))

    return dxi, dxf, dyi, dyf


def get_nij(f_rt_idx, dtype):
    if os.path.getsize(f_rt_idx) % (byte(dtype)*2) != 0:
        raise Exception('Invalid file size.')
    return int(os.path.getsize(f_rt_idx) / (byte(dtype)*2))


def set_gs_dir(gs, dir_top):
    if 'dir' in gs.keys():
        gs['dir'] = os.path.join(dir_top, gs['dir'])


def set_dir(pdir, job):
    d = {}
    for step in job.keys():
        d[step] = os.path.join(pdir, f'{step:02d}_{job[step]}')
    return d


def read_bin(f, d='', miss=None):
    if miss is None:
        return np.fromfile(os.path.join(d, f['path']), dtype_str_to_np(f['dtype']))
    else:
        return np.ma.masked_equal(
                 np.fromfile(os.path.join(d, f['path']), dtype_str_to_np(f['dtype'])), 
                 miss)


def file_bin(path, dtype=None, endian=None, rec=None):
    f = {'path': path}
    if dtype is not None:
        f['dtype'] = dtype
    if endian is not None:
        f['endian'] = str_endian(endian)
    if rec is not None:
        f['rec'] = rec
    return f


def dtype_str_to_np(dtype_str):
    if dtype_str == const.str_dtype_int1:
        return np.int8
    elif dtype_str == const.str_dtype_int2:
        return np.int16
    elif dtype_str == const.str_dtype_int4:
        return np.int32
    elif dtype_str == const.str_dtype_int8:
        return np.int64
    elif dtype_str == const.str_dtype_real:
        return np.float32
    elif dtype_str == const.str_dtype_dble:
        return np.float64


def str_endian(s):
    if s in [const.str_endian_little_long, 
             const.str_endian_little_short]:
        return const.str_endian_little_long
    elif s in [const.str_endian_big_long,
               const.str_endian_big_short]:
        return const.str_endian_big_long
    else:
        raise Exception(f'Invalid value in $s: {s}')


def get_endian(dct):
    if 'endian' in dct.keys():
        return dct['endian']
    else:
        return ''


def str_file_bin(f):
    if 'path' not in f.keys():
        raise Exception('"path" was not given for the file.')
    s = f'"{f["path"]}"'
    if 'dtype' in f.keys():
        s += f', dtype={f["dtype"]}'
    if 'rec' in f.keys():
        s += f', rec={f["rec"]}'
    if 'endian' in f.keys():
        s += f', endian={f["endian"]}'

    return s


def byte(dtype):
    if dtype == const.str_dtype_int1:
        b = 1
    elif dtype == const.str_dtype_int2:
        b = 2
    elif dtype == const.str_dtype_int4:
        b = 4
    elif dtype == const.str_dtype_int8:
        b = 8
    elif dtype == const.str_dtype_real:
        b = 4
    elif dtype == const.str_dtype_dble:
        b = 8
    else:
        raise Exception(f'Invalid value in $dtype: {dtype}')

    return b

