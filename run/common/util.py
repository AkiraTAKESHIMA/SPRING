import os
import sys
import shutil
import subprocess
import numpy as np

import const


def exec_program(prog, f_conf, f_log, f_err):
    pc = subprocess.run([prog, f_conf],
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                       encoding='utf-8')

    print(f_log)
    print(f_err)
    with open(f_log, 'w') as fp:
        fp.write(pc.stdout)
    with open(f_err, 'w') as fp:
        fp.write(pc.stderr)

    if pc.returncode != 0:
        raise Exception(f'Program `{prog}` failed.')


def dtype_str_to_np(dct, dtype_np_default):
    if 'dtype' in dct.keys():
        if dct['dtype'] == const.str_dtype_int1:
            return np.int8
        elif dct['dtype'] == const.str_dtype_int2:
            return np.int16
        elif dct['dtype'] == const.str_dtype_int4:
            return np.int32
        elif dct['dtype'] == const.str_dtype_int8:
            return np.int64
        elif dct['dtype'] == const.str_dtype_real:
            return np.float32
        elif dct['dtype'] == const.str_dtype_dble:
            return np.float64
    else:
        return dtype_np_default


def str_endian(s):
    if s in [const.str_endian_little_short, 
             const.str_endian_little_long]:
        return const.str_endian_little_long
    elif s in [const.str_endian_little_short,
               const.str_endian_little_long]:
        return const.str_endian_little_long
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

