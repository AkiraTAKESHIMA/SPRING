import os
import sys
import shutil
import subprocess
import json

sys.path.append('../../../common')
import const, util

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


if __name__ == '__main__':
    step = int(sys.argv[0][1:3])

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)
    os.makedirs(lconst.dir_out[step], exist_ok=True)

    #
    # Prepare directories and input data
    #
    dir_flwdir = os.path.join(lconst.dir_tmp[step], '1min_flwdir')
    if os.path.isdir(dir_flwdir):
        shutil.rmtree(dir_flwdir)
    os.makedirs(dir_flwdir, exist_ok=True)

    for var in ['flwdir', 'elevtn']:
        org = os.path.join(os.path.abspath(cnf['FLOW']['dir']), cnf['FLOW'][var])
        dst = os.path.join(dir_flwdir, f'{var}.bin')
        os.symlink(org, dst)

    org = os.path.join(os.path.abspath(lconst.dir_tmp[lutil.istep('rasterize_OGCM')]), 'mask.bin')
    dst = os.path.join(dir_flwdir, 'lndmsk.bin')
    os.symlink(org, dst)

    dir_gcmmap = os.path.join(lconst.dir_tmp[step], 'gcmmap')
    os.makedirs(dir_gcmmap, exist_ok=True)

    dir_tmp = os.path.join(lconst.dir_tmp[step], 'tmp')
    os.makedirs(os.path.join(dir_tmp, '1min'), exist_ok=True)
    os.makedirs(os.path.join(dir_tmp, 'map/1min'), exist_ok=True)

    dir_map = os.path.join(lconst.dir_tmp[step], 'map')
    os.makedirs(dir_map, exist_ok=True)

    #
    # Make a setting file for FLOW
    #
    f_params = os.path.join(lconst.dir_tmp[step], 'params.txt')
    fp = open(f_params,'w')
    fp.write(f'\
{cnf["RM"]["ncx"]}  ! nXX\n\
{cnf["RM"]["ncy"]}  ! nYY\n\
""  ! fgcmidx\n\
{cnf["options"]["earth"]["shape"]}  ! earth\'s shape\n\
{cnf["options"]["earth"]["r"]}  ! earth\'s diameter [m]\n\
0.d0  ! square of the earth\'s eccentricity\n')
    fp.close()

    #
    # Run FLOW
    #
    dir_FLOW_src = os.path.join(os.path.abspath(cnf['dir_top']), 'etc/FLOW_free/src')
    os.chdir(lconst.dir_tmp[step])

    for i, prog in enumerate([
      'make_gcmmap',
      'modify_hires', 
      'calc_uparea',
      'const_network',
      'define_catchment',
      'visual_check',
      'gcm_rivermap',
      'set_map',
      'calc_inpmat',
    ]):
        print(f'Executing {prog}...')
        f_log = f'{lconst.dir_log[step]}/{i+1:02d}_{prog}.out'
        f_err = f'{lconst.dir_log[step]}/{i+1:02d}_{prog}.err'
        util.exec_program(prog, '', f_log, f_err)
