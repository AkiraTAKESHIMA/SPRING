import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


def blocks_body(pdir_in, srcMeshNameFmt, tgtMeshNameFmt, lst_landType, runname, grid_coef):
    dtype = cnf['remapping_table']['dtype_idx']
    endian = cnf['remapping_table']['endian']

    s = f'\
\n\
[input]'

    for landType in lst_landType:
        srcMeshName = srcMeshNameFmt.format(landType=landType)
        tgtMeshName = tgtMeshNameFmt.format(landType=landType)
        dir_in = f'{pdir_in}/rt_{srcMeshName}_to_{tgtMeshName}'
        nij = util.get_nij(f'{dir_in}/grid.bin', dtype)

        s += f'\
\n\
  length_rt: {nij}\n\
  dir: "{dir_in}"\n\
  f_rt_sidx: "grid.bin", {dtype}, 1, {endian}\n\
  f_rt_tidx: "grid.bin", {dtype}, 2, {endian}\n\
  f_rt_area: "area.bin", dble, 1, {endian}\n\
  f_rt_coef: "coef.bin", dble, 1, {endian}\n\
'

    s += f'\
[end]\n'

    s += f'\
\n\
[output]\n\
  grid_coef: {grid_coef}\n\
  grid_sort: target\n\
  opt_coef_sum_modify: 1.d0\n\
\n\
  dir: "{lconst.dir_tmp[step]}/{runname}"\n\
  f_rt_sidx: "grid.bin", {dtype}, 1, {endian}\n\
  f_rt_tidx: "grid.bin", {dtype}, 2, {endian}\n\
  f_rt_area: "area.bin", dble, 1, {endian}\n\
  f_rt_coef: "coef.bin", dble, 1, {endian}\n\
[end]\n'

    return s


def block_options(opt):
    s = f'\
\n\
[options]\n\
  old_files: remove\n\
[end]\n'

    return s


def merge_rt(srcMeshNameFmt, tgtMeshNameFmt, lst_landType, pdir_in, grid_coef):
    for landType in lst_landType:
        srcMeshName = srcMeshNameFmt.format(landType=landType)
        tgtMeshName = tgtMeshNameFmt.format(landType=landType)
        runname = f'rt_{srcMeshName}_to_{tgtMeshName}'
        f_rt = f'{pdir_in}/{runname}/grid.bin'
        if not os.path.isfile(f_rt):
            raise Exception(f'File not found: {f_rt}')

    def get_meshBaseName(meshName):
        if 'landType' in meshName:
            loc = meshName.index('landType')
            return meshName[:loc-2]
        else:
            return meshName

    runname = f'rt_{get_meshBaseName(srcMeshNameFmt)}_to_{get_meshBaseName(tgtMeshNameFmt)}'

    dir_tmp = f'{lconst.dir_tmp[step]}/{runname}'

    f_conf = f'{lconst.dir_set[step]}/{runname}.conf'
    print(f_conf)
    fp = open(f_conf, 'w')
    fp.write(lconf.head(dir_tmp))
    fp.write(blocks_body(pdir_in, srcMeshNameFmt, tgtMeshNameFmt, lst_landType, 
                         runname, grid_coef))
    fp.write(block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/{runname}.out'
    f_err = f'{lconst.dir_log[step]}/{runname}.err'
    util.exec_program(const.prog_merge_rt, f_conf, f_log, f_err)


if __name__ == '__main__':
    step = int(sys.argv[0][1:3])

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)
    os.makedirs(lconst.dir_out[step], exist_ok=True)

    # IO_bnd to LSM
    merge_rt('IO_LSM_bnd_{landType}', 'LSM_bnd_{landType}', 
             ['river', 'noriv'],
             lconst.dir_tmp[lutil.istep('make_rt_standard-2')], 
             'target')

    # IO_met to LSM
    #merge_rt('IO_met', 'LSM_{landType}', 
    #         ['river', 'noriv_real', 'noriv_virt'],
    #         lconst.dir_tmp[lutil.istep('make_rt_standard-2')],
    #         'target')

    # IO_metnc to LSM
    #merge_rt('IO_metnc', 'LSM_{landType}', 
    #         ['river', 'noriv_real', 'noriv_virt'],
    #         lconst.dir_tmp[lutil.istep('make_rt_standard-2')],
    #         'target')

    # AGCM to LSM
    #merge_rt('AGCM', 'LSM_{landType}', 
    #         ['river', 'noriv'], 
    #         lconst.dir_tmp[lutil.istep('define_LSM')],
    #         'target')

    # LSM to AGCM
    #merge_rt('LSM_{landType}', 'AGCM', 
    #         ['river', 'noriv'], 
    #         lconst.dir_tmp[lutil.istep('define_LSM')],
    #         'target')

    # LSM to IO_row
    #merge_rt('LSM_latlon_{landType}', 'IO_LSM_row', 
    #         ['river', 'noriv_real', 'noriv_virt'],
    #         lconst.dir_tmp[lutil.istep('make_rt_standard-2')],
    #         'target')

    # LSM to IO_latlon
    #merge_rt('LSM_{landType}', 'IO_latlon', 
    #         ['river', 'noriv_real', 'noriv_virt'],
    #         lconst.dir_tmp[lutil.istep('make_rt_standard-2')],
    #         'target')

    # LSM to OGCM via AGCM

    # LSM_noriv to OGCM via AGCM
