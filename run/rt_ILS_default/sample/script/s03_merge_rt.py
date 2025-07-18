import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util, conf

import s00_const as lconst
import s00_util as lutil


def merge_rt(srcMeshNameFmt, tgtMeshNameFmt, runname_out,
             opt_idx_duplication, grid_coef, opt_coef_sum_modify):
    dir_tmp_make_rt = f'{lconst.dir_tmp[util.istep("make_rt", lconst.job)]}'

    srcMeshName = util.get_meshBaseName(srcMeshNameFmt)
    tgtMeshName = util.get_meshBaseName(tgtMeshNameFmt)
    runname = f'{srcMeshName}_to_{tgtMeshName}'

    srcMeshName_river = srcMeshNameFmt.format(landType='river')
    srcMeshName_noriv = srcMeshNameFmt.format(landType='noriv')
    tgtMeshName_river = tgtMeshNameFmt.format(landType='river')
    tgtMeshName_noriv = tgtMeshNameFmt.format(landType='noriv')

    rtName_river = f'{srcMeshName_river}_to_{tgtMeshName_river}'
    rtName_noriv = f'{srcMeshName_noriv}_to_{tgtMeshName_noriv}'

    dir_rt_river = f'{dir_tmp_make_rt}/{rtName_river}'
    dir_rt_noriv = f'{dir_tmp_make_rt}/{rtName_noriv}'
    length_rt_river = util.get_nij(f'{dir_rt_river}/grid.bin', cnf['remapping_table']['dtype_idx'])
    length_rt_noriv = util.get_nij(f'{dir_rt_noriv}/grid.bin', cnf['remapping_table']['dtype_idx'])

    dir_tmp = f'{lconst.dir_tmp[step]}/{runname}'

    f_conf = f'{lconst.dir_set[step]}/{runname}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.merge_rt.head(dir_tmp))
    fp.write(conf.merge_rt.block_input_bgn())
    fp.write(conf.merge_rt.block_input_rt(length_rt_river, dir_rt_river, cnf['remapping_table']))
    fp.write(conf.merge_rt.block_input_rt(length_rt_noriv, dir_rt_noriv, cnf['remapping_table']))
    fp.write(conf.merge_rt.block_input_end(opt_idx_duplication=opt_idx_duplication))
    fp.write(conf.merge_rt.block_output(cnf['remapping_table'], dir_tmp, 
                                        grid_coef, opt_coef_sum_modify))
    fp.write(conf.merge_rt.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/{runname}.out'
    f_err = f'{lconst.dir_log[step]}/{runname}.err'
    util.exec_program(const.prog_merge_rt, f_conf, f_log, f_err)

    util.make_slink(f'{dir_tmp}', 
                    f'{const.dir_out}/{runname_out}')

    return


if __name__ == '__main__':
    step = int(sys.argv[0][1:3])

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    # IO_bnd to MATSIRO
    merge_rt('IO_MATSIRO_bnd_{landType}', 'MATSIRO_bnd_{landType}', 
             'IO_bnd_to_MATSIRO',
             'stop', 'target', "1.d0")

    # IO_met to MATSIRO
    merge_rt('IO_met', 'MATSIRO_{landType}', 
             'IO_met_to_MATSIRO',
             'stop', 'target', "1.d0")

    # IO_metnc to MATSIRO
    merge_rt('IO_metnc', 'MATSIRO_{landType}',
             'IO_metnc_to_MATSIRO',
             'stop', 'target', "1.d0")

    # MATSIRO to IO_row
    merge_rt('MATSIRO_latlon_{landType}', 'IO_MATSIRO_row_{landType}',
             'MATSIRO_to_IO_row',
             'stop', 'target', "1.d0")

    # MATSIRO to IO_latlon
    merge_rt('MATSIRO_{landType}', 'IO_latlon',
             'MATSIRO_to_IO_latlon',
             'stop', 'target', "1.d0")


