import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util, conf

import s00_const as lconst
import s00_util as lutil


def make_rt(cnf, srcMeshNameFmt, tgtMeshNameFmt, lst_landType,
            use_grdara_src, use_grdara_tgt, runname_out):
    for landType in lst_landType:
        srcMeshName = srcMeshNameFmt.format(landType=landType)
        tgtMeshName = tgtMeshNameFmt.format(landType=landType)
        print(f'{srcMeshName} to {tgtMeshName}')
        runname = f'{srcMeshName}_to_{tgtMeshName}'

        dir_tmp = f'{lconst.dir_tmp[step]}/{runname}'

        f_conf = f'{lconst.dir_set[step]}/{runname}.conf'
        print('config: '+f_conf)
        fp = open(f_conf, 'w')
        fp.write(conf.head(dir_tmp))
        fp.write(conf.remap.block_gs(cnf[srcMeshName], use_grdara_src))
        fp.write(conf.remap.block_gs(cnf[tgtMeshName], use_grdara_tgt))
        fp.write(conf.remap.block_remapping(cnf['remapping_table'], dir_tmp))
        fp.write(conf.remap.block_options(cnf['options']))
        fp.close()

        f_log = f'{lconst.dir_log[step]}/{runname}.out'
        f_err = f'{lconst.dir_log[step]}/{runname}.err'
        util.exec_program(const.prog_remap, f_conf, f_log, f_err)

    if runname_out is not None:
        util.make_slink(f'{dir_tmp}', f'{const.dir_out}/{runname_out}')


if __name__ == '__main__':
    step = int(sys.argv[0][1:3])

    cnf = json.load(open('conf.json','r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    # IO_bnd to MATSIRO
    make_rt(cnf, 'IO_MATSIRO_bnd_{landType}', 'MATSIRO_bnd_{landType}', 
            ['river', 'noriv'], 
            False, False, None)

    # IO_met to MATSIRO
    make_rt(cnf, 'IO_met', 'MATSIRO_{landType}', 
            ['river', 'noriv'],
            False, False, None)

    # IO_metnc to MATSIRO
    make_rt(cnf, 'IO_metnc', 'MATSIRO_{landType}',
            ['river', 'noriv'],
            False, False, None)

    # MATSIRO to CMF
    # *** Values of "area.bin" is incorrect but not used for remapping
    make_rt(cnf, 'MATSIRO_latlon_{landType}', 'CaMa-Flood_latlon_{landType}',
            ['river'],
            False, False, 'MATSIRO_to_CaMa-Flood')

    # CaMa-Flood to MATSIRO
    # *** Values of "area.bin" is incorrect but not used for remapping
    make_rt(cnf, 'CaMa-Flood_latlon_{landType}', 'MATSIRO_latlon_{landType}',
            ['river'],
            False, False, 'CaMa-Flood_to_MATSIRO')

    # MATSIRO to IO_row
    # *** Verification data for source grid is incorrect
    make_rt(cnf, 'MATSIRO_latlon_{landType}', 'IO_MATSIRO_row_{landType}',
            ['river', 'noriv'],
            False, True, None)

    # MATSIRO to IO_latlon
    make_rt(cnf, 'MATSIRO_{landType}', 'IO_latlon',
            ['river', 'noriv'],
            False, False, None)

    # CaMa-Flood to IO_row
    # *** Verification data for source grid is incorrect
    make_rt(cnf, 'CaMa-Flood_latlon_{landType}', 'IO_CaMa-Flood_row_{landType}',
            ['river'],
            False, True, 'CaMa-Flood_to_IO_row')

