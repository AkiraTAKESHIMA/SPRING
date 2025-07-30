import os
import sys
import json

import const, util, conf
from const import k_lt, k_gs, k_rt, k_opt
from util import istep, file_bin

import s00_const as lconst
import s00_util as lutil


def make_grid_data(cnf, step, update_data, meshNameFmt, lst_landType):
    for landType in lst_landType:
        meshBaseName = util.get_meshBaseName(meshNameFmt)
        meshName = meshNameFmt.format(landType=landType)

        dir_tmp = f'{lconst.dir_tmp[step]}/{meshBaseName}/{landType}'

        c = cnf[k_gs][meshName]

        if 'fin_grdara' in c.keys():
            print(f'{meshName} grid data already exist.')
            util.make_slink(os.path.join(c['dir'], c['fin_grdara']['path']),
              os.path.join(lutil.get_outdir_grid(meshBaseName, landType), 'grdara.bin'))
            return

        c['fout_grdara'] = file_bin('grdara.bin')

        f_conf = f'{lconst.dir_set[step]}/{meshName}.conf'
        print('config: '+f_conf)
        fp = open(f_conf, 'w')
        fp.write(conf.make_grid_data.head(dir_tmp))
        fp.write(conf.make_grid_data.block_gs(
                   c, c['dir'], dir_tmp,
                   ['rstidx', 'grdidx'],
                   ['ara']))
        fp.write(conf.make_grid_data.block_options(cnf[k_opt]))
        fp.close()

        if update_data:
            f_log = f'{lconst.dir_log[step]}/{meshName}.out'
            f_err = f'{lconst.dir_log[step]}/{meshName}.err'
            util.exec_program(const.prog_make_grid_data, f_conf, f_log, f_err)

            util.make_slink(os.path.join(dir_tmp, c['fout_grdara']['path']),
              os.path.join(lutil.get_outdir_grid(meshBaseName, landType), 'grdara.bin'))

        c['fin_grdara'] = c['fout_grdara']
        c['fin_grdara']['path'] = os.path.join(os.getcwd(), dir_tmp, c['fin_grdara']['path'])
        del(c['fout_grdara'])


def make_grid_data_all(cnf, step, update_data):
    make_grid_data(cnf, step, update_data, 'CMF_{landType}', ['river'])
    make_grid_data(cnf, step, update_data, 'MATSIRO_{landType}', cnf[k_lt])

    for landType in ['river']:
        cin = cnf[k_gs][f'CMF_{landType}']

        c = cnf[k_gs][f'CMF_simple_{landType}']
        util.copy_dict_elem(c, cin, 'dir')
        util.copy_dict_elem(c, cin, '_dir')
        util.copy_dict_elem(c, cin, 'fin_grdidx')
        util.copy_dict_elem(c, cin, 'fin_grdara')

        c = cnf[k_gs][f'IO_CMF_row_{landType}']
        util.copy_dict_elem(c, cin, 'dir')
        util.copy_dict_elem(c, cin, '_dir')
        util.copy_dict_elem(c, cin, 'fin_grdidx')
        util.copy_dict_elem(c, cin, 'fin_grdara')

    for landType in cnf[k_lt]:
        cin = cnf[k_gs][f'MATSIRO_{landType}']

        c = cnf[k_gs][f'MATSIRO_simple_{landType}']
        util.copy_dict_elem(c, cin, 'dir')
        util.copy_dict_elem(c, cin, '_dir')
        util.copy_dict_elem(c, cin, 'fin_grdidx')
        util.copy_dict_elem(c, cin, 'fin_grdara')

        c = cnf[k_gs][f'IO_MATSIRO_row_{landType}']
        util.copy_dict_elem(c, cin, 'dir')
        util.copy_dict_elem(c, cin, '_dir')
        util.copy_dict_elem(c, cin, 'fin_grdidx')
        util.copy_dict_elem(c, cin, 'fin_grdara')


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    make_grid_data_all(cnf, step, update_data)

    util.make_new_f_cnf(step, cnf)
