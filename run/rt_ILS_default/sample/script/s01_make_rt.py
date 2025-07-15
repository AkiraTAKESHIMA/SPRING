import os
import sys
import subprocess
import json
sys.path.append('../../../common')
import const, util


def head(srcMeshName, tgtMeshName):
    return f'\
#\n\
path_report: "{const.dir_tmp}/01_make_rt/{srcMeshName}_to_{tgtMeshName}/report.txt"\n\
'


def block_gs_model_latlon(gs, name, landType):
    fin_grdidx = util.str_file_bin(gs[f'f_grdidx_{landType}'])

    wfmt = f'\
\n\
[grid_system_latlon]\n\
  name: "{name}"\n\
  nx: {gs["ncx"]}\n\
  ny: {gs["ncy"]}\n\
  west: {gs["west"]}\n\
  east: {gs["east"]}\n\
  south: {gs["south"]}\n\
  north: {gs["north"]}\n\
  is_south_to_north: {gs["is_south_to_north"]}\n\
  dir: "{gs["dir"]}"\n\
  fin_grdidx: {fin_grdidx}\n\
[end]\n\
'

    return wfmt


def block_gs_model_raster(gs, name, landType):
    fin_rstidx = util.str_file_bin(gs[f'f_rstidx_{landType}'])
    fin_grdidx = util.str_file_bin(gs[f'f_grdidx_{landType}'])

    return f'\
\n\
[grid_system_raster]\n\
  name: "{name}"\n\
  nx: {gs["ndx"]}\n\
  ny: {gs["ndy"]}\n\
  west: {gs["west"]}\n\
  east: {gs["east"]}\n\
  south: {gs["south"]}\n\
  north: {gs["north"]}\n\
  is_south_to_north: {gs["is_south_to_north"]}\n\
  dir: "{gs["dir"]}"\n\
  fin_rstidx: {fin_rstidx}\n\
  fin_grdidx: {fin_grdidx}\n\
  in_grid_sz: {gs["ncx"]}, {gs["ncy"]}\n\
[end]\n\
'
    return f


def block_gs_io_latlon(gs, name, layer=1):
    idx_bgn = (layer-1) * gs['nx'] * gs['ny'] + 1

    return f'\
\n\
[grid_system_latlon]\n\
  name: "{name}"\n\
  nx: {gs["nx"]}\n\
  ny: {gs["ny"]}\n\
  west: {gs["west"]}\n\
  east: {gs["east"]}\n\
  south: {gs["south"]}\n\
  north: {gs["north"]}\n\
  is_south_to_north: {gs["is_south_to_north"]}\n\
  idx_bgn: {idx_bgn}\n\
[end]\n\
'


def block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm):
    return f'\
\n\
[remapping]\n\
  dir: "../tmp/01_make_rt/{srcMeshName}_to_{tgtMeshName}"\n\
  fout_rt_sidx: "grid.bin", int4, 1, big\n\
  fout_rt_tidx: "grid.bin", int4, 2, big\n\
  fout_rt_area: "area.bin", dble, 1, big\n\
  fout_rt_coef: "coef.bin", dble, 1, big\n\
\n\
  vrf_source_form: {srcVrfForm}\n\
  fout_vrf_grdidx     : "vrf/src_idx.bin", int4\n\
  fout_vrf_grdara_true: "vrf/src_val.bin", dble, 1\n\
  fout_vrf_grdara_rt  : "vrf/src_val.bin", dble, 2\n\
  fout_vrf_rerr_grdara: "vrf/src_val.bin", dble, 3\n\
  fout_vrf_grdnum     : "vrf/src_num.bin", int4\n\
\n\
  vrf_target_form: {tgtVrfForm}\n\
  fout_vrf_grdidx     : "vrf/tgt_idx.bin", int4\n\
  fout_vrf_grdara_true: "vrf/tgt_val.bin", dble, 1\n\
  fout_vrf_grdara_rt  : "vrf/tgt_val.bin", dble, 2\n\
  fout_vrf_rerr_grdara: "vrf/tgt_val.bin", dble, 3\n\
  fout_vrf_grdnum     : "vrf/tgt_num.bin", int4\n\
[end]\n\
'


def block_options(earth):
    if earth['shape'] == 'sphere':
        lines_earth = f'\
  earth_shape: {earth["shape"]}\n\
  earth_r: {earth["diameter"]}\n\
'
    elif earth['shape'] == 'ellips':
        lines_earth = f'\
  earth_shape: {earth["shape"]}\n\
  earth_r: {earth["diameter"]}\n\
  earth_e2: {earth["squre_eccentricity"]}\n\
'

    return f'\
\n\
[options]\n\
  old_files: remove\n\
{lines_earth}\
[end]\n\
'


def layer(landType):
    if landType == 'river':
        return 1
    elif landType == 'noriv':
        return 2
    else:
        raise Exception(f'Invalid value in $landType: {landType}')

#===============================================================
#
#===============================================================

def make_rt_cmf_to_io_row(f, gs_cmf, gs_io_row, earth):
    srcMeshName = 'CaMa-Flood'
    tgtMeshName = 'out_CaMa-Flood_row'
    srcVrfForm = 'index'
    tgtVrfForm = 'auto'
    landType = 'river'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_model_latlon(gs_cmf, srcMeshName, landType))
    fp.write(block_gs_io_latlon(gs_io_row, tgtMeshName))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()

    exec_remap(f)


def make_rt_cmf_to_mat_river(f, gs_cmf, gs_mat, earth):
    srcMeshName = 'CaMa-Flood'
    tgtMeshName = 'MATSIRO_river'
    srcVrfForm = 'index'
    tgtVrfForm = 'index'
    landType = 'river'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_model_latlon(gs_cmf, srcMeshName, landType))
    fp.write(block_gs_model_latlon(gs_mat, tgtMeshName, landType))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()

    exec_remap(f)


def make_rt_io_bnd_to_mat(f, gs_io_mat_bnd, gs_mat, landType, earth):
    srcMeshName = 'in_MATSIRO_bnd'
    tgtMeshName = f'MATSIRO_{landType}'
    srcVrfForm = 'auto'
    tgtVrfForm = 'index'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_io_latlon(gs_io_mat_bnd, srcMeshName, layer(landType)))
    fp.write(block_gs_model_latlon(gs_mat, tgtMeshName, landType))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()

    exec_remap(f)


def make_rt_io_metnc_to_mat(f, gs_io_metnc, gs_mat, landType, earth):
    srcMeshName = 'in_metnc'
    tgtMeshName = f'MATSIRO_{landType}'
    srcVrfForm = 'auto'
    tgtVrfForm = 'index'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_io_latlon(gs_io_metnc, srcMeshName))
    fp.write(block_gs_model_raster(gs_mat, tgtMeshName, landType))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()

    exec_remap(f)


def make_rt_io_met_to_mat(f, gs_io_met, gs_mat, landType, earth):
    srcMeshName = 'in_met'
    tgtMeshName = f'MATSIRO_{landType}'
    srcVrfForm = 'auto'
    tgtVrfForm = 'index'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_io_latlon(gs_io_met, srcMeshName))
    fp.write(block_gs_model_raster(gs_mat, tgtMeshName, landType))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()

    exec_remap(f)


def make_rt_mat_river_to_cmf(f, gs_mat, gs_cmf, earth):
    srcMeshName = 'MATSIRO_river'
    tgtMeshName = 'CaMa-Flood'
    srcVrfForm = 'index'
    tgtVrfForm = 'index'
    landType = 'river'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_model_latlon(gs_mat, srcMeshName, landType))
    fp.write(block_gs_model_latlon(gs_cmf, tgtMeshName, landType))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()

    exec_remap(f)


def make_rt_mat_to_io_latlon(f, gs_mat, gs_io_latlon, landType, earth):
    srcMeshName = f'MATSIRO_{landType}'
    tgtMeshName = 'out_MATSIRO_latlon'
    srcVrfForm = 'index'
    tgtVrfForm = 'auto'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_model_raster(gs_mat, srcMeshName, landType))
    fp.write(block_gs_io_latlon(gs_io_latlon, tgtMeshName))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()

    exec_remap(f)


def make_rt_mat_to_io_row(f, gs_mat, gs_io_row, landType, earth):
    srcMeshName = f'MATSIRO_{landType}'
    tgtMeshName = 'out_MATSIRO_row'
    srcVrfForm = 'index'
    tgtVrfForm = 'auto'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_model_latlon(gs_mat, srcMeshName, landType))
    fp.write(block_gs_io_latlon(gs_io_row, tgtMeshName))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()

    exec_remap(f)


def exec_remap(f):
    #"""
    pc = subprocess.run([const.prog_remap, f], 
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE, 
                        encoding='utf-8')
    print('stdout:')
    print(pc.stdout.strip())
    print('stderr:')
    print(pc.stderr.strip())
    #"""
    pass


#===============================================================
#
#===============================================================

def run():
    cnf = json.load(open('conf.json','r'))
    for meshName in [
      'MATSIRO', 'CaMa-Flood', 
      'in_MATSIRO_bnd', 'in_met', 'in_metnc',
      'out_MATSIRO_row', 'out_MATSIRO_latlon', 
      'out_CaMa-Flood_row', 'out_CaMa-Flood_latlon']:
        util.set_gs(cnf[meshName], cnf['dir_top'])

    dir_set_this = f'{const.dir_set}/01_make_rt'
    os.makedirs(dir_set_this, exist_ok=True)

    # input to MATSIRO
    for landType in cnf["landType"]:
        make_rt_io_bnd_to_mat(f'{dir_set_this}/in_MATSIRO_bnd_to_MATSIRO_{landType}.conf', 
                              cnf["in_MATSIRO_bnd"], cnf["MATSIRO"], landType, cnf["Earth"])
    for landType in cnf["landType"]:
        make_rt_io_met_to_mat(f'{dir_set_this}/in_met_to_MATSIRO_{landType}.conf',
                              cnf["in_met"], cnf["MATSIRO"], landType, cnf["Earth"])
    for landType in cnf["landType"]:
        make_rt_io_metnc_to_mat(f'{dir_set_this}/in_metnc_to_MATSIRO_{landType}.conf',
                                cnf["in_metnc"], cnf["MATSIRO"], landType, cnf["Earth"])

    # MATSIRO to CMF
    make_rt_mat_river_to_cmf(f'{dir_set_this}/MATSIRO_river_to_CaMa-Flood.conf',
                             cnf["MATSIRO"], cnf["CaMa-Flood"], cnf["Earth"])
    # CMF to MATSIRO
    make_rt_cmf_to_mat_river(f'{dir_set_this}/CaMa-Flood_to_MATSIRO_river.conf', 
                             cnf["CaMa-Flood"], cnf["MATSIRO"], cnf["Earth"])

    # MATSIRO to output
    for landType in cnf["landType"]:
        make_rt_mat_to_io_latlon(f'{dir_set_this}/MATSIRO_{landType}_to_out_MATSIRO_latlon.conf',
                                 cnf["MATSIRO"], cnf["out_MATSIRO_latlon"], landType, cnf["Earth"])
    for landType in cnf["landType"]:
        make_rt_mat_to_io_row(f'{dir_set_this}/MATSIRO_{landType}_to_out_MATSIRO_row.conf',
                              cnf["MATSIRO"], cnf["out_MATSIRO_row"], landType, cnf["Earth"])

    # CMF to output
    make_rt_cmf_to_io_row(f'{dir_set_this}/CaMa-Flood_to_out_CaMa-Flood_row.conf', 
                          cnf["CaMa-Flood"], cnf["out_CaMa-Flood_row"], cnf["Earth"])


if __name__ == '__main__':
    run()
