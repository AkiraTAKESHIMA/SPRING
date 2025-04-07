import os
import sys
import subprocess


def head(srcMeshName, tgtMeshName):
    return '\
#\n\
path_report: "../tmp/01_make_rt/{srcMeshName}_to_{tgtMeshName}/report.txt"\n\
'.format(srcMeshName=srcMeshName, tgtMeshName=tgtMeshName)


def block_gs_model_latlon(grid_system, name, landType):
    wfmt = '\
\n\
[grid_system_latlon]\n\
  name: "{name}"\n\
  nx: {{ncx}}\n\
  ny: {{ncy}}\n\
  west: {{west}}\n\
  east: {{east}}\n\
  south: {{south}}\n\
  north: {{north}}\n\
  is_south_to_north: {{is_south_to_north}}\n\
  fin_grdidx: "{{f_grdidx_{landType}}}"\n\
[end]\n\
'.format(name=name, landType=landType)

    return wfmt.format(**grid_system)


def block_gs_model_raster(grid_system, name, landType):
    wfmt = '\
\n\
[grid_system_raster]\n\
  name: "{name}"\n\
  nx: {{ndx}}\n\
  ny: {{ndy}}\n\
  west: {{west}}\n\
  east: {{east}}\n\
  south: {{south}}\n\
  north: {{north}}\n\
  fin_rstidx: "{{f_rstidx_{landType}}}"\n\
  fin_grdidx: "{{f_grdidx_{landType}}}"\n\
  in_grid_sz: {{ncx}}, {{ncy}}\n\
  is_south_to_north: {{is_south_to_north}}\n\
[end]\n\
'.format(name=name, landType=landType)

    return wfmt.format(**grid_system)


def block_gs_io_latlon(grid_system, name, layer=1):
    idx_bgn = (layer-1) * grid_system['nx'] * grid_system['ny'] + 1

    return '\
\n\
[grid_system_latlon]\n\
  name: "{name}"\n\
  nx: {nx}\n\
  ny: {ny}\n\
  west: {west}\n\
  east: {east}\n\
  south: {south}\n\
  north: {north}\n\
  is_south_to_north: {is_south_to_north}\n\
  idx_bgn: {idx_bgn}\n\
[end]\n\
'.format(**grid_system, name=name, idx_bgn=idx_bgn)


def block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm):
    return '\
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
'.format(
    srcMeshName=srcMeshName, tgtMeshName=tgtMeshName,
    srcVrfForm=srcVrfForm, tgtVrfForm=tgtVrfForm)


def block_options(earth):
    if earth['shape'] == 'sphere':
        wfmt_earth = '\
  earth_shape: {shape}\n\
  earth_r: {r}\n\
'
    elif earth['shape'] == 'ellips':
        wfmt_earth = '\
  earth_shape: {shape}\n\
  earth_r: {r}\n\
  earth_e2: {e2}\n\
'

    return '\
\n\
[options]\n\
  old_files: remove\n\
{lines_earth}\
[end]\n\
'.format(
lines_earth=wfmt_earth.format(**earth))


def layer(landType):
    if landType == 'river':
        return 1
    elif landType == 'noriv':
        return 2
    else:
        raise Exception('Invalid value in $landType: {}'.format(landType))

#===============================================================
#
#===============================================================

def wconf_cmf_to_io_row(f, gs_cmf, gs_io_row, earth):
    srcMeshName = 'cmf'
    tgtMeshName = 'io-row'
    srcVrfForm = 'index'
    tgtVrfForm = 'auto'
    landType = 'river'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_model_latlon(gs_cmf, srcMeshName, landType))
    fp.write(block_gs_io_latlon(gs_io_row, srcMeshName))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()


def wconf_cmf_to_mat_river(f, gs_cmf, gs_mat, earth):
    srcMeshName = 'cmf'
    tgtMeshName = 'mat-river'
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


def wconf_io_bnd_to_mat(f, gs_io_mat_bnd, gs_mat, landType, earth):
    srcMeshName = 'io-mat-bnd'
    tgtMeshName = 'mat-'+landType
    srcVrfForm = 'auto'
    tgtVrfForm = 'index'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_io_latlon(gs_io_mat_bnd, srcMeshName, layer(landType)))
    fp.write(block_gs_model_latlon(gs_mat, tgtMeshName, landType))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()


def wconf_io_metnc_to_mat(f, gs_io_metnc, gs_mat, landType, earth):
    srcMeshName = 'io-metnc'
    tgtMeshName = 'mat-'+landType
    srcVrfForm = 'auto'
    tgtVrfForm = 'index'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_io_latlon(gs_io_metnc, srcMeshName))
    fp.write(block_gs_model_raster(gs_mat, tgtMeshName, landType))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()


def wconf_io_met_to_mat(f, gs_io_met, gs_mat, landType, earth):
    srcMeshName = 'io-met'
    tgtMeshName = 'mat-'+landType
    srcVrfForm = 'auto'
    tgtVrfForm = 'index'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_io_latlon(gs_io_met, srcMeshName))
    fp.write(block_gs_model_raster(gs_mat, tgtMeshName, landType))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()


def wconf_mat_river_to_cmf(f, gs_mat, gs_cmf, earth):
    srcMeshName = 'mat-river'
    tgtMeshName = 'cmf'
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


def wconf_mat_to_io_latlon(f, gs_mat, gs_io_latlon, landType, earth):
    srcMeshName = 'mat-'+landType
    tgtMeshName = 'io-latlon'
    srcVrfForm = 'index'
    tgtVrfForm = 'auto'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_model_raster(gs_mat, srcMeshName, landType))
    fp.write(block_gs_io_latlon(gs_io_latlon, tgtMeshName))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()


def wconf_mat_to_io_row(f, gs_mat, gs_io_row, landType, earth):
    srcMeshName = 'mat-'+landType
    tgtMeshName = 'io-row'
    srcVrfForm = 'index'
    tgtVrfForm = 'auto'

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_model_latlon(gs_mat, srcMeshName, landType))
    fp.write(block_gs_io_latlon(gs_io_row, tgtMeshName))
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()


"""
def wconf_(f, gs_, gs_, landType):
    srcMeshName = ''
    tgtMeshName = ''
    srcVrfForm = ''
    tgtVrfForm = ''

    fp = open(f,'w')
    fp.write(head(srcMeshName, tgtMeshName))
    fp.write(block_gs_())
    fp.write(block_gs_())
    fp.write(block_remapping(srcMeshName, tgtMeshName, srcVrfForm, tgtVrfForm))
    fp.write(block_options(earth))
    fp.close()
"""


#===============================================================
#
#===============================================================

def run():
    gs_mat = dict(
      ncx=720, ncy=360, ndx=21600, ndy=10800,
      west=-180, east=180, south=-90, north=90, 
      is_south_to_north='.true.',
      f_rstidx_river='mat_rstidx_river', 
      f_rstidx_noriv='mat_rstidx_noriv',
      f_grdidx_river='mat_grdidx_river', 
      f_grdidx_noriv='mat_grdidx_noriv',
    )

    gs_cmf = dict(
      ncx=720, ncy=360, 
      west=-180, east=180, south=-90, north=90, 
      is_south_to_north='.true.',
      f_grdidx_river='cmf_grdidx_river', 
      f_grdidx_noriv='cmf_grdidx_noriv',
    )

    gs_io_mat_bnd = dict(
      nx=720, ny=360,
      west=-180, east=180, south=-90, north=90, 
      is_south_to_north='.false.',
    )

    gs_io_met = dict(
      nx=720, ny=360,
      west=-180, east=180, south=-90, north=90, 
      is_south_to_north='.false.',
    )

    gs_io_metnc = dict(
      nx=720, ny=360,
      west=-180, east=180, south=-90, north=90, 
      is_south_to_north='.true.',
    )

    gs_io_row = dict(
      nx=720, ny=360,
      west=-180, east=180, south=-90, north=90, 
      is_south_to_north='.false.',
    )

    gs_io_latlon = dict(
      nx=720, ny=360,
      west=-180, east=180, south=-90, north=90, 
      is_south_to_north='.false.',
    )

    earth = dict(
      shape='sphere', r='6713.d3'
    )

    lst_landType = ['river', 'noriv']

    # I/O to MATSIRO
    for landType in lst_landType:
        wconf_io_bnd_to_mat('tmp/make_rt/io-bnd_to_mat-'+landType+'.conf', 
                            gs_io_mat_bnd, gs_mat, landType, earth)
    for landType in lst_landType:
        wconf_io_met_to_mat('tmp/make_rt/io-met_to_mat-'+landType+'.conf',
                            gs_io_met, gs_mat, landType, earth)
    for landType in lst_landType:
        wconf_io_metnc_to_mat('tmp/make_rt/io-metnc_to_mat-'+landType+'.conf',
                              gs_io_metnc, gs_mat, landType, earth)

    # MATSIRO to CMF
    wconf_mat_river_to_cmf('tmp/make_rt/mat-river_to_cmf.conf',
                           gs_mat, gs_cmf, earth)
    # CMF to MATSIRO
    wconf_cmf_to_mat_river('tmp/make_rt/cmf_to_mat-river.conf', 
                           gs_cmf, gs_mat, earth)

    # MATSIRO to I/O
    for landType in lst_landType:
        wconf_mat_to_io_latlon('tmp/make_rt/mat-'+landType+'_to_io-latlon.conf',
                               gs_mat, gs_io_latlon, landType, earth)
    for landType in lst_landType:
        wconf_mat_to_io_row('tmp/make_rt/mat-'+landType+'_to_io-row.conf',
                            gs_mat, gs_io_row, landType, earth)

    # CMF to I/O
    wconf_cmf_to_io_row('tmp/make_rt/cmf_to_io_row.conf', gs_cmf, gs_io_row, earth)


if __name__ == '__main__':
    run()
