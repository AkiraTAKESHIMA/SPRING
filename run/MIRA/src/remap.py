import os
import sys
import subprocess
import argparse
import json
import util


def block_mesh(refinement, meshType, resolution):
    meshName, meshDir, nk, nij = util.get_mesh(refinement, meshType, resolution)

    s = f'\
[mesh_polygon]\n\
  name: "{meshName}"\n\
  np: {nk}\n\
  nij: {nij}\n\
  dir: "{meshDir}"\n\
  f_x_vertex: "xyz.bin", rec=1\n\
  f_y_vertex: "xyz.bin", rec=2\n\
  f_z_vertex: "xyz.bin", rec=3\n\
  arc_parallel: .false.\n\
[end]\n\
'
    return s


def block_remapping_rt(srcMeshName, tgtMeshName, isForth):
    rtDir = util.get_rtDir(srcMeshName, tgtMeshName, isForth)

    s = f'\
[remapping]\n\
  dir: "{rtDir}"\n\
  fout_rt_sidx: "grid.bin", rec=1\n\
  fout_rt_tidx: "grid.bin", rec=2\n\
  fout_rt_area: "area.bin"\n\
  fout_rt_coef: "coef.bin"\n\
[end]\n\
'

    return s


def block_remapping_remap(srcMeshName, tgtMeshName, isForth, var, i):
    rtDir = util.get_rtDir(srcMeshName, tgtMeshName, isForth)
    nij = int(os.path.getsize(f'{rtDir}/grid.bin') / 8)
    fin_grdval, fout_grdval = util.get_fieldBinFile(isForth, var, i)

    s = f'\
[remapping]\n\
  dir: "{rtDir}"\n\
  length_rt: {nij}\n\
  fin_rt_sidx: "grid.bin", rec=1\n\
  fin_rt_tidx: "grid.bin", rec=2\n\
  fin_rt_area: "area.bin"\n\
  fin_rt_coef: "coef.bin"\n\
\n\
  dir: "{util.get_fieldDir(srcMeshName, tgtMeshName, var)}"\n\
  fin_grdval : "{fin_grdval}"\n\
  fout_grdval: "{fout_grdval}"\n\
[end]\n\
'

    return s


def block_options():
    s = '\
[options]\n\
  old_files: remove\n\
[end]\
'

    return s


def make_remapping_table(
  srcRefinement, srcMeshType, srcResolution,
  tgtRefinement, tgtMeshType, tgtResolution,
  isForth,
  overwrite,
  log):

    srcMeshName, *_ = util.get_mesh(srcRefinement, srcMeshType, srcResolution)
    tgtMeshName, *_ = util.get_mesh(tgtRefinement, tgtMeshType, tgtResolution)
    sForth = util.get_sForth(isForth)

    if log:
        print(f'Making table {srcMeshName} to {tgtMeshName} ({sForth})')

    if not overwrite:
        rtDir = util.get_rtDir(srcMeshName, tgtMeshName, isForth)
        if os.path.isfile(f'{rtDir}/grid.bin') and\
          os.path.isfile(f'{rtDir}/area.bin') and\
          os.path.isfile(f'{rtDir}/coef.bin'):
            if log:
                print(f'  File already exists: {rtDir}/grid.bin')
            return

    f_conf = f'conf/remapping_table/{srcMeshName}_to_{tgtMeshName}/{sForth}.conf'
    os.makedirs(os.path.dirname(f_conf), exist_ok=True)
    if log:
        print(f'  conf: {f_conf}')
    with open(f_conf, 'w') as wf:
        wf.write(f'\
#\n\
path_report: "{util.get_rtDir(srcMeshName, tgtMeshName, isForth)}/report.txt"\n\
\n')

        if isForth:
            wf.write(f'\
{block_mesh(srcRefinement, srcMeshType, srcResolution)}\
\n\
{block_mesh(tgtRefinement, tgtMeshType, tgtResolution)}\
\n')
        else:
            wf.write(f'\
{block_mesh(tgtRefinement, tgtMeshType, tgtResolution)}\
\n\
{block_mesh(srcRefinement, srcMeshType, srcResolution)}\
\n')

        wf.write(f'\
{block_remapping_rt(srcMeshName, tgtMeshName, isForth)}\
\n\
{block_options()}\
')

    cp = subprocess.run(
      [util.PROGRAM_REMAP, f_conf], 
      capture_output=True,
    )

    f_log = f'{util.get_rtDir(srcMeshName, tgtMeshName, isForth)}/log'
    f_log_stdout = f_log + '.out'
    f_log_stderr = f_log + '.err'
    if log:
        print(f'  stdout: {f_log_stdout}')
        print(f'  stderr: {f_log_stderr}')
    os.makedirs(os.path.dirname(f_log), exist_ok=True)
    with open(f_log_stdout, 'w') as wf:
        wf.write(cp.stdout.decode())
    with open(f_log_stderr, 'w') as wf:
        wf.write(cp.stderr.decode())
    if cp.returncode != 0:
        print(cp.stdout.decode())
        print(cp.stderr.decode())
        print(f'*** returncode: {cp.returncode}')
        sys.exit(1)


def remap_field(
  srcRefinement, srcMeshType, srcResolution,
  tgtRefinement, tgtMeshType, tgtResolution, 
  isForth, var, i,
  overwrite, 
  log):

    srcMeshName, srcMeshDir, *_ = util.get_mesh(srcRefinement, srcMeshType, srcResolution)
    tgtMeshName, tgtMeshDir, *_ = util.get_mesh(tgtRefinement, tgtMeshType, tgtResolution)
    sForth = util.get_sForth(isForth)

    if log:
        print(f'Remapping {srcMeshName} to {tgtMeshName} ({sForth}) iter{i}')

    if not overwrite:
        fieldDir = util.get_fieldDir(srcMeshName, tgtMeshName, var)
        fin_grdval, fout_grdval = util.get_fieldBinFile(isForth, var, i)
        if os.path.isfile(f'{fieldDir}/{fout_grdval}'):
            if log:
                print(f'  File already exists: {fieldDir}/{fout_grdval}')
            return

    f_conf = f'conf/remap_iter/{srcMeshName}_to_{tgtMeshName}/{var}_{i:04d}_{sForth}.conf'
    os.makedirs(os.path.dirname(f_conf), exist_ok=True)
    if log:
        print(f'  conf: {f_conf}')
    with open(f_conf, 'w') as wf:
        wf.write(f'\
#\n\
path_report: "{util.get_remapDir(srcMeshName, tgtMeshName)}/report/{var}_{i:04d}_{sForth}.txt"\n\
\n')
        if isForth:
            wf.write(f'\
{block_mesh(srcRefinement, srcMeshType, srcResolution)}\
\n\
{block_mesh(tgtRefinement, tgtMeshType, tgtResolution)}\
\n')
        else:
            wf.write(f'\
{block_mesh(tgtRefinement, tgtMeshType, tgtResolution)}\
\n\
{block_mesh(srcRefinement, srcMeshType, srcResolution)}\
\n')
        wf.write(f'\
{block_remapping_remap(srcMeshName, tgtMeshName, isForth, var, i)}\
\n\
{block_options()}\
')

    # Prepare original fields
    if i == 0 and isForth:
        if log:
            print('  Copying original fields')
        f_src_ini, _ = util.get_fieldBinFile(isForth, var, 0)   # src_iter0000
        _, f_tgt_ini = util.get_fieldBinFile(isForth, var, -1)  # tgt_iter0000

        fieldDir = util.get_fieldDir(srcMeshName, tgtMeshName, var)
        os.makedirs(fieldDir, exist_ok=True)
        cp = subprocess.run(
          ['cp', '-f', f'{srcMeshDir}/val_{var}.bin', f'{fieldDir}/{f_src_ini}'], 
          check=True
        )
        subprocess.run(
          ['cp', '-f', f'{tgtMeshDir}/val_{var}.bin', f'{fieldDir}/{f_tgt_ini}'], 
          check=True
        )

    # Remap
    cp = subprocess.run(
      [util.PROGRAM_REMAP, f_conf], 
      capture_output=True,
    )

    f_log = f'{util.get_remapDir(srcMeshName, tgtMeshName)}/log/{var}_{i:04d}_{sForth}'
    f_log_stdout = f_log + '.out'
    f_log_stderr = f_log + '.err'
    if log:
        print(f'  stdout: {f_log_stdout}')
        print(f'  stderr: {f_log_stderr}')
    os.makedirs(os.path.dirname(f_log), exist_ok=True)
    with open(f_log_stdout, 'w') as wf:
        wf.write(cp.stdout.decode())
    with open(f_log_stderr, 'w') as wf:
        wf.write(cp.stderr.decode())
    if cp.returncode != 0:
        print(cp.stdout.decode())
        print(cp.stderr.decode())
        print(f'*** returncode: {cp.returncode}')
        sys.exit(1)


def plot_field(
  srcRefinement, srcMeshType, srcResolution,
  tgtRefinement, tgtMeshType, tgtResolution, 
  mesh, var, i,
  overwrite,
  log):

    import numpy as np

    """
    mesh = {src, tgt}
    """

    srcMeshName, *_ = util.get_mesh(srcRefinement, srcMeshType, srcResolution)
    tgtMeshName, *_ = util.get_mesh(tgtRefinement, tgtMeshType, tgtResolution)
    if log:
        print(f'Plotting field of {srcMeshName} to {tgtMeshName}, {var}, iter{i}')

    if mesh == 'src':
        mesh_long = 'source'
    elif mesh == 'tgt':
        mesh_long = 'target'

    vmin, vmax = util.LST_RANGE[var]

    remapDir = util.get_remapDir(srcMeshName, tgtMeshName)
    f_ini = f'{remapDir}/field/{var}/{var}_{mesh}_{0:04d}.bin'
    f_val = f'{remapDir}/field/{var}/{var}_{mesh}_{i:04d}.bin'
    f_dif = f'{remapDir}/field/{var}_diff/{var}_{mesh}_{i:04d}.bin'
    if i != 0:
        if log:
            print(f'  diff: {f_dif}')
        os.makedirs(os.path.dirname(f_dif), exist_ok=True)
        (np.fromfile(f_val) - np.fromfile(f_ini)).tofile(f_dif)

    f_conf = f'conf/plot_field/{srcMeshName}_to_{tgtMeshName}/{var}_{mesh}_{i:04d}.conf'
    if log:
        print(f'  conf: {f_conf}')
    os.makedirs(os.path.dirname(f_conf), exist_ok=True)
    with open(f_conf, 'w') as wf:
        wf.write(f'\
{block_mesh(srcRefinement, srcMeshType, srcResolution)}\
\n\
{block_mesh(tgtRefinement, tgtMeshType, tgtResolution)}\
\n\
')
        wf.write(f'\
[figures]\n\
  cmap: jet\n\
  vmin: {vmin}\n\
  vmax: {vmax}\n\
\n\
  dir: ""\n\
\n\
  mesh: {mesh_long}\n\
  path_fig: "fig/field/{srcMeshName}_to_{tgtMeshName}/{var}/{var}_{mesh}_{i:04d}.png"\n\
  f_grdval: "{f_val}"\n\
\n\
  mesh: {mesh_long}\n\
  path_fig: "fig/field/{srcMeshName}_to_{tgtMeshName}/{var}_diff/{var}_{mesh}_{i:04d}.png"\n\
  f_grdval: "{f_dif}"\n\
[end]\n\
')

    cp = subprocess.run(
      ['python3', util.SCRIPT_PLOT_FIELD, f_conf], 
      capture_output=True,
    )
    if cp.returncode != 0:
        print(cp.stdout.decode())
        print(cp.stderr.decode())
        print(f'*** returncode: {cp.returncode}')
        sys.exit(1)


def make_netCDF(
  srcRefinement, srcMeshType, srcResolution,
  tgtRefinement, tgtMeshType, tgtResolution,
  var,
  overwrite,
  log):

    import numpy as np
    import netCDF4

    srcMeshName, _, _, nsij, *_ = util.get_mesh(srcRefinement, srcMeshType, srcResolution)
    tgtMeshName, _, _, ntij, *_ = util.get_mesh(tgtRefinement, tgtMeshType, tgtResolution)
    fieldDir = util.get_fieldDir(srcMeshName, tgtMeshName, var)

    f_nc = f'{fieldDir}/{var}.nc'
    print(f'Out: {f_nc}')
    if os.path.isfile(f_nc):
        if overwrite:
            os.remove(f_nc)
        else:
            if log:
                print(f'  File already exists: {f_nc}')
            return
    nc = netCDF4.Dataset(f_nc, 'w', format='NETCDF4')

    var_nc = util.TBL_VAR[var]

    dim_iter = nc.createDimension('iteration', None)
    dim_elem_tgt = nc.createDimension('elem_tgt', ntij)
    dim_elem_src = nc.createDimension('elem_src', nsij)

    src = nc.createVariable(f'{var_nc}_remap_src', np.float64, ('iteration', 'elem_src'))
    tgt = nc.createVariable(f'{var_nc}_remap_tgt', np.float64, ('iteration', 'elem_tgt'))

    f_src = f'{fieldDir}/{var}_src_{util.ITERMAX:04d}.bin'
    f_tgt = f'{fieldDir}/{var}_tgt_{util.ITERMAX:04d}.bin'
    if not os.path.isfile(f_src):
        print(f'*** File not found: {f_src}')
        return
    if not os.path.isfile(f_tgt):
        print(f'*** File not found: {f_tgt}')
        return

    for i in range(util.ITERMAX+1):
        src[i,:] = np.fromfile(f'{fieldDir}/{var}_src_{i:04d}.bin')
        tgt[i,:] = np.fromfile(f'{fieldDir}/{var}_tgt_{i:04d}.bin')
    nc.close()


def calc_metrics(
  srcRefinement, srcMeshType, srcResolution,
  tgtRefinement, tgtMeshType, tgtResolution,
  var,
  overwrite,
  log):

    srcMeshName, *_ = util.get_mesh(srcRefinement, srcMeshType, srcResolution)
    tgtMeshName, *_ = util.get_mesh(tgtRefinement, tgtMeshType, tgtResolution)

    f_src = util.get_meshNCFile(srcRefinement, srcMeshType, srcResolution)
    f_tgt = util.get_meshNCFile(tgtRefinement, tgtMeshType, tgtResolution)
    f_data = util.get_fieldNCFile(srcMeshName, tgtMeshName, var)
    f_metrics, fb_metrics = util.get_metricsFile(srcMeshName, tgtMeshName, var)

    if not overwrite:
        if os.path.isfile(f_metrics):
            if log:
                print(f'  File already exists: {f_metrics}')
            return

    os.makedirs(os.path.dirname(fb_metrics), exist_ok=True)
    if log:
        print(f'Out: {f_metrics}')

    cp = subprocess.run(['python3', util.SCRIPT_CANGAMETRICSDRIVER,
      '--ss', f_src, '--smc', '1',
      '--st', f_tgt, '--tmc', '1',
      '--data', f_data, '--field', util.TBL_VAR[var],
      '--output', fb_metrics],
      capture_output=True)
    if cp.returncode != 0:
        print(cp.stdout.decode())
        print(cp.stderr.decode())
        print(f'*** return code: {cp.returncode}')


def plot_metrics(
  srcRefinement, srcMeshType, srcResolution,
  tgtRefinement, tgtMeshType, tgtResolution,
  var,
  overwrite,
  log):

    import pandas as pd
    import numpy as np
    import matplotlib.pyplot as plt

    srcMeshName, *_ = util.get_mesh(srcRefinement, srcMeshType, srcResolution)
    tgtMeshName, *_ = util.get_mesh(tgtRefinement, tgtMeshType, tgtResolution)

    f_metrics, _ = util.get_metricsFile(srcMeshName, tgtMeshName, var)
    df = pd.read_csv(f_metrics)
    df = df.drop(df.index[0])

    d = df['GC']
    dd = np.array([np.log10(abs(v)) if v != 0 else np.nan for v in d])
    plt.plot(range(len(dd)), dd)
    plt.show()



job = sys.argv[1]

if job == 'make_rt':
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    parser.add_argument('srcRefinement', type=str)
    parser.add_argument('srcMeshType', type=str)
    parser.add_argument('srcResolution', type=int)
    parser.add_argument('tgtRefinement', type=str)
    parser.add_argument('tgtMeshType', type=str)
    parser.add_argument('tgtResolution', type=int)
    parser.add_argument('isForth', type=str)
    parser.add_argument('--overwrite', action='store_true')
    parser.add_argument('--log', action='store_false')
    args = parser.parse_args()

    make_remapping_table(
      args.srcRefinement, args.srcMeshType, args.srcResolution, 
      args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
      args.isForth, 
      args.overwrite, 
      args.log)

elif job == 'remap':
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    parser.add_argument('srcRefinement', type=str)
    parser.add_argument('srcMeshType', type=str)
    parser.add_argument('srcResolution', type=int)
    parser.add_argument('tgtRefinement', type=str)
    parser.add_argument('tgtMeshType', type=str)
    parser.add_argument('tgtResolution', type=int)
    parser.add_argument('isForth', type=str)
    parser.add_argument('variable', type=str)
    parser.add_argument('iteration', type=int)
    parser.add_argument('--overwrite', action='store_true')
    parser.add_argument('--log', action='store_false')
    args = parser.parse_args()

    remap_field(
      args.srcRefinement, args.srcMeshType, args.srcResolution, 
      args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
      args.isForth, args.variable, args.iteration,
      args.overwrite, 
      args.log)


elif job == 'remap_iter':
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    parser.add_argument('srcRefinement', type=str)
    parser.add_argument('srcMeshType', type=str)
    parser.add_argument('srcResolution', type=int)
    parser.add_argument('tgtRefinement', type=str)
    parser.add_argument('tgtMeshType', type=str)
    parser.add_argument('tgtResolution', type=int)
    parser.add_argument('variable', type=str)
    parser.add_argument('--overwrite', action='store_true')
    parser.add_argument('--log', action='store_false')
    args = parser.parse_args()

    for i in range(util.ITERMAX):
        for isForth in (True, False):
            remap_field(
              args.srcRefinement, args.srcMeshType, args.srcResolution, 
              args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
              isForth, args.variable, i,
              args.overwrite, 
              args.log)


elif job == 'plot_field': 
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    parser.add_argument('srcRefinement', type=str)
    parser.add_argument('srcMeshType', type=str)
    parser.add_argument('srcResolution', type=int)
    parser.add_argument('tgtRefinement', type=str)
    parser.add_argument('tgtMeshType', type=str)
    parser.add_argument('tgtResolution', type=int)
    parser.add_argument('mesh', choices=['src', 'tgt'], type=str)
    parser.add_argument('variable', type=str)
    parser.add_argument('iteration', type=int)
    parser.add_argument('--overwrite', action='store_true')
    parser.add_argument('--log', action='store_false')
    args = parser.parse_args()

    plot_field(
      args.srcRefinement, args.srcMeshType, args.srcResolution, 
      args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
      args.mesh, args.varriable, args.iteration,
      args.overwrite, 
      args.log)

elif job == 'make_netCDF':
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    parser.add_argument('srcRefinement', type=str)
    parser.add_argument('srcMeshType', type=str)
    parser.add_argument('srcResolution', type=int)
    parser.add_argument('tgtRefinement', type=str)
    parser.add_argument('tgtMeshType', type=str)
    parser.add_argument('tgtResolution', type=int)
    parser.add_argument('variable', type=str)
    parser.add_argument('--overwrite', action='store_true')
    parser.add_argument('--log', action='store_false')
    args = parser.parse_args()

    make_netCDF(
      args.srcRefinement, args.srcMeshType, args.srcResolution,
      args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
      args.variable, 
      args.overwrite,
      args.log)

elif job == 'calc_metrics':
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    parser.add_argument('srcRefinement', type=str)
    parser.add_argument('srcMeshType', type=str)
    parser.add_argument('srcResolution', type=int)
    parser.add_argument('tgtRefinement', type=str)
    parser.add_argument('tgtMeshType', type=str)
    parser.add_argument('tgtResolution', type=int)
    parser.add_argument('variable', type=str)
    parser.add_argument('--overwrite', action='store_true')
    parser.add_argument('--log', action='store_false')
    args = parser.parse_args()

    calc_metrics(
      args.srcRefinement, args.srcMeshType, args.srcResolution,
      args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
      args.variable, 
      args.overwrite,
      args.log)

elif job == 'plot_metrics':
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    parser.add_argument('srcRefinement', type=str)
    parser.add_argument('srcMeshType', type=str)
    parser.add_argument('srcResolution', type=int)
    parser.add_argument('tgtRefinement', type=str)
    parser.add_argument('tgtMeshType', type=str)
    parser.add_argument('tgtResolution', type=int)
    parser.add_argument('variable', type=str)
    parser.add_argument('--overwrite', action='store_true')
    parser.add_argument('--log', action='store_false')
    args = parser.parse_args()

    plot_metrics(
      args.srcRefinement, args.srcMeshType, args.srcResolution,
      args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
      args.variable, 
      args.overwrite,
      args.log)

elif job == 'all':
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    parser.add_argument('srcRefinement', type=str)
    parser.add_argument('srcMeshType', type=str)
    parser.add_argument('srcResolution', type=int)
    parser.add_argument('tgtRefinement', type=str)
    parser.add_argument('tgtMeshType', type=str)
    parser.add_argument('tgtResolution', type=int)
    parser.add_argument('variable', type=str)
    parser.add_argument('--overwrite', action='store_true')
    parser.add_argument('--log', action='store_false')
    args = parser.parse_args()


    for isForth in (True, False):
        make_remapping_table(
          args.srcRefinement, args.srcMeshType, args.srcResolution, 
          args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
          isForth, 
          args.overwrite, 
          args.log)

    for i in range(util.ITERMAX):
        for isForth in (True, False):
            remap_field(
              args.srcRefinement, args.srcMeshType, args.srcResolution, 
              args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
              isForth, args.variable, i,
              args.overwrite, 
              args.log)

    make_netCDF(
      args.srcRefinement, args.srcMeshType, args.srcResolution,
      args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
      args.variable, 
      args.overwrite,
      args.log)

    calc_metrics(
      args.srcRefinement, args.srcMeshType, args.srcResolution,
      args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
      args.variable, 
      args.overwrite,
      args.log)
