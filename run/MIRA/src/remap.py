import os
import sys
import subprocess
import argparse
import json
import util


def block_mesh(refinement: str, meshType: str, resolution: int):
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


def block_remapping_rt(srcMeshName: str, tgtMeshName: str, isForth: bool):
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


def block_remapping_remap(
        srcMeshName: str, tgtMeshName: str, isForth: bool, var: str, i: int):
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
  srcRefinement: str, srcMeshType: str, srcResolution: str,
  tgtRefinement: str, tgtMeshType: str, tgtResolution: str,
  isForth: bool,
  overwrite: bool,
  log: bool):

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
  srcRefinement: str, srcMeshType: str, srcResolution: str,
  tgtRefinement: str, tgtMeshType: str, tgtResolution: str, 
  isForth: bool, var: str, i: int,
  overwrite: bool, 
  log: bool):

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
  srcRefinement: str, srcMeshType: str, srcResolution: int,
  tgtRefinement: str, tgtMeshType: str, tgtResolution: int, 
  mesh: str, var: str, i: int,
  plot_val: bool, vmin: float, vmax: float, 
  plot_diff: bool, dmax: float,
  overwrite: bool,
  log: bool):

    import numpy as np

    """
    mesh = {src, tgt}
    """

    if i == 0:
        plot_diff = False

    srcMeshName, *_ = util.get_mesh(srcRefinement, srcMeshType, srcResolution)
    tgtMeshName, *_ = util.get_mesh(tgtRefinement, tgtMeshType, tgtResolution)
    if log:
        print(f'Plotting field of {srcMeshName} to {tgtMeshName}, {var}, iter{i}')

    if mesh == 'src':
        mesh_long = 'source'
    elif mesh == 'tgt':
        mesh_long = 'target'

    remapDir = util.get_remapDir(srcMeshName, tgtMeshName)
    f_ini = f'{remapDir}/field/{var}/{var}_{mesh}_{0:04d}.bin'
    f_val = f'{remapDir}/field/{var}/{var}_{mesh}_{i:04d}.bin'
    f_dif = f'{remapDir}/field/{var}_diff/{var}_{mesh}_{i:04d}.bin'
    if plot_diff:
        if log:
            print(f'  diff: {f_dif}')
        os.makedirs(os.path.dirname(f_dif), exist_ok=True)
        (np.fromfile(f_val) - np.fromfile(f_ini)).tofile(f_dif)

    if vmin is None:
        vmin = util.DICT_VAR[var]["vmin"]
    if vmax is None:
        vmax = util.DICT_VAR[var]["vmax"]
    if plot_diff and dmax is None:
        dmax = util.DICT_VAR[var]["dmax"]

    f_conf = f'conf/plot_field/{srcMeshName}_to_{tgtMeshName}/{var}_{mesh}_{i:04d}.conf'
    fig_val = f'fig/field/{srcMeshName}_to_{tgtMeshName}/{var}/{var}_{mesh}_{i:04d}.png'
    fig_dif = f'fig/field/{srcMeshName}_to_{tgtMeshName}/{var}_diff/{var}_{mesh}_{i:04d}.png'
    if log:
        print(f'  conf: {f_conf}')
        if plot_val and plot_diff:
            print(f'  fig_val : {fig_val}')
            print(f'  fig_diff: {fig_dif}')
        elif plot_val:
            print(f'  fig_val: {fig_val}')
        elif plot_diff:
            print(f'  fig_diff: {fig_dif}')
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
\n\
  dir: ""\n\
')
        if plot_val:
            wf.write(f'\
\n\
  mesh: {mesh_long}\n\
  path_fig: "{fig_val}"\n\
  f_grdval: "{f_val}"\n\
  vmin: {vmin}\n\
  vmax: {vmax}\n\
')
        if plot_diff:
            wf.write(f'\
\n\
  mesh: {mesh_long}\n\
  path_fig: "{fig_dif}"\n\
  f_grdval: "{f_dif}"\n\
  vmin: {-dmax}\n\
  vmax: {dmax}\n\
')
        wf.write('\
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


def make_NetCDF(
  srcRefinement: str, srcMeshType: str, srcResolution: str,
  tgtRefinement: str, tgtMeshType: str, tgtResolution: str,
  var: str,
  overwrite: bool,
  log: bool):

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

    var_nc = util.DICT_VAR[var]["name"]

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
  srcRefinement: str, srcMeshType: str, srcResolution: str,
  tgtRefinement: str, tgtMeshType: str, tgtResolution: str,
  var: str,
  overwrite: bool,
  log: bool):

    srcMeshName, *_ = util.get_mesh(srcRefinement, srcMeshType, srcResolution)
    tgtMeshName, *_ = util.get_mesh(tgtRefinement, tgtMeshType, tgtResolution)

    f_src = util.get_meshNCFile(srcRefinement, srcMeshType, srcResolution)
    f_tgt = util.get_meshNCFile(tgtRefinement, tgtMeshType, tgtResolution)
    f_data = util.get_fieldNCFile(srcMeshName, tgtMeshName, var)
    f_metrics, fb_metrics = util.get_SPRINGMetricsFile(srcMeshName, tgtMeshName, var)

    args = ['--ss', f_src, '--smc', '1', '--st', f_tgt, '--tmc', '1']

    if srcRefinement == 'r' or tgtRefinement == 'r':
        arg_grad = []
    else:
        arg_grad = ['--includeGradientMetrics']

    if not overwrite:
        if os.path.isfile(f_metrics):
            if log:
                print(f'  File already exists: {f_metrics}')
            return

    os.makedirs(os.path.dirname(fb_metrics), exist_ok=True)
    if log:
        print(f'Out: {f_metrics}')

    cp = subprocess.run(['python3', util.SCRIPT_CANGAMETRICSDRIVER] + \
      ['--ss', f_src, '--smc', '1', '--st', f_tgt, '--tmc', '1'] + \
      arg_grad + \
      ['--data', f_data, '--field', util.DICT_VAR[var]["name"],
       '--output', fb_metrics],
      capture_output=True)
    if cp.returncode != 0:
        print(cp.stdout.decode())
        print(cp.stderr.decode())
        print(f'*** return code: {cp.returncode}')


def measure_time(
  srcRefinement: str, srcMeshType: str, srcResolution: str,
  tgtRefinement: str, tgtMeshType: str, tgtResolution: str,
  itermax: int,
  overwrite_rt: bool, overwrite_summary: bool):

    isForth = True

    srcMeshName, *_ = util.get_mesh(srcRefinement, srcMeshType, srcResolution)
    tgtMeshName, *_ = util.get_mesh(tgtRefinement, tgtMeshType, tgtResolution)
    path_report_org = f"{util.get_rtDir(srcMeshName, tgtMeshName, isForth)}/report.txt"

    dir_report = f'out/measure_time/{srcMeshName}-{tgtMeshName}'
    os.makedirs(dir_report, exist_ok=True)

    for i in range(itermax):
        make_remapping_table(
            srcRefinement, srcMeshType, srcResolution,
            tgtRefinement, tgtMeshType, tgtResolution,
            isForth, overwrite_rt, False)

        path_report = f'{dir_report}/{i:06d}.txt'
        cp = subprocess.run(['mv', path_report_org, path_report],
                capture_output=True)
        if cp.returncode != 0:
            print(cp.stdout.decode())
            print(cp.stderr.decode())
            print(f'*** return code: {cp.returncode}')
            quit()
        lines = open(path_report, 'r').readlines()
        for i, line in enumerate(lines):
            if 'Process Time' in line: break
        for line in lines[i:]:
            print(line.strip())


def list_outputs(dataName: str):
    if dataName == 'remapping_table':
        list_output__remappingTable()

    elif dataName in ['remapped', 'NetCDF', 'metrics']:
        list_output__metrics(dataName)

    else:
        raise Exception(f'Invalid value in `dataName`: {dataName}')


def list_output__remappingTable():
    lst_mesh = get_lst_mesh()

    def list_all(lst_mesh, srcRefinement, tgtRefinement):
        print(f'{" "*9}{tgtRefinement}')

        line = ' '*9
        for tgtMeshType in util.DICT_MESH[tgtRefinement]:
            line += f'{tgtMeshType:<{3*len(util.DICT_MESH[tgtRefinement][tgtMeshType])}s}'
        print(line)

        line = ' '*9
        for tgtMeshType in util.DICT_MESH[tgtRefinement]:
            for tgtResolution in util.DICT_MESH[tgtRefinement][tgtMeshType]:
                line += f'{str(tgtResolution):<3s}'
        print(line)

        is_first = True
        srcMeshType_prev = ''
        for srcMeshInfo in lst_mesh[srcRefinement]:
            srcMeshType, srcResolution = srcMeshInfo
            srcMeshName, *_ = util.get_mesh(srcRefinement, srcMeshType, srcResolution)
            
            line = get_row_head(is_first, srcRefinement, srcMeshType, srcMeshType_prev, srcResolution)
            is_first = False
            srcMeshType_prev = srcMeshType

            for tgtMeshInfo in lst_mesh[tgtRefinement]:
                tgtMeshType, tgtResolution = tgtMeshInfo
                tgtMeshName, *_ = util.get_mesh(tgtRefinement, tgtMeshType, tgtResolution)

                line += ' '

                for isForth in (True, False):
                    rtDir = util.get_rtDir(srcMeshName, tgtMeshName, isForth)
                    f_rt_grid = os.path.join(rtDir, 'grid.bin')
                    f_rt_coef = os.path.join(rtDir, 'coef.bin')
                    if os.path.isfile(f_rt_grid) and os.path.isfile(f_rt_coef):
                        if os.path.getsize(f_rt_grid) == os.path.getsize(f_rt_coef):
                            stat = 0
                        else:
                            stat = 1
                    else:
                        stat = 1
                    line += util.get_char_stat(stat)
            print(line)

    lst_refinement = []
    for srcRefinement in util.DICT_MESH:
        for tgtRefinement in util.DICT_MESH:
            if srcRefinement == tgtRefinement:
                lst_refinement.append((srcRefinement, tgtRefinement))
    for srcRefinement in util.DICT_MESH:
        for tgtRefinement in util.DICT_MESH:
            if srcRefinement != tgtRefinement:
                lst_refinement.append((srcRefinement, tgtRefinement))
    for (srcRefinement, tgtRefinement) in lst_refinement:
            list_all(lst_mesh, srcRefinement, tgtRefinement)


def list_output__metrics(dname: str):
    lst_mesh = get_lst_mesh()

    if dname == 'remapped':
        def func_get_f(srcMeshName, tgtMeshName, var):
            return os.path.join(util.get_fieldDir(srcMeshName, tgtMeshName, var),
                                util.get_fieldBinFile(True, var, util.ITERMAX-1)[1])
    elif dname == 'NetCDF':
        func_get_f = util.get_fieldNCFile
    elif dname == 'metrics':
        def func_get_f(srcMeshName, tgtMeshName, var):
            return util.get_SPRINGMetricsFile(srcMeshName, tgtMeshName, var)[0]
    else:
        raise Exception(f'Invalid value in `dname`: {dname}')

    def list_all(lst_mesh, srcRefinement, tgtRefinement):
        print(f'{" "*9}{tgtRefinement}')

        line = ' '*9
        for tgtMeshType in util.DICT_MESH[tgtRefinement]:
            line += f'{tgtMeshType:<{6*len(util.DICT_MESH[tgtRefinement][tgtMeshType])}s}'
        print(line)

        line = ' '*9
        for tgtMeshType in util.DICT_MESH[tgtRefinement]:
            for tgtResolution in util.DICT_MESH[tgtRefinement][tgtMeshType]['file']:
                line += f'{str(tgtResolution):<6s}'
        print(line)

        is_first = True
        srcMeshType_prev = ''
        for srcMeshInfo in lst_mesh[srcRefinement]:
            srcMeshType, srcResolution = srcMeshInfo
            srcMeshName, *_ = util.get_mesh(srcRefinement, srcMeshType, srcResolution)

            line = get_row_head(is_first, srcRefinement, srcMeshType, srcMeshType_prev, srcResolution)
            is_first = False
            srcMeshType_prev = srcMeshType

            for tgtMeshInfo in lst_mesh[tgtRefinement]:
                tgtMeshType, tgtResolution = tgtMeshInfo
                tgtMeshName, *_ = util.get_mesh(tgtRefinement, tgtMeshType, tgtResolution)

                line += ' '
                for var in util.DICT_VAR:
                    f = func_get_f(srcMeshName, tgtMeshName, var)
                    if os.path.isfile(f):
                        stat = 0
                    else:
                        stat = 1

                    line += util.get_char_stat(stat)
            print(line)

    lst_refinement = []
    for srcRefinement in util.DICT_MESH:
        for tgtRefinement in util.DICT_MESH:
            if srcRefinement == tgtRefinement:
                lst_refinement.append((srcRefinement, tgtRefinement))
    for srcRefinement in util.DICT_MESH:
        for tgtRefinement in util.DICT_MESH:
            if srcRefinement != tgtRefinement:
                lst_refinement.append((srcRefinement, tgtRefinement))
    for (srcRefinement, tgtRefinement) in lst_refinement:
        list_all(lst_mesh, srcRefinement, tgtRefinement)


def get_lst_mesh():
    lst_mesh = {}
    for refinement in util.DICT_MESH:
        lst_mesh[refinement] = []
        d1 = util.DICT_MESH[refinement]
        for meshType in d1:
            d2 = d1[meshType]
            for resolution in d2['file']:
                lst_mesh[refinement].append((meshType, resolution))
    return lst_mesh


def get_row_head(is_first, srcRefinement, srcMeshType, srcMeshType_prev, srcResolution):
    if is_first:
        line = f'{srcRefinement}'
    else:
        line = ' '
    if srcMeshType != srcMeshType_prev:
        line += f' {srcMeshType:<4s}'
    else:
        line += f' {"":<4s}'
    line += f' {srcResolution}'

    return line


def calc_convergence_rate(
  refinement: str,
  srcMeshType: str, tgtMeshType: str, 
  algorithm: str, degree: int, var: str, metric: str):

    import pandas as pd
    import numpy as np
    import matplotlib.pyplot as plt

    lst_nij = []
    lst_v = []
    for resolution in util.DICT_MESH_RESOLUTION[refinement]:
        *_, nij = util.get_mesh(refinement, tgtMeshType, resolution)
        lst_nij.append(nij)
        
        f_metrics = util.get_metricsFile(
            refinement, algorithm,
            srcMeshType, resolution, 
            tgtMeshType, resolution,
            degree, var)
        lst_v.append(pd.read_csv(f_metrics, skiprows=range(1,2), nrows=1)[metric].iloc[0])
    lst_nij = np.array(lst_nij)
    lst_v = np.array(lst_v)

    lst_h = 1.0 / np.sqrt(lst_nij)

    #plt.plot(lst_h, lst_v)
    #plt.xscale('log')
    #plt.yscale('log')
    #plt.show()
    
    x = np.log(lst_h)
    y = np.log(lst_v)
    r, b = np.polyfit(x, y, 1)

    print(f'convergence rate r = {r:.3f}, constant C = {np.exp(b):.3e}')

    x = np.log(lst_h[-2:])
    y = np.log(lst_v[-2:])
    r, b = np.polyfit(x, y, 1)

    print(f'convergence rate r = {r:.3f}, constant C = {np.exp(b):.3e}')


def plot_metrics(
  refinement: str, 
  srcMeshType: str, srcResolution: str,
  tgtMeshType: str, tgtResolution: str,
  metric: str, degree: int, var: str,
  ymin: float, ymax: float,
  plot_GMLS: bool, figname_add: str,
  overwrite: bool,
  log: bool):

    import pandas as pd
    import numpy as np
    import matplotlib.pyplot as plt

    srcMeshName, *_ = util.get_mesh(refinement, srcMeshType, srcResolution)
    tgtMeshName, *_ = util.get_mesh(refinement, tgtMeshType, tgtResolution)

    x = np.arange(0, util.ITERMAX+1, util.ITERINT_PLOT)

    ds = {}
    lst_label = []

    f_metrics, _ = util.get_SPRINGMetricsFile(srcMeshName, tgtMeshName, var)
    if log:
        print(f'Metrics: {f_metrics}')
    df = pd.read_csv(f_metrics)[metric]
    df = df.drop(df.index[0])
    y = df.values.astype(np.float64)
    # TMP
    #y = y[x]
    y = y[x[:-1]]
    y = np.r_[y, y[-1]]
    ds["SPRING"] = y
    lst_label.append('SPRING')

    for algorithm in util.DICT_METRICSDATA.keys():
        if not plot_GMLS and algorithm == 'GMLS':
            continue

        d = util.DICT_METRICSDATA[algorithm]

        if degree is None:
            deg = tuple(d["degree"].keys())[-1]
        else:
            if degree not in d['degree'].keys():
                continue
            deg = degree
        f_metrics = util.get_MIRAMetricsFile(
          refinement, algorithm,
          srcMeshType, srcResolution, 
          tgtMeshType, tgtResolution, 
          deg, var)

        if log:
            print(f'Metrics: {f_metrics}')
        df = pd.read_csv(f_metrics)[metric]
        y = df.drop(df.index[0]).values
        ds[algorithm] = y
        lst_label.append(f'{algorithm} ({d["degree"][deg]["label"]})')


    if "function" in util.DICT_METRIC[metric].keys():
        for algorithm in ds.keys():
            ds[algorithm] = util.DICT_METRIC[metric]["function"](ds[algorithm])
    if log:
        for algorithm in ds.keys():
            y = ds[algorithm]
            print(f'{algorithm:<{util.CLEN_ALGORITHM}s} '\
                  f'0: {y[0]:9.2e} {util.ITERMAX}: {y[-1]:9.2e}, '\
                  f'isnan: {np.isnan(y).any():1b} '\
                  f'min: {np.nanmin(y):9.2e}, max: {np.nanmax(y):9.2e}, '\
                  f'mean: {np.nanmean(y):9.2e}')

    f_fig = util.get_metricsFigFile(
      refinement, srcMeshType, srcResolution, tgtMeshType, tgtResolution,
      metric, degree, var, figname_add)
    os.makedirs(os.path.dirname(f_fig), exist_ok=True)

    fig = plt.figure(figsize=(10,6))
    ax = fig.add_subplot(position=(0.1, 0.1, 0.6, 0.8))
    for algorithm, label in zip(list(ds.keys()), lst_label):
        if not plot_GMLS and algorithm == 'GMLS':
            continue

        #y = ds[algorithm][metric].values.astype(np.float64)
        y = ds[algorithm]
        if algorithm == 'SPRING':
            color = util.COLOR_ALGORITHM_SPRING
            zorder = 2
        else:
            color = util.DICT_METRICSDATA[algorithm]["color"]
            zorder = 1

        ax.plot(x, y, label=label, color=color, zorder=zorder)
        ax.scatter(x, y, s=10, marker='o', color=color, zorder=zorder)

    _, _, ymin_, ymax_ = ax.axis()
    if ymin is None:
        ymin = ymin_
    if ymax is None:
        ymax = ymax_
    xticks = np.arange(0, util.ITERMAX+1, util.ITERINT_TICK)
    yticks = ax.get_yticks()
    kw = dict(linewidth=0.5, color='silver', zorder=0)
    ax.hlines(yticks, x.min(), x.max(), **kw)
    ax.vlines(xticks, ymin, ymax, **kw)
    ax.set_xlim(x.min(), x.max())
    ax.set_ylim(ymin, ymax)

    ax.legend(bbox_to_anchor=(1.05, 1.0), loc='upper left', borderaxespad=0, fontsize=12)
    ax.set_xlabel('Remap iteration', fontsize=12)
    ax.set_ylabel(util.DICT_METRIC[metric]["label"], fontsize=12)
    ax.set_title(f'{srcMeshType}{srcResolution}_{tgtMeshType}{tgtResolution} {var}', fontsize=12)
    if log:
        print(f'Fig: {f_fig}')
    plt.savefig(f_fig, bbox_inches='tight')
    plt.show()


def plot_consistency():

    import pandas as pd
    import numpy as np
    import matplotlib.pyplot as plt

    degree = 0
    lst_algorithm = ['SPRING'] + \
                    [a for a in util.DICT_METRICSDATA \
                     if degree in util.DICT_METRICSDATA[a]['degree'].keys()]
    #lst_metric = ('GC', 'GL1', 'GL2')
    lst_metric = ('GC', 'GMaxE', 'GMinE')

    lst_meshTypePair = (
        ('u', 'CS'  , 'ICOD'), 
        ('u', 'ICOD', 'RLL' ), 
        ('u', 'RLL' , 'CS'  ),
        ('r', 'CS'  , 'ICOD'),
    )

    lst_resolutionPair = {
      'u': ((0, 0), (0, 2), (0, 4), (2, 2), (2, 4), (4, 4)),
      'r': ((0, 0), (0, 2), (2, 2))
    }

    def get_meshPair(refinement, srcMeshType, srcResolution, tgtMeshType, tgtResolution):
        srcMeshType_ = util.DICT_MESH[refinement][srcMeshType]['name_fig']
        tgtMeshType_ = util.DICT_MESH[refinement][tgtMeshType]['name_fig']
        return f'({refinement}) {srcMeshType_}{srcResolution}-{tgtMeshType_}{tgtResolution}'

    lst_meshPair = []
    for (refinement, srcMeshType, tgtMeshType) in lst_meshTypePair:
        for (srcResolution, tgtResolution) in lst_resolutionPair[refinement]:
            lst_meshPair.append(get_meshPair(
                refinement, srcMeshType, srcResolution, tgtMeshType, tgtResolution))

    clen_algorithm = max([len(a) for a in lst_algorithm])
    clen_mesh = max([max([len(a) for a in util.DICT_MESH[refinement]])\
                     for refinement in util.TBL_RFN])
    clen_meshPair = max([sum([len(mesh) for mesh in meshPair])+1 for meshPair in lst_meshPair])

    line = ' ' * (clen_algorithm + 1 + clen_meshPair + 1)
    for metric in lst_metric:
        line += f'{metric:{11*len(util.LST_VAR)}s}'
    print(line)

    line = ' ' * (clen_algorithm + 1 + clen_meshPair + 1)
    for metric in lst_metric:
        for var in util.LST_VAR:
            line += f'{var:11s}'
    print(line)

    ds = {}
    for algorithm in lst_algorithm:
        ds[algorithm] = {}
        for (refinement, srcMeshType, tgtMeshType) in lst_meshTypePair:
            for (srcResolution, tgtResolution) in lst_resolutionPair[refinement]:
                meshPair = get_meshPair(
                    refinement, srcMeshType, srcResolution, tgtMeshType, tgtResolution)
                if meshPair == lst_meshPair[0]:
                    line = f'{algorithm:<{clen_algorithm}s}'
                else:
                    line = ' ' * clen_algorithm
                line += f' {meshPair:<{clen_meshPair}s}'

                ds[algorithm][meshPair] = {}
                for var in util.LST_VAR:
                    f_metrics = util.get_metricsFile(
                        refinement, algorithm,
                        srcMeshType, srcResolution, 
                        tgtMeshType, tgtResolution,
                        degree, var)
                    d = pd.read_csv(f_metrics, skiprows=range(1,2), nrows=1)
                    ds[algorithm][meshPair][var] = d

                for metric in lst_metric:
                    for var in util.LST_VAR:
                        line += f' {ds[algorithm][meshPair][var][metric].iloc[0]:10.3e}'
                print(line)

    # Plot
    f_fig = util.get_consistencyFigFile()

    hratio = (6, 1)
    hspace = 0.05

    marker = dict(A1='o', A2='s', TPO='D', TPW='^', CFR='v')
    linestyle_hlines = dict(linewidth=0.5, color='silver')
    linestyle_vlines = dict(linewidth=0.5, color='silver', linestyle='dashed')

    color = dict(SPRING=util.COLOR_ALGORITHM_SPRING)
    for algorithm in lst_algorithm[1:]:
        color[algorithm] = util.DICT_METRICSDATA[algorithm]['color']

    for metric in lst_metric[:1]:
        fs_title = 12
        fs_ylabel = 10
        x = np.arange(len(lst_meshPair))

        fig = plt.figure(figsize=(16,6))
        axes = fig.subplots(2, len(lst_algorithm), sharex=True, sharey=False, 
            height_ratios=hratio,
            gridspec_kw=dict(bottom=0.2, top=0.90, wspace=0, hspace=hspace))
        uaxes = axes[0,:]
        laxes = axes[1,:]

        ymin, ymax = 1e20, -1e20
        for algorithm, uax, lax in zip(lst_algorithm, uaxes, laxes):
            for var in util.LST_VAR:
                y = np.array([ds[algorithm][meshPair][var][metric].iloc[0] \
                             for meshPair in lst_meshPair])
                if "function" in util.DICT_METRIC[metric].keys():
                    y = util.DICT_METRIC[metric]["function"](y)

                kwargs = dict(marker=marker[var],
                    facecolor='none', edgecolor='k', linewidth=1)

                uax.scatter(x, y, **kwargs)
                lax.scatter(x, [0 if np.isnan(yy) else np.nan for yy in y], **kwargs)

                if not np.isnan(y).all():
                    ymin = min(ymin, np.nanmin(y))
                    ymax = max(ymax, np.nanmax(y))


            uax.set_title(algorithm, fontsize=fs_title)


        if ymin == 1e20:
            ymin, ymax = -1, 1
        yrange = ymax - ymin
        ymin -= yrange * 0.1
        ymax += yrange * 0.05
        xmin = -0.5
        xmax = len(lst_meshPair) - 0.5
        for (uax, lax) in zip(uaxes, laxes):
            uax.spines['bottom'].set_visible(False)
            uax.tick_params(axis='x', which='both', bottom=False, labelbottom=False)

            lax.spines['top'].set_visible(False)
            lax.set_xticks(x, lst_meshPair, rotation=270)

            uax.set_ylim(ymin, ymax)
            uax.hlines(uax.get_yticks(), xmin, xmax, **linestyle_hlines)
            uax.vlines(range(len(lst_meshPair)), ymin, ymax, **linestyle_vlines)

            lax.set_ylim(-1, 1)
            lax.hlines(0, xmin, xmax, **linestyle_hlines)
            lax.vlines(range(len(lst_meshPair)), -1, 1, **linestyle_vlines)
            if 'tick_zero' in util.DICT_METRIC[metric].keys():
                lax.set_yticks([0], [util.DICT_METRIC[metric]['tick_zero']])
            else:
                lax.set_yticks([0])

            lax.tick_params(axis='x', direction='in')
            lax.set_xlim(xmin, xmax)
            for ax in (uax, lax):
                ax.tick_params(axis='y', direction='in')

        uax, lax = axes[:,0]
        uax.set_ylabel(util.DICT_METRIC[metric]['label'], fontsize=fs_ylabel)
        for uax, lax in zip(uaxes[1:], laxes[1:]):
            for ax in (uax, lax):
                ax.tick_params(axis='y', labelleft=False)

        # 切断線
        kwargs = dict(clip_on=False, lw=1.0, zorder=1, color='dimgray')
        kx = 0.005
        dx, dy = 0.015, 0.09
        y0 = 1 + np.mean(hratio) * hspace * 0.5

        for lax in laxes:
            lax.plot((-kx-dx, -kx+dx), (y0-dy, y0+dy), transform=lax.transAxes, **kwargs)
            lax.plot((+kx-dx, +kx+dx), (y0-dy, y0+dy), transform=lax.transAxes, **kwargs)
        lax = laxes[-1]
        lax.plot((1+kx-dx, 1+kx+dx), (y0-dy, y0+dy), transform=lax.transAxes, **kwargs)
        lax.plot((1-kx-dx, 1-kx+dx), (y0-dy, y0+dy), transform=lax.transAxes, **kwargs)

    print(f_fig)
    os.makedirs(os.path.dirname(f_fig), exist_ok=True)
    fig.savefig(f_fig, bbox_inches='tight', pad_inches=0.1, dpi=240)

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
      args.isForth=='True', 
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
      args.isForth=='True', args.variable, args.iteration,
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
    parser.add_argument('--val', action='store_false')
    parser.add_argument('--vmin', type=float)
    parser.add_argument('--vmax', type=float)
    parser.add_argument('--diff', action='store_false')
    parser.add_argument('--dmax', type=float)
    parser.add_argument('--overwrite', action='store_true')
    parser.add_argument('--log', action='store_false')
    args = parser.parse_args()

    plot_field(
      args.srcRefinement, args.srcMeshType, args.srcResolution, 
      args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
      args.mesh, args.variable, args.iteration,
      args.val, args.vmin, args.vmax, 
      args.diff, args.dmax,
      args.overwrite, 
      args.log)

elif job == 'make_NetCDF':
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

    make_NetCDF(
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

    make_NetCDF(
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

elif job == 'measure_time':
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    parser.add_argument('srcRefinement', type=str)
    parser.add_argument('srcMeshType', type=str)
    parser.add_argument('srcResolution', type=int)
    parser.add_argument('tgtRefinement', type=str)
    parser.add_argument('tgtMeshType', type=str)
    parser.add_argument('tgtResolution', type=int)
    parser.add_argument('itermax', type=int)
    parser.add_argument('--overwrite_rt', action='store_true')
    parser.add_argument('--overwrite_summary', action='store_true')
    args = parser.parse_args()

    measure_time(
      args.srcRefinement, args.srcMeshType, args.srcResolution,
      args.tgtRefinement, args.tgtMeshType, args.tgtResolution,
      args.itermax,
      args.overwrite_rt, args.overwrite_summary)

elif job == 'list_outputs':
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    parser.add_argument('data', choices=('remapping_table', 'remapped', 'NetCDF', 'metrics'))
    args = parser.parse_args()

    list_outputs(args.data)

elif job == 'calc_convergence_rate':
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    parser.add_argument('refinement', choices=tuple(util.TBL_RFN.keys()))
    parser.add_argument('srcMeshType', choices=util.LST_MESH_TYPE)
    parser.add_argument('tgtMeshType', choices=util.LST_MESH_TYPE)
    parser.add_argument('algorithm', choices=util.LST_ALGORITHM)
    parser.add_argument('order', type=int)
    parser.add_argument('var', choices=util.LST_VAR)
    parser.add_argument('metric', choices=util.LST_METRIC)
    args = parser.parse_args()

    calc_convergence_rate(
      args.refinement, 
      args.srcMeshType, args.tgtMeshType,
      args.algorithm, args.order, args.var, args.metric)

elif job == 'plot_metrics':
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    parser.add_argument('refinement', type=str)
    parser.add_argument('srcMeshType', type=str)
    parser.add_argument('srcResolution', type=int)
    parser.add_argument('tgtMeshType', type=str)
    parser.add_argument('tgtResolution', type=int)
    parser.add_argument('metric', type=str)
    parser.add_argument('variable', type=str)
    parser.add_argument('--min', type=float)
    parser.add_argument('--max', type=float)
    parser.add_argument('--degree', type=int, default=None)
    parser.add_argument('--GMLS', action='store_false')
    parser.add_argument('--figadd', type=str)
    parser.add_argument('--overwrite', action='store_true')
    parser.add_argument('--log', action='store_false')
    args = parser.parse_args()

    plot_metrics(
      args.refinement, 
      args.srcMeshType, args.srcResolution,
      args.tgtMeshType, args.tgtResolution,
      args.metric, args.degree, args.variable, 
      args.min, args.max,
      args.GMLS,
      args.figadd, 
      args.overwrite,
      args.log)

elif job == 'plot_consistency':
    parser = argparse.ArgumentParser()
    parser.add_argument('job')
    args = parser.parse_args()

    plot_consistency()

else:
    raise Exception(f'Invalid value in `job`: {job}')

