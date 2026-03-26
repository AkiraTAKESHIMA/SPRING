import os
import sys
import subprocess

PROGRAM_REMAP = './bin/std/remap.exe'

LST_VAR = [
  'TPW',
]


def getMesh(isUniform, meshType, resolution):
    if isUniform:
        u = 'u'
        refinement = 'UniformlyRefined'
        if meshType == 'RLL':
            np = 4
        elif meshType == 'CS':
            np = 4
        elif meshType == 'ICOD':
            np = 6
        else:
            raise Exception('Invalid value in meshType: {meshType}')
    else:
        u = 'r'
        refinement = 'RegionallyRefined'
        if meshType == 'CS':
            np = 4
        elif meshType == 'ICOD':
            np = 6
        else:
            raise Exception('Invalid value in meshType: {meshType}')

    meshName = f'MIRA{u}-{meshType}-{resolution}'
    meshDir = f'../../dat/mesh/MIRA/{refinement}/{meshType}/r{resolution}'

    nij = int(os.path.getsize(f'{meshDir}/xyz.bin') / (8*3*np))

    return meshName, meshDir, np, nij


def block_mesh(isUniform, meshType, resolution):
    meshName, meshDir, np, nij = getMesh(isUniform, meshType, resolution)

    s = f'\
[mesh_polygon]\n\
  name: "{meshName}"\n\
  np: {np}\n\
  nij: {nij}\n\
  dir: "{meshDir}"\n\
  f_x_vertex: "xyz.bin", rec=1\n\
  f_y_vertex: "xyz.bin", rec=2\n\
  f_z_vertex: "xyz.bin", rec=3\n\
  arc_parallel: .false.\n\
[end]\n\
'
    return s


def get_refinement(isUniform):
    if isUniform:
        return 'UniformlyRefined'
    else:
        return 'RegionallyRefined'


def get_sForth(isForth):
    if isForth:
        return 'forth'
    else:
        return 'back'


def get_remapDir(srcMeshName, tgtMeshName):
    return f'out/remap_iter/{srcMeshName}_to_{tgtMeshName}'


def get_fieldDir(srcMeshName, tgtMeshName, var):
    return f'{get_remapDir(srcMeshName, tgtMeshName)}/field/{var}'


def get_rtDir(srcMeshName, tgtMeshName, isForth):
    return f'out/remapping_table/{srcMeshName}_to_{tgtMeshName}/{get_sForth(isForth)}'


def get_f_grdval(isForth, var, i):
    if isForth:
        fin_grdval = f'{var}_src_{i:04d}.bin'
        fout_grdval = f'{var}_tgt_{i+1:04d}.bin'
    else:
        fin_grdval = f'{var}_tgt_{i+1:04d}.bin'
        fout_grdval = f'{var}_src_{i+1:04d}.bin'

    return fin_grdval, fout_grdval


def block_remapping_rt(srcMeshName, tgtMeshName, isForth):
    rtDir = get_rtDir(srcMeshName, tgtMeshName, isForth)
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


def block_remapping_remap(srcMeshName, tgtMeshName, isForth, i):
    rtDir = get_rtDir(srcMeshName, tgtMeshName, isForth)
    

    nij = int(os.path.getsize(f'{rtDir}/grid.bin') / 8)

    s = f'\
[remapping]\n\
  dir: "{rtDir}"\n\
  length_rt: {nij}\n\
  fin_rt_sidx: "grid.bin", rec=1\n\
  fin_rt_tidx: "grid.bin", rec=2\n\
  fin_rt_area: "area.bin"\n\
  fin_rt_coef: "coef.bin"\n\
'

    for var in LST_VAR:
        fin_grdval, fout_grdval = get_f_grdval(isForth, var, i)

        s += f'\
\n\
  dir: "{get_fieldDir(srcMeshName, tgtMeshName, var)}"\n\
  fin_grdval : "{fin_grdval}"\n\
  fout_grdval: "{fout_grdval}"\n\
'

    s += f'\
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
      srcIsUniform, srcMeshType, srcResolution,
      tgtIsUniform, tgtMeshType, tgtResolution,
      isForth
    ):

    srcMeshName, *_ = getMesh(srcIsUniform, srcMeshType, srcResolution)
    tgtMeshName, *_ = getMesh(tgtIsUniform, tgtMeshType, tgtResolution)
    sForth = get_sForth(isForth)

    print(f'Making table {srcMeshName} to {tgtMeshName} ({sForth})')

    f_conf = f'conf/remapping_table/{srcMeshName}_to_{tgtMeshName}/{sForth}.conf'
    os.makedirs(os.path.dirname(f_conf), exist_ok=True)
    print(f'  conf: {f_conf}')
    with open(f_conf, 'w') as wf:
        wf.write(f'\
#\n\
path_report: "{get_rtDir(srcMeshName, tgtMeshName, isForth)}/report.txt"\n\
\n')

        if isForth:
            wf.write(f'\
{block_mesh(srcIsUniform, srcMeshType, srcResolution)}\
\n\
{block_mesh(tgtIsUniform, tgtMeshType, tgtResolution)}\
\n')
        else:
            wf.write(f'\
{block_mesh(tgtIsUniform, tgtMeshType, tgtResolution)}\
\n\
{block_mesh(srcIsUniform, srcMeshType, srcResolution)}\
\n')

        wf.write(f'\
{block_remapping_rt(srcMeshName, tgtMeshName, isForth)}\
\n\
{block_options()}\
')

    cp = subprocess.run(
           [PROGRAM_REMAP, f_conf], 
           capture_output=True,
           #check=True,
         )

    f_log = f'{get_rtDir(srcMeshName, tgtMeshName, isForth)}/log'
    f_log_stdout = f_log + '.out'
    f_log_stderr = f_log + '.err'
    print(f'  stdout: {f_log_stdout}')
    print(f'  stderr: {f_log_stderr}')
    os.makedirs(os.path.dirname(f_log), exist_ok=True)
    with open(f_log_stdout, 'w') as wf:
        wf.write(cp.stdout.decode())
    with open(f_log_stderr, 'w') as wf:
        wf.write(cp.stderr.decode())
    if cp.returncode != 0:
        print(f'returncode: {cp.returncode}')


def remap_field(
      srcIsUniform, srcMeshType, srcResolution,
      tgtIsUniform, tgtMeshType, tgtResolution, 
      isForth, i):

    srcMeshName, *_ = getMesh(srcIsUniform, srcMeshType, srcResolution)
    tgtMeshName, *_ = getMesh(tgtIsUniform, tgtMeshType, tgtResolution)
    sForth = get_sForth(isForth)

    print(f'Remapping {srcMeshName} to {tgtMeshName} ({sForth}) iter{i}')

    f_conf = f'conf/remap_iter/{srcMeshName}_to_{tgtMeshName}/iter{i:04d}_{sForth}.conf'
    os.makedirs(os.path.dirname(f_conf), exist_ok=True)
    print(f'  conf: {f_conf}')
    with open(f_conf, 'w') as wf:
        wf.write(f'\
#\n\
path_report: "{get_remapDir(srcMeshName, tgtMeshName)}/report/iter{i:04d}_{sForth}.txt"\n\
\n')
        if isForth:
            wf.write(f'\
{block_mesh(srcIsUniform, srcMeshType, srcResolution)}\
\n\
{block_mesh(tgtIsUniform, tgtMeshType, tgtResolution)}\
\n')
        else:
            wf.write(f'\
{block_mesh(tgtIsUniform, tgtMeshType, tgtResolution)}\
\n\
{block_mesh(srcIsUniform, srcMeshType, srcResolution)}\
\n')
        wf.write(f'\
{block_remapping_remap(srcMeshName, tgtMeshName, isForth, i)}\
\n\
{block_options()}\
')

    # Prepare original fields
    if i == 0:
        copy_field_org(srcMeshName, tgtMeshName)


    # Remap
    cp = subprocess.run(
           [PROGRAM_REMAP, f_conf], 
           capture_output=True,
           #check=True,
         )

    f_log = f'{get_remapDir(srcMeshName, tgtMeshName)}/log/iter{i:04d}_{sForth}'
    f_log_stdout = f_log + '.out'
    f_log_stderr = f_log + '.err'
    print(f'  stdout: {f_log_stdout}')
    print(f'  stderr: {f_log_stderr}')
    os.makedirs(os.path.dirname(f_log), exist_ok=True)
    with open(f_log_stdout, 'w') as wf:
        wf.write(cp.stdout.decode())
    with open(f_log_stderr, 'w') as wf:
        wf.write(cp.stderr.decode())
    if cp.returncode != 0:
        print(f'returncode: {cp.returncode}')

    return cp.returncode


def copy_field_org(srcMeshName, tgtMeshName):
    for var in LST_VAR:
        f_src_ini, _ = get_f_grdval(isForth, var, 0)   # src_iter0000
        _, f_tgt_ini = get_f_grdval(isForth, var, -1)  # tgt_iter0000

        fieldDir = get_fieldDir(srcMeshName, tgtMeshName, var)

        if not os.path.isfile(f'{fieldDir}/{f_src_ini}'):
            refine = get_refinement(srcIsUniform)
            f_src_org = f'../../dat/mesh/MIRA/{refine}/{srcMeshType}/r{srcResolution}/val_{var}.bin'
            os.makedirs(fieldDir, exist_ok=True)
            subprocess.run(
              ['cp', f_src_org, f'{fieldDir}/{f_src_ini}'], 
              check=True
            )

        if not os.path.isfile(f'{fieldDir}/{f_tgt_ini}'):
            refine = get_refinement(srcIsUniform)
            f_tgt_org = f'../../dat/mesh/MIRA/{refine}/{tgtMeshType}/r{tgtResolution}/val_{var}.bin'
            os.makedirs(fieldDir, exist_ok=True)
            subprocess.run(
              ['cp', f_tgt_org, f'{fieldDir}/{f_tgt_ini}'], 
              check=True
            )



if __name__ == '__main__':
    job = sys.argv[1]

    if job == 'rt':
        srcIsUniform = sys.argv[2] == 'True'
        srcMeshType = sys.argv[3]
        srcResolution = int(sys.argv[4])
        tgtIsUniform = sys.argv[5] == 'True'
        tgtMeshType = sys.argv[6]
        tgtResolution = int(sys.argv[7])
        isForth = sys.argv[8] == 'True'

        make_remapping_table(
          srcIsUniform, srcMeshType, srcResolution, 
          tgtIsUniform, tgtMeshType, tgtResolution,
          isForth)

    elif job == 'remap':
        srcIsUniform = sys.argv[2] == 'True'
        srcMeshType = sys.argv[3]
        srcResolution = int(sys.argv[4])
        tgtIsUniform = sys.argv[5] == 'True'
        tgtMeshType = sys.argv[6]
        tgtResolution = int(sys.argv[7])
        isForth = sys.argv[8] == 'True'
        i = int(sys.argv[9])

        remap_field(
          srcIsUniform, srcMeshType, srcResolution, 
          tgtIsUniform, tgtMeshType, tgtResolution,
          isForth, i)
