import os
import sys
import netCDF4
import numpy as np


itermax = 1000

TABLE_VAR = dict(
  TPW = 'TotalPrecipWater',
  CRF = 'CloudFraction'   ,
  TOP = 'Topography'      ,
  A1  = 'AnalyticalFun1'  ,
  A2  = 'AnalyticalFun2'  ,
)

TABLE_REFINEMENT = dict(
  MIRAu = 'UniformlyRefined',
  MIRAr = 'RegionallyRefined',
)

def get_num_elem(mesh):
    if 'MIRAu' or 'MIRAr' in mesh:
        refinement, meshName, meshRes = mesh.split('-')
        if refinement == 'MIRAu':
            refinementName = 'UniformlyRefined'
        else:
            refinementName = 'RegionallyRefined'
        path_field = f'../../dat/mesh/MIRA/{refinementName}/{meshName}/r{meshRes}/val_A1.bin'
        return int(os.path.getsize(path_field) / 8)
    else:
        raise Exception(f'Invalid input for mesh: {mesh}')

#
# e.g. srcMesh = MIRAu-CS-4
def makeNetCDF(srcMesh, tgtMesh, var):
    dir_field = f'out/remap/{srcMesh}_to_{tgtMesh}/field'

    path_nc = f'{dir_field}/{var}.nc'
    if os.path.isfile(path_nc):
        os.remove(path_nc)
    nc = netCDF4.Dataset(path_nc, 'w', format='NETCDF4')

    ntij = get_num_elem(tgtMesh)
    nsij = get_num_elem(srcMesh)

    var_nc = TABLE_VAR[var]

    dim_iter = nc.createDimension('iteration', None)
    dim_elem_tgt = nc.createDimension('elem_tgt', ntij)
    dim_elem_src = nc.createDimension('elem_src', nsij)

    src = nc.createVariable(f'{var_nc}_remap_src', np.float64, ('iteration', 'elem_src'))
    tgt = nc.createVariable(f'{var_nc}_remap_tgt', np.float64, ('iteration', 'elem_tgt'))
    for i in range(1, itermax+1):
        src[i,:] = np.fromfile(f'{dir_field}/{var}_src_iter{i:04d}.bin')
        tgt[i,:] = np.fromfile(f'{dir_field}/{var}_tgt_iter{i:04d}.bin')
    nc.close()


if __name__ == '__main__':

    srcMesh, tgtMesh, var = sys.argv[1:]

    makeNetCDF(srcMesh, tgtMesh, var)

