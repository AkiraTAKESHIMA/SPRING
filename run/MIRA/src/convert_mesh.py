import os
import sys
import json
import netCDF4
import numpy as np
import matplotlib.pyplot as plt
import cartopy.crs as ccrs


NCMESHDIR = '../../ext/MIRA-Datasets/Meshes'
BINMESHDIR = '../../dat/mesh/MIRA'

MESHDICT = json.load(open('mesh.json', 'r'))

XYZ_MISS = -1e20
r2d = 180 / np.pi
d2r = np.pi / 180

# array (3,mij,mk)
def getCellVertices(nc, key):
    mij, mk = nc.variables[key].shape
    return nc.variables['coord'][:,nc.variables[key][:].reshape(-1)-1].reshape(3,mij,mk)


def convert(nc):

    """
    # Calc. errors of radius
    c = nc.variables['coord'][:]
    r = np.sqrt((c**2).sum(axis=0))
    r_mean = r.mean()
    errmin = (r.min() - r_mean) / r_mean
    errmax = (r.max() - r_mean) / r_mean
    print(f'r mean: {r_mean}\n'\
          f'  min : {r.min()} (err: {errmin})\n'\
          f'  max : {r.max()} (err: {errmax})')

    # Plot errors of radius
    lats = np.arcsin(c[0]/r) * r2d
    lons = np.arctan2(c[2]/r, c[1]/r) * r2d
    errs = (r - r_mean) / r_mean

    vmax = np.abs(errs).max()

    fig = plt.figure()
    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    ax.scatter(lons, lats, c=errs, cmap=plt.cm.bwr, s=20,
      vmin=-vmax, vmax=vmax)
    ax.coastlines(linewidth=0.5)
    plt.colorbar(aspect=50, pad=0.08, orientation='horizontal')
    plt.show()
    """

    nk = 0
    nij = 0
    for iConn in (1,2,3):
        key = f'connect{iConn}'
        if key not in nc.variables.keys():
            continue
        mij, mk = nc.variables[key].shape
        print(f'{key} mk: {mk}, mij: {mij}')
        nk = max(nk, mk)
        nij += mij
    print(f'nk: {nk}, nij: {nij}')

    xyz = np.full((3,nij,nk), XYZ_MISS)

    ijs = 0
    for iConn in (1,2,3):
        key = f'connect{iConn}'
        if key not in nc.variables.keys():
            continue
        mij, mk = nc.variables[key].shape
        #for ij in range(mij):
        #    xyz[:,ijs+ij,:mk] = getCellVertices(nc, key, ij)
        xyz[:,ijs:ijs+mij,:mk] = getCellVertices(nc, key)
        ijs += mij
    return xyz


for refinement in MESHDICT.keys():
    r = MESHDICT[refinement]
    for meshType in r.keys():
        m = r[meshType]
        for resolution in m["file"].keys():
            f = m["file"][resolution]
            if f is None:
                f = m["file"][tuple(m["file"].keys())[0]]
            f_nc = f'{NCMESHDIR}/{refinement}/{m["name"]}/{f.format(r=resolution)}'
            if not os.path.isfile(f_nc):
                print(f'File not found: {f_nc}')

            if refinement == 'RegionallyRefined': continue

            print(f'In: {f_nc}')
            xyz = convert(netCDF4.Dataset(f_nc, 'r'))
            f_bin = f'{BINMESHDIR}/{refinement}/{m["name"]}/{resolution}/xyz.bin'
            print(f'Out: {f_bin}')
            os.makedirs(os.path.dirname(f_bin), exist_ok=True)
            xyz.tofile(f_bin)
