import os
import json


ITERMAX = 1000

PROGRAM_REMAP = './bin/std/remap.exe'
SCRIPT_PLOT_FIELD = 'bin/std/mkfig_remap.py'
SCRIPT_CANGAMETRICSDRIVER = 'src/MIRA/CANGAMetricsDriver.py'

NCMESHDIR = '../../ext/MIRA-Datasets/Meshes'
BINMESHDIR = '../../dat/mesh/MIRA'

MESHDICT = json.load(open('mesh.json', 'r'))

TBL_VAR = dict(
  TPW = 'TotalPrecipWater',
  CRF = 'CloudFraction'   ,
  TOP = 'Topography'      ,
  A1  = 'AnalyticalFun1'  ,
  A2  = 'AnalyticalFun2'  ,
)

LST_RANGE = dict(
  TPW = (0., 70.),
)

COLOR_TPW = (
  (  0,   0, 255),  #     - 5
  (  3,  58, 177),  #  5 - 10
  (  5, 121,  93),  # 10 - 15
  (  8, 179,  15),  # 15 - 20
  ( 88, 204,  10),  # 20 - 25
  (169, 228,   5),  # 25 - 30
  (255, 255,   0),  # 30 - 35
  (255, 225,   0),  # 35 - 40
  (255, 195,   0),  # 40 - 45
  (255, 165,   0),  # 45 - 50
  (255, 107,   0),  # 50 - 55
  (255,  54,   0),  # 55 - 60
  (255,   0,   0),  # 60 - 65
  (200,   0,  44),  # 65 - 70
  (142,   0,  92),  # 70 - 75
  ( 87,   0, 136),  # 75 -
)

COLOR_TPO = (
  (  0,   0, 255),  # -11000 - -10500
  (  1,  22, 225),  # -10500 - -10000
  (  2,  45, 195),  # -10000 -  -9500
  (  3,  67, 165),  #  -9500 -  -9000
  (  4,  90, 135),  #  -9000 -  -8500
  (  5, 116,  99),  #  -8500 -  -8000
  (  6, 139,  69),  #  -8000 -  -7500
  (  7, 161,  39),  #  -7500 -  -7000
  ( 14, 181,  15),  #  -7000 -  -6500
  ( 45, 190,  13),  #  -6500 -  -6000
  ( 76, 200,  11),  #  -6000 -  -5500
  (107, 209,   9),  #  -5500 -  -5000
  (138, 219,   7),  #  -5000 -  -4500
  (169, 228,   5),  #  -4500 -  -4000
  (199, 238,   3),  #  -4000 -  -3500
  (236, 249,   1),  #  -3500 -  -3000
  (255, 250,   0),  #  -3000 -  -2500
  (255, 239,   0),  #  -2500 -  -2000
  (255, 227,   0),  #  -2000 -  -1500
  (255, 216,   0),  #  -1500 -  -1000
  (255, 204,   0),  #  -1000 -   -500
  (255, 193,   0),  #   -500 -      0
  (255, 181,   0),  #      0 -    500
  (255, 170,   0),  #    500 -   1000
  (255, 153,   0),  #   1000 -   1500
  (255, 128,   0),  #   1500 -   2000
  (255, 107,   0),  #   2000 -   2500
  (255,  87,   0),  #   2500 -   3000
  (255,  66,   0),  #   3000 -   3500
  (255,  45,   0),  #   3500 -   4000
  (255,  25,   0),  #   4000 -   4500
  (255,   4,   0),  #   4500 -   5000
  (238,   0,  14),  #   5000 -   5500
  (217,   0,  31),  #   5500 -   6000
  (196,   0,  48),  #   6000 -   6500
  (171,   0,  68),  #   6500 -   7000
  (150,   0,  85),  #   7000 -   7500
  (129,   0, 102),  #   7500 -   8000
  (108,   0, 119),  #   8000 -   8500
  ( 87,   0, 136),  #   8500 - 
)

COLOR_CFR = (
  (  0,   0, 255),  # 0.00 - 0.05
  (  2,  45, 195),  # 0.05 - 0.10
  (  4,  94, 129),  # 0.10 - 0.15
  (  6, 139,  69),  # 0.15 - 0.20
  ( 20, 183,  14),  # 0.20 - 0.25
  ( 82, 202,  11),  # 0.25 - 0.30
  (150, 223,   6),  # 0.30 - 0.35
  (212, 242,   2),  # 0.35 - 0.40
  (255, 246,   0),  # 0.40 - 0.45
  (255, 223,   0),  # 0.45 - 0.50
  (255, 197,   0),  # 0.50 - 0.55
  (255, 174,   0),  # 0.55 - 0.60
  (255, 136,   0),  # 0.60 - 0.65
  (255,  95,   0),  # 0.65 - 0.70
  (255,  50,   0),  # 0.70 - 0.75
  (255,   8,   0),  # 0.75 - 0.80
  (217,   0,  31),  # 0.80 - 0.85
  (175,   0,  65),  # 0.85 - 0.90
  (129,   0, 102),  # 0.90 - 0.95
  ( 87,   0, 136),  # 0.95 - 1.00
)


def get_nk(refinement: str, meshType: str, resolution: int):
    if refinement == 'u':
        if meshType == 'CS':
            nk = 4
        elif meshType == 'ICOD':
            nk = 6
        elif meshType == 'RLL':
            nk = 4
        else:
            raise Exception(f'Invalid value in meshType: {meshType}')
    elif refinement == 'r':
        if meshType == 'CS':
            nk = 4
        elif meshType == 'RLL':
            nk = 4
        else:
            raise Exception(f'Invalid value in meshType: {meshType}')
    else:
        raise Exception(f'Invaid value in refinement: {refinement}')

    return nk


def get_meshType(refinement: str, meshType: str):
    return MESHDICT[get_refinement(refinement)][meshType]['name']


def get_resolution(refinement: str, meshType: str, resolution: int):
    return tuple(MESHDICT[get_refinement(refinement)][meshType]['file'].keys())[resolution]


def get_mesh(refinement: str, meshType: str, resolution: int):
    meshType_ = meshType

    refinement_ = get_refinement(refinement)

    resolution_ = get_resolution(refinement, meshType, resolution)


    meshName = f'MIRA{refinement}{meshType}{resolution}'
    meshDir = f'../../dat/mesh/MIRA/{refinement_}/{meshType_}/{resolution_}'

    nk = get_nk(refinement, meshType, resolution)
    nij = int(os.path.getsize(f'{meshDir}/xyz.bin') / (8*3*nk))

    return meshName, meshDir, nk, nij


def get_refinement(refinement: str):
    if refinement == 'u':
        return 'UniformlyRefined'
    elif refinement == 'r':
        return 'RegionallyRefined'
    else:
        raise Exception(f'Invalid value in refinement: {refinement}')


def get_sForth(isForth: bool):
    if isForth:
        return 'forth'
    else:
        return 'back'


def get_meshNCFile(refinement: str, meshType: str, resolution: int):
    refinement_ = get_refinement(refinement)
    resolution_ = get_resolution(refinement, meshType, resolution)

    u = MESHDICT[refinement_]
    m = u[meshType]
    f = m["file"][resolution_]
    if f is None:
        f = m["file"][tuple(m["file"].keys())[0]]
    return f'{NCMESHDIR}/{refinement_}/{m["name"]}/{f.format(r=resolution_)}'


def get_remapDir(srcMeshName: str, tgtMeshName: str):
    return f'out/remap_iter/{srcMeshName}_to_{tgtMeshName}'


def get_fieldDir(srcMeshName: str, tgtMeshName: str, var: str):
    return f'{get_remapDir(srcMeshName, tgtMeshName)}/field/{var}'


def get_rtDir(srcMeshName: str, tgtMeshName: str, isForth: bool):
    return f'out/remapping_table/{srcMeshName}_to_{tgtMeshName}/{get_sForth(isForth)}'


def get_fieldBinFile(isForth: bool, var: str, i: int):
    if isForth:
        fin_grdval = f'{var}_src_{i:04d}.bin'
        fout_grdval = f'{var}_tgt_{i+1:04d}.bin'
    else:
        fin_grdval = f'{var}_tgt_{i+1:04d}.bin'
        fout_grdval = f'{var}_src_{i+1:04d}.bin'

    return fin_grdval, fout_grdval


def get_fieldNCFile(srcMeshName: str, tgtMeshName: str, var: str):
    return f'{get_fieldDir(srcMeshName, tgtMeshName, var)}/{var}.nc'


def get_metricsFile(srcMeshName: str, tgtMeshName: str, var: str):
    return f'out/metrics/{srcMeshName}_to_{tgtMeshName}_{TBL_VAR[var]}.csv',\
           f'out/metrics/{srcMeshName}_to_{tgtMeshName}.csv'



