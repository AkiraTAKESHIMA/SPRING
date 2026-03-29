import os
import json


ITERMAX = 1000
ITERINT_PLOT = 10

PROGRAM_REMAP = './bin/std/remap.exe'
SCRIPT_PLOT_FIELD = 'bin/std/mkfig_remap.py'
SCRIPT_CANGAMETRICSDRIVER = 'src/MIRA/CANGAMetricsDriver.py'

MIRADATASETDIR = '../../ext/MIRA-Datasets'
MIRAMESHDIR = '{MIRADATASETDIR}/Meshes'
BINMESHDIR = '../../dat/mesh/MIRA'
MIRAMETRICSDIR = f'{MIRADATASETDIR}/MetricsData'
METRICSFIGDIR = 'fig/metrics'

TBL_RFN = dict(
  u = 'UniformlyRefined' ,
  r = 'RegionallyRefined',
)

TBL_VAR = dict(
  TPW = 'TotalPrecipWater',
  CFR = 'CloudFraction'   ,
  TPO = 'Topography'      ,
  A1  = 'AnalyticalFun1'  ,
  A2  = 'AnalyticalFun2'  ,
)

TBL_METRIC = dict(
  GC = r'Global Conservation Error ${L_g}$',
  GL1 = r'Standard Error 1 ${E_{L_1}}$',
  GL2 = r'Standard Error 2 ${E_{L_2}}$',
  GLinf = r'Largest Pointwise Error ${E_{L_max}}$',
  GMaxE = 'Global Overshoot ${|G_{max}|}$',
  GMinE = 'Global Undershoot ${|G_{min}|}$',
  LMaxL1 = 'L1 of Local Maxima ${L_{max,1}}$',
  LMaxL2 = 'L2 of Local Maxima ${L_{max,2}}$',
  LMaxLm = 'Linf of Local Maxima ${L_{max,inf}}$',
  LMinL1 = 'L1 of Local Minima ${L_{min,1}}$',
  LMinL2 = 'L2 of Local Minima ${L_{min,2}}$',
  LMinLm = 'Linf of Local Minima ${L_{min,inf}}$',
  H12T = '',
  H1T = '',
  H12S = '',
  H1S = '',
)

DICT_MESH = {
  "u": {
    "CS": {
      "name": "CS",
      "file": {
        "r16": "sample_NM16_O10_CS-{r}_TPW_CFR_TPO_A1_A2.nc",
        "r32": None,
        "r64": None,
        "r128": None,
        "r256": None
      }
    },
    "ICOD": {
      "name": "ICOD",
      "file": {
        "r16": "sample_NM16_O10_ICOD-{r}_TPW_CFR_TPO_A1_A2.nc",
        "r32": None,
        "r64": None,
        "r128": None,
        "r256": None
      }
    },
    "RLL": {
      "name": "RLL",
      "file": {
        "r30-60": "sample_NM16_O10_RLL-{r}_TPW_CFR_TPO_A1_A2.nc",
        "r90-180": None,
        "r180-360": None,
        "r360-720": None,
        "r720-1440": None
      }
    }
  },
  "r": {
    "CS": {
      "name": "CS",
      "file": {
        "r32": "sample_NM32_O18_{r}_lev1_tr_enhanced_TPW_CFR_TPO_A1_A2.nc",
        "r64": "sample_NM32_O18_{r}_lev2_tr_enhanced_TPW_CFR_TPO_A1_A2.nc",
        "r128": "sample_NM32_O18_{r}_lev1_tr_enhanced_TPW_CFR_TPO_A1_A2.nc"
      }
    },
    "ICOD": {
      "name": "MPAS",
      "file": {
        "r3": "sample_NM32_O18_mpas_{r}_enhanced_TPW_CFR_TPO_A1_A2.nc",
        "r4": None,
        "r5": None
      }
    }
  }
}

DICT_METRICSDATA = {
  "ESMF": {
    "mesh": {
      "u": {
        "CS": {
          "dir": "CS",
          "file": "CS",
          "resolution": ["16", "32", "64", "128", "256"]
        },
        "ICOD": {
          "dir": "MPAS",
          "file": "ICOD",
          "resolution": ["16", "32", "64", "128", "256"]
        },
        "RLL": {
          "dir": "RLL",
          "file": "RLL",
          "resolution": ["16", "32", "64", "128", "256"]
        }
      },
      "r": {
        "CS": {
          "file": "cs",
          "resolution": ["32", "64", "128"]
        },
        "ICOD": {
          "file": "icodr",
          "resolution": ["3", "4", "5"]
        },
      }
    },
    "mode": {
      1: {
        "dir": "conserve",
        "file": "conserve",
        "label": "conserve"
      },
      2: {
        "dir": "conserve2nd",
        "file": "conserve2nd",
        "label": "conserve2nd"
      },
    },
    "color": "mediumorchid",
  },
  "GMLS": {
    "mesh": {
      "u": {
        "CS": {
          "dir": "CS",
          "file": "CS",
          "resolution": ["16", "32", "64", "128", "256"]
        },
        "ICOD": {
          "dir": "MPAS",
          "file": "ICOD",
          "resolution": ["16", "32", "64", "128", "256"]
        },
        "RLL": {
          "dir": "RLL",
          "file": "RLL",
          "resolution": ["16", "32", "64", "128", "256"]
        }
      },
      "r": {
        "CS": {
          "file": "CS",
          "resolution": ["32", "64", "128"],
        },
        "ICOD": {
          "file": "ICOD",
          "resolution": ["32", "64", "128"],
        },
      }
    },
    "mode": {
      1: {
        "dir": "degree-1",
        "file": "O2",
        "label": "p=1"
      },
      2: {
        "dir": "degree-2",
        "file": "O3",
        "label": "p=2"
      },
      3: {
        "dir": "degree-3",
        "file": "O4",
        "label": "p=3"
      },
      4: {
        "dir": "degree-4",
        "file": "O5",
        "label": "p=4"
      }
    },
    "color": "burlywood",
  },
  "GMLS-CAAS": {
    "mesh": {
      "u": {
        "CS": {
          "dir": "CS",
          "file": "CS",
          "resolution": ["16", "32", "64", "128", "256"]
        },
        "ICOD": {
          "dir": "MPAS",
          "file": "ICOD",
          "resolution": ["16", "32", "64", "128", "256"]
        },
        "RLL": {
          "dir": "RLL",
          "file": "RLL",
          "resolution": ["16", "32", "64", "128", "256"]
        }
      },
      "r": {
        "CS": {
          "file": "CS",
          "resolution": ["32", "64", "128"],
        },
        "ICOD": {
          "file": "ICOD",
          "resolution": ["32", "64", "128"],
        },
      },
    },
    "mode": {
      1: {
        "dir": "degree-1",
        "file": "O2",
        "label": "p=1"
      },
      2: {
        "dir": "degree-2",
        "file": "O3",
        "label": "p=2"
      },
      3: {
        "dir": "degree-3",
        "file": "O4",
        "label": "p=3"
      },
      4: {
        "dir": "degree-4",
        "file": "O5",
        "label": "p=4"
      }
    },
    "color": "orange",
  },
  "TempestRemap": {
    "mesh": {
      "u": {
        "CS": {
          "dir": "CS",
          "file": "CS",
          "resolution": ["16", "32", "64", "128", "256"]
        },
        "ICOD": {
          "dir": "MPAS",
          "file": "ICOD",
          "resolution": ["16", "32", "64", "128", "256"]
        },
        "RLL": {
          "dir": "RLL",
          "file": "RLL",
          "resolution": ["30-60", "90-180", "180-360", "360-720", "720-1440"]
        }
      },
      "r": {
        "CS": {
          "file": "cs",
          "resolution": ["32", "64", "128"]
        },
        "ICOD": {
          "file": "icodr",
          "resolution": ["3", "4", "5"]
        },
      }
    },
    "mode": {
      0: {
        "dir": "degree-0",
        "file": "O1",
        "label": "p=0"
      },
      1: {
        "dir": "degree-1",
        "file": "O2",
        "label": "p=1"
      },
      2: {
        "dir": "degree-2",
        "file": "O3",
        "label": "p=2"
      },
      3: {
        "dir": "degree-3",
        "file": "O4",
        "label": "p=3"
      }
    },
    "color": "dodgerblue",
  },
  "WLS-ENOR": {
    "mesh": {
      "u": {
        "CS": {
          "dir": "CS",
          "file": "CS",
          "resolution": ["16", "32", "64", "128", "256"]
        },
        "ICOD": {
          "dir": "MPAS",
          "file": "ICOD",
          "resolution": ["16", "32", "64", "128", "256"]
        },
        "RLL": {
          "dir": "RLL",
          "file": "RLL",
          "resolution": ["16", "32", "64", "128", "256"]
        }
      },
      "r": {
        "CS": {
          "file": "RRMr",
          "resolution": ["16", "32", "64"]
        },
        "ICOD": {
          "file": "MPAS",
          "resolution": ["16", "32", "64"]
        }
      },
    },
    "mode": {
      2: {
        "dir": "degree-2",
        "file": "p=2",
        "label": "p=2"
      },
      3: {
        "dir": "degree-3",
        "file": "p=3",
        "label": "p=3"
      },
      4: {
        "dir": "degree-4",
        "file": "p=4",
        "label": "p=4"
      }
    },
    "color": "mediumseagreen",
  }
}
"""
for alrogithm in DICT_METRICSDATA["r"].keys():
    d = DICT_METRICSDATA["r"][algorithm]
    for refinement in d["mesh"].keys():
        for srcMeshType, tgtMeshType in zip(
        ("CS", "ICOD"), ("ICOD", "RLL"), ("RLL", "CS")):
            for mode in  d["mode"].keys():
                mode_dir = d["mode"][mode]["dir"]
                mode_file = d["mode"][mode]["file"]
                for srcResolution in d["mesh"][meshType]["resolution"]:
                    for tgtResolution in d["mesh"][meshType]["resolution"]:
"""
            

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
        elif meshType == 'ICOD':
            nk = 7
        else:
            raise Exception(f'Invalid value in meshType: {meshType}')
    else:
        raise Exception(f'Invaid value in refinement: {refinement}')

    return nk


def get_meshType(refinement: str, meshType: str):
    return DICT_MESH[refinement][meshType]['name']


def get_resolution(refinement: str, meshType: str, resolution: int):
    return tuple(DICT_MESH[refinement][meshType]['file'].keys())[resolution]


def get_mesh(refinement: str, meshType: str, resolution: int):
    meshType_ = get_meshType(refinement, meshType)

    refinement_ = TBL_RFN[refinement]

    resolution_ = get_resolution(refinement, meshType, resolution)


    meshName = f'MIRA{refinement}{meshType}{resolution}'
    meshDir = f'../../dat/mesh/MIRA/{refinement_}/{meshType_}/{resolution_}'

    nk = get_nk(refinement, meshType, resolution)
    nij = int(os.path.getsize(f'{meshDir}/xyz.bin') / (8*3*nk))

    return meshName, meshDir, nk, nij


def get_sForth(isForth: bool):
    if isForth:
        return 'forth'
    else:
        return 'back'


def get_meshNCFile(refinement: str, meshType: str, resolution: int):
    refinement_ = TBL_RFN[refinement]
    resolution_ = get_resolution(refinement, meshType, resolution)

    m = DICT_MESH[refinement][meshType]
    f = m["file"][resolution_]
    if f is None:
        f = m["file"][tuple(m["file"].keys())[0]]
    return f'{MIRAMESHDIR}/{refinement_}/{m["name"]}/{f.format(r=resolution_)}'


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


def get_MIRAMetricsFile(
  refinement: str, algorithm: str, 
  srcMeshType: str, srcResolution: int, 
  tgtMeshType: str, tgtResolution: int,
  mode: int, var: str):

    d = DICT_METRICSDATA[algorithm]
    srcMesh = d["mesh"][refinement][srcMeshType]
    tgtMesh = d["mesh"][refinement][tgtMeshType]
    mode_dir = d["mode"][mode]["dir"]
    mode_file = d["mode"][mode]["file"]

    s = f'{srcMesh["file"]}{srcMesh["resolution"][srcResolution]}'
    t = f'{tgtMesh["file"]}{tgtMesh["resolution"][tgtResolution]}'
    f = f'metrics_{s}_{t}_{mode_file}_{TBL_VAR[var]}.csv'
    if refinement == 'u':
        return f'{MIRAMETRICSDIR}/{TBL_RFN[refinement]}/{algorithm}/'\
               f'{srcMesh["dir"]}-{tgtMesh["dir"]}/{mode_dir}/{f}'
    elif refinement == 'r':
        return f'{MIRAMETRICSDIR}/{TBL_RFN[refinement]}/{algorithm}/{mode_dir}/{f}'


def get_metricsFigFile(
  refinement: str, 
  srcMeshType: str, srcResolution: int, 
  tgtMeshType: str, tgtResolution: int,
  metric: str, var: str, add: str=''):

    if add == '':
        add_ = 'default'
    else:
        add_ = add

    return f'{METRICSFIGDIR}/{TBL_RFN[refinement]}/'\
           f'{srcMeshType}{srcResolution}-{tgtMeshType}{tgtResolution}/'\
           f'{metric}_{var}_{add_}.png'

