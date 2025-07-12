import os
import sys
import copy


def update_directory(d):
    tmp = d['tmp']

    d['set'] = {}
    for key in tmp:
        if key == 'top':
            continue
        d['set'][key] = os.path.join('../set', tmp[key])

    for key in tmp:
        if key == 'top':
            continue
        tmp[key] = os.path.join(tmp['top'], tmp[key])
    del(tmp['top'])

    d['tmp'] = tmp



def set_matsiro(cmf):
    return dict(
      ncx = cmf['ncx'], ncy = cmf['ncy'],
      ndx = cmf['ndx'], ndy = cmf['ndy'],
      west = cmf['west'], east = cmf['east'], south = cmf['south'], north = cmf['north'],
      mdx_1deg = int(cmf['ndx'] / (cmf['east']-cmf['west'])),
      mdy_1deg = int(cmf['ndy'] / (cmf['north']-cmf['south'])),
    )


def adjust_settings(settings):
    update_directory(settings['directory'])
    settings['matsiro'] = set_matsiro(settings['cmf'])

