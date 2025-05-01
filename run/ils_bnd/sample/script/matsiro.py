def set_matsiro(cmf):
    return dict(
      ncx = cmf['ncx'], ncy = cmf['ncy'],
      ndx = cmf['ndx'], ndy = cmf['ndy'],
      west = cmf['west'], east = cmf['east'], south = cmf['south'], north = cmf['north'],
      mdx_1deg = int(cmf['ndx'] / (cmf['east']-cmf['west'])),
      mdy_1deg = int(cmf['ndy'] / (cmf['north']-cmf['south'])),
    )
