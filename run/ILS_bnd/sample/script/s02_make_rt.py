import os
import sys
import json

import s00_const as const
import s00_util as util
import s00_conf as conf



def run():
    lst_dataName = ['glcnmo', 'gtopo30', 'hwsd', 'jra55', 'modis']

    if len(sys.argv) != 2:
        raise Exception('Usage: python {} $dataName'.format(sys.argv[0]))
    dataName = sys.argv[1]
    if dataName not in lst_dataName:
        raise Exception(f'Data name "{dataName}" is invalid. Valid data names are as follows:'\
                        + lst_dataName)

    settings = json.load(open('test.json','r'))
    util.adjust_settings(settings)
    for key in settings['f_list_tiles']:
        if key not in lst_dataName:
            raise Exception(f'Data name "{dataName}" is invalid. Valid data names are as follows:'\
                            + lst_dataName + \
                            ' Check the values in the setting file.')


    for landType in settings['landType']:
        f_list_tiles = settings['f_list_tiles'][dataName]
        if not os.path.isfile(f_list_tiles):
            raise Exception('File not found: '+f_list_tiles+\
                          '\nCheck the values in the setting file.')

        for tileName in open(f_list_tiles,'r').readlines():
        #for tileName in ['E140N40']:
            f_conf = os.path.join(settings['directory']['set']['02'], 
                                  f'{dataName}/{landType}/{tileName}.conf')
            os.makedirs(os.path.dirname(f_conf), exist_ok=True)
            print(f_conf)

            fp = open(f_conf,'w')

            if dataName in ['glcnmo', 'gtopo30', 'hwsd', 'jra55']:
                fp.write(conf.conf_remap_latlon(
                  tileName, landType, dataName, 
                  settings[dataName], settings['matsiro'], settings['directory'], settings['options']))
            elif dataName in ['modis']:
                fp.write(conf.conf_remap_modis(
                  tileName, landType, dataName, 
                  settings[dataName], settings['matsiro'], settings['directory'], settings['options']))

            fp.close()


if __name__ == '__main__':
    run()
