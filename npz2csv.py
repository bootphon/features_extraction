
"""This script contains functions to convert numpy savez file into CSV.

The npz files must respect the following conventions:
They must contains 2 arrays:

- a 1D-array named 'times'
- a 2D-array named 'features', the 'feature' dimension along the columns and\
 the 'time' dimension along the lines

"""
# -*- coding: utf-8 -*-

import os
import numpy as np


def convert(npz_folder, csv_filename='./features.features'):
    """Append a folder of numpy ndarray files in npz format into csv file.

    Parameters
    ----------
    npz_folder : dirname
        The folder containing the npz files to convert.
    csv_filename : filename
        The output csv file.
    """
    files = os.listdir(npz_folder)
    res_ = ''
    for f in files:
        base_name = f.split('.')[0] 
        data = np.load(os.path.join(npz_folder, f))
        for time_, feats_ in zip(data['time'], data['features']):
            feat_str = ','.join(['%f'%x for x in feats_])
            res_ = res_ +  '{name},{time},{feats}\n'.format(name=base_name, time=time_, feats=feat_str)
    with open(csv_filename, 'w') as f:
        f.write(res_)


