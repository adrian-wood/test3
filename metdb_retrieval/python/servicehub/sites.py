import sys
import csv
import numpy as np
from unit_utils import *


# ----------------------------------------------------------------------
class Sites():
    '''Class to determine if a given site ID is required.
    '''

    def __init__(self, site_file):
        '''Currently only set up for METARS where we want all stations
           except those starting EG, and LNDSYN for all stations other
           than block 03.
        '''

        if 'METARS' in site_file:
            self.site_lookup = 'EG'
            self.count = 1
            self.type = 'METARS'
        elif 'LNDSYN' in site_file:
            self.site_lookup = 3
            self.count = 1
            self.type = 'LNDSYN'
        else:
            print('Error: no site details for ', site_file)
            sys.exit(2)

    def required(self, obs, i):
        '''Checks if the Ith observation is required and returns
           true or False accordingly.
        '''
        if self.type == 'METARS':
            id = obs['ICAO_ID'][i]
            if id == MDI or id[0:2] == self.site_lookup:
                return False
            else:
                return True
        elif self.type == 'LNDSYN':
            id = obs['WMO_BLCK_NMBR'][i]
            if id == MDI or id == self.site_lookup:
                return False
            else:
                return True
        else:
            print('Error: no site selection available')
            sys.exit(2)
