import sys
import csv
import numpy as np
MDI = np.ma.masked

# ----------------------------------------------------------------------
class Sites():
    '''Class containing site details read from a site file (in csv format
       with a header line).  Details are held in a dictionary where key
       is WMO block/station (string) and value is the WOW site ID.
    '''

    def __init__(self, site_file):

        self.site_lookup = {}
        fp = open(site_file)
        try:
            with fp as sites:
                site_reader = csv.DictReader(sites)
                for site in site_reader:
                    mo_site = site['Met_Office_Id']
                    if mo_site in self.site_lookup:
                        print 'WARNING: duplicate site in ', site_file,\
                            mo_site
                    else:
                        self.site_lookup[mo_site] = site['Site Id']
        except:
            print 'ERROR: reading site data from ', site_file
            sys.exit(2)
        finally:
            fp.close()
        self.count = len(self.site_lookup)

    def lookup(self, block, station):
        '''Returns WOW site id for the given block and station number, or
           None if not found.
        '''

        id = "{:02d}{:03d}".format(block, station)
        if id in self.site_lookup:
            return self.site_lookup.get(id)
        else:
            return None

    def required(self, obs, i):
        '''Checks if the Ith observation is in the site table and returns
           true or False accordingly.
        '''
        block = obs['WMO_BLCK_NMBR'][i]
        station = obs['WMO_STTN_NMBR'][i]
        if block == MDI or station == MDI:
            return False
        else:
            id = self.lookup(block, station)
            if id:
                return True
            else:
                return False


