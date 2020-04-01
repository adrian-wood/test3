""" PROGRAM       : update_stnmas.py
    PURPOSE       : Update abbreviated station list from differences in 
                    two versions of VOL A.

    USAGE         : update_stnmas.py <vola_1> <vola_2> <stnmas>

    AUTHOR        : Sheila Needham

    REVISION INFO :
    MB-2075: Aug 2020 New

"""
import sys
from pandas import read_csv
import pandas as pd
import numpy as np


def convert_to_decimal(dms):
    '''Convert a lat/lon from degrees, minutes and seconds to
       degrees and tenths.'''
    quad = dms[-1]
    sign = 1
    if quad == 'S' or quad == 'W':
        sign = -1

    secs = dms[-3:-1]
    mins = dms[-6:-4]
    deg = dms[:-7]
    try:
        degrees = (int(deg) + int(mins)/60.0 + int(secs)/3600.0) * sign
    except ValueError:
        print('conversion error on ', dms)
        sys.exit(8)

    return degrees


def convert_to_type(row):
    '''Decide whether station is surface, upper-air or both based
       on remarks.'''

    sub_index = getattr(row, 'IndexSubNbr')
    remarks = getattr(row, 'ObsRems')
    stn_type = ''
    if sub_index == 1.0:
        stn_type = 'U/AIR'
    else:
        if remarks.find('GOS') > -1:
            stn_type = 'SURF'
        if remarks.find('GUAN') > -1:
            if len(stn_type) > 0:
                stn_type += ' & U/AIR'
            else:
                stn_type = 'U/AIR'

    return stn_type


def read_vola(filename):
    '''Read VolA legacy file generated from Oscar by WMO.
    
    Args:
      * filename (str) - path to VOL A file
      
    Returns:
      * vola_df (dataframe) - whole file converted to a pandas
        dataframe with only the columns required by STNMAS.
    
    '''

    VolA = filename

    # Read legacy VolA into a Dataframe
    try:
        pandas_data = read_csv(VolA, delimiter='\t', encoding='utf-8')
    except:
        print('Unable to read ', VolA)
        return None

    # select columns needed for abbreviated list
    subset = pandas_data.loc[:, ['IndexNbr', 'Latitude', 'Longitude',
                                 'StationName', 'Hp', 'Hha',
                                 'ObsRems', 'IndexSubNbr']]
    # and sort into WMO station order
    sorted_subset = subset.sort_values(by=['IndexNbr', 'IndexSubNbr'])

    
    hdrs = ['WMO_ID', 'LAT', 'LONG', 'SENSOR_HT', 'STN_HT', 'TYPE', 'NAME']
    station_list = []

    # Loop over rows of input data
    for row in sorted_subset.itertuples():

        # WMO ID
        wmo_id = getattr(row, 'IndexNbr')
        if np.isnan(wmo_id):
            print('Missing WMO ID - skipping')
            continue
        wmo_id = '{:05.0f}'.format(wmo_id)

        # lat and longs in degrees, minutes and seconds are converted to
        # decimal degrees
        lat_dms = getattr(row, 'Latitude')
        lat = convert_to_decimal(lat_dms)
        lon_dms = getattr(row, 'Longitude')
        lon = convert_to_decimal(lon_dms)

        # Two heights
        sensor_ht = getattr(row, 'Hp')
        if np.isnan(sensor_ht):
            sensor_ht = ''
        else:
            sensor_ht = '{:.0f}'.format(sensor_ht)

        station_ht = getattr(row, 'Hha')
        if np.isnan(station_ht):
            station_ht = ''
        else:
            station_ht = '{:.0f}'.format(station_ht)

        # Station name
        name = getattr(row, 'StationName')

        # Station type - surface or upper-air, or both
        stn_type = convert_to_type(row)

        row = [wmo_id, lat, lon, sensor_ht, station_ht,
                   stn_type, name.strip()]
        station_list.append(row)
        
    return pd.DataFrame(station_list,columns=hdrs)
   
def dataframe_difference(df1, df2, which=None):
    """Find rows which are different between two DataFrames."""
    comparison_df = df1.merge(df2,
                              indicator=True,
                              how='outer')
    if which is None:
        diff_df = comparison_df[comparison_df['_merge'] != 'both']
    else:
        diff_df = comparison_df[comparison_df['_merge'] == which]
    
    return diff_df

if __name__ == "__main__":

    # Get command line argument - input files
    if len(sys.argv) != 4:
        print('ERROR:', sys.argv[0], '- usage <vola_old> <vola_new> <stnmas>')
        sys.exit(8)
    vola1 = sys.argv[1]
    vola2 = sys.argv[2]
    stnmas = sys.argv[3]
    df1 = read_vola(vola1)
    df2 = read_vola(vola2)

    print('NEW STATIONS')
    new_stations = dataframe_difference(df1,df2, which='right_only')
    print(new_stations)
    print('OLD STATIONS')
    old_stations = dataframe_difference(df1, df2, which='left_only')
    print(old_stations)
    print('IN BOTH')
    both = dataframe_difference(df1, df2, which='both')
    print(both)
    