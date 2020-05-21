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
import pdb
hdrs = ['WMO_ID', 'LAT', 'LONG', 'SENSOR_HT', 'STN_HT', 'TYPE', 'NAME']
  

def convert_to_decimal(dms):
    '''Convert a lat/lon from degrees, minutes and seconds to
       degrees and tenths.
       
    Args:
       * dms (str) - e.g. '34 25 00N'
       
    Returns:
       * degrees (str) - degrees and minutes to 3dp e.g. '   34.417'
       
    Exceptions:
       * exit 8 on invalid input string

       N.B. No validation done on range of values.
    '''

    if len(dms) < 6:
        print(f'conversion error on {dms}')
        sys.exit(8)

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
       on remarks.
       
    Args:
       * row (named tuple) - one row from VolA dataFrame
    
    Returns:
       * stn_type (str) - one of blank, SURF, U/AIR or SURF & U/AIR
    
    Exceptions:
       exit(8) - if row attributes not found
       
    '''

    stn_type = ''
    try:
        sub_index = row.IndexSubNbr
        remarks = row.ObsRems
    except AttributeError:
        print(f'Invalid row from Vol A dataframe: {row}')
        sys.exit(8)
    
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


def read_stnmas(filename):
    '''Read MetDB abbreviated station list

    Args:
        * filename (str) : location of abbreviated STNMAS

    Returns:
        * df (dataframe) : Pandas dataframe of STNMAS content
    
    Errors:
        * Returns None if IOError on filename
    '''
    try:
        with open(filename, 'r') as f:
            lines = f.readlines()
    except IOError:
        print(f'IOError reading {filename}')
        return None

    lines = lines[2:]

    hdrs = ['WMO_ID', 'LAT', 'LONG', 'SENSOR_HT', 'STN_HT', 'TYPE', 'NAME']
    stations = []
    pdb.set_trace()
    for line in lines:
        if line.strip() == '':
            break
        wmo = line[1:6] # check
        if wmo == '99999':
            break
        lat = line[8:15] # check
        lon = line[16:24]
        ht1 = line[29:33]    # Sensor height
        ht2 = line[37:41]    # Station height
        stype = line[42:56].strip()
        name = line[57:].strip()

        stations.append((wmo, lat, lon, ht1, ht2, stype, name))

    df = pd.DataFrame(stations, columns=hdrs)
    # print(df)
    return df


def write_stnmas(df, filename):
    '''Write out abbreviated stationlist from dataframe.

    '''
    
    line1 = '  WMO      LAT      LONG   SENSOR   STN   STN            STATION\n'
    line2 = ' INDEX   DEG/THS  DEG/THS  HT (M)  HT (M) TYPE           NAME\n'
    nines = ' 99999'

    print(f'Writing to {filename}')
    with open(filename, 'w') as f:
        f.write(line1)
        f.write(line2)
        for row in df.itertuples(index=False):
            f.write(" {:5}  {:7} {:8}     {:4}    {:4} {:<15}{:<48}\n".
                     format(row.WMO_ID, row.LAT, row.LONG,
                            row.SENSOR_HT, row.STN_HT, row.TYPE, row.NAME))
        f.write(nines)
    return

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
    pdb.set_trace()
    # Loop over rows of input data
    for row in sorted_subset.itertuples():

        # WMO ID
        wmo_id = row.IndexNbr
        if np.isnan(wmo_id):
            # print('Missing WMO ID - skipping')
            continue
        wmo_id = '{:05.0f}'.format(wmo_id)

        # lat and longs in degrees, minutes and seconds are converted to
        # decimal degrees
        lat_dms = row.Latitude
        lat = convert_to_decimal(lat_dms)
        lat = '{:7.3f}'.format(lat)
        lon_dms = row.Longitude
        lon = convert_to_decimal(lon_dms)
        lon = '{:8.3f}'.format(lon)

        # Two heights
        sensor_ht = row.Hp
        if np.isnan(sensor_ht):
            sensor_ht = '    '
        else:
            sensor_ht = '{:4.0f}'.format(sensor_ht)

        station_ht = row.Hha
        if np.isnan(station_ht):
            station_ht = ''
        else:
            station_ht = '{:4.0f}'.format(station_ht)

        # Station name
        name = row.StationName

        # Station type - surface or upper-air, or both
        stn_type = convert_to_type(row)

        row = [wmo_id, lat, lon, sensor_ht, station_ht,
                   stn_type, name.strip()]
        station_list.append(row)
        
    return pd.DataFrame(station_list,columns=hdrs)
   
def dataframe_difference(df1, df2, which=None):
    '''Compare two dataframes and return differences.
    
    Merges two dataframes on WMO_ID and TYPE compound key values using an
    outer join which produces a dataframe with columns for left (x) and 
    right (y) tables and a merge column that says where the content came from.
    The options are 'both' for key in both tables, 'left-only' for key only in
    df1 or 'right-only' for key only in df2.
    Return the subset of rows matching 'which' type.

    Args:
      * df1 (dataframe) - left-hand or oldest content
      * df2 (dataframe) - right-hand or newest content
      * which (str) - type of comparison
                      'both', 'left-only', 'right-only'
                      or None (default). None is equivalent to
                      a union of both tables.
      
    Returns:
      * diff_df (dataframe) - subset of rows dependent on 'which' selection.
                              Could be None.
    
    '''
    merged = df1.merge(df2, on=['WMO_ID','TYPE'],
                              indicator=True,
                              how='outer')
    
    if which is None:
        diff_df = merged[merged['_merge'] != 'both']
    else:
        diff_df = merged[merged['_merge'] == which]
    
    return diff_df


def add_stations(df1, df2):
    '''Insert df1 into df2.

    Args:
        * df1 (DataFrame) - new stations from the latest Vol A
        * df2 (DataFrame) - StationMaster

    Returns:
        * df3 (DataFrame) - Updated StationMaster

    '''
    df3 = None
    # merge frames; this should consist of left_only and right_only as they
    # are meant to be new stations but it's possible a station might already
    # be in stnmas, so ignore those 'new' stations by selecting
    # 'left_only' to concatenate.
    both = pd.merge(df1, df2, on=['WMO_ID', 'TYPE'], how='outer', indicator=True)
    condition = (both['_merge'] == 'left_only')
    new_stations = both[condition]
    # now select just those new stations from the original input and add them
    # to the end of stnmas
    condition = (df1['WMO_ID'].isin(new_stations.WMO_ID)) & (df1['TYPE'].isin(new_stations.TYPE))
    additions = df1[condition]
    df3 = pd.concat([df2, additions])
    # finally, sort
    df3 = df3.sort_values(by=['WMO_ID', 'TYPE'])

    warning = (both['_merge'] == 'both')
    if not both[warning].empty:
        print('WARNING: New stations already in STNMAS:')
        print(both[warning])

    return df3


def delete_stations(df1, df2):
    '''Deletes df1 from df2
    '''
    pdb.set_trace()
    both = pd.merge(df1, df2, on=['WMO_ID'], how='outer', indicator=True)

    condition = (both['_merge'] == 'both')
    if both[condition].empty:
        print('No stations to delete')
        return df2

    old_stations = both[condition]
    # now find those stations in df2
    condition = (df2['WMO_ID'].isin(old_stations.WMO_ID))
    
    deletions = df2[condition]
    print('Deleting these entries:')
    print(deletions)
    rows = deletions.index.tolist()
    df3 = df2.drop(rows)
    
    input('Press enter to continue...')
    return df3

def update_stations(df1, df2):
    '''Updates df2 from df1
    
    Args:
        * df1 - list of new entries to replace those in df2
        
    '''
    # update is just delete and insert...?
    removed = delete_stations(df1, df2)
    updated = add_stations(df1, removed)
    df3 = updated
    return df3


if __name__ == "__main__":

    # Get command line argument - input files
    if len(sys.argv) != 4:
        print('ERROR:', sys.argv[0], '- usage <vola_old> <vola_new> <stnmas>')
        sys.exit(8)
    vola1 = sys.argv[1]
    vola2 = sys.argv[2]
    stnmas = sys.argv[3]
    pdb.set_trace()
    df1 = read_vola(vola1)
    df2 = read_vola(vola2)

    print(f'NEW STATIONS - WMO_ID in {vola2} only')
    selection = dataframe_difference(df1,df2, which='right_only')
    #print(selection)
    # Re-format so it looks like a STNMAS entry
    new_stations = selection[['WMO_ID', 'LAT_y', 'LONG_y', 'SENSOR_HT_y', 'STN_HT_y', 'TYPE', 'NAME_y']]
    new_stations.columns = hdrs
    new_stations = new_stations.reset_index(drop=True)
    print(new_stations)
    input("Press Enter to continue...")

    print(f'OLD STATIONS - WMO_ID in {vola1} only')
    selection = dataframe_difference(df1, df2, which='left_only')
    old_stations = selection[['WMO_ID', 'LAT_x', 'LONG_x', 'SENSOR_HT_x', 'STN_HT_x', 'TYPE', 'NAME_x']]
    old_stations.columns = hdrs
    old_stations = old_stations.reset_index(drop=True)
    print(old_stations)
    input("Press Enter to continue...")
    
    # now find entries that are in both tables
    both = dataframe_difference(df1, df2, which='both')
    # Compare colums from left and right tables to select a set where there are differences
    changes = both[(both.LAT_x != both.LAT_y)  | (both.LONG_x != both.LONG_y) |
                    (both.SENSOR_HT_x != both.SENSOR_HT_y) |
                    (both.STN_HT_x != both.STN_HT_y) |
                    (both.NAME_x != both.NAME_y)]
    print(f'UPDATED STATIONS - differences between {vola1} and {vola2} ')
    changed_stations = changes[['WMO_ID', 'LAT_y', 'LONG_y', 'SENSOR_HT_y', 'STN_HT_y', 'TYPE', 'NAME_y']]
    changed_stations.columns = hdrs 
    changed_stations = changed_stations.reset_index(drop=True)
    print(changed_stations)
    input("Press Enter to continue...")
    # find these in stnmas and update them
    #pd.options.display.width = 0
    #with pd.option_context('display.max_rows', None, 'display.max_columns', None):
    #    print(diffs)
    stnmas_df = read_stnmas(stnmas)
    stnmas_df = stnmas_df.reset_index(drop=True)
    #print('Original stnmas')
    print(stnmas_df)
    input("Press Enter to continue...")

    stnmas_df = add_stations(new_stations, stnmas_df)
    #print('stations added')
    #print(stnmas_df)
    input("Press Enter to continue...")
     
    stnmas_df = delete_stations(old_stations, stnmas_df)
    #print('stations delete')
    #print(stnmas_df)
    input("Press Enter to continue...")
    stnmas_df = update_stations(changed_stations, stnmas_df)
    print(stnmas_df)
    write_stnmas(stnmas_df, 'test_data/new_stnmas')
