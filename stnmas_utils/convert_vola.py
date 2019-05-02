""" PROGRAM       : convert_vola.py
    PURPOSE       : Convert a legacy format WMO Vol A file into STNMAS
                    format.

    USAGE         : convert_vola.py vola_legacy_report.txt

    AUTHOR        : Sheila Needham

    REVISION INFO :
    MB-1952: Apr 2019 update to Python 3
    MB-1683: June 2018 Original

"""
import sys
from pandas import read_csv
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


def main():
    '''Read VolA legacy file generated from Oscar by WMO and convert
       to stnmas abbreviated format.'''

    # Get command line argument - input file
    if len(sys.argv) != 2:
        print('ERROR:', sys.argv[0], '- filename argument required')
        sys.exit(8)

    VolA = sys.argv[1]

    # Read legacy VolA into a Dataframe
    try:
        pandas_data = read_csv(VolA, delimiter='\t', encoding='utf-8')
    except:
        print('Unable to read ', VolA)
        sys.exit(8)

    # select columns needed for abbreviated list
    subset = pandas_data.loc[:, ['IndexNbr', 'Latitude', 'Longitude',
                                 'StationName', 'Hp', 'Hha',
                                 'ObsRems', 'IndexSubNbr']]
    print(subset.head())

    # and sort into WMO station order
    sorted_subset = subset.sort_values(by=['IndexNbr', 'IndexSubNbr'])

    # Open output file and write header lines
    abrv_file = VolA + '.stnmas'
    outp = open(abrv_file, 'w', encoding='utf-8')

    line1 = '  WMO      LAT      LONG   SENSOR   STN   STN            STATION\n'
    line2 = ' INDEX   DEG/THS  DEG/THS  HT (M)  HT (M) TYPE           NAME\n'
    outp.write(line1)
    outp.write(line2)

    count = 0

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

        # format the output
        line_n = "{:>6}{: 9.3f}{: 9.3f}{:>9}{:>8} {:12}   {}\n".\
            format(wmo_id, lat, lon, sensor_ht, station_ht,
                   stn_type, name.strip())
        outp.write(line_n)
        count += 1

    outp.close()
    print(str(count), ' records written to ', abrv_file)

if __name__ == "__main__":
    main()
