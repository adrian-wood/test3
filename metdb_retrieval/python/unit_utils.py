import numpy as np
import os
import re
import sys

MDI = np.ma.masked
MPS2KTS = 3600./1852.
K2C = 273.15
EC_LOCAL = "74"
EC_VERSION = "30"
codes = {}   # dict for all code tables loaded
wmo_tableb = {}  # table B entries required for flag tables
local_tableb = {}


# ---------------------------------------------------------------------
def kelvin_to_celsius(value):
    """Convert temperature from K to degrees c.
       parameter: float temperature in K or MDI
       returns: float temperature in degrees C or MDI
    """
    result = value
    if value is not MDI:
        result = value - K2C
    return result


# ----------------------------------------------------------------------
def mps_to_kts(value):
    """Convert from m/s to knots.
       parameter: float speed in metres per second or MDI
       returns: float speed in knots or MDI
    """
    result = value
    if value is not MDI:
        result = value * MPS2KTS
    return value


# --------------------------------------------------------------------
def code_lookup(fxy, value):
    """Lookup a BUFR code table value and return the string value.
       parameters: string fxy descriptor
                   integer value to be decoded
       returns: string text from code table or the original value as string
                if not found.
    """

    if value is MDI:
        return ''

    path = set_bufr_path(fxy)

    codetable = str(int(fxy))  # removes leading zeroes
    codevalue = str(value)

    table = None       # the specific table

    if codetable in codes:
        table = codes[codetable]
    else:
        table = {}   # dict for this code table
        filename = codetable + ".table"
        try:
            with open(path + '/codetables/' + filename, 'r') as t:
                for line in t:
                    (k, v) = line.strip().split(None, 1)
                    table[k] = v
            # save it for next time
            codes[codetable] = table
        except:
            print("Code table not found:" + path + '/codetables/' + filename)
            codes[codetable] = None

    if table:
        decode = table.get(codevalue, codevalue)
    else:
        decode = codevalue

    # wrap the string in quotes in case it contains commas
    return "'{}'".format(decode)


# --------------------------------------------------------------------
def flag_lookup(fxy, value):
    """Convert a flag table value into its component values, separating
       them with semi-colons.  Remember that flag values consist of bits
       from left to right where bit 1 is the most significant.
       parameters: string fxy descriptor
                   integer value to be decoded
       returns: string text from flag table or the origin value as a string
                if not found.
    """

    if value is MDI:
        return ''

    bitwidth = get_bitwidth(fxy)

    missing = 2**bitwidth - 1
    decode = ''
    if value == missing:
        return "'{}'".format('--')
    else:
        for b in range(bitwidth):
            if value % 2 == 1:
                decode = decode + code_lookup(fxy, bitwidth - b)
            value = value/2

    # replace consecutive quotes with a semi-colon to delimit flag items
    return decode.replace('\'\'', ';')


# --------------------------------------------------------------------
def get_bitwidth(fxy):
    """Find the bitwidth for the given fxy descriptor from ecCodes
       element.table.  This will either be the WMO international table
       or a local one.  Only read each table once into two dictionaries.
       parameter: string fxy descriptor
       returns: integer bitwidth
    """

    path = set_bufr_path(fxy)

    filename = 'element.table'

    y = int(fxy[-3:])
    x = int(fxy[:-3])

    if (x >= 48 and x <= 63) or y >= 192:  # local table entry
        if len(local_tableb) == 0:
            with open(path + filename) as t:
                for line in t:
                    entry = line.split('|')
                    local_tableb[entry[0]] = entry[7]
        tableb = local_tableb

    else:  # wmo table entry

        if len(wmo_tableb) == 0:
            with open(path + filename) as t:
                for line in t:
                    entry = line.split('|')
                    wmo_tableb[entry[0]] = entry[7]
        tableb = wmo_tableb

    if fxy in tableb:
        return int(tableb[fxy])
    else:
        print " Table B not found ", fxy
        sys.exit(2)


# ------------------------------------------------------------
def set_bufr_path(fxy):
    """Set path to either BUFR local or international tables in the
       eccodes installation.
       parameter: string fxy descriptor
       returns: string full path to directory containing code tables
    """
    EC_PATHS = os.getenv('ECCODES_DEFINITION_PATH', None)
    if EC_PATHS:
        paths = EC_PATHS.split(':')
        if len(paths) == 2:
            wmo_path = paths[0]
            local_path = paths[1]
        else:
            print 'Invalid ECCODES paths - needs wmo and local paths',
            EC_PATHS
            sys.exit(2)
    else:
        print 'ECCODES_DEFINITION_PATH required'
        sys.exit(2)

    # see if it's an international or local entry

    y = int(fxy[-3:])
    x = int(fxy[:-3])

    if (x >= 48 and x <= 63) or y >= 192:
        path = (local_path + "/bufr/tables/0/local/1/" +
                EC_LOCAL + "/0/")
    else:
        path = (wmo_path + "/bufr/tables/0/wmo/" +
                EC_VERSION + "/")

    return path
