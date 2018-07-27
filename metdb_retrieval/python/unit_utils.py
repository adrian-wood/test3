""" .. module:: unit_utils.py
    .. moduleauthor:: Sheila Needham
"""
import numpy as np
import os
import sys

MDI = np.ma.masked
MPS2KTS = 3600./1852.
K2C = 273.15
EC_LOCAL = "74"
EC_VERSION = "30"
codes = {}   # dict for all code tables loaded


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
            with open(path + filename) as t:
                for line in t:
                    (k, v) = line.strip().split(None, 1)
                    table[k] = v

            # save it for next time
            codes[codetable] = table
        except:
            print("Code table not found:" + codetable)
            codes[codetable] = None

    if table:
        decode = table.get(codevalue, codevalue)
    else:
        decode = codevalue

    # wrap the string in quotes in case it contains commas
    return "'{}'".format(decode)


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
                EC_LOCAL + "/0/codetables/")
    else:
        path = (wmo_path + "/bufr/tables/0/wmo/" +
                EC_VERSION + "/codetables/")

    return path
