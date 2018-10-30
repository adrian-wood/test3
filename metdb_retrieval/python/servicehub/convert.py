# ----------------------------------------------------------------------
#
# MODULE        : convert.py
#
# PURPOSE       : Functions to convert MetDB to the format required
#                 by ServiceHub.
#
#
# REVISION INFO :
# MB-1803: Nov 2018 Check for missing data in date functions.
#                   Function to reset replication counts if limit
#                   exceeded,
#                   Function to convert standard tempertures to 2dp as
#                   string.                                         SN
# MB-1824: Oct 2018 Function to remove 44-byte raw report text header SN
# MB-1803: Oct 2018 Functions for land and marine synops            SN
#
# ----------------------------------------------------------------------
# (C) CROWN COPYRIGHT 2018 - MET OFFICE. All Rights Reserved.
#
# Met Office, United Kingdom.
#
# The use, duplication and disclosure of this code is strictly
# prohibited without the permission of The Meteorological Database
# Team at the above address.
# ----------------------------------------------------------------------

import numpy as np
from unit_utils import *


# ---------------------------------------------------------------------
def metar_temp(value):
    """Convert temperature from K to degrees c.
       Specific for METARS which are converted from C to K by MetDB
       using 273K.
       parameter: float temperature in K or MDI
       returns: string temperature in degrees C or MDI
    """
    result = ""
    if value is not MDI:
        result = "{:.0f}".format(value - 273)
    return result


# ---------------------------------------------------------------------
def synop_temp(value):
    """Convert temperature from K to degrees c.
       Specific for SYNOP/SHIP which are converted from C to K by MetDB
       using 273.1K.
       parameter: float temperature in K or MDI
       returns: string temperature in degrees C and tenths, or MDI
    """
    result = ""
    if value is not MDI:
        result = "{:.1f}".format(value - 273.1)
    return result


# ----------------------------------------------------------------------
def convert_to_celsius(temp):
    """Convert termperature from K to degrees C using the standard
       conversion after checking for missing data.
       parameter: float temperature in K or MDI
       returns: string temperature in degrees C and hundredths, or MDI
    """
    value = ""
    if temp is not MDI:
        value = "{:5.2f}".format(kelvin_to_celsius(temp))
    return value


# ---------------------------------------------------------------------
def limit_reps(actual, limit):
    """Check the actual number of replications does not exceed the max
       specified; reset to max if necessary.
       parameter: integer replication count (from the data)
       parameter: string limit as specified in the elements table
       returns: string giving the actual or maximum.
    """
    limit = int(limit)
    if actual is not MDI:
        value = min(actual, limit)
    else:
        value = actual
    return value


# ---------------------------------------------------------------------
def ship_id(id, callsign):
    """Convert a ship/buoy ID from either number (BUOY_IDNY) or
       SHIP (CALLSIGN).  Bug in the python metdb module means that
       missing strings are not handled correctly so this will take
       whatever value it finds in either slot! This only works because
       these elements are mutually exclusive.  it should still work when
       the metdb module is fixed.
       parameters: integer - buoy identity
                   string - ship callsign
       returns: string of one or the other.
    """
    result = ""
    if id is not MDI:
        result = string(numeric)
    elif callsign is not MDI:
        result = callsign
    return result


# -----------------------------------------------------------------------
def id_from(station):
    """Return a string representation of station ID.
    """
    value = "{:05d}".format(station)
    return value


# -----------------------------------------------------------------------
def wmo_id(block, station):
    """Return a string representation of station ID.
    """
    value = "{:02d}{:03d}".format(block, station)
    return value


# ----------------------------------------------------------------------
def get_rain(rain):
    """Return rainfall amount
    """
    value = ""
    if rain is not MDI:
        value = "{:.2f}".format(rain)
    return value


# ----------------------------------------------------------------------
def datetime_from(*args):
    """Return a string representation of the date and time in the
       day/month/year hh:MM:ss format.
       Args: 4, 5 or 6 integers givin date as year, month, day, hour with
             option minute, second values.
       Returns: dd/MM/yyyy hh:00 (for 4 args)
                dd/MM/yyyy hh:mm (for 5 args)
                dd/MM/yyyy hh:mm:ss (for 6 args)
    """
    if MDI not in args:
        if len(args) == 6:
            (year, month, day, hour, minute, second) = args
            value = "{:02d}/{:02d}/{:04d} {:02d}:{:02d}:{:02d}".\
                format(day, month, year, hour, minute, second)
        elif len(args) == 5:
            (year, month, day, hour, minute) = args
            value = "{:02d}/{:02d}/{:04d} {:02d}:{:02d}".\
                format(day, month, year, hour, minute)
        elif len(args) == 4:
            (year, month, day, hour) = args
            value = "{:02d}/{:02d}/{:04d} {:02d}:00".\
                format(day, month, year, hour)

    return value


# ----------------------------------------------------------------------
def report_text(string):
    """Return a string containing raw report text without the 44 byte
       header.
       Returns: string(44:)
    """
    if len(string) > 44:
        return string[44:]
    else:
        return ""
