import numpy as np
from unit_utils import *


# ---------------------------------------------------------------------
# This section has all the individual conversion functions that can be
# called from the elements table.
# ---------------------------------------------------------------------

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


# -----------------------------------------------------------------------
def id_from(station):
    """Return a string representation of station ID.
    """
    value = "{:05d}".format(station)
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
