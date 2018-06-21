""" .. module:: convert
    .. moduleauthor:: Sheila Needham
"""
import numpy as np
from unit_utils import *


# ---------------------------------------------------------------------
# This section has all the individual conversion functions that can be
# called from the elements table.
# ---------------------------------------------------------------------
def convert_ww(code):
    """Returns a present weather code value from WMO table 4677 mapped
       from BUFR table 020003.
    """
    value = ""
    if code is not MDI:
        num = int(code)
        if num in range(0, 100):  # up to but not including 100
            value = str(code)
        elif num in range(100, 200):   # AWS equivalents
            num = num - 100
            value = str(num)
        elif num in range(200, 510):
            value = ""
    return value


# ----------------------------------------------------------------------
def convert_to_mb(pressure):
    """Returns a string representation of pressure in hPa (mb)
       from an input pressure in Pa.
    """
    value = ""
    if pressure is not MDI:
        value = "{:6.2f}".format(pressure * 0.01)
    return value


# ----------------------------------------------------------------------
def convert_to_kts(speed):
    """Converts from m/s to knots.
    """
    value = ""
    if speed is not MDI:
        value = "{:.2f}".format(mps_to_kts(speed))
    return value


# ----------------------------------------------------------------------
def convert_to_cm(depth):
    """Converts snow depth in metres to cm.
    """
    value = ""
    if depth is not MDI:
        if depth > 0:
            depth = depth * 100.0
        value = "{:.1f}".format(depth)
    return value


# -----------------------------------------------------------------------
def id_from(block, station):
    """Returns a string representation of WMO block and station
       number from integer inputs of block and station.
    """
    value = "{:02d}{:03d}".format(block, station)
    return value


# ----------------------------------------------------------------------
def convert_to_celsius(temp):
    """Returns a string representation of temperature in degrees
       celsius from input real number in Kelvin.
    """
    value = ""
    if temp is not MDI:
        value = "{:5.2f}".format(kelvin_to_celsius(temp))
    return value


# ----------------------------------------------------------------------
def get_daily_temp(temp, period):
    """Returns temperature in Celsius if the period is 12 hours.
       Max temp over last 12 hours is reported at 18Z
       Min temp over last 12 hours is reported at 06Z
       Otherwise, missing data.
    """
    value = ""
    if period is not MDI:
        if int(period) == -12:
            if temp is not MDI:
                value = "{:.2f}".format(kelvin_to_celsius(temp))
    return value


# ----------------------------------------------------------------------
def get_daily_rain(rain, period):
    """Returns rainfall amount if the accumulation period is 24 hours.
       Otherwise, missing data.
    """
    value = ""
    if period is not MDI:
        if int(period) == -24:
            if rain is not MDI:
                value = "{:.2f}".format(rain)
    return value


# ----------------------------------------------------------------------
def convert_vis(num):
    """Returns a visibility code.
    """
    value = ""
    if num is not MDI:
        if num >= 40000:
            value = "9"
        elif num >= 20000:
            value = "8"
        elif num >= 10000:
            value = "7"
        elif num >= 4000:
            value = "6"
        elif num >= 2000:
            value = "5"
        elif num >= 1000:
            value = "4"
        elif num >= 400:
            value = "3"
        elif num >= 200:
            value = "2"
        elif num >= 100:
            value = "1"
        else:
            value = "0"
    return value


# ----------------------------------------------------------------------
def date_from(year, month, day, hour, minute):
    """Returns a string representation of the date and time in the
       wow csv format from input date/time in integers.
    """
    value = "{:02d}/{:02d}/{:04d} {:02d}:{:02d}".\
        format(day, month, year, hour, minute)
    return value
