""" .. module:: unit_utils.py
    .. moduleauthor:: Sheila Needham
"""
import numpy as np
MDI = np.ma.masked
MPS2KTS = 3600./1852.    
K2C = 273.15

# ---------------------------------------------------------------------
def kelvin_to_celsius(value):
    """Converts temperature from K to degrees c.
       parameter: float temperature in K or MDI
       returns: float temperature in degrees C or MDI
    """
    result = value
    if value is not MDI:
        result = value - K2C
    return result


# ----------------------------------------------------------------------
def mps_to_kts(value):
    """Converts from m/s to knots.
       parameter: float speed in metres per second or MDI
       returns: float speed in knots or MDI
    """
    result = value
    if value is not MDI:
        result = value * MPS2KTS
    return value
