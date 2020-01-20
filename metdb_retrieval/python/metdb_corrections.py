""" Contains corrections and additions to the metdb module that have not yet
    made it to the prodction version.

    MB-1916: May 2019 Updated after metdb-python version 0.13.1 released
"""
import metdb


def local_updates():
    # Local corrections for errors in the python subtypes module

    metdb.subtypes.DTYPE_MAPS["AIRQAL"]['AQTY_OBSG_STTN_DMNT_EMSN_SRC'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["AIRQALEU"]['AQTY_OBSG_STTN_DMNT_EMSN_SRC'] = 'i4'


    # This is to overcome the problem of nested replications
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_SIG_WX_INSY_ID_1'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_SIG_WX_INSY_ID_2'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_SIG_WX_INSY_ID_3'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_SIG_WX_DSC_ID_1'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_SIG_WX_DSC_ID_2'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_SIG_WX_DSC_ID_3'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_SIG_WX_PHNM_ID_1'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_SIG_WX_PHNM_ID_2'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_SIG_WX_PHNM_ID_3'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_CLD_AMT_ID_1'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_CLD_AMT_ID_2'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_CLD_AMT_ID_3'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_CLD_TYPE_ID_1'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_CLD_TYPE_ID_2'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_CLD_TYPE_ID_3'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_CLD_BASE_HT_1'] = 'f4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_CLD_BASE_HT_2'] = 'f4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['CHG_CLD_BASE_HT_3'] = 'f4'
    metdb.subtypes.DTYPE_MAPS["TAFS"]['TAF_RPT_TXT'] = 'S5000'
