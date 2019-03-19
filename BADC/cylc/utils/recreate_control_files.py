"""Recreate the control files for BADC extractions.
   Reads a configuration file control_file.cfg which holds all the details,
   and creates new control files accordingly in a directory in /tmp.
   NB I am sure there are better ways to do this.
   Andy Moorhouse, August 2018
   MB-1942 Update for Python 3                             Andy Moorhouse
"""

import os
from configparser import SafeConfigParser
from datetime import datetime, timedelta

config_file = '/var/moods/BADC/utils/control_file.cfg'
Config = SafeConfigParser()
now = datetime.now()


def create_control_file(datatype, run_time):
    pathtime = run_time.strftime("%Y%m%d_%H%M%S")
    path = "/tmp/BADC_generated_control_files_" + pathtime + "/"
    if not os.path.exists(path):
        os.makedirs(path)

    print("Run time: ", run_time.strftime("%Y%m%d/%H%MZ"))
    period_hours = Config.getint(datatype, 'period_hours')
    run_day = Config.get(datatype, 'run_day')
    lag_hours = Config.getfloat(datatype, 'lag_hours')
    run_offset_hours = Config.getfloat(datatype, 'run_offset_hours')

    if run_day == "all":
        # get the time within the previous run, calculated as
        # (now - lag time - period - offset):
        rtmlmp = (run_time - timedelta(hours=lag_hours) -
                  timedelta(hours=period_hours) -
                  timedelta(hours=run_offset_hours))
        hr = int(rtmlmp.strftime("%H"))  # the hour within the previous run

        # obtain the start time of the period that the hour is in...
        for period in range(24//period_hours):
            period_start_hr = ((period + 1) * period_hours)
            if hr < period_start_hr:
                ret_start = rtmlmp.replace(microsecond=0, second=0, minute=0,
                                           hour=(period * period_hours))
                break

        # calculate retrieval end time, and run time of last run...
        ret_end = (ret_start + timedelta(hours=period_hours - 1) +
                   timedelta(minutes=59))
        ret_prt = (ret_start + timedelta(hours=lag_hours) +
                   timedelta(hours=period_hours) +
                   timedelta(hours=run_offset_hours) + timedelta(seconds=10))

    else:
        offset = (run_time.weekday() - int(run_day)) % 7
        ret_start = (run_time.replace(microsecond=0, second=0, hour=0,
                                      minute=0) -
                     timedelta(days=(offset + 7)))
        ret_end = (ret_start.replace(microsecond=0, second=0, hour=23,
                                     minute=59) + timedelta(days=6))
        ret_prt = (ret_start + timedelta(days=offset + 1) +
                   timedelta(hours=lag_hours) +
                   timedelta(hours=run_offset_hours) +
                   timedelta(seconds=10))

    print("Retrieval Start Time: ", ret_start.strftime("%Y%m%d/%H%MZ"))
    print("Retrieval End   Time: ", ret_end.strftime("%Y%m%d/%H%MZ"))
    print("Previous Run    Time: ", ret_prt.strftime("%Y%m%d/%H%M%SZ"))

    # Create control file
    with open(path + "MDB.BADC." + datatype + ".CONTROL", 'w') as fh:
        if datatype != "CLIMAT":  # CLIMAT doesn't have the usual info in file
            fh.write(ret_start.strftime("%Y%m%d/%H%MZ") + '\n')
            fh.write(ret_end.strftime("%Y%m%d/%H%MZ") + '\n')
            fh.write('{:>4}'.format(period_hours) + '\n')
            fh.write('{:>4}'.format(int(lag_hours)) + '\n')
            fh.write(ret_prt.strftime("%Y%m%d/%H%M%SZ") + '\n')
        else:
            fh.write(ret_prt.strftime("%Y%m%d/%H%MZ") + '\n')
        fh.write('{:>4}'.format(0) + '\n')
        fh.write('{:>8}'.format(0) + '\n')


def main():

    Config.read(config_file)
    datatypes = [x.strip()
                 for x in Config.get('DEFAULT', 'datatypes').split(',')]
    for datatype in datatypes:
        print("---------------------------------------------")
        print("Datatype: " + datatype)
        create_control_file(datatype, now)
# can replace "now" with datetime.strptime("20180714T1045Z","%Y%m%dT%H%MZ"))


if __name__ == "__main__":
    main()
