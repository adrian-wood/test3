import sys
import os
import calendar
import datetime as dt
miscPath = os.environ.get('MISC_BASE_DIR', '/var/moods')
sys.path.append(os.path.join(miscPath, 'metdb_utils'))
import mdb_files.RetrievalTable as RT
import mdb_files.DataAccessLog as DA
from web.jinja_render import jinja_render
from web.monthly_archive import monthly_archive


def main():
    '''Create a web page summarizing MetDB retrievals by datatype in a month.

    Historic data_access.log files in a base directory are examined for the
    year and month supplied as parameters. The files must be in subdirectories
    of <server>/<YYYY>/<MM>. Counts of retrievals are done for each <server>
    from every log file in the directories (NB no actual notice is taken of
    the timestamps in the log files - any file present in the directory will
    be examined).
    The base directory of the historical logfiles defaults to the location on
    mdb-apps-test that they are retrieved every day to, i.e.
    /var/www/html/mdb_activity/data_access_logs. Files in this directory are
    zipped up, only a month's worth are kept online (therefore this script
    should usually be run on the first of the month). This base  directory can
    be specified by setting the $DATA_ACCESS_LOG_DIR environment variable;
    this can be useful when generating historic reports having unzipped
    log files to another location.
    A MetDB retrieval_table file is read to determine what are "valid"
    datatypes; this must be specified by the $RETRIEVAL_TABLE environment
    variable.

    Args:
        year: the year (2016 onwards).
        month: the month number.

    Returns:
        None.

    Raises:
        ValueError: if $RETRIEVAL_TABLE is not set.
        ValueError: if exactly 2 arguments are not supplied.
        ValueError: if 1st arg (year) is < 2016 (no history prior to then),
                    or later than the current year.
        ValueError: if 2nd arg not a month number, i.e. between 1 and 12.
    '''
    print("-" * 80)
    print('Starting', sys.argv[0], '...')
    print("Using", miscPath, "as base path to MetDB Utilities, templates etc.")

    # Obtain and validate all arguments and external variables.
    # Exit if no good.
    try:
        rt_file = os.environ.get('RETRIEVAL_TABLE')  # retrieval_table
        if rt_file is None:
            raise ValueError('$RETRIEVAL_TABLE must be specified')
        else:
            # Get a set of all datatypes in the retrieval table
            with open(rt_file) as f:
                rt = RT.RetrievalTable(f)
            rt_datatypes = set(rt.list_datatypes())

        # Obtain the year (YYYY) and month (MM) supplied as arguments.
        # Formats match directory structure of stored data_access.log files.
        if len(sys.argv) == 3:
            year = int(sys.argv[1])
            if not 2015 < year < dt.date.today().year:
                raise ValueError('Invalid year (must be 2016 - now): %s'
                                 % year)
            month = int(sys.argv[2])
            if not 1 <= month <= 12:
                raise ValueError('Invalid month number: %s' % month)
            else:
                monthstr = str(month).zfill(2)
        else:
            raise ValueError('Incorrect number of arguments '
                             'supplied: %s' % (len(sys.argv) - 1))

    except (ValueError, OSError) as e:
        print('ERROR:', e)
        sys.exit(2)

    # Local variables
    # data_access logfiles
    da_dir = os.environ.get('DATA_ACCESS_LOG_DIR',
                            '/var/www/html/mdb_activity/data_access_logs')
    servers = sorted(os.listdir(da_dir))
    archive_base = '/var/www/html/mdb_activity/monthly_datatype_retrievals_archive'
    template_dir = os.path.join(miscPath, 'mdb_activity/templates')
    template_name = 'monthly_datatypes_template.html'
    now = dt.datetime.now()
    datestr = now.strftime("%H:%M %d-%m-%Y")
    procDate = dt.datetime(year=year, month=month, day=1)
    lastMonth = procDate - dt.timedelta(days=1)
    lastMonthsPage = lastMonth.strftime("/%Y/%b.html")
    warning = False

    # master dictionary with server as key, and values of a further
    # dictionary of dataype: count
    server_counts = {}

    # dictionary with server as key, and values of a list of "misssing"
    # data_access_log files
    missing_files = {}

    print('Examining data_access.log files in directory', da_dir)
    print('Year:', year)
    print('Month:', monthstr)
    print('Servers:', ', '.join(servers))
    print("-" * 80)

    # Determine how many days are in this month (2nd part of tuple returned
    # bycalendar.monthrange)
    num_days = calendar.monthrange(year, month)[1]

    for server in servers:
        print(' Processing files for server:', server)
        counts_dict = {}
        da_path = os.path.join(da_dir, server, str(year), monthstr)
        for day in range(1, num_days + 1):
            da_file = ''.join(['data_access.log-',
                               str(year),
                               monthstr,
                               str(day).zfill(2)])
            full_path = os.path.join(da_path, da_file)
            if os.path.exists(full_path):
                print('  Reading file', full_path)
                with open(full_path, errors='ignore') as f:
                    da_file = DA.DataAccessLog(f)
                    for datatype, count in da_file.count_by_datatype.items():
                        if datatype == '':
                            datatype = '(blank)'
                        if datatype in counts_dict:
                            counts_dict[datatype] += count
                        else:
                            counts_dict[datatype] = count
            else:
                print('  ERROR:', da_file, 'does not exist')
                warning = True
                if server in missing_files.keys():
                    missing_files[server].append(day)
                else:
                    missing_files[server] = [day]

        server_counts[server] = counts_dict  # add this count dict to master

    # The server_counts dictionaries won't have all datatypes in them, or the
    # same datatypes. So obtain the superset (all_datatypes) of datatypes...
    all_datatypes = set()
    for datatype_counts in server_counts.values():
        all_datatypes.update(datatype_counts.keys())

    # ... and create a dictionary of counts from the superset...
    by_datatype = {}
    overall_totals = [0] * len(servers)
    invalid_count = 0
    for datatype in all_datatypes:
        if datatype in rt_datatypes:
            valid = 'Y'
        elif (datatype == 'SPECI') and 'METARS' in rt_datatypes:
            valid = 'Y'
        elif ((datatype == 'LTAFS' or datatype == 'STAFS') and
              'TAFS' in rt_datatypes):
            valid = 'Y'
        else:
            valid = 'N'
            invalid_count += 1
        counts = []
        for server_count, server in enumerate(servers):
            # get the count for the dataype for this server, default 0
            count = server_counts[server].get(datatype, 0)
            counts.append(count)
            overall_totals[server_count] += count
        total = sum(counts)
        counts.append(total)
        by_datatype[datatype] = [valid, counts]

    # add up the total reads for each server, and append it to the list
    total_reads = sum(overall_totals)
    overall_totals.append(total_reads)

    # Obtain the list of datatypes that have not been retrieved this month
    not_requested = ', '.join(sorted(rt_datatypes.difference(all_datatypes)))

    # Create the keyword:arguments dictionary for the Jinja template
    templateVars = {"year": year,
                    "month": calendar.month_name[month],
                    "servers": servers,
                    "by_datatype": by_datatype,
                    "overall_totals": overall_totals,
                    "not_requested": not_requested,
                    "invalid_count": invalid_count,
                    "warning": warning,
                    "missing_files": missing_files,
                    "last_months_page": lastMonthsPage,
                    "creator": sys.argv[0],
                    "runner": os.getenv('USER'),
                    "where": os.uname()[1],
                    "runtime": datestr}

    # Render the HTML from template, create a page and archive it
    htmlout = jinja_render(template_dir, template_name, **templateVars)
    # with open(html_file, "w") as outp:
    #     outp.write(htmlout)
    monthly_archive("/var/www/html/mdb_activity/monthly_datatype_retrievals.html",
                    archive_base, htmlout)

    print('... finished', sys.argv[0])


if __name__ == "__main__":
    main()
