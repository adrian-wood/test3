import sys
import os
import calendar
import time
import datetime as dt
miscPath = os.environ.get('MISC_BASE_DIR', '/var/moods')
sys.path.append(os.path.join(miscPath, 'metdb_utils'))
from web.jinja_render import jinja_render
from web.daily_archive import daily_archive
import mdb_files.DataAccessLog as DA


def get_counts(da_file):
    '''Obtains the retrieval counts by userid and by contact from
    a DataAccessLog object. Returns two lists, one for each.

    Args:
        da_file: A DataAccessLog object.

    Returns:
        byUserIDRows: A list of counts by user.
        byUserIDRows: A list of counts by contact.

    Raises:
        None.
    '''
    byUseridRows = []
    byContactRows = []

    for user in da_file.userid_contact.keys():
        useridRow = {"userid": user,
                     "count": da_file.count_by_userid[user],
                     "contacts": da_file.userid_contact[user]}
        byUseridRows.append(useridRow)

    for contact in da_file.contact_userid.keys():
        contactRow = {"contact": contact,
                      "count": da_file.count_by_contact[contact],
                      "userids": da_file.contact_userid[contact]}
        byContactRows.append(contactRow)

    return(byUseridRows, byContactRows)


def main():
    '''Create web pages summarizing MetDB retrievals by userid and contact.

    For specified servers (mdbapop-prod and mdbapus-prod), reads the current
    data_access.log files and creates a web page giving details of MetDB
    retrievals by userID and contact (fields in the logfile).

    Args:
        None.

    Returns:
        None.

    Raises:
        None.
    '''
    print("-" * 80)
    print('Starting', sys.argv[0], '...')
    print("Using", miscPath, "as base path to MetDB Utilities, templates etc.")

    # Local variables
    # data_access logfiles
    da_dir = os.environ.get('DATA_ACCESS_LOG_DIR',
                            '/var/www/html/mdb_activity/data_access_logs')
    servers = sorted(os.listdir(da_dir))
    servers.remove('mdbdb-prod')
    archive_base = '/var/www/html/mdb_activity/daily_user_contact_archive'
    template_dir = os.path.join(miscPath, 'mdb_activity/templates')
    template_name = 'daily_user_contact_template.html'
    now = dt.datetime.now()
    yesterday = now - dt.timedelta(days=1)
    logDate = yesterday.strftime("%Y/%m/%d")
    datestr = now.strftime("%H:%M %d-%m-%Y")
    da_file_name = now.strftime("data_access.log-%Y%m%d")

    print('Reading data_access.log files in directory', da_dir)
    print("-" * 80)

    print(' Processing files for server mdbapus-prod...')
    da_path = os.path.join(da_dir, 'mdbapus-prod')
    full_path = os.path.join(da_path,
                             now.strftime("%Y"),
                             now.strftime("%m"),
                             da_file_name)
    if os.path.exists(full_path):
        print('  Reading file', full_path)
        with open(full_path, errors='ignore') as f:
            da_file = DA.DataAccessLog(f)
            userByUseridRows, userByContactRows = get_counts(da_file)
            userTotal = da_file.retrieval_count
    else:
        print('  ERROR:', da_file, 'does not exist')

    print(' Processing files for server mdbapop-prod...')
    da_path = os.path.join(da_dir, 'mdbapop-prod')
    full_path = os.path.join(da_path,
                             now.strftime("%Y"),
                             now.strftime("%m"),
                             da_file_name)
    if os.path.exists(full_path):
        print('  Reading file', full_path)
        with open(full_path, errors='ignore') as f:
            da_file = DA.DataAccessLog(f)
            operByUseridRows, operByContactRows = get_counts(da_file)
            operTotal = da_file.retrieval_count
    else:
        print('  ERROR:', da_file, 'does not exist')

    templateVars = {"userByUseridRows": sorted(userByUseridRows,
                                               key=lambda i: i['count'],
                                               reverse=True),
                    "userByContactRows": sorted(userByContactRows,
                                                key=lambda i: i['count'],
                                                reverse=True),
                    "userTotal": userTotal,
                    "operByUseridRows": sorted(operByUseridRows,
                                               key=lambda i: i['count'],
                                               reverse=True),
                    "operByContactRows": sorted(operByContactRows,
                                                key=lambda i: i['count'],
                                                reverse=True),
                    "operTotal": operTotal,
                    "logDate": logDate,
                    "yesterdays_page": yesterday.strftime("/%Y/%m/%d.html"),
                    "creator": sys.argv[0],
                    "runDate": time.strftime("%c"),
                    "runner": os.getenv('USER'),
                    "where": os.uname()[1]}

    # Render the HTML from template, create a page and archive it
    htmlout = jinja_render(template_dir, template_name, **templateVars)

    htmlPage = os.environ.get('HTML_PAGE')
    if htmlPage is None:
        daily_archive('/var/www/html/mdb_activity/daily_user_contact.html',
                      archive_base, htmlout)
    else:
        with open(htmlPage, "w") as outp:
            outp.write(htmlout)

    print('... finished', sys.argv[0])


if __name__ == "__main__":
    main()
