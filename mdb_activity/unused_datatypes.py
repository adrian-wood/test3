import sys
import os
from pathlib import Path
from datetime import datetime
miscPath = os.environ.get('MISC_BASE_DIR', '/var/moods')
sys.path.append(os.path.join(miscPath, 'metdb_utils'))
import mdb_files.RetrievalTable as RT
import mdb_files.DataAccessLog as DA


def main():
    '''Create a report of un-retrieved datatypes.

    A MetDB retrieval_table file is read to determine the set of current
    datatypes (this file must be specified by the $RETRIEVAL_TABLE environment
    variable).
    Then, all data_access.log files under a base directory (which must be
    specified by the $DATA_ACCESS_LOG_DIR environment variable) are looped
    over, and a DataAccessLog object is created for each. The
    count_by_datatype attribute provides a set of datatypes that have been
    retrieved in this logfile. That set is subtracted from the initial set of
    datatypes from the retrieval_table.
    When all the logfiles have been processed in this way, we are left with the
    set of datatypes that have never been retrieved.

    Args:
        None.

    Returns:
        None.

    Raises:
        ValueError: if $RETRIEVAL_TABLE or $DATA_ACCESS_LOG_DIR is not set.
    '''
    now = datetime.now()
    print("-" * 80)
    print('Starting', sys.argv[0], 'at', now.strftime("%d/%m/%Y %H:%M:%S"))
    print("Using", miscPath, "as base path to MetDB Utilities, templates etc.")
    print("-" * 80)

    try:
        rt_file = os.environ.get('RETRIEVAL_TABLE')
        if rt_file is None:
            raise ValueError('$RETRIEVAL_TABLE must be specified')
        else:
            with open(rt_file) as f:
                rt = RT.RetrievalTable(f)
            rt_datatypes = set(rt.list_datatypes())

        da_dir = os.environ.get('DATA_ACCESS_LOG_DIR')
        if da_dir is None:
            raise ValueError('$DATA_ACCESS_LOG_DIR must be specified')
    except (ValueError, OSError) as e:
        print('ERROR:', e)
        sys.exit(2)

    for log_file in Path(da_dir).rglob('data_access.log*'):
        print('  Looking for', len(rt_datatypes), 'datatypes:',
              sorted(rt_datatypes))
        print('  In file:', log_file)
        with open(log_file, errors='ignore') as f:
            da_file = DA.DataAccessLog(f)
            rt_datatypes -= da_file.count_by_datatype.keys()

    print("-" * 80)
    print('The following', len(rt_datatypes), 'datatypes have not been',
          'retrieved:', sorted(rt_datatypes))
    print("-" * 80)
    now = datetime.now()
    print('Finished at', now.strftime("%d/%m/%Y %H:%M:%S"))


if __name__ == "__main__":
    main()
