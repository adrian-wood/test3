""" 
`lookup.py` is a script to print an outline [ELEMENTS] section for an
SSOT config file.

See Technote 44 for contents of SSOT config files.  This script
generates the element name sub-sections within an [ELEMENTS]
section based on a MetDB elements_index file as input. 

Known limitations:

1. Only works for new elements_indexes, i.e. no wraparound lines
2. May not cope with merge sequences
3. Doesn't handle associated data and some operators very well
4. Has very little error handling!

.. warning:: Use with caution and remember to fill in the blank fields generated
             by this script.


It uses the first sequence in the first index by default but you
can choose others using command line options:

-i for index number (as given in the elements_index) 

-s for sequence (starting at zero relative to the index).

Script needs access to BUFR tables via the BUFR_LIBRARY environment
variable and other modules in the metdb_utils/bufr package.

Run from the command line as follows:
::

  $ module load scitools
  $ export BUFR_LIBRARY=/home/moodsf/MetDB_BUFR25.0.00/tables/
  $ export PYTHONPATH=/var/moods/metdb_utils/bufr:$PYTHONPATH 
  $ python /var/moods/metdb_utils/bufr/lookup.py aatsr


"""

import elements
import bufr
import elementIndex as ec
import sys
import argparse


ints = ['YEAR', 'MNTH', 'DAY', 'HOUR', 'MINT', 'SCND']

def find_in_table(tab, serial_number, seg, pos):
    '''Finds element name for a given index, segment and position.
    
    args:
    * tab (TableObj)
    * serial_number (str) - index number (from 1)
    * seg (int) - segment number
    * pos (int) - position in segment
    
    returns:
    * e (ElementObj) - matching element or None if not found
    '''

    for e in tab.elements:
        (s1, p1) = e.location[serial_number]
        if s1 == seg and p1 == pos:
            return e
    return None

def lookup(filename, serial_number, offset):
    '''Print [ELEMENTS] sections for the given filename'''

    tableb = bufr.TableB()

    #  Read the file into an ElementObj structure
    table = ec.read_elements(filename)
    if table is None:
        print(f"ERROR: failed to read {filename}")
        return

    #  Create a new elements table from just the given
    # sequence (this will have descriptors in the element
    # names fields)
    seqObj = table.sequences[serial_number][offset]
    new_table = elements.process(seqObj.desc)

    # Match the segment and position from the new table
    # with the original to get the actual element name,
    # so now we've paired up descriptors and element
    # names which is what's required for SSOT
    for e in new_table.elements:
        (s1, p1) = e.location[1]
        element = find_in_table(table, serial_number, s1, p1)
        if element:
            text = '\n  description = '
            desc = e.name.split(' ')[0]
            name = element.name.strip()
            if desc[0] == '0':
                text += '\n  descriptor = ' + desc.strip()
                units = tableb.lookup(desc).unit
                if 'CODE' in units or 'FLAG' in units:
                    if 'CODE' in units:
                        text += '\n  reported_units = code'
                    if 'FLAG' in units:
                        text += '\n  reported_units = flag'
                    text += '\n  table_id = ' + desc.strip()
                else:
                    text += '\n  reported_units = '
                    if any(x in name for x in ints):
                        text += '\n  python_type = I'
                    else:
                        text += '\n  python_type = R'
            else:
                text += '\n  reported_units = numeric'
                text += '\n  python_type = I'
            print('  [[{:s}]]  {:s}\n'. format(name, text))


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Prints an outline [ELEMENTS] section for SSOT')

    parser.add_argument('filename',
                         action='store',
                         help='New format elements_index filename')

    parser.add_argument('-i', action='store', dest='serial', default=1, type=int,
                         help='elements file index reference')

    parser.add_argument('-s', action='store', dest='seq', default=0, type=int,
                         help=('number of BUFR sequence to use (starting at 0)'
                               ' relative to the index reference'))
                        
    cmdline = parser.parse_args()
    lookup(cmdline.filename, cmdline.serial, cmdline.seq)
