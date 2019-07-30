''' Rough-and-ready utility to print an outline [ELEMENTS] section
    for a SSOT config file.

    Needs an elements_index file as input. Run as follows:
        module load scitools
        cd /var/moods/metdb_utils/bufr/ssot-test
        python lookup.py path-to-aatsr

    Known limitations:
    1) Only works for new elements_indexes, i.e. no wraparound lines
    2) Can't cope with merge sequences
    3) Has very little error handling!
'''

import new_segments
import ElementClass as ec
import parse_elements as parse
import sys

text = '\n  description = \n  reported_units = \n  python_type =\n'

def find_in_table(tab, serial_number, seg, pos):
    for e in tab.elements:
        (s1, p1) = e.location[serial_number]
        if s1 == seg and p1 == pos:
            return e
    return None
def lookup(filename):
    table = parse.parse_elements(filename)
    serial_number = 1
    seqObj = table.sequences[serial_number][0]
    new_table = new_segments.process(seqObj.desc)

    for e in new_table.elements:
        (s1, p1) = e.location[1]
        element = find_in_table(table, serial_number, s1, p1)
        if element:
            desc = e.name.split(' ')[0]
            name = element.name.strip()
            print('  [[{:s}]]  {:s}  descriptor = {:40s}\n'.format(name, text, desc))

if __name__ == "__main__":
    filename = sys.argv[1]
    lookup(filename)
