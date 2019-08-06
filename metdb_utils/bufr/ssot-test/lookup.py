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
import bufr
import ElementClass as ec
import parse_elements as parse
import sys

tableb = bufr.TableB()
ints = ['YEAR', 'MNTH', 'DAY', 'HOUR', 'MINT', 'SCND']

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
                    text += '\n  python_type = I'
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
    filename = sys.argv[1]
    lookup(filename)
