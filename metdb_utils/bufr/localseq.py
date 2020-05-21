'''
`localseq.py` parses a MetDB local sequence file to extract definitions.

Code is currently installed on `mdb-apps-test:/var/moods/metdb_utils/bufr`
and can be run from there.

Local sequences are held in a different format to Table D sequences, e.g.
/usr/local/moods/tables/bufr_localseq.  The format is described in
MetDB Technote 1. 

This module includes a function to read a single file and return a 
dictionary containing one or more sequences from it.

Usage:

    >>> import localseq
    >>> seq = localseq.read_localseq('bufr_localseq/aatsr')


'''

import bufr
import sys
import re


def read_localseq(filename):
    '''Read a MetDB format local_seq file to get one or more sequences.

    args:
      * filename (str) - name of file containing sequences.

    returns:
      * local_seqs (Dictionary)  - key is local sequence number as str:
        value is list of strings comprising the sequence

        e.g. {'312231': ['001007', '002019', '001096', ...],
        '312045': ['001007', '002019', '001096', ...] }

    Example input file - bufr_localseq/aatsr

    Format of the input file is summarised below:

      A line starting with the Table D descriptor, maybe followed by a blank
      line, then any number of lines defining the sequence, each starting with
      one or more descriptors delimited by commas or spaces. Text can follow.
      A blank line ends the sequence.  There may be more than one sequence in
      a file.
    '''

    local_seqs = {}

    # read the file
    try:
        inp = open(filename)
        lines = inp.readlines()
        inp.close()
    except IOError as e:
        print(f'Error reading {filename} - {e}')
        sys.exit(1)

    found = False  # set true when we find the start of a sequence

    # regex to find groups of digits delimited by commas or spaces
    pattern = re.compile(r'(\d+)[, \n]')

    # loop over lines
    for line in lines:

        # if the line starts with a descriptor and we haven't started a
        # sequence yet, then remember the descriptor and initialise an empty
        # list to build the sequence. Set an indicator to show that we've
        # found the start of a new sequence.
        if bufr.is_a_descriptor(line[0:6]) and not found:
            descr = line[0:6]
            # print(f'Starting with {descr}')
            seq = []
            found = True

        # else if the line is blank and we have built up a sequence, then this
        # is the end of a sequence so save it in the dictionary and reset the
        # indicator to show we are not working on a sequence now
        elif line.strip() == '' and len(seq) > 0:
            # print(f'End of sequence with length {len(seq)}')
            local_seqs[descr] = seq
            found = False

        # else the line will contain a mix of descriptors and text. Extract the
        # descriptors using the regular expression and append them to the
        # sequence.
        else:
            # e.g. 001007, 002019  SATELLITE IDENTIFIER, SATELLITE INSTRUMENTS
            # descriptor string delimited by a space before text
            
            delimit = re.search(r'[a-zA-Z]', line)
            if delimit:
                delimit = delimit.start()
            else:
                delimit = len(line)
            items = re.findall(pattern, line[:delimit])
            for i in items:
                if bufr.is_a_descriptor(i):
                    seq.append(i)
                else:
                    break

    # The file might not have a blank line at the end so make sure any sequence
    # we have been building is added to the dictionary.
    if found and len(seq) > 0:
        # print(f'End of dataset last sequence with length {len(seq)}')
        local_seqs[descr] = seq

    return local_seqs
