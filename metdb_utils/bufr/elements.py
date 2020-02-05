"""
`elements.py` contains functions for managing MetDB elements_indexes.

Code is currently installed on `mdb-apps-test:/var/moods/metdb_utils/bufr`
and can be run from there.

It uses the BUFR module which requires `bufr_tableb` and `bufr_tabled` in
standard MetDB format.

Environment variable `BUFR_LIBRARY` gives the path to both files; if not
defined then tables should be in the current working directory.

    >>> import os
    >>> os.environ.update({'BUFR_LIBRARY':'/home/moodsf/MetDB_BUFR25.0.00/tables/'})

The generator is based on
`Technote 28 <https://metnet2/content/metdb-technical-note-28-metdb-element-indexes>`_
and will produce the segment structure,
replication counts, and elements with segment and position.
Once you have the output from this program you should work out which elements
you want to be retrievable and create MetDB element names in the usual way.

**N.B. Once you have edited the element names, the names field must be less than
36 characters, as delimited by the <--- ---> line. The names output by 
this program will be longer by default.**

Limitations:
  * It generates a working elements_index apart from the element names which
    you have to update by hand (See Technote 9).
  * It only works on one sequence at a time; you have to use the output to
    update an existing elements_index manually.
  * It does not recognise local Table D sequences that are not in the MetDB
    bufr_tabled (for example merge sequence)
  * It does not cater for sequences containing replicated increments; there are
    none in the MetDB currently.
  * It does not cater for all possible BUFR operations but does include
    everything in current use.

Usage:

    >>> import elements
    >>> seq=['315009']
    >>> new_index = elements.process(seq)
    >>> print(new_index)
        (example output below)

.. literalinclude:: examples/315009.txt

If you get this error or something similar:

UnicodeDecodeError: 'ascii' codec can't decode byte 0xc2 in position
3441: ordinal not in range(128)

it is probably because your system has the setting LANG=C. Change it
to LANG=en_GB.UTF-8


The easiest way to pass a sequence to the program is to cut-and-paste from an
mhs_decode of sample data, e.g.

    >>> text = ('001087 001085 001086 002036 002148 002149 022055 022056 '
    ...         '022067 301011 301012 301021 008080 033050 109000 031002 '
    ...         '007065 008080 033050 022045 008080 033050 022064 008080 '
    ...         '033050')
    >>> seq = text.strip().split(' ')
    >>> elements.process(seq)


"""
import os
import sys
import bufr
import numpy as np
import ElementClass as ec


def hasvalue(desc):
    '''Decides if given descriptor will have a corresponding value.

       These do no have values in the BUFR data array:
         * Replication operators (F=1)
         * Replication counts
         * Operators (F=2)...except
         * Bitmap placeholders
    '''

    (F, X, Y) = (desc[0], desc[1:3], desc[3:6])
    # all replication operators
    if F == '1':
        return False

    # short and long replication counts (but not bitmaps!)
    if F == '0' and X == '31' and (Y == '000' or
                                   Y == '001' or
                                   Y == '002'):
        return False

    # Specific operators (listed to avoid a catchall situation)
    if F == '2' and (X == '01' or X == '02'):
        return False
    if F == '2' and X == '03':
        return False
    if F == '2' and X == '04':
        return False
    if F == '2' and (X == '05' or X == '06'):
        return False
    if F == '2' and (X == '07'):
        return False
    if F == '2' and (X == '08'):
        return False
    if F == '2' and (X == '22'):
        return False
    if F == '2' and (X == '36' or X == '37'):
        return False
    if F == '2' and (X == '23' or X == '24'
                     or X == '25' or X == '32'):
        if Y == '255':
            return True
        else:
            return False

    return True


def process(input_seq):
    """Expands sequence and divide into segments ready to
       create an elements index.

       Start by assuming the sequence has no replications
       and is already expanded, then look at each descriptor
       in turn and update it accordingly.

       Each descriptor in the final expansion will be
       associated with zero or more replications from
       which we can work out the segment then the position
       in the segment.

       Args:

       * seq : list of string descriptors in FXXYYY form

       Returns:

       * TableObj: representing an elements_index

    """

    # make a local copy since we don't want to change the original list
    seq = input_seq.copy()

    tableD = bufr.TableD()
    tableB = bufr.TableB()

    table = ec.TableObj(1, 'NEW DATATYPE')

    ndes = len(seq)
    rep = [[0] for _ in range(ndes)]

    # These variables used to delimit descriptors that need
    # special handling (F=2 descriptors)
    group_start = []
    group_end = []
    assoc_start = []
    assoc_end = []

    warning = ''  # for error messages at the end

    # counts associated with each replication
    repcount = {}
    maxrep = 0

    # Start by creating a SeqObj representing the sequence
    orig_seq = seq.copy()
    new_sequence = ec.SeqObj(orig_seq, len(seq), 1, 'New sequence')
    table.add_sequence(new_sequence)

    # variable i is the current position in seq; seq is updated
    # dynamically so we can't use a simple loop.
    i = 0

    while i < len(seq):

        desc = seq[i]
        if desc == '':
            print('Error in sequence', seq)
            sys.exit(1)

        level = rep[i].copy()

        (F, X, Y) = (desc[0], desc[1:3], desc[3:6])

        if F == '3':
            # Look up expansion, insert it in the sequence
            # and delete the table D descriptor.
            # Extend the replication array so it stays
            # in sync.
            insert = tableD.lookup(desc)
            if not insert:
                print('Error: Table D not found ', desc)
                sys.exit(1)
            extras = len(insert)
            seq[i:i] = insert
            rep[i:i] = [level] * extras
            del seq[i + extras]
            del rep[i + extras]
            i -= 1  # descriptor has been replaced so need to look at it again

        elif F == '1':
            # Increment the replication count and add it to the list for
            # this descriptor and all those covered by the replication count,
            # For delayed replication the class 31 descriptor
            # is not included in the count so extend the range by 1.
            # The replication counter is either an indicator (for delayed reps)
            # or a value in the descriptor. Store this in the repcount
            # dictionary.
            maxrep += 1
            level.append(maxrep)
            if Y == '000':
                if seq[i + 1] == '031001' or seq[i + 1] == '031002':
                    counter = -1
                else:
                    counter = -2
                for group in range(int(X) + 2):
                    rep[i + group] = level
            else:
                counter = int(Y)
                for group in range(int(X) + 1):
                    rep[i + group] = level
            repcount[maxrep] = counter

        elif F == '2':
            # Most operators do not have values in the data array but the
            # ones in this section require special handling.

            # 203 operators delimit a section that updates Table B so the
            # descriptors between the 203YYY and 203255 do not have values
            # in the output array.
            if X == '03':
                if Y != '255':
                    # start of group redefining reference values
                    group_start.append(i)
                else:
                    group_end.append(i)

            # Take out the descriptors to do with bitmaps
            if X == '22' or X == '36' or X == '37':
                del seq[i]
                del rep[i]
                i -= 1

            # 204 operators indicate associated data will be added to each
            # value until terminated by 204000.  For every data value two
            # extras will be added (when we get to the segment/position
            # section)
            if X == '04':
                if not warning:
                    warning = (' *** WARNING *** Associated data'
                               ' elements not included')
                if Y != '000':
                    assoc_start.append(i)
                else:
                    assoc_end.append(i)

        i += 1  # Next descriptor

    # Now work out a segment number for each descriptor.  A segment is a
    # block of replicated or unreplicated data, which is found by comparing
    # the rep list for each descriptor against the previous one; if it is
    # the same then they are both in the same segment; if they are different
    # then it starts a new segment.
    segment = []
    segcount = 1
    segment.append(segcount)
    for i in range(1, len(rep)):
        if rep[i] != rep[i-1]:
            segcount += 1
        segment.append(segcount)

    # Create a list of indexes that do not have corresponding output
    # values (other than operators which are handled by a function).

    if len(group_start) != len(group_end):
        print('Wrong number of start/end positions (group):',
              group_start, group_end)
        sys.exit(2)

    skip = []  # list of indices to be skipped in the postion count
    for s, e in zip(group_start, group_end):
        for i in range(s, e + 1):
            skip.append(i)

    # Create a list of indexes that have extra displacements added for
    # associated data

    if len(assoc_start) != len(assoc_end):
        print('Wrong number of start/end positions (assoc):',
              assoc_start, assoc_end)
        sys.exit(2)

    extra = []
    for s, e in zip(assoc_start, assoc_end):
        for i in range(s, e + 1):
            extra.append(i)

    # Finally, work out the offset for each descriptor within a segment.
    # Some descriptors don't count because they don't have values in the
    # final decode, this includes replication operators, replication counts
    # and F=2 operators (the hasvalue function determines this).
    # A position of 0 indicates no value in the segment.

    position = []
    poscount = 0
    if hasvalue(seq[0]):
        poscount += 1
    position.append(poscount)

    for i in range(1, len(segment)):
        if segment[i] != segment[i-1]:
            poscount = 0    # back to the start for a new segment
        if i in skip:
            position.append(0)
        elif hasvalue(seq[i]):
            if i in extra:
                if seq[i][0:3] == '031':  # class 31 not included
                    position.append(0)
                else:
                    poscount += 3  # sig., associated data & actual
                    position.append(poscount)
            else:
                poscount += 1
                position.append(poscount)
        else:
            position.append(0)

    # Create an index object
    nsegs = max(segment)
    nlevels = max([len(_) for _ in rep]) - 1
    nreps = max([max(_) for _ in rep])

    s = np.array(segment)
    p = np.array(position)
    r = np.array(rep)
    segList = []

    for i in range(1, max(s)+1):
        npos = max(np.where(s == i, p, 0))
        ind = np.where(s == i)[0]

        thisRep = r[ind]
        nr = max([len(_) for _ in thisRep]) - 1
        levels = thisRep[0][1:]
        prlev = ''.join(["%4d" % _ for _ in levels])
        segList.append(ec.SegmentObj(i, npos, nr, levels, ''))

    # convert repcount dict to list
    replist = [ v for k, v in sorted(repcount.items())] 
    indexObj = ec.IndexObj(1, ' ', nsegs, nreps, nlevels, segList, replist)
    table.add_index(indexObj)

    # The best we can do for element names is Table B descriptors.
    elem_list = []

    for i in range(len(seq)):
        desc = seq[i]
        (F, X, Y) = (desc[0], desc[1:3], desc[3:6])
        if F == '0':
            name = tableB.lookup(desc).name
        elif F == '1':
            name = 'REPLICATION'
            segment[i] = 0
            position[i] = max(rep[i])
        elif F == '2' and Y == '255':
            name = 'QUALITY PLACEHOLDER'
        else:
            name = ''
        if position[i] != 0:
            fullname = desc + ' ' + name
            edict = {1: (segment[i], position[i])}
            elem_list.append(ec.ElementObj(fullname,' ', ' ',edict))

    table.add_elements(elem_list)

    # We can't handle the extra elements generated by 204 operations so just
    # print out a warning instead. The resulting output will be correct but
    # will not include the associated data.
    if warning:
        print(warning)

    return table

def read_elements(filename):
    try:
        f = open(filename)
        lines = f.read().splitlines()
        f.close()
    except FileNotFoundError:
        print("File not found:", filename)
        sys.exit(1)

    # The first few lines in the element index are used for titles and other comments.
    # These can be spread over as many lines as needed.  The 2000 character width version
    # is indicated by the character '1' in column one of the title line.  A space in
    # this position indicates version 0 and is the default version.

    # After the title there is a line with the heading "BUFR SEQUENCES"
    # starting in column 1 followed by a blank line.

    ptr = 0  # current line
    try:
        version = int(lines[ptr][0:1])
    except ValueError:
        version = 0
    title = lines[ptr][1:].strip()
    table = ec.TableObj(version, title)

    while lines[ptr].find('BUFR SEQUENCES') == -1:
        ptr += 1

    ptr += 2

    # Each sequence is preceded by two integers, a serial number for the index and
    # the number of descriptors in the sequence listed. The format is (T2,2I4,2X,8I7)
    # for the first line of each sequence and descriptors are listed eight to a line
    # for as many lines as necessary. Any spare space on the last line can be used for
    # comment.

    while lines[ptr].strip() != '':
        serial_number = int(lines[ptr][0:5])
        ndesc = int(lines[ptr][5:9])

    # work out how many lines we need to get the whole sequence
        nlines = (ndesc - 1) // 8 + 1
        seq = []
        for i in range(nlines):
            text = lines[ptr][10:].strip()
            items = text.split(' ')
            seq.extend(items)
            ptr += 1

    # remove any extra text following the sequence
        text = ' '.join(_ for _ in seq[ndesc:])
        seq = seq[0:ndesc]

    # store the sequence in a dictionary keyed on the serial number
        sequence = ec.SeqObj(seq, ndesc, serial_number, text)
        table.add_sequence(sequence)

    # move on to the index section
    ptr += 1

    # After the BUFR sequences and a blank line the indexes are listed giving
    # details of the structure of the segments of the BUFR messages. These are
    # listed consecutively with a blank line between each. The first line has
    # the word "INDEX" followed by a serial number (1, 2, 3, etc.) in the format
    # (A5,I3) after which the rest of the line can be used for comments as
    # can the following line.

    while lines[ptr].strip() != '':
        if lines[ptr].find('ELEMENT NAMES AND LOCATIONS') > -1:
            break   # to get the element names
        serial_number = int(lines[ptr][5:8])
        index_text = lines[ptr][9:]
    # skip a line
        ptr += 2

    # The next line contains three numbers in the format (T2,3I4),
    # the numbers being the total number of segments, the number of
    # replications and the maximum depth of nesting of replications

        nseg = int(lines[ptr][0:5])
        nrep = int(lines[ptr][5:9])
        ndep = int(lines[ptr][9:13])

    # After a blank line there follows details of the structure of each segment
    # listed one to a line with the data listed in format (I6,16I4).
    # The first number is the segment number, the second is the number of data
    # values in that segment and the third is the number of replications that segment
    # is in. If this last number is greater than zero, it is followed by the replication
    # numbers concerned starting with the outermost one.
        ptr += 2
        segments = []
        for i in range(nseg):
            segnum = int(lines[ptr][0:5])
            posnum = int(lines[ptr][5:9])
            repcount = int(lines[ptr][9:13])
            col = 13
            rep = []
            for j in range(repcount):
                rep.append(int(lines[ptr][col:col+4]))
                col += 4
            text = lines[ptr][col:].strip()
            segments.append(ec.SegmentObj(segnum, posnum, repcount, rep, text))
            ptr += 1
    # The last segment is followed by a blank line and then the replication
    # counts for all replications in order in format (T2,17I4).
        if nrep > 0:
            ptr += 1
            col = 1
            replications = []
            for j in range(nrep):
                replications.append(int(lines[ptr][col:col+4]))
                col += 4
            ptr += 1
        else:
            replications = []

        indexObj = ec.IndexObj(serial_number, index_text, nseg, nrep, ndep, segments, replications)
        table.add_index(indexObj)

    # skip blank line at end of this index
        ptr += 1

    numindexes = table.numindex

    # This section starts with the heading "ELEMENT NAMES AND LOCATIONS" preceded
    # and followed by blank lines after which there are two lines of column headings.
    # The first item is "ELEMENT NAME" and the amount of space assigned for this is indicated
    # by a line of dashes in angle brackets: this starts in column 2 and can be any suitable
    # size as long as it is long enough for the longest element name.

    element_list = []
    ptr += 3
    namelen = lines[ptr].find('>')
    ptr += 1
    offset = namelen
    while lines[ptr] != '':
        element_dict = {}
        name = lines[ptr][0:offset]
        col = offset + 2
        T = lines[ptr][col:col+1]
        col += 2
        id = lines[ptr][col:col+2]
        col += 2
        for i in range(numindexes):
            try:
                seg = int(lines[ptr][col:col+4])
                pos = int(lines[ptr][col+4:col+8])
            except ValueError:
                seg = 0
                pos = 0
            col += 8
            serial_number = i + 1
            if serial_number in element_dict:
                print('Error: duplicate INDEX ID', serial_number)
            else:
                element_dict[serial_number] = (seg, pos)

        element_list.append(ec.ElementObj(name, T, id, element_dict))
        ptr += 1

    table.add_elements(element_list)

    return(table)

if __name__ == '__main__':

    seq = ['315009']
    table = process(seq)
    print(table)
