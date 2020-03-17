"""
`elements.py` converts a BUFR sequence to a partial elements_index.

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

.. warning:: Once you have edited the element names, the names field must be less than
             36 characters, as delimited by the <--- ---> line. The names output by 
             this program will be longer by default.

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

```UnicodeDecodeError: 'ascii' codec can't decode byte 0xc2 in position
3441: ordinal not in range(128)```

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
import elementIndex as ec


def hasvalue(desc):
    '''Decides if given descriptor will have a corresponding value.

    args:
      * desc (str) - FXXYYY descriptor
    
    returns:
      * boolean - False if it meets criteria below, otherwise True
        to indicate it does have a corresponding value.

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

       args:
         * seq : list of string descriptors in FXXYYY form

       returns:
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


if __name__ == '__main__':
    '''Print an elements_index for a given sequence.
    
    Usage:

    >>> import elements
    >>> seq = ['315009']
    >>> table = process(seq)
    >>> print(table)
    
    '''

    seq = ['315009']
    table = process(seq)
    print(table)
