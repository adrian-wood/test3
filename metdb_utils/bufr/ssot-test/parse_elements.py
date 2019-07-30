import ElementClass as ec
import sys

def parse_elements(filename):
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

if __name__ == "__main__":
    table = parse_elements('aatsr')
    print(table)
