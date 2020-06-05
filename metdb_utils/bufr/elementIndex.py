'''
The elementIndex module contains classes/functions for handling an elements_index.

Elements_indexes are used by the MetDB to map decoded BUFR messages to 
retrieval elements in a MetDB request string.  The format is described in
Technote 28.

This module has classes for each section of an elements_index so that one
instance of a TableObj represents one element_index file as shown in the
:download:`class diagram <images/MetDB_Element_Index_structure.pdf>`.

Usage:
    >>> import elementIndex as ec
    >>> table = ec.read_elements('elements_index/aatsr')
    >>> # print the sequences
    >>> print(table.list_sequences())
    >>> # print the element names
    >>> print(table.list_element_names())

'''

class TableObj:
    '''
    Class represents a complete elements_index file.


    Attributes:
      * version (int): 1 for new format element_indexes
      * title (str)  : First line title
      * sequences (dict) : key is index number and
        value is a list of seqObjs 
      * numindex (int) : number of indexes 
      * indexes (dict) : key is index number and
        value is and indexObj
      * elements (list) : list of elementObjs

'''
    def __init__(self, version, title):
        '''Initialise an empty elements_index
        
        args:
          * version (int) - 1 for new index format
          * title (str) - first line information
          
        '''

        self.version = version
        self.title = title
        self.sequences = {}
        self.numindex = 0
        self.indexes = {}
        self.elements = []

    def add_index(self, indexObj):
        '''Add an IndexObj to the elements_index

        args:
          * indexObj : complete index object with unique ID

        errors:
          * duplicate index - index ignored          

        '''
        serial_number = indexObj.serial
        if serial_number in self.indexes:
            print("Error: duplicate index", serial_number)
        else:
            self.indexes[serial_number] = indexObj

    def add_elements(self, elements):
        '''Add an ElementObj to the elements_index'''

        self.elements = elements
    
    def list_element_names(self):
        '''List all element names used by elements_index

        returns:
          * lnames (list) - list of strings
        '''
        lnames = []
        for eObj in self.elements:
            lnames.append(eObj.name)
        return lnames

    def add_sequence(self, seq):
        '''Add a SeqObj to the list for its index'''

        serial_number = seq.index_ref
        if serial_number not in self.sequences:
            self.sequences[serial_number] = []
        self.sequences[serial_number].append(seq)
        self.numindex = max(self.numindex, int(serial_number))

    def list_sequences(self):
        '''List all BUFR sequences used in elements_index.

        returns:
          * lseq (list) - list of sequences, each of which is
            a list of descriptors as strings
        '''
        lseq = []
        for seqObj in list(self.sequences.values()):
            for s in seqObj:
                lseq.append(s.desc)
        return lseq

    def __str__(self):
        '''Return a string which when printed will form an elements_index.'''

        line = '{:1d}   {:s}\n\n'.format(self.version, self.title)
        line += 'BUFR SEQUENCES\n'
        for i, seqlist in self.sequences.items():
            for seq in seqlist:
                line += str(seq)

        for i, s in self.indexes.items():
            line += str(s)

        line += '\n\nELEMENT NAMES AND LOCATIONS '\
                ' (SEGMENT THEN POSITION IN EACH PAIR OF COLS)\n'
        line += '\n{:{width}s}'.format('ELEMENT NAME',
                                       width=ElementObj.namelen)
        line += '   T ID'
        for i in range(self.numindex):
            line += '{:4d}{:4d}'.format(i+1, i+1)
        line += '\n <' + '-' * (ElementObj.namelen - 1) + '>'
        line += ' - --'
        for i in range(self.numindex):
            line += '  --  --'
        for e in self.elements:
            line += str(e)
        line += '\n\n(Blank line indicates end)'
        return line


class SeqObj:
    '''
    Class represents a BUFR sequence from an elements_index

    Attributes:
      * desc (list of strings): descriptor sequence as list of FXY values
      * nseq (int) : length of sequence
      * index_ref (int) : index number associated with the sequence
      * text (string): short description of the sequence or its source
    '''

    def __init__(self, desc, nseq, index_ref, text):
        self.desc = desc
        self.nseq = nseq
        self.index_ref = index_ref
        self.text = text

    def __str__(self):
        '''Return string in the format required for part 1 of an elements index'''

        line = '\n {:4d}{:4d}   '.format(self.index_ref, self.nseq)
        for pos, item in enumerate(self.desc):
            if pos % 8 == 0 and pos > 0:
                line += '\n{:12s}{:7s}'.format(' ', item)
            else:
                line += '{:7s}'.format(item)
        line += self.text.strip()
        return line


class SegmentObj:
    '''
    Class represents one segment of an decoded sequence (as in TN28)

    Attributes:
      * segnum (int): segment number
      * posnum (int): number of items in the segment
      * repcount (int): number of replications this segment is in
      * reps (list of int): replication numbers applied to this segment
      * text (string): short description of segment
    '''

    def __init__(self, segnum, posnum, repcount, reps, text):
        self.segnum = segnum
        self.posnum = posnum
        self.repcount = repcount
        self.reps = reps
        self.text = text

    def __str__(self):
        '''Return string in the format required for part 2 of and elements index'''

        prlev = ''.join(["%4d" % _ for _ in self.reps])
        line = '\n {:4d}{:4d}{:4d}{:}         {:}'.format(
               self.segnum, self.posnum, self.repcount, prlev, self.text)
        return line


class IndexObj:
    '''
    Class representing a complete index map of segments.

    Attributes:
      * serial (int): index number (starting at 1)
      * text (string): short description for the index
      * nseg (int): number of segments (rows in the output)
      * nrep (int): number of replications
      * ndep (int): replication nesting level
      * segments(list of SegmentObj) : segment objects in order
      * replications (list or int): replication values

    '''

    def __init__(self, serial, text, nseg, nrep, ndep, segments, replications):
        self.serial = serial
        self.text = text
        self.nseg = nseg
        self.nrep = nrep
        self.ndep = ndep
        self.segments = segments
        self.replications = replications

    def __str__(self):
        section = '\n\nINDEX{:3d} {:s}'.format(self.serial, self.text)
        section += ('\n\n {:4d}{:4d}{:4d}  segments, replications,'
                    ' nesting level\n').format(self.nseg, self.nrep,
                                               self.ndep)
        for s in self.segments:
            section += str(s)

        if len(self.replications) > 0:
            prrep = ''.join("%4d" % _ for _ in self.replications)
            section += '\n\n ' + prrep

        return section


class ElementObj:
    '''
    Class represents one row of the name section of an elements_index

    Attributes:
      * name (string): element name
      * T (string): data type if required (See TN28)
      * id (string): ID if required (See TN28)
      * location (dict) : dictionary with key=index number and
        value = (segment, position) pairs

    Class Attribute:
      * namelen (int): maximum length of any name so far

    '''

    namelen = 0

    def __init__(self, name, T, id, element_dict):
        self.name = name.strip()
        self.T = T
        self.id = id
        self.location = element_dict
        ElementObj.namelen = max(ElementObj.namelen, len(self.name.strip()))

    def __str__(self):
        width = ElementObj.namelen
        line = '\n {:{width}s}{:>2s}{:>3s} '.format(self.name, self.T,
                                                  self.id, width=width)
        for v in self.location.values():
            line += '{:4d}{:4d}'.format(v[0], v[1])
        return line


def read_elements(filename):
    '''Read an element_index file and return a TableObj.

    args:
      * filename (str) - full path to elements_index file

    returns:
      * table (TableObj) - representation of the elements_index
        or None if error encountered 

    errors:
      * FileNotFoundError - if filename does not exist

    '''

    try:
        f = open(filename)
        lines = f.read().splitlines()
        f.close()
    except FileNotFoundError:
        print("FileNotFoundError:", filename)
        return None

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
    table = TableObj(version, title)

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
        sequence = SeqObj(seq, ndesc, serial_number, text)
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
            for _ in range(repcount):
                rep.append(int(lines[ptr][col:col+4]))
                col += 4
            text = lines[ptr][col:].strip()
            segments.append(SegmentObj(segnum, posnum, repcount, rep, text))
            ptr += 1
    # The last segment is followed by a blank line and then the replication
    # counts for all replications in order in format (T2,17I4).
        if nrep > 0:
            ptr += 1
            col = 1
            replications = []
            for i in range(nrep):
                if i > 0 and i % 17 == 0:
                    ptr += 1
                    col = 1
                replications.append(int(lines[ptr][col:col+4]))
                col += 4
            ptr += 1
        else:
            replications = []

        indexObj = IndexObj(serial_number, index_text, nseg, nrep, ndep, segments, replications)
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
        name = lines[ptr][0:offset+1]
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

        element_list.append(ElementObj(name, T, id, element_dict))
        ptr += 1

    table.add_elements(element_list)

    return(table)
