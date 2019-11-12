"""
The ElementClass module contains classes for components of an elements_index.

Elements_indexes are used by the MetDB to map decoded BUFR messages to 
retrieval elements in a MetDB request string.  The format is described in
Technote 28.

This module has classes for each section of an elements_index so that one
instance of a TableObj represents one element_index file as shown in the
:download:`class diagram <images/MetDB_Element_Index_structure.pdf>`.


"""

class TableObj:
    """
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


"""
    def __init__(self, version, title):
        """Initialise an empty elements_index"""

        self.version = version
        self.title = title
        self.sequences = {}
        self.numindex = 0
        self.indexes = {}
        self.elements = []

    def add_index(self, indexObj):
        """Add an IndexObj to the elements_index

        Args:

        * indexObj : complete index object with unique ID

        """
        serial_number = indexObj.serial
        if serial_number in self.indexes:
            print("Error: duplicate index", serial_number)
        else:
            self.indexes[serial_number] = indexObj

    def add_elements(self, elements):
        """Add an ElementObj to the elements_index"""

        self.elements = elements

    def add_sequence(self, seq):
        """Add a SeqObj to the list for its index"""

        serial_number = seq.index_ref
        if serial_number not in self.sequences:
            self.sequences[serial_number] = []
        self.sequences[serial_number].append(seq)
        self.numindex = max(self.numindex, int(serial_number))

    def __str__(self):
        """Return a string which when printed will form an elements_index."""
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
        line += ' T ID'
        for i in range(self.numindex):
            line += '{:4d}{:4d}'.format(i+1, i+1)
        line += '\n<' + '-' * (ElementObj.namelen - 2) + '>'
        line += ' - --'
        for i in range(self.numindex):
            line += '  --  --'
        for e in self.elements:
            line += str(e)
        line += '\n\n(Blank line indicates end)'
        return line


class SeqObj:
    """
    Class represents a BUFR sequence from an elements_index

    Attributes:

    * desc (list of strings): descriptor sequence as list of FXY values

    * nseq (int) : length of sequence

    * index_ref (int) : index number associated with the sequence

    * text (string): short description of the sequence or its source
    """

    def __init__(self, desc, nseq, index_ref, text):
        self.desc = desc
        self.nseq = nseq
        self.index_ref = index_ref
        self.text = text

    def __str__(self):
        """Print in the format required for part 1 of an elements index"""

        line = '\n{:6d}{:4d}  '.format(self.index_ref, self.nseq)
        for pos, item in enumerate(self.desc):
            if pos % 8 == 0 and pos > 0:
                line += '\n{:12s}{:7s}'.format(' ', item)
            else:
                line += '{:7s}'.format(item)
        line += self.text.strip()
        return line


class SegmentObj:
    """
    Class represents one segment of an decoded sequence (as in TN28)

    Attributes:

    * segnum (int): segment number

    * posnum (int): number of items in the segment

    * repcount (int): number of replications this segment is in

    * reps (list of int): replication numbers applied to this segment

    * text (string): short description of segment
"""
    def __init__(self, segnum, posnum, repcount, reps, text):
        self.segnum = segnum
        self.posnum = posnum
        self.repcount = repcount
        self.reps = reps
        self.text = text

    def __str__(self):
        """Print in the format required for part 2 of and elements index"""

        prlev = ''.join(["%4d" % _ for _ in self.reps])
        line = '\n{:4d}{:4d}{:4d}{:}         {:}'.format(
               self.segnum, self.posnum, self.repcount, prlev, self.text)
        return line


class IndexObj:
    """
    Class representing a complete index map of segments.

    Attributes:

    * serial (int): index number (starting at 1)

    * text (string): short description for the index

    * nseg (int): number of segments (rows in the output)

    * nrep (int): number of replications

    * ndep (int): replication nesting level

    * segments(list of SegmentObj) : segment objects in order

    * replications (list or int): replication values

    """

    def __init__(self, serial, text, nseg, nrep, ndep, segments, replications):
        self.serial = serial
        self.text = text
        self.nseg = nseg
        self.nrep = nrep
        self.ndep = ndep
        self.segments = segments
        self.replications = replications

    def __str__(self):
        section = '\n\nINDEX{:4d} {:s}'.format(self.serial, self.text)
        section += ('\n\n{:4d}{:4d}{:4d}  segments, replications,'
                    ' nesting level\n').format(self.nseg, self.nrep,
                                               self.ndep)
        for s in self.segments:
            section += str(s)

        if len(self.replications) > 0:
            prrep = ''.join("%4d" % _ for _ in self.replications)
            section += '\n\n' + prrep

        return section


class ElementObj:
    """
    Class represents one row of the name section of an elements_index

    Attributes:

    * name (string): element name

    * T (string): data type if required (See TN28)

    * id (string): ID if required (See TN28)

    * location (dict) : dictionary with key=index number and
                        value = (segment, position) pairs

    Class Attribute:

    * namelen (int): maximum length of any name so far

    """

    namelen = 0

    def __init__(self, name, T, id, element_dict):
        self.name = name.strip()
        self.T = T
        self.id = id
        self.location = element_dict
        ElementObj.namelen = max(ElementObj.namelen, len(self.name.strip()))

    def __str__(self):
        width = ElementObj.namelen
        line = '\n{:{width}s}{:>2s}{:>3s}'.format(self.name, self.T,
                                                  self.id, width=width)
        for i, v in self.location.items():
            line += '{:4d}{:4d}'.format(v[0], v[1])
        return line
