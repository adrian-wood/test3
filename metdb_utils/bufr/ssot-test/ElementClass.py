class TableObj:
    '''Class describes a complete elements_index file.'''
    def __init__(self, version, title):
        self.version = version
        self.title = title
        self.sequences = {}
        self.numindex = 0
        self.indexes = {}
        self.elements = []


    def add_index(self, indexObj):
        serial_number = indexObj.serial
        if serial_number in self.indexes:
            print("Error: duplicate index", serial_number)
        else:
            self.indexes[serial_number] = indexObj

    def add_elements(self, elements):
        self.elements = elements

    def add_sequence(self, seq):
        serial_number = seq.index_ref
        if serial_number not in self.sequences:
            self.sequences[serial_number] = []
        self.sequences[serial_number].append(seq)
        self.numindex = max(self.numindex, int(serial_number))

    def __str__(self):
        '''Returns a string which when printed will form an elements_index.'''
        line = '{:1d}   {:s}\n\n'.format(self.version, self.title)
        line += 'BUFR SEQUENCES\n'
        for i, seqlist in self.sequences.items():
            for seq in seqlist:
                line += str(seq)

        for i, s in self.indexes.items():
            line += str(s)

        line += '\n\nELEMENT NAMES AND LOCATIONS '\
                 ' (SEGMENT THEN POSITION IN EACH PAIR OF COLS)\n'
        line += '\n{:{width}s}'.format('ELEMENT NAME', width = ElementObj.namelen)
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
    def __init__(self, desc, nseq, index_ref, text):
        self.desc = desc
        self.nseq = nseq
        self.index_ref = index_ref
        self.text = text

    def __str__(self):
        line = '\n{:6d}{:4d}  '.format(self.index_ref, self.nseq)
        for pos, item in enumerate(self.desc):
            if pos % 8 == 0 and pos > 0:
                line += '\n{:12s}{:7s}'.format(' ', item)
            else:
                line += '{:7s}'.format(item)
        line += self.text.strip()
        return line


class SegmentObj:
    '''Describes a single segment.'''
    def __init__(self, segnum, posnum, repcount, reps, text):
        self.segnum = segnum
        self.posnum = posnum
        self.repcount = repcount
        self.reps = reps
        self.text = text

    def __str__(self):
        prlev = ''.join(["%4d" % _ for _ in self.reps])
        line = '\n{:4d}{:4d}{:4d}{:}         {:}'.format(self.segnum, self.posnum, self.repcount,
                                          prlev, self.text)
        return line

class IndexObj:
    '''Describes a complete index map of segments.'''
    def __init__(self, serial, text, nseg, nrep, ndep, segments, replications):
        self.serial = serial
        self.text = text
        self.nseg = nseg
        self.nrep = nrep
        self.ndep = ndep
        self.segments = segments
        self.replications = replications

    def __str__(self):
        section ='\n\nINDEX{:4d} {:s}'.format(self.serial, self.text)
        section += '\n\n{:4d}{:4d}{:4d}  segments, replications, nesting level\n'.\
            format(self.nseg, self.nrep, self.ndep)
        for s in self.segments:
            section += str(s)

        if len(self.replications) > 0:
            prrep = ''.join("%4d" % _ for _ in self.replications)
            section += '\n\n' + prrep

        return section

class ElementObj:
    '''Describes a single element in a particular index.'''
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
