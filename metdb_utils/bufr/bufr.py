"""
The `bufr.py` module provides access to MetDB BUFR tables in Python.

Specify a location for the tables with BUFR_LIBRARY or use the 
current working directory.

    >>> import os
    >>> os.environ.update(
    ... {'BUFR_LIBRARY':'/home/moodsf/MetDB_BUFR25.0.00/tables/'})

Access BUFR tables as follows:

    >>> tabled = bufr.TableD()
    >>> seq = tabled.lookup('300002')
    >>> print(seq)
    ['000002', '000003']
    >>> tableb = bufr.TableB()
    >>> entry = tableb.lookup('001007')
    >>> print(entry.name)
    'SATELLITE IDENTIFIER'

Help will give you a list of the entry attributes:

    >>> help(entry)

Expand a nested BUFR sequence as follows:

    >>> seq = tabled.expander(['300010'])
    >>> print(seq)
    ['000010','000011','000012','101000','031001','000030'])
    >>> seq = tabled.expander(['001007', '300003'])
    >>> print(seq)
    ['001007', '000010', '000011', '000012'])


"""
import os
import sys
import re
# =========================================================================


def fxy(desc):
    """ Utility function to convert integer to FXXYYY.

        Splits up a 16-bit BUFR descriptor into F (2 bits),
        X (6 bits) & Y (8 bits).

        Args:

        * desc (int) : integer representation of a descriptor

        Returns:

        * fxy (str) : 'FXXYYY' equivalent

    """
    fx = desc // 256
    f = desc // 16384
    x = fx - (fx // 64) * 64
    y = desc - (desc // 256) * 256
    return "{:01d}{:02d}{:03d}".format(f, x, y)


def is_a_descriptor(text):
    """Check if a string contains 6 digits that could be a BUFR descriptor.

    This is a utility function useful when parsing a text file.

    Args:
    
    * text (string) : input string

    Returns:

    * (Boolean) : True if string is FXXYYY format, otherwise False
    """

    return isinstance(text, str) and \
        text.isdigit() and \
        len(text) == 6 and \
        text[0] in {'0', '1', '2', '3'}


class TableD:
    ''' The TableD class represents a complete table D as a dictionary.

        Exceptions:
          * sys.exit(8) : file not found.

        Attributes:
          * tabled (dict) : {'descriptor': {'seq': ['d1','d2',...,],
            'title': text} }
            Key is an F3 descriptor as a string and the value is a sequence of
            descriptors (also as strings).
          * header (str) : first line of bufr_tabled file

    '''
    # ---------------------------------------------------------------------

    tabled = {}
    header = None

    def __init__(self):
        """ Read TableD into dictionary.

            Enviroment variables:

            * BUFR_LIBRARY: path to tables (optional)

            Exceptions:

            * sys.exit(8): file not found
        """

        bufrPath = os.environ.get('BUFR_LIBRARY')
        if bufrPath is None:
            print('WARNING: BUFR_LIBRARY not set - using cwd')
            bufrPath = '.'

        tabledFile = bufrPath + '/' + 'bufr_tabled'

        inp = None
        try:
            inp = open(tabledFile, 'r')
            (TableD.header, TableD.tabled) = self.__parseTabled(inp)
        except IOError:
            print('Cannot find file:', tabledFile)
            sys.exit(8)
        finally:
            if inp:
                inp.close()

    # ---------------------------------------------------------------------

    @classmethod
    def lookup(cls, descr):
        """ Return the sequence defined by the given descriptor.

            Args:

            * descr (str) : 3XXYYY descriptor

            Returns:

            * list (str): list of expansion or None
        """
        if descr in cls.tabled:
            return cls.tabled[descr]['seq']
        else:
            return None
    # ---------------------------------------------------------------------

    @classmethod
    def description(cls, descr):
        """ Return any text associated with the given descriptor.

            Args:

            * descr (str) : 3XXYYY descriptor

            Returns:

            * text (str): Up to 80 bytes of text
        """
        if descr in cls.tabled:
            return cls.tabled[descr]['title']
        else:
            return None
    # ---------------------------------------------------------------------

    @classmethod
    def expander(cls, seq):
        """ Return the expanded sequence represented by seq.

            Args:

            * seq (list of str) : list of FXXYYY descriptors

            Returns:

            * list (str): list with F3 descriptors expanded
        """
        final = []
        for d in seq:
            if d[0] != '3':
                final.extend([d])
            else:
                newseq = cls.lookup(d)
                final.extend(cls.expander(newseq))
        return final
    # ---------------------------------------------------------------------

    @classmethod
    def add(cls, descriptor, sequence, text):
        """Add a new sequence to Table D.

            Args:

            * descriptor (str) : F=3 descriptor
            * seq(list of str) : sequence of descriptors
            * text (str)       : Description of sequence (can be blank)

         """
        if descriptor in cls.tabled:
            print(f"{descriptor} already in Table D")
        else:
            cls.tabled[descriptor] = {'seq': sequence, 'title': text}
            print(f"sequence {descriptor} added")

    @classmethod
    def writeTabled(cls, filename):
        """Write out TableD structure in MetDB format.

           Args:

           * filename (str): name of new file to be created
        """

        outp = None
        newline = "\n"
        try:
            outp = open(filename, "w+")
            outp.write(cls.header)
            outp.write(newline)

            for k, v in sorted(cls.tabled.items()):
                desc = k
                seq = v['seq']
                text = v['title']
                ndes = len(seq)
                if text:
                    outp.write(f"{' ':10s}{text:60s}{newline}")
                outp.write(f"{desc:6s}{ndes:3d} ")
                for i in range(0, ndes, 10):
                    for p in seq[i:i + 10]:
                        outp.write(f"{p:7s}")
                    outp.write(f"  {newline}")
                outp.write(f"  {newline}")

        except IOError as err:
            print(f"Failed to write new TableD {err}")
        finally:
            if outp:
                outp.close()

    def __parseTabled(self, inp):
        # Parse MetDB Format Table D
        ndes = 0
        tabled = {}
        text = ''
        pattern = re.compile(r'[a-zA-Z]')  # chars in titles but not in descr

        for count, line in enumerate(inp):
            if count == 0:
                header = line
                continue

            if re.search(pattern, line):
                if ndes == 0:
                    text = line.strip()
                continue

            f = line[0:6]
            if f[0].isdigit() and ndes > 10:
                temp = line.split()
                seq.extend(temp)
            elif f[0] == '3':
                if line.find("?") > 0:
                    temp = line.split("?")
                    ndes = int(temp[0].split()[1])
                    seq = temp[1].split()
                else:
                    temp = line.split()
                    if len(temp[0]) > 6:
                        a = temp[0]
                        ndes = int(a[6:9])
                        temp[2:] = temp[1:]
                    else:
                        ndes = int(temp[1])
                    seq = temp[2:]
                descr = f

            else:

                if ndes > 0:
                    if descr in tabled:
                        print(descr, ' already in table D')
                    else:
                        tabled[descr] = {'seq': seq, 'title': text}
                        text = ''
                    ndes = 0
        # print('...', len(tabled), ' D sequences read')
        return (header, tabled)

# =========================================================================


class TableBEntry:
    """ This class represents a single Table B entry.

        Attributes:

        * descr (str) : Table B descriptor.
        * name (str) : element name.
        * unit (str) : units abbreviation.
        * scale (str) : scale factor.
        * ref (str) : reference value.
        * width (str) : bit-width.
    """
    def __init__(self, descr, name, unit, scale, ref, width):
        self.descr = descr
        self.name = name
        self.unit = unit
        self.scale = scale
        self.ref = ref
        self.width = width

    def __str__(self):
        return self.descr + ":" + self.name + \
            ' unit =' + self.unit + \
            ' scale=' + self.scale + \
            ' ref  =' + self.ref + \
            ' width=' + self.width


# =========================================================================

class TableB:
    """ The TableB class represents a complete table B as a dictionary.

        Key is an F0 descriptor as a string and the value is a TableBentry
        object.

        Exceptions:

        * sys.exit(8) : file not found.

        Attributes:

        * tableb (dict) : {'descriptor': TableBentry}
    """

    tableb = {}

    def __init__(self):
        """ Read TableB into dictionary.

            Enviroment variables:

            * BUFR_LIBRARY: path to tables (optional)

            Exceptions:

            * sys.exit(8): file not found
        """

        bufrPath = os.environ.get('BUFR_LIBRARY')
        if bufrPath is None:
            print('WARNING: BUFR_LIBRARY not set - using cwd')
            bufrPath = '.'

        tablebFile = bufrPath + '/' + 'bufr_tableb'

        inp = None
        try:
            inp = open(tablebFile, 'r')
            # skip header
            for _ in range(0, 1):
                inp.readline()
            while True:
                line1 = inp.readline()
                if not line1:
                    break
                desc = line1[0:6]
                name = line1[7:73].strip()
                line2 = inp.readline()
                if not line2:
                    break
                unit = line2[1:25]
                scale = line2[26:29]
                ref = line2[29:40]
                width = line2[40:43]
                entry = TableBEntry(desc, name, unit, scale, ref, width)
                if desc not in TableB.tableb:
                    TableB.tableb[desc] = entry

        except IOError:
            print('Cannot find file:', tablebFile)
            sys.exit(8)

        finally:
            if inp:
                inp.close()
    # ---------------------------------------------------------------------

    @classmethod
    def lookup(cls, desc):
        """ Return the Entry for the given descriptor.

            Args:

            * descr (str): 0XXYYY descriptor

            Returns:

            * entry (TableBentry) : Object representing the entry or None
        """
        if desc in cls.tableb:
            return cls.tableb[desc]
        else:
            return None
