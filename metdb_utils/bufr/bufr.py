"""
The bufr module provides access to MetDB BUFR tables in Python.

BUFR tables are `bufr_tableb` and `bufr_tabled` in standard MetDB format.
Environment variable `BUFR_LIBRARY` gives the path to both files; if not
defined then tables should be in the current working directory.

    >>> import os
    >>> os.environ.update({'BUFR_LIBRARY':'/home/moodsf/MetDB_BUFR24.0.00/tables/'})

Access BUFR tables as follows:
    >>> tabled = bufr.TableD()
    >>> seq = tabled.lookup('300002')
    >>> print(seq)
    ['000002', '000003']
    >>> tableb = bufr.TableB()
    >>> entry = tableb.lookup('001007')
    >>> print(entry.name)
    'SATELLITE IDENTIFIER'

Expand a nested BUFR sequence as follows:
    >>> seq = tableD.expander(['300010'])
    >>> print(seq)
    ['000010','000011','000012','101000','031001','000030'])
    >>> seq = tableD.expander(['001007', '300003'])
    >>> print(seq)
    ['001007', '000010', '000011', '000012'])

"""
import os
import sys
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
    y = desc -(desc //256) * 256
    return "{:01d}{:02d}{:03d}".format(f, x, y)
    

class TableD:
    """ The TableD class represents a complete table D as a dictionary.

        Exceptions:

        * sys.exit(8) : file not found.

        Attributes:

        * tabled (dict) : {'descriptor': ['d1','d2',...,]}
            Key is an F3 descriptor as a string and the value is a sequence of
            descriptors (also as strings).
    """
    # ---------------------------------------------------------------------

    tabled = {}

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
            TableD.tabled = self.__parseTabled(inp)
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
            return cls.tabled[descr]
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
                newseq = TableD.lookup(d)
                final.extend(TableD.expander(newseq))
        return final
    # ---------------------------------------------------------------------

    def __parseTabled(self, inp):
        # Parse MetDB Format Table D
        ndes = 0
        tabled = {}
        for line in inp:

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
                    ndes = int(temp[1])
                    seq = temp[2:]
                descr = f

            else:

                if ndes > 0:
                    if descr in tabled:
                        print(descr, ' already in table D')
                    else:
                        tabled[descr] = seq
                ndes = 0
        print('...', len(tabled), ' D sequences read')
        return tabled

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
            for line in range(0, 1):
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
                    scale = line2[27:29]
                    ref = line2[29:41]
                    width = line2[40:44]
                    entry = TableBEntry(desc, name, unit, scale, ref, width)
                    if desc not in TableB.tableb:
                        TableB.tableb[desc] = entry

        except IOError:
            print('Cannot find file:', tablebFile)
            sys.exit(8)

        finally:
            if inp:
                inp.close()
        print('...', len(TableB.tableb), ' Table B descriptors read.')
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
