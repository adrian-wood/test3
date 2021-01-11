"""
The ``ArchiveLookup`` module contains classes and functions for handling a MetDB
``archive_lookup`` file.

A ``archive_lookup`` file consists of:

* 7 **header** lines,
* Several **archive_set** lines, each of which contains details for a MetDB datatype that is being archived - NB a single datatype may have more than one archive_set, if archivals are being performed for merged data of this datatype,
  * one of several specific **"other"** datasets, such as ``ELEMIDX, HKEEP`` etc, 
* Some **footer** lines, containing "CARTSUB" information (check if this is still required), and version history.

The **archive_sete** lines in the file are in alphabetical order of datatype (including the 
**"other"** datasets).

This module has three classes: 

#. ``ArchiveSet`` representing (effectively) a **single line** of an ``archive_lookup`` file;
#. ``DatatypeAL`` representing **all the lines** for a single MetDB datatype in an ``archive_lookup`` file;
#. ``ArchiveLookup`` representing a **complete** ``archive_lookup`` file: the file **headers**, a **dictionary** of all the ``DatatypeAL`` objects, and the file **footers**.

Usage:
    >>> from ArchiveLookup import ArchiveLookup
    >>> alobj = ArchiveLookup()        # create an empty ArchiveLookup object
    >>> alobj.read_AL(‘metdb_repo/TABLES/archive_lookup’)   # populate the object with the contents of a file
    >>> print(alobj.list_datatypes())    # print a list of the datatypes in the object
    >>> print(alobj.datatypes[“ASCAT”])  # print the line for the ASCAT datatype
    >>> alobj.write_AL(‘new_al_file’)    # write a new archive_lookup file – should be identical to the file read in
"""
from dataclasses import dataclass, field


@dataclass
class ArchiveSet:
    """
    This class effectively represents a single line of a MetDB ``archive_lookup``.
    """

    days_back: int  # Days back from TODAY for start of period.
    # ...  add rest of fields from archive_lookup
    comments: str  # Comments

    def __str__(self):
        """
        Provide a string representation of an ``ArchiveSet``.

        Returns:
            str: A string representation of an ArchiveSet object in the format
            expected by MetDB in a archive_lookup file. Note that the datatype name
            is not an attribute of the ArchiveSet class, so it cannot be returned.
        """
        pass


class DatatypeAL:
    """
    This class is the collection of ``ArchiveSet`` objects for a single datatype.

    Attributes:
        archivesets (list): list of ``ArchiveSet`` objects.
        number_of_archivesets (int): the number of archivesets for this datatype.
    """

    def __init__(self):
        self.archivesets = []
        self.number_of_archivesets = 0

    def add_archive_set(self, archive_set):
        """Add an ``ArchiveSet`` object to the list for this Datatype.

        Args:
            archive_set: an ArchiveSet object

        Raises:
            TypeError: if argument supplied is not a ``ArchiveSet`` object.
        """
        pass

    def __str__(self):
        """
        Provide a string representation of a ``DatatypeAL`` object.

        Returns:
            str: A string representation of a DatatypeAL object in the format
            expected by MetDB in a retrieval_table file.
        """
        pass


class ArchiveLookup:
    """
    Represents a complete ``archive_lookup`` file, including the header and footer
    sections, plus details of every datatype that is currently being archived in MetDB.

    Attributes:
        datatypes (dict): key: datatype name, value: ArchiveSet object
        headers (list): list of strings for the headers in a file.
        footers (list): list of strings for the footer in a file.
    """

    def __init__(self, al_file=None):
        """Initialise a new ``ArchiveLookup`` object  and populate it with the
        content of the ``archive_lookup`` file, if supplied.

        Args:
            al_file: A file name (optional).
        """
        pass

    def read_AL(self, al_file):
        """
        Read the lines in the supplied ``al_file`` (i.e. a MetDB ``archive_lookup`` 
        file), creating the **headers** (first 7 lines), **datatypes** (line 8 to the
        next blank line) and **footers** (remaining lines) from the file.
        For each "datatype" line in the file:
        
        #. call the ``_unpack_line`` function to obtain a datatype name and ``ArchiveSet`` object for the line.
        #. call the ``add_archive_set`` function with this datatype name and ``ArchiveSet`` object.

        Args:
            al_file (str): a file name.

        Raises:
            ValueError: if no filename supplied.
        """
        pass

    def write_AL(self, al_file):
        """
        Write out this ``ArchiveLookup`` object to the supplied ``al_file`` filename.
        
        The file will be in a format that MetDB can read and will contain the 
        **headers** (first 7 lines), all the **datatypes** (line 8 to the
        next blank line) and the **footers**.

        The line for each individual dataset should be obtained from the 
        StorageArchiveSet(__str__) function.

        Args:
            al_file (str): a file name.

        Raises:
            OSError: if any OS errors writing the file.
        """
        pass

    def list_datatypes(self):
        """
        List all the datatypes currently contained in this ``ArchiveLookup`` object.
        
        Returns:
            list: a list of the dataypes currently held.
        """
        pass

    def _unpack_line(al_line):
        """
        From a string containing an "archive_set" line read from an ``archive_lookup`` file,
        extract the values (datatype, ).
        
        Args:
            al_line (str): an archive_set line.

        Raises:
            ValueError: if unable to unpack the archive_set line.

        Returns:
            tuple: tuple containing:
                - datatype (str): The datatype name.
                - a_set (ArchiveSet): An ArchiveSet object.
        """
        pass

