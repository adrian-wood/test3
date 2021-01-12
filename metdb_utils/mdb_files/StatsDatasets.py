"""
The ``StatsDatasets`` module contains classes and functions for handling a MetDB
``stats_datasets`` file.

A ``stats_datasets`` file consists of:

* 7 **header** lines;
* Several **stats_dataset** lines, each of which contains details for a MetDB dataset that is having statistics collected for - NB a single datatype may have more than one stats_dataset, if statistics are being collected for merged data of this datatype,
* Some **footer** lines, containing version history.

The lines in the file are in alphabetical order of datatype.

This module has three classes: 

#. ``StatsSet`` representing (effectively) a **single line** of a ``stats_datasets`` file;
#. ``DatatypeSD`` representing **all the lines** for a single MetDB datatype in a ``stats_datasets`` file;
#. ``StatsDatasets`` representing a **complete** ``stats_datasets`` file: the file **headers**, a **dictionary** of all the ``DatatypeSD`` objects, and the file **footers**.

Usage:
    >>> from StatsDatasets import StatsDatasets
    >>> sdobj = StatsDatasets()        # create an empty StatsDatasets object
    >>> sdobj.read_SD(‘metdb_repo/TABLES/stats_datasets’)   # populate the object with the contents of a file
    >>> print(sdobj.list_datatypes())    # print a list of the datatypes in the object
    >>> sdobj.write_SD(‘new_sd_file’)    # write a new stats_datasets file – should be identical to the file read in
"""
from dataclasses import dataclass


@dataclass
class StatsSet:
    """
    This class effectively represents a single line of a MetDB ``stats_datasets``.
    """

    model: int  # Model or area of coverage.
    # add rest of fields
    dataset_name: str  # Name of dataset.

    def __str__(self):
        """
        Provide a string representation of a ``StatsSet``.

        Returns:
            str: A string representation of a StatsSet object in the format
            expected by MetDB in a stats_datasets file. Note that the datatype name
            is not an attribute of the StatsSet class, so it cannot be returned.
        """

    pass


class DatatypeSD:
    """
    This class is the collection of ``StatsSet`` objects for a single datatype.

    Attributes:
        datasets (list): list of ``StatsSet`` objects.
        number_of_stats_sets (int): the number of datasets for this datatype.
    """

    def __init__(self):
        self.datasets = []
        self.number_of_stats_sets = 0

    def add_dataset(self, dataset):
        """Add a ``Dataset`` object to the list for this Datatype.

        Args:
            dataset: an Dataset object

        Raises:
            TypeError: if argument supplied is not a ``Dataset`` object.
        """

        pass

    def __str__(self):
        """
        Provide a string representation of a ``DatatypeSD`` object.

        Returns:
            str: A string representation of a DatatypeSD object in the format
            expected by MetDB in a stats_datasets file.
        """

        pass


class StatsDatasets:
    """
    Represents a complete ``stats_datasets`` file, including the header and footer
    sections, plus the details of each dataset for every datatype retrievable from
    MetDB, and the **"other"** datasets in the file.
    
    Attributes:
        datatypes (dict): key: datatype name, value: Datatype object
        headers (list): list of strings for the headers in a file.
        footers (list): list of strings for the footer in a file.
    """

    def __init__(self, sd_file=None):
        """Initialise a new ``StatsDatasets`` object  and populate it with the
        content of the ``stats_datasets`` file, if supplied.

        Args:
            sd_file: A file name (optional).
        """
        pass

    def read_SD(self, sd_file):
        """
        Read the lines in the supplied ``sd_file`` (i.e. a MetDB ``stats_datasets`` 
        file), creating the **headers** (first 7 lines), **datasets** (line 8
        to the next blank line) and **footers** (remaining lines) from the file.
        For each "availability set" line in the file:
        
        #. call the ``_unpack_line`` function to obtain a datatype name and ``Dataset`` object for the line.
        #. call the ``add_dataset`` function with this datatype name and ``Dataset`` object.

        Args:
            sd_file (str): a file name.

        Raises:
            ValueError: if no filename supplied.
        """

        pass

    def _unpack_line(self, line):
        """
        Unpack a single line from a stats_datasets into components and
        create a Dataset object from them.

        Args:
            line (str): the line of text to unpack

        Returns:
            tuple: tuple containing:
                - datatype (str): the name of the datatype
                - a_set (Dataset): a Dataset object.
        """
        pass

    def list_datatypes(self):
        """
        List all the datatypes currently contained in this ``StatsDatasets`` object.

        Returns:
            list: a sorted list of the dataypes currently held.
        """
        pass

    def dataset_count(self, datatype):
        """
        Count of the ``Dataset`` objects for a supplied Datatype name.

        Args:
            datatype (str): the name of the Datatype to get the count for.
            
        Returns:
            int: a count of Dataset objects for the supplied datatype.
        """
        pass

    def write_SD(self, sd_file):
        """
        Write out this ``StatsDatasets`` object to the supplied ``sd_file`` filename.
        
        The file will be in a format that MetDB can read and will contain the 
        **headers**, all the **datatypes** and the **footers**.
        The lines for each datatype should be obtained from the 
        ``DatatypeSD(__str__)`` function, each line being prefixed with the dataype name.
        OSError exceptions when attempting to create the file will result in sys.exit(2).

        Args:
            sd_file (str): the file to be created.

        Raises:
            ValueError: if no filename supplied.
        """
        pass

