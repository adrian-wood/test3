"""
The ``StorageDatasets`` module contains classes and functions for handling a MetDB
``storage_datasets`` file.

A ``storage_datasets`` file consists of:

* 7 **header** lines,
* Several **datatype** lines, each of which contains details for either:

  * a MetDB datatype that is being stored in a **MetDB storage** dataset,
  * one of several specific **"other"** datasets, such as ``ELEMIDX, HKEEP`` etc, 
* Some **footer** lines, containing version history.

The **datatype** lines in the file are in alphabetical order of datatype (including the 
**"other"** datasets).

This module has two classes: 

#. ``StorageDataset`` representing (effectively) a **single line** of a ``storage_datasets`` file;
#. ``StorageDatasets`` representing a **complete** ``storage_datasets`` file: the file **headers**, a **dictionary** of all the ``StorageDataset`` objects, and the file **footers**.

Usage:
    >>> from StorageDatasets import StorageDatasets
    >>> sdobj = StorageDatasets()        # create an empty StorageDatasets object
    >>> sdobj.read_SD(‘metdb_repo/TABLES/storage_datasets’)   # populate the object with the contents of a file
    >>> print(sdobj.list_datatypes())    # print a list of the datatypes in the object
    >>> print(sdobj.datatypes[“ASCAT”])  # print the line for the ASCAT datatype
    >>> sdobj.write_SD(‘new_sd_file’)    # write a new storage_datasets file – should be identical to the file read in
"""
from dataclasses import dataclass, field


@dataclass
class StorageDataset:
    """
    Represents the storage_dataset for either:
    
    #. a MetDB datatype
    #. one of several specific **other** datasets - ``ELEMIDX, HKEEP`` etc.

    Note that it does NOT contain an attribute for the datatype name.
    """

    direct: str  # "T" for MetDB storage datasets (meaning "direct access"), "F" for "other" datasets (meaning "sequential access").
    formatted: str  # "F" for MetDB storage datasets (meaning read in Fortran as "formatted"), "T" for "Other" datasets (meaning read as "unformatted" via C I/O routine).
    record_length: int  # the record length of the dataset.
    dataset_name: str  # the name of the dataset.
    comment: str  # optional comment

    def __str__(self):
        """
        Provide a string representation of a StorageDataset.

        Returns:
            str: A string representation of a StorageDataset object in the format
            expected by MetDB in a storage_datasets file.
        """
        pass


class StorageDatasets:
    """
    Represents a complete ``storage_datasets`` file, including the header and footer
    sections, plus details of every datatype (that is currently being stored in MetDB)
    and the **"other"** datasets in the file.

    Attributes:
        datatypes (dict): key: datatype name, value: StorageDataset object
        headers (list): list of strings for the headers in a file.
        footers (list): list of strings for the footer in a file.
    """

    def __init__(self, sd_file=None):
        """Initialise a new ``StorageDatasets`` object  and populate it with the
        content of the ``storage_datasetes`` file, if supplied.

        Args:
            sd_file: A file name (optional).
        """
        self.datatypes = {}
        self.headers = []
        self.footers = []
        if sd_file:
            self.read_SD(sd_file)

    def read_SD(self, sd_file):
        """
        Read the lines in the supplied ``sd_file`` (i.e. a MetDB ``storage_datasets`` 
        file), creating the **headers** (first 7 lines), **datatypes** (line 8 to the
        next blank line) and **footers** (remaining lines) from the file.
        For each "datatype" line in the file:
        
        #. call the ``_unpack_line`` function to obtain a datatype name and ``StorageDataset`` object for the line.
        #. add a new key:value pair to the ``datatypes`` dictionary of this datatype name and ``StorageDataset`` object.

        Args:
            sd_file (str): a file name.

        Raises:
            ValueError: if no filename supplied.
        """
        pass

    def write_SD(self, sd_file):
        """
        Write out this ``StorageDatasets`` object to the supplied ``sd_file`` filename.
        
        The file will be in a format that MetDB can read and will contain the 
        **headers** (first 7 lines), all the **datatypes** (line 8 to the
        next blank line) and the **footers**.

        The line for each individual dataset should be obtained from the 
        StorageDataset(__str__) function.

        Args:
            sd_file (str): a file name.

        Raises:
            OSError: if any OS errors writing the file.
        """
        pass

    def list_datatypes(self):
        """
        List all the datatypes currently contained in this ``StorageDatasets`` object.
        
        Returns:
            list: a list of the dataypes currently held.
        """
        pass

    def _unpack_line(sd_line):
        """
        From a string containing a "dataset" line read from a ``storage_datasets`` file,
        extract the values (datatype, direct, formatted, record_length, dataset_name and
        comment).
        
        Args:
            sd_line (str): a storage datasets line.

        Raises:
            ValueError: if unable to unpack the dataset line.

        Returns:
            tuple: tuple containing:
                - datatype (str): The datatype name.
                - sd (StorageDataset): A StorageDataset object.
        """
        pass

