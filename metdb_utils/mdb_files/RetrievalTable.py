"""
The ``RetrievalTable`` module contains classes and functions for handling a MetDB
``retrieval_table`` file.

A ``retrieval_table`` file consists of:

* 7 **header** lines;
* Several "dataset" lines, each of which contains details of the dataset for either:

  * a MetDB datatype (a datatype may have more than one dataset),
  * one of several specific **"other"** entries, such as ``OFFLINE, STNABRV`` etc;
* Some **footer** lines, containing version history.

The lines in the file are in alphabetical order of datatype.

This module has three classes: 

#. ``Dataset`` representing (effectively) a **single line** of a ``retrieval_table`` file;
#. ``DatatypeRT`` representing **all the lines** for a single MetDB datatype in a ``retrieval_table`` file;
#. ``RetrievalTable`` representing a **complete** ``retrieval_table`` file: the file **headers**, a **dictionary** of all the ``DatatypeRT`` objects, and the file **footers**.

Usage:
    >>> from RetrievalTable import RetrievalTable
    >>> rtobj = RetrievalTable()        # create an empty RetrievalTable object
    >>> rtobj.read_RT(‘metdb_repo/TABLES/retrieval_table’)   # populate the object with the contents of a file
    >>> print(rtobj.list_datatypes())    # print a list of the datatypes in the object
    >>> rtobj.write_RT(‘new_rt_file’)    # write a new retrieval_table file – should be identical to the file read in
"""
from dataclasses import dataclass
from datetime import date, time


@dataclass
class Dataset:
    """
    This class effectively represents a single line of a MetDB ``retrieval_table``.
    """

    model: int  # Model or area of coverage.
    raw_merged: str  # R for Raw, M for Merged.
    start_date: date  # start date of storage period, blank if on-line dataset.
    start_time: time  # start time of storage period, can be blank.
    end_date: date  # end date of storage period, blank if on-line dataset.
    end_time: time  # end time of storage period, can be blank.
    storage_medium: int  # 1=disk, 2=tape
    list_code: int  # Associated data code number.
    elements_list: str  # Number of ELEMENTS library.
    elements_index: str  # name of relevant element_index file.
    retention_period_days: int  # retention period in days.
    index_records: int  # index records per dataset.
    index_minutes: int  # index period in minutes.
    record_length: int  # record length of dataset.
    skeleton_flag: str  # T if dataset name is a MASS skeleton, else F.
    mass_stream: str  # Archive MASS stream.
    dataset_name: str  # Name of dataset.

    def __str__(self):
        """
        Provide a string representation of a ``Dataset``.

        Returns:
            str: A string representation of a Dataset object in the format
            expected by MetDB in a retrieval_table file. Note that the datatype name
            is not an attribute of the Dataset class, so it cannot be returned.
        """
        return " ".join(
            [
                f"{self.model:>2}",
                self.raw_merged,
                f"{self.start_date:%Y/%m/%d}"
                if self.start_date is not None
                else "    /  /  ",
                f"{self.start_time:%H:%M}" if self.start_time is not None else "  :  ",
                f"{self.end_date:%Y/%m/%d}"
                if self.end_date is not None
                else "    /  /  ",
                f"{self.end_time:%H:%M}" if self.end_time is not None else "  :  ",
                f"{self.storage_medium}",
                f"{self.list_code:>2}" if self.list_code is not None else "  ",
                f"{self.elements_list:<8}",
                f"{self.elements_index:<8}",
                f"{self.retention_period_days:>5}"
                if self.retention_period_days is not None
                else "     ",
                f"{self.index_records:>4}"
                if self.index_records is not None
                else "    ",
                f"{self.index_minutes:>5}"
                if self.index_minutes is not None
                else "     ",
                f"{self.record_length:>6}",
                self.skeleton_flag,
                self.mass_stream,
                self.dataset_name,
            ]
        )


class DatatypeRT:
    """
    This class is the collection of ``Dataset`` objects for a single datatype.

    Attributes:
        datasets (list): list of ``Dataset`` objects.
        number_of_datasets (int): the number of datasets for this datatype.
    """

    def __init__(self):
        self.datasets = []
        self.number_of_datasets = 0

    def add_dataset(self, dataset):
        """Add a ``Dataset`` object to the list for this Datatype.

        Args:
            dataset: an Dataset object

        Raises:
            TypeError: if argument supplied is not a ``Dataset`` object.
        """
        if isinstance(dataset, Dataset):
            self.number_of_datasets += 1
            self.datasets.append(dataset)
        else:
            raise TypeError("Argument is not a Dataset object.")

    def __str__(self):
        """
        Provide a string representation of a ``DatatypeRT`` object.

        Returns:
            str: A string representation of a DatatypeRT object in the format
            expected by MetDB in a retrieval_table file.
        """
        newline = "\n"  # \escapes are not allowed inside f-strings
        return f'{newline.join(f"{dataset}" for dataset in self.datasets)}'


class RetrievalTable:
    """
    Represents a complete ``retrieval_table`` file, including the header and footer
    sections, plus the details of each dataset for every datatype retrievable from
    MetDB, and the **"other"** datasets in the file.
    
    Attributes:
        datatypes (dict): key: datatype name, value: Datatype object
        headers (list): list of strings for the headers in a file.
        footers (list): list of strings for the footer in a file.
    """

    def __init__(self, rt_file=None):
        """Initialise a new ``RetrievalTable`` object  and populate it with the
        content of the ``retrieval_table`` file, if supplied.

        Args:
            rt_file: A file name (optional).
        """
        self.datatypes = {}
        self.headers = "\n".join(
            [
                "                                               MET.D.B. RETRIEVAL TABLE",
                "                                               ------------------------",
                " (T2,A8,I3,1X,A1,2(I5,4(1X,I2)),I2,I3,2(1X,A8),I6,I5,I6,I7,L2,1X,A3,1X,A60)              Format for reading table below",
                "",
                "  DATA    MO R    START TIME        END TIME     S LI ELEMENTS ELEMENTS  RTN. INDX INDEX RECORD S MAS",
                "  TYPE    DL M YYYY/MM/DD HH:MM YYYY/MM/DD HH:MM M ST   LIST    INDEX    DAYS RECS  MINS LENGTH F STM DATA SET NAME",
                "-------- -- - ---------------- ---------------- - -- -------- -------- ----- ---- ----- ------ - --- -----------------------------------------------",
            ]
        )
        self.footers = "\n".join(
            [
                "\n",
                " (Blank line above indicates end of table entries)",
                "",
                " (Blank line above indicates end of path variable definitions)",
                "",
                "For details of the format of this table see 'MetDB Technote 4: A Guide To MetDB Data Retrieval Management'.",
                "\n",
            ]
        )
        if rt_file:
            self.read_RT(rt_file)

    def read_RT(self, rt_file):
        """
        Read the lines in the supplied ``rt_file`` (i.e. a MetDB ``retrieval_table`` 
        file), creating the **headers** (first 7 lines), **datasets** (line 8
        to the next blank line) and **footers** (remaining lines) from the file.
        For each "availability set" line in the file:
        
        #. call the ``_unpack_line`` function to obtain a datatype name and ``Dataset`` object for the line.
        #. call the ``add_dataset`` function with this datatype name and ``Dataset`` object.

        Args:
            rt_file (str): a file name.

        Raises:
            ValueError: if no filename supplied.
        """
        if rt_file is None:
            raise ValueError("Filename must be supplied")

        with open(rt_file) as f:
            _rtlines = f.readlines()
            self.headers = _rtlines[:7]
            for _count, _line in enumerate(_rtlines[7:]):
                if _line.strip():
                    try:
                        datatype, dataset = self._unpack_line(_line)
                        if datatype not in self.datatypes:  # new dict entry needed
                            self.datatypes[datatype] = DatatypeRT()
                        self.datatypes[datatype].add_dataset(dataset)
                    except ValueError as e:
                        print("ERROR: Invalid line", _line, "in file:", e)
                else:
                    break  # blank line so we're done with dataset lines
            self.footers = _rtlines[(_count + 7) :]

    def _unpack_line(self, line):
        """
        Unpack a single line from a retrieval_table into components and
        create a Dataset object from them.

        Args:
            line (str): the line of text to unpack

        Returns:
            tuple: tuple containing:
                - datatype (str): the name of the datatype
                - a_set (Dataset): a Dataset object.
        """

        _datatype = line[1:9]
        _model = line[10:12]
        _raw_merged_flag = line[13:14]
        _start_yyyy = line[15:19]
        _start_mm = line[20:22]
        _start_dd = line[23:25]
        _start_hh = line[26:28]
        _start_min = line[29:31]
        _end_yyyy = line[32:36]
        _end_mm = line[37:39]
        _end_dd = line[40:42]
        _end_hh = line[43:45]
        _end_min = line[46:49]
        _storage_medium = line[49:50]
        _list_code_number = line[51:53]
        _elements_list = line[54:62]
        _elements_index = line[63:71]
        _retention_period_days = line[72:77]
        _index_records_per_dataset = line[78:82]
        _index_period_minutes = line[83:88]
        _record_length = line[89:95]
        _skeleton_flag = line[96:97]
        _mass_stream = line[98:101]
        _dataset_name = line[102:]

        if _start_yyyy.isspace():
            _start_date = None
        else:
            _start_date = date(int(_start_yyyy), int(_start_mm), int(_start_dd))

        if _start_hh.isspace():
            _start_time = None
        else:
            _start_time = time(int(_start_hh), int(_start_min))

        if _end_yyyy.isspace():
            _end_date = None
        else:
            _end_date = date(int(_end_yyyy), int(_end_mm), int(_end_dd))

        if _end_hh.isspace():
            _end_time = None
        else:
            _end_time = time(int(_end_hh), int(_end_min))

        if _retention_period_days.isspace():
            _retention_period_days = None
        else:
            _retention_period_days = int(_retention_period_days)

        if _model == " 0" or _datatype.strip() == "STNMAS":
            _index_records_per_dataset = None
            _index_period_minutes = None
            if _datatype.strip() == "STNMAS":
                _list_code_number = int(_list_code_number)
            else:
                _list_code_number = None
        else:
            _list_code_number = int(_list_code_number)
            _index_records_per_dataset = int(_index_records_per_dataset)
            _index_period_minutes = int(_index_period_minutes)

        _a_set = Dataset(
            int(_model),
            _raw_merged_flag,
            _start_date,
            _start_time,
            _end_date,
            _end_time,
            int(_storage_medium),
            _list_code_number,
            _elements_list.strip(),
            _elements_index.strip(),
            _retention_period_days,
            _index_records_per_dataset,
            _index_period_minutes,
            int(_record_length),
            _skeleton_flag,
            _mass_stream,
            _dataset_name.rstrip("\n"),  # keeps trailing spaces
        )

        return _datatype.strip(), _a_set

    def list_datatypes(self):
        """
        List all the datatypes currently contained in this ``RetrievalTable`` object.

        Omit the "pseudo-datatypes" - lines that exist in the retrieval_table but are
        not actual datatypes.

        Returns:
            list: a sorted list of the dataypes currently held.
        """
        datatypes = set(self.datatypes.keys())
        psuedo_datatypes = {
            "ASSOC",
            "ELEMENTS",
            "ELEMIDX",
            "OFFLINE",
            "STNABRV",
            "STNICAO",
            "STNIND",
        }
        return sorted(list(datatypes - psuedo_datatypes))

    def dataset_count(self, datatype):
        """
        Count of the ``Dataset`` objects for a supplied Datatype name.

        Args:
            datatype (str): the name of the Datatype to get the count for.
            
        Returns:
            int: a count of Dataset objects for the supplied datatype.
        """
        return self.datatypes[datatype].number_of_datasets

    def write_RT(self, rt_file):
        """
        Write out this ``RetrievalTable`` object to the supplied ``rt_file`` filename.
        
        The file will be in a format that MetDB can read and will contain the 
        **headers**, all the **datatypes** and the **footers**.
        The lines for each datatype should be obtained from the 
        ``DatatypeRT(__str__)`` function, each line being prefixed with the dataype name.
        OSError exceptions when attempting to create the file will result in sys.exit(2).

        Args:
            rt_file (str): the file to be created.

        Raises:
            ValueError: if no filename supplied.
        """
        if rt_file is None:
            raise ValueError("Filename must be supplied")

        try:
            with open(rt_file, "w") as f:
                f.writelines(self.headers)
                for datatype, datasets in self.datatypes.items():
                    for line in str(datasets).splitlines():
                        f.write(f" {datatype:<9}" + line + "\n")
                f.writelines(self.footers)
        except OSerror as e:
            print("ERROR: Unable to write file", rt_file, ":", e)
            sys.exit(2)

