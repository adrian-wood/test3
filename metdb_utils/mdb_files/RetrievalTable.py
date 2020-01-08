'''
The RetrievalTable module contains classes relating to the MetDB
retrieval_table file. This file holds information for each MetDB "datatype"
such as the location of the datafile, indexing period, MASS storage stream
and so on.
There can be several entries for a  single MetDB datatype, each entry
represents a "dataset" - online or archived, for example.
'''


class RetrievalTable:
    '''This class represents a collection of retrievals from MetDB, typically
    but not necessarily all the retrievals recorded in a data_access.log file.

    Attributes:
        * retrievals (list): a list of MetDBRetrieval objects.
        * count_by_datatype (dict): the number of times that a retrieval for a
                                    specific datatype occurs in the collection
        * count_by_user (dict): the number of times that a retrieval for a
                                    specific user occurs in the collection.
    '''
    def __init__(self, rt_file=None):
        '''Initialise a new RetrievalTable object  and populate it with the
        content of the supplied retrieval_table file, if any.

        Args:
            rt_file: A file object (optional).
        '''
        self.datatypes = {}
        if rt_file is None:
            self.headers = []
            self.footers = []
        else:
            _rtlines = rt_file.readlines()
            self.headers = _rtlines[:7]
            for _count, _line in enumerate(_rtlines[7:]):
                if _line.strip():
                    datatype, dataset = self._unpack_line(_line)
                    if datatype not in self.datatypes:  # new dict entry needed
                        self.datatypes[datatype] = Datatype()
                    self.datatypes[datatype].add_dataset(dataset)
                else:
                    break  # blank line so we're done with dataset lines
            self.footers = _rtlines[(_count + 7):]

    def _unpack_line(self, line):
        '''Unpack a single line from a retrieval_table into components and
        create a Dataset object from them.

        Args:
            line: the line of text to unpack

        Returns:
            datatype (str): the name of the datatype
            dataset (Dataset): a Dataset object
        '''
        _datatype = line[1:9].strip()
        _model = line[10:12].strip()
        _raw_merged_flag = line[13:14].strip()
        _start_time = line[15:31].strip()
        _end_time = line[32:48].strip()
        _storage_medium = line[49:50].strip()
        _list_code_number = line[51:53].strip()
        _elements_list = line[54:62].strip()
        _elements_index = line[63:71].strip()
        _retention_period_days = line[72:77].strip()
        _index_records_per_dataset = line[78:82].strip()
        _index_period_minutes = line[83:88].strip()
        _record_length = line[89:95].strip()
        _skeleton_flag = line[96:97].strip()
        _mass_stream = line[98:101].strip()
        _dataset_name = line[102:].strip()

        _dataset = Dataset(_model,
                           _raw_merged_flag,
                           _start_time,
                           _end_time,
                           _storage_medium,
                           _list_code_number,
                           _elements_list,
                           _elements_index,
                           _retention_period_days,
                           _index_records_per_dataset,
                           _index_period_minutes,
                           _record_length,
                           _skeleton_flag,
                           _mass_stream,
                           _dataset_name)

        return _datatype, _dataset

    def list_datatypes(self):
        '''Return a list of datatypes in this object.
        Omit the "pseudo-datatypes" - lines that exist in the
        retrieval_table but are not actual datatypes.
        '''
        datatypes = list(self.datatypes.keys())
        # remove the "pseudo-datatypes" that are in the retrieval_table
        for pseud in ['ASSOC', 'ELEMENTS', 'ELEMIDX', 'OFFLINE', 'STNABRV',
                      'STNICAO', 'STNIND', 'STNMAS']:
            datatypes.remove(pseud)
        return datatypes

    def write_file(self, filename):
        pass  # TODO


class Datatype:
    '''This class is the collection of Dataset objects for a datatype.

    Attributes:
        * datasets (list): a list of Dataset objects.
        * number_of_datasets (int): number of Dataset objects held.
    '''
    def __init__(self):
        '''Initialise a new Datatype object'''
        self.datasets = []
        self.number_of_datasets = 0

    def add_dataset(self, dataset):
        '''Add a Dataset object to the list for this Datatype.
        Args:
            dataset: a Dataset object
        '''
        if isinstance(dataset, Dataset):
            self.number_of_datasets += 1
            self.datasets.append(dataset)
        else:
            raise TypeError("Argument is not a Dataset object.")


class Dataset:
    """ This class represents a single Dataset entry line in a retrieval table.

        Attributes:
        * model (int): Model or area of coverage
        * raw_merged_flag (str): R for Raw, M for Merged
        * start_time (str): of storage period, blank if on-line dataset
        * end_time (str): of storage period, blank if on-line dataset
        * storage_medium (int): 1=disk, 2=tape
        * list_code_number (int): Associated data code number
        * elements_list (int): Number of ELEMENTS library
        * elements_index (str): name of relevant element_index file
        * retention_period_days (int): retention period in days
        * index_records_per_dataset (int): index records per dataset
        * index_period_minutes (int) index period in minutes
        * record_length (int): record length of dataset
        * skeleton_flag (str): T if dataset name is a MASS skeleton, else F
        * mass_stream (str): Archive MASS stream
        * dataset_name (str): Name of dataset
    """
    def __init__(self,
                 model,
                 raw_merged_flag,
                 start_time,
                 end_time,
                 storage_medium,
                 list_code_number,
                 elements_list,
                 elements_index,
                 retention_period_days,
                 index_records_per_dataset,
                 index_period_minutes,
                 record_length,
                 skeleton_flag,
                 mass_stream,
                 dataset_name):
        '''Initialise a new Dataset object'''
        self.model = model
        self.raw_merged_flag = raw_merged_flag
        self.start_time = start_time
        self.end_time = end_time
        self.storage_medium = storage_medium
        self.list_code_number = list_code_number
        self.elements_list = elements_list
        self.elements_index = elements_index
        self.retention_period_days = retention_period_days
        self.index_records_per_dataset = index_records_per_dataset
        self.index_period_minutes = index_period_minutes
        self.record_length = record_length
        self.skeleton_flag = skeleton_flag
        self.mass_stream = mass_stream
        self.dataset_name = dataset_name
