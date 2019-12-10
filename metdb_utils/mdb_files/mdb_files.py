class RetrievalTable:
    def __init__(self, rt_file=None):
        if rt_file is None:
            self.headers = []
            self.datatypes = {}
            self.footers = []
        else:
            _rtlines = rt_file.readlines()
            self.headers = _rtlines[:7]
            for _count, _line in enumerate(_rtlines[7:]):
                if _line.strip():
                    datatype, dataset = self.unpack_line(_line)
                    if datatype not in self.datatypes:  # new dict entry needed
                        self.datatypes[datatype: None]
                    self.datatypes[datatype].add_dataset(dataset)
                else:
                    break  # blank line so we're done with dataset lines
            self.footers = _rtlines[(_count + 7):]

    def unpack_line(self, line):
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
        return list(self.datatypes.keys())

    def write_file(self, filename):
        pass  # TODO


class Datatype:

    def __init__(self):
        self.datasets = []
        self.number_of_datasets = 0

    def add_dataset(self, dataset):
        self.number_of_datasets += 1
        self.datasets.append(dataset)


class Dataset:
    """ This class represents a single Dataset entry line in a retrieval table.

        Attributes:

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
