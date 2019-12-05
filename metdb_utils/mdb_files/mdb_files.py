class RetrievalTableEntry:

    def __init__(self,
                 datatype,
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
        self.datatype = datatype
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


class RetrievalTable:
    def __init__(self, rt_file):
        self.entries = []
        _rtlines = rt_file.readlines()
        self.headers = _rtlines[:7]
        for _count, _line in enumerate(_rtlines[7:]):
            if _line.strip():
                _datatype = _line[1:9].strip()
                _model = _line[10:12].strip()
                _raw_merged_flag = _line[13:14].strip()
                _start_time = _line[15:31].strip()
                _end_time = _line[32:48].strip()
                _storage_medium = _line[49:50].strip()
                _list_code_number = _line[51:53].strip()
                _elements_list = _line[54:62].strip()
                _elements_index = _line[63:71].strip()
                _retention_period_days = _line[72:77].strip()
                _index_records_per_dataset = _line[78:82].strip()
                _index_period_minutes = _line[83:88].strip()
                _record_length = _line[89:95].strip()
                _skeleton_flag = _line[96:97].strip()
                _mass_stream = _line[98:101].strip()
                _dataset_name = _line[102:].strip()
                _entry = RetrievalTableEntry(_datatype,
                                             _model,
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
                self.entries.append(_entry)
            else:
                break  # blank line so we're done
        self.footers = _rtlines[(_count + 7):]

    def datatypes(self):
        #   _datatypes = []
        #   for _entry in self.entries:
        #       _datatypes.append(_entry.datatype)
        #   return _datatypes
        _datatypes = [_entry.datatype for _entry in self.entries]
        return _datatypes
