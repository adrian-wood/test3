'''
The DataAccessLog module contains classes relating to MetDB data_access.log
files, which record every retrieval from MetDB.
'''

import re
from collections import defaultdict


class DataAccessLog:
    '''This class represents a collection of retrievals from MetDB, typically
    but not necessarily all the retrievals recorded in a data_access.log file.

    Attributes:
        * retrievals (list): a list of MetDBRetrieval objects.
        * retrieval_count (int): the total number of retrievals.
        * count_by_datatype (dict): the number of times that a retrieval for a
                                    specific datatype occurs in the collection
        * count_by_userid (dict): the number of times that a retrieval for a
                                  specific user occurs in the collection.
        * count_by_contact (dict): the number of times that a retrieval for a
                                   specific contact occurs in the collection.
        * userid_contact (dict): a dictionary whose keys are userids appearing
                                 in the collection, with values of a further
                                 dictionary whose keys are contacts and values
                                 of the number of times that a retrieval for
                                 that particular userid/contact appears.
        * contact_userid (dict): the opposite of userid_contact, i.e.
                                 a dictionary whose keys are contacts appearing
                                 in the collection, with values of a further
                                 dictionary whose keys are userids and values
                                 of the number of times that a retrieval for
                                 that particular contact/userid appears.
    '''
    def __init__(self, da_log_file=None):
        '''Initialize an empty DataAccessLog object and populate it with the
        content of the supplied data_access.log file, if any.

        Args:
            da_log_file: A file object (optional).
        '''
        self.retrievals = []
        self.retrieval_count = 0
        self.count_by_datatype = defaultdict(int)
        self.count_by_userid = defaultdict(int)
        self.count_by_contact = defaultdict(int)
        self.userid_contact = defaultdict(lambda: defaultdict(int))
        self.contact_userid = defaultdict(lambda: defaultdict(int))
        if da_log_file is not None:
            self.read_da_log(da_log_file)

    def read_da_log(self, da_log_file):
        '''Read a data_access.log file, creating a MetDBRetrieval object for
        each line.

        Args:
            da_log_file: A file object.
        '''
        _da_log_lines = da_log_file.readlines()
        for _count, _line in enumerate(_da_log_lines):
            _log_ts = _line[0:15]  # always 1st 15 chars, but no year, arggh
            _host = _line.split()[3]
            _item_list = re.findall(r"[^[]*\[([^]]*)\]", _line)
            _RPC_server_number = _item_list[0]
            _timestamp = _item_list[1]
            _program_number = _item_list[2]
            _userid = _item_list[3]
            _client = _item_list[4]
            _contact = _item_list[5]
            _datatype = _item_list[6].strip()
            if len(_item_list) == 10:
                _nobs = _item_list[7]
                _nelem = _item_list[8]
                _req = _item_list[9]
            elif len(_item_list) == 8:  # RPC_ATOMIC don't have nobs or nelem
                _nobs = None
                _nelem = None
                _req = _item_list[7]
            retrieval = MetDBRetrieval(_log_ts, _host, _RPC_server_number,
                                       _timestamp, _program_number,
                                       _userid, _client, _contact,
                                       _datatype, _nobs, _nelem, _req)
            self.retrievals.append(retrieval)
            self.retrieval_count += 1
            self.count_by_datatype[_datatype] += 1
            self.count_by_userid[_userid] += 1
            self.count_by_contact[_contact] += 1
            self.userid_contact[_userid][_contact] += 1
            self.contact_userid[_contact][_userid] += 1


class MetDBRetrieval:
    '''This class effectively represents a single line in a data_access.log
    file, i.e. a single MetDB Retrieval.

    Attributes:
        * log_timestamp (str): timestamp of logfile entry
        * host (str): host the request originated from
        * RPC_server_number (int): RPC server number used
        * timestamp (int): epoch timestamp (?)
        * program_number (int): program number used
        * userid (str): User ID
        * client (str): client
        * contact (str): contact email address
        * datatype (str): MetDB datatype requested
        * nobs (int): number of obs per request (maybe None)
        * nelem (int): number of elements retrievd (maybe None)
        * partial_request_strin (str): MetDB request string
    '''
    def __init__(self,
                 log_timestamp,
                 host,
                 RPC_server_number,
                 timestamp,
                 program_number,
                 userid,
                 client,
                 contact,
                 datatype,
                 nobs,
                 nelem,
                 partial_request_string):
        '''Initialize a MetDBRetrieval object populated with the arguments
        supplied (typically read from a line in the data_access.log file).

        Args:
            log_timestamp: timestamp of the entry in the log
            host: name of the host where the RPC server is running
            RPC_server_number: RPC server number in use
            timestamp: timestamp in MetDB
            program_number: program number used to handle the request
            userid: userID issuing the request
            client: client issuing the request
            contact: contact issuing the request
            datatype: datatype being retrieved
            nobs: number of observations retrieved in one call
            nelem: number of elements being retrieved
            partial_request_string: the partial CEQ string
        '''
        self.log_timestamp = log_timestamp
        self.host = host
        self.RPC_server_number = RPC_server_number
        self.timestamp = timestamp
        self.program_number = program_number
        self.userid = userid
        self.client = client
        self.contact = contact
        self.datatype = datatype
        self.number_of_observations_per_call = nobs
        self.number_of_elements_retrieved = nelem
        self.partial_request_string = partial_request_string
