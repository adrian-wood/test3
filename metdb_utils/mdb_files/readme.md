# Python Interfaces to MetDB Files

## Purpose

This library is provided to enable easy access from Python to a variety of MetDB files, such as `data_access.log` files, the `retrieval_table` and so on.

It is the user's responsibility to obtain these MetDB files; there is no automatic retrieval from MetDB servers or BitBucket for example.

The intention is that when access to a MetDB file is required from Python, a new module is written and added to this library, so others can use it later.

There is also the possibility at some point in the future of the MetDB files being created by these libraries.

## Generated Documentation
See [http://mdb-apps-test/mdb_files/html/](http://mdb-apps-test/mdb_files/html/) for generated documentation.

To re-create documentation:

```
$ cd <metdb-misc-repo>/metdb_utils/mdb_files/docs
$ module load scitools
$ make html
Running Sphinx v1.8.4
...
```
Copy the resulting `html` and `doctrees` subdirectories (under `_build`) to the desired location, usually `/var/www/html/mdb_files`.

## Tests
Every module should have unit tests written for it; they should be added to the `test_mdb_files.py` script.

"Canned" data for use by the unit tests can be added to the `test_data` directory, updating the `readme.md` in that directory at the same time.

To run the unit tests, (assuming you have cloned this repo to `~/metdb-misc`):

```
$ cd ~/metdb-misc/metdb_utils/mdb_files
$ module load scitools
$  python -m unittest discover -v
test_count_by_datatype (test_mdb_files.Test_Data_Access_Log) ... ok
test_count_datasets (test_mdb_files.Test_Retrieval_Table) ... ok
test_count_datatypes (test_mdb_files.Test_Retrieval_Table) ... ok
test_list_datatypes (test_mdb_files.Test_Retrieval_Table) ... ok

----------------------------------------------------------------------
Ran 4 tests in 0.038s

OK
```