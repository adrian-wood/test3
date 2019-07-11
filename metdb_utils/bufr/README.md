# Python interface to the MetDB BUFR tables and utilities.

See [http://mdb-apps-test/bufr/html/](http://mdb-apps-test/bufr/html/) for usage.

Currently installed on `mdb-apps-test:/var/moods/metdb_utils/bufr`.

## Content 
  * bufr.py - source 
  * test_bufr.py - unit tests - `pytest --verbose test_bufr.py`
  * segments.py - element index generator
  * test_segments.py - unit tests - `pytest --verbose test_segments.py`
  * run all tests with `pytest --verbose`
  * /docs - generated with `sphinx-quickstart`.  To create documentation run `make html` then copy html and doctrees sub-directories of _build to the web location.
  
