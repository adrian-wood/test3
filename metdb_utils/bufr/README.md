# Python interface to the MetDB BUFR tables and utilities.

See [http://mdb-apps-test/bufr/html/](http://mdb-apps-test/bufr/html/) for usage.

Currently installed on `mdb-apps-test:/var/moods/metdb_utils/bufr`.

## Content 
  * bufr.py - bufr table utilities
  * ElementClass.py - objects representing elements_index
  * elements.py - element index generator
  * localseq.py - bufr local sequence reader
  * lookup.py - create SSOT outline elements
  * run all tests with `module load scitools; python -m unittest -v`
  * /docs - generated with `sphinx-quickstart`.  To create documentation run `make html` then copy html and doctrees sub-directories of _build to the web location.
  
