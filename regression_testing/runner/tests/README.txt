This directory contains two sets of units tests which can be run as follows:
module load scitools
python -m unittest -v

This should end in something like
----------------------------------------------------------------------
Ran 12 tests in 0.236s

OK

Unit tests are for the TestAnything module (used for running the regression tests)
and the webapi module (used for the specific web API regression tests).
These unit tests are not used in the actual regression tests, they are just for 
testing changes to the regression framework.

