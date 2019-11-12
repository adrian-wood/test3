.. MetDB BUFR documentation master file, created by
   sphinx-quickstart on Mon Jun 10 11:24:18 2019.

MetDB BUFR Python
=================

The **metdb_utils/bufr** package provides a Python interface to MetDB BUFR tables with
associated utilities.

BUFR tables are the MetDB files `bufr_tableb` and `bufr_tabled` in standard format. 
Environment variable BUFR_LIBRARY gives the path to both files; if not defined 
then tables should be in the current working directory.


There are also modules for accessing `bufr_localseq` files 
and `element_index` files, for example to generate new element_indexes from
a BUFR sequence and to help create SSOT config files.

The package is currently installed on mdb-apps-test:/var/moods/metdb_utils/bufr and
should be used within a scitools environment.

You can access the modules in a number of ways:

1. Interactively::

   $ module load scitools
   $ export PYTHONPATH=/var/moods/metdb_utils/bufr
   $ python
   >>> import bufr

2. On the command line::

   $ module load scitools
   $ export PYTHONPATH=/var/moods/metdb_utils/bufr
   $ export BUFR_LIBRARY=/home/moodsf/MetDB_BUFR25.0.00/tables/
   $ python /var/moods/metdb_utils/bufr/lookup.py aatsr

3. From your programs

.. code-block:: python

   import sys
   import os
   sys.path.append('/var/moods/metdb_utils/bufr')
   import elements
   os.environ['BUFR_LIBRARY'] = '/home/moodsf/MetDB_BUFR25.0.00/tables/'
   table = elements.read_elements('aatsr')


BUFR Tables
===========

.. automodule:: bufr
   :members:

Local Sequences
===============

.. automodule:: localseq
   :members: 

Elements Index Generator
========================

.. automodule:: elements
   :members: process

Elements Index Classes
======================

.. automodule:: ElementClass
   :members:

Single Source Of Truth
======================

.. automodule:: lookup

.. toctree::
   :maxdepth: 2
   :caption: Contents:

Contents
========

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

