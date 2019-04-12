# SSOT - Single Source Of Truth

## Background
This is a WIP to create a central repository of information about MetDB data subtypes so that other outputs can be generated from it.  Examples of locations containing MetDB subtype information include:
* subtype web pages https://metnet2/content/metdb-retrieval-aatsr-data-metdb
* metdb-python subtypes.py module https://exxgitrepo:8443/projects/MOOD/repos/metdb-python/browse/lib/metdb/subtypes.py
* ServiceHub element retrieval lists https://exxgitrepo:8443/projects/MOOD/repos/metdb-misc/browse/metdb_retrieval/servicehub/aireps_elements.txt
* web retrieval tables https://exxgitrepo:8443/projects/MOOD/repos/metdb/browse/TABLES/web_element_retrieval/amdars
* element indexes https://exxgitrepo:8443/projects/MOOD/repos/metdb/browse/TABLES/elements_index/amdars
* storage headers https://exxgitrepo:8443/projects/MOOD/repos/metdb/browse/TABLES/storage_data/hdramd1
* and more ...

If we can capture all this information in one set of config files, we can use jinja2 templates to produce specific datasets in the required format.

## Installation

Currently installed on mdb-apps-test:/var/moods/ssot with web pages at /var/www/html/ssot/

## Layout

### lib
Contains programs for processing and validating config files.

### configs
Contains the configuration files for different data types, each in their own sub-directory. Format to be defined.

### templates
Jinja2 templates for producing output data in a variety of formats, e.g. subtype web pages, metdb-python subtype code snippets.

### docs
TBD

## Usage
Currently we are only using configs to produce metdb-python subtypes.py snippets.  Create a sub-directory in /datatypes and an elements.cfg file.  The only variables needed at this stage are description and returned type for each element. The returned types are I for integer (which includes code/flag tables), R for real or S with a string length for character elements, e.g.

[BUOY_IDNY]

description = Buoy identifier

returned_type = I


[STTN_NAME]

description = Station name or other identification

returned_type = S32


[DROG_DPTH]

description = Drogue depth

returned_type = R


[STTN_RPRT_TYPE]

description = Type of station

reported_units = code

table_id = 002001

returned_type = I


If the element is a code or flag table, include reported_units and table_id as well.


Programs run in the scitools environment:

`module load scitools`

`python create_subtypes.py <directory containing elements.cfg>`

This will create two files: subtypes.py and subtypes_snippet.py in the datatypes/directory - subtype_snippet.py can be pasted into a python program to temporarily add new information to the metdb-ptyhon module; sutbypes.py is in the correct format to be pasted into the metdb-python/subtypes.py module as part of a regular update.


