## Retrieval of African SYNOPS to send to WOW

### Purpose
This application extracts data from the MetDB using the python module, formats it into one or more CSV files and sends them to DART for onward transmission to the WOW server.

### Running the Scripts
1. The application runs on the Linux Server `mdb-apps` under the `moodsf` account via `cron`.
1. The bash script `get_data_for_wow.sh` is a wrapper that loads the python enviroment, runs the retrieval, FTPs the output files and tidies up.  It takes one argument - the full pathname to a config file.  e.g.  
    `get_data_for_wow.sh /var/moods/metdb_retrieval/wow/african_obs.cfg`
1. The wrapper script passes the config file on to the python script `get_data_for_wow.py` via the command line option -c. e.g. `python get_data_for_wow.py -c /var/moods/metdb_retrieval/wow/african_obs.cfg`

### Configuration
The config file has one section `[REQUEST]` containing the following items:
1. `base_dir` - full path to the base directory containing the various components including the config file itself.
1. `run_time` - the time the application last ran successfully in the form YYYY-MM-DDThh:mm:ss
1. `output_dir` - full path to the directory which will contain the output files.
1. `output_file` - filename pattern for output files. The variables **dt** and **timestamp** are overwritten to produce the output files.
1. `subtype` - LNDSYN
1. `platform` - comma separated list of WMO platforms, either block or block and station number.  Only those stations that have a site ID in the WOW site table will be output.
1. `contact` - e-mail for RPC contact details.
1. `start_time` - comma separated list of observation hours.  Requests will be made in the window HH00Z - HH59Z for each of the hours in turn, up to the current time.
1. `element_file` - name of file containing details of how to produce the CSV format. `wow_elements.txt`
1. `site_file` - WOW site IDs and WMO block and station numbers. `WOW_African_Sites.csv`


### Element File
This consists of a table of three columns:
1. Number giving the order of the output column
1. CSV output column name - as required by the WOW API
1. MetDB function/element names required to produce the output.  Any valid MetDB elements can be used but the functions must be pre-defined in the python Local_funcs class (in get_data_for_wow.py).  The exception to this is sites.lookup() where lookup is a method called on the sites instance within the code.  
    The MetDB element names must be in upper-case, function names in lower-case.  Items preceded by % are fixed values transferred to the output.

### Site File
This is provided by developers of the WOW system and maps the WMO block and station numbers to WOW site IDs.  The format is specified by WOW; only first (wow site id) and third (WMO block/station) columns are used by this application.

### get_data_for_wow.py
This is the main retrieval program and contains the following classes:

**Settings()** - contains details from the config file.  Assumes unique names across all sections; these are then the attribute names.

**Elements()** - contains details read from the element mapping file.  This includes the CSV output column name and the MetDB elements and/or functions needed to produce data for that column. The element_map is a dictionary with key = the csv column and value = a list of order and function.

### wow specific modules are:
**sites.py** - containing site details read from a site file (in csv format with a header line).  Details are held in a dictionary where key is WMO block/station (string) and value is the WOW site ID.

**convert.py** - has all the individual conversion functions that can be called from the elements table.  Each method is defined as static so it can be called without having to create an instance first.

The MetDB retrieval request is built from the start_time in the config file, for example if the run is at 0630, then requests will be for 00Z and 06Z today and 12Z and 18Z yesterday.  The RECEIVED AFTER parameter is used to get data since the last run of the program.  Retrieval by WMO block is more efficient than by block/station number and any data retrieved for stations not in the WOW site list will be ignored.

### Installation
Currently installed on mdb-apps:/var/moods/metdb_retrieval/

### crontab
```# wow obs...
30 00,06,12,18 * * * /var/moods/metdb_retrieval/wow/get_data_for_wow.sh /var/moods/metdb_retrieval/wow/african_obs.cfg >>/tmp/get_testwow_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1
```









