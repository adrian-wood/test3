# stnmas_utils

## Monthly WMO Station Updates
Files: 
```
convert_vola.py
stnmas_updates.sh
```
Please see [MetDB WI15: Maintaining MetDB's Station List - STNMAS](https://metnet2/content/metdb-wi15-maintaining-metdbs-station-list-stnmas) for details.

## Web Utilities
The home.html page is deployed at `mdb-apps:/var/www/html/stations/` - this contains links for browsing recent versions of STNMAS and other station datasets.

## Station Search
Files: 
```
stnlist_input.html   
stnlist_template.html 
stnerr_template.html 
station_search.py 
test_station_search.py
```
The HTML pages are deployed at `mdb-apps:/var/www/html/stations`
The CGI-script is deployed at `mdb-apps:/var/www/cgi-bin/stations`

`test_station_search.py` has unit tests for the cgi-script.  To run the tests, download both programs to the same directory then
```
module load scitools
python -m unittest -v test_station_search.py
```






