## Retrieval of MetDB data 

### Purpose
This application extracts data from the MetDB using the python module, formats it into one or more CSV files and sends them to DART for onward transmission to other systems (e.g. WOW server, ServiceHub).


### Documentation 
See [Technote 36: MetDB Retrieval Applications](https://metnet2/content/metdb-technote-36-metdb-retrieval-applications) for further details.

### Installation
Currently installed on mdb-apps:/var/moods/metdb_retrieval/

### crontab
```
# WOW retrievals
30 00,06,12,18 * * * /var/moods/metdb_retrieval/wow/get_data_for_wow.sh /var/moods/metdb_retrieval/wow/african_obs.cfg >>/tmp/get_wow_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1
# SREW data for ServiceHub
12,22,45 * * * * /var/moods/metdb_retrieval/ervicehub/get_srew_data.sh /var/moods/metdb_retrieval/servicehub/srew_obs.cfg >>/tmp/get_srew_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1
# RAINFALL data for ServiceHub
25 * * * * /var/moods/metdb_retrieval/servicehub/get_rainfall_data.sh /var/moods/metdb_retrieval/servicehub/rainfall_obs.cfg >>/tmp/get_rainfall_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1
```









