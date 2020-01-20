#!/bin/bash -l
module load scitools
module display scitools
python /var/moods/rpc_stats/rpc_server_usage.py oper
python /var/moods/rpc_stats/rpc_server_usage.py user
