## RPC server usage stats

### Running the Script

Program currently installed at /var/moods/rpc_stats/rpc_server_usage.py
Submitted by moodsf cron every 5 minutes - two runs: one for oper and one for user stats.
Data file with timestamp, oper and user percentages will be saved to the same directory.
Graphs showing the last 24h usage created in /var/www/html/rpc_stats/plots.
Once a day (at the 23:55 run) the latest run is copied to plots/archive.
Page to display both graphs at /var/www/html/rpc_stats/rpc_graphs.html.

'''#RPC usage stats...
00,5,10,15,20,25,30,35,40,45,50,55 * * * * /var/moods/rpc_stats/run_rpc_server_usage.sh >/tmp/rpc_server_usage 2>&1
'''

