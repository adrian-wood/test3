## Housekeeping script for `moodsf` on `mdb-apps` server.

### Purpose

This script performs housekeeping for the `moodsf` user on the server `mdb-apps`. This script should be amended when any new processing is added to `mdb-apps` to avoid the situation where we run out of space (as was the case for `usmdb` on `els030`).

### Running the Script
1. The scripts run on the Linux Server `mdb-apps` as the `moodsf` account via `cron`.
1. `cron` jobs are set up as follows:
```# Housekeeping for the moodsf account
00 04 * * * /var/moods/housekeeping/run_hk.sh >> /tmp/housekeeping_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1```

