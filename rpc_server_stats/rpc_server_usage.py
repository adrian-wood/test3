"""Extract current RPC usage from the web page that shows locked servers.
   Count them for linux and gpcs user ranges and append this with a
   timestamp to a text file. Retrieve the last 300 entries and plot a graph.
   Save this for web viewing and if it's the last run of the day save a
   copy for archive purposes.

   Runs as a cron job every 5 minutes.
   Needs one argument: either oper or user to produce stats for mdbabop
   or mdbapus respectively.
"""
import urllib.request, urllib.parse, urllib.error
import re
import sys
import logging
from optparse import OptionParser
import matplotlib as mpl
mpl.use('Agg')
import datetime as dt
import matplotlib.pyplot as plt

# Set the logging level for debugging
logging.basicConfig(level=logging.DEBUG)

# One argument being the server ID (no options yet)
parser = OptionParser()
(options, args) = parser.parse_args()
if len(args) != 1:
    print('One argument required - oper or user')
    sys.exit(8)

# check for valid argument
arg = args[0]

logging.debug('arg=' + arg)

if arg.lower() == 'oper':
    filename = 'prodop'
    server = 'mdbapop-prod'
    title = 'Production - Operational Users'
elif arg.lower() == 'user':
    filename = 'produs'
    server = 'mdbapus-prod'
    title = 'Production - General Users'
else:
    print('Invalid argument - oper or user')
    sys.exit(8)

# Set variables for this server
URL = r'http://' + server + '/cgi-bin/moods/rpcprog.pl'
HTML = r'/var/www/html/rpc_stats/plots/'
HOME = r'/var/www/html/rpc_stats/'

# Get RPC usage details from the locked server web page
inp = None
try:
    inp = urllib.request.urlopen(URL)
    response = inp.read()
except IOError:
    print('error opening file')
    sys.exit(8)

# parse the page, first splitting it into lines
string = []
lines = response.decode().split('<tr>')

linuxtot = 0
linuxfree = 0
gpcstot = 0
gpcsfree = 0

# Use a regex to find the words free or locked against each
# prognum in the given range. This assumes that prognums
# will only exist if they are available for use; that will
# not be true for mdb-test or if the ranges change over 
# time.
# TODO: improve the check by using a server.conf file
# to determine the actual range of servers available.

lock = r'(200[0-9]{3}).*(free|locked)'
for l in lines:
    num = re.search(lock, l)
    if num:
        prognum = num.group(1)
        status = num.group(2)
        if int(prognum) <= 200200:
            gpcstot += 1
            if status == 'free':
                gpcsfree += 1
        else:
            linuxtot += 1
            if status == 'free':
                linuxfree += 1

logging.debug('Linux=' + str(linuxtot) + ' Linux free ' + str(linuxfree))
logging.debug('GPCS =' + str(gpcstot) + ' GPCS free ' + str(gpcsfree))

linuxused = (linuxtot - linuxfree) * 100.0 / linuxtot
gpcsused = (gpcstot - gpcsfree) * 100.0 / gpcstot

logging.debug('Linux ' + str(linuxused) + '%')
logging.debug('GPCS  ' + str(gpcsused) + '%')

today = dt.datetime.now()
output = filename + '_server_usage_' + today.strftime("%Y%m%d")
latest = filename + '_server_usage_latest'
tstamp = today.strftime("%Y%m%d%H%M")
hh = int(today.strftime("%H"))
mm = int(today.strftime("%M"))

# append the details to a log file
f = open(HOME+filename + '_server_usage.data', 'a')
f.write(tstamp + ',' + str(linuxused) + ',' + str(gpcsused) + '\n')
f.close()

# produce a plot of the last 300 intervals
maxback = 300
t = 0
x = []
yo = []
yu = []

for line in reversed(open(HOME+filename + "_server_usage.data").readlines()):
    (time, linux, gpcs) = line.split(',')
    x.append(mpl.dates.date2num(dt.datetime.strptime(time, "%Y%m%d%H%M")))
    yo.append(linux)
    yu.append(gpcs)
    t += 1
    if t > maxback:
        break

# convert str to float due to changes to matplotlib
yo = list(map(float, yo))
yu = list(map(float, yu))

fig = plt.figure(figsize=(9,7))
ax = fig.add_subplot(111)

ax.plot_date(x, yo, "r-", label="Linux users")
ax.plot_date(x, yu, "b-", label="GPCS users")
ax.grid(True)

dateFmt = mpl.dates.DateFormatter('%d/%H:%M')
ax.xaxis.set_major_formatter(dateFmt)
hoursLoc = mpl.dates.HourLocator(interval=1)
ax.xaxis.set_major_locator(hoursLoc)

plt.setp(ax.get_xticklabels(), fontsize=7.)
fig.autofmt_xdate(bottom=0.18)  # adjust for date labels display

ax.set_xlabel('Local time (dd/hh:mm)')
ax.set_ylabel('server usage (%)')
ax.set_ylim([0, 100])
plt.legend(loc='best')

ax.set_title(title)

outfile = HTML + latest + '.png'
backup = HTML + 'archive/' + output + '.png'

logging.debug('output at ' + outfile)

# update the 'latest' graph
plt.savefig(outfile)

# copy it to archive if it's the last run today
if hh >= 23 and mm >= 55:
    plt.savefig(backup)
