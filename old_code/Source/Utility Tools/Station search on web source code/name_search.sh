#!/bin/sh
#
#-------------------------------------------------------------
# This is a cgi script written by Stan Kellett G08, x6954
#-------------------------------------------------------------
#
# This script runs name_search.exe which searches both the abbreviated
# Station Master and the abbreviated ICAO list, for a input string in
# Stations name. The output is output to the screen on a seperate web page.
#
# Revision Controll:
#
# $Revision: 1$
# $Date: 31/08/2006 16:16:02$
# $Source: /home/us0400/httpd/cgi-bin/station_search/RCS/name_search.sh,v $
#
# $Log:
#  1    Met_DB_Project 1.0         31/08/2006 16:16:02    Stan Kellett    
# $
# Revision 1.2  2000/06/29  11:15:08  11:15:08  usmdb (Generic MDB account)
# changed /dev/ to /station_search/, Stan Kellett.
# 
# Revision 1.1  2000/06/29  10:42:06  10:42:06  usmdb (Generic MDB account)
# Initial revision
# 
#

echo "Content-Type: text/html"
echo

# !1.2 changed /dev/ to /station_search/
/home/us0400/httpd/cgi-bin/station_search/name_search.exe
