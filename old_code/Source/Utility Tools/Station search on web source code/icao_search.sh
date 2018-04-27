#!/bin/sh
#
#-------------------------------------------------------------
# This is a cgi script written by Stan Kellett G08, x6954
#-------------------------------------------------------------
# This script calls icao_search.exe which searches the abbreviated
# icao list for a icao and outputs the information to the screen.
# 
# Revision Control:
#
# $Revision: 1$
# $Date: 31/08/2006 16:15:00$
# $Source: /home/us0400/httpd/cgi-bin/station_search/RCS/icao_search.sh,v $
#
# $Log:
#  1    Met_DB_Project 1.0         31/08/2006 16:15:00    Stan Kellett    
# $
# Revision 1.1  2000/06/09  08:51:10  08:51:10  usmdb (Generic MDB account)
# Initial revision
# 
#

echo "Content-Type: text/html"
echo

/home/us0400/httpd/cgi-bin/station_search/icao_search.exe
