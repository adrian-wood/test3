#!/bin/sh
#
#-------------------------------------------------------------
# This is a cgi script written by Stan Kellett G08, x6954
# This script seaches for details from the Full Station Master
# of a submitted ICAO value using display_full_icao_details.exe
#-------------------------------------------------------------
#
# Revision Control:
#
# $Revision: 1$
# $Date: 13/11/2006 16:47:30$
# $Source: /home/us0400/httpd/cgi-bin/station_search/RCS/search_full_icaolist.sh,v $
#
# $Log:
#  1    Met_DB_Project 1.0         13/11/2006 16:47:30    Kudsia Gwangwaa 
# $
# Revision 1.2  2000/06/29  10:55:58  10:55:58  usmdb (Generic MDB account)
# changed /dev/ to /station_search/
# 
# Revision 1.1  2000/06/09  11:20:34  11:20:34  usmdb (Generic MDB account)
# Initial revision
# 
#

echo "Content-Type: text/html"
echo

# !1.2 changed /dev/ to /station_search/
/home/us0400/httpd/cgi-bin/station_search/display_full_icao_details.exe $QUERY_STRING
