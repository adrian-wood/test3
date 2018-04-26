#!/bin/sh
#
#-------------------------------------------------------------
# This is a cgi script written by Stan Kellett G08, x6954
# This cgi script uses display_full_wmo_details.exe, within 
# this directory.
# It searches for details from the Full Station Master using
# the WMO number as a key input as QUERY_STRING
#-------------------------------------------------------------
#
# Revision Control:
#
# $Revision: 1$
# $Date: 31/08/2006 16:17:38$
# $Source: /home/us0400/httpd/cgi-bin/station_search/RCS/search_full_list.sh,v $
#
# $Log:
#  1    Met_DB_Project 1.0         31/08/2006 16:17:38    Stan Kellett    
# $
# Revision 1.2  2000/06/29  10:53:39  10:53:39  usmdb (Generic MDB account)
# changed /dev/ to /station_search/
# 
# Revision 1.1  2000/06/26  15:50:32  15:50:32  usmdb (Generic MDB account)
# Initial revision
# 
#

echo "Content-Type: text/html"
echo

# !1.2 changed /dev/ to /station_search/
/home/us0400/httpd/cgi-bin/station_search/display_full_wmo_details.exe $QUERY_STRING
