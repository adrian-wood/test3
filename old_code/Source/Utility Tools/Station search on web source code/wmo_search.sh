#!/bin/sh
#
#-------------------------------------------------------------
# This is a cgi script written by Stan Kellett G08, x6954
# This cgi script using wmo_search.exe, within this directory
# searches for details from abreviated staion master  for details
# on a submitted wmo number.
#-------------------------------------------------------------
#
# Revision Control:
#
# $Revision: 1$
# $Date: 31/08/2006 16:16:43$
# $Source: /home/us0400/httpd/cgi-bin/station_search/RCS/wmo_search.sh,v $
#
# $Log:
#  1    Met_DB_Project 1.0         31/08/2006 16:16:43    Stan Kellett    
# $
# Revision 1.1  2000/06/09  11:58:39  11:58:39  usmdb (Generic MDB account)
# Initial revision
# 
# 
#

echo "Content-Type: text/html"
echo

/home/us0400/httpd/cgi-bin/station_search/wmo_search.exe
