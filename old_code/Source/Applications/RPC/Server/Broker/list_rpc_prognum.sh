#!/bin/sh

#-----------------------------------------------------------------------
#
# routine     : list_rpc_prognum.sh
#
# purpose     : to produce a summary list of the MetDB RPC prognum
#             : status files.
#
# description : CGI script, called by html form. This script loops over
#             : the prognum status files, reading the contents of each
#             : into an array. The array is then output so the details
#             : for a file are contained in a single row.
#
# called by   : /home/us0400/mdb/scripts/mdb_rpc_tools.html
#
# author      : Simon Cox MetDB Team 11 Feb 2000
#
# $Revision: 1$
# $Date: 11/10/2006 11:35:22$
# $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/list_rpc_prognum.sh,v $
#
#-----------------------------------------------------------------------
# $Log:
#  1    Met_DB_Project 1.0         11/10/2006 11:35:22    Kudsia Gwangwaa 
# $
# Revision 1.2  2000/03/07  09:42:57  09:42:57  usmdb (Generic MDB account)
# Changed from ksh to sh. IBM unix partition only has
# a posix shell - S.Cox
# 
# Revision 1.1  2000/02/22  09:44:12  09:44:12  usmdb (Generic MDB account)
# Initial revision
# 
#-----------------------------------------------------------------------

echo "Content-Type: text/html"
echo

#-----------------------------------------------------------------------
# html header
#-----------------------------------------------------------------------

echo "<html>"
echo "<head>"
echo "<title>MetDB multi-user RPC prognum status</title>"
echo "<META HTTP-EQUIV="expires" CONTENT="0">"
echo "</head>"

echo "<body bgcolor="#FFFFFF" link="#0000FF" vlink="#800080">"
echo "<pre>"
echo "MetDB multi-user RPC prognum status"
echo
echo "Current time: "`date`
echo

#-----------------------------------------------------------------------
# loop over pronum files in /usr/local/mdb/bin. Read the contents of
# each file into an array. Output array
#-----------------------------------------------------------------------

DATA_DIR=/usr/local/mdb/bin

typeset text[8]
for i in `ls ${DATA_DIR}/prognum_200*.txt`
do
  count=0
  for j in `cat ${i}`
  do
    let count=count+1
    text[${count}]=${j}
  done
  echo ${text[*]}
done

echo "</pre>"
echo "</body>"
echo "</html>"
