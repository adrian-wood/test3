#!/bin/sh
#------------------------------------------------------------------------
#
# Example script to run MetDB RPC.
#
# Important - do not change the envronment variables METDB_SERVER_NUMBER
#             or METDB_FREEPN_NUMBER unless instructed to by the MetDB
#             team.
#
# $Log:
#  1    Met_DB_Project 1.0         11/10/2006 11:27:20    Kudsia Gwangwaa 
# $
# Revision 1.4  2003/03/14 12:44:17  usmdb
# Separated environment variable initialisation and export.
# Added METDB_SERVER_IPNAME - S.Cox
#
# Revision 1.3  2000/09/05  11:05:01  11:05:01  usmdb (Generic MDB account)
# Addition of environment variables METDB_RPC_TIMEOUT and
# METDB_DEBUG_LEVEL - S.Cox
# 
# Revision 1.2  2000/08/08  11:28:01  11:28:01  usmdb (Generic MDB account)
# Addition of ./ before time - S.Cox
# 
# Revision 1.1  99/03/15  11:12:55  11:12:55  usmdb (Generic MDB account)
# Initial revision
# 
# $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/run_MetDBRPC.sh,v $
# $Revision: 1$
#
#------------------------------------------------------------------------

METDB_SERVER_NUMBER=33556608
METDB_FREEPN_NUMBER=33556609
METDB_SERVER_TICINFO=MetDB_RPC_TIC.txt
METDB_RPC_TIMEOUT=5400
METDB_DEBUG_LEVEL=0
METDB_SERVER_IPNAME=ukmet

export METDB_SERVER_NUMBER
export METDB_FREEPN_NUMBER
export METDB_SERVER_TICINFO
export METDB_RPC_TIMEOUT
export METDB_DEBUG_LEVEL
export METDB_SERVER_IPNAME

./client.exe

exit
