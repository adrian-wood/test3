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
#  2    Met_DB_Project 1.1         21/09/2011 10:57:06    Sheila Needham
#       Updated for new servers
#  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
#       Initial check-in of revised client software (previously under client
#       directory)
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
# $Revision: 2$
#
#------------------------------------------------------------------------

METDB_SERVER_NUMBER=33556628
METDB_FREEPN_NUMBER=33556629
METDB_SERVER_TICINFO=MetDB_RPC_TIC.txt
METDB_RPC_TIMEOUT=5400
METDB_DEBUG_LEVEL=0
METDB_CLIENT_CONTACT=sheila.needham@metoffice.gov.uk
METDB_SERVER_IPNAME=bprd

export METDB_SERVER_NUMBER
export METDB_FREEPN_NUMBER
export METDB_SERVER_TICINFO
export METDB_RPC_TIMEOUT
export METDB_DEBUG_LEVEL
export METDB_SERVER_IPNAME
export METDB_CLIENT_CONTACT

echo "compiling"
ifort lndsyn.f MetDBRPC3.a -o client.exe

echo "executing"
./client.exe

echo "complete"
exit
