#!/bin/sh
#------------------------------------------------------------------------
#
# Routine     : systemtest_MetDBRPC
#
# Purpose     : Script to test run MetDB RPC.
#
# Description : This script can be run to test that MetDB RPC is working
#             : correctly from the T3E. It can also be run on an HP box.
#
#             : The script will first rpcinfo -t the MetDB broker server
#             : on the IBM. This will check that the T3E can "see" the
#             : MetDB RPC servers. If not, it will perform 6 retries 
#             : (30 second wait between each). If it still can't "see"
#             : the broker server, it will report an error and exit.
#             : If it can "see" the broker server, it will attempt a
#             : MetDB RPC call to retrieve 3 hours of SYNOP observations
#             : over the network.
#
#             : If all is well, several thousand obs will be retrieved
#             : across the network, with a return code of ISTAT=0.
#
# Usage       : The MetDB RPC client code (tar file) can be downloaded
#             : from the MetNet:
#             : /www01/metdb/mdb/librarian/MetDBRPC.tar
#
#             : Once tar unpacked, the C code is complied:
#             : T3E : make -f Makefile_T3E
#             : HP  : make -f makefile_HP
#
#             : The MetDB RPC executable can then be built:
#             : f90 -o client.exe lndsyn.f MetDBRPC.a
#
#             : The client.exe is pointed to by the variable EXEC in this
#             : script.
#
# Important   : Do not change the envronment variables METDB_SERVER_NUMBER
#             : or METDB_FREEPN_NUMBER unless instructed to by the MetDB
#             : team.
#
# $Log:
#  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
#       Initial check-in of revised client software (previously under client
#       directory)
# $
# Revision 1.5  99/07/28  15:28:46  15:28:46  usmdb (Generic MDB account)
# Addition of MDBOM to the error output messages - S.Cox
# 
# Revision 1.4  99/07/28  15:06:35  15:06:35  usmdb (Generic MDB account)
# Changed wait to 30 seconds - S.Cox
# 
# Revision 1.3  99/07/28  14:37:28  14:37:28  usmdb (Generic MDB account)
# Additions to header - S.Cox
# 
# Revision 1.2  99/07/28  12:50:29  12:50:29  usmdb (Generic MDB account)
# Correct EXEC path
# 
# Revision 1.1  99/07/28  12:17:26  12:17:26  usmdb (Generic MDB account)
# Initial revision
# 
# 
# $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/systemtest_MetDBRPC.sh,v $
# $Revision: 1$
#
#------------------------------------------------------------------------

export METDB_SERVER_NUMBER=33556608
export METDB_FREEPN_NUMBER=33556609
export METDB_DEBUG_LEVEL=1
export METDB_SERVER_TICINFO=MetDB_RPC_TIC.txt

EXEC=./client.exe

RPCINFO_SLEEP=30
RPCINFO_RETRIES=6

echo "\nT3E - IBM MetDB RPC status"
echo "=========================="

#========================================================================
#
# Can we connect to MDBOE? (rpcinfo -t ukmet $METDB_SERVER_NUMBER)
#
#========================================================================

echo "\nAttempt to connect to IBM MDBOE (RPC prognum "$METDB_SERVER_NUMBER")"
echo "------------------------------------------------------"

number=0
while [ "$number" -le "$RPCINFO_RETRIES" ]
do
  if test $number -gt 0
  then
    echo "MDBOE not responding - sleeping "$RPCINFO_SLEEP" seconds"
    sleep $RPCINFO_SLEEP
    echo "try to connect again - retry "$number" of "$RPCINFO_RETRIES
  fi
  mdboe_okay=1
  echo "rpcinfo -t ukmet $METDB_SERVER_NUMBER"
  rpcinfo -t ukmet $METDB_SERVER_NUMBER
  if test $? -eq 0
  then
    mdboe_okay=0
    echo "MDBOE (RPC prognum "$METDB_SERVER_NUMBER") Accepting requests from T3E"
    break  
  fi
  number=`expr $number + 1 `
done 
  
if test $mdboe_okay -eq 1
then
  echo
  echo "------------------------------------------------------------"
  echo "ERROR! MDBOE (RPC prognum "$METDB_SERVER_NUMBER") NOT AVAILABLE FROM T3E"
  echo
  echo "ACTION: Can MDBOE can be contacted from an HP with"
  echo "        rpcinfo -t ukmet "$METDB_SERVER_NUMBER
  echo "No    : MDBOE and MDBOF should be restarted on the IBM."
  echo "      : This will probably be done by the MetDB RPC"
  echo "      : monitoring job MDBOM - check that it has detected"
  echo "      : the problem. EJES job MDBOM"
  echo "Yes   : There is probably a T3E-IBM connection problem"
  echo "        This may be cured by restarting TCP/IP on the IBM"
  echo "------------------------------------------------------------"
  exit 1
fi

#========================================================================
#
# Can we connect to MDBOF? (rpcinfo -t ukmet $METDB_FREEPN_NUMBER)
#
#========================================================================

echo "\nAttempt to connect to IBM MDBOF (RPC prognum "$METDB_FREEPN_NUMBER")"
echo "------------------------------------------------------"

number=0
while [ "$number" -le "$RPCINFO_RETRIES" ]
do
  if test $number -gt 0
  then
    echo "MDBOF not responding - sleeping "$RPCINFO_SLEEP" seconds"
    sleep $RPCINFO_SLEEP
    echo "try to connect again - retry "$number" of "$RPCINFO_RETRIES
  fi
  mdbof_okay=1
  echo "rpcinfo -t ukmet $METDB_FREEPN_NUMBER"
  rpcinfo -t ukmet $METDB_FREEPN_NUMBER
  if test $? -eq 0
  then
    mdbof_okay=0
    echo "MDBOF (RPC prognum "$METDB_FREEPN_NUMBER") Accepting requests from T3E"
    break  
  fi
  number=`expr $number + 1 `
done 
  
if test $mdbof_okay -eq 1
then
  echo
  echo "------------------------------------------------------------"
  echo "ERROR! MDBOF (RPC prognum "$METDB_FREEPN_NUMBER") NOT AVAILABLE FROM T3E"
  echo
  echo "ACTION: Can MDBOF can be contacted from an HP with"
  echo "        rpcinfo -t ukmet "$METDB_FREEPN_NUMBER
  echo "No    : MDBOE and MDBOF should be restarted on the IBM."
  echo "      : This will probably be done by the MetDB RPC"
  echo "      : monitoring job MDBOM - check that it has detected"
  echo "      : the problem. EJES job MDBOM"
  echo "Yes   : There is probably a T3E-IBM connection problem"
  echo "        This may be cured by restarting TCP/IP on the IBM"
  echo "------------------------------------------------------------"
  exit 1
fi

#========================================================================
#
# Can we run a MetDB RPC
#
#========================================================================

echo "\nAbout to run MetDB RPC from T3E"
echo "---------------------------------"

time $EXEC

echo "\nAfter MetDB RPC from T3E\n"

exit 0
