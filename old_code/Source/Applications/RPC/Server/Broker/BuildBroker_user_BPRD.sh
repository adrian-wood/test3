#!/bin/sh

# --------------------------------------------------------------------
# Script to build the user broker on BPRD
# --------------------------------------------------------------------

TEMPDIR=/tmp/mdbrpc_broker_user
SRCEDIR=/usr/local/mdb/rpc/source
EXECDIR=/usr/local/mdb/rpc/bprd/user

BASE_PROGNUM=200200
PROGNUM_COUNT=100
BROKER_PROGNUM=0x2000894
PROGNUM_DIR=user
SERVER_DIR=user
echo $SERVER_DIR

mkdir $TEMPDIR

# --------------------------------------------------------------------
# Copy source to $TEMPDIR
# --------------------------------------------------------------------

echo "\ncopying source to $TEMPDIR\n"

  cp $SRCEDIR/msrpc_call_broker.h       $TEMPDIR/.
  cp $SRCEDIR/kill_server.h             $TEMPDIR/.
  cp $SRCEDIR/constant2.h               $TEMPDIR/constants.h
  cp $SRCEDIR/prognum_details.h         $TEMPDIR/.
  cp $SRCEDIR/kill_server.h             $TEMPDIR/.
  cp $SRCEDIR/msrpc_call_broker.c       $TEMPDIR/.
  cp $SRCEDIR/msrpc_call_broker_svc.c   $TEMPDIR/.
  cp $SRCEDIR/msrpc_call_broker_xdr.c   $TEMPDIR/.
  cp $SRCEDIR/mdb_rpc_broker2.c         $TEMPDIR/mdb_rpc_broker.c
  cp $SRCEDIR/get_prognum.c             $TEMPDIR/.
  cp $SRCEDIR/get_current_time.c        $TEMPDIR/.
  cp $SRCEDIR/print_current_time.c      $TEMPDIR/.
  cp $SRCEDIR/read_prognum_details.c    $TEMPDIR/.
  cp $SRCEDIR/write_prognum_details.c   $TEMPDIR/.
  cp $SRCEDIR/force_free_prognum.c      $TEMPDIR/.
  cp $SRCEDIR/reset_prognum.c           $TEMPDIR/.
  cp $SRCEDIR/calc_century_day.c        $TEMPDIR/.
  cp $SRCEDIR/calc_century_hour.c       $TEMPDIR/.
  cp $SRCEDIR/kill_server.c             $TEMPDIR/.
  cp $SRCEDIR/kill_server_clnt.c        $TEMPDIR/.
  cp $SRCEDIR/kill_server_xdr.c         $TEMPDIR/.

cd $TEMPDIR

# --------------------------------------------------------------------
# Edit constants.h
# --------------------------------------------------------------------

echo "editing constants.h\n"

ex constants.h <<EOD1
%s/XXXXXX/$BASE_PROGNUM
%s/YY/$PROGNUM_COUNT
%s/UUUU/$PROGNUM_DIR
%s/DDDD/$SERVER_DIR
wq
EOD1

# --------------------------------------------------------------------
# Edit msrpc_call_broker.h
# --------------------------------------------------------------------

echo "\nediting msrpc_call_broker.h\n"

ex msrpc_call_broker.h <<EOD2
%s/XXXXXXXXX/$BROKER_PROGNUM
wq
EOD2

# --------------------------------------------------------------------
# Compile source
# --------------------------------------------------------------------

echo "\ncompiling source\n"

cc -o $EXECDIR/broker_new -DBPRD *.c /usr/lpp/tcpip/rpc/lib/librpclib.a

# --------------------------------------------------------------------
# Tidy up and finish
# --------------------------------------------------------------------

rm -f *.o

cd $EXECDIR

rm -r $TEMPDIR

echo "\nbroker_new built\n"
echo "You should now move it to /usr/local/mdb/rpc/bprd/user/bin\n"
echo "Rename to broker and stop and restart the MDBOT to activate\n"
exit
