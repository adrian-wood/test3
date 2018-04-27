#!/bin/sh

# --------------------------------------------------------------------
# Script to build the broker_test MetDB RPC broker server
# --------------------------------------------------------------------

TEMPDIR=/var/tmp/mdbrpc_broker_test
SRCEDIR=${BASE_DIR}/apps/RPC/test_system/broker_code
EXECDIR=${BASE_DIR}/apps/RPC/test_system

BASE_PROGNUM=200000
PROGNUM_COUNT=20
BROKER_PROGNUM=0x2000770

rm -rf $TEMPDIR
mkdir $TEMPDIR

# --------------------------------------------------------------------
# Copy source to $TEMPDIR
# --------------------------------------------------------------------

echo "\ncopying source to $TEMPDIR\n"

  cp $SRCEDIR/msrpc_call_broker.h       $TEMPDIR/.
  cp $SRCEDIR/kill_server.h             $TEMPDIR/.
  cp $SRCEDIR/constants.h               $TEMPDIR/.
  cp $SRCEDIR/prognum_details.h         $TEMPDIR/.
  cp $SRCEDIR/kill_server.h             $TEMPDIR/.
  cp $SRCEDIR/msrpc_call_broker.c       $TEMPDIR/.
  cp $SRCEDIR/msrpc_call_broker_svc.c   $TEMPDIR/.
  cp $SRCEDIR/msrpc_call_broker_xdr.c   $TEMPDIR/.
  cp $SRCEDIR/mdb_rpc_broker.c          $TEMPDIR/.
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

cc -o $SRCEDIR/broker_test -lnsl -DHPUX *.c

# --------------------------------------------------------------------
# Tidy up and finish
# --------------------------------------------------------------------

rm -f *.o

cd $WORKDIR

rm -r $TEMPDIR

echo ; echo "broker_test built in "$SRCEDIR
echo "You should now move it to "$EXECDIR ; echo

exit
