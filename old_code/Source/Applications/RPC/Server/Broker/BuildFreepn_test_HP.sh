#!/bin/sh

# --------------------------------------------------------------------
# Script to build The freepn_test MetDB RPC freepn server
# --------------------------------------------------------------------

TEMPDIR=/var/tmp/mdbrpc_freepn_test
SRCEDIR=${BASE_DIR}/apps/RPC/test_system/broker_code
EXECDIR=${BASE_DIR}/apps/RPC/test_system

FREEPN_PROGNUM=0x2000771

rm -rf $TEMPDIR
mkdir $TEMPDIR

# --------------------------------------------------------------------
# Copy source to $TEMPDIR
# --------------------------------------------------------------------

echo "\ncopying source to $TEMPDIR\n"

  cp $SRCEDIR/msrpc_call_freepn.h       $TEMPDIR/.
  cp $SRCEDIR/constants.h               $TEMPDIR/.
  cp $SRCEDIR/prognum_details.h         $TEMPDIR/.
  cp $SRCEDIR/msrpc_call_freepn.c       $TEMPDIR/.
  cp $SRCEDIR/msrpc_call_freepn_svc.c   $TEMPDIR/.
  cp $SRCEDIR/msrpc_call_freepn_xdr.c   $TEMPDIR/.
  cp $SRCEDIR/print_current_time.c      $TEMPDIR/.
  cp $SRCEDIR/free_prognum.c            $TEMPDIR/.
  cp $SRCEDIR/read_prognum_details.c    $TEMPDIR/.
  cp $SRCEDIR/write_prognum_details.c   $TEMPDIR/.

cd $TEMPDIR

# --------------------------------------------------------------------
# Edit msrpc_call_freepn.h
# --------------------------------------------------------------------

echo "editing msrpc_call_freepn.h\n"

ex msrpc_call_freepn.h <<EOD1
%s/XXXXXXXXX/$FREEPN_PROGNUM
wq
EOD1

# --------------------------------------------------------------------
# Compile source
# --------------------------------------------------------------------

echo "\ncompiling source\n"

cc -o $SRCEDIR/freepn_test -lnsl -DHPUX *.c

# --------------------------------------------------------------------
# Tidy up and finish
# --------------------------------------------------------------------

rm -f *.o

cd $WORKDIR

rm -r $TEMPDIR

echo ; echo "freepn_test built in "$SRCEDIR
echo "You should now move it to "$EXECDIR ; echo 

exit
