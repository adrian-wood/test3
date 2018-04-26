#!/bin/sh

# --------------------------------------------------------------------
# Script to build The freepn_test MetDB RPC freepn server
# --------------------------------------------------------------------

TEMPDIR=/tmp/mdbrpc_freepn_test
SRCEDIR=/usr/local/mdb/source
EXECDIR=/usr/local/mdb

FREEPN_PROGNUM=0x2000771

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

#  -o $EXECDIR/freepn_test -DBPRD *.c /usr/lpp/tcpip/rpc/lib/librpclib.a
cc -o $EXECDIR/freepn_test -DBPRD *.c -l rpclib

# --------------------------------------------------------------------
# Tidy up and finish
# --------------------------------------------------------------------

rm -f *.o

cd $EXECDIR

rm -r $TEMPDIR

echo "\nfreepn_test built\n"
echo "You should now move it to /usr/local/mdb/test/bin\n"
exit
