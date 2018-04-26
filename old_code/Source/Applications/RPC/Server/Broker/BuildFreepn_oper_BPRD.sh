#!/bin/sh

# --------------------------------------------------------------------
# Script to build the oper freepn server on BPRD
# --------------------------------------------------------------------

TEMPDIR=/tmp/mdbrpc_freepn_oper
SRCEDIR=/usr/local/mdb/rpc/source
EXECDIR=/usr/local/mdb/rpc/bprd/oper

FREEPN_PROGNUM=0x200088B

mkdir $TEMPDIR

# --------------------------------------------------------------------
# Copy source to $TEMPDIR
# --------------------------------------------------------------------

echo "\ncopying source to $TEMPDIR\n"

  cp $SRCEDIR/msrpc_call_freepn.h       $TEMPDIR/.
  cp $SRCEDIR/constant2.h               $TEMPDIR/constants.h
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
# Edit constants.h
# --------------------------------------------------------------------

echo "editing constants.h\n"

ex constants.h <<EOD1
%s/UUUU/oper
%s/DDDD/oper
wq
EOD1

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

cc -o $EXECDIR/freepn_new -DBPRD *.c /usr/lpp/tcpip/rpc/lib/librpclib.a
#cc -o $EXECDIR/freepn_oper *.c -l rpclib

# --------------------------------------------------------------------
# Tidy up and finish
# --------------------------------------------------------------------

rm -f *.o

cd $EXECDIR

rm -r $TEMPDIR

echo "\nfreepn_new built\n"
echo "You should now move it to /usr/local/mdb/rpc/bprd/oper/bin\n"
echo "Rename to freepn and stop and restart MDBOR to activate\n"
exit
