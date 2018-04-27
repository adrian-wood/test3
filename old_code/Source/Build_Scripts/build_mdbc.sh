#! /bin/sh
# ---------------------------------------------------------------
# Script : build_mdbc.sh
#
# Purpose: To build the mdb C interface that's needed by MDBSHEL.
#
#          The load module created here is included when making
#          MDB.LOADLIB(MDBSHEL)
# ---------------------------------------------------------------
# compile the mdb C interface

c89 -I/usr/local/lib/f95 -c -Wc,dll mdbc.c

# link into a PDS using the MDBDLL side-deck to resolve external
# references (to mdbwrap)

c89 -o "//'MCC3.SNLIB.LOAD(MDBC)'" \
  /usr/local/lib/f95/quickfit.o mdbc.o MDBDLL.x \
  /usr/local/lib/f95/libf96.a
