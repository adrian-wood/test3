#!/bin/sh
#-----------------------------------------------------------------------
# FILE NAME      : build_bufrshel.sh
#
# PURPOSE        : Compile the C part of BUFRSHEL
#
# DESCRIPTION    : This script will compile the bufrshel.c routines
#                  (without the -DF95 option as they will be called
#                  by Fortran 77) and copy to a PDS ready for linking
#                  in to the F77 preprocessor part of BUFRSHEL.
#
# REVISION INFO  :
# $Workfile: build_bufrshel.sh$ $Folder: Build_Scripts$
# $Revision: 1$ $Date: 28/02/2011 10:36:24$
#
# $Log:
#  1    MetDB_Refresh 1.0         28/02/2011 10:36:24    Alison Weir     Script
#        to build the C interfaces to BUFRDLL
# $
#-----------------------------------------------------------------------

# compile the bufr C interface
c89 -o bufrshel77.o -I/usr/local/lib/f95 -c -Wc,dll bufrshel.c

# link into a PDS using the BUFRDLL side-deck to resolve external
# references

c89 -o "//'TSTMDB.LOADLIB(BUFRSHL1)'" \
/usr/local/lib/f95/quickfit.o bufrshel77.o BUFRDLL.x \
/usr/local/lib/f95/libf96.a
