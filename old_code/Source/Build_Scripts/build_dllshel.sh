#!/bin/sh
#----------------------------------------------------------------------
# FILE NAME      : build_dllshel
#
# PURPOSE        : Compile the C part of MDBSHEL and BUFRSHEL
#
# DESCRIPTION    : This script will compile the mdbc.c and bufrshel.c
#                  routines and copy to a PDS ready for linking
#                  in to the F77 preprocessor parts of the shells,
#                  the aim being to create a combined shell for both 
#                  mdb and bufr.
#
# REVISION INFO  :
# $Workfile: build_dllshel.sh$ $Folder: Build_Scripts$
# $Revision: 2$ $Date: 07/03/2011 13:50:51$
#
# $Log:
#  2    MetDB_Refresh 1.1         07/03/2011 13:50:51    Alison Weir
#       Correct comments
#  1    MetDB_Refresh 1.0         28/02/2011 10:36:50    Alison Weir     Script
#        to build the C interfaces to BUFRDLL and MDBDLL, when combined in one
#        shell
# $
#----------------------------------------------------------------------

# Compile the bufr and mdb C interfaces

# Note that bufrshel.c result is named bufrshel77.o to avoid conflict
# with the version compiled with the -DF95 option for MDBDLL.
c89 -o bufrshel77.o -I/usr/local/lib/f95 -c -Wc,dll bufrshel.c
c89 -I/usr/local/lib/f95 -c -Wc,dll mdbc.c

# Link into a PDS using the MDBDLL and BUFRDLL side-decks to 
# resolve external references

c89 -o "//'TSTMDB.LOADLIB(SHELLC)'" \
bufrshel77.o BUFRDLL.x \
mdbc.o MDBDLL.x \
/usr/local/lib/f95/quickfit.o \
/usr/local/lib/f95/libf96.a
