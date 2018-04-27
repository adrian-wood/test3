#! /bin/sh
#----------------------------------------------------------------------
# FILE NAME      : build_mdbxtrct
#
# PURPOSE        : Compile and link MDBXTRCT
#
# DESCRIPTION    : Uses common code from the latest
#                  build and mdbxtrct source from StarTeam.
#                  Complete the build using DBJCLINK.CNTL(MDBXTRCT).
#
# REVISION INFO  :
# $Workfile: build_mdbxtrct.sh$ $Folder: Build_Scripts$
# $Revision: 1$ $Date: 14/02/2012 09:33:34$
#
# $Log:
#  1    Met_DB_Project 1.0         14/02/2012 09:33:34    Sheila Needham
#       Initial version.
# $
#----------------------------------------------------------------------
REF=/u/os/t12db/dbopsrce
f2003h -I${REF}/mods mdbxtrct.f90 \
       -o MDBXTRCT \
        ${REF}/zpdate_mod.o \
        ${REF}/int2ch_mod.o \
        ${REF}/MetDB_c_utils.o \
        ${REF}/dt2hrs.o \
        ${REF}/eb2asc.o \
        ${REF}/char2.o \
        ${REF}/char3.o \
        ${REF}/ichar2.o \
        ${REF}/ichar3.o
