#!/bin/sh
#-----------------------------------------------------------------------
# FILE NAME      : build_mdbxprnt.sh
#
# PURPOSE        : Build the MDBXPRNT load module
#
# DESCRIPTION    : This replaces the MDB.UTILITY.SRCE version
#                  Make sure all the source code files are in the 
#                  current working directory before running this.
#
# REVISION INFO  :
# $Workfile: build_mdbxprnt.sh$ $Folder: Build_Scripts$
# $Revision: 1$ $Date: 21/11/2011 09:37:19$
#
# $Log:
#  1    Met_DB_Project 1.0         21/11/2011 09:37:19    Sheila Needham  New,
#       replaces old F77 build.
# $
#-----------------------------------------------------------------------


f95 -o "//'MCC3.DB.LOAD(MDBXPRNT)'" \
  mdbxprnt.f90 ichar3.f90 ichar2.f90 datim.f90

exit 0
