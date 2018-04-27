#!/usr/bin/sh
#-------------------------------------------------------------------
# Script to create HOWFULL load module from f90 source
# may be replaced by a Makefile as more load modules migrated
# to be built from unix services.
#-------------------------------------------------------------------
# Written by Stan kellett
# 
# $Date: 06/11/2009 10:49:01$
# $Revision: 2$
#
# $Log:
#  2    Met_DB_Project 1.1         06/11/2009 10:49:01    Stan Kellett    fixed
#        op folder locations
#  1    Met_DB_Project 1.0         04/11/2009 16:21:47    Stan Kellett    build
#        script for new f90 version of howfull.f
#       howfull.pre (with preprocessor statements). prepro.exe is executable
#       built from prepro.f90
# $
#
#--------------------------------------------------------------------
# (C) CROWN COPYRIGHT 2009 - MET OFFICE. All Rights Reserved.
#
# Met Office, United Kingdom
#
# The use, duplication and disclosure of this code is strictly
# prohibited without the permission of The Meteorological Database
# Team at the above address.
#---------------------------------------------------------------------
# first of all howfull.pre needs to have non IBM preprocessed comments
# stripped out

cp utility_sourceF90/howfull.pre howfull.f90
ln -fs utility_sourceF90/howfull.pre fort.10
ln -fs howfull.f90 fort.20 

./prepro.exe

rm fort.10
rm fort.20

# compile and link using temporary .f90 version of howfull.pre to build howfull load module yymmdd should be replaced
# by the year month and day for operational change day. Make sure the library exists in Z/OS first.
f95 -O4 -dusty -mismatch_all -o "//'MCC3.DBLOAD.Dyymmdd(HOWFULL)'" howfull.f90 opsourceF90/datim.f90 opsourceF90/ichar2.f90

# rm the temporary .f90 version of howfull.pre
rm howfull.f90

exit
