#! /bin/sh
#----------------------------------------------------------------------
# FILE NAME      : build_cartsub
#
# PURPOSE        : Compile and link MetDB archiving routine CARTSUB.
#
# DESCRIPTION    : Uses common code zpdate and datim from the latest
#                  build and submit.c and cartsub.f90 source
#                 from StarTeam.
#                 Check the -o destination of the executable
#                 before running.
#
# REVISION INFO  :
# $Workfile: build_cartsub.sh$ $Folder: Build_Scripts$
# $Revision: 2$ $Date: 09/03/2012 11:59:56$
#
# $Log:
#  2    Met_DB_Project 1.1         09/03/2012 11:59:56    Sheila Needham
#       updated load library reference
#  1    Met_DB_Project 1.0         14/02/2012 09:09:18    Sheila Needham
#       Replacement for DBJCLLIB.CNTL(CARTSUB)
# $
#----------------------------------------------------------------------
OLD_CODE=/u/os/t12db/dbopsrce
xlc -c submit.c
f2003h -o "//'mcc3.dbload.dyymmdd(cartsub)'" \
    cartsub.f90 submit.o \
    $OLD_CODE/zpdate_mod.o $OLD_CODE/datim.o
exit 0
