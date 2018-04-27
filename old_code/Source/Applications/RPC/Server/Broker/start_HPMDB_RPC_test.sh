#!/bin/sh
#-----------------------------------------------------------------------
# Routine       : start_HPMDB_RPC_test.sh
#
# Purpose       : script to start HPMDB RPC servers broker_test and
#               : freepn_test
#
# Description   : This script creates the symbolic links required for
#                 HPMDB retrieval, and starts the RPC broker_test and
#                 freepn_test servers necessary for multi-user RPC
#                 access to the HPMDB.
#
# Revision info :              
# 
# $Revision: 1$
# $Date: 11/10/2006 11:35:30$
# $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/start_HPMDB_RPC_test.sh,v $
#
# Change history :
#
# $Log:
#  1    Met_DB_Project 1.0         11/10/2006 11:35:30    Kudsia Gwangwaa 
# $
# Revision 1.7  2003/04/23 11:20:03  usmdb
# Environment variables replace symbolic links - S.Cox
#
# Revision 1.6  2002/01/18 11:56:49  usmdb
# Removed BASE_DIR - S.Cox
#
# Revision 1.5  2001/08/21 09:17:12  usmdb
# Removed symbolic links to MetDB storage datasets - S.Cox
#
# Revision 1.4  2001/08/17 11:00:49  usmdb
# Added many subtypes - S.Cox
#
# Revision 1.3  2001/02/23 11:24:41  usmdb
# Added MDB.ATOVSG and changed other links to be in-line with latest
# HPMDB storage - S.Cox
#
# Revision 1.2  2001/01/10 12:48:19  usmdb
# Correct header re server names - S.Cox
#
# Revision 1.1  2001/01/10  12:34:24  12:34:24  usmdb (Generic MDB account)
# Initial revision
# 
#-----------------------------------------------------------------------

#-------------------------------------------------------------------
# set up environment variablee.
#-------------------------------------------------------------------

export BUFR_LIBRARY=${METDB_BASE_DIR}/data/
export METDB_ELEMIDX=${METDB_BASE_DIR}/data/element.index
export METDB_STNABRV=${METDB_BASE_DIR}/data/MDB.STNMAS.ABRVLST

#-------------------------------------------------------------------
# run the HPMDB RPC test servers
#-------------------------------------------------------------------

nohup broker_test >> ./logs/broker_test.output 2>&1 &
nohup freepn_test >> ./logs/freepn_test.output 2>&1 &

exit 0
