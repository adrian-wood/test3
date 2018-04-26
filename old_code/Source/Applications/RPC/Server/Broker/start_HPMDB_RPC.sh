#!/bin/sh
#-----------------------------------------------------------------------
# Routine       : start_HPMDB_RPC.sh
#
# Purpose       : script to start HPMDB RPC servers broker and freepn
#
# Description   : This script creates the symbolic links required for
#                 HPMDB retrieval, and starts the RPC broker and
#                 freepn servers necessary for multi-user RPC access
#                 to the HPMDB.
#
# Revision info :              
# 
# $Revision: 1$
# $Date: 11/10/2006 11:35:30$
# $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/start_HPMDB_RPC.sh,v $
#
# Change history :
#
# $Log:
#  1    Met_DB_Project 1.0         11/10/2006 11:35:30    Kudsia Gwangwaa 
# $
# Revision 1.6  2003/04/23 11:18:09  usmdb
# Environment variables replace symbolic links - S.Cox
#
# Revision 1.5  2002/01/18 11:59:53  usmdb
# Removed BASE_DIR - S.Cox
#
# Revision 1.4  2001/08/21 09:17:12  usmdb
# Removed symbolic links to MetDB storage datasets - S.Cox
#
# Revision 1.3  2001/08/17 11:00:04  usmdb
# Added many subtypes - S.Cox
#
# Revision 1.2  2001/02/23 11:24:41  usmdb
# Added MDB.ATOVSG and changed other links to be in-line with latest
# HPMDB storage - S.Cox
#
# Revision 1.1  2001/01/10 12:47:08  usmdb
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
# run the HPMDB RPC servers
#-------------------------------------------------------------------

nohup broker >> ./logs/broker.output 2>&1 &
nohup freepn >> ./logs/freepn.output 2>&1 &

exit 0
