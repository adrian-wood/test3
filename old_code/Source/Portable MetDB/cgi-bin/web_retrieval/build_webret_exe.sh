#!/bin/sh
#-------------------------------------------------------------------
# ROUTINE       : build_webret_exe.sh
#
# DESCRIPTION   : to build the webret.exe executable. webret.exe is 
#               : the MetDB web retrieval executable for HPMDB. It
#               : is called by the CGI script webret.pl
#
# REVISION INFO :
#
# $Revision: 1$
# $Date: 20/09/2006 16:23:38$
# $Source: /home/us0400/mdb/op/lib/web_source/RCS/build_webret_exe.sh,v $
#
# CHANGE RECORD :
#
# $Log:
#  1    Met_DB_Project 1.0         20/09/2006 16:23:38    Stan Kellett    
# $
# Revision 1.3  2002/02/05 09:27:40  usmdb
# Now f90 compiles. New library mdb.a - S.Cox
#
# Revision 1.2  2001/01/18 11:06:10  usmdb
# Will make exe in cgi-bin location - S.Cox
#
# Revision 1.1  2001/01/18  10:52:59  10:52:59  usmdb (Generic MDB account)
# Initial revision
# 
# Written 17-01-2001 S.Cox
#-------------------------------------------------------------------

BASE_DIR=/home/us0400/mdb_new/op
DBASE_DIR=${BASE_DIR}/d_base/processed
DATA_DIR=${BASE_DIR}/data
MDBDIS_DIR=${BASE_DIR}/lib/web_source
EXE_DIR=/home/us0400/httpd/cgi-bin/PortMetDB/web_retrieval

ifc -nus -w -C -o ${EXE_DIR}/webret.exe \
          ${BASE_DIR}/lib/web_source/webret.F \
	  ${BASE_DIR}/lib/web_source/libweb_source.a \
 	  ${BASE_DIR}/lib/source/mdb.a

exit
