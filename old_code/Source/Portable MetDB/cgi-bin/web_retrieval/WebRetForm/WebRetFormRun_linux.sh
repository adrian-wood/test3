#!/bin/sh
# --------------------------------------------------------------------
# Submit jobs to make MetDB web extraction forms
#
# $Revision: 1$
# $Date: 20/09/2006 16:10:05$
# $Source: /home/us0400/mdb/op/lib/web_source/RCS/WebRetFormRun_HP.sh,v $
#
# $Log:
#  1    Met_DB_Project 1.0         20/09/2006 16:10:05    Stan Kellett    
# $
# Revision 1.3  2001/10/05 10:57:58  usmdb
# brought up to date with IBM version - S.Cox
#
# Revision 1.2  2001/01/26 08:42:49  usmdb
# Added cd line for LOOKUP - S.Cox
#
# Revision 1.1  2001/01/18  14:35:43  14:35:43  usmdb (Generic MDB account)
# Initial revision
# 
# --------------------------------------------------------------------

cd /home/us0400/httpd/cgi-bin/PortMetDB/web_retrieval/WebRetForm

ln -s /home/us0400/httpd/cgi-bin/PortMetDB/web_retrieval/WebRetForm/WebRetFormLookup_opnl_linux.txt LOOKUP

WEBDIR=/home/us0400/usmdb/public_html/web_retrieval

rm -f $WEBDIR/mdb_AIREPS.html
rm -f $WEBDIR/mdb_AMDARS.html
rm -f $WEBDIR/mdb_ATAFS.html
rm -f $WEBDIR/mdb_BATHY.html
rm -f $WEBDIR/mdb_BOGUS.html
rm -f $WEBDIR/mdb_BUOYPROF.html
rm -f $WEBDIR/mdb_DRIFTR.html
rm -f $WEBDIR/mdb_DROPSOND.html
rm -f $WEBDIR/mdb_ESAWS.html
rm -f $WEBDIR/mdb_HEALTHRR.html
rm -f $WEBDIR/mdb_LNDSYN.html
rm -f $WEBDIR/mdb_METARS.html
rm -f $WEBDIR/mdb_NCM.html
rm -f $WEBDIR/mdb_PILOT.html
rm -f $WEBDIR/mdb_SAMOSX.html
rm -f $WEBDIR/mdb_SHPSYN.html
rm -f $WEBDIR/mdb_SREWS.html
rm -f $WEBDIR/mdb_TAFS.html
rm -f $WEBDIR/mdb_TBUS.html
rm -f $WEBDIR/mdb_TEMP.html
rm -f $WEBDIR/mdb_TESAC.html
rm -f $WEBDIR/mdb_TROPADV.html

/home/us0400/httpd/cgi-bin/PortMetDB/web_retrieval/WebRetForm/WebRetForm.exe

rm LOOKUP

exit
