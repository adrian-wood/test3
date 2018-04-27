/* REXX ------------------------------------------------------------*
 *
 * SYNOPSIS : Run a dedicated MDB server process for MDB client
 * LIBRARY  : MDB.REXX.EXEC(MSRPCTST)
 * CREATED  :  7 Apr 1997 11:04am Catlow M
 * NOTES    :
 *
 * This exec calls the dedicated MDB RPC server program to service
 * requests from a mdb RPC client.  This process is forked by
 * the /usr/local/mdb/bin/mdb_start_server exec in open edition,
 * and is terminated on receipt of the sigterm from the client.
 *
 * REVISION INFO :
 *
 * $Revision: 1$
 * $Date: 11/10/2006 11:36:13$
 * $Source: /home/us0400/mdb/op/lib/RPC/server/main/RCS/msrpctst.rexx,v $
 *
 * CHANGE RECORD :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:36:13    Kudsia Gwangwaa 
 * $
 * Revision 2.3  2002/11/13 08:59:18  usmdb
 * Changed mcc3.scload to mcc3.dbload.rpc - S.Cox
 *
 * Revision 2.2  2002/09/17 07:58:36  usmdb
 * Changed SYS1.TCPPARMS(TCPDATOP) to SYS1.TCPPARMS(TCPDATA)
 * for new IBM Z800 - P.Nelson
 *
 * Revision 2.1  2002/06/20 09:11:20  usmdb
 * Allocated file CEEDUMP to SYSOUT(Q) - S.Cox
 *
 * Revision 2.0  2001/09/13 13:43:02  usmdb
 * Added code to check for BPRD. Removed OFFLINE SYSOUT allocation
 * as done in MDBREST. Added copyright and modified header - S.Cox
 *
 * Revision 1.5  99/07/15  15:39:22  15:39:22  usmdb (Generic MDB account)
 * Revert back to writing output to SYSOUT. New EJES system (live from
 * 15th July 1999) can display SYSOUT from forked tasks.
 *
 * Revision 1.4  98/07/29  08:28:16  08:28:16  usmdb (Generic MDB account)
 * Submit Test RPC load module rather than operational!
 *
 * Revision 1.3  1998/05/14 08:00:07  usmdb
 * Correct errors in comments! - S.Cox
 *
 * Revision 1.2  1998/05/13 14:37:42  usmdb
 * Allocate datasets instead of SYSOUT for output. This is beacuse
 * the new OS-390 operating system (live from 14th May 1998) will not
 * produce SYSOUT from tasks forked by OE - S.Cox, M.Catlow, R.Cains
 *
 * Revision 1.1  1998/02/03 14:12:16  usmdb
 * Initial revision
 *
 *-------------------------------------------------------------------*
 * (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
 *
 * Met Office, United Kingdom
 *
 * The use, duplication and disclosure of this code is strictly
 * prohibited without the permission of The Meteorological Database
 * Team at the above address.
 *-------------------------------------------------------------------*/

parse upper arg prognum

datim = DATE('O') TIME('N')
say datim 'MDB.REXX.EXEC(MSRPCTST) starting on PROGNUM =' PROGNUM

/*-------------------------------------------------------------------*
 * if IBM R25 (BPRD), allocate TCPIP library.
 *-------------------------------------------------------------------*/

system = MVSVAR('sysname')
if system = BPRD then do
  "ALLOC F(SYSTCPD) DA('SYS1.TCPPARMS(TCPDATA)') REUSE SHR"
end

/*-------------------------------------------------------------------*
 * allocate datasets for main server output
 *-------------------------------------------------------------------*/

"ALLOC FI(SYSABEND) SYSOUT(Q)"
"ALLOC FI(SYSPRINT) SYSOUT(Q)"
"ALLOC FI(FT06F001) SYSOUT(Q)"
"ALLOC FI(CEEDUMP) SYSOUT(Q)"

/*-------------------------------------------------------------------*
 * submit the main RPC server passing the program number as argument
 *-------------------------------------------------------------------*/

"call 'mcc3.dbload.rpc(msrpc)' '"prognum"'"

datim = DATE('O') TIME('N')
say datim 'MDB.REXX.EXEC(MSRPCTST) ended, RC =' RC

/*-------------------------------------------------------------------*
 * free the main RPC server output datasets
 *-------------------------------------------------------------------*/

"FREE FI(SYSABEND)"
"FREE FI(SYSPRINT)"
"FREE FI(FT06F001)"
"FREE FI(CEEDUMP)"

Exit 0
