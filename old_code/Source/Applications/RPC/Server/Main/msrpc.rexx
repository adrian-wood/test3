/* REXX ------------------------------------------------------------*
 *
 * SYNOPSIS : Run a dedicated MDB server process for MDB client
 * LIBRARY  : MDB.REXX.EXEC(MSRPC)
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
 * $Date: 11/10/2006 11:36:12$
 * $Source: /home/us0400/mdb/op/lib/RPC/server/main/RCS/msrpc.rexx,v $
 *
 * CHANGE RECORD :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:36:12    Kudsia Gwangwaa 
 * $
 * Revision 2.2  2002/09/17 07:58:36  usmdb
 * Changed SYS1.TCPPARMS(TCPDATOP) to SYS1.TCPPARMS(TCPDATA)
 * for new IBM Z800 - P.Nelson
 *
 * Revision 2.1  2002/06/20 09:11:20  usmdb
 * Allocated file CEEDUMP to SYSOUT(Q) - S.Cox
 *
 * Revision 2.0  2001/09/13 13:39:21  usmdb
 * Added code to check for BPRD. Added copyright and modified
 * header - S.Cox
 *
 * Revision 1.5  99/07/20  12:02:25  12:02:25  usmdb (Generic MDB account)
 * Remove OFFLINE SYSOUT allocation - done in MDBREST.
 *
 * Revision 1.4  99/07/16  15:28:59  15:28:59  usmdb (Generic MDB account)
 * Revert back to writing output to SYSOUT. New EJES system (live from
 * 15th July 1999) can display SYSOUT from forked tasks.
 *
 * Revision 1.3  98/05/14  07:58:29  07:58:29  usmdb (Generic MDB account)
 * Correct errors in comments! - S.Cox
 *
 * Revision 1.2  1998/05/13 14:47:31  usmdb
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
say datim 'MDB.REXX.EXEC(MSRPC) starting on PROGNUM =' PROGNUM

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

"call 'sys1.sdbload(msrpc)' '"prognum"'"

datim = DATE('O') TIME('N')
say datim 'MDB.REXX.EXEC(MSRPC) ended, RC =' RC

/*-------------------------------------------------------------------*
 * free the main RPC server output datasets
 *-------------------------------------------------------------------*/

"FREE FI(SYSABEND)"
"FREE FI(SYSPRINT)"
"FREE FI(FT06F001)"
"FREE FI(CEEDUMP)"

Exit 0