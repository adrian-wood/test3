/*-------------------------------------------------------------------*
 * Program      : constants.h
 *
 * Language     : C
 *
 * Description  : Include file containing constants used in broker
 *              : and freepn HP server code.
 *
 * Notes        : The values of BASE_PROGNUM and PROGNUM_COUNT will
 *              : be inserted by the build job on the HP.
 *              : These values depend on whether we are building the
 *              : operational or test broker server.
 *
 * $Revision: 2$
 * $Date: 11/11/2009 09:23:38$
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/constants.h,v $
 *
 * Changes      :
 *
 * $Log:
 *  2    Met_DB_Project 1.1         11/11/2009 09:23:38    Sheila Needham
 *       Added comments
 *  1    Met_DB_Project 1.0         09/11/2009 12:43:03    Sheila Needham
 *       Copie to constants.h as part of build
 * $
 * Revision 2.0  2001/09/13 14:41:52  usmdb
 * Added BPRD section. Added copyright - S.Cox
 *
 * Revision 1.3  2001/01/10  12:28:40  12:28:40  usmdb (Generic MDB account)
 * Changed locations for PROGNUM_PATH_PREFIX and
 * DIAG_PATH_PREFIX - S.Cox
 *
 * Revision 1.2  2001/01/09  16:38:39  16:38:39  usmdb (Generic MDB account)
 * Added HP-UX constants. IBM and HPUX constants
 * distinguished by preprocessor statements - S.Cox
 *
 * Revision 1.1  98/02/03  09:58:55  09:58:55  usmdb (Generic MDB account)
 * Initial revision
 *
 * 02-02-1998   : Written - S.Cox
 *
 *-------------------------------------------------------------------
 * (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
 *
 * Met Office, United Kingdom
 *
 * The use, duplication and disclosure of this code is strictly
 * prohibited without the permission of The Meteorological Database
 * Team at the above address.
 *-------------------------------------------------------------------*/

#if defined (HPUX)
#define MSRPC_BROKER_SERVER "us0400"                        /* RPC broker server machine */
#define MSRPC_MAIN_SERVER "us0400"                          /* RPC main server machine */
#define TRANSPORT "tcp"                                     /* RPC transport protocol */
#define MYTIMEOUT 10000                                     /* RPC timeout limit (secs) */
#define BASE_PROGNUM XXXXXX                                 /* Base prognum for server */
#define PROGNUM_COUNT YY                                    /* No. of prognum files    */
#define KILL_AFTER_HOURS_OLD 6                              /* server can be killed if */
#define PROGNUM_PATH_PREFIX "./prognum_files"               /* prognum files path */
#define DIAG_PATH_PREFIX "./logs"                           /* diagnostic output files path */
#define DIR "."                                             /* server directory */
#elif defined (BPRD)
#define MSRPC_BROKER_SERVER "bprd"                          /* RPC broker server machine */
#define MSRPC_MAIN_SERVER "bprd"                            /* RPC main server machine */
#define TRANSPORT "tcp"                                     /* RPC transport protocol */
#define MYTIMEOUT 10000                                     /* RPC timeout limit (secs) */
#define BASE_PROGNUM XXXXXX                                 /* Base prognum for server */
#define PROGNUM_COUNT YY                                    /* No. of prognum files    */
#define KILL_AFTER_HOURS_OLD 6                              /* server can be killed if */
#define PROGNUM_PATH_PREFIX "/usr/local/mdb/rpc/bprd/UUUU/prognum"   /* prognum files path */
#define DIAG_PATH_PREFIX "/tmp/mdb"                         /* diagnostic output files path */
#define DIR "/usr/local/mdb/rpc/bprd/DDDD/bin"              /* server directory */
#else
#define MSRPC_BROKER_SERVER "ukmet"                         /* RPC broker server machine */
#define MSRPC_MAIN_SERVER "ukmet"                           /* RPC main server machine */
#define TRANSPORT "tcp"                                     /* RPC transport protocol */
#define MYTIMEOUT 10000                                     /* RPC timeout limit (secs) */
#define BASE_PROGNUM XXXXXX                                 /* Base prognum for server */
#define PROGNUM_COUNT YY                                    /* No. of prognum files    */
#define KILL_AFTER_HOURS_OLD 6                              /* server can be killed if */
#define PROGNUM_PATH_PREFIX "/usr/local/mdb/rpc/prod/UUUU/prognum"   /* prognum files path */
#define DIAG_PATH_PREFIX "/tmp/mdb"                         /* diagnostic output files path */
#define DIR "/usr/local/mdb/rpc/prod/DDDD/bin"              /* server directory */ 
#endif
