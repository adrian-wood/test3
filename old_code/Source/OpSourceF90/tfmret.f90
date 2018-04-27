 SUBROUTINE TFMRET(CTYPE,ITIME,TRANGE,IRTIM,AREA,CIDENT,&       !2.0
 &ARRAY,NOBS,NELEM,IOBS,IDESC,NDES,&                            !2.0
 &ISTAT,IERR,IDSK,CSTR,CREPRT,LFLAG,&                           !2.0
 &CHAREP,ORDER,LATEST,TAFLEN,METSPECI,&                       !1.18!H!G
 &FOUND,RPOLE,IVER)                                            !G

!-----------------------------------------------------------------------
!
! SUBROUTINE    : TFMRET
!
!               : ANSI standard except for '!' used for comments,
!                 variable name lengths greater than 6 characters
!
! PURPOSE       : RETRIEVAL ROUTINE FOR CHAINED 23 BYTE INDEXES
!
! DESCRIPTION   : LOOP OVER INDEX BLOCKS COVERING REQUESTED PERIOD
!                   LOOP OVER INDEX ENTRIES IN ONE BLOCK
!                     LOOP OVER REPORTS IN CHAIN
!                       CALL EXPANSION
!                       TRANSFER TO USERS ARRAY
!
! DATA TYPE(S)  : METARS, TAFS, SAMOSX, NCM, SREW, TBUS, ATAFS, CLIMAT
!
! CALLED BY     : MDB
!
! CALLS         : DT2HRS (FUNCTION), HRS2DT
!               : MAPRD
!               : IDMTCH,SUBPER
!               : SETHED
!               : MTRINT
!               : TAFINT
!               : NCMINT
!               : SRWINT
!               : MTREXP
!               : TAFEXP
!               : NCMEXP
!               : SRWEXP
!               : CONDES
!               : EXPREQ (FUNCTION)
!               : TRNSFR
!               : EOCHN (FUNCTION DEFINED AT END OF THIS MEMBER)
!               : SORTCH
!
! PARAMETERS    :
!  ALL THESE PARAMETERS HAVE BEEN VALIDATED IN MDB SO INVALID
!  COMBINATIONS WILL NOT OCCUR.
!      CTYPE    CHAR*8      DATA SUBTYPE
!      ITIME(9) INTEGER (1) YYYY    START DATE  (MUST BE SET BY NOW)
!                       (2) MM
!                       (3) DD
!                       (4) HHMM
!                       (5) YYYY     END DATE   (MUST BE SET BY NOW)
!                       (6) MM
!                       (7) DD
!                       (8) HHMM
!                       (9) INCREMENT           (DEFAULT IS 1 HOUR)
!      IRTIM(10) INTEGER (1) YYYY     EARLIEST T.O.R
!                       (2) MM
!                       (3) DD
!                       (4) HH
!                       (5) MIN
!                       (6) YYYY     LATEST T.O.R
!                       (7) MM
!                       (8) DD
!                       (9) HH
!                       (10) MIN
!      TRANGE   INTEGER TIME RANGE IN MINUTES IF INCREMENT REQUIRED
!      ARRAY(NOBS,NELEM)   REAL USERS DATA ARRAY
!      NOBS     INTEGER    NUMBER OF OBSERVATIONS
!      NELEM    INTEGER    NUMBER OF ELEMENTS
!      IOBS     INTEGER    NUMBER OF NEXT OBSERVATION TO BE PUT IN
!                          ARRAY ON ENTRY. LAST OB ON EXIT.
!      IDESC(NDES)  INTEGER LIST OF ELEMENT SUBSCRIPTS
!      NDES     INTEGER    NUMBER OF SUBSCRIPTS (NOT DESCRIPTORS!)
!      AREA(5)  REAL    (1) 0.0 FOR LAT/LON AREA -2.0 FOR GLOBAL      !G
!                       (2) TOP LH LAT
!                       (3) TOP LH LON
!                       (4) BOTTOM RH LAT
!                       (5) BOTTOM RH LON
!      CIDENT(50) CHARACTER*9 LIST OF IDENTIFIERS WITH '0' AT END OF
!                       LIST.  CIDENT(1)='0' FOR ALL DATA.
!      ISTAT    INTEGER RETURNS 4 FOR MORE DATA TO COME
!           IF RESET BY THE USER- ASSUMES A NEW REQUEST
!      IERR     INTEGER  ERROR INDICATOR
!      IDSK(5)  INTEGER  (2) RECORD LENGTH
!                        (3) FT NUMBER
!                        (5) 1 FOR DISK, 2 FOR TAPE
!      CSTR     CHARACTER(NOBS) ARRAY FOR CHARACTER ELEMENTS
!      CREPRT   CHARACTER(NOBS) ARRAY FOR RAW REPORTS (+ HEADER)
!      LFLAG    LOGICAL  TRUE FOR DIAGNOSTIC OUTPUT
!      CHAREP   LOGICAL  TRUE FOR CHARACTER REPORT ONLY
!      ORDER    CHAR*1   'F'ORWARDS OR 'B'ACKWARDS IN TIME
!      LATEST   LOGICAL  TRUE FOR LATEST REPORTS ONLY
!      TAFLEN   INTEGER  1 FOR LONG, 2 FOR SHORT TAF, 0 FOR BOTH   !1.18
!      METSPECI INTEGER  0 FOR METAR, 3 FOR SPECI                  !1.18
!      FOUND    LOGICAL(*) KEYWORDS FOUND OR NOT
!      RPOLE(2) REAL       Rotated pole lat/lon                       !G
!      IVER     INTEGER    1 for preferred, 2 for all                 !H
!
! REVISION INFO :
!
! $Workfile: tfmret.f90$ $Folder: OpSourceF90$
! $Revision: 1$ $Date: 26/01/2010 10:18:13$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.3  2002/10/07  15:10:22  15:10:22  usmdb (Generic MetDB acc
! 21 Oct 2002
! 2.3  Don't reject report when TAFEXP gives a nonzero return code.
!
! Revision 2.2  2002/03/07  15:55:04  15:55:04  usmdb (Generic MetDB acc
! 18 March 2002    C Long
! 2.2  Correct the January change.  For NCM & SREWs some weeding is done
!      as the chain is read in, so when an ob is skipped the pointer kep
!      in advance must be overwritten.
!
! Revision 2.1  2002/01/15  15:14:30  15:14:30  usmdb (Generic MetDB acc
! 21 Jan 2002     C Long
! 2.1  Instead of keeping obs in large array of long strings, keep
!      only the trailers (pointing to this ob rather than the next)
!      so that the string length or dimension can be increased
!      without a big space oevrhead.
!       Change misleading comments about >1 ob in a BUFR message:
!      TFMRET doesn't handle BUFR messages!  But leave code as in
!      other retrieval routines.  (N.B. LMIDMSG is still needed
!      in case a single ob has to be returned next time.)
!
! Revision 2.0  2001/01/08  11:59:20  11:59:20  usmdb (Generic MetDB acc
! Removed argument CERR from the calls to MTREXP and TAFEXP.
! Removed unused variables and dummy arguments. Moved declaration
! of NDES before declaration of IDESC(NDES). Always pass KMSG
! rather than CMSG to MAPRD. Changed calls to IDMTCH and
! SUBPER - they now return a return code rather than making
! use of the obsolete ALTERNATE RETURN feature. Added copyright
! and modified header - S.Cox
!
! Revision 1.18  2000/12/08  14:23:48  14:23:48  usmdb (Generic MDB acco
! Change day 18DEC00    R Hirst
! Enable retrieval of SPECIs (from the METAR datastore)
! and change variable name TLEN to TAFLEN.
!
!
! Revision 1.17  2000/03/31  10:50:30  10:50:30  usmdb (Generic MDB acco
! Change day: 3 March 2000
! Initialise KEPTIM to -9999999 instead of 0. This prevents
! SREW reports at 00Z being rejected as duplicates! - S.Cox
!
! Revision 1.16  2000/03/10  09:44:11  09:44:11  usmdb (Generic MDB acco
! 20 March 2000     C Long
! 1.16  In the check to make sure there's only one report returned for
!       a given time (unless VERSION ALL) use report time, not T.O.R.!
!
! Revision 1.15  99/02/11  12:24:20  12:24:20  usmdb (Generic MDB accoun
! 15-02-1999 S.Cox, J.Norton
! a) Check on the Max chain length. If the message number in the chain e
!    the max allowed, issue a warning and stop reading the chain. - S.Co
! b) Add subtype to NO DATA FOR THIS PERIOD warning message - S.Cox
! c) Extra argument passed to SRWEXP - J.Norton
! v(G)=170, ev(G)=52
!
! Revision 1.14  99/01/14  14:18:00  14:18:00  usmdb (Generic MDB accoun
! 18-01-99 S.Cox, C.Long, J.Norton  - Ref problems 314, 220
! a) Allow VERSION ALL retrieval of SREW reports - S.Cox
! b) Correct setting of data time for TROPADV retrieval - S.Cox
! c) Fudge for retrieval of old METAR data - see comments in code - C.Lo
! v(G) = 170, ev(G) = 53
!
! Revision 1.13  98/10/15  11:39:15  11:39:15  usmdb (Generic MDB accoun
! 19-10-98  S.Cox  Ref. Problem 268
! a) CLIMAT storage has been changed to use TAFREP. CLIMAT retrieval
!    is now through TFMRET rather than BUSRET. TFMRET has been changed
!    to allow CLIMAT retrieval. It is now possible for obs to be chained
!    in different data blocks.
! b) Addition of subtype ATAFS for Automatic TAFS retrieval
! c) Don't convert negative TORs for CLIMAT data - TOR can be very large
!    offset for this subtype.
! v(G)=167, ev(G)=53
!
! Revision 1.12  98/09/02  15:06:08  15:06:08  usmdb (Generic MDB accoun
! 21-09-98  S.Cox  Ref. Problem 144 (a), 186 (b)
! a) Correct setting of IOBS if a retrieval spans more than one
!    dataset.
! b) Initialise variable KEPTIM for each new chain of obs.
!
! Revision 1.11  98/08/21  10:09:49  10:09:49  usmdb (Generic MDB accoun
! Increase max length of report to 8500 for TROPADV
!
! Revision 1.10  98/08/17  09:25:12  09:25:12  usmdb (Generic MDB accoun
! Addition of routine TRPINT for TROPADV retrieval.
! Increase Max report length to 5000 for TROPADV
! retrieval.
!
! Revision 1.9  98/06/11  13:40:14  13:40:14  usmdb (Generic MDB account
! Addition of routine TRPINT for TROPADV retrieval.
! Increase Max report length to 5000 for TROPADV
!
! Revision 1.8  98/05/14  16:01:12  16:01:12  usmdb (Generic MDB account
! Change loop over reports in chain. Change from loop over number
! of entries in chain to a DO WHILE until reached end of chain
! (next block/record pointers = 0)
!
! Revision 1.7  98/04/20  08:13:25  08:13:25  usmdb (Generic MDB account
!  Make preferred reports for NCMs default option.
!
! Revision 1.6  98/03/12  09:32:45  09:32:45  usmdb (Generic MDB account
! Allow retieval using preferred report flag options of NCM
! reports. IMPLICIT NONE added and each variable declared
! on seperate line.
!
! Revision 1.5  1997/08/04 13:36:17  uspm
! First revisioned version for  1  - with Y2K change
!
! Revision 1.4  1997/04/18 15:25:03  uspm
! Latest version dated 18-4-97 from COSMOS
!
! Revision 1.3  1997/02/27 12:17:29  uspm
! Latest version from COSMOS
!
! Revision 1.2  1997/02/17 09:24:03  uspm
! Remove definition of function eochn from
! modules as it is in a separate module.
!
! Revision 1.1  1997/02/17 09:16:27  uspm
! Initial revision
!
! 15-06-1998  !L : Addition of routine TRPINT for TROPADV retrieval.
!                : Increase Max report length to 5000 for TROPADV
!                : retrieval - J.Lewthwaite/S.Cox
!
! 18-05-1998  !K : Change loop over reports in chain. Change from
!                : loop over number of entries in chain to a DO WHILE
!                : until reached end of chain (next block/record
!                : pointers = 0) - S.Cox
!
! 20-04-98 !j  Make preferred reports for NCMs default option.
!                  J Norton.
!
! 16-03-98 !i  IMPLICIT NONE added and each variable declared on
!              seperate line. Definitions added where known. Output of
!              test variables changed where it causes printer jams.
!                  J Norton.
!
! 16-03-98 !H  Allow retieval using preferred report flag options
!              of NCM reports. J Norton.
!
! 28-07-97 !G  Integer IAREA changed to Real AREA for the rotated
!              lat/lon change. Call to ARMTCH replaced with a call to
!              VALAREA. Dynamic common names changed to make them
!              less contentious - S.Cox
!
! 18-04-97 !F  Increase declaration of CREP from 1000 to 2500 to cope
!              with subtype TBUS - S.Cox
!
! 26-02-97 !E  Change the users RECEIEVED BEFORE check slightly - S.Cox
!
! MAR 96       REPLACE CALLS TO MAPRD, IDXRD AND MSGRD WITH FOUR      !D
!              CALLS TO MAPRD WITH LONGER ARGUMENT LIST               !D
! FEB 96       ADD NEW DATA TYPES NCM AND SREW AND MODIFY PARAMETERS  !C
!              IN CALL TO TAFINT AND TAFEXP.                          !C
!              MOVE CALL TO EXPREQ OUTSIDE OF MAIN LOOP TO REMOVE     !C
!              UNNECESSARY REPETITIVE CHECKING AND CHANGE PARAMETERS  !C
!              TO ALLOW ALL DATA TYPES VIA THIS ROUTINE TO BE CHECKED.!C
!              IMPROVE DOCUMENTATION.
! DEC 95       SELECT INDEX ENTRIES BEFORE SORTING (FOR EFFICIENCY);  !B
!              GET LAT/LONG HERE INSTEAD OF FROM ARMTCH (OMIT ARGS);  !B
!              IF ONLY ONE IDENT IS WANTED, STOP SEARCH WHEN FOUND;   !B
!              & CORRECT TEST FOR INCREMENT IN REQUEST                !B
! 01/12/95     CHANGE NAME FROM TFMTRT TO TFMRET TO BE SAME AS SHELL.
!              USE SORTCH FOR INDEX SORT (NOT JSL82 - ASSEMBLER!),    !A
!              SORTING ON IDENT & INDEX BLOCK NUMBER IF LATEST -      !A
!              TFMTRT SET BLOCK NUMBER BUT SORTED ON FLAGS (NEVER SET!)
! 18/04/94     CHANGE TO THE ARGUMENT LIST OF MAPRD TO MAKE MAPRD MORE
!              FLEXIBLE. CALCULATION OF MESSAGE LENGTHS AND EXTRACTION
!              OF TAFS/METARS FROM MESSAGE IS NOW PERFORMED AT THIS
!              LEVEL (TFMRET) RATHER FROM WITHIN MAPRD.
! 21/03/94     ADDITIONAL ARGUMENTS (LOCDFG AND INDLEN) ADDED TO MAPRD
!              INTERFACE. LOCDFG IS A FLAG WHICH INDICATES A LOCAL
!              TABLE D ENTRY; INDLEN IS THE LENGTH OF THE INDEX ENTRIES
!              WHICH ENABLES MAPRD TO HANDLE VARIABLE INDEX LENGTHS.
! 07/03/94     CORRECT RETRIEVAL OF NON-MET ELEMENTS BY MAKING SURE
!              IMAX IS INITIALISED WHEN NOT CALLING MTREXP
! 24/01/94     START TIME CAN BE COMBINED WITH A SUB-PERIOD AND
!              INCREMENT E.G.
!              START TIME TODAY-3/08050Z-0910Z END TIME TODAY/0910Z
!              INCREMENT 3 FOR 3 HOURLY REPORTS ON THE HOUR (NOMINAL)
! 18/01/94     INCREASE DIMENSION OF CNTRY ARRAY TO COPE WITH POSSIBLE
!              OVERFLOWS.
! 26/07/93     RE-INTRODUCE CHANGE MADE ON 15/06/93 !
! 24/06/93     INCREASE SIZE OF TAF EXPANSION ARRAY
! 15/06/93     FORCE A CHECK ON IDENTIFIER FOR LATEST REPORTS ON
!              RE-ENTRY AFTER A FULL ARRAY.
! 10/06/93     CORRECT TOR FOR LATEST REPORTS
! 25/05/93     PUT ALL I/O INTO A SEPARATE MODULE
! 21/05/93     CHECK FOR LONG OR SHORT TAFS.
! 21/05/93     INTRODUCE FORWARDS/BACKWARDS AND CORRECT LATEST IN A
!              TIME RANGE.
! 21/05/93     OPTIMISE WHEN EXPANSION IS DONE BY CHECKING IF ONLY
!              ELEMENTS FROM INDEX OR REPORT TEXT ARE REQUIRED.
! 22/04/93     ALLOW RETRIEVAL TO THE MINUTE
! 20/04/93     INTRODUCE TAF EXPANSION
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE                                                  !i

INTEGER        MDES         !                                  !i
INTEGER        MDATA        !                                  !i
INTEGER        INDLEN       !                                  !i
INTEGER        IHDLEN       !                                  !i
INTEGER        MXINDX       !                                  !i
INTEGER        MXCHLEN      !                                  !i
INTEGER        MXRPLEN      !                                  !i

!-----------------------------------------------------------------------
! Parameter statements
!-----------------------------------------------------------------------

PARAMETER      (MDES=500)                                      !i
PARAMETER      (MDATA=1000)                                    !i
PARAMETER      (INDLEN=23)                                     !i
PARAMETER      (IHDLEN=44)                                     !i
PARAMETER      (MXINDX=1020)                                   !i
PARAMETER      (MXCHLEN=100)                                 !2.1
PARAMETER      (MXRPLEN=27990)                               !2.1

!-----------------------------------------------------------------------
! Declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

INTEGER        NDES         !                                  !i

INTEGER block_in_first_pointer                              !1.14c
INTEGER        I            !                                  !i
INTEGER        IBLOCK       !                                  !i
INTEGER        ICHL1        !                                  !i
INTEGER        ICHL2        !                                  !i
INTEGER        ICHL3        !                                  !i
INTEGER        ICMIN        !                                  !i
INTEGER        ID           !                                  !i
INTEGER        ID2          ! day (end of index block)      !1.15b
INTEGER        IDAT         !                                  !i
INTEGER        IDATHR       !                                  !i
INTEGER        IDAY         !                                  !i
INTEGER        IDESC(NDES)  !                                  !i
INTEGER        IDHR         !                                  !i
INTEGER        IDISP(MDES)  !                                  !i
INTEGER        IDSK(5)      !                                  !i
INTEGER        IDREC        !                                  !i
INTEGER        IENT1        !                                  !i
INTEGER        IERR         !                                  !i
INTEGER        IFAIL        !                                  !i
INTEGER        IH           !                                  !i
INTEGER        IH2          ! hour (end of index block)     !1.15b
INTEGER        IHOUR        !                                  !i
INTEGER        II           !                                  !i
INTEGER        IHL          !                                  !i
INTEGER        IM           !                                  !i
INTEGER        IM2          ! month (end of index block)    !1.15b
INTEGER        IMAX         !                                  !i
INTEGER        IMD          !                                  !i
INTEGER        IMIN         !                                  !i
INTEGER        INCR         !                                  !i
INTEGER        INDHR1       !                                  !i
INTEGER        INDHR2       !                                  !i
INTEGER        INUM         !                                  !i
INTEGER        INXBLK       !                                  !i
INTEGER        INXHRS       !                                  !i
INTEGER        IOB          !                                  !i
INTEGER        IOBS         !                                  !i
INTEGER        IRCHR        !                                  !i
INTEGER        IRECLN       !                                  !i
INTEGER        IREP1        !                                  !i
INTEGER        IRLEN        !                                  !i
INTEGER        IRTIM1       ! century mins of T.O.R start time !i
INTEGER        IRTIM2       ! century mins of T.O.R end time   !i
INTEGER        ISECT1(9)    !                                  !i
INTEGER        ISHR1        !                                  !i
INTEGER        ISHR2        !                                  !i
INTEGER        ISLOTHR      !                                  !i
INTEGER        ISTAT        !                                  !i
INTEGER        ISTEP        !                                  !i
INTEGER        IRTIM(10)    !                                  !i
INTEGER        ITHR(3)      !                                  !i
INTEGER        ITIME(9)     !                                  !i
INTEGER        ITIMND       ! century mins of end time         !i
INTEGER        ITIMST       ! century mins of start time       !i
INTEGER        ITIM1        ! century hour of start time       !i
INTEGER        ITIM2        ! century hour of end time         !i
INTEGER        ITORM        !                                  !i
INTEGER        ITRIES       !                                  !i
INTEGER        IVER         !                                  !i
INTEGER        IY           !                                  !i
INTEGER        IY2          ! year (end of index block)     !1.15b
INTEGER        J1           !                                  !i
INTEGER        J2           !                                  !i
INTEGER        J3           !                                  !i
INTEGER        J5           !                                  !i
INTEGER        K            !                                  !i
INTEGER        K1           !                                  !i
INTEGER        K2           !                                  !i
INTEGER        KEPTIM       !                                  !i
INTEGER        KOUNT        !                                  !i
INTEGER        KREP         !- count of reports                !H
INTEGER        LAT          !                                  !i
INTEGER        LON          !                                  !i
INTEGER        NAC          !                                  !i
INTEGER        NAMD         !                                  !i
INTEGER        NBLOCK       !                                  !i
INTEGER        NB2RD        !                                  !i
INTEGER        NCHN         !                                  !i
INTEGER        NCOR         !                                  !i
INTEGER        NELEM        !                                  !i
INTEGER        NELMIX       !                                  !i
INTEGER        NLAT         !- index entry latitude           !Gi
INTEGER        NLON         !- index entry longitude          !Gi
INTEGER        NOBS         !                                  !i
INTEGER        NREC         !                                  !i
INTEGER        NREP         !                                  !i
INTEGER        NSEND        !                                  !i
INTEGER        NSQ          !- 1 or 0 if Local Desc or not  !1.13a
INTEGER        NTOR         !                                  !i
INTEGER        NXTCHN       !                                  !i
INTEGER        NXTENT       !                                  !i
INTEGER        NXTIME       !                                  !i
INTEGER        NXTREP       !                                  !i
INTEGER        RC           !- general return code            !2.0
INTEGER record_in_first_pointer                             !1.14c
INTEGER        TAFTYP       !                                  !i
INTEGER        TAFLEN       !                            !1.18 !i
INTEGER        METTYP       !                               !1.18
INTEGER        METSPECI     !                               !1.18
INTEGER        TRANGE       !                                  !i

!-----------------------------------------------------------------------
! Declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

LOGICAL  AREA_FLAG     !- TRUE if ob in users area             !Gi
LOGICAL  CHANRED                                               !i
LOGICAL  CHAREP                                                !i
LOGICAL dubious_chains                                      !1.14c
LOGICAL  EXPAND                                                !i
LOGICAL  FOUND(*)                                              !i
LOGICAL  INDXRED                                               !i
LOGICAL  ISPREF        !- TRUE if preferred flag is set        !Hi
LOGICAL  LATEST                                                !i
LOGICAL  LCLIM         !- TRUE if CLIMAT data wanted        !1.13a
LOGICAL  LCONT                                                 !i
LOGICAL  LDONE                                                 !i
LOGICAL  LFLAG                                                 !i
LOGICAL  LMIDMSG       ! set if no room for this ob in array !2.1
LOGICAL  LOCDFG        !- local table D flag                   !i
LOGICAL  MSGSNT                                                !i
LOGICAL  NTRYCHK                                               !i
LOGICAL  PASSED                                                !i
LOGICAL  READ_CHNREPS  !- TRUE if still chain reports to read   !K
LOGICAL  SKIP          ! set if ob skipped in reading chain  !2.2
LOGICAL  WASPREF       !- TRUE if report was once preferred    !Hi

!-----------------------------------------------------------------------
! Declare real variables (in alphabetical order)
!-----------------------------------------------------------------------

REAL        AREA(5)        !- user-defined area                !Gi
REAL        ARRAY(NOBS,NELEM) !- users array                   !i
REAL        RLAT           !- index entry latitude (real)      !Gi
REAL        RLON           !- index entry longitude (real)     !Gi
REAL        ROT_LAT        !- rotated lat                      !Gi
REAL        ROT_LON        !- rotated lon                      !Gi
REAL        RPOLE(2)       !- rotated pole lat/lon             !Gi
REAL        VALUES(MDATA)  !- DEBUFR values array              !i

!-----------------------------------------------------------------------
! Declare character variables (in alphabetical order)
!-----------------------------------------------------------------------
                                                                     !i
CHARACTER*4           BULCOD                                   !i
CHARACTER*4           CCCC                                     !i
CHARACTER*9           CIDENT(50)                               !i
CHARACTER*(MXRPLEN)   KMSG                                   !2.1
CHARACTER             CNAM*20000                               !i
CHARACTER*(INDLEN)    CNTRY(5*3*MXINDX) ! index entries wanted !i
CHARACTER*(MXRPLEN)   CREP                                   !2.1
CHARACTER*(*)         CREPRT(NOBS)                             !i
CHARACTER*(*)         CSTR(NOBS)                               !i
CHARACTER*5           CTAF(2)                                  !i
CHARACTER*(INDLEN)    CTRAIL(MXCHLEN)                          !i
CHARACTER*(INDLEN)    TRAILX          ! copy of ctrail         !hi
CHARACTER*(INDLEN)    CTRAN(5*MXINDX) ! index entries read in  !i
CHARACTER*(*)         CTYPE                                    !i
CHARACTER*80          HEAD                                     !4
CHARACTER*(IHDLEN)    HEADER                                   !i
CHARACTER*9           LASTID                                   !i
CHARACTER*23          MASK             !- for index entry sort !Ai
CHARACTER*1           ORDER                                    !i
CHARACTER*9           THISID                                   !i

!-----------------------------------------------------------------------
! Declare functions
!-----------------------------------------------------------------------

INTEGER   DT2HRS      !- Function to return century hour
LOGICAL   EOCHN
LOGICAL   EXPREQ

!-----------------------------------------------------------------------
! Dynamic common, compile on IBM mainframe with FARMS='DC(*)'
!-----------------------------------------------------------------------

COMMON /TFMCOM1/ CNAM,KMSG,CREP,CNTRY,CTRAIL,CTRAN           !2.1
COMMON /TFMCOM2/ IDISP,VALUES                                !2.1

!-----------------------------------------------------------------------
! Ensures that contents of variables/arrays are still available on
! re-entry
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! Data statements.
!-----------------------------------------------------------------------

DATA CTAF   /'LONG ','SHORT'/
DATA HEADER /'0000Z DD/MM/YY LA.TTN LON.TTW CCCC TORM A C '/
DATA MASK   /'  XXXXXXXXX    '/    !- mask for ident sort       !A

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

HEAD = '$Workfile: tfmret.f90$ ' //&
      &'$Revision: 1$ $Date: 26/01/2010 10:18:13$ '

EXPAND=.TRUE.
IERR=0
II=0
IOB=IOBS-1
IRLEN=IDSK(2)

if (lflag) write(*,*)'in tfmret: record length = ',irlen

!-----------------------------------------------------------------------
! Check the number of elements selected by the user to establish
! whether the user may only be requesting data available from the index.
! The function EXPREQ will return a true value if all the user's      '
! elements can be obtained from the index and trailer.
!-----------------------------------------------------------------------

IF (CTYPE(1:4).EQ.'TAFS' .OR. CTYPE(1:5).EQ.'ATAFS') THEN   !1.13b
  IF (NDES .LE. 18) THEN
    EXPAND=EXPREQ(IDESC,NDES)
  ENDIF
ELSEIF (CTYPE(1:6) .EQ. 'METARS') THEN
  IF (NDES .LE. 17) THEN
    EXPAND=EXPREQ(IDESC,NDES)
  ENDIF
ELSEIF (CTYPE(1:4) .EQ. 'SREW' .OR. CTYPE(1:3) .EQ. 'NCM') THEN
  IF (NDES .LE. 16) THEN
    EXPAND=EXPREQ(IDESC,NDES)
  ENDIF
ENDIF

LCLIM = (CTYPE(1:6).EQ.'CLIMAT')                            !1.13a

!-----------------------------------------------------------------------
! check for continuation and set up loop counters
!-----------------------------------------------------------------------

IF (ISTAT.EQ.16) THEN
  if (lflag) write(*,*)'in tfmret: new dataset'
  ISTAT = 0
ELSE IF (ISTAT.EQ.4) THEN
  if (lflag) write(*,*)'in tfmret: continuation'

!-----------------------------------------------------------------------
! initialise loop counters
!-----------------------------------------------------------------------

  ISHR1=NXTIME     !- start time
  IENT1=NXTENT     !- re-start entry number
  IREP1=NXTREP     !- re-start report number
  ICHL1=NXTCHN     !- re-start point in chained reports
  NSEND=NXTREP-1
  LCONT=.TRUE.     !- this is a continuation

  if (lflag) write(6,9001)ishr1,ient1,irep1,ichl1,nsend       !2.0
ENDIF

IF (ISTAT.EQ.0) THEN
  if (lflag) then
    write(6,9002)(idesc(k),k=1,ndes)
    print*,'request area:',area                                 !g
    print*,'request  ids:',cident
    print*,'request  time',itime
    print*,'taf type',taflen                                 !1.18
    print*,'options (l,ord,raw)',latest,order,charep
  endif
  IF (NELEM.LT.NDES) THEN
    IERR=16
    WRITE(6,9012) NOBS,NDES
    GOTO 999
  ENDIF

!-----------------------------------------------------------------------
! Read map block to find dataset details
! (Assumes map is in record 1 even if data set has >12000 records)    !4
!-----------------------------------------------------------------------

  CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,LOCDFG,&
            &NELMIX, IBLOCK, INUM, CTRAN, IDATHR, IDREC,&       !D
            &IRECLN, KMSG, 'MAPR1')                             !4

  if (lflag) write(*,*)'in tfmret:  locdfg = ',locdfg       !1.13a

  IF (LOCDFG) THEN                                          !1.13a
    NSQ=1                                                   !1.13a
  ELSE                                                      !1.13a
    NSQ=0                                                   !1.13a
  ENDIF                                                     !1.13a

!-----------------------------------------------------------------------
! process request times
!-----------------------------------------------------------------------

  ITIM1  = DT2HRS(ITIME(1),ITIME(2),ITIME(3),(ITIME(4)/100))
  ITIM2  = DT2HRS(ITIME(5),ITIME(6),ITIME(7),(ITIME(8)/100))
  INCR   = ITIME(9)

!-----------------------------------------------------------------------
! earliest and latest required time in century minutes
!-----------------------------------------------------------------------

  ITIMST = ITIM1*60 + MOD(ITIME(4),100)
  ITIMND = ITIM2*60 + MOD(ITIME(8),100)

  if (lflag) then
    write(6,9003)itimst,itimnd
    write(6,9003)itim1,itim2
  endif

!-----------------------------------------------------------------------
! adjust times so that they fall on block boundaries.
! indhr is offset of requested time in an index block
!-----------------------------------------------------------------------

  INDHR1  = MOD(ITIME(4)/100+ISLOTHR,INXHRS)
  INDHR2  = MOD(ITIME(8)/100+ISLOTHR,INXHRS)

!-----------------------------------------------------------------------
! base hours of blocks for start and end of request
!-----------------------------------------------------------------------

  ISHR1   = ITIM1-INDHR1
  ISHR2   = ITIM2-INDHR2

!-----------------------------------------------------------------------
! base 'hours' need to be adjusted if climat data is requested
! since this data is stored by the month rather than the hour
!-----------------------------------------------------------------------

  IF (LCLIM) THEN                                           !1.13a
    ISHR1=ISHR1-24                                          !1.13a
    ISHR2=ISHR2-24                                          !1.13a
    INDHR1=0                                                !1.13a
    INDHR2=0                                                !1.13a
  ENDIF                                                     !1.13a

!-----------------------------------------------------------------------
! if latest is requested, then look back through a max of 3 indexes
! from the end of the request.  print a warning if the user is
! trying to look through too long a period.
!-----------------------------------------------------------------------

  IF (LATEST) THEN
    IF (FOUND(1)) THEN ! TIME RANGE GIVEN

!-----------------------------------------------------------------------
! number of blocks needed
!-----------------------------------------------------------------------

      NB2RD = (ISHR2-ISHR1)/INXHRS + 1
      if (lflag) print*,' number of blocks needed ',nb2rd
      IF (NB2RD.GT.3) THEN
        WRITE(6,9004) INXHRS*3,(ITIME(K),K=5,8)
        NB2RD = 3
      ENDIF
    ELSE
      NB2RD = 3
    ENDIF

!-----------------------------------------------------------------------
! set start to end time so that there's only one loop over hours
!-----------------------------------------------------------------------

    ISHR1 = ISHR2
  ENDIF

!-----------------------------------------------------------------------
! decide on looping order (doesn't matter for latest because indexes
! are sorted anyway)
!-----------------------------------------------------------------------

  IF (ORDER.EQ.'B') THEN
    I     = ISHR1
    ISHR1 = ISHR2
    ISHR2 = I
    ISTEP = -INXHRS
  ELSE
    ISTEP = INXHRS
  ENDIF

  if (lflag) write(6,9005)inxblk,inxhrs,islothr,indhr1,&
                  &indhr2,ishr1,ishr2,istep

!-----------------------------------------------------------------------
! convert requested time of receipt to century minutes
!-----------------------------------------------------------------------

  IF (IRTIM(1).EQ.0) THEN
    IRTIM1 = 0
  ELSE
    IRTIM1 = DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4))
    IRTIM1 = (IRTIM1)*60+IRTIM(5)
  ENDIF
  IF (IRTIM(6).EQ.0) THEN
    IRTIM2 = 0
  ELSE
    IRTIM2 = DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9))
    IRTIM2 = (IRTIM2)*60+IRTIM(10)
  ENDIF
  if (lflag) print*,'irtim1/2:',irtim1,irtim2

!-----------------------------------------------------------------------
! initialise loop counters
!-----------------------------------------------------------------------

  IENT1   = 1   ! ENTRY NUMBER
  IREP1   = 1   ! REPORT WITHIN MESSAGE
  NXTREP  = 1
  NSEND   = 0
  LMIDMSG = .FALSE. ! no ob waiting for user to call again   !2.1
  LCONT   = .FALSE.
  LDONE   = .FALSE. ! SET TRUE WHEN ELEMENTS HAVE BEEN MAPPED
  MSGSNT  = .FALSE.
  INDXRED = .FALSE. ! INDEX HAS NOT BEEN READ
  NTRYCHK = .FALSE. ! ENTRY HAS NOT BEEN CHECKED
  CHANRED = .FALSE. ! CHAIN HAS NOT BEEN READ
  LASTID  = ' '
  PASSED  = .FALSE.
ENDIF

ISTAT = 0

!=======================================================================
!
! LOOP OVER INDEX BLOCKS
! AT INDEX LEVEL CHECK FOR VALID INDEX BLOCK TIME
!
!=======================================================================

if (lflag) print*,' looping over times ',ishr1,ishr2,istep

DO 799 J1=ISHR1,ISHR2,ISTEP

!-----------------------------------------------------------------------
! check whether index has already been read
!-----------------------------------------------------------------------

IF(.NOT.INDXRED)THEN
  IF(LATEST)THEN
    if(lflag)print*,' reading',nb2rd,'index blocks'

!-----------------------------------------------------------------------
! ishr1=ishr2 so 799 only loops once.
! read up to 3 index blocks consecutively, starting with the latest.
!-----------------------------------------------------------------------

    IHL=J1
    KOUNT=0
    DO 130 J2=1,NB2RD

    IBLOCK=MOD(IHL/INXHRS,INXBLK)+2+NSQ                     !1.13a

    CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,LOCDFG,&
              &NELMIX, IBLOCK, INUM, CTRAN, IDATHR, IDREC,&    !D
              &IRECLN, KMSG, 'IDXRD')                       !2.0!D

!-----------------------------------------------------------------------
! check that correct date/time has been read
!-----------------------------------------------------------------------

    IDAY=IDATHR/256
    IHOUR=IDATHR-IDAY*256
    CALL HRS2DT(IY,IM,ID,IH,IHL)
    IF(ID.NE.IDAY.OR.IH.NE.IHOUR)THEN
      CALL HRS2DT(IY2,IM2,ID2,IH2,IHL+INXHRS-1)             !1.15b
      WRITE(6,9015)CTYPE,IY,IM,ID,IH,IY2,IM2,ID2,IH2        !1.15b
    ELSE
      ITHR(J2)=IHL

!-----------------------------------------------------------------------
! select index entries, setting last byte in identifier field to
! indicate which index block this report has come from (1,2 or 3)
!-----------------------------------------------------------------------

    DO 80 J3=1,INUM                  ! ADD ENTRIES FROM BLOCK

!-----------------------------------------------------------------------
! check identifier
!-----------------------------------------------------------------------

        CALL IDMTCH(CTRAN(J3),THISID,CIDENT,LFLAG,RC)         !2.0
        IF (RC.NE.0) GOTO 80                                  !2.0

!-----------------------------------------------------------------------
! Check if we want to rotate the Lat and Long values from the index
! entry for a rotated grid and then check it falls within the grid area
! or if we just want to check the observation lies within a normal lat
! long area or whether we want all obs (no checking)
! This can be done by looking at AREA(1). If = 0 then normal grid
!                                            = 1 then rotated grid
!                                            = anything else so no chk
!-----------------------------------------------------------------------

        NLAT=ICHAR(CTRAN(J3)(13:13))*256 +&                     !G
            &ICHAR(CTRAN(J3)(14:14))                            !G
        NLON=ICHAR(CTRAN(J3)(15:15))*256 +&                     !G
            &ICHAR(CTRAN(J3)(16:16))                            !G

        IF (NLAT.GT.32768) NLAT=NLAT-65536                      !G
        IF (NLON.GT.32768) NLON=NLON-65536                      !G

        RLAT=NLAT*0.01                                          !G
        RLON=NLON*0.01                                          !G

        IF (AREA(1).EQ.1.0) THEN                                !G
          CALL ROTAREA(RLAT,RLON,ROT_LAT,ROT_LON,RPOLE)         !G
          CALL VALAREA(ROT_LAT,ROT_LON,AREA,AREA_FLAG,LFLAG)    !G
        ELSEIF (AREA(1).EQ.0.0) THEN                            !G
          CALL VALAREA(RLAT,RLON,AREA,AREA_FLAG,LFLAG)          !G
        ELSE                                                    !G
          AREA_FLAG=.TRUE.                                      !G
        ENDIF                                                   !G

        IF (AREA_FLAG) THEN                                     !G

!-----------------------------------------------------------------------
! check if there are any tafs of the right type in the chain
! (taftyp is 1 or 2 if only long or short tafs, 3 if both)
!-----------------------------------------------------------------------

          IF ((CTYPE(1:4).EQ.'TAFS' .OR.&                   !1.13b
              &CTYPE(1:5).EQ.'ATAFS') .AND.&                !1.13b
              &TAFLEN.GT.0) THEN                      !1.18 !1.13b
            TAFTYP=MOD(ICHAR(CTRAN(J3)(17:17)),4)               !B
            IF (TAFTYP.LT.3 .AND. TAFTYP.NE.TAFLEN) GO TO 80    !B
          ENDIF

!-----------------------------------------------------------------------
! Check for METARs and/or SPECIs in the chain.
! If METARs only         then METTYP is 0.
! If SPECIs only or both then METTYP is 3.
! If SPECIs wanted but only METARs in the chain, then skip the chain.
!-----------------------------------------------------------------------

                IF (CTYPE(1:6).EQ.'METARS') THEN                   !1.18
                  METTYP=MOD(ICHAR(CTRAN(J3)(17:17)),4)            !1.18
                  IF (METSPECI.EQ.3 .AND. METTYP.EQ.0) GOTO 80     !1.18
                ENDIF                                              !1.18

                if(lflag)print*,thisid,' wanted for further tests'

                KOUNT=KOUNT+1
                CNTRY(KOUNT)=CTRAN(J3)
                CNTRY(KOUNT)(11:11)=CHAR(J2)   ! (J2=1,2,3 FOR BLOCK) !A

!-----------------------------------------------------------------------
! if only one ident requested & whole strings match, stop looking. !b
! (latest index first, so jump out of loop, not just this index block)
!-----------------------------------------------------------------------

          IF (CIDENT(2).EQ.'00000' .AND.&
             &CIDENT(1)(1:8).EQ.CTRAN(J3)(3:10)) GO TO 131      !B

        ENDIF  !- area_flag                                     !G

80    CONTINUE

    ENDIF

!-----------------------------------------------------------------------
! decrement hour
!-----------------------------------------------------------------------

    IHL=IHL-INXHRS
130       CONTINUE ! END OF LOOP OVER INDEX BLOCKS
131       ITRIES=KOUNT                                               !B
    if (lflag) print *,itries,'entries accepted for ithr',ithr
  ELSE       ! END OF 'LATEST' BLOCK

!-----------------------------------------------------------------------
! if not latest, read a single index block for this period
!-----------------------------------------------------------------------

    IBLOCK=MOD(J1/INXHRS,INXBLK)+2+NSQ                      !1.13a


    CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,LOCDFG,&!B
              &NELMIX, IBLOCK, INUM, CTRAN, IDATHR, IDREC,&    !D
              &IRECLN, KMSG, 'IDXRD')                       !2.0!D
      ITRIES=0
      DO 81 J3=1,INUM                  ! ADD ENTRIES FROM BLOCK
        CALL IDMTCH(CTRAN(J3),THISID,CIDENT,LFLAG,RC)         !2.0
        IF (RC.NE.0) GOTO 81                                  !2.0

!-----------------------------------------------------------------------
! Check if we want to rotate the Lat and Long values from the index
! entry for a rotated grid and then check it falls within the grid area
! or if we just want to check the observation lies within a normal lat
! long area or whether we want all obs (no checking)
! This can be done by looking at AREA(1). If = 0 then normal grid
!                                            = 1 then rotated grid
!                                            = anything else so no chk
!-----------------------------------------------------------------------

        NLAT=ICHAR(CTRAN(J3)(13:13))*256+&                     !G
            & ICHAR(CTRAN(J3)(14:14))                            !G
        NLON=ICHAR(CTRAN(J3)(15:15))*256+&                     !G
            & ICHAR(CTRAN(J3)(16:16))                            !G

        IF (NLAT.GT.32768) NLAT=NLAT-65536                      !G
        IF (NLON.GT.32768) NLON=NLON-65536                      !G

        RLAT=NLAT*0.01                                          !G
        RLON=NLON*0.01                                          !G

        IF (AREA(1).EQ.1.0) THEN                                !G
          CALL ROTAREA(RLAT,RLON,ROT_LAT,ROT_LON,RPOLE)         !G
          CALL VALAREA(ROT_LAT,ROT_LON,AREA,AREA_FLAG,LFLAG)    !G
        ELSEIF (AREA(1).EQ.0.0) THEN                            !G
          CALL VALAREA(RLAT,RLON,AREA,AREA_FLAG,LFLAG)          !G
        ELSE                                                    !G
          AREA_FLAG=.TRUE.                                      !G
        ENDIF                                                   !G

        IF (AREA_FLAG) THEN                                     !G


          IF ((CTYPE(1:4).EQ.'TAFS' .OR.&                   !1.13b
              &CTYPE(1:5).EQ.'ATAFS') .AND.&                !1.13b
              &TAFLEN.GT.0) THEN                      !1.18 !1.13b
            TAFTYP=MOD(ICHAR(CTRAN(J3)(17:17)),4)               !B
            IF (TAFTYP.LT.3 .AND. TAFTYP.NE.TAFLEN) GO TO 81    !B
          ENDIF
          if(lflag)print*,thisid,' wanted for further tests'
          ITRIES=ITRIES+1
          CNTRY(ITRIES)=CTRAN(J3)                               !B

!-----------------------------------------------------------------------
! Check for METARs and/or SPECIs in the chain.
! If METARs only         then METTYP is 0.
! If SPECIs only or both then METTYP is 3.
! If SPECIs wanted but only METARs in the chain, then skip the chain.
!-----------------------------------------------------------------------

          IF (CTYPE(1:6).EQ.'METARS') THEN                   !1.18
            METTYP=MOD(ICHAR(CTRAN(J3)(17:17)),4)            !1.18
            IF (METSPECI.EQ.3 .AND. METTYP.EQ.0) GOTO 81     !1.18
          ENDIF                                              !1.18

!-----------------------------------------------------------------------
! if only one ident requested & whole strings match, stop looking!b
!-----------------------------------------------------------------------

          IF (CIDENT(2).EQ.'00000' .AND.&
             &CIDENT(1)(1:8).EQ.CTRAN(J3)(3:10)) GO TO 82       !B

        ENDIF  !- area_flag

81    CONTINUE

82  IDAY=IDATHR/256                                             !B
    IHOUR=IDATHR-IDAY*256
    CALL HRS2DT(IY,IM,ID,IH,J1)
    IF(ID.NE.IDAY.OR.IH.NE.IHOUR)THEN
      CALL HRS2DT(IY2,IM2,ID2,IH2,J1+INXHRS-1)              !1.15b
      WRITE(6,9015)CTYPE,IY,IM,ID,IH,IY2,IM2,ID2,IH2        !1.15b
      GOTO 799  ! NEXT INDEX BLOCK
    ENDIF
    if(lflag)print*,'from index block',iblock,itries,'entries'
  ENDIF

!-----------------------------------------------------------------------
! sort entries into station identifier order
! (for latest reports the last byte in the identifier field has been
! set above to 1,2 or 3 to indicate the index block)                  !A
!-----------------------------------------------------------------------

  CALL SORTCH(CNTRY,INDLEN,ITRIES,MASK)                         !A
  INDXRED=.TRUE.
ENDIF

!=======================================================================
!
! LOOP OVER ENTRIES FROM THIS INDEX BLOCK (OR BLOCKS IF LATEST)
!
!=======================================================================

if (lflag) then
  write(*,*)'in tfmret: looping over entries ',ient1,itries
endif

!-----------------------------------------------------------------------
! _________________________________________________________________
! :      :      : HOUR : MINUTE :IDENTI-  :NOBS:LATITUDE :LONGITUDE
! :      :      :  (6  :   (1   :  FIER   :IN  :         :
! :      :      : BITS):  BYTE) :(9 BYTES):CHN :(2 BYTES):(2 BYTES
!
!                    1      2      3 - 11   12   13-14     15-16
!
! _____________________________
! : FLAGS : TOR : REC : BLOCK :
! :       :     : NUM : NUMBER:
! :       :     :     :       :
!
!   17     18-19 20-21  22-23
! -----------------
! :        :SH:LO :
! :        :RT:NG : BYTE 17 EXPANDED FOR TAFS & ATAFS
! -----------------
!  1 - 6    7   8
! BITS
!-----------------------------------------------------------------------

DO 699 J2=IENT1,ITRIES

!-----------------------------------------------------------------------
! get lat & long from index entry
!-----------------------------------------------------------------------

  LAT=ICHAR(CNTRY(J2)(13:13))*256+ICHAR(CNTRY(J2)(14:14))       !B
  LON=ICHAR(CNTRY(J2)(15:15))*256+ICHAR(CNTRY(J2)(16:16))       !B
  IF (LAT.GE.32768) LAT=LAT-65536                               !B
  IF (LON.GE.32768) LON=LON-65536                               !B

  IF (LAT.NE.-32768) THEN                                       !B
    RLAT=LAT*0.01                                               !B
    RLON=LON*0.01                                               !B
  ELSE                                                          !B
    RLAT=-9999999.                                              !B
    RLON=-9999999.                                              !B
  ENDIF                                                         !B

!-----------------------------------------------------------------------
! at entry level check for required identifier and lat/lon area.
!-----------------------------------------------------------------------

IF(.NOT.NTRYCHK)THEN
  THISID=CNTRY(J2)(3:10)  ! IGNORE LAST BYTE (1,2,3 IF LATEST) !A

!-----------------------------------------------------------------------
! for latest requests we only need one report for a given id so
! check if a report of this id has already been returned
!-----------------------------------------------------------------------

  IF(LATEST)THEN
    IF(THISID.EQ.LASTID)THEN
      IF(PASSED)GOTO 699
    ELSE
      LASTID=THISID
      PASSED=.FALSE.
    ENDIF
  ENDIF

!-----------------------------------------------------------------------
! number of reports in chain
!-----------------------------------------------------------------------

  NCHN=ICHAR(CNTRY(J2)(12:12))
  IF (NCHN.GT.MXCHLEN) WRITE(*,*)'TFMRET: MDB WARNING - ',& !1.15a
    &'MAX CHAIN LENGTH EXCEEDED'                            !1.15a

  if(lflag)print*,' entry ',j2,' reports in chain ',nchn

!-----------------------------------------------------------------------
! physical block (not counting index & map blocks and local d entry)
!-----------------------------------------------------------------------

  NBLOCK = ICHAR(CNTRY(J2)(22:22))*256 + ICHAR(CNTRY(J2)(23:23))

  IBLOCK = 1+NSQ+INXBLK+NBLOCK                              !1.13a


!-----------------------------------------------------------------------
! and first record in block
!-----------------------------------------------------------------------

  NREC = ICHAR(CNTRY(J2)(20:20))*256 + ICHAR(CNTRY(J2)(21:21))
  IDREC=NREC

!-----------------------------------------------------------------------
! get date/time from cntry flag if we're looking at more than one
! index block, i.e. get back 1,2 or 3 set in byte 11 before sort
! and from that get index block time (kept in array).                '
!-----------------------------------------------------------------------

  IF(LATEST)THEN
    I=ICHAR(CNTRY(J2)(11:11))
    IDAT=ITHR(I)
    CALL HRS2DT(IY,IM,IDAY,IHOUR,IDAT)
    if(lflag)print*,'ithr(',i,') is ',idat
    if(lflag)print*,'amended time',iy,im,iday,ihour
  ELSE
    IDAT=J1
  ENDIF
  NTRYCHK=.TRUE.
ENDIF

!-----------------------------------------------------------------------
! Read in reports in this chain. READ_CHNREPS is true initially.
! Keep reading until READ_CHNREPS is false (when the pointer to
! the next report in the chain is zero).
! Keep the trailers for all the reports in the chain, but replace  !2.1
! a pointer to the next ob in the chain with a pointer to this ob, !2.1
! so that the obs can be easily read in again whichever way        !2.1
! (forwards or backwards) the chain is looped round.               !2.1
! So the pointer in the first trailer is taken from the index.     !2.1
!-----------------------------------------------------------------------

KREP=0

IF (.NOT.CHANRED) THEN                                          !K
  if (lflag) write(6,*)'tfmret: reading in chain'               !k

  READ_CHNREPS = .TRUE.                                         !K
  J5           = 0                                              !K
  KEPTIM       = -9999999                              !1.17!1.12b
  CTRAIL(1)(20:23)=CNTRY(J2)(20:23) ! pointer to first ob    !2.1

  DO WHILE (READ_CHNREPS)                                       !K
    if (lflag) write(6,*)                                       !k

!-----------------------------------------------------------------------
! get message
!-----------------------------------------------------------------------

  CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,LOCDFG,&
            &NELMIX, IBLOCK, INUM, CTRAN, IDATHR, IDREC,&      !D
            &IRECLN, KMSG, 'MSGRD')                            !DH

  J5 = J5 + 1                                                   !K

!-----------------------------------------------------------------------
! The code below treats NCM & SREWs differently from other types.  !2.2
! It's not clear why - but NCMs & SREWs can be skipped as the      !2.2
! chain is read in (if they fail TOR or preferred flag tests)      !2.2
! while for other data types the whole chain is read in.           !2.2
!-----------------------------------------------------------------------

  if(lflag)print*,' tfmret rec=',j5,found(9),'>',ctype,'<',iver !i
  if(lflag)print*,' tfmret report transferred kmsg(',j5,')>',&
           &kmsg(1:irecln-23),'<'                               !i

  TRAILX = KMSG(IRECLN-22:IRECLN)                               !H
                                                                !H
  IF ((CTYPE(1:3).EQ.'NCM' .OR. CTYPE(1:4).EQ.'SREW') .AND.&!1.14a
    &IVER.NE.2) THEN                                        !1.14a

    NTOR = ICHAR(TRAILX(18:18))*256 + ICHAR(TRAILX(19:19))      !H
                                                                      !H
!-----------------------------------------------------------------------
! check to see if ntor is meant to be negative. ntor is read from 2   !H
! bytes which means that the maximum positive value ntor can have is  !H
! 32768. if ntor is greater than this value it must be converted into !H
! a negative number. this is done simply by subtracting the maximum   !H
! possible value that can be held in a 2 byte integer when the sign bitH
! is ignored (i.e. 2 to the power 16) from ntor.                      !H
!-----------------------------------------------------------------------
                                                                      !H
    IF (NTOR.GT.32768 .AND. .NOT.LCLIM) THEN              !1.13c!H
      NTOR = NTOR - 65536                                       !H
    ENDIF                                                       !H
                                                                      !H
!-----------------------------------------------------------------------
! Change ITORM.GT.IRTIM2 to ITORM.GE.IRTIM2                          !Eh
!-----------------------------------------------------------------------

    IDHR=ICHAR(TRAILX(1:1))                                  !1.16
    IDHR=MOD(IDHR,64)                                        !1.16
    IMIN=ICHAR(TRAILX(2:2))                                  !1.16

    ITORM = (IDAT)*60 + NTOR                                    !H
    ISPREF=ICHAR(TRAILX(17:17))/128.EQ.1                        !H
    WASPREF=MOD(ICHAR(TRAILX(17:17))/32,2).EQ.1                 !H
    if(lflag)then                                               !h
      write(6,'('' tfmret ctrail >'',z46,''<'')')trailx(1:23)   !h
      print*,' tfmret msg >',kmsg(1:irecln-23),'<'              !h
    endif                                                       !h
                                                                !h
    if(lflag)print*,' tfmret is/waspref ',j5,ispref,waspref,itorm,&
      &irtim1,irtim2,keptim,krep,nchn,imin,idrec                !h
!-----------------------------------------------------------------------
! check time of receipt & preferred flags, only keeping report if it  !H
! was received in time slot and (if ver=1, preferred report requested),H
! it is preferred or was preferred at the cutoff time.  if ver=1, only!H
! one report is wanted for any time, but two or more obs for that time!H
! may have been preferred before a cutoff, so keep report time when a !H
! preferred report is found to avoid returning more than one. (reports!H
! for the same time are consecutive in the chain). Change check (!H)  !H
! slightly from .GT. to .GE.                                          !H
!-----------------------------------------------------------------------
                                                                      !H
    SKIP=.FALSE.                                             !2.2
    IF (ITORM.LT.IRTIM1) THEN                                   !H
      SKIP=.TRUE.                                                !2.2
      if(lflag)print*,' tfmret time test received too early ',&
      &'itorm<irtim1 ',itorm,irtim1                              !h
    ELSEIF (IRTIM2.GT.0 .AND. ITORM.GE.IRTIM2) THEN              !H
      SKIP=.TRUE.                                                !2.2
      if(lflag)print*,' tfmret time test received too late ',&
      &'itorm>=irtim2 ',itorm,irtim2                             !h
    ELSEIF (IVER.EQ.1.AND.IRTIM2.EQ.0.AND..NOT.ISPREF) THEN      !H
      SKIP=.TRUE.                                                !2.2
      if(lflag)print*,' tfmret time test not finally preferred ',&
      & ' iver,irtim2,ispref ',iver,irtim2,ispref                 !h
    ELSEIF (IVER.EQ.1.AND.IRTIM2.GT.0.AND..NOT.WASPREF) THEN     !H
      SKIP=.TRUE.                                                !2.2
      if(lflag)print*,' tfmret time test ',&
      & 'not preferred at cutoff time ',&
      & ' iver,irtim2,waspref ',iver,irtim2,waspref               !h
    ELSEIF (IVER.EQ.1.AND.IDHR*60+IMIN.EQ.KEPTIM) THEN           !1.16
      SKIP=.TRUE.                                                !2.2
      if(lflag)print*,' tfmret: only one ob for each time ',&     !1.16
      & ' iver,keptim ',iver,keptim                               !1.16
    ELSE                                                         !H
      if(lflag)print*,' tfmret time test 6 - good report'        !h
      KEPTIM=IDHR*60+IMIN  ! return only one for this time       !1.16
KREP=KREP+1      ! add to number of reports found          !H
      CTRAIL(KREP)(1:19)=KMSG(IRECLN-22:IRECLN-4)          !2.1
      IF (KREP.LT.NCHN) THEN                                 !2.1
        CTRAIL(KREP+1)(20:23)=KMSG(IRECLN-3:IRECLN)          !2.1
      ENDIF                                                  !2.1
                                                                !H
      if(lflag)print*,' tfmret block ',iblock,' record ',idrec  !h
    ENDIF                                                       !H

! If a report has been skipped above, overwrite the pointer to the !2.2
! skipped report (kept in advance) with the pointer to the next    !2.2
! report (which may, of course, be overwritten in its turn).       !2.2

    IF (SKIP .AND. KREP.LT.NCHN) THEN                        !2.2
      CTRAIL(KREP+1)(20:23)=KMSG(IRECLN-3:IRECLN)            !2.2
    ENDIF                                                    !2.2
                                                                !H
  ELSE                                                          !H
    KREP=KREP+1        ! add to number of reports found         !H
    CTRAIL(KREP)(1:19)=KMSG(IRECLN-22:IRECLN-4)              !2.1
    IF (KREP.LT.NCHN) THEN                                   !2.1
      CTRAIL(KREP+1)(20:23)=KMSG(IRECLN-3:IRECLN)            !2.1
    ENDIF                                                    !2.1
                                                                !H
    if(lflag)print*,' tfmret block ',iblock,' record ',idrec    !h
  ENDIF                                                         !H
                                                                !H

!-----------------------------------------------------------------------
! get block and record number of next in chain. If the block number or
! record number = 0, set READ_CHNREPS = .FALSE. - we will read the
! chain no further
!-----------------------------------------------------------------------

  NBLOCK = ICHAR(TRAILX(22:22))*256&                        !1.13a
                &+ ICHAR(TRAILX(23:23))                     !1.13a

!-----------------------------------------------------------------------
! The code that follows is a fix needed at least for METARs in    !1.14c
! April-May 1997, when it appears that one-day data sets were     !1.14c
! made with the correct block number in zero pointers and the     !1.14c
! original block number in pointers that need to be followed!     !1.14c
! So assume chains must be within blocks if the first pointer     !1.14c
! (at the end of the block) has record zero & correct block.      !1.14c
!-----------------------------------------------------------------------

  dubious_chains=.FALSE.                                    !1.14c
  IF (NBLOCK.NE.IBLOCK-1-NSQ-INXBLK) THEN                   !1.14c
    CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,&    !1.14c
              &LOCDFG,NELMIX,IBLOCK,INUM,CTRAN,IDATHR,1,&   !1.14c
              &IRECLN,KMSG,'MSGRD')                         !1.14c
    block_in_first_pointer=&                                !1.14c
      &ICHAR(KMSG(IRECLN-1:IRECLN-1))*256&                  !1.14c
     &+ICHAR(KMSG(IRECLN:IRECLN))                           !1.14c
    record_in_first_pointer=&                               !1.14c
      &ICHAR(KMSG(IRECLN-3:IRECLN-3))*256&                  !1.14c
     &+ICHAR(KMSG(IRECLN-2:IRECLN-2))                       !1.14c
    IF (block_in_first_pointer.EQ.IBLOCK-1-NSQ-INXBLK&      !1.14c
       &.AND. record_in_first_pointer.EQ.0) THEN            !1.14c
      dubious_chains=.TRUE.                                 !1.14c
    ENDIF                                                   !1.14c
  ENDIF                                                     !1.14c

  IF (.NOT.dubious_chains) IBLOCK = 1+NSQ+INXBLK+NBLOCK     !1.14c

  NREC = ICHAR(TRAILX(20:20))*256&
              &+ ICHAR(TRAILX(21:21))                           !H
  IDREC=NREC

  if (lflag) then                                           !1.13a
          write(6,*)'tfmret: next block, record in chain = ',&    !1.13a
          &iblock,idrec                                          !1.13a
        endif                                                     !1.13a

        READ_CHNREPS = (IDREC.GT.0 .AND. NBLOCK.GT.0 .AND.&      !1.15a
     &                  KREP.LT.MXCHLEN)                          !1.15a

        ENDDO      !- end of loop over reports in chain               !K

!-----------------------------------------------------------------------
! decide whether to loop forwards or backwards over chain
! order='f' is forwards in time i.e start at the end of the chain
! and loop from nchn to 1
!-----------------------------------------------------------------------

        IF(LATEST.OR.ORDER.EQ.'B')THEN
          ICHL1=1
          ICHL2=KREP
          ICHL3=1
        ELSE    ! IF(ORDER.EQ.'F')THEN
          ICHL1=KREP
          ICHL2=1
          ICHL3=-1
        ENDIF
        CHANRED=.TRUE.
      ENDIF

!=======================================================================
!
! LOOP OVER REPORTS IN CHAIN
!
! AT MESSAGE LEVEL CHECK TIME OF OB AND TIME OF RECEIPT.
! ALSO CHECK TAF LONG/SHORT FLAG
!
!=======================================================================

      if(lflag)print*,' looping over chain ',ichl1,ichl2,ichl3

      DO 675 J5=ICHL1,ICHL2,ICHL3

!-----------------------------------------------------------------------
! TFMRET only deals with data types with only one ob per message,  !2.1
! but LMIDMSG is set when an ob was ready to return to the user,   !2.1
! but the user's array was full.  If so, checks can be skipped.    !2.1
!-----------------------------------------------------------------------

  IF(.NOT.LMIDMSG)THEN

!-----------------------------------------------------------------------
! check taf type at report level
!-----------------------------------------------------------------------

    IF (CTYPE(1:4).EQ.'TAFS' .OR.&                          !1.13b
       &CTYPE(1:5).EQ.'ATAFS') THEN                         !1.13b
      TAFTYP=MOD(ICHAR(CTRAIL(J5)(17:17)),4)
      IF (TAFLEN.NE.0.AND.TAFLEN.NE.TAFTYP) GOTO 675         !1.18
    ENDIF

!-----------------------------------------------------------------------
! Check whether METAR or SPECI at the report level.
!-----------------------------------------------------------------------

    IF (CTYPE(1:6).EQ.'METARS') then                         !1.18
      METTYP=MOD(ICHAR(CTRAIL(J5)(17:17)),4)                 !1.18
      IF (METSPECI.NE.METTYP) GOTO 675                       !1.18
    ENDIF                                                    !1.18

    IDHR=ICHAR(CTRAIL(J5)(1:1))
    IDHR=MOD(IDHR,64)
    IMIN=ICHAR(CTRAIL(J5)(2:2))

!-----------------------------------------------------------------------
! compare obs time with start/end times if given
!-----------------------------------------------------------------------

    IF(FOUND(1))THEN                    ! IF START TIME GIVEN
      ICMIN=DT2HRS(IY,IM,IDAY,IHOUR+IDHR) ! CENTURY-HOUR OF OB
      ICMIN=ICMIN*60 + IMIN             ! CENTURY-MINUTE OF OB
      if(lflag)print*,' tfmret c.min',icmin,iy,im,iday,ihour,idhr,&
                     & imin
      if(lflag)print*,' rept c.min',icmin,itimst,itimnd
      IF(FOUND(1).AND.ICMIN.LT.ITIMST) GOTO 675 ! IF START GIVEN..
      IF(FOUND(2).AND.ICMIN.GT.ITIMND) GOTO 675 ! IF END GIVEN...
      if(lflag)print*,' tfmret rept passed obs time test '

!-----------------------------------------------------------------------
! if there's an increment in the request, see if report is needed.   '
!-----------------------------------------------------------------------

      IF(FOUND(4))THEN                                         !B
        CALL SUBPER(ITIMST,TRANGE,INCR,ITIMND,ICMIN,RC)       !2.0
        IF (RC.NE.0) GOTO 675                                 !2.0
        if(lflag)print*,' fits increment',incr,'hours'         !b
      ENDIF
    ENDIF

!-----------------------------------------------------------------------
! check if tor in range of users request
! entry holds time of receipt in minutes after slot base hour
!-----------------------------------------------------------------------

    NTOR = ICHAR(CTRAIL(J5)(18:18))*256 + ICHAR(CTRAIL(J5)(19:19))

!-----------------------------------------------------------------------
! check to see if ntor is meant to be negative. ntor is read from 2
! bytes which means that the maximum positive value ntor can have is
! 32768. if ntor is greater than this value it must be converted into
! a negative number. this is done simply by subtracting the maximum
! possible value that can be held in a 2 byte integer when the sign bit
! is ignored (i.e. 2 to the power 16) from ntor.
!-----------------------------------------------------------------------

    IF (NTOR.GT.32768 .AND. .NOT.LCLIM) THEN                !1.13c
      NTOR = NTOR - 65536
    ENDIF

!-----------------------------------------------------------------------
! Change ITORM.GT.IRTIM2 to ITORM.GE.IRTIM2                           !E
!-----------------------------------------------------------------------

    ITORM = (IDAT)*60 + NTOR
    IF((IRTIM1+IRTIM2.NE.0).AND.&
      &(IRTIM1.NE.0 .AND. ITORM.LT.IRTIM1) .OR.&
      &(IRTIM2.NE.0 .AND. ITORM.GE.IRTIM2)) GOTO 675           !E
    PASSED=.TRUE.

    if (lflag) then
      print*,'itorm:',itorm
      print*,' ntor ',ntor
      print*,' report passed all checks '
    endif

!-----------------------------------------------------------------------
! calculate actual t.o.r in isect1
!-----------------------------------------------------------------------

    ISECT1(5)=MOD(ITORM,60)
    IRCHR=ITORM/60
    CALL HRS2DT(ISECT1(1),ISECT1(2),ISECT1(3),ISECT1(4),IRCHR)
    if(lflag)print*,' isect',isect1

!-----------------------------------------------------------------------
! get collecting centre, bulletin code, amend and /cor numbers
!-----------------------------------------------------------------------

    BULCOD=CTRAIL(J5)(3:6)
    CCCC=CTRAIL(J5)(8:11)
    NAC=ICHAR(CTRAIL(J5)(7:7))
    NAMD=NAC/16
    NCOR=MOD(NAC,16)
    ISECT1(7)=NAMD
    ISECT1(8)=NCOR

!-----------------------------------------------------------------------
! Read the report in again, using pointer kept in trailer-reading  !2.1
! loop. Put total length at start & initialise header.             !2.1
!-----------------------------------------------------------------------

    IBLOCK=ICHAR(CTRAIL(J5)(22:22))*256&                     !2.1
         &+ICHAR(CTRAIL(J5)(23:23))+1+NSQ+INXBLK             !2.1
    IDREC =ICHAR(CTRAIL(J5)(20:20))*256&                     !2.1
         &+ICHAR(CTRAIL(J5)(21:21))                          !2.1

    CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,&     !2.1
              &LOCDFG,NELMIX,IBLOCK,INUM,CTRAN,IDATHR,&      !2.1
              &IDREC,IRECLN,KMSG,'MSGRD')                    !2.1
    IRECLN=IRECLN-INDLEN                                     !2.1

    WRITE(CREP(1:4),'(I4)')IRECLN+IHDLEN                     !2.1

    IF (LCLIM) THEN                                         !1.13a
      CALL SETHED(HEADER,RLAT,RLON,0,0,1,IM,&               !1.13a
                 &IY,ISECT1(4),ISECT1(5),NAMD,NCOR,CCCC)    !1.13a
    ELSE                                                    !1.13a
      CALL SETHED(HEADER,RLAT,RLON,IHOUR+IDHR,IMIN,IDAY,IM,&!1.13a
                 &IY,ISECT1(4),ISECT1(5),NAMD,NCOR,CCCC)    !1.13a
    ENDIF                                                   !1.13a

    CREP(4+1:4+IHDLEN)=HEADER                                !2.1
    CREP(4+IHDLEN+1:4+IHDLEN+IRECLN)=KMSG(1:IRECLN)          !2.1
    if(lflag)print*,'message length',irecln,crep(1:80)       !2.1

!=======================================================================
!
! EXPAND REPORT (UNLESS JUST THE RAW REPORT IS NEEDED)
!
!=======================================================================

!-----------------------------------------------------------------------
! Initialise expansion arrays
!-----------------------------------------------------------------------
    IF(CTYPE(1:6).EQ.'METARS')THEN
      CALL MTRINT(VALUES,CNAM,RLAT,RLON,IY,IM,IDAY,IHOUR+IDHR,&
               &IMIN,BULCOD,CCCC,THISID)
      if(lflag)then
        write(6,9011)(values(k),k=1,10)
        write(6,*)cnam(1:80)
      endif
    ELSEIF (CTYPE(1:4).EQ.'TAFS' .OR.&                      !1.13b
         &CTYPE(1:5).EQ.'ATAFS') THEN                       !1.13b
      CALL TAFINT(VALUES,CNAM,RLAT,RLON,IY,IM,IDAY,IHOUR+IDHR,&!C
               &IMIN,BULCOD,CCCC,THISID,CTAF(TAFTYP))          !C
      if(lflag)then
        write(6,9011)(values(k),k=1,10),(values(150+k),k=1,10),&
                  &(values(300+k),k=1,10)
        write(6,*)cnam(1:80)
      endif
    ELSEIF(CTYPE(1:4).EQ.'SREW')THEN                           !C
      CALL SRWINT(VALUES,CNAM,RLAT,RLON,IY,IM,IDAY,IHOUR+IDHR,&!C
               &BULCOD,CCCC,THISID)                            !C
    ELSEIF(CTYPE(1:3).EQ.'NCM')THEN                            !C
      CALL NCMINT(VALUES,CNAM,RLAT,RLON,IY,IM,IDAY,IHOUR+IDHR,&!C
               &IMIN,BULCOD,CCCC,THISID)                       !C
    ELSEIF(CTYPE(1:4).EQ.'TROP') THEN                           !L
      CALL TRPINT(VALUES,CNAM,IY,IM,IDAY,IHOUR+IDHR,IMIN,&  !1.14b
               &BULCOD,CCCC,THISID)                         !1.14b
    ENDIF

!-----------------------------------------------------------------------
! check if expansion is needed
!-----------------------------------------------------------------------

    IF(.NOT.CHAREP)THEN     ! SOME ELEMENTS SELECTED
      IF (EXPAND) THEN      ! WHICH ARE NOT IN INDEX
        if(lflag)print*,' doing expansion '
        IF(CTYPE(1:6).EQ.'METARS')THEN

!-----------------------------------------------------------------------
! expand metar report
!-----------------------------------------------------------------------

          CALL MTREXP(KMSG,IRECLN,VALUES,CNAM,IFAIL)         !2.1
          if(lflag)write(6,9011)(values(k),k=1,66)

        ELSEIF (CTYPE(1:4).EQ.'TAFS' .OR.&                  !1.13b
               &CTYPE(1:5).EQ.'ATAFS') THEN                 !1.13b

!-----------------------------------------------------------------------
! expand taf reports
!-----------------------------------------------------------------------
          CALL TAFEXP(KMSG,IRECLN,VALUES,NAMD,IFAIL)       !2.1,2
          if(lflag)then
            write(6,9014)(values(k),k=1,150)
9014            format(' values after tafexp'/&
                       &(5F13.2) )
          ENDIF

!2.3 delete 11 lines ---------------------------------------------------
! print one warning message if any obs cannot be decoded.
!-----------------------------------------------------------------------
!
          IF(IFAIL.EQ.8)THEN
            IF(.NOT.MSGSNT)THEN
              WRITE(6,9008)
              MSGSNT=.TRUE.
            ENDIF
            IF(NREP.EQ.1)GOTO 675 ! SKIP TO NEXT REPORT
          ENDIF
        ELSEIF(CTYPE(1:4).EQ.'SREW')THEN
          CALL SRWEXP(KMSG,VALUES,IRECLN)                    !2.1
        ELSEIF(CTYPE(1:3).EQ.'NCM')THEN
          CALL NCMEXP(KMSG,IRECLN,VALUES)                    !2.1
        ENDIF
      ENDIF
    ENDIF ! END OF EXPANSION
  ENDIF

  NREP=1
  IMAX=1

!-----------------------------------------------------------------------
! Check if there's room for this report; if not, set LMIDMSG.      !2.1
!-----------------------------------------------------------------------

  IF(NOBS-IOB.LT.NREP-NSEND)THEN
    NSEND=IREP1+(NOBS-IOB)-1
    LMIDMSG=.TRUE.
  ELSE
    NSEND=NREP
    LMIDMSG=.FALSE.
  ENDIF
  if(lflag)print*,' &&& irep1,nsend,iob,lmidmsg',irep1,&
                          &nsend,iob,lmidmsg

!=======================================================================
!
! Transfer data from VALUES, ISECT1, CNAM & CREP,                  !2.1
! using displacements in IDISP array,                              !2.1
! to ARRAY, CSTR & CREPRT, starting at IOB+1.                      !2.1
!
!=======================================================================

!-----------------------------------------------------------------------
! map users elements to displacements in values array
!-----------------------------------------------------------------------

  IF(.NOT.LDONE)THEN
    if(lflag) then
      write(6,*)'calling condes:ndes,idesc,idisp',ndes,idesc,idisp
    endif
    CALL CONDES(IDESC,NDES,IDISP)
    IMD=NDES
    if(lflag)write(6,9010)(idisp(k),k=1,ndes)
    LDONE=.TRUE.
  ENDIF
  CALL TRNSFR(IDISP,IMD,IREP1,NSEND,CSTR,IOB,ARRAY,CREP,NOBS,&
             &NELEM,ISECT1,IMAX,VALUES,CNAM,CREPRT,II,LFLAG)
  if (lflag) then
    write(6,9013)((array(k1,k2),k1=1,nrep),k2=1,imd)
9013      format(' output array after trnsfr'/(8f10.2))
    write(6,*)' '
  endif

!=======================================================================
!
! INCREMENT AND SAVE LOOP COUNTERS FOR NEXT ENTRANCE
!
!=======================================================================

!-----------------------------------------------------------------------
! LMIDMSG is true here when a message has been decoded             !2.1
! but the data hasn't been put in the user's array.                !2.1
!-----------------------------------------------------------------------

  IF(LMIDMSG)THEN
    if(lflag)print*,'no more room in array',iob               !2.0
    IF(NSEND+1.GT.NREP)THEN ! (Must be true if one ob/msg?)  !2.1
      NXTREP=1
      NSEND=0
      IF(EOCHN(J5,ICHL2,ICHL3))THEN ! TEST FOR END OF CHAIN
        NXTCHN=1
        CHANRED=.FALSE.
        IF(J2+1.GT.ITRIES)THEN ! END OF INDEX ENTRIES THIS BLOCK
          NXTENT=1
          IF(J1+INXHRS.GT.ISHR2)GOTO 999 ! FINISHED ALTOGETHER
          NXTIME=J1+INXHRS
          INDXRED=.FALSE.
        ELSE
          NXTENT=J2+1
          NXTIME=J1
        ENDIF
        NTRYCHK=.FALSE.
      ELSE
        NXTCHN=J5+ICHL3 ! NEXT IN CHAIN
        NXTENT=J2  ! SAME INDEX ENTRY
        NXTIME=J1  ! SAME INDEX BLOCK
      ENDIF
    ELSE      ! (Never reached if only one ob per message?)  !2.1
      NXTREP=NSEND+1 ! NEXT REPORT IN MESSAGE
      NXTCHN=J5
      NXTIME=J1
      NXTENT=J2
    ENDIF
    ISTAT=4 ! SET MORE DATA TO COME AND RETURN
    if (lflag) write(6,9009)nxtrep,nxtent,nxtime,nxtchn
    GOTO 999
  ELSE                      ! if not LMIDMSG...
    IOB=IOB+(NSEND-IREP1)+1 ! INCREMENT NO OF REPORTS TRANSFERRED
    NSEND=0
    NXTREP=1
    IREP1=1
    IF(IOB.EQ.NOBS)THEN ! ARRAY IS NOW FULL

!-----------------------------------------------------------------------
! increment loop variables for entry next time
!-----------------------------------------------------------------------

      IF(EOCHN(J5,ICHL2,ICHL3))THEN
        NXTCHN=1
        CHANRED=.FALSE.
        IF(J2+1.GT.ITRIES)THEN
          NXTENT=1
          IF(J1+INXHRS.GT.ISHR2)GOTO 999
          NXTIME=J1+INXHRS
          INDXRED=.FALSE.
        ELSE
          NXTENT=J2+1
          NXTIME=J1
        ENDIF
        NTRYCHK=.FALSE.
      ELSE
        NXTCHN=J5+ICHL3
        NXTENT=J2
        NXTIME=J1

!-----------------------------------------------------------------------
! force identifier check again on re-entry, otherwise you get more
! than just the latest obs
!-----------------------------------------------------------------------

        IF(LATEST)THEN
          NTRYCHK=.FALSE.
          CHANRED=.FALSE.
        ENDIF
      ENDIF
      ISTAT=4 ! MORE DATA TO COME (MAYBE)
      if(lflag)print*,'array full:nxtrep,nxtent,nxtime ',&
                      &nxtrep,nxtent,nxtime
      GOTO 999
    ENDIF
  ENDIF
  IF(LATEST.AND.PASSED)THEN
    NTRYCHK=.FALSE.
    CHANRED=.FALSE.
    if(lflag)print*,' do not want any more of this chain'
    GOTO 699
  ENDIF
675   CONTINUE   ! END OF LOOP OVER REPORTS IN THIS CHAIN
      NTRYCHK=.FALSE.
      CHANRED=.FALSE.
      if(lflag)print*,' idrec at end of chain is ',idrec

!-----------------------------------------------------------------------
! we have got to the end of the chain for the required report
! if no more identifiers are needed we can skip the rest of the
! index entries
!-----------------------------------------------------------------------

699   CONTINUE   ! END OF LOOP OVER ENTRIES THIS HOUR
IENT1=1
INDXRED=.FALSE.
799   CONTINUE   ! END OF LOOP OVER HOURS
IF(IOB.EQ.0)THEN
  IF(LCONT)THEN
    ISTAT=0
  ELSE
    ISTAT=8
  ENDIF
ENDIF

!-----------------------------------------------------------------------
! return number of reports
!-----------------------------------------------------------------------

999   CONTINUE

IF (II.NE.0) THEN                                           !1.12a
  IOBS=II                                                   !1.12a
ELSE                                                        !1.12a
  IOBS=IOBS-1                                               !1.12a
ENDIF                                                       !1.12a

if(lflag)print*,'exit. ii,iob,iobs,nobs',ii,iob,iobs,nobs
RETURN

9001  FORMAT(' POINTERS ON RE-ENTRY:'/&
      &' START HOUR ',I8/&
      &' ENTRY NUMBER ',I5/&
      &' REPORT NUMBER ',I5/&
      &' CHAIN ',I5/&
      &' NSEND ',I5)
9002  FORMAT(' DESCRIPTOR '/(5I8))
9003  FORMAT(' REQUEST START:',I10,' END:',I10)
9004  FORMAT(' MDB WARNING: CAN ONLY FIND LATEST REPORTS IN',&
      &' THE ',I3,' HOURS UP TO ',I4,'/',I2.2,'/',I2.2,1X,&
      &I4.4,'Z')
9005  FORMAT(' NO OF INDEX BLOCKS (INXBLK)',I8/&
      &' HOURS PER BLOCK    (INXHRS)',I8/&
      &' START OF 1ST SLOT  (ISLOTHR)',I8/&
      &' HOUR OFFSET        (INDHR1/2)',2I4/&
      &' SLOT HOURS FOR REQUEST  (ISHR1/2) ',2I8/&
      &' LOOP DIRECTION ',I8)
9008  FORMAT(' MDB WARNING: UNABLE TO DECODE REPORT(S)')
9009  FORMAT(' NXTREP =',I4,' NXTENT= ',I4/&
      &' NXTIME =',I4,' NXTCHN= ',I4)
9010  FORMAT(' IDISP',(10I5))
9011  FORMAT(' AFTER MTRINT ',5F12.3/5F12.3)
9012      FORMAT(' MDB ERROR: ARRAY NOT LARGE ENOUGH -TRY ARRAY(',&
           &I3,',',I3,')')
9015  FORMAT(' NO ',A8, ' DATA IN MDB FOR PERIOD ',I4,'/',&
            &I2.2,'/',I2.2,1X,I2.2,'Z - ',I4,'/',I2.2,'/',&
            &I2.2,1X,I2.2,'Z')                                    !1.15b
END SUBROUTINE TFMRET

LOGICAL FUNCTION EOCHN(IPT,IMAX,INC)    ! to see if end of chain
IF(INC.GT.0)THEN  ! backwards in time, i.e. down chain as stored
  EOCHN=IPT+1.GT.IMAX
ELSE              ! forwards in time, i.e. from end of chain
  EOCHN=IPT-1.LT.IMAX
ENDIF
RETURN
END FUNCTION EOCHN
