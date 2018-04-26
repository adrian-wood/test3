SUBROUTINE GETREQ(CTYPE,REQ,ILEN,ITIME,TRANGE,IRECV,IFORM,LATEST,&
&ISTAN,ISATID,AREA,CIDENT,ORDER,IOVER,IDTYPE,IVER,&
&ICENT,TEST,MSG,FOUND,ITOD,ISTYP,IFAIL,CERR,UAPART,&
&DDICTNAME,IMODEL,RPOLE,SELECT)                     !2.2

!-----------------------------------------------------------------------
!
! ROUTINE       : GETREQ
!
! PURPOSE       : to decode the users request string
!
! CALLED BY     : MDB
!
! CALLS         : CHKMODID, GETARE, GETDAT, GETKEY, GETSTN, GETSTR
!
! ARGUMENTS     : (1) CTYPE       data subtype
!                 (2) REQ         input request string
!                 (3) ILEN        length of request
!                 (4) ITIME(9)    obs times
!                 (5) TRANGE      sub-period range in minutes
!                 (6) IRECV(10)   cutoff times
!                 (7) IFORM       1 for current 2 for merged
!                 (8) LATEST      logical
!                 (9) ISTAN(50)   station list
!                (10) ISATID(10)  satellite identifiers
!                (11) AREA(5)    lat/lon area
!                (12) CIDENT(50)  character identifiers
!                (13) ORDER       'f'orwards/'b'ackwards/' ' none     !2
!                (14) IOVER       1=land, 2=sea 0 for all
!                (15) IDTYPE      type of identifier
!                (16) IVER        1 for latest version
!                (17) ICENT       current century hour
!                (18) TEST        logical (for diagnostics)
!                (19) MSG         logical (for 1 bufr msg at a time)
!                (20) FOUND(*)    logical array for keywords found
!                (21) ITOD        1 if request in today format
!                (22) ISTYP       1 surface, 2 upair (for stnmas)
!                (23) IFAIL       8 if error detected
!                (24) CERR        error message
!                (25) UAPART      indicator for upper air retrieval.
!                (26) DDICTNAME   Data Dictionary name
!                (27) IMODEL      Merged model dataset pointer
!                (28) RPOLE(2)    Rotated Pole coords form GETARE     !F
!                (29) SELECT(50)  SELECT keyword values             !3
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Author: Richard Weedon$
! $Folder: OpSourceF90$
! $Workfile: getreq.f90$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
!
! Revision 2.3  2003/05/06  08:32:04  08:32:04  usmdb (MetDB account c/o usjh)
! 2.3.  19 May 2003.  Brian Barwell.  Change 58/03.
! Change value of NKEYS from 34 to 35 (omitted from version 2.2).
!
! Revision 2.2  2003/03/06  09:11:09  09:11:09  usmdb (MetDB account c/o usjh)
! Added SELECT keyword section - S.Cox
!
! Revision 2.1  2002/09/04  13:59:26  13:59:26  usmdb (MetDB account c/o usjh)
! Added keyword DATA MERGED - has the same meaning as DATA ARCHIVE.
! Chang to MODEL keyword, so that if the model type is not
! recognised, GETREQ will return with an error instead of
! defaulting to the Global Atmosphere model - S.Cox
!
! Revision 2.0  2001/01/08  11:58:41  11:58:41  usmdb (Generic MetDB account)
! Removed unused argument ENDREQ from calls to GETDAT.
! Added copyright and modified header - S.Cox
!
! Revision 1.6  98/07/23  08:37:18  08:37:18  usmdb (Generic MDB account)
! New keyword RETBUFR for retrieval of BUFR messages
!
! Revision 1.5  98/05/15  10:09:16  10:09:16  usmdb (Generic MDB account)
! Removal of CLIMUK specific code.
!
! Revision 1.4  97/08/04  13:11:00  13:11:00  uspm (Pat McCormack)
! First revisioned version for 1  - with Y2K change
!
! Revision 1.3  1997/05/12 13:24:43  uspm
! Version dated 21-4-97 copied from 1
!
! Revision 1.2  1997/02/12 12:31:15  uspm
! Update dir name in variable head
!
! Revision 1.1  1997/02/11 14:41:39  uspm
! Initial revision
!
! 20-07-98 !H : S.Cox - Addition of new keyword RETBUFR to allow
!             : retrieval of BUFR messages in CREP - S.Cox
!
! 18-05-98 !G : S.Cox - Removal of CLIMUK specific code.
!
! 28-07-97 !F : J.Lewthwaite/S.Cox - New keyword RPOLE. Change call to
!             : GETARE to include RPOLE. Integer variable IAREA has
!             : been changed to real variable AREA. Also change
!             : keyword MODEL section so routine CHKMODID is called
!             : to get a model id code for the model id name given.
!
! 21-04-97 !E : S.Cox - New keywords DDICT and MODEL. DDICT is required
!             : so that a user can pass a test data dictionary name to
!             : the MDB in the request string. MODEL is required so that
!             : a user can specify which merged dataset to retrieve
!             : data from.
!
! 13-12-96 !D : S.Cox - Add a section to determine whether or not the
!             : user has in-line elements or an elements dataset. If
!             : the former, only look for keywords that occur in the
!             : request string (REQ) before and including the keyword
!             : ELEMENTS, not after.
!
! 23-11-96 !C : S.Cox - New keyword COMBINED (keyword 30) for upper air
!             : retrieval. The user can use this to select parts A&B or
!             : C&D.
!
!             : COMBINED PARTAB,   UAPART = 1
!             : COMBINED PARTCD,   UAPART = 2
!
! 08-08-96 !B : S.Cox - New keywords for upper air retrieval.
!
!             : 25 = STANDARD (Also STANDARD PARTA or PARTC)
!             : 26 = SIGNIFICANT (Also SIGNIFICANT PARTB or PARTD)
!             : 27 = FIXED
!             : 28 = MOBILE
!             : 29 = PROCESSED
!
!             : The number of keywords has been increased to 29 and a
!             : argument UAPART has been added.
!
! 27-06-96 !A : S.Cox - Added ISTYPE to the calls to GETSTN and GETSTR
!             : which were mising it. Also added to the PLATFORM section
!             : for STNMAS. It is now possible to use PLATFORM XX to
!             : select a WMO BLOCK.
!
! 10-12-93    : allow sub-period on the START TIME (used with an
!             : increment)
!
! 29-11-93    : new words 'SURFACE' or 'UPAIR' allowed after a list
!             : of identifiers.
!
! 24-05-93    : correct IPOS increment after finding ORDER keyword
!
! 20-05-93    : correction to platform for CLIMUK - must be a list of
!             : integer stations
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2006 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------
! parameter statements
!-----------------------------------------------------------------------

INTEGER     NKEYS

PARAMETER   (NKEYS=35)                                        !2.3

!-----------------------------------------------------------------------
! declare integers
!-----------------------------------------------------------------------

INTEGER   DDPOS        !- pos of 2nd " in ddict name            !E
INTEGER   ENDREQ       !- end pos of CREQ to pass to GETKEY     !D
INTEGER   I            !- general use variable
INTEGER   ICENT        !- current century hour
INTEGER   IDLEN        !- used in GETSTN
INTEGER   IDTYPE       !- type of identifier
INTEGER   IDUM         !- dummy variable
INTEGER   IFAIL        !- status flag from GETREQ
INTEGER   IFORM        !- 1 for current data, 2 for merged
INTEGER   ILEN         !- length of request string
INTEGER   IMODEL       !- model type (so to get right merge data)
INTEGER   INUM         !- number of identifiers in GETSTN
INTEGER   INXT         !- character position of next word
INTEGER   ITIME(9)     !- request times
INTEGER   IRECV(10)    !- cutoff times
INTEGER   ISATID(10)   !- array of satellite ids requested
INTEGER   ISTAN(50)    !- array of stations requested
INTEGER   ISTYP        !- 1 surface, 2 uprair for STNMAS
INTEGER   ITOD         !- 1 if request in TODAY format
INTEGER   IOVER        !- over land/sea
INTEGER   IVER         !- version number of report requested
INTEGER   J            !- general use variable
INTEGER   IPOS         !- position counter in REQ
INTEGER   SELECT(50)   !- SELECT keyword values               !2.2
INTEGER   TRANGE       !- sub-period range
INTEGER   UAPART       !- 0 = A&C or B&D (Upper air retrieval)  !B
                             !- 1 = stand A or sig B or comb AB     !C!B
                             !- 2 = stand C or sig D or comb CD     !C!B

!-----------------------------------------------------------------------
! declare reals
!-----------------------------------------------------------------------

REAL      AREA(5)      !- user-selected area                    !F
REAL      RPOLE(2)     !- Rotated Lat/Long pole coords          !F

!-----------------------------------------------------------------------
! declare logicals
!-----------------------------------------------------------------------

LOGICAL   ELEMDS       !- .FALSE. if in-line elems used         !D
LOGICAL   FOUND(*)     !- array of 'found' keywords
LOGICAL   LATEST       !- keyword LATEST
LOGICAL   MSG          !- keyword MSG
LOGICAL   TEST         !- keyword TEST

!-----------------------------------------------------------------------
! declare characters (in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER*(*) CERR         !- error message
CHARACTER*(*) CIDENT(50)   !- character identifiers array
CHARACTER*20  CMSG         !- message from GETSTN
CHARACTER*(*) CTYPE        !- users subtype
CHARACTER*(*) DDICTNAME    !- Data Dictionary datset name       !E
CHARACTER*15  KEYWORD      !- keyword
CHARACTER*(*) ORDER        !- order keyword
CHARACTER*(*) REQ          !- request string
CHARACTER*80  HEAD         !- revision information              !2

!-----------------------------------------------------------------------
! declare functions
!-----------------------------------------------------------------------

INTEGER   INXKEY       !- return character position of next word

!     SAVE statement not required in this routine                   !2.3

!-----------------------------------------------------------------------
!- Initilialise variables
!-----------------------------------------------------------------------

HEAD='$Workfile: getreq.f90$ ' //&
    &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'

DO J=1,NKEYS
  FOUND(J)=.FALSE.
ENDDO

!-----------------------------------------------------------------------
!- Determine whether or not the user has in-line elements. If so,     !D
!- only pass CREQ up to and including 'ELEMENTS ' to GETKEY
!-----------------------------------------------------------------------

ELEMDS=.TRUE.                                                   !D
ENDREQ=ILEN                                                     !D

I=INDEX(REQ,' ELEMENTS ')                                       !D
IF (I.NE.0) THEN                                                !D
  I=I+9                                                         !D
  IF (REQ(I+1:I+2).NE.'FT' .OR. REQ(I+5:I+8).NE.'F001') THEN    !D
    ELEMDS=.FALSE.                                              !D
  ENDIF                                                         !D
ENDIF                                                           !D

IF (.NOT.ELEMDS) ENDREQ=I                                       !D

!-----------------------------------------------------------------------
!- Loop over keywords
!-----------------------------------------------------------------------

IPOS=1

100   CALL GETKEY(REQ(:ENDREQ),IPOS,ENDREQ,KEYWORD,IFAIL)             !D
IF(IFAIL.EQ.8)THEN  ! keyword not found
  CERR='KEYWORD NOT FOUND'
  GOTO 999   ! exit
ENDIF

!-----------------------------------------------------------------------
!- START TIME keyword
!-----------------------------------------------------------------------

IF(KEYWORD(1:10).EQ.'START TIME')THEN
  IF(FOUND(1))THEN
    CERR='Duplicate keyword: START TIME'                        !2
    IFAIL=8
    GOTO 999
  ENDIF
  CALL GETDAT(REQ(:ENDREQ),IPOS,ICENT,ITIME,ITOD,&          !2.0!D
             &TRANGE,IFAIL,CMSG)
  IF(IFAIL.EQ.8)THEN
    CERR='INVALID START TIME '//CMSG
    GOTO 999
  ENDIF
  FOUND(1)=.TRUE.

!-----------------------------------------------------------------------
!- END TIME keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:8).EQ.'END TIME')THEN
  IF(FOUND(2))THEN
    CERR='Duplicate keyword: END TIME'                          !2
    IFAIL=8
    GOTO 999
  ENDIF
  CALL GETDAT(REQ(:ENDREQ),IPOS,ICENT,ITIME(5),IDUM,&       !2.0!D
             &IDUM,IFAIL,CMSG)
  IF(IFAIL.EQ.8)THEN
    CERR='INVALID END TIME '//CMSG
    GOTO 999
  ELSEIF(IFAIL.EQ.4)THEN
    CERR='INVALID SUB-PERIOD'//CMSG
    IFAIL=8
    GOTO 999
  ENDIF
  FOUND(2)=.TRUE.

!-----------------------------------------------------------------------
!- RECEIVED BEFORE/AFTER/BETWEEN keywords
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:8).EQ.'RECEIVED')THEN
  IF(FOUND(5))THEN
    CERR='Duplicate keyword: RECEIVED'                          !2
    IFAIL=8
    GOTO 999
  ENDIF
  IF(REQ(IPOS:IPOS+5).EQ.'BEFORE')THEN
    IPOS=IPOS+7
    CALL GETDAT(REQ(:ENDREQ),IPOS,ICENT,IRECV(6),IDUM,&     !2.0!D
               &IDUM,IFAIL,CMSG)
    IF(IFAIL.EQ.8)THEN
      CERR='INVALID CUTOFF TIME '//CMSG
      GOTO 999
    ELSEIF(IFAIL.EQ.4)THEN
      CERR='INVALID SUB-PERIOD'//CMSG
      IFAIL=8
      GOTO 999
    ENDIF
    IRECV(10)=MOD(IRECV(9),100)
    IRECV(9)=IRECV(9)/100
  ELSEIF(REQ(IPOS:IPOS+4).EQ.'AFTER')THEN
    IPOS=IPOS+6
    CALL GETDAT(REQ(:ENDREQ),IPOS,ICENT,IRECV(1),IDUM,&     !2.0!D
               &IDUM,IFAIL,CMSG)
    IF(IFAIL.EQ.8)THEN
      CERR='INVALID CUTOFF TIME '//CMSG
      GOTO 999
    ELSEIF(IFAIL.EQ.4)THEN
      CERR='INVALID SUB-PERIOD'//CMSG
      IFAIL=8
      GOTO 999
    ENDIF
    IRECV(5)=MOD(IRECV(4),100)
    IRECV(4)=IRECV(4)/100
  ELSEIF(REQ(IPOS:IPOS+6).EQ.'BETWEEN')THEN
    IPOS=IPOS+8
    CALL GETDAT(REQ(:ENDREQ),IPOS,ICENT,IRECV(1),IDUM,&     !2.0!D
               &IDUM,IFAIL,CMSG)
    IF(IFAIL.EQ.8)THEN
      CERR='INVALID CUTOFF TIME (1) '//CMSG
      GOTO 999
    ELSEIF(IFAIL.EQ.4)THEN
      CERR='INVALID SUB-PERIOD'//CMSG
      IFAIL=8
      GOTO 999
    ENDIF
    IRECV(5)=MOD(IRECV(4),100)
    IRECV(4)=IRECV(4)/100
    IPOS=IPOS+4  ! MOVE PAST 'AND'
    CALL GETDAT(REQ(:ENDREQ),IPOS,ICENT,IRECV(6),IDUM,&     !2.0!D
               &IDUM,IFAIL,CMSG)                                !D
    IF(IFAIL.EQ.8)THEN
      CERR='INVALID CUTOFF TIME (2) '//CMSG
      GOTO 999
    ELSEIF(IFAIL.EQ.4)THEN
      CERR='INVALID SUB-PERIOD'//CMSG
      IFAIL=8
      GOTO 999
    ENDIF
    IRECV(10)=MOD(IRECV(9),100)
    IRECV(9)=IRECV(9)/100
  ELSE
    CERR='INVALID CUTOFF TIME '//CMSG
    IFAIL=8
    GOTO 999
  ENDIF
  FOUND(5)=.TRUE.

!-----------------------------------------------------------------------
!- LATEST keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:6).EQ.'LATEST')THEN
  IF(FOUND(3))THEN
    CERR='Duplicate keyword: LATEST'                            !2
    IFAIL=8
    GOTO 999
  ENDIF
  LATEST=.TRUE.
  FOUND(3)=.TRUE.

!-----------------------------------------------------------------------
!- INCREMENT keyword
!-----------------------------------------------------------------------

 ELSEIF(KEYWORD(1:9).EQ.'INCREMENT')THEN
   IF(FOUND(4))THEN
     CERR='Duplicate keyword: INCREMENT'                         !2
     IFAIL=8
     GOTO 999
   ENDIF
   I=INDEX(REQ(IPOS:ENDREQ),' ')                                 !D
   IF(I.EQ.2)THEN
     READ(REQ(IPOS:IPOS),'(I1)')ITIME(9)
   ELSEIF(I.EQ.3)THEN
     READ(REQ(IPOS:IPOS+1),'(I2)')ITIME(9)
   ELSE
     CERR='INVALID INCREMENT'
     IFAIL=8
     GOTO 999
   ENDIF
   IPOS=IPOS+I
   FOUND(4)=.TRUE.

!-----------------------------------------------------------------------
!- SATELLITE ID keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:12).EQ.'SATELLITE ID')THEN
  IF(FOUND(12))THEN
    CERR='Duplicate keyword: SATELLITE ID'                      !2
    IFAIL=8
    GOTO 999
  ENDIF
  INUM=10
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISATID,INUM,IDLEN,&    !D!A
             &ISTYP,IFAIL,CERR)                                 !D
  IF(IFAIL.EQ.8)THEN
    CERR='INVALID SATELLITE ID'//CMSG
    GOTO 999
  ENDIF
  FOUND(12)=.TRUE.

!-----------------------------------------------------------------------
!- STATION NUMBER keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:14).EQ.'STATION NUMBER')THEN
  IF(FOUND(15))THEN
    CERR='Duplicate keyword: STATION NUMBER'                    !2
    IFAIL=8
    GOTO 999
  ENDIF
  INUM=49
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISTAN(2),INUM,IDLEN,&  !D!A
             &ISTYP,IFAIL,CERR)                                 !D
  IF(IFAIL.EQ.8)THEN
    CERR='INVALID STN NUMBER'//CMSG
    GOTO 999
  ENDIF
  ISTAN(1)=INUM
  FOUND(15)=.TRUE.

!-----------------------------------------------------------------------
!- WMO BLOCK keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:9).EQ.'WMO BLOCK')THEN
  IF(FOUND(16))THEN
    CERR='Duplicate keyword: WMO BLOCK'                         !2
    IFAIL=8
    GOTO 999
  ENDIF
  INUM=1
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISTAN(2),INUM,IDLEN,&  !D!A
             &ISTYP,IFAIL,CERR)
  IF(IFAIL.EQ.8)THEN
    CERR='INVALID WMO BLOCK'//CMSG
    GOTO 999
  ENDIF
  FOUND(16)=.TRUE.

!-----------------------------------------------------------------------
!- WMO ID keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:6).EQ.'WMO ID')THEN
  IF(FOUND(17))THEN
    CERR='Duplicate keyword: WMO ID'                            !2
    IFAIL=8
    GOTO 999
  ENDIF
  INUM=49
  IDTYPE=1
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISTAN(2),INUM,IDLEN,&  !D!A
             &ISTYP,IFAIL,CERR)
  IF(IFAIL.EQ.8)THEN
    CERR='INVALID WMO ID'//CMSG
    GOTO 999
  ENDIF
  ISTAN(1)=INUM
  FOUND(17)=.TRUE.

!-----------------------------------------------------------------------
!- DCNN ID keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:7).EQ.'DCNN ID')THEN
  IF(FOUND(19))THEN
    CERR='Duplicate keyword: DCNN ID'                           !2
    IFAIL=8
    GOTO 999
  ENDIF
  INUM=49
  IDTYPE=3
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISTAN(2),INUM,IDLEN,&  !D!A
             &ISTYP,IFAIL,CERR)
  IF(IFAIL.EQ.8)THEN
    CERR='INVALID WMO ID'//CMSG
    GOTO 999
  ENDIF
  ISTAN(1)=INUM
  FOUND(19)=.TRUE.

!-----------------------------------------------------------------------
!- RAIN ID keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:7).EQ.'RAIN ID')THEN
  IF(FOUND(20))THEN
    CERR='Duplicate keyword: RAIN ID'                           !2
    IFAIL=8
    GOTO 999
  ENDIF
  INUM=49
  IDTYPE=4
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISTAN(2),INUM,IDLEN,&  !D!A
             &ISTYP,IFAIL,CERR)                                 !D
  IF(IFAIL.EQ.8)THEN
    CERR='INVALID RAIN ID'//CMSG
    GOTO 999
  ENDIF
  ISTAN(1)=INUM
  FOUND(20)=.TRUE.

!-----------------------------------------------------------------------
!- ICAO ID keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:7).EQ.'ICAO ID')THEN
  IF(FOUND(18))THEN
    CERR='Duplicate keyword: ICAO ID'                           !2
    IFAIL=8
    GOTO 999
  ENDIF
  INUM=50
  IDTYPE=2
  CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN,&    !D!A
             &ISTYP,IFAIL,CERR)                                 !D
  IF(IFAIL.EQ.8)THEN
    CERR='INVALID ICAO ID'//CMSG
    GOTO 999
  ENDIF
  FOUND(18)=.TRUE.

!-----------------------------------------------------------------------
!- PLATFORM keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:8).EQ.'PLATFORM')THEN
  IF(FOUND(6))THEN
    CERR='Duplicate keyword: PLATFORM'                          !2
    IFAIL=8
    GOTO 999
  ENDIF

!-----------------------------------------------------------------------
!- identifiers must be full for STNMAS to identify the type
!-----------------------------------------------------------------------

  IF(CTYPE(1:6).EQ.'STNMAS')THEN
    IF(REQ(IPOS:IPOS).LE.'Z')THEN
      IDTYPE=2     ! ICAO ID
      INUM=50
      CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,&                    !D
                 &CIDENT,INUM,IDLEN,ISTYP,IFAIL,CERR)
      IF(IFAIL.EQ.8)THEN
        CERR='INVALID PLATFORM '//CMSG
        GOTO 999
      ENDIF
    ELSE ! digits
      INUM=49
      CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,&                    !D
                 &ISTAN(2),INUM,IDLEN,ISTYP,IFAIL,CERR)         !D
      IF(IFAIL.EQ.8)THEN
        CERR='INVALID PLATFORM '//CMSG
        GOTO 999
      ENDIF
      ISTAN(1)=INUM
      IF(IDLEN.EQ.6)THEN
        IDTYPE=4      ! RAIN ID
      ELSEIF(IDLEN.EQ.5)THEN
        IDTYPE=1      ! WMO ID
      ELSEIF(IDLEN.EQ.4)THEN
        IDTYPE=3      ! DCNN ID
      ELSEIF(IDLEN.EQ.2)THEN                   !A
        IDTYPE=1      ! WMO BLOCK              !A
      ELSE
        CERR='INVALID ID LIST'//CMSG
        IFAIL=8
        GOTO 999
      ENDIF
    ENDIF
  ELSE     ! all other types
    INUM=50
    CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN,&    !D
               &ISTYP,IFAIL,CERR)                               !D
    IF(IFAIL.EQ.8)THEN
      CERR='INVALID PLATFORM'//CMSG
      GOTO 999
    ENDIF
  ENDIF
  FOUND(6)=.TRUE.

!-----------------------------------------------------------------------
!- BUOY ID keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:7).EQ.'BUOY ID')THEN
  IF(FOUND(13))THEN
    CERR='Duplicate keyword: BUOY ID'                           !2
    IFAIL=8
    GOTO 999
  ENDIF
  INUM=50
  CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN,&      !D
             &ISTYP,IFAIL,CERR)                                 !D
  IF(IFAIL.EQ.8)THEN
    CERR='INVALID BUOY ID'//CMSG
    GOTO 999
  ENDIF
  FOUND(13)=.TRUE.

!-----------------------------------------------------------------------
!- AIRCRAFT ID keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:11).EQ.'AIRCRAFT ID')THEN
  IF(FOUND(14))THEN
    CERR='Duplicate keyword: AIRCRAFT ID'                       !2
    IFAIL=8
    GOTO 999
  ENDIF
  INUM=50
  CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN,&      !D
             &ISTYP,IFAIL,CERR)                                 !D
  IF(IFAIL.EQ.8)THEN
    CERR='INVALID AIRCRAFT ID'//CMSG
    GOTO 999
  ENDIF
  FOUND(14)=.TRUE.

!-----------------------------------------------------------------------
!- AREA keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:4).EQ.'AREA')THEN
  IF(FOUND(7))THEN
    CERR='Duplicate keyword: AREA'                              !2
    IFAIL=8
    GOTO 999
  ENDIF

!-----------------------------------------------------------------------
!- AREA WMO BLOCK keyword
!-----------------------------------------------------------------------

  IF(REQ(IPOS:IPOS+8).EQ.'WMO BLOCK')THEN
    IPOS=IPOS+10
    IF(CTYPE(1:6).EQ.'STNMAS')THEN
      INUM=49
      IDTYPE=1
      CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISTAN(2),INUM,IDLEN,&!D
                 &ISTYP,IFAIL,CERR)                             !D
      IF(IFAIL.EQ.8)THEN
        CERR='INVALID AREA'//CMSG
        GOTO 999
      ENDIF
      ISTAN(1)=INUM
    ELSE
      INUM=50
      CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN,&  !D
                 &ISTYP,IFAIL,CERR)                             !D
      IF(IFAIL.EQ.8)THEN
        CERR='INVALID AREA'//CMSG
        GOTO 999
      ENDIF
    ENDIF

!-----------------------------------------------------------------------
!- AREA ICAO REGION keyword
!-----------------------------------------------------------------------

  ELSEIF(REQ(IPOS:IPOS+10).EQ.'ICAO REGION')THEN
    IPOS=IPOS+12
    IF(CTYPE(1:6).EQ.'STNMAS')THEN
      INUM=50
      IDTYPE=2
      CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN,&  !D
                 &ISTYP,IFAIL,CERR)                             !D
      IF(IFAIL.EQ.8)THEN
        CERR='INVALID AREA'//CMSG
        GOTO 999
      ENDIF
    ELSE
      INUM=50
      CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN,&  !D
                 &ISTYP,IFAIL,CERR)                             !D
      IF(IFAIL.EQ.8)THEN
        CERR='INVALID AREA'//CMSG
        GOTO 999
      ENDIF
    ENDIF

!-----------------------------------------------------------------------
!- AREA WMO SEA AREA keyword
!-----------------------------------------------------------------------

  ELSEIF(REQ(IPOS:IPOS+11).EQ.'WMO SEA AREA')THEN
    IPOS=IPOS+13
    INUM=50
    CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN,&    !D
               &ISTYP,IFAIL,CERR)                               !D
    IF(IFAIL.EQ.8)THEN
      CERR='INVALID AREA'//CMSG
      GOTO 999
    ENDIF

!-----------------------------------------------------------------------
!- AREA LAT LAT LON LON keyword
!-----------------------------------------------------------------------

  ELSE
    AREA(1)=-9                                                  !F
    CALL GETARE(REQ(:ENDREQ),IPOS,ENDREQ,AREA,RPOLE,IFAIL,&     !F
    &CMSG)
    IF(IFAIL.EQ.8)THEN
      CERR='INVALID AREA'//CMSG
      GOTO 999
    ENDIF
  ENDIF
  FOUND(7)=.TRUE.

!-----------------------------------------------------------------------
!- Rotated POLE
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:5).EQ.'RPOLE') THEN                            !F
  AREA(1)=-4                                                    !F
  CALL GETARE(REQ(:ENDREQ),IPOS,ENDREQ,AREA,RPOLE,IFAIL,CMSG)   !F
  IF (IFAIL .EQ. 8) THEN                                        !F
    CERR='INVALID POLE COORDS'//CMSG                            !F
    GOTO 999                                                    !F
  ENDIF                                                         !F
  FOUND(33)=.TRUE.                                              !F

!-----------------------------------------------------------------------
!- OVER keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:4).EQ.'OVER')THEN
  IF(FOUND(8))THEN
    CERR='Duplicate keyword: OVER'                              !2
    IFAIL=8
    GOTO 999
  ENDIF
  IF(REQ(IPOS:IPOS+3).EQ.'LAND')THEN
    IOVER=1
    IPOS=IPOS+5
  ELSEIF(REQ(IPOS:IPOS+2).EQ.'SEA')THEN
    IOVER=2
    IPOS=IPOS+4
  ELSE
    IOVER=0
  ENDIF
  FOUND(8)=.TRUE.

!-----------------------------------------------------------------------
!- VERSION keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:7).EQ.'VERSION')THEN
  IF(FOUND(9))THEN
    CERR='Duplicate keyword: VERSION'                           !2
    IFAIL=8
    GOTO 999
  ENDIF
  IF(REQ(IPOS:IPOS+5).EQ.'LATEST')THEN
    IVER=1
    IPOS=IPOS+7
  ELSEIF(REQ(IPOS:IPOS+8).EQ.'PREFERRED')THEN
    IVER=1
    IPOS=IPOS+10
  ELSEIF(REQ(IPOS:IPOS+2).EQ.'ALL')THEN
    IVER=2
    IPOS=IPOS+4
  ELSEIF(REQ(IPOS:IPOS+6).EQ.'LONGEST')THEN
    IVER=3
    IPOS=IPOS+8
  ELSE
    CERR='INVALID VERSION '
    IFAIL=8
    GOTO 999
  ENDIF
  FOUND(9)=.TRUE.

!-----------------------------------------------------------------------
!- DATA keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:4).EQ.'DATA')THEN
  IF(FOUND(10))THEN
    CERR='Duplicate keyword: DATA'                              !2
    IFAIL=8
    GOTO 999
  ENDIF
  IF(REQ(IPOS:IPOS+6).EQ.'CURRENT')THEN
    IFORM=1
    IPOS=IPOS+8
  ELSEIF(REQ(IPOS:IPOS+6).EQ.'ARCHIVE')THEN
    IFORM=2
    IPOS=IPOS+8
  ELSEIF(REQ(IPOS:IPOS+5).EQ.'MERGED')THEN                    !2.1
    IFORM=2                                                   !2.1
    IPOS=IPOS+7                                               !2.1
  ELSE
    CERR='INVALID DATA KEYWORD'
    IFAIL=8
    GOTO 999
  ENDIF
  FOUND(10)=.TRUE.

!-----------------------------------------------------------------------
!- ORDER keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:5).EQ.'ORDER')THEN
  IF(FOUND(22))THEN
    CERR='Duplicate keyword: ORDER'                             !2
    IFAIL=8
    GOTO 999
  ENDIF
  IF(REQ(IPOS:IPOS+7).EQ.'BACKWARD')THEN
    ORDER='B'
    IPOS=IPOS+9
  ELSEIF(REQ(IPOS:IPOS+6).EQ.'FORWARD')THEN
    ORDER='F'
    IPOS=IPOS+8
  ELSEIF(REQ(IPOS:IPOS+3).EQ.'NONE')THEN                        !2
    ORDER=' '                                                   !2
    IPOS=IPOS+5                                                 !2
  ELSE
    CERR='ORDER INVALID'
    IFAIL=8
    GOTO 999
  ENDIF
  FOUND(22)=.TRUE.

!-----------------------------------------------------------------------
!- MESSAGE keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:7).EQ.'MESSAGE')THEN
  IF(FOUND(24))THEN
    CERR='Duplicate keyword: MESSAGE'                           !2
    IFAIL=8
    GOTO 999
  ENDIF
  MSG=.TRUE.
  FOUND(24)=.TRUE.

!-----------------------------------------------------------------------
!- TEST keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:4).EQ.'TEST')THEN
  IF(FOUND(23))THEN
    CERR='Duplicate keyword: TEST'                              !2
    IFAIL=8
    GOTO 999
  ENDIF
  TEST=.TRUE.
  FOUND(23)=.TRUE.

!-----------------------------------------------------------------------
!- STANDARD keyword                                                   !B
!-----------------------------------------------------------------------
                                                                      !B
ELSEIF(KEYWORD(1:8).EQ.'STANDARD')THEN                          !B
  IF(FOUND(25))THEN                                             !B
    CERR='Duplicate keyword: STANDARD'                          !2
    IFAIL=8                                                     !B
    GOTO 999                                                    !B
  ENDIF                                                         !B
  UAPART=0                                                      !B
  IF(REQ(IPOS:IPOS+4).EQ.'PARTA')THEN                           !B
    UAPART=1                                                    !B
    IPOS=IPOS+6                                                 !B
  ELSEIF(REQ(IPOS:IPOS+4).EQ.'PARTC')THEN                       !B
    UAPART=2                                                    !B
    IPOS=IPOS+6                                                 !B
  ENDIF                                                         !B
  FOUND(25)=.TRUE.                                              !B

!-----------------------------------------------------------------------
!- SIGNIFICANT keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:8).EQ.'COMBINED')THEN                          !C
  IF(FOUND(30))THEN                                             !C
    CERR='Duplicate keyword: COMBINED'                          !2
    IFAIL=8                                                     !C
    GOTO 999                                                    !C
  ENDIF                                                         !C
  UAPART=0                                                      !C
  IF(REQ(IPOS:IPOS+5).EQ.'PARTAB')THEN                          !C
    UAPART=1                                                    !C
    IPOS=IPOS+7                                                 !C
  ELSEIF(REQ(IPOS:IPOS+5).EQ.'PARTCD')THEN                      !C
    UAPART=2                                                    !C
    IPOS=IPOS+7                                                 !C
  ENDIF                                                         !C
  FOUND(30)=.TRUE.                                              !C

!-----------------------------------------------------------------------
!- COMBINED keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:11).EQ.'SIGNIFICANT')THEN                      !B
  IF(FOUND(26))THEN                                             !B
    CERR='Duplicate keyword: SIGNIFICANT'                       !2
    IFAIL=8                                                     !B
    GOTO 999                                                    !B
  ENDIF                                                         !B
  UAPART=0                                                      !B
  IF(REQ(IPOS:IPOS+4).EQ.'PARTB')THEN                           !B
    UAPART=1                                                    !B
    IPOS=IPOS+6                                                 !B
  ELSEIF(REQ(IPOS:IPOS+4).EQ.'PARTD')THEN                       !B
    UAPART=2                                                    !B
    IPOS=IPOS+6                                                 !B
  ENDIF                                                         !B
  FOUND(26)=.TRUE.                                              !B

!-----------------------------------------------------------------------
!- FIXED keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:5).EQ.'FIXED')THEN                             !B
  IF(FOUND(27))THEN                                             !B
    CERR='Duplicate keyword: FIXED'                             !2
    IFAIL=8                                                     !B
    GOTO 999                                                    !B
  ENDIF                                                         !B
  FOUND(27)=.TRUE.                                              !B

!-----------------------------------------------------------------------
!- MOBILE keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:6).EQ.'MOBILE')THEN                            !B
  IF(FOUND(28))THEN                                             !B
    CERR='Duplicate keyword: MOBILE'                            !2
    IFAIL=8                                                     !B
    GOTO 999                                                    !B
  ENDIF                                                         !B
  FOUND(28)=.TRUE.                                              !B

!-----------------------------------------------------------------------
!- PROCESSED keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:9).EQ.'PROCESSED')THEN                         !B
  IF(FOUND(29))THEN                                             !B
    CERR='Duplicate keyword: PROCESSED'                         !2
    IFAIL=8                                                     !B
    GOTO 999                                                    !B
  ENDIF                                                         !B
  FOUND(29)=.TRUE.                                              !B

!-----------------------------------------------------------------------
!- DDICT keyword                                                      !E
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:5).EQ.'DDICT')THEN
  IF(FOUND(31))THEN
    CERR='Duplicate keyword: DDICT'                             !2
    IFAIL=8
    GOTO 999
  ENDIF
  FOUND(31)=.TRUE.

  IF (REQ(IPOS:IPOS).EQ.'"') THEN
    DDPOS=INDEX(REQ(IPOS+1:ENDREQ),'"')
    IF (DDPOS.GT.0) THEN
      DDICTNAME(2:DDPOS)=REQ(IPOS+1:IPOS+DDPOS-1)
      IPOS=IPOS+DDPOS+2
    ELSE
      WRITE(6,*)'In GETREQ: Cannot find DDICTNAME 2nd quote!'
      IFAIL=8
      GOTO 999
    ENDIF
  ELSE
    WRITE(6,*)'In GETREQ: Cannot find DDICTNAME 1st quote!'
    IFAIL=8
    GOTO 999
  ENDIF

!-----------------------------------------------------------------------
!- MODEL keyword                                                      !E
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:5).EQ.'MODEL')THEN
  IF(FOUND(32))THEN
    CERR='Duplicate keyword: MODEL'                             !2
    IFAIL=8
    GOTO 999
  ENDIF

  I=INDEX(REQ(IPOS:),' ')                                       !F
  IF (I.NE.0) THEN                                              !F
    CALL CHKMODID(REQ(IPOS:IPOS+I-2),I-1,IMODEL)                !F
  ENDIF                                                         !F

  IF (IMODEL.EQ.0) THEN                                         !F
    WRITE(6,*)'MDB ERROR: GETREQ: MODEL TYPE ',&              !2.1
   &REQ(IPOS:IPOS+I-2),' NOT RECOGNISED'                      !2.1
    CERR='SEE ERROR MESSAGE ABOVE'                            !2.1
    IFAIL=8                                                   !2.1
    RETURN                                                    !2.1
  ENDIF                                                         !F

  IPOS=IPOS+I                                                   !F

  FOUND(32)=.TRUE.

!-----------------------------------------------------------------------
!- RETBUFR keyword
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:7).EQ.'RETBUFR')THEN                           !H
  IF(FOUND(34))THEN                                             !H
    CERR='Duplicate keyword: RETBUFR'                           !2
    IFAIL=8                                                     !H
    GOTO 999                                                    !H
  ENDIF                                                         !H
  FOUND(34)=.TRUE.                                              !H

!-----------------------------------------------------------------------
!- SELECT keyword
!-----------------------------------------------------------------------

ELSEIF (KEYWORD(1:6).EQ.'SELECT') THEN                        !2.2
  IF (FOUND(35)) THEN                                         !2.2
    CERR='Duplicate keyword: SELECT'                            !2
    IFAIL=8                                                   !2.2
    RETURN                                                    !2.2
  ENDIF                                                       !2.2
  FOUND(35)=.TRUE.                                            !2.2
  INUM=50                                                     !2.2
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,SELECT,INUM,IDLEN,&    !2.2
             &ISTYP,IFAIL,CERR)                               !2.2
  IF (IFAIL.EQ.8) THEN                                        !2.2
    CERR='INVALID SELECT VALS 1' //CMSG                       !2.2
    RETURN                                                    !2.2
  ENDIF                                                       !2.2

!-----------------------------------------------------------------------
!- ELEMENTS keyword (don't process elements here - just bypass)
!-----------------------------------------------------------------------

ELSEIF(KEYWORD(1:8).EQ.'ELEMENTS')THEN
  INXT=INXKEY(REQ(:ENDREQ),IPOS,ENDREQ)                         !D
  IF(INXT.NE.IPOS)THEN
    IPOS=INXT
  ELSE
    IPOS=ENDREQ                                                 !D
  ENDIF
  FOUND(11)=.TRUE.
ENDIF

!-----------------------------------------------------------------------
!- next keyword
!-----------------------------------------------------------------------

IF(IPOS.LT.ENDREQ) GOTO 100                                     !D

999   RETURN
END SUBROUTINE GETREQ
