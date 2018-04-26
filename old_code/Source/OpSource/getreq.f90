SUBROUTINE GETREQ(CTYPE,REQ,ILEN,ITIME,TRANGE,IRECV,IFORM,LATEST, &
           ISTAN,ISATID,AREA,CIDENT,ORDER,IOVER,IDTYPE,IVER, &
           ICENT,TEST,MSG,FOUND,ITOD,ISTYP,IFAIL,CERR,UAPART, &
           DDICTNAME,IMODEL,RPOLE,SELECT)

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
!                (13) ORDER       'f'orwards/'b'ackwards/' ' none
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
!                (28) RPOLE(2)    Rotated Pole coords form GETARE
!                (29) SELECT(50)  SELECT keyword values
!
! REVISION INFO :
!
! $Revision: 6$
! $Date: 22/11/2010 16:42:57$
! $Author: Stan Kellett$
! $Folder: OpSource$
! $Workfile: getreq.f90$
!
! CHANGE RECORD :
!
! $Log:
!  6    MetDB_Refresh 1.5         22/11/2010 16:42:57    Stan Kellett    use
!       inxkey changed to use inxkey_mod
!  5    MetDB_Refresh 1.4         17/11/2010 15:52:55    John Norton     After
!       merge batch 8 changes
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:15:41    John Norton     
!  1    MetDB_Refresh 1.0         13/10/2010 16:00:38    John Norton     f77
!       code copied out of StarTeam
!       MIDE\Met_DB_Project\Development\Source\OpSource
! $
!
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

use chkmodid_mod
use getare_mod
use getdat_mod
use getkey_mod
use getstn_mod
use getstr_mod
use inxkey_mod      !function to return character position of next word

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(INOUT)     :: CTYPE !- users subtype
CHARACTER(*), INTENT(INOUT)     :: REQ !- request string
INTEGER, INTENT(INOUT)          :: ILEN !- length of                    request string
INTEGER, INTENT(INOUT)          ::  ITIME(9) !- request times
INTEGER, INTENT(INOUT)          ::  TRANGE !- sub-period range
INTEGER, INTENT(INOUT)          ::  IRECV(10) !- cutoff times
INTEGER, INTENT(INOUT)          ::  IFORM !- 1 for current data, 2 for merged
LOGICAL, INTENT(INOUT)          ::  LATEST !- keyword LATEST
INTEGER, INTENT(INOUT)          ::  ISTAN(50) !- array of stations requested
INTEGER, INTENT(INOUT)          ::  ISATID(10) !- array of satellite ids requested
REAL, INTENT(INOUT)             ::  AREA(5) !- user-selected area
CHARACTER(*), INTENT(INOUT)     ::  CIDENT(50) !- character identifie   rs array
CHARACTER(*), INTENT(INOUT)     ::  ORDER !- order keyword
INTEGER, INTENT(INOUT)          ::  IOVER !- over land/sea
INTEGER, INTENT(INOUT)          ::  IDTYPE !- type of identifier
INTEGER, INTENT(INOUT)          ::  IVER !- version number of report requested
INTEGER, INTENT(INOUT)          ::  ICENT !- current century hour
LOGICAL, INTENT(INOUT)          ::  TEST !- keyword TEST
LOGICAL, INTENT(INOUT)          ::  MSG !- keyword MSG
LOGICAL, INTENT(INOUT)          ::  FOUND(:) !- array of 'found' keywords
INTEGER, INTENT(INOUT)          ::  ITOD !- 1 if request in TODAY format
INTEGER, INTENT(INOUT)          ::  ISTYP !- 1 surface, 2 uprair for STNMAS
INTEGER, INTENT(INOUT)          ::  IFAIL !- status flag from GETREQ
CHARACTER(*), INTENT(INOUT)     ::  CERR !- error message
INTEGER, INTENT(INOUT)          ::  UAPART !- 0 = A&C or B&D (Upper air retrieval)
CHARACTER(*), INTENT(INOUT)     ::  DDICTNAME !- Data Dictionary dats   et name
INTEGER, INTENT(INOUT)          ::  IMODEL !- model type (so to get right merge data)
REAL, INTENT(INOUT)             ::  RPOLE(2) !- Rotated Lat/Long pole coords
INTEGER, INTENT(INOUT)          ::  SELECT(50) !- SELECT keyword values

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>


!-----------------------------------------------------------------------
! parameter statements
!-----------------------------------------------------------------------

INTEGER, PARAMETER          ::  NKEYS = 35


!-----------------------------------------------------------------------
! declare integers
!-----------------------------------------------------------------------

INTEGER      ::  DDPOS !- pos of 2nd " in ddict name
INTEGER      ::  ENDREQ !- end pos of CREQ to pass to GETKEY
INTEGER      ::  I     !- general use variable
INTEGER      ::  IDLEN !- used in GETSTN
INTEGER      ::  IDUM  !- dummy variable
INTEGER      ::  INUM  !- number of identifiers in GETSTN
INTEGER      ::  INXT  !- character position of next word
INTEGER      ::  J     !- general use variable
INTEGER      ::  IPOS  !- position counter in REQ
                       !- 1 = stand A or sig B or comb AB
                       !- 2 = stand C or sig D or comb CD

!-----------------------------------------------------------------------
! declare reals
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
! declare logicals
!-----------------------------------------------------------------------

LOGICAL      ::  ELEMDS !- .FALSE. if in-line elems used

!-----------------------------------------------------------------------
! declare characters (in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER(20) :: CMSG   !- message from GETSTN
CHARACTER(15) :: KEYWORD !- keyword

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------
!     SAVE statement not required in this routine

!-----------------------------------------------------------------------
!- Initilialise variables
!-----------------------------------------------------------------------

DO J=1,NKEYS
  FOUND(J)=.FALSE.
END DO

!-----------------------------------------------------------------------
!- Determine whether or not the user has in-line elements. If so,
!- only pass CREQ up to and including 'ELEMENTS ' to GETKEY
!-----------------------------------------------------------------------

ELEMDS=.TRUE.
ENDREQ=ILEN

I=INDEX(REQ,' ELEMENTS ')
IF (I /= 0) THEN
  I=I+9
  IF (REQ(I+1:I+2) /= 'FT' .OR. REQ(I+5:I+8) /= 'F001') THEN
    ELEMDS=.FALSE.
  END IF
END IF

IF (.NOT.ELEMDS) ENDREQ=I

!-----------------------------------------------------------------------
!- Loop over keywords
!-----------------------------------------------------------------------

IPOS=1

100   CALL GETKEY(REQ(:ENDREQ),IPOS,ENDREQ,KEYWORD,IFAIL)
IF(IFAIL == 8)THEN  ! keyword not found
  CERR='KEYWORD NOT FOUND'
  GOTO 999   ! exit
END IF

!-----------------------------------------------------------------------
!- START TIME keyword
!-----------------------------------------------------------------------

IFLABEL1: &
IF(KEYWORD(1:10) == 'START TIME')THEN
  IF(FOUND(1))THEN
    CERR='Duplicate keyword: START TIME'
    IFAIL=8
    GOTO 999
  END IF
 CALL GETDAT(REQ(:ENDREQ),IPOS,ICENT,ITIME,ITOD, &
              TRANGE,IFAIL,CMSG)
  IF(IFAIL == 8)THEN
    CERR='INVALID START TIME '//CMSG
    GOTO 999
  END IF
  FOUND(1)=.TRUE.

!-----------------------------------------------------------------------
!- END TIME keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:8) == 'END TIME')THEN
  IF(FOUND(2))THEN
    CERR='Duplicate keyword: END TIME'
    IFAIL=8
    GOTO 999
  END IF
  CALL GETDAT(REQ(:ENDREQ),IPOS,ICENT,ITIME(5),IDUM, &
              IDUM,IFAIL,CMSG)
  IF(IFAIL == 8)THEN
    CERR='INVALID END TIME '//CMSG
    GOTO 999
  ELSE IF(IFAIL == 4)THEN
    CERR='INVALID SUB-PERIOD'//CMSG
    IFAIL=8
    GOTO 999
  END IF
  FOUND(2)=.TRUE.

!-----------------------------------------------------------------------
!- RECEIVED BEFORE/AFTER/BETWEEN keywords
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:8) == 'RECEIVED')THEN
  IF(FOUND(5))THEN
    CERR='Duplicate keyword: RECEIVED'
    IFAIL=8
    GOTO 999
  END IF
IFLABEL2: &
  IF(REQ(IPOS:IPOS+5) == 'BEFORE')THEN
    IPOS=IPOS+7
    CALL GETDAT(REQ(:ENDREQ),IPOS,ICENT,IRECV(6),IDUM, &
                IDUM,IFAIL,CMSG)
    IF(IFAIL == 8)THEN
      CERR='INVALID CUTOFF TIME '//CMSG
      GOTO 999
    ELSE IF(IFAIL == 4)THEN
      CERR='INVALID SUB-PERIOD'//CMSG
      IFAIL=8
      GOTO 999
    END IF
    IRECV(10)=MOD(IRECV(9),100)
    IRECV(9)=IRECV(9)/100
  ELSE IF(REQ(IPOS:IPOS+4) == 'AFTER')THEN
    IPOS=IPOS+6
    CALL GETDAT(REQ(:ENDREQ),IPOS,ICENT,IRECV(1),IDUM, &
                IDUM,IFAIL,CMSG)
    IF(IFAIL == 8)THEN
      CERR='INVALID CUTOFF TIME '//CMSG
      GOTO 999
    ELSE IF(IFAIL == 4)THEN
      CERR='INVALID SUB-PERIOD'//CMSG
      IFAIL=8
      GOTO 999
    END IF
    IRECV(5)=MOD(IRECV(4),100)
    IRECV(4)=IRECV(4)/100
  ELSE IF(REQ(IPOS:IPOS+6) == 'BETWEEN')THEN
    IPOS=IPOS+8
    CALL GETDAT(REQ(:ENDREQ),IPOS,ICENT,IRECV(1),IDUM, &
                IDUM,IFAIL,CMSG)
    IF(IFAIL == 8)THEN
      CERR='INVALID CUTOFF TIME (1) '//CMSG
      GOTO 999
    ELSE IF(IFAIL == 4)THEN
      CERR='INVALID SUB-PERIOD'//CMSG
      IFAIL=8
      GOTO 999
    END IF
    IRECV(5)=MOD(IRECV(4),100)
    IRECV(4)=IRECV(4)/100
    IPOS=IPOS+4  ! MOVE PAST 'AND'
    CALL GETDAT(REQ(:ENDREQ),IPOS,ICENT,IRECV(6),IDUM, &
                IDUM,IFAIL,CMSG)
    IF(IFAIL == 8)THEN
      CERR='INVALID CUTOFF TIME (2) '//CMSG
      GOTO 999
    ELSE IF(IFAIL == 4)THEN
      CERR='INVALID SUB-PERIOD'//CMSG
      IFAIL=8
      GOTO 999
    END IF
    IRECV(10)=MOD(IRECV(9),100)
    IRECV(9)=IRECV(9)/100
  ELSE
    CERR='INVALID CUTOFF TIME '//CMSG
    IFAIL=8
    GOTO 999
  END IF IFLABEL2
  FOUND(5)=.TRUE.

!-----------------------------------------------------------------------
!- LATEST keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:6) == 'LATEST')THEN
  IF(FOUND(3))THEN
    CERR='Duplicate keyword: LATEST'
    IFAIL=8
    GOTO 999
  END IF
  LATEST=.TRUE.
  FOUND(3)=.TRUE.

!-----------------------------------------------------------------------
!- INCREMENT keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:9) == 'INCREMENT')THEN
  IF(FOUND(4))THEN
    CERR='Duplicate keyword: INCREMENT'
    IFAIL=8
    GOTO 999
  END IF
  I=INDEX(REQ(IPOS:ENDREQ),' ')
  IF(I == 2)THEN
    READ(REQ(IPOS:IPOS),'(I1)')ITIME(9)
  ELSE IF(I == 3)THEN
    READ(REQ(IPOS:IPOS+1),'(I2)')ITIME(9)
  ELSE
    CERR='INVALID INCREMENT'
    IFAIL=8
    GOTO 999
  END IF
  IPOS=IPOS+I
  FOUND(4)=.TRUE.

!-----------------------------------------------------------------------
!- SATELLITE ID keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:12) == 'SATELLITE ID')THEN
  IF(FOUND(12))THEN
    CERR='Duplicate keyword: SATELLITE ID'
    IFAIL=8
    GOTO 999
  END IF
  INUM=10
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISATID,INUM,IDLEN, &
              ISTYP,IFAIL,CERR)
  IF(IFAIL == 8)THEN
    CERR='INVALID SATELLITE ID'//CMSG
    GOTO 999
  END IF
  FOUND(12)=.TRUE.

!-----------------------------------------------------------------------
!- STATION NUMBER keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:14) == 'STATION NUMBER')THEN
  IF(FOUND(15))THEN
    CERR='Duplicate keyword: STATION NUMBER'
    IFAIL=8
    GOTO 999
  END IF
  INUM=49
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISTAN(2:),INUM,IDLEN, &
              ISTYP,IFAIL,CERR)
  IF(IFAIL == 8)THEN
    CERR='INVALID STN NUMBER'//CMSG
    GOTO 999
  END IF
  ISTAN(1)=INUM
  FOUND(15)=.TRUE.

!-----------------------------------------------------------------------
!- WMO BLOCK keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:9) == 'WMO BLOCK')THEN
  IF(FOUND(16))THEN
    CERR='Duplicate keyword: WMO BLOCK'
    IFAIL=8
    GOTO 999
  END IF
  INUM=1
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISTAN(2:),INUM,IDLEN, &
              ISTYP,IFAIL,CERR)
  IF(IFAIL == 8)THEN
    CERR='INVALID WMO BLOCK'//CMSG
    GOTO 999
  END IF
  FOUND(16)=.TRUE.

!-----------------------------------------------------------------------
!- WMO ID keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:6) == 'WMO ID')THEN
  IF(FOUND(17))THEN
    CERR='Duplicate keyword: WMO ID'
    IFAIL=8
    GOTO 999
  END IF
  INUM=49
  IDTYPE=1
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISTAN(2:),INUM,IDLEN, &
              ISTYP,IFAIL,CERR)
  IF(IFAIL == 8)THEN
    CERR='INVALID WMO ID'//CMSG
    GOTO 999
  END IF
  ISTAN(1)=INUM
  FOUND(17)=.TRUE.

!-----------------------------------------------------------------------
!- DCNN ID keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:7) == 'DCNN ID')THEN
  IF(FOUND(19))THEN
    CERR='Duplicate keyword: DCNN ID'
    IFAIL=8
    GOTO 999
  END IF
  INUM=49
  IDTYPE=3
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISTAN(2:),INUM,IDLEN, &
              ISTYP,IFAIL,CERR)
  IF(IFAIL == 8)THEN
    CERR='INVALID WMO ID'//CMSG
    GOTO 999
  END IF
  ISTAN(1)=INUM
  FOUND(19)=.TRUE.

!-----------------------------------------------------------------------
!- RAIN ID keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:7) == 'RAIN ID')THEN
  IF(FOUND(20))THEN
    CERR='Duplicate keyword: RAIN ID'
    IFAIL=8
    GOTO 999
  END IF
  INUM=49
  IDTYPE=4
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISTAN(2:),INUM,IDLEN, &
              ISTYP,IFAIL,CERR)
  IF(IFAIL == 8)THEN
    CERR='INVALID RAIN ID'//CMSG
    GOTO 999
  END IF
  ISTAN(1)=INUM
  FOUND(20)=.TRUE.

!-----------------------------------------------------------------------
!- ICAO ID keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:7) == 'ICAO ID')THEN
  IF(FOUND(18))THEN
    CERR='Duplicate keyword: ICAO ID'
    IFAIL=8
    GOTO 999
  END IF
  INUM=50
  IDTYPE=2
  CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN, &
              ISTYP,IFAIL,CERR)
  IF(IFAIL == 8)THEN
    CERR='INVALID ICAO ID'//CMSG
    GOTO 999
  END IF
  FOUND(18)=.TRUE.

!-----------------------------------------------------------------------
!- PLATFORM keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:8) == 'PLATFORM')THEN
  IF(FOUND(6))THEN
    CERR='Duplicate keyword: PLATFORM'
    IFAIL=8
    GOTO 999
  END IF

!-----------------------------------------------------------------------
!- identifiers must be full for STNMAS to identify the type
!-----------------------------------------------------------------------

IFLABEL3: &
  IF(CTYPE(1:6) == 'STNMAS')THEN
IFLABEL4: &
    IF(REQ(IPOS:IPOS) <= 'Z')THEN
      IDTYPE=2     ! ICAO ID
      INUM=50
      CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ, &
                  CIDENT,INUM,IDLEN,ISTYP,IFAIL,CERR)
      IF(IFAIL == 8)THEN
        CERR='INVALID PLATFORM '//CMSG
        GOTO 999
      END IF
    ELSE ! digits
      INUM=49
      CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ, &
                  ISTAN(2:),INUM,IDLEN,ISTYP,IFAIL,CERR)
      IF(IFAIL == 8)THEN
        CERR='INVALID PLATFORM '//CMSG
        GOTO 999
      END IF
      ISTAN(1)=INUM
IFLABEL5: &
      IF(IDLEN == 6)THEN
        IDTYPE=4      ! RAIN ID
      ELSE IF(IDLEN == 5)THEN
        IDTYPE=1      ! WMO ID
      ELSE IF(IDLEN == 4)THEN
        IDTYPE=3      ! DCNN ID
      ELSE IF(IDLEN == 2)THEN                  
        IDTYPE=1      ! WMO BLOCK              
      ELSE
        CERR='INVALID ID LIST'//CMSG
        IFAIL=8
        GOTO 999
      END IF IFLABEL5
    END IF IFLABEL4
  ELSE     ! all other types
    INUM=50
    CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN, &
                ISTYP,IFAIL,CERR)
    IF(IFAIL == 8)THEN
      CERR='INVALID PLATFORM'//CMSG
      GOTO 999
    END IF
  END IF IFLABEL3
  FOUND(6)=.TRUE.

!-----------------------------------------------------------------------
!- BUOY ID keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:7) == 'BUOY ID')THEN
  IF(FOUND(13))THEN
    CERR='Duplicate keyword: BUOY ID'
    IFAIL=8
    GOTO 999
  END IF
  INUM=50
  CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN, &
              ISTYP,IFAIL,CERR)
  IF(IFAIL == 8)THEN
    CERR='INVALID BUOY ID'//CMSG
    GOTO 999
  END IF
  FOUND(13)=.TRUE.

!-----------------------------------------------------------------------
!- AIRCRAFT ID keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:11) == 'AIRCRAFT ID')THEN
  IF(FOUND(14))THEN
    CERR='Duplicate keyword: AIRCRAFT ID'
    IFAIL=8
    GOTO 999
  END IF
  INUM=50
  CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN, &
              ISTYP,IFAIL,CERR)
  IF(IFAIL == 8)THEN
    CERR='INVALID AIRCRAFT ID'//CMSG
    GOTO 999
  END IF
  FOUND(14)=.TRUE.

!-----------------------------------------------------------------------
!- AREA keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:4) == 'AREA')THEN
  IF(FOUND(7))THEN
    CERR='Duplicate keyword: AREA'
    IFAIL=8
    GOTO 999
  END IF

!-----------------------------------------------------------------------
!- AREA WMO BLOCK keyword
!-----------------------------------------------------------------------

IFLABEL6: &
  IF(REQ(IPOS:IPOS+8) == 'WMO BLOCK')THEN
    IPOS=IPOS+10
IFLABEL7: &
    IF(CTYPE(1:6) == 'STNMAS')THEN
      INUM=49
      IDTYPE=1
      CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,ISTAN(2:),INUM,IDLEN, &
                  ISTYP,IFAIL,CERR)
      IF(IFAIL == 8)THEN
        CERR='INVALID AREA'//CMSG
        GOTO 999
      END IF
      ISTAN(1)=INUM
    ELSE
      INUM=50
      CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN, &
                  ISTYP,IFAIL,CERR)
      IF(IFAIL == 8)THEN
        CERR='INVALID AREA'//CMSG
        GOTO 999
      END IF
    END IF IFLABEL7

!-----------------------------------------------------------------------
!- AREA ICAO REGION keyword
!-----------------------------------------------------------------------

  ELSE IF(REQ(IPOS:IPOS+10) == 'ICAO REGION')THEN
    IPOS=IPOS+12
IFLABEL8: &
    IF(CTYPE(1:6) == 'STNMAS')THEN
      INUM=50
      IDTYPE=2
      CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN, &
                  ISTYP,IFAIL,CERR)
      IF(IFAIL == 8)THEN
        CERR='INVALID AREA'//CMSG
        GOTO 999
      END IF
    ELSE
      INUM=50
      CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN, &
                  ISTYP,IFAIL,CERR)
      IF(IFAIL == 8)THEN
        CERR='INVALID AREA'//CMSG
        GOTO 999
      END IF
    END IF IFLABEL8

!-----------------------------------------------------------------------
!- AREA WMO SEA AREA keyword
!-----------------------------------------------------------------------

  ELSE IF(REQ(IPOS:IPOS+11) == 'WMO SEA AREA')THEN
    IPOS=IPOS+13
    INUM=50
    CALL GETSTR(REQ(:ENDREQ),IPOS,ENDREQ,CIDENT,INUM,IDLEN, &
                ISTYP,IFAIL,CERR)
    IF(IFAIL == 8)THEN
      CERR='INVALID AREA'//CMSG
      GOTO 999
    END IF

!-----------------------------------------------------------------------
!- AREA LAT LAT LON LON keyword
!-----------------------------------------------------------------------

  ELSE
    AREA(1)=-9
    CALL GETARE(REQ(:ENDREQ),IPOS,ENDREQ,AREA,RPOLE,IFAIL, &
    CMSG)
    IF(IFAIL == 8)THEN
      CERR='INVALID AREA'//CMSG
      GOTO 999
    END IF
  END IF IFLABEL6
  FOUND(7)=.TRUE.

!-----------------------------------------------------------------------
!- Rotated POLE
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:5) == 'RPOLE') THEN
  AREA(1)=-4
  CALL GETARE(REQ(:ENDREQ),IPOS,ENDREQ,AREA,RPOLE,IFAIL,CMSG)
  IF (IFAIL  ==  8) THEN
    CERR='INVALID POLE COORDS'//CMSG
    GOTO 999
  END IF
  FOUND(33)=.TRUE.

!-----------------------------------------------------------------------
!- OVER keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:4) == 'OVER')THEN
  IF(FOUND(8))THEN
    CERR='Duplicate keyword: OVER'
    IFAIL=8
    GOTO 999
  END IF
  IF(REQ(IPOS:IPOS+3) == 'LAND')THEN
    IOVER=1
    IPOS=IPOS+5
  ELSE IF(REQ(IPOS:IPOS+2) == 'SEA')THEN
    IOVER=2
    IPOS=IPOS+4
  ELSE
    IOVER=0
  END IF
  FOUND(8)=.TRUE.

!-----------------------------------------------------------------------
!- VERSION keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:7) == 'VERSION')THEN
  IF(FOUND(9))THEN
    CERR='Duplicate keyword: VERSION'
    IFAIL=8
    GOTO 999
  END IF
IFLABEL9: &
  IF(REQ(IPOS:IPOS+5) == 'LATEST')THEN
    IVER=1
    IPOS=IPOS+7
  ELSE IF(REQ(IPOS:IPOS+8) == 'PREFERRED')THEN
    IVER=1
    IPOS=IPOS+10
  ELSE IF(REQ(IPOS:IPOS+2) == 'ALL')THEN
    IVER=2
    IPOS=IPOS+4
  ELSE IF(REQ(IPOS:IPOS+6) == 'LONGEST')THEN
    IVER=3
    IPOS=IPOS+8
  ELSE
    CERR='INVALID VERSION '
    IFAIL=8
    GOTO 999
  END IF IFLABEL9
  FOUND(9)=.TRUE.

!-----------------------------------------------------------------------
!- DATA keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:4) == 'DATA')THEN
  IF(FOUND(10))THEN
    CERR='Duplicate keyword: DATA'
    IFAIL=8
    GOTO 999
  END IF
IFLABEL10: &
  IF(REQ(IPOS:IPOS+6) == 'CURRENT')THEN
    IFORM=1
    IPOS=IPOS+8
  ELSE IF(REQ(IPOS:IPOS+6) == 'ARCHIVE')THEN
    IFORM=2
    IPOS=IPOS+8
  ELSE IF(REQ(IPOS:IPOS+5) == 'MERGED')THEN
    IFORM=2
    IPOS=IPOS+7
  ELSE
    CERR='INVALID DATA KEYWORD'
    IFAIL=8
    GOTO 999
  END IF IFLABEL10
  FOUND(10)=.TRUE.

!-----------------------------------------------------------------------
!- ORDER keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:5) == 'ORDER')THEN
  IF(FOUND(22))THEN
    CERR='Duplicate keyword: ORDER'
    IFAIL=8
    GOTO 999
  END IF
IFLABEL11: &
  IF(REQ(IPOS:IPOS+7) == 'BACKWARD')THEN
    ORDER='B'
    IPOS=IPOS+9
  ELSE IF(REQ(IPOS:IPOS+6) == 'FORWARD')THEN
    ORDER='F'
    IPOS=IPOS+8
  ELSE IF(REQ(IPOS:IPOS+3) == 'NONE')THEN
    ORDER=' '
    IPOS=IPOS+5
  ELSE
    CERR='ORDER INVALID'
    IFAIL=8
    GOTO 999
  END IF IFLABEL11
  FOUND(22)=.TRUE.

!-----------------------------------------------------------------------
!- MESSAGE keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:7) == 'MESSAGE')THEN
  IF(FOUND(24))THEN
    CERR='Duplicate keyword: MESSAGE'
    IFAIL=8
    GOTO 999
  END IF
  MSG=.TRUE.
  FOUND(24)=.TRUE.

!-----------------------------------------------------------------------
!- TEST keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:4) == 'TEST')THEN
  IF(FOUND(23))THEN
    CERR='Duplicate keyword: TEST'
    IFAIL=8
    GOTO 999
  END IF
  TEST=.TRUE.
  FOUND(23)=.TRUE.

!-----------------------------------------------------------------------
!- STANDARD keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:8) == 'STANDARD')THEN
  IF(FOUND(25))THEN
    CERR='Duplicate keyword: STANDARD'
    IFAIL=8
    GOTO 999
  END IF
  UAPART=0
  IF(REQ(IPOS:IPOS+4) == 'PARTA')THEN
    UAPART=1
    IPOS=IPOS+6
  ELSE IF(REQ(IPOS:IPOS+4) == 'PARTC')THEN
    UAPART=2
    IPOS=IPOS+6
  END IF
  FOUND(25)=.TRUE.

!-----------------------------------------------------------------------
!- SIGNIFICANT keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:8) == 'COMBINED')THEN
  IF(FOUND(30))THEN
    CERR='Duplicate keyword: COMBINED'
    IFAIL=8
    GOTO 999
  END IF
  UAPART=0
  IF(REQ(IPOS:IPOS+5) == 'PARTAB')THEN
    UAPART=1
    IPOS=IPOS+7
  ELSE IF(REQ(IPOS:IPOS+5) == 'PARTCD')THEN
    UAPART=2
    IPOS=IPOS+7
  END IF
  FOUND(30)=.TRUE.

!-----------------------------------------------------------------------
!- COMBINED keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:11) == 'SIGNIFICANT')THEN
  IF(FOUND(26))THEN
    CERR='Duplicate keyword: SIGNIFICANT'
    IFAIL=8
    GOTO 999
  END IF
  UAPART=0
  IF(REQ(IPOS:IPOS+4) == 'PARTB')THEN
    UAPART=1
    IPOS=IPOS+6
  ELSE IF(REQ(IPOS:IPOS+4) == 'PARTD')THEN
    UAPART=2
    IPOS=IPOS+6
  END IF
  FOUND(26)=.TRUE.

!-----------------------------------------------------------------------
!- FIXED keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:5) == 'FIXED')THEN
  IF(FOUND(27))THEN
    CERR='Duplicate keyword: FIXED'
    IFAIL=8
    GOTO 999
  END IF
  FOUND(27)=.TRUE.

!-----------------------------------------------------------------------
!- MOBILE keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:6) == 'MOBILE')THEN
  IF(FOUND(28))THEN
    CERR='Duplicate keyword: MOBILE'
    IFAIL=8
    GOTO 999
  END IF
  FOUND(28)=.TRUE.

!-----------------------------------------------------------------------
!- PROCESSED keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:9) == 'PROCESSED')THEN
  IF(FOUND(29))THEN
    CERR='Duplicate keyword: PROCESSED'
    IFAIL=8
    GOTO 999
  END IF
  FOUND(29)=.TRUE.

!-----------------------------------------------------------------------
!- DDICT keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:5) == 'DDICT')THEN
  IF(FOUND(31))THEN
    CERR='Duplicate keyword: DDICT'
    IFAIL=8
    GOTO 999
  END IF
  FOUND(31)=.TRUE.

IFLABEL12: &
  IF (REQ(IPOS:IPOS) == '"') THEN
    DDPOS=INDEX(REQ(IPOS+1:ENDREQ),'"')
    IF (DDPOS > 0) THEN
      DDICTNAME(2:DDPOS)=REQ(IPOS+1:IPOS+DDPOS-1)
      IPOS=IPOS+DDPOS+2
    ELSE
      WRITE(6,*)'In GETREQ: Cannot find DDICTNAME 2nd quote!'
      IFAIL=8
      GOTO 999
    END IF
  ELSE
    WRITE(6,*)'In GETREQ: Cannot find DDICTNAME 1st quote!'
    IFAIL=8
    GOTO 999
  END IF IFLABEL12

!-----------------------------------------------------------------------
!- MODEL keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:5) == 'MODEL')THEN
  IF(FOUND(32))THEN
    CERR='Duplicate keyword: MODEL'
    IFAIL=8
    GOTO 999
  END IF

  I=INDEX(REQ(IPOS:),' ')
  IF (I /= 0) THEN
    CALL CHKMODID(REQ(IPOS:IPOS+I-2),I-1,IMODEL)
  END IF

  IF (IMODEL == 0) THEN
    WRITE(6,*)'MDB ERROR: GETREQ: MODEL TYPE ', &
    REQ(IPOS:IPOS+I-2),' NOT RECOGNISED'
    CERR='SEE ERROR MESSAGE ABOVE'
    IFAIL=8
    RETURN
  END IF

  IPOS=IPOS+I

  FOUND(32)=.TRUE.

!-----------------------------------------------------------------------
!- RETBUFR keyword
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:7) == 'RETBUFR')THEN
  IF(FOUND(34))THEN
    CERR='Duplicate keyword: RETBUFR'
    IFAIL=8
    GOTO 999
  END IF
  FOUND(34)=.TRUE.

!-----------------------------------------------------------------------
!- SELECT keyword
!-----------------------------------------------------------------------

ELSE IF (KEYWORD(1:6) == 'SELECT') THEN
  IF (FOUND(35)) THEN
    CERR='Duplicate keyword: SELECT'
    IFAIL=8
    RETURN
  END IF
  FOUND(35)=.TRUE.
  INUM=50
  CALL GETSTN(REQ(:ENDREQ),IPOS,ENDREQ,SELECT,INUM,IDLEN, &
              ISTYP,IFAIL,CERR)
  IF (IFAIL == 8) THEN
    CERR='INVALID SELECT VALS 1' //CMSG
    RETURN
  END IF

!-----------------------------------------------------------------------
!- ELEMENTS keyword (don't process elements here - just bypass)
!-----------------------------------------------------------------------

ELSE IF(KEYWORD(1:8) == 'ELEMENTS')THEN
  INXT=INXKEY(REQ(:ENDREQ),IPOS,ENDREQ)
  IF(INXT /= IPOS)THEN
    IPOS=INXT
  ELSE
    IPOS=ENDREQ
  END IF
  FOUND(11)=.TRUE.
END IF IFLABEL1

!-----------------------------------------------------------------------
!- next keyword
!-----------------------------------------------------------------------

IF(IPOS < ENDREQ) GOTO 100

999   RETURN
END SUBROUTINE GETREQ
