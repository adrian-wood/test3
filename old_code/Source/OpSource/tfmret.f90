SUBROUTINE TFMRET(CTYPE,ITIME,TRANGE,IRTIM,AREA,CIDENT, &
                  ARRAY,NOBS,NELEM,IOBS,IDESC,NDES,     &
                  ISTAT,IERR,IDSK,CSTR,CREPRT,LFLAG,    &
                  CHAREP,ORDER,LATEST,TAFLEN,METSPECI,  &
                  FOUND,RPOLE,IVER)

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
!               : ICHAR2 (FUNCTION)
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
! ARGUMENTS     :
!  ALL THESE ARGUMENTS HAVE BEEN VALIDATED IN MDB SO INVALID
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
!      NDES     INTEGER    NUMBER OF SUBSCRIPTS (NOT DESCRIPTORS)
!      AREA(5)  REAL    (1) 0.0 FOR LAT/LON AREA -2.0 FOR GLOBAL
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
!      TAFLEN   INTEGER  1 FOR LONG, 2 FOR SHORT TAF, 0 FOR BOTH
!      METSPECI INTEGER  0 FOR METAR, 3 FOR SPECI
!      FOUND    LOGICAL(*) KEYWORDS FOUND OR NOT
!      RPOLE(2) REAL       Rotated pole lat/lon
!      IVER     INTEGER    1 for preferred, 2 for all
!
! REVISION INFO :
!
! $Workfile: tfmret.f90$ $Folder: OpSource$
! $Revision: 8$ $Date: 22/08/2011 16:01:44$
!
! CHANGE RECORD :
!
! $Log:
!  8    MetDB_Refresh 1.7         22/08/2011 16:01:44    Brian Barwell   For
!       TROPADV bulletins, allow for >1 entry for a station in each index
!       period.
!  7    MetDB_Refresh 1.6         09/02/2011 17:26:57    Sheila Needham  Use
!       int2ch function
!  6    MetDB_Refresh 1.5         20/12/2010 16:00:19    Sheila Needham
!       Correct INTENTS
!  5    MetDB_Refresh 1.4         23/11/2010 10:04:09    Stan Kellett
!       deleted internal function eochn as taken now from external file.
!       Removed function declarations as declared in mod files.
!  4    MetDB_Refresh 1.3         18/11/2010 16:28:12    Brian Barwell
!       Obsolete block of code deleted and other changes after testing.
!  3    MetDB_Refresh 1.2         10/11/2010 16:16:01    John Norton     MetDB
!       Refresh: Changed EOCHN back to external function.
!  2    MetDB_Refresh 1.1         10/11/2010 14:57:35    John Norton     MetDB
!       Refresh: Updated after review comments 
!  1    MetDB_Refresh 1.0         04/11/2010 13:25:18    John Norton     MetDB
!       Refresh batch 8 before reviewing.
! $
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

USE condes_mod
USE int2ch_mod
USE dt2hrs_mod
USE eochn_mod
USE expreq_mod
USE hrs2dt_mod
USE ichar2_mod
USE idmtch_mod
USE maprd_mod
USE mtrexp_mod
USE mtrint_mod
USE ncmexp_mod
USE ncmint_mod
USE rotarea_mod
USE sethed_mod
USE sortch_mod
USE srwexp_mod
USE srwint_mod
USE subper_mod
USE tafint_mod
USE tafexp_mod
USE trnsfr_mod
USE trpint_mod
USE valarea_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(IN) ::  CTYPE
INTEGER,      INTENT(IN) ::  ITIME(9) !
INTEGER,      INTENT(IN) ::  TRANGE !
INTEGER,      INTENT(IN) ::  IRTIM(10) !
REAL,         INTENT(IN) ::  AREA(5) !- user-defined area
CHARACTER(9), INTENT(IN) ::  CIDENT(50)
INTEGER,      INTENT(INOUT) ::  NOBS !
INTEGER,      INTENT(INOUT) ::  NELEM !
REAL,         INTENT(INOUT) ::  ARRAY(NOBS,NELEM) !- users array
INTEGER,      INTENT(INOUT) ::  IOBS !
INTEGER,      INTENT(INOUT) ::  NDES !
INTEGER,      INTENT(INOUT) ::  IDESC(NDES) !
INTEGER,      INTENT(INOUT) ::  ISTAT !
INTEGER,      INTENT(OUT) ::  IERR !
INTEGER,      INTENT(INOUT) ::  IDSK(5) !
CHARACTER(*), INTENT(INOUT) ::  CSTR(NOBS)
CHARACTER(*), INTENT(INOUT) ::  CREPRT(NOBS)
LOGICAL,      INTENT(IN) ::  LFLAG
LOGICAL,      INTENT(IN) ::  CHAREP
CHARACTER(1), INTENT(IN) ::  ORDER
LOGICAL,      INTENT(IN) ::  LATEST
INTEGER,      INTENT(IN) ::  TAFLEN !
INTEGER,      INTENT(IN) ::  METSPECI !
LOGICAL,      INTENT(IN) ::  FOUND(:)
REAL,         INTENT(INOUT) ::  RPOLE(2) !- rotated pole lat/lon
INTEGER,      INTENT(IN) ::  IVER !

! Local declarations:

!-----------------------------------------------------------------------
! Parameter statements
!-----------------------------------------------------------------------

INTEGER,     PARAMETER ::  MDES = 500      !
INTEGER,     PARAMETER ::  MDATA = 1263    !
INTEGER,     PARAMETER ::  INDLEN = 23     !
INTEGER,     PARAMETER ::  IHDLEN = 44     !
INTEGER,     PARAMETER ::  MXINDX = 1020   !
INTEGER,     PARAMETER ::  MXCHLEN = 100   !
INTEGER,     PARAMETER ::  MXRPLEN = 27990 !

!-----------------------------------------------------------------------
! Declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

INTEGER      ::  I          !
INTEGER      ::  IBLOCK     !
INTEGER      ::  ICHL1      !
INTEGER      ::  ICHL2      !
INTEGER      ::  ICHL3      !
INTEGER      ::  ICMIN      !
INTEGER      ::  ID         !
INTEGER      ::  ID2        ! day (end of index block)
INTEGER      ::  IDAT       !
INTEGER      ::  IDATHR     !
INTEGER      ::  IDAY       !
INTEGER      ::  IDHR       !
INTEGER      ::  IDISP(MDES) !
INTEGER      ::  IDREC      !
INTEGER      ::  IENT1      !
INTEGER      ::  IFAIL      !
INTEGER      ::  IH         !
INTEGER      ::  IH2        ! hour (end of index block)
INTEGER      ::  IHL        !
INTEGER      ::  IHOUR      !
INTEGER      ::  II         !
INTEGER      ::  IM         !
INTEGER      ::  IM2        ! month (end of index block)
INTEGER      ::  IMAX       !
INTEGER      ::  IMD        !
INTEGER      ::  IMIN       !
INTEGER      ::  INCR       !
INTEGER      ::  INDHR1     !
INTEGER      ::  INDHR2     !
INTEGER      ::  INUM       !
INTEGER      ::  INXBLK     !
INTEGER      ::  INXHRS     !
INTEGER      ::  IOB        !
INTEGER      ::  IRCHR      !
INTEGER      ::  IRECLN     !
INTEGER      ::  IREP1      !
INTEGER      ::  IRLEN      !
INTEGER      ::  IRTIM1     ! century mins of T.O.R start time
INTEGER      ::  IRTIM2     ! century mins of T.O.R end time
INTEGER      ::  ISECT1(9)  !
INTEGER      ::  ISHR1      !
INTEGER      ::  ISHR2      !
INTEGER      ::  ISLOTHR    !
INTEGER      ::  ISTEP      !
INTEGER      ::  ITHR(3)    !
INTEGER      ::  ITIMND     ! century mins of end time
INTEGER      ::  ITIMST     ! century mins of start time
INTEGER      ::  ITIM1      ! century hour of start time
INTEGER      ::  ITIM2      ! century hour of end time
INTEGER      ::  ITORM      !
INTEGER      ::  ITRIES     !
INTEGER      ::  IY         !
INTEGER      ::  IY2        ! year (end of index block)
INTEGER      ::  J1         !
INTEGER      ::  J2         !
INTEGER      ::  J3         !
INTEGER      ::  J5         !
INTEGER      ::  K          !
INTEGER      ::  K1         !
INTEGER      ::  K2         !
INTEGER      ::  KEPTIM     !
INTEGER      ::  KOUNT      !
INTEGER      ::  KREP       !- count of reports
INTEGER      ::  LAT        !
INTEGER      ::  LON        !
INTEGER      ::  METTYP     !
INTEGER      ::  NAC        !
INTEGER      ::  NAMD       !
INTEGER      ::  NBLOCK     !
INTEGER      ::  NB2RD      !
INTEGER      ::  NCHN       !
INTEGER      ::  NCOR       !
INTEGER      ::  NELMIX     !
INTEGER      ::  NLAT       !- index entry latitude
INTEGER      ::  NLON       !- index entry longitude
INTEGER      ::  NREC       !
INTEGER      ::  NREP       !
INTEGER      ::  NSEND      !
INTEGER      ::  NSQ        !- 1 or 0 if Local Desc or not
INTEGER      ::  NTOR       !
INTEGER      ::  NXTCHN     !
INTEGER      ::  NXTENT     !
INTEGER      ::  NXTIME     !
INTEGER      ::  NXTREP     !
INTEGER      ::  RC         !- general return code
INTEGER      ::  TAFTYP     !

!-----------------------------------------------------------------------
! Declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

LOGICAL      ::  AREA_FLAG !- TRUE if ob in users area
LOGICAL      ::  CHANRED
LOGICAL      ::  EXPAND
LOGICAL      ::  INDXRED
LOGICAL      ::  ISPREF !- TRUE if preferred flag is set
LOGICAL      ::  LCLIM !- TRUE if CLIMAT data wanted
LOGICAL      ::  LCONT
LOGICAL      ::  LDONE
LOGICAL      ::  LMIDMSG ! set if no room for this ob in array
LOGICAL      ::  LOCDFG !- local table D flag
LOGICAL      ::  NTRYCHK
LOGICAL      ::  PASSED
LOGICAL      ::  READ_CHNREPS !- TRUE if still chain reports to read
LOGICAL      ::  SKIP  ! set if ob skipped in reading chain
LOGICAL      ::  WASPREF !- TRUE if report was once preferred

!-----------------------------------------------------------------------
! Declare real variables (in alphabetical order)
!-----------------------------------------------------------------------

REAL         ::  RLAT      !- index entry latitude (real)               i
REAL         ::  RLON      !- index entry longitude (real)
REAL         ::  ROT_LAT   !- rotated lat
REAL         ::  ROT_LON   !- rotated lon
REAL         ::  VALUES(MDATA) !- DEBUFR values array

!-----------------------------------------------------------------------
! Declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER(4)       ::  BULCOD
CHARACTER(4)       ::  CCCC
CHARACTER(20000)   ::  CNAM
CHARACTER(MXRPLEN) ::  CREP
CHARACTER(MXRPLEN) ::  KMSG
CHARACTER(INDLEN)  ::  CNTRY(5*3*MXINDX) ! index entries wanted
CHARACTER(5)       ::  CTAF(2)
CHARACTER(INDLEN)  ::  CTRAIL(MXCHLEN)
CHARACTER(INDLEN)  ::  CTRAN(5*MXINDX) ! index entries read in
CHARACTER(IHDLEN)  ::  HEADER
CHARACTER(9)       ::  LASTID
CHARACTER(23)      ::  MASK             !- for index entry sort
CHARACTER(9)       ::  THISID
CHARACTER(INDLEN)  ::  TRAILX      ! copy of ctrail

!-----------------------------------------------------------------------
! Dynamic common, compile on IBM mainframe with FARMS='DC(*)'
!-----------------------------------------------------------------------

COMMON /TFMCOM1/ CNAM,KMSG,CREP,CNTRY,CTRAIL,CTRAN
COMMON /TFMCOM2/ IDISP,VALUES

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
DATA MASK   /'  XXXXXXXXX    '/    !- mask for ident sort

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------


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

IFLABEL1: &
IF (CTYPE(1:4) == 'TAFS' .OR. CTYPE(1:5) == 'ATAFS') THEN
  IF (NDES <= 18) EXPAND=EXPREQ(IDESC,NDES)
ELSE IF (CTYPE(1:6) ==  'METARS') THEN
  IF (NDES <= 17) EXPAND=EXPREQ(IDESC,NDES)
ELSE IF (CTYPE(1:4) ==  'SREW' .OR. CTYPE(1:3)  ==  'NCM') THEN
  IF (NDES <= 16) EXPAND=EXPREQ(IDESC,NDES)
END IF IFLABEL1

LCLIM = (CTYPE(1:6) == 'CLIMAT')

!-----------------------------------------------------------------------
! check for continuation and set up loop counters
!-----------------------------------------------------------------------

IFLABEL2: &
IF (ISTAT == 16) THEN
  if (lflag) write(*,*)'in tfmret: new dataset'
  ISTAT = 0
ELSE IF (ISTAT == 4) THEN
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

  if (lflag) write(6,9001)ishr1,ient1,irep1,ichl1,nsend
END IF IFLABEL2

IFLABEL3: &
IF (ISTAT == 0) THEN
  if (lflag) then
    write(6,9002)(idesc(k),k=1,ndes)
    print*,'request area:',area
    print*,'request  ids:',cident
    print*,'request  time',itime
    print*,'taf type',taflen
    print*,'options (l,ord,raw)',latest,order,charep
  END IF
  IF (NELEM < NDES) THEN
    IERR=16
    WRITE(6,9012) NOBS,NDES
    GO TO 999
  END IF

!-----------------------------------------------------------------------
! Read map block to find dataset details
! (Assumes map is in record 1 even if data set has >12000 records)
!-----------------------------------------------------------------------

  CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,LOCDFG, &
             NELMIX, IBLOCK, INUM, CTRAN, IDATHR, IDREC,     &
             IRECLN, KMSG, 'MAPR1')

  if (lflag) write(*,*)'in tfmret:  locdfg = ',locdfg

  IF (LOCDFG) THEN
    NSQ=1
  ELSE
    NSQ=0
  END IF

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
  END IF

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

  IF (LCLIM) THEN
    ISHR1=ISHR1-24
    ISHR2=ISHR2-24
    INDHR1=0
    INDHR2=0
  END IF

!-----------------------------------------------------------------------
! if latest is requested, then look back through a max of 3 indexes
! from the end of the request.  print a warning if the user is
! trying to look through too long a period.
!-----------------------------------------------------------------------

IFLABEL4: &
  IF (LATEST) THEN
IFLABEL5: &
    IF (FOUND(1)) THEN ! TIME RANGE GIVEN

!-----------------------------------------------------------------------
! number of blocks needed
!-----------------------------------------------------------------------

      NB2RD = (ISHR2-ISHR1)/INXHRS + 1
      if (lflag) print*,' number of blocks needed ',nb2rd
      IF (NB2RD > 3) THEN
        WRITE(6,9004) INXHRS*3,(ITIME(K),K=5,8)
        NB2RD = 3
      END IF
    ELSE
      NB2RD = 3
    END IF IFLABEL5

!-----------------------------------------------------------------------
! set start to end time so that there's only one loop over hours
!-----------------------------------------------------------------------

    ISHR1 = ISHR2
  END IF IFLABEL4

!-----------------------------------------------------------------------
! decide on looping order (doesn't matter for latest because indexes
! are sorted anyway)
!-----------------------------------------------------------------------

  IF (ORDER == 'B') THEN
    I     = ISHR1
    ISHR1 = ISHR2
    ISHR2 = I
    ISTEP = -INXHRS
  ELSE
    ISTEP = INXHRS
  END IF

  if (lflag) write(6,9005)inxblk,inxhrs,islothr,indhr1, &
                   indhr2,ishr1,ishr2,istep

!-----------------------------------------------------------------------
! convert requested time of receipt to century minutes
!-----------------------------------------------------------------------

  IF (IRTIM(1) == 0) THEN
    IRTIM1 = 0
  ELSE
    IRTIM1 = DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4))
    IRTIM1 = (IRTIM1)*60+IRTIM(5)
  END IF
  IF (IRTIM(6) == 0) THEN
    IRTIM2 = 0
  ELSE
    IRTIM2 = DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9))
    IRTIM2 = (IRTIM2)*60+IRTIM(10)
  END IF
  if (lflag) print*,'irtim1/2:',irtim1,irtim2

!-----------------------------------------------------------------------
! initialise loop counters
!-----------------------------------------------------------------------

  IENT1   = 1   ! ENTRY NUMBER
  IREP1   = 1   ! REPORT WITHIN MESSAGE
  NXTREP  = 1
  NSEND   = 0
  LMIDMSG = .FALSE. ! no ob waiting for user to call again
  LCONT   = .FALSE.
  LDONE   = .FALSE. ! SET TRUE WHEN ELEMENTS HAVE BEEN MAPPED
  INDXRED = .FALSE. ! INDEX HAS NOT BEEN READ
  NTRYCHK = .FALSE. ! ENTRY HAS NOT BEEN CHECKED
  CHANRED = .FALSE. ! CHAIN HAS NOT BEEN READ
  LASTID  = ' '
  PASSED  = .FALSE.
END IF IFLABEL3

ISTAT = 0

!=======================================================================
!
! LOOP OVER INDEX BLOCKS
! AT INDEX LEVEL CHECK FOR VALID INDEX BLOCK TIME
!
!=======================================================================

if (lflag) print*,' looping over times ',ishr1,ishr2,istep

DOLABEL1: &
DO J1=ISHR1,ISHR2,ISTEP

!-----------------------------------------------------------------------
! check whether index has already been read
!-----------------------------------------------------------------------

IFLABEL6: &
IF(.NOT.INDXRED)THEN
IFLABEL7: &
  IF(LATEST)THEN
    if(lflag)print*,' reading',nb2rd,'index blocks'

!-----------------------------------------------------------------------
! ishr1=ishr2 so 799 only loops once.
! read up to 3 index blocks consecutively, starting with the latest.
!-----------------------------------------------------------------------

    IHL=J1
    KOUNT=0
DOLABEL2: &
    DO J2=1,NB2RD

      IBLOCK=MOD(IHL/INXHRS,INXBLK)+2+NSQ

      CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,LOCDFG, &
                 NELMIX, IBLOCK, INUM, CTRAN, IDATHR, IDREC,     &
                 IRECLN, KMSG, 'IDXRD')

!-----------------------------------------------------------------------
! check that correct date/time has been read
!-----------------------------------------------------------------------

      IDAY=IDATHR/256
      IHOUR=IDATHR-IDAY*256
      CALL HRS2DT(IY,IM,ID,IH,IHL)
IFLABEL8: &
      IF(ID /= IDAY.OR.IH /= IHOUR)THEN
        CALL HRS2DT(IY2,IM2,ID2,IH2,IHL+INXHRS-1)
        WRITE(6,9015)CTYPE,IY,IM,ID,IH,IY2,IM2,ID2,IH2
      ELSE
        ITHR(J2)=IHL

!-----------------------------------------------------------------------
! select index entries, setting last byte in identifier field to
! indicate which index block this report has come from (1,2 or 3)
!-----------------------------------------------------------------------

DOLABEL3: &
        DO J3=1,INUM                  ! ADD ENTRIES FROM BLOCK

!-----------------------------------------------------------------------
! check identifier
!-----------------------------------------------------------------------

          CALL IDMTCH(CTRAN(J3),THISID,CIDENT,LFLAG,RC)
          IF (RC /= 0) GO TO 80

!-----------------------------------------------------------------------
! Check if we want to rotate the Lat and Long values from the index
! entry for a rotated grid and then check it falls within the grid area
! or if we just want to check the observation lies within a normal lat
! long area or whether we want all obs (no checking)
! This can be done by looking at AREA(1). If = 0 then normal grid
!                                            = 1 then rotated grid
!                                            = anything else so no chk
!-----------------------------------------------------------------------

          NLAT=ICHAR(CTRAN(J3)(13:13))*256 + ICHAR(CTRAN(J3)(14:14))
          NLON=ICHAR(CTRAN(J3)(15:15))*256 + ICHAR(CTRAN(J3)(16:16))

          IF (NLAT > 32768) NLAT=NLAT-65536
          IF (NLON > 32768) NLON=NLON-65536

          RLAT=NLAT*0.01
          RLON=NLON*0.01

          IF (AREA(1) == 1.0) THEN
            CALL ROTAREA(RLAT,RLON,ROT_LAT,ROT_LON,RPOLE)
            CALL VALAREA(ROT_LAT,ROT_LON,AREA,AREA_FLAG,LFLAG)
          ELSE IF (AREA(1) == 0.0) THEN
            CALL VALAREA(RLAT,RLON,AREA,AREA_FLAG,LFLAG)
          ELSE
            AREA_FLAG=.TRUE.
          END IF

IFLABEL9: &
          IF (AREA_FLAG) THEN

!-----------------------------------------------------------------------
! check if there are any tafs of the right type in the chain
! (taftyp is 1 or 2 if only long or short tafs, 3 if both)
!-----------------------------------------------------------------------

            IF ((CTYPE(1:4) == 'TAFS' .OR. CTYPE(1:5) == 'ATAFS')  &
                .AND. TAFLEN > 0) THEN
              TAFTYP=MOD(ICHAR(CTRAN(J3)(17:17)),4)
              IF (TAFTYP < 3 .AND. TAFTYP /= TAFLEN) GO TO 80
            END IF

!-----------------------------------------------------------------------
! Check for METARs and/or SPECIs in the chain.
! If METARs only         then METTYP is 0.
! If SPECIs only or both then METTYP is 3.
! If SPECIs wanted but only METARs in the chain, then skip the chain.
!-----------------------------------------------------------------------

            IF (CTYPE(1:6) == 'METARS') THEN
              METTYP=MOD(ICHAR(CTRAN(J3)(17:17)),4)
              IF (METSPECI == 3 .AND. METTYP == 0) GO TO 80
            END IF

            if(lflag)print*,thisid,' wanted for further tests'

            KOUNT=KOUNT+1
            CNTRY(KOUNT)=CTRAN(J3)
            CNTRY(KOUNT)(11:11)=int2ch(J2) ! (J2=1,2,3 FOR BLOCK)

!-----------------------------------------------------------------------
! if only one ident requested & whole strings match, stop looking.
! (latest index first, so jump out of loop, not just this index block)
!-----------------------------------------------------------------------

            IF (CIDENT(2) == '00000' .AND. &
                CIDENT(1)(1:8) == CTRAN(J3)(3:10)) GO TO 131

          END IF IFLABEL9 !- area_flag

80        CONTINUE
        END DO DOLABEL3

      END IF IFLABEL8

!-----------------------------------------------------------------------
! decrement hour
!-----------------------------------------------------------------------

      IHL=IHL-INXHRS
130   CONTINUE
    END DO DOLABEL2 ! END OF LOOP OVER INDEX BLOCKS
131 ITRIES=KOUNT
    if (lflag) print *,itries,'entries accepted for ithr',ithr
  ELSE       ! END OF 'LATEST' BLOCK

!-----------------------------------------------------------------------
! if not latest, read a single index block for this period
!-----------------------------------------------------------------------

    IBLOCK=MOD(J1/INXHRS,INXBLK)+2+NSQ

    CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,LOCDFG, &
               NELMIX, IBLOCK, INUM, CTRAN, IDATHR, IDREC,     &
               IRECLN, KMSG, 'IDXRD')
    ITRIES=0
DOLABEL4: &
    DO J3=1,INUM                  ! ADD ENTRIES FROM BLOCK
      CALL IDMTCH(CTRAN(J3),THISID,CIDENT,LFLAG,RC)
      IF (RC /= 0) GO TO 81

!-----------------------------------------------------------------------
! Check if we want to rotate the Lat and Long values from the index
! entry for a rotated grid and then check it falls within the grid area
! or if we just want to check the observation lies within a normal lat
! long area or whether we want all obs (no checking)
! This can be done by looking at AREA(1). If = 0 then normal grid
!                                            = 1 then rotated grid
!                                            = anything else so no chk
!-----------------------------------------------------------------------

      NLAT=ICHAR(CTRAN(J3)(13:13))*256 + ICHAR(CTRAN(J3)(14:14))
      NLON=ICHAR(CTRAN(J3)(15:15))*256 + ICHAR(CTRAN(J3)(16:16))

      IF (NLAT > 32768) NLAT=NLAT-65536
      IF (NLON > 32768) NLON=NLON-65536

      RLAT=NLAT*0.01
      RLON=NLON*0.01

      IF (AREA(1) == 1.0) THEN
        CALL ROTAREA(RLAT,RLON,ROT_LAT,ROT_LON,RPOLE)
        CALL VALAREA(ROT_LAT,ROT_LON,AREA,AREA_FLAG,LFLAG)
      ELSE IF (AREA(1) == 0.0) THEN
        CALL VALAREA(RLAT,RLON,AREA,AREA_FLAG,LFLAG)
      ELSE
        AREA_FLAG=.TRUE.
      END IF

IFLABEL10: &
      IF (AREA_FLAG) THEN

        IF ((CTYPE(1:4) == 'TAFS' .OR. CTYPE(1:5) == 'ATAFS') .AND. &
            TAFLEN > 0) THEN
          TAFTYP=MOD(ICHAR(CTRAN(J3)(17:17)),4)
          IF (TAFTYP < 3 .AND. TAFTYP /= TAFLEN) GO TO 81
        END IF
        if(lflag)print*,thisid,' wanted for further tests'
        ITRIES=ITRIES+1
        CNTRY(ITRIES)=CTRAN(J3)

!-----------------------------------------------------------------------
! Check for METARs and/or SPECIs in the chain.
! If METARs only         then METTYP is 0.
! If SPECIs only or both then METTYP is 3.
! If SPECIs wanted but only METARs in the chain, then skip the chain.
!-----------------------------------------------------------------------

        IF (CTYPE(1:6) == 'METARS') THEN
          METTYP=MOD(ICHAR(CTRAN(J3)(17:17)),4)
          IF (METSPECI == 3 .AND. METTYP == 0) GO TO 81
        END IF

!-----------------------------------------------------------------------
! If only 1 identifier requested & whole strings match, stop looking.
! (Does not apply to TROPADV beacuse reports are not chained.)       !8
!-----------------------------------------------------------------------

        IF (CTYPE(1:7) /= 'TROPADV' .AND. CIDENT(2) == '00000' &     !8
            .AND. CIDENT(1)(1:8) == CTRAN(J3)(3:10)) GO TO 82        !8

        END IF IFLABEL10 !- area_flag

81      CONTINUE
      END DO DOLABEL4

82  IDAY=IDATHR/256
    IHOUR=IDATHR-IDAY*256
    CALL HRS2DT(IY,IM,ID,IH,J1)
    IF(ID /= IDAY.OR.IH /= IHOUR)THEN
      CALL HRS2DT(IY2,IM2,ID2,IH2,J1+INXHRS-1)
      WRITE(6,9015)CTYPE,IY,IM,ID,IH,IY2,IM2,ID2,IH2
      GO TO 799 ! NEXT INDEX BLOCK
    END IF
    if(lflag)print*,'from index block',iblock,itries,'entries'
  END IF IFLABEL7

!-----------------------------------------------------------------------
! sort entries into station identifier order
! (for latest reports the last byte in the identifier field has been
! set above to 1,2 or 3 to indicate the index block)
!-----------------------------------------------------------------------

  CALL SORTCH(CNTRY,INDLEN,ITRIES,MASK)
  INDXRED=.TRUE.
END IF IFLABEL6

!=======================================================================
!
! LOOP OVER ENTRIES FROM THIS INDEX BLOCK (OR BLOCKS IF LATEST)
!
!=======================================================================

IF (LFLAG) WRITE(*,*)'In TFMRET: looping over entries ',ient1,itries

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

DOLABEL5: &
DO J2=IENT1,ITRIES

!-----------------------------------------------------------------------
! get lat & long from index entry
!-----------------------------------------------------------------------

  LAT=ICHAR(CNTRY(J2)(13:13))*256+ICHAR(CNTRY(J2)(14:14))
  LON=ICHAR(CNTRY(J2)(15:15))*256+ICHAR(CNTRY(J2)(16:16))
  IF (LAT >= 32768) LAT=LAT-65536
  IF (LON >= 32768) LON=LON-65536

  IF (LAT /= -32768) THEN
    RLAT=LAT*0.01
    RLON=LON*0.01
  ELSE
    RLAT=-9999999.
    RLON=-9999999.
  END IF

!-----------------------------------------------------------------------
! at entry level check for required identifier and lat/lon area.
!-----------------------------------------------------------------------

IFLABEL11: &
IF(.NOT.NTRYCHK)THEN
  THISID=CNTRY(J2)(3:10)  ! IGNORE LAST BYTE (1,2,3 IF LATEST)

!-----------------------------------------------------------------------
! for latest requests we only need one report for a given id so
! check if a report of this id has already been returned
!-----------------------------------------------------------------------

  IF(LATEST)THEN
    IF(THISID == LASTID)THEN
      IF(PASSED)GO TO 699
    ELSE
      LASTID=THISID
      PASSED=.FALSE.
    END IF
  END IF

!-----------------------------------------------------------------------
! number of reports in chain
!-----------------------------------------------------------------------

  NCHN=ICHAR(CNTRY(J2)(12:12))
  IF (NCHN > MXCHLEN) WRITE(*,*)'TFMRET: MDB WARNING - ', &
     'MAX CHAIN LENGTH EXCEEDED'

  if(lflag)print*,' entry ',j2,' reports in chain ',nchn

!-----------------------------------------------------------------------
! physical block (not counting index & map blocks and local d entry)
!-----------------------------------------------------------------------

  NBLOCK = ICHAR(CNTRY(J2)(22:22))*256 + ICHAR(CNTRY(J2)(23:23))

  IBLOCK = 1+NSQ+INXBLK+NBLOCK


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
  END IF
  NTRYCHK=.TRUE.
END IF IFLABEL11

!-----------------------------------------------------------------------
! Read in reports in this chain. READ_CHNREPS is true initially.
! Keep reading until READ_CHNREPS is false (when the pointer to
! the next report in the chain is zero).
! Keep the trailers for all the reports in the chain, but replace
! a pointer to the next ob in the chain with a pointer to this ob,
! so that the obs can be easily read in again whichever way
! (forwards or backwards) the chain is looped round.
! So the pointer in the first trailer is taken from the index.
!-----------------------------------------------------------------------

KREP=0

IFLABEL12: &
IF (.NOT.CHANRED) THEN
  if (lflag) write(6,*)'tfmret: reading in chain'

  READ_CHNREPS = .TRUE.
  J5           = 0
  KEPTIM       = -9999999
  CTRAIL(1)(20:23)=CNTRY(J2)(20:23) ! pointer to first ob

DOLABEL6: &
  DO WHILE (READ_CHNREPS)
    if (lflag) write(6,*)

!-----------------------------------------------------------------------
! get message
!-----------------------------------------------------------------------

    CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,LOCDFG, &
               NELMIX, IBLOCK, INUM, CTRAN, IDATHR, IDREC,     &
               IRECLN, KMSG, 'MSGRD')
    J5 = J5 + 1

!-----------------------------------------------------------------------
! The code below treats NCM & SREWs differently from other types.
! It's not clear why - but NCMs & SREWs can be skipped as the
! chain is read in (if they fail TOR or preferred flag tests)
! while for other data types the whole chain is read in.
!-----------------------------------------------------------------------

    if(lflag)print*,' tfmret rec=',j5,found(9),'>',ctype,'<',iver
    if(lflag)print*,' tfmret report transferred kmsg(',j5,')>', &
              kmsg(1:irecln-23),'<'

    TRAILX = KMSG(IRECLN-22:IRECLN)

IFLABEL13: &
    IF ((CTYPE(1:3) == 'NCM' .OR. CTYPE(1:4) == 'SREW') .AND. &
       IVER /= 2) THEN

      NTOR = ICHAR(TRAILX(18:18))*256 + ICHAR(TRAILX(19:19))

!-----------------------------------------------------------------------
! check to see if ntor is meant to be negative. ntor is read from 2
! bytes which means that the maximum positive value ntor can have is
! 32768. if ntor is greater than this value it must be converted into
! a negative number. this is done simply by subtracting the maximum
! possible value that can be held in a 2 byte integer when the sign bit
! is ignored (i.e. 2 to the power 16) from ntor.
!-----------------------------------------------------------------------

      IF (NTOR > 32768 .AND. .NOT.LCLIM) NTOR = NTOR - 65536

!-----------------------------------------------------------------------
! Change ITORM > IRTIM2 to ITORM >= IRTIM2
!-----------------------------------------------------------------------

      IDHR=ICHAR(TRAILX(1:1))
      IDHR=MOD(IDHR,64)
      IMIN=ICHAR(TRAILX(2:2))

      ITORM = (IDAT)*60 + NTOR
      ISPREF=ICHAR(TRAILX(17:17))/128 == 1
      WASPREF=MOD(ICHAR(TRAILX(17:17))/32,2) == 1
      if(lflag)then
        write(6,'('' tfmret ctrail >'',z46,''<'')')trailx(1:23)
        print*,' tfmret msg >',kmsg(1:irecln-23),'<'
      END IF

      if(lflag)print*,' tfmret is/waspref ',j5,ispref,waspref,itorm, &
         irtim1,irtim2,keptim,krep,nchn,imin,idrec
!-----------------------------------------------------------------------
! check time of receipt & preferred flags, only keeping report if it
! was received in time slot and (if ver=1, preferred report requested),
! it is preferred or was preferred at the cutoff time.  if ver=1, only
! one report is wanted for any time, but two or more obs for that time
! may have been preferred before a cutoff, so keep report time when a
! preferred report is found to avoid returning more than one. (reports
! for the same time are consecutive in the chain). Change check (.f77!H)
! slightly from  >  to  >=
!-----------------------------------------------------------------------

      SKIP=.FALSE.
IFLABEL14: &
      IF (ITORM < IRTIM1) THEN
        SKIP=.TRUE.
        if(lflag)print*,' tfmret time test received too early ', &
         'itorm<irtim1 ',itorm,irtim1
      ELSE IF (IRTIM2 > 0 .AND. ITORM >= IRTIM2) THEN
        SKIP=.TRUE.
        if(lflag)print*,' tfmret time test received too late ', &
         'itorm>=irtim2 ',itorm,irtim2
      ELSE IF (IVER == 1.AND.IRTIM2 == 0.AND..NOT.ISPREF) THEN
        SKIP=.TRUE.
        if(lflag)print*,' tfmret time test not finally preferred ', &
         ' iver,irtim2,ispref ',iver,irtim2,ispref
      ELSE IF (IVER == 1.AND.IRTIM2 > 0.AND..NOT.WASPREF) THEN
        SKIP=.TRUE.
        if(lflag)print*,' tfmret time test ', &
         'not preferred at cutoff time ', &
         ' iver,irtim2,waspref ',iver,irtim2,waspref
      ELSE IF (IVER == 1.AND.IDHR*60+IMIN == KEPTIM) THEN
        SKIP=.TRUE.
        if(lflag)print*,' tfmret: only one ob for each time ', &
         ' iver,keptim ',iver,keptim
      ELSE
        if(lflag)print*,' tfmret time test 6 - good report'
        KEPTIM=IDHR*60+IMIN  ! return only one for this time
        KREP=KREP+1      ! add to number of reports found
        CTRAIL(KREP)(1:19)=KMSG(IRECLN-22:IRECLN-4)
        IF (KREP < NCHN) THEN
          CTRAIL(KREP+1)(20:23)=KMSG(IRECLN-3:IRECLN)
        END IF

        if(lflag)print*,' tfmret block ',iblock,' record ',idrec
      END IF IFLABEL14

! If a report has been skipped above, overwrite the pointer to the
! skipped report (kept in advance) with the pointer to the next
! report (which may, of course, be overwritten in its turn).

      IF (SKIP .AND. KREP < NCHN) THEN
        CTRAIL(KREP+1)(20:23)=KMSG(IRECLN-3:IRECLN)
      END IF

    ELSE
      KREP=KREP+1        ! add to number of reports found
      CTRAIL(KREP)(1:19)=KMSG(IRECLN-22:IRECLN-4)
      IF (KREP < NCHN) THEN
        CTRAIL(KREP+1)(20:23)=KMSG(IRECLN-3:IRECLN)
      END IF

      if(lflag)print*,' tfmret block ',iblock,' record ',idrec
    END IF IFLABEL13

!-----------------------------------------------------------------------
! get block and record number of next in chain. If the block number or
! record number = 0, set READ_CHNREPS = .FALSE. - we will read the
! chain no further
!-----------------------------------------------------------------------

    NBLOCK = ICHAR(TRAILX(22:22))*256 + ICHAR(TRAILX(23:23))
    IBLOCK = 1+NSQ+INXBLK+NBLOCK

    NREC = ICHAR(TRAILX(20:20))*256 + ICHAR(TRAILX(21:21))
    IDREC=NREC

    IF (LFLAG) THEN
      write(6,*)'tfmret: next block, record in chain = ', &
      iblock,idrec
    END IF

    READ_CHNREPS = (IDREC > 0 .AND. NBLOCK > 0 .AND. KREP < MXCHLEN)

  END DO DOLABEL6    !- end of loop over reports in chain

!-----------------------------------------------------------------------
! decide whether to loop forwards or backwards over chain
! order='f' is forwards in time i.e start at the end of the chain
! and loop from nchn to 1
!-----------------------------------------------------------------------

  IF(LATEST.OR.ORDER == 'B')THEN
    ICHL1=1
    ICHL2=KREP
    ICHL3=1
  ELSE    ! IF(ORDER == 'F')THEN
    ICHL1=KREP
    ICHL2=1
    ICHL3=-1
  END IF
  CHANRED=.TRUE.
END IF IFLABEL12

!=======================================================================
!
! LOOP OVER REPORTS IN CHAIN
!
! AT MESSAGE LEVEL CHECK TIME OF OB AND TIME OF RECEIPT.
! ALSO CHECK TAF LONG/SHORT FLAG
!
!=======================================================================

if(lflag)print*,' looping over chain ',ichl1,ichl2,ichl3

DOLABEL7: &
DO J5=ICHL1,ICHL2,ICHL3

!-----------------------------------------------------------------------
! TFMRET only deals with data types with only one ob per message,
! but LMIDMSG is set when an ob was ready to return to the user,
! but the user's array was full.  If so, checks can be skipped.
!-----------------------------------------------------------------------

IFLABEL16: &
  IF(.NOT.LMIDMSG)THEN

!-----------------------------------------------------------------------
! check taf type at report level
!-----------------------------------------------------------------------

    IF (CTYPE(1:4) == 'TAFS' .OR. &
        CTYPE(1:5) == 'ATAFS') THEN
      TAFTYP=MOD(ICHAR(CTRAIL(J5)(17:17)),4)
      IF (TAFLEN /= 0.AND.TAFLEN /= TAFTYP) GO TO 675
    END IF

!-----------------------------------------------------------------------
! Check whether METAR or SPECI at the report level.
!-----------------------------------------------------------------------

    IF (CTYPE(1:6) == 'METARS') then
      METTYP=MOD(ICHAR(CTRAIL(J5)(17:17)),4)
      IF (METSPECI /= METTYP) GO TO 675
    END IF

    IDHR=ICHAR(CTRAIL(J5)(1:1))
    IDHR=MOD(IDHR,64)
    IMIN=ICHAR(CTRAIL(J5)(2:2))

!-----------------------------------------------------------------------
! compare obs time with start/end times if given
!-----------------------------------------------------------------------

IFLABEL17: &
    IF(FOUND(1))THEN                    ! IF START TIME GIVEN
      ICMIN=DT2HRS(IY,IM,IDAY,IHOUR+IDHR) ! CENTURY-HOUR OF OB
      ICMIN=ICMIN*60 + IMIN             ! CENTURY-MINUTE OF OB
      if(lflag)print*,' tfmret c.min',icmin,iy,im,iday,ihour,idhr, &
                       imin
      if(lflag)print*,' rept c.min',icmin,itimst,itimnd
      IF(FOUND(1).AND.ICMIN < ITIMST) GO TO 675 ! IF START GIVEN..
      IF(FOUND(2).AND.ICMIN > ITIMND) GO TO 675 ! IF END GIVEN...
      if(lflag)print*,' tfmret rept passed obs time test '

!-----------------------------------------------------------------------
! if there's an increment in the request, see if report is needed.   '
!-----------------------------------------------------------------------

      IF(FOUND(4))THEN
        CALL SUBPER(ITIMST,TRANGE,INCR,ITIMND,ICMIN,RC)
        IF (RC /= 0) GO TO 675
        if(lflag)print*,' fits increment',incr,'hours'
      END IF
    END IF IFLABEL17

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

    IF (NTOR > 32768 .AND. .NOT.LCLIM) NTOR = NTOR - 65536

!-----------------------------------------------------------------------
! Change ITORM > IRTIM2 to ITORM >= IRTIM2
!-----------------------------------------------------------------------

    ITORM = (IDAT)*60 + NTOR
    IF((IRTIM1+IRTIM2 /= 0).AND. &
       (IRTIM1 /= 0 .AND. ITORM < IRTIM1) .OR. &
       (IRTIM2 /= 0 .AND. ITORM >= IRTIM2)) GO TO 675
    PASSED=.TRUE.

    if (lflag) then
      print*,'itorm:',itorm
      print*,' ntor ',ntor
      print*,' report passed all checks '
    END IF

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
! Get METAR station height if available (bytes 9-10 of index entry)
!-----------------------------------------------------------------------

    IF (CTYPE(1:6)  /=  'METARS' .OR. & ! Not METARS or ..
        CNTRY(J2)(9:10) == '  ') THEN   ! height missing
      ISECT1(9) = -9999999
    ELSE
      I = ICHAR2(CNTRY(J2)(9:10))
      IF (I > 32767) I = I - 65536
      ISECT1(9) = I
    END IF

!-----------------------------------------------------------------------
! Read the report in again, using pointer kept in trailer-reading
! loop. Put total length at start & initialise header.
!-----------------------------------------------------------------------

    IBLOCK=ICHAR(CTRAIL(J5)(22:22))*256 &
          +ICHAR(CTRAIL(J5)(23:23))+1+NSQ+INXBLK
    IDREC =ICHAR(CTRAIL(J5)(20:20))*256 &
          +ICHAR(CTRAIL(J5)(21:21))

    CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,  &
               LOCDFG,NELMIX,IBLOCK,INUM,CTRAN,IDATHR,   &
               IDREC,IRECLN,KMSG,'MSGRD')
    IRECLN=IRECLN-INDLEN

    WRITE(CREP(1:5),'(I5)')IRECLN+IHDLEN

    IF (LCLIM) THEN
      CALL SETHED(HEADER,RLAT,RLON,0,0,1,IM,             &
                  IY,ISECT1(4),ISECT1(5),NAMD,NCOR,CCCC)
    ELSE
      CALL SETHED(HEADER,RLAT,RLON,IHOUR+IDHR,IMIN,IDAY,IM, &
                  IY,ISECT1(4),ISECT1(5),NAMD,NCOR,CCCC)
    END IF

    CREP(5+1:5+IHDLEN)=HEADER
    CREP(5+IHDLEN+1:5+IHDLEN+IRECLN)=KMSG(1:IRECLN)
    if(lflag)print*,'message length',irecln,crep(1:80)

!=======================================================================
!
! EXPAND REPORT (UNLESS JUST THE RAW REPORT IS NEEDED)
!
!=======================================================================

!-----------------------------------------------------------------------
! Initialise expansion arrays
!-----------------------------------------------------------------------
IFLABEL18: &
    IF(CTYPE(1:6) == 'METARS')THEN
      CALL MTRINT(VALUES,CNAM,RLAT,RLON,IY,IM,IDAY,IHOUR+IDHR, &
                IMIN,BULCOD,CCCC,THISID)
      if(lflag)then
        write(6,9011)(values(k),k=1,10)
        write(6,*)cnam(1:80)
      END IF
    ELSE IF (CTYPE(1:4) == 'TAFS' .OR. CTYPE(1:5) == 'ATAFS') THEN
      CALL TAFINT(VALUES,CNAM,RLAT,RLON,IY,IM,IDAY,IHOUR+IDHR, &
                IMIN,BULCOD,CCCC,THISID,CTAF(TAFTYP))
      if(lflag)then
        write(6,9011)(values(k),k=1,10),(values(150+k),k=1,10), &
                   (values(300+k),k=1,10)
        write(6,*)cnam(1:80)
      END IF
    ELSE IF(CTYPE(1:4) == 'SREW')THEN
      CALL SRWINT(VALUES,CNAM,RLAT,RLON,IY,IM,IDAY,IHOUR+IDHR, &
                BULCOD,CCCC,THISID)
    ELSE IF(CTYPE(1:3) == 'NCM')THEN
      CALL NCMINT(VALUES,CNAM,RLAT,RLON,IY,IM,IDAY,IHOUR+IDHR, &
                IMIN,BULCOD,CCCC,THISID)
    ELSE IF(CTYPE(1:4) == 'TROP') THEN
      CALL TRPINT(VALUES,CNAM,IY,IM,IDAY,IHOUR+IDHR,IMIN, &
                BULCOD,CCCC,THISID)
    END IF IFLABEL18

!-----------------------------------------------------------------------
! check if expansion is needed
!-----------------------------------------------------------------------

IFLABEL19: &
    IF(.NOT.CHAREP)THEN     ! SOME ELEMENTS SELECTED
IFLABEL20: &
      IF (EXPAND) THEN      ! WHICH ARE NOT IN INDEX
        if(lflag)print*,' doing expansion '
IFLABEL21: &
        IF(CTYPE(1:6) == 'METARS')THEN

!-----------------------------------------------------------------------
! expand metar report
!-----------------------------------------------------------------------

          CALL MTREXP(KMSG,IRECLN,VALUES,CNAM,IFAIL)
          if(lflag)write(6,9011)(values(k),k=1,66)

        ELSE IF (CTYPE(1:4) == 'TAFS' .OR. CTYPE(1:5) == 'ATAFS') THEN

!-----------------------------------------------------------------------
! expand taf reports
!-----------------------------------------------------------------------
          CALL TAFEXP(KMSG,IRECLN,VALUES,NAMD,IFAIL)
          if (lflag) write(6,9014) (values(k),k=1,150)
9014      format(' values after tafexp'/ (5F13.2) )

        ELSE IF(CTYPE(1:4) == 'SREW')THEN
          CALL SRWEXP(KMSG,VALUES,IRECLN)
        ELSE IF(CTYPE(1:3) == 'NCM')THEN
          CALL NCMEXP(KMSG,IRECLN,VALUES)
        END IF IFLABEL21
      END IF IFLABEL20
    END IF IFLABEL19 ! END OF EXPANSION
  END IF IFLABEL16

  NREP=1
  IMAX=1

!-----------------------------------------------------------------------
! Check if there's room for this report; if not, set LMIDMSG.
!-----------------------------------------------------------------------

  IF(NOBS-IOB < NREP-NSEND)THEN
    NSEND=IREP1+(NOBS-IOB)-1
    LMIDMSG=.TRUE.
  ELSE
    NSEND=NREP
    LMIDMSG=.FALSE.
  END IF
  if(lflag)print*,' &&& irep1,nsend,iob,lmidmsg',irep1, &
                           nsend,iob,lmidmsg

!=======================================================================
!
! Transfer data from VALUES, ISECT1, CNAM & CREP,
! using displacements in IDISP array,
! to ARRAY, CSTR & CREPRT, starting at IOB+1.
!
!=======================================================================

!-----------------------------------------------------------------------
! map users elements to displacements in values array
!-----------------------------------------------------------------------

  IF(.NOT.LDONE)THEN
    if(lflag) then
      write(6,*)'calling condes:ndes,idesc,idisp',ndes,idesc,idisp
    END IF
    CALL CONDES(IDESC,NDES,IDISP)
    IMD=NDES
    if(lflag)write(6,9010)(idisp(k),k=1,ndes)
    LDONE=.TRUE.
  END IF
  CALL TRNSFR(IDISP,IMD,IREP1,NSEND,CSTR,IOB,ARRAY,CREP,NOBS, &
              NELEM,ISECT1,IMAX,VALUES,CNAM,CREPRT,II,LFLAG)
  if (lflag) then
    write(6,9013)((array(k1,k2),k1=1,nrep),k2=1,imd)
9013      format(' output array after trnsfr'/(8f10.2))
    write(6,*)' '
  END IF

!=======================================================================
!
! INCREMENT AND SAVE LOOP COUNTERS FOR NEXT ENTRANCE
!
!=======================================================================

!-----------------------------------------------------------------------
! LMIDMSG is true here when a message has been decoded
! but the data hasn't been put in the user's array.
!-----------------------------------------------------------------------

IFLABEL22: &
  IF(LMIDMSG)THEN
    if(lflag)print*,'no more room in array',iob
IFLABEL23: &
    IF(NSEND+1 > NREP)THEN ! (Must be true if one ob/msg?)
      NXTREP=1
      NSEND=0
IFLABEL24: &
      IF(EOCHN(J5,ICHL2,ICHL3))THEN ! TEST FOR END OF CHAIN
        NXTCHN=1
        CHANRED=.FALSE.
        IF(J2+1 > ITRIES)THEN ! END OF INDEX ENTRIES THIS BLOCK
          NXTENT=1
          IF(J1+INXHRS > ISHR2)GO TO 999 ! FINISHED ALTOGETHER
          NXTIME=J1+INXHRS
          INDXRED=.FALSE.
        ELSE
          NXTENT=J2+1
          NXTIME=J1
        END IF
        NTRYCHK=.FALSE.
      ELSE
        NXTCHN=J5+ICHL3 ! NEXT IN CHAIN
        NXTENT=J2  ! SAME INDEX ENTRY
        NXTIME=J1  ! SAME INDEX BLOCK
      END IF IFLABEL24
    ELSE      ! (Never reached if only one ob per message?)
      NXTREP=NSEND+1 ! NEXT REPORT IN MESSAGE
      NXTCHN=J5
      NXTIME=J1
      NXTENT=J2
    END IF IFLABEL23
    ISTAT=4 ! SET MORE DATA TO COME AND RETURN
    if (lflag) write(6,9009)nxtrep,nxtent,nxtime,nxtchn
    GO TO 999
  ELSE                      ! if not LMIDMSG...
    IOB=IOB+(NSEND-IREP1)+1 ! INCREMENT NO OF REPORTS TRANSFERRED
    NSEND=0
    NXTREP=1
    IREP1=1
IFLABEL25: &
    IF(IOB == NOBS)THEN ! ARRAY IS NOW FULL

!-----------------------------------------------------------------------
! increment loop variables for entry next time
!-----------------------------------------------------------------------

IFLABEL26: &
      IF(EOCHN(J5,ICHL2,ICHL3))THEN
        NXTCHN=1
        CHANRED=.FALSE.
        IF(J2+1 > ITRIES)THEN
          NXTENT=1
          IF(J1+INXHRS > ISHR2)GO TO 999
          NXTIME=J1+INXHRS
          INDXRED=.FALSE.
        ELSE
          NXTENT=J2+1
          NXTIME=J1
        END IF
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
        END IF
      END IF IFLABEL26
      ISTAT=4 ! MORE DATA TO COME (MAYBE)
      if(lflag)print*,'array full:nxtrep,nxtent,nxtime ', &
                       nxtrep,nxtent,nxtime
      GO TO 999
    END IF IFLABEL25
  END IF IFLABEL22
  IF(LATEST.AND.PASSED)THEN
    NTRYCHK=.FALSE.
    CHANRED=.FALSE.
    if(lflag)print*,' do not want any more of this chain'
    GO TO 699
  END IF
675   CONTINUE   ! END OF LOOP OVER REPORTS IN THIS CHAIN
END DO DOLABEL7
NTRYCHK=.FALSE.
CHANRED=.FALSE.
if(lflag)print*,' idrec at end of chain is ',idrec

!-----------------------------------------------------------------------
! we have got to the end of the chain for the required report
! if no more identifiers are needed we can skip the rest of the
! index entries
!-----------------------------------------------------------------------

699   CONTINUE   ! END OF LOOP OVER ENTRIES THIS HOUR
END DO DOLABEL5
IENT1=1
INDXRED=.FALSE.
799   CONTINUE   ! END OF LOOP OVER HOURS
END DO DOLABEL1
IF(IOB == 0)THEN
  IF(LCONT)THEN
    ISTAT=0
  ELSE
    ISTAT=8
  END IF
END IF

!-----------------------------------------------------------------------
! return number of reports
!-----------------------------------------------------------------------

999 CONTINUE

IF (II /= 0) THEN
  IOBS=II
ELSE
  IOBS=IOBS-1
END IF

if(lflag)print*,'exit. ii,iob,iobs,nobs',ii,iob,iobs,nobs
RETURN

9001  FORMAT(' POINTERS ON RE-ENTRY:'/ &
       ' START HOUR ',I8/ &
       ' ENTRY NUMBER ',I5/ &
       ' REPORT NUMBER ',I5/ &
       ' CHAIN ',I5/ &
       ' NSEND ',I5)
9002  FORMAT(' DESCRIPTOR '/(5I8))
9003  FORMAT(' REQUEST START:',I10,' END:',I10)
9004  FORMAT(' MDB WARNING: CAN ONLY FIND LATEST REPORTS IN', &
       ' THE ',I3,' HOURS UP TO ',I4,'/',I2.2,'/',I2.2,1X, &
       I4.4,'Z')
9005  FORMAT(' NO OF INDEX BLOCKS (INXBLK)',I8/ &
       ' HOURS PER BLOCK    (INXHRS)',I8/ &
       ' START OF 1ST SLOT  (ISLOTHR)',I8/ &
       ' HOUR OFFSET        (INDHR1/2)',2I4/ &
       ' SLOT HOURS FOR REQUEST  (ISHR1/2) ',2I8/ &
       ' LOOP DIRECTION ',I8)
9009  FORMAT(' NXTREP =',I4,' NXTENT= ',I4/ &
       ' NXTIME =',I4,' NXTCHN= ',I4)
9010  FORMAT(' IDISP',(10I5))
9011  FORMAT(' AFTER MTRINT ',5F12.3/5F12.3)
9012  FORMAT(' MDB ERROR: ARRAY NOT LARGE ENOUGH -TRY ARRAY(', &
      I3,',',I3,')')
9015  FORMAT(' NO ',A8, ' DATA IN MDB FOR PERIOD ',I4,'/', &
       I2.2,'/',I2.2,1X,I2.2,'Z - ',I4,'/',I2.2,'/', &
       I2.2,1X,I2.2,'Z')

END SUBROUTINE TFMRET
