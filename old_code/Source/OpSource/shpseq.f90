SUBROUTINE SHPSEQ(IDENT,IDATIM,PRESS,TEND,PFLAG, &
                  NEWLAT,NEWLON,DS,VS,LLFLAG)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SHPSEQ
!
! PURPOSE       : PRESSURE TENDENCY & MOVEMENT CHECK FOR SHIPS
!
! DESCRIPTION   : SHPSEQ RETRIEVES PRESSURE, LATITUDE & LONGITUDE
!               : 3 HOURS AGO, USING THE INDEX(ES) BEING UPDATED BY
!               : SYNSTO TO FIND THE REPORT AND BIT COUNT RETRIEVAL
!               : TO FIND THE PRESSURE etc.
!
! DATA TYPE(S)  : SHIPs
!
! CALLED BY     : SYNEXP
!
! CALLS         : MDBIO (map & message calls), DATE31, EB2ASC,
!               : BITINDX,VALUSR
!               : COMMON /SHPIX/ IS SHARED WITH SHPSTO
!
! ARGUMENTS     :  (1) SHIP FOR WHICH REPORT IS REQUIRED           (I)
!               :  (2) DATE/TIME OF NEW REPORT (year/mon/day/hour) (I)
!               :  (3) PRESSURE IN NEW REPORT                      (I)
!               :  (4) PRESSURE TENDENCY IN NEW REPORT             (I)
!               :  (5) RESULT OF PRESSURE TEST (0: CONSISTENT,     (O)
!               :      (1: INCONSISTENT, MISSING: NO TEST)
!               :  (6) LATITUDE IN NEW REPORT                      (I)
!               :  (7) LONGITUDE IN NEW REPORT                     (I)
!               :  (8) DIRECTION OF MOVEMENT IN LAST 3 HOURS       (I)
!               :      (DEGREES CLOCKWISE FROM NORTH)
!               :  (9) MEAN SPEED LAST 3 HOURS (CONVERTED TO M/S)  (I)
!               : (10) RESULT OF POSITION TEST (0: CONSISTENT,     (O)
!               :      (1: INCONSISTENT, MISSING: NO TEST)
!
! REVISION INFO :
!
!
! $Workfile: shpseq.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 16/05/2011 09:08:07$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         16/05/2011 09:08:07    Sheila Needham  Added
!       SAVE
!  1    MetDB_Refresh 1.0         04/01/2011 16:36:47    Rosemary Lavery
!       Initial Import
! $
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

! Uses constants for Pi, radius of the earth & missing data value.

USE BITINDX_mod
USE DESFXY_mod
USE EB2ASC_mod
USE MDBIO_mod
USE METDB_COM_mod, ONLY : PI, REARTH, RMISS    ! supplied constants
USE READIDX_mod
USE VALUSR_mod
USE ZPDATE_mod

IMPLICIT NONE

! Interface arguments

CHARACTER (LEN=9), INTENT(IN)  :: IDENT
INTEGER, INTENT(IN)            :: IDATIM(5)
REAL, INTENT(IN)               :: PRESS
REAL, INTENT(IN)               :: TEND
REAL, INTENT(OUT)              :: PFLAG
REAL, INTENT(IN)               :: NEWLAT
REAL, INTENT(IN)               :: NEWLON
REAL, INTENT(IN)               :: DS
REAL, INTENT(IN)               :: VS
REAL, INTENT(OUT)              :: LLFLAG

! Local parameters

INTEGER, PARAMETER  :: MAXLREP = 999
INTEGER, PARAMETER  :: NDIM1 = 1          ! 1 item only
REAL,    PARAMETER  :: DSTOL = 45.        ! 45 DEGREES (SAME AS RESOLUTION)
CHARACTER (LEN=8), PARAMETER  :: CDTYPE = 'SHPSYN  '  ! Data Type for SYNOPs

! Local variables

INTEGER  :: CENDAY
INTEGER  :: DISPL(3)         ! index variable
INTEGER  :: F
INTEGER  :: I
INTEGER  :: IBLOCK
INTEGER  :: IBUFR
INTEGER  :: IDESC(3)         ! index variable
INTEGER  :: IDSK(5)
INTEGER  :: IL
INTEGER  :: INDHOR
INTEGER  :: INDLEN = 23      !  length of index entry
INTEGER  :: INXBLK = 0       !  NONZERO ONCE MAP HAS BEEN READ
INTEGER  :: INXHRS
INTEGER  :: IREPL(3)         ! index variable
INTEGER  :: ISECT1(1) = 0    !- ISECT1  for VALUSR (not used)
INTEGER  :: ISLOTHR
INTEGER  :: ISTAT
INTEGER  :: IX
INTEGER  :: IXHOUR(3)        !! MUST AGREE WITH SYNSTO (common)
INTEGER  :: IY
INTEGER  :: J
INTEGER  :: LENCHR(1)        ! dummy argument to VALUSR
INTEGER  :: LREP
INTEGER  :: LX
INTEGER  :: NBLOCK
INTEGER  :: NELEM            ! NUMBER OF ELEMENTS TO BE RETRIEVED (=3)
INTEGER  :: NELMIX
INTEGER  :: NELREQ           ! NUMBER OF ELEMENTS TO BE RETRIEVED (=3)
INTEGER  :: NREC
INTEGER  :: NSQ
INTEGER  :: NTRIES(3)        !! MUST AGREE WITH SYNSTO
INTEGER  :: N1 = 1           ! Argument to VALUSR
INTEGER  :: N3 = 3           ! Argument to BITINDX
INTEGER  :: OBHOUR
INTEGER  :: REFVAL(3)        ! index variable
INTEGER  :: SCALE(3)         ! index variable
INTEGER  :: TAGHOR
INTEGER  :: WIDTH(3)         ! index variable

REAL     :: ARRAY(1,3)       ! 2D array to match VALUSR
REAL     :: DIR
REAL     :: DIST
REAL     :: LATDIF
REAL     :: LONDIF
REAL     :: OLDLAT
REAL     :: OLDLON
REAL     :: OLDP
REAL     :: TOL = 10.        ! TOLERANCE 10PA (=0.1MB)
REAL     :: VSDIST
REAL     :: VSTOL= 30000.    ! 30 km (5 knots roughly 28km in 3 hours)
REAL     :: X
REAL     :: Y

LOGICAL  :: LFLAG
LOGICAL  :: LOCDFG
LOGICAL  :: MATCH            ! Returned from READIDX
LOGICAL  :: NEWBITCALL = .TRUE.   !- TRUE for first call to BITINDEX

CHARACTER (LEN=4)        :: BUFR = 'BUFR'
CHARACTER (LEN=1)        :: C = ' '
CHARACTER (LEN=1)        :: CAR(NDIM1)
CHARACTER (LEN=1)        :: CHRELM(1)  ! dummy argument to VALUSR
CHARACTER (LEN=10000)    :: CINDX(12)
CHARACTER (LEN=23)       :: ENTRY
CHARACTER (LEN=23)       :: INDEKS(3*27998/23,3)  !! MUST AGREE WITH SYNSTO
CHARACTER (LEN=6)        :: LOCALD
CHARACTER (LEN=MAXLREP)  :: REPORT
CHARACTER (LEN=1)        :: XTYPE

COMMON /ShpiX/ IDSK, IXHOUR,NTRIES, INDEKS

SAVE

!---------------------------------------------------------------------

! Initialize

PFLAG = RMISS             ! SET FLAG TO "NO TEST DONE"
LLFLAG = RMISS            ! SET FLAG TO "NO TEST DONE"
LFLAG = .FALSE.
MATCH = .FALSE.           ! Set Flag to FALSE - it will be set TRUE
                          ! by READIDX if the element index is matched

!-----------------------------------------------------------------------
! Read map block to find dataset details (only first 8 args relevant)
! (No test can be done until the storage program has been called and
! the common block set, so return if the FT number is still zero.)
!-----------------------------------------------------------------------

IFBLOCK1: &
IF (INXBLK == 0) THEN
  IF (IDSK(3) == 0) RETURN

  CALL EB2ASC(4,BUFR)        ! first time only!

  CALL MDBIO(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LOCDFG,NELMIX,  &
    IBLOCK,NTRIES(1),INDEKS(:,1),IXHOUR(1),NREC,LREP,REPORT,'MAPRD')

  NSQ=0
  IF (LOCDFG) NSQ=1

!-----------------------------------------------------------------------
! Read the element index for bit count retrieval from the data set.
! The element numbers are from the data dictionary for SHPSYN, beware
! of changes to the data dictionary elements list.
!
! There may be more than one element index, sort it out in BITINDX later
!-----------------------------------------------------------------------

  CALL READIDX(CDTYPE,CINDX,XTYPE,MATCH)

  IF (.NOT. MATCH) THEN
    WRITE(*,*)'In SHPSEQ: MDB Error: Cannot find element ',  &
    'index for subtype SHPSYN'
  END IF
  NELEM=3
  IDESC(1)=3   ! LTTD
  IDESC(2)=4   ! LNGD
  IDESC(3)=49  ! MSL_PESR
  DO J=1,3
    IREPL(J)=1
  END DO
END IF IFBLOCK1

!-----------------------------------------------------------------------
! Convert date/time of report to century-hour
!-----------------------------------------------------------------------

CALL DATE31(IDATIM(3),IDATIM(2),IDATIM(1),CENDAY)
OBHOUR=(CENDAY-1)*24+IDATIM(4)

!-----------------------------------------------------------------------
! Century-hour of report required & hour relative to index period
!-----------------------------------------------------------------------

INDHOR=((OBHOUR-3-ISLOTHR)/6)*6
TAGHOR=MOD(OBHOUR-3+24-ISLOTHR,6)

!-----------------------------------------------------------------------
! Find which of the 3 index blocks in core to look through for a report
! 3 hours back.  (Maybe none if the data is too old - if so, return.)
!-----------------------------------------------------------------------

LX=0
DO I=1,3
  IF (IXHOUR(I) == INDHOR) LX=I
END DO
IF (LX == 0) RETURN

!-----------------------------------------------------------------------
! Loop round entries in index for right period looking for identifier
! if the latest hour in the entry implies there can't be a report for
! the required time, then no point in looking on - return.
!-----------------------------------------------------------------------

DOBLOCK1: &
DO I=1,NTRIES(LX)

IFBLOCK2: &
  IF (INDEKS(I,LX)(3:11) == IDENT) THEN
    IF (MOD(ICHAR(INDEKS(I,LX)(1:1)),64) < TAGHOR) RETURN
    IF (MOD(ICHAR(INDEKS(I,LX)(1:1)),64) /= TAGHOR  &
      .AND. ICHAR(INDEKS(I,LX)(12:12)) == 1) RETURN

!-----------------------------------------------------------------------
! Get relative block number from pointer in entry & convert it to true
! block number using parameters returned by call to read map.
! If trailer doesn't point any further, there's no report, so return
!-----------------------------------------------------------------------

    ENTRY=INDEKS(I,LX)

10  CONTINUE
    NBLOCK=ICHAR(ENTRY(22:22))*256+ICHAR(ENTRY(23:23))
    NREC  =ICHAR(ENTRY(20:20))*256+ICHAR(ENTRY(21:21))
    IF (NBLOCK <= 0 .OR. NREC <= 0) RETURN  ! NO REPORT FOUND

!-----------------------------------------------------------------------
! Read next (or first) report in chain (& reset index entry to trailer
! for next pointer)  (Only relevant args: IBLOCK,NREC,LREP,REPORT)
! (MDBIO doesn't reread data block if a copy is in core, so may not find
! ob if index has just been updated... - hence check for zero length!)
! If ob is too long (i.e. rubbish on the end!), give up - MDBIO will
! have truncated it, so no trailer to follow chain.
!-----------------------------------------------------------------------

    IBLOCK=1+NSQ+INXBLK+NBLOCK

    CALL MDBIO(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LOCDFG,NELMIX,  &
      IBLOCK,NTRIES(1),INDEKS(:,1),IXHOUR(1),NREC,LREP,REPORT,'MSGRD')

    IF (LREP < 23 .OR. LREP > MAXLREP) RETURN
    ENTRY=REPORT(LREP-22:LREP)

!-----------------------------------------------------------------------
! If not right hour or not preferred report, look on down the chain.
!-----------------------------------------------------------------------

    IF (MOD(ICHAR(ENTRY(1:1)),64) /= TAGHOR) GO TO 10
    IF (ICHAR(ENTRY(17:17)) < 128) GO TO 10

!-----------------------------------------------------------------------
! If preferred report for 3 hours ago, retrieve pressure & lat/long
! (only one report, no characters wanted - hence last 5 VALUSR args!)
!-----------------------------------------------------------------------

    IBUFR=INDEX(REPORT,BUFR)
    IL=ICHAR(REPORT(IBUFR+29:IBUFR+29))*256 +  &
       ICHAR(REPORT(IBUFR+30:IBUFR+30))

    CALL DESFXY(IL,F,IX,IY)
    WRITE(LOCALD,'(I1,I2.2,I3.3)')F,IX,IY

    CALL BITINDX(CINDX,IDESC,NELEM,IREPL,.FALSE.,           &
                 ISTAT,LFLAG,LOCALD,REPORT(IBUFR:),NELREQ,  &
                 DISPL,WIDTH,SCALE,REFVAL,N3,NEWBITCALL)

    CALL VALUSR(REPORT(IBUFR:),NELREQ,        &
                DISPL,WIDTH,SCALE,REFVAL,     &
                ARRAY,N1,N1,CAR,C,CAR,LFLAG,  &
                ISECT1,CHRELM,LENCHR)

    OLDLAT=ARRAY(1,1)
    OLDLON=ARRAY(1,2)
    OLDP=ARRAY(1,3)

IFBLOCK3: &
    IF (OLDLAT /= RMISS .AND. OLDLON /= RMISS .AND.  &
        NEWLAT /= RMISS .AND. NEWLON /= RMISS .AND.  &
        DS /= RMISS .AND. VS /= RMISS) THEN

!-----------------------------------------------------------------------
! Test the ship's movement, working out a distance from the latitude &
! longitude differences and comparing it with VS, the mean speed over
! the last 3 hours (in m/s).
! (A is earth's radius in metres; speed*3*3600 is metres in 3 hours)
! N.B. The latitude difference is simply a fraction of the earth's
! circumference; the distance represented by the longitude difference
! depends on the latitude!  In one line the position difference is:
! 2*PI*A/360*SQRT((LAT1-LAT2)**2+((LONG1-LONG2)*COS((LAT1+LAT2)/2))**2)
!-----------------------------------------------------------------------

      LATDIF=NEWLAT-OLDLAT
      LONDIF=NEWLON-OLDLON
      X=(LONDIF/360)*(2*PI*REARTH)*COS((OLDLAT+NEWLAT)*PI/360.)
      Y=(LATDIF/360)*(2*PI*REARTH)

      DIST=SQRT(X**2+Y**2)          ! distance from positions
      VSDIST=VS*3.*3600.            ! distance from mean speed

!-----------------------------------------------------------------------
! DS is measured clockwise from N with a resolution of 45 degrees, so
! find DIR as an angle from N too;
! accept differences close to 360 as well as close to 0.
!-----------------------------------------------------------------------

      IF (DIST /= 0) THEN
        IF (LATDIF /= 0) THEN
          DIR=ATAN(LONDIF/LATDIF)*180./PI
          IF (LATDIF < 0) DIR=DIR+180.
          IF (LATDIF > 0 .AND. DIR < 0) DIR=DIR+360.
        ELSE
          IF (LONDIF > 0) DIR=90.   ! eastwards (compass bearing!
          IF (LONDIF < 0) DIR=270. ! westwards (compass bearing!
        END IF
      ELSE
        DIR=DS                      ! no ds check if dist=0
      END IF

!-----------------------------------------------------------------------
! Movement OK if distance & direction both agree to within tolerance
!-----------------------------------------------------------------------

      IF ((ABS(DIR-DS) <= DSTOL .OR. ABS(DIR-DS) >= 360.-DSTOL)  &
                            .AND. ABS(DIST-VSDIST) <= VSTOL) THEN
        LLFLAG=0.0                  ! consistent
      ELSE
        LLFLAG=1.0                  ! inconsistent
      END IF
    END IF IFBLOCK3

!-----------------------------------------------------------------------
! Now test pressure. The only aim is to say whether this pressure is
! consistent with the last one and the tendency, so ignore any flag
! set on the old report - need longer sequence to see what's right.
!-----------------------------------------------------------------------

    IF (PRESS /= RMISS .AND. OLDP /= RMISS) THEN
      IF (ABS(PRESS-OLDP-TEND) <= TOL) THEN
        PFLAG=0.0     ! consistent
      ELSE
        PFLAG=1.0     ! inconsistent
      END IF
    END IF
!         PRINT*,'SHPSEQ ',ident,oldp,press,tend,pflag
!         PRINT*,'SHPSEQ',OLDLAT,OLDLON,newlat,newlon,llflag
    RETURN            ! return with test done
  END IF IFBLOCK2
END DO DOBLOCK1

RETURN                ! return with no entry found
END SUBROUTINE SHPSEQ
