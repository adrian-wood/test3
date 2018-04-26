SUBROUTINE SYNSEQ(IDENT,IDATIM,PRESS,TEND,FLAG)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNSEQ
!
! PURPOSE       : PRESSURE TENDENCY CHECK FOR SYNOPS
!
! DESCRIPTION   : SYNSEQ DOES A LOW-LEVEL RETRIEVAL TO GET PRESSURE
!                 3 HOURS AGO, USING THE INDEX(ES) BEING UPDATED BY
!                 SYNREP TO FIND THE REPORT AND BIT COUNT RETRIEVAL
!                 TO FIND THE PRESSURE
!
! DATA TYPE(S)  : SYNOPs
!
! CALLED BY     : SYNEXP
!
! CALLS         : MDBIO (map & message calls), DATE31, EB2ASC,
!                 BITINDX,VALUSR
!                 COMMON /SYNIX/ IS SHARED WITH SYNSTO
!
! ARGUMENTS     : (1) STATION FOR WHICH REPORT IS REQUIRED        (I)
!                 (2) DATE/TIME OF NEW REPORT (year/mon/day/hour) (I)
!                 (3) PRESSURE IN NEW REPORT                      (I)
!                 (4) PRESSURE TENDENCY IN NEW REPORT             (I)
!                 (5) RESULT OF TEST (0: CONSISTENT,              (O)
!                      (1: INCONSISTENT, MISSING: NO TEST)
!
! REVISION INFO :
!
!
! $Workfile: synseq.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 11/01/2011 10:44:56$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         11/01/2011 10:44:56    Brian Barwell
!       Overlooked ".GT." changed to ">".
!  2    MetDB_Refresh 1.1         10/01/2011 14:51:06    Rosemary Lavery
!       corrections after review
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

USE BITINDX_mod
USE DESFXY_mod
USE EB2ASC_mod
USE MDBIO_mod
USE READIDX_mod
USE VALUSR_mod
USE ZPDATE_mod

! Interface Arguments

CHARACTER (LEN=5), INTENT(IN)  :: IDENT
INTEGER, INTENT(IN)            :: IDATIM(5)
REAL, INTENT(IN)               :: PRESS
REAL, INTENT(IN)               :: TEND
REAL, INTENT(OUT)              :: FLAG

! Local Parameters

INTEGER, PARAMETER            :: MAXLREP = 999
INTEGER, PARAMETER            :: NDIM1 = 1            ! 1 item only
CHARACTER (LEN=8), PARAMETER  :: CDTYPE = 'LNDSYN  '  ! Data Type for SYNOPs

! Local Variables

INTEGER  :: CENDAY
INTEGER  :: DISPL(NDIM1)   ! - one item
INTEGER  :: F
INTEGER  :: I
INTEGER  :: IBLOCK
INTEGER  :: IBUFR
INTEGER  :: IDESC(NDIM1)
INTEGER  :: IDSK(5)
INTEGER  :: IL
INTEGER  :: INDLEN = 23    !- length of index entry
INTEGER  :: INDHOR
INTEGER  :: INXBLK = 0     !- NONZERO ONCE MAP HAS BEEN READ
INTEGER  :: INXHRS
INTEGER  :: IREPL(NDIM1)
INTEGER  :: ISECT1(1) = 0  !- not used in VALUSR
INTEGER  :: ISLOTHR
INTEGER  :: ISTAT
INTEGER  :: IXHOUR(3)      !!! MUST AGREE WITH SYNSTO
INTEGER  :: LENCHR(1)      !- dummy argument for VALUSR
INTEGER  :: LREP
INTEGER  :: LX
INTEGER  :: NBLOCK
INTEGER  :: NELMIX
INTEGER  :: NELREQ
INTEGER  :: NREC
INTEGER  :: NSQ
INTEGER  :: NTRIES(3)      !!! MUST AGREE WITH SYNSTO
INTEGER  :: N1 = 1
INTEGER  :: OBHOUR
INTEGER  :: REFVAL(NDIM1)
INTEGER  :: SCALE(NDIM1)
INTEGER  :: TAGHOR
INTEGER  :: WIDTH(NDIM1)
INTEGER  :: X
INTEGER  :: Y

REAL     :: OLDP(NDIM1,NDIM1)
REAL     :: TOL = 100.      !- TOLERANCE 100PA (=1.0MB)

LOGICAL  :: LFLAG = .FALSE. ! argument for VALUSR, BITINDX
LOGICAL  :: LOCDFG
LOGICAL  :: MATCH           ! Returned from READIDx
LOGICAL  :: NEWBITCALL = .TRUE.  !- TRUE for first call to BITINDEX

CHARACTER (LEN=4)        :: BUFR = 'BUFR'
CHARACTER (LEN=1)        :: C = ' '
CHARACTER (LEN=1)        :: CAR(NDIM1)
CHARACTER (LEN=1)        :: CHRELM(1)   !- dummy argument for VALUSR
CHARACTER (LEN=10000)    :: CINDX(12)   ! ELEMENT INDEX RECORD
CHARACTER (LEN=23)       :: ENTRY
CHARACTER (LEN=23)       :: INDEKS(7*27998/23,3)   !!! MUST AGREE WITH SYNSTO
CHARACTER (LEN=6)        :: LOCALD
CHARACTER (LEN=MAXLREP)  :: REPORT
CHARACTER (LEN=1)        :: XTYPE

COMMON /SYNIX/ IDSK, IXHOUR,NTRIES, INDEKS

! ---------------------------------------------------------------------

SAVE

! Initialize

FLAG=-9999999        ! SET FLAG TO "NO TEST DONE"
LFLAG=.FALSE.
MATCH=.FALSE.        ! Returned TRUE from READIDX if element index OK
CAR(1)=' '

! Read map block to find dataset details (only first 8 args relevant)
! (No test can be done until the storage program has been called and
! the common block set, so return if the FT number is still zero.)

IFBLOCK1: &
IF (INXBLK == 0) THEN
  IF (IDSK(3) == 0) RETURN
  CALL EB2ASC(4,BUFR)       ! first time only!

  CALL MDBIO(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LOCDFG,NELMIX,  &
    IBLOCK,NTRIES(1),INDEKS(:,1),IXHOUR(1),NREC,LREP,REPORT,'MAPRD')

  NSQ=0
  IF (LOCDFG) NSQ=1

! The element index for fast retrieval is read from the data set.
! The element number is the one stored in the data dictionary for
! LNDSYN MSL_PESR so beware of changes to the data dictionary.  In
! general the element name can be transformed to a number by calls to
! DDICT, EXPELM and MAPELM.

  CALL READIDX(CDTYPE,CINDX,XTYPE,MATCH)

  IF (.NOT. MATCH) THEN
    WRITE(*,*)'SYNSEQ: MDB Error: Cannot find element index ',  &
    'for LNDSYN'
  END IF
  IDESC(1)=53         !  MSL_PESR
  IREPL(1)=1
END IF IFBLOCK1

! Convert date/time of report to century-hour

CALL DATE31(IDATIM(3),IDATIM(2),IDATIM(1),CENDAY)
OBHOUR=(CENDAY-1)*24+IDATIM(4)

! Century-hour of report required & hour relative to index period

INDHOR=((OBHOUR-3-ISLOTHR)/6)*6
TAGHOR=MOD(OBHOUR-3+24-ISLOTHR,6)

! Find which of the 3 index blocks in core to look through for a report
! 3 hours back.  (Maybe none if the data is too old - if so, return.)

LX=0
DO I=1,3
  IF (IXHOUR(I) == INDHOR) LX=I
END DO
IF (LX == 0) RETURN

! Loop round entries in index for right period looking for identifier.
! If the latest hour in the entry implies there can't be a report for
! the required time, or if there's only one ob and it's for the wrong
! hour, then there's no point in looking on, so return.

DO_INDX: &
DO I=1,NTRIES(LX)
  IF (INDEKS(I,LX)(3:7) == IDENT) THEN
    IF (MOD(ICHAR(INDEKS(I,LX)(1:1)),64) < TAGHOR) RETURN
    IF (MOD(ICHAR(INDEKS(I,LX)(1:1)),64) /= TAGHOR  &
      .AND. ICHAR(INDEKS(I,LX)(12:12)) == 1) RETURN

! Get relative block number from pointer in entry & convert it to true
! block number using parameters returned by call to read map.
! If trailer doesn't point any further, there's no report, so return

    ENTRY=INDEKS(I,LX)

10  CONTINUE
    NBLOCK=ICHAR(ENTRY(22:22))*256+ICHAR(ENTRY(23:23))
    NREC  =ICHAR(ENTRY(20:20))*256+ICHAR(ENTRY(21:21))
    IF (NBLOCK <= 0 .OR. NREC <= 0) RETURN  ! NO REPORT FOUND

! Read next (or first) report in chain (& reset index entry to trailer
! for next pointer)  (Only relevant args: IBLOCK,NREC,LREP,REPORT)
! (MDBIO doesn't reread data block if a copy is in core, so may not find
! ob if index has just been updated... - hence check for zero length!)
! If ob is too long (i.e. rubbish on the end!), give up - MDBIO will
! have truncated it, so no trailer to follow chain.

    IBLOCK=1+NSQ+INXBLK+NBLOCK

    CALL MDBIO(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LOCDFG,NELMIX,  &
      IBLOCK,NTRIES(1),INDEKS(:,1),IXHOUR(1),NREC,LREP,REPORT,'MSGRD')

    IF (LREP < 23 .OR. LREP > MAXLREP) RETURN
    ENTRY=REPORT(LREP-22:LREP)

! If not right hour or not preferred report, look on down the chain.

    IF (MOD(ICHAR(ENTRY(1:1)),64) /= TAGHOR) GO TO 10
    IF (ICHAR(ENTRY(17:17)) < 128) GO TO 10

! If preferred report for 3 hours ago, retrieve pressure

    IBUFR=INDEX(REPORT,BUFR)
    IL=ICHAR(REPORT(IBUFR+29:IBUFR+29))*256 +  &
       ICHAR(REPORT(IBUFR+30:IBUFR+30))

    CALL DESFXY(IL,F,X,Y)
    WRITE(LOCALD,'(I1,I2.2,I3.3)')F,X,Y

    CALL BITINDX(CINDX,IDESC,NDIM1,IREPL(1),.FALSE.,     &
       ISTAT,LFLAG,LOCALD,REPORT(IBUFR:),NELREQ,DISPL,   &
       WIDTH,SCALE,REFVAL,NDIM1,NEWBITCALL)

    CALL VALUSR(REPORT(IBUFR:),NELREQ,DISPL,WIDTH,SCALE,        &
                REFVAL,OLDP,N1,N1,CAR,C,CAR,LFLAG,ISECT1,  &
                CHRELM,LENCHR)

! (only one element wanted, no characters involved - hence last 6 args!)

! Finally do test.  The only aim is to say whether this pressure is
! consistent with the last one and the tendency, so ignore any flag
! set on the old report - need longer sequence to see what's right.

    IF (ABS(PRESS - OLDP(1,1) - TEND) <= TOL) THEN
      FLAG=0.0        ! consistent
     ELSE
      FLAG=1.0        ! inconsistent
    END IF
!      PRINT*,'SYNSEQ ',ident,OLDP(1,1),PRESS,tend,FLAG
    RETURN            ! return with test done
  END IF
END DO DO_INDX

RETURN                ! return with no entry found
END SUBROUTINE SYNSEQ
