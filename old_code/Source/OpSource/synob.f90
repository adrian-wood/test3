SUBROUTINE SYNOB(REPORT,REPLEN,OCOR,CORNUM,YYGGIW,IDATIM,TTAAII, &
                 CCCC,ICCCC,MIMJ,NFT,BLKSIZ)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNOB
!
! PURPOSE       : CALLS TO EXPAND REPORT, MAKE BUFR MESSAGE AND
!                 STORE REPORT AND MESSAGE
!
! CALLED BY     : SYNBUL
!
! CALLS         : DATIM, OBHOUR, SYNEXP,ENBUFR,SYNIND
!
! ARGUMENTS     : (1) REPORT   STARTS WITH 5-FIG STN NUMBER IF LAND,
!                              CALL SIGN (?) IF SHIP              (I)
!                 (2) REPLEN   LENGTH OF REPORT                   (I)
!                 (3) OCOR     TRUE IF CORRECTED REPORT           (I)
!                 (4) CORNUM   NUMBER OF CORRECTION               (I)
!                 (5) YYGGIW   DATE/TIME GROUP FROM 2ND LINE OF   (IO)
!                             BULLETIN (ONLY FOR SYNEXP TO GET IW)
!                 (6) IDATIM   ARRAY OF Y,M,D,H,M FROM BULLETIN   (I)
!                 (7) TTAAII   BULLETIN IDENTIFIER                (I)
!                 (8) CCCC     ORIGINATING CENTRE FOR BULLETIN    (I)
!                 (9) ICCCC    ORIGINATING CENTRE IN INTEGER      (I)
!                (10) MIMJ     'AAXX', 'BBXX' or 'OLDS'           (I)
!                             (' ' has been set to AAXX by SYNBUL)
!                (11) NFT      FT NUMBER FOR STORAGE OF THIS DATA (I)
!                (12) BLKSIZ   BLOCK SIZE OF DATA SET FOR THIS DATA (I)
!
! REVISION INFO :
!
!
! $Workfile: synob.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 16/03/2012 16:37:17$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         16/03/2012 16:37:17    John Norton     Set
!       iver=13 to force storage to use old version of BUFR descriptors
!       (radiation).
!  2    MetDB_Refresh 1.1         10/01/2011 14:51:06    Rosemary Lavery
!       corrections after review
!  1    MetDB_Refresh 1.0         04/01/2011 16:36:47    Rosemary Lavery
!       Initial Import
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE OBHOUR_mod
USE DATIM_mod
USE METDB_COM_mod, ONLY : RMISS   ! missing data valuea value
USE SYNEXP_mod
USE ENBUFR_mod
USE SYNIND_mod

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)     :: REPORT
INTEGER, INTENT(IN)               :: REPLEN
LOGICAL, INTENT(IN)               :: OCOR
CHARACTER (LEN=2), INTENT(IN)     :: CORNUM
CHARACTER (LEN=5), INTENT(INOUT)  :: YYGGIW
INTEGER, INTENT(IN)               :: IDATIM(5)
CHARACTER (LEN=6), INTENT(IN)     :: TTAAII
CHARACTER (LEN=4), INTENT(IN)     :: CCCC
INTEGER, INTENT(IN)               :: ICCCC
CHARACTER (LEN=4), INTENT(IN)     :: MIMJ
INTEGER, INTENT(IN)               :: NFT
INTEGER, INTENT(IN)               :: BLKSIZ

! Local Parameters

INTEGER, PARAMETER  :: IVER = 13  ! Use version 13 pre-radiation change

! Local Variables

INTEGER  :: DAY
INTEGER  :: DESCR(500)
INTEGER  :: HOUR
INTEGER  :: I
INTEGER  :: IDES
INTEGER  :: IL
INTEGER  :: IVALUE
INTEGER  :: LAT
INTEGER  :: LEN
INTEGER  :: LONG
INTEGER  :: NDES
INTEGER  :: NELEM
INTEGER  :: NELM
INTEGER  :: NOBS
INTEGER  :: NOW(8)
INTEGER  :: Q
INTEGER  :: RC
INTEGER  :: REPDAT(5)
INTEGER  :: TOL
INTEGER  :: TOR(5)

REAL     :: ARRAY(300)
REAL     :: LATLON(4)

LOGICAL  :: CMPRES = .FALSE.
LOGICAL  :: SYNTAX

CHARACTER (LEN=9)      :: ID
CHARACTER (LEN=9)      :: IDENT
CHARACTER (LEN=10000)  :: MESSAG
CHARACTER (LEN=13)     :: MSGHDR = '....Z SYNOB: '

COMMON /SYNMES/ MESSAG,DESCR,ARRAY

! --------------------------------------------------------------------

! PASS COPY OF BULLETIN DATE/TIME IN CASE IT IS RESET FOR REPORT!

DO I=1,5
  REPDAT(I)=IDATIM(I)
END DO
!**********************************************************************
!
! IF SHIP, HANDLE CALL SIGN, TIME GROUP & LAT/LONG HERE
!
!**********************************************************************

IFBLOCK1: &
IF (MIMJ == 'BBXX' .OR. MIMJ == 'OOXX') THEN
  IF (REPLEN > 20) THEN
    IL=INDEX(REPORT(1:20),' 99')       ! FIND LATITUDE GROUP
  ELSE
    IL=INDEX(REPORT(1:REPLEN),' 99')
  END IF

  IF (IL < 10) RETURN                  ! WITH C/S & TIME IN FRONT

! WE HAVE FOUND 99LAT. THERE SHOULD BE TWO GROUPS BEFORE IT, THE CALL
! SIGN & YYGGIW: ASSUME THAT ORDER UNLESS 2ND GROUP IS NOT 5 FIGURES

  ID=REPORT(1:IL-7)                    ! ASSUME CALL SIGN FIRST,
  YYGGIW=REPORT(IL-5:IL-1)             ! THEN DATE/TIME GROUP

  DAY=IVALUE(YYGGIW(1:2))              ! GOOD DAY?
  HOUR=IVALUE(YYGGIW(3:4))             ! GOOD HOUR?
  IF (DAY < 0.OR.DAY > 31 .OR. HOUR < 0.OR.HOUR >= 24) THEN
    YYGGIW=REPORT(1:5)                 ! IF NOT,
    ID=REPORT(7:IL-1)                  ! TRY CALL SIGN LAST.
  END IF

  DAY=IVALUE(YYGGIW(1:2))
  HOUR=IVALUE(YYGGIW(3:4))
  IF (DAY < 0.OR.DAY > 31 .OR. HOUR < 0.OR.HOUR >= 24) RETURN

! INCREASE TOLERANCE FOR MOBSYN AS DELAYED OBS. ARE OFTEN RECEIVED.

  TOL = 3
  IF (MIMJ == 'OOXX') TOL = 24
  CALL OBHOUR(REPDAT,DAY,HOUR,MIMJ,TOL,RC)
  IF (RC == 4) RETURN                  ! TOLERANCE EXCEEDED
  REPDAT(5)=0                          ! ZERO MINUTES

!-----------------------------------------------------------------------
! GET LAT & LONG (IN TENTHS) & PUT THEM IN A REAL ARRAY FOR SYNEXP.
! SAFETY CHECKS ADDED FOR SHORT REPORTS.
!-----------------------------------------------------------------------

  IF ((IL+11) <= REPLEN) THEN
    LAT=IVALUE(REPORT(IL+3:IL+5))      ! LATITUDE
    Q=IVALUE(REPORT(IL+7:IL+7))        ! QUADRANT
    LONG=IVALUE(REPORT(IL+8:IL+11))    ! LONGITUDE
  ELSE
    PRINT*,'SYNOB: NO LAT,Q & LON - SHORT REPORT: ',  &
    REPORT(1:REPLEN)
    RETURN
  END IF

IFBLOCK2: &
  IF (Q == 1.OR.Q == 3.OR.Q == 5.OR.Q == 7)THEN

    IF (LAT < 0 .OR. LAT > 900)THEN
      WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)
      PRINT*,MSGHDR,'BAD SYNOP LATITUDE: ',REPORT(1:REPLEN)
      RETURN
    ELSE
      IF (Q == 3 .OR. Q == 5) LAT=-LAT     ! SOUTH NEGATIVE
      LATLON(1)=LAT/10.
    END IF

    IF(LONG < 0 .OR. LONG > 1800)THEN
      WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)
      PRINT*,MSGHDR,'BAD SYNOP LONGITUDE: ',REPORT(1:REPLEN)
      RETURN
    ELSE
      IF (Q == 5 .OR. Q == 7) LONG=-LONG   ! WEST NEGATIVE

      LATLON(2)=LONG/10.
    END IF
  ELSE
    WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)
    PRINT*,MSGHDR,'BAD SYNOP QUADRANT: ',REPORT(1:REPLEN)
    RETURN                             !INVALID QUADRANT
  END IF IFBLOCK2

  IL=IL+13                             ! PAST LAT/LONG GROUPS
  IF(IL > REPLEN)THEN
    WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)
    PRINT*,MSGHDR,' IN SYNOB - BAD SYNOP REPORT LENGTH >', &
           REPORT(1:REPLEN),'< IL=',IL,' REPLEN=',REPLEN
    RETURN
  END IF
ELSE                                   ! IF NOT SHIP...
  IL=1
  ID=REPORT(1:5)
END IF IFBLOCK1
!**********************************************************************
!
! SET ARRAY TO MISSING & CALL EXPANSION ROUTINE FOR REST OF REPORT.
! IF EXPANSION HAS FAILED TO FIND ANY ELEMENTS, PRINT AND GIVE UP.
!
!**********************************************************************
DO I=1,300
  ARRAY(I)=RMISS
END DO

CALL DATIM(NOW)
DO I=0,4
  TOR(1+I)=NOW(8-I)
END DO

CALL SYNEXP(ID,REPORT(IL:),REPLEN-IL+1,ARRAY,      &
            YYGGIW,REPDAT,LATLON,MIMJ,SYNTAX,NELM)
IF (NELM == 0) THEN
  WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)
  PRINT*,MSGHDR,'BAD SYNOP >',REPORT(IL:REPLEN),'<'
  RETURN
END IF

! Add check for block 99 KAWN SYNOPS
IF (ID(1:2) == '99'.AND.CCCC == 'KAWN')THEN
  WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)
  PRINT *,'SYNOB: block 99 KAWN SYNOP rejected >',  &
  REPORT(IL:REPLEN),'<'
  RETURN
END IF

!**********************************************************************
!
! CALL BUFR ENCODING ROUTINE, WITH CURRENT TIME AS TIME OF RECEIPT TO
! GO IN SECTION 1.  PUT REPORT IN OUTPUT STRING BEFORE BUFR MESSAGE.
!
!**********************************************************************
MESSAG(1:REPLEN)=REPORT

IF (MIMJ == 'BBXX') THEN
  DESCR(1)=IDES(302200)       ! Ship sequence
ELSE IF (MIMJ == 'OOXX') THEN
  DESCR(1)=IDES(302205)       ! Mobile SYNOP sequence
ELSE
  DESCR(1)=IDES(302202)       ! Land SYNOP sequence
END IF

NDES=1
NELEM=300
NOBS=1
IDENT=ID  ! KEEP IDENT FOR INDEX: ENBUFR WILL CONVERT ID TO ASCII
CALL ENBUFR(DESCR,ARRAY,NDES,NELEM,NOBS,ID,TOR,  &
            MESSAG(REPLEN+1:),CMPRES,LEN,IVER)

! SET CCCC & LAND/SEA FLAG IN SECTION 1 OF THE BUFR MESSAGE
! (DISPLACEMENT AS FOR BUFR VERSION 1; CHANGE IF TOTAL LENGTH AT START)

MESSAG(REPLEN+9:REPLEN+9)=CHAR(ICCCC/256)
MESSAG(REPLEN+10:REPLEN+10)=CHAR(MOD(ICCCC,256))

IF (MIMJ == 'BBXX') THEN
  MESSAG(REPLEN+13:REPLEN+13)=CHAR(1)          ! sea
ELSE
  MESSAG(REPLEN+13:REPLEN+13)=CHAR(0)          ! land
END IF
!**********************************************************************
!
! PASS REPORT PLUS BUFR MESSAGE TO INDEX AND STORAGE ROUTINES.
!
!**********************************************************************
LEN=REPLEN+LEN                      ! REPORT + MESSAGE LENGTH
CALL SYNIND(MESSAG,LEN,OCOR,CORNUM,TTAAII,MIMJ,LATLON,  &
            CCCC,REPDAT,NFT,BLKSIZ,SYNTAX,NELM,IDENT)
RETURN
END SUBROUTINE SYNOB
