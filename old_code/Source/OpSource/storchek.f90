SUBROUTINE STORCHEK (BULL, BUL18, DATYPE, STORFLAG, MIMJ)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : STORCHEK
!
! PURPOSE       : STORCHEK IS USED TO CHECK THE DATA TYPE AND STORAGE
!                 REQUIREMENTS FOR CERTAIN TYPES OF DATA. FOR SOME
!                 DATA TYPES THE TYPE IS CHANGED (E.G. "TEMP", "PILOT"
!                 AND "DROPSOND" ALL BECOME "UPRAIR"), OR THE STORAGE
!                 FLAG IS CHANGED DEPENDING ON THE VALUE OF'MIMIMJMJ'
!                 IN THE MESSAGE.
!
! CALLED BY     : ANYTHING WHICH NEEDS TO STORE CHARACTER MESSAGES.
!
! USAGE         : CALL STORCHEK (BULL, MIMJ, DATYPE, STORFLAG)
!
! ARGUMENTS     : BULL      (INPUT)  CHARACTER*(*) MESSAGE TEXT.
!                 BUL18     (INPUT)  'TTAAII CCCC YYGGGG' (GTS HEADER).
!                 DATYPE    (IN/OUT) DATA TYPE (UP TO 8 CHARACTERS).
!                 STORFLAG  (IN/OUT) STORAGE FLAG (T=STORE IT, F=DON'T)
!                 MIMJ      (OUTPUT) 'MIMIMJMJ' DECODED FROM MESSAGE.
!
! CALLS         : ACHTST, DREG
!
! REVISION INFO :
!
! $Workfile: storchek.f90$ $Folder: OpSource$
! $Revision: 8$ $           $
!
! CHANGE RECORD :
!
! $Log:
!  8    MetDB_Refresh 1.7         06/05/2011 16:13:40    Brian Barwell   Adjust
!        message text and set NTMSG before calling DREG. Also remove obsolete
!       GOES SATOB test.
!  7    MetDB_Refresh 1.6         21/04/2011 17:19:15    Brian Barwell   Checks
!        on bulletin length added.
!  6    MetDB_Refresh 1.5         25/01/2011 15:47:35    Brian Barwell   BULL
!       back to INTENT(IN) after correction to DREG.
!  5    MetDB_Refresh 1.4         22/12/2010 13:02:26    Sheila Needham  Change
!        INTENT to INOUT on BULL (updated in DREG)
!  4    MetDB_Refresh 1.3         22/12/2010 10:28:31    Sheila Needham  Update
!        following review
!  3    MetDB_Refresh 1.2         21/12/2010 11:44:13    Richard Weedon
!       parameters updated for call to DREG. Int var NTMSG(8) added. 
!  2    MetDB_Refresh 1.1         16/12/2010 14:10:26    Richard Weedon  up to
!       F95 standard
!  1    MetDB_Refresh 1.0         07/12/2010 16:39:06    Richard Weedon
!       initial Draft
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
! Interfaces
USE achtst_mod
USE dreg_mod
!
IMPLICIT NONE

! Arguments

CHARACTER(LEN=*), INTENT(IN)    :: BULL      ! (1)  BULLETIN TEXT
CHARACTER(LEN=18),INTENT(IN)    :: BUL18     ! (2) 'TTAAII CCCC YYGGGG' (GTS HEADER)
CHARACTER(LEN=8), INTENT(INOUT) :: DATYPE    ! (3)  DATA TYPE     
LOGICAL,          INTENT(INOUT) :: STORFLAG  ! (4) 'STORAGE REQUIRED' FLAG
CHARACTER(LEN=7), INTENT(OUT)   :: MIMJ      ! (5) 'MIMIMJMJ' FROM MESSAGE

! Local variables

INTEGER           :: ICH       ! INDEX FOR LOC IN "BULL"
INTEGER           :: LENBUL    ! LENGTH OF "BULL"
INTEGER           :: LENTYP    ! DATA TYPE NAME LENGTH
INTEGER           :: NONLET    ! INT ARGUMENT FOR "ACHTST"
INTEGER           :: NTMSG(8)  ! DATE TIME ARRAY FOR DREG
LOGICAL           :: ALLETT    ! ARG FOR CALL TO "ACHTST"
LOGICAL           :: OSPACE    ! ARG FOR CALL TO "ACHTST"
LOGICAL           :: OLFCR     ! ARG FOR CALL TO "ACHTST"
LOGICAL           :: ERROR     ! FLAG FOR PROD DREGS ENTRY
CHARACTER         :: CH        ! CHARACTER FROM "BULL"
CHARACTER(LEN=40) :: TEXT40    ! ERROR MESS FOR DREGS ENT

!=======================================================================
! (1) BULLETIN PREPROCESSING - LOOK FOR 'MIMIMJMJ'
!=======================================================================

MIMJ = ''
ERROR = .FALSE.
LENBUL = LEN(BULL)

!     LOCATE FIRST LETTER IN FIRST 10 CHARACTERS

ICH = 1
CH = BULL(1:1)
DO WHILE (ICH <= 10 .AND. (CH < 'A' .OR. CH > 'Z' .OR.  &
     (CH > 'I' .AND. CH < 'J') .OR.           &
     (CH > 'R' .AND. CH < 'S'))) ! NOT A LETTER
   ICH = ICH + 1
   IF (ICH > LENBUL) EXIT
   CH = BULL(ICH:ICH)
END DO

!      EXTRACT 'MIMIMJMJ'

if_constr1 : &
IF (ICH <= 10 .AND. ICH+7 <= LENBUL) THEN ! LETTER WAS FOUND
   CALL ACHTST (ICH, 8, ALLETT, NONLET, OSPACE, OLFCR, BULL)
   IF (ALLETT.OR.(OSPACE.OR.OLFCR)) THEN
      IF (ALLETT) NONLET = 9
      MIMJ = BULL(ICH:ICH+NONLET-2)
      IF (MIMJ == 'NIL' .OR. MIMJ == 'OTR') MIMJ = ' '
   END IF
!                IF 'SHPANC' FOUND (COMMON IN SHPSYN FROM PANC) LOOK
!                                   FOR 'BBXX' IN NEXT 10 CHARACTERS
   IF (MIMJ == 'SHPANC') THEN
      ICH = ICH + NONLET
      IF (ICH+9 <= LENBUL) THEN
         IF (INDEX(BULL(ICH:ICH+9),'BBXX') > 0) MIMJ = 'BBXX'
      ELSE
         MIMJ = ' '
      END IF
   END IF
END IF if_constr1

!=======================================================================
! (2) ADDITIONAL CHECKS FOR VARIOUS DATA TYPES
!=======================================================================

!                        "UPRAIR" COVERS "TEMP", "PILOT" AND "DROPSOND"
if_constr2 : &
IF (DATYPE == 'TEMP' .OR. DATYPE == 'PILOT' .OR. DATYPE == 'DROPSOND') THEN
    DATYPE = 'UPRAIR'

!         CHANGE "SYNOP" TO "LNDSYN", "SHPSYN" OR "MOBSYN"
!         (OR "NCM" IF MIMJ='OLDC' AS THESE COME IN WITH SYNOP HEADERS)

ELSE IF (DATYPE == 'SYNOP') THEN if_constr2
   IF (MIMJ == 'OLDC') THEN
      DATYPE = 'NCM'
   ELSE IF (MIMJ == 'BBXX') THEN
      DATYPE = 'SHPSYN'
   ELSE IF (MIMJ == 'AAXX' .OR. MIMJ == 'OLDS' .OR. MIMJ == ' ') THEN
      DATYPE = 'LNDSYN'
   ELSE IF (MIMJ == 'OOXX') THEN
      DATYPE = 'MOBSYN'
   ELSE
      STORFLAG = .FALSE.
      LENTYP = 5
      ERROR = .TRUE.
   END IF

!     "SPECIAL" STORED WITH "METARS"

ELSE IF (DATYPE == 'SPECIAL') THEN if_constr2
   DATYPE = 'METARS'

!     CHECK MIMJ FOR SUBSEA
!     'MiMiMjMj' =   JJ..   KK..   MMXX     NNXX
!     'data type =  BATHY  TESAC  WAVEOB  TRACKOB

ELSE IF (DATYPE == 'SUBSEA') THEN if_constr2
   IF (MIMJ == 'NNXX') THEN
      DATYPE = 'TRKOB'
   ELSE IF (MIMJ /= 'JJVV' .AND.MIMJ /= 'JJXX' .AND. MIMJ /= 'JJYY' &
      .AND. MIMJ /= 'KKXX' .AND.MIMJ /= 'KKYY') THEN
      STORFLAG = .FALSE.
      LENTYP = 6
      IF (MIMJ /= 'MMXX') ERROR = .TRUE. ! (I.E. NOT WAVEOB)
   END IF

!     CHECK MIMJ = 'ZZYY' FOR BUOYS

ELSE IF (DATYPE == 'BUOY') THEN if_constr2
   IF (MIMJ /= 'ZZYY') THEN
      STORFLAG = .FALSE.
      LENTYP = 4
      ERROR = .TRUE.
   END IF

!    CHECK MIMJ = 'YYXX' FOR SATOBS

ELSE IF (DATYPE == 'SATOBS') THEN if_constr2
   IF (MIMJ /= 'YYXX') THEN
      STORFLAG = .FALSE.
      LENTYP = 6
      ERROR = .TRUE.
   END IF
END IF  if_constr2

!=======================================================================
! (3) PRINT MESSAGE & WRITE DREGS ENTRY IF FAILED CHECKS
!=======================================================================

IF (ERROR) THEN
   IF (MIMJ == ' ') THEN
      ICH = 20
      TEXT40 = 'NO "MIMIMJMJ" CODED IN BULLETIN.'
   ELSE
      ICH = 21
      TEXT40 = 'UNEXPECTED "MIMIMJMJ" = ' // MIMJ
   END IF
   NTMSG(:) = 0
   CALL DREG (ICH, TEXT40, BULL, 'STORCHEK', DATYPE, BUL18,NTMSG,' ')
   WRITE (6,'(T5,A,T15,3A)') 'STORCHEK:',&
      DATYPE(1:LENTYP), ' REPORT WITH ', TEXT40
END IF

RETURN
END SUBROUTINE STORCHEK
