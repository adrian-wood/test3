SUBROUTINE WINPRO(BULL,IFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : WINPRO IN MDBSTOR
!
! PURPOSE       : TO SPLIT WIND PROFILER BULLETIN INTO PROFILES,
!                 RE-ENCODE & CALL AIRSTO TO STORE EACH PROFILE.
!
! DATA TYPE(S)  : WIND PROFILERS
!
! CALLED BY     : MDBSTOR
!                 (WITH ANY BUFR MESSAGE OF TYPE 2 OR 6 IN JAN 97!)
!
! CALLS         : DEBUFR, ENBUFR, AIRSTO, EB2ASC, DATIM,
!                 DESFXY, CENTURY, INDLALO, BUFRHED
!
! PARAMETERS    : (1) BULLETIN
!                 (2) FT NUMBER FOR STORAGE
!
! REVISION INFO :
!
!
! $Workfile: winpro.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 16/12/2010 15:02:29$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         16/12/2010 15:02:29    Stan Kellett    SAVE
!       added as default on GPCS is SAVE for f77 but it is bad practice to
!       assume this 
!  2    MetDB_Refresh 1.1         16/12/2010 13:31:08    Alison Weir
!       Changes following review - IVER=13, FIRST SAVE
!  1    MetDB_Refresh 1.0         14/12/2010 16:21:09    Rosemary Lavery
!       Initial Port
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

USE AIRSTO_MOD
USE BUFRHED_MOD
USE CENTURY_MOD
USE DATIM_MOD
USE DEBUFR_MOD
USE DESFXY_MOD
USE EB2ASC_MOD
USE ENBUFR_MOD
USE INDLALO_MOD

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)  :: BULL  ! (A1)
INTEGER, INTENT(INOUT)         :: IFT   ! (A2)

! Local Parameters

INTEGER, PARAMETER             :: KINDR6 = SELECTED_REAL_KIND(6)
INTEGER, PARAMETER             :: BLKSIZ = 27998
INTEGER, PARAMETER             :: NH=25
INTEGER, PARAMETER             :: NR=11
INTEGER, PARAMETER             :: IVER = 13
REAL (KIND=KINDR6), PARAMETER  :: PI = 3.14159

! Local Variables

INTEGER   :: DESCR(9999)
INTEGER   :: DESC(3333)
INTEGER   :: DATIME(5)
INTEGER   :: TOR(5)
INTEGER   :: NOW(8)
INTEGER   :: F
INTEGER   :: X
INTEGER   :: Y
INTEGER   :: NDTOTAL
INTEGER   :: IBUFR
INTEGER   :: IDES
INTEGER   :: NOB
INTEGER   :: NOBS
INTEGER   :: I
INTEGER   :: IC
INTEGER   :: ID
INTEGER   :: NIL = 0     ! for arguments passed
INTEGER   :: NIL2(2) = (/ 0,0 /)
INTEGER   :: NIL3(3) = (/ 0,0,0 /)
INTEGER   :: NIL5(5) = (/ 0,0,0,0,0 /)
INTEGER   :: K
INTEGER   :: L
INTEGER   :: LM
INTEGER   :: ND
INTEGER   :: NL
INTEGER   :: NM

REAL      :: VALUES(9999)
REAL      :: VAL(3333)
REAL      :: MS = -9999999.
REAL      :: PI_OVER_180 = PI/180.
REAL      :: HT
REAL      :: DD
REAL      :: FF
REAL      :: U
REAL      :: V

LOGICAL               :: FIRST = .TRUE.
LOGICAL               :: CMPRES          ! set if message compressed

CHARACTER (LEN=3333)  :: MESSAGE
CHARACTER (LEN=5)     :: NAMES
CHARACTER (LEN=23)    :: ENTRY
CHARACTER (LEN=4)     :: BUFR = 'BUFR'
CHARACTER (LEN=9)     :: IDENT = '         '
CHARACTER (LEN=1)     :: BLNK = ' '      ! for arguments passed

COMMON /WINDS/ DESCR,DESC, VALUES,VAL, MESSAGE

SAVE

! ----------------------------------------------------------------------

! First call only
IF (FIRST) THEN
  CALL EB2ASC(4,BUFR)
  FIRST=.FALSE.
END IF

IC=INDEX(BULL,'I')

CALL DATIM(NOW)
DO I=1,5
  TOR(I)=NOW(9-I)
END DO

! COME BACK TO HERE WITH IBUFR UPDATED TO SEE IF THERE'S ANOTHER BUFR
! MESSAGE IN THE BULLETIN

IBUFR=INDEX(BULL,BUFR)

! These 3 below are the return points from the subroutine
   10 CONTINUE
      IF (IBUFR+3 > LEN(BULL)) RETURN
IF (BULL(IBUFR:IBUFR+3) /= BUFR) RETURN

ND=9999
NOBS=9999
CALL DEBUFR(DESCR,VALUES,NAMES,ND,NOBS,BULL(IBUFR:),.FALSE.)

! If the decode fails (no obs or no descriptors), give up.

IF (NOBS == 0 .OR. ND == 0) RETURN

! See if message is compressed.  So far (March 2003) messages with
! NOBS>1 have not been compressed, which means the descriptors are
! expanded several times in DESCR; but nothing output from DEBUFR
! indicates this, so get the compression flag directly from the
! message in case compression is ever used.

CALL BUFRHED(BULL(IBUFR:),NIL,NIL3,NIL2,NIL2,NIL5,BLNK,BLNK,CMPRES, &
             NIL,NIL,DESCR,.FALSE.)

!----------------------------------------------------------------------
! IN JAN 97 WE WANT TO STORE DATA FROM TWO KINDS OF BULLETIN:
! - AMERICAN PROFILES HAVE SURFACE DATA & U,V,W PROFILES WITH FIXED
!    INCREMENTS (SEVERAL OBS PER MESSAGE, NOT COMPRESSED).
! - EUROPEAN PROFILES HAVE NO SURFACE DATA & DD,FF,W PROFILES WITH
!    EXPLICIT HEIGHTS (SEVERAL obs per bulletin, as above).
! BOTH HAVE LOTS OF RADAR ELEMENTS - BUT ALL THE ELEMENTS ARE DIFFERENT.
! (THERE ARE ACTUALLY TWO KINDS OF EUROPEAN MESSAGE, ONE OF THEM WITH
! NOTHING BUT RADAR DATA - IGNORE THESE.)

! THIS ATTEMPT KEEPS METEOROLOGICAL DATA, U & V RATHER THAN DD & FF,
! with EXPLICIT HEIGHTS IN THE PROFILE, and also signal-to-noise ratio.
! Other RADAR ELEMENTS COULD "EASILY" BE added (add code to recognise
! OTHER DESCRIPTORS & PUT THE ELEMENTS in the output sequence).

! THE STRUCTURE OF THE OUTPUT MESSAGE IS: PRELIMINARIES (STATION, TIME,
! PLACE, SURFACE WEATHER, INSTRUMENTATION) THEN REPLICATE HT, U,V,W
! WITH Q/C FLAGS & signal-to-noise ratio.
!----------------------------------------------------------------------

DO_OBS: &
DO NOB=1,NOBS
  DO I=1,3333
    VAL(I)=MS           ! SET OUTPUT VALUE ARRAY TO MISSING
  END DO

! If there are several obs not compressed, later obs can have
! different numbers of levels and hence of descriptors, so point
! to a later expansion of the sequence in the descriptor array.
! Note: VALUES is effectively a 2-D array: VALUES(NOBS,*)

  IF (NOB == 1 .OR. CMPRES) THEN
    NDTOTAL=0
  ELSE
    NDTOTAL=NDTOTAL+ND+1
    ND=DESCR(NDTOTAL)
  END IF

  LM=0                  ! ELEMENT NUMBER IN VALUES
  NL=0                  ! NUMBER OF LEVELS

DOLABEL2: &
  DO I=NDTOTAL+1,NDTOTAL+ND
    CALL DESFXY(DESCR(I),F,X,Y)

IF_ELEM: &
    IF (F == 0) THEN    ! IGNORE ANY OPERATORS
      LM=LM+1           ! NEXT ELEMENT

IF_XY: &
      IF (X == 1 .AND. Y == 1) THEN
        VAL(1)=VALUES((LM-1)*NOBS+NOB)  ! BLOCK NUMBER
      ELSE IF (X == 1 .AND. Y == 2) THEN
        VAL(2)=VALUES((LM-1)*NOBS+NOB)  ! STATION NUMBER
      ELSE IF (X == 2 .AND. Y == 1) THEN
        VAL(3)=VALUES((LM-1)*NOBS+NOB)  ! STATION TYPE

      ELSE IF (X == 4 .AND. Y == 1) THEN
        VAL(4)=VALUES((LM-1)*NOBS+NOB)  ! YEAR
        IF (VAL(4) < 100.) THEN         ! 2-fig year from LFPW
          VAL(4)=VAL(4)+CENTURY(NINT(0+VAL(4)))
        END IF
      ELSE IF (X == 4 .AND. Y == 2) THEN
        VAL(5)=VALUES((LM-1)*NOBS+NOB)  ! MONTH
      ELSE IF (X == 4 .AND. Y == 3) THEN
        VAL(6)=VALUES((LM-1)*NOBS+NOB)  ! DAY
      ELSE IF (X == 4 .AND. Y == 4) THEN
        VAL(7)=VALUES((LM-1)*NOBS+NOB)  ! HOUR
      ELSE IF (X == 4 .AND. Y == 5) THEN
        VAL(8)=VALUES((LM-1)*NOBS+NOB)  ! MINUTE

! Recognise 005001 as well as 005002 (hundredths)

      ELSE IF (X == 5 .AND. (Y == 2 .OR. Y == 1)) THEN
        VAL(9)=VALUES((LM-1)*NOBS+NOB)  ! LATITUDE
      ELSE IF (X == 6 .AND. (Y == 2 .OR. Y == 1)) THEN
        VAL(10)=VALUES((LM-1)*NOBS+NOB) ! LONGITUDE

      ELSE IF (X == 7 .AND. Y == 1) THEN
        HT=VALUES((LM-1)*NOBS+NOB)      ! STATION HEIGHT
        VAL(11)=HT                      ! INIT VERTICAL COORD
        VAL(NH+1)=HT                    ! SURFACE HEIGHT

! SURFACE WEATHER (US DATA ONLY)

      ELSE IF (X == 10 .AND. Y == 51) THEN
        VAL(12)=VALUES((LM-1)*NOBS+NOB) ! MSL PRESSURE
      ELSE IF (X == 12 .AND. Y == 1) THEN
        VAL(13)=VALUES((LM-1)*NOBS+NOB) ! SURFACE TEMP
      ELSE IF (X == 13 .AND. Y == 14) THEN
        VAL(14)=VALUES((LM-1)*NOBS+NOB) ! RAINFALL
      ELSE IF (X == 13 .AND. Y == 3) THEN
        VAL(15)=VALUES((LM-1)*NOBS+NOB) ! SURFACE REL HUM

! INSTRUMENTATION (EUROPEAN ONLY)

      ELSE IF (X == 2 .AND. Y == 3) THEN
        VAL(16)=VALUES((LM-1)*NOBS+NOB) ! MEASURING EQUIPMENT
      ELSE IF (X == 2 .AND. Y == 101) THEN
        VAL(17)=VALUES((LM-1)*NOBS+NOB) ! TYPE OF ANTENNA
      ELSE IF (X == 2 .AND. Y == 106) THEN
        VAL(18)=VALUES((LM-1)*NOBS+NOB) ! 3DB BEAMWIDTH
        IF (VAL(18) >= 6.25) VAL(18) = MS
      ELSE IF (X == 2 .AND. Y == 121) THEN
        VAL(19)=VALUES((LM-1)*NOBS+NOB) ! MEAN FREQUENCY
        IF (VAL(19) >= 2.0465E+8) VAL(19) = MS

      ELSE IF (X == 25 .AND. Y == 1) THEN
        VAL(20)=VALUES((LM-1)*NOBS+NOB) ! RANGE-GATE LENGTH

      ELSE IF (X == 25 .AND. Y == 20) THEN
        VAL(21)=VALUES((LM-1)*NOBS+NOB) ! MEAN SPEED EST
      ELSE IF (X == 25 .AND. Y == 21) THEN
        VAL(22)=VALUES((LM-1)*NOBS+NOB) ! WIND COMPUT ENHANCE
      ELSE IF (X == 4 .AND. Y == 25) THEN
        VAL(23)=2                       ! 008021 --> AVERAGED
        VAL(24)=VALUES((LM-1)*NOBS+NOB) ! AVERAGING PERIOD (MINS)
        IF (VAL(24) /= MS .AND. VAL(24) < 0)   &
            VAL(24)=-VAL(24)                    ! US <0: MAKE IT >0

!----------------------------------------------------------------------
! END OF HEADER ELEMENTS.  NOW REPLICATE WINDS AT DIFFERENT HEIGHTS.
!----------------------------------------------------------------------

! A HEIGHT ITSELF (EURO) OR AN INCREMENT (US) STARTS A NEW LEVEL
! (BUT MAKE SURE THERE IS A WIND FOR THE LAST LEVEL FIRST!)
! (There was a check below for heights out of order, a fix to cope
! with inadequate decode output; but it lost data for 10772, which
! reports station height 109m & first height 100m - check removed)
!  Test for descriptor '8450' (= hex '1F02', i.e. BUFR 033002) has
! been added because Austrian profiler data has this QC descriptor
! in the data instead of using associated field bits (204001).
!  Note: NH is the number of descriptors before the replication
! and NR the number of replicated descriptors so VAL(NH+NL*NR+N)
! is the Nth descriptor for the (NL+1)th replication.

      ELSE IF ((X == 7 .AND. Y == 5) .OR.  &
               (X == 7 .AND. Y == 7) .OR.  &
               (X == 10.AND. Y == 7)) THEN
        IF (Y == 5) HT=HT+VALUES((LM-1)*NOBS+NOB)  ! INCREMENT
        IF (Y == 7) HT=VALUES((LM-1)*NOBS+NOB)     ! RESET
        K=NH+NL*NR
        IF (VAL(K+6) /= MS .OR. VAL(K+7) /= MS .OR.  &
            VAL(K+8) /= MS) THEN
          NL=NL+1                               ! NEW LEVEL
          VAL(NH+NL*NR+1)=HT                    ! HEIGHT

! If there's no data for this level, reset data height to next.

        ELSE
          VAL(NH+NL*NR+1)=HT                    ! HEIGHT
        END IF

      ELSE IF (X == 25 .AND. Y == 32) THEN
        VAL(NH+NL*NR+2)=VALUES((LM-1)*NOBS+NOB) ! PROFILER MODE
      ELSE IF (X == 25 .AND. Y == 34) THEN
        VAL(NH+NL*NR+3)=VALUES((LM-1)*NOBS+NOB) ! US Q/C FLAGS

! CONVERT DD & FF TO U & V.
! CHECK FOR Q/C BIT(S), PROBABLY ONLY WITH DD.

      ELSE IF (X == 11 .AND. Y == 1) THEN
        DD=VALUES((LM-1)*NOBS+NOB)      ! WIND DIRECTION
        IF (DESCR(I-1) == 0) THEN                 ! IF BIT ADDED
          VAL(NH+NL*NR+4)=VALUES((LM-2)*NOBS+NOB) ! HORIZ Q/C FLAG
        ELSE IF (DESCR(I+1) == 8450) THEN         ! Q/C INFO
          VAL(NH+NL*NR+4)=VALUES((LM)*NOBS+NOB)   ! HORIZ Q/C
        END IF
      ELSE IF (X == 11 .AND. Y == 2) THEN
        FF=VALUES((LM-1)*NOBS+NOB)      ! WIND SPEED
        IF (DESCR(I-1) == 0) THEN                 ! IF BIT ADDED,
          VAL(NH+NL*NR+4)=VALUES((LM-2)*NOBS+NOB) ! HORIZ Q/C FLAG
        ELSE IF (DESCR(I+1) == 8450) THEN         ! Q/C INFO
          VAL(NH+NL*NR+4)=VALUES((LM)*NOBS+NOB)   ! HORIZ Q/C
        END IF
        IF (DD /= MS .AND. FF /= MS) THEN
          U=-FF*SIN(DD*PI_OVER_180)
          V=-FF*COS(DD*PI_OVER_180)
          VAL(NH+NL*NR+6)=U
          VAL(NH+NL*NR+7)=V
        END IF

! IF U & V REPORTED, STORE THEM AS THEY ARE.
! W CAN HAVE A Q/C BIT IN EUROPEAN DATA.

      ELSE IF (X == 11 .AND. Y == 3) THEN
        VAL(NH+NL*NR+6)=VALUES((LM-1)*NOBS+NOB) ! U COMPONENT
      ELSE IF (X == 11 .AND. Y == 4) THEN
        VAL(NH+NL*NR+7)=VALUES((LM-1)*NOBS+NOB) ! V COMPONENT

      ELSE IF (X == 11 .AND. Y == 6) THEN
        VAL(NH+NL*NR+8)=VALUES((LM-1)*NOBS+NOB) ! W COMPONENT
        IF (DESCR(I-1) == 0) THEN               ! IF ADDED BIT(S)
          VAL(NH+NL*NR+5)=VALUES((LM-2)*NOBS+NOB) ! VERT Q/C FLAG
        ELSE IF (DESCR(I+1) == 8450) THEN         ! Q/C INFO
          VAL(NH+NL*NR+5)=VALUES((LM)*NOBS+NOB)   ! VERT Q/C
        END IF

      ELSE IF (X == 11 .AND. Y == 50) THEN
        VAL(NH+NL*NR+9)=VALUES((LM-1)*NOBS+NOB) ! STD DEV (HORIZ)
      ELSE IF (X == 11 .AND. Y == 51) THEN
        VAL(NH+NL*NR+10)=VALUES((LM-1)*NOBS+NOB) ! STD DEV (VERT)
      ELSE IF (X == 21 .AND. Y == 30) THEN
        VAL(NH+NL*NR+11)=VALUES((LM-1)*NOBS+NOB) ! S/N RATIO
      END IF IF_XY
    END IF IF_ELEM
  END DO DOLABEL2

! Diagnostic Print
!               WRITE (*,'(12F6.1)')  (VAL(IJ),IJ=1,NH)
!               WRITE (*,'(11F8.2)')  (VAL(IJ),IJ=NH+1,NH+NR*NL)

! AFTER STORING THE NUMBER OF LEVELS WE CAN ENCODE THIS PROFILE
! (FIRST UPDATE NL TO INCLUDE LAST LEVEL - IF WIND FOUND)

  K=NH+NL*NR
  IF (VAL(K+6) /= MS.OR.VAL(K+7) /= MS.OR.VAL(K+8) /= MS) NL=NL+1
  VAL(NH)=NL           ! number of levels with winds
  DESC(1)=IDES(309251)
  NM=1
  CALL ENBUFR(DESC,VAL,NM,NH+NL*NR,1,            &
              NAMES,TOR,MESSAGE,.FALSE.,L,IVER)
  TOR(1)=NOW(8)                  ! RESET YEAR (<100 AFTER ENBUFR)

  DATIME(1)=VAL(4)               ! YEAR
  DATIME(2)=VAL(5)
  DATIME(3)=VAL(6)
  DATIME(4)=VAL(7)
  DATIME(5)=VAL(8)               ! MINUTE
  ENTRY(2:2)=CHAR(DATIME(5))     ! PUT MINUTE IN INDEX ENTRY TOO

  CALL INDLALO(ENTRY,VAL(9),VAL(10))

  ID=VAL(1)*1000+VAL(2)
  WRITE (IDENT(1:5),'(I5.5)') ID ! station number in figures
  ENTRY(3:6)=BULL(IC:IC+3)       ! TTAA
  ENTRY(8:11)=BULL(IC+7:IC+10)   ! CCCC
  ENTRY(12:12)=CHAR(1)           ! NUMBER OF OBS IN MESSAGE

  CALL AIRSTO(DATIME,ENTRY,MESSAGE(:L),IFT,BLKSIZ,IDENT,TOR)
END DO DO_OBS

! GET LENGTH OF MESSAGE (ASSUMING BUFR EDITION OF 2 OR MORE) AND
! INCREMENT POINTER TO SEE IF ANOTHER BUFR MESSAGE FOLLOWS.

L=ICHAR(BULL(IBUFR+6:IBUFR+6))*256+ICHAR(BULL(IBUFR+7:IBUFR+7))

IBUFR=IBUFR+L

GO TO 10

END SUBROUTINE WINPRO
