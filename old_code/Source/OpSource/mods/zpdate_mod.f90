!-----------------------------------------------------------------------
! ZPDATE - MetDB version
! ------
!
! REVISION INFO  :
!
! $Revision: 1$
! $Date: 15/11/2010 10:45:11$
!
! CHANGE HISTORY :
!
! $Log:
!  1    MetDB_Refresh 1.0         15/11/2010 10:45:11    Sheila Needham
!       Initial
! $
! Revision 1.2  2003/01/30 15:32:35  usmdb
! Now MetDB version as a bug found in the original. DATE31 and
! DATE32 problems with the MONTHS array when the month is
! January - S.Cox
!
! 7 Updated 17/4/98 by Stephen Turner
!                       Converted to F90 and added Zeller Method
! 6 Updated 23/3/98 by Edward Jones
!                       Added JDAY  Routine
! 5 Updated 17/2/98 by Edward Jones
!                       Added DATCHK and MNTHDS Routines
! 4 Updated 30/1/98 by Edward Jones
!                       Update ZPDATE subroutine
!                       Added ISALEAP subroutine
!                       Ported to HP, Cray and PC from MET.SRCELIB
! 3 RE-COMPILED 29/10/91 TO PRODUCE 31 BIT ADDRESSING MODE VERSION
!       BY M. COLLIER - COPIED TO MET.PROGLIB
! 2 RE-WRITE   BY PAUL WHITE  13/12/89
!                       TO MAKE RE-ENTRANT WITH INLINE EXPANSION
!                       TRANSLATED BY FPP 2.26B16 13/12/89  11:41:08
!                       SWITCHES: LSTOFF=T,OPTON=78,TDYON=FX
! 1 TRANSLATED BY FPP 2.26B16 11/12/89  11:44:56   TDYON=X
! 0 ORIGINAL VERSION BY JOHN PRINCE, LONG AGO IN THE MISTS OF TIME,
!                                     FOR THE IBM WITH STATIC MEMORY
!
!-----------------------------------------------------------------------

MODULE zpdate_mod
SAVE

CONTAINS
LOGICAL FUNCTION ISALEAP(IY)
!
!     Returns .TRUE. if IY is a Leap year
!     Returns .FALSE. if IY is not a Leap year
!
  IMPLICIT NONE
!					INPUT ARGUMENT
  INTEGER, INTENT(IN) :: IY


  IF (IY/4*4 .NE. IY) THEN			 ! Divide by 4
     ISALEAP=.FALSE.
  ELSE  		
    IF (IY/400*400 .EQ. IY) THEN	 ! Century check
       ISALEAP=.TRUE.
    ELSE
	  IF (IY/100*100 .EQ. IY) THEN   ! Century qualifier
         ISALEAP=.FALSE.
      ELSE
        ISALEAP=.TRUE.
      ENDIF
    ENDIF
  ENDIF
END FUNCTION ISALEAP

!-------------------------------------------------------------------------

SUBROUTINE  ZPDATE
!
!     Prints version information
!
  PRINT *, '  ZPDATE - f90 MetDB version'
  PRINT *, '  LAST MODIFIED $Date: 15/11/2010 10:45:11$'
  PRINT *, '  by Simon Cox (DD)'
  RETURN
END SUBROUTINE ZPDATE

!-----------------------------------------------------------------------
SUBROUTINE DATE21 (IDY, IY, ICD)
!
!     DAYS SINCE 1.1.1900, FROM DAY OF YEAR
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: IDY, IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: ICD
!                       LOCAL VARIABLES
  INTEGER :: IYN

  IYN = IY - 1900
  IF (IYN .GT. 0) THEN
     ICD = IDY + IYN*365 + (IYN-1)/4 - (IYN-1)/100 + (IYN+299)/400
  ELSE
     ICD = IDY + IYN*365 + IYN/4 - IYN/100
  ENDIF

END SUBROUTINE DATE21
!-----------------------------------------------------------------------
SUBROUTINE DATE23 (IDY, IY, ID, IM, INY)
!
!     DAY, MONTH, YEAR FROM DAY OF YEAR
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN) ::  IDY, IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: ID, IM, INY
!                       LOCAL VARIABLES
  INTEGER :: I, K, days_in_feb
  INTEGER, DIMENSION(12) :: MONTHS


  IF (ISALEAP(IY)) THEN
     days_in_feb = 29
  ELSE
     days_in_feb = 28
  ENDIF


  MONTHS = (/31,days_in_feb,31,30,31,30,31,31,30,31,30,31/)
  K = IDY

  DO I=1,12
    K = K - MONTHS(I)
    IF (K .GT. 0) THEN
       CYCLE
    ELSE
      ID = K + MONTHS(I)
      IM = I
      INY = IY
    ENDIF
    EXIT
  END DO

END SUBROUTINE DATE23
!-----------------------------------------------------------------------
SUBROUTINE DATE13 (ICD, ID, IM, INY)
!
!     DAY, MONTH, YEAR FROM DAYS SINCE 1.1.1900
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN) :: ICD
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: ID, IM, INY
!                       LOCAL VARIABLES
  INTEGER :: IDY, IY
  INTEGER :: K,KD,KE,KY,I,K1X, days_in_feb
  INTEGER, DIMENSION(12) :: MONTHS

  K = ICD
  KE = 0
  IF (K .GE. 366) THEN				!these allow for the non-leap years 1900,
     K = K + 1
     IF (K .GE. 73416) THEN         !2100,
        K = K + 1
        IF (K .GE. 109941) THEN     !2200,
           K = K + 1
           IF (K .GE. 146466) THEN  !2300 ...
       		  K = K + 1
           ENDIF
        ENDIF
     ENDIF
  ENDIF
  IF (K .LE. -36159) THEN			! and 1800 respectively
     K = K - 1
  ENDIF 	

  KY = K/1461*4
  KD = K - K/1461*1461
  IF (KD .LT. 0) THEN
     KD = KD + 1461
     KY = KY - 4
  ENDIF
  KY = KY + 1900
  IF (KD .GT. 366) THEN
     KD = KD - 1
     KE = KD/365
     KD = KD - KD/365*365
  ENDIF
  IF (KD .EQ. 0) THEN
     KE = KE - 1
     KD = 365
  ENDIF
  INY = KY + KE
  IDY = KD
  IY = INY

  IF (ISALEAP(IY)) THEN
     days_in_feb = 29
  ELSE
     days_in_feb = 28
  ENDIF

  MONTHS = (/31,days_in_feb,31,30,31,30,31,31,30,31,30,31/)
	
  K1X = IDY

  DO I=1,12
    K1X = K1X - MONTHS(I)
    IF (K1X .GT. 0) THEN
	   CYCLE
	ELSE
      ID = K1X + MONTHS(I)
      IM = I
      INY = IY

    ENDIF
    EXIT
  END DO	

END SUBROUTINE DATE13
!-----------------------------------------------------------------------
SUBROUTINE DATE31 (ID, IM, IY, ICD)
!
!     DAYS SINCE 1.1.1900 FROM DAY, MONTH, YEAR
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN) :: ID, IM, IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: ICD
!                       LOCAL VARIABLES
  INTEGER :: IDY, INY
  INTEGER :: K,IYN, days_in_feb
  INTEGER, DIMENSION(12) :: MONTHS

  IF (ISALEAP(IY)) THEN
     days_in_feb = 29
  ELSE
     days_in_feb = 28
  ENDIF

! array ranges from 0 to November (not from Jan to Dec as in
! previous version)                                                 !1.2

  MONTHS = (/0,31,days_in_feb,31,30,31,30,31,31,30,31,30/)          !1.2

  K = SUM(MONTHS(1:IM))                                             !1.2

  IDY = K + ID
  INY = IY
  IYN = INY - 1900
  IF (IYN .GT. 0) THEN
     ICD = IDY + IYN*365 + (IYN-1)/4 - (IYN-1)/100 + (IYN+299)/400
  ELSE
    ICD = IDY + IYN*365 + IYN/4 - IYN/100
  ENDIF


 END SUBROUTINE DATE31
!-----------------------------------------------------------------------
SUBROUTINE  DATE12(ICD,IDY,IY)
!
!     DAY OF YEAR FROM DAYS SINCE 1.1.1900
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: ICD
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: IDY, IY
!                       LOCAL VARIABLES
  INTEGER :: K, KD, KE, KY

  K = ICD
  KE = 0
  IF (K .GE. 366) THEN
     K = K + 1
     IF (K .GE. 73416) THEN
        K = K + 1
        IF (K .GE. 109941) THEN
           K = K + 1
           IF (K .GE. 146466) THEN
		      K = K + 1
		   ENDIF
         ENDIF
     ENDIF
  ENDIF
  IF (K .LE. -36159) THEN
     K = K - 1
  ENDIF
     KY = K/1461*4
     KD = K - K/1461*1461
     IF (KD .LT. 0) THEN
        KD = KD + 1461
        KY = KY - 4
     ENDIF
     KY = KY + 1900
     IF (KD .GT. 366) THEN
        KD = KD - 1
        KE = KD/365
        KD = KD - KD/365*365
     ENDIF
     IF (KD .EQ. 0) THEN
        KE = KE - 1
        KD = 365
     ENDIF
     IY = KY + KE
     IDY = KD

END SUBROUTINE DATE12
!-----------------------------------------------------------------------
SUBROUTINE  DATE32(ID,IM,IY,IDY,INY)
!
!     DAY OF YEAR FROM DAY, MONTH, YEAR
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: ID, IM, IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: IDY, INY
!                       LOCAL VARIABLES
  INTEGER :: K, days_in_feb
  INTEGER, DIMENSION(12) :: MONTHS



  IF (ISALEAP(IY)) THEN
     days_in_feb = 29
  ELSE
     days_in_feb = 28
  ENDIF

! array ranges from 0 to November (not from Jan to Dec as in
! previous version)                                                 !1.2

  MONTHS = (/0,31,days_in_feb,31,30,31,30,31,31,30,31,30/)          !1.2

  K = SUM(MONTHS(1:IM))                                             !1.2

  IDY = K + ID
  INY = IY

END SUBROUTINE DATE32


!-----------------------------------------------------------------------
INTEGER FUNCTION MNTHDS(MONTH,YEAR)
!
!	Returns Days in Month from Month and Year.
!
  IMPLICIT NONE
!					INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: MONTH,YEAR
!					LOCAL VARIABLES
!  LOGICAL :: ISALEAP


SELECT CASE (MONTH)
  CASE (1,3,5,7,8,10,12)
    MNTHDS = 31
  CASE (4,6,9,11)
    MNTHDS = 30
  CASE (2)
    IF (ISALEAP(YEAR)) THEN
	  MNTHDS = 29
    ELSE
	  MNTHDS = 28
    ENDIF
  CASE DEFAULT
	PRINT *, "Error in function MNTHDS"
	RETURN
END SELECT

END FUNCTION MNTHDS
!-----------------------------------------------------------------------
SUBROUTINE DATCHK (DAY,MONTH,YEAR,VALID)
!
!	Checks the date and returns to a given statement for invalid
!     values
!
  IMPLICIT NONE
  INTEGER, INTENT(IN)  :: DAY,MONTH,YEAR
  LOGICAL, INTENT(OUT) :: VALID

  INTEGER :: LDAY
  INTEGER :: LMNTH
  !INTEGER :: MNTHDS1


  IF (YEAR .EQ. 1752) THEN
     LDAY=14
     LMNTH=9
  ELSE
    LDAY=1
    LMNTH=1
  ENDIF

!	Check that within valid year range
!	Check that within valid month range
!	and check that within valid day range

  IF ((YEAR .GE. 1752) .AND.			&
              (YEAR .LE. 2399) .AND.    &
              (MONTH .GE. LMNTH) .AND.  &
              (MONTH .LE. 12)   .AND.   &
              (DAY .GE. LDAY) .AND.     &
           (DAY .LE. MNTHDS(MONTH,YEAR))) THEN
!	Valid date, so return from here	
              VALID=.TRUE.
  ELSE
              VALID=.FALSE.
  ENDIF
  RETURN
END SUBROUTINE DATCHK
!-----------------------------------------------------------------------
! The JDATE Conversion algorithms are based on the algorithm published
! in a letter to the editor of Communications of the ACM (CACM, volume 11,
! number 10, October 1968, p.657) by Henry F. Fliegel and
! Thomas Van Flandern
! This algorithm is valid only for dates from
! 1/3/-4900 G onward when converting from a Julian day number to a date,
! or from 1/3/-4800 when converting from a date to a Julian day number.
! It should be noted that these algorithms are valid only in the
! Gregorian Calendar and the Proleptic Gregorian Calendar (after the
! dates given above). They do not handle dates in the Julian Calendar.
!-----------------------------------------------------------------------
SUBROUTINE JDATE31(ID,IM,IY,OD)
!
!     Returns the Julian Day Number for a Day, Month, Year
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: ID,IM,IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: OD

  OD = ID - 32075											     &
           + 1461 * ( IY + 4800 - ( 14 - IM ) / 12 )/4		     &
           + 367 * ( IM - 2 + (( 14 - IM ) / 12 ) * 12 ) / 12    &
           - 3 * ( ( IY + 4900 - ( 14 - IM ) / 12 ) / 100 ) / 4


END SUBROUTINE JDATE31

!-----------------------------------------------------------------------
SUBROUTINE JDATE13(ID,OD,OM,OY)
!
!     Returns the Day, Month, Year from a Julian Day Number
!

  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: ID
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: OD,OM,OY
!                       LOCAL VARIABLES
  INTEGER :: J,I,L,N

  L = ID + 68569
  N = ( 4 * L ) / 146097
  L = L - ( 146097 * N + 3 ) / 4
  I = ( 4000 * ( L + 1 ) ) / 1461001
  L = L - ( 1461 * I ) / 4 + 31
  J = ( 80 * L ) / 2447
  OD = L - ( 2447 * J ) / 80
  L = J / 11
  OM = J + 2 - ( 12 * L )
  OY = 100 * ( N - 49 ) + I + L


END SUBROUTINE JDATE13

END MODULE zpdate_mod
