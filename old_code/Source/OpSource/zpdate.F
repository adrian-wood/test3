      SUBROUTINE  ZPDATE                                                        

C**********************************************************************
C                                                                     *
C   RECORD OF CHANGES:                                                *
C   ==================                                                *
C                                                                     *
C  0 ORIGINAL VERSION BY JOHN PRINCE, LONG AGO IN THE MISTS OF TIME,  *
C                                     FOR THE IBM WITH STATIC MEMORY  *
C  1 TRANSLATED BY FPP 2.26B16 11/12/89  11:44:56   TDYON=X           *         
C  2 RE-WRITE   BY PAUL WHITE  13/12/89                               *
C                        TO MAKE RE-ENTRANT WITH INLINE EXPANSION     *
C                        TRANSLATED BY FPP 2.26B16 13/12/89  11:41:08 *
C                        SWITCHES: LSTOFF=T,OPTON=78,TDYON=FX         *
C  3 RE-COMPILED 29/10/91 TO PRODUCE 31 BIT ADDRESSING MODE VERSION   *
C        BY M. COLLIER - COPIED TO MET.PROGLIB                        *
C  4 Updated 30/1/98 by Edward Jones                                  *
C                        Update ZPDATE subroutine                     *
C                        Added ISALEAP subroutine                     *
C                        Ported to HP, Cray and PC from MET.SRCELIB   *
C  5 Updated 17/2/98 by Edward Jones                                  *
C                        Added DATCHK and MNTHDS Routines             *
C  6 Updated 23/3/98 by Edward Jones                                  *
C                        Added JDAY Routine                           *
C  7 Updated 9/6/98 by Edward Jones                                   *
C                        Modified MNTHDS to work on IBM following     *
C                        Fault report raised by Glen Harris           *
C**********************************************************************
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:26:02    Sheila Needham  
! $
! Revision 1.3  1998/09/23 15:28:16  usmdb
! Add preprocessor around DATCHK as this has the same
! name as another MetDB routine. Preprocess will
! remove code from HPUX version.
!
c Revision 1.2  98/09/22  13:45:58  13:45:58  usjl (Jon Lewthwaite)
c Latest version from MVS also Y2k compliant
c 
! Revision 1.1  1997/08/14 08:13:37  uspm
! Initial revision
!
!
!----------------------------------------------------------------------         

      CHARACTER*132 HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/zpdate.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:26:02$ '

      WRITE (6, 900)
      RETURN

  900 FORMAT('  ZPDATE (Y2K Compliance Checked)'/
     1       '  LAST MODIFIED TUESDAY  9th June 1998'/
     2       '  By Edward Jones, OS(DD)'/
     3       '  Contact Software Engineering Group with any queries'/)

      END

C-----------------------------------------------------------------------
      SUBROUTINE DATE21 (IDY, IY, ICD)
C
C     DAYS SINCE 1.1.1900, FROM DAY OF YEAR
C
      IMPLICIT NONE
C                        INPUT ARGUMENTS
      INTEGER IDY, IY
C                       OUTPUT ARGUMENTS
      INTEGER ICD
C                       LOCAL VARIABLES
      INTEGER IYN

      IYN = IY - 1900
      IF (IYN .GT. 0) THEN
         ICD = IDY + IYN*365 + (IYN-1)/4 - (IYN-1)/100 + (IYN+299)/400
      ELSE
         ICD = IDY + IYN*365 + IYN/4 - IYN/100
      ENDIF

      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE DATE23 (IDY, IY, ID, IM, INY)
C
C     DAY, MONTH, YEAR FROM DAY OF YEAR
C
      IMPLICIT NONE
C                        INPUT ARGUMENTS
      INTEGER IDY, IY
C                       OUTPUT ARGUMENTS
      INTEGER ID, IM, INY
C                       LOCAL VARIABLES
      INTEGER I, K, MONTHS(12)

      MONTHS(1) = 31

      IF (IY/4*4 .NE. IY) THEN
         MONTHS(2) = 28
      ELSE
         IF (IY/400*400 .EQ. IY) THEN
            MONTHS(2) = 29
         ELSE IF (IY/100*100 .EQ. IY) THEN
            MONTHS(2) = 28
         ELSE
            MONTHS(2) = 29
         ENDIF
      ENDIF

      MONTHS(3) = 31
      MONTHS(4) = 30
      MONTHS(5) = 31
      MONTHS(6) = 30
      MONTHS(7) = 31
      MONTHS(8) = 31
      MONTHS(9) = 30
      MONTHS(10) = 31
      MONTHS(11) = 30
      MONTHS(12) = 31

      K = IDY
      I = 0

  100 CONTINUE

      I = I + 1
      K = K - MONTHS(I)
      IF (K .GT. 0) GO TO 100
      ID = K + MONTHS(I)
      IM = I
      INY = IY
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE DATE13 (ICD, ID, IM, INY)
C
C     DAY, MONTH, YEAR FROM DAYS SINCE 1.1.1900
C
      IMPLICIT NONE
C                        INPUT ARGUMENTS
      INTEGER ICD
C                       OUTPUT ARGUMENTS
      INTEGER ID, IM, INY
C                       LOCAL VARIABLES
      INTEGER IDY, IY
      INTEGER K,KD,KE,KY,I,K1X,MONTHS(12)

      K = ICD
      KE = 0
      IF (K .GE. 366) THEN
         K = K + 1
         IF (K .GE. 73416) THEN
            K = K + 1
            IF (K .GE. 109941) THEN
               K = K + 1
               IF (K .GE. 146466) K = K + 1
            ENDIF
         ENDIF
      ENDIF
      IF (K .LE. (-36159)) K = K - 1
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
      MONTHS(1) = 31
      IF (IY/4*4 .NE. IY) THEN
         MONTHS(2) = 28
      ELSE
         IF (IY/400*400 .EQ. IY) THEN
            MONTHS(2) = 29
         ELSE IF (IY/100*100 .EQ. IY) THEN
            MONTHS(2) = 28
         ELSE
            MONTHS(2) = 29
         ENDIF
      ENDIF
      MONTHS(3) = 31
      MONTHS(4) = 30
      MONTHS(5) = 31
      MONTHS(6) = 30
      MONTHS(7) = 31
      MONTHS(8) = 31
      MONTHS(9) = 30
      MONTHS(10) = 31
      MONTHS(11) = 30
      MONTHS(12) = 31
      K1X = IDY
      I = 0
  100 CONTINUE
      I = I + 1
      K1X = K1X - MONTHS(I)
      IF (K1X .GT. 0) GO TO 100
      ID = K1X + MONTHS(I)
      IM = I
      INY = IY

      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE DATE31 (ID, IM, IY, ICD)
C
C     DAYS SINCE 1.1.1900 FROM DAY, MONTH, YEAR
C
      IMPLICIT NONE
C                        INPUT ARGUMENTS
      INTEGER ID, IM, IY
C                       OUTPUT ARGUMENTS
      INTEGER ICD
C                       LOCAL VARIABLES
      INTEGER IDY, INY
      INTEGER I,K,MONTHS(12),IYN

      MONTHS(1) = 31
      IF (IY/4*4 .NE. IY) THEN
         MONTHS(2) = 28
      ELSE
         IF (IY/400*400 .EQ. IY) THEN
            MONTHS(2) = 29
         ELSE IF (IY/100*100 .EQ. IY) THEN
            MONTHS(2) = 28
         ELSE
            MONTHS(2) = 29
         ENDIF
      ENDIF
      MONTHS(3) = 31
      MONTHS(4) = 30
      MONTHS(5) = 31
      MONTHS(6) = 30
      MONTHS(7) = 31
      MONTHS(8) = 31
      MONTHS(9) = 30
      MONTHS(10) = 31
      MONTHS(11) = 30
      MONTHS(12) = 31
      K = 0
      DO 100 I = 1, IM
         K = K + MONTHS(I)
  100 CONTINUE
      K = K - MONTHS(IM)
      IDY = K + ID
      INY = IY
      IYN = INY - 1900
      IF (IYN .GT. 0) THEN
         ICD = IDY + IYN*365 + (IYN-1)/4 - (IYN-1)/100 + (IYN+299)/400
      ELSE
         ICD = IDY + IYN*365 + IYN/4 - IYN/100
      ENDIF

      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE  DATE12(ICD,IDY,IY)
C
C     DAY OF YEAR FROM DAYS SINCE 1.1.1900
C
      IMPLICIT NONE
C                        INPUT ARGUMENTS
      INTEGER ICD
C                       OUTPUT ARGUMENTS
      INTEGER IDY, IY
C                       LOCAL VARIABLES
      INTEGER K, KD, KE, KY

      K = ICD
      KE = 0
      IF (K .GE. 366) THEN
         K = K + 1
         IF (K .GE. 73416) THEN
            K = K + 1
            IF (K .GE. 109941) THEN
               K = K + 1
               IF (K .GE. 146466) K = K + 1
            ENDIF
         ENDIF
      ENDIF
      IF (K .LE. (-36159)) K = K - 1
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
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE  DATE32(ID,IM,IY,IDY,INY)
C
C     DAY OF YEAR FROM DAY, MONTH, YEAR
C
      IMPLICIT NONE
C                        INPUT ARGUMENTS
      INTEGER ID, IM, IY
C                       OUTPUT ARGUMENTS
      INTEGER IDY, INY
C                       LOCAL VARIABLES
      INTEGER I, K, MONTHS(12)

      MONTHS(1) = 31

      IF (IY/4*4 .NE. IY) THEN
         MONTHS(2) = 28
      ELSE
         IF (IY/400*400 .EQ. IY) THEN
            MONTHS(2) = 29
         ELSE IF (IY/100*100 .EQ. IY) THEN
            MONTHS(2) = 28
         ELSE
            MONTHS(2) = 29
         ENDIF
      ENDIF

      MONTHS(3) = 31
      MONTHS(4) = 30
      MONTHS(5) = 31
      MONTHS(6) = 30
      MONTHS(7) = 31
      MONTHS(8) = 31
      MONTHS(9) = 30
      MONTHS(10) = 31
      MONTHS(11) = 30
      MONTHS(12) = 31

      K = 0
      DO 100 I = 1, IM
         K = K + MONTHS(I)
100   CONTINUE
      K = K - MONTHS(IM)
      IDY = K + ID
      INY = IY
      RETURN
      END

C--------------------------------------------------------
      LOGICAL FUNCTION ISALEAP(IY)
C
C     Returns .TRUE. if IY is a Leap year
C     Returns .FALSE. if IY is not a Leap year
C
      IMPLICIT NONE
C     INPUT ARGUMENTS
      INTEGER IY
C  LOCAL VARIABLES

C Divide by 4
      IF (IY/4*4 .NE. IY) THEN
         ISALEAP=.FALSE.
      ELSE
C Century Check

      IF (IY/400*400 .EQ. IY) THEN
         ISALEAP=.TRUE.
C Century check
      ELSE IF (IY/100*100 .EQ. IY) THEN
        ISALEAP=.FALSE.
      ELSE
        ISALEAP=.TRUE.
      ENDIF
      ENDIF
      RETURN
      END
C-----------------------------------------------------------------------
      INTEGER FUNCTION MNTHDS (MONTH,YEAR)
C
C Returns Days in Month from Month and Year.
C
      IMPLICIT NONE
C     INPUT ARGUMENTS
      INTEGER MONTH,YEAR
C     LOCAL VARIABLES
      LOGICAL ISALEAP

C First, see if month has 31 days
      IF ((MONTH .EQ. 1) .OR. (MONTH .EQ. 3) .OR. (MONTH .EQ. 5) .OR.
     & (MONTH .EQ. 7). OR. (MONTH .EQ. 8) .OR. (MONTH .EQ. 10) .OR.
     &    (MONTH .EQ. 12)) THEN
      MNTHDS=31
C Now deal with February
      ELSE IF (MONTH .EQ. 2) THEN
        IF (ISALEAP(YEAR)) THEN
            MNTHDS=29
        ELSE
            MNTHDS=28
        ENDIF
C Now see if month has 30 days
      ELSE IF ((MONTH .EQ. 4) .OR. (MONTH .EQ. 6) .OR.
     &         (MONTH .EQ. 9) .OR. (MONTH .EQ. 11)) THEN
            MNTHDS=30
C Only get here if not a valid month number
      ELSE
            MNTHDS=-99
      ENDIF

      RETURN
      END
C-----------------------------------------------------------------------
#if defined (MVS)
      SUBROUTINE DATCHK (DAY,MONTH,YEAR,*)
C
C Checks the date and returns to a given statement for invalid
C     values
C
      IMPLICIT NONE
C     INPUT ARGUMENTS
      INTEGER DAY,MONTH,YEAR
C     LOCAL VARIABLES
      INTEGER LDAY
      INTEGER LMNTH
      INTEGER MNTHDS

      LOGICAL VALID

      IF (YEAR .EQ. 1752) THEN
          LDAY=14
          LMNTH=9
      ELSE
          LDAY=1
          LMNTH=1
      ENDIF

C Check that within valid year range
C Check that within valid month range
C and check that within valid day range

      IF ((YEAR .GE. 1752) .AND.
     &    (YEAR .LE. 2399) .AND.
     &    (MONTH .GE. LMNTH) .AND.
     &    (MONTH .LE. 12)   .AND.
     &    (DAY .GE. LDAY) .AND.
     &    (DAY .LE. MNTHDS(MONTH,YEAR))) THEN
C Valid date, so return from here
              VALID=.TRUE.
      ELSE
              VALID=.FALSE.
      ENDIF
C     Faulty date, so return to alternate return
C     address
      IF (VALID) THEN
         RETURN
      ELSE
         RETURN 1
      ENDIF
      END
#endif
C-----------------------------------------------------------------------
C The JDATE Conversion algorithms are based on the algorithm published
C in a letter to the editor of Communications of the ACM (CACM, volume 11,
C number 10, October 1968, p.657) by Henry F. Fliegel and
C Thomas C. Van Flandern
C This algorithm is valid only for dates from
C 1/3/-4900 G onward when converting from a Julian day number to a date,
C or from 1/3/-4800 when converting from a date to a Julian day number.
C It should be noted that these algorithms are valid only in the
C Gregorian Calendar and the Proleptic Gregorian Calendar (after the
C dates given above). They do not handle dates in the Julian Calendar.
C-----------------------------------------------------------------------
      SUBROUTINE JDATE31(ID,IM,IY,OD)
C
C     Returns the Julian Day Number for a Day, Month, Year
C
      IMPLICIT NONE
C                        INPUT ARGUMENTS
      INTEGER ID,IM,IY
C                       OUTPUT ARGUMENTS
      INTEGER OD

      OD = ID - 32075
     &        + 1461 * ( IY + 4800 - ( 14 - IM ) / 12 )/4
     &        + 367 * ( IM - 2 + (( 14 - IM ) / 12 ) * 12 ) / 12
     &        - 3 * ( ( IY + 4900 - ( 14 - IM ) / 12 ) / 100 ) / 4

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE JDATE13(ID,OD,OM,OY)
C
C     Returns the Day, Month, Year from a Julian Day Number
C

      IMPLICIT NONE
C                        INPUT ARGUMENTS
      INTEGER OD,OM,OY
C                       OUTPUT ARGUMENTS
      INTEGER ID
C                       LOCAL VARIABLES
      INTEGER J,I,L,N

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

      RETURN
      END
