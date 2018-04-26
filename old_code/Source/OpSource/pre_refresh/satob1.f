      SUBROUTINE SATOB1(NFT,POINT,BULLEND,BULL,TTAAII,CCCC,OCOR)    !2.1

!-----------------------------------------------------------------------
!
! PROGRAM       : SATOB1
!
! PURPOSE       : DECODES SATOB SECTION ONE AND DETERMINES SECTION
!                 NUMBER OF NEXT SECTION
!
! CALLED BY     : STBBUL
!
! CALLS         : SATOB2, SATOB3, SATOB4, SATOB5, SATOB7,
!                 DATIM, STBIND, STBENC, AIRSTO
!
! PARAMETERS    : (1) NFT      FT NUMBER (10 FOR SATOBS)
!                 (2) POINT    POINTER TO WHERE TO START IN BULLETIN
!                 (3) BULLEND  NUMBER OF LAST CHARACTER IN BULLETIN
!                 (4) BULL     REPORT DATA
!                 (5) TTAAII   BULLETIN IDENTIFIER
!                 (6) CCCC     COLLECTING CENTRE
!                 (7) OCOR     CORRECTION FLAG
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:06$
! $Source: /home/us0400/mdb/op/lib/source/RCS/satob1.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:06    Sheila Needham  
! $
! Revision 2.1  2001/12/05 09:10:08  usmdb
! Now receives arguments TTAAII & CCCC. Calls AIRSTO instead of
! AMDSTO - S.Cox
!
! Revision 2.0  2001/07/03  10:43:48  10:43:48  usmdb (Generic MetDB account)
! Separated variable declaration and initialisation. Removed unused
! dummy argument CCCC. Removed unused argument CORF from call to
! STBIND as not used in STBIND. Added copyright and modified
! header - S.Cox
!
! Revision 1.6  98/10/07  16:08:15  16:08:15  usmdb (Generic MetDB accou
! Set century day in 2000 to 100, not 0
!
! Revision 1.5  98/05/15  10:32:59  10:32:59  usmdb (Generic MDB account
! Remove change !D (data now stored as GOESAMW)
!
! Revision 1.4  97/10/24  13:42:06  13:42:06  usjl (Jon Lewthwaite)
! EXTRA CODE FOR WMO NOV97 CODE CHANGE TO SECTION ONE. PRE NOV97
! SECTION ONES (OLD) STILL CATERED FOR.                               !E
! CATER FOR NEW STYLE HIGH DENSITY SECTION TWO AS WELL AS OLD STYLE   !D
!
! Revision 1.3  1997/08/28 10:17:46  usjl
! DETERMINE WHETHER A SECTION TWO WITH AN ODD NUMBER OF 'OBS' IS
! PADDED AT END WITH ' ///// /////' OR NOT - DH                       !C
!
! Revision 1.2  1997/07/31 09:34:04  uspm
! First revision for 1
!
! Revision 1.1  1997/07/04 13:18:52  uspm
! Initial revision
!
! 25/11/96   EXTRA CODE TO COPE WITH SATOBS WITH PRE NOV93 STYLE
!            SECTION ONE (ANCIENT)                                    !B
!
! 14/10/96   INTRODUCE TIME OF RECEIPT STORAGE IN SECTION ONE
!            OF BUFR MESSAGE.                                         !A
!
! 05/06/96   REMOVE COMMON BLOCK 'TBULL', REPLACED BY A NEW
!            PARAMETER 'BULL'.
!
! OCT 93     FIRST WRITTEN
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER YEAR, MONTH, DAY, HOUR, MINUTE, NYEAR                 !1.6
      INTEGER WINDMETH, SATID, PRESSAM
      INTEGER POINT, BULLEND, ENDDCV
      INTEGER IVALUE, IDES
      INTEGER MONTHDAY(12), NOW(8), DATETIME(5), TOR(5)
      INTEGER DECADE, NOWYEAR
      INTEGER NN, NOBS, NELEM, JUMP
      INTEGER DESCR(1000)
      INTEGER MESSLEN
      INTEGER ICCCC, NFT, NFTPAR
      INTEGER I, J, K, L, M                                           !C
      INTEGER MISSING                                               !2.0
      INTEGER BLKSIZ                                                !2.1

      REAL DCVALS(10000)

      LOGICAL KNOTS, OCOR, ANCSECT1, ODDTOT, SLASHES                !2.1
      LOGICAL OLDSECT1                                                !E

      CHARACTER BULL*(*), ENTRY*23, MESSAGE*10000
      CHARACTER SECTIONS*21, SECTION*1, SATIDSTR*9                  !2.1
      CHARACTER HEAD*132
      CHARACTER*(*) TTAAII                                          !2.1
      CHARACTER*(*) CCCC                                            !2.1

      DATA MISSING/-9999999/                                        !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/satob1.F,v $
     &'//'$ $Date: 30/01/2006 20:24:06$ $Revision: 1$'

      ENTRY=' '
      SECTIONS=' 222 333 444 555 777 '
*
      KNOTS=.FALSE.
      ANCSECT1=.FALSE.  !ANCIENT SECTION ONES ARE PRE NOV 1993 CODE !B
      OLDSECT1=.FALSE.  !    OLD SECTION ONES ARE PRE NOV 1997 CODE !E
*
      MONTHDAY( 1)=31
      MONTHDAY( 2)=28    ! DON'T PANIC; I HAVEN'T FORGOTTEN
      MONTHDAY( 3)=31
      MONTHDAY( 4)=30
      MONTHDAY( 5)=31
      MONTHDAY( 6)=30
      MONTHDAY( 7)=31
      MONTHDAY( 8)=31
      MONTHDAY( 9)=30
      MONTHDAY(10)=31
      MONTHDAY(11)=30
      MONTHDAY(12)=31
*
      CALL DATIM(NOW)
      IF (MOD(NOW(8),4) .EQ. 0) MONTHDAY(2)=29    ! OK UNTIL YEAR 2100
*
      IF (BULL(POINT+ 9:POINT+10) .EQ. '//') ANCSECT1=.TRUE.        !B
      IF (BULL(POINT+21:POINT+21) .EQ.  ' ') OLDSECT1=.TRUE.        !E
*
      IF (ANCSECT1) THEN                                            !B
         IF (NOW(6).EQ.1 .AND. NOW(5).LT.3) THEN
            MONTH=MISSING
         ELSE
            MONTH=NOW(7)
         ENDIF
      ELSE
         MONTH=IVALUE(BULL(POINT+2:POINT+3))
      ENDIF
      IF (MONTH.LT.1 .OR. MONTH.GT.12) THEN
         PRINT*, 'SATOB WITH INVALID MONTH'
         RETURN
      ENDIF
*
      DAY=IVALUE(BULL(POINT:POINT+1))
      IF (DAY.GT.50 .AND. DAY.LT.82) THEN
         DAY=DAY-50
         KNOTS=.TRUE.
      ENDIF
      IF (DAY.LT.1 .OR. DAY.GT.MONTHDAY(MONTH)) THEN
         PRINT*, 'SATOB WITH INVALID DAY'
         RETURN
      ENDIF
*
      IF (ANCSECT1) THEN                                            !B
         HOUR=IVALUE(BULL(POINT+2:POINT+3))
      ELSE
         HOUR=IVALUE(BULL(POINT+6:POINT+7))
      ENDIF
      IF (HOUR.LT.0 .OR. HOUR.GT.23) THEN
         PRINT*, 'SATOB WITH INVALID HOUR'
         RETURN
      ENDIF
*
      IF (ANCSECT1) THEN                                            !B
         MINUTE=IVALUE(BULL(POINT+4:POINT+4))
         IF (MINUTE .NE. MISSING) MINUTE=MINUTE*10
      ELSE
         MINUTE=IVALUE(BULL(POINT+8:POINT+9))
      ENDIF
      IF (MINUTE.LT.0 .OR. MINUTE.GT.59) MINUTE=30    !NEAR ENOUGH
*
      DECADE=NOW(8)/10
      NOWYEAR=MOD(NOW(8), 10)
      IF (ANCSECT1) THEN                                            !B
         YEAR=MISSING
      ELSE
         YEAR=IVALUE(BULL(POINT+4:POINT+4))
      ENDIF
      IF (YEAR .EQ. MISSING) THEN
         IF (NOW(7).EQ.1 .AND. NOW(6).EQ.1 .AND. NOW(5).LT.3) THEN
            PRINT*, 'SATOB WITH INVALID YEAR'
            RETURN
         ELSE
            YEAR=NOWYEAR    !SYSTEM YEAR OK UNLESS EARLY ON 1ST JAN
         ENDIF
      ENDIF
      IF (YEAR.NE.NOWYEAR .AND. NOWYEAR.EQ.0) DECADE=DECADE-1
      YEAR=DECADE*10 + YEAR
*
      IF (ANCSECT1) THEN                                            !B
         POINT=POINT+4
      ELSE
         POINT=POINT+10
      ENDIF
*
      IF (ANCSECT1) THEN                                            !B
         WINDMETH=MISSING
      ELSE
         WINDMETH=IVALUE(BULL(POINT:POINT))
      ENDIF
*
      SATID=IVALUE(BULL(POINT+2:POINT+4))
      SATIDSTR(1:3)=BULL(POINT+2:POINT+4)                         !2.1
*
      IF (ANCSECT1 .OR. OLDSECT1) THEN                              !E
         POINT=POINT+7                                              !E
      ELSE                                                          !E
         POINT=POINT+14  !SKIP PAST NEW F'S 6-FIGURE GROUP FOR NOW  !E
      ENDIF              !STORAGE OF IT MAY BE INTRODUCED LATER     !E
*
   10 CONTINUE
      IF (POINT .GE. BULLEND) RETURN
      DO I=1, 10000
         DCVALS(I)=MISSING
      ENDDO
      SECTION='?'
      I=POINT-1
      DO POINT=I, BULLEND-4
         DO J=1,17,4
            IF (BULL(POINT:POINT+4) .EQ. SECTIONS(J:J+4)) THEN
               SECTION=SECTIONS(J+1:J+1)
               GOTO 20
            ENDIF
         ENDDO
      ENDDO
      IF (SECTION .EQ. '?') THEN
C        PRINT*, 'NO MORE SECTIONS'
         RETURN
      ENDIF
*
   20 CONTINUE
      IF      (SECTION .EQ. '4') THEN
         NELEM=10
C        DESCR(1)=51649
         DESCR(1)=IDES (309193)
      ELSE IF (SECTION .EQ. '7') THEN
         NELEM=11
C        DESCR(1)=51650
         DESCR(1)=IDES (309194)
      ELSE
         NELEM=16
C        DESCR(1)=51648
         DESCR(1)=IDES (309192)
      ENDIF
      IF      (SECTION.EQ.'2') THEN
         JUMP=30
      ELSE IF (SECTION.EQ.'3' .OR. SECTION.EQ.'5') THEN
         JUMP=12
      ELSE IF (SECTION.EQ.'4') THEN
         JUMP=6
      ELSE
         JUMP=18
      ENDIF
      READ (SECTION, '(I1)') PRESSAM
*  EQUATE PRESSURE ASSESSMENT METHOD WITH SECTION NUMBER
*  UNTIL A BETTER SCHEME THOUGHT UP
*
      POINT=POINT+5
      K=POINT
      NOBS=0
      SLASHES=.FALSE.                                               !C
*
   30 CONTINUE
      IF (SECTION.EQ.'7' .AND. BULL(K+2:K+4).EQ.'///') K=K+6
      NN=IVALUE(BULL(K+3:K+4))
      IF (NN .LT. 1) THEN
C        PRINT*, 'UNDECODABLE NN |', BULL(K+3:K+4), '|'
         GOTO 10
      ELSE
         NOBS=NOBS + NN
         IF (SECTION.EQ.'2') THEN
            IF (MOD(NN,2) .EQ. 0) THEN
               L=NN/2
               ODDTOT=.FALSE.                                        !C
            ELSE
               L=NN/2 + 1
               ODDTOT=.TRUE.                                         !C
               IF (.NOT.SLASHES) THEN                                !C
                  M=INDEX(BULL(K:K + 6 + L*JUMP), '/////')
                  IF (M .GT. 0) SLASHES=.TRUE.
               ENDIF                                                 !C
            ENDIF
         ELSE IF (SECTION .EQ. '7') THEN
            IF (MOD(NN,5) .EQ. 0) THEN
               L=NN/5
            ELSE
               L=NN/5 + 1
            ENDIF
         ELSE
            L=NN
         ENDIF
         IF (SECTION.EQ.'2') THEN                                    !C
            IF (.NOT.ODDTOT .OR. SLASHES) THEN                       !C
               K=K + 6 + L*JUMP
            ELSE
               K=K + 6 + (L-1)*JUMP + 18
            ENDIF
         ELSE
            K=K + 6 + L*JUMP
         ENDIF                                                       !C
         IF      (K .GT. BULLEND  ) THEN
C           PRINT*, 'OFF END OF BULLETIN'
            NOBS=0
            RETURN
         ELSE IF (K .GT. BULLEND-3) THEN
C           PRINT*, 'AT END OF BULLETIN'
            GOTO 40
         ELSE
            DO J=1,17,4
               IF (BULL(K-1:K+3) .EQ. SECTIONS(J:J+4)) THEN
C                 PRINT*, 'NEXT SECTION FOUND'
                  GOTO 40
               ENDIF
            ENDDO
            GOTO 30
         ENDIF
      ENDIF
*
   40 CONTINUE
      I=0
      DO J=1,NOBS
         DCVALS(J)=SATID
      ENDDO
      IF (SECTION.EQ.'2' .OR. SECTION.EQ.'3' .OR. SECTION.EQ.'5') THEN
         I=I+NOBS
         DO J=1,NOBS
            DCVALS(I+J)=WINDMETH
         ENDDO
      ENDIF
      I=I+NOBS
      DO J=1,NOBS
         DCVALS(I+J)=YEAR
      ENDDO
      I=I+NOBS
      DO J=1,NOBS
         DCVALS(I+J)=MONTH
      ENDDO
      I=I+NOBS
      DO J=1,NOBS
         DCVALS(I+J)=DAY
      ENDDO
      I=I+NOBS
      DO J=1,NOBS
         DCVALS(I+J)=HOUR
      ENDDO
      I=I+NOBS
      DO J=1,NOBS
         DCVALS(I+J)=MINUTE
      ENDDO
      IF (SECTION .NE. '7') THEN
         I=I+NOBS
         DO J=1,NOBS
            IF (SECTION .EQ. '4') THEN
               DCVALS(I+J)=0.0
            ELSE
               DCVALS(I+J)=2.0
            ENDIF
         ENDDO
      ENDIF
      IF (SECTION.EQ.'2' .OR. SECTION.EQ.'3' .OR. SECTION.EQ.'5') THEN
         I=I+NOBS
         DO J=1,NOBS
            DCVALS(I+J)=PRESSAM
         ENDDO
      ENDIF
      ENDDCV=I+J - 1
*
      IF      (SECTION .EQ. '2') THEN
         CALL SATOB2     (POINT,KNOTS,DCVALS,NOBS,BULL,SLASHES)      !C
      ELSE IF (SECTION .EQ. '3') THEN
         CALL SATOB3(POINT,KNOTS,DCVALS,NOBS,BULL)
      ELSE IF (SECTION .EQ. '4') THEN
         CALL SATOB4(POINT,DCVALS,NOBS,BULL)
      ELSE IF (SECTION .EQ. '5') THEN
         CALL SATOB5(POINT,DCVALS,NOBS,BULL)
      ELSE
         CALL SATOB7(POINT,DCVALS,NOBS,BULL)
      ENDIF
*
*
      ENTRY(10:10)=SECTION
      CALL STBIND(NOBS,DCVALS,SATIDSTR(1:3),DATETIME,ENTRY)        !2.1
      CALL STBENC(DCVALS,NELEM,NOBS,DESCR,MESSAGE,MESSLEN,DATETIME)
*
* PUT CURRENT TIME AS TIME OF RECEIPT IN SECTION 1 OF THE BUFR MESSAGE
* (DISPLACEMENT AS FOR BUFR VERSION 1; CHANGE IF TOTAL LENGTH AT START)
*
      CALL DATIM(NOW)

      TOR(1)=NOW(8)                                                !1.6
      NYEAR=MOD(NOW(8),100)                                        !1.6
      IF (NYEAR.EQ.0) NYEAR=100                                    !1.6
      MESSAGE(17:17)=CHAR(NYEAR)                                   !1.6

      DO I=1,4                                                     !1.6
         TOR(1+I)=NOW(8-I)
         MESSAGE(17+I:17+I)=CHAR(MOD(NOW(8-I),100))                   !A
      ENDDO
      MESSAGE(13:13)=CHAR(4) ! DATA TYPE (SINGLE LEVEL, NOT SATELLITE)

!-----------------------------------------------------------------------
! Set bulletin details to go in trailer and call AIRSTO
!-----------------------------------------------------------------------

        SATIDSTR=ENTRY(3:11)                                        !2.1
        ENTRY(3:6)=TTAAII(1:4)                                      !2.1
        ENTRY(7:7)=CHAR(0)                                          !2.1
        IF (OCOR) ENTRY(7:7)=CHAR(1)                                !2.1
        ENTRY(8:11)=CCCC                                            !2.1
        BLKSIZ=27998                                                !2.1

        CALL AIRSTO(DATETIME,ENTRY,MESSAGE(:MESSLEN),               !2.1
     &              NFT,BLKSIZ,SATIDSTR,TOR)                        !2.1

      IF (BULL(POINT-1:POINT-1) .NE. '=') GOTO 10

  100 FORMAT (1X, F12.2)

      RETURN
      END
