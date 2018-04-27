      SUBROUTINE GOESW1(NFT,POINT,BULLEND,BULL)                     !2.0

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : GOESW1                                              
!                                                                     
! PURPOSE       : Decodes section 1 of GOES Atmospheric Motion Winds  
!                                                                     
! CALLED BY     : STBBUL                                              
!                                                                     
! CALLS         : GOESED, GOESW2, DATIM, STBIND, STBENC, AIRSTO       
!                                                                     
! PARAMETERS    : (1) NFT      FT NUMBER (10 FOR SATOBS)              
!                 (2) POINT    POINTER TO WHERE TO START IN BULLETIN  
!                               (date group after MiMiMjMj)           
!                 (3) BULLEND  NUMBER OF LAST CHARACTER IN BULLETIN   
!                 (4) BULL     bulletin                             !2.0
!                                                                     
! REVISION INFO :
!
! $Revision: 2$
! $Date: 22/06/2007 14:43:34$
! $Source: /home/us0400/mdb/op/lib/source/RCS/goesw1.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  2    Met_DB_Project 1.1         22/06/2007 14:43:34    Brian Barwell
!       Obsolete module used for storage of GOESAMW, GOESVIS and GOESWV data
!       which terminated in August 2006.
!  1    Met_DB_Project 1.0         30/01/2006 20:22:39    Sheila Needham  
! $
! Revision 2.1  2001/07/03 10:41:07  usmdb
! Removed argument CORF from call to STBIND as not used in
! STBIND - S.Cox
!
! Revision 2.0  2001/05/31  13:27:50  13:27:50  usmdb (Generic MetDB account)
! Removed unused dummy argument CCCC. Removed unused variables,
! added copyright and modified header - S.Cox
! 
! Revision 1.4  99/04/12  11:00:19  11:00:19  usmdb (Generic MetDB account)
! 19 April 1999,  Infoman 62045,  Brian Barwell,  v(G)=27, ev(G)=10.
! Output revised and uninformative messages removed.
! 
! Revision 1.3  98/10/15  11:08:59  11:08:59  usmdb (Generic MDB account)
! Oct 98: Set century-year in 2000 to 100, not 0
!
! Revision 1.2  98/05/21  15:25:20  15:25:20  usmdb (Generic MDB account)
! Correct the GOESAMW duplicate checking. SATIDSTR must be *9 for
! duplicate check in AIRSTO                                           !a
!
! Revision 1.1  98/03/12  09:06:25  09:06:25  usmdb (Generic MDB account)
! Initial revision
!
! Feb 98: made from SATOB1                                          
!                                                                     
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        


      INTEGER YEAR, MONTH, DAY, HOUR, MINUTE, NYEAR                 !1.3
      INTEGER WINDMETH, SATID, PRESSAM
      INTEGER POINT, BULLEND
      INTEGER IVALUE, IDES
      INTEGER MONTHDAY(12), NOW(8), DATETIME(5), TOR(5)
      INTEGER DECADE, NOWYEAR
      INTEGER NN, NNN, NELEM                                        !2.0
      INTEGER DESCR(1000)
      INTEGER MESSLEN
      INTEGER NFT
      INTEGER BLKSIZ
      INTEGER I,J                                                   !2.0
      INTEGER MISSING
      INTEGER IRC
*
      REAL DCVALS(10000)
*
      LOGICAL KNOTS                                                 !2.1
*
      CHARACTER*1 BULL(*)
      CHARACTER ENTRY*23, MESSAGE*10000
      CHARACTER SATIDSTR*9                                          !2.0
      CHARACTER HEAD*132

      DATA MISSING/-9999999/
      DATA BLKSIZ/27998/
      HEAD='
     &Source: $
     &'//'$Revision: 2$ $Date: 22/06/2007 14:43:34$'
      ENTRY=' '
      KNOTS=.FALSE.
*
      MONTHDAY( 1)=31
      MONTHDAY( 2)=28    ! DON'T PANIC; DICK HADN'T FORGOTTEN
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
* CALL GOESED TO REMOVE CR/LF DOUBLE SPACES ETC.
*
      CALL GOESED(POINT,BULLEND,BULL)
*
* Convert date etc from section 1 (YYMMJ GGggw III// FFFFFF)
*
      CALL DATIM(NOW)
      IF (MOD(NOW(8),4) .EQ. 0) MONTHDAY(2)=29    ! OK UNTIL YEAR 2100
*
      MONTH=IVALUE(BULL(POINT+2)//BULL(POINT+3))
      IF (MONTH.LT.1 .OR. MONTH.GT.12) THEN
        PRINT*, 'SATOB WITH INVALID MONTH'
        RETURN
      ENDIF
*
      DAY=IVALUE(BULL(POINT)//BULL(POINT+1))
      IF (DAY.GT.50 .AND. DAY.LT.82) THEN
        DAY=DAY-50
        KNOTS=.TRUE.
      ENDIF
      IF (DAY.LT.1 .OR. DAY.GT.MONTHDAY(MONTH)) THEN
        PRINT*, 'SATOB WITH INVALID DAY'
        RETURN
      ENDIF
*
      HOUR=IVALUE(BULL(POINT+6)//BULL(POINT+7))
      IF (HOUR.LT.0 .OR. HOUR.GT.23) THEN
        PRINT*, 'SATOB WITH INVALID HOUR'
        RETURN
      ENDIF
*
      MINUTE=IVALUE(BULL(POINT+8)//BULL(POINT+9))
      IF (MINUTE.LT.0 .OR. MINUTE.GT.59) MINUTE=30    !NEAR ENOUGH
*
      DECADE=NOW(8)/10
      NOWYEAR=MOD(NOW(8), 10)
      YEAR=IVALUE(BULL(POINT+4))
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
      POINT=POINT+10
*
      WINDMETH=IVALUE(BULL(POINT))
*
      SATIDSTR=BULL(POINT+2)//BULL(POINT+3)//BULL(POINT+4)
      SATID=IVALUE(SATIDSTR(1:3))                                    !a
*
      POINT=POINT+15    !SKIP PAST NEW F'S 6-FIGURE GROUP FOR NOW
*
* Check that 222 section follows *******************************
*
      IF (BULL(POINT-1)//BULL(POINT)//BULL(POINT+1)//BULL(POINT+2)//
     &    BULL(POINT+3).NE.' 222 ') RETURN
      POINT=POINT+4
      NNN = 0                                                       !1.4
*
*     come back here after NN obs
*
   10 CONTINUE
*
* EXTRACT NN FROM B1B2B3NN GROUP
*
      IF (POINT .GE. BULLEND) THEN                                  !1.4
         YEAR = MOD(YEAR,100) ! for printout only                   !1.4
         WRITE (6,'(T4,I2.2,A,   I3.2,2(A,I2.2),'':'',   I7,A,I2)') !1.4
     &              HOUR, 'Z',  DAY,'/',MONTH,'/',YEAR,  NNN,       !1.4
     &            ' OBSERVATIONS:  WIND METHOD =', WINDMETH         !1.4
         RETURN                                                     !1.4
      END IF                                                        !1.4
      NN=IVALUE(BULL(POINT+3)//BULL(POINT+4))

      IF (NN .LT. 1) THEN
        PRINT*,'GOESSW1: undecodable NN in ',(BULL(I),I=POINT,POINT+4)
        return  ! should look for another section indicator
      ENDIF
!               these values need to be refreshed each time around
!
      NELEM=16
      DESCR(1)=IDES(309192)
      PRESSAM=2  ! "pressure assessment method" equals section number!?
!
      DO I=1, NN*NELEM
        DCVALS(I)=MISSING
      ENDDO

! Set date etc in value array for encoding

      I=0
      DO J=1,NN
        DCVALS(J)=SATID
      ENDDO

      I=I+NN
      DO J=1,NN
        DCVALS(I+J)=WINDMETH
      ENDDO

      I=I+NN
      DO J=1,NN
        DCVALS(I+J)=YEAR
      ENDDO

      I=I+NN
      DO J=1,NN
        DCVALS(I+J)=MONTH
      ENDDO

      I=I+NN
      DO J=1,NN
        DCVALS(I+J)=DAY
      ENDDO

      I=I+NN
      DO J=1,NN
        DCVALS(I+J)=HOUR
      ENDDO

      I=I+NN
      DO J=1,NN
        DCVALS(I+J)=MINUTE
      ENDDO

      I=I+NN
      DO J=1,NN
        DCVALS(I+J)=2.0
      ENDDO

      I=I+NN
      DO J=1,NN
        DCVALS(I+J)=PRESSAM
      ENDDO

! Call expansion program for rest of values for these NN obs

      CALL GOESW2(POINT,KNOTS,DCVALS,NN,BULL,IRC)

! Index, encode & store message

      IF (IRC.EQ.0) THEN
        NNN = NNN + NN  ! Observation counter                       !1.4
        ENTRY(10:10)='2'
        CALL STBIND(NN,DCVALS,SATIDSTR(1:3),DATETIME,ENTRY)         !2.1
        CALL STBENC(DCVALS,NELEM,NN,DESCR,MESSAGE,MESSLEN,DATETIME)
*
* Put current time as time of receipt in section 1 of the BUFR message
* (displacement as for BUFR version 1; change if total length at start)
*
        CALL DATIM(NOW)

        TOR(1)=NOW(8)                                              !1.3
        NYEAR=MOD(NOW(8),100)                                      !1.3
        IF (NYEAR.EQ.0) NYEAR=100                                  !1.3
        MESSAGE(17:17)=CHAR(NYEAR)                                 !1.3

        DO I=1,4                                                   !1.3
          TOR(1+I)=NOW(8-I)
          MESSAGE(17+I:17+I)=CHAR(MOD(NOW(8-I),100))
        ENDDO
        MESSAGE(13:13)=CHAR(5) ! DATA TYPE (SINGLE LEVEL, SATELLITE)
*
        CALL AIRSTO(DATETIME,ENTRY,MESSAGE(1:MESSLEN),
     &              NFT,BLKSIZ,SATIDSTR,TOR)
      ENDIF

! POINT has been incremented by GOESW2, past a number of 5-figure groups
! with spaces after them.  Go back to check for new section indicator or
! another NN obs in the same section.

      IF (BULL(POINT-1).NE.'=') GOTO 10  ! look for next NN
!
      RETURN
      END
