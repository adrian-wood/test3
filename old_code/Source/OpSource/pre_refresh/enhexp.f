      SUBROUTINE ENHEXP(REPORT,REPLEN,POS,EXPARR,STNID,OBDATE,NUM_VALS,
     &                  SYNTAX)

!-----------------------------------------------------------------------
!                                                            
! PROGRAM       : ENHEXP                                      
!
! PURPOSE       : TO EXPAND REPORT INTO AN ARRAY READY FOR    
!                 ENCODING INTO BUFR.                         
!                                                            
! CALLED BY     : ENHBUL                                      
!                                                            
! CALLS         : NONE                                        
!                                                            
! PARAMETERS    : REPORT   CHARACTER STRING OF REPORT (I)     
!                 REPLEN   LENGTH OF REPORT  (I)              
!                 POS      FIRST GROUP POSITION (I)           
!                 EXPARR   EXPANSION ARRAY   (O)              
!                 STNID    STATION ID (STRING) (I)            
!                 NUM_VALS NUMBER OF DATA VALUES (O)          
!                 SYNTAX   SET TO TRUE IF SYNTAX ERROR FOUND  
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:23$
! $Source: /home/us0400/mdb/op/lib/source/RCS/enhexp.F,v $
!                                                            
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:23    Sheila Needham  
! $
! Revision 2.1  2001/07/03 10:39:12  usmdb
! Corrected initialisation loop for IARR. Now loops from 1 to 30
! instead of 0 to 30 - S.Cox
!
! Revision 2.0  2001/05/31  13:27:45  13:27:45  usmdb (Generic MetDB account)
! Removed unused variables, added copyright and modified
! header - S.Cox
! 
! Revision 1.4  99/03/08  09:41:26  09:41:26  usmdb (Generic MetDB account)
! 8th March 1999 John Norton
! Remove setting gust time to midnight when gust value is calm.
! 
! Revision 1.3  99/02/11  11:53:07  11:53:07  usmdb (Generic MDB account)
! 15th February 1999 John Norton
! Remove call to STAPOS to find out station details done in
! ENHBUL instead.
! Set Gust time to midnight (00:00) when Gust time is zero.
!
! Revision 1.2  97/10/24  12:19:41  12:19:41  usjl (Jon Lewthwaite)
! Year, month, day, hour and minute values set for report.            !a
!
! Temperature passed to tempexp in dedicated string to avoid
! ICA error.                                                          !b
!
! Revision 1.1  1997/09/25 15:51:37  usjl
! Initial revision
!
! 06/05/1997   COPIED FROM BTHEXP                      JN
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

      IMPLICIT NONE

      CHARACTER*4096 REPORT
      CHARACTER*132  HEAD
      CHARACTER*4    TTVAL
      INTEGER        REPLEN
      INTEGER        POS
      REAL           EXPARR(30)
      CHARACTER*5    STNID
      INTEGER        OBDATE(5)
      LOGICAL        STORE
      INTEGER        NUM_VALS
      LOGICAL        SYNTAX

      INTEGER        CENTDY
      INTEGER        DAY_M1
      INTEGER        MON_M1
      INTEGER        YR_M1

      INTEGER        GROUP
      INTEGER        I
      INTEGER        FLAG(7)
      INTEGER        IARR(30)
      INTEGER        SPCPOS         ! POSITION OF NEXT SPACE IN REPORT

      LOGICAL        OFIGTS
      REAL           TEMPEXP        ! function to expand temperature

      INTEGER        MISSIN                 ! integer missing data value
      REAL           RMISS                  ! real missing data value
      REAL           KTS2MPS                ! knots to m/s conversion
      DATA           MISSIN/-9999999/
      DATA           RMISS/-9999999./
      DATA           KTS2MPS/0.5144444444/   ! 1852/3600 IS THE STANDARD


*************************************************************
*
*     INITIALISE ARRAY TO MISSING DATA
*
*************************************************************
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/enhexp.F,v $
     &'//'$ $Date: 30/01/2006 20:22:23$ $Revision: 1$'

      DO I= 1,30                                                    !2.1
        IARR(I) = MISSIN
      ENDDO

      NUM_VALS=0
      TTVAL(1:)=' '                                                 !b

*
* WMO BLK AND STN NO
*
      READ (STNID(01:02),'(I2)') IARR(1)
      READ (STNID(03:05),'(I3)') IARR(2)

*
* year
*
      IARR(3)=OBDATE(1)

*
* month
*
      IARR(4)=OBDATE(2)

*
* day
*
      IARR(5)=OBDATE(3)

*
* hour
*
      IARR(6)=OBDATE(4)

*
* minute
*
      IARR(7)=OBDATE(5)




* Initialise group done indicators

      DO GROUP=1,7
        FLAG(GROUP)=0
      ENDDO

* Loop through groups in report

      DO WHILE (POS.LT.REPLEN)

* Find position of next space in report

        SPCPOS=INDEX(REPORT(POS:REPLEN),' ')

c     PRINT*,' ENHEXP >',REPORT(POS-2:POS+2),'<',POS,REPLEN,SPCPOS,
c    &                   (REPLEN-POS)                ! test lines

* Test that first digit of group is a number

        IF (OFIGTS(REPORT,POS,POS).AND.
     &       (SPCPOS.EQ.6.OR.(SPCPOS.EQ.0.AND.(REPLEN-POS).EQ.4)))THEN
*
* 1 Group (1dadafafa - Modal wind speed and direction)
*
          IF (REPORT(POS:POS).EQ.'1')THEN
            IF (FLAG(1) .EQ. 0) THEN
              IF (OFIGTS(REPORT,POS+1,POS+2)) THEN
! DADA
                READ (REPORT(POS+1:POS+2),'(I2)') IARR(17)
                IARR(15)=10
                IARR(16)=6
                FLAG(1)=1
                STORE=.TRUE.
                NUM_VALS=NUM_VALS+1
              ENDIF

              IF (OFIGTS(REPORT,POS+3,POS+4)) THEN
!fafa
                IARR(18)=4
                READ (REPORT(POS+3:POS+4),'(I2)') IARR(19)
                FLAG(1)=1
                STORE=.TRUE.
                NUM_VALS=NUM_VALS+1
              ENDIF
            ENDIF
          ENDIF
*
* 2 Group (20fafafa - Modal wind speed > 99)
*
          IF ((REPORT(POS:POS+1).EQ.'20').AND.(IARR(18).GE.0))THEN
            IF (FLAG(2) .EQ. 0) THEN
              IF (OFIGTS(REPORT,POS+2,POS+4)) THEN
!fafafa
                READ (REPORT(POS+2:POS+4),'(I3)') IARR(19)
                FLAG(1)=1
                FLAG(2)=1
                STORE=.TRUE.
                NUM_VALS=NUM_VALS+1
              ENDIF
            ENDIF
          ENDIF
*
* 3 Group (3dgdgfgfg - Direction and speed of max gust)
*
          IF (REPORT(POS:POS).EQ.'3')THEN
            IF (FLAG(3) .EQ. 0) THEN
              IF (OFIGTS(REPORT,POS+1,POS+2)) THEN
!dgdg
                READ (REPORT(POS+1:POS+2),'(I2)') IARR(25)
                IARR(25)=IARR(25)
                FLAG(3)=1
                STORE=.TRUE.
                NUM_VALS=NUM_VALS+1
              ENDIF

              IF (OFIGTS(REPORT,POS+3,POS+4)) THEN
!fgfg
                READ (REPORT(POS+3:POS+4),'(I2)') IARR(26)
                FLAG(3)=1
                STORE=.TRUE.
                NUM_VALS=NUM_VALS+1
              ENDIF
            ENDIF
          ENDIF
*
* 4 Group (40fgfgfg - Speed of max gust > 99)
*
          IF ((REPORT(POS:POS+1).EQ.'40').AND.(IARR(15).GE.0))THEN
            IF (FLAG(4) .EQ. 0) THEN
              IF (OFIGTS(REPORT,POS+2,POS+4)) THEN
!fgfg
                READ (REPORT(POS+2:POS+4),'(I3)') IARR(26)
                FLAG(4)=1
                STORE=.TRUE.
                NUM_VALS=NUM_VALS+1
              ENDIF
            ENDIF
          ENDIF
*
* 5 Group (5HHmm - Time of max gust)
*
          IF (REPORT(POS:POS).EQ.'5')THEN
            IF (FLAG(5) .EQ. 0) THEN
              IF (OFIGTS(REPORT,POS+1,POS+4)) THEN
!HHmm
                READ (REPORT(POS+1:POS+2),'(I2)') IARR(23)   !HH
                READ (REPORT(POS+3:POS+4),'(I2)') IARR(24)   !mm
                FLAG(5)=1
                STORE=.TRUE.
                NUM_VALS=NUM_VALS+1

! year, month and day for time of max gust

                IF(IARR(23).LE.OBDATE(4))THEN
! Take bulletin date for year, month and day values
                  IARR(20)=OBDATE(1)                         !YEAR
                  IARR(21)=OBDATE(2)                         !MONTH
                  IARR(22)=OBDATE(3)                         !DAY
                ELSE
! Take yesterday for year, month and day values
                  CALL DATE31(OBDATE(3),OBDATE(2),OBDATE(1),CENTDY)
                  CALL DATE13((CENTDY-1),DAY_M1,MON_M1,YR_M1)
                  IARR(20)=YR_M1                             !YEAR
                  IARR(21)=MON_M1                            !MONTH
                  IARR(22)=DAY_M1                            !DAY
                ENDIF
              ENDIF
            ENDIF
          ENDIF
*
* 6 Group (6snT0T0T0 - 10cm soil temperature)
*
          IF (REPORT(POS:POS).EQ.'6')THEN
            IF (FLAG(6) .EQ. 0) THEN
              IF (OFIGTS(REPORT,POS+1,POS+4)) THEN
                TTVAL=REPORT(POS+1:POS+4)                          !b
                IARR(12)=NINT(TEMPEXP(TTVAL)*10)                   !b
                FLAG(6)=1
                STORE=.TRUE.
                NUM_VALS=NUM_VALS+1
              ENDIF
            ENDIF
          ENDIF
*
* 7 Group (7kkkk - Global irradiation)
*
          IF (REPORT(POS:POS).EQ.'7')THEN
            IF (FLAG(7) .EQ. 0) THEN
              IF (OFIGTS(REPORT,POS+1,POS+4)) THEN
                IARR(13)=1
                READ (REPORT(POS+1:POS+4),'(I4)') IARR(14)
                FLAG(7)=1
                STORE=.TRUE.
                NUM_VALS=NUM_VALS+1
              ENDIF
            ENDIF
          ENDIF

          POS=POS+6
        ELSE
          SYNTAX=.TRUE.
c     PRINT*,' ENHEXP - GROUP LENGTH PROBLEM ',STNID,(OBDATE(I),I=3,5)
c     PRINT*,' >',REPORT(1:REPLEN),'<',POS,REPLEN,SPCPOS
          IF (SPCPOS.EQ.0) THEN
            POS=REPLEN
          ELSE
            POS=POS+SPCPOS
          ENDIF
        ENDIF
*
      ENDDO

*************************************************************
*
*     TRANSFER ELEMENTS FROM INTEGER ARRAY TO REAL ARRAY
*
*************************************************************

      DO I= 1,30
        IF(IARR(I).NE.MISSIN)THEN
          EXPARR(I)= REAL(IARR(I))
        ELSE
          EXPARR(I) = RMISS
        ENDIF
      ENDDO

*************************************************************
*
*     CONVERSIONS
*
*************************************************************

! set 10cm soil temperature depth coordinate value and temperature value
! to degrees and tenths.

      IF(IARR(12).NE.MISSIN)THEN
        EXPARR(11)= 0.1
        EXPARR(12)= EXPARR(12)/10.0
      ENDIF

! convert wind speeds from knots to metres per second

      IF(IARR(19).NE.MISSIN)THEN
        EXPARR(19)= EXPARR(19)*KTS2MPS
      ENDIF

      IF(IARR(26).NE.MISSIN)THEN
        EXPARR(26)= EXPARR(26)*KTS2MPS
      ENDIF

! convert wind directions from 10's of degrees to degrees

      IF(IARR(17).NE.MISSIN)THEN
        EXPARR(17)= EXPARR(17)*10
      ENDIF

      IF(IARR(25).NE.MISSIN)THEN
        EXPARR(25)= EXPARR(25)*10
      ENDIF

! convert global radiation from kJ/m2 to J/m2

      IF(IARR(14).NE.MISSIN)THEN
        EXPARR(14)= EXPARR(14)*1000
      ENDIF

c     PRINT*,' ENHEXP flags ',FLAG               ! test line
                                                 ! test line
c
c     IF(STORE)THEN                                ! test line
c       DO I= 1,30                                 ! test line
c         IF(expARR(I).NE.rmiss)THEN               ! test line
c           PRINT*,' ENHEXP ',I,EXPARR(I),iarr(i)  ! test line
c         ENDIF                                    ! test line
c       ENDDO                                      ! test line
c     ELSE                                         ! test line
c        PRINT*,' ENHEXP - NO DATA ELEMENTS REPORTED! '
c     ENDIF                                        ! test line

      RETURN
      END
