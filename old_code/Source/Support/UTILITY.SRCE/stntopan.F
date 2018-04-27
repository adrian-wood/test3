!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                     !
!                                                                     !
! PURPOSE       : COPY STN DETAILS TO PANEL VARIABLES                 !
!                                                                     !
! CALLED BY     : STNDISP (FORTRAN)                                   !
!                                                                     !
! PARAMETERS    : NONE                                                !
!                                                                     !
! CHANGE RECORD :                                                     !
!   DATE : JUL 92  PURPOSE: FIRST VERSION OF SOFTWARE                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$Log:
! 1    Met_DB_Project 1.0         28/02/2006 12:07:56    Sheila Needham  
!$
!Revision 1.2  1999/06/18 09:38:29  usmdb
!Fix for 1999 problem
!
!$Revision: 1$
!$Date: 28/02/2006 12:07:56$                                                              
!
!

*234567
      SUBROUTINE STNTOPAN
*
* INCLUDE ALL THE VARIABLE STATEMENTS    
*
      INCLUDE(STNINIT)
*
      MISSING=-9999999                                              !1.2
*
      DAY1=DAY(1)
      DAY2=DAY(2)
      DAY3=DAY(3)
      DAY4=DAY(4)
      DAY5=DAY(5)
      DAY6=DAY(6)
      DAY7=DAY(7)
      DAY8=DAY(8)
      DAY9=DAY(9)
      DAY10=DAY(10)
      DO 54 IJK=1,13
        IF(SYN(IJK:IJK).EQ.'X')THEN
           SYN(IJK:IJK)='Y'
        ELSE
           SYN(IJK:IJK)='N'
        ENDIF
  54  CONTINUE
      DO 53 IJK=1,23
        IF(NCM(IJK:IJK).EQ.'X')THEN
           NCM(IJK:IJK)='Y'
        ELSE
           NCM(IJK:IJK)='N'
        ENDIF
  53  CONTINUE
      SYNTIM1=SYNTIM(1)
      SYNTIM2=SYNTIM(2)
      SYNTIM3=SYNTIM(3)
      SYNTIM4=SYNTIM(4)
      SYNTIM5=SYNTIM(5)
      SYNTIM6=SYNTIM(6)
      SYNTIM7=SYNTIM(7)
      SYNTIM8=SYNTIM(8)
      SYNTIM9=SYNTIM(9)
      SYNTIM10=SYNTIM(10)
      IF(NCMTIM1(1).EQ.0)THEN
        NCMTIM1A='NO '
      ELSE
        NCMTIM1A='YES'
      ENDIF
      IF(NCMTIM1(2).EQ.0)THEN
        NCMTIM1B='NO '
      ELSE
        NCMTIM1B='YES'
      ENDIF
      IF(NCMTIM1(3).EQ.0)THEN
        NCMTIM1C='NO '
      ELSE
        NCMTIM1C='YES'
      ENDIF
      IF(NCMTIM1(4).EQ.0)THEN
        NCMTIM1D='NO '
      ELSE
        NCMTIM1D='YES'
      ENDIF
      IF(NCMTIM1(5).EQ.0)THEN
        NCMTIM1E='NO '
      ELSE
        NCMTIM1E='YES'
      ENDIF
      IF(NCMTIM1(6).EQ.0)THEN
        NCMTIM1F='NO '
      ELSE
        NCMTIM1F='YES'
      ENDIF
      IF(NCMTIM1(7).EQ.0)THEN
        NCMTIM1G='NO '
      ELSE
        NCMTIM1G='YES'
      ENDIF
      IF(NCMTIM1(8).EQ.0)THEN
        NCMTIM1H='NO '
      ELSE
        NCMTIM1H='YES'
      ENDIF
      IF(NCMTIM1(9).EQ.0)THEN
        NCMTIM1I='NO '
      ELSE
        NCMTIM1I='YES'
      ENDIF
      IF(NCMTIM1(10).EQ.0)THEN
        NCMTIM1J='NO '
      ELSE
        NCMTIM1J='YES'
      ENDIF
      IF(NCMTIM2(1).EQ.0)THEN
        NCMTIM2A='NO '
      ELSE
        NCMTIM2A='YES'
      ENDIF
      IF(NCMTIM2(2).EQ.0)THEN
        NCMTIM2B='NO '
      ELSE
        NCMTIM2B='YES'
      ENDIF
      IF(NCMTIM2(3).EQ.0)THEN
        NCMTIM2C='NO '
      ELSE
        NCMTIM2C='YES'
      ENDIF
      IF(NCMTIM2(4).EQ.0)THEN
        NCMTIM2D='NO '
      ELSE
        NCMTIM2D='YES'
      ENDIF
      IF(NCMTIM2(5).EQ.0)THEN
        NCMTIM2E='NO '
      ELSE
        NCMTIM2E='YES'
      ENDIF
      IF(NCMTIM2(6).EQ.0)THEN
        NCMTIM2F='NO '
      ELSE
        NCMTIM2F='YES'
      ENDIF
      IF(NCMTIM2(7).EQ.0)THEN
        NCMTIM2G='NO '
      ELSE
        NCMTIM2G='YES'
      ENDIF
      IF(NCMTIM2(8).EQ.0)THEN
        NCMTIM2H='NO '
      ELSE
        NCMTIM2H='YES'
      ENDIF
      IF(NCMTIM2(9).EQ.0)THEN
        NCMTIM2I='NO '
      ELSE
        NCMTIM2I='YES'
      ENDIF
      IF(NCMTIM2(10).EQ.0)THEN
        NCMTIM2J='NO '
      ELSE
        NCMTIM2J='YES'
      ENDIF
      F1=FLAGS(1:1)
      F2=FLAGS(2:2)
      F3=FLAGS(3:3)
      F4=FLAGS(4:4)
      F5=FLAGS(5:5)
      F6=FLAGS(6:6)
      F7=FLAGS(7:7)
      F8=FLAGS(8:8)
      F9=FLAGS(9:9)
      F10=FLAGS(10:10)
      F11=FLAGS(11:11)
      F12=FLAGS(12:12)
      F13=FLAGS(13:13)
      F14=FLAGS(14:14)
      F15=FLAGS(15:15)
      F16=FLAGS(16:16)
      F17=FLAGS(17:17)
      F18=FLAGS(18:18)
      F19=FLAGS(19:19)
      F20=FLAGS(20:20)
      F21=FLAGS(21:21)
      F22=FLAGS(22:22)
      F23=FLAGS(23:23)
      F24=FLAGS(24:24)
*
      BLKSTN=BLK*1000+STN                                               02240076
      IF (BLKSTN .LT. 99999) THEN                                       02260099
        WRITE (WMOCHA, '(I5.5)') BLKSTN                                 02270099
      ELSE                                                              02280099
        WMOCHA='-----'                                                  02290099
      ENDIF                                                             02300099
      IF (ICAO.EQ.'    ' .OR. F15.EQ.'2') THEN                          02310099
         ICAOCHA='----'
      ELSE
         ICAOCHA=ICAO
      ENDIF
      IF (DCNN .GT. 0) THEN                                             02320099
        WRITE (DCNNCHA, '(I4.4)') DCNN                                  02330099
      ELSE                                                              02340099
        DCNNCHA='----'                                                  02350099
      ENDIF                                                             02360099
      IF (RAIN .GT. 0) THEN                                             02370099
        WRITE (RAINCHA, '(I6.6)') RAIN                                  02380099
      ELSE                                                              02390099
        RAINCHA='------'                                                02400099
      ENDIF                                                             02410099
*
      IF(OYEAR.EQ.MISSING)THEN                                      !1.2
        COYEAR='--'
      ELSE
        WRITE(COYEAR,'(I2.2)')OYEAR
      ENDIF
*
      IF(OMONTH.EQ.MISSING)THEN                                     !1.2
        COMONTH='--'
      ELSE
        WRITE(COMONTH,'(I2.2)')OMONTH
      ENDIF
*
      IF(ODAY.EQ.MISSING)THEN                                       !1.2
        CODAY='--'
      ELSE
        WRITE(CODAY,'(I2.2)')ODAY
      ENDIF
*
      IF(OHOUR.EQ.MISSING)THEN                                      !1.2
        COHOUR='--'
      ELSE
        WRITE(COHOUR,'(I2.2)')OHOUR
      ENDIF
*
      IF(CYEAR.EQ.MISSING)THEN                                      !1.2
        CCYEAR='--'
      ELSE
        WRITE(CCYEAR,'(I2.2)')CYEAR
      ENDIF
*
      IF(CMONTH.EQ.MISSING)THEN                                     !1.2
        CCMONTH='--'
      ELSE
        WRITE(CCMONTH,'(I2.2)')CMONTH
      ENDIF
*
      IF(CDAY.EQ.MISSING)THEN                                       !1.2
        CCDAY='--'
      ELSE
        WRITE(CCDAY,'(I2.2)')CDAY
      ENDIF
*
      IF(CHOUR.EQ.MISSING)THEN                                      !1.2
        CCHOUR='--'
      ELSE
        WRITE(CCHOUR,'(I2.2)')CHOUR
      ENDIF
*
      RETURN
      END
