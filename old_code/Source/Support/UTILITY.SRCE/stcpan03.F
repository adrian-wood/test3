***********************************************************************
*                                                                     *
* PROGRAM       : STCPAN03                                            *
*                                                                     *
* PURPOSE       : COPY STN DETAILS TO PANEL VARIABLES                 *
*                 INCLUDING SETTING CLOSE DATES ETC                   *
*                                                                     *
* CALLED BY     : STNDISP (FORTRAN)                                   *
*                                                                     *
* PARAMETERS    : NONE                                                *
*                                                                     *
*Y2K  16.06.1997  STCPAN03 IS YEAR 2000 COMPLIANT.
*                                                                     *
* CHANGE RECORD :                                                     *
*   DATE : SEP 92  PURPOSE: FIRST VERSION OF SOFTWARE                 *
*                                                                     *
***********************************************************************
*234567
      SUBROUTINE STCPAN03
*
* INCLUDE ALL THE VARIABLE STATEMENTS
*
      INCLUDE(STNINIT)
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
      CYEAR=IYEAR
      CMONTH=IMONTH
      CDAY=IDAY
      CHOUR=IHOUR
*
      IF(OYEAR.EQ.99)THEN
        COYEAR='--'
      ELSE
        WRITE(COYEAR,'(I2.2)')OYEAR
      ENDIF
*
      IF(OMONTH.EQ.99)THEN
        COMONTH='--'
      ELSE
        WRITE(COMONTH,'(I2.2)')OMONTH
      ENDIF
*
      IF(ODAY.EQ.99)THEN
        CODAY='--'
      ELSE
        WRITE(CODAY,'(I2.2)')ODAY
      ENDIF
*
      IF(OHOUR.EQ.99)THEN
        COHOUR='--'
      ELSE
        WRITE(COHOUR,'(I2.2)')OHOUR
      ENDIF
*
      IF(CYEAR.EQ.99)THEN
        CCYEAR='--'
      ELSE
        WRITE(CCYEAR,'(I2.2)')CYEAR
      ENDIF
*
      IF(CMONTH.EQ.99)THEN
        CCMONTH='--'
      ELSE
        WRITE(CCMONTH,'(I2.2)')CMONTH
      ENDIF
*
      IF(CDAY.EQ.99)THEN
        CCDAY='--'
      ELSE
        WRITE(CCDAY,'(I2.2)')CDAY
      ENDIF
*
      IF(CHOUR.EQ.99)THEN
        CCHOUR='--'
      ELSE
        WRITE(CCHOUR,'(I2.2)')CHOUR
      ENDIF
*
      RETURN
      END
