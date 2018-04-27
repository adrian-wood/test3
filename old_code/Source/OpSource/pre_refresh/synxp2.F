      SUBROUTINE SYNXP2(REPORT,POINT,NGRPS,SYNTAX,
     &        LSEA,ISWELL,TWTYPE,TWTWTW,
     &        DS,VS,TBTYPE,TBTBTB,WVMS,WVHT,WVPER,SWELWV,
     &        ISICE,ESES,RS,CI,SI,BI,DI,ZI,REPLEN)                    !A

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNXP2
!
! PURPOSE       : TO EXPAND SECTION 2 OF A SYNOP/SHIP REPORT INTO
!                 REAL VARIABLES.
!
! DESCRIPTION   :  WMO CODE FM 12-IX (SYNOP COASTAL STNS)
!                  WMO CODE FM 13-IX (SHIP)
!
! CALLED BY     : SYNEXP
!
! CALLS         : IVALUE (FUNCTION)
!
! PARAMETERS    : (1)REPORT CHAR      FULL REPORT                 (I)
!                 (2)POINT  INTEGER   START OF 222.. GROUP        (I)
!                 (3)NGRPS  INTEGER   NUMBER OF GROUPS IN SECTION (I)
!                 (4)SYNTAX LOGICAL   TRUE IF SYNTAX ERROR FOUND  (O)
!                 (5)LSEA   LOGICAL   TRUE IF USEFUL DATA FOUND   (O)
!                 (6)-(24)  REAL      VARIABLES (SEE SHP/SYNINI)  (O)
!                 (25)      INTEGER   LENGTH OF REPORT            (I) !A
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:05$
! $Source: /home/us0400/mdb/op/lib/source/RCS/synxp2.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:05    Sheila Needham  
! $
! Revision 2.1  2005/08/02 10:42:54  usmdb
! 15/08/2005 Stop processing of '2' group when '1' group a
! lready
! processed. John Norton.
!
! Revision 2.0  2001/07/03  10:44:10  10:44:10  usmdb (MetDB account c/o John C Ward)
! changed preprocessor statement to if (MVS), else....  Removed
! unused variable, added copyright and modified header - S.Cox
!
! Revision 1.6  2001/03/07  11:32:31  11:32:31  usmdb (Generic MetDB account)
! Change day:   19MAR01         R Hirst
! Change de-coding of a variable/confused swell wave direction
! from 0 to 990.
!
! Revision 1.5  98/01/29  11:40:36  11:40:36  usmdb (Generic MetDB account)
! Addition of IBM preprocess directive
!
! Revision 1.4  97/08/28  10:38:14  10:38:14  usjl (Jon Lewthwaite)
! REPLEN passed from SYNEXP. Used to check that we havent come to the
! end of the report when checking for ICE data. Jon Lewthwaite
!
! NOV 94: SET WAVE & SWELL PERIODS TO MISSING IF 99 (CONFUSED SEA)
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

#if defined (MVS)
      INCLUDE (SYNCOM)                                                          
      INCLUDE (SYNELM)                                                          
      INCLUDE (SHPELM)                                                          
#else
      INCLUDE 'SYNCOM'
      INCLUDE 'SYNELM'
      INCLUDE 'SHPELM'
#endif

      CHARACTER REPORT*(*)
      INTEGER NGRPS
      INTEGER IVALUE,LASTID
      INTEGER I,IS,IG
      INTEGER POINT,ISWELL
      INTEGER REPLEN                                                 !A
      LOGICAL LSEA,WAVE,SYNTAX                                      !2.0
      REAL SPCONV(0:9)                                              !2.0
      CHARACTER*132 CHEAD

      DATA SPCONV/0.,1.,4.,7.,9.,12.,14.,17.,19.,21./               !2.0

      CHEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/synxp2.F,v $
     &'//'$ $Date: 30/01/2006 20:25:05$ $Revision: 1$'
*
      ISWELL=0
      WAVE=.FALSE.
      IG=1
      LASTID=0
*
* CHECK THAT GROUPS ARE IN ASCENDING ORDER AFTER 222 GROUP ITSELF
*
10    CONTINUE
      I=IVALUE(REPORT(POINT:POINT))
      IF (I.NE.MISSIN .AND. IG.GT.1) THEN
        IF (I.LT.LASTID) THEN
          SYNTAX=.TRUE.
          RETURN
        ENDIF
        LASTID=I
      ENDIF
*
* 222DSVS DIRECTION AND SPEED OF MOVEMENT
* CONVERT DS FROM COMPASS POINTS (TBL 0700) TO DEGREES
* CONVERT VS FROM MULTIPLES OF 5KTS TO M/S (USING CONVERSIONS PROPOSED
* BY WMO CBS - WG DATA MAN. 2ND SESSION FEB 1994)
*
      IF (REPORT(POINT:POINT+2).EQ.'222') THEN
        I=IVALUE(REPORT(POINT+3:POINT+3))
        IF (I.NE.MISSIN .AND. I.LT.9) DS=I*45.
        I=IVALUE(REPORT(POINT+4:POINT+4))
        IF (I.NE.MISSIN) VS=SPCONV(I)
        LSEA=.TRUE.
*
* SEA TEMPERATURE GROUP, INDICATOR IS SIGN AND MEASUREMENT METHOD
* CONVERT TEMP IN TENTHS TO K
* CONVERT MEASURMENT METHOD TO BUFR TBL 002038
*
      ELSEIF (REPORT(POINT:POINT).EQ.'0') THEN
        I=IVALUE(REPORT(POINT+2:POINT+4))
        IF (I.NE.MISSIN) THEN
          IS=IVALUE(REPORT(POINT+1:POINT+1))
          IF (IS.NE.MISSIN) THEN
            IF (MOD(IS,2).EQ.1) THEN
              TWTWTW=-I*0.1+TCONV
            ELSE
              TWTWTW=I*0.1+TCONV
            ENDIF
            LSEA=.TRUE.
            IF (IS.EQ.0.OR.IS.EQ.1) THEN
              TWTYPE=0.
            ELSE IF (IS.EQ.2.OR.IS.EQ.3) THEN
              TWTYPE=1.
            ELSE IF (IS.EQ.4.OR.IS.EQ.5) THEN
              TWTYPE=2.
            ENDIF
          ENDIF
        ENDIF
*
* INSTRUMENTAL WAVE DATA HEIGHT IN UNITS OF 0.5 M.
* PERIOD OF 99 (CONFUSED SEA) SET TO MISSING.
*
      ELSE IF (REPORT(POINT:POINT).EQ.'1') THEN
        WVMS=0.             ! INSTRUMENTAL MEASUREMENT GROUP
        I=IVALUE(REPORT(POINT+1:POINT+2))
        IF (I.NE.MISSIN .AND. I.NE.99) WVPER=I
        I=IVALUE(REPORT(POINT+3:POINT+4))
        IF (I.NE.MISSIN) WVHT=I*0.5   ! METRES
        LSEA=.TRUE.
        WAVE=.TRUE.         ! MAY BE MORE ACCURATE DATA IN '70' GROUP
*
* WIND WAVE GROUP; AS ABOVE BUT ESTIMATED.
*
      ELSE IF (REPORT(POINT:POINT).EQ.'2') THEN
        IF (.NOT.WAVE) THEN                                         !2.1
          WVMS=1.  !  ESTIMATED WAVE GROUP
          I=IVALUE(REPORT(POINT+1:POINT+2))
          IF (I.NE.MISSIN .AND. I.NE.99) WVPER=I
          I=IVALUE(REPORT(POINT+3:POINT+4))
          IF (I.NE.MISSIN) WVHT=I*0.5   ! METRES
          LSEA=.TRUE.
        ENDIF                                                       !2.1
*
*  SWELL WAVE GROUP, TWO POSSIBLE SWELLS. THIS GROUP CONTAINS BOTH
*  DIRECTIONS; GROUP '4' HAS PERIOD AND HT OF FIRST; GROUP '5' THEN
*  PERIOD AND HEIGHT OF THE SECOND
*
      ELSE IF (REPORT(POINT:POINT).EQ.'3') THEN
        I=IVALUE(REPORT(POINT+1:POINT+2))
        IF (I.NE.MISSIN) THEN
          ISWELL=ISWELL+1
          IF (I.EQ.99) THEN
            SWELWV(1)=990.  ! VARIABLE                              !1.6
          ELSE
            SWELWV(1)=I*10. !  DIRECTION IN DEGREES.
          ENDIF
          LSEA=.TRUE.
        ENDIF
        I=IVALUE(REPORT(POINT+3:POINT+4))
        IF (I.NE.MISSIN) THEN
          ISWELL=ISWELL+1
          IF (I.EQ.99) THEN
            SWELWV(4)=990.  ! VARIABLE                              !1.6
          ELSE
            SWELWV(4)=I*10. ! DIRECTION IN DEGREES.
          ENDIF
          LSEA=.TRUE.
        ENDIF
*
*  SWELL WAVE PERIOD AND HEIGHT FOR FIRST SWELL GROUP
*
      ELSE IF (REPORT(POINT:POINT).EQ.'4') THEN
        I=IVALUE(REPORT(POINT+1:POINT+2))
        IF (I.NE.99) SWELWV(2)=I         ! PERIOD IN SECONDS
        I=IVALUE(REPORT(POINT+3:POINT+4))
        IF (I.NE.MISSIN) SWELWV(3)=I*0.5 ! WAVE HEIGHT, M.
        LSEA=.TRUE.
*
*  SWELL WAVE GROUP, SECOND OF TWO GROUPS ALLOWED.
*
      ELSE IF (REPORT(POINT:POINT).EQ.'5') THEN
        I=IVALUE(REPORT(POINT+1:POINT+2))
        IF (I.NE.99) SWELWV(5)=I         ! PERIOD IN SECONDS
        I=IVALUE(REPORT(POINT+3:POINT+4))
        IF (I.NE.MISSIN) SWELWV(6)=I*0.5 ! WAVE HEIGHT, M.
        LSEA=.TRUE.
*
* ICE ACCRETION, THICKNESS AND RATE.
* CONVERT SHIP CODE TBL 1751 TO BUFR FLAG TBL 20033
* ICE THICKNESS FROM CM TO M
* RATE OF ACCRETION CODE TBL 3551 = BUFR CODE 20032
*
      ELSEIF (REPORT(POINT:POINT).EQ.'6') THEN
        I=IVALUE(REPORT(POINT+1:POINT+1))
        IF (I.NE.MISSIN) THEN
          IF (I.EQ.1) ISICE=8
          IF (I.EQ.2) ISICE=4
          IF (I.EQ.3) ISICE=12
          IF (I.EQ.4) ISICE=2
          IF (I.EQ.5) ISICE=10
        ENDIF
        I=IVALUE(REPORT(POINT+2:POINT+3))
        IF (I.NE.MISSIN) ESES=I*0.01
        RS=IVALUE(REPORT(POINT+4:POINT+4))
        LSEA=.TRUE.
*
* MORE ACCURATE MEASURED WAVE HEIGHTS. OVERWRITE THOSE FROM 1-GROUP
*
      ELSEIF (REPORT(POINT:POINT+1).EQ.'70') THEN
        IF (WAVE) THEN
          I=IVALUE(REPORT(POINT+2:POINT+4))
          IF (I.NE.MISSIN) WVHT=I*0.1
        ENDIF
*
* WET BULB TEMPERATURE AND MEASUREMENT TYPE
* SHIP CODE TBL 3855 TO BUFR TBL 002039
*
      ELSEIF (REPORT(POINT:POINT).EQ.'8') THEN
        I=IVALUE(REPORT(POINT+2:POINT+4))
        IF (I.NE.MISSIN) THEN
          IS=IVALUE(REPORT(POINT+1:POINT+1))
          IF (IS.NE.MISSIN) THEN
            IF (IS.EQ.0.OR.IS.EQ.5) THEN
              TBTBTB=I*0.1+TCONV
            ELSE
              TBTBTB=-I*0.1+TCONV
            ENDIF
            LSEA=.TRUE.
            IF (IS.EQ.0.OR.IS.EQ.1) THEN
              TBTYPE=0.
            ELSE IF (IS.EQ.2) THEN
              TBTYPE=1.
            ELSE IF (IS.EQ.5.OR.IS.EQ.6) THEN
              TBTYPE=2.
            ELSE IF (IS.EQ.7) THEN
              TBTYPE=3.
            ENDIF
          ENDIF
        ENDIF
*
* ICE DATA - 5 DIGIT GROUP FOLLOWING THE WORD 'ICE '
*
      ELSEIF (POINT+3 .LE. REPLEN) THEN                               !A
        IF (REPORT(POINT:POINT+3).EQ.'ICE ') THEN
          POINT=POINT+4
          IG=IG+1
          IF (IG.GT.NGRPS) GOTO 999
*
* ASSUME THE NEXT GROUP IS ICE INFO - CISIBIDIZI
* (CONCENTRATION, DEVELOPMENT, AMOUNT & TYPE, BEARING, SITUATION/TREND)
* CI IS CODE TABLE 0639, 020034 IN BUFR, SI IS TABLE 3739/020037,
* BI IS TABLE 0439/020035, ZI IS 5239/020036.
*
          CI=IVALUE(REPORT(POINT:POINT))
          SI=IVALUE(REPORT(POINT+1:POINT+1))
          BI=IVALUE(REPORT(POINT+2:POINT+2))
          I=IVALUE(REPORT(POINT+3:POINT+3))
          IF (I.GE.1.AND.I.LE.8) DI=I*45.
          ZI=IVALUE(REPORT(POINT+4:POINT+4))
          LSEA=.TRUE.
        ENDIF
      ENDIF
*
* INCREMENT GROUP POINTERS (ALL GROUPS HAVE 5 FIGURES)
*
      POINT=POINT+6
      IG=IG+1
      IF (IG.LE.NGRPS) GOTO 10
999   CONTINUE
      RETURN
      END
