      SUBROUTINE SYNXP3(REPORT,POINT,NGRPS,SYNTAX,
     &   IWMOB,IWMOR,WNDKTS,WPER,IR,IDATIM,TPER1,TPER2,TGTG,VERTV,
     &   A3,HHH,MAXTTT,MINTTT,E,SSS,
     &   T1GUST,T2GUST,VGUST, T1GUS,T2GUS,VGUS,
     &   T1MEAN,T2MEAN,VMEAN,ICL,GROUP8,VSIF,RRR3,TR3,R24,IE,EEE,
     &   SUN2,SUN1,RNET,GLSO,DISO,LW,SW,RNET24,GLSO24,
     &   DISO24,LW24,SW24,PCHG24,SEASTE,SEAVIS,VV,
     &   LRAD,LSEA)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNXP3
!
! PURPOSE       : EXPANDS GROUPS FROM SECTION 3 OF A SYNOP REPORT.
!
! DESCRIPTION   : PROCESSES ALL GROUPS AFTER THE SECTION IDENTIFIER,
!                 CALLS SUBROUTINES FOR 5-GROUPS, 8-GROUPS & 9-GROUPS.
!
! CALLED BY     : SYNEXP
!
! CALLS         : SYNTPR, SYNFIV, SYNCLD, SYNNIN
!
! PARAMETERS    : (1)REPORT  CHAR    COMPLETE REPORT               (I)
!                 (2)POINT   INTEGER START OF GROUP AFTER 333      (I)
!                 (3)NGRPS   INTEGER TOTAL NUMBER OF GRPS IN S3    (I)
!                 (4)SYNTAX  LOGICAL TRUE IF SYNTAX ERROR FOUND    (O)
!                 (5)IWMOB   INTEGER WMO BLOCK                     (I)
!                 (6)IWMOR   INTEGER WMO REGION                    (I)
!                 (7)WNDKTS  LOGICAL TRUE IF WND REPORTED IN KTS   (I)
!                 (8)WPER    REAL    PAST WEATHER PERIOD (HOURS)   (I)
!                 (9)IR      REAL    RAINFALL REPORTING FLAG       (I)
!                (10)IDATIM  INTEGER ARRAY OF YMDHM                (I)
!                (11)-(50)   REAL    ELEMENTS. SEE SYNINI          (O)
!                (51)LRAD    LOGICAL TRUE IF RADIATION GRPS FOUND  (O)
!                (52)LSEA    LOGICAL TRUE IF COASTAL GRPS FOUND    (O)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:06$
! $Source: /data/us0400/mdb/op/lib/source/RCS/synxp3.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:06    Sheila Needham  
! $
! Revision 2.2  2002/12/02  14:31:51  14:31:51  usmdb (Generic MetDB account)
! Change day 16DEC02    R Hirst
! Correct the evaluation of rainfall periods for blocks 42 & 43.
! 
! Revision 2.1  2002/05/07  15:04:48  15:04:48  usmdb (Generic MetDB account)
! 20 May 2002 Section 3 Grp 7 should allow values from 0mm.
! Changed .GT.0 to .GE.0. John Hodkinson
!
! Revision 2.0  2001/07/03  10:44:10  10:44:10  usmdb (Generic MetDB account)
! changed preprocessor statement to if (1 ), else....  Removed
! unused variables, added copyright and modified header - S.Cox
!
! Revision 1.8  2000/11/07  12:03:42  12:03:42  usmdb (Generic MetDB account)
! 20/11/2000 Stan Kellett.
! Correction to MetDB storing Antartic Section 3, 7 groups as 24 Hour
! Rainfall accumulations.
!
! Revision 1.7  2000/10/17  09:42:35  09:42:35  usmdb (Generic MDB account)
! Lines of code missed out in September change, which meant
! that rainfall period was not handled correctly have
! now been added.          Stanley Kellett. 16/10/00
!
! Revision 1.7  2000/10/04  15:47:46  15:47:46  usmdb (Generic MDB account)
! 16/10/2000 Lines of code missed out in September change, which meant
! that rainfall period was not handled correctly have now been
! added. Stanley Kellett.
!
! Revision 1.6  2000/08/09  15:04:30  15:04:30  usmdb (Generic MDB account)
! 21/8/2000 Change to allow for Blks 42 and 43 reporting / at the end
! of the rainfall group to indicate rain since 3Z. Stan Kellett
!
! Revision 1.5  98/01/29  11:41:11  11:41:11  usmdb (Generic MDB account)
! Addition of IBM preprocess directive.
!
! Revision 1.4  97/07/31  11:40:25  11:40:25  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.3  1997/07/04 13:52:17  uspm
! Latest version from  1  - Y2K check
!
! Revision 1.2  1997/05/15 15:17:59  uspm
! Correct preprocessor checks
!
! FEB 95 - CONVERT RAINFALL TO KG/M**2 (I.E. MM), NOT METRES!
!
! JAN 95 - GET BOTH GUSTS (IF 2 REPORTED) BACK FROM SYNNIN
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
#else
      INCLUDE 'SYNCOM'
      INCLUDE 'SYNELM'
#endif

      INTEGER IVALUE
      INTEGER IWMOB,IWMOR,HR,IDATIM(*),IG,IH
      INTEGER POINT,NGRPS,I,IR
      CHARACTER REPORT*500
      LOGICAL LRAD,LSEA,SYNTAX
      INTEGER ISIG,ICL,LASTID                                       !2.0
      LOGICAL WNDKTS
      LOGICAL HEADSET                                               !2.1
      CHARACTER*132 CHEAD

      DATA HEADSET/.FALSE./                                         !2.1

      IF (.NOT.HEADSET) THEN                                        !2.1
        CHEAD='$RCSfile: $ '//
     &'$Revision: 1$ $Date: 30/01/2006 20:25:06$'
        HEADSET=.TRUE.                                              !2.1
      ENDIF                                                         !2.1

      ISIG=0 !  COUNT OF SIGNIFICANT LAYERS IN 8 GROUPS
      ICL=0  !  CLOUD 8 GROUP COUNTER
      HR=IDATIM(4)
***********************************************************************
*
* CHECK SEQUENCE OF GROUPS. SET SYNTAX FLAG & STOP IF DESCENDING
*
***********************************************************************
      IG=1
      LASTID=0
10    CONTINUE
      I=IVALUE(REPORT(POINT:POINT))
      IF(I.LT.LASTID)THEN
        SYNTAX=.TRUE.
        RETURN
      ENDIF
      LASTID=I
***********************************************************************
*
* MAX DRY BULB TEMPERATURE (DEGREES AND TENTHS TO K)
*
***********************************************************************
      IF (REPORT(POINT:POINT).EQ.'1') THEN
        I=IVALUE(REPORT(POINT+2:POINT+4))
        IF (I.NE.MISSIN) THEN
          IF (REPORT(POINT+1:POINT+1).EQ.'1') THEN
            MAXTTT=-I*0.1 + TCONV
          ELSE IF (REPORT(POINT+1:POINT+1).EQ.'0') THEN
            MAXTTT=I*0.1 + TCONV
          ENDIF
          IF (MAXTTT.GT.RMISS) CALL SYNTPR(IWMOR,HR,1,TPER1)
        ENDIF
***********************************************************************
*
* MIN DRY BULB TEMPERATURE (DEGREES AND TENTHS TO K)
*
***********************************************************************
      ELSE IF (REPORT(POINT:POINT).EQ.'2') THEN
        I=IVALUE(REPORT(POINT+2:POINT+4))
        IF (I.NE.MISSIN) THEN
          IF (REPORT(POINT+1:POINT+1).EQ.'1') THEN
            MINTTT=-I*0.1+TCONV
          ELSE IF (REPORT(POINT+1:POINT+1).EQ.'0') THEN
            MINTTT=I*0.1+TCONV
          ENDIF
          IF (MINTTT.GT.RMISS) CALL SYNTPR(IWMOR,HR,2,TPER2)
        ENDIF
***********************************************************************
*
* STATE OF GROUND WITHOUT SNOW AND GRASS MINIMUM
* E SYNOP CODE 0901 = BUFR CODE 020062
* GRASS MINIMUM (WHOLE DEGREES TO K)
*
***********************************************************************
      ELSE IF (REPORT(POINT:POINT).EQ.'3') THEN
        E=IVALUE(REPORT(POINT+1:POINT+1))
        I=IVALUE(REPORT(POINT+3:POINT+4))
        IF (I.NE.MISSIN) THEN
          IF (REPORT(POINT+2:POINT+2).EQ.'1') THEN
            TGTG=-I+TCONV
          ELSE IF (REPORT(POINT+2:POINT+2).EQ.'0') THEN
            TGTG= I+TCONV
          ENDIF
        ENDIF
***********************************************************************
*
* STATE OF GROUND WITH SNOW AND SNOW DEPTH.
* E' SYNOP CODE 0975 = BUFR CODE 020062 +10
* SSS SYNOP CODE 3889 = BUFR CODE 013013 (M AND -1,-2)
*
* #### NOTE THAT SOME IRANIAN STATIONS (BLOCK 40) REPORT THE
* #### 850MB LEVEL HEIGHT IN THIS GROUP, INSTEAD OF SNOW DATA.
*
***********************************************************************
      ELSE IF (REPORT(POINT:POINT).EQ.'4') THEN
        I=IVALUE(REPORT(POINT+1:POINT+1))
        IH=IVALUE(REPORT(POINT+2:POINT+4))
        IF (I.EQ.8 .AND. IWMOB.EQ.40 .AND. IH.GE.300) THEN
          A3=85000.     ! ASSUME 850MB HEIGHT
          HHH=IH+1000.
        ELSE            ! IT MUST BE STATE OF GROUND AND SNOW DEPTH
          IF (I.NE.MISSIN) E=I+10.
          IF (IH.GT.0 .AND. IH.LE.996) THEN
            SSS=IH*0.01
          ELSE IF (IH.EQ.997) THEN
            SSS=-1.
          ELSE IF (IH.EQ.998) THEN
            SSS=-2.
          ENDIF
        ENDIF
***********************************************************************
*
* HANDLE 5-GROUPS IN A SEPARATE ROUTINE (BECAUSE THE RADIATION GROUPS
* START WITH 0-6).  WHEN SYNFIV FINDS A GROUP UNCONNECTED WITH 5-GROUPS
* THE MAIN LOOP GOES ON WITH THAT GROUP RATHER THAN MOVING TO THE NEXT.
*
***********************************************************************
      ELSE IF(REPORT(POINT:POINT).EQ.'5')THEN
        LRAD=.TRUE.
        CALL SYNFIV(REPORT,POINT,NGRPS,IG,
     -    IR,IE,EEE,SUN2,SUN1,RNET,GLSO,DISO,LW,SW,SWNET,SDIR,
     -    RNET24,GLSO24,DISO24,LW24,SW24,SWNET24,SDIR24,PCHG24)
        IF (IG.LE.NGRPS) GO TO 10
***********************************************************************
*
* RAINFALL AMOUNT AND PERIOD.
* CONVERT AMOUNT FROM SYNOP CODE TABLE 3590 TO KG/M**2 (NUMERICALLY
* SAME AS MM) & PERIOD FROM CODE TABLE 4019 TO HOURS
*
***********************************************************************
      ELSE IF (REPORT(POINT:POINT).EQ.'6') THEN
        I=IVALUE(REPORT(POINT+1:POINT+3))
        IF (I.GT.0 .AND. I.LE.989) THEN
          RRR3=I
        ELSE IF (I.GT.990 .AND. I.LE.999) THEN
          RRR3=(I-990.)*0.1
        ELSE IF (I.EQ.990.) THEN
          RRR3=-1.
        ENDIF
*
!1.7  For India and Sri-Lanka a tR value of / means that the rain
!1.6  reported is that since 0300Z.
        IF((REPORT(POINT+4:POINT+4).EQ.'/').AND.(IWMOB.GE.42)   !1.6
     &    .AND.(IWMOB.LE.43)) THEN                              !1.6
          TR3=-(MOD(IDATIM(4)+20,24)+1)                     !2.2!1.7
        ELSE                                                    !1.6
          I = IVALUE(REPORT(POINT+4:POINT+4))                   !1.7
          IF (I.GE.1 .AND. I.LE.4) THEN                         !1.6
            TR3=-I*6                                            !1.6
          ELSE IF (I.GE.5 .AND. I.LE.7) THEN                    !1.6
            TR3=-(I-4.)                                         !1.6
          ELSE IF (I.EQ.8) THEN                                 !1.6
            TR3=-9.                                             !1.6
          ELSE IF (I.EQ.9) THEN                                 !1.6
            TR3=-15.                                            !1.6
          ELSE                                                  !1.7
            TR3=I                                               !1.7
          ENDIF                                                 !1.6
        ENDIF                                                   !1.6
*
* SOME BLOCKS IN THE TWENTIES & THIRTIES REPORT 12-HOUR RAINFALL WITH
* TR=0 BECAUSE THE 12-HOUR PERIOD DEPENDS ON LOCAL TIME RATHER THAN
* ENDING AT THE REPORT TIME.
*
        IF (I.EQ.0 .AND.IWMOB.GE.20.AND.IWMOB.LE.39) TR3=-12.
***********************************************************************
*
* 24 HOUR RAINFALL AMOUNT (TENTHS OF MM TO KG/M**2, I.E. MM)
*
***********************************************************************
      ELSE IF (REPORT(POINT:POINT).EQ.'7') THEN
        I=IVALUE(REPORT(POINT+1:POINT+4))
        IF (IWMOB.EQ.89) THEN             !1.8 Antartic 7 groups hold
          R24=-9999999.0                  !1.8 prevailing wind not rain
                                          !1.8 so set R24 to missing
        ELSE                              !1.8 It is 24 hour rainfall
          IF (I.GE.0 .AND. I.LE.9998) THEN                    !2.1
            R24=I*0.1
          ELSE IF (I.EQ.9999) THEN
            R24=-1.
          ENDIF
        ENDIF                             !1.8
***********************************************************************
*
* HANDLE CLOUD 8-GROUPS IN SUBROUTINE
*
***********************************************************************
      ELSE IF (REPORT(POINT:POINT).EQ.'8') THEN    !  CLOUD GROUPS
        IF (REPORT(POINT:POINT+4).NE.'80000') THEN
          CALL SYNCLD(POINT,REPORT,IG,NGRPS,ICL,GROUP8,VSIF,VERTV)
        ENDIF
***********************************************************************
*
* HANDLE 9-GROUPS IN SUBROUTINE, RETURNING AT END OF 9-GROUPS
*
***********************************************************************
      ELSE IF (REPORT(POINT:POINT).EQ.'9') THEN
        CALL SYNNIN(REPORT(POINT:),NGRPS-IG+1,WNDKTS,WPER,
     -              T1GUST,T2GUST,VGUST, T1GUS,T2GUS,VGUS,
     -              T1MEAN,T2MEAN,VMEAN,
     -              LSEA,SEASTE,SEAVIS,VV)
        RETURN
      ENDIF
***********************************************************************
*
* MOVE ON TO NEXT GROUP
*
***********************************************************************
      POINT=POINT+6   !  NEXT GROUP
      IG=IG+1
      IF (IG.LE.NGRPS) GOTO 10
999   CONTINUE
      RETURN
      END
