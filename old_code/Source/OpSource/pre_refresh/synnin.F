      SUBROUTINE SYNNIN(GROUP9,NINGPS,WNDKTS,WPER,
     &                  T1GUST,T2GUST,VGUST, T1GUS,T2GUS,VGUS,
     &                  T1MEAN,T2MEAN,VMEAN,
     &                  LSEA,SEASTE,SEAVIS,VV)

!-----------------------------------------------------------------------
!                                                                     
! SUBROUTINE    : SYNNIN                                              
!                                                                     
! PURPOSE       : TO GET DATA FROM 9-GROUPS IN SECTION 3 OF SYNOP:    
!                 WIND, SEA & VISIBILITY                              
!                                                                     
! DESCRIPTION   : LOOK FOR 904.. & 907.. (TIME GROUPS), 910.. & 911.. 
!                 (MAX GUST), 912.. (MAX MEAN WIND), 920.., 921.. AND 
!                 924.. (SEA) AND 961.. (VISIBILITY IN DENSE FOG).    
!                                                                     
! CALLED BY     : SYNXP3                                              
!                                                                     
! PARAMETERS    : (1) GROUP9 CHAR    ARRAY OF 9-GROUPS            (I) 
!                 (2) NINGPS INTEGER NUMBER OF 9-GROUPS           (I) 
!                 (3) WNDKTS LOGICAL TRUE IF UNITS ARE KNOTS      (I) 
!                 (4) WPER   REAL    PAST WEATHER PERIOD (HOURS)  (I) 
!                 (5) T1GUST REAL    START OF GUST PERIOD         (O) 
!                 (6) T2GUST REAL    END OF GUST PERIOD           (O) 
!                 (7) VGUST  REAL    MAX GUST                     (O) 
!                 (8) T1MEAN REAL    START OF MAX WIND PERIOD     (O) 
!                 (9) T2MEAN REAL    END OF MAX WIND PERIOD       (O) 
!                (10) VMEAN  REAL    MAX MEAN WIND                (O) 
!                (11) LSEA   LOGICAL SET IF SEA GROUP FOUND       (O) 
!                (12) SEASTE REAL    STATE OF SEA                 (O) 
!                (13) SEAVIS REAL    VISIBILITY TOWARDS SEA       (O) 
!                (14) VV     REAL    VISIBILITY                   (O) 
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:52$
! $Source: /home/us0400/mdb/op/lib/source/RCS/synnin.F,v $
!                                                                     
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:52    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:06  usmdb
! Changed preprocessor statement to if (MVS), else...
! Separated variable declaration and initialisation. declared
! array dimension variables before array declaration. Added
! copyright and modified header - S.Cox
!
! Revision 1.6  2000/03/10  09:55:23  09:55:23  usmdb (Generic MetDB account)
! 20 March 2000     C Long
! 1.6  Set dense fog visibilities to 80m,50m,20m: only tens of metres
!      kept, so that's how 75,45,15 decode anyway!
! 
! Revision 1.5  98/01/29  11:34:48  11:34:48  usmdb (Generic MDB account)
! Addition of IBM preprocess directive.
!
! Revision 1.4  97/07/31  11:38:35  11:38:35  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.3  1997/07/04 13:45:12  uspm
! Latest version from  1  - Y2K check
!
! Revision 1.2  1997/05/15 15:10:46  uspm
! Add revision information and preprocessor checks about INCLUDE
! statements
!
! JAN 95: RETURN BOTH GUSTS (IF TWO REPORTED)                       
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
      INCLUDE (SYNELM)
      INCLUDE (SYNCOM)
#else
      INCLUDE 'SYNELM'
      INCLUDE 'SYNCOM'
#endif

      REAL TT,WIND, START,END
      LOGICAL WNDKTS, LSEA
      INTEGER NINGPS                                                !2.0
      CHARACTER*6 GROUP9(NINGPS) ! 5-FIGURE GROUPS WITH SPACES BETWEEN
      INTEGER I,IG,IND,LASTIND,IVALUE                               !2.0
      REAL VSCONV(0:9)                                              !2.0
      CHARACTER*132 CHEAD
*
* SINGLE-FIGURE VISIBILITY CODE (4300): CONVERT TO LOWEST VALUE IN RANGE
*
      DATA VSCONV/0.,50.,200.,500.,1000.,2000.,4000.,10000.,
     &          20000.,50000./                                      !2.0

      CHEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/synnin.F,v $
     $'//'$ $Date: 30/01/2006 20:24:52$ $Author: Sheila Needham$ '
     &//'$Revision: 1$'
*
* LOOP OVER 9-GROUPS (WHICH MAY BE FOLLOWED BY 0-GROUPS FOR WINDS >100
* - HENCE NOT SIMPLY DO 10 IG=1,NINGPS!
*
      IG=1
      LASTIND=0
   10 IND=IVALUE(GROUP9(IG)(1:3))
***********************************************************************
*
* WIND GROUPS
*
***********************************************************************
*
* WIND GROUPS MAY BE PRECEDED BY TIME GROUPS: 907.. CHANGES PERIOD FROM
* 10 MINUTES, 904.. CHANGES END OF PERIOD FROM REPORT TIME TO EARLIER.
* SUCH A CHANGE ONLY APPLIES TO THE NEXT GROUP.
*
      IF (IND.EQ.904 .OR. IND.EQ.907) THEN
        I=IVALUE(GROUP9(IG)(4:5))
        IF (I.LE.60) THEN
          TT=I*6.0               ! TENTHS OF AN HOUR TO MINUTES
        ELSE IF (I.GE.61 .AND. I.LE.66) THEN
          TT=(I-60+6)*60         ! WHOLE HOURS (7-12) TO MINUTES
        ELSE IF (I.GT.66) THEN
          TT=RMISS
        ENDIF
        LASTIND=IND              ! KEEP 904/907 TO SET PERIOD
*
* 9-GROUPS WITH WINDS IN MAY HAVE SPEED=99 AND A 0-GROUP TO FOLLOW
* IF THE SPEED IS >100.  CONVERT KNOTS TO M/S IF NECESSARY.
*
      ELSE IF (IND.EQ.910 .OR. IND.EQ.911 .OR. IND.EQ.912) THEN
        I=IVALUE(GROUP9(IG)(4:5))
        IF(I.EQ.99)THEN
          IG=IG+1
          IF (IG.GT.NINGPS) RETURN
          IF (GROUP9(IG)(1:2).EQ.'00') I=IVALUE(GROUP9(IG)(3:5))
        ENDIF
*
        WIND=I
        IF (I.NE.MISSIN .AND. WNDKTS) WIND=WIND*KTS2MPS
*
* THE PERIOD IS EITHER THE LAST 10 MINUTES, OR THE PRESENT WEATHER
* PERIOD, OR THE LAST TT MINUTES (TT AS IN 907.. GROUP) OR THE 10
* MINUTES ENDING TT MINUTES BEFORE THE REPORT TIME (TT AS IN 904..).
* EXPRESS THESE PERIODS AS MINUTES RELATIVE TO THE REPORT TIME.
*
        IF (LASTIND.EQ.907 .AND. TT.NE.RMISS) THEN
          START=-TT
          END=0
        ELSE IF (LASTIND.EQ.904 .AND. TT.NE.RMISS) THEN
          START=-TT-10.
          END=-TT
        ELSE IF (LASTIND.EQ.904 .OR. LASTIND.EQ.907) THEN
          START=RMISS
          END=RMISS
        ELSE
          START=WPER*60.          ! CONVERT HOURS TO MINUTES
          END=0
        ENDIF
        LASTIND=0
*
* AND SET THE APPROPRIATE ELEMENT (GUST OR MEAN WIND)
*
        IF (IND.EQ.910) THEN      ! GUST IN LAST 10 MINUTES
          T1GUST=-10.
          T2GUST=0
          VGUST=WIND
        ELSE IF (IND.EQ.911) THEN ! GUST IN ANY OTHER PERIOD
          T1GUS=START
          T2GUS=END
          VGUS=WIND
        ELSE IF (IND.EQ.912) THEN
          T1MEAN=START
          T2MEAN=END
          VMEAN=WIND
        ENDIF
***********************************************************************
*
* SEA GROUPS (920.., 921.., 924..)
*
***********************************************************************
*
* STATE OF SEA: BUFR TABLE SAME AS WMO SYNOP CODE TABLE.  (IGNORE WIND
* FORCE IN LAST FIGURE, THE ONLY DIFFERENCE BETWEEN 920.. & 921..)
*
      ELSE IF (IND.EQ.920 .OR. IND.EQ.921) THEN
        LSEA=.TRUE.
        SEASTE=IVALUE(GROUP9(IG)(4:4))
*
* STATE OF SEA & VISIBILITY: CONVERT VISIBILITY TO METRES FOR BUFR.
*
      ELSE IF (IND.EQ.924) THEN
        LSEA=.TRUE.
        SEASTE=IVALUE(GROUP9(IG)(4:4))
        I=IVALUE(GROUP9(IG)(5:5))
        IF (I.NE.MISSIN) SEAVIS=VSCONV(I)
***********************************************************************
*
* DENSE FOG GROUP (961..)           SEE W1W1 IN CODE TABLE 4687
! Set value close to midpoint of range (only tens of metres        !1.6
! in BUFR 020001, so can't set 75m etc)                            !1.6
*
***********************************************************************
      ELSE IF (IND.EQ.961) THEN
        I=IVALUE(GROUP9(IG)(4:5)) ! W1W1
        IF (I.EQ.47) VV=80.       ! 60-90M                         !1.6
        IF (I.EQ.48) VV=50.       ! 30-60M                         !1.6
        IF (I.EQ.49) VV=20.       ! <30M                           !1.6
      ENDIF
*
      IG=IG+1
      IF (IG.LE.NINGPS) GOTO 10
      RETURN
      END
