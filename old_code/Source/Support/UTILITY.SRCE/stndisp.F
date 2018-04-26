***********************************************************************
*                                                                     *
* PROGRAM       : STNDISP                                             *
*                                                                     *
* PURPOSE       : FORTRAN PROGRAM LINKED WITH DIALOGUE MANAGER AND    *
*                 CLISTS TO PROVIDE AN ON-LINE INTERACTIVE DISPLAY    *
*                 FOR THE STATION MASTER DATA STORE                   *
*                                                                     *
* DESCRIPTION   : THIS PROGRAM (STARTED BY A CLIST) DISPLAYS PANELS   *
*                 FOR THE USER TO MAKE CHOICES ABOUT THE DATA TO BE   *
*                 RETRIEVED. IT THEN GETS THE REQUIRED DATA AND       *
*                 DISPLAYS IT ON APPROPRIATE PANELS                   *
*                                                                     *
* CALLED BY     : SDBSTND (CLIST)                                     *
*                                                                     *
* CALLS         : DMLINK - TO LINK CODE TO DIALOGUE VARIABLES         *
*                 STNDISPA                                            *
*                 STNTOPAN                                            *
*                                                                     *
* PARAMETERS    : NONE                                                *
*                                                                     *
*Y2K  16.06.1997  STNDISP IS YEAR 2000 COMPLIANT.
*Y2K                      ROUTINE CONTAINS DATE MANAGEMENT.
*                                                                     *
* CHANGE RECORD :                                                     *
*   DATE : MAY 92 FIRST VERSION OF SOFTWARE                           *
*          SEP 95 EXTRA INPUT TO A PANEL AND NEW (WARNING) PANELS     A
*                                                                     *
***********************************************************************
*234567
*
      PROGRAM STNDISP
*
* INCLUDE ALL THE VARIABLE STATEMENTS
*
      INCLUDE(STNINIT)
      INTEGER TODAY(8)
      CHARACTER*26 CTEXT
*
* SET UP LINKS TO DIALOGUE MANAGER
*
      CALL DMLINK
*
* GET CURRENT DATE
*
      CALL DATIM(TODAY)
      IHOUR=TODAY(5)
      IDAY=TODAY(6)
      IMONTH=TODAY(7)
      IYEAR=TODAY(8)-((TODAY(8)/100)*100)
*
      CDENT='      '
*
* DISPLAY PRIMARY OPTION PANELS
*
  100 CMDSTR='DISPLAY PANEL(STN001)'
      LASTRC = ISPEX(50,CMDSTR)
      IF(LASTRC.NE.0)THEN
        GOTO 999
      ENDIF
*
      IF(ACTION.EQ.'D')THEN
         CPANEL='DISPLAY '
      ELSEIF(ACTION.EQ.'B')THEN
         CPANEL='BROWSE  '
      ENDIF
*
      IF(ACTION.EQ.'B')THEN
*
* BROWSE STATION MASTER MAIN DATA STORE
*
        LASTRC = ISPLNK('BROWSE ','''SDB.STNMAS.MAINB'' ')
*
        IF (LASTRC .EQ. 0) GO TO 100
        IF(LASTRC.GT.0)THEN
          PRINT*,'PROBLEM WITH BROWSING DATASET'
          GOTO 100
        ENDIF
      ENDIF
*
* SELECT OPTION DEPENDING ON USER RESPONSE
*
 123  CONTINUE
      VIEWCURR='Y'  ! A
      STNTYPE=' '   ! A
      F15='?'       ! A
      V=' '
      W=' '
      X=' '
      Y=' '
      CMDSTR='DISPLAY PANEL(STN002)'
      LASTRC = ISPEX(50,CMDSTR)
      IF(LASTRC.NE.0)THEN
        GOTO 100
      ENDIF
*
* EVALUATE IDENTIFIER ENTERED AND RETRIEVE DATA
*
      IF(V.EQ.'S')THEN
         IDTYPE=1
      ELSEIF(W.EQ.'S')THEN
         IDTYPE=2
      ELSEIF(X.EQ.'S')THEN
         IDTYPE=3
      ELSEIF(Y.EQ.'S')THEN
         IDTYPE=4
      ELSE
         GOTO 123
      ENDIF
      IF (SUA .EQ. 'U') CURR='C'  ! A
*
      CALL STNDISPA
      IF (ISTAT .EQ. 16) GOTO 123  ! A (ERROR COND WAS ISTAT=2)
*
      IF (IDTYPE .EQ. 1) THEN                        ! A
         IF      (SUA.EQ.'S' .AND. F15.EQ.'2') THEN  ! A
            CMDSTR = 'DISPLAY PANEL(STN013)'         ! A
            LASTRC = ISPEX(50,CMDSTR)                ! A
            IF (LASTRC .NE. 0) GOTO 123              ! A
         ELSE IF (SUA.EQ.'U' .AND. F15.EQ.'1') THEN  ! A
            CMDSTR = 'DISPLAY PANEL(STN014)'         ! A
            LASTRC = ISPEX(50,CMDSTR)                ! A
            IF (LASTRC .NE. 0) GOTO 123              ! A
         END IF                                      ! A
*
         IF (VIEWCURR .EQ. '?') THEN                 ! A
            CMDSTR = 'DISPLAY PANEL(STN012)'         ! A
            LASTRC = ISPEX(50,CMDSTR)                ! A
            IF (LASTRC .NE. 0) GOTO 123              ! A
         END IF                                      ! A
*
         IF      (F15 .EQ. '0') THEN                 ! A
            STNTYPE = 'UNKNOWN'                      ! A
         ELSE IF (F15 .EQ. '1') THEN                 ! A
            STNTYPE = 'SURFACE (SYNOPS)'             ! A
         ELSE IF (F15 .EQ. '2') THEN                 ! A
            STNTYPE = 'UPPER AIR'                    ! A
         ELSE IF (F15 .EQ. '3') THEN                 ! A
            STNTYPE = 'SURFACE AND UPPER AIR'        ! A
         END IF                                      ! A
      ELSE IF (IDTYPE .EQ. 2) THEN                   ! A
         STNTYPE = 'SURFACE (METARS)'                ! A
      ELSE IF (IDTYPE .EQ. 3) THEN                   ! A
         STNTYPE = 'CLIMATOLOGICAL'                  ! A
      ELSE                                           ! A
         STNTYPE = 'RAINFALL'                        ! A
      END IF                                         ! A
*
* COPY ARRAYS TO SINGLE VARIABLES FOR DIALOGUE OUTPUT
*
      IF((ISTAT.LE.4).AND.(ACTION.EQ.'D'))THEN
*
         CALL STNTOPAN
*
* 'DISPLAY' - DISPLAY APPROPRIATE DATA PANELS
*
         IF(ACTION.EQ.'D')THEN
            CMDSTR='DISPLAY PANEL(STN003)'
            LASTRC = ISPEX(50,CMDSTR)
            IF(LASTRC.NE.0)THEN
              GOTO 123
            ENDIF
            CMDSTR='DISPLAY PANEL(STN004)'
            LASTRC = ISPEX(50,CMDSTR)
            IF(LASTRC.NE.0)THEN
              GOTO 123
            ENDIF
            CMDSTR='DISPLAY PANEL(STN005)'
            LASTRC = ISPEX(50,CMDSTR)
            IF(LASTRC.NE.0)THEN
              GOTO 123
            ENDIF
            CMDSTR='DISPLAY PANEL(STN006)'
            LASTRC = ISPEX(50,CMDSTR)
            IF(LASTRC.NE.0)THEN
              GOTO 123
            ENDIF
         ENDIF
      ELSE
         CMDSTR='DISPLAY PANEL(STN007)'
         LASTRC = ISPEX(50,CMDSTR)
         GOTO 123
      ENDIF
*
      IF(LASTRC.EQ.0) GOTO 123
*
 999  STOP
      END
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
      SUBROUTINE DMLINK
*
      INCLUDE(STNINIT)
*
* LINK ISPF PANEL VARIABLES TO FORTRAN VARIABLES
*
      LASTRC=ISPLNK('VDEFINE','ACTION ',ACTION,'CHAR ',1)
      LASTRC=ISPLNK('VDEFINE','A ',A,'CHAR ',1)
      LASTRC=ISPLNK('VDEFINE','CDENT ',CDENT,'CHAR ',6)
      LASTRC=ISPLNK('VDEFINE','STNTYPE ',STNTYPE,'CHAR ',21)
      LASTRC=ISPLNK('VDEFINE','SUA ',SUA,'CHAR ',1)
      LASTRC=ISPLNK('VDEFINE','CURR ',CURR,'CHAR ',1)
      LASTRC=ISPLNK('VDEFINE','WMOCHA ',WMOCHA,'CHAR ',5)
      LASTRC=ISPLNK('VDEFINE','ICAOCHA ',ICAOCHA,'CHAR ',4)
      LASTRC=ISPLNK('VDEFINE','DCNNCHA ',DCNNCHA,'CHAR ',4)
      LASTRC=ISPLNK('VDEFINE','RAINCHA ',RAINCHA,'CHAR',6)
      LASTRC=ISPLNK('VDEFINE','LAT1 ',LAT1,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','LAT2 ',LAT2,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','LATP ',LATP,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','LONG1 ',LONG1,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','LONG2 ',LONG2,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','LONGP ',LONGP,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','HT1CHA ',HT1CHA,'CHAR',4)
      LASTRC=ISPLNK('VDEFINE','HT2CHA ',HT2CHA,'CHAR',4)
      LASTRC=ISPLNK('VDEFINE','STATION ',STATION,'CHAR',42)
      LASTRC=ISPLNK('VDEFINE','COUNTRY ',COUNTRY,'CHAR',32)
      LASTRC=ISPLNK('VDEFINE','REGION ',REGION,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','OYEAR ',OYEAR,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','OMONTH ',OMONTH,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','ODAY ',ODAY,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','OHOUR ',OHOUR,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','CYEAR ',CYEAR,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','CMONTH ',CMONTH,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','CDAY ',CDAY,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','CHOUR ',CHOUR,'FIXED',4)
*
      LASTRC=ISPLNK('VDEFINE','F1 ',F1,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F2 ',F2,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F3 ',F3,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F4 ',F4,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F5 ',F5,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F6 ',F6,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F7 ',F7,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F8 ',F8,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F9 ',F9,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F10 ',F10,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F11 ',F11,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F12 ',F12,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F13 ',F13,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F14 ',F14,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F15 ',F15,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F16 ',F16,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F17 ',F17,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F18 ',F18,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F19 ',F19,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F20 ',F20,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F21 ',F21,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F22 ',F22,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F23 ',F23,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','F24 ',F24,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','CLASS ',CLASS,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','HAW00 ',HAW00,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','HAW12 ',HAW12,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','MAP ',MAP,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','RUN1 ',RUN1,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','RUN2 ',RUN2,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','OREC ',OREC,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','NREC ',NREC,'FIXED',4)
*
      LASTRC=ISPLNK('VDEFINE','UA ',UA,'CHAR',4)
      LASTRC=ISPLNK('VDEFINE','SYN ',SYN,'CHAR',13)
      LASTRC=ISPLNK('VDEFINE','NCM ',NCM,'CHAR',23)
      LASTRC=ISPLNK('VDEFINE','NUM ',NUM,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','DAY1 ',DAY1,'CHAR',9)
      LASTRC=ISPLNK('VDEFINE','DAY2 ',DAY2,'CHAR',9)
      LASTRC=ISPLNK('VDEFINE','DAY3 ',DAY3,'CHAR',9)
      LASTRC=ISPLNK('VDEFINE','DAY4 ',DAY4,'CHAR',9)
      LASTRC=ISPLNK('VDEFINE','DAY5 ',DAY5,'CHAR',9)
      LASTRC=ISPLNK('VDEFINE','DAY6 ',DAY6,'CHAR',9)
      LASTRC=ISPLNK('VDEFINE','DAY7 ',DAY7,'CHAR',9)
      LASTRC=ISPLNK('VDEFINE','DAY8 ',DAY8,'CHAR',9)
      LASTRC=ISPLNK('VDEFINE','DAY9 ',DAY9,'CHAR',9)
      LASTRC=ISPLNK('VDEFINE','DAY10 ',DAY10,'CHAR',9)
      LASTRC=ISPLNK('VDEFINE','SYNTIM1 ',SYNTIM1,'CHAR',24)
      LASTRC=ISPLNK('VDEFINE','SYNTIM2 ',SYNTIM2,'CHAR',24)
      LASTRC=ISPLNK('VDEFINE','SYNTIM3 ',SYNTIM3,'CHAR',24)
      LASTRC=ISPLNK('VDEFINE','SYNTIM4 ',SYNTIM4,'CHAR',24)
      LASTRC=ISPLNK('VDEFINE','SYNTIM5 ',SYNTIM5,'CHAR',24)
      LASTRC=ISPLNK('VDEFINE','SYNTIM6 ',SYNTIM6,'CHAR',24)
      LASTRC=ISPLNK('VDEFINE','SYNTIM7 ',SYNTIM7,'CHAR',24)
      LASTRC=ISPLNK('VDEFINE','SYNTIM8 ',SYNTIM8,'CHAR',24)
      LASTRC=ISPLNK('VDEFINE','SYNTIM9 ',SYNTIM9,'CHAR',24)
      LASTRC=ISPLNK('VDEFINE','SYNTIM10 ',SYNTIM10,'CHAR',24)
      LASTRC=ISPLNK('VDEFINE','NCMTIM1A ',NCMTIM1A,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM1B ',NCMTIM1B,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM1C ',NCMTIM1C,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM1D ',NCMTIM1D,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM1E ',NCMTIM1E,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM1F ',NCMTIM1F,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM1G ',NCMTIM1G,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM1H ',NCMTIM1H,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM1I ',NCMTIM1I,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM1J ',NCMTIM1J,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM2A ',NCMTIM2A,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM2B ',NCMTIM2B,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM2C ',NCMTIM2C,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM2D ',NCMTIM2D,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM2E ',NCMTIM2E,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM2F ',NCMTIM2F,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM2G ',NCMTIM2G,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM2H ',NCMTIM2H,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM2I ',NCMTIM2I,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','NCMTIM2J ',NCMTIM2J,'CHAR',3)
      LASTRC=ISPLNK('VDEFINE','SQUARE ',SQUARE,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','CHASER ',CHASER,'CHAR',6)
      LASTRC=ISPLNK('VDEFINE','RECNO ',RECNO,'FIXED',4)
      LASTRC=ISPLNK('VDEFINE','V ',V,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','W ',W,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','X ',X,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','Y ',Y,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','UP ',UP,'CHAR',1)
      LASTRC=ISPLNK('VDEFINE','SOURCE ',SOURCE,'CHAR',20)
      LASTRC=ISPLNK('VDEFINE','AUSER ',AUSER,'CHAR',20)
      LASTRC=ISPLNK('VDEFINE','COYEAR ',COYEAR,'CHAR',2)
      LASTRC=ISPLNK('VDEFINE','COMONTH ',COMONTH,'CHAR',2)
      LASTRC=ISPLNK('VDEFINE','CODAY ',CODAY,'CHAR',2)
      LASTRC=ISPLNK('VDEFINE','COHOUR ',COHOUR,'CHAR',2)
      LASTRC=ISPLNK('VDEFINE','CCYEAR ',CCYEAR,'CHAR',2)
      LASTRC=ISPLNK('VDEFINE','CCMONTH ',CCMONTH,'CHAR',2)
      LASTRC=ISPLNK('VDEFINE','CCDAY ',CCDAY,'CHAR',2)
      LASTRC=ISPLNK('VDEFINE','CCHOUR ',CCHOUR,'CHAR',2)
      LASTRC=ISPLNK('VDEFINE','CPANEL ',CPANEL,'CHAR',8)
*
      RETURN
      END
