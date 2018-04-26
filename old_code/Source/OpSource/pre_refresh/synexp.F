      SUBROUTINE SYNEXP(IDENT,REPORT,REPLEN,FREXP,YYGGIW,IDATIM,LATLON,
     &                  MIMJ,SYNTAX,NELM)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNEXP
!
! PURPOSE       : SYNOP EXPANSION PROGRAM TO PUT A SINGLE REPORT INTO
!                 AN ARRAY OF REAL ELEMENTS READY FOR BUFR ENCODING.
!
! DESCRIPTION   :
!  1. THE MANDATORY GROUPS IN SECTIONS 0 & 1 OF A SYNOP ARE EXPANDED
!     FIRST. IF THIS WORKS, OTHER SECTIONS ARE LOOKED FOR, INCLUDING
!     SECTION 2 AS SOME COASTAL STATIONS USE IT. SECTION GROUP COUNTS
!     & STARTS ARE KEPT & USED TO EXPAND A REPORT A SECTION AT A TIME.
!     SYNOPS USE 5-FIG GROUPS EXCEPT FOR SECTION MARKERS 333/444/555.
!     MTRLOC IS USED TO INCREMENT THE POINTER THROUGH THE REPORT.
!  2. TIME OF REPORT AND THE YYGGIW GROUP FROM THE BULLETIN HEADING
!     ARE USED IF AVAILABLE, BUT A 9-GROUP TIME WILL TAKE PRECEDENCE.
!  3. MARINE AND RADIATION SECTIONS ARE OPTIONAL FOR LAND SYNOPS: A
!     ZERO REPLICATION COUNT WILL SKIP THOSE ELEMENTS IN BUFR ENCODING
!  4. SECTIONS 0 TO 5 :  SECTION 0 STATION - MANDATORY
!                     :  SECTION 1 TWO MANDATORY GROUPS + MAIN INFO
!                     :  SECTION 2 SHIP/COASTAL SECTION
!                     :  SECTION 3 REGIONALLY DEFINED SECTION
!                     :  SECTION 4 MOUNTAIN CLOUD SECTION (NATIONAL)
!                     :  SECTION 5 NATIONAL USE ONLY
!
!  NOTE: ON ENTRY THE FIRST GROUP SHOULD BE A 5 FIGURE BLOCK/STATION
!        NUMBER (03 INSERTED FOR 3-FIG BRITISH STATIONS BY SYNBUL) FOR
!        A LAND SYNOP; SHIPS & MOBILE SYNOPS START WITH SECTION 1.   !2
!  NOTE: THE ELEMENT VARIABLE NAMES CORRESPOND AS MUCH AS POSSIBLE
!        TO THOSE USED IN THE SYNOP CODE DEFINITION.
!  NOTE: THE ELEMENTS IN THE MAIN DATA LIST ARE DEFINED BY AN INCLUDE.
!        SYNIT GIVES THE COMPLETE LIST OF VARIABLES WITH FULL NAMES.
!  NOTE: THE EXPANSION ARRAY REXP HAS FIXED DISPLACEMENTS AND MISSING
!        DATA FOR ELEMENTS NOT REPORTED (MARINE/RADIATION).
!        FREXP IS EDITED FOR DELAYED REPLICATIONS & MISSING SECTIONS
!        SO THAT IT IS READY TO PASS TO THE BUFR ENCODING.
!
! DATA TYPE(S)  : SYNOPS  (WMO CODE FM 12, 13 AND 14)                !2
!
! CALLED BY     : SYNOB
!
! CALLS         : SYNIT (INITIALISE), MTRLOC (DELIMIT GROUP),
!                 SYNREG (WORK OUT WMO REGION FROM WMO BLK AND STN.),
!                 FFUNIT (WIND UNITS), STAPOS, SYNRPR (PERIOD),
!                 SYNXP1, SYNXP2, SYNXP3, SYNXP4, SYNXP5 (SECTIONS),
!                 SYNQC (QC: INTERNAL CONSISTENCY), SYNSEQ OR SHPSEQ,
!                 ONLAND, DATE31/DATE13
!
! PARAMETERS    : (1)IDENT   CHAR    STATION NUMBER OR CALL SIGN  (I)
!                 (2)REPORT  CHAR    SINGLE REPORT                (I)
!                 (3)REPLEN  INTEGER REPORT LENGTH                (I)
!                 (4)FREXP   REAL    ARRAY OF VALUES              (O)
!                 (5)YYGGIW  CHAR    FROM BULLETIN HEADING        (I)
!                 (6)IDATIM  INTEGER ARRAY YY,MM,DD,HH,MIN        (I)
!                 (7)LATLON  REAL    ARRAY LAT,LON  (INPUT FOR SHIPS)
!                 (8)MIMJ    CHAR    TO DECIDE IF LAND OR SHIP    (I)
!                 (9)SYNTAX  LOGICAL TRUE IF SYNTAX ERROR FOUND   (O)
!                (10)NELM    INTEGER NUMBER OF GOOD ELEMENTS      (O)
!                                    (NELM=0 TO REJECT REPORT)
!
! REVISION INFO :
!
! $Workfile: synexp.F$ $Folder: pre_refresh$
! $Revision: 3$ $Date: 24/04/2008 11:55:29$
!
! CHANGE RECORD :
!
! $Log:
!  3    Met_DB_Project 1.2         24/04/2008 11:55:29    Brian Barwell
!       Revised algorithm for setting observation time.
!  2    Met_DB_Project 1.1         22/04/2008 12:00:45    Brian Barwell
!       Modified to handle mobile SYNOPs. Revision information and comment
!       characters updated.
!  1    Met_DB_Project 1.0         30/01/2006 20:24:49    Sheila Needham  
! $
! Revision 2.5  2002/12/02  14:30:52  14:30:52  usmdb (Generic MetDB account)
! Change day 16DEC02    R Hirst
! Call SYNXP1 with an additional argument IDATIM.
!
! Revision 2.4  2002/08/07  09:14:45  09:14:45  usmdb (Generic MetDB account)
! 19th August 2002. Kudsia Gwangwaa.
! Modified function OUKSTN to include list of identifiers and
! modified SYNEXP to call section 5 for these stations.
!
! Revision 2.3  2001/11/07  10:55:59  10:55:59  usmdb (Generic MetDB account)
! No longer set lats, longs and heights of WMO pseudo block 99
! stations (currently all CDL stations) to missing.
! Change day 19NOV01   R HIRST
!
! Revision 2.2  2001/09/05  09:01:11  09:01:11  usmdb (Generic MetDB account)
! Check added to prevent out of bounds error on HP - S.Cox
!
! Revision 2.1  2001/08/09  09:27:26  09:27:26  usmdb (Generic MetDB account)
! 20/8/2001, !2.1
!       Allow for 00 group even if wind speed below 100 kts.
!               Stan kellett.
!
! Revision 2.0  2001/07/03  10:44:03  10:44:03  usmdb (Generic MetDB account)
! Changed preprocessor statement to be if (1 ), else...
! Separates variable declaration and initialisation. Removed unused
! variables. Removed variable VV from call to SYNXP5 as not used in
! SYNXP5. Added copyright and modified header - S.Cox
!
! Revision 1.26  2001/05/09  09:38:26  09:38:26  usmdb (Generic MetDB account)
! 21 May 2001 : New argument addend to call to SYNQC called MSG. This
! is a diagnostic switch to print out messages from SYNQC. Set to
! .FALSE. within SYNEXP - Stan Kellett
!
! Revision 1.25  2001/03/09  13:12:04  13:12:04  usmdb (Generic MetDB account)
! Change to allow storage of SAMOS NT SYNOPS that use section 1 group 9
! in confusing way.
!
! Revision 1.24  2001/03/07 11:15:53  usmdb
! !1.24  19th March 2001; Stop 333// being treated as the start of
!        Section 3 for SHPSYN reports. Stan
! !1.24a Modifications to time check to allow 10 minute SYNOPs to
!        be stored with correct time. Hodge
!
! Revision 1.23  2000/10/04  15:48:41  15:48:41  usmdb (Generic MetDB account)
! 16 Oct 2000 Additional output where station details are not
!             found.  Hodge
!
! Revision 1.22  2000/08/09  15:08:39  15:08:39  usmdb (Generic MDB account)
! 21/8/2000, !1.22, changed call to SYNRPR adding more
!             arguments. Stan Kellett
!
! Revision 1.21  2000/07/10  11:11:48  11:11:48  usmdb (Generic MDB account)
! 17 Jul 2000, 1.21, Fix bug to create a full WMO number out of
!              Block No and Station Number. Stan Kellett.
!
! Revision 1.20  2000/06/08  15:20:45  15:20:45  usmdb (Generic MDB account)
!
! 9/5/2000 For calculating Automatic clouds, function
! added to check if an overseas UK station.
!
! Revision 1.19  2000/04/07  09:25:18  09:25:18  usmdb (Generic MDB account)
! 17 April 2000      C Long
! 1.19  Set visibilities for VV=90 & VV=91 to avoid confusion
!       with other visibility scales (30m for 90, 60m for 91)
!
! Revision 1.18  2000/03/10  09:54:19  09:54:19  usmdb (Generic MDB account)
! 20 March 2000     C Long
! 1.18  Set visibility <100m from group at start to 90m for all stations
!       (UK obs should reset it with 961.. group)
!
! Revision 1.17  99/03/11  14:38:44  14:38:44  usmdb (Generic MDB account)
! dummy revision due to problem with checkin
!
! Revision 1.16  99/03/11  13:29:32  13:29:32  usmdb (Generic MDB account)
! 15th March 1999. Handle reports that have 333// for section 3. This
! applies mainly to blocks 50 -59. Jon Letwhwaite
!
! Revision 1.15  99/02/11  11:53:52  11:53:52  usmdb (Generic MDB account)
! 15th February 1999 John Norton
! Stop calling STAPOS when a station with WMO block 99
! (CDL Report) is expanded.
!
! Revision 1.14  98/10/15  11:07:30  11:07:30  usmdb (Generic MDB account)
! 1.14 Use IX (not WXMAN) to set manual/automatic flag
!
! Revision 1.13  98/09/08  17:02:48  17:02:48  usmdb (Generic MDB account)
! 09 September 1998 Pass Weather period to SYNQC for past weather test.
! John Norton
!
! Revision 1.12  98/07/23  08:02:47  08:02:47  usmdb (Generic MDB account)
! Modify change l above to set time to bull time within 10 minutes
! of bull time otherwise use report time if reported. J Norton.       !P
!
! Revision 1.11  98/04/20  06:58:50  06:58:50  usmdb (Generic MDB account)
! Previous change not required - error was in following IF THEN
! statement. Pointer could look beyond end of report                  !O
!
! Revision 1.10  98/03/11  10:34:53  10:34:53  usmdb (Generic MDB account)
! Add check for MTRLOC call to see if have reached end of report
! i.e. flag LREPFL is set to false. Hodge                             !N
!
! Revision 1.9  98/02/04  15:53:12  15:53:12  usmdb (Generic MDB account)
! Add output of both heights returned from STAPOS. New value STNHTP
! added as item 6 in REXP & METELM arrays. Subsequent values have
! been moved on 1 place in array. J.Norton.                           !M
!
! Revision 1.8  1998/01/29 11:32:12  usmdb
! Addition of IBM preprocess directive
!
! Revision 1.7  98/01/27  10:17:56  10:17:56  usmdb (Generic MDB account)
! Set report and index times to same time. Reports with a report
! time within 50 mins of bulletin time will be stored using report
! time. Otherwise bulletin time will be used.                         !L
!
! Revision 1.6  1997/11/18 12:16:55  usjl
! Change handling of date and times when a report has time
! reported in a '9' group. J.Norton                                   !K
! Ship sequence checks not performed and values flagged if latitude
! or longitude missing. J.Norton                                      !J
!
! Revision 1.5  1997/11/04 14:45:22  usjl
! Add checks on reports with only two groups in section 1. J.Norton.  !I
!
! Revision 1.4  1997/10/24 13:12:10  usjl
! Add additional calls to STAPOS, so that the lat,lon and height are
! always extracted if the station exists in the abbreviated
! list - S.Cox                                                        !H
!
! Revision 1.3  1997/08/28 10:41:15  usjl
! Pass REPLEN to SYNXP2                                               !G
!
! Revision 1.2  1997/07/31 11:37:46  uspm
! First revision for COSMOS
!
! Revision 1.1  1997/07/04 13:42:31  uspm
! Initial revision
!
! 14-05-97  !F  : Include wmo block 88 (Falklands) in national section
!               : check
!
! 04-02-97      : Uncomment calls from previous change
!
! 19-11-96  !E  : Pass ix value to SYNXP1 for use in determining ww
!
! 19-11-96  !D  : Use 60 minute gust in preference to 10 minute gust
!
! 20-08-96  !C  : Fix mountain cloud at 2 groups (or none)
!
! 13-08-96  !B  : Set number of cloud groups to 4 if more than 4 found
!
! 11-08-96  !A  : Set DD=990 (variable) to zero (once done by SYNQC)
!               : to avoid "set to missing" message from encode
!
! 18-02-96      : Call SYNSEQ & ONLAND & pass syntax flag to SYNQC
!
! 23-11-94      : Extend land synop version to cope with ships too
!               : & count met elements
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

#if defined (MVS)
      INCLUDE (SYNELM)  ! FOR ELEMENT VARIABLES
      INCLUDE (SHPELM)  ! FOR EXTRA SHIP ELEMENTS
      INCLUDE (SYNCOM)  ! FOR CONSTANTS
#else
      INCLUDE 'SYNELM'  ! FOR ELEMENT VARIABLES
      INCLUDE 'SHPELM'  ! FOR EXTRA SHIP ELEMENTS
      INCLUDE 'SYNCOM'  ! FOR CONSTANTS
#endif

      INTEGER IR,IX                                                 !2.0
      INTEGER IDATIM(5),CENDAY,GRPCNT,NELEM                         !2.0
      CHARACTER IDENT*(*),REPORT*(*),YYGGIW*5,MIMJ*4
      REAL REXP(2,150)  ! ARRAY USED TO LOAD DATA INTO.
      REAL FREXP(*)              ! COPY OF ABOVE ARRAY,MINUS ASSOCIATED
                                 ! DATA FOR REPLICATION COUNT
      REAL LATLON(4),PSEQ,STNHTP,LLFLAG
      INTEGER ISWELL,ICL,IHOUR                                      !2.0
      INTEGER I,J,IVALUE,IRC,L,LGROUP                               !2.0
      LOGICAL LREPFL,SYNTAX,BADGRP,WNDKTS,IWREP,WXMAN
      LOGICAL LSEA,LRAD,LAUTO,SHIP, ONLAND
      LOGICAL ERR          ! TRUE IF DATA VALUE ERROR                !i
      LOGICAL OUKSTN       !1.20 Function to check if UK oversea STn
      LOGICAL DATVAL       ! TRUE IF NO DATA VALUES FOUND            !i
      LOGICAL FIRST        ! TRUE if first call to subroutine        !2

      LOGICAL MOBILE    !Flag set to .TRUE. if mobile SYNOP          !2
      LOGICAL UKSHIP    !Flag set to .TRUE. if a UK ship            !2.4
      INTEGER INSHIP    !Ship identifier                            !2.4

      INTEGER REPLEN,GRPLEN,POINT1,POINT2,POINT,NGRP,NSECT,IMT
      INTEGER SECNO,SECPTR(5),SECGPS(5)  ! SECTION STARTS & GROUP COUNTS
      INTEGER IWMOB,IWMOS,IWMOR
      INTEGER NGRPS,NELM                                            !2.0
      REAL VISCONV(90:99)                                           !2.0
      REAL HLCONV(0:9)                                              !2.0
      LOGICAL MSG          !1.26 Flag passed to SYNQC for diagnostics

! THE FOLLOWING ARRAY IS USED TO COUNT METEOROLOGICALLY USEFUL DATA.
! IT REPRESENTS THE FIRST 109 ELEMENTS OF THE ARRAY REXP (50 ON FIRST
! LINE, 59 ON SECOND) WITH 1 SET IF THE ELEMENT IS TO BE COUNTED, 0 IF
! IT IS FIXED, INSTRUMENTAL OR COORDINATE INFORMATION.

      CHARACTER METELM*109                                           !m
      CHARACTER*80 CHEAD                                             !2

      DATA  VISCONV /30.,60.,200.,500.,1000.,2000.,4000.,10000.,
     &               20000.,50000./                                 !2.0
      DATA  HLCONV /25.,50.,100.,200.,300.,600.,1000.,1500.,
     &              2000.,2500./                                    !2.0

      DATA  METELM  /'00000000000000111111110111010110101100100111011111
     &11101011111111111111101100010100011011111111111111010101011'/  !m

      DATA  FIRST /.TRUE./                                           !2

      IF (FIRST) THEN                                                !2
        CHEAD = '$Workfile: synexp.F$ ' //
     &          '$Revision: 3$ $Date: 24/04/2008 11:55:29$'
        FIRST = .FALSE.                                              !2
      END IF                                                         !2

! INITIALISE ALL THE TEMPORARY VARIABLES HOLDING ELEMENT VALUES

      CALL SYNIT(
     +        WMOBLK,WMOSTA,WMOREG,LAT,LON,STNHT,STNTYP,
     +        DD,FF,TTT,TDTDTD,UUU,VV,VERTV,WW,WPER,
     +        W1,W2,NT,TPER1,
     +        MAXTTT,TPER2,MINTTT,E,TR,RRR,TR3,RRR3,R24,
     +        T1GUST,T2GUST,VGUST,T1MEAN,T2MEAN,VMEAN,PPPP,
     +        P0P0P0,A3,HHH,SSS,TGTG,ATEND,PCHANG,PCHG24,
     +        IE,EEE,SUN2,SUN1,RNET,GLSO,DISO,LW,SW,SWNET,SDIR,
     +        RNET24,GLSO24,DISO24,LW24,SW24,SWNET24,SDIR24,
     +        SEAVIS,SEASTE,TWTYPE,TWTWTW,
     +        DS,VS,TBTYPE,TBTBTB, WVMS,WVHT,WVPER,SWELWV,
     +        ISICE,ESES,RS,CI,SI,BI,DI,ZI,
     +        CL,CM,CH,NH,HL,VSIF,GROUP8,MTCL,IW,HOUR,MINS,
     +        T1GUS,T2GUS,VGUS)                                    !D

        STNHTP=RMISS

      DO I=1,150     ! ARRAY WITH FIXED SLOTS FOR Q/C
       DO J=1,2
        REXP(J,I)=RMISS
       ENDDO
      ENDDO
!                    ! SECTION POINTERS & GROUP COUNTS
      DO J=1,5
       SECPTR(J)=0
       SECGPS(J)=0
      ENDDO

      SHIP=MIMJ.EQ.'BBXX'
      MOBILE=MIMJ.EQ.'OOXX'                                          !2
      LREPFL=.FALSE. ! END OF REPORT FOUND FLAG
      LRAD=.FALSE.   ! RADIATION SECTION USED FLAG.
      LSEA=.FALSE.   ! COASTAL DATA FLAG.
      LAUTO=.FALSE.  ! AUTO OR SEMIAUTO STATION FLAG
      WXMAN=.TRUE.   ! DEFAULT WEATHER CODES USED ARE 'MANNED'
      SYNTAX=.FALSE.

      UKSHIP=.FALSE.  ! UK ship flag                               !2.4
      INSHIP=-9999999 ! Numeric ship identifier set to missing     !2.4

      NELM=0         ! RETURN WITH NELM=0 TO REJECT REPORT

      NGRP=0         ! NUMBER OF GROUPS FOUND IN REPORT.
      NSECT=0        ! NUMBER OF SYNOP SECTIONS FOUND
      NELEM=4        ! NUMBER OF ELEMENTS REQUESTED FROM STATIONMASTER
      GRPCNT=0       ! NO. OF CURRENT GROUP
      ISWELL=0       ! SWELL WAVE GROUP COUNT, SECTION 2.
      ICL=0          ! COUNT OF CLOUD GROUPS
      IMT=0          ! COUNT OF MOUNTAIN CLOUD GROUPS

      POINT2=1       ! POINT TO FIRST GROUP
!*********************************************************************
! GET WIND REPORTING UNITS FROM YYGGIW.
! IF IW IS MISSING, UNITS WILL BE FOUND BY WMO BLOCK NUMBER LATER.
! CONVERT SYNOP CODE TO BUFR CODE:
! IW SYNOP CODE TBL 1855, BUFR FLAG TBL 2002
! SET WNDKTS IF WIND SPEEDS NEED TO BE CONVERTED TO M/S
!*********************************************************************
      IWREP=.FALSE.
      IF(YYGGIW.NE.' ')THEN
        I=IVALUE(YYGGIW(5:5))
        IF(I.NE.MISSIN)THEN
          IF(I.GE.3)THEN
            WNDKTS=.TRUE.
            IF(I.EQ.3)IW=4.
            IF(I.EQ.4)IW=12.
          ELSE
            WNDKTS=.FALSE.
            IF(I.EQ.0)IW=0.
            IF(I.EQ.1)IW=8.
          ENDIF
          IWREP=.TRUE.
        ENDIF
      ENDIF
!********************************************************************
!
! EXPAND SECTION 0 (JUST THE STATION NUMBER) IF IT IS A LAND SYNOP
!
!********************************************************************

! KEEP POINT1 POINTING TO THE FIRST GROUP & MOVE POINT2 TO THE NEXT.
! MTRLOC RETURNS THE LENGTH OF THE FIRST GROUP, SETTING LREPFL IF
! THERE ARE NO MORE GROUPS. (WHEN 5-FIGURE GROUPS HAVE BEEN COUNTED
! THERE IS NO NEED FOR MTRLOC; USE POINT INSTEAD OF POINT1 & POINT2.)

      IF (.NOT.SHIP .AND. .NOT.MOBILE) THEN                          !2
        POINT1=POINT2
        CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
        IF (GRPLEN.NE.5 .OR. LREPFL) RETURN

! ASSUME STATION ID IS THE FIRST GROUP

        IWMOB=IVALUE(REPORT(POINT1:POINT1+1))
        IWMOS=IVALUE(REPORT(POINT1+2:POINT1+4))
        CALL SYNREG(IWMOB,IWMOS,IWMOR)
        WMOBLK=IWMOB
        WMOSTA=IWMOS
        WMOREG=IWMOR

! SET DEFAULT WIND REPORTING UNITS ACCORDING TO BLOCK AND STN NUMBER

        IF(.NOT.IWREP)THEN
          CALL FFUNIT(IWMOB,IWMOS,I)
          IF(I.EQ.3.OR.I.EQ.4)THEN
            WNDKTS=.TRUE.
          ELSE
            WNDKTS=.FALSE.
          ENDIF
        ENDIF

!-----------------------------------------------------------------------
! Get station latitude, longitude, height from the abbreviated station
! list. Preference is to get the surface station details 'S'.
!-----------------------------------------------------------------------

        CALL STAPOS(IWMOB*1000+IWMOS,'S',LAT,LON,STNHTP,STNHT,IRC)

        IF (LAT.EQ.RMISS .AND. LON.EQ.RMISS) THEN                     !H
          IRC=0                                                       !H
          CALL STAPOS(IWMOB*1000+IWMOS,'X',LAT,LON,STNHTP,STNHT,IRC)  !H
        ENDIF                                                         !H

        IF (LAT.EQ.RMISS .AND. LON.EQ.RMISS) THEN                     !H
          IRC=0                                                       !H
          CALL STAPOS(IWMOB*1000+IWMOS,'U',LAT,LON,STNHTP,STNHT,IRC)  !H
        ENDIF                                                         !H

! If still no station details output error message                 !1.23

        IF (LAT.EQ.RMISS .AND. LON.EQ.RMISS) THEN                  !1.23
          WRITE(6,*) 'NO STATION DETAILS- ',IWMOB*1000+IWMOS       !1.23
        ENDIF                                                      !1.23
!***********************************************************************
! Note. The next line of code affects how station heights are set in  !m
! station height (STNHT). Users requested that old coding practice    !m
! for station height be retained when pressure sensor height (STNHTP) !m
! was additionally made available on retrieval. When they are ready   !m
! to use separate values remove or comment out the line.              !m
!***********************************************************************
        IF (STNHTP.NE.RMISS.AND.STNHT.EQ.RMISS) STNHT=STNHTP
        LATLON(1)=LAT
        LATLON(2)=LON           ! PUT LAND LAT/LONG IN ARRAY FOR INDEX
      ELSE                      ! IF SHIP, LAT/LONG ALREADY FOUND

! For mobile SYNOPs, skip Marsden square group and get station height.

        IF (MOBILE) THEN                                             !2
          CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)            !2
          POINT1 = POINT2                                            !2
          CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)            !2
          I = IVALUE(REPORT(POINT1+4:POINT1+4))                      !2
          IF (I.GE.1 .AND. I.LE.8) THEN   ! Good unit indicator      !2
            STNHT = IVALUE(REPORT(POINT1:POINT1+3))                  !2
            IF (I.GT.4) THEN  ! Height in feet                       !2
              STNHT = STNHT*0.3048                                   !2
            END IF                                                   !2
          END IF                                                     !2
        END IF

        LAT=LATLON(1)
        LON=LATLON(2)

!-----------------------------------------------------------------------
!Recognise UK ships to enable wind gust data obtained from
!section 5 from lightships and moored buoys, to be decoded and stored.
!First check if it is a ship whose identifier starts with '6' (UK)
!-----------------------------------------------------------------------

        IF(IDENT(1:1) .EQ. '6' ) THEN                            !2.4

!-----------------------------------------------------------------------
!2.4 Attempt to convert the first 5 characters of the ship identifier   )
!2.4  (IDENT) to an integer using function IVALUE.
!2.4 IVALUE will return -9999999 if non-digits are found.
!-----------------------------------------------------------------------

          INSHIP = IVALUE(IDENT(1:5))                            !2.4

!2.4 Check if the ship is a UK ship if valid value for INSHIP

          IF (INSHIP.GT.0) UKSHIP = OUKSTN(INSHIP)              !2.4

        ENDIF                                                   !2.4
      ENDIF

!***********************************************************************
! FIND STARTS OF OTHER SECTIONS AND COUNT GROUPS IN EACH SECTION.
! SECPTR & SECGPS ARE START & GROUP COUNT FOR THE SECTIONS FOUND.
! SECGPS=0 FOR MISSING SECTIONS.  COUNT DOES NOT INCLUDE NNN-GROUP
! EXCEPT FOR 222..; AND COUNT FOR 222 SECTION MAY INCLUDE 'ICE'.
!  CHECK THAT THE SECTION HAS NOT BEEN FOUND BEFORE - ESPECIALLY
! IMPORTANT FOR SHIPS'' SECTION 2, BECAUSE 222.. MAY OCCUR LATER!
!********************************************************************

      SECPTR(1)=POINT2           ! START OF SECTION 1
      SECNO=1                    ! HANDLING SECTION 1
      NGRP=0
      BADGRP=.FALSE.             ! WILL BE SET IF GROUP NOT 5 FIGURES

20    CONTINUE                   ! BACK TO HERE IN LOOP ROUND GROUPS

      POINT1=POINT2              ! START OF NEXT GROUP

      CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)

! IF THE GROUP LENGTH IS 3                                          !o

      IF (GRPLEN.EQ.3) THEN                                         !o

! CHECK FOR GLOBAL EXCHANGE SECTION, SECTION 3

        IF (REPORT(POINT1:POINT1+2).EQ.'333'                        !o
     &     .AND. SECGPS(3).EQ.0) THEN                               !o
          SECGPS(SECNO)=NGRP     ! GROUP COUNT FOR PREVIOUS SECTION !o
          SECNO=3                ! HANDLING SECTION 3               !o
          NGRP=0                 ! RESET GROUP COUNT TO ZERO        !o
          SECPTR(3)=POINT2       ! START OF FIRST 5-FIGURE GROUP    !o
          BADGRP=.FALSE.       ! RESET FLAG FOR NON-5-FIGURE GROUPS !o

!   CHECK FOR REGIONAL EXCHANGE SECTION, SECTION 4, MOUNTAIN CLOUD DATA.

        ELSE IF (REPORT(POINT1:POINT1+2)                            !o
     -  .EQ.'444'.AND. SECGPS(4).EQ.0) THEN                         !o
          SECGPS(SECNO)=NGRP   ! GROUP COUNT FOR PREVIOUS SECTION   !o
          SECNO=4              ! HANDLING SECTION 4                 !o
          NGRP=0               ! RESET GROUP COUNT TO ZERO          !o
          SECPTR(4)=POINT2     ! START OF FIRST 5-FIGURE GROUP      !o
          BADGRP=.FALSE.       ! RESET FLAG FOR NON-5-FIGURE GROUPS !o

!   CHECK FOR NATIONAL EXCHANGE SECTION, SECTION 5.

        ELSE IF (REPORT(POINT1:POINT1+2)                            !o
     -         .EQ.'555'.AND. SECGPS(5).EQ.0) THEN                  !o
          SECGPS(SECNO)=NGRP   ! GROUP COUNT FOR PREVIOUS SECTION   !o
          SECNO=5              ! HANDLING SECTION 5                 !o
          NGRP=0               ! RESET GROUP COUNT TO ZERO          !o
          SECPTR(5)=POINT2     ! START OF FIRST 5-FIGURE GROUP      !o
          BADGRP=.FALSE.       ! RESET FLAG FOR NON-5-FIGURE GROUPS !o
        ELSE IF (REPORT(POINT1:POINT1+2).EQ.'ICE'                   !o
     &         .AND.SECNO.EQ.2) THEN                     !O
          NGRP=NGRP+1          ! INCLUDE 'ICE' IN SECTION 2 COUNT.  !o
        ELSE                                                        !o
! IF THE GROUP LENGTH IS 3 AND NOT ONE OF THE ABOVE TYPES THEN IT IS BAD
          BADGRP=.TRUE.                                             !o
        ENDIF                                                       !o

!****** GROUP LENGTH IS 5

      ELSE IF (GRPLEN.EQ.5) THEN                                    !o

!  CHECK FOR COASTAL SECTION, SECTION 2.  N.B. IN THIS CASE (ONLY)
!  PASS THE 222.. GROUP ITSELF TO THE SECTION-HANDLING PROGRAM: 222
!  IS FOLLOWED BY INFORMATION IN THE CASE OF SHIPS.

        IF (SECGPS(2).EQ.0 .AND.                                    !o
     -     (REPORT(POINT1:POINT1+4).EQ.'222//' .OR.                 !o
     -      REPORT(POINT1:POINT1+4).EQ.'22200' .OR.                 !o
     -  (SHIP .AND. REPORT(POINT1:POINT1+2).EQ.'222'))) THEN        !o
          SECGPS(SECNO)=NGRP     ! GROUP COUNT FOR PREVIOUS SECTION !o
          SECNO=2                ! HANDLING SECTION 2               !o
          NGRP=1                 ! RESET GROUP COUNT TO ONE         !o
          SECPTR(2)=POINT1       ! KEEP START OF SECTION            !o
          BADGRP=.FALSE.       ! RESET FLAG FOR NON-5-FIGURE GROUPS !o
        ELSE IF ((REPORT(POINT1:POINT1+4).EQ.'33333'              !1.16
     &     .OR. REPORT(POINT1:POINT1+4).EQ.'333//')              !1.16
     &     .AND.SECGPS(3).EQ.0                                   !1.24
     &     .AND.(.NOT.SHIP)) THEN                                !1.24
          SECGPS(SECNO)=NGRP     ! GROUP COUNT FOR PREVIOUS SECTION !o
          SECNO=3                ! HANDLING SECTION 3               !o
          NGRP=0                 ! RESET GROUP COUNT TO ZERO        !o
          SECPTR(3)=POINT2       ! START OF FIRST 5-FIGURE GROUP    !o
          BADGRP=.FALSE.       ! RESET FLAG FOR NON-5-FIGURE GROUPS !o
! if grplen is 5 then increment the number of groups in that section!o
        ELSE                                                        !o
          NGRP=NGRP+1   ! COUNT 5 FIG GROUPS IF not above types     !o
        ENDIF                                                       !o
      ENDIF                                                         !o

      IF (.NOT.LREPFL) GOTO 20

      SECGPS(SECNO)=NGRP
      LREPFL=.FALSE.
!**********************************************************************
!
! WE NOW HAVE COUNTS OF 5-FIGURE GROUPS IN THE DIFFERENT SECTIONS,
! STOPPING IF A NON-5-FIGURE GROUP WAS FOUND.
!
! FIRST PROCESS MANDATORY GROUPS IN SECTION 1: IRIXHVV & NDDFF
!  N.B. WHEN FF=99, A GROUP 00FFF FOLLOWS
!
!**********************************************************************
!
! SOME STATIONS DON'T REPORT NDDFF; TO BE SURE IT'S THERE WE WANT EITHER
! ..... ..... 1S... (WHERE S IS 0 0R 1) OR ..... ...99 00... 1S...
! - BUT THIS WOULD BE TOO RESTRICTIVE, 1- & 2-GROUPS MAY BE LEFT OUT!
! SO JUST SKIP FIRST GROUP IF THE PATTERN IS ..... 1S... 2S...
!
!**********************************************************************
      POINT=SECPTR(1)
      NGRPS=SECGPS(1)  ! NO. OF GROUPS IN THIS SECTION
                                                                      !I
      IF (NGRPS.LE.1) THEN                                            !I
!       PRINT*,' SYNEXP NGRPS.LE.1 '                                  !I
!       PRINT*,' >',REPORT(1:REPLEN),'<'                              !I
        RETURN                                                        !I
      ELSEIF (NGRPS.EQ.2) THEN                                        !I
                                                                      !I
! TEST FIRST TWO GROUPS TO FIND IF ALL CHARACTERS ARE VALID AND THERE IS
! AT LEAST ONE VALID DATA VALUE (IRIXHVV NDDFF).                      !I
                                                                      !I
        ERR=.FALSE.                                                   !I
        DATVAL=.FALSE.                                                !I
! IR                                                                  !I
        IF (REPORT(POINT:POINT).LT.'1' .OR.                           !I
     &      REPORT(POINT:POINT).GT.'4')ERR=.TRUE.                     !I
! IX                                                                  !I
        IF (REPORT(POINT+1:POINT+1).LT.'1' .OR.                       !I
     &      REPORT(POINT+1:POINT+1).GT.'7')ERR=.TRUE.                 !I
! h                                                                   !I
        IF (REPORT(POINT+2:POINT+2).GE.'0' .AND.                      !I
     &      REPORT(POINT+2:POINT+2).LE.'9')THEN                       !I
           DATVAL=.TRUE.                                              !I
        ELSEIF(REPORT(POINT+2:POINT+2).EQ.'/')THEN                    !I
        ELSE                                                          !I
           ERR=.TRUE.                                                 !I
        ENDIF                                                         !I
! VV                                                                  !I
        IF (REPORT(POINT+3:POINT+4).GE.'00' .AND.                     !I
     &      REPORT(POINT+3:POINT+4).LE.'99')THEN                      !I
           DATVAL=.TRUE.                                              !I
        ELSEIF(REPORT(POINT+3:POINT+4).EQ.'//')THEN                   !I
        ELSE                                                          !I
           ERR=.TRUE.                                                 !I
        ENDIF                                                         !I
! N                                                                   !I
        IF (REPORT(POINT+6:POINT+6).GE.'0' .AND.                      !I
     &      REPORT(POINT+6:POINT+6).LE.'9')THEN                       !I
           DATVAL=.TRUE.                                              !I
        ELSEIF(REPORT(POINT+6:POINT+6).EQ.'/')THEN                    !I
        ELSE                                                          !I
           ERR=.TRUE.                                                 !I
        ENDIF                                                         !I
! dd                                                                  !I
        IF (REPORT(POINT+7:POINT+8).GE.'00' .AND.                     !I
     &      REPORT(POINT+7:POINT+8).LE.'36')THEN                      !I
           DATVAL=.TRUE.                                              !I
        ELSEIF(REPORT(POINT+7:POINT+8).EQ.'99')THEN                   !I
           DATVAL=.TRUE.                                              !I
        ELSEIF(REPORT(POINT+7:POINT+8).EQ.'//')THEN                   !I
        ELSE                                                          !I
           ERR=.TRUE.                                                 !I
        ENDIF                                                         !I
! ff                                                                  !I
        IF (REPORT(POINT+9:POINT+10).GE.'00' .AND.                    !I
     &      REPORT(POINT+9:POINT+10).LE.'99')THEN                     !I
           DATVAL=.TRUE.                                              !I
        ELSEIF(REPORT(POINT+9:POINT+10).EQ.'//')THEN                  !I
        ELSE                                                          !I
           ERR=.TRUE.                                                 !I
           PRINT*,' SYNEXP ff >',REPORT(POINT+9:POINT+10),'<'         !I
        ENDIF                                                         !I
                                                                      !I
        IF(ERR.OR..NOT.DATVAL)THEN                                    !I
!         PRINT*,' SYNEXP NGRPS.EQ.2       failed!!!!!!!'             !I
          RETURN                                                      !I
        ELSE                                                          !I
        ENDIF                                                         !I
      ENDIF                                                           !I

! UNLESS SECOND & THIRD GROUPS LOOK LIKE 1-GROUP & 2-GROUP (TEMPERATURE)
! ASSUME FIRST TWO GROUPS ARE CLOUD/VIS/WIND (IRIXHVV NDDFF).

      IF (REPORT(POINT+6:POINT+6).EQ.'1' .AND.
     &    REPORT(POINT+12:POINT+12).EQ.'2') THEN
        POINT=POINT+6                            ! SKIP FIRST GROUP
        NGRPS=NGRPS-1
        PRINT *,'SYNEXP: NO NDDFF? ',REPORT(1:REPLEN)
      ELSE

! ASSUME IR GROUP FIRST

        IR=IVALUE(REPORT(POINT:POINT))

! IF NO RAINFALL GROUP SET PERIOD WITHOUT RAIN FROM HOUR & WMO REGION

        IF (IR.EQ.3) THEN
          RRR=0.
          CALL SYNRPR(IDATIM(4),IWMOR,IWMOB,IWMOS,TR)             !1.22
        ENDIF

! IX: SET FLAG TO SAY WHICH PRESENT/PAST WEATHER CODE TABLES ARE USED

        IX=IVALUE(REPORT(POINT+1:POINT+1))
        IF (IX.NE.MISSIN) THEN
          IF (IX.LE.4) THEN
            WXMAN=.TRUE.
          ELSE
            WXMAN=.FALSE.
          ENDIF
          IF (IX.GE.4) LAUTO=.TRUE.
        ENDIF

! HT OF LOWEST CLOUD

        I=IVALUE(REPORT(POINT+2:POINT+2))
        IF (I.NE.MISSIN) HL=HLCONV(I)

! VIS: set <100m to 90m (agreed with NWP, Adam Maycock, Jan 2000) !1.18
! (This value may be reset later by a 961.. group)                !1.18

        I=IVALUE(REPORT(POINT+3:POINT+4))
        IF (I.NE.MISSIN) THEN
          IF (I.EQ.0) THEN
            VV=90.                                                !1.18
          ELSE IF (I.LE.50) THEN
            VV=I*100
          ELSE IF (I.GE.56 .AND. I.LE.80) THEN
            VV=(I-50)*1000
          ELSE IF (I.GT.80 .AND. I.LE.89) THEN
            VV=(I-80)*5000+30000
          ELSE IF (I.GT.89) THEN
            VV=VISCONV(I)
          ENDIF
        ENDIF
!
        POINT=POINT+6
        NGRPS=NGRPS-1
!**********************************************************************
!
! NOW NDDFF GROUP(S).  FIRST TOTAL CLOUD AMOUNT
!
!**********************************************************************
        NT=IVALUE(REPORT(POINT:POINT))

! WIND DIRECTION (MAY BE 99; IF SO, PASS 990 TO Q/C & RESET IT AFTER) !B

        I=IVALUE(REPORT(POINT+1:POINT+2))
        IF (I.NE.MISSIN) THEN
          DD=I*10.
        ENDIF

! WIND SPEED (IN FOLLOWING 00FFF GROUP IF 99)

        I=IVALUE(REPORT(POINT+3:POINT+4))
        IF (I.NE.99) THEN
          FF=I
!2.1 Fix to problem 577 in which some Ships use a template
!2.1 which includes a '00' group after the wind group even
!2.1 if the FF value is less than 99.
!2.1 To get round this problem, a check is made if the following
!2.1 group starts with '00', and if so then the pointer needs to be
!2.1 moved to the start of the '00' group.
          IF (POINT+7.LE.LEN(REPORT)) THEN                 !2.2
            IF (REPORT(POINT+6:POINT+7).EQ.'00')           !2.1
     &              POINT = POINT + 6                      !2.1
          ENDIF                                            !2.2
        ELSE
          POINT=POINT+6
          NGRPS=NGRPS-1
          IF (REPORT(POINT:POINT+1).EQ.'00') THEN
            FF=IVALUE(REPORT(POINT+2:POINT+4))
          ELSE
            FF=99.
          ENDIF
        ENDIF
        IF (WNDKTS .AND. FF.GT.RMISS) FF=FF*KTS2MPS

        POINT=POINT+6
        NGRPS=NGRPS-1
      ENDIF
!***************************************************************
!
! NOW CALL SYNXP1 TO DECODE SECTION 1 FROM TEMPERATURE GROUP ON,
! THEN SUBROUTINES TO HANDLE SECTIONS 2,3,4,5 IF THEY EXIST.
!
!***************************************************************
      CALL SYNXP1(REPORT,POINT,NGRPS,SYNTAX,IWMOB,
     + WXMAN,TTT,TDTDTD,UUU,P0P0P0,PPPP,A3,HHH,ATEND,PCHANG,RRR,
     +  TR,WW,W1,W2,NH,CL,CM,CH,HOUR,MINS,IX,IDATIM)              !2.5!E

! SHIP OR COASTAL SECTION (222); NOT ALL ELEMENTS REPORTED IF COASTAL

      IF (SECGPS(2).GT.0) THEN
        POINT=SECPTR(2)        ! START OF SECTION
        NGRPS=SECGPS(2)        ! NUMBER OF GROUPS IN SECTION
        CALL SYNXP2(REPORT,POINT,NGRPS,SYNTAX,
     +              LSEA,ISWELL,TWTYPE,TWTWTW,
     +              DS,VS,TBTYPE,TBTBTB,WVMS,WVHT,WVPER,SWELWV,
     +              ISICE,ESES,RS,CI,SI,BI,DI,ZI,REPLEN)             !G
      ENDIF

! 333 SECTION.  FIRST SET PERIOD OF PAST WEATHER TO 3 HOURS FOR OBS AT
! NOMINAL HOUR 3,9,15,21Z, 6 HOURS FOR A NOMINAL HOUR OF 0,6,12,18Z.
! (SAME PERIOD MAY BE USED FOR WINDS IN 9-GROUPS)

! IF HOUR & MINS SET BY TIME GROUP IN SECTION 1, ADJUST HOUR FOR PERIOD
! checks.

      IF (MINS.GT.30 .AND. HOUR.NE.RMISS)THEN                       !k
        IF (HOUR.EQ.23) THEN                                        !k
          IHOUR=0                                                   !k
        ELSE                                                        !k
          IHOUR=HOUR+1                                              !k
        ENDIF                                                       !k
      ELSE                                                          !k
        IHOUR=IDATIM(4)
      ENDIF                                                         !k

      IF (MOD(IHOUR,3).EQ.0) THEN
        WPER=-6+MOD(IHOUR,6)    ! -6 PLUS 0 OR 3 IF MAIN/INTERMED HOUR
      ELSE
        WPER=-1                 ! OTHERWISE ASSUME HOURLY OBS
      ENDIF

      IF (SECGPS(3).GT.0) THEN
        POINT=SECPTR(3)        ! START OF SECTION
        NGRPS=SECGPS(3)        ! NUMBER OF GROUPS IN SECTION
        CALL SYNXP3(REPORT,POINT,NGRPS,SYNTAX,
     +   IWMOB,IWMOR,WNDKTS,WPER,IR,IDATIM,TPER1,TPER2,TGTG,VERTV,
     +   A3,HHH,MAXTTT,MINTTT,E,SSS,
     +   T1GUST,T2GUST,VGUST, T1GUS,T2GUS,VGUS,
     +   T1MEAN,T2MEAN,VMEAN,ICL,GROUP8,VSIF,RRR3,TR3,R24,IE,EEE,
     +   SUN2,SUN1,RNET,GLSO,DISO,LW,SW,RNET24,GLSO24,
     +   DISO24,LW24,SW24,PCHG24,SEASTE,SEAVIS,VV,LRAD,LSEA)
      ENDIF

! MOUNTAIN CLOUD SECTION (444)

      IF (SECGPS(4).GT.0) THEN
        POINT=SECPTR(4)        ! START OF SECTION
        NGRPS=SECGPS(4)        ! NUMBER OF GROUPS IN SECTION
! IMT IS ALWAYS 0 OR 2 ON EXIT                                      !C
        CALL SYNXP4(REPORT,POINT,NGRPS,IMT,MTCL)
      ENDIF

! NATIONAL SECTION (555) ONLY EXPANDED FOR BLOCKS 3 AND OVERSEAS !1.20
!                             UK OWNED STATIONS                  !1.20

      IF ((WMOBLK.EQ.3.OR.OUKSTN(INT(WMOSTA+WMOBLK*1000.0)).OR.  !1.21
     &    UKSHIP).AND. SECGPS(5).GT.0) THEN                      !2.4
        POINT=SECPTR(5)        ! START OF SECTION (FALKLANDS ADDED F
        NGRPS=SECGPS(5)        ! NUMBER OF GROUPS IN SECTION
        CALL SYNXP5(REPORT,POINT,NGRPS,WNDKTS,T1GUST,T2GUST,VGUST,
     &              NT,ICL,GROUP8,VSIF,VERTV)                       !2.0
      ENDIF
!**********************************************************************
! RULES USED FOR SETTING DATES AND TIMES (Revised May 2008).         !3
! ----------------------------------------------------------
!
! REPORT TIME is taken from a 9-group (9GGgg) in section 1 of message
!  and contained in variables HOUR & MINS (HOUR='GG', MINS='gg').
!  (See WMO SYNOP regulation 12.2.8 about coding the 9-group.)
! BULLETIN TIME (day and hour) is taken from group YYGGiw in section
!  0 of message with year and month taken from the system clock (see
!  routine OBHOUR). It is held in the 5-element array IDATIM, the
!  minute (IDATIM(5)) being zero at this point.
!
! If report time is not available use the bulletin time.
!
! Otherwise use the report time but with the following modifications:
!
!   - For manual reports, if the report time is less than 10 minutes
!     after the bulletin hour, use the bulletin time with minutes=0.
!
!   - For reports other than mobile SYNOPs, if the report time is not
!     more than 10 minutes before the bulletin hour, use the bulletin
!     time with minutes=0.
!
! If using the report time changes the hour from 00 to 23, a day will
! need to be subtracted from the date.
!
! The bulletin hour is expected to be consistent with the report
! time rounded either forwards or backwards to a whole hour. If this
! is not the case, print a warning and use the bulletin hour. This
! is the best policy as this case can be generated by a missing
! indicator ('333') for section 3 of the report leading to a 9-group
! in section 3 being erroneously interpreted as 9GGgg in section 1.
!
!**********************************************************************

      IF (MINS.NE.RMISS .AND. HOUR.NE.RMISS) THEN                    !l
        I = NINT(HOUR)                                               !3
        IF (IDATIM(4).EQ.I) THEN                                     !3

! Bulletin hours and report hours agree.                             !3
! Reset time to whole hour if manual report and minutes < 10.        !3

          IF (MINS.LT.10.0 .AND. .NOT.LAUTO) THEN                    !3
            IDATIM(5) = 0                                            !3
          ELSE                                                       !3
            IDATIM(5) = INT(MINS)                                    !3
          END IF                                                     !3

        ELSE IF (IDATIM(4).EQ.(I+1) .OR.                             !3
     &          (IDATIM(4).EQ.0 .AND. I.EQ.23)) THEN                 !3

! Bulletin hour is report hour rounded forwards to next whole hour.  !3
! Reset time to whole hour if not mobile SYNOP and minutes > 49.     !3

          IF (MINS.GE.50.0 .AND. .NOT.MOBILE) THEN                   !3
            IDATIM(5) = 0                                            !3
          ELSE                                                       !3
            IDATIM(4) = I                                            !3
            IDATIM(5) = INT(MINS)                                    !3

! Subtract a day if IDATIM(4) was changed from 0 to 23.              !3

            IF (I.EQ.23) THEN                                        !3
              CALL DATE31 (IDATIM(3),IDATIM(2),IDATIM(1),CENDAY)     !l
              CENDAY = CENDAY - 1                                    !l
              CALL DATE13 (CENDAY,IDATIM(3),IDATIM(2),IDATIM(1))     !l
            ENDIF
          END IF

        ELSE

! Ignore report time if not same hour or previous hour.             !lp

          WRITE (6,'(T4,A,T13,A,2(A,I2.2,A,I2.2))')                  !3
     &        'SYNEXP:', 'TIMES DIFFER - ',                          !3
     &        'BULLETIN DD/HH IS ', IDATIM(3), '/', IDATIM(4),       !3
     &        ',  REPORT HH:MM IS ', I, ':', INT(MINS)               !3
          I = MIN0(REPLEN,102)                                       !3
          WRITE (6,'(T13,2A)') 'REPORT STARTS: ', REPORT(1:I)        !3
        END IF
      END IF                                                         !l

!**********************************************************************
!
! END OF EXPANSION. PUT VALUES IN ARRAY FOR INTERNAL CONSISTENCY CHECKS
!
!**********************************************************************
      REXP(2,1)=WMOBLK
      REXP(2,2)=WMOSTA
      REXP(2,3)=WMOREG
      REXP(2,4)=LAT
      REXP(2,5)=LON
      REXP(2,6)=STNHT                     ! station height          !m
      REXP(2,7)=STNHTP                    ! pressure sensor height  !m
      IF (IX.LT.4) REXP(2,8)=1.       ! use IX (not WXMAN!) to    !1.14
      IF (IX.GE.4) REXP(2,8)=0.       ! set manual/automatic flag !1.14
      REXP(2,9)=IDATIM(1)                 ! YEAR                    !m
      REXP(2,10)=IDATIM(2)                ! MONTH                   !m
      REXP(2,11)=IDATIM(3)                ! DAY                     !m
      REXP(2,12)=IDATIM(4)                ! HOUR                    !m
      REXP(2,13)=IDATIM(5)                ! MINUTE                  !m
      IF (IWREP) REXP(2,14)=IW                                      !m
      REXP(2,15)=DD                                                 !m
      REXP(2,16)=FF                                                 !m
      REXP(2,17)=TTT                                                !m
      REXP(2,18)=TDTDTD                                             !m
      REXP(2,19)=UUU                                                !m
      REXP(2,20)=VV                                                 !m
      REXP(2,21)=VERTV                                              !m
      REXP(2,22)=WW                                                 !m
      REXP(2,23)=WPER                                               !m
      REXP(2,24)=W1                                                 !m
      REXP(2,25)=W2                                                 !m
      REXP(2,26)=NT         !  TOTAL CLOUD COVER                    !m
      REXP(2,27)=TPER1                                              !m
      REXP(2,28)=MAXTTT                                             !m
      REXP(2,29)=TPER2                                              !m
      REXP(2,30)=MINTTT                                             !m
      REXP(2,31)=E                                                  !m
      REXP(2,32)=TR                                                 !m
      REXP(2,33)=RRR                                                !m
      REXP(2,34)=TR3                                                !m
      REXP(2,35)=RRR3                                               !m
      REXP(2,36)=R24                                                !m
      IF(VGUS.EQ.RMISS)THEN                                      !D !m
         REXP(2,37)=T1GUST                                       !D !m
         REXP(2,38)=T2GUST                                       !D !m
         REXP(2,39)=VGUST                                        !D !m
      ELSE                                                       !D !m
         REXP(2,37)=T1GUS                                        !D !m
         REXP(2,38)=T2GUS                                        !D !m
         REXP(2,39)=VGUS                                         !D !m
      ENDIF                                                      !D !m
      REXP(2,40)=T1MEAN                                             !m
      REXP(2,41)=T2MEAN                                             !m
      REXP(2,42)=VMEAN                                              !m
      REXP(2,43)=PPPP                                               !m
      REXP(2,44)=P0P0P0                                             !m
      REXP(2,45)=A3                                                 !m
      REXP(2,46)=HHH                                                !m
      REXP(2,47)=SSS                                                !m
      REXP(2,48)=TGTG                                               !m
      REXP(2,49)=ATEND                                              !m
      REXP(2,50)=PCHANG                                             !m
      REXP(2,51)=PCHG24                                             !m

! RADIATION ELEMENTS (IF ANY)

      IF(LRAD)THEN
        REXP(2,52)=IE                                               !m
        REXP(2,53)=EEE                                              !m
        REXP(2,54)=-24                                              !m
        REXP(2,55)=SUN2                                             !m
        REXP(2,56)=-1.                                              !m
        REXP(2,57)=SUN1                                             !m
        REXP(2,58)=RNET                                             !m
        REXP(2,59)=GLSO                                             !m
        REXP(2,60)=DISO                                             !m
        REXP(2,61)=LW                                               !m
        REXP(2,62)=SW                                               !m
        REXP(2,63)=SWNET                                            !m
        REXP(2,64)=SDIR                                             !m
        REXP(2,65)=RNET24                                           !m
        REXP(2,66)=GLSO24                                           !m
        REXP(2,67)=DISO24                                           !m
        REXP(2,68)=LW24                                             !m
        REXP(2,69)=SW24                                             !m
        REXP(2,70)=SWNET24                                          !m
        REXP(2,71)=SDIR24                                           !m
      ENDIF

! SHIP ELEMENTS OR COASTAL (IF ANY)

      IF(LSEA)THEN           ! MARINE DATA FOUND.
        REXP(2,72)=1.        ! SEA QUALIFIER                        !m
        REXP(2,73)=SEAVIS    ! VISIBILITY TOWARDS SEA               !m
        REXP(2,74)=SEASTE    ! STATE OF SEA                         !m
        REXP(2,75)=1.        ! (DIRECTION FROM CODE FIGURE, VERY ROUGH)
        REXP(2,76)=DS        ! DIRECTION OF MOVEMENT                !m
        REXP(2,77)=1.        ! (SPEED FROM CODE FIGURE, VERY ROUGH) !m
        REXP(2,78)=VS        ! SPEED OF MOVEMENT                    !m
        REXP(2,79)=TWTYPE    ! SEA-SURFACE TEMP. MEASUREMENT METHOD !m
        REXP(2,80)=TWTWTW    ! SEA-SURFACE TEMPERATURE              !m
        REXP(2,81)=TBTYPE    ! WET-BULB TEMPERATURE MEASUREMENT METHOD
        REXP(2,82)=TBTBTB    ! WET-BULB TEMPERATURE                 !m
        REXP(2,83)=WVMS      ! WAVE MEASUREMENT METHOD              !m
        REXP(2,84)=WVPER     ! WAVE PERIOD                          !m
        REXP(2,85)=WVHT      ! WAVE HEIGHT                          !m
        REXP(2,86)=RMISS     ! (CANCEL WAVE MEASUREMENT METHOD)     !m
        REXP(2,87)=SWELWV(1) ! SWELL WAVE DIRECTION                 !m
        REXP(2,88)=SWELWV(2) ! SWELL WAVE PERIOD                    !m
        REXP(2,89)=SWELWV(3) ! SWELL WAVE HEIGHT                    !m
        REXP(2,90)=SWELWV(4) ! SWELL WAVE DIRECTION                 !m
        REXP(2,91)=SWELWV(5) ! SWELL WAVE PERIOD                    !m
        REXP(2,92)=SWELWV(6) ! SWELL WAVE HEIGHT                    !m
        REXP(2,93)=ISICE     !                                      !m
        REXP(2,94)=ESES      !                                      !m
        REXP(2,95)=RS        !                                      !m
        REXP(2,96)=CI        !                                      !m
        REXP(2,97)=SI        !                                      !m
        REXP(2,98)=BI        !                                      !m
        REXP(2,99)=DI        !                                      !m
        REXP(2,100)=ZI        !                                     !m
      ENDIF                                                         !m

! END WITH CLOUD.
                                                                    !m
      REXP(2,101)=7.                                                !m
      REXP(2,102)=CL                                                !m
      REXP(2,103)=8.                                                !m
      REXP(2,104)=CM                                                !m
      REXP(2,105)=9.                                                !m
      REXP(2,106)=CH                                                !m
      REXP(2,107)=0.                                                !m
      REXP(2,108)=NH                                                !m
      REXP(2,109)=HL                                                !m

! SECTION 3 CLOUD GROUPS (LAYER NUMBER, AMOUNT, TYPE, BASE)

      IF(ICL.GT.0)THEN
        IF(ICL.GT.4) ICL=4                                            !B
        DO I=1,ICL                                                    !B
         REXP(2,110+(I-1)*4)=VSIF(I)                                !m
         REXP(2,111+(I-1)*4)=GROUP8(1+(I-1)*3)                      !m
         REXP(2,112+(I-1)*4)=GROUP8(2+(I-1)*3)                      !m
         REXP(2,113+(I-1)*4)=GROUP8(I*3)                            !m
        ENDDO
      ENDIF

! MOUNTAIN CLOUD GROUPS (008002, AMOUNT, TYPE, HEIGHT OF TOP, SHAPE)
! (THE SECOND GROUP MAY BE MISSING )                                !C
      IF(IMT.GT.0)THEN
        DO I=1,2                                                    !C
         REXP(2,126+(I-1)*5)=RMISS    ! 008002 (CODE FIGURE NOT DECIDED)
         REXP(2,127+(I-1)*5)=MTCL(1,I)                              !m
         REXP(2,128+(I-1)*5)=MTCL(2,I)                              !m
         REXP(2,129+(I-1)*5)=MTCL(3,I)                              !m
         REXP(2,130+(I-1)*5)=MTCL(4,I)                              !m
        ENDDO
      ENDIF
!*********************************************************************
!
! DO INTERNAL CONSISTENCY, LAND/SEA & SEQUENCE CHECKS
! & COUNT METEOROLOGICAL ELEMENTS
!
!*********************************************************************

! CHECK INTERNAL CONSISTENCY (CLOUD, WEATHER, VIS ETC), SETTING FLAGS
! IN REXP(1,N) - OR THE SYNTAX FLAG.

      MSG=.FALSE.                   !1.26 diagnostics not required
      CALL SYNQC(REXP,WXMAN,IR,IX,ICL,WPER,MSG,SYNTAX,SHIP)    !1.26

! SYNQC HAS CHECKED FOR DD=990 (VARIABLE): NOW SET THAT TO ZERO    !B

      IF (REXP(2,15).EQ.990.) REXP(2,15)=0.                        !Bm

! PRESSURE SEQUENCE CHECK: PSEQ IS SET LIKE THE Q/C FLAGS (CONSISTENT,
! SUSPECT, NO TEST DONE) - FLAG ALL ELEMENTS INVOLVED IN THE SAME WAY.
! SAME FOR SHIPS'' LAT/LONG FLAG (LAT/LONG CHECKED AGAINST MOVEMENT).
! SKIP LAND CHECK IF PRESSURE OR CHANGE MISSING.
! SKIP SHIP CHECK IF CALL SIGN IS ONLY 'SHIP'!.
! NO SEQUENCE CHECKS ARE DONE FOR MOBILE SYNOPS.                     !2

      PSEQ=RMISS  ! No check yet                                     !2

      IF (SHIP) THEN                                               !j
        IF (LAT.EQ.RMISS.OR.LON.EQ.RMISS)THEN                      !j
!         DO NOT DO SEQUENCE CHECK                                 !j
        ELSEIF (IDENT(1:6).NE.'SHIP  ') THEN                       !j
          CALL SHPSEQ(IDENT,IDATIM,PPPP,PCHANG,PSEQ,
     &                LAT,LON,DS,VS,LLFLAG)
          REXP(1,4)=LLFLAG            ! LAT
          REXP(1,5)=LLFLAG            ! LONG
          REXP(1,76)=LLFLAG           ! DS                          !m
          REXP(1,78)=LLFLAG           ! VS                          !m
        ENDIF
      ELSE IF (.NOT.MOBILE .AND.                                     !2
     &         PPPP.NE.RMISS .AND. PCHANG.NE.RMISS) THEN             !2
        CALL SYNSEQ(IDENT(1:5),IDATIM,PPPP,PCHANG,PSEQ)              !2
      ENDIF
      REXP(1,43)=PSEQ                 ! MSL PRESSURE                !m
      REXP(1,49)=PSEQ                 ! PRESSURE TENDENCY           !m
      REXP(1,50)=PSEQ                 ! PRESSURE CHANGE             !m

! IF SHIP IS ONLAND (ACCORDING TO OUR MAP OF ONE-DEGREE SQUARES WITH
! SEA IN) FLAG LAT & LONG.

      IF (SHIP) THEN
        IF (LAT.EQ.RMISS.OR.LON.EQ.RMISS)THEN                      !j
          REXP(1,4)=1.                ! LATITUDE SUSPECT            j
          REXP(1,5)=1.                ! LONGITUDE SUSPECT           j
        ELSEIF (ONLAND(LAT,LON)) THEN
          REXP(1,4)=1.                ! LATITUDE SUSPECT
          REXP(1,5)=1.                ! LONGITUDE SUSPECT
        ENDIF
      ENDIF

! COUNT ELEMENTS WHICH ARE METEOROLOGICAL, NOT MISSING & NOT FLAGGED,
! THEN ADD 3 FOR EACH CLOUD GROUP.

                                                                    !m
      DO I=15,109      ! (DD-HL)                                    !m
       IF (METELM(I:I).EQ.'1' .AND. REXP(2,I).NE.RMISS
     -                        .AND. REXP(1,I).NE.1) NELM=NELM+1
      ENDDO
      NELM=NELM+3*(ICL+IMT)                                       !C
!*********************************************************************
!
! CONVERT 2-DIMENSIONAL ARRAY USED FOR QUALITY CONTROL INTO
! 1-DIMENSIONAL ARRAY FOR BUFR ENCODING.
!
!*********************************************************************
      J=1          ! OUTPUT SUBSCRIPT
      FREXP(J)=2.  !  SIGNIFICANCE OF ASSOCIATED DATA, INDICATING QC.

! ID DETAILS: CALL SIGN (SHIPS OR MOBILE SYNOPS); WMO NO. (LAND SYNOPS)

      IF (SHIP) THEN                                                 !2
        FREXP(2)=RMISS    ! BUOY ID Q/C missing                      !2
        FREXP(3)=RMISS    ! BUOY ID missing                          !2
        FREXP(4)=RMISS    ! Call sign Q/C missing                    !2
        FREXP(5)=1.       ! Call sign already in character string    !2
        J=5                                                          !2
      ELSE IF (MOBILE) THEN                                          !2
        FREXP(2)=RMISS    ! Call sign Q/C missing                    !2
        FREXP(3)=1.       ! Call sign already in character string    !2
        J=3                                                          !2
      ELSE                                                           !2
        DO I=1,3          ! WMO block, station, region               !2
          J=J+2                                                      !2
          FREXP(J-1)=REXP(1,I)   ! Flag                              !2
          FREXP(J)=REXP(2,I)     ! Element                           !2
        END DO                                                       !2
      END IF                                                         !2

! SEPARATE CODE FOR LAND STATIONS & SHIPS UNTIL CLOUD DATA. LAND FIRST:

      IF (.NOT.SHIP) THEN
        DO I=4,51 !   MANDATORY ELEMENTS (LAT-PCHG24)                !2
          J=J+2
          FREXP(J-1)=REXP(1,I)       ! FLAG
          FREXP(J)=REXP(2,I)         ! ELEMENT
        ENDDO

! OPTIONAL RADIATION SECTION - INSERT DELAYED REPLICATION COUNT

        IF(.NOT.LRAD)THEN
          J=J+1
          FREXP(J)=0.
        ELSE
          J=J+1
          FREXP(J)=1.

          DO I=52,71     ! (IE-SDIR24)                              !m
           J=J+2
           FREXP(J-1)=REXP(1,I)
           FREXP(J)=REXP(2,I)
          ENDDO
        ENDIF

! OPTIONAL COASTAL SECTION

        IF(.NOT.LSEA)THEN
          J=J+1
          FREXP(J)=0.
        ELSE
          J=J+1
          FREXP(J)=1.
!                          LAND/SEA, VIS, STATE OF SEA
          DO I=72,74     ! (1.-SEASTE)                              !m
           J=J+2
           FREXP(J-1)=REXP(1,I)
           FREXP(J)=REXP(2,I)
          ENDDO
!                          SEA WATER TEMPERATURE
          DO I=79,80     ! (TWTYPE,TWTWTW)                          !m
           J=J+2
           FREXP(J-1)=REXP(1,I)
           FREXP(J)=REXP(2,I)
          ENDDO
!                          WAVES & SWELL (INCLUDING MISSING VALUE OF
!                           ELEMENT 85 TO CANCEL MEASUREMENT METHOD)
          DO I=83,92     ! (WVMS-SWELWV(6))                         !m
           J=J+2
           FREXP(J-1)=REXP(1,I)
           FREXP(J)=REXP(2,I)
          ENDDO
        ENDIF
      ELSE

! SHIPS HAVE NO SNOW, STATE OF GROUND ETC, NO RADIATION SECTION,
! NO SPECIFICALLY COASTAL ELEMENTS LIKE VISIBILITY OUT TO SEA.

        DO I=4,30              ! LAT, LONG ETC  (LAT-MINTTT)        !m
         J=J+2
         FREXP(J-1)=REXP(1,I)
         FREXP(J)=REXP(2,I)
        ENDDO

! SKIP STATE OF GROUND FOR SHIPS

        DO I=32,43             ! (TR-PPPP)                          !m
         J=J+2
         FREXP(J-1)=REXP(1,I)
         FREXP(J)=REXP(2,I)
        ENDDO

! SKIP HIGH-LEVEL PRESSURE & SNOW FOR SHIPS

        DO I=49,51             ! (ATEND-PCHG24)                     !m
         J=J+2
         FREXP(J-1)=REXP(1,I)
         FREXP(J)=REXP(2,I)
        ENDDO

! SKIP COASTAL ELEMENTS BUT THEN DO ALL MARINE SECTION, INCLUDING ICE.

        DO I=75,100            ! (1.-ZI)                            !m
         J=J+2
         FREXP(J-1)=REXP(1,I)
         FREXP(J)=REXP(2,I)
        ENDDO
      ENDIF

! FINALLY CLOUD: MAIN CLOUD GROUP, THEN ANY 8-GROUPS & MOUNTAIN CLOUD

      DO I=101,109             ! (7.-HL)                            !m
       J=J+2
       FREXP(J-1)=REXP(1,I)
       FREXP(J)=REXP(2,I)
      ENDDO

! 8-GROUPS

      J=J+1
      FREXP(J)=ICL                     ! COUNT OF 8-GROUPS          !B

      DO LGROUP=1,ICL                                               !B
       DO L=1,4
        J=J+2
        FREXP(J-1)=REXP(1,109+(LGROUP-1)*4+L)                       !m
        FREXP(J)=REXP(2,109+(LGROUP-1)*4+L)                         !m
       ENDDO
      ENDDO

! MOUNTAIN CLOUD GROUPS (SHIPS WON''T HAVE ANY!)

      J=J+1
      FREXP(J)=IMT                     ! COUNT OF MOUNTAIN GROUPS   !C

      DO LGROUP=1,IMT                                               !C
       DO L=1,5
        J=J+2
        FREXP(J-1)=REXP(1,125+(LGROUP-1)*5+L)                       !m
        FREXP(J)=REXP(2,125+(LGROUP-1)*5+L)                         !m
       ENDDO
      ENDDO
      RETURN
      END
