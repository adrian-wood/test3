      SUBROUTINE SYNFIV(REPORT,POINT,NGRPS,IG,                      !1.6
     &    IR,IE,EEE,SUN2,SUN1,RNET,GLSO,DISO,LW,SW,SWNET,SDIR,      !1.6
     &    RNET24,GLSO24,DISO24,LW24,SW24,SWNET24,SDIR24,PCHG24)     !1.6

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNFIV
!
! PURPOSE       : TO EXPAND 5-GROUPS AND ASSOCIATED RADIATION GROUPS
!                 FROM SECTION 3 OF A SYNOP REPORT.
!
! DESCRIPTION   : EVAPOTRANSPIRATION & 24-HOUR PRESSURE CHANGE ARE
!                 SIMPLE, UNLIKE 55... GROUPS, WHICH CAN BE SUNSHINE
!                 (55SSS/553SS) OR RADIATION INDICATORS (55407 ETC),
!                 & MAY BE FOLLOWED BY RADIATION GROUPS STARTING 0-6.
!                 THE INITIAL 0-6 SAYS WHICH RADIATION ELEMENT (SEE
!                 ARGUMENT LIST) & THE INDICATOR GROUPS EFFECTIVELY
!                 ADD 7 & 8 TO THIS LIST, AVOIDING CONFUSION WITH 7-
!                 & 8-GROUPS IN SECTION 3, BUT LEAVING A 6-GROUP AS
!                 EITHER RADIATION OR RAINFALL, A 5-GROUP RADIATION
!                 OR ANOTHER MAIN 5-GROUP.
!
! CALLED BY     : SYNXP3
!
! CALLS         : IVALUE, MTRLOC
!
! PARAMETERS    : (1)REPORT  CHAR    COMPLETE REORT               (I)
!                 (2)POINT   INTEGER START OF FIRST 5-GROUP       (I)
!                 (3)NGRPS   INTEGER NUMBER OF GROUPS IN SECTION  (I)
!                 (4)IG      INTEGER GROUP NUMBER               (I/O)
!                 (5)IR      INTEGER RAINFALL REPORTING FLAG      (I)
!                 (6)IE      REAL    EVAPO INSTR.                 (O)
!                 (7)EEE     REAL    24 HR EVAPOTRANSPIRATION     (O)
!                 (8)SUN2    REAL    DAILY SUNSHINE               (O)
!                 (9)SUN1    REAL    HOURLY SUNSHINE              (O)
!                (10)RNET    REAL    HOURLY NET RAD.              (O)
!                (11)GLSO    REAL    HOURLY GLOBAL SOLAR RAD.     (O)
!                (12)DISO    REAL    HOURLY DIFFUSE SOLAR RAD.    (O)
!                (13)LW      REAL    HOURLY NET LONGWAVE RAD.     (O)
!                (14)SW      REAL    HOURLY SHORTWAVE RAD.        (O)
!                (15)SWNET   REAL    HOURLY NET SHORTWAVE         (O)
!                (16)SDIR    REAL    HOURLY DIRECT SOLAR RAD.     (O)
!                (17)RNET24  REAL    DAILY NET RAD.               (O)
!                (18)GLSO24  REAL    DAILY GLOBAL SOLAR RAD.      (O)
!                (19)DISO24  REAL    DAILY DIFFUSE SOLAR RAD.     (O)
!                (20)LW24    REAL    DAILY NET LONGWAVE RAD.      (O)
!                (21)SW24    REAL    DAILY SHORTWAVE RAD.         (O)
!                (22)SWNET24 REAL    DAILY NET SHORTWAVE          (O)
!                (23)SDIR24  REAL    DAILY DIRECT SOLAR RADIATION (O)
!                (24)PCHG24  REAL    24 HOUR PRESSURE CHANGE      (O)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:49$
! $Source: /home/us0400/mdb/op/lib/source/RCS/synfiv.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:49    Sheila Needham  
! $
! Revision 2.1  2001/08/09 10:36:54  usmdb
! Completely restructured, variable names made consistent with
! Manual on Codes, all currently reported radiation groups and
! following groups checked (because fix !1.6 didn't work).
!
! Revision 2.0  2001/07/03  10:44:04  10:44:04  usmdb (Generic MetDB account)
! Changed preprocessor statement to be if (MVS), else...
! Separates variable declaration and initialisation. Removed
! unused variables. Added copyright and modified header - S.Cox
! 
! Revision 1.6  2000/09/06  10:40:49  10:40:49  usmdb (Generic MetDB account)
! 18th Sep 2000, Revision 1.6, Stanley Kellett
! Checks added so that code copes with 55/// and 53// so that
! the following 2 group is treated as a Radiation group and
! not as a min temp group out of sequence.
!
! Revision 1.5  98/01/29  11:32:55  11:32:55  usmdb (Generic MDB account)
! Addition of IBM preprocess directive
!
! Revision 1.4  97/07/31  11:37:56  11:37:56  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.3  1997/07/04 13:43:00  uspm
! Latest version from  1  - Y2K check
!
! Revision 1.2  1997/05/15 15:08:00  uspm
! Add revision information and preprocessor statements about
! INCLUDE statements
!
! JAN 95 - IMPROVE DECISION BETWEEN RAINFALL GROUP & SHORT-WAVE RAD
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

      CHARACTER*(*) REPORT
      INTEGER POINT         ! to first character of a group
      INTEGER NGRPS         ! number of groups in section
      INTEGER IG            ! number of current group in section
      INTEGER IR            ! rainfall indicator
      INTEGER J1            ! second figure of 5-group
      INTEGER J2            ! third figure of 5-group
      INTEGER J4            ! last figure of 5-group
      INTEGER I             ! value from several figures
      INTEGER ID            ! indicates which radiation element
      INTEGER IVALUE
      INTEGER SCALE         ! set to 10**4 if daily, 10**3 if hourly

#if defined (MVS)
      INCLUDE (SYNCOM)
      INCLUDE (SYNELM)
#else
      INCLUDE 'SYNCOM'
      INCLUDE 'SYNELM'
#endif

      CHARACTER*132 HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/synfiv.F,v $
     &'//'$ $Date: 30/01/2006 20:24:49$ $Author: Sheila Needham$ '//
     &'$Revision: 1$'

! Loop round 5-groups (outer loop) & any following radiation groups
! (inner loop)

      DO WHILE (IG.LE.NGRPS .AND. REPORT(POINT:POINT).EQ.'5')
        J1=IVALUE(REPORT(POINT+1:POINT+1))

! Use the second figure (J1) to identify the (main) 5-group.
! We're only interested in evapotranspiration (<4), sunshine and
! radiation (5) and 24-hour pressure change (8-9): ignore 4, 6 & 7.

! J1<4 indicates 5EEEiE: convert EEE from tenths of mm to kg/m**2
! (SYNOP code 1806, BUFR code 002004)

        IF (J1.GE.0 .AND. J1.LE.3) THEN
          I=IVALUE(REPORT(POINT+1:POINT+3))
          IF (I.NE.MISSIN) EEE=I*0.1
          IE=IVALUE(REPORT(POINT+4:POINT+4))

! J1=8 or 9 means 24-hour pressure change in mb & tenths: convert to Pa

        ELSE IF (J1.EQ.8 .OR. J1.EQ.9) THEN
          I=IVALUE(REPORT(POINT+2:POINT+4))
          IF (I.NE.MISSIN) THEN
            IF (J1.EQ.8) THEN
              PCHG24=I*10.
            ELSE IF (J1.EQ.9) THEN
              PCHG24=-I*10.
            ENDIF
          ENDIF

! This leaves sunshine and radiation groups.  The starting groups are
! 55sss, 553ss (daily & hourly sunshine), 55407, 55408, 55507, 55508.

        ELSE IF (J1.EQ.5) THEN                ! if second figure is 5,
          J2=IVALUE(REPORT(POINT+2:POINT+2))  ! look at third figure
          IF (J2.GE.0 .AND. J2.LT.3) THEN     ! 55sss, daily sunshine
            I=IVALUE(REPORT(POINT+2:POINT+4)) ! tenths of hour
            IF (I.NE.MISSIN) SUN2=I*0.1       ! to hours
            SCALE=10000                       ! daily amounts follow
          ELSE IF (J2.EQ.3) THEN              ! 553ss, hourly sunshine
            I=IVALUE(REPORT(POINT+3:POINT+4)) ! tenths of hour
            IF (I.NE.MISSIN) SUN1=I*6         ! to minutes
            SCALE=1000                        ! hourly amounts follow
          ELSE IF (J2.EQ.4 .OR. J2.EQ.5) THEN ! 4 if daily, 5 if hourly
            J4=IVALUE(REPORT(POINT+4:POINT+4))! J4 sets ID=7 or ID=8
            IF (J2.EQ.4) SCALE=10000          ! daily radiation next
            IF (J2.EQ.5) SCALE=1000           ! hourly radiation next
          ELSE IF (REPORT(POINT+2:POINT+4).EQ.'///') THEN ! no daily sun
            SCALE=10000                       ! so daily amounts next
          ELSE
            SCALE=MISSIN                      ! no radiation to follow
          ENDIF

! If scale has been set, the 5-group just handled may be followed by
! radiation groups (one or more), daily or hourly as given by scale.
! Loop until another 55-group is found (5 but not 55 may be radiation!)
! - or 56-, 57-, 58-, 59-, all of which end the radiation section.

! N.B. Radiation groups MAY follow: only 55407 etc say they MUST follow.
! 55/// & 553// are obviously not worth reporting unless radiation does
! follow, but no regulation rules out these groups on their own - and
! some stations do send skeleton groups!

          IF (SCALE.NE.MISSIN) THEN
            DO WHILE (IG.LT.NGRPS .AND.
     &               (REPORT(POINT+6:POINT+7).LT.'55' .OR.
     &                REPORT(POINT+6:POINT+7).GT.'59'))
              IG=IG+1
              POINT=POINT+6
              ID=IVALUE(REPORT(POINT:POINT))
              IF (J2.EQ.4 .OR. J2.EQ.5) THEN
                ID=J4

! If a 6-group does not follow one of 55407 etc, then assume it is
! rainfall (and return) if rainfall is expected in section 3 and
! this is either the last group in section 3 or the last 6-group
! (i.e. a higher first figure follows)
! or if it starts 699 (most likely to be rainfall in mm).

              ELSE IF (ID.EQ.6) THEN
                IF ((IR.EQ.0 .OR. IR.EQ.2) .AND. (IG.EQ.NGRPS .OR.
     &              REPORT(POINT+6:POINT+6).GT.'6')) RETURN
                IF (REPORT(POINT+1:POINT+2).EQ.'99') RETURN
              ELSE IF (ID.GT.6) THEN
                RETURN
              ENDIF

! If we've reached this point, the group must be radiation.

              I=IVALUE(REPORT(POINT+1:POINT+4))
              IF (I.NE.MISSIN) THEN
                I=I*SCALE
                IF (SCALE.EQ.10000) THEN
                  IF (ID.EQ.0) RNET24=I
                  IF (ID.EQ.1) RNET24=-I
                  IF (ID.EQ.2) GLSO24=I
                  IF (ID.EQ.3) DISO24=I
                  IF (ID.EQ.4) LW24=-I
                  IF (ID.EQ.5) LW24=I
                  IF (ID.EQ.6) SW24=I
                  IF (ID.EQ.7) SWNET24=I
                  IF (ID.EQ.8) SDIR24=I
                ELSE IF (SCALE.EQ.1000.) THEN
                  IF (ID.EQ.0) RNET=I
                  IF (ID.EQ.1) RNET=-I
                  IF (ID.EQ.2) GLSO=I
                  IF (ID.EQ.3) DISO=I
                  IF (ID.EQ.4) LW=-I
                  IF (ID.EQ.5) LW=I
                  IF (ID.EQ.6) SW=I
                  IF (ID.EQ.7) SWNET=I
                  IF (ID.EQ.8) SDIR=I
                ENDIF
              ENDIF
            ENDDO                        ! loop round radiation groups
          ENDIF
        ENDIF
        POINT=POINT+6
        IG=IG+1
      ENDDO                              ! loop round groups in section
      RETURN
      END
