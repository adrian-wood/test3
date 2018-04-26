      SUBROUTINE SYNRPR(HOUR,REGION,BLOCK,STNNUM,PERIOD)            !1.3

!-----------------------------------------------------------------------
!
! PROGRAM       : SYNRPR
!
! PURPOSE       : TO DECIDE THE PERIOD WITH ZERO RAINFALL WHEN IR=3
!                 IN A SYNOP.  THE RAINFALL PERIOD FOR REPORTS AT A
!                 GIVEN HOUR VARIES FROM REGION TO REGION (& WITHIN
!                 REGIONS), SO THE PERIODS BELOW ARE BASED ON WORK
!                 BY RUSSELL WATKINS IN 1993.  THE PERIOD WITH NO
!                 RAINFALL MAY BE REPORTED EXPLICITLY IN THE FUTURE.
!
! CALLED BY     : SYNEXP
!
! PARAMETERS    : HOUR    HOUR OF OBSERVATION.                 (I)  !1.3
!                 REGION  WMO REGION OF STATION.               (I)  !1.3
!                 BLOCK   WMO BLOCK Number of Station, 2 digits(I)  !1.3
!                 STNNUM  WMO Station Number, 3 digits         (I)  !1.3
!                 PERIOD  PERIOD OF NO RAIN.                   (O)  !1.3
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:00$
! $Source: /data/us0400/mdb/op/lib/source/RCS/synrpr.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:00    Sheila Needham  
! $
! Revision 2.1  2002/12/02  14:35:57  14:35:57  usmdb (Generic MetDB account)
! Change day 16DEC02    R Hirst
! No longer evaluate rainfall periods for blocks 42 & 43 in this
! routine. They are done in SYNXP1 & SYNXP3.
! 
! Revision 2.0  2001/07/03  10:44:08  10:44:08  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.5  2000/11/07  12:18:17  12:18:17  usmdb (Generic MetDB account)
! 20/11/2000 Stanley Kellett
! Changed the default rainfall periods at 06z and 18z for former USSR
! stations from 6 hrs to 12 hrs.
!
! Revision 1.4  2000/10/04  15:43:44  15:43:44  usmdb (Generic MDB account)
! 16 Oct 2000 Stanley Kellett
! Bugfix - Change went in during August to update Rainfall defaults,
! however one bug got in when a AND was plaed instead of an OR leading
! to some Stations getting an incorrect default rainfall period stored.
! Comments improved to make more readable.
!
! Revision 1.3  2000/08/09  15:08:56  15:08:56  usmdb (Generic MDB account)
! 21/8/2000 Out of date defaults updated with infomation supplied
! by Keith Grant and WMO manual. Stan Kellett
!
! Revision 1.2  97/07/31  11:39:25  11:39:25  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 13:48:06  uspm
! Initial revision
!
! FEB 97: PERIOD VALUES CHANGED FROM POSITIVE TO NEGATIVE. THIS
!         WAS DONE TO MAKE THE PERIODS CONSISTENT WITHIN SYNOP
!         STORAGE.                                                    JN
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

      IMPLICIT NONE                                                 !1.3

      INTEGER HOUR,REGION
      INTEGER BLOCK,                            !1.3 Block number
     &        STNNUM                            !1.3 Station number
      REAL PERIOD
      CHARACTER*132 HEAD
*
      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/synrpr.f,v $
     &'//'$ $Date: 30/01/2006 20:25:00$ $Revision: 1$'

!1.4 At 00z the Asian Region 2 (except for China which has a default
!1.4 rainfall period of 6hrs) and the former USSR countries
!1.4 have a default rainfall period of 12 hrs
      IF (HOUR.EQ.0) THEN
        IF ((REGION.EQ.2 .AND.                    !1.4 Asia
     &  .NOT.(BLOCK.GE.50 .AND. BLOCK.LE.59)).OR. !1.4 Not China
     &  (BLOCK.GE.20 .AND. BLOCK.LE.38)) THEN     !1.4 Former USSR
          PERIOD= -12.

!1.4 At 00z The Antartic and Region 5 have a default of 24 hrs except
!1.4 for Australia and New Zealand who due to their practice of
!1.4 reporting rainfall  either side of hour are dealt with
!1.4 at the end of the routine.
        ELSEIF (BLOCK.EQ.89 .OR.                 !1.4 Antarctic
     &   (REGION.EQ.5 .AND.(BLOCK.EQ.91 .OR.     !1.4 Region 5 non-
     &    BLOCK.GE.96))) THEN                    !1.4  Australian/
                                                 !1.4  New Zeland
          PERIOD= -24.

!1.4 At 00z the default Rainfall Period for everyone else is 6 hours
        ELSE                                     !1.3 Everyone else
          PERIOD= -6.
        ENDIF
*
      ELSE IF (HOUR.EQ.6) THEN

!1.4 At hour 6 Europe Reports a 12 hour Rainfall Period, Africa
!1.4 reports a 24 hour Period. The default for the rest of the world
!1.4 is 6 hours.
        IF (REGION.EQ.6.OR.                           !1.5 Europe or
     &      (BLOCK.GE.20.AND.BLOCK.LE.38)) THEN       !1.5 Former USSR
          PERIOD= -12.
        ELSEIF (REGION.EQ.1) THEN                     !1.3 Africa
          PERIOD= -24.                                !1.3
        ELSE
          PERIOD= -6.
        ENDIF

!1.3 At 9Z Iceland reports a 15 hour period
      ELSE IF ( HOUR.EQ.9 .AND.                       !1.4 09Z
     &         BLOCK.EQ.4 .AND. STNNUM.GE.000 .AND.   !1.4 Iceland
     &         STNNUM.LE.199) THEN                    !1.4
        PERIOD = -15.                                 !1.4
!
      ELSE IF (HOUR.EQ.12) THEN

!1.4 At 12z South America has a 24 hour default Rainfall Period.
        IF (REGION.EQ.3) THEN                     !1.3 South America
          PERIOD = -24.                             !1.3

!1.4 At 12z Antarctic and Asia (except for China) and the former
!1.4 USSR countries have a 12 hour default Rainfall Period
        ELSEIF (BLOCK.EQ.89 .OR.                     !1.4 Antarctic
     &          (BLOCK.GE.20.AND.BLOCK.LE.39).OR.    !1.5 Former USSR
     &          (REGION.EQ.2 .AND.                   !1.4 Asia but
     &  .NOT.(BLOCK.GE.50 .AND. BLOCK.LE.59))) THEN  !1.4 Not China
          PERIOD = -12.                              !1.4

!1.4 At 12z the rest of the world has a default Rainfall Period of
!1.4 6 hours.
        ELSE                                        !1.3 Everyone else
          PERIOD= -6.
        ENDIF
*
      ELSE IF (HOUR.EQ.18) THEN

!1.4 At 18z Iceland has a default Rainfall Period of 9 hours
        IF (BLOCK.EQ.4 .AND. STNNUM.GE.000 .AND.    !1.4 Iceland
     &                       STNNUM.LE.199) THEN    !1.4
          PERIOD= -9.                               !1.3

!1.4 At 18z the Antartic has a default rainfall period of 18 hours
        ELSEIF (BLOCK.EQ.89) THEN                   !1.3 Antarctic,
          PERIOD= -18.                              !1.3

!1.4 At 18z Africa, Former USSR and Europe have a default Rainfall
!1.4 period of 12 hours
        ELSEIF (REGION.EQ.1 .OR.                   !1.4 Africa
     &          (BLOCK.GE.20 .AND. BLOCK.LE.38)    !1.4 Former USSR
     &      .OR. REGION.EQ.6) THEN                 !1.4 Europe
          PERIOD= -12.

!1.4 At 18z everyone else in world has a default rainfall Period
!1.4 of 6 hours
        ELSE
          PERIOD= -6.
        ENDIF
      ENDIF
*
!1.3 For Australia and New Zealand rainfall could be reported at
!1.3 at an hour before or after the main hour
      IF (REGION.EQ.5 .AND.                         !1.4 Region 5
     &    BLOCK.GE.92 .AND. BLOCK.LE.95) THEN       !1.4 Aus.& NZ
        IF (HOUR.EQ.0 .OR. HOUR.EQ.23 .OR.          !1.4 00Z ob
     &       HOUR.EQ.1) THEN                        !1.4
          PERIOD = -24.                             !1.3
        ELSE                                        !1.3 all other times
          PERIOD = -6.                              !1.3
        ENDIF                                       !1.3
      ENDIF                                         !1.3

      RETURN
      END
