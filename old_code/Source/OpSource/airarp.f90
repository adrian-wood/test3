SUBROUTINE AIRARP(REPLEN,REPORT,TAILNO,YYGGGG,CCCC, &
TTAAII,NFTAIR,NFTBCN,SPAN)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRARP
!
! PURPOSE       : This subroutine calls the various decode routines
!                 required for AIREPs!
!
! DESCRIPTION   : After a single report has been passed by AIRBUL
!                 this routine passes the report through the various
!                 decode routines for AIREPs. The returned values
!                 are then passed to AIRENC which controls the
!                 storage routines.
!
! CALLS         : AIRSGN  AIRPOS  AIRTIM  AIRLEV  AIRELM  AIROPT
!                 AIRSET  AIRENC
!
! CALLED BY     : AIRBUL
!
! PARAMETERS    : 1. REPLEN - length of report              - I
!                 2. REPORT - report                        - I/O
!                 3. TAILNO - tail number                   - I
!                 4. YYGGGG - bulletin date/time            - I
!                 5. CCCC   - collecting centre             - I
!                 6. TTAAII - from bulletin hedaer          - I
!                 7. NFTAIR - FT no for AIREPS storage      - I
!                 8. NFTBCN - FT no for BEACONS data set    - I
!                 9. time span of reports in bulletin (for AIRIND)
!
! REVISION INFO :
!
! $Workfile: airarp.f90$ $Folder: OpSource$
! ›Revision: 7› ›Date: 16/03/2011 12:24:53›
!
! $Log:
!  8    MetDB_Refresh 1.7         19/04/2011 16:35:13    Brian Barwell   Check
!       that tail number can be found in message. Some tidying up of source.
!  7    MetDB_Refresh 1.6         16/03/2011 12:24:53    Alison Weir     Change
!        REPLEN intent to INOUT
!  6    MetDB_Refresh 1.5         25/01/2011 15:41:07    Richard Weedon  minor
!       corrections made
!  5    MetDB_Refresh 1.4         17/01/2011 15:05:38    Richard Weedon
!       copyright date changed to 2011.
!  4    MetDB_Refresh 1.3         17/01/2011 15:03:23    Richard Weedon  END
!       SUBROUTINE added
!  3    MetDB_Refresh 1.2         11/01/2011 13:10:03    Richard Weedon  Final
!       version. All mod files added, compilation test completed succesfully
!  2    MetDB_Refresh 1.1         07/01/2011 16:22:11    Richard Weedon  INTENT
!        statements added, untested as mod files not yet complete
!  1    MetDB_Refresh 1.0         06/01/2011 17:15:25    Richard Weedon
!       Development Version. NOTE - Incomplete for this revision as work is in
!        progress!!!!
!       Changes made to param REPORT (set to INOUT) . Errors on compile, under
!        investigation.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
! Interfaces
 USE AIRSGN_mod
 USE AIRPOS_mod
 USE AIRTIM_mod
 USE AIRLEV_mod
 USE AIROPT_mod
 USE AIRSET_mod
 USE AIRENC_mod
 USE AIRELM_mod
!
IMPLICIT NONE
!
! Input parameters
CHARACTER(LEN=4),INTENT(IN)      ::  CCCC
CHARACTER(LEN=6),INTENT(IN)      ::  TTAAII
CHARACTER(LEN=120),INTENT(INOUT) ::  REPORT !len of bull of airep reps
CHARACTER(LEN=*),INTENT(IN)      ::  TAILNO !tail number
CHARACTER(LEN=6),INTENT(IN)      ::  YYGGGG
!
INTEGER,INTENT(IN)            ::  NFTAIR    !FT no. of storage dataset
INTEGER,INTENT(IN)            ::  NFTBCN    !FT no. for Beacons list ds
INTEGER,INTENT(INOUT)         ::  REPLEN    !len of rep being decoded
INTEGER,INTENT(IN)            ::  SPAN      !time span of obs in bull
!
!integers
INTEGER          ::  POINT      !position in report array
INTEGER          ::  DECERR     !decode error flag
INTEGER          ::  NFT        !no of FT unit
INTEGER          ::  LTAILNO    !length of tail number
INTEGER          ::  LSIGN      !length of call sign at start
INTEGER          ::  LDIFF      !=LTAILNO-LSIGN
INTEGER          ::  LREST      !length of text before tail number
INTEGER          ::  I          !loop variable
INTEGER          ::  START      !pointer to start of report
INTEGER          ::  TOR(5)     !Time of Receipt
INTEGER          ::  TEMP_FLAG  !used in melems and optelm
INTEGER          ::  WIND_FLAG  !used in melems and optelm
INTEGER          ::  MATCH
INTEGER          ::  ARYFLG

!declare real
REAL             ::  MID_DD
REAL             ::  MID_HH
REAL             ::  MID_MM
REAL             ::  MID_MTH
REAL             ::  MID_YY
REAL             ::  TIMEYY
REAL             ::  TIMEMNTH
REAL             ::  TIMEDD          !decoded day group
REAL             ::  TIMEHH          !decoded hour
REAL             ::  TIMEMM          !decoded mins
REAL             ::  WINDD           !decoded wind direction
REAL             ::  WINDS           !decoded wind speed
REAL             ::  AIR_ARAY1(18)   !elements array
REAL             ::  AIR_ARAY2(18)   !elements array with mid point
REAL             ::  LAT             !decoded latitude
REAL             ::  TEMP            !decoded air temperature
REAL             ::  LONG            !decode longitude
REAL             ::  LAT2
REAL             ::  LONG2
REAL             ::  LEVEL           !decoded flight level
REAL             ::  OPT_WIND        !opt elements wind dir decode
REAL             ::  OPT_WNDS        !opt elements wind speed decode
REAL             ::  OPT_TURB        !opt elements turbulence decode
REAL             ::  OPT_ICE         !opt elements icing decode
REAL             ::  OPT_TEMP        !opt elements temperature decode
REAL             ::  INDX_ARAY1(8)
REAL             ::  INDX_ARAY2(8)

!character
CHARACTER(LEN=8) ::  BEAC_NAME        !beacon name
CHARACTER(LEN=8) ::  SIGN             !callsign name

SAVE

!initialize variables

MATCH=0
POINT=1                  !points to start of report
DECERR=0
WIND_FLAG=1
TEMP_FLAG=1
LAT=-9999999.
LONG=-9999999.
LAT2=-9999999.
LONG2=-9999999.
LEVEL=-9999999.
TEMP=-9999999.
TIMEDD=-9999999.
TIMEHH=-9999999.
TIMEMM=-9999999.
MID_YY=-9999999.
MID_MTH=-9999999.
MID_DD=-9999999.
MID_HH=-9999999.
MID_MM=-9999999.
WINDD=-9999999.
WINDS=-9999999.
OPT_TEMP=-9999999.
OPT_WIND=-9999999.
OPT_WNDS=-9999999.
OPT_TURB=-9999999.
OPT_ICE=-9999999.
NFT=NFTAIR
BEAC_NAME='       '
SIGN='        '

!---------------------------------------------------------------------
!Remove ARP, AIREP etc at start (but see similar code in AIRSGN!)
!---------------------------------------------------------------------

START=1
IF (REPORT(1:4) == 'ARP ')    START=5
IF (REPORT(1:6) == 'AIREP ')  START=7
IF (REPORT(1:7) == 'AIREPS ') START=8
IF (REPORT(1:4) == 'POS ')    START=5
POINT=START

!----------------------------------------------------------------------
!Check length of report. Less than 23 characters long will mean that
!there is no useful data. We must at least obtain time/position/level.
!----------------------------------------------------------------------

 IF (REPLEN < 23) DECERR = 1

!----------------------------------------------------------------------
!All the routines have an error flag (DECERR) which is always zero
!if each routine has run properly. The value of DECERR will change
!if any AIREP group cannot be decoded properly. This will halt the
!rest of the decode and control will pass to the storage routine.
!This check stops the decode routines from trying to decode the wrong
!group.
!----------------------------------------------------------------------

IF (DECERR == 0) CALL AIRSGN (REPORT, REPLEN, POINT, SIGN)

!----------------------------------------------------------------------
!this subroutine returns the Lat/Long of the aircraft. If the aircraft
!used a Beacon name to identify its position then the Beacon name
!----------------------------------------------------------------------

IF (DECERR == 0) CALL AIRPOS (REPLEN, REPORT, POINT,  &
                 LAT, LONG, BEAC_NAME, NFTBCN, DECERR)

!----------------------------------------------------------------------
!This subroutine returns the day/time group in its component parts!
!----------------------------------------------------------------------

IF (DECERR == 0) CALL AIRTIM (REPLEN, REPORT, POINT, TIMEDD,  &
                 TIMEYY, TIMEMNTH, TIMEHH, TIMEMM, YYGGGG, DECERR)

!----------------------------------------------------------------------
!This subroutine decodes the flight level.
!----------------------------------------------------------------------

IF (DECERR == 0) CALL AIRLEV (REPLEN, REPORT, POINT, LEVEL, DECERR)

!----------------------------------------------------------------------
!this subroutine decodes the air temperature and wind groups. The
!wind group is split into the direction and the speed.
!----------------------------------------------------------------------

IF (DECERR == 0) CALL AIRELM (REPLEN, REPORT, POINT,  &
                 TEMP, WINDD, WINDS, WIND_FLAG, TEMP_FLAG)

!----------------------------------------------------------------------
!The following IF BLOCK checks to see if we have reached the end of the
!report. There is nothing to gain by calling AIROPT if we have.
!----------------------------------------------------------------------

IFCONSTR1 : &
IF (POINT < REPLEN .AND. DECERR == 0) THEN

!----------------------------------------------------------------------
!The subroutine AIROPT decodes the 'optional' elements groups.
!The AIRELM subroutine is used to decode wind and temp and two further
!modules can be called from within AIROPT to decode turbulence and
!icing if reported.
! If a tail number has been found (from a bulletin known to have
!tail numbers in addition to the usual identifiers), reset the
!call sign to the tail number - and assume there's no more data.
! Swap tail number & call sign in report text to be stored too.
!----------------------------------------------------------------------

  IFCONSTR2 : &
  IF (TAILNO /= ' ' .AND. TTAAII == 'UANT99' .AND. &
          (CCCC == 'KDDL' .OR. CCCC == 'CWAO')) THEN

! Find the lengths of SIGN (at start) & tail number, and of the
! text in between (length here includes space on end).

    LSIGN=INDEX(SIGN,' ')
    IF (LSIGN == 0) LSIGN=LEN(SIGN)
    LTAILNO=INDEX(TAILNO,' ')
    IF (LTAILNO == 0) LTAILNO=LEN(TAILNO)
    LREST=INDEX(REPORT,TAILNO(1:LTAILNO-1))

! Abandon the rest of this section if the tail number wasn't found
! IN the report. This can happen with some corrupt reports.

    GOODTAIL : IF (LREST > 0) THEN

! If sign & tail number have same length, they can just be swapped.
! Otherwise the rest of the text must be moved left or right,
! from the left to move left or from the right to move right.

      LDIFF=LTAILNO-LSIGN
      IF (LDIFF < 0) THEN
        DO I=LSIGN,LREST-1
          REPORT(I+LDIFF:I+LDIFF)=REPORT(I:I)
        END DO
      ELSE IF (LDIFF > 0) THEN
        DO I=LREST-1,LSIGN,-1
          REPORT(I+LDIFF:I+LDIFF)=REPORT(I:I)
        END DO
      END IF
      REPORT(1:LTAILNO)=TAILNO
      REPORT(LREST+LDIFF:LREST+LDIFF+LSIGN-2)=SIGN

      SIGN=TAILNO(:LTAILNO)
    END IF GOODTAIL
  ELSE IFCONSTR2
     CALL AIROPT(REPLEN,REPORT,POINT,TIMEDD,TIMEHH,TIMEMM,LAT,LAT2,&
     LONG,LONG2,OPT_TEMP,OPT_WIND,OPT_WNDS,OPT_TURB,OPT_ICE,MID_YY,&
     MID_MTH,MID_DD,MID_HH,MID_MM,SIGN,MATCH,NFT,NFTBCN,DECERR)
  END IF IFCONSTR2
END IF IFCONSTR1

!----------------------------------------------------------------------
!The subroutine AIRSET stores the retrieved elements in an array
!ready for BUFR encoding.
!----------------------------------------------------------------------

IF (DECERR == 0) THEN
  CALL AIRSET(LAT,LONG,LAT2,LONG2,MATCH,TIMEYY,TIMEMNTH,TIMEDD,&
 TIMEHH,TIMEMM,LEVEL,TEMP,WINDD,WINDS,OPT_TEMP,OPT_WIND,       &
 OPT_WNDS,OPT_TURB,OPT_ICE,MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM,&
 ARYFLG,AIR_ARAY1,AIR_ARAY2,INDX_ARAY1,INDX_ARAY2)
END IF

!----------------------------------------------------------------------
!This section calls the routine for indexing and storage
!----------------------------------------------------------------------

IF (DECERR == 0) THEN
 CALL AIRENC(AIR_ARAY1,AIR_ARAY2,ARYFLG,INDX_ARAY1,INDX_ARAY2,&
      SIGN,BEAC_NAME,REPORT(START:),REPLEN-START+1,&
      SPAN,CCCC,NFTAIR,TOR,TTAAII)
END IF
RETURN
END SUBROUTINE AIRARP
