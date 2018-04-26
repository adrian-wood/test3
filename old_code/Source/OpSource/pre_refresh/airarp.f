      SUBROUTINE AIRARP(REPLEN,REPORT,TAILNO,YYGGGG,CCCC,
     &                  TTAAII,NFTAIR,NFTBCN,SPAN)                !2.1a

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
! CALLS         : AIRSGN
!                 AIRPOS
!                 AIRTIM
!                 AIRLEV
!                 AIRELM
!                 AIROPT
!                 AIRSET
!                 AIRENC
!
! CALLED BY     : AIRBUL
!
! PARAMETERS    : 1. REPLEN - length of report              - I
!                 2. REPORT - report                        - I
!                 3. TAILNO - tail number                   - I    !2.1
!                 4. YYGGGG - bulletin date/time            - I
!                 5. CCCC   - collecting centre             - I
!                 6. TTAAII - from bulletin hedaer          - I
!                 7. NFTAIR - FT no for AIREPS storage      - I
!                 8. NFTBCN - FT no for BEACONS data set    - I
!                 9. time span of reports in bulletin (for AIRIND) !2.1a
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:34$
! $Source: /data/us0400/mdb/op/lib/source/RCS/airarp.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:34    Sheila Needham  
! $
! Revision 2.1  2002/03/07  15:55:57  15:55:57  usmdb (Generic MetDB account)
! 18 March 2002      C Long
! 2.1  Pass tail number, to replace call sign if bulletin heading fits.
!      Initialise variables at start only rather than at start and end.
!      Avoid risky character assignments (overlapping strings).
! 2.1a Pass bulletin time span through to AIRENC/IND.
! 
! Revision 2.0  2001/05/31  13:27:18  13:27:18  usmdb (Generic MetDB account)
! Removed unused variable ICCC, Moved data statement to be before
! executable section. Removed unused argument CORF. Removed
! unused arguments IN, CORF to AIRENC as not used in AIRENC.
! Added copyright and modified header - S.Cox
!
! Revision 1.4  98/02/04  14:56:12  14:56:12  usmdb (Generic MetDB account)
! Check for spurious groups at the start of an AIREP report.
!
! Revision 1.3  1997/09/10 15:55:41  uspm
! Additional check added before call to AIROPT to ensure
! that DECERR hasn't been set to indicate an error
!
! Revision 1.2  1997/07/31 09:03:23  uspm
! First revision for COSMOS
!
! Revision 1.1  1997/07/03 13:34:00  uspm
! Initial revision
!
! 02.02.98 : Check for spurious groups at the start of an AIREP
!            report.                                                  !c
!
! 15.09.97 : Additional check added before call to AIROPT to ensure
!            that DECERR hasn't been set to indicate an error -JL     !B
!
!  Sept 96 : Allow 8th element in index arrays for count of values    !a
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

!integers
      INTEGER REPLEN           !length of report being decoded
      INTEGER POINT            !position in report array
      INTEGER SPAN             !time span of obs in bulletin       !2.1a
      INTEGER DECERR           !decode error flag
      INTEGER NFT              !no of FT unit
      INTEGER NFTAIR           !FT no. of storage dataset
      INTEGER NFTBCN           !FT no. for Beacons list dataset
      INTEGER LTAILNO          !length of tail number              !2.1
      INTEGER LSIGN            !length of call sign at start       !2.1
      INTEGER LDIFF            !=LTAILNO-LSIGN                     !2.1
      INTEGER LREST            !length of text before tail number  !2.1
      INTEGER I                !loop variable                      !2.1
      INTEGER START            !pointer to start of report         !2.1
      INTEGER TOR(5)           !Time of Receipt
      INTEGER TEMP_FLAG        !used in melems and optelm
      INTEGER WIND_FLAG        !used in melems and optelm
      INTEGER MATCH
      INTEGER ARYFLG

!declare real
      REAL    MID_DD
      REAL    MID_HH
      REAL    MID_MM
      REAL    MID_MTH
      REAL    MID_YY
      REAL    TIMEYY
      REAL    TIMEMNTH
      REAL    TIMEDD           !decoded day group
      REAL    TIMEHH           !decoded hour
      REAL    TIMEMM           !decoded mins
      REAL    WINDD            !decoded wind direction
      REAL    WINDS            !decoded wind speed
      REAL    AIR_ARAY1(18)   !elements array
      REAL    AIR_ARAY2(18)  !elements array with mid point
      REAL    LAT              !decoded latitude
      REAL    TEMP             !decoded air temperature
      REAL    LONG             !decode longitude
      REAL    LAT2
      REAL    LONG2
      REAL    LEVEL            !decoded flight level
      REAL    OPT_WIND         !optional elements wind direction decode
      REAL    OPT_WNDS         !optional elements wind speed decode
      REAL    OPT_TURB         !optional elements turbulence decode
      REAL    OPT_ICE          !optional elements icing decode
      REAL    OPT_TEMP         !optional elements temperature decode
      REAL    INDX_ARAY1(8)                                          !a
      REAL    INDX_ARAY2(8)                                          !a

!character
      CHARACTER*6 YYGGGG
      CHARACTER*120 REPORT     !length of bulletin of airep reports
      CHARACTER*8   BEAC_NAME  !beacon name
      CHARACTER*8   SIGN       !callsign name
      CHARACTER*6   TTAAII
      CHARACTER*4   CCCC
      CHARACTER*(*) TAILNO     ! tail number                       !2.1
      CHARACTER*132 HEAD       !revision information

      SAVE

!initialize variables
      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/airarp.F,v $
     &$Revision: 1$ $Date: 30/01/2006 20:20:34$'

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
!Remove ARP, AIREP etc at start (but see similar code in AIRSGN!)  !2.1
!---------------------------------------------------------------------

      START=1                                                      !2.1
      IF (REPORT(1:4).EQ.'ARP ')    START=5                        !2.1
      IF (REPORT(1:6).EQ.'AIREP ')  START=7                        !2.1
      IF (REPORT(1:7).EQ.'AIREPS ') START=8                        !2.1
      IF (REPORT(1:4).EQ.'POS ')    START=5                        !2.1
      POINT=START                                                  !2.1

!----------------------------------------------------------------------
!check length of report. less than 23 characters long will mean that
!there is no useful data. we must at least obtain time/position/level
!----------------------------------------------------------------------

       IF (REPLEN .LT. 23) THEN
         DECERR=1
       ENDIF

!----------------------------------------------------------------------
!All the routines have an error flag (DECERR) which is always zero
!if each routine has run properly. The value of DECERR will change
!if any AIREP group cannot be decoded properly. This will halt the
!rest of the decode and control will pass to the storage routine.
!This check stops the decode routines from trying to decode the wrong
!group
!----------------------------------------------------------------------

      IF (DECERR .EQ. 0) THEN
        CALL AIRSGN(REPORT,REPLEN,POINT,SIGN)
      ENDIF

!----------------------------------------------------------------------
!this subroutine returns the Lat/Long of the aircraft. If the aircraft
!used a Beacon name to identify its position then the Beacon name
!----------------------------------------------------------------------

       IF (DECERR .EQ. 0) THEN
        CALL AIRPOS(REPLEN,REPORT,POINT,LAT,LONG,
     &  BEAC_NAME,NFTBCN,DECERR)
      ENDIF

!----------------------------------------------------------------------
!This subroutine returns the day/time group in its component parts!
!----------------------------------------------------------------------

      IF (DECERR .EQ. 0) THEN
        CALL AIRTIM(REPLEN,REPORT,POINT,TIMEDD,TIMEYY,TIMEMNTH,
     &  TIMEHH,TIMEMM,YYGGGG,DECERR)
      ENDIF

!----------------------------------------------------------------------
!This subroutine decodes the flight level.
!----------------------------------------------------------------------

      IF (DECERR .EQ. 0) THEN
        CALL AIRLEV(REPLEN,REPORT,POINT,LEVEL,DECERR)
      ENDIF

!----------------------------------------------------------------------
!this subroutine decodes the air temperature and wind groups. The
!wind group is split into the direction and the speed.
!----------------------------------------------------------------------

      IF (DECERR .EQ. 0) THEN
        CALL AIRELM(REPLEN,REPORT,POINT,TEMP,WINDD,
     &  WINDS,WIND_FLAG,TEMP_FLAG)
      ENDIF

!----------------------------------------------------------------------
!The following IF BLOCK checks to see if we have reached the end of the
!report. There is nothing to gain by calling AIROPT if we have.
!----------------------------------------------------------------------

      IF (POINT .LT. REPLEN .AND. DECERR .EQ. 0) THEN                !B

!----------------------------------------------------------------------
!The subroutine AIROPT decodes the 'optional' elements groups.
!The AIRELM subroutine is used to decode wind and temp and two further
!modules can be called from within AIROPT to decode turbulence and
!icing if reported.
! If a tail number has been found (from a bulletin known to have   !2.1
!tail numbers in addition to the usual identifiers), reset the     !2.1
!call sign to the tail number - and assume there's no more data.   !2.1
! Swap tail number & call sign in report text to be stored too.    !2.1
!----------------------------------------------------------------------

        IF (TAILNO.NE.' ' .AND. TTAAII.EQ.'UANT99' .AND.           !2.1
     &     (CCCC.EQ.'KDDL' .OR. CCCC.EQ.'CWAO')) THEN              !2.1

! Find the lengths of SIGN (at start) & tail number, and of the    !2.1
! text in between (length here includes space on end).             !2.1

          LSIGN=INDEX(SIGN,' ')                                    !2.1
          IF (LSIGN.EQ.0) LSIGN=LEN(SIGN)                          !2.1
          LTAILNO=INDEX(TAILNO,' ')                                !2.1
          IF (LTAILNO.EQ.0) LTAILNO=LEN(TAILNO)                    !2.1
          LREST=INDEX(REPORT,TAILNO(1:LTAILNO-1))                  !2.1
          LDIFF=LTAILNO-LSIGN                                      !2.1

! If sign & tail number have same length, they can just be swapped.!2.1
! Otherwise the rest of the text must be moved left or right,      !2.1
! from the left to move left or from the right to move right.      !2.1

          IF (LDIFF.LT.0) THEN                                     !2.1
            DO I=LSIGN,LREST-1                                     !2.1
              REPORT(I+LDIFF:I+LDIFF)=REPORT(I:I)                  !2.1
            ENDDO                                                  !2.1
          ELSE IF (LDIFF.GT.0) THEN                                !2.1
            DO I=LREST-1,LSIGN,-1                                  !2.1
              REPORT(I+LDIFF:I+LDIFF)=REPORT(I:I)                  !2.1
            ENDDO                                                  !2.1
          ENDIF                                                    !2.1
          REPORT(1:LTAILNO)=TAILNO                                 !2.1
          REPORT(LREST+LDIFF:LREST+LDIFF+LSIGN-2)=SIGN             !2.1

          SIGN=TAILNO(:LTAILNO)                                    !2.1
        ELSE                                                       !2.1
          CALL AIROPT(REPLEN,REPORT,POINT,TIMEDD,TIMEHH,TIMEMM,LAT,LAT2,
     &    LONG,LONG2,OPT_TEMP,OPT_WIND,OPT_WNDS,OPT_TURB,OPT_ICE,MID_YY,
     &    MID_MTH,MID_DD,MID_HH,MID_MM,SIGN,MATCH,NFT,NFTBCN,DECERR)
        ENDIF                                                      !2.1
      ENDIF

!----------------------------------------------------------------------
!The subroutine AIRSET stores the retrieved elements in an array
!ready for BUFR encoding.
!----------------------------------------------------------------------

      IF (DECERR .EQ. 0) THEN
        CALL AIRSET(LAT,LONG,LAT2,LONG2,MATCH,TIMEYY,TIMEMNTH,TIMEDD,
     &  TIMEHH,TIMEMM,LEVEL,TEMP,WINDD,WINDS,OPT_TEMP,OPT_WIND,
     &  OPT_WNDS,OPT_TURB,OPT_ICE,MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM,
     &  ARYFLG,AIR_ARAY1,AIR_ARAY2,INDX_ARAY1,INDX_ARAY2)
      ENDIF

!----------------------------------------------------------------------
!This section calls the routine for indexing and storage
!----------------------------------------------------------------------

      IF (DECERR .EQ. 0) THEN
        CALL AIRENC(AIR_ARAY1,AIR_ARAY2,ARYFLG,INDX_ARAY1,INDX_ARAY2,
     &              SIGN,BEAC_NAME,REPORT(START:),REPLEN-START+1,
     &              SPAN,CCCC,NFTAIR,TOR,TTAAII)                  !2.1a
      ENDIF
      RETURN
      END
