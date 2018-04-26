      SUBROUTINE AIROPT(REPLEN,REPORT,POINT,TIMEDD,TIMEHH,TIMEMM,LAT,
     &LAT2,LONG,LONG2,OPT_TEMP,OPT_WIND,OPT_WNDS,OPT_TURB,OPT_ICE,
     &MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM,SIGN,MATCH,NFT,NFTBCN,DECERR)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIROPT
!
! PURPOSE       : SEARCH FOR OPTIONAL ELEMENT GROUPS IN AIREP
!
! DESCRIPTION   : LOOKS FOR CERTAIN COMBINATIONS OF WORDS AND LETTERS
!                 TO DETERMINE A PARTICULAR ELEMENT GROUP. ONCE FOUND
!                 THE RELEVANT DECODE ROUTINE IS CALLED. THE SEARCH
!                 WILL CONTINUE FOR A MAXIMUM 4 LOOPS OR UNTIL THE
!                 END OF THE REPORT IS REACHED
!
! CALLED BY     : AIRARP
!
! CALLS TO      : AIRELM
!                 AIRICE
!                 AIRTRB
!                 AIRLOC
!                 AIRMID                                          !1.10
!                 AIRPOS                                          !1.10
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:43$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airopt.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:43    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:23  usmdb
! Removed unused variables. Initialised LAT_FLAG to zero.
! Removed argument REPLEN from call to AIRTRB as not used in
! AIRTRB. Added copyright and modified header - S.Cox
!
! Revision 1.10  99/07/12  16:13:31  16:13:31  usmdb (Generic MetDB account)
! 19 July 1999    C Long
! 1.10 If a longitude (or latitude) is reported with MID, check the
!      calculated midpoint against this & give up if they disagree.
! 
! Revision 1.9  99/04/12  10:57:00  10:57:00  usmdb (Generic MDB account)
! 19 April 1999     C Long
! 1.9 Set wind flag for AIRELM call and remove some useless IFs
!
! Revision 1.8  98/09/16  16:11:01  16:11:01  usmdb (Generic MDB account)
! 21/09/1998 Additional checks to stop 'out-of-bounds' conditions.
!
! Revision 1.7  98/07/23  08:03:51  08:03:51  usmdb (Generic MDB account)
! Remove diagnostic write statements                                  !C
!
! Revision 1.6  98/02/04  14:49:39  14:49:39  usmdb (Generic MDB account)
! Restructure routine to better cope with MID point data
!
! Revision 1.5  1997/09/22 13:35:38  uspm
! Correct call to 2nd call to AIRPOS - pass NFTBCN
!
! Revision 1.4  1997/07/31 09:07:48  uspm
! First revision for  1
!
! Revision 1.3  1997/07/03 14:04:50  uspm
! Latest version from  1  - with Y2K check
!
! 23/06/97 : Correct call to AIRLOC in turbulence decode seciton      !A
!
! Revision 1.2  1997/06/05 14:59:10  uspm
! Correct call to getloc - greplen had not been passed
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

!declare integers
      INTEGER REPLEN          !Report Length
      INTEGER POINT,COUNT     !Position within report
      INTEGER WIND_FLAG       !Flag indicates wind group found
      INTEGER MID_FLAG        !Flag indicates if MiD group found
      INTEGER TURB_FLAG       !Flag indicates if Turbulence group found
      INTEGER ICE_FLAG        !Flag indicates if icing group found
      INTEGER TEMP_FLAG       !Flag indicates if temperature group found
      INTEGER END_FLAG        !Flag indicates if end of report
      INTEGER ATP_FLAG        !Flag for At this Point indicator
      INTEGER MID_OLD         !Flag to indicate if group decoded
      INTEGER WIND_OLD        !Flag to indicate if group decoded
      INTEGER ATP_OLD         !Flag to indicate if group decoded
      INTEGER TEMP_OLD        !Flag to indicate if group decoded
      INTEGER TURB_OLD        !Flag to indicate if group decoded
      INTEGER ICE_OLD         !Flag to indicate if group decoded
      INTEGER LAT_FLAG
      INTEGER LAT_OLD
      INTEGER DECERR          !Decode error flag
      INTEGER GRPLEN          !Length of single group within report
      INTEGER NFTBCN
      INTEGER NFT,ind2
      INTEGER MATCH
      INTEGER GROUPS
      INTEGER I
      INTEGER IND
      INTEGER TEST
      INTEGER MIDZ                                                 !1.9
      INTEGER NEWZ                                                 !1.9

!declare real
      REAL    MID_MM
      REAL    MID_MTH
      REAL    MID_YY
      REAL    MID_DD
      REAL    MID_HH
      REAL    WHL_LAT
      REAL    WHL_LONG
      REAL    TIMEDD          !Day of current report
      REAL    TIMEHH          !Hour of current report
      REAL    TIMEMM          !Mins of current report
      REAL    CALC_LAT
      REAL    CALC_LONG
      REAL    LAT
      REAL    LONG
      REAL    TEMP            !Decoded temperature group
      REAL    LAT2            !Second Latitude
      REAL    LONG2           !Second Longitude
      REAL    OPT_TEMP        !Decoded optional temp group
      REAL    OPT_WIND        !Decode optional wind direction group
      REAL    OPT_WNDS        !Decoded optional wind speed group
      REAL    OPT_TURB        !Decoded option turb group
      REAL    OPT_ICE         !Decoded optional ice group
      REAL    TLAT
      REAL    TLONG
      REAL    MISSING

!declare character
      CHARACTER*(*) REPORT    !Airep report
      CHARACTER*8 BEAC_NAME   !Becaon name used in call to AIRPOS
      CHARACTER*8 SIGN
      CHARACTER*132 HEAD      !Revision information

!declare logical
      LOGICAL LREPFL          !Flag .TRUE. if end of report in AIRLOC
      LOGICAL DEBUG           !Used for debugging
      LOGICAL GEN_MID
      LOGICAL LLCORD

!initialize variables
      MISSING=-9999999.
      CALC_LAT=-9999999.
      CALC_LONG=-9999999.
      OPT_TEMP=-9999999.
      OPT_ICE=-9999999.
      OPT_TURB=-9999999.
      OPT_WNDS=-9999999.
      OPT_WIND=-9999999.
      WHL_LAT=-9999999.
      WHL_LONG=-99999999.
      MID_FLAG=0
      WIND_FLAG=0
      TEMP_FLAG=0
      ICE_FLAG=0
      TURB_FLAG=0
      END_FLAG=0
      LAT_FLAG=0                                                    !2.0
      MID_OLD=0
      WIND_OLD=0
      ATP_OLD=0
      TEMP_OLD=0
      TURB_OLD=0
      ICE_OLD=0
      LAT_OLD=0
      GROUPS=0
      LAT2=-9999999.
      LONG2=-9999999.
      COUNT=0
      DEBUG=.FALSE.
      LREPFL=.FALSE.
      BEAC_NAME='AIROPT'
      GEN_MID=.false.
      LLCORD=.FALSE.

      i=0
      IND=0
      IND2=0
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airopt.F,v $
     &'//' $Date: 30/01/2006 20:20:43$ $Revision: 1$'

!----------------------------------------------------------------------
!Loop round and identify as many groups as possible. We only need
!one wind/temp/turb and icing group here - so a flag is set to indicate
!if one found.
!----------------------------------------------------------------------

       IF (REPORT(POINT:POINT) .EQ. '=') THEN  !Check for End of report
         END_FLAG=1
       ENDIF

!----------------------------------------------------------------------
!Estimate the number of optional groups to decode
!----------------------------------------------------------------------

       TEST=POINT-2
       DO WHILE (TEST .LE. REPLEN)
         IF (REPORT(test:test) .EQ. ' ') THEN                   !B
           GROUPS=GROUPS+1                                            !B
           TEST=TEST+1
         ELSEIF (REPORT(test:test) .EQ. '=') THEN
           TEST=REPLEN+1
         ELSE
           TEST=TEST+1
         ENDIF                                                        !B
       ENDDO                                                          !B

!----------------------------------------------------------------------
!Since the optional groups are at the end of an airep report the loop
!continues until the end delimeter '=' is found.
!----------------------------------------------------------------------

       DO WHILE (END_FLAG .EQ. 0 .AND. COUNT .LE. GROUPS)             !B

!----------------------------------------------------------------------
!The MID flag checks to see if the string 'MID' is present in the report
!----------------------------------------------------------------------

         IF (MID_FLAG .EQ. 0) THEN
           IF (REPORT(POINT:POINT+2) .EQ. 'MID') THEN
             GEN_MID=.TRUE.
             MID_FLAG=1
           ENDIF                                                      !B
         ENDIF                                                        !B

!----------------------------------------------------------------------
!----------------------------------------------------------------------

         IF ((REPORT(POINT:POINT).LT.'A') .OR.
     &       (REPORT(POINT:POINT).GT.'Z')) THEN
           IND=INDEX(REPORT(POINT:POINT+12),' ')-1                    !B
           IND=IND+POINT-1
           IF (IND .GT. REPLEN) THEN
             IND=REPLEN
           ENDIF
             IF (((REPORT(IND:IND) .EQ. 'N') .OR.
     &       (REPORT(IND:IND) .EQ. 'S') .OR.
     &       (REPORT(IND:IND) .EQ. 'E') .OR.
     &       (REPORT(IND:IND) .EQ. 'W')) .AND.
     &       LAT_FLAG.EQ.0) THEN
               CALL AIRPOS(REPLEN,REPORT,POINT,LAT2,LONG2,
     &         BEAC_NAME,NFTBCN,DECERR)
               IF ((lat2 .gt. -99) .and. (long2 .gt. -99)) then       !B
                 GEN_MID=.FALSE.                                      !B
                 LLCORD=.TRUE.
               ELSE
                 GEN_MID=.TRUE.
               ENDIF
             ELSE IF (REPORT(IND:IND).EQ.'=' .AND.                 !1.9
     &           IND-POINT+1.GE.3 .AND. WIND_FLAG.EQ.0) THEN       !1.9
               WIND_FLAG=1                                         !1.9
               CALL AIRELM(REPLEN,REPORT,POINT,TEMP,OPT_WIND,OPT_WNDS,
     &         WIND_FLAG,0)
               IF ((OPT_WIND .GE. 0) .AND. (OPT_WIND .LE. 360)) THEN
                 WIND_FLAG=1
                 IF (.NOT. LLCORD) THEN
                   GEN_MID=.TRUE.
                 ENDIF
               ENDIF
             ENDIF
         ENDIF

!----------------------------------------------------------------------
!   Look for an at this point (atp) indicator
!----------------------------------------------------------------------

         IF (ATP_FLAG .EQ. 0) THEN
           IF (REPORT(POINT:POINT+2) .EQ. 'ATP') THEN
             ATP_FLAG=1
             CALL AIRPOS(REPLEN,REPORT,POINT,LAT2,LONG2,BEAC_NAME,
     &       NFTBCN,DECERR)
             IF (DECERR .EQ. 0) THEN
               CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)
             ENDIF
           ENDIF
         ENDIF


!----------------------------------------------------------------------
!  Look for temperature group
!----------------------------------------------------------------------

         IF (TEMP_FLAG .EQ. 0) THEN
           IF (((REPORT(POINT:POINT) .EQ. 'M') .OR.
     &     (REPORT(POINT:POINT) .EQ. 'P')).AND.
     &     (REPORT(POINT:POINT+3) .NE. 'MID')) THEN
             TEMP_FLAG=1
             CALL AIRELM(REPLEN,REPORT,POINT,OPT_TEMP,OPT_WIND,OPT_WNDS,
     &       0,TEMP_FLAG)
             TEMP_FLAG=1
             IF (.NOT. LLCORD) THEN
               GEN_MID=.TRUE.
             ENDIF
           ENDIF
         ENDIF


!----------------------------------------------------------------------
!   Look for turbulence group
!----------------------------------------------------------------------

         IF (TURB_FLAG .EQ. 0) THEN
           IF ((REPORT(POINT:POINT) .EQ. 'T') .OR.
     &     (REPORT(POINT:POINT+2) .EQ. 'LTT') .OR.
     &     (REPORT(POINT:POINT+3) .EQ. 'LT T') .OR.
     &     (REPORT(POINT:POINT+3) .EQ. 'MODT') .OR.
     &     (REPORT(POINT:POINT+3) .EQ. 'CODE')) THEN
             TURB_FLAG=1
             CALL AIRTRB(REPORT,POINT,OPT_TURB)                    !2.0
             CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)          !A
           ENDIF
         ENDIF

!----------------------------------------------------------------------
!   Look for icing group
!----------------------------------------------------------------------

         IF (ICE_FLAG .EQ. 0) THEN
           IF((REPORT(POINT:POINT) .EQ. 'I') .OR.
     &     (REPORT(POINT:POINT+1) .EQ. 'LI')) THEN
             ICE_FLAG=1
             CALL AIRICE(REPORT,POINT,OPT_ICE)
           ENDIF
         ENDIF

!----------------------------------------------------------------------
! Check for end of report
!----------------------------------------------------------------------

         IF (REPORT(POINT:POINT) .EQ. '=') THEN
           END_FLAG=1
         ENDIF

!----------------------------------------------------------------------
!Check to see if any group has been decoded this time round. If there
!hasnt been something decoded move the pointer onto the next group
!----------------------------------------------------------------------

        IF ((MID_FLAG .EQ. MID_OLD) .AND. (ATP_FLAG .EQ. ATP_OLD) .AND.
     &  (TEMP_FLAG .EQ. TEMP_OLD) .AND. (WIND_FLAG .EQ. WIND_OLD) .AND.
     &  (TURB_FLAG .EQ. TURB_OLD) .AND. (ICE_FLAG .EQ. ICE_OLD) .AND.
     &  (LAT_FLAG .EQ. LAT_OLD)) THEN
          CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)
        ELSE
          MID_OLD=MID_FLAG             !update all the flags to the
          ATP_OLD=ATP_FLAG            !current settings, since at least
          TEMP_OLD=TEMP_FLAG           !one flag will have changed.
          WIND_OLD=WIND_FLAG
          TURB_OLD=TURB_FLAG
          ICE_OLD=ICE_FLAG
        ENDIF

        COUNT=COUNT+1
      ENDDO

!----------------------------------------------------------------------
!If we have been unable to resolve a Lat/Long position for the MID
!point data we can generate a position by looking at previously stored
!reports and the current position.
! AIRMID tries to find the previous report from this aircraft     !1.10
!and returns a midpoint lat/long if succesful.  If a second       !1.10
!latitude or longitude was reported (usually only longitude),     !1.10
!it is kept as TLONG (or TLAT) and the calculated midpoint        !1.10
!checked against it.  If midpoint latitude & longitude are        !1.10
!both reported, they are put with the calculated time without     !1.10
!any check - dangerous?                                           !1.10
!----------------------------------------------------------------------
      IF ((WIND_FLAG .GT. 0) .OR. (TEMP_FLAG .GT. 0) .OR.
     &    (MID_FLAG .GT. 0)) THEN
        TLAT=LAT2
        TLONG=LONG2
        CALL AIRMID(TIMEDD,TIMEHH,TIMEMM,MID_YY,MID_MTH,MID_DD,
     &  MID_HH,MID_MM,LAT,LONG,CALC_LAT,CALC_LONG,SIGN,MATCH,NFT)
        LAT2=CALC_LAT
        LONG2=CALC_LONG

! If MID_FLAG & GEN_MID are both set, MID may have been followed  !1.10
! by a longitude which can be checked against that from AIRMID:   !1.10
! give up if they disagree.                                       !1.10

        IF (GEN_MID) THEN
          IF ((LAT2 .LT. -999) .OR. (LONG2 .LT .-999)) THEN           !C
            MID_YY=MISSING                                            !C
          ELSE                                                    !1.10
            IF (TLONG.NE.MISSING .AND.                            !1.10
     &          ABS(TLONG-LONG2).GT.0.01) THEN                    !1.10
              MID_YY=MISSING                                      !1.10
              PRINT *,'AIROPT: midpoint found but longitude reported'
              PRINT *,' and the two are different:',TLONG,LONG2   !1.10
            ENDIF                                                 !1.10
            IF (TLAT.NE.MISSING .AND.                             !1.10
     &          ABS(TLAT-LAT2).GT.0.01) THEN                      !1.10
              MID_YY=MISSING                                      !1.10
              PRINT *,'AIROPT: midpoint found but latitude reported'
              PRINT *,' and the two are different:',TLAT,LAT2     !1.10
            ENDIF                                                 !1.10
          ENDIF
        ELSE
          IF (TIMEDD .GT. -9999) THEN
            LAT2=TLAT
            LONG2=TLONG
          ELSE
            MID_YY=MISSING
            LAT2=MISSING
            LONG2=MISSING
          ENDIF
        ENDIF

        IF (MID_YY.NE.MISSING) THEN                               !1.9
          MIDZ=MID_HH*100+MID_MM                                  !1.9
          NEWZ=TIMEHH*100+TIMEMM                                  !1.9
          WRITE (*,1) SIGN,MIDZ,NEWZ,LAT2,LONG2                   !1.9
    1      FORMAT (' ',A9,' midpoint report at ',I4.4,'Z from ',
     &             I4.4,'Z ob. Lat/long:',2F7.2)                  !1.9
        ENDIF
      ENDIF
      DECERR=0

!----------------------------------------------------------------------
!Returns to AIRARP
!----------------------------------------------------------------------
      RETURN
      END
