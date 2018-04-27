      SUBROUTINE STAPOS (REQ_WMONO, TYPE, LAT, LONG, HGPT, HGT, ISTAT)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! SUBROUTINE  : STAPOS
!
! PURPOSE     : TO RETURN STATION DETAILS FOR A GIVEN WMO STATION
!               NUMBER AND STATION TYPE.
!
! DESCRIPTION : A BINARY SEARCH IS MADE OF WMO STATION NUMBERS IN
!               THE ABBREVIATED STATION LIST FOR A GIVEN STATION.
!               DETAILS FOR AN ENTRY FOR WITH THE REQUIRED STATION
!               TYPE ARE LOCATED IF PRESENT AND ARE RETURNED TO THE
!               CALLING PROGRAM. A RETURN CODE IS SET > 0 IF DETAILS
!               COULD NOT BE FOUND.
!
! USAGE       : CALL STAPOS
!                      (REQ_WMONO, TYPE, LAT, LONG, HGPT, HGT, ISTAT)
!
! PARAMETERS  :   NAME    I/O  TYPE      CONTENT
!                 ----    ---  ----      -------
!               REQ_WMONO  I  (I*4) TARGET WMO STATION NUMBER
!               TYPE       I  (C*(*)) TARGET STATION TYPE (SEE NOTE 1)
!               LAT        O  (R*4) LATITUDE OF TARGET STATION
!               LON        O  (R*4) LONGITUDE OF TARGET STATION
!               HGPT       O  (R*4) P. SENSOR HEIGHT OF TARGET STN.
!               HGT        O  (R*4) SURFACE HEIGHT OF TARGET STATION
!               ISTAT      O  (I*4) RETURN CODE (SEE NOTE 2)
!
!               NOTES: (1) STATION TYPE IS CODED AS A TEXT STRING
!                          STARTING WITH "S", "U" OR "X" WHERE
!                          S=SURFACE, U=UPPER AIR, X=UNSPECIFIED.
!
!                      (2) RETURN CODE IS 0 FOR SUCCESSFUL SEARCH, 8
!                          FOR STATION NOT FOUND, OR 16 (WITH MESSAGE)
!                          FOR BAD CODING OF "REQ_MONO" OR "TYPE".
!
! CALLED BY   : USER'S PROGRAM
!
! CALLS       : STARAY
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:29$
! $Source: /home/us0400/mdb/op/lib/source/RCS/stapos.F,v $
!
! CHANGE RECORD :
!
! THE FIRST VERSION OF THIS ROUTINE WAS BY J LEWTHWAITE AND WAS DATED
! 6 FEBRUARY 1996. IT WAS COMPLETELY RE-WRITTEN BY BRIAN BARWELL IN MAY
! 1999 TO ALLOW BINARY SEARCHES FOR WMO STATION NUMBERS.
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:29    Sheila Needham  
! $
! Revision 2.1  2001/11/07 10:56:18  usmdb
! Enable retrieval of lats, longs and heights for WMO pseudo
! block 99 stations (currently all CDL stations) from the
! appropriate Abbreviated Station List.
! Change day 19NOV01   R HIRST
!
! Revision 2.0  2001/07/03  10:43:58  10:43:58  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.5  99/06/10  14:45:22  14:45:22  usmdb (Generic MetDB account)
! 21 June 1999, Infoman 63414, Brian Barwell
! New version with binary search of abbreviated station list.
!
! Revision 1.4  99/02/11  11:55:38  11:55:38  usmdb (Generic MDB account)
! 15th February 1999 John Norton
! Correct output of error message when station id out of
! range.
!
! Revision 1.3  97/11/20  09:54:50  09:54:50  usmdb (Generic MDB account)
! Increase the size of the UpperAir array to cope with
! additional stations.
!
! Revision 1.2  97/08/04  15:40:06  15:40:06  uspm (Pat McCormack)
! Add Y2K check comments
!
! Revision 1.1  1997/07/04 13:31:57  uspm
! Initial revision
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

!----------------------------------------------------------------------
!     DECLARE VARIABLES, INITIALISATIONS ETC.
!    (IF IT IS NECESSARY TO INCREASE THE NUMBER OF STATIONS WHICH CAN
!     BE HANDLED, ALTER THE NUMBER ON THE PARAMETER STATEMENT BELOW.
!     NO OTHER CHANGES TO THIS OR ANY OTHER ROUTINE WILL BE REQUIRED.)
!----------------------------------------------------------------------
!                                                        PARAMETERS
!
      INTEGER    MAXSTNS       ! MAXIMUM NUMBER OF STATIONS         !1.5
      INTEGER    MAXPLUS1      ! MAXIMUM NUMBER OF STATIONS + 1     !1.5
      PARAMETER (MAXSTNS=14000, MAXPLUS1=MAXSTNS+1)                 !1.5
!                                                          INTEGERS
!
      INTEGER ISTAT            ! ERROR CODE RETURNED TO USER        !1.5
      INTEGER IPOS             ! INTERNAL POINTER TO ARRAYS         !1.5
      INTEGER ITYPE            ! CODED "TYPE" (X,S,U = 1,2,3)       !1.5
      INTEGER JPOS, J1, J2     ! LOOP VARIABLE & LIMITS FOR SEARCH  !1.5
      INTEGER NUMSTNS          ! NO. OF STATIONS FOUND IN LIST      !1.5
      INTEGER NUMWMO           ! NO. OF DIFFERENT STATIONS IN LIST  !1.5
      INTEGER LISTWMO(MAXSTNS) ! WMO STATION NUMBERS IN LIST        !1.5
      INTEGER LOOKWMO(MAXSTNS) ! LIST OF DIFFERENT WMO NUMBERS      !1.5
      INTEGER NPOS(MAXPLUS1)   ! LOOKWMO/LISTWMO COLLATING POINTERS !1.5
      INTEGER NTYPE(MAXSTNS)   ! STATION TYPES (SEE ABOVE)          !1.5
      INTEGER REQ_WMONO        ! TARGET WMO STATION NUMBER          !1.5
!                                                             REALS
      REAL HGT, HGPT           ! STATION AND SENSOR HEIGHTS         !1.5
      REAL LAT, LONG           ! LAT & LONG OF TARGET STATION       !1.5
      REAL DEGLAT(MAXSTNS)     ! LATITUDES OF STATIONS              !1.5
      REAL DEGLON(MAXSTNS)     ! LONGITUDES OF STATIONS             !1.5
      REAL HTP(MAXSTNS)        ! PRESSURE SENSOR HEIGHTS OF STNS.   !1.5
      REAL HTSTN(MAXSTNS)      ! SURFACE HEIGHTS OF STATIONS        !1.5
!
!                                                        CHARACTERS
      CHARACTER*(*) TYPE       ! TARGET STATION TYPE                !1.5
      CHARACTER*132 HEAD       ! FOR REVISION INFORMATION           !1.5
!                                                           LOGICAL
!
      LOGICAL FIRST            ! FLAG FOR FIRST CALL TO STAPOS      !1.5
      DATA FIRST/.TRUE./                                            !1.5
!                        COMMON BLOCK (FOR DYNAMIC ALLOCATION ONLY)
!
      COMMON /COMSTNS/ LISTWMO, DEGLAT, DEGLON, HTP, HTSTN, NTYPE,  !1.5
     &                 LOOKWMO, NPOS                                !1.5
!                                                   SAVED VARIABLES
      SAVE /COMSTNS/, NUMSTNS, NUMWMO, FIRST                        !1.5
!                                              REVISION INFORMATION
      HEAD='                                                        !1.5
     &$Source: /home/us0400/mdb/op/lib/source/RCS/stapos.F,v $
     &'//'$ $Date: 30/01/2006 20:24:29$ $Revision: 1$'
!
!----------------------------------------------------------------------
!     IF THIS IS THE FIRST CALL TO STAPOS, READ THE ABBREVIATED
!     STATION LIST AND STORE DETAILS IN ARRAYS.
!----------------------------------------------------------------------
!
      IF (FIRST) THEN                                               !1.5
         NUMSTNS = MAXSTNS                                          !1.5
         CALL STARAY (LISTWMO, DEGLAT, DEGLON, HTP, HTSTN, NTYPE,   !1.5
     &                NUMSTNS, LOOKWMO, NPOS, NUMWMO)               !1.5
         NPOS(NUMWMO+1) = NUMSTNS + 1                               !1.5
         FIRST = .FALSE.                                            !1.5
      END IF                                                        !1.5
!
!----------------------------------------------------------------------
!     CHECK FOR VALID ICAO NUMBER AND REQUIRED STATION TYPE.
!----------------------------------------------------------------------
!
      IF (REQ_WMONO.LT.01001 .OR. REQ_WMONO.GT.99999) THEN      !2.1!1.5
         WRITE (6,'(T5,2A,I8)') 'STAPOS:   REQUESTED WMO STATION ', !1.5
     &                  'NUMBER OUT OF RANGE -', REQ_WMONO          !1.5
         ISTAT = 16                                                 !1.5
      ELSE                                                          !1.5
         ITYPE = INDEX('XSU',TYPE(1:1))                             !1.5
         IF (ITYPE.EQ.0) THEN                                       !1.5
            WRITE (6,'(T5,5A)') 'STAPOS:   STATION TYPE ',          !1.5
     &               'DOES NOT BEGIN "S", "U" OR "X". ',            !1.5
     &               'VALUE IS "', TYPE, '"'                        !1.5
            ISTAT = 16                                              !1.5
!
!----------------------------------------------------------------------
!     LOOK UP REQUESTED WMO STATION NUMBER IN LOOK-UP TABLE.
!----------------------------------------------------------------------
!
         ELSE                                                       !1.5
            ISTAT = 8 ! MATCH NOT YET FOUND                         !1.5
            CALL ISRCH (REQ_WMONO, LOOKWMO, NUMWMO, IPOS)           !1.5
!
!----------------------------------------------------------------------
!     IF FOUND, LOOK FOR REQUIRED STATION TYPE IN ORIGINAL LIST
!----------------------------------------------------------------------
!
            IF (IPOS.GT.0) THEN                                     !1.5
               J1 = NPOS(IPOS)                                      !1.5
               J2 = NPOS(IPOS+1) - 1                                !1.5
               DO JPOS=J1,J2                                        !1.5
                  IF (NTYPE(JPOS).EQ.ITYPE .OR.                     !1.5
     &               (NTYPE(JPOS).EQ.4 .AND. ITYPE.GT.1)) THEN      !1.5
                     ISTAT = 0 ! MATCH FOUND                        !1.5
                     IPOS = JPOS                                    !1.5
                     GO TO 1                                        !1.5
                  END IF                                            !1.5
               END DO ! JPOS                                        !1.5
            END IF                                                  !1.5
    1       CONTINUE                                                !1.5
         END IF                                                     !1.5
      END IF                                                        !1.5
!
!----------------------------------------------------------------------
!     RETURN STATION DETAILS IF A MATCH HAS BEEN FOUND OR MISSING
!     DATA VALUES IF NOT
!----------------------------------------------------------------------
!
      IF (ISTAT.EQ.0) THEN                     ! MATCH FOUND        !1.5
         LAT  = DEGLAT(IPOS)                                        !1.5
         LONG = DEGLON(IPOS)                                        !1.5
         HGT  = HTSTN(IPOS)                                         !1.5
         HGPT = HTP(IPOS)                                           !1.5
      ELSE                                    ! NO MATCH FOUND      !1.5
         LAT  = -9999999.0                                          !1.5
         LONG = -9999999.0                                          !1.5
         HGT  = -9999999.0                                          !1.5
         HGPT = -9999999.0                                          !1.5
      END IF                                                        !1.5
!                                          RETURN TO CALLING PROGRAM
      RETURN
      END
