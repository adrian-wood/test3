      SUBROUTINE PAOBIND
     &             (A, NOBS, NELEM, BULL, IUNIT, LENREC, IDMSG, ITOR)

!-----------------------------------------------------------------------
!
! PROGRAM       : PAOBIND
!
! PURPOSE       : SETS UP MOST OF THE INDEX ENTRY FOR A PAOB MESSAGE
!                 AND CALLS "AIRSTO" TO STORE THE DATA.
!
! USAGE         : CALL PAOBIND
!                  (A, NOBS, NELEM, BULL, IUNIT, LENREC, IDMSG, ITOR)
!                   1    2      3     4     5       6      7      8
!
! PARAMETERS    : (ALL PARAMETERS ARE INPUT VARIABLES)
!                 (1) ARRAY OF OBSERVED VALUES IN CORRECT SEQUENCE
!                 (2) NUMBER OF OBSERVATIONS IN MESSAGE
!                 (3) NUMBER OF ELEMENTS PER OB IN "A"
!                 (4) BUFR MESSAGE
!                 (5) UNIT NUMBER OF STORAGE DATA SET
!                 (6) RECORD LENGTH OF STORAGE DATA SET
!                 (7) (CHARACTER) MESSAGE ID (FOR INDEX BYTES 3-11)
!                 (8) TIME OF RECEIPT (YEAR, MONTH, DAY, HOUR, MIN.)
!
! CALLED BY     : PAOBUL
!
! CALLS         : AIRSTO, LATBOX.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:51$
! $Source: /data/us0400/mdb/op/lib/source/RCS/paobind.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:51    Sheila Needham  
! $
! Revision 2.1  2002/03/07  15:52:57  15:52:57  usmdb (Generic MetDB account)
! 2.1.  18 March 2002.  Brian Barwell.  Change 21/02.
! Bug correction: Declare IDMSG as CHARACTER*9, not INTEGER.
! 
! Revision 2.0  2001/07/03  10:43:44  10:43:44  usmdb (Generic MetDB account)
! Removed unused local variables. Added copyright and modified
! header - S.Cox
!
! Revision 1.1  99/03/11  13:47:57  13:47:57  usmdb (Generic MetDB account)
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL. OPERATIONAL: MARCH 1999.
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
!
      INTEGER IUNIT, LENREC     ! UNIT & RECORD LENGTH OF STORAGE D/S
      INTEGER IVAL              ! INTERNAL DOGSBODY VARIABLE
      INTEGER ITOR(5)           ! TIME OF RECEIPT (Y,M,D,H,M)
      INTEGER MINLAT, MAXLAT    ! MINIMUM & MAXIMUM LATITUDES
      INTEGER MINLON, MAXLON    ! MINIMUM & MAXIMUM LONGITUDES
      INTEGER NOBS, NELEM       ! NO. OF OBS. & DATA VALUES IN MESSAGE
      INTEGER DATIME(5)         ! OBSERVATION TIME (Y,M,D,H,M)
!
      REAL BOX(4)               ! BOUNDARIES OF BOX CONTAINING OBS.
      REAL A(NOBS,NELEM)        ! OBSERVATION VALUES
!
      CHARACTER*9   IDMSG       ! MESSAGE IDENTIFIER                !2.1
      CHARACTER*23  ENTRY       ! 23-CHARACTER INDEX ENTRY
      CHARACTER*132 HEAD        ! FOR REVISION INFORMATION
      CHARACTER*(*) BULL        ! BUFR-CODED MESSAGE TO BE STORED
!
      INTEGER NYEAR, NMONTH, NDAY, NHOUR, NMIN                      !2.0
      DATA    NYEAR, NMONTH, NDAY, NHOUR, NMIN
     &      /   1,     2,     3,     4,    5 /                      !2.0
!
!                                                  REVISION INFORMATION
      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/paobind.F,v $
     &'//'$Date: 30/01/2006 20:23:51$ $Revision: 1$'
!
!-----------------------------------------------------------------------
!
!     SET UP INDEX ENTRY (MESSAGE IDENT, NO. OF OBS, LATS & LONGS)
!     (LATITUDES: WHOLE DEGREES PLUS 90. LONGITUDES: (DEGREES/2 + 90).)
!     (TIMES & BLOCK/RECORD NUMBER TO BE ADDED BY STORAGE PROGRAM.)
!  ___________________________________________________________________
!  : LAND : FINE : HOUR : MIN. : IDENTI-  : NUMBER  : LAT/ :       :
!  : /SEA : MESH :  (6  :  (6  : FIER     : OF OBS. : LONG : FLAGS :
!  : FLAG : FLAG : BITS): BITS): (9 CHARS):         :  BOX :       :
!  +---|------|---------+------+----------+---------+------+-------+--
!  0   V      V         1      2         11        12     16      17
!    SEA=1  OUT=1
!  ___________________________________________________________________
!  :             : MIN : MAX : MIN : MAX :     : TIME :RECORD: BLOCK :
!  : (SEE ABOVE) : LAT : LAT : LONG: LONG:FLAGS:  OF  :  NO. :  NO.  :
!  :             :     :     :     :     :     : RCPT.:      :       :
!  +-------------+-----+-----+-----+-----+-----+------+------+-------+
!  0            12    13    14    15    16    17     19     21      23
!
      ENTRY = ' '
      WRITE (ENTRY(3:11),'(A)') IDMSG     ! MESSAGE IDENTIFIER
      ENTRY(12:12) = CHAR(NOBS)           ! NUMBER OF OBSERVATIONS
      ENTRY(17:17) = CHAR(0)              ! NO FLAGS
!
!-----------------------------------------------------------------------
!     PUT LAT/LONG DETAILS IN INDEX ENTRY
!-----------------------------------------------------------------------
!
!                                           GET LAT/LONG BOX BOUNDARIES
      CALL LATBOX (A, NOBS, 6, BOX)
!
      IF (NOBS.GT.1) THEN !               > 1 OB - STORE BOX BOUNDARIES
         MINLAT = NINT(BOX(1)+90.0)
         MAXLAT = NINT(BOX(2)+90.0)
         MINLON = (NINT(BOX(3))+180)/2
         MAXLON = (NINT(BOX(4))+181)/2 ! "181" TO ALLOW FOR ROUNDING
!
      ELSE !                               ONLY 1 OB - STORE LAT & LONG
         IVAL = NINT(BOX(1)*100.0)
         MINLAT = IVAL/256
         MAXLAT = MOD(IVAL,256)
         IVAL = NINT(BOX(3)*100.0)
         MINLON = IVAL/256
         MAXLON = MOD(IVAL,256)
      END IF
!                                            PUT DETAILS IN INDEX ENTRY
      ENTRY(13:13) = CHAR(MINLAT)
      ENTRY(14:14) = CHAR(MAXLAT)
      ENTRY(15:15) = CHAR(MINLON)
      ENTRY(16:16) = CHAR(MAXLON)
!                                      PUT OBSERVATION TIME IN "DATIME"
!
      DATIME(1) = NINT(A(1,NYEAR))  ! (FROM DATIM, SO ALWAYS 4 DIGITS)
      DATIME(2) = NINT(A(1,NMONTH))
      DATIME(3) = NINT(A(1,NDAY))
      DATIME(4) = NINT(A(1,NHOUR))
      DATIME(5) = NINT(A(1,NMIN))
!
!-----------------------------------------------------------------------
!     PASS INDEX ENTRY AND DATE/TIME OF DATA TO STORAGE ROUTINE
!-----------------------------------------------------------------------
!
      CALL AIRSTO (DATIME, ENTRY, BULL, IUNIT, LENREC, IDMSG, ITOR)
!
!                                                     STORAGE COMPLETED
      RETURN
      END
