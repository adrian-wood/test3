      SUBROUTINE BECRET (NFTBCN, BEACON, DEGLAT, DEGLON, NUMSTNS)   !1.4

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! SUBROUTINE    : BECRET
!
! PURPOSE       : To load the beacon list into memory.
!
! DESCRIPTION   : The beacon list (MDB.BEACONS) is opened and read line
!                 by line.  The beacon data is put into three arrays, a
!                 CHARACTER*8 array for the beacon identifiers and REAL
!                 arrays for latitudes and longitudes.  The data set is
!                 closed and the arrays returned to the calling program.
!
! USAGE         : CALL BECRET (NFTBCN, BEACON, DEGLAT, DEGLON, NUMSTNS)
!
! PARAMETERS    : NFTBCN   I   Unit number for beacon list
!                 BEACON   O   (C*8 array)  Beacon identifiers
!                 DEGLAT   O   (R*4 array)  Beacon latitudes in degs
!                 DEGLON   O   (R*4 array)  Beacon longitudes in degs
!                 NUMSTNS I/O  Input:  Maximum allowed no. of beacons
!                            Output: Actual no. of beacons in list
! CALLED BY     : BECPOS
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:59$
! $Source: /data/us0400/mdb/op/lib/source/RCS/becret.F,v $
!
! CHANGE RECORD :
!
! The first version of this routine was by J Lewthwaite and was dated
! 26 February 1996. It was completely re-written by Brian Barwell in
! May 1999 to allow binary searches of the beacon list.
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:59    Sheila Needham  
! $
! Revision 2.1  2003/03/06  09:09:27  09:09:27  usmdb (MetDB account c/o usjh)
! Read Beacon list filename from environment variable
! METDB_BCNLIST - S.Cox
! 
! Revision 2.0  2001/05/31  13:27:29  13:27:29  usmdb (MetDB account c/o usjh)
! Added copyright and modified header - S.Cox
! 
! Revision 1.6  2001/02/08  10:40:01  10:40:01  usmdb (Generic MetDB account)
! Added pre-processor statement around the OPEN statement - S.Cox
! 
! Revision 1.5  2001/02/06 16:00:51  usmdb
! 19 February 2001,  Brian Barwell.
! 1.5  Add statement to open the beacon list.
!
! Revision 1.4  99/06/10  14:42:18  14:42:18  usmdb (Generic MDB account)
! 21 June 1999, Infoman 63414, Brian Barwell, v(G)=8, ev(G)=4.
! New version with binary search of Beacon list.
!
! Revision 1.3  98/02/04  15:00:45  15:00:45  usmdb (Generic MDB account)
! Add write statement to print out unknown beacons
!
! Revision 1.2  1997/07/31 09:11:35  uspm
! First revision for  1
!
! Revision 1.1  1997/07/04 10:56:32  uspm
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

!----------------------------------------------------------------------
! Declare variables
!----------------------------------------------------------------------

      INTEGER IOS             ! STATUS CODE FROM READ STATEMENT     !1.4
      INTEGER KODE            ! INTERNALLY USED STATUS CODE         !1.4
      INTEGER MAXSTNS         ! MAXIMUM NUMBER OF STATIONS          !1.4
      INTEGER N               ! LOCALLY USED INTEGER                !1.4
      INTEGER NFTBCN          ! UNIT NUMBER BEACONS LIST            !1.4
      INTEGER NUMSTNS         ! NUMBER OF BEACONS FOUND IN LIST     !1.4

      LOGICAL HEADSET         ! TRUE if HEAD set                    !2.1

      REAL DEGLAT(*)          ! LATITUDES OF STATIONS               !1.4
      REAL DEGLON(*)          ! LONGITUDES OF STATIONS              !1.4

      CHARACTER*8 BEACON(*)   ! BEACON IDENTIFIERS                  !1.4
      CHARACTER*132 HEAD      ! REVISION INFORMATION

#if ! defined (MVS)
      INTEGER         LEV            !- Length of METDB_BCNLIST     !2.1 
      INTEGER         RC             !- Return code                 !2.1
      LOGICAL         FEXIST         !- TRUE if file exists         !2.1
      CHARACTER*200   METDB_BCNLIST  !- Beacon list PATH            !2.1
#endif

!-----------------------------------------------------------------------        
! Data statements
!-----------------------------------------------------------------------        

      DATA KODE/0/                                                  !1.4
      DATA HEADSET/.FALSE./                                         !2.1

!-----------------------------------------------------------------------        
! Revision information
!-----------------------------------------------------------------------        

      IF (.NOT.HEADSET) THEN                                        !2.1
        HEAD='$RCSfile: becret.F,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:20:59$'
        HEADSET=.TRUE.                                              !2.1
      ENDIF                                                         !2.1

!-----------------------------------------------------------------------        
! Initialise variables
!-----------------------------------------------------------------------        

      MAXSTNS = NUMSTNS                                             !1.4
      NUMSTNS = 0                                                   !1.4

!-----------------------------------------------------------------------        
! Get environment variable METDB_BCNLIST and find the length of it.
!-----------------------------------------------------------------------        

#if ! defined (MVS)
      CALL METDB_GETENV("METDB_BCNLIST",METDB_BCNLIST,RC)           !2.1
      IF (RC.NE.0) THEN                                             !2.1
        WRITE(6,*)'BECRET: ERROR: ENV VAR METDB_BCNLIST not set'    !2.1
        RETURN                                                      !2.1
      ENDIF                                                         !2.1
      LEV=LEN(METDB_BCNLIST)                                        !2.1
      DO WHILE (METDB_BCNLIST(LEV:LEV).EQ.' ')                      !2.1
        LEV=LEV-1                                                   !2.1
      ENDDO                                                         !2.1
#endif

!----------------------------------------------------------------------
!     OPEN THE BEACON LIST DATA SET
!----------------------------------------------------------------------

#if defined (MVS)
      OPEN (NFTBCN, FILE='MDBBCN', ACTION='READ')                   !1.5
#else
      INQUIRE (FILE=METDB_BCNLIST(1:LEV),EXIST=FEXIST)              !2.1
      IF (.NOT.FEXIST) THEN                                         !2.1
        WRITE(6,*)'MDBBCN: ERROR: File ',METDB_BCNLIST(1:LEV),      !2.1
     &            ' not found'                                      !2.1
        RETURN                                                      !2.1
      ENDIF                                                         !2.1
      OPEN(NFTBCN,FILE=METDB_BCNLIST(1:LEV),IOSTAT=RC)              !2.1
      IF (RC.NE.0) THEN                                             !2.1
        WRITE(6,*)'MDBBCN: ERROR: Could not open file ',            !2.1
     &                  METDB_BCNLIST(1:LEV)                        !2.1
        RETURN                                                      !2.1
      ENDIF                                                         !2.1
#endif

!----------------------------------------------------------------------
!     LOOP OVER BEACONS IN LIST AND STORE LOCATION DETAILS
!----------------------------------------------------------------------
!
      DO WHILE (KODE.EQ.0)                                          !1.4
!                                        CHECK FOR TOO MANY STATIONS
         IF (NUMSTNS.GE.MAXSTNS) THEN                               !1.4
            KODE = 1                                                !1.4
!                                        READ DETAILS OF NEXT BEACON
         ELSE                                                       !1.4
            N = NUMSTNS + 1                                         !1.4
            READ (NFTBCN, '(A8,F7.2,F11.2)', END=1, IOSTAT=IOS)     !1.4
     &            BEACON(N), DEGLAT(N), DEGLON(N)                   !1.4
!                                               CHECK FOR READ ERROR
            IF (IOS.NE.0) THEN                                      !1.4
               KODE = 2                                             !1.4
!                                                  INCREMENT COUNTER
            ELSE                                                    !1.4
               NUMSTNS = N                                          !1.4
            END IF                                                  !1.4
         END IF                                                     !1.4
      END DO                                                        !1.4
!
!----------------------------------------------------------------------
!     CLOSE THE LIST AND OUTPUT A MESSAGE GIVING NUMBER OF BEACONS
!----------------------------------------------------------------------
!
    1 CONTINUE                                                      !1.4
      CLOSE (NFTBCN)                                                !1.4
!                                                     OUTPUT MESSAGE
      IF (KODE.EQ.0) THEN                                           !1.4
         WRITE (6,'(/T5,A,I7,A)') 'BECRET:', NUMSTNS,               !1.4
     *            ' LOCATIONS READ FROM THE BEACON LIST.'           !1.4
      ELSE IF (KODE.EQ.1) THEN                                      !1.4
         WRITE (6,'(/T5,A,I6,A)')                                   !1.4
     *            'BECRET:  BEACON LIST TRUNCATED AFTER', NUMSTNS,  !1.4
     *            ' STATIONS DUE TO LIMIT ON ARRAY SIZES.'          !1.4
      ELSE IF (KODE.EQ.2) THEN                                      !1.4
         WRITE (6,'(/T5,A,I6,A)')                                   !1.4
     *            'BECRET:  BEACON LIST TRUNCATED AFTER', NUMSTNS,  !1.4
     *            ' STATIONS DUE TO I/O ERROR READING LIST.'        !1.4
      END IF                                                        !1.4
!                                          RETURN TO CALLING PROGRAM
      RETURN
      END
