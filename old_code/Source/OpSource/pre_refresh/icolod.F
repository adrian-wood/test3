      SUBROUTINE ICOLOD (ICAOLIST, DEGLAT, DEGLON, HT, HP, NUMICAO)  !2

!-----------------------------------------------------------------------
!
! PROGRAM     : ICOLOD
!
! PURPOSE     : To load the ICAO abbreviated list into memory.
!
! DESCRIPTION : The ICAO list (MDB.ICAO.LIST) is read line by line and
!               the data is put into separate arrays, a character array
!               for the station ICAO Id., real arrays for latitude and
!               longitude, and integer arrays for station height and
!               barometer heights. These arrays are then returned to
!               the calling program.
!
!               ICOLOD was largely rewritten for version 2 (including
!               the argument list) when the format of the ICAO list was
!               changed in October 2010.
!
! USAGE       : CALL ICOLOD (ICAOLIST, DEGLAT, DEGLON, HT, HP, NUMICAO)
!
! PARAMETERS  : (All parameters are output except NUMICAO which is I&O)
!
!               ICAOLIST  (C*4 array)  ICAO Id's
!               DEGLAT    (R*4 array)  Station latitudes in degrees
!               DEGLON    (R*4 array)  Station longitudes in degrees
!               HT        (I*4 array)  Station heights in metres
!               HP        (I*4 array)  Station barometer hts in metres
!               NUMICAO   (I*4) Input: Maximum allowed no. of ICAO IDs
!                              Output: Actual no. of ICAO IDs in list
!
! CALLED BY   : ICOBRV
!
! REVISION INFO :
!
! $Workfile: icolod.F$ $Folder: pre_refresh$
! $Revision: 3$ $Date: 06/10/2010 14:35:02$
!
! CHANGE RECORD :
!
! $Log:
!  3    Met_DB_Project 1.2         06/10/2010 14:35:02    Brian Barwell
!       Pre-processing statements added (copied from version 1) for PC version
!        of MetDB,
!  2    Met_DB_Project 1.1         01/10/2010 16:27:55    Brian Barwell
!       Rewritten for new-format ICAO list.
!  1    Met_DB_Project 1.0         30/01/2006 20:22:56    Sheila Needham  
! $
! Revision 2.1  2003/03/06  09:09:52  09:09:52  usmdb (MetDB account c/o usjh)
! Read ICAO list filename from environment variable
! METDB_ICAOLIST - S.Cox
!
! Revision 2.0  2001/07/03  10:43:33  10:43:33  usmdb (MetDB account c/o usjh)
! Changed pre-processor directive to if (1 ), else...
! Added copyright and modified header - S.Cox
!
! Revision 1.2  99/05/06  14:43:02  14:43:02  usmdb (Generic MetDB account)
! 17 May 1999, Infoman 62837, Brian Barwell, v(G)=12, ev(G)=3.
! Rewritten with alphabetically sorted ICAO list for binary searches.
!
! Revision 1.1  1998/09/16 15:39:03  usmdb
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER*4 ICAO          ! Target ICAO identifier            !1.2
      CHARACTER*4 ICAOLIST(*)   ! List of ICAO identifiers          !1.2
      CHARACTER*80 HEAD         ! Revision information               !2

      INTEGER HT(*)             ! Station heights                    !2
      INTEGER HP(*)             ! Station barometer heights          !2
      INTEGER IHT, IHP          ! Station height & barometer ht.     !2
      INTEGER KODE              ! Internally used status code       !1.2
      INTEGER MAXICAO           ! Max. no. of ICAO identifiers       !2
      INTEGER NUMICAO           ! Number of ICAO identifiers found  !1.2

      REAL SLAT, SLON           ! Station latitude & longitude      !1.2
      REAL DEGLAT(*)            ! Latitudes of stations             !1.2
      REAL DEGLON(*)            ! Longitudes of stations            !1.2

#if ! defined (MVS)
      INTEGER         LEV             !- Length of METDB_ICAOLIST   !2.1
      INTEGER         RC              !- Return code                !2.1
      LOGICAL         FEXIST          !- TRUE if file exists        !2.1
      CHARACTER*200   METDB_ICAOLIST  !- ICAO list PATH             !2.1
#endif
!                                                        Data statement
      DATA KODE/0/                                                  !1.2
!                                                  Revision information
      HEAD = '$Workfile: icolod.F$ ' //
     &       '$Revision: 3$ $Date: 06/10/2010 14:35:02$'

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

      MAXICAO = NUMICAO                                              !2
      NUMICAO = 0                                                   !1.2

!-----------------------------------------------------------------------
! Get environment variable METDB_ICAOLIST and find the length of it.
!-----------------------------------------------------------------------

#if ! defined (MVS)
      CALL METDB_GETENV("METDB_ICAOLIST",METDB_ICAOLIST,RC)         !2.1
      IF (RC.NE.0) THEN                                             !2.1
        WRITE(6,*)'ICOLOD: ERROR: ENV VAR METDB_ICAOLIST not set'   !2.1
        RETURN                                                      !2.1
      ENDIF                                                         !2.1
      LEV=LEN(METDB_ICAOLIST)                                       !2.1
      DO WHILE (METDB_ICAOLIST(LEV:LEV).EQ.' ')                     !2.1
        LEV=LEV-1                                                   !2.1
      ENDDO                                                         !2.1
#endif

!----------------------------------------------------------------------
!     OPEN THE ICAO LIST
!----------------------------------------------------------------------

#if defined (MVS)
      OPEN (82, FILE='STNICAO', ACTION='READ')
#else
      INQUIRE (FILE=METDB_ICAOLIST(1:LEV),EXIST=FEXIST)             !2.1
      IF (.NOT.FEXIST) THEN                                         !2.1
        WRITE(6,*)'ICOLOD: ERROR: File ',METDB_ICAOLIST(1:LEV),     !2.1
     &            ' not found'                                      !2.1
        RETURN                                                      !2.1
      ENDIF                                                         !2.1
      OPEN(82,FILE=METDB_ICAOLIST(1:LEV),IOSTAT=RC)                 !2.1
      IF (RC.NE.0) THEN                                             !2.1
        WRITE(6,*)'ICOLOD: ERROR: Could not open file ',            !2.1
     &                  METDB_ICAOLIST(1:LEV)                       !2.1
        RETURN                                                      !2.1
      ENDIF                                                         !2.1
#endif

!----------------------------------------------------------------------
!     LOOP OVER STATIONS IN ICAO LIST
!----------------------------------------------------------------------

      DO WHILE (KODE.EQ.0)                                          !1.2
!                                      READ DETAILS OF NEXT STATION
!
        READ (82, '(A,T11,F8.3,F9.3,2I5)', IOSTAT=KODE)              !2
     &        ICAO, SLAT, SLON, IHT, IHP                             !2
!                                              CHECK FOR READ ERROR
         IF (KODE.NE.0) THEN                                        !1.2
            KODE = 2                                                !1.2
!                               CHECK FOR BLANK LINE AT END OF LIST
!
         ELSE IF (ICAO.EQ.'    ') THEN                               !2
            KODE = 1                                                !1.2
!                                       CHECK FOR TOO MANY STATIONS
         ELSE IF (NUMICAO.GE.MAXICAO) THEN                           !2
            KODE = 3                                                !1.2
!                                   GOOD DATA - GET STATION DETAILS
         ELSE                                                       !1.2
            NUMICAO = NUMICAO + 1                                    !2
            ICAOLIST(NUMICAO) = ICAO  ! ICAO IDENTIFIER              !2
            DEGLAT(NUMICAO) = SLAT    ! LATITUDE                     !2
            DEGLON(NUMICAO) = SLON    ! LONGITUDE                    !2
            HT(NUMICAO) = IHT         ! STATION HEIGHT               !2
            HP(NUMICAO) = IHP         ! BAROMETER HEIGHT             !2
         END IF
      END DO
!
!----------------------------------------------------------------------
!     CLOSE THE LIST AND OUTPUT A MESSAGE GIVING NUMBER OF STATIONS
!----------------------------------------------------------------------
!
      CLOSE (82)                                                    !1.2
!
      IF (KODE.LE.1) THEN                                           !2.0
         WRITE (6,'(/T5,A,I7,A)') 'ICOLOD:', NUMICAO,                !2
     *            ' STATION RECORDS READ FROM THE ICAO LIST.'       !1.2
      ELSE IF (KODE.EQ.2) THEN                                      !1.2
         WRITE (6,'(/T5,A,I6,A)')                                   !1.2
     *            'ICOLOD:  ICAO LIST TRUNCATED AFTER', NUMICAO,     !2
     *            ' STATIONS DUE TO I/O ERROR READING LIST.'        !1.2
      ELSE IF (KODE.EQ.3) THEN                                      !1.2
         WRITE (6,'(/T5,A,I6,A)')                                   !1.2
     *            'ICOLOD:  ICAO LIST TRUNCATED AFTER', NUMICAO,     !2
     *            ' STATIONS DUE TO LIMIT ON ARRAY SIZES.'          !1.2
      END IF                                                        !1.2

      RETURN
      END
