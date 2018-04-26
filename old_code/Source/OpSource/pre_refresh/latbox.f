      SUBROUTINE LATBOX(A,NOBS,NLAT,BOX)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM       : LATBOX
!
! PURPOSE       : FIND LAT-LONG BOX ENCLOSING SET OF LAT/LONGS
!                 (N.B. THIS IS NOT AS SIMPLE AS IT MIGHT SEEM,
!                  BECAUSE THE BOX MIGHT CROSS 180 DEGREES!)
!
! CALLED BY     : ERSIND, INDEX1, INDEX2, MERGE, PAOBIND,            !2
!                 SALTSSH, SFERICS, STBIND                           !2
!
! PARAMETERS    : (1) ARRAY OF VALUES
!                 (2) NUMBER OF VALUES OF EACH ELEMENT
!                 (3) SUBSCRIPT OF LATITUDE (ASSUME LONGITUDE NEXT)
!                 (4) LAT-LONG BOX (MIN LAT, MAX LAT, MIN LONG...)
!
! REVISION INFO :
!
! $Workfile: latbox.f$ $Folder: pre_refresh$
! $Revision: 3$ $Date: 29/09/2008 16:18:21$
!
! CHANGE RECORD :
!
! $Log:
!  3    Met_DB_Project 1.2         29/09/2008 16:18:21    Brian Barwell
!       Comments added to declarations.
!  2    Met_DB_Project 1.1         12/09/2008 13:55:40    Brian Barwell
!       Introduce check for missing lat/longs.
!  1    Met_DB_Project 1.0         30/01/2006 20:23:04    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:35  usmdb
! Removed unused variable I, added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  09:29:02  09:29:02  uspm (Pat McCormack)
! First revision for MVS
!
! Revision 1.1  1997/07/04 12:49:23  uspm
! Initial revision
!
! MADE NOV 96 FROM CODE IN ERSIND
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

!declare integer
      INTEGER NO          ! Loop variable for loop over obs.       !2.0
      INTEGER NGOOD       ! Number of good lat/longs                 !2
      INTEGER NLAT        ! Latitude subscript for A array
      INTEGER NOBS        ! Number of obs. in bulletin

!declare real
      REAL BOX(4)         ! Limits of lat/long box (output)
      REAL A(NOBS,*)      ! Array of decoded BUFR data values
      REAL ALAT           ! Latitude                                 !2
      REAL ALON           ! Longitude                                !2
      REAL MINLAT         ! Minimum latitude in bulletin
      REAL MAXLAT         ! Maximum latitude in bulletin
      REAL MINLON         ! Minimum longitude in bulletin
      REAL MAXLON         ! Maximum longitude in bulletin
      REAL MINPOS         ! Minimum positive longitude
      REAL MAXPOS         ! Maximum positive longitude
      REAL MINNEG         ! Minimum negative longitude
      REAL MAXNEG         ! Maximum negative longitude

!declare character
      CHARACTER*80 HEAD                                              !2

!declare logical
      LOGICAL FIRST                                                  !2
      DATA FIRST /.TRUE./                                            !2

!Revision information                                                !2
      IF (FIRST) THEN                                                !2
        HEAD = '$Workfile: latbox.f$ ' //
     &         '$Revision: 3$ $Date: 29/09/2008 16:18:21$'
        FIRST = .FALSE.                                              !2
      END IF                                                         !2

!initialize variables
      MINLAT=+9999999.
      MAXLAT=-9999999.
      MINPOS=+9999999.
      MAXPOS=-9999999.
      MINNEG=-9999999.
      MAXNEG=+9999999.

! Keep max & min latitudes for index entry ignoring missing data.    !2
! Longitudes are less simple because range can cross 180: keep 4     !2
! numbers, max & min pos & neg, to work out range at end.            !2

      NGOOD = 0                                                      !2
      DO NO=1,NOBS                                                   !2
        ALAT = A(NO,NLAT)                                            !2
        ALON = A(NO,NLAT+1)                                          !2
        IF (ABS(ALAT).LE.90.0 .AND. ABS(ALON).LE.180.0) THEN         !2
          NGOOD = NGOOD + 1                                          !2
          IF (ALAT.GT.MAXLAT) MAXLAT = ALAT                          !2
          IF (ALAT.LT.MINLAT) MINLAT = ALAT                          !2

          IF (ALON.GE.0) THEN                                        !2
            IF (ALON.GT.MAXPOS) MAXPOS = ALON                        !2
            IF (ALON.LT.MINPOS) MINPOS = ALON                        !2
          ELSE
            IF (ALON.LT.MAXNEG) MAXNEG = ALON                        !2
            IF (ALON.GT.MINNEG) MINNEG = ALON                        !2
          END IF
        END IF                                                       !2
      END DO                                                         !2

! Find max & min longitude from max & min pos & neg values

      IF (NGOOD.EQ.0) THEN                 ! NO GOOD LAT/LONGS       !2
        MINLAT = -9999999.                                           !2
        MINLON = -9999999.                                           !2
        MAXLON = -9999999.                                           !2
      ELSE IF (MINNEG.EQ.-9999999) THEN    ! NO NEGATIVE LONGITUDES  !2
        MINLON=MINPOS
        MAXLON=MAXPOS
      ELSE IF (MAXPOS.EQ.-9999999) THEN    ! NO POSITIVE LONGITUDES
        MINLON=MAXNEG
        MAXLON=MINNEG
      ELSE IF (MINPOS-MINNEG.LE.180) THEN  ! 0 IN RANGE
        MINLON=MAXNEG
        MAXLON=MAXPOS
      ELSE IF (MINPOS-MINNEG.GT.180) THEN  ! 180 IN RANGE
        MINLON=MINPOS
        MAXLON=MINNEG
      ENDIF

      BOX(1)=MINLAT                          !Set up Lat/Long box
      BOX(2)=MAXLAT                          !in single dimension array
      BOX(3)=MINLON                          !box - to be passed back
      BOX(4)=MAXLON                          !to calling routine.

      RETURN
      END
