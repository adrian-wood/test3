!  $Header: findbox.f, 1, 27/02/2007 16:11:06, Stan Kellett$

      SUBROUTINE FINDBOX (BOX,GRID,ROW_LEN,COL_LEN,DX,DY,NEAREST,
     &                    POSN,ERROR,ERR_UNIT,MSG_LVL)
!
!     Subroutine to find the position of a box inside a lat/long grid
!     in terms of the X and Y coords. of the TLH and BRH corners of
!     the box.
!
!     If NEAREST.NE.0 then the caller wants points to fit exactly.
!     Otherwise, find the best fit possible.
!     This subroutine DOES NOT interpolate.

      IMPLICIT NONE

!     Subroutine arguments - INTENT IN.

      INTEGER BOX(4),       ! lat/long of TLH and BRH corners of box.
     &        GRID(4)       ! lat/long of TLH and BRH corners of grid.

      INTEGER ROW_LEN,      ! number of grid points along a row.
     &        COL_LEN,      ! number of grid points down a column.
     &        DX,           ! X-direction increment of grid.
     &        DY,           ! Y-direction increment of grid.
     &        NEAREST       ! if not 0, exact fit only.
     
      INTEGER ERR_UNIT,     ! unit to send error messages to.
     &        MSG_LVL       ! level of messages returned.

!     Subroutine arguments - INTENT OUT.

      INTEGER POSN(4)       ! grid point coords of box.

      INTEGER ERROR         ! error code.

!     Local variables.

      INTEGER INDEX1,       ! loop index.
     &        TEMP_INT      ! working values.

!--------------------------------------------------------------------

! Initialise error status to 'no error'

      ERROR  = 0

!     Try to find X position of TLH and BRH corners.
!     Assume following notation
!
!        ----> scan this way. 
!     X=1                 X=5  
! Y=1   X    X    X    X    X   I
!                               I
!       X    X    X    X    X   I
!                               V
! Y=3   X    X    X    X    X    scan down this way.


      DO INDEX1 = 1 , ROW_LEN

!       Latitude of point INDEX1.

        TEMP_INT = GRID(1) + (INDEX1 - 1) * DX

!       If exact fit is specified, latitude must fit exactly.

        IF (NEAREST.EQ.0) THEN
          IF (ABS(TEMP_INT-BOX(1)).LE.ABS(DX / 2)) POSN(1) = INDEX1
          IF (ABS(TEMP_INT-BOX(3)).LE.ABS(DX / 2)) POSN(3) = INDEX1
        ELSE
          IF (TEMP_INT.EQ.BOX(1)) POSN(1) = INDEX1
          IF (TEMP_INT.EQ.BOX(3)) POSN(3) = INDEX1
        ENDIF

      END DO

!     Now try to find Y-positions.

      DO INDEX1 = 1 , COL_LEN

!       Long. of point INDEX1.

        TEMP_INT = GRID(2) + (INDEX1 - 1) * DY

!       If exact fit specified, longitiude must fit exactly.

        IF (NEAREST.EQ.0) THEN
          IF (ABS(TEMP_INT-BOX(2)).LE.ABS(DY / 2)) POSN(2) = INDEX1
          IF (ABS(TEMP_INT-BOX(4)).LE.ABS(DY / 2)) POSN(4) = INDEX1
        ELSE
          IF (TEMP_INT.EQ.BOX(2)) POSN(2) = INDEX1
          IF (TEMP_INT.EQ.BOX(4)) POSN(4) = INDEX1
        ENDIF

      END DO

!     4 possible cases -
!      a) box is entirely inside grid (POSN NE 0)
!      b) first point is outside grid (either POSN(1) or POSN(2) EQ 0)
!      c) last point is outside grid (either POSN(3) or POSN(4) EQ 0)
!     d) box is entirely outside grid (all POSN EQ 0)

!     Disaster !

      TEMP_INT = POSN(1) + POSN(2) + POSN(3) + POSN(4)

      IF (TEMP_INT.EQ.0) THEN
        ERROR = 9101
        IF (MSG_LVL.LT.3) THEN
          WRITE (ERR_UNIT,*) ' ERROR - box to extract does not lie ',
     &                       'inside grid.'
          WRITE (ERR_UNIT,9001) BOX
          WRITE (ERR_UNIT,9002) GRID
 9001     FORMAT (' Box corners - ',4(1X,I6.6))
 9002     FORMAT (' Grid corners - ',4(1X,I6.6))
        ENDIF
        RETURN
      ENDIF

!     The TLH corner is outside the grid if either POSN(1) or
!     POSN(2) = 0.

      IF ((POSN(1).EQ.0).OR.(POSN(2).EQ.0)) THEN
        IF (MSG_LVL.LT.2) THEN
          WRITE (ERR_UNIT,*) ' WARNING - top-left-hand corner of ',
     &                       'limited area lies outside the grid.'
        ENDIF

!       Get nearest corner, unless exact fit only is specified.

        IF (NEAREST.EQ.0) THEN
          IF (POSN(1).EQ.0) POSN(1) = 1
          IF (POSN(2).EQ.0) POSN(2) = 1
        ELSE
          IF (MSG_LVL.LT.3) THEN
            WRITE (ERR_UNIT,*) ' ERROR - exact fit of limited area ',
     &                         'is not possible.'
          ENDIF
          ERROR = 9102
          RETURN
        ENDIF

      ENDIF

!     The BRH corner is outside the grid if either POSN(3) or
!     POSN(4) = 0.

      IF ((POSN(3).EQ.0).OR.(POSN(4).EQ.0)) THEN
        IF (MSG_LVL.LT.2) THEN
          WRITE (ERR_UNIT,*) ' WARNING - bottom-right-hand corner ',
     &                       'of limited area lies outside the grid.'
        ENDIF

!       Get nearest corner, unless exact fit only is specified.

        IF (NEAREST.EQ.0) THEN
          IF (POSN(3).EQ.0) POSN(3) = ROW_LEN
          IF (POSN(4).EQ.0) POSN(4) = COL_LEN
        ELSE
          IF (MSG_LVL.LT.3) THEN
            WRITE (ERR_UNIT,*) ' ERROR - exact fit of limited area ',
     &                         'is not possible.'
          ENDIF
          ERROR = 9102
          RETURN
        ENDIF

      ENDIF

      RETURN
      END
