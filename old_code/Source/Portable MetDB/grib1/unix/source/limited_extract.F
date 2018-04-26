!+    Extract and decode some of the data in a file of GRIB messages
! $Header: limited_extract.F, 2, 12/01/2009 16:48:31, Alison Weir$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Limited_Extract (file_name, unit_num, mesgs, len_mesg,
     &  block_0, block_1, block_2, block_3, block_4, block_r,
     &  box_coords, num_level, fp_data, len_fp, num_fp, nearest,
     &  err_unit, msg_lvl)

      IMPLICIT NONE

!     Description:
!       Extracts and decodes data for a specified geographical area
!       from a file containing one or more GRIB messages. The messages
!       may contain data for a larger geographical area.
!
!     Method:
!       <Say how it does it: refer to external documentation>
!       <If this routine is very complex, then include a
!        "pseudo code" description of it to make its structure
!        and method clear>
!
!     Current Code Owner: Andrew Edmunds OS(US)
!
!     History:
!       This file is under RCS control. See the RCS log for change
!       history information.
!
!     Code Description:
!       FORTRAN 77 with extensions recommended in the Met. Office
!       F77 Standard.
!
! Revision Information:
!
! $Revision: 2$
! $Date: 12/01/2009 16:48:31$
! $Folder: source$
! $Workfile: limited_extract.F$ 
!
! $Log:
!  2    Met_DB_Project 1.1         12/01/2009 16:48:31    Alison Weir     SX6
!       directives changed to L64 to allow compilation on IBM supercomputer.
!  1    Met_DB_Project 1.0         27/02/2007 16:05:03    Stan Kellett    
! $
! Revision 6.1  2001/11/05 10:21:34  usmdb
! Superstash.
!
! Revision 6.0  2001/08/20 12:12:02  usmdb
! new version 6.0, changed so a single version using preprocessor statements and to compile on f90 on HPUX11.
!
!
!     Scalar arguments with INTENT(IN):

      CHARACTER*50 file_name       ! Name of GRIB file
      INTEGER    unit_num          ! Unit number of GRIB file
      INTEGER    len_mesg          ! Length of GRIB messages (bytes)
      INTEGER    num_level         ! No. of fields to extract from
      INTEGER    len_fp            ! Length of the data array
      INTEGER    nearest           ! If non-zero, pick up exactly
                                   ! the points specified
      INTEGER    err_unit          ! Unit to send error messages to
      INTEGER    msg_lvl           ! Level of messages returned

!     Array arguments with INTENT(IN):

      INTEGER    box_coords(4)     ! Coords of the box corners

!     Array arguments with INTENT(OUT):

      CHARACTER*1 mesgs(len_mesg)           ! Array to hold file
      INTEGER    block_0( 4, num_level)     ! GRIB parameters
      INTEGER    block_1(79, num_level)     ! Product description !6.1
      INTEGER    block_2(20, num_level)     ! Grid description
      INTEGER    block_3( 2, num_level)     ! Bitmap parameters
      INTEGER    block_4( 2, num_level)     ! Data description
      REAL       block_r(20, num_level)     ! Real GRIB parameters
      REAL       fp_data(len_fp, num_level) ! Array to hold decoded
                                            ! data
      INTEGER    num_fp(num_level)          ! No. of points retrieved

!     Local Parameters:

!     *** MACHINE DEPENDANT ***

      INTEGER    word_size         ! No. of bits in an INTEGER
# if defined (CRAY) || defined (L64)           /*2*/
      PARAMETER (word_size = 64)
# else
      PARAMETER (word_size = 32)
# endif

!     Local Scalars:

      INTEGER    err_u             ! Unit number actually used for
                                   ! error messages
      INTEGER    num_mesgs         ! Number of messages in file
      INTEGER    len_vert          ! Length of vertical coordinate
                                   ! parameter array
      INTEGER    num_vert          ! Number of vertical coordinate
                                   ! parameters
      INTEGER    len_bitmap        ! Length of bitmap array
      INTEGER    num_bitmap        ! Number of points in the bitmap
      INTEGER    len_q             ! Length of array quasi
      INTEGER    num_q             ! Number of rows in quasi-regular
                                   ! grid
      INTEGER    width             ! No. of bits used to pack data
      INTEGER    word              ! Position in message
      INTEGER    off               ! Offset in message
      INTEGER    error             ! Error indicator
      INTEGER    row_length        ! Number of points along a row
      INTEGER    col_length        ! Number of points down a column
      INTEGER    dx                ! X direction increment
      INTEGER    dy                ! Y direction increment
      INTEGER    temp_len          ! Temporary length for fp_data
      INTEGER    temp_grib         ! Temporary length for mesgs
      INTEGER    temp_err          ! Temporary error indicator
      INTEGER    temp_int1         ! Temporary value used in tests
      INTEGER    temp_pos1         ! Store for posn(1) between calls
      INTEGER    index1            ! Loop indices
      INTEGER    index2


!     Local Arrays:

      INTEGER    heads(1000)        ! Starts of messages
      INTEGER    tails(1000)        ! Ends of messages
      INTEGER    bitmap(50000)      ! Bitmap
      INTEGER    quasi(5000)        ! Quasi-regular grid
      INTEGER    work_int1(5000)    ! Work array 1
      INTEGER    work_int2(5000)    ! Work array 2
      INTEGER    grid_coords(4)     ! TLH and BRH lat/long of grid
      INTEGER    box_posn(4)        ! Box position in grid
      REAL       fp_work(50000)     ! Work array
      REAL       vert_coords(5000)  ! Vertical coordinates
      REAL       work_re1(5000)     ! Real work array

!     Functions and subroutines used:

      EXTERNAL   Analyse
      EXTERNAL   Decode
      EXTERNAL   FindBox
      
! Revision Code:
      LOGICAL HEADSET
      CHARACTER*132 HEAD
      DATA HEADSET /.FALSE./
      
      IF (.NOT.HEADSET) THEN
        HEAD='
     &$Source: /home/us0400/usjh/temp/lib/../../../grib/source/RCS/limited_extract.F,v $
     &'//'$Date: 12/01/2009 16:48:31$'
        HEADSET = .TRUE.
      ENDIF                
! End of Rev
!

!-    End of header

      DATA BOX_POSN /4*0/

!     Check for a sensible unit for error messages.

      IF ( err_unit .GT. 0 ) THEN
        err_u = err_unit
      ELSE
        err_u = 6
      END IF

!     First, call analyse to find out what's in the file.

      CALL ANALYSE (UNIT_NUM,FILE_NAME,MESGS,LEN_MESG,
     &              HEADS,TAILS,NUM_MESGS,ERR_U)

!     If there are no messages, don't carry on.

      IF (NUM_MESGS.EQ.0 .AND. MSG_LVL.LT.3) THEN
        WRITE ( UNIT = ERR_U, FMT = '(A)' )
     &    ' ERROR - there are no GRIB messages in the file.'
        RETURN
      ENDIF

!     Check that there are enough levels.

      IF (NUM_MESGS.LT.NUM_LEVEL) THEN
        IF (MSG_LVL .LT. 2) THEN
          WRITE (ERR_U,9004) NUM_MESGS,NUM_LEVEL
 9004     FORMAT (' WARNING - file has ', I4, ' messages, but ',
     &            ' NUM_LEVEL is ', I4)
        ENDIF
      ENDIF

!     Start loop over number of levels ------------------------------

      DO INDEX1 = 1 , NUM_MESGS

!       First need to call DECODE to pick up blocks 1 and 2.

        LEN_VERT   = 100           
        LEN_BITMAP = 1
        LEN_Q      = 100
        TEMP_LEN   = 1          ! just get first two blocks.
        TEMP_GRIB  = (TAILS(INDEX1) - HEADS(INDEX1) + 1) /
     &               (WORD_SIZE/8)
        BLOCK_4(2, INDEX1) = 0
        WORD       = 0
        OFF        = 0
        ERROR      = 0

!       DECODE checks BOX_POSN(1) to see whether we're doing a LIMEXT.
!       Need to fool it into thinking we're not.

        TEMP_POS1  = BOX_POSN(1)
        BOX_POSN(1)= 0

        CALL DECODE(FP_DATA(1,INDEX1),FP_WORK,TEMP_LEN,NUM_FP(INDEX1),
     &              VERT_COORDS,LEN_VERT,NUM_VERT,BITMAP,LEN_BITMAP,
     &              NUM_BITMAP,QUASI,LEN_Q,NUM_Q,WIDTH,WORD_SIZE,
     &              BLOCK_0(1,INDEX1),BLOCK_1(1,INDEX1),
     &              BLOCK_2(1,INDEX1),BLOCK_3(1,INDEX1),
     &              BLOCK_4(1,INDEX1),BLOCK_R(1,INDEX1),
     &              MESGS(HEADS(INDEX1)),TEMP_GRIB,BOX_POSN,
     &              WORD,OFF,ERROR,WORK_INT1,WORK_INT2,WORK_RE1,
     &              ERR_U, MSG_LVL)

!       Check that lat/long grid is described in block 2.

        IF ((BLOCK_1(4,INDEX1)/128).NE.1) THEN
          IF (MSG_LVL .LT. 3) THEN
            WRITE (UNIT = ERR_U, FMT = '(A)' )
     &        ' ERROR - grid must be defined in block 2 for ' //
     &        'limited area extract.'
          ENDIF
          RETURN
        ENDIF

        IF (BLOCK_2(3,INDEX1).NE.0) THEN
          IF (MSG_LVL .LT. 3) THEN 
            WRITE (ERR_U,9001) BLOCK_2(3,INDEX1)
 9001       FORMAT (' ERROR - grid type ', I3, ' invalid for limited ',
     &              ' area extract.')
          ENDIF
          RETURN
        ENDIF

!       Get grid information.

        GRID_COORDS(1) = BLOCK_2(7,INDEX1) ! long. of origin (X coord)
        GRID_COORDS(2) = BLOCK_2(6,INDEX1) ! lat of origin. (Y coord)
        GRID_COORDS(3) = BLOCK_2(10,INDEX1) ! long of last pt.(X cood)
        GRID_COORDS(4) = BLOCK_2(9,INDEX1) ! lat of last pt. (Y coord)

        ROW_LENGTH     = BLOCK_2(4,INDEX1)
        COL_LENGTH     = BLOCK_2(5,INDEX1)

!       If direction increments are not given, calculate them.

        TEMP_INT1 = BLOCK_2(8,INDEX1) / 128

        IF (TEMP_INT1.EQ.0) THEN
          DX = (GRID_COORDS(3) - GRID_COORDS(1)) / (ROW_LENGTH - 1)
          DY = (GRID_COORDS(4) - GRID_COORDS(2)) / (COL_LENGTH - 1)
        ELSE
          DX = BLOCK_2(11,INDEX1)
          DY = BLOCK_2(12,INDEX1)
        ENDIF

!       First time around, find nearest grid point to
!       TLH and BRH corners of box.

        IF (INDEX1.EQ.1) THEN

          ERROR = 0

          CALL FINDBOX (BOX_COORDS,GRID_COORDS,ROW_LENGTH,COL_LENGTH,
     &                  DX,DY,NEAREST,BOX_POSN,ERROR,ERR_U,
     &                  MSG_LVL)
          IF (ERROR.GT.9000) RETURN

        ELSE

!         Check that grids are the same in some way.
!         (code in here!)
!         Requirement for the subroutine is simply that the grid-point
!         offset should be the same - not bothered about the real
!         meaning of the grids.

          TEMP_ERR = 0

          DO INDEX2 = 4 , 7
            IF (BLOCK_2(INDEX2,INDEX1).NE.BLOCK_2(INDEX2,INDEX1-1))
     &        TEMP_ERR = 1
          END DO

          DO INDEX2 = 9 , 10
            IF (BLOCK_2(INDEX2,INDEX1).NE.BLOCK_2(INDEX2,INDEX1-1))
     &        TEMP_ERR = 1
          END DO

          IF (BLOCK_2(13,INDEX1).NE.BLOCK_2(13,INDEX1-1)) TEMP_ERR = 1

          IF (TEMP_ERR.EQ.0) THEN

!           Restore the value of POSN(1).

            BOX_POSN(1) = TEMP_POS1

          ELSE

!           Grids are not alike enough to assume that grid-point
!           offsets will be the same.

            IF (MSG_LVL .LT. 3) THEN
              WRITE (ERR_U,9002) INDEX1
 9002         FORMAT (' WARNING - grid ', I3.3, ' has different ',
     &                'structure to grid 1.')
            ENDIF

            ERROR = 0

            CALL FINDBOX (BOX_COORDS,GRID_COORDS,ROW_LENGTH,
     &                    COL_LENGTH,DX,DY,NEAREST,BOX_POSN,ERROR,
     &                    ERR_U,MSG_LVL)
            IF (ERROR.GT.9000) RETURN

          ENDIF

        ENDIF

!       Set the array dimensions to something sensible.

        LEN_VERT   = NUM_VERT
        IF (LEN_VERT.EQ.0) LEN_VERT = 1

        LEN_BITMAP = ROW_LENGTH * COL_LENGTH
        IF (LEN_BITMAP.EQ.0) LEN_BITMAP = 1

        LEN_Q      = NUM_Q
        IF (LEN_Q.EQ.0) LEN_Q = 1

!       Clear error indicator.

        ERROR = 0

        CALL DECODE(FP_DATA(1,INDEX1),FP_WORK,LEN_FP,NUM_FP(INDEX1),
     &              VERT_COORDS,LEN_VERT,NUM_VERT,BITMAP,LEN_BITMAP,
     &              NUM_BITMAP,QUASI,LEN_Q,NUM_Q,WIDTH,WORD_SIZE,
     &              BLOCK_0(1,INDEX1),BLOCK_1(1,INDEX1),
     &              BLOCK_2(1,INDEX1),BLOCK_3(1,INDEX1),
     &              BLOCK_4(1,INDEX1),BLOCK_R(1,INDEX1),
     &              MESGS(HEADS(INDEX1)),TEMP_GRIB,BOX_POSN,
     &              WORD,OFF,ERROR,WORK_INT1,WORK_INT2,WORK_RE1,
     &              ERR_U,MSG_LVL)

!       End loop over number of levels ------------------------------

      END DO

      RETURN
      END ! SUBROUTINE Limited_Extract

