!+    Encode a field of data in edition 1 GRIB
! $Header: grib_edtn_1_enc.F, 1, 27/02/2007 16:05:02, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Grib_Edtn_1_Enc (fp_data, len_fp, vert_coords,
     &  len_vert, bitmap, len_bitmap, quasi, len_q, width, word_size,
     &  block_0, block_1, block_2, block_3, block_4, block_r,
     &  mesg, len_mesg, word, error, work_int1, work_int2, work_re1,
     &  err_unit, msglvl)

      IMPLICIT NONE

!     Description:
!       Encodes the data field fp_data and the various ancillary
!       information from the other input arguments as a GRIB message,
!       and returns it in mesg.
!
!       WARNING - Maximum number of bits per data value is 30. This
!       is non-standard GRIB.
!
!     Method:
!       <Say how it does it: refer to external documentation>
!       <If this routine is very complex, then include a
!        "pseudo code" description of it to make its structure
!        and method clear>
!
!     Current Code Owner: Database Development Group, ISD, TS 
!
!     History:
!       This file is under RCS control. See the RCS log for change
!       history information.
!
!       Y2K compliance - valid years are 1 to 100   (11/05/1998)
!                                                        search on !4.15
!
!     Code Description:
!       FORTRAN 77 with extensions recommended in the Met. Office
!       F77 Standard.
!$Log:
! 1    Met_DB_Project 1.0         27/02/2007 16:05:02    Stan Kellett    
!$
!Revision 6.5  2004/05/18 11:30:45  usmdb
!Add SX6 directives.
!
!Revision 6.4  2002/07/05 10:44:26  usmdb
!tidied up machine dependent code, initialised msg array and corrected
!some spelling mistakes.     Stan Kellett and Brian Barlow.
!
!Revision 6.3  2001/12/18 15:40:29  usjh
!Additional check for sub centre ID.
!
!Revision 6.2  2001/11/29 12:48:21  usmdb
!Bug fix for SuperSTASH code. Coded 1 instead of 41.
!John Hodkinson
!
!Revision 6.1  2001/11/05 10:11:36  usmdb
!Super stash.
!
!Revision 6.0  2001/08/20 11:29:42  usmdb
!New version with pre-processor statements as HPUX11 f90 compiler will
!not compile some old statements.
! 
!$Revision: 1$
!$Date: 27/02/2007 16:05:02$
!
!     Scalar arguments with INTENT(IN):

      INTEGER    len_fp            ! Number of data points
      INTEGER    len_vert          ! Number of vertical coordinate
                                   ! parameters, if level type is
                                   ! hybrid level
      INTEGER    len_bitmap        ! Number of grid points, if
                                   ! bitmapped grid used
      INTEGER    len_q             ! Number of rows/columns, if
                                   ! grid type is quasi-regular
      INTEGER    word_size         ! Number of bits in an INTEGER
      INTEGER    len_mesg          ! Length of array mesg
      INTEGER    err_unit          ! Unit number requested for error
                                   ! messages
      INTEGER    msglvl            ! Level of diagnostic messages
                                   ! required

!     Array arguments with INTENT(IN):

      REAL       vert_coords(len_vert)   ! Vertical coordinate params,
                                         ! if level type is hybrid
                                         ! level
      INTEGER    bitmap(len_bitmap)      ! Bit map, if used
      INTEGER    quasi(len_q)            ! Length of each row or
                                         ! column, if grid type is
                                         ! quasi-regular
      INTEGER    block_3(2)        ! Not used
      INTEGER    block_4(2)        ! Parameters for data section
      REAL       block_r(20)       ! Misc. real parameters

!     Scalar arguments with INTENT(INOUT):

      INTEGER    width             ! Number of bits which should be
                                   ! used to encode each data value

!     Array arguments with INTENT(INOUT):

      REAL       fp_data(len_fp)   ! Field of data to be encoded
      INTEGER    block_0(4)        ! Parameters for indicator section
      INTEGER    block_1(79)       !6.1 Parameters for product definition
                                   ! section
      INTEGER    block_2(20)       ! Parameters for grid description
                                   ! section

!     Scalar arguments with INTENT(OUT):

      INTEGER    word              ! Number of elements of mesg
                                   ! occupied by encoded message

!     Array arguments with INTENT(OUT):

      INTEGER    mesg(len_mesg)    ! Encoded GRIB message
      INTEGER    work_int1(*)      ! Workspace
      INTEGER    work_int2(*)      ! Workspace
      REAL       work_re1(*)       ! REAL workspace (no longer used)

!     Error status:

      INTEGER    error             ! +ve => warning or fatal error
      
!6.1  local parameters:

      INTEGER    PP_BIT_LENGTH     !6.1 number of bits stored for pp
      PARAMETER (PP_BIT_LENGTH=32) !6.1 header info from Super Stash

!     Local Scalars:

!     Used throughout routine

      INTEGER    jgrib           ! Used in yucky equivalence
      INTEGER    index1          ! Loop indices
      INTEGER    index2
      INTEGER    err_u           ! Unit number actually used for error
                                 ! messages
      INTEGER    bin_sign        ! Binary sign
      INTEGER    length          ! Safe place for length of data array
      INTEGER    byte            ! Size of chunk to pack
      INTEGER    nval            ! Number of chunks to pack
      INTEGER    off             ! Bit offset in message
      INTEGER    loop            !6.1 Loop control for Super Stash

!     Used in block 2

      INTEGER    blk_size        ! Length of block 2
      INTEGER    octets          ! Position of vert coords, or 255

!     Used in block 3

      INTEGER    bmap_inc        ! Bit-map included flag
      INTEGER    grid_size       ! Grid-size
      INTEGER    noct            ! Number of octets in block 3
      INTEGER    ibof            ! Offset of end of block 3
      INTEGER    len_bk3         ! Length of block 3


!     Used in block 4

      INTEGER    max_row_i       ! Maximum value in row, as an integer
      INTEGER    q_grid          ! Quasi-regular grid flag
      INTEGER    expo            ! Exponent of reference value
      INTEGER    manti           ! Mantissa of reference value
      INTEGER    len_data        ! Number of data values in block 4
      INTEGER    nub             ! Number of unused bits
      INTEGER    flag            ! Flag for block 4
      INTEGER    rep             ! 1 if spherical harmonics
      INTEGER    pac             ! 1 if row-by-row packing requested
      INTEGER    integ           ! 1 if data is integer
      INTEGER    cont            ! 1 if flag continued in o14
      INTEGER    scale           ! Scale factor
      INTEGER    num_rows        ! Number of rows in grid
      INTEGER    num_cols        ! Number of columns in grid
      INTEGER    n1              ! Octet at which 1st order data
                                 ! starts
      INTEGER    n2              ! Octet at which 2nd order data
                                 ! starts
      INTEGER    oct_1st         ! No of octets for 1st order data
      INTEGER    bits_1st        ! Offset of 1st order data
      INTEGER    data_pt         ! Number of values scaled so far
      INTEGER    data_pos        ! Pointer to position in data array
      INTEGER    row_pos         ! Pointer to position in row
      INTEGER    bits_bk4        ! Length of block 4 in bits
      INTEGER    oct_bk4         ! Length of block 4 in octets
      INTEGER    unused          ! Number of bits unused at end of
                                 ! block 4

!     Temporary values and error indicators

      INTEGER    temp_int        ! Temporary integer
      INTEGER    temp_wordbk4    ! Position of length of block 4
      INTEGER    temp_offbk4     ! Offset of length of block 4
      INTEGER    temp_err        ! Error indicator for subroutines
      INTEGER    temp_word       ! Pointer to start of data block
      INTEGER    temp_off        ! Pointer to offset of data block
      INTEGER    temp_scale2     ! Temporary scale factor
      INTEGER    temp_wordwid    ! Pointer to start of widths
      INTEGER    temp_offwid     ! Pointer to offset of widths
      INTEGER    err_y           ! Error in year
      INTEGER    err_m           ! Error in month
      INTEGER    err_d           ! Error in day
      INTEGER    err_h           ! Error in hour
      INTEGER    err_min         ! Error in minute
      INTEGER    latest_err_stat ! Error status from most recent
                                 ! subroutine call		 
# if defined (HP) || defined (CRAY) || defined (SX6)                /*6.5*/
      INTEGER    IN_DATA(LEN_FP) !6.0 Integer array used to represent Real array'd
      INTEGER    I               !6.0 Loop control for conversion of IN_DATA
# endif
      REAL       zgrib           ! Used in icky equivalence
      REAL       real_scale      ! Factor used to scale data
      REAL       row_max         ! Maximum value in row
      REAL       row_min         ! Minimum value in row
      REAL       data_max        ! Maximum value in field
      REAL       data_min        ! Minimum value in field

      REAL       temp_scale      ! Used in calculation of scale factor

      CHARACTER*80 version       ! Used to hold version text to appear
                                 ! in load module
!     Local Arrays:

      INTEGER    lat(2)          ! Store for latitude/long. values
      INTEGER    store(85)       !6.1 Temporary array

!     Functions and subroutines used:

      EXTERNAL   SBytes
      EXTERNAL   SByte
      EXTERNAL   Offset
      EXTERNAL   Prtbin
      EXTERNAL   Orig_Centre_Check
      EXTERNAL   Parameter_Check
      EXTERNAL   Level_Desc_Enc
      EXTERNAL   Time_Unit_Check
      EXTERNAL   Time_Range_Enc
      EXTERNAL   ConFP
      INTEGER    Ceil_Log2
      EXTERNAL   Ceil_Log2
      REAL       Realarr_Min
      EXTERNAL   Realarr_Min
      REAL       Realarr_Max
      EXTERNAL   Realarr_Max
      INTEGER    Intarr_Min
      EXTERNAL   Intarr_Min
      INTEGER    Intarr_Max
      EXTERNAL   Intarr_Max

!-    End of header

      EQUIVALENCE (ZGRIB,JGRIB)

!     Set version text

      version='grib_edtn_1_enc.f,v 4.14 98/07/21 13:27:00 ccae Rel $'

!     Clear the error indicator.

      ERROR = 0

!     Check for a sensible unit number for error messages.

      IF ( err_unit .GT. 0 ) THEN
        err_u = err_unit
      ELSE
        err_u = 6
      END IF

      IF ( err_unit .LT. 0 ) THEN
        IF ( msglvl .LT. 2 ) THEN
          WRITE (UNIT = err_u, FMT = '(A, I8, A)')
     &      ' WARNING - ', err_unit, ' is an illegal unit number' //
     &      ' for error messages. Sending them to unit 6.'
        END IF
      END IF

      LENGTH = LEN_FP

!     Initialise MESG array to zeroes                             !6.4

      DO LOOP=1,LEN_MESG                                          !6.4
        MESG(LOOP) = 0                                            !6.4
      END DO                                                      !6.4
      
!     ***************************************************************
!
!     BLOCK 0 - INDICATOR BLOCK.
!
!     ***************************************************************

!     The letters G R I B are inserted in the
!     first 4 elements of 'STORE'.

      STORE(1) = 71
      STORE(2) = 82
      STORE(3) = 73
      STORE(4) = 66

!     Set up pointer to first bit of message.

      WORD = 1
      BYTE = 8
      NVAL = 4
      OFF  = 0

!     Pack 4 8-bit fields in block 0 of coded array.

      CALL SBYTES (MESG(WORD),STORE(1),OFF,BYTE,0,NVAL)
      CALL OFFSET (OFF,NVAL,WORD,BYTE,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN

!     Keep pointers to beginning of length field.

      TEMP_WORDBK4 = WORD
      TEMP_OFFBK4  = OFF

!     Leave length of field as 0 for time being.

      CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN

!     Pack edition number - 1

      CALL SBYTE (MESG(WORD),BLOCK_0(1),OFF,8)
      CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN

!     ***************************************************************
!
!               BLOCK 1 - PRODUCT DEFINITION BLOCK
!
!     NON-STANDARD GRIB LEFT     -  length of block 1 set to 28.
!
!     ***************************************************************

!     Length of block 1 (in octets) in first 3 octets.

!6.1 If not a Super Stash GRIB message then the number of Octets
!6.1 is 28. However if Super Stash then there will be 220 Octets
!6.1 The sub centre number is stored in BLOCK_1(34) 2 indicates  
!6.1 SuperStash.
      IF (BLOCK_1(34).NE.2) THEN                                  !6.1
        STORE(1) = 0    !  Length can be variable.
        STORE(2) = 0    !  28 acceptable for edition 1.
        STORE(3) = 28   !
      ELSE                                                        !6.1
        STORE(1) = 0                                              !6.1
        STORE(2) = 0                                              !6.1
        STORE(3) = 220                                            !6.1
        
!6.1 Only for Super Stash set STORE(29) to STORE(40) to 0 as these Octets 
!6.1 are reserved
        DO LOOP = 29, 40                                          !6.1
          STORE(LOOP) = 0                                         !6.1
        ENDDO                                                     !6.1
        
!6.1 Now set STORE(41) to STORE(85) to hold the Super Stash pp header
!6.1 Information.
        DO LOOP = 41, 85                                          !6.1
          STORE(LOOP) = BLOCK_1 (LOOP - 6)                        !6.1
        ENDDO                                                     !6.1
         
      ENDIF                                                       !6.1

!     Fourth octet is Table 2 version number.

      IF (BLOCK_0(2) .LT. 0 .OR. BLOCK_0(2) .GT. 255) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U, 9113) BLOCK_0(2)
 9113   FORMAT (' ERROR - invalid Table 2 version number ', I3)
        RETURN
      ENDIF

      IF ( BLOCK_0(2) .LT. 1 .OR. BLOCK_0(2) .GT. 3) THEN
         ERROR = 2
         IF (MSGLVL .LT. 2) WRITE (ERR_U,9101) BLOCK_0(2)
 9101    FORMAT (' WARNING - centre-defined Table 2 number ', I3)
      ENDIF

      STORE(4) = BLOCK_0(2)

!     Identification of originating centre

      CALL Orig_Centre_Check(block_1(1), msglvl, err_u,
     &  latest_err_stat)
      error = Max(error, latest_err_stat)
      IF ( error .EQ. 3 ) RETURN
      store(5) = block_1(1)

!     Model identification

      STORE(6) = BLOCK_1(2)

      IF (BLOCK_1(2) .LT. 0 .OR. BLOCK_1(2) .GT. 255) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9103) BLOCK_1(2)
 9103   FORMAT (' ERROR - invalid model identification ', I4)
        RETURN
      ENDIF

!     Grid definition

      STORE(7) = BLOCK_1(3)

      IF (BLOCK_1(3) .LT. 0 .OR. BLOCK_1(3) .GT. 255) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9104) BLOCK_1(3)
 9104   FORMAT (' ERROR - invalid grid identification ', I4)
        RETURN
      ENDIF

!     Block indicator flags ( code table 1 ).
!
!     BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!         none             00000000               0
!          2               10000000             128
!          3               01000000              64
!         2 and 3          11000000             192

      STORE(8) = BLOCK_1(4)

!     Shift 2-bit flag field right by 6 bits to facilitate checking.

      TEMP_INT = BLOCK_1(4) / 64

      IF (TEMP_INT.LT.0.OR.TEMP_INT.GT.3)    ERROR = 3

!     Check if uncatalogued grid is described.

      IF (BLOCK_1(3).EQ.255.AND.BLOCK_1(4).EQ.0)  ERROR = 3
      IF (BLOCK_1(3).EQ.255.AND.BLOCK_1(4).EQ.64) ERROR = 3

!     If a grid catalogue number is given grid should still be
!     described in section 2. Check for this and print warning.

      IF (BLOCK_1(3).NE.255.AND.(BLOCK_1(4).EQ.0.OR.BLOCK_1(4).EQ.64))
     &  THEN
        ERROR = 2
        IF (MSGLVL .LT. 2) WRITE( UNIT = ERR_U, FMT = '(A)' )
     &    ' WARNING - catalogued grid not described in section 2.'
      ENDIF

      IF (ERROR .EQ. 3) THEN
        CALL PRTBIN (BLOCK_1(4),8,TEMP_INT,TEMP_ERR)
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9105) TEMP_INT
 9105   FORMAT (' ERROR - invalid block indicator flag ', I8.8)
        RETURN
      ENDIF

!     Identification of parameter

      CALL Parameter_Check(block_1(5), msglvl, err_u,
     &  latest_err_stat)
      error = Max(error, latest_err_stat)
      IF ( error .EQ. 3 ) RETURN
      store(9) = block_1(5)

!     Level description

      CALL Level_Desc_Enc(block_1(6), block_1(7), block_1(8),
     &  msglvl, err_u, store(10), latest_err_stat)
      error = Max(error, latest_err_stat)
      IF ( error .EQ. 3 ) RETURN

!     Date and time. check valid ranges and missing data values.

      STORE(13) = BLOCK_1(9)
      ERR_Y = 0
      IF (BLOCK_1(9).LT.1.OR.BLOCK_1(9).GT.100) ERR_Y = 1     !4.15 !4.16

      STORE(14) = BLOCK_1(10)
      ERR_M = 0
      IF (BLOCK_1(10).LT.1.OR.BLOCK_1(10).GT.12) ERR_M = 1

      STORE(15) = BLOCK_1(11)
      ERR_D = 0
      IF (BLOCK_1(11).LT.1.OR.BLOCK_1(11).GT.31) ERR_D = 1

      STORE(16) = BLOCK_1(12)
      ERR_H = 0
      IF (BLOCK_1(12).LT.0.OR.BLOCK_1(12).GT.23) ERR_H = 1

      STORE(17) = BLOCK_1(13)
      ERR_MIN = 0
      IF (BLOCK_1(13).LT.0.OR.BLOCK_1(13).GT.59) ERR_MIN = 1

      IF ( (ERR_Y + ERR_M + ERR_D + ERR_H + ERR_MIN) .NE. 0) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9109) BLOCK_1(9),
     &    BLOCK_1(10),BLOCK_1(11),BLOCK_1(12),BLOCK_1(13)
 9109   FORMAT (' ERROR - invalid date/time ', 3I2, ' / ', 2I2)
        RETURN
      ENDIF

!     Unit of time

      CALL Time_Unit_Check(block_1(14), msglvl, err_u,
     &  latest_err_stat)
      error = Max(error, latest_err_stat)
      IF ( error .EQ. 3 ) RETURN
      store(18) = block_1(14)

!     Time range(s) and time range indicator

      CALL Time_Range_Enc(block_1(15), block_1(16), block_1(17),
     &  block_1(18), msglvl, err_u, store(19), latest_err_stat)
      error = Max(error, latest_err_stat)
      IF ( error .EQ. 3 ) RETURN

!     Octet 24 is described as "Number missing from averages or
!     accumulations". This definition is too ambiguous for any
!     sensible use to be made of the octet, so just hard-code it
!     to zero.

      STORE(24) = 0

!     Century of reference time

      IF (BLOCK_1(19).GT.255) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9112) BLOCK_1(19)
 9112   FORMAT (' ERROR - invalid century ', I3)
        RETURN
      ENDIF

      STORE(25) = BLOCK_1(19)

!     Sub centre ID
      IF (BLOCK_1(34).EQ.2) THEN                                 !6.3
        STORE(26) = BLOCK_1(34)                                  !6.3
      ELSE                                                       !6.3 
        STORE(26) = 0                                            !6.3
      ENDIF                                                      !6.3
        
!     Decimal scale factor

      TEMP_INT = BLOCK_1(20)
      BIN_SIGN   = 0
      IF (TEMP_INT.LT.0) THEN
        BIN_SIGN   = 32768
        TEMP_INT = - TEMP_INT
      ENDIF
      BLOCK_1(20) = TEMP_INT + BIN_SIGN

!     8 low order bits in STORE(28); high order bits in STORE(27)

      STORE(27) = BLOCK_1(20)
      STORE(28) = STORE(27)
      STORE(27) = STORE(27) / 256
      STORE(28) = STORE(28) - STORE(27) * 256

!6.1  If BLOCK_1(34) is set to 2 then Super Stash
      IF (BLOCK_1(34).EQ.2) THEN
      
!6.1 Pack 40 8-bit fields of block 1 in coded array, if
!6.1 Sub-centre is 2 (

        BYTE = 8
        NVAL = 40                                                  !6.1

        CALL SBYTES (MESG(WORD),STORE(1),OFF,BYTE,0,NVAL)
        CALL OFFSET (OFF,NVAL,WORD,BYTE,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
      
!6.1  Pack 45 32-bit fields of block 1 in coded array.

        NVAL = 45                                                  !6.1

        CALL SBYTES (MESG(WORD),STORE(41),OFF,PP_BIT_LENGTH,0,NVAL)!6.2
        CALL OFFSET (OFF,NVAL,WORD,PP_BIT_LENGTH,WORD_SIZE,        !6.1
     &               LEN_MESG,ERROR)                               !6.1
        IF (ERROR .EQ. 3) RETURN

!6.1 Else not Superstash so only Twenty Eight 8-bit values to store
      ELSE                                                         !6.1
      
        BYTE = 8                                                   !6.1
        NVAL = 28                                                  !6.1

        CALL SBYTES (MESG(WORD),STORE(1),OFF,BYTE,0,NVAL)          !6.1
        CALL OFFSET (OFF,NVAL,WORD,BYTE,WORD_SIZE,LEN_MESG,ERROR)  !6.1
        IF (ERROR .EQ. 3) RETURN                                   !6.1
      ENDIF                                                        !6.1
        

!     ***************************************************************
!
!                 BLOCK 2 - GRID DESCRIPTION BLOCK.
!
!     ***************************************************************

!     If grid description block is included..

      IF (BLOCK_1(4).EQ.128.OR.BLOCK_1(4).EQ.192) THEN

!       If quasi-regular grid, Q_GRID is set to 1.

        Q_GRID = 0

        IF (LEN_Q.GT.1) Q_GRID = 1

!       Length of grid description block.
!
!       Length = 32 octets => regular lat/long
!                             regular gaussian
!                             polar stereographic
!                             spherical harmonics
!
!              = 42 octets => rotated lat/long
!
!       Vertical coordinates are added.
!       List of number of points per row added for
!       quasi-regular grids.

        IF (BLOCK_2(3).EQ.10) THEN
          TEMP_INT = 42
        ELSE
          TEMP_INT = 32
        ENDIF

        BLK_SIZE = TEMP_INT

!       Add on number of vertical coordinates and
!       space for quasi-regular grid.

        IF (BLOCK_2(1).NE.0) BLK_SIZE = TEMP_INT + BLOCK_2(1) * 4
        IF (Q_GRID.EQ.1)     BLK_SIZE = BLK_SIZE + LEN_Q * 2

        CALL SBYTE (MESG(WORD),BLK_SIZE,OFF,24)
        CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

!       Number of vertical coordinate parameters.

        IF (BLOCK_2(1).GT.255) THEN
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE (ERR_U,9201) BLOCK_2(1)
 9201     FORMAT (' ERROR - too many vertical coordinates - ', I3)
          RETURN
        ENDIF

        CALL SBYTE (MESG(WORD),BLOCK_2(1),OFF,8)
        CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

!       This byte is either PV, PL or 255.
!       Position of vertical coordinates takes precendence
!       over that of quasi-regular grid.

        OCTETS = 0

        IF ((BLOCK_2(1).EQ.0).AND.(Q_GRID.EQ.0)) OCTETS = 255
        IF ((BLOCK_2(1).NE.0).OR.(Q_GRID.EQ.1))  OCTETS = TEMP_INT + 1

        CALL SBYTE (MESG(WORD),OCTETS,OFF,8)
        CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

!       Data representation type.

        IF ( (BLOCK_2(3) .NE.  0) .AND. (BLOCK_2(3) .NE.  1) .AND.
     &       (BLOCK_2(3) .NE.  4) .AND. (BLOCK_2(3) .NE.  5) .AND.
     &       (BLOCK_2(3) .NE. 10) .AND. (BLOCK_2(3) .NE. 50)     ) THEN
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE (ERR_U,9203) BLOCK_2(3)
 9203     FORMAT (' ERROR - invalid data representation type - ',
     &            I3)
          RETURN
        ENDIF

        CALL SBYTE (MESG(WORD),BLOCK_2(3),OFF,8)
        CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

!       Lat/long, gaussian grid or mercator.

        IF (BLOCK_2(3).EQ.0.OR.BLOCK_2(3).EQ.4.OR.BLOCK_2(3).EQ.1.
     &      OR.BLOCK_2(3).EQ.10) THEN

!         Quasi-regular grid. Either Ni and Di or Nj and Dj
!         should be coded as missing (all bits set to 1).

          IF (Q_GRID.EQ.1) THEN

            IF (BLOCK_2(4).NE.0.AND.BLOCK_2(5).NE.0) THEN
              ERROR = 3
              IF (MSGLVL .LT. 3) WRITE (ERR_U,2910)
 2910         FORMAT (' ERROR - for a quasi regular grid',
     &                ' either Ni or Nj should be 0.')
              RETURN
            ENDIF

            IF (BLOCK_2(4).EQ.0.AND.BLOCK_2(5).EQ.0) THEN
              ERROR = 3
              IF (MSGLVL .LT. 3) WRITE (ERR_U,2910)
              RETURN
            ENDIF

            DO INDEX1 = 1 , 2
              IF (BLOCK_2(INDEX1+3).EQ.0) THEN
                BLOCK_2(INDEX1+3)  = 65535   ! points along row/column
                BLOCK_2(INDEX1+10) = 65535   ! direction increment
              ENDIF
            END DO

!           For a quasi regular gaussian grid, bit 3 of the
!           scanning mode flags must be set to 0.

            IF (BLOCK_2(3).EQ.4) THEN

!             Shift scanning mode flag right to make checking
!             easy.

              TEMP_INT = BLOCK_2(13) / 32

              IF ((MOD(TEMP_INT,2)).EQ.1) THEN
                ERROR = 3
                CALL PRTBIN(BLOCK_2(13),8,TEMP_INT,TEMP_ERR)
                IF (MSGLVL .LT. 3) WRITE(ERR_U,9211) TEMP_INT
 9211           FORMAT(' ERROR - invalid scanning mode ',
     &                 I8.8, ' for quasi regular gaussian grid.')
                RETURN
              ENDIF

            ENDIF             ! end of gaussian quasi section.

          ENDIF               ! end of quasi section.

!         Number of lat/long points.

          CALL SBYTES (MESG(WORD),BLOCK_2(4),OFF,16,0,2)
          CALL OFFSET (OFF,2,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Lat/long of origin.
!         Sign bit set to 1 if values are negative.

          IF (ABS(BLOCK_2(6)).GT.90000)  ERROR = 3
          IF (ABS(BLOCK_2(7)).GT.360000) ERROR = 3

          IF (ERROR .EQ. 3) THEN
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9204) BLOCK_2(6),
     &        BLOCK_2(7)
 9204       FORMAT (' ERROR - invalid lat/long value ',
     &              I5, 2X, '/', I6)
            RETURN
          ENDIF

          DO INDEX1 = 1 , 2
            LAT(INDEX1) = BLOCK_2(INDEX1+5)
            BIN_SIGN   = 0
            IF (LAT(INDEX1).LT.0) THEN
              BIN_SIGN   = 8388608
              LAT(INDEX1) = - LAT(INDEX1)
            ENDIF
            LAT(INDEX1) = LAT(INDEX1) + BIN_SIGN
          END DO

          CALL SBYTES (MESG(WORD),LAT(1),OFF,24,0,2)
          CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Resolution flag.
!
!         INCREMENTS       BINARY CODE     DECIMAL VALUE
!
!          NOT GIVEN        00000000              0
!           GIVEN           10000000            128
!
!         ECCENTRICITY     BINARY CODE     DECIMAL VALUE
!
!         EARTH SPHERICAL   00000000              0
!         1/f = 1/297.0     01000000             56
!
!         U AND V          BINARY CODE     DECIMAL VALUE
!
!         RELATIVE TO E&N   00000000              0
!         RELATIVE TO X&Y   00001000              8
!
!         Shift 5 bit flag field to low order bit

          TEMP_INT = BLOCK_2(8) / 8

          IF (TEMP_INT.LT.0.OR.TEMP_INT.GT.25) THEN
            ERROR = 3
            CALL PRTBIN (BLOCK_2(8),8,TEMP_INT,TEMP_ERR)
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9205) TEMP_INT
 9205       FORMAT (' ERROR - invalid resolution/cmpt flag ', I8.8)
            RETURN
          ENDIF

!         Set to all 1-bits unused increment fields.

          IF (BLOCK_2(8).EQ.0) THEN
            BLOCK_2(11) = 65535  ! 1111111111111111 binary
            BLOCK_2(12) = 65535  ! 1111111111111111 binary
          ENDIF

          CALL SBYTE (MESG(WORD),BLOCK_2(8),OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Lat/long of extreme point.

          IF (ABS(BLOCK_2(9)).GT.90000)  ERROR = 3
          IF (ABS(BLOCK_2(10)).GT.360000) ERROR = 3

          IF (ERROR .EQ. 3) THEN
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9204) BLOCK_2(9),
     &        BLOCK_2(10)
            RETURN
          ENDIF

!         Sign bit set to 1 if values are negative.

          DO INDEX1 = 1 , 2
            LAT(INDEX1) = BLOCK_2(INDEX1+8)
            BIN_SIGN   = 0
            IF (LAT(INDEX1).LT.0) THEN
              BIN_SIGN   = 8388608
              LAT(INDEX1) = - LAT(INDEX1)
            ENDIF
            LAT(INDEX1) = LAT(INDEX1) + BIN_SIGN
          END DO

          CALL SBYTES (MESG(WORD),LAT(1),OFF,24,0,2)
          CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Direction increments / number of latitude lines.

          CALL SBYTES (MESG(WORD),BLOCK_2(11),OFF,16,0,2)
          CALL OFFSET (OFF,2,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Scanning mode flags.
!
!         VALID VALUES       VALUE / 32
!          BINARY             DECIMAL
!
!           00000000               0
!           10000000               4
!           01000000               2
!           11000000               6
!           00100000               1
!           10100000               5
!           01100000               3
!           11100000               7
!
!         Shift 3 bit flag field to low order bits.

          TEMP_INT = BLOCK_2(13) / 32

          IF (TEMP_INT.LT.0.OR.TEMP_INT.GT.7) THEN
            ERROR = 3
            CALL PRTBIN (BLOCK_2(13),8,TEMP_INT,TEMP_ERR)
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9206) TEMP_INT
 9206       FORMAT (' ERROR - invalid scanning mode flags ', I8.8)
            RETURN
          ENDIF

          CALL SBYTE (MESG(WORD),BLOCK_2(13),OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Mercator needs 3 more octets for Latin.
!
!         Check valid Latin - MORE THAN ONE POSSIBLE?

          IF (BLOCK_2(3).EQ.1) THEN

            IF (Q_GRID.EQ.1) THEN
              ERROR = 3
              IF (MSGLVL .LT. 3) WRITE (ERR_U,9212)
 9212         FORMAT (' ERROR - quasi regular mercator ',
     &                'not defined.')
              RETURN
            ENDIF

            IF (ABS(BLOCK_2(14)).GT.90000) THEN
              ERROR = 3
              IF (MSGLVL .LT. 3) WRITE (ERR_U,9207) BLOCK_2(14)
 9207         FORMAT (' ERROR - invalid latin value - ', I5)
              RETURN
            ENDIF

!           Sign bit set to 1 if values are negative.

            LAT(1) = BLOCK_2(14)
            BIN_SIGN   = 0
            IF (LAT(1).LT.0) THEN
              BIN_SIGN   = 8388608
              LAT(1) = - LAT(1)
            ENDIF
            LAT(1) = LAT(1) + BIN_SIGN

            CALL SBYTE (MESG(WORD),LAT(1),OFF,24)
            CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN

!           Skip one reserved octet.

            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN

          ENDIF

          IF (BLOCK_2(3).NE.1) THEN

!           Skip 4 reserved octets.

            CALL OFFSET (OFF,4,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN

          ENDIF

!         Rotated lat/long grid.  !6.4

          IF (BLOCK_2(3).EQ.10) THEN

!           Lat/long of southern pole.

            IF (ABS(BLOCK_2(14)).GT.90000)  ERROR = 3
            IF (ABS(BLOCK_2(15)).GT.360000) ERROR = 3

            IF (ERROR .EQ. 3) THEN
              IF (MSGLVL .LT. 3) WRITE (ERR_U,9204) BLOCK_2(14),
     &          BLOCK_2(15)
              RETURN
            ENDIF

!           Sign bit set to 1 if values are negative.

            DO INDEX1 = 1 , 2
              LAT(INDEX1) = BLOCK_2(INDEX1+13)
              BIN_SIGN   = 0
              IF (LAT(INDEX1).LT.0) THEN
                BIN_SIGN   = 8388608
                LAT(INDEX1) = - LAT(INDEX1)
              ENDIF
              LAT(INDEX1) = LAT(INDEX1) + BIN_SIGN
            END DO

            CALL SBYTES (MESG(WORD),LAT(1),OFF,24,0,2)
            CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN

!           Angle of rotation.
!           Real number, so covert to GRIB format.

            CALL CONFP (BLOCK_R(1),EXPO,MANTI)

            CALL SBYTE (MESG(WORD),EXPO,OFF,8)
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
            CALL SBYTE (MESG(WORD),MANTI,OFF,24)
            CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN

          ENDIF

        ENDIF

!       Spherical harmonic data.

        IF (BLOCK_2(3).EQ.50) THEN

!         Pentagonal resolution parameters.

          CALL SBYTES (MESG(WORD),BLOCK_2(4),OFF,16,0,3)
          CALL OFFSET (OFF,3,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Representation type and mode.
!
!         !! no checks made on validity of these values !!

          CALL SBYTES (MESG(WORD),BLOCK_2(7),OFF,8,0,2)
          CALL OFFSET (OFF,2,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Skip 18 reserved octets.

          CALL OFFSET (OFF,18,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

        ENDIF

!       Polar Stereographic.

        IF (BLOCK_2(3).EQ.5) THEN

!         Number of lat/long points.

          CALL SBYTES (MESG(WORD),BLOCK_2(4),OFF,16,0,2)
          CALL OFFSET (OFF,2,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Lat/long of origin.
!         Sign bit set to 1 if values are negative.

          IF (ABS(BLOCK_2(6)).GT.90000)  ERROR = 3
          IF (ABS(BLOCK_2(7)).GT.360000) ERROR = 3

          IF (ERROR .EQ. 3) THEN
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9204) BLOCK_2(6),
     &        BLOCK_2(7)
            RETURN
          ENDIF

          DO INDEX1 = 1 , 2
            LAT(INDEX1) = BLOCK_2(INDEX1+5)
            BIN_SIGN   = 0
            IF (LAT(INDEX1).LT.0) THEN
              BIN_SIGN   = 8388608
              LAT(INDEX1) = - LAT(INDEX1)
            ENDIF
            LAT(INDEX1) = LAT(INDEX1) + BIN_SIGN
          END DO

          CALL SBYTES (MESG(WORD),LAT(1),OFF,24,0,2)
          CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Resolution flag.
!
!         REMENTS       BINARY CODE     DECIMAL VALUE
!
!         NOT GIVEN        00000000              0
!          GIVEN           10000000            128
!
!         ECCENTRICITY     BINARY CODE     DECIMAL VALUE
!
!         EARTH SPHERICAL   00000000              0
!         1/f = 1/297.0     01000000             56
!
!         U AND V          BINARY CODE     DECIMAL VALUE
!
!         RELATIVE TO E&N   00000000              0
!         RELATIVE TO X&Y   00001000              8
!
!         Shift 5 bit flag field to low order bit

          TEMP_INT = BLOCK_2(8) / 8

          IF (TEMP_INT.LT.0.OR.TEMP_INT.GT.25) THEN
            ERROR = 3
            CALL PRTBIN (BLOCK_2(8),8,TEMP_INT,TEMP_ERR)
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9205) TEMP_INT
            RETURN
          ENDIF

!         Set to all 1-bits unused increment fields.

          IF (BLOCK_2(8).EQ.0) THEN
            BLOCK_2(11)  = 65535  ! 1111111111111111 binary
            BLOCK_2(12)  = 65535  ! 1111111111111111 binary
          ENDIF

          CALL SBYTE (MESG(WORD),BLOCK_2(8),OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Longitude parallel to Y-axis.

          IF (ABS(BLOCK_2(9)).GT.360000) THEN
            ERROR = 3
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9208) BLOCK_2(9)
 9208       FORMAT (' ERROR - invalid LoV value - ', I6)
            RETURN
          ENDIF

!         Sign bit set to 1 if values are negative.

          LAT(1) = BLOCK_2(9)
          BIN_SIGN   = 0
          IF (LAT(1).LT.0) THEN
            BIN_SIGN   = 8388608
            LAT(1) = - LAT(1)
          ENDIF
          LAT(1) = LAT(1) + BIN_SIGN

          CALL SBYTES (MESG(WORD),LAT(1),OFF,24,0,1)
          CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Direction increments / number of latitude lines.

          CALL SBYTES (MESG(WORD),BLOCK_2(10),OFF,24,0,2)
          CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Projection centre flag
!
!         Bit 1 = 0 if North Pole on plane  (=0d)
!               = 1 if South Pole on plane  (=128d)

          TEMP_INT = BLOCK_2(12) / 128

          IF (TEMP_INT.LT.0.OR.TEMP_INT.GT.1) THEN
            ERROR = 3
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9209) BLOCK_2(12)
 9209       FORMAT (' ERROR - invalid projection flag value - ', I6)
            RETURN
          ENDIF

          CALL SBYTE (MESG(WORD),BLOCK_2(12),OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Scanning mode flags.
!
!         VALID VALUES       VALUE / 32
!           BINARY             DECIMAL
!
!           00000000               0
!           10000000               4
!           01000000               2
!           11000000               6
!           00100000               1
!           10100000               5
!           01100000               3
!           11100000               7
!
!         Shift 3 bit flag field to low order bits.

          TEMP_INT = BLOCK_2(13) / 32

          IF (TEMP_INT.LT.0.OR.TEMP_INT.GT.7) THEN
            ERROR = 3
            CALL PRTBIN (BLOCK_2(13),8,TEMP_INT,TEMP_ERR)
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9206) TEMP_INT
            RETURN
          ENDIF

          CALL SBYTE (MESG(WORD),BLOCK_2(13),OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Skip 4 reserved octets.

          CALL OFFSET (OFF,4,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

        ENDIF

!       Add vertical coordinate parameters for hybrid levels.

        IF (BLOCK_2(1).NE.0) THEN

          DO INDEX1 = 1 , BLOCK_2(1)
            CALL CONFP (VERT_COORDS(INDEX1),EXPO,MANTI)
            CALL SBYTE (MESG(WORD),EXPO,OFF,8)
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
            CALL SBYTE (MESG(WORD),MANTI,OFF,24)
            CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
          END DO

        ENDIF

!       Number of points per row(column) for quasi regular
!       grids.

        IF (Q_GRID.EQ.1) THEN

!         Check that there's one value for each row(column).

          DO INDEX1 = 1 , 2
            IF (BLOCK_2(INDEX1+3).NE.65535) THEN
              IF (LEN_Q.NE.BLOCK_2(INDEX1+3)) THEN
                ERROR = 3
                IF (MSGLVL .LT. 3) WRITE (ERR_U,9213) LEN_Q,
     &            BLOCK_2(INDEX1+3)
 9213           FORMAT(' ERROR - list of number of points ',
     &                 'per row/column is ', I9.9, ' long but ',
     &                 'there are ', I9.9, ' rows/columns.')
                RETURN
              ENDIF
            ENDIF
          END DO

          CALL SBYTES(MESG(WORD),QUASI(1),OFF,16,0,LEN_Q)
          CALL OFFSET(OFF,LEN_Q,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

        ENDIF

      ENDIF

!     ***************************************************************
!
!             BLOCK 3 (IF REQUIRED) - BIT MAP BLOCK.
!
!     NON-STANDARD GRIB LEFT     - does not allow centre-defined map
!
!     ***************************************************************

      BMAP_INC = 0

      IF (BLOCK_1(4).EQ.64.OR.BLOCK_1(4).EQ.192) THEN

        BMAP_INC = 1

!       Check that bit-map length = grid-size.

        GRID_SIZE = 0

        IF (Q_GRID.EQ.0) THEN
          GRID_SIZE = BLOCK_2(4) * BLOCK_2(5)
        ELSE
          DO INDEX1 = 1 , LEN_Q
            GRID_SIZE = GRID_SIZE + QUASI(INDEX1)
          END DO
        ENDIF

        IF (LEN_BITMAP.NE.GRID_SIZE) THEN
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE(ERR_U,9301) LEN_BITMAP,
     &      GRID_SIZE
 9301     FORMAT(' ERROR - bit-map length ', I10,
     &           ' does not equal grid-size ', I10)
          RETURN
        ENDIF

!       Set up length of block 3.

        NUB  = 0
        NOCT = GRID_SIZE  /8
        IBOF = GRID_SIZE - NOCT * 8

!      Round to whole number of octets and calculate no. of bits
!      unused

        IF (IBOF.NE.0) THEN
          NUB  = 8 - IBOF
          NOCT = NOCT + 1
        ENDIF

!       Round to even number of octets,correcting no. bits unused

        IF (MOD(NOCT,2).NE.0) THEN
          NUB  = NUB + 8
          NOCT = NOCT + 1
        ENDIF

        LEN_BK3 = NOCT + 6

        CALL SBYTE(MESG(WORD),LEN_BK3,OFF,24)
        CALL OFFSET(OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
        IF(ERROR .EQ. 3) RETURN

!       Set up number of unused bits at the end of block.

        CALL SBYTE(MESG(WORD),NUB,OFF,8)
        CALL OFFSET(OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF(ERROR .EQ. 3) RETURN

!       Set octet 5-6 octet (numeric).
!
!       Does not allow centre-defined bit-map.

        CALL SBYTE(MESG(WORD),0,OFF,16)
        CALL OFFSET(OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
        IF(ERROR .EQ. 3) RETURN

!       Set up bit map.

        DO INDEX2 = 1 , LEN_BITMAP

          CALL SBYTE(MESG(WORD),BITMAP(INDEX2),OFF,1)
          CALL OFFSET(OFF,1,WORD,1,WORD_SIZE,LEN_MESG,ERROR)
          IF(ERROR .EQ. 3) RETURN

        END DO

!       Skip unused bits.

        CALL OFFSET(OFF,NUB,WORD,1,WORD_SIZE,LEN_MESG,ERROR)
        IF(ERROR .EQ. 3) RETURN
      ENDIF

!     ***************************************************************
!
!              BLOCK 4 - BINARY DATA BLOCK.
!
!     ***************************************************************

!     Retain pointers to word and bit position of binary data
!     block length field.

      TEMP_WORD = WORD
      TEMP_OFF = OFF

!     Leave length as 0 for the moment.  !6.4

      CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN

!     4 bit flag / 4 bit count of unused bits at end of block octet.
!
!
!     BIT         VALUE              MEANING
!
!      1            0                Grid-point data
!                   1                Spherical harmonics
!      2            0                Simple packing
!                   1                Complex/2nd order packing
!      3            0                F/P values represented
!                   1                Integer values represented
!      4            0                No flags at o14
!                   1                Extra flags at o14
!
!     Shift flag right 4 bits for testing.

      FLAG = BLOCK_4(1) / 16

      IF (FLAG.LT.0.OR.FLAG.GT.15) THEN
        ERROR = 3
        CALL PRTBIN (BLOCK_4(1),8,TEMP_INT,TEMP_ERR)
        IF (MSGLVL .LT. 3) WRITE (ERR_U,4901) TEMP_INT
 4901   FORMAT (' ERROR - invalid block 4 flag', I8.8)
        RETURN
      ENDIF

!     Flag field is BLOCK_4(1).

      CALL SBYTE (MESG(WORD),BLOCK_4(1),OFF,8)
      CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN

!     Spherical harmonics.

      REP = 0
      IF (FLAG.GE.8) REP = 1

!     Complex packing.

      PAC = 0
      IF ((MOD(FLAG/4,2)).EQ.1) THEN
        PAC = 1
        IF (MSGLVL .LT. 1) WRITE ( UNIT = ERR_U, FMT = '(A)' )
     &    ' NOTE - row by row packing used.'
      ENDIF

!     Integer/F.P.

      INTEG = 0
      IF ((MOD(FLAG/2,2)).EQ.1) THEN
        ERROR = 3
        INTEG= 1
        IF (MSGLVL .LT. 3) WRITE ( UNIT = ERR_U, FMT = '(A)' )
     &    ' ERROR - packing of integer data is not available.'
        RETURN
      ENDIF

!     Flag continued in o14.

      CONT = 0
      IF ((MOD(FLAG,2)).EQ.1) THEN
        CONT= 1
      ENDIF

!     Find maximum and minimum values in data array. For
!     data in spherical harmonic form the first word contains
!     the real (0,0) coefficient, which is treated separately.

      LEN_DATA = LENGTH - REP

      data_max = Realarr_Max(fp_data(rep + 1), len_data)
      data_min = Realarr_Min(fp_data(rep + 1), len_data)

!     Scale factor. If WIDTH is passed, need to calculate scale
!     factor. Following method assumes that the largest packed
!     integer will fill WIDTH bits. If accuracy passed, it's
!     simple!
!
!     Calculate scale factor

      IF (WIDTH.NE.0) THEN
        TEMP_SCALE = (DATA_MAX-DATA_MIN)/(2**(WIDTH+1)-1)
        IF (TEMP_SCALE.NE.0.) TEMP_SCALE = ALOG(TEMP_SCALE)/ALOG(2.)
     &                                     + 2.
        TEMP_SCALE2 = MIN(INT(TEMP_SCALE),
     &                INT(TEMP_SCALE+SIGN(1.,TEMP_SCALE)))
        REAL_SCALE = 2.**TEMP_SCALE2
        SCALE = TEMP_SCALE2
      ELSE
        SCALE = BLOCK_0(4)
        REAL_SCALE = 2. ** SCALE
      ENDIF

!     Set sign bit (bit 16) and put scale factor into remaining
!     15 bits of coded 16 bit field.

      BIN_SIGN = 0
      IF (SCALE.LT.0) THEN
        BIN_SIGN  = 32768
        SCALE = - SCALE
      ENDIF
      SCALE = SCALE + BIN_SIGN

      CALL SBYTE (MESG(WORD),SCALE,OFF,16)
      CALL OFFSET (OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN

!     Minimum (reference) value.
!
!     Convert minimum value (DATA_MIN) to GRIB format (EXPO,MANTI)

      CALL CONFP (DATA_MIN,EXPO,MANTI)

      CALL SBYTE (MESG(WORD),EXPO,OFF,8)
      CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
      CALL SBYTE (MESG(WORD),MANTI,OFF,24)
      CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN

!     If WIDTH has not been specified, need to calculate it. This can
!     cause problems if the field is of constant value. In this case,
!     all we need to know is the reference value - the rest of the
!     field can be packed as 0s.

      IF (WIDTH.EQ.0) THEN

        TEMP_SCALE = NINT((DATA_MAX-DATA_MIN) / REAL_SCALE)

        IF (TEMP_SCALE .EQ. 0.0) THEN
          WIDTH = 1
        ELSE
          TEMP_INT = INT(TEMP_SCALE)
          width = Ceil_Log2(temp_int)
        ENDIF

      ENDIF

!     Pack the number of bits in each data value.

      CALL SBYTE (MESG(WORD),WIDTH,OFF,8)
      CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN

!     If spherical harmonics data, next 4 octets are different format.

      IF (REP.EQ.1) THEN

!       Second order spherical harmonics not supported.

        IF (PAC.EQ.1) THEN
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE( UNIT = ERR_U, FMT = '(A)' )
     &      ' ERROR - encoding of spherical harmonics with ' //
     &      'second order packing not yet supported.'
          RETURN
        ENDIF

!       Store real (0,0) coefficient in floating
!       point form.

        CALL CONFP (FP_DATA(1),EXPO,MANTI)

        CALL SBYTE (MESG(WORD),EXPO,OFF,8)
        CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
        CALL SBYTE (MESG(WORD),MANTI,OFF,24)
        CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
      ENDIF

!     ---------------------------
!     Simple packed data.
!     ---------------------------

      IF (PAC.EQ.0) THEN

!       Scale and store data values.

        DO INDEX1 = REP+1 , LENGTH
          JGRIB           = NINT ((FP_DATA(INDEX1)-DATA_MIN)
     &                            / REAL_SCALE)
          FP_DATA(INDEX1) = ZGRIB
        END DO

        LEN_DATA = LENGTH - REP

# if defined (HP) || defined (CRAY) || defined (SX6)               /*6.5*/
        DO I=1,LEN_DATA                                             !6.0
          IN_DATA(I)=TRANSFER(FP_DATA(REP+I),0)                     !6.0
        ENDDO                                                       !6.0
        CALL SBYTES (MESG(WORD),IN_DATA,OFF,WIDTH,0,LEN_DATA)       !6.0
        DO I=1,LEN_DATA                                             !6.0
          FP_DATA(REP+I)=TRANSFER(IN_DATA(I),0.0)                   !6.0
        ENDDO                                                       !6.0
# else
        CALL SBYTES (MESG(WORD),FP_DATA(REP+1),OFF,WIDTH,0,LEN_DATA)
# endif

        CALL OFFSET (OFF,LEN_DATA,WORD,WIDTH,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

      ENDIF

!     -----------------------
!     Second order packing.
!     -----------------------

      IF (PAC.EQ.1) THEN

!       Set up number of rows and no. of columns.

        IF (Q_GRID.EQ.0) THEN
          NUM_ROWS = BLOCK_2(5)
          NUM_COLS = BLOCK_2(4)
        ELSE
          NUM_ROWS = BLOCK_2(4)
          IF (NUM_ROWS.EQ.65535) NUM_ROWS = BLOCK_2(5)
        ENDIF

!       Pack octet number at which 1st order start.

        N1 = 22 + NUM_ROWS
        CALL SBYTE(MESG(WORD),N1,OFF,16)
        CALL OFFSET (OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)

!       Pack extended flags.

        TEMP_INT = 16
        CALL SBYTE(MESG(WORD),TEMP_INT,OFF,8)
        CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)

!       Find number octets needed for first order data.

        OCT_1ST = (WIDTH * NUM_ROWS) / 8
        BITS_1ST = (WIDTH * NUM_ROWS ) - (OCT_1ST * 8)

        IF (BITS_1ST.NE.0) THEN
          OCT_1ST = OCT_1ST + 1
          BITS_1ST = 8 - BITS_1ST
        ENDIF

!       Pack octet number at which 2nd order start.

        N2 = 22 + NUM_ROWS + OCT_1ST
        CALL SBYTE(MESG(WORD),N2,OFF,16)
        CALL OFFSET (OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

!       Pack number of rows.

        CALL SBYTE(MESG(WORD),NUM_ROWS,OFF,16)
        CALL OFFSET (OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

!       Pack number of grid points.

        CALL SBYTE(MESG(WORD),LEN_FP,OFF,16)
        CALL OFFSET (OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

!       Next octet is reserved.

        CALL SBYTE(MESG(WORD),0,OFF,8)
        CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

!       Keep pointers to widths and 1st order values.

        TEMP_WORDWID = WORD
        TEMP_OFFWID = OFF

!       Leave packed with 0 for time being.

        CALL OFFSET (OFF,NUM_ROWS,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

        CALL OFFSET (OFF,OCT_1ST,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

!       Scale and store data values.

        DATA_PT = 1

!       Set up pointer to position in FP_DATA - needed for
!       quasi regular grids.

        DATA_POS = 0

        DO INDEX1 = REP+1 , NUM_ROWS

          IF (Q_GRID.EQ.1) NUM_COLS = QUASI(INDEX1-REP)

!         ----------------
!         No bit-map.
!         ----------------

          IF (BMAP_INC.EQ.0) THEN

!           First get integer differences for row.

            DO INDEX2 = 1 , NUM_COLS
              WORK_INT1(INDEX2) = NINT((FP_DATA(INDEX2+DATA_POS)
     &                                 - DATA_MIN) / REAL_SCALE)
            END DO

!           Find the integer reference value for this row and store.

            max_row_i         = Intarr_Max(work_int1, num_cols)
            work_int2(index1) = Intarr_Min(work_int1, num_cols)
            ROW_MAX = REAL(MAX_ROW_I)
            ROW_MIN = REAL(WORK_INT2(INDEX1))

!           Check that there is a range of values.

            IF (ROW_MAX.EQ.ROW_MIN) THEN
              WORK_INT2(NUM_ROWS+INDEX1) = 0
            ELSE
              TEMP_INT = INT(ROW_MAX) - INT(ROW_MIN)
              work_int2(num_rows + index1) = Ceil_Log2(temp_int)
            ENDIF

!           Scale (relative to integer ref.) and pack.

            DO INDEX2 = 1 , NUM_COLS                   !6.4 (next line)
              WORK_INT1(INDEX2) = WORK_INT1(INDEX2) - WORK_INT2(INDEX1)
            END DO

!           Set-up ROW_POS pointer (just NUM_COLS with no bit-map).

            ROW_POS = NUM_COLS + 1

          ENDIF

!         ------------------
!         Bit-map present.
!         ------------------

          IF (BMAP_INC.EQ.1) THEN

!           First get integer differences for row.

            ROW_POS = 1

            DO INDEX2 = 1 , NUM_COLS
              IF (BITMAP(INDEX2+DATA_POS).EQ.1) THEN
                WORK_INT1(ROW_POS) = NINT((FP_DATA(DATA_PT)-DATA_MIN)
     &                                   /REAL_SCALE)
                DATA_PT = DATA_PT + 1
                ROW_POS = ROW_POS + 1
              ENDIF
            END DO

!           Find the integer reference value for this row and store.
!
!           Check that there are some values in the row.

            IF ((ROW_POS - 1).EQ.0) THEN
              MAX_ROW_I         = 0
              WORK_INT2(INDEX1) = 0
            ELSE
              max_row_i         = Intarr_Max(work_int1, row_pos - 1)
              work_int2(index1) = Intarr_Min(work_int1, row_pos - 1)
            ENDIF

            ROW_MAX = REAL(MAX_ROW_I)
            ROW_MIN = REAL(WORK_INT2(INDEX1))

!           Check that there is a range of values.

            IF (ROW_MAX.EQ.ROW_MIN) THEN
              WORK_INT2(NUM_ROWS+INDEX1) = 0
            ELSE
              TEMP_INT = INT(ROW_MAX) - INT(ROW_MIN)
              work_int2(num_rows + index1) = Ceil_Log2(temp_int)
            ENDIF

!           Scale (relative to integer ref.) and pack.

            DO INDEX2 = 1 , (ROW_POS - 1)              !6.4 (next line)
              WORK_INT1(INDEX2) = WORK_INT1(INDEX2) - WORK_INT2(INDEX1)
            END DO

          ENDIF

!         ----------------------------------
!         End of bit-map specific section.
!         ----------------------------------

!         Put scaled data values into MESG array.
!         (Not required if data range is zero)                    !6.4

          IF (WORK_INT2(NUM_ROWS+INDEX1).GT.0) THEN               !6.4
            CALL SBYTES (MESG(WORD),WORK_INT1(1),OFF,             !6.4
     &                   WORK_INT2(NUM_ROWS+INDEX1),0,ROW_POS-1)
            CALL OFFSET (OFF,ROW_POS-1,WORD,WORK_INT2(NUM_ROWS+INDEX1),
     &                   WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
          END IF                                                  !6.4

!         Increment pointer to location in input array

          DATA_POS = DATA_POS + NUM_COLS

        END DO

!       Now pack widths, stored in array
!       WORK_INT2(NUM_ROWS+1,NUM_ROWS*2)

        CALL SBYTES(MESG(TEMP_WORDWID),WORK_INT2(NUM_ROWS+1),
     &              TEMP_OFFWID,8,0,NUM_ROWS)
        CALL OFFSET(TEMP_OFFWID,NUM_ROWS,TEMP_WORDWID,8,
     &              WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

!       Now pack 1st order values.

        CALL SBYTES(MESG(TEMP_WORDWID),WORK_INT2(1),
     &              TEMP_OFFWID,WIDTH,0,NUM_ROWS)
        CALL OFFSET(TEMP_OFFWID,NUM_ROWS,TEMP_WORDWID,WIDTH,
     &              WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

!       Pad out to whole number of octets.

        IF (BITS_1ST.NE.0) THEN
          CALL OFFSET(TEMP_OFFWID,BITS_1ST,TEMP_WORDWID,1,
     &                WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
        ENDIF

      ENDIF

!     ------------------------------------
!     End of complex-packing block.
!     ------------------------------------

!     Enter length of binary data block, having ensured that
!     the length is an even number of octets.
!
!     Length of binary data block in bits.

      BITS_BK4 = (WORD-TEMP_WORD) * WORD_SIZE + OFF - TEMP_OFF

      OCT_BK4 = BITS_BK4 / 16
      OCT_BK4 = BITS_BK4 - ( OCT_BK4*16 )

!     Fill unused portion of last 2 octets with binary zeroes.

      UNUSED = 0
      IF (OCT_BK4.NE.0) THEN
        UNUSED = 16 - OCT_BK4
        CALL OFFSET (OFF,1,WORD,UNUSED,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
      ENDIF

      BITS_BK4 = (WORD-TEMP_WORD) * WORD_SIZE + OFF - TEMP_OFF

!     Enter length - in octets - of data block.

      BITS_BK4 = BITS_BK4 / 8
      CALL SBYTE (MESG(TEMP_WORD),BITS_BK4,TEMP_OFF,24)
      CALL OFFSET (TEMP_OFF,1,TEMP_WORD,24,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN

!     Enter number of unused bits in flag/bit count field.

      FLAG = (FLAG * 16) + UNUSED
      CALL SBYTE (MESG(TEMP_WORD),FLAG,TEMP_OFF,8)

!     ***************************************************************
!
!                      BLOCK 5 - END BLOCK.
!
!     ***************************************************************

!     Add 7 7 7 7 to coded data.

      STORE(1) = 55
      STORE(2) = 55
      STORE(3) = 55
      STORE(4) = 55

      CALL SBYTES (MESG(WORD),STORE(1),OFF,8,0,4)
      CALL OFFSET (OFF,4,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN

!     Enter total length into Block 0.

      IF (OFF.NE.0) THEN
        BLOCK_0(3) = ((WORD-1)*WORD_SIZE/8) + OFF/8
      ELSE
        BLOCK_0(3) = (WORD-1) *WORD_SIZE/8
      ENDIF

      CALL SBYTES (MESG(TEMP_WORDBK4),BLOCK_0(3),TEMP_OFFBK4,24,0,1)

!     Set any unused part of last word to binary zeroes.

      IF (OFF.EQ.0) WORD = WORD - 1

      RETURN
      END ! SUBROUTINE Grib_Edtn_1_Enc
