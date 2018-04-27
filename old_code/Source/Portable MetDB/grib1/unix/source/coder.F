!+    Encode a data field in GRIB format
! $Header: coder.F, 1, 27/02/2007 16:04:57, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Coder (fp_data, len_fp, vert_coords, len_vert,
     &  bitmap, len_bitmap, quasi, len_q, width, word_size,
     &  block_0, block_1, block_2, block_3, block_4, block_r,
     &  mesg, len_mesg, word, error, work_int1, work_int2, work_re1,
     &  err_unit, msglvl)

      IMPLICIT NONE

!     Description:
!       Encodes the data field fp_data and the various ancillary
!       information from the other input arguments as a GRIB message,
!       and returns it in mesg.
!
!       WARNING - Maximum number of bits per data value is 32. This is
!       non-standard GRIB.
!
!     Method:
!       <Say how it does it: refer to external documentation>
!       <If this routine is very complex, then include a
!        "pseudo code" description of it to make its structure
!        and method clear>
!
!     Current Code Owner: MetDB Team
!
!     History:
!       This file is under RCS control. See the RCS log for change
!       history information.
!
!     Code Description:
!       FORTRAN 77 with extensions recommended in the Met. Office
!       F77 Standard.
!
!$Revision: 1$
!$Date: 27/02/2007 16:04:57$
!
!$Log:
! 1    Met_DB_Project 1.0         27/02/2007 16:04:57    Stan Kellett    
!$
!Revision 6.2  2004/05/18 11:26:20  usmdb
!Add SX6 directives.
!
!Revision 6.1  2001/11/05 10:15:31  usmdb
!Superstash update.
!
!Revision 6.0  2001/08/09 10:28:36  usmdb
!replaces coder.f in old directory
!replaced equivalences as will not compile on nag f90 compiler
!As routine to get round roblems does not work on MVS version preproc.
!statements have been added.    Stan Kellett
!
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

      REAL       vert_coords(len_vert)  ! Vertical coordinate params,
                                        ! if level type is hybrid
                                        ! level
      INTEGER    bitmap(len_bitmap)     ! Bit map, if used
      INTEGER    quasi(len_q)           ! Length of each row or
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
      INTEGER    block_1(79)       ! Parameter for product definition !6.1
                                   ! section 
      INTEGER    block_2(20)       ! Parameters for grid description
                                   ! section

!     Scalar arguments with INTENT(OUT):

      INTEGER    word              ! Number of elements of mesg
                                   ! occupied by encoded message

!     Array arguments with INTENT(OUT):

      INTEGER    mesg(len_mesg)    ! Encoded GRIB message
      INTEGER    work_int1(*)      ! Workspace
      INTEGER    work_int2(*)
      REAL       work_re1(*)

!     Error status:

      INTEGER    error             ! +ve => warning or fatal error

!     Local Parameters:

      INTEGER    Max_Bits          ! Maximum width to pack values in
      PARAMETER (Max_Bits = 30)

!     Local Scalars:

      INTEGER    jgrib             ! Used in nasty equivalence
      INTEGER    byte              ! Size of chunk to pack
      INTEGER    nval              ! Number of chunks to pack
      INTEGER    off               ! Bit offset
      INTEGER    index1            ! Loop indices
      INTEGER    index2
      INTEGER    err_u             ! Unit number actually used for
                                   ! error messages
      INTEGER    blk_size          ! Size of block 2
      INTEGER    bin_sign          ! Binary sign
      INTEGER    manti             ! Mantissa of reference value
      INTEGER    expo              ! Exponent of reference value
      INTEGER    nub               ! Number of unused bits
      INTEGER    ngrd              ! Number of grid points
      INTEGER    noct              ! Number of octets in block 3
      INTEGER    ibof              ! Offset of end of block 3
      INTEGER    len_bk3           ! Length of block 3
      INTEGER    rep               ! 1 if spherical harmonics
      INTEGER    flag              ! Flag for block 4 
      INTEGER    len_data          ! Number of data values in block 4
      INTEGER    scale             ! Scale factor
      INTEGER    bits_bk4          ! Length of block 4 in bits
      INTEGER    oct_bk4           ! Length of block 4 in octets.
      INTEGER    unused            ! No of bits unused at end of blk 4

      INTEGER    temp_int          ! Store for temporary results
      INTEGER    temp_err          ! Error indicator for subroutines
      INTEGER    length            ! Safe place to store len_fp
      INTEGER    err_y             ! Error in year
      INTEGER    err_m             ! Error in month
      INTEGER    err_d             ! Error in day
      INTEGER    err_h             ! Error in hour
      INTEGER    err_min           ! Error in minute
      INTEGER    temp_word         ! Pointer to start of data block
      INTEGER    temp_off          ! Offset at start of data block
      INTEGER    temp_scale2       ! Temporary scale factor
      
# if defined (HP) || defined (CRAY) || defined (SX6)   /*6.2*/
      INTEGER    IN_DATA(LEN_FP)   !6.0 Temperary integer array used as integer
      				   ! representation of reals for F90 compiler
				   ! in Unix
      INTEGER    I                 ! loop control used in converting above
# endif

      REAL       zgrib             ! Used in horrible equivalence
      REAL       max_data          ! Maximum value in field
      REAL       min_data          ! Minimum value in field
      REAL       real_scale        ! Factor used in scaling data
      REAL       temp_scale        ! Used in calculation of scale
                                   ! factor

!     Local Arrays:

      INTEGER    lat(2)            ! Array to store lat/long values
      INTEGER    store(28)         ! Temporary array
      
!     Functions and subroutines used:

      EXTERNAL   Grib_Edtn_1_Enc
      EXTERNAL   Sbytes
      EXTERNAL   Sbyte
      EXTERNAL   Offset
      EXTERNAL   Prtbin
      EXTERNAL   ConFP
      INTEGER    Ceil_Log2
      EXTERNAL   Ceil_Log2
      REAL       Realarr_Min
      EXTERNAL   Realarr_Min
      REAL       Realarr_Max
      EXTERNAL   Realarr_Max 
        
      EQUIVALENCE (ZGRIB,JGRIB)   
        
! Revision Code:
      LOGICAL HEADSET
      CHARACTER*132 HEAD
      DATA HEADSET /.FALSE./
      
      IF (.NOT.HEADSET) THEN
        HEAD='
     &$Source: /home/us0400/grib/source/RCS/coder.F,v $
     &'//'$Date: 27/02/2007 16:04:57$ $Revision: 1$'
        HEADSET = .TRUE.
      ENDIF                
! End of Revision Code
!

!-    End of header

 
!     Clear error indicators.
      
      ERROR = 0

!     Set up the unit number for error messages.

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

!     Store length of field somewhere safe.
 
      LENGTH = LEN_FP
 
!     Check number of bits per data field.
 
      IF ( WIDTH .GT. WORD_SIZE .OR. WIDTH .GT. Max_Bits ) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U, 9000) WIDTH, WORD_SIZE,
     &    Max_Bits
 9000   FORMAT (' ERROR - number of bits per data value, ', I3,
     &              'exceeds word length, ', I3, ' or maximum ',
     &              ' permitted value, ', I3)
        RETURN
      ENDIF
 
!     Check edition number - if 1 then call Grib_Edtn_1_Enc.
 
      IF (BLOCK_0(1).EQ.1) THEN
        CALL Grib_Edtn_1_Enc (fp_data, len_fp, vert_coords,
     &    len_vert, bitmap, len_bitmap, quasi, len_q, width, word_size,
     &    block_0, block_1, block_2, block_3, block_4, block_r,
     &    mesg, len_mesg, word, error, work_int1, work_int2, work_re1,
     &    err_u, msglvl)
        RETURN
      ENDIF
 
!     Edition 0 is deprecated, all other versions are invalid.
 
      IF ( block_0(1) .EQ. 0 ) THEN
        error = 2
        IF ( msglvl .LT. 2 ) THEN
          WRITE ( UNIT = err_u, FMT = '(A)' )
     &      ' WARNING - Coding in edition 0 GRIB. Edition 0 GRIB ' //
     &      'is obsalescent.'
        END IF
      ELSE
        error = 3
        IF (msglvl .LT. 3) THEN
          WRITE ( UNIT = err_u, FMT = '(A, I8, A)' )
     &      ' ERROR - specified GRIB edition number, ', block_0(1),
     &       ', is undefined'
        END IF
        RETURN
      END IF

!     ***************************************************************
!
!                        BLOCK 0 - INDICATOR BLOCK.
!
!     ***************************************************************

!     The letters G R I B are inserted in the
!     first 4 elements of 'STORE'.
 
      STORE(1) = 71
      STORE(2) = 82
      STORE(3) = 73
      STORE(4) = 66
 
!     Set up pointers to first bit of output array.
 
      WORD  = 1
      BYTE  = 8
      NVAL  = 4
      OFF   = 0
 
!     Pack 4 8-bit fields in block 0 of coded array.
 
      CALL SBYTES (MESG(WORD),STORE(1),OFF,BYTE,0,NVAL)
      CALL OFFSET (OFF,NVAL,WORD,BYTE,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN

!     ***************************************************************
!
!                   BLOCK 1 - PRODUCT DEFINITION BLOCK
!
!     NON-STANDARD GRIB LEFT - length of block = 24 (ok for edn. 0).
!                              octet 24 = 0 - WMO definition is
!                              ambiguous.
!
!     ***************************************************************

!     Length of block 1 (in octets) in first 3 octets.
 
      STORE(1) = 0      ! length can be variable.
      STORE(2) = 0      ! 24 is acceptable for edition 0.
      STORE(3) = 24     ! Will be changed for edition 1.
 
!     Fourth octet is edition number - 0.
 
      STORE(4) = 0
 
!     Identification of centre
 
      STORE(5) = BLOCK_1(1)
      IF (BLOCK_1(1).LT.1.OR.BLOCK_1(1).GT.98) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9001) BLOCK_1(1)
 9001   FORMAT (' ERROR - invalid originating centre ', I3)
        RETURN
      ENDIF
 
!     Model identification
 
      STORE(6) = BLOCK_1(2)
      IF (BLOCK_1(2).LT.1.OR.BLOCK_1(2).GT.255) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9002) BLOCK_1(2)
 9002   FORMAT (' ERROR - invalid model identification ', I4)
        RETURN
      ENDIF
 
!     Grid definition
 
      STORE(7) = BLOCK_1(3)
      IF (BLOCK_1(3).LT.1.OR.BLOCK_1(3).GT.255) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9003) BLOCK_1(3)
 9003   FORMAT (' ERROR - invalid grid identification ', I4)
        RETURN
      ENDIF
 
!     Block indicator flags ( code table 1 ).
!
!     BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!         none             00000000               0
!          2               10000000             128
!          3               01000000              64
!       2 and 3            11000000             192
 
      STORE(8) = BLOCK_1(4)
 
!     Shift 2-bit flag field right by 6 bits to facilitate checking
 
      TEMP_INT = BLOCK_1(4) / 64

      IF (TEMP_INT.LT.0.OR.TEMP_INT.GT.3)    ERROR = 3
 
!     Check that uncatalogued grid is described.
 
      IF (BLOCK_1(3).EQ.255.AND.BLOCK_1(4).EQ.0)  ERROR = 3
      IF (BLOCK_1(3).EQ.255.AND.BLOCK_1(4).EQ.64) ERROR = 3
 
!     If a grid catalogue number is given grid should still be
!     described in section 2. Check for this and print warning.
 
      IF (BLOCK_1(3).NE.255.AND.(BLOCK_1(4).EQ.0.OR.BLOCK_1(4).EQ.64))
     &  THEN
        ERROR = 2
        IF (MSGLVL .LT. 2) WRITE (UNIT = ERR_U, FMT = '(A)' )
     &    ' WARNING - catalogued grid not described in section 2.'
      ENDIF

      IF (ERROR .EQ. 3) THEN
        CALL PRTBIN (BLOCK_1(4),8,TEMP_INT,TEMP_ERR)
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9004) TEMP_INT
 9004   FORMAT (' ERROR - invalid block indicator flag ', I8.8)
        RETURN
      ENDIF
 
!     Parameter identifier ( code table 2 ).
 
      STORE(9) = BLOCK_1(5)
      IF (BLOCK_1(5).LT.0.OR.BLOCK_1(5).GT.255) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9005) BLOCK_1(5)
 9005   FORMAT (' ERROR - invalid parameter ', I4)
        RETURN
      ENDIF
 
!     Indicator of level type ( code table 3 ).
 
      STORE(10) = BLOCK_1(6)
 
!     Code figures for level type range from 0 - 111, with 160
!     being used for depth below sea-level.
 
      IF (  (BLOCK_1(6) .LT. 0) .OR.
     &     ((BLOCK_1(6) .GT. 111) .AND. (BLOCK_1(6) .NE. 160)) ) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9006) BLOCK_1(6)
 9006   FORMAT (' ERROR - invalid level type ', I4)
        RETURN
      END IF
 
!     Description of level or layer ( code table 3 ).
!
!     For certain special levels no further description is
!     necessary, and the fields are set to 0.
 
      IF (BLOCK_1(6).LT.100.OR.BLOCK_1(6).EQ.102) THEN
        BLOCK_1(7) = 0
        BLOCK_1(8) = 0
      ENDIF

      STORE(11) = BLOCK_1(7)
      STORE(12) = BLOCK_1(8)
 
!     Certain level types require that the description occupy
!     both 8 bit fields.
 
      IF (BLOCK_1(6).EQ.100.OR.BLOCK_1(6).EQ.103.OR.BLOCK_1(6).EQ.105 
     &    .OR.BLOCK_1(6).EQ.107.OR.BLOCK_1(6).EQ.109.OR.BLOCK_1(6).EQ
     &    .111.OR.BLOCK_1(6).EQ.160) THEN
 
!       8 low order bits in STORE(12), high order bits in STORE(11).
 
        STORE(12) = STORE(11)
        STORE(11) = STORE(11) / 256
        STORE(12) = STORE(12) - STORE(11) * 256
 
!       Check maximum possible in 16 bits.
 
        IF (BLOCK_1(7).GT.65535) ERROR = 3
      ELSE
        IF (BLOCK_1(7).GT.255.OR.BLOCK_1(8).GT.255) ERROR = 3
      ENDIF

      IF (ERROR .EQ. 3) THEN
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9007) BLOCK_1(7),BLOCK_1(8)
 9007   FORMAT (' ERROR - level description ', I8, 3X, I8)
        RETURN
      ENDIF
 
!     Date and time. check valid ranges and missing data values.
 
      STORE(13) = BLOCK_1(9)
      ERR_Y = 0
      IF (BLOCK_1(9).LT.1.OR.BLOCK_1(9).GT.100) ERR_Y = 1   !4.10

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

      IF ( (ERR_Y + ERR_M + ERR_D + ERR_H + ERR_MIN) .NE. 0 ) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9008) BLOCK_1(9),
     &    BLOCK_1(10),BLOCK_1(11),BLOCK_1(12),BLOCK_1(13)
 9008   FORMAT (' ERROR - invalid date/time ', 3I2, ' / ', 2I2)
        RETURN
      ENDIF
 
!     Unit of time - ( code table 4 ).
!     Time range(s) and time range indicator ( code table 5 ).
!
!     Time unit codes are in the range 0 - 7, with 254 used for
!     seconds.
 
      STORE(18) = BLOCK_1(14)
      IF (  (BLOCK_1(14) .LT. 0) .OR.
     &     ((BLOCK_1(14) .GT. 7) .AND. (BLOCK_1(14) .NE. 254)) ) THEN
        ERROR = 3
      END IF
 
!     There can be one or two time ranges, each in 1 octet or
!     one time range in 2 octets.
 
      IF (BLOCK_1(17).EQ.0.OR.BLOCK_1(17).EQ.1
     &    .OR.BLOCK_1(17).EQ.10)   BLOCK_1(16)  = 0
 
!     Extra options - p2 used,not p1
 
      IF (BLOCK_1(17).EQ.123.OR.BLOCK_1(17).EQ.124)  BLOCK_1(15) = 0
 
!     Cross check range values and flag values.
 
      IF (BLOCK_1(15).GT.65535.OR.BLOCK_1(15).LT.0)   ERROR = 3
      IF (BLOCK_1(15).GT.255.AND.BLOCK_1(17).NE.10)   ERROR = 3
      IF (BLOCK_1(16).GT.255.OR.BLOCK_1(16).LT.0)     ERROR = 3
      IF (BLOCK_1(17).LT.0.OR.BLOCK_1(17).GT.124)     ERROR = 3

      IF (ERROR .EQ. 3) THEN
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9009) BLOCK_1(14),
     &    BLOCK_1(15),BLOCK_1(16),BLOCK_1(17)
 9009   FORMAT (' ERROR - time unit/time 1/time 2/indicator - ',
     &          I8, 2X, '/', I8, 2X, '/', I8, 2X, '/', I8)
        RETURN
      ENDIF

      STORE(19) = BLOCK_1(15)
      STORE(20) = BLOCK_1(16)
 
!     One time range occupying both octets.
 
      IF (BLOCK_1(17).EQ.10) THEN
 
!       8 low order bits in STORE(20); high order bits in STORE(19)
 
        STORE(20) = STORE(19)
        STORE(19) = STORE(19) / 256
        STORE(20) = STORE(20) - STORE(19) * 256
      ENDIF
 
!     Time range indicator
 
      STORE(21) = BLOCK_1(17)
 
!     Number averaged.
 
      STORE(22) = 0
      STORE(23) = 0
 
!     Check for number of averages if BLOCK_1(17) indicates that
!     data is averaged.
 
      IF ( ((BLOCK_1(17) .EQ. 3) .AND. (BLOCK_1(18) .EQ. 0)) .OR.
     &     ((BLOCK_1(17) .GT. 112) .AND. (BLOCK_1(17) .LT. 118) .AND.
     &      (BLOCK_1(18) .EQ. 0))
     &   ) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9013) BLOCK_1(17),
     &    BLOCK_1(18)
 9013   FORMAT (' ERROR - indicator/number averaged - ',
     &          I8, 2X, '/', I8)
        RETURN
      ENDIF
 
!     Value in two octets.
 
      STORE(22) = BLOCK_1(18)
 
!     8 low order bits in STORE(23); high order bits in STORE(22).
 
      STORE(23) = STORE(22)
      STORE(22) = STORE(22) / 256
      STORE(23) = STORE(23) - STORE(22) * 256
 
!     Number missing from averaged or accumulated data
 
      STORE(24) = 0  ! NON-STANDARD - ambiguous definition.
 
!     Pack 24 8-bit fields of block 1 in coded array.
 
      BYTE = 8
      NVAL = 24

      CALL SBYTES (MESG(WORD),STORE(1),OFF,BYTE,0,NVAL)
      CALL OFFSET (OFF,NVAL,WORD,BYTE,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN

!     ***************************************************************
!
!                 BLOCK 2 - GRID DESCRIPTION BLOCK.
!
!     ***************************************************************

!     Set array STORE to 0 . It is used to zero fill reserved octets.
 
      DO 100 INDEX1 = 1 , 28
        STORE(INDEX1) = 0
 100  CONTINUE
 
!     If grid description block is included...
 
      IF (BLOCK_1(4).EQ.128.OR.BLOCK_1(4).EQ.192) THEN
 
!       Length of grid description block.
!
!       Length = 32 octets => regular latitude/longitude
!                             regular gaussian
!                             spherical harmonics
!                             polar stereographic
!
!              = 42 octets => rotated lat/long grid
!       
!       For any data on hybrid levels the vertical coordinates
!       are added.
 
        IF (BLOCK_2(3).EQ.10) THEN
          BLK_SIZE = 42
        ELSE
          BLK_SIZE = 32  
        ENDIF

        IF (BLOCK_1(6).EQ.109.OR.BLOCK_1(6).EQ.110)
     &      BLK_SIZE = BLK_SIZE + LEN_VERT * 4

        CALL SBYTE (MESG(WORD),BLK_SIZE,OFF,24)
        CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Number of unused bits at end of block.
!       This is hard-coded to 0. Since vertical coord parameters
!       are stored in VERT_COORDS, all bits are used.
 
        CALL SBYTE (MESG(WORD),STORE(1),OFF,8)
        CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Next octet is reserved.
 
        CALL SBYTE (MESG(WORD),STORE(1),OFF,8)
        CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Data representation type.
 
        IF ( (BLOCK_2(3) .NE.  0) .AND. (BLOCK_2(3) .NE.  1) .AND.
     &       (BLOCK_2(3) .NE.  4) .AND. (BLOCK_2(3) .NE.  5) .AND.
     &       (BLOCK_2(3) .NE. 10) .AND. (BLOCK_2(3) .NE. 50)     ) THEN
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE (ERR_U,9010) BLOCK_2(3)
 9010     FORMAT (' ERROR - invalid data representation type - ',
     &            I3)
          RETURN
        ENDIF

        CALL SBYTE (MESG(WORD),BLOCK_2(3),OFF,8)
        CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Lat/long, gaussian grid or mercator.
 
        IF (BLOCK_2(3).EQ.0.OR.BLOCK_2(3).EQ.4.OR.BLOCK_2(3).EQ.1.
     &      OR.BLOCK_2(3).EQ.10) THEN
 
!         Number of lat/long points.
 
          CALL SBYTES (MESG(WORD),BLOCK_2(4),OFF,16,0,2)
          CALL OFFSET (OFF,2,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
 
!         Lat/long of origin.
!         Sign bit set to 1 if values are negative.
 
          IF (ABS(BLOCK_2(6)).GT.90000)  ERROR = 3
          IF (ABS(BLOCK_2(7)).GT.360000) ERROR = 3

          IF (ERROR .EQ. 3) THEN
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9020) BLOCK_2(6),
     &        BLOCK_2(7)
 9020       FORMAT (' ERROR - invalid lat/long value  ',
     &              I5, 2X, '/', I6)
            RETURN
          ENDIF

          DO 200 INDEX1 = 1 , 2
            LAT(INDEX1) = BLOCK_2(INDEX1+5)
            BIN_SIGN   = 0
            IF (LAT(INDEX1).LT.0) THEN
              BIN_SIGN   = 8388608
              LAT(INDEX1) = - LAT(INDEX1)
            ENDIF
            LAT(INDEX1) = LAT(INDEX1) + BIN_SIGN
  200     CONTINUE

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
!         Shift 1 bit flag field to low order bit
 
          TEMP_INT = BLOCK_2(8) / 128

          IF (TEMP_INT.LT.0.OR.TEMP_INT.GT.1) THEN
            ERROR = 3
            CALL PRTBIN (BLOCK_2(8),8,TEMP_INT,TEMP_ERR)
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9011) TEMP_INT
 9011       FORMAT (' ERROR - invalid resolution flag ', I8.8)
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
 
          IF (ABS(BLOCK_2(9)).GT.90000)   ERROR = 3
          IF (ABS(BLOCK_2(10)).GT.360000) ERROR = 3

          IF (ERROR .EQ. 3) THEN
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9030) BLOCK_2(9),
     &        BLOCK_2(10)
 9030       FORMAT (' ERROR - invalid lat/long value  ',
     &              I5, 2X, '/', I6)
            RETURN
          ENDIF
 
!         Sign bit set to 1 if values are negative.
 
          DO 300 INDEX1 = 1 , 2
            LAT(INDEX1) = BLOCK_2(INDEX1+8)
            BIN_SIGN   = 0
            IF (LAT(INDEX1).LT.0) THEN
              BIN_SIGN   = 8388608
              LAT(INDEX1) = - LAT(INDEX1)
            ENDIF
            LAT(INDEX1) = LAT(INDEX1) + BIN_SIGN
  300     CONTINUE

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
!            BINARY           DECIMAL
!
!           00000000             0
!           10000000             4
!           01000000             2
!           11000000             6
!           00100000             1
!           10100000             5
!           01100000             3
!           11100000             7
!
!         Shift 3 bit flag field to low order bits.
 
          TEMP_INT = BLOCK_2(13) / 32

          IF (TEMP_INT.LT.0.OR.TEMP_INT.GT.7) THEN
            ERROR = 3
            CALL PRTBIN (BLOCK_2(13),8,TEMP_INT,TEMP_ERR)
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9014) TEMP_INT
 9014       FORMAT (' ERROR - invalid scanning mode flags ', I8.8)
            RETURN
          ENDIF

          CALL SBYTE (MESG(WORD),BLOCK_2(13),OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
 
!         Mercator needs 3 more octets for Latin.
!
!         Check valid Latin - MORE THAN ONE POSSIBLE?
 
          IF (BLOCK_2(3).EQ.1) THEN

            IF (ABS(BLOCK_2(14)).GT.90000) THEN
              ERROR = 3
              IF (MSGLVL .LT. 3) WRITE (ERR_U,9038) BLOCK_2(14)
 9038         FORMAT (' ERROR - invalid latin value - ', I5)
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
 
!           One reserved octet.
 
            CALL SBYTE (MESG(WORD),STORE(1),OFF,8)
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN

          ENDIF

          IF (BLOCK_2(3).NE.1) THEN
 
!           4 reserved octets.
 
            CALL SBYTES (MESG(WORD),STORE(1),OFF,8,0,4)
            CALL OFFSET (OFF,4,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN

          ENDIF

!         Rotated lat/long grid.

          IF (BLOCK_2(3).EQ.10) THEN

!           Latitude/longitude of the southern pole.

            IF (ABS(BLOCK_2(14)).GT.90000)   ERROR = 3
            IF (ABS(BLOCK_2(15)).GT.360000)  ERROR = 3

            IF (ERROR .EQ. 3) THEN
              IF (MSGLVL .GT. 3) WRITE (ERR_U,9030) BLOCK_2(14),
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

!           Angle of rotation.
!           Real number, so convert into GRIB format.

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
 
!         18 reserved octets.
 
          CALL SBYTES (MESG(WORD),STORE(1),OFF,8,0,18)
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
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9020) BLOCK_2(6),
     &        BLOCK_2(7)
            RETURN
          ENDIF

          DO 350 INDEX1 = 1 , 2
            LAT(INDEX1) = BLOCK_2(INDEX1+5)
            BIN_SIGN   = 0
            IF (LAT(INDEX1).LT.0) THEN
              BIN_SIGN   = 8388608
              LAT(INDEX1) = - LAT(INDEX1)
            ENDIF
            LAT(INDEX1) = LAT(INDEX1) + BIN_SIGN
  350     CONTINUE

          CALL SBYTES (MESG(WORD),LAT(1),OFF,24,0,2)
          CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
 
!         Resolution flag
!
!         INCREMENTS       BINARY CODE     DECIMAL VALUE
!
!          NOT GIVEN        00000000              0
!           GIVEN           10000000            128
!
!         Shift 1 bit flag field to low order bit
 
          TEMP_INT = BLOCK_2(8) / 128

          IF (TEMP_INT.LT.0.OR.TEMP_INT.GT.1) THEN
            ERROR = 3
            CALL PRTBIN (BLOCK_2(8),8,TEMP_INT,TEMP_ERR)
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9011) TEMP_INT
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
 
!         Longitude parallel to Y-axis.
 
          IF (ABS(BLOCK_2(9)).GT.360000) THEN
            ERROR = 3
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9037) BLOCK_2(9)
 9037       FORMAT (' Invalid LoV value - ', I6)
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
 
!         Projection centre flag (0 if North Pole on plane
!                                 1 if South Pole on plane.)
 
          IF (BLOCK_2(12).NE.0.AND.BLOCK_2(12).NE.1) THEN
            ERROR = 3
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9035) BLOCK_2(12)
 9035       FORMAT (' ERROR - invalid projection flag value - ', I6)
            RETURN
          ENDIF

          CALL SBYTE (MESG(WORD),BLOCK_2(12),OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
 
!         Scanning mode flags.
!
!         VALID VALUES       VALUE / 32
!            BINARY           DECIMAL
!
!           00000000             0
!           10000000             4
!           01000000             2
!           11000000             6
!           00100000             1
!           10100000             5
!           01100000             3
!           11100000             7
!
!         Shift 3 bit flag field to low order bits.
 
          TEMP_INT = BLOCK_2(13) / 32

          IF (TEMP_INT.LT.0.OR.TEMP_INT.GT.7) THEN
            ERROR = 3
            CALL PRTBIN (BLOCK_2(13),8,TEMP_INT,TEMP_ERR)
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9014) TEMP_INT
            RETURN
          ENDIF

          CALL SBYTE (MESG(WORD),BLOCK_2(13),OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
 
!         4 reserved octets.
 
          CALL SBYTES (MESG(WORD),STORE(1),OFF,8,0,4)
          CALL OFFSET (OFF,4,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

        ENDIF
 
!       Add vertical coordinate parameters for hybrid levels.
 
        IF (BLOCK_1(6).EQ.109.OR.BLOCK_1(6).EQ.110) THEN

          DO 400 INDEX1 = 1 , LEN_VERT
            CALL CONFP (VERT_COORDS(INDEX1),EXPO,MANTI)
            CALL SBYTE (MESG(WORD),EXPO,OFF,8)
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
            CALL SBYTE (MESG(WORD),MANTI,OFF,24)
            CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
  400     CONTINUE

        ENDIF

      ENDIF

!     ***************************************************************
!
!                   BLOCK 3 (IF REQUIRED) - BIT MAP BLOCK.
!
!     NON-STANDARD GRIB LEFT     - does not allow centre-defined map
!
!     ***************************************************************

      IF (BLOCK_1(4).EQ.64.OR.BLOCK_1(4).EQ.192) THEN
 
!       Check that bit-map length = grid-size.
 
        IF (LEN_BITMAP.NE.(BLOCK_2(4)*BLOCK_2(5))) THEN
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE(ERR_U,9012) LEN_BITMAP,
     &      (BLOCK_2(4)*BLOCK_2(5))
 9012     FORMAT(' ERROR - bitmap length ', I10,
     &           ' does not equal grid-size ', I10)
          RETURN
        ENDIF
 
!       Set up length of block 3.
 
        NUB  = 0
        NGRD = BLOCK_2(4)*BLOCK_2(5)
        NOCT = NGRD/8
        IBOF = NGRD-NOCT*8
 
!       Round to whole number of octets and calculate no. of bits
!       unused
 
        IF (IBOF.NE.0) THEN
          NUB=8-IBOF
          NOCT=NOCT+1
        ENDIF
 
!       Round to even number of octets,correcting no. bits unused
 
        IF (MOD(NOCT,2).NE.0) THEN
          NUB=NUB+8
          NOCT=NOCT+1
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
!       Does not allow centre-defined bitmap
 
        CALL SBYTE(MESG(WORD),0,OFF,16)
        CALL OFFSET(OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
        IF(ERROR .EQ. 3) RETURN
 
!       Set up bitmap.
 
        DO 310 INDEX2 = 1 , LEN_BITMAP
          CALL SBYTE(MESG(WORD),BITMAP(INDEX2),OFF,1)
          CALL OFFSET(OFF,1,WORD,1,WORD_SIZE,LEN_MESG,ERROR)
          IF(ERROR .EQ. 3) RETURN
 310    CONTINUE
 
!       Set up unused bits.
 
        CALL SBYTES(MESG(WORD),STORE(1),OFF,1,0,NUB)
        CALL OFFSET(OFF,NUB,WORD,1,WORD_SIZE,LEN_MESG,ERROR)
        IF(ERROR .EQ. 3) RETURN
      ENDIF

!     ***************************************************************
!
!                      BLOCK 4 - BINARY DATA BLOCK.
!
!     ***************************************************************

!     Retain pointers to word and bit position of binary data
!     block length field. Enter length as 0.
 
      TEMP_WORD = WORD
      TEMP_OFF = OFF

      CALL SBYTE (MESG(WORD),STORE(1),OFF,24)
      CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
 
!     4 bit flag / 4 bit count of unused bits at end of block octet.
!
!     Flag is 1000 for spherical harmonics, 0000 for lat/long
!     or gaussian grid.
 
      REP = 0
      IF (BLOCK_2(3).EQ.50) REP = 1
 
!     Flag field is irep,shifted left by 7 bits
 
      FLAG = REP * 128

      CALL SBYTE (MESG(WORD),FLAG,OFF,8)
      CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
 
!     Find maximum and minimum values in data array. For
!     data in spherical harmonic form the first word contains
!     the real (0,0) coefficient, which is treated separately.
 
      LEN_DATA = LENGTH - REP
      max_data = Realarr_Max(fp_data(rep + 1), len_data)
      min_data = Realarr_Min(fp_data(rep + 1), len_data)
 
!     Scale factor. Either width passed in or binary accuracy.
!     If binary accuracy, scale factor is simply the accuracy, and
!     the data are multiplied by 1/2**acc. If not, calculate the
!     scale factor. The calculation below assumes that the maximum
!     packed integer will fill WIDTH bits.

      IF (WIDTH.NE.0) THEN

        TEMP_SCALE = (MAX_DATA-MIN_DATA)/(2**(WIDTH+1)-1)
        IF (TEMP_SCALE.NE.0.) TEMP_SCALE = ALOG(TEMP_SCALE)/ALOG(2.)
     &                                     + 2.
        TEMP_SCALE2 = MIN(INT(TEMP_SCALE),
     &                    INT(TEMP_SCALE+SIGN(1.,TEMP_SCALE)))
        REAL_SCALE = 2.**TEMP_SCALE2
        SCALE = TEMP_SCALE2
      ELSE
        SCALE = BLOCK_0(4)
        REAL_SCALE = 2.**BLOCK_0(4)
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
!     Convert minimum value (fmin) to grib format (iexp,imant)
 
      CALL CONFP (MIN_DATA,EXPO,MANTI)

      CALL SBYTE (MESG(WORD),EXPO,OFF,8)
      CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
      CALL SBYTE (MESG(WORD),MANTI,OFF,24)
      CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
 
!     Number of bits in each data value. If WIDTH is passed, simply
!     pack this. If not, need to calculate it from the maximum width
!     needed.

      IF (WIDTH.EQ.0) THEN
        TEMP_INT = INT((MAX_DATA - MIN_DATA) / REAL_SCALE)
        width = Ceil_Log2(temp_int)
      ENDIF

      CALL SBYTE (MESG(WORD),WIDTH,OFF,8)
      CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
 
!     If spherical harmonics data, next 4 octets are different format.
 
      IF (REP.EQ.1) THEN
 
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
 
!     Scale and store data values.
 
      DO 600 INDEX1 = REP+1 , LENGTH
        JGRIB = NINT ( (FP_DATA(INDEX1)-MIN_DATA) / REAL_SCALE )
        FP_DATA(INDEX1) = ZGRIB
  600 CONTINUE

# if defined (HP) || defined (CRAY) || defined (SX6)   /*6.2*/
      DO I = 1,LEN_DATA                                            !6.0
        IN_DATA(I) = TRANSFER(FP_DATA(REP+I),0) 	           !6.0
      ENDDO                                                        !6.0
      CALL SBYTES (MESG(WORD),IN_DATA,OFF,WIDTH,0,LEN_DATA)        !6.0
      DO I = 1,LEN_DATA                                            !6.0
        FP_DATA(REP+I) = TRANSFER (IN_DATA(I),0.0)                 !6.0
      ENDDO                                                        !6.0
# else
      CALL SBYTES (MESG(WORD),FP_DATA(REP+1),OFF,WIDTH,0,LEN_DATA)
# endif

      CALL OFFSET (OFF,LEN_DATA,WORD,WIDTH,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
 
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
        CALL SBYTE (MESG(WORD),STORE(1),OFF,UNUSED)
        CALL OFFSET (OFF,1,WORD,UNUSED,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
      ENDIF

      BITS_BK4 = (WORD-TEMP_WORD) * WORD_SIZE + OFF - TEMP_OFF
 
!     Enter length - in octets - of data block.
 
      BITS_BK4 = BITS_BK4 / 8
      CALL SBYTE (MESG(TEMP_WORD),BITS_BK4,TEMP_OFF,24)
      CALL OFFSET (TEMP_OFF,1,TEMP_WORD,24,WORD_SIZE,LEN_MESG,ERROR)
 
!     Enter number of unused bits in flag/bit count field.
 
      FLAG = FLAG + UNUSED
      CALL SBYTE (MESG(TEMP_WORD),FLAG,TEMP_OFF,8)

!********************************************************************
!
!                      BLOCK 5 - END BLOCK.
!
!********************************************************************

!     Add 7 7 7 7 to coded data.
 
      STORE(1) = 55
      STORE(2) = 55
      STORE(3) = 55
      STORE(4) = 55

      CALL SBYTES (MESG(WORD),STORE(1),OFF,8,0,4)
      CALL OFFSET (OFF,4,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
 
!     Set any unused part of last word to binary zeroes.
 
      IF (OFF.NE.0) THEN
        TEMP_INT = WORD_SIZE - OFF
        CALL SBYTE (MESG(WORD),STORE(5),OFF,TEMP_INT)
      ELSE
        WORD = WORD - 1
      ENDIF

      RETURN
      END ! SUBROUTINE Coder

      
