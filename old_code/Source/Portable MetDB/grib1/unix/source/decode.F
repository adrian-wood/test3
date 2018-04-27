!+    Decode a GRIB message
! $Header: decode.F, 1, 27/02/2007 16:04:58, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Decode (fp_data, fp_work, len_fp, num_fp,
     &  vert_coords, len_vert, num_vert, bitmap, len_bitmap,
     &  num_bitmap, quasi, len_q, num_q, width, word_size,
     &  block_0, block_1, block_2, block_3, block_4, block_r, mesg,
     &  len_mesg, posn, word, off, error, work_int1, work_int2,
     &  work_re1, err_unit, msglvl)

      IMPLICIT NONE

!     Description:
!       Decodes the GRIB message in mesg, returning the data field
!       in fp_data and various ancillary information in other output
!       arguments.
!
!       WARNING - Maximum number of bits per data value is 32. This is
!       non-standard grib.
!
!     Method:
!       <Say how it does it: refer to external documentation>
!       <If this routine is very complex, then include a
!        "pseudo code" description of it to make its structure
!        and method clear>
!
!     Current Code Owner: MetDB team
!
!     History:
!       This file is under RCS control. See the RCS log for change
!       history information.
!
!     Code Description:
!       FORTRAN 77 with extensions recommended in the Met. Office
!       F77 Standard.
!
!     Scalar arguments with INTENT(IN):
! $Log:
!  1    Met_DB_Project 1.0         27/02/2007 16:04:58    Stan Kellett    
! $
! Revision 6.1  2004/05/18 11:27:27  usmdb
! Add SX6 directives.
!
! Revision 6.0  2001/08/09 10:58:39  usmdb
! replaces decode.f, equivalence replaced by nag f90 routine as will not compile
! using this compiler. As routine will not work on MVS then pre-proc statements
! are used.   Stan Kellett.
!
! 
! $Revision: 1$
! $Date: 27/02/2007 16:04:58$

      INTEGER    len_fp            ! Length of array fp_data
      INTEGER    len_vert          ! Length of array vert_coords
      INTEGER    len_bitmap        ! Length of array bitmap
      INTEGER    len_q             ! Length of array quasi
      INTEGER    word_size         ! Number of bits in an INTEGER
      INTEGER    len_mesg          ! Length of array mesg
      INTEGER    err_unit          ! Unit number requested for error
                                   ! messages
      INTEGER    msglvl            ! Level of diagnostics required

!     Array arguments with INTENT(IN):

      INTEGER    mesg(len_mesg)    ! GRIB message in encoded form
      INTEGER    posn(4)           ! Lat/long box for which data
                                   ! should be extracted

!     Scalar arguments with INTENT(INOUT):

      INTEGER    word              ! Word offset in message
      INTEGER    off               ! Bit offset in message

!     Array arguments with INTENT(INOUT):

      INTEGER    block_4(2)        ! Parameters for data section

!     Scalar arguments with INTENT(OUT):

      INTEGER    num_fp            ! Number of elements of fp_data
                                   ! occupied by field
      INTEGER    num_vert          ! Number of elements of vert_coords
                                   ! occupied by vertical coord params
      INTEGER    num_bitmap        ! Number of elements of bitmap
                                   ! occupied by the bit map
      INTEGER    num_q             ! Number of elements of quasi used
      INTEGER    width             ! Number of bits used to encode
                                   ! each data value in the message

!     Array arguments with INTENT(OUT):

      REAL       fp_data(len_fp)        ! Decoded field of data
      REAL       fp_work(len_fp)        ! Workspace
      REAL       vert_coords(len_vert)  ! Vertical coordinate
                                        ! parameters
      INTEGER    bitmap(len_bitmap)     ! Bit map
      INTEGER    quasi(len_q)           ! Number of points along each
                                        ! parallel or meridian, for a
                                        ! quasi-regular grid
      INTEGER    block_0(4)             ! Parameters from indicator
                                        ! section
      INTEGER    block_1(*)             ! Parameters from product
                                        ! definition section
      INTEGER    block_2(20)            ! Parameters from grid
                                        ! description section
      INTEGER    block_3(2)             ! Parameters from bitmap
                                        ! section
      REAL       block_r(20)            ! Misc. real parameters
      INTEGER    work_int1(*)           ! Workspace
      INTEGER    work_int2(*)
      REAL       work_re1(*)

!     Error status:

      INTEGER    error             ! +ve => warning or fatal error

!     Local Parameters:

      INTEGER    Max_Bits
      PARAMETER (Max_Bits = 30)

!     Local Scalars:

      INTEGER    jgrib         ! integer used in equivalence (bleah)
      INTEGER    byte          ! size of chunk to extract
      INTEGER    nval          ! number of chunks to extract
      INTEGER    edtn0         ! indicator of level 0 or -1
      INTEGER    index1        ! DO loop index
      INTEGER    err_u         ! Unit number actually used for error
                               ! messages
      INTEGER    bk2_inc       ! Block 2 included flag
      INTEGER    len_bk2       ! Length of block 2
      INTEGER    bk3_inc       ! Block 3 included flag
      INTEGER    len_bk3       ! Length of block 3
      INTEGER    unused        ! No. of bits unused at end of block 4
      INTEGER    expo          ! Exponent of ref. value
      INTEGER    manti         ! Mantissa of ref. value
      INTEGER    nub           ! Number of unused bits
      INTEGER    bmf           ! Bit-map follows indicator
      INTEGER    octs_bk4      ! Octets in block 4
      INTEGER    flag          ! Flag from block 4
      INTEGER    rep           ! Grid point/sph harms. indicator
      INTEGER    scale         ! Scale factor
      INTEGER    max_int       ! Maximum integer in width bits + 1
      INTEGER    data_pt       ! Pointer to postion in data array
      INTEGER    zero_bits     ! Empty bits at end of block 4

!     For limited area extract.

      INTEGER    limext        ! 1 if limited extract, else 0
      INTEGER    lim_cols      ! number of columns in box
      INTEGER    lim_rows      ! number of rows in box
      INTEGER    lim_word      ! store for word in between calls
      INTEGER    lim_off       ! store for off between calls
      INTEGER    gpoff         ! grid point number of the first box
                               ! point
      INTEGER    row_count     ! counter used when scanning bitmap
      INTEGER    box_row       ! ditto
      INTEGER    col_count     ! ditto!
      INTEGER    col_in        ! indicator - 1 if column in limited
                               ! area
      INTEGER    in            ! indicator - 1 if inside limited area
      INTEGER    boxed         ! indicator - 1 if moved into lim. area
      INTEGER    len_dataptn   ! number of values in this array
      INTEGER    count         ! counter
      INTEGER    skip          ! number of data values to leave packed
      INTEGER    unpk          ! number to unpack
      INTEGER    row_len       ! length of a "row"

      INTEGER    temp_int      ! Store for temporary results
      INTEGER    temp_word     ! Word number of start of block 4
      INTEGER    temp_off      ! Offset at start of block 4
      INTEGER    temp_scale2   ! Temporary scale factor

      REAL       zgrib         ! Real used in grotty equivalence
      REAL       data_min      ! Minimum value in data
      REAL       real_scale    ! Scale factor
      REAL       max_real      ! Missing data indicator (scaled using
                               ! max_int)

# if defined (HP) || defined (CRAY) || defined (SX6)    /*6.1*/
      INTEGER    IN_DATA (LEN_FP)!6.0 used to reppresent REAL as a Integer
      INTEGER    I               !6.0 loop control
# endif

!     Local Arrays:

      INTEGER    store(28)     ! Temporary array
      INTEGER    lat(2)        ! Store for lat/long values
      INTEGER    scan_posn(4)  ! positions allowing for scanning modes
      INTEGER    data_ptn(50000) ! array to store analysis of bitmap

!     Functions and subroutines used:

      EXTERNAL   Grib_Edtn_1_Dec
      EXTERNAL   GBytes
      EXTERNAL   GByte
      EXTERNAL   Offset
      EXTERNAL   DecFP


!-    End of header

      EQUIVALENCE (ZGRIB,JGRIB)       ! Bleah!
    
!
! Revision Code:
      LOGICAL HEADSET
      CHARACTER*300 HEAD
      DATA HEADSET /.FALSE./
      
      
      IF (.NOT.HEADSET) THEN
        HEAD='
     &$Source: /home/us0400/grib/source/RCS/decode.F,v $
     &'//'$Date: 27/02/2007 16:04:58$ $Revision: 1$'
        HEADSET = .TRUE.
      ENDIF                
! End of Rev
!
!
!     Reset the error indicator.

      ERROR = 0

!     Ensure that array lengths returned are zero in cases where
!     the arrays are not used.

      num_vert = 0
      num_q = 0
      num_bitmap = 0

!     Check for a sensible unit for error messages.

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

!     ***************************************************************
!
!                      BLOCK 0 - INDICATOR BLOCK.
!
!     ***************************************************************

!     Save WORD and OFF first.

      LIM_WORD = WORD
      LIM_OFF  = OFF

      WORD = 1
      OFF  = 0
      BYTE = 8
      NVAL  = 8

!     Extract 8 octets - ascii G R I B and edition dependent info.

      CALL GBYTES (MESG(1),STORE(1),0,BYTE,0,NVAL)
      CALL OFFSET (OFF,NVAL,WORD,BYTE,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
 
!     Test for ASCII G R I B
 
      IF (STORE(1).NE.71.OR.STORE(2).NE.82.OR.STORE(3).NE.73.
     &    OR.STORE(4).NE.66) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE( UNIT = ERR_U, FMT = '(A)' )
     &    ' ERROR - header does not contain ascii G R I B.'
        RETURN
      ENDIF

!     ***************************************************************
!
!                  BLOCK 1 - PRODUCT DEFINITION BLOCK.
!
!     ***************************************************************

!     Next 4 octets are either message length and edition number 1 or
!                              section length and edition number.
 
      BLOCK_0(1) = STORE(8)

      IF (BLOCK_0(1).EQ.1) THEN

!       Check for limited extract.

        IF (POSN(1).NE.0) THEN
          WORD = LIM_WORD
          OFF  = LIM_OFF
        ENDIF

        CALL Grib_Edtn_1_Dec (fp_data, fp_work, len_fp, num_fp,
     &    vert_coords, len_vert, num_vert, bitmap, len_bitmap,
     &    num_bitmap, quasi, len_q, num_q, width, word_size,
     &    block_0, block_1, block_2, block_3, block_4, block_r, mesg,
     &    len_mesg, posn, word, off, error, work_int1, work_int2,
     &    work_re1, err_u, msglvl)
        RETURN
      ENDIF
 
!     If o5-7 do not contain 0, 0 and 24 respectively
!     then the data is in the old version of the code.
 
      IF (STORE(5).EQ.0.AND.STORE(6).EQ.0.AND.STORE(7).EQ.24) THEN
        BLOCK_0(1) = 0
        EDTN0 = 1
        NVAL = 20       ! NON-STANDARD - block length variable.
      ELSE
        EDTN0 = 0
        BLOCK_0(1) = -1
        NVAL = 16
      ENDIF

!     If POSN(1) isn't 0, then we're doing a limited extract.

      IF (POSN(1).NE.0) THEN        
        LIMEXT   = 1

        LIM_COLS = ABS(POSN(3) - POSN(1)) + 1
        LIM_ROWS = ABS(POSN(4) - POSN(2)) + 1

        SCAN_POSN(1) = MIN(POSN(1),POSN(3))
        SCAN_POSN(2) = MIN(POSN(2),POSN(4))
        SCAN_POSN(3) = MAX(POSN(1),POSN(3))
        SCAN_POSN(4) = MAX(POSN(2),POSN(4))

      ELSE
        LIMEXT   = 0
      ENDIF

      IF (LIMEXT.EQ.0) THEN                ! start of no limext block.

!       Extract next 16 or 20 octets of block 1 - number depends
!       on version of code.
 
        CALL GBYTES (MESG(WORD),STORE(5),OFF,BYTE,0,NVAL)
        CALL OFFSET (OFF,NVAL,WORD,BYTE,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Transfer product definition information to output array,
!       except for number averaged.
 
        DO INDEX1 = 1 , 17
          BLOCK_1(INDEX1) = STORE(INDEX1+4*EDTN0)
        END DO  
 
!       Check for ECMWF as originating centre.
 
        IF (BLOCK_1(1).EQ.98) THEN
          ERROR = 2
          IF (MSGLVL .LT. 2) WRITE( UNIT = ERR_U, FMT = '(A)' )
     &      ' WARNING - data may have been coded using ECMWF ' //
     &      'non-standard table 2.'
        ENDIF
 
!       Number averaged occupies 2 octets.
 
        BLOCK_1(18) = STORE(18+4*EDTN0) * 256 + STORE(19+4*EDTN0)
 
!       Description of level or layer ( code table 3 ).
!
!       Certain level types require that the description occupy
!       both 8 bit fields. Put single value in first level word.
 
        TEMP_INT = 32 + (EDTN0*224)

        IF (BLOCK_1(6).EQ.100.OR.BLOCK_1(6).EQ.103.OR.
     &      BLOCK_1(6).EQ.105.OR.BLOCK_1(6).EQ.107.OR.
     &      BLOCK_1(6).EQ.109.OR.BLOCK_1(6).EQ.111.OR.
     &      BLOCK_1(6).EQ.160) THEN
          BLOCK_1(7) = BLOCK_1(7) * TEMP_INT + BLOCK_1(8)
          BLOCK_1(8) = 0
        ENDIF
 
!       Time range.
!       One time range can occupy two octets.
 
        IF (BLOCK_1(17).EQ.10) THEN
          BLOCK_1(15) = BLOCK_1(15) * TEMP_INT + BLOCK_1(16)
          BLOCK_1(16) = 0
        ENDIF

!       *************************************************************
!
!                  BLOCK 2 - GRID DESCRIPTION BLOCK.
!
!       *************************************************************
 
!       Check flag indicating if block 2 is included in code.
!
!       BK2_INC is set to 1 if block 2 is included.
 
        BK2_INC = 0
 
        IF (EDTN0.EQ.0) THEN
 
!         OLD VERSION OF CODE
!
!         BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!              none             00000000               0
!               2               00000001               1
!               3               00000010               2
!           2 and 3             00000011               3
 
          IF (BLOCK_1(4).EQ.1.OR.BLOCK_1(4).EQ.3) BK2_INC = 1
        ELSE
 
!         NEW VERSION
!
!         BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!              none             00000000               0
!               2               10000000             128
!               3               01000000              64
!           2 and 3             11000000             192
 
          IF (BLOCK_1(4).EQ.128.OR.BLOCK_1(4).EQ.192) BK2_INC = 1
        ENDIF
 
        IF (BK2_INC.EQ.1) THEN                ! start of block 2.
    
!         Length of grid description block.
    
          CALL GBYTE (MESG(WORD),LEN_BK2,OFF,24)
          CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

!         Number of unused bits at end of block.
    
          CALL GBYTE (MESG(WORD),UNUSED,OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
    
!         Next octet is reserved.
    
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
    
!         Data representation type.
    
          CALL GBYTE (MESG(WORD),BLOCK_2(3),OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
 
!         Lat/long grid, gaussian grid, spherical harmonics,
!         polar stereographic, Lambert-conformal and mercator are the only data
!         representations handled.                          !5.8
 
          IF (BLOCK_2(3).NE.0.AND.BLOCK_2(3).NE.4.AND.BLOCK_2(3).NE.50
     &        .AND.BLOCK_2(3).NE.1.AND.BLOCK_2(3).NE.5.AND.BLOCK_2(3).
     &        NE.10.AND.BLOCK_2(3).NE.3) THEN                           !5.8
            ERROR = 3
            IF (MSGLVL .LT. 3) WRITE (UNIT = ERR_U, FMT = '(A)' )
     &        ' ERROR - grid description block not yet defined.'
            RETURN
          ENDIF
 
!         Length = 32 octets => regular lat/long
!                               regular gaussian
!                               mercator 
!                               polar stereographic
!                               spherical harmonics
!
!                = 42 octets => rotated lat/long
!
!         The vertical coords are added.
 
!         Get number of vertical coordinate parameters, if any.

          IF (BLOCK_2(3).EQ.10) THEN 
            NUM_VERT = (LEN_BK2 - 42) / 4
          ELSE
            NUM_VERT = (LEN_BK2 - 32) / 4
          ENDIF 

!         Uncomment for debug.
!         WRITE (6,*) ' NUM_VERT = ',NUM_VERT

          IF (LEN_VERT.LT.NUM_VERT) THEN
            ERROR = 3
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9201) NUM_VERT,LEN_VERT
 9201       FORMAT (' ERROR - number of vertical coordinates - ',
     &              I4, ', array size is - ', I4)
            RETURN
          ENDIF
    
!         Lat/long, gaussian grid or mercator.
    
          IF (BLOCK_2(3).EQ.0.OR.BLOCK_2(3).EQ.4.OR.BLOCK_2(3).EQ.1.
     &        OR.BLOCK_2(3).EQ.10) THEN      ! lat,gauss or merc.
 
!           Number of lat/long points.
 
            CALL GBYTES (MESG(WORD),BLOCK_2(4),OFF,16,0,2)
            CALL OFFSET (OFF,2,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           Lat/long of origin.
 
            CALL GBYTES (MESG(WORD),LAT(1),OFF,24,0,2)
            CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           If sign bit set to 1 , values are negative.
 
            DO INDEX1 = 1 , 2
              BLOCK_2(INDEX1+5) = LAT(INDEX1)
              IF (LAT(INDEX1).GT.8388608) THEN
                BLOCK_2(INDEX1+5) = BLOCK_2(INDEX1+5) - 8388608
                BLOCK_2(INDEX1+5) = - BLOCK_2(INDEX1+5)
              ENDIF
            END DO  
 
!           Resolution flag.
 
            CALL GBYTE (MESG(WORD),BLOCK_2(8),OFF,8)
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           Lat/long of extreme points.
 
            CALL GBYTES (MESG(WORD),LAT(1),OFF,24,0,2)
            CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           If sign bit set to 1 , values are negative.
 
            DO INDEX1 = 1 , 2
              BLOCK_2(INDEX1+8) = LAT(INDEX1)
              IF (LAT(INDEX1).GT.8388608) THEN
                BLOCK_2(INDEX1+8) = BLOCK_2(INDEX1+8) - 8388608
                BLOCK_2(INDEX1+8) = - BLOCK_2(INDEX1+8)
              ENDIF
            END DO  
 
!           Direction increments / number of latitude lines.
 
            CALL GBYTES (MESG(WORD),BLOCK_2(11),OFF,16,0,2)
            CALL OFFSET (OFF,2,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           If no direction increments,coded with all bits set to 1.
!           Convert to 0 in this case.
 
            IF (BLOCK_2(8).EQ.0) THEN
              BLOCK_2(11) = 0
              BLOCK_2(12) = 0
            ENDIF
 
!           Scanning mode flags.
 
            CALL GBYTE (MESG(WORD),BLOCK_2(13),OFF,8)
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           Mercator - next 3 octets contain Latin.
 
            IF (BLOCK_2(3).EQ.1) THEN
              CALL GBYTE (MESG(WORD),BLOCK_2(14),OFF,24)
              CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN
 
!             1 reserved octet.
 
              CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            ENDIF
 
            IF (BLOCK_2(3).NE.1) THEN
 
!             4 reserved octets.
 
              CALL OFFSET (OFF,4,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN
            ENDIF

!           Rotated lat/long grid.

            IF (BLOCK_2(3).EQ.10) THEN

!             Lat/long of southern pole.
          
              CALL GBYTES (MESG(WORD),LAT(1),OFF,24,0,2)
              CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN
 
!             If sign bit set to 1 , values are negative.
 
              DO INDEX1 = 1 , 2
                BLOCK_2(INDEX1+13) = LAT(INDEX1)
                IF (LAT(INDEX1).GT.8388608) THEN
                  BLOCK_2(INDEX1+13) = BLOCK_2(INDEX1+13) - 8388608
                  BLOCK_2(INDEX1+13) = - BLOCK_2(INDEX1+13)
                ENDIF
              END DO 

!             Angle of rotation.
!             Real number, so convert from GRIB format.

              CALL GBYTE (MESG(WORD),EXPO,OFF,8)
              CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN
              CALL GBYTE (MESG(WORD),MANTI,OFF,24)
              CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN
      
              CALL DECFP (BLOCK_R(1),EXPO,MANTI)

            ENDIF

           ENDIF                     ! end of lat, gauss, or merc.


! Lambert Conformal Grid                                         !5.8
          
           IF (BLOCK_2(3).EQ.3) THEN                             !5.8

! Number of lat/long points.                                     !5.8

            CALL GBYTES (MESG(WORD),BLOCK_2(4),OFF,16,0,2)       !5.8

! Shift Word from 11-12, OFF from 16-16                          !5.8

            CALL OFFSET (OFF,2,WORD,16,WORD_SIZE,LEN_MESG,ERROR) !5.8 
            IF (ERROR.EQ.3) RETURN                               !5.8
  
! Lat/long of origin.                                            !5.8
 
            CALL GBYTES (MESG(WORD),LAT(1),OFF,24,0,2)           !5.8

! Shift WORD from 12-14, OFF from 16-0                           !5.8

            CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR) !5.8 
            IF (ERROR.EQ.3) RETURN                               !5.8
 
! If sign bit set to 1 , values are negative.                    !5.8
  
            DO INDEX1 = 1 , 2                                    !5.8
              BLOCK_2(INDEX1+5) = LAT(INDEX1)                    !5.8
              IF (LAT(INDEX1).GT.8388608) THEN                   !5.8
                BLOCK_2(INDEX1+5) = BLOCK_2(INDEX1+5) - 8388608  !5.8
                BLOCK_2(INDEX1+5) = - BLOCK_2(INDEX1+5)          !5.8
              ENDIF                                              !5.8
            END DO                                               !5.8 
  
! Resolution flag.                                               !5.8

            CALL GBYTE (MESG(WORD),BLOCK_2(8),OFF,8)             !5.8

! Shift WORD from 14-14, OFF from 0-8                            !5.8

            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)  !5.8
            IF (ERROR.EQ.3) RETURN                               !5.8
  
!           Lov (d)                                              !5.8

            CALL GBYTES (MESG(WORD),BLOCK_2(9),OFF,24,0,1)       !5.8 

! Shift WORD from 14-15, OFF from 8,24                           !5.8

            CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR) !5.8 

            IF (ERROR.EQ.3) RETURN                               !5.8

! If sign bit set to 1 , values are negative.                    !5.8

            IF (BLOCK_2(9).GT.8388608) THEN                      !5.8
              BLOCK_2(9) = BLOCK_2(9) - 8388608                  !5.8
              BLOCK_2(9) = - BLOCK_2(9)                          !5.8
            ENDIF                                                !5.8
   
! Direction increments / number of latitude lines. (e)           !5.8
! Shift WORD from  15-16                                         !5.8

            CALL GBYTES (MESG(WORD),BLOCK_2(10),OFF,24,0,2)      !5.8
            CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR) !5.8
            IF (ERROR.EQ.3) RETURN                               !5.8
  
!           If no direction increments,coded with all bits set to 1. !5.8
!           Convert to 0 in this case.                           !5.8
  
            IF (BLOCK_2(8).EQ.0) THEN                            !5.8
              BLOCK_2(10) = 0                                    !5.8
              BLOCK_2(11) = 0                                    !5.8
            ENDIF                                                !5.8
  
! Projection Centre. (f)                                         !5.8
! Shift WORD from 16-17                                          !5.8

            CALL GBYTE (MESG(WORD),BLOCK_2(12),OFF,8)            !5.8
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)  !5.8
            IF (ERROR.EQ.3) RETURN                               !5.8
 
! Scanning mode flags. (g)                                       !5.8
! Shift WORD from 16-17                                          !5.8

            CALL GBYTE (MESG(WORD),BLOCK_2(13),OFF,8)            !5.8
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)  !5.8
            IF (ERROR.EQ.3) RETURN                               !5.8
 
! Latin1 and Latin2.  (h)                                        !5.8
  
!            IF (BLOCK_2(3).EQ.1) THEN                           !5.8
              CALL GBYTES (MESG(WORD),BLOCK_2(14),OFF,24,0,2)    !5.8
              CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR) !5.8
              IF (ERROR.EQ.3) RETURN                             !5.8
 
! Lat/long of southern pole. (i)                                 !5.8
            
              CALL GBYTES (MESG(WORD),LAT(1),OFF,24,0,2)         !5.8
              CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR) !5.8
              IF (ERROR.EQ.3) RETURN                             !5.8
 
! If sign bit set to 1 , values are negative.                    !5.8
  
              DO INDEX1 = 1 , 2                                  !5.8
                BLOCK_2(INDEX1+15) = LAT(INDEX1)                 !5.8
                IF (LAT(INDEX1).GT.8388608) THEN                 !5.8
                  BLOCK_2(INDEX1+15) = BLOCK_2(INDEX1+15) - 8388608 !5.8
                  BLOCK_2(INDEX1+15) = - BLOCK_2(INDEX1+15)      !5.8
                ENDIF                                            !5.8
              END DO                                             !5.8

! Skip last two octets                                            !5.8
              CALL OFFSET (OFF,2,WORD,8,WORD_SIZE,LEN_MESG,ERROR) !5.8
          ENDIF                                                   !5.8

    
!         Spherical harmonic data.
    
          IF (BLOCK_2(3).EQ.50) THEN     ! spherical harmonics.
 
!           Pentagonal resolution parameters.
 
            CALL GBYTES (MESG(WORD),BLOCK_2(4),OFF,16,0,3)
            CALL OFFSET (OFF,3,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           Representation type and mode.
 
            CALL GBYTES (MESG(WORD),BLOCK_2(7),OFF,8,0,2)
            CALL OFFSET (OFF,2,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           18 reserved octets.
 
            CALL OFFSET (OFF,18,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
        
          ENDIF                         ! end of sph. harms.
    
!         Polar Stereographic.
    
          IF (BLOCK_2(3).EQ.5) THEN     ! polar stereo.
 
!           Number of points along X and Y axes.
 
            CALL GBYTES (MESG(WORD),BLOCK_2(4),OFF,16,0,2)
            CALL OFFSET (OFF,2,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           Lat/long of origin.
 
            CALL GBYTES (MESG(WORD),LAT(1),OFF,24,0,2)
            CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           If sign bit set to 1 , values are negative.
 
            DO INDEX1 = 1 , 2
              BLOCK_2(INDEX1+5) = LAT(INDEX1)
              IF (LAT(INDEX1).GT.8388608) THEN
                BLOCK_2(INDEX1+5) = BLOCK_2(INDEX1+5) - 8388608
                BLOCK_2(INDEX1+5) = - BLOCK_2(INDEX1+5)
              ENDIF
            END DO  
 
!           Resolution flag.
 
            CALL GBYTE (MESG(WORD),BLOCK_2(8),OFF,8)
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           Long of vertical axis.
 
            CALL GBYTE (MESG(WORD),LAT(1),OFF,24)
            CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           If sign bit set to 1 , values are negative.
 
            BLOCK_2(9) = LAT(1)
          
            IF (LAT(1).GT.8388608) THEN
              BLOCK_2(9) = BLOCK_2(9) - 8388608
              BLOCK_2(9) = - BLOCK_2(9)
            ENDIF
 
!           Direction increments / number of latitude lines.
 
            CALL GBYTES (MESG(WORD),BLOCK_2(10),OFF,24,0,2)
            CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           If no direction increments,coded with all bits set to 1.
!           Convert to 0 in this case.
 
            IF (BLOCK_2(8).EQ.0) THEN
              BLOCK_2(10) = 0
              BLOCK_2(11) = 0
            ENDIF
 
!           Projection centre flag.
 
            CALL GBYTE (MESG(WORD),BLOCK_2(12),OFF,8)
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           Scanning mode flags.
 
            CALL GBYTE (MESG(WORD),BLOCK_2(13),OFF,8)
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           4 reserved octets.
 
            CALL OFFSET (OFF,4,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
        
          ENDIF                          ! end of polar stereo.
    
!         Vertical coordinate parameters for hybrid levels.
    
          IF (NUM_VERT.NE.0) THEN        ! vertical coordinates.
         
            DO INDEX1 = 1 , NUM_VERT
            
              CALL GBYTE (MESG(WORD),EXPO,OFF,8)
              CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN
              CALL GBYTE (MESG(WORD),MANTI,OFF,24)
              CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN
              CALL DECFP (VERT_COORDS(INDEX1),EXPO,MANTI)

            END DO  
  
          ENDIF                          ! end of vertical coords.
 
        ENDIF                            ! end of block 2.
 
!       RETURN if only product and grid definition blocks required.
 
        IF (LEN_FP.EQ.1) RETURN

      ENDIF                              ! end of no LIMEXT block.

!     Intermediate calculations to cope with scan down column
!     rather than along row.

      ROW_LEN  = BLOCK_2(4)
      GPOFF    = ROW_LEN * (SCAN_POSN(2) - 1) + SCAN_POSN(1)
      TEMP_INT = BLOCK_2(13) / 32

      IF (MOD(TEMP_INT,2).EQ.1) THEN

!       Grid scans down columns rather than along rows.

        ROW_LEN = BLOCK_2(5)

        IF (LIMEXT.NE.0) THEN

          TEMP_INT = LIM_ROWS
          LIM_ROWS = LIM_COLS   ! switch round rows and columns.
          LIM_COLS = TEMP_INT

          TEMP_INT = SCAN_POSN(1)
          SCAN_POSN(1) = SCAN_POSN(2)
          SCAN_POSN(2) = TEMP_INT

          TEMP_INT = SCAN_POSN(3)
          SCAN_POSN(3) = SCAN_POSN(4)
          SCAN_POSN(4) = TEMP_INT

          GPOFF = ROW_LEN * (SCAN_POSN(2) - 1) + SCAN_POSN(1)

        ENDIF

      ENDIF
          
!     ***************************************************************
!
!                     BLOCK 3 - BIT MAP BLOCK.
!
!     ***************************************************************

!     BK3_INC is set to 1 , if block 3 is included.
 
      BK3_INC = 0
 
      IF (EDTN0.EQ.0) THEN
 
!            OLD VERSION OF CODE
!
!            BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!                none             00000000               0
!                 2               00000001               1
!                 3               00000010               2
!             2 and 3             00000011               3
 
        IF (BLOCK_1(4).EQ.2.OR.BLOCK_1(4).EQ.3) BK3_INC = 1
      ELSE
 
!            NEW VERSION
!
!            BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!                none             00000000               0
!                 2               10000000             128
!                 3               01000000              64
!             2 and 3             11000000             192
 
        IF (BLOCK_1(4).EQ.64.OR.BLOCK_1(4).EQ.192) BK3_INC = 1
      ENDIF
 
      IF (BK3_INC.EQ.1) THEN                ! start of bitmap section.

        DO INDEX1 = 1 , LEN_BITMAP
          BITMAP(INDEX1) = 9
        END DO  

!       If doing a limited area extract, need to get stored WORD and
!       OFF.

        IF (LIMEXT.EQ.1) THEN
          WORD = LIM_WORD
          OFF  = LIM_OFF
        ENDIF

!       Get length of the bit map block
 
        CALL GBYTE (MESG(WORD),LEN_BK3,OFF,24)
        CALL OFFSET(OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
        IF( ERROR .EQ. 3 ) RETURN
 
!       Get number of unused bits at the end of the block 3.
 
        CALL GBYTE(MESG(WORD),NUB,OFF,8)
        CALL OFFSET(OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF( ERROR .EQ. 3 ) RETURN
 
!       Check if bit map follows
 
        CALL GBYTE (MESG(WORD),BMF,OFF,16)
        CALL OFFSET(OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
        IF( ERROR .EQ. 3 ) RETURN
 
!       Get bit-map.
 
        IF (BMF.EQ.0) THEN                      ! get bitmap.

          NUM_BITMAP = (LEN_BK3 - 6) * 8 - NUB
 
!         Check that bitmap array is big enough.
 
          IF (LEN_BITMAP.LT.NUM_BITMAP) THEN
            ERROR = 3
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9301) NUM_BITMAP,
     &        LEN_BITMAP
 9301       FORMAT (' ERROR - number of points in bitmap - ', I7,
     &            ', array size - ', I7)
            RETURN
          ENDIF
 
!         Get bitmap.

          CALL GBYTES (MESG(WORD),BITMAP(1),OFF,1,0,NUM_BITMAP)
          CALL OFFSET(OFF,NUM_BITMAP,WORD,1,WORD_SIZE,LEN_MESG,ERROR)
          IF(ERROR .EQ. 3) RETURN

          IF (LIMEXT.NE.0) THEN

!           Analyse the bitmap to find out which points
!           to unpack. The format of the array DATA_PTN is
!           (#skip,#unpack,#skip,#unpack,etc). #skip are the
!           number of data points (ie 1s in the bitmap) which
!           we don't need to unpack. #unpack are the number of
!           points inside the limited area box.

!           Initialise.

            BOX_ROW = 0
            IN      = 0
            BOXED   = 0
            SKIP    = 0
            UNPK    = 0
            COUNT   = 1

!           Scan bitmap, as far as the last point in the limited area.

            DO INDEX1 = 1 , ROW_LEN * (SCAN_POSN(4) - 1)
     &                              + SCAN_POSN(3) + 1

!             Find position in grid, and in limited area box.

              ROW_COUNT = (INDEX1 - 1) / ROW_LEN + 1
              BOX_ROW   = ROW_COUNT - SCAN_POSN(2) + 1  
              IF (BOX_ROW.LT.0.OR.BOX_ROW.GT.LIM_ROWS) BOX_ROW = 0

              COL_COUNT = INDEX1 - (ROW_COUNT - 1) * ROW_LEN
              COL_IN    = 0
              IF (COL_COUNT.LE.SCAN_POSN(3).AND.COL_COUNT.GE.
     &            SCAN_POSN(1)) COL_IN = 1

!             Check to see whether we're in the limited area.

              BOXED = 0
              IF (BOX_ROW.NE.0.AND.COL_IN.EQ.1) BOXED = 1

!             Update position flag.

              IF (IN.EQ.0.AND.BOXED.EQ.1) THEN   ! moved into box.

                DATA_PTN(COUNT) = SKIP
                IN    = 1
                SKIP  = 0
                COUNT = COUNT + 1

              ENDIF

              IF (IN.EQ.1.AND.BOXED.EQ.0) THEN   ! moved out of box.

                DATA_PTN(COUNT) = UNPK
                IN    = 0
                UNPK  = 0
                COUNT = COUNT + 1

              ENDIF

!             Now, do the real work!

              IF (BITMAP(INDEX1).EQ.1) THEN
                IF (IN.EQ.0) SKIP = SKIP + 1
                IF (IN.EQ.1) UNPK = UNPK + 1
              ENDIF

            END DO

            LEN_DATAPTN = COUNT - 1

          ENDIF                     ! end of limited extract section.

!         Skip over unused bits
 
          CALL OFFSET(OFF,NUB,WORD,1,WORD_SIZE,LEN_MESG,ERROR)
 
        ELSE                 !  trying to read a site-defined bitmap.
 
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE( UNIT = ERR_U, FMT = '(A)' )
     &      ' ERROR - predefined bitmap not available.'
          RETURN
        
        ENDIF                   ! end of BMF section.
 
      ENDIF                     ! end of block 3 included section.
 
!     ***************************************************************
!
!                  BLOCK 4  -  BINARY DATA BLOCK
!
!     ***************************************************************
 
!     Get length of binary data block.
   
!     If no bitmap, and limited extract being done, we need to restore
!     WORD and OFF.

      IF (BK3_INC.EQ.0.AND.LIMEXT.NE.0) THEN
        WORD = LIM_WORD
        OFF  = LIM_OFF
      ENDIF

      TEMP_WORD = WORD
      TEMP_OFF = OFF
 
      CALL GBYTE (MESG(WORD),OCTS_BK4,OFF,24)
      CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
    
!     4 bit flag / 4 bit count of unused bits at end of block octet.
    
      CALL GBYTE (MESG(WORD),FLAG,OFF,8)
      CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
 
      IF (EDTN0.EQ.0) THEN
 
!       OLD VERSION OF CODE
!
!       0000---- grid point
!       0001---- spherical harmonics
 
        REP = FLAG / 16
        UNUSED = FLAG - REP * 16
 
      ELSE
 
!       NEW VERSION OF CODE
!
!       0000---- grid point
!       1000---- spherical harmonics
 
        REP = FLAG / 128
        UNUSED = FLAG - REP * 128
 
      ENDIF
    
!     Get scale factor
    
      CALL GBYTE (MESG(WORD),SCALE,OFF,16)
      CALL OFFSET (OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
 
      TEMP_SCALE2 = SCALE
      
      IF (SCALE.GE.32768) THEN
        TEMP_SCALE2 = TEMP_SCALE2 - 32768
        TEMP_SCALE2 = - TEMP_SCALE2
      ENDIF
    
!     Get reference value (DATA_MIN) in GRIB format (EXPO,MANTI)
    
      CALL GBYTE (MESG(WORD),EXPO,OFF,8)
      CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
      
      CALL GBYTE (MESG(WORD),MANTI,OFF,24)
      CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
                 
      CALL DECFP (DATA_MIN,EXPO,MANTI)
      REAL_SCALE = 2.**TEMP_SCALE2
 
!     Get number of bits in each data value.
    
      CALL GBYTE (MESG(WORD),WIDTH,OFF,8)
      CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
 
!     Check number of bits per data field.
 
      IF (WIDTH.GT.WORD_SIZE.OR.WIDTH.GT.Max_Bits) THEN
        ERROR = 3
        IF (MSGLVL.LT.3) WRITE (ERR_U,9401) WIDTH,WORD_SIZE,
     &    Max_Bits
 9401   FORMAT (' ERROR - number of bits per data value, ', I3,
     &          ' exceeds word length, ', I3, ' or maximum ',
     &          'permitted value, ', I3)
        RETURN
      ENDIF

      IF (WIDTH.EQ.0) THEN                                          !5.8
        ERROR=3                                                     !5.8
        IF (MSGLVL.LT.3) WRITE (ERR_U,9405) WIDTH                   !5.8
 9405   FORMAT (' ERROR - number of bits per data value, ',I3,      !6.0
     &          ' Data cannot be decoded')                          !5.8
        RETURN                                                      !5.8
      ENDIF                                                         !5.8
     
!     If data is in spherical harmonic form, next 4 octets
!     contain the real (0,0) coefficient.
    
      IF (REP.EQ.1) THEN
 
!       Get real (0,0) coefficient in grib format and
!       convert to floating point.
 
        CALL GBYTE (MESG(WORD),EXPO,OFF,8)
        CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
        
        CALL GBYTE (MESG(WORD),MANTI,OFF,24)
        CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
        
        CALL DECFP (FP_DATA(1),EXPO,MANTI)
                         
      ENDIF
    
!     Decode data values to floating point and store in FP_DATA.

!--------------------------
!     No bit-map.
!--------------------------

      IF (BK3_INC.EQ.0) THEN
 
!       First calculate the number of data values.
 
        IF (LIMEXT.EQ.0) THEN
          NUM_FP = OCTS_BK4 - 11 - REP * 4
          NUM_FP = (NUM_FP * 8 - UNUSED) / WIDTH
        ELSE
          NUM_FP = LIM_COLS * LIM_ROWS
        ENDIF
 
!       Check length of output array.
 
        IF (NUM_FP+REP.GT.LEN_FP) THEN
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE (ERR_U,9402) NUM_FP,LEN_FP
 9402     FORMAT (' ERROR - number of values to be decoded is - ',
     &            I7, ', array size - ', I7)
          RETURN
        ENDIF

!       If limited extract, we just need to unpack small chunks.

        IF (LIMEXT.EQ.0) THEN

# if defined (HP) || defined (CRAY) ||defined (SX6)   /*6.1*/
          DO I=1,NUM_FP                                !6.0
            IN_DATA(I) = TRANSFER (FP_DATA(REP+I),0)   !6.0
	  ENDDO                                        !6.0
          CALL GBYTES (MESG(WORD),IN_DATA,OFF,WIDTH,0, !6.0
     &                 NUM_FP)                         !6.0
          DO I=1,NUM_FP                                !6.0
            FP_DATA(REP+I) = TRANSFER (IN_DATA(I),0.0) !6.0
	  ENDDO                                        !6.0
# else
          CALL GBYTES (MESG(WORD),FP_DATA(REP+1),OFF,WIDTH,0,
     &                 NUM_FP)
# endif

          CALL OFFSET (OFF,NUM_FP,WORD,WIDTH,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
        ELSE
          OFF = OFF + (GPOFF - 1) * WIDTH

          DO INDEX1 = 1 , LIM_ROWS
	  
# if defined (HP) || defined (CRAY) || defined (SX6)                 /*6.1*/
            DO I=1,LIM_COLS
              IN_DATA(I) = TRANSFER                                   !6.0
     &                   (FP_DATA((REP+(INDEX1-1)*LIM_COLS+1)+I-1),0) !6.0
            ENDDO                                                     !6.0
            CALL GBYTES(MESG(WORD),IN_DATA,                           !6.0
     &                  OFF,WIDTH,0,LIM_COLS) 
            DO I=1,LIM_COLS                                           !6.0
	      FP_DATA((REP+(INDEX1-1)*LIM_COLS+1)+I-1)=               !6.0
     &                  TRANSFER(IN_DATA(I),0.0)                      !6.0
            ENDDO                                                     !6.0
# else     
              CALL GBYTES(MESG(WORD),FP_DATA(REP+(INDEX1-1)*LIM_COLS+1),
     &                  OFF,WIDTH,0,LIM_COLS)
# endif
     
            CALL OFFSET (OFF,LIM_COLS,WORD,WIDTH,WORD_SIZE,LEN_MESG,
     &                   ERROR)
            IF (ERROR .EQ. 3) RETURN
            OFF = OFF + (ROW_LEN - LIM_COLS) * WIDTH
          END DO
        ENDIF                    ! no need to correct offset.

!       Scale and store floating point values.

        DO INDEX1 = REP+1 , REP+NUM_FP
          ZGRIB           = FP_DATA (INDEX1)
          FP_DATA(INDEX1) = DATA_MIN + JGRIB * REAL_SCALE
        END DO  
 
      ENDIF
!-------------------------
!     Bit-map present
!-------------------------
      IF (BK3_INC.EQ.1) THEN
 
!       First calculate the number of data values.

        IF (LIMEXT.EQ.0) THEN
          NUM_FP = OCTS_BK4 - 11 - (REP * 4)
          NUM_FP = (NUM_FP * 8 - UNUSED) / WIDTH
        ELSE

!         Count up how many points present in the area.

          NUM_FP = 0

          DO INDEX1 = 2 , LEN_DATAPTN , 2
            NUM_FP = NUM_FP + DATA_PTN(INDEX1)
          END DO

        ENDIF

!       Check length of output array.
 
        IF ((NUM_FP+REP).GT.LEN_FP) THEN
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE (ERR_U,9402) NUM_FP,LEN_FP
          RETURN
        ENDIF
 
!       Calculate missing data indicator.
 
        MAX_INT = (2 ** WIDTH)
        MAX_REAL = DATA_MIN + MAX_INT * REAL_SCALE
        BLOCK_R(2) = MAX_REAL
        BLOCK_3(1) = NINT ( MAX_REAL + .5 )
 
        IF (MSGLVL .LT. 1) WRITE (ERR_U,9403) BLOCK_R(2)
 9403   FORMAT(' NOTE - bit-map used. Missing data indicator - ',
     &         F12.4)

!       If we're extracting a limited area, we get chunks.

        IF (LIMEXT.EQ.0) THEN
	
# if defined (HP) || defined (CRAY) || defined (SX6)               /*6.1*/
          DO I=1,NUM_FP                                             !6.0
            IN_DATA(I) = TRANSFER (FP_WORK(REP+I),0)                !6.0
	  ENDDO                                                     !6.0
          CALL GBYTES (MESG(WORD),IN_DATA,OFF,WIDTH,0,NUM_FP)       !6.0
          DO I=1,NUM_FP                                             !6.0
	    FP_WORK(REP+I) = TRANSFER(IN_DATA(I),0.0)               !6.0
	  ENDDO                                                     !6.0
# else
          CALL GBYTES (MESG(WORD),FP_WORK(REP+1),OFF,WIDTH,0,NUM_FP)
# endif

          CALL OFFSET (OFF,NUM_FP,WORD,WIDTH,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
        ELSE
          TEMP_INT = 1

          DO INDEX1 = 1 , LEN_DATAPTN , 2

!           Skip DATA_PTN(INDEX1) * WIDTH.

            CALL OFFSET (OFF,DATA_PTN(INDEX1),WORD,WIDTH,
     &                   WORD_SIZE,LEN_MESG,ERROR)

!           Unpack DATA_PTN(INDEX1 + 1).

# if defined (HP) || defined (CRAY) || defined (SX6)      /*6.1*/
            DO I=1,DATA_PTN(INDEX1+1)                      !6.0
              IN_DATA(I) = TRANSFER                        !6.0
     &                   (FP_WORK(REP+TEMP_INT+I-1),0)     !6.0
            ENDDO                                          !6.0
            CALL GBYTES (MESG(WORD),IN_DATA,               !6.0
     &                   OFF,WIDTH,0,DATA_PTN(INDEX1+1))   !6.0
            DO I=1,DATA_PTN(INDEX1+1)                      !6.0
     	      FP_WORK(REP+TEMP_INT+I-1) =                  !6.0
     &	      TRANSFER(IN_DATA(I),0.0)                     !6.0
            ENDDO                                          !6.0
# else
            CALL GBYTES (MESG(WORD),FP_WORK(REP+TEMP_INT),
     &                   OFF,WIDTH,0,DATA_PTN(INDEX1+1))
# endif

            CALL OFFSET (OFF,DATA_PTN(INDEX1+1),WORD,WIDTH,
     &                   WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN

            TEMP_INT = TEMP_INT + DATA_PTN(INDEX1 + 1)

          END DO

        ENDIF

        DATA_PT = 1

!       Scale and store.

        IF (LIMEXT.EQ.0) THEN               ! scan through whole map.

          DO INDEX1 = REP+1 , NUM_BITMAP

            IF (BITMAP(INDEX1).NE.0.AND.BITMAP(INDEX1).NE.1) THEN
              ERROR = 3
              IF (MSGLVL .LT. 3) WRITE (ERR_U,9404) INDEX1,
     &          BITMAP(INDEX1)
 9404         FORMAT(' ERROR - bit-map element (', I9, ') = ',
     &               I1, ' - invalid')
              RETURN
            ENDIF

            IF (BITMAP(INDEX1).EQ.0) THEN
              FP_DATA(INDEX1) = BLOCK_R(2)
            ENDIF
 
            IF (BITMAP(INDEX1).EQ.1) THEN
              ZGRIB = FP_WORK(DATA_PT)
              FP_DATA(INDEX1) = DATA_MIN + JGRIB * REAL_SCALE
              DATA_PT = DATA_PT + 1
            ENDIF

          END DO

        ENDIF 

        IF (LIMEXT.NE.0) THEN            ! scan through limited area.

          COUNT = 1

          DO INDEX1 = GPOFF , (SCAN_POSN(4) - 1) * ROW_LEN
     &                        + SCAN_POSN(3)

            IF (BITMAP(INDEX1).NE.0.AND.BITMAP(INDEX1).NE.1) THEN
              ERROR = 3
              IF (MSGLVL .LT. 3) WRITE (ERR_U,9404) INDEX1,
     &          BITMAP(INDEX1)
              RETURN
            ENDIF

            COL_COUNT = MOD(INDEX1,ROW_LEN)        ! check X position.
            IF (COL_COUNT.EQ.0) COL_COUNT = ROW_LEN

            IF (COL_COUNT.GE.SCAN_POSN(1).AND.     ! inside the box.
     &          COL_COUNT.LE.SCAN_POSN(3)) THEN

              IF (BITMAP(INDEX1).EQ.0) THEN
                FP_DATA(COUNT) = BLOCK_R(2)
              ENDIF
 
              IF (BITMAP(INDEX1).EQ.1) THEN
                ZGRIB = FP_WORK(DATA_PT)
                FP_DATA(COUNT) = DATA_MIN + JGRIB * REAL_SCALE
                DATA_PT = DATA_PT + 1
              ENDIF

              COUNT = COUNT + 1

            ENDIF

          END DO  

          NUM_BITMAP = LIM_COLS * LIM_ROWS

        ENDIF

!       Set length equal to number of grid points.

        NUM_FP = NUM_BITMAP
 
      ENDIF
 
!     Include (0,0) coefficient in count, if spherical harmonic data.
 
      NUM_FP = REP + NUM_FP

!     Can return now, if doing a limited extract.

      IF (LIMEXT.NE.0) RETURN

!     Skip any zero fill at end of data block.
    
      ZERO_BITS = (WORD-TEMP_WORD) * WORD_SIZE + OFF -TEMP_OFF
      ZERO_BITS = OCTS_BK4 * 8 - ZERO_BITS
      
      IF (ZERO_BITS.NE.0) THEN
        CALL OFFSET (OFF,1,WORD,ZERO_BITS,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
      ENDIF
 
!     ***************************************************************
!
!                       BLOCK 5 - END BLOCK.
!
!     ***************************************************************
 
!     Check 7 7 7 7 at end of coded data.
 
      CALL GBYTES (MESG(WORD),STORE(1),OFF,8,0,4)

!     55 is the value of ascii character '7'
 
      DO INDEX1 = 1 , 4
        IF (STORE(INDEX1).NE.55) ERROR = 3
      END DO  
 
      IF (ERROR .EQ. 3 .AND. MSGLVL .LT. 3) WRITE ( UNIT = ERR_U,
     &  FMT = '(A)' ) ' ERROR - no 7777 group found'
 
      RETURN
      END ! SUBROUTINE Decode
