!+    Decode an edition 1 GRIB message
! $Header: grib_edtn_1_dec.F, 1, 27/02/2007 16:05:01, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Grib_Edtn_1_Dec (fp_data, fp_work, len_fp, num_fp,
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
!       WARNING - The maximum number of bits per data value is 32.
!       This is non-standard grib.
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
!$Log:
! 1    Met_DB_Project 1.0         27/02/2007 16:05:01    Stan Kellett    
!$
!Revision 6.4  2004/05/18 11:31:59  usmdb
!Add SX6 directives.
!
!Revision 6.3  2003/01/14 16:29:12  usmdb
!fixed bug introduced with addition of Super Stash code. 5 changed
!to 29 in array STORE.
!Stan Kellett. 14th January 2003
!
!Revision 6.2  2002/07/05 10:02:11  usmdb
!changed byte to pp_bit_length as this was a bug.
!Stan Kellett.
!
!Revision 6.1  2001/11/05 10:18:08  usmdb
!Superstash.
!
!Revision 6.0  2001/08/20 11:20:22  usmdb
!New version using pre-processor Statements to cope with f90 compiler on HPUX11.
!
!
! $Revision: 1$
! $Date: 27/02/2007 16:05:01$
!
!
!     Scalar arguments with INTENT(IN):

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
      INTEGER    num_q             ! Number of elements of quasi
                                   ! used
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

      INTEGER    Max_Bits          ! Maximum number of bits per data
      PARAMETER (Max_Bits = 30)    ! field
      INTEGER    PP_BIT_LENGTH     !6.1 Number of bits for Super Stash
                                   !6.1 pp header information returned
				   !6.1 in each BLOCK_1 element
      PARAMETER (PP_BIT_LENGTH=32) !6.1

!     Local Scalars:

!     Used throughout routine
 
      INTEGER    jgrib            ! Integer used in icky equivalence
      INTEGER    byte             ! Size of chunk to extract
      INTEGER    nval             ! Number of chunks to extract
      INTEGER    words_mesg       ! Length of message in words
      INTEGER    index1           ! Loop indicies
      INTEGER    index2
      INTEGER    err_u            ! Unit number actually used for
                                  ! error messages
      INTEGER    loop             !6.1 loop control used in Block 1
      REAL       zgrib            ! Real used in equivalence (bleah)
      REAL       data_min         ! Minimum value in data
      REAL       real_scale       ! Scale factor
      REAL       max_real         ! Missing data indicator scaled by
                                  ! max_int
      REAL       convert          ! Convert to SI factor
            
!     Used in block 2
 
      INTEGER    blk2_inc         ! Block 2 included indicator
      INTEGER    len_bk2          ! Length of block 2 (bytes)
      INTEGER    max_bk2          ! Grid definition size of blk 2
      INTEGER    q_inc            ! Quasi-regular grid indicator
 
!     Used in block 3
 
      INTEGER    blk3_inc         ! Block 3 included indicator
      INTEGER    len_bk3          ! Length of block 3 (bytes)
      INTEGER    nub              ! Number of unused bits
      INTEGER    bmf              ! Bitmap following indicator
 
!     Used in block 4
 
      INTEGER    expo             ! Exponent of ref. value
      INTEGER    manti            ! Mantissa of ref. value
      INTEGER    octs_bk4         ! Length of block 4 (bytes)
      INTEGER    rep              ! Grid point/sph. harms. indicator
      INTEGER    flag             ! Flag in block 4
      INTEGER    flag2            ! Continuation of FLAG
      INTEGER    unused           ! Number of bits unused in blk 4
      INTEGER    pac              ! Row by row packing indicator
      INTEGER    integ            ! Integer data indicator
      INTEGER    cont             ! Flag continuation indicator
      INTEGER    scale            ! Scale factor
      INTEGER    max_int          ! Max. integer possible in width + 1
      INTEGER    data_pt          ! Position in data array
      INTEGER    num_rows         ! Number of rows in the grid
      INTEGER    num_cols         ! Number of points in a row
      INTEGER    n1               ! Octet number at which 1st order
                                  ! start
      INTEGER    n2               ! Octet number at which 2nd order
                                  ! start
      INTEGER    p1               ! Number of 1st order packed values
      INTEGER    p2               ! Number of 2nd order packed values
      INTEGER    oct_1st          ! Number of octets in 1st order block
      INTEGER    row_pt           ! Data points in a row
      INTEGER    row_pos          ! Position in row
      INTEGER    zero_bits        ! Zero fill bits at end of block 4

!     Used for limited area extract

      INTEGER    lim_word         ! pointer to start of data block
      INTEGER    lim_off          ! offset of start of data block
      INTEGER    limext           ! non-zero for limited extract
      INTEGER    lim_cols         ! number of columns in limited area
      INTEGER    lim_rows         ! number of rows in limited area
      INTEGER    row_len          ! row length of whole grid
      INTEGER    col_len          ! column length of whole grid
      INTEGER    gpoff            ! offset of first box point in grid
      INTEGER    box_row          ! position counter used with bitmap
      INTEGER    in               ! flag used with bitmap (1 when in
                                  ! box)
      INTEGER    boxed            ! another flag with bitmap
      INTEGER    skip             ! counter used with bitmap
      INTEGER    unpk             ! another bitmap counter!
      INTEGER    count            ! number of values present in data
      INTEGER    row_count        ! position in box
      INTEGER    col_count        ! position in box
      INTEGER    col_in           ! inside box column range if 1
      INTEGER    len_dataptn      ! number of values in data_ptn
      INTEGER    counter1         ! counter used for rbr + limext
      INTEGER    counter2         ! ditto
      INTEGER    skipping         ! flag (1=skipping values)
      INTEGER    lim_data         ! count of values scaled so far

!     Temporary values
 
      INTEGER    temp_int         ! Temporary array
      INTEGER    temp_word        ! Pointer to start of data block
      INTEGER    temp_off         ! Offset of start of data block
      INTEGER    temp_scale2      ! Temporary scale factor
      INTEGER    temp_rows        ! Either num_rows or lim_rows
      INTEGER    temp_cols        ! Either num_cols or lim_cols
      INTEGER    temp_skip        ! Number of values to skip
      INTEGER    temp_unpk        ! Number to unpack
# if defined (HP) || defined (CRAY) || defined (SX6)    /*6.3*/
      INTEGER    IN_DATA(LEN_FP)  !6.0 temp integer to represent real
      INTEGER    I                !6.0 loop counter
# endif

!     Local Arrays:

      INTEGER    store(500)       ! Temporary array
      INTEGER    lat(2)           ! Store for lat/long values
      INTEGER    scan_posn(4)     ! posn of box allowing for scan mode
      INTEGER    data_ptn(50000)  ! array to store analysis of bitmap
      
!     Functions and subroutines used:

      EXTERNAL   GBytes
      EXTERNAL   GByte
      EXTERNAL   Offset
      EXTERNAL   DecFP
      EXTERNAL   Level_Desc_Dec
      EXTERNAL   Time_Range_Dec

!-    End of header
     

      EQUIVALENCE (ZGRIB,JGRIB)
   
!
! Revision Code:
      LOGICAL HEADSET
      CHARACTER*132 HEAD
      DATA HEADSET /.FALSE./
      
      IF (.NOT.HEADSET) THEN
        HEAD='
     &$Source: /home/us0400/grib/source/RCS/grib_edtn_1_dec.F,v $
     &'//'$Date: 27/02/2007 16:05:01$'
        HEADSET = .TRUE.
      ENDIF     
                 
! End of Rev
!
      
!     Clear error indicator.

      ERROR = 0

!     Ensure that array lengths returned are zero in cases where
!     the arrays are not used.

      num_vert = 0
      num_q = 0
      num_bitmap = 0

!     Check that the user has passed a sensible unit number for error
!     messages

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
!                     BLOCK 0 - INDICATOR BLOCK.
!
!     ***************************************************************

!     Save WORD and OFFSET, in case we are doing a limited area
!     extract.

      LIM_WORD = WORD
      LIM_OFF  = OFF

!     Extract 8 octets containing ascii G R I B, length of message
!         and edition number.
 
      WORD = 1
      OFF  = 0
      BYTE = 8
      NVAL  = 4
 
      CALL GBYTES (MESG(1),STORE(1),0,BYTE,0,NVAL)
      CALL OFFSET (OFF,NVAL,WORD,BYTE,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
 
!     Test for ascii G R I B
 
      IF (STORE(1).NE.71.OR.STORE(2).NE.82.OR.STORE(3).NE.73.
     &    OR.STORE(4).NE.66) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE( UNIT = ERR_U, FMT = '(A)' )
     &    ' ERROR - header does not contain ascii G R I B'
        RETURN
      ENDIF
 
!     Pick up total length of message and check array length.
 
      CALL GBYTE (MESG(WORD),BLOCK_0(3),OFF,24)
      CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
 
      WORDS_MESG = BLOCK_0(3) * 8 / WORD_SIZE
      
      IF (WORDS_MESG.GT.LEN_MESG) THEN
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9001) WORDS_MESG,
     &    LEN_MESG
 9001   FORMAT (' ERROR - size of message = ', I9,
     &          '. Size of array = ', I9)
        RETURN
      ENDIF
 
!     Get edition number.
 
      CALL GBYTE(MESG(WORD),BLOCK_0(1),OFF,8)
      CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)

!     Check for limited area extract.

      IF (POSN(1).NE.0) THEN
        LIMEXT = 1

        LIM_COLS = ABS(POSN(3) - POSN(1)) + 1
        LIM_ROWS = ABS(POSN(4) - POSN(2)) + 1

!       Virtual position of box - allows for scanning modes
!       128 and/or 64.

        SCAN_POSN(1) = MIN(POSN(1),POSN(3))
        SCAN_POSN(2) = MIN(POSN(2),POSN(4))
        SCAN_POSN(3) = MAX(POSN(1),POSN(3))
        SCAN_POSN(4) = MAX(POSN(2),POSN(4))
      ELSE
        LIMEXT = 0
      ENDIF

      IF (LIMEXT.EQ.0) THEN              ! start of no limext block.

!     ***************************************************************
!
!               BLOCK 1 - PRODUCT DEFINITION BLOCK.
!
!     ***************************************************************

!       Extract first 4 octets of block 1.
 
        NVAL  = 28                          !6.1
 
        CALL GBYTES (MESG(WORD),STORE(1),OFF,BYTE,0,NVAL)
        CALL OFFSET (OFF,NVAL,WORD,BYTE,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Length of block 1.
        BLOCK_1(21) =STORE(1)*65536+STORE(2)*256+STORE(3)             !6.1

!6.1 If length of section 1 (given by STORE(1) to STORE(3) is greater
!6.1 than expanded national section.
        IF (BLOCK_1(21).GT.28) THEN                                   !6.1
	
!6.1 If UKMO (Originating centre is 74) then if Sub centre is 2 then the
!6.1 following section from octets 29 to 220 holds pp information
!6.1 for Super Stash.
	  IF ((STORE(5).EQ.74).AND.(STORE(26).EQ.2)) THEN     !6.1

!6.1 Extract octets 29 to 40 as twelve 8-bit values
!6.1 These values are reserved for future use so ignore for now.
            NVAL = 12                                               
            CALL GBYTES (MESG(WORD),STORE(29),OFF,BYTE,0,NVAL) !6.1
            CALL OFFSET (OFF,NVAL,WORD,BYTE,WORD_SIZE,LEN_MESG,ERROR) !6.1
            IF (ERROR .EQ. 3) RETURN        !6.1
	    
!6.1 Extract octets 41 to 220 as forty-five 32-bit words
            NVAL = 45                                               
            CALL GBYTES (MESG(WORD),STORE(41),OFF,PP_BIT_LENGTH       !6.1
     &                       ,0,NVAL)                                 !6.1
            CALL OFFSET (OFF,NVAL,WORD,PP_BIT_LENGTH,WORD_SIZE,       !6.2
     &                   LEN_MESG,ERROR)                              !6.2
            IF (ERROR .EQ. 3) RETURN                                  !6.1

!6.1 Now set BLOCK_1(34) to hold the Sub Centre number
            BLOCK_1(34) = STORE(26)                                   !6.1
	    
!6.1 Store the Super Stash pp information in BLOCK_1(35) to BLOCK_1(79) 
            DO LOOP=35,79                                             !6.1
	      BLOCK_1(LOOP) = STORE(LOOP+6)                           !6.1
	    ENDDO	
          ELSE
            ERROR = 2
            IF (MSGLVL .LT. 2) WRITE( UNIT = ERR_U, FMT = '(A)' )
     &        ' WARNING - extended product definition section used.'
            NVAL  = BLOCK_1(21) - 28        !6.1 already got the first 28 bytes
!       Extract next NVAL octets of block 1.
 
            CALL GBYTES (MESG(WORD),STORE(29),OFF,BYTE,0,NVAL)  !6.3
            CALL OFFSET (OFF,NVAL,WORD,BYTE,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN

!6.1 There are 12 reserved values, so store from Octet 40 to end as 1
!6.1 Octet per array element	    
            IF ( NVAL .GT. 12 ) THEN
              DO INDEX1 = 1 , NVAL - 12
                BLOCK_1(33 + INDEX1) = STORE(40+INDEX1)
              END DO
            ENDIF
	  ENDIF
        ENDIF
 
!6.1    Set the Table 2 edition number.
 
        BLOCK_0(2) = STORE(4)
 
        IF (MSGLVL .LT. 1) WRITE (ERR_U,9101) BLOCK_0(2)
 9101   FORMAT(' NOTE - coded using Table 2 version -- ', I3)
 
 
!       Transfer product definition information to output array
 
        DO index1 = 1 , 5
          block_1(index1) = store(index1 + 4)
        END DO  
 
        CALL Level_Desc_Dec(store(10), block_1(6), block_1(7),
     &    block_1(8))
      
        DO index1 = 9 , 14
          block_1(index1) = store(index1 + 4)
        END DO  

        CALL Time_Range_Dec(store(19), block_1(15), block_1(16),
     &    block_1(17), block_1(18))
     
!       Century of reference time.
 
        BLOCK_1(19) = STORE(25)

!       Decimal scale factor (D). (NB STORE(26) is reserved.)
 
        BLOCK_1(20) = STORE(27) * 256 + STORE(28)

!       D can be negative - if so, leftmost bit will be set to 1.

        IF (BLOCK_1(20) .GT. 32768) THEN
          BLOCK_1(20) = BLOCK_1(20) - 32768
          BLOCK_1(20) = - BLOCK_1(20)
        ENDIF



!       *************************************************************
!
!                 BLOCK 2 - GRID DESCRIPTION BLOCK.
!
!       *************************************************************
 
!       Check flag indicating if block 2 is included in code.
!       BLK2_INC is set to 1 if block 2 is included.
 
        BLK2_INC = 0
 
!       NEW VERSION
!
!       BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!           none             00000000               0
!            2               10000000             128
!            3               01000000              64
!        2 and 3             11000000             192
 
        IF (BTEST(BLOCK_1(4), 7)) BLK2_INC = 1    !5.15
 
        IF (BLK2_INC.EQ.1) THEN           ! block 2 is included.
    
!         Length of grid description block.
    
          CALL GBYTE (MESG(WORD),LEN_BK2,OFF,24)
          CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
 
!         Length = 32 octets => regular lat/long
!                               regular gaussian
!                               mercator
!                               polar stereographic
!                               spherical harmonics
!
!                  42 octets => rotated lat/long
! 
!         The vertical coords are added.
!
!         Get number of vertical coordinate parameters, if any.
 
          CALL GBYTE (MESG(WORD),BLOCK_2(1),OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
 
          IF (LEN_VERT.LT.BLOCK_2(1)) THEN
            ERROR = 3
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9201) BLOCK_2(1),
     &        LEN_VERT
 9201       FORMAT (' ERROR - number of vertical coordinates - ',
     &      I4, ', array size is - ', I4)
            RETURN
          ENDIF
 
!         Set NUM_VERT = BLOCK_2(1) (redundant information, but
!         maintains consistency with other arrays.)
 
          NUM_VERT = BLOCK_2(1)
    
!         PV, PL or 255. 
    
          CALL GBYTE (MESG(WORD),BLOCK_2(2),OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
    
!         Data representation type.
    
          CALL GBYTE (MESG(WORD),BLOCK_2(3),OFF,8)
          CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN
 
!         Check for a quasi-regular grid.
 
          Q_INC    = 0
          MAX_BK2 = 32    ! most grid types use 32 octets.

          IF (BLOCK_2(3).EQ.10.OR.BLOCK_2(3).EQ.3) MAX_BK2 = 42    ! 5.14 
!                                                     rotated lat uses 42

          IF (LEN_BK2.GT.MAX_BK2) THEN
 
!           Either have vertical coords, or quasi grid, or both.
 
            IF (BLOCK_2(1).NE.0) THEN
 
!             Vertical coords.
 
              TEMP_INT = MAX_BK2 + BLOCK_2(1) * 4

              IF (TEMP_INT.NE.LEN_BK2) THEN
                Q_INC   = 1                  ! quasi regular grid too
                NUM_Q = (LEN_BK2 - TEMP_INT) / 2
              ELSE
                Q_INC   = 0
              ENDIF

            ELSE
 
!             Just a quasi regular grid.
 
              Q_INC   = 1
              NUM_Q = (LEN_BK2 - MAX_BK2) / 2

            ENDIF

          ENDIF
 
!         Lat/longitude grid, gaussian grid, spherical harmonics,
!         polar stereographic, Lambert Conformal and mercator are the only data
!         representations handled.

          IF (BLOCK_2(3).NE.0.AND.BLOCK_2(3).NE.4.AND.
     &        BLOCK_2(3).NE.50.AND.BLOCK_2(3).NE.1.AND.
     &        BLOCK_2(3).NE.5.AND.BLOCK_2(3).NE.10.AND.
     &        BLOCK_2(3).NE.3) THEN                            !5.14
           
             ERROR = 3
             IF (MSGLVL .LT. 3) WRITE ( UNIT = ERR_U, FMT = '(A)' )
     &         ' ERROR - grid description block not yet defined.'
             RETURN
          ENDIF
    
!         Lat/long, gaussian grid or mercator.
    

          IF (BLOCK_2(3).EQ.0.OR.BLOCK_2(3).EQ.4.OR.BLOCK_2(3).EQ.1.
     &        OR.BLOCK_2(3).EQ.10) THEN        ! lat/long grid
 
!           Number of lat/long points.
 
            CALL GBYTES (MESG(WORD),BLOCK_2(4),OFF,16,0,2)
            CALL OFFSET (OFF,2,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           Quasi regular grid has one of these set to 65535.
 
            IF (Q_INC.EQ.1) THEN
              DO INDEX1 = 1 , 2
                IF (BLOCK_2(INDEX1+3).EQ.65535) BLOCK_2(INDEX1+3) = 0
              END DO
            ENDIF
 
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
 
!           Quasi regular grid - one of direction incs. coded with
!           all bits set to 1.
 
            IF (Q_INC.EQ.1) THEN
              DO INDEX1 = 1 , 2
                IF (BLOCK_2(INDEX1+10).EQ.65535) BLOCK_2(INDEX1+10)=0
              END DO
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
 
!           1 reserved octet.
 
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

            ENDIF          ! end of rotated lat/long grid.

          ENDIF            ! end of lat/long grid.
    

! Lambert Conformal Grid                                         !5.14
          
           IF (BLOCK_2(3).EQ.3) THEN                             !5.14

! Number of lat/long points.                                     !5.14

            CALL GBYTES (MESG(WORD),BLOCK_2(4),OFF,16,0,2)       !5.14

! Shift Word from 11-12, OFF from 16-16                          !5.14

            CALL OFFSET (OFF,2,WORD,16,WORD_SIZE,LEN_MESG,ERROR) !5.14 
            IF (ERROR.EQ.3) RETURN                               !5.14
  
! Lat/long of origin.                                            !5.14
 
            CALL GBYTES (MESG(WORD),LAT(1),OFF,24,0,2)           !5.14

! Shift WORD from 12-14, OFF from 16-0                           !5.14

            CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR) !5.14 
            IF (ERROR.EQ.3) RETURN                               !5.14
 
! If sign bit set to 1 , values are negative.                    !5.14
  
            DO INDEX1 = 1 , 2                                    !5.14
              BLOCK_2(INDEX1+5) = LAT(INDEX1)                    !5.14
              IF (LAT(INDEX1).GT.8388608) THEN                   !5.14
                BLOCK_2(INDEX1+5) = BLOCK_2(INDEX1+5) - 8388608  !5.14
                BLOCK_2(INDEX1+5) = - BLOCK_2(INDEX1+5)          !5.14
              ENDIF                                              !5.14
            END DO                                               !5.14 
  
! Resolution flag.                                               !5.14

            CALL GBYTE (MESG(WORD),BLOCK_2(8),OFF,8)             !5.14

! Shift WORD from 14-14, OFF from 0-8                            !5.14

            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)  !5.14
            IF (ERROR.EQ.3) RETURN                               !5.14
  
!           Lov (d)                                              !5.14

            CALL GBYTES (MESG(WORD),BLOCK_2(9),OFF,24,0,1)       !5.14 

! Shift WORD from 14-15, OFF from 8,24                           !5.14

            CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR) !5.14 

            IF (ERROR.EQ.3) RETURN                               !5.14

! If sign bit set to 1 , values are negative.                    !5.14

            IF (BLOCK_2(9).GT.8388608) THEN                      !5.14
              BLOCK_2(9) = BLOCK_2(9) - 8388608                  !5.14
              BLOCK_2(9) = - BLOCK_2(9)                          !5.14
            ENDIF                                                !5.14
   
! Direction increments / number of latitude lines. (e)           !5.14
! Shift WORD from  15-16                                         !5.14

            CALL GBYTES (MESG(WORD),BLOCK_2(10),OFF,24,0,2)      !5.14
            CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR) !5.14
            IF (ERROR.EQ.3) RETURN                               !5.14
  
!           If no direction increments,coded with all bits set to 1. !5.14
!           Convert to 0 in this case.                           !5.14
  
            IF (BLOCK_2(8).EQ.0) THEN                            !5.14
              BLOCK_2(10) = 0                                    !5.14
              BLOCK_2(11) = 0                                    !5.14
            ENDIF                                                !5.14
  
! Projection Centre. (f)                                         !5.14
! Shift WORD from 16-17                                          !5.14

            CALL GBYTE (MESG(WORD),BLOCK_2(12),OFF,8)            !5.14
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)  !5.14
            IF (ERROR.EQ.3) RETURN                               !5.14
 
! Scanning mode flags. (g)                                       !5.14
! Shift WORD from 16-17                                          !5.14

            CALL GBYTE (MESG(WORD),BLOCK_2(13),OFF,8)            !5.14
            CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)  !5.14
            IF (ERROR.EQ.3) RETURN                               !5.14
 
! Latin1 and Latin2.  (h)                                        !5.14
  
!            IF (BLOCK_2(3).EQ.1) THEN                           !5.14
              CALL GBYTES (MESG(WORD),BLOCK_2(14),OFF,24,0,2)    !5.14
              CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR) !5.14
              IF (ERROR.EQ.3) RETURN                             !5.14
 
! Lat/long of southern pole. (i)                                 !5.14
            
              CALL GBYTES (MESG(WORD),LAT(1),OFF,24,0,2)         !5.14
              CALL OFFSET (OFF,2,WORD,24,WORD_SIZE,LEN_MESG,ERROR) !5.14
              IF (ERROR.EQ.3) RETURN                             !5.14
 
! If sign bit set to 1 , values are negative.                    !5.14
  
              DO INDEX1 = 1 , 2                                  !5.14
                BLOCK_2(INDEX1+15) = LAT(INDEX1)                 !5.14
                IF (LAT(INDEX1).GT.8388608) THEN                 !5.14
                  BLOCK_2(INDEX1+15) = BLOCK_2(INDEX1+15) - 8388608 !5.14
                  BLOCK_2(INDEX1+15) = - BLOCK_2(INDEX1+15)      !5.14
                ENDIF                                            !5.14
              END DO                                             !5.14

! Skip last two octets                                            !5.14
              CALL OFFSET (OFF,2,WORD,8,WORD_SIZE,LEN_MESG,ERROR) !5.14
          ENDIF                                                   !5.14


    

!         Spherical harmonic data.
    
          IF (BLOCK_2(3).EQ.50) THEN
 
!           Pentagonal resolution parameters.
 
            CALL GBYTES (MESG(WORD),BLOCK_2(4),OFF,16,0,3)
            CALL OFFSET (OFF,3,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           Representation type and mode.
 
            CALL GBYTES (MESG(WORD),BLOCK_2(7),OFF,BYTE,0,2)
            CALL OFFSET (OFF,2,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
!           18 reserved octets.
 
            CALL OFFSET (OFF,18,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN
 
          ENDIF
    
!         Polar Stereographic.
    
          IF (BLOCK_2(3).EQ.5) THEN
 
!           Number of points along x and y axes.
 
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
 
          ENDIF
    
!         Vertical coordinate parameters for hybrid levels.
    
        
          IF (BLOCK_2(1).NE.0) THEN
          
            DO INDEX1 = 1 , BLOCK_2(1)
              CALL GBYTE (MESG(WORD),EXPO,OFF,8)
              CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN
              CALL GBYTE (MESG(WORD),MANTI,OFF,24)
              CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN
              CALL DECFP (VERT_COORDS(INDEX1),EXPO,MANTI)
            END DO  
    
          ENDIF
 
!         Quasi regular grid.
 
          IF (Q_INC.EQ.1) THEN
 
!           Check that the array is big enough.
 
            IF (LEN_Q.LT.NUM_Q) THEN
              ERROR = 3
              IF (MSGLVL .LT. 3) WRITE (ERR_U,9202) NUM_Q,LEN_Q
 9202         FORMAT (' ERROR - quasi regular grid has ', I9.9,
     &                ' elements. Array has ', I9.9, '.')
              RETURN
            ENDIF

            CALL GBYTES(MESG(WORD),QUASI(1),OFF,16,0,NUM_Q)
            CALL OFFSET(OFF,NUM_Q,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
            IF (ERROR .EQ. 3) RETURN

          ENDIF

        ENDIF             ! end of block 2 included.

!       RETURN if only product and grid definition blocks required.
 
        IF (LEN_FP.EQ.1) RETURN

      ENDIF               ! end of no limext section.

!     Intermediate calculations to cope with scanning mode of 32.

      IF (LIMEXT.EQ.0) THEN
        SCAN_POSN(1) = 1              ! Y coord of TLH corner
        SCAN_POSN(2) = 1              ! X coord of TLH corner
        SCAN_POSN(3) = BLOCK_2(5)     ! Y coord of BRH corner
        SCAN_POSN(4) = BLOCK_2(4)     ! X coord of BRH corner
      ENDIF

      ROW_LEN  = BLOCK_2(4)
      COL_LEN  = BLOCK_2(5)
      GPOFF    = ROW_LEN * (SCAN_POSN(2) - 1) + SCAN_POSN(1)
      TEMP_INT = BLOCK_2(13) / 32

      IF (MOD(TEMP_INT,2).EQ.1) THEN

!       Grid scans down columns rather than along rows.

        ROW_LEN = BLOCK_2(5)
        COL_LEN = BLOCK_2(4)

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

!     Check for quasi-regular grid and limited extract.

      IF (LIMEXT.NE.0) THEN
        IF (BLOCK_2(4).EQ.0.OR.BLOCK_2(5).EQ.0) THEN
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE ( UNIT = ERR_U, FMT = '(A)' )
     &      ' ERROR - cannot extract a limited area from a ' //
     &      'quasi-regular grid.'
          RETURN
        ENDIF
      ENDIF

!     **************************************************************
!
!                     BLOCK 3 - BIT MAP BLOCK.
!
!     **************************************************************

!     BLK3_INC is set to 1 , if block 3 is included.
 
      BLK3_INC = 0
 
!     BLOCKS INCLUDED    BINARY VALUE      DECIMAL VALUE
!
!         none             00000000               0
!          2               10000000             128
!          3               01000000              64
!      2 and 3             11000000             192
 
      IF (BLOCK_1(4).EQ.64.OR.BLOCK_1(4).EQ.192) BLK3_INC = 1
 
      IF (BLK3_INC.EQ.1) THEN
 
!       Fill bit-map array with 9's to force error if bit-map not read
 
        DO INDEX1 = 1 , LEN_BITMAP
          BITMAP(INDEX1) = 9
        END DO  

!       If doing a limited extract, restore WORD and OFF.

        IF (LIMEXT.NE.0) THEN
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
 
        IF( BMF.EQ.0 ) THEN
        
          NUM_BITMAP = (LEN_BK3 - 6) * 8 - NUB
 
!         Check that array is large enough for bit-map.
 
          IF (LEN_BITMAP.LT.NUM_BITMAP) THEN
            ERROR = 3
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9301) NUM_BITMAP,
     &        LEN_BITMAP
 9301       FORMAT (' ERROR - number of points in bitmap - ',
     &              I7, ', array size - ', I7)
            RETURN
          END IF
 
!         Get bitmap.
 
          CALL GBYTES (MESG(WORD),BITMAP(1),OFF,1,0,NUM_BITMAP)
          CALL OFFSET(OFF,NUM_BITMAP,WORD,1,WORD_SIZE,LEN_MESG,ERROR)
          IF (ERROR .EQ. 3) RETURN

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

          ENDIF                       ! end of limited extract section

!         Skip over unused bits
 
          CALL OFFSET(OFF,NUB,WORD,1,WORD_SIZE,LEN_MESG,ERROR)
 
        ELSE
 
!         Get predefined bitmap.
 
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE( UNIT = ERR_U, FMT = '(A)' )
     &      ' ERROR - predefined bitmap not available.'
          RETURN
        ENDIF
 
      ENDIF
 
!     ***************************************************************
!
!                   BLOCK 4 - BINARY DATA BLOCK.
!
!     ***************************************************************
 
!     Get length of binary data block.

!     If no bitmap, and limited area extract is being done, we need
!     to restore the values of WORD and OFFSET.

      IF (BLK3_INC.EQ.0.AND.LIMEXT.EQ.1) THEN
        WORD = LIM_WORD
        OFF  = LIM_OFF
      ENDIF

      TEMP_WORD = WORD
      TEMP_OFF = OFF
 
      CALL GBYTE (MESG(WORD),OCTS_BK4,OFF,24)
      CALL OFFSET (OFF,1,WORD,24,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
    
!     4 bit flag / 4 bit count of unused bits at end of block octet.
!   
!
!     BIT           VALUE               MEANING
!
!      1              0                 Grid-point data
!                     1                 Spherical harmonics
!      2              0                 Simple packing
!                     1                 Complex
!      3              0                 Floating-point data
!                     1                 Integer data
!      4              0                 No additional flags
!                     1                 Additional flags at o14
 
      CALL GBYTE (MESG(WORD),BLOCK_4(1),OFF,8)
      CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
      IF (ERROR .EQ. 3) RETURN
 
!     Shift flag to pick up number of unused bits.
 
      REP  = BLOCK_4(1) / 128
      FLAG = BLOCK_4(1) / 16
      UNUSED  = BLOCK_4(1) - (FLAG * 16)
 
!     Simple/complex packing.
 
      PAC = 0
      
      IF ((MOD(FLAG/4,2)).EQ.1) THEN
        PAC = 1
        IF (MSGLVL .LT. 1) WRITE( UNIT = ERR_U, FMT = '(A)' )
     &    ' NOTE - row by row packing used.'
      ENDIF
 
!     Floating point/integer data.
 
      INTEG = 0
      
      IF ((MOD(FLAG/2,2)).EQ.1) THEN
        INTEG = 1
        ERROR = 3
        IF (MSGLVL .LT. 3) WRITE(UNIT = ERR_U, FMT = '(A)' )
     &    ' ERROR - decoding of integer data not yet supported.'
        RETURN
      ENDIF
 
!     Flag continued in o14 - not until complex packing.
 
      CONT = 0
      
      IF ((MOD(FLAG,2)).EQ.1) THEN
        CONT = 1
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
        IF (MSGLVL .LT. 3) WRITE (ERR_U,9401) WIDTH,
     &    WORD_SIZE,Max_Bits
 9401   FORMAT (' ERROR - number of bits per data value, ', I3,
     &          ' exceeds word length, ', I3, ' or maximum ',
     &          ' permitted value, ', I3)
        RETURN
      ENDIF


      IF (WIDTH.EQ.0) THEN                                          !5.14
        ERROR=3                                                     !5.14
        IF (MSGLVL.LT.3) WRITE (ERR_U,9405) WIDTH                   !5.14
 9405   FORMAT (' ERROR - number of bits per data value, ',I3,      !6.0
     &          ' Data cannot be decoded')                          !5.14
        RETURN                                                      !5.14
      ENDIF                                                         !5.14
      
    
!     If data is in spherical harmonic form, next 4 octets
!     contain the real (0,0) coefficient.
    
      IF (REP.EQ.1) THEN
 
        IF (PAC.EQ.1) THEN
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE( UNIT = ERR_U, FMT = '(A)' )
     &      ' ERROR - decoding of spherical harmonics encoded ' //
     &      'using 2nd order packing not supported.'
          RETURN
        ENDIF
 
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
 
!     If values are to be converted to SI units, divide by 10**D.
 
      CONVERT = 1.0
      
      IF (BLOCK_4(2).NE.0) THEN
        IF (MSGLVL .LT. 1) WRITE( UNIT = ERR_U, FMT = '(A)' )
     &    ' NOTE - decoded values converted to SI units.'
        CONVERT = 10.0 ** BLOCK_1(20)
      ENDIF
    
!     Decode data values to floating point and store in FP_DATA.
    
!---------------------------------------------------
!     Start of simple packed data decoding section.
!---------------------------------------------------
 
      IF (PAC.EQ.0) THEN
 
!       Calculate the number of data values.

        IF (LIMEXT.EQ.0) THEN
          NUM_FP = OCTS_BK4 - 11 - REP * 4
          NUM_FP = (NUM_FP * 8 - UNUSED) / WIDTH
        ELSE
          NUM_FP = LIM_COLS * LIM_ROWS
        ENDIF
 
        IF (BLK3_INC.EQ.0) THEN        ! start of no bitmap section.
 
!         Flag will not be continued in o14.
 
          BLOCK_4(1) = FLAG * 16
 
!         Check length of output array.
 
          IF (NUM_FP+REP.GT.LEN_FP) THEN
            ERROR = 3
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9402) NUM_FP,LEN_FP
 9402       FORMAT (' ERROR - number of values to be decoded is - ',
     &              I7, ', array size - ', I7)
            RETURN
          ENDIF

!         If LIMEXT is not 0, need to unpack limited area.

          IF (LIMEXT.EQ.0) THEN
	  
# if defined (HP) || defined (CRAY) || defined (SX6)            /*6.3*/
            DO I=1,NUM_FP                                        !6.0
              IN_DATA(I) = TRANSFER (FP_DATA(REP+I),0)           !6.0
	    ENDDO                                                !6.0
	    CALL GBYTES (MESG(WORD),IN_DATA,OFF,WIDTH,0,         !6.0
     &                   NUM_FP)                                 !6.0
            DO I=1,NUM_FP                                        !6.0
              FP_DATA(REP+I) = TRANSFER (IN_DATA(I),0.0)         !6.0
	    ENDDO                                                !6.0
# else
            CALL GBYTES (MESG(WORD),FP_DATA(REP+1),OFF,WIDTH,0,
     &                   NUM_FP)
# endif

            CALL OFFSET (OFF,NUM_FP,WORD,WIDTH,WORD_SIZE,LEN_MESG,
     &                   ERROR)
            IF (ERROR .EQ. 3) RETURN
          ELSE
            OFF = OFF + (GPOFF - 1) * WIDTH

            DO INDEX1 = 1 , LIM_ROWS
    
# if defined (HP) || defined (CRAY) || defined (SX6)             /*6.3*/
              DO I=1,LIM_COLS                                       !6.0
                IN_DATA(I) =                                        !6.0
     &              TRANSFER (FP_DATA(REP+(INDEX1-1)*LIM_COLS+I),0) !6.0
	      ENDDO                                                 !6.0
              CALL GBYTES (MESG(WORD),IN_DATA,
     &                     OFF,WIDTH,0,LIM_COLS)
              DO I=1,LIM_COLS                                       !6.0
                FP_DATA(REP+(INDEX1-1)*LIM_COLS+I)=                 !6.0
     &               TRANSFER (IN_DATA(I),0.0)                      !6.0
	      ENDDO                                                 !6.0
# else
              CALL GBYTES (MESG(WORD),FP_DATA(REP+(INDEX1-1)
     &                     *LIM_COLS+1),OFF,WIDTH,0,LIM_COLS)
# endif

              CALL OFFSET (OFF,LIM_COLS,WORD,WIDTH,WORD_SIZE,LEN_MESG,
     &                     ERROR)
              IF (ERROR .EQ. 3) RETURN

              OFF = OFF + (ROW_LEN - LIM_COLS) * WIDTH
            END DO

          ENDIF                    ! no need to correct offset.
 
!         Scale and store in FP_DATA
 
          DO INDEX1 = REP+1 , REP+NUM_FP
            ZGRIB     = FP_DATA (INDEX1)
            FP_DATA(INDEX1) = (DATA_MIN + JGRIB * REAL_SCALE) 
     &                        / CONVERT
          END DO  
 
        ENDIF                   ! end of no bitmap section.
 
        IF (BLK3_INC.EQ.1) THEN      ! start of bitmap section.
 
!         Check length of output array.

          IF (LIMEXT.EQ.0) THEN
            NUM_FP = OCTS_BK4 - 11 - REP * 4
            NUM_FP = (NUM_FP * 8 - UNUSED) / WIDTH
          ELSE
            NUM_FP = 0

            DO INDEX1 = 2 , LEN_DATAPTN , 2
              NUM_FP = NUM_FP + DATA_PTN(INDEX1)
            END DO
          ENDIF

          IF ((NUM_FP+REP).GT.LEN_FP) THEN
            ERROR = 3
            IF (MSGLVL .LT. 3) WRITE (ERR_U,9402) NUM_FP,LEN_FP
            RETURN
          ENDIF
 
!         Calculate missing data indicator
 
          MAX_INT    = (2 ** WIDTH)
          MAX_REAL   = (DATA_MIN + MAX_INT * REAL_SCALE) / CONVERT
          BLOCK_R(2) = MAX_REAL
          BLOCK_3(1) = NINT ( MAX_REAL + .5 )
          
          IF (MSGLVL .LT. 1) WRITE (ERR_U,9403) BLOCK_R(2)
 9403     FORMAT(' NOTE - bitmap used. Missing data indicator is ',
     &           F12.4)

!         If a limited area extract, get chunks of data.

          IF (LIMEXT.EQ.0) THEN 

# if defined (HP) || defined (CRAY) || defined (SX6)           /*6.3*/
            DO I=1,NUM_FP                                       !6.0
              IN_DATA(I) = TRANSFER (FP_WORK(REP+I),0)          !6.0
	    ENDDO                                               !6.0
            CALL GBYTES (MESG(WORD),IN_DATA,OFF,WIDTH,0,NUM_FP) !6.0
            DO I=1,NUM_FP                                       !6.0
	      FP_WORK(REP+I) = TRANSFER(IN_DATA(I),0.0)         !6.0
	    ENDDO                                               !6.0
# else
            CALL GBYTES (MESG(WORD),FP_WORK(REP+1),OFF,WIDTH,0,NUM_FP)
# endif

            CALL OFFSET (OFF,NUM_FP,WORD,WIDTH,WORD_SIZE,LEN_MESG,
     &                   ERROR)
            IF (ERROR .EQ. 3) RETURN
          ELSE
            TEMP_INT = 1

            DO INDEX1 = 1 , LEN_DATAPTN , 2

!             Skip DATA_PTN(INDEX1) * WIDTH.

              CALL OFFSET (OFF,DATA_PTN(INDEX1),WORD,WIDTH,
     &                     WORD_SIZE,LEN_MESG,ERROR)

!             Unpack DATA_PTN(INDEX1 + 1).

# if defined (HP) || defined (CRAY) || defined (SX6)       /*6.3*/
              DO I=1,DATA_PTN(INDEX1+1)                     !6.0
                IN_DATA(I) =                                !6.0
     &            TRANSFER (FP_WORK(REP+TEMP_INT+I-1),0)    !6.0
	      ENDDO                                         !6.0
              CALL GBYTES (MESG(WORD),IN_DATA,              !6.0
     &                     OFF,WIDTH,0,DATA_PTN(INDEX1+1))  !6.0
              DO I=1,DATA_PTN(INDEX1+1)                     !6.0
                FP_WORK(REP+TEMP_INT+I-1)=                  !6.0
     &                 TRANSFER(IN_DATA(I),0.0)             !6.0
              ENDDO                                         !6.0
# else
              CALL GBYTES (MESG(WORD),FP_WORK(REP+TEMP_INT),
     &                     OFF,WIDTH,0,DATA_PTN(INDEX1+1))
# endif

              CALL OFFSET (OFF,DATA_PTN(INDEX1+1),WORD,WIDTH,
     &                     WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN

              TEMP_INT = TEMP_INT + DATA_PTN(INDEX1 + 1)

            END DO

          ENDIF

          DATA_PT = 1

!         If normal extract, scan through whole bitmap.

          IF (LIMEXT.EQ.0) THEN
            DO INDEX1 = REP+1 , REP+NUM_BITMAP
 
              IF (BITMAP(INDEX1).NE.0.AND.BITMAP(INDEX1).NE.1) THEN
                ERROR = 3
                IF (MSGLVL .LT. 3) WRITE (ERR_U,9404) INDEX1,
     &            BITMAP(INDEX1)
 9404           FORMAT(' ERROR - bitmap element (', I9.9, ') = ', I2,
     &                 ' - invalid.')
                RETURN
              ENDIF
 
              IF (BITMAP(INDEX1).EQ.0) THEN
                FP_DATA(INDEX1) = BLOCK_R(2)
              ENDIF
 
              IF (BITMAP(INDEX1).EQ.1) THEN
                ZGRIB           = FP_WORK(DATA_PT)
                FP_DATA(INDEX1) = (DATA_MIN + JGRIB * REAL_SCALE)
     &                            / CONVERT
                DATA_PT         = DATA_PT + 1
              ENDIF
 
            END DO  
          ENDIF

!         If a limited area extract, scan through relevant section
!         of bitmap only.

          IF (LIMEXT.NE.0) THEN

            COUNT = 1

            DO INDEX1 = GPOFF , (SCAN_POSN(4) - 1) * ROW_LEN
     &                        + SCAN_POSN(3)

              IF (BITMAP(INDEX1).NE.0.AND.BITMAP(INDEX1).NE.1) THEN
                ERROR = 3
                IF (MSGLVL .LT. 3) WRITE (ERR_U,9404) INDEX1,
     &            BITMAP(INDEX1)
                RETURN
              ENDIF

              COL_COUNT = MOD(INDEX1,ROW_LEN)      ! check X position.
              IF (COL_COUNT.EQ.0) COL_COUNT = ROW_LEN

              IF (COL_COUNT.GE.SCAN_POSN(1).AND.   ! inside the box.
     &            COL_COUNT.LE.SCAN_POSN(3)) THEN

                IF (BITMAP(INDEX1).EQ.0) THEN
                  FP_DATA(COUNT) = BLOCK_R(2)
                ENDIF

                IF (BITMAP(INDEX1).EQ.1) THEN
                  ZGRIB = FP_WORK(DATA_PT)
                  FP_DATA(COUNT) = (DATA_MIN + JGRIB * REAL_SCALE)
     &                             / CONVERT
                  DATA_PT = DATA_PT + 1
                ENDIF

                COUNT = COUNT + 1

              ENDIF

            END DO

            NUM_BITMAP = LIM_COLS * LIM_ROWS

          ENDIF                             ! end of limited extract.

!         Include missing values in count of data.
 
          NUM_FP = NUM_BITMAP

        ENDIF
 
      ENDIF

!---------------------------------------------------
!     Start of 2nd order packed decoding section.
!---------------------------------------------------
 
      IF (PAC.EQ.1) THEN

        IF (Q_INC.EQ.0) THEN
          NUM_ROWS = COL_LEN
          NUM_COLS = ROW_LEN
        ELSE
          NUM_ROWS = COL_LEN
          IF (NUM_ROWS.EQ.0) NUM_ROWS = ROW_LEN
        ENDIF
 
!       Get N1, octet number at which 1st order values start.
 
        CALL GBYTE (MESG(WORD),N1,OFF,16)
        CALL OFFSET (OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Get extended flags.
 
        CALL GBYTE (MESG(WORD),FLAG2,OFF,8)
        CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Combine flags into one octet.
 
        IF (CONT.EQ.1) THEN
          BLOCK_4(1) = FLAG * 16 + (FLAG2/16)
        ENDIF
 
!       Get N2, octet number at which 2nd order values start.
 
        CALL GBYTE (MESG(WORD),N2,OFF,16)
        CALL OFFSET (OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Get P1, number of 1st order packed values.
 
        CALL GBYTE (MESG(WORD),P1,OFF,16)
        CALL OFFSET (OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Get P2, number of 2nd order packed values.
 
        CALL GBYTE (MESG(WORD),P2,OFF,16)
        CALL OFFSET (OFF,1,WORD,16,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Next octet is reserved.
 
        CALL OFFSET (OFF,1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Extract the widths.
 
        CALL GBYTES (MESG(WORD),WORK_INT2(NUM_ROWS+1),OFF,8,0,P1)
        CALL OFFSET (OFF,P1,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
 
!       Calculate number of words for 1st order data - read P1 lots
!       of width WIDTH, but skip a whole number of words (sneaky!) .
 
        OCT_1ST = N2 - N1
 
        CALL GBYTES (MESG(WORD),WORK_INT2(1),OFF,WIDTH,0,P1)
        CALL OFFSET (OFF,OCT_1ST,WORD,8,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN

!       If doing a limited area extract, num_fp is smaller.

        IF (LIMEXT.EQ.0) THEN
          NUM_FP = P2
        ELSE
          NUM_FP = LIM_ROWS * LIM_COLS
        ENDIF
 
!       Check length of output array.
 
        IF (NUM_FP+REP.GT.LEN_FP) THEN
          ERROR = 3
          IF (MSGLVL .LT. 3) WRITE (ERR_U,9402) NUM_FP,LEN_FP
          RETURN
        ENDIF

!---------------------------
!       No bit-map.
!---------------------------
        
        IF (BLK3_INC.EQ.0) THEN
 
!         Extract 2nd order data row by row.
 
          DATA_PT = 0           ! counter to position in data.

!         If doing a limited extract, loop over lim_rows.

          IF (LIMEXT.EQ.0) THEN
            TEMP_ROWS    = P1
            SCAN_POSN(1) = 1
            SCAN_POSN(4) = TEMP_ROWS
          ELSE
            TEMP_ROWS    = SCAN_POSN(4) - SCAN_POSN(2) + 1

!           Need to shift along to the beginning of the first limited 
!           area row.

            DO INDEX2 = 1 , SCAN_POSN(2) - 1
              CALL OFFSET(OFF,NUM_COLS,WORD,
     &          WORK_INT2(NUM_ROWS+INDEX2),WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN
            END DO
          ENDIF

!         Same loop for normal or limited area extract.

          DO INDEX1 = SCAN_POSN(2) , SCAN_POSN(4)

            IF (Q_INC.EQ.1) NUM_COLS = QUASI(INDEX1)

            IF (LIMEXT.EQ.0) THEN
	    
# if defined (HP) || defined (CRAY) || defined (SX6)             /*6.3*/
              DO I=1,NUM_COLS                                     !6.0
                IN_DATA(I)=TRANSFER(WORK_RE1(I),0)                !6.0
	      ENDDO                                               !6.0
              CALL GBYTES (MESG(WORD),IN_DATA,OFF,                !6.0
     &                     WORK_INT2(NUM_ROWS+INDEX1),0,NUM_COLS) !6.0
              DO I=1,NUM_COLS                                     !6.0
                WORK_RE1(I)=TRANSFER(IN_DATA(I),0.0)              !6.0
	      ENDDO                                               !6.0
# else
              CALL GBYTES (MESG(WORD),WORK_RE1(1),OFF,
     &                     WORK_INT2(NUM_ROWS+INDEX1),0,NUM_COLS)
# endif
              CALL OFFSET (OFF,NUM_COLS,WORD,
     &          WORK_INT2(NUM_ROWS+INDEX1),WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN
            ELSE
              
!             Skip along to start of limited area row.

              CALL OFFSET(OFF,SCAN_POSN(1)-1,WORD,WORK_INT2(NUM_ROWS+
     &                    INDEX1),WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN

!             Get limited area rows-worth of data.

# if defined (HP) || defined (CRAY) || defined (SX6)             /*6.3*/
              DO I=1,LIM_COLS                                     !6.0
                IN_DATA(I)=TRANSFER(WORK_RE1(I),0)                !6.0
	      ENDDO                                               !6.0
              CALL GBYTES(MESG(WORD),IN_DATA,OFF,                 !6.0
     &                    WORK_INT2(NUM_ROWS+INDEX1),0,LIM_COLS)  !6.0
              DO I=1,LIM_COLS                                     !6.0
                WORK_RE1(I)=TRANSFER(IN_DATA(I),0.0)              !6.0
	      ENDDO                                               !6.0
# else
              CALL GBYTES(MESG(WORD),WORK_RE1(1),OFF,
     &                    WORK_INT2(NUM_ROWS+INDEX1),0,LIM_COLS)
# endif

              CALL OFFSET(OFF,LIM_COLS,WORD,
     &          WORK_INT2(NUM_ROWS+INDEX1),WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN

!             And skip any values to the right of the area.

              CALL OFFSET (OFF,NUM_COLS-SCAN_POSN(3),WORD,WORK_INT2(
     &                     NUM_ROWS+INDEX1),WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN

            ENDIF
 
!           Scale and store.

            IF (LIMEXT.EQ.0) THEN
              TEMP_COLS = NUM_COLS
            ELSE
              TEMP_COLS = LIM_COLS
            ENDIF

            DO INDEX2 = 1 , TEMP_COLS
              ZGRIB            = WORK_RE1(INDEX2)
              JGRIB            = JGRIB + WORK_INT2(INDEX1)
              FP_DATA(INDEX2+DATA_PT) = (DATA_MIN + JGRIB*REAL_SCALE)
     &                                  / CONVERT
            END DO  
 
!           Need to keep a count of the number of values
!           extracted in case we have a quasi regular grid.
 
            DATA_PT = DATA_PT + TEMP_COLS
 
          END DO  
 
        ENDIF

!-----------------------------
!       Bit-map present.
!-----------------------------
 
        IF (BLK3_INC.EQ.1) THEN
 
!         NUM_FP = length of bit-map if normal extract.

          IF (LIMEXT.EQ.0) THEN
            NUM_FP = NUM_BITMAP
          ELSE

!           Count up how many points are present in limited area.

            NUM_FP = 0

            DO INDEX1 = 2 , LEN_DATAPTN , 2
              NUM_FP = NUM_FP + DATA_PTN(INDEX1)
            END DO

          ENDIF
 
!         Calculate missing data indicator
 
          MAX_INT    = (2 ** WIDTH)
          MAX_REAL   = (DATA_MIN + MAX_INT * REAL_SCALE) 
     &                 / CONVERT
          BLOCK_R(2) = MAX_REAL   
          BLOCK_3(1) = NINT ( MAX_REAL + .5 )
       
          IF (MSGLVL .LT. 1) WRITE (ERR_U,9403) BLOCK_R(2)
 
!         Extract 2nd order data row by row.
 
          DATA_PT = 0

!         If doing a full extract, step through row by row.

          IF (LIMEXT.EQ.0) THEN

            DO INDEX1 = 1 , P1

              IF (Q_INC.EQ.1) NUM_COLS = QUASI(INDEX1)
 
!             Find out how many second order values to extract.
 
              ROW_PT = 0
 
              DO INDEX2 = 1 , NUM_COLS
                IF (BITMAP(INDEX2+DATA_PT).EQ.1) ROW_PT = ROW_PT + 1
              END DO  
 
# if defined (HP) || defined (CRAY) || defined (SX6)           /*6.3*/ 
              DO I=1,ROW_PT                                     !6.0
                IN_DATA(I)=TRANSFER(WORK_RE1(I),0)              !6.0
	      ENDDO                                             !6.0
              CALL GBYTES (MESG(WORD),IN_DATA,OFF,              !6.0
     &                     WORK_INT2(NUM_ROWS+INDEX1),0,ROW_PT) !6.0
              DO I=1,ROW_PT                                     !6.0
                WORK_RE1(I)=TRANSFER(IN_DATA(I),0.0)                 !6.0
	      ENDDO                                             !6.0
# else
              CALL GBYTES (MESG(WORD),WORK_RE1(1),OFF,
     &                     WORK_INT2(NUM_ROWS+INDEX1),0,ROW_PT)
# endif

              CALL OFFSET (OFF,ROW_PT,WORD,WORK_INT2(NUM_ROWS+INDEX1),
     &                     WORD_SIZE,LEN_MESG,ERROR)
              IF (ERROR .EQ. 3) RETURN
 
!             Scale and store.
 
              ROW_POS = 1

              DO INDEX2 = 1 , NUM_COLS
 
                IF (BITMAP(INDEX2+DATA_PT).NE.0.AND.
     &              BITMAP(INDEX2+DATA_PT).NE.1) THEN
                  ERROR = 3
                  RETURN
                ENDIF
 
                IF (BITMAP(INDEX2+DATA_PT).EQ.0) THEN
                  FP_DATA(INDEX2+DATA_PT) = BLOCK_R(2)
                ENDIF
 
                IF (BITMAP(INDEX2+DATA_PT).EQ.1) THEN
                  ZGRIB            = WORK_RE1(ROW_POS)
                  JGRIB            = JGRIB + WORK_INT2(INDEX1)
                  FP_DATA(INDEX2+DATA_PT) = (DATA_MIN + JGRIB
     &                             * REAL_SCALE) / CONVERT
                  ROW_POS          = ROW_POS + 1
                ENDIF
 
              END DO  
 
!             Keep a count of the number of data points, in case
!             we have a quasi regular grid.
 
              DATA_PT = DATA_PT + NUM_COLS
 
            END DO  
          ELSE

!           Things are much more complicated for a limited extract..!
!           First, set up some pointers.

            COUNTER1 = 1     ! points to next skip value in scan_ptn
            COUNTER2 = 2     ! points to next unpack value in scan_ptn

            SKIP    = DATA_PTN(COUNTER1)  ! number of values to skip.
            UNPK    = DATA_PTN(COUNTER2)  ! number of values to unpack

            SKIPPING = 1                  ! 0 if we're unpacking.
            LIM_DATA = 0
            DATA_PT  = 0

!           Loop over rows until the last row in the limited area.

            DO INDEX1 = 1 , SCAN_POSN(4)

!             Check whether we're in the limited area.

              IF (INDEX1.GE.SCAN_POSN(2)) IN = 1

!             Count up the number of data values in this row.

              ROW_PT = 0

              TEMP_INT = (INDEX1-1) * NUM_COLS

              DO INDEX2 = 1 , NUM_COLS
                IF (BITMAP(INDEX2+TEMP_INT).EQ.1) ROW_PT = ROW_PT + 1
              END DO  

!             First we check for values to skip.

              IF (SKIP.EQ.0) SKIPPING = 0

              IF (SKIPPING.EQ.1) THEN
                TEMP_SKIP = SKIP
                IF (TEMP_SKIP.GT.ROW_PT) TEMP_SKIP = ROW_PT

                CALL OFFSET (OFF,TEMP_SKIP,WORD,WORK_INT2(NUM_ROWS+
     &                       INDEX1),WORD_SIZE,LEN_MESG,ERROR)
                IF (ERROR .EQ. 3) RETURN

                SKIP    = SKIP - TEMP_SKIP
                ROW_PT  = ROW_PT - TEMP_SKIP ! if 0, finished this row
              ENDIF

!             Update. (I konw this should all be in a subroutine,
!             but it would be too slow!
!             Only update if there is more to skip on this row.

              IF (SKIP.LE.0.AND.ROW_PT.GT.0) THEN 
                COUNTER1 = COUNTER1 + 2
                SKIP     = DATA_PTN(COUNTER1)
                SKIPPING = 0
              ENDIF

!             Check to see if there are any values to unpack yet.

              IF (SKIPPING.EQ.0.AND.UNPK.LE.0.AND.ROW_PT.GT.0) THEN
                COUNTER2 = COUNTER2 + 2
                UNPK     = DATA_PTN(COUNTER2)
                SKIPPING = 1
              ENDIF

!             May actually be able to unpack something now ..!

              IF (SKIPPING.EQ.0.AND.ROW_PT.GT.0) THEN
                TEMP_UNPK = UNPK
                IF (TEMP_UNPK.GT.ROW_PT) TEMP_UNPK = ROW_PT
		
# if defined (HP) || defined (CRAY) || defined (SX6)                /*6.3*/
                DO I=1,TEMP_UNPK                                     !6.0
                  IN_DATA(I)=TRANSFER(WORK_RE1(I),0)                 !6.0
		ENDDO                                                !6.0
                CALL GBYTES (MESG(WORD),IN_DATA,OFF,                 !6.0
     &                       WORK_INT2(NUM_ROWS+INDEX1),0,TEMP_UNPK) !6.0
                DO I=1,TEMP_UNPK                                     !6.0
                  WORK_RE1(I)=TRANSFER(IN_DATA(I),0.0)               !6.0
		ENDDO                                                !6.0
# else
                CALL GBYTES (MESG(WORD),WORK_RE1(1),OFF,
     &                       WORK_INT2(NUM_ROWS+INDEX1),0,TEMP_UNPK)
# endif

                CALL OFFSET (OFF,TEMP_UNPK,WORD,WORK_INT2(NUM_ROWS+
     &                       INDEX1),WORD_SIZE,LEN_MESG,ERROR)
                IF (ERROR .EQ. 3) RETURN

                UNPK    = UNPK - TEMP_UNPK
                ROW_PT  = ROW_PT - TEMP_UNPK

              ENDIF

!             Scale and store.
 
              LIM_DATA = 0
              ROW_POS  = 1

              DO INDEX2 = 1 , LIM_COLS

                TEMP_INT = (INDEX1-1) * NUM_COLS

                IF (BITMAP(INDEX2+TEMP_INT+SCAN_POSN(2)-1).NE.0.AND.
     &              BITMAP(INDEX2+TEMP_INT+SCAN_POSN(2)-1).NE.1) THEN
                  ERROR = 3
                  RETURN
                ENDIF
 
                IF (BITMAP(INDEX2+TEMP_INT+SCAN_POSN(2)-1).EQ.0) THEN
                  FP_DATA(INDEX2+DATA_PT) = BLOCK_R(2)
                ELSE
                  ZGRIB   = WORK_RE1(ROW_POS)
                  JGRIB   = JGRIB + WORK_INT2(INDEX1)
                  FP_DATA(INDEX2+DATA_PT) = (DATA_MIN +
     &              JGRIB * REAL_SCALE) / CONVERT
                  LIM_DATA = LIM_DATA + 1
                  ROW_POS  = ROW_POS + 1
                ENDIF
 
              END DO  
 
!             Update again!

              IF (UNPK.LE.0) THEN
                COUNTER2 = COUNTER2 + 2
                UNPK     = DATA_PTN(COUNTER2)
                SKIPPING = 1
              ENDIF

!             Final check - may need to skip tail-end of row.

              IF (SKIPPING.EQ.1.AND.ROW_PT.GT.0) THEN
                TEMP_SKIP = SKIP
                IF (TEMP_SKIP.GT.ROW_PT) TEMP_SKIP = ROW_PT

                CALL OFFSET (OFF,TEMP_SKIP,WORD,WORK_INT2(NUM_ROWS+
     &                       INDEX1),WORD_SIZE,LEN_MESG,ERROR)
                IF (ERROR .EQ. 3) RETURN

                SKIP    = SKIP - TEMP_SKIP
              ENDIF

!             Update.

              IF (SKIP.LE.0) THEN
                COUNTER1 = COUNTER1 + 2
                SKIP     = DATA_PTN(COUNTER1)
                SKIPPING = 0
              ENDIF

!             Add on a row, if we're inside the limited area.

              IF (IN.EQ.1) DATA_PT = DATA_PT + LIM_COLS

            END DO

            NUM_FP = LIM_ROWS * LIM_COLS  ! give NUM_FP sensible value

          ENDIF            ! end of limited extract if-block.

        ENDIF              ! end of BLK3_INC section.
 
      ENDIF                ! end of complex packing block.

!--------------------------------------
!     End of complex packing section.
!--------------------------------------

!     Include (0,0) coefficient in count, if spherical harmonic data.
 
      NUM_FP = REP + NUM_FP
    
!     Skip any zero fill at end of data block.
    
      ZERO_BITS = (WORD-TEMP_WORD) * WORD_SIZE + OFF -TEMP_OFF
      ZERO_BITS = OCTS_BK4 * 8 - ZERO_BITS 
   
      IF (ZERO_BITS.NE.0) THEN
        CALL OFFSET (OFF,1,WORD,ZERO_BITS,WORD_SIZE,LEN_MESG,ERROR)
        IF (ERROR .EQ. 3) RETURN
      ENDIF
 
!     ***************************************************************
!
!                     BLOCK 5 - END BLOCK.
!
!     ***************************************************************

!     Check 7 7 7 7 at end of coded data.
 
      CALL GBYTES (MESG(WORD),STORE(1),OFF,8,0,4)
 
!     55 is the value of ascii character '7'
 
      DO INDEX1 = 1 , 4
        IF (STORE(INDEX1).NE.55) ERROR = 3
      END DO  
 
      IF (ERROR .EQ. 3 .AND. MSGLVL .LT. 3) WRITE (UNIT = ERR_U,
     &  FMT = '(A)' ) ' ERROR - no 7777 group found.'
 
 
      RETURN
      END ! SUBROUTINE Grib_Edtn_1_Dec
