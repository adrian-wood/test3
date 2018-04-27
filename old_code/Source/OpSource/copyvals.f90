SUBROUTINE COPYVALS (IDISP, WANTOB, NDISP, INTELM, &
                     VALUES1, CSTR1, NOBS1, NOB1, &
                     VALUES2, CSTR2, NOBS2, NOB2, ICODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  COPYVALS
!
!    To extract requested data values from a decoded BUFR message and
!    return them in user-supplied arrays.
!
! DESCRIPTION:
!
!    COPYVALS extracts required data from the output of a BUFR decode
!    (real values in VALUES1, character strings in CSTR1) and puts it
!    in arrays supplied by the user (real values in VALUES2, character
!    data in CSTR2). Data items are specified by the IDISP array as
!    output by a call to ELEMENTS and WANTOB defines which observations
!    in VALUES1 are required. INTELM is an input array of items not
!    obtained from BUFR section 4 (time of receipt, data selection
!    code, originating subcentre): this may or may not be used.
!
!    NOBS1 and NOBS2 are the maximum number of observations that arrays
!    VALUES1 and VALUES2 can hold.  NOB1 should be input as the number
!    of the last observation in VALUES1 whose data has been transferred
!    and NOB2 should be specified as the actual number of observations
!    in VALUES2. Both of these variables are updated on output. Data
!    transfer stops when VALUES2 has been filled or when VALUES1 has
!    been exhausted.
!
!    An error message and non-zero error code are generated if array
!    CSTR2 is not big enough to hold all the requested data. Control
!    then immediately returns to the calling program.
!
! USAGE:  CALL COPYVALS (IDISP, WANTOB, NDISP, INTELM,
!                        VALUES1, CSTR1, NOBS1, NOB1,
!                        VALUES2, CSTR2, NOBS2, NOB2, ICODE)
! ARGUMENTS:
!
!    Name   I/O  Type  Size    Description
!    ----   ---  ----  ----    -----------
!    IDISP   I   I*4  NDISP  Array of displacements for VALUES1
!    WANTOB  I   L*1    *    Array of 'wanted' flags for observations
!    NDISP   I   I*4         Dimension of IDISP (= data values per ob.)
!    INTELM  I   I*4    *    Additional data for transfer (TOR etc.)
!    VALUES1 I   R*4    *    (Array dimensioned (NOBS1,*))  Array of
!                               data values from decoded BUFR message
!    CSTR1   I  C*(*)        Character data from decoded BUFR message
!    NOBS1   I   I*4         First dimension of VALUES1 array
!    NOB1   I/O  I*4         Last observation used in VALUES1 array
!    VALUES2 O   R*4    *    (Array dimensioned (NOBS2,*))  User's
!                               output array of data values
!    CSTR2   O  C*(*) NOBS2  User's output string of character data
!    NOBS2   I   I*4         First dimension of VALUES2 array
!    NOB2   I/O  I*4         Number of observations in VALUES2 array
!    ICODE   O   I*4         Return code (see below)
!
! RETURN CODES:
!
!      0  Data values copied to user's array with no errors.
!    211  Output array for character data (CSTR2) is too small.
!
! CALLS:  COPYVALS does not call any other routines.
!
! REVISION INFO:
!
!    $Workfile: copyvals.f90$ $Folder: OpSource$
!    $Revision: 2$ $Date: 18/10/2010 15:18:22$
!
! CHANGE RECORD:
!
! $Log:
!  2    MetDB_Refresh 1.1         18/10/2010 15:18:22    John Norton
!       Updated after review by Brian.
!  1    MetDB_Refresh 1.0         13/10/2010 16:46:17    John Norton     Ported
!        f90 code
! $
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

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

INTEGER, INTENT(IN)             ::  NDISP ! Number of data elements per ob.
INTEGER, INTENT(IN)             ::  IDISP(NDISP) ! Array of displacements for VALUES1
LOGICAL(kind=1), INTENT(IN)     ::  WANTOB(*)
INTEGER, INTENT(IN)             ::  INTELM(*) ! Additional data for transfer (TOR etc.)
INTEGER, INTENT(INOUT)          ::  NOBS1 ! Max. number of obs in VALUES1 array
REAL, INTENT(IN)                ::  VALUES1(NOBS1,*) ! Data values from BUFR decode
CHARACTER(*), INTENT(IN)        ::  CSTR1
INTEGER, INTENT(INOUT)          ::  NOB1 ! Actual number of obs in VALUES1 array
INTEGER, INTENT(IN)             ::  NOBS2 ! Max. number of obs in VALUES2 array
REAL, INTENT(OUT)               ::  VALUES2(NOBS2,*) ! User's output array of data values
CHARACTER(*), INTENT(OUT)       ::  CSTR2(NOBS2)
INTEGER, INTENT(INOUT)          ::  NOB2 ! Actual number of obs in VALUES2 array
INTEGER, INTENT(OUT)            ::  ICODE ! Return code

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  I      ! Integer variable for local use
INTEGER      ::  ICH    ! Pointer to byte in CSTR2 string
INTEGER      ::  IEND1  ! Pointer to end of substring in CSTR1
INTEGER      ::  IEND2  ! Pointer to end of substring in CSTR2
INTEGER      ::  ILEN   ! Length of character substring
INTEGER      ::  IND    ! Type indicator for negative IDISP
INTEGER      ::  IOB1   ! Counter for of obs in VALUES1 array
INTEGER      ::  IOB2   ! Counter for of obs in VALUES2 array
INTEGER      ::  ISTART1 ! Pointer to start of substring in CSTR1
INTEGER      ::  ISTART2 ! Pointer to start of substring in CSTR2
INTEGER      ::  IVAL   ! Value indicator for negative IDISP
INTEGER      ::  J      ! Loop variable for loop over elements
INTEGER      ::  LCSTR2 ! Length of CSTR2 string

REAL         ::  RMDI   ! Real missing data indicator

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

DATA RMDI /-9999999.0/
!                                                       Initialisations
ICODE = 0               ! No problems yet
LCSTR2 = LEN(CSTR2(1))  ! Length of CSTR2 elements

!=======================================================================
!     LOOP OVER OBSERVATUONS TO BE TRANSFERRED
!=======================================================================
!                                              Initialisations for loop
IOB1 = NOB1
IOB2 = NOB2
!                                                         Loop over obs
DOLABEL1: &
DO WHILE (IOB1 < NOBS1 .AND. IOB2 < NOBS2)
  IOB1 = IOB1 + 1
IFLABEL1: &
  IF (WANTOB(IOB1)) THEN

!=======================================================================
!         LOOP OVER ELEMENTS IN USER'S REQUEST STRING
!=======================================================================
!                                              Initialisations for loop
    IOB2 = IOB2 + 1  ! Next output ob
    ISTART2 = 1      ! Start at CSTR2 byte 1
!                                                    Loop over elements
DOLABEL2: &
    DO J=1,NDISP

!-----------------------------------------------------------------------
!  (1)  Displacement > 0:  Subscript of VALUES1 array
!-----------------------------------------------------------------------

IFLABEL2: &
      IF (IDISP(J) > 0) THEN            ! VALUES1 subscript
        VALUES2(IOB2,J) = VALUES1(IOB1,IDISP(J))

!-----------------------------------------------------------------------
!  (2)  Displacement = 0:  Missing data indicator
!-----------------------------------------------------------------------

      ELSE IF (IDISP(J) == 0) THEN      ! Item not available
        VALUES2(IOB2,J) = RMDI

      ELSE
        IVAL = -IDISP(J)
        IND = MOD(IVAL,10)
        IVAL = IVAL/10

!-----------------------------------------------------------------------
!  (3)  Displacement = -10N:  Character data with N = VALUES1 subscript
!-----------------------------------------------------------------------

IFLABEL3: &
        IF (IND == 0) THEN              ! Character element
          I = NINT(VALUES1(IOB1,IVAL))
          ILEN = I/65536           ! No. of characters
          ISTART1 = MOD(I,65536)   ! Start char. in input
          IEND1 = ISTART1 + ILEN-1 ! End char. in input
          IEND2 = ISTART2 + ILEN-1 ! End char. in output

!                      Check space in CSTR2 and transfer character data

          IF (IEND2 <= LCSTR2) THEN
            CSTR2(IOB2)(ISTART2:IEND2) = CSTR1(ISTART1:IEND1)
          ELSE
            ICODE = 211
          END IF
!                                                Put pointer in VALUES2

          VALUES2(IOB2,J) = I - ISTART1 + ISTART2
          ISTART2 = IEND2 + 1

!-----------------------------------------------------------------------
!  (4)  Displacement = -(10N+1):  N is subscript if INTELM array
!-----------------------------------------------------------------------

        ELSE IF (IND == 1) THEN         ! Value in INTELM
          VALUES2(IOB2,J) = INTELM(IVAL)

!-----------------------------------------------------------------------
!  (5)  Displacement = -(10N+2):  N is the data value itself
!-----------------------------------------------------------------------

        ELSE IF (IND == 2) THEN         ! Value in IVAL
          VALUES2(IOB2,J) = IVAL
        END IF IFLABEL3
      END IF IFLABEL2
    END DO DOLABEL2    ! of loop over elements
  END IF IFLABEL1
END DO DOLABEL1   ! End of loop over observations

NOB1 = IOB1
NOB2 = IOB2
!                                           Message for CSTR2 too small

IF (ICODE == 211) WRITE (6,'(T5,A,T15,A)') 'COPYVALS:', &
   'OUTPUT ARRAY FOR CHARACTER DATA IS TOO SMALL'
!
RETURN
END SUBROUTINE COPYVALS
