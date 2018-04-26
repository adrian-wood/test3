SUBROUTINE ARGCHOP (NOBS1, NSEQ, NTH, IDESCR, NDESCR,       &
                    NAMES, VALIN, VALOUT, MSGBULL, LENBUL,  &
                    ICAT,ISUB)

!-----------------------------------------------------------------------
!
! SUBROUTINE  : ARGCHOP
!
! PURPOSE     : To create a new ARGO bulletin containing one report
!
! DESCRIPTION : Given an array of decoded values (VALIN) from an
!               ARGO bulletin, ARGCHOP extracts the Nth observation
!               and copies it to a similar array (VALOUT), then converts
!               this into a new BUFR message MSGBULL of length LENBUL.
!
!               VALOUT is only for internal use but is put in the
!               argument list so that the user can have control over
!               its size. It should be the same size as VALIN but must
!               be a separate array.
!
! USAGE       : CALL ARGCHOP (NOBS1, NSEQ, NTH, IDESCR, NDESCR,
!                         NAMES, VALIN, VALOUT, MSGBULL, LENBUL,
!                         ICAT, ISUB)
!
! ARGUMENTS   : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!     No.  Arg.  I/O Type  Size   Contents
!     --  -----  --- ----  ----   --------
!      1  NOBS1   I   I4        Number of obs in input array (VALIN).
!      2  NSEQ    I   I4        BUFR sequence descriptor for output.
!      3  NTH     I   I4        Required OB from input array (Nth OB).
!      4  IDESCR  I   I4    *   Descriptor array
!      5  NDESCR  I   I4        Number of descriptors (for 1st ob)
!      6  NAMES   I/O   C(*)      Any character values output from debufr
!      7  VALIN   I   R4    *   Input array of decoded element values
!                                 (effective dimensions (NOBS1,NELEMS).)
!      8  VALOUT  O   R4    *   Work array for values to be encoded
!                                 (effective dimensions (1,NELEMS).)
!      9  MSGBULL O  C(*)       Output ARGO bulletin ('BUFR..7777').
!     10  LENBUL  O   I4        Length of output bulletin in bytes.
!     11  ICAT    I   I4        BUFR data category
!     12  ISUB    I   I4        BUFR data sub-category
!
! CALLED BY   : STOREUM
!
! CALLS       : DATIM, ENBUFV4, DESFXY
!
! HISTORY     : This version by Sheila Needham based on MSGCHOP.
!
! REVISION INFO :
!
!
! $Workfile: argchop.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 18/03/2011 21:51:01$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         18/03/2011 21:51:01    Sheila Needham  Change
!        INTENT on NAMES to INOUT because it may be updated by EB2ASC
!  2    MetDB_Refresh 1.1         07/03/2011 11:05:42    Stan Kellett
!       updated to call enbufv4 rather than enbufv2
!  1    MetDB_Refresh 1.0         27/01/2011 16:19:32    Rosemary Lavery
!       Initial port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE DATIM_mod
USE DESFXY_mod
USE ENBUFV4_mod

IMPLICIT NONE

! Arguments

INTEGER, INTENT(IN)             :: NOBS1     ! (a1)  Number of obs in input values array VALIN
INTEGER, INTENT(IN)             :: NSEQ      ! (a2)  Number of obs in output BUFR message
INTEGER, INTENT(IN)             :: NTH       ! (a3)  Observation number required from VALIN
INTEGER, INTENT(IN)             :: IDESCR(:) ! (a4)  Descriptor array
INTEGER, INTENT(IN)             :: NDESCR    ! (a5)  Number of descriptors for 1st ob in IDESCR
CHARACTER (LEN=*),INTENT(INOUT) :: NAMES     ! (a6)  Input character values
REAL, INTENT(IN)                :: VALIN(:)  ! (a7)  Decoded values from ARGO bulletin
REAL, INTENT(OUT)               :: VALOUT(:) ! (a8)  Values for encoding new ARGO bulletin
CHARACTER (LEN=*), INTENT(OUT)  :: MSGBULL   ! (a9)  Re-encoded ARGO bulletin
INTEGER, INTENT(OUT)            :: LENBUL    ! (a10) Length of re-encoded message
INTEGER, INTENT(IN)             :: ICAT      ! (a11) BUFR category
INTEGER, INTENT(IN)             :: ISUB      ! (a12) Data sub-category

! Parameters

INTEGER, PARAMETER  :: MAXDES = 7000  ! must be big enough for one fully
!                                       expanded descriptor sequence
INTEGER, PARAMETER  :: NOBS = 1       ! )
INTEGER, PARAMETER  :: IEDN = 3       ! )
INTEGER, PARAMETER  :: MSTR = 0       ! )
INTEGER, PARAMETER  :: MSTR_VER = 13  ! )
INTEGER, PARAMETER  :: ICTR = 74      ! ) for call to ENBUFV4
INTEGER, PARAMETER  :: NIL  = -99     ! )
INTEGER, PARAMETER  :: ISC3 = 1       ! )
LOGICAL, PARAMETER  :: OFF = .FALSE.  ! )

! Variables

INTEGER  :: IDES         ! 'FXXYYY'-to-integer conversion function
INTEGER  :: IC           ! Position in ICPTR array
INTEGER  :: ICPTR(10)    ! positions of character values
INTEGER  :: IX           ! Position in IDESCR array
INTEGER  :: J            ! Loop variable for general use
INTEGER  :: JDES         ! Loop variable for loop over descriptorss
INTEGER  :: JVAL         ! Loop variable for loop over ob values
INTEGER  :: NDES         ! Number of descriptors in LISTDES array
INTEGER  :: NTIME(6)     ! Time of receipt (yr, mon, day, hr, min, sec)
INTEGER  :: NF           ! Components of descriptor FXY
INTEGER  :: NELEMS       ! Number of values per ob in VALIN array
INTEGER  :: NOW(8)       ! Current date and time (from call to DATIM)
INTEGER  :: NVAL1        ! Pointer to location in VALIN array
INTEGER  :: NVAL2        ! Pointer to location in VALOUT array
INTEGER  :: NX           ! Components of descriptor FXY
INTEGER  :: NY           ! Components of descriptor FXY
INTEGER, SAVE  :: LISTDES(MAXDES) ! BUFR descriptor sequence for message

CHARACTER (LEN=1)  :: CDUMMY! Dummy character argument for ENBUFV4


!-----------------------------------------------------------------------

! Initializations
IC = 0    ! count of character elements

! Find position in IDESCR for the start of this OB

IX = 1    ! pointer to start of descriptor list in IDESCR
NDES=NDESCR
DO J=2,NTH
  IX = IX+NDES
  NDES = IDESCR(IX)
  IX=IX+1
END DO

! Check there will be room for the encoder to expand the replications

IF (NDES > MAXDES) THEN
  WRITE(6,'(''ARGCHOP: Too many replications'',I6)')NDES
  LENBUL = 0
  RETURN
END IF

! Find number of data elements corresponding to these descriptors

NELEMS=0
DO JDES=IX,IX+NDES-1
  CALL DESFXY (IDESCR(JDES), NF,NX,NY)
  IF (NF == 0) THEN
     NELEMS = NELEMS + 1
  END IF

! Store the position of character elements

  IF (IDESCR(JDES) > 131072)THEN
    IC=IC+1
    ICPTR(IC)=NELEMS
  END IF
END DO ! JDES

!-----------------------------------------------------------------------
!     EXTRACT DATA FOR THE NTH OBSERVATION
!-----------------------------------------------------------------------

NVAL1 = NTH       ! First data value in VALIN
NVAL2 = 1         ! First data value in VALOUT

! Transfer data values to VALOUT
DO JVAL=1,NELEMS
  VALOUT(NVAL2) = VALIN(NVAL1)
  NVAL1 = NVAL1 + NOBS1
  NVAL2 = NVAL2 + 1
END DO ! JVAL

! Adjust pointers to character elements
! (DEBUFR puts a length and pointer to the character string in the
!   values array, but ENBUFR only needs the pointer)

DO J=1,IC
  JVAL=ICPTR(J)
  VALOUT(JVAL)=AMOD(VALOUT(JVAL),65536.0)
END DO

!-----------------------------------------------------------------------
!     CREATE NEW ARGO BULLETIN CONTAINING ONE OBSERVATION
!-----------------------------------------------------------------------

! Get time of receipt

CALL DATIM (NOW)

DO J=1,6                 ! Yr, mon, day, hr, min, sec
  NTIME(J) = NOW(9-J)
END DO ! J

! Encode new bulletin
LISTDES(1) = IDES(NSEQ)  ! BUFR sequence
DO J=2,MAXDES
  LISTDES(J) = 0         ! Rest of descriptor array
END DO ! J

NDES = 1

CALL ENBUFV4 (LISTDES, VALOUT, NDES, NELEMS, NOBS, NAMES, NTIME,  &
              MSGBULL, OFF, LENBUL, IEDN, MSTR, MSTR_VER, ICTR, NIL, &
              ICAT, ISUB, NIL, NIL, OFF, CDUMMY, OFF, CDUMMY, ISC3)

!                                             Return to calling program
RETURN
END SUBROUTINE ARGCHOP
