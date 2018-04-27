SUBROUTINE BUFDMAP(STRING,CMPRES,NEXBIT,LASBIT,DSPLAY,NZEROS)
! ---------------------------------------------------------------------
!
! Program       : BUFDMAP
!
! Called by     : BUFDATA
!
! Purpose       : To return the number of zero bits in a bit map used
!  (informal)     by quality operations & (if requested) display it.
!
!    (formal)     Count the zero bits, putting X (rather than a dot)
!                 for each zero in the string to be printed. (Zeros
!                 show which elements have extra values to follow.)
!                 Print the bits in groups of 10, 5 groups to a line.
!
! Calls         : VALUE to get bit
!
! Parameters    :
!  (1) STRING   bit string (data section of BUFR message)          (i)
!                (not changed)
!  (2) CMPRES   true if data compressed (& bit map has 6-bit increment
!                widths between map bits!)                         (i)
!                (not changed)
!  (3) NEXBIT   number of bits before bit map                      (i)
!                (not changed)
!  (4) LASBIT   last bit in the bit map (in STRING)                (i)
!                (not changed)
!  (5) DSPLAY   set if bit map is to be printed                    (i)
!                (not changed)
!  (6) NZEROS   number of zeros in bit map                         (o)
!
! Error returns : none
!
! REVISION INFO :
!
! $Workfile: bufdmap.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 20/10/2010 09:16:31$
!
! CHANGE RECORD :
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
! ---------------------------------------------------------------------

USE value_mod

IMPLICIT  NONE

CHARACTER(LEN=*) ::  STRING      ! argument (1)
CHARACTER(LEN=1) ::  BITS(999)     ! printable bit map (bits set to . or X)
LOGICAL ::  CMPRES        ! argument (2)
LOGICAL ::  DSPLAY        ! argument (5)

INTEGER ::  NEXBIT        ! argument (3)
INTEGER ::  LASBIT        ! argument (4)
INTEGER ::  IBEFOR        ! number of bits before next bit in map
INTEGER ::  BIT           ! value of bit in map
INTEGER ::  IBIT          ! number of current bit in map
INTEGER ::  NZEROS        ! argument (6)
INTEGER ::  I             ! loop variable
!INTEGER ::  VALUE         ! function to get value from STRING


! Loop round the bits in the map (skipping any increment widths if the
! data is compressed - they must be zero), putting X in the bit string
! to be printed for any zero in the map. (Zeroes rather than ones mean
! that quality values will follow.)

IBIT=0
NZEROS=0
IBEFOR=NEXBIT
DO WHILE (IBEFOR < LASBIT)
  IBIT=IBIT+1
  BIT=VALUE(STRING,IBEFOR,1)
  IF (BIT == 1) THEN
    BITS(IBIT)='.'
  ELSE
    BITS(IBIT)='X'
    NZEROS=NZEROS+1
  END IF
  IF (CMPRES) IBEFOR=IBEFOR+6
END DO

! Print IBIT bits in groups of 10, 5 groups per line

IF (DSPLAY) THEN
  PRINT *,' '
 PRINT *,IBIT,'elements before this point referrred to by bit map'
 PRINT *,'       (X marks elements with quality values to follow)'
  PRINT *,' '
  WRITE (*,'((5(1X,10A1)))') (BITS(I),I=1,IBIT)
  PRINT *,' '
END IF
RETURN
END SUBROUTINE BUFDMAP
