FUNCTION VALUE(STRING,IBEFOR,WIDTH)

!-----------------------------------------------------------------------
!
! ROUTINE       : value
!
! PURPOSE       : get value in width bits after ibefor bits in string
!                     /////    /////            //////         //////
! NOTE          : must assume that octets correspond to characters
!
! ARGUMENTS     : (1) string of descriptors
!               : (2) number of bits to skip before target field
!               : (3) width of target field
!
! REVISION INFO :
!
! $Workfile: value.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 18/11/2010 13:30:01$
!
! CHANGE RECORD :
!
! $Log:
!  6    MetDB_Refresh 1.5         18/11/2010 13:30:01    Sheila Needham  Change
!        INTENT on IBEFOR
!  5    MetDB_Refresh 1.4         27/10/2010 12:17:57    Richard Weedon
!       updated to f95 standard
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:58:02    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare variables
!-----------------------------------------------------------------------

! Arguments

CHARACTER(LEN=*),INTENT(IN)   ::  STRING
INTEGER,INTENT(INOUT)         ::  IBEFOR
INTEGER,INTENT(IN)            ::  WIDTH
INTEGER ::  VALUE

! local variables

INTEGER ::  BITNO
INTEGER ::  BYTENO
INTEGER ::  I
INTEGER ::  NBYTES
INTEGER ::  NLAST
INTEGER ::  OCTET
INTEGER ::  TWOTO(0:8)



!-----------------------------------------------------------------------
! Save all variables
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! I/TWOTO(N) chops bottom N bits of I,  MOD(I,TWOTO(8-N)) chops top N.
!-----------------------------------------------------------------------

DATA TWOTO/1,2,4,8,16,32,64,128,256/

!-----------------------------------------------------------------------
! BYTENO - number of octet in string where value starts
! BITNO  - number of bit in that octet where value starts
! NBYTES - number of octets over which the value extends
! NLAST  - number of bits in the value in the last of these octets
!-----------------------------------------------------------------------


BYTENO=IBEFOR/8
BITNO=IBEFOR-BYTENO*8
VALUE=0
IF (WIDTH == 0) RETURN

!-----------------------------------------------------------------------
! either the value is within an octet, in which case it may have to be
! "topped & tailed", or it extends over more than one octet, in which
! case an octet is either at the start (so may have to be "topped") or
! in the middle (just add it in) or at the end (may need "tailing").
!-----------------------------------------------------------------------

IF (WIDTH+BITNO <= 8) THEN
  OCTET=ICHAR(STRING(BYTENO+1:BYTENO+1))
  IF (OCTET < 0) OCTET=OCTET+256
  VALUE=MOD(OCTET,TWOTO(8-BITNO))/TWOTO(8-WIDTH-BITNO)
ELSE
  NBYTES=(WIDTH+BITNO+7)/8
  NLAST=WIDTH+BITNO-(NBYTES-1)*8
  DO I=1,NBYTES
    OCTET=ICHAR(STRING(BYTENO+I:BYTENO+I))
    IF (OCTET < 0) OCTET=OCTET+256
    IF (I == 1) THEN
      VALUE=MOD(OCTET,TWOTO(8-BITNO))
    ELSE IF (I < NBYTES) THEN
      VALUE=VALUE*256+OCTET
    ELSE IF (I == NBYTES) THEN
      VALUE=VALUE*TWOTO(NLAST)+OCTET/TWOTO(8-NLAST)
    END IF
  END DO
END IF
IBEFOR=IBEFOR+WIDTH

RETURN
END FUNCTION VALUE
