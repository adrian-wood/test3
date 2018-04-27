SUBROUTINE VALOUT(STRING,IBEFOR,WIDTH,VALUE)

!-----------------------------------------------------------------------
!
! ROUTINE       : VALOUT
!
! PURPOSE       : put value in width bits after ibefor bits in string
!                     ~~~~~    ~~~~~            ~~~~~~         ~~~~~~
! NOTE          : must assume that octets correspond to characters
!
! ARGUMENTS     : (1) data section of BUFR message               (i/o)
!                 (2) number of bits already in string           (i/o)
!                 (3) width of new field                          (i)
!                 (4) value to be inserted                        (i)
!
! REVISION INFO :
!
! $Workfile: valout.F90$ $Folder: OpSource$
! $Revision: 3$ $Date: 09/02/2011 17:32:52$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         09/02/2011 17:32:52    Sheila Needham  Use
!       int2ch function
!  2    MetDB_Refresh 1.1         20/12/2010 15:55:20    Stan Kellett    added
!       INTENTs
!  1    MetDB_Refresh 1.0         22/11/2010 09:36:57    Sheila Needham
!       Renamed from .f90 because file has pre-processing directives
!       
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

USE int2ch_mod
IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare variables
!-----------------------------------------------------------------------

! Arguments
CHARACTER(LEN=*), INTENT(INOUT) ::  STRING
INTEGER, INTENT(INOUT) ::  IBEFOR
INTEGER, INTENT(IN) ::  WIDTH
INTEGER, INTENT(IN) ::  VALUE

! Local variabls
INTEGER ::  BYTENO
INTEGER ::  BITNO
INTEGER ::  I
#if defined (T3E)
INTEGER ::  ITEMP
#endif
INTEGER ::  NBYTES
INTEGER ::  NLAST
INTEGER ::  OCTET
INTEGER ::  TWOTO(0:8)
INTEGER ::  VAL



!-----------------------------------------------------------------------
! Save all variables
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! /TWOTO(N) puts N zero bits at top of value, *TWOTO(N) puts N at tail
!-----------------------------------------------------------------------

DATA TWOTO/1,2,4,8,16,32,64,128,256/

!-----------------------------------------------------------------------
! BYTENO - number of octet in string where value is to start
! BITNO  - number of bit in that octet where value is to start
! NBYTES - number of octets over which the value will extend
! NLAST  - number of bits in the value in the last of these octets
!-----------------------------------------------------------------------


IF (WIDTH == 0) RETURN

#if defined (T3E)
BYTENO=shiftr(IBEFOR, 3)
BITNO=IBEFOR-shiftl(BYTENO, 3)
NBYTES=shiftr((WIDTH+BITNO+7), 3)
NLAST=WIDTH+BITNO-shiftl((NBYTES-1), 3)
#else
BYTENO=IBEFOR/8
BITNO=IBEFOR-BYTENO*8
NBYTES=(WIDTH+BITNO+7)/8
NLAST=WIDTH+BITNO-(NBYTES-1)*8
#endif

!-----------------------------------------------------------------------
! if all the bits will go in the same byte, put tail on new value & add
! it to bits already used in byte.  (if none used, octet=0 clears byte)
!-----------------------------------------------------------------------

IF (NBYTES == 1) THEN
  IF (BITNO == 0) THEN
    OCTET=0
  ELSE
    OCTET=ICHAR(STRING(BYTENO+1:BYTENO+1))
    IF (OCTET < 0) OCTET=OCTET+256
  END IF
#if defined (T3E)
  ITEMP=OCTET+shiftl(VALUE,8-NLAST)
  STRING(BYTENO+1:BYTENO+1)=int2ch(ITEMP)
#else
  STRING(BYTENO+1:BYTENO+1)=int2ch(OCTET+VALUE*TWOTO(8-NLAST))
#endif
ELSE

!-----------------------------------------------------------------------
! if more than one byte is needed, set the bytes from right to left.
! first chop off bits to go in last byte (after mod must add tail).
!-----------------------------------------------------------------------

#if defined (T3E)
  OCTET=shiftl(VALUE-                   &
     &  shiftl(shiftr(VALUE, NLAST), NLAST), 8-NLAST)
#else
  OCTET=MOD(VALUE,TWOTO(NLAST))*TWOTO(8-NLAST)
#endif
  STRING(BYTENO+NBYTES:BYTENO+NBYTES)=int2ch(OCTET)

!-----------------------------------------------------------------------
! set any intermediate bytes by chopping off 8-bit "sausages".
! /TWOTO(NLAST) aligns the new bits with the spare bits in the string.
!-----------------------------------------------------------------------

#if defined (T3E)
  VAL=shiftr(VALUE, NLAST)
  DO  I=NBYTES-1,2,-1
   OCTET=iand(val, 255)
   STRING(BYTENO+I:BYTENO+I)=int2ch(OCTET)
   VAL=shiftr(VAL , 8)
   END DO
#else
  VAL=VALUE/TWOTO(NLAST)
  DO I=NBYTES-1,2,-1
   OCTET=MOD(VAL,256)
   STRING(BYTENO+I:BYTENO+I)=int2ch(OCTET)
   VAL=VAL/256
   END DO
#endif

!-----------------------------------------------------------------------
! finally set the first byte, adding to any bits already used.
!-----------------------------------------------------------------------

  IF (BITNO == 0) THEN
    OCTET=0
  ELSE
    OCTET=ICHAR(STRING(BYTENO+1:BYTENO+1))
    IF (OCTET < 0) OCTET=OCTET+256
  END IF
  STRING(BYTENO+1:BYTENO+1)=int2ch(OCTET+VAL)
END IF

IBEFOR=IBEFOR+WIDTH

RETURN
END SUBROUTINE VALOUT
