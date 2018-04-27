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
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /data/us0400/mdb/op/lib/source/RCS/value.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2002/04/09  11:45:55  11:45:55  usmdb (Generic MetDB account)
! After each call to ICHAR, the value returned by ICHAR is checked.
! If < 0, 256 is added - needed for Sun OS.
! Added IMPLICIT NONE and declared variables. Added SAVE and
! changed HEAD*1 to HEAD*132 - S.Cox
!
! Revision 2.0  2001/03/07 10:19:21  usmdb
! Added copyright. Modified header & comments - S.Cox
!
! Revision 1.2  1999/02/11 12:02:12  usmdb
! 15 Feb 1999   C Long
! HEAD= first time only
!
! Revision 1.1  97/06/19  13:43:12  13:43:12  uspm (Pat McCormack)
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE                                                 !2.1

!-----------------------------------------------------------------------
! Declare variables
!-----------------------------------------------------------------------

INTEGER BITNO
INTEGER BYTENO
INTEGER I
INTEGER IBEFOR
INTEGER NBYTES
INTEGER NLAST
INTEGER OCTET
INTEGER TWOTO(0:8)
INTEGER VALUE
INTEGER WIDTH

LOGICAL HEADSET                                               !1.2

CHARACTER HEAD*132                                            !2.1
CHARACTER STRING*(*)

!-----------------------------------------------------------------------
! Save all variables
!-----------------------------------------------------------------------

SAVE                                                          !2.1

!-----------------------------------------------------------------------
! I/TWOTO(N) chops bottom N bits of I,  MOD(I,TWOTO(8-N)) chops top N.
!-----------------------------------------------------------------------

DATA TWOTO/1,2,4,8,16,32,64,128,256/
DATA HEADSET/.FALSE./                                         !1.2

!-----------------------------------------------------------------------
! BYTENO - number of octet in string where value starts
! BITNO  - number of bit in that octet where value starts
! NBYTES - number of octets over which the value extends
! NLAST  - number of bits in the value in the last of these octets
!-----------------------------------------------------------------------

IF (.NOT.HEADSET) THEN                                        !1.2
  HEAD='$Workfile: value.f90$ ' //&
      &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'
  HEADSET=.TRUE.                                              !1.2
ENDIF                                                         !1.2

BYTENO=IBEFOR/8
BITNO=IBEFOR-BYTENO*8
VALUE=0
IF (WIDTH.EQ.0) RETURN

!-----------------------------------------------------------------------
! either the value is within an octet, in which case it may have to be
! "topped & tailed", or it extends over more than one octet, in which
! case an octet is either at the start (so may have to be "topped") or
! in the middle (just add it in) or at the end (may need "tailing").
!-----------------------------------------------------------------------

IF (WIDTH+BITNO.LE.8) THEN
  OCTET=ICHAR(STRING(BYTENO+1:BYTENO+1))
  IF (OCTET.LT.0) OCTET=OCTET+256                             !2.1
  VALUE=MOD(OCTET,TWOTO(8-BITNO))/TWOTO(8-WIDTH-BITNO)
ELSE
  NBYTES=(WIDTH+BITNO+7)/8
  NLAST=WIDTH+BITNO-(NBYTES-1)*8
  DO I=1,NBYTES                                               !2.1
    OCTET=ICHAR(STRING(BYTENO+I:BYTENO+I))
    IF (OCTET.LT.0) OCTET=OCTET+256                           !2.1
    IF (I.EQ.1) THEN
      VALUE=MOD(OCTET,TWOTO(8-BITNO))
    ELSE IF (I.LT.NBYTES) THEN
      VALUE=VALUE*256+OCTET
    ELSE IF (I.EQ.NBYTES) THEN
      VALUE=VALUE*TWOTO(NLAST)+OCTET/TWOTO(8-NLAST)
    ENDIF
  END DO                                                       !2.1
ENDIF
IBEFOR=IBEFOR+WIDTH

RETURN
END FUNCTION VALUE
