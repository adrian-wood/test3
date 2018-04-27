INTEGER FUNCTION IVALUE(STRING)                               !1.3

!-----------------------------------------------------------------------
!
! PROGRAM       : IVALUE (FUNCTION)
!
! PURPOSE       : TO CHECK STRING FOR FIGURES AND CONVERT TO INTEGER
!
! CALLED BY     : MDB U/A EXPANSION
!
! PARAMETERS    : (1) CHARACTER STRING TO BE CONVERTED
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ivalue.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/07/03 10:43:35  usmdb
! Removed byte counts from INTEGER declarations. Separated
! variable declaration and initialisation. Added copyright,
! modified header and comments - S.Cox
!
! Revision 1.3  2000/06/08  15:25:57
! 19/06/2000 Allow to cope with string lengths >6 and of 0. Stan K.
!
! Revision 1.2  97/07/31  09:28:23  09:28:23  uspm (Pat McCormack)
! First revision for MVS
!
! Revision 1.1  1997/07/04 12:45:30  uspm
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE                                                 !1.3

CHARACTER     STRING*(*)
CHARACTER*4   FORM(6)                                         !2.0
CHARACTER*132 HEAD
INTEGER       MISSING          ! Missing indicator            !2.0
INTEGER       L                ! used to hold LEN(STRING)     !2.0
INTEGER       I                ! Loop control                 !2.0

LOGICAL HEADSET                                               !2.0

DATA FORM/'(I1)','(I2)','(I3)','(I4)','(I5)','(I6)'/          !2.0
DATA HEADSET/.FALSE./                                         !2.0
DATA MISSING/-9999999/                                        !2.0

IF (.NOT.HEADSET) THEN                                        !1.3
  HEAD='&
 &$Source: /home/us0400/mdb/op/lib/source/RCS/ivalue.F,v $&
 &'//'$ $Date: 26/01/2010 10:18:13$ $Revision: 1$'
  HEADSET = .TRUE.                                            !1.3
ENDIF                                                         !1.3

L=LEN(STRING)

IVALUE = MISSING                             ! initialise     !1.3

!-----------------------------------------------------------------------
!1.3 Make sure that string length is between and including 1 and 6
!-----------------------------------------------------------------------

IF ((L.GE.1).AND.(L.LE.6)) THEN                               !1.3
  DO I=1,L                                                    !1.3

!-----------------------------------------------------------------------
!1.3 If character being checked is not numeric then return.
!-----------------------------------------------------------------------

    IF (STRING(I:I).LT.'0' .OR. STRING(I:I).GT.'9') RETURN    !1.3

  END DO                                                       !1.3
  READ (STRING,FORM(L)) IVALUE                                !1.3
ENDIF                                                         !1.3

RETURN
END FUNCTION IVALUE
