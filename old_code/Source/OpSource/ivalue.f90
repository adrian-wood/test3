INTEGER FUNCTION IVALUE (STRING)

!-----------------------------------------------------------------------
!
! PROGRAM       : IVALUE (FUNCTION)
!
! PURPOSE       : TO CHECK STRING FOR FIGURES AND CONVERT TO INTEGER
!
! ARGUMENTS     : (1) CHARACTER STRING TO BE CONVERTED
!
! REVISION INFO :
!
! $Workfile: ivalue.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 18/11/2010 12:17:35$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         18/11/2010 12:17:35    Richard Weedon  Var
!       Dec updated
!  2    MetDB_Refresh 1.1         19/10/2010 14:35:02    Brian Barwell
!       Porting items 1-8, 12, 13 & removal of program change indicators done.
!  1    MetDB_Refresh 1.0         12/10/2010 16:28:11    Brian Barwell
!       Initial f77 version before porting to f90/95.
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

CHARACTER(LEN=*),INTENT(IN)   ::     STRING
CHARACTER(LEN=4)              ::     FORM(6)
INTEGER                       ::     MISSING          ! Missing indicator
INTEGER                       ::     L                ! used to hold LEN(STRING)
INTEGER                       ::     I                ! Loop control

DATA FORM/'(I1)','(I2)','(I3)','(I4)','(I5)','(I6)'/
DATA MISSING/-9999999/

L=LEN(STRING)

IVALUE = MISSING                             ! initialise

!-----------------------------------------------------------------------
! Make sure that string length is between and including 1 and 6
!-----------------------------------------------------------------------

IF (L >= 1 .AND. L <= 6) THEN
  DO I=1,L

!-----------------------------------------------------------------------
! If character being checked is not numeric then return.
!-----------------------------------------------------------------------

    IF (STRING(I:I) < '0' .OR. STRING(I:I) > '9') RETURN

  END DO
  READ (STRING,FORM(L)) IVALUE
END IF

RETURN
END FUNCTION IVALUE
