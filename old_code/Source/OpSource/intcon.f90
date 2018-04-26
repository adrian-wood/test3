LOGICAL FUNCTION INTCON (REQUEST, IPOS, IPOS2)

!-----------------------------------------------------------------------
!
! PROGRAM       : INTCON
!
! PURPOSE       : TO CHECK TO SEE IF YOU CAN CONVERT SPECIFIED
!                 CHARACTERS INTO INTEGERS
!
! DESCRIPTION   : A FUNCTION THAT LOOKS AT A SET OF CHARACTERS TO SEE
!                 IF THEY CAN BE CONVERTED INTO INTEGERS
!
! CALLED BY     : MDB,VALDAT,VALARE
!
! CALLS         : NONE
!
! PARAMETERS    : (1) REQUEST   USERS REQUEST STRING
!                 (2) IPOS      STARTING POSITION IN STRING
!                 (3) IPOS2     END POSITION IN STRING
!
! REVISION INFO :
!
! $Workfile: intcon.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 22/11/2010 16:58:01$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         22/11/2010 16:58:01    Stan Kellett
!       removed typo comma at end of IPOS declaration.
!  3    MetDB_Refresh 1.2         18/11/2010 10:47:53    Richard Weedon  Var
!       declarations updated to f95 standard
!  2    MetDB_Refresh 1.1         19/10/2010 14:32:45    Brian Barwell
!       Porting items 1-8, 12, 13 & removal of program change indicators done.
!  1    MetDB_Refresh 1.0         12/10/2010 16:27:06    Brian Barwell
!       Initial f77 version before porting to f90/95.
! $
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

INTEGER           ::     IPOS
INTEGER           ::     IPOS2
INTEGER           ::     I
CHARACTER(LEN=*)  ::     REQUEST

INTCON=.TRUE.
DO I=IPOS,IPOS2
  IF ((REQUEST(I:I) < '0').OR. (REQUEST(I:I) > '9')) THEN
    INTCON=.FALSE.
  END IF
END DO

RETURN
END FUNCTION INTCON
