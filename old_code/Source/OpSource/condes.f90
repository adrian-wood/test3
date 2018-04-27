SUBROUTINE CONDES(IDESC,NDES,IDISP)
!-----------------------------------------------------------------------
!
! SUBROUTINE    : CONDES IN LOAD MODULE TFMTRT
!
! PURPOSE       : CONVERTS ELEMENTS SELECTED TO DISPLACEMENTS IN
!                 ARRAY.
!
! DESCRIPTION   :
!
! DATA TYPE(S)  : METARS, TAFS
! HANDLED
!
! CALLED BY     : TFMTRT
!
! CALLS         : NOTHING
!
! PARAMETERS    :
!
!      IDESC(NDES) INTEGER LIST OF ELEMENT NUMBERS SELECTED
!      NDES        INTEGER NUMBER OF ELEMENTS
!      IDISP(NDES) INTEGER LIST OF DISPLACEMENTS/INDICATORS FOR
!                          TRNSFR TO USE TO MOVE DATA TO USERS ARRAY
!
!Y2K  26.06.1997  CONDES IS YEAR 2000 COMPLIANT.
!
! REVISION INFO  :
!
! $Revision: 6$
! $Date: 01/11/2010 15:07:07$
! $Source: /home/us0400/mdb/op/lib/source/RCS/condes.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  6    MetDB_Refresh 1.5         01/11/2010 15:07:07    Stan Kellett    rework
!        done after review
!  5    MetDB_Refresh 1.4         20/10/2010 09:03:14    Stan Kellett    Files
!       ready for review
!  4    MetDB_Refresh 1.3         15/10/2010 16:17:35    Stan Kellett    Basic
!       port almost complete just require the modules added for the interface
!       files they will need.
!  3    MetDB_Refresh 1.2         14/10/2010 17:59:43    Stan Kellett    Basic
!       port done upto changing of argument declarations. Ordering of
!       declarations and interface files still to do.
!  2    MetDB_Refresh 1.1         12/10/2010 10:10:13    Stan Kellett
!       Continuation markers changed to f90 standard.
!       Logical operators such as .LE. .GE. .GT. .LT. etc changed to <=, >=,
!       >, and < as part of porting to f90/95
!  1    MetDB_Refresh 1.0         11/10/2010 13:39:02    Stan Kellett
!       Initial f77 versions before porting to f90/95
! $
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

! Argument declarations
INTEGER, INTENT(IN)      :: NDES         ! NUMBER OF ELEMENTS
INTEGER, INTENT(INOUT)   :: IDESC(NDES)  ! LIST OF ELEMENT NUMNERS
INTEGER, INTENT(INOUT)   :: IDISP(NDES)  ! LIST DISPLACEMENTS

! Local variable declarations
INTEGER                  :: J1           ! LOOP OVER NUMBER OF ELEMENTS
INTEGER                  :: IBIT18 = 131072

DESLOOP: &
DO J1=1,NDES

! CHARACTER ELEMENTS
  IF(IDESC(J1) >  IBIT18)THEN
    IDISP(J1)=IDESC(J1)

!   INDEX INFO INDICATOR
  ELSEIF(IDESC(J1) >  65280)THEN
    IDISP(J1)=65280-IDESC(J1)

! CHARACTER REPORT TEXT INDICATOR
  ELSEIF(IDESC(J1) == 65250)THEN
    IDISP(J1)=-99

! ALL OTHER VALUES TRANSFERRED DIRECTLY FROM EXPANSION
  ELSE
    IDISP(J1)=IDESC(J1)
  END IF

END DO DESLOOP

RETURN
END SUBROUTINE CONDES
