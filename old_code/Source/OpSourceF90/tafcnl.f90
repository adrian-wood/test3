SUBROUTINE TAFCNL(REPORT,REPLEN,C_RC)
!-----------------------------------------------------------------------
!
!   PROGRAM     : TAFCNL
!
!   PURPOSE     : This routine having been activated by the presence of
!                 Amendment indicator will check for the presence of a
!                 cancellation flag. This is normally at the end of the
!                 bulletin.
!
!   DESCRIPTION : Groups delimited by spaces are found by MTRLOC.
!
!  CALLED BY    : TAFEXP
!
!  CALLS        : MTRLOC  to delimit a group
!
!  PARAMETERS   : 1 REPORT - TAF REPORT TO BE DECODED          (input)
!                 2 REPLEN - REPORT LENGTH                     (input)
!                 3 C_RC   - RETURN CODE                      (output)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Author: Richard Weedon$
! $Folder: OpSourceF90$
! $Workfile: tafcnl.f90$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

CHARACTER*(*) REPORT    ! Report
INTEGER       REPLEN    ! Report Length
INTEGER       C_RC      ! Return code 1 = TAF cancelled
                        ! 2= TAF VALID


INTEGER       IN        ! pointer to group position
INTEGER       IN_OLD
INTEGER       GRPLEN    ! Group length

CHARACTER*20 GROUP      ! Group

LOGICAL LREPFL
LOGICAL HEADSET
CHARACTER*132 HEAD
DATA HEADSET/.FALSE./

IF (.NOT.HEADSET) THEN
  HEAD='$Workfile: tafcnl.f90$ ' //&
&'$Revision: 1$ $Date: 26/01/2010 10:18:13$'
  HEADSET=.TRUE.
ENDIF

LREPFL=.FALSE.
C_RC=0
IN=1

CALL MTRLOC(REPLEN,REPORT,IN,GRPLEN,LREPFL)
GROUP=REPORT(IN:IN+GRPLEN-1)

DO WHILE (.NOT.LREPFL)
  IN_OLD=IN
  CALL MTRLOC(REPLEN,REPORT,IN,GRPLEN,LREPFL)
  GROUP=REPORT(IN_OLD:IN_OLD+GRPLEN-1)

  IF (GROUP(1:4).EQ.'CNCL') THEN
    C_RC=1
  ELSE IF (GROUP(1:3).EQ.'CNL') THEN
    C_RC=1
  ELSE IF (GROUP(1:2).EQ.'CL') THEN
    C_RC=1
  ENDIF

END DO

RETURN
END
