SUBROUTINE SYNXP4(REPORT,POINT,NGRPS,IMT,MTCL)

!-----------------------------------------------------------------------
!
! PROGRAM       : SYNXP4
!
! PURPOSE       : EXPAND MOUNTAIN CLOUD GROUPS IN SYNOP (SECTION 4)
!
! DESCRIPTION   :
!
! CALLED BY     : SYNEXP
!
! ARGUMENTS     : (1) REPORT
!                 (2) STARTING POINT
!                 (3) NUMBER OF GROUPS IN SECTION (ALL NCHHCT)
!                 (4) NUMBER OF MOUNTAIN CLOUD GROUPS (MAX 2)
!                 (5) MOUNTAIN CLOUD DATA
!
! REVISION INFO :
!
! $Workfile: synxp4.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 23/12/2010 15:39:31$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         23/12/2010 15:39:31    John Norton     After
!       rework done
!  2    MetDB_Refresh 1.1         23/12/2010 10:48:08    John Norton     Ported
!        version for MDBSTOR batches 6 & 7
!  1    MetDB_Refresh 1.0         23/12/2010 10:16:13    John Norton
!       Pre-ported version but with changed extension for synop routines.
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE ivalue_mod !function

! <Data Modules>

USE metdb_com_mod, only : MISSIN

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: REPORT    !a01
INTEGER,          INTENT(INOUT) :: POINT     !a02
INTEGER,          INTENT(IN)    :: NGRPS     !a03
INTEGER,          INTENT(OUT)   :: IMT       !a04
REAL,             INTENT(INOUT) :: MTCL(4,2) !a05

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  I
INTEGER          ::  J

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

DOLABEL1: &
DO J=1,MIN(NGRPS,2)
!
! EACH GROUP GIVES AMOUNT, TYPE, HEIGHT & CLOUD TOP DESCRIPTION (CT)
! - BUT HEIGHT OF TOPS IS ABOVE MSL, NOT RELATIVE TO STATION!
!
  MTCL(1,J)=IVALUE(REPORT(POINT:POINT))       ! N (AMOUNT)
  MTCL(2,J)=IVALUE(REPORT(POINT+1:POINT+1))   ! C (TYPE)
  I=IVALUE(REPORT(POINT+2:POINT+3))             ! HH
  IF (I /= MISSIN) MTCL(3,J)=I*100.           ! TO METRES
  MTCL(4,J)=IVALUE(REPORT(POINT+4:POINT+4))   ! CT
  POINT=POINT+6
END DO DOLABEL1
IMT=2
RETURN
END SUBROUTINE SYNXP4
