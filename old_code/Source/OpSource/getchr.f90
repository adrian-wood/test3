CHARACTER(LEN=*) FUNCTION GETCHR(RDISP,CD)

!-----------------------------------------------------------------------
!
! FUNCTION      : GETCHR IN MDB
!
! PURPOSE       : TO EXTRACT CHARACTER ELEMENTS FROM A STRING RETURNED
!                 BY MDB
! DESCRIPTION   :
!
! DATA TYPE(S)  : ALL MDB TYPES
!
! CALLED BY     : USER
!
! CALLS         : NOTHING
!
! ARGUMENTS     : (1) RDISP POINTER VALUE FROM MDB REAL ARRAY
!                 (2) CD    CHARACTER STRING
!
! INTRODUCED : 17/02/92
!
!Y2K  26.06.1997  GETCHR IS YEAR 2000 COMPLIANT.
!
! REVISION INFO :
!
! $Workfile: getchr.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 06/06/2011 11:22:42$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         06/06/2011 11:22:42    Sheila Needham  Make
!       the function assumed length (obsolescent but still works)
!  1    MetDB_Refresh 1.0         21/04/2011 12:05:12    John Norton
!       Initial version
! $
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>


! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

REAL,             INTENT(IN)  :: RDISP
CHARACTER(LEN=*), INTENT(IN)  :: CD

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  ID
INTEGER          ::  ILEN
INTEGER          ::  IS

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------


GETCHR(1:) = ' '           ! Initialise output string

ID=RDISP
ILEN=ID/65536
IS=MOD(ID,65536)
GETCHR=CD(IS:IS+ILEN-1)

RETURN
END FUNCTION GETCHR
