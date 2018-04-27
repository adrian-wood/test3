FUNCTION FINDPATH(PREFIX, PATH, PREF, NPATH)

!-----------------------------------------------------------------------
!
! ROUTINE       : FINDPATH
!
! PURPOSE   :  To match an input PREFIX to a list of NPATH
!              prefixes (PREF) and return the corresponding
!              PATHNAME
!
! ARGUMENTS     : PREFIX  - prefix of wanted path name
!                 NPATH   - number of paths
!                 PATH    - list of pathnames
!                 PREF    - list of corresponding prefixes
!
! REVISION INFO :
!
! $Workfile: findpath.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 05/04/2011 14:13:50$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         05/04/2011 14:13:50    Alison Weir
!       Initial version - find zfs path name
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

CHARACTER(*),INTENT(IN):: PREFIX
INTEGER     ,INTENT(IN):: NPATH
CHARACTER(*),INTENT(IN):: PATH(NPATH)
CHARACTER(*),INTENT(IN):: PREF(NPATH)

CHARACTER(LEN=LEN(PATH(1))) :: FINDPATH

FINDPATH=' '
DO I=1,NPATH
  IF(PREF(I) == PREFIX) THEN
    FINDPATH=PATH(I)
    EXIT
  END IF
END DO

END FUNCTION FINDPATH
