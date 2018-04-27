 SUBROUTINE MDBRSN(RQSTDSN,RESTDSN,MSTREAM,RC)
!-----------------------------------------------------------------------
!
! Program        : MDBRSN
!
! Purpose        : Synchronous restore of MDB MASS datasets
!
! Description    : Passes parameters to a shell script.
!
! Calls          : ALLTSO,TSOENV,TSOLNK
!
! Arguments      : RQSTDSN  (i)   MASS dsn
!                  RESTDSN  (i)   dataprk dsn
!                  MSTREAM  (i)   MASS Stream name
!                  RC       (o)   Return code - not to be trusted!
!
! REVISION INFO  :
!
! $Workfile: mdbrsn.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 01/12/2010 10:53:45$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         01/12/2010 10:53:45    Sheila Needham  Change
!        script location to /usr/local/mdb
!  2    MetDB_Refresh 1.1         12/11/2010 11:57:04    Sheila Needham  Remove
!        prints
!  1    MetDB_Refresh 1.0         12/11/2010 11:50:44    Sheila Needham  New
!       version
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
! ---------------------------------------------------------------------

IMPLICIT NONE

! Arguments

CHARACTER(LEN=44),INTENT(IN)  :: RQSTDSN
CHARACTER(LEN=44),INTENT(IN)  :: RESTDSN
CHARACTER(LEN=3) ,INTENT(IN)  :: MSTREAM
INTEGER          ,INTENT(OUT) :: RC

! Local Variables

CHARACTER(LEN=200):: CMD
CHARACTER(LEN=100):: ARGS

! Function

INTEGER :: system_cmd

ARGS = TRIM(RQSTDSN)//' '//TRIM(RESTDSN)// ' '//TRIM(MSTREAM)
CMD  = '/usr/local/mdb/mdbmoose.sh '//ARGS//CHAR(0)

! Call C system_cmd function to run the script

RC = system_cmd(CMD)

RETURN
END SUBROUTINE MDBRSN
