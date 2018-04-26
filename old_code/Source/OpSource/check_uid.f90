SUBROUTINE CHECK_UID(UID)
!
!------------------------------------------------------------------
! ROUTINE : CHECK_UID
!
! DESCRIPTION : RETURNS THE USER ID TO THE CALLING FUNCTION.
!
! CALLED BY : USERS PROGRAM
!
! CALLS : C ROUTINE GETID
!
! ARGUMENTS :
!
! UID : CHAR(LEN=5)   : USERID
!
! REVISION INFO:
!
! $Workfile: check_uid.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 19/07/2011 15:27:58$
!
! CHANGE RECORD:
!
! $Log:
!  6    MetDB_Refresh 1.5         19/07/2011 15:27:58    Sheila Needham  Remove
!        print statement
!  5    MetDB_Refresh 1.4         05/07/2011 10:49:08    Richard Weedon  last
!       update
!  4    MetDB_Refresh 1.3         01/07/2011 16:39:09    Richard Weedon
!       Updated
!  3    MetDB_Refresh 1.2         01/07/2011 16:34:25    Richard Weedon
!       updated
!  2    MetDB_Refresh 1.1         23/06/2011 14:19:46    Richard Weedon  Check
!       uid routine initail version
!  1    MetDB_Refresh 1.0         16/06/2011 16:00:52    Richard Weedon
!       initial version
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
!
IMPLICIT NONE
!
! DECLARATIONS
!
CHARACTER(5),INTENT(INOUT)     ::   UID
!
! CALL C ROUTINE TO OBTAIN USERID
!
call getid(UID)
!
!
END SUBROUTINE CHECK_UID
