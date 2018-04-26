LOGICAL FUNCTION INQUIRE(filename,mode)
!-----------------------------------------------------------------------
!
! PROGRAM       : INQUIRE
!
! PURPOSE       : to find out if a dataset or ddname exists
!
! DESCRIPTION   : The INQUIRE function does not produce the same
!                 results in F95 as it does in VSFortran, specifically
!                 it cannot be used to test the existence of a dataset
!                 before the dataset has been opened (!).
!                 This function attempts to open a dataset either by
!                 datasetname or by ddname, then tests the IOSTAT to
!                 return an existence flag.
!
! CALLED BY     : Anything
!
! ARGUMENTS     : (1)  FILENAME   Character*(*)            (input)
!               : (2)  MODE       either 'DSN' or 'DDN'    (input)
!
! REVISION INFO :
!
! $Workfile: inquire.F90$ $Folder: OpSource$
! $Revision: 4$  $Date: 24/11/2010 12:38:33$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         24/11/2010 12:38:33    Sheila Needham  Remove
!        print
!  3    MetDB_Refresh 1.2         18/11/2010 10:36:53    Richard Weedon
!       removed print*,
!  2    MetDB_Refresh 1.1         18/11/2010 10:32:11    Richard Weedon  Update
!        from Test/use
!  1    MetDB_Refresh 1.0         09/11/2010 10:47:37    Sheila Needham  New
!       function to replace VSFortran INQUIRE functionality for existence of a
!        file
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
!-----------------------------------------------------------------------

IMPLICIT NONE

! Arguments

CHARACTER(LEN=*),INTENT(IN) :: filename     ! dsn or ddname
CHARACTER(LEN=*),INTENT(IN) :: mode         ! 'DSN' or 'DDN'

! Local Variables

CHARACTER(LEN=LEN_TRIM(FILENAME)+4)  :: file    ! file to be tested
INTEGER                              :: irc     ! IO return code
INTEGER                              :: i       ! name length

inquire = .FALSE.


#if defined (MVS)

i=INDEX(filename,' ')
IF (i == 0 )THEN
  i=LEN(filename)
ELSE
  i=i-1
END IF

IF ( mode == 'DSN') THEN
    file="//'"//filename(1:i)//"'"
ELSE IF(mode == 'DDN') THEN
    file="DD:"//filename(1:i)
ELSE
  WRITE(6,*)'INQUIRE: Invalid MODE ',mode
  RETURN
END IF

#else

file=filename

#endif

! Attempt to open the file
#if defined (MVS)

OPEN(81,FILE=file,ACTION='READ',IOSTAT=irc)

#else

OPEN(81,FILE=file,IOSTAT=irc)

#endif

IF (irc == 0 )THEN
  inquire = .TRUE.
  CLOSE(81)
END IF

RETURN
END FUNCTION INQUIRE
