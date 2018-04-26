SUBROUTINE DELSPCE (CREQ, LENGTH, REQUEST)

!----------------------------------------------------------------------
! SUBROUTINE   : DELSPCE
!
! PURPOSE      : Removes multiple spaces (and other characters such
!                as binary zeroes) from user's MDB request string.
!                Also returns the length of the modified string.
!
! CALLED BY    : MDB
!
! USAGE        : CALL DELSPCE (CREQ, LENGTH, REQUEST)
!
! ARGUMENTS    : CREQ     O   (CHARACTER*(*)) Modified copy of user's
!                               MDB request string.
!                LENGTH   O   Length of modified MDB request string.
!                REQUEST  I   (CHARACTER*(*)) User's original MDB
!                               request string.
!
! REVISION INFO :
!
! $Revision: 6$
! $Date: 01/11/2010 15:07:07$
! $Source: /data/us0400/mdb/op/lib/source/RCS/delspce.f,v $
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
!---------------------------------------------------------------------
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
CHARACTER(LEN=*), INTENT(INOUT)  :: CREQ     ! Modified copy of user's request string
INTEGER, INTENT(INOUT)           :: LENGTH   ! Length of modified request string
CHARACTER(LEN=*), INTENT(INOUT)  :: REQUEST  ! User's original request string

! Local variables
INTEGER                          :: IPOS     ! Position in original request string
INTEGER                          :: LENOLD   ! Length of original request string
!
LOGICAL                          :: SPACE    ! .TRUE. if last character was a space
!                                                       Initialisations
LENOLD = LEN(REQUEST)  ! Length of input string
CREQ = ' '             ! Set output to all spaces
LENGTH = 0             ! Nothing in output string yet
SPACE = .FALSE.        ! No space found yet

!                                  Loop over characters in input string
CHARLOOP: &
DO IPOS=1,LENOLD
!                           Check for alphanumeric character or a space
!                           immediately after an alphanumeric character

  IF (REQUEST(IPOS:IPOS) >  ' ' .OR.                              &
     (REQUEST(IPOS:IPOS) == ' ' .AND. .NOT.SPACE)) THEN

!                            Copy the character; note if it was a space
    LENGTH = LENGTH + 1
    CREQ(LENGTH:LENGTH) = REQUEST(IPOS:IPOS)
    SPACE = REQUEST(IPOS:IPOS) == ' '
  END IF
END DO CHARLOOP
!                                         Ensure string ends with space
IF (.NOT.SPACE) LENGTH = LENGTH + 1
!                                             Return to calling program
RETURN
END SUBROUTINE DELSPCE
