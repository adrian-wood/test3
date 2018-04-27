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
! PARAMETERS   : CREQ     O   (CHARACTER*(*)) Modified copy of user's
!                               MDB request string.
!                LENGTH   O   Length of modified MDB request string.
!                REQUEST  I   (CHARACTER*(*)) User's original MDB
!                               request string.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:04$
! $Source: /data/us0400/mdb/op/lib/source/RCS/delspce.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:04    Sheila Needham  
! $
! Revision 2.1  2003/03/05  16:22:56  16:22:56  usmdb (MetDB account c/o usjh)
! 2.1.  17 March 2003.  Brian Barwell.  Change 29/03.
! Completely rewritten. Now ignores binary zeroes.
! 
! Revision 2.0  2001/01/08  11:58:36  11:58:36  usmdb (Generic MDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.3  97/08/04  13:07:12  13:07:12  uspm (Pat McCormack)
! First revisioned version for  1  - with Y2K changes
!
! Revision 1.2  1997/02/12 12:27:31  uspm
! Update dir name in variable head
!
! Revision 1.1  1997/02/11 13:45:03  uspm
! Initial revision
!
! 05/07/95    : ADD ADDITIONAL PARAMETER 'REQUEST' TO CALL AND
!               UPDATE SUBROUTINE TO '95 TEAM STANDARDS.       A.M
!
!---------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!                                                             Variables

      INTEGER IPOS           ! Position in original request string
      INTEGER LENGTH         ! Length of modified request string
      INTEGER LENOLD         ! Length of original request string
!
      LOGICAL FIRST          ! .TRUE. if first call to subroutine
      LOGICAL SPACE          ! .TRUE. if last character was a space

      CHARACTER*(*) CREQ     ! Modified copy of user's request string
      CHARACTER*132 HEAD     ! For revision information
      CHARACTER*(*) REQUEST  ! User's original request string

!                                                   Data initialisation
      DATA FIRST/.TRUE./
!                                                       Saved variables
      SAVE FIRST
!                                Revision information (first call only)
      IF (FIRST) THEN
        HEAD = '$RCSfile: $ ' //
     &  '$Revision: 1$ $Date: 30/01/2006 20:22:04$'
        FIRST = .FALSE.
      END IF
!                                                       Initialisations
      LENOLD = LEN(REQUEST)  ! Length of input string
      CREQ = ' '             ! Set output to all spaces
      LENGTH = 0             ! Nothing in output string yet
      SPACE = .FALSE.        ! No space found yet

!                                  Loop over characters in input string
      DO IPOS=1,LENOLD
!                           Check for alphanumeric character or a space
!                           immediately after an alphanumeric character

        IF (REQUEST(IPOS:IPOS).GT.' ' .OR.
     &     (REQUEST(IPOS:IPOS).EQ.' ' .AND. .NOT.SPACE)) THEN

!                            Copy the character; note if it was a space
          LENGTH = LENGTH + 1
          CREQ(LENGTH:LENGTH) = REQUEST(IPOS:IPOS)
          SPACE = REQUEST(IPOS:IPOS).EQ.' '
        END IF
      END DO
!                                         Ensure string ends with space
      IF (.NOT.SPACE) LENGTH = LENGTH + 1
!                                             Return to calling program
      RETURN
      END
