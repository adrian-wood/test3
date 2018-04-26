SUBROUTINE GETKEY(REQ,IPOS,ILEN,KEY,IFAIL)

!-----------------------------------------------------------------------
!
! ROUTINE       : GETKEY
!
! PURPOSE       : Identify the next keyword in the request.
!
! DESCRIPTION   : Return IFAIL=8 and set KEY=' ' if keyword not found
!               : reset IPOS to point to the charcters after the key-
!               : word or to the next word or to ilen if the end of
!               : the request is reached.
!               : (a word is defined as a group of characters with a
!               : space before and after)
!
! CALLED BY     : GETREQ
!
! CALLS         : Nothing.
!
! ARGUMENTS     : (1) REQ    character request string from the start
!               : (2) IPOS   points to first character of keyword to be
!               :            identified on entry.
!               : (3) ILEN   length of request
!               : (4) KEY    keyword returned
!               : (5) IFAIL  set to 8 if keyword not found
!
! REVISION INFO :
!
! $Workfile: getkey.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 17/11/2010 15:52:55$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         17/11/2010 15:52:55    John Norton     After
!       merge batch 8 changes
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:15:19    John Norton     
!  1    MetDB_Refresh 1.0         13/10/2010 16:00:38    John Norton     f77
!       code copied out of StarTeam
!       MIDE\Met_DB_Project\Development\Source\OpSource
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

! Use statements:
! <Interfaces>

!None

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(INOUT)     ::  REQ !- users request string
INTEGER, INTENT(INOUT)          ::  IPOS !- position in string REQ
INTEGER, INTENT(INOUT)          ::  ILEN !- length of REQ
CHARACTER(*), INTENT(INOUT)     ::  KEY !- individual keyword selecte   d
INTEGER, INTENT(INOUT)          ::  IFAIL !- error status

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!-----------------------------------------------------------------------
! parameter statements
!-----------------------------------------------------------------------

INTEGER, PARAMETER          ::  NKEYS = 35

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

INTEGER      ::  I             !- general loop counter
INTEGER      ::  J             !- general loop counter
INTEGER      ::  WORDLEN(NKEYS) !- keyword name lengths


CHARACTER(15)  ::  WORDS(NKEYS) !- keyword names

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
! save all variables
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! data statements
!-----------------------------------------------------------------------


DATA WORDS/'START TIME','END TIME','LATEST','INCREMENT', &
           'RECEIVED','PLATFORM','AREA','OVER','VERSION', &
           'DATA','ELEMENTS','SATELLITE ID','BUOY ID', &
           'AIRCRAFT ID','STATION NUMBER','WMO BLOCK', &
           'WMO ID','ICAO ID','DCNN ID','RAIN ID','LATEST', &
           'ORDER','TEST','MESSAGE','STANDARD','SIGNIFICANT', &
           'FIXED','MOBILE','PROCESSED','COMBINED', &
           'DDICT','MODEL','RPOLE','RETBUFR','SELECT'/

DATA WORDLEN/10,8,6,9,8,8,4,4,7,4,8,12,7,11,14,9,6,7,7,7,6,5, &
             4,7,8,11,5,6,9,8,5,5,5,7,6/

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

IFAIL=0
KEY=' '


!-----------------------------------------------------------------------
! loop over all possible keywords
!-----------------------------------------------------------------------

DOLABEL1: &
DO J=1,NKEYS
  IF ((IPOS+WORDLEN(J)-1)  <=  ILEN) THEN
    IF (REQ(IPOS:IPOS+WORDLEN(J)-1) == WORDS(J)(1:WORDLEN(J))) &
    THEN
!-----------------------------------------------------------------------
! keyword found, move past it onto start of next word and RETURN.
!-----------------------------------------------------------------------

      KEY=WORDS(J)
      IPOS=IPOS+WORDLEN(J)+1
      IF (IPOS > ILEN) IPOS=ILEN
      RETURN
    END IF
  END IF
END DO DOLABEL1 !- J loop

!-----------------------------------------------------------------------
!- keyword not found
!-----------------------------------------------------------------------

I=INDEX(REQ(IPOS:ILEN),' ')
IF (I == 0) THEN
  IFAIL=8
  IPOS=ILEN
ELSE
  IPOS=IPOS+I
  IF (IPOS > ILEN) IPOS=ILEN
END IF

RETURN
END SUBROUTINE GETKEY
