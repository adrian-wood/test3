SUBROUTINE GETSTR(REQ,IPOS,ILEN,CIDENT,INUM,IDLEN,ISTYP, &
                  IFAIL,CERR)

!-----------------------------------------------------------------------
!
! PROGRAM       : GETSTR IN MDB
!
! PURPOSE       : TO GET CHARACTER IDENTIFIERS FROM USERS REQUEST
!
! DESCRIPTION   : COPES WITH IDENTIFIERS OF VARIABLE LENGTH
!
! CALLED BY     : GETREQ IN MDB
!
! CALLS         : NXTKEY
!
! PARAMETERS    : (1)REQ    USERS REQUEST
!               : (2)IPOS   POINTING TO FIRST GROUP ON ENTRY, TO NEXT
!                           KEYWORD (OR ILEN) ON EXIT
!                 (3)ILEN   LENGTH OF REQUEST
!                 (4)CIDENT(*)  LIST OF IDENTIFIERS
!                 (5)INUM MAX NO IN LIST ON ENTRY, ACTUAL NO ON EXIT
!                 (6)IDLEN  LENGTH OF GROUPS (-1 FOR MIXED LENGTHS)
!                 (7)ISTYP  1 SURFACE 2 UPAIR (FOR STNMAS)
!                 (8)IFAIL    8 IF ERROR DETECTED
!                 (9)CERR     ERROR MESSAGE
!
!Y2K  26.06.1997  GETSTR IS YEAR 2000 COMPLIANT.
!
! REVISION INFO :
!
! $Revision: 9$
! $Date: 20/12/2010 12:44:04$
! $Source: /home/us0400/mdb/op/lib/source/RCS/getstr.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  9    MetDB_Refresh 1.8         20/12/2010 12:44:04    Sheila Needham
!       Initialise IFAIL and CERR
!  8    MetDB_Refresh 1.7         22/11/2010 16:51:03    Stan Kellett    use
!       nxtkey changed to use nxtkey_mod
!  7    MetDB_Refresh 1.6         22/11/2010 16:49:18    Stan Kellett
!       function declaration NXTKEY not needed as declared in mod file
!  6    MetDB_Refresh 1.5         18/11/2010 10:55:49    John Norton
!       Updated after merge batch 8 review.
!  5    MetDB_Refresh 1.4         17/11/2010 15:52:55    John Norton     After
!       merge batch 8 changes
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:16:20    John Norton     
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

use nxtkey_mod

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(INOUT)     ::  REQ
INTEGER, INTENT(INOUT)          ::  IPOS
INTEGER, INTENT(INOUT)          ::  ILEN
CHARACTER(*), INTENT(INOUT)     ::  CIDENT(:)
INTEGER, INTENT(INOUT)          ::  INUM
INTEGER, INTENT(INOUT)          ::  IDLEN
INTEGER, INTENT(INOUT)          ::  ISTYP
INTEGER, INTENT(INOUT)          ::  IFAIL
CHARACTER(*), INTENT(INOUT)     ::  CERR

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  COUNT
INTEGER      ::  GRPLEN
INTEGER      ::  I

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

COUNT=0
IDLEN=0
IFAIL = 0
CERR = ''
! CHECK WE'RE NOT LOOKING AT A KEYWORD
10 CONTINUE
IFLABEL1: &
IF(.NOT.NXTKEY(REQ,IPOS,ILEN))THEN
! FIND THE LENGTH OF THIS GROUP
  I=INDEX(REQ(IPOS:ILEN),' ')
  IF(I == 0)THEN
    GRPLEN=ILEN-IPOS+1
  ELSE
    GRPLEN=I-1
  END IF
  IF(REQ(IPOS:IPOS+6) == 'SURFACE')THEN
    ISTYP=1
    IPOS=IPOS+GRPLEN+1
    GOTO 997
  ELSE IF(REQ(IPOS:IPOS+4) == 'UPAIR')THEN
    ISTYP=2
    IPOS=IPOS+GRPLEN+1
    GOTO 997
  END IF
! SET LENGTH OF ALL GROUPS FOR RETURN
  IF(IDLEN /= -1)THEN
    IF(IDLEN == 0)THEN  ! FIRST TIME THROUGH
      IDLEN=GRPLEN
    ELSE IF(IDLEN /= GRPLEN)THEN
      IDLEN=-1
    END IF
  END IF
! INCREMENT COUNT AND TRANSFER TO CIDENT
  COUNT=COUNT+1
  IF(COUNT <= INUM)THEN
    CIDENT(COUNT)=REQ(IPOS:IPOS+GRPLEN-1)
    IPOS=IPOS+GRPLEN+1
  ELSE
    CERR=':LIST TOO LONG'
    IFAIL=8
    GOTO 999
  END IF
ELSE ! END OF LIST ENCOUNTERED
  GOTO 998
END IF IFLABEL1
997   IF(IPOS < ILEN)GOTO 10   ! GO BACK FOR NEXT GROUP
998   INUM=COUNT
999   RETURN
END SUBROUTINE GETSTR
