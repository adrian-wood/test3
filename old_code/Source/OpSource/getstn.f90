SUBROUTINE GETSTN(REQ,IPOS,ILEN,ISTN,INUM,IDLEN,ISTYP, &
                     IFAIL,CERR)

!-----------------------------------------------------------------------
!
! PROGRAM       : GETSTN IN MDB
!
! PURPOSE       : TO GET INTEGER STATION IDENTIFIERS FROM USERS REQUEST
!
! DESCRIPTION   : COPES WITH IDENTIFIERS LENGTH 2,4,5 AND 6
!                 I.E. NN     WMO BLOCK OR DCNN
!                      NNNNN  WMO BLK/STN
!                      NNNN   DCNN
!                      NNNNNN RAINFALL STN
!      29/11/93   AND ALSO WITH SPECIAL WORDS SURFACE AND UPAIR
!
! CALLED BY     : GETREQ IN MDB
!
! CALLS         : NXTKEY
!
! PARAMETERS    : (1)REQ    USERS REQUEST
!               : (2)IPOS   POINTING TO FIRST GROUP ON ENTRY, TO NEXT
!                           KEYWORD (OR ILEN) ON EXIT
!                 (3)ILEN   LENGTH OF REQUEST
!                 (4)ISTN(*)    LIST OF STATIONS
!                 (5)INUM MAX NO IN LIST ON ENTRY, ACTUAL NO ON EXIT
!                 (6)IDLEN  LENGTH OF GROUPS (-1 FOR MIXED LENGTHS)
!                 (7)ISTYP  1 SUFRACE, 2 UPAIR (FOR STNMAS)
!                 (8)IFAIL    8 IF ERROR DETECTED
!                 (9)CERR     ERROR MESSAGE
!
!Y2K  26.06.1997  GETSTN IS YEAR 2000 COMPLIANT.
!
! REVISION INFO :
!
! $Workfile: getstn.f90$ $Folder: OpSource$
! $Revision: 7$ $Date: 20/12/2010 12:42:31$
!
! CHANGE RECORD :
!
! $Log:
!  7    MetDB_Refresh 1.6         20/12/2010 12:42:31    Sheila Needham
!       Initialise IFAIL and CERR
!  6    MetDB_Refresh 1.5         22/11/2010 16:47:25    Stan Kellett
!       function declaration not needed as declared in mod file
!  5    MetDB_Refresh 1.4         17/11/2010 15:52:55    John Norton     After
!       merge batch 8 changes
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:16:08    John Norton     
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

USE nxtkey_mod

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(INOUT)     ::  REQ
INTEGER, INTENT(INOUT)          ::  IPOS
INTEGER, INTENT(INOUT)          ::  ILEN
INTEGER, INTENT(INOUT)          ::  ISTN(:)
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

LOGICAL      ::  INTCON
LOGICAL      ::  OCHK

CHARACTER(4) ::  CFMT

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

CFMT='(I1)'
COUNT=0
IDLEN=0
IFAIL = 0
CERR = ''

! CHECK WE'RE NOT LOOKING AT A KEYWORD

10    CONTINUE
IFLABEL1: &
IF(.NOT.NXTKEY(REQ,IPOS,ILEN))THEN
! FIND THE LENGTH OF THIS GROUP
  I=INDEX(REQ(IPOS:ILEN),' ')
  IF(I == 0)THEN
    GRPLEN=ILEN-IPOS+1
  ELSE
    GRPLEN=I-1
  END IF
! CHECK FOR SPECIAL WORDS
  IF (GRPLEN >= 7) THEN
    IF(REQ(IPOS:IPOS+6) == 'SURFACE')THEN
      ISTYP=1
      IPOS=IPOS+GRPLEN+1
      GOTO 997
    END IF
  END IF

  IF (GRPLEN >= 5 ) THEN
    IF(REQ(IPOS:IPOS+4) == 'UPAIR')THEN
      ISTYP=2
      IPOS=IPOS+GRPLEN+1
      GOTO 997
    ENDIF
  END IF
! SET LENGTH OF ALL GROUPS FOR RETURN
  IF(IDLEN /= -1)THEN
    IF(IDLEN == 0)THEN  ! FIRST TIME THROUGH
      IDLEN=GRPLEN
    ELSE IF(IDLEN /= GRPLEN)THEN
      IDLEN=-1
    END IF
  END IF
! CHECK FOR VALID INTEGERS THROUGHOUT THE GROUP
  OCHK=INTCON(REQ,IPOS,IPOS+GRPLEN-1)
  IF(.NOT.OCHK)THEN
    CERR=':NON-DIGITS'
    IFAIL=8
    GOTO 999
  END IF
  COUNT=COUNT+1
  IF(COUNT <= INUM)THEN
    CFMT='(I1)'
    WRITE(CFMT(3:3),'(I1)')GRPLEN
    READ(REQ(IPOS:IPOS+GRPLEN-1),CFMT)ISTN(COUNT)
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
END SUBROUTINE GETSTN
