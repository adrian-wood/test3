SUBROUTINE IDMTCH (ENTRY, ID, IDLIST, LFLAG, RC)

!-----------------------------------------------------------------------
!
! PROGRAM       : IDMTCH
!
! PURPOSE       : COMPARE A GIVEN IDENTIFIER WITH A LIST OF REQUESTED
!                 IDENTS.
!
! CALLED BY     : TFMRET, SYNRET, UPRRET
!
! PARAMETERS    : (1) ENTRY  CHAR*(*) INDEX ENTRY                 (I)
!               : (2) ID     CHAR*9   IDENTIFIER FROM ENTRY       (O)
!                 (3) IDLIST CHAR*9 ARRAY OF REQUIRED IDENTIFIERS (I)
!                 (4) LFLAG  LOGICAL  TRUE FOR EXTRA DIAGNOSTICS  (I)
!                 (5) RC     INTEGER RETURN CODE. 0 IF ID MATCHED (O)
!                                                 1 IF NO MATCH
!
! REVISION INFO :
!
! $Workfile: idmtch.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 19/10/2010 14:33:49$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         19/10/2010 14:33:49    Brian Barwell
!       Porting items 1-8, 12, 13 & removal of program change indicators done.
!  1    MetDB_Refresh 1.0         12/10/2010 16:26:43    Brian Barwell
!       Initial f77 version before porting to f90/95.
! $
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

CHARACTER*9 ID,IDLIST(:)
CHARACTER*(*) ENTRY
LOGICAL LFLAG
INTEGER J1, IPOS
INTEGER RC

ID=ENTRY(3:11)
IF(IDLIST(1)(1:5) == '00000')THEN
  IF(LFLAG)PRINT*,' ALL IDENTIFIERS WANTED'
  RC=0
  RETURN
ELSE              ! LOOP OVER LIST TERMINATED BY '00000'
  DO J1=1,50
    IF(IDLIST(J1)(1:5) == '00000')THEN ! END OF LIST WITH NO MATCH
      IF(LFLAG)PRINT*,' NO MATCH '
      RC=1
      RETURN
    ELSE

! CHECK IDENT IN ENTRY AGAINST AS MANY CHARACTERS AS ARE SET (UP TO 9)
! IN THIS ITEM IN THE LIST

      IPOS=INDEX(IDLIST(J1),' ')
      IF (IPOS == 0) IPOS=10
      IF(ID(1:IPOS-1) == IDLIST(J1)(1:IPOS-1))THEN
        IF(LFLAG)PRINT*,' WANT THIS ONE ',IDLIST(J1),ID
        RC=0
        RETURN
      END IF
    END IF
  END DO
END IF
IF(LFLAG)PRINT*,' NO MATCH '

RC=1
RETURN          ! IF 50 IDENTS IN LIST & NONE FOUND
END SUBROUTINE IDMTCH
