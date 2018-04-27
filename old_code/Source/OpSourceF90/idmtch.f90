SUBROUTINE IDMTCH(ENTRY,ID,IDLIST,LFLAG,RC)                   !2.0

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
!Y2K  26.06.1997  IDMTCH IS YEAR 2000 COMPLIANT.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/idmtch.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:58:46  usmdb
! Replaced ALTERNATE RETURN with a return code. Added copyright
! and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:11:58  13:11:58  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
!
! Revision 1.1  1997/02/11 14:52:03  uspm
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

CHARACTER*9 ID,IDLIST(*)
CHARACTER*(*) ENTRY
LOGICAL LFLAG
INTEGER       J1, IPOS
INTEGER RC                                                    !2.0

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/idmtch.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

ID=ENTRY(3:11)
IF(IDLIST(1)(1:5).EQ.'00000')THEN
  IF(LFLAG)PRINT*,' ALL IDENTIFIERS WANTED'
  RC=0                                                        !2.0
  RETURN
ELSE              ! LOOP OVER LIST TERMINATED BY '00000'
  DO 100 J1=1,50
  IF(IDLIST(J1)(1:5).EQ.'00000')THEN  ! END OF LIST WITH NO MATCH
    IF(LFLAG)PRINT*,' NO MATCH '
    RC=1                                                      !2.0
    RETURN                                                    !2.0
  ELSE

! CHECK IDENT IN ENTRY AGAINST AS MANY CHARACTERS AS ARE SET (UP TO 9)
! IN THIS ITEM IN THE LIST

    IPOS=INDEX(IDLIST(J1),' ')
    IF(IPOS.EQ.0)IPOS=10
    IF(ID(1:IPOS-1).EQ.IDLIST(J1)(1:IPOS-1))THEN
      IF(LFLAG)PRINT*,' WANT THIS ONE ',IDLIST(J1),ID
      RC=0                                                    !2.0
      RETURN
    ENDIF
  ENDIF
100     CONTINUE
ENDIF
IF(LFLAG)PRINT*,' NO MATCH '

RC=1                                                          !2.0
RETURN          ! IF 50 IDENTS IN LIST & NONE FOUND           !2.0
END SUBROUTINE IDMTCH
