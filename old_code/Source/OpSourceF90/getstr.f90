SUBROUTINE GETSTR(REQ,IPOS,ILEN,CIDENT,INUM,IDLEN,ISTYP,&
&IFAIL,CERR)

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
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/getstr.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:58:42  usmdb
! Added copyright and modified header & comments - S.Cox
!
! Revision 1.2  97/08/04  13:11:28  13:11:28  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
!
! Revision 1.1  1997/02/12 12:38:29  uspm
! Initial revision
!
! 29/11/93    LOOK FOR WORDS SURFACE OR UPAIR AT THE END OF THE LIST
!             OF IDENTIFIERS
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
CHARACTER*(*) REQ,CERR,CIDENT(*)
INTEGER IPOS,ILEN,INUM,IDLEN,IFAIL
INTEGER I, ISTYP

LOGICAL NXTKEY
INTEGER COUNT,GRPLEN

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/getstr.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

COUNT=0
IDLEN=0
! CHECK WE'RE NOT LOOKING AT A KEYWORD
10    IF(.NOT.NXTKEY(REQ,IPOS,ILEN))THEN
! FIND THE LENGTH OF THIS GROUP
  I=INDEX(REQ(IPOS:ILEN),' ')
  IF(I.EQ.0)THEN
    GRPLEN=ILEN-IPOS+1
  ELSE
    GRPLEN=I-1
  ENDIF
  IF(REQ(IPOS:IPOS+6).EQ.'SURFACE')THEN
    ISTYP=1
    IPOS=IPOS+GRPLEN+1
    GOTO 997
  ELSEIF(REQ(IPOS:IPOS+4).EQ.'UPAIR')THEN
    ISTYP=2
    IPOS=IPOS+GRPLEN+1
    GOTO 997
  ENDIF
! SET LENGTH OF ALL GROUPS FOR RETURN
  IF(IDLEN.NE.-1)THEN
    IF(IDLEN.EQ.0)THEN  ! FIRST TIME THROUGH
      IDLEN=GRPLEN
    ELSEIF(IDLEN.NE.GRPLEN)THEN
      IDLEN=-1
    ENDIF
  ENDIF
! INCREMENT COUNT AND TRANSFER TO CIDENT
  COUNT=COUNT+1
  IF(COUNT.LE.INUM)THEN
    CIDENT(COUNT)=REQ(IPOS:IPOS+GRPLEN-1)
    IPOS=IPOS+GRPLEN+1
  ELSE
    CERR=':LIST TOO LONG'
    IFAIL=8
    GOTO 999
  ENDIF
ELSE ! END OF LIST ENCOUNTERED
  GOTO 998
ENDIF
997   IF(IPOS.LT.ILEN)GOTO 10  ! GO BACK FOR NEXT GROUP
998   INUM=COUNT
999   RETURN
END SUBROUTINE GETSTR
