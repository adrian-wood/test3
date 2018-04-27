SUBROUTINE NMSTEM(CNAME,CSTEM,CINST)

!-----------------------------------------------------------------------
! SUBROUTINE    : NMSTEM
!
! PURPOSE       : SPLITS A FULL NAME INTO THE ELEMENT NAME AND ITS
!                 INSTANTIATION.
!
! DESCRIPTION   : E.G. ELEM_NAME_1 BECOMES ELEM_NAME AND _1
!
! CALLED BY     : MAPELM, ALSELM
!
! CALLS         : NOTHING
!
! ARGUMENTS     : INPUT  : CNAME    CHAR*36 FULL NAME
!                 OUTPUT : CSTEM    CHAR*32 ELEMENT NAME (STEM)
!                 OUTPUT : CINST    CHAR*6 INSTANTIATION
!
!Y2K  26.06.1997  NMSTEM IS YEAR 2000 COMPLIANT.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/nmstem.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:59:02  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:17:29  13:17:29  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
!
! Revision 1.1  1997/02/11 16:45:22  uspm
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
CHARACTER*(*)  CNAME,CSTEM,CINST

CHARACTER*6 BLANK
LOGICAL INTCON
INTEGER I, I1

CHARACTER*132      HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/nmstem.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

BLANK=' '
I1=1
5     CONTINUE
I=INDEX(CNAME(I1:),'_')
IF(I.NE.0)THEN   ! UNDERLINE FOUND
  IF(INTCON(CNAME,I1+I,I1+I))THEN
    CSTEM=CNAME(1:I1+I-2)
    CINST=CNAME(I1+I-1: )
  ELSE
    I1=I1 + I
    GOTO 5
  ENDIF
ELSE  ! NO UNDERSCORES FOUND
  CSTEM=CNAME
  CINST=BLANK
ENDIF

RETURN
END SUBROUTINE NMSTEM
