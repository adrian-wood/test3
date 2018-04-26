LOGICAL FUNCTION INTCON(REQUEST,IPOS,IPOS2)

!-----------------------------------------------------------------------
!
! PROGRAM       : INTCON
!
! PURPOSE       : TO CHECK TO SEE IF YOU CAN CONVERT SPECIFIED
!                 CHARACTERS INTO INTEGERS
!
! DESCRIPTION   : A FUNCTION THAT LOOKS AT A SET OF CHARACTERS TO SEE
!                 IF THEY CAN BE CONVERTED INTO INTEGERS
!
! CALLED BY     : MDB,VALDAT,VALARE
!
! CALLS         : NONE
!
! PARAMETERS    : (1) REQUEST   USERS REQUEST STRING
!                 (2) IPOS      STARTING POSITION IN STRING
!                 (3) IPOS2     END POSITION IN STRING
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/intcon.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2003/01/06 16:21:06  usmdb
! Replace CHARACTER*500 REQUEST with CHARACTER*(*) REQUEST
! and improved readability of code - S.Cox
!
! Revision 2.0  2001/01/08 11:58:48  usmdb
! Added copyright & modified header - S.Cox
!
! Revision 1.1  97/08/06  07:50:30  07:50:30  uspm (Pat McCormack)
! Initial revision
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

INTEGER       IPOS,IPOS2, I
CHARACTER*132 HEAD
CHARACTER*(*) REQUEST                                         !2.1

HEAD='$RCSfile: intcon.f,v $ ' //&
    &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'

INTCON=.TRUE.
DO I=IPOS,IPOS2                                               !2.1
  IF ((REQUEST(I:I).LT.'0').OR. (REQUEST(I:I).GT.'9')) THEN
    INTCON=.FALSE.
  ENDIF
ENDDO                                                         !2.1

RETURN
END FUNCTION INTCON
