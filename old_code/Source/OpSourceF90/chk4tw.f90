SUBROUTINE CHK4TW(IDESC,IREPL,NDES,WANTEMP,WANTWND,USRLEVT,&
&USRLEVW,LFLAG)

!-----------------------------------------------------------------------
!
! subroutine    : CHK4TW
!
!               : ANSI standard except for IMPLICIT NONE and '!' used
!               : for comments
!
! purpose       : To look through the users requested upper-air elements
!               : to see if they want temperatures and/or winds, and if
!               : so how many levels they have allowed for.
!
! description   : This routine uses the element numbers as defined in
!               : the data dictionary for temperature, dew-point, wind
!               : direction and wind speed.
!
! data type(s)  : TEMP, PILOT, DROPSOND
!
! called by     : UPRRET
!
! sub calls     : None
!
! arguments     : IDESC    (ip)  : array of element numbers requested
!               : IREPL    (ip)  : levels corresponding to IDESC
!               : NDES     (ip)  : no of elements requested
!               : WANTEMP  (op)  : true if temperatures wanted
!               : WANTWND  (op)  : True if winds wanted
!               : USRLEVT  (op)  : max no of temp levels allowed for
!               : USRLEVW  (op)  : max no of wind levels allowed for
!               : LFLAG    (ip)  : true for diagnostics
!
!Y2K  26.06.1997  CHK4TW is Year 2000 compliant.
!
! revision info :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/chk4tw.F,v $
!
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:58:30  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.3  97/09/22  10:48:52  10:48:52  uspm (Pat McCormack)
! Change order of variable type definitions to satisfy F90 NAG compiler
!
! Revision 1.2  1997/08/04 12:54:21  uspm
! First revisioned version for COSMOS - with Y2K changes
!
! Revision 1.1  1997/02/20 08:53:56  uspm
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

!-----------------------------------------------------------------------
! Declare argument types - variables used as dimensions first
!-----------------------------------------------------------------------

INTEGER NDES

INTEGER IDESC(NDES)
INTEGER IREPL(NDES)
INTEGER USRLEVT
INTEGER USRLEVW
LOGICAL LFLAG
LOGICAL WANTEMP
LOGICAL WANTWND

!-----------------------------------------------------------------------
! Declare local variables
!-----------------------------------------------------------------------

INTEGER J

CHARACTER*132 HEAD

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/chk4tw.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

WANTEMP=.FALSE.
WANTWND=.FALSE.
USRLEVT=0
USRLEVW=0

DO J=1,NDES
  IF(IDESC(J).EQ.24.OR.IDESC(J).EQ.25)THEN
    WANTEMP=.TRUE.
    IF(IREPL(J).GT.USRLEVT) USRLEVT=IREPL(J)
  ELSEIF(IDESC(J).EQ.26.OR.IDESC(J).EQ.27)THEN
    WANTWND=.TRUE.
    IF(IREPL(J).GT.USRLEVW) USRLEVW=IREPL(J)
  ENDIF
END DO

IF(LFLAG)THEN
  PRINT*,'IN CHK4TW: WANTEMP =',WANTEMP,' LEVELS=',USRLEVT
  PRINT*,'           WANTWND =',WANTWND,' LEVELS=',USRLEVW
ENDIF

RETURN
END SUBROUTINE CHK4TW
