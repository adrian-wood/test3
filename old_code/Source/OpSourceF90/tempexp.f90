REAL FUNCTION TEMPEXP(REPORT)  ! Temperature in Kelvin.

!-----------------------------------------------------------------------
!
! PROGRAM       : TEMPEXP  IN TFMRET
!
! PURPOSE       : EXPAND TEMPERATURE DATA FROM AN NCM REPORT INTO
!                 A REAL VALUE.
!
! DESCRIPTION   : CONVERTS A 4 CHARACTER DATA STRING INTO A REAL VALUE
!                 AND CONVERTS THE VALUE TO A TEMPERATURE IN KELVIN.
!
! DATA TYPE(S)  : NCM, ESAWS
!
! CALLED BY     : NCMEXP, ENHEXP
!
! CALLS         : NONE
!
! PARAMETERS    : (1) REPORT
!                 (2) TEMPEXP
!
!Y2K  26.06.1997  TEMPEXP is Year 2000 compliant.
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:59:19  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.4  97/10/24  12:44:39  12:44:39  usjl (Jon Lewthwaite)
! Additionaly called for use in expansion of esaws reports.
!
! Revision 1.3  1997/08/04 13:35:58  uspm
! First revisioned version for MVS - with Y2K change
!
! Revision 1.2  1997/02/20 13:46:13  uspm
! Latest version (!a) from MVS
!
! Revision 1.1  1997/02/17 11:58:30  uspm
! Initial revision
!
! 06/10/97   Additionaly called for use in expansion of esaws
!            reports. John Norton
!
! 29/01/97   Conversion factor changed from 273.2 to 273.1            !a
!            John Norton
!
! FEB 96     INTRODUCED IN LINE WITH NCM RETRIEVAL.
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

! Declare variables

CHARACTER*4   REPORT           ! Data passed for conversion.

INTEGER       POS              ! Position in data string
                               ! being checked.
INTEGER       SIGN             ! Qualifier for positive or
                               ! negative temperature.

REAL          TEMP             ! Real value of character data.

LOGICAL       CHECK            ! Set if all characters in the data
                               ! string are numeric.
CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/tempexp.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

! Initialise variables

CHECK=.TRUE.
POS=1

! Check numeric content of data string.

DO POS=1,4
  IF (REPORT(POS:POS) .LT. '0' .OR. REPORT(POS:POS) .GT. '9') THEN
    CHECK=.FALSE.
  ENDIF
END DO

! Convert value to temperature in Kelvin checking for negative or
! positive values.

IF (CHECK) THEN
  READ (REPORT,'(I1,F3.1)') SIGN,TEMP
  IF (SIGN .EQ. 1) THEN
    TEMP=-TEMP
  ENDIF
  TEMPEXP=TEMP+273.1                                         !a
ELSE
  TEMPEXP=-9999999
ENDIF

RETURN
END FUNCTION TEMPEXP
