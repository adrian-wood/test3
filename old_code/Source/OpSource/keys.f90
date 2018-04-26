SUBROUTINE KEYS(WORDS)

!-----------------------------------------------------------------------
! ROUTINE       : KEYS
!
! PURPOSE       : Initialises an array of MDB keywords.
!
! DESCRIPTION   : The array 'WORDS' is simply initiated with all
!                 known key words.
!
! CALLED BY     : EXPELM
!
! CALLS         : None
!
! ARGUMENTS     : input  : none
!               : output : CHARACTER*9 WORDS(*)
!               : in/out : none
!
! REVISION INFO :
!
!
! $Workfile: keys.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 12/11/2010 17:13:17$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         12/11/2010 17:13:17    Rosemary Lavery remove
!        old header
!  4    MetDB_Refresh 1.3         04/11/2010 15:41:00    Rosemary Lavery
!       Corrections after review 
!  3    MetDB_Refresh 1.2         04/11/2010 13:40:32    Rosemary Lavery
!       correction ahead of review
!  2    MetDB_Refresh 1.1         13/10/2010 13:33:29    Rosemary Lavery F90
!       amendments (before review)
!  1    MetDB_Refresh 1.0         12/10/2010 17:34:46    Rosemary Lavery
!       Initial F90 conversion
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

IMPLICIT NONE

CHARACTER (LEN=36), INTENT(OUT)  ::  WORDS(:)

SAVE

WORDS(1)  = 'START'
WORDS(2)  = 'END'
WORDS(3)  = 'LATEST'
WORDS(4)  = 'INCREMENT'
WORDS(5)  = 'RECEIVED'
WORDS(6)  = 'PLATFORM'
WORDS(7)  = 'AREA'
WORDS(8)  = 'OVER'
WORDS(9)  = 'VERSION'
WORDS(10) = 'DATA'
WORDS(11) = 'ELEMENTS'
WORDS(12) = 'SATELLITE'
WORDS(13) = 'BUOY'
WORDS(14) = 'AIRCRAFT'
WORDS(15) = 'STATION'
WORDS(16) = 'WMO'
WORDS(17) = 'ICAO'
WORDS(18) = 'DCNN'
WORDS(19) = 'RAIN'
WORDS(20) = 'ORDER'
WORDS(21) = 'TEST'
WORDS(22) = 'MESSAGE'
WORDS(23) = 'STANDARD'
WORDS(24) = 'SIGNIFICANT'
WORDS(25) = 'FIXED'
WORDS(26) = 'MOBILE'
WORDS(27) = 'PROCESSED'
WORDS(28) = 'COMBINED'
WORDS(29) = 'DDICT'
WORDS(30) = 'MODEL'
WORDS(31) = 'RPOLE'
WORDS(32) = 'RETBUFR'
WORDS(33) = 'SELECT'

RETURN
END SUBROUTINE KEYS
