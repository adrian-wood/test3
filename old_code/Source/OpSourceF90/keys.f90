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
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /data/us0400/mdb/op/lib/source/RCS/keys.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2003/03/06  09:11:55  09:11:55  usmdb (MetDB account c/o usjh)
! Added 'SELECT' to WORDS array. Added HEADSET variable and SAVE
! statement - S.Cox
!
! Revision 2.0  2001/01/08  11:58:49  11:58:49  usmdb (MetDB account c/o usjh)
! Added copyright and modified header - S.Cox
!
! Revision 1.4  98/07/23  08:38:49  08:38:49  usmdb (Generic MDB account
! new keyword RETBUFR for retrieval of BUFR messages
!
! Revision 1.3  97/08/04  13:13:26  13:13:26  uspm (Pat McCormack)
! First revisioned version for 1  - with Y2K change
!
! Revision 1.2  1997/05/12 13:25:11  uspm
! Version dated 21-4-97 copied from 1
!
! Revision 1.1  1997/02/11 16:21:42  uspm
! Initial revision
!
! 20-07-98  !E  : Add new keyword RETBUFR for retrieval of BUFR
!               : messages - S.Cox
!
! 28-07-97  !D  : Add new keyword RPOLE for use with rotated lat/lon
!               : area - S.Cox
!
! 21-04-97  !C  : Added new keywords DDICT (for ddict name) and MODEL
!               : (for merged data source) - S.Cox
!
! 23-11-96  !B  : Added new keyword COMBINED for Upper-Air - S.Cox
!
! 26-09-96  !A  : Added 5 new keywords STANDARD, SIGNIFICANT, FIXED,
!               : MOBILE, PROCESSED for Upper-Air - S.Cox
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

LOGICAL       HEADSET                                         !2.1
CHARACTER*36  WORDS(*)
CHARACTER*132 HEAD

SAVE                                                          !2.1

DATA HEADSET/.FALSE./                                         !2.1

IF (.NOT.HEADSET) THEN                                        !2.1
  HEAD='$RCSfile: keys.f,v $ ' //&
      &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'
  HEADSET=.TRUE.                                              !2.1
ENDIF                                                         !2.1

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
WORDS(23) = 'STANDARD'                                          !A
WORDS(24) = 'SIGNIFICANT'                                       !A
WORDS(25) = 'FIXED'                                             !A
WORDS(26) = 'MOBILE'                                            !A
WORDS(27) = 'PROCESSED'                                         !A
WORDS(28) = 'COMBINED'                                          !B
WORDS(29) = 'DDICT'                                             !C
WORDS(30) = 'MODEL'                                             !C
WORDS(31) = 'RPOLE'                                             !D
WORDS(32) = 'RETBUFR'                                           !E
WORDS(33) = 'SELECT'                                          !2.1

RETURN
END SUBROUTINE KEYS
