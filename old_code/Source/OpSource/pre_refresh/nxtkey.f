      LOGICAL FUNCTION NXTKEY(REQ,IPOS,ILEN)

!-----------------------------------------------------------------------
!
! FUNCTION      : NXTKEY
!
! PURPOSE       : TRUE if the current group is a keyword
!
! CALLED BY     : GETSTN
!                 GETSTR
!
! ARGUMENTS     : (All input)
!                  1. User's MDB retrieval request string
!                  2. Pointer to current position in request string
!                  3. Length of request string
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 25/04/2006 15:39:03$
! $Author: Brian Barwell$
! $Folder: pre_refresh$
! $Workfile: nxtkey.f$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         25/04/2006 15:39:03    Brian Barwell   Add
!       'RETG' (for RETGRIB) to list of recognised keywords.
!  1    Met_DB_Project 1.0         30/01/2006 20:23:47    Sheila Needham  
! $
!
! Revision 2.3  2003/03/06  09:11:41  09:11:41  usmdb (MetDB account c/o usjh)
! Added 'SELE' for 'SELECT' keyword to list of keywords. Increase
! number of keywords to 32 - S.Cox
!
! Revision 2.2  2002/11/04  14:32:25  14:32:25  usmdb (MetDB account c/o usjh)
! Add DDIC for 'DDICT' to the list of keywords. Increase number
! of keywords to 31 - S.Cox
!
! Revision 2.1  2002/09/04  14:00:23  14:00:23  usmdb (Generic MetDB account)
! Added 'RETB' to KEY list. Removed duplicate 'WMO ' keyword.
! Changed revision information section and replaced GOTO 999 with
! RETURN - S.Cox
!
! Revision 2.0  2001/01/08  11:59:02  11:59:02  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:17:40  13:17:40  uspm (Pat McCormack)
! First revisioned version for 1  - with Y2K change
!
! Revision 1.1  1997/02/11 16:46:10  uspm
! Initial revision
!
! 23-11-96  !B  : S.Cox - Addition of keyword COMBINED
! 23-09-96  !A  : S.Cox - Addition of keywords for Upper-Air retrieval
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2006 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER        NKEYS
      PARAMETER      (NKEYS=33)    !- number of keywords              !2

      INTEGER        IPOS          !- position in request string
      INTEGER        ILEN          !- length of request string
      INTEGER        J             !- general loop counter
      LOGICAL        HEADSET       !- true if HEAD set              !2.1
      CHARACTER*(*)  REQ           !- request string
      CHARACTER*4    KEY(NKEYS)    !- array of keywords
      CHARACTER*80   HEAD          !- revision information            !2

      DATA HEADSET/.FALSE./                                         !2.1

      DATA KEY/'STAR','END ','LATE','INCR','DDIC',                  !2.2
     &         'RECE','PLAT','AREA','OVER','VERS',
     &         'DATA','ELEM','SATE','BUOY','SELE',                  !2.3
     &         'AIRC','STAT','WMO ','RETB','RETG',                    !2
     &         'ICAO','DCNN','RAIN','LATE','ORDE','TEST','MESS',
     &         'STAN','SIGN','FIXE','MOBI','PROC','COMB'/           !2.1

      IF (.NOT.HEADSET) THEN                                        !2.1
        HEAD='$Workfile: nxtkey.f$ ' //
     &       '$Revision: 2$ $Date: 25/04/2006 15:39:03$'
        HEADSET=.TRUE.                                              !2.1
      ENDIF                                                         !2.1

      NXTKEY=.FALSE.                                                !2.1
      IF ((ILEN-IPOS+1).LT.4) RETURN                                !2.1

      DO J=1,NKEYS                                                  !2.1
        IF (REQ(IPOS:IPOS+3).EQ.KEY(J)) THEN                        !2.1
          NXTKEY=.TRUE.                                             !2.1
          RETURN                                                    !2.1
        ENDIF                                                       !2.1
      ENDDO                                                         !2.1

      RETURN                                                        !2.1
      END
