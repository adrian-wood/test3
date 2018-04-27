      FUNCTION ARTMCK(DATIME,SYS_YEAR)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM       : FUNCTION ARTMCK
!
! PURPOSE       : To check that an AIREP report has a date/time within
!                 the accepted range of +-1 Year and MONTH, DAY, HOUR
!                 and Mins are within the normal maximum range ie.
!                 1-12 for Months etc
!
! CALLED BY     : AIRENC                                            !1.2
!
! ARGUMENTS     : 1 DATIME - INTEGER ARRAY(5) Y,M,D,H,Mins - Input
!                 2 SYS_YEAR INTEGER Current System Year   - Input
!                 3 ARMTCK   LOGICAL .TRUE. if Date/Time valid - Output
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:56$
! $Source: /home/us0400/mdb/op/lib/source/RCS/artmck.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:56    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:28  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  2000/03/10  09:58:46  09:58:46  usmdb (Generic MetDB account)
! 20 March 2000     C Long
! 1.2  Correct logic - must never have been tested!
! 
! Revision 1.1  98/04/10  12:54:06  12:54:06  usmdb (Generic MDB account)
! Initial revision
!
! Written: 02/04/1998 - Jon Lewthwaite
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

!declare variables
      INTEGER SYS_YEAR
      INTEGER DATIME(5)
      CHARACTER HEAD*132
      LOGICAL ARTMCK

      SAVE
      ARTMCK=.FALSE.
      HEAD='$Source: /home/us0400/mdb/op/lib/source/RCS/artmck.F,v $
     &'//'$Date: 30/01/2006 20:20:56$ $Revision: 1$'

      IF (DATIME(1).GE.(SYS_YEAR-1)) THEN
        IF (DATIME(1).LE.(SYS_YEAR+1)) THEN
          IF (DATIME(2).GE.1.AND.DATIME(2).LE.12) THEN            !1.2
            IF (DATIME(3).GE.1.AND.DATIME(3).LE.31) THEN          !1.2
              IF (DATIME(4).GE.0.AND.DATIME(4).LE.23) THEN        !1.2
                IF (DATIME(5).GE.0.AND.DATIME(5).LE.59) THEN      !1.2
                  ARTMCK=.TRUE.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      RETURN
      END
