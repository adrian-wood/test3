      SUBROUTINE TESSC3(REPORT,POS,EXPARR,POS1A,POS2A)

!-----------------------------------------------------------------------
!
! PROGRAM       : TESSC3
!
! PURPOSE       : To expand section 3 of TESAC (current profile)
!
! CALLED BY     : TESAC
!
! CALLS         : IVALUE   converts figures to numbers
!
! PARAMETERS    : REPORT   character string                       (I)
!                 POS      pointer to report                     (I/O)
!                 EXPARR   expansion array                        (O)
!                 POS1A    end of temp/salinity profile in array  (I)
!                 POS2A    end of current profile in array        (O)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:20$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tessc3.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:20    Sheila Needham  
! $
! Revision 2.1  2001/11/06 10:16:28  usmdb
! 19 Nov 2001    C Long
! 2.1  Reduce argument list, correct declarations,
!      tidy up in many ways.
!
! Revision 2.0  2001/07/03  10:44:17  10:44:17  usmdb (Generic MetDB account)
! Separated variable declaration and initialisation. Removed unused
! argument IERR. Added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  11:43:21  11:43:21  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.1  1997/07/04 14:29:48  uspm
! Initial revision
!
! AUG 96 - REMOVAL OF GOTO STATMENTS AND ADDITIONAL                   !B
!          CHECKING AGAINST REPORT LENGTH.
!
! JAN 96 - DIRECTION IS JUST IN TENS, NOT LIKE WIND!                  !A
!          CORRECT PROFILE LOOP
!
! INTRODUCED  :  22/08/94
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER*(*)  REPORT
      CHARACTER*132  HEAD
      REAL           EXPARR(0:*)
      REAL           MISING                                         !2.0
      REAL           DEPTH
      REAL           CDIR,CSPEED
      INTEGER        REPLEN
      INTEGER        POS
      INTEGER        POS1A
      INTEGER        POS2
      INTEGER        POS2A
      INTEGER        IVALUE
      INTEGER        NLEVELS         ! NUMBER OF DEPTH LAYERS
      INTEGER        K3,K4,K6
      LOGICAL        GARBLED

      DATA           MISING/-9999999./                              !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/tessc3.F,v $
     &'//'$ $Date: 30/01/2006 20:25:20$ $Revision: 1$'

! POS2 is where the count of levels will be stored in the value array;
! depth and current direction & speed subscripts are relative to POS2.

      REPLEN=LEN(REPORT)
      K3=MISING
      K4=MISING
      K6=MISING
      POS2=POS1A+7
      NLEVELS=0

! POS at start should point to 66 at start of current profile

      IF (POS+4.LE.REPLEN .AND. REPORT(POS:POS+1).EQ.'66') THEN

! 66 is followed by k6, k4 & k3   (see code tables 2267, 2265, 2264)
!   k6: how ship's velocity & motion is removed from measurements
!   k4: period of current measurement (1-9 correspond to BUFR 11-19)
!   k3: duration & time...

        K6=IVALUE(REPORT(POS+2:POS+2))

        K4=IVALUE(REPORT(POS+3:POS+3))
        IF (K4.NE.MISING) K4=K4+10

        K3=IVALUE(REPORT(POS+4:POS+4))

! Move POS to next group

        POS=POS+6

! Loop round depth & current groups (depth group marked 2) until
! one of the following is reached:
!   55555 at start of section 4
!   call sign (letters) or 99999 before buoy number in section 5

        GARBLED=.FALSE.
        DO WHILE(POS+4.LE.REPLEN .AND. .NOT.(
     &           REPORT(POS:POS+4).EQ.'55555' .OR.
     &           REPORT(POS:POS+4).EQ.'99999' .OR.
     &          (REPORT(POS:POS).GE.'A'.AND.REPORT(POS:POS).LE.'Z')))

          IF (.NOT.GARBLED .AND. REPORT(POS:POS).EQ.'2') THEN
            DEPTH=IVALUE(REPORT(POS+1:POS+4))
            POS=POS+6

! Convert direction from tens of degrees (direction towards, not from!)
! Convert speed from cm/s to m/s.

            IF (POS+4.LE.REPLEN) THEN
              CDIR=IVALUE(REPORT(POS:POS+1))
              CSPEED=IVALUE(REPORT(POS+2:POS+4))
              IF (CDIR.NE.MISING) CDIR=CDIR*10.
              IF (CSPEED.NE.MISING) CSPEED=CSPEED*0.01

              NLEVELS=NLEVELS+1
              EXPARR(POS2+(6*NLEVELS)-4)=DEPTH
              EXPARR(POS2+(6*NLEVELS)-2)=CDIR
              EXPARR(POS2+(6*NLEVELS))=CSPEED

              POS=POS+6
            ENDIF

! If 2-group (depth) not found where expected, stop expanding profile
! (but loop on until a delimiting group is found).

          ELSE
            IF (.NOT.GARBLED) PRINT *,
     &        'TESSC3 stopped at unexpected group: ',REPORT(POS:POS+4)
            GARBLED=.TRUE.
            POS=POS+6
          ENDIF
        ENDDO
      ENDIF

! Set POS2A to end of current profile in value array.

      POS2A=POS2+6*NLEVELS

! Put number of levels & k6/k4/k3 in values array.

      EXPARR(POS1A+2)=K6
      EXPARR(POS1A+4)=K4
      EXPARR(POS1A+6)=K3
      EXPARR(POS1A+7)=NLEVELS
      RETURN
      END
