      SUBROUTINE TESSC2(REPORT,POS,EXPARR,POS1A,ENDVALS)

!-----------------------------------------------------------------------
!
! PROGRAM       : TESSC2
!
! PURPOSE       : To expand section 2 of TESAC
!                 (temperature & salinity profile)
!
! CALLED BY     : TESAC
!
! CALLS         : IVALUE   converts figures to numbers
!
! PARAMETERS    : REPORT   character string        (I)
!                 POS      pointer to report      (I/O)
!                 EXPARR   expansion array         (O)
!                 POS1A    end of profile in array (O)
!                 TOTDEP   total water depth       (O)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:20$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tessc2.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:20    Sheila Needham  
! $
! Revision 2.1  2001/11/06 10:16:15  usmdb
! 19 Nov 2001    C Long
! 2.1  Reduce argument list, tidy up in many ways,
!      return instrumental details from group at start of section 2
!      to be put in value array by TESSC4.
!
! Revision 2.0  2001/07/03  10:44:16  10:44:16  usmdb (Generic MetDB account)
! Separated variable declaration and initialisation. Removed unused
! argument IERR. Added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  11:43:12  11:43:12  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.1  1997/07/04 14:29:12  uspm
! Initial revision
!
! INTRODUCED 22/08/94
!
! AUG 96 - REMOVAL OF GOTO STATMENTS AND ADDITIONAL                   !B
!          CHECKING AGAINST REPORT LENGTH.
!
! JAN 96 - ALLOW FOR NEGATIVE TEMPERATURES IN PROFILE,                !A
!          CORRECT PROFILE LOOP
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

      CHARACTER*(*)  REPORT                                        !2.1
      CHARACTER*132  HEAD
      REAL           EXPARR(0:*)                                   !2.1
      REAL           ENDVALS(3)
      REAL           MISING                                        !2.0
      REAL           DEPTH
      REAL           TEMP
      REAL           SALIN
      INTEGER        REPLEN
      INTEGER        POS
      INTEGER        POS1
      INTEGER        POS1A
      INTEGER        IVALUE
      INTEGER        NLEVELS  ! number of levels (replications)    !2.1
      INTEGER        K1       ! digitization indicator
      INTEGER        K2       ! salinity/depth measurement method
      LOGICAL        GARBLED  ! true if 2-group not found

      DATA           MISING/-9999999./                             !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/tessc2.F,v $
     &'//'$ $Date: 30/01/2006 20:25:20$ $Revision: 1$'

! POS1 is where the count of levels will be stored in the value array;
! depth, temperature & salinity subscripts are relative to POS1.

      REPLEN=LEN(REPORT)
      ENDVALS(1)=MISING         ! sea depth
      ENDVALS(2)=MISING         ! IxIxIx
      ENDVALS(3)=MISING         ! XrXr
      K1=MISING
      K2=MISING
      POS1=29
      NLEVELS=0

! POS at start points to start of section 2 - should be 888

      IF (REPORT(POS:POS+2).EQ.'888' .AND. POS+4.LE.REPLEN) THEN

! 888 is followed by k1 (code table 2262), values 7 or 8, BUFR 0 or 1.

        K1=IVALUE(REPORT(POS+3:POS+3))
        IF (K1.EQ.7) THEN
          K1=0
        ELSE IF (K1.EQ.8) THEN
          K1=1
        ELSE
          K1=MISING
        ENDIF

! k2 follows: values of 0 to 3, no conversion needed (code table 2263)

        K2=IVALUE(REPORT(POS+4:POS+4))
        IF (K2.LT.0 .OR. K2.GT.3) K2=MISING

! Move POS to next group, IxIxIxXrXr (XBT instrument & recorder type).
! This group, made mandatory in 2000, could be confused with the first
! depth group: if it starts with a 2, assume it's instrumentation if
! another 2-group follows, depth if a 3- or 4-group follows.
! That is, skip it if it's not a 2-group or if a 2-group follows.

        POS=POS+6
        IF (REPORT(POS:POS).NE.'2' .OR.
     &      REPORT(POS+6:POS+6).EQ.'2') THEN
          ENDVALS(2)=IVALUE(REPORT(POS:POS+2))      ! IxIxIx
          ENDVALS(3)=IVALUE(REPORT(POS+3:POS+4))    ! XrXr
          POS=POS+6
        ENDIF

! Loop round depth, temperature & salinity groups (starting 2, 3
! or 4 respectively) until one of the following is reached:
!   66... at start of section 3 (current profile),
!   55555 at start of section 4,
!   call sign (with letters) or 99999 before buoy number in section 5,
!   00000 indicating that lowest data is at bottom (end of section 2)

        GARBLED=.FALSE.
        DO WHILE (POS+4.LE.REPLEN .AND. .NOT.(
     &            REPORT(POS:POS+4).EQ.'99999' .OR.
     &            REPORT(POS:POS+4).EQ.'55555' .OR.
     &            REPORT(POS:POS+1).EQ.'66'    .OR.
     &            REPORT(POS:POS+4).EQ.'00000' .OR.
     &     IVALUE(REPORT(POS:POS+4)).EQ.-9999999))

          IF (.NOT.GARBLED .AND. REPORT(POS:POS).EQ.'2') THEN
            DEPTH=IVALUE(REPORT(POS+1:POS+4))
            POS=POS+6

! Convert temperature from hundredths Celsius to real degrees Kelvin

            TEMP=MISING
            IF (POS+4.LE.REPLEN .AND. REPORT(POS:POS).EQ.'3') THEN
              TEMP=IVALUE(REPORT(POS+1:POS+4))
              IF (TEMP.GE.5000) TEMP=5000-TEMP
              IF (TEMP.NE.MISING) TEMP=TEMP*0.01+273.15
              POS=POS+6
            ENDIF

! Convert salinity from hundredths of parts per thousand to real

            SALIN=MISING
            IF (POS+4.LE.REPLEN .AND. REPORT(POS:POS).EQ.'4') THEN
              SALIN=IVALUE(REPORT(POS+1:POS+4))
              IF (SALIN.NE.MISING) SALIN=SALIN*0.01
              POS=POS+6
            ENDIF

! If T or salinity reported, store either or both with depth

            IF (TEMP.NE.MISING .OR. SALIN.NE.MISING) THEN
              NLEVELS=NLEVELS+1
              EXPARR(POS1+6*NLEVELS-4)=DEPTH
              EXPARR(POS1+6*NLEVELS-2)=TEMP
              EXPARR(POS1+6*NLEVELS)=SALIN
            ENDIF

! If 2-group (depth) not found where expected, stop expanding profile.

          ELSE
            IF (.NOT.GARBLED) PRINT *,
     &        'TESSC2 stopped at unexpected group: ',REPORT(POS:POS+4)
            GARBLED=.TRUE.
            POS=POS+6
          ENDIF
        ENDDO

! If profile loop was ended by 00000, store last depth as total depth
! & skip the 00000 (bottom indicator) itself.

        IF (POS+4.LE.REPLEN .AND. REPORT(POS:POS+4).EQ.'00000') THEN
          IF (NLEVELS.GE.1) ENDVALS(1)=EXPARR(POS1+(6*NLEVELS)-4)
          POS=POS+6
        ENDIF
      ENDIF

! Set POS1A to end of first profile in value array.

      POS1A=POS1+6*NLEVELS

! Put number of levels & k1/k2 in values array.

      EXPARR(POS1)=NLEVELS
      EXPARR(26)=K1
      EXPARR(28)=K2
      RETURN
      END
