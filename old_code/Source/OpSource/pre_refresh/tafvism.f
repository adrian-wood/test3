      SUBROUTINE TAFVISM(GROUP,VVVV)

!-----------------------------------------------------------------------
!
!  PROGRAM     : TAFVISM
!
!  PURPOSE     : To decode visibility in statute miles
!                from a group of the form nSM, nnSM, PnSM
!                or n a/bSM, where b is 2, 4, 8 or 16
!                (or na/bSM, more common in practice than n a/bSM)
!
!  DESCRIPTION : The miles are converted to metres
!
!  CALLED BY   : TAFEXP
!
!  PARAMETERS  : 1 GROUP - group to be expanded, ending 'SM'
!                2 VVVV  - visibility returned (in metres)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:16$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tafvism.f,v $
!
! CHANGE RECORD :
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

      CHARACTER*(*) GROUP
      CHARACTER*1   M           ! figure (?) before '/'
      CHARACTER*1   N           ! figure (?) after '/'
      REAL          VVVV
      INTEGER       L           ! length of group
      INTEGER       INUM        ! numerator of fraction
      INTEGER       IDENOM      ! denominator of fraction

      CHARACTER*132 HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/tafvism.f,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:25:16$ '

      VVVV=-9999999.
      L=LEN(GROUP)

! First a whole number of statute miles (1 figure or 2)
! (In TAFs anything more than 6SM is forecast as P6SM, though METARs
!  can have 2 figures - see National Practices in Manual on Codes
!  (WMO No.306, Vol.II).  But our expansion treats P6SM like 6SM.)

      IF (L.EQ.3) THEN
        READ (GROUP(1:1),'(F1.0)') VVVV
      ELSE IF (L.EQ.4) THEN
        IF (GROUP(1:1).EQ.'P') THEN
          READ (GROUP(2:2),'(F1.0)') VVVV
        ELSE
          READ (GROUP(1:2),'(F2.0)') VVVV
        ENDIF

! Then a fraction of a mile (half, quarters or eighths)
! (See under METARs in National Practices for rules about fractions:
!  quarters only if <3, eighths only if <2, sixteenths only if <3/8.)

      ELSE IF (L.EQ.5) THEN
        M=GROUP(1:1)
        N=GROUP(3:3)
        IF (GROUP(2:2).EQ.'/' .AND.
     &     (M.EQ.'1' .OR. M.EQ.'3' .OR. M.EQ.'5' .OR. M.EQ.'7') .AND.
     &     (N.EQ.'2' .OR. N.EQ.'4' .OR. N.EQ.'8')) THEN
          INUM=ICHAR(M)-ICHAR('0')
          IDENOM=ICHAR(N)-ICHAR('0')
          VVVV=REAL(INUM)/REAL(IDENOM)
        ENDIF

! or sixteenths

      ELSE IF (L.EQ.6 .AND. GROUP(2:4).EQ.'/16') THEN
        INUM=ICHAR(GROUP(1:1))-ICHAR('0')
        VVVV=REAL(INUM)/16.

! Finally a whole number (single figure) with a fraction.

      ELSE IF (L.EQ.6 .OR. L.EQ.7) THEN
        IF (GROUP(L-3:L-3).EQ.'/') THEN

! whole miles

          READ (GROUP(1:1),'(F1.0)') VVVV

! fraction (assuming there's a space between whole miles & fraction
! & no sixteenths with a whole number, so single-figure denominator)

          M=GROUP(L-4:L-4)
          N=GROUP(L-2:L-2)
          IF ((M.EQ.'1' .OR. M.EQ.'3' .OR. M.EQ.'5' .OR. M.EQ.'7') .AND.
     &       (N.EQ.'2' .OR. N.EQ.'4' .OR. N.EQ.'8')) THEN
            INUM=ICHAR(M)-ICHAR('0')
            IDENOM=ICHAR(N)-ICHAR('0')
            VVVV=VVVV+REAL(INUM)/REAL(IDENOM)
          ENDIF
        ENDIF
      ENDIF

! If a statute mile is 5280 feet and a foot is 0.3048 metres,
! then a statute mile is 5280*0.3048=1609.344 metres.

      VVVV=VVVV*1609.344
      RETURN
      END
