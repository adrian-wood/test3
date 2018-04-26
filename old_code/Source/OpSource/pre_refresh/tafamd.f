      SUBROUTINE TAFAMD(POINT,BEND,OAMD,AMDNUM,OCOR,CORNUM,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : TAFAMD
!
! PURPOSE       : To see if a bulletin or report is amended or
!                 corrected (by the group pointed to).
!                 (Like AMDCOR - but doesn't reset flags & numbers)
!
! DESCRIPTION   : If the next three characters are AMD or COR,
!                 or AA or CC followed by a letter, set a flag
!                 and remove the group, with any number after
!                 AMD or COR. If RTD, just delete it & number.
!
! CALLED BY     : TAFBUL
!
! CALLS         : NCHTST
!
! PARAMETERS    : (1) POINT  pointer to group to be checked        I/O
!                 (2) BEND   pointer to end of bulletin or report  I/O
!                 (3) OAMD   set if AMD or AA. found                O
!                 (4) AMDNUM number of amendment                   I/O
!                 (5) OCOR   set if COR or CC. found                O
!                 (6) CORNUM number of correction                  I/O
!                 (7) BULL   report or bulletin                    I/O
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:09$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tafamd.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:09    Sheila Needham  
! $
! Revision 2.0  2002/01/16 09:51:51  usmdb
! Initial revision.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER        POINT
      INTEGER        BEND
      INTEGER        INUM
      INTEGER        NSP
      INTEGER        NDEL

      LOGICAL        FIRST
      LOGICAL        OAMD
      LOGICAL        OCOR
      LOGICAL        OLFCR
      LOGICAL        ONUM
      LOGICAL        OSPACE

      CHARACTER*(*)  BULL
      CHARACTER*(*)  AMDNUM
      CHARACTER*(*)  CORNUM
      CHARACTER*3    LET
      CHARACTER*3    FIG
      CHARACTER*132  HEAD

      DATA FIRST/.TRUE./

      IF (FIRST) THEN
        HEAD='$RCSfile: tafamd.F,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:25:09$'
        FIRST=.FALSE.
      ENDIF

! Move past any spaces before group to be checked
! (Come back here if AMD or COR has been found but not both,
!  in case deleting one has left the other in its place.)

   10 NDEL=0
      DO WHILE (POINT.LT.BEND .AND. BULL(POINT:POINT).EQ.' ')
        POINT=POINT+1
      ENDDO

! If 'AMD' or 'COR', set corresponding flag.  If 'RTD', just delete...

      LET=BULL(POINT:POINT+2)
      IF (LET.EQ.'AMD' .OR. LET.EQ.'COR' .OR. LET.EQ.'RTD') THEN
        IF (LET.EQ.'AMD') OAMD=.TRUE.
        IF (LET.EQ.'COR') OCOR=.TRUE.

! See if next group (or rest of AMD/COR group) is 1 or 2 figures.

        IF (BULL(POINT+3:POINT+3).EQ.' ') THEN
          NSP=1
        ELSE
          NSP=0
        ENDIF

        FIG=BULL(POINT+3+NSP:POINT+5+NSP)
        CALL NCHTST(1,3,ONUM,INUM,OSPACE,OLFCR,FIG)

! If 1-2 figures are followed by a delimiter, set AMD/COR number.
! Also set number of characters to delete at end.

        IF ((OSPACE.OR.OLFCR) .AND. INUM.EQ.2) THEN
          IF (LET.EQ.'AMD') AMDNUM(2:2)=FIG(1:1)
          IF (LET.EQ.'COR') CORNUM(2:2)=FIG(1:1)
          NDEL=3+NSP+2
        ELSE IF ((OSPACE.OR.OLFCR) .AND. INUM.EQ.3) THEN
          IF (LET.EQ.'AMD') AMDNUM(1:2)=FIG(1:2)
          IF (LET.EQ.'COR') CORNUM(1:2)=FIG(1:2)
          NDEL=3+NSP+3
        ELSE
          NDEL=3+NSP
        ENDIF

! If there are 3 letters, AA. or CC. with the third letter in the
! range A-I, set the number corresponding to the third letter.
! (Check for <A & <0 rather than =' ' in case CRLF left...)
! If RR is followed by a letter, just delete the group.

      ELSE IF (BULL(POINT+3:POINT+3).LT.'A' .AND.
     &         BULL(POINT+3:POINT+3).LT.'0' .AND.
     &         LET(3:3).GE.'A') THEN
        IF (LET(1:2).EQ.'AA') THEN
          OAMD=.TRUE.
          WRITE (AMDNUM,'(I2.2)') 1+ICHAR(LET(3:3))-ICHAR('A')
          NDEL=4
        ELSE IF (LET(1:2).EQ.'CC') THEN
          OCOR=.TRUE.
          WRITE (CORNUM,'(I2.2)') 1+ICHAR(LET(3:3))-ICHAR('A')
          NDEL=4
        ELSE IF (LET(1:2).EQ.'RR') THEN
          NDEL=4
        ENDIF
      ENDIF

! Delete any group recognised (including any following number)
! by moving up the rest of the report or bulletin to overwrite it.
! Then go round again to check the group that has been moved up.

      IF (NDEL.GT.0) THEN
        BULL(POINT:BEND)=BULL(POINT+NDEL:BEND)
        BEND=BEND-NDEL
        GO TO 10
      ENDIF
      RETURN
      END
