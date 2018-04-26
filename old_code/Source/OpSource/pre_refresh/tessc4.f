      SUBROUTINE TESSC4(REPORT,POS,EXPARR,ARRPOS3,ENDVALS)

!-----------------------------------------------------------------------
!
! PROGRAM       : TESSC4
!
! PURPOSE       : To expand section 4 of TESAC (sea depth)
!
! CALLED BY     : TESAC
!
! CALLS         : IVALUE   converts figures to numbers
!
! PARAMETERS    : REPORT   character string                        (I)
!                 POS      pointer to report                      (I/O)
!                 EXPARR   value array                             (O)
!                 ARRPOS3  array subscript: end of current profile (I)
!                 ENDVALS  sea depth (section 2) & instrumentation (I)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:21$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tessc4.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:21    Sheila Needham  
! $
! Revision 2.1  2001/11/06 10:16:45  usmdb
! 19 Nov 2001    C Long
! 2.1  Reduce argument list, correct declaration of passed variables.
!      Pass 3 elements from TESSC2, not just sea depth.
!
! Revision 2.0  2001/07/03  10:44:17  10:44:17  usmdb (Generic MetDB account)
! Separated variable declaration and initialisation. Removed unused
! argument IERR. Added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  11:43:33  11:43:33  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.1  1997/07/04 14:30:50  uspm
! Initial revision
!
! AUG 96 - REMOVAL OF GOTO STATMENTS AND ADDITIONAL                   !B
!          CHECKING AGAINST REPORT LENGTH.
!
! JAN 96 - DO NOT SET DEPTH WHEN NONE FOUND!                          !A
!
! INTRODUCED  : 22/08/94
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
      REAL           ENDVALS(3)
      INTEGER        POS
      INTEGER        ARRPOS3
      INTEGER        IVALUE                                         !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/tessc4.F,v $
     &'//'$ $Date: 30/01/2006 20:25:21$ $Revision: 1$'

! Should be pointing to 55555 at start of section 4.  If so, skip 55555.

      IF (POS+4.LE.LEN(REPORT)) THEN
        IF (REPORT(POS:POS+4).EQ.'55555') THEN
          POS=POS+6

! Sea depth group starts with 1.
! (This group & 00000 in section 2 should be mutually exclusive)

          IF (POS+4.LE.LEN(REPORT) .AND. REPORT(POS:POS).EQ.'1') THEN
            ENDVALS(1)=IVALUE(REPORT(POS+1:POS+4))
            POS=POS+6
          ENDIF
        ENDIF
      ENDIF

! Put sea depth (and IxIxIx & XrXr) at end of array for encoding.

      EXPARR(ARRPOS3+2)=ENDVALS(1)
      EXPARR(ARRPOS3+4)=ENDVALS(2)
      EXPARR(ARRPOS3+6)=ENDVALS(3)
      RETURN
      END
