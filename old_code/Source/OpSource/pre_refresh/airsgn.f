      SUBROUTINE AIRSGN(REPORT,REPLEN,POINT,SIGN)

      IMPLICIT NONE
!-----------------------------------------------------------------------
!
! PROGRAM       : AIRSGN
!
! PURPOSE       : to find the callsign in an AIREP report
!
! DESCRIPTION   : Accept 3 or more characters starting with a letter as
!                 an identifier, once ARP & any COR or RTD are removed.
!
! CALLED BY     : AIRARP
!
! CALLS TO      : AMDCOR, AIRLOC
!
! PARAMETERS    : 1. REPORT                                     (I)
!                 2. REPLEN  length of REPORT                   (I)
!                 3. POINT   pointer to REPORT                 (I/O)
!                         (to group after call sign on output)
!                 4. SIGN    call sign                          (O)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:45$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airsgn.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:45    Sheila Needham  
! $
! Revision 2.1  2002/01/16 09:39:10  usmdb
! 21 Jan 2002     C Long
! 2.1  Drastic rewrite: use new AMDCOR to delete COR & RTD,
!      accept any 3 characters starting with a letter as ident
!      (patterns in old code were ignored because IF condition
!      badly constructed!)
!
! Revision 2.0  2001/05/31  13:27:25  13:27:25  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.7  98/02/04  15:08:11  15:08:11  usmdb (Generic MetDB account)
! Remove any unwanted spaces from the AIREP report also
! accept new three character callsign configuration.                  !A
!
! Revision 1.6  1997/08/06 09:58:08  uspm
! Remove blank characters past column 72 as these get wrapped
! on transfer to 1 .
!
! Revision 1.5  1997/07/31 09:08:21  uspm
! First revision for 1
!
! Revision 1.4  1997/07/25 15:33:32  uspm
! Add check on grplen to avoid array out of bounds
!
! Revision 1.3  1997/07/03 14:10:22  uspm
! Latest version from 1  - Y2K check
!
! Revision 1.2  1997/06/05 15:01:33  uspm
! When calling airgrp pass report(x:) rather than report(x:1)
! to ensure HP airgrp has string of length grplen to work with
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

      INTEGER POINT           ! Position in AIREP report
      INTEGER START           ! Points to first group (for final print)
      INTEGER REPLEN          ! Report length
      INTEGER GRPLEN          ! Length of group in report

      CHARACTER*(*) REPORT
      CHARACTER*(*) SIGN      ! Aircraft call sign
      CHARACTER*132 HEAD      ! Revision information
      CHARACTER AMDNUM,CORNUM ! For AMDCOR call - not used

      LOGICAL OAMD,OCOR       ! For AMDCOR call - not used
      LOGICAL LREPFL          ! Set if end of report
      SAVE

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airsgn.F,v $
     &'//' $Date: 30/01/2006 20:20:45$ $Author: Sheila Needham$ '//
     &'$Revision: 1$'

      DO WHILE (REPORT(POINT:POINT).EQ.' ')
        POINT=POINT+1
      ENDDO
      START=POINT

! Skip (A)IREP or ARP at start of report

      IF (REPORT(POINT:POINT+3).EQ.'ARP ')   POINT=POINT+4
      IF (REPORT(POINT:POINT+3).EQ.'ARS ')   POINT=POINT+4
      IF (REPORT(POINT:POINT+5).EQ.'AIREP ') POINT=POINT+6
      IF (REPORT(POINT:POINT+4).EQ.'IREP ')  POINT=POINT+5

! Delete any COR or RTD groups which follow AIREP or ARP

      CALL AMDCOR(POINT,REPLEN,OAMD,AMDNUM,OCOR,CORNUM,REPORT)

! Take next group as call sign if it has 3 or more characters
! starting with a letter.
! If not, return dummy call sign & point back to unrecognised group.

      CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)

      SIGN=REPORT(POINT-GRPLEN-1:POINT-2)
      IF (GRPLEN.LT.3 .OR. SIGN(1:1).LT.'A' .OR. SIGN(1:1).GT.'Z') THEN
        SIGN='AIREP'
        PRINT *,'AIRSGN: no call sign found ',
     &  REPORT(START:MIN(START+50,REPLEN))
        POINT=POINT-GRPLEN-1
      ENDIF
      RETURN
      END
