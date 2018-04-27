      SUBROUTINE UAPART(REPORT,TT,PART,PART_TYPE,STANDRD,TEMP,
     &                  PILOT)                                      !2.0

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM       : UAPART
!
! PURPOSE       : Decide if upper air report is TEMP or PILOT
!                 & set part                                       !2.1
!
! DESCRIPTION   : Look for MiMiMjMj and use it if found; if not, use TT.
!
! DATA TYPE(S)  : UPPER AIR (TEMP & PILOT, ALL PARTS)
!
! CALLED BY     : UAXPAND
!
! CALLS         : NONE
!
! PARAMETERS    : (1) report (starting 'TTAA' or 'PPBB' OR...?)     (i)
!                 (2) 'TT' from TTAAii                              (i)
!                 (3) part (A,B,C,D)                                (o)
!                 (4) numeric part indicator (0,1,2,3)              (o)
!                 (5) true if standard level data                   (o)
!                 (6) true if temp or dropsonde                     (o)
!                 (7) true if pilot                                 (o)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:39$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uapart.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:39    Sheila Needham  
! $
! Revision 2.1  2003/01/06 16:09:20  usmdb
! 20 Jan 2003   C Long
! 2.1  Remove misleading comments about dropsondes.
!
! Revision 2.0  2001/07/03  10:44:33  10:44:33  usmdb (Generic MetDB account)
! Removed unused dummy argument IFAIL. Added copyright and
! modified header - S.Cox
!
! Revision 1.2  98/02/19  13:39:13  13:39:13  usmdb (Generic MetDB account)
! Set PART(='B') for dropsonde as well as TEMP=.TRUE.                 !A
!
! 30/12/97: use MiMiMjMj if found (except for dropsonde), otherwise TT
!
! Revision 1.1  1997/07/31 08:53:46  uspm
! Initial revision
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

!Declare Character
      CHARACTER REPORT *(*)
      CHARACTER PART *(*)
      CHARACTER TT *(*)
      CHARACTER MI*1
      CHARACTER HEAD*132

!Declare Integer
      INTEGER PART_TYPE
      INTEGER PTR

!Declare Logical
      LOGICAL STANDRD
      LOGICAL TEMP
      LOGICAL PILOT

!Initialize Variables
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/uapart.f,v $
     &'//'$ $Date: 30/01/2006 20:25:39$ $Revision: 1$'

! Look for MiMiMjMj and use it to decide part & TEMP/PILOT if found

      PTR=1

      DO WHILE (PTR.LT.10 .AND.
     &    ((REPORT(PTR:PTR+1).NE.'TT' .AND.
     &     REPORT(PTR:PTR+1).NE.'PP' .AND.
     &     REPORT(PTR:PTR+1).NE.'UU' .AND.
     &     REPORT(PTR:PTR+1).NE.'QQ' .AND.
     &     REPORT(PTR:PTR+1).NE.'XX' .AND.
     &     REPORT(PTR:PTR+1).NE.'II' .AND.
     &     REPORT(PTR:PTR+1).NE.'EE') .OR.
     &  (REPORT(PTR+2:PTR+3).NE.'AA' .AND.
     &   REPORT(PTR+2:PTR+3).NE.'BB' .AND.
     &   REPORT(PTR+2:PTR+3).NE.'CC' .AND.
     &   REPORT(PTR+2:PTR+3).NE.'DD')))
        PTR=PTR+1
      ENDDO

! Set PART as last letter of MiMiMjMj & TEMP/PILOT from first 2 letters
! A dropsonde must be a TEMP, part A or B.                         !2.1

      IF (PTR.LT.10) THEN                                            !a
        MI=REPORT(PTR:PTR)
        PART=REPORT(PTR+3:PTR+3)
        TEMP=MI.EQ.'T'.OR.MI.EQ.'U'.OR.MI.EQ.'I'
        PILOT=MI.EQ.'P'.OR.MI.EQ.'Q'.OR.MI.EQ.'E'
        IF (TT.EQ.'UZ') TEMP=.TRUE.                                  !a

! If no MiMiMjMj found (one may be corrupted) use TT to decide part
! (not always possible: some bulletins can contain different parts)

      ELSE
        TEMP=TT.EQ.'US'.OR.TT.EQ.'UK'.OR.TT.EQ.'UL'.OR.TT.EQ.'UE'
     &   .OR.TT.EQ.'UZ'
        PILOT=TT.EQ.'UP'.OR.TT.EQ.'UG'.OR.TT.EQ.'UH'.OR.TT.EQ.'UQ'

        PART=' '
        IF (TT.EQ.'US' .OR. TT.EQ.'UP') PART='A'
        IF (TT.EQ.'UK' .OR. TT.EQ.'UG') PART='B'
        IF (TT.EQ.'UL' .OR. TT.EQ.'UH') PART='C'
        IF (TT.EQ.'UE' .OR. TT.EQ.'UQ') PART='D'
        IF (TT.EQ.'UZ') PART='B'                                     !a
      ENDIF

! Finally set PART_TYPE & STANDRD from PART

      IF (PART.EQ.'A') PART_TYPE=0
      IF (PART.EQ.'B') PART_TYPE=1
      IF (PART.EQ.'C') PART_TYPE=2
      IF (PART.EQ.'D') PART_TYPE=3

      STANDRD=PART.EQ.'A'.OR.PART.EQ.'C'

      RETURN                                                            00450000
      END
