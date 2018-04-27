      SUBROUTINE BATESBUL(BULL,TTAAII,CCCC,OCOR,MIMJ,NFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : BATESBUL
!
! PURPOSE       : to find reports in a BATHY/TESAC bulletin
!                 and then call routines to expand, encode
!                 & store them.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : BULLED, BATHY, TESAC
!
! PARAMETERS    : BULL    bulletin (edited by BULLED here)         (I/O)
!                 TTAAii  bulletin identifier                        (I)
!                 CCCC    originating centre                         (I)
!                 OCOR    bulletin correction flag                   (I)
!                 MiMj    first MiMiMjMj (on input), then changed..(I/O)
!                 NFT     FT number for SUBSEA dataset               (I)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:58$
! $Source: /home/us0400/mdb/op/lib/source/RCS/batesbul.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:58    Sheila Needham  
! $
! Revision 2.1  2001/10/09 10:31:58  usmdb
! 15 Oct 2001    C Long
! 2.1  Don't lose ident off end of last ob in bulletin.
!
! Revision 2.0  2001/09/05  09:28:27  09:28:27  usmdb (Generic MetDB account)
! Initial revision
!
! INTRODUCED  17 Sept 2001
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

      IMPLICIT NONE

      CHARACTER*(*)  BULL
      CHARACTER*(*)  TTAAII   ! (not used till AIRSTO adds trailers)
      CHARACTER*(*)  CCCC     ! (not used till AIRSTO adds trailers)
      CHARACTER*(*)  MIMJ     ! first MiMiMjMj, then current...
      CHARACTER*4    NEXT_MIMJ! JJ.. or KK.. at start of next report

      INTEGER        POINT    ! moved to start of each report in turn
      INTEGER        REPLEN   ! length of current report
      INTEGER        NFT      ! storage FT number
      INTEGER        BULEND   ! end of bulletin
      INTEGER        IQ       ! points to equal sign (from start of ob)
      INTEGER        IX       ! points to next MiMiMjMj (from first...)
      LOGICAL        OCOR     ! used to set COR flag in trailer

      CHARACTER*132  HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/batesbul.F,v $
     &'//'$ $Date: 30/01/2006 20:20:58$ $Revision: 1$ '

! Replace CR, LF & multiple spaces in reports by single spaces.

      POINT=INDEX(BULL,MIMJ)
      BULEND=LEN(BULL)                      ! BULLED may reset BULEND!
      CALL BULLED(POINT,BULEND,BULL)

! Delimit report, either BATHY or TESAC, by looking for JJ.. or KK..
! (4 letters, 3rd & 4th same) at start of next report.

      DO WHILE (MIMJ.NE.' ')                ! until no more MiMj's
        IQ=INDEX(BULL(POINT:BULEND),'=')    ! look for next equal sign

! Look for next JJ.. or KK.. (4-character group, JJ/KK & another pair)

        IX=POINT
        NEXT_MIMJ=' '
        DO WHILE (((BULL(IX:IX+2).NE.' JJ' .AND. BULL(IX:IX+2).NE.' KK')
     &            .OR. BULL(IX+3:IX+3).NE.BULL(IX+4:IX+4)
     &            .OR. BULL(IX+5:IX+5).NE.' ') .AND. IX.LT.BULEND-5)
          IX=IX+1
        ENDDO
        IF (IX.LT.BULEND-5) THEN                                   !2.1
          NEXT_MIMJ=BULL(IX+1:IX+4)                                !2.1
        ELSE                                                       !2.1
          IX=0    ! no further MiMj                                !2.1
        ENDIF                                                      !2.1

! Set length of report from whichever of =, JJ.. & KK.. comes first

        REPLEN=BULEND-POINT                 ! in case last ob
        IF (IQ.GT.0) REPLEN=IQ-1            ! if equals found
        IF (IX.GT.0 .AND. IX-POINT.LT.REPLEN) REPLEN=IX-POINT

! Expand, encode & store BATHY or TESAC

        IF (REPLEN.GE.24) THEN
          IF (MIMJ(1:2).EQ.'JJ') THEN
            CALL BATHY(BULL(POINT:POINT+REPLEN-1),TTAAII,CCCC,OCOR,NFT)
          ELSE IF (MIMJ(1:2).EQ.'KK') THEN
            CALL TESAC(BULL(POINT:POINT+REPLEN-1),TTAAII,CCCC,OCOR,NFT)
          ENDIF
        ELSE
          PRINT *,'BATESBUL: too short! ',BULL(POINT:POINT+REPLEN-1)
        ENDIF

! Move on to next report in bulletin
! (IX=0 if none; but then MIMJ=' ' jumps out of loop, so OK)       !2.1

        POINT=IX+1
        MIMJ=NEXT_MIMJ
      ENDDO
      RETURN
      END
