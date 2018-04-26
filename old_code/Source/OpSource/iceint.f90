SUBROUTINE ICEINT(ARRAY,CDATA,YEAR,MONTH,DAY,HOUR,MIN,TOR,  &
                  REPLEN,TTAAII,CCCC,THISID,CORFLAG)

!-----------------------------------------------------------------------
!
! PROGRAM       : ICEINT
!
! PURPOSE       : TO INITIALISE THE ELEMENTS AVAILABLE FOR SEAICE
!                 AND TROPADV RETRIEVAL.
!
! DESCRIPTION   : A REAL ARRAY IS USED TO HOLD NUMERIC ELEMENTS AND
!                 A CHARACTER STRING HOLDS CHARACTER ELEMENTS.
!
! DATA TYPE(S)  : SEAICE, TROPADV
!
! CALLED BY     : ICEREP
!
! CALLS         : NONE
!
! FUNCTIONS     : NONE
!
! ARGUMENTS     : (1) ARRAY     EXPANSION ARRAY          (O)
!                 (2) CDATA     CHARACTER ELEMENT DATA   (O)
!                 (3) YEAR      YEAR OF REPORT           (I)
!                 (4) MONTH     MONTH OF REPORT          (I)
!                 (5) DAY       DAY OF REPORT            (I)
!                 (6) HOUR      HOUR OF REPORT           (I)
!                 (7) MIN       MINUTE OF REPORT         (I)
!                 (8) TOR       TIME OF RECEIPT ARRAY    (I)
!                 (9) REPLEN    REPORT LENGTH            (I)
!                (10) TTAAII    BULLETIN IDENTIFIER      (I)
!                (11) CCCC      ORIGINATING CENTRE       (I)
!                (12) THISID    REPORT IDENTIFIER        (I)
!                (13) CORFLAG   CORRECTION FLAG          (I)
!
! REVISION INFO :
!
! $Workfile: iceint.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 16/02/2011 12:13:01$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         16/02/2011 12:13:01    John Norton     Rework
!        done
!  4    MetDB_Refresh 1.3         20/12/2010 12:55:24    Sheila Needham  Add
!       INTENTS to agree with the interface file
!  3    MetDB_Refresh 1.2         18/11/2010 11:45:43    John Norton     After
!       doing merge batch 9 changes.
!  2    MetDB_Refresh 1.1         19/10/2010 14:37:39    Brian Barwell
!       Porting items 1-8, 12, 13 & removal of program change indicators done.
!  1    MetDB_Refresh 1.0         12/10/2010 16:25:27    Brian Barwell
!       Initial f77 version before porting to f90/95.
! $
! The original F77 version was introduced in July 1996.
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Arguments

REAL,         INTENT(OUT)   :: ARRAY(:) ! Expanded numeric values
CHARACTER(*), INTENT(OUT)   :: CDATA    ! Text element data
INTEGER,      INTENT(IN)    :: YEAR     ! Year of report
INTEGER,      INTENT(IN)    :: MONTH    ! Month of report
INTEGER,      INTENT(IN)    :: DAY      ! Day of report
INTEGER,      INTENT(IN)    :: HOUR     ! Hour of report
INTEGER,      INTENT(IN)    :: MIN      ! Minute of report
INTEGER,      INTENT(IN)    :: TOR(:)   ! Time of receipt array
INTEGER,      INTENT(IN)    :: REPLEN   ! Length of report
CHARACTER(*), INTENT(IN)    :: TTAAII   ! Bulletin identifier
CHARACTER(*), INTENT(IN)    :: CCCC     ! Originating centre
CHARACTER(*), INTENT(IN)    :: THISID   ! Report identifier
INTEGER,      INTENT(IN)    :: CORFLAG  ! Correction flag.

! Initialise array.

ARRAY(1)=(65536*6)+1         ! Report identifier
ARRAY(2)=YEAR                ! Year of report
ARRAY(3)=MONTH               ! Month of report
ARRAY(4)=DAY                 ! Day of report
ARRAY(5)=HOUR                ! Hour of report
ARRAY(6)=MIN                 ! Minute of report
ARRAY(7)=TOR(1)              ! Year of receipt
ARRAY(8)=TOR(2)              ! Month of receipt
ARRAY(9)=TOR(3)              ! Day of receipt
ARRAY(10)=TOR(4)             ! Hour of receipt
ARRAY(11)=TOR(5)             ! Minute of receipt
ARRAY(12)=(65536*6)+10       ! Bulletin identifier
ARRAY(13)=(65536*4)+16       ! Originating/collection centre
ARRAY(14)=REPLEN             ! Length of report

IF (CORFLAG > 0) THEN        ! Set correction flag
  ARRAY(15)=CORFLAG-1
ELSE
  ARRAY(15)=CORFLAG
END IF

CDATA(1:9)=THISID
CDATA(10:15)=TTAAII
CDATA(16:19)=CCCC

RETURN
END SUBROUTINE ICEINT
