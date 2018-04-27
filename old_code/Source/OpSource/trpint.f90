SUBROUTINE TRPINT(VALUES,CNAM,YEAR,MONTH,DAY,HOUR,MINT, &
                  BULCOD,II,STNID)

!----------------------------------------------------------------------
!
! PROGRAM       : TRPINT
!
! PURPOSE       : TO INITIALISE ELEMENTS OF THE EXPANSION ARRAY WITH
!                 DATA NOT AVAILABLE IN THE REPORT.
!
! DESCRIPTION   : DATA FROM THE INDEX ENTRY AND TRAILER IS USED TO
!                 FILL PART OF THE EXPANSION ARRAY TO ALLOW RETRIEVAL
!                 OF ELEMENTS NOT CONTAINED IN THE REPORT.
!
! DATA TYPE(S)  : Tropical Advisory
!
! CALLED BY     : TFMRET
!
! CALLS         : NONE
!
! ARGUMENTS     : (1) VALUES    EXPANSION ARRAY          (I/O)
!                 (2) CNAM      CHARACTER ELEMENT DATA   (O)
!                 (3) LAT       LATITUDE                 (I)
!                 (4) LON       LONGITUDE                (I)
!                 (5) YEAR      YEAR OF REPORT           (I)
!                 (6) MONTH     MONTH OF REPORT          (I)
!                 (7) DAY       DAY OF REPORT            (I)
!                 (8) HOUR      HOUR OF REPORT           (I)
!                 (9) BULCOD    BULLETIN IDENTIFIER      (I)
!                (10) CCCC      COLLECTION CENTRE        (I)
!                (11) STNID     STATION IDENTIFIER       (I)
!
!Y2K  26.06.1997  TRPINT is Year 2000 compliant.
!
! REVISION INFO:
!
! $Workfile: trpint.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 17/11/2010 09:58:48$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         17/11/2010 09:58:48    John Norton
!       Updated after doing rework for batch 13.
!  1    MetDB_Refresh 1.0         09/11/2010 11:35:01    John Norton     MetDB
!       Refresh created file  
! $
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

! Use statements:
! <Interfaces>

! None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

REAL,         INTENT(INOUT) ::  VALUES(:) ! Expanded data values
CHARACTER(*), INTENT(OUT)   ::  CNAM ! String to hold text element d    ata
INTEGER,      INTENT(IN)    ::  YEAR ! Year of observation
INTEGER,      INTENT(IN)    ::  MONTH ! Month of observation
INTEGER,      INTENT(IN)    ::  DAY ! Day of observation
INTEGER,      INTENT(IN)    ::  HOUR ! Hour of observation
INTEGER,      INTENT(IN)    ::  MINT
CHARACTER(*), INTENT(IN)    ::  BULCOD ! Bulletin identifier
CHARACTER(*), INTENT(IN)    ::  II ! II part of TTAAII
CHARACTER(*), INTENT(IN)    ::  STNID ! Station identifier

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

! None

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!----------------------------------------------------------------------
! Initialise the rest of the array values
!----------------------------------------------------------------------

 VALUES(1)=(65536*4)+1       ! TTAAII
 VALUES(2)=YEAR              ! Year of observation
 VALUES(3)=MONTH             ! Month of observation
 VALUES(4)=DAY               ! Day of observation
 VALUES(5)=HOUR              ! Hour of observation
 VALUES(6)=MINT              ! Min of observation
 VALUES(12)=(65536*6)+5      ! Bulletin Id
 VALUES(13)=(65536*4)+11     ! CC
 CNAM(1:4)=STNID
 CNAM(5:10)=BULCOD//II(1:2)
 CNAM(11:14)=STNID

 RETURN
END SUBROUTINE TRPINT
