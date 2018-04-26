SUBROUTINE GRIBINDX (DSNX, IDATA, ITIME, KODE)

!----------------------------------------------------------------------
! SUBROUTINE   : GRIBINDX
!
! PURPOSE      : TO OPEN THE INDEX DATA SET FOR A GRIB DATA TYPE AND
!                CHECK THE USER'S DATE/TIME WINDOW.
!
! DESCRIPTION  : 'GRIBINDX' LOCATES AND OPENS THE INDEX DATA SET FOR
!                A GRIB DATA TYPE AND READS THE RETENTION PERIOD FROM
!                ITS HEADER RECORD.
!
!                THE USER'S DATE/TIME WINDOW ('ITIME') IS CHECKED
!                AGAINST THE RETENTION PERIOD AND ALTERED IF (1) IT
!                STARTS BEFORE THE START OF THE RETENTION PERIOD, OR
!                (2) IT ENDS AFTER THE CURRENT TIME.
!
! USAGE        : CALL GRIBINDX (DSNX, IDATA, ITIME, KODE)
!
! PARAMETERS   : DSNX     I   (CHARACTER*(*)) NAME OF INDEX DATA SET.
!                IDATA    I   5-ELEMENT INTEGER ARRAY WITH INDEX DATA
!                             SET DETAILS AS OUTPUT BY 'DDICT':
!                               1  LENGTH OF DATA SET NAME,
!                               2  DATA SET RECORD LENGTH,
!                               3  REQUIRED FT NUMBER,
!                              4&5 (NOT USED FOR GRIB DATA).
!                ITIME   I/O  (8-ELEMENT INTEGER ARRAY) USER'S START
!                             AND END TIMES AS OUTPUT BY 'DDICT'.
!                             (EACH TIME IS YYYY, MM, DD, HH.)
!                KODE     O   OUTPUT STATUS CODE:
!                               4  INDEX DATA SET FOUND AND OPENED
!                              16  INDEX DATA SET COULD NOT BE OPENED
!
! CALLED BY    : GRIBRET
!
! CALLS        : DATIM, DT2HRS
!

! REVISION INFO :
!
! $Workfile: gribindx.F90$ $Folder: OpSource$
! $Revision: 7$ $Date: 20/12/2010 10:18:50$
!
! CHANGE RECORD :
!
! $Log:
!  7    MetDB_Refresh 1.6         20/12/2010 10:18:50    Sheila Needham
!       Initialise OUT variable KODE
!  6    MetDB_Refresh 1.5         22/11/2010 12:46:27    Stan Kellett    change
!        use datim to use datim_mod
!       change use d2hrs to use dt2hrs_mod and comment about call
!       add use inquire_mod
!  5    MetDB_Refresh 1.4         18/11/2010 11:45:43    John Norton     After
!       doing merge batch 9 changes.
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:16:53    John Norton     
!  1    MetDB_Refresh 1.0         13/10/2010 16:00:38    John Norton     f77
!       code copied out of StarTeam
!       MIDE\Met_DB_Project\Development\Source\OpSource
! $
!
!-----------------------------------------------------------------------
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

use datim_mod
use dt2hrs_mod    !Function
use inquire_mod

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(IN)        ::  DSNX ! NAME OF INDEX DATA SET
INTEGER, INTENT(IN)             ::  IDATA(5) ! INFORMATION ABOUT INDEX DATA SET (SEE ABOVE)
INTEGER, INTENT(INOUT)          ::  ITIME(8) ! START AND END TIMES OF REQUEST PERIOD
INTEGER, INTENT(OUT)            ::  KODE ! STATUS CODE RETURNED BY THIS ROUTINE

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!                                                             VARIABLES
!
INTEGER      ::  I ! IMPLIED DO LOOP VARIABLE FOR WRITE STATEMENT
INTEGER      ::  IDUMMY ! TO HOLD UNUSED DATA IN READ STATEMENT
INTEGER      ::  IEND ! CENTURY HOUR FOR END OF TIME WINDOW
INTEGER      ::  INOW ! CENTURY HOUR FOR CURRENT DATE/TIME
INTEGER      ::  IOS ! ERROR STATUS FROM I/O STATEMENT
INTEGER      ::  ISTART ! CENTURY HOUR FOR START OF TIME WINDOW
INTEGER      ::  ITHEN ! CENTURY HOUR FOR START OF RETENTION PERIOD
INTEGER      ::  IUNIT ! UNIT NUMBER FOR INDEX DATA SET
INTEGER      ::  LASTUNIT = 0 ! UNIT NUMBER USED BY PREVIOUS CALL
INTEGER      ::  NYEARS ! DATA RETENTION PERIOD (YEARS)
INTEGER      ::  NOLDEST ! YEAR OF START OF RETENTION PERIOD
INTEGER      ::  NOW(8) ! CURRENT DATE/TIME (AS OUTPUT BY 'DATIM')
!
LOGICAL      ::  FOUND ! FLAG INDICATING WHETHER INDEX DATA SET EXISTS
!

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------
!                                                   DATA INITIALISATION
!
SAVE LASTUNIT
KODE = 16               ! Assume the worst
!                                                  
!
!-----------------------------------------------------------------------
!     OPEN INDEX DATA SET FOR THIS DATA TYPE
!-----------------------------------------------------------------------
!                                      CLOSE LAST INDEX DATA SET IF ANY
!
IF (LASTUNIT > 0) CLOSE (LASTUNIT)
!
!                                   CHECK WHETHER INDEX DATA SET EXISTS
IUNIT = IDATA(3)

FOUND = INQUIRE(DSNX(1:IDATA(1)),'DSN')

IFLABEL1: &
IF (.NOT.FOUND) THEN
   WRITE (6,'(T5,A,T15,2A)') 'GRIBINDX:', &
             'INDEX DATA SET NOT FOUND - ', DSNX(1:IDATA(1))
   KODE = 16
ELSE
!                                                   OPEN INDEX DATA SET
!
#if defined (MVS)
   OPEN (IUNIT, FILE="//'"//DSNX(1:IDATA(1))//"'", &  
         FORM='UNFORMATTED', ACTION='READ', ACCESS='DIRECT',      &
         RECL=IDATA(2), IOSTAT=IOS)
#else
   OPEN (IUNIT, FILE='/'//DSNX(1:IDATA(1)), STATUS='OLD', &
         FORM='UNFORMATTED', ACCESS='DIRECT', &
         RECL=IDATA(2), IOSTAT=IOS)
#endif
!                                                   CHECK FOR I/O ERROR
IFLABEL2: &
   IF (IOS /= 0) THEN
     WRITE (6,*) 'GRIBINDX:',       &
                 'I/O ERROR OPENING DATA SET ', DSNX(1:IDATA(1)),IOS
      KODE = 16
   ELSE
!
!-----------------------------------------------------------------------
!     CHECK DETAILS OF REQUESTED DATE/TIME WINDOW
!-----------------------------------------------------------------------
!
!                               GET RETENTION PERIOD FROM HEADER RECORD
      LASTUNIT = IUNIT
      READ (IUNIT,REC=1) IDUMMY, IDUMMY, NYEARS
!
!                                                 COMPUTE CENTURY HOURS
      CALL DATIM (NOW)
      NOLDEST = NOW(8) - NYEARS
      ISTART = DT2HRS (ITIME(1), ITIME(2), ITIME(3), ITIME(4))
      IEND   = DT2HRS (ITIME(5), ITIME(6), ITIME(7), ITIME(8))
      INOW   = DT2HRS (  NOW(8),  NOW(7),   NOW(6),   NOW(5))
      ITHEN  = DT2HRS ( NOLDEST,  NOW(7),   NOW(6),   NOW(5))
!
!         CHECK THAT START TIME IS NOT BEFORE START OF RETENTION PERIOD
!
      IF (ISTART < ITHEN) THEN
         ITIME(1) = NOLDEST  ! Oldest retained year
         ITIME(2) = NOW(7)   ! Month
         ITIME(3) = NOW(6)   ! Day
         ITIME(4) = NOW(5)   ! Hour
!                                                               WARNING
         WRITE (6,'(T5,A,T15,A/T15,A,I6,3I4)') 'GRIBINDX:', &
               'START TIME IS BEFORE START OF RETENTION PERIOD.', &
               'TIME HAS BEEN RESET TO', (ITIME(I),I=1,4)
      END IF
!                         CHECK THAT END TIME IS NOT AFTER CURRENT TIME
!
      IF (IEND > INOW) THEN
         ITIME(5) = NOW(8)   ! Current year
         ITIME(6) = NOW(7)   ! Month
         ITIME(7) = NOW(6)   ! Day
         ITIME(8) = NOW(5)   ! Hour
      END IF
      KODE = 4  ! OK
   END IF IFLABEL2
END IF IFLABEL1
!                                             RETURN TO CALLING PROGRAM
RETURN
END SUBROUTINE GRIBINDX
