SUBROUTINE GRIBREAD (IUNIT, LENREC, LEFT, NCOPIED, RECORD, KODE)

!----------------------------------------------------------------------
! SUBROUTINE   : GRIBREAD
!
! PURPOSE      : TO READ THE NEXT RECORD FROM A GRIB DATA SET.
!
! DESCRIPTION  : 'GRIBREAD' RETURNS ONE RECORD AT A TIME FROM A GRIB
!                DATASET. IF IT IS THE FIRST RECORD, THE START OF THE
!                MESSAGE ('GRIB') IS LOCATED AND THE TOTAL MESSAGE
!                LENGTH DECODED AND RETURNED. FURTHER MESSAGES MAY
!                APPEAR IN THE SAME RECORD. A SECOND RECORD IS CONCAT-
!                ENATED TO THE CURRENT RECORD WHEN SEACHING FOR
!                THE NEXT OCCURRENCE OF 'GRIB' TO ALLOW FOR OVERFLOW.
!                ONLY ONE RECORD IS RETURNED TO THE CALLER.
!
! USAGE        : CALL GRIBREAD
!                     (IUNIT, LENREC, LEFT, NCOPIED, RECORD, KODE)
!
! PARAMETERS   : IUNIT    I   UNIT NUMBER FOR GRIB DATA SET.
!                LENREC   I   RECORD LENGTH OF GRIB DATA SET.
!                LEFT    I/O  AMOUNT OF GRIB MESSAGE LEFT TO READ IN.
!                             IF STARTING A NEW MESSAGE, INPUT A ZERO
!                             OR NEGATVE VALUE: IN THIS CASE THE
!                             TOTAL MESSAGE LENGTH IS RETURNED,
!                             OTHERWISE 'LEFT' IS NOT ALTERED.
!                NCOPIED  O   BYTES IN RECORD BEFORE NEXT TO COPY.
!                             (USUALLY 0, BUT FOR THE FIRST RECORD IT
!                             IS SET TO THE BYTE NUMBER BEFORE 'GRIB'
!                             TO PREVENT ANY DATA BEFORE 'GRIB' BEING
!                             RETURNED TO THE USER.)
!                RECORD   O   (C*(*)) NEXT RECORD OF GRIB DATA SET.
!                KODE     O   RETURN CODE - VALUES AS FOLLOWS:
!                               0  LAST RECORD OF GRIB DATA SET READ,
!                               4  NEXT RECORD OF GRIB DATA SET READ
!                                  BUT MORE STILL TO COME,
!                               8  START OF GRIB MESSAGE NOT FOUND,
!                              16  FATAL ERROR DUE TO EITHER:
!                                   - I/O ERROR IN READ STATEMENT, OR
!                                   - END OF GRIB MESSAGE NOT FOUND.
! CALLS        : ICHAR3
!
! CALLED BY    : GRIBRET
!
! REVISION INFO :
!
! $Workfile: gribread.f90$ $Folder: OpSource$
! $Revision: 10$ $Date: 21/03/2012 11:22:36$
!
! CHANGE RECORD :
!
! $Log:
!  10   MetDB_Refresh 1.9         21/03/2012 11:22:36    Sheila Needham  Added
!       SAVE for all variables
!  9    MetDB_Refresh 1.8         15/03/2012 14:14:36    Sheila Needham
!       Changes for RADOP and RADRF
!  8    MetDB_Refresh 1.7         29/11/2010 08:43:01    Sheila Needham  Add
!       IREC to SAVE variables
!  7    MetDB_Refresh 1.6         26/11/2010 09:17:53    Sheila Needham
!       Changes for direct access reads
!  6    MetDB_Refresh 1.5         22/11/2010 16:54:57    Stan Kellett
!       removed declaration of function ICHAR3 as already declared in mod
!       file.
!  5    MetDB_Refresh 1.4         18/11/2010 11:45:43    John Norton     After
!       doing merge batch 9 changes.
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:17:09    John Norton     
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

use ichar3_mod

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

INTEGER, INTENT(IN)             ::  IUNIT ! UNIT NUMBER FOR GRIB DATA SET
INTEGER, INTENT(IN)             ::  LENREC ! RECORD LENGTH OF GRIB DATA SET
INTEGER, INTENT(INOUT)          ::  LEFT ! SET TO NUMBER OF BYTES IN MESSAGE IF NEW D/S
INTEGER, INTENT(OUT)            ::  NCOPIED ! BYTES IN RECORD BEFORE NEXT TO COPY
CHARACTER(*), INTENT(OUT)       ::  RECORD ! TO HOLD ONE RECORD OF GRIB DATA
INTEGER, INTENT(OUT)            ::  KODE ! OUTPUT STATUS CODE - SEE ABOVE

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
INTEGER,PARAMETER :: RECLEN=9216  ! Record length for local declarations

!                                                             VARIABLES
!
INTEGER      ::  IGRIB ! LOCATION OF START OF 'GRIB' IN FIRST RECORD
INTEGER      ::  IOS ! STATUS CODE FROM READ STATEMENT
INTEGER      ::  IREC ! DIRECT ACCESS RECORD NUMBER
INTEGER      ::  LAST ! EXPECTED LOCATION OF END OF GRIB MESSAGE
INTEGER      ::  LENGTH ! LENGTH OF NEXT MESSAGE
INTEGER      ::  MGRIB ! GRIB EDITION NUMBER IN MESSAGE SECTION 0
!
LOGICAL      ::  FIRST = .TRUE. ! .TRUE. FOR FIRST CALL TO SUBROUTINE
LOGICAL      ::  GOTNEXT ! True if next rec has already been read in
!
CHARACTER(4) ::  GRIB, SEVENS ! 'GRIB' AND '7777' IN ASCII
CHARACTER(RECLEN)  :: NEXTREC   ! next record
CHARACTER(2*RECLEN) :: BOTHRECS  ! ... concatenated records

!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------
!
SAVE
!                                REVISION INFORMATION & INITIALISATIONS
IF (FIRST) THEN
!
   GRIB   = CHAR(71) // CHAR(82) // CHAR(73) // CHAR(66) ! 'GRIB'
   SEVENS = CHAR(55) // CHAR(55) // CHAR(55) // CHAR(55) ! '7777'
   IREC   = 1
   FIRST  = .FALSE.
   GOTNEXT = .FALSE.
END IF
!             READ THE NEXT RECORD IF NECCESSARY AND CHECK FOR I/O ERROR
!
IF (GOTNEXT) THEN
   RECORD = NEXTREC
   GOTNEXT = .FALSE.
ELSE
   IOS=0
   READ (IUNIT, REC=IREC, IOSTAT=IOS) RECORD(1:LENREC)
   IREC = IREC + 1
END IF
!
IFLABEL1: &
IF (IOS == 214) THEN         ! End of dataset
   KODE = 8
   IREC = 1
   RETURN
ELSE IF (IOS /= 0) THEN
   WRITE (6,'(T5,A,T15,A,I4)') 'GRIBREAD:', &
            'I/O ERROR READING GRIB DATA SET.  STATUS =',IOS
   KODE = 16
   IREC = 1
   RETURN
!                                              FIRST RECORD OF DATA SET
ELSE IF (LEFT <= 0) THEN
!                                                         LOCATE 'GRIB'
   IGRIB = INDEX(RECORD(1:LENREC-15),GRIB)
   IF (IGRIB == 0) THEN
      WRITE (6,'(T5,A,T15,A,I4)') 'GRIBREAD:', &
               'START OF GRIB MESSAGE NOT FOUND ON UNIT ', IUNIT
      KODE = 8
      IREC = 1
      RETURN
   ELSE
!                              'GRIB' FOUND. (ANYTHING BEFORE 'GRIB' IS
!                                NOT WANTED SO SET NCOPIED TO SKIP IT.)
      NCOPIED = IGRIB - 1
      MGRIB = ICHAR(RECORD(IGRIB+7:IGRIB+7)) ! GRIB edition
   END IF
!                                                 DECODE MESSAGE LENGTH
   IF (MGRIB == 1) THEN
      LEFT = ICHAR3(RECORD(IGRIB+4:IGRIB+6))
   ELSE IF (MGRIB == 2) THEN
      LEFT = 256*ICHAR3(RECORD(IGRIB+12:IGRIB+14)) + &
                 ICHAR (RECORD(IGRIB+15:IGRIB+15))
   END IF
ELSE
!                                         NOT FIRST RECORD OF DATA SET.
!                              WHOLE RECORD IS WANTED SO SET NCOPIED=0.
   NCOPIED = 0
END IF IFLABEL1
!                     CHECK WHETHER END OF MESSAGE HAS NOW BEEN READ IN
KODE = 4
LAST = LEFT + NCOPIED
IF (LAST < LENREC) THEN
!
!                                 MAY BE ANOTHER MESSAGE AFTER THIS ONE
  READ (IUNIT,REC=IREC,IOSTAT=IOS)NEXTREC(1:LENREC)
  IREC =IREC + 1
  IF (IOS /= 0 )THEN
    KODE = 8
    IREC = 1
    NEXTREC = CHAR(0)
    GOTNEXT=.FALSE.
  ELSE
    GOTNEXT=.TRUE.
  END IF
! Look for another message in this record (may overflow into next)

  BOTHRECS = RECORD//NEXTREC
  IGRIB = INDEX(BOTHRECS(LAST+1:LAST+4),GRIB)
  IF (IGRIB > 0)THEN
    MGRIB = ICHAR(BOTHRECS(LAST+IGRIB+7:LAST+IGRIB+7))
    IF (MGRIB == 1)THEN
      LENGTH = ICHAR3(BOTHRECS(LAST+IGRIB+4:LAST+IGRIB+6))
    ELSE IF (MGRIB == 2) THEN
      LENGTH = 256*ICHAR3(BOTHRECS(LAST+IGRIB+12:LAST+IGRIB+14)) &
              + ICHAR (BOTHRECS(LAST+IGRIB+15:LAST+IGRIB+15))
    END IF
!                             LEFT is now the remains of the last msg
!                               plus the new one
    LEFT= LEFT+LENGTH
  END IF
ELSE            ! Message goes into next record so we're copying it all
  GOTNEXT=.FALSE.

END IF

!                                             RETURN TO CALLING PROGRAM
RETURN
END SUBROUTINE GRIBREAD
