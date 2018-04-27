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
! CALLS        : DATIM, D2HRS
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/gribindx.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:58:43  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  2000/12/08  14:35:08  14:35:08  usmdb (Generic MDB account)
! pre-processor statements added around OPEN statement - S.Cox
!
! Revision 1.1  2000/12/08  14:16:34  14:16:34  usmdb (Generic MDB account)
! Initial revision
!
! ORIGINAL VERSION BY BRIAN BARWELL, AUGUST 2000.
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
USE ZPDATE_STUFF
IMPLICIT NONE
!                                                             VARIABLES
!
INTEGER DT2HRS     ! FUNCTION TO CONVERT TIME TO CENTURY HOURS
INTEGER I          ! IMPLIED DO LOOP VARIABLE FOR WRITE STATEMENT
INTEGER IDATA(5)   ! INFORMATION ABOUT INDEX DATA SET (SEE ABOVE)
INTEGER IDUMMY     ! TO HOLD UNUSED DATA IN READ STATEMENT
INTEGER IEND       ! CENTURY HOUR FOR END OF TIME WINDOW
INTEGER INOW       ! CENTURY HOUR FOR CURRENT DATE/TIME
INTEGER IOS        ! ERROR STATUS FROM I/O STATEMENT
INTEGER ISTART     ! CENTURY HOUR FOR START OF TIME WINDOW
INTEGER ITHEN      ! CENTURY HOUR FOR START OF RETENTION PERIOD
INTEGER ITIME(8)   ! START AND END TIMES OF REQUEST PERIOD
INTEGER IUNIT      ! UNIT NUMBER FOR INDEX DATA SET
INTEGER KODE       ! STATUS CODE RETURNED BY THIS ROUTINE
INTEGER LASTUNIT   ! UNIT NUMBER USED BY PREVIOUS CALL
INTEGER NYEARS     ! DATA RETENTION PERIOD (YEARS)
INTEGER NOLDEST    ! YEAR OF START OF RETENTION PERIOD
INTEGER NOW(8)     ! CURRENT DATE/TIME (AS OUTPUT BY 'DATIM')
!
LOGICAL FOUND      ! FLAG INDICATING WHETHER INDEX DATA SET EXISTS
!
CHARACTER*(*) DSNX ! NAME OF INDEX DATA SET
CHARACTER*132 HEAD ! FOR REVISION INFORMATION
!                                                   DATA INITIALISATION
DATA LASTUNIT/0/
!
SAVE LASTUNIT
!                                                  REVISION INFORMATION
HEAD='&
& $Source: /home/us0400/mdb/op/lib/source/RCS/gribindx.F,v $&
&'//'$Date: 26/01/2010 10:18:13$ $Revision: 1$'
!
!-----------------------------------------------------------------------
!     OPEN INDEX DATA SET FOR THIS DATA TYPE
!-----------------------------------------------------------------------
!                                      CLOSE LAST INDEX DATA SET IF ANY
!
IF (LASTUNIT.GT.0) CLOSE (LASTUNIT)
!
!                                   CHECK WHETHER INDEX DATA SET EXISTS
IUNIT = IDATA(3)
INQUIRE (FILE='/'//DSNX(1:IDATA(1)), EXIST=FOUND, IOSTAT=IOS)
IF (.NOT.FOUND) THEN
   WRITE (6,'(T5,A,T15,2A)') 'GRIBINDX:',&
            &'INDEX DATA SET NOT FOUND - ', DSNX(1:IDATA(1))
   KODE = 16
ELSE
!                                                   OPEN INDEX DATA SET
!
!if defined (MVS)
   OPEN (IUNIT, FILE='/'//DSNX(1:IDATA(1)), STATUS='OLD',&
        &FORM='UNFORMATTED', ACTION='READ', ACCESS='DIRECT',&
        &RECL=IDATA(2), IOSTAT=IOS)
!else
!endif
!                                                   CHECK FOR I/O ERROR
   IF (IOS.NE.0) THEN
      WRITE (6,'(T5,A,T15,3A)') 'GRIBINDX:',&
              &'I/O ERROR OPENING DATA SET ', DSNX(1:IDATA(1))
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
      IF (ISTART.LT.ITHEN) THEN
         ITIME(1) = NOLDEST  ! Oldest retained year
         ITIME(2) = NOW(7)   ! Month
         ITIME(3) = NOW(6)   ! Day
         ITIME(4) = NOW(5)   ! Hour
!                                                               WARNING
         WRITE (6,'(T5,A,T15,A/T15,A,I6,3I4)') 'GRIBINDX:',&
              &'START TIME IS BEFORE START OF RETENTION PERIOD.',&
              &'TIME HAS BEEN RESET TO', (ITIME(I),I=1,4)
      END IF
!                         CHECK THAT END TIME IS NOT AFTER CURRENT TIME
!
      IF (IEND.GT.INOW) THEN
         ITIME(5) = NOW(8)   ! Current year
         ITIME(6) = NOW(7)   ! Month
         ITIME(7) = NOW(6)   ! Day
         ITIME(8) = NOW(5)   ! Hour
      END IF
      KODE = 4  ! OK
   END IF
END IF
!                 RETURN TO CALLING PROGRAM
RETURN
END SUBROUTINE GRIBINDX
