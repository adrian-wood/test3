      SUBROUTINE GRIBREAD (IUNIT, LENREC, LEFT, NCOPIED, RECORD, KODE)

!----------------------------------------------------------------------
! SUBROUTINE   : GRIBREAD
!
! PURPOSE      : TO READ THE NEXT RECORD FROM A GRIB DATA SET.
!
! DESCRIPTION  : 'GRIBREAD' READS THE NEXT RECORD FROM A GRIB DATA
!                SET. IF IT IS THE FIRST RECORD, THE START OF THE
!                MESSAGE ('GRIB') IS LOCATED AND THE TOTAL MESSAGE
!                LENGTH DECODED AND RETURNED. IF IT CONTAINS ALL OR
!                PART OF THE END-OF-MESSAGE INDICATOR ('7777') THIS
!                WILL BE CHECKED.
!
!                EACH GRIB DATA SET IS ASSUMED TO CONTAIN ONLY ONE
!                GRIB MESSAGE.
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
! $Revision: 2$
! $Date: 22/07/2011 14:48:55$
! $Source: /data/us0400/mdb/op/lib/source/RCS/gribread.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         22/07/2011 14:48:55    Richard Weedon
!       updated for radar doppler winds
!  1    Met_DB_Project 1.0         30/01/2006 20:22:43    Sheila Needham  
! $
! Revision 2.1  2002/06/10  15:11:53  15:11:53  usmdb (Generic MetDB account)
! 2.1.  17 June 2002.  Brian Barwell.  Change 45/02.
! Check whether GRIB-1 or GRIB-2 before extracting message length.
!
! Revision 2.0  2001/01/08  11:58:44  11:58:44  usmdb (Generic MDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.1  2000/12/08  14:17:15  14:17:15  usmdb (Generic MDB account)
! Initial revision
!
! ORIGINAL VERSION BY BRIAN BARWELL, OCTOBER 2000.
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
!                                                             VARIABLES
!
      INTEGER ICHAR3    ! ROUTINE TO CONVERT CHARACTER*3 TO INTEGER !2.1
      INTEGER IGRIB     ! LOCATION OF START OF 'GRIB' IN FIRST RECORD
      INTEGER IOS       ! STATUS CODE FROM READ STATEMENT
      INTEGER IUNIT     ! UNIT NUMBER FOR GRIB DATA SET
      INTEGER KODE      ! OUTPUT STATUS CODE - SEE ABOVE
      INTEGER LAST      ! EXPECTED LOCATION OF END OF GRIB MESSAGE
      INTEGER LEFT      ! SET TO NUMBER OF BYTES IN MESSAGE IF NEW D/S
      INTEGER LENREC    ! RECORD LENGTH OF GRIB DATA SET
      INTEGER MGRIB     ! GRIB EDITION NUMBER IN MESSAGE SECTION 0  !2.1
      INTEGER NCOPIED   ! BYTES IN RECORD BEFORE NEXT TO COPY
      INTEGER NFRST7    ! FIRST BYTE OF '7777' TO CHECK IN RECORD
      INTEGER NLAST7    ! LAST BYTE OF '7777' TO CHECK IN RECORD
      INTEGER NUM7      ! NUMBER OF BYTES OF '7777' TO CHECK IN RECORD
!
      LOGICAL FIRST     ! .TRUE. FOR FIRST CALL TO SUBROUTINE
!
      CHARACTER*4 GRIB, SEVENS  ! 'GRIB' AND '7777' IN ASCII
      CHARACTER*132 HEAD        ! FOR REVISION INFORMATION
      CHARACTER*(*) RECORD      ! TO HOLD ONE RECORD OF GRIB DATA
!                                                                  DATA
      DATA FIRST /.TRUE./
!                                                                 SAVES
      SAVE FIRST, GRIB, SEVENS
!                                REVISION INFORMATION & INITIALISATIONS
      IF (FIRST) THEN
         HEAD='
     &   $Source: /data/us0400/mdb/op/lib/source/RCS/gribread.F,v $
     &   '//'$Date: 22/07/2011 14:48:55$ $Revision: 2$'
!
         GRIB   = CHAR(71) // CHAR(82) // CHAR(73) // CHAR(66) ! 'GRIB'
         SEVENS = CHAR(55) // CHAR(55) // CHAR(55) // CHAR(55) ! '7777'
         FIRST  = .FALSE.
      END IF
!                          READ THE NEXT RECORD AND CHECK FOR I/O ERROR
!
      READ (IUNIT, '(A)', IOSTAT=IOS) RECORD(1:LENREC)
!
*#######################################################################
*OLD  IF (IOS.NE.0) THEN
!                              I/O ERROR: PRINT MESSAGE AND SET KODE=16
      IF (IOS.GT.0) THEN
*#######################################################################
         WRITE (6,'(T5,A,T15,A,I4)') 'GRIBREAD:',
     &            'I/O ERROR READING GRIB DATA SET.  STATUS =',IOS
         KODE = 16
         RETURN
* NEW ##################################################################
!                          END OF DATA SET: SET KODE=8 TO GET IT CLOSED
      ELSE IF (IOS.LT.0) THEN
         KODE = 8
         RETURN
*#######################################################################
!                                              FIRST RECORD OF DATA SET
      ELSE IF (LEFT.LE.0) THEN
!                                                         LOCATE 'GRIB'
         IGRIB = INDEX(RECORD(1:LENREC-15),GRIB)                    !2.1
         IF (IGRIB.EQ.0) THEN
            WRITE (6,'(T5,A,T15,A,I4)') 'GRIBREAD:',
     &               'START OF GRIB MESSAGE NOT FOUND ON UNIT ', IUNIT
            KODE = 8
            RETURN
         ELSE
!                              'GRIB' FOUND. (ANYTHING BEFORE 'GRIB' IS
!                                NOT WANTED SO SET NCOPIED TO SKIP IT.)
            NCOPIED = IGRIB - 1
            MGRIB = ICHAR(RECORD(IGRIB+7:IGRIB+7)) ! GRIB edition   !2.1
         END IF
!                                                 DECODE MESSAGE LENGTH
         IF (MGRIB.EQ.1) THEN                                       !2.1
            LEFT = ICHAR3(RECORD(IGRIB+4:IGRIB+6))                  !2.1
         ELSE IF (MGRIB.EQ.2) THEN                                  !2.1
            LEFT = 256*ICHAR3(RECORD(IGRIB+12:IGRIB+14)) +          !2.1
     &                 ICHAR (RECORD(IGRIB+15:IGRIB+15))            !2.1
         END IF                                                     !2.1
      ELSE
!                                         NOT FIRST RECORD OF DATA SET.
!                              WHOLE RECORD IS WANTED SO SET NCOPIED=0.
         NCOPIED = 0
      END IF
!                     CHECK WHETHER END OF MESSAGE HAS NOW BEEN READ IN
      KODE = 4
      LAST = LEFT + NCOPIED
      IF (LAST.LT.LENREC) KODE = 0  ! (NO MORE RECORDS TO READ)
!
!                                   CHECK FOR '7777' AT END OF MESSAGE.
!                  (NOTE: '7777' MAY SPAN MORE THAN ONE RECORD SO CHECK
!                       HOW MANY 7'S THERE ARE IN CURRENT RECORD FIRST)
!
      NFRST7 = MAX0(LAST-3,1)
      NLAST7 = MIN0(LAST,LENREC)
      NUM7  = NLAST7 - NFRST7 + 1
      IF (NUM7.GE.1 .AND. RECORD(NFRST7:NLAST7).NE.SEVENS(1:NUM7)) THEN
!
!                                            PRINT MESSAGE IF NOT FOUND
!
         WRITE (6,'(T5,A,T15,A,I4)') 'GRIBREAD:',
     &         'END OF GRIB MESSAGE NOT FOUND ON UNIT ', IUNIT
         KODE = 16
      END IF
!                                             RETURN TO CALLING PROGRAM
      RETURN
      END
