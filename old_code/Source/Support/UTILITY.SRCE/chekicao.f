      PROGRAM CHEKICAO

!-----------------------------------------------------------------------
!
! PROGRAM     : CHEKICAO
!
! PURPOSE :
!
!    To check that ICAO identifiers listed in an ICAO list for use by
!    MetDB storage are in alphabetical order.
!
! DESCRIPTION :
!
!    CHEKICAO checks the ICAO identifiers listed in the ICAO list and
!    prints out a warning message for any duplicated entries and any
!    entries not in alphabetical order.
!
!    In addition, the printout on unit 6 will also give the last ICAO
!    identifier in the list and the total number of entries. The latter
!    may need to be checked against the parameter MAXICAO in subroutine
!    ICOBRV which controls array sizes. This is currently (24/09/2010)
!    set to 10000.
!
!    The ICAO list needs to be supplied on a JCL statement with a DD
!    name of STNICAO, e.g.
!
!        //STNICAO DD DSN=MDB.ICAO.LIST,LABEL=(,,,IN),DISP=SHR
!
! REVISION INFO :
!
! $Workfile: chekicao.f$ $Folder: UTILITY.SRCE$
! $Revision: 1$ $Date: 07/10/2010 12:02:33$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         07/10/2010 12:02:33    Brian Barwell   New
!       utility to check ICAO list.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IRC             ! Return code from I/O statement
      INTEGER NSTN            ! Counter for ICAO identifiers

      CHARACTER*4 NEWSTN      ! Identifier of current station
      CHARACTER*4 OLDSTN      ! Identifier of previous station

      LOGICAL MORE            ! Flag for more entries to read
!                                                                 Title
      WRITE (6,'(/T3,A/)') 'Details of ICAO list:'
!                                                        Open ICAO list
      OPEN (82, FILE='STNICAO', FORM='FORMATTED',
     &      IOSTAT=IRC, ACTION='READ')
!                                                   Check for I/O error
      IF (IRC.NE.0) THEN
        WRITE(6,'(T6,A,I5/)') 'Error opening ICAO list - IOSTAT =', IRC
      ELSE
!                                                       Initialisations
        NSTN = 0          ! Count of ICAO identifiers
        OLDSTN = '    '   ! Last identifier
!                                               Loop over table entries
        MORE = .TRUE.
        DO WHILE (MORE)
!                                              Read first line of entry
          READ (82,'(A4)',IOSTAT=IRC) NEWSTN
!                                                   Check for I/O error
          IF (IRC.GT.0) THEN
            WRITE(6,'(T6,A,I5/)')
     &              'Error reading ICAO list - IOSTAT =', IRC
            MORE = .FALSE.
!                                             Check for end of data set
          ELSE IF (IRC.LT.0) THEN
            WRITE (6,'(T6,4A)')
     &               'Premature end of data set after ', OLDSTN
            MORE = .FALSE.
!                                                 Check for end of list
          ELSE IF (NEWSTN.EQ.'    ') THEN
            MORE = .FALSE.

          ELSE
            NSTN = NSTN + 1
!                                        Check for station out of order
            IF (NEWSTN.LT.OLDSTN) THEN
              WRITE (6,'(T6,4A)')
     &                 'Incorrect order: ', NEWSTN, ' follows ', OLDSTN

!                                             Check for duplicate entry
            ELSE IF (NEWSTN.EQ.OLDSTN) THEN
              WRITE (6,'(T6,2A)') 'Duplicate entry for ', NEWSTN
            END IF
!                                       Remember new station identifier
            OLDSTN = NEWSTN
          END IF
        END DO
!                     Print last identifier and total number of entries

        WRITE (6,'(/T3,A,A5)') 'Last entry in ICAO list is for', OLDSTN
        WRITE (6,'(/T3,A,I6)') 'Number of entries in ICAO list is',NSTN

!                                                       Close ICAO list
        CLOSE (82)
      END IF

      STOP
      END
