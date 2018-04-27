      SUBROUTINE TRANGE (VALUES, NUMOBS, NPOS, NTIME)
!
!-----------------------------------------------------------------------
!
! SUBROUTINE  : TRANGE
!
! PURPOSE     : To find the time range of observations in a BUFR
!               message.
!
! DESCRIPTION : TRANGE loops through all the observations in a BUFR
!               message and extracts the dates and times of the
!               earliest and latest, returning them in the 6x2 array
!               NTIME. (The '6' represents year, month, day, hour,
!               minute and second.) Times containing any missing data
!               are ignored.
!
!               The array VALUES contains the partially decoded BUFR
!               message with data for NUMOBS observations, and the
!               6-element array NPOS contains the second subscripts of
!               VALUES corresponding to the six date/time elements.
!
! USAGE       : CALL TRANGE (VALUES, NUMOBS, NPOS, NTIME)
!
! PARAMETERS  : ('I'=Input, 'O'=Output)
!
!               VALUES (I) Real array of size (NUMOBS,*) with decoded
!                          coordinate values from BUFR message.
!               NUMOBS (I) Number of observations in BUFR message.
!               NPOS   (I) 6-element array with positions (i.e. 2nd
!                          subscript of VALUES) for year, month, day,
!                          hour, minute and second respectively.
!                          (Specify NPOS(6)=0 if seconds not available)
!               NTIME  (O) 6x2 integer array containing year, month,
!                          day, hour, minute and second of oldest and
!                          newest observations in BUFR message.
!
! CALLED BY   : INDEX1
!
! CALLS       : None.
!
! HISTORY     : Original version by Brian Barwell, 1 November 2000.
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:27$
! $Source: /home/us0400/mdb/op/lib/source/RCS/trange.F,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:27    Sheila Needham  
! $
! Revision 2.0  2001/06/06 10:20:57  usmdb
! Initial version
!
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
!
      IMPLICIT NONE
!                                                             Variables
!
      INTEGER J              ! Loop variable for date/time elements
      INTEGER JOB            ! Loop variable for observation loop
      INTEGER NPOS(6)        ! Positions of date/time in VALUES
      INTEGER NTIME(6,2)     ! Yr,mon,day,hr,min,sec of ob. time window
      INTEGER NUMOBS         ! Number of observations in VALUES
      INTEGER NUMPOS         ! Number of valid date/time elements
!
      LOGICAL GOODTIME       ! Flag for no missing data in date/time
      LOGICAL BETTER         ! Flag for new oldest or newest date/time
      LOGICAL DIFFT          ! Flag for different date/time from stored
      LOGICAL FIRST          ! .TRUE. if first call to this routine
!
      REAL VALUES(NUMOBS,*)  ! Values decoded from BUFR bulletin
      REAL TIME1(6)          ! Oldest date/time found so far
      REAL TIME2(6)          ! Newest date/time found so far
!
      CHARACTER HEAD*132     ! Revision details
!                                                        Saved variable
      SAVE FIRST
!                                                   Data initialisation
      DATA FIRST /.TRUE./
!                                                  Revision information
      IF (FIRST) THEN
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/trange.F,v $
     &   '//'$Date: 30/01/2006 20:25:27$ $Revision: 1$'
         FIRST = .FALSE.
      END IF
!
!-----------------------------------------------------------------------
!     NUMBER OF VALID DATE/TIME ELEMENTS (6 IF SECONDS PRESENT, ELSE 5)
!-----------------------------------------------------------------------
!
      IF (NPOS(6).LE.0) THEN  ! seconds not available
         NUMPOS = 5
         TIME1(6) = 0.0
         TIME2(6) = 0.0
      ELSE
         NUMPOS = 6
      END IF
!
!-----------------------------------------------------------------------
!     INITIALISE DATE/TIME ARRAYS FOR OLDEST AND NEWEST DATA
!-----------------------------------------------------------------------
!
      DO J=1,NUMPOS
         TIME1(J) = +9999999.0
         TIME2(J) = -9999999.0
      END DO ! J
!
!-----------------------------------------------------------------------
!     LOOP OVER OBSERVATIONS CHECKING OBSERVATION TIMES
!-----------------------------------------------------------------------
!
      DO JOB=1,NUMOBS
!                              Check for missing data (negative values)
         GOODTIME = .TRUE.
         DO J=1,NUMPOS
            IF (VALUES(JOB,NPOS(J)).LT.0.0) GOODTIME = .FALSE.
         END DO ! J
!
         IF (GOODTIME) THEN
!
!-----------------------------------------------------------------------
!           CHECK FOR NEWEST OBSERVATION TIME SO FAR
!-----------------------------------------------------------------------
!
            DIFFT = .FALSE.
            BETTER = .FALSE.
            J = 1
!                      Check for date/time different from newest so far
!
            DO WHILE (.NOT.DIFFT .AND. J.LE.NUMPOS)
               DIFFT = VALUES(JOB,NPOS(J)).NE.TIME2(J)
               IF (DIFFT) BETTER = VALUES(JOB,NPOS(J)).GT.TIME2(J)
               J = J + 1
            END DO ! J
!                                  If newest yet found, store date/time
            IF (BETTER) THEN
               DO J=1,NUMPOS
                  TIME2(J) = VALUES(JOB,NPOS(J))
               END DO ! J
            END IF
!
!-----------------------------------------------------------------------
!           CHECK FOR OLDEST OBSERVATION TIME SO FAR
!-----------------------------------------------------------------------
!
            DIFFT = .FALSE.
            BETTER = .FALSE.
            J = 1
!                      Check for date/time different from oldest so far
!
            DO WHILE (.NOT.DIFFT .AND. J.LE.NUMPOS)
               DIFFT = VALUES(JOB,NPOS(J)).NE.TIME1(J)
               IF (DIFFT) BETTER = VALUES(JOB,NPOS(J)).LT.TIME1(J)
               J = J + 1
            END DO ! J
!                                  If oldest yet found, store date/time
            IF (BETTER) THEN
               DO J=1,NUMPOS
                  TIME1(J) = VALUES(JOB,NPOS(J))
               END DO ! J
            END IF
         END IF
      END DO ! JOB
!
!-----------------------------------------------------------------------
!     CONVERT DATE/TIMES TO INTEGERS AND RETURN
!-----------------------------------------------------------------------
!
      DO J=1,6
         IF (TIME1(J).GT.9999.0) TIME1(J) = -9999999.0
         NTIME(J,1) = NINT(TIME1(J))
         NTIME(J,2) = NINT(TIME2(J))
      END DO ! J
!
      RETURN
      END
