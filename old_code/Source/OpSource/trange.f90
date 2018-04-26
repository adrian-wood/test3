SUBROUTINE TRANGE (VALUES, NUMOBS, NPOS, NTIME)

!-----------------------------------------------------------------------
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
! $Workfile: trange.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 26/06/2012 17:02:49$
!
! CHANGE RECORD:
!
! $Log:
!  5    MetDB_Refresh 1.4         26/06/2012 17:02:49    Brian Barwell   Treat
!       missing data in seconds as zero.
!  4    MetDB_Refresh 1.3         28/01/2011 12:50:53    Alison Weir     Change
!        following review - use RMISS constant
!  3    MetDB_Refresh 1.2         27/01/2011 14:40:14    Richard Weedon  USE
!       statement removed, added in error
!  2    MetDB_Refresh 1.1         27/01/2011 14:37:18    Richard Weedon
!       Revision info template added
!  1    MetDB_Refresh 1.0         27/01/2011 14:31:53    Richard Weedon
!       Initial port passes compilation test with USE statement for index1.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE metdb_com_mod, only : RMISS

IMPLICIT NONE
!                                                            Variables
! Arguments
INTEGER,INTENT(IN)      ::   NUMOBS            ! Number of observations
                                               ! in VALUES
REAL,INTENT(IN)         ::   VALUES(NUMOBS,*)  ! Values decoded
                                               ! from BUFR bulletin
INTEGER,INTENT(IN)      ::   NPOS(6)           ! Positions of
                                               ! date/time in VALUES
INTEGER,INTENT(OUT)     ::   NTIME(6,2)        ! Yr,mon,day,hr,min &
                                               ! sec of ob. time window
! Variables
INTEGER       ::   J          ! Loop variable for date/time elements
INTEGER       ::   JOB        ! Loop variable for observation loop
INTEGER       ::   NUMPOS     ! Number of valid date/time elements

LOGICAL       ::   GOODTIME   ! Flag for no missing data in date/time
LOGICAL       ::   BETTER     ! Flag for new oldest or newest date/time
LOGICAL       ::   DIFFT      ! Flag for different date/time from stored

REAL          ::   TIME1(6)   ! Oldest date/time found so far
REAL          ::   TIME2(6)   ! Newest date/time found so far

!-----------------------------------------------------------------------
!     NUMBER OF VALID DATE/TIME ELEMENTS (6 IF SECONDS PRESENT, ELSE 5)
!-----------------------------------------------------------------------

IF (NPOS(6) <= 0) THEN  ! seconds not available
   NUMPOS = 5
   TIME1(6) = 0.0
   TIME2(6) = 0.0
ELSE
   NUMPOS = 6
END IF

!-----------------------------------------------------------------------
!     INITIALISE DATE/TIME ARRAYS FOR OLDEST AND NEWEST DATA
!-----------------------------------------------------------------------

DO J=1,NUMPOS
   TIME1(J) = -RMISS    !ie, positive value
   TIME2(J) =  RMISS
END DO

!-----------------------------------------------------------------------
!     LOOP OVER OBSERVATIONS CHECKING OBSERVATION TIMES
!  Note: seconds may be missing data while rest of date/time is OK.  !5
!-----------------------------------------------------------------------

do_constr1 : &
DO JOB=1,NUMOBS
!                      Check for missing data (value < 0) - not seconds
   GOODTIME = .TRUE.
   DO J=1,5                                                          !5
      IF (VALUES(JOB,NPOS(J)) < 0.0) GOODTIME = .FALSE.
   END DO

   if_constr1 : &
   IF (GOODTIME) THEN

!-----------------------------------------------------------------------
!           CHECK FOR NEWEST OBSERVATION TIME SO FAR
!-----------------------------------------------------------------------

      DIFFT = .FALSE.
      BETTER = .FALSE.
      J = 1
!                      Check for date/time different from newest so far

      DO WHILE (.NOT.DIFFT .AND. J <= NUMPOS)
         DIFFT = VALUES(JOB,NPOS(J)) /= TIME2(J)
         IF (DIFFT) BETTER = VALUES(JOB,NPOS(J)) > TIME2(J)
         J = J + 1
      END DO
!                                  If newest yet found, store date/time
      IF (BETTER) THEN
         DO J=1,NUMPOS
            TIME2(J) = VALUES(JOB,NPOS(J))
         END DO
      END IF

!-----------------------------------------------------------------------
!           CHECK FOR OLDEST OBSERVATION TIME SO FAR
!-----------------------------------------------------------------------

      DIFFT = .FALSE.
      BETTER = .FALSE.
      J = 1
!                      Check for date/time different from oldest so far

      DO WHILE (.NOT.DIFFT .AND. J <= NUMPOS)
         DIFFT = VALUES(JOB,NPOS(J)) /= TIME1(J)
         IF (DIFFT) BETTER = VALUES(JOB,NPOS(J)) < TIME1(J)
         J = J + 1
      END DO
!                                  If oldest yet found, store date/time
      IF (BETTER) THEN
         DO J=1,NUMPOS
            TIME1(J) = VALUES(JOB,NPOS(J))
         END DO
      END IF
   END IF if_constr1
END DO  do_constr1

!-----------------------------------------------------------------------
!     CONVERT DATE/TIMES TO INTEGERS AND RETURN
!-----------------------------------------------------------------------

DO J=1,6
   IF (TIME1(J) > 9999.0) TIME1(J) = RMISS
   NTIME(J,1) = NINT(TIME1(J))
   NTIME(J,2) = NINT(TIME2(J))
END DO
!                                   Set seconds to zero if missing data
IF (NTIME(6,1) < 0) NTIME(6,1) = 0                                   !5
IF (NTIME(6,2) < 0) NTIME(6,2) = 0                                   !5

RETURN
END SUBROUTINE TRANGE
