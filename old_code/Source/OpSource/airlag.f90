SUBROUTINE AIRLAG(BULL,YYGGGG,SPAN,RC)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRLAG
!
! PURPOSE       : To find the time span of reports in a bulletin
!
! DESCRIPTION   : Delimit reports by equal signs, look in each report
!                 for first 4-figure group, keep greatest difference
!                 in hours between report & bulletin.
!
! CALLED BY     : AIRBUL
!
! PARAMETERS    : (1) bulletin (starting with the first report)      (I)
!                 (2) YYGGgg (day/hour/minute) from bulletin heading (I)
!                 (3) max diff b/n report & bulletin times (hours)   (O)
!                 (4) return code =0 if hour/minute in all reports,  (O)
!                                 =4 if a report had day/hour/minute
!                                 =8 if any report had no such group
!
!
! REVISION INFO:
!
! $Workfile: airlag.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 16/03/2011 12:23:20$
!
! CHANGE RECORD:
!
! $Log:
!  6    MetDB_Refresh 1.5         16/03/2011 12:23:20    Alison Weir     Amend
!       if statements to prevent substring out of range errors
!  5    MetDB_Refresh 1.4         25/01/2011 15:58:46    Richard Weedon
!       logical TIMED dec altered
!  4    MetDB_Refresh 1.3         25/01/2011 15:57:16    Richard Weedon  USE
!       ivalue_mod added and declaration of integer ivalue removed
!  3    MetDB_Refresh 1.2         25/01/2011 12:13:37    Richard Weedon  labels
!        for do constructs added
!  2    MetDB_Refresh 1.1         12/01/2011 15:58:10    Richard Weedon  Add
!       revision log etc
!  1    MetDB_Refresh 1.0         12/01/2011 15:54:32    Richard Weedon
!       Initial version, passed basic compilation test
! $
!
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
USE ivalue_mod
!
IMPLICIT NONE
! Parameters
CHARACTER(LEN=*),INTENT(IN)    ::   BULL   ! bulletin
CHARACTER(LEN=6),INTENT(IN)    ::   YYGGGG ! date/time from bull heading
INTEGER,INTENT(OUT)            ::   SPAN   ! max hour diff in bull
INTEGER,INTENT(OUT)            ::   RC     ! return code
!
! Variables
INTEGER                        ::   RSTART ! pointer to report start
INTEGER                        ::   REPLEN ! report length
INTEGER                        ::   REPEND ! report end
INTEGER                        ::   BULHOUR! bulletin hour
INTEGER                        ::   DAY    ! report day
INTEGER                        ::   HOUR   ! report hour
INTEGER                        ::   MINUTE ! report minute
INTEGER                        ::   DIFF   ! bull-report hour diff
INTEGER                        ::   I      ! loop variable
INTEGER                        ::   NGROUPS ! no of groups found so far


LOGICAL               ::  TIMED         ! set if good time group found

SAVE

BULHOUR=IVALUE(YYGGGG(3:4))
SPAN=-9999999
RC=0

! Split the bulletin into reports ending with equal signs.

RSTART=1
REPLEN=LEN(BULL)                      ! initialise to start loop
do_constr1 : &
DO WHILE (REPLEN > 0)
  REPLEN=INDEX(BULL(RSTART:),'=')     ! find next '='
  REPEND=RSTART+REPLEN                ! set report end from length
  IF (REPLEN == 0) REPEND=LEN(BULL)   ! end of bulletin if no '='

! Look for a four-figure group (delimited by spaces or CRLF)
! which could be hour & minute.  (Or 6-figure day/hour/minute)
!  It should be the 3rd or 4th group - 4th unless beacon reported
! instead lat/long or no space between lat & long.
! But there may still be ARP at the start - so try up to 5 groups.
! And make sure a 4-figure group isn't a latitude with N or S detached!

  I=RSTART                            ! report pointer for loop
  TIMED=.FALSE.                       ! false till time found
  NGROUPS=0                           ! only first few groups...

do_constr2 : &
  DO WHILE (.NOT.TIMED .AND. I < REPEND .AND. NGROUPS < 5)

IF_CONSTR1 : &    ! found the start of a group
    IF (BULL(I:I) <= ' ' .AND. BULL(I+1:I+1) > ' ') THEN
      NGROUPS=NGROUPS+1

IF_CONSTR2 : &    !is report long enough to hold a 4 figure group
      IF (I+5 <= REPEND) THEN

IF_CONSTR3: &     !is this a 4 figure group
        IF (IVALUE(BULL(I+1:I+4)) > 0 .AND. BULL(I+5:I+5) <= ' ') THEN

          IF (I+7 <= REPEND) THEN   !make sure this isn't a latitude
            IF (BULL(I+5:I+7) == ' N '.OR. &
                BULL(I+5:I+7) == ' S ') THEN
              ! it is - skip to next loop
              I=I+1
              CYCLE
            END IF
          END IF

! If 4 figures followed by space, see if figures could be hour & minute
! and, if so, find difference in hours.

          HOUR=IVALUE(BULL(I+1:I+2))
          MINUTE=IVALUE(BULL(I+3:I+4))
          IF (HOUR < 24 .AND. MINUTE < 60) THEN
            TIMED=.TRUE.
            DIFF=BULHOUR-HOUR
            IF (DIFF < 0) DIFF=DIFF+24
            IF (DIFF > SPAN) SPAN=DIFF
          ELSE
            RC=8
          END IF

        ELSE   ! not 4 figure group, is it a 6 figure group?

IF_CONSTR4: &     !is report long enough to hold a 6 figure group
          IF (I+7 <= REPEND) THEN

IF_CONSTR5: &     !is this a 6 figure group
            IF (IVALUE(BULL(I+1:I+6)) > 0 .AND. &
                BULL(I+7:I+7) <= ' ') THEN

! 6-figure group including day is unambiguous, so not included in span

              print *,BULL(RSTART:I+7)
              DAY=IVALUE(BULL(I+1:I+2))
              HOUR=IVALUE(BULL(I+3:I+4))
              MINUTE=IVALUE(BULL(I+5:I+6))
              IF (DAY <= 31 .AND. HOUR < 24 .AND. MINUTE < 60) THEN
                TIMED=.TRUE.
                IF (RC == 0) RC=4
              ELSE
                RC=8
              END IF

            END IF  IF_CONSTR5
          END IF    IF_CONSTR4
        END IF      IF_CONSTR3

      END IF        IF_CONSTR2
    END IF          IF_CONSTR1
    I=I+1
  END DO do_constr2

! Move on to next report.

  RSTART=RSTART+REPLEN
END DO do_constr1
RETURN
END SUBROUTINE AIRLAG
