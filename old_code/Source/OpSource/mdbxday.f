      SUBROUTINE MDBXDAY(REQ,REQS)
      IMPLICIT NONE

!----------------------------------------------------------------------
!
! PROGRAM       : MDBXDAY
!
! PURPOSE       : To copy an MDB request,
!                 replacing any TODAY or TODAY-n by an explicit date.
!
! DATA TYPES    : any retrievable using MDB requests
!
! CALLED BY     : MDBX
!
! CALLS         : DATE31,DATE13
!
! PARAMETERS    : (1) input request
!                 (2) output request
!
!----------------------------------------------------------------------
      CHARACTER*(*) REQ
      CHARACTER*(*) REQS

      INTEGER IST            ! pointer to START TIME
      INTEGER LST            ! pointer to END TIME
      INTEGER IXSLASH        ! pointer to date/time slash in start time
      INTEGER IXTODAY        ! nonzero if date is TODAY-n

      INTEGER NOW(8)         ! current date/time
      INTEGER CENDAY         ! century-day
      INTEGER N              ! days back from today
      INTEGER YEAR           ! year to go in start or end time
      INTEGER MONTH
      INTEGER DAY

! Find start time and slash between date & time.
! See if the date is explicit or TODAY-n.

      IST=INDEX(REQ,'START TIME ')
      IXSLASH=INDEX(REQ(IST:),'/')
      IXTODAY=INDEX(REQ(IST:IST+IXSLASH-1),'TODAY')

! If TODAY-n, find the corresponding century-day & put it in the
! output request as year, month, day.

      REQS(1:11)='START TIME '
      IF (IXTODAY.GT.0) THEN
        CALL DATIM(NOW)
        CALL DATE31(NOW(6),NOW(7),NOW(8),CENDAY)
        IF (IXSLASH-IXTODAY.GT.5) THEN
          READ (REQ(IST+IXTODAY+4:IST+IXSLASH-1),*) N
          CENDAY=CENDAY+N
        ENDIF
        CALL DATE13(CENDAY,DAY,MONTH,YEAR)
        WRITE (REQS(12:19),'(I4.4,2I2.2)') YEAR,MONTH,DAY
      ELSE
        REQS(12:19)=REQ(IST+IXSLASH-9:IST+IXSLASH-2) ! copy yyyymmdd
      ENDIF
      REQS(20:26)=REQ(IST+IXSLASH-1:IST+IXSLASH+5)   ! copy '/hhmmZ '

! Find end time and slash between date & time.
! See if the date is explicit or TODAY-n.

      REQS(27:35)='END TIME '
      LST=INDEX(REQ,' END TIME ')
      IXSLASH=INDEX(REQ(LST:),'/')
      IXTODAY=INDEX(REQ(LST:LST+IXSLASH-1),'TODAY')

! If TODAY-n, find the corresponding century-day & put it in the
! output request as year, month, day.

      IF (IXTODAY.GT.0) THEN
        CALL DATIM(NOW)
        CALL DATE31(NOW(6),NOW(7),NOW(8),CENDAY)
        IF (IXSLASH-IXTODAY.GT.5) THEN
          READ (REQ(LST+IXTODAY+4:LST+IXSLASH-1),*) N
          CENDAY=CENDAY+N
        ENDIF
        CALL DATE13(CENDAY,DAY,MONTH,YEAR)
        WRITE (REQS(36:43),'(I4.4,2I2.2)') YEAR,MONTH,DAY
      ELSE
        REQS(36:43)=REQ(LST+IXSLASH-9:LST+IXSLASH-2) ! copy yyyymmdd
      ENDIF

! Finally copy the rest of the request (from /hhmmZ in END TIME).

      REQS(44:)=REQ(LST+IXSLASH-1:)
      RETURN
      END
