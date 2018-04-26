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
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:40$
! $Source: /data/us0400/mdb/op/lib/source/RCS/airlag.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:40    Sheila Needham  
! $
! Revision 2.0  2002/03/07  15:56:53  15:56:53  usmdb (Generic MetDB account)
! Initial version
! 
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER RSTART        ! pointer to report start
      INTEGER REPLEN        ! report length
      INTEGER REPEND        ! report end
      INTEGER BULHOUR       ! bulletin hour
      INTEGER DAY           ! report day
      INTEGER HOUR          ! report hour
      INTEGER MINUTE        ! report minute
      INTEGER DIFF          ! bulletin-report hour difference
      INTEGER SPAN          ! max hour difference in bulletin
      INTEGER I             ! loop variable
      INTEGER IVALUE        ! figures-to-number conversion
      INTEGER RC            ! return code
      INTEGER NGROUPS       ! number of groups found so far

      CHARACTER*(*)  BULL   ! bulletin (starting with first report)
      CHARACTER*6    YYGGGG ! date/time from bulletin heading
      CHARACTER*132  HEAD   ! revision information

      LOGICAL TIMED         ! set if good time group found

      SAVE

      HEAD='$RCSfile: $ ' //
     &     '$Revision: 1$ $Date: 30/01/2006 20:20:40$'

      BULHOUR=IVALUE(YYGGGG(3:4))
      SPAN=-9999999
      RC=0

! Split the bulletin into reports ending with equal signs.

      RSTART=1
      REPLEN=LEN(BULL)                      ! initialise to start loop
      DO WHILE (REPLEN.GT.0)
        REPLEN=INDEX(BULL(RSTART:),'=')     ! find next '='
        REPEND=RSTART+REPLEN                ! set report end from length
        IF (REPLEN.EQ.0) REPEND=LEN(BULL)   ! end of bulletin if no '='

! Look for a four-figure group (delimited by spaces or CRLF)
! which could be hour & minute.  (Or 6-figure day/hour/minute)
!  It should be the 3rd or 4th group - 4th unless beacon reported
! instead lat/long or no space between lat & long.
! But there may still be ARP at the start - so try up to 5 groups.
! And make sure a 4-figure group isn't a latitude with N or S detached!

        I=RSTART                            ! report pointer for loop
        TIMED=.FALSE.                       ! false till time found
        NGROUPS=0                           ! only first few groups...
        DO WHILE (.NOT.TIMED .AND. I.LT.REPEND .AND. NGROUPS.LT.5)
          IF (BULL(I:I).LE.' ' .AND. BULL(I+1:I+1).GT.' ') THEN
            NGROUPS=NGROUPS+1
            IF (IVALUE(BULL(I+1:I+4)).GT.0 .AND.
     &          BULL(I+5:I+5).LE.' ' .AND.
     &          .NOT.BULL(I+5:I+7).EQ.' N '.AND.
     &          .NOT.BULL(I+5:I+7).EQ.' S ') THEN

! If 4 figures followed by space, see if figures could be hour & minute
! and, if so, find difference in hours.

              HOUR=IVALUE(BULL(I+1:I+2))
              MINUTE=IVALUE(BULL(I+3:I+4))
              IF (HOUR.LT.24 .AND. MINUTE.LT.60) THEN
                TIMED=.TRUE.
                DIFF=BULHOUR-HOUR
                IF (DIFF.LT.0) DIFF=DIFF+24
                IF (DIFF.GT.SPAN) SPAN=DIFF
              ELSE
                RC=8
              ENDIF

! 6-figure group including day is unambiguous, so not included in span

            ELSE IF (IVALUE(BULL(I+1:I+6)).GT.0 .AND.
     &               BULL(I+7:I+7).LE.' ') THEN
                     print *,BULL(RSTART:I+7)
              DAY=IVALUE(BULL(I+1:I+2))
              HOUR=IVALUE(BULL(I+3:I+4))
              MINUTE=IVALUE(BULL(I+5:I+6))
              IF (DAY.LE.31 .AND. HOUR.LT.24 .AND. MINUTE.LT.60) THEN
                TIMED=.TRUE.
                IF (RC.EQ.0) RC=4
              ELSE
                RC=8
              ENDIF
            ENDIF
          ENDIF
          I=I+1
        ENDDO

! Move on to next report.

        RSTART=RSTART+REPLEN
      ENDDO
      RETURN
      END
