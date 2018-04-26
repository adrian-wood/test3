MODULE tafint_mod
  INTERFACE
    SUBROUTINE TAFINT(REXP,CNAM,RLAT,RLON,YR,MON,DAY,HR,MNT,BULCOD, &
                      CCCC,THISID,TAFLEN)

    IMPLICIT NONE

! Subroutine arguments:

    REAL,         INTENT(INOUT) ::  REXP(:) ! Expansion array.   !2.1
    CHARACTER(*), INTENT(INOUT) ::  CNAM ! Character element data. Used with
                                         ! function GETCHR to expand numeric
                                         ! value into text string.
    REAL,         INTENT(INOUT) ::  RLAT ! Station latitude.
    REAL,         INTENT(INOUT) ::  RLON ! Station longitude.
    INTEGER,      INTENT(INOUT) ::  YR ! Year of report.
    INTEGER,      INTENT(INOUT) ::  MON ! Month of report.
    INTEGER,      INTENT(INOUT) ::  DAY ! Day of report.
    INTEGER,      INTENT(IN)    ::  HR ! Hour of report.
    INTEGER,      INTENT(INOUT) ::  MNT ! Minute of report.
    CHARACTER(*), INTENT(INOUT) ::  BULCOD ! Bulletin identifier.
    CHARACTER(*), INTENT(INOUT) ::  CCCC ! Originating centre.
    CHARACTER(*), INTENT(INOUT) ::  THISID ! Station identifier.
    CHARACTER(*), INTENT(INOUT) ::  TAFLEN ! TAF length (either long or short)

    END SUBROUTINE TAFINT
  END INTERFACE
END MODULE tafint_mod
