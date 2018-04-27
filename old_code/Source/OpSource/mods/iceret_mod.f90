MODULE ICERET_MOD
  INTERFACE
    SUBROUTINE ICERET (REQTIME,ARRAY,NOBS,NELEM,NEXTOB,SUBS,NSUB,  &
                       REQID,STATUS,ERROR,DISK,CSTR,CREP,LCHREP,   &
                       ORDER,FOUND,VERSION)
    IMPLICIT NONE
!                   Subroutine arguments

    INTEGER         REQTIME(9) ! Requested retrieval start and end
                               ! times.
    INTEGER         NOBS       ! Number of observations requested.
    INTEGER         NELEM      ! Number of elements requested.
    REAL     ARRAY(NOBS,NELEM) ! Array to hold retrieved data.
    INTEGER         NEXTOB     ! The last observation to be retrieved
                               ! on exit.
    INTEGER         NSUB       ! Number of subscripts in array SUBS.
    INTEGER         SUBS(NSUB) ! Array of subscripts.
    CHARACTER(9)    REQID(:)   ! User's requested identifiers.
    INTEGER         STATUS     ! Status flag used to start or continue
                               ! a retrieval and a return code to
                               ! provide the user with information on
                               ! the completed retrieval.
    INTEGER         ERROR      ! Return code value for errors.
    INTEGER         DISK(5)    ! Dataset allocation details.
    CHARACTER(*)    CSTR(NOBS) ! Returns character element data.
    CHARACTER(*)    CREP(NOBS) ! Returns the unexpanded report.
    LOGICAL         LCHREP     ! True if only report is required.
    CHARACTER(1)    ORDER      ! Specifies the order which the reports
                               ! should be returned to the user.
    LOGICAL         FOUND(:)   ! Keyword flags.
    INTEGER         VERSION    ! The number of versions of a report
                               ! to be retrieved.

    END SUBROUTINE ICERET
  END INTERFACE
END MODULE ICERET_MOD
