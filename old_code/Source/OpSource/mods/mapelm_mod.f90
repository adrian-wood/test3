MODULE MAPELM_MOD
INTERFACE
SUBROUTINE MAPELM(USRELM,NUSR,IXELEM,IXNUM,NXELEM, &
                        IREF,NREF,IREPL,LTEST)

IMPLICIT NONE

! Interface argument list

! Element names from request

INTEGER,           INTENT(IN)  :: NUSR       ! num of elements requested
CHARACTER (LEN=*), INTENT(IN)  :: USRELM(:)  ! element names with "instances"

! Index names & numbers (all elements for this data type)
! (The numbers refer to entries in a bit or array index used later.)

CHARACTER (LEN=*), INTENT(IN)  :: IXELEM(:)  ! element name
INTEGER,           INTENT(IN)  :: IXNUM(:)   ! element number
INTEGER,           INTENT(IN)  :: NXELEM     ! number of retrievable elements

! Numbers to be returned, corresponding to element names requested above
! (NREF may be greater than NUSR - there may be entities...)

INTEGER,           INTENT(OUT) :: IREF(:)    ! index numbers for requested elements
INTEGER,           INTENT(OUT) :: NREF       ! number of elements in IREF & IREPL
INTEGER,           INTENT(OUT) :: IREPL(:)   ! instance (or 1 if not replicated)

LOGICAL,           INTENT(IN)  :: LTEST      ! TRUE to print diagnostics

END SUBROUTINE MAPELM
END INTERFACE
END MODULE MAPELM_MOD
