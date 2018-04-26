MODULE tableb_mod
  INTERFACE
    SUBROUTINE TABLEB(X,Y,VER,SCALE,REFVAL,WIDTH,IFORMAT,NAME,UNIT)
      IMPLICIT NONE
      INTEGER         ,INTENT(IN)  :: X           ! Descriptor
      INTEGER         ,INTENT(IN)  :: Y           ! Descriptor
      INTEGER         ,INTENT(IN)  :: VER         ! Requested version
      INTEGER         ,INTENT(OUT) :: SCALE       ! Scale
      INTEGER         ,INTENT(OUT) :: REFVAL      ! Reference value
      INTEGER         ,INTENT(OUT) :: WIDTH       ! Bit width
      CHARACTER(LEN=*),INTENT(OUT) :: IFORMAT     ! Format
      CHARACTER(LEN=*),INTENT(OUT) :: NAME        ! Name
      CHARACTER(LEN=*),INTENT(OUT) :: UNIT        ! Units
    END SUBROUTINE TABLEB
  END INTERFACE
END MODULE tableb_mod
