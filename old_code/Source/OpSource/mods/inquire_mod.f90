MODULE INQUIRE_MOD
  INTERFACE
    LOGICAL FUNCTION INQUIRE(filename,mode)
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*),INTENT(IN) :: filename     ! dsn or ddname
      CHARACTER(LEN=*),INTENT(IN) :: mode         ! 'DSN' or 'DDN'      
    END FUNCTION INQUIRE  
  END INTERFACE
END MODULE INQUIRE_MOD