 MODULE readcf_mod
   INTERFACE
     SUBROUTINE READCF(INDX,TEXT,NCODES,N)
       IMPLICIT NONE
       CHARACTER(LEN=1),INTENT(OUT):: INDX(:)  ! array to read tables into
       CHARACTER(LEN=1),INTENT(OUT):: TEXT(:)  ! array to read tables into
       INTEGER         ,INTENT(INOUT) :: NCODES ! number of code tables (elements)
       INTEGER         ,INTENT(INOUT) :: N      ! pointer to character array TEXT
     END SUBROUTINE READCF
   END INTERFACE
 END MODULE readcf_mod
