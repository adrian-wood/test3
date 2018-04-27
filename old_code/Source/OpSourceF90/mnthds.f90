!-----------------------------------------------------------------------
INTEGER FUNCTION MNTHDS(MONTH,YEAR)
!
!	Returns Days in Month from Month and Year.
!
  IMPLICIT NONE
!					INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: MONTH,YEAR
!					LOCAL VARIABLES
!  LOGICAL :: ISALEAP


SELECT CASE (MONTH)
  CASE (1,3,5,7,8,10,12)
    MNTHDS = 31
  CASE (4,6,9,11)
    MNTHDS = 30
  CASE (2)
    IF (ISALEAP(YEAR)) THEN
	  MNTHDS = 29
    ELSE
	  MNTHDS = 28
    ENDIF
  CASE DEFAULT
	PRINT *, "Error in function MNTHDS"
	RETURN
END SELECT

END FUNCTION MNTHDS
!-----------------------------------------------------------------------
