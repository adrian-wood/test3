LOGICAL FUNCTION ISALEAP(IY)
!
!     Returns .TRUE. if IY is a Leap year
!     Returns .FALSE. if IY is not a Leap year
!
  IMPLICIT NONE
!					INPUT ARGUMENT
  INTEGER, INTENT(IN) :: IY


  IF (IY/4*4 .NE. IY) THEN			 ! Divide by 4
     ISALEAP=.FALSE.
  ELSE  		
    IF (IY/400*400 .EQ. IY) THEN	 ! Century check
       ISALEAP=.TRUE.
    ELSE
	  IF (IY/100*100 .EQ. IY) THEN   ! Century qualifier
         ISALEAP=.FALSE.
      ELSE
        ISALEAP=.TRUE.
      ENDIF
    ENDIF
  ENDIF
END FUNCTION ISALEAP
