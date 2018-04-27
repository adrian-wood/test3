
      LOGICAL FUNCTION CHECK_PATTERN(NAME,DETAILS,NAME_LENGTH,POS,
     &                               STRING_LENGTH)                
!***11111111111111111111111111111111111111111111111111111111111111111111        
!                                                                               
! FILE NAME: check_pattern.f
!
! TYPE: function
!
! LANGUAGE: FORTRAN 77         
!                                                                               
!***22222222222222222222222222222222222222222222222222222222222222222222        
!                                                                               
! PURPOSE: To check if character pattern is in Station name part of
!          a line of the abreviated station master list.
!
!***33333333333333333333333333333333333333333333333333333333333333333333        
!                                                                               
! STRUCTURE: 
!  Function checks chunks of the Station Name to a match is found or end
!  of the line is reached. If found .TRUE. is returned otherwise .FALSE.
!  is returned.
!                                           
! REFERENCES:                                                                
!   None                                   
!                                                                               
!***44444444444444444444444444444444444444444444444444444444444444444444        
!                                                                               
! EXTERNAL ROUTINES:                                                             
!                                                                               
! EXTERNAL I/O (FILES) EXPLICITLY ALLOCATED                                           
!  INPUT:                                                                       
!    NONE                                                                       
!  INPUT/OUTPUT:                                                                
!    NONE                                                                       
!  OUTPUT :                                                                     
!    NONE                                                                       
!                                                                               
! EXTERNAL I/O (FILES) DYNAMICALLY ALLOCATED                                            
!   INPUT: NONE                                                                 
!   INPUT/OUTPUT: NONE                                                          
!   OUTPUT : NONE                                                               
!                                                                               
!***55555555555555555555555555555555555555555555555555555555555555555555        
!                                                                               
! RETURN CODES                                                       
!                                                                               
!***66666666666666666666666666666666666666666666666666666666666666666666
!
! INTERFACE
!  
!
!***77777777777777777777777777777777777777777777777777777777777777777777   
!
! Program written my Stanley Kellett, MetDB team April 2000     
!
! CHANGE HISTORY                                                                
!                                                                               
! $Revision: 1$
! $Date: 31/08/2006 15:57:25$
! $Source: /home/us0400/usmdb/station_search/source/RCS/check_pattern.F,v $
! 
! $Log:
!  1    Met_DB_Project 1.0         31/08/2006 15:57:25    Stan Kellett    
! $
! Revision 1.2  2001/09/04 08:34:39  usmdb
! updated for f90 compiler.
!
! Revision 1.1  2000/06/07 12:39:53  usmdb
! Initial revision
!
!                                                                               
!***88888888888888888888888888888888888888888888888888888888888888888888
        
      IMPLICIT NONE 

! Arguments IN:
      CHARACTER*42 NAME             ! Name Pattern for search
      CHARACTER*89 DETAILS          ! line of Station Details
      INTEGER*4    NAME_LENGTH      ! length of name pattern
      INTEGER*4    POS              ! Start position for searching 
                                    ! pattern
      INTEGER*4    STRING_LENGTH    ! length of string to be checked

! Local Variables:
      INTEGER*4             I       ! Loop control variable

! Code for Revision software
      CHARACTER*132 HEAD
      LOGICAL HEADSET               !1.2
      DATA    HEADSET /.FALSE./     !1.2

      IF (.NOT.HEADSET) THEN
        HEAD='
     &        $Source: /home/us0400/usmdb/station_search/source/RCS/check_pattern.F,v $
     &       '//'$Date: 31/08/2006 15:57:25$ $Revision: 1$'
        HEADSET=.TRUE.
      ENDIF

! Initialising variables:
      CHECK_PATTERN=.FALSE.         ! value returned if pattern not found

! Check Station name part of Station details line, if NAME is found
! within then return .TRUE.
      DO I=POS,(POS+(STRING_LENGTH-NAME_LENGTH))
         IF (DETAILS(I:(I+(NAME_LENGTH-1))).EQ.NAME(1:NAME_LENGTH)) THEN
            CHECK_PATTERN=.TRUE.
         ENDIF
      ENDDO

! Return and end
      RETURN
      END
