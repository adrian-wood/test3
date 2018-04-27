      FUNCTION VALDDY (DAY,MONTH,YEAR)

!-----------------------------------------------------------------------
!                                                                  
! PROGRAM       : VALDDY                                              
!                                                                  
! PURPOSE       : this subroutine checks the date input for validity  
!                                                                  
! DESCRIPTION   : A date is passed to the subroutine.  This is        
!                 checked for validity and is TRUE for valid day and  
!                 FALSE for invalid. Routine expires 2399 so write a  
!                 replacement before then! Only valid from 1753       
!                                                                  
!                                                                  
! CALLS         : MNTHDS                                             
!                                                                  
! CALLED BY     : OBHOUR                                             
!                                                                  
! PARAMETERS    : 1. DAY    - INPUT DAY                               
!                 2. MONTH  - INPUT MONTH                             
!                 3. YEAR   - INPUT YEAR                              
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:58$
! $Source: /home/us0400/mdb/op/lib/source/RCS/valddy.F,v $
!                                                                  
! CHANGE RECORD :                                                   
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:58    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:39  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  98/09/16  15:29:01  15:29:01  usmdb (Generic MetDB account)
! Initial revision
!
! 06/08/1998 Initial version based on DATCHK in ZPDATE             
!                                                                  
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

C     INPUT ARGUMENTS

      INTEGER DAY,MONTH,YEAR

C     LOCAL VARIABLES

      INTEGER MNTHDS   ! function in zpdate that gives days of month

      LOGICAL VALDDY

      CHARACTER*132 HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/valddy.F,v $
     &$Revision: 1$ $Date: 30/01/2006 20:25:58$'

C Check that within valid year range
C Check that within valid month range
C and check that within valid day range

      IF ((YEAR .GE. 1753) .AND.
     &    (YEAR .LE. 2399) .AND.
     &    (MONTH .GE. 1) .AND.
     &    (MONTH .LE. 12)   .AND.
     &    (DAY .GE. 1) .AND.
     &    (DAY .LE. MNTHDS(MONTH,YEAR))) THEN
C Valid date
              VALDDY=.TRUE.
      ELSE
              VALDDY=.FALSE.
      ENDIF

      END
