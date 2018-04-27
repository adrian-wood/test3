      SUBROUTINE GETHDR (TYPES, NTYPES)
!
      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE    : GETHDR                                                
!                                                                      
! PURPOSE       : TO CREATE A DATA SET OF HEADER RANGES FOR IDENTIFYING 
!                 BULLETINS TO BE PROCESSED BY A STORAGE JOB.           
!                                                                      
! DESCRIPTION   : "GETHDR" READS A DATA SET GIVING A LIST OF GTS HEADER 
!                 RANGES AND PROCESSING DETAILS FOR BULLETINS RECEIVED  
!                 VIA A GIVEN STREAM FROM TROPICS AND CREATES A SIMILAR 
!                 DATA SET WITH DETAILS FOR ONLY THE BULLETINS REQUIRED 
!                 BY A PARTICULAR STORAGE JOB.                          
!                                                                      
!                 THE INPUT DATA SET MUST HAVE A DDNAME OF "GTSHEAD"    
!                 AND THE OUTPUT MUST HAVE A DDNAME OF "HEADERS" (AS    
!                 REQUIRED BY SUBROUTINE 'INITSTOR'). BOTH DATA SETS    
!                 MUST BE IN THE FORMAT REQUIRED BY 'INITSTOR'.         
!                 "GETHDR" OPENS THE DATA SETS ON UNITS 10 AND 11 BUT   
!                 BOTH ARE CLOSED BEFORE EXITING.                       
!                                                                      
!                 'GETHDR' WAS WRITTEN SO THAT MET.D.B. STORAGE JOBS    
!                 PROCESSING DATA FROM TROPICS DATA STREAM 'SDB1' WOULD 
!                 NOT NEED THEIR OWN DATA SET OF BULLETIN HEADERS BUT   
!                 COULD CREATE THE DATA SET THEMSELVES FROM A MASTER    
!                 LIST.  THIS AIDS MAINTENANCE AS OTHERWISE CHANGING    
!                 HEADERS WOULD MEAN REMEMBERING TO CHANGE TWO DATA     
!                 SETS IN THE SAME WAY.                                 
!                                                                      
! USAGE         : CALL GETHDR (TYPES, NTYPES)                           
!                                                                      
! PARAMETERS    : TYPES   I    CHARACTER*8 ARRAY CONTAINING A LIST OF   
!                              DATA TYPES OF BULLETINS TO BE STORED.    
!                              (UNUSED ELEMENTS SHOULD BE BLANKS.)      
!                                                                      
!                 NTYPES  I/O  (INPUT)  SIZE OF 'TYPES' ARRAY.          
!                              (OUTPUT) NUMBER OF ELEMENTS IN 'TYPES'   
!                                       ARRAY WITH VALID DATA TYPES.    
!                                                                      
! CALLED BY     : MET.D.B. STORAGE JOBS PROCESSING SOME BUT NOT ALL THE 
!                 BULLETINS RECEIVED VIA A SINGLE STREAM FROM TROPICS.  
!                                                                      
! FILES USED    : FILES "GTSHEAD" AND "HEADERS" OPENED (AND CLOSED) ON  
!                 UNITS 10 AND 11 (SEE DESCRIPTION ABOVE FOR DETAILS).  
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:33$
! $Source: /home/us0400/mdb/op/lib/source/RCS/gethdr.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:33    Sheila Needham  
! $
! Revision 2.0  2001/05/31 14:16:38  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  2000/06/08  15:39:13  15:39:13  usmdb (Generic MetDB account)
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL, OCT. 1999.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

!                                                            PARAMETERS
      INTEGER    NFLAGS,   NITEMS
      PARAMETER (NFLAGS=9, NITEMS=12)
!                                                             VARIABLES
!
      INTEGER IOS                    ! STATUS CODE FROM READ STATEMENT
      INTEGER I1(NITEMS), I2(NITEMS) ! PROCESSING PARAMETERS
      INTEGER J                      ! VARIABLE FOR LOCAL USE
      INTEGER NTYPES                 ! NUMBER OF DATA TYPES
!
      LOGICAL F1(NFLAGS), F2(NFLAGS) ! DATA PROCESSING FLAGS
      LOGICAL WANTED                 ! FLAG FOR WANTED HEADER RANGE
!
      CHARACTER*6 H1(2), H2(2)       ! BULLETIN HEADER RANGES
      CHARACTER*8 DTYP1, DTYP2       ! DATA TYPES
      CHARACTER*8 TYPES(*)           ! LIST OF WANTED DATA TYPES
      CHARACTER*40 FORMAT            ! FORMAT OF HEADER RANGES DATA SET
      CHARACTER*80 RECORD            ! RECORD READ FROM INPUT
      CHARACTER*132 HEAD             ! FOR REVISION INFORMATION
!
!                                                   DATA INITIALISATION
      DATA  H2,   DTYP2,     F2,             I2
     &   /2*' ', 'OTHERS  ', NFLAGS*.FALSE., NITEMS*-1/
      I2(2) = 0  !  (I/O LEVEL)
!                                                  REVISION INFORMATION
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/gethdr.F,v $
     &'//'$Date: 30/01/2006 20:22:33$ $Revision: 1$'
!                                       OPEN INPUT AND OUTPUT DATA SETS
!
      OPEN (11, FILE='GTSHEAD', STATUS='OLD')  ! INPUT
      OPEN (10, FILE='HEADERS', STATUS='NEW')  ! OUTPUT
!
!                                   FIND NUMBER OF DATA TYPES REQUESTED
      J = NTYPES
      DO WHILE (J.GT.0 .AND. TYPES(J).EQ.' ')
         J = J - 1
      END DO
      NTYPES = J
!                         COPY HEADER OF INPUT LIST OF BULLETIN HEADERS
      DO J=1,7
         READ  (11,'(A)') RECORD
         WRITE (10,'(A)') RECORD
         IF (J.EQ.3) FORMAT = RECORD(1:40)
      END DO ! J
!
      DTYP1 = '?'
      DO WHILE (DTYP1.NE.' ')
!                                                        READ NEXT LINE
         READ (11,FORMAT) H1, DTYP1, F1, I1
!                                            CHECK FOR WANTED DATA TYPE
         WANTED = .FALSE.
         DO J=1,NTYPES
            IF (DTYP1.EQ.TYPES(J)) WANTED = .TRUE.
         END DO ! J
!                              UNWANTED RANGE - ADD HEADERS TO 'OTHERS'
!
         IF (.NOT.WANTED .AND. DTYP1.NE.' ') THEN
            H2(2) = H1(2)
            IF (H2(1).EQ.' ') H2(1) = H1(1)
!
!                               OUTPUT RECORD FOR UNWANTED HEADER RANGE
         ELSE
            IF (H2(1).NE.' ') WRITE (10,FORMAT) H2, DTYP2, F2, I2
            H2(1) = ' '
!                                 OUTPUT RECORD FOR WANTED HEADER RANGE
            IF (DTYP1.NE.' ') THEN
               I1(2) = 0  !  (I/O LEVEL)
               WRITE (10,FORMAT) H1, DTYP1, F1, I1
            END IF
         END IF
      END DO
!                            COPY BLANK LINE AND ALL SUBSEQUENT RECORDS
      IOS = 0
      RECORD = ' '
      DO WHILE (IOS.EQ.0)
         WRITE (10,'(A)') RECORD
         READ  (11,'(A)',IOSTAT=IOS) RECORD
      END DO
!                                    CLOSE INPUT AND OUTPUT, AND RETURN
      CLOSE (11)
      CLOSE (10)
      RETURN
      END
