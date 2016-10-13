PROGRAM GETGRIB
!-----------------------------------------------------------------------
!
! PROGRAM       : GETGRIB
!
! PURPOSE       : Generic program to retrieve GRIB data from MetDB and 
!                 write summary information to a file
! 
! USAGE         : getgrib.exe output_file < input_file
!
! INPUT         : stdin should consist of two lines, repeated for each type
!   line 1 : data subtype name (A8)
!   line 2 : request string (A80)
!
! 
! REVISION INFO :
!
! $Workfile: airind.f90$ $Folder: F95_source$
! $Revision: 1$ $Date: 30/11/2012 10:33:50$
!
! CHANGE RECORD :
!
! $Log:
!  1    MOODS      1.0         30/11/2012 10:33:50    Paul Barnham    Initial
!       version copied from MetDB project.
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
      
INTEGER,PARAMETER :: LCSTR=9216               
INTEGER,PARAMETER :: NSIZE=400                             
REAL              :: RARRAY(NSIZE)        ! 1D array for GRIB                                  
CHARACTER(1)      :: CSTR(NSIZE)                                          
CHARACTER(LCSTR)  :: CREP(NSIZE)          ! for GRIB data 
CHARACTER(200)    :: FILEOUT              ! output filename                                    

CHARACTER(130)     :: WANTED               ! request string
CHARACTER(8)      :: DATYPE                                    
CHARACTER(4)      :: GRIB
CHARACTER(4)      :: SEVENS                                          
LOGICAL           :: STARTED
INTEGER           :: I,J
INTEGER           :: IG                   ! index of GRIB in string
INTEGER           :: I7                   ! index of 7777 in string
INTEGER           :: COUNT                ! no. of GRIB messages
INTEGER           :: TOT                  ! no. of bytes per type 
INTEGER           :: LN                   ! length of cmd argument
INTEGER           :: ISTATA               ! I/O status 
!                                                                      
GRIB   = CHAR(71) // CHAR(82) // CHAR(73) // CHAR(66) ! 'GRIB'   
SEVENS = CHAR(55) // CHAR(55) // CHAR(55) // CHAR(55) ! '7777'   

! Get output file name                    

CALL GET_COMMAND_ARGUMENT(1, FILEOUT, LN, ISTATA)
IF (ISTATA /= 0 .OR. FILEOUT == ' ') THEN
  PRINT *, 'OUTPUT FILE NAME NOT SET'
  STOP 8
END IF
OPEN (6, FILE=FILEOUT, IOSTAT=ISTATA)
IF (ISTATA /= 0) THEN
  PRINT *, 'PROBLEM OPENING OUTPUT: "ISTAT"=', ISTATA
  STOP 8 
END IF
 
! Loop over input requests

MOREREQ: &
DO WHILE (.TRUE.)
  READ (5,'(A8/A)',END=999) DATYPE, WANTED
  IF (DATYPE(1:1) == ' ') EXIT MOREREQ
  WRITE(6,*) ' DATA TYPE ',DATYPE
  WRITE(6,*) WANTED 

! Initialise 

  ISTAT=0
  ITEMS=1
  COUNT=0
  TOT=0
  STARTED=.FALSE.
! Loop over calls to MDB getting data

MOREDATA:&
  DO WHILE (ISTAT .LE. 4)
    NOBS=NSIZE
 
    CALL MDB (DATYPE, WANTED, RARRAY, NOBS,      &                   
                          ITEMS, ISTAT, CSTR, CREP)  

    IF (ISTAT == 16) THEN                                         
      WRITE (6,*) 'ERROR FROM METDB '                             
      WRITE(6,'(/I12,A)')0,' OBSERVATIONS RETRIEVED' 
      WRITE(6,'(/A,I12/)')'Total Bytes transferred ',0                                                        
      CYCLE MOREREQ
    ELSE IF (ISTAT == 8) THEN                                     
      WRITE(6,*) 'NO MDB DATA AVAILABLE FOR REQUESTED DATE'       
      WRITE(6,'(/I12,A)')0,' OBSERVATIONS RETRIEVED'
      WRITE(6,'(/A,I12/)')'Total Bytes transferred ',0                                                        
      CYCLE MOREREQ
    ELSE                                  
                            
! Look for start and end of GRIB messages
                                         
      DO J=1,NOBS                                                 
        I = RARRAY(J)      ! Length of data 
        TOT=TOT+I                                             
!                                     LOOK FOR GRIB AND 7777
        IF (I .GT. 0) THEN
          I7=INDEX(CREP(J)(1:I),SEVENS)
          IF(I7 > 0)WRITE(6,*)'7777 FOUND IN RECORD ',J,' OFFSET ',I7

          IG=INDEX(CREP(J)(1:I),GRIB)
	  IF(IG > 0)THEN
            WRITE(6,'(A,I5,A,i6)')'GRIB FOUND IN RECORD ',J,' OFFSET ',IG
            COUNT=COUNT+1
 	  END IF  
 
        ELSE
          WRITE(6,*)'NO MORE DATA '
          EXIT MOREDATA
        END IF
      END DO
      IF (ISTAT.EQ.0) THEN
        WRITE(6,*)'NO MORE DATA '
        EXIT MOREDATA
      END IF
!                                                       ANY MORE DATA ? 
    END IF                                                        
  END DO MOREDATA 

  WRITE(6,'(/I12,A)')COUNT,' OBSERVATIONS RETRIEVED' 
  WRITE(6,'(/A,I12/)')'Total Bytes transferred ',tot                                                      
                              
END DO MOREREQ
                                                                    
! Kill the RPC server

999 CONTINUE
ISTAT=99
CALL MDB (DATYPE, WANTED, RARRAY, NOBS,      &                   
                          ITEMS, ISTAT, CSTR, CREP)  
STOP                                                             
END PROGRAM GETGRIB                                        
