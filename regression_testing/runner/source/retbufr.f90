PROGRAM RETBUFR

!
! TO RETRIEVE UNDECODED BUFR MESSAGES
!

IMPLICIT NONE

INTEGER,PARAMETER  :: IOBS=200
INTEGER,PARAMETER  :: IELS=1
INTEGER,PARAMETER  :: LENREP=28672      !- size of CREP (o/p from MDB)

CHARACTER(8)       :: CSUBT
CHARACTER(500)     :: CREQ
REAL               :: ARRAY (IOBS,IELS)

INTEGER            :: GET1BMSG_ISTAT
INTEGER            :: ISTAT
INTEGER            :: NOBS
INTEGER            :: NELEM
INTEGER            :: I,L
INTEGER            :: LSBUFR
INTEGER            :: NBUFR   ! Number of BUFR messages
INTEGER            :: NMBUFR
INTEGER            :: TOT_MSGS
INTEGER            :: TOT_LEN
CHARACTER(1)       :: CSTR(IOBS)
CHARACTER(LENREP)  :: CREP(IOBS)
CHARACTER(LENREP)  :: SBUFR            !- BUFR message from GET1BMSG


LOGICAL  :: MDBCALLED        ! Flag that MDB has been called
LOGICAL  :: MORE_MSGS
LOGICAL  :: MORE_DATA
CHARACTER(200) :: FILE_IN    ! File containing input
CHARACTER(200) :: FILE_OUT   ! File for printed output
CHARACTER(200) :: RECORD     ! Record read from unit 5
INTEGER :: LN                ! Argument length returned by GET_COMMAND_ARGUMENT
INTEGER :: ISTATA            ! Return status for GET_COMMAND_ARGUMENT
INTEGER :: ISTAT5            ! Return status for I/O

MDBCALLED=.FALSE.

FILE_IN=' '
FILE_OUT=' '
CALL GET_COMMAND_ARGUMENT(1, FILE_IN, LN, ISTATA)
IF (ISTATA /= 0 .OR. FILE_IN == ' ') THEN
  WRITE(6,*) 'INPUT FILE NAME NOT SET'
  GO TO 999
END IF
CALL GET_COMMAND_ARGUMENT(2, FILE_OUT, LN, ISTATA)
IF (ISTATA /= 0 .OR. FILE_OUT == ' ') THEN
  WRITE(6,*) 'OUTPUT FILE NAME NOT SET'
  GO TO 999
END IF
!                                                       Open data output
OPEN (6, FILE=FILE_OUT, IOSTAT=ISTAT5)
IF (ISTAT5 /= 0) THEN
  WRITE(6,*) 'PROBLEM OPENING OUTPUT: "ISTAT5"=', ISTAT5
  GO TO 999
END IF
!                                                        Open data input
OPEN (5, FILE=FILE_IN, IOSTAT=ISTAT5)
IF (ISTAT5 /= 0) THEN
  WRITE(6,*)'PROBLEM OPENING INPUT: "ISTAT5"=', ISTAT5
  GO TO 999
END IF


!--------------------------------------------------------------
! Initialise variables and get request
!--------------------------------------------------------------

NOBS  = IOBS
NELEM = IELS
ISTAT = 0
TOT_MSGS = 0
TOT_LEN=0
L = 1

READ (5,'(A)',IOSTAT=ISTAT5) CSUBT
CREQ = ' '
DO WHILE (ISTAT5.EQ.0)
  READ (5,'(A)',IOSTAT=ISTAT5) RECORD
  IF (ISTAT5.EQ.0) THEN 
    CREQ(L:L+71) = RECORD
    L = L + 72
  ENDIF  
END DO
WRITE(6,*)' DATA TYPE ',CSUBT
WRITE(6,*)TRIM(CREQ)

!-----------------------------------------------------------------
! Keep calling MetDB while ISTAT = 0 or 4
!----------------------------------------------------------------
MORE_DATA=.TRUE.
DO WHILE (MORE_DATA)

  MDBCALLED = .TRUE.
  CALL MDB(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,CSTR,CREP)

  IF (ISTAT == 4 .OR. ISTAT == 0) THEN
    CALL COUNTBMSG(CREP,NOBS,NELEM,ARRAY,NBUFR)
    IF (NBUFR < 1) THEN
      WRITE(6,*)' No BUFR messages found'
      EXIT
    END IF  
  
    NMBUFR         = 0  !- initialise NMBUFR.
    GET1BMSG_ISTAT = 0  !- must be 0 for each new CREP
    MORE_MSGS = .TRUE.
  
    DO WHILE (MORE_MSGS)
!                                            Get one message at a time
      CALL GET1BMSG(CREP,NOBS,NELEM,ARRAY,SBUFR,LSBUFR,GET1BMSG_ISTAT)

      IF (GET1BMSG_ISTAT == 4) THEN  !- A BUFR message found.

        NMBUFR=NMBUFR+1      
        TOT_MSGS=TOT_MSGS+1  
        WRITE(6,*)'BUFR ',TOT_MSGS,' : ', LSBUFR,' bytes'
	TOT_LEN=TOT_LEN+LSBUFR
	
      END IF 
      MORE_MSGS = GET1BMSG_ISTAT == 4
    ENDDO
  
  END IF  
  MORE_DATA=ISTAT == 4
ENDDO

! Error
IF (ISTAT == 8) THEN
  WRITE (6,*)'No data available.'
  
ELSE IF (ISTAT == 16) THEN
  WRITE(6,*)'Error from MDB.'
  
ELSE IF (ISTAT > 16 ) THEN
  WRITE(6,*)'RPC error.'  
  
ENDIF

! Kill the server
IF (MDBCALLED) THEN
  ISTAT = 99
  CALL MDB(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,CSTR,CREP)
END IF

! Write out data length
WRITE(6,*)TOT_MSGS,' OBSERVATIONS RETRIEVED'
WRITE(6,*)TOT_LEN,' bytes transferred'

999  CONTINUE

STOP
END
