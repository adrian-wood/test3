!-----------------------------------------------------------------------
! PROGRAM     : wfsyn
!
! PURPOSE     : Retrieve latest LNDSYN for Heathrow and encode as BUFR
!
! DESCRIPTION : Calls MetDB for latest Heathrow LNDSYN;
!               encodes BUFR;
!               writes encoded BUFR message to file.
!
! ARGUMENTS   : none
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2016 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

PROGRAM wfsyn
IMPLICIT NONE

INTEGER,PARAMETER   :: IOBS = 2
INTEGER,PARAMETER   :: IELS = 15

CHARACTER (LEN=8)   :: CSUBT
CHARACTER (LEN=500) :: CREQ,CELEMS
CHARACTER (LEN=25)  :: CELEM
CHARACTER (LEN=1)   :: LETTER

REAL                :: ARRAY(IOBS,IELS)

INTEGER             :: NOBS
INTEGER             :: NELEM
INTEGER             :: ISTAT,TOT
INTEGER             :: I,J,CELEMCT

LOGICAL             :: RPCERROR

CHARACTER (LEN=1)   :: CSTR(IOBS)
CHARACTER (LEN=1)   :: CREP(IOBS)

!-----------------------------------------------------------------------
! Variables for BUFR encoding
!-----------------------------------------------------------------------
INTEGER,PARAMETER :: MAXDESC=50     !- max no. of expanded descriptors.
INTEGER,PARAMETER ::   MAXOBS=1     !- max no. of obs.
INTEGER,PARAMETER ::   RECLEN=25000 !- BUFR message size (record len)

REAL ::   VALUES(MAXOBS*MAXDESC)    !- BUFR values array.
INTEGER  ::  DESCR(MAXDESC)         !- BUFR descriptors array.
INTEGER  ::  DATIME(6)              !- date/time array.
INTEGER  ::  NOBSB                  !- no. of obs.
INTEGER  ::  NELEMB                 !- no. of elements.
INTEGER  ::  NDESC                  !- no. of descriptors.
INTEGER  ::  L                      !- length of BUFR message.
INTEGER  ::  RC                     !- return code from METDB_COPEN
INTEGER  ::  NBYTES                 !- amount of data written to file
INTEGER  ::  IDES                   !- external IDES function.
INTEGER  ::  Edition                !- BUFR edition number
INTEGER  ::  MasterTable            !- BUFR Master table no.
INTEGER  ::  OrigCentre             !- Originating Centre
INTEGER  ::  OrigSubCentre          !- Originating Centre
INTEGER  ::  DataType               !- data category type
INTEGER  ::  LocalDataSubtype       !- data category subtype
INTEGER  ::  IntDataSubtype         !- data category subtype
INTEGER  ::  VerMasTab              !- Version no. of master tables
INTEGER  ::  VerLocTab              !- Version no. of local tables
INTEGER  ::  Sect3Type              !- BUFR section 3 data type
LOGICAL  ::  CMP                    !- BUFR compression flag.
LOGICAL  ::  ExtraSect1             !- F for no extra BUFR s.1 data
LOGICAL  ::  ExtraSect2             !- FALSE for no BUFR section 2

CHARACTER(LEN=RECLEN) ::  MESAGE       !- final BUFR message.
CHARACTER(LEN=100) ::      NAMES       !- BUFR names string.
CHARACTER(LEN=4) ::         CharSect1  !- extra BUFR sect 1 data
CHARACTER(LEN=1) ::         CharSect2  !- BUFR sect 2 data

!-----------------------------------------------------------------------
! RPC Retrieval Section
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Initialise Variables
!-----------------------------------------------------------------------
CSUBT = 'LNDSYN '

CELEMS = 'LTTD LNGD STTN_HGHT &
        YEAR MNTH DAY HOUR MINT &
        PESR_SNSR_HGHT STTN_PESR &
        SRFC_RLTV_HUMDY SRFC_AIR_TMPR &
        SRFC_WIND_DRCTN SRFC_WIND_SPED &
        MXMM_MEAN_SRFC_WIND_SPED '

CREQ = 'LATEST &
        PLATFORM 03772 &
        ELEMENTS ' // CELEMS

NOBS  = IOBS
NELEM = IELS

ISTAT = 0
TOT   = 0
RPCERROR = .FALSE.

!-----------------------------------------------------------------------
! Keep calling MetDB while ISTAT = 0 or 4
!-----------------------------------------------------------------------
DO WHILE (ISTAT <= 4)

  WRITE(6,*)'wfsyn: About to call MetDB'
  CALL MDB(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,CSTR,CREP)
  WRITE(6,*)'wfsyn: Back from MDB - ISTAT =',ISTAT

  IF (ISTAT <= 4) THEN

   DO I=1,NOBS
     TOT=TOT+1
   ENDDO

  ENDIF
  IF (ISTAT == 0) EXIT
  IF (ISTAT > 4) THEN
      WRITE(0,*)'wfsyn: Error in MetDB, ISTAT = ',ISTAT
      RPCERROR=.TRUE.
      EXIT
   ENDIF  
ENDDO

WRITE(6,'(/1X,''NOBS  = '',I5)')TOT
WRITE(6,'( 1X,''ISTAT = '',I5)')ISTAT

!-----------------------------------------------------------------------
! ALWAYS KILL THE SERVER WHEN FINISHED !!!!!
!-----------------------------------------------------------------------

WRITE(6,*)'wfsyn: About to Kill Server'
ISTAT=99
CALL MDB(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,CSTR,CREP)

!-----------------------------------------------------------------------
! If we have had errors in RPC call, exit before creating BUFR file
!-----------------------------------------------------------------------
IF (RPCERROR) THEN
  WRITE(6,*)'wfsyn: Errors from RPC call; exiting'
  STOP 9
ENDIF
  
!-----------------------------------------------------------------------
! Print out the element values retrieved in RPC call
!-----------------------------------------------------------------------
CELEM=''   
CELEMCT=1
DO I=1,(LEN_TRIM(CELEMS)+1)
  LETTER=CELEMS(I:I)
  IF (LETTER == ' ') THEN
    WRITE(6,'(3A,I2,A,F15.6)')'wfsyn: Value in retrieved data for ',CELEM,'(element ',CELEMCT,'): ',ARRAY(1,CELEMCT)
    CELEM=''
    CELEMCT=CELEMCT+1
  ELSE
    CELEM = TRIM(CELEM) // LETTER
  ENDIF
ENDDO

!-----------------------------------------------------------------------
! BUFR Encoding Section
!-----------------------------------------------------------------------
! initialise variables.
!-----------------------------------------------------------------------

NDESC=1               !- code 1 descriptors per ob.
NELEMB=24             !- code 24 elements per ob.
NOBSB=1               !- code 1 observations.
L=0                   !- eventual output from enbufr, octets.
CMP=.TRUE.            !- perform BUFR compression.
MESAGE(:)=' '
NAMES(:)=' '

DO I=1,MAXDESC
  DESCR(I)=0
ENDDO

DO I=1,MAXDESC*MAXOBS
  VALUES(I)=0.0
ENDDO

!-----------------------------------------------------------------------
! code descriptors (1 per ob) - describes 24 elements per ob.
!-----------------------------------------------------------------------

DESCR(1)=IDES(303221)

!-----------------------------------------------------------------------
! code values array.
!-----------------------------------------------------------------------

NAMES(1:32)="heathrow"    !- Site Name
NAMES(33:40)="3772"       !- SSPA ID
NAMES(41:48)="surface1"   !- Observation ID

VALUES(1)=1              !- Position in Names array for Site Name
VALUES(2)=33             !- Position in Names array for SSPA Identity
VALUES(3)=ARRAY(1,1)     !- latitude (1st element back from RPC call)
VALUES(4)=ARRAY(1,2)     !- longitude (2nd element)
VALUES(5)=ARRAY(1,3)     !- station height (3rd element)
VALUES(6)=ARRAY(1,4)     !- year (4th element)
VALUES(7)=ARRAY(1,5)     !- month (5th element)
VALUES(8)=ARRAY(1,6)     !- day (6th element)
VALUES(9)=ARRAY(1,7)     !- hour (7th element)
VALUES(10)=ARRAY(1,8)    !- minute (8th element)
VALUES(11)=00            !- second (not in LNDSYN)
VALUES(12)=1             !- Replication Count
VALUES(13)=41            !- Position in Names array for Observation ID
VALUES(14)=ARRAY(1,9)    !- Station Barometer Height (9th element)
VALUES(15)=ARRAY(1,10)   !- Station Pressure (10th element)
VALUES(16)=-9999999.0    !- Station Humidity Sensor Height
VALUES(17)=ARRAY(1,11)   !- Relative Humidity (11th element)
VALUES(18)=-9999999.0    !- Station Temperature Sensor Height
VALUES(19)=ARRAY(1,12)   !- Air Temperature (12th element)
VALUES(20)=-9999999.0    !- Station Wind Sensor Height
VALUES(21)=ARRAY(1,13)   !- Wind Direction (13th element)
VALUES(22)=ARRAY(1,14)   !- Wind Speed (14th element)
VALUES(23)=-9999999.0    !- Wind Speed Minimum
VALUES(24)=ARRAY(1,15)   !- Wind Speed Maximum (15th element)

!-----------------------------------------------------------------------
! call enbufv4 to code the BUFR message
!-----------------------------------------------------------------------

DATIME(1)    = ARRAY(1,4) !- current year
DATIME(2)    = ARRAY(1,5) !- current month
DATIME(3)    = ARRAY(1,6) !- current day
DATIME(4)    = ARRAY(1,7) !- current hour
DATIME(5)    = ARRAY(1,8) !- current minute
DATIME(6)    = 00         !- current second

Edition      = 4          !-
MasterTable  = 0          !- use default = 0
OrigCentre   = 74         !- use default (UK Met Office)
OrigSubCentre   = 0       !- use default = 0
DataType     = 0          !- use default = 255
LocalDataSubType  = 25    !- use default = 255
IntDataSubType  = 0       !- use default = 255
VerMasTab    = 13         !- use default = 13
VerLocTab    = 0          !- use default = 0
Sect3Type    = 1          !- use default = 1
ExtraSect1   = .False.    !- no extra BUFR section 1 data
ExtraSect2   = .FALSE.    !- no BUFR section 2 data
CharSect1    = ''         !- dummy extra BUFR section 1 data
CharSect2    = ''         !- dummy BUFR section 2 data

!print*,'BUFRencode Edition 4.'
!print*,'Descr ',DESCR
!print*,'Values ',VALUES
!print*,'Datime ',DATIME

CALL ENBUFV4(DESCR,VALUES,NDESC,NELEMB,NOBSB,NAMES,DATIME,    &
                  MESAGE,CMP,L,Edition,MasterTable, &
                  VerMasTab,OrigCentre,    &
                  OrigSubCentre, &
                  DataType,LocalDataSubType, &
                  IntDataSubType,VerLocTab,        &
                  ExtraSect1,CharSect1,ExtraSect2,CharSect2,Sect3Type)

!-----------------------------------------------------------------------
! Use C I/O to write the BUFR message to file 
!-----------------------------------------------------------------------

! Open file.  arg1 = open on unit 20.
!             arg2 = filename
!             arg3 = filemode = 2 = open for read/write new
!             arg4 = RC = return code. 0 = OK, >0 = failed.

CALL METDB_COPEN(20,'WFSYN.bufr',2,RC)

IF (RC /= 0) THEN
  WRITE(6,*)'wfsyn: ERROR: File Open failed. RC = ',RC
ELSE

! Write file. arg1 = write to unit 20.
!             arg2 = BUFR message to write
!             arg3 = Amount of data written (return value)

  CALL METDB_CWRITE(20,MESAGE(1:L),NBYTES)
  WRITE(6,*)'wfsyn: INFO: Bytes written to file = ',NBYTES

! Close file. arg1 = close unit 20.

  CALL METDB_CCLOSE(20)
ENDIF

STOP
END
