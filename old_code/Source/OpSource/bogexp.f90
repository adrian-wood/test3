SUBROUTINE BOGEXP(REPORT,VALUES,bogusType,IDENT_PRESSURE)

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : BOGEXP                                              
!                                                                     
! PURPOSE       : DECODES BOGUS REPORTS                               
!                                                                     
! DATA TYPE(S)  : BOGUS (ALL BOGUS TYPES)                             
!                                                                     
! CALLED BY     : BOGUS (MAIN PROGRAM)                                
!                                                                     
! CALLS         : NONE                                                
!                                                                     
! PARAMETERS    : (1) REPORT FROM DATASET     I                       
!                 (2) DECODED ELEMENTS ARRAY  O                       
!                 (3) BOGUS REPORT TYPE       I                       
!                 (4) PRESSURE for IDENT      O                       
!                
! REVISION INFO :
!
! $Workfile: bogexp.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 19/01/2012 15:35:13$
!                                                     
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         19/01/2012 15:35:13    Stan Kellett
!       Initial port ahead of debugging
!  1    Met_DB_Project 1.0         19/01/2012 15:09:27    Stan Kellett
!       Initial version still being ported from .f version
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------    

IMPLICIT NONE    

!Subroutine arguments
CHARACTER(LEN=80)           :: REPORT          !Character report being read-in
REAL                        :: VALUES(*)       !Decoded elements array *(13)
CHARACTER(LEN=3)            :: bogusType       !Bogus report type
CHARACTER(LEN=4)            :: IDENT_PRESSURE  !Pressure kept to be placed in INDEX

!declare integer
INTEGER                     :: MISSING         !Indicates missing/invalid data
INTEGER                     :: BOGUS_HOUR      !Hour of Bogus report (at start)
INTEGER                     :: BOGUS_HOUR2     !Second Bogus Hour (with minutes)
INTEGER                     :: BOGUS_MINS      !Minutes of Bogus report
INTEGER                     :: TEMP            !Air temperature variable
INTEGER                     :: WIND_DRCTN      !Wind direction variable
INTEGER                     :: WIND_SPEED      !Wind speed variable
INTEGER                     :: HUMIDITY        !Relative humidity variable
INTEGER                     :: PRESSURE        !Pressure variable
INTEGER                     :: I

!declare real
REAL                        :: LAT             !Decoded Latitude variable
REAL                        :: LONG            !Decoded longitude variable
REAL                        :: C2KELVIN        !Conversion from degrees C to Kelvin
REAL                        :: TEMP_KELVIN     !Converted Temperature
REAL                        :: KT2MS           !Conversion from knots to ms-1
REAL                        :: WIND_SPEED_MS   !Converted wind speed

!save values
SAVE

!initalize variables

MISSING = -9999999
PRESSURE = -9999999
TEMP_KELVIN = -9999999.
WIND_DRCTN = -9999999
WIND_SPEED_MS = -9999999.
HUMIDITY = -9999999
C2KELVIN = 273.              ! temperatures in whole degrees  
KT2MS=1852./3600.
IDENT_PRESSURE(:) = ' '                            

DO I=1,13
  VALUES(I)=MISSING
ENDDO

! Get hour from start of record - but replace it by later hour if set

READ (REPORT(1:2),'(I2)') BOGUS_HOUR
IF (bogusType == 'DBC' .OR. bogusType == 'ABC') THEN
  BOGUS_MINS=0
ELSE
  READ (REPORT(8:9),'(I2)') BOGUS_HOUR2
  READ (REPORT(10:11),'(I2)') BOGUS_MINS
  IF (BOGUS_HOUR2 < 0 .OR. BOGUS_HOUR2 > 23) THEN
    BOGUS_MINS=0
  ELSE
    BOGUS_HOUR=BOGUS_HOUR2
  ENDIF
ENDIF

! Lat/Long.

READ (REPORT(20:23),'(F4.1)') LAT
READ (REPORT(26:30),'(F5.1)') LONG

IF (REPORT(24:24) == 'S') LAT = -LAT
IF (REPORT(31:31) == 'W') LONG = -LONG

! Depending on the bogus type we may wish to decode more of the report.

IF (bogusType == 'DBS' .OR. bogusType == 'DBU' .OR.
&    bogusType.EQ.'ABS' .OR. bogusType.EQ.'ABU') THEN

! Pressure (from bytes 40-43 if DBU, 46-49 otherwise; convert mb to Pa)

  IF (bogusType == 'DBU') THEN
    IDENT_PRESSURE=REPORT(40:43)
  ELSE
    IDENT_PRESSURE=REPORT(46:49)
  ENDIF

  IF (IDENT_PRESSURE /= '9999') THEN
    READ (IDENT_PRESSURE,'(I4)') PRESSURE
    PRESSURE=PRESSURE*100
  ENDIF

! Temperature (convert to Kelvin)

  IF (REPORT(52:54) /= '999') THEN
    READ (REPORT(52:54),'(I3)') TEMP
    TEMP_KELVIN=REAL(TEMP)+C2KELVIN
  ENDIF

! Wind direction and speed.

  IF (REPORT(57:62) /= '999999') THEN
    READ (REPORT(57:59),'(I3)') WIND_DRCTN
    READ (REPORT(60:62),'(I3)') WIND_SPEED
    WIND_SPEED_MS=WIND_SPEED*KT2MS
  ENDIF

! Relative humidity.

  IF (REPORT(65:67) /= '999') THEN
    READ (REPORT(65:67),'(I3)') HUMIDITY
  ENDIF

! Put the decoded elements in an array for BUFR encoding
! (Year, month & day in elements 2-4 will be filled in from header)

  VALUES(9)  = REAL(PRESSURE)
  VALUES(10) = REAL(WIND_DRCTN)
  VALUES(11) = WIND_SPEED_MS
  VALUES(12) = TEMP_KELVIN
  VALUES(13) = REAL(HUMIDITY)
ENDIF
VALUES(1) = 1.                 !Character Displacement
VALUES(5) = REAL(BOGUS_HOUR)
VALUES(6) = REAL(BOGUS_MINS)      
VALUES(7) = LAT
VALUES(8) = LONG

RETURN
END SUBROUTINE BOGEXP
