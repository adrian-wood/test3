      SUBROUTINE BOGEXP(REPORT,VALUES,TYPE,IDENT_PRESSURE)

      IMPLICIT NONE

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
! $Revision: 1$
! $Date: 30/01/2006 20:21:01$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bogexp.F,v $
!                                                     
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:01    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:30  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.7  2000/04/07  09:12:17  09:12:17  usmdb (Generic MetDB account)
! 17 April 2000     C Long
! 1.7  Drastic tidy up to remove redundant & misleading code.
!      BOGRPT renamed BOGEXP. 
! 
! Revision 1.6  99/09/09  09:55:35  09:55:35  usmdb (Generic MDB account)
! 20 Sept 99  C Long
! 1.6  Change C/K conversion from 273.1 to 273 (whole degrees)
!
! Revision 1.5  98/04/20  06:49:56  06:49:56  usmdb (Generic MDB account)
! Keep pressure as I4 format for use in the IDENT index entry field.  !d
!
! Revision 1.4  98/03/03  16:05:01  16:05:01  usjl (Jon Lewthwaite)
! Correct decode of Temps.                                            !c
!
! Revision 1.3  1997/09/25 14:01:53  usjl
! Improve  and correct various bugs with decode                       !b
!
! Revision 1.2  1997/07/31 09:12:04  uspm
! First revision for  1
!
! Revision 1.1  1997/07/04 10:59:23  uspm
! Initial revision
!
! 14/04/97 prupose - Addition of minutes to bogus reports now
! requires decoding and storage                                       !a
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

!declare character
      CHARACTER REPORT*80        !Character report being read-in
      CHARACTER TYPE*3           !Bogus report type
      CHARACTER HEAD*132         !Version header
      CHARACTER IDENT_PRESSURE*4 !Pressure kept to be placed in INDEX

!declare integer
      INTEGER MISSING            !Indicates missing/invalid data
      INTEGER BOGUS_HOUR         !Hour of Bogus report (at start)
      INTEGER BOGUS_HOUR2        !Second Bogus Hour (with minutes)
      INTEGER BOGUS_MINS         !Minutes of Bogus report
      INTEGER TEMP               !Air temperature variable
      INTEGER WIND_DRCTN         !Wind direction variable
      INTEGER WIND_SPEED         !Wind speed variable
      INTEGER HUMIDITY           !Relative humidity variable
      INTEGER PRESSURE           !Pressure variable
      INTEGER I

!declare real
      REAL LAT                   !Decoded Latitude variable
      REAL LONG                  !Decoded longitude variable
      REAL VALUES(*)             !Decoded elements array *(13)
      REAL C2KELVIN              !Conversion from degrees C to Kelvin
      REAL TEMP_KELVIN           !Converted Temperature
      REAL KT2MS                 !Conversion from knots to ms-1
      REAL WIND_SPEED_MS         !Converted wind speed

!save values
      SAVE

!initalize variables
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/bogexp.F,v $
     &'//'$ $Date: 30/01/2006 20:21:01$ $Revision: 1$'

      MISSING=-9999999
      PRESSURE=-9999999
      TEMP_KELVIN=-9999999.
      WIND_DRCTN=-9999999
      WIND_SPEED_MS=-9999999.
      HUMIDITY=-9999999
      C2KELVIN=273.              ! temperatures in whole degrees   !1.6
      KT2MS=1852./3600.
      IDENT_PRESSURE(:)=' '                                          !d

      DO I=1,13
        VALUES(I)=MISSING
      ENDDO

! Get hour from start of record - but replace it by later hour if set

      READ (REPORT(1:2),'(I2)') BOGUS_HOUR
      IF (TYPE.EQ.'DBC' .OR. TYPE.EQ.'ABC') THEN
        BOGUS_MINS=0
      ELSE
        READ (REPORT(8:9),'(I2)') BOGUS_HOUR2
        READ (REPORT(10:11),'(I2)') BOGUS_MINS
        IF (BOGUS_HOUR2.LT.0 .OR. BOGUS_HOUR2.GT.23) THEN
          BOGUS_MINS=0
        ELSE
          BOGUS_HOUR=BOGUS_HOUR2
        ENDIF
      ENDIF

! Lat/Long.

      READ (REPORT(20:23),'(F4.1)') LAT
      READ (REPORT(26:30),'(F5.1)') LONG

      IF (REPORT(24:24).EQ.'S') LAT=-LAT
      IF (REPORT(31:31).EQ.'W') LONG=-LONG

! Depending on the bogus type we may wish to decode more of the report.

      IF (TYPE.EQ.'DBS' .OR. TYPE.EQ.'DBU' .OR.
     &    TYPE.EQ.'ABS' .OR. TYPE.EQ.'ABU') THEN

! Pressure (from bytes 40-43 if DBU, 46-49 otherwise; convert mb to Pa)

        IF (TYPE.EQ.'DBU') THEN
          IDENT_PRESSURE=REPORT(40:43)
        ELSE
          IDENT_PRESSURE=REPORT(46:49)
        ENDIF

        IF (IDENT_PRESSURE.NE.'9999') THEN
          READ (IDENT_PRESSURE,'(I4)') PRESSURE
          PRESSURE=PRESSURE*100
        ENDIF

! Temperature (convert to Kelvin)

        IF (REPORT(52:54).NE.'999') THEN
          READ (REPORT(52:54),'(I3)') TEMP
          TEMP_KELVIN=REAL(TEMP)+C2KELVIN
        ENDIF

! Wind direction and speed.

        IF (REPORT(57:62).NE.'999999') THEN
          READ (REPORT(57:59),'(I3)') WIND_DRCTN
          READ (REPORT(60:62),'(I3)') WIND_SPEED
          WIND_SPEED_MS=WIND_SPEED*KT2MS
        ENDIF

! Relative humidity.

        IF (REPORT(65:67).NE.'999') THEN
          READ (REPORT(65:67),'(I3)') HUMIDITY
        ENDIF

! Put the decoded elements in an array for BUFR encoding
! (Year, month & day in elements 2-4 will be filled in from header)

        VALUES(9)=REAL(PRESSURE)
        VALUES(10)=REAL(WIND_DRCTN)
        VALUES(11)=WIND_SPEED_MS
        VALUES(12)=TEMP_KELVIN
        VALUES(13)=REAL(HUMIDITY)
      ENDIF
      VALUES(1)=1.                 !Character Displacement
      VALUES(5)=REAL(BOGUS_HOUR)
      VALUES(6)=REAL(BOGUS_MINS)                                    !A
      VALUES(7)=LAT
      VALUES(8)=LONG
      RETURN
      END
