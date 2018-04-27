      SUBROUTINE STBIND(NOBS,ARRAY,ID,DATETIME,ENTRY)               !2.0

!-----------------------------------------------------------------------
!
! PROGRAM       : STBIND
!
! PURPOSE       : GET TIME & PLACE DESCRIPTORS IN SATOB MESSAGE
!                 & MAKE INDEX ENTRY FROM THEM
!
! CALLED BY     : SATOB1, GOESW1
!
! CALLS         : BOXLALO, INDLALO, LATBOX                          !2.1
!
! PARAMETERS    : (1) NUMBER OF REPORTS IN ARRAY
!               : (2) ARRAY OF DATA VALUES
!               : (3) SATELLITE ID
!               : (4) DATE/TIME OF FIRST REPORT
!               : (5) INDEX ENTRY SO FAR
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:31$
! $Source: /data/us0400/mdb/op/lib/source/RCS/stbind.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:31    Sheila Needham  
! $
! Revision 2.1  2005/03/17  11:41:51  11:41:51  usmdb (MetDB account c/o John C Ward)
! 2.1.  21 March 2005.  Brian Barwell.
! Call BOXLALO to make area information for index entry. 
! Revision information tidied up.
! 
! Revision 2.0  2001/07/03 11:08:37 11:08:37  usmdb (MetDB account c/o J C Ward)
! Removed unused dummy argument CORF. Added copyright amd modified
! header - S.Cox
!
! Revision 1.3  2000/03/10  09:37:05  09:37:05  usmdb (Generic MetDB acc
! 20 March 2000     C Long
! 1.3  Call INDLALO to put lat/long in index entry
!
! Revision 1.2  97/07/31  11:35:27  11:35:27  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 13:34:26  uspm
! Initial revision
!
! 29/11/96     CALL TO LATBOX TO CORRECTLY CALCULATE LAT/LONG BOX
!              WHERE THERE IS MORE THAN ONE REPORT PER INDEX         !B
!
! 29/11/96     ADDITION OF IMPLICIT NONE AND ALL VARIABLES BEING
!              DECLARED.                                             !A
!
! MARCH 94:    MADE FROM AMDIND
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2005 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE                                                   !A

!declare integer
      INTEGER DATETIME(5)   ! Data date & time (yr, mon, day, hr, min)
      INTEGER BEFORLAT      ! Data elements in ARRAY before 'latitude'
      INTEGER BEFORLON      ! Data elements in ARRAY before 'longitude'
      INTEGER BFORYEAR      ! Data elements in ARRAY before 'year'
      INTEGER NLAT          ! Position of latitude in elements of ARRAY
      INTEGER NOBS          ! Number of observations in BUFR message
      INTEGER ITEMP         ! Used for temporary storage

!declare real
      REAL ARRAY(*)         ! Array of data values for BUFR message
      REAL BOX(4)           ! Observation 'box' boundary lat/longs

!declare character
      CHARACTER ENTRY*23    ! Index entry
      CHARACTER ID*3        ! Satellite identifier
      CHARACTER SECTION*1   ! Section number of SATOB message
      CHARACTER HEAD*132    ! Revision information

!declare logical                                                    !2.1
      LOGICAL FIRST         ! Flag  for first call to STBIND        !2.1

!save variables
      SAVE

!initialize variables

      DATA FIRST /.TRUE./                                           !2.1

      IF (FIRST) THEN                                               !2.1
        HEAD = '$RCSfile: $ ' //                                    !2.1
     &         '$Revision: 1$ $Date: 30/01/2006 20:24:31$'                              !2.1
        FIRST = .FALSE.                                             !2.1
      END IF                                                        !2.1

*   THE BOXED 'NON-SATELLITE' INDEX.
*
* _______________________________________________________________
* : COR  : FINE : HOUR :MINUTE : SAT  ID :     :SECTION :NUMBER :
* :      : MESH :   6  :   1   :         :SPARE:NUMBER  :OF OBS :
* : FLAG : FLAG : BITS : BYTE  : 3 BYTES :     :1 BYTE  :2 BYTES:
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* 0                    1       2         5     9       10      12
*
* _______________________________________________________________
* : MIN  : MAX  :MIN   :MAX   :REPORT:TYPE   : TOR : REC :BLOCK :
* : LAT  : LAT  :LONG  :LONG  :FLAGS :FLAGS  :     : NUM :NUMBER:
* :1 BYTE:1 BYTE:1 BYTE:1 BYTE:4 BITS:4 BITS :     :     :      :
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*12     13     14     15     16             17    19    21     23
*
*  THE TYPE FLAGS ARE:
* ________________________________________________
* :TEMPERATURE:WIND:CLOUD COVER:RELATIVE HUMIDITY:
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* 0           1    2           3                 4
*
      SECTION = ENTRY(10:10)
*
* SET UP TIME,POSITION ENTRIES.
*
      IF (SECTION.EQ.'4' .OR. SECTION.EQ.'7') THEN
         BFORYEAR = 1
      ELSE
         BFORYEAR = 2
      END IF
*
      DATETIME(1) = ARRAY((BFORYEAR  )*NOBS + 1) ! YEAR
      DATETIME(2) = ARRAY((BFORYEAR+1)*NOBS + 1) ! MONTH
      DATETIME(3) = ARRAY((BFORYEAR+2)*NOBS + 1) ! DAY
      DATETIME(4) = ARRAY((BFORYEAR+3)*NOBS + 1) ! HOUR
      DATETIME(5) = ARRAY((BFORYEAR+4)*NOBS + 1) ! MINUTE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   INDEX ENTRY SECTIONS HANDLED BY THIS ROUTINE ARE:                !
!                                                                    !
!   MINUTES OF FIRST REPORT                                          !
!   NUMBER OF REPORTS IN SEQUENCE REFERRED TO BY INDEX ENTRY         !
!   LATITUDE AND LONGITUDE BOXES                                     !
!                                                                    !
!   INDEX ENTRY SECTIONS TO BE DONE BY NEXT ROUTINE ARE:             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ENTRY(2:2)=CHAR(DATETIME(5)) ! LOAD IN MINUTES OF REPORT
      ENTRY(3:9) = '       '
      ENTRY(3:5)=ID

      ITEMP=NOBS/256
      ENTRY(11:11)=CHAR(ITEMP)
      ITEMP=MOD(NOBS,256)
      ENTRY(12:12)=CHAR(ITEMP)
*
* LATITUDE / LONGITUDE BOX
* UNITS IN DEGREES IN ARRAY, 1/100TH DEGREES PRECISION.
*
      IF      (SECTION .EQ. '4') THEN
         BEFORLAT = 7
      ELSE IF (SECTION .EQ. '7') THEN
         BEFORLAT = 6
      ELSE
         BEFORLAT = 9
      END IF
      BEFORLON = BEFORLAT+1
*
      IF(NOBS.GT.1)THEN   !  MORE THAN 1 REPORT IN MESSAGE

        NLAT=BEFORLAT+1
        CALL LATBOX(ARRAY,NOBS,NLAT,BOX)
        CALL BOXLALO (BOX, ENTRY(13:16))                            !2.1

      ELSE IF(NOBS.EQ.1)THEN  !   SINGLE REPORT MESSAGE, SINGLE LAT/LONG
*
        CALL INDLALO(ENTRY,ARRAY(BEFORLAT+1),ARRAY(BEFORLON+1))    !1.3
*
      ENDIF
*
* SET TYPE FLAGS ACCORDING TO TABLE BELOW
*
*  ________________________________________________________________
*  SECTION  TEMPERATURE(8)  WIND(4)  CLOUD(2)  REL HUM(1)  FLAG TOT
*     2          Y            Y        N          N           12
*     3          N            Y        N          N            4
*     4          Y            N        N          N            8
*     5          Y            N        Y          N           10
*     7          N            N        N          Y            1
*  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*
      IF      (SECTION .EQ. '2') THEN
         ENTRY(17:17) = CHAR(12)
      ELSE IF (SECTION .EQ. '3') THEN
         ENTRY(17:17) = CHAR( 4)
      ELSE IF (SECTION .EQ. '4') THEN
         ENTRY(17:17) = CHAR( 8)
      ELSE IF (SECTION .EQ. '5') THEN
         ENTRY(17:17) = CHAR(10)
      ELSE
         ENTRY(17:17) = CHAR( 1)
      END IF
*
      RETURN
      END
