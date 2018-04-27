SUBROUTINE bogind (REPORT, VALUES, bogusType, OFT, YMD, TOR, IDENT_PRESSURE)

!-----------------------------------------------------------------------
!
! PROGRAM       : BOGIND
!
! PURPOSE       : To BUFR encode, index & store BOGUS data
!
! DESCRIPTION   : Make BUFR message & 23-byte index entry,
!                 call TAFREP to store.
!
! DATA TYPE(S)  : BOGUS
!
! CALLED BY     : BOGUS
!
! CALLS         : ENBUFR, INDLALO, TAFREP                          !1.7
!
! PARAMETERS    : (1) REPORT  - report text                    (i) !1.7
!                 (2) VALUES  - ELEMENTS ARRAY                 (i) !1.7
!                 (3) TYPE    - BOGUS DATATYPE                 (i) !1.7
!                 (4) OFT     - storage FT number     (for TAFREP) !1.7
!                 (5) YMD     - year, month, day               (i) !1.7
!                 (6) TOR     - time of receipt                (i) !1.7
!                 (7) pressure from report (character*4)       (i) !1.7
!
! REVISION INFO :
!
! $Workfile: bogind.f90$    $Folder: OpSource$
! $Revision: 5$    $Date: 24/01/2012 15:36:47$
!
! CHANGE RECORD :
!
! $Log:
!  5    Met_DB_Project 1.4         24/01/2012 15:36:47    Stan Kellett    Added
!        table version number for enbufr, hardwired to 13
!  4    Met_DB_Project 1.3         19/01/2012 15:15:58    Stan Kellett
!       corrected bigind to bogind
!  3    Met_DB_Project 1.2         19/01/2012 15:10:53    Stan Kellett    added
!        END SUBROUTINE bogind
!  2    Met_DB_Project 1.1         19/01/2012 14:25:46    Stan Kellett
!       version almost migrated
!  1    Met_DB_Project 1.0         19/01/2012 12:08:51    Stan Kellett    f77
!       version saved with .f90 extension
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
USE enbufr_mod
USE indlalo_mod
USE tafrep_mod

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(LEN=*)               :: REPORT         !Observation     
REAL                           :: VALUES(13)     !Decoded elements
CHARACTER(LEN=3)               :: bogusType      !Bogus report type
INTEGER                        :: OFT            !Storage dataset unit number
INTEGER                        :: YMD(3)         !Year Month Day in numeric
INTEGER                        :: TOR(5)         !Time of receipt
CHARACTER(LEN=4)               :: IDENT_PRESSURE !Pressure for identifier

!declare character
CHARACTER(LEN*2000)            :: MESSAGE        !Report Text + BUFR message
CHARACTER(LEN=23)              :: indexEntry     !23-byte index entry
CHARACTER(LEN=9)               :: IDENT          !Identifier for index
CHARACTER(LEN=4)               :: ID             !Identifier to be encoded 

!declare real
REAL                           :: UNIQUE_ID

!declare integer
INTEGER                        :: LAT            !Integer Latitude   
INTEGER                        :: LONG           !Integer Longitude  
INTEGER                        :: BLKSIZ         !Blksize of dataset
INTEGER                        :: IHOUR          !Hour of bogus reports
INTEGER                        :: L              !Length of BUFR message
INTEGER                        :: I              !Used as loop counter
INTEGER                        :: DATIME(5)      !Date/Time array for storage
INTEGER                        :: DESCR(30)            
INTEGER                        :: NDESCR
INTEGER                        :: NVALUES
INTEGER                        :: NOBS
INTEGER                        :: IDES
INTEGER                        :: IPRESSURE
INTEGER                        :: iVer=13        ! Bufr Table version number

!save values
SAVE

!initialize variables

BLKSIZ = 27998

! Character report appears before the BUFR message so insert report.
! Put year, month, day in values array.

MESSAGE(1:) = REPORT
VALUES(2)   = YMD(1)
VALUES(3)   = YMD(2)
VALUES(4)   = YMD(3)

! Set the local sequence descriptor and other BUFR variables.

DESCR(1) = IDES(302196)
NDESCR = 1
NVALUES = 13
NOBS = 1
ID = bogusType

! Call ENBUFR to encode decoded values array into BUFR message

CALL ENBUFR(DESCR, VALUES, NDESCR, NVALUES, NOBS, ID, &
&            TOR, MESSAGE(81:), .FALSE., L , iVer)

! Don't store report if lat or long is missing. 

LatLongCheck: IF (VALUES(7) /= -9999999. .AND. VALUES(8) /= -9999999.) THEN  
  LAT=VALUES(7)*100      !integer latitude in hundredths   
  LONG=VALUES(8)*100     !integer longitude in hundredths  

! Start index entry, leaving times & block/record for TAFREP to fill in.

  IHOUR = VALUES(5)
  ENTRY(3:6) = '    '
  ENTRY(7:7) = CHAR(0)           ! Cor number
  ENTRY(8:11) = IDENT_PRESSURE
  ENTRY(12:12) = CHAR(NVALUES)
  CALL INDLALO(indexEntry, VALUES(7), VALUES(8))   

! As there are only a few types, and no other suitable identifier,
! add a number depending on the coordinates so that boguses of the
! same type are not treated as obs for the same station and chained.
! (Pressure may be missing, hence the check for zero).

  IDENT(1:5) = TYPE
  READ (IDENT_PRESSURE,'(I4)') IPRESSURE
  IF (IPRESSURE == 0) IPRESSURE = 1
  UNIQUE_ID = IPRESSURE*LAT*LONG
    WRITE (IDENT(6:9),'(A4)') UNIQUE_ID

! Put time of data in integer array for TAFREP & store message

  DO I = 1, 5                               
    DATIME(I) = VALUES(1+I)     ! INTEGER DATE/TIME   
  ENDDO                                              
  CALL TAFREP(DATIME, indexEntry, MESSAGE(:80+L), OFT, BLKSIZ, IDENT)
ENDIF LatLongCheck

RETURN
END SUBROUTINE bogind
