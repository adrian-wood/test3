SUBROUTINE STMRET(WMOLST,ICAOLST,DCNNLST,RAINLST,IDTYPE,USERPOS, &
                  STNTYPE,ISTAT,RARRAY,NOBS,NELEM,ARRAY1,CSTR,   &
                  CERROR)

!-----------------------------------------------------------------------
!
! PROGRAM       : STMRET
!
! PURPOSE       : PROGRAM TO RETRIEVE STATION MASTER DETAILS THROUGH MDB
!
! DESCRIPTION   : THIS PROGRAM SETS UP A CALL TO STNRET WHICH
!                 LOCATES RECORD IN THE STATION MASTER DATASET.
!                 THE REQUIRED DETAILS ARE THEN OUTPUT
!
! CALLED BY     : MDB
!
! CALLS         : STNRET
!               : STNARY
!
! PARAMETERS    :
!
! REVISION INFO :
!
!
! $Workfile: stmret.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 09/02/2011 17:17:04$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         09/02/2011 17:17:04    Sheila Needham  Use
!       CSTR(:)
!  4    MetDB_Refresh 1.3         22/11/2010 13:10:29    Stan Kellett    CSTR
!       corrected to CSTR(*)
!  3    MetDB_Refresh 1.2         16/11/2010 14:07:18    Rosemary Lavery
!       updated
!  2    MetDB_Refresh 1.1         12/11/2010 17:10:31    Rosemary Lavery remove
!        old revision info
!  1    MetDB_Refresh 1.0         04/11/2010 13:28:15    Rosemary Lavery
!       Initial port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE STNRET_MOD
USE STNARY_MOD

IMPLICIT NONE

! Interface Arguments

INTEGER,            INTENT(INOUT)  :: WMOLST(50)
CHARACTER (LEN=4),  INTENT(INOUT)  :: ICAOLST(50)
INTEGER,            INTENT(INOUT)  :: DCNNLST(50)
INTEGER,            INTENT(INOUT)  :: RAINLST(50)
INTEGER,            INTENT(INOUT)  :: IDTYPE
CHARACTER (LEN=1),  INTENT(INOUT)  :: USERPOS
INTEGER,            INTENT(INOUT)  :: STNTYPE
INTEGER,            INTENT(INOUT)  :: ISTAT
INTEGER,            INTENT(INOUT)  :: NOBS
INTEGER,            INTENT(INOUT)  :: NELEM
REAL,               INTENT(INOUT)  :: RARRAY(NOBS,NELEM)
INTEGER,            INTENT(INOUT)  :: ARRAY1(:)
CHARACTER (LEN=*),  INTENT(INOUT)  :: CSTR(:)
CHARACTER (LEN=48), INTENT(INOUT)  :: CERROR

! Local Parameters

CHARACTER (LEN=4)     :: ICAO
CHARACTER (LEN=4)     :: UA
CHARACTER (LEN=6)     :: CHASER
CHARACTER (LEN=24)    :: FLAGS
CHARACTER (LEN=24)    :: SYNTIM(10)
CHARACTER (LEN=13)    :: SYN
CHARACTER (LEN=23)    :: NCM
CHARACTER (LEN=9)     :: DAY(10)
CHARACTER (LEN=42)    :: STATION
CHARACTER (LEN=32)    :: COUNTRY
CHARACTER (LEN=1)     :: LATP
CHARACTER (LEN=1)     :: LONGP
CHARACTER (LEN=1)     :: POS

INTEGER               :: NOBCON
INTEGER               :: RECNO
INTEGER               :: BLK
INTEGER               :: STN
INTEGER               :: DCNN
INTEGER               :: RAIN
INTEGER               :: REGION
INTEGER               :: LAT1
INTEGER               :: LAT2
INTEGER               :: LONG1
INTEGER               :: LONG2
INTEGER               :: HT1
INTEGER               :: HT2
INTEGER               :: CLASS
INTEGER               :: HAW00
INTEGER               :: HAW12
INTEGER               :: OYEAR
INTEGER               :: OMONTH
INTEGER               :: ODAY
INTEGER               :: OHOUR
INTEGER               :: CYEAR
INTEGER               :: CMONTH
INTEGER               :: CDAY
INTEGER               :: CHOUR
INTEGER               :: MAP
INTEGER               :: RUN1
INTEGER               :: RUN2
INTEGER               :: OREC
INTEGER               :: NREC
INTEGER               :: NUM
INTEGER               :: NCMTIM1(10)
INTEGER               :: NCMTIM2(10)
INTEGER               :: SQUARE
INTEGER               :: ILOOP1
INTEGER               :: ILOOP2
LOGICAL               :: STNOK


! CHECK THE STATUS OF ISTAT BEFORE STARTING/CONTINUING THE RETRIEVAL

IF(ISTAT == 0)THEN
  NOBCON=1
ELSE IF(ISTAT > 4)THEN
  CERROR='STATUS INCORRECT FOR RETRIEVAL'
  GOTO 999
END IF

IF((ISTAT == 4).AND.(NOBCON > NOBS))THEN
   NOBCON=1
ELSE IF((ISTAT /= 4).AND.(NOBCON > NOBS))THEN
   CERROR='PROBLEM WITH THE RETRIEVAL'
   GOTO 999
END IF

DO ILOOP1=1,NOBS
  DO ILOOP2=1,NELEM
    RARRAY(ILOOP1,ILOOP2)=-9999999.0
  END DO
END DO

POS = 'A'

! CALL STNRET (TO LOCATE AND RETRIEVE CORRECT RECORD)

 100  CONTINUE
  CALL STNRET(WMOLST,ICAOLST,DCNNLST,RAINLST,                       &
              IDTYPE,POS,ISTAT,                                     &
              BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1,                    &
              LONG2,LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN, &
              OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,      &
              FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,      &
              UA,SYN,NCM,NUM,                                       &
              DAY,SYNTIM,NCMTIM1,NCMTIM2,SQUARE,CHASER,RECNO)

IF(ISTAT == 8)THEN
  CERROR='DATA REQUESTED NOT AVAILABLE'
  NOBS=0
  GOTO 999
ELSE
  STNOK = .TRUE.
  IF (USERPOS == 'L' .AND. FLAGS(9:9)   == '2' &
                     .AND. NREC         >  0 ) STNOK = .FALSE.
  IF (STNTYPE ==  1  .AND. FLAGS(15:15) == '2') STNOK = .FALSE.
  IF (STNTYPE ==  2  .AND. FLAGS(15:15) < '2') STNOK = .FALSE.
END IF

! PUT DATA INTO USERS ARRAY
IF (STNOK) &
  CALL STNARY(BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1,                    &
              LONG2,LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN, &
              OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,      &
              FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,      &
              UA,SYN,NCM,                                           &
              DAY,SYNTIM,NCMTIM1,NCMTIM2,SQUARE,CHASER,             &
              RARRAY,NOBS,NELEM,ARRAY1,CSTR,NOBCON)

  IF((NOBCON > NOBS).AND.(ISTAT == 4))THEN
    ISTAT=4
  ELSE IF((NOBCON <= NOBS).AND.(ISTAT == 4))THEN
    GOTO 100
  ELSE
    ISTAT=0
    NOBS=NOBCON-1
  END IF

999  RETURN
END SUBROUTINE STMRET
