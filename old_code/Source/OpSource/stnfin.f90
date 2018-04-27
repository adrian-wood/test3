SUBROUTINE STNFIN(INDREC2,DETAILS)

!-----------------------------------------------------------------------
!
! PROGRAM       : STNFIN IN STNRT AND STNADMIN
!
! PURPOSE       : TO READ A RECORD FROM THE MAIN STATIONMASTER
!                 DATASET, VIA UNIT 88.
!
! CALLED BY     : STNRET
!
! CALLS         : -
!
! PARAMETERS    : (1) RECORD NUMBER   Integer
!                 (2) Station details (derived-type) stnmas_details
!
! REVISION INFO :
!
!
! $Workfile: stnfin.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 19/06/2011 10:38:53$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         19/06/2011 10:38:53    Sheila Needham  Use a
!       derived type for argument passing
!  2    MetDB_Refresh 1.1         16/11/2010 14:07:18    Rosemary Lavery
!       updated
!  1    MetDB_Refresh 1.0         12/11/2010 14:24:27    Rosemary Lavery
!       initial port
! $
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE IVALUE_MOD
USE STNMAS_MOD   ! derived type STNMAS_DETAILS

IMPLICIT NONE
! Interface Arguments

INTEGER,             INTENT(INOUT)  :: INDREC2
TYPE(STNMAS_DETAILS),INTENT(INOUT)  :: DETAILS

! Local Variables

CHARACTER(LEN=2)   :: COYEAR,COMONTH,CODAY,COHOUR  ! Date of opening
CHARACTER(LEN=2)   :: CCYEAR,CCMONTH,CCDAY,CCHOUR  ! Date of closure
CHARACTER(LEN=264) :: CFMT                         ! Format string
INTEGER            :: K                            ! List counter


DATA CFMT/'(1X,I2.2,I3.3,1X,A4,1X,I2.2,I2.2,A1,1X,I3.3,I2.2,A1,&
           &1X,I6,1X,I6,1X,A42,1X,A32,1X,I2,1X,I4.4,1X,I6,&
           &1X,A2,A2,A2,A2,1X,A2,A2,A2,A2,&
           &1X,A24,1X,I2,1X,I5,1X,I5,1X,I3,1X,I5.5,1X,I5.5,&
           &1X,I5.5,1X,I5.5,1X,A4,1X,A13,1X,A23,1X,I2,&
           &1X,10(A9,1X,A24,1X,I2,1X,I2,1X),I4,1X,A6,1X,I5)'/


SAVE

! SEARCHING STATION MASTER DATASET


READ(88,CFMT,REC=INDREC2) details%BLK,details%STN,details%ICAO,&
        details%LAT1,details%LAT2,details%LATP,details%LONG1,     &
        details%LONG2,details%LONGP,details%HT1,details%HT2,&
        details%STATION,details%COUNTRY,details%REGION,  &
        details%DCNN,details%RAIN,    &
        COYEAR,COMONTH,CODAY,COHOUR,&
        CCYEAR,CCMONTH,CCDAY,CCHOUR, &
        details%FLAGS, &
        details%CLASS,details%HAW00,details%HAW12,details%MAP,   &
        details%RUN1,details%RUN2,details%OREC,details%NREC,         &
        details%UA,details%SYN,details%NCM,details%NUM,          &
        (details%DAY(K),details%SYNTIM(K),details%NCMTIM1(K), &
        details%NCMTIM2(K),K=1,10),         &
        details%SQUARE,details%CHASER,details%RECNO


details%OYEAR=IVALUE(COYEAR)
details%OMONTH=IVALUE(COMONTH)
details%ODAY=IVALUE(CODAY)
details%OHOUR=IVALUE(COHOUR)

details%CYEAR=IVALUE(CCYEAR)
details%CMONTH=IVALUE(CCMONTH)
details%CDAY=IVALUE(CCDAY)
details%CHOUR=IVALUE(CCHOUR)


RETURN
END SUBROUTINE STNFIN
