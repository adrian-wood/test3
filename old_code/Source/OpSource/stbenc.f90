SUBROUTINE STBENC(DCVALS,NELEM,NOBS,DESCR,MESSAGE,MESSLEN,DATETIME)

!-----------------------------------------------------------------------
!
! PROGRAM       : STBENC
!
! PURPOSE       : INITIALISES VALUES BEFORE A CALL TO BUFR ENCODING
!                 SOFTWARE
!
! CALLED BY     : SATOB1
!
! CALLS         : ENBUFR
!
! ARGUMENTS     : (1) DCVALS - DECODED VALUES OCCUPYING NOBS*NELEM
!                              LENGTH IN REAL ARRAY
!                 (2) NELEM - NUMBER OF ELEMENTS
!                 (3) NOBS - NUMBER OF 'OBS'
!                 (4) DESCR - INTEGER LIST OF BUFR DESCRIPTORS
!                 (5) MESSAGE - CHARACTER STRING FOR BUFR'ED MESSAGE
!                 (6) MESSLEN - LENGTH OF BUFR'ED MESSAGE IN OCTETS
!                 (7) DATETIME - YEAR, MONTH, DAY, HOUR, MINUTE ARRAY
!
! REVISION INFO :
!
!
! $Workfile: stbenc.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 20/01/2011 16:56:28$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         20/01/2011 16:56:28    Rosemary Lavery amend
!       IVER to be current version
!  1    MetDB_Refresh 1.0         12/01/2011 14:13:03    Rosemary Lavery
!       Initial Port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE ENBUFR_mod

IMPLICIT NONE

! Interface Arguments

REAL, INTENT(INOUT)               :: DCVALS(:)
INTEGER, INTENT(INOUT)            :: NELEM
INTEGER, INTENT(INOUT)            :: NOBS
INTEGER, INTENT(INOUT)            :: DESCR(:)
CHARACTER (LEN=*), INTENT(INOUT)  :: MESSAGE
INTEGER, INTENT(INOUT)            :: MESSLEN
INTEGER, INTENT(INOUT)            :: DATETIME(:)

! Local Parameters

INTEGER, PARAMETER  :: IVER = 13         ! Gives current version

! Local Variables

INTEGER  :: NDESCR

LOGICAL  :: CMPRES
LOGICAL  :: DISPLAY

CHARACTER (LEN=4), SAVE  :: BUFR
CHARACTER (LEN=1)        :: NAMES

! ---------------------------------------------------------------------

! Assign string "BUFR" for reference with messages

BUFR = CHAR(66) // CHAR(85) // CHAR(70) // CHAR(82)

NDESCR  = 1
CMPRES  = .TRUE.
DISPLAY = .FALSE.
MESSAGE = ' '

CALL ENBUFR(DESCR, DCVALS, NDESCR, NELEM, NOBS, NAMES, DATETIME,  &
            MESSAGE, CMPRES, MESSLEN, IVER)

IF (MESSAGE(1:4) /= BUFR) THEN
   PRINT*, 'A SATOB BULLETIN NOT SENT TO MDB'
   RETURN
END IF

NOBS = 10000

RETURN
END SUBROUTINE STBENC
