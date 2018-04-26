FUNCTION OUKSTN(STNNUM)

!-----------------------------------------------------------------------
!
! Description:
!   This Function returns either .TRUE. if the station number input
!   is a UK overseas station otherwise it returns .FALSE.>
!
! Method:
!   The station number is input to the function as an integer
!   An IF statement then checks to see if the station number is one
!   of a list of overseas stations.
!   If it is then the return condition is set to .TRUE.
!   Else the return condition is set to .FALSE.
!   The file for reference purposes which the list of stations was
!   taken from is MDS5.MID**SYF.PICKLIST
!   where ** may change from time to time
!
! ARGUMENTS: STNNUM - WMO Block and station number          (I)
!            OUKSTN - returned
!
! REVISION INFO :
!
! $Workfile: oukstn.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 22/03/2011 10:04:04$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         22/03/2011 10:04:04    Brian Barwell   DO
!       loop rewritten for Fortran 90.
!  2    MetDB_Refresh 1.1         28/01/2011 10:03:51    Sheila Needham  Ported
!        to F90
!  1    MetDB_Refresh 1.0         28/01/2011 09:46:41    Sheila Needham
!       Initial F77 version
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
IMPLICIT NONE

! Arguments

INTEGER,INTENT(IN) :: STNNUM     ! Station Number input to the function
LOGICAL            :: OUKSTN

! Local Variables:

INTEGER,PARAMETER:: NUMSTNS=70   ! number of overseas stations in list
INTEGER          :: L            ! loop control Variable
INTEGER          :: STNLIST(NUMSTNS)  ! List of UK overseas stations
DATA STNLIST/08495,10401,10320,17601,61901,61902,88883,88878, &
             88897,88889,62001,62023,62026,62029,62051,62052, &
             62081,62101,62103,62105,62106,62107,62109,62112, &
             62114,62117,62120,62123,62125,62126,62128,62131, &
             62132,62133,62136,62137,62138,62142,62143,62144, &
             62145,62147,62152,62155,62162,62163,62166,62168, &
             62202,62206,62217,62301,62303,62304,62305,62402, &
             62407,62410,62414,63105,63108,63109,63110,63113, &
             63116,63117,63119,64045,64046,64049 /

! Loop over each station until list exhausted or station number
! is found in list of overseas station.

DO L=1,NUMSTNS
  OUKSTN = STNNUM == STNLIST(L)
  IF (OUKSTN) EXIT
END DO

RETURN
END FUNCTION OUKSTN
