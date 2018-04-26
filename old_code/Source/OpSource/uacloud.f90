SUBROUTINE UACLOUD(OB,ARRAY,QCBIT_ARRAY)

!-----------------------------------------------------------------------
!
! PROGRAM       : UACLOUD
!
! PURPOSE       : TO HANDLE 41414 SECTION IN TEMP PART B (CLOUD GROUP)
!
! DATA TYPE(S)  : UPPER AIR TEMP PART B
!
! CALLED BY     : UAXPAND
!
! CALLS         : IVALUE (FUNCTION)
!
! ARGUMENTS     : (1) OB:           INPUT REPORT (5-FIG GROUPS) STARTING
!                                    AT 41414
!                 (2) ARRAY:        OUTPUT ARRAY, WITH HEADER DATA
!                                    ALREADY IN IT
!                 (3) QCBIT_ARRAY:  OUTPUT SUBSCRIPT (TO BE UPDATED BY
!                                    UAXPAND)
!
! REVISION INFO :
!
!
! $Workfile: uacloud.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 24/01/2011 13:05:29$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         24/01/2011 13:05:29    Rosemary Lavery
!       Initial port
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

USE IVALUE_mod
USE METDB_COM_mod, ONLY: MISSIN

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN)  :: OB                ! (1) report being expanded
REAL, INTENT(INOUT)            :: ARRAY(:)          ! (2) array of decoded values
REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)  ! (3) array of qc bits

! Local Parameters

INTEGER, PARAMETER  :: BASE = 15   ! decode values array displacement

! Local Variables

INTEGER  :: CH                 ! cloud type high
INTEGER  :: CL                 ! cloud type low
INTEGER  :: CLDTYP             ! cloud type
INTEGER  :: CM                 ! cloud type medium
INTEGER  :: HC                 ! height code figure for lowest laye
INTEGER  :: NH                 ! total amount low cloud
INTEGER  :: REP_COUNT          ! number of repetitions

REAL     :: H
REAL     :: HLCONV(0:9) = (/ 25.,50.,100.,200.,300.,600.,1000.,1500., &
                             2000.,2500. /)

! ---------------------------------------------------------------------

SAVE

!initialize variables

REP_COUNT=0                    ! init to zero levels

! --------------------------------------------------------------------
! the 41414 section is a cloud group as in a synop: nh,cl,h,cm,ch
! (nh is the total amount of low cloud if there is low cloud,
!  otherwise the total amount of middle cloud: decide which from cl)
! --------------------------------------------------------------------

NH=IVALUE(OB(7:7))                  ! low or middle cloud amount
CLDTYP=IVALUE(OB(8:8))
IF (CLDTYP /= MISSIN) CL=CLDTYP+30. ! low cloud type
HC=IVALUE(OB(9:9))                  ! height of lowest layer
CLDTYP=IVALUE(OB(10:10))
IF (CLDTYP /= MISSIN) CM=CLDTYP+20. ! middle cloud type
CLDTYP=IVALUE(OB(11:11))
IF (CLDTYP /= MISSIN) CH=CLDTYP+10. ! high cloud type

! ---------------------------------------------------------------------
! Convert cloud height code figure to a real height.Check we have a
! valid cloud height code figure before processing.
! ---------------------------------------------------------------------

IF (HC >= 0 .AND. HC <= 9) THEN
  H=HLCONV(HC)
ELSE
  H=MISSIN
END IF

! --------------------------------------------------------------------
! set values in output array for encoding: 4 elements replicated for
! low, middle and high cloud - there may not be data for all of these.
! if there is low cloud, then h is its base and nh its amount
! --------------------------------------------------------------------

REP_COUNT=REP_COUNT+1                  ! REPLICATION COUNT

ARRAY(BASE+(4*REP_COUNT))=7            ! 008002, CODE FOR LOW
ARRAY(BASE+(4*REP_COUNT)+1)=CL         ! LOW CLOUD TYPE
ARRAY(BASE+(4*REP_COUNT)+2)=MISSIN

IF (CL > 0) THEN                       ! AMOUNT OF LOW CLOUD
  ARRAY(BASE+(4*REP_COUNT)+2)=NH
ELSE IF (CL <= 0.AND.NH /= 9) THEN     ! AMOUNT=0 IF NO LOW CLOUD
  ARRAY(BASE+(4*REP_COUNT)+2)=0
ELSE
  ARRAY(BASE+(4*REP_COUNT)+3)=MISSIN
END IF

IF (CL > 0) THEN                       ! HEIGHT OF LOWEST CLOUD
  ARRAY(BASE+(4*REP_COUNT)+3)=H
END IF

! --------------------------------------------------------------------
! QCbits for low cloud
! --------------------------------------------------------------------

QCBIT_ARRAY(BASE+(4*REP_COUNT))=0.     !ob type okay
QCBIT_ARRAY(BASE+(4*REP_COUNT)+1)=0.   !cloud type okay
QCBIT_ARRAY(BASE+(4*REP_COUNT)+2)=0.   !cloud amount okay
QCBIT_ARRAY(BASE+(4*REP_COUNT)+3)=0.   !cloud height okay

! --------------------------------------------------------------------
! if there is low cloud, the middle cloud amount is not known
! --------------------------------------------------------------------

IF (CM /= MISSIN) THEN
  REP_COUNT=REP_COUNT+1              ! increment replication count
  ARRAY(BASE+(4*REP_COUNT))=8        ! 008002, CODE FOR MIDDLE
  ARRAY(BASE+(4*REP_COUNT)+1)=CM     !middle cloud type

  IF (CL <= 0) THEN                  !amount of middle cloud
    ARRAY(BASE+(4*REP_COUNT)+2)=NH
  ELSE
    ARRAY(BASE+(4*REP_COUNT)+2)=MISSIN !middle amount not given
  END IF

  IF (CL <= 0 .AND. CM > 0) THEN
    ARRAY(BASE+(4*REP_COUNT)+3)=H       !high cloud base
  ELSE
    ARRAY(BASE+(4*REP_COUNT)+4)=MISSIN !high cloud base not given
  END IF
END IF

! --------------------------------------------------------------------
! QCbits for middle cloud
! --------------------------------------------------------------------

QCBIT_ARRAY(BASE+(4*REP_COUNT))=0.     !ob type okay
QCBIT_ARRAY(BASE+(4*REP_COUNT)+1)=0.   !cloud type okay
QCBIT_ARRAY(BASE+(4*REP_COUNT)+2)=0.   !cloud amount okay
QCBIT_ARRAY(BASE+(4*REP_COUNT)+3)=0.   !cloud height okay

! --------------------------------------------------------------------
! high cloud amount is never known, its base only if no low or middle
! --------------------------------------------------------------------

IF (CH /= MISSIN) THEN
  REP_COUNT=REP_COUNT+1           ! RESET REPLICATION COUNT
  ARRAY(BASE+(4*REP_COUNT))=9     ! 008002, CODE FOR HIGH
  ARRAY(BASE+(4*REP_COUNT)+1)=CH  ! HIGH CLOUD TYPE
  ARRAY(BASE+(4*REP_COUNT)+2)=MISSIN !no amount

  IF (CL <= 0 .AND. CM <= 0) THEN
    ARRAY(BASE+(4*REP_COUNT)+3)=H       !height of cloud
  ELSE
    ARRAY(BASE+(4*REP_COUNT)+3)=MISSIN !no height
  END IF
END IF

! --------------------------------------------------------------------
! QCbits for high cloud
! --------------------------------------------------------------------

QCBIT_ARRAY(BASE+(4*REP_COUNT))=0.     !ob type okay
QCBIT_ARRAY(BASE+(4*REP_COUNT)+1)=0.   !cloud type okay
QCBIT_ARRAY(BASE+(4*REP_COUNT)+2)=0.   !cloud amount okay
QCBIT_ARRAY(BASE+(4*REP_COUNT)+3)=0.   !cloud height okay

! --------------------------------------------------------------------
! set number of cloud replications
! --------------------------------------------------------------------

ARRAY(18)=REP_COUNT

RETURN
END SUBROUTINE UACLOUD
