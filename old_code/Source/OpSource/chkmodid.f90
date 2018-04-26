SUBROUTINE CHKMODID(InputName,LengthName,CodeName)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : CHKMODID
!
! PURPOSE       : Given a MODEL ID name, return the corresponding
!               : MODEL ID code.
!
! CALLED BY     : GETREQ
!
! ARGUMENTS     :
!
! InputName     : char*(*) (ip) : Code name to get Code value for
! LengthName    : integer  (ip) : Length of NAME (variable)
! CodeName      : integer  (op) : Code value for Code name
!
! REVISION INFO :
!
! $Workfile: chkmodid.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 01/11/2010 15:07:07$
!
! CHANGE RECORD :
!
! $Log:
!  6    MetDB_Refresh 1.5         01/11/2010 15:07:07    Stan Kellett    rework
!        done after review
!  5    MetDB_Refresh 1.4         20/10/2010 09:03:14    Stan Kellett    Files
!       ready for review
!  4    MetDB_Refresh 1.3         15/10/2010 16:17:35    Stan Kellett    Basic
!       port almost complete just require the modules added for the interface
!       files they will need.
!  3    MetDB_Refresh 1.2         14/10/2010 17:59:43    Stan Kellett    Basic
!       port done upto changing of argument declarations. Ordering of
!       declarations and interface files still to do.
!  2    MetDB_Refresh 1.1         12/10/2010 10:08:57    Stan Kellett
!       Continuation markers changed to f90 standard.
!       Logical operators such as .LE. .GE. .GT. .LT. etc changed to <=, >=,
!       >, and < as part of porting to f90/95
!  1    MetDB_Refresh 1.0         06/10/2010 16:25:28    Stan Kellett
!       Initial f77 version of software before porting to f90/95
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------
! Parameter Statements
!-----------------------------------------------------------------------
INTEGER, PARAMETER             :: MaxNames = 40

!-----------------------------------------------------------------------
! Declare argument variables (in interface order)
!-----------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN)   :: InputName  !- BUFR code name
INTEGER, INTENT(IN)            :: LengthName !- Length of BUFR code name
INTEGER, INTENT(INOUT)         :: CodeName   !- BUFR code value

!-----------------------------------------------------------------------
! Declare INTEGER variables (in alphabetical order)
!-----------------------------------------------------------------------
INTEGER                        :: I          !- General loop counter
INTEGER                        :: NameLen    !- Length of DataName

!-----------------------------------------------------------------------
! Declare LOGICAL variables
!-----------------------------------------------------------------------
LOGICAL                        :: First=.TRUE. !- TRUE on 1st call to routine

!-----------------------------------------------------------------------
! Declare CHARACTER(), PARAMETER      :: variables (in alphabetical order)
!-----------------------------------------------------------------------
CHARACTER(LEN=40)                     :: DataName(MaxNames)  !- Model Ids in program

!-----------------------------------------------------------------------
! SAVE statement to ensure all variables are still set on next call
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! Initialise DataName array on first call to routine only. This section
! replaces the DATA statements in previous revisions. These
! contained too many continuation lines for the NAG f95 compiler.
! The subscript numbering for DataName appears to be: 1-10 for global
! models, 11-20 for limited area atmospheric models, 21-30 for limited
! area ocean and wave models, 31-40 for Thailand area models.
! NAE_AIR assigned to 16, next to MSCAL_AIR_UK which it replaces.
! MES4K_AIR assigned to 14 for 4km Mesoscale Model, March 2006.
! 'NEMO' models assigned to 10, 28 and 29, June 2008.
!-----------------------------------------------------------------------

FIRSTCHK: &
IF (First) THEN
!                                                  Revision information
  First = .FALSE.

  DataName( 1) = 'GLOBL_AIR                              '
  DataName( 2) = 'GLOBL_AIR_FRCT                         '
  DataName( 3) = 'GLOBL_AIR_AVTN                         '
  DataName( 4) = '                                       '
  DataName( 5) = '                                       '
  DataName( 6) = 'GLOBL_STPR                             '
  DataName( 7) = 'GLOBL_WAVE                             '
  DataName( 8) = 'GLOBL_SEA                              '
  DataName( 9) = 'GLOBL_SST                              '
  DataName(10) = 'GLOBL_NEMO                             '
  DataName(11) = '                                       '
  DataName(12) = '                                       '
  DataName(13) = '                                       '
  DataName(14) = 'MES4K_AIR                              '
  DataName(15) = 'MSCAL_AIR_UK                           '
  DataName(16) = 'NAE_AIR                                '
  DataName(17) = 'UKV_AIR                                '
  DataName(18) = '                                       '
  DataName(19) = '                                       '
  DataName(20) = '                                       '
  DataName(21) = 'ERPN_WAVE                              '
  DataName(22) = '                                       '
  DataName(23) = 'INDN_OCN                               '
  DataName(24) = 'MDTRN_SEA                              '
  DataName(25) = 'NTH_NRTH_ATLC_SEA                      '
  DataName(26) = 'NRTH_ATLC_SEA                          '
  DataName(27) = 'EQURL_PAFC_SEA                         '
  DataName(28) = '                                       '
  DataName(29) = '                                       '
  DataName(30) = '                                       '
  DataName(31) = '                                       '
  DataName(32) = '                                       '
  DataName(33) = '                                       '
  DataName(34) = '                                       '
  DataName(35) = 'MSCAL_AIR_THLD                         '
  DataName(36) = 'MSCAL_AIR_BNGK                         '
  DataName(37) = 'SOTH_EAST_ASIA_AIR                     '
  DataName(38) = '                                       '
  DataName(39) = '                                       '
  DataName(40) = '                                       '
END IF FIRSTCHK

!-----------------------------------------------------------------------
! Loop over Model Id names in DATA statements to see if there is a
! match with the users InputName
!-----------------------------------------------------------------------

CodeName=0

IF (LengthName >  0) THEN
  DO I=1,MaxNames
   IF (DataName(I)(1:1) /= ' ') THEN
      NameLen=INDEX(DataName(I)(1:),' ')-1
      IF (InputName(1:LengthName) == DataName(I)(1:NameLen)) THEN
        CodeName=I
      END IF
    END IF
  END DO
END IF

RETURN
END SUBROUTINE CHKMODID
