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
! $Workfile: chkmodid.f$ $Folder: pre_refresh$
! $Revision: 5$ $Date: 17/08/2009 16:45:56$
!
! CHANGE RECORD :
!
! $Log:
!  5    Met_DB_Project 1.4         17/08/2009 16:45:56    Brian Barwell
!       UKV_AIR added as element 17. Regional NEMO model elements deleted (28
!       & 29).
!  4    Met_DB_Project 1.3         22/05/2008 16:23:35    Brian Barwell
!       Assign model identifiers for NEMO models.
!  3    Met_DB_Project 1.2         19/10/2006 16:21:51    Stan Kellett    Added
!        Ninth degree North Atlantic, Med and Indian Ocean models.
!  2    Met_DB_Project 1.1         06/03/2006 15:24:57    Brian Barwell   Allow
!        specification of MODEL MES4K_AIR to get 4km Mesoscale model merged
!       data.
!  1    Met_DB_Project 1.0         30/01/2006 20:21:41    Sheila Needham  
! $
! Revision 2.1  2004/12/06 12:22:14  usmdb
! 2.1.  20 December 2004.  Brian Barwell.  Remedy CHG009255.
! Add NAE_AIR as element 16 of DataName for NAE Model merged data.
!
! Revision 2.0  2001/01/08  11:58:30  11:58:30  usmdb (MetDB account c/o John C
!     Ward)
! Replaced data statements with array initialisations on
! first call to routine only. This is because there are
! too many continuation lines with the statements method and
! the NAG f95 compiler issues an error. Added copyright and
! modified header - S.Cox
!
! Revision 1.4  2000/11/07  12:16:15  12:16:15  usmdb (Generic MDB account)
! 20 Nov 2000    C Long
! 1.4  Add GLOBL_SST to list of models recognised.
!
! Revision 1.3  97/09/22  11:11:25  11:11:25  uspm (Pat McCormack)
! Add Thailand model ids
!
! Revision 1.2  1997/08/20 12:21:36  uspm
! Use of NEONS naming convention
!
! Revision 1.1  1997/08/04 13:45:31  uspm
! Initial revision
!
! 29-09-97  !B  : Addition of Model id names for TOMS project - S.Cox
!
! 01-09-97  !A  : All model id names (DataName) have changed so that
!               : they follow the NEONS naming convention - S.Cox
!
! 28-07-97      : Operational - S.Cox
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2009 - MET OFFICE. All Rights Reserved.
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

      INTEGER   MaxNames

      PARAMETER (MaxNames=40)

!-----------------------------------------------------------------------
! Declare INTEGER variables (in alphabetical order)
!-----------------------------------------------------------------------

      INTEGER   CodeName         !- BUFR code value
      INTEGER   I                !- General loop counter
      INTEGER   LengthName       !- Length of BUFR code name
      INTEGER   NameLen          !- Length of DataName

!-----------------------------------------------------------------------
! Declare LOGICAL variables
!-----------------------------------------------------------------------

      LOGICAL   First            !- TRUE on 1st call to routine     !2.0

!-----------------------------------------------------------------------
! Declare CHARACTER variables (in alphabetical order)
!-----------------------------------------------------------------------

      CHARACTER*(*)    InputName  !- BUFR code name

      CHARACTER*40     DataName(MaxNames)  !- Model Ids in program
      CHARACTER*80     HEAD       !- Revision information            !5

!-----------------------------------------------------------------------
! Data statements
!-----------------------------------------------------------------------

      DATA   First /.TRUE./                                         !2.0

!-----------------------------------------------------------------------
! SAVE statement to ensure all variables are still set on next call
!-----------------------------------------------------------------------

      SAVE

!-----------------------------------------------------------------------
! Initialise DataName array on first call to routine only. This section
! replaces the DATA statements in previous revisions. These
! contained too many continuation lines for the NAG f95 compiler.   !2.0
! The subscript numbering for DataName appears to be: 1-10 for global
! models, 11-20 for limited area atmospheric models, 21-30 for limited
! area ocean and wave models, 31-40 for Thailand area models.
! NAE_AIR assigned to 16, next to MSCAL_AIR_UK which it replaces.   !2.1
! MES4K_AIR assigned to 14 for 4km Mesoscale Model, March 2006.     !2.2
! 'NEMO' models assigned to 10, 28 and 29, June 2008.                !4
!-----------------------------------------------------------------------

      IF (First) THEN
!                                                  Revision information
        HEAD = '$Workfile: chkmodid.f$ ' //
     &         '$Revision: 5$ $Date: 17/08/2009 16:45:56$'
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
        DataName(10) = 'GLOBL_NEMO                             '     !4
        DataName(11) = '                                       '
        DataName(12) = '                                       '
        DataName(13) = '                                       '
        DataName(14) = 'MES4K_AIR                              '    !2.2
        DataName(15) = 'MSCAL_AIR_UK                           '
        DataName(16) = 'NAE_AIR                                '    !2.1
        DataName(17) = 'UKV_AIR                                '     !5
        DataName(18) = '                                       '
        DataName(19) = '                                       '
        DataName(20) = '                                       '
        DataName(21) = 'ERPN_WAVE                              '
        DataName(22) = '                                       '
        DataName(23) = 'INDN_OCN                               '    !ST3
        DataName(24) = 'MDTRN_SEA                              '    !ST3
        DataName(25) = 'NTH_NRTH_ATLC_SEA                      '    !ST3
        DataName(26) = 'NRTH_ATLC_SEA                          '
        DataName(27) = 'EQURL_PAFC_SEA                         '
        DataName(28) = '                                       '     !5
        DataName(29) = '                                       '     !5
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
      ENDIF

!-----------------------------------------------------------------------
! Loop over Model Id names in DATA statements to see if there is a
! match with the users InputName
!-----------------------------------------------------------------------

      CodeName=0

      IF (LengthName.GT.0) THEN
        DO I=1,MaxNames
          IF (DataName(I)(1:1).NE.' ') THEN
            NameLen=INDEX(DataName(I)(1:),' ')-1
            IF (InputName(1:LengthName).EQ.DataName(I)(1:NameLen)) THEN
              CodeName=I
            ENDIF
          ENDIF
        ENDDO
      ENDIF

      RETURN
      END
