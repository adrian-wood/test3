SUBROUTINE ALSELM(CTYPE,USRELM,NUM,LTEST)

!-----------------------------------------------------------------------
!
! ROUTINE       : ALSELM
!
! PURPOSE       : Corrects element names that are spelt wrongly in
!               : tech note 3 or those that are aliased.
!
! DARA TYPES    : METARS, TAFS, ATAFS, STNMAS, LNDSYN, SHPSYN, UKMMWI,
!               : ATOVSG, ATOVSL, BUOY, DRIFTR, TEMP, PILOT, DROPSOND,
!               : SSMI
!
! CALLED BY     : MDB
!
! CALLS         : Nothing
!
! ARGUMENTS     :
!
! CTYPE         : char*(*)  (ip)  : data subtype
! USRELM(*)     : char*(*)  (iop) : element names
! NUM           : integer   (ip)  : number in list
! LTEST         : logical   (ip)  : TRUE for diagnostics
!
! REVISION INFO :
!
! $Revision: 4$
! $Date: 25/11/2010 10:48:18$
! $Source: /data/us0400/mdb/op/lib/source/RCS/alselm.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         25/11/2010 10:48:18    Stan Kellett    added
!       USE nmstem_mod
!  3    MetDB_Refresh 1.2         18/11/2010 14:06:48    Richard Weedon
!       Updated to include call to NMSELM
!  2    MetDB_Refresh 1.1         17/11/2010 15:35:22    Stan Kellett
!       Removed revision infor missed in original porting and changed
!       copyright to 2010
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
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

! Use statements:
USE nmstem_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(IN)    ::  CTYPE !- input subtype
CHARACTER(*), INTENT(INOUT) ::  USRELM(*) !- input element names
INTEGER,      INTENT(IN)    ::  NUM   !- no. of elems in list
LOGICAL,      INTENT(IN)    ::  LTEST !- TRUE for diagnostics

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!-----------------------------------------------------------------------
! parameter statements
!-----------------------------------------------------------------------

INTEGER,     PARAMETER ::  NTYPE = 15 !- no. of subtypes
INTEGER,     PARAMETER ::  MAXELM = 15 !- max no. of elements per subtype

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

INTEGER      ::  I                    !- general index variable
INTEGER      ::  ICHG(NTYPE)          !- lengths of elem lists
INTEGER      ::  ITYPE                !- subtype array pointer
INTEGER      ::  J1,J2                !- loop counters

LOGICAL      ::  FIRST = .TRUE.       !- TRUE if 1st time in routine

CHARACTER(36) ::  OLDNAM(MAXELM,NTYPE) !- list of old elem names
CHARACTER(36) ::  NEWNAM(MAXELM,NTYPE) !- list of new elem names
CHARACTER(32) ::  CELEM             !- element name minus repl
CHARACTER(6)  ::  CINST              !- element name replication
CHARACTER(8)  ::  SUBTYPE(NTYPE)     !- list of subtypes

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! the section below puts old and new names into memory on first call to
! ALSELM only. This section replaces the DATA statements used in
! previous revisions, which containined too many continuation
! lines for the NAG95 compiler.                                     !2.0
!-----------------------------------------------------------------------

IFLABEL1: &
IF (FIRST) THEN
  FIRST = .FALSE.

  DO J1=1,NTYPE
    SUBTYPE(J1) = ' '
    ICHG(J1) = 0
    DO J2=1,MAXELM
      OLDNAM(J2,J1) = ' '
      NEWNAM(J2,J1) = ' '
    END DO
  END DO

  SUBTYPE( 1)   = 'METARS  '
  ICHG( 1)      = 9
  OLDNAM( 1, 1) = 'WIND_SPD                            '
  OLDNAM( 2, 1) = 'VRBL_WIND_MAX_DIR                   '
  OLDNAM( 3, 1) = 'VRBL_WIND_MIN_DIR                   '
  OLDNAM( 4, 1) = 'SIG_WX_DSCPT_ID                     '
  OLDNAM( 5, 1) = 'RCT_WX_DSCPT_ID                     '
  OLDNAM( 6, 1) = 'TOTAL_CLOUD                         '
  OLDNAM( 7, 1) = 'WIND_SHR_RNWY_NUM                   '
  OLDNAM( 8, 1) = 'WIND_SHR_PLL_RNWY_ID                '
  OLDNAM( 9, 1) = 'RNWY_WIND_SHR_ID                    '
  NEWNAM( 1, 1) = 'WND_SPD                             '
  NEWNAM( 2, 1) = 'VRBL_WND_MAX_DIR                    '
  NEWNAM( 3, 1) = 'VRBL_WND_MIN_DIR                    '
  NEWNAM( 4, 1) = 'SIG_WX_DSC_ID                       '
  NEWNAM( 5, 1) = 'RCT_WX_DSC_ID                       '
  NEWNAM( 6, 1) = 'SIGNIFICANT_CLOUD                   '
  NEWNAM( 7, 1) = 'WND_SHR_RNWY_NUM                    '
  NEWNAM( 8, 1) = 'WND_SHR_PLL_RNWY_ID                 '
  NEWNAM( 9, 1) = 'RNWY_WND_SHR_ID                     '

  SUBTYPE( 2)   = 'TAFS    '
  ICHG( 2)      = 3
  OLDNAM( 1, 2) = 'WIND_SPD                            '
  OLDNAM( 2, 2) = 'TOTAL_CLOUD                         '
  OLDNAM( 3, 2) = 'CHANGE_TOTAL_CLOUD                  '
  NEWNAM( 1, 2) = 'WND_SPD                             '
  NEWNAM( 2, 2) = 'SIGNIFICANT_CLOUD                   '
  NEWNAM( 3, 2) = 'CHANGE_SIGNIFICANT_CLOUD            '

  SUBTYPE( 3)   = 'ATAFS   '
  ICHG( 3)      = 3
  OLDNAM( 1, 3) = 'WIND_SPD                            '
  OLDNAM( 2, 3) = 'TOTAL_CLOUD                         '
  OLDNAM( 3, 3) = 'CHANGE_TOTAL_CLOUD                  '
  NEWNAM( 1, 3) = 'WND_SPD                             '
  NEWNAM( 2, 3) = 'SIGNIFICANT_CLOUD                   '
  NEWNAM( 3, 3) = 'CHANGE_SIGNIFICANT_CLOUD            '

  SUBTYPE( 4)   = 'STNMAS  '
  ICHG( 4)      = 5
  OLDNAM( 1, 4) = 'LATITUDE                            '
  OLDNAM( 2, 4) = 'LONGITUDE                           '
  OLDNAM( 3, 4) = 'RNFLL_STN_ID                        '
  OLDNAM( 4, 4) = 'STN_CHSTC_ID                        '
  OLDNAM( 5, 4) = 'STN_AVBTY_ID                        '
  NEWNAM( 1, 4) = 'LAT                                 '
  NEWNAM( 2, 4) = 'LON                                 '
  NEWNAM( 3, 4) = 'RNFL_STN_ID                         '
  NEWNAM( 4, 4) = 'STN_CRST_IC                         '
  NEWNAM( 5, 4) = 'STN_AVBY_ID                         '

  SUBTYPE( 5)   = 'LNDSYN  '
  ICHG( 5)      = 6
  OLDNAM( 1, 5) = 'LOW_CLOD_AMNT                       '
  OLDNAM( 2, 5) = 'LOW_CLOD_BASE_HGHT                  '
  OLDNAM( 3, 5) = 'PRCTN_PERD_1                        '
  OLDNAM( 4, 5) = 'PRCTN_AMNT_1                        '
  OLDNAM( 5, 5) = 'PRCTN_PERD_2                        '
  OLDNAM( 6, 5) = 'PRCTN_AMNT_2                        '
  NEWNAM( 1, 5) = 'LWST_CLOD_AMNT                      '
  NEWNAM( 2, 5) = 'LWST_CLOD_BASE_HGHT                 '
  NEWNAM( 3, 5) = 'PRCTN_PERD_SCTN1                    '
  NEWNAM( 4, 5) = 'PRCTN_AMNT_SCTN1                    '
  NEWNAM( 5, 5) = 'PRCTN_PERD_SCTN3                    '
  NEWNAM( 6, 5) = 'PRCTN_AMNT_SCTN3                    '

  SUBTYPE( 6)   = 'SHPSYN  '
  ICHG( 6)      = 6
  OLDNAM( 1, 6) = 'LOW_CLOD_AMNT                       '
  OLDNAM( 2, 6) = 'LOW_CLOD_BASE_HGHT                  '
  OLDNAM( 3, 6) = 'PRCTN_PERD_1                        '
  OLDNAM( 4, 6) = 'PRCTN_AMNT_1                        '
  OLDNAM( 5, 6) = 'PRCTN_PERD_2                        '
  OLDNAM( 6, 6) = 'PRCTN_AMNT_2                        '
  NEWNAM( 1, 6) = 'LWST_CLOD_AMNT                      '
  NEWNAM( 2, 6) = 'LWST_CLOD_BASE_HGHT                 '
  NEWNAM( 3, 6) = 'PRCTN_PERD_SCTN1                    '
  NEWNAM( 4, 6) = 'PRCTN_AMNT_SCTN1                    '
  NEWNAM( 5, 6) = 'PRCTN_PERD_SCTN3                    '
  NEWNAM( 6, 6) = 'PRCTN_AMNT_SCTN3                    '

  SUBTYPE( 7)   = 'UKMMWI  '
  ICHG( 7)      = 3
  OLDNAM( 1, 7) = 'UKMO_WIND_SPED                      '
  OLDNAM( 2, 7) = 'UKMO_WIND_DRCTN                     '
  OLDNAM( 3, 7) = 'UKMO_PRDT_CNFC                      '
  NEWNAM( 1, 7) = 'FRST_UKMO_WIND_SPED                 '
  NEWNAM( 2, 7) = 'FRST_UKMO_WIND_DRCTN                '
  NEWNAM( 3, 7) = 'FRST_UKMO_PRDT_CNFC                 '

  SUBTYPE( 8)   = 'ATOVSG  '
  ICHG( 8)      = 6
  OLDNAM( 1, 8) = 'SEA_RGHS_1                          '
  OLDNAM( 2, 8) = 'SEA_RGHS_2                          '
  OLDNAM( 3, 8) = 'GRDY_MODL_PMTR_1                    '
  OLDNAM( 4, 8) = 'GRDY_MODL_PMTR_2                    '
  OLDNAM( 5, 8) = 'GRDY_MODL_PMTR_3                    '
  OLDNAM( 6, 8) = 'GRDY_MODL_PMTR_4                    '
  NEWNAM( 1, 8) = 'SEA_RGHS1                           '
  NEWNAM( 2, 8) = 'SEA_RGHS2                           '
  NEWNAM( 3, 8) = 'GRDY_MODL_PMTR1                     '
  NEWNAM( 4, 8) = 'GRDY_MODL_PMTR2                     '
  NEWNAM( 5, 8) = 'GRDY_MODL_PMTR3                     '
  NEWNAM( 6, 8) = 'GRDY_MODL_PMTR4                     '

  SUBTYPE( 9)   = 'ATOVSL  '
  ICHG( 9)      = 6
  OLDNAM( 1, 9) = 'SEA_RGHS_1                          '
  OLDNAM( 2, 9) = 'SEA_RGHS_2                          '
  OLDNAM( 3, 9) = 'GRDY_MODL_PMTR_1                    '
  OLDNAM( 4, 9) = 'GRDY_MODL_PMTR_2                    '
  OLDNAM( 5, 9) = 'GRDY_MODL_PMTR_3                    '
  OLDNAM( 6, 9) = 'GRDY_MODL_PMTR_4                    '
  NEWNAM( 1, 9) = 'SEA_RGHS1                           '
  NEWNAM( 2, 9) = 'SEA_RGHS2                           '
  NEWNAM( 3, 9) = 'GRDY_MODL_PMTR1                     '
  NEWNAM( 4, 9) = 'GRDY_MODL_PMTR2                     '
  NEWNAM( 5, 9) = 'GRDY_MODL_PMTR3                     '
  NEWNAM( 6, 9) = 'GRDY_MODL_PMTR4                     '

  SUBTYPE(10)   = 'BUOY    '
  ICHG(10)      = 7
  OLDNAM( 1,10) = 'WIND_WAVE_PERD_1                    '
  OLDNAM( 2,10) = 'WIND_WAVE_HGHT_1                    '
  OLDNAM( 3,10) = 'WIND_WAVE_PERD_2                    '
  OLDNAM( 4,10) = 'WIND_WAVE_HGHT_2                    '
  OLDNAM( 5,10) = 'ENGRG_INFMN_1                       '
  OLDNAM( 6,10) = 'ENGRG_INFMN_2                       '
  OLDNAM( 7,10) = 'ENGRG_INFMN_3                       '
  NEWNAM( 1,10) = 'WIND_WAVE_PERD1                     '
  NEWNAM( 2,10) = 'WIND_WAVE_HGHT1                     '
  NEWNAM( 3,10) = 'WIND_WAVE_PERD2                     '
  NEWNAM( 4,10) = 'WIND_WAVE_HGHT2                     '
  NEWNAM( 5,10) = 'ENGRG_INFMN1                        '
  NEWNAM( 6,10) = 'ENGRG_INFMN2                        '
  NEWNAM( 7,10) = 'ENGRG_INFMN3                        '

  SUBTYPE(11)   = 'DRIFTR  '
  ICHG(11)      = 7
  OLDNAM( 1,11) = 'WIND_WAVE_PERD_1                    '
  OLDNAM( 2,11) = 'WIND_WAVE_HGHT_1                    '
  OLDNAM( 3,11) = 'WIND_WAVE_PERD_2                    '
  OLDNAM( 4,11) = 'WIND_WAVE_HGHT_2                    '
  OLDNAM( 5,11) = 'ENGRG_INFMN_1                       '
  OLDNAM( 6,11) = 'ENGRG_INFMN_2                       '
  OLDNAM( 7,11) = 'ENGRG_INFMN_3                       '
  NEWNAM( 1,11) = 'WIND_WAVE_PERD1                     '
  NEWNAM( 2,11) = 'WIND_WAVE_HGHT1                     '
  NEWNAM( 3,11) = 'WIND_WAVE_PERD2                     '
  NEWNAM( 4,11) = 'WIND_WAVE_HGHT2                     '
  NEWNAM( 5,11) = 'ENGRG_INFMN1                        '
  NEWNAM( 6,11) = 'ENGRG_INFMN2                        '
  NEWNAM( 7,11) = 'ENGRG_INFMN3                        '

  SUBTYPE(12)   = 'TEMP    '
  ICHG(12)      = 2
  OLDNAM( 1,12) = 'MXMM_WIND_LEVL_WIND_SHER_1          '
  OLDNAM( 2,12) = 'MXMM_WIND_LEVL_WIND_SHER_2          '
  NEWNAM( 1,12) = 'MXMM_WIND_LEVL_WIND_SHER1           '
  NEWNAM( 2,12) = 'MXMM_WIND_LEVL_WIND_SHER2           '

  SUBTYPE(13)   = 'PILOT   '
  ICHG(13)      = 2
  OLDNAM( 1,13) = 'MXMM_WIND_LEVL_WIND_SHER_1          '
  OLDNAM( 2,13) = 'MXMM_WIND_LEVL_WIND_SHER_2          '
  NEWNAM( 1,13) = 'MXMM_WIND_LEVL_WIND_SHER1           '
  NEWNAM( 2,13) = 'MXMM_WIND_LEVL_WIND_SHER2           '

  SUBTYPE(14)   = 'DROPSOND'
  ICHG(14)      = 2
  OLDNAM( 1,14) = 'MXMM_WIND_LEVL_WIND_SHER_1          '
  OLDNAM( 2,14) = 'MXMM_WIND_LEVL_WIND_SHER_2          '
  NEWNAM( 1,14) = 'MXMM_WIND_LEVL_WIND_SHER1           '
  NEWNAM( 2,14) = 'MXMM_WIND_LEVL_WIND_SHER2           '

  SUBTYPE(15)   = 'SSMI    '
  ICHG(15)      = 15
  OLDNAM( 1,15) = 'LTTD_1                              '
  OLDNAM( 2,15) = 'LNGD_1                              '
  OLDNAM( 3,15) = 'SRFC_TYPE_1                         '
  OLDNAM( 4,15) = 'BRGTS_TMPR_F85GHZ_V_1               '
  OLDNAM( 5,15) = 'BRGTS_TMPR_F85GHZ_H_1               '
  OLDNAM( 6,15) = 'LTTD_2                              '
  OLDNAM( 7,15) = 'LNGD_2                              '
  OLDNAM( 8,15) = 'SRFC_TYPE_2                         '
  OLDNAM( 9,15) = 'BRGTS_TMPR_F85GHZ_V_2               '
  OLDNAM(10,15) = 'BRGTS_TMPR_F85GHZ_H_2               '
  OLDNAM(11,15) = 'LTTD_3                              '
  OLDNAM(12,15) = 'LNGD_3                              '
  OLDNAM(13,15) = 'SRFC_TYPE_3                         '
  OLDNAM(14,15) = 'BRGTS_TMPR_F85GHZ_V_3               '
  OLDNAM(15,15) = 'BRGTS_TMPR_F85GHZ_H_3               '
  NEWNAM( 1,15) = 'LTTD1                               '      
  NEWNAM( 2,15) = 'LNGD1                               '      
  NEWNAM( 3,15) = 'SRFC_TYPE1                          '      
  NEWNAM( 4,15) = 'BRGTS_TMPR_F85GHZ_V1                '      
  NEWNAM( 5,15) = 'BRGTS_TMPR_F85GHZ_H1                '      
  NEWNAM( 6,15) = 'LTTD2                               '    
  NEWNAM( 7,15) = 'LNGD2                               '   
  NEWNAM( 8,15) = 'SRFC_TYPE2                          '   
  NEWNAM( 9,15) = 'BRGTS_TMPR_F85GHZ_V2                ' 
  NEWNAM(10,15) = 'BRGTS_TMPR_F85GHZ_H2                '    
  NEWNAM(11,15) = 'LTTD3                               '    
  NEWNAM(12,15) = 'LNGD3                               '   
  NEWNAM(13,15) = 'SRFC_TYPE3                          '   
  NEWNAM(14,15) = 'BRGTS_TMPR_F85GHZ_V3                '   
  NEWNAM(15,15) = 'BRGTS_TMPR_F85GHZ_H3                '   

END IF IFLABEL1

!-----------------------------------------------------------------------
! match subtype to find which list to compare
!-----------------------------------------------------------------------

DO J1=1,NTYPE
  IF (CTYPE == SUBTYPE(J1)) THEN
    ITYPE=J1
    GOTO 11
  END IF
END DO

!-----------------------------------------------------------------------
! no changes for this ctype
!-----------------------------------------------------------------------

GOTO 999

11    CONTINUE

IF (LTEST) WRITE(*,*)'In ALSELM: ITYPE = ',ITYPE

!-----------------------------------------------------------------------
! loop over users element names
!-----------------------------------------------------------------------

DOLABEL1: &
DO J1=1,NUM

!-----------------------------------------------------------------------
! Loop over element names in subtype character array. If user element
! name USRELM matches the OLDNAM exactlty or the non-replicated
! USRELM matches the OLDNAM exactly, put NEWNAM into USRELM.
!-----------------------------------------------------------------------

DOLABEL2: &
  DO J2=1,ICHG(ITYPE)                                         
IFLABEL2: &
    IF (USRELM(J1) == OLDNAM(J2,ITYPE)) THEN                  
      I=INDEX(NEWNAM(J2,ITYPE),' ')-1                         
      USRELM(J1) = NEWNAM(J2,ITYPE)(1:I)                     
      GOTO 155                                                
    ELSE                                                     
      CALL NMSTEM(USRELM(J1),CELEM,CINST)                     
      IF (CELEM == OLDNAM(J2,ITYPE)) THEN                     
        I=INDEX(NEWNAM(J2,ITYPE),' ')-1                       
        USRELM(J1) = NEWNAM(J2,ITYPE)(1:I)//CINST             
        GOTO 155                                              
      END IF                                                 
    END IF IFLABEL2                                         
  END DO DOLABEL2 !- j2                                       

155     CONTINUE

END DO DOLABEL1 !- j1

999   CONTINUE

RETURN
END SUBROUTINE ALSELM
