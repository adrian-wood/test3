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
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /data/us0400/mdb/op/lib/source/RCS/alselm.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2002/02/04  11:46:47  11:46:47  usmdb (Generic MetDB account)
! Correct SSMI entry. OLDNAM repeated in error. Code now has
! OLDNAM entries followed by NEWNAM entries. - S.Cox
!
! Revision 2.0  2001/01/08  11:58:26  11:58:26  usmdb (Generic MetDB account)
! Replaced too-long DATA statements by array
! initialisation. Delete an old commented section
! of code. Added copyright and tidied header - S.Cox
!
! Revision 1.9  99/01/14  14:21:04  14:21:04  usmdb (Generic MDB account)
! 18-01-1999 S.Cox, Ref MetDB problem 336
! Alias element names of the form ELEMENT_1 where ELEMENT is not
! a true replication. e.g. for LNDSYN, PRCTN_PERD_1 becomes
! PRCTN_PERD_SCTN1.
!
! Revision 1.8  98/10/15  11:38:37  11:38:37  usmdb (Generic MDB account)
! 19-10-98 : Addition of subtype ATAFS (Automatic TAFS). They have
! exactly the same attributes as TAFS - S.Cox
!
! Revision 1.7  98/01/19  08:31:02  08:31:02  usmdb (Generic MDB account)
! Add aliasing for subtype UKMMWI
!
! Revision 1.6  1997/08/04 12:48:29  uspm
! First revisioned version for 1  - with Y2K changes
!
! Revision 1.5  1997/07/09 07:36:41  uspm
! Latest version from COSMOS - changes marked !A
!
! Revision 1.4  1997/02/12 12:13:28  uspm
! Update dir name in variable head
!
! Revision 1.3  1997/02/11 12:45:53  uspm
! Put space between comment marker and $Log entries
!
! Revision 1.2  1997/02/11 12:44:50  uspm
! Put $Source: /data/us0400/mdb/op/lib/source/RCS/alselm.F,v $ on a
! line without the quotes so if it becomes too long after substitution
! should get warning but no failure at compile time.
!
! Revision 1.1  1997/02/11 12:24:32  uspm
! Initial revision
!
! 01-12-97  !B  : Add aliasing for subtype UKMMWI
!
! 27-05-97  !A  : Add aliasing for subtypes LNDSYN & SHPSYN
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------
! parameter statements
!-----------------------------------------------------------------------

INTEGER NTYPE              !- no. of subtypes
INTEGER MAXELM             !- max no. of elements per subtype

PARAMETER (NTYPE=15)                                      !1.9!B!A
PARAMETER (MAXELM=15)                                         !1.9

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

INTEGER        I                      !- general index variable
INTEGER        ICHG(NTYPE)            !- lengths of elem lists
INTEGER        ITYPE                  !- subtype array pointer
INTEGER        J1,J2                  !- loop counters
INTEGER        NUM                    !- no. of elems in list

LOGICAL        FIRST                  !- TRUE if 1st time in routine
LOGICAL        LTEST                  !- TRUE for diagnostics

CHARACTER*(*)  CTYPE                  !- input subtype
CHARACTER*(*)  USRELM(*)              !- input element names
CHARACTER*36   OLDNAM(MAXELM,NTYPE)   !- list of old elem names
CHARACTER*36   NEWNAM(MAXELM,NTYPE)   !- list of new elem names
CHARACTER*32   CELEM                  !- element name minus repl
CHARACTER*6    CINST                  !- element name replication
CHARACTER*8    SUBTYPE(NTYPE)         !- list of subtypes
CHARACTER*132  HEAD

DATA FIRST/.TRUE./

!-----------------------------------------------------------------------
! the section below puts old and new names into memory on first call to
! ALSELM only. This section replaces the DATA statements used in
! previous revisions, which containined too many continuation
! lines for the NAG95 compiler.                                     !2.0
!-----------------------------------------------------------------------

IF (FIRST) THEN
  FIRST = .FALSE.

  HEAD='$RCSfile: $ ' //&
      &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'

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
  NEWNAM( 1,15) = 'LTTD1                               '      !2.1
  NEWNAM( 2,15) = 'LNGD1                               '      !2.1
  NEWNAM( 3,15) = 'SRFC_TYPE1                          '      !2.1
  NEWNAM( 4,15) = 'BRGTS_TMPR_F85GHZ_V1                '      !2.1
  NEWNAM( 5,15) = 'BRGTS_TMPR_F85GHZ_H1                '      !2.1
  NEWNAM( 6,15) = 'LTTD2                               '      !2.1
  NEWNAM( 7,15) = 'LNGD2                               '      !2.1
  NEWNAM( 8,15) = 'SRFC_TYPE2                          '      !2.1
  NEWNAM( 9,15) = 'BRGTS_TMPR_F85GHZ_V2                '      !2.1
  NEWNAM(10,15) = 'BRGTS_TMPR_F85GHZ_H2                '      !2.1
  NEWNAM(11,15) = 'LTTD3                               '      !2.1
  NEWNAM(12,15) = 'LNGD3                               '      !2.1
  NEWNAM(13,15) = 'SRFC_TYPE3                          '      !2.1
  NEWNAM(14,15) = 'BRGTS_TMPR_F85GHZ_V3                '      !2.1
  NEWNAM(15,15) = 'BRGTS_TMPR_F85GHZ_H3                '      !2.1

ENDIF

!-----------------------------------------------------------------------
! match subtype to find which list to compare
!-----------------------------------------------------------------------

DO J1=1,NTYPE
  IF (CTYPE.EQ.SUBTYPE(J1)) THEN
    ITYPE=J1
    GOTO 11
  ENDIF
ENDDO

!-----------------------------------------------------------------------
! no changes for this ctype
!-----------------------------------------------------------------------

GOTO 999

11    CONTINUE

IF (LTEST) WRITE(*,*)'In ALSELM: ITYPE = ',ITYPE

!-----------------------------------------------------------------------
! loop over users element names
!-----------------------------------------------------------------------

DO J1=1,NUM

!-----------------------------------------------------------------------
! Loop over element names in subtype character array. If user element
! name USRELM matches the OLDNAM exactlty or the non-replicated
! USRELM matches the OLDNAM exactly, put NEWNAM into USRELM.
!-----------------------------------------------------------------------

  DO J2=1,ICHG(ITYPE)                                         !1.9
    IF (USRELM(J1).EQ.OLDNAM(J2,ITYPE)) THEN                  !1.9
      I=INDEX(NEWNAM(J2,ITYPE),' ')-1                         !1.9
      USRELM(J1) = NEWNAM(J2,ITYPE)(1:I)                      !1.9
      GOTO 155                                                !1.9
    ELSE                                                      !1.9
      CALL NMSTEM(USRELM(J1),CELEM,CINST)                     !1.9
      IF (CELEM.EQ.OLDNAM(J2,ITYPE)) THEN                     !1.9
        I=INDEX(NEWNAM(J2,ITYPE),' ')-1                       !1.9
        USRELM(J1) = NEWNAM(J2,ITYPE)(1:I)//CINST             !1.9
        GOTO 155                                              !1.9
      ENDIF                                                   !1.9
    ENDIF                                                     !1.9
  END DO  !- j2                                                !1.9

155     CONTINUE

END DO !- j1

999   CONTINUE

RETURN
END SUBROUTINE ALSELM
