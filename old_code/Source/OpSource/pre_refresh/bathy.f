      SUBROUTINE BATHY(REPORT,TTAAII,CCCC,OCOR,NFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : BATHY
!
! PURPOSE       : To expand, encode & store one BATHY report
!
! CALLED BY     : BATESBUL
!
! CALLS         : BTHSCn (n=1,4), BTHIND,
!                 ENBUFR, CCCODE, AIRSTO                              !2
!
! PARAMETERS    : REPORT   to be expanded & stored                  (I)
!                 TTAAii   bulletin identifier                      (I)
!                 CCCC     originating centre                       (I)
!                 OCOR     correction flag                          (I)
!                 NFT      FT number for BATHY & TESAC              (I)
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 25/04/2006 15:08:51$
! $Author: Brian Barwell$
! $Folder: pre_refresh$
! $Workfile: bathy.f$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         25/04/2006 15:08:51    Brian Barwell
!       Increae array sizes and string lengths for BATYHYs with up to 1600
!       levels.
!  1    Met_DB_Project 1.0         30/01/2006 20:20:58    Sheila Needham  
! $
!
! Revision 2.3  2003/06/11 08:33:38  usmdb
! 11 June 2003    C Long
! 2.3  Encode with new sequence 331203 (with 16-bit replication count).
!
! Revision 2.2  2003/06/09  12:57:37  12:57:37  usmdb (MetDB account c/o usjh)
! 9 June 2003    C Long
! 2.2  Increase array size from 500 to 1500 (after obs with 400 levels).
!
! Revision 2.1  2001/12/05  09:09:24  09:09:24  usmdb (MetDB account c/o usjh)
! Replace call to AMDSTO with AIRSTO - S.Cox
!
! Revision 2.0  2001/09/05  09:28:27  09:28:27  usmdb (Generic MetDB account)
! Initial revision
!
! INTRODUCED  17/09/01
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2006 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

! For alphanumeric BATHY bulletins without optional sections and      !2
! with n levels, the following approximate lengths in bytes apply:    !2
!    Original message: 6n+96      BUFR message: 4.25n+77              !2
!    Message trailer:  23         Total length: 10.25n+196            !2
!    BUFR descriptors: 2n+21      Values for encoding: 4n+40          !2
! Values in the PARAMETER statement below allow for up to 1600 levels !2
! in the BUFR message but a bit more in the original. The highest     !2
! seen to date (24/4/2006) is 1347 levels. A check in BTHSC2 limits   !2
! the BUFR message to 1600 levels - this will need changing as well   !2
! as parameters below if there is a need to increase the limit.       !2

      INTEGER   LARRAY    ! Size of descriptor array (DESCR)
      INTEGER   LEVELS    ! Max. levels in BUFR message               !2
      INTEGER   LSTRING   ! Length of MESAGE string
      INTEGER   LVALS     ! Size of EXPARR array                      !2
      PARAMETER (LEVELS=1600, LARRAY=2*LEVELS+21)                     !2
      PARAMETER (LVALS=2*LARRAY, LSTRING=17200)                       !2

      CHARACTER*(*)  REPORT
      CHARACTER*(*)  TTAAII
      CHARACTER*9    ID
      CHARACTER*9    IDENT    !- copy of ID                         !2.1
      CHARACTER*4    MIMJ
      CHARACTER*4    CCCC
      CHARACTER*23   ENTRY
      CHARACTER*(LSTRING) MESAGE

      REAL           EXPARR(0:LVALS)                                  !2
      REAL           TOTDEP        ! total depth

      INTEGER        REPLEN        ! length of character string
      INTEGER        MESLEN        ! length of BUFR message
      INTEGER        DATIME(5)
      INTEGER        I,IERR
      INTEGER        POS           ! pointer to character string
      INTEGER        ARAYPOS       ! array pointer (to end of profile)
      INTEGER        NFT           ! FT number for storage
      INTEGER        BLKSIZ
      INTEGER        ICCCC         ! number corresponding to CCCC
      INTEGER        DESCR(LARRAY)
      INTEGER        TOR(5),NOW(8)
      INTEGER        NOBS
      INTEGER        NDESCR
      INTEGER        IDES

      LOGICAL        CMPRES
      LOGICAL        OCOR
      LOGICAL        IDFLG         ! set if call sign, not number

      CHARACTER*80   HEAD          ! revision information             !2
      HEAD='$Workfile: bathy.f$ ' //
     &     '$Revision: 2$ $Date: 25/04/2006 15:08:51$'

! Set first element in array to indicate that every
! other element is quality control bit - not as yet used

      EXPARR(0)=1.0
      DO I=1,LVALS                                                    !2
        EXPARR(I)=-9999999.0
      ENDDO

! Expand the report section by section, starting with section 4 to get
! the call sign.  Only go on past section 1 if there's no error.
! Keep a copy of ID in IDENT. This is because ENBUFR will convert ID
! from EBCDIC to ASCII and we need to pass an un-converted ID to AIRSTO.

      REPLEN=LEN(REPORT)
      CALL BTHSC4(REPORT,REPLEN,EXPARR,IDFLG,ID)
      IDENT=ID                                                      !2.1

      POS=1
      CALL BTHSC1(REPORT,REPLEN,EXPARR,POS,DATIME,REPORT(1:4),IERR)

      IF (IERR.EQ.0) THEN
        CALL BTHSC2(REPORT,REPLEN,EXPARR,TOTDEP,POS,ARAYPOS)
        CALL BTHSC3(REPORT,REPLEN,EXPARR,TOTDEP,POS,ARAYPOS)

! Make an index entry.

        CALL BTHIND(EXPARR,IDFLG,IDENT,OCOR,ENTRY)                  !2.1

! Get system time as time of receipt to be used by ENBUFR

        CALL DATIM(NOW)
        DO I=0,4
          TOR(I+1)=NOW(8-I)
        ENDDO

! Put report before BUFR message in string to be stored

        MESAGE(1:REPLEN)=REPORT

! Encode BUFR message

        DESCR(1)=IDES(331203)                                       !2.3
        NDESCR=1
        NOBS=1
        CMPRES=.FALSE.
        CALL ENBUFR(DESCR,EXPARR,NDESCR,LVALS,NOBS,ID,TOR,            !2
     &              MESAGE(REPLEN+1:),CMPRES,MESLEN)

! Find BUFR code figure corresponding to CCCC.
! Put this & data type (31=oceanographic) in section 1 of BUFR message.

        IF (CCCC.NE.' ') THEN
          CALL CCCODE(287,ICCCC,CCCC)
          MESAGE(REPLEN+9:REPLEN+9)=CHAR(ICCCC/256)
          MESAGE(REPLEN+10:REPLEN+10)=CHAR(MOD(ICCCC,256))
        ENDIF

        MESAGE(REPLEN+13:REPLEN+13)=CHAR(31)

!-----------------------------------------------------------------------
! Set bulletin details to go in trailer and call AIRSTO
!-----------------------------------------------------------------------

        ENTRY(3:6)=TTAAII(1:4)                                      !2.1
        ENTRY(7:7)=CHAR(0)                                          !2.1
        IF (OCOR) ENTRY(7:7)=CHAR(1)                                !2.1
        ENTRY(8:11)=CCCC                                            !2.1
        BLKSIZ=27998                                                !2.1

        CALL AIRSTO(DATIME,ENTRY,MESAGE(:REPLEN+MESLEN),            !2.1
     &              NFT,BLKSIZ,IDENT,TOR)                           !2.1
      ENDIF

      RETURN                                                        !2.1
      END
