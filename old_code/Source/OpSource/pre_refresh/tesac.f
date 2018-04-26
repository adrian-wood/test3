      SUBROUTINE TESAC(REPORT,TTAAII,CCCC,OCOR,NFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : TESAC
!
! PURPOSE       : To expand, encode & store one TESAC report
!
! CALLED BY     : BATESBUL
!
! CALLS         : TESSCn (n=1,5), TESIND,
!                 ENBUFR, CCCODE, AMDSTO
!
! PARAMETERS    : REPORT   to be expanded & stored                   (I)
!                 TTAAii   bulletin identifier                       (I)
!                 CCCC     originating centre                        (I)
!                 OCOR     correction flag                           (I)
!                 NFT      FT number for BATHY & TESAC               (I)
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 03/04/2006 11:26:59$
! $Source: /net/home/h01/mdb_new/op/lib/source/RCS/tesac.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         03/04/2006 11:26:59    Brian Barwell
!       Increase array sizes to allow storage of TESACs with up to 640 levels.
!  1    Met_DB_Project 1.0         30/01/2006 20:25:18    Sheila Needham  
! $
! Revision 2.3  2001/12/05 09:10:42  usmdb
! Call AIRSTO instead of AMDSTO - S.Cox
!
! Revision 2.2  2001/11/06  10:15:39  10:15:39  usmdb (Generic MetDB account)
! 19 Nov 2001     C Long
! 2.2  Strip down argument lists in calls to TESSCn.
!      Initialise value array before TESSC5 called, not after!
!
! Revision 2.1  2001/10/09  10:32:12  10:32:12  usmdb (Generic MetDB account)
! 15 Oct 2001    C Long
! 2.1  Correct TESSC5 call (arguments weren't set till after...)
!
! Revision 2.0  2001/09/05  09:28:27  09:28:27  usmdb (Generic MetDB account)
! Initial revision
!
! INTRODUCED 17/09/01
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

! For alphanumeric TESAC bulletins without optional sections and    !2.4
! with n levels, the following approximate lengths in bytes apply:  !2.4
!    Original message: 18n+51     BUFR message: 6.125n+77           !2.4
!    Message trailer:  23         Total length: 24.125n+151         !2.4
! Values in the PARAMETER statement below allow for up to 640       !2.4
! levels. The highest seen to date (10/3/2006) is 520 levels.       !2.4
! (Length of merged messages is about 48.75n+1093 but the number    !2.4
! of levels selected for merging is limited by the merge table.)    !2.4

      INTEGER        LARRAY      ! size of value & descriptor array
      INTEGER        LSTRING     ! size of string for text & message
      INTEGER        MAXREPL     ! max text length for given string
      INTEGER        BLKSIZ      ! blocksize of storage data set
      PARAMETER      (LARRAY=4000, LSTRING=15600, MAXREPL=11600)    !2.4
      PARAMETER      (BLKSIZ=27998)                                 !2.3

      CHARACTER*(*)  REPORT
      CHARACTER*9    ID
      CHARACTER*9    IDENT                                          !2.3
      CHARACTER*(*)  TTAAII
      CHARACTER*4    CCCC
      CHARACTER*23   ENTRY       ! index entry
      CHARACTER*(LSTRING) MESAGE ! report & then BUFR message
      CHARACTER*132  HEAD

      INTEGER        REPLEN      ! report length
      INTEGER        MESLEN      ! BUFR message length
      INTEGER        I           ! short-term loop variable
      INTEGER        POS         ! pointer to character string
      INTEGER        ARRPOS2     ! array pointer (end of T/sal profile)
      INTEGER        ARRPOS3     ! array pointer (end of current profile
      INTEGER        IERR        ! error flag - may be set by TESSCn
      INTEGER        IDES
      INTEGER        ICCCC       ! number corresponding to CCCC
      INTEGER        NOBS        ! number of obs in BUFR message (=1)
      INTEGER        DATIME(5)   ! date & time of data (input)
      INTEGER        NOW(8),TOR(5) ! current date & time
      INTEGER        NFT         ! FT number for storage
      INTEGER        NDESCR
      INTEGER        DESCR(LARRAY)

      REAL           EXPARR(0:LARRAY)
      REAL           ENDVALS(3)  ! sea depth & IxIxIxXrXr to go on end

      LOGICAL        CMPRES
      LOGICAL        OCOR          ! FLAG FOR CORRECTION
      LOGICAL        IDFLG

! Put arrays in dynamic common so that if expansion array overflows,
! it will overflow into arrays not used till encode...

      COMMON /TESACOM/ EXPARR,DESCR,MESAGE

      HEAD='$RCSfile: tesac.f,v $ ' //
     &     '$Revision: 2$ $Date: 03/04/2006 11:26:59$'

! Set first value to show that q/c bits precede all other values   !2.2

      EXPARR(0)=1
      DO I=1,LARRAY
        EXPARR(I)=-9999999.0
      ENDDO

! If the report is too long for the strings and arrays provided,
! truncate it to avoid abending.  But call TESSC5 first to get
! the identifier from the end before resetting the length!
! Keep a copy of ID in IDENT. This is because ENBUFR will convert ID
! from EBCDIC to ASCII and we need to pass an un-converted ID to AIRSTO.

      REPLEN=LEN(REPORT)                                           !2.1

      CALL TESSC5(REPORT,EXPARR,IDFLG,ID)                          !2.2
      IDENT=ID

      IF (REPLEN.GT.MAXREPL) THEN
        PRINT*,'TESAC for ',REPORT(REPLEN-5:REPLEN),' truncated'   !2.1
        PRINT*,'Length reset from',REPLEN,'to',MAXREPL
        PRINT*,REPORT(1:120)                                       !2.1
        REPLEN=MAXREPL
      ENDIF

! Put the report in characters (which may have been truncated above)
! at the start of the string to be stored.

      MESAGE(1:REPLEN)=REPORT

! Expand report section by section, then encode & store.
! (Sea depth can come from either section 2 or section 4, so is    !2.2
! passed from TESSC2 to be finally stored on the end by TESSC4,    !2.2
! together with recently added instrumentation details.)           !2.2

      POS=1
      CALL TESSC1(REPORT,POS,EXPARR,DATIME,IERR)

      IF (IERR.EQ.0) THEN
        CALL TESSC2(REPORT,POS,EXPARR,ARRPOS2,ENDVALS)             !2.2
        CALL TESSC3(REPORT,POS,EXPARR,ARRPOS2,ARRPOS3)             !2.2
        CALL TESSC4(REPORT,POS,EXPARR,ARRPOS3,ENDVALS)             !2.2
      ENDIF

! If the expansion worked, first make an index entry

      IF (IERR.EQ.0) THEN
        CALL TESIND(EXPARR,IDFLG,IDENT,OCOR,ENTRY)                 !2.3

! Get current time for ENBUFR to set as time of receipt

        CALL DATIM(NOW)
        DO I=0,4
          TOR(I+1)=NOW(8-I)
        ENDDO

! Encode BUFR message

        DESCR(1)=IDES(331195)                                      !2.2
        NDESCR=1
        NOBS=1
        CMPRES=.FALSE.
        CALL ENBUFR(DESCR,EXPARR,NDESCR,LARRAY,NOBS,ID,TOR,
     &              MESAGE(REPLEN+1:),CMPRES,MESLEN)

! Find number corresponding to CCCC in code table 001031.
! Put this number & data type into section 1 of BUFR message.
! Data type is 31 for oceanographic data

        IF(CCCC.NE.' ') THEN
          CALL CCCODE(287,ICCCC,CCCC)
          MESAGE(REPLEN+9:REPLEN+9)=CHAR(ICCCC/256)
          MESAGE(REPLEN+10:REPLEN+10)=CHAR(MOD(ICCCC,256))
        ENDIF

        MESAGE(REPLEN+13:REPLEN+13)=CHAR(31)

!-----------------------------------------------------------------------
! Set bulletin details to go in trailer and call AIRSTO
!-----------------------------------------------------------------------

        ENTRY(3:6)=TTAAII(1:4)                                      !2.3
        ENTRY(7:7)=CHAR(0)                                          !2.3
        IF (OCOR) ENTRY(7:7)=CHAR(1)                                !2.3
        ENTRY(8:11)=CCCC                                            !2.3

        CALL AIRSTO(DATIME,ENTRY,MESAGE(:REPLEN+MESLEN),            !2.3
     &              NFT,BLKSIZ,IDENT,TOR)                           !2.3
      ENDIF

      RETURN
      END
