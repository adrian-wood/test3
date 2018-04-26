      SUBROUTINE UPRPARTS(KeptPart,PosnPart,NumParts,Trailor,Message,
     &                    Diagnostics,CharIndex,Istat,WantTemps,
     &                    WantWinds,NumTempLevels,NumWindLevels,
     &                    Latitude,Longitude,Hour,Minute,Day,Month,
     &                    Year,ReportText,FinalProfile,MaxFinalElms,
     &                    ArrElmNum,ArrSegNum,ArrSubNum,ArrNRows,
     &                    ArrStyp,ArrSegst,ArrSegLen,ArrNsegs,
     &                    Isect1,CallSign,                          !2.0
     &                    NewMdbCall,CCCC,TTAA,Displ,Width,       !G!F!C
     &                    Scale,RefVal,Found)                     !I!G!C

!-----------------------------------------------------------------------
!
! subroutine    : UPRPARTS
!
!               : ANSI standard except for IMPLICIT NONE and '!' used
!               : for comments
!
! purpose       : MetDB retrieval, called by upper-air retrieval module.
!               : Used to extract parts from index entry using fast-
!               : retrieval and put them into an array for user-
!               : retrieval from.
!
! description   : array format:
!
!      1,  2  QC, WMO_BLCK_NMBR
!      3,  4  QC, WMO_STTN_NMBR
!      5,  6  QC, CALL_SIGN
!      7,  8  QC, LTTD
!      9, 10  QC, LNGD
!     11, 12  QC, STTN_HGHT
!     13, 14  QC, PESR_SNSR_HGHT
!     15, 16  QC, YEAR
!     17, 18  QC, MNTH
!     19, 20  QC, DAY
!     21, 22  QC, HOUR
!     23, 24  QC, MINT
!     25, 26  QC, MXMM_WIND_LEVL_WIND_SHER_1
!     27, 28  QC, MXMM_WIND_LEVL_WIND_SHER_2
!     29, 30  QC, RADI_SNDE_TYPE
!     31, 32  QC, TRCKG_SYTM
!     33, 34  QC, RADTN_CORTN
!     35, 36  QC, SEA_SRFC_TMPR
!     37, 38  QC, CLOD_RPLTN_CONT
!
!     DO I=1,4
!
!       39+(I-1)*6, 40+(I-1)*6  QC, CLOD_TYPE
!       41+(I-1)*6, 42+(I-1)*6  QC, CLOD_AMNT
!       43+(I-1)*6, 44+(I-1)*6  QC, CLOD_BASE_HGHT
!
!     ENDDO
!
!     63, 64  QC, LEVL_RPLTN_CONT
!
!     DO I=1,200
!
!       65+(I-1)*14, 66+(I-1)*14  QC, LEVL_IDNY
!       67+(I-1)*14, 68+(I-1)*14  QC, LEVL_PESR
!       69+(I-1)*14, 70+(I-1)*14  QC, LEVL_HGHT
!       71+(I-1)*14, 72+(I-1)*14  QC, LEVL_TMPR
!       73+(I-1)*14, 74+(I-1)*14  QC, LEVL_DEW_PONT_TMPR
!       75+(I-1)*14, 76+(I-1)*14  QC, LEVL_WIND_DRTCN
!       77+(I-1)*14, 78+(I-1)*14  QC, LEVL_WIND_SPED
!
!     ENDDO
!
!     Only the elements extracted from BUFR section 3 go in
!     the array index. Other elements are assigned differ-
!     ently. These elements are:
!
!     RPRT_IDNY
!     RCPT_YEAR, RCPT_MNTH, RCPT_DAY, RCPT_HOUR, RCPT_MINT
!     RCPT_YEAR_PARTA, RCPT_MNTH_PARTA, RCPT_DAY_PARTA,            !1.11
!     RCPT_HOUR_PARTA, RCPT_MINT_PARTA                             !1.11
!     RCPT_YEAR_PARTB, RCPT_MNTH_PARTB, RCPT_DAY_PARTB,            !1.11
!     RCPT_HOUR_PARTB, RCPT_MINT_PARTB                             !1.11
!     RCPT_YEAR_PARTC, RCPT_MNTH_PARTC, RCPT_DAY_PARTC,            !1.11
!     RCPT_HOUR_PARTC, RCPT_MINT_PARTC                             !1.11
!     RCPT_YEAR_PARTD, RCPT_MNTH_PARTD, RCPT_DAY_PARTD,            !1.11
!     RCPT_HOUR_PARTD, RCPT_MINT_PARTD                             !1.11
!     PESR_SNSR_FLAG                                                  !D
!     LEVL_CDTN_CODE
!     RPRT_TEXT
!
! data type(s)  : TEMP, PILOT, DROPSOND
!
! called by     : UPRRET
!
! sub calls     : SETHED & many others
!
! arguments     :
!
! KeptPart(4)   : logical   (ip)  : which parts have been kept
! PosnPart(4)   : integer   (ip)  : position of part in chain
! NumParts      : integer   (ip)  : number of parts in the chain
! Trailor       : character (ip)  : chained index entry
! Message       : character (ip)  : chained message
! Diagnostics   : logical   (ip)  : TRUE if messages wanted
! CharIndex     : character (ip)  : element index record
! Istat         : integer   (iop) : MetDB retrieval status indicator
! WantTemps     : logical   (ip)  : TRUE if temp levels wanted
! WantWinds     : logical   (ip)  : TRUE if wind levels wanted
! NumTempLevels : integer   (ip)  : number of temp levels wanted
! NumWindLevels : integer   (ip)  : number of wind levels wanted
! Latitude      : real      (ip)  : latitude from index entry
! Longitude     : real      (ip)  : longitude from index entry
! Hour          : integer   (ip)  : hour of report
! Minute        : integer   (ip)  : minute of report
! Day           : integer   (ip)  : day of report
! Month         : integer   (ip)  : month of report
! Year          : integer   (ip)  : year of report
! ReportText    : character (ip)  : raw report text for user
! FinalProfile  : real      (op)  : final profile
! MaxFinalElms  : integer   (ip)  : max no. of elems in FinalProfile
! ArrElmNum     : integer   (op)  : ARRINDX elements array
! ArrSegNum     : integer   (op)  : ARRINDX segment number array
! ArrSubNum     : integer   (op)  : ARRINDX segment subscript array
! ArrNRows      : integer   (op)  : ARRINDX no. of elements
! ArrStype      : integer   (op)  : ARRINDX segment repl count array
! ArrSegst      : integer   (op)  : ARRINDX segment start array
! ArrSegLen     : integer   (op)  : ARRINDX segment length array
! ArrNsegs      : integer   (op)  : ARRINDX no. of segments
! MaxArrElms    : integer   (ip)  : Max no. of ARRINDX elements
! MaxArrSegs    : integer   (ip)  : Max no. of ARRINDX segments
! Isect1        : integer   (op)  : array of TOR e.t.c.
! CallSign      : character (op)  : ship call sign
! NewMdbCall    : logical   (iop) : TRUE if new MetDB call
! CCCC          : character (op)  : Collecting centre                 !F
! TTAA          : character (op)  : TTAA                              !F
! Displ         : integer   (ip)  : Array used in BITINDX             !G
! Width         : integer   (ip)  : Array used in BITINDX             !G
! Scale         : integer   (ip)  : Array used in BITINDX             !G
! RefVal        : integer   (ip)  : Array used in BITINDX             !G
! Found(*)      : logical   (ip)  : Array of MetDB keywords selected  !I
!
!Y2K  26.06.1997  UPRPARTS is Year 2000 compliant.
!Y2K                       Routine contains date management.
!
! revision info :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:50$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uprparts.f,v $
!
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:50    Sheila Needham  
! $
! Revision 2.2  2003/01/06 16:11:48  usmdb
! 20 Jan 2003     C Long
! 2.2 Unset any flag on minute before passing it to SETHED
!
! Revision 2.1  2001/05/31  13:23:40  13:23:40  usmdb
! Removed argument 1 from call to BITINDX as not used in
! BITINDX - S.Cox
!
! Revision 2.0  2001/01/08  11:59:23  11:59:23  usmdb (Generic MetDB account)
! Removed arguments 9 and 10 from call to UPAIND as
! they are unused. Removed EXTERNAL statements. Moved
! declaration of NUMDESC in routines StndElems & SgnfElems
! before any array declarations. Removed unused dummy
! arguments MAXARRSEGS and MAXARRELMS. Added copyright and
! tidied header - S.Cox
!
! Revision 1.13  2000/08/09  15:02:34  15:02:34  usmdb (Generic MDB account)
! 21 Aug 2000   C Long
! 1.13  Copy only whole date/times to final profile, not e.g hour & minute
!       from different parts.  (Piecemeal approach only worked while launch
!       time wasn't allowed to reset hour!)
!
! Revision 1.12  2000/07/10  11:20:02  11:20:02  usmdb (Generic MDB account)
! 17 July 2000     C Long
! 1.12  Sort (using SORTR, not SORTN) on level identity as well as
!       pressure or height, so that order of levels is completely
!       determined by sort (& hence same if retrieval repeated).
!
! Revision 1.11  2000/03/10  09:31:33  09:31:33  usmdb (Generic MDB account)
! Implemented 20-03-2000 - S.Cox
! Extract TOR for each part in a sonde ascent and
! make retrievable for user.
!
! Revision 1.10  98/11/12  11:45:58  11:45:58  usmdb (Generic MDB account)
! 16-11-98 S.Cox
! Change declaration of CharIndex - now an array of element indexes.
!
! Revision 1.9  98/07/23  08:42:53  08:42:53  usmdb (Generic MDB account)
! changes to allow retrieval of BUFR messages
!
! Revision 1.8  97/09/22  11:03:07  11:03:07  uspm (Pat McCormack)
! Pass 2 extra arguments to VALUSR.
!
! Revision 1.7  1997/08/04 13:37:40  uspm
! First revisioned version for COSMOS - with Y2K changes
!
! Revision 1.6  1997/07/25 14:17:18  uspm
! Latest version from COSMOS dated 21-7-97
!
! Revision 1.4  1997/04/04 13:12:12  uspm
! Version dated 03-04-97 from COSMOS
!
! Revision 1.3  1997/02/27 12:17:59  uspm
! Latest version from COSMOS
!
! Revision 1.2  1997/02/20 13:22:46  uspm
! Latest version from COSMOS
!
! Revision 1.1  1997/02/17 12:00:25  uspm
! Initial revision
!
! 20-07-98  !I  : Allow retrieval of BUFR messages. The BUFR message is
!               : returned in the raw report string. Also correct the
!               : length of the report text - S.Cox
!
! 29-09-97  !H  : Pass 2 extra arguments to VALUSR. These are not used
!               : in VALUSR for upper-air retrieval, but are needed to
!               : keep code consistent - S.Cox
!
! 21-07-97  !G  : Declare Displ, Width, Scale, RefVal arrays in
!               : UPRRET and pass to UPRPARTS. This is necessary for
!               : retrieval of merged data. Also change COMMON names
!               : to make them more unique - S.Cox
!
! 30-06-97  !F  : Extract TTAA from the trailor and pass this and CCCC
!               : back to UPRRET - S.Cox
!
! 03-04-97  !E  : Correct setting of TOR in ISect1 array - S.Cox
!
! 05-03-97  !D  : Add Pressure sensor flag to ISECT1(12) to indicate
!               : 1 if part B doesn't have a pressure sensor, or if
!               : there is no part B, indicate 1 if the part A doesn't
!               : have a pressure sensor. Change FinalProfile sort
!               : method. Check all levels for pressures and heights.
!               : If all levels, have pressure, sort on pressure, else
!               : sort on height. If after height sort, some levels
!               : have missing heights, put them at the end of the
!               : profile e.g. PILOT part A Max Winds - S.Cox
!
! 21-02-97  !C  : New argument NewMdbCall. This is needed by BITINDX
!               : to decide if a new call has been made to the MetDB
!               : with ISTAT=0. Also change checks for missing data
!               : in the profiles from .EQ. Imdi to .LT. Tmdi (missing
!               : data threshold value) - S.Cox
!
! 04-02-97  !B  : Pass Diagnostics argument to BITINDX - S.Cox
!
! 13-12-96  !A  : Add some dynamic common - S.Cox
!
! 09-09-96      : Written S.Cox
!
!-----------------------------------------------------------------------
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

      INTEGER   HeaderLen
      INTEGER   MaxPartElms
      INTEGER   SgnfElms
      INTEGER   StndElms

      REAL      Rmdi
      REAL      Tmdi                                                  !C

      PARAMETER (HeaderLen=44)       ! length of raw report header
      PARAMETER (MaxPartElms=4270)   ! max no of elements in PartProfile
                                     ! array 300*14=4200 level elements
                                     ! + 70 non-level elements
      PARAMETER (Rmdi=-9999999.0)    ! missing data indicator
      PARAMETER (Tmdi=-9000000.0)    ! missing data threshold         !C
      PARAMETER (SgnfElms=2135)      ! no. of significant elements
      PARAMETER (StndElms=160)       ! no. of standard elements

!-----------------------------------------------------------------------
! declare integers
!-----------------------------------------------------------------------

      INTEGER   ArrElmNum(*)       ! ARRINDX elements array
      INTEGER   ArrNrows           ! ARRINDX no. of elements
      INTEGER   ArrNsegs           ! ARRINDX no. of segments
      INTEGER   ArrSegLen(*)       ! ARRINDX segment length array
      INTEGER   ArrSegNum(*)       ! ARRINDX segment no. array
      INTEGER   ArrSegst(*)        ! ARRINDX segment start array
      INTEGER   ArrStyp(*)         ! ARRINDX segment repl count arr
      INTEGER   ArrSubNum(*)       ! ARRINDX segment subscript array

      INTEGER   BufrEnd            ! end of BUFR message in report    !I
      INTEGER   BufrStart          ! start of BUFR message in report
      INTEGER   CurrentTOR         ! TOR of current part
      INTEGER   Day                ! day

      INTEGER   DescSgnf(SgnfElms)
      INTEGER   DescStnd(StndElms)
      INTEGER   Displ(*)                                              !G
      INTEGER   DummyInt1(1)       ! dummy array for VALUSR           !H

      INTEGER   ElemPointer        ! pointer to position in FinalProfile
      INTEGER   F
      INTEGER   FilledUserLevels   ! filled no. of user levels requested
      INTEGER   HghtSortMask(14)   ! SORTR mask for heights       !1.12
      INTEGER   Hour               ! hour
      INTEGER   I                  ! general loop counter
      INTEGER   Ifail              ! condition on return from BITINDX
      INTEGER   ILocalD            ! integer Local D descriptor
      INTEGER   Isect1(*)          ! array of TOR e.t.c.
      INTEGER   Istat              ! MetDB retrieval status condition
      INTEGER   J                  ! general loop counter
      INTEGER   K                  ! general loop counter
      INTEGER   LenReportText      ! length of users report text string
      INTEGER   LevelType          ! type of level
      INTEGER   MaxFinalElms       ! max number of elems in FinalProfile
      INTEGER   Minute             ! minute
      INTEGER   MissHght           ! missing height levels counter
      INTEGER   Month              ! month
      INTEGER   NAC
      INTEGER   NAMD
      INTEGER   NCOR
      INTEGER   Nelreq           ! number of elements required
      INTEGER   NumParts         ! number of parts in the chain
      INTEGER   NumTempLevels    ! number of temp levels wanted
      INTEGER   NumWindLevels    ! number of wind levels wanted
      INTEGER   PartLevels       ! no. of levels in the part
      INTEGER   PartMap(4)       ! maps A,B,C,D to A,C,B,D
      INTEGER   PartReportLen    ! length of report text for a part
      INTEGER   PosnPart(4)      ! position of part in chain
      INTEGER   RawReportLen     ! length of raw report text
      INTEGER   RawReportPointer ! length of raw report text
      INTEGER   RcptHour
      INTEGER   RcptMinute

      INTEGER   RefVal(*)                                             !G
      INTEGER   ReplSgnf(SgnfElms)
      INTEGER   ReplStnd(StndElms)
      INTEGER   Scale(*)                                              !G
      INTEGER   StoredTOR            ! highest TOR of the 4 parts

      INTEGER   PresSortMask(14) ! SORTR mask for pressures       !1.12
      INTEGER   TotalPartLevels  ! length of total raw report
      INTEGER   TotalUserLevels  ! total no. of user levels requested
      INTEGER   TrByte17         ! trailor byte 17                    !D

      INTEGER   Width(*)                                              !G

      INTEGER   X
      INTEGER   Y
      INTEGER   Year             ! year

!-----------------------------------------------------------------------
! declare reals (alphabetical order)
!-----------------------------------------------------------------------

      REAL      Latitude                     ! latitude
      REAL      Longitude                    ! longitude
      REAL      FinalProfile(*)              ! final profile
      REAL      PartProfile(1,MaxPartElms)   ! initial profile

!-----------------------------------------------------------------------
! declare logicals (alphabetical order)
!-----------------------------------------------------------------------

      LOGICAL   AllPres          ! TRUE if all levels have pressures  !D
      LOGICAL   AllHght          ! TRUE if all levels have pressures  !D
      LOGICAL   Diagnostics      ! TRUE if output wanted
      LOGICAL   FirstCall        ! TRUE if first call to subroutine
      LOGICAL   Found(*)         ! Array of MetDB keywords selected   !I
      LOGICAL   KeptPart(4)      ! which parts are in the chain
      LOGICAL   NewMdbCall       ! TRUE if new MetDB call             !C
      LOGICAL   TempLevel        ! TRUE if level is a temp level
      LOGICAL   WantTemps        ! TRUE if user wants temperature levels
      LOGICAL   WantWinds        ! TRUE if user wants wind levels
      LOGICAL   WindLevel        ! TRUE if level is a wind level

!-----------------------------------------------------------------------
! declare characters (alphabetical order)
!-----------------------------------------------------------------------

      CHARACTER*4     BUFR
      CHARACTER*(*)   CallSign(1)          ! Ship Call Sign
      CHARACTER*4     CCCC                 ! collecting centre
      CHARACTER*(*)   CharIndex(*)                                 !1.10
      CHARACTER*132   HEAD                 ! revision information

      CHARACTER*(*)   ReportText

      CHARACTER*1     DummyChr1            ! dummy string for valusr
      CHARACTER*1     DummyChr2(1)         ! dummy string for valusr

      CHARACTER*6     LocalD               ! local sequence descriptor

      CHARACTER*(HeaderLen)  PartHeader    ! header for each part

      CHARACTER*4     SEVENS               ! '7777' end of BUFR msg   !I
      CHARACTER*(*)   Trailor(1:NumParts)
      CHARACTER*(*)   TTAA                                            !F
      CHARACTER*(*)   Message(1:NumParts)

!-----------------------------------------------------------------------
! declare functions
!-----------------------------------------------------------------------

      INTEGER         DT2HRS             ! declare function

!-----------------------------------------------------------------------
! dynamic common, compile with FPARMS='DC(*)' on IBM mainframe
!-----------------------------------------------------------------------

      COMMON /UPRPAR1/DescSgnf,DescStnd,ReplSgnf                    !G!A
      COMMON /UPRPAR2/ReplStnd,PartProfile                          !G!A

!-----------------------------------------------------------------------
! data statements
!-----------------------------------------------------------------------

      DATA      BUFR           /'BUFR'/
      DATA      FirstCall      /.TRUE./
      DATA      HghtSortMask   /0,-2,0,0,0,1,0,0,0,0,0,0,0,0/     !1.12
      DATA      PartMap        /1,3,2,4/
      DATA      PresSortMask   /0,-2,0,-1,0,0,0,0,0,0,0,0,0,0/    !1.12
      DATA      SEVENS         /'7777'/                               !I

!-----------------------------------------------------------------------
! need a SAVE statement for portability.
!-----------------------------------------------------------------------

      SAVE

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/uprparts.f,v $
     &'//'$ $Revision: 1$ $Date: 30/01/2006 20:25:50$ '

!-----------------------------------------------------------------------
! Initialise TOR in ISect1 for each part
!-----------------------------------------------------------------------

      DO I=13,32                                                   !1.11
        Isect1(I)=-9999999                                         !1.11
      ENDDO                                                        !1.11

!-----------------------------------------------------------------------
! If first call to subroutine, get the list of element indexes for
! standard and then significant parts. Call UPAIND to initialise the
! element index for ARRINDX to put elements into the user's array.
! Convert EBCDIC BUFR to ASCII on first call to subroutine only.
!-----------------------------------------------------------------------

      If (FirstCall) THEN
        CALL StndElems(DescStnd,StndElms,ReplStnd)
        CALL SgnfElems(DescSgnf,SgnfElms,ReplSgnf)

        CALL UPAIND(ArrElmNum,ArrSegNum,ArrSubNum,ArrNrows,ArrStyp,
     &  ArrSegst,ArrSegLen,ArrNsegs)                                !2.0

        CALL EB2ASC(4,BUFR)
        CALL EB2ASC(4,SEVENS)                                         !I
        PartHeader='0000Z DD/MM/YY LA.TTN LON.TTW CCCC TORM A C '
        FirstCall=.FALSE.
      ENDIF

      IF (Diagnostics) THEN
        write(*,*)'IN UPRPARTS: KeptPart = ',KeptPart
        write(*,*)'IN UPRPARTS: PosnPart = ',PosnPart
        write(*,*)'IN UPRPARTS: NumParts = ',NumParts
      ENDIF

!-----------------------------------------------------------------------
! Set the number of total number of user levels. Note: if the user
! wants both temperature and wind levels, NumTempLevels and
! NumWindLevels are the same. This condition is ensured in MDB.
!-----------------------------------------------------------------------

      IF (WantTemps) TotalUserLevels=NumTempLevels
      IF (WantWinds) TotalUserLevels=NumWindLevels

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

      FilledUserLevels  = 0
      TotalPartLevels   = 0
      ElemPointer       = 0
      StoredTOR         = 0
      RawReportPointer  = 6        !- start past report text length   !I
      ReportText(:)     = ' '
      CCCC(:)           = ' '                                         !F
      TTAA(:)           = ' '                                         !F

      LenReportText     = LEN(ReportText)

      DO I=1,MaxFinalElms
        FinalProfile(I)=Rmdi
      ENDDO

!-----------------------------------------------------------------------
! Loop through the parts in the order A,C,B,D using PartMap. Check that
! the part is in the chain and that there is room left in the users
! array.
!-----------------------------------------------------------------------

      DO I=1,4

        IF (KeptPart(PartMap(I))) THEN

!-----------------------------------------------------------------------
! Initialise the PartProfile array to missing. Find the length of the
! raw report text and the start position of the BUFR message. Extract
! the local BUFR sequence descriptor from the message for use by
! BITINDX.
!-----------------------------------------------------------------------

          DO J=1,MaxPartElms
            PartProfile(1,J)=Rmdi
          ENDDO

          RawReportLen=INDEX(Message(PosnPart(PartMap(I))),BUFR)-1
          BufrStart=RawReportLen+1
          BufrEnd=INDEX(Message(PosnPart(PartMap(I))),SEVENS)+3       !I

          ILocalD=ICHAR(Message(PosnPart(PartMap(I)))(BufrStart+29:
     &            BufrStart+29))*256+
     &            ICHAR(Message(PosnPart(PartMap(I)))(BufrStart+30:
     &            BufrStart+30))

          CALL DESFXY(ILocalD,F,X,Y)
          WRITE(LocalD,'(I1,I2.2,I3.3)')F,X,Y

!-----------------------------------------------------------------------
! If the part is A or C (I.LE.2) call BITINDX with the standard element
! list, else call it with the significant element list.
!-----------------------------------------------------------------------

          IF (I.LE.2) THEN
            CALL BITINDX(CharIndex,DescStnd,StndElms,               !2.1
     &      ReplStnd,.TRUE.,Ifail,Diagnostics,LocalD,                 !B
     &      Message(PosnPart(PartMap(I)))(BufrStart:),
     &      Nelreq,Displ,Width,Scale,RefVal,MaxPartElms,NewMdbCall)   !C
          ELSE
            CALL BITINDX(CharIndex,DescSgnf,SgnfElms,               !2.1
     &      ReplSgnf,.TRUE.,Ifail,Diagnostics,LocalD,                 !B
     &      Message(PosnPart(PartMap(I)))(BufrStart:),
     &      Nelreq,Displ,Width,Scale,RefVal,MaxPartElms,NewMdbCall)   !C
          ENDIF

          IF (Ifail.eq.16) THEN
            Istat=16
            IF (Diagnostics) THEN
              write(*,*)'IN UPRPARTS: Element Index not found'
            ENDIF
            GOTO 999
          ENDIF

!-----------------------------------------------------------------------
! Call VALUSR to put the details into the array PartProfile.
!-----------------------------------------------------------------------

          CALL VALUSR(Message(PosnPart(PartMap(I)))(BufrStart:),
     &    Nelreq,Displ,Width,Scale,RefVal,PartProfile,1,1,CallSign,
     &    DummyChr1,DummyChr2,Diagnostics,Isect1,DummyChr2,DummyInt1) !H

          IF (Diagnostics) THEN
            write(*,'(/1X,''In UPRPARTS: Back from valusr''/)')
            DO J=1,1000
              write(*,*)'PartProfile = ',J,PartProfile(1,J)
            ENDDO
          ENDIF

!-----------------------------------------------------------------------
! Put report text into report text string.
!-----------------------------------------------------------------------

          CCCC       = Trailor(PosnPart(PartMap(I)))(8:11)
          NAC        = ICHAR(Trailor(PosnPart(PartMap(I)))(7:7))
          NAMD       = NAC/16
          NCOR       = MOD(NAC,15)

          IF (TTAA(:).EQ.' ') THEN                                    !F
            TTAA = Trailor(PosnPart(PartMap(I)))(3:6)                 !F
          ENDIF                                                       !F

          RcptHour   = ICHAR(Message(PosnPart(PartMap(I)))
     &                 (BufrStart+19:BufrStart+19))
          RcptMinute = ICHAR(Message(PosnPart(PartMap(I)))
     &                 (BufrStart+20:BufrStart+20))

! Minute from index can have power of 2 (64 or 128) added as flag. !2.2
! (UPRRET has zeroed missing minute)  Unset flag to call SETHED.   !2.2

          CALL SETHED(PartHeader,Latitude,Longitude,               !2.2
     &                Hour,MOD(Minute,64),Day,                     !2.2
     &                Month,Year,RcptHour,RcptMinute,NAMD,NCOR,CCCC)

          IF (FOUND(34)) THEN                                         !I
            PartReportLen=BufrEnd-BufrStart+1                         !I
          ELSE                                                        !I
            PartReportLen=RawReportLen+HeaderLen                      !I
          ENDIF                                                       !I

          IF ((RawReportPointer+PartReportLen-1).LE.LenReportText) THEN

            IF (FOUND(34)) THEN                                       !I
              ReportText(RawReportPointer:RawReportPointer+           !I
     &        PartReportLen-1)=Message(PosnPart(PartMap(I)))          !I
     &        (BufrStart:BufrEnd)                                     !I
            ELSE                                                      !I
              ReportText(RawReportPointer:RawReportPointer+43)=       !I
     &        PartHeader                                              !I
              ReportText(RawReportPointer+44:RawReportPointer+44+     !I
     &        PartReportLen-1)=Message(PosnPart(PartMap(I)))          !I
     &        (1:RawReportLen)                                        !I
            ENDIF                                                     !I

            IF (Diagnostics) THEN
              write(*,*)'In UPRPARTS: ReportText=',ReportText
            ENDIF

!-----------------------------------------------------------------------
! Final length of the report text minus the length (held in ReportText
! (1:5). Only ReportText(6:) are transferred to user by VALARR in
! UPRRET.                                                             !I
!-----------------------------------------------------------------------

            WRITE(ReportText(1:5),'(I5)')(RawReportPointer+           !I
     &                                    PartReportLen-6)            !I

          ENDIF

          IF (FOUND(34)) THEN                                         !I
            RawReportPointer=RawReportPointer+PartReportLen           !I
          ELSE                                                        !I
            RawReportPointer=RawReportPointer+PartReportLen+1         !I
          ENDIF                                                       !I

!-----------------------------------------------------------------------
! Handle the TOR for each part.
!-----------------------------------------------------------------------

          IF (PartProfile(1,26).GT.Tmdi .AND.                       !E!C
     &        PartProfile(1,28).GT.Tmdi .AND.                       !E!C
     &        PartProfile(1,30).GT.Tmdi .AND.                       !E!C
     &        PartProfile(1,32).GT.Tmdi) THEN                       !E!C

!-----------------------------------------------------------------------
! Put the TOR for each part into the Isect1 array.                 !1.11
!-----------------------------------------------------------------------

            IF (I.EQ.1) J=13      !- part A                        !1.11
            IF (I.EQ.2) J=23      !- part C                        !1.11
            IF (I.EQ.3) J=18      !- part B                        !1.11
            IF (I.EQ.4) J=28      !- part D                        !1.11

            Isect1(J)=NINT(PartProfile(1,26))                      !1.11
            Isect1(J+1)=NINT(PartProfile(1,28))                    !1.11
            Isect1(J+2)=NINT(PartProfile(1,30))                    !1.11
            Isect1(J+3)=NINT(PartProfile(1,32))                    !1.11
            Isect1(J+4)=NINT(PartProfile(1,34))                    !1.11

!-----------------------------------------------------------------------
! Put the latest TOR into the Isect1 array as well.
!
! Convert the TOR YYYY,MM,DD,HH to century hours & add minutes. If
! later than stored TOR, put into Isect1 array
!-----------------------------------------------------------------------

            CurrentTOR=DT2HRS(NINT(PartProfile(1,26)),
     &                        NINT(PartProfile(1,28)),
     &                        NINT(PartProfile(1,30)),
     &                        NINT(PartProfile(1,32)))

            CurrentTOR=(60*CurrentTOR)+NINT(PartProfile(1,34))

            IF (CurrentTOR.GT.StoredTOR) THEN
              StoredTOR=CurrentTOR
              Isect1(1)=NINT(PartProfile(1,26))
              Isect1(2)=NINT(PartProfile(1,28))
              Isect1(3)=NINT(PartProfile(1,30))
              Isect1(4)=NINT(PartProfile(1,32))
              Isect1(5)=NINT(PartProfile(1,34))
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
! If the part is a part A or B, look at the trailor bit of byte 17. If
! it is 0, the part has a pressure sensor. If it is 1 it doesn't. Set
! Isect1(12) to the value of byte 17, bit 5.
!-----------------------------------------------------------------------

          IF (I.EQ.1 .OR. I.EQ.3) THEN                                !D
            TrByte17 = ICHAR(Trailor(PosnPart(PartMap(I)))(17:17))    !D
            Isect1(12) = MOD(TrByte17/8,2)                            !D
          ENDIF                                                       !D

!=======================================================================
! Put non-level elements into FinalProfile array. Parts of this process
! will be different for standard (A&C) and significant (B&D) parts as
! standard parts have information significant parts don't and vice versa
!=======================================================================

!-----------------------------------------------------------------------
! Put WMO_BLCK_NMBR, WMO_STTN_NMBR, CALL_SIGN, LTTD, LNGD, STTN_HGHT,
! PESR_HGHT + associated QC bits into the  !1.13
! FinalProfile if they are not already there. This is the same for both
! standard and significant parts.
!-----------------------------------------------------------------------

          DO J=1,14                                               !1.13
            IF (FinalProfile(J).LT.Tmdi) THEN                         !C
              FinalProfile(J)=PartProfile(1,J)
            ENDIF
          ENDDO

!-----------------------------------------------------------------------
! For date/time (year, month, day, hour, minute) take all five    !1.13
! elements from the same part.  Launch time can reset the hour &  !1.13
! hence possible the date, so don't set year/month/day/hour &     !1.13
! then minute from a different part!                              !1.13
! So code below sets date/time from first part but then resets it !1.13
! if part has minute set & final profile not.                     !1.13
!-----------------------------------------------------------------------

          IF (FinalProfile(16).LT.Tmdi .OR.                       !1.13
     &      ((FinalProfile(24).LT.Tmdi .AND.                      !1.13
     &         PartProfile(1,24).GT.Tmdi))) THEN                  !1.13
            DO J=15,24                                            !1.13
              FinalProfile(J)=PartProfile(1,J)                    !1.13
            ENDDO                                                 !1.13
          ENDIF                                                   !1.13

!-----------------------------------------------------------------------
! for standard parts (A&C) only !! put MXMM_WIND_LEVL_WIND_SHER_1 and
! MXMM_WIND_LEVL_WIND_SHER_2 + associated QC bits into the FinalProfile
! if they are not already there.
!-----------------------------------------------------------------------

          IF (I.LE.2) THEN         !- Parts A&C only
            DO J=25,28
              IF (FinalProfile(J).LT.Tmdi) THEN                       !C
                FinalProfile(J)=PartProfile(1,J+10)
              ENDIF
            ENDDO

!-----------------------------------------------------------------------
! for significant parts (B&D) only !! put RADI_SNDE_TYPE, TRCKG_SYTM,
! RADTN_CORTN, SEA_SRFC_TMPR, CLOD_RPLTN_CONT, CLOD_TYPE, CLOD_AMNT,
! CLOD_BASE_HGHT + associated QC bits into the FinalProfile if they are
! are not already there.
!-----------------------------------------------------------------------

          ELSE                     !- Parts B&D only
            DO J=29,38
              IF (FinalProfile(J).LT.Tmdi) THEN                       !C
                FinalProfile(J)=PartProfile(1,J+6)
              ENDIF
            ENDDO   !- j loop

            DO J=1,4                           !- 4 cloud groups
              DO K=39+(J-1)*6,44+(J-1)*6
                IF (FinalProfile(K).LT.Tmdi) THEN                     !C
                  FinalProfile(K)=PartProfile(1,K+6)
                ENDIF
              ENDDO   !- k loop
            ENDDO   !- j loop
          ENDIF   !- i loop

!=======================================================================
! Now we need to put the level data into the FinalProfile. Make a note
! of the number of levels in the part. The location in the PartProfile
! array is different for standard and significant parts.
!=======================================================================

          IF (I.LE.2) PartLevels=NINT(PartProfile(1,40))  ! Parts A&C
          IF (I.GE.3) PartLevels=NINT(PartProfile(1,70))  ! Parts B&D

          IF (Diagnostics) THEN
            write(*,*)'In UPRPARTS: PartLevels      = ',PartLevels
            write(*,*)'In UPRPARTS: TotalUserLevels = ',TotalUserLevels
          ENDIF

!-----------------------------------------------------------------------
! User wants both wind and temperature levels. J is the loop counter,
! TotalUserLevels is the number of levels the user has requested,
! FilledUserLevels is the number of user levels we have filled so far.
! Keep looping over levels until the number of levels transferred
! has reached the number of levels requested by the user, or we have
! transfered all the levels for the part. For each level, there are 14
! elements. Loop over these, transferring them to the FinalProfile
! array.
!-----------------------------------------------------------------------

          IF (PartLevels.GT.0) THEN

            IF (WantTemps .AND. WantWinds) THEN

              TotalPartLevels=TotalPartLevels+PartLevels
              J=1
              DO WHILE (FilledUserLevels.LT.TotalUserLevels .AND.
     &        J.LE.PartLevels)

                DO K=1,14                             !- 14 elements
                  ElemPointer=ElemPointer+1
                  IF (I.LE.2) THEN                    !- Parts A&C
                    FinalProfile(64+ElemPointer)=
     &              PartProfile(1,40+K+(J-1)*14)
                  ELSE                                !- Parts B&D
                    FinalProfile(64+ElemPointer)=
     &              PartProfile(1,70+K+(J-1)*14)
                  ENDIF   !- i.le.2
                ENDDO   !- k loop

                J=J+1
                FilledUserLevels=FilledUserLevels+1
              ENDDO   !- do while levels to transfer

              IF (Diagnostics) THEN
               write(*,*)'In UPAPARTS: FilledUserLevs=',FilledUserLevels
              ENDIF

!-----------------------------------------------------------------------
! User wants just wind levels or just temperature levels. Again loop
! over levels until there are no more to read, or the user's max has
! been reached. For each level, work out what type of level it is.
! (LevelType). If we want to transfer this level, do, otherwise don't.
!-----------------------------------------------------------------------

            ELSEIF ((WantTemps .AND. (.NOT.WantWinds)) .OR.
     &              (WantWinds .AND. (.NOT.WantTemps))) THEN

              DO J=1,PartLevels

                IF (I.LE.2) THEN                        !- Parts A&C
                  LevelType=NINT(PartProfile(1,42+(J-1)*14))
                ELSE                                    !- Parts B&D
                  LevelType=NINT(PartProfile(1,72+(J-1)*14))
                ENDIF   !- i.le.2

                WindLevel=((MOD(LevelType/64,2).EQ.1) .OR.   !- surf
     &                     (MOD(LevelType/32,2).EQ.1) .OR.   !- stnd
     &                     (MOD(LevelType/16,2).EQ.1) .OR.   !- trop
     &                     (MOD(LevelType/8,2).EQ.1)  .OR.   !- max wind
     &                     (MOD(LevelType/2,2).EQ.1))        !- sig wind

                TempLevel=((MOD(LevelType/64,2).EQ.1) .OR.   !- surf
     &                     (MOD(LevelType/32,2).EQ.1) .OR.   !- stnd
     &                     (MOD(LevelType/16,2).EQ.1) .OR.   !- trop
     &                     (MOD(LevelType/4,2).EQ.1))        !- sig temp

                IF (Diagnostics) THEN
                  write(*,*)'In UPRPARTS: J,WindLevel,TempLevel=',
     &            J,WindLevel,TempLevel
                ENDIF

                IF ((WantWinds .AND. WindLevel) .OR. (WantTemps .AND.
     &          TempLevel)) THEN

                  TotalPartLevels=TotalPartLevels+1
                  IF (FilledUserLevels.LT.TotalUserLevels) THEN

                    DO K=1,14                             !- 14 elements
                      ElemPointer=ElemPointer+1
                      IF (I.LE.2) THEN                    !- Parts A&C
                        FinalProfile(64+ElemPointer)=
     &                  PartProfile(1,40+K+(J-1)*14)
                      ELSE                                !- Parts B&D
                        FinalProfile(64+ElemPointer)=
     &                  PartProfile(1,70+K+(J-1)*14)
                      ENDIF   !- i.le.2
                    ENDDO   !- k loop

                    FilledUserLevels=FilledUserLevels+1

                  ENDIF  !- FilledUserLevels.lt.TotalUserLevel
                ENDIF   !- want levels block
              ENDDO   !- do while levels to transfer

              IF (Diagnostics) THEN
               write(*,*)'In UPRPARTS: FilledUserLevs=',FilledUserLevels
               write(*,*)'In UPRPARTS: TotalPartLevs =',TotalPartLevels
              ENDIF

!-----------------------------------------------------------------------
! User doesn't want any level data - so don't transfer any to
! FinalProfile
!-----------------------------------------------------------------------

            ELSE
              IF (Diagnostics) THEN
                write(*,*)'In UPRPARTS: No level data requested'
              ENDIF
            ENDIF   !- WantTemps/WantWinds if block

          ENDIF   !- PartLevels.gt.0 check

        ENDIF   !- KeptPart if block

        IF (Diagnostics) THEN
          write(*,'(/1X,''In UPRPARTS: FinalProfile''/)')
          DO J=1,1000
            write(*,*)'FinalProfile=',J,FinalProfile(J)
          ENDDO
        ENDIF

!-----------------------------------------------------------------------
! Check all the levels in the final profile for any missing pressures
! and heights. These are used later in the profile sorting.           !D
!-----------------------------------------------------------------------

        AllPres = .TRUE.
        AllHght = .TRUE.

        IF (FilledUserLevels.GT.0) THEN
          DO J=1,FilledUserLevels
            IF (FinalProfile(68+(J-1)*14).LE.Tmdi) AllPres = .FALSE.
            IF (FinalProfile(70+(J-1)*14).LE.Tmdi) AllHght = .FALSE.
          ENDDO
        ENDIF

      ENDDO   !- i loop over parts A,C,B,D

!-----------------------------------------------------------------------
! Put total number of levels into the FinalProfile array, and set the
! condition code to 0 or 8 depending on whether the user has allowed
! enough replication counts for them.
!-----------------------------------------------------------------------

      IF (FilledUserLevels.GT.0) FinalProfile(64)=REAL(TotalPartLevels)

      IF (TotalPartLevels.LE.FilledUserLevels) THEN
        Isect1(11)=0
      ELSE
        Isect1(11)=8
      ENDIF

      IF (Diagnostics) THEN
        write(*,                                                  !1.12
     &   '(/1X,''In UPRPARTS: Final profile before SORTR''/)')   !1.12
        DO J=1,1000
          write(*,*)'J,FinalProfile=',J,FinalProfile(J)
        ENDDO
      ENDIF

!-----------------------------------------------------------------------
! Now sort the levels into order. If all levels have a pressure, sort
! on pressures. Pass the array elements from subscript 65 onwards to
! the sort program as we only want to sort the levels. The levels are
! sorted into decreasing pressure order.                          !1.12
!-----------------------------------------------------------------------

      IF (AllPres) THEN

        CALL SORTR(FinalProfile(65),14,FilledUserLevels,          !1.12
     &             PresSortMask)

        IF (Diagnostics) THEN
          write(*,                                                !1.12
     &     '(/1X,''UPRPARTS: Profile after sort on pressure''/)') !1.12
          DO J=1,1000
            write(*,*)'J,FinalProfile=',J,FinalProfile(J)
          ENDDO
        ENDIF

!-----------------------------------------------------------------------
! If not all of the levels have a pressure, sort them on heights, !1.12
! in ascending order.                                             !1.12
!-----------------------------------------------------------------------

      ELSE    !- no pressures available, sort on heights

        CALL SORTR(FinalProfile(65),14,FilledUserLevels,          !1.12
     &             HghtSortMask)                                  !1.12

        IF (Diagnostics) THEN
          write(*,                                                !1.12
     &     '(/1X,''UPRPARTS: profile after SORTR on height''/)')  !1.12
          DO J=1,1000
            write(*,*)'J,FinalProfile=',J,FinalProfile(J)
          ENDDO
        ENDIF

!-----------------------------------------------------------------------
! We may have sorted the levels by height, but it is still possible for
! there to be missing height levels. This is usually because a Max wind
! has been reported in a PILOT part A. In this case, we want to put
! the missing height levels at the end of the profile. Loop over the
! levels, replacing the missing heights with a large positive height,
! resort the levels on height and change the large +ve heights back
! to missing data indicators.                                         !D
!-----------------------------------------------------------------------

        IF (.NOT.AllHght) THEN
          MissHght=0
          J=0
          DO WHILE (J.LT.FilledUserLevels)
            J=J+1
            IF (FinalProfile(70+(J-1)*14).LE.Tmdi) THEN
              FinalProfile(70+(J-1)*14)=9000000.0
              MissHght=MissHght+1
            ENDIF
          ENDDO

          CALL SORTR(FinalProfile(65),14,FilledUserLevels,        !1.12
     &               HghtSortMask)                                !1.12

          DO J=1,MissHght
            FinalProfile(70+(FilledUserLevels-J)*14)=Rmdi
          ENDDO
        ENDIF

      ENDIF  !- sort on pressures or heights.

999   CONTINUE

      RETURN
      END

!***********************************************************************
!**   Subroutine StndElems
!***********************************************************************

      SUBROUTINE StndElems(Desc,NumDesc,Repl)

!-----------------------------------------------------------------------
!
! subroutine    : StndElems
!
!               : ANSI standard except for IMPLICIT NONE and '!' used
!               : for comments
!
! purpose       : MetDB Upper-Air retrieval. Used to set up the
!               : user selection element pointers for standard parts
!               : (A&C). These are passed to BITINDX for fast-retrieval
!               : of the data.
!
!               : Basically, the element pointer is the Data Dictionary
!               : element number for the element name wanted. Where
!               : there are replications e.g. levels, the element
!               : pointer is the same, but the replication count will
!               : increment.
!
! data type(s)  : TEMP, PILOT, DROPSOND
!
! called by     : UPRPARTS
!
! sub calls     : None
!
! arguments     :
!
! Desc(NumDesc) : integer   (op) : array of pointers
! NumDesc       : integer   (ip) : Number of pointers
! Repl(NumDesc) : integer   (op) : array of replications
!
! change record :
!
! 16-09-96      : Written S.Cox
!
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER  I,J
      INTEGER  Counter
      INTEGER  NumDesc
      INTEGER  Desc(NumDesc)
      INTEGER  Repl(NumDesc)

      SAVE

!-----------------------------------------------------------------------
! WMO_BLCK_NMBR, WMO_STTN_NMBR, CALL_SIGN, LTTD, LNGD, STTN_HGHT,
! PESR_HGHT, YEAR, MNTH, DAY, HOUR, MINT, RCPT_YEAR, RCPT_MNTH,
! RCPT_DAY, RCPT_HOUR, RCPT_MINT
!-----------------------------------------------------------------------

      DO I=1,17             !- our elements 1-17.
        Desc(I)=I+1         !- elements 2-18 in Data Dictionary.
        Repl(I)=1           !- only 1 replication of each element.
      ENDDO

!-----------------------------------------------------------------------
! MXMM_WIND_LEVL_WIND_SHER_1, MXMM_WIND_LEVL_WIND_SHER_2,
! LEVL_RPLTN_CONT
!-----------------------------------------------------------------------

      Desc(18)=28           ! wind shear 1 (element 28 in Data Dict)
      Repl(18)=1
      Desc(19)=29           ! wind shear 2 (element 29 in Data Dict)
      Repl(19)=1
      Desc(20)=20           ! level count (element 20 in Data Dict)
      Repl(20)=1

!-----------------------------------------------------------------------
! LEVL_IDNY, LEVL_PESR, LEVL_HGHT, LEVL_TMPR, LEVL_DEW_PONT_TMPR,
! LEVL_WIND_DRCTN, LEVL_WIND_SPED
!
! Loop over 20 standard levels, 7 elements in each.
!-----------------------------------------------------------------------

      Counter=20               !- current position in our array

      DO I=1,20                !- We set a max of 20 standard levels.
        DO J=1,7               !- 7 elements per level.
          Counter=Counter+1    !- increment our array pointer
          Desc(Counter)=20+J   !- put element in array (21-27 in D.Dict)
          Repl(Counter)=I      !- store replication number.
        ENDDO
      ENDDO

      RETURN
      END

!***********************************************************************
!**   Subroutine SgnfElems
!***********************************************************************

      SUBROUTINE SgnfElems(Desc,NumDesc,Repl)

!-----------------------------------------------------------------------
!
! subroutine    : SgnfElems
!
!               : ANSI standard except for IMPLICIT NONE and '!' used
!               : for comments
!
! purpose       : MetDB Upper-Air retrieval. Used to set up the
!               : user selection element pointers for significant parts
!               : (B&D). These are passed to BITINDX for fast-retrieval
!               : of the data.
!
!               : Basically, the element pointer is the Data Dictionary
!               : element number for the element name wanted. Where
!               : there are replications e.g. levels, the element
!               : pointer is the same, but the replication count will
!               : increment.
!
! data type(s)  : TEMP, PILOT, DROPSOND
!
! called by     : UPRPARTS
!
! sub calls     : None
!
! arguments     :
!
! Desc(NumDesc) : integer   (op) : array of pointers
! NumDesc       : integer   (ip) : Number of pointers
! Repl(NumDesc) : integer   (op) : array of replications
!
! change record :
!
! 16-09-96      : Written S.Cox
!
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER  I,J
      INTEGER  Counter
      INTEGER  NumDesc
      INTEGER  Desc(NumDesc)
      INTEGER  Repl(NumDesc)

      SAVE

!-----------------------------------------------------------------------
! WMO_BLCK_NMBR, WMO_STTN_NMBR, CALL_SIGN, LTTD, LNGD, STTN_HGHT,
! PESR_HGHT, YEAR, MNTH, DAY, HOUR, MINT, RCPT_YEAR, RCPT_MNTH,
! RCPT_DAY, RCPT_HOUR, RCPT_MINT
!-----------------------------------------------------------------------

      DO I=1,17             !- our elements 1-17.
        Desc(I)=I+1         !- elements 2-18 in Data Dictionary.
        Repl(I)=1           !- only 1 replication of each element.
      ENDDO

!-----------------------------------------------------------------------
! RADI_SNDE_TYPE, TRCKG_SYTM, RADTN_CORTN, SEA_SRFC_TMPR,
! CLOD_PLTN_CONT
!-----------------------------------------------------------------------

      DO I=1,5
        Desc(I+17)=I+29        !- our elements 18-22.
        Repl(I+17)=1           !- only 1 replication of each element.
      ENDDO

!-----------------------------------------------------------------------
! CLOD_TYPE, CLOD_AMNT, CLOD_BASE_HGHT
!
! Loop over 4 cloud groups, 3 elements in each.
!-----------------------------------------------------------------------

      Counter=22               !- current position in our array

      DO I=1,4                 !- 4 cloud groups
        DO J=1,3               !- 3 elements per group
          Counter=Counter+1    !- increment our array pointer
          Desc(Counter)=34+J   !- put elems in array (35-37) in D.Dict
          Repl(Counter)=I      !- store replication number
        ENDDO
      ENDDO

!-----------------------------------------------------------------------
! LEVL_RPLTN_CONT
!-----------------------------------------------------------------------

      Desc(35)=20              !- put rep count in our array
      Repl(35)=1               !- 1 replication

!-----------------------------------------------------------------------
! LEVL_IDNY, LEVL_PESR, LEVL_HGHT, LEVL_TMPR, LEVL_DEW_PONT_TMPR,
! LEVL_WIND_DRCTN, LEVL_WIND_SPED
!
! Loop over 300 significant levels, 7 elements in each.
!-----------------------------------------------------------------------

      Counter=35               !- current position in our array

      DO I=1,300               !- We set a max of 300 sig levels.
        DO J=1,7               !- 7 elements per level.
          Counter=Counter+1    !- increment our array pointer
          Desc(Counter)=20+J   !- put element in array (21-27 in D.Dict)
          Repl(Counter)=I      !- store replication number.
        ENDDO
      ENDDO

      RETURN
      END
