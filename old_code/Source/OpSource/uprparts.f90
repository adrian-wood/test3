SUBROUTINE UPRPARTS(KeptPart,PosnPart,NumParts,Trailor,Message, &
                    Diagnostics,CharIndex,Istat,WantTemps, &
                    WantWinds,NumTempLevels,NumWindLevels, &
                    Latitude,Longitude,Hour,Minute,Day,Month, &
                    Year,ReportText,FinalProfile,MaxFinalElms, &
                    ArrElmNum,ArrSegNum,ArrSubNum,ArrNRows, &
                    ArrStyp,ArrSegst,ArrSegLen,ArrNsegs, &
                    Isect1,CallSign,                      &
                    NewMdbCall,CCCC,TTAA,Displ,Width,     &
                    Scale,RefVal,Found)

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
!     END DO
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
!     END DO
!
!     Only the elements extracted from BUFR section 3 go in
!     the array index. Other elements are assigned differ-
!     ently. These elements are:
!
!     RPRT_IDNY
!     RCPT_YEAR, RCPT_MNTH, RCPT_DAY, RCPT_HOUR, RCPT_MINT
!     RCPT_YEAR_PARTA, RCPT_MNTH_PARTA, RCPT_DAY_PARTA,
!     RCPT_HOUR_PARTA, RCPT_MINT_PARTA
!     RCPT_YEAR_PARTB, RCPT_MNTH_PARTB, RCPT_DAY_PARTB,
!     RCPT_HOUR_PARTB, RCPT_MINT_PARTB
!     RCPT_YEAR_PARTC, RCPT_MNTH_PARTC, RCPT_DAY_PARTC,
!     RCPT_HOUR_PARTC, RCPT_MINT_PARTC
!     RCPT_YEAR_PARTD, RCPT_MNTH_PARTD, RCPT_DAY_PARTD,
!     RCPT_HOUR_PARTD, RCPT_MINT_PARTD
!     PESR_SNSR_FLAG
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
! ArrStyp       : integer   (op)  : ARRINDX segment repl count array
! ArrSegst      : integer   (op)  : ARRINDX segment start array
! ArrSegLen     : integer   (op)  : ARRINDX segment length array
! ArrNsegs      : integer   (op)  : ARRINDX no. of segments
! Isect1        : integer   (op)  : array of TOR e.t.c.
! CallSign      : character (op)  : ship call sign
! NewMdbCall    : logical   (iop) : TRUE if new MetDB call
! CCCC          : character (op)  : Collecting centre
! TTAA          : character (op)  : TTAA
! Displ         : integer   (ip)  : Array used in BITINDX
! Width         : integer   (ip)  : Array used in BITINDX
! Scale         : integer   (ip)  : Array used in BITINDX
! RefVal        : integer   (ip)  : Array used in BITINDX
! Found(*)      : logical   (ip)  : Array of MetDB keywords selected
!
!Y2K  26.06.1997  UPRPARTS is Year 2000 compliant.
!Y2K                       Routine contains date management.
!
! REVISION INFO:
!
! $Workfile: uprparts.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 26/11/2010 12:04:14$
!
! change record :
!
! $Log:
!  5    MetDB_Refresh 1.4         26/11/2010 12:04:14    Brian Barwell   Add
!       ':' to first argument in calls to SORTR.
!  4    MetDB_Refresh 1.3         26/11/2010 11:13:23    Brian Barwell
!       Changed FINALPROFILE(*) to FINALPROFILE(:).
!  3    MetDB_Refresh 1.2         18/11/2010 15:09:25    John Norton     Merge
!       batch 20 changes and minor porting issues fixed.
!  2    MetDB_Refresh 1.1         17/11/2010 09:58:48    John Norton
!       Updated after doing rework for batch 13.
!  1    MetDB_Refresh 1.0         09/11/2010 11:35:01    John Norton     MetDB
!       Refresh created file  
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE bitindx_mod
USE dt2hrs_mod
USE desfxy_mod
USE eb2asc_mod
USE sethed_mod
USE sortr_mod
USE upaind_mod
USE valusr_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

LOGICAL,      INTENT(IN)    ::  KeptPart(4) ! which parts are in the chain
INTEGER,      INTENT(IN)    ::  PosnPart(4) ! position of part in chain
INTEGER,      INTENT(IN)    ::  NumParts ! number of parts in the chain
CHARACTER(*), INTENT(IN)    ::  Trailor(1:NumParts)
CHARACTER(*), INTENT(INOUT) ::  Message(1:NumParts)
LOGICAL,      INTENT(IN)    ::  Diagnostics ! TRUE if output wanted
CHARACTER(*), INTENT(IN)    ::  CharIndex(:)
INTEGER,      INTENT(INOUT) ::  Istat ! MetDB retrieval status condition
LOGICAL,      INTENT(IN)    ::  WantTemps ! TRUE if user wants temperature levels
LOGICAL,      INTENT(IN)    ::  WantWinds ! TRUE if user wants wind levels
INTEGER,      INTENT(IN)    ::  NumTempLevels ! number of temp levels wanted
INTEGER,      INTENT(IN)    ::  NumWindLevels ! number of wind levels wanted
REAL,         INTENT(IN)    ::  Latitude ! latitude
REAL,         INTENT(IN)    ::  Longitude ! longitude
INTEGER,      INTENT(IN)    ::  Hour ! hour
INTEGER,      INTENT(IN)    ::  Minute ! minute
INTEGER,      INTENT(IN)    ::  Day ! day
INTEGER,      INTENT(IN)    ::  Month ! month
INTEGER,      INTENT(IN)    ::  Year ! year
CHARACTER(*), INTENT(INOUT) ::  ReportText
REAL,         INTENT(OUT)   ::  FinalProfile(:) ! final profile
INTEGER,      INTENT(IN)    ::  MaxFinalElms ! max number of elems in FinalProfile
INTEGER,      INTENT(OUT)   ::  ArrElmNum(:) ! ARRINDX elements array
INTEGER,      INTENT(OUT)   ::  ArrSegNum(:) ! ARRINDX segment no. array
INTEGER,      INTENT(OUT)   ::  ArrSubNum(:) ! ARRINDX segment subscript array
INTEGER,      INTENT(OUT)   ::  ArrNrows ! ARRINDX no. of elements
INTEGER,      INTENT(OUT)   ::  ArrStyp(:) ! ARRINDX segment repl count array
INTEGER,      INTENT(OUT)   ::  ArrSegst(:) ! ARRINDX segment start array
INTEGER,      INTENT(OUT)   ::  ArrSegLen(:) ! ARRINDX segment length array
INTEGER,      INTENT(OUT)   ::  ArrNsegs ! ARRINDX no. of segments
INTEGER,      INTENT(OUT)   ::  Isect1(:) ! array of TOR e.t.c.
CHARACTER(*), INTENT(OUT)   ::  CallSign(1) ! Ship Call Sign
LOGICAL,      INTENT(INOUT) ::  NewMdbCall ! TRUE if new MetDB call
CHARACTER(4), INTENT(OUT)   ::  CCCC ! collecting centre
CHARACTER(*), INTENT(OUT)   ::  TTAA
INTEGER,      INTENT(INOUT) ::  Displ(:)
INTEGER,      INTENT(INOUT) ::  Width(:)
INTEGER,      INTENT(INOUT) ::  Scale(:)
INTEGER,      INTENT(INOUT) ::  RefVal(:)
LOGICAL,      INTENT(IN)    ::  Found(:) ! Array of MetDB keywords selected

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!-----------------------------------------------------------------------
! parameter statements
!-----------------------------------------------------------------------

INTEGER,     PARAMETER ::  HeaderLen = 44     ! length of raw report header
INTEGER,     PARAMETER ::  MaxPartElms = 4270 ! max no of elements in PartProfile
                                              ! array 300*14=4200 level elements
                                              ! + 70 non-level elements
INTEGER,     PARAMETER ::  SgnfElms = 2135    ! no. of significant elements
INTEGER,     PARAMETER ::  StndElms = 160     ! no. of standard elements

REAL,        PARAMETER ::  Rmdi = -9999999.0  ! missing data indicator
REAL,        PARAMETER ::  Tmdi = -9000000.0  ! missing data threshold

!-----------------------------------------------------------------------
! declare integers
!-----------------------------------------------------------------------

INTEGER      ::  BufrEnd     ! end of BUFR message in report
INTEGER      ::  BufrStart   ! start of BUFR message in report
INTEGER      ::  CurrentTOR  ! TOR of current part

INTEGER      ::  DescSgnf(SgnfElms)
INTEGER      ::  DescStnd(StndElms)
INTEGER      ::  DummyInt1(1) ! dummy array for VALUSR

INTEGER      ::  ElemPointer ! pointer to position in FinalProfile
INTEGER      ::  F
INTEGER      ::  FilledUserLevels ! filled no. of user levels requested
INTEGER      ::  HghtSortMask(14) ! SORTR mask for heights
INTEGER      ::  I           ! general loop counter
INTEGER      ::  Ifail       ! condition on return from BITINDX
INTEGER      ::  ILocalD     ! Integer Local D descriptor
INTEGER      ::  J           ! general loop counter
INTEGER      ::  K           ! general loop counter
INTEGER      ::  LenReportText ! length of users report text string
INTEGER      ::  LevelType   ! type of level
INTEGER      ::  MissHght    ! missing height levels counter
INTEGER      ::  NAC
INTEGER      ::  NAMD
INTEGER      ::  NCOR
INTEGER      ::  Nelreq    ! number of elements required
INTEGER      ::  Nobs      ! value passed to valusr
INTEGER      ::  PartLevels ! no. of levels in the part
INTEGER      ::  PartMap(4) ! maps A,B,C,D to A,C,B,D
INTEGER      ::  PartReportLen ! length of report text for a part
INTEGER      ::  RawReportLen ! length of raw report text
INTEGER      ::  RawReportPointer ! length of raw report text
INTEGER      ::  RcptHour
INTEGER      ::  RcptMinute

INTEGER      ::  ReplSgnf(SgnfElms)
INTEGER      ::  ReplStnd(StndElms)
INTEGER      ::  StoredTOR     ! highest TOR of the 4 parts

INTEGER      ::  PresSortMask(14) ! SORTR mask for pressures
INTEGER      ::  TotalPartLevels ! length of total raw report
INTEGER      ::  TotalUserLevels ! total no. of user levels requested
INTEGER      ::  TrByte17  ! trailor byte 17

INTEGER      ::  X
INTEGER      ::  Y

!-----------------------------------------------------------------------
! declare reals (alphabetical order)
!-----------------------------------------------------------------------

REAL         ::  PartProfile(1,MaxPartElms) ! initial profile

!-----------------------------------------------------------------------
! declare logicals (alphabetical order)
!-----------------------------------------------------------------------

LOGICAL      ::  AllPres   ! TRUE if all levels have pressures
LOGICAL      ::  AllHght   ! TRUE if all levels have pressures
LOGICAL      ::  FirstCall = .TRUE. ! TRUE if first call to subroutine
LOGICAL      ::  TempLevel ! TRUE if level is a temp level
LOGICAL      ::  WindLevel ! TRUE if level is a wind level

!-----------------------------------------------------------------------
! declare characters (alphabetical order)
!-----------------------------------------------------------------------

CHARACTER(4)         :: BUFR = 'BUFR'

CHARACTER(1)         :: DummyChr1         ! dummy string for valusr
CHARACTER(1)         :: DummyChr2(1)      ! dummy string for valusr

CHARACTER(6)         :: LocalD            ! local sequence descriptor

CHARACTER(HeaderLen) :: PartHeader ! header for each part

CHARACTER(4)         :: SEVENS = '7777'   ! '7777' end of BUFR msg

!-----------------------------------------------------------------------
! dynamic common, compile with FPARMS='DC(*)' on IBM mainframe
!-----------------------------------------------------------------------

COMMON /UPRPAR1/DescSgnf,DescStnd,ReplSgnf
COMMON /UPRPAR2/ReplStnd,PartProfile

!-----------------------------------------------------------------------
! data statements
!-----------------------------------------------------------------------

DATA      HghtSortMask   /0,-2,0,0,0,1,0,0,0,0,0,0,0,0/
DATA      PartMap        /1,3,2,4/
DATA      PresSortMask   /0,-2,0,-1,0,0,0,0,0,0,0,0,0,0/

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! need a SAVE statement for portability.
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! Initialise TOR in ISect1 for each part
!-----------------------------------------------------------------------

DO I=13,32
  Isect1(I)=-9999999
END DO

!-----------------------------------------------------------------------
! If first call to subroutine, get the list of element indexes for
! standard and then significant parts. Call UPAIND to initialise the
! element index for ARRINDX to put elements into the user's array.
! Convert EBCDIC BUFR to ASCII on first call to subroutine only.
!-----------------------------------------------------------------------

If (FirstCall) THEN
  CALL StndElems(DescStnd,StndElms,ReplStnd)
  CALL SgnfElems(DescSgnf,SgnfElms,ReplSgnf)

  CALL UPAIND(ArrElmNum,ArrSegNum,ArrSubNum,ArrNrows,ArrStyp, &
  ArrSegst,ArrSegLen,ArrNsegs)

  CALL EB2ASC(4,BUFR)
  CALL EB2ASC(4,SEVENS)
  PartHeader='0000Z DD/MM/YY LA.TTN LON.TTW CCCC TORM A C '
  FirstCall=.FALSE.
END IF

IF (Diagnostics) THEN
  write(*,*)'IN UPRPARTS: KeptPart = ',KeptPart
  write(*,*)'IN UPRPARTS: PosnPart = ',PosnPart
  write(*,*)'IN UPRPARTS: NumParts = ',NumParts
END IF

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
RawReportPointer  = 6        !- start past report text length
ReportText(:)     = ' '
CCCC(:)           = ' '
TTAA(:)           = ' '

LenReportText     = LEN(ReportText)

DO I=1,MaxFinalElms
  FinalProfile(I)=Rmdi
END DO

!-----------------------------------------------------------------------
! Loop through the parts in the order A,C,B,D using PartMap. Check that
! the part is in the chain and that there is room left in the users
! array.
!-----------------------------------------------------------------------

DOLABEL1: &
DO I=1,4

IFLABEL1: &
  IF (KeptPart(PartMap(I))) THEN

!-----------------------------------------------------------------------
! Initialise the PartProfile array to missing. Find the length of the
! raw report text and the start position of the BUFR message. Extract
! the local BUFR sequence descriptor from the message for use by
! BITINDX.
!-----------------------------------------------------------------------

    DO J=1,MaxPartElms
      PartProfile(1,J)=Rmdi
    END DO

    RawReportLen=INDEX(Message(PosnPart(PartMap(I))),BUFR)-1
    BufrStart=RawReportLen+1
    BufrEnd=INDEX(Message(PosnPart(PartMap(I))),SEVENS)+3

    ILocalD=ICHAR(Message(PosnPart(PartMap(I)))(BufrStart+29: &
            BufrStart+29))*256+ &
            ICHAR(Message(PosnPart(PartMap(I)))(BufrStart+30: &
            BufrStart+30))

    CALL DESFXY(ILocalD,F,X,Y)
    WRITE(LocalD,'(I1,I2.2,I3.3)')F,X,Y

!-----------------------------------------------------------------------
! If the part is A or C (I <= 2) call BITINDX with the standard element
! list, else call it with the significant element list.
!-----------------------------------------------------------------------

    IF (I <= 2) THEN
      CALL BITINDX(CharIndex,DescStnd,StndElms,       &
      ReplStnd,.TRUE.,Ifail,Diagnostics,LocalD,       &
      Message(PosnPart(PartMap(I)))(BufrStart:), &
      Nelreq,Displ,Width,Scale,RefVal,MaxPartElms,NewMdbCall)
    ELSE
      CALL BITINDX(CharIndex,DescSgnf,SgnfElms,       &
      ReplSgnf,.TRUE.,Ifail,Diagnostics,LocalD,       &
      Message(PosnPart(PartMap(I)))(BufrStart:), &
      Nelreq,Displ,Width,Scale,RefVal,MaxPartElms,NewMdbCall)
    END IF

    IF (Ifail == 16) THEN
      Istat=16
      IF (Diagnostics) THEN
        write(*,*)'IN UPRPARTS: Element Index not found'
      END IF
      GOTO 999
    END IF

!-----------------------------------------------------------------------
! Call VALUSR to put the details into the array PartProfile.
!-----------------------------------------------------------------------
    Nobs=1
    CALL VALUSR(Message(PosnPart(PartMap(I)))(BufrStart:), &
    Nelreq,Displ,Width,Scale,RefVal,PartProfile,1,Nobs,CallSign, &
    DummyChr1,DummyChr2,Diagnostics,Isect1,DummyChr2,DummyInt1)

    IF (Diagnostics) THEN
      write(*,'(/1X,''In UPRPARTS: Back from valusr''/)')
      DO J=1,1000
        write(*,*)'PartProfile = ',J,PartProfile(1,J)
      END DO
    END IF

!-----------------------------------------------------------------------
! Put report text into report text string.
!-----------------------------------------------------------------------

    CCCC       = Trailor(PosnPart(PartMap(I)))(8:11)
    NAC        = ICHAR(Trailor(PosnPart(PartMap(I)))(7:7))
    NAMD       = NAC/16
    NCOR       = MOD(NAC,15)

    IF (TTAA(:) == ' ') THEN
      TTAA = Trailor(PosnPart(PartMap(I)))(3:6)
    END IF

    RcptHour   = ICHAR(Message(PosnPart(PartMap(I))) &
                 (BufrStart+19:BufrStart+19))
    RcptMinute = ICHAR(Message(PosnPart(PartMap(I))) &
                 (BufrStart+20:BufrStart+20))

! Minute from index can have power of 2 (64 or 128) added as flag.
! (UPRRET has zeroed missing minute)  Unset flag to call SETHED.

    CALL SETHED(PartHeader,Latitude,Longitude,        &
                Hour,MOD(Minute,64),Day,              &
                Month,Year,RcptHour,RcptMinute,NAMD,NCOR,CCCC)

    IF (FOUND(34)) THEN
      PartReportLen=BufrEnd-BufrStart+1
    ELSE
      PartReportLen=RawReportLen+HeaderLen
    END IF

IFLABEL2: &
    IF ((RawReportPointer+PartReportLen-1) <= LenReportText) THEN

      IF (FOUND(34)) THEN
        ReportText(RawReportPointer:RawReportPointer+    &
        PartReportLen-1)=Message(PosnPart(PartMap(I)))   &
        (BufrStart:BufrEnd)
      ELSE
        ReportText(RawReportPointer:RawReportPointer+43)=   &
        PartHeader
        ReportText(RawReportPointer+44:RawReportPointer+44+ &
        PartReportLen-1)=Message(PosnPart(PartMap(I)))      &
        (1:RawReportLen)
      END IF

      IF (Diagnostics) THEN
        write(*,*)'In UPRPARTS: ReportText=',ReportText
      END IF

!-----------------------------------------------------------------------
! Final length of the report text minus the length (held in ReportText
! (1:5). Only ReportText(6:) are transferred to user by VALARR in
! UPRRET.
!-----------------------------------------------------------------------

      WRITE(ReportText(1:5),'(I5)')(RawReportPointer+     &
                                    PartReportLen-6)

    END IF IFLABEL2

    IF (FOUND(34)) THEN
      RawReportPointer=RawReportPointer+PartReportLen
    ELSE
      RawReportPointer=RawReportPointer+PartReportLen+1
    END IF

!-----------------------------------------------------------------------
! Handle the TOR for each part.
!-----------------------------------------------------------------------

IFLABEL3: &
    IF (PartProfile(1,26) > Tmdi .AND.       &
        PartProfile(1,28) > Tmdi .AND.       &
        PartProfile(1,30) > Tmdi .AND.       &
        PartProfile(1,32) > Tmdi) THEN

!-----------------------------------------------------------------------
! Put the TOR for each part into the Isect1 array.
!-----------------------------------------------------------------------

      IF (I == 1) J=13      !- part A
      IF (I == 2) J=23      !- part C
      IF (I == 3) J=18      !- part B
      IF (I == 4) J=28      !- part D

      Isect1(J)=NINT(PartProfile(1,26))
      Isect1(J+1)=NINT(PartProfile(1,28))
      Isect1(J+2)=NINT(PartProfile(1,30))
      Isect1(J+3)=NINT(PartProfile(1,32))
      Isect1(J+4)=NINT(PartProfile(1,34))

!-----------------------------------------------------------------------
! Put the latest TOR into the Isect1 array as well.
!
! Convert the TOR YYYY,MM,DD,HH to century hours & add minutes. If
! later than stored TOR, put into Isect1 array
!-----------------------------------------------------------------------

      CurrentTOR=DT2HRS(NINT(PartProfile(1,26)), &
                        NINT(PartProfile(1,28)), &
                        NINT(PartProfile(1,30)), &
                        NINT(PartProfile(1,32)))

      CurrentTOR=(60*CurrentTOR)+NINT(PartProfile(1,34))

      IF (CurrentTOR > StoredTOR) THEN
        StoredTOR=CurrentTOR
        Isect1(1)=NINT(PartProfile(1,26))
        Isect1(2)=NINT(PartProfile(1,28))
        Isect1(3)=NINT(PartProfile(1,30))
        Isect1(4)=NINT(PartProfile(1,32))
        Isect1(5)=NINT(PartProfile(1,34))
      END IF
    END IF IFLABEL3

!-----------------------------------------------------------------------
! If the part is a part A or B, look at the trailor bit of byte 17. If
! it is 0, the part has a pressure sensor. If it is 1 it doesn't. Set
! Isect1(12) to the value of byte 17, bit 5.
!-----------------------------------------------------------------------

    IF (I == 1 .OR. I == 3) THEN
      TrByte17 = ICHAR(Trailor(PosnPart(PartMap(I)))(17:17))
      Isect1(12) = MOD(TrByte17/8,2)
    END IF

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

    DO J=1,14
      IF (FinalProfile(J) < Tmdi) THEN
        FinalProfile(J)=PartProfile(1,J)
      END IF
    END DO

!-----------------------------------------------------------------------
! For date/time (year, month, day, hour, minute) take all five
! elements from the same part.  Launch time can reset the hour &
! hence possible the date, so don't set year/month/day/hour &
! then minute from a different part!
! So code below sets date/time from first part but then resets it
! if part has minute set & final profile not.
!-----------------------------------------------------------------------

    IF (FinalProfile(16) < Tmdi .OR.        &
      ((FinalProfile(24) < Tmdi .AND.       &
         PartProfile(1,24) > Tmdi))) THEN
      DO J=15,24
        FinalProfile(J)=PartProfile(1,J)
      END DO
    END IF

!-----------------------------------------------------------------------
! for standard parts (A&C) only !! put MXMM_WIND_LEVL_WIND_SHER_1 and
! MXMM_WIND_LEVL_WIND_SHER_2 + associated QC bits into the FinalProfile
! if they are not already there.
!-----------------------------------------------------------------------

IFLABEL4: &
    IF (I <= 2) THEN         !- Parts A&C only
      DO J=25,28
        IF (FinalProfile(J) < Tmdi) THEN
          FinalProfile(J)=PartProfile(1,J+10)
        END IF
      END DO

!-----------------------------------------------------------------------
! for significant parts (B&D) only !! put RADI_SNDE_TYPE, TRCKG_SYTM,
! RADTN_CORTN, SEA_SRFC_TMPR, CLOD_RPLTN_CONT, CLOD_TYPE, CLOD_AMNT,
! CLOD_BASE_HGHT + associated QC bits into the FinalProfile if they are
! are not already there.
!-----------------------------------------------------------------------

    ELSE                     !- Parts B&D only
      DO J=29,38
        IF (FinalProfile(J) < Tmdi) THEN
          FinalProfile(J)=PartProfile(1,J+6)
        END IF
      END DO  !- j loop

      DO J=1,4                           !- 4 cloud groups
        DO K=39+(J-1)*6,44+(J-1)*6
          IF (FinalProfile(K) < Tmdi) THEN
            FinalProfile(K)=PartProfile(1,K+6)
          END IF
        END DO  !- k loop
      END DO  !- j loop
    END IF IFLABEL4 !- i loop

!=======================================================================
! Now we need to put the level data into the FinalProfile. Make a note
! of the number of levels in the part. The location in the PartProfile
! array is different for standard and significant parts.
!=======================================================================

    IF (I <= 2) PartLevels=NINT(PartProfile(1,40))  ! Parts A&C
    IF (I >= 3) PartLevels=NINT(PartProfile(1,70))  ! Parts B&D

    IF (Diagnostics) THEN
      write(*,*)'In UPRPARTS: PartLevels      = ',PartLevels
      write(*,*)'In UPRPARTS: TotalUserLevels = ',TotalUserLevels
    END IF

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

IFLABEL5: &
    IF (PartLevels > 0) THEN

IFLABEL6: &
      IF (WantTemps .AND. WantWinds) THEN

        TotalPartLevels=TotalPartLevels+PartLevels
        J=1
DOLABEL2: &
        DO WHILE (FilledUserLevels < TotalUserLevels .AND. &
        J <= PartLevels)

          DO K=1,14                             !- 14 elements
            ElemPointer=ElemPointer+1
            IF (I <= 2) THEN                    !- Parts A&C
              FinalProfile(64+ElemPointer)= &
              PartProfile(1,40+K+(J-1)*14)
            ELSE                                !- Parts B&D
              FinalProfile(64+ElemPointer)= &
              PartProfile(1,70+K+(J-1)*14)
            END IF  !- i <= 2
          END DO  !- k loop

          J=J+1
          FilledUserLevels=FilledUserLevels+1
        END DO DOLABEL2 !- do while levels to transfer

        IF (Diagnostics) THEN
         write(*,*)'In UPAPARTS: FilledUserLevs=',FilledUserLevels
        END IF

!-----------------------------------------------------------------------
! User wants just wind levels or just temperature levels. Again loop
! over levels until there are no more to read, or the user's max has
! been reached. For each level, work out what type of level it is.
! (LevelType). If we want to transfer this level, do, otherwise don't.
!-----------------------------------------------------------------------

      ELSE IF ((WantTemps .AND. (.NOT.WantWinds)) .OR. &
              (WantWinds .AND. (.NOT.WantTemps))) THEN

DOLABEL3: &
        DO J=1,PartLevels

          IF (I <= 2) THEN                        !- Parts A&C
            LevelType=NINT(PartProfile(1,42+(J-1)*14))
          ELSE                                    !- Parts B&D
            LevelType=NINT(PartProfile(1,72+(J-1)*14))
          END IF  !- i <= 2

          WindLevel=((MOD(LevelType/64,2) == 1) .OR. & !- surf
                     (MOD(LevelType/32,2) == 1) .OR. & !- stnd
                     (MOD(LevelType/16,2) == 1) .OR. & !- trop
                     (MOD(LevelType/8,2) == 1)  .OR. & !- max wind
                     (MOD(LevelType/2,2) == 1))        !- sig wind

          TempLevel=((MOD(LevelType/64,2) == 1) .OR. & !- surf
                     (MOD(LevelType/32,2) == 1) .OR. & !- stnd
                     (MOD(LevelType/16,2) == 1) .OR. & !- trop
                     (MOD(LevelType/4,2) == 1))        !- sig temp

          IF (Diagnostics) THEN
            write(*,*)'In UPRPARTS: J,WindLevel,TempLevel=', &
            J,WindLevel,TempLevel
          END IF

IFLABEL7: &
          IF ((WantWinds .AND. WindLevel) .OR. (WantTemps .AND. &
          TempLevel)) THEN

            TotalPartLevels=TotalPartLevels+1
IFLABEL8: &
            IF (FilledUserLevels < TotalUserLevels) THEN

              DO K=1,14                             !- 14 elements
                ElemPointer=ElemPointer+1
                IF (I <= 2) THEN                    !- Parts A&C
                  FinalProfile(64+ElemPointer)= &
                  PartProfile(1,40+K+(J-1)*14)
                ELSE                                !- Parts B&D
                  FinalProfile(64+ElemPointer)= &
                  PartProfile(1,70+K+(J-1)*14)
                END IF  !- i <= 2
              END DO  !- k loop

              FilledUserLevels=FilledUserLevels+1

            END IF IFLABEL8 !- FilledUserLevels < TotalUserLevel
          END IF IFLABEL7 !-wantlevels block
        END DO DOLABEL3 !- do while levels to transfer

        IF (Diagnostics) THEN
         write(*,*)'In UPRPARTS: FilledUserLevs=',FilledUserLevels
         write(*,*)'In UPRPARTS: TotalPartLevs =',TotalPartLevels
        END IF

!-----------------------------------------------------------------------
! User doesn't want any level data - so don't transfer any to
! FinalProfile
!-----------------------------------------------------------------------

      ELSE
        IF (Diagnostics) THEN
          write(*,*)'In UPRPARTS: No level data requested'
        END IF
      END IF IFLABEL6 !- WantTemps/WantWinds if block

    END IF IFLABEL5 !- PartLevels > 0 check

  END IF IFLABEL1 !- KeptPart if block

  IF (Diagnostics) THEN
    write(*,'(/1X,''In UPRPARTS: FinalProfile''/)')
    DO J=1,1000
      write(*,*)'FinalProfile=',J,FinalProfile(J)
    END DO
  END IF

!-----------------------------------------------------------------------
! Check all the levels in the final profile for any missing pressures
! and heights. These are used later in the profile sorting.
!-----------------------------------------------------------------------

  AllPres = .TRUE.
  AllHght = .TRUE.

  IF (FilledUserLevels > 0) THEN
    DO J=1,FilledUserLevels
      IF (FinalProfile(68+(J-1)*14) <= Tmdi) AllPres = .FALSE.
      IF (FinalProfile(70+(J-1)*14) <= Tmdi) AllHght = .FALSE.
    END DO
  END IF

END DO DOLABEL1 !- i loop over parts A,C,B,D

!-----------------------------------------------------------------------
! Put total number of levels into the FinalProfile array, and set the
! condition code to 0 or 8 depending on whether the user has allowed
! enough replication counts for them.
!-----------------------------------------------------------------------

IF (FilledUserLevels > 0) FinalProfile(64)=REAL(TotalPartLevels)

IF (TotalPartLevels <= FilledUserLevels) THEN
  Isect1(11)=0
ELSE
  Isect1(11)=8
END IF

IF (Diagnostics) THEN
  write(*,  &
   '(/1X,''In UPRPARTS: Final profile before SORTR''/)')
  DO J=1,1000
    write(*,*)'J,FinalProfile=',J,FinalProfile(J)
  END DO
END IF

!-----------------------------------------------------------------------
! Now sort the levels into order. If all levels have a pressure, sort
! on pressures. Pass the array elements from subscript 65 onwards to
! the sort program as we only want to sort the levels. The levels are
! sorted into decreasing pressure order.
!-----------------------------------------------------------------------

IFLABEL9: &
IF (AllPres) THEN

  CALL SORTR(FinalProfile(65:),14,FilledUserLevels,  &
             PresSortMask)

  IF (Diagnostics) THEN
    write(*, &
     '(/1X,''UPRPARTS: Profile after sort on pressure''/)')
    DO J=1,1000
      write(*,*)'J,FinalProfile=',J,FinalProfile(J)
    END DO
  END IF

!-----------------------------------------------------------------------
! If not all of the levels have a pressure, sort them on heights,
! in ascending order.
!-----------------------------------------------------------------------

ELSE    !- no pressures available, sort on heights

  CALL SORTR(FinalProfile(65:),14,FilledUserLevels,  &
             HghtSortMask)

  IF (Diagnostics) THEN
    write(*, &
     '(/1X,''UPRPARTS: profile after SORTR on height''/)')
    DO J=1,1000
      write(*,*)'J,FinalProfile=',J,FinalProfile(J)
    END DO
  END IF

!-----------------------------------------------------------------------
! We may have sorted the levels by height, but it is still possible for
! there to be missing height levels. This is usually because a Max wind
! has been reported in a PILOT part A. In this case, we want to put
! the missing height levels at the end of the profile. Loop over the
! levels, replacing the missing heights with a large positive height,
! resort the levels on height and change the large +ve heights back
! to missing data indicators.
!-----------------------------------------------------------------------

IFLABEL10: &
  IF (.NOT.AllHght) THEN
    MissHght=0
    J=0
    DO WHILE (J < FilledUserLevels)
      J=J+1
      IF (FinalProfile(70+(J-1)*14) <= Tmdi) THEN
        FinalProfile(70+(J-1)*14)=9000000.0
        MissHght=MissHght+1
      END IF
    END DO

    CALL SORTR(FinalProfile(65:),14,FilledUserLevels, &
               HghtSortMask)

    DO J=1,MissHght
      FinalProfile(70+(FilledUserLevels-J)*14)=Rmdi
    END DO
  END IF IFLABEL10

END IF IFLABEL9 !- sort on pressures or heights.

999   CONTINUE

RETURN
CONTAINS

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

  ! Use statements:
  ! <Interfaces>

  ! None

  ! <Data Modules>

  IMPLICIT NONE

  ! Subroutine arguments:

  INTEGER,      INTENT(IN)    ::  NumDesc
  INTEGER,      INTENT(OUT)   ::  Desc(NumDesc)
  INTEGER,      INTENT(OUT)   ::  Repl(NumDesc)

  ! Subroutine result:
  !<declare the type returned by the Subroutine>
  ! Local declarations:
  !<parameters, derived data types, variables, ...>

  INTEGER      ::  I,J
  INTEGER      ::  Counter

  ! INTERFACE blocks
  !<INCLUDE interface files...>
  !<other interface blocks...>
  !<other specification statements ...>
  !-----------------------------------------------------------------------
  !<executable statements ...>
  !-----------------------------------------------------------------------

  SAVE

  !-----------------------------------------------------------------------
  ! WMO_BLCK_NMBR, WMO_STTN_NMBR, CALL_SIGN, LTTD, LNGD, STTN_HGHT,
  ! PESR_HGHT, YEAR, MNTH, DAY, HOUR, MINT, RCPT_YEAR, RCPT_MNTH,
  ! RCPT_DAY, RCPT_HOUR, RCPT_MINT
  !-----------------------------------------------------------------------

  DO I=1,17             !- our elements 1-17.
    Desc(I)=I+1         !- elements 2-18 in Data Dictionary.
    Repl(I)=1           !- only 1 replication of each element.
  END DO

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
    END DO
  END DO

  RETURN
  END SUBROUTINE StndElems

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

  ! Use statements:
  ! <Interfaces>

  ! None

  ! <Data Modules>

  IMPLICIT NONE

  ! Subroutine arguments:

  INTEGER,      INTENT(IN)    ::  NumDesc
  INTEGER,      INTENT(OUT)   ::  Desc(NumDesc)
  INTEGER,      INTENT(OUT)   ::  Repl(NumDesc)

  ! Subroutine result:
  !<declare the type returned by the Subroutine>
  ! Local declarations:
  !<parameters, derived data types, variables, ...>

  INTEGER      ::  I,J
  INTEGER      ::  Counter

  ! INTERFACE blocks
  !<INCLUDE interface files...>
  !<other interface blocks...>
  !<other specification statements ...>
  !-----------------------------------------------------------------------
  !<executable statements ...>
  !-----------------------------------------------------------------------

  SAVE

  !-----------------------------------------------------------------------
  ! WMO_BLCK_NMBR, WMO_STTN_NMBR, CALL_SIGN, LTTD, LNGD, STTN_HGHT,
  ! PESR_HGHT, YEAR, MNTH, DAY, HOUR, MINT, RCPT_YEAR, RCPT_MNTH,
  ! RCPT_DAY, RCPT_HOUR, RCPT_MINT
  !-----------------------------------------------------------------------

  DO I=1,17             !- our elements 1-17.
    Desc(I)=I+1         !- elements 2-18 in Data Dictionary.
    Repl(I)=1           !- only 1 replication of each element.
  END DO

  !-----------------------------------------------------------------------
  ! RADI_SNDE_TYPE, TRCKG_SYTM, RADTN_CORTN, SEA_SRFC_TMPR,
  ! CLOD_PLTN_CONT
  !-----------------------------------------------------------------------

  DO I=1,5
    Desc(I+17)=I+29        !- our elements 18-22.
    Repl(I+17)=1           !- only 1 replication of each element.
  END DO

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
    END DO
  END DO

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
    END DO
  END DO

  RETURN
  END SUBROUTINE SgnfElems
END SUBROUTINE UPRPARTS
