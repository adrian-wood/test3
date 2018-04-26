PROGRAM STATSBUF

!-----------------------------------------------------------------------
!
! PROGRAM       : STATSBUF
!
! PURPOSE       : WRITE 24 HOUR TOTALS FOR BUFR DATASETS.
!
! DESCRIPTION   : WRITE THE BUFR STATISTICS TO BROWSABLE DATASETS.
!
! DATA TYPES    : TEMPS(FIXED & MOBILE), PILOTS(FIXED & MOBILE),
!               : DROPSOND                                          !1.6
!
! JCL ON        : SYS1.OPSPROD.JOBLIB(MDBSTATS) SUBMITTED AT 0430Z  !1.6
!
! NAMELIST      : INSTAT  (Unit 2).  Contents as follows:           !2
!
!                 Variable Type        Description              Default
!                 -------- ----        -----------              -------
!                  NDAY    I*4   Day of month for which stats     -1
!                                are required (or, if negative,
!                                days before 'today').
!                  DSN     C*10  First two levels of the stats dsns
!                                                          'MDB.STATS'
!
! CALLS         : HRS2DT
!               : DT2HRS
!               : DATE31
!               : DATE13
!
! I/O           : UNITS 21 TO 20+NDSN (STATS DATASETS)                !A
!               : UNITS 51 TO 50+NDSN (DATA STORES)                   !A
!
! REVISION INFO :
!
! $Revision:
! $Date: 28/08/2012 16:30:48$
! $Source: $
!
! CHANGE RECORD :
!
! $Log:
!-----------------------------------------------------------------------
! ndsn = number of datasets to check
!-----------------------------------------------------------------------
! use statements
USE hrs2dt_mod
USE dt2hrs_mod
USE zpdate_mod
USE datim_mod
!
IMPLICIT NONE
!
INTEGER,PARAMETER             ::     BLKSIZ=27998
INTEGER,PARAMETER             ::     NDSN=5
CHARACTER(LEN=BLKSIZ)         ::     CINDX
CHARACTER(LEN=3)              ::     CMONTH(12)
CHARACTER(LEN=9)              ::     WKDAY(7)
CHARACTER(LEN=23)             ::     CNTRY
CHARACTER(LEN=80)             ::     CHEAD(4)
CHARACTER(LEN=80)             ::     CDATA(672)
INTEGER                       ::     NBLKS
INTEGER                       ::     NXBLK
INTEGER                       ::     NXHRS
INTEGER                       ::     NAMHR
INTEGER                       ::     NTRIES
INTEGER                       ::     NTOR
INTEGER                       ::     I
INTEGER                       ::     IBLOCK
INTEGER                       ::     ICD
INTEGER                       ::     ID
INTEGER                       ::     ID1
INTEGER                       ::     IDAY
INTEGER                       ::     IDHR
INTEGER                       ::     IDT(14)
INTEGER                       ::     IH1
INTEGER                       ::     IHOUR
INTEGER                       ::     IM
INTEGER                       ::     IMNHR
INTEGER                       ::     IMXHR
INTEGER                       ::     IM1
INTEGER                       ::     INDHR1
INTEGER                       ::     INXBLK
INTEGER                       ::     INXHRS
INTEGER                       ::     ISHR1
INTEGER                       ::     ITIM1
INTEGER                       ::     ITOR
INTEGER                       ::     ITORMX
INTEGER                       ::     ITORMN
INTEGER                       ::     ITOTAL
INTEGER                       ::     IWKDAY
INTEGER                       ::     IY
INTEGER                       ::     IY1
INTEGER                       ::     IUNIT
INTEGER                       ::     IDSUNT
INTEGER                       ::     J1
INTEGER                       ::     J2
INTEGER                       ::     J3
INTEGER                       ::     J27
INTEGER                       ::     NSQ
INTEGER                       ::     NSEQBL
INTEGER                       ::     OBS
INTEGER                       ::     OBTOT
INTEGER                       ::     PARTS
INTEGER                       ::     PARTTOT
INTEGER                       ::     RECL(NDSN)
INTEGER                       ::     IOS
INTEGER                       ::     LENQ
LOGICAL                       ::     LLAT
LOGICAL                       ::     LTOR
LOGICAL                       ::     UPAIR
LOGICAL                       ::     TEMP
LOGICAL                       ::     PILOT
LOGICAL                       ::     FIXED
LOGICAL                       ::     MOBILE
LOGICAL                       ::     PFSOUGHT
LOGICAL                       ::     PMSOUGHT
LOGICAL                       ::     TFSOUGHT
LOGICAL                       ::     TMSOUGHT
LOGICAL                       ::     DRSOUGHT
LOGICAL                       ::     PFFOUND
LOGICAL                       ::     PMFOUND
LOGICAL                       ::     TFFOUND
LOGICAL                       ::     TMFOUND
LOGICAL                       ::     DRFOUND
LOGICAL                       ::     TPFOUND
LOGICAL                       ::     GOOD
#if ! defined (MVS)
CHARACTER(LEN=8)              ::     DATYPE(NDSN)
CHARACTER(LEN=8)              ::     THRDLQ(NDSN)
CHARACTER(LEN=200)            ::     FILENAME
CHARACTER(LEN=200)            ::     STORAGE_DIR
#endif

!
INTEGER                       ::     NDAY ! DOM for which stats are req
                                    ! (or, if neg, days before 'today')
CHARACTER(LEN=10)             ::     DSN    ! First 2 lev stats dataset
NAMELIST /INSTAT/ NDAY, DSN
DATA  NDAY, DSN /  -1,  'MDB.STATS'/

! Blocksizes for FT51, FT52 etc in order

DATA RECL/27998,27998,27998,27998,27998/

DATA WKDAY/'Sunday','Monday','Tuesday','Wednesday','Thursday',&
      'Friday','Saturday'/
DATA CMONTH/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',&
      'Oct','Nov','Dec'/
#if ! defined (MVS)
DATA DATYPE/'TEMPF','TEMPM','PILOTF','PILOTM','DROPSOND'/
DATA THRDLQ/'UPRAIR','UPRAIR','UPRAIR','UPRAIR','UPRAIR'/
#endif

!-----------------------------------------------------------------------
! get ENV VAR STORAGE_DIR from system
!-----------------------------------------------------------------------

#if ! defined (MVS)
CALL METDB_GETENV("STORAGE_DIR",STORAGE_DIR,I)
#endif

!-----------------------------------------------------------------------
! Read first two levels of statistics data set and date from
! namelist. (Negative dates refer to days before today.)
!-----------------------------------------------------------------------

INQUIRE (FILE='FT02F001', EXIST=GOOD)
IF (GOOD) THEN
  OPEN (2,IOSTAT=IOS)
  READ (2, INSTAT, IOSTAT=IOS)
  CLOSE (2)
END IF

LENQ = INDEX(DSN//' ',' ') - 1    ! LENGTH OF HLQ
!-----------------------------------------------------------------------
! get current date/time from system
!-----------------------------------------------------------------------

CALL DATIM(IDT)
!                                              NDAY = days before today
IF (NDAY <= 0) THEN
  CALL DATE31 (IDT(6), IDT(7), IDT(8), ICD)
  ICD = ICD + NDAY
  CALL DATE13 (ICD, ID, IM, IY)
!                                            NDAY = actual day of month
ELSE
  IM = IDT(7)
  IY = IDT(8)
  IF (NDAY > IDT(6)) THEN    ! Must be last month
    IM = IM - 1
    IF (IM == 0) THEN    ! Must be last year
      IM = 12
      IY = IY - 1
    END IF
  END IF
  ID = NDAY
  CALL DATE31 (ID, IM, IY, ICD)
END IF

IWKDAY = MOD(ICD,7) + 1

!-----------------------------------------------------------------------
! loop over datasets
!-----------------------------------------------------------------------

  DO 199 J1=1,NDSN
    IUNIT = 50 + J1   ! UNIT NUMBER FOR A DATA STORE
    IDSUNT=20+J1      ! UNIT NUMBER FOR A STATS DATASET
#if defined (MVS)
    OPEN(IDSUNT)
#else
    FILENAME=DSN(1:LENQ)//'.'//DATYPE(J1)
    OPEN(IDSUNT,FILE=FILENAME)
    I=INDEX(STORAGE_DIR,' ')-1
    FILENAME=STORAGE_DIR(1:I)//'/MDB.'//THRDLQ(J1)
    OPEN(IUNIT,ACCESS='DIRECT',RECL=RECL(J1),FILE=FILENAME)
#endif
    READ(IDSUNT,'(A80)')CHEAD
    UPAIR=.FALSE.
    IF (CHEAD(2)(70:70)  ==  'U') UPAIR=.TRUE.
    IF (UPAIR) THEN
      TFSOUGHT=.FALSE.
      TMSOUGHT=.FALSE.
      PFSOUGHT=.FALSE.
      PMSOUGHT=.FALSE.
      DRSOUGHT=.FALSE.
      IF (CHEAD(2)(71:72)  ==  'TF') TFSOUGHT=.TRUE.
      IF (CHEAD(2)(71:72)  ==  'TM') TMSOUGHT=.TRUE.
      IF (CHEAD(2)(71:72)  ==  'PF') PFSOUGHT=.TRUE.
      IF (CHEAD(2)(71:72)  ==  'PM') PMSOUGHT=.TRUE.
      IF (CHEAD(2)(71:72)  ==  'DR') DRSOUGHT=.TRUE.
    ENDIF
    DO 10 I=1,32
      CDATA(I)=' '
   10     CONTINUE
    READ(IDSUNT,'(A80)')(CDATA(I),I=33,672)
#if defined (MVS)
    OPEN(IUNIT,ACCESS='DIRECT',RECL=RECL(J1),ACTION='READ')
#else
    OPEN(IUNIT,ACCESS='DIRECT',RECL=RECL(J1))
#endif
    CDATA(1)='=================================='
    CDATA(2)(1:9)=WKDAY(IWKDAY)
    WRITE(CDATA(2)(11:12),'(I2.2)')ID
    CDATA(2)(14:16)=CMONTH(IM)
    WRITE(CDATA(2)(18:21),'(I4)')IY
    IF (UPAIR) THEN
      CDATA(3)='---------------------------------------------'
      CDATA(4)='Hours      Ascents     Receipt (Z)      Parts'
      CDATA(5)='                       First  Last           '
      CDATA(6)='---------------------------------------------'
    ELSE
      CDATA(3)='----------------------------------'
      CDATA(4)='Hours   Observations   Receipt (Z)'
      CDATA(5)='                       First  Last'
      CDATA(6)='----------------------------------'
    ENDIF

!-----------------------------------------------------------------------
! read map block
!-----------------------------------------------------------------------

    READ(IUNIT,REC=1)CINDX(1:RECL(J1))
    READ(CINDX(1:2),'(A2)')NBLKS !NUMBER OF BLOCKS IN DATASET
    READ(CINDX(3:4),'(A2)')NXBLK !NUMBER OF INDEX BLOCKS
    READ(CINDX(5:6),'(A2)')NXHRS !NUMBER OF HOURS IN INDEX BLOCK
    READ(CINDX(7:8),'(A2)')NAMHR !NUMBER OF HOURS AFTER MIDNIGHT
    INXBLK = NXBLK
    INXHRS = NXHRS

    NSEQBL=ICHAR(CINDX(8+NBLKS:8+NBLKS)) ! SEQUENCE BLOCK NUMBER
    IF (NSEQBL  >  0) NSQ = 1   ! BLOCK B/N MAP & INDEX
    IF (NSEQBL  ==  0) NSQ = 0  ! IF ZERO, NO SEQ BLOCK

    ITOTAL=0

!-----------------------------------------------------------------------
! loop over hours
!-----------------------------------------------------------------------

    DO 179 J2 = 0,23

!-----------------------------------------------------------------------
! find index block for this hour
!-----------------------------------------------------------------------

      J27=J2+7
      ITIM1 = DT2HRS(IY,IM,ID,J2)      ! REQUIRED CENTURY HOUR
      INDHR1 = MOD(J2+24-NAMHR,INXHRS) ! DISPLACEMENT IN SLOT
      ISHR1 = ITIM1-INDHR1             ! SLOT HOUR
      IBLOCK = MOD(ISHR1/INXHRS,INXBLK)+2+NSQ  ! BLOCK NUMBER
      READ(IUNIT,REC=IBLOCK) CINDX(1:RECL(J1))
      IDAY = ICHAR(CINDX(1:1))
      IHOUR = ICHAR(CINDX(2:2))
      CALL HRS2DT(IY1,IM1,ID1,IH1,ISHR1)

!-----------------------------------------------------------------------
! check index base hour
!-----------------------------------------------------------------------

      IF (IDAY /= ID1.OR.IHOUR /= IH1) THEN
        WRITE(CDATA(J27)(3:4),'(I2.2)')J2
        CDATA(J27)(12:18)='NO DATA'
      ELSE
        READ(CINDX(3:4),'(A2)') NTRIES
        OBTOT = 0
        PARTTOT  = 0
        LTOR = .TRUE.
        LLAT = .TRUE.

!-----------------------------------------------------------------------
! loop over entries in this block
!-----------------------------------------------------------------------

        DO 159 J3 = 1,NTRIES
          CNTRY = CINDX(7+23*(J3-1):29+23*(J3-1))
          IF (UPAIR) THEN
            TEMP=.FALSE.
            PILOT=.FALSE.
            FIXED=.FALSE.
            MOBILE=.FALSE.
            TFFOUND=.FALSE.
            TMFOUND=.FALSE.
            PFFOUND=.FALSE.
            PMFOUND=.FALSE.
            DRFOUND=.FALSE.
            TPFOUND=.FALSE.
            OBS=0
            PARTS=0
            IF (MOD(ICHAR(CNTRY(17:17))/8, 2)  ==  1) THEN
              IF (MOD(ICHAR(CNTRY(17:17))/4, 2)  ==  1) THEN
                TPFOUND=.TRUE.
              ELSE
                DRFOUND=.TRUE.
              ENDIF
            ELSE
              IF (MOD(ICHAR(CNTRY(17:17))/4, 2)  ==  1) THEN
                TEMP=.TRUE.
              ELSE
                PILOT=.TRUE.
              ENDIF
              IF (MOD(ICHAR(CNTRY(17:17))/2, 2)  ==  1) THEN
                MOBILE=.TRUE.
              ELSE
                FIXED=.TRUE.
              ENDIF
              IF (TEMP .AND. FIXED)   TFFOUND=.TRUE.
              IF (TEMP .AND. MOBILE)  TMFOUND=.TRUE.
              IF (PILOT .AND. FIXED)  PFFOUND=.TRUE.
              IF (PILOT .AND. MOBILE) PMFOUND=.TRUE.
            ENDIF
            IF (TFSOUGHT .AND. TFFOUND) OBS=1
            IF (TFSOUGHT .AND. TPFOUND) OBS=1
            IF (TMSOUGHT .AND. TMFOUND) OBS=1
            IF (PFSOUGHT .AND. PFFOUND) OBS=1
            IF (PFSOUGHT .AND. TPFOUND) OBS=1
            IF (PMSOUGHT .AND. PMFOUND) OBS=1
            IF (DRSOUGHT .AND. DRFOUND) OBS=1
            IF (OBS  ==  1) PARTS=ICHAR(CNTRY(12:12))
          ELSE
            OBS = ICHAR(CNTRY(12:12))
            IF (OBS  ==  0) OBS = 1
          ENDIF

!-----------------------------------------------------------------------
! check for current hour
!-----------------------------------------------------------------------

          IDHR = ICHAR(CNTRY(1:1))
          IDHR = MOD(IDHR,64)
          IF (IDHR == INDHR1) THEN
            OBTOT = OBTOT + OBS
            IF (UPAIR) PARTTOT=PARTTOT + PARTS
            READ(CNTRY(18:19),'(A2)')NTOR
            ITOR=NTOR

!-----------------------------------------------------------------------
! find earliest and latest times
!-----------------------------------------------------------------------

            IF (LTOR) THEN
              ITORMX = ITOR
              ITORMN = ITOR
              LTOR = .FALSE.
            ELSEIF (ITOR > ITORMX) THEN
              ITORMX = ITOR
            ELSEIF (ITOR < ITORMN) THEN
              ITORMN = ITOR
            ENDIF
          ENDIF
159           CONTINUE                   ! END OF LOOP OVER ENTRIES
        IF (OBTOT == 0) THEN
          WRITE(CDATA(J27)(3:4),'(I2.2)')J2
          CDATA(J27)(12:18)='NO DATA'
        ELSE

!-----------------------------------------------------------------------
! convert times to hhmm z
!-----------------------------------------------------------------------

          IMNHR = IH1 + ITORMN/60
          IF (IMNHR >= 24) IMNHR = IMNHR-24
          ITORMN = IMNHR*100 + MOD(ITORMN,60)
          IMXHR = IH1 + ITORMX/60
          IF (IMXHR >= 24) IMXHR = IMXHR-24
          ITORMX = IMXHR*100 + MOD(ITORMX,60)
          ITOTAL=ITOTAL+OBTOT
          WRITE(CDATA(J27)(3:4),'(I2.2)')J2
          WRITE(CDATA(J27)(12:16),'(I5)')OBTOT
          WRITE(CDATA(J27)(25:28),'(I4.4)')ITORMN
          WRITE(CDATA(J27)(31:34),'(I4.4)')ITORMX
          IF (UPAIR) WRITE(CDATA(J27)(39:43),'(I5)')PARTTOT
        ENDIF
      ENDIF
179       CONTINUE
    CDATA(31)='Total Obs'
    WRITE(CDATA(31)(11:16),'(I6)')ITOTAL
    CDATA(32)='=================================='
    REWIND IDSUNT
    WRITE(IDSUNT,'(A80)')CHEAD
    WRITE(IDSUNT,'(A80)')CDATA
    DO I = 1,4
       PRINT*, CHEAD(I)
    END DO
    DO I = 2,31
       PRINT*, CDATA(I)
    END DO
    PRINT*, ' '
    PRINT*, ' '
    PRINT*, ' '
    CLOSE(IUNIT)
    CLOSE(IDSUNT)
199     CONTINUE
STOP
END PROGRAM STATSBUF
