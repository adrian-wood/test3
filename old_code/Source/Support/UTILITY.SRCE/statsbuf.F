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
! $Revision: 2$
! $Date: 06/01/2009 13:33:12$
! $Source: /home/us0400/mdb/op/lib/utility/RCS/statsbuf.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         06/01/2009 13:33:12    Sheila Needham  Added
!        namelist input
!  1    Met_DB_Project 1.0         28/02/2006 12:07:45    Sheila Needham  
! $
! Revision 1.7  2001/10/25 11:49:51  usmdb
! Code added for HPMDB - S.Cox
!
! Revision 1.6  2001/10/02  11:16:17  11:16:17  usmdb (Generic MetDB account)
! 1.6.  2 Oct 2001.  Brian Barwell.  Change 131/01.
! Changes to array of record lengths to reflect change to list of
! data types processed.
!
! Revision 1.5  2001/03/23  10:33:10  10:33:10  usmdb (Generic MetDB account)
! 26 March 2001    C Long
! 1.5  Change WINPRO (FT52) blocksize to 27998.
!      BLKSIZ declared as integer & 2 unused variables removed.
!
! Revision 1.4  2000/03/21  16:06:00  16:06:00  usmdb (Generic MetDB account)
! 21-03-2000 Removal of BUOY stats - S.Cox
!
! Revision 1.3  2000/03/10  09:29:29  09:29:29  usmdb (Generic MDB account)
! Operational 20-03-2000
! Change in BUOY storage dataset blocksize from 15476
! to 27998 - S.Cox
!
! Revision 1.2  99/07/12  15:58:45  15:58:45  usmdb (Generic MDB account)
! Add new datatypes: UKMOSST, ESAHRWVW, ERSUWI, GOESAMW, PAOBS - remove CLIMAT
!
! 16/11/98 !1.1 Remove SATEMS
!
! 02/07/97 !C   REMOVAL OF AMDARS. NOW 23 BYTE CHAINED TO COPE WITH
!               ACARS.
!
! 22/01/97 !B   ADDITION OF DATA SUB-SUBTYPE TEMP & PILOT COMBINED.
!               ADD TOTAL FOR THIS TO TEMPS (FIXED) & PILOTS (FIXED)
!
! 18/11/96 !A   ADDITION OF DATA SUBTYPES SATEMS, TEMPS, PILOTS,
!               DROPSOND
!
! 28/11/94      ADDITION OF DATA SUBTYPES SATOBS, WINPRO, BATHY/TESAC
!               CHECK FOR BUFR DESCRIPTOR SEQUENCE IN SECOND BLOCK
!               SEND YESTERDAY'S HOURLY TOTALS TO FT06
!
! 01/11/93      DRIFTERS NEW RECORD LENGTH
!
! 16/12/91      NEW INDEX FORMAT
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! ndsn = number of datasets to check
!-----------------------------------------------------------------------

      INTEGER      BLKSIZ                                           !1.5
      PARAMETER(NDSN=5,BLKSIZ=27998)                    !1.6!1.4!1.2!A!C
      CHARACTER*(BLKSIZ) CINDX
      CHARACTER*3  CMONTH(12)
      CHARACTER*9  WKDAY(7)
      CHARACTER*23 CNTRY
      CHARACTER*80 CHEAD(4),CDATA(672)
      INTEGER*2    NBLKS,NXBLK,NXHRS,NAMHR,NTRIES,NTOR              !1.5
      INTEGER      IDT(14)
      INTEGER      DT2HRS,OBS,OBTOT,PARTS,PARTTOT
      INTEGER      RECL(NDSN)                                         !A
      INTEGER      IOS                                                !2
      INTEGER      LENQ                                               !2
      LOGICAL      LLAT,LTOR                                        !1.5
      LOGICAL      UPAIR,TEMP,PILOT,FIXED,MOBILE                      !A
      LOGICAL      PFSOUGHT,PMSOUGHT,TFSOUGHT,TMSOUGHT,DRSOUGHT       !A
      LOGICAL      PFFOUND,PMFOUND,TFFOUND,TMFOUND,DRFOUND            !A
      LOGICAL      TPFOUND                                            !B
      LOGICAL GOOD                                                   !2
#if ! defined (MVS)
      CHARACTER*8   DATYPE(NDSN)                                    !2
      CHARACTER*8   THRDLQ(NDSN)                                    !2
      CHARACTER*200 FILENAME                                        !2
      CHARACTER*200 STORAGE_DIR                                     !2
#endif

!
      INTEGER NDAY        ! Day of month for which stats are required !2
                          ! (or, if negative, days before 'today')    !2
      CHARACTER*10 DSN    ! First 2 levels of stats dataset          !2
      NAMELIST /INSTAT/ NDAY, DSN                                    !2
      DATA  NDAY, DSN /  -1,  'MDB.STATS'/                           !2

! Blocksizes for FT51, FT52 etc in order                            !1.5

      DATA RECL/27998,27998,27998,27998,27998/              !1.6!1.5!1.4

      DATA WKDAY/'Sunday','Monday','Tuesday','Wednesday','Thursday',
     &           'Friday','Saturday'/
      DATA CMONTH/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     &            'Oct','Nov','Dec'/
#if ! defined (MVS)
      DATA DATYPE/'TEMPF','TEMPM','PILOTF','PILOTM','DROPSOND'/     !1.7
      DATA THRDLQ/'UPRAIR','UPRAIR','UPRAIR','UPRAIR','UPRAIR'/     !1.7
#endif

!-----------------------------------------------------------------------
! get ENV VAR STORAGE_DIR from system
!-----------------------------------------------------------------------

#if ! defined (MVS)
      CALL METDB_GETENV("STORAGE_DIR",STORAGE_DIR,I)                !1.7
#endif

!-----------------------------------------------------------------------
! Read first two levels of statistics data set and date from
! namelist. (Negative dates refer to days before today.)
!-----------------------------------------------------------------------

      INQUIRE (FILE='FT02F001', EXIST=GOOD)                         !2
      IF (GOOD) THEN                                                !2
        OPEN (2,IOSTAT=IOS)                                         !2
        READ (2, INSTAT, IOSTAT=IOS)                                !2
        CLOSE (2)                                                   !2
      END IF                                                        !2

      LENQ = INDEX(DSN//' ',' ') - 1    ! LENGTH OF HLQ             !2
!-----------------------------------------------------------------------
! get current date/time from system
!-----------------------------------------------------------------------

      CALL DATIM(IDT)                                               !2
!                                              NDAY = days before today
      IF (NDAY.LE.0) THEN                                           !2
        CALL DATE31 (IDT(6), IDT(7), IDT(8), ICD)                   !2
        ICD = ICD + NDAY                                            !2
        CALL DATE13 (ICD, ID, IM, IY)                               !2
!                                            NDAY = actual day of month
      ELSE                                                          !2
        IM = IDT(7)                                                 !2
        IY = IDT(8)                                                 !2
        IF (NDAY.GT.IDT(6)) THEN   ! Must be last month             !2
          IM = IM - 1                                               !2
          IF (IM.EQ.0) THEN    ! Must be last year                  !2
            IM = 12                                                 !2
            IY = IY - 1                                             !2
          END IF                                                    !2
        END IF                                                      !2
        ID = NDAY                                                   !2
        CALL DATE31 (ID, IM, IY, ICD)                               !2
      END IF                                                        !2

      IWKDAY = MOD(ICD,7) + 1                                       !1.7

!-----------------------------------------------------------------------
! loop over datasets
!-----------------------------------------------------------------------

        DO 199 J1=1,NDSN
          IUNIT = 50 + J1   ! UNIT NUMBER FOR A DATA STORE            !A
          IDSUNT=20+J1      ! UNIT NUMBER FOR A STATS DATASET         !A
#if defined (MVS)
          OPEN(IDSUNT)
#else
          FILENAME=DSN(1:LENQ)//'.'//DATYPE(J1)                     !2
          OPEN(IDSUNT,FILE=FILENAME)                                !1.7
          I=INDEX(STORAGE_DIR,' ')-1                                !1.7
          FILENAME=STORAGE_DIR(1:I)//'/MDB.'//THRDLQ(J1)            !1.7
          OPEN(IUNIT,ACCESS='DIRECT',RECL=RECL(J1),FILE=FILENAME)   !1.7
#endif
          READ(IDSUNT,'(A80)')CHEAD
          UPAIR=.FALSE.
          IF (CHEAD(2)(70:70) .EQ. 'U') UPAIR=.TRUE.
          IF (UPAIR) THEN                                             !A
            TFSOUGHT=.FALSE.
            TMSOUGHT=.FALSE.
            PFSOUGHT=.FALSE.
            PMSOUGHT=.FALSE.
            DRSOUGHT=.FALSE.
            IF (CHEAD(2)(71:72) .EQ. 'TF') TFSOUGHT=.TRUE.
            IF (CHEAD(2)(71:72) .EQ. 'TM') TMSOUGHT=.TRUE.
            IF (CHEAD(2)(71:72) .EQ. 'PF') PFSOUGHT=.TRUE.
            IF (CHEAD(2)(71:72) .EQ. 'PM') PMSOUGHT=.TRUE.
            IF (CHEAD(2)(71:72) .EQ. 'DR') DRSOUGHT=.TRUE.
          ENDIF
          DO 10 I=1,32
            CDATA(I)=' '
   10     CONTINUE
          READ(IDSUNT,'(A80)')(CDATA(I),I=33,672)
#if defined (MVS)
          OPEN(IUNIT,ACCESS='DIRECT',RECL=RECL(J1),ACTION='READ')
#else
          OPEN(IUNIT,ACCESS='DIRECT',RECL=RECL(J1))                 !1.7
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
          IF (NSEQBL .GT. 0) NSQ = 1  ! BLOCK B/N MAP & INDEX
          IF (NSEQBL .EQ. 0) NSQ = 0  ! IF ZERO, NO SEQ BLOCK

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
            INDHR1 = MOD(J2+24-NAMHR,INXHRS) ! DISPLACEMENT IN SLOT   !A
            ISHR1 = ITIM1-INDHR1             ! SLOT HOUR
            IBLOCK = MOD(ISHR1/INXHRS,INXBLK)+2+NSQ  ! BLOCK NUMBER
            READ(IUNIT,REC=IBLOCK) CINDX(1:RECL(J1))
            IDAY = ICHAR(CINDX(1:1))
            IHOUR = ICHAR(CINDX(2:2))
            CALL HRS2DT(IY1,IM1,ID1,IH1,ISHR1)

!-----------------------------------------------------------------------
! check index base hour
!-----------------------------------------------------------------------

            IF (IDAY.NE.ID1.OR.IHOUR.NE.IH1) THEN
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
                IF (UPAIR) THEN                                       !A
                  TEMP=.FALSE.
                  PILOT=.FALSE.
                  FIXED=.FALSE.
                  MOBILE=.FALSE.
                  TFFOUND=.FALSE.
                  TMFOUND=.FALSE.
                  PFFOUND=.FALSE.
                  PMFOUND=.FALSE.
                  DRFOUND=.FALSE.
                  TPFOUND=.FALSE.                                     !B
                  OBS=0
                  PARTS=0
                  IF (MOD(ICHAR(CNTRY(17:17))/8, 2) .EQ. 1) THEN
                    IF (MOD(ICHAR(CNTRY(17:17))/4, 2) .EQ. 1) THEN    !B
                      TPFOUND=.TRUE.
                    ELSE
                      DRFOUND=.TRUE.
                    ENDIF
                  ELSE
                    IF (MOD(ICHAR(CNTRY(17:17))/4, 2) .EQ. 1) THEN
                      TEMP=.TRUE.
                    ELSE
                      PILOT=.TRUE.
                    ENDIF
                    IF (MOD(ICHAR(CNTRY(17:17))/2, 2) .EQ. 1) THEN
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
                  IF (TFSOUGHT .AND. TPFOUND) OBS=1                   !B
                  IF (TMSOUGHT .AND. TMFOUND) OBS=1
                  IF (PFSOUGHT .AND. PFFOUND) OBS=1
                  IF (PFSOUGHT .AND. TPFOUND) OBS=1                   !B
                  IF (PMSOUGHT .AND. PMFOUND) OBS=1
                  IF (DRSOUGHT .AND. DRFOUND) OBS=1
                  IF (OBS .EQ. 1) PARTS=ICHAR(CNTRY(12:12))           !A
                ELSE
                  OBS = ICHAR(CNTRY(12:12))
                  IF (OBS .EQ. 0) OBS = 1
                ENDIF

!-----------------------------------------------------------------------
! check for current hour
!-----------------------------------------------------------------------

                IDHR = ICHAR(CNTRY(1:1))
                IDHR = MOD(IDHR,64)
                IF (IDHR.EQ.INDHR1) THEN
                  OBTOT = OBTOT + OBS
                  IF (UPAIR) PARTTOT=PARTTOT + PARTS                  !A
                  READ(CNTRY(18:19),'(A2)')NTOR
                  ITOR=NTOR

!-----------------------------------------------------------------------
! find earliest and latest times
!-----------------------------------------------------------------------

                  IF (LTOR) THEN
                    ITORMX = ITOR
                    ITORMN = ITOR
                    LTOR = .FALSE.
                  ELSEIF (ITOR.GT.ITORMX) THEN
                    ITORMX = ITOR
                  ELSEIF (ITOR.LT.ITORMN) THEN
                    ITORMN = ITOR
                  ENDIF
                ENDIF
159           CONTINUE                   ! END OF LOOP OVER ENTRIES
              IF (OBTOT.EQ.0) THEN
                WRITE(CDATA(J27)(3:4),'(I2.2)')J2
                CDATA(J27)(12:18)='NO DATA'
              ELSE

!-----------------------------------------------------------------------
! convert times to hhmm z
!-----------------------------------------------------------------------

                IMNHR = IH1 + ITORMN/60
                IF (IMNHR.GE.24) IMNHR = IMNHR-24
                ITORMN = IMNHR*100 + MOD(ITORMN,60)
                IMXHR = IH1 + ITORMX/60
                IF (IMXHR.GE.24) IMXHR = IMXHR-24
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
      END
