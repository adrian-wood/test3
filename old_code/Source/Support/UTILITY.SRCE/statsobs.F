       PROGRAM STATSOBS

!-----------------------------------------------------------------------
!
! PROGRAM       : STATSOBS
!
! PURPOSE       : Print observation totals in a 24-hour period.
!
! DESCRIPTION   : STATSOBS computes statistics of observations stored in
!                 the MetDB for various observation types as listed in
!                 the input on unit 5. Observation type codes should be
!                 as in MDB.DATASETS and should be listed one to a line
!                 starting in column 1. Entries after the blank line are
!                 ignored.
!
!                 The output consists of observation totals, first&last
!                 receipt times and latitude & longitude limits for each
!                 hour for the day specified in the NAMELIST input (see
!                 below). Also the total number of observations for the
!                 whole day is given.
!
!                 If "DSN" is specified in the NAMELIST, the tabulated
!                 output will be added to a statistics data set if one
!                 exists (see below for naming convention).
!
!                 STATSOBS should not be used for any data type for
!                 which the storage data set has overflow blocks or uses
!                 trailers and/or 'chaining'.
!
!                 STATSOBS reads index blocks in the storage data sets
!                 but not data blocks. It can handle old format 12- or
!                 23-byte index entries or new format index entry
!                 versions 1 and 2.
!
! NAMELIST      : INSTAT  (Unit 2).  Contents as follows:          !2
!
!                 Variable Type        Description              Default
!                 -------- ----        -----------              -------
!                  NDAY    I*4   Day of month for which stats     -1
!                                are required (or, if negative,
!                                days before 'today').
!
!                  DSN     C*10  First two levels of stats      Blanks
!                                data sets (or
!                                blanks if no data sets).
!                 Notes:
!                   - If NDAY is greater then the current day, it is
!                     assumed to apply for last month.
!                   - Statistics data sets are assumed to have names of
!                     the form "DSN.datatype" where "DSN" is as
!                     specified (e.g. MDB.STATS) and datatype is the
!                     qualifier of the name of the storage data set.
!
! CALLS         : DATE13, DATE31, DATIM, DSINFO, ICHAR2, ICHAR3
!
! HISTORY       : Original version by Brian Barwell, May 2001.
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 06/01/2009 13:41:35$
! $Source: /home/us0400/mdb/op/lib/utility/RCS/statsobs.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         06/01/2009 13:41:35    Sheila Needham
!       Changes to namelist input to allow alternate stats datasets
!  1    Met_DB_Project 1.0         28/02/2006 12:07:47    Sheila Needham  
! $
! Revision 2.1  2001/10/25 15:47:27  usmdb
! Code added for HPMDB. Correction made so stats will still
! work if an index block is exactly full - S.Cox/B.Barwell
!
! Revision 2.0  2001/10/02 11:20:31  usmdb
! Initial Revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
      INTEGER I, J        ! General variables for local use
      INTEGER IBOX        ! Offset for start of lat/lon box in entry
      INTEGER ICD         ! Century day for statistics date
      INTEGER IDT(8)      ! Current date and time (from DATIM)
      INTEGER IFIRST(0:23)! Receipt time for earliest ob. each hour
      INTEGER IHHMM1      ! Earliest observation time in 'hhmm' format
      INTEGER IHHMM2      ! Latest observation time in 'hhmm' format
      INTEGER IHOUR       ! Offset for hour in index entry
      INTEGER ILAST(0:23) ! Receipt time for latest ob. each hour
      INTEGER IOBS        ! Number of observations (from index entry)
      INTEGER IOS         ! Status from I/O statements
      INTEGER IPBOX       ! Location of lat/lon info in current index
      INTEGER IPT         ! Pointer to end of last index entry processed
      INTEGER IPTIM1      ! First index time to look at
      INTEGER IPTIM2      ! Last index time to look at
      INTEGER IT(5)       ! Index time (year, month, day, hour, minute)
      INTEGER ITIM1,ITIM2 ! Century mins for 0000Z & 2359Z on stats date
      INTEGER ITOR        ! Receipt time mins (from index time, then 0Z)
      INTEGER IVERSN      ! Storage d/s. version nos. (new format only)
      INTEGER IWKDAY      ! Day of week (1=Sunday etc.) for stats date
      INTEGER IXBLK       ! Current index block number
      INTEGER IY, IM, ID  ! Year, month and day of statistics date
      INTEGER JENTRY      ! Loop variable for loop over index entries
      INTEGER JHOUR       ! Loop variable for loop over hours (0-23)
      INTEGER JPTIM       ! Loop variable for index periods (cent.mins.)
      INTEGER KODE        ! Return code from DSINFO
      INTEGER LATN, LATS  ! North and south limits for index entry
      INTEGER LENQ        ! Length of first two levels of stats dsn  !2
      INTEGER LENREC      ! Record length of storage data set
      INTEGER LENTRY      ! Index entry length for storage data set
      INTEGER LINE        ! Subscript for CDATA
      INTEGER LONE, LONW  ! East and west limits for index entry
      INTEGER MAXLAT(0:23)! Maximum observation latitude for each hour
      INTEGER MAXLON(0:23)! Maximum observation longitude for each hour
      INTEGER MAXNDX      ! Maximum number of entries in 1 index block
      INTEGER NBLKS       ! Total number of blocks in storage data set
      INTEGER MINLAT(0:23)! Minimum observation latitude for each hour
      INTEGER MINLON(0:23)! Minimum observation longitude for each hour
      INTEGER NHOUR       ! Observation century hour
      INTEGER NSQ         ! Offset for block numbering (old format only)
      INTEGER NTOTAL      ! Number of observations (all hours)
      INTEGER NTRIES      ! Number of entries in current index block
      INTEGER NUMMAP      ! Number of map blocks in storage data set
      INTEGER NUMOBS(0:23)! Number of observations for each hour
      INTEGER NXBLKS      ! Number of index blocks in storage data set
      INTEGER NXHEAD      ! Index block header length
      INTEGER NXMIN       ! Index period in minutes
      INTEGER NX00Z       ! Index period offset from 00Z
      INTEGER NX1         ! Block number of first index block
#if ! defined (MVS)
      INTEGER NFT                                                   !2.1
      INTEGER DTPATHLEN                                             !2.1
#endif

      LOGICAL ACFT           ! Flag for aircraft (AIREP or AMDAR) data
      LOGICAL GOOD           ! 'So far so good' flag
      LOGICAL LATE           ! Flag for T.O.R. outside limit in index
      LOGICAL LATEOB(2,0:23) ! Flag for unreliable IFIRST and ILAST
      LOGICAL OLDFMT         ! Flag for storage data set in old format
      LOGICAL STATOPEN       ! Flag to indicate if stats d/s is open

      CHARACTER*80  CDATA(672) ! Stats data set contents after CHEAD
      CHARACTER*80  CHEAD(4)   ! First 4 lines of stats data set
      CHARACTER*3   CMONTH(12) ! Abbreviated (3-letter) names of months
      CHARACTER*132 HEAD       !  For revision information
      CHARACTER*5   INDXTAG    ! 5-character tag time from index block
      CHARACTER*27998 RECORD   ! Buffer to hold 1 record of storage d/s.
      CHARACTER*200 STATNAME   ! Name of statistics data set
      CHARACTER*200 STORNAME   ! Name of storage data set
      CHARACTER*5   TIMTAG     ! 5-character index tag time (ymdhm)
      CHARACTER*8   TYPE       ! MetDB code for data type
      CHARACTER*9   WKDAY(7)   ! Names of days of the week
#if ! defined (MVS)
      CHARACTER*8 DT                                                !2.1
#endif
!
!                                                    External functions
!
      INTEGER ICHAR2      ! Converts 2-byte character to integer
      INTEGER ICHAR3      ! Converts 3-byte character to integer
!
!                                                              NAMELIST
!
      INTEGER NDAY        ! Day of month for which stats are required
                          ! (or, if negative, days before 'today')
      CHARACTER*10 DSN    ! First two levels of stats dataset name  !2
      NAMELIST /INSTAT/ NDAY, DSN                                   !2
      DATA  NDAY, DSN /  -1,  ' '/                                  !2
!                                                   Data initialisation
!
      DATA WKDAY  /'Sunday   ', 'Monday   ', 'Tuesday  ', 'Wednesday',
     &             'Thursday ', 'Friday   ', 'Saturday '/
      DATA CMONTH /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     &             'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
      DATA IVERSN /0/
!                                                  Revision information
      HEAD='$RCSfile: statsobs.F,v $ ' //
     &     '$Revision: 2$ $Date: 06/01/2009 13:41:35$'
!
!-----------------------------------------------------------------------
! Read high level qualifier of statistics data set and date from
! namelist. (Negative dates refer to days before today.)
!-----------------------------------------------------------------------

      INQUIRE (FILE='FT02F001', EXIST=GOOD)
      IF (GOOD) THEN
#if defined (MVS)
        OPEN (2,IOSTAT=IOS)
#else
        OPEN (2,FILE='FT02F001',IOSTAT=IOS)                         !2.1
#endif
        READ (2, INSTAT, IOSTAT=IOS)
        CLOSE (2)
      END IF

      LENQ = INDEX(DSN//' ',' ') - 1    ! Length of DSN             !2

!-----------------------------------------------------------------------
! Get century day of date for which statistics are required
!-----------------------------------------------------------------------

      CALL DATIM (IDT)  ! Today's date

!                                              NDAY = days before today
      IF (NDAY.LE.0) THEN
        CALL DATE31 (IDT(6), IDT(7), IDT(8), ICD)
        ICD = ICD + NDAY
        CALL DATE13 (ICD, ID, IM, IY)
!                                            NDAY = actual day of month
      ELSE
        IM = IDT(7)
        IY = IDT(8)
        IF (NDAY.GT.IDT(6)) THEN   ! Must be last month
          IM = IM - 1
          IF (IM.EQ.0) THEN    ! Must be last year
            IM = 12
            IY = IY - 1
          END IF
        END IF
        ID = NDAY
        CALL DATE31 (ID, IM, IY, ICD)
      END IF
!                                                       Get day of week
      IWKDAY = MOD(ICD,7) + 1
!
!=======================================================================
!                    LOOP OVER MET.D.B. DATA TYPES
!=======================================================================
!
      TYPE = 'NOTBLANK'
      DO WHILE (TYPE.NE.' ')
        GOOD = .TRUE.
!                                             Read data type from input
        READ (5,'(A)',IOSTAT=IOS) TYPE
        IF (IOS.NE.0) TYPE = ' '
        IF (TYPE.EQ.' ') GOOD = .FALSE.
!
!                           Locate storage data set & check return code
        IF (GOOD) THEN
#if defined (MVS)
          CALL DSINFO (TYPE, 1, 10, LENREC, KODE, STORNAME)
          IF (KODE.NE.1) THEN ! Problem
            WRITE (6,'(/T3,3A,I3)') 'STATSOBS:  Data type ',TYPE,
     &               ' Return code from DSINFO is', KODE
            GOOD = .FALSE.
          END IF
#else
          OPEN(15,FILE='HPMDB_datasets',FORM='FORMATTED')           !2.1
          READ(15,*)                                                !2.1
          READ(15,*)                                                !2.1
          IOS = 0                                                   !2.1
          DO WHILE (IOS.EQ.0)                                       !2.1
            READ(15,'(A8,2X,I6,2(2X,I3),2X,A100)')DT,LENREC,NFT,    !2.1
     &      DTPATHLEN,STORNAME                                      !2.1
            IF (DT.EQ.TYPE) THEN                                    !2.1
              OPEN(10,FILE=STORNAME(1:DTPATHLEN),ACCESS='DIRECT',   !2.1
     &             RECL=LENREC,IOSTAT=IOS)                          !2.1
              IF (IOS.NE.0) THEN                                    !2.1
                WRITE(6,*)'STATSJOB: ERROR OPENING DATASET ',       !2.1
     &          STORNAME(1:DTPATHLEN)                               !2.1
                GOOD = .FALSE.                                      !2.1
              ENDIF                                                 !2.1
              IOS = 100  !- subtype matched. Exit loop.             !2.1
            ENDIF                                                   !2.1
            IF (DT.EQ.'END') IOS = 200  !- stop at end of file.     !2.1
          ENDDO                                                     !2.1
          CLOSE(15)                                                 !2.1
#endif
        END IF
!                                               Look for stats data set
        STATOPEN = .FALSE.
        IF (GOOD) THEN
          IF (DSN(1:1).NE.' ') THEN                                  !2
!                                               Get stats data set name
            STATNAME = DSN                                           !2
#if defined (MVS)
            J = INDEX(STORNAME,'.')            ! 1st dot in STORNAME
            IF (J.GT.0) THEN
              STATNAME = DSN(1:LENQ) // STORNAME(J:38)               !2
!
!                                               Check whether it exists
!
              INQUIRE (FILE='/'//STATNAME, EXIST=STATOPEN)
            END IF
#else
            STATNAME = DSN(1:LENQ) // '.' // TYPE                   !2
            INQUIRE (FILE=STATNAME, EXIST=STATOPEN)                 !2.1
#endif
          END IF
!                                          If stats data set exists ...
          IF (STATOPEN) THEN
!                                                           ... open it
#if defined (MVS)
            OPEN (20, FILE='/'//STATNAME, IOSTAT=IOS)
#else
            OPEN (20, FILE=STATNAME, IOSTAT=IOS)                    !2.1
#endif
!                                                     Check status code
            IF (IOS.NE.0) THEN
              WRITE (6,'(/T3,4A)') 'STATSOBS:  Data type ',TYPE,
     &               ' Error opening ', STATNAME
              GOOD = .FALSE.
!                                                  If OK, read the data
            ELSE
              READ (20,'(A)') CHEAD, (CDATA(I),I=33,672)
            END IF
          ELSE
!                                 Make titles if data set doesn't exist
            I = INDEX(TYPE,' ')
            IF (I.EQ.0) I = 9
            CHEAD(1) = ' '
            WRITE (CHEAD(1),'(T19,23A1)') ('=', J=1,I+14)
            WRITE (CHEAD(2),'(T19,2A)') 'MDB TOTALS FOR ', TYPE
            CHEAD(3) = CHEAD(1)
            CHEAD(4) = ' '
          END IF
        END IF
!
        IF (GOOD) THEN
!                                         Table headings for statistics
          CDATA(1) =
     &       '========================================================='
          WRITE (CDATA(2)(:),'(A,I3.2,A4,I5.4)')
     &           WKDAY(IWKDAY), ID, CMONTH(IM), IY
          I = INDEX(STORNAME,' ')
#if defined (MVS)
          CDATA(2)(59-I:58) = STORNAME(1:I)
#else
          CDATA(2)(50:) = TYPE                                      !2.1
#endif
          CDATA(3) =
     &       '---------------------------------------------------------'
          CDATA(4) =
     &       'Hours   Observations   Receipt (Z)   Latitude   Longitude'
          CDATA(5) =
     &       '                       First  Last   Min  Max    Min  Max'
          CDATA(6) = CDATA(3)
          CDATA(32)= CDATA(1)
!
!-----------------------------------------------------------------------
! Read first block of storage data set
!-----------------------------------------------------------------------
!
          READ (10,REC=1) RECORD(1:LENREC)
          I = ICHAR2(RECORD(1:2))
!                                                 I=0: New format
          IF (I.EQ.0) THEN
            READ (RECORD(1:28),'(7A4)')
     &            IVERSN, NBLKS, NUMMAP, NXBLKS, NXMIN, NX00Z, LENTRY
!
            IF (IVERSN.EQ.257) THEN      ! Index entry version 1
              IHOUR = 3                  ! Index byte containing hour
              IBOX = 9                   ! Offset for lat/lon box
            ELSE IF (IVERSN.EQ.258) THEN ! Index entry version 2
              IHOUR = 10                 ! Index byte containing hour
              IBOX = 16                  ! Offset for lat/lon box
            ELSE
              WRITE (6,'(/T5,A,I6)') 'INVALID VERSION NUMBER -', I
              GOOD = .FALSE.
            END IF
!
            OLDFMT = .FALSE.             ! Not old data set format
            NXHEAD = 10                  ! Index block header length
            NX1 =  NUMMAP + 3            ! Number of first index block
            MAXNDX = (LENREC-NXHEAD)/LENTRY  ! Max. entries per block
!
!                                                 I>0: Old format
          ELSE
            NBLKS  = I                         ! Total no. of blocks
            NXBLKS = ICHAR2(RECORD(3:4))       ! No. of index blocks
            NXMIN  = ICHAR2(RECORD(5:6))*60    ! Index period (minutes)
            NX00Z  = MOD(ICHAR2(RECORD(7:8))*60,NXMIN) ! Index offset
            LENTRY = ICHAR2(RECORD(LENREC-1:LENREC))   ! Entry length
!
!                   Note: the index entry length is missing from ERSUAT
!                      and ERSUWA data sets for which the length is 12.
!
            IF (LENTRY.EQ.0) LENTRY = 12
!                                       Get number of first index block
!                             (=3 if sequence record exists, =2 if not)
!
            IF ((NBLKS.LE.12000 .AND.
     &           ICHAR(RECORD(NBLKS+8:NBLKS+8)).GT.0) .OR.
     &          (NBLKS.GT.12000 .AND. ICHAR(RECORD(9:9)).GT.0)) THEN
              NX1 = 3
            ELSE
              NX1 = 2
            END IF

            NSQ = NX1 - 2          ! Offset for block numbers in index
            OLDFMT = .TRUE.        ! Old data set format
            NXHEAD = 6             ! Index block header length
            IF (LENTRY.EQ.12) THEN
              IHOUR = 2            ! Index byte containing hour
              IBOX = 5             ! Offset for start of lat/lon box
            ELSE
              IHOUR = 1            ! Index byte containing hour
              IBOX = 13            ! Offset for start of lat/lon box
            END IF
            MAXNDX = (LENREC-NXHEAD-2)/LENTRY  ! Index block capacity
          END IF
        END IF
!
        IF (GOOD) THEN
!                            AIREP & AMDAR need special treatment later
!
          ACFT = TYPE(1:5).EQ.'AIREP' .OR. TYPE(1:5).EQ.'AMDAR'
!
!-----------------------------------------------------------------------
! Initialise hourly arrays and index times for loop
!-----------------------------------------------------------------------
!
          DO JHOUR=0,23
            NUMOBS(JHOUR) =  0        ! Number of observations
            IFIRST(JHOUR) =  9999     ! Earliest receipt time
            ILAST(JHOUR)  = -9999     ! Latest receipt time
            MINLAT(JHOUR) =  9999     ! Minimum latitude
            MAXLAT(JHOUR) = -9999     ! Maximum latitude
            MINLON(JHOUR) =  9999     ! Minimum longitude
            MAXLON(JHOUR) = -9999     ! Maximum longitude
            LATEOB(1,JHOUR) = .FALSE. ! Flag for unreliable IFIRST
            LATEOB(2,JHOUR) = .FALSE. ! Flag for unreliable ILAST
          END DO
          NTOTAL = 0                  ! Total number of observations
!
          ITIM1 = 1440*(ICD-1)   ! Century minutes for 0000Z
          ITIM2 = ITIM1 + 1439   ! Century minutes for 2359Z
!
          IPTIM1 = ITIM1 - MOD((ITIM1-NX00Z),NXMIN)  ! First index time
          IPTIM2 = ITIM2 - MOD((ITIM2-NX00Z),NXMIN)  ! Last index time
!
!=======================================================================
!         LOOP OVER INDEX PERIODS COVERING STATISTICS DATE
!=======================================================================
!
          DO JPTIM=IPTIM1,IPTIM2,NXMIN
!                                                   Index date and time
!
            CALL DATE13 (JPTIM/1440+1, IT(3), IT(2), IT(1))
            IT(4) = MOD(JPTIM/60,24)  ! Hour
            IT(5) = MOD(JPTIM,60)     ! Minute
!                                                  5-character time tag
            TIMTAG = CHAR(MOD(IT(1),100)) //
     &         CHAR(IT(2)) // CHAR(IT(3)) // CHAR(IT(4)) // CHAR(IT(5))
!
!                                 Find base index block for this period
!
            IXBLK = MOD((JPTIM-NX00Z)/NXMIN,NXBLKS) + NX1
!
!                     Check for old data (IXBLK=0 stops any processing)
!
            READ (10,REC=IXBLK) INDXTAG
            IF ((.NOT.OLDFMT.AND.INDXTAG.NE.TIMTAG) .OR.
     &          (OLDFMT.AND.INDXTAG(1:2).NE.TIMTAG(3:4))) IXBLK = 0
!
!=======================================================================
!           LOOP OVER INDEX BLOCKS FOR THIS INDEX PERIOD
!=======================================================================
!
            DO WHILE (IXBLK.GT.0)
!                                                  Read new index block
              READ (10,REC=IXBLK) RECORD(1:LENREC)
!                                                 Get number of entries
              IF (OLDFMT) THEN
                I = NXHEAD + MAXNDX*LENTRY
                IF (ICHAR2(RECORD(I+1:I+2)).NE.0) THEN ! Block full
                  NTRIES = MAXNDX
                ELSE
                  NTRIES = MOD(ICHAR2(RECORD(3:4))-1,MAXNDX)+1      !2.1
                END IF
              ELSE
                NTRIES = ICHAR2(RECORD(6:7))
              END IF
!
!=======================================================================
!             LOOP OVER INDEX ENTRIES IN INDEX BLOCK
!=======================================================================
!
              DO JENTRY=1,NTRIES
                IPT = NXHEAD + (JENTRY-1)*LENTRY ! End of previous entry
!
!                                          Get observation century hour
                IF (OLDFMT) THEN
                  NHOUR = MOD(ICHAR(RECORD(IPT+IHOUR:IPT+IHOUR)),32) +
     &                    JPTIM/60
                ELSE
                  NHOUR = ICHAR(RECORD(IPT+IHOUR:IPT+IHOUR))
                  IF (NHOUR.LT.IT(4)) NHOUR = NHOUR + 24
                  NHOUR = 24*(JPTIM/1440) + NHOUR
                END IF
!                                                Convert to actual hour
                NHOUR = NHOUR - 24*(ICD-1)
!
!                                  Special treatment for aircraft data:
!                                Skip entry if zero data record pointer
                IF (ACFT) THEN
                  IF (ICHAR2(RECORD(IPT+22:IPT+23)).EQ.0) NHOUR = -1
                END IF
!                                          Check for hour in range 0-23
!
                IF (NHOUR.GE.0 .AND. NHOUR.LE.23) THEN
!
!                               Extract number of obs. and receipt time
                  LATE = .FALSE.
!                                           Old format
                  IF (OLDFMT) THEN
                    IF (LENTRY.EQ.12) THEN
                      IOBS = MOD(ICHAR2(RECORD(IPT+3:IPT+4)),1024)
                      ITOR = 3*ICHAR(RECORD(IPT+9:IPT+9))
                      IF (ITOR.GE.765) LATE = .TRUE.    ! (765=3*255)
                    ELSE
                      IOBS = ICHAR (RECORD(IPT+12:IPT+12))
                      ITOR = ICHAR2(RECORD(IPT+18:IPT+19))
                      IF (ITOR.GT.50000) ITOR = 65536 - ITOR
!
!                             SATOBS: Number of obs can be >255, so can
!                                  overflow into byte 11 of index entry
!
                      IF (TYPE(1:5).EQ.'SATOB') THEN
                        IOBS = IOBS +
     &                         256*MOD(ICHAR(RECORD(IPT+11:IPT+11)),16)
!
!                          AIREPs and AMDARs: count only 1 ob per entry
!
                      ELSE IF (ACFT) THEN
                        IOBS = 1
                      END IF
                    END IF
!                                           New format
                  ELSE ! New format
                    IF (IVERSN.EQ.257) THEN
                      IOBS = ICHAR2(RECORD(IPT+13:IPT+14))
                      ITOR = ICHAR2(RECORD(IPT+18:IPT+19))
                    ELSE
                      IOBS = ICHAR2(RECORD(IPT+20:IPT+21))
                      ITOR = ICHAR2(RECORD(IPT+25:IPT+26))
                    END IF
                  END IF
!                                                 Update number of obs.
!
                  NUMOBS(NHOUR) = NUMOBS(NHOUR) + IOBS
                  NTOTAL = NTOTAL + IOBS
!                                            Update receipt time limits
!
                  ITOR = ITOR + JPTIM - ITIM1   ! Minutes since 00Z
!
                  IF (ITOR.LT.IFIRST(NHOUR)) THEN  ! Earliest so far
                    IFIRST(NHOUR) = ITOR
                    IF (LATE) LATEOB(1,NHOUR) = .TRUE.
                  END IF
!
                  IF (ITOR.GT.ILAST(NHOUR)) THEN   ! Latest so far
                    ILAST(NHOUR) = ITOR
                    IF (LATE) LATEOB(2,NHOUR) = .TRUE.
                  END IF
!                                        Extract lat/lon box boundaries
                  IPBOX = IPT + IBOX
                  IF ((OLDFMT .AND. LENTRY.EQ.23 .AND.
     &                (IOBS.EQ.1 .AND. TYPE(1:6).NE.'SFERIC')) .OR.
     &                (IVERSN.EQ.258 .AND. IOBS.EQ.1)) THEN ! 1 Lat/lon
                    LATS = ICHAR2(RECORD(IPBOX  :IPBOX+1))
                    LONW = ICHAR2(RECORD(IPBOX+2:IPBOX+3))
!
!             Allow for funny missing data (Hex 3700 = 14080) in SFLOCs
!
                    IF (TYPE(1:5).EQ.'SFLOC' .AND.
     &                  (LATS.EQ.14080 .OR. LONW.EQ.14080)) THEN
                      LATS = -32768
                      LONW = -32768
                    END IF
!
                    IF (LATS.GE.32768) LATS = LATS - 65536
                    IF (LONW.GE.32768) LONW = LONW - 65536
                    LATN = (LATS+ 9099)/100 -  90
                    LATS = (LATS+ 9000)/100 -  90
                    LONE = (LONW+18099)/100 - 180
                    LONW = (LONW+18000)/100 - 180
                  ELSE                                    ! lat/lon box
                    LATS =   ICHAR(RECORD(IPBOX  :IPBOX  )) -  90
                    LATN =   ICHAR(RECORD(IPBOX+1:IPBOX+1)) -  90
                    LONW = 2*ICHAR(RECORD(IPBOX+2:IPBOX+2)) - 180
                    LONE = 2*ICHAR(RECORD(IPBOX+3:IPBOX+3)) - 180
                  END IF
!                                     Check for missing lat and/or long
!                            (-32768 in entry converts to -326 OR -327)
!
                  IF (LATN.NE.-326 .AND. LATS.NE.-327 .AND.
     &                LONE.NE.-326 .AND. LONW.NE.-327) THEN ! OK
!
!                                           Check for dateline crossing
                    IF (LONW.GT.LONE) THEN
                      LONW = -180
                      LONE = 180
                    END IF
!                             Update current max and min lats and longs
!
                    MAXLAT(NHOUR) = MAX0(MAXLAT(NHOUR),LATN)
                    MINLAT(NHOUR) = MIN0(MINLAT(NHOUR),LATS)
                    MAXLON(NHOUR) = MAX0(MAXLON(NHOUR),LONE)
                    MINLON(NHOUR) = MIN0(MINLON(NHOUR),LONW)
                  END IF
                END IF
              END DO ! JENTRY  -  End of loop over index entries
!
!                                                  Get next index block
              IF (NTRIES.EQ.MAXNDX) THEN
                IF (OLDFMT) THEN
                  I = NXHEAD + MAXNDX*LENTRY
                  IXBLK = ICHAR2(RECORD(I+1:I+2))
                  IF (IXBLK.GT.0) IXBLK = IXBLK + NSQ
                ELSE
                  IXBLK = ICHAR3(RECORD(8:10))
                END IF
              ELSE
                IXBLK = 0
              END IF
            END DO !   End of loop over index blocks for this period
          END DO ! JPTIM  -  End of loop over index periods
!
!                                                Close storage data set
          CLOSE (10)
!
!=======================================================================
!         GENERATE PRINTED OUTPUT AND UPDATE STATS DATA SET
!=======================================================================
!
!                                                       Loop over hours
          DO JHOUR=0,23
            LINE = JHOUR + 7
            CDATA(LINE) = ' '
!                                                 No data for this hour
            IF (NUMOBS(JHOUR).EQ.0) THEN
              WRITE (CDATA(LINE),'(I4.2,I12)') JHOUR, NUMOBS(JHOUR)
            ELSE
!                                               Some data for this hour
              I = MOD(IFIRST(JHOUR),1440)
              IHHMM1 = I + 40*(I/60)        ! Earliest T.O.R.
              I = MOD(ILAST(JHOUR),1440)
              IHHMM2 = I + 40*(I/60)        ! Latest T.O.R.
!
              WRITE (CDATA(LINE),'(I4.2,I12,4X,2(3X,I4.4),I6,I5,I7,I5)')
     &               JHOUR, NUMOBS(JHOUR), IHHMM1, IHHMM2,
     &               MINLAT(JHOUR), MAXLAT(JHOUR),
     &               MINLON(JHOUR), MAXLON(JHOUR)
!
              IF (IABS(MINLAT(JHOUR)).GT.90) CDATA(LINE)(37:40) = ' ---'
              IF (IABS(MAXLAT(JHOUR)).GT.90) CDATA(LINE)(42:45) = ' ---'
              IF (IABS(MINLON(JHOUR)).GT.180)CDATA(LINE)(49:52) = ' ---'
              IF (IABS(MAXLON(JHOUR)).GT.180)CDATA(LINE)(54:57) = ' ---'
!
              IF (LATEOB(1,JHOUR)) CDATA(LINE)(23:23) = '>'
              IF (LATEOB(2,JHOUR)) CDATA(LINE)(30:30) = '>'
            END IF
!
            WRITE (CDATA(31),'(A,I11,64X)') 'Total', NTOTAL
          END DO ! JHOUR  -  End loop over hours
!                                                 Update stats data set
          IF (STATOPEN) THEN
            REWIND 20
            WRITE (20,'(A80)') CHEAD, CDATA
          END IF
!                                                        Printed output
!
          WRITE (6,'(///(T2,A80))') CHEAD, (CDATA(LINE),LINE=2,31)
        END IF
!                     Close stats data set and end loop over data types
!
        IF (STATOPEN) CLOSE (20)
      END DO
!                                                        All finished !
      STOP
      END
