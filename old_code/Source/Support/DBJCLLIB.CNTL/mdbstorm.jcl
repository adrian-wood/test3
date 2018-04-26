//M12CLMON JOB (M12,DB,WDS0BF),MDBTEAM.6953,PRTY=8,MSGCLASS=Q           00010007
// EXEC FORT2C                                                          00010912
//SYSIN DD DSN=MDB.SOURCE(ICHAR2),DISP=SHR
// EXEC FORT2CLG                                                        00010912
      PROGRAM MDBSTORM
      IMPLICIT NONE

!----------------------------------------------------------------------
!
! PROGRAM       : MDBSTORM  (storage monitor)
!
! PURPOSE       : To see how many obs (messages) are in latest indexes
!                 of data sets listed in input (any number, any order,
!                 TESCO or not).
!                 Where 'obs' follows the blocksize in the input, the
!                 output is the number of reports; otherwise it is the
!                 number of BUFR messages stored, each of these being
!                 many reports for satellite data.
!
! DATA TYPE(S)  : Any with DSNs listed in input
!
! CALLS         : DATIM, ICHAR2
!
!----------------------------------------------------------------------

      CHARACTER*8 MAPHDR     ! Map record header (old format)
      CHARACTER*7 INDHED     ! Index block header (day,hour,entries,obs)
      CHARACTER*8 TYPE       ! Data type
      CHARACTER*23 DSN       ! Data set name
      CHARACTER*3 OBS        ! ='obs' if total number of obs in index

      INTEGER AMSTAR
      INTEGER BLKSIZ         ! Record length of storage data set
      INTEGER CENDAY         ! Century day
      INTEGER CENTHR         ! Century hour
      INTEGER CENMIN         ! Century minute
      INTEGER DAY            ! Day of start of index period
      INTEGER ENTRIES        ! Number of entries for index period
      INTEGER HOUR           ! Hour of start of index period
      INTEGER I,J            ! Loop counters and other local use
      INTEGER IBL,IB         ! Block number for index
      INTEGER ICHAR2         ! External function for C*2 --> I*4
      INTEGER INDHOR
      INTEGER IRC            ! Return code from I/O statement
      INTEGER LDSN           ! Length of data set name
      INTEGER MIN            ! Minute of start of index period
      INTEGER NB             ! Total number of records (old format)
      INTEGER NDB            ! Number of data records
      INTEGER NMAP           ! Number of current map record
      INTEGER NMAPBLK        ! Total number of map records
      INTEGER NOW(8)         ! Date/time (as output from DATIM)
      INTEGER NSQ            ! =1 if sequence in 2nd block, =0 if not
      INTEGER NXB            ! Total number of index records
      INTEGER NXH            ! Index period in hours
      INTEGER NXM            ! Index period in minutes
      INTEGER NOBS           ! Number of reports for index period

      CHARACTER HEAD*132     ! Revision information

      HEAD='
     &$Source:                                                 $
     &'//'$Date: 28/02/2006 12:18:38$ $Revision: 1$'

      CALL DATIM(NOW)
      CALL DATE31(NOW(6),NOW(7),NOW(8),CENDAY)
      CENTHR=(CENDAY-1)*24+NOW(5)
      CENMIN=CENTHR*60+NOW(4)

! Loop round the data sets in the input list, opening each with the
! blocksize specified in the input & reading the map block header.
! (The input is taken from MDB.DATASETS, with some lines deleted.)

      DSN(1:1)='/'
      IRC=0
      DO WHILE (IRC.EQ.0)
        READ (*,'(1X,A8,9X,I5,6X,A22,A3)',IOSTAT=IRC)
     &        TYPE,BLKSIZ,DSN(2:),OBS
        LDSN=INDEX(DSN,' ')-1
        IF (IRC.NE.0 .OR. LDSN.LE.4) STOP
        PRINT *,' '
        PRINT *,TYPE

        OPEN (1,FILE=DSN(1:LDSN),ACTION='READ',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=BLKSIZ)

        READ (1,REC=1) MAPHDR
        NB=ICHAR2(MAPHDR(1:2))

! The number in the first 2 bytes decides the old & new data set
! formats, zero indicating TESCO.

        IF (NB.GT.0) THEN               ! Old data set format
          NXB=ICHAR2(MAPHDR(3:4))       ! number of index blocks
          NXH=ICHAR2(MAPHDR(5:6))       ! hours per index block
          AMSTAR=ICHAR2(MAPHDR(7:8))    ! shift of period start from 0Z

! Work out block number of latest index block from clock time & numbers
! in map header.  (Assume sequence record unless TAFs or METARs, NCM or
! SREW - not stored in BUFR.)

          NSQ=1
          IF (TYPE.EQ.'METARS' .OR.TYPE.EQ.'NCM' .OR.TYPE.EQ.'SREW' .OR.
     &        TYPE.EQ.'PRESTEL' .OR. INDEX(TYPE,'TAF').GT.0) NSQ=0
          INDHOR=MOD(NOW(5)+24-AMSTAR,NXH)
          IBL=MOD((CENTHR-INDHOR)/NXH,NXB)+2+NSQ

! Loop round latest & previous index block

          DO IB=IBL-40/NXH,IBL
            IF (IB.LE.1+NSQ) THEN       ! Was current index block first?
              READ (1,REC=IB+NSQ+NXB) INDHED ! If so, previous is last.
            ELSE                        ! Otherwise just IBL-1.
              READ (1,REC=IB) INDHED
            ENDIF

            DAY=ICHAR(INDHED(1:1))
            HOUR=ICHAR(INDHED(2:2))
            ENTRIES=ICHAR2(INDHED(3:4))
            NOBS=ICHAR2(INDHED(5:6))

! Print count(s) if day/hour tag matches current time, i.e. if day is
! today or yesterday.

            IF (DAY.EQ.NOW(6).OR.DAY.EQ.NOW(6)-1.OR.NOW(6).EQ.1) THEN
              IF (OBS.EQ.'obs') THEN
                WRITE (*,1) NOBS,ENTRIES,DAY,HOUR
    1 FORMAT (I7,' reports from',I5,' stations for',I3,'/',I2.2,'Z')
              ELSE
                WRITE (*,2) ENTRIES,DAY,HOUR
    2 FORMAT (I7,' messages for period starting',I3,'/',I2.2,'Z')
              ENDIF

! If tag doesn't match, then print "no data yet" for period starting...

            ELSE
              IF (IB.EQ.IBL) WRITE (*,3) MOD(CENTHR-INDHOR,24)
              IF (IB.EQ.IBL-1) WRITE (*,3) MOD(CENTHR-NXH-INDHOR,24)
    3 FORMAT (' No data yet for period starting ',I2.2,'Z')
            ENDIF
          ENDDO

! For new format (TESCO) reread map header & work out index block number

        ELSE                            ! New data set format
          READ (1,REC=1) I,NB,NMAPBLK,NXB,NXM,I

          IBL=MOD((CENMIN-I)/NXM,NXB)+3+NMAPBLK
          DO IB=IBL-1,IBL
            IF (IB.LE.2+NMAPBLK) THEN
              READ (1,REC=IB+NXB) INDHED
            ELSE
              READ (1,REC=IB) INDHED
            ENDIF

            DAY=ICHAR(INDHED(3:3))
            HOUR=ICHAR(INDHED(4:4))
            MIN=ICHAR(INDHED(5:5))
            ENTRIES=ICHAR2(INDHED(6:7))

! Print count if day/hour tag matches current time.
! For TESCO print minute as well as hour of start of index period.

            IF (DAY.EQ.NOW(6).OR.DAY.EQ.NOW(6)-1.OR.NOW(6).EQ.1) THEN
              WRITE (*,22) ENTRIES,DAY,HOUR,MIN
   22 FORMAT (I7,' messages for period starting',I3,'/',2I2.2,'Z')

! TESCO index period is minutes, not hours, so work from century-minute

            ELSE
              J = (CENMIN-I)/NXM - (IBL-IB)
              J = J*NXM + I
              HOUR = MOD(J/60,24)
              MIN = MOD(J,60)
              WRITE (*,33) HOUR,MIN
   33 FORMAT (' No data yet for period starting ',2I2.2,'Z')
            ENDIF
          ENDDO
        ENDIF
        CLOSE (1)
      ENDDO
      STOP
      END
//LKED.PR DD DSN=MET.PROGLIB,DISP=SHR
 INCLUDE PR(ZPDATE)
//GO.SYSIN DD *
 AATSR     T   F  27998   9  MDB.AATSR
 AIREP     T   F  27998  10  MDB.AIREPS
 AIRS      T   F  27998   8  MDB.AIRS
 AMDARS    T   F  27998  10  MDB.AMDARS
 AMSUB     T   F  27998   9  MDB.AMSUB
 ASARWVS   T   F  27998  11  MDB.ASARWVS
 ATAFS     T   F  23476   9  MDB.ATAFS             obs
 ATOVSG    T   F  27998  10  MDB.ATOVSG
 ATOVSL    T   F  27998  10  MDB.ATOVSL
 AVHRRG    T   F  27998  10  MDB.AVHRRG
 BOGUS     T   F  27998   9  SDB.BOGUS             obs
 BUOY      T   F  27998   8  MDB.BUOY
 ERSURA    T   F  23476  10  MDB.ERSURA
 ERSUWA    T   F  23476  10  MDB.ERSUWA
 ERSUWI    T   F  27998  10  MDB.ERSUWI
 ESACMW    T   F  27998  10  MDB.ESACMW
 ESACSR    T   F  27998  10  MDB.ESACSR
 ESACSWVW  T   F  27998  12  MDB.ESACSWVW
 ESAHRVW   T   F  27998  11  MDB.ESAHRVW
 ESAHRWVW  T   F  27998  12  MDB.ESAHRWVW
 ESAWS     T   F  27998   9  MDB.ESAWS             obs
 FOAMSST   T   F  27998  11  MDB.FOAMSST
 GOESAMW   T   F  27998  11  MDB.GOESAMW
 GOESBUFR  T   F  27998  12  MDB.GOESBUFR
 GOESVIS   T   F  27998  11  MDB.GOESVIS
 GOESWV    T   F  27998  10  MDB.GOESWV
 GPSIWV    T   F  27998  10  MDB.GPSIWV
 LNDSYN    T   F  27998  10  MDB.LNDSYN            obs
 METARS    T   F  23476  10  MDB.METARS            obs
 MODIS     T   F  27998   9  MDB.MODIS
 NCM       T   F  27998   7  MDB.NCM               obs
 OZONESAT  T   F  27998  12  MDB.OZONESAT
 PAOBS     T   F  27998   9  MDB.PAOBS
 PRESTEL   T   F  27998  11  MDB.PRESTEL
 RASS      T   F   8906   8  MDB.RASS
 SATOBS    T   F  27998  10  MDB.SATOBS
 SEAWINDS  T   F  27998  12  MDB.SEAWINDS
 SFERICS   T   F  27998  11  MDB.SFERICS
 SFLOC     T   F  27998   9  MDB.SFLOC
 SHPSYN    T   F  27998  10  MDB.SHPSYN            obs
 SREW      T   F  27998   8  MDB.SREW              obs
 SSMI      T   F  27998   8  MDB.SSMI
 SUBSEA    T   F  27998  10  MDB.SUBSEA
 TAFS      T   F  23476   8  MDB.TAFS              obs
 UKMOSST   T   F  27998  11  MDB.UKMOSST
 UPRAIR    T   F  27998  10  MDB.UPRAIR            obs
 WINDPROF  T   F  27998  10  MDB.WINPRO
