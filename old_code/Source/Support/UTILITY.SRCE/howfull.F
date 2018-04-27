      PROGRAM HOWFULL
      IMPLICIT NONE

!----------------------------------------------------------------------
!
! PROGRAM       : HOWFULL
!
! PURPOSE       : To see how full certain kinds of MetDB data set are
!
! DATA TYPE(S)  : Any stored by programs which claim blocks only when
!                 needed (AIRSTO, ERSREP etc - any but TAFREP & clones)
!
! CALLS         : DATIM, ICHAR2                                     !1.7
!
! REVISION INFO :
!
! $Workfile: howfull.F$ $Folder: UTILITY.SRCE$
! $Revision: 6$ $Date: 17/08/2009 16:39:14$
!
! $Log:
!  6    Met_DB_Project 1.5         17/08/2009 16:39:14    Brian Barwell
!       Assign prefix for UKV model and delete those for obsolete FOAM models.
!  5    Met_DB_Project 1.4         22/05/2008 16:25:05    Brian Barwell
!       Assign suitable prefexes for NEMO models.
!  4    Met_DB_Project 1.3         19/12/2007 14:58:41    Brian Barwell
!       Modify check for location of map in storage data set.
!  3    Met_DB_Project 1.2         18/12/2006 12:25:03    Stan Kellett
!       Addition of 3 more models for FOAM merge.
!  2    Met_DB_Project 1.1         06/03/2006 15:26:46    Brian Barwell
!       Specify prefix for 4km Mesoscale model merged data sets.
!  1    Met_DB_Project 1.0         28/02/2006 12:07:30    Sheila Needham  
! $
! Revision 1.9  2005/03/03 09:34:09 09:34:09  usmdb (MetDB account c/o J C Ward)
! 1.9.  21 March 2005.  Brian Barwell.
! Look for data sets containing '.OPER.NAE' and set table heading.
!
! Revision 1.8  2001/10/25 11:49:34 11:49:34  usmdb (MetDB account c/o J C Ward)
! Code added for HPMDB - S.Cox
!
! Revision 1.7  2001/10/22 13:08:20  usmdb
! 1.7.  22 Oct 2001.  Brian Barwell.  Change 137/01.
! Add capability to handle new-format storage data sets, produce
! tabulated output and write to data set as well as printout.
!
! Revision 1.6  2001/02/21 09:58:50  usmdb
! Moved SSTMER IF block above GLOMER IF block as SSTMER was
! not being invoked - S.Cox
!
! Revision 1.5  2001/02/06 11:50:42  usmdb
! Added Merged SST.
! !1.5, Stan Kellett, 19 February 2001.
!
! Revision 1.4  2000/12/08  14:50:08  14:50:08  usmdb (Generic MDB account)
! Changes to allow howfull to work on HP - S.Cox
!
! Revision 1.3  2000/08/09  15:28:23  15:28:23  usmdb (Generic MDB account)
! 21 August 2000    C Long
! Count blocks with top bit set in map to indicate overflow
!
! Revision 1.2  2000/06/08  15:18:21  15:18:21  usmdb (Generic MDB account)
! 19 June 2000     C Long
! Recognise mesoscale & stratospheric merge DSNs
!
! Revision 1.1  99/07/21  12:06:02  12:06:02  usmdb (Generic MDB account)
! Initial revision
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2009 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!----------------------------------------------------------------------

! Parameter                                                        !1.7
      INTEGER    MAXIND      ! Upper limit of INDB array           !1.7
      PARAMETER (MAXIND=5000)                                      !1.7

!Declare Character
      CHARACTER*1 MAP(65535) ! Complete MAP records (old format)
      CHARACTER*8 MAPHDR     ! Map record header (old format)
      CHARACTER*2 MAPTRL     ! Map record trailer (old format)
      CHARACTER*46 DSN       ! Data set name
      CHARACTER*80 LINE      ! Line of output for unit 6           !1.7
      CHARACTER*21 PREFIX    ! Prefix for printout                 !1.7
      CHARACTER*27998 MAPREC ! Map record (new format)             !1.7
!
      INTEGER BLKSIZ         ! Record length of storage data set
      INTEGER I,J            ! Loop counters and other local use
      INTEGER ICHAR2         ! External function for C*2 --> I*4   !1.7
      INTEGER INDB(0:MAXIND) ! No. of data recs. for each index    !1.7
      INTEGER IRC            ! Return code from I/O statement
      INTEGER IS             ! Location of subtype name in DSN     !1.4
      INTEGER IX             ! Index pointer in map record
      INTEGER LDSN           ! Length of data set name             !1.4
      INTEGER MAPLEN1        ! Number of index pointers in map record
      INTEGER MAX            ! Max. number of data records per index
      INTEGER MEAN           ! Mean number of data records per index
      INTEGER NB             ! Total number of records (old format)
      INTEGER NDB            ! Number of data records
      INTEGER NINDB          ! Highest INDB element to look at     !1.7
      INTEGER NMAP           ! Number of current map record        !1.7
      INTEGER NMAPBLK        ! Total number of map records
      INTEGER NOW(8)         ! Date/time (as output from DATIM)
      INTEGER NSEQBL         ! Record number of BUFR sequence record
      INTEGER NSQ            ! =1 if BUFR sequence record; else 0
      INTEGER NUSE           ! % of data records in use
      INTEGER NXB            ! Total number of index records
      INTEGER NXM            ! Index period in minutes             !1.7

      CHARACTER HEAD*80      ! Revision information                !ST4
#if ! defined (MVS)
      CHARACTER*200   STORAGE_DIR                                  !1.8
      CHARACTER*200   FILENAME                                     !1.8
#endif

!                      Common block (for dynamic allocation only)  !1.7

      COMMON /COMAPS/ MAP, MAPREC                                  !1.7

      HEAD = '$Workfile: howfull.F$ ' //
     &       '$Revision: 6$ $Date: 17/08/2009 16:39:14$'

!-----------------------------------------------------------------------
! get ENV VAR BASE_DIR from system
!-----------------------------------------------------------------------

#if ! defined (MVS)
      CALL METDB_GETENV("STORAGE_DIR",STORAGE_DIR,I)               !1.8
#endif

! Open output file for statistics output.                          !1.7

      OPEN (10,FILE='STATS',IOSTAT=IRC)                            !1.8

! Headings for tabulated output.                                   !1.7

      CALL DATIM (NOW)                                             !1.7
      I = MOD(NOW(8),100)  ! 2-digit year                          !1.7
      WRITE (10,'(T12,A,2I2.2,A,I3.2,2(''/'',I2.2))')              !1.7
     &          'MET.D.B. STORAGE DATA SET USAGE AT ',             !1.7
     &           NOW(5), NOW(4), 'Z ON', NOW(6), NOW(7), I         !1.7
      WRITE (10,'(T12,2A)') ('--------------------------',J=1,2)   !1.7
      WRITE (10,'(/T31,A /T35,A)')                                 !1.7
     &     '% Data records   Data records/index   Index period',   !1.7
     &         'in use          Mean     Max.      (minutes)'      !1.7

! Loop round the data sets in the input list, opening each with the
! blocksize specified in the input.
! Read the map block. All the calculations will be based on the map.

! Pre-processor statement: DSN(1:1) = '/' only if platform is IBM. This
! is because on IBM , DSN names must start with a '/' to distinguish
! them from DDNAMEs                                                !1.4

#if defined (MVS)
      DSN(1:1)='/'
#endif

      IRC=0
      DO WHILE (IRC.EQ.0)

! Pre-processor statement: If platform is IBM , read in DSN(2:46),
! otherwise read in DSN(1:45)                                      !1.4

#if defined (MVS)
        READ (*,'(A45,I5)',IOSTAT=IRC) DSN(2:46),BLKSIZ
#else
        READ (*,'(A45,I5)',IOSTAT=IRC) DSN(1:45),BLKSIZ            !1.8
#endif

        LDSN=INDEX(DSN,' ')-1                                      !1.4
        IF (IRC.NE.0 .OR. LDSN.LE.4) GO TO 99                      !1.7
        MAPLEN1=BLKSIZ-LEN(MAPHDR)-LEN(MAPTRL)
!                                            Clear out INDB array  !1.7
        DO I=0,MAXIND                                              !1.7
          INDB(I) = 0                                              !1.7
        END DO                                                     !1.7

! Pre-processor statements: If platform is IBM, specify ACTION='READ'
! otherwise omit.                                                  !1.4

#if defined (MVS)
        OPEN (1,FILE=DSN(1:LDSN),ACTION='READ',form='unformatted', !1.4
     &        ACCESS='DIRECT',RECL=BLKSIZ)
#else
        I=INDEX(STORAGE_DIR,' ')-1                                 !1.8
        FILENAME=STORAGE_DIR(1:I)//'/'//DSN(1:LDSN)                !1.8
        OPEN (1,FILE=FILENAME,FORM='UNFORMATTED',                  !1.8
     &        ACCESS='DIRECT',RECL=BLKSIZ)                         !1.8
#endif

        READ (1,REC=1) MAPHDR,(MAP(I),I=1,MAPLEN1),MAPTRL

        NB = ICHAR2(MAPHDR(1:2))                                   !1.7
        IF (NB.GT.0) THEN               ! Old data set format      !1.7
          NXB = ICHAR2(MAPHDR(3:4))                                !1.7
          NXM = ICHAR2(MAPHDR(5:6))*60                             !1.7

! If the map has >12000 records look for it at the end of the data set.
! (The test on MAP(NXB+1) was added after storage data sets for    !ST4
! merged SUBSEA data were expanded beyond 12000 records but the    !ST4
! map was kept in record 1. MAP(NXB+1) is the byte for the first   !ST4
! data record and should never be 0 if the map is in record 1.)    !ST4

          IF (NB.GT.12000 .AND. MAP(NXB+1).EQ.CHAR(0)) THEN        !ST4
            NSEQBL=ICHAR(MAP(1))

            NMAPBLK=(NB+(BLKSIZ-1))/BLKSIZ
            DO J=1,NMAPBLK
              READ (1,REC=NB-NMAPBLK+J) (MAP(I),
     &              I=(J-1)*BLKSIZ+1,MIN(J*BLKSIZ,NB))
            ENDDO
          ELSE
            NSEQBL = ICHAR(MAP(NB)) ! BUFR seq. block
          ENDIF

          NSQ=0
          IF (NSEQBL.GT.0) NSQ=1

! The map byte for a data block shows which index block owns it.
! Loop round the data block bytes, counting data blocks owned by
! each index block - and those not in use (zero bytes) in INDB(0).
! (Some kinds of storage set the top bit to indicate overflow:     !1.3
!  ignore this bit. (It is clear from the number of index blocks   !1.3
!  when the bit is being used to indicate overflow.))              !1.3

          NDB=NB-1-NSQ-NXB
          DO I=NXB+1,NXB+NDB
            IX=ICHAR(MAP(I))
            IF (NXB.LT.128 .AND. IX.GT.128) IX=IX-128              !1.3
            INDB(IX)=INDB(IX)+1
          ENDDO
          NINDB = NXB + 1                                          !1.7

        ELSE                     ! New data set format             !1.7

          READ (1,REC=1) I, NB, NMAPBLK, NXB, NXM                  !1.7

          J = 2 + NMAPBLK + NXB  ! Records before 1st data record  !1.7
          NDB = NB - J           ! Number of data records          !1.7
          J = 2*J + 1            ! Start of 1st map pointer        !1.7
          NMAP = 3               ! Number of first map record      !1.7

          READ (1,REC=NMAP) MAPREC(1:BLKSIZ) ! 1st map record      !1.7

          DO I=1,NDB                                               !1.7
            IF (J.GE.BLKSIZ) THEN  ! Get next map record           !1.7
              NMAP = NMAP + 1                                      !1.7
              READ (1,REC=NMAP) MAPREC(1:BLKSIZ)                   !1.7
              J = 1                                                !1.7
            END IF                                                 !1.7

            IX = ICHAR2(MAPREC(J:J+1)) ! Pointer to index record   !1.7
            INDB(IX) = INDB(IX) + 1                                !1.7
            J = J + 2                                              !1.7
          END DO                                                   !1.7
          NINDB = NXB + NMAPBLK + 2                                !1.7
        END IF                                                     !1.7
!                                             Close MetDB data set
        CLOSE (1)                                                  !1.7

! Find the maximum data records for any index period, for comparison
! with the mean to see if one period has an unusually large total.

        MAX = 0                                                    !1.7
        DO I=1,NINDB                                               !1.7
          IF (INDB(I).GT.MAX) MAX = INDB(I)                        !1.7
        END DO                                                     !1.7

        MEAN = NINT((NDB-INDB(0))/FLOAT(NXB))  ! Mean no. of recs. !1.7
        NUSE = NINT((NDB-INDB(0))/(0.01*NDB))  ! % data blks used  !1.7

! IS is the start position of MDB subtype name. This is 6 for 1 ,
! (due to the prefix '/' of the DSN), and 5 for other systems -
! determined by checking for '/' in position 1 of the DSN. If not
! present, subtract 1 from IS=6.                                   !1.4

        IS=6                                                       !1.4
        IF (DSN(1:1).NE.'/') IS=IS-1                               !1.4

! Assign prefix for merged data sets                               !1.7

        IF (INDEX(DSN,'.OPER.GAIR').GT.0) THEN                     !ST5
          PREFIX = '       Global merged '                         !1.7
        ELSE IF (INDEX(DSN,'.OPER.NAE').GT.0) THEN                 !1.9
          PREFIX = '          NAE merged '                         !1.9
        ELSE IF (INDEX(DSN,'.OPER.MES4K').GT.0) THEN               !1.10
          PREFIX = '4km.Mesoscale merged '                         !1.10
        ELSE IF (INDEX(DSN,'.OPER.UKV').GT.0) THEN                 !ST6
          PREFIX = '          UKV merged '                         !ST6
        ELSE IF (INDEX(DSN,'.OPER.GLBLNEMO').GT.0) THEN            !ST5
          PREFIX = '  Global NEMO merged '                         !ST5
        ELSE IF (INDEX(DSN,'.OPER.GSST').GT.0) THEN                !1.7
          PREFIX = '          SST merged '                         !1.7
        ELSE                                                       !1.5
          PREFIX = ' '                                             !1.7
        END IF
!                             Output statistics for this data set  !1.4

        I = INDEX(DSN(IS:),'.') - 2                                !1.7
        IF (I.LT.0) I = 7                                          !1.7
        WRITE (10,'(T2,2A,T31,I8,I15,I9,I13)')                     !1.7
     &             PREFIX, DSN(IS:IS+I), NUSE, MEAN, MAX, NXM      !1.7
      END DO
!                                Copy statistics to printed output !1.7
  99  REWIND 10                                                    !1.7
      IRC = 0                                                      !1.7
      LINE = ' '                                                   !1.7
      DO WHILE (IRC.EQ.0)                                          !1.7
        WRITE (6,'(A)') LINE                                       !1.7
        READ (10,'(A)',IOSTAT=IRC) LINE                            !1.7
      END DO                                                       !1.7
!
      CLOSE (10)                                                   !1.7
      STOP
      END
