      PROGRAM STATSMET

!-----------------------------------------------------------------------
!
! PROGRAM       : STATSMET
!
! PURPOSE       : PRINT 24 HOUR TOTALS FOR MDB 23 BYTE CHAINED D/S
!
! DESCRIPTION   : PRINTS OUT THE TOTALS FOR EACH DATA-TYPE SPECIFIED
!               : BELOW
!
! DATA TYPES    : TAFS, METARS, LNDSYN, SHPSYN, SREW, NCM, ESAWS,
!               : PRESTEL, TBUS, BOGUS, ATAFS, MOBSYN                !3
!
! DATA READ IN  : MDB.{DATATYPE}  TO GET MAP BLOCK AND INDEX BLOCKS !1.4
!
! NAMELIST      : INSTAT  (Unit 2).  Contents as follows:            !4
!
!                 Variable Type        Description              Default
!                 -------- ----        -----------              -------
!                  NDAY    I*4   Day of month for which stats     -1
!                                are required (or, if negative,
!                                days before 'today').
!                  DSN     C*10  First two levels of the stats dsns
!                                                          'MDB.STATS'
!
!
! CALLS         : STATSUB
!
! I/O           : UNITS 21 TO 20+NDSN   (STATS DATASETS) !4
!               : UNITS 51 TO 50+NDSN   (DATA STORES)
!
! REVISION INFO :
!
! $Workfile: statsmet.F$ $Folder: UTILITY.SRCE$
! $Revision: 4$ $Date: 06/01/2009 13:23:25$
!
! CHANGE RECORD :
!
! $Log:
!  4    Met_DB_Project 1.3         06/01/2009 13:23:25    Sheila Needham  Added
!        namelist input
!  3    Met_DB_Project 1.2         03/04/2008 14:20:01    Brian Barwell   New
!       (12th) data type added - MOBSYN.
!  2    Met_DB_Project 1.1         25/10/2007 10:57:39    Brian Barwell
!       HEALTHRR and PLAINOB removed from array DATYPE and corresponding
!       values from IBLK. IDSN reduced to 11. Big arrays put in common block.
!  1    Met_DB_Project 1.0         28/02/2006 12:07:46    Sheila Needham  
! $
! Revision 1.5  2001/10/25 11:50:19  usmdb
! Code added for HPMDB - S.Cox
!
! Revision 1.4  2001/10/02  11:16:39  11:16:39  usmdb (Generic MetDB account)
! 1.4.  2 Oct 2001.  Brian Barwell.  Change 131/01.
! Changes to array of record lengths to reflect change to list of
! data types processed.
!
! Revision 1.3  2000/03/21  16:06:24  16:06:24  usmdb (Generic MetDB account)
! 21-03-2000 Addition of BUOY stats - S.Cox
!
! 02/10/98  Add extra data types: TROPADV ESAWS HEALTHRR PLAINOB    !1.1
!           PRESTEL SFERICS SFLOC TBUS TRACKOB BOGUS GOESAMW CLIMAT !1.1
!           ATAFS                                                   !1.1
!
! 05/01/98  SPLIT INTO STATSMET AND STATSUB(T12JH)
!
! 02/07/97  AMDARS/ACARS MOVED IN FROM 23 BYTE UNCHINED!
!
! 18/11/96  SHIPS ADDED
!
! 29/08/96  allow for chains crossing from one block to another,
!           correct mask for sort, improve indentation
!
! 22/07/96  AIREP DATATYPE INCLUDED
!
! 31/10/95  NCM AND SREW DATA TYPES INCLUDED. JSL82 (ASSEMBLER)
!           REPLACED WITH SORTCH(FORTRAN)(t12jl)
!
! 12/10/95  SYNOPS INCLUDED IN SUMMATIONS AND OUTPUT,OVERFLOWS
!           FOR TAFS/METARS/SYNOPS ALSO INCLUDED IN OUTPUT. (t12jl)
!
! 28/11/94  SEND HOURLY TOTALS FOR YESTERDAY TO FT06
!
! 20/04/94  MAJOR CHANGES IN THE SUMMATIONS  AND OUTPUT
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER ITORMN(0:23)   !1ST TIME OF RECEIPT
      INTEGER ITORMX(0:23)   !last time of receipt
      INTEGER ITOTAL         !total number of receipts
      INTEGER LATERP(0:23)  !number of late reports
      INTEGER NREP(0:23)     !number of reports

      INTEGER*2 OFLOWS     !NUMBER OF OVER FLOWS

      INTEGER BLKSIZ  !Block size for datatype
      INTEGER IUNIT  !data store unit numbers
      INTEGER IDSUNT !data output for stats
      INTEGER I
      INTEGER ID  ! date
      INTEGER IM  ! month
      INTEGER IY  ! year
      INTEGER J2  !hours
      INTEGER J27 !hours+7 = no.lines down for output data
      INTEGER LF
      INTEGER J1   !loop over number of datasets

      INTEGER MAXOFL
      INTEGER MAXNID
      INTEGER NDSN  !number of datasets
      INTEGER INDHED
      INTEGER IOS                                                    !4
      INTEGER LENQ                                                   !4

      PARAMETER (NDSN=12,INDHED=6,MAXOFL=7,MAXNID=1020)              !3

      INTEGER IBLK(NDSN)

      INTEGER IDT(14)
      INTEGER ICD
      INTEGER NFREE
      INTEGER*2 BLOCKS
      INTEGER   IWKDAY
      INTEGER*2 RECLEN(999)  !array of report length slots after header
      INTEGER*2 RECDIS(999)

      CHARACTER*80 CHEAD(4)
      CHARACTER*80 CDATA(672)
      CHARACTER*27998 BLOCK

      LOGICAL GOOD                                                 !4

!-----------------------------------------------------------------------
! ndsn is the number of data types
! laterp counts reports received later than 18hrs after t.o.r.
! late is flag to say current report is late
! reclen is the array of report length slots after the block header.
! its dimension is arbitrary, it only needs to be big enough for the
! reports that will fit in a block.  we do not know where the lengths
! will end and the reports, slotted in from the end, will meet them!
!-----------------------------------------------------------------------

      CHARACTER*3     CMONTH(12)
      CHARACTER*9     WKDAY(7)

      CHARACTER*23    INDEKS(MAXOFL*MAXNID)      !index entries
      CHARACTER*80    HEAD                       !Revision info      !2
#if ! defined (MVS)
      CHARACTER*8     DATYPE(NDSN)                                  !1.5
      CHARACTER*200   FILENAME                                      !1.5
      CHARACTER*200   STORAGE_DIR                                   !1.5
#endif
!
      INTEGER NDAY        ! Day of month for which stats are required!4
                          ! (or, if negative, days before 'today')   !4
      CHARACTER*10 DSN    ! First 2 levels of stats dataset          !4
      NAMELIST /INSTAT/ NDAY, DSN                                    !4
      DATA  NDAY, DSN /  -1,  'MDB.STATS'/                           !4
!                                                   Data initialisation
!                                  Common (for dynamic allocation only)

      COMMON /COMSTS/ BLOCK, CDATA, INDEKS                           !2

      DATA IBLK/23476,23476,27998,27998,27998,27998,27998,           !2
     &          27998,27998,27998,23476,27998/                       !3

!-----------------------------------------------------------------------
! size of the data blocks;character* for map,block,dummy must
! be as big as the largest block size (set to 27998)
!-----------------------------------------------------------------------

      DATA WKDAY/'Sunday','Monday','Tuesday','Wednesday','Thursday',
     &           'Friday','Saturday'/
      DATA CMONTH/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     &            'Oct','Nov','Dec'/

#if ! defined (MVS)
      DATA DATYPE/'TAFS','METARS','LNDSYN','SHPSYN','SREW','NCM',   !1.5
     &            'ESAWS','PRESTEL','TBUS','BOGUS','ATAFS','MOBSYN'/ !3
#endif

!-----------------------------------------------------------------------
! Revesion details
!-----------------------------------------------------------------------

      HEAD = '$Workfile: statsmet.F$ ' //
     &       '$Revision: 4$ $Date: 06/01/2009 13:23:25$'

!-----------------------------------------------------------------------
! get ENV VAR STORAGE_DIR from system
!-----------------------------------------------------------------------

#if ! defined (MVS)
      CALL METDB_GETENV("STORAGE_DIR",STORAGE_DIR,I)                !1.5
#endif

!-----------------------------------------------------------------------
! Read first two levels of statistics data set and date from        !4
! namelist. (Negative dates refer to days before today.)            !4
!-----------------------------------------------------------------------

      INQUIRE (FILE='FT02F001', EXIST=GOOD)                         !4
      IF (GOOD) THEN                                                !4
        OPEN (2,IOSTAT=IOS)                                         !4
        READ (2, INSTAT, IOSTAT=IOS)                                !4
        CLOSE (2)                                                   !4
      END IF                                                        !4

      LENQ = INDEX(DSN//' ',' ') - 1    ! LENGTH OF HLQ             !4
!-----------------------------------------------------------------------
! get current date/time from system
!-----------------------------------------------------------------------

      CALL DATIM(IDT)                                               !4
!                                              NDAY = days before today
      IF (NDAY.LE.0) THEN                                           !4
        CALL DATE31 (IDT(6), IDT(7), IDT(8), ICD)                   !4
        ICD = ICD + NDAY                                            !4
        CALL DATE13 (ICD, ID, IM, IY)                               !4
!                                            NDAY = actual day of month
      ELSE                                                          !4
        IM = IDT(7)                                                 !4
        IY = IDT(8)                                                 !4
        IF (NDAY.GT.IDT(6)) THEN   ! Must be last month             !4
          IM = IM - 1                                               !4
          IF (IM.EQ.0) THEN    ! Must be last year                  !4
            IM = 12                                                 !4
            IY = IY - 1                                             !4
          END IF                                                    !4
        END IF                                                      !4
        ID = NDAY                                                   !4
        CALL DATE31 (ID, IM, IY, ICD)                               !4
      END IF                                                        !4

      IWKDAY = MOD(ICD,7) + 1                                       !1.5

!-----------------------------------------------------------------------
! loop over datasets
!-----------------------------------------------------------------------

       DO J1=1,NDSN             !from 1 to total no. of datasets

        IDSUNT=20 + J1             !unit set for output
        IUNIT = 50 + J1            !iunit set to read DSN FTxxF001
#if defined (MVS)
        OPEN(IDSUNT)
#else
        FILENAME=DSN(1:LENQ)//'.'//DATYPE(J1)                       !4
        OPEN(IDSUNT,FILE=FILENAME)                                  !1.5
        I=INDEX(STORAGE_DIR,' ')-1                                  !1.5
        FILENAME=STORAGE_DIR(1:I)//'/MDB.'//DATYPE(J1)              !1.5
        OPEN(IUNIT,ACCESS='DIRECT',RECL=IBLK(J1),FILE=FILENAME)     !1.5
#endif
        READ(IDSUNT,'(A80)')CHEAD  !READ HEADER FROM IDSUNT
        DO I=1,32
          CDATA(I)=' '
        ENDDO

        READ(IDSUNT,'(A80)')(CDATA(I), I=33,672)

!-----------------------------------------------------------------------
! write header for next sequence of values
!-----------------------------------------------------------------------

         CDATA(1)='=================================='
         CDATA(2)(1:9)=WKDAY(IWKDAY)
         WRITE(CDATA(2)(11:12),'(I2.2)')ID        !write day of week
         CDATA(2)(14:16)=CMONTH(IM)
         WRITE(CDATA(2)(18:21),'(I4)')IY          !write month
         CDATA(3)='----------------------------------'

         CDATA(4)='Hours   Observations   Receipt (Z)'

         CDATA(5)='                       First  Last'

         CDATA(6)='----------------------------------'

        BLKSIZ=IBLK(J1)
        CALL STATSUB(IY,IM,ID,IUNIT,BLKSIZ,NREP,ITORMN,ITORMX
     &            ,LATERP,ITOTAL,OFLOWS,NFREE)

!-----------------------------------------------------------------------
! produce output for screen/printout and storage in stats dataset
!-----------------------------------------------------------------------

         DO J2=0,23
           J27=J2+7
           IF (NREP(J2).EQ.0) THEN
             WRITE(CDATA(J27)(3:4),'(I2.2)')J2
             CDATA(J27)(12:18)='NO DATA'
           ELSE

!-----------------------------------------------------------------------
! output for screen/printout and storage in stats dataset
!-----------------------------------------------------------------------

             WRITE(CDATA(J27)(3:4),'(I2.2)')J2       !hour
             WRITE(CDATA(J27)(12:16),'(I5)')NREP(J2) !number reports
             WRITE(CDATA(J27)(25:28),'(I4.4)')ITORMN(J2) !1st t.o.r.
             WRITE(CDATA(J27)(31:34),'(I4.4)')ITORMX(J2) !last t.o.r.
 !       WRITE(CDATA(J27)(43:44),'(I2.2)')LATERP(J2) !no. reports late
           ENDIF
         ENDDO

         IF(OFLOWS.EQ.0) THEN
           CDATA(31)='TOTAL'
           WRITE(CDATA(31)(7:12),'(I6)')ITOTAL
         ELSE
           CDATA(31)='Total        Oflows      Nfree'
           WRITE(CDATA(31)(7:12),'(I6)')ITOTAL
           WRITE(CDATA(31)(21:26),'(I4)')OFLOWS
           WRITE(CDATA(31)(31:36),'(I4)')NFREE
         ENDIF
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
         PRINT*, ' '              !leave spaces between one data
         PRINT*, ' '              !type values and the next on output

!-----------------------------------------------------------------------
! print the number of overflows used and the number free for each
! data type. Used as part of the daily checks and replaces T12DBSAT.
! samosx does not use overflows and will not be shown.
!-----------------------------------------------------------------------

        IF (OFLOWS.GT.0) THEN

          PRINT*,' '
          PRINT*,OFLOWS,'OVERFLOW BLOCKS',NFREE,'FREE'
          PRINT*,' '

          LF=NFREE*100/OFLOWS     !percantage of overflows left
          IF(LF.LT.25 .AND. LF.GT.10) THEN
            PRINT*,'!!!!LESS THAN 25% OVERFLOWS LEFT!!!!'
          ELSEIF(LF.LE.10)THEN
            PRINT*,'**!!**LESS THAN 10% OVERFLOWS LEFT**!!**'
          END IF
        END IF

       CLOSE(IDSUNT)

      ENDDO                         !end of loop over datasets

      STOP
      END
