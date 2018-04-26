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
! $Workfile: statsmet.F90$
! $Revision: 1$ $                         $
!
! CHANGE RECORD :
!
! $Log:
!-----------------------------------------------------------------------

IMPLICIT NONE

INTEGER         ::    ITORMN(0:23)   !1ST TIME OF RECEIPT
INTEGER         ::    ITORMX(0:23)   !last time of receipt
INTEGER         ::    ITOTAL         !total number of receipts
INTEGER         ::    LATERP(0:23)  !number of late reports
INTEGER         ::    NREP(0:23)     !number of reports

INTEGER(KIND=2) ::    OFLOWS     !NUMBER OF OVER FLOWS

INTEGER         ::    BLKSIZ  !Block size for datatype
INTEGER         ::    IUNIT  !data store unit numbers
INTEGER         ::    IDSUNT !data output for stats
INTEGER         ::    I
INTEGER         ::    ID  ! date
INTEGER         ::    IM  ! month
INTEGER         ::    IY  ! year
INTEGER         ::    J2  !hours
INTEGER         ::    J27 !hours+7 = no.lines down for output data
INTEGER         ::    LF
INTEGER         ::    J1   !loop over number of datasets

INTEGER         ::    MAXOFL
INTEGER         ::    MAXNID
INTEGER         ::    NDSN  !number of datasets
INTEGER         ::    INDHED
INTEGER         ::    IOS
INTEGER         ::    LENQ

PARAMETER (NDSN=12,INDHED=6,MAXOFL=7,MAXNID=1020)

INTEGER         ::    IBLK(NDSN)

INTEGER         ::    IDT(14)
INTEGER          ::    ICD
INTEGER          ::    NFREE
! INTEGER(KIND=2)  ::    BLOCKS
INTEGER          ::    IWKDAY
! INTEGER(KIND=2)  ::    RECLEN(999)  !arr rep length slots after header
! INTEGER(KIND=2)  ::    RECDIS(999)

CHARACTER(LEN=80)      ::     CHEAD(4)
CHARACTER(LEN=80)      ::     CDATA(672)
CHARACTER(LEN=27998)   ::     BLOCK

LOGICAL GOOD

!-----------------------------------------------------------------------
! ndsn is the number of data types
! laterp counts reports received later than 18hrs after t.o.r.
! late is flag to say current report is late
! reclen is the array of report length slots after the block header.
! its dimension is arbitrary, it only needs to be big enough for the
! reports that will fit in a block.  we do not know where the lengths
! will end and the reports, slotted in from the end, will meet them!
!-----------------------------------------------------------------------

CHARACTER(LEN=3)   ::    CMONTH(12)
CHARACTER(LEN=9)   ::    WKDAY(7)

CHARACTER(LEN=23)  ::    INDEKS(MAXOFL*MAXNID)      !INDEX ENTRIES
CHARACTER(LEN=80)  ::    HEAD                       !REVISION INFO
! #if ! defined (MVS)
CHARACTER(LEN=8)   ::    DATYPE(NDSN)
CHARACTER(LEN=200) ::    FILENAME
CHARACTER(LEN=200) ::    STORAGE_DIR
! #endif
!
INTEGER            ::    NDAY      ! Day of mon for which stats are req
                    ! (or, if negative, days before 'today')
CHARACTER(LEN=10) ::    DSN    ! First 2 levels of stats dataset
NAMELIST /INSTAT/ NDAY, DSN
DATA  NDAY, DSN /  -1,  'MDB.STATS'/
!                                                   Data initialisation
!                                  Common (for dynamic allocation only)

COMMON /COMSTS/ BLOCK, CDATA, INDEKS

DATA IBLK/23476,23476,27998,27998,27998,27998,27998,&
               27998,27998,27998,23476,27998/

!-----------------------------------------------------------------------
! size of the data blocks;character* for map,block,dummy must
! be as big as the largest block size (set to 27998)
!-----------------------------------------------------------------------

DATA WKDAY/'Sunday','Monday','Tuesday','Wednesday','Thursday',&
                'Friday','Saturday'/
DATA CMONTH/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',&
                'Oct','Nov','Dec'/

#if ! defined (MVS)
DATA DATYPE/'TAFS','METARS','LNDSYN','SHPSYN','SREW','NCM',&
                 'ESAWS','PRESTEL','TBUS','BOGUS','ATAFS','MOBSYN'/
#endif

!-----------------------------------------------------------------------
! Revesion details
!-----------------------------------------------------------------------

HEAD = '$Workfile: statsmet.F90$ ' //&
           '$Revision: 1$ $Date: 22/08/2012 11:56:30$'

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
IF_LABEL1 : &
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
END IF IF_LABEL1

IWKDAY = MOD(ICD,7) + 1

!-----------------------------------------------------------------------
! loop over datasets
!-----------------------------------------------------------------------

 DO_LABEL1: &
 DO J1=1,NDSN             !from 1 to total no. of datasets

  IDSUNT=20 + J1             !unit set for output
  IUNIT = 50 + J1            !iunit set to read DSN FTxxF001
#if ! defined (MVS)
  OPEN(IDSUNT)
#else
  FILENAME=DSN(1:LENQ)//'.'//DATYPE(J1)
  OPEN(IDSUNT,FILE=FILENAME)
  I=INDEX(STORAGE_DIR,' ')-1
  FILENAME=STORAGE_DIR(1:I)//'/MDB.'//DATYPE(J1)
  OPEN(IUNIT,ACCESS='DIRECT',RECL=IBLK(J1),FILE=FILENAME)
#endif
  READ(IDSUNT,'(A80)')CHEAD  !READ HEADER FROM IDSUNT
  DO I=1,32
    CDATA(I)=' '
  END DO

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
  CALL STATSUB(IY,IM,ID,IUNIT,BLKSIZ,NREP,ITORMN,ITORMX &
                 ,LATERP,ITOTAL,OFLOWS,NFREE)

!-----------------------------------------------------------------------
! produce output for screen/printout and storage in stats dataset
!-----------------------------------------------------------------------

   DO_LABEL2: &
   DO J2=0,23
     J27=J2+7
     IF_LABEL2: &
     IF (NREP(J2) == 0) THEN
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
     END IF IF_LABEL2
   END DO DO_LABEL2

   IF(OFLOWS == 0) THEN
     CDATA(31)='TOTAL'
     WRITE(CDATA(31)(7:12),'(I6)')ITOTAL
   ELSE
     CDATA(31)='Total        Oflows      Nfree'
     WRITE(CDATA(31)(7:12),'(I6)')ITOTAL
     WRITE(CDATA(31)(21:26),'(I4)')OFLOWS
     WRITE(CDATA(31)(31:36),'(I4)')NFREE
   END IF
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

  IF (OFLOWS > 0) THEN

    PRINT*,' '
    PRINT*,OFLOWS,'OVERFLOW BLOCKS',NFREE,'FREE'
    PRINT*,' '

    LF=NFREE*100/OFLOWS     !percantage of overflows left
    IF(LF < 25 .AND. LF > 10) THEN
      PRINT*,'!!!!LESS THAN 25% OVERFLOWS LEFT!!!!'
    ELSE IF(LF <= 10)THEN
      PRINT*,'**!!**LESS THAN 10% OVERFLOWS LEFT**!!**'
    END IF
  END IF

 CLOSE(IDSUNT)

END DO DO_LABEL1              !end of loop over datasets

STOP
END PROGRAM STATSMET
