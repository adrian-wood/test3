PROGRAM HOWFULL

!----------------------------------------------------------------------
!
! PROGRAM       : HOWFULL
!
! PURPOSE       : To see how full certain kinds of MetDB data set are
!
! DATA TYPE(S)  : Any stored by programs which claim blocks only when
!                 needed (AIRSTO, ERSREP etc - any but TAFREP & clones)
!
! CALLS         : DATIM, ICHAR2
!
! REVISION INFO :
!
! $Workfile: howfull.F90$ $Folder: OpSource$
! $Revision: 3$ $Date: 21/06/2011 18:59:09$
! $Log:
!  3    MetDB_Refresh 1.2         21/06/2011 18:59:09    Sheila Needham
!       Updated
!  2    MetDB_Refresh 1.1         15/04/2011 14:18:41    Alison Weir     Ported
!        to F95 and amended to be able to read ZFS datasets.
!  1    MetDB_Refresh 1.0         15/04/2011 10:59:35    Alison Weir
!       Initial f90 version copied from howfull.pre in MetDB project
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!----------------------------------------------------------------------

! Use statements:
USE datim_mod
USE ichar2_mod

IMPLICIT NONE

!Declare Characters
CHARACTER(LEN=1), dimension(:), allocatable :: MAP ! Complete MAP records (old format)
CHARACTER(LEN=8)  :: MAPHDR      ! Map record header (old format)
CHARACTER(LEN=2)  :: MAPTRL      ! Map record trailer (old format)
CHARACTER(LEN=60) :: DSN         ! Data set name
CHARACTER(LEN=80) :: LINE        ! Line of output for unit 6
CHARACTER(LEN=21) :: PREFIX      ! Prefix for printout
CHARACTER(LEN=27998)  :: MAPREC  ! Map record (new format)
INTEGER           :: BLKSIZ      ! Record length of storage data set
INTEGER           :: I           ! Loop counter and other local use
INTEGER           :: J           ! Loop counter and other local use
INTEGER, DIMENSION(:), ALLOCATABLE    :: INDB ! No. of data recs. for each index
INTEGER           :: IRC         ! Return code from I/O statement
INTEGER           :: IS          ! Location of subtype name in DSN
INTEGER           :: IX          ! Index pointer in map record
INTEGER           :: LDSN        ! Length of data set name
INTEGER           :: MAPLEN1     ! No. of index pointers in map record
INTEGER           :: MAX         ! Max. number of data records per index
INTEGER           :: MEAN        ! Mean number of data records per index
INTEGER           :: NB          ! Total number of records (old format)
INTEGER           :: NDB         ! Number of data records
INTEGER           :: NINDB       ! Highest INDB element to look at
INTEGER           :: NMAP        ! Number of current map record
INTEGER           :: NMAPBLK     ! Total number of map records
INTEGER           :: NOW(8)      ! Date/time (as output from DATIM)
INTEGER           :: NSEQBL      ! Record number of BUFR sequence record
INTEGER           :: NSQ         ! =1 if BUFR sequence record; else 0
INTEGER           :: NUMREC      ! Record number in storage dataset
INTEGER           :: NUSE        ! % of data records in use
INTEGER           :: NXB         ! Total number of index records
INTEGER           :: NXM         ! Index period in minutes
INTEGER           :: IERR        ! error condition from IO of storage dat
INTEGER           :: INT4        ! dummy variable used in TRANSFER routin
INTEGER           :: IC          ! character count in storage dataset blo

CHARACTER*200     :: FILENAME    ! File name and path of storage dataset
CHARACTER*27998   :: BLOCK       ! Record of storage datasets

LOGICAL           :: ZFS         ! Storage file is on ZFS

#if ! defined (MVS)
CHARACTER(LEN=200) :: STORAGE_DIR
#endif

!-----------------------------------------------------------------------
! get ENV VAR BASE_DIR from system
!-----------------------------------------------------------------------

#if ! defined (MVS)
CALL METDB_GETENV("STORAGE_DIR",STORAGE_DIR,I)
#endif

! Open output file for statistics output.
OPEN (10,FILE='DD:STATS',IOSTAT=IRC)

! Headings for tabulated output.

CALL DATIM (NOW)
I = MOD(NOW(8),100)  ! 2-digit year
WRITE (10,'(T12,A,2I2.2,A,I3.2,2(''/'',I2.2))')       &
              'MET.D.B. STORAGE DATA SET USAGE AT ',  &
               NOW(5), NOW(4), 'Z ON', NOW(6), NOW(7), I
WRITE (10,'(T12,2A)') ('--------------------------',J=1,2)
WRITE (10,'(/T31,A /T35,A)')                                   &
         '% Data records   Data records/index   Index period', &
             'in use          Mean     Max.      (minutes)'

! Loop round the data sets in the input list, opening each with the
! blocksize specified in the input.
! Read the map block. All the calculations will be based on the map.

IRC=0
DOLABEL1: &
DO WHILE (IRC == 0)

#if defined (MVS)
   OPEN  (5,FILE='DD:INPUT',ACTION='READ',FORM='FORMATTED')
#endif
   READ (5,'(A60,1X,I5)',IOSTAT=IRC) DSN(1:60),BLKSIZ

   LDSN=INDEX(DSN,' ')-1
   IF (IRC /= 0 .OR. LDSN <= 4) GO TO 99
   MAPLEN1=BLKSIZ-LEN(MAPHDR)-LEN(MAPTRL)
   ALLOCATE (MAP(MAPLEN1))

! Pre-processor statements: If platform is IBM, specify ACTION='READ'
! otherwise omit.

#if defined (MVS)
   ZFS = INDEX(DSN(1:LDSN),'/') > 0
   IF (ZFS) THEN
     FILENAME=DSN(1:LDSN)              ! ZFS format
   ELSE
     FILENAME="//'"//DSN(1:LDSN)//"'"  ! Z/OS format
   END IF

   CALL METDB_COPEN(1,TRIM(FILENAME)//CHAR(0),0,IERR)

! If unit 1 fails to open then write out an error message and end program
   IF (IERR /= 0) THEN
      WRITE(6,*)'fatal error opening unit 1 so exiting'
      WRITE(6,*)'unable to open ',FILENAME
      WRITE(6,*)'IOSTAT status = ',IERR
      STOP
   END IF

#else
   I=INDEX(STORAGE_DIR,' ')-1
   FILENAME=STORAGE_DIR(1:I)//'/'//DSN(1:LDSN)

   CALL METDB_COPEN(1,FILENAME(1:LDSN+I+1)//CHAR(0),0,IERR)

#endif

   NUMREC = 1
   CALL METDB_CREAD_DIR (1, BLOCK(1:BLKSIZ), BLKSIZ, NUMREC, IERR)

   MAPHDR = BLOCK(1:8)
   IC = 9
   DO I = 1, MAPLEN1
     MAP(I) = BLOCK(IC:IC)
     IC = IC + 1
   END DO
   MAPTRL = BLOCK(IC:IC+1)

   NB = ICHAR2(MAPHDR(1:2))
IFLABEL1: &
   IF (NB > 0) THEN                ! Old data set format
      NXB = ICHAR2(MAPHDR(3:4))
      NXM = ICHAR2(MAPHDR(5:6))*60

! If the map has >12000 records look for it at the end of the data set.
! (The test on MAP(NXB+1) was added after storage data sets for
! merged SUBSEA data were expanded beyond 12000 records but the
! map was kept in record 1. MAP(NXB+1) is the byte for the first
! data record and should never be 0 if the map is in record 1.)

IFLABEL2: &
     IF (NB > 12000 .AND. MAP(NXB+1) == CHAR(0)) THEN
        NSEQBL=ICHAR(MAP(1))

        NMAPBLK=(NB+(BLKSIZ-1))/BLKSIZ

DOLABEL2: &
        DO J=1,NMAPBLK
          NUMREC = NB-NMAPBLK+J
          CALL METDB_CREAD_DIR (1, BLOCK(1:BLKSIZ), BLKSIZ,  &
              NUMREC, IERR)
          IC = 1
          DO I = (J-1)*BLKSIZ+1, MIN(J*BLKSIZ,NB)
            MAP(I) = BLOCK(IC:IC)
            IC = IC + 1
          END DO

        END DO DOLABEL2
     ELSE
        NSEQBL = ICHAR(MAP(NB)) ! BUFR seq. block
     END IF  IFLABEL2

     NSQ=0
     IF (NSEQBL > 0) NSQ=1
     ALLOCATE (INDB(0:NXB+1))
!                                            Clear out INDB array
     DO I=0,NXB+1
        INDB(I) = 0
     END DO

! The map byte for a data block shows which index block owns it.
! Loop round the data block bytes, counting data blocks owned by
! each index block - and those not in use (zero bytes) in INDB(0).
! (Some kinds of storage set the top bit to indicate overflow:
!  ignore this bit. (It is clear from the number of index blocks
!  when the bit is being used to indicate overflow.))

     NDB=NB-1-NSQ-NXB
     DO I=NXB+1,NXB+NDB
        IX=ICHAR(MAP(I))
        IF (NXB < 128 .AND. IX > 128) IX=IX-128
        INDB(IX)=INDB(IX)+1
     END DO
     NINDB = NXB + 1

   ELSE                     ! New data set format

     NUMREC = 1
     CALL METDB_CREAD_DIR (1, BLOCK(1:BLKSIZ), BLKSIZ, NUMREC, IERR)
     I       = TRANSFER(BLOCK( 1: 4),INT4)
     NB      = TRANSFER(BLOCK( 5: 8),INT4)
     NMAPBLK = TRANSFER(BLOCK( 9:12),INT4)
     NXB     = TRANSFER(BLOCK(13:16),INT4)
     NXM     = TRANSFER(BLOCK(17:20),INT4)

     J = 2 + NMAPBLK + NXB  ! Records before 1st data record
     NDB = NB - J           ! Number of data records
     J = 2*J + 1            ! Start of 1st map pointer
     NMAP = 3               ! Number of first map record

     CALL METDB_CREAD_DIR (1, MAPREC(1:BLKSIZ), BLKSIZ, NMAP, IERR)
     NINDB = NXB + NMAPBLK + 2  !
     ALLOCATE (INDB(0:NINDB))
!                                            Clear out INDB array
     DO I=0,NINDB
        INDB(I) = 0
     END DO

     DO I=1,NDB
       IF (J >= BLKSIZ) THEN  ! Get next map record
          NMAP = NMAP + 1
          CALL METDB_CREAD_DIR (1, MAPREC(1:BLKSIZ), BLKSIZ, NMAP, IERR)
          J = 1
        END IF

        IX = ICHAR2(MAPREC(J:J+1)) ! Pointer to index record
        INDB(IX) = INDB(IX) + 1
        J = J + 2
      END DO
!      NINDB = NXB + NMAPBLK + 2
   END IF  IFLABEL1
!                                             Close MetDB data set
   CALL METDB_CCLOSE(1)

! Find the maximum data records for any index period, for comparison
! with the mean to see if one period has an unusually large total.

   MAX = 0
   DO I=1,NINDB
      IF (INDB(I) > MAX) MAX = INDB(I)
   END DO

   MEAN = NINT((NDB-INDB(0))/FLOAT(NXB))  ! Mean no. of recs.
   NUSE = NINT((NDB-INDB(0))/(0.01*NDB))  ! % data blks used

   DEALLOCATE (INDB)

! IS is the start position of MDB subtype name.
! This will be 1 after the first '.'.

   IS = INDEX(DSN,'.')
   IF (IS > 0) THEN
     IS = IS + 1
   ELSE
   ! unusual dsn format - set a dummy value and see what happens!
     IS = 1
   END IF

! Assign prefix for merged data sets

   IF (INDEX(DSN,'.OPER.GAIR') > 0) THEN
      PREFIX = '       Global merged '
   ELSE IF (INDEX(DSN,'.OPER.NAE') > 0) THEN
      PREFIX = '          NAE merged '
   ELSE IF (INDEX(DSN,'.OPER.MES4K') > 0) THEN
      PREFIX = '4km.Mesoscale merged '
   ELSE IF (INDEX(DSN,'.OPER.UKV') > 0) THEN
      PREFIX = '          UKV merged '
   ELSE IF (INDEX(DSN,'.OPER.GLBLNEMO') > 0) THEN
      PREFIX = '  Global NEMO merged '
   ELSE IF (INDEX(DSN,'.OPER.GSST') > 0) THEN
      PREFIX = '          SST merged '
   ELSE
      PREFIX = ' '
   END IF
!                             Output statistics for this data set

   I = INDEX(DSN(IS:),'.') - 2
   IF (I < 0) I = 7
   WRITE (10,'(T2,2A,T31,I8,I15,I9,I13)')  &
       PREFIX, DSN(IS:IS+I), NUSE, MEAN, MAX, NXM

   DEALLOCATE (MAP)

END DO  DOLABEL1
!                                Copy statistics to printed output

! close stats output file and then reopen with read only access.
! old f77 program just rewound unit 10 but this does not seem to work with f90

99 CONTINUE

CLOSE (10)
OPEN (10,FILE='DD:STATS',ACTION='READ',IOSTAT=IRC)

! Initial IRC and LINE
IRC = 0
LINE = ' '

! loop over each line and print out to standard out
DO WHILE (IRC == 0)
   WRITE (6,'(A)') LINE
   READ (10,'(A)',IOSTAT=IRC) LINE
END DO
!

! close open units
CLOSE (10)
CLOSE (5)

! Stop and end program
STOP
END PROGRAM HOWFULL
