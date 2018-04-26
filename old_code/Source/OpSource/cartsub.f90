PROGRAM CARTSUB

!-----------------------------------------------------------------------
!
! Program       : CARTSUB
!
! Purpose       : To update and submit JCL for MetDB MASSR daily
!               : archive.
!
! Description   : Reads records from a Lookup Table and uses values
!                 from these records to overwrite question marks in
!                 skeletal lines of JCL read from a Jobs Dataset.
!                 Stage 1 copies one or more days worth of data (going
!                 back by the number of days specified in the Lookup
!                 Table) from the on-line Datastore for a Subtype to
!                 the Datapark.  Stage 2 copies the data from the
!                 Datapark to MASS-R in one of 17
!                 streams using MOOSE. If a copy in a stream is
!                 successful (COND CODE 0), the data in that stream
!                 are scratched from the datapark. Stage 3 checks the
!                 (E)JES entries created by stages 1. Stage 4
!                 checks the (E)JES entries created by stage 2.
!
! Data Type(s)  : Generic (all types archived)
!
! Called by     : Submitted by OPC at 0105Z each morning
!
! Calls         : DATE13 and DATE31 in ZPDATE
!                 DATIM
!
! I/O           : FT10 'MDB.ARCHIVE.LOOKUP' the Lookup Table      (I)
!               : FT20 'MDB.ARCHIVE.JOBS'   the Jobs Dataset      (I)
!               : FT51-67 'MDB.ARCHIVE.DATAPARK.SCRATCH.stream_last_3
!                                                                 (I/O)
!                 datapark name(s) for the 17 scratches from
!                 the Datapark
!
! REVISION INFO :
!
! $Revision: 3$
! $Date: 29/05/2013 15:55:11$
! $Workfile: cartsub.f90$
!
! CHANGE RECORD :
! $Log:
!  3    Met_DB_Project 1.2         29/05/2013 15:55:11    Brian Barwell   Two
!       lines added to get correct ZFS path names for MSGCTPFD & MSGCTPUK.
!  2    Met_DB_Project 1.1         20/03/2012 07:53:19    Sheila Needham
!       Correct stream name B02 to B01
!  1    Met_DB_Project 1.0         14/02/2012 09:12:16    Sheila Needham  F90
!       version with redesign for periodic archiving and to allow for empty
!       streams.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2013 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE zpdate_mod
USE datim_mod

IMPLICIT NONE

! Local Parameters

INTEGER,PARAMETER      :: STREAMS=17          !Number of streams
INTEGER,PARAMETER      :: MAXJ=2000           ! Max num of lines of JCL
CHARACTER(1),PARAMETER :: QUOTE=''''
CHARACTER(1),PARAMETER :: BLANK=''
CHARACTER(1),PARAMETER :: SPACE=' '

! Local Variables

INTEGER :: LINES_OF_JCL     !No. of JCL lines in stages 1, 2, 3, 5
INTEGER :: LINES_COPY       !No. of JCL lines submitted from stage 2
INTEGER :: LINES_OF_JCL2    !No. of JCL lines in stage3 Pt2
INTEGER :: LINES_OF_JCL3    !Totals for lines of JCL 1 & 2
INTEGER :: ALL_LINES_OF_JCL !No. of JCL lines in stage 4
INTEGER :: LINES_READ       !No. of lines read so far
INTEGER :: LINES_READF      !Start point of section3
INTEGER :: CENDAY           !Century Day
INTEGER :: BASE_DATE(5)     !base date YMDHM
INTEGER :: BASE_DAY         ! and century day for periodic archiving
INTEGER :: STREAM_COUNT     !No. of streams being archived today
INTEGER :: ARSTREAM(STREAMS)!List of streams being archived today
INTEGER :: I,J,K,IJ,Q,P     !Loop counters
INTEGER :: IOSTAT
INTEGER :: IOS
INTEGER :: L,M              !Loop counters
INTEGER :: LC               !line count
INTEGER :: S,N              !char search ind !2.7
INTEGER :: T
INTEGER :: MINLINE,MAXLINE  !Loop counters
INTEGER :: NPERIOD          !Archive period
INTEGER :: NOW(8)           !System date/time
INTEGER :: DAY_NOW          !System day
INTEGER :: MONTH_NOW        !System month
INTEGER :: YEAR_NOW         !System year
INTEGER :: DAYS_BACK        !From Lookup Table
INTEGER :: DAY_TO_ARCHIVE   !From system and days back
INTEGER :: MONTH_TO_ARCHIVE !From system and days back
INTEGER :: YEAR_TO_ARCHIVE  !From system and days back
INTEGER :: POS              !Position of substring
INTEGER :: ISP              !Position of space in dsn
INTEGER :: STREAM_TOTALS(STREAMS)     !Subtypes in each stream
INTEGER :: DATA_FOR_DATAPARK(STREAMS) !For Datapark in each stream
INTEGER :: HOLD_COUNT                 !For the net
INTEGER :: UNIT   !Unit number of the DATAPARK.STREAM D/S
CHARACTER(22) :: ARCHIVE_IP             !Archive string pt1
CHARACTER(8)  :: ARCHIVE_IP2            !Archive string pt2
CHARACTER(11) :: DDNAME
CHARACTER(1),PARAMETER  :: ARCH_ID(17)=                           &
                           (/'a','b','c','d','e','a','b','c',     &
                             'd','e','f','g','h','i','j','k','l'  /)
CHARACTER(80) :: CONTROL_LINE           ! control info
CHARACTER(80) :: CONTROL_LINE2          !control line part3
CHARACTER(80) :: JCL_LINE(MAXJ)         !JCL read in
CHARACTER(80) :: JCL_COPY(MAXJ)         !JCL to submit (temp)
CHARACTER(80) :: JCL_OUT(MAXJ)          !JCL to submit (temp)
CHARACTER(3)  :: JOB_NAME_LAST_3(STREAMS,99) !In (E)JES
CHARACTER(30) :: DATAPARK_NAME(STREAMS,99)   !The full name
CHARACTER(25) :: ARCJES_NAME(STREAMS,99)     !The full name
CHARACTER(1)  :: IN_DATAPARK(STREAMS,99)     !'Y' if already there
CHARACTER(8)  :: PATH
CHARACTER(3),PARAMETER  :: STREAM_LAST_3(STREAMS)=          &
                    (/'A01','B01','C01','D01','E01','A10',  &
                      'B10','C10','D10','E10','F10','G10',  &
                      'H10','I10','J10','K10','L10'         /)
CHARACTER(4)  :: START_TIME             !Of the 24 hours to archive
CHARACTER(4)  :: STOP_TIME              !Of the 24 hours to archive
CHARACTER(250):: LOOKUP_RECORD          !From the lookup table
CHARACTER(4)  :: CYEAR_TO_ARCHIVE       !Char of YEAR_TO_ARCHIVE
CHARACTER(2)  :: STREAM                 !Char for PRINT statement
CHARACTER(44) :: DS_TO_SCRATCH          !From Datapark
CHARACTER(28) :: DSN_IN                 !data set to be copied
CHARACTER(8)  :: DATYPE                 !data subtype
LOGICAL :: ARCHIVE_THEM         !data not GRIB; to be archived
LOGICAL :: OJCL(MAXJ)           ! TRUE to include lines of JCL
                                ! for submission

! ----------------------------------------------------------------

!Initialize variables

LINES_OF_JCL=0
LINES_READ=0
LOOKUP_RECORD(:)=BLANK
STREAM_TOTALS(:)=0
DATA_FOR_DATAPARK(:)=0
JOB_NAME_LAST_3(:,:)=BLANK
DATAPARK_NAME(:,:)=BLANK
ARCJES_NAME(:,:)=BLANK
IN_DATAPARK(:,:)=BLANK

DS_TO_SCRATCH(1:10)='   DELETE '

ARCHIVE_IP='EXEC ARCHFILE,STREAM='//QUOTE
ARCHIVE_IP2=QUOTE//',NAME='//QUOTE

!Open the Lookup Table (10) and the Jobs Dataset (20)
OPEN(10,FILE='DD:FT10F001',                                 &
        ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ')
OPEN(20,FILE='DD:FT20F001',                                 &
        ACCESS='DIRECT',RECL=80,ACTION='READ')

!   Today's date used to work out actual archive dates
!  Can be overridden by dd/mm/yyyy on FT05 input

IOS=0
OPEN(5,FILE='DD:FT05F001',ACTION='READ',FORM='FORMATTED',IOSTAT=IOS)
IF (IOS == 0) THEN
  READ(5,IOSTAT=IOS,FMT='(I2,1x,I2,1x,I4)')        &
           DAY_NOW,MONTH_NOW,YEAR_NOW
END IF
IF (IOS /= 0) THEN
  CALL DATIM(NOW)
  DAY_NOW=NOW(6)
  MONTH_NOW=NOW(7)
  YEAR_NOW=NOW(8)
END IF

! Skip the preliminary lines of comment in the Lookup Table
DO WHILE (LOOKUP_RECORD(1:6)  /=  'LOOKUP')
  READ(10,'(A250)') LOOKUP_RECORD
END DO

! Skip the preliminary lines of comment in the Jobs Dataset

READ(20,REC=1)CONTROL_LINE
READ(CONTROL_LINE(56:57),'(I2.2)')LINES_READ

! Set up JCL to extract data from datastore to Datapark

READ(20,REC=LINES_READ+1)CONTROL_LINE
READ(CONTROL_LINE(49:50),'(I2.2)')LINES_OF_JCL
LINES_READ=LINES_READ+1
DO I=1,LINES_OF_JCL
  READ(20,REC=LINES_READ+I)JCL_LINE(I)
END DO

! -------------------------------------------------------------------
! Loop round all the archive records in the Lookup Table. Read them
! into a char string; most fields may then be copied straight to the
! appropriate line of JCL as chars.
! -------------------------------------------------------------------

READ(10,'(A250)') LOOKUP_RECORD
DoLookup: &
DO WHILE (LOOKUP_RECORD(1:6)  /=  'END OF')
  ARCHIVE_THEM=.TRUE.

! JCL lines are set up for output
  JCL_OUT(:)=JCL_LINE(:)
  OJCL(:)=.TRUE.
  READ(LOOKUP_RECORD(215:216),'(I2)')NPERIOD    ! length of archive period
  READ(LOOKUP_RECORD(219:234),'(I4,4(1X,I2))')BASE_DATE

! Start date is always a number of days back from TODAY
  READ(LOOKUP_RECORD(15:16),'(I2.2)') DAYS_BACK
  CALL DATE31(DAY_NOW,MONTH_NOW,YEAR_NOW,CENDAY)
  CENDAY=CENDAY-DAYS_BACK
  CALL DATE13                                                 &
    (CENDAY,DAY_TO_ARCHIVE,MONTH_TO_ARCHIVE,YEAR_TO_ARCHIVE)
  WRITE(CYEAR_TO_ARCHIVE,'(I4.4)')YEAR_TO_ARCHIVE

! Decide if this datatype needs archiving today
  CALL DATE31(BASE_DATE(3),BASE_DATE(2),BASE_DATE(1),BASE_DAY)
  IF ( MOD((CENDAY-BASE_DAY),NPERIOD)  /=  0) ARCHIVE_THEM=.false.

ToBeArchived: &
  IF(ARCHIVE_THEM) THEN
    START_TIME=LOOKUP_RECORD(18:21)
    STOP_TIME=LOOKUP_RECORD(24:27)
    JCL_OUT(1)(8:10)=LOOKUP_RECORD(29:31) !Job name (last 3)
    READ(LOOKUP_RECORD(143:144),'(I2)') I !Stream number
    JCL_OUT(2)(36:38)=STREAM_LAST_3(I)    !Job to release
    JCL_OUT(3)(18:25)=LOOKUP_RECORD(175:182) ! extraction program name

! Different JCL for MVS and ZFS datasets so choose between them

    DATYPE=LOOKUP_RECORD(1:7)
    DSN_IN=LOOKUP_RECORD(36:62)
! ZFS data set - add name and reset flag to omit MVS lines
! Pathname format is .../subtype/filename
    IF(INDEX(DSN_IN,'/') > 0) THEN
      PATH=DATYPE(1:7)
      IF (DATYPE == 'MSGRDFD') PATH='MSGRADFD'
      IF (DATYPE == 'MSGCPFD') PATH='MSGCTPFD'                       !3
      IF (DATYPE == 'MSGCPUK') PATH='MSGCTPUK'                       !3
      POS=INDEX(JCL_OUT(10),'???')
      JCL_OUT(10)(POS:)=TRIM(PATH) // TRIM(DSN_IN) // QUOTE
      OJCL(5)=.FALSE.
      OJCL(6)=.FALSE.
      OJCL(7)=.FALSE.
! MVS data set - add name and reset flag to omit ZFS lines
    ELSE
      POS=INDEX(JCL_OUT(7),'???')
      JCL_OUT(7)(POS:POS+26)=DSN_IN
      OJCL(8)=.FALSE.
      OJCL(9)=.FALSE.
      OJCL(10)=.FALSE.
    END IF
    JCL_OUT(12)(30:33)=LOOKUP_RECORD(64:67) !CYLs on D'park
    JCL_OUT(13)(26:30)=LOOKUP_RECORD(69:73) !BLKSIZE
    JCL_OUT(14)(19:48)=LOOKUP_RECORD(75:104) !DSN on D'park

    I=INDEX(JCL_OUT(14),'YYMMDD')
    IF (I == 0) THEN
      ARCHIVE_THEM=.FALSE.
      PRINT*, 'Corrupt record in Lookup Table for ',    &
            LOOKUP_RECORD(36:62)
      PRINT*, 'Archiving is not possible from this datastore'
    ELSE
      JCL_OUT(14)(I:I+1)=CYEAR_TO_ARCHIVE(3:4)
      WRITE(JCL_OUT(14)(I+2:I+3),'(I2.2)')MONTH_TO_ARCHIVE
      WRITE(JCL_OUT(14)(I+4:I+5),'(I2.2)')DAY_TO_ARCHIVE

      WRITE(JCL_OUT(16)(1:4),'(I4.4)')YEAR_TO_ARCHIVE
      WRITE(JCL_OUT(16)(5:6),'(I2.2)')MONTH_TO_ARCHIVE
      WRITE(JCL_OUT(16)(7:8),'(I2.2)')DAY_TO_ARCHIVE
      JCL_OUT(16)(10:13)=START_TIME
! find end date
      CENDAY=CENDAY+NPERIOD-1
!24 hour slot spans a midnight
      IF (START_TIME /= '0000') CENDAY=CENDAY+1
      CALL DATE13                                             &
        (CENDAY,DAY_TO_ARCHIVE,MONTH_TO_ARCHIVE,              &
         YEAR_TO_ARCHIVE)

      WRITE(JCL_OUT(16)(16:19),'(I4.4)')YEAR_TO_ARCHIVE
      WRITE(JCL_OUT(16)(20:21),'(I2.2)')MONTH_TO_ARCHIVE
      WRITE(JCL_OUT(16)(22:23),'(I2.2)')DAY_TO_ARCHIVE
      JCL_OUT(16)(25:28)=STOP_TIME
      JCL_OUT(16)(31:35)=LOOKUP_RECORD(69:73) !BLKSIZE
      IF (LOOKUP_RECORD(106:107) == '  ')THEN
        JCL_OUT(16)(36:)=LOOKUP_RECORD(112:141) ! Parms
      ELSE
        JCL_OUT(16)(37:38)=LOOKUP_RECORD(106:107) !index len
        JCL_OUT(16)(40:69)=LOOKUP_RECORD(112:141) !Parms
      END IF

!Diagnostic in case of failure
!           DO I=1,LINES_OF_JCL
!             PRINT*, 'JCL_OUT IS ', JCL_OUT(I)
!           END DO
    END IF
  END IF ToBeArchived

  IF(ARCHIVE_THEM) THEN
! Copy all lines flagged as required
    LC=0
    DO L=1,LINES_OF_JCL
      IF(OJCL(L)) THEN
        LC=LC+1
        JCL_COPY(LC)=JCL_OUT(L)
      END IF
    END DO
    LINES_COPY=LC
    CALL SUBMIT(JCL_COPY,LC,IOSTAT)
    IF(IOSTAT /=0)PRINT*,'Error writing to INTOUT'

    PRINT*, 'Job submitted to copy to Datapark from ',    &
            LOOKUP_RECORD(36:62)
  END IF

!Now put values in arrays for future reference
  IF (ARCHIVE_THEM) THEN  !Values for stage two
    READ(LOOKUP_RECORD(143:144),'(I2)') I !Stream number
    STREAM_TOTALS(I)=STREAM_TOTALS(I)+1
    DATAPARK_NAME(I,STREAM_TOTALS(I))=JCL_OUT(14)(19:48)
    IN_DATAPARK(I,STREAM_TOTALS(I))=LOOKUP_RECORD(212:212)
    DATA_FOR_DATAPARK(I)=DATA_FOR_DATAPARK(I)+1
    JOB_NAME_LAST_3(I,STREAM_TOTALS(I))=LOOKUP_RECORD(29:31)
    ARCJES_NAME(I,STREAM_TOTALS(I))=LOOKUP_RECORD(157:173)
  END IF
  READ(10,'(A250)') LOOKUP_RECORD
END DO DoLookup

LINES_READ=LINES_READ+1+LINES_OF_JCL

!---------------------------------------------------------------------
!The second stage from the Jobs dataset is to copy the data from the
!Datapark to MASS in 'streams' (up to 17) and
!then to scratch the data from the Datapark if the copy was
!successful (COND CODE 0)
!---------------------------------------------------------------------
STREAM_COUNT=0
DO K=1,STREAMS
  IF (STREAM_TOTALS(K) > 0 )THEN
    STREAM_COUNT=STREAM_COUNT+1
    ARSTREAM(STREAM_COUNT)=K
  END IF
END DO
!
!Establish the length of the first and second parts of the array
! and read into LINES_OF_JCL and LINES_OF_JCL2
!
READ(20,REC=LINES_READ+1)CONTROL_LINE
READ(CONTROL_LINE(53:54),'(I2.2)')LINES_OF_JCL
READ(20,REC=LINES_READ+2)CONTROL_LINE2
READ(CONTROL_LINE2(54:55),'(I2.2)')LINES_OF_JCL2
LINES_READF=LINES_READ
!
! loop through all the archive streams.
!
DoStreams: &
DO J=1,STREAM_COUNT
  K=ARSTREAM(J)        !K is the stream number
!
! Increment the pointer to the template to skip
! the first two lines.
!
LINES_READ=LINES_READ+2
!
! Populate the output array with the first part
! of the JCL template
!
  DO I=1,LINES_OF_JCL
    READ(20,REC=LINES_READ+I)JCL_LINE(I)
  END DO
!
! Increase LINES_READ by the lines read in pt1
  LINES_READ=LINES_READ+LINES_OF_JCL
!
!
  HOLD_COUNT=DATA_FOR_DATAPARK(K)  !For the Net
  JCL_LINE(1)(8:10)=STREAM_LAST_3(K)
  IF (J > 1) HOLD_COUNT=HOLD_COUNT+1
  IF (J == STREAM_COUNT) THEN
    JCL_LINE(3)(4:5)='NE' !Un-comment line three
    WRITE(JCL_LINE(3)(25:26),'(I2.2)')HOLD_COUNT
    JCL_LINE(3)(37:39)=STREAM_LAST_3(K)
    JCL_LINE(3)(46:48)=STREAM_LAST_3(K)
  ELSE
    JCL_LINE(2)(4:5)='NE' !Un-comment line two
    WRITE(JCL_LINE(2)(25:26),'(I2.2)')HOLD_COUNT
    JCL_LINE(2)(37:39)=STREAM_LAST_3(K)
    JCL_LINE(2)(46:48)=STREAM_LAST_3(K)
    JCL_LINE(2)(55:57)=STREAM_LAST_3(K+1)
  END IF

!
! Loop through the data types for the archive stream
! Construct the full moose archive string
! to include the data type
!
DoDataTypes: &
  DO T=1,STREAM_TOTALS(K)
    S=0
    WRITE(JCL_LINE(T+LINES_OF_JCL),                          &
              '(A2,A1,I2.2,A23,A1,A2,A8,A30)')               &
               "//",STREAM_LAST_3(K)(1:1),T,                 &
               ARCHIVE_IP,ARCH_ID(K),STREAM_LAST_3(K)(2:3),  &
               ARCHIVE_IP2,DATAPARK_NAME(K,T)
    DO N=0,30
      IF (DATAPARK_NAME(K,T)(N:N) == " ".AND.S == 0) THEN
        S=N
      END IF
    END DO
    S=S+39
    JCL_LINE(T+LINES_OF_JCL)=                                 &
    JCL_LINE(T+LINES_OF_JCL)(1:S-1)//QUOTE
  END DO DoDataTypes
!
! Calculate the offset of the output array.
  Q=STREAM_TOTALS(K)
! Populate the output array with the remaining part
! of the JCL template. Insert the archive stream.
!
! Increment the lines read pointer by 5. This will
! ensure that it points to the start of the second
! part of the template
  LINES_READ=LINES_READ+5
!
  LINES_OF_JCL3=LINES_OF_JCL+Q
  DO P=1,LINES_OF_JCL2
    READ(20,REC=LINES_READ)JCL_LINE(LINES_OF_JCL3+P)
    POS=INDEX(JCL_LINE(LINES_OF_JCL3+P),'???')
    IF (POS > 0)THEN
      JCL_LINE(LINES_OF_JCL3+P)(POS:POS+2)=STREAM_LAST_3(K)
    END IF
    LINES_READ=LINES_READ+1
  END DO

! Reset LINES_OF_JCL include the streams o/p and
! the second part of the array
  LINES_OF_JCL3=LINES_OF_JCL3+LINES_OF_JCL2
!
  UNIT=50+K
  DDNAME='DD:FTxxF001'
  WRITE(DDNAME(6:7),'(I2.2)')UNIT
  OPEN(UNIT,FILE=DDNAME,ACCESS='SEQUENTIAL',FORM='FORMATTED')
  DO M=1,STREAM_TOTALS(K)
    ISP=INDEX(DATAPARK_NAME(K,M),' ')
    IF(ISP == 0)ISP=31
    DS_TO_SCRATCH(11:44)=''''//DATAPARK_NAME(K,M)(1:ISP-1)  &
                             //''''
    WRITE(UNIT,'(A44)')DS_TO_SCRATCH
  END DO
  CLOSE(UNIT)

!Diagnostic in case of failure
!       PRINT*,'START OF JCL OP'
!       DO I=1,LINES_OF_JCL3
!         PRINT*,JCL_LINE(I)
!       END DO

!
! Reset the LINES_READ var to point to the start
! of the JCL template (pt3). Also reset the
! counters for the STREAMS o/p and JCL_LINES3
  LINES_READ=LINES_READF
  Q=0
  CALL SUBMIT(JCL_LINE,LINES_OF_JCL3,IOSTAT)
  IF(IOSTAT /=0)PRINT*,'Error writing to INTOUT'
  LINES_OF_JCL3=0

  WRITE(STREAM,'(I2)')K
  PRINT*, 'Job submitted to copy data in stream ', STREAM,    &
          ' from Datapark to Cartridge'
! PRINT*, 'If the copy is successful, the data are ',         &
!         'scratched from the Datapark'
END DO DoStreams

LINES_READ=LINES_READF
LINES_OF_JCL=LINES_OF_JCL+LINES_OF_JCL2
LINES_READ=LINES_READ+6+LINES_OF_JCL

!---------------------------------------------------------------------
!The third stage from is to submit jobs to check E(JES) output for the
!jobs created in stages one and two
!---------------------------------------------------------------------
READ(20,REC=LINES_READ+1)CONTROL_LINE
READ(CONTROL_LINE(49:50),'(I2.2)')LINES_OF_JCL
DO L=1,STREAM_COUNT !The streams (currently 17)
  K=ARSTREAM(L)
  DO J=1,STREAM_TOTALS(K) !Jobs in each stream
    DO I=1,LINES_OF_JCL
      READ(20,REC=LINES_READ+1+I)JCL_LINE(I+(J-1)*LINES_OF_JCL)
      IF (IN_DATAPARK(K,J) == 'Y')                         &
         JCL_LINE(I+(J-1)*LINES_OF_JCL)='//*' !Comment out
    END DO

    IF (J == 1) THEN
      JCL_LINE(1)(8:10)=STREAM_LAST_3(K)
    ELSE
      JCL_LINE(1+(J-1)*LINES_OF_JCL)='//*' !Comment out
      JCL_LINE(2+(J-1)*LINES_OF_JCL)='//*' !Comment out
      JCL_LINE(3+(J-1)*LINES_OF_JCL)='//*' !Comment out
    END IF

    JCL_LINE(4+(J-1)*LINES_OF_JCL)(7:9)=JOB_NAME_LAST_3(K,J)
    JCL_LINE(7+(J-1)*LINES_OF_JCL)(26:50)=ARCJES_NAME(K,J)
    JCL_LINE(13+(J-1)*LINES_OF_JCL)(11:13)=JOB_NAME_LAST_3(K,J)

!Check if stepname needs updating in Tivoli failure output
    IF(LINES_OF_JCL > 19)THEN
      MINLINE=14+(J-1)*LINES_OF_JCL
      MAXLINE=J*LINES_OF_JCL-1
      DO IJ=MINLINE,MAXLINE
        POS=INDEX(JCL_LINE(IJ),'???.')
        IF(POS > 0)THEN
          JCL_LINE(IJ)(POS:POS+2)=JOB_NAME_LAST_3(K,J)
        END IF
        POS=INDEX(JCL_LINE(IJ),'???.')
        IF(POS > 0)THEN
          JCL_LINE(IJ)(POS:POS+2)=JOB_NAME_LAST_3(K,J)
        END IF
      END DO
    END IF

  END DO !J loop

  ALL_LINES_OF_JCL=LINES_OF_JCL*STREAM_TOTALS(K)
  JCL_LINE(ALL_LINES_OF_JCL)='//'

!Diagnostic in case of failure
!       PRINT*,'STAGE 4 O/P'
!       DO I=1,ALL_LINES_OF_JCL
!         PRINT*, 'JCL_LINE IS ', JCL_LINE(I)
!       END DO

  CALL SUBMIT(JCL_LINE,ALL_LINES_OF_JCL,IOSTAT)
  IF(IOSTAT /=0)PRINT*,'Error writing to INTOUT'

  WRITE(STREAM,'(I2)')K
  PRINT*, 'Job submitted to check (E)JES for jobs in stream ', &
          STREAM
END DO !K loop

LINES_READ=LINES_READ+1+LINES_OF_JCL+1

!The fourth stage from the Jobs dataset is to examine, on E(JES), the
!output from the jobs created in stage three
READ(20,REC=LINES_READ+1)CONTROL_LINE
READ(CONTROL_LINE(49:50),'(I2.2)')LINES_OF_JCL

DO L=1,STREAM_COUNT !The streams (currently 17)
  K=ARSTREAM(L)
  DO I=1,LINES_OF_JCL
    READ(20,REC=LINES_READ+1+I)JCL_LINE(I)
  END DO

  JCL_LINE(01)(08:10)=STREAM_LAST_3(K)
  JCL_LINE(04)(07:09)=STREAM_LAST_3(K)
  JCL_LINE(07)(26:28)=STREAM_LAST_3(K)
  JCL_LINE(13)(11:13)=STREAM_LAST_3(K)

!Check if stepname needs updating in Tivoli failure output
  IF(LINES_OF_JCL > 19)THEN
    DO IJ=14,LINES_OF_JCL
      POS=INDEX(JCL_LINE(IJ),'???.')
      IF(POS > 0)THEN
        JCL_LINE(IJ)(POS:POS+2)=STREAM_LAST_3(K) !Stream name
      END IF
      POS=INDEX(JCL_LINE(IJ),'???.')
      IF(POS > 0)THEN
        JCL_LINE(IJ)(POS:POS+2)=STREAM_LAST_3(K) !Stream name
      END IF
    END DO
  END IF

!Diagnostic in case of failure
!     PRINT*,'STAGE 5 O/P'
!       DO I=1,LINES_OF_JCL
!         PRINT*, 'JCL_LINE IS ', JCL_LINE(I)
!       END DO

  CALL SUBMIT(JCL_LINE,LINES_OF_JCL,IOSTAT)
  IF(IOSTAT /=0)PRINT*,'Error writing to INTOUT'

  PRINT*, 'Job submitted to check MDBDB', STREAM_LAST_3(K)
END DO !K loop

CLOSE(10)
CLOSE(20)
END PROGRAM CARTSUB
