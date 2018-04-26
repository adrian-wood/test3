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
!                 Stages 1 and 2 copy 24 hours' worth of data (going
!                 back by the number of days specified in the Lookup
!                 Table) from the on-line Datastore for a Subtype to
!                 the Datapark.  Stage 3 copies the data from the
!                 Datapark to MASS-R in one of 17
!                 streams using MOOSE. If a copy in a stream is
!                 successful (COND CODE 0), the data in that stream
!                 are scratched from the datapark. Stage 4 checks the
!                 (E)JES entries created by stages 1 and 2. Stage 5
!                 checks the (E)JES entries created by stage 3.
!
! Data Type(s)  : Generic (all types archived)
!
! Called by     : Submitted by OPC at 0105Z each morning
!
! Calls         : SUBMIT
!                 DATE13 and DATE31 in ZPDATE
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
! $Revision: 17$
! $Date: 03/11/2009 09:20:52$
! $Workfile: cartsub.f$
!
! CHANGE RECORD :
! $Log:
!  17   Met_DB_Project 1.16        03/11/2009 09:20:52    Sheila Needham
!       Change format of scratch listings for IDCAMS (instead of MSCRATCH)
!  16   Met_DB_Project 1.15        25/08/2009 10:35:40    Sheila Needham  Final
!        revision for MASS-R changes
!  15   Met_DB_Project 1.14        20/08/2009 14:47:09    Richard Weedon
!       latest copy CARTSUBX from mcc3.rw.cntl 
!  14   Met_DB_Project 1.13        20/08/2009 14:37:29    Richard Weedon  copy
!       taken from dbsrclib created 3rd August 2009 Update :- this may be an
!       older version do not use!!!!!!
!       
!  13   Met_DB_Project 1.12        20/08/2009 14:33:14    Richard Weedon
!       updated
!  12   Met_DB_Project 1.11        11/08/2009 15:29:13    Richard Weedon  Order
!        of execution altered in section3. The K loop will now submit jobs in
!       reverse order.
!  11   Met_DB_Project 1.10        11/08/2009 14:50:16    Richard Weedon
!       Latest version FTP from MCC3.RW.CNTL
!       
!  10   Met_DB_Project 1.9         11/08/2009 14:10:18    Richard Weedon  Array
!        arch_id altered to reflect changes in STREAM_LAST_3. Note this is a
!       test version
!  9    Met_DB_Project 1.8         11/08/2009 14:06:08    Richard Weedon  Test
!       version - array STREAM_LAST_3 altered so that the job are submitted in
!        an alternative sequence
!  8    Met_DB_Project 1.7         24/07/2009 14:21:00    Richard Weedon  Do
!       Not Use wrong file enetered
!  7    Met_DB_Project 1.6         24/07/2009 09:02:20    Richard Weedon  Final
!        update
!  6    Met_DB_Project 1.5         22/07/2009 17:07:27    Richard Weedon
!       archive string modified
!  5    Met_DB_Project 1.4         16/07/2009 16:49:18    Richard Weedon
!       removed comment lines from SUBMIT call
!  4    Met_DB_Project 1.3         16/07/2009 16:02:52    Richard Weedon
!       Resubmit
!  3    Met_DB_Project 1.2         16/07/2009 15:57:59    Richard Weedon
!       Section re-written to accomodate the new MOOSE interface.
!       TDB.ARCHIVE.JOBS re-wrtten as part of this process.
!  2    Met_DB_Project 1.1         08/03/2006 11:54:18    John Norton
!       Updated for wrting SYSLOG messages for generation of Remedy Incidents
!       when failures occur.
!  1    Met_DB_Project 1.0         30/01/2006 20:21:38    Sheila Needham  
! $
! Revision 2.5  2003/03/31  15:00:47  15:00:47  usmdb (MetDB account c/o John C
!  Ward)
! Change day 07APR03  R Hirst
! Change the number of streams from 9 to 17.
!
! Revision 2.4  2003/01/14  09:21:13  09:21:13  usmdb (MetDB account c/o usjh)
! Change day 20JAN03  R Hirst
! Add stage 5 which checks the (E)JES output from the nine
! MDBDBARn jobs.
!
! Revision 2.3  2002/09/04  13:57:12  13:57:12  usmdb (Generic MetDB account)
! Change day 16SEP02  R Hirst
! Simplify the handling of non_TESCO subtypes
!
! Revision 2.2  2002/07/16  13:53:26  13:53:26  usmdb (Generic MetDB account)
! Change day 16JUL02  R Hirst
! Convert from using UABRF to using MOUSE/MASS
!
! Revision 2.1  2002/04/30  15:28:41  15:28:41  usmdb (Generic MetDB account)
! Change day 01MAY02  R HIRST
! A complete re-write of this routie
!
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!Declare Integer and PARAMETERise STREAMS
      INTEGER LINES_OF_JCL     !No. of JCL lines in stages 1, 2, 3, 5
      INTEGER LINES_OF_JCL2    !No. of JCL lines in stage3 Pt2 !2.7
      INTEGER LINES_OF_JCL3    !Totals for lines of JCL 1 & 2! !2.7
      INTEGER ALL_LINES_OF_JCL !No. of JCL lines in stage 4
      INTEGER LINES_READ       !No. of lines read so far
      INTEGER LINES_READF      !Start point of section3
      INTEGER CENDAY           !Century Day
      INTEGER I,J,K,IJ,Q,P   !Loop counters                      !2.7
      INTEGER S,N              !char search ind !2.7
      INTEGER*4 T
      INTEGER MINLINE,MAXLINE  !Loop counters                      !2.6
      INTEGER NOW(8)           !System date/time
      INTEGER DAY_NOW          !System day
      INTEGER MONTH_NOW        !System month
      INTEGER YEAR_NOW         !System year
      INTEGER DAYS_BACK        !From Lookup Table
      INTEGER DAY_TO_ARCHIVE   !From system and days back
      INTEGER MONTH_TO_ARCHIVE !From system and days back
      INTEGER YEAR_TO_ARCHIVE  !From system and days back
      INTEGER STREAMS          !Number of streams
      INTEGER POS              !Position of substring               !2.6
      INTEGER ISP              !Position of space in dsn           !ST17
      PARAMETER (STREAMS=17)                                        !2.5

      INTEGER STREAM_TOTALS(STREAMS)     !Subtypes in each stream
      INTEGER DATA_FOR_DATAPARK(STREAMS) !For Datapark in each stream
      INTEGER HOLD_COUNT                 !For the net
      INTEGER UNIT   !Unit number of the DATAPARK.STREAM D/S        !2.2

!Declare character
      CHARACTER*22  ARCHIVE_IP             !Archive string pt1      !2.7
      CHARACTER*8   ARCHIVE_IP2            !Archive string pt2      !2.7
      CHARACTER*1   ARCH_ID(17)            !Archive identifier
      CHARACTER*80  CONTROL_LINE           ! control info
      CHARACTER*80  CONTROL_LINE2          !control line part3      !2.7
      CHARACTER*80  JCL_LINE(2000)         !JCL to submit
      CHARACTER*3   JOB_NAME_LAST_3(STREAMS,99) !In (E)JES
      CHARACTER*30  DATAPARK_NAME(STREAMS,99)   !The full name      !2.2
      CHARACTER*25  ARCJES_NAME(STREAMS,99)     !The full name
      CHARACTER*1   IN_DATAPARK(STREAMS,99)     !'Y' if already there
      CHARACTER*3   STREAM_LAST_3(STREAMS) !Stream name             !2.2
      CHARACTER*4   START_TIME             !Of the 24 hours to archive
      CHARACTER*4   STOP_TIME              !Of the 24 hours to archive
      CHARACTER*132 HEAD                   !Revision information
      CHARACTER*250 LOOKUP_RECORD          !From the lookup table
      CHARACTER*4   CYEAR_TO_ARCHIVE       !Char of YEAR_TO_ARCHIVE
      CHARACTER*2   STREAM                 !Char for PRINT statement!2.5
      CHARACTER*44  DS_TO_SCRATCH          !From Datapark           !2.2

!Declare Logical
      LOGICAL ARCHIVE_THEM         !data not GRIB; to be archived

!Initialize variables
      HEAD='$Workfile: cartsub.f$' //
     &'$Revision: 17$ $Date: 03/11/2009 09:20:52$'

      LINES_OF_JCL=0
      LINES_READ=0
      LOOKUP_RECORD(:)=' '

      DO I=1,STREAMS
        STREAM_TOTALS(I)=0
        DATA_FOR_DATAPARK(I)=0
      ENDDO

      DO J=1,99
        DO I=1,STREAMS
          JOB_NAME_LAST_3(I,J)=' '
          DATAPARK_NAME(I,J)=' '
          ARCJES_NAME(I,J)=' '
          IN_DATAPARK(I,J)=' '
        ENDDO
      ENDDO

!Set stream name for each stream; they all start 'MDB'              !2.2
      STREAM_LAST_3(01)='A01'                                       !2.5
      STREAM_LAST_3(02)='B01'                                       !2.5
      STREAM_LAST_3(03)='C01'                                       !2.5
      STREAM_LAST_3(04)='D01'                                       !2.5
      STREAM_LAST_3(05)='E01'                                       !2.5
      STREAM_LAST_3(06)='A10'                                       !2.5
      STREAM_LAST_3(07)='B10'                                       !2.5
      STREAM_LAST_3(08)='C10'                                       !2.5
      STREAM_LAST_3(09)='D10'                                       !2.5
      STREAM_LAST_3(10)='E10'                                       !2.5
      STREAM_LAST_3(11)='F10'                                       !2.5
      STREAM_LAST_3(12)='G10'                                       !2.5
      STREAM_LAST_3(13)='H10'                                       !2.5
      STREAM_LAST_3(14)='I10'
      STREAM_LAST_3(15)='J10'
      STREAM_LAST_3(16)='K10'
      STREAM_LAST_3(17)='L10'

      ARCH_ID(01)='a'
      ARCH_ID(02)='b'
      ARCH_ID(03)='c'
      ARCH_ID(04)='d'
      ARCH_ID(05)='e'
      ARCH_ID(06)='a'
      ARCH_ID(07)='b'
      ARCH_ID(08)='c'
      ARCH_ID(09)='d'
      ARCH_ID(10)='e'
      ARCH_ID(11)='f'
      ARCH_ID(12)='g'
      ARCH_ID(13)='h'
      ARCH_ID(14)='i'
      ARCH_ID(15)='j'
      ARCH_ID(16)='k'
      ARCH_ID(17)='l'


      DS_TO_SCRATCH(1:10)='   DELETE '                          !ST17

      ARCHIVE_IP='EXEC ARCHFILE,STREAM='''
      ARCHIVE_IP2=''',NAME='''

!Open the Lookup Table (10) and the Jobs Dataset (20)
      OPEN(10,ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ')
      OPEN(20,ACCESS='DIRECT',RECL=80,ACTION='READ')

!Get the system date/time
      CALL DATIM(NOW)
      DAY_NOW=NOW(6)
      MONTH_NOW=NOW(7)
      YEAR_NOW=NOW(8)

!Skip the preliminary lines of comment in the Lookup Table
      DO WHILE (LOOKUP_RECORD(1:6) .NE. 'LOOKUP')
        READ(10,'(A250)') LOOKUP_RECORD
      END DO

!Skip the preliminary lines of comment in the Jobs Dataset
      READ(20,REC=1)CONTROL_LINE
      READ(CONTROL_LINE(56:57),'(I2.2)')LINES_READ

!The first stage from the Jobs Dataset is to get 24 hours' worth of all
!''non TESCO' subtypes not already in the Datapark (most) copied to the
!Datapark. First, determine the number of JCL lines in this stage.
      READ(20,REC=LINES_READ+1)CONTROL_LINE
      READ(CONTROL_LINE(49:50),'(I2.2)')LINES_OF_JCL
      LINES_READ=LINES_READ+1

!Loop round all the 'non TESCO' records in the Lookup Table. Read them
!into a char string; most fields may then be copied straight to the
!appropriate line of JCL as chars. The 'days back' field needs to be
!converted to integer before calling DATE31 and DATE13. A description
!of each field appears in the preliminary lines of comment in the
!Lookup Table (MDB.ARCHIVE.LOOKUP).
      PRINT*, 'Looping round the non-TESCO records in the Lookup Table'
      DO WHILE (LOOKUP_RECORD(1:6) .NE. 'END OF')
        READ(10,'(A250)') LOOKUP_RECORD
        IF (LOOKUP_RECORD(1:6).NE.'END OF') THEN
          ARCHIVE_THEM=.TRUE.
          IF (LOOKUP_RECORD(212:212) .NE. 'Y') THEN  !Not in Datapark

!JCL lines are read for each subtype for the Datapark
            DO I=1,LINES_OF_JCL
              READ(20,REC=LINES_READ+I)JCL_LINE(I)
            ENDDO

            READ(LOOKUP_RECORD(15:16),'(I2.2)') DAYS_BACK
            CALL DATE31(DAY_NOW,MONTH_NOW,YEAR_NOW,CENDAY)
            CENDAY=CENDAY-DAYS_BACK
            CALL DATE13
     &        (CENDAY,DAY_TO_ARCHIVE,MONTH_TO_ARCHIVE,YEAR_TO_ARCHIVE)
            WRITE(CYEAR_TO_ARCHIVE,'(I4.4)')YEAR_TO_ARCHIVE

            START_TIME=LOOKUP_RECORD(18:21)
            STOP_TIME=LOOKUP_RECORD(24:27)
            JCL_LINE(1)(8:10)=LOOKUP_RECORD(29:31) !Job name (last 3)
            READ(LOOKUP_RECORD(143:144),'(I2)') I !Stream number    !2.5
            JCL_LINE(2)(36:38)=STREAM_LAST_3(I)   !Job to release   !2.5
            JCL_LINE(6)(14:40)=LOOKUP_RECORD(36:62) !Datastore name
            JCL_LINE(8)(30:33)=LOOKUP_RECORD(64:67) !CYLs on D'park !2.3
            JCL_LINE(9)(26:30)=LOOKUP_RECORD(69:73) !BLKSIZE
            JCL_LINE(10)(19:48)=LOOKUP_RECORD(75:104) !DSN on D'park!2.3

            I=INDEX(JCL_LINE(10),'YYMMDD')                          !2.3
            IF (I.EQ.0) THEN
              ARCHIVE_THEM=.FALSE.
              PRINT*, 'Corrupt record in Lookup Table for ',
     &                LOOKUP_RECORD(36:62)
              PRINT*, 'Archiving is not possible from this datastore'
            ELSE
              JCL_LINE(10)(I:I+1)=CYEAR_TO_ARCHIVE(3:4)             !2.3
              WRITE(JCL_LINE(10)(I+2:I+3),'(I2.2)')MONTH_TO_ARCHIVE !2.3
              WRITE(JCL_LINE(10)(I+4:I+5),'(I2.2)')DAY_TO_ARCHIVE   !2.3

              WRITE(JCL_LINE(12)(1:4),'(I4.4)')YEAR_TO_ARCHIVE      !2.3
              WRITE(JCL_LINE(12)(5:6),'(I2.2)')MONTH_TO_ARCHIVE     !2.3
              WRITE(JCL_LINE(12)(7:8),'(I2.2)')DAY_TO_ARCHIVE       !2.3
              JCL_LINE(12)(10:13)=START_TIME                        !2.3
              IF (START_TIME.NE.'0000') THEN
!24 hour slot spans a midnight
                CENDAY=CENDAY+1
                CALL DATE13
     &            (CENDAY,DAY_TO_ARCHIVE,MONTH_TO_ARCHIVE,
     &             YEAR_TO_ARCHIVE)
              ENDIF

              WRITE(JCL_LINE(12)(16:19),'(I4.4)')YEAR_TO_ARCHIVE    !2.3
              WRITE(JCL_LINE(12)(20:21),'(I2.2)')MONTH_TO_ARCHIVE   !2.3
              WRITE(JCL_LINE(12)(22:23),'(I2.2)')DAY_TO_ARCHIVE     !2.3
              JCL_LINE(12)(25:28)=STOP_TIME                         !2.3
              JCL_LINE(12)(31:35)=LOOKUP_RECORD(69:73) !BLKSIZE     !2.3
              JCL_LINE(12)(37:38)=LOOKUP_RECORD(106:107) !Index len !2.3
              JCL_LINE(12)(40:69)=LOOKUP_RECORD(112:141) !Parms     !2.3

!Diagnostic in case of failure
!             DO I=1,LINES_OF_JCL
!               PRINT*, 'JCL_LINE IS ', JCL_LINE(I)
!             END DO

              CALL SUBMIT(JCL_LINE,LINES_OF_JCL)

              PRINT*, 'Job submitted to copy to Datapark from ',
     &                LOOKUP_RECORD(36:62)
            ENDIF
          ENDIF

!Now put values in arrays for future reference
          IF (ARCHIVE_THEM) THEN  !Values for stage three
            READ(LOOKUP_RECORD(143:144),'(I2)') I !Stream number    !2.5
            STREAM_TOTALS(I)=STREAM_TOTALS(I)+1
            DATAPARK_NAME(I,STREAM_TOTALS(I))=JCL_LINE(10)(19:48)   !2.3
            IN_DATAPARK(I,STREAM_TOTALS(I))=LOOKUP_RECORD(212:212)

            IF (LOOKUP_RECORD(212:212) .NE. 'Y') THEN  !Stage four
              DATA_FOR_DATAPARK(I)=DATA_FOR_DATAPARK(I)+1
              JOB_NAME_LAST_3(I,STREAM_TOTALS(I))=LOOKUP_RECORD(29:31)
              ARCJES_NAME(I,STREAM_TOTALS(I))=LOOKUP_RECORD(157:173)
            ENDIF
          ENDIF
        ENDIF
      END DO !'WHILE' loop

      LINES_READ=LINES_READ+1+LINES_OF_JCL

!Skip any blank lines in the Lookup Table between the non TESCO
!and TESCO sections
      DO WHILE (LOOKUP_RECORD(1:6) .NE. 'LOOKUP')
        READ(10,'(A250)') LOOKUP_RECORD
      END DO

!The second stage from the Jobs Dataset is to get 24 hours' worth of
!all TESCO subtypes not already in the Datapark (currently all)
!copied to the Datapark. First, determine the number of JCL lines
!in this stage.
      READ(20,REC=LINES_READ+1)CONTROL_LINE
      READ(CONTROL_LINE(49:50),'(I2.2)')LINES_OF_JCL

!Loop round all the TESCO records in the Lookup Table
      PRINT*, ' '
      PRINT*, 'Looping round the TESCO records in the Lookup Table'
      DO WHILE (LOOKUP_RECORD(1:6) .NE. 'END OF')
        READ(10,'(A250)') LOOKUP_RECORD
        IF (LOOKUP_RECORD(1:6).NE.'END OF') THEN
          ARCHIVE_THEM=.TRUE.
          IF (LOOKUP_RECORD(212:212) .NE. 'Y') THEN

!JCL lines are read for each subtype for the Datapark
            DO I=1,LINES_OF_JCL
              READ(20,REC=LINES_READ+1+I)JCL_LINE(I)
            ENDDO

            READ(LOOKUP_RECORD(15:16),'(I2.2)') DAYS_BACK
            CALL DATE31(DAY_NOW,MONTH_NOW,YEAR_NOW,CENDAY)
            CENDAY=CENDAY-DAYS_BACK
            CALL DATE13
     &        (CENDAY,DAY_TO_ARCHIVE,MONTH_TO_ARCHIVE,YEAR_TO_ARCHIVE)
            WRITE(CYEAR_TO_ARCHIVE,'(I4.4)')YEAR_TO_ARCHIVE

            START_TIME=LOOKUP_RECORD(18:21)
            STOP_TIME=LOOKUP_RECORD(24:27)
            JCL_LINE(1)(8:10)=LOOKUP_RECORD(29:31) !Job name (last 3)
            READ(LOOKUP_RECORD(143:144),'(I2)') I !Stream number    !2.5
            JCL_LINE(2)(36:38)=STREAM_LAST_3(I)   !Job to release   !2.5
            JCL_LINE(6)(14:40)=LOOKUP_RECORD(36:62) !Datastore name
            JCL_LINE(8)(16:19)=LOOKUP_RECORD(64:67) !CYLs in Datapark
            JCL_LINE(8)(48:52)=LOOKUP_RECORD(69:73) !BLKSIZE
            JCL_LINE(9)(14:43)=LOOKUP_RECORD(75:104) !DSN in Datapark

            I=INDEX(JCL_LINE(9),'YYMMDD')
            IF (I.EQ.0) THEN
              ARCHIVE_THEM=.FALSE.
              PRINT*, 'Corrupt record in Lookup Table for ',
     &                LOOKUP_RECORD(36:62)
              PRINT*, 'Archiving is not possible from this datastore'
            ELSE
              JCL_LINE(9)(I:I+1)=CYEAR_TO_ARCHIVE(3:4)
              WRITE(JCL_LINE(9)(I+2:I+3),'(I2.2)')MONTH_TO_ARCHIVE
              WRITE(JCL_LINE(9)(I+4:I+5),'(I2.2)')DAY_TO_ARCHIVE

              WRITE(JCL_LINE(11)(1:4),'(I4.4)')YEAR_TO_ARCHIVE
              WRITE(JCL_LINE(11)(5:6),'(I2.2)')MONTH_TO_ARCHIVE
              WRITE(JCL_LINE(11)(7:8),'(I2.2)')DAY_TO_ARCHIVE
              JCL_LINE(11)(10:13)=START_TIME
              IF (START_TIME.NE.'0000') THEN
!24 hour slot spans a midnight
                CENDAY=CENDAY+1
                CALL DATE13
     &            (CENDAY,DAY_TO_ARCHIVE,MONTH_TO_ARCHIVE,
     &             YEAR_TO_ARCHIVE)
              ENDIF

              WRITE(JCL_LINE(11)(16:19),'(I4.4)')YEAR_TO_ARCHIVE
              WRITE(JCL_LINE(11)(20:21),'(I2.2)')MONTH_TO_ARCHIVE
              WRITE(JCL_LINE(11)(22:23),'(I2.2)')DAY_TO_ARCHIVE
              JCL_LINE(11)(25:28)=STOP_TIME
              JCL_LINE(11)(31:35)=LOOKUP_RECORD(69:73) !BLKSIZE
              JCL_LINE(11)(37:66)=LOOKUP_RECORD(112:141) !Parms if any

!Diagnostic in case of failure
!             DO I=1,LINES_OF_JCL
!               PRINT*, 'JCL_LINE IS ', JCL_LINE(I)
!             END DO

              CALL SUBMIT(JCL_LINE,LINES_OF_JCL)

              PRINT*, 'Job submitted to copy to Datapark from ',
     &                LOOKUP_RECORD(36:62)
            ENDIF
          ENDIF

!Now put values in arrays for future reference
          IF (ARCHIVE_THEM) THEN  !Values for stage three
            READ(LOOKUP_RECORD(143:144),'(I2)') I !Stream number    !2.5
            STREAM_TOTALS(I)=STREAM_TOTALS(I)+1
            DATAPARK_NAME(I,STREAM_TOTALS(I))=JCL_LINE(9)(14:43)    !2.2
            IN_DATAPARK(I,STREAM_TOTALS(I))=LOOKUP_RECORD(212:212)

            IF (LOOKUP_RECORD(212:212) .NE. 'Y') THEN  !Stage four
              DATA_FOR_DATAPARK(I)=DATA_FOR_DATAPARK(I)+1
              JOB_NAME_LAST_3(I,STREAM_TOTALS(I))=LOOKUP_RECORD(29:31)
              ARCJES_NAME(I,STREAM_TOTALS(I))=LOOKUP_RECORD(157:173)
            ENDIF
          ENDIF
        ENDIF
      END DO !'WHILE' loop

      PRINT*, ' '
      LINES_READ=LINES_READ+1+LINES_OF_JCL+1

!The third stage from the Jobs dataset is to copy the data from the
!Datapark to 36 track cartridges in 'streams' (currently 17) and
!then to scratch the data from the Datapark if the copy was
!successful (COND CODE 0)
!
!Establish the length of the first and second parts of the array
! and read into LINES_OF_JCL and LINES_OF_JCL2
!
      READ(20,REC=LINES_READ+1)CONTROL_LINE
      READ(CONTROL_LINE(53:54),'(I2.2)')LINES_OF_JCL
      READ(20,REC=LINES_READ+2)CONTROL_LINE2                        !2.7
      READ(CONTROL_LINE2(54:55),'(I2.2)')LINES_OF_JCL2              !2.7
      LINES_READF=LINES_READ                                        !2.7
!
! loop through all the archive streams.
!
      DO K=1,STREAMS !The 'streams'
!
! Increment the pointer to the template to skip                     !2.7
! the first two lines.                                              !2.7
!
      LINES_READ=LINES_READ+2                                       !2.7
!
! Populate the output array with the first part                     !2.7
! of the JCL template                                               !2.7
!                                                                   !2.7
        DO I=1,LINES_OF_JCL                                         !2.7
          READ(20,REC=LINES_READ+I)JCL_LINE(I)                      !2.7
        ENDDO                                                       !2.7
!
! Increase LINES_READ by the lines read in pt1                      !2.7
        LINES_READ=LINES_READ+LINES_OF_JCL                          !2.7
!
!
        HOLD_COUNT=DATA_FOR_DATAPARK(K)  !For the Net
        JCL_LINE(1)(8:10)=STREAM_LAST_3(K)                          !2.5
        IF (K.GT.1) HOLD_COUNT=HOLD_COUNT+1
        IF (K.EQ.STREAMS) THEN
          JCL_LINE(3)(4:5)='NE' !Un-comment line three
          WRITE(JCL_LINE(3)(25:26),'(I2.2)')HOLD_COUNT
          JCL_LINE(3)(37:39)=STREAM_LAST_3(K)                       !2.5
          JCL_LINE(3)(46:48)=STREAM_LAST_3(K)                       !2.5
        ELSE
          JCL_LINE(2)(4:5)='NE' !Un-comment line two
          WRITE(JCL_LINE(2)(25:26),'(I2.2)')HOLD_COUNT
          JCL_LINE(2)(37:39)=STREAM_LAST_3(K)                       !2.5
          JCL_LINE(2)(46:48)=STREAM_LAST_3(K)                       !2.5
          JCL_LINE(2)(55:57)=STREAM_LAST_3(K+1)                     !2.5
        ENDIF

!
!
! Loop through the data types for the archive stream                !2.7
! Construct the full moose archive string
! to include the data type
!
        DO T=1,STREAM_TOTALS(K)                                     !2.7
        S=0                                                         !2.7
        WRITE(JCL_LINE(T+LINES_OF_JCL),'(A2,A1,I2.2,
     &  A23,A1,A2,A8,A30)')"//",STREAM_LAST_3(K)(1:1),T,
     &  ARCHIVE_IP,ARCH_ID(K),STREAM_LAST_3(K)(2:3),
     &  ARCHIVE_IP2,DATAPARK_NAME(K,T)
        DO N=0,30
        IF (DATAPARK_NAME(K,T)(N:N).EQ." ".AND.
     &  S.EQ.0) THEN
          S=N
         ENDIF
        END DO
        S=S+39
        JCL_LINE(T+LINES_OF_JCL)=
     &  JCL_LINE(T+LINES_OF_JCL)(1:S-1)//"'"
        END DO
!
! Calculate the offset of the output array.                         !2.7
        Q=STREAM_TOTALS(K)                                          !2.7
! Populate the output array with the remaining part                 !2.7
! of the JCL template. Insert the archive stream.                   !2.7
!                                                                   !2.7
! Increment the lines read pointer by 5. This will                  !2.7
! ensure that it points to the start of the second                  !2.7
! part of the template                                              !2.7
        LINES_READ=LINES_READ+5                                     !2.7
!
        LINES_OF_JCL3=LINES_OF_JCL+Q                                !2.7
        DO P=1,LINES_OF_JCL2                                        !2.7
        READ(20,REC=LINES_READ)JCL_LINE(LINES_OF_JCL3+P)            !2.7
        POS=INDEX(JCL_LINE(LINES_OF_JCL3+P),'???')                  !2.7
        IF (POS.GT.0)THEN                                           !2.7
         JCL_LINE(LINES_OF_JCL3+P)(POS:POS+2)=STREAM_LAST_3(K)      !2.7
        ENDIF                                                       !2.7
        LINES_READ=LINES_READ+1                                     !2.7
        END DO

! Reset LINES_OF_JCL include the streams o/p and                    !2.7
! the second part of the array                                      !2.7
        LINES_OF_JCL3=LINES_OF_JCL3+LINES_OF_JCL2                   !2.7
!
!
        UNIT=50+K
        OPEN(UNIT,ACCESS='SEQUENTIAL',FORM='FORMATTED')             !2.2
        DO J=1,STREAM_TOTALS(K)                                     !2.2
          ISP=INDEX(DATAPARK_NAME(K,J),' ')                       !ST17
          IF(ISP.EQ.0)ISP=31                                      !ST17
          DS_TO_SCRATCH(11:44)=''''//DATAPARK_NAME(K,J)(1:ISP-1)  !ST17
     &                             //''''                         !ST17
          WRITE(UNIT,'(A44)')DS_TO_SCRATCH                          !2.2
        END DO                                                      !2.2
        CLOSE(UNIT)                                                 !2.2

!Diagnostic in case of failure
!       PRINT*,'START OF JCL OP'
!       DO I=1,LINES_OF_JCL3
!         PRINT*,JCL_LINE(I)
!       END DO

!
! Reset the LINES_READ var to point to the start                   !2.7
! of the JCL template (pt3). Also reset the                        !2.7
! counters for the STREAMS o/p and JCL_LINES3                      !2.7
        LINES_READ=LINES_READF                                     !2.7
        Q=0                                                        !2.7
        CALL SUBMIT(JCL_LINE,LINES_OF_JCL3)
        LINES_OF_JCL3=0                                            !2.7

        WRITE(STREAM,'(I2)')K                                       !2.5
        PRINT*, 'Job submitted to copy data in stream ', STREAM,
     &          ' from Datapark to Cartridge'
        PRINT*, 'If the copy is successful, the data are ',         !2.3
     &          'scratched from the Datapark'                       !2.3
      END DO !K LOOP

      LINES_READ=LINES_READF                                        !2.7
      LINES_OF_JCL=LINES_OF_JCL+LINES_OF_JCL2                       !2.7
      LINES_READ=LINES_READ+6+LINES_OF_JCL                          !2.7

!The fourth stage from the Jobs dataset is to examine, on E(JES), the
!output from the jobs created in stages one and two
      READ(20,REC=LINES_READ+1)CONTROL_LINE
      PRINT*,'CONTROL LINE'
      PRINT*,CONTROL_LINE
      READ(CONTROL_LINE(49:50),'(I2.2)')LINES_OF_JCL
      PRINT*,'Stage four lines of JCL'
      PRINT*,LINES_OF_JCL
      DO K=1,STREAMS !The streams (currently 17)
        DO J=1,STREAM_TOTALS(K) !Jobs in each stream
          DO I=1,LINES_OF_JCL
            READ(20,REC=LINES_READ+1+I)JCL_LINE(I+(J-1)*LINES_OF_JCL)
            IF (IN_DATAPARK(K,J).EQ.'Y')
     &         JCL_LINE(I+(J-1)*LINES_OF_JCL)='//*' !Comment out
          ENDDO

          IF (J.EQ.1) THEN
            JCL_LINE(1)(8:10)=STREAM_LAST_3(K)                      !2.5
          ELSE
            JCL_LINE(1+(J-1)*LINES_OF_JCL)='//*' !Comment out
            JCL_LINE(2+(J-1)*LINES_OF_JCL)='//*' !Comment out
            JCL_LINE(3+(J-1)*LINES_OF_JCL)='//*' !Comment out
          ENDIF

          JCL_LINE(4+(J-1)*LINES_OF_JCL)(7:9)=JOB_NAME_LAST_3(K,J)
          JCL_LINE(7+(J-1)*LINES_OF_JCL)(26:50)=ARCJES_NAME(K,J)
          JCL_LINE(13+(J-1)*LINES_OF_JCL)(11:13)=JOB_NAME_LAST_3(K,J)

!Check if stepname needs updating in Tivoli failure output          !2.6
        IF(LINES_OF_JCL.GT.19)THEN                                  !2.6
          MINLINE=14+(J-1)*LINES_OF_JCL                             !2.6
          MAXLINE=J*LINES_OF_JCL-1                                  !2.6
          DO IJ=MINLINE,MAXLINE                                     !2.6
            POS=INDEX(JCL_LINE(IJ),'???.')                          !2.6
            IF(POS.GT.0)THEN                                        !2.6
              JCL_LINE(IJ)(POS:POS+2)=JOB_NAME_LAST_3(K,J)          !2.6
            ENDIF                                                   !2.6
            POS=INDEX(JCL_LINE(IJ),'???.')                          !2.6
            IF(POS.GT.0)THEN                                        !2.6
              JCL_LINE(IJ)(POS:POS+2)=JOB_NAME_LAST_3(K,J)          !2.6
            ENDIF                                                   !2.6
          END DO                                                    !2.6
        ENDIF                                                       !2.6

        END DO !J loop

        ALL_LINES_OF_JCL=LINES_OF_JCL*STREAM_TOTALS(K)
        JCL_LINE(ALL_LINES_OF_JCL)='//'

!Diagnostic in case of failure
!       PRINT*,'STAGE 4 O/P'
!       DO I=1,ALL_LINES_OF_JCL
!         PRINT*, 'JCL_LINE IS ', JCL_LINE(I)
!       END DO

        CALL SUBMIT(JCL_LINE,ALL_LINES_OF_JCL)

        WRITE(STREAM,'(I2)')K                                       !2.5
        PRINT*, 'Job submitted to check (E)JES for jobs in stream ',
     &          STREAM
      END DO !K loop

      PRINT*, ' '
      LINES_READ=LINES_READ+1+LINES_OF_JCL+1

!The fifth stage from the Jobs dataset is to examine, on E(JES), the
!output from the jobs created in stage three
      READ(20,REC=LINES_READ+1)CONTROL_LINE                         !2.4
      READ(CONTROL_LINE(49:50),'(I2.2)')LINES_OF_JCL                !2.4

      DO K=1,STREAMS !The streams (currently 17)                    !2.4

        DO I=1,LINES_OF_JCL                                         !2.4
          READ(20,REC=LINES_READ+1+I)JCL_LINE(I)                    !2.4
        ENDDO                                                       !2.4

        JCL_LINE(01)(08:10)=STREAM_LAST_3(K)                        !2.5
        JCL_LINE(04)(07:09)=STREAM_LAST_3(K)                        !2.5
        JCL_LINE(07)(26:28)=STREAM_LAST_3(K)                        !2.5
        JCL_LINE(13)(11:13)=STREAM_LAST_3(K)                        !2.5

!Check if stepname needs updating in Tivoli failure output          !2.6
        IF(LINES_OF_JCL.GT.19)THEN                                  !2.6
          DO IJ=14,LINES_OF_JCL                                     !2.6
            POS=INDEX(JCL_LINE(IJ),'???.')                          !2.6
            IF(POS.GT.0)THEN                                        !2.6
              JCL_LINE(IJ)(POS:POS+2)=STREAM_LAST_3(K) !Stream name !2.6
            ENDIF                                                   !2.6
            POS=INDEX(JCL_LINE(IJ),'???.')                          !2.6
            IF(POS.GT.0)THEN                                        !2.6
              JCL_LINE(IJ)(POS:POS+2)=STREAM_LAST_3(K) !Stream name !2.6
            ENDIF                                                   !2.6
          END DO                                                    !2.6
        ENDIF                                                       !2.6


!Diagnostic in case of failure
!     PRINT*,'STAGE 5 O/P'
!       DO I=1,LINES_OF_JCL                                         !2.4
!         PRINT*, 'JCL_LINE IS ', JCL_LINE(I)                       !2.4
!       END DO                                                      !2.4

        CALL SUBMIT(JCL_LINE,LINES_OF_JCL)                          !2.4

        PRINT*, 'Job submitted to check MDBDB', STREAM_LAST_3(K)    !2.5
      END DO !K loop                                                !2.4

      CLOSE(10)
      CLOSE(20)
      STOP
      END
