      PROGRAM MDBINITG

!-----------------------------------------------------------------------
!
! PROGRAM       : MDBINITG
!
! PURPOSE       : Initialise MDB index data set for GRIB data storage.
!
! DESCRIPTION   : Data set parameters input in PARM.
!                 Initialisation stops with warning if parameters
!                 not consistent with MDB structures.
!
! DATA TYPE(S)  : Any stored as GRIB bulletins.
!
! CALLS         : PARM
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 28/02/2006 12:07:37$
! $Source: /data/us0400/mdb/op/lib/utility/RCS/mdbinitg.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         28/02/2006 12:07:37    Sheila Needham  
! $
! Revision 2.3  2002/06/10  15:12:28  15:12:28  usmdb (Generic MetDB account)
! 2.3.  17 June 2002.  Brian Barwell.  Change 45/02.
! Put GRIB edition & no. of fields each data time in index header. 
! 
! Revision 2.2  2001/12/05 15:25:45  usmdb
! 2.2.  4 December 2001.  Brian Barwell.
! Alter WRITE statement for FORTRAN 95 compiler on HP.
!
! Revision 2.1  2001/11/12  11:41:00  11:41:00  usmdb (Generic MetDB account)
! Moved SKEL declaration before NAMELIST - S.Cox
!
! Revision 2.0  2001/08/30 10:03:43  usmdb
! pre-processor statements added - S.Cox
!
! Revision 1.1  2000/12/08 15:12:02  usmdb
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!                                                            PARAMETERS

      INTEGER    MAXBLK          ! MAXIMUM BLOCK SIZE
      PARAMETER (MAXBLK=27998)
!                                                             VARIABLES
      INTEGER I                  ! LOCALLY USED POINTER
      INTEGER IB                 ! POINTER TO POSITION IN BLOCK
      INTEGER ISPACES            ! NO. OF SPACES USED TO CENTRE TITLES
      INTEGER J                  ! GENERAL LOOP VARIABLE
      INTEGER JDAY               ! LOOP VARIABLE FOR DAY NUMBER
      INTEGER JMONTH             ! LOOP VARIABLE FOR MONTH NUMBER
      INTEGER LENMON             ! CURRENT ELEMENT OF 'MONLEN'
      INTEGER MONLEN(12)         ! LENGTHS OF NAMES OF MONTHS
      INTEGER NBLOKS             ! NUMBER OF BLOCKS IN DATA SET
      INTEGER NBT                ! NUMBER OF BLOCKS PER TRACK
      INTEGER NGRIB              ! GRIB EDITION NUMBER FOR DATA     !2.3
      INTEGER NIL                ! ZERO                             !2.3
      INTEGER NKEEP              ! REQUIRED RETENTION PERIOD (YEARS)
      INTEGER NMSGS              ! NO. OF GRIB MSGS. EACH DATA TIME !2.3
      INTEGER NSIZE              ! BLOCK SIZE OF DATA SET
      INTEGER NTRKS              ! NUMBER OF TRACKS FOR DATA SET
      INTEGER NUMDAYS(12)        ! NUMBER OF DAYS IN EACH MONTH (FEB=29)

      CHARACTER*(MAXBLK) BLOCK   ! DATA BLOCK
      CHARACTER*132   HEAD       ! REVISION INFORMATION
      CHARACTER*9     MONTH(12)  ! NAMES OF MONTHS
      CHARACTER*44    SKEL       ! SKELETON DATA SET NAME FOR GRIB DATA
#if defined (MVS)
      CHARACTER*80    PARMIN     ! INPUT PARAMETER TEXT
#else
      INTEGER   IERROR
      NAMELIST /STORE_DETAILS/NTRKS,NSIZE,NKEEP,NGRIB,NMSGS,SKEL
#endif

!                                                   DATA INITIALISATION
#if defined (MVS)
      DATA PARMIN /' '/
#endif
      DATA NIL /0/                                                  !2.3
      DATA MONTH /'JANUARY',   'FEBRUARY', 'MARCH',    'APRIL',
     &            'MAY',       'JUNE',     'JULY',     'AUGUST',
     &            'SEPTEMBER', 'OCTOBER',  'NOVEMBER', 'DECEMBER'/
      DATA MONLEN  / 7,  8,  5,  5,  3,  4,  4,  6,  9,  7,  8,  8/
      DATA NUMDAYS /31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
!
!                                                  REVISION INFORMATION
      HEAD='$RCSfile: mdbinitg.F,v $ ' //
     &     '$Revision: 1$ $Date: 28/02/2006 12:07:37$'

!                                 READ PARM AS CHARACTERS, THEN CONVERT
#if defined (MVS)
      CALL PARM (1,PARMIN)
      READ (PARMIN,*) NTRKS, NSIZE, NKEEP, NGRIB, NMSGS, SKEL       !2.3
#else
      OPEN (10,FILE='STORE_DESCRIPTION',IOSTAT=IERROR)
      IF (IERROR.EQ.0) THEN
        READ (10,NML=STORE_DETAILS,IOSTAT=IERROR)
        CLOSE(10)
      ELSE
        WRITE(6,*)'IOSTAT=',IERROR,' reading file STORE_DESCRIPTION'
        STOP
      ENDIF

      OPEN (1,FILE='FTN01',RECL=NSIZE,ACCESS='DIRECT',IOSTAT=IERROR)
      IF (IERROR.NE.0) THEN
        WRITE(6,*)'IOSTAT=',IERROR,' opening file FTN01'
        STOP
      ENDIF  
#endif
!
!                                    COMPUTE NUMBER OF BLOCKS PER TRACK
      IF (NSIZE.GT.32767) THEN
         WRITE (6,'(T6,A)') 'BLOCK SIZE MUST BE NOT MORE THAN 32767.'
         STOP
      ELSE IF (NSIZE.GT.27998) THEN
         NBT = 1
      ELSE IF (NSIZE.GT.18452) THEN
         NBT = 2
      ELSE IF (NSIZE.GT.13682) THEN
         NBT = 3
      ELSE IF (NSIZE.GT.10796) THEN
         NBT = 4
      ELSE IF (NSIZE.GE.9306) THEN
         NBT = 5
      ELSE
!             EACH RECORD CORRESPONDS TO 1 YEAR AND HAS 1 BYTE FOR EACH
!             HOUR + 1 UNUSED BYTE BETWEEN DAYS (=25 BYTES/DAY), MAKING
!             775 (=31*25) BYTES/MONTH AND 9300 (=12*775) BYTES/YEAR.
!             THE YEAR IS HELD IN BYTES 1-4 AND 00Z JANUARY 1 STARTS IN
!             BYTE 7, SO THE RECORD LENGTH IS 9306.
!
         WRITE (6,'(T6,A)') 'BLOCK SIZE MUST BE AT LEAST 9306.'
         STOP
      END IF
!                                  COMPUTE NUMBER OF BLOCKS IN DATA SET
      NBLOKS = NBT*NTRKS
!                                   PRINT FAILURE MESSAGE IF NOT ENOUGH
      I = NKEEP + 3
      IF (NBLOKS.LE.I) THEN
         WRITE (6,'(T6,3(A,I4))') 'INDEX DATA SET MUST HAVE MORE THAN',
     &            I, ' BLOCKS TO RETAIN DATA FOR', NKEEP, ' YEARS.'
         STOP
      END IF
!                                 MAKE DATA SET HEADER BLOCK (RECORD 1)
      BLOCK = ' '
      I = NBLOKS - 3
      NMSGS = 65536*NGRIB + NMSGS                                   !2.3
      BLOCK(21:64) = SKEL

#if defined (MVS)
      WRITE (1) NBLOKS, I, NKEEP, NMSGS, NIL, BLOCK(21:NSIZE)       !2.3
#else
      WRITE (1,REC=1,IOSTAT=IERROR) NBLOKS,I,NKEEP,NMSGS,NIL,
     &                              BLOCK(21:NSIZE)                 !2.3
#endif
!
!                              MAKE BLOCK WITH DATE HEADINGS (RECORD 2)
      BLOCK = ' '
!                                           LOOP OVER MONTHS
      DO JMONTH=1,12
         LENMON = MONLEN(JMONTH)
         ISPACES = 11 - LENMON/2
         IB = 775*(JMONTH-1) + 6  ! (775 = 31 DAYS X 25 BYTES)
         BLOCK(IB:IB) = ':'
!                                           LOOP OVER DAYS
         DO JDAY=1,NUMDAYS(JMONTH)
!                                           MONTH HEADINGS
            I = IB + ISPACES
            BLOCK(I+1:I+LENMON) = MONTH(JMONTH)(1:LENMON)
            I = I + LENMON + 2
!                                           DAY NUMBERS
            IF (JDAY.LT.10) THEN
               WRITE (BLOCK(I:I),'(I1)') JDAY
            ELSE
               WRITE (BLOCK(I:I+1),'(I2)') JDAY
            END IF
            IB = IB + 25
            BLOCK(IB:IB) = ':'
         END DO
      END DO
!                                           WRITE RECORD
#if defined (MVS)
      WRITE (1) BLOCK(1:NSIZE)
#else
      WRITE (1,REC=2,IOSTAT=IERROR) BLOCK(1:NSIZE) 
#endif
!
!                                 MAKE BLOCK WITH HOUR MARKS (RECORD 3)
      BLOCK = ' '
      DO JMONTH=1,12
         IB = 775*(JMONTH-1) + 6  ! (775 = 31 DAYS X 25 BYTES)
         WRITE (BLOCK(IB:),'(40A)')
     *         (' *.....:.....*.....:.....', J=1,NUMDAYS(JMONTH))
      END DO
#if defined (MVS)
      WRITE (1) BLOCK(1:NSIZE)
#else
      WRITE (1,REC=3,IOSTAT=IERROR) BLOCK(1:NSIZE) 
#endif
!
!                            SET OTHER BLOCKS TO BLANKS (RECORDS 4-END)
      BLOCK = ' '
      DO J=4,NBLOKS
#if defined (MVS)
         WRITE (1) BLOCK(1:NSIZE)
#else
         WRITE (1,REC=J,IOSTAT=IERROR) BLOCK(1:NSIZE) 
#endif
      END DO ! J
!                                                PRINT MESSAGE AND STOP
!
      WRITE (6,'(/T6,3A)') 'INDEX DATA SET CREATED.'
      STOP
      END
