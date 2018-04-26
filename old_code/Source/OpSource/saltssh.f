      PROGRAM SALTSSH

!-----------------------------------------------------------------------
!
! PROGRAM       : SALTSSH
!
! PURPOSE       : to store BUFR messages made from satellite altimeter
!                 sea surface heights (anomalies)
!
! CALLED BY     : nothing
!
! CALLS         : MHSIFFC, LOCALD, DATE13, DATE31,                  !ST2
!                 ENBUFR, LATBOX, AIRSTO
!
! FILES         : FT01 - Housekeeping data set (for termination flag)
!                 FT10 - Input file found by MHSIFFC
!                 FT20 - Storage data set
!
! REVISION INFO :
!
! $Revision: 7$
! $Date: 08/06/2012 17:47:59$
! $Workfile: saltssh.f$
! $Folder: OpSource$
!
! CHANGE RECORD :
!
! $Log:
!  7    Met_DB_Project 1.6         08/06/2012 17:47:59    Sheila Needham  Added
!        extra namelist input; updated for F95 build (but not completed
!       converted to Fortran 90)
!  6    Met_DB_Project 1.5         15/03/2010 16:12:21    Richard Weedon
!       default value set to SALT
!  5    Met_DB_Project 1.4         04/03/2010 15:16:55    Richard Weedon
!       updated for new storage job
!  4    Met_DB_Project 1.3         03/03/2010 15:31:10    Richard Weedon  Check
!        for instor data added. This to be used against real time data type.
!  3    Met_DB_Project 1.2         28/01/2009 16:57:10    Brian Barwell
!       Expand satellite arrays to include JASON-2 (WMO code 261).
!  2    Met_DB_Project 1.1         13/12/2006 10:31:25    Stan Kellett    code
!       amended to allow for data coming in every day rather than just twice a
!        week.
!  1    Met_DB_Project 1.0         30/01/2006 20:24:04    Sheila Needham  
! $
! Revision 2.2  2004/02/02 11:32:13  usmdb
! 19th January 2004. Stanley Kellett. Change form 05/04.
! One line change  to code along with change to comments to
!  allow
! for the data being sent in twice a week.
!
! Revision 2.1  2003/02/13  15:44:07  15:44:07  usmdb (MetDB account c/o usjh)
! 17 March 2003     C Long
! 2.1  Set satellite identifiers for Jason-1, GFO & Envisat.
!
! Revision 2.0  2001/10/29  10:16:09  10:16:09  usmdb (MetDB account c/o usjh)
! initial revision
!
! INTRODUCED    : 29-10-01
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2009 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      USE zpdate_mod
      USE locald_mod
      USE enbufr_mod
      USE ides_mod
      USE latbox_mod

      IMPLICIT NONE

      INTEGER MAXNOBS
      INTEGER MAXNID            ! Number of satellite IDs recognised  !3
      PARAMETER (MAXNOBS=250)
      PARAMETER (MAXNID=15)                                          !3

      REAL OB(MAXNOBS*9)       ! ident, year,...,minute, lat, long, SSH
      REAL SATNUM
      REAL RLAT
      REAL RLONG
      REAL SSH
      REAL FRACTION_OF_DAY     ! fractional part of DAYS
      REAL BOX(4)              ! lat/long box for index
      DOUBLE PRECISION DAYS    ! days since 1 Jan 1950 (with fraction)

      CHARACTER DSN*44         ! DSN of input file
      CHARACTER OWNER*8        ! 4th level of input DSN (for search)
      CHARACTER HLQ*4          ! 4th char of MHS dataset name 'R'
      CHARACTER STAT*4         ! MHSIFF mode 'get' or 'ren' or 'del'
      CHARACTER ENTRY*23       ! index entry
      CHARACTER IDENT*9        ! identifier (to go in index entry)
      CHARACTER FMT*30         ! format statment for MHS d/s
      CHARACTER*9 IDENTS(MAXNID)  ! identifiers for index entries
      CHARACTER MESSAGE*2500   ! BUFR message
      CHARACTER JOBNAME*25     ! Jobname from INSTOR
      CHARACTER CH*1           ! dummy name string for ENBUFR
      CHARACTER DDUNIT*11      ! FT unit for main d/s
      CHARACTER SEQREQ*27998   ! sequence record from storage data set
      CHARACTER HKFLAGS*4      ! flags in housekeeping data set
      CHARACTER HEAD*80                                            !ST2

      LOGICAL MHSERROR         ! set if MHSRC error
      LOGICAL STOPNOW          ! set if termination flag set in HKDS
      LOGICAL NEWHOUR          ! set when hour of data changes

      INTEGER BUFNUM(MAXNID)  ! satellite numbers as in 001007      !3
      INTEGER IFT              ! FT number for output
      INTEGER ILEN             ! Length of MHS dsn
      INTEGER IOFLAG           ! I/O status flag
      INTEGER IOPEN            ! Open mode
      INTEGER IORC             ! return code from READ
      INTEGER IOS              ! return code from READ
      INTEGER IVER             ! Table B version number
      INTEGER MHSRC            ! return code from MHSIFFC
      INTEGER BLKSIZ           ! blocksize of storage data set
      INTEGER DESCR(20)        ! descriptors (with room for expansion)
      INTEGER ND               ! descriptor count (changed by ENBUFR)
      INTEGER NELEM            ! dimension of OB array for ENBUFR
      INTEGER NID              ! no. of possible satellites
      INTEGER NOBS             ! number of obs in BUFR message
      INTEGER NSTORED          ! number of obs stored (from all files)
      INTEGER OLDDAY           ! day of last ob
      INTEGER OLDHOUR          ! hour of last ob
      INTEGER I,J              ! loop variables
      INTEGER L                ! length of BUFR messsage
      INTEGER NUMREC           ! record number
      INTEGER YEAR,MONTH,DAY   ! date of data
      INTEGER HOUR,MINUTE      ! time of data
      INTEGER NOW(8)           ! current time (year in NOW(8)...)
      INTEGER TOR(5)           ! current date/time (year, month...)
      INTEGER CENDAY           ! current century-day (time of receipt)
      INTEGER DAYSOLD          ! days from data time to current time
      INTEGER DATIME(5)        ! date/time of data
      INTEGER FIFTY_YEARS      ! days from 1 Jan 1900 to 1 Jan 1950:
                               ! 50*365+12 days, because only
! -------------------Namelist ------------------------------------
      NAMELIST /INSTOR/ JOBNAME,OWNER
      NAMELIST /SATID/ NID,IDENTS,BUFNUM
! -------------------Namelist ------------------------------------
      DATA FIFTY_YEARS/18262/  ! 12 leap years in 1904-1948 (1900 not!)
      DATA BLKSIZ/27998/
      DATA MHSERROR/.FALSE./
      DATA STOPNOW/.FALSE./
      DATA NSTORED/0/
      DATA IFT/20/             ! storage data set FT number
      DATA IVER/13/
      DATA FMT/'(F3.1,F10.3,F10.5,F11.5,E13.4)'/

      HEAD='$Workfile: saltssh.f$ ' //
     &     '$Revision: 7$ $Date: 08/06/2012 17:47:59$'

! Open housekeeping data set to check for termination flag.

      OPEN (1,FILE='DD:HKEEP',ACCESS='DIRECT',RECL=950,ACTION='READ')

! Open storage data set, read local BUFR sequence & keep it for encode.

      IOFLAG=0
      IOPEN=3   ! read/write access
      DDUNIT='DD:FTxxF001'
      WRITE(DDUNIT(6:7),'(I2.2)')IFT
      CALL METDB_COPEN(IFT,DDUNIT//CHAR(0),IOPEN, IOFLAG)
!!    OPEN (IFT,ACCESS='DIRECT',RECL=BLKSIZ)
!!    READ (IFT,REC=2) SEQREQ
      NUMREC=2
      CALL METDB_CREAD_DIR(IFT,SEQREQ(1:BLKSIZ),BLKSIZ,NUMREC,IOFLAG)
      CALL LOCALD(0,0,DESCR,ND,SEQREQ,'ADD')

!  Read inline data to establish Owner                               !6
      OPEN(2,FILE='DD:FT02F001',ACTION='READ',IOSTAT=IOS)
       IF (IOS.EQ.0) THEN                                            !6
        READ(2,INSTOR,IOSTAT=IOS)                                    !6
        WRITE(6,'(A)')'----OWNER---'                                 !6
        WRITE(6,'(A)')OWNER                                          !6
        WRITE(6,'(A)')'-------------'                                !6
       ELSE                                                          !6
        WRITE(6,'(A)')'OWNER SET TO DEFAULT'                         !6
        WRITE(6,'(A)')'NO INSTOR LINE PROVIDED'                      !6
        OWNER='SALT'
       ENDIF
      CLOSE(2)
!  Read inline data to establish satellite ids and codes             !6
      OPEN(3,FILE='DD:FT03F001',ACTION='READ',IOSTAT=IOS)
      IF (IOS.EQ.0) THEN                                            !6
        READ(3,SATID,IOSTAT=IOS)                                    !6
        WRITE(6,'(A)')'----SATELLITE MAPPING-----'                   !6
        WRITE(6,'(A,I5)')(IDENTS(I),BUFNUM(I),I=1,NID)
        WRITE(6,'(A)')'--------------------------'
      ELSE                                                          !6
        WRITE(6,'(A)')'OWNER SET TO DEFAULT'                         !6
        WRITE(6,'(A)')'NO INSTOR LINE PROVIDED'                      !6
        OWNER='SALT'
      ENDIF
      CLOSE(3)
!  Look for an input file.

      DO WHILE(.NOT.MHSERROR.AND..NOT.STOPNOW)
        STAT='GET'//CHAR(0)
        HLQ='R'//CHAR(0)
        CALL MHSIFFC(DSN,STAT,HLQ,
     &         OWNER(1:4)//CHAR(0),MHSRC)

! If a data set was found, open it.

        IF (MHSRC.GT.0) THEN
          OPEN (10, FILE="//'"//DSN//"'", ACTION='READ',
     &       FORM='FORMATTED',IOSTAT=IOFLAG)
          PRINT*,DSN,' opened'

! Get current time & put it in time of receipt order.
! (Assume it's enough to do this at the start of each file.)

          CALL DATIM(NOW)
          DO I=1,5
            TOR(I)=NOW(9-I)
          ENDDO

! Loop round input records (one ob per record: ident,days,lat,long,SSH)

          NEWHOUR=.FALSE.
          IORC=0
          DO WHILE (IORC.EQ.0)

! If new hour, set first ob in array from data read in last time.
! (last time - so SATNUM must be set if NEWHOUR is true!)

            IF (NEWHOUR) THEN
              NEWHOUR=.FALSE.
              NOBS=1

              OB(NOBS)=BUFNUM(INT(SATNUM))

              OB(MAXNOBS+NOBS)=YEAR       ! OB(NOBS,2)
              OB(MAXNOBS*2+NOBS)=MONTH    ! OB(NOBS,3)
              OB(MAXNOBS*3+NOBS)=DAY      ! OB(NOBS,4)
              OB(MAXNOBS*4+NOBS)=HOUR     ! OB(NOBS,5)
              OB(MAXNOBS*5+NOBS)=MINUTE   ! OB(NOBS,6)

              OB(MAXNOBS*6+NOBS)=RLAT     ! OB(NOBS,7)
              OB(MAXNOBS*7+NOBS)=RLONG    ! OB(NOBS,8)
              OB(MAXNOBS*8+NOBS)=SSH      ! OB(NOBS,9)
            ELSE
              NOBS=0
            ENDIF

            NUMREC=1
            DO WHILE (IORC.EQ.0 .AND. NOBS.LT.MAXNOBS .AND..NOT.NEWHOUR)
              READ (10,FMT=FMT,IOSTAT=IORC)
     &                 SATNUM,DAYS,RLAT,RLONG,SSH

! Convert (real) days since 1 Jan 1950 to date & time
! (DATE13 wants a century-day as input; a century-day is one on the
! first day, whereas FIFTY_YEARS and DAYS are differences. Hence +1)

              CALL DATE13(FIFTY_YEARS+INT(DAYS)+1,DAY,MONTH,YEAR)
              FRACTION_OF_DAY=DAYS-INT(DAYS)
              HOUR=24.*FRACTION_OF_DAY
              MINUTE=1440.*FRACTION_OF_DAY-60*HOUR

! When the hour changes, come out of the read loop to make a BUFR
! message & store it.  Print number of obs stored to follow progress.

              IF ((HOUR.NE.OLDHOUR .OR. DAY.NE.OLDDAY)
     &          .AND. NSTORED.GT.0) THEN
                NEWHOUR=.TRUE.
                WRITE (*,1) DAY,MONTH,YEAR, HOUR,MINUTE, NSTORED
    1           FORMAT (' Data now for ',I2,'/',I2.2,'/',I4,
     &                  I7.2,I2.2,'Z', I11,' obs stored')
              ELSE
                NOBS=NOBS+1

! If it's the same hour, put the data in the array for encoding.

! In two-dimensional terms set OB(NOBS,1) etc.  But OB will need
! compression if NOBS.LT.MAXNOBS, i.e. the NOBS values of the first
! element are not immediately followed by NOBS values of the second, so
! express subscripts one-dimensionally, OB(M,N) as OB(MAXNOBS*(N-1)+M).

                OB(NOBS)=BUFNUM(INT(SATNUM))

                OB(MAXNOBS+NOBS)=YEAR     ! OB(NOBS,2)
                OB(MAXNOBS*2+NOBS)=MONTH  ! OB(NOBS,3)
                OB(MAXNOBS*3+NOBS)=DAY    ! OB(NOBS,4)
                OB(MAXNOBS*4+NOBS)=HOUR   ! OB(NOBS,5)
                OB(MAXNOBS*5+NOBS)=MINUTE ! OB(NOBS,6)

                OB(MAXNOBS*6+NOBS)=RLAT   ! OB(NOBS,7)
                OB(MAXNOBS*7+NOBS)=RLONG  ! OB(NOBS,8)
                OB(MAXNOBS*8+NOBS)=SSH    ! OB(NOBS,9)
              ENDIF
            ENDDO

! We've read in a batch of obs: now make a BUFR message & store it.
! If array not full (NOBS.LT.MAXNOBS), then compress it to encode,
! i.e. turn a (MAXNOBS,NELEMS) array into a (NOBS,NELEMS) array.

            IF (NOBS.GT.0) THEN
              IF (NOBS.LT.MAXNOBS) THEN
                DO I=2,9
                  DO J=1,NOBS
                    OB(NOBS*(I-1)+J)=OB(MAXNOBS*(I-1)+J)
                  ENDDO
                ENDDO
              ENDIF

! Make a BUFR message

              DESCR(1)=IDES(312224)
              ND=1
              NELEM=9
              CALL ENBUFR(DESCR,OB,ND,NELEM,NOBS,
     &                    CH,TOR,MESSAGE,.TRUE.,L,IVER)

! Set COR number to go in trailer from number of days old:         !ST2
! this should ensure that each update is preferred in turn.        !ST2
! The most recent data is expected to be for 2 days ago and the    !ST2
! oldest 3 weeks before that (say 22 days ago). The function       !ST2
! (4n+5)/7 maps numbers in the range 2-22 to 1-13 for the COR      !ST2
! number. This is put in trailer; AIRSTO will set flag in first    !ST2
! byte. (AIRSTO seems to expect 0.LT.COR.LT.16.)

              CALL DATE31(TOR(3),TOR(2),TOR(1),CENDAY)
              DAYSOLD=CENDAY-(FIFTY_YEARS+DAYS+1)
              ENTRY(7:7)=CHAR((4*DAYSOLD+5)/7)                     !ST2

              ENTRY(12:12)=CHAR(NOBS)

! Put latitude & longitude (box) in index entry.

              CALL LATBOX(OB,NOBS,7,BOX)

              ENTRY(13:13)=CHAR(90+IFIX(BOX(1)))
              ENTRY(14:14)=CHAR(90+IFIX(BOX(2)))
              ENTRY(15:15)=CHAR(90+IFIX(BOX(3))/2)
              ENTRY(16:16)=CHAR(90+IFIX(BOX(4))/2)

! Set date/time from first ob in this batch.

              DO I=1,5
                DATIME(I)=IFIX(OB(NOBS*I+1))
              ENDDO
              WRITE(6,'(6I12)')NOBS,DATIME
              IDENT=IDENTS(INT(SATNUM))

! Store the message.  Reset old day & hour for next ob (from the data
! already read in but not encoded in this message).

              CALL AIRSTO(DATIME,ENTRY,MESSAGE(:L),IFT,BLKSIZ,IDENT,TOR)
            ENDIF
            NSTORED=NSTORED+NOBS
            OLDHOUR=HOUR
            OLDDAY=DAY
          ENDDO

! If no more data, rename so that this file won't be found next time.

          CLOSE (10)
          STAT='REN'//CHAR(0)
          HLQ='P'//CHAR(0)
          ILEN=INDEX(DSN,' ')
          CALL MHSIFFC(DSN(1:ILEN-1)//CHAR(0),STAT,
     &                 HLQ,OWNER(1:4)//CHAR(0),MHSRC)

! Check if the operators have requested termination of MetDB jobs. !ST2
! If so, stop the job now.                                         !ST2

          READ (1,REC=2) HKFLAGS                                   !ST2
          IF (HKFLAGS(2:2).NE.CHAR(0) .OR.                         !ST2
     &        HKFLAGS(3:3).NE.CHAR(0)) STOPNOW=.TRUE.              !ST2

! If there are no more files to store, terminate the job.          !ST2

        ELSE IF (MHSRC.EQ.0) THEN
          STOPNOW=.TRUE.                                           !ST2
        ELSE
          PRINT *,'Something wrong: MHSIFF return code was',MHSRC
          MHSERROR=.TRUE.
        ENDIF
      ENDDO
!                  Print information message if no data sets were found

      IF (NSTORED.EQ.0) THEN
        WRITE (6,'(T5,A)') 'NO SALTSSH DATA FOUND' !ST2
      ELSE
        WRITE (6,*) 'ALL DATASETS PROCESSED'
      END IF
      STOP
      END
