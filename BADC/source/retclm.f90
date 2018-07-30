      PROGRAM RETCLM
!-----------------------------------------------------------------------
!
! PROGRAM       : RETCLM
!
! PURPOSE       : Extracts CLIMAT data from the MetDB and formats
!                 for transmission to BADC
!
! DESCRIPTION   : MetDB has 3 months online and access is for whole
!                 months at a time. Data arrival time varies so assume
!                 it can arrive at any time.
!                 Try extracting data from the current month and the
!                 previous three months, received after the last run
!                 (as stored in the control dataset FT22).
!
!
! RETURN CODES:
!
!        0     Successful completion
!        8     No data found in MetDB
!
!
! CALLS TO      : MNTHDS    ZPDATE function returns days in a month
!                 DATIM     Current date function
!
!
! REVISION INFO :
!
! MB-575  Convert to f90 on Linux                         Andy Moorhouse
!  2    Met_DB_Project 1.1         02/06/2009 13:16:08    Sheila Needham
!       Minor changes to comments following review
!  1    Met_DB_Project 1.0         01/06/2009 09:45:41    Sheila Needham
!       Initial versions
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2009-18 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The MetDB Team.
!-----------------------------------------------------------------------
use zpdate_mod
      IMPLICIT NONE

      INTEGER IOBS
      INTEGER IELS
      INTEGER IRECLEN
      INTEGER STRLEN

      PARAMETER      (IOBS=100)      !no of obs per MDB call
      PARAMETER      (IELS=1)        !just report text
      PARAMETER      (IRECLEN=1620)  !output record length
      PARAMETER      (STRLEN=15000)  !max length of report string

      CHARACTER*8    CSUBT
      CHARACTER*100  CREQ
      CHARACTER*9    CSTR(IOBS)
      CHARACTER*14   ARUN
      CHARACTER*(STRLEN) CREP(IOBS)
      REAL           ARRAY(IOBS,IELS)
      INTEGER        NOBS
      INTEGER        NELEM
      INTEGER        IPT        ! pointer through string
      INTEGER        NREC       ! no of records per bulletin output
      INTEGER        ISTAT
      INTEGER        MNTHTOT    ! Obs per month
      INTEGER        TOT        ! Total obs
      INTEGER        I,J        ! Loop counters
      INTEGER        ILEN       ! Length of report (including header)
      INTEGER        TIME(8)    ! Current date and time
      INTEGER        NDAYS      ! no of days in a month
!     INTEGER        MNTHDS     ! Function in ZPDATE

      COMMON /BIG/CREP

!---------------------------------------------------------
! Initialise variables
!---------------------------------------------------------
      TOT = 0
      CSUBT = 'CLIMAT  '

      CREQ='START TIME YYYYMM01/0000Z END TIME YYYYMMDD/2359Z  &
           &RECEIVED AFTER YYYYMMDD/HHMMZ &
           &ELEMENTS &
           &RPRT_TEXT '

!  Get current date

      CALL DATIM(TIME)
      WRITE(6,'(A,I2.2,''/'',I2.2,''/'',I4.4,1X,2(I2.2))')  &
              'Date:',TIME(6),TIME(7),TIME(8),TIME(5),TIME(4)

!  Get date and time of last run

      READ(22,'(A14)')ARUN
      WRITE(6,*)'Previous run was at ',ARUN
      CREQ(66:79)=ARUN
!  and update with the current date and time
      WRITE(ARUN,'(I4.4,I2.2,I2.2,''/'',I2.2,I2.2,''Z'')')  &
                  (TIME(J),J=8,4,-1)
!  Loop over this month and the previous 3 months
      DO J=1,4
        NOBS = IOBS
        NELEM = IELS
        ISTAT = 0
        MNTHTOT = 0

! Finish setting up the request string - CLIMAT reports can only
! be extracted as whole months, any other start/end times give
! unpredictable results!

        NDAYS=MNTHDS(TIME(7),TIME(8))
        WRITE(CREQ(12:15),'(I4.4)')TIME(8)   ! start
        WRITE(CREQ(16:17),'(I2.2)')TIME(7)
        WRITE(CREQ(36:39),'(I4.4)')TIME(8) ! end
        WRITE(CREQ(40:41),'(I2.2)')TIME(7)
        WRITE(CREQ(42:43),'(I2.2)')NDAYS

        WRITE(6,*)CREQ

!---------------------------------------------------------
! Keep calling MetDB while ISTAT = 0 or 4
!---------------------------------------------------------

        DO WHILE (ISTAT.LE.4)

          CALL MDB(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,CSTR,CREP)

          IF (ISTAT.LE.4 .AND. NOBS.GT.0) THEN
            DO I=1,NOBS
              ILEN=ARRAY(I,1)
              IF(ILEN.GT.STRLEN)THEN
                WRITE(6,*)'WARNING:Report too long',ILEN
              ELSE
                MNTHTOT=MNTHTOT+1
                NREC=(ILEN-45+IRECLEN)/IRECLEN
                WRITE(10,'(I6,'','',I6,'','',A)')  &
                              NREC,ILEN-44,CREP(I)(1:44)
                IPT=45  ! start of data
                DO WHILE (IPT.LE.ILEN)
                  WRITE(10,'(A)')CREP(I)(IPT:MIN(ILEN,IPT+IRECLEN-1))
                  IPT=IPT+IRECLEN
                END DO
              ENDIF
            END DO
          ENDIF

          IF (ISTAT.EQ.0) GOTO 9999
        ENDDO

        WRITE(6,*)'CLIMAT: Error in MetDB, ISTAT = ',ISTAT

 9999   CONTINUE

        WRITE(6,'( 1X,''ISTAT= '',I5)')ISTAT
        WRITE(6,'( 1X,''TOTAL= '',I5/)')MNTHTOT

!   decrement date by a month

        TIME(7)=TIME(7)-1
        IF(TIME(7).EQ.0)THEN
          TIME(7)=12
          TIME(8)=TIME(8)-1
        ENDIF
        TOT=TOT+MNTHTOT
      END DO

!  end of loop over months

      IF (TOT .EQ. 0)THEN
        ISTAT = 8
      ELSE
        ISTAT=0
      ENDIF

!   update control dataset and output summary
      REWIND(22)
      WRITE(22,'(A14)')ARUN
      WRITE(22,'(I4)')ISTAT
      WRITE(22,'(I8)')TOT

      WRITE(6,'( 1X,''ISTAT      = '',I5)')ISTAT
      WRITE(6,'( 1X,''TOTAL      = '',I5)')TOT

      IF(ISTAT.GT.0) call exit(ISTAT)

      STOP
      END
