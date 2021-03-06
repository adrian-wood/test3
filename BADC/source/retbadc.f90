      PROGRAM RETBADC
!-----------------------------------------------------------------------
!
! PROGRAM       : RETBADC
!
! PURPOSE       : Main program for MDB retrievals for BADC
!
! DESCRIPTION   : Prepares inputs for call to MetDB retrieval program
!                 based on control information on FT22 and request
!                 details on FT21.
!                 If MODE=CATCHUP then extraction is for the current
!                 dates in the control dataset.
!                 If the previous run was successful, a RECEIVED AFTER
!                 keyword is added to the request string.
!                 IF MODE=NORMAL then the start and end dates are
!                 incremented by the amount specified in the control
!                 dataset.
!                 Completion code is output to the control dataset,
!                 along with updated start/end times, the time the
!                 job ran and number of obs extracted.
!
! RETURN CODES:
!
!        0     Successful completion
!     2110     No ELEMENTS keyword in request string
!     2111     No start date found in request string
!     2112     No end date found in request string
!
!       other return codes set by called programs
!
! CALLS TO      : DATIM     System date and time
!                 DATE13    Date parameters to century day conversion
!                 DATE31    Century day to date parameters conversion
!                 MDBRET    Retrieves obs from MetDB
!                 SYSRCX    Program abend with return code
!
!
! REVISION INFO :
!
! MB-575  Convert to f90 on Linux                         Andy Moorhouse
!  2    Met_DB_Project 1.1         02/06/2009 13:16:07    Sheila Needham
!       Minor changes to comments following review
!  1    Met_DB_Project 1.0         01/06/2009 09:45:40    Sheila Needham
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
use datim_mod
use zpdate_mod

      IMPLICIT NONE

      CHARACTER(14) AEND      ! End time for request
      CHARACTER(16) ARUN      ! Date/time job ran
      CHARACTER(14) ASTART    ! Start time for request
      CHARACTER(14) AFTER     ! Date for RECEIVED AFTER keyword
      CHARACTER(8)  CSUBT     ! MetDB data subtype
      LOGICAL       ENDODAT   !  True when all input lines have been read
      INTEGER       ERRCOD    ! Output error code
      CHARACTER(80) HEAD      ! Source version details
      INTEGER       I1        ! pointer to position in req string
      INTEGER       ICOND     ! COND CODE from previous run
      INTEGER       IY        ! year
      INTEGER       IM        ! month
      INTEGER       ID        ! day
      INTEGER       IH        ! hour
      INTEGER       IMIN      ! minute
      INTEGER       ICD       ! century day
      INTEGER       ICH       ! century hour
      INTEGER       J         ! Loop counter
      CHARACTER(80) LINE      ! One line of input request
      INTEGER       NELEM     ! Number of elements to be retrieved
      INTEGER       NFREQ     ! Frequency of runs in hours
      INTEGER       NOBS      ! Total number of obs received
      INTEGER       REQLEN    ! Length of request string
      INTEGER       TIME(8)   ! Current date and time
      INTEGER       TIMLAG    ! not used in this program
      CHARACTER(4000) TREQ    ! Request string for MetDB call
      CHARACTER(4000) TEMPREQ ! Req string from ELEMENTS onwards
      CHARACTER(7)  MODE      ! 'CATCHUP' or 'NORMAL' on FT05


      ERRCOD= 0  ! BADC OUTPUT ERROR CODE
      NELEM=0    ! TOTAL NUMBER OF ELEMENTS FROM METDB

      READ(5,'(A7)',END=10)MODE
10    CONTINUE
      IF (MODE /= 'NORMAL' .AND. MODE /= 'CATCHUP') THEN
        WRITE(6,*)'Invalid mode ',MODE,'. Running with NORMAL.'
        MODE='NORMAL'
      ENDIF

!  Get current date and time
      CALL DATIM(TIME)

!*======================================================================
!* Read request details:
!* Data subtype
!* Request string terminated by ENDOFDATA
!* Number of elements in request
!*======================================================================

      open(21,file="FT21F001",action="read",form="formatted")
      READ(21,'(A8)')CSUBT
      I1=1            ! pointer through request string
      ENDODAT=.FALSE.
      DO WHILE (.NOT.ENDODAT)
        READ(21,'(A80)')LINE
        IF (LINE(1:9) == 'ENDOFDATA') THEN
          ENDODAT = .TRUE.
        ELSE
          TREQ(I1:)=LINE
          I1=I1+INDEX(LINE,';')
          TREQ(I1-1:I1-1)=' '
        ENDIF
      ENDDO
      READ(21,'(I4)')NELEM
      close(21)

      REQLEN=I1
      WRITE(6,*)'Extracting data for ',CSUBT
      WRITE(6,*)'Mode is ',MODE

!*===============================================
! READ details of last run from control dataset:
! start time YYYYMMDD/HHMMZ
! end time YYYYMMDD/HHMMZ
! increment in hours for next run
! time of run YYYYMMDD/HHMMSSZ
! completion code
! ===============================================

      open(22,file="FT22F001",action="readwrite",form="formatted")
      READ(22,'(A14)')ASTART
      READ(22,'(A14)')AEND
      READ(22,'(I4)')NFREQ
      READ(22,'(I4)')TIMLAG
      READ(22,'(A16)')ARUN
      READ(22,'(I4)')ICOND
      WRITE(6,*)'Previous run start:',astart
      WRITE(6,*)'Previous run end  :',aend
      WRITE(6,*)'Increment         :',nfreq
      WRITE(6,*)'Condition code    :',icond

!*==========================================================
! Calculate new start and end times
!*==========================================================
      IF (MODE == 'NORMAL') THEN   ! calculate new dates

! Start date

        READ(ASTART,'(i4,i2,i2,1x,i2,i2)')IY,IM,ID,IH,IMIN
        CALL DATE31(ID,IM,IY,ICD)
        ICH = (ICD-1)*24 + IH           ! Century hour

! increment hour
        ICH = ICH + NFREQ
        IH=MOD(ICH,24)
        ICD = (ICH-IH)/24 + 1
        CALL DATE13(ICD,ID,IM,IY)

        WRITE(ASTART,'(i4.4,i2.2,i2.2,''/'',i2.2,i2.2,''Z'')') &
                       IY,  IM,  ID,      IH,  IMIN


! End date

        READ(AEND,'(i4,i2,i2,1x,i2,i2)')IY,IM,ID,IH,IMIN
        CALL DATE31(ID,IM,IY,ICD)
        ICH = (ICD-1)*24 + IH

! increment hour
        ICH = ICH + NFREQ
        IH=MOD(ICH,24)
        ICD = (ICH-IH)/24 + 1
        CALL DATE13(ICD,ID,IM,IY)
        WRITE(AEND,'(i4.4,i2.2,i2.2,''/'',i2.2,i2.2,''Z'')') &
                       IY,  IM,  ID,      IH,  IMIN

      ENDIF  ! otherwise run with the old dates

      WRITE(6,'(''Starting at '',A)')ASTART
      WRITE(6,'(''Ending at   '',A)')AEND
! If this is a rerun and previous run was sucessful, add keyword
! to restrict retrieval to extra data only
      IF (MODE == 'CATCHUP' .AND. ICOND == 0) THEN
        AFTER=ARUN(1:13)//'Z'
        I1=INDEX(TREQ,'ELEMENTS')
        IF (I1 > 0) THEN
          TEMPREQ=TREQ(I1:)
          TREQ(I1:)='RECEIVED AFTER '//AFTER//' '
          TREQ(I1+31:)=TEMPREQ
          REQLEN=REQLEN+31
        ELSE
          ERRCOD=2110
          GOTO 999
        ENDIF
      ENDIF

      I1=INDEX(TREQ,'START TIME')
      IF (I1 > 0) THEN
         TREQ(I1+11:I1+24) = ASTART
      ELSE
        ERRCOD=2111
        GOTO 999
      ENDIF
      I1=INDEX(TREQ,'END TIME')
      IF (I1 > 0) THEN
         TREQ(I1+9:I1+22) = AEND
      ELSE
        ERRCOD=2112
        GOTO 999
      ENDIF

      WRITE(6,*)TREQ(1:REQLEN)

!*==========================================================
! Extract and process data
!*==========================================================

      CALL MDBRET(CSUBT,TREQ,NELEM,ERRCOD,NOBS)
      IF (ERRCOD > 0) THEN
        WRITE(6,*)'ERROR: Returned from MDBRET ',ERRCOD
        IF (ERRCOD == 1008 .AND. MODE == 'CATCHUP') THEN
          WRITE(6,*)'Resetting NO DATA error because this is a CATCHUP'
          ERRCOD=0
        ENDIF
      ENDIF

      WRITE(6,*)' ============================================'
      WRITE(6,*)' Total observations extracted ',NOBS

!*==========================================================
! Output run details to control dataset
!*==========================================================

999   REWIND 22

      WRITE(6,*)' Updating Control dataset, completion code ',ERRCOD

      WRITE(ARUN,'(I4.4,I2.2,I2.2,''/'',I2.2,I2.2,I2.2,''Z'')') &
                  (TIME(J),J=8,3,-1)
      WRITE(22,'(A14)')ASTART
      WRITE(22,'(A14)')AEND
      WRITE(22,'(I4)')NFREQ
      WRITE(22,'(I4)')TIMLAG
      WRITE(22,'(A16)')ARUN
      WRITE(22,'(I4)')ERRCOD
      WRITE(22,'(I8)')NOBS

      close(22)


      IF (ERRCOD > 0) call exit(ERRCOD)  

      STOP
      END
