      PROGRAM CNTLCHK

!-----------------------------------------------------------------------
!
! PROGRAM       : CNTLCHK
!
! PURPOSE       : Controls running of BADC data extraction steps
!                 by returning condition codes and/or altering
!                 the control dataset
!
! DESCRIPTION   : Looks at a control dataset (FT22) to determine if
!                 enough time had lapsed for the next retrieval to
!                 run. Time delays vary between data types and are set
!                 to ensure 98% of obs are in for the given period.
!                 If SENDONLY is entered on FT05 then no extraction
!                 is needed.
!
!
! RETURN CODES  :  0 - normal extraction
!                  1 - current date is too close to start time
!                  2 - current date is too close to end time
!                 10 - no extraction needed
! CALLS:          DATIM
!                 DATE31
!                 DATE13
!                 SYSRCX
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
! prohibited without the permission of The Metdb Team.
!-----------------------------------------------------------------------
!
!use datim_mod
use zpdate_mod
use datim_mod

      IMPLICIT NONE

      INTEGER       I1      ! pointer to position in req string
      INTEGER       IY      ! year
      INTEGER       IM      ! month
      INTEGER       ID      ! day
      INTEGER       IH      ! hour
      INTEGER       IMIN    ! minute
      INTEGER       ICD     ! century day
      INTEGER       ICH     ! century hour
      INTEGER       IRC     ! return code
      INTEGER       J       ! Loop counter
      INTEGER       TIME(8) ! Current date and time
      INTEGER       TIMNOW  ! Current century hour
      CHARACTER*8   MODE    ! Type of run NORMAL,CATCHUP,SENDONLY


      CHARACTER*14  ASTART  ! Start time for request
      CHARACTER*14  AEND    ! End time for request
      INTEGER       NFREQ   ! Frequency of runs in hours
      INTEGER       TIMLAG  ! Period required before next retrieval
      CHARACTER*16  ARUN    ! Date/time job ran
      INTEGER       ICOND   ! COND CODE from previous run
      INTEGER       TOTOBS  ! total obs from previous run


!  Get current date and time

      CALL DATIM(TIME)
      WRITE(6,*)'Time now:',TIME(8),TIME(7),TIME(6),TIME(5)
      CALL DATE31(TIME(6),TIME(7),TIME(8),ICD)
      TIMNOW = (ICD-1)*24 + TIME(5)

!  Read MODE

      READ(5,'(A)')MODE
      WRITE(6,*)'Mode is ',MODE
      IRC=0

      IF(MODE.EQ.'SENDONLY')THEN
        WRITE(6,*)'Skipping date checks'
        IRC=10
        GOTO 999
      ENDIF

!*===============================================
! READ details of last run from control dataset:
! start time YYYYMMDD/HHMMZ
! end time YYYYMMDD/HHMMZ
! increment in hours for next run
! time delay in hours before the extraction should run
! time of run YYYYMMDD/HHMMSSZ
! completion code
! ===============================================
      open(22,file="FT22F001",action="read",form="formatted")
      READ(22,'(A14)')ASTART
      READ(22,'(A14)')AEND
      READ(22,'(I4)')NFREQ
      READ(22,'(I4)')TIMLAG
      READ(22,'(A16)')ARUN
      READ(22,'(I4)')ICOND
      READ(22,'(I8)')TOTOBS
      WRITE(6,*)'Previous run start:',astart
      WRITE(6,*)'Previous run end  :',aend
      WRITE(6,*)'Increment         :',nfreq
      WRITE(6,*)'Time delay        :',timlag
      WRITE(6,*)'Condition code    :',icond
      close(22)

!*============================================================
! Calculate new start and end times are far enough in the past
!  for approx 98% of obs to be stored
!*============================================================
      READ(ASTART,'(i4,i2,i2,1x,i2,i2)')IY,IM,ID,IH,IMIN
      CALL DATE31(ID,IM,IY,ICD)
      ICH = (ICD-1)*24 + IH
      IF(ICOND.EQ.0)THEN   ! increment hour
        ICH = ICH + NFREQ   ! increment hour
        IH=MOD(ICH,24)
        ICD = (ICH-IH)/24 + 1
        CALL DATE13(ICD,ID,IM,IY)
        WRITE(ASTART,'(i4.4,i2.2,i2.2,''/'',i2.2,i2.2,''Z'')'), &
                       IY,  IM,  ID,      IH,  IMIN
      ENDIF
      IF (ICH+TIMLAG.GT.TIMNOW)THEN
        WRITE(6,*)'Too early for this retrieval'
        IRC=1
        GOTO 999
      ENDIF
!
! check the end date
!
      READ(AEND,'(i4,i2,i2,1x,i2,i2)')IY,IM,ID,IH,IMIN
      CALL DATE31(ID,IM,IY,ICD)
      ICH = (ICD-1)*24 + IH
      IF(ICOND.EQ.0)THEN                ! increment hour
        ICH = ICH + NFREQ
        IH=MOD(ICH,24)
        ICD = (ICH-IH)/24 + 1
        CALL DATE13(ICD,ID,IM,IY)
        WRITE(AEND,'(i4.4,i2.2,i2.2,''/'',i2.2,i2.2,''Z'')'), &
                     IY,  IM,  ID,      IH,  IMIN
      ENDIF
      IF (ICH+TIMLAG.GT.TIMNOW)THEN
        WRITE(6,*)'Too early for this retrieval'
        IRC=2
        GOTO 999
      ENDIF
999   CONTINUE
      IF(MODE.NE.'SENDONLY')THEN
        WRITE(6,*)'New start:',astart
        WRITE(6,*)'New end  :',aend
      ENDIF

      IF(IRC.GT.0) call exit(IRC)
      STOP

      END
