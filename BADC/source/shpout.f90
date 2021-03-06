      SUBROUTINE SHPOUT(RARRAY,MAXOBS,MAXELS, &
                          NELEM,NOBS,CSTR,IERR)
!-----------------------------------------------------------------------
!
! SUBROUTINE    : SHPOUT
!
! PURPOSE       : OUTPUT SHIP SYNOP DATA FOR BADC
!
! DESCRIPTION   : Formatted output, one line per observation
!
! ARGUMENTS     : RARRAY (I) Data from MetDB
!                 MAXOBS (I) Dimension of RARRAY
!                 MAXELS (I) Dimension of RARRY
!                 NELEM  (I) Number of data elements
!                 NOBS   (I) Number of observations
!                 CSTR   (I) Character elements
!                 IERR   (O) Error Code
! QC FLAGS : Each byte in the QCFLAG array corresponds to an element
!            42 SRFC_WIND_DRCTN                 41
!            44 SRFC_WIND_SPED                  43
!            46 SRFC_AIR_TMPR                   45
!            48 SRFC_DEW_PONT_TMPR              47
!            52 HRZL_VSBLY                      51
!            56 CRNT_WTHR_TYPE                  55
!            60 PRMY_PAST_WTHR_TYPE             59
!            62 SCNY_PAST_WTHR_TYPE             61
!            64 TOTL_CLOD_AMNT                  63
!            76 PRCTN_AMNT_SCTN1                75
!            80 PRCTN_AMNT_SCTN3                79
!            98 Q3HOUR_PESR_TNDY                97
!           100 Q3HOUR_STTN_LEVL_PESR_DFFRC     99
!           154 LOW_CLOD_TYPE                  153
!           156 MEDM_CLOD_TYPE                 155
!           158 HIGH_CLOD_TYPE                 157
!           160 LWST_CLOD_AMNT                 159
!           162 LWST_CLOD_BASE_HGHT            161
!           164 CLOD_AMNT                      163
!           166 CLOD_TYPE                      165
!           168 CLOD_BASE_HGHT                 167
!           170 CLOD_AMNT                      169
!           172 CLOD_TYPE                      171
!           174 CLOD_BASE_HGHT                 173
!           176 CLOD_AMNT                      175
!           178 CLOD_TYPE                      177
!           180 CLOD_BASE_HGHT                 179
!           182 CLOD_AMNT                      181
!           184 CLOD_TYPE                      183
!           186 CLOD_BASE_HGHT                 185
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
!

      IMPLICIT NONE

! Subroutine arguments:
      REAL, INTENT(IN)         :: RARRAY(MAXOBS,MAXELS)
      INTEGER, INTENT(IN)      :: MAXOBS
      INTEGER, INTENT(IN)      :: MAXELS
      INTEGER, INTENT(IN)      :: NELEM
      INTEGER, INTENT(IN)      :: NOBS
      CHARACTER(*), INTENT(IN) :: CSTR(MAXOBS)
      INTEGER, INTENT(OUT)     :: IERR

! Local declarations:
      INTEGER       I,J,K      ! LOOP COUNTERS
      INTEGER       OBNUM      ! MAIN LOOP COUNTER

      INTEGER       N_HEADER_ELEMS     ! NUMBER HEADER ELEMS
      INTEGER       N_LEVELS           ! NUMBER LEVELS
      INTEGER       NELEMS_PER_LINE    ! NUMBER ELEMS PER LINE
      CHARACTER(9)  CALLSIGN
      CHARACTER(4)  CCCC               ! Collecting Centre
      CHARACTER(4)  BULLID             ! Bulletin ID
      INTEGER       IOS                ! OUTPUT STATUS
      INTEGER       DATE(5)            ! DATE ARRAY
      INTEGER       TOR1(5)            ! TIME OF RECEIPT ARRAY 1
      INTEGER       QCFLAGS(186)       ! Actual flags
      INTEGER       QCINDEX(30)        ! positions of flags
      INTEGER       INDX               ! used as array index

      DATA QCINDEX/41,43,45,47,51,55,59,61,63,75,79,97,99, &
                   153,155,157,159,161,163,165,167,169,171, &
                   173,175,177,179,181,183,185/


      NELEMS_PER_LINE=0     ! Not needed for 1-D data
      N_HEADER_ELEMS=0      !
      N_LEVELS=0            !

! LOOP THROUGH REPORTS PASSED FROM MDBRET

      DO OBNUM =1,NOBS                        ! REPORTS LOOP

        CALLSIGN=CSTR(OBNUM)(1:9)   !Call sign
        CCCC=CSTR(OBNUM)(10:13)   ! CCCC
        BULLID=CSTR(OBNUM)(14:17)   ! Bul.id

!*REPORT TIMES
        DO J=1,5
          K=J*2
          DATE(J) = NINT(RARRAY(OBNUM,K))        !Obs Date/time
        ENDDO
!* TOR
        DO J=1,5
          K=J*2 +10
          TOR1(J) = NINT(RARRAY(OBNUM,K)) !Receipt Date/time
        ENDDO
!*---------------------------------------------------
!* Set QC Flags in a parallel array for ease of output
!*---------------------------------------------------

        DO j=1,186
          qcflags(j)=-999.0
        ENDDO

        DO J=1,30
          INDX=QCINDEX(J)
          IF (RARRAY(OBNUM,INDX) < 0) THEN
            QCFLAGS(INDX+1)=-1
          ELSE
            QCFLAGS(INDX+1)=NINT(RARRAY(OBNUM,INDX))
          ENDIF
        ENDDO
!*
!----------------------------------------------------------------------
!* Output data
!----------------------------------------------------------------------
        WRITE(10,  &
           '(A8,3('','',I4),  &
           2('','',I4.4,4('','',I2.2)),'','',A9,  &
             3('','',F13.3),'','',A4,'','',A4,  &
             4('','',F13.3),  &
             4('','',F13.3,'','',I2),  &
             2('','',F13.3,'','',F13.3,'','',I2),  &
             '','',F13.3,  &
             3('','',F13.3,'','',I2),  &
             5('','',F13.3),  &
             '','',F13.3,'','',I2,'','',F13.3,  &
             '','',F13.3,'','',I2,  &
             8('','',F13.3),  &
             2('','',F13.3,'','',I2),  &
             26('','',F13.3),  &
             17('','',F13.3,'','',I2))'  &
             ,IOSTAT=IOS,ERR=9999)  &
         'SHPSYN',N_HEADER_ELEMS,N_LEVELS,NELEMS_PER_LINE,  &
         (DATE(J),J=1,5),(TOR1(K),K=1,5),CALLSIGN,  &
         (RARRAY(OBNUM,J),J=24,28,2),CCCC,BULLID,  &
         (RARRAY(OBNUM,J),J=34,40,2),  &
         (RARRAY(OBNUM,J),QCFLAGS(J),J=42,48,2),  &
         RARRAY(OBNUM,50),RARRAY(OBNUM,52),QCFLAGS(52),  &
         RARRAY(OBNUM,54),RARRAY(OBNUM,56),QCFLAGS(56),  &
         RARRAY(OBNUM,58),  &
         (RARRAY(OBNUM,J),QCFLAGS(J),J=60,64,2),  &
         (RARRAY(OBNUM,J),J=66,74,2),  &
          RARRAY(OBNUM,76),QCFLAGS(76),RARRAY(OBNUM,78),  &
          RARRAY(OBNUM,80),QCFLAGS(80),  &
         (RARRAY(OBNUM,J),J=82,96,2),  &
         (RARRAY(OBNUM,J),QCFLAGS(J),J=98,100,2),  &
         (RARRAY(OBNUM,J),J=102,152,2),  &
         (RARRAY(OBNUM,J),QCFLAGS(J),J=154,186,2)


      END DO                                  ! REPORTS LOOP

      RETURN

!----------------------------------------------------------------------
!   ERROR HANDLING FOR OUTPUT TO FT10 DATA OUTPUT DATASET
!----------------------------------------------------------------------

 9999 WRITE(6,9990)IOS
 9990 FORMAT(' ERROR ',I4,' OCCURRED WHILE WRITING OUT SHPSYN DATA')

      IERR = 3001  ! INDICATES WRITE OUT TO FT10 PROBLEM
      RETURN
      END
