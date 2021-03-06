      SUBROUTINE LNDOUT(RARRAY,MAXOBS,MAXELS, &
                        NELEM,NOBS,CSTR,IERR)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : LNDOUT
!
! PURPOSE       : Output SYNOP data for BADC. Assumes QC bits are
!                 provided in RARRAY
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
!
! QC FLAGS : Each byte in the QCFLAG array corresponds to an element
!            48 SRFC_WIND_DRCTN                 47
!            50 SRFC_WIND_SPED                  49
!            52 SRFC_AIR_TMPR                   51
!            54 SRFC_DEW_PONT_TMPR              53
!            58 HRZL_VSBLY                      57
!            62 CRNT_WTHR_TYPE                  61
!            66 PRMY_PAST_WTHR_TYPE             65
!            68 SCNY_PAST_WTHR_TYPE             67
!            70 TOTL_CLOD_AMNT                  69
!            88 PRCTN_AMNT_SCTN1                87
!            92 PRCTN_AMNT_SCTN3                91
!           116 Q3HOUR_PESR_TNDY               115
!           118 Q3HOUR_STTN_LEVL_PESR_DFFRC    117
!           184 LOW_CLOD_TYPE                  183
!           186 MEDM_CLOD_TYPE                 185
!           188 HIGH_CLOD_TYPE                 187
!           190 LWST_CLOD_AMNT                 189
!           192 LWST_CLOD_BASE_HGHT            191
!           194 CLOD_AMNT                      193
!           196 CLOD_TYPE                      195
!           198 CLOD_BASE_HGHT                 197
!           200 CLOD_AMNT                      199
!           202 CLOD_TYPE                      202
!           204 CLOD_BASE_HGHT                 203
!           206 CLOD_AMNT                      205
!           208 CLOD_TYPE                      207
!           210 CLOD_BASE_HGHT                 209
!           212 CLOD_AMNT                      211
!           214 CLOD_TYPE                      213
!           216 CLOD_BASE_HGHT                 215
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
      INTEGER       I,J,K      ! Loop counters
      INTEGER       OBNUM      ! Main loop counter

      INTEGER       N_HEADER_ELEMS     ! NUMBER HEADER ELEMS
      INTEGER       N_LEVELS           ! NUMBER LEVELS
      INTEGER       NELEMS_PER_LINE    ! NUMBER ELEMS PER LINE
      CHARACTER(4)  CCCC               ! Collecting centre
      CHARACTER(4)  BULLID             ! Bulletin ID
      INTEGER       IOS                ! File status
      INTEGER       DATE(5)            ! DATE ARRAY
      INTEGER       TOR1(5)            ! TIME OF RECEIPT ARRAY
      INTEGER       QCFLAGS(216)       ! Actual flags
      INTEGER       QCINDEX(30)        ! positions of flags
      INTEGER       INDX               ! used as array index

      DATA QCINDEX/47,49,51,53,57,61,65,67,69,87,91,115,117, &
                   183,185,187,189,191,193,195,197,199,201, &
                   203,205,207,209,211,213,215/



      NELEMS_PER_LINE=0    ! Not needed for 1-D data
      N_HEADER_ELEMS=0     !
      N_LEVELS=0           !
      IERR = 0

! LOOP THROUGH REPORTS PASSED FROM MDBRET

      DO OBNUM =1,NOBS                        ! REPORTS LOOP
        CCCC=CSTR(OBNUM)(01:04)     !CCCC
        BULLID=CSTR(OBNUM)(05:08)     !Bull.id

        DO J=1,5
          K=J*2
          DATE(J) = NINT(RARRAY(OBNUM,K))        !Obs Date/time
        ENDDO

        DO J=1,5
          k=j*2 + 10
          TOR1(J) = NINT(RARRAY(OBNUM,K)) !Receipt Date/time
        ENDDO
!*---------------------------------------------------
!* Set QC Flags in a parallel array for ease of output
!*---------------------------------------------------

        DO j=1,216
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

!*=====================================================
!* Output data BEWARE - the format statement from Hell!
!*=====================================================
        WRITE(10,  &
           '(A8,3('','',I4),  &
           2('','',I4.4,4('','',I2.2)),  &
             7('','',F13.3),'','',A4,'','',A4,  &
             4('','',F13.3),  &
             4('','',F13.3,'','',I2),  &
             2('','',F13.3,'','',F13.3,'','',I2),  &
             '','',F13.3,  &
             3('','',F13.3,'','',I2),  &
             8('','',F13.3),  &
             '','',F13.3,'','',I2,'','',F13.3,  &
             '','',F13.3,'','',I2,  &
             11('','',F13.3),  &
             2('','',F13.3,'','',I2),  &
             32('','',F13.3),  &
             17('','',F13.3,'','',I2),  &
             8('','',F13.3))'  &
             ,IOSTAT=IOS,ERR=9999)  &
         'LNDSYN',N_HEADER_ELEMS,N_LEVELS,NELEMS_PER_LINE,  &
         (DATE(J),J=1,5),(TOR1(K),K=1,5),  &
         (RARRAY(OBNUM,J),J=22,34,2),CCCC,BULLID,  &
         (RARRAY(OBNUM,J),J=40,46,2),  &
         (RARRAY(OBNUM,J),QCFLAGS(J),J=48,54,2),  &
         RARRAY(OBNUM,56),RARRAY(OBNUM,58),QCFLAGS(58),  &
         RARRAY(OBNUM,60),RARRAY(OBNUM,62),QCFLAGS(62),  &
         RARRAY(OBNUM,64),  &
         (RARRAY(OBNUM,J),QCFLAGS(J),J=66,70,2),  &
         (RARRAY(OBNUM,J),J=72,86,2),  &
          RARRAY(OBNUM,88),QCFLAGS(88),RARRAY(OBNUM,90),  &
          RARRAY(OBNUM,92),QCFLAGS(92),  &
         (RARRAY(OBNUM,J),J=94,114,2),  &
         (RARRAY(OBNUM,J),QCFLAGS(J),J=116,118,2),  &
         (RARRAY(OBNUM,J),J=120,182,2),  &
         (RARRAY(OBNUM,J),QCFLAGS(J),J=184,216,2),  &
         (RARRAY(OBNUM,J),J=218,232,2)


      END DO                                  ! REPORTS LOOP

      RETURN

!----------------------------------------------------------------------
!   ERROR HANDLING FOR OUTPUT TO FT10 DATA OUTPUT DATASET
!----------------------------------------------------------------------

 9999 WRITE(6,9990)IOS
 9990 FORMAT(' ERROR ',I4,' occurred while writing out SYNOP data')

      IERR = 3001  ! INDICATES WRITE OUT TO FT10 PROBLEM
      RETURN
      END
