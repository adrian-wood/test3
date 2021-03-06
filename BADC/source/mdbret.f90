      SUBROUTINE MDBRET(CSUBT,CREQ,NELEM,ERRCOD,TOTOBS)
!-----------------------------------------------------------------------
!
! SUBROUTINE    : MDBRET
!
! PURPOSE       : Extracts data from the MDB and calls individual
!                 output subroutines
!
! DESCRIPTION   :
!
! ARGUMENTS     : CSUBT  (I) Data subtype
!                 CREQ   (I) MetDB Request string
!                 NELEM  (I) Number of elements to be extracted
!                 ERRCOD (O) Return Code
!                 TOTOBS (O) Number of obs extracted
!
!
! RETURN CODES:
!
!        0     Successful completion
!     1008     No data found in MetDB
!     1016     Fatal error in MetDB call
!     1055     Too many elements selected for array size
!     1100     Invalid data type
!
!       other return codes set by called programs
!
! CALLS TO      : DATIM     System date and time
!                 DATE13    Date parameters to century day conversion
!                 DATE31    Century day to date parameters conversion
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
!use amdout_mod
      IMPLICIT NONE

      CHARACTER*8   CSUBT      ! METDB DATA SUBTYPE
      CHARACTER*4000 CREQ      ! FINAL REQUEST STRING
      INTEGER       NOBS
      INTEGER       NELEM
      INTEGER       ISTAT
      INTEGER       PREVOB

      INTEGER       MAXOBS
      INTEGER       MAXELS

      INTEGER       INOBS      ! NUMBER OF METDB OBSERVATIONS
      INTEGER       ERRCOD     ! Error code
      INTEGER       IERR       ! Error code FROM CALLED routines
      INTEGER       TOTOBS     ! Total obs retrieved

      PARAMETER (MAXOBS=100,MAXELS=7072)

      REAL           RARRAY(MAXOBS,MAXELS) ! MetDB data
      CHARACTER*80   CSTR(MAXOBS)          ! FOR CHARACTER ELEMENTS
      CHARACTER*400  CREP(MAXOBS)          ! FOR REPORT TEXT

      COMMON /LARGE/RARRAY             !For Dynamic common

      IF(NELEM.GT.MAXELS)THEN
          WRITE(6,*)'ERROR:Number of elements ',NELEM, &
          ' exceeds max permitted ',MAXELS,' FOR TYPE ',CSUBT
          ERRCOD=1055
          RETURN
!         GOTO 999
      ENDIF

      PREVOB = 0
      ISTAT = 0
      NOBS = MAXOBS
      TOTOBS = 0

      open(10,file="FT10F001",action="write",form="formatted",position="append")

!*======================================================================
!*--- LOOP BACK TO HERE IF THERE IS ANY MORE DATA TO COME
!*======================================================================
      DO WHILE (ISTAT.LE.4)

        CALL MDB(CSUBT,CREQ,RARRAY,NOBS,NELEM,ISTAT,CSTR,CREP)

        IF(ISTAT.GE.8)THEN
           ERRCOD= ISTAT+1000
           WRITE(6,*)'ERROR: MetDB retrieval ',ISTAT
        ELSEIF(ISTAT.LE.4.AND.NOBS.GT.0)THEN
           TOTOBS=TOTOBS + NOBS

           IF    (CSUBT(1:6).EQ.'AMDARS')THEN
             CALL AMDOUT(RARRAY,MAXOBS,MAXELS, &
                          NELEM,NOBS,CSTR,IERR)
           ELSEIF(CSUBT.EQ.'LNDSYN')THEN
             CALL LNDOUT(RARRAY,MAXOBS,MAXELS, &
                      NELEM,NOBS,CSTR,IERR)
            ELSEIF(CSUBT(1:6).EQ.'PILOT ')THEN
              CALL PLTOUT(RARRAY,MAXOBS,MAXELS, &
                          NELEM,NOBS,CSTR,IERR)
            ELSEIF(CSUBT(1:6).EQ.'OZONEP')THEN
              CALL OZPOUT(RARRAY,MAXOBS,MAXELS, &
                          NELEM,NOBS,CSTR,IERR)
            ELSEIF(CSUBT(1:6).EQ.'RASS  ')THEN
               CALL RASOUT(RARRAY,MAXOBS,MAXELS, &
                          NELEM,NOBS,CSTR,IERR)
            ELSEIF(CSUBT(1:6).EQ.'SHPSYN')THEN
              CALL SHPOUT(RARRAY,MAXOBS,MAXELS, &
               NELEM,NOBS,CSTR,IERR)
            ELSEIF(CSUBT(1:6).EQ.'TEMP  ')THEN
              CALL TMPOUT(RARRAY,MAXOBS,MAXELS, &
               NELEM,NOBS,CSTR,IERR)
            ELSEIF(CSUBT(1:6).EQ.'WINPRO')THEN
              CALL WINOUT(RARRAY,MAXOBS,MAXELS, &
               NELEM,NOBS,CSTR,IERR)
            ELSEIF(CSUBT(1:6).EQ.'METARS')THEN
              CALL MTROUT(MAXOBS,NOBS,CREP,IERR)
           ELSE
             WRITE(6,*)'ERROR: Invalid data type ',CSUBT
             ERRCOD=1100
             EXIT
           ENDIF
           IF(IERR.GT.0)THEN
             WRITE(6,*)'ERROR: In data output ',IERR
             ERRCOD=2000+IERR
             EXIT
           ENDIF
         ENDIF

         IF (ISTAT.EQ.0) EXIT  ! no more obs

      END DO                  ! END PROCESS MDB OBS LOOP

      ISTAT=99  ! Kill the server
      CALL MDB(CSUBT,CREQ,RARRAY,NOBS,NELEM,ISTAT,CSTR,CREP)
      close(10)

      RETURN
      END
