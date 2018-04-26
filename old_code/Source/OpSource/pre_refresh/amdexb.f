      SUBROUTINE AMDEXB(NDES,IDESCR,LOC_DES,NUMREP,EXPARR,OARRAY)   !2.0

      IMPLICIT NONE                                                   !A

!-----------------------------------------------------------------------
!
! PROGRAM       : AMDEXB
!
! PURPOSE       : TO EXPAND AN AMDAR REPORT
!
! DESCRIPTION   : SORT DECODED BUFR MESSAGE TO LOOP ROUND REPORTS RATHER
!                 THAN DESCRIPTORS. MATCH DESCRIPTORS THAT WE REQUIRE
!                 AND KEEP THE DISPLACEMENTS. EXTRACT THE REQUIRED
!                 VALUES FROM THE DECODED ARRAY.
!
! DATA TYPE(S)  : AMDAR
!
! CALLED BY     : AMDAR
!
! CALLS         : LOCALD                                           !1.8
!
! PARAMETERS    : (1) number of descriptors from decode      (i)   !1.8
!                 (2) descriptors from decoded message       (i)   !1.8
!                 (3) sequence descriptor for encoding data  (i)   !1.8
!                 (4) number of reports in message           (i)   !1.8
!                 (5) data array from decode                 (i)   !1.8
!                 (6) sorted output array                    (o)   !1.8
!
! REVISION INFO :
!
! $Workfile: amdexb.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 11/05/2007 09:27:21$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         11/05/2007 09:27:21    Brian Barwell
!       Descriptor search section rewritten to allow for high-res.
!       temperatures etc.
!  1    Met_DB_Project 1.0         30/01/2006 20:20:53    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:30  usmdb
! Separated variable declaration and initialisation. Removed unused
! dummy arguments ID and NFTAMD. Removed local variable TEXT. Added
! copyright and modified header - S.Cox
!
! Revision 1.8  2000/03/10  09:51:48  09:51:48  usmdb (Generic MetDB account)
! 20 March 2000     C Long
! Correct subscript in loop to recognise descriptors & reencode
! high-precision lat/long as hundredths. Recognise high-precision
! lat/long.
!
! Revision 1.7  97/08/11  08:29:17  08:29:17  uspm (Pat McCormack)
! Remove redundant diagnostic message
!
! Revision 1.6  1997/07/31 09:09:51  uspm
! First revision for  1
!
! Revision 1.5  1997/07/11 12:59:55  uspm
! Correct setting of variable HEAD
!
! Revision 1.4  1997/07/11 10:46:33
! General clean-up. Add IMPLICIT NONE. Remove section where
! descriptors read-in from element dataset and replace it with a
! call to LOCALD. Call to FINDES removed and code added to loop
! round descriptors as a replacement. Change method of loading data
! into the output array.                                             !A
!
! Revision 1.3  1997/07/03 14:31:16  uspm
! Latest version from  1  - Y2K check
!
! Revision 1.2  1997/05/14 13:31:51  uspm
! add preprocessor code around open statement and revision information
!
! 14/07/92 INCREASE ARRAY SIZES TO COPE WITH LARGER BULLETINS AND SO
!          MORE REPORTS.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

!---------------------------------------------------------------------
!Declare integer
!---------------------------------------------------------------------

      INTEGER IDESCR(*)
      INTEGER NUMREP
      INTEGER NDES
      INTEGER I,J
      INTEGER SEQ(100)
      INTEGER VALUE_PNTR(999)                                        !2
      INTEGER NSEQ
      INTEGER F,X,Y
      INTEGER IDES
      INTEGER K
      INTEGER LOC_DES

!---------------------------------------------------------------------
!Declare Real
!---------------------------------------------------------------------

      REAL EXPARR(NUMREP,*)
      REAL OARRAY(*)
      REAL MISSING

!---------------------------------------------------------------------
!Declare Character
!---------------------------------------------------------------------

      CHARACTER HEAD*80                                              !2

!---------------------------------------------------------------------
!Declare Logical
!---------------------------------------------------------------------

      LOGICAL FIRST                                                  !2
      LOGICAL INCORE                                               !2.0
      LOGICAL DESCR_SEARCH                                           !2

!---------------------------------------------------------------------
!Initialize variables and SAVE
!---------------------------------------------------------------------

      SAVE

      DATA FIRST/.TRUE./, INCORE/.FALSE./                            !2

      IF (FIRST) THEN                                                !2
        HEAD = '$Workfile: amdexb.f$ ' //
     &         '$Revision: 2$ $Date: 11/05/2007 09:27:21$'
        MISSING = -9999999.
        FIRST = .FALSE.                                              !2
      END IF                                                         !2

      IF (.NOT. INCORE) THEN
        F=3
        X=MOD(LOC_DES,100000)/1000                                   !A
        Y=MOD(LOC_DES,1000)                                          !A
        CALL LOCALD(X,Y,SEQ,NSEQ,' ',' ')    !Expand localseq        !A
        DO I=1,NSEQ
          IF (SEQ(I) .GT. 131072) THEN
            SEQ(I)=SEQ(I)-131072       ! Unset character flag      !1.8
          ENDIF
        ENDDO
        INCORE=.TRUE.
      ENDIF

      DO I=1,NDES
        IF (IDESCR(I) .GT. 131072) THEN
          IDESCR(I)=IDESCR(I)-131072   ! Unset character flag      !1.8
        ENDIF
      ENDDO

!----------------------------------------------------------------------
! Loop round the descriptors in the target sequence, looking for   !1.8
! a match each with a decoded descriptor. If a match is found,       !2
! keep the displacement of the corresponding data value in EXPARR.   !2
! Descriptors other than Table B descriptors are ignored.            !2
! Some descriptors are re-encoded as follows:                        !2
!   005001 --> 005002  (high res. --> coarse res. latitude)          !2
!   006001 --> 006002  (high res. --> coarse res. longitude)         !2
!   007010 --> 007002  (flight level --> altitude)                   !2
!   010070 --> 007002  (Indicated altitude --> altitude)             !2
!   007007 --> 007002  (height --> altitude)                         !2
!   012101 --> 012001  (0.01 deg. --> 0.1 deg. dry bulb temp.)       !2
!   012103 --> 012003  (0.01 deg. --> 0.1 deg. dew point temp.)      !2
!----------------------------------------------------------------------

      DO J=1,NSEQ              ! Loop round output sequence        !1.8
        VALUE_PNTR(J)=MISSING                                        !2
        DESCR_SEARCH=.TRUE.                                          !2
        I = 0   ! Descriptor counter                                 !2
        K = 0   ! Data value counter                                 !2

                               ! Search input sequence
        DO WHILE (DESCR_SEARCH .AND. I.LT.NDES)                      !2
          I = I + 1                                                  !2
          IF (IDESCR(I).LT.16384) THEN ! Table B descriptor          !2
            K = K + 1
            IF (SEQ(J).EQ.IDESCR(I) .OR.                           !1.8
     & (SEQ(J).EQ.IDES(005002).AND.IDESCR(I).EQ.IDES(005001)) .OR.   !2
     & (SEQ(J).EQ.IDES(006002).AND.IDESCR(I).EQ.IDES(006001)) .OR.   !2
     & (SEQ(J).EQ.IDES(007002).AND.(IDESCR(I).EQ.IDES(007010) .OR.   !2
     & IDESCR(I).EQ.IDES(010070).OR.IDESCR(I).EQ.IDES(007007))).OR.  !2
     & (SEQ(J).EQ.IDES(012001).AND.IDESCR(I).EQ.IDES(012101)).OR.    !2
     & (SEQ(J).EQ.IDES(012003).AND.IDESCR(I).EQ.IDES(012103))) THEN  !2
              VALUE_PNTR(J) = K                                      !2
              DESCR_SEARCH = .FALSE.                                 !2
            END IF
          END IF
        END DO
      ENDDO

!----------------------------------------------------------------------
!Now load values into the Output array ready for BUFR encode. If the
!VALUE_PNTR is missing then the value loaded into the output array
!OARRAY is also set to missing.                                      !A
!----------------------------------------------------------------------

      K=1
      DO J=1,NUMREP
        DO I=1,NSEQ
          IF (VALUE_PNTR(I) .NE. MISSING) THEN
            OARRAY(K)=EXPARR(J,VALUE_PNTR(I))
          ELSE
            OARRAY(K)=MISSING
          ENDIF
          K = K + 1                                                  !2
        ENDDO
      ENDDO

      RETURN
      END
