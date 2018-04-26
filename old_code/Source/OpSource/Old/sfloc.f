      SUBROUTINE SFLOC(BULL,POINT,BULLEND,
     &                 TTAAii,CCCC,YYGGGG,CORNUM,NFTSFR)

!-----------------------------------------------------------------------
!
! PROGRAM       : SFLOC
!
! PURPOSE       : expand SFLOC bulletin, encoding each set of fixes
!                 in BUFR & storing
!
! CALLED BY     : MDBSTOR
!
! CALLS         : BULLED, IVALUE, SFDATE, ENBUFR, AIRSTO
!
! PARAMETERS    : (1) bulletin
!                 (2) start of data in bulletin
!                 (3) end of data in bulletin
!                 (4) TTAAii from bulletin heading
!                 (5) CCCC from bulletin heading
!                 (6) YYGGGG from bulletin heading
!                 (7) COR number from bulletin heading
!                 (8) FT number of storage data set
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 05/06/2008 11:17:35$
! $Source: /data/us0400/mdb/op/lib/source/RCS/sfloc.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         05/06/2008 11:17:35    Brian Barwell   SFLOC
!        data stopped in December 2007. SFLOC routine removed from MDBSTOR
!       load module in May 2008.
!  1    Met_DB_Project 1.0         30/01/2006 20:24:13    Sheila Needham  
! $
! Revision 2.1  2005/03/17  11:41:26  11:41:26  usmdb (MetDB account c/o John C Ward)
! 2.1.  21 March 2005.  Brian Barwell.
! Call BOXLALO or INDLALO as appropriate to make area information
! for index entry. Revision information tidied up.
! 
! Revision 2.0  2001/07/03 10:43:53 10:43:53 usmdb (MetDB account c/o J C Ward)
! Removed unused variable. Added copyright and modified
! header - S.Cox
!
! Revision 1.3  98/08/12  08:42:30  08:42:30  usmdb (Generic MetDB account)
! set location method missing if not 66600, 66611 or 66666 (UK obs
! use 66612, & we only get UK obs!)                                   !b
!
! Revision 1.2  98/06/11  11:17:37  11:17:37  usmdb (Generic MDB account)
! compress any sequence of point fixes                                !a
!
! Revision 1.1  98/05/15  10:27:35  10:27:35  usmdb (Generic MDB account)
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2005 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
      CHARACTER BULL*(*)
      INTEGER   POINT
      INTEGER   BULLEND
      CHARACTER TTAAii*6
      CHARACTER CCCC*4
      CHARACTER YYGGGG*6
      CHARACTER CORNUM*2
      INTEGER   NFTSFR

      CHARACTER MESSAGE*999
      CHARACTER ENTRY*23
      CHARACTER ID*9
      CHARACTER HEAD*132
      INTEGER   IVALUE
      INTEGER   IDES
      INTEGER   MISSING
      INTEGER   I666
      INTEGER   HOUR
      INTEGER   AI       ! lower case ai, used to set 008007
      INTEGER   BIGAI    ! Ai with capital A, kept as code figure
      INTEGER   NFIXES
      INTEGER   X4
      INTEGER   K
      INTEGER   I,J
      INTEGER   L
      INTEGER   ND
      INTEGER   NF
      INTEGER   NELEM
      INTEGER   NOBS

      REAL      LAT
      REAL      LONG
      REAL      BOX(4)     ! Min lat, Max lat, Min long, Max long.  !2.1
      REAL      ARRAY(999)
      REAL      YARRA(999)                                           !a
      INTEGER   DATIME(5)
      INTEGER   NOW(8)
      INTEGER   TOR(5)
      INTEGER   DESCR(999)
      LOGICAL   CMPRES
      LOGICAL   FIRST      ! Flag for first call to SFLOC           !2.1

      DATA FIRST/.TRUE./, MISSING/-9999999/                         !2.1
!                                                   Revision information
      IF (FIRST) THEN                                               !2.1
        HEAD='$RCSfile: $ ' //                                      !2.1
     &       '$Revision: 2$ $Date: 05/06/2008 11:17:35$'                                !2.1
        FIRST = .FALSE.                                             !2.1
      END IF                                                        !2.1

      NFIXES=0
      NOBS=1
      ID='SFLOC'
      CMPRES=.FALSE.
      ENTRY(3:11)=TTAAii(1:4)//CORNUM(2:2)//CCCC
      ARRAY(1)=18.             ! set feature code (always the same)

! Set location method from figures after 666

      CALL BULLED(POINT,BULLEND,BULL)
      I666=INDEX(BULL,' 666')
      IF (BULL(I666+4:I666+5).EQ.'00') THEN                          !b
        ARRAY(8)=0                                                   !b
      ELSE IF (BULL(I666+4:I666+5).EQ.'11') THEN                     !b
        ARRAY(8)=1                                                   !b
      ELSE IF (BULL(I666+4:I666+5).EQ.'66') THEN                     !b
        ARRAY(8)=6                                                   !b
      ELSE                                                           !b
        ARRAY(8)=MISSING                                             !b
      ENDIF                                                          !b
      POINT=I666+7

! Hour is in report, day (& hour) in bulletin heading
! (Half-hourly reports with 30 in heading and hour+50]
! (The 30-minute period is right for UK data, all we get in April 98)

      HOUR=IVALUE(BULL(POINT:POINT+1))
      IF (HOUR.GE.50) HOUR=HOUR-50
      ARRAY(6)=0                        ! minutes (0 or 30)
      IF (IVALUE(YYGGGG(3:4)).EQ.HOUR .AND.
     &    IVALUE(YYGGGG(5:6)).EQ.30) ARRAY(6)=30.
      ARRAY(7)=-30.                     ! 30-minute period

! Set the date from the hour above and day/hour in the bulletin heading

      CALL SFDATE(HOUR,YYGGGG,DATIME)
      DO I=1,4
        ARRAY(I+1)=DATIME(I)
      ENDDO

      CALL DATIM(NOW)
      DO I=1,5
        TOR(I)=NOW(9-I)
      ENDDO

! After the hour group all groups are either fixes (lat/longs) or
! 9-groups resetting the last 3 figures of the hour group.
! Start a new BUFR message whenever a 9-group is found,
! compressing point fixes or replicating line or area lat/longs.

! X4 is used to set lat/long,
! ai to describe distribution as point, line or area,
! Ai (accuracy etc) is left as a code figure.

! We only expect point data to start with (May 1998).  Line or area
! bulletins could be made in Bracknell, but while they can't be plotted
! they won't be made.  And we only get bulletins from Bracknell.
! If we stored line or area data, we would need more work on retrieval.
! So give up if Ai says line or area.  (info from Paul Taylor, May 98)

      DO WHILE (POINT.LT.BULLEND)
        X4=IVALUE(BULL(POINT+2:POINT+2))
        AI=IVALUE(BULL(POINT+3:POINT+3))
        IF (AI.EQ.2) ARRAY(9)=0   ! point
        IF (AI.EQ.4) ARRAY(9)=2   ! area
        IF (AI.EQ.6) ARRAY(9)=1   ! line
        BIGAI=IVALUE(BULL(POINT+4:POINT+4))
        POINT=POINT+6

        IF (AI.GT.2) THEN                                           !a
          PRINT *,'SFLOC bulletin with line or area data - not stored!'
          PRINT *,BULL(1:120)                                       !a
          RETURN                                                    !a
        ENDIF                                                       !a

! Set lat/long in half-degrees using X4 for N/S and k for E/W
! - but there is still uncertainty about the hundreds of the longitude:
! we can only assume (in the UK) that all longitudes are less than 100!

        DO WHILE (BULL(POINT:POINT).NE.'9' .AND. POINT.LT.BULLEND)
          LAT=IVALUE(BULL(POINT:POINT+1))
          LONG=IVALUE(BULL(POINT+2:POINT+3))
          K=IVALUE(BULL(POINT+4:POINT+4))
          IF (LAT.NE.MISSING.AND.LONG.NE.MISSING.AND.K.NE.MISSING) THEN
            IF (MOD(K,5).EQ.1 .OR. MOD(K,5).EQ.3) LAT=LAT+0.5
            IF (MOD(K,5).EQ.2 .OR. MOD(K,5).EQ.3) LONG=LONG+0.5
            IF (X4.EQ.1) LAT=-LAT
            IF (K.GE.5) LONG=-LONG

! We've got a lat/long: now put it in the encoding array

            IF (ARRAY(9).EQ.0) THEN      ! If point data...
              IF (NFIXES.EQ.0) THEN      ! if first fix...          !a
                ARRAY(10)=1              ! (single fixes for points)
                ARRAY(11)=LAT                                       !a
                ARRAY(12)=LONG                                      !a
                ARRAY(13)=NF
                ARRAY(14)=BIGAI
              ELSE                       ! if not first fix,        !a
                DO I=1,14                ! copy first fix           !a
                  ARRAY(NFIXES*14+I)=ARRAY(I)                       !a
                ENDDO                                               !a
                ARRAY(NFIXES*14+11)=LAT  ! & reset lat/long         !a
                ARRAY(NFIXES*14+12)=LONG                            !a
              ENDIF                                                 !a
            ELSE                         ! If line or area...
              ARRAY(10+NFIXES*2+1)=LAT
              ARRAY(10+NFIXES*2+2)=LONG
            ENDIF
            NFIXES=NFIXES+1

! Keep max & min lat & long for index entry (no problem crossing 180
! degrees: longitude can't reach 100!)

            IF (NFIXES.EQ.1) THEN
              BOX(1) = LAT                                          !2.1
              BOX(2) = LAT                                          !2.1
              BOX(3) = LONG                                         !2.1
              BOX(4) = LONG                                         !2.1
            ELSE
              IF ( LAT.LT.BOX(1)) BOX(1) = LAT                      !2.1
              IF ( LAT.GT.BOX(2)) BOX(2) = LAT                      !2.1
              IF (LONG.LT.BOX(3)) BOX(3) = LONG                     !2.1
              IF (LONG.GT.BOX(4)) BOX(4) = LONG                     !2.1
            ENDIF
          ENDIF
          POINT=POINT+6
        ENDDO

! 9-group reached (or end of data): encode, make index entry & store.
! First make BUFR message, transposing the encoding array if data are
! point fixes

        IF (NFIXES.GT.0) THEN
          DESCR(1)=IDES(316020)
          ND=1
          IF (ARRAY(9).EQ.0) THEN        ! if point data...
            DO I=1,NFIXES
              DO J=1,14
                YARRA((J-1)*NFIXES+I)=ARRAY((I-1)*14+J)
              ENDDO
            ENDDO

            CMPRES=.TRUE.
            NELEM=14
            NOBS=NFIXES
            CALL ENBUFR(DESCR,YARRA,ND,NELEM,NOBS,ID,TOR,MESSAGE,
     &                CMPRES,L)
          ELSE                           ! if line or area...
            ARRAY(10)=NFIXES
            ARRAY(10+NFIXES*2+3)=NF
            ARRAY(10+NFIXES*2+4)=BIGAI
            CMPRES=.FALSE.
            NELEM=10+NFIXES*2+2
            NOBS=1
            CALL ENBUFR(DESCR,ARRAY,ND,NELEM,NOBS,ID,DATIME,MESSAGE,
     &                CMPRES,L)
          ENDIF

! Put lat/long in index entry as 2-byte integers (hundredths) or box

          ENTRY(12:12)=CHAR(NFIXES)
          IF (NFIXES.EQ.1) THEN
            CALL INDLALO (ENTRY, LAT, LONG)                         !2.1
          ELSE
            CALL BOXLALO (BOX, ENTRY(13:16))                        !2.1
          ENDIF

! Store these fixes & carry on if there's more data in the bulletin.

          CALL AIRSTO(DATIME,ENTRY,MESSAGE(1:L),NFTSFR,27998,ID,TOR)
          NFIXES=0
        ENDIF

! nf (code table 2836): set bottom end of range in BUFR message

        NF=IVALUE(BULL(POINT+1:POINT+1))  ! nf
        IF (NF.LE.1) THEN
          NF=NF+1
        ELSE
          NF=NF*NF
        ENDIF
      ENDDO
      RETURN
      END
