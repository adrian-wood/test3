      SUBROUTINE STORCHEK (BULL, BUL18, DATYPE, STORFLAG, MIMJ)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : STORCHEK
!
! PURPOSE       : STORCHEK IS USED TO CHECK THE DATA TYPE AND STORAGE
!                 REQUIREMENTS FOR CERTAIN TYPES OF DATA. FOR SOME
!                 DATA TYPES THE TYPE IS CHANGED (E.G. "TEMP", "PILOT"
!                 AND "DROPSOND" ALL BECOME "UPRAIR"), OR THE STORAGE
!                 FLAG IS CHANGED DEPENDING ON THE VALUE OF'MIMIMJMJ'
!                 IN THE MESSAGE.
!
! CALLED BY     : ANYTHING WHICH NEEDS TO STORE CHARACTER MESSAGES.
!
! USAGE         : CALL STORCHEK (BULL, MIMJ, DATYPE, STORFLAG)
!
! PARAMETERS    : BULL      (INPUT)  CHARACTER*(*) MESSAGE TEXT.
!                 BUL18     (INPUT)  'TTAAII CCCC YYGGGG' (GTS HEADER).
!                 DATYPE    (IN/OUT) DATA TYPE (UP TO 8 CHARACTERS).
!                 STORFLAG  (OUTPUT) STORAGE FLAG (T=STORE IT, F=DON'T)
!                 MIMJ      (OUTPUT) 'MIMIMJMJ' DECODED FROM MESSAGE.
!
! CALLS         : ACHTST, DREG
!
! REVISION INFO :
!
! $Workfile: storchek.f$ $Folder: pre_refresh$
! $Revision: 3$ $Date: 04/11/2008 14:52:32$
!
! CHANGE RECORD :
!
! $Log:
!  3    Met_DB_Project 1.2         04/11/2008 14:52:32    Brian Barwell   If
!       MIMJ is 'SHPANC' look for 'BBXX' further down the bulletin.
!  2    Met_DB_Project 1.1         22/04/2008 13:57:39    Brian Barwell   Add
!       section to recognise MOBSYN. Delete PLAINOB section and 'MIMJ' check
!       for SFLOC. Update revision information and comment characters.
!  1    Met_DB_Project 1.0         30/01/2006 20:24:40    Sheila Needham  
! $
! Revision 2.1  2002/11/04  14:24:02  14:24:02  usmdb (Generic MetDB account)
! 2.1.  18 November 2002.  Brian Barwell.  Change 108/02.
! Suppress mobile SYNOP messages. Delete obsolete SATEM check.
!
! Revision 2.0  2001/07/03  11:08:38  11:08:38  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.2  2000/11/07  12:11:51  12:11:51  usmdb (Generic MetDB account)
! 20NOV00 R Hirst
! Enable storage of SPECIs (in the METAR datastore)
!
! Revision 1.1  2000/06/08  15:43:05  15:43:05  usmdb (Generic MDB account)
! Initial revision
!
! 1.0   NOV 98:  ORIGINAL VERSION  (BRIAN BARWELL)
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!
      INTEGER ICH                   ! INDEX FOR LOCATION IN "BULL"
      INTEGER LENTYP                ! DATA TYPE NAME LENGTH
      INTEGER NONLET                ! INTEGER ARGUMENT FOR "ACHTST"
!
      LOGICAL ALLETT, OSPACE, OLFCR ! ARGUMENTS FOR CALL TO "ACHTST"
      LOGICAL ERROR                 ! FLAG FOR PRODUCING DREGS ENTRY
      LOGICAL FIRST                 ! .TRUE. IF FIRST CALL           !2
      LOGICAL STORFLAG              ! 'STORAGE REQUIRED' FLAG
!
      CHARACTER CH                  !  CHARACTER FROM "BULL"
      CHARACTER*7 MIMJ              !  'MIMIMJMJ' FROM MESSAGE TEXT
      CHARACTER*8 DATYPE            !  DATA TYPE
      CHARACTER*18 BUL18            !  'TTAAII CCCC YYGGGG' (GTS HEADER)
      CHARACTER*40 TEXT40           !  ERROR MESSAGE FOR DREGS ENTRY
      CHARACTER*80 HEAD             !  FOR REVISION INFORMATION      !2
      CHARACTER*(*) BULL            !  BULLETIN TEXT

      DATA FIRST /.TRUE./                                            !2
!                                                  REVISION INFORMATION
      IF (FIRST) THEN                                                !2
        HEAD = '$Workfile: storchek.f$ ' //
     &         '$Revision: 3$ $Date: 04/11/2008 14:52:32$'
        FIRST = .FALSE.                                              !2
      END IF                                                         !2

!=======================================================================
! (1) BULLETIN PREPROCESSING - LOOK FOR 'MIMIMJMJ'
!=======================================================================

      ERROR = .FALSE.
!                            LOCATE FIRST LETTER IN FIRST 10 CHARACTERS
      ICH = 1
      CH = BULL(1:1)
      DO WHILE (ICH.LE.10 .AND. (CH.LT.'A' .OR. CH.GT.'Z' .OR.
     &         (CH.GT.'I' .AND. CH.LT.'J') .OR.
     &         (CH.GT.'R' .AND. CH.LT.'S'))) ! NOT A LETTER
         ICH = ICH + 1
         CH = BULL(ICH:ICH)
      END DO
!                                                    EXTRACT 'MIMIMJMJ'
      MIMJ = ' '
      IF (ICH.LE.10) THEN ! LETTER WAS FOUND
         CALL ACHTST (ICH, 8, ALLETT, NONLET, OSPACE, OLFCR, BULL)
         IF (ALLETT .OR. (OSPACE .OR. OLFCR)) THEN
            IF (ALLETT) NONLET = 9
            MIMJ = BULL(ICH:ICH+NONLET-2)
            IF (MIMJ.EQ.'NIL' .OR. MIMJ.EQ.'OTR') MIMJ = ' '
         END IF
!                IF 'SHPANC' FOUND (COMMON IN SHPSYN FROM PANC) LOOK !3
!                                   FOR 'BBXX' IN NEXT 10 CHARACTERS !3
         IF (MIMJ.EQ.'SHPANC') THEN                                  !3
            ICH = ICH + NONLET                                       !3
            IF (INDEX(BULL(ICH:ICH+9),'BBXX').GT.0) THEN             !3
               MIMJ = 'BBXX'                                         !3
            ELSE                                                     !3
               MIMJ = ' '                                            !3
            END IF                                                   !3
         END IF                                                      !3
      END IF

!=======================================================================
! (2) ADDITIONAL CHECKS FOR VARIOUS DATA TYPES
!=======================================================================

!                        "UPRAIR" COVERS "TEMP", "PILOT" AND "DROPSOND"

      IF (DATYPE.EQ.'TEMP' .OR. DATYPE.EQ.'PILOT' .OR.
     &    DATYPE.EQ.'DROPSOND') THEN
          DATYPE = 'UPRAIR'

!                      CHANGE "SYNOP" TO "LNDSYN", "SHPSYN" OR "MOBSYN"
!          (OR "NCM" IF MIMJ='OLDC' AS THESE COME IN WITH SYNOP HEADERS)

      ELSE IF (DATYPE.EQ.'SYNOP') THEN
         IF (MIMJ.EQ.'OLDC') THEN
            DATYPE = 'NCM'
         ELSE IF (MIMJ.EQ.'BBXX') THEN
            DATYPE = 'SHPSYN'
         ELSE IF (MIMJ.EQ.'AAXX' .OR. MIMJ.EQ.'OLDS' .OR.
     &            MIMJ.EQ.' ') THEN
            DATYPE = 'LNDSYN'
         ELSE IF (MIMJ.EQ.'OOXX') THEN                               !2
            DATYPE = 'MOBSYN'                                        !2
         ELSE
            STORFLAG = .FALSE.
            LENTYP = 5
            ERROR = .TRUE.
         END IF
!                                        "SPECIAL" STORED WITH "METARS"
      ELSE IF (DATYPE.EQ.'SPECIAL') THEN                            !1.2
         DATYPE = 'METARS'                                          !1.2
!                                                 CHECK MIMJ FOR SUBSEA
!                           'MiMiMjMj' =   JJ..   KK..   MMXX     NNXX
!                            Data type =  BATHY  TESAC  WAVEOB  TRACKOB

      ELSE IF (DATYPE.EQ.'SUBSEA') THEN
         IF (MIMJ.EQ.'NNXX') THEN
            DATYPE = 'TRKOB'
         ELSE IF (MIMJ.NE.'JJVV' .AND.MIMJ.NE.'JJXX' .AND.MIMJ.NE.'JJYY'
     &      .AND. MIMJ.NE.'KKXX' .AND.MIMJ.NE.'KKYY') THEN
            STORFLAG = .FALSE.
            LENTYP = 6
            IF (MIMJ.NE.'MMXX') ERROR = .TRUE. ! (I.E. NOT WAVEOB)
         END IF
!                                                  CHECK MIMJ FOR BUOYS
      ELSE IF (DATYPE.EQ.'BUOY') THEN
         IF (MIMJ.NE.'ZZYY') THEN
            STORFLAG = .FALSE.
            LENTYP = 4
            ERROR = .TRUE.
         END IF
!                                 REJECT GOES AND CHECK MIMJ FOR SATOBS
!                           (GOES DATA IS NOW ACQUIRED IN BUFR VIA GTS)

      ELSE IF (DATYPE.EQ.'SATOBS') THEN
!                                            GOES CHECK
         IF (BUL18(8:11).EQ.'KWBC' .AND.
     &      (BUL18(1:4) .EQ.'TWNA' .OR. BUL18(1:4).EQ.'TWSA')) THEN
            STORFLAG = .FALSE.
!                                            MIMJ CHECK
         ELSE IF (MIMJ.NE.'YYXX') THEN
            STORFLAG = .FALSE.
            LENTYP = 6
            ERROR = .TRUE.
         END IF
      END IF

!=======================================================================
! (3) PRINT MESSAGE & WRITE DREGS ENTRY IF FAILED CHECKS
!=======================================================================

      IF (ERROR) THEN
         IF (MIMJ.EQ.' ') THEN
            ICH = 20
            TEXT40 = 'NO "MIMIMJMJ" CODED IN BULLETIN.'
         ELSE
            ICH = 21
            TEXT40 = 'UNEXPECTED "MIMIMJMJ" = ' //MIMJ(1:NONLET-1) //'.'
         END IF
         CALL DREG (ICH, TEXT40, BULL, 'STORCHEK', DATYPE, BUL18,0,' ')
         WRITE (6,'(T5,A,T15,3A)') 'STORCHEK:',
     &              DATYPE(1:LENTYP), ' REPORT WITH ', TEXT40
      END IF

      RETURN
      END
