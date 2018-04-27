      SUBROUTINE BUFRCHEK (BULL, NOW, NBUFR, LENBUL, NBTYP, NBSUB, KODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE  : BUFRCHEK
!
! USAGE       : CALL BUFRCHEK
!                    (BULL, NOW, NBUFR, LENBUL, NBTYP, NBSUB, KODE)
!
! PURPOSE     : TO CHECK WHETHER A MESSAGE IS BUFR ENCODED AND IF SO,
!               TO CHECK THE MESSAGE LENGTH. OPTIONALLY, A TIME OF
!               RECEIPT CAN ALSO BE INSERTED IN THE MESSAGE.
!
! DESCRIPTION : 'BUFRCHEK' EXAMINES THE TEXT OF MESSAGE 'BULL' AND
!               LOCATES 'BUFR' AT THE START OF THE MESSAGE. IF FOUND,
!               THE BUFR DATA CATEGORY TYPE AND SUB-TYPE ARE DECODED
!               FROM SECTION 1 AND A CHECK IS MADE THAT THE LOCATION
!               OF THE END-OF-MESSAGE MARKER (ASCII '7777') AGREES
!               WITH THE MESSAGE LENGTH EXTRACTED FROM SECTION 0 (OR,
!               IF NOT AVAILABLE THERE, CALCULATED BY ADDING THE
!               SECTION LENGTHS TOGETHER).
!                 'BUFRCHEK' ALSO EXTRACTS THE BUFR DATA CATEGORY TYPE
!               AND SUB-TYPE AND RETURNS THEM TO THE CALLING PROGRAM.
!               IF SUPPLIED BY THE USER, IT CAN ALSO INSERT A TIME OF
!               RECEIPT IN SECTION 1 OF THE BUFR MESSAGE.
!                 THE OUTCOME OF THE APPLIED CHECKS IS INDICATED BY A
!               RETURN CODE (SEE BELOW FOR INTERPRETATION).
!
! PARAMETERS  : BULL   (INPUT - CHARACTER*(*)) MESSAGE TO BE CHECKED.
!               NOW    (INPUT)  8-ELEMENT INTEGER ARRAY WITH DATE/TIME
!                               OF RECEIPT FROM CALL TO 'DATIM'. (SET
!                               NOW(1)<0 IF T.O.R. NOT TO BE INSERTED)
!               NBUFR  (OUTPUT) START OF BUFR-ENCODED MESSAGE.
!               LENBUL (OUTPUT) LENGTH OF BUFR-ENCODED MESSAGE.
!               NBTYP  (OUTPUT) BUFR DATA CATEGORY TYPE DECODED FROM
!                               SECTION 1 OF THE MESSAGE.
!               NBSUB  (OUTPUT) BUFR DATA CATEGORY SUB-TYPE DECODED
!                               FROM SECTION 1 OF THE MESSAGE.
!               KODE   (OUTPUT) RETURN CODE - CODED AS FOLLOWS:
!
!                        0 - MESSAGE SUCCESSFULLY CHECKED,
!                        1 - 'BUFR' NOT FOUND AT START OF MESSAGE,
!                        2 - '7777' NOT FOUND IN EXPECTED LOCATION
!                            AT END OF MESSAGE.
!
!                      IF KODE=1, 'NBUFR' AND 'LENBUL' ARE RETURNED
!                      AS ZERO, AND 'NBTYP' AND 'NBSUB' AS -1.
!
! CALLED BY   : BUFRDAT, MDBSTOR, STOREUM.
!
! REVISION INFO :
!
! $Workfile: bufrchek.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 28/09/2007 11:38:10$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         28/09/2007 11:38:10    Brian Barwell
!       Changes to handle bulletins coded in BUFR edition 4.
!  1    Met_DB_Project 1.0         30/01/2006 20:21:19    Sheila Needham  
! $
! Revision 2.0  2001/05/31 14:16:37  usmdb
! Replaced function statement calls with call to external
! function ICHAR3. Removed unused variables, added copyright and
! modified header - S.Cox
!
! Revision 1.1  99/04/12  11:05:48  11:05:48  usmdb (Generic MetDB account)
! Initial revision
!
! ORIGINAL VERSION BY BRIAN BARWELL:  OPERATIONAL FROM 19 APRIL 1999.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!
      INTEGER IBUFR            ! LOCATION IN BUFR MESSAGE
      INTEGER ICHAR3           ! C*3 TO I*4 CONVERSION ROUTINE
      INTEGER IEDTN            ! BUFR EDITION NUMBER IN MESSAGE      !2
      INTEGER ISECT1           ! LOCATION OF START OF BUFR SECTION 1
      INTEGER J                ! GENERAL LOOP VARIABLE
      INTEGER KODE             ! RETURN CODE (SEE ABOVE FOR DETAILS)
      INTEGER LAST             ! EXPCTED END OF BUFR MESSAGE
      INTEGER LENBUL           ! LENGTH OF MESSAGE (FROM SECTION 0)
      INTEGER LENMSG           ! LENGTH OF "BULL" STRING
      INTEGER NBTYP, NBSUB     ! BUFR TYPE & SUBTYPE (FROM SECTION 1)
      INTEGER NBUFR            ! LOCATION OF "BUFR" IN BULLETIN
      INTEGER NOW(8)           ! DATE/TIME OF RECEIPT (FROM 'DATIM')
      INTEGER NSECT            ! NUMBER OF BUFR SECTIONS TO SKIP
      LOGICAL FIRST            ! FLAG FOR FIRST CALL TO "BUFRCHEK"
      CHARACTER*4 BUFR, SEVENS ! "BUFR" AND "7777" IN ASCII
      CHARACTER*132 HEAD       ! FOR REVISION INFORMATION
      CHARACTER*(*) BULL       ! BULLETIN TO BE CHECKED
!
!                                              SAVE AND DATA STATEMENTS
      SAVE FIRST, BUFR, SEVENS
      DATA FIRST/.TRUE./
!                                   (FIRST CALL ONLY) SET UP ASCII TEXT
      IF (FIRST) THEN
!                                                  REVISION INFORMATION
         HEAD = '$Workfile: bufrchek.f$ ' //
     &          '$Revision: 2$ $Date: 28/09/2007 11:38:10$'
         SEVENS = CHAR(55) // CHAR(55) // CHAR(55) // CHAR(55) ! '7777'
         BUFR   = CHAR(66) // CHAR(85) // CHAR(70) // CHAR(82) ! 'BUFR'
         FIRST = .FALSE.
      END IF
!                                                       LOOK FOR "BUFR"
      NBUFR = INDEX(BULL(:),BUFR)
!
      IF (NBUFR.LE.0) THEN !          "BUFR" NOT FOUND: RETURN CODE = 1
         LENBUL = 0
         NBTYP = -1
         NBSUB = -1
         KODE = 1
!
      ELSE !      "BUFR" FOUND: GET MESSAGE LENGTH & START OF SECTION 1
!
         LENMSG = LEN(BULL)
         IEDTN = ICHAR(BULL(NBUFR+7:NBUFR+7)) ! BUFR EDITION NO.     !2
         IF (IEDTN.LT.2) THEN                                        !2
!                                   BUFR EDITION = 0 OR 1:  ADD SECTION
!                                   LENGTHS TO GET TOTAL MESSAGE LENGTH
            ISECT1 = NBUFR + 4
            NSECT = 3
            IF (ICHAR(BULL(ISECT1+7:ISECT1+7)).GT.127) NSECT = 4
            IBUFR = ISECT1
            DO J=1,NSECT
               IBUFR = IBUFR + ICHAR3(BULL(IBUFR:IBUFR+2))          !2.0
               IF (IBUFR.GT.LENMSG-2) GO TO 1
            END DO ! J
    1       LAST = IBUFR + 3
            LENBUL = LAST - NBUFR + 1
         ELSE
!                                             BUFR EDITION > 1:  DECODE
!                                                 LENGTH FROM SECTION 1
            ISECT1 = NBUFR + 8
            LENBUL = ICHAR3(BULL(NBUFR+4:NBUFR+6))                  !2.0
            LAST = NBUFR + LENBUL - 1
         END IF
!                                            FIND BUFR TYPE AND SUBTYPE
         IF (IEDTN.LT.4) THEN                                        !2
           NBTYP = ICHAR(BULL(ISECT1+8:ISECT1+8))
           NBSUB = ICHAR(BULL(ISECT1+9:ISECT1+9))
         ELSE                                                        !2
           NBTYP = ICHAR(BULL(ISECT1+10:ISECT1+10))                  !2
           NBSUB = ICHAR(BULL(ISECT1+12:ISECT1+12))                  !2
         END IF                                                      !2
!                                   CHECK FOR END OF MESSAGE INDICATOR.
!                                IF NOT WHERE EXPECTED: RETURN CODE = 2
!
         IF (LENBUL.LT.4 .OR. LAST.GT.LENMSG .OR.
     &       BULL(LAST-3:LAST).NE.SEVENS) THEN
            KODE = 2
         ELSE
!                          END OF MESSAGE AS EXPECTED:  RETURN CODE = 0
            KODE = 0
!                                   INSERT TIME OF RECEIPT IN SECTION 1
            IF (NOW(1).GE.0) THEN
               IF (IEDTN.LT.4) THEN    ! EDITIONS 0-3: YEAR IS       !2
                                       ! IN RANGE 1-100, NOT 0-99    !2
                  IBUFR = ISECT1 + 12
                  BULL(IBUFR:IBUFR) = CHAR(1+MOD(NOW(8)-1,100)) ! YEAR

               ELSE                    ! EDITION 4: USE FULL YEAR;   !2
                                       ! ALSO INCLUDE SECONDS        !2
                  IBUFR = ISECT1 + 16                                !2
                  BULL(IBUFR-1:IBUFR) = CHAR(NOW(8)/256) //          !2
     &                 CHAR(MOD(NOW(8),256))           ! FULL YEAR   !2
                  BULL(IBUFR+5:IBUFR+5) = CHAR(NOW(3)) ! SECONDS     !2
               END IF                                                !2
!
               DO J=7,4,-1             ! MONTH, DAY, HOUR, MINUTE
                  IBUFR = IBUFR + 1
                  BULL(IBUFR:IBUFR) = CHAR(NOW(J))
               END DO ! J
            END IF
         END IF
      END IF
!                                             RETURN TO CALLING PROGRAM
      RETURN
      END
