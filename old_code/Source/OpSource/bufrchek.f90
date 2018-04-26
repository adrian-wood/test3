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
! PARAMETERS  : BULL   (IN/OUT - CHARACTER*(*)) MESSAGE TO BE CHECKED.
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
! $Workfile: bufrchek.f90$ $Folder: OpSource$
! $Revision: 7$ $Date: 29/05/2011 16:45:55$
!
! CHANGE RECORD :
!
! $Log:
!  7    MetDB_Refresh 1.6         29/05/2011 16:45:55    Brian Barwell
!       Modification to truncated message check.
!  6    MetDB_Refresh 1.5         27/05/2011 15:05:34    Brian Barwell   Check
!       for truncated BUFR message added
!  5    MetDB_Refresh 1.4         22/12/2010 12:41:21    Sheila Needham  Change
!        INTENT to INOUT on BULL
!  4    MetDB_Refresh 1.3         22/12/2010 12:39:28    Sheila Needham  Remove
!        extract ICHAR3 declaration
!  3    MetDB_Refresh 1.2         22/12/2010 08:40:43    Sheila Needham
!       Changes following review
!  2    MetDB_Refresh 1.1         08/12/2010 10:09:02    Richard Weedon  add
!       use statement for value_mod
!  1    MetDB_Refresh 1.0         24/11/2010 16:28:38    Richard Weedon  Intial
!        f90 port
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Interfaces

USE ichar3_mod

IMPLICIT NONE

! Arguments

CHARACTER(LEN=*),INTENT(INOUT) ::  BULL   ! (1) BULLETIN TO BE CHECKED
INTEGER,INTENT(IN)          ::  NOW(8) ! (2) DATE/TIME OF RECEIPT (FROM 'DATIM')
INTEGER,INTENT(OUT)         ::  NBUFR  ! (3) LOCATION OF "BUFR" IN BULLETIN
INTEGER,INTENT(OUT)         ::  LENBUL ! (4) LENGTH OF MESSAGE (FROM SECTION 0)
INTEGER,INTENT(OUT)         ::  NBTYP  ! (5) BUFR TYPE & SUBTYPE (FROM SECTION 1)
INTEGER,INTENT(OUT)         ::  NBSUB  ! (6) BUFR TYPE & SUBTYPE (FROM SECTION 1)
INTEGER,INTENT(OUT)         ::  KODE   ! (7) RETURN CODE (SEE ABOVE FOR DETAILS)

! Local variables

CHARACTER(LEN=4)     ::  BUFR   ! "BUFR" IN ASCII
LOGICAL              ::  FIRST  ! FLAG FOR FIRST CALL TO "BUFRCHEK"
INTEGER              ::  IBUFR  ! LOCATION IN BUFR MESSAGE
INTEGER              ::  IEDTN  ! BUFR EDITION NUMBER IN MESSAGE
INTEGER              ::  ISECT1 ! LOCATION OF START OF BUFR SECTION 1
INTEGER              ::  J      ! GENERAL LOOP VARIABLE
INTEGER              ::  LAST   ! EXPCTED END OF BUFR MESSAGE
INTEGER              ::  LENMSG ! LENGTH OF "BULL" STRING
INTEGER              ::  NSECT  ! NUMBER OF BUFR SECTIONS TO SKIP
CHARACTER(LEN=4)     ::  SEVENS ! "7777" IN ASCII

!                                              SAVE AND DATA STATEMENTS
SAVE FIRST, BUFR, SEVENS
DATA FIRST/.TRUE./
!                                   (FIRST CALL ONLY) SET UP ASCII TEXT
IF (FIRST) THEN
   SEVENS = CHAR(55) // CHAR(55) // CHAR(55) // CHAR(55) ! '7777'
   BUFR   = CHAR(66) // CHAR(85) // CHAR(70) // CHAR(82) ! 'BUFR'
   FIRST = .FALSE.
END IF
!                                        Initialisation - no errors yet
KODE = 0
LENBUL = 0
NBTYP = -1
NBSUB = -1
!                                                       LOOK FOR "BUFR"
NBUFR = INDEX(BULL(:),BUFR)
LENMSG = LEN(BULL)

IFCONST1: &
IF (NBUFR <= 0) THEN         ! "BUFR" NOT FOUND: RETURN CODE = 1
   KODE = 1

ELSE IF (LENMSG < NBUFR+37) THEN ! BUFR MESSAGE TRUNCATED: RETURN CODE = 2
   KODE = 2 

ELSE IFCONST1 ! "BUFR" FOUND: GET MESSAGE LENGTH & START OF SECTION 1

   IEDTN = ICHAR(BULL(NBUFR+7:NBUFR+7)) ! BUFR EDITION NO.
   IFCONSTR2 : &
   IF (IEDTN < 2) THEN
!                                   BUFR EDITION = 0 OR 1:  ADD SECTION
!                                   LENGTHS TO GET TOTAL MESSAGE LENGTH
      ISECT1 = NBUFR + 4
      NSECT = 3
      IF (ICHAR(BULL(ISECT1+7:ISECT1+7)) > 127) NSECT = 4
      IBUFR = ISECT1
      DO J=1,NSECT
         IBUFR = IBUFR + ICHAR3(BULL(IBUFR:IBUFR+2))
         IF (IBUFR > LENMSG-2) EXIT
      END DO ! J
      LAST = IBUFR + 3
      LENBUL = LAST - NBUFR + 1
   ELSE IFCONSTR2
!                                             BUFR EDITION > 1:  DECODE
!                                                 LENGTH FROM SECTION 1
      ISECT1 = NBUFR + 8
      LENBUL = ICHAR3(BULL(NBUFR+4:NBUFR+6))
      LAST = NBUFR + LENBUL - 1
   END IF IFCONSTR2
!                                            FIND BUFR TYPE AND SUBTYPE
   IF (IEDTN < 4) THEN
     NBTYP = ICHAR(BULL(ISECT1+8:ISECT1+8))
     NBSUB = ICHAR(BULL(ISECT1+9:ISECT1+9))
   ELSE
     NBTYP = ICHAR(BULL(ISECT1+10:ISECT1+10))
     NBSUB = ICHAR(BULL(ISECT1+12:ISECT1+12))
   END IF
!                                   CHECK FOR END OF MESSAGE INDICATOR.
!                                IF NOT WHERE EXPECTED: RETURN CODE = 2

   IF (LENBUL < 4 .OR. LAST > LENMSG) THEN
     KODE = 2
   ELSE
     IF (BULL(LAST-3:LAST) /= SEVENS) KODE = 2
   END IF
!                            CONTINUE IF END OF MESSAGE INDICATOR FOUND
   IFCONSTR3 : &
   IF (KODE == 0) THEN
!                                   INSERT TIME OF RECEIPT IN SECTION 1
      IFCONSTR4 : &
      IF (NOW(1) >= 0) THEN
         IF (IEDTN < 4) THEN     ! EDITIONS 0-3: YEAR IS
                                 ! IN RANGE 1-100, NOT 0-99
            IBUFR = ISECT1 + 12
            BULL(IBUFR:IBUFR) = CHAR(1+MOD(NOW(8)-1,100)) ! YEAR

         ELSE                    ! EDITION 4: USE FULL YEAR;
                                 ! ALSO INCLUDE SECONDS
            IBUFR = ISECT1 + 16
            BULL(IBUFR-1:IBUFR) = CHAR(NOW(8)/256) // &
                 CHAR(MOD(NOW(8),256))           ! FULL YEAR
            BULL(IBUFR+5:IBUFR+5) = CHAR(NOW(3)) ! SECONDS
         END IF
!
         DO J=7,4,-1             ! MONTH, DAY, HOUR, MINUTE
            IBUFR = IBUFR + 1
            BULL(IBUFR:IBUFR) = CHAR(NOW(J))
         END DO ! J
      END IF IFCONSTR4
   END IF IFCONSTR3
END IF IFCONST1
!                                             RETURN TO CALLING PROGRAM
RETURN
END SUBROUTINE BUFRCHEK
