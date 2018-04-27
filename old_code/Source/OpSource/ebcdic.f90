SUBROUTINE EBCDIC (LEN, STRING)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : EBCDIC
!
! PURPOSE       : CONVERTS A CHARACTER STRING FROM ASCII TO EBCDIC.
!                 TEXT CONVERSION IS BASED ON SUBROUTINE "ASC2EB" IN
!                 "MET.SRCELIB(IBMIS08)" BUT WITH THREE DIFFERENCES.
!
!                  1. LOWER CASE ASCII CONVERTS TO UPPER CASE EBCDIC.
!                  2. CARRAIGE RETURN (HEX "13") CONVERTS TO HEX "15".
!                  3. HEX "80" TO "FF" (SHOULDN'T OCCUR IN ASCII)
!                     CONVERT TO THE SAME CHARACTERS AS "00" TO "7F".
!
!                 (ITEM (2) SIMULATES WHAT WAS USED BY OLD ASSEMBLER
!                  SDB SOFTWARE AND IS STILL EXPECTED BY SOME FORTRAN
!                  ROUTINES USED BY THE MET.D.B.)
!
! USAGE         : CALL EBCDIC (LEN, STRING)
!
! ARGUMENTS     : LEN      (I)   LENGTH OF STRING TO BE CONVERTED
!                 STRING  (I/O)  (C*(*)) CHARACTER STRING TO BE
!                              CONVERTED FROM ASCII TO EBCDIC
!
! REVISION INFO:
!
! $Workfile: ebcdic.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 22/12/2010 13:09:46$
!
! CHANGE RECORD:
!
! $Log:
!  4    MetDB_Refresh 1.3         22/12/2010 13:09:46    Sheila Needham  Change
!        from CHAR*1 STRING(LEN) to CHAR*LEN STRING
!  3    MetDB_Refresh 1.2         22/12/2010 09:53:20    Sheila Needham
!       Updated following review
!  2    MetDB_Refresh 1.1         13/12/2010 12:21:29    Richard Weedon
!       Revision info corrected
!  1    MetDB_Refresh 1.0         30/11/2010 11:19:41    Richard Weedon
!       Initial version. Note Data declaration altered from original. Column
!       and row refs taken out as these will not compile.
! $
!
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
IMPLICIT NONE

! Arguments

INTEGER,         INTENT(IN)    :: LEN        ! (1) LENGTH OF "STRING"
CHARACTER(LEN=LEN),INTENT(INOUT) :: STRING     ! (2) CHAR STR TO BE CONV

! Local Variables

INTEGER   ::  J                ! LOOP VARIABLE
INTEGER   ::  LOOKUP(0:255)    ! LOOKUP TABLE
!
!INITIALISE LOOKUP TABLE
DATA LOOKUP / &
!  0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F <col
   0,  1,  2,  3, 55, 45, 46, 47, 22,  5, 37, 11, 12, 21, 14, 15, &
  16, 17, 18, 19, 60, 61, 50, 38, 24, 25, 63, 39, 28, 29, 30, 31, &
  64, 79,127,123, 91,108, 80,125, 77, 93, 92, 78,107, 96, 75, 97, &
 240,241,242,243,244,245,246,247,248,249,122, 94, 76,126,110,111, &
 124,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214, &
 215,216,217,226,227,228,229,230,231,232,233, 74,224, 90, 95,109, &
 121,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214, &
 215,216,217,226,227,228,229,230,231,232,233,192,106,208,161,  7, &
!
!     ("80"-"FF" CONVERTED TO SAME AS "00"-"7F".)
!
   0,  1,  2,  3, 55, 45, 46, 47, 22,  5, 37, 11, 12, 21, 14, 15, &
  16, 17, 18, 19, 60, 61, 50, 38, 24, 25, 63, 39, 28, 29, 30, 31, &
  64, 79,127,123, 91,108, 80,125, 77, 93, 92, 78,107, 96, 75, 97, &
 240,241,242,243,244,245,246,247,248,249,122, 94, 76,126,110,111, &
 124,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214, &
 215,216,217,226,227,228,229,230,231,232,233, 74,224, 90, 95,109, &
 121,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214, &
 215,216,217,226,227,228,229,230,231,232,233,192,106,208,161,  7/
!
!
DO J=1,LEN
   STRING(J:J) = CHAR(LOOKUP(ICHAR(STRING(J:J))))
END DO ! J
!
RETURN
END SUBROUTINE EBCDIC
