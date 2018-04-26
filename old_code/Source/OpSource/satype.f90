SUBROUTINE SATYPE (TEXT, LIST1, LIST2, NTYPES, NUMTYP)

!-----------------------------------------------------------------------
!
! PROGRAM       : SATYPE
!
! PURPOSE       : TO SEARCH A SEQUENCE OF RANGES OF CHARACTER STRINGS
!                 (E.G. BULLETIN HEADERS OR DATA TYPE NAMES) TO FIND
!                 THE ONE CORRESPONDING TO A GIVEN STRING.
!
! DESCRIPTION   : A CHARACTER STRING IS COMPARED WITH A BATCH OF NON-
!                 OVERLAPPING RANGES AND THE VALUE IS RETURNED OF THE
!                 RANGE CONTAINING THE SPECIFIED STRING. RANGES ARE
!                 DEFINED BY THEIR FIRST AND LAST MEMBERS AND MUST BE
!                 IN ALPHANUMERICAL ORDER. SEARCHING IS DONE USING
!                 SUCCESSIVE HALVING OF THE SEARCH WINDOW.
!                 IF THE SPECIFIED STRING DOES NOT OCCUR IN ANY OF
!                 THE RANGES, A ZERO OR NEGATIVE VALUE IR RETURNED.
!
! CALLED BY     : SATIND
!
! ARGUMENTS     : (1) CHARACTER STRING TO BE SEARCHED FOR.
!                 (2) ARRAY OF FIRST CHARACTER STRINGS IN EACH RANGE.
!                 (3) ARRAY OF LAST CHARACTER STRINGS IN EACH RANGE.
!                 (4) NUMBER OF RANGES (= DIMENSION OF ABOVE ARRAYS).
!                 (5) NUMBER OF RANGE CONTAINING SPECIFIED STRING
!                     (VALUE RETURNED BY THIS ROUTINE).
!
! REVISION INFO :
!
!
! $Workfile: satype.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 16/03/2011 12:25:34$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         16/03/2011 12:25:34    Alison Weir     Amend
!       to avoid array subscript out of range errors
!  1    MetDB_Refresh 1.0         14/12/2010 16:21:09    Rosemary Lavery
!       Initial Port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)  :: TEXT           ! (A1) (Input) Text to be searched for
INTEGER, INTENT(IN)            :: NTYPES         ! (A4) (Input) Number of search ranges
CHARACTER (LEN=*), INTENT(IN)  :: LIST1(NTYPES)  ! (A2) Array of beginnings of text ranges
CHARACTER (LEN=*), INTENT(IN)  :: LIST2(NTYPES)  ! (A3) Array of ends of text ranges
INTEGER, INTENT(OUT)           :: NUMTYP         ! (A5) (Output) Number of range containing TEXT

! Local Variables

INTEGER  :: IFIRST    ! ) First and last ranges during binary
INTEGER  :: ILAST     ! )   search (they home in on NUMTYP)

!-----------------------------------------------------------------------
!                                INITIALISE TO FULL RANGE OF DATA TYPES
IFIRST = 0
ILAST  = NTYPES
!                                            CHOOSE TYPE NEAR MID-RANGE
    1 CONTINUE
NUMTYP = (IFIRST + ILAST + 1) / 2
!                                        COMPARE WITH CURRENT DATA TYPE
!                                          LIMITS AND RESET ONE OF THEM
IF (TEXT < LIST1(NUMTYP)) THEN
   ILAST = NUMTYP - 1
ELSE
   IFIRST = NUMTYP
END IF
!                                           SEARCH ENDS IF FIRST = LAST
IF (IFIRST /= ILAST) GO TO 1
!
!             CHECK UPPER LIMIT OF VALID BULLETIN HEADERS FOR DATA TYPE
!           FOUND (SIGN OF "NUMTYP" IS CHANGED IF HEADER OUTSIDE RANGE)
!
NUMTYP = IFIRST
IF (NUMTYP /= 0) THEN
  IF (TEXT > LIST2(NUMTYP)) NUMTYP = -NUMTYP
END IF

RETURN
END SUBROUTINE SATYPE
