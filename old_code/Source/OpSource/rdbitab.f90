 SUBROUTINE RDBITAB(CINDEX,LOCALD,BINDEX,NDEX,MESTRUCT,BINMAX,&
 LFLAG,IFAIL)
!
!-----------------------------------------------------------------------
! subroutine     : RDBITAB
!
! portablity     : ANSI standard except for '!' used for comments,
!                : IMPLICIT NONE and variable length names greater than
!                : 6 characters.
!
! purpose        : to extract the bit index corresponding to the
!                : sequence given by LOCALD.
!                : BINDEX, NDEX and MESTRUCT are two dimensional in the
!                : calling program so that once a sequence has been
!                : found it is stored for future reference.  Indexes may
!                : come from different MetDB datasets (i.e. in storage
!                : programs).
!                : There is no validation of the input string i.e. to
!                : check where the end is, it assumes the element index
!                : has been set up correctly.
!
! called by      : BITINDX, BUFINDX
!
! calls          : nothing
!
! arguments      : CINDEX   (ip) - element index record
!                : LOCALD   (ip) - sequence identifier
!                : BINDEX   (op) - list of index entries
!                : NDEX     (op) - number of element index entries
!                : MESTRUCT (op) - message structure
!                : BINMAX   (ip) - maximum no. of index entries allowed
!                : LFLAG    (ip) - flag for diagnostics
!                : IFAIL    (op) - error code 16= table not found
!                                             8 = list truncated
!
!
! revision info  :
!
!
! change record  :
!
! $Log:
!  5    MetDB_Refresh 1.4         10/11/2010 16:03:58    Richard Weedon  peer
!       review rework complete
!  4    MetDB_Refresh 1.3         28/10/2010 16:29:42    Richard Weedon
!       Updated
!  3    MetDB_Refresh 1.2         25/10/2010 10:12:01    Richard Weedon
!       updated
!  2    MetDB_Refresh 1.1         21/10/2010 16:33:01    Richard Weedon
!       Updated for declarations
!  1    MetDB_Refresh 1.0         21/10/2010 15:36:01    Richard Weedon  Ported
!        to F95 standard , passed basic f95 -c port test
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
!
IMPLICIT NONE
!
!-----------------------------------------------------------------------
! declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

INTEGER :: BINMAX
INTEGER :: I
INTEGER :: IFAIL
INTEGER :: IP
INTEGER :: J
INTEGER :: NDEX
INTEGER :: NUMTAB     ! no. of index tables
INTEGER :: TABLEN(99)

!-----------------------------------------------------------------------
! declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

LOGICAL  LFLAG

!-----------------------------------------------------------------------
! declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER(LEN=*)  ::  BINDEX(BINMAX)
CHARACTER(LEN=*)  ::  CINDEX(*)
CHARACTER(LEN=12) ::  LASTINDX
CHARACTER(LEN=6)  ::  LOCALD
CHARACTER(LEN=*)  ::  MESTRUCT
CHARACTER(LEN=6)  ::  TABLID(99)

!-----------------------------------------------------------------------
! data statements
!-----------------------------------------------------------------------

DATA     LASTINDX/'            '/

!-----------------------------------------------------------------------
! SAVE statement to ensure all variables are still set on next call
!-----------------------------------------------------------------------

SAVE

IFAIL=0                                                         !A

IF_CONST1 : &
IF (LASTINDX /= CINDEX(1)(1:12)) THEN                         !1.2

!----------------------------------------------------------------------
! An index table consists of the identifier, number of entries, 36 byte
! entries and a 500 byte message structure.
!
! First time through set up a list of table identifiers (TABLID)
! and number of index entries (TABLEN)
!----------------------------------------------------------------------

  IF (LFLAG) WRITE(*,*)'In RDBITAB : New index = ',&
    CINDEX(1)(1:12)

  READ(CINDEX(1)(1:2),'(I2.2)')NUMTAB

  DO I=1,NUMTAB                                               !1.2
    TABLID(I)=CINDEX(I)(3:8)                                  !1.2
    READ(CINDEX(I)(9:12),'(I4)')TABLEN(I)                     !1.2
  END DO                                                      !1.2

  LASTINDX=CINDEX(1)(1:12)                                    !1.2

END IF IF_CONST1

!----------------------------------------------------------------------
! Now find the table for the current LOCALD sequence
!----------------------------------------------------------------------

DO I=1,NUMTAB

  IF_CONST2 : &
  IF (LOCALD == TABLID(I)) THEN

!----------------------------------------------------------------------
! set up BINDEX
!----------------------------------------------------------------------

    NDEX=TABLEN(I)
    IF (NDEX >  BINMAX) THEN                                 !C
      IFAIL=8
      WRITE(*,*)'MDB WARNING: In RDBITAB: ELEMENT INDEX TOO ',&
       'LARGE FOR SEQUENCE ',LOCALD,' - TRUNCATED AT ',BINMAX
      NDEX=BINMAX                                               !C
    END IF

    IP=13                                                     !1.2
    DO J=1,NDEX
      BINDEX(J)=CINDEX(I)(IP:IP+35)                           !1.2
      IP=IP+36
    END DO
    MESTRUCT=CINDEX(I)(IP:IP+499)                           !1.2!B
    GOTO 999
  END IF IF_CONST2
END DO
IFAIL=16

999   CONTINUE

IF (LFLAG) THEN
  WRITE(*,*)'In RDBITAB: NDEX = ',NDEX,' First and last BINDEX = '
  WRITE(*,*)BINDEX(1),BINDEX(NDEX)
  WRITE(*,*)'In RDBITAB:  MESTRUCT = ',MESTRUCT
END IF

RETURN
END SUBROUTINE RDBITAB
