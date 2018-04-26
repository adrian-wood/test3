SUBROUTINE RDBITAB(CINDEX,LOCALD,BINDEX,NDEX,MESTRUCT,BINMAX,&  !C
&LFLAG,IFAIL)

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
!Y2K  26.06.1997  RDBITAB is Year 2000 compliant.
!
! revision info  :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/rdbitab.F,v $
!
! change record  :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:59:06  usmdb
! Removed unused variable FIRST. Added copyright and
! modified header - S.Cox
!
! Revision 1.2  99/03/11  09:28:23  09:28:23  usmdb (Generic MDB account)
! revision info incorrect. checkout & checkin to make it
! version 1.2 - S.Cox 11/03/1999
!
! Revision 1.1  99/01/15  14:53:20  14:53:20  usmdb (Generic MDB account)
! Initial revision
!
! 21-07-97   !C  : Change variable name MAXELM to BINMAX for clarity
!                : reasons - S.Cox
!
! 27-03-97   !B  : BINDEX and MESTRUCT declarations changed from
!                : char*36 and char*63 respectively to char*(*). The
!                : size of MESTRUCT has changed from char*63 to
!                : char*500, so changes made - S.Cox
!
! 04-02-97   !A  : Initialise IFAIL to zero each call - S.Cox
!
! 26-07-96       : Written by S.M.Needham
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

IMPLICIT NONE

!-----------------------------------------------------------------------
! declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

INTEGER  BINMAX                                                 !C
INTEGER  I
INTEGER  IFAIL
INTEGER  IP
INTEGER  J
INTEGER  NDEX
INTEGER  NUMTAB     ! no. of index tables
INTEGER  TABLEN(99)

!-----------------------------------------------------------------------
! declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

LOGICAL  LFLAG

!-----------------------------------------------------------------------
! declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER*(*)    BINDEX(BINMAX)                               !C!B
CHARACTER*(*)    CINDEX(*)                                    !1.2
CHARACTER*132    HEAD
CHARACTER*12     LASTINDX
CHARACTER*6      LOCALD
CHARACTER*(*)    MESTRUCT                                       !B
CHARACTER*6      TABLID(99)

!-----------------------------------------------------------------------
! data statements
!-----------------------------------------------------------------------

DATA     LASTINDX/'            '/

!-----------------------------------------------------------------------
! SAVE statement to ensure all variables are still set on next call
!-----------------------------------------------------------------------

SAVE

HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/rdbitab.F,v $&
&'//'$ $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

IFAIL=0                                                         !A

IF (LASTINDX.NE.CINDEX(1)(1:12)) THEN                         !1.2

!----------------------------------------------------------------------
! An index table consists of the identifier, number of entries, 36 byte
! entries and a 500 byte message structure.
!
! First time through set up a list of table identifiers (TABLID)
! and number of index entries (TABLEN)                              !1.2
!----------------------------------------------------------------------

  IF (LFLAG) WRITE(*,*)'In RDBITAB : New index = ',&          !1.2
            &CINDEX(1)(1:12)                                  !1.2

  READ(CINDEX(1)(1:2),'(I2.2)')NUMTAB                         !1.2

  DO I=1,NUMTAB                                               !1.2
    TABLID(I)=CINDEX(I)(3:8)                                  !1.2
    READ(CINDEX(I)(9:12),'(I4)')TABLEN(I)                     !1.2
  END DO                                                       !1.2

  LASTINDX=CINDEX(1)(1:12)                                    !1.2

ENDIF

!----------------------------------------------------------------------
! Now find the table for the current LOCALD sequence
!----------------------------------------------------------------------

DO I=1,NUMTAB

  IF (LOCALD.EQ.TABLID(I)) THEN

!----------------------------------------------------------------------
! set up BINDEX
!----------------------------------------------------------------------

    NDEX=TABLEN(I)
    IF (NDEX.GT.BINMAX) THEN                                 !C
      IFAIL=8
      WRITE(*,*)'MDB WARNING: In RDBITAB: ELEMENT INDEX TOO ',&
     &'LARGE FOR SEQUENCE ',LOCALD,' - TRUNCATED AT ',BINMAX
      NDEX=BINMAX                                               !C
    ENDIF

    IP=13                                                     !1.2
    DO J=1,NDEX
      BINDEX(J)=CINDEX(I)(IP:IP+35)                           !1.2
      IP=IP+36
    END DO
    MESTRUCT=CINDEX(I)(IP:IP+499)                           !1.2!B
    GOTO 999
  ENDIF
END DO
IFAIL=16

999    CONTINUE

IF (LFLAG) THEN
  WRITE(*,*)'In RDBITAB: NDEX = ',NDEX,' First and last BINDEX = '
  WRITE(*,*)BINDEX(1),BINDEX(NDEX)
  WRITE(*,*)'In RDBITAB:  MESTRUCT = ',MESTRUCT
ENDIF

RETURN
END SUBROUTINE RDBITAB
