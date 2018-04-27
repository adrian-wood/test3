SUBROUTINE BOYENC(ARRAY,DATIME,TTAAII,CCCC,OCOR,DRGFLG,NFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : BOYENC
!
! PURPOSE       : TO ENCODE BUOY MESSAGE INTO BUFR AND STORE
!
! CALLED BY     : BUOY MDBSTOR
!
! CALLS         : BOYIND, CCCODE, AIRSTO, DATIM, ENBUFR
!
! ARGUMENTS     : (ALL INPUT)
!                 ARRAY  - EXPANDED ARRAY OF ELEMENTS
!                 DATIME - TIME OF OB ARRAY
!                 TTAAII -
!                 CCCC   - COLLECTING CENTRE
!                 OCOR   - CORRECTED REPORT FLAG
!                 DRGFLG - DROGUE FLAG
!                 NFT    - FT NUMBER FOR MDB DATASET
!
!
! REVISION INFO :
!
! $Workfile: boyenc.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 14/01/2011 17:57:56$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         14/01/2011 17:57:56    Rosemary Lavery
!       updated comments (call list) on review
!  3    MetDB_Refresh 1.2         14/01/2011 11:30:31    Alison Weir
!       Correct character in Revision info
!  2    MetDB_Refresh 1.1         14/01/2011 11:21:03    Alison Weir     Ported
!        to f95 - MDBSTOR batch12
!  1    MetDB_Refresh 1.0         05/01/2011 17:04:51    Alison Weir
!       MDBSTOR batch 12 initial F77 version
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
USE airsto_mod
USE boyind_mod
USE cccode_mod
USE datim_mod
USE enbufr_mod

IMPLICIT NONE

! Subroutine arguments:

REAL,             INTENT(IN)    ::     ARRAY(0:600)  !A01
INTEGER,          INTENT(INOUT) ::     DATIME(5)     !A02
CHARACTER(LEN=6), INTENT(IN)    ::     TTAAII        !A03
CHARACTER(LEN=4), INTENT(IN)    ::     CCCC          !A04
LOGICAL,          INTENT(IN)    ::     OCOR          !A05
LOGICAL,          INTENT(IN)    ::     DRGFLG        !A06
INTEGER,          INTENT(IN)    ::     NFT           !A07

! Local declarations:

CHARACTER(LEN=10)   ::     NAMES
CHARACTER(LEN=23)   ::     ENTRY
CHARACTER(LEN=3000) ::     MESAGE
CHARACTER(LEN=9)    ::     IDENT

INTEGER             ::     NOW(8)
INTEGER             ::     TOR(5)
INTEGER             ::     ICCCC
INTEGER             ::     I
INTEGER             ::     BLKSIZ
INTEGER             ::     IDES
INTEGER             ::     NDES
INTEGER             ::     DESCR(300)
INTEGER             ::     NELEM
INTEGER             ::     NOBS
INTEGER             ::     LEN
INTEGER             ::     IVER=13       !TABLEB VERSION NUMBER

LOGICAL             ::     CMPRES=.FALSE.

!************************************************************
!
!     SET CLASS 3 DESCRIPTOR ACCORDING TO WHETHER DROGUE
!     IS PRESENT - DIFFERENT DESCRIPTOR LISTS
!
!************************************************************

IF (DRGFLG) THEN
  DESCR(1) = IDES(331201)  !WAS 331199
ELSE
  DESCR(1) = IDES(331200)  !WAS 331198
END IF

!************************************************************
!
!     SET UP PARAMETERS FOR ENBUFR INTERFACE
!     NDES    - NUMBER OF DESCRIPTORS
!     NELEM   - NUMBER OF ELEMENTS
!     NOBS    - NUMBER OF REPORTS PER BUFR MESSAGE
!     NAMES   - STRING (EMPTY FOR BUOY TYPE)
!     CMPRES  - DATA COMPRESSION FLAG
!     BLKSIZ  - BLOCKSIZE FOR MDB DATASET
!     TOR     - TIME OF RECEIPT ARRAY
!     MESAGE  - OUTPUT BUFR MESAGE
!     LEN     - LENGTH OF OUTPUT BUFR MESSAGE
!
!************************************************************

NDES = 1
NELEM = 600
NOBS = 1
NAMES = ' '
CMPRES = .FALSE.
BLKSIZ = 27998

CALL DATIM(NOW)
DO I = 0,4
  TOR(I+1) = NOW(8-I)
END DO

!************************************************************
!
!     ENCODE INTO BUFR MESSAGE
!
!************************************************************

CALL ENBUFR(DESCR,ARRAY,NDES,NELEM,NOBS,NAMES,TOR, &
            MESAGE,CMPRES,LEN,IVER)

!************************************************************
!
! Set collecting centre & data type in section 1 of BUFR message
! (displacements for messages without total length at start!)
!
!************************************************************

CALL CCCODE(287,ICCCC,CCCC)   ! CCCC code from table 001031

IF (ICCCC /= 65536) THEN      ! 2 BYTES OF 1'S - MISSING DATA
  MESAGE(9:9) = CHAR(ICCCC/256)
  MESAGE(10:10) = CHAR(MOD(ICCCC,256))
END IF

MESAGE(13:13) = CHAR(31)      ! DATA TYPE - OCEANOGRAPHIC

!************************************************************
!
!     COMPILE INDEX ENTRY
!
!************************************************************

CALL BOYIND(ARRAY,OCOR,DRGFLG,ENTRY)

!************************************************************
!
!     STORE MESSAGE
!
!************************************************************

IDENT=ENTRY(3:11)
IF (OCOR) THEN
  ENTRY(3:11)=TTAAII(3:6)//CHAR(1)//CCCC
ELSE
  ENTRY(3:11)=TTAAII(3:6)//CHAR(0)//CCCC
END IF
CALL AIRSTO(DATIME,ENTRY,MESAGE(1:LEN),NFT,BLKSIZ,IDENT,TOR)

RETURN
END SUBROUTINE BOYENC
