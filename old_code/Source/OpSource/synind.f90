SUBROUTINE SYNIND(MESSAG,MESLEN,OCOR,CORNUM,TTAAII,MIMJ,  &
                  LATLON,CCCC,DATIME,NFT,BLKSIZ,SYNTAX,NELM,ID)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNIND
!
! PURPOSE       : TO PASS A SYNOP MESSAGE TO THE SYNREP FOR STORAGE,
!                 STARTING AN INDEX ENTRY (AFTER FURTHER CHECKS
!                 FOR COR, AMD & NIL).
!
! CALLED BY     : SYNOB
!
! CALLS         : SYNSTO, MOBSTO, SHPSTO, INDLALO
!
! ARGUMENTS     : (1) MESSAG   REPORT+BUFR MESSAGE
!                 (2) MESLEN   MESSAGE LENGTH
!                 (3) OCOR     LOGICAL SET IF REPORT CORRECTED
!                 (4) CORNUM   NUMBER OF CORRECTION
!                 (5) TTAAII   TT TYPE OF BULLETIN (SA IS METAR)
!                 (6) MIMJ     AAXX FOR LAND, BBXX FOR SHIPS
!                 (7) LATLON   LATITUDE/LONGITUDE INFO
!                 (8) CCCC     ORIGINATING CENTRE FOR BULLETIN
!                 (9) DATIME   BULLETIN DATE/TIME
!                (10) NFT      FT NUMBER FOR STORAGE OF THIS DATA
!                (11) BLKSIZ   BLOCK SIZE OF DATA SET FOR THIS DATA
!                (12) SYNTAX   TRUE IF SYNTAX FLAG HAS BEEN RAISED
!                (13) NELM     ELEMENT COUNT TO GO IN INDEX
!                (14) ID       IDENTIFIER TO GO IN INDEX ENTRY
!
! REVISION INFO :
!
!
! $Workfile: synind.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 10/01/2011 15:08:49$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         10/01/2011 15:08:49    Rosemary Lavery
!       updated
!  3    MetDB_Refresh 1.2         10/01/2011 14:51:06    Rosemary Lavery
!       corrections after review
!  2    MetDB_Refresh 1.1         07/01/2011 13:16:20    Rosemary Lavery
!       INTENTs modified based on modules called  
!  1    MetDB_Refresh 1.0         04/01/2011 16:36:47    Rosemary Lavery
!       Initial Import
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

USE INDLALO_mod
USE MOBSTO_mod
USE SHPSTO_mod
USE SYNSTO_mod

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)     :: MESSAG
INTEGER, INTENT(IN)               :: MESLEN
LOGICAL, INTENT(IN)               :: OCOR
CHARACTER (LEN=2), INTENT(IN)     :: CORNUM
CHARACTER (LEN=6), INTENT(IN)     :: TTAAII
CHARACTER (LEN=4), INTENT(IN)     :: MIMJ
REAL, INTENT(IN)                  :: LATLON(4)
CHARACTER (LEN=4), INTENT(IN)     :: CCCC
INTEGER, INTENT(INOUT)            :: DATIME(5)
INTEGER, INTENT(IN)               :: NFT
INTEGER, INTENT(IN)               :: BLKSIZ
LOGICAL, INTENT(IN)               :: SYNTAX
INTEGER, INTENT(IN)               :: NELM
CHARACTER (LEN=*),INTENT(INOUT)   :: ID

! Local Variables

INTEGER  :: ICOR
INTEGER  :: NCOR

CHARACTER (LEN=23)  :: ENTRY

! ---------------------------------------------------------------------

! Set position in index entry

CALL INDLALO(ENTRY,LATLON(1),LATLON(2))

! CHECK IF COR

NCOR=0
IF (.NOT.OCOR) ICOR=INDEX(MESSAG(5:MESLEN),' COR ')
IF (CORNUM > '01') THEN
  NCOR=ICHAR(CORNUM(2:2))-ICHAR('0')
ELSE IF (OCOR .OR. ICOR > 0) THEN
  NCOR=1
END IF
!**********************************************************************
!                                                                     *
! PASS REPORT AND ASSOCIATED INFORMATION ON FOR STORING IN DATABASE,  *
! MAKING CHARACTER STRING OF TTAAII, COR NUMBER & CCCC TO GO IN INDEX.*
!                                                                     *
!**********************************************************************

! SET BULLETIN DETAILS, ELEMENT COUNT & SYNTAX FLAG IF NECESSARY

ENTRY(3:11)=TTAAII(3:6)//CHAR(NCOR)//CCCC
ENTRY(12:12)=CHAR(NELM)                     ! ELEMENT COUNT

IF (SYNTAX) THEN
  ENTRY(17:17)=CHAR(64)                     ! FLAG IF SYNTAX ERROR
ELSE
  ENTRY(17:17)=CHAR(0)                      ! CLEAR BYTE IF NOT
END IF

! FINALLY CALL SYNSTO, MOBSTO OR SHPSTO FOR STORAGE

IF (MIMJ == 'AAXX' .OR. MIMJ == 'OLDS') THEN
  CALL SYNSTO(DATIME,ENTRY,MESSAG(1:MESLEN),NFT,BLKSIZ,ID)
ELSE IF (MIMJ == 'BBXX') THEN
  CALL SHPSTO(DATIME,ENTRY,MESSAG(1:MESLEN),NFT,BLKSIZ,ID)
ELSE IF (MIMJ == 'OOXX') THEN
  CALL MOBSTO(DATIME,ENTRY,MESSAG(1:MESLEN),NFT,BLKSIZ,ID)
END IF

RETURN
END SUBROUTINE SYNIND
