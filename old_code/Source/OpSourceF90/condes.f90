SUBROUTINE CONDES(IDESC,NDES,IDISP)
!-----------------------------------------------------------------------
!
! SUBROUTINE    : CONDES IN LOAD MODULE TFMTRT
!
! PURPOSE       : CONVERTS ELEMENTS SELECTED TO DISPLACEMENTS IN
!                 ARRAY.
!
! DESCRIPTION   :
!
! DATA TYPE(S)  : METARS, TAFS
! HANDLED
!
! CALLED BY     : TFMTRT
!
! CALLS         : NOTHING
!
! PARAMETERS    :
!
!      IDESC(NDES) INTEGER LIST OF ELEMENT NUMBERS SELECTED
!      NDES        INTEGER NUMBER OF ELEMENTS
!      IDISP(NDES) INTEGER LIST OF DISPLACEMENTS/INDICATORS FOR
!                          TRNSFR TO USE TO MOVE DATA TO USERS ARRAY
!
!Y2K  26.06.1997  CONDES IS YEAR 2000 COMPLIANT.
!
! REVISION INFO  :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/condes.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:58:35  usmdb
! Separated declaration & initialisation of IBIT18.
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  12:54:46  12:54:46  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K changes
!
! Revision 1.1  1997/02/17 11:49:27  uspm
! Initial revision
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

INTEGER IDESC(NDES),IDISP(NDES)
INTEGER IBIT18                                                !2.0

CHARACTER*132 HEAD

DATA IBIT18/131072/                                           !2.0

HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/condes.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

DO 199 J1=1,NDES
! CHARACTER ELEMENTS
IF(IDESC(J1).GT.IBIT18)THEN
  IDISP(J1)=IDESC(J1)
! INDEX INFO INDICATOR
ELSEIF(IDESC(J1).GT.65280)THEN
  IDISP(J1)=65280-IDESC(J1)
! CHARACTER REPORT TEXT INDICATOR
ELSEIF(IDESC(J1).EQ.65250)THEN
  IDISP(J1)=-99
! ALL OTHER VALUES TRANSFERRED DIRECTLY FROM EXPANSION
ELSE
  IDISP(J1)=IDESC(J1)
ENDIF
199   CONTINUE
RETURN
END SUBROUTINE CONDES
