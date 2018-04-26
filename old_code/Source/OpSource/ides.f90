FUNCTION IDES(FXXYYY)

!-----------------------------------------------------------------------
!
! ROUTINE       : IDES
!
! PURPOSE       : to convert a 6-fig readable descriptor to an integer
!
! ARGUMENTS     : (1) descriptor in 6-figure from fxxyyy
!
! REVISION INFO :
!
! $Workfile: ides.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 20/10/2010 09:16:31$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:58:02    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

INTEGER ::  F,X,Y,FXXYYY


F=FXXYYY/100000
X=FXXYYY/1000-100*F
Y=FXXYYY-(100*F+X)*1000
IDES=F*16384+X*256+Y

RETURN
END FUNCTION IDES
