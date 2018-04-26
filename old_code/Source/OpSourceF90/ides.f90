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
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ides.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/03/07 10:19:16  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  1997/06/19 13:40:30  uspm
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

INTEGER F,X,Y,FXXYYY
CHARACTER HEAD*132

HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/ides.F,v $&
&'//'$ $Date: 26/01/2010 10:18:13$ $Revision: 1$'

F=FXXYYY/100000
X=FXXYYY/1000-100*F
Y=FXXYYY-(100*F+X)*1000
IDES=F*16384+X*256+Y

RETURN
END FUNCTION IDES
