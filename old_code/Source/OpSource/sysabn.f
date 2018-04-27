       SUBROUTINE SYSABN(STOP_CODE)
       
!-----------------------------------------------------------------------
!
! ROUTINE       : SYSABN
!
! DESCRIPTION   : On IBM this is a system routine to stop execution
!                 of a program and issue an abend code of STOP_ABEND.
!                 This is the version for a non-IBM platform.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:07$
! $Source: /home/us0400/mdb/op/lib/source/RCS/sysabn.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:07    Sheila Needham  
! $
! Revision 2.0  2001/06/12 14:29:44  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  97/07/04  14:02:09  14:02:09  uspm (Pat McCormack)
! Initial revision
! 
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
       
       IMPLICIT NONE
       
       INTEGER STOP_CODE
       CHARACTER*132 HEAD
       
       HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/sysabn.F,v $
     &'//'$ $Date: 30/01/2006 20:25:07$ $Revision: 1$'
      
       IF (STOP_CODE.EQ.910) THEN
         STOP 910
       ELSEIF (STOP_CODE.EQ.920) THEN
         STOP 920
       ELSEIF (STOP_CODE.EQ.930) THEN
         STOP 930
       ELSE 
         STOP ' abended'
       ENDIF

       RETURN
       END
