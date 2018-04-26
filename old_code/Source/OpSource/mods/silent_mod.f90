!-----------------------------------------------------------------------
! MODULE        : SILENT
!
! PURPOSE       : 'Dummy' versions of MetDB routines which communicate
!                 with the operators.
!
! DESCRIPTION   : This module contains FORTRAN versions of the routines
!                 TELLOPS, REPLYT and REPLYTU which don't send messages
!                 to or receive replies from the operators. It is for
!                 use in test MetDB storage runs (e.g. integration test
!                 jobs) and should be included in any non-operational
!                 MetDB storage job.
!
!                  TELLOPS just writes a message to unit 6 instead of
!                          sending it to the operator.
!                  REPLYT  is a dummy routine which always returns 'no
!                          reply'.
!                  REPLYTU looks at bytes 80 & 81 of record 2 of the
!                          housekeeping data set returning a suitable
!                          termination flag if either is 1.
!
!                 The version of REPLYTU enables the user to terminate
!                 a test MetDB storage run using the program STOPJOBS.
!
! REVISION INFO:
!
! $Workfile: silent_mod.f90$ $Folder: mods$
! $Revision: 5$ $Date: 26/01/2011 13:15:26$
!
! CHANGE RECORD:
!
! $Log:
!  5    MetDB_Refresh 1.4         26/01/2011 13:15:26    Alison Weir     Module
!        statements added
!  4    MetDB_Refresh 1.3         20/01/2011 14:35:48    Richard Weedon
!       Updated
!  3    MetDB_Refresh 1.2         20/01/2011 13:19:55    Richard Weedon
!       Correction to revision info template
!  2    MetDB_Refresh 1.1         20/01/2011 13:18:34    Richard Weedon  add
!       revsion info template
!  1    MetDB_Refresh 1.0         20/01/2011 13:15:41    Richard Weedon  ported
!        under mdbstor batch 22. passes basic compilation test.
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

MODULE silent_mod
CONTAINS

SUBROUTINE TELLOPS (TEXT)
! Arguments
CHARACTER(LEN=*),INTENT(IN)   ::  TEXT
!
WRITE (6,'(T2,2A)') 'TELLOPS:  ', TEXT
RETURN
END SUBROUTINE TELLOPS
!-----------------------------------------------------------------------
SUBROUTINE REPLYT (REPLY)
! Arguments
CHARACTER(LEN=4),INTENT(OUT)   ::  REPLY
REPLY = '    '           ! i.e. returns 'no reply'
RETURN
END SUBROUTINE REPLYT
!-----------------------------------------------------------------------
SUBROUTINE REPLYTU (REPLY)
! Arguments
CHARACTER(LEN=4),INTENT(OUT)  ::  REPLY
! Local variables
CHARACTER(LEN=2)   ::  Z0001
CHARACTER(LEN=2)   ::  Z0100
CHARACTER(LEN=81)  ::  C81
!
! initialise variables
Z0001=CHAR(0)//CHAR(1)
Z0100=CHAR(1)//CHAR(0)
!                         Read record 2 of housekeeping data set
READ (1,REC=2) C81
!                         Process flags in bytes 80 and 81
IF (C81(80:81) == Z0100) THEN
   REPLY = 'U   '                    ! 'Urgent' stop
ELSE IF (C81(80:81) == Z0001) THEN
   REPLY = 'T   '                    ! 'Tidy' stop
ELSE
   REPLY = '    '                    ! No termination
END IF
!
RETURN
END SUBROUTINE REPLYTU

END MODULE silent_mod
