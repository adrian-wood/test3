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
! REVISION INFO :
!
! $Workfile: silent.f$ $Folder: UTILITY.SRCE$
! $Revision: 1$ $Date: 08/06/2007 15:35:13$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         08/06/2007 15:35:13    Brian Barwell   
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
      SUBROUTINE TELLOPS (TEXT)
      CHARACTER*(*) TEXT
      WRITE (6,'(T2,2A)') 'TELLOPS:  ', TEXT
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE REPLYT (REPLY)
      CHARACTER*4 REPLY
      REPLY = '    '        ! i.e. returns 'no reply'
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE REPLYTU (REPLY)
      CHARACTER*4 REPLY
      CHARACTER*2 Z0001/Z0001/, Z0100/Z0100/
      CHARACTER*81 C81
!                                Read record 2 of housekeeping data set
      READ (1,REC=2) C81
!                                      Process flags in bytes 80 and 81
      IF (C81(80:81).EQ.Z0100) THEN
         REPLY = 'U   '                    ! 'Urgent' stop
      ELSE IF (C81(80:81).EQ.Z0001) THEN
         REPLY = 'T   '                    ! 'Tidy' stop
      ELSE
         REPLY = '    '                    ! No termination
      END IF
!
      RETURN
      END
