!-----------------------------------------------------------------------
! MODULE        : WAIT3  (modified version of subroutine REPLYTU)
!
! PURPOSE       : To provide a delay of 3 seconds between processing
!                 of MHS data sets.
!
! DESCRIPTION   : This is a copy of the version of REPLYTU in module
!                 SILENT but with a 3-second delay. It was written for
!                 integration testing to prevent the monitor job from
!                 finishing too early and shutting down other jobs
!                 prematurely. A version with a 5 second delay (WAIT5)
!                 is also available.
!
! REVISION INFO :
!
! $Workfile: wait3.f$ $Folder: UTILITY.SRCE$
! $Revision: 1$ $Date: 08/06/2007 15:40:17$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         08/06/2007 15:40:17    Brian Barwell   
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
      SUBROUTINE REPLYTU (REPLY)

      CHARACTER*4 REPLY
      CHARACTER*2 Z0001/Z0001/, Z0100/Z0100/
      CHARACTER*81 C81
!                                                    Wait for 3 seconds
      CALL SECSOUT (3)
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

      RETURN
      END
