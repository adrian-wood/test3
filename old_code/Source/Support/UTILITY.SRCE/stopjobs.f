      PROGRAM STOPJOBS

!-----------------------------------------------------------------------
! PROGRAM       : STOPJOBS
!
! PURPOSE       : To terminate a test MetDB storage run.
!
! DESCRIPTION   : STOPJOBS is used to terminate a test run of one or
!                 more MetDB storage jobs. It can be used to stop a
!                 continuously running test storage run or for
!                 premature termination of a run using 'MODE=1' (e.g.
!                 an integration test). See MetDB Technical Note 14,
!                 section 5.6 for details.
!
!                 In order for it to work, the storage jobs must
!                 include a monitor job and each job must also include
!                 the load module MCC3.DB.LOAD(SILENT) (so it will not
!                 work for operational jobs!).
!
!                 STOPJOBS should be run with either STOP or STOPNOW
!                 supplied on unit 5 to simulate operator replies 'T'
!                 and 'U' respectively.
!
! REVISION INFO :
!
! $Workfile: stopjobs.f$ $Folder: UTILITY.SRCE$
! $Revision: 2$ $Date: 11/06/2007 10:28:34$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         11/06/2007 10:28:34    Brian Barwell   Add
!       OPEN stetement for unit 1 (accidentally omitted in version 1).
!  1    Met_DB_Project 1.0         08/06/2007 15:37:11    Brian Barwell   
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

      IMPLICIT NONE

      CHARACTER*2 Z0001, Z0100, Z    ! 2-character flags
      CHARACTER*8 NAME               ! Input data
      CHARACTER*950 RECORD           ! Housekeeping data set record

      DATA Z0001/Z0001/, Z0100/Z0100/
      COMMON /COMREC/ RECORD         ! For dynamical allocation only

!                                 Read input data ('STOP' or 'STOPNOW')
      READ (5,'(A)') NAME

!-----------------------------------------------------------------------
!  If input is 'STOP', set flag for 'tidy' stop (i.e. set second
! character of Z to binary 1) and print a message.
!-----------------------------------------------------------------------

      IF (NAME.EQ.'STOP') THEN
         Z = Z0001             ! Flag for 'tidy' stop
         WRITE (6,'(/T3,A)')
     &            'STORAGE JOBS WILL TERMINATE WHEN NO DATA REMAINS'

!-----------------------------------------------------------------------
!  If input is 'STOPNOW', set flag for 'urgent' stop (i.e. set first
! character of Z to binary 1) and print a message.
!-----------------------------------------------------------------------

      ELSE IF (NAME.EQ.'STOPNOW') THEN
         Z = Z0100             ! Flag for 'urgent' stop
         WRITE (6,'(/T3,A)')
     &            'STORAGE JOBS WILL TERMINATE AS SOON AS POSSIBLE'

!-----------------------------------------------------------------------
!  If input is neither 'STOP' nor 'STOPNOW', just print a warning
! message and return.
!-----------------------------------------------------------------------

      ELSE
         WRITE (6,'(/T3,2A)') 'INVALID DATA INPUT - ', NAME
         RETURN
      END IF

!-----------------------------------------------------------------------
!  Set flags in housekeeping status record (record 2, bytes 80-81).
! These bytes are not used by operational jobs but for jobs which use
! the module MCC3.DB.LOAD(SILENT) the version of routine REPLYTU which
! it contains will use these flags to control running of storage jobs.
!  The update to record 2 is done as quickly as possible so as not to
! conflict with the monitor job.
!-----------------------------------------------------------------------
!                                            Open housekeeping data set
      OPEN (1, ACCESS='DIRECT', RECL=950)
!                                                       Update record 2
      READ (1,REC=2) RECORD
      RECORD(80:81) = Z
      WRITE (1,REC=2) RECORD
!                                           Close housekeeping data set
      CLOSE (1)
      STOP
      END
