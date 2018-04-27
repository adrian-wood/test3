      SUBROUTINE BUFRPATH(DIR_NAME,LEN_DIR_NAME)

!----------------------------------------------------------------------
!
! ROUTINE       : BUFRPATH in load module BUFR
!
! PURPOSE       : to read the bufr table directory path set in the
!               : environment variable BUFR_LIBRARY. The directory
!               : path is passed back to the calling program where the
!               : BUFR table is opened.
!
! CALLED BY     : TABLEB, TABLED, CODE, LOCALB, LOCALD
!
! CALLS         : METDB_GETENV
!
! ARGUMENTS     :
!
! DIR_NAME      : char*(*) (op)  : directory path name
! LEN_DIR_NAME  : integer  (op)  : length of LEN_DIR_NAME
!
! REVISION INFO :
!
! $Revision: 1$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bufrpath.F,v $
! $Date: 30/01/2006 20:21:27$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:27    Sheila Needham  
! $
! Revision 2.0  2001/03/07 10:19:09  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  2000/08/25 15:29:26  usmdb
! Call metdb_getenv rather than UM routine
! FORT_GET_ENV - S.Cox
!
! Revision 1.1  99/03/15  13:32:35  13:32:35  usmdb (Generic MDB account
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

      IMPLICIT NONE

      CHARACTER  ENV_NAME*12    !- environment variable
      CHARACTER  DIR_NAME*(*)   !- BUFR tables directory path
      INTEGER    LEN_DIR_NAME   !- length of dir_name
      INTEGER    RC             !- return code from METDB_GETENV
      CHARACTER  HEAD*132       !- RCS INFO

      SAVE

      ENV_NAME = 'BUFR_LIBRARY'
      RC       = 0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/bufrpath.F,v $
     &'//'$ $Date: 30/01/2006 20:21:27$ $Revision: 1$'

!-----------------------------------------------------------------------
! Call the MetDB C routine METDB_GETENV to read the BUFR tables 
! directory path from the environment variable 'BUFR_LIBRARY'. Input 
! to METDB_GETENV is ENV_NAME. Returned variables are DIR_NAME and RC
!-----------------------------------------------------------------------

      CALL METDB_GETENV(ENV_NAME(1:12),DIR_NAME,RC)

      IF (RC.EQ.-1) THEN
        WRITE(6,*)'BUFRPATH: ERROR: Environment variable ',
     &            'BUFR_LIBRARY not set'
      ENDIF

      IF (RC.NE.0) STOP

!-----------------------------------------------------------------------
! Find the true length of DIR_NAME by trimming off trailing blanks.
!-----------------------------------------------------------------------

      LEN_DIR_NAME = LEN(DIR_NAME)
      DO WHILE (DIR_NAME(LEN_DIR_NAME:LEN_DIR_NAME).EQ.' ')
        LEN_DIR_NAME = LEN_DIR_NAME - 1
      ENDDO

      RETURN
      END
