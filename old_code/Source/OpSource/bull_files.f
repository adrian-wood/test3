      SUBROUTINE BULL_FILES(BULL,FOLDER,FILE,LFOLDER,LFILE,ISTAT)

!-----------------------------------------------------------------------
!
! PROGRAM       : BULL_FILES
!
! PURPOSE       : to return bulletin folder and filenames from a
!               : user-supplied string containing bulletin filenames.
!               : This routine returns the folder name+length and
!               : the filename+length.
!
! CALLED BY     : Many.
!
! ARGUMENTS     : (1) CHAR*(*) string containing list of bulletin
!                     filenames separated by spaces or line feeds  (I/P)
!                 (2) CHAR*(*) folder name                         (O/P)
!                 (3) CHAR*(*) file name                           (O/P)
!                 (4) INT      length of FOLDER                    (O/P)
!                 (5) INT      length of FILE                      (O/P)
!                 (6) ISTAT    return code                        (IO/P)
!                              Input:  0 New BULL for BULL_FILES
!                                      4 Subsequent calls to BULL_FILES
!                              Output: 0 All filenames returned
!                                      4 More filenames to come
!                                      16 Error
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:32$
! $Source: /home/us0400/mdb/op/apps/store_data/RCS/bull_files.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:32    Sheila Needham  
! $
! Revision 1.3  2003/05/08 14:35:43  usmdb
! Correct tabbing - S.Cox
!
! Revision 1.2  2002/11/26 14:12:03  usmdb
! Many improvements. No longer need to put in full path - S.Cox
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!----------------------------------------------------------------------
! declare variables
!----------------------------------------------------------------------

      INTEGER       BULEND            ! end of BULL
      INTEGER       FEND              ! end of full pathname
      INTEGER       FLEN              ! length of full pathname
      INTEGER       FSTART            ! start of full pathname
      INTEGER       I                 ! general loop variable
      INTEGER       IPOS              ! position of LF in string
      INTEGER       ISTAT             ! Return code
      INTEGER       LFILE             ! length of FILE
      INTEGER       LFOLDER           ! length of FOLDER
      INTEGER       SLASH             ! position of '/' in string

      CHARACTER*(*) BULL              ! list of bulletin filenames
      CHARACTER*(*) FILE              ! filename extracted from BULL
      CHARACTER*(*) FOLDER            ! folder extracted from BULL
      CHARACTER*1   LF                ! Line feed character.

!----------------------------------------------------------------------
! save all variables
!----------------------------------------------------------------------

      SAVE

!----------------------------------------------------------------------
! illegal ISTAT as input. Return.
!----------------------------------------------------------------------

      IF (ISTAT.NE.0 .AND. ISTAT.NE.4) THEN
        WRITE(6,*)'BULL_FILES: Invalid ISTAT. ISTAT = ',ISTAT
        ISTAT=16
        RETURN
      ENDIF

!----------------------------------------------------------------------
! initialise variables.
!----------------------------------------------------------------------

      LF      = CHAR(10)
      LFILE   = 0
      LFOLDER = 0

!----------------------------------------------------------------------
! find end of list (look for 2 spaces in BULL string).
!----------------------------------------------------------------------

      IF (ISTAT.EQ.0) THEN
        BULEND = INDEX(BULL,'  ')
        IF (BULEND.EQ.0) BULEND = LEN(BULL)
        FSTART=1
      ENDIF
        
!----------------------------------------------------------------------
! step through BULL returning folder and file
!----------------------------------------------------------------------
 
      IPOS=INDEX(BULL(FSTART:),LF)-1
      IF (IPOS.LE.0) IPOS=INDEX(BULL(FSTART:),' ')-1
      FEND = FSTART + IPOS - 1
      FLEN = FEND - FSTART + 1

!-----------------------------------------------------------------------
! Find last '/' in BULL - in order to extract the folder and file names
!-----------------------------------------------------------------------

      SLASH=0
      I=FEND

      DO WHILE (SLASH.EQ.0 .AND. I.GT.FSTART)
        IF (BULL(I:I).EQ.'/') SLASH=I
        I=I-1
      ENDDO

      IF (SLASH.GT.0) THEN
        FOLDER  = BULL(FSTART:SLASH-1)
        LFOLDER = (SLASH-1) - FSTART + 1
        FILE    = BULL(SLASH+1:FEND)
        LFILE   = FEND - (SLASH+1) + 1
      ELSE
        FOLDER  = '.'
        LFOLDER = 1
        FILE    = BULL(FSTART:FEND)
        LFILE   = FEND - FSTART + 1
      ENDIF

      FSTART = FSTART + FLEN + 1     

!-----------------------------------------------------------------------
! ISTAT=0 = all filenames returned. ISTAT=4 = more to come.
!-----------------------------------------------------------------------

      IF (FSTART.GE.BULEND) THEN
        ISTAT=0
      ELSE
        ISTAT=4
      ENDIF

      RETURN
      END
