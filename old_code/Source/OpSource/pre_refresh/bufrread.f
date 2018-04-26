      SUBROUTINE BUFRREAD(FNAME,BUFMES,L,RC)

!-----------------------------------------------------------------------
!
! PROGRAM       : BUFRREAD
!
! PURPOSE       : to return a bufr message read from a file. C I/O
!                 routines are called to open the file and read the
!                 data. 2 blocks of 250000 bytes are read from the file.
!                 Each BUFR message is assumed to be < 250000 bytes in
!                 size.
!
! CALLED BY     : BUFRdecode
!
! ARGUMENTS     : (1) BUFR file to open, read                      (I/P)
!                 (2) BUFR message                                 (O/P)
!                 (3) length of BUFR message                       (O/P)
!                 (4) return code                                  (O/P)
!                      0 More messages to read
!                     -1 All messages from this dataset returned
!                     >0 Error : 1 = file open failed
!                                2 = no data read from file
!                                3 = '7777' not found
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 25/09/2008 13:32:49$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bufrread.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         25/09/2008 13:32:49    Richard Weedon  LRECL
!        increased to 250000
!  1    Met_DB_Project 1.0         30/01/2006 20:21:29    Sheila Needham  
! $
! Revision 2.5 2008/09/25 13:30
! Increased Parameter LRECL to 250000 blocks.
! Richard Weedon
!
! Revision 2.4  2002/09/16 10:32:22  usmdb
! Set FILEOPEN=FALSE and close file before routine returns
! with RC>0 - S.Cox
!
! Revision 2.3  2002/06/11 11:08:05  usmdb
! Increased LRECL to 150000 - allows for much longer BUFR
! messages e.g. new sonde - S.Cox
!
! Revision 2.2  2002/04/09 11:33:49  usmdb
! After each call to ICHAR, the value returned by ICHAR is checked.
! If < 0, 256 is added - needed for Sun OS - S.Cox
!
! Revision 2.1  2002/02/14 14:36:22  usmdb
! Fix small error - S.Cox
!
! Revision 2.0  2001/03/07 10:25:44  usmdb
! Initial revision
!
! 14-02-2001: Based on MODELB - S.Cox
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!-----------------------------------------------------------------------
! Parameter statements
!-----------------------------------------------------------------------

      INTEGER    LRECL          !- Amount of data to read each time 
      PARAMETER (LRECL=250000)                                      !2.5

!-----------------------------------------------------------------------
! Declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

      INTEGER FLOPT      !- BUFR section 2 present indicator.
      INTEGER IPOS       !- Position of 'BUFR' in CS.
      INTEGER ILEN       !- Length of data read from file
      INTEGER I1,I2,I3   !- Results from ICHAR                      !2.2
      INTEGER RC         !- Return code
      INTEGER L          !- Length of BUFR message
      INTEGER N          !- Current position in BUFR message

!-----------------------------------------------------------------------
! Declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

      LOGICAL FILEOPEN   !- TRUE if a file is open.
      LOGICAL HEADSET    !- TRUE if RCS info to be initialised

!-----------------------------------------------------------------------
! Declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

      CHARACTER           BUFMES*(*)  !- BUFR message to return.
      CHARACTER*4         BUFR        !- "BUFR"
      CHARACTER*(LRECL*2) CS          !- String for two
                                      !- consecutive blocks
      CHARACTER*(*)       FNAME       !- Filename to open.
      CHARACTER*132       HEAD        !- Revision information
      CHARACTER*4         SEVENS      !- "7777"

!-----------------------------------------------------------------------
! SAVE all variables.
!-----------------------------------------------------------------------

      SAVE

!-----------------------------------------------------------------------
! Data statements - initialise variables once only.
!-----------------------------------------------------------------------

      DATA BUFR     /'BUFR'/
      DATA HEADSET  /.FALSE./
      DATA SEVENS   /'7777'/
      DATA FILEOPEN /.FALSE./

!-----------------------------------------------------------------------
! RCS information.
!-----------------------------------------------------------------------

      IF (.NOT.HEADSET) THEN
        HEAD='$RCSfile: bufrread.f,v $ ' //
     &       '$Revision: 2$ $Date: 25/09/2008 13:32:49$'
        HEADSET=.TRUE.
      ENDIF

!-----------------------------------------------------------------------
! Initialise variables.
!-----------------------------------------------------------------------

      ILEN = 0
      L    = 0
      RC   = 0

!-----------------------------------------------------------------------
! If a file is not open, open the file and read data from it. 
! Read 2 strings of length LRECL the first time, unless end of data.
! IRC>0 means error, IRC<0 end of data
!-----------------------------------------------------------------------

      IF (.NOT.FILEOPEN) THEN

        FILEOPEN = .TRUE.
        N        = 1
        CS(:)    = ' '
 
        CALL METDB_COPEN(1,FNAME,0,RC)
        IF (RC.NE.0) THEN
          RC=1
          CALL METDB_CCLOSE(1)
          FILEOPEN=.FALSE.
          RETURN
        ENDIF

!-----------------------------------------------------------------------
! read 1st LRECL block of data. If none read, set error flag and return. 
!-----------------------------------------------------------------------

        CALL METDB_CREAD(1,CS(1:LRECL),ILEN) !- read       
        IF (ILEN.LT.1) THEN
          RC=2
          CALL METDB_CCLOSE(1)
          FILEOPEN=.FALSE.
          RETURN
        ENDIF

!-----------------------------------------------------------------------
! read 2nd LRECL block of data if necessary.
!-----------------------------------------------------------------------

        IF (ILEN.EQ.LRECL) THEN
          CALL METDB_CREAD(1,CS(LRECL+1:2*LRECL),ILEN)
        ENDIF

      ENDIF !- FILEOPEN

!-----------------------------------------------------------------------
! Look for 'BUFR' (start of message). If not found, we have reached
! the end of the data. Set RC=-1, and close the file.
!-----------------------------------------------------------------------

      IPOS=INDEX(CS(N:),BUFR)

      IF (IPOS.LE.0) THEN    !- end of data if no 'BUFR' found.
        RC=-1
        CALL METDB_CCLOSE(1)
        FILEOPEN=.FALSE.
        RETURN
      ENDIF

!-----------------------------------------------------------------------
! determine BUFR message length. 1st check BUFR edition number. If
! 2 or more, the BUFR length is in section 0 of the message. If <2,
! the BUFR length needs to be detrmines by adding up section lengths.
!-----------------------------------------------------------------------

      N=N+IPOS-1

! section 0
      I1=ICHAR(CS(N+7:N+7))                                         !2.2
      IF (I1.LT.0) I1=I1+256                                        !2.2
      IF (I1.GE.2) THEN      !- BUFR edition 2 or greater.          !2.2
                             !- Get msg length from sect 0.
        I1=ICHAR(CS(N+4:N+4))                                       !2.2
        I2=ICHAR(CS(N+5:N+5))                                       !2.2
        I3=ICHAR(CS(N+6:N+6))                                       !2.2
        IF (I1.LT.0) I1=I1+256                                      !2.2
        IF (I2.LT.0) I2=I2+256                                      !2.2
        IF (I3.LT.0) I3=I3+256                                      !2.2
        L=(65536*I1)+(256*I2)+I3                                    !2.2
      ELSE !- BUFR edition 0 or 1. Add up section lengths.
        L=L+4
! section 1
        FLOPT=ICHAR(CS(N+L+7:N+L+7))      !- optional section ?     !2.2
        IF (FLOPT.LT.0) FLOPT=FLOPT+256                             !2.2
        I1=ICHAR(CS(N+L:N+L))                                       !2.2
        I2=ICHAR(CS(N+L+1:N+L+1))                                   !2.2
        I3=ICHAR(CS(N+L+2:N+L+2))                                   !2.2
        IF (I1.LT.0) I1=I1+256                                      !2.2
        IF (I2.LT.0) I2=I2+256                                      !2.2
        IF (I3.LT.0) I3=I3+256                                      !2.2
        L=L+(65536*I1)+(256*I2)+I3                                  !2.2
        
! section 2
        IF (FLOPT.GE.128) THEN                                            
          I1=ICHAR(CS(N+L:N+L))                                     !2.2
          I2=ICHAR(CS(N+L+1:N+L+1))                                 !2.2
          I3=ICHAR(CS(N+L+2:N+L+2))                                 !2.2
          IF (I1.LT.0) I1=I1+256                                    !2.2
          IF (I2.LT.0) I2=I2+256                                    !2.2
          IF (I3.LT.0) I3=I3+256                                    !2.2
          L=L+(65536*I1)+(256*I2)+I3                                !2.2
        ENDIF
                                                             
! section 3
        I1=ICHAR(CS(N+L:N+L))                                       !2.2
        I2=ICHAR(CS(N+L+1:N+L+1))                                   !2.2
        I3=ICHAR(CS(N+L+2:N+L+2))                                   !2.2
        IF (I1.LT.0) I1=I1+256                                      !2.2
        IF (I2.LT.0) I2=I2+256                                      !2.2
        IF (I3.LT.0) I3=I3+256                                      !2.2
        L=L+(65536*I1)+(256*I2)+I3                                  !2.2

! sections 4 & 5
        I1=ICHAR(CS(N+L:N+L))                                       !2.2
        I2=ICHAR(CS(N+L+1:N+L+1))                                   !2.2
        I3=ICHAR(CS(N+L+2:N+L+2))                                   !2.2
        IF (I1.LT.0) I1=I1+256                                      !2.2
        IF (I2.LT.0) I2=I2+256                                      !2.2
        IF (I3.LT.0) I3=I3+256                                      !2.2
        L=L+(65536*I1)+(256*I2)+I3+4                                !2.2
      ENDIF

!-----------------------------------------------------------------------
! Check that '7777' present at end of message.
!-----------------------------------------------------------------------

      IF (CS(N+L-4:N+L-1).NE.SEVENS) THEN  !- end of data if no '7777'
        RC=3
        CALL METDB_CCLOSE(1)
        FILEOPEN=.FALSE.
        RETURN
      ENDIF

!-----------------------------------------------------------------------
! We now have a BUFR message - but reset N for next before returning.
! If pointer is now to second LRECL string, move that string up and
! read another record into the second string. This should keep BUFR
! messages within the first string of length LRECL.
!-----------------------------------------------------------------------

      BUFMES=CS(N:N+L-1)                                            !2.1

      N=N+L
      IF (N.GT.LRECL) THEN
        CS(1:LRECL)=CS(LRECL+1:2*LRECL)
        CS(LRECL+1:2*LRECL)= ' '
        CALL METDB_CREAD(1,CS(LRECL+1:2*LRECL),ILEN)
        IF (ILEN.LT.1) CS(LRECL+1:2*LRECL)=' '
        N=N-LRECL
      ENDIF

      RETURN
      END
