      PROGRAM BUFRdecode

!-----------------------------------------------------------------------
!
! ROUTINE       : BUFRdecode
!
! PURPOSE       : Example program to decode a BUFR message.
!
!               : This program is provided only as an example of how to
!               : read a BUFR message, BUFR decode it and output the
!               : decoded values to stdout.
!
!               : This version uses the BUFR routines supplied   !ST 2
!               : with the SIGWX package, and has a revised       !ST 2
!               : call to DEBUFR.                                 !ST 2
!
!               : It may be necessary to change the parameters
!               : MXDES and MXVAL to suit your own needs.
!
!               : MXDES is the maximum number of expanded descriptors
!               : per observation.
!
!               : MXVAL is the maximum number of observations per
!               : BUFR message.
! 
! REVISION INFO :
!
! $Revision: 2$
! $Date: 29/09/2008 11:06:37$
! $Source: /home/us0400/mdb/op/lib/source/RCS/BUFRdecode_sw.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         29/09/2008 11:06:37    Richard Weedon  MESS
!       increased to 250000
!  1    Met_DB_Project 1.0         06/07/2006 12:57:30    Alison Weir     
! $
!
! Revision 2.5  2008/09/29 11:05     usmdb
! Increased size of MESS to 250000 - R.Weedon
!
! Revision 2.4  2002/06/17 11:14:12  usmdb
! Code changed to handle replication descriptors better. A replications
! descriptor is in the form 1NNNNN where NNNNN is the number of
! replications (14 bit number). S.Cox
!
! Revision 2.3  2002/06/11 15:19:53  usmdb
! Increased size of MESS to 150000 - S.Cox
!
! Revision 2.2  2002/04/09 11:33:30  usmdb
! Changed MXDES to 7500 to keep memory usage to below 20MW
! on T3E - S.Cox
!
! Revision 2.1  2001/12/04 12:54:24  usmdb
! Increased size of DESCR array & NAMES string - S.Cox
!
! Revision 2.0  2001/03/07 10:19:08  usmdb
! Added Copyright and modified header - S.Cox
!
! Revision 1.6  2000/08/25 15:27:31  usmdb
! initialise NMES - S.Cox
!
! Revision 1.5  99/05/11  14:13:47  14:13:47  usmdb (Generic MDB account
! Now calls MODELB to read BUFR messages. Uses C IO - S.Cox
! 
! Revision 1.3  99/03/03  11:59:38  11:59:38  usmdb (Generic MDB account
! change to V1.2 not correct. Corrected error.
! 
! Revision 1.2  99/02/18  15:22:44  15:22:44  usmdb (Generic MDB account
! Add loop over number of BUFR messages in a record
! 
! Revision 1.1  1998/10/08 08:49:56  usmdb
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2006 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare variables
!-----------------------------------------------------------------------

      INTEGER    MXDES         !- Num of descriptors (I/O to DEBUFR sub)
      INTEGER    MXVAL         !- Num of obs (I/O to DEBUFR sub)
            
      PARAMETER  (MXDES=7500)  !- Set max num of elements per ob.   !2.2
      PARAMETER  (MXVAL=500)   !- Set max num of obs.

      INTEGER    I,J           !- Used in FOR loops.                
      INTEGER    IRC           !- return code from READBUFR
      INTEGER    L             !- Length of BUFR message
      INTEGER    LEN_FNAME     !- Length of FNAME
      INTEGER    F,X,Y         !- O/P from DESFXY subroutine.
      INTEGER    OCOUNT,DCOUNT !- obs and desc counters.
      INTEGER    NDESCR        !- Num of descriptors.
      INTEGER    NMES          !- Num of BUFR messages per record.
      INTEGER    NOBS          !- Num of values.
      INTEGER	 DESCR(MXDES)  !- Descriptors array.
      INTEGER    USERDESC(MXDES)     !- User descriptors array (descs)

      REAL       VALUES(MXDES*MXVAL) !- Values array.
      REAL       USERVALS(MXVAL,MXDES) !- User values array (obs,descs)

      CHARACTER*10000  NAMES  !- Length for any char strings.
      CHARACTER*400    FNAME  !- File to BUFR decode.
      CHARACTER*250000 MESS   !- BUFR message length (same as RECL) !2.3 
      CHARACTER*132    HEAD   !- RCS information
      CHARACTER CHARSECT2*240   !Section 2 data                   !ST 2

      LOGICAL   SECT2     !TRUE if section 2 is required          !ST 2

      HEAD='$RCSfile: BUFRdecode_sw.f,v $ ' //
     &     '$Revision: 2$ $Date: 29/09/2008 11:06:37$'
           
      NMES = 0                                                      !1.6
      IRC  = 0                                                      !1.7

!-----------------------------------------------------------------------
! Read environment variable BUFRDEC_FILE to get name of file to BUFR
! decode. Find the true length of BUFRDEC_FILE by trimming off trailing
! blanks.
!-----------------------------------------------------------------------

      CALL METDB_GETENV('BUFRDEC_FILE',FNAME,IRC)                   !1.7

      IF (IRC.NE.0) THEN                                            !1.7
        WRITE(6,*)'BUFRdecode: ERROR: BUFRDEC_FILE environment ',   !1.7
     &            'not set'                                         !1.7
        STOP                                                        !1.7
      ENDIF                                                         !1.7

      LEN_FNAME = LEN(FNAME)                                        !1.7
      DO WHILE (FNAME(LEN_FNAME:LEN_FNAME).EQ.' ')                  !1.7
        LEN_FNAME = LEN_FNAME - 1                                   !1.7
      ENDDO                                                         !1.7

!-----------------------------------------------------------------------
! Call READBUFR to read a BUFR message from the file. Continue while
! IRC=0 (more data to read). If IRC=-1, all data has been read. If
! IRC>0, there was an error.
!-----------------------------------------------------------------------

      DO WHILE (IRC.EQ.0)                                           !1.7

        CALL BUFRREAD(FNAME(1:LEN_FNAME),MESS,L,IRC)                !1.7

        IF (IRC.EQ.0) THEN                                          !1.7
          NMES = NMES + 1

!-----------------------------------------------------------------------
! Decode report. Argument .TRUE. to print decoded message.
!-----------------------------------------------------------------------
 
          WRITE(6,'(/1X,''BUFR message number = '',I8/)')NMES      

          NDESCR=MXDES             !- set size of DESCR array.
          NOBS=MXDES*MXVAL         !- set size of VALUES.

          SECT2= .FALSE.      !- extract SWM section 2 data       !ST 2
          CHARSECT2=' '      !- initialise                        !ST 2

          CALL DEBUFR(DESCR,VALUES,NAMES,NDESCR,NOBS,
     &                MESS(1:L),.TRUE.,
     &                SECT2,CHARSECT2)                            !ST 2

          WRITE(6,'(/1X,''Obs in message      = '',I8 )')NOBS
          WRITE(6,'( 1X,''Descriptors per ob  = '',I8/)')NDESCR 

          IF (SECT2) THEN                                         !ST 2
            WRITE(6,'( 1X,''LOCAL SECTION 2 = '')')               !ST 2
            WRITE(6,'( 1X,4(/A60))')                              !ST 2
     &                     CHARSECT2(1:60),CHARSECT2(61:120),     !ST 2
     &                     CHARSECT2(121:180),CHARSECT2(181:240)  !ST 2
          ENDIF                                                   !ST 2

          DCOUNT=0   !- user count of element descriptors only
          OCOUNT=0   !- user count of observations
            
!-----------------------------------------------------------------------
! Loop through the number of descriptors returned by DEBUFR (these
! include element (F=0), replication (F=1), operator (F=2) and
! sequence (F=3) descriptors. Call DESFXY to convert 16 bit descriptor 
! to F,X,Y parts. For the element descriptors only, store the descriptor
! in readable form in the user descriptor array. Loop over the obs and 
! store these in the user values array. These will correspond directly 
! to the user descriptors array.
!
! Note for replication, the descriptor returned will be in the form
! FNNNNN where F=1 and NNNNN is the number of replicated levels
! (stored as 16 bits.) 
!-----------------------------------------------------------------------

          IF (NDESCR.GT.0 .AND. NOBS.GT.0) THEN
            DO J = 1,NDESCR
              CALL DESFXY(DESCR(J),F,X,Y)
              IF (F.EQ.1) THEN               !- replication.
                X=0                          !- XX set to zero for repl.
                Y=MOD(DESCR(J),16384)        !- XXYYY
              ENDIF
              USERDESC(J)=100000*F+1000*X+Y
              DO I = 1,NOBS
                OCOUNT=OCOUNT+1
                USERVALS(I,J)=VALUES(OCOUNT)
              ENDDO
            ENDDO

            DO I = 1,NOBS
              WRITE(6,'(/1X,''Observation number : '',I4/)')I
              DCOUNT=0
              DO J = 1,NDESCR
                IF ((USERDESC(J)/100000).EQ.0) THEN
                  DCOUNT=DCOUNT+1
                  WRITE(6,'(1X,I4.4,2X,I6.6,2X,F12.3)')J,
     &            USERDESC(J),USERVALS(I,DCOUNT)
                ELSE
                  WRITE(6,'(1X,I4.4,2X,I6.6)')J,USERDESC(J)
                ENDIF   
              ENDDO
            ENDDO
          ENDIF

        ELSEIF (IRC.EQ.1) THEN
          WRITE(6,*)'BUFRdecode: ERROR: file open failed'           !1.7
        ELSEIF (IRC.EQ.2) THEN                                      !1.7
          WRITE(6,*)'BUFRdecode: ERROR: file read failed'           !1.7
        ELSEIF (IRC.EQ.3) THEN                                      !1.7
          WRITE(6,*)'BUFRdecode: ERROR: "7777" not found in file'   !1.7
        ELSE
          CONTINUE
        ENDIF

      ENDDO  !- while (irc)                                         !1.7
      
      STOP
      END
