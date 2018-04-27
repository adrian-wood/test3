      SUBROUTINE CHSBUL (CHASE_DATIM, REPORT_TYPE,
     &                    MISNG_RPRTS, MISRCNT    ,
     &                    MISNG_ELEMS, MISECNT    ,
     &                    STNLIST)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : CHSBUL                                              
!                                                                      
! PURPOSE        : To create the messages for the MDB Chaser output    
!                  using the MHS system                                
!                                                                      
! DESCRIPTION    : A message is output to notify that reports are      
!                  missing, followed by a list of the station          
!                  identifiers whose reports have bnot been received.  
!                  If all reports have been received then the above    
!                  message will be replaced with a confirmation that   
!                  all reports have been received.                     
!                  If any elements in a report are missing or suspect  
!                  then another message followed by a list of station  
!                  identifiers and an abbreviation for each element    
!                  missing or suspect will be output.                  
!                  Should no elements be flagged, confirmation of this 
!                  will be output.                                     
!                  Data types that do ot have tyheir report content    
!                  checked will not have an output message relating    
!                  to elements missing or suspect.                     
!                                                                      
! DATA TYPE(S)   : SYNOP, NCM, SREW                                    
! HANDLED                                                             
!                                                                      
! CALLED BY      : CHSMAIN                                             
!                                                                      
! CALLS          : UADATA   - Format message information               
!                                                                      
! REVISION INFO  :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:42$
! $Source: /home/us0400/mdb/op/lib/source/RCS/chsbul.F,v $
!
! CHANGE RECORD  :                                         
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:42    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:31  usmdb
! Correct HEAD, added copyright and modified header - S.Cox
!
! Revision 1.2  98/09/30  09:52:34  09:52:34  usmdb (Generic MDB account)
! Tidy up of order of variable definitions.
! v(G)=20, ev(G)= 1
! 
! Revision 1.1  98/09/08  16:11:23  16:11:23  usmdb (Generic MDB account
! Initial revision
!                                                                      
! 18/12/1997      Add details of MET.PROCLIB members called to header  
!                 documentation. John Norton                           
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

! Declare variables

      INTEGER       BUFPOS       ! The current position within the data
                                 ! output string.
      INTEGER       CHASE_DATIM(9)  ! Array of Chase epoch data.
      INTEGER       LOOPCNT      ! The number of stations to be output.
      INTEGER       LOOPSTN      ! A count of the number of station
                                 ! identifiers, whose reports are
                                 ! missing, output.
      INTEGER       MISECNT      ! The number of stations whose reports
                                 ! have missing or suspect elements.
      INTEGER       MISRCNT      ! The number of stations whose reports
                                 ! are missing.
      INTEGER       REPORT_TYPE  ! The data type identifier
      INTEGER       STNLIST      ! The total number of stations to be
                                 ! chased.

      CHARACTER*1   BLANK        ! Used to output a blank text string.
      CHARACTER*68  BUFFER       ! The output data string.

      CHARACTER*(*) MISNG_ELEMS(*)! The list of station identifiers and
                                 ! abbreviations of missing or suspect
                                 ! elements.
      CHARACTER*(*) MISNG_RPRTS(*)! The list of missing stations.
      CHARACTER*5   YYGG         ! The hour being chased.
      CHARACTER*132  HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/chsbul.F,v $
     &'//'$Date: 30/01/2006 20:21:42$ $Revision: 1$'                                   !2.0

! Initialise variables

      WRITE(YYGG,'(I2.2,A)') CHASE_DATIM(5),'00Z'
      BLANK=' '
      BUFPOS=1
      LOOPCNT=1

!     If any reports are missing then add the appropriate
!      message to the bulletin for the data type being chased.

      IF (MISRCNT .GT. 0) THEN

        IF (REPORT_TYPE.EQ. 1) THEN

          BUFFER='THE FOLLOWING SYNOP REPORTS FOR ' // YYGG
          CALL UADATA(BUFFER,39)
          WRITE(12,'(1X,A)')BUFFER                         ! test line

        ELSEIF (REPORT_TYPE .EQ. 2) THEN

          BUFFER='THE FOLLOWING SREW REPORTS FOR ' // YYGG
          CALL UADATA(BUFFER,38)
          WRITE(12,'(1X,A)')BUFFER                         ! test line

        ELSEIF (REPORT_TYPE .EQ. 3) THEN

          BUFFER='THE FOLLOWING NCM REPORTS FOR ' // YYGG
          CALL UADATA(BUFFER,37)
          WRITE(12,'(1X,A)')BUFFER                         ! test line
        ENDIF

!       Add the standard text requesting re-transmission.

        BUFFER='ARE MISSING FROM THE DATABANK. PLEASE SEND OR REPEAT.'
        CALL UADATA(BUFFER,53)
          WRITE(12,'(1X,A)')BUFFER                         ! test line
        BUFFER(1:)=BLANK
        CALL UADATA(BUFFER,1)
          WRITE(12,'(1X,A)')BUFFER                         ! test line

!       Output the list of station identifiers whose reports
!        are outstanding.

        DO WHILE (LOOPCNT .LE. MISRCNT )

          IF ((MISRCNT+1)-LOOPCNT .GE. 11) THEN

            BUFFER(1:)=BLANK
            DO LOOPSTN=LOOPCNT,LOOPCNT+10
              BUFFER(BUFPOS:BUFPOS+5)=MISNG_RPRTS(LOOPSTN)(1:6)
              BUFPOS=BUFPOS+6
            ENDDO
            CALL UADATA(BUFFER,66)
          WRITE(12,'(1X,A)')BUFFER                         ! test line
            BUFPOS=1
            LOOPCNT=LOOPCNT+11
          ELSE
            BUFFER(1:)=BLANK
            DO LOOPSTN=LOOPCNT,MISRCNT
              BUFFER(BUFPOS:BUFPOS+5)=MISNG_RPRTS(LOOPSTN)(1:6)
              BUFPOS=BUFPOS+6
            ENDDO
            CALL UADATA(BUFFER,BUFPOS)
          WRITE(12,'(1X,A)')BUFFER                         ! test line
            LOOPCNT=MISRCNT+1
          ENDIF
        ENDDO

!     If all reports for a data type have been received, then output a
!       then output a message to confirm this.

      ELSE
        IF (REPORT_TYPE .EQ. 1) THEN

          BUFFER='ALL SYNOP REPORTS RECEIVED FOR ' // YYGG
          CALL UADATA(BUFFER,38)
          WRITE(12,'(1X,A)')BUFFER                         ! test line

        ELSEIF (REPORT_TYPE .EQ. 2) THEN

          BUFFER='ALL SREW REPORTS RECEIVED FOR ' // YYGG
          CALL UADATA(BUFFER,37)
          WRITE(12,'(1X,A)')BUFFER                         ! test line

        ELSEIF (REPORT_TYPE .EQ. 3) THEN

          BUFFER='ALL NCM REPORTS RECEIVED FOR ' // YYGG
          CALL UADATA(BUFFER,36)
          WRITE(12,'(1X,A)')BUFFER                         ! test line
        ENDIF
      ENDIF
      BUFFER(1:)=BLANK
      CALL UADATA(BUFFER,1)
          WRITE(12,'(1X,A)')BUFFER                         ! test line

!     If any elements of a report are suspect or missing
!      then output the message to indicate this.

      IF (MISECNT .GT. 0) THEN

        IF (REPORT_TYPE .EQ. 1) THEN

          BUFFER='THE FOLLOWING SYNOP ELEMENTS ARE SUSPECT OR '//
     &           'MISSING FOR ' // YYGG
          CALL UADATA(BUFFER,63)
          WRITE(12,'(1X,A)')BUFFER                         ! test line

        ELSEIF (REPORT_TYPE .EQ. 3) THEN

          BUFFER='THE FOLLOWING NCM ELEMENTS ARE SUSPECT OR '//
     &           'MISSING FOR ' // YYGG
          CALL UADATA(BUFFER,61)
          WRITE(12,'(1X,A)')BUFFER                         ! test line
        ENDIF
        BUFFER(1:)=BLANK
        CALL UADATA(BUFFER,1)
          WRITE(12,'(1X,A)')BUFFER                         ! test line

!       Output the station identifier and element abbreviation.

        DO LOOPCNT=1,MISECNT

          BUFFER=MISNG_ELEMS(LOOPCNT)(1:66)
          CALL UADATA(BUFFER,66)
          WRITE(12,'(1X,A)')BUFFER                         ! test line
        ENDDO
        BUFFER(1:)=BLANK
        CALL UADATA(BUFFER,1)
          WRITE(12,'(1X,A)')BUFFER                         ! test line

!       If no elements are suspect or missing then output a message
!         to confirm this.
!         No message is output if all stations are missing.

      ELSEIF (MISRCNT .LT. STNLIST) THEN

        IF (REPORT_TYPE .EQ. 1) THEN

          BUFFER='NO SYNOP ELEMENTS ARE SUSPECT OR '//
     &           'MISSING FOR ' // YYGG
          CALL UADATA(BUFFER,53)
          WRITE(12,'(1X,A)')BUFFER                         ! test line

        ELSEIF (REPORT_TYPE .EQ. 3) THEN

          BUFFER='NO NCM ELEMENTS ARE SUSPECT OR '//
     &           'MISSING FOR ' // YYGG
          CALL UADATA(BUFFER,51)
          WRITE(12,'(1X,A)')BUFFER                         ! test line
        ENDIF
      ENDIF

!     Add an extra blank line after any suspect or missing elements
!       have output, except for SREW data which has no elements chased.

      IF (REPORT_TYPE .NE. 2) THEN

        BUFFER(1:)=BLANK
        CALL UADATA(BUFFER,1)
          WRITE(12,'(1X,A)')BUFFER                         ! test line
      ENDIF

      RETURN
      END

