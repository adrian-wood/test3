      SUBROUTINE CHSDBL (CHASE_DATIM, REPORT_TYPE,
     &                   MISNG_RPRTS, MISRCNT, DAY_OF_WEEK)         !2.0

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : CHSDBL                                              
!                                                                      
! PURPOSE        : To create the daily messages for the MDB Chaser     
!                  output using the MHS system.                        
!                                                                      
! DESCRIPTION    : A message is output for each hour, with a list of   
!                  the station identifiers whose reports have not been 
!                  received.                                           
!                  If all reports have been received then the above    
!                  message will only contain date and day.             
!                                                                      
! DATA TYPE(S)   : SYNOP, NCM, SREW                                    
! HANDLED                                                             
!                                                                      
! CALLED BY      : CHSDAY                                              
!                                                                      
! CALLS          : UADATA   - Format message information               
!
! REVISION INFO  :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:43$
! $Source: /home/us0400/mdb/op/lib/source/RCS/chsdbl.F,v $
!
! CHANGE RECORD  :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:43    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:31  usmdb
! Removed unused dummy argument STNLIST. Added copyright
! and modified header - S.Cox
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
      INTEGER       LOOPCNT      ! The number of stations to be output.
      INTEGER       LOOPSTN      ! A count of the number of station
                                 ! identifiers, whose reports are
                                 ! missing, output.
      INTEGER       MISRCNT      ! The number of stations whose reports
                                 ! are missing.
      INTEGER       REPORT_TYPE  ! The data type identifier

      INTEGER       CHASE_DATIM(9)  ! Array of Chase epoch data.
      CHARACTER*10  DAY_OF_WEEK  ! The name of the day of the week
      CHARACTER*18  DDYYGG       ! The hour being chased.

      CHARACTER*(*) MISNG_RPRTS(*)! The list of missing stations.
      CHARACTER*68  BUFFER       ! The output data string.
      CHARACTER*1   BLANK        ! Used to output a blank text string.
      CHARACTER*132  HEAD

       HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/chsdbl.F,v $
     &'//'$Date: 30/01/2006 20:21:43$ $Revision: 1$'

! Initialise variables

      WRITE(DDYYGG,'(2I2.2,A4,A10)')
     &       CHASE_DATIM(4),CHASE_DATIM(5),'00Z ',DAY_OF_WEEK
      BLANK=' '
      BUFPOS=1
      LOOPCNT=1

!     If any reports are missing then add the appropriate
!      message to the bulletin for the data type being chased.

      IF (MISRCNT .GT. 0) THEN

          BUFFER=DDYYGG
          CALL UADATA(BUFFER,18)
          WRITE(14,'(1X,A)')BUFFER

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
          WRITE(14,'(1X,A)')BUFFER
            BUFPOS=1
            LOOPCNT=LOOPCNT+11
          ELSE
            BUFFER(1:)=BLANK
            DO LOOPSTN=LOOPCNT,MISRCNT
              BUFFER(BUFPOS:BUFPOS+5)=MISNG_RPRTS(LOOPSTN)(1:6)
              BUFPOS=BUFPOS+6
            ENDDO
            CALL UADATA(BUFFER,BUFPOS)
          WRITE(14,'(1X,A)')BUFFER                         ! test line
            LOOPCNT=MISRCNT+1
          ENDIF
        ENDDO

!     If all reports for a data type have been received, then output a
!       then output a message to confirm this.

      ELSE
        IF (REPORT_TYPE .EQ. 1) THEN

          BUFFER=DDYYGG // 'ALL SYNOP REPORTS RECEIVED '
          CALL UADATA(BUFFER,45) ! 38 changed from 38 to 45 !1.3
          WRITE(14,'(1X,A)')BUFFER                         ! test line

        ELSEIF (REPORT_TYPE .EQ. 2) THEN

          BUFFER=DDYYGG // 'ALL SREW REPORTS RECEIVED '
          CALL UADATA(BUFFER,44) ! 37 changed to 44 !1.3
          WRITE(14,'(1X,A)')BUFFER                         ! test line

        ELSEIF (REPORT_TYPE .EQ. 3) THEN

          BUFFER=DDYYGG // 'ALL NCM REPORTS RECEIVED '
          CALL UADATA(BUFFER,43) ! 36 changed to 43 !1.3
          WRITE(14,'(1X,A)')BUFFER                         ! test line
        ENDIF
      ENDIF
      BUFFER(1:)=BLANK
      CALL UADATA(BUFFER,1)
          WRITE(14,'(1X,A)')BUFFER                         ! test line

      RETURN
      END
