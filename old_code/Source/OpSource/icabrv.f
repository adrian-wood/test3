      PROGRAM ICABRV

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! Program       : ICABRV
!
! Purpose       : To create the ICAO abbreviated List
!
! Description   : Required fields are read directly from the Browsable
!                 Station Master and re-written to MDB.ICAO.LIST in
!                 an appropriate format.
!
! Parameters    : NONE
!
! Called By     : NONE
!
! Calls To      : NONE
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:50$
! $Source: /home/us0400/mdb/op/lib/source/RCS/icabrv.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:50    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:32  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.3  2000/06/08  15:26:34  15:26:34  usmdb (Generic MetDB account)
! 19th June 2000, else statement to allow the update to pick up
!                 an ICAO id. when the previous stations id is 
!                 the same but closed. Stan Kellett
! 
! Revision 1.2  98/11/12  09:02:29  09:02:29  usmdb (Generic MDB account)
! 16/11/98 Enable recognision of '--' for unknown date/time field
!          (was '99') R Hirst
!
! Revision 1.1  98/09/16  15:39:11  15:39:11  usmdb (Generic MDB account)
! Initial revision
!
! 03/08/98 First Version - J.Lewthwaite
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

!----------------------------------------------------------------------
!Declare Logical
!----------------------------------------------------------------------

      LOGICAL EndOfData
      LOGICAL DontWrite
      LOGICAL PrevDontWrite

!----------------------------------------------------------------------
!Declare Charcater
!----------------------------------------------------------------------

      CHARACTER*692 LineOfData
      CHARACTER*692 TestLineOfData
      CHARACTER*4   ICAO
      CHARACTER*5   CharLat
      CHARACTER*6   CharLong
      CHARACTER*44  StationName
      CHARACTER*4   PrevICAO
      CHARACTER*8   CloseDate
      CHARACTER*132 HEAD
      CHARACTER*120 BLANK

!----------------------------------------------------------------------
!Declare Integer
!----------------------------------------------------------------------

      INTEGER LineRecNum
      INTEGER WMONum
      INTEGER RecordsCounted

!----------------------------------------------------------------------
!Initialize Variables
!----------------------------------------------------------------------

      EndOfData=.FALSE.
      TestLineOfData(:)=' '
      LineOfData(:)=' '
      LineRecNum=3
      WMONum=99999
      CharLat(:)=' '
      CharLong(:)=' '
      RecordsCounted=0
      StationName(:)=' '
      PrevICAO(:)=' '
      DontWrite=.FALSE.
      PrevDontWrite=.FALSE.
      CloseDate(:)=' '
      BLANK(:)=' '
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/icabrv.F,v $
     &'//'$Date: 30/01/2006 20:22:50$ $Revision: 1$'

!----------------------------------------------------------------------
!Open SDB.STNMAS.MAINB dataset (INPUT) and MDB.ICAO.LIST (Output)
!----------------------------------------------------------------------

      OPEN(70,ACCESS='DIRECT',RECL=692,FORM='FORMATTED')
      OPEN(71)

!----------------------------------------------------------------------
!Loop round input dataset until end of data records are found. An end
!of data record looks like this: 99999 ZZZZ 0000N 0000E.
!----------------------------------------------------------------------

      DO WHILE (.NOT. EndOfData .AND. LineRecNum .GT. 0)
        READ(70,'(A692)',REC=LineRecNum)LineOfData

!----------------------------------------------------------------------
!Keep the value of the previous ICAO to compare with the next record.
!If the ICAOs are found to be the same the open/close times are
!studied to see if one/both the stations are closed. If neither are
!closed than the first station details are kept and the second dropped.
!----------------------------------------------------------------------

        PrevDontWrite=DontWrite
        DontWrite=.FALSE.
        PrevICAO=ICAO

        IF ((LineOfData(8:8) .NE. ' ') .OR.
     &     (LineOfData(2:3) .EQ. '11')) THEN
          READ(LineOfData(8:11),'(A4)')ICAO
          READ(LineOfData(2:6),'(I5.5)')WMONum
          READ(LineOfData(13:17),'(A5)')CharLat
          READ(LineOfData(19:24),'(A6)')CharLong
          READ(LineOfData(40:82),'(A43)')StationName
          READ(LineOfData(140:147),'(A8)')CloseDate
          IF (LineOfData(2:3) .NE. '11') then
            IF (CloseDate .EQ. '--------') THEN                    !1.2
              DontWrite=.FALSE.
            ELSE
              DontWrite=.TRUE.
            ENDIF
            IF(PrevICAO .EQ. ICAO .AND. PrevDontWrite .AND.
     &        .NOT. DontWrite)THEN
              DontWrite=.FALSE.
            ELSEIF(PrevICAO .EQ. ICAO .AND. .NOT. PrevDontWrite) THEN
              DontWrite=.TRUE.
            ENDIF
          ELSE
            DontWrite=.FALSE.
          ENDIF
!----------------------------------------------------------------------
!Write the information to the output file
!----------------------------------------------------------------------

          IF (.NOT. DontWrite) THEN
            RecordsCounted=RecordsCounted+1
            WRITE(71,'(A4,1X,I5.5,1X,A5,1X,A6,1X,A43)')ICAO,WMONUM,
     &                            CharLat,CharLong,StationName
            WRITE(6,'(A4,1X,I5.5,1X,A5,1X,A6,1X,A43)')ICAO,WMONUM,
     &                            CharLat,CharLong,StationName
          ENDIF
        ELSE                                   !1.3
          ICAO = ' '                           !1.3
        ENDIF

!----------------------------------------------------------------------
!Check to see if its safe to increment the record pointer.
!----------------------------------------------------------------------

        READ(70,'(A692)',REC=LineRecNum+1)TestLineOfData
        IF (TestLineOfData(8:11) .EQ. 'ZZZZ') THEN
          EndOfData=.TRUE.
          WRITE(71,'(A120)')BLANK
        ELSE
          LineRecNum=LineRecNum+1
        ENDIF
      ENDDO
      WRITE(6,*)'No. Of ICAO records found/written = ',RecordsCounted
      STOP
      END
