      PROGRAM BOGUS

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM       : BOGUS
!
! PURPOSE       : To store BOGUS data
!
! DESCRIPTION   : BOGUS data is stored by a separate job (QTXXROIB).
!                 The input consists of headers (BOGUS or not) and
!                 records.  Store a BUFR message for each BOGUS record.
!
! TESTING       : The input data set is deleted at the end of the job,
!                 but the first step of the job prints it, so the data
!                 can be reused for tests by extracting to a data set
!                 (line command E) in EJES.
!
! DATA TYPE(S)  : BOGUS
!
! CALLED BY     : nothing, it's a main program
!
! CALLS         : BOGEXP to decode
!                 BOGIND to encode, index & store
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:02$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bogus.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:02    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:31  usmdb
! Removed unused variables, added copyright and modified
! header - S.Cox
!
! Revision 1.6  2000/04/07  09:11:48  09:11:48  usmdb (Generic MetDB account)
! 17 April 2000     C Long
! 1.6  Drastic tidy up to remove redundant & misleading code.
!      BOGHED renamed BOGUS
! 
! Revision 1.5  1998/04/20 06:48:36  usmdb
! Extra Arguments to BOGRPT and BOGSTO                                !D
!
! Revision 1.4  97/09/25  14:01:01  14:01:01  usjl (Jon Lewthwaite)
! Software re-written to eliminate reported bugs and improve
! performance.
!
! Revision 1.3  1997/08/07 14:44:18  uspm
! Amend for Year 2000 - use common routine to determine
! century - Jim Arnott                                                !C
!
! Revision 1.2  1997/07/31 09:11:55  uspm
! First revision for  1
!
! Revision 1.1  1997/07/04 10:58:27  uspm
! Initial revision
!
! 14/04/97 INCREASE SIZE OF VALUES ARRAY TO COPE WITH ADDITION OF
!          MINUTES FROM 12 TO 13                                      !B
!
! 03/03/97 CORRECT SPELLING                                           !A
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

!declare character
      CHARACTER HEADER*80                   !Header of bogus data
      CHARACTER TYPE*3                      !Type of bogus report
      CHARACTER HEAD*132                    !Revision header
      CHARACTER IDENT_PRESSURE*4            !Pressure value for IDENT!D

!declare integer
      INTEGER NOW(8)                        !System time
      INTEGER TOR(5)                        !T.O.R. or report
      INTEGER YMD(3)                        !Year, Month, Day
      INTEGER I                             !Used in loops          !2.0
      INTEGER IFT                           !Input dataset unit no.
      INTEGER OFT                           !Output dataset unit number
      INTEGER IOS                           !Iostat check from read hed
      INTEGER REP_COUNT                     !Count of reports
      INTEGER CENTURY                                               !C

!declare real
      REAL VALUES(13)                       !Decoded values array    !B

!declare logical
      LOGICAL INPUT_BOGUS                   !Latest header is bogus

!save values
      SAVE

!initialize variables
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/bogus.F,v $
     &'//'$ $Date: 30/01/2006 20:21:02$ $Revision: 1$'

      IOS=0
      REP_COUNT=0
      INPUT_BOGUS=.FALSE.
      IFT=9                                 !Unit no. of INPUT set.
      OFT=10                                !Unit no. of output set.

! Open both datsets, sequential input IFT & direct access storage OFT.

      OPEN (IFT,FORM='FORMATTED')
      OPEN (OFT,ACCESS='DIRECT',RECL=27998)  !Open storage dataset

! Get time of receipt from system time (same for all obs in this input)

      CALL DATIM(NOW)
      DO I=0,4
        TOR(I+1)=NOW(8-I)
      ENDDO

! Read in a record and check to see if it is a header. A header has 'Z'
! (after an hour) in the 3rd byte, a bogus header '10' in bytes 36-37.
! If this record is not a header & the last header was for BOGUS data,
! then it's a BOGUS record: decode, BUFR encode & store the data.
! Skip any non-BOGUS header & the following records.

      DO WHILE (IOS.EQ.0)
        READ (IFT,'(A80)',IOSTAT=IOS) HEADER
        IF (IOS.EQ.0) THEN
          IF (HEADER(3:3).EQ.'Z') THEN
            IF (HEADER(36:37).EQ.'10') THEN
              INPUT_BOGUS=.TRUE.
              READ(HEADER(7:8),'(I2)')YMD(3)     ! day
              READ(HEADER(10:11),'(I2)')YMD(2)   ! month
              READ(HEADER(13:14),'(I2)')YMD(1)   ! year
              IF (YMD(1).GE.0 .AND. YMD(1).LT.1900) THEN
                YMD(1)=YMD(1)+CENTURY(YMD(1))
              ENDIF
            ELSE
              INPUT_BOGUS=.FALSE.
            ENDIF
          ELSE IF (INPUT_BOGUS) THEN   ! if this record is not a header
            TYPE=HEADER(4:6)
            CALL BOGEXP(HEADER,VALUES,TYPE,IDENT_PRESSURE)
            REP_COUNT=REP_COUNT+1
            CALL BOGIND(HEADER,VALUES,TYPE,OFT,YMD,TOR,IDENT_PRESSURE)
          ENDIF
        ENDIF
      ENDDO

! Storage is now complete for this input: close the datasets &
! write out the number of reports found (including duplicates)

      CLOSE(IFT)                    !Close input dataset
      CLOSE(OFT)                    !Close output dataset
      WRITE (6,*) 'BOGUS storage run:',REP_COUNT,'BOGUS obs this time'
      STOP
      END
