      SUBROUTINE GETDAT(REQ,IPOS,ICENT,ITIME,IFORM,TRANGE,IFAIL,    !2.0
     &                  CERR)                                       !2.0

!-----------------------------------------------------------------------
!
! PROGRAM       : GETDAT IN MDB
!
! PURPOSE       : DECODE DATES AND TIMES FROM THE REQUEST
!
! DESCRIPTION   : FORMATS ALLOWED YYYYMMDD/HHMMZ
!                                 TODAY/HHMMZ
!                                 TODAY-N/HHMMZ
!                                 TODAY-NN/HHMMZ
!                                 TODAY-NNN/HHMMZ
!                          SUB-PERIOD ..HHMMZ-HHMMZ
!                          SUB-PERIOD ..HHMMZ - HHMMZ
!
!
! CALLED BY     : GETREQ IN MDB
!
! CALLS         : DATE13
!
! PARAMETERS    : (1)REQ   CHARACTER REQUEST
!                 (2)IPOS  POINTING TO 1ST CHAR OF DATE/TIME ON ENTRY
!                          AND FIRST CHAR OF NEXT GROUP ON EXIT
!                 (3)ICENT CURRENT CENTURY HOUR
!                 (4)ITIME(4) OUTPUT YYYY,MM,DD,HHMM
!                 (5)IFORM  =1 FOR TODAY FORMAT ELSE 2
!                 (6)TRANGE  SUB-PERIOD MINUTES
!                 (7)IFAIL = 8 IF INVALID CHARACTERS DETECTED
!                            4 IF SUB-PERIOD DETECTED (IT'S ONLY
!                            ALLOWED ON A START TIME)
!                 (8)CERR   ERROR MESSAGE OUTPUT
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:32$
! $Source: /data/us0400/mdb/op/lib/source/RCS/getdat.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:32    Sheila Needham  
! $
! Revision 2.1  2002/04/09  11:32:20  11:32:20  usmdb (Generic MetDB account)
! Two changes to prevent out of bounds problems on HP when a
! MetDB request doesn't include the ELEMENTS keyword. Also
! replaced all GOTO 999 with a RETURN statement - S.Cox
! 
! Revision 2.0  2001/01/08  11:58:40  11:58:40  usmdb (Generic MetDB account)
! Removed unused dummy argument ILEN. Added copyright
! and modified header, comments - S.Cox
!
! Revision 1.3  97/08/04  13:10:07  13:10:07  uspm (Pat McCormack)
! First revisioned version for 1  - with Y2K change
!
! Revision 1.2  1997/02/12 12:30:36  uspm
! Update dir name in variable head
!
! Revision 1.1  1997/02/11 14:38:28  uspm
! Initial revision
!
! 10/12/93  ALLOW SUB-PERIOD OF UP TO 24 HOURS
!
! 22/04/93  CORRECT INCREMENTS PAST END OF HHMM TO ALLOW FOR Z
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
      CHARACTER*(*) REQ,CERR
      INTEGER IPOS,ITIME(4),IFAIL,ICENT,TRANGE                      !2.0
      LOGICAL OCHK,INTCON
      INTEGER I, IFORM, ITEMP, IC, IT, ITM1, ITM2
      INTEGER LREQ                                                  !2.1

      CHARACTER*132 HEAD
      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/getdat.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:22:32$ '

! DECODE DATE/TIME FORM REQUEST AND PUT INTO ITIME ARRAY
! YYYY,MM,DD,HHMM

      IFAIL=0
      LREQ=LEN(REQ)                                                 !2.1

! CHECK FOR EITHER "TODAY" OR YYYYMMDD

      I=INDEX(REQ(IPOS:MIN(IPOS+20,LREQ)),'TODAY')                  !2.1
      IF(I.EQ.0) THEN

! YYYYMMDD/HHHHZ FOUND SO CHECK TO SEE IF
! IT CAN BE CONVERTED AND PLACE IN ARRAY

        IFORM=2
        OCHK=INTCON(REQ,IPOS,IPOS+7)
        IF(.NOT.OCHK)THEN
          CERR='TIME (YYYYMMDD)'
          IFAIL=8
          RETURN                                                    !2.1
        ENDIF
        OCHK=INTCON(REQ,IPOS+9,IPOS+12)
        IF(.NOT.OCHK)THEN
          CERR='TIME (HHMM)'
          IFAIL=8
          RETURN                                                    !2.1
        ENDIF
        READ(REQ(IPOS:IPOS+3),'(I4)')ITIME(1)
        READ(REQ(IPOS+4:IPOS+5),'(I2)')ITIME(2)
        READ(REQ(IPOS+6:IPOS+7),'(I2)')ITIME(3)
        READ(REQ(IPOS+9:IPOS+12),'(I4)')ITIME(4)
        IPOS=IPOS+14    ! MOVE PAST END AND Z
      ELSE

! "TODAY" FOUND SO LOOK FOR - TO SEE NO OF DAYS AGO (OR ZERO)

        IFORM=1
        I=INDEX(REQ(IPOS:IPOS+7),'-')
        IF(I.EQ.0)THEN   ! FORMAT IS TODAY/0900Z
          ITEMP=0
          IPOS=IPOS+6
          OCHK=INTCON(REQ,IPOS,IPOS+3)
          IF(.NOT.OCHK)THEN
            CERR='TIME (HHMM)'
            IFAIL=8
            RETURN                                                  !2.1
          ENDIF
          READ(REQ(IPOS:IPOS+3),'(I4)')ITIME(4)
          IPOS=IPOS+5
        ELSE
          IPOS=IPOS+I   ! IPOS NOW POINTS TO FIRST CHAR AFTER -
          I=INDEX(REQ(IPOS:IPOS+3),'/')
          IF(I.EQ.2)THEN ! ITS ONE DIGIT
            OCHK=INTCON(REQ,IPOS,IPOS)
            IF(.NOT.OCHK)THEN
              CERR='TIME (DAY)'
              IFAIL=8
              RETURN                                                !2.1
            ENDIF
            READ(REQ(IPOS:IPOS),'(I1)')ITEMP
            IPOS=IPOS+2
          ELSEIF(I.EQ.3)THEN ! ITS TWO DIGITS
            OCHK=INTCON(REQ,IPOS,IPOS+1)
            IF(.NOT.OCHK)THEN
              CERR='TIME (DAY)'
              IFAIL=8
              RETURN                                                !2.1
            ENDIF
            READ(REQ(IPOS:IPOS+1),'(I2)')ITEMP
            IPOS=IPOS+3
          ELSEIF(I.EQ.4)THEN ! ITS THREE DIGITS
            OCHK=INTCON(REQ,IPOS,IPOS+2)
            IF(.NOT.OCHK)THEN
              CERR='TIME (DAY)'
              IFAIL=8
              RETURN                                                !2.1
            ENDIF
            READ(REQ(IPOS:IPOS+2),'(I3)')ITEMP
            IPOS=IPOS+4
          ENDIF
! NOW IPOS IS POINTING TO HHHHZ
          OCHK=INTCON(REQ,IPOS,IPOS+3)
          IF(.NOT.OCHK)THEN
            CERR='TIME (HHMM)'
            IFAIL=8
            RETURN                                                  !2.1
          ENDIF
          READ(REQ(IPOS:IPOS+3),'(I4)')ITIME(4)
          IPOS=IPOS+5
        ENDIF
! SET DATE USING CURRENT DATE
        IC=ICENT
        IC=IC-ITEMP
        CALL DATE13(IC,ITIME(3),ITIME(2),ITIME(1))
      ENDIF

!-----------------------------------------------------------------------
! Check to see if we have reached the end of REQ.
!-----------------------------------------------------------------------

      IF (IPOS.GE.LREQ) RETURN                                      !2.1

! NOW CHECK FOR SUB-PERIOD
      I=INDEX(REQ(IPOS:IPOS+1),'-')
      IF(I.EQ.0)THEN
        IPOS=IPOS+1
      ELSE
        I=INDEX(REQ(IPOS:IPOS+7),'Z')
        IF(I.EQ.0)THEN
          CERR='SUB-PERIOD'
          IFAIL=8
          RETURN                                                    !2.1
        ENDIF
! MOVE POINTER TO START OF HHMMZ SUB-PERIOD
        IPOS=IPOS+I-5
        OCHK=INTCON(REQ,IPOS,IPOS+3)
        IF(.NOT.OCHK)THEN
          CERR='SUB-PERIOD HHMM'
          IFAIL=8
          RETURN                                                    !2.1
        ENDIF
        READ(REQ(IPOS:IPOS+3),'(I4)')IT
! NOW FIND TRANGE IN MINUTES
        ITM1=ITIME(4)/100*60 + MOD(ITIME(4),100)
        ITM2=IT/100*60 + MOD(IT,100)
        TRANGE=ITM2-ITM1
        IF(TRANGE.LT.0)TRANGE=TRANGE+1440
        IPOS=IPOS+6
        IFAIL=4
      ENDIF

      RETURN                                                        !2.1
      END
