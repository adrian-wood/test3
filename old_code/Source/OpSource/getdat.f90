SUBROUTINE GETDAT(REQ,IPOS,ICENT,ITIME,IFORM,TRANGE,IFAIL, &
                  CERR)

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
! $Workfile: getdat.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 23/11/2010 17:16:52$
!
! CHANGE RECORD :
!
! $Log:
!  6    MetDB_Refresh 1.5         23/11/2010 17:16:52    Stan Kellett    added
!       use zpdate_mod
!  5    MetDB_Refresh 1.4         17/11/2010 15:52:55    John Norton     After
!       merge batch 8 changes
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:15:03    John Norton     
!  1    MetDB_Refresh 1.0         13/10/2010 16:00:38    John Norton     f77
!       code copied out of StarTeam
!       MIDE\Met_DB_Project\Development\Source\OpSource
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>
USE zpdate_mod

!None

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(INOUT)     :: REQ
INTEGER, INTENT(INOUT)          :: IPOS
INTEGER, INTENT(INOUT)          :: ICENT
INTEGER, INTENT(INOUT)          :: ITIME(4)
INTEGER, INTENT(INOUT)          :: IFORM
INTEGER, INTENT(INOUT)          :: TRANGE
INTEGER, INTENT(INOUT)          :: IFAIL
CHARACTER(*), INTENT(INOUT)     :: CERR

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  I
INTEGER      ::  IC
INTEGER      ::  IT
INTEGER      ::  ITEMP
INTEGER      ::  ITM1
INTEGER      ::  ITM2
INTEGER      ::  LREQ

LOGICAL      ::  INTCON
LOGICAL      ::  OCHK

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! DECODE DATE/TIME FORM REQUEST AND PUT INTO ITIME ARRAY
! YYYY,MM,DD,HHMM

IFAIL=0
LREQ=LEN(REQ)

! CHECK FOR EITHER "TODAY" OR YYYYMMDD

I=INDEX(REQ(IPOS:MIN(IPOS+20,LREQ)),'TODAY')
IFLABEL1: &
IF(I == 0) THEN

! YYYYMMDD/HHHHZ FOUND SO CHECK TO SEE IF
! IT CAN BE CONVERTED AND PLACE IN ARRAY

  IFORM=2
  OCHK=INTCON(REQ,IPOS,IPOS+7)
  IF(.NOT.OCHK)THEN
    CERR='TIME (YYYYMMDD)'
    IFAIL=8
    RETURN
  END IF
  OCHK=INTCON(REQ,IPOS+9,IPOS+12)
  IF(.NOT.OCHK)THEN
    CERR='TIME (HHMM)'
    IFAIL=8
    RETURN
  END IF
  READ(REQ(IPOS:IPOS+3),'(I4)')ITIME(1)
  READ(REQ(IPOS+4:IPOS+5),'(I2)')ITIME(2)
  READ(REQ(IPOS+6:IPOS+7),'(I2)')ITIME(3)
  READ(REQ(IPOS+9:IPOS+12),'(I4)')ITIME(4)
  IPOS=IPOS+14    ! MOVE PAST END AND Z
ELSE

! "TODAY" FOUND SO LOOK FOR - TO SEE NO OF DAYS AGO (OR ZERO)

  IFORM=1
  I=INDEX(REQ(IPOS:IPOS+7),'-')
IFLABEL2: &
  IF(I == 0)THEN   ! FORMAT IS TODAY/0900Z
    ITEMP=0
    IPOS=IPOS+6
    OCHK=INTCON(REQ,IPOS,IPOS+3)
    IF(.NOT.OCHK)THEN
      CERR='TIME (HHMM)'
      IFAIL=8
      RETURN
    END IF
    READ(REQ(IPOS:IPOS+3),'(I4)')ITIME(4)
    IPOS=IPOS+5
  ELSE
    IPOS=IPOS+I   ! IPOS NOW POINTS TO FIRST CHAR AFTER -
    I=INDEX(REQ(IPOS:IPOS+3),'/')
IFLABEL3: &
    IF(I == 2)THEN ! ITS ONE DIGIT
      OCHK=INTCON(REQ,IPOS,IPOS)
      IF(.NOT.OCHK)THEN
        CERR='TIME (DAY)'
        IFAIL=8
        RETURN
      END IF
      READ(REQ(IPOS:IPOS),'(I1)')ITEMP
      IPOS=IPOS+2
    ELSE IF(I == 3)THEN ! ITS TWO DIGITS
      OCHK=INTCON(REQ,IPOS,IPOS+1)
      IF(.NOT.OCHK)THEN
        CERR='TIME (DAY)'
        IFAIL=8
        RETURN
      END IF
      READ(REQ(IPOS:IPOS+1),'(I2)')ITEMP
      IPOS=IPOS+3
    ELSE IF(I == 4)THEN ! ITS THREE DIGITS
      OCHK=INTCON(REQ,IPOS,IPOS+2)
      IF(.NOT.OCHK)THEN
        CERR='TIME (DAY)'
        IFAIL=8
        RETURN
      END IF
      READ(REQ(IPOS:IPOS+2),'(I3)')ITEMP
      IPOS=IPOS+4
    END IF IFLABEL3
! NOW IPOS IS POINTING TO HHHHZ
    OCHK=INTCON(REQ,IPOS,IPOS+3)
    IF(.NOT.OCHK)THEN
      CERR='TIME (HHMM)'
      IFAIL=8
      RETURN
    END IF
    READ(REQ(IPOS:IPOS+3),'(I4)')ITIME(4)
    IPOS=IPOS+5
  END IF IFLABEL2
! SET DATE USING CURRENT DATE
  IC=ICENT
  IC=IC-ITEMP
  CALL DATE13(IC,ITIME(3),ITIME(2),ITIME(1))
END IF IFLABEL1

!-----------------------------------------------------------------------
! Check to see if we have reached the end of REQ.
!-----------------------------------------------------------------------

IF (IPOS >= LREQ) RETURN

! NOW CHECK FOR SUB-PERIOD
I=INDEX(REQ(IPOS:IPOS+1),'-')
IFLABEL4: &
IF(I == 0)THEN
  IPOS=IPOS+1
ELSE
  I=INDEX(REQ(IPOS:IPOS+7),'Z')
  IF(I == 0)THEN
    CERR='SUB-PERIOD'
    IFAIL=8
    RETURN
  END IF
! MOVE POINTER TO START OF HHMMZ SUB-PERIOD
  IPOS=IPOS+I-5
  OCHK=INTCON(REQ,IPOS,IPOS+3)
  IF(.NOT.OCHK)THEN
    CERR='SUB-PERIOD HHMM'
    IFAIL=8
    RETURN
  END IF
  READ(REQ(IPOS:IPOS+3),'(I4)')IT
! NOW FIND TRANGE IN MINUTES
  ITM1=ITIME(4)/100*60 + MOD(ITIME(4),100)
  ITM2=IT/100*60 + MOD(IT,100)
  TRANGE=ITM2-ITM1
  IF(TRANGE < 0)TRANGE=TRANGE+1440
  IPOS=IPOS+6
  IFAIL=4
END IF IFLABEL4

RETURN
END SUBROUTINE GETDAT
