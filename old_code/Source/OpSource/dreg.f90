SUBROUTINE DREG (NUMERR, TEXT, BULL, CALLER, TYPE, HEADER, NTMSG, DSN)

!----------------------------------------------------------------------!
!                                                                      !
! SUBROUTINE   : DREG                                                  !
!                                                                      !
! PURPOSE      : TO ADD A RECORD TO THE DREGS DATA SET FOR THIS JOB.   !
!                                                                      !
! DESCRIPTION  : "DREG" IS USED TO ADD A DREGS MESSAGE FOR A BULLETIN  !
!                TO THE DREGS DATA SET ASSOCIATED WITH A STORAGE JOB.  !
!                                                                      !
!                IF THE CODE NUMBER IS SPECIFIED ("NUMERR">0), A DREGS !
!                RECORD IS WRITTEN TO THE DATA SET.                    !
!                                                                      !
!                IF "NUMERR" < OR =0, THE INFORMATION SPECIFIED IN THE !
!                ARGUMENTS IS STORED INTERNALLY FOR USE WHEN A DREGS   !
!                MESSAGE IS REQUESTED, ENABLING A FULL DREGS RECORD    !
!                TO BE OUTPUT LATER EVEN IF SOME OF THE INFORMATION    !
!                REQUIRED IS NOT AVAILABLE LOCALLY IN THE CALLING      !
!                ROUTINE.                                              !
!                                                                      !
!                A BLANK VALUE FOR ANY CHARACTER ARGUMENT (OR ZERO OR  !
!                NEGATIVE VALUE FOR "NTMSG") IN A CALL TO "DREG" WILL  !
!                BE TREATED AS 'VALUE NOT SUPPLIED' FOR THIS ARGUMENT. !
!                INTERNAL CHARACTER STRINGS CORRESPONDING TO SUCH      !
!                ARGUMENTS WILL BE SET TO BLANKS IF A VALID VALUE IS   !
!                SUPPLIED FOR AT LEAST ONE SUBSEQUENT ARGUMENT IN THE  !
!                LIST. SEE BELOW FOR EXAMPLES.                         !
!                                                                      !
!                FOR INTERNAL STORAGE, CHARACTER STRINGS ARE TRUNCATED !
!                OR RIGHT-PADDED WITH BLANKS TO THE FOLLOWING LENGTHS: !
!                                                                      !
!                        TEXT  BULL  CALLER  TYPE  HEADER  DSN         !
!                         40    512     8      8     18     40         !
!                                                                      !
!                THESE LENGTHS APPLY TO INTERNAL VARIABLES ONLY: THE   !
!                ARGUMENTS ARE NOT CHANGED BY THE SUBROUTINE.          !
!                                                                      !
!                THE UNIT NUMBER OF THE DREGS DATA SET CAN BE SET WITH !
!                A SPECIAL CALL IN WHICH 'TEXT' IS 'DREGUNIT' AND THE  !
!                VALUE OF 'NUMERR' IS THE UNIT NUMBER REQUIRED: THIS   !
!                MUST BE THE FIRST CALL TO 'DREG'. TO SUPPRESS DREGS   !
!                OUTPUT ALTOGETHER, SPECIFY 'NUMERR' < OR =0.          !
!                                                                      !
! USAGE        : CALL DREG (NUMERR, TEXT, BULL, CALLER, TYPE, HEADER,  !
!                           NTMSG, DSN)                                !
!                                                                      !
! ARGUMENTS   :                                                        !
!                                                                      !
!              : NUMERR  (INTEGER) CODE NUMBER ASSOCIATED WITH MESSAGE !
!              : TEXT    TEXT OF ERROR MESSAGE                         !
!              : BULL    BULLETIN TEXT                                 !
!              : CALLER  NAME OF CALLING ROUTINE                       !
!              : TYPE    DATA TYPE FOR BULLETIN CAUSING DREGS MESSAGE  !
!              : HEADER  GTS BULLETIN HEADER ("TTAAII CCCC YYGGGG")    !
!              : NTMSG   (8-ELEMENT INTEGER ARRAY) TIME OF MESSAGE (AS !
!                        OUTPUT BY A CALL TO "DATIM")                  !
!              : DSN     NAME OF TROPICS DATA SET CONTAINING BULLETIN  !
!                                                                      !
! CALLED BY    : VARIOUS MET.D.B. STORAGE ROUTINES.                    !
!                                                                      !
! EXAMPLES OF USE:  SUPPOSE THE THREE CALLS BELOW ARE EXECUTED.        !
! ---------------                                                      !
!                                                                      !
!  (1)    CALL DREG (0, ' ', ' ', ' ', ' ', GTSHDR, ITOR, DNAME)       !
!                                                                      !
!     THIS SETS INTERNAL STRINGS FOR GTS HEADER, TIME OF MESSAGE (HELD !
!     AS AN 18-CHARACTER TEXT STRING) AND DATA SET NAME. THOSE FOR     !
!     DATA TYPE, CALLING ROUTINE, BULLETIN TEXT AND ERROR MESSAGE      !
!     WILL BE SET TO BLANKS.                                           !
!                                                                      !
!  (2)    CALL DREG (0, ' ', ' ', 'MYSUB', 'AIREP', ' ', 0, ' ')       !
!                                                                      !
!     THIS SETS INTERNAL STRINGS FOR CALLING ROUTINE AND DATA TYPE.    !
!     STRINGS FOR BULLETIN TEXT AND ERROR MESSAGE WILL BE SET TO       !
!     BLANKS BUT THOSE FOR GTS HEADER, TIME OF MESSAGE AND DATA SET    !
!     NAME WILL NOT BE RESET.                                          !
!                                                                      !
!  (3)    CALL DREG (123, 'ANOTHER DISMAL FAILURE.', BULL(1:N),        !
!        &           'CRASHER', ' ', ' ', 0, ' ')                      !
!                                                                      !
!     THIS WILL OUTPUT A DREGS MESSAGE WITH THE SPECIFIED CODE NUMBER, !
!     ERROR TEXT, BULLETIN TEXT AND THE CALLING ROUTINE "CRASHER".     !
!     DATA TYPE WILL BE 'AIREP' FROM (2), AND GTS HEADER, MESSAGE      !
!     TIME AND DATA SET NAME WILL BE THOSE SPECIFIED IN (1).           !
!                                                                      !
!      NOTE:  IT IS SAFE TO CODE "NTMSG" AS '0' AS IN (2) AND (3)      !
!      ----   ABOVE WITHOUT GIVING A FULL 8-ELEMENT INTEGER ARRAY.     !
!
! REVISION INFO :
!
! CHANGE RECORD :
! $Workfile: dreg.f90$ $Folder: OpSource$
! $Revision: 7$ $Date: 06/05/2011 16:14:45$
!
! $Log:
!  7    MetDB_Refresh 1.6         06/05/2011 16:14:45    Brian Barwell   Use
!       element 8 of NTMSG (=year) to test whether date/time is specified.
!  6    MetDB_Refresh 1.5         10/03/2011 12:07:13    Sheila Needham
!       Corrections following review
!  5    MetDB_Refresh 1.4         07/03/2011 09:43:55    John Norton     After
!       updating for C I/O routines. Ready for review.
!  4    MetDB_Refresh 1.3         31/01/2011 15:04:52    Sheila Needham
!       Updated OPEN stmt
!  3    MetDB_Refresh 1.2         25/01/2011 15:45:52    Brian Barwell   Small
!       correction - BULL now INTENT(IN).
!  2    MetDB_Refresh 1.1         22/12/2010 09:43:15    Sheila Needham
!       Updated following review
!  1    MetDB_Refresh 1.0         16/12/2010 16:30:41    Richard Weedon
!       updated to f95 standard
! $
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL, JANUARY 2000.
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
! Interfaces (none)
IMPLICIT NONE

! Arguments
INTEGER,         INTENT(IN)    ::  NUMERR   ! (1) ERROR CODE NUMBER
CHARACTER(LEN=*),INTENT(IN)    ::  TEXT     ! (2) DREGS MESSAGE TEXT
CHARACTER(LEN=*),INTENT(IN)    ::  BULL     ! (3) VARIABLE LENGTH STRINGS
CHARACTER(LEN=*),INTENT(IN)    ::  CALLER   ! (4) NAME OF CALLING ROUTINE
CHARACTER(LEN=*),INTENT(IN)    ::  TYPE     ! (5) DATA TYPE
CHARACTER(LEN=*),INTENT(IN)    ::  HEADER   ! (6) GTS BULLETIN HEADER
INTEGER,         INTENT(IN)    ::  NTMSG(8) ! (7) TIME OF MESS (AS FROM "DATIM")
CHARACTER(LEN=*),INTENT(IN)    ::  DSN      ! (8) MHS DATA SET NAME

! Local Variables
INTEGER               :: J            ! local counter
INTEGER,PARAMETER     :: LENTXT=40
INTEGER,PARAMETER     :: LENDSN=40
INTEGER,PARAMETER     :: LENREC=656
INTEGER,PARAMETER     :: LENBUL=512
INTEGER               :: IRC          ! Return code from OPEN statement
INTEGER               :: LAST         ! LAST DREGS RECORD WRITTEN TO
INTEGER               :: MODE         ! 3 read and write existing (update)

INTEGER               :: NDREG=99     ! UNIT NUMBER OF DREGS DATA SET
INTEGER               :: NEXT         ! NEXT DREGS RECORD TO WRITE TO
INTEGER               :: NRECS        ! NO. OF RECORDS IN DREGS DATA SET!
INTEGER               :: RECNO        ! First record number for READ    !
LOGICAL               :: FIRST =.TRUE. ! FLAG FOR FIRST CALL TO "DREG"
LOGICAL               :: RESET        ! FLAG FOR RESETTING TEXT VARIABLES
CHARACTER(LEN=LENBUL) :: BULTXT       ! BULLETIN TEXT FOR OUTPUT
CHARACTER(LEN=LENREC) :: buffer       ! C READ buffer
CHARACTER             :: C            ! 1-BYTE VARIABLE FOR INTERNAL USE
CHARACTER(LEN=8)      :: DATYPE       ! DATA TYPE
CHARACTER(LEN=8)      :: DDNAME       ! dd name for read
CHARACTER(LEN=LENDSN) :: DSNAME       ! 'FROST' DATA SET NAME
CHARACTER(LEN=18)     :: GTS18        ! 'TTAAII CCCC YYGGGG'
CHARACTER(LEN=8)      :: MODULE       ! NAME OF CALLING ROUTINE
CHARACTER(LEN=LENTXT) :: MSGTXT       ! TEXT OF ERROR MESSAGE
CHARACTER(LEN=13)     :: YMDHM        ! TIME OF MESS ("YYYYMMDD:HHMM")

!                                                       SAVED VARIABLES
SAVE FIRST, NDREG, LAST, NRECS
SAVE DSNAME, YMDHM, GTS18, DATYPE, MODULE, BULTXT, MSGTXT

IF (NDREG <= 0) RETURN

!-----------------------------------------------------------------------
!     (FIRST CALL ONLY)  OPEN DREGS DATA SET & READ HEADER.
!                        ALSO CHECK FOR SPECIAL 'DREGUNIT' CALL.
!-----------------------------------------------------------------------

IF (FIRST) THEN
   FIRST = .FALSE.
!                                              CHECK FOR UNIT NO. RESET
   RESET = TEXT(1:8) == 'DREGUNIT'
   IF (RESET) NDREG = NUMERR
!                                                   OPEN DREGS DATA SET
   IF (NDREG > 0) THEN
     DDNAME='DREGS'
     MODE=3
     CALL METDB_COPEN( NDREG,'DD:'//ddname//CHAR(0), mode, irc)
     RECNO=1
     CALL METDB_CREAD_DIR(NDREG, buffer, LENREC, RECNO, irc)
     READ (BUFFER,'(2I6)') LAST, NRECS
     LAST = MAX0(LAST,1)       
   END IF
!                                             RETURN IF 'DREGUNIT' CALL
   IF (RESET) RETURN
END IF

!-----------------------------------------------------------------------
!     PAD OUT (OR TRUNCATE) ITEMS TO RIGHT LENGTH. ITEMS WILL BE RESET
!     TO BLANKS IF NO VALID VALUE WAS SPECIFIED IN THE ARGUMENT LIST
!     BUT A LATER ITEM IN THE LIST HAD BEEN SPECIFIED.
!-----------------------------------------------------------------------

RESET = .FALSE.
!                                        (1) MHS DATA SET NAME
IF (DSN(1:1) /= ' ') THEN
   DSNAME = DSN
   RESET = .TRUE.
END IF
!                                        (2) TIME OF RECEIPT
IF (NTMSG(8) == 0) THEN  ! (Year = 0)
   IF (RESET) YMDHM = ' '
ELSE
   WRITE (YMDHM,'(I4,2I2.2,1X,2I2.2)') (NTMSG(J),J=8,4,-1)
   RESET = .TRUE.
END IF
!                                        (3) GTS HEADER
IF (HEADER(1:1) == ' ') THEN
   IF (RESET) GTS18 = ' '
ELSE
   GTS18 = HEADER
   RESET = .TRUE.
END IF
!                                        (4) DATA TYPE
IF (TYPE(1:1) == ' ') THEN
   IF (RESET) DATYPE = ' '
ELSE
   DATYPE = TYPE
   RESET = .TRUE.
END IF
!                                        (5) CALLING ROUTINE
IF (CALLER(1:1) == ' ') THEN
   IF (RESET) MODULE = ' '
ELSE
   MODULE = CALLER
   RESET = .TRUE.
END IF
!                                        (6) BULLETIN TEXT
IF (BULL == ' ') THEN
   IF (RESET) BULTXT = ' '
ELSE
   BULTXT = BULL
   RESET = .TRUE.
END IF
!                                        (7) ERROR MESSAGE TEXT
IF (TEXT == ' ') THEN
   IF (RESET) MSGTXT = ' '
ELSE
   MSGTXT = TEXT
END IF

!-----------------------------------------------------------------------
!     IF THE ERROR CODE NUMBER "NUMERR" IS POSITIVE, WRITE OUT A NEW
!     DREGS MESSAGE, BUT FIRST CHECK TO SEE IF THE NEXT DREGS RECORD
!     IS FREE.
!-----------------------------------------------------------------------

IF (NUMERR > 0) THEN
!                                          FIND NEXT RECORD TO WRITE TO
   NEXT = LAST + 1
   IF (NEXT > NRECS) NEXT = 2
!                                         CHECK FIRST DIGIT OF THE YEAR

   CALL METDB_CREAD_DIR(NDREG, buffer, LENREC, NEXT, irc)
   C=BUFFER(1:1)
!                                        IF RECORD IS FREE, WRITE TO IT
   IF (C == ' ') THEN ! FREE
      WRITE(BUFFER,'(2I6)')NEXT,NRECS
      RECNO = 1
      CALL METDB_CWRITE_DIR(NDREG,BUFFER,LENREC,RECNO,IRC) ! header
      WRITE (BUFFER, '(2A, I5, 5(2X,A), 1X,A)')          &
           YMDHM, ':', NUMERR, MODULE, DATYPE, MSGTXT,   &
           DSNAME, GTS18, BULTXT
      CALL METDB_CWRITE_DIR(NDREG,BUFFER,LENREC,NEXT,IRC)  ! data
      LAST = NEXT
!                                     IF IT'S NOT FREE, PRINT A MESSAGE
   ELSE
      WRITE (6,'(T5,A,T15,A)')'DREG:', 'DREGS DATA SET HAS FILLED UP'
   END IF
END IF
!          RETURN TO CALLING PROGRAM
RETURN
END SUBROUTINE DREG
