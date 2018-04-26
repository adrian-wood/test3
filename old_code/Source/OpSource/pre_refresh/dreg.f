      SUBROUTINE DREG
     *           (NUMERR, TEXT, BULL, CALLER, TYPE, HEADER, NTMSG, DSN)
!
      IMPLICIT NONE
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
! PARAMETERS   : (ALL ARE INPUT PARAMETERS AND ARE CHARACTER*(*)       !
!                 UNLESS OTHERWISE STATED.)                            !
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
! $Revision: 1$
! $Date: 30/01/2006 20:22:06$
! $Source: /home/us0400/mdb/op/lib/source/RCS/dreg.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:06    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:41  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  2000/03/10  10:07:22  10:07:22  usmdb
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL, JANUARY 2000.
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
!
!                                                            PARAMETERS
      INTEGER    LENTXT,    LENDSN,    LENREC,     LENBUL
      PARAMETER (LENTXT=40, LENDSN=40, LENREC=656, LENBUL=512)
!                                                             VARIABLES
!
      INTEGER J                     ! INTEGER FOR LOCAL INTERNAL USE
      INTEGER LAST                  ! LAST DREGS RECORD WRITTEN TO
      INTEGER NDREG                 ! UNIT NUMBER OF DREGS DATA SET
      INTEGER NEXT                  ! NEXT DREGS RECORD TO WRITE TO
      INTEGER NRECS                 ! NO. OF RECORDS IN DREGS DATA SET
      INTEGER NTMSG(8)              ! TIME OF message (AS FROM "DATIM")
      INTEGER NUMERR                ! ERROR CODE NUMBER
!
      LOGICAL FIRST                 ! FLAG FOR FIRST CALL TO "DREG"
      LOGICAL RESET                 ! FLAG FOR RESETTING TEXT VARIABLES
!
      CHARACTER C                   ! 1-BYTE VARIABLE FOR INTERNAL USE
      CHARACTER*132 HEAD            ! FOR REVISION DETAILS
      CHARACTER*13 YMDHM            ! TIME OF MESSAGE ("YYYYMMDD HHMM")
      CHARACTER*8 DATYPE            ! DATA TYPE
      CHARACTER*8 MODULE            ! NAME OF CALLING ROUTINE
      CHARACTER*18 GTS18            ! 'TTAAII CCCC YYGGGG'
      CHARACTER*(LENTXT) MSGTXT     ! TEXT OF ERROR MESSAGE
      CHARACTER*(LENDSN) DSNAME     ! 'TROPICS' DATA SET NAME
      CHARACTER*(LENBUL) BULTXT     ! BULLETIN TEXT FOR OUTPUT
!
      CHARACTER*(*) TYPE, CALLER    !) (SUBROUTINE ARGUMENTS) AS
      CHARACTER*(*) HEADER, TEXT    !) PREVIOUS 6 VARIABLES BUT
      CHARACTER*(*) DSN, BULL       !) VARIABLE LENGTH STRINGS
!
!                                                   DATA INITIALISATION
      DATA FIRST/.TRUE./, NDREG/99/
!                                                       SAVED VARIABLES
      SAVE FIRST, NDREG, LAST, NRECS
      SAVE DSNAME, YMDHM, GTS18, DATYPE, MODULE, BULTXT, MSGTXT
!
      IF (NDREG.LE.0) RETURN
!
!-----------------------------------------------------------------------
!     (FIRST CALL ONLY)  OPEN DREGS DATA SET & READ HEADER.
!                        ALSO CHECK FOR SPECIAL 'DREGUNIT' CALL.
!-----------------------------------------------------------------------
!
      IF (FIRST) THEN
         FIRST = .FALSE.
!                                              CHECK FOR UNIT NO. RESET
         RESET = TEXT(1:8).EQ.'DREGUNIT'
         IF (RESET) NDREG = NUMERR
!                                                   OPEN DREGS DATA SET
         IF (NDREG.GT.0) THEN
            OPEN (NDREG, FILE='DREGS', STATUS='OLD', ACCESS='DIRECT',
     &            FORM='FORMATTED', RECL=LENREC, ACTION='READWRITE')
            READ (NDREG,'(2I6)',REC=1) LAST, NRECS
            LAST = MAX0(LAST,1)
         END IF
!                                             RETURN IF 'DREGUNIT' CALL
         IF (RESET) RETURN
!                                                  REVISION INFORMATION
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/dreg.F,v $
     &   '//'$Date: 30/01/2006 20:22:06$ $Revision: 1$'
      END IF
!
!-----------------------------------------------------------------------
!     PAD OUT (OR TRUNCATE) ITEMS TO RIGHT LENGTH. ITEMS WILL BE RESET
!     TO BLANKS IF NO VALID VALUE WAS SPECIFIED IN THE ARGUMENT LIST
!     BUT A LATER ITEM IN THE LIST HAD BEEN SPECIFIED.
!-----------------------------------------------------------------------
!
      RESET = .FALSE.
!                                        (1) MHS DATA SET NAME
      IF (DSN(1:1).NE.' ') THEN
         DSNAME = DSN
         RESET = .TRUE.
      END IF
!                                        (2) TIME OF RECEIPT
      IF (NTMSG(1).EQ.0) THEN
         IF (RESET) YMDHM = ' '
      ELSE
         WRITE (YMDHM,'(I4,2I2.2,1X,2I2.2)') (NTMSG(J),J=8,4,-1)
         RESET = .TRUE.
      END IF
!                                        (3) GTS HEADER
      IF (HEADER(1:1).EQ.' ') THEN
         IF (RESET) GTS18 = ' '
      ELSE
         GTS18 = HEADER
         RESET = .TRUE.
      END IF
!                                        (4) DATA TYPE
      IF (TYPE(1:1).EQ.' ') THEN
         IF (RESET) DATYPE = ' '
      ELSE
         DATYPE = TYPE
         RESET = .TRUE.
      END IF
!                                        (5) CALLING ROUTINE
      IF (CALLER(1:1).EQ.' ') THEN
         IF (RESET) MODULE = ' '
      ELSE
         MODULE = CALLER
         RESET = .TRUE.
      END IF
!                                        (6) BULLETIN TEXT
      IF (BULL.EQ.' ') THEN
         IF (RESET) BULL = ' '
      ELSE
         BULTXT = BULL
         RESET = .TRUE.
      END IF
!                                        (7) ERROR MESSAGE TEXT
      IF (TEXT.EQ.' ') THEN
         IF (RESET) MSGTXT = ' '
      ELSE
         MSGTXT = TEXT
      END IF
!
!-----------------------------------------------------------------------
!     IF THE ERROR CODE NUMBER "NUMERR" IS POSITIVE, WRITE OUT A NEW
!     DREGS MESSAGE, BUT FIRST CHECK TO SEE IF THE NEXT DREGS RECORD
!     IS FREE.
!-----------------------------------------------------------------------
!
      IF (NUMERR.GT.0) THEN
!                                          FIND NEXT RECORD TO WRITE TO
         NEXT = LAST + 1
         IF (NEXT.GT.NRECS) NEXT = 2
!                                         CHECK FIRST DIGIT OF THE YEAR
         READ (NDREG,'(A)',REC=NEXT) C
!                                        IF RECORD IS FREE, WRITE TO IT
         IF (C.EQ.' ') THEN ! FREE
            WRITE (NDREG, '(2I6)', REC=1) NEXT, NRECS  ! (HEADER)
            WRITE (NDREG, '(2A, I5, 5(2X,A), 1X,A)', REC=NEXT)
     &             YMDHM, ':', NUMERR, MODULE, DATYPE, MSGTXT,
     &             DSNAME, GTS18, BULTXT
            LAST = NEXT
!                                     IF IT'S NOT FREE, PRINT A MESSAGE
         ELSE
            WRITE (6,'(T5,A,T15,A)')
     *               'DREG:', 'DREGS DATA SET HAS FILLED UP'
         END IF
      END IF
!                                             RETURN TO CALLING PROGRAM
      RETURN
      END
