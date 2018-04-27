SUBROUTINE GRIBRET (DATYPE,DSNX,NOBS,IDATA,KEYWRD,LTEST,&
     &ISELECT, ITIME, ISTAT, CREP, RVALS)       !2.2

!----------------------------------------------------------------------
! SUBROUTINE   : GRIBRET
!
! PURPOSE      : TO RETURN REQUESTED GRIB FIELDS IN A USER-SUPPLIED
!                CHARACTER ARRAY.
!
! DESCRIPTION  : 'GRIBRET' RETRIEVES THE GRIB FIELD(S) MATCHING THE
!                DATA TYPE ('DATYPE') AND TIME WINDOW ('ITIME') AND
!                RETURNS THE DATA IN THE USER-SUPPLIED ARRAY 'CREP',
!                A CHARACTER*(*) ARRAY OF DIMENSION 'NOBS'.
!
!                IF TWO OR MORE FIELDS ARE REQUESTED THEY WILL BE
!                RETURNED IN ONE CALL (IF 'CREP' IS BIG ENOUGH),
!                FIELDS BEING CONCATENATED IN THE ARRAY.
!
!                IF 'CREP' IS NOT LARGE ENOUGH TO HOLD THE FIELD(S),
!                IT IS FILLED WITH AS MUCH DATA AS IT CAN HOLD AND A
!                STATUS CODE ('ISTAT') OF 4 IS RETURNED TO INDICATE
!                MORE DATA TO COME.  THE NEXT CALL WILL THEN RETURN
!                ANOTHER BATCH OF DATA STARTING FROM WHERE THE
!                PREVIOUS CALL LEFT OFF.  WHEN THE FINAL BATCH OF
!                DATA IS RETURNED THE UNUSED PART OF 'CREP' IS PADDED
!                WITH BINARY ZEROES AND AN 'ISTAT' OF 0 IS RETURNED.
!                IF NO DATA IS FOUND AN 'ISTAT' OF 8 IS RETURNED.
!
!                AFTER EACH CALL, THE VALUES IN THE REAL ARRAY
!                'RVALS' WILL CONTAIN THE NUMBER OF BYTES OF DATA
!                RETURNED IN THE CORRESPONDING ELEMENTS OF 'CREP'.
!                'RVALS' IS TREATED AS 1-DIMENSIONAL IN THIS ROUTINE
!                AND THEREFORE REQUIRES THE USER TO SPECIFY 'NELEM'=1
!                WHEN CALLING 'MDB' (SEE MET.D.B. TECH. NOTE. 3.)
!
! USAGE        : CALL GRIBRET (DATYPE, DSNX, NOBS, IDATA, KEYWRD,   !2.2
!                     LTEST, ISELECT, ITIME, ISTAT, CREP, RVALS)    !2.2
!
! PARAMETERS   : DATYPE   I   (CHARACTER*(*)) MET.D.B. DATA SUBTYPE.
!                DSNX     I   (CHARACTER*(*)) NAME OF INDEX DATA SET.
!                NOBS     I   DIMENSION OF 'CREP' ARRAY.
!                IDATA    I   5-ELEMENT INTEGER ARRAY WITH INDEX DATA
!                             SET DETAILS AS OUTPUT BY 'DDICT':
!                               1  LENGTH OF DATA SET NAME,
!                               2  DATA SET RECORD LENGTH,
!                               3  REQUIRED FT NUMBER,
!                              4&5 (NOT USED FOR GRIB DATA).
!                KEYWRD   I   ARRAY OF MDB KEYWORDS (FOR 'GRIBFIND')
!                LTEST    I   SET .TRUE. FOR EXTRA PRINT STATEMENTS.
!                ISELECT  I   LIST OF WANTED DATA SELECTION VALUES  !2.2
!                ITIME   I/O  (8-ELEMENT INTEGER ARRAY) USER'S START
!                             AND END TIMES AS OUTPUT BY 'DDICT'.
!                             (EACH TIME IS YYYY, MM, DD, HHMM.) THE
!                             WINDOW MAY BE NARROWED BY 'GRIBINDX'.
!                ISTAT   I/O  RETRIEVAL REQUEST STATUS:
!                               0  (INPUT)  NEW RETRIEVAL REQUEST,
!                                  (OUTPUT) RETRIEVAL COMPLETED,
!                               4  (INPUT)  CONTINUATION OF RETRIEVAL,
!                                  (OUTPUT) MORE DATA STILL TO COME,
!                               8  (OUTPUT) NO DATA AVAILABLE,
!                              16  (OUTPUT) FATAL ERROR IN RETRIEVAL.
!                CREP     O   (CHARACTER*(*)) ARRAY TO HOLD RETRIEVED
!                             GRIB DATA.
!                RVALS    O   (REAL ARRAY) NTH VALUE RETURNS THE
!                             NUMBER OF BYTES OF DATA IN CREP(N).
!
! CALLED BY    : MDB
!
! CALLS        : GRIBFIND, GRIBINDX, GRIBREAD
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /data/us0400/mdb/op/lib/source/RCS/gribret.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.2  2003/03/05  16:23:12  16:23:12  usmdb (MetDB account c/o usjh)
! Changed KEYWRD(34) to KEYWRD(*). Added new argument
! ISELECT - S.Cox ... also passed to GRIBFIND - B Barwell
!
! Revision 2.1  2001/08/09  09:26:14  09:26:14  usmdb (Generic MetDB account)
! 2.1.  20 August 2001.  Brian Barwell.  Change 105/01.
! Add KEYWRD array to argument and also to call to GRIBFIND.
!
! Revision 2.0  2001/01/08  11:58:44  11:58:44  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.1  2000/12/08  14:17:30  14:17:30  usmdb (Generic MDB account)
! Initial revision
!
! ORIGINAL VERSION BY BRIAN BARWELL, AUGUST 2000.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE
!                                                            PARAMETERS
!
INTEGER    MAXREC   ! MAXIMUM RECORD LENGTH FOR GRIB DATA SET
PARAMETER (MAXREC=9216)
!                                                             VARIABLES
!
INTEGER IDATA(5)  ! INFORMATION ABOUT INDEX DATA SET (SEE ABOVE)
INTEGER ISELECT(50) ! USER'S LIST OF DATA SELECTION VALUES    !3
INTEGER ISTAT     ! STATUS CODE RETURNED BY THIS ROUTINE
INTEGER ITIME(8)  ! START AND END TIMES OF REQUEST PERIOD
INTEGER IUNIT     ! UNIT NUMBER FOR GRIB DATA SET
INTEGER J         ! GENERAL LOOP VARIABLE
INTEGER KODE      ! RETURN CODE FROM SUBROUTINES CALLED
INTEGER LEFT      ! NUMBER OF BYTES OF MESSAGE LEFT TO TRANSFER
INTEGER LENREC    ! RECORD LENGTH OF GRIB DATA SET
INTEGER NCOPIED   ! NUMBER OF BYTES COPIED FROM 'BUFFER' SO FAR
INTEGER NEMPTY    ! UNFILLED SPACE IN ELEMENT OF USER BUFFER
INTEGER NEXTIN    ! NEXT BYTE TO COPY FROM 'BUFFER'
INTEGER NEXTOUT   ! NEXT BYTE TO COPY TO IN USER BUFFER
INTEGER NFILLED   ! FILLED SPACE IN ELEMENT OF USER BUFFER
INTEGER NHHMM1    ! HOURS AND MINUTES ('HHMM') FOR START TIME
INTEGER NHHMM2    ! HOURS AND MINUTES ('HHMM') FOR END TIME
INTEGER NLAST     ! NUMBER OF WANTED BYTES IN 'BUFFER'
INTEGER NLMNT     ! CURRENT ELEMENT OF USER BUFFER
INTEGER NOBS      ! NO. OF ELEMENTS IN USER BUFFER
INTEGER NSIZE     ! SIZE OF EACH ELEMENT OF USER BUFFER
INTEGER N2COPY    ! NUMBER OF BYTES STILL TO COPY FROM 'BUFFER'
!
REAL RVALS(*)     ! NUMBER OF OUTPUT BYTES IN EACH 'CREP' ELEMENT
!
LOGICAL COPYING   ! .TRUE. IF DATA TRANSFER IS IN PROGRESS
LOGICAL FIRST     ! .TRUE. IF FIRST CALL TO SUBROUTINE
LOGICAL FOUND     ! .TRUE. IF SOME DATA RETURNED TO USER THIS CALL
LOGICAL KEYWRD(*) ! MDB KEYWORDS (= 'FOUND' ARRAY IN 'MDB')   !2.2
LOGICAL LTEST     ! .TRUE. IF DIAGNOSTIC PRINTOUT IS REQUIRED
LOGICAL NEWDATA   ! .TRUE. IF NO GRIB DATA SET IS OPEN
LOGICAL NEWRQST   ! .TRUE. IF THIS CALL IS A NEW RETRIEVAL REQUEST
!
CHARACTER*(MAXREC) BUFFER  ! INTERNAL BUFFER FOR 1 GRIB RECORD
CHARACTER*(*) CREP(NOBS)   ! USER BUFFER TO HOLD GRIB DATA
CHARACTER*(*) DATYPE       ! MET.D.B.DATA TYPE
CHARACTER*(*) DSNX         ! NAME OF INDEX DATA SET
CHARACTER*132 HEAD         ! FOR REVISION INFORMATION
CHARACTER*8  OLDTYPE       ! MET.D.B. DATA TYPE FOR PREVIOUS CALL
!
!                                                   DATA INITIALISATION
DATA OLDTYPE /' '/, NEWDATA, FIRST /2*.TRUE./
!                                                                 SAVES
!
SAVE FIRST, LEFT, LENREC, NCOPIED, NLAST, NEWDATA, OLDTYPE, BUFFER
!
!-----------------------------------------------------------------------
!  REVISION INFORMATION (FIRST CALL ONLY)
!-----------------------------------------------------------------------
!
IF (FIRST) THEN
   HEAD='&
   & $Source: /data/us0400/mdb/op/lib/source/RCS/gribret.f,v $&
   & '//'$Date: 26/01/2010 10:18:13$ $Revision: 1$'
   FIRST = .FALSE.
END IF
!
!-----------------------------------------------------------------------
!  CHECK FOR NEW RETRIEVAL REQUEST
!-----------------------------------------------------------------------
!
IF (ISTAT.EQ.0) THEN ! NEW REQUEST
   NEWRQST = .TRUE.
   NEWDATA = .TRUE.
!  CHECK FOR INVALID STATUS CODE
ELSE IF (ISTAT.NE.4) THEN
   WRITE (6,'(T5,A,T15,A,I4)') 'GRIBRET:',&
            &'INVALID INPUT STATUS CODE - ', ISTAT
   ISTAT = 16
   RETURN
END IF
!
!-----------------------------------------------------------------------
!  INITIALISATIONS
!-----------------------------------------------------------------------
!                                      INITIALISE USER BUFFER TO ZEROES
NSIZE = LEN(CREP(1))
!                                          (1) FIRST ELEMENT
DO J=1,NSIZE
   CREP(1)(J:J) = CHAR(0)
END DO ! J
RVALS(1) = 0
!                                          (2) OTHER ELEMENTS
DO J=2,NOBS
   CREP(J) = CREP(1)
   RVALS(J) = 0
END DO ! JLMNT
!                                              INITIALISE POINTERS ETC.
NLMNT = 1
NFILLED = 0
COPYING = .TRUE.
FOUND = .FALSE.
!                              CONVERT HOURS FROM 'HHMM' TO 'HH' FORMAT
NHHMM1 = ITIME(4)
NHHMM2 = ITIME(8)
ITIME(4) = ITIME(4)/100
ITIME(8) = ITIME(8)/100
!                                               CHECK FOR NEW DATA TYPE
IF (DATYPE.NE.OLDTYPE) THEN
   NEWDATA = .TRUE.
   CALL GRIBINDX (DSNX, IDATA, ITIME, KODE)
!                                                     CHECK RETURN CODE
   IF (KODE.EQ.4) THEN
      OLDTYPE = DATYPE
   ELSE
      COPYING = .FALSE.  ! FORCES QUICK EXIT
      OLDTYPE = ' '
   END IF
END IF
!
!=======================================================================
!  LOOP UNTIL ALL DATA HAS BEEN TRANSFERRED OR USER'S BUFFER IS FILLED
!=======================================================================
!
DO WHILE (COPYING)
!                               IF NO DATA SET OPEN, LOOK FOR A NEW ONE
   IF (NEWDATA) THEN
      IF (NEWRQST) THEN
         KODE = -1         ! INDICATES NEW REQUEST
         NEWRQST = .FALSE.
      ELSE
         KODE = ISTAT
      END IF
!
      CALL GRIBFIND (DATYPE, IDATA, ITIME, ISELECT, KEYWRD,&  !2.2
                    &LTEST, KODE, IUNIT, LENREC)              !2.2
!
!                                         'KODE'=4: NEW DATA SET OPENED
      IF (KODE.EQ.4) THEN
         LEFT = -1
         CALL GRIBREAD&
             &(IUNIT, LENREC, LEFT, NCOPIED, BUFFER, KODE)
         IF (KODE.EQ.16) THEN
            COPYING = .FALSE.  ! FORCES QUICK EXIT
         ELSE IF (KODE.LT.8) THEN
            NLAST = MIN0(LEFT+NCOPIED,LENREC)
            NEWDATA = .FALSE.
         END IF
!                                           'KODE'=8: NO MORE DATA SETS
      ELSE
         COPYING = .FALSE.
      END IF
!                                                  POINTERS FOR BUFFERS
   ELSE
      FOUND = .TRUE.
      NEXTIN  = NCOPIED + 1
      NEXTOUT = NFILLED + 1
      NEMPTY = NSIZE - NFILLED
      N2COPY = NLAST - NCOPIED
!
!-----------------------------------------------------------------------
!  FILL CURRENT ELEMENT OF USER'S BUFFER WITH DATA FROM TRANSFER BUFFER
!-----------------------------------------------------------------------
!
      IF (NEMPTY.LT.N2COPY) THEN
         NCOPIED = NCOPIED + NEMPTY
         CREP(NLMNT)(NEXTOUT:NSIZE) = BUFFER(NEXTIN:NCOPIED)
         RVALS(NLMNT) = NSIZE
         LEFT = LEFT - NEMPTY
!                                          INCREMENT ELEMENT NUMBER ...
         IF (NLMNT.LT.NOBS) THEN
            NLMNT = NLMNT + 1
            NFILLED = 0
!                                    ... OR STOP IF ALL ELEMENTS FILLED
         ELSE
            KODE = 4  ! (MORE DATA TO COME)
            COPYING = .FALSE.
         END IF
!
!-----------------------------------------------------------------------
!  EMPTY DATA FROM TRANSFER BUFFER INTO USER'S BUFFER & READ MORE IN
!-----------------------------------------------------------------------
!
      ELSE
         NFILLED = NFILLED + N2COPY
         CREP(NLMNT)(NEXTOUT:NFILLED) = BUFFER(NEXTIN:NLAST)
         RVALS(NLMNT) = NFILLED
         LEFT = LEFT - N2COPY
!                                              READ ANOTHER DATA RECORD
         IF (LEFT.GT.0) THEN
            CALL GRIBREAD&
                &(IUNIT, LENREC, LEFT, NCOPIED, BUFFER, KODE)
!
!                                                     CHECK RETURN CODE
            IF (KODE.EQ.16) THEN
               COPYING = .FALSE.     ! FORCES QUICK EXIT
            ELSE IF (KODE.EQ.8) THEN
               CLOSE (IUNIT)
               NEWDATA = .TRUE.      ! LOOK FOR NEXT DATA SET
            ELSE
               NLAST = MIN0(LEFT+NCOPIED,LENREC)
            END IF
!                                            CLOSE FILE IF NO DATA LEFT
         ELSE
            CLOSE (IUNIT)
            NEWDATA = .TRUE.
         END IF
      END IF
   END IF
END DO
!
!=======================================================================
!  SET RETRIEVAL STATUS CODE.
!=======================================================================
!            ('KODE'=8 MEANS NO DATA FOUND THIS CALL BUT DATA MAY HAVE
!              BEEN FOUND BY PREVIOUS CALLS AS INDICATED BY 'FOUND'.)
!
IF (FOUND .AND. KODE.EQ.8) THEN
   ISTAT = 0
ELSE
   ISTAT = KODE
END IF
!                         RESTORE 'ITIME' AND RETURN TO CALLING PROGRAM
ITIME(4) = NHHMM1
ITIME(8) = NHHMM2
RETURN
END SUBROUTINE GRIBRET
