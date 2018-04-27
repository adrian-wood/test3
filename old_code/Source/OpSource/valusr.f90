SUBROUTINE VALUSR(MESSAGE,NELEM,DISPL,WIDTH,SCALE,REFVAL, &
                  ARRAY,NOB,NOBS,CSTR,CREP,CREPRT,LFLAG,ISECT1, &
                  CHRELM,LENCHR)

!-----------------------------------------------------------------------
!
! program       : VALUSR in MDB retrieval
!
! purpose       : To get specified values from a bufr message (single
!                 report, no compression) & put them in a user's array
!
! called by     : MDB Retrieval : SYNRET,UPRRET,UPRPARTS
!               : MDB Storage   : SYNSEQ, SHPSEQ
!
! calls         : VALUE, ASC2EB
!
! arguments     : (1) BUFR message
!                 (2) number of values wanted
!                 (3) table column: bits to skip before value
!                     (-9999999 if missing, -99 for report text)
!                 (4) table column: bits in value
!                 (5) table column: bufr scale
!                 (6) table column: bufr reference value
!                 (7) user's array                           (output)
!                 (8) next ob to go in user's array (subscript)
!                 (9) obs dimension of user's array
!                (10) user's character string                (output)
!                (11) report text with length & header at start
!                (12) user's array for report text           (output)
!                (13) true for diagnostics
!                (14) array of integer values to put in users array
!                (15) character elements ("    ","     ")
!                (16) lengths for each of CHRELM
!
!Y2K  26.06.1997  VALUSR is Year 2000 compliant.
!
! REVISION INFO:
!
! $Workfile: valusr.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 09/02/2011 17:35:14$
!
! Change Record :
!
! $Log:
!  3    MetDB_Refresh 1.2         09/02/2011 17:35:14    Sheila Needham  Use
!       int2ch function
!  2    MetDB_Refresh 1.1         17/11/2010 09:58:48    John Norton
!       Updated after doing rework for batch 13.
!  1    MetDB_Refresh 1.0         09/11/2010 11:35:01    John Norton     MetDB
!       Refresh created file  
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

USE asc2eb_mod
USE century_mod  !function
USE value_mod    !function
USE int2ch_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(INOUT) ::  MESSAGE
INTEGER,      INTENT(INOUT) ::  NELEM
INTEGER,      INTENT(INOUT) ::  DISPL(:)
INTEGER,      INTENT(INOUT) ::  WIDTH(:)
INTEGER,      INTENT(INOUT) ::  SCALE(:)
INTEGER,      INTENT(INOUT) ::  REFVAL(:)
INTEGER,      INTENT(INOUT) ::  NOBS
REAL,         INTENT(OUT)   ::  ARRAY(NOBS,NELEM)
INTEGER,      INTENT(IN)    ::  NOB
CHARACTER(*), INTENT(OUT)   ::  CSTR(NOBS)
CHARACTER(*), INTENT(IN)    ::  CREP
CHARACTER(*), INTENT(OUT)   ::  CREPRT(NOBS)
LOGICAL,      INTENT(IN)    ::  LFLAG
INTEGER,      INTENT(INOUT) ::  ISECT1(:) ! integer elements array
CHARACTER(*), INTENT(INOUT) ::  CHRELM(:) !- names to put in users string.
INTEGER,      INTENT(INOUT) ::  LENCHR(:) ! lengths of CHRELM elements

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  I,J
INTEGER      ::  IBEFOR
INTEGER      ::  IE1
INTEGER      ::  IE2
INTEGER      ::  IL
INTEGER      ::  IVAL
INTEGER      ::  LCREPRT        ! length of CREPRT
INTEGER      ::  LCSTR          ! length of CSTR
INTEGER      ::  LENCH
INTEGER      ::  LREP
INTEGER      ::  NCHARS
LOGICAL      ::  ONEWARNING = .FALSE.
INTEGER      ::  TENTO(0:9)
INTEGER      ::  TWOTO(0:30)

DATA TENTO/1,10,100,1000,10000,100000,1000000,10000000, &
           100000000,1000000000/
DATA TWOTO/1,2,4,8,16,32,64,128,256,512,1024,2048, &
 4096,8192,16384,32768,65536,131072,262144,524288,1048576, &
 2097152,4194304,8388608,16777216,33554432,67108864,134217728, &
 268435456,536870912,1073741824/

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Ensure that all variables/arrays are still set on re-entry.
!-----------------------------------------------------------------------
! Declare string containing keywords used by HP utility RCS, ident to
! allow automatic updating of files on checkin with change details and
! identification of version of version used to make obj, exe
!------------------------------------------------------------------------

SAVE

NCHARS=1                        ! pointer to character string

IF (LFLAG) WRITE(*,*)'In VALUSR: NELEM = ',NELEM

DOLABEL1: &
DO I=1,NELEM                    ! loop round elements
IFLABEL1: &
 IF (DISPL(I) == -9999999) THEN ! if element not found,
   ARRAY(NOB,I)=-9999999.       ! return missing value
 ELSE IF (DISPL(I) == -99) THEN ! report text wanted

!-----------------------------------------------------------------------
! Add a check on the length of the user's report text string. Only
! transfer as many characters as will fit!  !B S.Cox
!-----------------------------------------------------------------------

   READ(CREP(1:5),'(I5)')LREP   ! get length before header
   LCREPRT=LEN(CREPRT(NOB))             ! length of CREPRT
   IF (LREP <= LCREPRT) THEN
     CREPRT(NOB)(1:LREP)=CREP(6:LREP+5) ! copy header & report
   ELSE
     CREPRT(NOB)(1:LCREPRT)=CREP(6:LCREPRT+5)
   END IF
   ARRAY(NOB,I)=LREP              ! & return length in array.

!-----------------------------------------------------------------------
! Add a section to look for data in the integer array ISECT1 passed in
! to put in the users array. S.Cox !B
!-----------------------------------------------------------------------

 ELSE IF (DISPL(I) < 0 .AND. DISPL(I) > -99) THEN

!-----------------------------------------------------------------------
! If DISPL(I) == -1, the data is from the integer array ISECT1. If
! DISPL(I) == -2, the data is from the character array CHRELM passed
! in from the calling program.
!-----------------------------------------------------------------------

IFLABEL2: &
   IF (DISPL(I) == -1) THEN
     ARRAY(NOB,I)=ISECT1(REFVAL(I))
   ELSE IF (DISPL(I) == -2) THEN
     IVAL=REFVAL(I)
     IL=LENCHR(IVAL)
     IE1=NCHARS
     IE2=IE1+IL-1
     ARRAY(NOB,I)=IL*65536+NCHARS
     LCSTR=LEN(CSTR(NOB))
     IF (IE2 <= LCSTR) THEN
       CSTR(NOB)(IE1:IE2)=CHRELM(IVAL)(1:IL)
     END IF
     NCHARS=NCHARS+IL
   END IF IFLABEL2

!-----------------------------------------------------------------------
! assume character element if length is a multiple of 8 and at least 32.
! extract the value byte by byte, convert it to ebcdic, return length &
! pointer to user's character string in the real array.
!-----------------------------------------------------------------------

 ELSE IF (MOD(WIDTH(I),8) == 0 .AND. WIDTH(I) >= 32) THEN
   LENCH=WIDTH(I)/8             ! number of characters

   IF (LFLAG) THEN
     WRITE(*,*)'In VALUSR: Getting string: ',LENCH
     WRITE(*,*)'In VALUSR: MSG= ',MESSAGE(DISPL(I):DISPL(I)+LENCH)
   END IF

!-----------------------------------------------------------------------
! check added to only transfer characters to CSTR array if there is
! room  !B S.Cox
!-----------------------------------------------------------------------

   LCSTR=LEN(CSTR(NOB))             ! length of CSTR

   DO J=0,LENCH-1                   ! get them 8 bits at a time
     IF ((NCHARS+J) <= LCSTR) THEN
       IBEFOR=DISPL(I)+J*8
       CSTR(NOB)(NCHARS+J:NCHARS+J)= &
       int2ch(VALUE(MESSAGE,IBEFOR,8))
     END IF
   END DO

!-----------------------------------------------------------------------
! characters are in ascii in message itself, in ebcdic in trailer:
! translate if first character isn't ebcdic letter or figure.
!-----------------------------------------------------------------------

IFLABEL3: &
   IF ((NCHARS+LENCH-1) <= LCSTR) THEN
     IF (ICHAR(CSTR(NOB)(NCHARS:NCHARS)) < 192) THEN
       CALL ASC2EB(LENCH,CSTR(NOB)(NCHARS:NCHARS+LENCH-1))
     END IF

   ELSE

     IF (.NOT.ONEWARNING) THEN
       ONEWARNING=.TRUE.
       WRITE(*,*)'MDB WARNING: In VALUSR: NOT ENOUGH ROOM IN '
       WRITE(*,*)'USER STRING CSTR - RESPECIFY SIZE '
     END IF

     IF (NCHARS <= LCSTR) THEN
       IF (ICHAR(CSTR(NOB)(NCHARS:NCHARS)) < 192) THEN
         CALL ASC2EB(LCSTR,CSTR(NOB)(NCHARS:NCHARS+LCSTR-1))
       END IF
     END IF
   END IF IFLABEL3

   ARRAY(NOB,I)=LENCH*65536+NCHARS
   NCHARS=NCHARS+LENCH          ! move pointer past this string

!-----------------------------------------------------------------------
! If it's not a character element, use SCALE & REFVAL to set value in
! user's array.
!-----------------------------------------------------------------------

 ELSE
   IBEFOR=DISPL(I)              ! call to value changes IBEFOR
   IVAL=VALUE(MESSAGE,IBEFOR,WIDTH(I))
IFLABEL4: &
   IF (IVAL == TWOTO(WIDTH(I))-1) THEN
     ARRAY(NOB,I)=-9999999.     ! missing if all ones in message
   ELSE

!-----------------------------------------------------------------------
! If REFVAL = 1900 - this is to be added to the TOR value. However this
! presents a Y2K problem. There is only a REFVAL of 1900 for a TOR
! year, so this check is safe. Call function CENTURY to add either
! 1900 or 2000.
!-----------------------------------------------------------------------

     IF (REFVAL(I) == 1900) THEN
       IVAL=IVAL+CENTURY(IVAL)
       ARRAY(NOB,I)=IVAL
     ELSE
       ARRAY(NOB,I)=IVAL+REFVAL(I)
     END IF

!-----------------------------------------------------------------------
! Scale observation as long as REFVAL is not -1 or -2 (see .f77 !C and !D)
!-----------------------------------------------------------------------

IFLABEL5: &
     IF (REFVAL(I) /= -1 .AND. REFVAL(I) /= -2) THEN
       IF (SCALE(I) > 0) THEN
         ARRAY(NOB,I)=ARRAY(NOB,I)/TENTO(SCALE(I))
       ELSE IF (SCALE(I) < 0) THEN
         ARRAY(NOB,I)=ARRAY(NOB,I)*TENTO(-SCALE(I))
       END IF

     ELSE

!-----------------------------------------------------------------------
! If REFVAL is -1 or -2, we could have a rain or snow trace. This
! check is risky as we can't check that the class is 13. Scale the
! rain or snow amount if it is greater than 0, otherwise it it a
! trace, so we don't want to scale it.
!-----------------------------------------------------------------------

       IF (ARRAY(NOB,I) > 0) THEN
         IF (SCALE(I) > 0) THEN
           ARRAY(NOB,I)=ARRAY(NOB,I)/TENTO(SCALE(I))
         ELSE IF (SCALE(I) < 0) THEN
           ARRAY(NOB,I)=ARRAY(NOB,I)*TENTO(-SCALE(I))
         END IF
       END IF
     END IF IFLABEL5

   END IF IFLABEL4
 END IF IFLABEL1
END DO DOLABEL1                 ! end loop round elements

RETURN
END SUBROUTINE VALUSR
