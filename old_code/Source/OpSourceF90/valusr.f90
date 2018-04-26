SUBROUTINE VALUSR(MESSAGE,NELEM,DISPL,WIDTH,SCALE,REFVAL,&
&ARRAY,NOB,NOBS,CSTR,CREP,CREPRT,LFLAG,ISECT1,&
&CHRELM,LENCHR)                                !F

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
! parameters    : (1) BUFR message
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
!                (13) true for diagnostics                            !A
!                (14) array of integer values to put in users array   !B
!                (15) character elements ("    ","     ")             !F
!                (16) lengths for each of CHRELM                      !F
!
!Y2K  26.06.1997  VALUSR is Year 2000 compliant.
!
! Revision Info :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/valusr.F,v $
!
! Change Record :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:59:28  usmdb
! Moved declarations of NOBS and NELEM before declarations
! of arrays of size NOBS and/or NELEM. Added copyright and
! modified header - S.Cox
!
! Revision 1.8  99/01/14  14:16:01  14:16:01  usmdb (Generic MDB account)
! 18-01-1999 S.Cox
! Function VALUE changes the 2 argument passed to it. Change variables passed
! to VALUE so there is no adverse affect from this.
!
! Revision 1.7  98/10/07  16:04:17  16:04:17  usmdb (Generic MDB account)
! 19-10-98 S.Cox
! Correct Y2K problem in VALUSR where the TOR REFVAL in the element
! index = 1900. Check the BUFR value and call function CENTURY to add
! either 1900 or 2000.
!
! Revision 1.6  98/07/23  08:41:30  08:41:30  usmdb (Generic MDB account)
! incease length of report text to be 5 chars
!
! Revision 1.5  97/09/10  15:39:56  15:39:56  uspm (Pat McCormack)
! 2 new arguments passed in- CHRELM and LENCHR.
!
! Revision 1.4  1997/08/04 13:43:48  uspm
! First revisioned version for COSMOS - with Y2K changes
!
! Revision 1.3  1997/07/09 08:30:14  uspm
! Latest version from COSMOS - changes marked !D
!
! Revision 1.2  1997/02/20 13:50:10  uspm
! Latest version (!c) from COSMOS
!
! Revision 1.1  1997/02/12 09:35:35  uspm
! Initial revision
!
! 20-07-98 !G : Length of raw report text is now 5 characters instead
!             : of 4 for retrieval of BUFR messages - S.Cox
!
! 15-09-97 !F : 2 new arguments passed in. CHRELM and LENCHR. This
!             : means character names as set up in the calling program
!             : can be put into the user's CSTR string - S.Cox
!
! 28-07-97 !E : Change to allow the ASCII to EBCDIC conversion of BUFR
!             : character elements even when the user has not allowed
!             : enough room in CSTR. Print out a warning message to
!             : indicate this - S.Cox
!
! 19-05-97 !D : Correction to change !C. Only scale rain or snow traces
!             : if the BUFR element value if less than zero - S.Cox
!
! 07-02-97 !C : Don't scale rain or snow traces, leave them as -1 or -2
!             : This is risky as we can't check for class 13 - C.Long
!
! 03-10-96 !B : S.Cox - Add a section to look for elements that can be
!             : set in an integer array i.e. ISECT1. Looks for
!             : displacements (.lt.0) .and. (.gt.-99). Also added extra
!             : error checking for CREPRT and CSTR. Strings only get
!             : transferred to these user strings if there is room for
!             : them.
!
! 31-07-96 !A : S.Needham - Just add diagnostics option
!
! 22-05-96    : S.Cox - Correction to cstr variable. now being correctly
!             : used as an array of values.
!
!   Feb 95    : Made as equivalent of trnsfr for streamlined retrieval
!             : (though TRNSFR can handle several reports at a time)
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

IMPLICIT NONE

INTEGER       NOBS                                            !2.0
INTEGER       NELEM                                           !2.0

CHARACTER*(*) MESSAGE
CHARACTER*(*) CSTR(NOBS)
CHARACTER*(*) CREP
CHARACTER*(*) CREPRT(NOBS)
CHARACTER*(*) CHRELM(*)      !- names to put in users string.   !F
CHARACTER*132 HEAD
INTEGER       DISPL(*)
INTEGER       WIDTH(*)
INTEGER       SCALE(*)
INTEGER       REFVAL(*)
REAL          ARRAY(NOBS,NELEM)
INTEGER       TENTO(0:9)
INTEGER       TWOTO(0:30)
LOGICAL       LFLAG
LOGICAL       ONEWARNING                                        !E
INTEGER       LREP,LENCH
INTEGER       I,J
INTEGER       NOB                                             !2.0
INTEGER       IBEFOR,IVAL
INTEGER       IL,IE1,IE2                                        !F
INTEGER       VALUE
INTEGER       NCHARS
INTEGER       ISECT1(*)         ! integer elements array        !B
INTEGER       LCREPRT           ! length of CREPRT              !B
INTEGER       LCSTR             ! length of CSTR                !B
INTEGER       LENCHR(*)         ! lengths of CHRELM elements    !F

INTEGER       CENTURY           ! Y2K year function           !1.7

 DATA TENTO/1,10,100,1000,10000,100000,1000000,10000000,&
&100000000,1000000000/
 DATA TWOTO/1,2,4,8,16,32,64,128,256,512,1024,2048,&
&4096,8192,16384,32768,65536,131072,262144,524288,1048576,&
&2097152,4194304,8388608,16777216,33554432,67108864,134217728,&
&268435456,536870912,1073741824/

 DATA ONEWARNING /.FALSE./                                       !E

!-----------------------------------------------------------------------
! Ensure that all variables/arrays are still set on re-entry.
!-----------------------------------------------------------------------
! Declare string containing keywords used by HP utility RCS, ident to
! allow automatic updating of files on checkin with change details and
! identification of version of version used to make obj, exe
!------------------------------------------------------------------------

SAVE                                                            !E

HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/valusr.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

NCHARS=1                        ! pointer to character string

IF (LFLAG) WRITE(*,*)'In VALUSR: NELEM = ',NELEM

DO I=1,NELEM                    ! loop round elements
 IF (DISPL(I).EQ.-9999999) THEN ! if element not found,
   ARRAY(NOB,I)=-9999999.       ! return missing value
 ELSE IF (DISPL(I).EQ.-99) THEN ! report text wanted

!-----------------------------------------------------------------------
! Add a check on the length of the user's report text string. Only
! transfer as many characters as will fit!  !B S.Cox
!-----------------------------------------------------------------------

   READ(CREP(1:5),'(I5)')LREP   ! get length before header      !G
   LCREPRT=LEN(CREPRT(NOB))             ! length of CREPRT      !B
   IF (LREP.LE.LCREPRT) THEN                                    !B
     CREPRT(NOB)(1:LREP)=CREP(6:LREP+5) ! copy header & report  !G
   ELSE                                                         !B
     CREPRT(NOB)(1:LCREPRT)=CREP(6:LCREPRT+5)                 !G!B
   ENDIF                                                        !B
   ARRAY(NOB,I)=LREP              ! & return length in array.

!-----------------------------------------------------------------------
! Add a section to look for data in the integer array ISECT1 passed in
! to put in the users array. S.Cox !B
!-----------------------------------------------------------------------

 ELSEIF (DISPL(I).LT.0 .AND. DISPL(I).GT.-99) THEN              !B

!-----------------------------------------------------------------------
! If DISPL(I).EQ.-1, the data is from the integer array ISECT1. If
! DISPL(I).EQ.-2, the data is from the character array CHRELM passed
! in from the calling program.                                        !F
!-----------------------------------------------------------------------

   IF (DISPL(I).EQ.-1) THEN                                     !F
     ARRAY(NOB,I)=ISECT1(REFVAL(I))                           !F!B
   ELSEIF (DISPL(I).EQ.-2) THEN                                 !F
     IVAL=REFVAL(I)                                             !F
     IL=LENCHR(IVAL)                                            !F
     IE1=NCHARS                                                 !F
     IE2=IE1+IL-1                                               !F
     ARRAY(NOB,I)=IL*65536+NCHARS                               !F
     LCSTR=LEN(CSTR(NOB))                                       !F
     IF (IE2.LE.LCSTR) THEN                                     !F
       CSTR(NOB)(IE1:IE2)=CHRELM(IVAL)(1:IL)                    !F
     ENDIF                                                      !F
     NCHARS=NCHARS+IL                                           !F
   ENDIF                                                        !F

!-----------------------------------------------------------------------
! assume character element if length is a multiple of 8 and at least 32.
! extract the value byte by byte, convert it to ebcdic, return length &
! pointer to user's character string in the real array.
!-----------------------------------------------------------------------

 ELSE IF (MOD(WIDTH(I),8).EQ.0 .AND. WIDTH(I).GE.32) THEN
   LENCH=WIDTH(I)/8             ! number of characters

   IF (LFLAG) THEN
     WRITE(*,*)'In VALUSR: Getting string: ',LENCH
     WRITE(*,*)'In VALUSR: MSG= ',MESSAGE(DISPL(I):DISPL(I)+LENCH)
   ENDIF

!-----------------------------------------------------------------------
! check added to only transfer characters to CSTR array if there is
! room  !B S.Cox
!-----------------------------------------------------------------------

   LCSTR=LEN(CSTR(NOB))             ! length of CSTR            !B

   DO J=0,LENCH-1                   ! get them 8 bits at a time
     IF ((NCHARS+J).LE.LCSTR) THEN                              !B
       IBEFOR=DISPL(I)+J*8                                    !1.8
       CSTR(NOB)(NCHARS+J:NCHARS+J)=&
      &CHAR(VALUE(MESSAGE,IBEFOR,8))                          !1.8
     ENDIF                                                      !B
   END DO

!-----------------------------------------------------------------------
! characters are in ascii in message itself, in ebcdic in trailer:
! translate if first character isn't ebcdic letter or figure.
!-----------------------------------------------------------------------

   IF ((NCHARS+LENCH-1).LE.LCSTR) THEN                          !B
     IF (ICHAR(CSTR(NOB)(NCHARS:NCHARS)).LT.192) THEN
       CALL ASC2EB(LENCH,CSTR(NOB)(NCHARS:NCHARS+LENCH-1))
     ENDIF

   ELSE                                                         !E

     IF (.NOT.ONEWARNING) THEN
       ONEWARNING=.TRUE.
       WRITE(*,*)'MDB WARNING: In VALUSR: NOT ENOUGH ROOM IN '  !E
       WRITE(*,*)'USER STRING CSTR - RESPECIFY SIZE '           !E
     ENDIF

     IF (NCHARS.LE.LCSTR) THEN                                  !E
       IF (ICHAR(CSTR(NOB)(NCHARS:NCHARS)).LT.192) THEN         !E
         CALL ASC2EB(LCSTR,CSTR(NOB)(NCHARS:NCHARS+LCSTR-1))    !E
       ENDIF                                                    !E
     ENDIF                                                      !E
   ENDIF                                                        !B

   ARRAY(NOB,I)=LENCH*65536+NCHARS
   NCHARS=NCHARS+LENCH          ! move pointer past this string

!-----------------------------------------------------------------------
! If it's not a character element, use SCALE & REFVAL to set value in
! user's array.
!-----------------------------------------------------------------------

 ELSE
   IBEFOR=DISPL(I)              ! call to value changes IBEFOR
   IVAL=VALUE(MESSAGE,IBEFOR,WIDTH(I))
   IF (IVAL.EQ.TWOTO(WIDTH(I))-1) THEN
     ARRAY(NOB,I)=-9999999.     ! missing if all ones in message
   ELSE

!-----------------------------------------------------------------------
! If REFVAL = 1900 - this is to be added to the TOR value. However this
! presents a Y2K problem. There is only a REFVAL of 1900 for a TOR
! year, so this check is safe. Call function CENTURY to add either
! 1900 or 2000.
!-----------------------------------------------------------------------

     IF (REFVAL(I).EQ.1900) THEN                              !1.7
       IVAL=IVAL+CENTURY(IVAL)                                !1.7
       ARRAY(NOB,I)=IVAL                                      !1.7
     ELSE                                                     !1.7
       ARRAY(NOB,I)=IVAL+REFVAL(I)                            !1.7
     ENDIF                                                    !1.7

!-----------------------------------------------------------------------
! Scale observation as long as REFVAL is not -1 or -2 (see !C and !D)
!-----------------------------------------------------------------------

     IF (REFVAL(I).NE.-1 .AND. REFVAL(I).NE.-2) THEN            !C
       IF (SCALE(I).GT.0) THEN
         ARRAY(NOB,I)=ARRAY(NOB,I)/TENTO(SCALE(I))
       ELSE IF (SCALE(I).LT.0) THEN
         ARRAY(NOB,I)=ARRAY(NOB,I)*TENTO(-SCALE(I))
       ENDIF

     ELSE                                                       !D

!-----------------------------------------------------------------------
! If REFVAL is -1 or -2, we could have a rain or snow trace. This
! check is risky as we can't check that the class is 13. Scale the    !C
! rain or snow amount if it is greater than 0, otherwise it it a      !C
! trace, so we don't want to scale it.                                !C
!-----------------------------------------------------------------------

       IF (ARRAY(NOB,I).GT.0) THEN                              !D
         IF (SCALE(I).GT.0) THEN                                !D
           ARRAY(NOB,I)=ARRAY(NOB,I)/TENTO(SCALE(I))            !D
         ELSE IF (SCALE(I).LT.0) THEN                           !D
           ARRAY(NOB,I)=ARRAY(NOB,I)*TENTO(-SCALE(I))           !D
         ENDIF                                                  !D
       ENDIF                                                    !D
     ENDIF                                                      !D

   ENDIF
 ENDIF
END DO

RETURN
END SUBROUTINE VALUSR
