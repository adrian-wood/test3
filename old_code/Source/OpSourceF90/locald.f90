SUBROUTINE LOCALD(X,Y,SEQ,NSEQ,TEXT,MODE)

!-----------------------------------------------------------------------
!
! ROUTINE       : LOCALD
!
! PURPOSE       : to keep table b entries defined within a message
!               : for use later on in the decode; to find element
!               : details in these entries & in any local table b.
!
! DESCRIPTION   : free-form input as follows:
!               : a line starting with the table d descriptor, maybe
!               : followed by a blank line, then any number of
!               : lines defining the sequence, each starting with one
!               : or more descriptors delimited by commas or spaces.
!               : text can follow.  a blank line ends the sequence.
!               : if the input is not in the 5th argument, then a
!               : file 'localseq' will be read. N.B. there is no
!               : limit on the number of descriptors in these local
!               : sequences.
!
! CALLED BY     : DECODE (before TABLEB) & for any new MDB data set
!
! ARGUMENTS     : (1)    x                                    (input)
!               : (2)    y                                    (input)
!               : (3)    sequence of descriptors             (output)
!               : (4)    number of descriptors in sequence   (output)
!               : (5)    sequence of 80-byte lines in core    (input)
!               : (6)    mode ('add', 'new' or ' ')           (input)
!               : return nseq=0 if descriptor not in local table
!               : if x=y=0, just read in a (new) local table
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /data/us0400/mdb/op/lib/source/RCS/locald.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2002/04/09  11:44:57  11:44:57  usmdb (Generic MetDB account)
! Initialise NSEQ=0 at start of routine in case LOCALSEQ not
! found - S.Cox
!
! Revision 2.0  2001/03/07 10:19:17  usmdb
! Added copyright. Modified header and comments - S.Cox
!
! Revision 1.4  2000/08/25 14:51:34  usmdb
! Addition of preprocessor statement BPATH to allow users to code
! environment variable BUFR_LIBRARY on T3E and HP - S.Cox
!
! Revision 1.3  99/03/11  15:21:40  15:21:40  usmdb (Generic MDB account)
! 15/03/1999 S.Cox - ref MetDB problem 380
! for T3E version only, call BUFRPATH to read the BUFR table directory
! path from environment variable BUFR_LIBRARY.
!
! Revision 1.2  97/09/22  09:53:42  09:53:42  uspm (Pat McCormack)
! Change all labelled staements to be CONTINUE
!
! Revision 1.1  1997/06/19 13:41:38  uspm
! Initial revision
!
! Feb 97: increase descr dimension from 999 to 2000                   !a
!
! Jul 95: on the OPEN(81) statement, add FORM='FORMATTED' and on the
!         READ(81) statements, add a FORMAT statement. These changes
!         make the code more portable. (S.Cox)
!
! Jun 95: add SAVE statement, and change most non-standerd FORTRAN
!         statements in order to make it more readily portable. (S.Cox)
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

SAVE

INTEGER F,X,Y, SEQ(*), DESCR(2000), N                           !a
LOGICAL LOOKED, LOC
CHARACTER TEXT*(*),MODE*(*)
CHARACTER*80 LINE, SPACES
CHARACTER HEAD*132
CHARACTER FILENAME*208    ! LOCALSEQ full filename            !1.4

!if defined (BPATH)
!endif

DATA    N/0/
DATA    LOOKED/.FALSE./
DATA    LOC/.FALSE./
DATA    SPACES/' '/

HEAD='$RCSfile: $ ' //&
    &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'

NSEQ=0                                                        !2.1

!-----------------------------------------------------------------------
! if table not yet read in, put descriptors and counts in an array as
! follows: sequence descriptor, n, (descriptor(i),i=1,n) repeated.
!
! the first time, or if a new local table is to be used, see if string
! has been passed.  if not, look for a data set.  return if neither.
!-----------------------------------------------------------------------

IF (MODE.EQ.'ADD' .OR. MODE.EQ.'NEW' .OR. .NOT.LOOKED) THEN

  LOOKED=.TRUE.                ! looked for table (none if N=0)
  FILENAME = 'LOCALSEQ'                                       !1.4
  ISEQST=INDEX(TEXT,'3')       ! look for sequence in string

  IF (ISEQST.EQ.0) THEN
!if defined (BPATH)
!endif
    INQUIRE (FILE=FILENAME,EXIST=LOC)                         !1.4
    IF (.NOT.LOC) RETURN
    OPEN (81,FILE=FILENAME,FORM='FORMATTED')                  !1.4
  ENDIF

!-----------------------------------------------------------------------
! either read a line from the data set or take 80 bytes from the string
!-----------------------------------------------------------------------

  L=0                          ! line number
  LAST=0                       ! nonzero if last line
  IF (MODE.EQ.'NEW') N=0       ! count of descriptors in array
   10   CONTINUE
  ND=0                         ! count of descriptors in this seq
  IF (ISEQST.EQ.0) THEN
    READ (81,'(A80)',IOSTAT=LAST) LINE

  ELSE
    LINE=TEXT(L*80+1:L*80+80)
    L=L+1
  ENDIF

!-----------------------------------------------------------------------
! first find a line starting with a sequence descriptor: a 6-figure
! group with spaces before & after, 3nnnnn at start of line.
!-----------------------------------------------------------------------

  IF (LAST.EQ.0) THEN
    ISQ=INDEX(LINE,'3')
    IF (ISQ.EQ.0) GO TO 10
    IF (ISQ.GT.1 .AND. LINE(1:ISQ-1).NE.SPACES(1:ISQ-1)) GO TO 10
    IF (LINE(ISQ+6:ISQ+6).NE.SPACES(1:1)) GO TO 10

    DO 11 I=1,5
     IF (LINE(ISQ+I:ISQ+I).LT.'0') GO TO 10
     IF (LINE(ISQ+I:ISQ+I).GT.'9') GO TO 10
   11     CONTINUE

!-----------------------------------------------------------------------
! store 6-fig sequence descriptor as 16-bit descriptor in integer array.
!-----------------------------------------------------------------------

    READ (LINE(ISQ+1:ISQ+2),'(I2)') IX
    READ (LINE(ISQ+3:ISQ+5),'(I3)') IY
    DESCR(N+1)=3*16384+IX*256+IY

!-----------------------------------------------------------------------
! now find descriptors in sequence (at start of successive lines,
! maybe more than one on a line), checking for end of string.
!-----------------------------------------------------------------------

   20     CONTINUE
    IF (ISEQST.EQ.0) THEN
      READ (81,'(A80)',IOSTAT=LAST) LINE

    ELSE
      LINE=TEXT(L*80+1:L*80+80)
      L=L+1
      IND=INDEX(LINE,'END ')
      IF (IND.GT.0 .AND. LINE(:IND-1).EQ.SPACES(:IND-1)) LAST=4
    ENDIF
    IF (LAST.EQ.0 .AND. LINE.EQ.SPACES .AND. ND.EQ.0) GO TO 20

!-----------------------------------------------------------------------
! treat any 6-figure group as descriptor.  if not 6 figures, or
! character other than figure, space & comma found, read next line.
!-----------------------------------------------------------------------

    IF (LAST.EQ.0 .AND. LINE.NE.SPACES) THEN
      I=1
   21       CONTINUE
      IF (LINE(I:I).GE.'0' .AND. LINE(I:I).LE.'9') THEN
        DO 22 J=1,5
         IF (LINE(I+J:I+J).LT.'0') GO TO 20
         IF (LINE(I+J:I+J).GT.'9') GO TO 20
   22         CONTINUE

        READ (LINE(I:I),'(I1)') F
        READ (LINE(I+1:I+2),'(I2)') IX
        READ (LINE(I+3:I+5),'(I3)') IY

        ND=ND+1
        DESCR(N+2+ND)=F*16384+IX*256+IY
        I=I+5
      ELSE IF (LINE(I:I).NE.',' .AND. LINE(I:I).NE.' ') THEN
        GO TO 20
      ENDIF
      I=I+1
      IF (I.LT.75) GO TO 21
      GO TO 20
    ENDIF

!-----------------------------------------------------------------------
! if line has no descriptors, end the sequence, storing the count
! between the sequence descriptor & the descriptors in the sequence.
!-----------------------------------------------------------------------

    DESCR(N+2)=ND
    N=N+2+ND
    IF (LAST.EQ.0) GO TO 10
  ENDIF
  IF (ISEQST.EQ.0) CLOSE (81)
      ENDIF

!-----------------------------------------------------------------------
! finally (if there is a local table) look for the sequence requested.
!-----------------------------------------------------------------------

IF (N.GT.0 .AND. X.GT.0.AND.Y.GT.0) THEN
  I=0
   31   CONTINUE
  IF (DESCR(I+1).EQ.3*16384+X*256+Y) THEN
    NSEQ=DESCR(I+2)
    DO 32 J=1,NSEQ
     SEQ(J)=DESCR(I+2+J)
   32     CONTINUE
  ELSE
    I=I+2+DESCR(I+2)
    IF (I.LT.N) GO TO 31
  ENDIF
ENDIF

RETURN
END SUBROUTINE LOCALD
