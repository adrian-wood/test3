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
! $Workfile: locald.F90$ $Folder: OpSource$
! $Author: Alison Weir$ $Date: 09/03/2011 11:40:44$
!
! CHANGE RECORD :
!
! $Log:
!  13   MetDB_Refresh 1.12        09/03/2011 11:40:44    Alison Weir
!       Correct seq initialisation. Difficult to initialise complete array, so
!        only first element initialised.
!  12   MetDB_Refresh 1.11        07/03/2011 13:49:13    Alison Weir     seq
!       initialised (with parameter to give largest size)
!  11   MetDB_Refresh 1.10        21/02/2011 11:30:16    Alison Weir     (:)
!       changed to (*) for BUFRSHEL
!  10   MetDB_Refresh 1.9         21/12/2010 10:36:17    Sheila Needham
!       Correct initialisations
!  9    MetDB_Refresh 1.8         20/12/2010 16:45:41    Stan Kellett
!       INTENT(OUT) variables initialised at declaration. Ordered arguments
!       with intent together. Removed data statements and declared at
!       declaration.
!  8    MetDB_Refresh 1.7         26/11/2010 10:13:53    Alison Weir
!       Missing USE statement added
!  7    MetDB_Refresh 1.6         18/11/2010 09:47:20    Stan Kellett
!       Inquire removed and check done via IRC in open statement. Revision
!       info corrected.
!  6    MetDB_Refresh 1.5         10/11/2010 11:51:56    Richard Weedon  peer
!       reviewed
!  5    MetDB_Refresh 1.4         10/11/2010 09:32:42    Richard Weedon  Intial
!        version - Ported to F90
!  4    MetDB_Refresh 1.3         20/10/2010 09:17:23    Sheila Needham  Up to
!       and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:58:02    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
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
!
#if defined (BPATH)
USE bufrpath_mod
#endif
!
IMPLICIT NONE
!
SAVE
!
!Arguments with INTENT
INTEGER,INTENT(IN)          ::  X
INTEGER,INTENT(IN)          ::  Y
INTEGER,INTENT(OUT)         ::  NSEQ
INTEGER,INTENT(OUT)         ::  SEQ(*)
CHARACTER(LEN=*),INTENT(IN) ::  TEXT
CHARACTER(LEN=*),INTENT(IN) ::  MODE

!Local variables
INTEGER                     ::  F
INTEGER                     ::  I
INTEGER                     ::  IND
INTEGER                     ::  IRC
INTEGER                     ::  ISEQST
INTEGER                     ::  ISQ
INTEGER                     ::  IX
INTEGER                     ::  IY
INTEGER                     ::  J
INTEGER                     ::  L
INTEGER                     ::  LAST
INTEGER                     ::  N = 0
INTEGER                     ::  ND
INTEGER                     ::  DESCR(2000)
!
LOGICAL                     ::  LOOKED = .FALSE.
!
CHARACTER(LEN=80)           ::  LINE
CHARACTER(LEN=80)           ::  SPACES = ' '
CHARACTER(LEN=208)          ::  FILENAME    ! LOCALSEQ full filename

#if defined (BPATH)
CHARACTER(LEN=200) :: DIR_NAME   !- BUFR tables directory path
INTEGER ::  LEN_DIR_NAME  !- length of dir_name
#endif

!-----------------------------------------------------------------------
! if table not yet read in, put descriptors and counts in an array as
! follows: sequence descriptor, n, (descriptor(i),i=1,n) repeated.
!
! the first time, or if a new local table is to be used, see if string
! has been passed.  if not, look for a data set.  return if neither.
!-----------------------------------------------------------------------
NSEQ = 0
SEQ(1) = 0  !difficult to initialise assumed-size arrays fully

IF_CONSTR1 : &
IF (MODE == 'ADD' .OR. MODE == 'NEW' .OR. .NOT.LOOKED) THEN

  LOOKED=.TRUE.                ! looked for table (none if N=0)
  FILENAME = 'LOCALSEQ'
  ISEQST=INDEX(TEXT,'3')       ! look for sequence in string

  IF_CONSTR2 : &
  IF (ISEQST == 0) THEN
#if defined (BPATH)
    CALL BUFRPATH(DIR_NAME,LEN_DIR_NAME)
    FILENAME=DIR_NAME(1:LEN_DIR_NAME)//'LOCALSEQ'
#endif
#if defined (MVS)
    OPEN (81,FILE='DD:'//FILENAME,FORM='FORMATTED',ACTION='READ',IOSTAT=IRC)
#else
    OPEN (81,FILE=FILENAME,FORM='FORMATTED',IOSTAT=IRC)
#endif
    IF(IRC /= 0) RETURN
  END IF IF_CONSTR2

!-----------------------------------------------------------------------
! either read a line from the data set or take 80 bytes from the string
!-----------------------------------------------------------------------

  L=0                          ! line number
  LAST=0                       ! nonzero if last line
  IF (MODE == 'NEW') N=0       ! count of descriptors in array
   10   CONTINUE
  ND=0                         ! count of descriptors in this seq
  IF (ISEQST == 0) THEN
    READ (81,'(A80)',IOSTAT=LAST) LINE

  ELSE
    LINE=TEXT(L*80+1:L*80+80)
    L=L+1
  END IF

!-----------------------------------------------------------------------
! first find a line starting with a sequence descriptor: a 6-figure
! group with spaces before & after, 3nnnnn at start of line.
!-----------------------------------------------------------------------

  IF_CONSTR3 : &
  IF (LAST == 0) THEN
    ISQ=INDEX(LINE,'3')
    IF (ISQ == 0) GO TO 10
    IF (ISQ > 1 .AND. LINE(1:ISQ-1) /= SPACES(1:ISQ-1)) GO TO 10
    IF (LINE(ISQ+6:ISQ+6) /= SPACES(1:1)) GO TO 10

    DO I=1,5
     IF (LINE(ISQ+I:ISQ+I) < '0') GO TO 10
     IF (LINE(ISQ+I:ISQ+I) > '9') GO TO 10
    END DO

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
    IF (ISEQST == 0) THEN
      READ (81,'(A80)',IOSTAT=LAST) LINE

    ELSE
      LINE=TEXT(L*80+1:L*80+80)
      L=L+1
      IND=INDEX(LINE,'END ')
      IF (IND > 0 .AND. LINE(:IND-1) == SPACES(:IND-1)) LAST=4
    END IF
    IF (LAST == 0 .AND. LINE == SPACES .AND. ND == 0) GO TO 20

!-----------------------------------------------------------------------
! treat any 6-figure group as descriptor.  if not 6 figures, or
! character other than figure, space & comma found, read next line.
!-----------------------------------------------------------------------

    IF_CONSTR4 : &
    IF (LAST == 0 .AND. LINE /= SPACES) THEN
      I=1
   21       CONTINUE
      IF (LINE(I:I) >= '0' .AND. LINE(I:I) <= '9') THEN
        DO J=1,5
         IF (LINE(I+J:I+J) < '0') GO TO 20
         IF (LINE(I+J:I+J) > '9') GO TO 20
        END DO

        READ (LINE(I:I),'(I1)') F
        READ (LINE(I+1:I+2),'(I2)') IX
        READ (LINE(I+3:I+5),'(I3)') IY

        ND=ND+1
        DESCR(N+2+ND)=F*16384+IX*256+IY
        I=I+5
      ELSE IF (LINE(I:I) /= ',' .AND. LINE(I:I) /= ' ') THEN
        GO TO 20
      END IF
      I=I+1
      IF (I < 75) GO TO 21
      GO TO 20
    END IF IF_CONSTR4

!-----------------------------------------------------------------------
! if line has no descriptors, end the sequence, storing the count
! between the sequence descriptor & the descriptors in the sequence.
!-----------------------------------------------------------------------

    DESCR(N+2)=ND
    N=N+2+ND
    IF (LAST == 0) GO TO 10
  END IF IF_CONSTR3
  IF (ISEQST == 0) CLOSE (81)
END IF IF_CONSTR1

!-----------------------------------------------------------------------
! finally (if there is a local table) look for the sequence requested.
!-----------------------------------------------------------------------

IF (N > 0 .AND. X > 0.AND.Y > 0) THEN
  I=0
   31   CONTINUE
  IF (DESCR(I+1) == 3*16384+X*256+Y) THEN
    NSEQ=DESCR(I+2)
    DO J=1,NSEQ
     SEQ(J)=DESCR(I+2+J)
    END DO

  ELSE
    I=I+2+DESCR(I+2)
    IF (I < N) GO TO 31
  END IF
END IF

RETURN
END SUBROUTINE LOCALD
