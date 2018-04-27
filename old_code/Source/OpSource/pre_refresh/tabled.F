      SUBROUTINE TABLED(XREQ,YREQ,SEQ,ND)

!-----------------------------------------------------------------------
!
! PROGRAM       : TABLED
!
! PURPOSE       : to expand a BUFR sequence
!
! DESCRIPTION   :
!           The input consists of one or more lines for each sequence;
!           sequences may or may not be separated by blank lines.
!           Each sequence has a Table D descriptor, a 3-figure count,
!           and that many descriptors defining the sequence.
!             The descriptors are separated by spaces, so there is
!           room for up to 10 defining descriptors on the 1st line.
!           Leave a blank line if there are exactly 10.  If there are
!           more than 10, restart in column 1 on subsequent lines.
!             The descriptors are in BUFR form, but Table D is for
!           both BUFR & CREX.  Confusingly, most sequences are in
!           the BUFR section of the Manual on Codes, but a few are
!           in the CREX section.  But conceptually there is only one
!           Table D, so these two sources are combined in the input
!           used here: sequences with delayed replication will need
!           031001 deleted before use.
!             (One CREX sequence, D05006, has operators to change
!           the temperature units from C to K, a practice "not
!           recommended" by regulation 95.3.4.2; this will need
!           special treatment if the CREX version is ever used.)
!
! CALLED BY     : DECODE, ENCODE, CREXDAT
!
! ARGUMENTS     : (1)  X                                   (input)
!               : (2)  Y                                   (input)
!               : (3)  sequence                            (output)
!               : (4)  number of descriptors in sequence   (output)
!                      (returned as zero if sequence not found)
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 02/04/2008 10:53:53$
! $Source: /data/us0400/mdb/op/lib/source/RCS/tabled.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         02/04/2008 10:53:53    Stan Kellett
!       Increased MAXNDS from 3333 to 5000 to stop array overflow after
!       increase to number of sequences in table D
!  1    Met_DB_Project 1.0         30/01/2006 20:25:08    Sheila Needham  
! $
! Revision 2.2  2003/03/06  11:09:30  11:09:30  usmdb (MetDB account c/o usjh)
! 17 Mar 2003    C Long
! 2.2  Rewrite with readable input 
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

! MAXND is determined by a single byte for the count in the index.
! MAXNSEQ is arbitrary, MAXNDS limited by two bytes for pointers.

      INTEGER ND         ! number of descriptors in sequence
      INTEGER NDS        ! number of descriptors in all sequences
      INTEGER NSEQ       ! number of sequences
      INTEGER MAXND      ! max number of descriptors in a sequence
      INTEGER MAXNDS     ! max number of descriptors in all sequences
      INTEGER MAXNSEQ    ! max number of sequences
      PARAMETER (MAXND=255,MAXNDS=5000,MAXNSEQ=999)   !ST2

      INTEGER SEQ(*)     ! array for sequence to be returned
      INTEGER XREQ,YREQ  ! arguments 1 & 2
      INTEGER F,X,Y      ! from sequence descriptor during table read
      INTEGER OLDX,OLDY  ! X & Y for previous sequence read in
      INTEGER IRC        ! input return code
      INTEGER LEN_DIR_NAME
      INTEGER I          ! loop variable
      LOGICAL FEXIST     ! TRUE if TABLED exists
      LOGICAL INCORE     ! set if table read in
      LOGICAL READY      ! set if FT81 open

      INTEGER IFIRST     ! start of range in binary search
      INTEGER ILAST      ! end of range in binary search
      INTEGER NPOS       ! position of target in list
      INTEGER N          ! pointer to sequence in DESCHR
      CHARACTER*2 XY     ! descriptor to be looked for in index

      CHARACTER*5 INDEX(MAXNSEQ)  ! (descriptor, ND, pointer)*NSEQ
      CHARACTER*6 DESCHR(MAXNDS)  ! descriptors in character form
      CHARACTER*1 FLAG            ! not space if flagged for deletion
      CHARACTER*80 REVISION       ! line to skip at start of input
      CHARACTER*208 FILENAME      ! long string for use on HP
      CHARACTER*132 HEAD
#if defined (BPATH)
      CHARACTER  DIR_NAME*200   !- BUFR tables directory path       !1.4
#endif

      COMMON /BUFRSEQS/ INDEX,DESCHR  ! dynamic common to save space

      DATA    OLDX/0/,OLDY/0/     ! initialise last sequence read in
      DATA    INCORE/.FALSE./
      DATA    FILENAME/'TABLED'/
      DATA    LEN_DIR_NAME/0/

      SAVE

! Set 16-bit descriptor to look for in index

      XY(1:1)=CHAR(3*64+XREQ)
      XY(2:2)=CHAR(YREQ)

! Read table into core 1st time only.

      IF (.NOT.INCORE) THEN
        HEAD='$RCSfile: tabled.F,v $ ' //
     &       '$Revision: 2$ $Date: 02/04/2008 10:53:53$'

! See if unit=81 has been opened.
! If not, open it here - if there's a DDNAME TABLED.

        INQUIRE (81,OPENED=READY)
        IF (.NOT.READY) THEN
#if defined (BPATH)
          CALL BUFRPATH(DIR_NAME,LEN_DIR_NAME)                      !1.4
          FILENAME(1:LEN_DIR_NAME)=DIR_NAME(1:LEN_DIR_NAME)         !1.6
          FILENAME(LEN_DIR_NAME+1:LEN_DIR_NAME+6)='TABLED'          !1.6
#endif
          LEN_DIR_NAME=LEN_DIR_NAME+6
          INQUIRE (FILE=FILENAME,EXIST=FEXIST)
          IF (.NOT.FEXIST) THEN
            WRITE(6,*)'TABLED: ERROR - File ',
     &      FILENAME(1:LEN_DIR_NAME),' not found'
            STOP
          ENDIF

#if defined (MVS)
          OPEN (81,FILE=FILENAME,FORM='FORMATTED',IOSTAT=IRC,
     &             ACTION='READ')
#else
          OPEN (81,FILE=FILENAME,FORM='FORMATTED',IOSTAT=IRC)
#endif

          IF (IRC.NE.0) THEN
            WRITE(6,*)'TABLED: ERROR opening ',
     &      FILENAME(1:LEN_DIR_NAME),' IOSTAT = ',IRC
            STOP
          ENDIF
        ENDIF  !- ready

! Skip revision line at start

        READ (81,'(A80)') REVISION

! Loop round reading a whole sequence at a time, repeating the read if
! necessary to skip a blank line or (optional) sequence title.
! Read one more descriptor than expected, to check the count.
! Skip any sequence with a question mark after the count.

        IRC=0
        DO WHILE (IRC.EQ.0)
   10     READ (81,1,IOSTAT=IRC) F,X,Y,ND,FLAG,(DESCHR(NDS+I),I=1,ND+1)
    1         FORMAT (I1,I2,I3, I3,A1, 10(A6,1X)/(10(A6,1X)))
          IF (IRC.EQ.0) THEN
            IF (ND.EQ.0) GO TO 10       ! to skip blank line
            IF (FLAG.NE.' ') GO TO 10   ! to skip old sequence

! Make sure there are enough descriptors, & not too many or wrong count,
! that sequences being defined are in ascending order -
! & stop if only 9 on the first line when there should be 10.

            IF (F.NE.3 .OR. X.GE.64 .OR. Y.GE.256) THEN
              PRINT *,F*100000+X*1000+Y,'in Table D: bad F=3 descriptor'
              STOP
            ENDIF

            IF (DESCHR(NDS+ND).EQ.' ') THEN
              PRINT*,F*100000+X*1000+Y,'in Table D: too few descriptors'
              STOP
            ENDIF

            IF (DESCHR(NDS+ND+1).NE.' ') THEN
              PRINT*,F*100000+X*1000+Y,'in Table D: count wrong?'
              STOP
            ENDIF

            IF (N.GE.10 .AND. DESCHR(NDS+10).EQ.' ') THEN
              PRINT*,F*100000+X*1000+Y,'in Table D: no tenth descriptor'
              STOP
            ENDIF

            IF (X.LT.OLDX .OR. (X.EQ.OLDX .AND. Y.LT.OLDY)) THEN
              PRINT *,'Table D sequences out of order'
              PRINT*,F*100000+X*1000+Y,'follows',3*100000+OLDX*1000+OLDY
              STOP
            ENDIF

! If there's no error detected, index the sequence.

            INDEX(NSEQ+1)(1:1)=CHAR(3*64+X)
            INDEX(NSEQ+1)(2:2)=CHAR(Y)
            INDEX(NSEQ+1)(3:3)=CHAR(ND)
            INDEX(NSEQ+1)(4:4)=CHAR((NDS+1)/256)
            INDEX(NSEQ+1)(5:5)=CHAR(MOD(NDS+1,256))

! Update counts & reset last descriptor

            NSEQ=NSEQ+1
            NDS=NDS+ND
            OLDX=X
            OLDY=Y
          ENDIF
        ENDDO

        CLOSE (81)
        INCORE=.TRUE.
      ENDIF

! Initialise variables for binary search

      ND=0
      IFIRST=0
      ILAST=NSEQ

! Search as in ISRCH, looping until range is reduced to 1.
! Compare the target with an element near the middle of the range
! and reset the start or end of the range accordingly.

      DO WHILE (IFIRST.LT.ILAST)
        NPOS=(IFIRST+ILAST+1)/2

        IF (XY.LT.INDEX(NPOS)(1:2)) THEN
          ILAST=NPOS-1
        ELSE
          IFIRST=NPOS
        ENDIF
      ENDDO

! Set NPOS if target found, return if not

      NPOS=IFIRST
      IF (NPOS.EQ.0 .OR. XY.NE.INDEX(NPOS)(1:2)) RETURN

! Return expansion of sequence.  N points to sequence in DESCHR.

      ND=ICHAR(INDEX(NPOS)(3:3))
      N=ICHAR(INDEX(NPOS)(4:4))*256+ICHAR(INDEX(NPOS)(5:5))
      DO I=1,ND
        READ (DESCHR(N+I-1),'(I1,I2,I3)') F,X,Y
        SEQ(I)=F*16384+X*256+Y
      ENDDO

      RETURN
      END
