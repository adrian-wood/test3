      SUBROUTINE EXPELM(REQ,IPOS,ILEN,USRELM,NUM,QCFLAG,IFAIL,CERR)

!-----------------------------------------------------------------------
! ROUTINE       : EXPELM
!
! PURPOSE       : To extract the element names from a users request
!
! DESCRIPTION   : PARSE request a character at a time, calling GTGRP
!                 to list element names (& replicate with numbers on
!                 end if necessary) when a space or ')*n' is found,
!                 or stopping if a keyword is found among the names.
!                 Negative values of IERR from PARSE refer to e.g.
!                 unexpected spaces, positive values are errors.
!
! CALLED BY     : MDB
!
! CALLSE        : PARSE to delimit names & record any replication
!                 GTGRP to put names in array, replicating if necessary
!                 KEYS  to initialise array
!
! ARGUMENTS     : REQ     (ip)  MDB request string
!                 IPOS    (ip)  pointer to first element in request
!                 ILEN    (ip)  length of request string
!                 USRELM  (op)  array of fully qualified names
!                 NUM     (op)  number of elements output
!                 QCFLAG  (op)  true if last element is +QC_FLAGS
!                 IFAIL   (op)  error code 8-fatal
!                 CERR    (op)  error message
!
! REVISOIN INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:26$
! $Source: /data/us0400/mdb/op/lib/source/RCS/expelm.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:26    Sheila Needham  
! $
! Revision 2.2  2003/03/06  09:12:09  09:12:09  usmdb (MetDB account c/o usjh)
! Increased NKEYS to 33 for 'SELECT' keyword - S.Cox
! 
! Revision 2.1  2002/11/04  14:55:58  14:55:58  usmdb (MetDB account c/o usjh)
! 18 Nov 2002    C Long
! 2.1  Describe arguments & variables, use DO WHILE instead of GO TO
!      - and comment the code!
!      Checks for IERR=-1 and -5 deleted, as they appear not to be set.
!
! Revision 2.0  2001/01/08  11:58:38  11:58:38  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.6  98/07/23  08:39:42  08:39:42  usmdb (Generic MDB account)
! number of keywords increased
!
! Revision 1.5  97/08/04  13:08:44  13:08:44  uspm (Pat McCormack)
! First revisioned version for 1  - with Y2K change
!
! Revision 1.4  1997/07/25 14:20:42  uspm
! Latest version from 1  -dated 21-7-97
!
! Revision 1.3  1997/05/12 13:23:17  uspm
! Version dated 21-4-97 copied from 1
!
! Revision 1.2  1997/02/12 12:29:22  uspm
! Update dir name in variable head
!
! Revision 1.1  1997/02/11 13:58:32  uspm
! Initial revision
!
! 20-07-98 !F : S.Cox - No of keywords to check increased to 32
!
! 28-07-97 !E : J Lew - No of keywords to check increased to 31
!
! 21-07-97 !D : S.Cox - Number of elements increased from 5000 to 12000
!             :       - for merge requirement. Also change the name of
!             :       - common to make it less contentious and CERR now
!             :       - declared to inherit a character length.
!
! 21-04-97 !C : S.Cox - No. of keywords to check increased to 30
!
! 23-11-96 !B : S.Cox - No. of keywords to check increased to 28
!
! 26-09-96 !A : S.Cox - The program can now cope with a replication
!             :       - count of an element up to 9999. Also increased
!             :       - the number of allowed elements from 500 to 5000.
!             :       - The number of keywords to check has been updated
!             :       - from 22 to 27 for Upper-Air retrieval.
!
! 13-07-96    : S.Needham - new argument QCFLAG set to true if QC
!             : flags are requried with every element.
!
! 19-07-93    : change made to include extra parameters in the
!             : call to parse
!
!             : Introduced by A.Rankin
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

      INTEGER          NKEYS          ! number of keywords
      PARAMETER        (NKEYS=33)                                   !2.2

! Arguments in order (see above)

      CHARACTER*(*)    REQ
      INTEGER          IPOS
      INTEGER          ILEN
      CHARACTER*36     USRELM(*)
      INTEGER          NUM
      LOGICAL          QCFLAG
      INTEGER          IFAIL
      CHARACTER*(*)    CERR                                           !D

! Other variables:
! (The dimension of LEVEL is kept low, because more than 2 levels of
!  nesting is (a) unlikely to occur & (b) if it did occur, it would
!  probably need big increases in the sizes of STACK & ELEM to hold
!  all the names expanded out before the last nesting level.)

      INTEGER          I,J            ! loop counters
      INTEGER          SP             ! stack pointer
      INTEGER          NUMBER         ! number of names from GTGRP
      INTEGER          IERR           ! return code
      INTEGER          IREP           ! replication count
      INTEGER          INDEX          ! pointer to CHAR in REQ
      INTEGER          NEST           ! nesting level
      INTEGER          COUNT          ! total number of element names
      INTEGER          LEVEL(3)       ! nesting start at each level
      INTEGER          BASE           ! start of group for GTGRP
      INTEGER          OBRKT          ! number of open brackets
      INTEGER          CBRKT          ! number of closed brackets

      CHARACTER*1      CHAR           ! current character from REQ
      CHARACTER*1      NEXT           ! set if * or figure expected
      CHARACTER*1      STACK(1000)    ! current group(s)
      CHARACTER*36     TMPELM(12000)  ! names from GTGRP
      CHARACTER*36     WORDS(NKEYS)   ! request keywords
      CHARACTER*132    HEAD

      LOGICAL          GETGRP         ! set if GTGRP to be called
      LOGICAL          LKEY           ! set if keyword found among names
      LOGICAL          EXPECT         ! set if ) or * expected next

! Dynamic common. Compile with FPARMS='DC(*)' on IBM mainframe

      COMMON/EXPELM1/STACK,TMPELM,WORDS

! Revision info

      HEAD='$RCSfile: expelm.f,v $ ' //
     &     '$Revision: 1$ $Date: 30/01/2006 20:22:26$'

      GETGRP=.FALSE.
      LKEY=.FALSE.
      NEST=0
      BASE=0
      SP=0
      IERR=0
      IREP=0
      OBRKT=0
      CBRKT=0
      INDEX=IPOS
      COUNT=0
      QCFLAG=.FALSE.
      EXPECT=.FALSE.
      NEXT='/'

      CALL KEYS(WORDS)

! Look at each character of request in turn in PARSE, checking IERR
! on return to see if it's the end of a group.

      DO WHILE (INDEX.LE.ILEN .AND. IERR.EQ.0 .AND. .NOT.LKEY)
        CHAR=REQ(INDEX:INDEX)
        CALL PARSE(CHAR,SP,STACK,IREP,OBRKT,CBRKT,LEVEL,NEST,IERR,
     &             EXPECT,NEXT)

! If no serious error (only warnings) & not unmatched brackets at end
! of request...

        IF (IERR.LE.0 .AND.
     &           .NOT.(ILEN.EQ.INDEX .AND. CBRKT.NE.OBRKT)) THEN

! IERR=0: only get group if end of string (i.e. no other delimiter)

          IF (IERR.EQ.0) THEN
            IF (INDEX.EQ.ILEN) GETGRP=.TRUE.
            INDEX=INDEX+1

! IERR=-2: get group, assuming no more figures in replication count
!         (but don't increment INDEX if space may be needed between
!          nested names?)

          ELSE IF (IERR.EQ.-2) THEN
            IF (INDEX.EQ.ILEN .OR. SP.EQ.0) INDEX=INDEX+1
            GETGRP=.TRUE.

! IERR=-3: space between ) and * or between * and figure(s): only get
!          group if end of string (otherwise wait & see what's next)

          ELSE IF (IERR.EQ.-3) THEN
            IF (INDEX.GE.ILEN) GETGRP=.TRUE.
            INDEX=INDEX+1

! IERR=-4: get group, assuming no count after closed bracket

          ELSE IF (IERR.EQ.-4) THEN
            GETGRP=.TRUE.

! IERR=-6: get group if end of string or * and brackets match
! (and if * follows & brackets don't match (i.e. nesting),
!  then ignore the space just put on the stack by PARSE)

          ELSE IF (IERR.EQ.-6) THEN
            INDEX=INDEX+1
            IF (INDEX.EQ.ILEN) THEN
              GETGRP=.TRUE.
            ELSE
              IF (REQ(INDEX:INDEX).EQ.'*') THEN
                IF (OBRKT.NE.CBRKT) SP=SP-1
              ELSE
                IF (OBRKT.EQ.CBRKT) GETGRP=.TRUE.
              ENDIF
            ENDIF
          ENDIF

! Set any negative IERR to zero now that GETGRP has been set.

          IF (IERR.LT.0) IERR=0

! Get group off stack, either single element or set of elements in
! brackets, replicating if necessary.

          IF (GETGRP) THEN
            IF (NEST.EQ.0) THEN
              BASE=1
            ELSE
              BASE=LEVEL(NEST)
            ENDIF

            IF (SP.GT.0) THEN
              NUMBER=12000
              CALL GTGRP(SP,STACK,BASE,TMPELM,IREP,NUMBER,IERR)
            ELSE
              NUMBER=0
            ENDIF

! Check each element name against keywords, dropping out of PARSE loop
! if a keyword is found.

            IF (IERR.LE.0) THEN
              DO J=1,NKEYS
                I=1
                DO WHILE (I.LE.NUMBER .AND. .NOT.LKEY)
                  IF (TMPELM(I).EQ.WORDS(J)) THEN
                    LKEY=.TRUE.
                    NUMBER=I-1 ! reset NUMBER to drop keyword etc
                  ELSE
                    I=I+1      ! next name
                  ENDIF
                ENDDO
              ENDDO

! If there's any nesting at this point, reduce the nesting level now
! that the descriptors have been expanded out.
! (N.B. PARSE only INcreases NEST, EXPELM only DEcreases it.)

              IF (NEST.GE.1) NEST=NEST-1

! Put elements in USRELM, incrementing COUNT.
! (SP is zero after a single name or full expansion of a replication,
!  but >0 if, say, only an inner replication has been expanded...)

              IF (SP.EQ.0) THEN
                DO I=1,NUMBER
                  USRELM(COUNT+I)(1:36)=TMPELM(I)
                ENDDO
                COUNT=COUNT+NUMBER
              ENDIF

! Reset IREP & GETGRP for next time.

              IREP=0
              GETGRP=.FALSE.
            ENDIF
          ENDIF                ! end of GETGRP block
        ENDIF
      ENDDO                    ! end of PARSE loop

! If IERR is positive (negative values are only warnings) or
! brackets don't match, put corresponding error message in CERR.

      IF (IERR.LE.0 .AND. OBRKT.NE.CBRKT) IERR=1
      IF (IERR.GT.0) IFAIL=8
      IF (IERR.EQ.1) CERR='unmatched brackets in request string'
      IF (IERR.EQ.2) CERR='* not followed by count in request string'
      IF (IERR.EQ.3) CERR='an element name is too long'
      IF (IERR.EQ.4) CERR='too many names to replicate'
      IF (IERR.EQ.5) CERR='string of names to replicate is too long'
      IF (IERR.EQ.7) CERR='too many names after replication'
      IF (IERR.EQ.8) CERR='too many nesting levels (>3)'
      IF (IERR.EQ.9) CERR='first figure in replication count is zero'

! If the last element requested is QC_FLAGS, delete it & set a flag.

      NUM=COUNT
      IPOS=INDEX
      IF (USRELM(COUNT).EQ.'+QC_FLAGS') THEN
        NUM=NUM-1
        QCFLAG=.TRUE.
      ENDIF
      RETURN
      END
