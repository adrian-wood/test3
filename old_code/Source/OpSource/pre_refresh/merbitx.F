      SUBROUTINE MERBITX(SEQDES,NL,FS,XS,YS,SCALES,REFVALS,WIDTHS,BITS)

!-----------------------------------------------------------------------
!                                                                     !
! PROGRAM       : MERBITX                                             !
!                                                                     !
! PURPOSE       : Generate bit index for streamlined merge from bufr  !
!                 descriptor sequence (input as in SDB.BUFR.LOCALSEQ).!
!                 This is not quite the same as a retrieval bit index:!
!                 like a merge table it has a line for any associated !
!                 bits and the line for a replication count comes     !
!                 before the operator.                                !
!                                                                     !
! CALLED BY     : MERGE                                               !
!                                                                     !
! CALLS         : LOCALD, TABLED, TABLEB, DESFXY                   !1.2
!                                                                     !
! PARAMETERS    : (1) sequence descriptor                         (i) !
!                 (2) number of lines                             (o) !
!                 (3) F                 )                         (o) !
!                 (4) X                 ) for                     (o) !
!                 (5) Y                 ) each                    (o) !
!                 (6) scale             ) descriptor              (o) !
!                 (7) reference value   ) in expansion            (o) !
!                 (8) bits in value     )                         (o) !
!                 (9) bits before value )                         (o) !
!
! REVISION INFO:
!
! $Workfile: merbitx.F$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 29/03/2010 16:50:22$
!
! CHANGE RECORD:
!
! $Log:
!  2    Met_DB_Project 1.1         29/03/2010 16:50:22    Brian Barwell
!       Initialise variable ND at start of routine.
!  1    Met_DB_Project 1.0         28/02/2006 11:44:16    Sheila Needham  
! $
! Revision 1.2  2003/03/27 13:58:11  usmdb
! 7 April 2003    C Long
! 1.2  Remove LOCALB call.
!
! Revision 1.1  97/10/29  12:38:28  12:38:28  uspm
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER FORMAT*1
      CHARACTER NAME*64
      CHARACTER UNITS*24
      CHARACTER HEAD*80    ! revisioning information                 !2
      INTEGER BITS(*)      ! bits before value
      INTEGER BITS_REPL    ! bits replicated
      INTEGER DES(999)     ! array for expansion of descriptors
      INTEGER F
      INTEGER FS(*)
      INTEGER I            ! short-term loop variable
      INTEGER IASSOC       ! width of associated field
      INTEGER IBEFOR       ! bits before latest value
      INTEGER IBEFOR_REPL  ! bits before replication
      INTEGER IDES         ! function to put descriptor in binary form
      INTEGER ISCALE       ! scale increment (from operation
      INTEGER IWIDTH       ! width increment (from operation
      INTEGER N            ! current descriptor number
      INTEGER ND           ! descriptors count (adjusted in expansion)
      INTEGER NL           ! current line number in table
      INTEGER NL_REPL      ! line number for replication operator
      INTEGER NSEQ         ! descriptor count from Table D
      INTEGER REFVAL       ! reference value from Table D
      INTEGER REFVALS(*) ! reference values returned
      INTEGER SCALE        ! scale from Table B
      INTEGER SCALES(*)    ! scales returned
      INTEGER SEQ(999)     ! array for sequence from Table D
      INTEGER SEQDES       ! input (local) sequence descriptor
      INTEGER WIDTH        ! width from Table B
      INTEGER WIDTHS(*)    ! widths returned
      INTEGER X
      INTEGER XS(*)
      INTEGER Y
      INTEGER YS(*)
!                                                  Revision information
      HEAD = '$Workfile: merbitx.F$ ' //
     &       '$Revision: 2$ $Date: 29/03/2010 16:50:22$'

! Initialise loop variables & BUFR scale change etc

      DES(1)=SEQDES
      ND=1                 ! total no. of descriptors in DES         !2
      N=1                  ! current descriptor in DES array
      NL=0                 ! current line in output table (xs etc)
      IBEFOR=32            ! start in data section after length bits

      ISCALE=0
      IWIDTH=0
      IASSOC=0

! Convert descriptor to F, X & Y

  100 CONTINUE
      CALL DESFXY(DES(N),F,X,Y)

! Check for marker inserted after replication: if found, keep number of
! bits replicated (as WIDTH on operator line) & number of lines too...

      IF (DES(N).EQ.-1) THEN       ! if end-of-replication marker
        BITS_REPL=IBEFOR-IBEFOR_REPL
        WIDTHS(NL_REPL)=BITS_REPL  ! total number of bits replicated
        XS(NL_REPL)=NL-NL_REPL-1   ! number of elements (not used)
        N=N+1

! If replication descriptor found, move down descriptors after those
! replicated and insert a marker to be recognised above.
! Put line for count (if any) before operator as in merge table.

      ELSE IF (F.EQ.1) THEN
        IF (DES(N+1).EQ.IDES(031001)) THEN
          N=N+1                    ! go past count descriptor
          NL=NL+1                  ! one more output line
          FS(NL)=0
          XS(NL)=31
          YS(NL)=1
          SCALES(NL)=0
          REFVALS(NL)=0
          WIDTHS(NL)=8
          BITS(NL)=IBEFOR
          IBEFOR=IBEFOR+8
        ENDIF
        IBEFOR_REPL=IBEFOR

        N=N+1                      ! go past operator
        NL=NL+1                    ! add an output line
        NL_REPL=NL                 ! keep line number to set total bits
        FS(NL)=1                   ! & set F=1 there now

        DO I=ND,N+X,-1             ! move down descriptors after replicn
          DES(I+1)=DES(I)
        ENDDO
        ND=ND+1                    ! one more descriptor altogether
        DES(N+X)=-1                ! put marker before descriptors moved

! If any other operator, keep scale & width changes & associated width.
! Ignore anything else.

      ELSE IF (F.EQ.2) THEN
        IF (X.EQ.1 .AND. Y.GT.0) IWIDTH=Y-128
        IF (X.EQ.1 .AND. Y.EQ.0) IWIDTH=0
        IF (X.EQ.2 .AND. Y.GT.0) ISCALE=Y-128
        IF (X.EQ.2 .AND. Y.EQ.0) ISCALE=0
        IF (X.EQ.4) IASSOC=Y
        N=N+1

! If sequence descriptor found,
! insert sequence of descriptors, moving the rest down. adjust total.
! (the sequence descriptor itself is overwritten, so no N=N+1.)
! (Can't get rid of SEQ array without changing calls: NSEQ needed to
! move remaining descriptors down!)

      ELSE IF (F.EQ.3) THEN
        CALL LOCALD(X,Y,SEQ,NSEQ,' ',' ')
        IF (NSEQ.EQ.0) CALL TABLED(X,Y,SEQ,NSEQ)

        DO I=ND,N+1,-1
          DES(I+NSEQ-1)=DES(I)
        ENDDO

        DO I=1,NSEQ
          DES(N+I-1)=SEQ(I)
        ENDDO

        ND=ND+NSEQ-1

! If it's an element descriptor,
! return element details, adjusting Table B scale etc if necessary

      ELSE IF (F.EQ.0) THEN
        CALL TABLEB(X,Y,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)

! No operator applies to class 31, some not to code or flag table.
! For any associated field leave a line with a zero descriptor.

        IF (X.NE.31) THEN
          IF (IASSOC.GT.0) THEN
            NL=NL+1                ! one more output line
            FS(NL)=0
            XS(NL)=0
            YS(NL)=0
            SCALES(NL)=0
            REFVALS(NL)=0
            WIDTHS(NL)=IASSOC
            BITS(NL)=IBEFOR
            IBEFOR=IBEFOR+IASSOC
          ENDIF

          IF (INDEX(UNITS,'TABLE').EQ.0) THEN
            SCALE=SCALE+ISCALE
            WIDTH=WIDTH+IWIDTH
          ENDIF
        ENDIF

! Now the line for the element itself.

        NL=NL+1                    ! one more output line
        FS(NL)=F
        XS(NL)=X
        YS(NL)=Y
        SCALES(NL)=SCALE
        REFVALS(NL)=REFVAL
        WIDTHS(NL)=WIDTH
        BITS(NL)=IBEFOR

        IBEFOR=IBEFOR+WIDTH
        N=N+1
      ENDIF
      IF (N.LE.ND) GO TO 100
      RETURN
      END
