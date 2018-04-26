      PROGRAM MDBINITS

!-----------------------------------------------------------------------
!
! PROGRAM       : MDBINITS
!
! PURPOSE       : Initialise MDB data set with sequence in block 2
!
! DESCRIPTION   : Data set parameters input in PARM.
!                 Initialisation stops with warning if parameters
!                 not consistent with MDB structures.
!
! DATA TYPE(S)  : Any stored in BUFR (if no BUFR sequence use MDBINIT)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 28/02/2006 12:07:37$
! $Source: /home/us0400/mdb/op/lib/utility/RCS/mdbinits.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         28/02/2006 12:07:37    Sheila Needham  
! $
! Revision 2.2  2003/10/01 13:07:44  usmdb
! Added missing comment characters - S.Cox
!
! Revision 2.1  2003/09/04 15:07:13  usmdb
! 15 Sept 2003    C Long
! 2.1 Avoid risky index block counts (in case data is sporadic).
!
! Revision 2.0  2001/11/12  11:24:06  11:24:06  usmdb (MetDB account c/o usjh)
! Changes so it f95 compiles without errors or
! warnings - S.Cox
!
! Revision 1.2  2001/08/22 13:55:46  usmdb
! Due to library reorganisation - S.Cox
!
! Revision 1.2  99/05/04  15:21:59  15:21:59  usmdb (Generic MDB account)
! Updated for TOMS 5.0 - S.Cox
!
! Revision 1.1  99/04/28  09:30:56  09:30:56  usmdb (Generic MDB account)
! Initial revision
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

      INTEGER   LTRACK
      PARAMETER (LTRACK=56664)                                      !2.0

      INTEGER   NCYLS
      INTEGER   LB
      INTEGER   LENTRY
      INTEGER   IXBLKS
      INTEGER   IBKHRS
      INTEGER   IOFSET
      INTEGER   NOFLOS
      INTEGER   NBT
      INTEGER   NBLOKS
      INTEGER   NLEFT
      INTEGER   NSEQ
      INTEGER   NMAPBLK
      INTEGER   L
      INTEGER   I
      CHARACTER*2     CHAR2   ! Function                            !2.0
      CHARACTER*27998 BLOCK   ! Buffer to hold one block of ds      !2.0
      CHARACTER*27998 ZEROES  ! String of binary zeroes             !2.0
      CHARACTER HEAD*132
      LOGICAL   LOC

#if defined (MVS)
      CHARACTER PARMIN*80
#else
      INTEGER   IERROR
      NAMELIST /STORE_DETAILS/
     & NCYLS,LB,LENTRY,IXBLKS,IBKHRS,IOFSET,NOFLOS
#endif

      DATA LOC /.FALSE./

      DO I=1,27998                                                  !2.0
         ZEROES(I:I) = CHAR(0)                                      !2.0
      END DO ! I                                                    !2.0

      HEAD='$RCSfile: mdbinits.F,v $ ' //
     &     '$Revision: 1$ $Date: 28/02/2006 12:07:37$'

#if defined (MVS)
                           ! READ PARM AS CHARACTERS, THEN CONVERT
      CALL PARM(1,PARMIN)
      READ (PARMIN,*) NCYLS,LB,LENTRY,IXBLKS,IBKHRS,IOFSET,NOFLOS
#else
      OPEN (10,FILE='STORE_DESCRIPTION',IOSTAT=IERROR)
      IF (IERROR.EQ.0) THEN
        READ (10,NML=STORE_DETAILS,IOSTAT=IERROR)
        CLOSE(10)
      ELSE
        WRITE(6,*)'IOSTAT=',IERROR,' reading file STORE_DESCRIPTION'
        STOP
      ENDIF
      
      OPEN (1,FILE='FTN01',RECL=LB,ACCESS='DIRECT',IOSTAT=IERROR)
      IF (IERROR.NE.0) THEN
        WRITE(6,*)'IOSTAT=',IERROR,' opening file FTN01'
        STOP
      ENDIF  
#endif

      IF (IXBLKS.GE.256) THEN
        PRINT *,'MDB DATA SETS MUST HAVE LESS THAN 256 INDEX BLOCKS!'
        STOP
      ENDIF

      IF (NOFLOS.GT.0 .AND. IXBLKS.GE.128) THEN
        PRINT *,'MUST BE LESS THAN 128 INDEX BLOCKS WITH OVERFLOWS!'
        STOP
      ENDIF

      IF (IXBLKS.GE.128) THEN
        PRINT *,'Will data be stored with AIRSTO?'
        PRINT *,'If so, there must be less than 128 index blocks.'
        PRINT *,'Remove this check if you really want so many!'
        STOP
      ENDIF

      IF (MOD(IXBLKS,16).NE.0 .AND. MOD(IXBLKS,21).NE.0            !2.1
     &                        .AND. MOD(IXBLKS,31).NE.1) THEN      !2.1
        PRINT *,IXBLKS,'may be a risky number of index blocks.'    !2.1
        PRINT *,'  Divisors of the number of days in several consecutive
     & months are best avoided'                                    !2.1
        PRINT *,'  because data for a few months ago may seem current if
     & data is sporadic.'                                          !2.1
        PRINT *,'  The safest numbers are multiples of 16 or 21 (because
     & 2*16>31, 3*21>2*31 etc)'                                    !2.1
        PRINT *,'  Rerun without this check if you have good reasons for
     & your value of IXBLKS & data is not sporadic.'               !2.1
        STOP                                                       !2.1
      ENDIF                                                        !2.1

      INQUIRE (FILE='LOCALSEQ',EXIST=LOC) ! IS THERE A LOCAL SEQUENCE?
                                ! FIND BLOCK COUNT FROM SIZE IN CYLS
      NBT=(LTRACK+668)/(LB+668) ! BLOCKS PER TRACK (3390 disk)
      NBLOKS=NCYLS*15*NBT       ! TOTAL BLOCKS (IF 15 TRACKS/CYL)

      IF (NBLOKS.LT.12000 .AND. NBLOKS.GT.LB) THEN
        PRINT *,'The number of blocks is greater than the blocksize,'
        PRINT *,'but map overflow is only allowed with >12000 blocks'
        PRINT *,'- so try a different blocksize!'
        STOP
      ENDIF

      IF (NBLOKS.GT.65535) THEN
        PRINT *,'Total number of blocks is stored in 16 bits,'
        PRINT *,'so must not be more than 65535!'
        STOP
      ENDIF

      NLEFT=NBLOKS-1-NOFLOS     ! BLOCKS FOR INDEX & DATA
      IF (LOC) NLEFT=NLEFT-1    ! ONE LESS INDEX/DATA BLOCK IF SEQS
      IF (NOFLOS.GT.0) NOFLOS=NOFLOS+MOD(NLEFT,IXBLKS)
                          ! SET PARAMETERS AT START & END OF MAP BLOCK
      BLOCK = ZEROES                                                !2.0
      BLOCK(1:2) = CHAR2(NBLOKS)       ! TOTAL NO. BLOCKS IN DS     !2.0
      BLOCK(3:4) = CHAR2(IXBLKS)       ! NO. INDEX BLOCKS IN DS     !2.0
      BLOCK(5:6) = CHAR2(IBKHRS)       ! NO. HOURS IN INDEX BLOCK   !2.0
      BLOCK(7:8) = CHAR2(IOFSET)       ! HR 1 IN 1ST IXBLK AFTER 0Z !2.0
      BLOCK(LB-3:LB-2) = CHAR2(NOFLOS) ! NO. OVERFLOW RECORDS       !2.0
      BLOCK(LB-1:LB) = CHAR2(LENTRY)   ! LENGTH OF INDEX ENTRY      !2.0

      IF (LOC) THEN           ! Set byte for sequence record (if any)
        IF (NBLOKS.LE.12000) THEN
          NSEQ=NBLOKS+8      ! Set byte to 2 if seqs in 2nd block   !2.0
          BLOCK(NSEQ:NSEQ)=CHAR(2)                                  !2.0
        ELSE
          NMAPBLK=(NBLOKS+(LB-1))/LB
          IF (NBLOKS.GT.12000 .AND. NBLOKS-NMAPBLK.LE.12000) THEN
            PRINT *,'Only just over 12000 blocks, so not clear whether'
            PRINT *,' map goes at start or end: make it a bit bigger!'
            STOP
          ENDIF
          BLOCK(1:2)=CHAR2(NBLOKS) ! Leave NBLOKS as set below      !2.0
          BLOCK(9:9)=CHAR(2)   ! Sequence rec byte after header     !2.0
        ENDIF
      ENDIF

      WRITE(6,*)'MDBINITS: NBLOKS = ',NBLOKS                        !2.0

#if defined (MVS)
      WRITE (1) BLOCK(1:LB)         ! INITIALISE FIRST BLOCK        !2.0
#else
      WRITE(1,REC=1,IOSTAT=IERROR) BLOCK(1:LB)                      !2.0
#endif

      IF (LOC) THEN             ! Put any sequence in second block
        L=0
        BLOCK = ZEROES                                              !2.0
#if defined (MVS)
        OPEN (2,FILE='LOCALSEQ',FORM='UNFORMATTED')
   20   READ (2,END=21) BLOCK(L*80+1:L*80+80)                       !2.0
        L=L+1
        GO TO 20
   21   BLOCK(L*80+1:L*80+5)=' END '                                !2.0
        WRITE (1) BLOCK(:LB)                                        !2.0
#else
        OPEN (2,FILE='LOCALSEQ',FORM='FORMATTED',IOSTAT=IERROR)
   20   READ (2,'(A80)',IOSTAT=IERROR,END=21) BLOCK(L*80+1:L*80+80) !2.0
        L=L+1
        GO TO 20 
   21   BLOCK(L*80+1:L*80+5)=' END '                                !2.0
        WRITE (1,REC=2,IOSTAT=IERROR) BLOCK(:LB)                    !2.0
#endif        
      ELSE                      ! If none, second block is like rest
#if defined (MVS)
        WRITE (1) ZEROES(1:LB)                                      !2.0
#else
        WRITE (1,REC=2,IOSTAT=IERROR) ZEROES(1:LB)                  !2.0
#endif
      ENDIF
                             ! Initialise rest of blocks to zero,
      DO I=3,NBLOKS          ! including any map blocks at end.     !2.0
#if defined (MVS)
        WRITE (1) ZEROES(1:LB)                                      !2.0
#else
        WRITE (1,REC=I,IOSTAT=IERROR) ZEROES(1:LB)                  !2.0
#endif
      ENDDO                                                         !2.0

      STOP
      END
