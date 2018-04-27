//T12SCXP1 JOB (OP1,DB,WDS0BF),SCOX.6955,PRTY=8,MSGCLASS=Q,CLASS=I
//*
//* --------------------------------------------------------------------
//* To increase the size of an mdb dataset (no change in blocksize):
//* straight copy with extra blocks on end - this works even if there
//* are overflows!
//* Set blocksize & number of cylinders in parameter & FT02.        !1.5
//* This job should now move the map to the end if necessary        !1.5
//* and cope with an increase in the number of map blocks.          !1.5
//*
//* $Revision: 1$
//* $Date: 28/02/2006 12:18:41$
//*
//* $Log:
//*  1    Met_DB_Project 1.0         28/02/2006 12:18:41    Sheila Needham  
//* $
//* Revision 1.10  2003/06/02 10:39:39  usmdb
//* Changed TIC to WDS0BF - S.Cox
//*
//* Revision 1.9  2002/06/28 10:07:04  usmdb
//* Corrected number of blocks in new dataset - S.Cox
//*
//* Revision 1.8  2002/06/27 09:26:30  usmdb
//* Uncomment FT02 - S.Cox
//*
//* Revision 1.7  2002/06/27 08:19:25  usmdb
//* Replace most of INTEGER*2 read/write with CHARACTER read/write - S.Cox
//*
//* Revision 1.5  2000/11/01  15:09:48  15:09:48  usmdb (Generic MDB account)
//* 20 Nov 2000
//* 1.5  Allow for different numbers of map blocks in input & output
//*      & map at start in input and at end in output.
//*
//* --------------------------------------------------------------------
//*
// EXEC FORT2CLG,FPARMS='CHARLEN(28000),NOFIPS'
//FORT.SYSIN DD DSN=MDB.SOURCE(ICHAR2),DISP=SHR
//           DD DSN=MDB.SOURCE(CHAR2),DISP=SHR
//           DD *

!-----------------------------------------------------------------------
! Set blocksize & number of cylinders in OUTPUT data set            !1.5
! (Assume track length & gap between blocks for 3390 disk,          !1.5
!  with 15 tracks per cylinder)                                     !1.5
!-----------------------------------------------------------------------

      PARAMETER (LKSIZE=?????, NEWCYLS=????)                        !1.5
      PARAMETER (LTRACK=56664, LGAP=668, NTRKCYL=15)                !1.5

      CHARACTER*(LKSIZE)    BLOCK
      CHARACTER*(LKSIZE-12) MAP
      CHARACTER*(LKSIZE)    CMAP                                    !1.6
      CHARACTER*2           CHAR2              ! declare function   !1.6
      INTEGER               ICHAR2             ! declare function   !1.6

      INTEGER   NB,BLOCKS,XBLOKS,XHOURS,AMSTAR,NOFLOW,INDENT        !1.6

      INTEGER*2 ZERO(14000) /14000*0/          ! 28000 hex zeros    !1.5

      NBT=(LTRACK+LGAP)/(LKSIZE+LGAP)          ! blocks per track   !1.5
      NB=NEWCYLS*NTRKCYL*NBT                   ! blocks in new d/s  !1.5

!-----------------------------------------------------------------------
! get block count, reset it, move sequence block flag, adjust overflows
! (storage in a data set with overflows starts by dividing the non-
! overflow data blocks between index blocks, so adjust n(overflows)
! upwards to leave a multiple of xbloks.)
!-----------------------------------------------------------------------

      READ (1) CMAP                                                 !1.6
      BLOCKS=ICHAR2(CMAP(1:2))                                      !1.6
      XBLOKS=ICHAR2(CMAP(3:4))                                      !1.6
      XHOURS=ICHAR2(CMAP(5:6))                                      !1.6
      AMSTAR=ICHAR2(CMAP(7:8))                                      !1.6
      MAP=CMAP(9:LKSIZE-4)                                          !1.6
      NOFLOW=ICHAR2(CMAP(LKSIZE-3:LKSIZE-2))                        !1.6
      INDENT=ICHAR2(CMAP(LKSIZE-1:LKSIZE))                          !1.6

      CMAP(1:2)=CHAR2(NB)                                           !1.6
      
      IF (NOFLOW.GT.0) THEN                    ! If overflows,      !1.5
        NEXTRA=NB-BLOCKS                       ! new minus old      !1.5
        NOVER=NEXTRA-(NEXTRA/XBLOKS)*XBLOKS    ! Extra overflows    !1.5
        NOFLOW=NOFLOW+NOVER                    ! Reset overflows.   !1.5
        CMAP(LKSIZE-3:LKSIZE-2)=CHAR2(NOFLOW)                       !1.6
      ENDIF

!-----------------------------------------------------------------------
! Write the first block back,
!  moving the sequence flag if both data sets have map at start,    !1.5
!  just changing the block count if both data sets have map at end, !1.5
!  or replacing the map by sequence flag followed by zeros if map   !1.5
!   is to go at the end.                                            !1.5
!-----------------------------------------------------------------------

      IF (NB.LE.12000) THEN                                         !1.5
        MAP(NB:NB)=MAP(BLOCKS:BLOCKS)          ! seq flag at new NB
        MAP(BLOCKS:BLOCKS)=CHAR(0)             ! unset it at old NB
        WRITE (2) CMAP(1:8),MAP,CMAP(LKSIZE-3:LKSIZE)               !1.6
      ELSE IF (BLOCKS.GT.12000) THEN                                !1.5
        WRITE (2) CMAP(1:8),MAP,CMAP(LKSIZE-3:LKSIZE)               !1.6
      ELSE IF (NB.GT.12000 .AND. BLOCKS.LE.12000) THEN              !1.5
        WRITE (2) CMAP(1:8),MAP(BLOCKS:BLOCKS),                     !1.6
     &            (CHAR(0),I=1,LKSIZE-13),CMAP(LKSIZE-3:LKSIZE)     !1.6
      ENDIF

      DO J=2,BLOCKS                            ! Copy old blocks    !1.5
        READ (1) BLOCK
        WRITE (2) BLOCK
      ENDDO                                                         !1.5

      DO J=BLOCKS+1,NB                         ! Zero new blocks    !1.5
        WRITE (2) (ZERO(I),I=1,LKSIZE/2)       ! (blocksize even!)  !1.5
      ENDDO                                                         !1.5

!-----------------------------------------------------------------------
! Code to deal with map overflow blocks e.g. ATOVSG dataset. If BLOCKS
! > 12000, then the map blocks (index block pointers) are at the end of
! the dataset. Close then re-open the storage datasets as direct access.
! Calculate the number of map blocks at the end of the dataset. Loop
! over these, reading in the block from the end of unit 1 and writing to
! the end of unit 2. Previous section of code will have copied all the
! data blocks (including the map blocks) from unit 1 to unit 2, so zero
! the map blocks written to unit 2 by the previous section of code.
!-----------------------------------------------------------------------

      NMAPIN=(NB+(LKSIZE-1))/LKSIZE                                 !1.5
      NMAPOUT=(BLOCKS+(LKSIZE-1))/LKSIZE                            !1.5
      IF (NB.GT.12000) THEN                                         !1.5
        CLOSE(1)
        CLOSE(2)
        OPEN(1,ACCESS='DIRECT',RECL=LKSIZE)
        OPEN(2,ACCESS='DIRECT',RECL=LKSIZE)

!-----------------------------------------------------------------------
! There may be more map blocks in the output than in the input.     !1.5
! Only the input number of map blocks needs to be copied - any      !1.5
! extra block has already been set to zero - but they must be       !1.5
! copied to the right place, leaving the extra block at the end.    !1.5
!  If the map is at the start in the input & must go at the end     !1.5
! in the output, it's already been read in: just write it out.      !1.5
!-----------------------------------------------------------------------

        IF (BLOCKS.GT.12000) THEN    ! if map at end in input...    !1.5
          DO N=1,NMAPIN                                             !1.5
            READ(1,REC=BLOCKS-NMAPIN+N) BLOCK                       !1.5
            WRITE(2,REC=NB-NMAPOUT+N) BLOCK                         !1.5
            WRITE(2,REC=BLOCKS-NMAPIN+N) (ZERO(I),I=1,LKSIZE/2)     !1.5
          ENDDO
        ELSE                         ! if map at start in input...  !1.5
          WRITE(2,REC=NB-NMAPOUT+1) MAP                             !1.5
        ENDIF
        CLOSE(1)
        CLOSE(2)
      ENDIF

      STOP
      END
/*
//*            *************************
//*            ** Set Original DsName **
//GO.FT01F001 DD DSN=MDB.??????????,DISP=SHR,LABEL=(,,,IN)
//*            *************************************************
//*            ** Set new DsName   ...          Disk name ... **
//FT02F001 DD DSN=MDB.????????.NEW,DISP=(,CATLG),
// VOL=SER=OPR???,
// UNIT=DISK,DCB=(RECFM=F,BLKSIZE=?????),SPACE=(CYL,????)
//*                ** ... and blocksize **
//*                ***********************
//
