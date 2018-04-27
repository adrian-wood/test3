//M12DBLOK JOB (M12,CL,WDS0BF),C.LONG.6953,PRTY=8,MSGCLASS=Q
//*
//* To increase the size of an mdb dataset, changing the blocksize):
//* straight copy with extra blocks on end - this works even if there
//* are overflows!  Set blocksizes & block count in parameter & FT02.
//* New blocksize (NEWBLK) is assumed greater than old (LKSIZE).
//*
// EXEC FORT2CLG,FPARMS='CHARLEN(28000),NOFIPS'
//FORT.SYSIN DD *
      INTEGER*2 NB, BLOCKS,XBLOKS,XHOURS,AMSTAR, NOFLOW,INDENT
      INTEGER*2 NINBLK,LEFT
                                              !************************
      PARAMETER (LKSIZE=?????,NEWBLK=?????,NB=????) ! blocksizes, count
                                              !************************
      CHARACTER BLOCK*(LKSIZE),MAP*(LKSIZE-12)
      INTEGER*2 ZERO(14000)
!
! Get block count, reset it, move sequence block flag, adjust overflows
! (storage in a data set with overflows starts by dividing the non-
! overflow data blocks between index blocks, so ajust n(overflows)
! upwards to leave a multiple of xbloks.)
!
      READ (1) BLOCKS,XBLOKS,XHOURS,AMSTAR, MAP, NOFLOW,INDENT
      IF (ICHAR(MAP(BLOCKS:BLOCKS)).NE.0) THEN ! IF SEQUENCE BLOCK,
        NSQ=1
        IF (NB.NE.BLOCKS) THEN
          MAP(NB:NB)=MAP(BLOCKS:BLOCKS)        ! SET FLAG AT NEW NB
          MAP(BLOCKS:BLOCKS)=CHAR(0)           ! & UNSET IT AT OLD
        ENDIF
      ELSE
        NSQ=0
      ENDIF
!
      IF (NOFLOW.GT.0) THEN                    ! IF THERE ARE OVERFLOWS
        NEXTRA=NB-BLOCKS                       ! NUMBER OF EXTRA BLOCKS
        NOVER=NEXTRA-(NEXTRA/XBLOKS)*XBLOKS    !
        NOFLOW=NOFLOW+NOVER                    ! ADJUST OVERFLOW COUNT
      ENDIF
      WRITE (2) NB,XBLOKS,XHOURS,AMSTAR,MAP(1:LKSIZE-12),
     &         (ZERO(I),I=1,(NEWBLK-LKSIZE)/2),NOFLOW,INDENT
!
! Copy sequence block and index blocks (assuming no index overflow)
!
      DO J=2,1+NSQ+XBLOKS
        READ (1) BLOCK
        WRITE (2) BLOCK
      ENDDO
!
! Copy data blocks with zeros between lengths & records
! (all zeros if no data in block)
!
      DO J=1+NSQ+XBLOKS+1,BLOCKS
        READ (1) BLOCK(1:2),NINBLK,LEFT,BLOCK(7:LKSIZE)
        IF (NINBLK.GT.0) THEN
          LEFT=LEFT+NEWBLK-LKSIZE
          WRITE (2) BLOCK(1:2),NINBLK,LEFT,BLOCK(7:6+2*NINBLK),
     &     (ZERO(I),I=1,(NEWBLK-LKSIZE)/2),BLOCK(6+2*NINBLK+1:LKSIZE)
        ELSE
          WRITE (2) (ZERO(I),I=1,NEWBLK/2)
        ENDIF
      ENDDO
!
! Add new blocks, all zeros, to fill data set
!
      DO J=BLOCKS+1,NB
        WRITE (2) (ZERO(I),I=1,NEWBLK/2)
      ENDDO
      STOP
      END
/*
//*            *************************
//*            ** Set Original DSName **
//GO.FT01F001 DD DSN=MDB.??????,DISP=SHR,LABEL=(,,,IN)
//*            *************************************************
//*            ** Set new DSName   ...          Disk name ... **
//FT02F001 DD DSN=??????????,DISP=(,CATLG),VOL=SER=??????,
// UNIT=DISK,DCB=(RECFM=F,BLKSIZE=?????),SPACE=(CYL,?)
//*                ** ... and new blocksize **
//*                ***********************
//
