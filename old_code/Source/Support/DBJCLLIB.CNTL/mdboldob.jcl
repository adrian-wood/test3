//T12CLAMP JOB (OP1,DB,WDS0BF),C.LONG.6954,PRTY=8,MSGCLASS=Q,CLASS=I
//*
//* To make a copy of an archive data set suitable for storage
//* of old data by setting the map bytes for the data blocks.
//* (The output data set must be initialised beforehand; it may have
//* to be bigger than the input if there is a lot of data to store.)
//*
// EXEC FORT2CLG,FPARMS='CHARLEN(28000),NOFIPS'

      PARAMETER (LKSIZE=27998)
      CHARACTER*(LKSIZE)    MAP,BLOCK
      CHARACTER*2           TAG(255)          ! time tags
      INTEGER               XNO(255)          ! index block numbers
      INTEGER               BLOCKS,XBLOKS     ! block counts from map

      READ (1) MAP
      BLOCKS=ICHAR(MAP(1:1))*256+ICHAR(MAP(2:2))  ! blocks in data set
      XBLOKS=ICHAR(MAP(3:3))*256+ICHAR(MAP(4:4))  ! index blocks

      OPEN (2,ACCESS='DIRECT',RECL=LKSIZE)    ! direct to write map last

      DO J=2,BLOCKS                           ! loop round all blocks
        READ (1) BLOCK
        WRITE (2,REC=J) BLOCK                 ! copy block to output

! For index blocks list tags & numbers; for data blocks look up tag
! & set corresponding number in map to go in output data set.

        IF (J.GT.2 .AND. J.LE.2+XBLOKS) THEN  ! index block
          TAG(J)=BLOCK(1:2)                   ! list tags
          XNO(J)=J-1                          ! with numbers as in map
        ELSE IF (J.GT.2+XBLOKS) THEN          ! data block (or overflow)
          DO I=3,2+XBLOKS                     ! look for tag in list
            IF (TAG(I).EQ.BLOCK(1:2)) MAP(6+J:6+J)=CHAR(XNO(I))
          ENDDO
        ENDIF
      ENDDO

      WRITE (2,REC=1) MAP                     ! Finally write out map
      STOP
      END
//GO.FT01F001 DD DISP=SHR,DSN=MDB.DAAMDARS.GAIR.D030831,LABEL=(,,,IN)
//GO.FT02F001 DD DISP=OLD,DSN=MCC3.CLAMDARS.D030831
