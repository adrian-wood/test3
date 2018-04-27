!**********************************************************************
!                                                                     *
! PROGRAM       : MDBCHECK                                            *
!                                                                     *
! PURPOSE       : Make sure that lengths in data block add up to give *
!                 blocksize (in case some lengths overwritten...)     *
!                                                                     *
!**********************************************************************
      INTEGER   BLKSIZ
      INTEGER   BLOCKS,XBLOKS
      INTEGER   NINBLK,LEFT, nsq/1/
!
      PARAMETER (BLKSIZ=27998)
      CHARACTER*(BLKSIZ) MAP,BLOCK
      CHARACTER*2 STR
      INTWO(STR)=ICHAR(STR(1:1))*256+ICHAR(STR(2:2))
!
      OPEN (1,ACCESS='DIRECT',RECL=BLKSIZ)
!
! Read map to get data set parameters
!
      READ (1,REC=1) MAP
      BLOCKS=INTWO(MAP(1:2))
      XBLOKS=INTWO(MAP(3:4))
!
! Loop round data blocks, checking sum of lengths against blocksize
! for those currently in use.
!
      DO I=1+NSQ+XBLOKS+1,BLOCKS-1
        IF (MAP(7+i-NSQ:7+I-NSQ).NE.CHAR(0)) THEN
          READ (1,REC=I) BLOCK
          NINBLK=INTWO(BLOCK(3:4))
          LEFT=INTWO(BLOCK(5:6))
               print *,i,ninblk,left
!
          L=6+NINBLK*2+LEFT
          DO J=1,NINBLK
            L=L+INTWO(BLOCK(6+J*2-1:6+J*2)) ! TO END OF NEXT OB
          ENDDO
          IF (L.NE.BLKSIZ) PRINT *,I,'TH BLOCK INCONSISTENT',L
          IF (L.NE.BLKSIZ) PRINT *,BLOCK(:6+NINBLK*2)
        ENDIF
      ENDDO
      STOP
      END
