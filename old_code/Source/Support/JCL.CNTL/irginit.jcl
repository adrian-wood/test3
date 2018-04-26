//T12JHINT JOB (M12,JH,WDS0BF),MDBTEAM.6954,PRTY=8,MSGCLASS=Q
//*
//* INITIALISE MDB DATA SET - SET PARAMETERS ON EXEC CARD AT END
//* USE TO STORE CHARACTER DATA THAT IS IRREGULAR E.G TROPICAL ADVISORY
//*
//CL EXEC FORT2CL
!----------------------------------------------------------------------
!
!Y2K  06.01.1998  TRPINIT IS YEAR 2000 COMPLIANT.
!
!----------------------------------------------------------------------

      PARAMETER (LTRACK=56664,MAXBLK=13999) ! 13999 HALFWORDS
      INTEGER*2 BLOCK(MAXBLK) /MAXBLK*0/    ! 27998 BYTES
      INTEGER*1 BLCK1(MAXBLK) /MAXBLK*0/
      CHARACTER*80 PARMIN
!----------------------------------------------------------------------
! READ PARM AS CHARACTERS, THEN NUMBERS
!----------------------------------------------------------------------

      CALL PARM(1,PARMIN)
      READ (PARMIN,*) NCYLS,LB,IXBLKS

!----------------------------------------------------------------------
! FIND HOW MANY BLOCKS FROM SIZE IN CYLS
!----------------------------------------------------------------------

      NBT=(LTRACK+500)/(LB+500)  ! NUMBER OF BLOCKS PER TRACK
      NBLOKS=NCYLS*15*NBT  ! TOTAL NUMBER OF BLOCKS (IF 15 TRACKS/CYL)

!----------------------------------------------------------------------
! SET PARAMETERS AT START & END OF MAP
!----------------------------------------------------------------------

      BLCK1(1)=0           ! NO. OF IDS
      BLCK1(2)=IXBLKS+1    !TOTAL NUMBER OF INDEX BLOCKS
      BLCK1(3)=2           ! NEXT FREE INDEX BLOCK
      BLOCK(1)=IXBLKS+28   !TOTAL NUMBER OF DATABLKS
      BLOCK(2)=IXBLKS+2    !FIRST FREE DATABLK
      WRITE (1) BLCK1(1),BLCK1(2),BLCK1(3),(BLOCK(N),N=1,LB/2-3)

      DO N=1,2
        BLOCK(N)=0           ! RESET FIRST 5 HALFWORDS TO ZERO
      ENDDO

      DO I=2,NBLOKS-1
        WRITE (1) (BLOCK(N),N=1,LB/2)  ! INITIALISE REST OF BLOCKS
      ENDDO

      STOP
      END
//LKED.MP DD DSN=MET.PROGLIB,DISP=SHR
 INCLUDE MP(CLPARM)
//INIT PROC CYLS=
// EXEC FORT2G,
// PARM='&CYLS,&BLOCK,&INDBLOKS/'
//GO.FT01F001 DD DSN=&DSN,DISP=(,CATLG),VOL=SER=&DISK,
// DCB=(RECFM=F,BLKSIZE=&BLOCK),UNIT=DISK,SPACE=(CYL,&CYLS)
// PEND
// EXEC INIT,DSN='MDB.HEALTHRR.TEST',DISK=OPR011,CYLS=2,BLOCK=4096,
//    INDBLOKS=1
//* THE DISK= PARM SHOULD BE SET AS FOLLOWS;
//* FOR TEST DATASETS USE OPR011
//* FOR OPERATIONAL DATASETS USE OPR103
