!-----------------------------------------------------------------------
!
! ROUTINE       : MDBINIT
!
! PURPOSE       : INITIALISE MDB DATA SET - SET PARAMETERS ON EXEC
!                 CARD AT END (NO SEQUENCE IN SECOND BLOCK - USE
!                 MDBINITS IF ONE IS WANTED)
! REVISION INFO :
!
! $Revision: 1$
! $Date: 28/02/2006 12:07:35$
! $Source: /home/us0400/mdb/op/lib/utility/RCS/mdbinit.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         28/02/2006 12:07:35    Sheila Needham  
! $
! Revision 2.0  2001/11/09 14:47:26  usmdb
! Changes made for f95 compilation without errors
! and warnings - S.Cox
!
! Revision 1.2  2001/08/22 13:53:54  usmdb
! Due to library reorganisation - S.Cox
!
! Revision 1.1  97/10/09  12:39:28  12:39:28  uspm (Pat McCormack)
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

      INTEGER LTRACK
      PARAMETER (LTRACK=56664)                                      !2.0

      INTEGER I
      INTEGER NBLOKS
      INTEGER NBT
      INTEGER NOFLOS
      INTEGER IOFSET
      INTEGER IBKHRS
      INTEGER IXBLKS
      INTEGER LENTRY
      INTEGER LB
      INTEGER NCYLS

      CHARACTER*2     CHAR2   ! Function                            !2.0
      CHARACTER*27998 BLOCK   ! Buffer to hold one block of ds      !2.0
      CHARACTER*27998 ZEROES  ! String of binary zeroes             !2.0
      CHARACTER*132   HEAD

#if defined (MVS)
      CHARACTER*80 PARMIN
#else
      INTEGER IERROR
      NAMELIST / STORE_DETAILS /
     & NCYLS,LB,LENTRY,IXBLKS,IBKHRS,IOFSET,NOFLOS
#endif 

      DO I=1,27998                                                  !2.0
         ZEROES(I:I) = CHAR(0)                                      !2.0
      END DO ! I                                                    !2.0

      HEAD='$RCSfile: mdbinit.F,v $ ' //
     &     '$Revision: 1$ $Date: 28/02/2006 12:07:35$'

#if defined (MVS)
                           ! READ PARM AS CHARACTERS, THEN NUMBERS
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
                           ! FIND HOW MANY BLOCKS FROM SIZE IN CYLS
      NBT=(LTRACK+500)/(LB+500)  ! NUMBER OF BLOCKS PER TRACK
      NBLOKS=NCYLS*15*NBT  ! TOTAL NUMBER OF BLOCKS (IF 15 TRACKS/CYL)
                           ! SET PARAMETERS AT START & END OF MAP
      BLOCK = ZEROES                                                !2.0
      BLOCK(1:2) = CHAR2(NBLOKS)       ! TOTAL NO. BLOCKS IN DS     !2.0
      BLOCK(3:4) = CHAR2(IXBLKS)       ! NO. INDEX BLOCKS IN DS     !2.0
      BLOCK(5:6) = CHAR2(IBKHRS)       ! NO. HOURS IN INDEX BLOCK   !2.0
      BLOCK(7:8) = CHAR2(IOFSET)       ! HR 1 IN 1ST IXBLK AFTER 0Z !2.0
      BLOCK(LB-3:LB-2) = CHAR2(NOFLOS) ! NO. OVERFLOW RECORDS       !2.0
      BLOCK(LB-1:LB) = CHAR2(LENTRY)   ! LENGTH OF INDEX ENTRY      !2.0

#if defined (MVS)
      WRITE(1)BLOCK(1:LB)                                           !2.0
#else
      WRITE(1,REC=1,IOSTAT=IERROR)BLOCK(1:LB)                       !2.0
#endif

      DO I=1,NBLOKS-1                                               !2.0
#if defined (MVS)
        WRITE(1)ZEROES(1:LB)   ! INITIALISE REST OF BLOCKS          !2.0
#else
        WRITE(1,REC=I+1,IOSTAT=IERROR)ZEROES(1:LB)                  !2.0
#endif
      ENDDO                                                         !2.0

      STOP
      END
