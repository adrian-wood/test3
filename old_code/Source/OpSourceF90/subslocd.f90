SUBROUTINE SUBSLOCD(SUBTYPE,BULL,SEQDES)                        !A

!-----------------------------------------------------------------------
!
! program       : SUBSLOCD
!
! purpose       : To expand a BUFR message descriptor section
!               : and compare with local sequence descriptors
!               : held in memory (locald) to determine which local
!               : sequence can be used to describe the data.
!
! description   : The BUFR message is passed to SUBLOCD. section 3 is
!               : found, and the descriptors fully expanded (in case
!               : there are any sequence descriptors). The local
!               : sequence descritors already reside in memory.
!               : The expanded BUFR message
!               : descriptors are compared with the LOCALD descriptors,
!               : and if a match found, the LOCALD sequence descriptor
!               : is returned. I not, 0 is returned.
!
! called by     : SSMRET (SAT120, RTOVS), BUSRET (AMDARS, DRIFTR)
!
! calls         : DESFXY   : convert integer descriptor to FXXYYY
!               : LOCALD   : to expand sequence descriptor
!               : TABLED   : expand any table D descriptors
!
! arguments     : (1) Subtype                              (ip)
!               : (2) BUFR message                         (ip)
!               : (3) LOCALD sequence descriptor           (op)
!
!Y2K  26.06.1997  SUBSLOCD is Year 2000 compliant.
!
! revision info :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/subslocd.F,v $
!
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:59:17  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.4  97/09/10  15:44:30  15:44:30  uspm (Pat McCormack)
! Added subtype RTOVS
!
! Revision 1.3  1997/08/04 13:34:53  uspm
! First revisioned version for COSMOS - with Y2K change
!
! Revision 1.2  1997/04/07 12:39:30  uspm
! Add $Log
!
! Revision 1.1
! Initial Revision
!
! 15-09-97  !C  : Added subtype RTOVS - S.Cox
!
! 05-03-97  !B  : Added subtype DRIFTR/BUOY - S.Cox
!
! 17-02-97  !A  : re-named and expaded to cope with data types other
!               : other than just SAT120
!
! 04-12-96      : created S.Cox
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

!-----------------------------------------------------------------------
! declare variables.
!-----------------------------------------------------------------------

INTEGER        BSQ           !- BUFR edition displacement (0 or 4)
INTEGER        DESCR         !- descriptor from BUFR message
INTEGER        DESLST(8,10)  !- sequence descriptors
INTEGER        EXPDESCR(200) !- expanded BUFR message descriptors
INTEGER        F             !- BUFR descriptor F of FXXYYY
INTEGER        FLOPT         !- optional section 2 indicator
INTEGER        I,J           !- loop counters
INTEGER        L1,L2,L3,L4   !- length of BUFR sections 1-4
INTEGER        MINISEQ(30)   !- mini descriptor sequence
INTEGER        NMINISEQ      !- no. of mini descriptors
INTEGER        ND            !- no. of BUFR descriptors
INTEGER        NX            !- no. of expanded BUFR descriptors
INTEGER        NSEQ          !- no of expanded MDB descriptors
INTEGER        PTR           !- pointer
INTEGER        ROW           !- row in DESLST array
INTEGER        SEQ(200)      !- expanded MDB message descriptors
INTEGER        SEQDES        !- input sequence descriptor
INTEGER        X             !- BUFR descriptor XX of FXXYYY
INTEGER        Y             !- BUFR descriptor YYY of FXXYYY

LOGICAL        FIRST         !- TRUE if first call to SSMRET
LOGICAL        FULLMATCH     !- FALSE if descriptors not matched

CHARACTER*4    BUFR          !- BUFR character string
CHARACTER*(*)  BULL          !- input BUFR message
CHARACTER*(*)  SUBTYPE       !- MetDB subtype

!-----------------------------------------------------------------------
! Dynamic common
!-----------------------------------------------------------------------

COMMON /SUBLD1/EXPDESCR,MINISEQ,SEQ

!-----------------------------------------------------------------------
! SAVE statement to ensure all variables are still set on next call
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! Data statements
!-----------------------------------------------------------------------

DATA BUFR  /'BUFR'/        !- initialise BUFR in EBCDIC
DATA FIRST /.TRUE./        !- TRUE for first call

DATA DESLST /2,311194,311195,0,0,0,0,0,&           !- AMDARS
            &3,310208,310209,310199,0,0,0,0,&      !- SAT120
            &4,331196,331197,331198,331199,0,0,0,& !- DRIFTR    !B
            &3,310208,310209,310199,0,0,0,0,&      !- RTOVS     !C
            &0,0,0,0,0,0,0,0,&                     !- spare
            &0,0,0,0,0,0,0,0,&                     !- spare
            &0,0,0,0,0,0,0,0,&                     !- spare
            &0,0,0,0,0,0,0,0,&                     !- spare
            &0,0,0,0,0,0,0,0,&                     !- spare
            &0,0,0,0,0,0,0,0/                      !- spare

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/subslocd.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '


!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

L1=0                       !- length of section 1
L2=0                       !- length of section 2
L3=0                       !- length of section 3
L4=0                       !- length of section 4

SEQDES=0                   !- initialise sequence descriptor.

!-----------------------------------------------------------------------
! convert EBCDIC declared 'BUFR' to ASCII on an EBCDIC machine (IBM)
! on first call to SUBSLOCD only.
!-----------------------------------------------------------------------

IF (FIRST) THEN
  FIRST=.FALSE.
  CALL EB2ASC(4,BUFR)
ENDIF

!-----------------------------------------------------------------------
! find 'BUFR' in the message
!-----------------------------------------------------------------------

PTR=INDEX(BULL,BUFR)

IF (PTR.EQ.0) RETURN

!-----------------------------------------------------------------------
! determine the BUFR edition number of the message
!-----------------------------------------------------------------------

BSQ=0                                       !- BUFR edition 0 or 1
IF (ICHAR(BULL(PTR+7:PTR+7)).GE.2) BSQ=4    !- BUFR edition 2 or 3

!-----------------------------------------------------------------------
! skip BUFR section 0
!-----------------------------------------------------------------------

PTR=PTR+4+BSQ

!-----------------------------------------------------------------------
! skip BUFR section 1 and check if there is a optional section 2.
!-----------------------------------------------------------------------

L1=ICHAR(BULL(PTR+1:PTR+1))*256+ICHAR(BULL(PTR+2:PTR+2))
FLOPT=ICHAR(BULL(PTR+7:PTR+7))
PTR=PTR+L1

!-----------------------------------------------------------------------
! skip BUFR section 2 if there is one.
!-----------------------------------------------------------------------

IF (FLOPT.GE.128) THEN
  L2=ICHAR(BULL(PTR+1:PTR+1))*256+ICHAR(BULL(PTR+2:PTR+2))
  PTR=PTR+L2
ENDIF

!-----------------------------------------------------------------------
! find the number of descriptors in BUFR section 3.
!-----------------------------------------------------------------------

L3=ICHAR(BULL(PTR+1:PTR+1))*256+ICHAR(BULL(PTR+2:PTR+2))
ND=(L3-7)/2

!-----------------------------------------------------------------------
! loop over BUFR message descriptors. Expand any sequence descriptors.
!-----------------------------------------------------------------------

NX=0
DO I=1,ND
  DESCR=ICHAR(BULL(PTR+7+(2*(I-1)):PTR+7+(2*(I-1))))*256+&
       &ICHAR(BULL(PTR+8+(2*(I-1)):PTR+8+(2*(I-1))))
  CALL DESFXY(DESCR,F,X,Y)
  IF (F.EQ.3) THEN
    CALL TABLED(X,Y,MINISEQ,NMINISEQ)
    DO J=1,NMINISEQ
      NX=NX+1
      EXPDESCR(NX)=MINISEQ(J)
    END DO
  ELSE
    NX=NX+1
    EXPDESCR(NX)=DESCR
  ENDIF
END DO

!-----------------------------------------------------------------------
! compare local sequences against BUFR message expanded descriptors
! to see if they match. First match the subtype to get the correct row
! in the DESLST array, then loop over the number of descripors for
! that subtype, comparing each with the expanded descriptor sequence.
!-----------------------------------------------------------------------

ROW=0
IF (SUBTYPE(1:6).EQ.'AMDARS') ROW=1
IF (SUBTYPE(1:6).EQ.'SAT120') ROW=2
IF (SUBTYPE(1:6).EQ.'DRIFTR' .OR. SUBTYPE(1:4).EQ.'BUOY') ROW=3 !B
IF (SUBTYPE(1:5).EQ.'RTOVS')  ROW=4                             !C

IF (ROW.GT.0) THEN     !- i.e. subtype matched

  DO I=2,DESLST(1,ROW)+1

    SEQDES=DESLST(I,ROW)

    CALL LOCALD(MOD((SEQDES/1000),100),MOD(SEQDES,1000),&
               &SEQ,NSEQ,' ',' ')

    IF (NX.EQ.NSEQ) THEN
      FULLMATCH=.TRUE.
      DO J=1,NSEQ
        IF (EXPDESCR(J).NE.SEQ(J)) THEN
          FULLMATCH=.FALSE.
        ENDIF
      ENDDO
      IF (FULLMATCH) RETURN       !- matched sequence descriptor.
    ENDIF

  ENDDO  !- i loop

ENDIF  !- row.gt.0

SEQDES=0  !- If we get to here, no matches at all !!!

RETURN
END SUBROUTINE SUBSLOCD
