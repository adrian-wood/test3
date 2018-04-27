      SUBROUTINE GTGRP(SP,STACK,BASE,TMPELM,IREP,NUMBER,IERR)

!-----------------------------------------------------------------------
! SUBROUTINE    : GTGRP
!
! PURPOSE       : To get element name(s) from the current innermost
!                 nesting level, replicating if necessary.
!
! DESCRIPTION   : Put characters from BASE to SP in STACK into TMPELM
!                 as one name or several names, replicated if necessary.
!                 If there are other names on the stack (BASE>0) put all
!                 those just handled back on the stack for another level
!                 of nesting to be dealt with.
!
! CALLED BY     : EXPELM
!
! CALLS         : ITERAT to replicate
!                 PUSH & POP to put characters on stack or take them off
!
! PARAMETERS    : SP      stack pointer (>0)                      (i/o)
!                 STACK   stack                                   (i/o)
!                 BASE    points to start of name or open bracket  (i)
!                 TMPELM  array of element names                   (o)
!                 IREP    replication count                        (i)
!                 NUMBER  (input) dimension of TMPELM             (i/o)
!                         (output) number of names after replication
!                 IERR    return code (=3 if name too long,        (o)
!                                      =5 if string too long)
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:45$
! $Source: /data/us0400/mdb/op/lib/source/RCS/gtgrp.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:45    Sheila Needham  
! $
! Revision 2.1  2002/11/04  14:59:01  14:59:01  usmdb (Generic MetDB account)
! 18 Nov 2002    C Long
! 2.1  Describe arguments & variables, use DO WHILE instead of GO TO
!      - and comment the code!  
!      
! 
! Revision 2.0  2001/01/08  11:58:45  11:58:45  usmdb (Generic MetDB account)
! Removed unused variable COUNT. Added copyright and modified
! header - S.Cox
!
! Revision 1.4  97/08/04  13:11:46  13:11:46  uspm (Pat McCormack)
! First revisioned version for 1  - with Y2K change
!
! Revision 1.3  1997/07/25 14:55:27  uspm
! Declare types for i,k to satisfy implicit none
!
! Revision 1.2  1997/07/25 14:26:47  uspm
! Latest version from 1  dated 21-7-97
!
! Revision 1.1  1997/02/11 14:50:06  uspm
! Initial revision
!
! 21-07-97 !A  Increase size of ELEM from 400 characters to 1000.
!              Allows more element names within a replication
!              group - S.Cox
!
! 19-07-93     CHECK THAT MAXIMUM LENGTH OF ELEMENT NAME DOES NOT
!              EXCCED 32 CHARACTERS.
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

! Arguments in order (see above)

      INTEGER          SP
      CHARACTER*1      STACK(*)
      INTEGER          BASE
      CHARACTER*36     TMPELM(*)
      INTEGER          IREP
      INTEGER          NUMBER
      INTEGER          IERR

      INTEGER          TOP        ! last character to handle in STACK
      INTEGER          I          ! loop variable
      INTEGER          K          ! loop variable

      CHARACTER*1000   ELEM       ! string for characters from STACK
      CHARACTER*132    HEAD

      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/gtgrp.f,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:22:45$ '


! POP stack down to previous level, putting characters in ELEM

      TOP=SP-BASE+1
      IF (TOP.LE.LEN(ELEM)) THEN
        DO I=TOP,1,-1
          CALL POP (SP,ELEM(I:I),STACK,IERR)
          IF (IERR.EQ.6) RETURN              ! stack underflow
        ENDDO

! Replicate names if necessary, into TMPELM

        IF (ELEM(1:1).EQ.'(' .OR. IREP.GT.0) THEN
          CALL ITERAT(ELEM(1:TOP),IREP,TMPELM,NUMBER,IERR)

! Or put single element in TMPELM (error if it's too long)

        ELSE
          IF (TOP.GT.32) THEN
            IERR=3
          ELSE
            NUMBER=1
            TMPELM(NUMBER)(1:36)=ELEM(1:TOP)
          ENDIF
        ENDIF
        IF (IERR.GT.0) RETURN                ! name too long, too many?

! If anything is left on STACK (after POPs above), then replications
! are nested.  So put everything back on the stack & continue.

        IF (SP.GT.0) THEN                    ! if stack pointer >0
          DO I=1,NUMBER                      ! loop round names
            CALL PUSH(SP,' ',STACK,IERR)     ! space between names

! Loop round characters in each name till space or end of string until
! all names just put in TMPELM are back on STACK, separated by spaces.

            K=1
            DO WHILE (TMPELM(I)(K:K).NE.' ' .AND. K.LE.36)
              CALL PUSH(SP,TMPELM(I)(K:K),STACK,IERR)
              IF (IERR.EQ.5) RETURN          ! stack overflow
              K=K+1
            ENDDO
          ENDDO
        ENDIF

! Error if ELEM not big enough for all element names.

      ELSE
        IERR=5
      ENDIF
      RETURN
      END
