      SUBROUTINE ITERAT(ELEM,IREP,TMPELM,NUMBER,IERR)

!-----------------------------------------------------------------------
!
! ROUTINE       : ITERAT
!
! PURPOSE       : to put names from request in an array & replicate
!                 them with instance numbers on end
!
! CALLED BY     : GTGRP
!
! CALLS         : none
!
! PARAMETERS    : ELEM   element name(s) in string                  (i)
!                 IREP   number of replications                     (i)
!                 TMPELM array of element names                     (o)
!                 NUMBER  (input) dimension of TMPELM             (i/o)
!                         (output) number of names after replication
!                 IERR   return code (=3 if a name is too long,     (o)
!                                     =4 if >50 names to replicate)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:03$
! $Source: /home/us0400/mdb/op/lib/source/RCS/iterat.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:03    Sheila Needham  
! $
! Revision 2.3  2003/05/06 08:05:25  usmdb
! 19 May 2003     C Long
! 2.3  Don't stop if there are more than 50 names (& only 50 lengths kept):
!      delimit any further names by INDEX operations.
!
! Revision 2.2  2002/11/05  14:37:12  14:37:12  usmdb (MetDB account c/o usjh)
! 18 Nov 2002    C Long
! 2.2 Describe arguments & variables, use DO WHILE instead of GO TO
! !      - and comment the code!
!
! Revision 2.1  2002/02/04  11:46:24  11:46:24  usmdb (Generic MetDB account)
! Out of bounds on HP for single replications e.g. PRCT_CNFC*7
! Added check on length of string to prevent out of bounds - S.Cox
!
! Revision 2.0  2001/01/08 11:58:49  usmdb
! Removed unused variables NEXT & NUMBER. Added
! copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:13:09  13:13:09  uspm (Pat McCormack)
! First revisioned version for 1  - with Y2K change
!
! Revision 1.1  1997/02/11 16:20:39  uspm
! Initial revision
!
! 26-09-96  A  : S.Cox - added CNUM3 and CNUM4 so replications up
!                      - to 9999 can now be handled
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

! END(50) may overflow (ELEM*1000 & STACK*1(1000) too) if more nesting

      INTEGER           NUMBER      ! number of names after replication
      INTEGER           END(50)     ! length of each name
      INTEGER           I           ! pointer to string, then loop var
      INTEGER           IERR        ! return code
      INTEGER           IREP        ! replication count
      INTEGER           J           ! loop variable
      INTEGER           LENGTH      ! length of name
      INTEGER           NELEM       ! number of names found in string
      INTEGER           START       ! start of name in string

      LOGICAL           MANY        ! set if '(', so may be >1 name
      LOGICAL           DONE        ! set if no more names to find
      LOGICAL           FIRST       ! unset once revision header set

      CHARACTER*4       CNUM        ! instance number in figure(s)
      CHARACTER*(*)     ELEM        ! string from request
      CHARACTER*36      TMPELM(*)   ! array of names
      CHARACTER*132     HEAD

      DATA              FIRST/.TRUE./                               !2.1

      IF (FIRST) THEN                                               !2.1
        FIRST=.FALSE.                                               !2.1
        HEAD='$RCSfile: iterat.f,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:23:03$ '
      ENDIF                                                         !2.1

      NELEM=0
      DONE=.FALSE.

! If the string starts (& ends) with brackets, there is probably more
! than one name

      IF (ELEM(1:1).EQ.'(') THEN
        MANY=.TRUE.
        START=2
      ELSE
        MANY=.FALSE.
        START=1
      ENDIF
      I=START

! Loop round names (if MANY; outer loop) & round characters in name
! (inner loop) looking for delimiter.

      DO WHILE (.NOT.DONE)
        DO WHILE ( I.LE.LEN(ELEM) .AND. ELEM(I:I).NE.' '           !2.2
     &            .AND. ELEM(I:I).NE.')')                          !2.2
          I=I+1
        ENDDO
        IF (I.GE.LEN(ELEM) .OR. ELEM(I:I).EQ.')') DONE=.TRUE.      !2.2

! Keep name (if not too long) & length of name (if not too many)   !2.3

        LENGTH=I-START
        IF (LENGTH.GT.32) THEN
          IERR=3
          RETURN
        ELSE IF (LENGTH.GT.0) THEN
          NELEM=NELEM+1
          TMPELM(NELEM)=ELEM(START:I-1)
          IF (NELEM.LE.50) END(NELEM)=LENGTH                       !2.3
        ENDIF

! If there could be more than one name in brackets, look on.

        IF (.NOT.MANY) THEN
          DONE=.TRUE.                ! to drop out of loop
        ELSE IF (.NOT.DONE) THEN
          IF (ELEM(I:I).EQ.' ') I=I+1
          START=I
        ENDIF
      ENDDO

! The names in the request have been listed: now add numbers.      !2.3

      IF (IREP*NELEM.GT.NUMBER) THEN
        IERR=7
      ELSE IF (IERR.EQ.0) THEN
        NUMBER=0
        DO I=1,IREP
          WRITE (CNUM,'(I4)') I
          DO J=1,NELEM
            NUMBER=NUMBER+1

! Only 50 lengths are kept above; delimit any further names by     !2.3
! looking for a space or _1 followed by a space.                   !2.3
! The original NELEM names are delimited by spaces on the end.     !2.3
! But _1 is added, so when copying the first NELEM names (with     !2.3
! different tags) look for _1 followed by space.  (Must be at      !2.3
! least two spaces on end of max *32 name in *36 string.)          !2.3

            IF (J.LE.50) THEN                                      !2.3
              LENGTH=END(J)                                        !2.3
            ELSE                                                   !2.3
              IF (I.EQ.1) THEN                                     !2.3
                LENGTH=INDEX(TMPELM(J),' ')-1                      !2.3
              ELSE                                                 !2.3
                LENGTH=INDEX(TMPELM(J),'_1 ')-1                    !2.3
              ENDIF                                                !2.3
            ENDIF                                                  !2.3

            IF (I.LE.9) THEN
              TMPELM(NUMBER)=TMPELM(J)(1:LENGTH)//'_'//CNUM(4:4)   !2.3
            ELSE IF (I.LE.99) THEN
              TMPELM(NUMBER)=TMPELM(J)(1:LENGTH)//'_'//CNUM(3:4)   !2.3
            ELSE IF (I.LE.999) THEN
              TMPELM(NUMBER)=TMPELM(J)(1:LENGTH)//'_'//CNUM(2:4)   !2.3
            ELSE IF (I.LE.9999) THEN
              TMPELM(NUMBER)=TMPELM(J)(1:LENGTH)//'_'//CNUM        !2.3
            ENDIF
          ENDDO
        ENDDO
      ENDIF

      IF (IREP.EQ.0) NUMBER=NELEM
      RETURN
      END
