      INTEGER FUNCTION CREXVAL(FIGS,NINVAL,NBEFOR)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                     !
! PROGRAM       : CREXVAL                                             !
!                                                                     !
! PURPOSE       : to get integer from figures in CREX data section    !
!                 (from NINVAL FIGS after NBEFOR-1 characters)        !
!                                                                     !
! DESCRIPTION   : function returns -9999999 if not figures,           !
!                 NBEFOR is returned pointing to next character       !
!                                                                     !
! CALLED BY     : CREXDEC, CREXDAT                                    !
!                                                                     !
! PARAMETERS    :                                                     !
!    (1) FIGS    string of figures etc                             (I)!
!    (2) NINVAL  number of figures in value (if zero, pointer is      !
!                 just moved to next group) not counting minus     (I)!
!    (3) NBEFOR  pointer                                          (I/O)
!                    (input: points to any spaces etc before group    !
!                      - but points to first figure if no spaces)     !
!                    (output: points past group, to space if there is !
!                     one, or next figure if it follows immediately)  !
!                                                                     !
! Revision Info:                                                      !
! $Revision: 1$
! $Date: 30/01/2006 20:21:55$
! $Source: /home/us0400/mdb/op/lib/source/RCS/crexval.f,v $
!                                                                     !
! CHANGE RECORD:                                                      !
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:55    Sheila Needham  
! $
! Revision 1.1  2003/03/27 14:43:25  usmdb
! Initial revision
!
! Revision 1.1  2002/10/22 10:01:57  usmdb
! Initial revision
!                                                                     !
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.         !
!                                                                     !
! Met Office, United Kingdom                                          !
!                                                                     !
! The use, duplication and disclosure of this code is strictly        !
! prohibited without the permission of The Meteorological Database    !
! Team at the above address.                                          !
!                                                                     !
! ---------------------------------------------------------------------

      CHARACTER*(*) FIGS  ! argument (1)
      INTEGER NINVAL      ! argument (2)  in value (not counting minus)
      INTEGER NBEFOR      ! argument (3)
      INTEGER I           ! loop variable
      LOGICAL MINUS       ! set if minus sign at start of value

      LOGICAL HEADSET
      CHARACTER*132 HEAD
      DATA HEADSET/.FALSE./

      IF (.NOT. HEADSET) THEN
        HEAD='$RCSfile: crexval.f,v $ '//
     &   '$Revision: 1$ $Date: 30/01/2006 20:21:55$ '
        HEADSET = .TRUE.
      ENDIF

! Go past any spaces etc before the next figure/letter/slash/minus,
! skipping any character which is less than a figure (ASCII) or
! letter (EBCDIC) unless it is a slash or minus.

      DO WHILE (FIGS(NBEFOR:NBEFOR).LT.'0' .AND.
     &          FIGS(NBEFOR:NBEFOR).LT.'A' .AND.
     &          FIGS(NBEFOR:NBEFOR).NE.'/' .AND.
     &          FIGS(NBEFOR:NBEFOR).NE.'-')
        NBEFOR=NBEFOR+1  ! move pointer on till start of next group
      ENDDO

! Check for minus: if found, set flag & move pointer past it

      IF (FIGS(NBEFOR:NBEFOR).EQ.'-') THEN
        MINUS=.TRUE.
        NBEFOR=NBEFOR+1
      ELSE
        MINUS=.FALSE.
      ENDIF

! Convert the required number of figures & point past end of group.
! (Slashes or any other non-figures will give a value of -9999999.)
! Convert a figure at a time (getting a value from 0 to 9 by sub-
! tracting the ASCII or EBCDIC equivalent of zero from the equivalent
! of the figure concerned), building up a number by multiplying the
! result of converting the first n figures by 10 and adding the
! (n+1)-th at each step.

      IF (NINVAL.GT.0) THEN
        I=1
        CREXVAL=0
        DO WHILE (I.LE.NINVAL .AND. CREXVAL.NE.-9999999)
          IF (FIGS(NBEFOR+I-1:NBEFOR+I-1).GE.'0' .AND.
     &        FIGS(NBEFOR+I-1:NBEFOR+I-1).LE.'9') THEN
            CREXVAL=CREXVAL*10
     &              +ICHAR(FIGS(NBEFOR+I-1:NBEFOR+I-1))-ICHAR('0')
          ELSE
            CREXVAL=-9999999
          ENDIF
          I=I+1
        ENDDO
        IF (CREXVAL.NE.-9999999 .AND. MINUS) CREXVAL=-CREXVAL
        NBEFOR=NBEFOR+NINVAL  ! move pointer past figures
      ELSE
        CREXVAL=-9999999
      ENDIF

      RETURN
      END
