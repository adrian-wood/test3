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
! CALLS         : IVALUE                                              !
!                                                                     !
! PARAMETERS    : (1) string of figures etc                        (I)!
!                 (2) pointer                                     (I/O)
!                    (input: points to any spaces etc before group    !
!                      - but points to first figure if no spaces)     !
!                    (output: points past group, to space if there is !
!                     one, or next figure if it follows immediately)  !
!                 (3) number of figures in value (if zero, pointer    !
!                     is just moved to next group)                 (I)!
!                                                                     !
! Revision Info:                                                      !
! $Revision: 1$
! $Date: 11/10/2006 11:56:56$ 
! $Source: /home/us0400/mdb/op/lib/other/MDB.CREX.SRCE/RCS/crexval.f,v $ 
!                                                                     !
! CHANGE RECORD:                                                      !
! $Log:
!  1    Met_DB_Project 1.0         11/10/2006 11:56:56    Kudsia Gwangwaa 
! $
! Revision 1.1  2002/10/22 10:01:57  usmdb
! Initial revision
!
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


      INTEGER FUNCTION CREXVAL(FIGS,NBEFOR,NINVAL)
      CHARACTER*(*) FIGS
      INTEGER NBEFOR  ! points to start of group on entry
      INTEGER NINVAL  ! number of figures in value (not counting minus)
      INTEGER IVALUE  ! function to convert given number of figures
      LOGICAL MINUS
! ---------------------------------------------------------------------
! Revision Information: 
! ---------------------------------------------------------------------
      LOGICAL HEADSET
      CHARACTER*132 HEAD
      DATA HEADSET/.FALSE./
      
      IF (.NOT. HEADSET) THEN
        HEAD='$RCSfile: crexval.f,v $ '/
     &   '$Revision: 1$ $Date: 11/10/2006 11:56:56$ '
        HEADSET = .TRUE.
      ENDIF

! Go past any spaces etc before the next figure/letter/slash/minus

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

! Convert the required number of figures
! (slashes or any other non-figures will give a value of -9999999)
! & move the pointer past the end of the group

      IF (NINVAL.GT.0) THEN
        CREXVAL=IVALUE(FIGS(NBEFOR:NBEFOR+NINVAL-1))
        IF (CREXVAL.NE.-9999999 .AND. MINUS) CREXVAL=-CREXVAL
        NBEFOR=NBEFOR+NINVAL  ! move pointer past figures
      ELSE
        CREXVAL=-9999999
      ENDIF

      RETURN
      END
