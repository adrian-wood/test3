      SUBROUTINE DATIMGP(GROUP,BADTAF)

!-----------------------------------------------------------------------
!
! ROUTINE       : DATIMGP
!
! PURPOSE       : To check that a group is a possible time group,
!                 i.e. that it has either 4 or 6 figures followed
!                 by either space or Z.
!
! CALLED BY     : TAFBUL
!
! CALLS         : IVALUE
!
! PARAMETERS    : (1) GROUP    (start of) group to be checked
!                 (2) BADTAF   flag set if not 4 or 6 figures
!                              or not space or Z after them
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:56$
! $Source: /home/us0400/mdb/op/lib/source/RCS/datimgp.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:56    Sheila Needham  
! $
! Revision 2.0  2002/01/16 09:51:19  usmdb
! Initial revision.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER GROUP*(*)
      LOGICAL   BADTAF
      INTEGER   IVALUE         ! function to convert figures to number
      INTEGER   N              ! =4 or =6 if that many figures, else 0
      CHARACTER HEAD*132
      LOGICAL   FIRST
      DATA      FIRST/.TRUE./

      IF (FIRST) THEN
        HEAD='$RCSfile: datimgp.F,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:21:56$'
        FIRST=.FALSE.
      ENDIF

!-----------------------------------------------------------------------
! See if there are 4 figures (IVALUE returns missing data if not).
! If so, see if there are 6.  In either case see if space or Z follows.
!-----------------------------------------------------------------------

      BADTAF=.FALSE.
      N=0
      IF (IVALUE(GROUP(1:4)).GE.0) N=4
      IF (N.EQ.0) THEN
        BADTAF=.TRUE.
      ELSE
        IF (IVALUE(GROUP(5:6)).GE.0) N=6
        IF (GROUP(N+1:N+1).NE.' ' .AND. GROUP(N+1:N+1).NE.'Z') THEN
          BADTAF=.TRUE.
        ENDIF
      ENDIF

      RETURN
      END
