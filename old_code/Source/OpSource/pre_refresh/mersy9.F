      SUBROUTINE MERSY9(REPTXT,OUTMESS,REPLEN,REPLEN9)

      IMPLICIT NONE

!----------------------------------------------------------------------
!
! PROGRAM       : MERSY9
!
! PURPOSE       : To get 9-groups out of report text
!                 (all groups from first group starting with 9 in 333
!                  section to end of report)
!
! DATA TYPE(S)  : LNDSYN
!
! CALLED BY     : MERGE
!
! PARAMETERS    : (1) report text                                  (i)
!                 (2) string to be stored                         (i/o
!                      input: BUFR message at (1:REPLEN)
!                      output: 9-groups added at (REPLEN+1:)
!                              & message copied at (REPLEN+REPLEN9+1:)
!                               (but not if there are no 9-groups!)
!                 (3) length of BUFR message in OUTMESS            (i)
!                 (4) length of 9-groups in output string          (o)
!
!----------------------------------------------------------------------
! $Log:
!  1    Met_DB_Project 1.0         28/02/2006 11:44:23    Sheila Needham  
! $
! Revision 1.2  2000/03/10  09:21:36  09:21:36  usmdb (Generic MDB account)
! Operational 20-03-2000 S.Cox
! Changed the checking on whether more SYNOP groups are
! to be used. This was to prevent out-of-bounds errors
! on HP-UX.
! 
! Revision 1.1  99/07/13  13:19:00  13:19:00  usmdb (Generic MDB account)
! Initial revision
!----------------------------------------------------------------------

      CHARACTER REPTXT*(*)
      CHARACTER OUTMESS*(*)
      INTEGER REPLEN
      INTEGER REPLEN9
      INTEGER ISEC3
      INTEGER I,J
      LOGICAL MORE_DATA                                            !1.2
      LOGICAL START9
      CHARACTER HEAD*132        ! revision information

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/merge/RCS/mersy9.F,v $
     &'//'$ $Date: 28/02/2006 11:44:23$ $Revision: 1$'

      J=0                     ! NUMBER OF NINE GROUPS KEPT
      ISEC3=INDEX(REPTXT,' 333 ')
      IF (ISEC3.GT.0) THEN
        I=ISEC3+5             ! START OF FIRST GROUP IN 333 SECTION
        START9=.FALSE.        ! SET WHEN FIRST 9-GROUP FOUND

        MORE_DATA = (I+4.LE.LEN(REPTXT))                           !1.2
        DO WHILE (MORE_DATA)                                       !1.2

          IF (REPTXT(I:I).EQ.'9' .AND. .NOT.START9) START9=.TRUE.
          IF (START9) THEN    ! COPY FROM FIRST 9-GROUP TO SECTION END
            J=J+1
            OUTMESS(REPLEN+J*6-5:REPLEN+J*6-1)=REPTXT(I:I+4)       !1.2
            OUTMESS(REPLEN+J*6:REPLEN+J*6)=' '                     !1.2
          ENDIF
          I=I+6   !- move to start of next 5-figure group.

!----------------------------------------------------------------------
! Check whether we want to continue looping over report. 1st check
! that the end of the 5-fig group is not beyond the end of the report
! text. If not, check that if there is more data beyond the end of the
! group, that there is a space in the correct position. If not,
! we are dealing with a 3-figure group 555 i.e. end of 9-groups.   !1.2
!----------------------------------------------------------------------

          IF (I+4.GT.LEN(REPTXT)) THEN                             !1.2
            MORE_DATA = .FALSE.                                    !1.2
          ELSE                                                     !1.2
            IF (I+5.LT.LEN(REPTXT)) THEN                           !1.2
              IF (REPTXT(I+5:I+5).NE.' ') MORE_DATA = .FALSE.      !1.2
            ENDIF                                                  !1.2
          ENDIF                                                    !1.2

        ENDDO

        IF (J.GT.0) THEN
          OUTMESS(REPLEN+J*6+1:REPLEN+J*6+REPLEN)=OUTMESS(1:REPLEN)
          print *,' MERSY9:',J,'9-groups kept from ',REPTXT(ISEC3:)
        ENDIF
      ENDIF
      REPLEN9=J*6

      RETURN
      END
