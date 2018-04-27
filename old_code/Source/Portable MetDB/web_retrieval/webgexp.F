      SUBROUTINE WEBGEXP(CREP,CREP2,RPRTEND,LOOKUP)

!-----------------------------------------------------------------------
!
! routine       : WEBGEXP
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! descrition    : To produxe a CREP2 report text from a CREP report
!               : text by expanding 3 character groups e.g. '333' to
!               : 5 character groups e.g. ' 333 '
!
! arguments     :
!
! CREP          : char*(*)   (ip) : input text string
! CREP2         : char*(*)   (op) : output text string (expanded)
! RPRTEND       : integer    (ip) : pointer to end of text in CREP
! LOOKUP        : char*(*)   (ip) : 3 character group to expand
!
! data types    : WEBLND, WEBNCM, WEBSHP
!
! calls         : none
!
! change record :
!
! 29-07-1998    : Made operational - S.Cox
!
!-----------------------------------------------------------------------
! $Log:
!  1    Met_DB_Project 1.0         21/09/2006 11:30:40    Stan Kellett    
! $
! Revision 1.2  2001/01/18 09:48:44  usmdb
! Corrected "array out of bounds error" - caused when group
! e.g. "ICE" appears at end of report - S.Cox
!
! Revision 1.1  1998/07/29 08:06:23  usmdb
! Initial revision
!
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER       J            !- position of 3 char group in CREP
      INTEGER       RPRTEND      !- end of report text in CREP

      CHARACTER*(*) CREP         !- input report text
      CHARACTER*(*) CREP2        !- output report text (expanded)
      CHARACTER*132 HEAD         !- RCS info
      CHARACTER*(*) LOOKUP       !- 3 character group to expand

!-----------------------------------------------------------------------
! RCS revision information.
!-----------------------------------------------------------------------

      HEAD = '
     &$Source: /net/home/h01/mdb_new/op/lib/web_source/RCS/webgexp.F,v $
     &'//'$Date: 21/09/2006 11:30:40$ $Revision: 1$'

!-----------------------------------------------------------------------
! Find position of 3-character group in CREP. If found, expand the
! input report text - putting in output report text. Added check for
! group being at end of report. IF so, don't expand.                !1.2
!-----------------------------------------------------------------------

      J = INDEX(CREP,LOOKUP)
      IF (J.GT.0 .AND. ((RPRTEND-J).GT.4)) THEN                     !1.2
        CREP2(:) = CREP(:)
        CREP2(J+7:RPRTEND+2) = CREP(J+5:RPRTEND)
        CREP2(J+5:J+6)    = ' '
        CREP(:)  = CREP2(:)
        RPRTEND  = RPRTEND+2
      ENDIF

      RETURN
      END
