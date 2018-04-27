      SUBROUTINE WEBSHP(CREP,RPRTEND,WRITEHEADER,TOR,CREP2,IN_TEXT)

!-----------------------------------------------------------------------
!
! routine       : WEBSHP
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! purpose       : To format SHPSYN raw reports for web retrieval.
!
! description   : The SHPSYN raw report (including header) is passed
!               : to this routine. This routines formats it for web
!               : display.
!
! arguments     :
!
! CREP          : char*(*)   (ip) : input report text from MDB
! RPRTEND       : integer    (ip) : end of CREP.
! WRITEHEADER   : logical    (ip) : TRUE if report text header wanted
! TOR           : char*(*)   (ip) : report Time Of Receipt
! CREP2         : char*(*)   (ip) : expanded report text from WEBGEXP
! IN_TEXT       : char*(*)   (ip) : dummy - string initialised here.
!
! data types    : SHPSYN
!
! calls         : WEBFORM  : to output the final report text
!               : WEBGEXP  : to expand 3 character groups
!
! change record :
!
! 28-07-1998    : Made operational - S.Cox
!
!-----------------------------------------------------------------------
! $Log:
!  1    Met_DB_Project 1.0         21/09/2006 11:23:13    Stan Kellett    
! $
! Revision 1.2  2001/01/18 10:04:01  usmdb
! Changed format statement so output is correct on HP - S.Cox
!
! Revision 1.1  1998/07/29 08:06:40  usmdb
! Initial revision
!
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER         COL_END      !- end column of report on page
      INTEGER         COL_START    !- start column of report on page
      INTEGER         IPTR         !- report text pointer
      INTEGER         RPRTEND      !- length of input report text

      LOGICAL         FIRST        !- TRUE for 1st time in routine
      LOGICAL         WRITEHEADER  !- TRUE to output header

      CHARACTER*(*)   CREP         !- input report text
      CHARACTER*(*)   CREP2        !- text workspace (for expansion)
      CHARACTER*132   HEAD         !- RCS info
      CHARACTER*(*)   TOR          !- input time of receipt
      CHARACTER*(*)   IN_TEXT      !- text to preceed report on page

      DATA FIRST      /.TRUE./     !- 1st time in routine

!-----------------------------------------------------------------------
! RCS revision information.
!-----------------------------------------------------------------------

      HEAD = '
     &$Source: /net/home/h01/mdb_new/op/lib/web_source/RCS/webshp.F,v $
     &'//'$Date: 21/09/2006 11:23:13$ $Revision: 1$'

!-----------------------------------------------------------------------
! expand 3 figure groups below to 5 figure groups.
!-----------------------------------------------------------------------

      CALL WEBGEXP(CREP,CREP2,RPRTEND,' 222 ')
      CALL WEBGEXP(CREP,CREP2,RPRTEND,' 333 ')
      CALL WEBGEXP(CREP,CREP2,RPRTEND,' 444 ')
      CALL WEBGEXP(CREP,CREP2,RPRTEND,' 555 ')
      CALL WEBGEXP(CREP,CREP2,RPRTEND,' 999 ')
      CALL WEBGEXP(CREP,CREP2,RPRTEND,' ICE ')

!-----------------------------------------------------------------------
! Move pointer past the header text and then past the SHIP callsign.
! Assign start & end of page for formatted text.
!-----------------------------------------------------------------------

      IPTR       = 45
      IPTR       = IPTR + INDEX(CREP(IPTR:),' ')
      COL_START  = 19
      COL_END    = 78

      IF (FIRST) THEN
        FIRST = .FALSE.
        WRITE(6,'('' ddhhmmZ ident     report'')')
      ENDIF

      WRITE(6,*)

!-----------------------------------------------------------------------
! Call routine to format text on page.
!-----------------------------------------------------------------------

      CALL WEBFORM(CREP,IPTR,RPRTEND,COL_START,COL_END,IN_TEXT)

!-----------------------------------------------------------------------
! Finally output MetDB header if WRITEHEADER = .TRUE.
!-----------------------------------------------------------------------

      IF (WRITEHEADER) THEN
        IN_TEXT(:) = ' '
        WRITE(6,'(1X,2A)')IN_TEXT(1:COL_START-1),CREP(1:30)//       !1.2
     &  ' CC='//CREP(31:34)//' '//TOR//' AMD='//CREP(41:41)//       !1.2
     &  ' COR='//CREP(43:43)                                        !1.2
      ENDIF

      RETURN
      END
