      SUBROUTINE WEBSREW(CREP,RPRTEND,WRITEHEADER,TOR,IN_TEXT)

!-----------------------------------------------------------------------
!
! routine       : WEBSREW
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! purpose       : To format SREW raw reports for web retrieval.
!
! description   : The SREW raw report (including header) is passed
!               : to this routine. This routines formats it for web
!               : display.
!
! arguments     :
!
! CREP          : char*(*)   (ip) : input report text from MDB
! RPRTEND       : integer    (ip) : end of CREP.
! WRITEHEADER   : logical    (ip) : TRUE if report text header wanted
! TOR           : char*(*)   (ip) : report Time Of Receipt
! IN_TEXT       : char*(*)   (ip) : dummy - string initialised here.
!
! data types    : SREW
!
! calls         : WEBFORM  : to output the final report text
!
! change record :
!
! 28-07-1998    : Made operational - S.Cox
!
!-----------------------------------------------------------------------
! $Log:
!  1    Met_DB_Project 1.0         21/09/2006 11:28:30    Stan Kellett    
! $
! Revision 1.3  2001/01/18 10:04:24  usmdb
! Changed format statement so output is correct on HP - S.Cox
!
! Revision 1.2  99/03/11  15:12:28  15:12:28  usmdb (Generic MDB account)
! 15th March 1999 - S.Cox
! Correct formatting of SREW reports
! 
! Revision 1.1  98/07/29  08:06:44  08:06:44  usmdb (Generic MDB account)
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
      CHARACTER*132   HEAD         !- RCS info
      CHARACTER*(*)   TOR          !- input time of receipt
      CHARACTER*(*)   IN_TEXT      !- text to preceed report on page

      DATA FIRST      /.TRUE./     !- 1st time in routine

!-----------------------------------------------------------------------
! RCS revision information.
!-----------------------------------------------------------------------

      HEAD = '
     &$Source: /net/home/h01/mdb_new/op/lib/web_source/RCS/websrew.F,v $
     &'//'$Date: 21/09/2006 11:28:30$ $Revision: 1$'

!-----------------------------------------------------------------------
! Move pointer past the header text and then past the station number.
! Assign start & end of page for formatted text.
!-----------------------------------------------------------------------

      IPTR       = RPRTEND - 2                                      !1.2
      COL_START  = 15
      COL_END    = 80

      IF (FIRST) THEN
        FIRST = .FALSE.
        WRITE(6,'('' ddhhmmZ ident report'')')
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
        WRITE(6,'(1X,2A)')IN_TEXT(1:COL_START-1),CREP(1:30)//       !1.3
     &  ' CC='//CREP(31:34)//' '//TOR//' AMD='//CREP(41:41)//       !1.3
     &  ' COR='//CREP(43:43)                                        !1.3
      ENDIF

      RETURN
      END
