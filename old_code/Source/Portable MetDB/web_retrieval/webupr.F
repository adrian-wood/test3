      SUBROUTINE WEBUPR(CREP,RPRTEND,WRITEHEADER,IN_TEXT,           !1.4
     &                  TOR_UPR)                                    !1.4

!-----------------------------------------------------------------------
!
! routine       : WEBUPR
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! purpose       : To format TEMP, PILOT raw reports for web retrieval.
!
! description   : The raw report (including header) is passed
!               : to this routine. This routines formats it for web
!               : display.
!
! arguments     :
!
! CREP          : char*(*)   (ip) : input report text from MDB
! RPRTEND       : integer    (ip) : end of CREP.
! WRITEHEADER   : logical    (ip) : TRUE if report text header wanted
! IN_TEXT       : char*(*)   (ip) : dummy - string initialised here.
! TOR_UPR(4)    : char*(*)   (ip) : Time Of Receipt for parts in    !1.4
!                                 : the order A,C,B,D               !1.4
!
! data types    : TEMP, PILOT, DROPSOND
!
! calls         : WEBFORM  : to output the final report text
!
! change record :
!
! 28-07-1998    : Made operational - S.Cox
!
! 31-07-1998    : Correct end of report text so last character isn't
!               : ommitted - S.Cox
!
! 10-08-1998    : Change COL_START & COL_END slightly - S.Cox
!
!-----------------------------------------------------------------------
! $Log:
!  1    Met_DB_Project 1.0         21/09/2006 11:31:58    Stan Kellett    
! $
! Revision 1.5  2001/01/18 10:08:52  usmdb
! Corrected "array out of bounds error" and changed format
! statement so output is correct on HP - S.Cox
!
! Revision 1.4  2000/10/04  15:46:47  15:46:47  usmdb (Generic MDB account)
! TOR for each part now passed in. Output details in
! MetDB header - S.Cox
! 
! Revision 1.3  98/08/10  08:54:38  08:54:38  usmdb (Generic MDB account)
! no entry
!
! Revision 1.2  1998/07/31 10:52:05  usmdb
! no entry
!
! Revision 1.1  1998/07/29 08:07:03  usmdb
! Initial revision
!
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER         COL_END      !- end column of report on page
      INTEGER         COL_START    !- start column of report on page
      INTEGER         J            !- loop counter over 4 parts
      INTEGER         IZ1          !- pos of head time in current part
      INTEGER         IZ2          !- pos of head time in next part
      INTEGER         IPOS         !- position pointer on report text
      INTEGER         ISTART       !- start of part report text
      INTEGER         IEND         !- end of part report text
      INTEGER         PART_COUNT   !- part counter                  !1.5
      INTEGER         RPRTEND      !- length of input report text
      LOGICAL         FIRST        !- TRUE for 1st time in routine
      LOGICAL         WRITEHEADER  !- TRUE to output header

      CHARACTER*(*)   CREP         !- input report text
      CHARACTER*132   HEAD         !- RCS info
      CHARACTER*44    HEADER       !- report text header for each part
      CHARACTER*(*)   IN_TEXT      !- text to preceed report on page
      CHARACTER*(*)   TOR_UPR(4)   !- Time Of Receipt per part      !1.4

      DATA FIRST      /.TRUE./     !- 1st time in routine

!-----------------------------------------------------------------------
! RCS revision information.
!-----------------------------------------------------------------------

      HEAD = '
     &$Source: /net/home/h01/mdb_new/op/lib/web_source/RCS/webupr.F,v $
     &'//'$Date: 21/09/2006 11:31:58$ $Revision: 1$'

!-----------------------------------------------------------------------
! Assign start & end of page for formatted text.
!-----------------------------------------------------------------------

      COL_START  = 18
      COL_END    = 77
      IPOS       = 1
      PART_COUNT = 0                                                !1.5

      IF (FIRST) THEN
        FIRST = .FALSE.
        WRITE(6,'('' ddhhmmZ ident   report''/)')
      ENDIF

!-----------------------------------------------------------------------
! Loop over 4 parts, Finding header text and start & end of message
! for each.
!-----------------------------------------------------------------------
      
      DO J=1,4
        IF (TOR_UPR(J).NE.'TOR=**/****') THEN

          IZ1 = INDEX(CREP(IPOS:RPRTEND),'Z ')
          IF (IZ1.GT.0) THEN
            IF (PART_COUNT.NE.0) WRITE(6,*) !- write between parts  !1.5
            PART_COUNT = PART_COUNT + 1                             !1.5
            IPOS = IPOS+IZ1
            HEADER = CREP(IPOS-5:IPOS-5+43)
            ISTART = IPOS-5+44
            IZ2 = INDEX(CREP(IPOS:),'Z ')
            IF (IZ2.GT.0) THEN
              IEND = IPOS-5+IZ2
            ELSE
              IEND = RPRTEND+1
            ENDIF

            ISTART = ISTART - 1   !- to move to blank before the TTxx
            IEND   = IEND   - 1   !- compensate for above

!-----------------------------------------------------------------------
! Call routine to format text on page.
!-----------------------------------------------------------------------

            CALL WEBFORM(CREP,ISTART,IEND,COL_START,COL_END,IN_TEXT)
            IN_TEXT(:) = ' '

!-----------------------------------------------------------------------
! Output MetDB header if WRITEHEADER = .TRUE.
!-----------------------------------------------------------------------

            IF (WRITEHEADER) THEN
              WRITE(6,'(1X,2A)')IN_TEXT(1:COL_START-1),             !1.5
     &        HEADER(1:30)//' CC='//HEADER(31:34)//' '//            !1.5
     &        TOR_UPR(J)//' AMD='//HEADER(41:41)//' COR='//         !1.5
     &        HEADER(43:43)                                         !1.5
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      WRITE(6,*)

      RETURN
      END
