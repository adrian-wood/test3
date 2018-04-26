      SUBROUTINE WEBFORM(CREP,START,RPRTEND,COL_START,COL_END,
     &                   IN_TEXT)

!-----------------------------------------------------------------------
!
! routine       : WEBFORM
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! purpose       : To output report text to stdout
!
! description   : This routine takes a report text as input and a
!               : column start and end for the stdout. WEBFORM will
!               : write the report text to stdout between the column
!               : start and end.
!
! arguments     :
!
! CREP          : char*(*)   (ip) : text string to output
! START         : integer    (ip) : pointer to start of text in CREP
! RPRTEND       : integer    (ip) : pointer to end of text in CREP
! COL_START     : integer    (ip) : stdout page column start
! COL_END       : integer    (ip) : stdout page column end
! IN_TEXT       : char*(*)   (ip) : text to output before col_start
!
! data types    : all
!
! calls         : none
!
! format of output text
! =====================
!
! IF (col_start.GT.1)
!
!               col_start                                     col_end
!                  !                                             !
!                  V                                             V
!   xxx(IN_TEXT)xxxooooooooooooooooooooooooooooooooooooooooooooooo
!                  ooooooooooooooooooo(CREP)oooooooooooooooooooooo
!                  ooooooooooooooooooooooooooooooooooooooooooooooo
!
! ELSEIF (col_start.EQ.1)
!
!   col_start                                              col_end
!   !                                                            !
!   V                                                            V
!   oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!   oooooooooooooooooooooooooooo(CREP)oooooooooooooooooooooooooooo
!   oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
! ENDIF
!
! change record :
!
! 29-07-1998    : Made operational - S.Cox
!
!-----------------------------------------------------------------------
! $Log:
!  1    Met_DB_Project 1.0         21/09/2006 11:21:23    Stan Kellett    
! $
! Revision 1.3  2001/01/18 09:48:13  usmdb
! Changed format statement so output is correct on HP - S.Cox
!
! Revision 1.2  99/03/11  15:11:38  15:11:38  usmdb (Generic MDB account)
! 15th March 1999 - S.Cox (ref MetDB prob 425)
! Correct formatting where the report text string wraps to
! the next line by 1 character only.
! 
! Revision 1.1  98/07/29  08:06:18  08:06:18  usmdb (Generic MDB account)
! Initial revision
!
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER       RPRTEND      !- end of report in string CREP
      INTEGER       COL_START    !- stdout page start column
      INTEGER       COL_END      !- stdout page end column
      INTEGER       COL_SPAN     !- stdout page column span
      INTEGER       CREP_END
      INTEGER       LINE         !- counter of output lines.
      INTEGER       START        !- start of report in string CREP
      INTEGER       IPTR         !- pointer to position in CREP

      CHARACTER*(*) CREP         !- report to output to stdout
      CHARACTER*132 HEAD         !- RCS info
      CHARACTER*(*) IN_TEXT      !- input text to preceed report text
      CHARACTER*20  LINE_TEXT    !- text to preceed report text

      HEAD = '
     &$Source: /net/home/h01/mdb_new/op/lib/web_source/RCS/webform.F,v $
     &'//'$Date: 21/09/2006 11:21:23$ $Revision: 1$'

!-----------------------------------------------------------------------
! initilise variables
!-----------------------------------------------------------------------

      IPTR=START
      LINE=1
      LINE_TEXT(:)=' '
      COL_SPAN=COL_END-COL_START

!-----------------------------------------------------------------------
! loop while pointer to position in report text is not greater than the
! end of the report text.
!-----------------------------------------------------------------------

      DO WHILE (IPTR.LE.RPRTEND)                                    !1.2

!-----------------------------------------------------------------------
! if we are to output the 1st line, put IN_TEXT into the string
! LINE_TEXT. This is to preceed the main report text for the 1st output
! line only, otherwise LINE_TEXT = ' '
!-----------------------------------------------------------------------

        IF (LINE.EQ.1) THEN
          LINE_TEXT(1:LEN(IN_TEXT)) = IN_TEXT
        ELSE
          LINE_TEXT(:) = ' '
        ENDIF

!-----------------------------------------------------------------------
! calculate report text end for this line - it could be the end of the
! report text.
!-----------------------------------------------------------------------

        IF ((IPTR+COL_SPAN).LE.RPRTEND) THEN
          CREP_END = IPTR + COL_SPAN
        ELSE
          CREP_END = RPRTEND
        ENDIF

!-----------------------------------------------------------------------
! if COL_START>1, output LINE_TEXT followed by CREP, otherwise output
! just CREP. Increment IPTR and LINE.
!-----------------------------------------------------------------------

        IF (COL_START.GT.1) THEN
          WRITE(6,'(1X,A)')LINE_TEXT(1:COL_START-1)//               !1.3
     &                     CREP(IPTR:CREP_END)                      !1.3
        ELSE
          WRITE(6,'(1X,A)')CREP(IPTR:CREP_END)                      !1.3
        ENDIF

        IPTR=IPTR+COL_SPAN+1
        LINE=LINE+1
      ENDDO

      RETURN
      END
