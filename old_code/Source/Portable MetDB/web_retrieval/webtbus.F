      SUBROUTINE WEBTBUS(CREP,RPRTEND,WRITEHEADER,CREP2)            !1.3

!-----------------------------------------------------------------------
!
! routine       : WEBTBUS
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! purpose       : To format TBUS raw reports for web retrieval.
!
! description   : The TBUS raw report (including header) is passed
!               : to this routine. This routines formats it for web
!               : display.
!
! arguments     :
!
! CREP          : char*(*)   (ip) : input report text from MDB
! RPRTEND       : integer    (ip) : end of CREP.
! WRITEHEADER   : logical    (ip) : TRUE if report text header wanted
! TOR           : char*(*)   (ip) : report Time Of Receipt
! CREP2         : char*(*)   (ip) : temporary string                !1.3
!
! data types    : TBUS
!
! calls         : WEBFORM  : to output the final report text
!
! change record :
!
! 28-07-1998    : Made operational - S.Cox
!
!-----------------------------------------------------------------------
! $Log:
!  1    Met_DB_Project 1.0         21/09/2006 11:24:34    Stan Kellett    
! $
! Revision 1.4  2001/01/18 10:05:59  usmdb
! Changed format statement so output is correct on HP.
! Improveded LF and CR initialisations and added
! pre-processor statements as LF/CR are different on EBCDIC
! and ASCII machines - S.Cox
!
! Revision 1.3  2000/03/10  09:24:45  09:24:45  usmdb (Generic MDB account)
! Operational 20-03-2000
! CREP2 passed in as argument. Use this to hold a line
! of CREP data. Ref MetDB problem 534.
! 
! Revision 1.2  99/09/09  10:25:43  10:25:43  usmdb (Generic MDB account)
! Change date: 20-09-1999 - S.Cox
! Format text better using reported CR's and LF's
!
! Revision 1.1  98/07/29  08:06:54  08:06:54  usmdb (Generic MDB account)
! Initial revision
!
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER         COL_END      !- end column of report on page
      INTEGER         COL_START    !- start column of report on page
      INTEGER         I            !- loop counter
      INTEGER         PTR          !- report pointer                !1.2
      INTEGER         RPRTEND      !- length of input report text

      LOGICAL         WRITEHEADER  !- TRUE to output header

      CHARACTER*(*)   CREP         !- input report text
      CHARACTER*1     CR,LF        !- carriage return & linefeed
      CHARACTER*132   HEAD         !- RCS info
      CHARACTER*(*)   CREP2        !- string to hold line of text   !1.3

!-----------------------------------------------------------------------
! Initialise variables.
!-----------------------------------------------------------------------

      HEAD = '
     &$Source: /net/home/h01/mdb_new/op/lib/web_source/RCS/webtbus.F,v $
     &'//'$Date: 21/09/2006 11:24:34$ $Revision: 1$'

#if defined (MVS)
        LF = CHAR(37)                                               !1.4
        CR = CHAR(21)                                               !1.4
#else
        LF = CHAR(10)                                               !1.4
        CR = CHAR(13)                                               !1.4
#endif

!-----------------------------------------------------------------------
! Output report text header if wanted and report text.
!-----------------------------------------------------------------------

      WRITE(6,*)
      IF (WRITEHEADER) WRITE(6,'(1X,A)')CREP(1:44)                  !1.4
      WRITE(6,*)

!-----------------------------------------------------------------------
! Format report text. Ignore CR's, but line feed when LF found.
!-----------------------------------------------------------------------

      PTR = 0                                                       !1.2
      DO I=45,RPRTEND                                               !1.2
        IF (CREP(I:I).EQ.CR) THEN                                   !1.2
          CONTINUE                                                  !1.2
        ELSEIF (CREP(I:I).EQ.LF) THEN                               !1.2
          IF (PTR.GT.0) WRITE(6,'(1X,A)')CREP2(1:PTR)               !1.4
          CREP2(:) = ' '                                            !1.2
          PTR = 0                                                   !1.2
        ELSE                                                        !1.2
          PTR = PTR + 1                                             !1.2
          CREP2(PTR:PTR) = CREP(I:I)                                !1.2
        ENDIF                                                       !1.2
      ENDDO                                                         !1.2

      RETURN
      END
