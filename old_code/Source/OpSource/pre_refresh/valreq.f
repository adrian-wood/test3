      SUBROUTINE VALREQ(ITYPE,FOUND,ITIME,IRECV,ICT,ITOD,IFAIL,     !2.0
     &                  CERR)

!-----------------------------------------------------------------------
!
! ROUTINE       : VALREQ
!
! PURPOSE       : to validate the user's request string
!
! CALLED BY     : MDB
!
! CALLS         : VALDAT
!
! PARAMETERS    : ITYPE      :  data subtype (as in ddict)
!               : FOUND(*)   :  logical array for keywords found
!               : ITIME(9)   :  obs times
!               : IRECV(10)  :  cutoff times
!               : ICT(8)     :  current date/time
!               : ITOD       :  1 for request in today format
!               : IFAIL      :  8 if error detected, 4 for warning
!               : CERR       :  error message
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:59$
! $Source: /data/us0400/mdb/op/lib/source/RCS/valreq.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:59    Sheila Needham  
! $
! Revision 2.1  2002/05/07  09:07:23  09:07:23  usmdb (Generic MetDB account)
! 2.1.  20 May 2002.  Brian Barwell.  Change 36/02.
! Truncation of minutes from elements 4 and 8 of IETEME deleted.
! 
! Revision 2.0  2001/01/08  11:59:28  11:59:28  usmdb (Generic MDB account)
! Removed unused dummy argument IFORM. Added copyright and
! modified header - S.Cox
!
! Revision 1.5  98/05/15  10:09:46  10:09:46  usmdb (Generic MDB account)
! Removed CLIMUK specific code
!
! 18-05-98  !C  : Removed CLIMUK specific code - S.Cox
!
! 01-09-97  !B  : Removed section of code that set retrieval from
!               : current or merged data depending on the format of
!               : the START and END times. The default is now current
!               : data retrieval unless the keyword DATA ARCHIVE is
!               : used. - S.Cox
!
! 21-04-97  !A  : Removed all arguments passed and variables in VALREQ
!               : that were not being validated. - S.Cox
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

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

      INTEGER        ICT(8)
      INTEGER        IFAIL
      INTEGER        IRECV(10)
      INTEGER        ITIME(9)
      INTEGER        ITYPE
      INTEGER        ITOD
      INTEGER        J                !- general loop counter

      LOGICAL        FOUND(*)

      CHARACTER*(*)  CERR
      CHARACTER*132  HEAD

      HEAD='$RCSfile: $ ' //
     &     '$Revision: 1$ $Date: 30/01/2006 20:25:59$'

!-----------------------------------------------------------------------
! STNMAS - no start or end time should be specified. If so, issue a
! warning and reset the start and en times to zero.
!-----------------------------------------------------------------------

      IF (ITYPE.EQ.1) THEN

        IF (FOUND(1).OR.FOUND(2)) THEN
          CERR='START/END INVALID FOR THIS TYPE'
          IFAIL=4
          DO J=1,8
            ITIME(J)=0
          ENDDO
        ENDIF

!-----------------------------------------------------------------------
! All other data subtypes
!-----------------------------------------------------------------------

      ELSE

!-----------------------------------------------------------------------
! check start and end times
!-----------------------------------------------------------------------

        CALL VALDAT(ITIME,ITOD,FOUND,ICT,IFAIL,CERR)
        IF (IFAIL.EQ.8) GOTO 999

!-----------------------------------------------------------------------
! check received times
!-----------------------------------------------------------------------

        IF (FOUND(5)) THEN
          CALL VALREC(IRECV,IFAIL,CERR)
          IF (IFAIL.EQ.8) GOTO 999
        ENDIF
      ENDIF

 999  CONTINUE

      RETURN
      END
