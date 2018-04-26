         SUBROUTINE GETSTN(REQ,IPOS,ILEN,ISTN,INUM,IDLEN,ISTYP,

     &                     IFAIL,CERR)

!-----------------------------------------------------------------------
!
! PROGRAM       : GETSTN IN MDB
!
! PURPOSE       : TO GET INTEGER STATION IDENTIFIERS FROM USERS REQUEST
!
! DESCRIPTION   : COPES WITH IDENTIFIERS LENGTH 2,4,5 AND 6
!                 I.E. NN     WMO BLOCK OR DCNN
!                      NNNNN  WMO BLK/STN
!                      NNNN   DCNN
!                      NNNNNN RAINFALL STN
!      29/11/93   AND ALSO WITH SPECIAL WORDS SURFACE AND UPAIR
!
! CALLED BY     : GETREQ IN MDB
!
! CALLS         : NXTKEY
!
! PARAMETERS    : (1)REQ    USERS REQUEST
!               : (2)IPOS   POINTING TO FIRST GROUP ON ENTRY, TO NEXT
!                           KEYWORD (OR ILEN) ON EXIT
!                 (3)ILEN   LENGTH OF REQUEST
!                 (4)ISTN(*)    LIST OF STATIONS
!                 (5)INUM MAX NO IN LIST ON ENTRY, ACTUAL NO ON EXIT
!                 (6)IDLEN  LENGTH OF GROUPS (-1 FOR MIXED LENGTHS)
!                 (7)ISTYP  1 SUFRACE, 2 UPAIR (FOR STNMAS)
!                 (8)IFAIL    8 IF ERROR DETECTED
!                 (9)CERR     ERROR MESSAGE
!
!Y2K  26.06.1997  GETSTN IS YEAR 2000 COMPLIANT.
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 02/04/2009 11:19:40$
! $Source: /home/us0400/mdb/op/lib/source/RCS/getstn.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         02/04/2009 11:19:40    Richard Weedon
!       Select parameter increased to 50 CHG 7636
!  1    Met_DB_Project 1.0         30/01/2006 20:22:36    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:42  usmdb
! Added copyright and modified header & comments - S.Cox
!
! Revision 1.3  97/08/04  13:11:13  13:11:13  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
!
! Revision 1.2  1997/02/12 12:31:42  uspm
! Update dir name in variable head
!
! Revision 1.1  1997/02/11 14:44:13  uspm
! Initial revision
!
! 29/11/93     WORDS 'SURFACE' OR 'UPAIR' CAN APPEAR AT THE END OF
!              THE STATION LIST
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
      CHARACTER*(*) REQ,CERR
      INTEGER IPOS,ILEN,ISTN(*),INUM,IDLEN,IFAIL
      INTEGER I, ISTYP

      LOGICAL NXTKEY,INTCON,OCHK
      INTEGER COUNT,GRPLEN
      CHARACTER*4 CFMT

      CHARACTER*132 HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/getstn.F,v $
     &'//' $Revision: 2$ $Date: 02/04/2009 11:19:40$ '


      CFMT='(I1)'
      COUNT=0
      IDLEN=0
! CHECK WE'RE NOT LOOKING AT A KEYWORD
10    IF(.NOT.NXTKEY(REQ,IPOS,ILEN))THEN
! FIND THE LENGTH OF THIS GROUP
        I=INDEX(REQ(IPOS:ILEN),' ')
        IF(I.EQ.0)THEN
          GRPLEN=ILEN-IPOS+1
        ELSE
          GRPLEN=I-1
        ENDIF
! CHECK FOR SPECIAL WORDS
        IF(REQ(IPOS:IPOS+6).EQ.'SURFACE')THEN
          ISTYP=1
          IPOS=IPOS+GRPLEN+1
          GOTO 997
        ELSEIF(REQ(IPOS:IPOS+4).EQ.'UPAIR')THEN
          ISTYP=2
          IPOS=IPOS+GRPLEN+1
          GOTO 997
        ENDIF
! SET LENGTH OF ALL GROUPS FOR RETURN
        IF(IDLEN.NE.-1)THEN
          IF(IDLEN.EQ.0)THEN  ! FIRST TIME THROUGH
            IDLEN=GRPLEN
          ELSEIF(IDLEN.NE.GRPLEN)THEN
            IDLEN=-1
          ENDIF
        ENDIF
! CHECK FOR VALID INTEGERS THROUGHOUT THE GROUP
        OCHK=INTCON(REQ,IPOS,IPOS+GRPLEN-1)
        IF(.NOT.OCHK)THEN
          CERR=':NON-DIGITS'
          IFAIL=8
          GOTO 999
        ENDIF
        COUNT=COUNT+1
        IF(COUNT.LE.INUM)THEN
          CFMT='(I1)'
          WRITE(CFMT(3:3),'(I1)')GRPLEN
          READ(REQ(IPOS:IPOS+GRPLEN-1),CFMT)ISTN(COUNT)
          IPOS=IPOS+GRPLEN+1
        ELSE
          CERR=':LIST TOO LONG'
          IFAIL=8
          GOTO 999
        ENDIF
      ELSE ! END OF LIST ENCOUNTERED
        GOTO 998
      ENDIF
997   IF(IPOS.LT.ILEN)GOTO 10  ! GO BACK FOR NEXT GROUP
998   INUM=COUNT
999   RETURN
      END
