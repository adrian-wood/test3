      SUBROUTINE TESSC5(REPORT,EXPARR,IDFLG,ID)                    !2.2

!-----------------------------------------------------------------------
!
!  PROGRAM      : TESSC5
!
!  PURPOSE      : TO EXPAND SECTION 5 OF TESAC (IDENTIFIER)
!
!  DESCRIPTION  : Acceptable identifiers are a 5-figure group
!                 after 99999 or a group including a               !2.2
!                 letter after the last 5-figure group.
!
!  CALLED BY    : TESEXP
!
!  CALLS        : IVALUE   CONVERTS FROM CHAR TO NUMERIC TYPE
!
!  PARAMETERS   : REPORT   CHARACTER STRING OF REPORT (I)
!                 EXPARR   EXPANSION ARRAY            (O)
!                 IDFLG    FLAG FOR CALL SIGN         (O)
!                 ID       SHIPS CALL SIGN            (O)
!
! REVISION INFO :
!
! $Workfile: tessc5.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 16/08/2007 14:18:16$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         16/08/2007 14:18:16    Brian Barwell   Skip
!       groups of the form 'n////' when locating TESAC call sign.
!  1    Met_DB_Project 1.0         30/01/2006 20:25:21    Sheila Needham  
! $
! Revision 2.2  2001/11/06 10:16:59  usmdb
! 19 Nov 2001    C Long
! 2.2  Reduce argument list.
!      Allow for call sign starting with a figure but including letters
!
! Revision 2.1  2001/10/09  10:32:29  10:32:29  usmdb (Generic MetDB account)
! 15 Oct 2001     C Long
! 2.1  Check subscripts before substring operations to avoid
!      out-of-bounds errors.  Add more comments & tidy up.
!
! Revision 2.0  2001/07/03  10:44:18  10:44:18  usmdb (Generic MetDB account)
! Separated variable declaration and initialisation. Added copyright
! and modified header - S.Cox
!
! Revision 1.3  2000/10/18  10:08:52  10:08:52  usmdb (Generic MetDB acc
! 18 Oct 2000    C Long
! 1.3  Remove unused arguments from interface
!
! Revision 1.2  97/07/31  11:43:42  11:43:42  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 14:31:50  uspm
! Initial revision
!
! APR 95 - IMPROVE CODE TO FIND CHARACTER IDENTIFIER,
!          AS GROUP AFTER LAST 5-FIGURE GROUP
!
! INTRODUCED  : 22/08/94
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER*(*)  REPORT                                         !1.3
      CHARACTER*9    ID
      CHARACTER*80   HEAD                                            !2
      REAL           EXPARR(0:*)                                    !1.3
      REAL           MISING                                         !2.0
      REAL           DISP
      REAL           BUOYID
      INTEGER        REPLEN
      INTEGER        POS, IX,IY,IZ
      INTEGER        IVALUE
      INTEGER        LENID                                          !2.2
      LOGICAL        FIRST                                           !2
      LOGICAL        IDFLG

      DATA           MISING/-9999999./, FIRST/.TRUE./                !2

      IF (FIRST) THEN                                                !2
        HEAD = '$Workfile: tessc5.f$ ' //
     &         '$Revision: 2$ $Date: 16/08/2007 14:18:16$'
        FIRST = .FALSE.                                              !2
      END IF                                                         !2

! BUOYID - buoy identifier
! DISP - displacement of ship's call sign in string for encoding.
!        Because ID is the only character string in the report,
!        DISP will always be 1 if an ident is found.
! ID and BUOYID are mutually exclusive.

      ID=' '
      IDFLG=.FALSE.
      BUOYID=MISING
      DISP=MISING

! Starting at the end of the report,
! go back two 5-figure groups.  If the first is 99999, then a numeric
! buoy id follows.  If not, the ship's call sign is the last group -
! but it may be followed by spaces or rubbish!

      REPLEN=LEN(REPORT)                                           !2.2
      POS=REPLEN
      POS=POS-10

      IF (REPORT(POS-1:POS+3).EQ.'99999') POS=POS-1
      IF (REPORT(POS:POS+4).EQ.'99999') THEN
        POS=POS+6
        BUOYID=IVALUE(REPORT(POS:POS+4))
      ELSE

! Look for the first space in the last 20 characters of the report.
! Then look for two more spaces.  If the two groups so delimited are
! a 5-figure group and a non-5-figure group, assume the latter is the
! identifier; if both have 5 figures, then look one group further on.
! If only two spaces are found, assume there is none after the ident.
! (Use IVALUE to check for figures: it returns a number if the string
! contains only figures and BUFR missing data if not.)

        IX=INDEX(REPORT(REPLEN-20:REPLEN),' ')
        IF (IX.GT.0) THEN

! If space found, point past it & look for another

          IX=REPLEN-20+IX
   20     IF (IX.LE.REPLEN) THEN            ! if space not at end  !2.1
            IY=INDEX(REPORT(IX:REPLEN),' ')                        !2.1
          ELSE                                                     !2.1
            IY=0                                                   !2.1
          ENDIF                                                    !2.1

! And for a third space

   30     IF (IX+IY.LE.REPLEN) THEN                                !2.1
            IZ=INDEX(REPORT(IX+IY:REPLEN),' ')                     !2.1
          ELSE                                                     !2.1
            IZ=0                                                   !2.1
          ENDIF                                                    !2.1

! If the first two spaces delimit a 5-figure group,
! then if the second & third spaces do likewise, look on;
! if there's only one 5-figure group, assume the ident follows
! if the next group includes a letter.                             !2.2
! Allow for missing data (i.e. groups of the form 'n////').          !2

          IF (IY.EQ.6 .AND.
     &        (IVALUE(REPORT(IX:IX+4)).NE.-9999999 .OR.              !2
     &         (IVALUE(REPORT(IX:IX)).NE.-9999999 .AND.              !2
     &          REPORT(IX+1:IX+4).EQ.'////'))) THEN                  !2
            IF (IZ.EQ.6 .AND.
     &          (IVALUE(REPORT(IX+6:IX+10)).NE.-9999999 .OR.         !2
     &           (IVALUE(REPORT(IX+6:IX+6)).NE.-9999999 .AND.        !2
     &            REPORT(IX+7:IX+10).EQ.'////'))) THEN               !2
              IX=IX+IY
              IY=IZ
              GO TO 30
            ELSE IF (IX+IY.LE.REPLEN) THEN
              IF (IZ.GT.0) THEN
                ID=REPORT(IX+IY:IX+IY+IZ-1)
                LENID=IZ                                           !2.2
              ELSE
                ID=REPORT(IX+IY:REPLEN)
                LENID=REPLEN-(IX+IY)+1                             !2.2
              ENDIF

! ID has been set: if it's not all figures, it's a call sign.      !2.2

              IF (IVALUE(ID(1:LENID)).EQ.-9999999) THEN            !2.2
                IDFLG=.TRUE.
                DISP=1.0
              ENDIF
            ENDIF

! If the first group is not 5 figures, move on the pointer & try again.

          ELSE IF (IY.GT.0) THEN
            IX=IX+IY
            GO TO 20
          ENDIF
        ENDIF
      ENDIF

! Put values in expansion array.

      EXPARR(2)=DISP
      EXPARR(4)=BUOYID
      RETURN
      END
