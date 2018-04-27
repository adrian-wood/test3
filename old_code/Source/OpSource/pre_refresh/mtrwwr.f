      SUBROUTINE MTRWWR(POINT,REPORT,GRPLEN,ELEM,ESTART,WDWD,REWW,ERROR)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : MTRWWR
!
! PURPOSE       : To decode a recent or present weather group (METARs)
!                 or forecast weather (TAFs) into flags as described
!                 in BUFR flag tables 020192, 020193 & 020194.
!
! CALLED BY     : MTREXP, TAFEXP
!
! PARAMETERS    : (1)  POINT   start of group        (input/output)
!                 (2)  REPORT  report                (input)
!                 (3)  GRPLEN  group length          (input,changed)
!                 (4)  ELEM    element count array   (output)
!                 (5)  ESTART  position in ELEM      (input)
!                 (6)  WDWD    W'W' array            (output)
!                 (7)  REWW    REWW array            (output)
!                 (8)  ERROR   true if bad letters   (output)
!
! Note: ELEM(ESTART) is for W'W', ELEM(ESTART+1) is for REWW (METARs)
!      (See MTREXP and TAFEXP for layout of ELEM - which is NOT a flag
!       array, as is claimed in various places, but used in subscript
!       calculations!)
!
! REVISION INFO :
!
! $Revision: 4$
! $Date: 25/02/2011 15:17:31$
! $Source: /data/us0400/mdb/op/lib/source/RCS/mtrwwr.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  4    Met_DB_Project 1.2.1.0     25/02/2011 15:17:31    Brian Barwell   IF
!       statement moved into ELSE block at end.
!  3    Met_DB_Project 1.2         11/07/2008 16:31:49    Richard Weedon  The
!       table array spec amended
!  2    Met_DB_Project 1.1         30/06/2008 13:52:29    Richard Weedon
!       Changes made under CR4858 
!  1    Met_DB_Project 1.0         30/01/2006 20:23:39    Sheila Needham  
! $
! Revision 2.1  2002/10/07  15:08:42  15:08:42  usmdb (Generic MetDB account)
! 15 Oct 2001    C Long
! 2.1  Rewrite more compactly with fewer confusing flags.
!
! Revision 2.0  2001/01/08  11:59:00  11:59:00  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
! change 2.0a corrected handling of bad weather groups
! or cors within text part of message - S.Kellett
!
! Revision 1.2  99/02/11  14:31:14  14:31:14  usmdb (Generic MDB account
! 15th Feb 1999 - J.Lewthwaite
! WMO CBS changes to allow weather RAPE to be coded as RAPL
! v(G)=31, ev(G)=1
!
! Revision 1.1  97/08/07  09:09:01  09:09:01  uspm (Pat McCormack)
! Initial revision
!
! OCTOBER 1995    ADDITION OF NEW DESCRIPTOR 'PR' TO TABLE 20193.
!                 CHANGES TO PREVENT INCORRECT DECODE VALUE OF
!                 GROUPS CONTAINING REPEATED DESCRIPTORS OR
!                 PHENOMENON.
!                 REMOVAL OF RECENT WEATHER INTENSITY CHECK !
!                 REMOVAL OF DUPLICATED CODE AND ADDITION OF NEW
!                 CODE TO FULLY ACCOMODATE POSSIBLE WEATHER GROUPS.
!                 UPDATED TO CURRENT STANDARDS.
!
! 07/11/94        CORRECTION TO SET VICINITY CODE FIGURE TO CORRECT
!                 VALUE                                           !B
!
! FEBRUARY 1994   CORRECT THE SETTING OF THE CODE FIGURE FOR BUFR
!                 CODE TABLE 020192 INTENSITY OR PROXIMITY QUALIFIER
!                 AND REMOVE REDUNDANT CODE CHECKING FOR INTENSITY
!                 IN VICINITY INCLUDED IN ERROR IN INITIAL TESTS  !A
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

      INTEGER DESC             ! Weather descriptor.
      INTEGER ELEM(*)          ! Counts for current & recent weather
      INTEGER ESTART           ! Displacement within element array.
      INTEGER GRPLEN           ! Length of group (2-9 characters valid)
      INTEGER ID               ! Number of descriptor in TABLE.
      INTEGER L                ! Loop variable
      INTEGER PHEN             ! Weather phenomenon.
      INTEGER POINT            ! Start of group being checked.
      INTEGER QUAL             ! Intensity or qualifier (table 020192)

      REAL REWW(*)             ! Expanded recent weather.
      REAL WDWD(*)             ! Expanded weather group.

      CHARACTER HEAD*132       ! Revision information
      CHARACTER REPORT*(*)     ! Report being expanded.
      CHARACTER TABLE*58       ! Qualifier/Phenomenon letters
      CHARACTER PAIR*2         ! Pair of letters to look up in TABLE

      LOGICAL ERROR            ! Group cannot be successfully decoded.
      LOGICAL LREWW            ! Flag set if recent weather group (RE)
      LOGICAL FOUND            ! False if reported letters not in table

! Initialise variables.
      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/mtrwwr.f,v $
     &'//'$ $Date: 25/02/2011 15:17:31$ $Revision: 4$'

      ERROR=.FALSE.
      LREWW=.FALSE.
      DESC=0
      PHEN=0
      QUAL=0                   ! Default intensity is zero

! TABLE lists possible descriptors & phenomena in recent or current
! weather groups (see the columns in code table 4678, though the
! order is as in our local flag tables 020193 & 020194).

      TABLE( 1:16)='MIBCDRBLSHTSFZPR'   ! qualifiers
      TABLE(17:32)='DZRASNSGICPEGRGS'   ! phenomena: precipitation
      TABLE(33:46)='BRFGFUVADUSAHZ'     ! phenomena: obscuration
      TABLE(47:58)='POSQFCSSDSUP'         ! other phenomena

! First see if the group is recent weather (starting RE).

      IF (ELEM(ESTART).LT.3) THEN
        IF (GRPLEN.GE.4) THEN
          IF (REPORT(POINT:POINT+1).EQ.'RE') THEN
            POINT=POINT+2
            GRPLEN=GRPLEN-2
            LREWW=.TRUE.

! Otherwise it's current.  Does the group start VC (in the vicinity) ?

          ELSE IF (REPORT(POINT:POINT+1).EQ.'VC') THEN
            POINT=POINT+2
            GRPLEN=GRPLEN-2
            QUAL=5

! Old "heavy" intensity qualifier?

          ELSE IF (REPORT(POINT:POINT+1).EQ.'XX') THEN
            POINT=POINT+2
            GRPLEN=GRPLEN-2
            QUAL=3
          ENDIF
        ENDIF

! If none of the above, is the group length odd or even?
! Odd groups are only reported in the new code.
! Even groups can be either old or new code.
! If odd, it must start '+' or '-': set intensity to heavy or light.

        IF (GRPLEN.LT.4 .OR. (QUAL.EQ.0 .AND. .NOT.LREWW)) THEN
          IF (MOD(GRPLEN,2).NE.0) THEN
            IF (REPORT(POINT:POINT).EQ.'+') THEN
              QUAL=3
            ELSE IF (REPORT(POINT:POINT).EQ.'-') THEN
              QUAL=1
            ELSE
              ERROR=.TRUE.
              RETURN
            ENDIF
            POINT=POINT+1
            GRPLEN=GRPLEN-1

! If length is even, and phenomenon not one of those which can't have
! an intensity, set intensity to moderate rather than default zero.

          ELSE
            IF (REPORT(POINT:POINT+1) .NE. 'MI' .AND.
     &          REPORT(POINT:POINT+1) .NE. 'BC' .AND.
     &          REPORT(POINT:POINT+1) .NE. 'FG' .AND.
     &          REPORT(POINT:POINT+1) .NE. 'PR' .AND.
     &          REPORT(POINT:POINT+1) .NE. 'BR' .AND.
     &          REPORT(POINT:POINT+1) .NE. 'FU' .AND.
     &          REPORT(POINT:POINT+1) .NE. 'HZ') THEN
              QUAL=2
            ENDIF
          ENDIF
        ENDIF

! If there are 3 groups already, give up - there's no room in the array

      ELSE
        ERROR=.TRUE.
        RETURN
      ENDIF

! The group length at this point must be even.  If it's more than 8
! there's something wrong.

      IF (GRPLEN.GT.8) THEN
        ERROR=.TRUE.
        RETURN
      ENDIF

! Look for pairs of letters in the list of qualifiers & phenomena from
! code tables 020193 & 020194, skipping a pair if consecutive pairs
! are the same.
! The table includes PE for pellets, replaced by PL in 1998 to avoid
! (for political correctness) the combination RAPE; PE must still be
! handled for old data (only expanded when retrieved!).

      FOUND=.TRUE.
      L=1
      DO WHILE (FOUND .AND. L.LE.GRPLEN/2)
        PAIR=REPORT(POINT:POINT+1)
        IF (L.EQ.1 .OR. PAIR.NE.REPORT(POINT-2:POINT-1)) THEN
          IF (PAIR.EQ.'PL') PAIR='PE'
          FOUND=.FALSE.
          ID=0
          DO WHILE (.NOT.FOUND .AND. ID.LT.29)
            ID=ID+1
            IF (PAIR.EQ.TABLE(ID*2-1:ID*2)) THEN
              FOUND=.TRUE.
              IF (ID.LE.8) THEN
                DESC=2**(ID-1)+DESC
              ELSE
                PHEN=2**(ID-1-8)+PHEN
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        POINT=POINT+2
        L=L+1
      ENDDO

! If any reported characters not found in table, return.

      IF (.NOT.FOUND) THEN
        ERROR=.TRUE.
        RETURN
      ENDIF

! Increment output array subscript & return values

      IF (LREWW) THEN
        ELEM(ESTART+1)=ELEM(ESTART+1)+1
        IF (DESC.NE.0) REWW(1+(ELEM(ESTART+1)-1)*2)=DESC
        IF (PHEN.NE.0) REWW(2+(ELEM(ESTART+1)-1)*2)=PHEN
      ELSE
        ELEM(ESTART)=ELEM(ESTART)+1
        IF (DESC.GT.0) WDWD(2+(ELEM(ESTART)-1)*3)=DESC
        IF (PHEN.GT.0) WDWD(ELEM(ESTART)*3)=PHEN
        IF (QUAL.GT.0) WDWD(1+(ELEM(ESTART)-1)*3)=QUAL              !4
      ENDIF

      RETURN
      END
