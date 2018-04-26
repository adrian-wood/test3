      SUBROUTINE SYNSEQ(IDENT,IDATIM,PRESS,TEND,FLAG) ! & IDSK?

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNSEQ
!
! PURPOSE       : PRESSURE TENDENCY CHECK FOR SYNOPS
!
! DESCRIPTION   : SYNSEQ DOES A LOW-LEVEL RETRIEVAL TO GET PRESSURE
!                 3 HOURS AGO, USING THE INDEX(ES) BEING UPDATED BY
!                 SYNREP TO FIND THE REPORT AND BIT COUNT RETRIEVAL
!                 TO FIND THE PRESSURE
!
! DATA TYPE(S)  : SYNOPs
!
! CALLED BY     : SYNEXP
!
! CALLS         : MDBIO (map & message calls), DATE31, EB2ASC,
!                 BITINDX,VALUSR
!                 COMMON /SYNIX/ IS SHARED WITH SYNSTO
!
! PARAMETERS    : (1) STATION FOR WHICH REPORT IS REQUIRED        (I)
!                 (2) DATE/TIME OF NEW REPORT (year/mon/day/hour) (I)
!                 (3) PRESSURE IN NEW REPORT                      (I)
!                 (4) PRESSURE TENDENCY IN NEW REPORT             (I)
!                 (5) RESULT OF TEST (0: CONSISTENT,              (O)
!                      (1: INCONSISTENT, MISSING: NO TEST)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:00$
! $Source: /home/us0400/mdb/op/lib/source/RCS/synseq.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:00    Sheila Needham  
! $
! Revision 2.1  2001/07/03 10:39:59  usmdb
! Removed argument LFLAG from MDBIO calls as not used in MDBIO - S.Cox
!
! Revision 2.0  2001/05/31  13:27:54  13:27:54  usmdb (Generic MetDB account)
! Removed argument XTYPE from call to BITINDX as no longer used in
! BITINDX. Added copyright and modified header - S.Cox
! 
! Revision 1.7  98/11/12  08:49:32  08:49:32  usmdb (Generic MetDB account)
! 16-11-98 S.Cox
! Change declaration of CINDEX - now an array of element indexes.
! 
! Revision 1.6  98/05/15  10:26:54  10:26:54  usmdb (Generic MDB account)
! Change tolerence for pressure tendency check from 0.1mb to
! 1.0mb. - J Norton                                                   !G
!
! Further comments from Chris Hall about above change:
!
! The comment from the MetDB Team about pressure tendency reflects a
! lack of understanding about what is measured. The magnitude of the
! change reported in the pressure tendency group of a SYNOP is NOT the
! change in mean sea level pressure. It is the change in station level
! pressure. Station level pressure may remain constant while mslp
! changes due to a change of screen level temperature. This effect is
! most marked at high level stations around sunrise and sunset. For
! this reason the 1.0mb allowance in the SDB tendency check was correct
! and should be applied in the MetDB.
!
! Revision 1.5  97/09/10  15:54:04  15:54:04  uspm (Pat McCormack)
! Pass 2 additional arguments to VALUSR. VALUSR has changed for
! MetDB retrieval - S.Cox                                             !F
!
! Revision 1.4  1997/08/14 13:18:44  uspm
! 13-08-97  !E  : Change CHARACTER*27997 CINDX to CHARACTER*27978 CINDX
!               : as expected by READIDX - J.Lewthwaite
!
! 11-08-97  !D  : Replace read of IDSK(3) with a call to READIDX to get
!               : element index rather than reading it from the last
!               : record of the storage dataset - J.Lewthwaite
!
! 11-08-97  !C  : Extra argument added to calls to BITINDEX and VALUSR
!               : due to retrieval changes to BITINDEX & VALUSR. Also
!               : removed declarations of MAXSEG, MAXTAB & 
!               : MAXELM - S.Cox
!
! Revision 1.3  1997/08/11 11:01:19  uspm
! Changes marked !d incorporated from COSMOS
!
! Revision 1.2  1997/07/31 11:39:35  uspm
! First revision for COSMOS
!
! Revision 1.1  1997/07/04 13:49:08  uspm
! Initial revision
!
! 02-09-96  !B  : Avoid errors when long ob in chain
!
! 19-08-96  !A  : S.M.N  New structure of element index in last storage
!               : record (there may be more than one sequence).
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

      IMPLICIT NONE

      CHARACTER*5 IDENT
      INTEGER IDATIM(5), OBHOUR, INDHOR,TAGHOR, CENDAY
      REAL PRESS,TEND,FLAG, OLDP, TOL

      INTEGER ISECT1(1)        !- New argument for VALUSR (not used)  !C
      LOGICAL NEWBITCALL       !- New argument for BITINDEX           !C

! variables in common (shared with synsto)    (ift? blocksize?)

      INTEGER      IXHOUR(3),NTRIES(3)        !!! MUST AGREE WITH SYNSTO
      CHARACTER*23 INDEKS(7*27998/23,3)       !!! MUST AGREE WITH SYNSTO

      INTEGER IDSK(5),INXBLK,INXHRS,ISLOTHR,INDLEN, NSQ,NELMIX
      LOGICAL LFLAG,LOCDFG
      LOGICAL MATCH                           !Returned from READIDx  !D

! element index variables

      CHARACTER*10000   CINDX(12)     ! ELEMENT INDEX RECORD    !1.7!E!A
      CHARACTER*1   XTYPE                                             !A
      INTEGER IDESC,IREPL,NELEM
      INTEGER WIDTH,DISPL,SCALE,REFVAL

      CHARACTER*6  LOCALD
      INTEGER ISTAT
      INTEGER NELREQ
      INTEGER LX,I, IBUFR,IL,F,X,Y                                    !A
      INTEGER NBLOCK,IBLOCK,NREC,LREP,MAXLREP                         !B
      PARAMETER (MAXLREP=999)                                         !B
      CHARACTER ENTRY*23, REPORT*(MAXLREP), BUFR*4, C*1               !B
      CHARACTER HEAD*132
      CHARACTER*1 CHRELM(1)    !- dummy argument for VALUSR           !F
      INTEGER     LENCHR(1)    !- dummy argument for VALUSR           !F

      DATA BUFR/'BUFR'/
      DATA TOL/100./           !- TOLERANCE 100PA (=1.0MB)            !G
      DATA INXBLK/0/           !- NONZERO ONCE MAP HAS BEEN READ
      DATA INDLEN/23/          !- length of index entry
      DATA NEWBITCALL/.TRUE./  !- TRUE for first call to BITINDEX     !C
      DATA ISECT1(1)/0/        !- ISECT1 not used in VALUSR           !C

      COMMON /SYNIX/ IDSK, IXHOUR,NTRIES, INDEKS
      SAVE

      FLAG=-9999999            !- SET FLAG TO "NO TEST DONE"
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/synseq.F,v $
     &'//'$ $Date: 30/01/2006 20:25:00$ $Revision: 1$'

      FLAG=-9999999           ! SET FLAG TO "NO TEST DONE"
      LFLAG=.FALSE.
      MATCH=.FALSE.           !Set TRUE in READIDX if element index OK!D

! Read map block to find dataset details (only first 8 args relevant)
! (No test can be done until the storage program has been called and
! the common block set, so return if the FT number is still zero.)

      IF (INXBLK.EQ.0) THEN
        IF (IDSK(3).EQ.0) RETURN
        CALL EB2ASC(4,BUFR)       ! first time only!
        CALL MDBIO(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LOCDFG,        !2.1
     &    NELMIX,IBLOCK,NTRIES,INDEKS,IXHOUR,NREC,LREP,REPORT,'MAPRD')
        NSQ=0
        IF (LOCDFG) NSQ=1

! The element index for fast retrieval is read from the data set.
! The element number is the one stored in the data dictionary for
! LNDSYN MSL_PESR so beware of changes to the data dictionary.  In
! general the element name can be transformed to a number by calls to
! DDICT, EXPELM and MAPELM.

        CALL READIDX('LNDSYN  ',CINDX,XTYPE,MATCH) !D
        IF (.NOT. MATCH) THEN
          WRITE(*,*)'SYNSEQ: MDB Error: Cannot find element index ',!d
     &    'for LNDSYN'                                              !d
        ENDIF
        NELEM=1                                                     !A
        IDESC=53         !  MSL_PESR                                !A
        IREPL=1                                                     !A
      ENDIF

! Convert date/time of report to century-hour

      CALL DATE31(IDATIM(3),IDATIM(2),IDATIM(1),CENDAY)
      OBHOUR=(CENDAY-1)*24+IDATIM(4)

! Century-hour of report required & hour relative to index period

      INDHOR=((OBHOUR-3-ISLOTHR)/6)*6
      TAGHOR=MOD(OBHOUR-3+24-ISLOTHR,6)

! Find which of the 3 index blocks in core to look through for a report
! 3 hours back.  (Maybe none if the data is too old - if so, return.)

      LX=0
      DO I=1,3
        IF (IXHOUR(I).EQ.INDHOR) LX=I
      ENDDO
      IF (LX.EQ.0) RETURN

! Loop round entries in index for right period looking for identifier.
! If the latest hour in the entry implies there can't be a report for
! the required time, or if there's only one ob and it's for the wrong
! hour, then there's no point in looking on, so return.

      DO I=1,NTRIES(LX)
        IF (INDEKS(I,LX)(3:7).EQ.IDENT) THEN
          IF (MOD(ICHAR(INDEKS(I,LX)(1:1)),64).LT.TAGHOR) RETURN
          IF (MOD(ICHAR(INDEKS(I,LX)(1:1)),64).NE.TAGHOR
     &      .AND. ICHAR(INDEKS(I,LX)(12:12)).EQ.1) RETURN

! Get relative block number from pointer in entry & convert it to true
! block number using parameters returned by call to read map.
! If trailer doesn't point any further, there's no report, so return

          ENTRY=INDEKS(I,LX)
   10     NBLOCK=ICHAR(ENTRY(22:22))*256+ICHAR(ENTRY(23:23))
          NREC  =ICHAR(ENTRY(20:20))*256+ICHAR(ENTRY(21:21))
          IF (NBLOCK.LE.0 .OR. NREC.LE.0) RETURN  ! NO REPORT FOUND

! Read next (or first) report in chain (& reset index entry to trailer
! for next pointer)  (Only relevant args: IBLOCK,NREC,LREP,REPORT)
! (MDBIO doesn't reread data block if a copy is in core, so may not find
! ob if index has just been updated... - hence check for zero length!)
! If ob is too long (i.e. rubbish on the end!), give up - MDBIO will
! have truncated it, so no trailer to follow chain.                   !B

          IBLOCK=1+NSQ+INXBLK+NBLOCK
          CALL MDBIO(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LOCDFG,      !2.1
     &      NELMIX,IBLOCK,NTRIES,INDEKS,IXHOUR,NREC,LREP,REPORT,'MSGRD')
          IF (LREP.LT.23 .OR. LREP.GT.MAXLREP) RETURN                 !B
          ENTRY=REPORT(LREP-22:LREP)

! If not right hour or not preferred report, look on down the chain.

          IF (MOD(ICHAR(ENTRY(1:1)),64).NE.TAGHOR) GO TO 10
          IF (ICHAR(ENTRY(17:17)).LT.128) GO TO 10

! If preferred report for 3 hours ago, retrieve pressure
                                                                      !A
          IBUFR=INDEX(REPORT,BUFR)
          IL=ICHAR(REPORT(IBUFR+29:IBUFR+29))*256 +                   !A
     &       ICHAR(REPORT(IBUFR+30:IBUFR+30))                         !A
          CALL DESFXY(IL,F,X,Y)                                       !A
          WRITE(LOCALD,'(I1,I2.2,I3.3)')F,X,Y                         !A
          CALL BITINDX(CINDX,IDESC,NELEM,IREPL,.FALSE.,             !2.0
     &       ISTAT,LFLAG,LOCALD,REPORT(IBUFR:),NELREQ,DISPL,
     &       WIDTH,SCALE,REFVAL,1,NEWBITCALL)                         !C
          CALL VALUSR(REPORT(IBUFR:),NELREQ,DISPL,WIDTH,SCALE,
     &                REFVAL,OLDP,1,1,C,C,C,LFLAG,ISECT1,             !C
     &                CHRELM,LENCHR)                                !F!A

! (only one element wanted, no characters involved - hence last 6 args!)

! Finally do test.  The only aim is to say whether this pressure is
! consistent with the last one and the tendency, so ignore any flag
! set on the old report - need longer sequence to see what's right.

          IF (ABS(PRESS-OLDP-TEND).LE.TOL) THEN
            FLAG=0.0        ! consistent
           ELSE
            FLAG=1.0        ! inconsistent
          ENDIF
!      PRINT*,'SYNSEQ ',ident,OLDP,PRESS,tend,FLAG
          RETURN            ! return with test done
        ENDIF
      END DO
      RETURN                ! return with no entry found
      END
