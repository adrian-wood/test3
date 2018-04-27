      SUBROUTINE SHPSEQ(IDENT,IDATIM,PRESS,TEND,PFLAG,
     &                  NEWLAT,NEWLON,DS,VS,LLFLAG)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SHPSEQ
!
! PURPOSE       : PRESSURE TENDENCY & MOVEMENT CHECK FOR SHIPS
!
! DESCRIPTION   : SHPSEQ RETRIEVES PRESSURE, LATITUDE & LONGITUDE
!               : 3 HOURS AGO, USING THE INDEX(ES) BEING UPDATED BY
!               : SYNSTO TO FIND THE REPORT AND BIT COUNT RETRIEVAL
!               : TO FIND THE PRESSURE etc.
!
! DATA TYPE(S)  : SHIPs
!
! CALLED BY     : SYNEXP
!
! CALLS         : MDBIO (map & message calls), DATE31, EB2ASC,
!               : BITINDX,VALUSR
!               : COMMON /SHPIX/ IS SHARED WITH SHPSTO
!
! PARAMETERS    :  (1) SHIP FOR WHICH REPORT IS REQUIRED           (I)
!               :  (2) DATE/TIME OF NEW REPORT (year/mon/day/hour) (I)
!               :  (3) PRESSURE IN NEW REPORT                      (I)
!               :  (4) PRESSURE TENDENCY IN NEW REPORT             (I)
!               :  (5) RESULT OF PRESSURE TEST (0: CONSISTENT,     (O)
!               :      (1: INCONSISTENT, MISSING: NO TEST)
!               :  (6) LATITUDE IN NEW REPORT                      (I)
!               :  (7) LONGITUDE IN NEW REPORT                     (I)
!               :  (8) DIRECTION OF MOVEMENT IN LAST 3 HOURS       (I)
!               :      (DEGREES CLOCKWISE FROM NORTH)
!               :  (9) MEAN SPEED LAST 3 HOURS (CONVERTED TO M/S)  (I)
!               : (10) RESULT OF POSITION TEST (0: CONSISTENT,     (O)
!               :      (1: INCONSISTENT, MISSING: NO TEST)
!
! REVISION INFO :
!
! $Workfile: shpseq.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 08/08/2008 16:29:07$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         08/08/2008 16:29:07    Brian Barwell
!       Correction to size of INDEKS array.
!  1    Met_DB_Project 1.0         30/01/2006 20:24:15    Sheila Needham  
! $
! Revision 2.1  2001/07/03  10:39:44  10:39:44  usmdb (MetDB account c/o usjh)
! Removed argument LFLAG from MDBIO calls as not used in MDBIO - S.Cox
!
! Revision 2.0  2001/05/31  13:27:53  13:27:53  usmdb (Generic MetDB account)
! Removed argument XTYPE from call to BITINDX as it is no longer
! used in BITINDX. Removed unused variables, added copyright and
! modified header - S.Cox
!
! Revision 1.6  98/11/12  08:49:12  08:49:12  usmdb (Generic MetDB account)
! 16-11-98 S.Cox
! Change declaration of CINDEX - now an array of element indexes.
!
! Revision 1.5  97/09/10  15:48:48  15:48:48  uspm (Pat McCormack)
! Correct call to BITINDEX. Pass 3 elems (lat, lon,pressure)
! instead of 1. Also pass 2 additional arguments to VALUSR.           !F
!
! Revision 1.4  1997/08/14 13:16:23  uspm
! Change CHARACTER*27997 CINDX TO CHARACTER*27978 CINDX as expected
! by READIDX - J.Lewthwaite                                           !E
!
! Revision 1.3  1997/08/11 11:05:26  uspm
! Replace read of IDSK(3) with call to READIDX to get ELEMIDX rather
! than reading the bitindex from the last record in the storage
! dataset. - J.Lewthwaite                                             !D
! Extra argument added to calls to BITINDEX and VALUSR due to
! retrieval changes to BITINDEX & VALUSR - S.Cox                      !C
!
! Revision 1.2  1997/07/31 11:34:00  uspm
! First revision for COSMOS
!
! Revision 1.1  1997/07/04 13:24:14  uspm
! Initial revision
!
! 02-09-96  !B  : Avoid errors when long ob in chain
!
! 19-08-96  !A  : S.M.N New format element index in last storage record
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER*9 IDENT
      INTEGER IDATIM(5), OBHOUR, INDHOR,TAGHOR, CENDAY              !2.0
      REAL PRESS,TEND,PFLAG, OLDP, NEWLAT,NEWLON, DS,VS, LLFLAG, VSDIST
      REAL TOL, DSTOL,VSTOL
      REAL ARRAY(3), OLDLAT,OLDLON, LATDIF,LONDIF, X,Y, DIST,DIR
      REAL A,PI, RMISS

      INTEGER ISECT1(1)        !- New argument for VALUSR (not used)  !C
      LOGICAL NEWBITCALL       !- New argument for BITINDEX           !C
      CHARACTER*80 HEAD                                              !2

!-----------------------------------------------------------------------
! variables in common (shared with SYNSTO)    (ift? blocksize?)
!-----------------------------------------------------------------------

      INTEGER      IXHOUR(3), NTRIES(3)   !! MUST AGREE WITH SYNSTO
      CHARACTER*23 INDEKS(3*27998/23,3)   !! MUST AGREE WITH SYNSTO  !2

      INTEGER IDSK(5),INXBLK,INXHRS,ISLOTHR,INDLEN, NSQ,NELMIX
      LOGICAL LFLAG,LOCDFG                                          !2.0
      LOGICAL FIRST                                                  !2
      LOGICAL MATCH                           !Returned from READIDX  !D

!-----------------------------------------------------------------------
! element index variables
!-----------------------------------------------------------------------
                                                                      !A
      CHARACTER*10000 CINDX(12)                                 !1.6!E!A
      CHARACTER*1 XTYPE                                               !A
      INTEGER IDESC(3),IREPL(3),WIDTH(3),SCALE(3),REFVAL(3),DISPL(3)  !A

      INTEGER NELREQ,NELEM    ! NUMBER OF ELEMENTS TO BE RETRIEVED (=3)
      INTEGER LX,I, IBUFR,IL,F,IX,IY,J,ISTAT                          !A
      INTEGER NBLOCK,IBLOCK,NREC,LREP,MAXLREP                         !B

      PARAMETER (MAXLREP=999)                                         !B
      CHARACTER ENTRY*23, REPORT*(MAXLREP), BUFR*4, C*1               !B
      CHARACTER*6  LOCALD                                             !A
      CHARACTER*1  CHRELM(1)  ! dummy argument to VALUSR              !F
      INTEGER      LENCHR(1)  ! dummy argument to VALUSR              !F

      DATA BUFR/'BUFR'/
      DATA FIRST/.TRUE./      ! flag for first call to SHPSEQ        !2
      DATA TOL/10./           ! TOLERANCE 10PA (=0.1MB)
      DATA VSTOL/30000./      ! 30 km (5 knots roughly 28km in 3hours)
      DATA DSTOL/45./         ! 45 DEGREES (SAME AS RESOLUTION)
      DATA RMISS/-9999999./   ! missing data
      DATA INXBLK/0/          ! NONZERO ONCE MAP HAS BEEN READ
      DATA INDLEN/23/         ! length of index entry
      DATA A/6371229/         ! radius of earth in metres
      DATA PI/3.14159265/
      DATA NEWBITCALL/.TRUE./  !- TRUE for first call to BITINDEX     !C
      DATA ISECT1(1)/0/        !- ISECT1 not used in VALUSR           !C

      COMMON /ShpiX/ IDSK, IXHOUR,NTRIES, INDEKS

      IF (FIRST) THEN                                                !2
        HEAD = '$Workfile: shpseq.f$ ' //
     &         '$Revision: 2$ $Date: 08/08/2008 16:29:07$'
        FIRST = .FALSE.                                              !2
      END IF                                                         !2

      PFLAG=RMISS             ! SET FLAG TO "NO TEST DONE"
      LLFLAG=RMISS            ! SET FLAG TO "NO TEST DONE"
      LFLAG=.FALSE.
      MATCH=.FALSE.           ! Set Flag to False - it will be set    !D
                              ! TRUE by READIDX if the element        !D
                              ! index is matched                      !D

!-----------------------------------------------------------------------
! Read map block to find dataset details (only first 8 args relevant)
! (No test can be done until the storage program has been called and
! the common block set, so return if the FT number is still zero.)
!-----------------------------------------------------------------------

      IF (INXBLK.EQ.0) THEN
        IF (IDSK(3).EQ.0) RETURN
        CALL EB2ASC(4,BUFR)        ! first time only!
        CALL MDBIO(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LOCDFG,        !2.1
     &    NELMIX,IBLOCK,NTRIES,INDEKS,IXHOUR,NREC,LREP,REPORT,'MAPRD')
        NSQ=0
        IF (LOCDFG) NSQ=1

!-----------------------------------------------------------------------
! Read the element index for bit count retrieval from the data set.
! The element numbers are from the data dictionary for SHPSYN, beware
! of changes to the data dictionary elements list.
!
! There may be more than one element index, sort it out in BITINDX later
!-----------------------------------------------------------------------

        CALL READIDX('SHPSYN  ',CINDX,XTYPE,MATCH)                    !D
        IF (.NOT. MATCH) THEN                                         !D
          WRITE(*,*)'In SHPSEQ: MDB Error: Cannot find element ',     !D
     &    'index for subtype SHPSYN'                                  !D
        ENDIF
        NELEM=3                                                       !A
        IDESC(1)=3   ! LTTD                                           !A
        IDESC(2)=4   ! LNGD                                           !A
        IDESC(3)=49  ! MSL_PESR                                       !A
        DO J=1,3                                                      !A
          IREPL(J)=1                                                  !A
        ENDDO                                                         !A
      ENDIF

!-----------------------------------------------------------------------
! Convert date/time of report to century-hour
!-----------------------------------------------------------------------

      CALL DATE31(IDATIM(3),IDATIM(2),IDATIM(1),CENDAY)
      OBHOUR=(CENDAY-1)*24+IDATIM(4)

!-----------------------------------------------------------------------
! Century-hour of report required & hour relative to index period
!-----------------------------------------------------------------------

      INDHOR=((OBHOUR-3-ISLOTHR)/6)*6
      TAGHOR=MOD(OBHOUR-3+24-ISLOTHR,6)

!-----------------------------------------------------------------------
! Find which of the 3 index blocks in core to look through for a report
! 3 hours back.  (Maybe none if the data is too old - if so, return.)
!-----------------------------------------------------------------------

      LX=0
      DO I=1,3
        IF (IXHOUR(I).EQ.INDHOR) LX=I
      ENDDO
      IF (LX.EQ.0) RETURN

!-----------------------------------------------------------------------
! Loop round entries in index for right period looking for identifier
! if the latest hour in the entry implies there can't be a report for
! the required time, then no point in looking on - return.
!-----------------------------------------------------------------------

      DO I=1,NTRIES(LX)
        IF (INDEKS(I,LX)(3:11).EQ.IDENT) THEN
          IF (MOD(ICHAR(INDEKS(I,LX)(1:1)),64).LT.TAGHOR) RETURN
          IF (MOD(ICHAR(INDEKS(I,LX)(1:1)),64).NE.TAGHOR
     &      .AND. ICHAR(INDEKS(I,LX)(12:12)).EQ.1) RETURN

!-----------------------------------------------------------------------
! Get relative block number from pointer in entry & convert it to true
! block number using parameters returned by call to read map.
! If trailer doesn't point any further, there's no report, so return
!-----------------------------------------------------------------------

          ENTRY=INDEKS(I,LX)
   10     NBLOCK=ICHAR(ENTRY(22:22))*256+ICHAR(ENTRY(23:23))
          NREC  =ICHAR(ENTRY(20:20))*256+ICHAR(ENTRY(21:21))
          IF (NBLOCK.LE.0 .OR. NREC.LE.0) RETURN  ! NO REPORT FOUND

!-----------------------------------------------------------------------
! Read next (or first) report in chain (& reset index entry to trailer
! for next pointer)  (Only relevant args: IBLOCK,NREC,LREP,REPORT)
! (MDBIO doesn't reread data block if a copy is in core, so may not find
! ob if index has just been updated... - hence check for zero length!)
! If ob is too long (i.e. rubbish on the end!), give up - MDBIO will
! have truncated it, so no trailer to follow chain.                   !B
!-----------------------------------------------------------------------

          IBLOCK=1+NSQ+INXBLK+NBLOCK
          CALL MDBIO(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LOCDFG,      !2.1
     &      NELMIX,IBLOCK,NTRIES,INDEKS,IXHOUR,NREC,LREP,REPORT,'MSGRD')
          IF (LREP.LT.23 .OR. LREP.GT.MAXLREP) RETURN                 !B
          ENTRY=REPORT(LREP-22:LREP)

!-----------------------------------------------------------------------
! If not right hour or not preferred report, look on down the chain.
!-----------------------------------------------------------------------

          IF (MOD(ICHAR(ENTRY(1:1)),64).NE.TAGHOR) GO TO 10
          IF (ICHAR(ENTRY(17:17)).LT.128) GO TO 10

!-----------------------------------------------------------------------
! If preferred report for 3 hours ago, retrieve pressure & lat/long
! (only one report, no characters wanted - hence last 5 VALUSR args!)
!-----------------------------------------------------------------------

          IBUFR=INDEX(REPORT,BUFR)
          IL=ICHAR(REPORT(IBUFR+29:IBUFR+29))*256 +                   !A
     &       ICHAR(REPORT(IBUFR+30:IBUFR+30))                         !A
          CALL DESFXY(IL,F,IX,IY)                                     !A
          WRITE(LOCALD,'(I1,I2.2,I3.3)')F,IX,IY                       !A
          CALL BITINDX(CINDX,IDESC,NELEM,IREPL,.FALSE.,           !2.0!A
     &                 ISTAT,LFLAG,LOCALD,REPORT(IBUFR:),NELREQ,      !A
     &                 DISPL,WIDTH,SCALE,REFVAL,3,NEWBITCALL)     !F!C!A
          CALL VALUSR(REPORT(IBUFR:),NELREQ,
     &                DISPL,WIDTH,SCALE,REFVAL,
     &                ARRAY,1,1,C,C,C,.FALSE.,ISECT1,               !C!A
     &                CHRELM,LENCHR)                                  !F
          OLDLAT=ARRAY(1)
          OLDLON=ARRAY(2)
          OLDP=ARRAY(3)

          IF (OLDLAT.NE.RMISS .AND. OLDLON.NE.RMISS .AND.
     &        NEWLAT.NE.RMISS .AND. NEWLON.NE.RMISS .AND.
     &        DS.NE.RMISS .AND. VS.NE.RMISS) THEN

!-----------------------------------------------------------------------
! Test the ship's movement, working out a distance from the latitude &
! longitude differences and comparing it with VS, the mean speed over
! the last 3 hours (in m/s).
! (A is earth's radius in metres; speed*3*3600 is metres in 3 hours)
! N.B. The latitude difference is simply a fraction of the earth's
! circumference; the distance represented by the longitude difference
! depends on the latitude!  In one line the position difference is:
! 2*PI*A/360*SQRT((LAT1-LAT2)**2+((LONG1-LONG2)*COS((LAT1+LAT2)/2))**2)
!-----------------------------------------------------------------------

            LATDIF=NEWLAT-OLDLAT
            LONDIF=NEWLON-OLDLON
            X=(LONDIF/360)*(2*PI*A)*COS((OLDLAT+NEWLAT)*PI/360.)
            Y=(LATDIF/360)*(2*PI*A)

            DIST=SQRT(X**2+Y**2)          ! distance from positions
            VSDIST=VS*3.*3600.            ! distance from mean speed

!-----------------------------------------------------------------------
! DS is measured clockwise from N with a resolution of 45 degrees, so
! find DIR as an angle from N too;
! accept differences close to 360 as well as close to 0.
!-----------------------------------------------------------------------

            IF (DIST.NE.0) THEN
              IF (LATDIF.NE.0) THEN
                DIR=ATAN(LONDIF/LATDIF)*180./PI
                IF (LATDIF.LT.0) DIR=DIR+180.
                IF (LATDIF.GT.0 .AND. DIR.LT.0) DIR=DIR+360.
              ELSE
                IF (LONDIF.GT.0) DIR=90.  ! eastwards (compass bearing!
                IF (LONDIF.LT.0) DIR=270. ! westwards (compass bearing!
              ENDIF
            ELSE
              DIR=DS                      ! no ds check if dist=0
            ENDIF

!-----------------------------------------------------------------------
! Movement OK if distance & direction both agree to within tolerance
!-----------------------------------------------------------------------

            IF ((ABS(DIR-DS).LE.DSTOL .OR. ABS(DIR-DS).GE.360.-DSTOL)
     &                            .AND. ABS(DIST-VSDIST).LE.VSTOL) THEN
              LLFLAG=0.0                  ! consistent
            ELSE
              LLFLAG=1.0                  ! inconsistent
            ENDIF
          ENDIF

!-----------------------------------------------------------------------
! Now test pressure. The only aim is to say whether this pressure is
! consistent with the last one and the tendency, so ignore any flag
! set on the old report - need longer sequence to see what's right.
!-----------------------------------------------------------------------

          IF (PRESS.NE.RMISS .AND. OLDP.NE.RMISS) THEN
            IF (ABS(PRESS-OLDP-TEND).LE.TOL) THEN
              PFLAG=0.0     ! consistent
            ELSE
              PFLAG=1.0     ! inconsistent
            ENDIF
          ENDIF
!         PRINT*,'SHPSEQ ',ident,oldp,press,tend,pflag
!         PRINT*,'SHPSEQ',OLDLAT,OLDLON,newlat,newlon,llflag
          RETURN            ! return with test done
        ENDIF
      ENDDO

      RETURN                ! return with no entry found
      END
