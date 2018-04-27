      SUBROUTINE AIRENC(AIR_ARAY1,AIR_ARAY2,ARYFLG,INDX_ARAY1,
     &                  INDX_ARAY2,SIGN,BEAC_NAME,REPORT,REPLEN,
     &                  SPAN,CCCC,NFT,TOR,TTAAII)                  !2.2

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRENC
!
! PURPOSE       : ROUTINES CALLS THE MAIN STORAGE ROUTINES
!
! DESCRIPTION   : THE CHARACTER REPORT IS TAGGED TO THE FRONT OF THE
!                 BUFR MESSAGE. THE DECODED VALUES ARE PASSED TO THE
!                 BUFR ENCODE ROUTINES AND THE FINAL MESSAGE IS PASSED
!                 TO AMDSTO
!
! CALLED BY     : AIRARP
!
! CALLS         : AIRIND
!                 DATIM
!                 AIRPRE
!                 AIRCOD
!                 AIRSTO
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:38$
! $Source: /data/us0400/mdb/op/lib/source/RCS/airenc.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:38    Sheila Needham  
! $
! Revision 2.2  2002/03/07  15:56:11  15:56:11  usmdb (Generic MetDB account)
! 18 march 2002     C Long
! 2.2  Pass bulletin time span through to AIRIND
! 
! Revision 2.1  2002/01/16  09:38:50  09:38:50  usmdb (Generic MetDB account)
! 21 Jan 2002     C Long
! 2.1  Reject any ob with an 8-character identifier.
!
! Revision 2.0  2001/05/31  13:27:20  13:27:20  usmdb (Generic MetDB account)
! Removed argument NREPS from call to AIRCOD as not used in AIRCOD.
! Removed arguments NREPS and CORF from call to AIRIND as not used
! in AIRIND. Removed unused arguments IN and CORF. Changed
! preprocessor directive to if defined 1 , else... Changed
! initialisation of BUFR to a data statement for non-1  code.
! Added copyright and modified header and comments - S.Cox
!
! Revision 1.7  99/07/12  16:12:01  16:12:01  usmdb (Generic MetDB account)
! 19 July 1999     C Long
! 1.7 Loop round obs in opposite order, MID first,
!     to make it easier to find next midpoint.
!
! Revision 1.6  98/04/10  13:01:39  13:01:39  usmdb (Generic MDB account)
! Further improvement to the date/time check
!
! Revision 1.5  98/02/04  14:53:57  14:53:57  usmdb (Generic MDB account)
! Ensure that we can never pass an invalid date/time to
! AIRSTO
!
! Revision 1.4  1998/01/29 11:18:26  usmdb
! Addition of IBM preprocess directive
!
! Revision 1.3  97/07/31  09:06:26  09:06:26  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.2  1997/07/25 15:30:50  uspm
! Version dated 16/6/97 from 1
!
! Revision 1.1  1997/07/04 10:22:10  uspm
! Initial revision
!
! 02/04/98 - Further improvement to the date/time check. The          !f
!            check for the year has been made more restrictive        !f
!            and the date/time check has been moved into a            !f
!            function subroutine.                                     !f
!
! 02/02/98 - Ensure that we can never pass an invalid date/time
!            group to AIRSTO.                                         !e
!
! 12/06/97 - Pass TOR to AIRSTO                                       !d
!
! 26/03/97 - CORRECT CALL TO CCCODE                                   !C
!
! 17/03/97 - CALL CCCODE AND PUT ICCCC IN SECTION 1 OF BUFR MESSAGE   !C
!
!   Oct 96 - make sure text starts with identifier, not ARP!          !b
!
!   Sep 96 - call AIRSTO rather than TAFREP to store obs              !a
!            set current time in section 1 of message (as time
!            of receipt) rather than data time                        !a
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!declare variables
      INTEGER DESCR(25)        !BUFR descriptor
      INTEGER DATIME(5)        !DATIME array
      INTEGER NOW(8)           !
      INTEGER TOR(5)           !time or reciept
      INTEGER ARYFLG
      INTEGER TOTSTORE
      INTEGER NDESC            !number of BUFR descriptors
      INTEGER REPLEN           !length of character report
      INTEGER LENGTH           !length of BUFR message
      INTEGER LMESS            !length of BUFR+CHAR message
      INTEGER SPAN             !time span of reports in bulletin   !2.2
      INTEGER NFT              !data set unit number for MDB storage
      INTEGER I                !used in loops
      INTEGER IOS              !used to check MDB data set has opened
      INTEGER IDES
      INTEGER LOOP
      INTEGER Lident           ! length of ident (for INDEX to find it)
      INTEGER istart           ! ident starts at REPORT(ISTART:)
      INTEGER ICCCC            !Bufr code number for collecting centre
      INTEGER SYS_YEAR         !Current system year

!declare real
      REAL    AIR_ARAY1(18)   !data array
      REAL    AIR_ARAY2(18)  !elements array with mid point
      REAL    INDX_ARAY1(8)                                          !a
      REAL    INDX_ARAY2(8)                                          !a

!declare character
      CHARACTER REPORT*(*)     !Raw character report
      CHARACTER CCCC*(*)
      CHARACTER TTAAII*(*)
      CHARACTER MESAGE*10240   !BUFR+CHAR message
      CHARACTER ENTRY*23
      CHARACTER IDENT*9
      CHARACTER BUFR*4         !BUFR indicator
      CHARACTER SIGN*8         !aircraft callsign
      CHARACTER BEAC_NAME*8
      CHARACTER HEAD*132       !Revision information

      LOGICAL DATIME_TEST      !Indicates date/time in valid range    !f
      LOGICAL FIRST            !TRUE if 1st time in routine         !2.2
      LOGICAL ARTMCK                                                  !f

      SAVE

!----------------------------------------------------------------------
!Data statements
!----------------------------------------------------------------------

      DATA FIRST/.TRUE./                                            !2.2

!----------------------------------------------------------------------
!Initialise variables
!----------------------------------------------------------------------

      IF (FIRST) THEN                                               !2.2
        FIRST=.FALSE.                                               !2.2
        BUFR = CHAR(66) // CHAR(85) // CHAR(70) // CHAR(82)         !2.2
        HEAD='$RCSfile: $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:20:38$ '
      ENDIF                                                         !2.2

! Loop round the obs (2 of them if MID), storing them in time      !1.7
! order, MID first, to make it easier to find the next midpoint.   !1.7

      DO LOOP=ARYFLG,1,-1                                          !1.7

        DO I=1,25
          DESCR(I)=-9999999
        ENDDO
        IOS=1
        NDESC=1
        MESAGE(:)=' '
        DESCR(1)=IDES(311196)
        DO I=1,5
          DATIME(I)=-9999999
        ENDDO
        TOTSTORE=0
        DATIME_TEST=.FALSE.                                          !f

!----------------------------------------------------------------------
!Call cccode to convert collecting centre to BUFR code table number
!----------------------------------------------------------------------

        CALL CCCODE(287,ICCCC,CCCC)                                  !C
!----------------------------------------------------------------------
! Make an index entry for this report
!----------------------------------------------------------------------

        CALL AIRIND(INDX_ARAY1,INDX_ARAY2,LOOP,SIGN,TTAAII,CCCC,
     &              DATIME,ENTRY,IDENT,SPAN)                       !2.2

!----------------------------------------------------------------------
!Get system time now and use it as the time of receipt for this report
!----------------------------------------------------------------------

        CALL DATIM(NOW)
        DO I=1,5
          TOR(I)=NOW(9-I)
        ENDDO
        SYS_YEAR=TOR(1)              !Set current system year         !f
!---------------------------------------------------------------------
! Put text (starting with IDENT) at start of string to be stored
! Return if ident has 8 characters (no space in *8 string)         !2.1
! - or if ident not found in report (shouldn't happen!)            !2.1
!---------------------------------------------------------------------

        LIDENT=INDEX(IDENT(1:8),' ')                               !2.1
        IF (LIDENT.EQ.0) THEN                                      !2.1
          PRINT *,' AIRENC rejected 8-character identifier:'       !2.1
          PRINT *,IDENT,'   ',CCCC,'   ',REPORT(1:50)              !2.1
          RETURN
        ENDIF
        ISTART=INDEX(REPORT,IDENT(1:LIDENT))                          !b
        IF (ISTART.EQ.0) RETURN                                       !b
        MESAGE(1:REPLEN-ISTART+1)=REPORT(ISTART:REPLEN)               !b

!---------------------------------------------------------------------
! Encode the message into BUFR
!---------------------------------------------------------------------

        CALL AIRCOD(AIR_ARAY1,AIR_ARAY2,LOOP,SIGN,BEAC_NAME,
     &              DESCR,NDESC,REPLEN,MESAGE,LENGTH,TOR)         !2.0!a

!---------------------------------------------------------------------
!Set overall length of BUFR mesage (BUFR + CHAR).
!Call to storage routine - after checking there is a BUFR message.
!---------------------------------------------------------------------

        LMESS=LENGTH+REPLEN

!---------------------------------------------------------------------
!index entry,datime array and the message are passed to AIRSTO for
!storage.
!Also insert the collecting centre BUFR no into section 1
!---------------------------------------------------------------------

        IF (MESAGE(REPLEN+1:REPLEN+4) .EQ. BUFR) THEN
          MESAGE(REPLEN+13:REPLEN+13)=CHAR(4)
          MESAGE(REPLEN+9:REPLEN+9)=CHAR(ICCCC/256)                  !C
          MESAGE(REPLEN+10:REPLEN+10)=CHAR(MOD(ICCCC,256))           !C

!----------------------------------------------------------------------
!Check that the date time of the AIREP report is within valid range.
!If it is DATIME_TEST will be TRUE and the report stored else an
!error message will be printed.
!----------------------------------------------------------------------

          DATIME_TEST=ARTMCK(DATIME,SYS_YEAR)                        !f
          IF (DATIME_TEST) THEN                                      !f
            CALL AIRSTO(DATIME,ENTRY,MESAGE(1:LMESS),NFT,27998,IDENT,
     &      TOR)                                                     !D
          ELSE
            WRITE(6,*)'AIREPS(AIRENC):Warning - Bad Date/Time group ',
     &      DATIME,REPORT(1:50)
          ENDIF
        ENDIF
      ENDDO

      RETURN    !return to get next message or bulletin
      END
