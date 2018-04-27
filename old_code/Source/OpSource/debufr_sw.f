      SUBROUTINE DEBUFR(DESCR,VALUES,NAMES,ND,NOBS,STRING,DSPLAY,
     &                  SECT2,CHARSECT2)                             !2.5

!-----------------------------------------------------------------------
!
! ROUTINE       : DEBUFR
!
! PURPOSE       : to decode bufr message
!
! DESCRIPTION   : get the descriptors, number of reports and flag to
!               : indicate compression from section 3, skipping
!               : earlier sections, & call decode to do the rest.
!
! CALLED BY     : user...
!
! CALLS         : BUFDATA, VALUE                                    !2.4
!
! ARGUMENTS     : (1) sequence of descriptors        (to be returned)
!               : (2) array for values               (to be returned)
!               : (3) for any character values       (to be returned)
!               : (4) number of descriptors          (to be returned)
!               :      (passed as length of descriptor array)
!               : (5) number of reports              (to be returned)
!               :      (passed as length of value array)
!               : (6) bufr message
!               : (7) flag set if display of values required
!               : (8) flag set if section2 required             !2.5
!               : (9) section 2 character string                !2.5
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         25/09/2008 10:53:50    Richard Weedon
!       Revised for BUFR Edition4
!  1    Met_DB_Project 1.0         30/01/2006 20:21:58    Sheila Needham  
! $
!
! Revision 2.6  2008/09/25 10:55 
! Revisions introduced from BUFR Edition 4    
! INTEGER NED added - Bufr edition No
! BUFR Edition 4 - Section 2 indicator at Octet 10
! Revised indicator extraction
! Richard Weedon 25th Sept 2008
!  
! Revision 2.5  2005/03/07 13:56:24  usmdb
! Split from MetDB version debufr.f as new arguments required to decode
! SIGWX info supplied in section 2.
! Alison Weir. 7th March 2005.
!
! Revision 2.4  2003/05/06 08:00:06  usmdb
! CALL DECODE is now CALL BUFDATA - S.Cox.
!
! Revision 2.3  2002/10/07  16:03:12  16:03:12  usmdb (MetDB account c/o usjh)
! 21 Oct 2002     C Long
! 2.3  Return immediately if message has no descriptors.
!
! Revision 2.2  2002/09/04  13:53:12  13:53:12  usmdb (Generic MetDB account)
! 16 Sept 2002    C Long
! 2.2  Return with NOBS=0 when the section lengths show that there are
!      no descriptors or no data, as well as when DECODE finds an error.
!
! Revision 2.1  2002/04/09  11:34:34  11:34:34  usmdb (Generic MetDB account)
! 15 Apr 2002    C Long, S Cox
! 2.1 Replace ICHAR by VALUE (ICHAR not the same on all machines!).
!     Removed DECODI/DBUFRI. Initialised BUFR, SEVENS in ASCII &
!     therefore removed calls to EB2ASC.
!
! Revision 2.0  2001/03/07  10:19:11  usmdb
! Added copyright. Modified header & comments - S.Cox
!
! Revision 1.3  1997/09/22 09:35:13  uspm
! Change labelled statements to be CONTINUE
!
! Revision 1.2  1997/06/19 14:41:59  uspm
! Add IMPLICIT NONE and variable declarations
!
! Revision 1.1  1997/06/19 13:37:21  uspm
! Initial revision
!                                                             !
! Apr 96 - S.Cox : change initiliasation of BUFR and 7777 from hex
!                  constants to EBCDIC initilisation with EB2ASC
!                  conversions.
!
! Jun 95 - S.Cox
!
!        1) test for length at start by edition number rather than
!           pointer to 7777 at end.
!        2) some cosmetic changes
!
! Dec 91 - allow for bufr version with total length at start
!
! Feb 91 - different entry points for real & integer
!
! Jun 90 - real array of values rather than integer, pass array sizes
!        - for decode to check subscripts
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!-----------------------------------------------------------------------
! declare variables.
!-----------------------------------------------------------------------

      INTEGER   DESCR(*)
      INTEGER   F
      INTEGER   FLOPT     ! flag for optional section               !2.1
      INTEGER   I
      INTEGER   IBEFOR    ! number of bits before value in string   !2.1
      INTEGER   L
      INTEGER   L0
      INTEGER   L1
      INTEGER   L2
      INTEGER   L3
      INTEGER   LMAX      ! Max pos. in STRING to check for 'BUFR'  !2.1
      INTEGER   MAXDES
      INTEGER   MAXVAL
      INTEGER   N
      INTEGER   NED       !BUFR Edition No in message
      INTEGER   ND
      INTEGER   NOBS
      INTEGER   RC        ! Return code from DECODE                 !2.1
      INTEGER   VALUE     ! function to get integer from bits       !2.1
      INTEGER   X
      INTEGER   Y

      REAL      VALUES(*)

      CHARACTER BUFR*4
      CHARACTER HEAD*132
      CHARACTER NAMES*(*)
      CHARACTER SEVENS*4
      CHARACTER STRING*(*)
      CHARACTER CHARSECT2*240   !Section 2 data                    !2.5

      LOGICAL   CMPRES
      LOGICAL   DSPLAY
      LOGICAL   SECT2     !TRUE if section 2 is required           !2.5

!-----------------------------------------------------------------------
! Revision information.
!-----------------------------------------------------------------------

      HEAD='$RCSfile: debufr_sw.f,v $ ' //
     &     '$Revision: 2$ $Date: 25/09/2008 10:53:50$'

!-----------------------------------------------------------------------
! initialise variables BUFR and SEVENS (in ASCII).
!-----------------------------------------------------------------------

      BUFR=CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82)                   !2.1
      SEVENS=CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55)                 !2.1

      MAXDES=ND
      MAXVAL=NOBS

!-----------------------------------------------------------------------
! find 'BUFR' (in ASCII) at start
!-----------------------------------------------------------------------

      LMAX=MIN(100,LEN(STRING))                                     !2.1

      N=1
      DO WHILE (STRING(N:N+3).NE.BUFR .AND. N.LT.LMAX)              !2.1
        N=N+1
      ENDDO                                                         !2.1

      IF (N.EQ.LMAX) THEN                                           !2.1
        PRINT *,' BUFR NOT FOUND'
        NOBS=0
        RETURN
      ELSE                                                          !2.1
        N=N+4                    ! move past BUFR                   !2.1
      ENDIF

!-----------------------------------------------------------------------
! skip section 1.  the length after 'bufr' is (from edition 2 on)
! the length of the whole message, so skip it first if it points
! to '7777'. the minimum total length is (section by section)
! 8+18+0+10+4+4=44.
!
! if edition number is 2 or more, skip length anyway.
! (at one time it was not clear whether edition would change
! from 0 to 1 or 1 to 2, so checking the edition number alone
! seemed risky.
!-----------------------------------------------------------------------

      IBEFOR=0                                                      !2.1
      NED=VALUE(STRING(N+3:N+3),IBEFOR,8)  !BUFR Edition No         !2.6
      L=VALUE(STRING(N:N+2),IBEFOR,24)                              !2.1
      IBEFOR=0                                                      !2.1
      IF ((L.GE.44 .AND. STRING(N+L-8:N+L-5).EQ.SEVENS)             !2.6
     &       .OR. NED.GE.2) THEN                                    !2.6
        L0=L
        N=N+4
        IBEFOR=0                                                    !2.1
        L1=VALUE(STRING(N:N+2),IBEFOR,24)                           !2.1
      ELSE
        L1=L
      ENDIF
!                 Check flag for presence of optional section        2.6
      IF (NED.LT.4) THEN                                            !2.6
         IBEFOR=56 ! Editions 0-3 Section2 indicator flag in byte 8  2.6
      ELSE                                                          !2.6
         IBEFOR=72  ! Edition 4 Section2 indicator flag in byte 8    2.6
      ENDIF                                                         !2.6
      FLOPT=VALUE(STRING(N:N+9),IBEFOR,1)                           !2.6
      N=N+L1

!-----------------------------------------------------------------------
! if there's a section 2 (optional), store in CHARSECT2 if required !2.5
!-----------------------------------------------------------------------

      IF (FLOPT.EQ.1) THEN                                          !2.1
        IBEFOR=0                                                    !2.1
        L2=VALUE(STRING(N:N+2),IBEFOR,24)                           !2.1
	
        IF (SECT2) THEN                                             !2.5
	  IF (L2-4.LE.240) THEN    !ensure CHARSECT2 is large enough 2.5
            CHARSECT2=STRING(N+4:N+L2)                              !2.5
	  ELSE                                                      !2.5
	    CHARSECT2=STRING(N+4:N+244)                             !2.5
	    PRINT *,'Section 2 truncated'                           !2.5
	  ENDIF                                                     !2.5
	ENDIF                                                       !2.5
        N=N+L2  
	
      ENDIF

!-----------------------------------------------------------------------
! find no. of reports & compression flag
!-----------------------------------------------------------------------

      IBEFOR=0                                                      !2.1
      L3=VALUE(STRING(N:N+2),IBEFOR,24)                             !2.1
      IBEFOR=0                                                      !2.1
      NOBS=VALUE(STRING(N+4:N+5),IBEFOR,16)                         !2.1

      IBEFOR=1                                                      !2.1
      IF (VALUE(STRING(N+6:N+6),IBEFOR,1).EQ.1) THEN                !2.1
        CMPRES=.TRUE.
      ELSE
        CMPRES=.FALSE.
      ENDIF

! Find number of descriptors & return if there aren't any.          !2.3

      ND=(L3-7)/2
      IF (ND.LE.0) THEN                                             !2.3
        PRINT *,' No descriptors in message ',STRING                !2.3
        NOBS=0                                                      !2.3
        RETURN                                                      !2.3
      ENDIF                                                         !2.3

!-----------------------------------------------------------------------
! copy the descriptors to fullwords
! (consecutive 16-bit fields, so don't reset IBEFOR in between)     !2.1
!-----------------------------------------------------------------------

      IBEFOR=0                                                      !2.1
      DO I=1,ND
        DESCR(I)=VALUE(STRING(N+7:),IBEFOR,16)                      !2.1
      ENDDO

!-----------------------------------------------------------------------
! handle section 4 in bufdata
!-----------------------------------------------------------------------

      N=N+L3
      CALL BUFDATA(DESCR,VALUES,NAMES,ND,NOBS,STRING(N:),           !2.4
     &             CMPRES,DSPLAY,MAXDES,MAXVAL,RC)                  !2.4

      IF (RC.NE.0) THEN                                             !2.1
        CALL DESFXY(DESCR(ND),F,X,Y)
        PRINT *,' ERROR',ND,'-TH DESCRIPTOR IS',F*100000+X*1000+Y   !2.1
        NOBS=0
      ENDIF                                                         !2.1

!-----------------------------------------------------------------------
! return to calling program.
!-----------------------------------------------------------------------

      RETURN
      END
