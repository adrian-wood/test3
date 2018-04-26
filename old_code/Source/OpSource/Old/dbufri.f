      SUBROUTINE DBUFRI(DESCR,VALUES,NAMES,ND,NOBS,STRING,DSPLAY)

!-----------------------------------------------------------------------
!
! ROUTINE       : DBUFRI
!
! PURPOSE       : to decode bufr message
!
! DESCRIPTION   : Get the descriptors, number of reports and flag to
!               : indicate compression from section 3, skipping
!               : earlier sections, & call decodi to do the rest.
!
! CALLED BY     : user...
!
! CALLS         : DECODI
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
!
! REVISION INFO :
!
! $Workfile: dbufri.f$ $Folder: Old$
! $Revision: 3$ $Date: 26/11/2007 11:13:30$
!
! CHANGE RECORD :
!
! $Log:
!  3    Met_DB_Project 1.2         26/11/2007 11:13:30    Brian Barwell
!       Obsolete routine used for SAT120 storage (removed from BUFR load
!       module on 19 November 2007).
!  2    Met_DB_Project 1.1         19/03/2007 10:32:01    Brian Barwell
!       Modifications for BUFR edition 4.
!  1    Met_DB_Project 1.0         30/01/2006 20:21:57    Sheila Needham  
! $
! Revision 2.0  2002/04/09  11:46:47  11:46:47  usmdb (Generic MetDB account)
! Initial version
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
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
      INTEGER   FLOMP
      INTEGER   FLOPT
      INTEGER   I
      INTEGER   ID
      INTEGER   IDES
      INTEGER   L
      INTEGER   L0
      INTEGER   L1
      INTEGER   L2
      INTEGER   L3
      INTEGER   MAXDES
      INTEGER   MAXVAL
      INTEGER   N
      INTEGER   ND
      INTEGER   NED            ! BUFR edition number in message      !2
      INTEGER   NOBS
      INTEGER   X
      INTEGER   Y

      REAL      VALUES(*)

      CHARACTER BUFR*4
      CHARACTER HEAD*80                                              !2
      CHARACTER NAMES*(*)
      CHARACTER SEVENS*4
      CHARACTER STRING*(*)

      LOGICAL   CMPRES
      LOGICAL   DSPLAY

      HEAD='$Workfile: dbufri.f$ ' //
     &     '$Revision: 3$ $Date: 26/11/2007 11:13:30$'

!-----------------------------------------------------------------------
! Initialise variables BUFR and SEVENS.
!-----------------------------------------------------------------------

      BUFR = CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82)                  !2
      SEVENS=CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55)                  !2

      MAXDES=ND
      MAXVAL=NOBS

!-----------------------------------------------------------------------
! find 'BUFR' (in ASCII) at start
!-----------------------------------------------------------------------

      N=1
   20 CONTINUE
      IF (STRING(N:N+3).NE.BUFR) THEN
        IF (N.LT.100) THEN
          N=N+1
          GO TO 20
        ELSE
          PRINT *,' BUFR NOT FOUND'
          NOBS=0
          RETURN
        ENDIF
      ENDIF
      N=N+4

!-----------------------------------------------------------------------
! Skip section 1. The length after 'BUFR' is (from edition 2 on) the
! length of the whole message, so skip it first if it points to '7777'.
! The minimum total length is (section by section) 8+18+0+10+4+4=44.
!
! If edition number is 2 or more, skip length anyway.
! (At one time it was not clear whether edition would change from 0
! to 1 or 1 to 2, so checking the edition number alone seemed risky.)
!-----------------------------------------------------------------------

      NED = ICHAR(STRING(N+3:N+3))  ! BUFR edition number            !2

      L=ICHAR(STRING(N+1:N+1))*256+ICHAR(STRING(N+2:N+2))
      IF ((L.GE.44 .AND. STRING(N+L-8:N+L-5).EQ.SEVENS)
     &       .OR. NED.GE.2) THEN                                     !2
        L0=L
        N=N+4
        L1=ICHAR(STRING(N+1:N+1))*256+ICHAR(STRING(N+2:N+2))
      ELSE
        L1=L
      ENDIF
!                             Get flag for presence of optional section
      IF (NED.LT.4) THEN                                             !2
        FLOPT=ICHAR(STRING(N+7:N+7))  ! Edtns. 0-3: Flag in byte 8
      ELSE                                                           !2
        FLOPT=ICHAR(STRING(N+9:N+9))  ! Edtn. 4: Flag in byte 10     !2
      END IF                                                         !2
      N=N+L1

!-----------------------------------------------------------------------
! if there's a section 2 (optional), skip it.
!-----------------------------------------------------------------------

      IF (FLOPT.GE.128) THEN
        L2=ICHAR(STRING(N+1:N+1))*256+ICHAR(STRING(N+2:N+2))
        N=N+L2
      ENDIF

!-----------------------------------------------------------------------
! find no. of reports & compression flag
!-----------------------------------------------------------------------

      L3=ICHAR(STRING(N+1:N+1))*256+ICHAR(STRING(N+2:N+2))
      NOBS=ICHAR(STRING(N+4:N+4))*256+ICHAR(STRING(N+5:N+5))
      FLOMP=ICHAR(STRING(N+6:N+6))
      IF (MOD(FLOMP,128).GE.64) THEN
        CMPRES=.TRUE.
      ELSE
        CMPRES=.FALSE.
      ENDIF
      ND=(L3-7)/2

!-----------------------------------------------------------------------
! copy the descriptors to fullwords
!-----------------------------------------------------------------------

      DO I=1,ND
        ID=N+7+(I-1)*2
        DESCR(I)=ICHAR(STRING(ID:ID))*256+ICHAR(STRING(ID+1:ID+1))
      ENDDO

!-----------------------------------------------------------------------
! handle section 4 in decode
!-----------------------------------------------------------------------

      N=N+L3
      CALL DECODI(DESCR,VALUES,NAMES,ND,NOBS,STRING(N:),
     &            CMPRES,DSPLAY,MAXDES,MAXVAL,*10)
      RETURN

   10 CONTINUE
      CALL DESFXY(DESCR(ND),F,X,Y)
      IDES=F*100000+X*1000+Y
      PRINT *,' ERROR',ND,'-TH DESCRIPTOR IS',IDES
      NOBS=0

!-----------------------------------------------------------------------
! return to calling program.
!-----------------------------------------------------------------------

      RETURN
      END
