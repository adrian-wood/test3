      SUBROUTINE VALDAT(ITIME,ITOD,FOUND,ICT,IFAIL,CERR)

!-----------------------------------------------------------------------
!
! Routine       : VALDAT
!
! Purpose       : This routine performs date validation on the MDB
!               : request times.
!               : First the MDB request times are checked that they
!               : are in a valid format, Then various QC is performed
!               : on the START and END TIMES - whcich can be missing.
!               : See comments in code for full details.
!
! Called by     : VALREQ in MDB
!
! Calls         : DATCHK,HRS2DT,DT2HRS
!
! Parameters    : I/O  ITIME(9)  yyyy,mm,dd,hhmm,yyyy,mm,dd,hhmm,inc
!               : I    ITOD      1 for today format requested
!               : I    FOUND(*)  array of logicals for keyword found
!               : I    ICT(8)    current date/time (ymdhm...)
!               : O    IFAIL     8 for error, 4 for warning
!               : O    CERR      error message
!
! Revision info :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:57$
! $Source: /home/us0400/mdb/op/lib/source/RCS/valdat.F,v $
!
! Change record :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:57    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:27  usmdb
! Removal of argument to DATCHK as no longer needed.
! Addition of copyright - S.Cox
!
! Revision 1.5  2000/08/09  15:24:26  15:24:26  usmdb (Generic MDB account)
! 21 August 2000
! Rewrite of routine. Too many change to flag. Correct checks
! so that minutes are not ignored. Also allow for retrievals
! spanning beyond the current time - S.Cox
! 
! Revision 1.4  99/01/14  14:22:04  14:22:04  usmdb (Generic MDB account)
! 18-01-1999 S.Cox, Ref MetDB problem 220
! Correct setting of USER time to current time so that current time
! minutes are retained.
! v(G)=14, ev(G)=7
!
! Revision 1.3  97/09/10  15:25:40  15:25:40  uspm (Pat McCormack)
! Add a final check on START TIME and END TIME.
! If either is greater than the current time, set
! them equal to the current time.
!
! Revision 1.2  1997/08/04 13:43:09  uspm
! First revisioned version for COSMOS - with Y2K changes
!
! Revision 1.1  1997/02/12 09:19:42  uspm
! Initial revision
!
! 15-09-97  !C  : Add a final check on START TIME and END TIME. If
!               : either is greater than the current time, set them
!               : equal to the current time - S.Cox
!
! 23-10-96  !B  : Correction to change !A The end time hour was being
!               : set to hhZ when it should be set to hhmmZ - S.Cox
!
! 19-08-96  !A  : Set defualt end time to current time instead of
!               : start + 59 minutes - S.Needham
!
! 18-03-93      : Correct default start time
!
! 26-02-93      : Complete rewrite with a different argument list
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

      INTEGER       CNTMN    !- Current time in century mins
      INTEGER       DT2HRS   !- Convert date/time to century hours
      INTEGER       ICT(8)   !- Current time array
      INTEGER       IFAIL    !- Return code
      INTEGER       INDMN    !- User end time in century mins
      INTEGER       ISTMN    !- User start time in century mins
      INTEGER       ITIME(9) !- User request times array
      INTEGER       ITOD     !- User request in TODAY format
      INTEGER       J        !- General loop counter

      LOGICAL       FOUND(*) !- Array of user-selected MDB keywords

      CHARACTER*(*) CERR     !- Error message to return to calling prog.
      CHARACTER*30  CMSG     !- Error message returned by DATCHK
      CHARACTER*132 HEAD     !- Revision information

      EXTERNAL      DT2HRS   !- Convert date/time to century hours

!-----------------------------------------------------------------------
! Revision information
!------------------------------------------------------------------------

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/valdat.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:25:57$ '

!-----------------------------------------------------------------------
! Calculate current time in century minutes
!-----------------------------------------------------------------------

      CNTMN = DT2HRS(ICT(8),ICT(7),ICT(6),ICT(5))*60 + ICT(4)

!-----------------------------------------------------------------------
! User has coded START TIME
!-----------------------------------------------------------------------

      IF (FOUND(1)) THEN

!-----------------------------------------------------------------------
! Check start time is valid.
!-----------------------------------------------------------------------

        CALL DATCHK(ITIME(2),ITIME(3),ITIME(4),IFAIL,CMSG)          !2.0
        IF (IFAIL.EQ.8) THEN
          CERR='INVALID START '//CMSG
          GOTO 999
        ENDIF

        ISTMN = DT2HRS(ITIME(1),ITIME(2),ITIME(3),(ITIME(4)/100))*60 +
     &          MOD(ITIME(4),100)

!-----------------------------------------------------------------------
! User has coded START TIME and END TIME
!-----------------------------------------------------------------------

        IF (FOUND(2)) THEN
          CALL DATCHK(ITIME(2),ITIME(3),ITIME(4),IFAIL,CERR)        !2.0
          IF (IFAIL.EQ.8) THEN
            CERR='INVALID END '//CMSG
            GOTO 999
          ENDIF

          INDMN = DT2HRS(ITIME(5),ITIME(6),ITIME(7),(ITIME(8)/100))*60 +
     &            MOD(ITIME(8),100)

!-----------------------------------------------------------------------
! Check start is before end. Added minutes to check
!-----------------------------------------------------------------------

          IF (ISTMN.GT.INDMN) THEN
            CERR='START DATE IS AFTER END DATE'
            IFAIL=8
            GOTO 999
          ENDIF

!-----------------------------------------------------------------------
! check end date against todays date: if user requests date/time
! in today format but the job runs just after midnight by mistake
! change today to TODAY-1 and print a warning
!-----------------------------------------------------------------------

          IF (ITOD.EQ.1) THEN
            IF (ISTMN.GT.CNTMN) THEN
              ISTMN = ISTMN - 1440       !- 24 hours * 60 minutes
              INDMN = INDMN - 1440       !- 24 hours * 60 minutes

              CALL HRS2DT(ITIME(1),ITIME(2),ITIME(3),ITIME(4),ISTMN/60)
              ITIME(4) = ITIME(4) * 100 + MOD(ISTMN,60)

              CALL HRS2DT(ITIME(5),ITIME(6),ITIME(7),ITIME(8),INDMN/60)
              ITIME(8) = ITIME(8) * 100 + MOD(INDMN,60)

              CERR='TODAY CHANGED TO YESTERDAY'
              IFAIL=4
            ENDIF
          ENDIF
        ELSE

!-----------------------------------------------------------------------
! User has coded START TIME, but not END TIME
! set end time to current time + 59 minutes, unless start time is
! greater than current time, in which case, set end time to start time
! + 59 minutes.
!-----------------------------------------------------------------------

          IF (ISTMN.LT.CNTMN) THEN
            INDMN = CNTMN + 59
          ELSE
            INDMN = ISTMN + 59
          ENDIF

          CALL HRS2DT(ITIME(5),ITIME(6),ITIME(7),ITIME(8),INDMN/60)
          ITIME(8) = ITIME(8) * 100 + MOD(INDMN,60)

        ENDIF
      ELSE

!-----------------------------------------------------------------------
! User has not coded START TIME.
! Set start time to current time
!-----------------------------------------------------------------------

        ISTMN = CNTMN
        DO J=1,4
          ITIME(J)=ICT(9-J)
        ENDDO
        ITIME(4) = ITIME(4) * 100 + ICT(4)

        IF (FOUND(2)) THEN
          CERR=' CANNOT HAVE END TIME WITHOUT START TIME'
          IFAIL=8
          GOTO 999
        ELSE

!-----------------------------------------------------------------------
! User has not coded START TIME or END TIME.
! Set end time to start time + 59 minutes
!-----------------------------------------------------------------------

          INDMN = ISTMN + 59
          CALL HRS2DT(ITIME(5),ITIME(6),ITIME(7),ITIME(8),INDMN/60)
          ITIME(8) = ITIME(8) * 100 + MOD(INDMN,60)

        ENDIF
      ENDIF

999   CONTINUE
      RETURN
      END
