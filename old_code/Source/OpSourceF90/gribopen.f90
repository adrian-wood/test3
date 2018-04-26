SUBROUTINE GRIBOPEN (NYMDH, SKEL, LTEST, IUNIT, LENREC, KODE)

!----------------------------------------------------------------------
!
! SUBROUTINE  : GRIBOPEN
!
! PURPOSE     : ALLOCATES AND OPENS GRIB DATASET, RESTORING FROM TAPE
!               IF NECESSARY.
!
! DESCRIPTION : 'GRIBOPEN' MAKES UP A DATA SET NAME FROM THE SUPPLIED
!               SKELETON NAME AND DATE/HOUR DETAILS AND LOOKS FOR THE
!               DATA SET. IF NOT FOUND ON DISK IT LOOKS FOR A VERSION
!               RESTORED FROM TAPE (WITH A 'PUBLIC...' NAME), AND IF
!               STILL NOT FOUND WILL ARRANGE FOR A RESTORE JOB TO BE
!               SUBMITTED.
!
!               ONCE THE DATA SET HAS BEEN LOCATED OR SET UP ON DISK,
!               IT IS OPENED AND THE UNIT NUMBER AND RECORD LENGTH
!               ARE RETURNED TO THE CALLING ROUTINE.
!
! USAGE       : CALL GRIBOPEN (NYMDH, SKEL, LTEST, IUNIT, LENREC, KODE)
!
! PARAMETERS  : NYMDH(4) (I)  (C*2) YEAR, MONTH, DAY & HOUR FOR DSN.
!               SKEL     (I)  (C*(*)) SKELETON DATA SET NAME.
!               LTEST    (I)  LOGICAL FLAG FOR PRINTED DIAGNOSTICS.
!               IUNIT    (O)  UNIT NUMBER FOR GRIB DATA SET (=85).
!               LENREC   (O)  RECORD LENGTH OF GRIB DATA SET.
!               KODE     (O)  RETURN CODE - VALUES AS FOLLOWS:
!                               4 = DATA SET SUCCESSFULLY OPENED,
!                               8 = FATAL ERROR BECAUSE EITHER:
!                                  - OFFLINE RESTORE FAILED, OR
!                                  - I/O ERROR IN 'OPEN' STATEMENT.
!
! CALLED BY   : GRIBFIND
!
! CALLS       : MDBRSN (RESTORES DATASET FROM MASS - LIVES ON
!                       MET.PROGLIB AND CALLS
!                       SYS1.APPC.METEXEC(MDBRESTN))                !2.3
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /data/us0400/mdb/op/lib/source/RCS/gribopen.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.3  2003/03/06  09:06:31  09:06:31  usmdb (MetDB account c/o usjh)
! Removed call to MDBRST. All offline retrievals now from MASS
! using MDBRSN - S.Cox
!
! Revision 2.2  2003/02/03  16:03:34  16:03:34  usmdb (MetDB account c/o usjh)
! Pass the MASS stream as argument 3 to MDBRSN - S.Cox.
!
! Revision 2.1  2002/07/01  13:57:09  13:57:09  usmdb (MetDB account c/o usjh)
! Code added to restore off-line data from MASS if the dataset
! date .GE. 1 July 2002 - S.Cox
!
! Revision 2.0  2001/01/08  11:58:43  11:58:43  usmdb (Generic MetDB account)
! Added pre-processor statements around MDBRST call.
! If not 1 , issue an error as there is no off-line restore
! system on non-1  platforms yet .Added copyright and
! modified header - S.Cox
!
! Revision 1.2  2000/12/08  14:36:20  14:36:20  usmdb (Generic MDB account)
! pre-processor statements added around OPEN statement - S.Cox
!
! Revision 1.1  2000/12/08  14:16:54  14:16:54  usmdb (Generic MDB account)
! Initial revision
!
! ORIGINAL VERSION BASED PARTLY ON MDBALC, BRB 01/08/00.
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
!                      VARIABLES
INTEGER I             ! GENERAL LOOP COUNTER
INTEGER IPT           ! GENERAL POINTER
INTEGER IOS           ! ERROR STATUS FROM OPEN STATEMENT
INTEGER IUNIT         ! DATA SET UNIT NUMBER
INTEGER KODE          ! RETURN CODE FROM MDBRSN
INTEGER LENREC        ! DATA SET RECORD LENGTH
INTEGER NYMDH(4)      ! YEAR, MONTH, DAY, HOUR FOR DATA SET NAME
!
LOGICAL FIRST         ! .TRUE. FOR FIRST CALL TO SUBROUTINE
LOGICAL LTEST         ! .TRUE. IF DIAGNOSTICS WANTED
LOGICAL ONDISK        ! 'EXIST' FLAG FOR INQUIRE STATEMENTS
!
CHARACTER*44  DSNOFF  ! NAME OF OFF-LINE DATA SET TO BE RESTORED
CHARACTER*44  DSN     ! DATA SET TO OPEN (RESTORED IF NECESSARY)
CHARACTER*(*) SKEL    ! SKELETON DATA SET NAME
CHARACTER*132 HEAD    ! REVISION INFORMATION
CHARACTER*2   YMDH(4) ! DATE/TIME INDICATORS IN SKELETON NAME
!
!                                            DATA INITIALISATIONS
DATA FIRST /.TRUE./
DATA YMDH /'YY','MM','DD','HH'/
!                              SAVE VARIABLES USED BY LATER CALLS
SAVE FIRST, YMDH
!
!-----------------------------------------------------------------------
!  REVISION INFORMATION (FIRST CALL TO SUBROUTINE ONLY)
!-----------------------------------------------------------------------
!
IF (FIRST) THEN
   HEAD='&
   &$Source: /data/us0400/mdb/op/lib/source/RCS/gribopen.F,v $&
   &'//'$Date: 26/01/2010 10:18:13$ $Revision: 1$'
   FIRST = .FALSE.
END IF
!
!-----------------------------------------------------------------------
!  CONSTRUCT NAME OF REQUIRED DATA SET FROM THE SKELETON.
!-----------------------------------------------------------------------
!
! REPLACE YY, MM, DD, HH WITH YEAR, MONTH, DAY, HOUR
DSN = SKEL
DO I=1,4
   IPT = INDEX(DSN,YMDH(I))
   IF (IPT.GT.0) WRITE (DSN(IPT:IPT+1),'(I2.2)') NYMDH(I)
END DO ! I
!                                      SEE IF DATA SET IS ON DISK
INQUIRE (FILE='/'//DSN, EXIST=ONDISK)

IF (ONDISK) THEN
   IF (LTEST) WRITE (6,'(T5,A,T15,2A)') 'GRIBOPEN:',&
             &'DATA SET ALREADY ON DISK - ', DSN

!-----------------------------------------------------------------------
!  IF NOT FOUND, CHECK WHETHER IT HAS BEEN PREVIOUSLY RESTORED.
!  (IN THIS CASE, DATA SET NAME IS PREFIXED BY "PUBLIC.")
!-----------------------------------------------------------------------
!
ELSE
   DSNOFF = DSN
   DSN = 'PUBLIC.' // DSNOFF
   INQUIRE (FILE='/'//DSN, EXIST=ONDISK)

   IF (ONDISK) THEN
      IF (LTEST) WRITE (6,'(T5,A,T15,2A)') 'GRIBOPEN:',&
                &'DATA SET ALREADY RESTORED TO DISK - ', DSN
!
!-----------------------------------------------------------------------
!  IF STILL NOT FOUND, RESTORE THE DATA SET FROM MASS USING MDBRSN.
!-----------------------------------------------------------------------
!
   ELSE
!if defined (MVS)
     IF (LTEST) WRITE (6,'(T5,A,T15,2A)') 'GRIBOPEN:',&
     &'ABOUT TO RESTORE DATA SET ', DSN

!-----------------------------------------------------------------------
! Call MDBRSN to restore data from MASS.                            !2.3
!
! Note: INQUIRE IS USED BECAUSE 'MDBRSN' SEEMS TO GIVE A RETURN
! CODE 0 EVEN IF THE DATA SET DOESN'T EXIST!)                       !2.1
!-----------------------------------------------------------------------

      CALL MDBRSN (DSNOFF, DSN, 'E10', KODE)                  !2.3

      INQUIRE (FILE='/'//DSN, EXIST=ONDISK)
!                                            CHECK RESTORE STATUS
      IF (.NOT.ONDISK) THEN
         WRITE (6,'(T5,A,T15,2A)') 'GRIBOPEN:',&
                 &'OFFLINE RESTORE FAILED FOR ', DSN
         KODE = 8
         RETURN
      END IF
!else
!endif
         END IF
      ENDIF
!
!-----------------------------------------------------------------------
!  DATA SET IS NOW AVAILABLE ON DISK, SO OPEN IT ON UNIT 85.
!-----------------------------------------------------------------------
!
IUNIT = 85
!if defined (MVS)
OPEN (IUNIT, FILE='/'//DSN, ACTION='READ', ACCESS='SEQUENTIAL',&
     &FORM='FORMATTED', IOSTAT=IOS)
!else
!endif
!                                                 CHECK I/O STATUS CODE
IF (IOS.NE.0) THEN
   WRITE (6,'(T5,A,T15,2A)') 'GRIBOPEN:',&
   &'I/O ERROR OPENING DATA SET ', DSN
   KODE = 8
   RETURN
END IF
!                                            GET DATA SET RECORD LENGTH
INQUIRE (FILE='/'//DSN, RECL=LENREC)
!
KODE = 4
IF (LTEST) WRITE (6,'(T5,A,T15,2A)') 'GRIBOPEN:',&
 &' DATA SET OPENED - DSN = ', DSN
!
!-----------------------------------------------------------------------
!  DATA SET IS READY FOR READING: RETURN TO CALLING PROGRAM
!-----------------------------------------------------------------------
!
RETURN
END SUBROUTINE GRIBOPEN
