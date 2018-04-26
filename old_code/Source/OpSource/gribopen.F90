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
!                               8 = FATAL ERROR BECAUSE:
!                                  - I/O ERROR IN 'OPEN' STATEMENT.
!                              16 = FATAL ERROR BECAUSE:
!                                  - MASS RESTORE FAILED.
!
! CALLED BY   : GRIBFIND
!
! CALLS       : MDBRSN (RESTORES DATASET FROM MASS)
!
! REVISION INFO :
!
! $Workfile: gribopen.F90$ $Folder: OpSource$
! $Revision: 8$ $Date: 21/03/2012 11:17:13$
!
! CHANGE RECORD :
!
! $Log:
!  8    MetDB_Refresh 1.7         21/03/2012 11:17:13    Sheila Needham  Return
!        ISTAT=16 instead of 8 for MASS restore failures
!  7    MetDB_Refresh 1.6         26/11/2010 09:12:04    Sheila Needham  Update
!        OPEN stmt
!  6    MetDB_Refresh 1.5         22/11/2010 14:11:07    Stan Kellett    added
!       use inquire
!  5    MetDB_Refresh 1.4         18/11/2010 11:45:43    John Norton     After
!       doing merge batch 9 changes.
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:17:01    John Norton     
!  1    MetDB_Refresh 1.0         13/10/2010 16:00:38    John Norton     f77
!       code copied out of StarTeam
!       MIDE\Met_DB_Project\Development\Source\OpSource
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE mdbrsn_mod
USE inquire_mod

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

INTEGER, INTENT(IN)             ::  NYMDH(4) ! YEAR, MONTH, DAY, HOUR FOR DATA SET NAME
CHARACTER(*), INTENT(IN)        ::  SKEL ! SKELETON DATA SET NAME
LOGICAL, INTENT(IN)             ::  LTEST ! .TRUE. IF DIAGNOSTICS WANTED
INTEGER, INTENT(OUT)            ::  IUNIT ! DATA SET UNIT NUMBER
INTEGER, INTENT(OUT)            ::  LENREC ! DATA SET RECORD LENGTH
INTEGER, INTENT(OUT)            ::  KODE ! RETURN CODE FROM MDBRSN

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!                                                             VARIABLES
INTEGER      ::  I    ! GENERAL LOOP COUNTER
INTEGER      ::  IPT  ! GENERAL POINTER
INTEGER      ::  IOS  ! ERROR STATUS FROM OPEN STATEMENT
!
LOGICAL      ::  ONDISK ! 'EXIST' FLAG FOR INQUIRE STATEMENTS
!
CHARACTER(44) ::  DSNOFF ! NAME OF OFF-LINE DATA SET TO BE RESTORED
CHARACTER(44) ::  DSN ! DATA SET TO OPEN (RESTORED IF NECESSARY)
CHARACTER(2)  ::  YMDH(4) ! DATE/TIME INDICATORS IN SKELETON NAME
!

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------
!                                                  DATA INITIALISATIONS
DATA YMDH /'YY','MM','DD','HH'/
!                                    SAVE VARIABLES USED BY LATER CALLS
SAVE YMDH
!
!
!-----------------------------------------------------------------------
!  CONSTRUCT NAME OF REQUIRED DATA SET FROM THE SKELETON.
!-----------------------------------------------------------------------
!
!                    REPLACE YY, MM, DD, HH WITH YEAR, MONTH, DAY, HOUR
DSN = SKEL
DO I=1,4
   IPT = INDEX(DSN,YMDH(I))
   IF (IPT > 0) WRITE (DSN(IPT:IPT+1),'(I2.2)') NYMDH(I)
END DO ! I
!                                            SEE IF DATA SET IS ON DISK
ONDISK = INQUIRE(DSN,'DSN')
!
IFLABEL1: &
IF (ONDISK) THEN
   IF (LTEST) WRITE (6,'(T5,A,T15,2A)') 'GRIBOPEN:', &
              'DATA SET ALREADY ON DISK - ', DSN
!
!-----------------------------------------------------------------------
!  IF NOT FOUND, CHECK WHETHER IT HAS BEEN PREVIOUSLY RESTORED.
!  (IN THIS CASE, DATA SET NAME IS PREFIXED BY "PUBLIC.")
!-----------------------------------------------------------------------
!
ELSE
   DSNOFF = DSN
   DSN = 'PUBLIC.' // DSNOFF
   ONDISK = INQUIRE(DSN,'DSN')
!
IFLABEL2: &
   IF (ONDISK) THEN
      IF (LTEST) WRITE (6,'(T5,A,T15,2A)') 'GRIBOPEN:', &
                 'DATA SET ALREADY RESTORED TO DISK - ', DSN
!
!-----------------------------------------------------------------------
!  IF STILL NOT FOUND, RESTORE THE DATA SET FROM MASS USING MDBRSN.
!-----------------------------------------------------------------------
!
   ELSE
#if defined (MVS)
      IF (LTEST) WRITE (6,'(T5,A,T15,2A)') 'GRIBOPEN:', &
                 'ABOUT TO RESTORE DATA SET ', DSN

!-----------------------------------------------------------------------
! Call MDBRSN to restore data from MASS.
!
! Note: INQUIRE IS USED BECAUSE 'MDBRSN' SEEMS TO GIVE A RETURN
! CODE 0 EVEN IF THE DATA SET DOESN'T EXIST!)
!-----------------------------------------------------------------------

      CALL MDBRSN (DSNOFF, DSN, 'E10', KODE)

      ONDISK = INQUIRE(DSN,'DSN')
!                                                  CHECK RESTORE STATUS
      IF (.NOT.ONDISK) THEN
         WRITE (6,'(T5,A,T15,2A)') 'GRIBOPEN:', &
                  'OFFLINE RESTORE FAILED FOR ', DSN
         KODE = 16
         RETURN
      END IF
#else
      WRITE(6,*)'MDB ERROR: GRIBOPEN - NO OFFLINE SYSTEM TO'
      WRITE(6,*)'RETSORE OLD DATA. EXIT RETRIEVAL'
      KODE = 8
      RETURN
#endif
   END IF IFLABEL2
END IF IFLABEL1
!
!-----------------------------------------------------------------------
!  DATA SET IS NOW AVAILABLE ON DISK, SO OPEN IT ON UNIT 85.
!-----------------------------------------------------------------------
!
IUNIT = 85
LENREC = 9216
#if defined (MVS)
   OPEN (IUNIT, FILE="//'"//DSN//"'", ACTION='READ', IOSTAT=IOS, &
            ACCESS='DIRECT',RECL=LENREC)

#else
OPEN (IUNIT, FILE='/'//DSN, ACCESS='SEQUENTIAL', &
      FORM='FORMATTED', IOSTAT=IOS)
#endif
!                                                 CHECK I/O STATUS CODE
IF (IOS /= 0) THEN
   WRITE (6,'(T5,A,T15,2A)') 'GRIBOPEN:', &
            'I/O ERROR OPENING DATA SET ', DSN
   KODE = 8
   RETURN
END IF
!
KODE = 4
IF (LTEST) WRITE (6,'(T5,A,T15,2A)') 'GRIBOPEN:', &
                    ' DATA SET OPENED - DSN = ', DSN
!
!-----------------------------------------------------------------------
!  DATA SET IS READY FOR READING: RETURN TO CALLING PROGRAM
!-----------------------------------------------------------------------
!
RETURN
END SUBROUTINE GRIBOPEN
