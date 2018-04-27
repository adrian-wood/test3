SUBROUTINE SUMMARY (NFILES, NMSGS, KOUNTS, BULTYP, NTYPES)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SUMMARY
!
! PURPOSE       : TO PRINT OUT A TABLE IN A STORAGE JOB SUMMARISING
!                 THE NUMBER OF EACH TYPE OF MESSAGE PROCESSED AND
!                 THE NUMBERS STORED AND REJECTED.
!
! CALLED BY     : ANY STORAGE JOB READING GTS DATA FTP'D FROM TROPICS.
!
! USAGE         : CALL SUMMARY (NFILES, NMSGS, KOUNTS, BULTYP, NTYPES)
!
! ARGUMENTS    : 
!                 NFILES:  NUMBER OF DATA SETS PROCESSED.
!                 NMSGS:   TOTAL NUMBER OF MESSAGES PROCESSED.
!                 KOUNTS:  NUMBERS OF MESSAGES PROCESSED, STORED AND
!                          REJECTED FOR EACH DATA TYPE (SEE BELOW).
!                 BULTYP:  CHARACTER*8 ARRAY OF BULLETIN TYPE CODES.
!                 NTYPES:  TOTAL NUMBER OF BULLETIN TYPE CODES.
!
!                 ('KOUNTS' IS DIMENSIONED '(3,0:NTYPES)'. THE '3'
!                 REFERS TO MESSAGES (1) PROCESSED, (2) STORED AND (3)
!                 REJECTED. THE SECOND DIMENSION REFERS TO THE DATA
!                 TYPE WITH '0' BEING THE SUM OVER ALL TYPES.)
!
! REVISION INFO:
!
! $Workfile: summary.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 22/12/2010 12:06:33$
!
! CHANGE RECORD:
! $Log:
!  3    MetDB_Refresh 1.2         22/12/2010 12:06:33    Sheila Needham
!       Updated following review
!  2    MetDB_Refresh 1.1         08/12/2010 09:15:26    Richard Weedon  intent
!        added to var declarations
!  1    MetDB_Refresh 1.0         07/12/2010 17:09:07    Richard Weedon
!       Initial Draft
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
IMPLICIT NONE
!
! Arguments

INTEGER,         INTENT(IN)    :: NTYPES             ! TOTAL NUMBER OF DATA TYPES
INTEGER,         INTENT(IN)    :: NFILES             ! TOT NUM OF MESS PROCESSED
INTEGER,         INTENT(IN)    :: NMSGS              ! TOT NUM OF MESS PROCESSED
INTEGER,         INTENT(INOUT) :: KOUNTS(3,0:NTYPES) ! NUM OF MESS PROCESSED/STORED
CHARACTER(LEN=8),INTENT(IN)    :: BULTYP(NTYPES)     ! BULLETIN TYPE CODES

! Local Variables

INTEGER   :: I             ! COUNTER IN IMPLICIT DO LOOPS
INTEGER   :: JTYPE         ! DATA TYPE (LOOP VARIABLE)
!
!      HEADINGS FOR SUMMARY TABLE
WRITE (6,'(// 2(T3,A/), 2(/T14,A))')                 &
              'SUMMARY OF MESSAGES PROCESSED',      &
              '-----------------------------',      &
              'DATA TYPE     READ   STORED  REJECT',&
              '---------    ------  ------  ------'
!      UNKNOWN DATA TYPES
IF (KOUNTS(1,0) > 0)&
   WRITE (6,'(T15,A,2X,3I8)') 'UNKNOWN ',  (KOUNTS(I,0),I=1,3)
!
!     OTHER DATA TYPES
DO JTYPE=1,NTYPES
   IF (KOUNTS(1,JTYPE) > 0) THEN
      WRITE (6,'(T15,A,2X,3I8)')&
      BULTYP(JTYPE), (KOUNTS(I,JTYPE),I=1,3)
      KOUNTS(1,0) = KOUNTS(1,0) + KOUNTS(1,JTYPE)
      KOUNTS(2,0) = KOUNTS(2,0) + KOUNTS(2,JTYPE)
      KOUNTS(3,0) = KOUNTS(3,0) + KOUNTS(3,JTYPE)
   END IF
END DO ! JTYPE
!                                                        MESSAGE TOTALS
WRITE (6,'(T14,A / T15,A,3I8 / T14,A)')               &
              '---------    ------  ------  ------', &
              'TOTALS:   ',  (KOUNTS(I,0),I=1,3),   &
              '---------    ------  ------  ------'
WRITE (6,'(/T2,I15,A,I6,A)') NMSGS, ' MESSAGES PROCESSED FROM',&
       NFILES, ' DATA SETS.'
!  END OF PROCESSING
RETURN
END SUBROUTINE SUMMARY
