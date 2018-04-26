SUBROUTINE MDBLSEQ(CSUBT,MATCH)

!-----------------------------------------------------------------------
!
! ROUTINE       : MDBLSEQ
!
! PURPOSE       : To match a subtype in file MDBLSEQ, and put the
!               : sequence into memory by calling LOCALD.
!
! ARGUMENTS     :(1) CSUBT   (IN) : data subtype
!                (2) MATCH   (OUT): true if it's found
!
! CALLS         : LOCALD, METDB_GETENV
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 25/10/2011 11:34:59$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mdblseq.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         25/10/2011 11:34:59    Sheila Needham
!       Initial port to F90
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE locald_mod

IMPLICIT NONE

! Arguments

CHARACTER(*),INTENT(IN) ::  CSUBT
LOGICAL,INTENT(OUT)     ::  MATCH

! Local Variables

INTEGER        :: DUMMY(1)
INTEGER        :: IREC
INTEGER        :: LEV            !- Length of METDB_MDBLSEQ
INTEGER        :: RC             !- Return code
LOGICAL        :: FEXIST         !- TRUE if file exists
INTEGER        :: ZERO           !- Dummy var for LOCALD
CHARACTER(LEN=8)     :: SUBTYPE
CHARACTER(LEN=27998) :: MDBLOCD
CHARACTER(LEN=200)   :: METDB_MDBLSEQ  !- MDBLSEQ PATH

SAVE                                                          !2.0

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

IREC=0
SUBTYPE=' '
MATCH=.FALSE.

!-----------------------------------------------------------------------
! get environment variable METDB_MDBLSEQ and find the length of it.
!-----------------------------------------------------------------------

CALL METDB_GETENV("METDB_MDBLSEQ",METDB_MDBLSEQ,RC)
IF (RC /= 0) THEN
  WRITE(6,*)'MDBLSEQ: ERROR: ENV VAR METDB_MDBLSEQ not set'
  RETURN
END IF
LEV=LEN(METDB_MDBLSEQ)
DO WHILE (METDB_MDBLSEQ(LEV:LEV) == ' ')
  LEV=LEV-1
END DO

!-----------------------------------------------------------------------
! inquire to see if MDBLSEQ dataset exists
!-----------------------------------------------------------------------

INQUIRE (FILE=METDB_MDBLSEQ(1:LEV),EXIST=FEXIST)
IF (.NOT.FEXIST) THEN
  WRITE(6,*)'MDBLSEQ: ERROR: File ',METDB_MDBLSEQ(1:LEV),   &
            ' not found'
  RETURN
END IF

!-----------------------------------------------------------------------
! open the MDBLSEQ dataset
!-----------------------------------------------------------------------

OPEN(81,FILE=METDB_MDBLSEQ(1:LEV),FORM='FORMATTED',           &
        ACCESS='DIRECT',RECL=30000,IOSTAT=RC)

IF (RC /= 0) THEN
  WRITE(6,*)'MDBLSEQ: ERROR: Could not open file ',       &
             METDB_MDBLSEQ(1:LEV)
  RETURN
END IF

!-----------------------------------------------------------------------
! Loop over records in MDBLSEQ dataset until there are no more
! subtypes to read.
!-----------------------------------------------------------------------

DO WHILE(SUBTYPE /= 'LIST_END')

  IREC=IREC+1
  READ(81,'(A8,1X,A)',REC=IREC)SUBTYPE,MDBLOCD

!-----------------------------------------------------------------------
! If subtype matched then...
! If MATCH is TRUE then this is NOT the first sequence for this subtype
! so ADD it to LOCALD. Else MATCH is FALSE so this is the first
! sequence for this subtype so Call LOCALD with a mode of 'NEW' to
! start afresh.
!-----------------------------------------------------------------------

  IF (SUBTYPE == CSUBT) THEN
    IF (MATCH) THEN
      CALL LOCALD(0,0,DUMMY,ZERO,MDBLOCD,'ADD')
    ELSE
      CALL LOCALD(0,0,DUMMY,ZERO,MDBLOCD,'NEW')
    END IF
    MATCH=.TRUE.
  END IF
END DO

CLOSE(81)

RETURN
END SUBROUTINE MDBLSEQ
