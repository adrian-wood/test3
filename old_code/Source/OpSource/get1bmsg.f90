SUBROUTINE GET1BMSG(CREP,MDB_NOBS,MDB_NELEM,MDB_ARRAY, &
                    SBUFR,LSBUFR,STATUS)
!-----------------------------------------------------------------------
!
! SUBROUTINE    : GET1BMSG
!
! DESCRIPTION   : Routine to extract 1 BUFR message from a character
!                 array containing many.
!
! CALLED BY     : users program
!
! CALLS         : nothing
!
! ARGUMENTS        :
!
! (1) CREP        : IN    : Array of BUFR messages
! (2) MDBBD_NOBS  : IN    : Nobs dimension
! (3) MDBBD_NELEM : IN    : Nelem dimension
! (4) MDBBD_ARRAY : IN    : Array of MDB values
! (5) SBUFR       : OUT   : BUFR message
! (6) LSBUFR      : OUT   : Length of BUFR message
! (7) STATUS      : INOUT : Status IN 4 - continuation
!                                  OUT 16 - error
!                                       4 - more data to come
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/10/2011 11:43:25$
! $Source: /home/us0400/mdb/op/lib/source/RCS/get1bmsg.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/10/2011 11:43:25    Sheila Needham
!       Initial port
! $
! Revision 1.6  2002/10/22 10:09:22  usmdb
! Corrected adding up of section lengths - S.Cox
!
! Revision 1.5  2002/07/17 15:11:52  usmdb
! Added a check for MDB_NOBS=0 - S.Cox
!
! Revision 1.4  2002/07/09 15:04:41  usmdb
! Improvements - S.Cox
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

IMPLICIT NONE

! Arguments

INTEGER,INTENT(IN)      :: MDB_NOBS
CHARACTER(*),INTENT(IN) :: CREP(MDB_NOBS)
INTEGER,INTENT(IN)      :: MDB_NELEM
REAL,INTENT(IN)         :: MDB_ARRAY(MDB_NOBS,MDB_NELEM)
CHARACTER(*),INTENT(INOUT) :: SBUFR
INTEGER,INTENT(INOUT)   :: LSBUFR
INTEGER,INTENT(INOUT)   :: STATUS

! Parameters

INTEGER,PARAMETER :: LENREP = 28672  ! =28K
INTEGER,PARAMETER :: LENSTR =2*LENREP

! Local Variables

INTEGER               :: IBUFR
INTEGER               :: ICHAR3
INTEGER               :: IREP
INTEGER               :: J
INTEGER               :: M
INTEGER               :: MSTART
INTEGER               :: MEND
INTEGER               :: MLEN
INTEGER               :: NSECT
CHARACTER(LEN=LENSTR) :: CSTR

SAVE

!-----------------------------------------------------------------------
! first check if MDB_NOBS < 1
!-----------------------------------------------------------------------

IF (MDB_NOBS < 1) THEN
  WRITE(6,*)'GET1BMSG: ERROR: MDB_NOBS < 1'
  STATUS=16
  RETURN
ENDIF

!-----------------------------------------------------------------------
! initialise variables if STATUS=0 (New CREP) and check that CREP is
! of size LENREP
!-----------------------------------------------------------------------

IF (STATUS == 0) THEN

  CSTR=' '
  MEND=0
  MSTART=1

  IF (LEN(CREP(1)) /= LENREP) THEN
    STATUS=16
    WRITE(6,*)'GET1BMSG: ERROR: CREP  /=  ',LENREP
    RETURN
  ENDIF

!-----------------------------------------------------------------------
! Fill CSTR initially, and look for 'BUFR' (start of message)
!-----------------------------------------------------------------------

  IF (MDB_ARRAY(1,1) <= 0.0) THEN
    STATUS=16
    RETURN
  ENDIF

  IREP=1
  CSTR(1:LENREP)=CREP(IREP)

  IF (MDB_NOBS > 1) THEN
    IREP=2
    IF (MDB_ARRAY(2,1) > 0.0) CSTR(LENREP+1:)=CREP(IREP)
  ENDIF

  M=INDEX(CSTR(MSTART:),'BUFR')

ENDIF !- STATUS == 0

!-----------------------------------------------------------------------
! If 'BUFR' found, continue.
!-----------------------------------------------------------------------

IF (M > 0) THEN

!-----------------------------------------------------------------------
! Get BUFR edition number
!-----------------------------------------------------------------------

  MSTART=MSTART+M-1
  IBUFR=ICHAR(CSTR(MSTART+7:MSTART+7))

!-----------------------------------------------------------------------
! (1) BUFR edition <2: Add section lengths
!-----------------------------------------------------------------------

  IF (IBUFR < 2) THEN
    MEND=MSTART+3
    NSECT=3
    IF (ICHAR(CSTR(MSTART+11:MSTART+11)) > 127) NSECT = 4
    DO J=1,NSECT
      MEND=MEND+ICHAR3(CSTR(MEND+1:MEND+3))
    ENDDO ! J
    MEND=MEND+4  ! 4 bytes for section 4
    MLEN=MEND-MSTART+1

!-----------------------------------------------------------------------
! (2) BUFR edition >1: Get length from section 0
!-----------------------------------------------------------------------

  ELSE
    MLEN=ICHAR3(CSTR(MSTART+4:MSTART+6))
    MEND=MSTART+MLEN-1
  ENDIF

!-----------------------------------------------------------------------
! Put BUFR message in SBUFR
!-----------------------------------------------------------------------

  SBUFR  = CSTR(MSTART:MEND)
  LSBUFR = MLEN
  STATUS=4      !- bufr message found

  MSTART=MEND+1

!-----------------------------------------------------------------------
! We now have a BUFR message - but if pointer is now to second LENREP
! string, move that string up and put more data from the CREP array
! into the second string. This should keep BUFR messages within the
! first string of length LENREP.
!-----------------------------------------------------------------------

  IF (MSTART > LENREP) THEN
    CSTR(1:LENREP)=CSTR(LENREP+1:LENSTR)
    CSTR(LENREP+1:)=' '
    IREP=IREP+1
    IF (IREP <= MDB_NOBS) CSTR(LENREP+1:)=CREP(IREP)
    MSTART=MSTART-LENREP
  ENDIF

  M=INDEX(CSTR(MSTART:),'BUFR')   !- Look for 'BUFR' again.

ELSE
  STATUS=0   !- no bufr message found. End of data.
ENDIF !- M > 0

RETURN
END


