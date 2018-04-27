SUBROUTINE COUNTBMSG(CREP,MDB_NOBS,MDB_NELEM,MDB_ARRAY,NBUFR)

!-----------------------------------------------------------------------
!
! ROUTINE       : COUNTBMSG
!
! DESCRIPTION   : Routine to count the number of whole BUFR messages
!               : in the CREP character array.
!
! CALLED BY     : users program
!
! CALLS         : nothing
!
! ARGUMENTS      :
!
!  (1) CREP      : IN    : Array containing BUFR messages
!  (2) MDB_NOBS  : IN    : Dimension of CREP
!  (3) MDB_NELEM : IN    : NELEM dimension of values array
!  (4) MDB_ARRAY : IN    : MDB values array
!  (5) NBUFR     : OUT   : Number of whole BUFR messages
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/10/2011 11:43:04$
! $Source: /home/us0400/mdb/op/lib/source/RCS/countbmsg.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/10/2011 11:43:04    Sheila Needham
!       Initial port
! $
! Revision 1.3  2002/10/22 10:09:22  usmdb
! Corrected adding up of section lengths - S.Cox
!
! Revision 1.2  2002/07/11 15:58:07  usmdb
! Added check for MDB_NOBS=0 - S.Cox
!
! Revision 1.1  2002/07/09 15:05:12  usmdb
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Arguments

INTEGER,INTENT(IN)      ::  MDB_NOBS
CHARACTER(*),INTENT(IN) ::  CREP(MDB_NOBS)
INTEGER,INTENT(IN)      ::  MDB_NELEM
REAL,INTENT(IN)         ::  MDB_ARRAY(MDB_NOBS,MDB_NELEM)
INTEGER,INTENT(INOUT)   ::  NBUFR

! Parameters

INTEGER,PARAMETER :: LENREP =28672  ! = 28K
INTEGER,PARAMETER :: LENSTR =2*LENREP

! Local variables

CHARACTER(LEN=LENSTR)  :: CSTR
INTEGER                :: IBUFR
INTEGER                :: ICHAR3
INTEGER                :: IREP
INTEGER                :: J
INTEGER                :: M
INTEGER                :: MEND
INTEGER                :: MLEN
INTEGER                :: MSTART
INTEGER                :: NSECT

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

NBUFR=0
CSTR=' '
MEND=0
IREP=2
MSTART=1

!-----------------------------------------------------------------------
! first check if MDB_NOBS < 1
!-----------------------------------------------------------------------

IF (MDB_NOBS < 1) THEN
  WRITE(6,*)'COUNTBMSG: ERROR: MDB_NOBS < 1'
  RETURN
END IF

!-----------------------------------------------------------------------
! then check that CREP is of size LENREP
!-----------------------------------------------------------------------

IF (LEN(CREP(1)) /= LENREP) THEN
  WRITE(6,*)'COUNTBMSG: ERROR: CREP  /=  ',LENREP
  RETURN
END IF

!-----------------------------------------------------------------------
! Fill CSTR initially, and look for 'BUFR' (start of message)
!-----------------------------------------------------------------------

IF (MDB_ARRAY(1,1) <= 0.0) RETURN

CSTR(1:LENREP)=CREP(1)

IF (MDB_NOBS > 1) THEN
  IF (MDB_ARRAY(2,1) > 0.0) CSTR(LENREP+1:)=CREP(2)
END IF

M=INDEX(CSTR(MSTART:),'BUFR')

!-----------------------------------------------------------------------
! Loop continually while BUFR messages found.
!-----------------------------------------------------------------------

DO WHILE (M > 0)

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
    END DO ! J
    MEND=MEND+4  ! 4 bytes for section 4
    MLEN=MEND-MSTART+1

!-----------------------------------------------------------------------
! (2) BUFR edition >1: Get length from section 0
!-----------------------------------------------------------------------

  ELSE
    MLEN=ICHAR3(CSTR(MSTART+4:MSTART+6))
    MEND=MSTART+MLEN-1
  END IF

!-----------------------------------------------------------------------
! Increment number of BUFR messages. Update MSTART
!-----------------------------------------------------------------------

  NBUFR=NBUFR+1
  MSTART=MEND+1

!-----------------------------------------------------------------------
! We now have a BUFR message - but if pointer is now to second LENREP
! string, move that string up and put more data from the CREP array
! into the second string. This should keep BUFR messages within the
! first string of length LENREP.
!-----------------------------------------------------------------------

  IF (MSTART > LENREP) THEN
    CSTR(1:LENREP)=CSTR(LENREP+1:)
    CSTR(LENREP+1:)=' '
    IREP=IREP+1
    IF (IREP <= MDB_NOBS) CSTR(LENREP+1:)=CREP(IREP)
    MSTART=MSTART-LENREP
  END IF

  M=INDEX(CSTR(MSTART:),'BUFR')   !- Look for 'BUFR' again.

END DO !- M

RETURN
END SUBROUTINE COUNTBMSG

