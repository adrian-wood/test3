SUBROUTINE GETVALS (LISTDES, LISTLEN, MESSAGE, MAXVALS, VALUES, &
                    CSTR, NPOS, NUMOBS, KODE)
!
!-----------------------------------------------------------------------
! SUBROUTINE  : GETVALS
!
! PURPOSE     : To do a partial decode of a BUFR message, extracting
!               values for a user-supplied list of parameters.
!
! DESCRIPTION : The BUFR message (MESSAGE) is partially decoded to
!               determine the values of parameters whose descriptors
!               are given in the array LISTDES of length LISTLEN.
!               Decoded values are returned in the array VALUES.
!
!               Also returned are the number of observations in the
!               message (NUMOBS) and the locations in the VALUES
!               array of the descriptors in the original list (NPOS).
!               If a descriptor is not found in the message, the
!               corresponding element of NPOS is set to 0.
!
! USAGE       : CALL GETVALS (LISTDES, LISTLEN, MESSAGE, MAXVALS,
!                             VALUES, CSTR, NPOS, NUMOBS, KODE)
!
! PARAMETERS  : ('I'=Input, 'O'=Output)
!
!               LISTDES (I)  Integer array giving BUFR descriptors for
!                            parameters to be decoded from message.
!               LISTLEN (I)  Number of items in LISTDES. .
!               MESSAGE (I)  (CHARACTER*(*)) BUFR message to be
!                            decoded (starting from "BUFR").
!               MAXVALS (I)  Size of VALUES array.
!               VALUES  (O)  Real array of size MAXVALS to hold
!                            decoded values from message.
!               CSTR    (O)  (CHARACTER*(*)) Array of character
!                            elements decoded from message.
!               NPOS    (O)  Integer array of size LISTLEN giving
!                            location of LISTDES items in the
!                            sequence of decoded parameters.
!               NUMOBS  (O)  Number of observations in BUFR message.
!               KODE    (O)  Integer return code as follows:
!
!                               0  Successsful completion
!                              21  VALUES array too small (i.e. needs
!                                    more than MAXVALS elements)
!                              42  BUFR bulletin failed to decode
!
! CALLED BY   : INDEX1
!
! CALLS       : DECORD  (Also call to DREG commented out at the moment)
!
! HISTORY     : Original version by Brian Barwell, 1 August 2000.
!
! REVISION INFO:
!
! $Workfile: getvals.f90$ $Folder: OpSource$
! $Revision: 8$ $Date: 29/06/2012 11:49:52$
!
! CHANGE RECORD:
!
! $Log:
!  8    MetDB_Refresh 1.7         29/06/2012 11:49:52    Brian Barwell
!       Correction to error in comments.
!  7    MetDB_Refresh 1.6         26/06/2012 17:00:27    Brian Barwell   Look
!       for alternative resolution lat/long descriptors if needed.
!  6    MetDB_Refresh 1.5         27/01/2012 16:57:36    Brian Barwell
!       Increase MAXDES to 25000.
!  5    MetDB_Refresh 1.4         10/02/2011 11:09:08    Stan Kellett
!       uncomment USE decord_mod
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:16:28    John Norton     
!  1    MetDB_Refresh 1.0         13/10/2010 16:00:38    John Norton     f77
!       code copied out of StarTeam
!       MIDE\Met_DB_Project\Development\Source\OpSource
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:

use decord_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER, INTENT(IN)             ::  LISTLEN ! Number of descriptors to be decoded
INTEGER, INTENT(IN)             ::  LISTDES(LISTLEN) ! List of descriptors to be decoded
CHARACTER(*), INTENT(IN)        ::  MESSAGE ! BUFR message
INTEGER, INTENT(IN)             ::  MAXVALS ! Size of VALUES array
REAL, INTENT(OUT)               ::  VALUES(MAXVALS) ! Parameter values returned by DECORD
CHARACTER(*), INTENT(OUT)       ::  CSTR ! Character elements from DECORD !2.2
INTEGER, INTENT(OUT)            ::  NPOS(LISTLEN) ! Position of LISTDES items in sequence
INTEGER, INTENT(OUT)            ::  NUMOBS ! Number of observations in message
INTEGER, INTENT(OUT)            ::  KODE ! Return code

! Local declarations:
!                                                            Parameters
INTEGER, PARAMETER  ::  MAXDES = 25000  ! Size of LDES array         !6
!                                                             Variables
INTEGER  :: JDES         ! Loop variable for BUFR descriptors
INTEGER  :: KOPYDES(20)  ! Local copy of LISTDES with two additions  !7
INTEGER  :: KOPYLEN      ! No. of descriptors in KOPYDES (LISTLEN+2) !7
INTEGER  :: KOPYPOS(20)  ! Local copy of NPOS with two additions     !7
INTEGER  :: LDES(MAXDES) ! Array of BUFR descriptors from decode
INTEGER  :: LISTITEM     ! Loop variable for wanted parameters
INTEGER  :: NELEMS       ! Number of decoded values per ob.
INTEGER  :: NUMDES       ! Number of descriptors in message

!-----------------------------------------------------------------------
!  MAKE LOCAL COPY OF LISTDES AND ADD ALTERNATIVE LAT/LONG DESCRIPTORS
!-----------------------------------------------------------------------
! This section was added for data types like BUFR sondes and AMDARS  !7
! for which some sequences use high-resolution lat/long descriptors  !7
! 005001 & 006001 (binary 1281 & 1537) and some use low-resolution   !8
! ones 005002 & 006002 (binary 1282 & 1538). One pair will be set in !8
! LISTDES elements 7 & 8: the other pair is added to the list here   !7
! to ensure that DECORD will decode far enough for either case.      !7
! In what follows, 2563 = 1281 + 1282 and 3075 = 1537 + 1538.        !7

KOPYLEN = LISTLEN + 2                    ! 2 extra descriptors       !7
KOPYDES(1:LISTLEN) = LISTDES(1:LISTLEN)  ! Copy of LISTDES           !7
KOPYDES(1+LISTLEN) = 2563 - LISTDES(7)   ! Alternative latitude      !7
KOPYDES(2+LISTLEN) = 3075 - LISTDES(8)   ! Alternative longitude     !7

!-----------------------------------------------------------------------
!  PARTIAL DECODE OF BUFR MESSAGE TO FIND VALUES OF ITEMS IN 'LISTDES'.
!-----------------------------------------------------------------------
!                                  Initialisations for call to 'DECORD'
NUMDES = MAXDES
NUMOBS = MAXVALS
!                                        Partial decode of BUFR message

CALL DECORD (LDES, VALUES, CSTR, NUMDES, NUMOBS, &
             MESSAGE, .FALSE., KOPYDES, KOPYLEN)

!                          Warning message if bulletin failed to decode
!           (Message suppressed as some SEAWINDS bulletins have 0 obs.)
!                                                    (Return code = 42)
IFLABEL1: &
IF (NUMOBS <= 0) THEN
!*       WRITE (6,'(T5,A,T15,A)') 'GETVALS:',
!*   &                            'BUFR bulletin failed to decode'
!*       CALL DREG (30, 'BUFR BULLETIN FAILED TO DECODE.', MESSAGE,
!*   &              'GETVALS', ' ', ' ', 0, ' ')
   KODE = 42

ELSE
!                   Remove descriptors not corresponding to data values
   NELEMS = 0
   DO JDES=1,NUMDES
      LDES(JDES) = MOD(LDES(JDES),131072)
      IF (LDES(JDES) < 16384) THEN ! Element descriptor
         NELEMS = NELEMS + 1
         LDES(NELEMS) = LDES(JDES)
      END IF
   END DO ! JDES
!                          Warning message for "VALUES" array too small
!                                                    (Return code = 21)
IFLABEL2: &
   IF (NUMOBS*NELEMS > MAXVALS) THEN
      WRITE (6,'(T5,A,T15,A,I5,A,I6,A)') 'GETVALS:', &
            'Array is too small to hold ', NELEMS, &
            ' elements for', NUMOBS, ' observations.'
      KODE = 21
   ELSE
!
!-----------------------------------------------------------------------
!  FIND POSITIONS OF DESCRIPTORS WANTED FOR THE INDEX ENTRY
!-----------------------------------------------------------------------
!                                           Loop over wanted parameters
      DO LISTITEM=1,KOPYLEN
         KOPYPOS(LISTITEM) = 0                                       !7
!                                      Loop over items in BUFR sequence
         DO JDES=1,NELEMS                                            !7
            IF (LDES(JDES) == KOPYDES(LISTITEM)) THEN ! found it
               KOPYPOS(LISTITEM) = JDES                              !7
               EXIT                                                  !7
            END IF
         END DO
      END DO ! LISTITEM
      NPOS(1:LISTLEN) = KOPYPOS(1:LISTLEN)                           !7

!                            Check for alternative lat/long descriptors

      IF (NPOS(7) == 0) NPOS(7) = KOPYPOS(KOPYLEN-1)  ! Latitude     !7
      IF (NPOS(8) == 0) NPOS(8) = KOPYPOS(KOPYLEN)    ! Longitude    !7

!                             Set return code for successful completion
      KODE = 0
   END IF IFLABEL2
END IF IFLABEL1
!                                             Return to calling program
RETURN
END SUBROUTINE GETVALS
