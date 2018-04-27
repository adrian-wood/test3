      SUBROUTINE GETVALS (LISTDES, LISTLEN, MESSAGE, MAXVALS, VALUES,
     &                    CSTR, NPOS, NUMOBS, KODE)
!
!-----------------------------------------------------------------------
!
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
! $Revision: 1$
! $Date: 30/01/2006 20:22:37$
! $Source: /home/us0400/mdb/op/lib/source/RCS/getvals.f,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:37    Sheila Needham  
! $
! Revision 2.4  2005/06/07 09:05:21  usmdb
! 2.4.  20 June 2005.  Brian Barwell.  INC155236 & CHG014012.
!
! Increase size of LDES array in GETVALS from 8192 to 12500.
!
! Revision 2.3 2004/12/06 12:13:54 12:13:54 usmdb (MetDB account c/o J C Ward)
! 2.3.  20 December 2004.  Brian Barwell.  Remedy CHG009245.
! Increase size of array for descriptors from 4096 to 8192.
!
! Revision 2.2 2003/01/06 16:20:04 16:20:04 usmdb (MetDB account c/o J C Ward)
! CSTR incorrectly declared as an array. Changed declaration
! to a scalar - S.Cox
!
! Revision 2.1  2002/01/15 15:15:48  usmdb
! 2.1.  21 January 2002. Brian Barwell.  Change 164/01.
! Increase MAXDES from 512 to 4096 (required for GPSRO data).
!
! Revision 2.0  2001/06/06  10:19:19  10:19:19  usmdb (Generic MetDB account)
! Initial version
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!                                                            Parameters
!
      INTEGER    MAXDES        ! Maximum size for BUFR descriptor array
      PARAMETER (MAXDES=12500)                                      !2.4
!                                                             Variables
!
      INTEGER JDES             ! Loop variable for BUFR descriptors
      INTEGER KODE             ! Return code
      INTEGER LDES(MAXDES)     ! Array of BUFR descriptors
      INTEGER LISTLEN          ! Number of descriptors to be decoded
      INTEGER LISTDES(LISTLEN) ! List of descriptors to be decoded
      INTEGER LISTITEM         ! Loop variable for wanted parameters
      INTEGER MAXVALS          ! Size of VALUES array
      INTEGER NELEMS           ! Number of decoded values per ob.
      INTEGER NPOS(LISTLEN)    ! Position of LISTDES items in sequence
      INTEGER NUMDES           ! Number of descriptors in message
      INTEGER NUMOBS           ! Number of observations in message
!
      LOGICAL FIRST            ! .TRUE. if first call to this routine
      LOGICAL FOUND            ! Flag for item found in BUFR sequence
!
      REAL VALUES(MAXVALS)     ! Parameter values returned by DECORD
!
      CHARACTER*(*) CSTR       ! Character elements from DECORD     !2.2
      CHARACTER*(*) MESSAGE    ! BUFR message
      CHARACTER HEAD*132       ! Revision details
!                                                        Saved variable
      SAVE FIRST
!                                                   Data initialisation
      DATA FIRST /.TRUE./
!                                                  Revision information
      IF (FIRST) THEN
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/getvals.f,v $
     &   '//'$Date: 30/01/2006 20:22:37$ $Revision: 1$'
         FIRST = .FALSE.
      END IF
!
!-----------------------------------------------------------------------
!  PARTIAL DECODE OF BUFR MESSAGE TO FIND VALUES OF ITEMS IN 'LISTDES'.
!-----------------------------------------------------------------------
!
!                                  Initialisations for call to 'DECORD'
      NUMDES = MAXDES
      NUMOBS = MAXVALS
!                                        Partial decode of BUFR message
!
      CALL DECORD (LDES, VALUES, CSTR, NUMDES, NUMOBS,
     &             MESSAGE, .FALSE., LISTDES, LISTLEN)
!
!                          Warning message if bulletin failed to decode
!           (Message suppressed as some SEAWINDS bulletins have 0 obs.)
!                                                    (Return code = 42)
      IF (NUMOBS.LE.0) THEN
!*       WRITE (6,'(T5,A,T15,A)') 'GETVALS:',
!*   &                            'BUFR bulletin failed to decode'
!*       CALL DREG (30, 'BUFR BULLETIN FAILED TO DECODE.', MESSAGE,
!*   &              'GETVALS', ' ', ' ', 0, ' ')
         KODE = 42
!
      ELSE
!                   Remove descriptors not corresponding to data values
         NELEMS = 0
         DO JDES=1,NUMDES
            LDES(JDES) = MOD(LDES(JDES),131072)
            IF (LDES(JDES).LT.16384) THEN ! Element descriptor
               NELEMS = NELEMS + 1
               LDES(NELEMS) = LDES(JDES)
            END IF
         END DO ! JDES
!                          Warning message for "VALUES" array too small
!                                                    (Return code = 21)
         IF (NUMOBS*NELEMS.GT.MAXVALS) THEN
            WRITE (6,'(T5,A,T15,A,I5,A,I6,A)') 'GETVALS:',
     &            'Array is too small to hold ', NELEMS,
     &            ' elements for', NUMOBS, ' observations.'
            KODE = 21
         ELSE
!
!-----------------------------------------------------------------------
!  FIND POSITIONS OF DESCRIPTORS WANTED FOR THE INDEX ENTRY
!-----------------------------------------------------------------------
!
!                                           Loop over wanted parameters
            DO LISTITEM=1,LISTLEN
               NPOS(LISTITEM) = 0
               FOUND = .FALSE.
!                                      Loop over items in BUFR sequence
               JDES = 1
               DO WHILE (.NOT.FOUND .AND. JDES.LE.NELEMS)
                  IF (LDES(JDES).EQ.LISTDES(LISTITEM)) THEN ! found it
                     NPOS(LISTITEM) = JDES
                     FOUND = .TRUE.
                  END IF
                  JDES = JDES + 1
               END DO
            END DO ! LISTITEM
!                             Set return code for successful completion
            KODE = 0
         END IF
      END IF
!                                             Return to calling program
      RETURN
      END
