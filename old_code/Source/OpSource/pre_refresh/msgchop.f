      SUBROUTINE MSGCHOP (NCHAN, NCHANS, NOBS1, NELEMS, NOBS2, NSEQ,
     &                    VALIN, VALOUT, MSGBULL, LENBUL)

!-----------------------------------------------------------------------
!
! SUBROUTINE  : MSGCHOP
!
! PURPOSE     : To create a new MSGWINDS bulletin similar to an input
!               one but containing only observations for a given
!               channel.
!
! DESCRIPTION : Given an array of decoded values (VALIN) from an
!               MSGWINDS bulletin, MSGCHOP extracts observations
!               derived from a specified satellite channel (NCHAN),
!               copies them to a similar array (VALOUT) and converts
!               this into a new BUFR message MSGBULL of length LENBUL.
!
!               Channel numbers for the observations are input in an
!               array NCHANS. The numbers of observations in the input
!               values array and the output BUFR bulletin together with
!               the number of data values per observation and the BUFR
!               sequence descriptor for the output are also required as
!               input to MSGCHOP. Most of this information is obtained
!               from a prior call to the routine MSGDCODE.
!
!               VALOUT is only for internal use but is put in the
!               argument list so that the user can have control over
!               its size. It should be the same size as VALIN but must
!               be a separate array.
!
! USAGE       : CALL MSGCHOP (NCHAN, NCHANS, NOBS1, NELEMS, NOBS2,
!                             NSEQ, VALIN, VALOUT, MSGBULL, LENBUL)
!
! PARAMETERS  : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!     No.  Arg.  I/O Type  Size   Contents
!     --  -----  --- ----  ----   --------
!      1  NCHAN   I   I4        Channel number for obs in output.
!      2  NCHANS  I   I4  NOBS1 Array of channel numbers for each ob.
!      3  NOBS1   I   I4        Number of obs in input array (VALIN).
!      4  NELEMS  I   I4        Data values per ob in VALIN & VALOUT.
!      5  NOBS2   I   I4        Number of obs in output array (VALOUT).
!      6  NSEQ    I   I4        BUFR sequence descriptor for output.
!      7  VALIN   I   R4    *   Input array of decoded element values
!                                 (effective dimensions (NOBS1,NELEMS).)
!      8  VALOUT  I   R4    *   Work array for values to be encoded
!                                 (effective dimensions (NOBS2,NELEMS).)
!      9  MSGBULL O  C(*)       Output MSGWINDS bulletin ('BUFR..7777').
!     10  LENBUL  O   I4        Length of output bulletin in bytes.
!
! CALLED BY   : STOREUM
!
! CALLS       : DATIM, ENBUFV2, IDES
!
! HISTORY     : Original version by Brian Barwell, February 2004.
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:25$
! $Source: /home/us0400/mdb/op/lib/source/RCS/msgchop.f,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:25    Sheila Needham  
! $
! Revision 1.1  2004/02/02 12:33:24  usmdb
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2004 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!                                                            Parameters

      INTEGER MAXDES         ! Size of LISTDES array (1 value for
                             !   each descriptor after expansion)
      PARAMETER (MAXDES=300) ! (MSGWINDS currently has about 250)

!                                                             Variables

      INTEGER IDES          ! 'FXXYYY'-to-integer conversion function
      INTEGER J             ! Loop vaviable for general use
      INTEGER JOB           ! Loop vaviable for loop over observations
      INTEGER JVAL          ! Loop vaviable for loop over ob values
      INTEGER LENBUL        ! Length of re-encoded message
      INTEGER LISTDES(MAXDES) ! BUFR descriptor sequence for message
      INTEGER NCHAN         ! Channel for obs wanted in output bulletin
      INTEGER NCHANS(*)     ! Channel numbers of obs in input bulletin
      INTEGER NDES          ! Number of descriptors in LISTDES array
      INTEGER NTIME(5)      ! Time of receipt (yr, mon, day, hr, min)
      INTEGER NELEMS        ! Number of values per ob in VALIN array
      INTEGER NOBS1         ! Number of obs in input values array VALIN
      INTEGER NOBS2         ! Number of obs in output BUFR message
      INTEGER NOB2          ! Ob counter for obs in output array
      INTEGER NOW(8)        ! Current date and time (from call to DATIM)
      INTEGER NSEQ          ! Number of obs in output BUFR message
      INTEGER NVAL1         ! Pointer to location in VALIN array
      INTEGER NVAL2         ! Pointer to location in VALOUT array

      REAL VALIN(*)         ! Decoded values from MSGWINDS bulletin
      REAL VALOUT(*)        ! Values for encoding new MSGWINDS bulletin

      LOGICAL FIRST         ! .TRUE. if first call to subroutine

      CHARACTER*1 CDUMMY    ! Dummy character argument for ENBUFV2
      CHARACTER*132 HEAD    ! Revision information
      CHARACTER*(*) MSGBULL ! Re-encoded MSG bulletin
!                                                       Saved variables
      SAVE FIRST, LISTDES
!                                                       Data statements
      DATA FIRST /.TRUE./

!-----------------------------------------------------------------------
!     REVISION INFORMATION (FIRST CALL ONLY) AND INITIALISATIONS
!-----------------------------------------------------------------------
!                                                  Revision information
      IF (FIRST) THEN
        HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/msgchop.f,v $
     &   '//'$Date: 30/01/2006 20:23:25$ $Revision: 1$'
        FIRST = .FALSE.
      END IF
!                                                       Initialisations
      LISTDES(1) = IDES(NSEQ)  ! BUFR sequence

      DO J=2,MAXDES
        LISTDES(J) = 0         ! Rest of descriptor array
      END DO ! J
      NDES = 1

!-----------------------------------------------------------------------
!     LOOP OVER OBSERVATIONS EXTRACTING DATA FOR REQUIRED CHANNEL
!-----------------------------------------------------------------------

      NOB2 = 0  ! Output ob counter
!                                                Loop over observations
      DO JOB=1,NOBS1
        IF (NCHANS(JOB).EQ.NCHAN) THEN  ! Required channel
!                                                              Pointers
          NOB2 = NOB2 + 1  ! Update output ob counter
          NVAL1 = JOB      ! First data value in VALIN
          NVAL2 = NOB2     ! First data value for VALOUT

!                                  Transfer data values to VALOUT array
          DO JVAL=1,NELEMS
            VALOUT(NVAL2) = VALIN(NVAL1)
            NVAL1 = NVAL1 + NOBS1
            NVAL2 = NVAL2 + NOBS2
          END DO ! JVAL
        END IF
      END DO ! JOB

!-----------------------------------------------------------------------
!     CREATE NEW MSGWINDS BULLETIN CONTAINING EXTRACTED OBSERVATIONS
!-----------------------------------------------------------------------
!                                                   Get time of receipt
      CALL DATIM (NOW)
      DO J=1,5              ! Yr, mon, day, hr, min
        NTIME(J) = NOW(9-J)
      END DO ! J
!                                                   Encode new bulletin

      CALL ENBUFV2 (LISTDES, VALOUT, NDES, NELEMS, NOBS2, CDUMMY,
     &              NTIME, MSGBULL, .TRUE., LENBUL, 3, 0, 74, 5, 10,
     &              11, 0, .FALSE., CDUMMY, .FALSE., CDUMMY, 1)

!                                             Return to calling program
      RETURN
      END
