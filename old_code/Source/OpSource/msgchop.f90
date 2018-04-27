SUBROUTINE MSGCHOP (NCHAN, NCHANS, NOBS1, NELEMS, NOBS2, NSEQ, &
                    VALIN, VALOUT, MSGBULL, LENBUL)

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
! ARGUMENTS   : ('I'=Input, 'O'=Output, 'I/O'= Both)
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
!      8  VALOUT  O   R4    *   Work array for values to be encoded
!                                 (effective dimensions (NOBS2,NELEMS).)
!      9  MSGBULL O  C(*)       Output MSGWINDS bulletin ('BUFR..7777').
!     10  LENBUL  O   I4        Length of output bulletin in bytes.
!
! CALLED BY   : STOREUM
!
! CALLS       : DATIM, ENBUFV4, IDES
!
! HISTORY     : Original version by Brian Barwell, February 2004.
!
! REVISION INFO :
!
!
! $Workfile: msgchop.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 08/03/2011 09:05:53$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         08/03/2011 09:05:53    Stan Kellett
!       Removed LVER
!  2    MetDB_Refresh 1.1         07/03/2011 11:05:12    Stan Kellett
!       updated to use enbufv4 instead of enbufv2
!  1    MetDB_Refresh 1.0         27/01/2011 16:19:32    Rosemary Lavery
!       Initial port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE DATIM_mod
USE ENBUFV4_mod
USE IDES_mod

IMPLICIT NONE

! Interface Arguments
INTEGER, INTENT(IN)             :: NCHAN     ! (a1)  Channel for obs wan ted in output bulletin
INTEGER, INTENT(IN)             :: NCHANS(:) ! (a2)  Channel numbers of  obs in input bulletin
INTEGER, INTENT(IN)             :: NOBS1     ! (a3)  Number of obs in in put values array VALIN
INTEGER, INTENT(IN)             :: NELEMS    ! (a4)  Number of values pe r ob in VALIN array
INTEGER, INTENT(IN)             :: NOBS2     ! (a5)  Number of obs in ou tput BUFR message
INTEGER, INTENT(IN)             :: NSEQ      ! (a6)  Number of obs in ou tput BUFR message
REAL, INTENT(IN)                :: VALIN(:)  ! (a7)  Decoded values from  MSGWINDS bulletin
REAL, INTENT(OUT)               :: VALOUT(:) ! (a8)  Values for encoding  new MSGWINDS bulletin
CHARACTER (LEN=*), INTENT(OUT)  :: MSGBULL   ! (a9)  Re-encoded MSG bull etin
INTEGER, INTENT(OUT)            :: LENBUL    ! (a10) Length of re-encoded message

! Parameters

INTEGER, PARAMETER  :: MAXDES=300    ! Size of LISTDES array (1 value for
                                     ! each descriptor after expansion)
                                     ! (MSGWINDS currently has about 250)
INTEGER, PARAMETER  :: IEDN = 3      ! )
INTEGER, PARAMETER  :: MSTR = 0      ! )
INTEGER, PARAMETER  :: ICTR = 74     ! ) for call to ENBUFV4
INTEGER, PARAMETER  :: IDTATP = 5    ! )
INTEGER, PARAMETER  :: ISUBTP = 10   ! )
INTEGER, PARAMETER  :: MSTR_VER = 13 ! )
INTEGER, PARAMETER  :: ISC3 = 1      ! )
INTEGER, PARAMETER  :: NIL = -99     ! )
LOGICAL, PARAMETER  :: ON  = .TRUE.  ! )
LOGICAL, PARAMETER  :: OFF = .FALSE. ! )

! Variables

INTEGER  :: J         ! Loop vaviable for general use
INTEGER  :: JOB       ! Loop vaviable for loop over observations
INTEGER  :: JVAL      ! Loop vaviable for loop over ob values
INTEGER  :: NDES      ! Number of descriptors in LISTDES array
INTEGER  :: NTIME(6)  ! Time of receipt (yr, mon, day, hr, min, sec)
INTEGER  :: NOB2      ! Ob counter for obs in output array
INTEGER  :: NOW(8)    ! Current date and time (from call to DATIM)
INTEGER  :: NVAL1     ! Pointer to location in VALIN array
INTEGER  :: NVAL2     ! Pointer to location in VALOUT array
INTEGER, SAVE  :: LISTDES(MAXDES) ! BUFR descriptor sequence for message

CHARACTER (LEN=1)  :: CDUMMY  ! Dummy character argument for ENBUFV4

!-----------------------------------------------------------------------
! INITIALISATIONS
!-----------------------------------------------------------------------
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
  IF (NCHANS(JOB) == NCHAN) THEN  ! Required channel
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
DO J=1,6              ! Yr, mon, day, hr, min, sec
  NTIME(J) = NOW(9-J)
END DO ! J
!                                                   Encode new bulletin

CALL ENBUFV4 (LISTDES, VALOUT, NDES, NELEMS, NOBS2, CDUMMY, NTIME,  &
              MSGBULL, ON, LENBUL, IEDN, MSTR, MSTR_VER, ICTR, NIL, &
              IDTATP, ISUBTP, NIL, NIL, OFF, CDUMMY, OFF, CDUMMY, ISC3)

!                                             Return to calling program
RETURN
END SUBROUTINE MSGCHOP
