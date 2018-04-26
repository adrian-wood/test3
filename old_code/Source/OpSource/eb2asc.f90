SUBROUTINE EB2ASC(LENGTH,STRING)
! ---------------------------------------------------------------------
!
! PROGRAM       : EB2ASC
!
! CALLED BY     : BUFDATA,BUFDCHR,ENBUFV2,ENCODE
!
! PURPOSE       : TO CONVERT CHARACTERS FROM EBCDIC TO ASCII CODE
! CALLS         :
!
! Parameters    :
!  (1) LENGTH   Number of characters to be converted (I)
!  (2) STRING   String to be converted (I/O), i.e. number of
!
! Error returns : none
!
! REVISION INFO :
!
! $Workfile: eb2asc.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 09/02/2011 16:46:29$
!
! CHANGE RECORD :
! $Log:
!  5    MetDB_Refresh 1.4         09/02/2011 16:46:29    Sheila Needham  Use
!       int2ch function
!  4    MetDB_Refresh 1.3         14/12/2010 16:21:45    Rosemary Lavery Added
!       INTENTs
!  3    MetDB_Refresh 1.2         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  2    MetDB_Refresh 1.1         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  1    MetDB_Refresh 1.0         07/10/2010 10:56:56    Sheila Needham
!       Version copied from MET.SRCELIB
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
!
! ---------------------------------------------------------------------
USE int2ch_mod

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(IN)                   :: LENGTH
CHARACTER(LEN=LENGTH), INTENT(INOUT)  :: STRING

! Local Variables

INTEGER :: J
INTEGER :: IASCII

! THIS LOOKUP TABLE HELPS TO
!     CONVERT THE CONTENTS OF A CHARACTER STRING
!     FROM  EBCDIC TO ASCII REPRESENTATION
!
INTEGER :: IBMCHA(0:255)
DATA IBMCHA      &
      /  0,  1,  2,  3,156,  9,134,127,151,141,142, 11, 12, 13, 14, 15, &
        16, 17, 18, 19,157,133,  8,135, 24, 25,146,143, 28, 29, 30, 31, &
       128,129,130,131,132, 10, 23, 27,136,137,138,139,140,  5,  6,  7, &
       144,145, 22,147,148,149,150,  4,152,153,154,155, 20, 21,158, 26, &
        32,160,161,162,163,164,165,166,167,168, 91, 46, 60, 40, 43, 33, &
        38,169,170,171,172,173,174,175,176,177, 93, 36, 42, 41, 59, 94, &
        45, 47,178,179,180,181,182,183,184,185,124, 44, 37, 95, 62, 63, &
       186,187,188,189,190,191,192,193,194, 96, 58, 35, 64, 39, 61, 34, &
       195, 97, 98, 99,100,101,102,103,104,105,196,197,198,199,200,201, &
       202,106,107,108,109,110,111,112,113,114,203,204,205,206,207,208, &
       209,126,115,116,117,118,119,120,121,122,210,211,212,213,214,215, &
       216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231, &
       123, 65, 66, 67, 68, 69, 70, 71, 72, 73,232,233,234,235,236,237, &
       125, 74, 75, 76, 77, 78, 79, 80, 81, 82,238,239,240,241,242,243, &
        92,159, 83, 84, 85, 86, 87, 88, 89, 90,244,245,246,247,248,249, &
        48, 49, 50, 51, 52, 53, 54, 55, 56, 57,250,251,252,253,254,255/
!     =====================================================

DO J=1,LENGTH
  IASCII = IBMCHA( ICHAR(STRING(J:J)) )
  STRING(J:J) = int2ch(IASCII)
END DO

RETURN
END SUBROUTINE EB2ASC
