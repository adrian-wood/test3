SUBROUTINE BUFRQEL(STRING,CMPRES,LASTEL,LASBIT,N,NEXBIT,   &
    SCALE,WIDTH,REFVAL,FORMAT,DESCR,NLOG,IRC)

! ---------------------------------------------------------------------
!
! Program       : BUFRQEL
!
! Called by     : BUFDATA
!
! Purpose       : To find the element corresponding to a place holder
!  (informal)     for a quality operation.
!
!                 (A "place holder" is an operator 2xx255 as defined
!                 in BUFR Table C.  Such a descriptor corresponds to
!                 the value of an element in the data section, but
!                 only the bit map concerned shows which element,
!                 and hence which scale, width etc, allowing the
!                 value to be decoded.)
!
!    (formal)     BUFRQEL is called in two ways, (a) & (b)
!             (a)  If the descriptor sequence is known to include
!                 quality operations, it is called for every element
!                 to log the details which will be required when a
!                 place holder is reached.  These are the sequence
!                 number of the descriptor in the array as it now is
!                 (not necessarily as it will be returned to the user)
!                 and the scale, width etc used to decode the value
!                 (not necessarily those in Table B, so they have to
!                 be kept rather than looked up again).
!             (b)  When a place holder is reached, BUFRQEL is called
!                 to return the scale etc, replacing the place holder
!                 by the orginal element in the dsecriptor array.
!                    (The orginal intention was to insert pointers to
!                   link the value corresponding to the place holder
!                   & the original value in a way which survives any
!                   deletion of descriptors done before returning to
!                   the user.  The pointers would have consisted of
!                   the place holder itself with an insertion sequence
!                   number in the top bits.  But this would have
!                   returned more descriptors without corresponding
!                   values; it didn't apply to quality operations
!                   without place holders (e.g. % confidences; and
!                   it's inconsistent with our general approach to
!                   retrieval, to make named elements retrievable
!                   rather than leave users to interpret descriptor
!                   sequences.)
!                 The difficult part of the program finds the next
!                 zero in the bit map and goes back to the appropriate
!                 entry in the log, given that the bit map corresponds
!                 to the last N elements at a certain point in the
!                 decoding process.
!
! Calls         : VALUE to get bit from map
!
! Parameters    :
!  (1) STRING   bit string (data section of BUFR message)          (i)
!                (not changed)
!  (2) CMPRES   true if data compressed (& bit map has 6-bit increment
!                widths between map bits!)                         (i)
!                (not changed)
!  (3) LASTEL   value of NLOG at point where the bit map refers to the
!                last so many elements                             (i)
!                (not changed)
!  (4) LASBIT   last bit in the bit map (in STRING)                (i)
!                (not changed)
!  (5) N        number of current descriptor (place holder)        (i)
!                (not changed)
!  (6) NEXBIT   current next bit in bit map                       (i/?)
!                (input to give start of bit map; updated value output,
!                 but not used by calling program)
!  (7) SCALE    (a) to go in log or (b) from log               (i or o)
!                ((a): input, (b): output)
!  (8) WIDTH    (a) to go in log or (b) from log               (i or o)
!                ((a): input, (b): output)
!                 Input WIDTH decides between (a) & (b):
!                 WIDTH=0: look up log.  WIDTH>0: add details to log.
!  (9) REFVAL   (a) to go in log or (b) from log               (i or o)
!                ((a): input, (b): output)
! (10) FORMAT   (a) to go in log or (b) from log               (i or o)
!                ((a): input, (b): output)
! (11) DESCR    descriptor array                                  (i/o)
!                (returned with place holder replaced by element descr)
! (12) NLOG     number of elements logged                         (i/o)
!                (returned so that LASTEL can be set)
! (13) IRC      return code:                                       (o)
!         IRC=222: No bit map
!         IRC=223: No more zero bits in bit map
!         IRC=224: Log too small
!
! REVISION INFO :
!
! $Workfile: bufrqel.f90$ $Folder: OpSource$
! $Revision: 10$ $Date: 08/10/2012 11:04:01$
!
! CHANGE RECORD :
!
! $Log:
!  10   Met_DB_Project 1.9         08/10/2012 11:04:01    Sheila Needham
!       Increase LOGDIM and return error if no room in log array.
!  9    MetDB_Refresh 1.8         21/02/2011 11:30:16    Alison Weir     (:)
!       changed to (*) for BUFRSHEL
!  8    MetDB_Refresh 1.7         09/02/2011 16:37:31    Sheila Needham
!       Initialise IRC
!  7    MetDB_Refresh 1.6         24/11/2010 15:11:13    Brian Barwell   Intent
!        of argument CMPRES changed from INOUT to IN.
!  6    MetDB_Refresh 1.5         12/11/2010 16:06:36    Richard Weedon  Peer
!       rework completed
!  5    MetDB_Refresh 1.4         25/10/2010 13:52:07    Richard Weedon  INTENT
!        indicators added , & changed for F90 standard
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:57:24    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
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
! ---------------------------------------------------------------------

USE value_mod

IMPLICIT  NONE

INTEGER,PARAMETER ::  LOGDIM=5000
INTEGER           ::  ELMLOG(LOGDIM) ! to log width/scale/descriptors
INTEGER           ::  REFLOG(LOGDIM) ! to log reference values

CHARACTER(LEN=*),INTENT(IN)       ::  STRING        ! argument (1)
CHARACTER(LEN=1),INTENT(INOUT)    ::  FORMAT        ! argument (10)
LOGICAL,INTENT(IN)                ::  CMPRES        ! ARGUMENT (2)
INTEGER,INTENT(INOUT)             ::  DESCR(*)      ! ARGUMENT (11)

INTEGER,INTENT(IN)        ::  LASTEL  !last element in bit map-arg.(3)
INTEGER,INTENT(INOUT)     ::  NLOG    !number of values logged-arg.(12)
INTEGER                   ::  LSCALE  ! unsigned scale (range 0-255)
INTEGER,INTENT(INOUT)     ::  SCALE   ! argument (7)
INTEGER,INTENT(INOUT)     ::  WIDTH   ! argument (8)
INTEGER,INTENT(IN)        ::  N       ! argument (5)
INTEGER                   ::  NDFLAGS ! N & flags (to go in log)
INTEGER,INTENT(INOUT)     ::  REFVAL  !argument (9)
INTEGER,INTENT(IN)        ::  LASBIT  !last bit in bit map-argument(4)
INTEGER,INTENT(INOUT)     ::  NEXBIT  !next bit in bit map -argument(6)
INTEGER                   ::  MAPBIT ! 0 or 1 from bit map
INTEGER                   ::  LOGNTRY! subscript for ELMLOG & REFLOG
INTEGER,INTENT(OUT)       ::  IRC    ! argument (13)

SAVE

IRC = 0
! If this call is to log element details (WIDTH>0), then do so
! (if there's room left in the log)
! The log combines Table B details (scale, width & flags based on units)
! with a descriptor sequence number (which can be up to 2**13-1=8191
! before the top bit is reached, giving sign problems!)

IF_CONSTR1 : &
IF (WIDTH > 0) THEN
  
  IF (NLOG < LOGDIM) THEN
    NLOG=NLOG+1
    REFLOG(NLOG)=REFVAL
    LSCALE=SCALE
    IF (LSCALE < 0) LSCALE=SCALE+256      ! negative scale

    NDFLAGS=N*4                           ! N in top 14 bits
    IF (FORMAT == 'C') THEN
      NDFLAGS=NDFLAGS+1                   ! code table
    ELSE IF (FORMAT == 'F') THEN
      NDFLAGS=NDFLAGS+2                   ! flag table
    ELSE IF (FORMAT == 'A') THEN
      NDFLAGS=NDFLAGS+3                   ! characters
    END IF
    ELMLOG(NLOG)=NDFLAGS*65536+WIDTH*256  ! width in bits 17-24
    ELMLOG(NLOG)=ELMLOG(NLOG)+LSCALE      ! scale in bits 25-32
    
  ELSE
    PRINT*,'No room to log quality placeholder details. Change LOGDIM to &
    & get past this operation.'
    IRC=224
    RETURN
  END IF

! If the call is to find which element a place holder corresponds to
! and get its details from the log, find the next zero in the bit map.
! N.B. The ZEROs in the bit map correspond to values, not the ones!
! (because one bit set to 1 means missing in BUFR?)

ELSE IF_CONSTR1
  IF (LASBIT == 0) THEN
    PRINT *,'Quality place holder found, but no bit map defined'
    IRC=222
    RETURN
  END IF

! Loop round bits in bit map until zero bit found or end of map reached.
! N.B. if data is compressed, there are increment widths between bits!

  MAPBIT=1
  DO WHILE (MAPBIT == 1 .AND. NEXBIT <= LASBIT)
    MAPBIT=VALUE(STRING,NEXBIT,1)
    IF (CMPRES) NEXBIT=NEXBIT+6
  END DO

  IF (NEXBIT > LASBIT) THEN
    PRINT *,&
     'Quality place holder found, but no more zero bits in bit map'
    IRC=223
    RETURN
  END IF

! From bit number of zero in bit map get subscript of entry in log
! & set element details from log entry.

  LOGNTRY=LASTEL-LASBIT+NEXBIT
  IF (CMPRES) LOGNTRY=LASTEL-(LASBIT-NEXBIT)/7

  IF_CONSTR2 : &
  IF (LOGNTRY > LOGDIM) THEN
    PRINT*,'Quality operator place holder found, but log too'
    PRINT*,'small. Change LOGDIM to get past this operation.'
    IRC=224
    RETURN
  ELSE IF_CONSTR2
    WIDTH=MOD(ELMLOG(LOGNTRY),65536)/256
    SCALE=MOD(ELMLOG(LOGNTRY),256)
    IF (SCALE >= 128) SCALE=SCALE-256
    REFVAL=REFLOG(LOGNTRY)

! NDFLAGS is a 14-bit descriptor number combined with flags in the
! bottom 2 bits, hence NDFLAGS/4 is the number.
! The 2 bits only distinguish between 4 possible FORMATs.  TABLEB
! sets 5 different values, but the distinction between N & R is
! never used.  N.B. this is a constraint on values of FORMAT!

    NDFLAGS=ELMLOG(LOGNTRY)/65536
    FORMAT='N'                               ! numerical
    IF (MOD(NDFLAGS,4) == 1) FORMAT='C'      ! code table
    IF (MOD(NDFLAGS,4) == 2) FORMAT='F'      ! flag table
    IF (MOD(NDFLAGS,4) == 3) FORMAT='A'      ! alphanumeric

! Replace place holder by original descriptor

    DESCR(N)=DESCR(NDFLAGS/4)
  END IF IF_CONSTR2

! Copy the log entry for the original element to the slot for
! the current descriptor (just replacing the sequence number)

  IF (NLOG < LOGDIM) THEN
    NLOG=NLOG+1
    ELMLOG(NLOG)=ELMLOG(LOGNTRY)-NDFLAGS/4+N
    REFLOG(NLOG)=REFVAL
  END IF
END IF IF_CONSTR1
RETURN
END SUBROUTINE BUFRQEL
