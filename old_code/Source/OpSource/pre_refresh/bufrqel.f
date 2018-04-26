      SUBROUTINE BUFRQEL(STRING,CMPRES,LASTEL,LASBIT,N,NEXBIT,
     &                   SCALE,WIDTH,REFVAL,FORMAT,DESCR,NLOG,IRC)

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
! $Revision: 1$
! $Date: 30/01/2006 20:21:28$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bufrqel.f,v $
!
! CHANGE RECORD :
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
! ---------------------------------------------------------------------

      IMPLICIT  NONE

      INTEGER   LOGDIM
      PARAMETER (LOGDIM=333)
      INTEGER   ELMLOG(LOGDIM)   ! to log width/scale/descriptors
      INTEGER   REFLOG(LOGDIM)   ! to log reference values

      CHARACTER STRING*(*)       ! argument (1)
      CHARACTER FORMAT*1         ! argument (10)
      LOGICAL   CMPRES           ! argument (2)
      INTEGER   DESCR(*)         ! argument (11)

      INTEGER   LASTEL           ! last element in bit map - arg.(3)
      INTEGER   NLOG             ! number of values logged - arg.(12)
      INTEGER   LSCALE           ! unsigned scale (range 0-255)
      INTEGER   SCALE            ! argument (7)
      INTEGER   WIDTH            ! argument (8)
      INTEGER   VALUE            ! function to get bit from map
      INTEGER   N                ! argument (5)
      INTEGER   NDFLAGS          ! N & flags (to go in log)
      INTEGER   REFVAL           ! argument (9)
      INTEGER   LASBIT           ! last bit in bit map - argument (4)
      INTEGER   NEXBIT           ! next bit in bit map - argument (6)
      INTEGER   MAPBIT           ! 0 or 1 from bit map
      INTEGER   LOGNTRY          ! subscript for ELMLOG & REFLOG
      INTEGER   IRC              ! argument (13)

      LOGICAL   HEADSET
      DATA      HEADSET/.FALSE./
      CHARACTER HEAD*132

      SAVE

      IF (.NOT.HEADSET) THEN
        HEAD='$RCSfile: bufrqel.f,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:21:28$'
        HEADSET=.TRUE.
      ENDIF

! If this call is to log element details (WIDTH>0), then do so
! (if there's room left in the log)
! The log combines Table B details (scale, width & flags based on units)
! with a descriptor sequence number (which can be up to 2**13-1=8191
! before the top bit is reached, giving sign problems!)

      IF (WIDTH.GT.0) THEN
        IF (NLOG.LT.LOGDIM) THEN
          NLOG=NLOG+1
          REFLOG(NLOG)=REFVAL
          LSCALE=SCALE
          IF (LSCALE.LT.0) LSCALE=SCALE+256     ! negative scale

          NDFLAGS=N*4                           ! N in top 14 bits
          IF (FORMAT.EQ.'C') THEN                                   !2.1
            NDFLAGS=NDFLAGS+1                   ! code table        !2.1
          ELSE IF (FORMAT.EQ.'F') THEN                              !2.1
            NDFLAGS=NDFLAGS+2                   ! flag table        !2.1
          ELSE IF (FORMAT.EQ.'A') THEN                              !2.1
            NDFLAGS=NDFLAGS+3                   ! characters        !2.1
          ENDIF                                                     !2.1
          ELMLOG(NLOG)=NDFLAGS*65536+WIDTH*256  ! width in bits 17-24
          ELMLOG(NLOG)=ELMLOG(NLOG)+LSCALE      ! scale in bits 25-32
        ENDIF

! If the call is to find which element a place holder corresponds to
! and get its details from the log, find the next zero in the bit map.
! N.B. The ZEROs in the bit map correspond to values, not the ones!
! (because one bit set to 1 means missing in BUFR?)

      ELSE
        IF (LASBIT.EQ.0) THEN
          PRINT *,'Quality place holder found, but no bit map defined'
          IRC=222
          RETURN
        ENDIF

! Loop round bits in bit map until zero bit found or end of map reached.
! N.B. if data is compressed, there are increment widths between bits!

        MAPBIT=1
        DO WHILE (MAPBIT.EQ.1 .AND. NEXBIT.LE.LASBIT)
          MAPBIT=VALUE(STRING,NEXBIT,1)
          IF (CMPRES) NEXBIT=NEXBIT+6
        ENDDO

        IF (NEXBIT.GT.LASBIT) THEN
          PRINT *,
     &   'Quality place holder found, but no more zero bits in bit map'
          IRC=223
          RETURN
        ENDIF

! From bit number of zero in bit map get subscript of entry in log
! & set element details from log entry.

        LOGNTRY=LASTEL-LASBIT+NEXBIT
        IF (CMPRES) LOGNTRY=LASTEL-(LASBIT-NEXBIT)/7

        IF (LOGNTRY.GT.LOGDIM) THEN
          PRINT*,'Quality operator place holder found, but log too'
          PRINT*,'small. Change LOGDIM to get past this operation.'
          IRC=224
          RETURN
        ELSE
          WIDTH=MOD(ELMLOG(LOGNTRY),65536)/256
          SCALE=MOD(ELMLOG(LOGNTRY),256)
          IF (SCALE.GE.128) SCALE=SCALE-256
          REFVAL=REFLOG(LOGNTRY)

! NDFLAGS is a 14-bit descriptor number combined with flags in the
! bottom 2 bits, hence NDFLAGS/4 is the number.
! The 2 bits only distinguish between 4 possible FORMATs.  TABLEB   !2.1
! sets 5 different values, but the distinction between N & R is     !2.1
! never used.  N.B. this is a constraint on values of FORMAT!       !2.1

          NDFLAGS=ELMLOG(LOGNTRY)/65536
          FORMAT='N'                               ! numerical
          IF (MOD(NDFLAGS,4).EQ.1) FORMAT='C'      ! code table     !2.1
          IF (MOD(NDFLAGS,4).EQ.2) FORMAT='F'      ! flag table     !2.1
          IF (MOD(NDFLAGS,4).EQ.3) FORMAT='A'      ! alphanumeric   !2.1

! Replace place holder by original descriptor

          DESCR(N)=DESCR(NDFLAGS/4)
        ENDIF

! Copy the log entry for the original element to the slot for
! the current descriptor (just replacing the sequence number)

        IF (NLOG.LT.LOGDIM) THEN
          NLOG=NLOG+1
          ELMLOG(NLOG)=ELMLOG(LOGNTRY)-NDFLAGS/4+N
          REFLOG(NLOG)=REFVAL
        ENDIF
      ENDIF
      RETURN
      END
