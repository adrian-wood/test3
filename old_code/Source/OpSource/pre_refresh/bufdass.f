      SUBROUTINE BUFDASS(STRING,NOBS,CMPRES,NASSOX,MASSOC,LASSOC,DSPLAY,
     &                   IBEFOR,VALUES,IVAL,DESCR,N,ND)
! ---------------------------------------------------------------------
!
! Program       : BUFDASS
!
! Called by     : BUFDATA
!
! Purpose       : to get the associated field(s) attached to an element
!  (informal)     in a BUFR message
!                 (only one value in the simplest case,
!                  more than one value if there are nested 204yyy
!                  operations and/or compressed data)
!
!  (formal)       Get NASSOX fields (lengths given in LASSOC) from after
!                 IBEFOR bits in STRING.  Put them (each preceded by the
!                 meaning given in MASSOC) in the VALUES array, starting
!                 at VALUES(IVAL).  For each field put 031021 and 000000
!                 in the DESCR array, before DESCR(N), adjusting ND for
!                 descriptors inserted.  (For CMPRESsed data there will
!                 be NOBS values of each field.)
!
! Calls         : VALUE (function) to get number from bit string
!                 BUFDSPL to display values
!
! Parameters    :
!  (1) STRING   bit string from BUFR message
!                (not changed)
!  (2) NOBS     number of reports in message, i.e. number of
!               values for each field if data is compressed
!                (not changed)
!  (3) CMPRES   flag set if data compressed
!                (not changed)
!  (4) NASSOX   number of fields attached to this element
!                (not changed; may be >1)
!  (5) MASSOC   meaning of each associated field
!                (not changed; array of dimension NASSOX)
!  (6) LASSOC   number of bits in each associated field
!                (not changed; array of dimension NASSOX)
!  (7) DSPLAY   flag set if values to be displayed
!                (not changed)
!  (8) IBEFOR   number of bits before value concerned
!                (updated by VALUE to go past this value)
!  (9) VALUES   array of values from BUFR message
!                (returned with field(s) set (NOBS fields if CMPRES),
!                 each added field preceded by meaning, 031021)
! (10) IVAL     subscript for value concerned in VALUE array
!                (incremented by 2*NOBS, 2* because 031021 too)
! (11) DESCR    descriptor array
!                (returned with 031021 & 000000 inserted
!                 for each associated field)
! (12) N        subscript of current descriptor
!                (031021 & 000000 will be inserted before this;
!                 N returned adjusted for insertions)
! (13) ND       total number of expanded descriptors
!                (returned adjusted for insertions)
!
! Error returns : none
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:11$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bufdass.f,v $
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
!
! ---------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER BONES(0:31)    ! values of 2**n-1
      LOGICAL CMPRES         ! argument (3)
      LOGICAL DSPLAY         ! argument (7)
      INTEGER DESCR(*)       ! argument (11)
      INTEGER I              ! short-term loop variable
      INTEGER IBEFOR         ! argument (8)
      INTEGER INC            ! increment (from STRING)
      INTEGER IVAL           ! argument (10)
      INTEGER IVALUE         ! integer value from bit string
      INTEGER LAS            ! current value of LASSOC
      INTEGER LASSOC(*)      ! argument (6)
      INTEGER MASSOC(*)      ! argument (5)
      INTEGER N              ! argument (12)
      INTEGER NAS            ! field number
      INTEGER NASSOX         ! argument (4)
      INTEGER NCREM          ! increment width (from STRING)
      INTEGER ND             ! argument (13)
      INTEGER NOBS           ! argument (2)
      INTEGER VALUE          ! function
      REAL    VALUES(*)      ! argument (9)
! DOUBLE PRECISION VALUES(*) ! argument (9) in REAL*8 version
      DOUBLE PRECISION V     ! rounding fix for *4 (not needed if *8)
      REAL    MISSING
      CHARACTER*(*) STRING   ! argument (1)
      CHARACTER NAME*60      ! for BUFDSPL
      CHARACTER UNITS*24     ! for BUFDSPL

      DATA MISSING/-9999999./
      DATA BONES/0,1,3,7,15,31,63,127,255,511,1023,2047,
     & 4095,8191,16383,32767,65535,131071,262143,524287,1048575,
     & 2097151,4194303,8388607,16777215,33554431,67108863,134217727,
     & 268435455,536870911,1073741823,2147483647/

      LOGICAL   HEADSET
      DATA      HEADSET/.FALSE./
      CHARACTER HEAD*132

      IF (.NOT.HEADSET) THEN
        HEAD='$RCSfile: bufdass.f,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:21:11$'
        HEADSET=.TRUE.
      ENDIF

! Put meaning in front of each nested associated field in values array.
! (The next value if data is compressed or NOBS=1; if NOBS>1 without
! compression, values in this subset are at intervals of NOBS to leave
! room for values from other subsets.)

      DO NAS=NASSOX,1,-1
        IF (CMPRES) THEN
          DO I=1,NOBS
            VALUES(IVAL)=MASSOC(NAS)
            IVAL=IVAL+1
          ENDDO
        ELSE
          VALUES(IVAL)=MASSOC(NAS)
          IVAL=IVAL+NOBS
        ENDIF

! Get associated field itself (It can be missing if more than 2 bits.
! A 2-bit field is interpreted as below: a value of 3 is not missing!)
! (If output array is REAL*4, go via REAL*8 as for value itself - but
! is this fix pointless for values which are probably small integers?
! Whereas if the field has >24 bits, then REAL*4 can't cope anyway!)

        LAS=LASSOC(NAS)
        IVALUE=VALUE(STRING,IBEFOR,LAS)
        IF (LAS.GT.2 .AND. IVALUE.EQ.BONES(LAS)) THEN
          V=MISSING
        ELSE
          V=DBLE(IVALUE)
        ENDIF
        VALUES(IVAL)=V

! If the data is compressed, get the increment width.  If it is non-
! zero, get the increments (missing if all ones & >2 bits as above),
! if not repeat the base value (V above) NOBS times.

        IF (CMPRES) THEN
          NCREM=VALUE(STRING,IBEFOR,6)
          DO I=1,NOBS
            IF (NCREM.GT.0) THEN
              INC=VALUE(STRING,IBEFOR,NCREM)
              IF (LAS.GT.2 .AND. INC.EQ.BONES(NCREM)) THEN
                VALUES(IVAL)=MISSING
              ELSE
                VALUES(IVAL)=V+DBLE(INC)
              ENDIF
            ELSE
              VALUES(IVAL)=V
            ENDIF
            IVAL=IVAL+1
          ENDDO
        ELSE
          IVAL=IVAL+NOBS
        ENDIF

! If the values are to be displayed, set the name from the value of
! 031021 (how to interpret values) & call the usual display program.

        IF (DSPLAY) THEN
          UNITS='added field'
          NAME=' '
          IF (MASSOC(NAS).EQ.1) THEN
            NAME='0 = good, 1 = suspect or bad'
          ELSE IF (MASSOC(NAS).EQ.2) THEN
            NAME='0 = good, 1 = suspect, 2 = very suspect, 3 = bad'
          ELSE IF (MASSOC(NAS).EQ.7) THEN
            NAME='% confidence'
          ELSE IF (MASSOC(NAS).EQ.21) THEN
            NAME='0 = original, 1 = corrected'
          ENDIF

          CALL BUFDSPL(NAME,UNITS,' ',0,LAS,NOBS,IVAL,NCREM,VALUES,0)
        ENDIF
      ENDDO

! Move remaining descriptors down to make room for two descriptors
! per nested field.

      DO I=ND,N,-1
        DESCR(I+2*NASSOX)=DESCR(I)
      ENDDO

! For each field insert 031021 for meaning & zero for flag itself.

      DO I=0,NASSOX-1
        DESCR(N+I*2)=31*256+21
        DESCR(N+I*2+1)=0
      ENDDO

! Add number of descriptors inserted to total & current subscript

      ND=ND+2*NASSOX
      N=N+2*NASSOX
      RETURN
      END
