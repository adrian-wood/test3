      SUBROUTINE BUFREX(CMESS,BMESS,IRC)

!-----------------------------------------------------------------------
!
! PROGRAM       : BUFREX  To make a BUFR message from a CREX message
!
! PURPOSE       : Decode the CREX message, convert any values of
!                 elements whose BUFR & CREX units are different,
!                 and re-encode in BUFR.
!
! ASSUMPTIONS   : N-th value corresponds to N-th descriptor in output
!                 from CREX.
!                 Delayed replication 031001 may not have enough bits
!                 for CREX count, so always use 031002 in BUFR.
!                 First date/time from CREX message goes in BUFR sect 1.
!                 (MAXVAL & length of CHARS may need increasing.)
!
! CALLS         : CREX to decode the CREX message
!                 IVALUE to get data type
!                 DESFXY to convert descriptor to F, X & Y
!                 ENBUFV2 to re-encode in BUFR
!
! PARAMETERS    : (1) CREX message (input)
!                 (2) BUFR message (output)
!                 (3) return code
!                       (input: 0 for first report, 4 for more)
!                       output: 0 if no more, 4 if more to come)
!
! REVISION INFO :
!
! CHANGE RECORD :
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER MAXVAL
      PARAMETER (MAXVAL=999)
      REAL    OUT(MAXVAL)    ! values
      INTEGER DESC(MAXVAL)   ! descriptors
      INTEGER DATIME(5)      ! date/time to go in section 1
      INTEGER DATYPE         ! data type from Aaaa (as in Table A)
      INTEGER IXA            ! pointer to Aaaa
      INTEGER IVALUE         ! function to convert figures to number
      INTEGER F,X,Y          ! from descriptor FXXYYY
      INTEGER IRC            ! return code (see above)
      INTEGER I,J            ! loop variables
      INTEGER L              ! length of BUFR message
      INTEGER ND             ! number of descriptors
      INTEGER NOB            ! number of obs to go in BUFR message
      INTEGER NELEM          ! number of elements for ENBUFV2 call
                             !  (only needs to be big enough)
      CHARACTER CHARS*222    ! any character values
      CHARACTER CMESS*(*)    ! input CREX message
      CHARACTER BMESS*(*)    ! output BUFR message

! Decode one report from the CREX message (no display).

      ND=MAXVAL
      CALL CREX(CMESS,.FALSE.,MAXVAL,MAXVAL,ND,IRC,DESC,OUT,CHARS)

! Find data type in Aaaa group (CREX Table A is same as BUFR)

      IXA=INDEX(CMESS,' A')
      DATYPE=IVALUE(CMESS(IXA+2:IXA+4))

! Use expanded descriptor list to see if conversions are needed:
! for any temperature (assuming the few CREX entries with units=K
!  are for satellite data & therefore not used)
! and for 5 other elements (in Oct 2003).
! Loop backwards to set DATIME from first year etc.

      DO I=ND,1,-1
        CALL DESFXY(DESC(I),F,X,Y)
        IF (F.EQ.0 .AND. OUT(I).NE.-9999999.) THEN
          IF (X.EQ.12) THEN                    ! 012yyy:
            OUT(I)=OUT(I)+273.15               ! C to K

          ELSE IF (X.EQ.2 .AND. Y.EQ.168) THEN ! 002168:
            OUT(I)=OUT(I)*1000.                ! kPa to Pa
          ELSE IF (X.EQ.7 .AND. Y.EQ.10) THEN  ! 007010:
            OUT(I)=OUT(I)*0.3048               ! feet to metres
          ELSE IF (X.EQ.13 .AND. Y.EQ.55) THEN ! 013055:
            OUT(I)=OUT(I)/3600.                ! mm/hour to kg/m**2/sec
          ELSE IF (X.EQ.13 .AND. Y.EQ.58) THEN ! 013058:
            OUT(I)=OUT(I)*1000.                ! mm to m
          ELSE IF (X.EQ.15 .AND. Y.EQ.3) THEN  ! 015003:
            OUT(I)=OUT(I)/10000.               ! nbar to Pa

! For characters replace pointer combined with length by pointer alone.
! (Decode outputs combination, encode only wants pointer.)

          ELSE IF (DESC(I)/131072.EQ.1) THEN   ! character value
            OUT(I)=MOD(IFIX(OUT(I)),65536)     ! remove string length
          ENDIF

! Set DATIME from first year etc in message
! (If this descriptor is a time element (X=4 & Y<6), set corresponding
! value in appropriate DATIME element.)

          IF (X.EQ.4 .AND. Y.LE.5) DATIME(Y)=OUT(I)
        ENDIF
      ENDDO

! The above loop was round the expanded descriptors, in a one-to-one
! correspondence with the decoded values.  The descriptors to go in
! the BUFR message are the unexpanded sequence returned by the CREX
! decode, at the END of the descriptor array: insert 031002 after any
! delayed replication operator.  (031001 may not have enough bits.)
! (Use 2 subscripts in case 031002 has to be inserted!)

      I=0                           ! subscript in BUFR sequence
      J=0                           ! subscript in CREX sequence
      DO WHILE (DESC(MAXVAL-J).NE.0)
        DESC(I+1)=DESC(MAXVAL-J)
        I=I+1
        J=J+1
        CALL DESFXY(DESC(I),F,X,Y)

        IF (F.EQ.1 .AND. Y.EQ.0) THEN
          DESC(I+1)=31*256+2
          I=I+1
        ENDIF
      ENDDO
      ND=I

! Finally re-encode in BUFR (data type from CREX message, edition 3,
! UKMO (74) as originating centre, default table version).

      NELEM=MAXVAL
      NOB=1
      CALL ENBUFV2(DESC,OUT,ND,NELEM,NOB,CHARS,DATIME,BMESS,.FALSE.,
     &             L,3,0,74,DATYPE,0,-99,0,.FALSE.,' ',.FALSE.,' ',1)
      RETURN
      END
