SUBROUTINE BUFDSPL(NAME,UNITS,IFORMAT,SCALE,WIDTH,NOBS,IVAL,&
NCREM,VALUES,DESCR)
! ---------------------------------------------------------------------
!
! Program       : BUFDSPL
!
! Called by     : BUFDATA, BUFDASS
!
! Purpose       : to display values of the element just decoded
!  (informal)      (code/flag tables or numbers - NOT characters!)
!
!    (formal)     If there's only one value, print it after NAME etc.
!                 If there's more than one, print 6 per line, below
!                 the line with NAME etc.
!                 For numbers print SCALE figures after the decimal
!                 point or (if SCALE<0) a fractionless whole number
!                 - or print with an exponent if 11 figures too few.
!                    Look up the meaning of a code figure, but if
!                 none is found print the figure itself.
!
! Calls         : CODE to look up the meaning of a code figure
!
! Parameters    :
!  (1) NAME     element name from Table B                           (i)
!                (not changed)
!  (2) UNITS    element units from Table B                          (i)
!                (not changed)
!  (3) IFORMAT   'F' if flag table, 'C' if code table                (i)
!                (from our Table B, not changed)
!  (4) SCALE    from table B (perhaps modified),                    (i)
!               to decide how many figures to print after decimal point
!                (not changed)
!  (5) WIDTH    needed to interpret flag table values               (i)
!                (to number flags & know how many bits to print)
!                (not changed)
!  (6) NOBS     number of obs in message, hence number of values    (i)
!               of this element to display (if data compressed)
!                (not changed)
!  (7) IVAL     pointer to next (just updated!) slot in value array (i)
!                (not changed)
!  (8) NCREM    increment width (zero if all values same,           (i)
!               hence only one value needs to be displayed)
!                (not changed)
!  (9) VALUES   value array (one or NOBS values to be displayed)    (i)
!                (not changed)
! (10) DESCR    element descriptor (for CODE), passed as DESCR(N)   (i)
!                (not changed)
!
! Error returns : none
!
! REVISION INFO :
!
! $Workfile: bufdspl.f90$ $Folder: OpSource$
! $Revision: 10$ $Date: 21/02/2011 11:30:16$
!
! CHANGE RECORD :
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
! ---------------------------------------------------------------------

USE code_mod

IMPLICIT  NONE

REAL,INTENT(IN)              :: VALUES(*) ! argument (9)
CHARACTER(LEN=54),INTENT(IN) :: NAME      ! from Table B - argument (1)
CHARACTER(LEN=12),INTENT(IN) :: UNITS     ! from Table B - argument (2)
CHARACTER(LEN=1),INTENT(IN)  :: IFORMAT   ! from our Table B - arg (3)
INTEGER,INTENT(IN)           :: NOBS      ! argument (6)
INTEGER,INTENT(IN)           :: IVAL      ! argument (7)
INTEGER,INTENT(IN)           :: NCREM     ! argument (8)
INTEGER,INTENT(IN)           :: DESCR     ! argument (10)
INTEGER,INTENT(IN)           :: WIDTH     ! from Table B - argument (5)
!
REAL                         :: MISSING
INTEGER                      :: SCALE     !from Table B - argument (4)
INTEGER                      :: LEFT      !number left print (at 6/line)
INTEGER                      :: LINE      !val this line (MIN(6,LEFT))
INTEGER                      :: I,J       !loop variables
INTEGER                      :: ISTART    !subscript for this line
INTEGER                      :: ICODEF    !code fig value for CODE call
INTEGER                      :: NWORDS    !per line (1 or LINE if comp)
INTEGER                      :: WID       ! MIN(WIDTH,24)
INTEGER                      :: IV        !val for div by 2 to set bits
!
LOGICAL                      :: LINONE    !true if first line
!
CHARACTER(LEN=12)            :: WORD(6)   !to display codes for 6 values
CHARACTER(LEN=12)            :: TEXT      !to keep text to edit WORD(I)
CHARACTER(LEN=13)            :: BITS(6)   !to display bits for 6 values
CHARACTER(LEN=8)             :: FORM(10)  !FORMat(N)N fig in fraction
CHARACTER(LEN=6)             :: FXXYYY    !to print desc as 6 figures

DATA MISSING/-9999999./

DATA FORM/'(F12.1)','(F12.2)','(F12.3)','(F12.4)','(F12.5)',&
     &'(F12.6)','(F12.7)','(F12.8)','(F12.9)','(F12.10)'/


LEFT=NOBS
LINONE=.TRUE.
ISTART=IVAL-NOBS
DO_LOOP1: &
DO WHILE (LEFT > 0)

! From number of values left get number on this line (six per line).
! (LEFT starts at NOBS & is decremented by 6 for every line printed.)

  LINE=6
  IF (LEFT < 6) LINE=LEFT

! Find how many to print on this line
! (more than 1 if data is compressed and NOBS>1)

  IF ((NOBS > 1 .AND. NCREM > 0) .OR. LINONE) THEN
    NWORDS=LINE
    IF (NCREM == 0) NWORDS=1
  END IF
  WRITE (FXXYYY,'(I6.6)') (DESCR/256)*1000+MOD(DESCR,256)

! --------------------------- CODE FIGURE -----------------------------
! If the element is a code figure, look it up in the tables.
! (Don't call CODE more than once if all the figures are the same!)
! ---------------------------------------------------------------------

  IF_CONST1: &
  IF (IFORMAT == 'C') THEN

! Look up each code figure to go on this line in the tables.
! If no description is returned, display the code figure itself.

    DO_LOOP2: &
    DO I=1,NWORDS
      IF_CONST2: &
      IF (VALUES(ISTART+I-1) == MISSING) THEN
        WORD(I)='   - - - - -'
      ELSE
        ICODEF=VALUES(ISTART+I-1)
        WORD(I)=' '
        IF (ICODEF >= 0) CALL CODE(DESCR,ICODEF,WORD(I))
        IF (WORD(I) == ' ') WRITE (WORD(I)(6:12),'(I7)') ICODEF

! For 020003 (present weather) start display with code figure
! (unless description is short, 8 characters or less)
! because starts of descriptions are not unique.

        IF (DESCR == 20*256+3) THEN
          IF (WORD(I)(1:4) /= '    ') THEN
            TEXT=WORD(I)
            IF (ICODEF < 10) THEN
              WRITE(WORD(I),'(I1,1X,A10)')ICODEF,TEXT(1:10)
            ELSE IF (ICODEF < 100) THEN
              WRITE (WORD(I),'(I2,1X,A9)') ICODEF,TEXT(1:9)
            ELSE
              WRITE (WORD(I),'(I3,1X,A8)') ICODEF,TEXT(1:8)
            END IF
          ELSE
            WRITE (WORD(I)(1:3),'(I3)') ICODEF
          END IF
        END IF
      END IF IF_CONST2
    END DO DO_LOOP2

! Figure(s) looked up & ready for printing: now print descriptions
! (only one per line if NOBS=1 or no compression)

    IF (NOBS == 1 .OR. NCREM == 0) THEN
      IF (LINONE) PRINT *,NAME,'Code ',FXXYYY,' ',WORD(1)
    ELSE
      IF (LINONE) PRINT *,NAME,'Code ',FXXYYY
      PRINT *,(WORD(I),I=1,LINE)
    END IF

! --------------------------- FLAG TABLE ------------------------------
! If it's a flag table rather than a single code figure, split the
! value into separate flags & print the flag combination(s) as bits.
! Look up the bits from right to left, skipping the right-hand bit,
! which is only set for missing data.
! ---------------------------------------------------------------------

! To display each flag set (if NOBS=1 or all values the same)
! take the number representing the set of flags and divide
! repeatedly by 2, putting 1 in the string if the result is odd,
! i.e. the lowest-order bit in what's left is set to 1.

  ELSE IF (IFORMAT == 'F') THEN IF_CONST1
    IF (LINONE .AND. WIDTH > 24) PRINT *,WIDTH,'-bit flag ',   &
     & 'table: only first 24 flags displayed'
    WID=MIN(WIDTH,24)

    DO I=1,NWORDS
      IF (VALUES(ISTART+I-1) == MISSING) THEN
        BITS(I)='  - - - - - - - - - - - - - - -'
      ELSE
        BITS(I)(1:WID)='..............................'
        IV=VALUES(ISTART+I-1)       ! value representing bits
        DO J=1,WID                  ! loop round bits
          IF (MOD(IV,2) == 1) BITS(I)(WID-J+1:WID-J+1)='1'
          IV=IV/2                   ! lose another bit
        END DO
      END IF
    END DO

! If flag table values vary (more than one & not all the same), then
! print WID characters for each value, dots where bits not set, ones
! where set.  Up to 24 flags displayed, so print 3 tables per line.

! Don't print anything for bit map (printed by BUFDMAP).

! (Initialise a string with a character for each bit to dots, then
! overwrite the dots corresponding to bits which are set with ones,
! repeatedly dividing by 2 & checking for an odd result as above.)

    IF (NOBS == 1 .OR. NCREM == 0) THEN
      IF (LINONE .AND. .NOT.(FXXYYY == '031031')) THEN
        PRINT *,NAME,'Flag ',FXXYYY,' ',BITS(1)(1:WID)
      END IF
    ELSE
      IF (LINONE) PRINT *,NAME,'Flag ',FXXYYY
      PRINT *,(BITS(I)(1:WID),'  ',I=1,MIN(3,LINE))
      IF (LINE > 3) THEN
        PRINT *,(BITS(I)(1:WID),'  ',I=4,MIN(6,LINE))
      END IF
    END IF

! ----------------------------- NUMBER --------------------------------
! If whole number, first convert to a character string ending with a
! point (F12.0), then output the figures omitting the point.
! Numbers with negative scales can take values not displayable in 11
! figures: print such values with an exponent.  (1P puts one figure
! before the decimal point.)
! ---------------------------------------------------------------------

  ELSE IF_CONST1
    IF_CONST3: &
    IF (SCALE <= 0) THEN
      DO I=1,LINE
        IF (VALUES(ISTART+I-1) == MISSING) THEN
          WORD(I)='  - - - - - '
        ELSE IF (VALUES(ISTART+I-1) > 99999999999.) THEN
          WRITE (WORD(I),'(1P,E11.5)') VALUES(ISTART+I-1)
        ELSE
          WRITE (WORD(I),'(F12.0)') VALUES(ISTART+I-1)
        END IF
      END DO

! If there's only one value to print, put name & units on same line.
! Otherwise print name & units on one line, then values 6 to a line.

      IF (NOBS == 1 .OR. NCREM == 0) THEN
        IF (LINONE) PRINT *,NAME,UNITS,' ',WORD(1)(1:11)
      ELSE
        IF (LINONE) PRINT *,NAME,UNITS
        PRINT *,(WORD(I)(1:11),I=1,LINE)
      END IF

! If scale is positive, print SCALE figures after the decimal point.
! Either one value on the same line as the name & units:

    ELSE IF (SCALE > 0) THEN IF_CONST3
      IF (NOBS == 1 .OR. NCREM == 0) THEN
        IF (LINONE) THEN
          IF (VALUES(ISTART) == MISSING) THEN
            WORD(1)='   - - - - -'
            PRINT *,NAME,UNITS,WORD(1)
          ELSE
            IF (SCALE <= 10) THEN
              WRITE (WORD(1),FORM(SCALE)) VALUES(ISTART)
              PRINT *,NAME,UNITS,WORD(1)
            ELSE
              PRINT *,NAME,UNITS,' ',VALUES(ISTART)
            END IF
          END IF
        END IF
      ELSE

! Or name & units, then six values per line:
! (But can't cope with SCALE>10 with 6 values per line!)

        IF (LINONE) PRINT *,NAME,UNITS
        DO I=1,LINE
          IF (VALUES(ISTART+I-1) == MISSING) THEN
            WORD(I)='  - - - - - '
          ELSE
            IF (SCALE <= 10) THEN
              WRITE (WORD(I),FORM(SCALE)) VALUES(ISTART+I-1)
            ELSE
              PRINT *,SCALE,'is scale; only 10 figures shown'
              WRITE (WORD(I),FORM(10)) VALUES(ISTART+I-1)
            END IF
          END IF
        END DO
        PRINT *,(WORD(I),I=1,LINE)
      END IF
    END IF IF_CONST3
  END IF IF_CONST1

  LEFT=LEFT-6
  ISTART=ISTART+6
  LINONE=.FALSE.
END DO DO_LOOP1
RETURN
END SUBROUTINE BUFDSPL
