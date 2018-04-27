SUBROUTINE DECORD(DESCR,VALUES,NAMES,ND,NOBS,STRING,DSPLAY, &
                  LIST,INLIST)

!-----------------------------------------------------------------------
!
! PROGRAM       : DECORD
!
! PURPOSE       : TO DECODE START OF BUFR MESSAGE
!
! DESCRIPTION   : THE DESCRIPTORS IN SECTION 3, WITH ANY SEQUENCES
!                 EXPANDED, ARE EXAMINED UNTIL ALL THE (COORDINATE)
!                 DESCRIPTORS IN AN INPUT LIST HAVE BEEN FOUND
!                 AT LEAST ONCE.
!                 DECODE IS THEN CALLED TO GO SO FAR BUT NO FURTHER.
!
! CALLED BY     : GETVALS, MDBSTOR
!
! CALLS         : SCRIPT, BUFDATA, DESFXY, VALUE
!
! ARGUMENTS     : (1) SEQUENCE OF DESCRIPTORS        (TO BE RETURNED)
!                 (2) ARRAY FOR VALUES               (TO BE RETURNED)
!                 (3) FOR ANY CHARACTER VALUES       (TO BE RETURNED)
!                 (4) NUMBER OF DESCRIPTORS          (TO BE RETURNED)
!                      (PASSED AS LENGTH OF DESCRIPTOR ARRAY)         A
!                 (5) NUMBER OF REPORTS              (TO BE RETURNED)
!                      (PASSED AS LENGTH OF VALUE ARRAY)              A
!                 (6) BUFR MESSAGE
!                 (7) FLAG SET IF DISPLAY OF VALUES REQUIRED
!                 (8) LIST OF DESCRIPTORS TO BE FOUND AT START
!                 (9) NUMBER OF DESCRIPTORS IN LIST AT (8)
!
! REVISION INFO :
!
! $Workfile: decord.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 25/01/2011 21:46:44$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         25/01/2011 21:46:44    Sheila Needham
!       Updated following review
!  2    MetDB_Refresh 1.1         25/01/2011 15:12:42    John Norton     After
!       initial porting of batches MDBSTOR 19 & 23
!  1    MetDB_Refresh 1.0         19/01/2011 12:17:56    John Norton
!       Pre-porting f77 code for MDBSTOR batch 23.
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE bufdata_mod
USE desfxy_mod
USE script_mod
USE value_mod ! function

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(INOUT) :: DESCR(:)     !a01
REAL,             INTENT(INOUT) :: VALUES(:)    !a02
CHARACTER(LEN=*), INTENT(OUT)   :: NAMES        !a03
INTEGER,          INTENT(INOUT) :: ND           !a04 Number of descriptors in BUFR sequence !2.3
INTEGER,          INTENT(INOUT) :: NOBS         !a05
CHARACTER(LEN=*), INTENT(IN)    :: STRING       !a06
LOGICAL,          INTENT(IN)    :: DSPLAY       !a07
INTEGER,          INTENT(IN)    :: INLIST       !a09
INTEGER,          INTENT(IN)    :: LIST(INLIST) !a08 List of descriptors to look for

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  F ! F part of descriptor
INTEGER          ::  FLOMP ! Flag for compressed data (BUFR section 1)
INTEGER          ::  FLOPT ! Flag for optional BUFR section 2
INTEGER          ::  I !?
INTEGER          ::  IBEFOR !?
INTEGER          ::  ID !?
INTEGER          ::  IDE !?
INTEGER          ::  IDES !?
INTEGER          ::  IVER ! New dummy argument for call to BUFDATA
INTEGER          ::  J !?
INTEGER          ::  L1 !?
INTEGER          ::  L2 !?
INTEGER          ::  L3 !?
INTEGER          ::  LMAX ! Max pos. in STRING to check for 'BUFR'
INTEGER          ::  MAXDES !?
INTEGER          ::  MAXVAL !?
INTEGER          ::  N !?
INTEGER          ::  NEND ! Number of descriptors to decode
INTEGER          ::  NUMBUF !?
INTEGER          ::  RC ! Return code from BUFDATA
INTEGER          ::  X ! X part of descriptor
INTEGER          ::  Y ! Y part of descriptor

LOGICAL          ::  CMPRES

CHARACTER(LEN=4) ::  BUFR

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! SAVE all variables.
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! Initialise variables.
!-----------------------------------------------------------------------

BUFR=CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82)
MAXDES=ND
MAXVAL=NOBS

!-----------------------------------------------------------------------
! Find 'BUFR' (IN ASCII) at start of STRING.
!-----------------------------------------------------------------------

LMAX=MIN(100,LEN(STRING))

N=1
DO WHILE (STRING(N:N+3) /= BUFR .AND. N < LMAX)
  N=N+1
END DO

IF (N == LMAX) THEN
  PRINT *,' BUFR NOT FOUND'
  NOBS=0
  RETURN
ELSE
  N=N+4                    ! move past BUFR
END IF

IBEFOR = 0
NUMBUF = VALUE(STRING(N+3:N+3),IBEFOR,8)  ! BUFR edition no.
IF (NUMBUF >= 2) N=N+4

!                         SKIP SECTION 1 (ANY OPTIONAL SECTION?)
IBEFOR = 0
L1 = VALUE(STRING(N:N+2),IBEFOR,24)     ! Section 1 length
IF (NUMBUF < 4) THEN
  IBEFOR = 56        ! Edtns. 0-3: flag in byte 8
ELSE
  IBEFOR = 72        ! Edtn. 4: flag in byte 10
END IF
FLOPT = VALUE(STRING(N:N+9),IBEFOR,1)
N=N+L1
!                         IF THERE'S A SECTION 2, SKIP IT.
IF (FLOPT == 1) THEN
  IBEFOR = 0
  L2 = VALUE(STRING(N:N+2),IBEFOR,24)   ! Section 2 length
  N=N+L2
END IF
!                         FIND NO. OF REPORTS & COMPRESSION FLAG
IBEFOR = 0
L3 = VALUE(STRING(N:N+2),IBEFOR,24)   ! Section 3 length
IBEFOR = 32                           ! Skip octets 1-4
NOBS = VALUE(STRING(N:N+6),IBEFOR,16) ! NOBS in octets 5-6
FLOMP = VALUE(STRING(N:N+6),IBEFOR,8) ! Flags in octet 7

IF (MOD(FLOMP,128) >= 64) THEN
  CMPRES=.TRUE.
ELSE
  CMPRES=.FALSE.
END IF

!-----------------------------------------------------------------------
! WE HAVE NOW REACHED THE DESCRIPTORS IN SECTION 3.  COPY THEM TO A
! FULLWORD ARRAY, CALL SCRIPT TO EXPAND ANY SEQUENCES, AND LOOK FOR
! THE DESCRIPTORS IN THE INPUT LIST, STOPPING AT THE FIRST INSTANCE
! OF EACH, AND KEEPING THE HIGHEST OF THOSE FIRST SUBSCRIPTS.
! NOTE: IF THERE IS MORE THAT ONE OBSERVATION AND THE MESSAGE IS
! NOT COMPRESSED, THERE IS NO ALTERNATIVE TO DOING A FULL DECODE
! BECAUSE IT IS NECESSARY TO GET TO THE END OF EACH OBSERVATION IN
! ORDER TO FIND THE START OF THE NEXT ONE.
!-----------------------------------------------------------------------

ND=(L3-7)/2
!                         COPY THE DESCRIPTORS TO FULLWORDS
DO I=1,ND
  ID=N+7+(I-1)*2
  IBEFOR = 0
  DESCR(I) = VALUE(STRING(ID:ID+1),IBEFOR,16)
END DO
!                         CHECK FOR NEED TO DO FULL DECODE
IFLABEL1: &
IF (.NOT.CMPRES .AND. NOBS > 1) THEN
  NEND = ND
!                         PARTIAL DECODE: SEARCH THROUGH DESCRIPTORS
ELSE
!                         EXPAND ANY TABLE D SEQUENCES
  CALL SCRIPT(DESCR,ND,.FALSE.)

!                         SET NEND TO THE HIGHEST REQUIRED DESCRIPTOR
  NEND=0
DoInlist: &
  DO J=1,INLIST
    DO I=1,ND
      IF (DESCR(I) == LIST(J)) THEN
        IF (I > NEND) NEND=I
        CYCLE DoInlist
      END IF
    END DO
  END DO DoInlist

! F=3 descriptors should all have been expanded out by SCRIPT.
! Replications should also have been expanded out unless there was
! delayed replication. To prevent replications being expanded out a
! second time by BUFDATA below, replication descriptors (F=1) are
! here replaced by the harmless 202000 if the replication was not
! delayed (i.e. Y>0).

  DO I=1,NEND
    IF (DESCR(I)/16384 == 1 .AND. MOD(DESCR(I),256) > 0) &
        DESCR(I) = IDES(202000)
  END DO
END IF IFLABEL1

!-----------------------------------------------------------------------
! WE NOW HAVE A SEQUENCE OF DESCRIPTORS TO DECODE COORDINATES ONLY
!-----------------------------------------------------------------------

N=N+L3
CALL BUFDATA(DESCR,VALUES,NAMES,NEND,NOBS,STRING(N:), &
             CMPRES,DSPLAY,MAXDES,MAXVAL,IVER,RC)

IF (RC == 1) THEN
  CALL DESFXY(DESCR(NEND),F,X,Y)
  IDE=F*100000+X*1000+Y
  PRINT *,' ERROR',NEND,'-TH DESCRIPTOR IS',IDE
  NOBS=0
ELSE
  ND=NEND           ! RETURN NUMBER OF ELEMENTS DECODED
END IF

!-----------------------------------------------------------------------
! return to calling program.
!-----------------------------------------------------------------------

RETURN
END SUBROUTINE DECORD
