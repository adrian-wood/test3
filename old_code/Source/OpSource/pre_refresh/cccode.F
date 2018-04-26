      SUBROUTINE CCCODE(DESCR,ICCCC,CCCC)

!-----------------------------------------------------------------------
!
! ROUTINE       : CCCODE
!
! PURPOSE       : to find the code figure corresponding to a CCCC
!                 (collecting centre) in BUFR code table 001031
!
! DESCRIPTION   : THE DATA READ IN CONSISTS OF:
!                 - THE NUMBER OF TABLES (NCODES)
!                 - AN INDEX ENTRY FOR EACH TABLE CONSISTING OF THE
!                   DESCRIPTOR WITH A POINTER TO THE COUNT BELOW
!                 - FOR EACH TABLE THE NUMBER OF CODE FIGURES DEFINED
!                   (1 OCTET) FOLLOWED BY THE DESCRIPTIONS EACH
!                   PRECEDED BY A 1-OCTET LENGTH.
!
! CALLS         : BUFRPATH (non-IBM only) - get location of BUFR TABLES
!               : READCF - to read the CODEFIG table
!
! CALLED BY     : VARIOUS
!
! ARGUMENTS     : (1) DESCRIPTOR (001031 FOR CCCC)           (INPUT)
!                 (2) ICCCC COLLECTING CENTRE CODE FIGURE    (OUTPUT)
!                 (3) CCCC  COLLECTING CENTRE IN CHARACTERS  (INPUT)
!                 RETURN WITH ICCCC=X'FFFF' (MISSING DATA TO GO IN
!                 BUFR MESSAGE) IF CCCC NOT FOUND.
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 02/04/2008 11:54:52$
! $Source: /data/us0400/mdb/op/lib/source/RCS/cccode.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         02/04/2008 11:54:52    Stan Kellett
!       increased MAXNBL from 40000 to 50000
!  1    Met_DB_Project 1.0         30/01/2006 20:21:39    Sheila Needham  
! $
! Revision 2.2  2003/03/06  09:18:21  09:18:21  usmdb (MetDB account c/o usjh)
! Calls READCF to read the editable BUFR table directly rather than
! the machinable version - S.Cox
! 
! Revision 2.1  2002/10/07  16:06:48  16:06:48  usmdb (MetDB account c/o usjh)
! 16 Sept 2002    C Long
! 2.1  Allow for new index format
! 
! Revision 2.0  2001/04/23 12:54:49  usmdb
! Added code to read environment variable BUFR_LIBRARY
! for BUFR table locations if compiled with the -DBPATH
! flag. Separated variable declaration and initialisation,
! changed obsolete DO loop structure, added copyright and
! modified header - S.Cox
!
! Revision 1.5  1998/06/11 15:32:56  usmdb
! bigger TEXT array & check to avoid overwriting
!
! Revision 1.4  97/07/31  09:25:02  09:25:02  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.3  1997/07/03 15:04:49  uspm
! Latest 1  version - Y2K check
!
! Revision 1.2  1997/06/05 15:09:47  uspm
! Add iostat=ierror to open, read statements
!
! 6/5/98 - bigger TEXT array & check to avoid overwriting             !a
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!-----------------------------------------------------------------------
! parameter statements.
! MAXNBL & MAXNXB may need increasing as more code table are added,
! but otherwise there is plenty of room for expansion:
!-----------------------------------------------------------------------

      INTEGER   MAXNBL    ! max size of TEXT array                  !2.2
      INTEGER   MAXNXB    ! max size of INDX array                  !2.2

      PARAMETER (MAXNBL=50000)                                      !ST2
      PARAMETER (MAXNXB=4200)                                       !2.2

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

      INTEGER   DESCR                ! argument (1)
      INTEGER   ICCCC                ! argument (2)
      INTEGER   NCODES               ! number of code tables
      INTEGER   NXBLOKS              ! number of index blocks
      INTEGER   NBYTES               ! number of data bytes to follow
      INTEGER   NBLOKS               ! number of data blocks to read
      INTEGER   NFIGS                ! number of code figures in table
      INTEGER   I                    ! short-term loop variable
      INTEGER   J                    ! shorter-term loop variable
      INTEGER   N                    ! pointer to description
      INTEGER   L                    ! length of description
      INTEGER   X                    ! XX from input descriptor
      INTEGER   Y                    ! YYY from input descriptor
      INTEGER   IERROR               ! IOSTAT from READ
      INTEGER   IOS                  ! IOSTAT from OPEN
      INTEGER   LEN_DIR_NAME         ! length of dir_name           !2.0

      LOGICAL   FEXIST               ! TRUE if CODEFIG exists       !2.0
      LOGICAL   INCORE               ! set if tables already read in

#if defined (BPATH)
      CHARACTER  DIR_NAME*200        ! BUFR tables directory path   !2.0
#endif
      CHARACTER*1 INDX(MAXNXB) ! index array of table entries       !2.2
      CHARACTER*1 TEXT(MAXNBL) ! data array of table entries        !2.2
      CHARACTER   CCCC*4             ! argument (3)
      CHARACTER   HEAD*132
      CHARACTER   FILENAME*208       ! CODEFIG full filename
                                     !  (*208 needed on HP)         !2.0

!-----------------------------------------------------------------------
! Common blocks (for dynamic allocation - compile with FPARMS='DC(*)').
!-----------------------------------------------------------------------

      COMMON /CCCCOM1/ INDX, TEXT                                   !2.2

!-----------------------------------------------------------------------
! SAVE all variables.
!-----------------------------------------------------------------------

      SAVE

!-----------------------------------------------------------------------
! data statements.
!-----------------------------------------------------------------------

      DATA    FILENAME/'CODEFIG'/                                   !2.0
      DATA    LEN_DIR_NAME/0/                                       !2.0
      DATA    INCORE/.FALSE./                                       !2.0

!-----------------------------------------------------------------------
! INCORE=.FALSE. 1st time into routine.
!-----------------------------------------------------------------------

      IF (.NOT.INCORE) THEN

        HEAD='$RCSfile: cccode.F,v $ ' //
     &       '$Revision: 2$ $Date: 02/04/2008 11:54:52$'

#if defined (BPATH)
        CALL BUFRPATH(DIR_NAME,LEN_DIR_NAME)                        !2.0
        FILENAME(1:LEN_DIR_NAME)=DIR_NAME(1:LEN_DIR_NAME)           !2.0
        FILENAME(LEN_DIR_NAME+1:LEN_DIR_NAME+7)='CODEFIG'           !2.0
#endif

        LEN_DIR_NAME=LEN_DIR_NAME+7                                 !2.0
        INQUIRE (FILE=FILENAME,EXIST=FEXIST)                        !2.0
        IF (.NOT.FEXIST) THEN                                       !2.0
          WRITE(6,*)'CCCODE: ERROR - File ',                        !2.0
     &    FILENAME(1:LEN_DIR_NAME),' not found'                     !2.0
          STOP                                                      !2.0
        ENDIF                                                       !2.0

!-----------------------------------------------------------------------
! Open CODEFIG dataset.
!-----------------------------------------------------------------------

#if defined (MVS)
        OPEN (81,FILE=FILENAME,FORM='FORMATTED',IOSTAT=IERROR,      !2.2
     &        ACTION='READ')
#else
        OPEN (81,FILE=FILENAME,FORM='FORMATTED',IOSTAT=IERROR)      !2.2
#endif

        IF (IERROR.NE.0) THEN                                       !2.0
          WRITE(6,*)'CCCODE: ERROR opening ',                       !2.0
     &    FILENAME(1:LEN_DIR_NAME),' IOSTAT = ',IERROR              !2.0
          STOP                                                      !2.0
        ENDIF                                                       !2.0

!-----------------------------------------------------------------------
! Call READCF to read the code/flag table into memory.
!-----------------------------------------------------------------------

        NCODES=MAXNXB                                               !2.2
        NBYTES=MAXNBL                                               !2.2

        CALL READCF(INDX,TEXT,NCODES,NBYTES)                        !2.2

        INCORE=.TRUE.
        CLOSE(81)

      ENDIF

!-----------------------------------------------------------------------
! Each index entry has 5 bytes.  The descriptor is in the first two.
! Look for a table for the input descriptor.  From the other 3 bytes
! set N to point to the start of the table (number of entries).
!-----------------------------------------------------------------------

      X=DESCR/256                 ! X to be found in index
      Y=MOD(DESCR,256)            ! Y to be found in index

      I=1
      DO WHILE (I.LT.5*NCODES .AND. .NOT.
     &  (MOD(ICHAR(INDX(I)),64).EQ.X .AND. ICHAR(INDX(I+1)).EQ.Y))
        I=I+5
      ENDDO
      IF (I.GT.5*NCODES) RETURN   ! return if descriptor not found

!-----------------------------------------------------------------------
! Get pointer from last 3 bytes of index entry
!-----------------------------------------------------------------------

      N=ICHAR(INDX(I+4))+ICHAR(INDX(I+3))*256
     &       +MOD(ICHAR(INDX(I+2)),128)*65536

      IF (N.GE.MAXNBL) THEN                                         !2.2
        PRINT *,'CODE:',X*1000+Y,'code table not read in - no room!'
        RETURN
      ENDIF

!-----------------------------------------------------------------------
! Loop round the descriptions until CCCC is found.
!-----------------------------------------------------------------------

      NFIGS=ICHAR(TEXT(N))        ! assume 1-byte figure count
      N=N+1                       ! past 1-byte count
      I=0                         ! initialise code figure for loop
      ICCCC=65535                 ! in case CCCC not found
      DO WHILE (ICCCC.EQ.65535 .AND. I.LT.NFIGS)
        IF (TEXT(N+1).EQ.CCCC(1:1) .AND.
     &      TEXT(N+2).EQ.CCCC(2:2) .AND.
     &      TEXT(N+3).EQ.CCCC(3:3) .AND.
     &      TEXT(N+4).EQ.CCCC(4:4)) ICCCC=I
        L=ICHAR(TEXT(N))          ! length of this description
        N=N+L                     ! move on to next code figure
        I=I+1
      ENDDO

      RETURN
      END
