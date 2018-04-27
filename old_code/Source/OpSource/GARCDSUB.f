!
!-----------------------------------------------------------------------
!
! PROGRAM     : GARCDSUB
!
! PURPOSE     : Generate JCL to archive Radar Doppler GRIB files
!               NOTE This is a test version only!!!! 
!
! DESCRIPTION : Copies and edits skeleton JCL, repeating the archive
!               request once for each file.
!               A fixed stream name is used for archiving GRIB files
!
! CALLS       : LENSTR,SUBMIT
!
! FILES USED  : Unit  6:  Output for messages
!               Unit 11:  Input for skeleton JCL
!               Unit 13:  Input for list of GRIB files to be archived
!
! HISTORY     : 17 Jul 2009  Original version               RJ Lavery
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 21/07/2010 11:42:58$
! $Author: Richard Weedon$
! $Folder: OpSource$
! $Workfile: GARCDSUB.f$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         21/07/2010 11:42:58    Richard Weedon  Test
!       version for radar doppler data
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2009 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

                                                                        00160000
      IMPLICIT NONE                                                     00170000
                                                                        00180000

!     Local Parameters:

      INTEGER UMG                  ! Output unit for messages
      PARAMETER(UMG=06)
      INTEGER USK                  ! Input unit for skel archive JCL
      PARAMETER(USK=11)
      INTEGER UAF                  ! Input unit for archive filenames
      PARAMETER(UAF=13)

      INTEGER MXFILE               ! Max number of archive files
      PARAMETER(MXFILE=100)
      INTEGER MAXLIN               ! Max lines of skel JCL
      PARAMETER(MAXLIN=30)
      INTEGER MAXJCL               ! Max lines of JCL for dimension
      PARAMETER(MAXJCL=300)
      INTEGER LIMJCL               ! Max lines of JCL for submission
      PARAMETER(LIMJCL=200)

      CHARACTER*6 GRBSTM           ! MASS stream name for GRIB files
      PARAMETER(GRBSTM='mdbe10')
      CHARACTER*9 REPSEC           ! Used for start/end of repeat sectn
      PARAMETER(REPSEC='- + - + -')


!     Local Scalars:

      CHARACTER*30 CARC(MXFILE)    ! Archive filename list
      CHARACTER*80 SLINE(MAXLIN)   ! Lines of static JCL (start of job)
      CHARACTER*80 ELINE(MAXLIN)   ! Lines of static JCL (end of job)
      CHARACTER*80 RLINE(MAXLIN)   ! Lines of repeatable JCL (middle)
      CHARACTER*80 JLINE(MAXJCL)   ! Lines of output JCL

      INTEGER LOCFLE               ! Char position of filename keyword
      INTEGER LOCSTM               ! Char position of stream keyword
      INTEGER NLS,NLE,NLR          ! Num of lines of JCL by section
      INTEGER NARC                 ! Num of files to be archived
      INTEGER NJCL                 ! Num of lines of JCL written
      LOGICAL START                ! TRUE for start of job
      LOGICAL REPT                 ! TRUE for repeatable JCL
      CHARACTER SKEL1*80           ! Single line of skel JCL
      CHARACTER CARC1*30           ! Single line of archive file list

      INTEGER LENNAM               ! Length of filename
      INTEGER LENSTR               ! Function to find length

      INTEGER IOS                  ! i/o status
      INTEGER IA,IR,IE,IS          ! loop counter
      INTEGER II                   ! list counter
      INTEGER N                    ! counter

!     IBM Intrinsic function:  INDEX
!     External function:       LENGTH

!     -----------------------------------------------------------------

      WRITE(UMG,'('' GARCSUB  Vsn 1.0  20 Jul 2009''/)')

!     Read and store the skeleton JCL in sections

      OPEN(USK,ACTION='READ',IOSTAT=IOS)

      START=.TRUE.
      REPT=.FALSE.
      NLS=0
      NLR=0
      NLE=0

      DO WHILE (IOS.EQ.0)

         READ(USK,'(A80)',IOSTAT=IOS) SKEL1

         IF(IOS.EQ.0) THEN

!     Detect repeatable JCL and switch sections for storage.
!                 Start    Middle     End
!     START FLAG   T         F         F
!     REPEAT FLAG  F         T         F

!     Do not need to save the repeat boundary lines themselves
           IF(INDEX(SKEL1,REPSEC).GT.0) THEN
             REPT=.NOT.REPT
             IF(START) START=.FALSE.
           ELSE

             IF(START) THEN
               NLS=NLS+1
               SLINE(NLS)=SKEL1
             ELSE IF(REPT) THEN
               NLR=NLR+1
               RLINE(NLR)=SKEL1
             ELSE
               NLE=NLE+1
               ELINE(NLE)=SKEL1
             END IF

           END IF

         ELSE IF(IOS.GT.0) THEN
           WRITE(UMG,'('' Error reading archive skel JCL'')')
           STOP 99
         END IF

      END DO
      CLOSE(USK)

      IF(START) THEN
        WRITE(UMG,'('' Error: Repeatable section of skeleton JCL '',
     &    ''not found on FT'',I2.2)') USK
        STOP 99
      END IF

      WRITE(UMG,'('' Start Section'',I5,'' lines'')') NLS
      WRITE(UMG,'('' Repeatable JCL'',I5,'' lines'')') NLR
      WRITE(UMG,'('' End Section'',I5,'' lines''/)') NLE


!     Read and store list of files to be archived

      OPEN(UAF,ACTION='READ',IOSTAT=IOS)

      NARC=0
      DO WHILE (IOS.EQ.0)

         READ(UAF,'(A30)',IOSTAT=IOS) CARC1

         IF(IOS.EQ.0) THEN
           NARC=NARC+1
           CARC(NARC)=CARC1

         ELSE IF(IOS.GT.0) THEN
           WRITE(UMG,'('' Error reading archive file list'')')
           STOP 99
         END IF

      END DO
      CLOSE(UAF)

!     Edit and copy lines of JCL into output array.

!     Start section:  substitute STREAM keyword if found (same length)
      N=0
      DO IS=1,NLS
         N=N+1
         JLINE(N)=SLINE(IS)
         LOCSTM=INDEX(SLINE(IS),'%STREM')
         IF(LOCSTM.GT.0) JLINE(N)(LOCSTM:LOCSTM+5)=GRBSTM
      END DO

!     Repeatable section - copy once per file to be archived.
!     Substitute filename for keyword and update archive step number
!     in cols 4-5 of the EXEC statement line.

      DO IA=1,NARC
         LENNAM=LENSTR(CARC(IA))
         WRITE(UMG,'(I5,1X,A30)') LENNAM,CARC(IA)

         DO IR=1,NLR
            N=N+1

            JLINE(N)=RLINE(IR)
            IF(INDEX(JLINE(N),'EXEC').GT.0)
     &        WRITE(JLINE(N)(4:5),'(I2.2)') IA

            LOCFLE=INDEX(RLINE(IR),'%ARCFLE')
            IF(LOCFLE.GT.0) THEN
              JLINE(N)(LOCFLE:LOCFLE+LENNAM-1)=CARC(IA)
              JLINE(N)(LOCFLE+LENNAM:)=RLINE(IR)(LOCFLE+7:)
            END IF
         END DO

      END DO

!     End Section:  copy as is
      DO IE=1,NLE
         N=N+1
         JLINE(N)=ELINE(IE)
      END DO

      NJCL=N

!     Submit the JCL generated
      IF(NJCL.GT.LIMJCL) THEN
        WRITE(UMG,'('' Exceeded lines limit - JCL not submitted'')')
        STOP 99
      ELSE
        CALL SUBMIT(JLINE,NJCL)
        WRITE(UMG,'(/I5,'' lines of JCL submitted'')') NJCL
      END IF

      STOP
      END
