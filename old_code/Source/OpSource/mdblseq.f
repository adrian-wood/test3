      SUBROUTINE MDBLSEQ(CSUBT,MATCH)

!-----------------------------------------------------------------------
!
! ROUTINE       : MDBLSEQ
!
! PURPOSE       : To match a subtype in file MDBLSEQ, and put the
!               : sequence into memory by calling LOCALD.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:16$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mdblseq.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:16    Sheila Needham  
! $
! Revision 2.0  2003/01/17 12:21:40  usmdb
! Various changes including environment variable read of
! METDB_MDBLSEQ - S.Cox
!
! Revision 1.1  2002/06/19  10:59:09  10:59:09  usmdb (MetDB account c/o usjh)
! Initial revision
! 
! Revision 1.1  2002/06/19 10:55:28  usmdb
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

      IMPLICIT NONE

      INTEGER         DUMMY(1)
      INTEGER         IREC
      INTEGER         LEV            !- Length of METDB_MDBLSEQ     !2.0
      INTEGER         RC             !- Return code                 !2.0
      LOGICAL         FEXIST         !- TRUE if file exists         !2.0
      LOGICAL         HEADSET        !- TRUE if HEAD set            !2.0
      LOGICAL         MATCH

      CHARACTER*(*)   CSUBT
      CHARACTER*8     SUBTYPE
      CHARACTER*132   HEAD           !- Revision info               !2.0
      CHARACTER*27998 MDBLOCD
      CHARACTER*200   METDB_MDBLSEQ  !- MDBLSEQ PATH                !2.0

      SAVE                                                          !2.0

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

      DATA HEADSET/.FALSE./                                         !2.0

      IREC=0
      SUBTYPE=' '
      MATCH=.FALSE.

      IF (.NOT.HEADSET) THEN                                        !2.0
        HEAD='$RCSfile: mdblseq.f,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:23:16$'
        HEADSET=.TRUE.                                              !2.0
      ENDIF                                                         !2.0

!-----------------------------------------------------------------------        
! get environment variable METDB_MDBLSEQ and find the length of it.
!-----------------------------------------------------------------------        

      CALL METDB_GETENV("METDB_MDBLSEQ",METDB_MDBLSEQ,RC)           !2.0
      IF (RC.NE.0) THEN                                             !2.0
        WRITE(6,*)'MDBLSEQ: ERROR: ENV VAR METDB_MDBLSEQ not set'   !2.0
        RETURN                                                      !2.0
      ENDIF                                                         !2.0
      LEV=LEN(METDB_MDBLSEQ)                                        !2.0
      DO WHILE (METDB_MDBLSEQ(LEV:LEV).EQ.' ')                      !2.0
        LEV=LEV-1                                                   !2.0
      ENDDO                                                         !2.0

!-----------------------------------------------------------------------        
! inquire to see if MDBLSEQ dataset exists
!-----------------------------------------------------------------------        

      INQUIRE (FILE=METDB_MDBLSEQ(1:LEV),EXIST=FEXIST)              !2.0
      IF (.NOT.FEXIST) THEN                                         !2.0
        WRITE(6,*)'MDBLSEQ: ERROR: File ',METDB_MDBLSEQ(1:LEV),     !2.0
     &            ' not found'                                      !2.0
        RETURN                                                      !2.0
      ENDIF                                                         !2.0

!-----------------------------------------------------------------------        
! open the MDBLSEQ dataset
!-----------------------------------------------------------------------        

      OPEN(81,FILE=METDB_MDBLSEQ(1:LEV),FORM='FORMATTED',
     &        ACCESS='DIRECT',RECL=30000,IOSTAT=RC)                 !2.0

      IF (RC.NE.0) THEN                                             !2.0
        WRITE(6,*)'MDBLSEQ: ERROR: Could not open file ',           !2.0
     &             METDB_MDBLSEQ(1:LEV)                             !2.0
        RETURN                                                      !2.0
      ENDIF                                                         !2.0

!-----------------------------------------------------------------------
! Loop over records in MDBLSEQ dataset until there are no more
! subtypes to read.
!-----------------------------------------------------------------------

      DO WHILE(SUBTYPE.NE.'LIST_END')

        IREC=IREC+1
        READ(81,'(A8,1X,A)',REC=IREC)SUBTYPE,MDBLOCD                !2.0

!-----------------------------------------------------------------------        
! If subtype matched then...
! If MATCH is TRUE then this is NOT the first sequence for this subtype
! so ADD it to LOCALD. Else MATCH is FALSE so this is the first
! sequence for this subtype so Call LOCALD with a mode of 'NEW' to
! start afresh.
!-----------------------------------------------------------------------        

        IF (SUBTYPE.EQ.CSUBT) THEN
          IF (MATCH) THEN                                           !2.0
            CALL LOCALD(0,0,DUMMY,0,MDBLOCD,'ADD')                  !2.0
          ELSE                                                      !2.0
            CALL LOCALD(0,0,DUMMY,0,MDBLOCD,'NEW')                  !2.0
          ENDIF                                                     !2.0
          MATCH=.TRUE.
        ENDIF
      ENDDO

      CLOSE(81)                                                     !2.0

      RETURN
      END
