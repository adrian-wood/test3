      SUBROUTINE DSOPEN (IUNIT, DDNAME, DIRECT, FORMAT, LENREC, IOPEN,
     &                                                          IOFLAG)

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE    : DSOPEN                                               
!                                                                      
! USAGE         : CALL DSOPEN (IUNIT, DDNAME, DIRECT, FORMAT, LENREC,  
!                                                      IOPEN, IOFLAG)  
! PURPOSE       : TO OPEN A DATA SET.                                  
!                                                                     
! PARAMETERS    : (ALL PARAMETERS ARE INPUT EXCEPT "IOFLAG")           
!                 IUNIT:  UNIT NUMBER FOR DATA SET.                    
!                 DDNAME: (CHARACTER*(*)) DDNAME FOR STORAGE DATA SET. 
!                 DIRECT: (LOGICAL) .TRUE. IF DIRECT ACCESS.           
!                 FORMAT: (LOGICAL) .TRUE. IF FORMATTED DATA.          
!                 LENREC: RECORD LENGTH.                               
!                 IOPEN:  INPUT FOR "ACTION" PARAMETER AS FOLLOWS:     
!                           1 - OPEN DATA SET FOR READ ONLY,           
!                           2 - OPEN DATA SET FOR WRITE ONLY,          
!                           3 - OPEN DATA SET FOR READ & WRITE.        
!                         (OTHER VALUES - SAME AS 1, I.E. READ ONLY.)  
!                 IOFLAG: STATUS FLAG FROM "OPEN" STATEMENT.           
!                                                                      
! CALLED BY     : DSINFO.                                              
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:08$
! $Source: /home/us0400/mdb/op/lib/source/RCS/dsopen.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:08    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:43  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  99/03/11  13:44:17  13:44:17  usmdb (Generic MetDB account)
! Initial revision
! 
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL. OPERATIONAL: MARCH 1999.  
!                                                                      
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

      IMPLICIT NONE
*                                                             VARIABLES
*
      INTEGER IOPEN           ! "OPEN" ACTION (1=READ, 2=WRITE, 3=BOTH)
      INTEGER IOFLAG          ! STATUS FLAG FROM "OPEN" STATEMENT
      INTEGER IUNIT           ! UNIT NUMBER FOR DATA SET
      INTEGER LENREC          ! RECORD LENGTH OF DATA SET
      LOGICAL DIRECT          ! FLAG FOR DIRECT/SEQUENTIAL ACCESS
      LOGICAL FORMAT          ! FLAG FOR FORMATTED/UNFORMATTED DATA
      CHARACTER*(*) DDNAME    ! DDNAME FOR STORAGE DATA SET
      CHARACTER*11 FORM       ! TEXT FOR "FORM" PARAMETER IN "OPEN"
      CHARACTER*9 ACTION      ! TEXT FOR "ACTION" PARAMETER IN "OPEN"
      CHARACTER*132 HEAD      ! FOR REVISION INFORMATION
*
*                                                  REVISION INFORMATION
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/dsopen.F,v $
     &'//'$Date: 30/01/2006 20:22:08$ $Revision: 1$'
*                                            FORMATTED OR UNFORMATTED ?
      IF (FORMAT) THEN
         FORM = 'FORMATTED  '
      ELSE
         FORM = 'UNFORMATTED'
      END IF
*                                                    READ/WRITE ACTIONS
      ACTION = 'READ' ! (DEFAULT)
      IF (IOPEN.EQ.3) THEN
         ACTION = 'READWRITE'
      ELSE IF (IOPEN.EQ.2) THEN
         ACTION = 'WRITE'
      END IF
*                                   OPEN DATA SET FOR DIRECT ACCESS ...
      IF (DIRECT) THEN
         OPEN (IUNIT, FILE=DDNAME, STATUS='OLD', ACCESS='DIRECT',
     &         FORM=FORM, RECL=LENREC, ACTION=ACTION, IOSTAT=IOFLAG)
*
*                            ... OR OPEN DATA SET FOR SEQUENTIAL ACCESS
      ELSE
         OPEN (IUNIT, FILE=DDNAME, STATUS='OLD', ACCESS='SEQUENTIAL',
     &         FORM=FORM, ACTION=ACTION, IOSTAT=IOFLAG)
      END IF
*                                             RETURN TO CALLING PROGRAM
      RETURN
      END
