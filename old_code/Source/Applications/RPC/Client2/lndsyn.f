!---------------------------------------------------------------------
!
! Example fortran source to call MDB remotely for subtype LNDSYN.
! This program outputs MetDB data to file "unit7".
!
! Important: The final call to MDB with ISTAT=99 must always be in
!            the fortran calling program.
!
! $Log:
!  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
!       Initial check-in of revised client software (previously under client
!       directory)
! $
c Revision 1.2  99/07/28  12:12:38  12:12:38  usmdb (Generic MDB account)
c Changed NOBS to 5000
c 
c Revision 1.1  99/03/15  11:13:42  11:13:42  usmdb (Generic MDB account)
c Initial revision
c 
! $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/lndsyn.f,v $
! $Revision: 1$
!
!---------------------------------------------------------------------

      PROGRAM LNDSYN
                                                                                
      PARAMETER      (IOBS=5000)                                                
      PARAMETER      (IELS=7)                                                  
                                                                                
      CHARACTER*8    CSUBT                                                      
      CHARACTER*500  CREQ                                                       
      REAL           ARRAY(IOBS,IELS)                                           
      INTEGER        NOBS                                                       
      INTEGER        NELEM                                                      
      INTEGER        ISTAT,TOT                                                  
      CHARACTER*1    CSTR(IOBS)                                                 
      CHARACTER*1    CREP(IOBS)                                                 
                                                                                
      CSUBT = 'LNDSYN  '                                                        
      
      CREQ  = 'START TIME TODAY-1/0000Z ' //
     &        'END TIME TODAY-1/0259Z ' //
     &        'ELEMENTS WMO_BLCK_NMBR WMO_STTN_NMBR LTTD LNGD ' //
     &        'STTN_HGHT SRFC_AIR_TMPR SRFC_DEW_PONT_TMPR '
    
      NOBS  = IOBS                                                        
      NELEM = IELS                                                              
      ISTAT = 0                                                                 
      TOT   = 0
                    
      OPEN(7,FILE='unit7',FORM='FORMATTED')
                                                                        
      DO WHILE (ISTAT.LE.4)                                                     
        WRITE(6,*)'LNDSYN: About to call MetDB'                                           

        CALL MDB(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,CSTR,CREP)                  

        WRITE(6,*)'LNDSYN: Back from MDB - ISTAT =',ISTAT
                                  
        IF (ISTAT.LE.4) THEN
          DO I=1,NOBS
            TOT=TOT+1
            WRITE(7,'(/1x,''Observation '',i6/)')I
            WRITE(7,'(6(1X,F12.2))')(ARRAY(I,J),J=1,NELEM)
          ENDDO                                                         
        ENDIF

        IF (ISTAT.EQ.0) GOTO 9999                                              
      ENDDO                                                                     
                                                                                
      WRITE(6,*)'LNDSYN: Error in MetDB, ISTAT = ',ISTAT                                                   
                                                                                
 9999 CONTINUE                                                                  

      WRITE(6,'(/''LNDSYN: About to Kill Server'')')
      
!---------------------------------------------------------
! ALWAYS KILL THE SERVER WHEN FINISHED !!!!!
!---------------------------------------------------------

      CALL MDB(CSUBT,CREQ,ARRAY,NOBS,NELEM,99,CSTR,CREP)                  
                                                                                 
      WRITE(6,'(/1X,''NOBS       = '',I5)')TOT                                  
      WRITE(6,'( 1X,''ISTAT      = '',I5)')ISTAT                                
      
      CLOSE(7)
                                                                                
      STOP                                                                      
      END                                                                       
                                        
