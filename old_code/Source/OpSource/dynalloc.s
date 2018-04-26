DYNALLOC CSECT                                                          
         ENTRY DYNALC                                              D            
DYNALC   EQU   *                                                                
         CHAIN ,,*,BASE=(13,12)                                        
*********************************************************************** 
*                                                                     * 
* PROGRAM       : DYNALLOC IN RTCURRET                                * 
*                                                                     * 
* PURPOSE       :DYNAMICALLY ALLOCATE DATASETS                        * 
*                                                                     * 
* DESCRIPTION   :                                                     * 
*                                                                     * 
*                                                                     * 
* CALLED BY     : ........ IN YLOAD.                                  * 
*                                                                     * 
* CALLS         : ........ IN YLOAD.                                  * 
*                                                                     * 
* PARAMETERS    : (1)                                                 * 
*                 (2)                                                 * 
*                 (3)                                                 * 
*                                                                     * 
*Y2K  16.06.1997  DYNALLOC IS YEAR 2000 COMPLIANT.                              
*                                                                     * 
* CHANGE RECORD :           INTRODUCED  SEPTEMBER 1983                * 
*   DATE :-        PURPOSE:-                                          * 
*   NOVEMBER 1983  CHECK ON FIRST LETTER OF DD  NAME                  * 
*                  IF X THEN SYSTEM PROVIDES DDNAME                   * 
*                  OTHERWISE DDNAME PROVIDED IS USED FOR ALLOCATION   * 
*  FEBRUARY 1984   R15 USED FOR RETURN CODE                           * 
*               ABENDS FOR FAILURES IN ALLOCATION REMOVED FROM ROUTINE* 
*  JULY  1986      FREEMAIN INTRODUCED                             A  * 
*  OCTOBER 1986    REINTRODUCE ABENDS WHEN ALLOCATIONS FAIL         B * 
*                  S99NOCNV INTRODUCED TO AVOID USING EXISTING        * 
*                  ALLOCATIONS                                      B * 
*                                                                     * 
*  NOVEMBER 1986  REGS 3-5 USED AT ABEND 555 FOR ADDITIONAL INFO.   C * 
*  6/9/90    DYNALC ENTRY POINT FOR FORTRAN                         D *         
*                                                                     * 
*********************************************************************** 
*$Revision: 1$
*$Date: 30/01/2006 20:22:10$
*
***********************************************************************
*                                                                       
         EQUREG R                                                       
* CHANGE RECORD                                                         
*                                                                       
*  OF THE DD NAME IF THIS IS X THEN SYSTEM PROVIDES DDNAME              
*  OTHERWISE THE DNAME PROVIDED IS USED BY THE ALLOCATION ROUTINE       
*                                                                       
* FEBRUARY 1984 R15 USED FOR RETURN CODE TO CALLING PROGRAM             
*                                                                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
*                                                                       
         L     R2,0(R1)       ADDRESS OF DSNAME                         
         L     R10,4(R1)      ADDRESS OF LENGTH OF DSNAME               
         L     R9,8(R1)       ADDRESS OF VOLSER                         
         L     R11,12(R1)    DCB ADDRESS FOR DDNAME RETURNED            
*                                                                     * 
         LA    R0,128              AMOUNT OF STORAGE REQUIRED           
         GETMAIN R,LV=(R0)        GET STORAGE NECESSARY FOR REQUEST     
         LR    R8,R1               SAVE ADDRESS OF STORAGE AREA         
         USING S99RBP,R8           ESTABLISH ADDRESS ABILITY FOR 'RBPTR 
*                                  DSECT                                
         LA    R4,S99RBPTR+4       POINT 4 BYTES BEYOND START OF 'RBPTR 
         USING S99RB,R4            ESTABLISH ADDRESSABILITY FOR 'RB'    
*                                  DSECT                                
         ST    R4,S99RBPTR         MAKE 'RBPTR'POINT TO 'RB'            
         OI    S99RBPTR,S99RBPND   TURN ON HIGH ORDER BIT IN 'RBPTR'    
         XC    S99RB(RBLEN),S99RB  ZERO OUT 'RB' ENTIRELY               
         MVI   S99RBLN,RBLEN       PUT LENGTH OF 'RB'IN ITS LEN. FIELD  
         MVI   S99VERB,S99VRBAL    SET VERB CODE FIELD TO ALLOCATE FUNC 
         MVI   S99FLAG1,S99NOCNV   DO NOT USE EXISTING ALLOCATION     B 
         LA    R5,S99RB+RBLEN      POINT20 BYTES BEYOND START OF 'RB'   
         USING S99TUPL,R5          ESTABLISH ADDRESSABILITY FOR TEXT    
*                                  UNITS PTRS                           
         ST    R5,S99TXTPP         INITIALISE THE TEXT POINTERS ADDRESS 
*                                  IN 'RB'                              
         LA    R6,S99TUPL+24       POINT JUST PAST THE  SIX  TEXT UNIT  
*                                  POINTERS                             
         USING S99TUNIT,R6         SET ADDRESSSABILITY FOR THE FIRST    
*                                  TEXT UNIT                            
         ST    R6,S99TUPTR         POINT 1ST TEXT UNIT POINTER TO       
*                                  1ST TEXT UNIT                        
         LA    R7,DALDSNAM        GET THE KEY FOR DSNAME                
         STH      R7,S99TUKEY               PUT THE KEY IN THE TEXTUNIT 
*                                  KEY FIELD                            
         LA    R7,1                BECAUSE THE DSNAME KEY REQUIRES ONLY 
         STH   R7,S99TUNUM         ONE PARAMER,STORE 1 IN NUMBER FIELD  
         SR    R3,R3                                                    
         IC    R3,1(R10)      LOAD LENGTH OF DSNAME INTO R3             
         STH   R3,S99TULNG    PUT LENGTH INTO DSECT                     
         LA    R7,S99TUPAR    LOAD ADDRESS FOR MVC IN EX INSTR          
         EX    R3,MOVLEN      TO MOVE DSNAME                            
         LA    R6,S99TUNIT+6   POINT JUST PAST 1ST TEXT 0NIT            
         AR    R6,R3                                                    
         LA    R5,S99TUPL+4        POINT TO 2ND TEXT UNIT POINTER       
         ST    R6,S99TUPTR         POINT 2ND TEXT UNIT POINTER TO       
*                                  2ND TEXT UNIT                        
         LA    R7,DALSTATS         GET KEY FOR STATUS SPECIFICATION     
         STH   R7,S99TUKEY         AND PUT KEY IN TEXT UNIT             
         LA    R7,1                BECAUSE THE STATUS KEY REQUIRES      
         STH   R7,S99TUNUM         ONLY ONE PARAMETER,STORE 1 IN NUMBER 
*                                  FIELD                                
         STH   R7,S99TULNG         SET THE STATUS PARM FIELD LEN TO 1   
         MVI   S99TUPAR,X'08'      SET PARM FIELD TO INDICATE SHARE     
         LH    R10,0(R10)    LOAD LENGTH HALFWORD                       
         SRL   R10,8                                                    
         LTR   R10,R10            TEST FOR A STATUS SPECIFICATION       
         BZ    ADJPOINT                                                 
         STC   R10,S99TUPAR        RESET PARM FIELD                     
ADJPOINT EQU   *                                                        
         LA    R6,S99TUNIT+7      POINT JUST PAST THE SECOND TEXT UNIT  
         LA    R5,S99TUPL+4        POINT TO 3RD TEXT UNIT POINTER       
         ST    R6,S99TUPTR         POINT 3RD TEXT UNIT POINTER          
*                                  TO3RDTEXT UNIT                       
* TEST FOR CATALOGUED DATASET                                           
         CLC   0(4,R9),CATA                                             
         BE    LSTPTR                                                   
*                                                                       
*                                                                       
* TEST FOR SYSDA.IF FOUND GO TO SECTION TO MOVE IN UNITS=SYSDA          
*                                                                       
         CLC   0(5,R9),SYSDA                                            
         BNE   VOLSER                                                   
*                                                                       
TEMPDS   LA    R7,DALUNIT     GET KEY FOR UNIT                          
         STH   R7,S99TUKEY                                              
         LA    R7,1                                                     
         STH   R7,S99TUNUM    STORE IN NUMBER FIELD                     
         MVC   S99TULNG(2),H5      STORE LENGTH OF FIELD                
         MVC   S99TUPAR(5),SYSDA   STORE SYSDA FOR UNIT                 
         LA    R6,S99TUNIT+11 POINT PAST TEXT UNIT                      
         LA    R5,S99TUPL+4   POINT TO FOURTH TEXT UNIT POINTER         
         ST    R6,S99TUPTR    POINT 4TH TEXT UNIT POINTER TO            
*                             4TH TEXT UNIT                             
         B     LSTPTR                                                   
VOLSER   EQU   *                                                        
         LA    R7,DALVLSER         GET KEY FOR VOL , SER INFO           
         STH   R7,S99TUKEY                                              
         LA    R7,1                PARAM FIELD LENGTH                   
         STH   R7,S99TUNUM    STORE 6 IN NUMBER FIELD                   
         LA    R7,6               PARAM LENGTH                          
         STH   R7,S99TULNG    STORE 6 IN PARAM LENGTH FIELD             
         MVC   S99TUPAR(6),0(R9)  MOVE VOLSER I                         
         LA    R6,S99TUNIT+12                                           
         LA    R5,S99TUPL+4   POINT TO 3RD TEXT UNIT(POINTER)           
         ST    R6,S99TUPTR    POINT 3RD TXET UNIT POINTER TO 3RD        
*                             TEXTUNIT                                  
         LA    R7,DALUNIT     GET KEY FOR UNIT                          
         STH   R7,S99TUKEY                                              
         LA    R7,1           # SET TO 1                                
         STH   R7,S99TUNUM    STORE 4 IN NUMBER FIELD                   
         LA    R7,4           PARAM FIELD LENGTH                        
         STH   R7,S99TULNG    STORE 4 IN PARAM LENGTH FIELD             
         MVC   S99TUPAR(4),UNIT                                         
         LA    R6,S99TUNIT+10      PO                                   
         LA    R5,S99TUPL+4        POINT TO 4TH TEXT UNIT               
         ST    R6,S99TUPTR                                              
LSTPTR   OI    S99TUPTR,S99TUPLN TURN ON HIGH ORDER BIT TO INDICATE     
*                                  LAST PTR                             
         CLI   40(R11),C'X'        TEST FOR X IN DATA SET NAME          
         BE    NODD                IF X NO DD NAME SUPPLIED             
*                                                                       
         LA    R7,DALDDNAM         GET KEY FOR DDNAME                   
         STH   R7,S99TUKEY         PUT KEY INTO TEXT UNIT FIELD         
         LA    R7,1                                                     
         STH    R7,S99TUNUM        STORE 1 IN NUMBER FIELD              
         LA    R7,8                LENGTH OF DDNAME                     
         STH   R7,S99TULNG         STORE LENGTH IN DSECT                
         MVC   S99TUPAR(8),40(R11) MOVE IN  DDNAME                      
         LA    R6,S99TUNIT+14      POINT JUST PAST 5TH TEXT UNIT        
         LA    R5,S99TUPL+4        POINT TO 5TH TEXT UNIT               
         ST    R6,S99TUPTR         POINT TEXT UNIT POINTER TO TEXT UNIT 
*                                                                       
         DYNALLOC                                                       
*                                                                       
         BC    15,CHERRS(15)                                            
CHERRS   B     RETURN              RC=0                                 
         B     NOSYSRES            RC=4                                 
         B     REQDENIE            RC=8                                 
         B     PARMERRS            RC=12                                
         ABEND 999,DUMP            RC=16                                
NODD     LA    R7,DALRTDDN         GET KEY FOR 'RETURN DDNAME' AND      
*                                  PUT THE KEY IN TEXT UNIT KEY FIELD   
         STH   R7,S99TUKEY                                              
         LA    R7,1                BECAUSE 'RETURN DDNAME'KEY REQUIRES  
         STH   R7,S99TUNUM         ONLY 1 PARAMETER STORE 1 IN NUMBER   
*                                  FIELD                                
         LA    R7,8                SET LENGTH OF FIELD FOR RETURNUNG    
         STH   R7,S99TULNG         DDNAME TO 8                          
         LA    R7,S99TULNG+2  ADDRESS OF RETURNED DDNAME                
         LR    R1,R8               PUT REQ BLK PTR ADDR IN REG 1 FOR    
*                                  DYNALLOC                             
DYNAL    DYNALLOC                  INVOKE DYNAMIC ALLOCATION TO PROCESS 
*                                  REQUEST                              
         BC    15,ERRORS(R15)                                           
ERRORS   B     NOERRORS       R15=0                                     
CURENVIR B     NOSYSRES       R15=4                                     
INSTVALR B     REQDENIE       R15=8                                     
INVPARML B     PARMERRS       R15=12                                    
         ABEND    999,DUMP                                              
NOERRORS EQU   *    SUCCESSFUL DYNALLOC                                 
         MVC   40(8,R11),0(R7) MOVE DDNAME TO DCB                       
*                                                                       
*                                                                       
*                                                                       
RETURN   FREEMAIN R,LV=128,A=(R8)                                   A   
         RETURN13 RC=0                                                  
*                                                                       
NOSYSRES EQU   *    CURRENT ENVIRONMENT ERROR UNAVAILABILITYOF          
*                   SYSTEM RESOURCES  PROVIDES ERROR REASON CODE        
         FREEMAIN R,LV=128,A=(R8)                                   A   
         L        R3,4(R2)    2ND 4 CHARS OF DSN                    C   
         L        R4,8(R8)    ERROR REASON CODE                     C   
         L        R5,40(R11)  1ST 4 CHARS OF DDNAME                 C   
         ABEND    555,DUMP                                          B   
*                                                                       
REQDENIE EQU   *    REQUEST DENIED INSTALLATION VALIDATION              
         FREEMAIN R,LV=128,A=(R8)                                   A   
         ABEND    666,DUMP                                          B   
*                                                                       
PARMERRS EQU   *    INVALID PARAMETE LIST SEE ERROR REASON CODE         
         FREEMAIN R,LV=128,A=(R8)                                   A   
         ABEND     777,DUMP                                         B   
*                                                                       
*                                                                       
*                                                                       
MOVLEN   MVC   0(0,R7),0(R2)  MOVE DSNAME INOT DSECT                    
UNIT     DC    C'DISK'                                                  
SYSDA    DC    CL5'SYSDA'                                               
H5       DC    H'5'                                                     
CATA     DC    CL4'CATA'                                                
         DC    C'$Date: 30/01/2006 20:22:10$ $Revision: 1$'             
         IEFZB4D0                                                       
          IEFZB4D2                                                      
RBLEN    EQU   (S99RBEND-S99RB)                                         
         END                                                            
