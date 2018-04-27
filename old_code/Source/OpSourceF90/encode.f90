SUBROUTINE ENCODE(DESCR,VALUES,ND,NELEM,NOBS,NAMES,STRING,CMPRES,L4)            
                                                                                
!-----------------------------------------------------------------------        
!                                                                               
! ROUTINE       : ENCODE            (real input, q/c sequence)                  
!                                                                               
! PURPOSE       : to make a bufr bit string from arrays of values &             
!               : corresponding descriptors, compressing if required.           
!               : n.b. encode makes only the data section (#4) of a             
!               : BUFR message.                                                 
!                                                                               
! CALLED BY     : ENBUFR                                                        
!                                                                               
! CALLS         : TABLEB, TABLED, VALOUT, DESFXY, LOCALD,          !2.7         
!                 BUFRQOP, VALUE, BUFR207                          !2.3         
!                                                                               
! ARGUMENTS     : (1) descriptors (elements & associated fields only)           
!                 (2) values to be coded (nobs*nelem array)                     
!                 (3) number of descriptors (before expansion)                  
!                 (4) number of elements in descriptor string (nelem)           
!                 (5) number of reports (nobs)                                  
!                 (6) any character values (with pointers in array)             
!                 (7) output string (for section 4)                             
!                 (8) flag set if compression required                          
!                 (9) length of section (return zero if error)                  
!                                                                               
! REVISION INFO :                                                               
!                                                                               
! $Workfile: encode.f90$ $Folder: OpSourceF90$                                       
! $Revision: 1$ $Date: 12/02/2010 14:41:52$                                     
!                                                                               
! CHANGE RECORD :                                                               
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         12/02/2010 14:41:52    Richard Weedon
!       further files ported
!       
! $                                                                             
! Revision 2.9  2004/04/05 11:19:59  usmdb                                      
! 15 March 2004    C Long                                                       
! 2.9  Allow for new values of FORMAT from TABLEB,                              
!      distinguishing between code & flag tables.                               
!                                                                               
! Revision 2.8  2004/02/02  11:53:19  11:53:19  usmdb (MetDB account c/o usjh)  
! 16 Feb 2004                                                                   
! 2.8  If several subsets were encoded without compression,                     
!      replication counts were always taken from the first subset.              
!      This only works if the counts are all the same:                          
!      use the subscript for the subset concerned!                              
!                                                                               
! Revision 2.7  2003/03/27  13:32:39  13:32:39  usmdb (MetDB account c/o usjh)  
! 7 April 2003    C Long                                                        
! 2.7  Remove LOCALB call.                                                      
!                                                                               
! Revision 2.6  2002/08/07  09:22:38  09:22:38  usmdb (MetDB account c/o usjh)  
! Changed so that LOCALB is called before TABLEB and therefore                  
! takes precedence - S.Cox                                                      
!                                                                               
! Revision 2.5  2002/04/09  11:44:43  11:44:43  usmdb (Generic MetDB account)   
! After each call to ICHAR, the value returned by ICHAR is checked.             
! If < 0, 256 is added - needed for Sun OS.                                     
! Add preprocessor around EB2ASC calls - S.Cox                                  
!                                                                               
! Revision 2.4  2002/03/13 10:00:23  usmdb                                      
! 18 March 2002    C Long                                                       
! 2.4  Call new variable MSCALE, not LSCALE as in 2.2!                          
!      Allow for width=1 (1 not missing)                                        
!                                                                               
! Revision 2.3  2002/03/07  15:54:14  15:54:14  usmdb (Generic MetDB account)   
! 18 March 2002    C Long                                                       
! 2.3  Greatest encodable value is 2**width-2, not 2**width.                    
!      Implement 207YYY (to change scale, width & ref val together).            
!                                                                               
! Revision 2.2  2002/01/15  15:15:03  15:15:03  usmdb (Generic MetDB account)   
! 21 Jan 2002     C Long                                                        
! 2.2  Call BUFRQOP to see if log is needed for quality operations,             
!      replace NXZERO call by corrected code from DECODE,                       
!      set ELMLOG differently                                                   
!      (descriptor in top bits to avoid problem when width>127),                
!      improve code to get integer value from REAL*4 (using REAL*8)             
!      to avoid overflow for elements with >24 bits.                            
!                                                                               
! Revision 2.1  2001/10/03  15:09:38  15:09:38  usmdb (Generic MetDB account)   
! 15 Oct 2001    C Long                                                         
! 2.1  Correct 206yyy code by calling TABLEB if entry not in LOCALB             
!      and incrementing value subscript as well as descriptor subscript.        
!                                                                               
! Revision 2.0  2001/03/07  10:19:15  10:19:15  usmdb (Generic MetDB account)   
! Added copyright and modified header - S.Cox                                   
!                                                                               
! Revision 1.12  2000/11/07 12:10:18  usmdb                                     
! Remove references to FLAGON and ASSOC as they are                             
! not used - S.Cox                                                              
! 20 Nov 2000     C Long                                                        
! 1.12a  Correct code for reference value changes                               
! 1.12b  Re-expand original descriptors if NOBS>1 without compression           
!        (& make length of arrays for input descriptors & element               
!        details a PARAMETER so that it can be increased easily)                
!                                                                               
! Revision 1.11  2000/03/10  09:49:02  09:49:02  usmdb (Generic MDB account)    
! 20 March 2000      C Long                                                     
! 1.11  If a replication count is missing, carry on with a zero count           
!       rather than giving up & leaving an undecodable message.                 
! 1.11a Change missing data checks from .LE.-9999999 & .GT.-9999999             
!       to EQ & NE - there are now genuine values less than -9999999            
!       & should be no confusion left about whether the missing data            
!       indicator is minus seven nines or minus nine nines.                     
!                                                                               
! Revision 1.10  99/02/11  11:59:15  11:59:15  usmdb (Generic MDB account)      
! 15 Feb 1999       C Long                                                      
! 1.10 Do not delete operators till end,                                        
!      list elements just looked up in Table B,                                 
!      do not call LOCALB till after TABLEB.                                    
!                                                                               
! Revision 1.9  98/06/11  16:03:11  16:03:11  usmdb (Generic MDB account)       
! allow for 1- bit elements (increment width of 1, not 2!)                      
!                                                                               
! Revision 1.8  98/04/20  07:25:46  07:25:46  usmdb (Generic MDB account)       
! Handle any size of scaling.                                                   
!                                                                               
! Revision 1.7  98/03/05  14:36:28  14:36:28  usjl (Jon Lewthwaite)             
! Changed COMMON block from /LOG/ to /LOGCOM/                                   
!                                                                               
! Revision 1.6  1998/02/03 17:21:57  usmdb                                      
! Addition of (K .GT. NELEM) check                                              
!                                                                               
! Revision 1.5  1998/01/15 17:53:55  uspm                                       
! Increment character string pointer after 205YYY                               
!                                                                               
! Revision 1.4  1997/09/22 09:51:52  uspm                                       
! Add check on K so does not exceed nelem                                       
! Change all labelled statements to be CONTINUE                                 
!                                                                               
! Revision 1.3  1997/08/08 10:15:32  uspm                                       
! Latest changes from  1                                                        
!                                                                               
! Revision 1.2  1997/06/20 09:38:21  uspm                                       
! Remove unused statement function flagon.                                      
!                                                                               
! Revision 1.1  1997/06/19 13:40:01  uspm                                       
! Initial revision                                                              
!                                                                               
! 21-03-97: P McCormack - initialise nuref to one                               
!   Sep 97: Increment character string pointer after 205YYY           !d        
! 04-03-97: C.Long: cut out write to log for ECMWF q/c operations,              
!                   printing message & returning if operation found   !c        
! 27-02-97: C.Long: replace 2**n array by 2**n-1 (2**31 is too big for          
!                   integer, so cannot tell if some elements missing!)!b        
! 07-11-96: C.Long: stop if negative delayed replication count found! !a        
! 06-06-96: C.Long: remove special case of character compression when           
!                   some strings missing but the rest are all the same          
!                   (cannot use 1-bit increments for characters!)               
! 27-05-96: S.Cox: initialise octet 4 of section 4 to zero                      
! 21-05-96: S.Cox: change TWOTO to range from 0->31                             
!   Apr 96: S.Cox: change size of ELMLOG and REFLOG from 999 to 99999           
!   Jun 95: S.Cox: changes to allow code to work on a HP-UX machine.            
!           SAVE statement added and some non-portable code changed.            
!   Feb 94: finally try to implement rex''s quality operations!                 
!   Nov 94: improve error message when non-compressed value too big;            
!           include 1st input descriptor to identify data if sequence.          
!   Mar 94: do not scale code figures (-1 & -2) for precipitation               
!   Oct 93: correct error - do not let associated fields be scaled              
!   Sep 93: increase size of array for local sequence. let associated           
!           fields be nested; allow associated fields with characters           
!   May 93: allow 206yyy and local tables b & d                                 
!   Feb 92: allow missing values of character elements                          
!   Jun 91: allow change of reference value for code or flag table              
!   Apr 91: allow for rounding errors in real-to-integer conversion             
!   Mar 91: correct error when all values of an element are equal but           
!           the first, which is missing (set msflag for first value)            
!   Oct 90: q/c sequence changed to something more flexible         !q/c        
!   Jul 90: allow q/c sequence of differences  (not yet bufr)       !q/c        
!   Jun 90: add code to allow changes of reference value                        
!           and insertion of character strings.    real input.                  
!   Dec 89: check each value against field width, not just base value           
!                                                                               
!-----------------------------------------------------------------------        
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.                   
!                                                                               
! Met Office, United Kingdom                                                    
!                                                                               
! The use, duplication and disclosure of this code is strictly                  
! prohibited without the permission of The Meteorological Database              
! Team at the above address.                                                    
!-----------------------------------------------------------------------        
                                                                                
SAVE                                                                            
                                                                                
INTEGER DESCR(*)                                                                
REAL VALUES(NOBS,NELEM)                                                         
DOUBLE PRECISION D                                                              
INTEGER F,X,Y, SCALE,SCL, REFVAL,REF, WIDTH,WID, SEQ(999)                       
INTEGER NEWREF(20),REFDES(20), LASSOC(20)                                       
INTEGER EMBED, WIDEST, OCTET, BONES(0:31)                                       
INTEGER QCSTAR,QCNSEQ,QCELMT                                !q/c                
INTEGER QCSCAL,QCNREF,QCWID                                 !q/c                
LOGICAL QCSEQ,QCDIFF,QCVAL,QCLAST                           !q/c                
LOGICAL CMPRES,MSFLAG,DIFLAG                                !1.12               
CHARACTER STRING*(*),NAMES*(*), FORMAT*1,NAME*60,UNITS*24                       
CHARACTER HEAD*80                                           !ST 3               
LOGICAL HEADSET                                             !1.10               
                                                                                
INTEGER N_DEL,DEL_ADDR(0:9999),IADD                         !1.10               
                                                                                
PARAMETER (LOGDIM=999,LISTDIM=100)                           !2.2               
LOGICAL NODATA,LOGGED                                                           
LOGICAL   QUALOPS    ! true if quality operations expected   !2.2               
LOGICAL   BUFRQOP    ! function to set QUALOPS               !2.2               
LOGICAL   NOMISS     ! Flag for descr. without missing data   !4                
INTEGER QUALOP, ELMLOG(LOGDIM),REFLOG(LOGDIM)                 !C                
COMMON /COMLOG/ ELMLOG,REFLOG                                !2.2               
INTEGER   LOGNTRY    ! entry number in quality operation log !2.2               
INTEGER   MAPBIT     ! 0 or 1 from quality operation bit map !2.2               
INTEGER   VALUE      ! function used to return MAPBIT        !2.2               
                                                                                
REAL MIS                                                                        
INTEGER MISS                                                                    
                                                                                
REAL TENTO(-4:5)   ! powers of ten for commonest scales     !1.10               
REAL TEN_TO_SCALE                                           !1.10               
INTEGER INDES(LISTDIM) ! array for input descriptors       !1.12b               
INTEGER INPUTND    ! number of input descriptors            !1.10               
INTEGER NLISTED    ! number of elements in list below       !1.10               
INTEGER XYS(LISTDIM)                                       !1.12b               
INTEGER SCALES(LISTDIM),REFVALS(LISTDIM),WIDTHS(LISTDIM)   !1.12b               
CHARACTER*1 FORMATS(LISTDIM)                               !1.12b               
DATA NLISTED/0/                                             !1.10               
DATA HEADSET/.FALSE./                                       !1.10               
DATA TENTO/0.0001, 0.001, 0.01, 0.1, 1., 10., 100.,&        !1.10               
          &1000., 10000., 100000./                          !1.10               
                                                                                
DATA MIS/-9999999./                                                             
DATA MISS/-9999999/                                                             
DATA MSFLAG/.FALSE./                                        !1.12               
DATA BONES/0,1,3,7,15,31,63,127,255,511,1023,2047,&                             
&4095,8191,16383,32767,65535,131071,262143,524287,1048575,&                     
&2097151,4194303,8388607,16777215,33554431,67108863,134217727,&                 
&268435455,536870911,1073741823,2147483647/                                     
                                                                                
IF (.NOT.HEADSET) THEN                                      !1.10               
  HEAD='$Workfile: encode.f90$ ' //&                                              
      &'$Revision: 1$ $Date: 12/02/2010 14:41:52$'                              
  HEADSET=.TRUE.                                            !1.10               
ENDIF                                                                           
                                                                                
L4=0                                                                            
                                                                                
! See if the descriptor sequence is the same as for the last      !1.10         
! message, when we can use the listed element details.            !1.10         
                                                                                
IF (ND.NE.INPUTND) THEN                                     !1.10               
  NLISTED=0                                                 !1.10               
ELSE                                                        !1.10               
  DO I=1,ND                                                 !1.10               
    IF (DESCR(I).NE.INDES(I)) NLISTED=0                     !1.10               
  ENDDO                                                     !1.10               
ENDIF                                                       !1.10               
                                                                                
! If these input descriptors are different from the last lot,     !1.10         
! keep them (unless there are too many) to see if the next       !1.12b         
! message has the same sequence (& if so use the listed details)  !1.10         
                                                                                
IF (NLISTED.EQ.0) THEN                                      !1.10               
  IF (ND.LE.LISTDIM) THEN                                  !1.12b               
    INPUTND=ND                                              !1.10               
    DO I=1,ND                                               !1.10               
      INDES(I)=DESCR(I)                                     !1.10               
    ENDDO                                                   !1.10               
  ELSE                                                      !1.10               
    INPUTND=0                                               !1.10               
  ENDIF                                                     !1.10               
ENDIF                                                       !1.10               
                                                                                
! ----------------------------------------------------------------------        
! New variables to implement quality operations:           (feb 95)             
!                                                                               
! QUALOP - quality operation in force, x in 2xx000 (x=22-25,31)                 
! LASBIT - last bit in bit map (value of ibefor)                                
! NEXBIT - latest zero bit in bit map                                           
! LASTEL - last element in sequence covered by bit map (log pointer)            
! NLOG   - latest value with entry in decode log                                
! ELMLOG - array for logging width/scale/descriptor for values decoded          
! REFLOG - array for logging reference value for values decoded                 
! NODATA - flag set when non-coordinate values are suppressed by 221...         
! LOGGED - flag set if element replaces place holder, so scale etc set          
! ----------------------------------------------------------------------        
                                                                                
QUALOP=0                                                                        
LASBIT=0                                                                        
LASTEL=0                                                                        
NLOG=0                                                                          
NODATA=.FALSE.                                                                  
LOGGED=.FALSE.                                                                  
N_DEL=0                                                     !1.10               
                                                                                
! See if there will be any quality operations requiring a bit map  !2.2         
! in the expansion of the descriptor sequence.                     !2.2         
                                                                                
QUALOPS=BUFRQOP(DESCR,ND)                                    !2.2               
                                                                                
! ----------------------------------------------------------------------        
! Initialise number of observation (column number in values array).             
! NOB will be incremented if there are several obs but no compression.          
! Initialise count of bits in string, leaving room for length at start.         
! ----------------------------------------------------------------------        
                                                                                
NOB=1                                                                           
INAM=1                                                                          
IBEFOR=32                                                                       
CALL DESFXY(DESCR(1),F,X,Y)  ! keep first descriptor to identify                
INDESC=F*100000+X*1000+Y     ! data type (hoping it is F=3!)                    
                                                                                
!**********************************************************************         
!***********                              *****************************         
!***********   Expand descriptor string   *****************************         
!***********                              *****************************         
!                                                                               
!  N: subscript in descriptor array.     K: row number in value array.          
!       K is incremented after coding the value(s) of an element;               
!       N is incremented then and after handling an F=2 descriptor              
!    (when F=1 or F=3 the descriptor is removed & N left unchanged)             
!                                                                               
!**********************************************************************         
                                                                                
10  CONTINUE                                                                    
N=1                                                                             
K=1                                                                             
NREF=0                                                                          
NUREF=1                                                       !c                
IWIDTH=0                                                                        
ISCALE=0                                                    !q/c                
MSCALE=0     ! YYY from 207YYY                              !2.4                
NASSOC=0                                                                        
QCNSEQ=0                                                    !q/c                
QCSEQ=.FALSE.                                               !q/c                
20 CONTINUE                                                                     
CALL DESFXY(DESCR(N),F,X,Y)                                                     
ID=F*100000+X*1000+Y                                                            
                                                                                
!     PRINT *,ID                                                                
                                                                                
! ----------------------------------------------------------------------        
! If replication (F=1), repeat descriptors, finding count in data if            
! replication is delayed.                                                       
! ----------------------------------------------------------------------        
                                                                                
IF (F.EQ.1) THEN                                                                
                                                                                
! ----------------------------------------------------------------------        
! If the replication count is not in the descriptor, get it from the            
! data (for data repetition - 031011 or 031012 - there is only one value        
! (or none) in the input, so we do not need more than one descriptor).          
! Give up if a delayed replication count is negative (missing data?)  !a        
! No, better to set Y=0: because there is no error return, and if  !1.11        
! we just give up then decode will get a misleading count from     !1.11        
! 7777, Y=0 is more likely to leave a tidy message (but if any     !1.11        
! elements follow the replication their values will be suspect...) !1.11        
! ----------------------------------------------------------------------        
                                                                                
  IF (Y.EQ.0) THEN                                                              
    Y=VALUES(NOB,K)                                           !2.8              
    IF (Y.LT.0) THEN                                            !a              
      PRINT *,'Delayed replication count <0',Y,K,'th value'     !a              
      PRINT *,'Encoding will continue with a zero count.'    !1.11              
      Y=0                                                    !1.11              
    ENDIF                                                       !a              
    IF (DESCR(N+1).EQ.IDES(031011) .AND. Y.GT.1) Y=1                            
    IF (DESCR(N+1).EQ.IDES(031012) .AND. Y.GT.1) Y=1                            
    EMBED=1                                                                     
  ELSE                                                                          
    EMBED=0                                                                     
  ENDIF                                                                         
                                                                                
! ----------------------------------------------------------------------        
! Work out how many extra descriptors, move the rest down to make room          
! (working from right to left to avoid repetition!), & repeat from left         
! to right to fill the empty slot.                                              
! ----------------------------------------------------------------------        
                                                                                
  NEXTRA=X*(Y-1)                                                                
  IF (Y.GE.1) THEN                                                              
                                                                                
! ----------------------------------------------------------------------        
! First make room                                                               
! ----------------------------------------------------------------------        
                                                                                
    DO 110 I=ND,N+EMBED+X+1,-1                                                  
     DESCR(I+NEXTRA)=DESCR(I)                                                   
  110     CONTINUE                                                              
                                                                                
! ----------------------------------------------------------------------        
! Then repeat (bunch will recur at intervals of X) & adjust counts              
! ----------------------------------------------------------------------        
                                                                                
    DO 120 I=1,NEXTRA                                                           
     DESCR(N+EMBED+X+I)=DESCR(N+EMBED+I)                                        
  120     CONTINUE                                                              
                                                                                
! ----------------------------------------------------------------------        
! If I=1, then NEXTRA=0, so nothing is done; if Y=0, delete                     
! the descriptors that would otherwise be replicated  (NEXTRA=-X)               
! ----------------------------------------------------------------------        
                                                                                
  ELSE IF (Y.EQ.0) THEN                                                         
    DO 130 I=N+EMBED+1,ND-X                                                     
     DESCR(I)=DESCR(I+X)                                                        
  130     CONTINUE                                                              
        ENDIF                                                                   
                                                                                
! ----------------------------------------------------------------------        
! Delete replication descriptor to make sequence usable for next report         
! (any embedded count must be left for the value to be coded)                   
! ----------------------------------------------------------------------        
                                                                                
  DO 140 I=N+1,ND+NEXTRA                                                        
   DESCR(I-1)=DESCR(I)                                                          
  140   CONTINUE                                                                
                                                                                
  ND=ND+NEXTRA-1                                                                
                                                                                
! ----------------------------------------------------------------------        
! If a replication (delayed or not) is for one element only, 031031,            
! assume it is a bit map to be used in quality operations.    (feb 95)          
! set pointers to the last bit in the map & the bit before the start,           
! allowing for the bit count itself if it is to go in the data     !2.2         
! (getting width from Table B in case it is not the usual 8 bits)  !2.2         
! & for 6-bit increment widths (for count & bits) if compressed.   !2.2         
! ----------------------------------------------------------------------        
                                                                                
  IF (X.EQ.1 .AND. DESCR(N+EMBED).EQ.IDES(031031)) THEN                         
!         PRINT *,'NEW BIT MAP'                                                 
    LASBIT=IBEFOR+Y                                                             
    IF (CMPRES) LASBIT=LASBIT+6*Y                            !2.2               
    NEXBIT=IBEFOR                                                               
    IF (EMBED.EQ.1) THEN                                                        
      CALL TABLEB(31,MOD(DESCR(N),256),&                                        
                 &SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)                         
      IF (CMPRES) WIDTH=WIDTH+6                              !2.2               
      LASBIT=LASBIT+WIDTH                                                       
      NEXBIT=NEXBIT+WIDTH                                                       
    ENDIF                                                                       
  ENDIF                                                                         
                                                                                
! ----------------------------------------------------------------------        
! Code all values for descriptors between 203yyy & 203000 in y bits.            
! changed reference values are assumed not to be missing and not to             
! vary from report to report if data is compressed.                             
! Characters to be inserted (x=5) are assumed to follow the previous            
! character values in names, no pointer in values needed.                       
! ----------------------------------------------------------------------        
                                                                                
ELSE IF (F.EQ.2) THEN                                                           
                                                                                
! ----------------------------------------------------------------------        
! Start by deleting the F=2 descriptor, unless its a place holder for a         
! quality operation, in which case replace it by an element descriptor.         
!                                                                               
! If a place holder is found for the current quality operation, find            
! the next zero in the bit map, get the corresponding entry in the log          
! of decoded values, and jump to the element section to get this value.         
! ----------------------------------------------------------------------        
                                                                                
  IF (X.EQ.QUALOP .AND. Y.EQ.255) THEN                                          
    IF (LASBIT.EQ.0) THEN                                                       
      PRINT *,'QUALITY PLACE HOLDER BUT NO BIT MAP',ID                          
      GO TO 999                                                                 
    ENDIF                                                                       
                                                                                
    MAPBIT=1                                                 !2.2               
    DO WHILE (MAPBIT.EQ.1 .AND. NEXBIT.LE.LASBIT)            !2.2               
      MAPBIT=VALUE(STRING,NEXBIT,1)                          !2.2               
      IF (CMPRES) NEXBIT=NEXBIT+6                            !2.2               
    ENDDO                                                    !2.2               
                                                                                
    IF (NEXBIT.GT.LASBIT) THEN                               !2.2               
      PRINT *,'QUALITY PLACE HOLDER BUT NO MORE ZERO BITS',ID                   
      GO TO 999                                                                 
    ENDIF                                                                       
                                                                                
! ----------------------------------------------------------------------        
! Set scale etc from log & go round loop again with element descriptor          
! replacing place holder: set flag to get details from log, not table B.        
! ----------------------------------------------------------------------        
                                                                                
    LOGNTRY=LASTEL-LASBIT+NEXBIT                             !2.2               
    IF (CMPRES) LOGNTRY=LASTEL-(LASBIT-NEXBIT)/7             !2.2               
                                                                                
    IF (LOGNTRY.GT.LOGDIM) THEN                              !2.2               
      PRINT*,'Quality operator place holder found, but log too' !c              
      PRINT*,'small. Change LOGDIM to get past this operation.' !c              
      RETURN                                                    !c              
    ELSE                                                        !c              
      LEMLOG=ELMLOG(LOGNTRY)                                 !2.2               
!     PRINT *,LASTEL-LASBIT+NEXBIT,'TH LOG ENTRY USED'                          
!     PRINT *,'LASTEL,LASBIT,NEXBIT:'                                           
!     PRINT *,LASTEL,LASBIT,NEXBIT                                              
      DESCR(N)=LEMLOG/65536                                  !2.2               
      LOGGED=.TRUE.                                                             
    ENDIF                                                       !c              
  ELSE                                                                          
                                                                                
! ----------------------------------------------------------------------        
! If it is not a place holder, delete the F=2 descriptor                        
! ----------------------------------------------------------------------        
                                                                                
    N_DEL=N_DEL+1                                           !1.10               
    DEL_ADDR(N_DEL)=N                                       !1.10               
    N=N+1                                                   !1.10               
                                                                                
! ----------------------------------------------------------------------        
! Now handle the operation: first simple changes of field width etc...          
! ----------------------------------------------------------------------        
                                                                                
    IF (X.EQ.1) THEN                                                            
      IF (Y.EQ.0) THEN                                                          
        IWIDTH=0                                                                
      ELSE                                                                      
        IWIDTH=Y-128                                                            
      ENDIF                                                                     
                                                                                
    ELSE IF (X.EQ.2) THEN                                                       
      IF (Y.EQ.0) THEN                                                          
        ISCALE=0                                                                
      ELSE                                                                      
        ISCALE=Y-128                                                            
      ENDIF                                                                     
                                                                                
    ELSE IF (X.EQ.3) THEN                                                       
      IF (Y.EQ.0) THEN                                                          
        NREF=0                                                                  
      ELSE                                                                      
        DO WHILE (DESCR(N).NE.IDES(203255))                !1.12a               
          INVAL=VALUES(NOB,K)                                                   
                                                                                
! ----------------------------------------------------------------------        
! Keep changed ref values to use later                                          
! ----------------------------------------------------------------------        
                                                                                
          NREF=NREF+1                                      !1.12a               
          NEWREF(NREF)=INVAL                                                    
          REFDES(NREF)=DESCR(N)                                                 
                                                                                
! ----------------------------------------------------------------------        
! Code negative value by setting sign bit                                       
! ----------------------------------------------------------------------        
                                                                                
          IF (INVAL.GE.0) THEN                                                  
            CALL VALOUT(STRING,IBEFOR,Y,INVAL)                                  
          ELSE                                                                  
            CALL VALOUT(STRING,IBEFOR,1,1)                                      
            CALL VALOUT(STRING,IBEFOR,Y-1,-INVAL)                               
          ENDIF                                                                 
                                                                                
! ----------------------------------------------------------------------        
! If compressed, add zero increment width                                       
! ----------------------------------------------------------------------        
                                                                                
          IF (CMPRES) CALL VALOUT(STRING,IBEFOR,6,0)                            
          N=N+1            ! past element descriptor       !1.12a               
          K=K+1            ! past input value              !1.12a               
        ENDDO                                              !1.12a               
        N=N+1              ! past 203255                   !1.12a               
      ENDIF                                                                     
                                                                                
    ELSE IF (X.EQ.4) THEN                                                       
      IF (Y.EQ.0) THEN                                                          
        IF (NASSOC.GT.0) NASSOC=NASSOC-1                                        
      ELSE                                                                      
        NASSOC=NASSOC+1                                                         
        LASSOC(NASSOC)=Y                                                        
      ENDIF                                                                     
                                                                                
    ELSE IF (X.EQ.5) THEN                                                       
!if defined (MVS)                                                               
      CALL EB2ASC(Y,NAMES(INAM:))                                               
!endif                                                                          
      DO 250 I=0,Y-1                                                            
       OCTET=ICHAR(NAMES(INAM+I:INAM+I))                                        
       IF (OCTET.LT.0) OCTET=OCTET+256                        !2.5              
       CALL VALOUT (STRING,IBEFOR,8,OCTET)                                      
  250       CONTINUE                                                            
      INAM=INAM+Y                                              !d               
                                                                                
! ----------------------------------------------------------------------        
! To hide a local descriptor by 206yyy, where y is the field width of           
! the local element, skip the descriptor & set its values to missing            
! - unless a local entry (in an overriding table or in a local     !2.7         
! section with Y>192) has an entry with the same field width.      !2.7         
! (If it has, ignore 206... & go on to encode the element)                      
! ----------------------------------------------------------------------        
                                                                                
    ELSE IF (X.EQ.6) THEN                                                       
      CALL DESFXY(DESCR(N),NEXTF,NEXTX,NEXTY)                                   
      CALL TABLEB(NEXTX,NEXTY,&                              !2.1               
                 &SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)      !2.1               
                                                                                
      IF (WIDTH.EQ.0 .OR. WIDTH+IWIDTH.NE.Y) THEN                               
        N=N+1               ! skip next (element) descriptor !2.1               
        K=K+1               ! skip corresponding values      !2.1               
        CALL VALOUT(STRING,IBEFOR,Y,BONES(Y))    ! missing base                 
        IF (CMPRES) CALL VALOUT(STRING,IBEFOR,6,0) ! no increments              
      ENDIF                                                                     
                                                                                
! 207yyy changes scale, width & reference value all together:      !2.3         
! keep yyy as MSCALE to call BUFR207 when element details known.   !2.4         
                                                                                
    ELSE IF (X.EQ.7) THEN                                    !2.3               
      MSCALE=Y                                               !2.4               
                                                                                
! ----------------------------------------------------------------------        
! If 221yyy found, insert 221000 at end of coordinates-only section             
! so that flag to skip other elements can be unset when 221000 reached          
! (221yyy is already deleted, so descr(n) is now the first of the yyy)          
! ----------------------------------------------------------------------        
                                                                                
    ELSE IF (X.EQ.21) THEN                                                      
      IF (Y.EQ.0) THEN                                                          
        NODATA=.FALSE.                                                          
        PRINT *,'NON-COORD VALUES NO LONGER SUPPRESSED'                         
      ELSE                                                                      
        DO I=ND,N+Y,-1                                                          
         DESCR(I+1)=DESCR(I)                                                    
        ENDDO                                                                   
        ND=ND+1                                                                 
        DESCR(N+Y)=IDES(221000)                                                 
        NODATA=.TRUE.                                                           
!       PRINT *,'NON-COORD VALUES SUPPRESSED TILL...'                           
      ENDIF                                                                     
                                                                                
! ----------------------------------------------------------------------        
! If q/c sequence of differences is to be added to following elements,          
! note the start and length of the sequence (& delete it if y=0) and            
! jump over it: if it needs expanding, it will be expanded where added          
! to an element, not here where it is defined.  so length stays fixed.          
! (Note: a 223... operation is our old q/c extension if y>0, ECMWF''s           
! if y=0 or y=255; though we use 223000 to end ours, so 223000 is ours          
! if our operation is in force, i.e if QCNSEQ>0)                                
! ----------------------------------------------------------------------        
                                                                                
    ELSE IF (X.EQ.23 .AND. (Y.EQ.0 .AND. QCNSEQ.GT.0)&      !q/c                
                     &.OR. (Y.NE.0.AND.Y.NE.255)) THEN      !q/c                
!     PRINT *,' OUR Q/C OPERATION'                                              
      QCNSEQ=Y                                              !q/c                
      IF (Y.GT.0) THEN                                      !q/c                
        QCSTAR=N        ! (F=2 descriptor already deleted)  !q/c                
        N=N+Y                                               !q/c                
      ELSE                         ! if Y=0, delete the     !q/c                
        DO 201 I=QCSTAR+QCNSEQ,ND  ! descriptors which      !q/c                
         DESCR(I-QCNSEQ)=DESCR(I)  ! defined the last       !q/c                
201     CONTINUE                                                                
        ND=ND-QCNSEQ               ! q/c sequence           !q/c                
        N=N-QCNSEQ                 ! (adjusting pointer)    !q/c                
      ENDIF                                                 !q/c                
                                                                                
! ----------------------------------------------------------------------        
! If a quality operator is found, note the operation and keep the log           
! pointer to the last value decoded (unless this is set already).               
! ----------------------------------------------------------------------        
                                                                                
    ELSE IF ((X.GE.22.AND.X.LE.25).OR.X.EQ.32) THEN                             
      PRINT *,'QUALITY OPERATOR',X,Y                                            
      IF (Y.GT.0 .AND. Y.LT.255) THEN                                           
        PRINT *,'QUALITY OPERATOR WITH NONZERO YYY',ID                          
        GO TO 999                                                               
      ENDIF                                                                     
                                                                                
      IF (NLOG.EQ.0) THEN                                                       
        PRINT *,'QUALITY OPERATOR BUT NO LOG KEPT',ID                           
        GO TO 999                                                               
      ENDIF                                                                     
                                                                                
      QUALOP=X                                                                  
      IF (LASTEL.EQ.0) LASTEL=NLOG                                              
      IF (LASTEL.EQ.NLOG) PRINT *,LASTEL,'SET AS END POINT'                     
                                                                                
! ----------------------------------------------------------------------        
! Only one of the four bit map operators which follow is useful.                
! 236000 defines a bit map for use later, but it will be found anyway.          
! 237000 reuses a bit map, but only one can be currently defined.               
! 237255 cancels a bit map, but redefinition would have the same effect.        
! only 235000 is essential: it unsets the end of the sequence of values         
! referred to by a bit map - without it all quality operations would by         
! definition refer back to the same point.                                      
! ----------------------------------------------------------------------        
                                                                                
    ELSE IF (X.EQ.35) THEN                                                      
      LASBIT=0                                                                  
      LASTEL=0                                                                  
    ELSE IF (X.EQ.36) THEN                                                      
      CONTINUE                                                                  
    ELSE IF (X.EQ.37) THEN                                                      
      IF (Y.EQ.0) THEN                                                          
        CONTINUE                                                                
      ELSE IF (Y.EQ.255) THEN                                                   
        LASBIT=0                                                                
      ENDIF                                                                     
    ENDIF                                                                       
  ENDIF                                                                         
                                                                                
! ----------------------------------------------------------------------        
! Look up a sequence  (expansion will overwrite sequence descriptor)            
! ----------------------------------------------------------------------        
                                                                                
ELSE IF (F.EQ.3) THEN                                                           
  CALL LOCALD(X,Y,SEQ,NSEQ,' ',' ')                                             
  IF (NSEQ.EQ.0) CALL TABLED(X,Y,SEQ,NSEQ)                                      
  IF (NSEQ.EQ.0) THEN                                                           
    PRINT *,N,'-TH SEQUENCE DESCRIPTOR',ID,'NOT IN TABLE D'                     
    PRINT *,ND,'IS N(DESCRIPTORS)',NELEM,'IS N(ELEMENTS)'                       
    GO TO 999                                                                   
  ENDIF                                                                         
                                                                                
! ----------------------------------------------------------------------        
! Insert sequence of descriptors, moving the rest down. adjust total.           
! ----------------------------------------------------------------------        
                                                                                
  DO 310 I=ND,N+1,-1                                                            
   DESCR(I+NSEQ-1)=DESCR(I)                                                     
  310   CONTINUE                                                                
                                                                                
  DO 320 I=1,NSEQ                                                               
   DESCR(N+I-1)=SEQ(I)                                                          
  320   CONTINUE                                                                
                                                                                
  ND=ND+NSEQ-1                                                                  
                                                                                
! ----------------------------------------------------------------------        
!******      When an element descriptor is reached, encode      ********        
!******      its value(s) - one row of the input array or       ********        
!******      2 rows if there is an added q/c field first.       ********        
!                                                                               
! If 221... has suppressed non-coordinate values, skip this descriptor          
! unless it is in class 1-9 or 31.  N, the descriptor pointer, will be          
! advanced at the end of the section; K, the value pointer, must skip           
! any associated fields as well as the value itself.  (But there is             
! no input data for suppressed non-coordinate values!)                          
! ----------------------------------------------------------------------        
                                                                                
ELSE IF (F.EQ.0) THEN                                                           
  IF (.NOT.NODATA .OR. (X.GE.1.AND.X.LE.9) .OR. X.EQ.31) THEN                   
                                                                                
!***************                  if differences are to be added, first         
!     add      *                  make room for them before the element         
! q/c sequence *                  & insert the descriptors.   later the         
! differences  *                  element descriptor will be put after          
!   (if any)   *                  each "kind of difference" descriptor.         
!***************                                                                
                                                                                
    IF (.NOT.LOGGED) THEN                                                       
      IF (QCNSEQ.GT.0 .AND. .NOT.QCSEQ .AND. X.GE.10) THEN !q/c                 
        QCELMT=DESCR(N)                                    !q/c                 
        QCSEQ=.TRUE.                                       !q/c                 
                                                                                
! ----------------------------------------------------------------------        
! Make room                                                                     
! ----------------------------------------------------------------------        
                                                                                
        DO 600 I=ND,N,-1                                   !q/c                 
         DESCR(I+QCNSEQ)=DESCR(I)                          !q/c                 
  600         CONTINUE                                                          
        ND=ND+QCNSEQ                                       !q/c                 
                                                                                
! ----------------------------------------------------------------------        
! Insert sequence                                                               
! ----------------------------------------------------------------------        
                                                                                
        DO 610 I=0,QCNSEQ-1                                !q/c                 
         DESCR(N+I)=DESCR(QCSTAR+I)                         !q/c                
  610         CONTINUE                                                          
                                                                                
! ----------------------------------------------------------------------        
! Suspend table C changes                                                       
! ----------------------------------------------------------------------        
                                                                                
        QCWID=IWIDTH                                       !q/c                 
        IWIDTH=0                                           !q/c                 
        QCSCAL=ISCALE                                      !q/c                 
        ISCALE=0                                           !q/c                 
        QCNREF=NREF                                        !q/c                 
        NUREF=NREF+1                                       !q/c                 
                                                                                
! ----------------------------------------------------------------------        
! N-th descriptor just changed, so go round & look at it again                  
! ----------------------------------------------------------------------        
                                                                                
        GO TO 20                                           !q/c                 
      ENDIF                                                !q/c                 
                                                                                
!************                                                                   
!  LOOK UP  *                                                                   
!  TABLE B  *                                                                   
!  DETAILS  *                                                                   
!************                                                                   
                                                                                
      I=1                                                  !1.10                
      DO WHILE (I.LE.NLISTED.AND.DESCR(N).NE.XYS(I))       !1.10                
        I=I+1                                              !1.10                
      ENDDO                                                !1.10                
                                                                                
      IF (I.GT.NLISTED) THEN                               !1.10                
        CALL TABLEB(X,Y,SCALE,REFVAL,WIDTH,FORMAT,&                             
                   &NAME,UNITS)                             !2.6                
                                                                                
        IF (WIDTH.EQ.0) THEN                                                    
          PRINT *,N,'-TH ELEMENT DESCRIPTOR',ID,'NOT IN TABLE B'                
          PRINT *,ND,'IS N(DESCRIPTORS)',NELEM,'IS N(ELEMENTS)'                 
          GO TO 999                                                             
        ENDIF                                                                   
                                                                                
        IF (NLISTED.LT.LISTDIM) THEN                      !1.12b                
          NLISTED=NLISTED+1                                !1.10                
          XYS(NLISTED)=DESCR(N)                            !1.10                
          SCALES(NLISTED)=SCALE                            !1.10                
          REFVALS(NLISTED)=REFVAL                          !1.10                
          WIDTHS(NLISTED)=WIDTH                            !1.10                
          FORMATS(NLISTED)=FORMAT                          !1.10                
        ENDIF                                              !1.10                
      ELSE                                                 !1.10                
        SCALE=SCALES(I)                                    !1.10                
        REFVAL=REFVALS(I)                                  !1.10                
        WIDTH=WIDTHS(I)                                    !1.10                
        FORMAT=FORMATS(I)                                  !1.10                
      ENDIF                                                !1.10                
                                                                                
! ----------------------------------------------------------------------        
! Adjust the scale & field width (except for code or flag table)                
! & reference value (for any element) if changes are in force.                  
! Scale etc can be changed separately - or together by 207yyy.    !2.3          
! (Assume 207yyy takes precedence over 201yyy, 202yyy & 203yyy.)  !2.3          
! ----------------------------------------------------------------------        
                                                                                
      IF (FORMAT.NE.'C'.AND.FORMAT.NE.'F'.AND.X.NE.31) THEN   !2.9              
        IF (QCDIFF .OR. QCVAL) THEN                         !q/c                
          WIDTH=WIDTH+QCWID                                 !q/c                
          SCALE=SCALE+QCSCAL                                !q/c                
        ELSE                                                                    
          IF (MSCALE.GT.0) THEN                             !2.4                
            CALL BUFR207(MSCALE,SCALE,WIDTH,REFVAL)         !2.4                
          ELSE                                              !2.3                
            SCALE=SCALE+ISCALE                                                  
            WIDTH=WIDTH+IWIDTH                                                  
          ENDIF                                             !2.3                
        ENDIF                                               !q/c                
      ENDIF                                                 !q/c                
                                                                                
      IF (QCDIFF .OR. QCVAL) THEN                           !q/c                
        IF (QCDIFF) THEN                                                        
          REFVAL=-BONES(WIDTH-1)-1                       !b !q/c                
          QCDIFF=.FALSE.                                    !q/c                
        ELSE                                                                    
          DO 401 I=1,NUREF-1                                !q/c                
           IF (DESCR(N).EQ.REFDES(I)) REFVAL=NEWREF(I)      !q/c                
  401       CONTINUE                                                            
          QCVAL=.FALSE.                                     !q/c                
        ENDIF                                               !q/c                
      ELSE IF (MSCALE.EQ.0) THEN                            !2.4                
        DO 402 I=NUREF,NREF                                 !q/c                
         IF (DESCR(N).EQ.REFDES(I)) REFVAL=NEWREF(I)                            
  402         CONTINUE                                                          
      ENDIF                                                 !q/c                
                                                                                
! ----------------------------------------------------------------------        
! After 008023 in a q/c sequence, insert the element descriptor   !q/c          
! & set the difference flag.  If 008023=0, end the q/c sequence.  !q/c          
! But 008023=0 is followed by the value of the element itself,    !q/c          
! we mustn't reattach the sequence to it! So don't unset QCSEQ    !q/c          
! till next time round (set QCLAST to handle final value)         !q/c          
! ----------------------------------------------------------------------        
                                                                                
      IF (QCLAST) THEN                                      !q/c                
        QCLAST=.FALSE.                                      !q/c                
        QCSEQ=.FALSE.                                       !q/c                
      ENDIF                                                                     
                                                                                
      IF (QCSEQ.AND.X.EQ.8 .AND. (Y.EQ.23 .OR. Y.EQ.24)) THEN !q/c              
        IF (VALUES(NOB,K).NE.0) THEN                        !q/c                
          DO 75 I=ND,N+1,-1                                 !q/c                
           DESCR(I+1)=DESCR(I)                              !q/c                
   75           CONTINUE                                                        
          ND=ND+1                                           !q/c                
          DESCR(N+1)=QCELMT                                 !q/c                
          IF (Y.EQ.23) QCVAL=.TRUE.                         !q/c                
          IF (Y.EQ.24) QCDIFF=.TRUE.                        !q/c                
        ELSE                                                !q/c                
          QCLAST=.TRUE.   ! value of element itself follows !q/c                
          IWIDTH=QCWID    ! switch table c changes back     !q/c                
          ISCALE=QCSCAL                                     !q/c                
          NUREF=1                                           !q/c                
          NREF=QCNREF                                       !q/c                
        ENDIF                                               !q/c                
      ENDIF                                                 !q/c                
                                                                                
! ----------------------------------------------------------------------        
! Keep a log of modified Table B parameters for all values decoded:             
! descriptor in 16 bits, width in 8 bits, scale in 8 bits.         !2.2         
! (Scale can be negative, so take mod to keep it in one byte.)                  
! (too big an overhead to do this all the time?  input flag to skip it?)        
! ----------------------------------------------------------------------        
                                                                                
      IF (QUALOPS .AND. NLOG.LT.LOGDIM) THEN                 !2.2               
        NLOG=NLOG+1                                                             
        LSCALE=SCALE                                                            
        IF (LSCALE.LT.0) LSCALE=SCALE+256                                       
        ELMLOG(NLOG)=DESCR(N)*65536+WIDTH*256+LSCALE         !2.2               
        REFLOG(NLOG)=REFVAL                                                     
        PRINT *,WIDTH,SCALE,REFVAL,'        LOGGED'                             
      ENDIF                                                    !c               
      NASSOX=NASSOC                                                             
    ELSE                                                                        
                                                                                
! ----------------------------------------------------------------------        
! If logged flag set, get details from log, not table B, & unset flag           
! (copy log entry too, as an entry for this value!)                             
! N.B. quality values referring back to log have no associated fields.          
! ----------------------------------------------------------------------        
                                                                                
      WIDTH=MOD(LEMLOG,65536)/256                            !2.2               
      SCALE=MOD(LEMLOG,256)                                  !2.2               
      IF (SCALE.GE.128) SCALE=SCALE-256                                         
      REFVAL=REFLOG(LOGNTRY)                                 !2.2               
      LOGGED=.FALSE.                                                            
!     PRINT *,WIDTH,SCALE,REFVAL,'   FROM LOG'                                  
                                                                                
      NLOG=NLOG+1                                                               
      ELMLOG(NLOG)=ELMLOG(LASTEL-LASBIT+NEXBIT)                                 
      REFLOG(NLOG)=REFVAL                                                       
      NASSOX=0                                                                  
!     PRINT *,WIDTH,SCALE,REFVAL,'        LOGGED'                               
    ENDIF                                                                       
                                                                                
!***********                                                                    
!          *    if there are associated fields, first loop down stack           
! numbers  *    of lengths, treating associated fields like numbers;            
!          *    last time round loop see if value itself is characters          
!***********                                                                    
                                                                                
    DO 400 NAS=NASSOX,0,-1                                                      
    IF (K .GT. NELEM) GOTO 400                                                  
    IF (X.EQ.31 .AND. NAS.GT.0) GO TO 400                                       
    IF (NAS.GT.0 .OR. FORMAT.NE.'A') THEN                     !2.9              
      IF (NAS.GT.0) THEN                                                        
        SCL=0                                                                   
        REF=0                                                                   
        WID=LASSOC(NAS)                                                         
      ELSE                                                                      
        SCL=SCALE                                                               
        REF=REFVAL                                                              
        WID=WIDTH                                                               
      ENDIF                                                                     
                                                                                
      IF (SCL.LE.5 .AND. SCL.GE.-4) THEN                    !1.10               
        TEN_TO_SCALE=TENTO(SCL)                             !1.10               
      ELSE                                                  !1.10               
        TEN_TO_SCALE=10.0**SCL                              !1.10               
      ENDIF                                                 !1.10               
                                                                                
!-----------------------------------------------------------------------        
! Generally in BUFR, data values of 'all ones' mean 'missing data'   !4         
! but there are a couple of exceptions, namely:                      !4         
!  (1) Descriptors of width 1 bit (e.g. 031031),                     !4         
!  (2) Associated data of width 2 bits (see 031021 code value 2).    !4         
! Set a flag to indicate whether missing data values are acceptable. !4         
!-----------------------------------------------------------------------        
                                                                                
      NOMISS = WID.EQ.1 .OR. (NAS.GT.0 .AND. WID.EQ.2)         !4               
                                                                                
! ----------------------------------------------------------------------        
! Avoid scaling negative values of class 13 elements when these are             
! code figures rather than precipitation amounts, i.e encode -1 or              
! -2 regardless of scale.                                                       
! ----------------------------------------------------------------------        
                                                                                
      IF (.NOT.CMPRES) THEN                                                     
        IF (VALUES(NOB,K).EQ.MIS) THEN                     !1.11a               
          INVAL=BONES(WID)                                     !b               
        ELSE                                                                    
          D=VALUES(NOB,K)                                                       
          IF (.NOT.(X.EQ.13 .AND. (REF.EQ.-1.OR.REF.EQ.-2)&                     
                         &.AND. D.LT.0)) D=D*TEN_TO_SCALE   !1.10               
          D=D-REF                                            !2.2               
                                                                                
          IF (D.GE.0.0 .AND. (D.LT.BONES(WID) .OR.&            !4               
               &(NOMISS .AND. D.EQ.BONES(WID)))) THEN          !4               
            INVAL=D+0.5                                      !2.2               
          ELSE                                               !2.2               
            WRITE (*,1) K,ID,D,WID,INDESC                    !2.2               
1           FORMAT (I3,'-TH VALUE (',I6.6,')',F12.0,' IN',&  !2.2               
                 &I3,' BITS - SET TO MISSING',I15)                              
            INVAL=BONES(WID)                                   !b               
          ENDIF                                                                 
        ENDIF                                                                   
                                                                                
        CALL VALOUT(STRING,IBEFOR,WID,INVAL)                                    
      ELSE                                                                      
        WIDEST=BONES(WID)                                      !b               
        MSFLAG=.FALSE.                                       !2.2               
        DO 410 J=1,NOBS                                                         
                                                                                
!-----------------------------------------------------------------------        
! Set any value too large for the field width to missing so as not   !4         
! to interfere with working out the base value. But check NOMISS     !4         
! to see if missing values are appropriate for this descriptor.      !4         
! Use double precision on the way from real to integer, to avoid                
! losing precision on multiplying by tento(scale).                              
!-----------------------------------------------------------------------        
                                                                                
        IF (VALUES(J,K).NE.MIS) THEN                        !1.11a              
          D=VALUES(J,K)                                                         
          IF (.NOT.(X.EQ.13 .AND. (REF.EQ.-1.OR.REF.EQ.-2)&                     
                         &.AND. D.LT.0)) D=D*TEN_TO_SCALE   !1.10               
          D=D-REF                                            !2.2               
                                                                                
          IF (D.GE.0.0 .AND. (D.LT.WIDEST .OR.&                !4               
               &(NOMISS .AND. D.EQ.WIDEST))) THEN              !4               
            INVAL=D+0.5                                      !2.2               
          ELSE                                               !2.2               
            WRITE (6,'(3(A,I4),A,I6.6,A,F10.0)') ' VALUE',J,&   !3              
               &' (OF',NOBS,') OF ELEMENT',K,' (',ID,') WAS',D  !3              
            WRITE (6,'(2A,I3,A,I25)') '  SET TO MISSING AS ',&  !3              
               &'TOO BIG FOR FIELD WIDTH OF',WID,' BITS',INDESC !3              
            INVAL=MISS                                                          
          ENDIF                                                                 
        ELSE                                                                    
          INVAL=MISS                                                            
        ENDIF                                                                   
                                                                                
        IF (NOMISS .AND. INVAL.EQ.MISS) INVAL = WIDEST         !4               
                                                                                
! ----------------------------------------------------------------------        
! Find max & min values for column  (working in integer)                        
! ----------------------------------------------------------------------        
                                                                                
        IF (J.EQ.1) THEN                                                        
          MIN=INVAL                                                             
          MAX=INVAL                                                             
          IF (INVAL.EQ.MISS) MSFLAG=.TRUE.                 !1.11a               
        ELSE                                                                    
          IF (INVAL.NE.MISS) THEN                          !1.11a               
            IF (INVAL.LT.MIN) MIN=INVAL                                         
            IF (MIN.EQ.MISS) MIN=INVAL                     !1.11a               
            IF (INVAL.GT.MAX) MAX=INVAL                                         
          ELSE                                                                  
            MSFLAG=.TRUE.                                                       
          ENDIF                                                                 
        ENDIF                                                                   
  410         CONTINUE                                                          
                                                                                
!-----------------------------------------------------------------------        
! Work out the greatest increment. Unless NOMISS is set, all ones    !4         
! means missing data so the range of values is one more than         !4         
! MAX-MIN. If all the values are the same, no increments will be     !4         
! coded. If all good values are the same but there are missing       !4         
! values too (as indicated by MSFLAG), set the range to 1.           !4         
!-----------------------------------------------------------------------        
                                                                                
        IF (MAX.GT.MIN) THEN                                                    
          MAXDIF=MAX-MIN+1                                                      
          IF (NOMISS) MAXDIF = MAXDIF - 1                      !4               
        ELSE IF (MIN.NE.MISS .AND. MSFLAG) THEN            !1.11a               
          MAXDIF=1                                                              
        ELSE                                                                    
          MAXDIF=0                                                              
        ENDIF                                                                   
        MSFLAG=.FALSE.   ! is this needed here???            !2.2               
                                                                                
! ----------------------------------------------------------------------        
! Find out how many bits needed to code increments (none if all values          
! same, one if some missing but others all the same, etc)                       
! (N.B. MAXDIF is integer, MAX & MIN real)                                      
! A 1-bit element should have an increment width of 1, not 2.        !f         
! (A warning will be issued below if any value of a 1-bit element    !f         
! being compressed is missing.)                                      !f         
! ----------------------------------------------------------------------        
                                                                                
        DO 420 J=0,30                                                           
         IF (MAXDIF.LE.BONES(J)) GO TO 421                     !b               
  420         CONTINUE                                                          
  421         CONTINUE                                                          
        INCWID=J                                                                
        IF (WIDTH.EQ.1 .AND. INCWID.GT.1) INCWID=1           !2.2               
                                                                                
! ----------------------------------------------------------------------        
! Encode the values of the given element. For NOBS reports there are            
! NOBS+2 values to go in the bit string, the first and second being             
! the minimum and the width of the increments.                                  
! If the minimum is missing, set the value to all ones, as many ones as         
! fill the field.                                                               
! ----------------------------------------------------------------------        
                                                                                
        IF (INCWID.GT.0) THEN                                                   
          NINCR=NOBS                                                            
        ELSE                                                                    
          NINCR=0                                                               
        ENDIF                                                                   
                                                                                
        DO 430 J=-1,NINCR                                                       
        IF (J.EQ.-1) THEN                                                       
          IF (MIN.EQ.MISS) THEN                            !1.11a               
            INVAL=BONES(WID)                                   !b               
          ELSE                                                                  
            INVAL=MIN                                                           
          ENDIF                                                                 
        ELSE IF (J.EQ.0) THEN                                                   
          INVAL=INCWID                                                          
          WID=6                                                                 
        ELSE                                                                    
          IF (VALUES(J,K).EQ.MIS) THEN                     !1.11a               
            IF (WIDTH.EQ.1) PRINT *,'BUFR ENCODE warning: ',&                   
           &INDESC,'is a 1-bit element but has a missing value' !f              
            INVAL=BONES(INCWID)                                 !b              
          ELSE                                                                  
            D=VALUES(J,K)                                                       
            IF (.NOT.(X.EQ.13 .AND. (REF.EQ.-1.OR.REF.EQ.-2)&                   
                           &.AND. D.LT.0)) D=D*TEN_TO_SCALE !1.10               
            INVAL=D+0.5-REF-MIN                                                 
            IF (INVAL.LT.0 .OR. INVAL.GT.BONES(INCWID)) THEN    !b              
              INVAL=BONES(INCWID)                               !b              
            ENDIF                                                               
          ENDIF                                                                 
          WID=INCWID                                                            
        ENDIF                                                                   
                                                                                
        CALL VALOUT(STRING,IBEFOR,WID,INVAL)                                    
  430         CONTINUE                                                          
      ENDIF                                                                     
      K=K+1                                                                     
                                                                                
!**************  true compression of characters is unlikely to be               
!             *  useful so code a zero base value & then the names              
! characters  *  themselves, unchanged - but if they are all the                
!             *  same, code one name and a zero increment width.                
!**************                                                                 
                                                                                
    ELSE IF (NAS.EQ.0 .AND. FORMAT.EQ.'A') THEN               !2.9              
      IF (.NOT.CMPRES) THEN                                                     
        NCHARS=WIDTH/8                                                          
        INM=VALUES(NOB,K)                                                       
!if defined (MVS)                                                               
        IF (INM.NE.MIS) CALL EB2ASC(NCHARS,NAMES(INM:))    !1.11a               
!endif                                                                          
        DO 510 J=0,NCHARS-1                                                     
         IF (INM.NE.MIS) THEN                              !1.11a               
           OCTET=ICHAR(NAMES(INM+J:INM+J))                                      
           IF (OCTET.LT.0) OCTET=OCTET+256                    !2.5              
         ELSE                                                                   
           OCTET=255                                                            
         ENDIF                                                                  
         CALL VALOUT(STRING,IBEFOR,8,OCTET)                                     
  510         CONTINUE                                                          
! Reset INAM, used if 205YYY, from INM (used only here) if not missing          
        IF (INM.NE.MIS) INAM=INM+NCHARS                    !1.11a               
      ELSE                                                                      
                                                                                
! ----------------------------------------------------------------------        
! *************************************************                             
! * If the message is to be compressed, compare   *                             
! * names till two are different or all the same  *                             
! *************************************************                             
! ----------------------------------------------------------------------        
                                                                                
        NCHARS=WIDTH/8                                                          
        INMX=0                                                                  
        MSFLAG=.FALSE.                                                          
        DIFLAG=.FALSE.                                                          
                                                                                
! ----------------------------------------------------------------------        
! Loop to set (perhaps) INMX, MSFLAG, DIFLAG.                                   
! (At the end INAM is set to the greatest pointer for values of this            
! element, in case any plain language follows)                                  
! ----------------------------------------------------------------------        
                                                                                
        DO 520 J=1,NOBS                                                         
        IJ=VALUES(J,K)                                                          
        IF (IJ.EQ.MIS) THEN                                !1.11a               
          MSFLAG=.TRUE.                                                         
        ELSE                                                                    
          IF (INMX.EQ.0) THEN                                                   
            INMX=IJ                                                             
          ELSE                                                                  
            IF(NAMES(IJ:IJ+NCHARS-1).NE.NAMES(INMX:INMX+NCHARS-1))&             
           &DIFLAG=.TRUE.                                                       
            IF (IJ.GT.INMX) INMX=IJ                                             
          ENDIF                                                                 
        ENDIF                                                                   
  520         CONTINUE                                                          
                                                                                
! ----------------------------------------------------------------------        
! Enough comparisons have now been done to decide (depending on which,          
! if any, of INAM, MSFLAG & DIFLAG have been set) between the following:        
! - all values are the same (missing or not)         (INCWID=0)                 
! - some values are not missing and are different    (INCWID=NCHARS)            
! (N.B. for characters INCWID is octets rather than bits!)                      
! ----------------------------------------------------------------------        
                                                                                
        IF (DIFLAG .OR. (MSFLAG.AND.INMX.GT.0)) THEN                            
                                                                                
! ----------------------------------------------------------------------        
!           *************************************************                   
!           *  The names are not all the same, so encode    *                   
!           *  zero base value & basic width & the names    *                   
!           *  themselves as increments.                    *                   
!           *************************************************                   
!       (INMX>0 implies that there is a name that is not missing!)              
! ----------------------------------------------------------------------        
                                                                                
          DO 540 J=1,NCHARS                                                     
           OCTET=0                                                              
           CALL VALOUT(STRING,IBEFOR,8,OCTET)                                   
  540           CONTINUE                                                        
                                                                                
          INCWID=NCHARS                                                         
          CALL VALOUT(STRING,IBEFOR,6,INCWID)                                   
                                                                                
          DO 550 I=1,NOBS                                                       
           INM=VALUES(I,K)                                                      
!if defined (MVS)                                                               
           IF (INM.NE.MIS) CALL EB2ASC(NCHARS,NAMES(INM:)) !1.11a               
!endif                                                                          
           DO 560 J=0,NCHARS-1                                                  
            IF (INM.NE.MIS) THEN                           !1.11a               
              OCTET=ICHAR(NAMES(INM+J:INM+J))                                   
              IF (OCTET.LT.0) OCTET=OCTET+256                 !2.5              
            ELSE                                                                
              OCTET=255                                                         
            ENDIF                                                               
            CALL VALOUT(STRING,IBEFOR,8,OCTET)                                  
  560            CONTINUE                                                       
  550           CONTINUE                                                        
        ELSE                                                                    
! ----------------------------------------------------------------------        
! *************************************************                             
! *  All the names are the same (but all missing  *                             
! *  if MSFLAG is set): encode one name (or ones  *                             
! *  if missing) and a zero increment width.      *                             
! *************************************************                             
! ----------------------------------------------------------------------        
                                                                                
!if defined (MVS)                                                               
          IF (INMX.GT.0) CALL EB2ASC(NCHARS,NAMES(INMX:))                       
!endif                                                                          
          DO 585 J=0,NCHARS-1                                                   
           IF (INMX.GT.0) THEN                                                  
             OCTET=ICHAR(NAMES(INMX+J:INMX+J))                                  
             IF (OCTET.LT.0) OCTET=OCTET+256                  !2.5              
           ELSE                                                                 
             OCTET=255                                                          
           ENDIF                                                                
           CALL VALOUT(STRING,IBEFOR,8,OCTET)                                   
  585           CONTINUE                                                        
          INCWID=0                                                              
          CALL VALOUT(STRING,IBEFOR,6,INCWID)                                   
        ENDIF                                                                   
! Reset INAM, used if 205YYY, from INMX (used only here) if not missing         
! (INMX rather than INM for compressed data: INMX is the max pointer            
! for this set of values - values could be pointed to out of order,             
! so INM might not point to the last value if there is more than one!)          
        IF (INMX.GT.0) INAM=INMX+NCHARS                                         
      ENDIF                                                                     
      K=K+1                                                                     
    ENDIF                                                                       
  400     CONTINUE                                                              
  ENDIF                                                                         
                                                                                
! ----------------------------------------------------------------------        
! * * *  Finally move pointer on to next descriptor  * * * * * * * * *          
! ----------------------------------------------------------------------        
                                                                                
  N=N+1                                                                         
ENDIF                                                                           
                                                                                
! ----------------------------------------------------------------------        
! N.B. after coding a character element INAM is left pointing to the            
! next characters in the names string in case there are inserted                
! characters (F=2,X=5)                                                          
!                                                                               
! N is incremented at the end of the F=0 & F=2 sections, K in                   
! the character & number subsections of F=0, because with q/c                   
! fields it must be incremented twice for numbers, N only once.                 
!                                                                               
!***********************************************************************        
!                                                                      *        
!  Loop round the descriptors, if there are any left. If not, there    *        
!  may be a further set of data for the same descriptor sequence,      *        
!  which is reusable in expanded form (only F=0 descriptors).          *        
!                                                                      *        
!***********************************************************************        
! ----------------------------------------------------------------------        
                                                                                
IF (N.LE.ND .AND. K.LE.NELEM) GO TO 20                                          
                                                                                
! ----------------------------------------------------------------------        
! Delete any F=2 descriptors listed for deletion                                
! ----------------------------------------------------------------------        
                                                                                
IF (N_DEL.GT.0) THEN                                        !1.10               
  DEL_ADDR(0)=0                                             !1.10               
  DEL_ADDR(N_DEL+1)=N                                       !1.10               
  IADD=1                                                    !1.10               
  DO I=0,N_DEL                                              !1.10               
    DO J=DEL_ADDR(I)+1,DEL_ADDR(I+1)-1                      !1.10               
      DESCR(IADD)=DESCR(J)                                  !1.10               
      IADD=IADD+1                                           !1.10               
    ENDDO                                                   !1.10               
  ENDDO                                                     !1.10               
  N=N-N_DEL                                                 !1.10               
  ND=ND-N_DEL                                               !1.10               
ENDIF                                                       !1.10               
                                                                                
! ----------------------------------------------------------------------        
! If several sets of data, but no compression, go round again.                  
! ----------------------------------------------------------------------        
                                                                                
IF (QCNSEQ.GT.0) THEN          ! if there has been a 223YYY !q/c                
  DO 700 I=QCSTAR+QCNSEQ,ND    ! but no 223000, delete the  !q/c                
   DESCR(I-QCNSEQ)=DESCR(I)    ! descriptors which defined  !q/c                
700   CONTINUE                                                                  
  ND=ND-QCNSEQ                 ! the Q/C sequence.          !q/c                
ENDIF                                                                           
                                                                                
! Reset the descriptors to the input sequence to expand again    !1.12b         
! from scratch. Stop if not all the input descriptors were kept. !1.12b         
                                                                                
IF (.NOT.CMPRES .AND. NOB.LT.NOBS) THEN                                         
  IF (INPUTND.LE.LISTDIM) THEN                             !1.12b               
    ND=INPUTND                                             !1.12b               
    DO I=1,ND                                              !1.12b               
      DESCR(I)=INDES(I)                                    !1.12b               
    ENDDO                                                  !1.12b               
    N=1                                                    !1.12b               
  ELSE                                                     !1.12b               
    print *,' More than',LISTDIM,'descriptors input'       !1.12b               
    print *,' - too many for array in which copy is kept ' !1.12b               
    print *,' to reexpand if NOBS>1 without compression. ' !1.12b               
    print *,' Change LISTDIM to keep more descriptors.'    !1.12b               
  ENDIF                                                    !1.12b               
  NOB=NOB+1                                                                     
  GO TO 10                                                                      
ENDIF                                                                           
                                                                                
! ----------------------------------------------------------------------        
! Finally store length of bit string, rounded up to nearest halfword            
! and initialise octet 4 of section 4 to zero. (S.Cox 27-05-96)                 
! ----------------------------------------------------------------------        
                                                                                
999   CONTINUE                                                                  
L=(IBEFOR+7)/8                                                                  
IF (MOD(L,2).EQ.1) L=L+1                                                        
L4=L                                                                            
IBEFOR=0                                                                        
CALL VALOUT(STRING,IBEFOR,24,L)                                                 
STRING(4:4)=CHAR(0)                     ! S.Cox 27-05-96                        
                                                                                
RETURN                                                                          
END SUBROUTINE ENCODE                                                           
