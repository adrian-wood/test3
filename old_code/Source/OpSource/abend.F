#if defined (MVS)
ABEND     CSECT
          CHAIN
***********************************************************************
*                                                                     
* PROGRAM       : ABEND                                               
*                                                                     
* PURPOSE       : WRITTEN FOR IDMS TAF/METAR STORAGE TO PRESERVE DATA 
*                 BASE ABEND CODES RATHER THAN LET FORTRAN LOSE THEM. 
*                 NOW USED TO STOP FORTRAN TRYING TO CARRY ON AFTER A 
*                 WRITE TO AN MDB DATA SET HAS FAILED.                
*                                                                     
* CALLED BY     : TAFREP                                              
*                                                                     
* REVISION INFO :
*
* $Revision: 1$
* $Date: 30/01/2006 20:20:32$
* $Source: /home/us0400/mdb/op/lib/source/RCS/abend.F,v $
*
* CHANGE RECORD :                                                     
*
* $Log:
*  1    Met_DB_Project 1.0         30/01/2006 20:20:32    Sheila Needham  
* $
* Revision 2.0  2001/05/31 13:27:16  usmdb
* Preprocessor directives changed to if defined (MVS), else...
* Added copyright and modified header - S.Cox
*
* Revision 1.2  99/04/29  15:21:18  15:21:18  usmdb
* Correct comment leaders - S.Cox
* 
* Revision 1.1  99/04/29  15:06:17  15:06:17  usmdb
* Initial revision
*
* MAY 88   ZERO STAE & SPIE TO STOP FORTRAN LOSING THE ABEND CODE     A
*                                                                     
***********************************************************************
* (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
*
* Met Office, United Kingdom.
*
* The use, duplication and disclosure of this code is strictly
* prohibited without the permission of The Meteorological Database
* Team at the above address.
***********************************************************************

          L    1,0(1)       POINT R1 TO RETURN CODE IN CHARACTERS
          PACK DW,0(4,1)
          ESTAE 0
          SPIE
          CVB  1,DW         RETURN CODE IN BINARY IN R1
          LA   0,128        DUMP OPTION
          SLL  0,24         MOVE TO LEFT-HAND BYTE
          OR   1,0          COMBINE WITH RETURN CODE
          SVC  13           ABEND
DW        DC   D'0'
          DC   C'$Date: 30/01/2006 20:20:32$ $Revision: 1$'
          END
#else
      SUBROUTINE ABEND ()

!-----------------------------------------------------------------------
!
! PURPOSE :
!
! To halt the calling program.
! (This is a niaive version of an MVS assembler program used to 
! halt a program preserving any abend code)
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      CHARACTER*132 HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/abend.F,v $
     &'//'$ $Date: 30/01/2006 20:20:32$ $Revision: 1$'
   
      STOP
      END
#endif
