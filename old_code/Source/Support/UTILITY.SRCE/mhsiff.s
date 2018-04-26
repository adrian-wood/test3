MHSIFF   TITLE 'MHSIFF MHS Information File Supervisor mk3 (for FTP)'
* ---------------------------------------------------------------------
* PROGRAM    : MHSIFF  MHS Information File Supervisor FTP version
* LOCATION   : MHS.LIB.ASM, MHS.LIB.LOAD
* TYPE       : Subr CSECT
* SITE       : Met. Office
* LANGUAGE   : 370 Assembler
* PROGRAMMER : Martyn Catlow  (Ian Cook FTP adaption using CSI)
* DEPARTMENT : Central Computing(2)
* WRITTEN    : 12/91
* VERSION    : 1.4  07/01/94  11:00   N Hancock
*            : Call it MHSIFF (from MHSIFS) and load dynamically.
* VERSION    : 3.0  06/05/98  Ian Cook
*            : Adapted for reception of FTP based transfers.  Changed
*            : to use Catalog Search Interface, instead of AMS or FDR.
* ---------------------------------------------------------------------
*  This is a copy of version 3.0 of MHSIFF taken from MHS.FTP.SRCE in
* May 2007. It doesn't contain Martyn Catlow's updates (version 3.2)
* for fixing the occasional problem of premature access to an MHS data
* set at the time it is created. It is preserved here for use by batch
* MetDB storage jobs and test MetDB storage runs as, unlike version
* 3.2, it does not need to be run from an 'authorised' library.
* ---------------------------------------------------------------------
*
* OVERVIEW   : MHSIFF is responsible for User Agent (UA) file handling,
*              it may be called to Get, Rename, or Free a file.
*
*
* FUNCTION   : When called to Get a file, MHSIFS calls MHSIFF if the
*              environment is non authorised. MHSIFF issues a CSI
*              catalog search for the given HLQ.  If no files are
*              found, control is returned to the caller; otherwise a
*              single file will be allocated with a unique ddname for
*              each call.
*
*              If the file is sucessfully processed MHSIXM (MHS
*              Information Teansmission Manager), or any other caller,
*              will make a call to MHSIFS to Rename the UA dataset, to
*              prevent its re-selection. MHSIFS will call MHSIFF or
*              MHSIFL to do the rename,depending on environment.
*
*              When a file is unsucessfully processed, MHSIXM will
*              call MHSIFS to Free the file for later re-transmission.
*
*
* CALL PARMS : CALL MHSIFF,(DSN,DDN,DSP,OWN,RCI)
*
*              DSN (C*44) - The UA file datasetname (returned on Get)
*              DDN (C*08) - The UA file ddaname (returned on Get, and
*                           provided by caller on Free or Rename.)
*              DSP (C*06) - DSP(1) = F(ree), R(ename), G(et), I(nit)
*                           DSP(2) = HLQ type (X,R,P,F etc)
*                           DSP(3) = Test Data( T)
*                           DSP(4) = File Type (N, nonvsam)
*                           DSP(5) = Class code (G,O,C etc)
*                           DSP(6) = Dest Code (0,1,2,3 etc)
*              OWN (C*08) - Owners id or datatype
*              RCI (C*32) - Diagnostic area
*
*  NOTE: DSP(2 to 6) are unused by MHSIFF
* --------------------------------------------------------------------
MHSIFF   AMODE    31
MHSIFF   RMODE    24
MHSIFF   CSECT
         PRINT    NOGEN
         EQUREG   R
         SAVE     (14,12),,*                Establish adressability
         LR       R12,R15
         USING    MHSIFF,R12
* --------------------------------------------------------------------
         LR       R8,R1                     Save parmlist
         LH       R0,H4096                  Chain save areas
         GETMAIN  R,LV=(0)
         ST       R13,4(,R1)
         ST       R1,8(,R13)
         LR       R13,R1
         LH       R0,H4096
         GETMAIN  R,LV=(0),LOC=(RES,ANY)    Get request block w/s
         ST       R1,ARB
*----------------------------------------------------------------------
         LM       R2,R6,0(R8)               DSN, DDN, DISP, ORIGIN, RC
         ST       R2,ADSNR                  DSN for caller
         ST       R3,AFSADD                 DDname
         L        R4,0(R4)                  Actual Disposition
         ST       R4,DISP                   4 chars sufficent
         ST       R5,AFSOWN                 Originator
         CLC      BNKOWN,0(R5)              Owner specified ?
         BE       STRC                      No, then use MHSX HLQ
*
         MVC      HLQ,HLQMHSR               Set received file HLQ
STRC     ST       R6,ARTCD                  Return code
         XC       0(8,R6),0(R6)             Clear return code area
*----------------------------------------------------------------------
FSCKE    CLI      DISP,C'E'                 END ?
         BE       FSEN01                    Shutdown cleanly
*----------------------------------------------------------------------
FSCKF    CLI      DISP,C'F'                 FREE a file ?
         BNE      FSCKR                     See if rename
         L        R3,AFSADD                 File name
         LA       R2,DDNAME1                for this unallocation
         LA       R4,7
         EX       R4,MVCVAR
         BAL      R14,FSDY02                Go free it.
         BAL      R14,FSDYER
         LTR      R15,R15
         BE       FSR0                      Ok
         B        FSR4                      Problem, but continue
*----------------------------------------------------------------------
FSCKR    CLI      DISP,C'R'                 RENAME a file ?
         BNE      FSCKD                     Check if get
         BAL      R14,FSAMRE                Go Rename it
         BAL      R14,FSAMER                Must assume it renames
         LTR      R15,R15
         BE       DUNAL
         WTO      'MHS450E IFS Rename request failed ',ROUTCDE=(11,8)
*
DUNAL    L        R3,AFSADD                 File name
         LA       R2,DDNAME1                for this unallocation
         LA       R4,7
         EX       R4,MVCVAR
         BAL      R14,FSDY02                Now free allocation
         BAL      R14,FSDYER                Process the error
         LTR      R15,R15
         BE       FSR0                      Ok
         B        FSR4                      Problem, but continue
*----------------------------------------------------------------------
FSCKD    CLI      DISP,C'D'                 DELETE a file ?
         BNE      FSCKG                     Check if get
         BAL      R14,FSAMDE                Go Delete it
         LTR      R15,R15
         BE       FSR0
         WTO      'MHS450E IFS Delete request failed ',ROUTCDE=(11,8)
         B        FSR4
*----------------------------------------------------------------------
FSCKG    CLI      DISP,C'G'                 GET ? If not, bad parm.
         BE       TSTIN
         BAL      R14,FSER01                Good Grief, parameter error
         B        FSR12
*
TSTIN    TM       IFLAGS,INIT               Init flags set ? No,
         BZ       FSGET                     Get next file, if any
         NI       IFLAGS,FF-INIT            Turn off INIT flag
         TM       IFLAGS,TEMPF              DDs open ?
         BZ       FSAM01                    Get next file, if any
*
         NI       IFLAGS,FF-TEMPF           Turn off TEMPF flag
         BAL      R14,FSDY03                Allocate SYSIN etc
         BAL      R14,FSDYER
         LTR      R15,R15
         BNE      FSR12                     Big problem
*
FSAM01   LA       R3,L31B02                 Here we go again
         LA       R2,L24B02
         BSM      R3,R2
L24B02   OPEN     (AMSCNTL,(OUTPUT))        Set up the CSI LISTCAT
         PUT      AMSCNTL,CNTL1             control card
         CLOSE    AMSCNTL
         BSM      0,R3
         PRINT GEN
L31B02   FREEPOOL AMSCNTL                   Free up storage
         PRINT NOGEN
*
*        LINK     EP=MHSCSI                 Invoke CSI to do a LISTC
         CALL     MHSCSI                    Invoke CSI to do a LISTC
         BAL      R14,FSAMER                CSI error to process
         LTR      R15,R15
         BNE      FSR8                      Nonzero must be a problem
*
OPNSY    LA       R3,L31B03                 Getting used to this
         LA       R2,L24B03
         BSM      R3,R2
L24B03   OPEN     SYSPRINT                  Open Sysprint for scan
         BSM      0,R3
L31B03   EQU      *                         What does caller want ?
         SPACE
*----------------------------------------------------------------------
* FSGET - Get a file from sysprint listing and allocate it
*----------------------------------------------------------------------
FSGET    LA       R3,L31B04                 Get a UA file
         LA       R2,L24B04
         BSM      R3,R2
L24B04   GET      SYSPRINT                  Get LISTCAT record
         BSM      0,R3
L31B04   TM       IFLAGS,EOD                End of data ?
         BZ       FSCK01                    No, process the record
*
         LA       R3,L31B06                 Go to 24 bit mode
         LA       R2,L24B06
         BSM      R3,R2
L24B06   CLOSE    (SYSPRINT)                Close SYSPRINT
         BSM      0,R3
L31B06   NI       IFLAGS,FF-EOD             Turn off EOD flag
         OI       IFLAGS,INIT               Make for INIT next call
         B        FSR8                      Set RC=8 (EOD)
*
FSCK01   CLC      1(7,R1),=C'NONVSAM'       One we're interested in?
         BNE      FSGET                     No ?, get next record
*        XR       R4,R4                     Clear R4
*        ICM      R4,3,0(R1)                RDW record length
*        SH       R4,=H'21'                 Minus rubbish
         LA       R3,16(R1)                 Point at DSN.
         LR       R5,R3                     Starting point of dsn
         LA       R4,44                     Max dsn length
CKSP     CLI      0(R3),C' '                Check for space
         BE       FNDLN                     Found end of DSN
         LA       R3,1(R3)                  Point to next char
         BCT      R4,CKSP                   Go check next char
         B        FSGET                     This isnt a DSN, get next
*
FNDLN    SR       R3,R5                     calculate dsn length
         ST       R3,WKDSNL                 and length
         LA       R3,16(R1)                 Reload start of DSN
         ST       R3,AWKDSN                 Save DSN address
*
         L        R2,AFSOWN                 Identify owner of file
         CLC      BNKOWN,0(R2)              Don't care ?
         BE       CKTST                     No, we don't
*
         L        R3,AWKDSN                 A(DSN)
         LA       R3,22(R3)                 Look at owner
         CLC      0(4,R3),0(R2)             Is this my kind of Gal ?
         BNE      FSGET                     No, skip it.
*
CKTST    L        R3,AWKDSN                 A(DSN)
         LA       R3,5(R3)                  Look at data class
         CLI      0(R3),C'T'                Test dataset ? Skip it.
         BE       FSGET
*
         L        R3,AWKDSN                 Get DSN address
         LA       R6,MHSDSN                 MHSDSN resource
         ENQ      ((R6),(R3),E,35,SYSTEM),RET=TEST
         LTR      R15,R15                   Resource enqued ?
         BNE      FSGET                     Nonzero means yes so skip.
*
         GETMAIN RU,LV=GQLENG+RIBLENG
         LR       R5,R1
         GQSCAN  AREA=((5),RIBLENG),REQLIM=0,SCOPE=ALL,                *
               RESNAME=(SYSDSN,(3),(4))
         LR       R2,R15
         FREEMAIN RU,LV=GQLENG+RIBLENG,A=(R5)
         C        R2,F4
         BNE      FSGET                     Must be in use
*
         MVC      DSNAME1,DSNBLNK           Clear alloc DSN
         L        R3,AWKDSN                 A(DSN)
         L        R4,WKDSNL                 length of DSN
         BCTR     R4,0                      -1 for move
         LA       R2,DSNAME1                Dynalloc DSN
         EX       R4,MVCVAR                 Move it
         L        R2,ADSNR                  Set callers DSN to blank
         LA       R3,DSNBLNK
         EX       R4,MVCVAR                 Move it
*
         LA       R4,43                     Move the whole thing
         LA       R3,DSNAME1                Dynalloc DSN
         L        R2,ADSNR                  Callers DSN area
         EX       R4,MVCVAR                 Move it
*
         BAL      R14,FSDY01                Allocate UA dataset
         BAL      R14,FSDYER                Process error
         LTR      R15,R15
         BNE      FSGET                     Problem, but continue
*
         L        R2,AFSADD                 Callers DDNAME area
         LA       R3,DDNAME1                DDNAME allocated
         LA       R4,7
         EX       R4,MVCVAR
*
         B        FSR0                      Neat return
         SPACE
*----------------------------------------------------------------------
* FSAMRE - Rename a UA file after processing
*----------------------------------------------------------------------
FSAMRE   ST       R14,FSAMRES               Save return address
         MVC      RENDSN,DSNBLNK            Blank out AMS DSNs
         MVC      RENDSP,DSNBLNK
         LA       R4,43                     Move in the full
         L        R3,ADSNR                  DSN supplied by caller
         LA       R2,RENDSN                 Old name
         EX       R4,MVCVAR
         LA       R2,RENDSP                 New name
         EX       R4,MVCVAR
         MVI      RENDSP+3,C'P'             P is for processed
         LA       R3,L31B07                 Now go and rename it
         LA       R2,L24B07
         BSM      R3,R2
L24B07   OPEN     (AMSCNTL,(OUTPUT))        Assume all is well !
         PUT      AMSCNTL,CNTL2
         PUT      AMSCNTL,CNTL3
         CLOSE    AMSCNTL
         BSM      0,R3
L31B07   FREEPOOL AMSCNTL
         LINK     EP=IDCAMS,PARAM=(H0,ALTDD),VL=1
         BAL      R14,FSAMER                Process any errors
*
FSRTN0   L        R14,FSAMRES
         BR       R14
FSAMRES  DC       F'0'
         SPACE
*----------------------------------------------------------------------
* FSAMDE - Delete a UA file after processing
*----------------------------------------------------------------------
FSAMDE   ST       R14,FSAMDES               Save return address
         TM       IFLAGS,TEMPF              Init flags set ? No,
         BZ       DEOK                      Do the delete
*
         NI       IFLAGS,FF-TEMPF           Turn off TEMPF flag
         BAL      R14,FSDY03                Allocate SYSIN etc
*
DEOK     MVC      DELDSN,DSNBLNK            Blank out AMS DSN
         LA       R4,43                     Move in the full
         L        R3,ADSNR                  DSN supplied by caller
         LA       R2,DELDSN                 Cluster name
         EX       R4,MVCVAR
         LA       R3,L31B08
         LA       R2,L24B08
         BSM      R3,R2
L24B08   OPEN     (AMSCNTL,(OUTPUT))        Assume all is well !
         PUT      AMSCNTL,CNTL4
         CLOSE    AMSCNTL
         BSM      0,R3
L31B08   FREEPOOL AMSCNTL
         LINK     EP=IDCAMS,PARAM=(H0,ALTDD),VL=1
         BAL      R14,FSAMER                Process any errors
*
         L        R14,FSAMDES
         BR       R14
FSAMDES  DC       F'0'
         SPACE
*----------------------------------------------------------------------
* FSEN01 - Come here if caller requested a happy ending
*----------------------------------------------------------------------
FSEN01   LA       R3,L31B05                 DISP must be END
         LA       R2,L24B05                 Last time chaps
         BSM      R3,R2
L24B05   CLOSE    (SYSPRINT,,AMSPRINT)
         BSM      0,R3
L31B05   BAL      R14,FSDY04                De-allocate FSALOG
         BAL      R14,FSDYER                Dont care if it failed
         B        FSR0
*
FSR12    LA       R2,12                     A catastrophe, can't go on
         B        FSR
FSR8     LA       R2,8                      No data to process
         B        FSR
FSR4     LA       R2,4                      File unallocation failed
         B        FSR
FSR0     LA       R2,0
FSR      L        R1,ARB
         LH       R0,H4096
         FREEMAIN R,LV=(0),A=(R1)
         LR       R1,R13                    Restore save areas
         L        R13,4(,R13)
         LH       R0,H4096
         FREEMAIN R,LV=(0),A=(R1)
         LR       R15,R2
         RETURN   (14,12),RC=(15)
         SPACE
*---------------------------------------------------------------------
* Parameter error: Class 3, ARTCD->(F'3',R15)
*----------------------------------------------------------------------
FSER01   ST       R14,FSER01S               Save return address
         L        R2,ARTCD                  Address of diagnostic area
         LA       R3,3                      Error type 3 indicator
         ST       R3,0(R2)                  Save
         LA       R3,12                     Serious error
         ST       R3,4(R2)                  Save
         MVC      PEDISP,DISP               Move in bad parm
         WTO      MF=(E,PEW)                Issue a WTO
         LA       R15,12                    Set return code
         L        R14,FSER01S               Go back
         BR       R14
FSER01S  DC       F'0'
PEW      WTO      'MHS451E IFS Parameter error ID=  ',ROUTCDE=(11),MF=L
PEDISP   EQU      PEW+36,1
         SPACE
*----------------------------------------------------------------------
* IDCAFS error: Class 2, ARTCD->(F'2',R15)
*----------------------------------------------------------------------
FSAMER   ST       R14,FSAMERS               Save return address
         LTR      R15,R15                   Was there an error ?
         BE       FSAMR0                    No ?, then go back
*
         C        R15,F8                    Serious error ?
         BL       FSAMR0                    We have something then
*
         OI       IFLAGS,INIT               Make for INIT next call
         L        R2,ARTCD                  Diagnostic area
         LA       R3,2                      AMS error indicator
         ST       R3,0(R2)                  Save
         ST       R15,4(R2)                 Save R15
FSAMR8   LA       R15,8                     Set R15 in error
         B        FSAMR
FSAMR0   LA       R15,0                     Set R15 to zero
FSAMR    L        R14,FSAMERS
         BR       R14
FSAMERS  DC       F'0'
         SPACE
*----------------------------------------------------------------------
* EOD Error: Class 4, ARTCD->(F'4',R15)
*----------------------------------------------------------------------
FSSYEND  OI       IFLAGS,EOD                 Set End of data flag
         L        R2,ARTCD                   Addrees of diag
         LA       R15,4                      error class
         ST       R15,0(R2)                  Save EOD indicator
         LA       R15,8                      Return code 8
         ST       R15,4(R2)                  Set return code
         BSM      0,R3
         SPACE
*----------------------------------------------------------------------
* DYSETUP - Set most dynalloc parms
*----------------------------------------------------------------------
         USING    S99RBP,R5
         USING    S99RB,R4
DYSETUP  ST       R14,UADYSUS               Save return address
         L        R5,ARB                    Storage
         LA       R4,4(R5)                  Address of RB
         XC       S99RB(S99RBEND-S99RB),S99RB
         ST       R4,S99RBPTR               Store RB address
         OI       S99RBPTR,S99RBPND         and turn on last flag
         MVI      S99RBLN,S99RBEND-S99RB    Set length of RB
         OI       S99FLAG1,S99NOCNV         Set Flags
         LA       R1,S99RBP
         L        R14,UADYSUS
         BR       R14
UADYSUS  DC       F'0'
*---------------------------------------------------------------------
* FSDY01 - Allocate user agent message dataset
*----------------------------------------------------------------------
FSDY01   ST       R14,FSDY01S               Save return address
*
         BAL      R14,DYSETUP
         OI       S99VERB,S99VRBAL          Set allocation verb
         LA       R2,FSADSA                 Allocation text
         ST       R2,S99TXTPP               Store in text pointer
         DYNALLOC                           Go to it
         LTR      R15,R15
         BNE      BRBK                      Bad soul
*
         BAL      R14,DYSETUP               Now get the DDname
         OI       S99VERB,S99VRBIN          Set info verb
         LA       R2,FSADSI                 Allocation text
         ST       R2,S99TXTPP               Store in text pointer
         DYNALLOC                           Go to it
*
         MVC      DDNAME1,DDNAMEI           copy the ddname
BRBK     L        R14,FSDY01S               Restore return address
         BR       R14                       Go back
*
FSDY01S  DC       F'0'                      Return address
         SPACE
*----------------------------------------------------------------------
* FSDY02 - De-alocate user agent message file
*----------------------------------------------------------------------
FSDY02   ST       R14,FSDY02S               Save return address
         BAL      R14,DYSETUP
         XC       S99VERB,S99VERB
         OI       S99VERB,S99VRBUN          Set Deallocation verb
         LA       R2,FSADSU                 Indicate unalloc only
         ST       R2,S99TXTPP
         DYNALLOC
*
         L        R14,FSDY02S               Restore return address
         BR       R14                       Goback
FSDY02S  DC       F'0'
         SPACE
*---------------------------------------------------------------------
* FSDY03 - Allocate SYSIN/SYSPRINT files
*----------------------------------------------------------------------
FSDY03   ST       R14,FSDY03S               Save return address
*                                           SYSIN
         BAL      R14,DYSETUP
         OI       S99VERB,S99VRBAL          Set allocation verb
         LA       R2,FSASIA                 Allocation text
         ST       R2,S99TXTPP               Store in text pointer
         DYNALLOC                           Go to it
*                                           SYSPRINT
         BAL      R14,DYSETUP
         OI       S99VERB,S99VRBAL          Set allocation verb
         LA       R2,FSASPA                 Allocation text
         ST       R2,S99TXTPP               Store in text pointer
         DYNALLOC                           Go to it
*                                           AMSPRINT
         BAL      R14,DYSETUP
         OI       S99VERB,S99VRBAL          Set allocation verb
         LA       R2,FSAAMA                 Allocation text
         ST       R2,S99TXTPP               Store in text pointer
         DYNALLOC                           Go to it
*
         L        R14,FSDY03S               Restore return address
         BR       R14                       Goback
FSDY03S  DC       F'0'
         SPACE
*----------------------------------------------------------------------
* De-alocate SYSIN/SYSPRINT
*----------------------------------------------------------------------
FSDY04   ST       R14,FSDY04S               Save return address
*                                           SYSIN De-alloc
         BAL      R14,DYSETUP
         OI       S99VERB,S99VRBUN          Set Deallocation verb
         LA       R2,FSASIU                 Unallocate it
         ST       R2,S99TXTPP
         DYNALLOC
*                                           SYSPRINT De-alloc
         BAL      R14,DYSETUP
         OI       S99VERB,S99VRBUN          Set Deallocation verb
         LA       R2,FSASPU                 Unalloc
         ST       R2,S99TXTPP
         DYNALLOC
*                                           AMSPRINT De-alloc
         BAL      R14,DYSETUP
         OI       S99FLAG1,S99NOCNV         Set flags
         LA       R2,FSAAMU                 Unalloc
         ST       R2,S99TXTPP
         DYNALLOC
*
         L        R14,FSDY04S               Restore return address
         BR       R14                       Goback
FSDY04S  DC       F'0'
         SPACE
*----------------------------------------------------------------------
* DYNALLOC Error: Class 1, ARTCD->(F'1',R15,S99ERROR,S99INFO)
*----------------------------------------------------------------------
FSDYER   ST       R14,FSDYERS               Save return address
         LTR      R15,R15                   Check if there was an error
         BE       FSDYER0                   No, then return
         L        R2,ARTCD                  Load addrees of diag area
         LA       R3,1                      Indicate a class 1 error
         ST       R3,0(R2)                  Save
         ST       R15,4(R2)                 Save R15 return code
         XR       R3,R3                     Clear R3
         LH       R3,S99ERROR               Load SVC99 Error code
         C        R3,F5896                  Dataset gone ?
         BE       FSDYER8                   Not an error
*
         ST       R3,8(R2)                  Save
         CVD      R3,WS1D                   Convert error code
         UNPK     WS1F,WS1D+4(4)
         OI       WS1F+3,X'F0'
         MVC      DYWERR,WS1F
         L        R15,4(R2)
         CVD      R15,WS1D                  Now issue a WTO
         UNPK     WS1F,WS1D+6(2)
         OI       WS1F+3,X'F0'
         MVC      DYWR15,WS1F+2
         MVC      DYWRDSN,DSNBLNK           If DSN is blank then we
         TM       S99VERB,S99VRBUN          must be doing a dealloc
         BNZ      MVDDN                     based on DDNAME only.
         MVC      DYWRDSN,DSNAME1
MVDDN    MVC      DYWRDDN,DDNAME1
         WTO      MF=(E,DYW)
         WTO      MF=(E,DYW2)
FSDYER8  LA       R15,8                     Serious allocation error
         B        FSDYE
FSDYER4  LA       R15,4                     Dataset is in use
         B        FSDYE
FSDYER0  LA       R15,0                     Ensure Good RC
FSDYE    L        R14,FSDYERS               Go home.
         BR       R14
FSDYERS  DC       F'0'
DYW      WTO      'MHS452E IFS DYNER R15=nn, ER=nnnn                   *
                                                  ',ROUTCDE=(11),MF=L
DYW2     WTO      'MHS452E IFS DYNER DDN=                              *
                                                  ',ROUTCDE=(11),MF=L
DYWR15   EQU      DYW+26,2
DYWERR   EQU      DYW+33,4
DYWRDSN  EQU      DYW+40,44
DYWRDDN  EQU      DYW2+26,8
         SPACE
*----------------------------------------------------------------------
* Dynamic allocation text
*----------------------------------------------------------------------
RB       DC  CL20' '                             Request block
ARB      DS  0F                                  RB pointer on FW
         DC  X'80',AL3(RB)
*
FSADSA   DC  A(DSN1),A(STATKE)                   UA dataset
         DC  X'80',AL3(STATOL)                   Old
FSADSU   DC  A(STATKE),X'80',AL3(DDN1)
FSADSI   DC  A(DSN1),X'80',AL3(DDNI)             Inquire
*
FSASPA   DC  A(DSN2),A(DDN2)                     Sysprint
         DC  A(UNIT),A(TRK),A(PRIM1),A(SEC)
         DC  X'80',AL3(STATNE)                   New
FSASPU   DC  A(DSN2),A(STATDE)                   Delete
         DC  X'80',AL3(DDN2)
*
FSASIA   DC  A(DSN3),A(DDN3)                     Sysin
         DC  A(UNIT),A(TRK),A(PRIM1)
         DC  A(BLKSIZ),A(LRECL),A(RECFM)
         DC  X'80',AL3(STATNE)                   New
FSASIU   DC  A(DSN3),A(STATDE)                   Delete
         DC  X'80',AL3(DDN3)
*
FSAAMA   DC  A(DSN4),A(DDN4)                     Amsprint
         DC  A(UNIT),A(CYL),A(PRIM1),A(SEC)
         DC  X'80',AL3(STATNE)                   New
FSAAMU   DC  A(DSN4),A(STATDE)                   Delete
         DC  X'80',AL3(DDN4)
*
DSN1     DC  AL2(02),AL2(01),AL2(44)             UA file DSN
DSNAME1  DC  CL44'MHSX.PnDyyddd.Thhmmss.jjjjjjjj.Snnn'
DDN1     DC  AL2(01),AL2(01),AL2(08)
DDNAME1  DC  CL8'SYSnnnnn'
DDNI     DC  AL2(04),AL2(01),AL2(08)
DDNAMEI  DC  CL8'SYSnnnnn'
*                                                SYSPRINT file
DSN2     DC  AL2(02),AL2(01),AL2(44)
DSNAME2  DC  CL44'&&FSASYP'
DDN2     DC  AL2(01),AL2(01),AL2(08)
DDNAME2  DC  CL8'SYSPRINT'
*                                                SYSIN file
DSN3     DC  AL2(02),AL2(01),AL2(44)
DSNAME3  DC  CL44'&&FSASYI'
DDN3     DC  AL2(01),AL2(01),AL2(08)
DDNAME3  DC  CL8'SYSIN'
*                                                AMSPRINT file
DSN4     DC  AL2(02),AL2(01),AL2(44)
DSNAME4  DC  CL44'&&FSASYA'
DDN4     DC  AL2(01),AL2(01),AL2(08)
DDNAME4  DC  CL8'AMSPRINT'
*
STATSH   DC  AL2(04),AL2(01),AL2(01),X'08'       Shr
STATNE   DC  AL2(04),AL2(01),AL2(01),X'04'       New
STATOL   DC  AL2(04),AL2(01),AL2(01),X'01'       Old
*
STATKE   DC  AL2(05),AL2(01),AL2(01),X'08'       Keep
STATDE   DC  AL2(05),AL2(01),AL2(01),X'04'       Delete
*
UNIT     DC  AL2(21),AL2(01),AL2(05),CL5'SYSSQ'
TRK      DC  AL2(07),AL2(00)
CYL      DC  AL2(08),AL2(00)
PRIM1    DC  AL2(10),AL2(01),AL2(03),AL3(01)
SEC      DC  AL2(11),AL2(01),AL2(03),AL3(03)
BLKSIZ   DC  AL2(48),AL2(01),AL2(02),AL2(80)
LRECL    DC  AL2(66),AL2(01),AL2(02),AL2(80)
RECFM    DC  AL2(73),AL2(01),AL2(01),AL1(144)
         SPACE
*----------------------------------------------------------------------
* Working storage areaa
*----------------------------------------------------------------------
MVCVAR   MVC      0(0,R2),0(R3)       General purpose MVC
F1       DC       F'1'                One
F4       DC       F'4'                Four
F8       DC       F'8'                Eight
F5896    DC       F'5896'             Five eight nine six
H0       DC       H'0'                Half zero
H4096    DC       H'4096'             Half 1 page
AWKDSN   DC       F'0'                Address of current DSN
WKDSNL   DC       F'0'                Address of current DSN
BNKOWN   DC       CL8' '              No owner
SYSDSN   DC       CL8'SYSDSN'         QSCAN resource name
FSADD    DC       CL8'FSAFInn'        Cyclic number 00 to 99
AFSADD   DC       F'0'                address of FSADD
FSAOWN   DC       CL8'       '        Origin user or jobname
AFSOWN   DC       F'0'                address of originator
WS1D     DC       D'0'                Convert ws
WS1F     DC       F'0'                Convert ws
MHSDSN   DC       CL8'MHSDSN'         ENQ major name
RIBLENG  EQU      ((RIBEND-RIB)+(RIBEEND-RIBE)*100)
GQLENG   EQU      48
IFLAGS   DC       X'03'               INIT set on
EOD      EQU      X'04'               End of data flag
TEMPF    EQU      X'02'               Sysin/out files open
INIT     EQU      X'01'               Init flag
FF       EQU      X'FF'               All flags on
ADSNR    DC       F'0'                DSN for caller
DISP     DC       F'0'                File disposition
ARTCD    DC       F'0'                return code
DSNBLNK  DC       CL44' '             For reseting dataset names
CNTL1    DS       0CL80               Listcat IDCAMS card
HLQ      DC       CL04'MHSX'          HLQ of UA files
FILLER1  DC       CL76'.** '
CNTL2    DS       0CL80               Alter IDCAMS command
REN      DC       CL08' ALTER  '
RENDSN   DC       CL44' '             Dataset name
FILLER2  DC       CL28' -'
CNTL3    DS       0CL80
RENNEW   DC       CL09' NEWNAME('     New dataset name
RENDSP   DC       CL44' '
FILLER3  DC       CL27')'
CNTL4    DS       0CL80               Del IDCAMS command
DEL      DC       CL08' DELETE '
DELDSN   DC       CL44' '             Dataset name
FILLER4  DC       CL28'  '
HLQMHSR  DC       CL04'MHSR'          HLQ of received files
ALTDD    DS       0F                        Alternate DDNAMES for
         DC       H'48'                     IDCAMS Rename
         DC       XL32'00'
         DC       CL8'SYSIN'
         DC       CL8'AMSPRINT'
*----------------------------------------------------------------------
* DCBs
*----------------------------------------------------------------------
AMSPRINT DCB      DDNAME=AMSPRINT,DSORG=PS,MACRF=(GL),EODAD=FSSYEND
SYSPRINT DCB      DDNAME=SYSPRINT,DSORG=PS,MACRF=(GL),EODAD=FSSYEND
AMSCNTL  DCB      DDNAME=SYSIN,DSORG=PS,MACRF=(PM)
*----------------------------------------------------------------------
* DSECTS
*----------------------------------------------------------------------
         IEFZB4D0
         IEFZB4D2
         ISGRIB
         END
