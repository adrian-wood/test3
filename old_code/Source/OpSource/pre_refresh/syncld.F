      SUBROUTINE SYNCLD(POINT,REPORT,IG,NGRPS,ICL,CLOUD,VSIF,VERTV)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNCLD
!
! PURPOSE       : EXPANDS CLOUD 8-GROUPS IN SECTION 3 OR 5 OF SYNOP
!
! DESCRIPTION   : SOME BRITISH AUTOMATIC STATIONS REPORT CLOUD IN THE
!                 LOCAL 555 SECTION RATHER THAN THE USUAL 333 SECTION
!                 - SO EXPAND THEM IN A ROUTINE CALLABLE BY BOTH.
!                 LOOP ROUND WHILE THE GROUPS START WITH 8
!
! CALLED BY     : SYNXP3, SYNXP5
!
! PARAMETERS    : (1) POINT  INTEGER START OF FIRST 8-GROUP       (I)
!                         (POINTS TO START OF LAST 8-GROUP ON RETURN)
!                 (2) REPORT CHAR    COMPLETE REPORT TEXT         (I)
!                 (3) IG     INTEGER NUMBER OF FIRST GROUP        (I)
!                 (4) NGRPS  INTEGER NUMBER OF GROUPS IN SECTION  (I)
!                 (5) ICL    INTEGER NUMBER OF CONSEC 8-GROUPS    (O)
!                                   (WITH CLOUD, NOT VERTICAL VIS)
!                 (6) CLOUD  REAL    3*4 ARRAY FOR CLOUD DATA     (O)
!                 (7) VSIF   REAL    SIGNIFICANCE OF CLOUD LAYERS (O)
!                 (8) VERTV  REAL    VERTICAL VISIBILITY          (O)
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 20/04/2010 09:36:14$
! $Source: /home/us0400/mdb/op/lib/storage/RCS/syncld.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         20/04/2010 09:36:14    Sheila Needham
!       Remove initialisation of layer count (ICL) so that 8 groups accumulate
!        from sections 3 and 5.
!  1    Met_DB_Project 1.0         30/01/2006 20:24:45    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:02  usmdb
! Changed preprocessor statement to if (1 ), else... Added
! copyright and modified header - S.Cox
!
! Revision 1.7  98/01/29  11:30:37  11:30:37  usmdb (Generic MetDB account)
! Addition of IBM preprocess directive
!
! Revision 1.6  97/08/28  10:22:27  10:22:27  usjl (Jon Lewthwaite)
! CHANGE CONVERSIONS FOR HEIGHT VALUE 99 TO MINIMUM OF RANGE 2500M.   !B
!
! Revision 1.5  1997/07/31 11:37:27  uspm
! CHANGE CONVERSIONS FOR HEIGHT RANGE 91-98 TO MINIMUM OF RANGE.
! VALUE 91 CONVERTED TO 25 METRES - MIDDLE OF LOWEST RANGE.           !A
!
! Revision 1.4  1997/07/25 13:46:17  uspm
! Latest version from COSMOS - correct decoding of cloud heights
!
! Revision 1.3  1997/07/04 08:04:08  uspm
! Latest version from COSMOS - Y2K check
!
! Revision 1.2  1997/05/15 15:06:40  uspm
! Add revision log and preprocessor statements around INCLUDE statements
!
! DEC 94 - DON'T SET HEIGHT FOR HSHS=89, IT'S TOO BIG TO ENCODE
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER REPORT*500
      INTEGER       I
      INTEGER       ICL
      INTEGER       IG
      INTEGER       ISIG
      INTEGER       IVALUE
      INTEGER       NGRPS
      INTEGER       POINT
      REAL          CLOUD(3,4)
      REAL          HH(90:99)
      REAL          VSIF(4)
      REAL          VERTV


      INCLUDE (SYNCOM)




      DATA (HH(I),I=90,99)
     &                   /25,50,100,200,300,600,1000,1500,2000,2500/ !ab

      CHARACTER*132 CHEAD
      CHEAD='
     &$Source: /home/us0400/mdb/op/lib/storage/RCS/syncld.F,v $
     $'//'$ $Date: 20/04/2010 09:36:14$ $Author: Sheila Needham$ '
     &//'$Revision: 2$'
*
      ISIG=0                 ! COUNT OF SIGNIFICANT LAYERS IN 8-GROUPS
*
*   EACH 8-GROUP CONSISTS OF NS,C,HSHS - AMOUNT, TYPE, HEIGHT.
*   UP TO 4 GROUPS ARE HANDLED, 3 LAYERS AND 1 CONVECTIVE.
*   IF SKY OBSCURED (NS=9) THEN HSHS IS VERTICAL VISIBILITY.
*   CONVERT HSHS SYNOP CODE 1677 TO MAXIMUM OF RANGE IN METRES.
*   (BUT VERTICAL VIS MUST BE SMALL - CAN'T ENCODE MUCH MORE THAN 1KM!)
*   NS SYNOP CODE 2700 = BUFR CODE 020011
*   C  SYNOP CODE 0500 = BUFR CODE 020012 (0-9)
*
   10 I=IVALUE(REPORT(POINT+1:POINT+1))
      IF (I.EQ.9) THEN
        I=IVALUE(REPORT(POINT+3:POINT+4))
        IF (I.NE.MISSIN) THEN
          IF (I.GE.0 .AND. I.LE.50) THEN
            VERTV=I*30.
          ELSE IF (I.GE.90 .AND. I.LE.99) THEN
            VERTV=HH(I)
          ENDIF
        ENDIF
      ELSE IF (I.NE.MISSIN) THEN
        ICL=ICL+1                          ! ONE MORE CLOUD GROUP
        IF (ICL.LE.4) THEN
          CLOUD(1,ICL)=I                   ! CLOUD AMOUNT

          I=IVALUE(REPORT(POINT+2:POINT+2))
          CLOUD(2,ICL)=I                   ! CLOUD TYPE
          IF(I.EQ.9)THEN
            VSIF(ICL)=4.                   ! INDICATE CB
          ELSE
            ISIG=ISIG+1                    ! NEXT SIGNIFICANT LAYER
            VSIF(ICL)=ISIG
          ENDIF

          I=IVALUE(REPORT(POINT+3:POINT+4))
          IF(I.GE.0 .AND. I.LE.50)THEN
            CLOUD(3,ICL)=I*30.             ! HEIGHT OF CLOUD BASE
          ELSE IF(I.GE.56 .AND. I.LE.80)THEN
            CLOUD(3,ICL)=(I-50.)*300.
          ELSE IF(I.GE.81 .AND. I.LE.88)THEN
            CLOUD(3,ICL)=9000.+(I-80.)*1500.
          ELSE IF(I.EQ.89)THEN        ! >21KM: NOT OBVIOUS WHAT TO SET
            CLOUD(3,ICL)=RMISS        ! & ANYWAY TOO BIG TO ENCODE...
          ELSE IF(I.GE.90 .AND. I.LE.99)THEN                       !b
            CLOUD(3,ICL)=HH(I)
          ENDIF
        ENDIF
      ENDIF
*
* IF THERE'S ANOTHER 8-GROUP, LOOP ON  (BUT ONLY KEEP 4 GROUPS' DATA!)
*
      IF (IG.LT.NGRPS) THEN
        IF (REPORT(POINT+5:POINT+6).EQ.' 8') THEN
          POINT=POINT+6
          IG=IG+1
          GO TO 10
        ENDIF
      ENDIF
      RETURN
      END
