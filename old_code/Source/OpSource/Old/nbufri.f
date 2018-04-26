      SUBROUTINE NBUFRI(DESCR,VALUES,ND,NELEM,NOBS,NAMES,DATIME,MESAGE,
     &                  CMPRES,L)

!-----------------------------------------------------------------------
!
! ROUTINE       : NBUFRI
!
! PURPOSE       : to make a complete bufr message, given a descriptor
!               : sequence (which will be expanded if necessary) and
!               : an array of values corresponding to the expanded
!               : sequence (i.e. the user must know the expansion
!               : beforehand!)
!
! CALLED BY     : user...
!
! CALLS         : DESFXY, ENCODI
!
! ARGUMENTS     : (1) descriptor sequence (may need expanding)
!                 (2) nobs*nelem array of values
!                 (3) number of descriptors (input, then output;
!                      returned as zero if there's an error.)
!                 (4) number of elements in array (nelem)
!                 (5) number of observations in array (nobs)
!                 (6) any character values (with pointers in array)
!                 (7) five integers: year, month, day, hour, minute
!                 (8) character string for bufr message
!                 (9) flag set if compression required
!                 (10) total length of bufr message
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 26/11/2007 11:12:15$
! $Source: /data/us0400/mdb/op/lib/source/RCS/nbufri.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         26/11/2007 11:12:15    Brian Barwell
!       Obsolete routine used for SAT120 storage (removed from BUFR load
!       module on 19 November 2007).
!  1    Met_DB_Project 1.0         30/01/2006 20:23:40    Sheila Needham  
! $
! Revision 2.0  2002/04/09  11:47:02  11:47:02  usmdb (Generic MetDB account)
! Initial version
! 
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      INTEGER DESCR(*),DATIME(5), F,X,Y                             !2.1
      REAL VALUES(NOBS,NELEM)                                       !2.1
      CHARACTER MESAGE*(*), NAMES*(*), BUFR*4, SEVENS*4, CCCCNO*2
      LOGICAL CMPRES
      CHARACTER HEAD*132

      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/nbufri.F,v $
     &'//'$ $Date: 26/11/2007 11:12:15$ $Revision: 2$'

      BUFR='BUFR'
      SEVENS='7777'
      CALL EB2ASC(4,BUFR)
      CALL EB2ASC(4,SEVENS)

! ----------------------------------------------------------------------
!  if number of descriptors passed as zero, use the descriptors in the
!  message passed: look for 'bufr', then use the section lengths (skip-
!  ping any section 2) to reach the descriptors in section 3.
! ----------------------------------------------------------------------

      IF (ND.EQ.0) THEN
        N=1
   10   IF (MESAGE(N:N+3).NE.BUFR) THEN
          IF (N.LT.100) THEN
            N=N+1
            GO TO 10
          ELSE
            PRINT *,' NO DESCRIPTORS PASSED, BUT NO BUFR MESSAGE EITHER'
            RETURN
          ENDIF
        ENDIF
        N=N+4

! ----------------------------------------------------------------------
! skip section 1.  the length after 'bufr' is (from edition 2 on) the
! length of the whole message, so skip it first if it points to '7777'.
! the minimum total length is (section by section) 8+18+0+10+4+4=44.
! ----------------------------------------------------------------------

        L=ICHAR(MESAGE(N+1:N+1))*256+ICHAR(MESAGE(N+2:N+2))
        IF (L.GE.44 .AND. MESAGE(N+L-8:N+L-5).EQ.SEVENS) THEN
           N=N+4
           L1=ICHAR(MESAGE(N+1:N+1))*256+ICHAR(MESAGE(N+2:N+2))
        ELSE
           L1=L
        ENDIF
        CCCCNO=MESAGE(N+4:N+5)
        N=N+L1

        IF (ICHAR(MESAGE(N+7:N+7)).GE.128) THEN
          L2=ICHAR(MESAGE(N+1:N+1))*256+ICHAR(MESAGE(N+2:N+2))
          N=N+L2
        ENDIF
      ENDIF

! ----------------------------------------------------------------------
!  set up section 1 of message (section 2 missing) with 'BUFR' at start
! ----------------------------------------------------------------------

      MESAGE(1:4)=BUFR

! length

      MESAGE(5:7)=CHAR(0)//CHAR(0)//CHAR(18)

! bufr revision number

      MESAGE(8:8)=CHAR(0)

! originating centre (country/centre)
! if nd=0, cccc as in input message.
! if nd>0, set met office cccc

      IF (ND.EQ.0) MESAGE(9:10)=CCCCNO
      IF (ND.GT.0) MESAGE(9:10)=CHAR(0)//CHAR(74)

! cor sequence number

      MESAGE(11:11)=CHAR(0)

! optional section flag in first bit

      MESAGE(12:12)=CHAR(0)

! bufr message type (from table a)

      MESAGE(13:13)=CHAR(255)

! message subtype (local number)

      MESAGE(14:14)=CHAR(0)

! number of non-standard table set

      MESAGE(15:16)=CHAR(0)//CHAR(1)

! year, month, day, hour, minute
! (century-year is 100 (not 0!) when year is a multiple of 100)    !1.3

      NYEAR=MOD(DATIME(1),100)                                     !1.3
      IF (NYEAR.EQ.0) NYEAR=100                                    !1.3
      MESAGE(17:17)=CHAR(NYEAR)                                    !1.3

      DO 20 I=2,5                                                  !1.3
        MESAGE(16+I:16+I)=CHAR(DATIME(I))
   20 CONTINUE

! finally one byte of padding

      MESAGE(22:22)=CHAR(0)

! ---------------------------------------------section 3----------------
! if number of descriptors was zero, copy descriptors from message
! ----------------------------------------------------------------------

      IF (ND.EQ.0) THEN
        L3=ICHAR(MESAGE(N+1:N+1))*256+ICHAR(MESAGE(N+2:N+2))
        ND=(L3-7)/2
        DO 30 I=1,ND
          ID=N+7+(I-1)*2
          DESCR(I)=ICHAR(MESAGE(ID:ID))*256+ICHAR(MESAGE(ID+1:ID+1))
   30   CONTINUE
      ENDIF

! ---------------------------------------------section 3----------------
! put descriptors (plus number of obs & compression flag) in section 3
!
! L is length of section 3 (7 bytes before descriptors, padding at end)
! ----------------------------------------------------------------------

      L=7+ND*2+1
      MESAGE(23:25)=CHAR(0)//CHAR(L/256)//CHAR(MOD(L,256))
      MESAGE(26:26)=CHAR(0)

! number of observations

      MESAGE(27:28)=CHAR(NOBS/256)//CHAR(MOD(NOBS,256))

! observed data, may be compressed

      IF (CMPRES) THEN
        MESAGE(29:29)=CHAR(128+64)
      ELSE
        MESAGE(29:29)=CHAR(128)
      ENDIF

! split descriptor into fxx & yyy

      DO 40 I=0,ND-1
        CALL DESFXY(DESCR(I+1),F,X,Y)

! ----------------------------------------------------------------------
! the category of a class d descriptor for a whole observation corres-
! ponds to the kind of data in class a, so set that byte in section 1.
! ----------------------------------------------------------------------

        IF (I.EQ.0 .AND. F.EQ.3 .AND. X.GE.7 .AND. X.LE.12) THEN
          MESAGE(13:13)=CHAR(X-7)
        ENDIF

! put fxx in one byte & yyy in other

        MESAGE(30+I*2:30+I*2)=CHAR(F*64+X)
        MESAGE(31+I*2:31+I*2)=CHAR(Y)
   40 CONTINUE

! ----------------------------------------------------------------------
! now encode the bit string in section 4 and put 7777 at the end.
! (arguments for bufr4 as for this program, but l4 & no datime)
!
! L=length of sections 0,1 & 3  (no section 2)
! ----------------------------------------------------------------------

      L=4+18+L
      CALL ENCODI(DESCR,VALUES,ND,NELEM,NOBS,NAMES,MESAGE(L+1:),
     &            CMPRES,L4)

! ----------------------------------------------------------------------
! l4 is returned as zero if there's an error in bufr4.  if so, return
! nd=0 to warn the user to stop; if not, put 7777 at the end.
! ----------------------------------------------------------------------

      IF (L4.EQ.0) THEN
        ND=0
      ELSE
        L=L+L4
        MESAGE(L+1:L+4)=SEVENS
        L=L+4
      ENDIF

      RETURN
      END
