//M12CLABL JOB (M12,DB,WDS0BF),SDBTEAM.6953,PRTY=8,MSGCLASS=Q           00010007
// EXEC FORT2C,FPARMS='MAP,CHARLEN(28000),NOFIPS,DC(*)'                 00010209
      INTEGER FUNCTION IVALUE(STRING)                               !1.3

!-----------------------------------------------------------------------
!
! PROGRAM       : IVALUE (FUNCTION)
!
! PURPOSE       : TO CHECK STRING FOR FIGURES AND CONVERT TO INTEGER
!
! CALLED BY     : MDB U/A EXPANSION
!
! PARAMETERS    : (1) CHARACTER STRING TO BE CONVERTED
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 28/02/2006 12:18:46$
! $Source: /home/us0400/mdb/op/lib/other/MCC3.DBJCLLIB.CNTL/RCS/newtablb.jcl,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         28/02/2006 12:18:46    Sheila Needham  
! $
! Revision 1.1  2003/08/05 10:47:13  usmdb
! Initial revision
!
! Revision 2.0  2001/07/03 10:43:35  usmdb
! Removed byte counts from INTEGER declarations. Separated
! variable declaration and initialisation. Added copyright,
! modified header and comments - S.Cox
!
! Revision 1.3  2000/06/08  15:25:57
! 19/06/2000 Allow to cope with string lengths >6 and of 0. Stan K.
!
! Revision 1.2  97/07/31  09:28:23  09:28:23  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.1  1997/07/04 12:45:30  uspm
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE                                                 !1.3

      CHARACTER     STRING*(*)
      CHARACTER*5   FORM(10)                                        !2.0
      CHARACTER*132 HEAD
      INTEGER       MISSING          ! Missing indicator            !2.0
      INTEGER       L                ! used to hold LEN(STRING)     !2.0
      INTEGER       I                ! Loop control                 !2.0

      DATA FORM/'(I1)','(I2)','(I3)','(I4)','(I5)','(I6)',          !2.0
     &          '(I7)','(I8)','(I9)','(I10)'/
      DATA MISSING/-9999999/                                        !2.0

      L=LEN(STRING)
      IVALUE = MISSING                             ! initialise     !1.3

      IF (L.LE.10) THEN
        DO I=1,L                                                    !1.3
          IF (STRING(I:I).LT.'0' .OR. STRING(I:I).GT.'9') RETURN    !1.3
        ENDDO                                                       !1.3
        READ (STRING,FORM(L)) IVALUE                                !1.3
      ENDIF
      RETURN
      END
// EXEC FORT2CLG,FPARMS='MAP,CHARLEN(28000),NOFIPS,DC(*)'               00010209
!-----------------------------------------------------------------------
!
! ROUTINE       : NEWTABLB
!
! PURPOSE       : to merge new WMO Table B entries into our Table B,
!                 keeping any local entries.
!
! CALLS         : IVALUE
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 28/02/2006 12:18:46$
! $Source: /home/us0400/mdb/op/lib/other/MCC3.DBJCLLIB.CNTL/RCS/newtablb.jcl,v $
!
! CHANGE RECORD :
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

      CHARACTER LINE*180
      CHARACTER NAME*64,NU*24,CREXUNIT*24
      CHARACTER MNAME*64,MU*24
      CHARACTER VERSION*5/'!m.nn'/
      INTEGER CREXCALE,CREXWID

! Read the first line of our table to get the current revision number

      READ (1,'(A80)') LINE(1:80)
      WRITE (3,'(A80)') LINE(1:80)

      N=IVALUE(LINE(17:18))+1
      VERSION(2:2)=LINE(15:15)
      VERSION(4:4)=CHAR(ICHAR('0')+N/10)
      VERSION(5:5)=CHAR(ICHAR('0')+MOD(N,10))

! Read a line from our Table B

      READ (1,'(I1,I2,I3,1X,A64)',IOSTAT=MRC) MF,MX,MY,MNAME
      READ (1,'(1X,A24,I4,I11,I3)',IOSTAT=MRC) MU,MSCALE,MREF,MWIDTH

! Read a WMO entry.  Fields are separated by commas (no spaces either
! side), any name which includes a comma being enclosed in brackets.

      NRC = 0
      DO WHILE (NRC.EQ.0)
   10   READ (2,IOSTAT=NRC) LINE

! Ignore any line that doesn't start with zero & comma (F=0)
! & any whose name is "Reserved".

        IF (LINE(1:2).EQ.'0,') THEN
          IX=3
! X
          IY=INDEX(LINE(IX:),',')
          NX=IVALUE(LINE(IX:IX+IY-2))
          IX=IX+IY
! Y
          IY=INDEX(LINE(IX:),',')
          NY=IVALUE(LINE(IX:IX+IY-2))
          IX=IX+IY

! Name (may be enclosed in brackets - and may be too long!)

          IF (LINE(IX:IX).EQ.'"') THEN
            IY=INDEX(LINE(IX+1:),'",')
            NAME=LINE(IX+1:IX+1+IY-2)
            IX=IX+1+IY+1
          ELSE
            IY=INDEX(LINE(IX:),',')
            NAME=LINE(IX:IX+IY-2)
            IF (NAME.EQ.'Reserved') GO TO 10
            IX=IX+IY
          ENDIF
          if (IY-1.GT.64) print *,'name too long: ',NAME
! Units
          IY=INDEX(LINE(IX:),',')
          NU=LINE(IX:IX+IY-2)
          IX=IX+IY

! Scale (may be negative)

          IY=INDEX(LINE(IX:),',')
          IF (LINE(IX:IX).EQ.'-') THEN
            NSCALE=-IVALUE(LINE(IX+1:IX+IY-2))
          ELSE
            NSCALE=IVALUE(LINE(IX:IX+IY-2))
          ENDIF
          IX=IX+IY

! Reference value (may be negative)

          IY=INDEX(LINE(IX:),',')
          IF (LINE(IX:IX).EQ.'-') THEN
            NREF=-IVALUE(LINE(IX+1:IX+IY-2))
          ELSE
            NREF=IVALUE(LINE(IX:IX+IY-2))
          ENDIF
          IX=IX+IY
! Width
          IY=INDEX(LINE(IX:),',')
          NWIDTH=IVALUE(LINE(IX:IX+IY-2))
          IX=IX+IY

! CREX units

          IY=INDEX(LINE(IX:),',')
          CREXUNIT=LINE(IX:IX+IY-2)
          IX=IX+IY

! CREX scale (may be negative)

          IY=INDEX(LINE(IX:),',')
          IF (LINE(IX:IX).EQ.'-') THEN
            CREXCALE=-IVALUE(LINE(IX+1:IX+IY-2))
          ELSE
            CREXCALE=IVALUE(LINE(IX:IX+IY-2))
          ENDIF
          IX=IX+IY

! CREX width (last, so delimited by blank, not comma)

          IY=INDEX(LINE(IX:),' ')
          CREXWID=IVALUE(LINE(IX:IX+IY-2))
          IX=IX+IY

! If not F=0, read the next line

        ELSE IF (NRC.EQ.0) THEN
          GO TO 10
        ENDIF

! Compare the latest entries from the two inputs (assumed to be in order
! If a WMO entry is not in our table, output it with a version number.

   20   IF (NX*1000+NY .LT. MX*1000+MY) THEN
          WRITE (3,'(I1,I2.2,I3.3,1X,A64,4X,A5)') NF,NX,NY,NAME,VERSION
          WRITE (3,'(1X,A24,I4,I11,I3, 1X,A24,I4,I3)')
     &          NU,NSCALE,NREF,NWIDTH, CREXUNIT,CREXCALE,CREXWID
          IF (NRC.EQ.0) GO TO 10

! If the next entry (in order) is from our table, output it if it's
! local, otherwise don't write it out, assuming we're out of date.

        ELSE IF (NX*1000+NY .GT. MX*1000+MY) THEN
          IF (MY.GE.192) THEN
            WRITE (3,'(I1,I2.2,I3.3,1X,A64)') MF,MX,MY,MNAME
            WRITE (3,'(1X,A24,I4,I11,I3)') MU,MSCALE,MREF,MWIDTH
          ELSE
            print *,'entry deleted (no longer in WMO table):'
            WRITE (*,'(I1,I2.2,I3.3,1X,A64)') MF,MX,MY,MNAME
          ENDIF

! Read another line from our Table B

          READ (1,'(I1,I2,I3,1X,A64)',IOSTAT=MRC) MF,MX,MY,MNAME
          READ (1,'(1X,A24,I4,I11,I3)',IOSTAT=MRC) MU,MSCALE,MREF,MWIDTH
          GO TO 20

! If the next entry is in both inputs, print a message if the details
! are inconsistent, writing out the WMO version.

        ELSE
          IF (MSCALE.NE.NSCALE .OR. MREF.NE.NREF .OR.
     &        MWIDTH.NE.NWIDTH) THEN
            print *,'inconsistent details for'
            WRITE (*,'(I1,I2.2,I3.3,1X,A64)') MF,MX,MY,MNAME
          ENDIF

! Read another line from our Table B

          READ (1,'(I1,I2,I3,1X,A64)',IOSTAT=MRC) MF,MX,MY,MNAME
          READ (1,'(1X,A24,I4,I11,I3)',IOSTAT=MRC) MU,MSCALE,MREF,MWIDTH

! Write out WMO entry

          WRITE (3,'(I1,I2.2,I3.3,1X,A64)') NF,NX,NY,NAME
          WRITE (3,'(1X,A24,I4,I11,I3, 1X,A24,I4,I3)')
     &          NU,NSCALE,NREF,NWIDTH, CREXUNIT,CREXCALE,CREXWID
          IF (NRC.EQ.0) GO TO 10
        ENDIF
      ENDDO

! Assume our table continues beyond the end of the WMO table (local
! classes), so copy remaining entries.

      DO WHILE (MRC.EQ.0)
        READ (1,'(I1,I2,I3,1X,A64)',IOSTAT=MRC) MF,MX,MY,MNAME
        IF (MRC.EQ.0) THEN
          READ (1,'(1X,A24,I4,I11,I3)',IOSTAT=MRC) MU,MSCALE,MREF,MWIDTH
          WRITE (3,'(I1,I2.2,I3.3,1X,A64)') MF,MX,MY,MNAME
          WRITE (3,'(1X,A24,I4,I11,I3)') MU,MSCALE,MREF,MWIDTH
        ENDIF
      ENDDO
      STOP
      END
//GO.FT01F001 DD DSN=SDB.BUFR.TABLEB,DISP=SHR,LABEL=(,,,IN)
//FT02F001 DD DSN=MCC3.SKBUFR.TABLEB,DISP=SHR,LABEL=(,,,IN)
//FT03F001 DD DSN=MCC3.CLBB,DISP=SHR
//* 03F001 DD SYSOUT=Q
