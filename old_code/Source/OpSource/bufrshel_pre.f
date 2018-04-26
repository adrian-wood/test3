!-------------------------------------------------------------------
!
! Program: bufrshel_pre.f
!
! Language: Fortran 77
!
! Purpose: BUFRSHEL pre-processing routines.
!          Find lengths of character variables to pass to the
!          C functions.
!          Fortran to C inter-language calls have extra arguments
!          giving the lengths of character strings added to the end of
!          the argument list. Use the Fortran LEN function
!          to pass string lengths explicitly.
!          Routines are:
!            DEBUFR
!            CODE
!            ENBUFR
!            LOCALD
!            ENBUFV4
!
! Arguments: As for BUFR routines
!
! Called By: This code can be compiled on the GPCS into a BUFRSHEL
!            (along with bufshel.c) that is called by user
!            programs that use the BUFR routines.
!            It is also included in the MDBDLL build and so called
!            by the retrieval routines.
!
!
! Calls: C wrapper functions to load and call BUFRDLL entry point
!        routines (bufrshel.c)
!
! $Workfile: bufrshel_pre.f$ $Folder: OpSource$
! $Revision: 1$ $Date: 21/02/2011 11:23:35$
!
! Changes:
! $Log:
!  1    MetDB_Refresh 1.0         21/02/2011 11:23:35    Alison Weir
!       BUFRSHEL pre-processing routines
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
      SUBROUTINE DEBUFR(DESCR,VALUES,NAMES,ND,NOBS,STRING,DSPLAY)
      IMPLICIT NONE

      INTEGER          DESCR(*)
      REAL             VALUES(*)
      CHARACTER        NAMES*(*)
      INTEGER          ND
      INTEGER          NOBS
      CHARACTER        STRING*(*)
      LOGICAL          DSPLAY

      INTEGER          NAMES_L
      INTEGER          STRING_L

      NAMES_L=LEN(NAMES)
      STRING_L=LEN(STRING)

      CALL DEBUFRC(DESCR,VALUES,NAMES,ND,NOBS,STRING,DSPLAY,
     &             NAMES_L, STRING_L)

      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE CODE(DESCR,FIGURE,WORDS)
      IMPLICIT NONE

      INTEGER DESCR
      INTEGER FIGURE
      CHARACTER WORDS*(*)

      INTEGER WORDS_L

      WORDS_L = LEN(WORDS)
      CALL CODEC(DESCR,FIGURE,WORDS,WORDS_L)

      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE ENBUFR(DESCR,VALUES,ND,NELEM,NOBS,NAMES,DATIME,MESAGE,
     &                  CMPRES,L,IVER)
      IMPLICIT NONE

      INTEGER          DESCR(*)
      REAL             VALUES(*)
      INTEGER          ND
      INTEGER          NELEM
      INTEGER          NOBS
      CHARACTER        NAMES*(*)
      INTEGER          DATIME(5)
      CHARACTER        MESAGE*(*)
      LOGICAL          CMPRES
      INTEGER          L
      INTEGER          IVER

      INTEGER          NAMES_L
      INTEGER          MESAGE_L

      NAMES_L=LEN(NAMES)
      MESAGE_L=LEN(MESAGE)

      CALL ENBUFRC(DESCR,VALUES,ND,NELEM,NOBS,NAMES,DATIME,MESAGE,
     &                  CMPRES,L,IVER,NAMES_L,MESAGE_L)

      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE LOCALD(X,Y,SEQ,NSEQ,TEXT,MODE)
      IMPLICIT NONE

      INTEGER X
      INTEGER Y
      INTEGER SEQ(*)
      INTEGER NSEQ
      CHARACTER TEXT*(*)
      CHARACTER MODE*(*)

      INTEGER TEXT_L
      INTEGER MODE_L

      TEXT_L=LEN(TEXT)
      MODE_L=LEN(MODE)

      CALL LOCALDC(X,Y,SEQ,NSEQ,TEXT,MODE,TEXT_L,MODE_L)

      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE ENBUFV4(DESCR,VALUES,ND,NELEM,NOBS,NAMES,DATIME,
     &                   MESAGE,CMPRES,L,EDITION,MASTERTABLE,
     &                   VERMASTAB,
     &                   ORIGCENTRE,ORIGSUBCENTRE,
     &                   DATATYPE,LOCALDATASUBTYPE,INTDATASUBTYPE,
     &                   VERLOCTAB,EXTRASECT1,CHARSECT1,EXTRASECT2,
     &                   CHARSECT2,SECT3TYPE)
      IMPLICIT NONE

      INTEGER          DESCR(*)
      REAL             VALUES(*)
      INTEGER          ND
      INTEGER          NELEM
      INTEGER          NOBS
      CHARACTER        NAMES*(*)
      INTEGER          DATIME(5)
      CHARACTER        MESAGE*(*)
      LOGICAL          CMPRES
      INTEGER          L
      INTEGER          EDITION
      INTEGER          MASTERTABLE
      INTEGER          VERMASTAB
      INTEGER          ORIGCENTRE
      INTEGER          ORIGSUBCENTRE
      INTEGER          DATATYPE
      INTEGER          LOCALDATASUBTYPE
      INTEGER          INTDATASUBTYPE
      INTEGER          VERLOCTAB
      LOGICAL          EXTRASECT1
      CHARACTER        CHARSECT1*(*)
      LOGICAL          EXTRASECT2
      CHARACTER        CHARSECT2*(*)
      INTEGER          SECT3TYPE

      INTEGER          NAMES_L
      INTEGER          MESAGE_L
      INTEGER          CHARSECT1_L
      INTEGER          CHARSECT2_L

      NAMES_L=LEN(NAMES)
      MESAGE_L=LEN(MESAGE)
      CHARSECT1_L=LEN(CHARSECT1)
      CHARSECT2_L=LEN(CHARSECT2)

      CALL ENBUF4C(DESCR,VALUES,ND,NELEM,NOBS,NAMES,DATIME,
     &                   MESAGE,CMPRES,L,EDITION,MASTERTABLE,
     &                   VERMASTAB,
     &                   ORIGCENTRE,ORIGSUBCENTRE,
     &                   DATATYPE,LOCALDATASUBTYPE,INTDATASUBTYPE,
     &                   VERLOCTAB,EXTRASECT1,CHARSECT1,EXTRASECT2,
     &                   CHARSECT2,SECT3TYPE,NAMES_L,MESAGE_L,
     &                   CHARSECT1_L,CHARSECT2_L)

      RETURN
      END

