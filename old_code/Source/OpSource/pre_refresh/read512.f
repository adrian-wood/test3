      SUBROUTINE READ512 (NFT, BUFREC, NFIRST, KODE)
!
!-----------------------------------------------------------------------
!
! SUBROUTINE  : READ512
!
! PURPOSE     : To compress the data in a 28K buffer, removing
!               processed data, shifting along data still in use and
!               reading more records to fill the buffer. The data being
!               read in must be in fixed-length 512-byte records.
!
! USAGE       : CALL READ512 (NFT, BUFREC, NFIRST, KODE)
!
! PARAMETERS  : ('I'=Input, 'I/O'=Input & output)
!
!               NFT     (I)  Unit number of file containing messages.
!               BUFREC (I/O) 28K character buffer to hold messages.
!               NFIRST (I/O) First byte of character string following
!                            the part that has already been processed.
!               KODE   (I/O) Integer return code - coded as follows:
!                               0 - More data still to read
!                               1 - All data now read in
!                               2 - I/O error reading data set
!                            ("READ512" does nothing unless "KODE"
!                             is zero on input.)
!
! CALLED BY   : FINDFTP
!
! CALLS       : None
!
! HISTORY     : "READ512" is a modified version of "READ4K" but reads
!               data sets with record length 512 rather than 4096.
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:56$
! $Source: /home/us0400/mdb/op/lib/source/RCS/read512.F,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:56    Sheila Needham  
! $
! Revision 2.0  2001/06/06 10:20:35  usmdb
! Initial version
!
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
!
      IMPLICIT NONE
!                                                            Parameters
!
      INTEGER   LENGCH, LREC, NRECS ! Lengths & no. of records in buffer
      PARAMETER (LENGCH=28672)      ! Length of buffer =28K (this must
!                                     agree with "LENGCH" in "FINDFTP")
      PARAMETER (LREC=512)          ! For reading 512-byte records
      PARAMETER (NRECS=LENGCH/LREC) ! No. of records in 28K buffer
!
!                                                             Variables
!
      INTEGER   IOFLAG       ! "IOSTAT" variable from read statement
      INTEGER   IRECS        ! No. of records no longer needed
      INTEGER   JREC         ! Record number (loop variable)
      INTEGER   KODE         ! Return code (see above for coding)
      INTEGER   KOUNT        ! Locally used record counter
      INTEGER   NFIRST       ! First byte which must be kept
      INTEGER   NFT          ! Unit number of message file being read
!
      LOGICAL FIRST          ! Flag for first call to routine
!
      CHARACTER HEAD*132             ! Revision information
      CHARACTER*(LREC) BUFREC(NRECS) ! 28K Buffer to hold bulletins
!
!                                                        Saved variable
      SAVE FIRST
!                                                   Data initialisation
      DATA FIRST /.TRUE./
!                                                  Revision information
      IF (FIRST) THEN
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/read512.F,v $
     &   '//'$ $Date: 30/01/2006 20:23:56$ $Revision: 1$'
         FIRST = .FALSE.
      END IF
!                                Check whether more data can be read in
!
      IF (KODE.EQ.0 .AND. NFIRST.GT.512) THEN  ! More data
!
!                                      Discard records no longer needed
         IRECS = (NFIRST-1)/LREC
         KOUNT = 1
         DO JREC=IRECS+1,NRECS
            BUFREC(KOUNT) = BUFREC(JREC)
            KOUNT = KOUNT + 1
         END DO ! JREC
         NFIRST = NFIRST - LREC*IRECS
!                                      Fill rest of array with new data
         DO JREC=KOUNT,NRECS
            READ (NFT, '(A)', IOSTAT=IOFLAG, END=9) BUFREC(JREC)
            IF (IOFLAG.GT.0) THEN  ! I/O error
               KODE = 2  ! (Stops further processing of this data set)
               RETURN
            END IF
         END DO ! JREC
      END IF
!                                   Buffer filled: return with 'kode'=0
      RETURN
!                            End of data: fill unused space with blanks
    9 DO KOUNT=JREC,NRECS
         BUFREC(KOUNT) = ' '
      END DO ! KOUNT
!                                            Set return code and return
      KODE = 1  ! I.e. end of data set found
      RETURN
      END
