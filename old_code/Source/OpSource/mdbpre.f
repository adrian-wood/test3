      SUBROUTINE mdb(ctype,request,rvals,
     &                          nobs,nelem,status,cstr,crept)
!-------------------------------------------------------------------
!
! Program: MDB  (shell program)
!
! Purpose: Find lengths of character array variables to pass to the
!          C function.
!          VSFortran to C inter-language calls have extra arguments
!          giving the lengths of character strings added to the end of
!          the argument list. Character scalars are handled correctly
!          but character arrays are not; use the Fortran LEN function
!          to pass cstr and crept lengths explicitly.
! Arguments: As for MDB
!
! Calls: mdbc (C wrapper function to load and call DLL)
!
! $Workfile: mdbpre.f$ $Folder: OpSource$
! $Revision: 1$ $Date: 20/09/2011 08:56:33$
!
! Changes:
! $Log:
!  1    Met_DB_Project 1.0         20/09/2011 08:56:33    Sheila Needham
!       Copied from refresh view
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

! Arguments

      INTEGER nelem             ! Number of elements  (I)
      INTEGER nobs              ! Number of observations (/O)
      CHARACTER*(*) ctype       ! Data subtype   (I)
      CHARACTER*(*) request     ! Request string (I)
      REAL rvals(nobs,nelem)    ! Data array     (O)
      INTEGER status            ! Return Code    (I/O)
      CHARACTER*(*) cstr(nobs)  ! Character elements (O)
      CHARACTER*(*) crept(nobs) ! Report text       (O)

! Local variables

      INTEGER  cstr_L
      INTEGER  crept_L

      cstr_L = LEN(cstr(1))
      crept_L = LEN(crept(1))

      call mdbc(ctype,request,rvals,nobs,nelem,status,cstr,crept,
     &                  cstr_L,crept_L)

      RETURN
      END
