PROGRAM ncdfrtvl
!-----------------------------------------------------------------------
! PROGRAM       : NCDFRTVL
!
! PURPOSE       : To retrieve data from the MetDB using the atomic access
!                 API and print out some data values.
!
! DESCRIPTION
! -----------
! This program extracts data matching the request string and prints one or
! more values from all objects it finds.  The number of observations and 
! array elements printed can be controlled with namelist input.
!
!
!  The NAMELIST is called 'INPUT' and contains the following variables:
!     SUBTYPE  NetCDF/HDF5(/XML) subtype
!     REQUEST  Complete MDB request string which can run over more than
!              one line (see examples below)
!     MAXOBS   Maximum number of observations to print (default up to 10000) 
!     MAXPRINT Maximum number of data values to print out 
!                      (default up to 10000)
!
! Examples of &INPUT
! 1) An hours worth of LIDAR data from Exeter, printing all values
!    of all observations.  The request is continued on a second line
!    with the leading spaces left in (it doesn't matter here).
!  &INPUT SUBTYPE='LIDAR',
!         REQUEST='START TIME TODAY/0000Z END TIME TODAY/0059Z
!                  PLATFORM exeter' /
!
! 2) The latest SUBPHOTO observations. All obs and values.
!  &INPUT SUBTYPE='SUNPHOTO',REQUEST='LATEST' /
!
! 3) All OCEANRE observations for a day using a test location but
!    not printing any of the data. Note that the DDICT text must not
!    include spaces (i.e. cannot split over lines)
!  &INPUT SUBTYPE='OCEANRE',REQUEST='START TIME 20161201/0000Z
! END TIME 20161201/2359Z 
! DDICT "/var/moods/tests/DF_TESTS/OCEANRE/retrieval_table" '
! MAXPRINT=0 /
!         
! Note that only one request is allowed per run (unlike mdbrtvl).
!
! REVISIONS:
! MB-653:  Feb 2017 Initial version.                          SN
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2017 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
IMPLICIT NONE

! Kind types for atomic access retrieval 
INTEGER,PARAMETER :: real64 = selected_real_kind(15,307)
integer,PARAMETER :: real32 = selected_real_kind(6,37)
integer,PARAMETER :: int64 = selected_int_kind(15)
integer,PARAMETER :: int32 = selected_int_kind(9)
integer,PARAMETER :: int16 = selected_int_kind(4)
INTEGER,PARAMETER :: int8 = selected_int_kind(2)

! Namelist variables
CHARACTER (LEN=8)   :: subtype
CHARACTER (LEN=500) :: request
INTEGER             :: maxobs=-1
INTEGER             :: maxprint=-1

! Atommic access variables
INTEGER :: istat            ! subroutine return code
INTEGER :: handle           ! request handle
INTEGER :: nobsv            ! no. of obs retrieved
INTEGER :: nfiles           ! no. of files accessed
INTEGER :: nobjects         ! no. of objects per observation
INTEGER :: nmatches         ! no. of matches to object name
INTEGER :: obsv_id          ! observation ID
INTEGER :: obj_id           ! object ID
INTEGER :: dims(3)          ! object dimensions
INTEGER :: dimids(3)        ! dimension IDs
INTEGER :: bytes            ! no. of bytes
INTEGER :: type             ! data type (0=char,1=int,2=real)
INTEGER :: rank             ! rank (0=scalar)
INTEGER(int64),ALLOCATABLE  :: i64data(:,:,:)
INTEGER(int32),ALLOCATABLE  :: i32data(:,:,:)
INTEGER(int16),ALLOCATABLE  :: i16data(:,:,:)
INTEGER(int8),ALLOCATABLE   :: i8data(:,:,:)
REAL(real64),ALLOCATABLE    :: r64data(:,:,:)
REAL(real32),ALLOCATABLE    :: r32data(:,:,:)
CHARACTER(300),ALLOCATABLE :: cdata(:,:,:)
CHARACTER(200) :: dimnames(3)  ! dimension names
CHARACTER(200) :: units(3)     ! dimension units
CHARACTER(300) :: name         ! object name

! Other local variables
INTEGER :: k3x           ! dimension loops
INTEGER :: k2x 
INTEGER :: k1x 
INTEGER :: range

INTEGER :: i,j,k1,k2,k3  ! loop counters

CHARACTER(300) :: element 
CHARACTER(50)  :: IFMT1='(10I12)'
CHARACTER(50)  :: IFMT2='(T41,10I12))'
CHARACTER(50)  :: RFMT1='(10E12.4)'
CHARACTER(50)  :: RFMT2='(T41,10E12.4))'
CHARACTER(50)  :: CFMT1='(10A)'
CHARACTER(50)  :: CFMT2='(T41,1x,10A))'
CHARACTER(70)  :: FMT1
CHARACTER(30)  :: STR1

NAMELIST /INPUT/subtype,request,maxobs,maxprint
!---------------------------------------------------------
! Initialise request string   
!---------------------------------------------------------

maxobs=10000
maxprint=10000

OPEN(40,FILE='INPUT')
READ(40,NML=INPUT)
CLOSE(40)

OPEN(6,FILE='OUTPUT')
WRITE(6,*)'REQUEST for:',trim(SUBTYPE)
WRITE(6,*)TRIM(REQUEST)

CALL OPEN_METDB_REQUEST(subtype,request,handle,nobsv,nfiles,istat)

IF (istat /= 0) CALL shutdown('open_metdb_request',istat)

WRITE(6,'(A,I4)')'No. of observations =',nobsv
WRITE(6,'(A,I4)')'No. of files        =',nfiles

IF (nobsv == 0) CALL shutdown('No data ',nobsv)

! Set array print sizes 
k1x=maxprint
k2x=maxprint
k3x=maxprint
!---------------------------------------------------------
! Loop over observations  
!---------------------------------------------------------
obs: &
DO obsv_id=1,MIN(maxobs,nobsv)
  WRITE(6,'(A,i3.3)')'OBSERVATION NO. ',obsv_id

  CALL get_obsv_info(handle, obsv_id, nobjects, istat)
  IF ( istat /=0 ) CALL shutdown('get_obsv_info', istat)
  WRITE(6,'(I6,A)') nobjects,' Objects found' 
! Loop over data elements 
elem: &
  DO i=1,nobjects

! get the name of this object
    CALL get_object_info(handle, obsv_id, i, name, type, bytes, rank, istat)
    IF ( istat /=0 ) CALL shutdown('get_object_info', istat)
     
! now search for it again to check select by name is working correctly
    element=TRIM(name)
    CALL get_matches_to_name(handle, obsv_id, TRIM(element), nmatches, istat)
    IF (nmatches == 0) THEN
      PRINT*,'Element not found:',TRIM(element)
      CYCLE elem
    ELSE IF (nmatches > 1) THEN
      PRINT*,'Too many matches on this name:',TRIM(element)
      CYCLE elem
    END IF

    name=''
    CALL get_id_from_match(handle, obsv_id, 1, name, obj_id, istat)
    IF (istat /= 0 ) THEN
      PRINT*,'Error getting ID from match:',TRIM(element)
      CYCLE elem
    END IF

! Get the object metadata again, this time by the ID the match found (obj_id)  
    CALL get_object_info(handle, obsv_id, obj_id, name, type, bytes, rank, istat)
    IF (istat /= 0) THEN
      PRINT*,'Error from get_object_info - ',istat
      CYCLE elem
    END IF

! Get object dimensions for allocating arrays later - rank 0 is scalar so defaults to 1 
    dims(1:3)=1
    IF (rank > 0) THEN
      CALL get_object_dims(handle, obsv_id, obj_id, rank, dims(1:rank), istat)
    
      IF (istat /= 0 )THEN
        PRINT*,'Error from get_object_dims', istat
        CYCLE elem
      END IF
	  
      units=''
      dimnames=''
      CALL get_object_dimids(handle, obsv_id, obj_id, rank, dimids, dimnames, units, istat)
      FMT1='(/("dimensions of following variable(",i2,")",a,"(",a,")=",i6/))'
      IF (maxprint > 0) write(6,FMT=FMT1,ADVANCE='NO')&
                       (j,TRIM(dimnames(j)),TRIM(units(j)),dims(j),j=1,rank)
    END IF
	
    IF(maxprint > 0) WRITE(6,'(a40)',ADVANCE='NO')TRIM(name)//': '

! Character data
    IF (type == 0 )THEN
      allocate(cdata(dims(1),dims(2),dims(3)))
      cdata=''
      CALL retrieve_data(handle, obsv_id, obj_id, cdata, istat)
      IF (istat /=0 ) THEN
        PRINT*,'Error from retrieve_data: cdata',istat
      ELSE
        DO k3=1,MIN(dims(3),k3x)
          DO k2=1,MIN(dims(2),k2x)
            IF (rank > 0 )THEN
              CALL arr_str(rank, MIN(dims(1),k1x), k2, k3, str1)
              WRITE(6,'(T41,a/)')TRIM(str1)
            END IF
            range=MIN(dims(1),k1x)
            IF (range == 1 ) THEN
              WRITE(6,CFMT1)(adjustl(trim(cdata(k1,k2,k3))),k1=1,range)
            ELSE
              WRITE(6,CFMT2)(adjustl(trim(cdata(k1,k2,k3))),k1=1,range)
            END IF
          END DO
        END DO
      END IF
      deallocate(cdata)
    ELSE IF ( type == 1 )THEN
	
! Integer data
      IF (bytes == 1 ) THEN
        allocate(i8data(dims(1),dims(2),dims(3)))
        CALL retrieve_data(handle, obsv_id, obj_id, i8data, istat)
        IF (istat /=0 ) THEN
          PRINT*,'Error from retrieve_data: i8data',istat
        ELSE
          DO k3=1,MIN(dims(3),k3x)
            DO k2=1,MIN(dims(2),k2x)
              IF (rank > 0) THEN
                CALL arr_str(rank, MIN(dims(1),k1x), k2, k3, str1)
                WRITE(6,'(T41,a/)')TRIM(str1)
              END IF
              range=MIN(dims(1),k1x)
              IF (range == 1 ) THEN
                WRITE(6,IFmt1)(i8data(k1,k2,k3),k1=1,range)
              ELSE                      
                WRITE(6,IFmt2)(i8data(k1,k2,k3),k1=1,range)
              END IF
            END DO
          END DO
        END IF
        DEALLOCATE(i8data)
      ELSE IF ( bytes == 2 ) THEN
        allocate(i16data(dims(1),dims(2),dims(3)))
        CALL retrieve_data(handle, obsv_id, obj_id, i16data, istat)
        IF (istat /=0 ) THEN
          PRINT*,'Error from retrieve_data: i16data',istat
        ELSE
          DO k3=1,MIN(dims(3),k3x)
            DO k2=1,MIN(dims(2),k2x)
              IF (rank > 0) THEN
                CALL arr_str(rank, MIN(dims(1),k1x), k2, k3, str1)
                WRITE(6,'(T41,a/)')TRIM(str1)
              END IF
              range=MIN(dims(1),k1x)
              IF (range == 1 ) THEN
                WRITE(6,IFmt1)(i16data(k1,k2,k3),k1=1,range)
              ELSE                       
                WRITE(6,IFmt2)(i16data(k1,k2,k3),k1=1,range)
              END IF
            END DO
          END DO
        END IF
        DEALLOCATE(i16data)
      ELSE IF (bytes == 4) THEN
        allocate(i32data(dims(1),dims(2),dims(3)))
        CALL retrieve_data(handle,obsv_id,obj_id,i32data,istat)
        IF (istat /=0 ) THEN
          PRINT*,'Error from retrieve_data: i32data',istat
        ELSE
          DO k3=1,MIN(dims(3),k3x)
            DO k2=1,MIN(dims(2),k2x)
              IF (rank > 0) THEN
                CALL arr_str(rank, MIN(dims(1),k1x), k2, k3, str1)
                WRITE(6,'(T41,a/)')TRIM(str1)
              END IF
              range=MIN(dims(1),k1x)
              IF (range == 1 ) THEN
                WRITE(6,IFmt1)(i32data(k1,k2,k3),k1=1,range)
              ELSE                        
                WRITE(6,IFmt2)(i32data(k1,k2,k3),k1=1,range)
              END IF
            END DO
          END DO
        END IF
        DEALLOCATE(i32data)
      ELSE IF (bytes == 8) THEN
       allocate(i64data(dims(1),dims(2),dims(3)))
        CALL retrieve_data(handle,obsv_id,obj_id,i64data,istat)
        IF (istat /=0 ) THEN
          PRINT*,'Error from retrieve_data: i64data',istat
        ELSE
          DO k3=1,MIN(dims(3),k3x)
            DO k2=1,MIN(dims(2),k2x)
              IF (rank > 0) THEN
                CALL arr_str(rank, MIN(dims(1),k1x), k2, k3, STR1)
                WRITE(6,'(T41,a/)')TRIM(STR1)
              END IF
              range=MIN(dims(1),k1x)
              IF (range == 1 ) THEN
                WRITE(6,IFmt1)(i64data(k1,k2,k3),k1=1,range)
              ELSE
                WRITE(6,IFmt2)(i64data(k1,k2,k3),k1=1,range)
              END IF
            END DO
          END DO
        END IF
        DEALLOCATE(i64data)
      ELSE
        PRINT*,' Invalid byte count:',bytes
        CYCLE elem
      END IF
    ELSE IF ( type == 2) THEN
! Real data
      IF (bytes == 4) THEN
        allocate(r32data(dims(1),dims(2),dims(3)))
        CALL retrieve_data(handle,obsv_id,obj_id,r32data,istat)
        IF (istat /=0 ) THEN
          PRINT*,'Error from retrieve_data: r32data',istat
        ELSE
          DO k3=1,MIN(dims(3),k3x)
            DO k2=1,MIN(dims(2),k2x)
              IF (rank > 0) THEN
                CALL arr_str(rank, MIN(dims(1),k1x), k2, k3, str1)
                WRITE(6,'(T41,a/)')TRIM(str1)
              END IF
              range=MIN(dims(1),k1x)
              IF (range == 1 ) THEN
                WRITE(6,rfmt1)(r32data(k1,k2,k3),k1=1,range)
              ELSE                    
                WRITE(6,rfmt2)(r32data(k1,k2,k3),k1=1,range)
              END IF
            END DO
          END DO
        END IF
        DEALLOCATE(r32data)
      ELSE IF (bytes == 8) THEN
       allocate(r64data(dims(1),dims(2),dims(3)))
        CALL retrieve_data(handle,obsv_id,obj_id,r64data,istat)
        IF (istat /=0 ) THEN
          PRINT*,'Error from retrieve_data: r64data',istat
        ELSE
          DO k3=1,MIN(dims(3),k3x)
            DO k2=1,MIN(dims(2),k2x)
              IF (rank > 0) THEN
                CALL arr_str(rank, MIN(dims(1),k1x), k2, k3, str1)
                WRITE(6,'(T41,a/)')TRIM(str1)
              END IF
              range=MIN(dims(1),k1x)
              IF (range == 1 ) THEN
                WRITE(6,rfmt1)(r64data(k1,k2,k3),k1=1,range)
              ELSE                   
                WRITE(6,rfmt2)(r64data(k1,k2,k3),k1=1,range)
              END IF
            END DO
          END DO
        END IF
        DEALLOCATE(r64data)
      ELSE
        PRINT*,' Invalid byte count:',bytes
        CYCLE elem
      END IF
    ELSE
      PRINT*,'Invalid type:',type
      CYCLE elem
    END IF
  END DO ELEM
END DO OBS
WRITE(6,*)'Request complete.'
CALL CLOSE_METDB_REQUEST(0, istat)
CLOSE(6)
STOP
CONTAINS
  SUBROUTINE shutdown(func,istat)
! Print error code and close cleanly
  INTEGER :: istat
  CHARACTER(*) :: func
  WRITE(6,*)'Error from ',TRIM(func),':',ISTAT
  CALL close_metdb_request(0,istat)
  STOP 
  END SUBROUTINE shutdown
 
  SUBROUTINE arr_str(rank,k1,k2,k3,str)
! Creates a string containing the array segment defined by rank,
! k1, k2 & k3. e.g. (1-10,2,3)
  INTEGER,INTENT(IN)       :: rank
  INTEGER,INTENT(IN)       :: k1,k2,k3
  CHARACTER(*),INTENT(OUT) :: str
  str=''
  IF (rank == 1) THEN
    WRITE(str,'(a,I4.4,a)')'(1-',k1,')'
  ELSE IF (rank == 2) THEN
    WRITE(str,'(a,I4.4,a,I4.4,a)')'(1-',k1,',',k2,')'
  ELSE IF (rank == 3) THEN
    WRITE(str,'(a,I4.4,a,I4.4,a,I4.4,a)')'(1-',k1,',',k2,',',k3,')'
  END IF
  END SUBROUTINE arr_str
END PROGRAM ncdfrtvl
