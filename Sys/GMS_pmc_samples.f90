

module pmc_samples



 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         pmc_samples
 !          
 !          Purpose:
 !                        
 !                        This module contains a few (4) derived types, which
 !                        contain the sampled state of Hardware Perfomance Counters.
 !          History:
 !                        Date: 08-02-2022
 !                        Time: 08:13 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                   Bernard Gingold
 !          
 !                 
 !          References:
 !         
 !                
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
    use mod_kinds, only : i1,i4,sp
    use ISO_C_BINDING, only : c_size_t,c_float,c_int
    implicit none
    public

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=i4),  parameter :: PMC_SAMPLES_MAJOR = 1 
    ! Minor version
    integer(kind=i4),  parameter :: PMC_SAMPLES_MINOR = 0
    ! Micro version
    integer(kind=i4),  parameter :: PMC_SAMPLES_MICRO = 0
    ! Module full version
    integer(kind=i4),  parameter :: PMC_SAMPLES_FULLVER = &
         1000*PMC_SAMPLES_MAJOR+100*PMC_SAMPLES_MINOR+10*PMC_SAMPLES_MINOR
    ! Module creation date
    character(*),        parameter :: PMC_SAMPLES_CREATION_DATE = "08-02-2022 08:19 +00200 (TUE 08 FEB 2022 GMT+2)"
    ! Module build date
    character(*),        parameter :: PMC_SAMPLES_BUILD_DATE    = __DATE__ " " __TIME__
    ! Module author info
    character(*),        parameter :: PMC_SAMPLES_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    ! Module short info
    character(*),        parameter :: PMC_SAMPLES_SYNOPSIS      = "Sampling state of HW Performance Counters (Intel)."

    integer(kind=i4), parameter, private :: ncores = 28 ! Up to CLX (incl.) architecture (number of cores)
    
    !=========================================================!
    !      Derived types
    !=========================================================!

    type, public :: core_counters1D
   
       ! 11 Performance Counters collected states
       ! of profiled one-dimensional loop/problem
       ! Fixed counters
       integer(c_size_t), allocatable, dimension(:) :: tsc   ! TSC values
       integer(c_size_t), allocatable, dimension(:) :: r309  ! INST_RETIRED.ANY
       integer(c_size_t), allocatable, dimension(:) :: r30a  ! CPU_CLK_UNHALTED.THREAD
       integer(c_size_t), allocatable, dimension(:) :: r30b  ! CPU_CLK_UNHALTED.REF_TSC
       !dir$ attributes align : 64 :: tsc
       !dir$ attributes align : 64 :: r309
       !dir$ attributes align : 64 :: r30a
       !dir$ attributes align : 64 :: r30b
       ! Programmable counters
       integer(c_size_t), allocatable, dimension(:) :: c0
       integer(c_size_t), allocatable, dimension(:) :: c1
       integer(c_size_t), allocatable, dimension(:) :: c2
       integer(c_size_t), allocatable, dimension(:) :: c3
       integer(c_size_t), allocatable, dimension(:) :: c4
       integer(c_size_t), allocatable, dimension(:) :: c5
       integer(c_size_t), allocatable, dimension(:) :: c6
       integer(c_size_t), allocatable, dimension(:) :: c7
       
       !dir$ attributes align : 64 :: c0
       !dir$ attributes align : 64 :: c1
       !dir$ attributes align : 64 :: c2
       !dir$ attributes align : 64 :: c3
       !dir$ attributes align : 64 :: c4
       !dir$ attributes align : 64 :: c5
       !dir$ attributes align : 64 :: c6
       !dir$ attributes align : 64 :: c7
       integer(kind=i4)                    :: nx  ! size of data
       real(c_float),   dimension(ncores)  :: tsc_freq  ! CPU physical cores nominal frequencies
       integer(c_int),  dimension(ncores)  :: cores     ! Core ID as reported by RDTSCP
       integer(c_int),  dimension(28)      :: sckt      ! Socket ID (Really two sockets are expected)
       integer(c_int)                      :: fix_pmc_width
       integer(c_int)                      :: core_pmc_width
       logical(kind=i1)                    :: is_alloc
    end type core_counters1D

    type, public :: core_counters2D

       ! 11 Performance Counters collected states
       ! of profiled two-dimensional loop/problem
       ! Fixed counters
       integer(c_size_t), allocatable, dimension(:,:) :: tsc   ! TSC values
       integer(c_size_t), allocatable, dimension(:,:) :: r309  ! INST_RETIRED.ANY
       integer(c_size_t), allocatable, dimension(:,:) :: r30a  ! CPU_CLK_UNHALTED.THREAD
       integer(c_size_t), allocatable, dimension(:,:) :: r30b  ! CPU_CLK_UNHALTED.REF_TSC
       !dir$ attributes align : 64 :: tsc
       !dir$ attributes align : 64 :: r309
       !dir$ attributes align : 64 :: r30a
       !dir$ attributes align : 64 :: r30b
       ! Programmable counters
       integer(c_size_t), allocatable, dimension(:,:) :: c0
       integer(c_size_t), allocatable, dimension(:,:) :: c1
       integer(c_size_t), allocatable, dimension(:,:) :: c2
       integer(c_size_t), allocatable, dimension(:,:) :: c3
       integer(c_size_t), allocatable, dimension(:,:) :: c4
       integer(c_size_t), allocatable, dimension(:,:) :: c5
       integer(c_size_t), allocatable, dimension(:,:) :: c6
       integer(c_size_t), allocatable, dimension(:,:) :: c7
       
       !dir$ attributes align : 64 :: c0
       !dir$ attributes align : 64 :: c1
       !dir$ attributes align : 64 :: c2
       !dir$ attributes align : 64 :: c3
       !dir$ attributes align : 64 :: c4
       !dir$ attributes align : 64 :: c5
       !dir$ attributes align : 64 :: c6
       !dir$ attributes align : 64 :: c7
       integer(kind=i4)                    :: nx  ! size of data
       integer(kind=i4)                    :: ny  ! size of data
       real(c_float),   dimension(ncores)  :: tsc_freq  ! CPU physical cores nominal frequencies
       integer(c_int),  dimension(ncores)  :: cores     ! Core ID as reported by RDTSCP
       integer(c_int),  dimension(28)      :: sckt      ! Socket ID (Really two sockets are expected)
       integer(c_int)                      :: fix_pmc_width
       integer(c_int)                      :: core_pmc_width
       logical(kind=i1)                    :: is_alloc
    end type core_counters2D

      type, public :: core_counters3D

       ! 11 Performance Counters collected states
       ! of profiled three-dimensional loop/problem
       ! Fixed counters
       integer(c_size_t), allocatable, dimension(:,:,:) :: tsc   ! TSC values
       integer(c_size_t), allocatable, dimension(:,:,:) :: r309  ! INST_RETIRED.ANY
       integer(c_size_t), allocatable, dimension(:,:,:) :: r30a  ! CPU_CLK_UNHALTED.THREAD
       integer(c_size_t), allocatable, dimension(:,:,:) :: r30b  ! CPU_CLK_UNHALTED.REF_TSC
       !dir$ attributes align : 64 :: tsc
       !dir$ attributes align : 64 :: r309
       !dir$ attributes align : 64 :: r30a
       !dir$ attributes align : 64 :: r30b
       ! Programmable counters
       integer(c_size_t), allocatable, dimension(:,:,:) :: c0
       integer(c_size_t), allocatable, dimension(:,:,:) :: c1
       integer(c_size_t), allocatable, dimension(:,:,:) :: c2
       integer(c_size_t), allocatable, dimension(:,:,:) :: c3
       integer(c_size_t), allocatable, dimension(:,:,:) :: c4
       integer(c_size_t), allocatable, dimension(:,:,:) :: c5
       integer(c_size_t), allocatable, dimension(:,:,:) :: c6
       integer(c_size_t), allocatable, dimension(:,:,:) :: c7
       
       !dir$ attributes align : 64 :: c0
       !dir$ attributes align : 64 :: c1
       !dir$ attributes align : 64 :: c2
       !dir$ attributes align : 64 :: c3
       !dir$ attributes align : 64 :: c4
       !dir$ attributes align : 64 :: c5
       !dir$ attributes align : 64 :: c6
       !dir$ attributes align : 64 :: c7
       integer(kind=i4)                    :: nx  ! size of data
       integer(kind=i4)                    :: ny  ! size of data
       integer(kind=i4)                    :: nz  ! size if data
       real(c_float),   dimension(ncores)  :: tsc_freq  ! CPU physical cores nominal frequencies
       integer(c_int),  dimension(ncores)  :: cores     ! Core ID as reported by RDTSCP
       integer(c_int),  dimension(28)      :: sckt      ! Socket ID (Really two sockets are expected)
       integer(c_int)                      :: fix_pmc_width
       integer(c_int)                      :: core_pmc_width
       logical(kind=i1)                    :: is_allocated
    end type core_counters3D

      type, public :: core_counters4D

       ! 11 Performance Counters collected states
       ! of profiled four-dimensional loop/problem
       ! Fixed counters
       integer(c_size_t), allocatable, dimension(:,:,:,:) :: tsc   ! TSC values
       integer(c_size_t), allocatable, dimension(:,:,:,:) :: r309  ! INST_RETIRED.ANY
       integer(c_size_t), allocatable, dimension(:,:,:,:) :: r30a  ! CPU_CLK_UNHALTED.THREAD
       integer(c_size_t), allocatable, dimension(:,:,:,:) :: r30b  ! CPU_CLK_UNHALTED.REF_TSC
       !dir$ attributes align : 64 :: tsc
       !dir$ attributes align : 64 :: r309
       !dir$ attributes align : 64 :: r30a
       !dir$ attributes align : 64 :: r30b
       ! Programmable counters
       integer(c_size_t), allocatable, dimension(:,:,:,:) :: c0
       integer(c_size_t), allocatable, dimension(:,:,:,:) :: c1
       integer(c_size_t), allocatable, dimension(:,:,:,:) :: c2
       integer(c_size_t), allocatable, dimension(:,:,:,:) :: c3
       integer(c_size_t), allocatable, dimension(:,:,:,:) :: c4
       integer(c_size_t), allocatable, dimension(:,:,:,:) :: c5
       integer(c_size_t), allocatable, dimension(:,:,:,:) :: c6
       integer(c_size_t), allocatable, dimension(:,:,:,:) :: c7
       
       !dir$ attributes align : 64 :: c0
       !dir$ attributes align : 64 :: c1
       !dir$ attributes align : 64 :: c2
       !dir$ attributes align : 64 :: c3
       !dir$ attributes align : 64 :: c4
       !dir$ attributes align : 64 :: c5
       !dir$ attributes align : 64 :: c6
       !dir$ attributes align : 64 :: c7
       integer(kind=i4)                    :: nx  ! size of data
       integer(kind=i4)                    :: ny  ! size of data
       integer(kind=i4)                    :: nz
       integer(kind=i4)                    :: nw
       real(c_float),   dimension(ncores)  :: tsc_freq  ! CPU physical cores nominal frequencies
       integer(c_int),  dimension(ncores)  :: cores     ! Core ID as reported by RDTSCP
       integer(c_int),  dimension(28)      :: sckt      ! Socket ID (Really two sockets are expected)
       integer(c_int)                      :: fix_pmc_width
       integer(c_int)                      :: core_pmc_width
       logical(kind=i1)                    :: is_alloc
    end type core_counters4D

    ! TODO
    ! To add Uncore-Counters samples derived types

  contains

    subroutine init_core_counters1D(pmc,nx,val)
      !dir$ attributes code_align : 32 :: init_core_counters1D
      !dir$ optimize : 3
      use mod_print_error, only : handle_fatal_memory_error
      type(core_counters1D),       intent(inout)               :: pmc
      integer(kind=i4),            intent(in), value           :: nx
      integer(c_size_t),           intent(in), value, optional :: val
      ! Locals
      character(len=128), automatic :: msg
      integer(kind=i4),   automatic :: err
      character(len=28), parameter :: name = 'init_core_counters1D'
      ! Exec code ...
      if(pmc.is_alloc) then
         return
      end if
      allocate( pmc.tsc(nx),     &
                pmc.r309(nx),    &
                pmc.r30a(nx),    &
                pmc.r30b(nx),    &
                pmc.c0(nx),      &
                pmc.c1(nx),      &
                pmc.c2(nx),      &
                pmc.c3(nx),      &
                pmc.c4(nx),      &
                pmc.c5(nx),      &
                pmc.c6(nx),      &
                pmc.c7(nx),      &
                STAT=err,        &
                ERRMSG=msg)
      if(err/=0) then
         call handle_fatal_memory_error(.true.,  &
                   name//msg)
      end if
      pmc.is_alloc=.true.
      if(present(val)) then
         pmc.tsc  = val
         pmc.r309 = val
         pmc.r30a = val
         pmc.r30b = val
         pmc.c0   = val
         pmc.c1   = val
         pmc.c2   = val
         pmc.c3   = val
         pmc.c4   = val
         pmc.c5   = val
         pmc.c6   = val
         pmc.c7   = val
      end if

    end subroutine init_core_counters1D
    

    subroutine init_core_counters2D(pmc,nx,ny,val)
      !dir$ attributes code_align : 32 :: init_core_counters2D
      !dir$ optimize : 3
      use mod_print_error, only : handle_fatal_memory_error
      type(core_counters2D),       intent(inout)               :: pmc
      integer(kind=i4),            intent(in), value           :: nx
      integer(kind=i4),            intent(in), value           :: ny
      integer(c_size_t),           intent(in), value, optional :: val
      ! Locals
      character(len=128), automatic :: msg
      integer(kind=i4),   automatic :: err
      character(len=28), parameter :: name = 'init_core_counters2D'
      ! Exec code ...
      if(pmc.is_alloc) then
         return
      end if
      allocate( pmc.tsc(nx,ny),     &
                pmc.r309(nx,ny),    &
                pmc.r30a(nx,ny),    &
                pmc.r30b(nx,ny),    &
                pmc.c0(nx,ny),      &
                pmc.c1(nx,ny),      &
                pmc.c2(nx,ny),      &
                pmc.c3(nx,ny),      &
                pmc.c4(nx,ny),      &
                pmc.c5(nx,ny),      &
                pmc.c6(nx,ny),      &
                pmc.c7(nx,ny),      &
                STAT=err,           &
                ERRMSG=msg)
      if(err/=0) then
          call handle_fatal_memory_error(.true.,  &
                   name//msg)
      end if
      pmc.is_alloc = .true.
      if(present(val)) then
         pmc.tsc  = val
         pmc.r309 = val
         pmc.r30a = val
         pmc.r30b = val
         pmc.c0   = val
         pmc.c1   = val
         pmc.c2   = val
         pmc.c3   = val
         pmc.c4   = val
         pmc.c5   = val
         pmc.c6   = val
         pmc.c7   = val
      end if

    end subroutine init_core_counters2D
    
    
    subroutine init_core_counters3D(pmc,nx,ny,nz,val)
      !dir$ attributes code_align : 32 :: init_core_counters3D
      !dir$ optimize : 3
      use mod_print_error, only : handle_fatal_memory_error
      type(core_counters3D),       intent(inout)               :: pmc
      integer(kind=i4),            intent(in), value           :: nx
      integer(kind=i4),            intent(in), value           :: ny
      integer(kind=i4),            intent(in), value           :: nz
      integer(c_size_t),           intent(in), value, optional :: val
      ! Locals
      character(len=128), automatic :: msg
      integer(kind=i4),   automatic :: err
      character(len=28), parameter :: name = 'init_core_counters3D'
      ! Exec code ...
      if(pmc.is_alloc) then
         return
      end if
      allocate( pmc.tsc(nx,ny,nz),     &
                pmc.r309(nx,ny,nz),    &
                pmc.r30a(nx,ny,nz),    &
                pmc.r30b(nx,ny,nz),    &
                pmc.c0(nx,ny,nz),      &
                pmc.c1(nx,ny,nz),      &
                pmc.c2(nx,ny,nz),      &
                pmc.c3(nx,ny,nz),      &
                pmc.c4(nx,ny,nz),      &
                pmc.c5(nx,ny,nz),      &
                pmc.c6(nx,ny,nz),      &
                pmc.c7(nx,ny,nz),      &
                STAT=err,        &
                ERRMSG=msg)
      if(err/=0) then
          call handle_fatal_memory_error(.true.,  &
                    name//msg)
      end if
      
      if(present(val)) then
         pmc.tsc  = val
         pmc.r309 = val
         pmc.r30a = val
         pmc.r30b = val
         pmc.c0   = val
         pmc.c1   = val
         pmc.c2   = val
         pmc.c3   = val
         pmc.c4   = val
         pmc.c5   = val
         pmc.c6   = val
         pmc.c7   = val
      end if

    end subroutine init_core_counters3D
    
  
    subroutine init_core_counters4D(pmc,nx,ny,nz,nw,val)
      !dir$ attributes code_align : 32 :: init_core_counters4D
      !dir$ optimize : 3
      type(core_counters4D),       intent(inout)               :: pmc
      integer(kind=i4),            intent(in), value           :: nx
      integer(kind=i4),            intent(in), value           :: ny
      integer(kind=i4),            intent(in), value           :: nz
      integer(kind=i4),            intent(in), value           :: nw
       integer(c_size_t),           intent(in), value, optional :: val
       ! Locals
      character(len=128), automatic :: msg
      integer(kind=i4),   automatic :: err
      character(len=28), parameter :: name = 'init_core_counters4D'
      ! Exec code ...
      if(pmc.is_alloc) then
         return
      end if
      allocate( pmc.tsc(nx,ny,nz,nw),     &
                pmc.r309(nx,ny,nz,nw),    &
                pmc.r30a(nx,ny,nz,nw),    &
                pmc.r30b(nx,ny,nz,nw),    &
                pmc.c0(nx,ny,nz,nw),      &
                pmc.c1(nx,ny,nz,nw),      &
                pmc.c2(nx,ny,nz,nw),      &
                pmc.c3(nx,ny,nz,nw),      &
                pmc.c4(nx,ny,nz,nw),      &
                pmc.c5(nx,ny,nz,nw),      &
                pmc.c6(nx,ny,nz,nw),      &
                pmc.c7(nx,ny,nz,nw),      &
                STAT=err,                 &
                ERRMSG=msg)
      if(err/=0) then
          call handle_fatal_memory_error(.true.,  &
                   name//msg)
      end if
      
      if(present(val)) then
         pmc.tsc  = val
         pmc.r309 = val
         pmc.r30a = val
         pmc.r30b = val
         pmc.c0   = val
         pmc.c1   = val
         pmc.c2   = val
         pmc.c3   = val
         pmc.c4   = val
         pmc.c5   = val
         pmc.c6   = val
         pmc.c7   = val
      end if

    end subroutine init_core_counters4D
    

    
    
    
    






end module pmc_samples
