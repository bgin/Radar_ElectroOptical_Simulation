
module mod_compare_cpu_gpu


  !===================================================================================85
  !---------------------------- DESCRIPTION ------------------------------------------85
  !
  !
  !
  !          Module  name:
  !                         'mod_compare_cpu_gpu'
  !          
  !          Purpose:
  !                     Facilitating a comparison of floating point scalar results
  !                     between CPU and GPU.
  !
  !          History:
  !                        
  !                        Date: 04-04-2019
  !                        Time: 19:19 GMT+2
  !          Version:
  !
  !                      Major: 1
  !                      Minor: 0
  !                      Micro: 0
  !
  !          Author:
  !                 
  !                   Bernard Gingold
  !         
  !         
  !         
  !          
  !         
  !          E-mail:
  !                  
  !                      beniekg@gmail.com
  !=================================================================================
  ! Tab:5 col - Type and etc.. definitions
  ! Tab:10,11 col - Type , function and subroutine code blocks.
  use mod_kinds,    only : i1, i4, sp, dp
  use mod_vectypes, only : XMM2i4_t,XMM2r8_t,XMM4r4_t,XMM4i4,YMM4r8_t,YMM8r4_t,YMM8i4_t,   &
                           ZMM16r4_t,ZMM16i4_t,ZMM8r8_t,Mask2_t,Mask4_t,Mask8_t
  use mod_fpcompare
  implicit none

  !=====================================================59
  !  File and module information:
  !  version,creation and build date, author,description
  !=====================================================59

  ! Major version
  integer(kind=i4), parameter, public :: MOD_COMPARE_CPU_GPU_MAJOR = 1_int4

  ! Minor version
  integer(kind=i4), parameter, public :: MOD_COMPARE_CPU_GPU_MINOR = 0_int4

  ! Micro version
  integer(kind=i4), parameter, public :: MOD_COMPARE_CPU_GPU_MICRO = 0_int4

  ! Module full version
  integer(kind=i4), parameter, public :: MOD_COMPARE_CPU_GPU_FULLVER = 1000*MOD_COMPARE_CPU_GPU_MAJOR + &
       100*MOD_COMPARE_CPU_GPU_MINOR  + &
       10*MOD_COMPARE_CPU_GPU_MICRO
  ! Module creation date
  character(*),       parameter, public :: MOD_COMPARE_CPU_GPU_CREATION_DATE = "04-04-2019 20:33 +00200 (THR 4 APR 2019 GMT+2) "

  ! Module build date
  character(*),       parameter, public :: MOD_COMPARE_CPU_GPU_BUILD_DATE = "00-00-0000 00:00 "

  ! Module author info
  character(*),       parameter, public :: MOD_COMPARE_CPU_GPU_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com "

  ! Module short synopsis
  character(*),       parameter, public :: MOD_COMPARE_CPU_GPU_SYNOPSIS = "Floating-point reliable comparison of CPU-GPU results."

  !DIR$ IF .NOT. DEFINED (GMS_COMPARE_CPU_GPU_ADD_PADDING)
  !DIR$ DEFINE GMS_COMPARE_CPU_GPU_ADD_PADDING = 0   ! At least set to 0 (must figure out the size of descriptors)
  !DIR$ ENDIF

  ! Constants (local)
  
  real(kind=sp), parameter, private :: TINYR4   = TINY(1.0_sp)
  real(kind=dp), parameter, private :: TINYR8   = TINY(1.0_dp)
  complex(kind=sp), parameter, private :: CTINYR4 = (TINYR4,TINYR4)
  complex(kind=dp), parameter, private :: CTINYR8 = (TINYR8,TINYR8)
  real(kind=sp), parameter, private :: RADIX2R4 = 2.0_sp
  real(kind=dp), parameter, private :: RADIX2R8 = 2.0_dp
  real(kind=sp), parameter, private :: ZEROR4   = 0.0_sp
  real(kind=dp), parameter, private :: ZEROR8   = 0.0_dp
  real(kind=dp), parameter, private :: ONER8    = 1.0_dp
  real(kind=sp), parameter, private :: ONER4    = 1.0_sp
  real(kind=dp), parameter, private :: TENR8    = 10.0_dp
  real(kind=sp), parameter, private :: TENR4    = 10.0_sp
  real(kind=dp), parameter, private :: HUNDREDR8 = 100.0_dp
  real(kind=sp), parameter, private :: HUNDREDR4 = 100.0_sp
  real(kind=dp), parameter, private :: EPSR8     = 1.0E-15_dp
  real(kind=sp), parameter, private :: EPSR4     = 1.0E-15_sp
  real(kind=dp), parameter, private :: SRSR8     = 1.110223024625157E-16_dp
  real(kind=sp), parameter, private :: SRSR4     = 5.9604645E-8_sp
  real(kind=dp), parameter, private :: LRSR8     = 2.220446049250313E-16_dp
  real(kind=sp), parameter, private :: LRSR4     = 1.1920929E-7_sp
  integer(kind=i4), parameter, private :: EXPR4 = 24
  integer(kind=i4), parameter, private :: EXPR8 = 53
  integer(kind=i4), parameter, public  :: MAX_FUNC_NAME = 256
  type(XMM2r8_t),     parameter, private :: XMM2r8_InitVal  = XMM2r8_t(TINYR8)
  type(XMM4r4_t),     parameter, private :: XMM4r4_InitVal  = XMM4r4_t(TINYR4)
  type(YMM4r8_t),     parameter, private :: YMM4r8_InitVal  = YMM4r8_t(TINYR8)
  type(YMM8r4_t),     parameter, private :: YMM8r4_InitVal  = YMM8r4_t(TINYR4)
  type(ZMM16r4_t),    parameter, private :: ZMM16r4_InitVal = ZMM16r4_t(TINYR4)
  type(ZMM8r8_t),     parameter, private :: ZMM8r8_InitVal  = ZMM8r8_t(TINYR8)
  type(Mask2_t),      parameter, private :: Mask2_InitVal   = Mask2_t(.false.)
  type(Mask4_t),      parameter, private :: Mask4_InitVal   = Mask4_t(.false.)
  type(Mask8_t),      parameter, private :: Mask8_InitVal   = Mask8_t(.false.)
  type(Mask16_t),     parameter, private :: Mask16_InitVal  = Mask16_t(.false.)
  
  type, public :: Real4CompareData1D_t
     public
     character(len=MAX_FUNC_NAME) :: cfname !  // name of tested cpu function
     character(len=MAX_FUNC_NAME) :: gfname !  // name of tested gpu(kernel) function
     character(len=64)            :: cmpfname  ! name of floating-point reliable comparison function
   
     integer(kind=i4)           :: nx
     
    
     !DIR$   ATTRIBUTES ALIGN : 64 :: gloop_idx
     integer(kind=i4), allocatable, dimension(:) :: gloop_idx !  Same as above stores indices
     !  // of GPU failed values, indices of failed GPU results
     !DIR$   ATTRIBUTES ALIGN : 64 :: cpu_results        
     real(kind=sp),      allocatable, dimension(:) :: cpu_results ! valid cpu values (reference)
     !DIR$   ATTRIBUTES ALIGN : 64 :: gpu_results
     real(kind=sp),      allocatable, dimension(:) :: gpu_results ! gpu results
     !DIR$   ATTRIBUTES ALIGN : 64 :: inequality
     real(kind=sp),      allocatable, dimension(:) :: inequality  ! Stores GPU results which are inequal to refrence CPU results.
     !DIR$   ATTRIBUTES ALIGN : 64 :: delta
     real(kind=sp),      allocatable, dimension(:) :: delta
     !DIR$   ATTRIBUTES ALIGN : 64 :: status
     logical(kind=int4), allocatable, dimension(:) :: status ! logical -- return value of fp-compare functions
   

  end type Real4CompareData1D_t
  !================================================================================================================!
  
  type, public :: Real4CompareData2D_t
     public
     character(len=MAX_FUNC_NAME) :: cfname !  // name of tested cpu function
     character(len=MAX_FUNC_NAME) :: gfname !  // name of tested gpu(kernel) function
     character(len=64)            :: cmpfname  ! name of floating-point reliable comparison function
    
     integer(kind=i4)           :: nx
     integer(kind=i4)           :: ny
    
     !DIR$   ATTRIBUTES ALIGN : 64 :: gloop_idx
     integer(kind=i4), allocatable, dimension(:,:) :: gloop_idx !  Same as above stores indices
     !  // of GPU failed values.
     !DIR$   ATTRIBUTES ALIGN : 64 :: cpu_results       
     real(kind=sp),      allocatable, dimension(:,:) :: cpu_results ! valid cpu values (reference)
     !DIR$   ATTRIBUTES ALIGN : 64 :: gpu_results
     real(kind=sp),      allocatable, dimension(:,:) :: gpu_results
     !DIR$   ATTRIBUTES ALIGN : 64 :: inequality
     real(kind=sp),      allocatable, dimension(:,:) :: inequality
     !DIR$   ATTRIBUTES ALIGN : 64 :: delta
     real(kind=sp),      allocatable, dimension(:,:) :: delta
     !DIR$   ATTRIBUTES ALIGN : 64 :: status
     logical(kind=i4), allocatable, dimension(:,:) :: status
  
  end type Real4CompareData2D_t
  !=================================================================================================================!
  type, public :: Real4CompareData3D_t
     public
     character(len=MAX_FUNC_NAME) :: cfname !  // name of tested cpu function
     character(len=MAX_FUNC_NAME) :: gfname !  // name of tested gpu(kernel) function
     character(len=64)            :: cmpfname  ! name of floating-point reliable comparison function
    
     integer(kind=i4)           :: nx
     integer(kind=i4)           :: ny
     integer(kind=i4)           :: nz
   
     !DIR$   ATTRIBUTES ALIGN : 64 :: gloop_idx
     integer(kind=i4), allocatable, dimension(:,:,:) :: gloop_idx !  Same as above stores indices
     !  // of GPU failed values.
     !DIR$   ATTRIBUTES ALIGN : 64 :: cpu_results        
     real(kind=sp),      allocatable, dimension(:,:,:) :: cpu_results ! valid cpu values (reference)
     !DIR$   ATTRIBUTES ALIGN : 64 :: gpu_results
     real(kind=sp),      allocatable, dimension(:,:,:) :: gpu_results
     !DIR$   ATTRIBUTES ALIGN : 64 :: inequality
     real(kind=sp),      allocatable, dimension(:,:,:) :: inequality
     !DIR$   ATTRIBUTES ALIGN : 64 :: delta
     real(kind=sp),      allocatable, dimension(:,:,:) :: delta
     !DIR$   ATTRIBUTES ALIGN : 64 :: status
     logical(kind=i4), allocatable, dimension(:,:,:) :: status
   
  end type Real4CompareData3D_t
 
  !=======================================================================================================================!
  type, public :: Real8CompareData1D_t
     public
     character(len=MAX_FUNC_NAME) :: cfname !  // name of tested cpu function
     character(len=MAX_FUNC_NAME) :: gfname !  // name of tested gpu(kernel) function
     character(len=64)            :: cmpfname  ! name of floating-point reliable comparison function
    
     integer(kind=i4)           :: nx
   
     !DIR$   ATTRIBUTES ALIGN : 64 :: gloop_idx
     integer(kind=i4), allocatable, dimension(:) :: gloop_idx !  Same as above stores indices
     !  // of GPU failed values, indices of failed GPU results
     !DIR$   ATTRIBUTES ALIGN : 64 :: cpu_results      
     real(kind=dp),      allocatable, dimension(:) :: cpu_results ! valid cpu values (reference)
     !DIR$   ATTRIBUTES ALIGN : 64 :: gpu_results
     real(kind=dp),      allocatable, dimension(:) :: gpu_results ! gpu results
     !DIR$   ATTRIBUTES ALIGN : 64 :: inequality
     real(kind=dp),      allocatable, dimension(:) :: inequality
     !DIR$   ATTRIBUTES ALIGN : 64 :: delta
     real(kind=dp),      allocatable, dimension(:) :: delta
     !DIR$   ATTRIBUTES ALIGN : 64 :: status
     logical(kind=i4), allocatable, dimension(:) :: status ! logical -- return value of fp-compare functions
    
  end type Real8CompareData1D_t
  !===============================================================================================================================!
  type, public :: Real8CompareData2D_t
     public
     character(len=MAX_FUNC_NAME) :: cfname !  // name of tested cpu function
     character(len=MAX_FUNC_NAME) :: gfname !  // name of tested gpu(kernel) function
     character(len=64)            :: cmpfname  ! name of floating-point reliable comparison function
    
     integer(kind=i4)           :: nx
     integer(kind=i4)           :: ny
    
     !DIR$   ATTRIBUTES ALIGN : 64 :: gloop_idx
     integer(kind=i4), allocatable, dimension(:,:) :: gloop_idx !  Same as above stores indices
     !  // of GPU failed values.
     !DIR$   ATTRIBUTES ALIGN : 64 :: cpu_results       
     real(kind=dp),      allocatable, dimension(:,:) :: cpu_results ! valid cpu values (reference)
     !DIR$   ATTRIBUTES ALIGN : 64 :: gpu_results
     real(kind=dp),      allocatable, dimension(:,:) :: gpu_results
     !DIR$   ATTRIBUTES ALIGN : 64 :: inequality
     real(kind=dp),      allocatable, dimension(:,:) :: inequality
     !DIR$   ATTRIBUTES ALIGN : 64 :: delta
     real(kind=dp),      allocatable, dimension(:,:) :: delta
     !DIR$   ATTRIBUTES ALIGN : 64 :: status
     logical(kind=i4), allocatable, dimension(:,:) :: status
   
  end type Real8CompareData2D_t
  !======================================================================================================================!
  type, public :: Real8CompareData3D_t
     public
     character(len=MAX_FUNC_NAME) :: cfname !  // name of tested cpu function
     character(len=MAX_FUNC_NAME) :: gfname !  // name of tested gpu(kernel) function
     character(len=64)            :: cmpfname  ! name of floating-point reliable comparison function
   
     integer(kind=i4)           :: nx
     integer(kind=i4)           :: ny
     integer(kind=i4)           :: nz
    
     !DIR$   ATTRIBUTES ALIGN : 64 :: gloop_idx
     integer(kind=i4), allocatable, dimension(:,:,:) :: gloop_idx !  Same as above stores indices
     !  // of GPU failed values.
     !DIR$   ATTRIBUTES ALIGN : 64 :: cpu_results        
     real(kind=dp),      allocatable, dimension(:,:,:) :: cpu_results ! valid cpu values (reference)
     !DIR$   ATTRIBUTES ALIGN : 64 :: gpu_results
     real(kind=dp),      allocatable, dimension(:,:,:) :: gpu_results
     !DIR$   ATTRIBUTES ALIGN : 64 :: inequality
     real(kind=dp),      allocatable, dimension(:,:,:) :: inequality
     !DIR$   ATTRIBUTES ALIGN : 64 :: delta
     real(kind=dp),      allocatable, dimension(:,:,:) :: delta
     !DIR$   ATTRIBUTES ALIGN : 64 :: status
     logical(kind=i4), allocatable, dimension(:,:,:) :: status
    
  end type Real8CompareData3D_t
 
  !====================================================================================================!
  type, public :: Complex4CompareData_t
     public
     character(len=MAX_FUNC_NAME) :: cfname !  // name of tested cpu function
     character(len=MAX_FUNC_NAME) :: gfname !  // name of tested gpu(kernel) function
     character(len=64)            :: cmpfname  ! name of floating-point reliable comparison function

     integer(kind=i4)           :: nx
    
     !DIR$   ATTRIBUTES ALIGN : 64 :: gloop_idx
     integer(kind=i4),   allocatable, dimension(:) :: gloop_idx
     !DIR$   ATTRIBUTES ALIGN : 64 :: cpu_results
     complex(kind=sp),     allocatable, dimension(:) :: cpu_results
     !DIR$   ATTRIBUTES ALIGN : 64 :: gpu_results
     complex(kind=sp),     allocatable, dimension(:) :: gpu_results
     !DIR$   ATTRIBUTES ALIGN : 64 :: inequality
     complex(kind=sp),     allocatable, dimension(:) :: inequality
     !DIR$   ATTRIBUTES ALIGN : 64 :: delta
     complex(kind=sp),     allocatable, dimension(:) :: delta
     !DIR$   ATTRIBUTES ALIGN : 64 :: status
     logical(kind=i4),   allocatable, dimension(:) :: status

  end type Complex4CompareData_t



 !===============================================================================================!
  type, public :: XMM2r8CompareData_t
     public
     character(len=MAX_FUNC_NAME) :: cfname
     character(len=MAX_FUNC_NAME) :: gfname
     character(len=64)            :: cmpfname
     integer(kind=int4)           :: nx
!DIR$ ATTRIBUTES ALIGN : 64 :: vidx
     integer(kind=i4), allocatable, dimension(:) :: vidx
!DIR$ ATTRIBUTES ALIGN : 64 :: cpu_results
     type(XMM2r8_t),  allocatable, dimension(:) :: cpu_results
!DIR$ ATTRIBUTES ALIGN : 64 :: gpu_results
     type(XMM2r8_t),  allocatable, dimension(:) :: gpu_results
!DIR$ ATTRIBUTES ALIGN : 64 :: inequality
     type(XMM2r8_t),  allocatable, dimension(:) :: inequality
!DIR$ ATTRIBUTES ALIGN : 64 :: delta
     type(XMM2r8_t),  allocatable, dimension(:) :: delta
!DIR$ ATTRIBUTES ALIGN : 64 :: status
     type(Mask2_t),   allocatable, dimension(:) :: status
  end type XMM2r8CompareData_t
!===============================================================================================!
  type, public :: XMM4r4CompareData_t
     public
     character(len=MAX_FUNC_NAME) :: cfname
     character(len=MAX_FUNC_NAME) :: gfname
     character(len=64)            :: cmpfname
     integer(kind=i4)           :: nx
!DIR$ ATTRIBUTES ALIGN : 64 :: vidx
     integer(kind=i4), allocatable, dimension(:) :: vidx
!DIR$  ATTRIBUTES ALIGN : 64 :: cpu_results
     type(XMM4r4_t), allocatable, dimension(:) :: cpu_results
!DIR$ ATTRIBUTES ALIGN : 64 :: gpu_results
     type(XMM4r4_t), allocatable, dimension(:) :: gpu_results
!DIR$ ATTRIBUTES ALIGN : 64 :: inequality
     type(XMM4r4_t), allocatable, dimension(:) :: inequality
!DIR$ ATTRIBUTES ALIGN : 64 :: delta
     type(XMM4r4_t), allocatable, dimension(:) :: delta
!DIR$ ATTRIBUTES ALIGN : 64 :: status
     type(Mask4_t), allocatable, dimension(:)  :: status
  end type XMM4r4CompareData_t
!=============================================================================================!
  type, public :: YMM4r8CompareData_t
     public
     character(len=MAX_FUNC_NAME) :: cfname
     character(len=MAX_FUNC_NAME) :: gfname
     character(len=64)            :: cmpfname
     integer(kind=int4)           :: nx
!DIR$ ATTRIBUTES ALIGN : 64 :: vidx
     integer(kind=i4), allocatable, dimension(:) :: vidx
!DIR$ ATTRIBUTES ALIGN : 64 :: cpu_results
     type(YMM4r8_t), allocatable, dimension(:) :: cpu_results
!DIR$ ATTRIBUTES ALIGN : 64 :: gpu_results
     type(YMM4r8_t), allocatable, dimension(:) :: gpu_results
!DIR$ ATTRIBUTES ALIGN : 64 :: inequality
     type(YMM4r8_t), allocatable, dimension(:) :: inequality
!DIR$ ATTRIBUTES ALIGN : 64 :: delta
     type(YMM4r8_t), allocatable, dimension(:) :: delta
!DIR$ ATTRIBUTES ALIGN : 64 :: status
     type(Mask4_t),  allocatable, dimension(:) :: status
  end type YMM4r8CompareData_t
!===========================================================================================!
  type, public :: YMM8r4CompareData_t
     public
     character(len=MAX_FUNC_NAME) :: cfname
     character(len=MAX_FUNC_NAME) :: gfname
     character(len=64)            :: cmpfname
     integer(kind=i4)           :: nx
!DIR$ ATTRIBUTES ALIGN : 64 :: vidx
     integer(kind=i4), allocatable, dimension(:) :: vidx
!DIR$ ATTRIBUTES ALIGN : 64 :: cpu_results
     type(YMM8r4_t), allocatable, dimension(:) :: cpu_results
!DIR$ ATTRIBUTES ALIGN : 64 :: gpu_results
     type(YMM8r4_t), allocatable, dimension(:) :: gpu_results
!DIR$ ATTRIBUTES ALIGN : 64 :: inequality
     type(YMM8r4_t), allocatable, dimension(:) :: inequality
!DIR$ ATTRIBUTES ALIGN : 64 :: delta
     type(YMM8r4_t), allocatable, dimension(:) :: delta
!DIR$ ATTRIBUTES ALIGN : 64 :: status
     type(Mask8_t), allocatable, dimension(:)  :: status
  end type YMM8r4CompareData_t
!============================================================================================!
  type, public :: ZMM8r8CompareData_t
     public
     character(len=MAX_FUNC_NAME) :: cfname
     character(len=MAX_FUNC_NAME) :: gfname
     character(len=64)            :: cmpfname
     integer(kind=i4)           :: nx
!DIR$ ATTRIBUTES ALIGN : 64 :: vidx
     integer(kind=i4), allocatable, dimension(:) :: vidx
!DIR$ ATTRIBUTES ALIGN : 64 :: cpu_results
     type(ZMM8r8_t), allocatable, dimension(:) :: cpu_results
!DIR$ ATTRIBUTES ALIGN : 64 :: gpu_results
     type(ZMM8r8_t), allocatable, dimension(:) :: gpu_results
!DIR$ ATTRIBUTES ALIGN : 64 :: inequality
     type(ZMM8r8_t), allocatable, dimension(:) :: inequality
!DIR$ ATTRIBUTES ALIGN : 64 :: delta
     type(ZMM8r8_t), allocatable, dimension(:) :: delta
!DIR$ ATTRIBUTES ALIGN : 64 ::  status
     type(Mask8_t), allocatable, dimension(:)  :: status
  end type ZMM8r8CompareData_t
  !===========================================================================================!
  type, public :: ZMM16r4CompareData_t
     public
     character(len=MAX_FUNC_NAME) :: cfname
     character(len=MAX_FUNC_NAME) :: gfname
     character(len=64)            :: cmpfname
     integer(kind=int4)           :: nx
!DIR$ ATTRIBUTES ALIGN : 64 :: vidx
     integer(kind=i4), allocatable, dimension(:) :: vidx
!DIR$ ATTRIBUTES ALIGN : 64 :: cpu_results
     type(ZMM16r4_t), allocatable, dimension(:) :: cpu_results
!DIR$ ATTRIBUTES ALIGN : 64 :: gpu_results
     type(ZMM16r4_t), allocatable, dimension(:) :: gpu_results
!DIR$ ATTRIBUTES ALIGN : 64 :: inequality
     type(ZMM16r4_t), allocatable, dimension(:) :: inequality
!DIR$ ATTRIBUTES ALIGN : 64 :: delta
     type(ZMM16r4_t), allocatable, dimension(:) :: delta
!DIR$ ATTRIBUTES ALIGN : 64 :: status
     type(Mask16_t),  allocatable, dimension(:) ::  status
  end type ZMM16r4CompareData_t

contains

  !=========================================================!
  !   InitReal4CompareData1D
  !=========================================================!
     subroutine InitReal4CompareData1D(R4Data1D,nx,errstate,iounit,  &
                                       logging,verbose,append,fname)
                                  

                                 
                                  
!DIR$   ATTRIBUTES CODE_ALIGN:32 :: InitReal4CompareData1D
        use GMS_mod_print_error, only : handle_fatal_memory_error
        type(Real4CompareData1D_t),       intent(inout) :: R4Data1D

        integer(kind=i4),               intent(in)    :: nx

        logical(kind=i1),               intent(inout) :: errstate
        integer(kind=i4),               intent(in)    :: iounit
        logical(kind=i4),               intent(in)    :: logging
        logical(kind=i4),               intent(in)    :: verbose
        logical(kind=i4),               intent(in)    :: append
        character(len=*),                 intent(in)    :: fname
        ! Locals
        character(len=256), automatic :: emsg
        integer(kind=i4), automatic :: aerr
        ! Exec code .....
        R4Data1D.cfname   = "Not initialized yet "
        R4Data1D.gfname   = "Not initialized yet "
        R4Data1D.cmpfname = "Set during the comparison"

        R4Data1D.nx = nx

      
        if(allocated(R4Data1D.gloop_idx)) then
               deallocate(R4Data1D.gloop_idx)
               allocate(R4Data1D.gloop_idx(R4Data1D.nx), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
        else
               allocate(R4Data1D.gloop_idx(R4Data1D.nx), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
        end if
        R4Data1D.gloop_idx = -1
        if(allocated(R4Data1D.cpu_results)) then
               deallocate(R4Data1D.cpu_results)
               allocate(R4Data1D.cpu_results(R4Data1D.nx), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
        else
              allocate(R4Data1D.cpu_results(R4Data1D.nx), &
                    STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
        end if
        R4Data1D.cpu_results = TINY(1.0_sp)
        if(allocated(R4Data1D.gpu_results)) then
               deallocate(R4Data1D.gpu_results)
               allocate(R4Data1D.gpu_results(R4Data1D.nx), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
        else
               allocate(R4Data1D.gpu_results(R4Data1D.nx), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
        end if
        R4Data1D.gpu_results = TINY(1.0_sp)
        if(allocated(R4Data1D.inequality)) then
               deallocate(R4Data1D.inequality)
               allocate(R4Data1D.inequality(R4Data1D.nx), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
        else
               allocate(R4Data1D.inequality(R4Data1D.nx), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
        end if
        R4Data1D.inequality = TINY(1.0_sp)
        if(allocated(R4Data1D.delta)) then
               deallocate(R4Data1D.delta)
               allocate(R4Data1D.delta(R4Data1D.nx), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
        else
               allocate(R4Data1D.delta(R4Data1D.nx), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
        end if
        R4Data1D.delta = TINY(1.0_sp)
        if(allocated(R4Data1D.status)) then
               deallocate(R4Data1D.status)
               allocate(R4Data1D.status(R4Data1D.nx), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
        else
               allocate(R4Data1D.status(R4Data1D.nx), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
        end if
        R4Data1D.status = .false.
        errstate = .false.
        return
9999    call handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitReal4CompareData1D -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitReal4CompareData1D -- Memory Allocation Failure !!", &
                              emsg,770)

          
          
      end subroutine InitReal4CompareData1D
!===================================================================================================================!
     subroutine FPCompareReal4Data1D(R4Data1D,ulp,percent,n,cutoff,method)
        !DIR$  ATTRIBUTES CODE_ALIGN:32 :: FPCompareReal4Data1D
           use mod_print_error,  only : print_non_fatal_error
           type(Real4CompareData1D_t),      intent(inout) :: R4Data1D
           integer(kind=i4),              intent(in)    :: ulp
           real(kind=sp),                   intent(in)    :: percent
           integer(kind=i4),              intent(in)    :: n
           real(kind=sp),                   intent(in)    :: cutoff
           character(len=32),               intent(in)    :: method
           ! Locals
           integer(kind=i4), automatic :: i
           logical(kind=i4), automatic :: bres
           ! Exec code .....
Comparison:  select case(method)
               case("EqualTo")
                  R4Data1D.cmpfname = "EqualTo_Real_Single"
                  bres = .false.
                   !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                   do i=1,   R4Data1D.nx
                       if(bres=R4Data1D.cpu_results(i).EqualTo.R4Data1D.gpu_results(i)) then
                             R4Data1D.delta(i) = R4Data1D.cpu_results(i) - &
                                                 R4Data1D.gpu_results(i)
                             R4Data1D.status(i) = bres
                          else
                            
                             R4Data1D.gloop_idx(i) = i
                             R4Data1D.inequality(i) = R4Data1D.gpu_results(i)
                             R4Data1D.delta(i) =  R4Data1D.cpu_results(i) - &
                                                  R4Data1D.gpu_results(i)
                             R4Data1D.status(i) = bres
                        end if
                    end do
               case("Compare_Float")
                  R4Data1D.cmpfname = "Compare_Real_Single"
                  bres = .false.
                  !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                  do i=1, R4Data1D.nx
                      if(bres=Compare_Float(R4Data1D.cpu_result(i),  &
                                            R4Data1D.gpu_result(i),  &
                                            ULP=ulp,                 &
                                            Percent=percent))   then
                             R4Data1D.delta(i) = R4Data1D.cpu_results(i) - &
                                                 R4Data1D.gpu_results(i)
                             R4Data1D.status(i) = bres
                       else
                            
                             R4Data1D.gloop_idx(i) = i
                             R4Data1D.inequality(i) = R4Data1D.gpu_results(i)
                             R4Data1D.delta(i) =  R4Data1D.cpu_results(i) - &
                                                  R4Data1D.gpu_results(i)
                             R4Data1D.status(i) = bres
                       end if
                    end do
               case("Compare_Within_Tolerance")
                  R4Data1D.cmpfname = "cwt_Real_Single"
                  bres = .false.
                  !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                  do i=1, R4Data1D.nx
                     if(bres=Compare_Within_Tolerance(R4Data1D.cpu_results(i),  &
                                                      R4Data1D.gpu_results(i),  &
                                                      n, &
                                                      cutoff=cutoff)) then
                               R4Data1D.delta(i)  = R4Data1D.cpu_results(i) - &
                                                 R4Data1D.gpu_results(i)
                               R4Data1D.status(i) = bres
                      else
                             
                               R4Data1D.gloop_idx(i) = i
                               R4Data1D.inequality(i) = R4Data1D.gpu_results(i)
                               R4Data1D.delta(i) =  R4Data1D.cpu_results(i) - &
                                                    R4Data1D.gpu_results(i)
                               R4Data1D.status(i) = bres
                      end if
                   end do
                case default
                     call print_non_fatal_error( ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPCompareReal4Data1D: Unrecognized switch argument!! ",  &
                                               __LINE__,__FILE__)
                     return
                end select Comparison                               
                       
     end subroutine FPCompareReal4Data1D
!================================================================================================================================!
     subroutine InitReal4CompareData2D(R4Data2D,nx,ny,errstate,iounit, &
                                       logging,verbose,append,fname)

!DIR$  ATTRIBUTES CODE_ALIGN:32 :: InitREal4CompareData2D
           use mod_print_error, only : handle_fatal_memory_error
           type(Real4CompareData2D_t),            intent(inout) :: R4Data2D

           integer(kind=i4),               intent(in)    :: nx
           integer(kind=i4),               intent(in)    :: ny

           logical(kind=i1),               intent(inout) :: errstate
           integer(kind=i4),               intent(in)    :: iounit
           logical(kind=i4),               intent(in)    :: logging
           logical(kind=i4),               intent(in)    :: verbose
           logical(kind=i4),               intent(in)    :: append
           character(len=*),                 intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=int4), automatic :: aerr
        
           ! Exec code .....
           R4Data2D.cfname   = "Not initialized yet "
           R4Data2D.gfname   = "Not initialized yet "
           R4Data2D.cmpfname = "Set during the comparison"

           R4Data2D.nx = nx
           R4Data2D.ny = ny


         
           if(allocated(R4Data2D.gloop_idx)) then
              deallocate(R4Data2D.gloop_idx)
              allocate(R4Data2D.gloop_idx(R4Data2D.nx,R4Data2D.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(R4Data2D.gloop_idx(R4Data2D.nx,R4Data2D.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           R4Data2D.gloop_idx = -1
           if(allocated(R4Data2D.cpu_results)) then
              deallocate(R4Data2D.cpu_results)
              allocate(R4Data2D.cpu_results(R4Data2D.nx,R4Data2D.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(R4Data2D.cpu_results(R4Data2D.nx,R4Data2D.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           R4Data2D.cpu_results = TINY(1.0_sp)
           if(allocated(R4Data2D.gpu_results)) then
              deallocate(R4Data2D.gpu_results)
              allocate(R4Data2D.gpu_results(R4Data2D.nx,R4Data2D.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(R4Data2D.gpu_results(R4Data2D.nx,R4Data2D.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           R4Data2D.gpu_results = TINY(1.0_sp)
           if(allocated(R4Data2D.inequality)) then
              deallocate(R4Data2D.inequality)
              allocate(R4Data2D.inequality(R4Data2D.nx,R4Data2D.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(R4Data2D.inequality(R4Data2D.nx,R4Data2D.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           R4Data2D.inequality = TINY(1.0_sp)
           if(allocated(R4Data2D.delta)) then
              deallocate(R4Data2D.delta)
              allocate(R4Data2D.delta(R4Data2D.nx,R4Data2D.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(R4Data2D.delta(R4Data2D.nx,R4Data2D.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           R4Data2D.delta = TINY(1.0_sp)
           if(allocated(R4Data2D.status)) then
              deallocate(R4Data2D.status)
              allocate(R4Data2D.status(R4Data2D.nx,R4Data2D.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(R4Data2D.status(R4Data2D.nx,R4Data2D.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           R4Data2D.status = .false.
           errstate = .false.
           return
9999       call handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitReal4CompareData2D -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitReal4CompareData2D -- Memory Allocation Failure !!", &
                              emsg,760)

      
         
     end subroutine InitReal4CompareData2D         
!===========================================================================================================!
     subroutine FPCompareReal4Data2D(Data2D,ulp,percent,n,cutoff,method)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: FPCompareReal4Data2D
            use mod_print_error, only : print_non_fatal_error
            type(Real4CompareData2D_t),       intent(inout) :: Data2D
            integer(kind=i4),               intent(in)    :: ulp
            real(kind=sp),                    intent(in)    :: percent
            integer(kind=i4),               intent(in)    :: n
            real(kind=sp),                    intent(in)    :: cutoff
            character(len=32),                intent(in)    :: method
            ! Locals
            integer(kind=i4) :: i,j,idx
            logical(kind=i4) :: bres
            ! Exec code .....
Comparison:   select case("method")
                 case("EqualTo")
                     Data2D.cmpfname = "EqualTo_Real_Single"
                     bres = .false.
                     idx = 0
                     do j=1, Data2D.ny
                        !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                        do i=1, Data2D.nx
                           bres = Data2D.cpu_results(i,j) .EqualTo. &
                                  Data2D.GPU_RESULTS(i,j)
                           if(bres) then
                              !idx = i*nx+j
                              
                              Data2D.delta(i,j) = Data2D.cpu_results(i,j)- &
                                                  Data2D.gpu_results(i,j)
                              Data2D.status(i,j) = bres
                           else
                              idx = i*nx+j
                             
                              Data2D.gloop_idx(i,j) = idx
                              Data2D.inequality(i,j) = Data2D.gpu_results(i,j)
                              Data2D.delta(i,j) = Data2D.cpu_results(i,j) - &
                                   Data2D.gpu_results(i,j)
                              Data2D.status(i,j) = bres
                           end if
                        end do
                     end do
                  case("Compare_Float")
                     Data2D.cmpfname = "Compare_Real_Single"
                     bres = .false.
                     idx = 0
                     do j=1, Data2D.ny
                        !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                        do i=1, Data2D.nx
                           bres = Compare_Float(Data2D.cpu_results(i,j),  &
                                                Data2D.gpu_results(i,j),  &
                                                ULP=ulp
                                                PERCENT=percent)
                           if(bres)  then
                              Data2D.delta(i,j) = Data2D.cpu_results(i,j) - &
                                                  Data2D.gpu_results(i,j)
                              Data2D.status(i,j) = bres
                           else
                              idx = i*nx+j
                            
                              Data2D.gloop_idx(i,j) = idx
                              Data2D.inequality(i,j) = Data2D.gpu_results(i,j)
                              Data2D.delta(i,j) = Data2D.cpu_results(i,j) - &
                                   Data2D.gpu_results(i,j)
                              Data2D.status(i,j) = bres
                           end if
                        end do
                     end do
                  case("Compare_Within_Tolerance")
                     Data2D.cmpfname = "cwt_Real_Single"
                     bres = .false.
                     idx = 0
                     do j=1, Data2D.ny
                        !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                        do i=1, Data2D.nx
                           bres = Compare_Within_Tolerance(Data2D.cpu_results(i,j), &
                                                           Data2D.gpu_results(i,j), &
                                                           n,                       &
                                                           cutoff=cutoff)
                           if(bres) then
                               Data2D.delta(i,j) = Data2D.cpu_results(i,j) - &
                                                   Data2D.gpu_results(i,j)
                              Data2D.status(i,j) = bres
                           else
                              idx = i*nx+j
                           
                              Data2D.gloop_idx(i,j) = idx
                              Data2D.inequality(i,j) = Data2D.gpu_results(i,j)
                              Data2D.delta(i,j) = Data2D.cpu_results(i,j) - &
                                   Data2D.gpu_results(i,j)
                              Data2D.status(i,j) = bres
                           end if
                        end do
                     end do
                  case default
                     call  print_non_fatal_error( ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPCompareReal4Data2D: Unrecognized switch argument!! ",  &
                                              __LINE__,__FILE__)
                     return
                  end select Comparison                            
     end subroutine FPCompareReal4Data2D
!===================================================================================================================!
     subroutine  InitReal4CompareData3D(Data3D,nx,ny,nz,errstate,iounit, &
                                        logging,verbose,append,fname)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: InitReal4CompareData3D
           use mod_print_error, only : handle_fatal_memory_error
           type(Real4CompareData3D_t),          intent(inout) :: Data3D
           integer(kind=i4),                  intent(in)    :: nx
           integer(kind=i4),                  intent(in)    :: ny
           integer(kind=i4),                  intent(in)    :: nz
           logical(kind=i1),                  intent(inout) :: errstate
           integer(kind=i4),                  intent(in)    :: iounit
           logical(kind=i4),                  intent(in)    :: logging
           logical(kind=i4),                  intent(in)    :: verbose
           logical(kind=i4),                  intent(in)    :: append
           character(len=*),                    intent(in)    :: fname
           ! LOcals
           character(len=256), automatic :: emsg
           integer(kind=i4), automatic :: aerr
           ! Exec code ....
           Data3D.cfname = "Not initialized yet."
           Data3D.gfname = "Not initialized yet."
           Data3D.cmpfname = "Set by comparison function."
           Data3D.nx = nx
           Data3D.ny = ny
           Data3D.nz = nz
         
           if(allocated(Data3D.gloop_idx)) then
              deallocate(Data3D.gloop_idx)
              allocate(Data3D.gloop_idx(Data3D.nx,Data3D.ny,Data3D.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Data3D.gloop_idx(Data3D.nx,Data3D.ny,Data3D.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Data3D.gloop_idx = -1
           if(allocated(Data3D.cpu_results)) then
              deallocate(Data3D.cpu_results)
              allocate(Data3D.cpu_results(Data3D.nx,Data3D.ny,Data3D.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Data3D.cpu_results(Data3D.nx,Data3D.ny,Data3D.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Data3D.cpu_results = TINY(1.0_sp)
           if(allocated(Data3D.gpu_results)) then
              deallocate(Data3D.gpu_results)
              allocate(Data3D.gpu_results(Data3D.nx,Data3D.ny,Data3D.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Data3D.gpu_results(Data3D.nx,Data3D.ny,Data3D.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Data3D.gpu_results = TINY(1.0_sp)
           if(allocated(Data3D.inequality)) then
              deallocate(Data3D.inequality)
              allocate(Data3D.inequality(Data3D.nx,Data3D.ny,Data3D.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Data3D.inequality(Data3D.nx,Data3D.ny,Data3D.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Data3D.inequality = TINY(1.0_sp)
           if(allocated(Data3D.delta)) then
              deallocate(Data3D.delta)
              allocate(Data3D.delta(Data3D.nx,Data3D.ny,Data3D.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Data3D.delta(Data3D.nx,Data3D.ny,Data3D.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Data3D.delta = TINY(1.0_sp)
           if(allocated(Data3D.status)) then
              deallocate(Data3D.status)
              allocate(Data3D.status(Data3D.nx,Data3D.ny,Data3D.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Data3D.status(Data3D.nx,Data3D.ny,Data3D.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Data3D.status = .false.
           errstate = .false.
           return
 9999      call handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitReal4CompareData3D -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitReal4CompareData3D -- Memory Allocation Failure !!", &
                              emsg,971)
     end subroutine InitReal4CompareData3D
!=================================================================================================================================!
     subroutine FPCompareReal4Data3D(Data3D,ulp,percent,n,cutoff,method)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: FPCompareReal4Data3D
           use mod_print_error, only : print_non_fatal_error
           type(Real4CompareData3D_t),    intent(inou) :: Data3D
           integer(kind=i4),            intent(in)   :: ulp
           real(kind=sp),                 intent(in)   :: percent
           integer(kind=i4),            intent(in)   :: n
           real(kind=sp),                 intent(in)   :: cutoff
           character(len=32),             intent(in)   :: method
           ! Locals
           integer(kind=i4), automatic :: i,k,j,idx
           logical(kind=i4), automatic :: bres
           ! Exec code .....
Comparison:   select case("method")
                case("EqualTo")
                   Data3D.cmpfname = "EqualTo_Real_Single"
                   idx = 0
                   bres = .false.
                   do j=1, Data3D.nz
                      do k=1, Data3D.ny
                         !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                         do i=1, Data3D.nx
                            bres = Data3D.cpu_results(i,k,j) .EqualTo. &
                                   Data3D.gpu_results(i,k,j)
                            if(bres) then
                               Data3D.delta(i,k,j) = Data3D.cpu_results(i,k,j) - &
                                    Data3D.gpu_results(i,k,j)
                               Data3D.status(i,k,j) = bres
                            else
                               idx = i+Data3D.nx*(k+Data3D.ny*j)
                               Data3D.gloop_idx(i,k,j) = idx
                               Data3D.inequality(i,k,j) = Data3D.gpu_results(i,k,j)
                               Data3D.delta(i,k,j) = Data3D.cpu_results(i,k,j) - &
                                    Data3D.gpu_results(i,k,j)
                               Data3D.status(i,k,j) = bres
                            end if
                         end do
                      end do
                   end do
                case("Compare_Float")
                   Data3D.cmpfname = "Compare_Real_Single"
                   idx = 0
                   bres = .false.
                   do j=1, Data3D.nz
                      do k=1, Data3D.ny
                         !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                         do i=1, Data3D.nx
                            bres = Compare_Float(Data3D.cpu_results(i,k,j), &
                                                 Data3D.gpu_results(i,k,j), &
                                                 ULP=ulp,                   &
                                                 PERCENT=percent)
                            if(bres) then
                               Data3D.delta(i,k,j) = Data3D.cpu_results(i,k,j) - &
                                    Data3D.gpu_results(i,k,j)
                               Data3D.status(i,k,j) = bres
                            else
                               idx = i+Data3D.nx*(k+Data3D.ny*j)
                               Data3D.gloop_idx(i,k,j) = idx
                               Data3D.inequality(i,k,j) = Data3D.gpu_results(i,k,j)
                               Data3D.delta(i,k,j) = Data3D.cpu_results(i,k,j) - &
                                    Data3D.gpu_results(i,k,j)
                               Data3D.status(i,k,j) = bres
                            end if
                         end do
                      end do
                   end do
                case("Compare_Within_Tolerance")
                   Data3D.cmpfname = "cwt_Real_Single"
                   idx = 0
                   bres = .false.
                   do j=1, Data3D.nz
                      do k=1, Data3D.ny
                         !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                         do i=1, Data3D.nx
                            bres = Compare_Within_Tolerance(Data3D.cpu_results(i,k,j), &
                                                   Data3D.gpu_results(i,k,j), &
                                                   n, &
                                                   cutoff)
                            if(bres) then
                                 Data3D.delta(i,k,j) = Data3D.cpu_results(i,k,j) - &
                                    Data3D.gpu_results(i,k,j)
                               Data3D.status(i,k,j) = bres
                            else
                               idx = i+Data3D.nx*(k+Data3D.ny*j)
                               Data3D.gloop_idx(i,k,j) = idx
                               Data3D.inequality(i,k,j) = Data3D.gpu_results(i,k,j)
                               Data3D.delta(i,k,j) = Data3D.cpu_results(i,k,j) - &
                                    Data3D.gpu_results(i,k,j)
                               Data3D.status(i,k,j) = bres
                            end if
                         end do
                      end do
                   end do
                case default
                      call  print_non_fatal_error( ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPCompareReal4Data3D: Unrecognized switch argument!! ",  &
                                              __LINE__,__FILE__)
                                              return
               end select Comparison                               
     end subroutine FPCompareReal4Data3D
!================================================================================================================!
     subroutine InitReal8CompareData1D(Datum,nx,errstate,iounit,logging,verbose, &
                                       append,fname)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: InitReal8CompareData1D
           use mod_print_error, only : handle_fatal_memory_error
           type(Real8CompareData1D_t),              intent(inout) :: Datum
           integer(kind=i4),                      intent(in)    :: nx
           logical(kind=i1),                      intent(inout) :: errstate
           integer(kind=i4),                      intent(in)    :: iounit
           logical(kind=i4),                      intent(in)    :: logging
           logical(kind=i4),                      intent(in)    :: verbose
           logical(kind=i4),                      intent(in)    :: append
           character(len=*),                        intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=i4), automatic :: aerr
           ! EXec coce ...
           Datum.cfname = "Not initialized yet."
           Datum.gfname = "Not initialized yet."
           Datum.cmpfname = "Set by comparison function."
           Datum.nx = nx
           if(allocated(Datum.gloop_idx)) then
              deallocate(Datum.gloop_idx)
              allocate(Datum.gloop_idx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.gloop_idx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.gloop_idx = -1
           if(allocated(Datum.cpu_results)) then
              deallocate(Datum.cpu_results)
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.cpu_results = TINY(1.0_dp)
           if(allocated(Datum.gpu_results)) then
              deallocate(Datum.gpu_results)
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.gpu_results = TINY(1.0_dp)
           if(allocated(Datum.inequality)) then
              deallocate(Datum.inequality)
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.inequality = TINY(1.0_dp)
           if(allocated(Datum.delta)) then
              deallocate(Datum.delta)
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.delta = TINY(1.0_dp)
           if(allocated(Datum.status)) then
              deallocate(Datum.status)
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.status = .false.
           errstate = .false.
           return
9999       call handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitReal8CompareData1D -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitReal8CompareData1D -- Memory Allocation Failure !!", &
                              emsg,1175)
         end subroutine InitReal8CompareData1D
!================================================================================================================!
    subroutine FPCompareReal8Data1D(Datum,ulp,percent,n,cutoff,method)
      !DIR$ ATTRIBUTES CODE_ALIGN:32 :: FPCompareReal8Data1D
           use mod_print_error, only : print_non_fatal_error
           type(Real8CompareData1D_t),       intent(inout) :: Datum
           integer(kind=i4),               intent(in)    :: ulp
           real(kind=dp),                    intent(in)    :: percent
           integer(kind=i4),               intent(in)    :: n
           real(kind=dp),                    intent(in)    :: cutoff
           character(len=*),                 intent(in)    :: method
           ! Locals
           logical(kind=i4), automatic :: bres
           integer(kind=i4), automatic :: i
           ! Exec code ....
Comparison: select case("method")
                  case("EqualTo")
                  Datum.cmpfname = "EqualTo_Real_Double"
                  bres = .false.
                  !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                  do i=1, Datum.nx
                     bres = Datum.cpu_results(i) .EqualTo. &
                          Datum.gpu_results(i)
                     if(bres) then
                         Datum.delta(i) = Datum.cpu_results(i) - &
                                          Datum.gpu_results(i)
                         Datum.status(i) = bres
                     else
                            
                             Datum.gloop_idx(i) = i
                             Datum.inequality(i) = Datum.gpu_results(i)
                             Datum.delta(i) =  Datum.cpu_results(i) - &
                                                  Datum.gpu_results(i)
                             Datum.status(i) = bres
                     end if
                  end do
               case("Compare_Float")
                  Datum.cmpfname = "Compare_Real_Double"
                  bres = .false.
                  !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                  do i=1, Datum.nx
                     bres = Compare_Float(Datum.cpu_results(i), &
                                           Datum.gpu_results(i), &
                                           ulp, &
                                           percent)
                     if(bres) then
                             Datum.delta(i) = Datum.cpu_results(i) - &
                                          Datum.gpu_results(i)
                             Datum.status(i) = bres
                     else
                            
                             Datum.gloop_idx(i) = i
                             Datum.inequality(i) = Datum.gpu_results(i)
                             Datum.delta(i) =  Datum.cpu_results(i) - &
                                                  Datum.gpu_results(i)
                             Datum.status(i) = bres
                     end if
                   end do
                case("Compare_Within_Tolerance")
                   Datum.cmpfname = "cwt_Real_Double"
                   bres = .false.
                   !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                   do i=1, Datum.nx
                      bres = Compare_Within_Tolerance(Datum.cpu_results(i), &
                                             Datum.gpu_results(i), &
                                             n, &
                                             cutoff)
                      if(bres) then
                             Datum.delta(i) = Datum.cpu_results(i) - &
                                          Datum.gpu_results(i)
                             Datum.status(i) = bres
                      else
                            
                             Datum.gloop_idx(i) = i
                             Datum.inequality(i) = Datum.gpu_results(i)
                             Datum.delta(i) =  Datum.cpu_results(i) - &
                                                  Datum.gpu_results(i)
                             Datum.status(i) = bres
                      end if
                   end do
                case default
                     call  print_non_fatal_error( ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPCompareReal8Data1D: Unrecognized switch argument!! ",  &
                                              __LINE__,__FILE__)
                                              return
                end select Comparison
                      
    end subroutine FPCompareReal8Data1D
!================================================================================================================!
    subroutine InitReal8CompareData2D(Datum,nx,ny,errstate,iounit,logging,verbose, &
                                      append,fname)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: InitReal8CompareData2D
           use mod_print_error, only : handle_fatal_memory_error
           type(Real8CompareData2D),        intent(inout) :: Datum
           integer(kind=i4),              intent(in)    :: nx
           integer(kind=i4),              intent(in)    :: ny
           logical(kind=i1),              intent(inout) :: errstate
           integer(kind=i4),              intent(in)    :: iounit
           logical(kind=i4),              intent(in)    :: logging
           logical(kind=i4),              intent(in)    :: verbose
           logical(kind=i4),              intent(in)    :: append
           character(len=*),                intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=i4), automatic :: aerr
           ! Exec code ....
           Datum.cfname = "Not initialized yet."
           Datum.gfname = "Not initialized yet."
           Datum.cmpfname = "Set by comparison function."
           Datum.nx = nx
           Datum.ny = ny
           if(allocated(Datum.gloop_idx)) then
              deallocate(Datum.gloop_idx)
              allocate(Datum.gloop_idx(Datum.nx,Datum.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.gloop_idx(Datum.nx,Datum.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.gloop_idx = -1
           if(allocated(Datum.cpu_results)) then
              deallocate(Datum.cpu_results)
              allocate(Datum.cpu_results(Datum.nx,Datum.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.cpu_results(Datum.nx,Datum.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.cpu_results = TINY(1.0_dp)
           if(allocated(Datum.gpu_results)) then
              deallocate(Datum.gpu_results)
              allocate(Datum.gpu_results(Datum.nx,Datum.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.gpu_results(Datum.nx,Datum.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.gpu_results = TINY(1.0_dp)
           if(allocated(Datum.inequality)) then
              deallocate(Datum.inequality)
              allocate(Datum.inequality(Datum.nx,Datum.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.inequality(Datum.nx,Datum.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.inequality = TINY(1.0_dp)
           if(allocated(Datum.delta)) then
              deallocate(Datum.delta)
              allocate(Datum.delta(Datum.nx,Datum.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.delta(Datum.nx,Datum.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.delta = TINY(1.0_dp)
           if(allocated(Datum.status)) then
              deallocate(Datum.status)
              allocate(Datum.status(Datum.nx,Datum.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.status(Datum.nx,Datum.ny), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.status = .false.
           errstate = .false.
           return
9999       call handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitReal8CompareData2D -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitReal8CompareData2D -- Memory Allocation Failure !!", &
                              emsg,1358)
     end subroutine InitReal8CompareData2D
!================================================================================================================!
     subroutine FPCompareReal8Data2D(Datum,ulp,percent,n,cutoff,method)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: FPCompareReal8Data2D
           use mod_print_error, only : print_non_fatal_error
           type(Real8CompareData2D_t),          intent(inout) :: Datum
           integer(kind=i4),                  intent(in)    :: ulp
           real(kind=dp),                       intent(in)    :: percent
           integer(kind=i4),                  intent(in)    :: n
           real(kind=dp),                       intent(in)    :: cutoff
           character(len=*),                    intent(in)    :: method
           ! LOcals
           logical(kind=i4), automatic :: bres
           integer(kind=i4), automatic :: i,j,idx
           ! EXec code .....
Comparison:    select case("method")
                   case("EqualTo")
                      bres = .false.
                      Datum.cmpfname = "EqualTo_Real_Double"
                      idx = 0
                      do j=1, Datum.ny
                         !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                         do i=1, Datum.nx
                            bres = Datum.cpu_results(i,j).EqualTo. &
                                 Datum.gpu_results(i,j)
                            if(bres) then
                                 Datum.delta(i,j) = Datum.cpu_results(i,j)- &
                                                  Datum.gpu_results(i,j)
                                 Datum.status(i,j) = bres
                            else
                                 idx = i*nx+j
                             
                                 Datum.gloop_idx(i,j) = idx
                                 Datum.inequality(i,j) = Datum.gpu_results(i,j)
                                 Datum.delta(i,j) = Datum.cpu_results(i,j) - &
                                                    Datum.gpu_results(i,j)
                                 Datum.status(i,j) = bres
                           end if
                        end do
                     end do
                  case("Compare_Float")
                     bres = .false.
                     Datum.cmpfname = "Compare_Real_Double"
                     idx = 0
                     do j=1, Datum.ny
                        !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                        do i=1, Datum.ny
                           bres = Compare_Float(Datum.cpu_results(i,j), &
                                                 Datum.gpu_results(i,j), &
                                                 ulp, &
                                                 percent)
                           if(bres) then
                                 Datum.delta(i,j) = Datum.cpu_results(i,j)- &
                                                    Datum.gpu_results(i,j)
                                 Datum.status(i,j) = bres
                            else
                                 idx = i*nx+j
                             
                                 Datum.gloop_idx(i,j) = idx
                                 Datum.inequality(i,j) = Datum.gpu_results(i,j)
                                 Datum.delta(i,j) = Datum.cpu_results(i,j) - &
                                                    Datum.gpu_results(i,j)
                                 Datum.status(i,j) = bres
                           end if
                        end do
                     end do
                  case("Compare_Within_Tolerance")
                     bres = .false.
                     Datum.cmpfname = "cwt_Real_Double"
                     idx = 0
                     do j=1, Datum.ny
                        !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                        do i=1, Datum.nx
                           bres = Compare_Within_Tolerance(Datum.cpu_results(i,j), &
                                                  Datum.gpu_results(i,j), &
                                                  n, &
                                                  cutoff)
                           if(bres) then
                               if(bres) then
                                 Datum.delta(i,j) = Datum.cpu_results(i,j)- &
                                                    Datum.gpu_results(i,j)
                                 Datum.status(i,j) = bres
                            else
                                 idx = i*nx+j
                             
                                 Datum.gloop_idx(i,j) = idx
                                 Datum.inequality(i,j) = Datum.gpu_results(i,j)
                                 Datum.delta(i,j) = Datum.cpu_results(i,j) - &
                                                    Datum.gpu_results(i,j)
                                 Datum.status(i,j) = bres
                           end if
                        end do
                     end do
                  case default
                     call   print_non_fatal_error( ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPCompareReal8Data2D: Unrecognized switch argument!! ",  &
                                              __LINE__,__FILE__)
                     return
                  end select Comparison
     end subroutine FPCompareReal8Data2D
!================================================================================================================!
     subroutine InitReal8CompareData3D(Datum,nx,ny,nz,errstate,iounit,logging,verbose, &
          append,fname)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: InitReal8CompareData3D
           use mod_print_error, only : handle_fatal_memory_error
           type(Real8CompareData3D_t),           intent(inout) :: Datum
           integer(kind=i4),                   intent(in)    :: nx
           integer(kind=i4),                   intent(in)    :: ny
           integer(kind=i4),                   intent(in)    :: nz
           logical(kind=i1),                   intent(in)    :: errstate
           integer(kind=i4),                   intent(in)    :: iounit
           logical(kind=i4),                   intent(in)    :: logging
           logical(kind=i4),                   intent(in)    :: verbose
           logical(kind=i4),                   intent(in)    :: append
           character(len=*),                     intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=i4), automatic :: aerr
           ! Exec code ....
           Datum.cfname = "Not initiliazed yet."
           Datum.gfname = "Not initialized yet."
           Datum.cmpfname = "Initialized by compare function."
           Datum.nx = nx
           Datum.ny = ny
           Datum.nz = nz
           if(allocated(Datum.gloop_idx)) then
              deallocate(Datum.gloop_idx)
              allocate(Datum.gloop_idx(Datum.nx,Datum.ny,Datum.nz), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
               allocate(Datum.gloop_idx(Datum.nx,Datum.ny,Datum.nz), &
                   STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
            end if
            Datum.gloop_idx = -1
            if(allocated(Datum.cpu_results)) then
               deallocate(Datum.cpu_results)
               allocate(Datum.cpu_results(Datum.nx,Datum.ny,Datum.nz), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
            else
               allocate(Datum.cpu_results(Datum.nx,Datum.ny,Datum.nz), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
            end if
            Datum.cpu_results = TINY(1.0_dp)
            if(allocated(Datum.gpu_results)) then
               deallocate(Datum.gpu_results)
               allocate(Datum.gpu_results(Datum.nx,Datum.ny,Datum.nz), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
            else
               allocate(Datum.gpu_results(Datum.nx,Datum.ny,Datum.nz), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
            end if
            Datum.gpu_results = TINY(1.0_dp)
            if(allocated(Datum.inequality)) then
               deallocate(Datum.inequality)
               allocate(Datum.inequality(Datum.nx,Datum.ny,Datum.nz), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
            else
               allocate(Datum.inequality(Datum.nx,Datum.ny,Datum.nz), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
            end if
            Datum.inequality = TINY(1.0_dp)
            if(allocated(Datum.delta)) then
               deallocate(Datum.delta)
               allocate(Datum.delta(Datum.nx,Datum.ny,Datum.nz), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
            else
               allocate(Datum.delta(Datum.nx,Datum.ny,Datum.nz), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
            end if
            Datum.delta = TINY(1.0_dp)
            if(allocated(Datum.status)) then
               deallocate(Datum.status)
               allocate(Datum.status(Datum.nx,Datum.ny,Datum.nz), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
            else
               allocate(Datum.status(Datum.nx,Datum.ny,Datum.nz), &
                    STAT=aerr,ERRMSG=emsg)
               if(aerr /= 0) goto 9999
            end if
            Datum.status = .false.
            errstate = .false.
            return
9999        call  handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitReal8CompareData3D -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitReal8CompareData3D -- Memory Allocation Failure !!", &
                              emsg,1555)
    end subroutine InitReal8CompareData3D
          !================================================================================================================!
    subroutine FPCompareReal8Data3D(Datum,ulp,percent,n,cutoff,method)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: FPCompareReal8Data3D
           use mod_print_error, only : print_non_fatal_error
           type(Real8CompareData3D_t),       intent(inout) :: Datum
           integer(kind=i4),               intent(in)    :: ulp
           real(kind=dp),                    intent(in)    :: percent
           integer(kind=i4),               intent(in)    :: n
           real(kind=dp),                    intent(in)    :: cutoff
           character(len=*),                 intent(in)    :: method
           ! Locals
           logical(kind=i4), automatic :: bres
           integer(kind=i4), automatic :: i,j,k,idx
           ! Exec code ...
Comparison:  select case("method")
                case("EqualTo")
                   bres = .false.
                   Datum.cmpfname = "EqualTo_Real_Double"
                   idx = 0
                   do j=1, Datum.nz
                      do k=1, Datum.ny
                         !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                         do i=1, Datum.nx
                            bres = Datum.cpu_results(i,k,j) .EqualTo. &
                                   Datum.gpu_results(i,k,j)
                            if(bres) then
                                   
                               Datum.delta(i,k,j) = Datum.cpu_results(i,k,j) - &
                                                    Datum.gpu_results(i,k,j)
                               Datum.status(i,k,j) = bres
                            else
                               idx = i+Datum.nx*(k+Datum.ny*j)
                               Datum.gloop_idx(i,k,j) = idx
                               Datum.inequality(i,k,j) = Datum.gpu_results(i,k,j)
                               Datum.delta(i,k,j) = Datum.cpu_results(i,k,j) - &
                                                    Datum.gpu_results(i,k,j)
                               Datum.status(i,k,j) = bres
                            end if
                         end do
                      end do
                   end do
                case("Compare_Float")
                   bres = .false.
                   Datum.cmpfname = "Compare_Real_Double"
                   idx = 0
                   do j=1, Datum.nz
                      do k=1, Datum.ny
                         !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                         do i=1, Datum.nz
                            bres = Compare_Float(Datum.cpu_results(i,k,j), &
                                                  Datum.gpu_results(i,k,j), &
                                                  ulp, &
                                                  percent)
                            if(bres) then
                               Datum.delta(i,k,j) = Datum.cpu_results(i,k,j) - &
                                                    Datum.gpu_results(i,k,j)
                               Datum.status(i,k,j) = bres
                            else
                               idx = i+Datum.nx*(k+Datum.ny*j)
                               Datum.gloop_idx(i,k,j) = idx
                               Datum.inequality(i,k,j) = Datum.gpu_results(i,k,j)
                               Datum.delta(i,k,j) = Datum.cpu_results(i,k,j) - &
                                                    Datum.gpu_results(i,k,j)
                               Datum.status(i,k,j) = bres
                            end if
                         end do
                      end do
                   end do
                case("Compare_Within_Tolerance")
                   bres = .false.
                   Datum.cmpfname = "cwt_Real_DOuble"
                   idx = 0
                   do j=1, Datum.nz
                      do k=1, Datum.ny
                         !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                         do i=1, Datum.nx
                            bres = Compare_Within_Tolerance(Datum.cpu_results(i,k,j), &
                                                   Datum.gpu_results(i,k,j), &
                                                   n, &
                                                   cutoff)
                            if(bres) then
                               Datum.delta(i,k,j) = Datum.cpu_results(i,k,j) - &
                                                    Datum.gpu_results(i,k,j)
                               Datum.status(i,k,j) = bres
                            else
                               idx = i+Datum.nx*(k+Datum.ny*j)
                               Datum.gloop_idx(i,k,j) = idx
                               Datum.inequality(i,k,j) = Datum.gpu_results(i,k,j)
                               Datum.delta(i,k,j) = Datum.cpu_results(i,k,j) - &
                                                    Datum.gpu_results(i,k,j)
                               Datum.status(i,k,j) = bres
                            end if
                         end do
                      end do
                   end do
                case default
                   call  print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPCompareReal8Data3D: Unrecognized switch argument!! ",  &
                                              __LINE__,__FILE__)
                   return
                end select Comparison
     end subroutine FPCompareReal8Data3D
!================================================================================================================!
     subroutine InitComplex4CompareData(Datum,nx,errstate,iounit,logging, &
          verbose,append,fname)
!DIR$ ATTRIBUTES ALIGN:32 :: InitComplex4CompareData
           use mod_print_error, only : handle_fatal_memory_error
           type(Complex4CompareData_t),     intent(inout) :: Datum
           integer(kind=i4),              intent(in)    :: nx
           logical(kind=i1),              intent(inout) :: errstate
           integer(kinbd=i4),             intent(in)    :: iounit
           logical(kind=i4),              intent(in)    :: logging
           logical(kind=i4),              intent(in)    :: verbose
           logical(kind=i4),              intent(in)    :: append
           character(len=*),                intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=i4), automatic :: aerr
           ! Exec code .....
           Datum.cfname = "Not initiliazed yet."
           Datum.gfname = "Not initialized yet."
           Datum.cmpfname = "Initialized by compare function."
           Datum.nx = nx
           if(allocated(Datum.gloop_idx)) then
              deallocate(Datum.gloop_idx)
              allocate(Datum.gloop_idx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.gloop_idx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.gloop_idx = -1
           if(allocated(Datum.cpu_results)) then
              deallocate(Datum.cpu_results)
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.cpu_results = CTINYR4
           if(allocated(Datum.gpu_results)) then
              deallocate(Datum.gpu_results)
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.gpu_results = CTINYR4
           if(allocated(Datum.inequality)) then
              deallocate(Datum.inequality)
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.inequality = CTINYR4
           if(allocated(Datum.delta)) then
              deallocate(Datum.delta)
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.delta = CTINYR4
           if(allocated(Datum.status)) then
              deallocate(Datum.status)
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.status = .false.
           errstate = .false.
           return
9999       call  handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitComplex4CompareData -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitComplex4CompareData -- Memory Allocation Failure !!", &
                              emsg,1708)
     end subroutine InitComplex4CompareData
!================================================================================================================!
     subroutine FPComplex4CompareData(Datum,ulp,percent,n,cutoff,method)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: FPComplex4CompareData
           use mod_print_error, only : print_non_fatal_error
           type(Complex4CompareData_t),         intent(inout) :: Datum
           integer(kind=i4),                  intent(in)    :: ulp
           real(kind=sp),                       intent(in)    :: percent
           integer(kind=i4),                  intent(in)    :: n
           real(kind=sp),                       intent(in)    :: cutoff
           character(len=32),                   intent(in)    :: method
           ! Locals
           logical(kind=i4), automatic :: bres
           integer(kind=i4), automatic :: i
           ! EXec code .....
Comparison: select case("method")
                case("EqualTo")
                   bres = .false.
                   Datum.cmpfname = "EqualTo_Complex_Single"
                   do i=1, Datum.nx
                      bres = Datum.cpu_results(i) .EqualTo. &
                           Datum.gpu_results(i)
                      if(bres) then
                         
                         Datum.delta(i) = Datum.cpu_results(i) - &
                                          Datum.gpu_results(i)
                         Datum.status(i) = bres
                     else
                            
                         Datum.gloop_idx(i) = i
                         Datum.inequality(i) = Datum.gpu_results(i)
                         Datum.delta(i) =  Datum.cpu_results(i) - &
                                                  Datum.gpu_results(i)
                         Datum.status(i) = bres
                     end if
                  end do
               case("Compare_Float")
                  bres = .false.
                  Datum.cmpfname = "Compare_Complex_Single"
                  do i=1, Datum.nx
                     bres = Compare_Float(Datum.cpu_results(i), &
                                          Datum.gpu_results(i), &
                                          ulp, &
                                          percent)
                     if(bres) then
                         Datum.delta(i) = Datum.cpu_results(i) - &
                                          Datum.gpu_results(i)
                         Datum.status(i) = bres
                     else
                            
                         Datum.gloop_idx(i) = i
                         Datum.inequality(i) = Datum.gpu_results(i)
                         Datum.delta(i) =  Datum.cpu_results(i) - &
                                                  Datum.gpu_results(i)
                         Datum.status(i) = bres
                     end if
                  end do
               case("Compare_Within_Tolerance")
                  bres = .false.
                  Datum.cmpfname = "cwt_Complex_Single"
                  do i=1, Datum.nx
                     bres = cwt_Complex_Single(Datum.cpu_results(i), &
                                               Datum.gpu_results(i), &
                                               n, &
                                               cutoff)
                     if(bres) then
                         Datum.delta(i) = Datum.cpu_results(i) - &
                                          Datum.gpu_results(i)
                         Datum.status(i) = bres
                     else
                            
                         Datum.gloop_idx(i) = i
                         Datum.inequality(i) = Datum.gpu_results(i)
                         Datum.delta(i) =  Datum.cpu_results(i) - &
                                                  Datum.gpu_results(i)
                         Datum.status(i) = bres
                     end if
                  end do
               case default
                  call  print_non_fatal_error(" ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPComplex4CompareData: Unrecognized switch argument!! ",  &
                                              __LINE__,__FILE__)
                  return
              end select Comparison
     end subroutine FPComplex4CompareData
!================================================================================================================!     
    subroutine InitXMM2r8Data(Datum,nx,errstate,iounit,logging,verbose,  &
                              append, fname   )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: InitXMM2r8Data
           use mod_print_error,  only : handle_fatal_memory_error
           type(XMM2r8CompareData_t),        intent(inout) :: Datum
           integer(kind=i4),               intent(in)    :: nx
           logical(kind=i1),               intent(inout) :: errstate
           integer(kind=i4),               intent(in)    :: iounit
           logical(kind=i4),               intent(in)    :: logging
           logical(kind=i4),               intent(in)    :: verbose
           logical(kind=i4),               intent(in)    :: append
           character(len=*),                 intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=i4), automatic :: aerr
           ! Exec code ....
           Datum.cfname = "Not initiliazed yet."
           Datum.gfname = "Not initialized yet."
           Datum.cmpfname = "Initialized by compare function."
           Datum.nx = nx
           if(allocated(Datum.vidx)) then
              deallocate(Datum.vidx)
              allocate(Datum.vidx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.vidx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.vidx = -1
           if(allocated(Datum.cpu_results)) then
              deallocate(Datum.cpu_results)
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.cpu_results = XMM2r8_InitVal
           if(allocated(Datum.gpu_results)) then
              deallocate(Datum.gpu_results)
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.gpu_results = XMM2r8_InitVal
           if(allocated(Datum.inequality)) then
              deallocate(Datum.inequality)
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.inequality = XMM2r8_InitVal
           if(allocated(Datum.delta)) then
              deallocate(Datum.delta)
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.delta = XMM2r8_InitVal
           if(allocated(Datum.status)) then
              deallocate(Datum.status)
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.status = Mask2_InitVal
           errstate = .false.
           return
9999       call handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitXMM2r8Data -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitXMM2r8Data -- Memory Allocation Failure !!", &
                              emsg,1218)
     end subroutine InitXMM2r8Data
!=====================================================================================================!         
     subroutine FPCompareXMM2r8Data(Datum,n,cutoff,method)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: FPCompareXMM2r8Data
       type(XMM2r8CompareData_t),        intent(inout) :: Datum
       type(XMM2i4_t),                   intent(in)    :: n
       type(XMM2r8_t),                   intent(in)    :: cutoff
       character(len=32),                intent(in)    :: method
       ! Locals
!DIR$  ATTRIBUTES ALIGN : 8 :: bres
       type(Mask2_t), automatic :: bres
       integer(kind=i4), automatic :: i
       ! Exec code ....
Comparison: select case("method")
                case("vectype_equalto_vectype")
                  Datum.cmpfname = "xmm2r8_equalto_xmm2r8"
                  bres.m = .false.
                  !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                  do i=1,  Datum.nx
                     bres = xmm2r8_equalto_xmm2r8(Datum.cpu_results(i), &
                                                  Datum.gpu_results(i)  )
                     if(ALL(bres.m)) then
                        Datum.delta(i) = Datum.cpu_results(i) - &
                             Datum.gpu_results(i)
                        Datum.status(i) = bres
                     else
                        Datum.vidx(i) = i
                        Datum.inequality(i) = Datum.gpu_results(i)
                        Datum.delta(i)      = Datum.cpu_results(i) - &
                             Datum.gpu_results(i)
                        Datum.status(i) = bres
                     end if
                  end do
               case("xmm2r8_cwt_xmm2r8")
                  Datum.cmpfname = "xmm2r8_cwt_xmm2r8"
                  bres.m = .false.
                  !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                  do i=1, Datum.nx
                     bres = xmm2r8_cwt_xmm2r8(Datum.cpu_results(i),  &
                                              Datum.gpu_results(i),  &
                                              n,                     &
                                              cutoff )
                     if(ALL(bres.m)) then
                        Datum.delta(i) = Datum.cpu_results(i) - &
                             Datum.gpu_results(i)
                        Datum.status(i) = bres
                     else
                        Datum.vidx(i) = i
                        Datum.inequality(i) = Datum.gpu_results(i)
                        Datum.delta(i)      = Datum.cpu_results(i) - &
                             Datum.gpu_results(i)
                        Datum.status(i) = bres
                     end if
                  end do
               case default
                     call  print_non_fatal_error( ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPCompaXMM2r8Data: Unrecognized switch argument!! ",  &
                                              __LINE__,__FILE__)
                     return
               end select Comparison
     end subroutine FPCompareXMM2r8Data
!============================================================================================================!       
     subroutine InitXMM4r4Data(Datum,nx,errstate,iounit,logging,verbose, &
                              append, fname)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: InitXMM4r4Data
           type(XMM4r4CompareData_t),             intent(inout) :: Datum
           integer(kind=i4),                    intent(in) :: nx
           logical(kind=i1),                    intent(inout) :: errstate
           integer(kind=i4),                    intent(in)    :: iounit
           logical(kind=i4),                    intent(in)    :: logging
           logical(kind=i4),                    intent(in)    :: verbose
           logical(kind=i4),                    intent(in)    :: append
           character(len=*),                      intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=int4), automatic :: aerr
           ! EXec code ...
           Datum.cfname = "Not initiliazed yet."
           Datum.gfname = "Not initialized yet."
           Datum.cmpfname = "Initialized by compare function."
           Datum.nx = nx
           if(allocated(Datum.vidx)) then
              deallocate(Datum.vidx)
              allocate(Datum.vidx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.vidx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.vidx = -1
           if(allocated(Datum.cpu_results)) then
              deallocate(Datum.cpu_results)
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.cpu_results = XMM4r4_InitVal
           if(allocated(Datum.gpu_results)) then
              deallocate(Datum.gpu_results)
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.gpu_results = XMM4r4_InitVal
           if(allocated(Datum.inequality)) then
              deallocate(Datum.inequality)
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.inequality = XMM4r4_InitVal
           if(allocated(Datum.delta)) then
              deallocate(Datum.delta)
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.delta = XMM4r4_InitVal
           if(allocated(Datum.status)) then
              deallocate(Datum.status)
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.status = Mask2_InitVal
           errstate = .false.
           return
9999       call handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitXMM4r4Data -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitXMM4r4Data -- Memory Allocation Failure !!", &
                              emsg,1389)
     end subroutine InitXMM4r4Data
!===================================================================================================================!
     subroutine FPCompareXMM4r4Data(Datum,n,cutoff,method)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: FPCompareXMM4r4Data
           type(XMM4r4CompareData_t),        intent(inout) :: Datum
           type(XMM4i4_t),                   intent(in)    :: n
           type(XMM4r4_t),                   intent(in)    :: cutoff
           character(len=32),                intent(in)    :: method
           ! Locals
!DIR$      ATTRIBUTES ALIGN : 16 :: bres
           type(Mask4_t), automatic :: bres
           integer(kind=i4), automatic :: i
           ! Exec code ....
Comparison:   select case("method")
                 case("vectype_equalto_vectype")
                    Datum.cmpfname = "xmm4r4_equalto_xmm4r4"
                    bres.m = .false.
                    !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                    do i=1, Datum.nx
                       bres = xmm4r4_equalto_xmm4r4(Datum.cpu_results(i),  &
                                                    Datum.gpu_results(i) )
                       if(ALL(bres.m)) then
                           Datum.delta(i) = Datum.cpu_results(i) - &
                             Datum.gpu_results(i)
                           Datum.status(i) = bres
                       else
                           Datum.vidx(i) = i
                           Datum.inequality(i) = Datum.gpu_results(i)
                           Datum.delta(i)      = Datum.cpu_results(i) - &
                             Datum.gpu_results(i)
                           Datum.status(i) = bres
                        end if
                    end do
                 case("xmm4r4_cwt_xmm4r4")
                    Datum.cmpfname = "xmm4r4_cwt_xmm4r4"
                    bres.m = .false.
                    !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                    do i=1, Datum.nx
                       bres = xmm4r4_cwt_xmm4r4(Datum.cpu_results(i), &
                                                Datum.gpu_results(i), &
                                                n, &
                                                cutoff )
                       if(ALL(bres.m)) then
                            Datum.delta(i) = Datum.cpu_results(i) - &
                             Datum.gpu_results(i)
                           Datum.status(i) = bres
                       else
                           Datum.vidx(i) = i
                           Datum.inequality(i) = Datum.gpu_results(i)
                           Datum.delta(i)      = Datum.cpu_results(i) - &
                             Datum.gpu_results(i)
                           Datum.status(i) = bres
                        end if
                     end do
                  case default
                     call  print_non_fatal_error( ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPCompareXMM4r4Data: Unrecognized switch argument!! ",  &
                                               __LINE__,__FILE__)
                     return
                 end select Comparison
     end subroutine FPCompareXMM4r4Data
!==================================================================================================!
     subroutine InitYMM4r8Data(Datum,nx,errstate,iounit,logging,verbose, &
                               append,fname )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: InitYMM4r8Data
           type(YMM4r8CompareData_t),            intent(inout) :: Datum
           integer(kind=i4),                   intent(in)    :: nx
           logical(kind=i1),                   intent(inout) :: errstate
           integer(kind=i4),                   intent(in)    :: iounit
           logical(kind=i4),                   intent(in)    :: logging
           logical(kind=i4),                   intent(in)    :: verbose
           logical(kind=i4),                   intent(in)    :: append
           character(len=*),                     intent(in)    :: fname
           ! LOcals
           character(len=256), automatic ::  emsg
           integer(kind=int4), automatic :: aerr
           ! Exec code ....
           Datum.cfname = "Not initiliazed yet."
           Datum.gfname = "Not initialized yet."
           Datum.cmpfname = "Initialized by compare function."
           Datum.nx = nx
           if(allocated(Datum.vidx)) then
              deallocate(Datum.vidx)
              allocate(Datum.vidx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.vidx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.vidx = -1
           if(allocated(Datum.cpu_results)) then
              deallocate(Datum.cpu_results)
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.cpu_results = YMM4r8_InitVal
           if(allocated(Datum.gpu_results)) then
              deallocate(Datum.gpu_results)
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.gpu_results = YMM4r8_InitVal
           if(allocated(Datum.inequality)) then
              deallocate(Datum.inequality)
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.inequality = YMM4r8_InitVal
           if(allocated(Datum.delta)) then
              deallocate(Datum.delta)
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.delta = YMM4r8_InitVal
           if(allocated(Datum.status)) then
              deallocate(Datum.status)
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.status = Mask4_Initval
           errstate = .false.
           return
9999       call  handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitYMM4r8Data -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitYMM4r8Data -- Memory Allocation Failure !!", &
                              emsg,1541)
     end subroutine InitYMM4r8Data
!=====================================================================================================================!
     subroutine FPCompareYMM4r8Data(Datum,n,cutoff,method)
!DIR$  ATTRIBUTES CODE_ALIGN:32 :: FPCompareYMM4r8Data
           type(YMM4r8CompareData_t),       intent(inout) :: Datum
           type(XMM4i4_t),                  intent(in)    :: n
           type(YMM4r8_t),                  intent(in)    :: cutoff
           character(len=32),               intent(in)    :: method
           ! Locals
!DIR$ ATTRIBUTES ALIGN : 16 :: bres
           type(Mask4_t), automatic :: bres
           integer(kind=i4), automatic :: i
Comparison: select case("method")
                  case("vectype_equalto_vectype")
                     Datum.cmpfname = "ymm4r8_equalto_ymm4r8"
                     bres.m = .false.
                     !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                     do i=1, Datum.nx
                        bres = ymm4r8_equalto_ymm4r8(Datum.cpu_results(i), &
                                                     Datum.gpu_results(i) )
                       if(ALL(bres.m)) then
                           Datum.delta(i) = Datum.cpu_results(i) - &
                                            Datum.gpu_results(i)
                           Datum.status(i) = bres
                       else
                           Datum.vidx(i) = i
                           Datum.inequality(i) = Datum.gpu_results(i)
                           Datum.delta(i)      = Datum.cpu_results(i) - &
                                                 Datum.gpu_results(i)
                           Datum.status(i) = bres
                        end if
                     end do
                  case("ymm4r8_cwt_ymm4r8")
                     Datum.cmpfname = "ymm4r8_cwt_ymm4r8"
                     bres.m = .false.
                     !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                     do i=1, Datum.nx
                         bres = ymm4r8_cwt_ymm4r8(Datum.cpu_results(i), &
                                                 Datum.gpu_results(i), &
                                                 n, &
                                                 cutoff )
                       if(ALL(bres.m)) then
                            Datum.delta(i) = Datum.cpu_results(i) - &
                                             Datum.gpu_results(i)
                           Datum.status(i) = bres
                       else
                           Datum.vidx(i) = i
                           Datum.inequality(i) = Datum.gpu_results(i)
                           Datum.delta(i)      = Datum.cpu_results(i) - &
                             Datum.gpu_results(i)
                           Datum.status(i) = bres
                        end if
                     end do
                  case default
                      call  print_non_fatal_error( ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPCompareYMM4r8Data: Unrecognized switch argument!! ",  &
                                               __LINE__,__FILE__)
                      return
                  end select Comparison
     end subroutine FPCompareYMM4r8Data
!===================================================================================================================!
     subroutine InitYMM8r4Data(Datum,nx,errstate,iounit,logging,verbose,  &
                               append,fname )
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: InitYMM8r4Data
           type(YMM8r4CompareData_t),           intent(inout) :: Datum
           integer(kind=i4),                  intent(in)    :: nx
           logical(kind=i1),                  intent(inout) :: errstate
           integer(kind=i4),                  intent(in)    :: iounit
           logical(kind=i4),                  intent(in)    :: logging
           logical(kind=i4),                  intent(in)    :: verbose
           logical(kind=i4),                  intent(in)    :: append
           character(len=*),                    intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=int4), automatic :: aerr
           ! Exec code ....
            
           Datum.cfname = "Not initiliazed yet."
           Datum.gfname = "Not initialized yet."
           Datum.cmpfname = "Initialized by compare function."
           Datum.nx = nx
           if(allocated(Datum.vidx)) then
              deallocate(Datum.vidx)
              allocate(Datum.vidx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.vidx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.vidx = -1
           if(allocated(Datum.cpu_results)) then
              deallocate(Datum.cpu_results)
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.cpu_results = YMM8r4_InitVal
           if(allocated(Datum.gpu_results)) then
              deallocate(Datum.gpu_results)
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.gpu_results = YMM8r4_InitVal
           if(allocated(Datum.inequality)) then
              deallocate(Datum.inequality)
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.inequality = YMM8r4_InitVal
           if(allocated(Datum.delta)) then
              deallocate(Datum.delta)
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.delta = YMM8r4_InitVal
           if(allocated(Datum.status)) then
              deallocate(Datum.status)
              allocate(Datum.status(Datum.nx), &
                   STAT=aer,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.status(Datum.nx), &
                   STAT=aer,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.status = Mask4_InitVal
           errstate = .false.
           return
9999        call  handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitYMM8r4Data -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitYMM8r4Data -- Memory Allocation Failure !!", &
                              emsg,1694)
     end subroutine InitYMM8r4Data
!==========================================================================================================!
     subroutine FPCompareYMM8r4Data(Datum,n,cutoff,method)
!DIR$  ATTRIBUTES CODE_ALIGN:32 :: FPCompareYMM8r4Data
           type(YMM8r4CompareData_t),        intent(inout) :: Datum
           type(XMM8i4_t),                   intent(in)    :: n
           type(YMM8r4_t),                   intent(in)    :: cutoff
           character(len=32),                intent(in)    :: method
           ! Locals
!DIR$      ATTRIBUTES ALIGN : 32 :: bres
           type(Mask8_t), automatic :: bres
           integer(kind=i4), automatic :: i
           ! Exec code ....
Comparison:  select case("method")
                case("vectype_equalto_vectype")
                     Datum.cmpfname = "ymm8r4_equalto_ymm8r4"
                     bres.m = .false.
                     !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                     do i=1, Datum.nx
                        bres = ymm8r4_equalto_ymm8r4(Datum.cpu_results(i), &
                                                     Datum.gpu_results(i) )
                       if(ALL(bres.m)) then
                           Datum.delta(i) = Datum.cpu_results(i) - &
                                            Datum.gpu_results(i)
                           Datum.status(i) = bres
                       else
                           Datum.vidx(i) = i
                           Datum.inequality(i) = Datum.gpu_results(i)
                           Datum.delta(i)      = Datum.cpu_results(i) - &
                                                 Datum.gpu_results(i)
                           Datum.status(i) = bres
                        end if
                     end do
                  case("ymm8r4_cwt_ymm8r4")
                     Datum.cmpfname = "ymm8r4_cwt_ymm8r4"
                     bres.m = .false.
                     !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                     do i=1, Datum.nx
                         bres = ymm8r4_cwt_ymm8r4(Datum.cpu_results(i), &
                                                 Datum.gpu_results(i), &
                                                 n, &
                                                 cutoff )
                       if(ALL(bres.m)) then
                            Datum.delta(i) = Datum.cpu_results(i) - &
                                             Datum.gpu_results(i)
                           Datum.status(i) = bres
                       else
                           Datum.vidx(i) = i
                           Datum.inequality(i) = Datum.gpu_results(i)
                           Datum.delta(i)      = Datum.cpu_results(i) - &
                             Datum.gpu_results(i)
                           Datum.status(i) = bres
                        end if
                     end do
                  case default
                      call  print_non_fatal_error( ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPCompareYMM8r4Data: Unrecognized switch argument!! ",  &
                                               __LINE__,__FILE__)
                      return
              end select Comparison
     end subroutine FPCompareYMM8r4Data
!===============================================================================================================!
     subroutine InitZMM16r4Data(Datum,nx,errstate,iounit,logging,verbose,  &
                               append,fname)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: InitZMM16r4Data
           type(ZMM16r4CompareData_t),       intent(inout) :: Datum
           integer(kind=i4),               intent(in)    :: nx
           logical(kind=i1),               intent(inout) :: errstate
           integer(kind=i4),               intent(in)    :: iounit
           logical(kind=i4),               intent(in)    :: logging
           logical(kind=i4),               intent(in)    :: verbose
           logical(kind=i4),               intent(in)    :: append
           character(len=*),                 intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=i4), automatic :: aerr
           ! Exec code ....
           Datum.cfname = "Not initiliazed yet."
           Datum.gfname = "Not initialized yet."
           Datum.cmpfname = "Initialized by compare function."
           Datum.nx = nx
           if(allocated(Datum.vidx)) then
              deallocate(Datum.vidx)
              allocate(Datum.vidx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.vidx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.vidx = -1
           if(allocated(Datum.cpu_results)) then
              deallocate(Datum.cpu_results)
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.cpu_results = ZMM16r4_InitVal
           if(allocate(Datum.gpu_results)) then
              deallocate(Datum.gpu_results)
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.gpu_results = ZMM16r4_InitVal
           if(allocated(Datum.inequality)) then
              deallocate(Datum.inequality)
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.inequality = ZMM16r4_InitVal
           if(allocated(Datum.delta)) then
              deallocate(Datum.delta)
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.delta = ZMM16r4_InitVal
           if(allocated(Datum.status)) then
              deallocate(Datum.status)
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.status = Mask16_InitVal
           errstate = .false.
           return
9999       call  handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitZMM16r4Data -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitZMM16r4Data -- Memory Allocation Failure !!", &
                              emsg,1846)
     end subroutine InitZMM16r4Data
!===========================================================================================================!
     subroutine FPCompareZMM16r4Data(Datum,n,cutoff,method)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: FPCompareZMM16r4Data
            type(ZMM16r4CompareData_t),        intent(inout) :: Datum
            type(XMM16i4_t),                   intent(in)    :: n
            type(ZMM16r4_t),                   intent(in)    :: cutoff
            character(len=32),                 intent(in)    :: method
            ! Locals
!DIR$ ATTRIBUTES ALIGN : 64 :: bres
            type(Mask16_t), automatic :: bres
            integer(kind=i4), automatic :: i
            ! Exec code ....
Comparison: select case("method")
               case("vectype_equalto_vectype")
                  Datum.cmpfname = "zmm16r4_equalto_zmm16r4"
                  bres.m = .false.
                  !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                  do i=1, Datum.nx
                     bres = zmm16r4_equalto_zmm16r4(Datum.cpu_results(i), &
                          Datum.gpu_results(i))
                     if(ALL(bres.m)) then
                          Datum.delta(i) = Datum.cpu_results(i) - &
                                            Datum.gpu_results(i)
                          Datum.status(i) = bres
                      else
                          Datum.vidx(i) = i
                          Datum.inequality(i) = Datum.gpu_results(i)
                          Datum.delta(i)      = Datum.cpu_results(i) - &
                                                 Datum.gpu_results(i)
                          Datum.status(i) = bres
                      end if
                   end do
                case("zmm16r4_cwt_zmm16r4")
                   Datum.cmpfname = "zmm16r4_cwt_zmm16r4"
                   bres.m = .false.
                   !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=4))
                   do i=1, Datum.nx
                      bres = zmm16r4_cwt_zmm16r4(Datum.cpu_results(i), &
                                                 Datum.gpu_results(i), &
                                                 n, &
                                                 cutoff)
                      if(ALL(bres.m)) then
                          Datum.delta(i) = Datum.cpu_results(i) - &
                                            Datum.gpu_results(i)
                          Datum.status(i) = bres
                      else
                          Datum.vidx(i) = i
                          Datum.inequality(i) = Datum.gpu_results(i)
                          Datum.delta(i)      = Datum.cpu_results(i) - &
                                                 Datum.gpu_results(i)
                          Datum.status(i) = bres
                      end if
                   end do
                case default
                   call  print_non_fatal_error( ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPCompareZMM16r4Data: Unrecognized switch argument!! ",  &
                                               __LINE__,__FILE__)
                   return
                end select Comparison
     end subroutine FPCompareZMM16r4Data
!==========================================================================================================!
     subroutine InitZMM8r8Data(Datum,nx,errstate,iounit,logging,verbose, &
          append,fname)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: InitZMM8r8Data
           type(ZMM8r8CompareData_t),          intent(inout) :: Datum
           integer(kind=i4),                 intent(in)    :: nx
           logical(kind=i1),                 intent(in)    :: errstate
           integer(kind=i4),                 intent(in)    :: iounit
           logical(kind=i4),                 intent(in)    :: logging
           logical(kind=i4),                 intent(in)    :: verbose
           logical(kind=i4),                 intent(in)    :: append
           character(len=*),                   intent(in)    :: fname
           ! Locals
           character(len=256), automatic :: emsg
           integer(kind=int4), automatic :: aerr
           ! Exec code ....
           Datum.cfname = "Not initiliazed yet."
           Datum.gfname = "Not initialized yet."
           Datum.cmpfname = "Initialized by compare function."
           Datum.nx = nx
           if(allocated(Datum.vidx)) then
              deallocate(Datum.vidx)
              allocate(Datum.vidx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.vidx(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.vidx = -1
           if(allocated(Datum.cpu_results)) then
              deallocate(Datum.cpu_results)
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.cpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.cpu_results = ZMM8r8_InitVal
           if(allocated(Datum.gpu_results)) then
              deallocate(Datum.gpu_results)
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.gpu_results(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.gpu_results = ZMM8r8_InitVal
           if(allocated(Datum.inequality)) then
              deallocate(Datum.inequality)
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.inequality(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.inequality = ZMM8r8_InitVal
           if(allocated(Datum.delta)) then
              deallocate(Datum.delta)
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.delta(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.delta = ZMM8r8_InitVal
           if(allocated(Datum.status)) then
              deallocate(Datum.status)
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           else
              allocate(Datum.status(Datum.nx), &
                   STAT=aerr,ERRMSG=emsg)
              if(aerr /= 0) goto 9999
           end if
           Datum.status = Mask8_InitVal
           errstate = .false.
           return
9999       handle_fatal_memory_error(iounit, logging,verbose,append,fname, &
                     "logger: "// __FILE__ // "module: mod_compare_cpu_gpu, subroutine: InitZMM8r8Data -- Memory Allocation Failure !!", &                                                       
                              "module: mod_compare_cpu_gpu, subroutine: InitZMM8r8Data -- Memory Allocation Failure !!", &
                              emsg,2000)
     end subroutine InitZMM8r8Data
!========================================================================================================!
     subroutine FPCompareZMM8r8Data(Datum,n,cutoff,method)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: FPCompareZMM8r8Data
           type(ZMM8r8CompareData_t),           intent(inout) :: Datum
           type(XMM8i8_t),                      intent(in)    :: n
           type(ZMM8r8_t),                      intent(in)    :: cutoff
           character(len=32),                   intent(in)    :: method
           ! Locals
!DIR$  ATTRIBUTES ALIGN : 32 :: bres
           type(Mask8_t), automatic :: bres
           integer(kind=i4), automatic :: i
           ! Exec code ......
Comparison:   select case("method")
                 case("vectype_equalto_vectype")
                    Datum.cmpfname = "zmm8r8_equalto_zmm8r8"
                    bres.m = .false.
                    !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                    do i=1, Datum.nx
                       bres = zmm8r8_equalto_zmm8r8(Datum.cpu_results(i), &
                                                    Datum.gpu_results(i))
                       if(ALL(bres.m)) then
                          Datum.delta(i) = Datum.cpu_results(i) - &
                                            Datum.gpu_results(i)
                          Datum.status(i) = bres
                      else
                          Datum.vidx(i) = i
                          Datum.inequality(i) = Datum.gpu_results(i)
                          Datum.delta(i)      = Datum.cpu_results(i) - &
                                                 Datum.gpu_results(i)
                          Datum.status(i) = bres
                      end if
                   end do
                case("zmm8r8_cwt_zmm8r8")
                   Datum.cmpfname = "zmm8r8_cwt_zmm8r8"
                   bres.m = .false.
                   !DIR$ SIMD VECTORLENGTHFOR(REAL(KIND=8))
                   do i=1, Datum.nx
                      bres = zmm8r8_cwt_zmm8r8(Datum.cpu_results(i), &
                                               Datum.gpu_results(i), &
                                               n, &
                                               cutoff)
                      if(ALL(bres.m)) then
                          Datum.delta(i) = Datum.cpu_results(i) - &
                                            Datum.gpu_results(i)
                          Datum.status(i) = bres
                      else
                          Datum.vidx(i) = i
                          Datum.inequality(i) = Datum.gpu_results(i)
                          Datum.delta(i)      = Datum.cpu_results(i) - &
                                                 Datum.gpu_results(i)
                          Datum.status(i) = bres
                      end if
                   end do
                case default
                   call print_non_fatal_error( ================= Non-Fatal ================== " , &
                                             " Module: mod_compare_cpu_gpu, subroutine: FPCompareZMM8r8Data: Unrecognized switch argument!! ",  &
                                               __LINE__,__FILE__)
                   return
                end select Comparison
     end subroutine FPCompareZMM8r8Data
       
end module mod_compare_cpu_gpu
