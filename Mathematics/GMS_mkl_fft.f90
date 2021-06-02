
#include "Config.fpp"

module mod_mkl_fft

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_mkl_fft'
 !          
 !          Purpose:
 !                   Wrapper around Intel MKL Fortran FFT implementation.
 !          History:
 !                        Date: 06-05-2018
 !                        Time: 10:45 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                  Bernard Gingold
 !                 
 !          References:
 !         
 !                 Intel MKL library manual. 
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use module_kinds, only : i4, dp
    use IFPORT,       only :  TRACEBACKQQ
    
    use mkl_dfti , work_prec => DFTI_DOUBLE, DFTI_DOUBLE => DFTI_DOUBLE_R
    implicit none
    public
    
    private :: handle_descriptor_error, &
               print_descriptor_state
    
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(i4), parameter, public :: MOD_MKL_FFT_MAJOR = 1
    
    ! Minor version
    integer(i4), parameter, public :: MOD_MKL_FFT_MINOR = 1
    
    ! Micro version
    integer(i4), parameter, public :: MOD_MKL_FFT_MICRO = 0
    
    ! Module full version
    integer(i4), parameter, public :: MOD_MKL_FFT_FULLVER = 1000*MOD_MKL_FFT_MAJOR + &
                                                              100*MOD_MKL_FFT_MINOR  + &
                                                              10*MOD_MKL_FFT_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_MKL_FFT_CREATE_DATE = "06-05-2018 10:42 +00200 (SUN 06 MAY 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation date/time)
    character(*),  parameter, public :: MOD_MKL_FFT_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_MKL_FFT_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_MKL_FFT_DESCRIPT = "Wrappers around Intel MKL Fortran FFT implementation."
    
    ! Module constants
    
    integer(I32P), parameter, public :: TRANSFORM_FORWARD = 1_I32P
    
    integer(I32P), parameter, public :: TRANSFORM_BACKWARD = 2_I32P 
    
    contains
    
    !
    !  Complex-to-complex FFT 1D (out-place)
    !  No argument checking at this level
    !
    !
    subroutine fft1D_ccop( data_in,data_out,data_len,status,fft_type,  &
                         dim_len,verbose,callstack)
          complex(dp), dimension(data_len), intent(in) :: data_in
          !DIR$ ASSUME_ALIGNED data_in:64
          complex(dp), dimension(data_out), intent(out) :: data_out
          !DIR$ ASSUME)ALIGNED data_out:64
          integer(i4),                      intent(in)              ::  data_len
          integer(i4),                      intent(inout)           ::  status
          integer(i4),                      intent(in)              ::  fft_type ! backward or forward  (
          integer(i4),                      intent(in)              ::  dim_len
          logical(i4),                      intent(in),optional     ::  verbose
          logical(i4),                      intent(in)              ::  callstack
         ! type(QPCTimer_t),                   intent(inout)  ::  qpctimer
          ! Locals
          type(DFTI_DESCRIPTOR), pointer :: handle => null()
          !integer(BOOL) :: ifail
          ! logical(I32P) :: bfail
          integer(i4) :: status_on_fail ! return value (status) used on failure of FFT functions.
          integer(i4) :: caller_stat
          ! Executable statements
          if(0 /= status) status = 0
          
          status = DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_COMPLEX,dim_len,data_len)
          if(0 /= status) then
               call handle_descriptor_error(handle, "fft1D_ccop: Fatal Local Error -- DftiCreateDescriptor failed with an error:", &
                                                    "fft1D_ccop: DftiCreateDescriptor -- FAILED !!!", callstack,status)
              return                                       
          end if    
          status = DftiSetValue(handle,DFTI_PLACEMENT,DFTI_NOT_INPLACE)
         if(0 /= status) then
              call handle_descriptor_error(handle, "fft1D_ccop: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                   "fft1D_ccop: DftiSetValue -- FAILED !!!",callstack,status)
              return
         end if 
         status = DftiCommitDescriptor(handle)
         if(0 /= status) then
             call handle_descriptor_error(handle, "fft1D_ccop: Fatal Local Error -- DftiCommitDescriptor failed with an error:", &
                                                  "fft1D_ccop: DftiCommitDescriptor -- FAILED !!!",callstack,status)
             return
         end if
         if(fft_type == TRANSFORM_FORWARD) then
               status = DftiComputeForward(handle,data_in,data_out)
               if(0 /= status) then
                   call handle_descriptor_error(handle, "fft1D_ccop: Fatal Local Error -- DftiComputeForward failed with an error:", &
                                                        "fft1D_ccop: DftiComputeForward -- FAILED !!!",callstack,status )
                  return
              end if    
              
          else if(fft_type == TRANSFORM_BACKWARD) then
               status = DftiComputeBackward(handle,data_in,data_out)
               if(0 /= status) then
                   call handle_descriptor_error(handle, "fft1D_ccop: Fatal Local Error -- DftiComputeBackward failed with an error:", &
                                                        "fft1D_ccop: DftiComputeBackward -- FAILED !!!",callstack,status)
                   return
              end if     
          end if     
         if(verbose == .true.) then
              call print_descriptor_state(handle,caller_stat)
         end if
         if(0 == status)  then
             status_on_faile = DftiFreeDescriptor(handle)
         end if
   end subroutine      
         
    !
    !  Complex-to-complex FFT 1D (in-place)
    !  No argument checking at this level
    !
    !           
    subroutine fft1D_ccip(data_inout,data_len,status,fft_type, &
                            dim_len,verbose,callstack  )
          complex(dp), dimension(data_len), intent(inout) :: data_inout
          !DIR$ ASSUME_ALIGNED data_inout:64
          integer(i4),                      intent(in)    :: data_len
          integer(i4),                      intent(inout) :: status
          integer(i4),                      intent(in)    :: fft_type,dim_len
          logical(i4),                      intent(in)    :: verbose,callstack
          ! Locals
          type(DFT_DESCRIPTOR), pointer :: handle => null()
          integer(i4) :: status_on_fail,caller_stat
          ! Executable statements
          if(0 /= status) status = 0
          status =  DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_COMPLEX,dim_len,data_len)
          if(0 /= status) then
              call handle_descriptor_error(handle,"fft1D_ccin: Fatal Local Error -- DftiCreateDescriptor failed with an error:",&
                                                  "fft1D_ccin: DftiCreateDescriptor -- FAILED !!!",callstack,status )
              return
          end if
          status = DftiCommitDescriptor(handle)
          if(0 /= status) then
              call handle_descriptor_error(handle,"fft1D_ccin: Fatal Local Error -- DftiCommitDescriptor failed with an error:", &
                                                  "fft1D_ccin: DftiCommitDescriptor -- FAILED !!!",callstack,status)
              return
          end if
          if(fft_type == TRANSFORM_FORWARD) then
              status = DftiComputeForward(handle,data_inout)
              if(0 /= status) then
                  call handle_descriptor_error(handle,"fft1D_ccin: Fatal Local Error -- DftiComputeForward failed with an error:", &
                                                      "fft1D_ccin: DftiComputeForward -- FAILED !!!",callstack,status)
                  return
              end if
          else if(fft_type == TRANSFORM_BACKWARD) then
              status = DftiComputeBackward(handle,data_inout)
              if(0 /= status) then
                  call handle_descriptor_error(handle,"fft1D_ccin: Fatal Local Error -- DftiComputeBackward failed with an error:", &
                                                      "fft1D_ccin: DftiComputeBackwad -- FAILED !!!",callstack,status)
                  return
              end if
          end if
          if(verbose == .true.) then
              call print_descriptor_state(handle,status)
          end if
          if(verbose == .true.) then
              call print_descriptor_state(handle,caller_stat)
          end if
          if(0 ==  status) then
              status_on_fail = DftiFreeDescriptor(handle)
          end if
    end subroutine 
    
    !
    !  Complex-to-complex FFT 2D (out-place)
    !  No argument checking at this level
    !
    !                        
    subroutine fft2D_ccop(data_in,data_out,data_len1,data_len2,dim_len, &
                          status,fft_type,verbose,callstack    )
          complex(dp), dimension(data_len1,data_len2), intent(in)     :: data_in
          !DIR$ ASSUME_ALIGNED data_in:64
          complex(dp), dimension(data_len1,data_len2), intent(out)    :: data_out
          !DIR$ ASSUME_ALIGNED data_out:64
          integer(dp),                                 intent(in)     :: data_len1,data_len2, &
                                                                            dim_len
          integer(i4),                                 intent(inout)  :: status
          integer(i4),                                 intent(in)     :: fft_type
          logical(i4),                                 intent(in)     :: verbose,callstack
          ! Locals
          type(DFTI_DESCRIPTOR), pointer :: handle => null()
          integer(i4) :: stat_dont_care ,caller_stat
          ! Executable statements
          if(0 /= status) status = 0
          status = DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_COMPLEX,dim_len,[data_len1,data_len2])
          if(0 /= status) then
              call handle_descriptor_error(handle,"fft2D_ccop: Fatal Local Error -- DftiCreateDescriptor failed with an error:", &
                                                  "fft2D_ccop: DftiCreateDescriptor -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiSetValue(handle,DFTI_PLACEMENT,DFTI_NOT_INPLACE)
          if(0 /= status) then
              call handle_descriptor_error(handle, "fft2D_ccop: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                   "fft2D_ccop: DftiSetValue -- FAILED !!!",callstack,status)
              return
          end if
          status = DftiCommitDescriptor(handle)
          if(0 /= status) then
              call handle_descriptor_error(handle,"fft2D_ccop: Fatal Local Error -- DftiCommitDescriptor failed with an error:", &
                                                  "fft2D_ccop: DftiCommitDescriptor -- FAILED !!!",callstack,status)
              return
          end if
          if(fft_type == TRANSFORM_FORWARD) then
              status = DftiComputeForward(handle,data_in(:,1),data_out(:,1))
              if(0 /= status) then
                  call handle_descriptor_error(handle, "fft2D_ccop: Fatal Local Error -- DftiComputeForward failed with an error:", &
                                                       "fft2D_ccop: DftiComputeForward -- FAILED !!!",callstack,status )
                  return
              end if
          else if(fft_type == TRANSFORM_BACKWARD) then
              status = DftiComputeBackward(handle,data_in(:,1),data_out(:,1))
              if(0 /= status) then
                  call handle_descriptor_error(handle, "fft2D_ccop: Fatal Local Error -- DftiComputeBackward failed with an error:", &
                                                       "fft2D_ccop: DftiComputeBackward -- FAILED !!!",callstack,status)
                  return
              end if
          end if
          if(verbose == .true.) then
              call print_descriptor_state(handle,caller_stat)
          end if
          if(0 == status) then
              stat_dont_care = DftiFreeDescript(handle)
          end if
    end subroutine
                          
    !
    !  Complex-to-complex FFT 2D (in-place)
    !  No argument checking at this level
    !
    !                       
    subroutine fft2D_ccip(data_inout,data_len1,data_len2,dim_len, &
                          status,fft_type,verbose,callstack  )
          complex(dp), dimension(data_len1,data_len2), intent(inout) :: data_inout
          !DIR$ ASSUME_ALIGNED data_inout:64
          integer(i4),                                 intent(in)    :: data_len1,data_len2, &
                                                                          dim_len
          integer(i4),                                 intent(inout) :: status
          integer(i4),                                 intent(in)    :: fft_type
          logical(i4),                                 intent(in)    :: verbose,callstack
          ! Locals
          type(DFTI_DESCRIPTOR), pointer :: handle => null()
          integer(i4) :: stat_dont_care,caller_stat
          ! Executable statements
          if(0 /= status) status = 0
          status = DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_COMPLEX,dim_len,[data_len1,data_len2])
          if(0 /= status) then
              call handle_descriptor_error(handle, "fft2D_ccip: Fatal Local Error -- DftiCreateDescriptor failed with an error:", &
                                                   "fft2D_ccip: DftiCreateDescriptor -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiCommitDescriptor(handle)
          if(0 /= status) then
              call handle_descriptor_error(handle, "fft2D_ccip: Fatal Local Error -- DftiCommitDescriptor failed with an error:", &
                                                   "fft2D_ccip: DftiCommitDescriptor -- FAILED !!!",callstack,status)
              return
          end if
          if(fft_type == TRANSFORM_FORWARD) then
              status = DftiComputeForward(handle,data_inout(:,1))
              if(0 /= status) then
                  call handle_descriptor_error(handle, "fft2D_ccip: Fatal Local Error -- DftiComputeForward failed with an error:", &
                                                       "fft2D_ccip: DftiComputeForward -- FAILED !!!",callstack,status )
                  return
              end if
          else if(fft_type == TRANSFORM_BACKWARD) then
              status = DftiComputeBackward(handle,data_inout(:,1))
              if(0 /= status) then
                 call handle_descriptor_error(handle, "fft2D_ccip: Fatal Local Error -- DftiComputeBackward failed with an error:", &
                                                  "fft2D_ccip: DftiComputeBackward -- FAILED !!!",callstack,status)
                  return
              end if
          end if
          if(verbose == .true.) then
              call print_descriptor_state(handle,caller_stat)
          end if
          if(0 == status) then
              stat_dont_care = DftiFreeDescriptor(handle)
          end if
    end subroutine    
                          
    !
    !  Complex-to-complex FFT 3D (out-place)
    !  No argument checking at this level
    !
    !                         
    subroutine fft3D_ccop(data_in,data_out,data_len1,data_len2,data_len3,  &
                          dim_len,status,fft_type,verbose,callstack   )
          complex(R64P), dimension(data_len1,data_len2,data_len3), intent(in)    :: data_in
          !DIR$ ASSUME_ALIGNED data_in:64
          complex(R64P), dimension(data_len1,data_len2,data_len3), intent(out)   :: data_out
          !DIR$ ASSUME_ALIGNED data_out:64
          integer(I32P),                                           intent(in)    ::  data_len1 , &
                                                                                     data_len2,  &
                                                                                     data_len3,  &
                                                                                     dim_len
          integer(I32P),                                           intent(inout)  :: status
          integer(I32P),                                           intent(in)     :: fft_type
          logical(I32P),                                           intent(in)     :: verbose,callstack
          ! Locals
          type(DFTI_DESCRIPTOR), pointer :: handle => null()
          integer(I32P)                  :: stat_dont_care , caller_stat
          ! Executable statements
          if(0_I32P /= status) status = 0_I32P
          status = DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_COMPLEX,dim_len,[data_len1,data_len2,data_len3])
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft3D_ccop: Fatal Local Error -- DftiCreateDescriptor failed with an error:", &
                                                   "fft3D_ccop: DftiCreateDescriptor -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiSetValue(handle,DFTI_PLACEMENT,DFTI_NOT_INPLACE)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle,"fft3D_ccop: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                  "fft3D_ccop: DftiSetValue -- FAILED !!!",callstack,status)
              return
          end if
          status = DftiCommitDescriptor(handle)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft3D_ccop: Fatal Local Error -- DftiCommitDescriptor failed with an error:", &
                                                   "fft3D_ccop: DftiCommitDescriptor -- FAILED !!!",callstack,status)
              return
          end if
          if(fft_type == TRAMSFORM_FORWARD) then
              status = DftiComputeForward(handle,data_in(:,1,1),data_out(:,1,1))
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle, "fft3D_ccop: Fatal Local Error -- DftiComputeForward failed with an error:", &
                                                       "fft3D_ccop: DftiComputeForward -- FAILED !!!",callstack,status )
                  return
              end if
          else if(fft_type == TRANSFORM_BACKWARD) then
              status = DftiComputeBackward(handle,data_in(:,1,1),data_out(:,1,1))
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle,"fft3D_ccop: Fatal Local Error -- DftiComputeBackward failed with an error:", &
                                                      "fft3D_ccop: DftiComputeBackward -- FAILED !!!",callstack,status)
                  return
              end if
          end if
          if(verbose == .true.) then
              call print_descriptor_state(handle,caller_stat)
          end if
          if(0_I32P == status) then
              stat_dont_care = DftiFreeDescriptor(handle)
          end if
    end subroutine
                          
    !
    !  Complex-to-complex FFT 3D (in-place)
    !  No argument checking at this level
    !
    !    
    subroutine fft3D_ccip(data_inout,data_len1,data_len2,data_len3,  &
                         dim_len,status,fft_type,verbose,callstack)
          complex(R64P), dimension(data_len1,data_len2,data_len3), intent(inout) :: data_inout
          !DIR$ ASSUME_ALIGNED data_inout:64
          integer(I32P),                                           intent(in)    :: data_len1, &
                                                                                    data_len2, &
                                                                                    data_len3, &
                                                                                    dim_len
          integer(I32P),                                           intent(inout) :: status
          integer(I32P),                                           intent(in)    :: fft_type
          logical(I32P),                                           intent(in)    :: verbose,callstack
          ! Locals
          type(DFTI_DESCRIPTOR), pointer :: handle => null()
          integer(I32P)                  :: stat_dont_care,caller_stat
          ! Excutable code
          if(0_I32P /= status)  status = 0_I32P
          status = DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_COMPLEX,dim_len,[data_len1,data_len2,data_len3])
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft3D_ccip: Fatal Local Error -- DftiCreateDescriptor failed with an error:", &
                                                   "fft3D_ccip: DftiCreateDescriptor -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiCommitDescriptor(handle)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft3D_ccip: Fatal Local Error -- DftiCommitDescriptor failed with an error:", &
                                                   "fft3D_ccip: DftiCommitDescriptor -- FAILED !!!",callstack,status)
              return
          end if
          if(fft_type == TRANSFORM_FORWARD) then
              status = DftiComputeForward(handle,data_inout(:,1,1))
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle, "fft3D_ccip: Fatal Local Error -- DftiComputeForward failed with an error:", &
                                                       "fft3D_ccip: DftiComputeForward -- FAILED !!!",callstack,status )
                  return
              end if
          else if(fft_type == TRANSFORM_BACKWARD) then
              status = DftiComputeBackward(handle,data_inout(:,1,1))
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle, "fft3D_ccip: Fatal Local Error -- DftiComputeBackward failed with an error:", &
                                                       "fft3D_ccip: DftiComputeBackward -- FAILED !!!",callstack,status )
                  return
              end if
          end if
          if(verbose == .true.) then
              call print_descriptor_state(handle,caller_stat)
          end if
          if(0_I32P == status) then
              stat_dont_care = DftiFreeDescriptor(handle)
          end if
    end subroutine
                         
    !
    !  Real-to-complex FFT 1D (out-place)
    !  No argument checking at this level
    !
    !   
    subroutine fft1D_rcop(data_in,data_out,data_len1,dim_len,   &
                          status,fft_type,verbose,callstack)
          real(R64P),    dimension(data_len1), intent(inout)     :: data_in
          !DIR$ ASSUME_ALIGNED data_in:64
          complex(R64P), dimension(data_len1), intent(inout)     :: data_out
          !DIR$ ASSUME_ALIGNED data_out:64
          integer(I32P),                       intent(in)        :: data_len1,dim_len
          integer(I32P),                       intent(inout)     :: status
          integer(I32P),                       intent(in)        :: fft_type
          logical(I32P),                       intent(in)        :: verbose,callstack
          ! Locals
          type(DFTI_DESCRIPTOR), pointer :: handle => null()
          integer(I32P)                  :: stat_dont_care,caller_stat
          ! Executable statemtns
          if(0_I32P /= status) status = 0_I32P
          status = DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_REAL,dim_len,data_len1)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft1D_rcop: Fatal Local Error -- DftiCreateDescriptor failed with an error:", &
                                                   "fft1D_rcop: DftiCreateDescriptor -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiSetValue(handle,DFTI_PLACEMENT,DFTI_NOT_INPLACE)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle,"fft1D_rcop: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                  "fft1D_rcop: DftiSetValue -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiSetValue(handle,DFTI_CONJUGATE_EVEN_STORAGE,DFTI_COMPLEX_COMPLEX)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle,"fft1D_rcop: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                  "fft1D_rcop: DftiSetValue -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiCommitDescriptor(handle)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle,"fft1D_rcop: Fatal Local Error -- DftiCommitDescriptor failed with an error:", &
                                                  "fft1D_rcop: DftiCommitDescrptor -- FAILED !!!", callstack,status)
              return
          end if
          if(fft_type == TRANSFORM_FORWARD) then
              status = DftiComputeForward(handle,data_in,data_out)
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle,  "fft1D_rcop: Fatal Local Error -- DftiComputeForward failed with an error:", &
                                                        "fft1D_rcop: DftiComputeForward -- FAILED !!!", callstack,status)
                  return
              end if
          else if(fft_type == TRANSFORM_BACKWARD) then
              status = DftiComputeBackward(handle,data_out,data_in)
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle,  "fft1D_rcop: Fatal Local Error -- DftiComputeBackward failed with an error:", &
                                                        "fft1D_rcop: DftiComputeBackward -- FAILED !!!", callstack,status)
                  return
              end if
          end if
          if(verbose == .true.) then
              call print_descriptor_state(handle,caller_stat)
          end if
          if(0_I32P == status) then
              stat_dont_care = DftiFreeDescriptor(handle)
          end if
    end subroutine 
                          
    !
    !  Real-to-complex FFT 2D (out-place)
    !  No argument checking at this level
    !
    !   
    subroutine fft2D_rcop(data_r,data_c,data_len1,data_len2,cs,rs,dim_len, &
                           status,fft_type,verbose,callstack      )
          real(R64P),    dimension(data_len1,data_len2), intent(inout) :: data_r
          !DIR$ ASSUME_ALIGNED data_r:64
          complex(R64P), dimension(data_len1,data_len2), intent(inout) :: data_c
          !DIR$ ASSUME_ALIGNED data_c:64
          integer(I32P),                                 intent(in)    :: data_len1,data_len2
          integer(I32P), dimension(3),                   intent(in)    :: cs,rs ! strides
          integer(I32P),                                 intent(in)    :: dim_len
          integer(I32P),                                 intent(inout) :: status
          integer(I32P),                                 intent(in)    :: fft_type
          logical(I32P),                                 intent(in)    :: verbose,callstack
          ! Locals
          type(DFTI_DESCRIPTOR), pointer :: handle => null()
          integer(I32P) :: stat_dont_care,caller_stat
          if(0_I32P /= status) status = 0_I32P
          status = DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_REAL,dim_len,[data_len1,data_len2])
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft2D_rcop: Fatal Local Error -- DftiCreateDescriptor failed with an error:", &
                                                   "fft2D_rcop: DftiCreateDescriptor -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiSetValue(handle,DFTI_PLACEMENT,DFTI_NOT_INPLACE)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft2D_rcop: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                   "fft2D_rcop: DftiSetValue -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiSetValue(handle,DFTI_CONJUGATE_EVEN_STORAGE,DFTI_COMPLEX_COMPLEX)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft2D_rcop: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                   "fft2D_rcop: DftiSetValue -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiSetValue(handle,DFTI_INPUT_STRIDES,rs)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft2D_rcop: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                   "fft2D_rcop: DftiSetValue -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiSetValue(handle,DFTI_OUTPUT_STRIDES,cs)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft2D_rcop: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                   "fft2D_rcop: DftiSetValue -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiCommitDescriptor(handle)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle,"fft2D_rcop: Fatal Local Error -- DftiCommitDescriptor failed with an error:", &
                                                  "fft2D_rcop: DftiCommitDescrptor -- FAILED !!!", callstack,status)
              return
          end if
          if(fft_type == TRANSFORM_FORWARD) then
              status = DftiComputeForward(handle,data_r(:,1),data_c(:,1))
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle, "fft2D_rcop: Fatal Local Error -- DftiComputeForward failed with an error:", &
                                                       "fft2D_rcop: DftiComputeForward -- FAILED !!!", callstack,status)
                  return
              end if
          else if(fft_type == TRANSFORM_BACKWARD) then
               status = DftiComputeBackward(handle,data_c(:,1),data_r(:,1))
               if(0_I32P /= status) then
                   call handle_descriptor_error(handle, "fft2D_rcop: Fatal Local Error -- DftiComputeBackward failed with an error:", &
                                                        "fft2D_rcop: DftiComputeBackward -- FAILED !!!", callstack,status)
                   return
               end if
          end if
          if(verbose == .true.) then
              call print_descriptor_state(handle,caller_stat)
          end if
          if(0_I32P == status) then
              stat_dont_care = DftiFreeDescriptor(handle)
          end if
    end subroutine
                           
    !                       
    !   Multiple number of the same transform
    !   Complex domain array 1D                    
    !   Complex-to-complex
    !
    
    subroutine fft1D_mtin(datum,data_len1,dim_len,ntimes,status, &
                            fft_type,verbose,callstack   )
          complex(R64P), dimension(data_len1), intent(inout) :: datum
          !DIR$ ASSUME_ALIGNED datum:64
          integer(I32P),                       intent(in)       :: data_len1,dim_len,ntimes
          integer(I32P),                       intent(inout)    :: status
          integer(I32P),                       intent(in)       :: fft_type
          logical(I32P),                       intent(in)       :: verbose,callstack
          ! Locals
          type(DFTI_DESCRIPTOR), pointer :: handle => null()
          integer(I32P) :: stat_dont_care,caller_stat
          ! Executable code
          if(0_I32P /= status) status = 0_I32P
          status = DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_COMPLEX,dim_len,data_len1)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft1D_mtin: Fatal Local Error -- DftiCreateDescriptor failed with an error:", &
                                                   "fft1D_mtin: DftiCreateDescriptor -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiSetValue(handle,DFTI_NUMBER_OF_TRANSFORM,ntimes)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft1D_mtin: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                   "fft1D_mtin: DftiSetValue -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiSetValue(handle, DFTI_INPUT_DISTANCE,data_len1)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle,  "fft1D_mtin: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                    "fft1D_mtin: DftiSetValue -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiCommitDescriptor(handle)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle,  "fft1D_mtin: Fatal Local Error -- DftiCommitDescriptor failed with an error:", &
                                                    "fft1D_mtin: DftiCommitDescriptor -- FAILED !!!", callstack,status)
              return
          end if
          if(fft_type == TRANSFORM_FORWARD) then
              status = DftiComputeForward(handle,datum)
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle, "fft1D_mtin: Fatal Local Error -- DftiComputeForward failed with an error:", &
                                                       "fft1D_mtin: DftiComputeForward -- FAILED !!!", callstack,status)
                  return
              end if
          else if(fft_type == TRANSFORM_BACKWARD) then
              status = DftiComputeBackward(handle,datum) then
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle, "fft1D_mtin: Fatal Local Error -- DftiComputeBackward failed with an error:", &
                                                       "fft1D_mtin: DftiComputeBackward -- FAILED !!!", callstack,status)
                  return
              end if
          end if
          if(verbose == .true.) then
              call print_descriptor_state(handle,caller_stat)
          end if
          if(0_I32P == status)  then
               stat_dont_care = DftiFreeDescriptor(handle)
          end if
    end subroutine
                            
    !                       
    !   Multiple number of the same transform
    !   Complex domain array 2D                    
    !   Complex-to-complex
    !
    subroutine fft2D_mtin(datum,data_len1,data_len2,dim_len,ntimes,  &
                          status,fft_type,verbose,callstack      )
          complex(R64P), dimension(data_len1,data_len2), intent(inout) :: datum
          !DIR$ ASSUME_ALIGNED datum:64
          integer(I32P),                                 intent(in)       ::  data_len1,data_len2, &
                                                                              dim_len, ntimes
          integer(I32P),                                 intent(inout)    ::  status
          integer(I32P),                                 intent(in)       ::  fft_type
          logical(I32P),                                 intent(in)       ::  verbose,callstack
          ! Locals
          type(DFTI_DESCRIPTOR), pointer :: handle => null()
          integer(I32P)                  :: stat_dont_care,caller_stat
          integer(I32P)                  :: distance
          ! Executable statments
          if(0_I32P /= status) status = 0_I32P
          status = DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_COMPLEX,dim_len,[data_len1,data_len2])
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft2D_mtin: Fatal Local Error -- DftiCreateDescriptor failed with an error:", &
                                                   "fft2D_mtin: DftiCreateDescriptor -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiSetValue(handle,DFTI_NUMBERS_OF_TRANSFORMS,ntimes)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft2D_mtin: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                   "fft2D_mtin: DftiSetValue -- FAILED !!!", callstack,status)
              return
          end if
          distance = size(datum,dim=1) * size(datum,dim=2)
          status = DftiSetValue(handle,DFTI_INPUT_DISTANCE,distance)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft2D_mtin: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                   "fft2D_mtin: DftiSetValue -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiCommitDescriptor(handle)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle,  "fft2D_mtin: Fatal Local Error -- DftiCommitDescriptor failed with an error:", &
                                                    "fft2D_mtin: DftiCommitDescriptor -- FAILED !!!", callstack,status)
              return
          end if
          if(fft_type == TRANSFORM_FORWARD) then
              status = DftiComputeForward(handle,datum(:,1))
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle, "fft2D_mtin: Fatal Local Error -- DftiComputeForward failed with an error:", &
                                                       "fft2D_mtin: DftiComputeForward -- FAILED !!!", callstack,status)
                  return
              end if
          else if(fft_type == TRANSFORM_BACKWARD) then
              status = DftiComputeBackward(handle,datum(:,1))
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle, "fft2D_mtin: Fatal Local Error -- DftiComputeBackward failed with an error:", &
                                                       "fft2D_mtin: DftiComputeBackward -- FAILED !!!", callstack,status)
                  return
              end if
          end if
          if(verbose == .true.) then
              call print_descriptor_state(handle,caller_stat)
          end if
          if(0_I32P == status) then
              stat_dont_care = DftiFreeDescriptor(handle)
          end if
    end subroutine
                          
    !                       
    !   Multiple number of the same transform
    !   Complex domain array 3D                    
    !   Complex-to-complex
    !                      
    subroutine fft3D_mtin(datum,data_len1,data_len2,data_len3,dim_len, &
                            ntimes,status,fft_type,verbose,callstack  )
          complex(R64P), dimension(data_len1,data_len2,data_len3), intent(inout) :: datum
          !DIR$ ASSUME_ALIGNED datum:64
          integer(I32P),                                           intent(in)    :: data_len1, &
                                                                                    data_len2, &
                                                                                    data_len3, &
                                                                                    dim_len,   &
                                                                                    ntimes
          integer(I32P),                                           intent(inout) :: status
          integer(I32P),                                           intent(in)    :: fft_type
          logical(I32P),                                           intent(in)    :: verbose,callstack
          ! Locals
          type(DFT_DESCRIPTOR), pointer  :: handle => null()
          integer(I32P)                  :: stat_dont_care,caller_stat
          integer(I32P)                  :: distance
          ! Executable statments
          if(0_I32P /= status) status = 0_I32P
          status = DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_COMPLEX,dim_len,[data_len1,data_len2,data_len3])
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft3D_mtin: Fatal Local Error -- DftiCreateDescriptor failed with an error:", &
                                                   "fft3D_mtin: DftiCreateDescriptor -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiSetValue(handle,DFTI_NUMBERS_OF_TRANSFORMS,ntimes)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft3D_mtin: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                   "fft3D_mtin: DftiSetValue -- FAILED !!!", callstack,status)
              return
          end if
          distance = size(datum,dim=1) * size(datum,dim=2) * size(datum,dim=3)
          status = DftiSetValue(handle,DFTI_INPUT_DISTANCE,distance)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle, "fft3D_mtin: Fatal Local Error -- DftiSetValue failed with an error:", &
                                                   "fft3D_mtin: DftiSetValue -- FAILED !!!", callstack,status)
              return
          end if
          status = DftiCommitDescriptor(handle)
          if(0_I32P /= status) then
              call handle_descriptor_error(handle,  "fft3D_mtin: Fatal Local Error -- DftiCommitDescriptor failed with an error:", &
                                                    "fft3D_mtin: DftiCommitDescriptor -- FAILED !!!", callstack,status)
              return
          end if
          if(fft_type == TRANSFORM_FORWARD) then
              status = DftiComputeForward(handle,datum(:,1,1))
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle, "fft2D_mtin: Fatal Local Error -- DftiComputeForward failed with an error:", &
                                                       "fft2D_mtin: DftiComputeForward -- FAILED !!!", callstack,status)
                  return
              end if
          else if(fft_type == TRANSFORM_BACKWARD) then
              status = DftiComputeBackward(handle,datum(:,1,1))
              if(0_I32P /= status) then
                  call handle_descriptor_error(handle, "fft3D_mtin: Fatal Local Error -- DftiComputeBackward failed with an error:", &
                                                       "fft3D_mtin: DftiComputeBackward -- FAILED !!!", callstack,status)
                  return
              end if
          end if
          if(verbose == .true.) then
              call print_descriptor_state(handle,caller_stat)
          end if
          if(0_I32P == status) then
              stat_dont_care = DftiFreeDescriptor(handle)
          end if
    end subroutine
    !                        
    ! OMP - threaded versions
    ! Multi-Threading    operates on arrays 2D,
    ! where first dimension is a data and second dimension is
    ! a number of threads.
    ! 
                            
    subroutine fft_threaded_cipf(datum,data_len1,data_len2,dim_len,nthreads,status)
                                
          complex(R64P), dimension(data_len1,data_len2), intent(inout) :: datum
          !DIR$ ASSUME_ALIGNED datum:64
          integer(I32P),                                 intent(in)    :: data_len1, &
                                                                          data_len2, &
                                                                          dim_len,   &
                                                                          nthreads
          integer(I32P),                                 intent(inout) :: status
          ! Locals
          type(DFTI_DESCRIPTOR), pointer :: handle => null()
          
          integer(I32P) :: xth
          integer(I32P), dimension(2) :: length = [data_len1,data_len2]
          ! Executable statemetns
          !$OMP  PARALLEL DO SHARED(length,datum) PRIVATE(handle,status)
            do xth = 1, nthreads
                status = DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_COMPLEX,dim_len,length)
                status = DftiCommitDescriptor(handle)
                status = DftiComputeForward(handle,datum(:,xth))
                status = DftiFreeDescriptor(handle)
            end do
           !$OMP END PARALLEL DO
    end subroutine
                                
    subroutine fft_threaded_cipb(datum,data_len1,data_len2,dim_len,nthreads,status)
          complex(R64P), dimension(data_len1,data_len2), intent(inout) :: datum
          !DIR$ ASSUME_ALIGNED datum:64
          integer(I32P),                                 intent(in)    :: data_len1, &
                                                                          data_len2, &
                                                                          dim_len,   &
                                                                          nthreads
          integer(I32P),                                 intent(inout) :: status
          ! Locals
          type(DFTI_DESCRIPTOR), pointer :: handle => null()
          integer( I32P) :: xth
          integer( I32P), dimension(2) :: length = [data_len1,data_len2]
          ! Executable statemetns
          !$OMP PARALLEL DO SHARED(length,datum) PRIVATE(handle,status)
            do xth = 1, nthreads
                status = DftiCreateDescriptor(handle,DFTI_DOUBLE,DFTI_COMPLEX,dim_len,length)
                status = DftiCommitDescriptor(handle)
                status = DftiComputeBackward(handle,datum(:,xth))
                status = DftiFreeDescriptor(handle)
            end do
          !$OMP END PARALLEL DO
    end subroutine
                            
    subroutine print_descriptor_state(handle,stat)
          type(DFTI_DESCRIPTOR), pointer, intent(in)    :: handle
          integer(I32P),                  intent(inout) :: stat
         
          ! Locals
          integer(I32P)               :: precision,domain,rank,  &
                                         placement,nuthreads,    &
                                         ntransforms,            &
                                         indistance,outdistance, &
                                         commstat,status
              
          integer(I32P), dimension(5) :: lengths
          integer(I32P), dimension(6) :: instrides,outstrides
          real(R64P)                  :: fscale,bscale
          ! Executable statements
          
          print*, "    Dumping status of DFTI_DESCRIPTOR     "    
          status = DftiGetValue(handle,DFTI_PRECISION,precision)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else
              print*, " DFTI -- PRECISION: "
              if(DFTI_SINGLE == precision)  then
                  print*, "DFTI -- SINGLE_PRECISION"
              else if(DFTI_DOUBLE == precision) then
                  print*, "DFTI -- DOUBLE_PRECISION"
              end if
          end if
          status = DftiGetValue(handle,DFTI_FORWARD_DOMAIN,domain)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else 
              print*, "DFTI -- DOMAIN: "
              if(DFTI_COMPLEX == domain) then
                  print*, "DFTI -- DOMAIN_COMPLEX"
              else if(DFTI_REAL == domain) then
                  print*, "DFTI -- DOMAIN_REAL   "
              end if
          end if
          status = DftiGetValue(handle,DFTI_LENGTHS,lengths)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else
              print*, "DFTI_LENGTHS: ", lengths
          end if
          status = DftiGetValue(handle,DFTI_PLACEMENT,placement)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else
              print*, "DFTI_PLACEMENT: "
              if(DFTI_INPLACE == placement) then
                  print*, "DFTI_INPLACE "
              else if(DFTI_NOT_INPLACE == placement) then
                  print*, "DFTI_NOT_INPLACE "
              end if
          end if
          status = DftiGetValue(handle,DFTI_FORWARD_SCALE,fscale)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else
              print*, "DFTI_FORWARD_SCALE: ", fscale
          end if
          status = DftiGetValue(handle,DFTI_BACKWARD_SCALE,bscale)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else
              print*, "DFTI_BACKWARD_SCALE: ", bscale
          end if
          status = DftiGetValue(handle,DFTI_NUMBER_OF_USER_THREADS,nuthreads)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else
              print*, "DFTI_NUMBER_OF_USER_THREADS: ", nuthreads
          end if
          status = DftiGetValue(handle,DFTI_INPUT_STRIDES,instrides)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else 
              print*, "DFTI_INPUT_STRIDES: ", instrides
          end if
          status = DftiGetValue(handle,DFTI_OUTPUT_STRIDES,outstrides)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else
              print*, "DFTI_OUTPUT_STRIDES: ", outstrides
          end if
          status = DftiGetValue(handle,DFTI_NUMBER_OF_TRANSFORMS,ntransforms)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else
              print*, "DFTI_NUMBER_OF_TRANSFORMS: ",ntransforms
          end if
          status = DftiGetValue(handle,DFTI_COMMIT_STATUS,commstat)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else
              print*, "DFTI_COMMIT_STATUS: "
              if(DFTI_COMMITTED == commstat) then
                  print*, "DFTI_COMMITTED "
              else if (DFTI_UNCOMMITTED == commstat) then
                  print*, "DFTI_UNCOMMITTTED "
              end if
          endif
          status = DftiGetValue(handle,DFTI_INPUT_DISTANCE,indistance)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else
              print*, "DFTI_INPUT_DISTANCE: ", indistance
          end if
          status = DftiGetValue(handle,DFTI_OUTPUT_DISTANCE,outdistance)
          if(0_I32P /= status) then
              print*, "print_descriptor_state: Fatal Local Error -- DftiGetValue failed with an error:", status
              stat = status ! Copy state for the caller inspection.
              return
          else
              print*, "DFTI_OUTPUT_DISTANCE: ", outdistance
          end if
          print*, "     Endo of DFTI_DESCRIPTOR dump        "
    end subroutine
    
    subroutine handle_descriptor_error(handle,msg1,msg2,callstack,status)
          type(DFT_DESCRIPTOR), pointer :: handle
          character(len=*),     intent(in) :: msg1,msg2
          logical(I32P),        intent(in) :: callstack
          integer(I32P),        intent(in) :: status
          integer(I32P) :: stat_on_fail
          ! Executable statements
          print*, msg1, status
          stat_on_fail = DftiFreeDescriptor(handle)
          if(callstack == .true.) then
              call TRACEBACKQQ(STRING=msg2,USER_EXIT_CODE = -1)
          end if
    end subroutine
                         
                       

end module mod_mkl_fft
