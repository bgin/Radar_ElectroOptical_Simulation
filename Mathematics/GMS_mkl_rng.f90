
#include "Config.fpp"
include 'mkl_vsl.f90'

module mod_mkl_rng

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_mkl_rng'
 !          
 !          Purpose:
 !                   Wrapper around Intel MKL Fortran    VSL_STREAM_STATE functions
 !          History:
 !                        Date: 03-05-2018
 !                        Time: 12:23 GMT+2
 !                        Modified on:
 !                        Date: 24-11-2018
 !                        Time: 12:12 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 1
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
    
    use mod_kinds,      only : int1, int4
    use mkl_vsl_type
    use mkl_vsl
    implicit none
    private
    
    ! Major version
    integer(kind=int4), parameter, public :: MOD_MKL_RNG_MAJOR = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_MKL_RNG_MINOR = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_MKL_RNG_MICRO = 0_int4
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_MKL_RNG_FULLVER = 1000_int4*MOD_MKL_RNG_MAJOR + &
                                                                   100_int4*MOD_MKL_RNG_MINOR  + &
                                                                   10_int4*MOD_MKL_RNG_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_MKL_RNG_CREATE_DATE = "03-05-2018 12:27 +00200 (THR 03 MAY 2018 GMT+2)"
    
    ! Module build date (should be set after successful compilation date/time)
    character(*),  parameter, public :: MOD_MKL_RNG_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_MKL_RNG_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_MKL_RNG_DESCRIPT = "Fortran 2003 wrapper around Intel MKL VSL_STREAM_STATE functions."
    
!DIR$ IF .NOT. DEFINED (GMS_MKL_RNG_ADD_PADDING)
    !DIR$ DEFINE GMS_MKL_ADD_PADDING = 1
!DIR$ ENDIF
    
    !===================================
    !    type: MKLRandGen_t
    !===================================
    type, public :: MKLRandGen_t
        
          public
          
          sequence
          integer(kind=int4) :: m_method

          integer(kind=int4) :: m_brng

          integer(kind=int4) :: m_seed
          
          integer(kind=int4) :: m_error
          
          logical(kind=int4) :: m_isset
!DIR$ IF (GMS_MKL_RNG_ADD_PADDING .EQ. 1)
          integer(kind=int1), dimension(44), private :: pad0
!DIR$ ENDIF
          
          type(VSL_STREAM_STATE) :: m_stream
          
          contains
    
          !===================================
          !  Initialization, deinitialzation
          !===================================
    
          procedure, pass(this), public :: init_stream
          
          procedure, pass(this), public :: deinit_stream
          
         
          
    end type MKLRandGen_t
          
    contains
    
    subroutine init_stream(this,method,brng,seed)
           use IFPORT, only :  TRACEBACKQQ
           class(MKLRandGen_t),      intent(inout) :: this
           integer(kind=int4),       intent(in)    :: method,brng,seed
           ! Executable ststemtns
           this%m_method = method
           this%m_brng   = brng
           this%m_seed   = seed
           this%m_error  = 1_int4
           this%m_isset  = .false.
           this%m_error = vslnewstream(this%m_stream,this%m_brng,this%m_seed)
           if(VSL_ERROR_OK /= this%m_error) then
              print*, "vslnewstream failed with an error: ", this%m_error
              call TRACEBACKQQ(STRING="vslnewstream failed",USER_EXIT_CODE=-1)
              this%m_isset = .false.
              return
           end if
           this%m_isset = .true.
    end subroutine
    
    subroutine deinit_stream(this)
         use IFPORT, only :  TRACEBACKQQ
         class(MKLRandGen_t),   intent(inout) :: this
         ! Executable statemetns
         if(this%m_isset == .true. .AND.  &
            VSL_ERROR_OK == this%m_error ) then
                this%m_error = vsldeletestream(this%m_stream)
                if(VSL_ERROR_OK /= this%m_error) then
                     print*, "vsldeletestream failed with an error: ", this%m_error
                     call TRACEBACKQQ(STRING="vsldeletestream failed",USER_EXIT_CODE=-1)
                     return
                end if
                this%m_isset = .false.
         end if
    end subroutine        
             

    

   


end module mod_mkl_rng