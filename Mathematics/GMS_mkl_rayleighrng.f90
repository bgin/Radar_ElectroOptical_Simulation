
#include "Config.fpp"
 include 'mkl_vsl.f90'
 
 
module mod_mkl_rayleighrng

 
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_mkl_rayleighrng'
 !          
 !          Purpose:
 !                   Wrapper around Intel MKL Fortran    vdrngrayleigh subroutine
 !          History:
 !                        Date: 01-05-2018
 !                        Time: 10:11 GMT+2
 !          Modified:     
 !                        Date: 24-11-2018
 !                        Time: 12:28
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

     use mod_kinds,     only : int1, int4, dp
     use mkl_vsl_type
     use mkl_vsl
     implicit none
     private
     
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=int4), parameter, public :: MOD_MKL_RAYLEIGHRNG_MAJOR   = 1_int4
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_MKL_RAYLEIGHRNG_MINOR    = 0_int4
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_MKL_RAYLEIGHRNG_MICRO    = 0_int4
    
    ! Module full version
    integer(I32P), parameter, public :: MOD_MKL_RAYLEIGHRNG_FULLVER = 1000_int4*MOD_MKL_RAYLEIGHRNG_MAJOR + &
                                                              100_int4*MOD_MKL_RAYLEIGHRNG_MINOR  + &
                                                              10_int4*MOD_MKL_RAYLEIGHRNG_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_MKL_RAYLEIGHRNG_CREATE_DATE = "01-05-2018 10:11 +00200 (TUE 01 MAY 2018 GMT+2)" 
    
    ! Module build date (should be set after a successful compilation)
    character(*),  parameter, public :: MOD_MKL_RAYLEIGHRNG_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_MKL_RAYLEIGHRNG_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_MKL_RAYLEIGHRNG_DESCRIPT = "Fortran 2003 wrapper around Intel MKL vdrngrayleigh function."
    
!DIR$ IF .NOT. DEFINED (GMS_MKL_RAYLEIGHRNG_ADD_PADDING)
    !DIR$ DEFINE GMS_MKL_RAYLEIGHRNG_ADD_PADDING = 1
!DIR$ ENDIF
    
    !=======================================
    ! type: MKLRayleighRNG_t
    !=======================================
    type, public :: MKLRayleighRNG_t
        
           public
           sequence
           
           integer(kind=int4) :: m_nvalues
           
           integer(kind=int4) :: m_brng
           
           integer(kind=int4) :: m_seed
           
           integer(kind=int4) :: m_error
           
           real(kind=int4)    :: m_a
           
           real(kind=dp)      :: m_beta
           
!DIR$ IF (GMS_MKL_RAYLEIGHRNG_ADD_PADDING .EQ. 1)
           integer(kind=int1), dimension(36), private :: pad0
!DIR$ ENDIF
           
           ! public in order to eliminate copying procedures
!DIR$ ATTRIBUTES ALIGN : 64 :: m_rvec           
           real(kind=dp), allocatable, dimension(:) :: m_rvec
           
           contains
    
         
          
          !========================================
          !    Read/write procedures
          !========================================
          
          procedure, nopass, public :: write_state
          
          procedure, nopass, public :: read_state
          
          !=======================================
          !  Class helper procedures
          !=======================================
          
          procedure, pass(this), public ::  dbg_info
          
          !======================================
          !    Generic operators
          !======================================
          
          procedure, public :: copy_assign
          
          generic :: assignment (=) => copy_assign
          
          !=========================================
          !    Computational procedures
          !=========================================
          
          procedure, pass(this), public :: compute_rayleighrng1
          
          procedure, pass(this), public :: compute_rayleighrng2
          
        
    end type MKLRayleighRNG_t

    interface MKLRayleighRNG_t
    
        procedure :: constructor
        
    end interface MKLRayleighRNG_t
    
    contains
    
    !=================================================!
    !  @function: constructor                                          
    !  Initialization of object state.                                          
    !  Allocation and initialization to default values
    !  of real arrays
    !  @Warning
    !            Upon detection of non-fatal error
    !            variable 'err' will be set to -1
    !            Upon detection of fatal error like
    !            failed memory allocation 'STOP'
    !            will be executed.
    !            Values of variable 'err'
    !   1) -1 -- Object built already  or invalid argument
    !   2) -2 -- Invalid argument (any of them)  ! Not used here
    !=================================================!
    type(MKLRayleighRNG_t) function constructor(nvalues,brng,seed,a,beta, &
                                                logging,verbose,fname,append )
          use mod_print_error,   only : handle_fatal_memory_error
          use mod_constants,     only : pi_const, INITVAL
          integer(kind=int4),    intent(in) :: nvalues
          integer(kind=int4),    intent(in) :: brng
          integer(kind=int4),    intent(in) :: seed
          real(kind=dp),         intent(in) :: a
          real(kind=dp),         intent(in) :: beta
          logical(kind=int4),    intent(in) :: logging,verbose
          character(len=*),      intent(in) :: fname
          logical(kind=int4),    intent(in) :: append
          ! Locals
          character(len=256) :: emsg
          integer(kind=int4)      :: aerr
          ! Start of executable statements
          constructor%m_nvalues = nvalues
          constructor%m_brng    = brng
          constructor%m_seed    = seed
          constructor%m_error   = 1_int4
          constructor%m_a       = a
          constructor%m_beta    = beta
          associate(n=>constructor%m_nvalues)
               allocate(constructor%m_rvec(n), &
                        STAT=aerr,             &
                        ERRMSG=emsg )
          end associate
          if(aerr /= 0) then
              call handle_fatal_memory_error( logging,verbose,append,fname, &
                                             "logger: "// __FILE__ // "module: mod_mkl_rayleighrng, function: constructor -- Memory Allocation Failure !!", &                                                        &
                                             "module: mod_mkl_rayleighrng, function: constructor -- Memory Allocation Failure !!", &
                                              emsg,__LINE__)
          end if
          constructor%m_rvec = INITVAL
          
    end function constructor
    
   
    
 
    

    
    !============================================
    !   Read/write procedures
    !============================================
    
     subroutine read_state(this,form,unit,ioerr)
          implicit none
          class(MKLRayleighRNG_t),      intent(in)    :: this
          character(len=*),             intent(in)    :: form
          integer(kind=int4),           intent(in)    :: unit
          integer(kind=int4),           intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              READ(unit,*,iostat=ioerr) this
          case default
              READ(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine read_state
    
    subroutine write_state(this,form,unit,ioerr)
          implicit none
          class(MKLRayleighRNG_t),      intent(in)    :: this
          character(len=*),             intent(in)    :: form
          integer(kind=int4),           intent(in)    :: unit
          integer(kind=int4),           intent(inout) :: ioerr
          ! Start of executable statements
          select case(adjustl(trim(form)))
          case ("*")
              WRITE(unit,*,iostat=ioerr) this
          case default
              WRITE(unit,adjustl(trim(form)),iostat=ioerr) this
          end select
    end subroutine
    
    subroutine dbg_info(this)
          class(MKLRayleighRNG_t), intent(in) :: this
          ! Strat of executable ststements
          print*, "====================================================="
          print*, "     Dump of MKLRayleighRNG_t object state           "
          print*, "====================================================="
          print*, " Collected at date: ", __DATE__, "time: ", __TIME__
          print*, "====================================================="
          print*, " m_nvalues: ", this%m_nvalues
          print*, " m_brng:    ", this%m_brng
          print*, " m_seed:    ", this%m_seed
          print*, " m_error:   ", this%m_error
          print*, " m_a:       ", this%m_a
          print*, " m_beta:    ", this%m_beta
          print*, " m_rvec:    ", this%m_rvec
          print*, "====================================================="
    end subroutine dbg_info
    
    !====================================
    !  operator assignment (=)
    !====================================  
    subroutine copy_assign(lhs,rhs)
          class(MKLRayleighRNG_t), intent(inout) :: lhs
          class(MKLRayleighRNG_t), intent(in)    :: rhs
          ! Start of executable ststemetns
          lhs%m_nvalues = rhs%m_nvalues
          lhs%m_brng    = rhs%m_brng
          lhs%m_seed    = rhs%m_seed
          lhs%m_error   = rhs%m_error
          lhs%m_a       = rhs%m_a
          lhs%m_beta    = rhs%m_beta
          lhs%m_rvec    = rhs%m_rvec
    end subroutine copy_assign
    
    subroutine compute_rayleighrng1(this,method)
          use IFPORT, only :  TRACEBACKQQ
          class(MKLRayleighRNG_t),      intent(inout) :: this
          integer(kind=int4),           intent(in)    :: method
          ! Locals
!DIR$     ATTRIBUTES ALIGN : 64 :: stream
          type (VSL_STREAM_STATE) :: stream
          this%m_error = vslnewstream(stream,this%m_brng,this%m_seed)
          if(VSL_ERROR_OK /= this%m_error) then
              print*, "vslnewstream failed with an error: ", this%m_error
              call TRACEBACKQQ(STRING="vslnewstream failed",USER_EXIT_CODE=-1)
              return
          end if
          this%m_error = vdrngrayleigh(method,stream,this%m_nvalues,      &
                                   this%m_rvec,this%m_a,this%m_beta)
          if(VSL_ERROR_OK /= this%m_error) then
              print*, "vdrngrayleigh failed with an error: ", this%m_error
              call TRACEBACKQQ(STRING="vdrngrayleigh failed",USER_EXIT_CODE=-1)
              return
          end if
           this%m_error = vsldeletestream(stream)
          if(VSL_ERROR_OK /= this%m_error) then
              print*, "vsldeletestream failed with an error: ", this%m_error
              call TRACEBACKQQ(STRING="vsldeletestream failed",USER_EXIT_CODE=-1)
              return
          end if
          
    end subroutine compute_rayleighrng1
    
    subroutine compute_rayleighrng2(this,stream,method)
          use IFPORT, only :  TRACEBACKQQ
          class(MKLRayleighRNG_t),      intent(inout) :: this
          type(VSL_STREAM_STATE),       intent(inout) :: stream ! stream must be initialized by the caller of this subroutine
          integer(kind=int4),           intent(in)    :: method
          ! Start of executable statements
          this%m_error = vdrngrayleigh(method,stream.this%m_nvalues,this%m_rvec, &
                                       this%m_a,this%m_beta)
          if(VSL_ERROR_OK /= this%m_error) then
              print*, "vdrngrayleigh failed with an error: ", this%m_error
              call TRACEBACKQQ(STRING="vdrngrayleigh failed",USER_EXIT_CODE=-1)
              return
          end if
    end subroutine compute_rayleighrng2
    
    
end module mod_mkl_rayleighrng