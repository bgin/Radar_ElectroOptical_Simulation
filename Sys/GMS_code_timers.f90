
module mod_code_timers

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_code_timing'
 !          
 !          Purpose:
 !                    This module will be used to obtain crude and precise
 !                    performance of specific code block.
 !                    Crude and precise time measurement with the help
 !                    of 4 timers, additionaly time spent in process/thread
 !                    and kernel/user space is calculated by the calls to
 !                    kernal32 Fortran wrappers.
 !                    This module will be used mainly for performance tests
 !                    During such a test function or subroutine will be called
 !                    large number of time e.g 1000 in order to obtain as much
 !                    as possible running time samples which later be used to
 !                    compute basic statistical moments up to fourth.
 !                   
 !                     
 !          History:
 !                        Date: 02-08-2017
 !                        Time: 14:06 GMT+2
  !                       Date: 05-11-2019
 !                        Time: 16:27
 !          Version:
 !
 !                      Major: 2
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  Bernard Gingold
 !                 
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

   
    use module_kinds, only : i4,sp,dp

    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT , &
                                stdout=>OUTPUT_UNIT
    implicit none 
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=i4), parameter, public :: MOD_CODE_TIMERS_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MOD_CODE_TIMERS_MINOR = 0
    
    ! Micro version
    integer(kind=i4), parameter, public :: MOD_CODE_TIMERS_MICRO = 0
    
    ! Module/file full version
    integer(kind=i4), parameter, public :: MOD_CODE_TIMERS_FULLVER = 1000*MOD_CODE_TIMERS_MAJOR+100*MOD_CODE_TIMERS_MINOR + &
                                                                  10*MOD_CODE_TIMERS_MICRO
    
    ! Creation date
    character(*),  parameter, public :: MOD_CODE_TIMERS_CREATE_DATE = "02-08-2017 14:39 +00200 (WED 02 AUG 2017 GMT+2)"
    
    ! Build date (should be set to latest successfull build date/time)
    character(*),  parameter, public :: MOD_CODE_TIMERS_BUILD_DATE = "00-00-0000 00:00"
    
    ! Module/file author info
    character(*),  parameter, public :: MOD_CODE_TIMERS_AUTHOR = "Programmer: Bernard Gingold contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_CODE_TIMERS_DESCRIPT = "Crude and precise timing of code blocks."
    
    !================================================================70
    !  type: SCTimer_t
    !        Timer based on SYSTEM_CLOCK
    !================================================================70
    type, public :: SCTimer_t
        
       
        public
        character(len=32) :: m_tname  ! timer name
        
        integer(kind=int8b) :: m_on,m_off,m_delta
        
        integer(kind=i4) :: m_loc ! line of measured code.
        
        character(len=128) :: m_fpname ! File and procedure name
        
        logical(kind=i4)  :: m_isbuilt
        
    end type SCTimer_t
    
    !================================================================70
    !  type: CTTimer_t
    !        Timer based on CPU_TIME intrinsic
    !================================================================70
    type, public :: CTTimer_t
        
       
        public
        character(len=32) :: m_tname   ! timer name
        
        real(kind=sp) :: m_on,m_off,m_delta
        
        integer(kind=i4) :: m_loc  ! line of measured code.
        
        character(len=128) :: m_fpname  ! File and procedure name
        
        logical(kind=i4) :: m_isbuilt
        
    end type CTTimer_t
    
    !================================================================70
    ! type: DCTimer_t
    !       Timer based on DCLOCK intrinsic
    !================================================================70
    type, public :: DCTimer_t
        
       
        public 
        character(len=32) :: m_tname   ! timer name
        
        real(kind=dp) :: m_on,m_off,m_delta
        
        integer(kind=i4) :: m_loc  ! line of measured code.
        
        character(len=128) :: m_fpname   ! File and procedure name
        
    end type DCTimer_t
 
    
    contains
    
    !==============================================52
    !  ***********Implementation*************
    !==============================================52
    
    ! SCTimer_t object initialization.
    subroutine sctimer_init(this,tname,fpname)
          
          !
          type(SCTimer_t),   intent(inout) :: this
          character(len=*),  intent(in)    :: tname
          character(len=*),  intent(in)    :: fpname
          ! Start of executable statements
          if(this%m_isbuilt) then
              write(stderr,*) "sctimer_init: Timer is already initialized!"
              return
          end if
          this%m_tname   = tname
          this%m_on      = 0
          this%m_off     = 0
          this%m_delta   = 0
          this%m_loc     = 0
          this%m_fpname  = fpname
          this%m_isbuilt = .true.
    end subroutine
    
    ! SCTimer_t object destroy
    subroutine sctimer_destroy(this)
          
          type(SCTimer_t),  intent(inout) :: this
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              write(stderr,*) "sctimer_destroy: Timer has been destroyed!"
              return
          end if
          this%m_tname   = " "
          this%m_on      = -1
          this%m_off     = -1
          this%m_delta   = -1
          this%m_loc     = -1
          this%m_fpname  = " "
          this%m_isbuilt = .false.
    end subroutine
    
    ! SCTimer object reset
    subroutine sctimer_reset(this)
         
          type(SCTimer_t),  intent(inout) :: this
          ! Start of executable statements
          this%m_on    = 0
          this%m_off   = 0
          this%m_delta = 0
    end subroutine
    
    ! SCTimer object start measurement
    subroutine sctimer_start(this)
         
          type(SCTimer_t), intent(inout) :: this
          ! Start of executable statemetns
          call SYSTEM_CLOCK(this%m_on)
    end subroutine
    
    ! SCTimer_t object stop measurement
    subroutine sctimer_stop(this)
         
          type(SCTimer_t), intent(inout) :: this
          ! Start of executable statemetns
          call SYSTEM_CLOCK(this%m_off)
    end subroutine
    
    ! SCTimer_t object compute delta of measurements
    subroutine sctimer_delta(this,ifail)
         
          type(SCTimer_t),      intent(inout) :: this
          logical(kind=i4),   intent(out)   :: ifail
          ! Start of executable statements
          if((this%m_off-this%m_on).GT.0) then
              this%m_delta = this%m_off-this%m_on
          else
              this%m_delta = -2
              ifail = .true.
              return
          end if
    end subroutine

    !print SCTimer_t object
    subroutine sctimer_print(this)
         
          type(SCTimer_t), intent(in) :: this
          ! Start of executable statements
          write(stdout,*) "====================================================="
          write(stdout,*)   "1) Timer name: ",  this%m_tname
          write(stdout,10) this%m_on
10        format("2) start value=",I24.20)
          write(stdout,20) this%m_off
20        format("3) stop  value=",I24.20)
          write(stdout,30) this%m_delta
30        format("4) delta value=",I24.20)
          write(stdout,*)   "5) line of code: ", this%m_loc
          write(stdout,*)   "6) File name/procedure name: ",  this%m_fpname
          write(stdout,*) "====================================================="
    end subroutine
    
    ! CTTimer_t object initialiation
    subroutine cttimer_init(this,tname,fpname)
          
          type(CTTimer_t),  intent(inout) :: this
          character(len=*), intent(in)    :: tname,fpname
          ! Start of executable statements
          if(this%m_isbuilt) then
              write(stderr,*) "cttimer_init: CTTimer_t is already initialized!"
              return
          end if
          this%m_tname   = tname
          this%m_on      = 0.0_sp
          this%m_off     = 0.0_sp
          this%m_delta   = 0.0_sp
          this%m_loc     = -1
          this%m_fpname  = fpname
          this%m_isbuilt = .true.
    end subroutine
    
    ! CTTimer_t object destroy
    subroutine cttimer_destroy(this)
          !
          type(CTTimer_t),  intent(inout) :: this
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              write(stderr,*) "cttimer_destroy: CTTimer_t already destroyed!"
              return
          end if
          this%m_tname   = " "
          this%m_on      = -1.0_sp
          this%m_off     = -1.0_sp
          this%m_delta   = -1.0_sp
          this%m_loc     = -1
          this%m_fpname  = " "
          this%m_isbuilt = .false.
    end subroutine
    
    ! CTTimer_t object state reset
    subroutine cttimer_reset(this)
          !
          type(CTTimer_t), intent(inout) :: this
          ! Start of executable statements
          this%m_on    = 0.0_sp
          this%m_off   = 0.0_sp
          this%m_delta = 0.0_sp
    end subroutine
    
    ! CTTimer_t start measurement
    subroutine cttimer_start(this)
          !
          type(CTTimer_t), intent(inout) :: this
          ! Start of executable statements
          call CPU_TIME(this%m_on)
    end subroutine
    
    ! CTTimer_t stop measurement
    subroutine cttimer_stop(this)
          !
          type(CTTimer_t), intent(inout) :: this
          ! Start of executable statements
          call CPU_TIME(this%m_off)
    end subroutine
    
    ! CTTimer_t compute measurement delta
    subroutine cttimer_delta(this,ifail)
          !
          type(CTTimer_t), intent(inout) :: this
          logical(kind=i4),   intent(out)   :: ifail
          ! Start of executable statements
          if((this%m_off-this%m_on).GT.0.0_sp) then
              this%m_delta = this%m_off-this%m_on
          else
              this%m_delta = -2.0_sp
              ifail = .true.
          end if
    end subroutine
    
    ! CTTimer print object state
    subroutine cttimer_print(this)
          !
          type(CTTimer_t), intent(in) :: this
          ! Start of executable satements
          write(stdout,*) "==========================================="
          write(stdout,*) " 1) Timer  name: ", this%m_tname
          write(stdout,10) this%m_on
10        format(" 2) start value=",F10.6) 
          write(stdout,20) this%m_off
20        format(" 3) stop  value=",F10.6)
          write(stdout,30) this%m_delta
30        format(" 4) delta value=",F10.6)
          write(stdout,*) " 5) line of code:", this%m_loc
          write(stdout,*) " 6) File name/proc name: ", this%m_fpname
          write(stdout,*) "============================================"
    end subroutine
    
    ! DCTimer_t object initialiation
    subroutine dctimer_init(this,tname,fpname)
          ! 
          type(DCTimer_t),  intent(inout) :: this
          character(len=*), intent(in)    :: tname,fpname
          ! Start of executable statemetns
          if(this%m_isbuilt) then
              write(stderr,*) " dctimer_init: DCTimer_t already initialized"
              return
          end if
          this%m_tname   = tname
          this%m_on      = 0.0_dp
          this%m_off     = 0.0_dp
          this%m_delta   = 0.0_dp
          this%m_loc     = -1
          this%m_fpname  = fpname
          this%m_isbuilt = .true.
    end subroutine
    
    ! DCTimer_t  object destruction
    subroutine dctimer_destroy(this)
          !
          type(DCTimer_t), intent(inout) :: this
          ! Start of executable statements
          if(this%m_isbuilt .EQ. .false.) then
              write(stderr,*) " dctimer_destroy: DCTimer_t is already destroyed!!"
              return
          end if
          this%m_tname   = " "
          this%m_on      = -1.0_dp
          this%m_off     = -1.0_dp
          this%m_delta   = -1.0_dp
          this%m_loc     = -1
          this%m_fpname  = " "
          this%m_isbuilt = .false.
    end subroutine
    
    ! DCTimer_t reset measurement to 0.0
    subroutine dctimer_reset(this)
          !
          type(DCTimer_t), intent(inout) :: this
          ! Start of executable statements
          this%m_on    = 0.0_dp
          this%m_off   = 0.0_dp
          this%m_delta = 0.0_dp
    end subroutine
    
    ! DCTimer_t start measurement
    subroutine dctimer_start(this)
          use ifport
          !
          type(DCTimer_t), intent(inout) :: this
          ! Start of executable statements
          this%m_on = DCLOCK()
    end subroutine
    
    ! DCTimer_t stop measurement
    subroutine dctimer_stop(this)
          
          implicit none
          type(DCTimer_t), intent(inout) :: this
          ! Start of executable statements
          this%m_off = DCLOCK()
    end subroutine
    
    ! DCTimer measurememt delta
    subroutine dctimer_delta(this,ifail)
          !
          type(DCTimer_t), intent(inout) :: this
          logical(kind=i4),   intent(out)   :: ifail
          ! Start of executable statements
          ifail = .false.
          if((this%m_off-this%m_on).GT.0.0_dp) then
              this%m_delta = this%m_off-this%m_on
          else
              this%m_delta = -2.0_dp
              ifail = .true.
          end if
    end subroutine
    
    ! DCTimer_t print state
    subroutine dctimer_print(this)
          !
          type(DCTimer_t), intent(inout) :: this
          ! Start of executable statements
          write(stdout,*) "======================================"
          write(stdout,*) " 1) Timer name: ", this%m_tname
          write(stdout,10) this%m_on
10        format(" 2) start value=",F10.15)
          write(stdout,20) this%m_off
20        format(" 3) stop  value=",F10.15)
          write(stdout,30) this%m_delta
30        format(" 4) delta value=",F10.15)
          write(stdout,*)  " 5) line of code: ", this%m_loc
          write(stdout,*)  " 6) File name/proc name: ", this%m_fpname
    end subroutine
    
  
    
   
    
  
    
  
    
  
    
  
    
   
    
    
end module mod_code_timers
