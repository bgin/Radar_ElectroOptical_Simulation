
module mod_logevents
    
 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'module_logging_events'
 !          
 !          Purpose:
 !                        Declaration of logging events derived types. 
 !                        Following derived types are declared: 
 !                        1) PerfTimerEvent_t  
 !                        2) InvArgEvent_t 
 !                        3) FailAllocEvent_t  
 !                        4) IndexOutBoundsEvent_t
 !                        5) FailDeallocEvent_t
 !                        6) FileIOEvent_t
 !                        7) DisassociatedPtrEvent_t
 !                        8) FPTrapUndEvent_t
 !                        9) FPTrapOvfEvent_t
 !                        10) FPTrapDiv0Event_t
 !                        11) FPAbruptUndEvent_t
 !                        12) FPAbruptOvfEvent_t
 !                        13) FPAbruptDiv0Event_t
 !                        14) FPAbruptInvEvent_t
 !                        15) FPAbruptDmzEvent_t
 !                        16) FPPoleEvent_t   // domain error
 !                        17) FPSingularEvent_t  // domain error
 !                        
 !          History:
 !                      Date: 13-06-2017  -- Creation
 !                      Time: 13:42 GMT+2
 !                      Date: 31-10-2018  -- Modified
 !                      Time: 17:26 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                    
 !                      Bernard Gingold
 !
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !
 !          
 !
 !                       
 !==================================================================================85   


 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
    
     use mod_kinds,       only :  i4 , sp, dp
     use ISO_FORTRAN_ENV, only :  ERROR_UNIT
    
     implicit none
     
     public :: createPerfTimerEvent,       &
               destroyPerfTimerEvent,      &
               createInvArgEvent,          &
               destroyInvArgEvent,         &
               createFailAllocEvent,       &
               destroyFailAllocEvent,      &
               createIndexOutBoundsEvent,  &
               destroyIndexOutBoundsEvent, &
               createFailDeallocEvent,     &
               destroyFailDeallocEvent,    &
               createFileIOEvent,          &
               destroyFileIOEvent,         &
               createDisassocPtrEvent,     &
               destroyDisassocPtrEvent,    &
               createFPTrapUndEvent,       &
               destroyFPTrapUndEvent,      &
               createFPTrapOvfEvent,       &
               destroyFPTrapOvfEvent,      &
               createFPTrapDiv0Event,      &
               destroyFPTrapDiv0Event,     &
               createFPTrapInvEvent,       &
               destroyFPTrapInvEvent,      &
               createFPAbruptUndEvent,     &
               destroyFPAbruptUndEvent,    &
               createFPAbruptOvfEvent,     &
               destroyFPAbruptOvfEvent,    &
               createFPAbruptDiv0Event,    &
               destroyFPAbruptDiv0Event,   &
               createFPAbruptInvEvent,     &
               destroyFPAbruptInvEvent,    &
               createFPAbruptDmzEvent,     &
               destroyFPAbruptDmzEvent,    &
               createFPPoleEvent,          &
               destroyFPPoleEvent,         &
               createFPSingularEvent,      &
               destroyFPSingularEvent,     &
               createFPDomErrEvent,      &
               destroyFPDomErrorEvent,     &
               createWinErrorEvent,        &
               destroyWinErrorEvent
     
               
    !============================================50
    !        File/Module version info
    !============================================50
    
    ! File major version
    integer(kind=i4),  parameter, public  :: mod_logevents_major = 1_i4
    
    ! File minor version
    integer(kind=i4),  parameter, public :: mod_logevents_minor = 0_i4
    
    ! File micro(patch) version
    integer(kind=i4),  parameter, public :: mod_logevents_micro = 0_i4
    
    ! File full version
    integer(kind=i4),  parameter, public :: mod_logevents_fullver = 1000_i4*mod_logevents_major+100_i4*mod_logevents_minor + &
                                                                        10_i4*mod_logevents_micro
    
    ! Creation date
    character(*),        parameter, public  :: mod_logevents_creation_date="13-06-2017 13:42 +00200 (Tue 13 June 2017 GMT+2)"
    
    ! File build date/time , should be set manually after every recompilation
    character(*),        parameter, public  :: mod_logevents_build_date=" "
    
   
    
    ! Module Author
    character(*),        parameter, public  :: mod_logevents_author="Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),        parameter, public  :: mod_logevents_synopsis="Logging events derived types."
    
    !============================================50
    ! Declaration/Definition of derived type
    !============================================50
    
    
    !============================================50
    !  Type: PerfTimerEvent_t
    !============================================50
    
    type, public :: PerfTimerEvent_t
        
       
       
       
        ! Event name
        character(len=11) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
        ! File name  of measured event (top of hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80)  :: m_module_name
        
       
        ! Procedure name
        character(len=80)  :: m_proc_name
        
        ! Line of code(start,end) (bottom of hierarchy)
        integer(kind=i4)     :: m_line_st, m_line_en
        
        ! Date of event  as returned by date_and_time
        character(len=40)      :: m_date
        
        ! Time of event  as returned by date_and_time
        character(len=40)      :: m_time
        
        ! Number of timing runs
        integer(kind=i4)     :: m_nruns
        
        ! Allocatable array of type real(kind=4)
        ! for storing time measurement returned by CPU_TIME
        ! m_timings(1,:) - stores starting values
        ! m_timings(2,:) - stores ending values
        ! m_timings(3,:) - stores delta values
        ! Notification:
        !               Caller is responsible for array allocation/deallocation
        !DIR$ ATTRIBUTES ALIGN : 64 :: m_ctimings
        real(kind=sp), allocatable, dimension(:,:) :: m_ctimings
        
        ! Allocatable array of type integer(kind=8)
        ! for storing time measurement returned by SYSTEM_CLOCK
        ! m_timings(1,:) - stores starting values
        ! m_timings(2,:) - stores ending values
        ! m_timings(3,:) - stores delta values
        ! Notification:
        !               Caller is responsible for array allocation/deallocation
        !DIR$ ATTRIBUTES ALIGN : 64 :: m_sctimings
        integer(kind=8), allocatable, dimension(:,:) :: m_sctimings
        
        ! Allocatable array of type real(kind=8)
        ! for storing time measurement returned by DCLOCK
        ! m_timings(1,:) - stores starting values
        ! m_timings(2,:) - stores ending values
        ! m_timings(3,:) - stores delta values
        ! Notification:
        !               Caller is responsible for array allocation/deallocation
        !DIR$ ATTRIBUTES ALIGN : 64 :: m_dctimings
        real(kind=dp), allocatable, dimension(:,:) :: m_dctimings
        
        ! Allocatable array of type real(kind=8)
        ! for storing time measurement returned by QueryPerformanceCounter WIN API wrapper
        ! m_timings(1,:) - stores starting values
        ! m_timings(2,:) - stores ending values
        ! m_timings(3,:) - stores delta values
        ! Notification:
        !               Caller is responsible for array allocation/deallocation
        !DIR$ ATTRIBUTES ALIGN : 64 :: m_qpctimings
        type(T_LARGE_INTEGERX), allocatable, dimension(:,:) :: m_qpctimings
        
        ! Timers results delta.
        !DIR$ ATTRIBUTES ALIGN : 64 :: m_timers_delta
        real(kind=dp), allocatable, dimension(:,:)  :: m_timers_delta
        
        ! Average value of starting measurements
        real(kind=dp)         :: m_actimings,m_asctimings, &
                              m_adctimings,m_aqpctimings
        
        ! Average value of stoping measurements
        real(kind=dp)         :: m_actiminge,m_asctiminge, &
                              m_adctiminge,m_aqpctiminge
        
        ! Average value of delta measurements
        real(kind=dp)         :: m_actimingd,m_asctimingd, &
                              m_adctimingd,m_aqpctimingd
        
        ! Clock facility used to perform time measurement
        ! Position        1 - CPU_TIME * supported
        !   -||-          2 - SYSTEM_CLOCK  * supportd
        !   -||-          3 - C intrinsic like 'rdtsc' ..etc  (experimental)
        !   -||-          4 - DCLOCK * supported
        !   -||-          5 - QueryPerformanceCounter * supprted
        !   -||-          6 - OpenMP timing procedures (experimental)
        character(len=40), dimension(6)     :: m_timer_type
        
        ! Number of loop cycles needed to warm the CPU
        integer(kind=i4)     :: m_nwcycles
        
        ! Number of code hot-spots
        integer(kind=i4)     :: m_nhotspots
        
        ! Highest hotspot in term of CPU consumption
        real(kind=dp)        :: m_maxhotspot
        
        ! Number of GFLOPS per hotspot
        real(kind=dp)        :: m_gflops
        
         ! Is this event built(initilaized)
        logical(kind=i4)     :: m_isbuilt
        
    end type  PerfTimerEvent_t
    
    !============================================50
    ! Type: InvArgEvent_t
    !============================================50
    
    type, public :: InvArgEvent_t
        
      
       
        ! Event name
        character(len=13)   :: m_event_name
        
        ! Custom message
        character(len=80)   :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80)   :: m_file_name
        
        ! Module name
        character(len=80)   :: m_module_name
        
       
        
        ! Procedure name
        character(len=80)   :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)  :: m_line_st,m_line_en
        
        ! Date of event  as returned by date_and_time
        character(len=40)   :: m_date
        
        ! Time of event as returned by_date_and_time
        character(len=40)   :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(kind=i4)       :: m_severity
        
        ! Invalid argument(s)  scalar members
        ! are initialized to invalid value(s)
        ! other members of floating-point type 
        ! are initilized to Nan values and integers
        ! are initilized to Huge(integer(kind=4)) or
        ! Huge(integer(kind=8)) values.
        integer(kind=i4)     :: m_invargi32,m_invargi64
        
        real(kind=sp)          :: m_invargf32,m_invargf64
        
        ! In case of invalid argument of derived type
        ! then string containing derived type name
        ! is initiliazed by the caller.
        character(len=256) :: m_descript
        
         ! Is this event built(initilaized)
        logical(kind=i4)     :: m_isbuilt
        
    end type  InvArgEvent_t
    
    !============================================50
    ! Type: FailAllocEvent_t
    !============================================50
    
    type, public :: FailAllocEvent_t
        
        
        
        ! Name of event
        character(len=16)        :: m_event_name
        
        ! Custom message
        character(len=80)        :: m_msg
        
        ! File name (top of event hierrarchy)
        character(len=80)        :: m_file_name
        
        ! Module name
        character(len=80)        :: m_module_name
        
       
        
        ! Procedure name
        character(len=80)        :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)       :: m_line_st,m_line_en
        
        ! Date of event as returned by date_and_time
        character(len=40)        :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)        :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(kind=i4)       :: m_severity
        
        ! Allocation 'STAT' value
        integer(kind=i4)       :: m_astat
        
        ! ERRMSG
        character(len=80)        :: m_errmsg
        
         ! Is this event built(initilaized)
        logical(kind=i4)       :: m_isbuilt
        
    end type  FailAllocEvent_t
    
    !============================================50
    ! Type: IndexOutBoundsEvent_t
    !============================================50
    
    type, public :: IndexOutBoundsEvent_t
        
       
        
        ! Event name
        character(len=21)                  :: m_event_name
        
        ! Custom message
        character(len=80)                  :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80)                  :: m_file_name
        
        ! Module name
        character(len=80)                  :: m_module_name
        
      
        
        ! Procedure name
        character(len=80)                  :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)                 :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)                  :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)                  :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(kind=i4)                  :: m_severity
        
        ! Integer(kind=4) array which holds expected and actual values i.e.
        ! (array indices)
        ! 1st dimension - expected values
        ! 2nd dimension - actual   values
        integer(kind=i4), dimension(2,31) :: m_indices32
        
         
        
         ! Is this event built(initilaized)
        logical(kind=i4)                  :: m_isbuilt
        
    end type  IndexOutBoundsEvent_t
    
    !============================================50
    ! Type: FailDeallocEvent_t
    !============================================50
    
    type, public :: FailDeallocEvent_t
        
       
        
        ! Event name
        character(len=18)      :: m_event_name
        
        ! Custom message
        character(len=80)      :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80)      :: m_file_name
        
        ! Module name
        character(len=80)      :: m_module_name
        
    
        ! Procedure name
        character(len=80)      :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)      :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)      :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(kind=i4)     :: m_severity
        
        ! STAT value
        integer(kind=i4)     :: m_dstat
        
        ! ERRMSG
        character(len=80)      :: m_errmsg
        
         ! Is this event built(initilaized)
        logical(kind=i4)     :: m_isbuilt
        
    end type  FailDeallocEvent_t
    
    !============================================50
    ! Type: FileIOEvent_t
    ! Remark: This event describes a failed
    !          file I/O operation.
    !============================================50
    
    type, public :: FileIOEvent_t
        
       
        
        ! Event name
        character(len=13)      :: m_event_name
        
        ! Custom message
        character(len=80)      :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80)      :: m_file_name
        
        ! Module name
        character(len=80)      :: m_module_name
        
       
        
        ! Procedure name
        character(len=80)      :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)      :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)      :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(kind=i4)     :: m_severity
        
        ! Failed I/O operation - name
        character(len=24)      :: m_fileop
        
        ! IOSTAT
        integer(kind=i4)     :: m_iostat
        
        ! IOMSG
        character(len=80)      :: m_iomsg
        
         ! Is this event built(initilaized)
        logical(kind=i4)     :: m_isbuilt
        
    end type   FileIOEvent_t
    
    !============================================50
    ! Type: DisassocPtrEvent_t
    !============================================50
    
    type, public :: DisassocPtrEvent_t
        
       
        
        ! Event name
        character(len=24)      :: m_event_name
        
        ! Custom message
        character(len=80)      :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80)      :: m_file_name
        
        ! Module name
        character(len=80)      :: m_module_name
     
        
        ! Procedure name
        character(len=80)      :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)      :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)      :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(kind=i4)      :: m_severity
        
        ! Pointer association status
        logical(kind=i4)      :: m_status
        
         ! Is this event built(initilaized)
        logical(kind=i4)      :: m_isbuilt
        
    end type  DisassocPtrEvent_t
    
    !============================================50
    ! Type: FPTrapUndEvent_t
    !============================================50
    
    type :: FPTrapUndEvent_t
        
      
        
        ! Event name
        character(len=16)      :: m_event_name
        
        ! Custom message
        character(len=80)      :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80)      :: m_file_name
        
        ! Module name
        character(len=80)      :: m_module_name
        
     
        
        ! Procedure name
        character(len=80)      :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)      :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)      :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(kind=i4)      :: m_severity
        
        ! Value of fp variable encoded as a real(kind=8)
        real(kind=dp)           :: m_undval
        
        ! Value of FP-Env status flags
        integer(kind=i4)      :: m_flags
        
         ! Is this event built(initilaized)
        logical(kind=i4)      :: m_isbuilt
        
    end type  FPTrapUndEvent_t
    
    !============================================50
    ! Type: FPTrapOvfEvent_t
    !============================================50
    
    type :: FPTrapOvfEvent_t
        
        
        
        ! Event name
        character(len=16)      :: m_event_name
        
        ! Custom message
        character(len=80)      :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80)      :: m_file_name
        
        ! Module name
        character(len=80)      :: m_module_name
        
      
        
        ! Procedure name
        character(len=80)      :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)      :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)      :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Do not care (DC)
        integer(kind=i4)     :: m_severity
        
        ! Overflow value encoded as a real(kind=8) (its max value).
        real(kind=dp)          :: m_ovfval
        
        ! FP-Env status flag at exception moment
        integer(kind=i4)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(kind=i4)     :: m_isbuilt
        
    end type  FPTrapOvfEvent_t
    
    !============================================50
    ! Type: FPTrapDiv0Event_t
    !============================================50
    
    type, public :: FPTrapDiv0Event_t
        
       
        
        ! Event name
        character(len=17)      :: m_event_name
        
        ! Custon message
        character(len=80)      :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80)      :: m_file_name
        
        ! Module name
        character(len=80)      :: m_module_name
        
      
        
        ! Procedure name
        character(len=80)      :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)     :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)     :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(kind=i4)     :: m_severity
        
        ! Result of calculation (if possible to obtain)
        real(kind=dp)          :: m_div0val
        
        ! FP-Env flags
        integer(kind=i4)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(kind=i4)     :: m_isbuilt
        
    end type   FPTrapDiv0Event_t
    
     
    !============================================50
    ! Type: FPTrapInvEvent_t
    !============================================50
    
    type, public :: FPTrapInvEvent_t
        
       
        
        ! Event name
        character(len=16)      :: m_event_name
        
        ! Custom message
        character(len=80)      :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80)      :: m_file_name
        
        ! Module name
        character(len=80)      :: m_module_name
        
      
        
        ! Procedure name
        character(len=80)      :: m_proc_name
        
         ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)      :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)     :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(kind=i4)     :: m_severity
        
        ! Value of Inv variable (if possible to obtain)
        real(kind=dp)          :: m_inval
        
        ! FP-Env flags
        integer(kind=i4)     :: m_flags
        
        ! Is this event built(initilaized)
        logical(kind=i4)     :: m_isbuilt
        
    end type  FPTrapInvEvent_t
    
    
    !============================================50
    ! Type: FPAbruptUndEvent_t
    !============================================50
    
    type, public :: FPAbruptUndEvent_t
        
       
        
        ! Event name
        character(len=18)      :: m_event_name
        
        ! Custom message
        character(len=80)      :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80)      :: m_file_name
        
        ! Module name
        character(len=80)      :: m_module_name
        
      
        ! Procedure name
        character(len=80)      :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)      :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)      :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(kind=i4)          :: m_severity
        
        ! Value of UND variable (if possible to obtain)
        real(kind=dp)               :: m_undval
        
        ! FP-Env flags
        integer(kind=i4)          :: m_flags
        
         ! Is this event built(initilaized)
        logical(kind=i4)          :: m_isbuilt
        
    end type FPAbruptUndEvent_t
    
    !============================================50
    ! Type: FPAbruptOvfEvent_t
    !============================================50
    
    type :: FPAbruptOvfEvent_t
        
       
        ! Event name
        character(len=18) :: m_event_name
        
        ! Custom message
        character(len=80) :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80) :: m_file_name
        
        ! Module name
        character(len=80) :: m_module_name
        
      
        ! Procedure name
        character(len=80)          :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)         :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)          :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)          :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(kind=i4)          :: m_severity
        
        ! Value of overflown variable 
        real(kind=dp)               :: m_ovfval
        
        ! FP-Env flags
        integer(kind=i4)          :: m_flags
        
         ! Is this event built(initilaized)
        logical(kind=i4)          :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FPAbruptDiv0Event_t
    !============================================50
    
    type :: FPAbruptDiv0Event_t
        
        
       
        
        ! Event name
        character(len=19)            :: m_event_name
        
        ! Custom message
        character(len=80)            :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80)            :: m_file_name
        
        ! Module name
        character(len=80)            :: m_module_name
        
     
        
        ! Procedure name
        character(len=80)            :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)           :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)            :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)            :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(kind=i4)           :: m_severity
        
        ! Value of variable after attempted div by zero
        real(kind=dp)                :: m_div0val
        
        ! FP-Env flags
        integer(kind=i4)          :: m_flags
        
         ! Is this event built(initilaized)
        logical(kind=i4)          :: m_isbuilt
        
    end type  FPAbruptDiv0Event_t
    
    !============================================50
    ! Type: FPAbruptInvEvent_t
    !============================================50
    
    type, public :: FPAbruptInvEvent_t
        
       
        
        ! Event name
        character(len=18)     :: m_event_name
        
        ! Custom message
        character(len=80)     :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80)     :: m_file_name
        
        ! Module name
        character(len=80)     :: m_module_name
        
       
        
        ! Procedure name
        character(len=80)      :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)      :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)     :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(kind=i4)     :: m_severity
        
        ! Invalid fp value (if possible to obtain)
        real(kind=dp)          :: m_inval
        
        ! FP-Env flags
        integer(kind=i4)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(kind=i4)     :: m_isbuilt
        
    end type  FPAbruptInvEvent_t
    
    !============================================50
    ! Type: FPAbruptDmzEvent_t
    !============================================50
    
    type, public :: FPAbruptDmzEvent_t
        
       
        
        ! Event name
        character(len=18)      :: m_event_name
        
        ! Custom message
        character(len=80)      :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80)      :: m_file_name
        
        ! Module name
        character(len=80)      :: m_module_name
        
     
        
        ! Procedure name
        character(len=80)      :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)      :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)      :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(kind=i4)     :: m_severity
        
        ! Denormal value of variable (encoded as REAL(kind=8)
        real(kind=dp)          :: m_denval
        
        ! FP-Env flags
        integer(kind=i4)     :: m_flags
        
        ! Is this event built(initilaized)
        logical(kind=i4)     :: m_isbuilt
        
    end type
    
    !============================================50
    ! Type: FPPoleEvent_t
    !============================================50
    
    type, public :: FPPoleEvent_t
        
       
       
        
        ! Event name
        character(len=13)          :: m_event_name
        
        ! Custom message
        character(len=80)          :: m_msg
        
        ! File name (top of event hierarchy)
        character(len=80)          :: m_file_name
        
        ! Module name
        character(len=80)          :: m_module_name
        
      
        ! Procedure name
        character(len=80)          :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)         :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)          :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)          :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(kind=i4)          :: m_severity
        
        ! Pole value
        real(kind=dp)               :: m_polval
        
        ! FP-Env flags 
        integer(kind=i4)          :: m_flags
        
         ! Is this event built(initilaized)
        logical(kind=i4)          :: m_isbuilt
        
    end type  FPPoleEvent_t
    
    !============================================50
    ! Type: FPSingularEvent_t
    !============================================50
    
    type :: FPSingularEvent_t
        
        
         
        ! Event name
         character(len=17)     :: m_event_name
         
        ! Custom message
         character(len=80)     :: m_msg
         
        ! File name (top of event hierarchy)
        character(len=80)      :: m_file_name
        
        ! Module name
        character(len=80)      :: m_module_name
        
       
        
        ! Procedure name
        character(len=80)      :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)     :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)      :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)     :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(kind=i4)     :: m_severity
        
        ! Singularity value of variable
        real(kind=dp)          :: m_singval
        
        ! FP-Env flags
        integer(kind=i4)     :: m_flags
        
         ! Is this event built(initilaized)
        logical(kind=i4)     :: m_isbuilt
         
    end type  FPSingularEvent_t
    
    !============================================50
    ! Type: FPDomErrEvent_t
    !============================================50
    
    type :: FPDomErrEvent_t
        
        
        
        ! Event name
        character(len=15)          :: m_event_name
        
        ! User custom message
        character(len=80)          :: m_msg
        
         ! File name (top of event hierarchy)
        character(len=80)          :: m_file_name
        
        ! Module name
        character(len=80)          :: m_module_name
        
      
        
        ! Procedure name
        character(len=80)          :: m_proc_name
        
        ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=i4)         :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)          :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)          :: m_time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(kind=i4)         :: m_severity
        
        ! Domain error value
        real(kind=dp)              :: m_errval
        
        ! Domain specific value (upper/lower bound)
        real(kind=dp)              :: m_domval
        
        ! FP-Env flags
        integer(kind=i4)         :: m_flags
        
         ! Is this event built(initilaized)
        logical(kind=i4)         :: m_isbuilt
        
    end type  FPDomErrEvent_t
    
   
    
    !============================================50
    !  Implementation of event creation
    !  subroutines.
    !============================================50
    
   
    
   
    
    contains
    
    !====================================================59
    ! subroutine:
    !             createPerfTimerEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: PerfTimerEvent_t
    !  Notification:
    !                
    !=====================================================59
    subroutine createPerfTimerEvent(event,evname,umsg,fname,             &
                                    modname,procname,locs,loce,          &
                                    nruns,ctimings,sctimings,            &
                                    dctimings,qpctimings,timersd,        &
                                    actimings,asctimings,adctimings,     &
                                    aqpctimings,actiminge,asctiminge,    &
                                    adctiminge,aqpctiminge,actimingd,   &
                                    asctimingd,adctimingd,aqpctimingd,&
                                    timert,nwcycles,nhotspots,maxhotspot,&
                                    gflops,errstat                               )
          
         
          
          type(PerfTimerEvent_t),                       intent(inout) :: event
          character(len=*),                             intent(in)    :: evname,umsg,fname, &
                                                                         modname,procname
          integer(kind=i4),                           intent(in)    :: locs,loce,nruns
          real(kind=sp),          allocatable,             dimension(:,:),   intent(in)    :: ctimings
          integer(kind=8),        allocatable,            dimension(:,:),   intent(in)    :: sctimings
          real(kind=dp),          allocatable,           dimension(:,:),   intent(in)    :: dctimings
          type(T_LARGE_INTEGERX), allocatable,           dimension(:,:),   intent(in)    :: qpctimings
          real(kind=dp),          allocatable,          dimension(:,:),   intent(in)    :: timersd
          real(kind=dp),                                intent(in)    :: actimings
          real(kind=dp),                                intent(in)    :: asctimings
          real(kind=dp),                                intent(in)    :: adctimings
          real(kind=dp),                                intent(in)    :: aqpctimings
          real(kind=dp),                                intent(in)    :: actiminge
          real(kind=dp),                                intent(in)    :: asctiminge
          real(kind=dp),                                intent(in)    :: adctiminge
          real(kind=dp),                                intent(in)    :: aqpctiminge
          real(kind=dp),                                intent(in)    :: actimingd
          real(kind=dp),                                intent(in)    :: asctimingd
          real(kind=dp),                                intent(in)    :: adctimingd
          real(kind=dp),                                intent(in)    :: aqpctimingd
          character(len=40), dimension(6),              intent(in)    :: timert
          integer(kind=i4),                           intent(in)    :: nwcycles,nhotspots
          real(kind=dp),                                intent(in)    :: maxhotspot,gflops
          logical(kind=i4),                           intent(inout)    :: errstat
          ! Locals
          character(len=10)  ::  t
          character(len=8)   ::  d
          ! Start of executable statements
          ! Sanity checcking on errstat argument
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          if((allocated(ctimings)  .EQ.  .false.) .OR. &
             (allocated(sctimings) .EQ.  .false.) .OR. &
             (allocated(dctimings) .EQ.  .false.) .OR. &
             (allocated(qpctimings).EQ.  .false.) .OR. &
             (allocated(timersd)   .EQ.  .false.)     ) then
               WRITE(ERROR_UNIT,*) "createPerfTimeEvent: [FATAL-ERROR]: Unallocated array passed as an argument!!"
               errstat = .true.
               return
          end if
              
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
           
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_en     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_nruns       = nruns
             event%m_ctimings    = ctimings
             event%m_sctimings   = sctimings
             event%m_dctimings   = dctimings
             event%m_qpctimings  = qpctimings
             event%m_timers_delta= timersd
             event%m_actimings   = actimings
             event%m_asctimings  = asctimings
             event%m_adctimings  = adctimings
             event%m_aqpctimings = aqpctimings
             event%m_actiminge   = actiminge
             event%m_asctiminge  = asctiminge
             event%m_adctiminge  = adctiminge
             event%m_aqpctiminge = aqpctiminge
             event%m_actimingd   = actimingd
             event%m_asctimingd  = asctimingd
             event%m_adctimingd  = adctimingd
             event%m_aqpctimingd = aqpctimingd
             event%m_timer_type  = timert
             event%m_nwcycles    = nwcycles
             event%m_nhotspots   = nhotspots
             event%m_maxhotspot  = maxhotspot
             event%m_gflops      = gflops
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createPerfTimerEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                    
    subroutine destroyPerfTimerEvent(event,errstat)
          type(PerfTimerEvent_t),       intent(inout) :: event
          logical(kind=i4),           intent(inout) :: errstat
          ! Exec code
          if(errstat) errstat = .false.
          if(event%m_isbuilt == .false.) then
              errstat = .true.
              write(ERROR_UNIT,*) "================================================="
              write(ERROR_UNIT,*) "  destroyPerfTimerEvent: [FATAL-ERROR]           "
              write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
              write(ERROR_UNIT,*) "================================================="
              return
          end if
           event%m_event_name  = " "
           event%m_msg         = " "
           event%m_file_name   = " "
           event%m_module_name = " "
         
           event%m_proc_name   = " "
           event%m_line_st     = -1
           event%m_line_en     = -1
           event%m_date        = " "
           event%m_time        = " "
           event%m_nruns       = -1
           if(allocated(event%m_ctimings))     deallocate(event%m_ctimings)
           if(allocated(event%m_sctimings))    deallocate(event%m_sctimings)
           if(allocated(event%m_dctimings))    deallocate(event%m_dctimings)
           if(allocated(event%m_qpctimings))   deallocate(event%m_qpctimings)
           if(allocated(event%m_timers_delta)) deallocate(event%m_timers_delta)
           event%m_actimings   = -1.0_dp
           event%m_asctimings  = -1.0_dp
           event%m_adctimings  = -1.0_dp
           event%m_aqpctimings = -1.0_dp
           event%m_actiminge   = -1.0_dp
           event%m_asctiminge  = -1.0_dp
           event%m_adctiminge  = -1.0_dp
           event%m_aqpctiminge = -1.0_dp
           event%m_actimingd   = -1.0_dp
           event%m_asctimingd  = -1.0_dp
           event%m_adctimingd  = -1.0_dp
           event%m_aqpctimingd = -1.0_dp
           event%m_timer_type  = " "
           event%m_nwcycles    = -1
           event%m_nhotspots   = -1
           event%m_maxhotspot  = -1
           event%m_gflops      = 0.0_dp
           event%m_isbuilt     = .false.
    end subroutine destroyPerfTimerEvent
   
          
        
    !====================================================59
    ! subroutine:
    !             createInvArgEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: InvArgEvent_t
    !  Notification:
    !                Indicates passing of invalid scalar,
    !                derived type argument or array argument
    !=====================================================59
    subroutine createInvArgEvent(event,evname,umsg,fname, &
                                 modname,procname,locs,loce, &
                                 severity,invargi32,invargi64, &
                                 invargf32,invargf64, arg_type,errstat )
          use, intrinsic :: IEEE_ARITHMETIC
          
          type(InvArgEvent_t),       intent(inout)        :: event
          character(len=*),          intent(in)           :: evname,umsg,fname, &
                                                             modname,procname
          integer(kind=i4),        intent(in)           :: locs,loce,severity
          integer(kind=i4),        intent(in), optional :: invargi32
          integer(kind=8),           intent(in), optional :: invargi64
          real(kind=sp),             intent(in), optional :: invargf32
          real(kind=dp),             intent(in), optional :: invargf64
          character(len=*),          intent(in), optional :: arg_type
          logical(kind=i4),        intent(inout)        :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
            
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_en     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             if(present(invargi32)) then
                event%m_invargi32 = invargi32
             else
                event%m_invargi32 = HUGE(1_4)
             end if
             if(present(invargi64)) then
                event%m_invargi64 = invargi64
             else
                event%m_invargi64 = HUGE(1_8)
             end if
             if(present(invargf32)) then
                event%m_invargf32 = invargf32
             else
                event%m_invargf32 = IEEE_VALUE(1.0_sp,IEEE_QUIET_NAN)
             end if
             if(present(invargf64)) then
                event%m_invargf64 = invargf64
             else
                event%m_invargf64 = IEEE_VALUE(1.0_dp,IEEE_QUIET_NAN)
             end if
             if(present(arg_type)) then
                event%m_descript = arg_type
             else
                event%m_descript = "NONE"
             endif
             event%m_isbuilt     = .true.
          else
              errstat            = .true.
              write(ERROR_UNIT,*) "====================================================="
              write(ERROR_UNIT,*) "   createInvArgEvent: [FATAL-ERROR]"
              write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
              write(ERROR_UNIT,*) "====================================================="
              return
          end if
          
    end subroutine
                                 
    subroutine destroyInvArgEvent(event,errstate)
          use, intrinsic :: IEEE_ARITHMETIC 
          type(InvArgEvent_t),      intent(inout) :: event
          logical(kind=i4),       intent(inout) :: errstate
          ! Exec code ....
          if(errstate) errstate = .false.
          if(event%m_isbuilt == .false.) then
                errstate = .true.
                write(ERROR_UNIT,*) "================================================="
                write(ERROR_UNIT,*) "  destroyInvArgEvent: [FATAL-ERROR]           "
                write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                write(ERROR_UNIT,*) "================================================="
                return
          end if
           event%m_event_name  = " "
           event%m_msg         = " "
           event%m_file_name   = " "
           event%m_module_name = " "
         
           event%m_proc_name   = " "
           event%m_line_st     = -1
           event%m_line_en    = -1
           event%m_date        = " "
           event%m_time        = " "
           event%m_severity    = -1
           event%m_invargi32   = -1
           event%m_invargi64   = -1_8
           event%m_invargf32   = IEEE_VALUE(1.0_sp,IEEE_QUIET_NAN)  
           event%m_invargf64 = IEEE_VALUE(1.0_dp,IEEE_QUIET_NAN) 
           event%m_descript = " "     
           event%m_isbuilt  = .false.  
     end subroutine destroyInvArgEvent           
             
                
            
   
    !====================================================59
    ! subroutine:
    !             createFailAllocEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FailAllocEvent_t
    !  Notification:
    !                Indicates failed array allocation event
    !=====================================================59
    subroutine createFailAllocEvent(event,evname,umsg,fname, &
                                    modname,procname,locs,loce, &
                                    severity,astat,errmsg,errstat)
          implicit none
          type(FailAllocEvent_t),       intent(inout) :: event
          character(len=*),             intent(in)    :: evname,umsg,fname, &
                                                         modname,procname
          integer(kind=i4),           intent(in)    :: locs,loce,severity,astat
          character(len=*),             intent(in)    :: errmsg
          logical(kind=i4),           intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false.) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
            
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_en     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_astat       = astat
             event%m_errmsg      = errmsg
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFailAllocEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                    
    subroutine destroyFailAllocEvent(event,errstate)
          type(FailAllocEvent_t),       intent(inout) :: event
          logical(kind=i4),           intent(inout) :: errstate
          ! Exec code ....
          if(errstate) errstate = .false.
          if(event%m_isbuilt == .false.) then
                errstate = .true.
                write(ERROR_UNIT,*) "================================================="
                write(ERROR_UNIT,*) "  destroyFailAllocEvent: [FATAL-ERROR]           "
                write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                write(ERROR_UNIT,*) "================================================="
                return  
          end if
           event%m_event_name  = " "
           event%m_msg         = " "
           event%m_file_name   = " "
           event%m_module_name = " "
          
           event%m_proc_name   = " "
           event%m_line_st     = -1
           event%m_line_en     = -1
           event%m_date        = " "
           event%m_time        = " "
           event%m_severity    = -1
           event%m_astat       = -1
           event%m_errmsg      = " "
           event%m_isbuilt     = .false.
    end subroutine destroyFailAllocEvent
    
    !====================================================59
    ! subroutine:
    !             createIndexOutBoundsEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: IndexOutBoundsEvent_t
    !  Notification:
    !               Indicates writing/reading/passing 
    !               indices which are out of bounds.
    !=====================================================59
    subroutine createIndexOutBoundsEvent(event,evname,umsg,fname, &
                                         modname,procname,locs,loce, &
                                         severity,indices,errstat    )
         
          type(IndexOutBoundsEvent_t),         intent(inout) :: event
          character(len=*),                    intent(in)    :: evname,umsg,fname, &
                                                                modname,procname
          integer(kind=i4),                  intent(in)    :: locs,loce,severity
          integer(kind=i4), dimension(2,31), intent(in)    :: indices
          logical(kind=i4),                  intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false.) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
            
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_indices32   = indices
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createIndexOutBoundsEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
    
    subroutine destroyIndexOutBoundsEvent(event,errstat)
          type(IndexOutBoundsEvent_t),         intent(inout) :: event
          logical(kind=i4),                  intent(inout) :: errstat
          ! Exec code .....
          if(errstat) errstat = .false.
          if(event%m_isbuilt == .false.) then
                errstat = .true.
                write(ERROR_UNIT,*) "================================================="
                write(ERROR_UNIT,*) "  destroyIndexOutBoundsEvent: [FATAL-ERROR]           "
                write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                write(ERROR_UNIT,*) "================================================="
                return 
          end if
           event%m_event_name  = " "
           event%m_msg         = " "
           event%m_file_name   = " "
           event%m_module_name = " "
          
           event%m_proc_name   = " "
           event%m_line_st     = 0
           event%m_line_ed     = 0
           event%m_date        = " "
           event%m_time        = " "
           event%m_severity    = -1
           event%m_indices32   = -1
           event%m_isbuilt     = .false.
    end subroutine destroyIndexOutBoundsEvent
                                         
    !====================================================59
    ! subroutine:
    !             createFailDeallocEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FailDeallocEvent_t
    !  Notification:
    !                Indicates failed array deallocation event
    !=====================================================59
    subroutine createFailDeallocEvent(event,evname,umsg,fname, &
                                      modname,procname,locs,loce, &
                                      severity,dstat,errmsg,errstat)
          
          type(FailDeallocEvent_t),       intent(inout) :: event
          character(len=*),               intent(in)    :: evname,umsg,fname, &
                                                           modname,procname
          integer(kind=i4),             intent(in)    :: locs,loce,severity,dstat
          character(len=*),               intent(in)    :: errmsg
          logical(kind=i4),             intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
           
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_dstat       = dstat
             event%m_errmsg      = errmsg
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFailDeallocEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                      
    subroutine destroyFailDeallocEvent(event,errstate)
           type(FailDeallocEvent_t),        intent(inout) :: event
           logical(kind=i4),              intent(inout) :: errstate
           ! Exec code ....
           if(errstate) errstate = .false.
           if(event%m_isbuilt == .false.) then
                errstate = .true.
                write(ERROR_UNIT,*) "================================================="
                write(ERROR_UNIT,*) "  destroyFailDeallocEvent: [FATAL-ERROR]           "
                write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                write(ERROR_UNIT,*) "================================================="
                return 
           end if
            event%m_event_name  = " "
            event%m_msg         = " "
            event%m_file_name   = " "
            event%m_module_name = " "
           
            event%m_proc_name   = " "
            event%m_line_st     = -1
            event%m_line_ed     = -1
            event%m_date        = " "
            event%m_time        = " "
            event%m_severity    = -1
            event%m_dstat       = -1
            event%m_errmsg      = " "
            event%m_isbuilt     = .false.
    end subroutine destroyFailDeallocEvent
    !====================================================59
    ! subroutine:
    !             createFileIOEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FileIOEvent_t
    !  Notification:
    !                Indicates failed file I/O operation
    !=====================================================59
    subroutine createFileIOEvent(event,evname,umsg,fname, &
                                 modname,procname,locs,loce, &
                                 severity,fileop,iostat,iomsg,errstat )
         
          type(FileIOEvent_t),       intent(inout)  :: event
          character(len=*),          intent(in)     :: evname,umsg,fname, &
                                                        modname,procname
          integer(kind=i4),        intent(in)     :: locs,loce,severity
          character(len=*),          intent(in)     :: fileop
          integer(kind=i4),        intent(in)     :: iostat
          character(len=*),          intent(in)     :: iomsg
          logical(kind=i4),        intent(inout)  :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check of errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
            
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_fileop      = fileop
             event%m_iostat      = iostat
             event%m_iomsg       = iomsg
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFileIOEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                 
    subroutine destroyFileIOEvent(event,errstate)
          type(FileIOEvent_t),       intent(inout)  :: event
          logical(kind=i4),        intent(inout)  :: errstate
          ! Exec code ....
          if(errstate) errstate = .false.
          if(event%m_isbuilt == .false.) then
                errstate = .true.
                write(ERROR_UNIT,*) "================================================="
                write(ERROR_UNIT,*) "  destroyFileIOEvent: [FATAL-ERROR]           "
                write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                write(ERROR_UNIT,*) "================================================="
                return 
          end if
           event%m_event_name  =  " "
           event%m_msg         = " "
           event%m_file_name   = " "
           event%m_module_name = " "
         
           event%m_proc_name   = " "
           event%m_line_st     = -1
           event%m_line_ed     = -1
           event%m_date        = " "
           event%m_time        = " "
           event%m_severity    = -1
           event%m_fileop      = " "
           event%m_iostat      = -9999
           event%m_iomsg       = " "
           event%m_isbuilt     = .false.
    end subroutine destroyFileIOEvent
    !====================================================59
    ! subroutine:
    !             createDisassocPtrEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: DisassocPtrEvent_t
    !  Notification:
    !                none
    !=====================================================59
    subroutine createDisassocPtrEvent(event,evname,umsg,fname, &
                                      modname,procname,locs,loce, &
                                      severity,status,errstat    )
         
          type(DisassocPtrEvent_t),       intent(inout) :: event
          character(len=*),               intent(in)    :: evname,umsg,fname, &
                                                           modname,procname
          integer(kind=i4),             intent(in)    :: locs,loce,severity
          logical(kind=i4),             intent(in)    :: status
          logical(kind=i4),             intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
            
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_status      = status
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createDisassocPtrEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                      
    subroutine destroyDisassocPtrEvent(event,errstate)
           type(DisassocPtrEvent_t),       intent(inout) :: event
           logical(kind=i4),             intent(inout) :: errstate
           ! Exec code ....
           if(errstate) errstate = .false.
           if(event%m_isbuilt == .false.) then
                  errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyDisassocPtrEvent: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return
           end if
             event%m_event_name  = " "
             event%m_msg         = " "
             event%m_file_name   = " "
             event%m_module_name = " "
            
             event%m_proc_name   = " "
             event%m_line_st     = -1
             event%m_line_ed     = -1
             event%m_date        = " "
             event%m_time        = " "
             event%m_severity    = -1
             event%m_status      = .false.
             event%m_isbuilt     = .false.
    end subroutine destroyDisassocPtrEvent
    !====================================================59
    ! subroutine:
    !             createFPTrapUndEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPTrapUndEvent_t
    !  Notification:
    !                Caller of 'createFPTrapUndEvent'
    !                must cause directly or indirectly
    !                to modification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPTrapUndEvent(event,evname,umsg,fname, &
                                    modname,procname,locs,loce, &
                                    severity,undval,errstat    )
          use ifcore
         
          type(FPTrapUndEvent_t),        intent(inout) :: event
          character(len=*),              intent(in)    :: evname,umsg, &
                                                          fname,modname, &
                                                           procname
          integer(kind=i4),            intent(in)    :: locs,loce,severity
          real(kind=dp),                 intent(in)    :: undval
          logical(kind=i4),            intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false.) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
            
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_undval      = undval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPTrapUndEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                    
    subroutine destroyFPTrapUndEvent(event,errstate)
           type(FPTrapUndEvent_t),        intent(inout) :: event
           logical(kind=i4),            intent(inout) :: errstate
           ! Exec code ...
           if(errstate) errstate = .false.
           if(event%m_isbuilt == .false.) then
                  errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyFPTrapUndEvent: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return
            end if
             event%m_event_name  = " "
             event%m_msg         = " "
             event%m_file_name   = " "
             event%m_module_name = " "
           
             event%m_proc_name   = " "
             event%m_line_st     = 0
             event%m_line_ed     = 0
             event%m_date        = " "
             event%m_time        = " "
             event%m_severity    = -1
             event%m_undval      = -1.0_dp
             event%m_flags       = -1
             event%m_isbuilt     = .false.
    end subroutine destroyFPTrapUndEvent
    
    !====================================================59
    ! subroutine:
    !             createFPTrapOvfEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPTrapOvfEvent_t
    !  Notification:
    !                Caller of 'createFPTrapOvfEvent'
    !                must cause directly or indirectly
    !                to modification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPTrapOvfEvent(event,evname,umsg,fname, &
                                    modname,procname,locs,loce, &
                                    severity,ovfval,errstat     )
          use ifcore
          
          type(FPTrapOvfEvent_t),       intent(inout) :: event
          character(len=*),             intent(in)    :: evname
          character(len=*),             intent(in)    :: umsg
          character(len=*),             intent(in)    :: fname
          character(len=*),             intent(in)    :: modname
          character(len=*),             intent(in)    :: procname
          integer(kind=i4),           intent(in)    :: locs,loce,severity
          real(kind=dp),                intent(in)    :: ovfval
          logical(kind=i4),           intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check of errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
            
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_ovfval      = ovfval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPTrapOvfEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                    
    subroutine destroyFPTrapOvfEvent(event,errstate)
          type(FPTrapOvfEvent_t),       intent(inout) :: event
          logical(kind=i4),           intent(inout) :: errstate
          ! Exec code ....
          if(errstate) errstate = .false.
          if(event%m_isbuilt == .false.) then
                  errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyFPTrapOvfEvent: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return 
          end if
             event%m_event_name  = " "
             event%m_msg         = " "
             event%m_file_name   = " "
             event%m_module_name = " "
           
             event%m_proc_name   = " "
             event%m_line_st     = 0
             event%m_line_ed     = 0
             event%m_date        = " "
             event%m_time        = " "
             event%m_severity    = -1
             event%m_ovfval      = -1.0_dp
             event%m_flags       = -9999
             event%m_isbuilt     = .false.
    end subroutine destroyFPTrapOvfEvent
    !====================================================59
    ! subroutine:
    !             createFPTrapDiv0Event
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPTrapDiv0Event_t
    !  Notification:
    !                Caller of 'createFPTrapDiv0Event'
    !                must cause directly or indirectly
    !                to modification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPTrapDiv0Event(event,evname,umsg,fname, &
                                     modname,procname,locs,loce, &
                                     severity,div0val,errstat           )
          use ifcore
         
          type(FPTrapDiv0Event_t),        intent(inout)  :: event
          character(len=*),               intent(in)     :: evname
          character(len=*),               intent(in)     :: umsg
          character(len=*),               intent(in)     :: fname
          character(len=*),               intent(in)     :: modname
          character(len=*),               intent(in)     :: procname
          integer(kind=i4),             intent(in)     :: locs,loce,severity
          real(kind=dp),                  intent(in)     :: div0val
          logical(kind=i4),             intent(inout)  :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check of errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .true.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
           
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_div0val     = div0val
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPTrapDiv0Event: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                     
    subroutine destroyFPTrapDiv0Event(event,errstate)
           type(FPTrapDiv0Event_t),        intent(inout)  :: event
           logical(kind=i4),             intent(inout)  :: errstate
           ! Exec code ...
           if(errstate) errstate = .false.
           if(event%m_isbuilt == .false.) then
                   errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyFPTrapDiv0Event: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return 
           end if
             event%m_event_name  = " "
             event%m_msg         = " "
             event%m_file_name   = " "
             event%m_module_name = " "
            
             event%m_proc_name   = " "
             event%m_line_st     = -1
             event%m_line_ed     = -1
             event%m_date        = " "
             event%m_time        = " "
             event%m_severity    = -1
             event%m_div0val     = -1.0_dp
             event%m_flags       = -1
             event%m_isbuilt     = .false.
    end subroutine destroyFPTrapDiv0Event
    
    
    !====================================================59
    ! subroutine:
    !             createFPTrapInvEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPTrapInvEvent_t
    !  Notification:
    !                Caller of 'createFPTrapInvEvent'
    !                must cause directly or indirectly
    !                to modification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPTrapInvEvent(event,evname,umsg,fname, &
                                    modname,procname,locs,loce, &
                                    severity,inval,errstat          )
          use ifcore
          
          type(FPTrapInvEvent_t),         intent(inout)   :: event
          character(len=*),               intent(in)      :: evname
          character(len=*),               intent(in)      :: umsg
          character(len=*),               intent(in)      :: fname
          character(len=*),               intent(in)      :: modname
          character(len=*),               intent(in)      :: procname
          integer(kind=i4),             intent(in)      :: locs,loce,severity
          real(kind=dp),                  intent(in)      :: inval
          logical(kind=i4),             intent(inout)   :: errstat
          ! locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check of errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
            
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_inval       = inval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPTrapInvEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                    
    subroutine destroyFPTrapInvEvent(event,errstate)
           type(FPTrapInvEvent_t),         intent(inout)   :: event
           logical(kind=i4),             intent(inout)   :: errstate
           ! Exec code ....
           if(errstate) errstate = .false.
           if(event%m_isbuilt == .false.) then
                   errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyFPTrapInvEvent: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return 
           end if
             event%m_event_name  = " "
             event%m_msg         = " "
             event%m_file_name   = " "
             event%m_module_name = " "
           
             event%m_proc_name   = " "
             event%m_line_st     = -1
             event%m_line_ed     = -1
             event%m_date        = " "
             event%m_time        = " "
             event%m_severity    = -1
             event%m_inval       = -1.0_dp
             event%m_flags       = -1
             event%m_isbuilt     = .false.
    end subroutine destroyFPTrapInvEvent
    
    
    !====================================================59
    ! subroutine:
    !             createFPAbruptUndEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPAbruptUndEvent_t
    !  Notification:
    !                Caller of 'createFPAbruptUndEvent'
    !                must cause directly or indirectly
    !                to modification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPAbruptUndEvent(event,evname,umsg,fname, &
                                      modname,procname,locs,loce, &
                                      severity,undval,errstat           )
          use ifcore
          
          type(FPAbruptUndEvent_t),       intent(inout) :: event
          character(len=*),               intent(in)    :: evname
          character(len=*),               intent(in)    :: umsg
          character(len=*),               intent(in)    :: fname,modname
          character(len=*),               intent(in)    :: procname
          integer(kind=i4),             intent(in)    :: locs,loce,severity
          real(kind=dp),                  intent(in)    :: undval
          logical(kind=i4),             intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check of errstat arguments
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
            
             event%m_proc_name    = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_undval      = undval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPAbruptUndEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                      
    subroutine destroyFPAbruptUndEvent(event,errstate)
            type(FPAbruptUndEvent_t),       intent(inout) :: event
            logical(kind=i4),             intent(inout) :: errstate
            ! Exec code ....
            if(errstate) errstate = .false.
            if(event%m_isbuilt == .false.) then
                   errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyFPAbruptUndEvent: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return 
            end if
             event%m_event_name  = " "
             event%m_msg         = " "
             event%m_file_name   = " "
             event%m_module_name = " "
            
             event%m_proc_name   = " "
             event%m_line_st     = -1
             event%m_line_ed     = -1
             event%m_date        = " "
             event%m_time        = " "
             event%m_severity    = -1
             event%m_undval      = -1.0_dp
             event%m_flags       = -1
             event%m_isbuilt     = .false.
    end subroutine destroyFPAbruptUndEvent
    
    !====================================================59
    ! subroutine:
    !             createFPAbruptOvfEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPAbruptOvfEvent_t
    !  Notification:
    !                Caller of 'createFPAbruptOvfEvent'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPAbruptOvfEvent(event,evname,umsg,fname, &
                                      modname,procname,locs,loce, &
                                      severity,ovfval,errstat           )
          use ifcore
         
          type(FPAbruptOvfEvent_t),       intent(inout) :: event
          character(len=*),               intent(in)    :: evname
          character(len=*),               intent(in)    :: umsg
          character(len=*),               intent(in)    :: fname
          character(len=*),               intent(in)    :: modname
          character(len=*),               intent(in)    :: procname
          integer(kind=i4),             intent(in)    :: locs,loce,severity
          real(kind=dp),                  intent(in)    :: ovfval
          logical(kind=i4),             intent(inout) :: errstat
          ! locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
           
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_ovfval      = ovfval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPAbruptOvfEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                      
    subroutine destroyFPAbruptOvfEvent(event,errstate)
           type(FPAbruptOvfEvent_t),       intent(inout) :: event
           logical(kind=i4),             intent(inout) :: errstate
           ! Exec code ....
           if(errstate) errstate = .false.
           if(event%m_isbuilt == .false.) then
                   errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyFPAbruptOvfEvent: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return 
           end if
             event%m_event_name  = " "
             event%m_msg         = " "
             event%m_file_name   = " "
             event%m_module_name = " "
            
             event%m_proc_name   = " "
             event%m_line_st     = -1
             event%m_line_ed     = -1
             event%m_date        = " "
             event%m_time        = " "
             event%m_severity    = -1
             event%m_ovfval      = -1.0_dp
             event%m_flags       = -1
             event%m_isbuilt     = .false.
    end subroutine destroyFPAbruptOvfEvent       
    
                                      
    !====================================================59
    ! subroutine:
    !             createFPAbruptDiv0Event
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPAbruptDiv0Event_t
    !  Notification:
    !                Caller of 'createFPAbruptDiv0Event'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPAbruptDiv0Event(event,evname,umsg,fname, &
                                       modname,procname,locs,loce, &
                                       severity,div0val,errstat          )
          use ifcore
        
          type(FPAbruptDiv0Event_t),       intent(inout) :: event
          character(len=*),                intent(in)    :: evname
          character(len=*),                intent(in)    :: umsg
          character(len=*),                intent(in)    :: fname
          character(len=*),                intent(in)    :: modname
          character(len=*),                intent(in)    :: procname
          integer(kind=i4),              intent(in)    :: locs,loce,severity
          real(kind=dp),                   intent(in)    :: div0val
          logical(kind=i4),              intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false.) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
           
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_div0val     = div0val
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPAbruptDiv0Event: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                       
    subroutine destroyFPAbruptDiv0Event(event,errstate)
           type(FPAbruptDiv0Event_t),       intent(inout) :: event
           logical(kind=i4),              intent(inout) :: errstate
           ! Exec code ...
           if(errstate) errstate = .false.
           if(event%m_isbuilt == .false.) then
                  errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyFPAbruptDiv0Event: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return 
           end if
             event%m_event_name  = " "
             event%m_msg         = " "
             event%m_file_name   = " "
             event%m_module_name = " "
           
             event%m_proc_name   = " "
             event%m_line_st     = -1
             event%m_line_ed     = -1
             event%m_date        = " "
             event%m_time        = " "
             event%m_severity    = -1
             event%m_div0val     = -1.0_dp
             event%m_flags       = -1
             event%m_isbuilt     = .false.
    end subroutine destroyFPAbruptDiv0Event
    !====================================================59
    ! subroutine:
    !             createFPAbruptInvEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPAbruptInvEvent_t
    !  Notification:
    !                Caller of 'createFPAbruptInvEvent'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPAbruptInvEvent(event,evname,umsg,fname, &
                                      modname,procname,locs,loce, &
                                      severity,inval,errstat            )
          use ifcore
         
          type(FPAbruptInvEvent_t),       intent(inout) :: event
          character(len=*),               intent(in)    :: evname
          character(len=*),               intent(in)    :: umsg
          character(len=*),               intent(in)    :: fname
          character(len=*),               intent(in)    :: modname
          character(len=*),               intent(in)    :: procname
          integer(kind=i4),             intent(in)    :: locs,loce,severity
          real(kind=dp),                  intent(in)    :: inval
          logical(kind=i4),             intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check of errstat argument
          if(errstat .EQ. .true. ) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
           
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_inval       = inval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat             = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPAbruptInvEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                      
    subroutine destroyFPAbruptInvEvent(event,errstate)
           type(FPAbruptInvEvent_t),       intent(inout) :: event
           logical(kind=i4),             intent(inout) :: errstate
           ! Exec code ...
           if(errstate) errstate = .false.
           if(event%m_isbuilt == .false.) then
                  errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyFPAbruptInvEvent: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return 
           end if
             event%m_event_name  = " "
             event%m_msg         = " "
             event%m_file_name   = " "
             event%m_module_name = " "
             
             event%m_proc_name   = " "
             event%m_line_st     = -1
             event%m_line_ed     = -1
             event%m_date        = " "
             event%m_time        = " "
             event%m_severity    = -1
             event%m_inval       = -1.0_dp
             event%m_flags       = -1
             event%m_isbuilt     = .false.
    end subroutine destroyFPAbruptInvEvent
                                      
                                      
    !====================================================59
    ! subroutine:
    !             createFPAbruptDmzEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPAbruptDmzEvent_t
    !  Notification:
    !                Caller of 'createFPAbruptDmzEvent'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPAbruptDmzEvent(event,evname,umsg,fname, &
                                      modname,procname,locs,loce, &
                                      severity,denval,errstat           )
          use ifcore
          
          type(FPAbruptDmzEvent_t),      intent(inout) :: event
          character(len=*),              intent(in)    :: evname
          character(len=*),              intent(in)    :: umsg
          character(len=*),              intent(in)    :: fname
          character(len=*),              intent(in)    :: modname
          character(len=*),              intent(in)    :: procname
          integer(kind=i4),            intent(in)    :: locs,loce,severity
          real(kind=dp),                 intent(in)    :: denval
          logical(kind=i4),            intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check on errstat argument
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
            
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_denval      = denval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPAbruptDmzEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return
          end if
          
    end subroutine
                                      
    subroutine destroyFPAbruptDmzEvent(event,errstate)
           type(FPAbruptDmzEvent_t),      intent(inout) :: event
           logical(kind=i4),            intent(inout) :: errstate
           ! Exec code ....
           if(errstate) errstate = .false.
           if(event%m_isbuilt == .false.) then
                  errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyFPAbruptDmzEvent: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return 
           end if
             event%m_event_name  = " "
             event%m_msg         = " "
             event%m_file_name   = " "
             event%m_module_name = " "
           
             event%m_proc_name   = " "
             event%m_line_st     = -1
             event%m_line_ed     = -1
             event%m_date        = " "
             event%m_time        = " "
             event%m_severity    = -1
             event%m_denval      = -1.0_dp
             event%m_flags       = -1
             event%m_isbuilt     = .false.
    end subroutine destroyFPAbruptDmzEvent
                                      
    !====================================================59
    ! subroutine:
    !             createFPPoleEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPPoleEvent_t
    !  Notification:
    !                Caller of 'createFPPoleEvent'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPPoleEvent(event,evname,umsg,fname, &
                                 modname,procname,locs,loce, &
                                 severity,polval,errstat          )
          use ifcore
         
          type(FPPoleEvent_t),      intent(inout) :: event
          character(len=*),         intent(in)    :: evname
          character(len=*),         intent(in)    :: umsg
          character(len=*),         intent(in)    :: fname
          character(len=*),         intent(in)    :: modname
          character(len=*),         intent(in)    :: procname
          integer(kind=i4),       intent(in)    :: locs,loce,severity
          real(kind=dp),            intent(in)    :: polval
          logical(kind=i4),       intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check on errstat argument.
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
           
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_polval      = polval
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
             errstat = .true.
             write(ERROR_UNIT,*) "====================================================="
             write(ERROR_UNIT,*) "   createFPPoleEvent: [FATAL-ERROR]"
             write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
             write(ERROR_UNIT,*) "====================================================="
             return 
          end if
          
    end subroutine
                                 
    subroutine destroyFPPoleEvent(event,errstate)
            type(FPPoleEvent_t),      intent(inout) :: event
            logical(kind=i4),       intent(inout) :: errstate
            ! Exec code ...
            if(errstate) errstate = .false.
            if(event%m_isbuilt == .false.) then
                  errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyFPPoleEvent: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return 
            end if
             event%m_event_name  = " "
             event%m_msg         = " "
             event%m_file_name   = " "
             event%m_module_name = " "
            
             event%m_proc_name   = " "
             event%m_line_st     = -1
             event%m_line_ed     = -1
             event%m_date        = " "
             event%m_time        = " "
             event%m_severity    = -1
             event%m_polval      = -1.0_dp
             event%m_flags       = -1
             event%m_isbuilt     = .false.
            
    end subroutine destroyFPPoleEvent
    !====================================================59
    ! subroutine:
    !             createFPSingularEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPSingularEvent_t
    !  Notification:
    !                Caller of 'createFPSingularEvent'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !=====================================================59
    subroutine createFPSingularEvent(event,evname,umsg,fname,   &
                                     modname,procname,locs,loce,&
                                     severity,singularity,errstat       )
          use ifcore
         
          type(FPSingularEvent_t),      intent(inout) :: event
          character(len=*),             intent(in)    :: evname
          character(len=*),             intent(in)    :: umsg
          character(len=*),             intent(in)    :: fname
          character(len=*),             intent(in)    :: modname
          character(len=*),             intent(in)    :: procname
          integer(kind=i4),           intent(in)    :: locs,loce,severity
          real(kind=dp),                intent(in)    :: singularity
          logical(kind=i4),           intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check of errstat argument.
          if(errstat .EQ. .true.) then
             errstat = .false.
          end if
          if(event%m_isbuilt .EQ. .false. ) then
             call DATE_AND_TIME(date=d,time=t)
             event%m_event_name  = evname
             event%m_msg         = umsg
             event%m_file_name   = fname
             event%m_module_name = modname
           
             event%m_proc_name   = procname
             event%m_line_st     = locs
             event%m_line_ed     = loce
             event%m_date        = d
             event%m_time        = t
             event%m_severity    = severity
             event%m_singval     = singularity
             event%m_flags       = FOR_GET_FPE()
             event%m_isbuilt     = .true.
          else
              errstat = .true.
              write(ERROR_UNIT,*) "====================================================="
              write(ERROR_UNIT,*) "   createFPSingularEvent: [FATAL-ERROR]"
              write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
              write(ERROR_UNIT,*) "====================================================="
              return
          end if
          
    end subroutine
    
    subroutine destroyFPSingularEvent(event,errstate)
           type(FPSingularEvent_t),      intent(inout) :: event
           logical(kind=i4),           intent(inout) :: errstate
           ! Exec code ...
           if(errstate) errstate = .false.
           if(event%m_isbuilt == .false.) then
                  errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyFPSingularEvent: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return 
            end if
             event%m_event_name  = " "
             event%m_msg         = " "
             event%m_file_name   = " "
             event%m_module_name = " "
           
             event%m_proc_name   = " "
             event%m_line_st     = -1
             event%m_line_ed     = -1
             event%m_date        = " "
             event%m_time        = " " 
             event%m_severity    = -1
             event%m_singval     = -1.0_dp
             event%m_flags       = -1
             event%m_isbuilt     = .false.
    end subroutine destroyFPSingularEvent
                                     
    !============================================50
    ! subroutine:
    !             createFPDomErrEvent
    !  Purpose:
    !           Abstracts from the user creation
    !           and initilaization of object
    !           of type: FPDomErrEvent_t
    !  Notification:
    !                Caller of 'createFPSingularEvent'
    !                must cause directly or indirectly
    !                to midification of FP-Flags content
    !                register.
    !============================================50
    subroutine createFPDomErrEvent(event,evname,umsg,fname,   &
                                   modname,procname,locs,loce,&
                                   severity,errval,domval,errstat     )
          use ifcore
          
          type(FPDomErrEvent_t),      intent(inout) :: event
          character(len=*),           intent(in)    :: evname
          character(len=*),           intent(in)    :: umsg
          character(len=*),           intent(in)    :: fname
          character(len=*),           intent(in)    :: modname
          character(len=*),           intent(in)    :: procname
          integer(kind=i4),         intent(in)    :: locs,loce,severity
          real(kind=dp),              intent(in)    :: errval,domval
          logical(kind=i4),         intent(inout) :: errstat
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable statements
          ! Sanity check of errstat
          if(errstat .EQ. .true.) then 
             errstat = .false.
          end if
          
          if(event%m_isbuilt .EQ. .false.) then
              call DATE_AND_TIME(date=d,time=t)
              event%m_event_name  = evname
              event%m_msg         = umsg
              event%m_file_name   = fname
              event%m_module_name = modname
             
              event%m_proc_name   = procname
              event%m_line_st     = locs
              event%m_line_ed     = loce
              event%m_date        = d
              event%m_time        = t
              event%m_severity    = severity
              event%m_errval      = errval
              event%m_domval      = domval
              event%m_flags       = FOR_GET_FPE()
              event%m_isbuilt     = .true.
          else
              errstat = .true.
              write(ERROR_UNIT,*) "====================================================="
              write(ERROR_UNIT,*) "   createFPDomErrorEvent: [FATAL-ERROR]"
              write(ERROR_UNIT,*) "   Attempted an initialization of existing object!!"
              write(ERROR_UNIT,*) "====================================================="
              return
          end if
          
    end subroutine
                                   
    subroutine destroyFPDomErrorEvent(event,errstate)
          type(FPDomErrEvent_t),      intent(inout) :: event
          logical(kind=i4),         intent(inout) :: errstate
          ! Exec code ...
           if(errstate) errstate = .false.
           if(event%m_isbuilt == .false.) then
                  errstate = .true.
                  write(ERROR_UNIT,*) "================================================="
                  write(ERROR_UNIT,*) "  destroyFPDomErrorEvent: [FATAL-ERROR]           "
                  write(ERROR_UNIT,*) "  Attempted destruction of uninitialized object!!"
                  write(ERROR_UNIT,*) "================================================="
                  return 
           end if
              event%m_event_name  = " "
              event%m_msg         = " "
              event%m_file_name   = " "
              event%m_module_name = " "
            
              event%m_proc_name   = " "
              event%m_line_st     = -1
              event%m_line_ed     = -1
              event%m_date        = " "
              event%m_time        = " "
              event%m_severity    = -1
              event%m_errval      = -1.0_dp
              event%m_domval      = -1.0_dp
              event%m_flags       = -1
              event%m_isbuilt     = .false.
    end subroutine destroyFPDomErrorEvent
 
    
end module   mod_logevents
