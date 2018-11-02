
 #include "Config.fpp"
 
module mod_logger


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_logger'
 !          
 !          Purpose:
 !                        The module_logger provides static(module defined) procedures to 
 !                        manage a log file.
 !                        Following static procedures are provided:
 !                        - connect a file to logger
 !                        - logging process configuration
 !                        - log messages based on 17 logging events.
 !          History:
 !                      Date: 15-06-2017
 !                      Time: 11:56 GMT+2
 !                      Modified:
 !                      Date: 02-11-2018
 !                      Time: 12:04 GMT+2
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:
 !                    
 !                            Michael Baudin, 2008, michael.baudin@gmail.com
 !                   Changes: Arjen Markus, 2008, arjenmarkus@sourceforge.net  
 !                   Changes: Bernard Gingold, 2017, beniekg@gmail.com   
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

    use mod_kinds,       only : int4,dp
    use mod_logevents
    use ISO_FORTRAN_ENV, only : ERROR_UNIT
    implicit none
    
    public
    private :: log_error_terminate
    !============================================50
    ! Hardcoded File/module information
    !============================================50
    
    ! File version major
    integer(kind=int4), parameter, public :: mod_logger_major = 1_int4
    
    ! File version minor
    integer(kind=int4), parameter, public :: mod_logger_minor = 0_int4
    
    ! File version micro(patch)
    integer(kind=int4), parameter, public :: mod_logger_micro = 0_int4
    
    ! File full version
    integer(kind=int4), parameter, public :: mod_logger_version = 1000_int4*mod_logger_major+100_int4*mod_logger_minor + &
                                                                  10_int4*mod_logger_micro
    
    ! Creation date
    character(*), parameter,  public :: mod_logger_creation_date="15-06-2017 11:56 +00200 (Thr 15 Jun 2017 GMT+2)"
    
    ! File build date/time , should be set manually after every recompilation
    character(*), parameter,  public :: mod_logger_build_date=" "
    
   
    
    ! Module Author
    character(*), parameter,  public :: mod_logger_author="Changed and adapted by: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*), parameter,  public :: mod_logger_description="Events logging module"
    
    !============================================50
    ! Begin declaration/definition of  various
    ! module varaibles , procedures and overloaded
    ! module procedures.
    !============================================50
    
    ! Static variables
    
    ! Logical unit associated with the log file
    integer(I32P) :: log_fileunit = 6
    
    ! Logical unit associated with the stdout
    integer(I32P) :: log_stdout = -1
    
    ! Logical value set to false if user wants
    ! to deactivate stdout
    logical(I32P) :: activate_screen = .true.
    
    ! Logical value set to false if user wants
    ! to deactivate file output
    logical(I32P) :: activate_file = .true.
    
    ! Set to true if logger is activated already
    logical(I32P) :: logger_initialized = .false.
    
    ! Character delimiters
    integer(I32P), parameter, public :: LOG_LEVEL_DELIMITER = 50
    character(len=LOG_LEVEL_DELIMITER) :: log_level_string_volume="===================="
    character(len=LOG_LEVEL_DELIMITER) :: log_level_string_chapter="--------------------"
    character(len=LOG_LEVEL_DELIMITER) :: log_level_string_section="********************"
    character(len=LOG_LEVEL_DELIMITER) :: log_level_string_subsection="++++++++++++++++++++"
    
    ! Set to true whenever an error occurres.
    logical(I32P) :: logger_stoponerror = .true.    ! saved by default
    
    ! List of available delimiter levels
    integer(I32P), parameter, public :: LOG_LEVEL_VOLUME = 1
    integer(I32P), parameter, public :: LOG_LEVEL_CHAPTER = 2
    integer(I32P), parameter, public :: LOG_LEVEL_SECTION = 3
    integer(I32P), parameter, public :: LOG_LEVEL_SUBSECTION = 4
    
    ! Static module scope strings
    
    ! File name
    character(*), parameter, private :: file_name="mod_logger.f90"
    
    ! Module name
    character(*), parameter, private :: mod_name="mod_logger"
     !============================================50
    ! Type: LoggerErrorEvent
    ! Notification:
    !               This derived type is private
    !               to module_logger
    !============================================50
    
    type, private :: LoggerErrorEvent_t
        
        SEQUENCE
        private
        
        ! Event name
        character(len=18)       :: m_event_name
        
        ! Custom error message
        character(len=80)       :: m_errmsg
        
        ! File name declared as a parameter
        character(len=18)       :: m_fname=file_name
        
        ! Module name  declared as a parameter
        character(len=14)       :: m_modname=mod_name
        
        ! Current Win Process(top execution container) returned as a  handle
        integer(HANDLE)         :: m_phandle
        
        ! Current Win Process(top execution container) ID
        integer(DWORD)          :: m_pid
        
        ! Current Win Thread(executing failed procedure) returned as a handle
        integer(HANDLE)         :: m_thandle
        
        ! Current Win Thread(executing failed procedure) ID
        integer(DWORD)          :: m_tid
        
        ! Function name in this case is a top layer wrapper.
        character(len=40)       :: m_proc_name
        
         ! Line of code (start,end) (bottom of event hierarchy)
        integer(kind=int4)           :: m_line_st,m_line_ed
        
        ! Date of event as returned by date_and_time
        character(len=40)       :: m_date
        
        ! Time of event as returned by date_and_time
        character(len=40)       :: time
        
        ! Severity of the event
        ! 1) Catastrophic
        ! 2) Recovereable
        ! 3) Trap
        ! 4) Abort
        ! 5) Fault
        ! 6) Do not care (DC)
        integer(kind=int4)     :: m_severity
        
    end type
    
    !============================================50
    ! Generic logging-writer procedures which
    ! write to file.
    !============================================50
    
    interface  to_file
        module procedure to_fileUsrMsg
        module procedure to_filePerfTimerEvent
        module procedure to_fileInvArgEvent
        module procedure to_fileFailAllocEvent
        module procedure to_fileIndexOutBoundsEvent
        module procedure to_fileFailDeallocEvent
        module procedure to_fileFileIOEvent
        module procedure to_fileDisassocPtrEvent
        module procedure to_fileFPTrapUndEvent
        module procedure to_fileFPTrapOvfEvent
        module procedure to_fileFPTrapDiv0Event
        module procedure to_fileFPTrapInvEvent
        module procedure to_fileFPAbruptUndEvent
        module procedure to_fileFPAbruptOvfEvent
        module procedure to_fileFPAbruptDiv0Event
        module procedure to_fileFPAbruptInvEvent
        module procedure to_fileFPAbruptDmzEvent
        module procedure to_fileFPPoleEvent
        module procedure to_fileFPSingularEvent
        module procedure to_fileFPDomErrEvent
        module procedure to_fileWinError
    end interface
    
    !============================================50
    ! Generic logging-writer procedures which
    ! write to screen.
    !============================================50
    
    interface  to_screen
        module procedure to_screenUsrMsg
        module procedure to_screenPerfTimerEvent
        module procedure to_screenInvArgEvent
        module procedure to_screenFailAllocEvent
        module procedure to_screenIndexOutBoundsEvent
        module procedure to_screenFailDeallocEvent
        module procedure to_screenFileIOEvent
        module procedure to_screenDisassocPtrEven
        module procedure to_screenFPTrapUndEvent
        module procedure to_screenFPTrapOvfEvent
        module procedure to_screenFPTrapDiv0Event
        module procedure to_screenFPTrapInvEvent
        module procedure to_screenFPAbruptUndEvent
        module procedure to_screenFPAbruptOvfEvent
        module procedure to_screenFPAbruptDiv0Event
        module procedure to_screenFPAbruptInvEvent
        module procedure to_screenFPAbruptDmzEvent
        module procedure to_screenFPPoleEvent
        module procedure to_screenFPSingularEvent
        module procedure to_screenFPDomErrEvent
        module procedure to_screenWinError
    end interface
    
    !============================================50
    ! Higher level procedures which wrap lower-
    ! level to_file and to_screen procedures
    !============================================50
    
    interface log_event
        module procedure log_UsrMsg
        module procedure log_PerfTimerEvent
        module procedure log_InvArgEvent
        module procedure log_FailAllocEvent
        module procedure log_IndexOutBoundsEvent
        module procedure log_FailDeallocEvent
        module procedure log_FileIOEvent
        module procedure log_DisassocPtrEvent
        module procedure log_FPTrapUndEvent
        module procedure log_FPTrapOvfEvent
        module procedure log_FPTrapDiv0Event
        module procedure log_FPTrapInvEvent
        module procedure log_FPAbruptUndEvent
        module procedure log_FPAbruptOvfEvent
        module procedure log_FPAbruptDiv0Event
        module procedure log_FPAbruptInvEvent
        module procedure log_FPAbruptDmzEvent
        module procedure log_FPPoleEvent
        module procedure log_FPSingularEvent
        module procedure log_FPDomErrEvent
        module procedure log_WinError
    end interface
    
    !============================================50
    ! Generic configuration procedure
    !============================================50
    
    interface log_configure
        module procedure log_configure_logical
        module procedure log_configure_integer
        module procedure log_configure_character
    end interface
    
    !============================================50
    ! Generic cget procedure
    !============================================50
    
    interface log_cget
        module procedure log_cget_logical
        module procedure log_cget_integer
        module procedure log_cget_character
    end interface
    
    contains
    
    !============================================50
    !  function:
    !             get_module_logger_version_major
    !  Purpose:
    !             Returns this module major version
    !============================================50
    pure function get_module_logger_version_major() result(ver_major)
          
          ! Locals/return
          integer(kind=int4) :: ver_major
          ver_major = mod_logger_major
    end function
    
    !============================================50
    !  function:
    !             get_module_logger_version_minor
    !  Purpose:
    !             Returns this module minor version
    !============================================50
    pure function get_module_logger_version_minor() result(ver_minor)
         
          ! Locals/return
          integer(kind=int4) :: ver_minor
          ver_minor = mod_logger_minor
    end function
    
    !============================================50
    !  function:
    !             get_module_logger_version_micro
    !  Purpose:
    !             Returns this module micro version
    !============================================50
    pure function get_module_logger_version_micro() result(ver_micro)
         
          ! Locals/return
          integer(kind=int4) :: ver_micro
          ver_micro = mod_logger_micro
    end function
    
    !============================================50
    !  function:
    !             get_module_logger_full_version
    !  Purpose:
    !             Returns this module full version
    !============================================50
    pure function get_module_logger_full_version() result(ver_full)
         
         ! Locals/return
         integer(kind=int4) :: ver_full
         ver_full = mod_logger_version
    end function
    
    !============================================50
    !  function:
    !             get_module_creation_date
    !   Purpose:
    !             Returns this module creation date
    !============================================50
    pure function get_module_creation_date() result(cdate)
          implicit none
          ! Locals/returns
          character(len=52) :: cdate
          ! Start of executable statements
          cdate = mod_logger_creation_date
    end function
    
    !============================================50
    !  function:
    !             get_module_build_date
    !   Purpose:
    !             Returns this module build date
    !============================================50
    pure function get_module_build_date() result(bdate)
          implicit none
          ! Locals/returns
          character(len=52) :: bdate
          ! Start of executable statements
          bdate = mod_logger_build_date
    end function
    
   
    
    !============================================50
    !   function:
    !              get_module_author
    !   Purpose:
    !              Returns this module author info
    !============================================50
    pure function get_module_logger_author() result(modauthor)
          implicit none
          ! Locals returns
          character(len=60) :: modauthor
          ! Start of executable statements
          modauthor = mod_logger_author
    end function
    
    !============================================50
    !   function:
    !               get_module_description
    !
    !   Purpose:
    !               Returns this module description
    !============================================50
    pure function get_module_description() result(modescrpt)
          implicit none
          ! Locals/returns
          character(len=24) :: modescrpt
          ! Start of execuatble statements
          modescrpt = mod_logger_description
    end function
    
    
    !=====================================================================75
    ! subroutine: log_startup
    !
    !  Arguments:
    !     log_file           Name of the log file
    !   append, optional :
    !   - if present and true, then the logger appends the messages
    !        to the end of the log file.
    !   - if present and false, then the initialization of the
    !        logger overwrites the messages of the previous logging session.
    !   - if not provided, the default value is append=.true.
    ! Changes: 
    !          (Bernard Gingold) :  No changes to 
    !           original implementation was made.
    !=====================================================================75
    
    subroutine  log_startup(log_file,append)
          
          implicit none
          character(len=*),       intent(in)           :: log_file
          logical(kind=int4),     intent(in), optional :: append
          ! Locals
          logical(kind=int4)                           :: append_real
          character(len=256)                           :: emsg1,emsg2
          integer(kind=int4)                           :: ioerr
          ! Start of executable statements
          if(present(append)) then
              append_real = append
          else
              append_real = .true.
          end if
          if(logger_initialized .EQ. .true. )  then
              emsg1="log_startup: Logger is already initialized!!"
              
              call log_error(emsg1,"procedure: log_startup", &
                             322,326,1)
          else
              log_fileunit = log_get_freeunit()
              if(append_real .EQ. .true.) then
                 open(log_fileunit,FILE=log_file,IOMSG=emsg2,IOSTAT=ioerr, &
                      ACTION='WRITE',STATUS='UNKNOWN', POSITION='APPEND')
                 if(ioerr > 0) then
                    call  log_error(emsg2,"procedure: log_startup", &
                                    330,335,ioerr)
                 end if
              else
                 open(log_fileunit,FILE=log_file,IOMSG=emsg2,IOSTAT=ioerr, &
                      ACTION='WRITE',STATUS='UNKNOWN')
                  if(ioerr > 0) then
                      call log_error(emsg2,"procedure: log_startup", &
                                     341,344,ioerr)
                  end if
              end if
              logger_initialized = .true.
          end if
    end subroutine
    
    !============================================50
    ! subroutine: 
    !             to_fileUsrMsg
    ! Purpose:
    !           Writes to connected file generic
    !           user message.
    !           This subroutine may be used when
    !           no specific logger event must be
    !           wriiten to file or to screen.
    !============================================50
    subroutine to_fileUsrMsg(unit,msg)
        
         
          integer(kind=int4),    intent(in) :: unit
          character(len=*),      intent(in) :: msg
          ! Locals
          character(len=500)                :: filename
          character(len=256)                :: errmsg
          integer(kind=int4)                :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
             write(ERROR_UNIT,*) "to_fileUsrMsg: Invalid unit value:", unit
             return
          end if
          write(unit,'(a)') trim(msg)
          inquire(unit,name=filename)
          close(unit,IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileUsrMsg", &
                             377,380,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileUsrMsg", &
                             383,386,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_filePerfTimeEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: PerfTimeEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_filePerfTimerEvent(unit,event)
         
          integer(kind=int4),          intent(in) :: unit
          type(PerfTimerEvent_t),      intent(in) :: event
          ! Locals
          character(len=500)           :: filename
          character(len=256)           :: errmsg
          integer(I32P)                :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_filePerfTimerEvent: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
             call log_error(errmsg,"procedure: to_filePerfTimerEvent", &
                            413,416,ioerr)
          end if
          inquire(unit, NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_filePerfTimerEvent", &
                             419,422,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_filePerfTimerEvent", &
                             425,429,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileInvArgEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: InvArgEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileInvArgEvent(unit,event)
         
          integer(kind=int4),       intent(in) :: unit
          type(InvArgEvent_t),      intent(in) :: event
          ! Locals
          character(len=500)                   :: filename
          integer(kind=int4)                   :: ioerr
          character(len=256)                   :: errmsg
          ! Start of executable statements
          if(unit .EQ. -1) then
             write(ERROR_UNIT,*) "to_fileInvArgEvent: Invalid unit value: ", unit
             return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileInvArgEvent", &
                             455,459,ioerr)
          end if
          inquire(unit,NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
             call log_error(errmsg,"procedure: to_fileInvArgEvent", &
                            461,465,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileInvArgEvent", &
                             467,471,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFailAllocEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FailAllocEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFailAllocEvent(unit,event)
         
          integer(kind=int4),          intent(in) :: unit
          type(FailAllocEvent_t),      intent(in) :: event
          ! Locals
          character(len=500)                      :: filename
          character(len=256)                      :: errmsg
          integer(kind=int4)                      :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
             write(ERROR_UNIT,*) "to_fileFailAllocEvent: Invalid unit value: ", unit
             return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFailAllocEvent", &
                             498,502,ioerr)
          end if
          inquire(unit, NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
             call log_error(errmsg,"procedure: to_fileFailAllocEvent", &
                            503,508,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFailAllocEvent", &
                             509,513,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileIndexOutBoundsEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: IndexOutBoundsEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
     subroutine to_fileIndexOutBoundsEvent(unit,event)
         
          integer(kind=int4),               intent(in) :: unit
          type(IndexOutBoundsEvent_t),      intent(in) :: event
          ! Locals
          character(len=500)                           :: filename
          character(len=256)                           :: errmsg
          integer(kind=int4)                           :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileIndexOutBoundsEvent: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ierr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure:to_fileIndexOutBoundsEvent", &
                             539,542,ioerr)
          end if
          inquire(unit, NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure:to_fileIndexOutBoundsEvent", &
                             545,549,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure:to_fileIndexOutBoundsEvent" , &
                             551,555, ioerr)
          end if
     end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFailDeallocEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FailDeallocEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFailDeallocEvent(unit,event)
          
          integer(kind=int4),            intent(in) :: unit
          type(FailDeallocEvent_t),      intent(in) :: event
          ! Locals
          character(len=500)                        :: filename
          character(len=256)                        :: errmsg
          integer(kind=int4)                        :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFailDeallocEvent: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFailDeallocEvent", &
                             581,585,ioerr)
          end if
          inquire(unit, NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFailDeallocEvent", &
                              587,5591,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFailDeallocEvent", &
                             593,597,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFileIOEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FileIOEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFileIOEvent(unit,event)
         
          integer(kind=int4),        intent(in) :: unit
          type(FileIOEvent_t),       intent(in) :: event
          ! Locals
          character(len=500)                    :: filename
          character(len=256)                    :: errmsg
          integer(kind=int4)                    :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFileIOEvent: Invalid unit value: ",  unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFileIOEven", &
                             623,627,ioerr)
          end if
          inquire(unit, NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFileIOEven", &
                             629,633,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
               call log_error(errmsg, "procedure: to_fileFileIOEven", &
                              365,639,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_DisassocPtrEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: DisassocPtrEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileDissasocPtrEvent(unit,event)
          
          integer(kind=int4),            intent(in) :: unit
          type(DisassocPtrEvent_t),      intent(in) :: event
          ! Locals
          character(len=500)                        :: filename
          character(len=256)                        :: errmsg
          integer(kind=int4)                        :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileDisassocPtrEvent: ", unit
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileDisassocPtrEvent", &
                             664,668,ioerr)
          end if
          inquire(unit, NAME=filename)
          close(unit, IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileDisassocPtrEvent", &
                             670,674,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileDisassocPtrEvent", &
                             676,680,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFPTrapUndEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FPTrapUndEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFPTrapUndEvent(unit,event)
         
          integer(kind=int4),          intent(in) :: unit
          type(FPTrapUndEvent_t),      intent(in) :: event
          ! Locals
          character(len=500)                 :: filename
          character(len=256)                 :: errmsg
          integer(kind=int4)                 :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
             write(ERROR_UNIT,*) "to_fileFPTrapUndEvent: Invalid unit value: ", unit
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
             call log_error(errmsg,"procedure: to_fileFPTrapUndEvent", &
                            706,710,ioerr)
          end if
          inquire(unit,NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPTrapUndEvent", &
                             711,715,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPTrapUndEvent", &
                             717,721,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFPTrapOvfEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FPTrapOvfEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFPTrapOvfEvent(unit,event)
         
          integer(kind=int4),          intent(in) :: unit
          type(FPTrapOvfEvent_t), intent(in) :: event
          ! Locals
          character(len=500)                 :: filename
          character(len=256)                 :: errmsg
          integer(kind=int4)                      :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFPTrapOvfEvent: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPTrapOvfEvent", &
                             747,751,ioerr)
          end if
          inquire(unit, NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPTrapOvfEvent", &
                             753,757,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPTrapOvfEvent", &
                             759,763,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFPTrapDiv0Event
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FPTrapDiv0Event_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFPTrapDiv0Event(unit,event)
         
          integer(kind=int4),           intent(in) :: unit
          type(FPTrapDiv0Event_t), intent(in) :: event
          ! Locals
          character(len=500)                  :: filename
          character(len=256)                  :: errmsg
          integer(kind=int4)                       :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFPTrapDiv0Event: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPTrapDiv0Event", &
                             789,793,ioerr)
          end if
          inquire(unit,NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPTrapDiv0Event", &
                             795,799,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPTrapDiv0Event", &
                             801,805,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFPTrapInvEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FPTrapInvEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFPTrapInvEvent(unit,event)
         
          integer(kind=int4),          intent(in) :: unit
          type(FPTrapInvEvent_t), intent(in) :: event
          ! Locals
          character(len=500)                 :: filename
          character(len=256)                 :: errmsg
          integer(kind=int4)                      :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFPTrapInvEvent: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPTrapInvEvent", &
                             834,838,ioerr)
          end if
          inquire(unit,NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPTrapInvEvent", &
                             840,844,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPTrapInvEvent", &
                             846,850,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFPAbruptUndEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FPAbruptUndEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFPAbruptUndEvent(unit,event)
         
          integer(kind=int4),            intent(in) :: unit
          type(FPAbruptUndEvent_t), intent(in) :: event
          ! Locals
          character(len=500)                   :: filename
          character(len=256)                   :: errmsg
          integer(kind=int4)                        :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFPAbruptUndEvent: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptUndEvent", &
                             831,835,ioerr)
          end if
          inquire(unit,NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptUndEvent", &
                             837,841,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptUndEvent", &
                             843,847,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFPAbruptOvfEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FPAbruptOvfEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFPAbruptOvfEvent(unit,event)
         
          integer(kind=int4),            intent(in) :: unit
          type(FPAbruptUndEvent_t), intent(in) :: event
          ! Locals
          character(len=500)                   :: filename
          character(len=256)                   :: errmsg
          integer(kind=int4)                        :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFPAbruptUndEvent: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptUndEvent", &
                             873,877,ioerr)
          end if
          inquire(unit,NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptUndEvent", &
                             879,883,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptUndEvent", &
                             885,889,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFPAbruptDiv0Event
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FPAbruptDiv0Event_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFPAbruptDiv0Event(unit,event)
         
          integer(kind=int4),             intent(in) :: unit
          type(FPAbruptDiv0Event_t),      intent(in) :: event
          ! Locals
          character(len=500)                         :: filename
          character(len=256)                         :: errmsg
          integer(kind=int4)                         :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFPAbruptDiv0Event: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptDiv0Event", &
                             915,919,ioerr)
          end if
          inquire(unit,NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptDiv0Event", &
                             921,925,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptDiv0Event", &
                             927,931,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFPAbruptInvEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FPAbruptInvEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFPAbruptInvEvent(unit,event)
         
          integer(kind=int4),            intent(in) :: unit
          type(FPAbruptInvEvent_t),      intent(in) :: event
          ! Locals
          character(len=500)                        :: filename
          character(len=256)                        :: errmsg
          integer(kind=int4)                        :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFPAbruptInvEvent: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptInvEvent", &
                             1002,1006,ioerr)
          end if
          inquire(unit,NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptInvEvent", &
                             1008,1012,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptInvEvent", &
                             1015,1019,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFPAbruptDmzEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FPAbruptDmzEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFPAbruptDmzEvent(unit,event)
         
          integer(kind=int4),            intent(in) :: unit
          type(FPAbruptDmzEvent_t),      intent(in) :: event
          !Locals
          character(len=500)                        :: filename
          character(len=256)                        :: errmsg
          integer(kind=int4)                        :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFPAbruptDmzEvent: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptDmzEvent", &
                             1047,1051,ioerr)
          end if
          inquire(unit, NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptDmzEvent", &
                             1053,1057,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPAbruptDmzEvent", &
                             1059,1063,ioerr)
          end if
    end subroutine
    
     
    !============================================50
    ! subroutine:
    !             to_fileFPPoleEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FPPoleEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFPPoleEvent(unit,event)
        
          integer(kind=int4),        intent(in) :: unit
          type(FPPoleEvent_t),       intent(in) :: event
          ! Locals
          character(len=500)                    :: filename
          character(len=256)                    :: errmsg
          integer(kind=int4)                    :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFPPoleEvent: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPPoleEvent", &
                             1090,1094, ioerr)
          end if
          inquire(unit, NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPPoleEvent", &
                             1096,1100,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPPoleEvent", &
                             1102,1106, ioerr)
          end if
    end subroutine
    
      
    !============================================50
    ! subroutine:
    !             to_fileFPSingularEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FPSingularEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFPSingularEvent(unit,event)
          use ISO_FORTRAN_ENV, only : ERROR_UNIT
          implicit none
          integer(kind=int4),           intent(in) :: unit
          type(FPSingularEvent_t), intent(in) :: event
          ! Locals
          character(len=500)                  :: filename
          character(len=256)                  :: errmsg
          integer(kind=int4)                       :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFPSingularEvent: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPSingularEvent", &
                             1133,1137,ioerr)
          end if
          inquire(unit,NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPSingularEvent", &
                             1139,1143,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPSingularEvent", &
                             1145,1149,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_fileFPDomErrEvent
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: FPDomErrEvent_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileFPDomErrEvent(unit,event)
         
          integer(kind=int4),         intent(in) :: unit
          type(FPDomErrEvent_t),      intent(in) :: event
          ! Locals
          character(len=500)                     :: filename
          character(len=256)                     :: errmsg
          integer(kind=int4)                     :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
              write(ERROR_UNIT,*) "to_fileFPDomErrEvent: Invalid unit value: ", unit
              return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPDomErrEvent", &
                             1175,1179,ioerr)
          end if
          inquire(unit, NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPDomErrEvent", &
                             1811,1185,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileFPDomErrEvent", &
                             1187,1191,ioerr)
          end if
    end subroutine
    
    
    !============================================50
    ! subroutine:
    !             to_fileWinError
    !  Purpose:
    !             Writes to connected file
    !             the content of logger event
    !             of type: WinError_t
    !             Unformatted I/O list write is in use.
    !============================================50
    subroutine to_fileWinError(unit,event)
         
          integer(kind=int4),    intent(in) :: unit
          type(WinError_t), intent(in) :: event
          ! Locals
          character(len=500)           :: filename
          character(len=256)           :: errmsg
          integer(kind=int4)                :: ioerr
          ! Start of executable statements
          if(unit .EQ. -1) then
             write(ERROR_UNIT,*) "to_fileWinError: Invalid unit value:", unit
             return
          end if
          write(unit,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
             call log_error(errmsg,"procedure: to_fileWinError", &
                            1345,1349,ioerr)
          end if
          inquire(unit, NAME=filename)
          close(unit,IOSTAT=ioerr,IOMSG=errmsg)
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileWinErr", &
                             1351,1355,ioerr)
          end if
          open(unit,FILE=filename,IOMSG=errmsg,IOSTAT=ioerr, &
               ACTION='WRITE',STATUS='UNKNOWN',POSITION='APPEND')
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_fileWinError", &
                             1357,1361,ioerr)
          end if
    end subroutine
    !============================================50
    ! subroutine: 
    !             to_screenUsrMsg
    ! Purpose:
    !           Writes to screen user message.
    !           
    !           This subroutine may be used when
    !           no specific logger event must be
    !           wriiten to file or to screen.
    !============================================50
    subroutine to_screenUsrMsg(msg)
         
          implicit none
          
          character(len=*), intent(in) :: msg
          ! Start of executable statements
          
          write(OUTPUT_UNIT,'(a)') trim(msg)
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenPerfTimeEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: PerfTimeEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenPerfTimeEvent(event)
         
          type(PerfTimeEvent_t), intent(in) :: event
          ! Locals
          character(len=256)                :: errmsg
          integer(kind=int4)                     :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenPerfTimeEvent", &
                             1230,1234,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenInvArgEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: InvArgEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenInvArgEvent(event)
         
          type(InvArgEvent_t), intent(in) :: event
          ! Locals
          character(len=256)              :: errmsg
          integer(kind=int4)                   :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenInvArgEvent", &
                             1253,1257,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFailAllocEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FailAllocEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFailAllocEvent(event)
         
          type(FailAllocEvent_t), intent(in) :: event
          ! Locals
          character(len=256)                 :: errmsg
          integer(kind=int4)                      :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFailAllocEvent", &
                             1276,1280,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenIndexOutBoundsEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: IndexOutBoundsEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenIndexOutBoundsEvent(event)
         
          type(IndexOutBoundsEvent_t), intent(in) :: event
          ! Locals
          character(len=256)                      :: errmsg
          integer(kind=int4)                           :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenIndexOutBoundsEvent", &
                             1299,1303,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFailDeallocEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FailDeallocEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFailDeallocEvent(event)
          
          type(FailDeallocEvent_t), intent(in)      :: event
          ! Locals
          character(len=256)                        :: errmsg
          integer(kind=int4)                        :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFailDeallocEvent", &
                             1322,1326,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFileIOEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FileIOEvent_t
    !             Unformatted write is in use.
    !============================================50
    
    subroutine to_screenFileIOEvent(event)
         
          type(FileIOEvent_t), intent(in) :: event
          ! Locals
          character(len=256)              :: errmsg
          integer(kind=int4)                   :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFileIOEvent", &
                             1346,1350,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenDisassocPtrEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: DisassocPtrEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenDisassocPtrEvent(event)
          use ifcore, only : TRACEBACKQQ
        
          type(DisassocPtrEvent_t), intent(in)     :: event
          ! Locals
          character(len=256)                        :: errmsg
          integer(kind=int4)                        :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$  IF (PRINT_CALLSTACK .EQ. 1)
          call  TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenDisassocPtrEvent", &
                             1369,1373,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFPTrapUndEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FPTrapUndEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFPTrapUndEvent(event)
          use ifcore, only : TRACEBACKQQ
         
          type(FPTrapUndEvent_t),     intent(in) :: event
          ! Locals
          character(len=256)                      :: errmsg
          integer(kind=int4)                      :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF (PRINT_CALLSTACK .EQ. 1 )
          call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFPTrapUndEvent", &
                             1392,1396)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFPTrapOvfEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FPTrapOvfEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFPTrapOvfEvent(event)
          use ifcore, only : TRACEBACKQQ
          
          type(FPTrapOvfEvent_t),      intent(in) :: event
          ! Locals
          character(len=256)                      :: errmsg
          integer(kind=int4)                      :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF  (PRINT_CALLSTACK .EQ. 1)
          call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFPTrapOvfEvent", &
                             1415,1419,ioerr)
          end if
    end subroutine
    
     
    !============================================50
    ! subroutine:
    !             to_screenFPTrapDiv0Event
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FPTrapDiv0Event_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFPTrapDiv0Event(event)
          use ifcore, only : TRACEBACKQQ
         
          type(FPTrapDiv0Event_t),     intent(in) :: event
          ! Locals
          character(len=256)                       :: errmsg
          integer(kind=int4)                       :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF  (PRINT_CALLSTACK  .EQ. 1)
          call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFPTrapDiv0Event", &
                             1439,1443,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFPTrapInvEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FPTrapInvEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFPTrapInvEvent(event)
          use ifcore, only : TRACEBACKQQ
         
          type(FPTrapInvEvent_t),      intent(in) :: event
          ! Locals
          character(len=256)                      :: errmsg
          integer(kind=int4)                      :: ioerr
          ! Start of executable ststements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF  (PRINT_CALLSTACK .EQ. 1 )
          call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFPTrapInvEvent", &
                             1454,1458,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFPAbruptUndEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FPAbruptUndEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFPAbruptUndEvent(event)
          use ifcore, only : TRACEBACKQQ
         
          type(FPAbruptUndEvent_t),      intent(in) :: event
          ! Locals
          character(len=256)                        :: errmsg
          integer(kind=int4)                        :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF  (PRINT_CALLSTACK .EQ. 1 )
          call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFPAbruptUndEvent", &
                             1485,1489,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFPAbruptOvfEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FPAbruptOvfEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFPAbruptOvfEvent(event)
          use ifcore, only : TRACEBACKQQ
         
          type(FPAbruptOvfEvent_t), intent(in) :: event
          ! Locals
          character(len=256)                   :: errmsg
          integer(I32P)                        :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF (PRINT_CALLSTACK .EQ. 1)
          call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFPAbruptOvfEvent", &
                             1508,1512,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFPAbruptDiv0Event
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FPAbruptDiv0Event_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFPAbruptDiv0Event(event)
          use ifcore, only : TRACEBACKQQ
        
          type(FPAbruptDiv0Event_t),      intent(in) :: event
          ! Locals
          character(len=256)                         :: errmsg
          integer(kind=int4)                         :: ioerr
          ! Start of executable
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF (PRINT_CALLSTACK .EQ. 1 )
          call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFPAbruptDiv0Event", &
                             1532,1535,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFPAbruptInvEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FPAbruptInvEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFPAbruptInvEvent(event)
          use ifcore, only : TRACEBACKQQ
         
          type(FPAbruptInvEvent_t), intent(in) :: event
          ! Locals
          character(len=256)                   :: errmsg
          integer(kind=int4)                        :: ioerr
          ! Start of executable
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF (PRINT_CALLSTACK .EQ. 1)
          call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFPAbruptInvEvent", &
                             1554,1558,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFPAbruptDmzEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FPAbruptDmzEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFPAbruptDmzEvent(event)
          use ifcore, only : TRACEBACKQQ
         
          type(FPAbruptDmzEvent_t), intent(in) :: event
          ! Locals
          character(len=256)                   :: errmsg
          integer(kind=int4)                        :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF (PRINT_CALLSTACK .EQ. 1)
          call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFPAbruptDmzEvent", &
                             1577,1581,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFPPoleEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FPPoleEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFPPoleEvent(event)
          use ifcore, only : TRACEBACKQQ
        
          type(FPPoleEvent_t), intent(in) :: event
          ! Locals
          character(len=256)              :: errmsg
          integer(kind=int4)                   :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF (PRINT_CALLSTACK .EQ. 1)
          call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFPPoleEvent", &
                             1600,1604,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFPSingularEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FPSingularEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFPSingularEvent(event)
          use ifcore, only : TRACEBACKQQ
         
          type(FPSingularEvent_t), intent(in) :: event
          ! Locals
          character(len=256)                  :: errmsg
          integer(kind=int4)                       :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF (PRINT_CALLSTACK .EQ. 1  )
          call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFPSingularEvent", &
                             1623,1627,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenFPDomErrEvent
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: FPDomErrEvent_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenFPDomErrEvent(event)
          use ifcore, only : TRACEBACKQQ
          
          type(FPDomErrEvent_t),      intent(in) :: event
          ! Locals
          character(len=256)                     :: errmsg
          integer(kind=int4)                     :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF (PRINT_CALLSTACK .EQ. 1 )
          call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenFPDomErrEvent", &
                             1646,1650,ioerr)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             to_screenWinError
    !  Purpose:
    !             Writes to screen(std_out)
    !             the content of logger event
    !             of type: WinError_t
    !             Unformatted write is in use.
    !============================================50
    subroutine to_screenWinError(event)
          use ifcore, only : TRACEBACKQQ
        
          type(WinError_t), intent(in)      :: event
          ! Locals
          character(len=256)                :: errmsg
          integer(kind=int4)                :: ioerr
          ! Start of executable statements
          write(log_stdout,*,IOSTAT=ioerr,IOMSG=errmsg) event
!DIR$ IF (PRINT_CALLSTACK .EQ. 1 )
           call TRACEBACKQQ(event%m_msg,-1)
!DIR$ ENDIF
          if(ioerr > 0) then
              call log_error(errmsg,"procedure: to_screenWinError", &
                             1838,1842,ioerr)
          end if
    end subroutine
    !============================================50
    ! subroutine:
    !             log_UsrMsg
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileUsrMsg, to_screenUsrMsg
    !============================================50
    subroutine log_UsrMsg(msg)
         
          character(len=*), intent(in) :: msg
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileUsrMsg(msg)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenUsrMsg(msg)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_PerfTimerEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_filePerfTimerEvent, 
    !        to_screenPerfTimerEvent
    !============================================50
    subroutine log_PerfTimerEvent(unit,event)
       
          integer(kind=int4),          intent(in) :: unit
          type(PerfTimerEvent_t),      intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_filePerfTimerEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenPerfTimeEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_InvArgEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileInvArgEvent, 
    !        to_screenInvArgEvent
    !============================================50
    subroutine log_InvArgEvent(unit,event)
         
          integer(kind=int4),       intent(in) :: unit
          type(InvArgEvent_t),      intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileInvArgEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenInvArgEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FailAllocEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFailAllocEvent, 
    !        to_screenFailAllocEvent
    !============================================50
    subroutine log_FailAllocEvent(unit,event)
        
          integer(kind=int4),          intent(in) :: unit
          type(FailAllocEvent_t),      intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFailAllocEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFailAllocEvent(event)
          end if
    end subroutine
    
     
    !============================================50
    ! subroutine:
    !             log_IndexOutBoundsEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileIndexOutBoundsEvent, 
    !        to_screenIndexOutBoundsEvent
    !============================================50
    subroutine log_IndexOutBoundsEvent(unit,event)
         
          integer(kind=int4),               intent(in) :: unit
          type(IndexOutBoundsEvent_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileIndexOutBoundsEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenIndexOutBoundsEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FailDeallocEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFailDeallocEvent, 
    !        to_screenFailDeallocEvent
    !============================================50
    subroutine log_FailDeallocEvent(unit,event)
        
          integer(kind=int4),            intent(in) :: unit
          type(FailDeallocEvent_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFailDeallocEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFailDeallocEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FileIOEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFileIOEvent, 
    !        to_screenFileIOEvent
    !============================================50
    subroutine log_FileIOEvent(unit,event)
          
          integer(kind=int4),       intent(in) :: unit
          type(FileIOEvent_t),      intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFileIOEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFileIOEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_DisassocPtrEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileDisassocPtrEvent, 
    !        to_screenDisassocPtrEvent
    !============================================50
    subroutine log_DisassocPtrEvent(unit,event)
         
          integer(kind=int4),            intent(in) :: unit
          type(DisassocPtrEvent_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileDissasocPtrEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenDisassocPtrEvent(event)
          end if
    end subroutine 
    
    !============================================50
    ! subroutine:
    !             log_FPTrapUndEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFPTrapUndEvent, 
    !        to_screenFPTrapUndEvent
    !============================================50
    subroutine log_FPTrapUndEvent(unit,event)
          
          integer(kind=int4),          intent(in) :: unit
          type(FPTrapUndEvent_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFPTrapUndEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFPTrapUndEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FPTrapOvfEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFPTrapOvfEvent, 
    !        to_screenFPTrapOvfEvent
    !============================================50
    subroutine log_FPTrapOvfEvent(unit,event)
         
          integer(kind=int4),          intent(in) :: unit
          type(FPTrapOvfEvent_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFPTrapOvfEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFPTrapOvfEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FPTrapDiv0Event
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFPTrapDiv0Event, 
    !        to_screenFPTrapDiv0Event
    !============================================50
    subroutine log_FPTrapDiv0Event(unit,event)
          
          integer(kind=int4),           intent(in) :: unit
          type(FPTrapDiv0Event_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFPTrapDiv0Event(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFPTrapDiv0Event(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FPTrapInvEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFPTrapInvEvent, 
    !        to_screenFPTrapInvEvent
    !============================================50
    subroutine log_FPTrapInvEvent(unit,event)
          
          integer(kind=int4),          intent(in) :: unit
          type(FPTrapInvEvent_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFPTrapInvEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFPTrapInvEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FPAbruptUndEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFPAbruptUndEvent, 
    !        to_screenFPAbruptUndEvent
    !============================================50
    subroutine log_FPAbruptUndEvent(unit,event)
         
          integer(kind=int4), intent(in) :: unit
          type(FPAbruptUndEvent_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFPAbruptUndEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFPAbruptUndEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FPAbruptOvfEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFPAbruptOvfEvent, 
    !        to_screenFPAbruptOvfEvent
    !============================================50
    subroutine log_FPAbruptOvfEvent(unit,event)
         
          integer(kind=int4),            intent(in) :: unit
          type(FPAbruptOvfEvent_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFPAbruptOvfEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFPAbruptOvfEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FPAbruptDiv0Event
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFPAbruptDiv0Event, 
    !        to_screenFPAbruptDiv0Event
    !============================================50
    subroutine log_FPAbruptDiv0Event(unit,event)
          
          integer(kind=int4),            intent(in) :: unit
          type(FPAbruptDiv0Event_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFPAbruptDiv0Event(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFPAbruptDiv0Event(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FPAbruptInvEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFPAbruptInvEvent, 
    !        to_screenFPAbruptInvEvent
    !============================================50
    subroutine log_FPAbruptInvEvent(unit,event)
         
          integer(kind=int4), intent(in) :: unit
          type(FPAbruptInvEvent_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFPAbruptInvEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFPAbruptInvEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FPAbruptDmzEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFPAbruptDmzEvent, 
    !        to_screenFPAbruptDmzEvent
    !============================================50
    subroutine log_FPAbruptDmzEvent(unit,event)
        
          integer(kind=int4),            intent(in) :: unit
          type(FPAbruptDmzEvent_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFPAbruptDmzEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFPAbruptDmzEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FPPoleEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFPPoleEvent, 
    !        to_screenFPPoleEvent
    !============================================50
    subroutine log_FPPoleEvent(unit,event)
         
          integer(kind=int4),       intent(in) :: unit
          type(FPPoleEvent_t), intent(in) :: event
          ! Start of executable
          if(activate_file .EQ. .true.) then
              call to_fileFPPoleEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFPPoleEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FPSingularEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFPSingularEvent, 
    !        to_screenFPSingularEvent
    !============================================50
    subroutine log_FPSingularEvent(unit,event)
         
          integer(kind=int4),           intent(in) :: unit
          type(FPSingularEvent_t), intent(in) :: event
          ! Start of executable
          if(activate_file .EQ. .true.) then
              call to_fileFPSingularEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFPSingularEvent(event)
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_FPDomErrEvent
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileFPDomErrEvent, 
    !        to_screenFPDomErrEvent
    !============================================50
    subroutine log_FPDomErrEvent(unit,event)
         
          integer(kind=int4), intent(in) :: unit
          type(FPDomErrEvent_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileFPDomErrEvent(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenFPDomErrEvent(event)
          end if
    end subroutine
    
     
    !============================================50
    ! subroutine:
    !             log_WinError
    ! Purpose:
    !             Top-level wrapper subroutine
    !             which upon setting of global
    !             logical variable will either
    !             write user message to screen
    !             or to file.
    ! Calls:
    !        to_fileWinError, 
    !        to_screenWinError
    !============================================50
    subroutine log_WinError(unit,event)
         
          integer(kind=int4),    intent(in) :: unit
          type(WinError_t), intent(in) :: event
          ! Start of executable statements
          if(activate_file .EQ. .true.) then
              call to_fileWinError(unit,event)
          end if
          if(activate_screen .EQ. .true.) then
              call to_screenWinError(event)
          end if
    end subroutine 
    
    !==================================================================================88
    ! subroutine:
    !             log_configure_logical
    ! Purpose:
    !         log_configure_logical --
    !   Set the logical static "option" of the component to "value".
    !   
    !   Disable or enable the writing on standard output.
    !   option = "writeonlogfile"
    !   Disable or enable the writing on log file.
    !   option = "stoponerror"
    !   Configure the behaviour of the component whenever an
    !   error is met.
    !   If stoponerror is true, then the execution stops if an error is encountered.
    !   If stoponerror is false, then the execution continues if an error is encountered.
    !   In both cases, a message is displayed on standard output.
    !   Changes to original implementation:
    !   'timestamp' option was removed, because logger events contain date and time
    !    records.
    !==================================================================================88
    subroutine log_configure_logical(option,value)
          
          character(len=*), intent(in) :: option
          logical(kind=int4),    intent(in) :: value
          ! Start of executable statements
          select case(option)
          case ("writeonstdout")
              activate_screen = value
          case ("writeonlogfile")
              activate_file = value)
          case ("stoponerror")
              logger_stoponerror = value
          case default
              call log_error("case default executed!!",          &
                             "procedure: log_configure_logical", &
                             2203,2206,1)
          end select
    end subroutine
    
    !=============================================================================83
    !  subroutine:
    !               log_configure_integer
    !  Purpose:
    !                Set the integer static "option" of the component to "value".
    !                The "option" may be one of the following.
    !                option = "logfileunit"
    !                Force the logical unit for logging to be "value".
    !                Use this feature with caution, since the original
    !                logical unit is lost.
    !=============================================================================83
    subroutine log_configure_integer(option,value)
          
          character(len=*), intent(in) :: option
          integer(kind=int4),    intent(in) :: value
          ! Start of executable statements
          select case (option)
          case ("logfileunit")
              log_fileunit = value
          case default
              call log_error("case default executed!!",  &
                             "procedure: log_configure_integer", &
                             2229,2233,1)
          end select
    end subroutine
    
    !=========================================================================79
    !   subroutine:
    !                log_configure_character
    !   Purpose:
    !                 Set the character static "option" of the component to "value".
    !                 The "option" may be one of the following.
    !                 option = "level_string_volume"
    !                 Set the string used for volume delimiter.
    !                 option = "level_string_chapter"
    !                 Set the string used for chapter delimiter.
    !                 option = "level_string_section"
    !                 Set the string used for section delimiter.
    !                 option = "level_string_subsection"
    !                 Set the string used for subsection delimiter.
    !=========================================================================79
    subroutine log_configure_character(option,value)
         
          character(len=*), intent(in) :: option
          character(len=*), intent(in) :: value
          ! Start of executable statements
          select case (option)
          case ("level_string_volume")
               log_level_string_volume = value
          case ("level_string_chapter")
               log_level_string_chapter = value
          case ("level_string_section")
               log_level_string_section = value
          case ("level_string_subsection")
               log_level_string_subsection = value
          case default
               call log_error("case default executed!!",            &
                              "procedure: log_configure_character", &
                              2265,2270,1)
          end select
    end subroutine
    
    !===================================================================================89
    ! subroutine:
    !              log_cget_logical
    !
    !  Purpose:
    !                Returns the value of the given logical option.
    !                The "option" may be one of the following.
    !               
    !                 Current value of the option to enable / disable insertion of time stamps.
    !                 option = "writeonstdout"
    !                 Current value of the option to enable / disable writing on standard output.
    !                 option = "writeonlogfile"
    !                 Current value of the option to enable / disable writing on log file.
    !                 option = "stoponerror"
    !                 Current value of the option to enable / disable stopping when an error is met.
    !
    !===================================================================================89
    subroutine log_cget_logical(option,value)
          
          character(len=*), intent(in) :: option
          logical(kind=int4),    intent(out) :: value
          ! Start of executable statements
          select case (option)
          case ("writeonstdout")
               value = activate_screen
          case ("writeonlogfile")
               value = activate_file
          case ("stoponerror")
               value = logger_stoponerror
          case default
               call log_error("case default executed!!",     &
                              "procedure: log_cget_logical", & ,
                               2425,2429,1)
          end select
    end subroutine
    
    !=============================================================67
    !   subroutine:
    !               log_cget_integer
    !   Purpose:
    !                Returns the value of the given integer option.
    !                The "option" may be one of the following.
    !                option = "logfileunit"
    !                Current logical unit connected to the logging system.
    !
    !==============================================================67
    subroutine log_cget_integer(option,value)
         
          character(len=*), intent(in) :: option
          integer(kind=int4),    intent(out) :: value
          ! Start of executabl statements
          select case (option)
          case ("logfileunit")
                value = log_fileunit
          case default
                call log_error("case default executed!!",     &
                               "procedure: log_cget_integer", & 
                                2450,2454,1)
          end select
    end subroutine
    
    !=============================================================67
    !   subroutine:
    !                log_cget_character
    !   Purpose:
    !                 Returns the value of the given logical option.
    !                 The "option" may be one of the following.
    !                 option = "level_string_volume"
    !                 Get the string used for volume delimiter.
    !                 option = "level_string_chapter"
    !                 Get the string used for chapter delimiter.
    !                 option = "level_string_section"
    !                 Get the string used for section delimiter.
    !                 option = "level_string_subsection"
    !                 Get the string used for subsection delimiter.
    !
    !=============================================================67
    subroutine log_cget_character(option,value)
         
          character(len=*), intent(in)  :: option
          character(len=*), intent(out) :: value
          ! Start of executable stataments
          select case (option)
          case ("level_string_volume")
              value = log_level_string_volume
          case ("level_string_chapter")
              value = log_level_string_chapter
          case ("level_string_section")
              value = log_level_string_section
          case ("level_string_subsection")
              value = log_level_string_subsection
          case default
              call log_error("case default executed!!", &
                             "procedure: log_cget_character", &
                             2487,2491,1)
          end select
    end subroutine
    !============================================50
    ! subroutine:
    !             log_shutdown
    ! Purpose:
    !             Log shutdown processing
    !============================================50
    subroutine log_shutdown()
          
         
          ! Locals
          character(len=256) :: errmsg
          integer(kind=int4)      :: ioerr
          close(log_fileunit,IOMSG=errmsg,IOSTAT=ioerr)
          if(ioerr > 0) then
             call log_error(errmsg,"procedure: log_shutdown", &
                            363,366,ioerr)
          end if
          
    end subroutine
    
   
    
    !============================================50
    ! subroutine:
    !             log_reset
    ! Purpose:
    !           Set all internal settings to
    !           default values.
    !============================================50
    subroutine log_reset()
        
          activate_screen = .true.
          activate_file   = .true.
          
    end subroutine
    
    !============================================50
    ! function:
    !           log_isinitialized 
    ! Purpose:
    !           Returns true if the logger is
    !           already initialized
    !============================================50
    function log_isinitialized() result(b_res)
         
          ! Locals/return
          logical(kind=int4) :: b_res
          b_res = logger_initialized
    end function
    
    
    
    !============================================50
    ! subroutine:
    !              log_error
    !                       Generates an error
    !                       for the logger.
    !============================================50
    subroutine log_error(msg,procname,locst,loced,severity)
          use ifcore
          include 'IOSDEF.FOR'
          
          character(len=*), intent(in) :: msg,procname
                                         
          integer(kind=int4),    intent(in) :: locst,loced, &
                                          severity
          ! Locals
          integer(HANDLE)              :: thand,phand
          integer(DWORD)               :: tid,pid
!DIR$ IF (SHOW_CALLSTACK .EQ. 1)
          integer(kind=int4)                :: ret_stat
!DIR$ ENDIF
          character(len=40)            :: date_str,time_str
          type(LoggerErrorEvent_t)     :: errevent_t
          ! Start of executable statements
          call DATE_AND_TIME(date=date_str,time=time_str)
          errevent_t = LoggerErrorEvent_t("LoggerErrorEvent_t",   &
                                          msg,GetCurrentProcess(),&
                                          GetCurrentProcessId(),  &
                                          GetCurrentThread(),     &
                                          GetCurrentThreadId(),   &
                                          procname,locst,loced,   &
                                          date_str,time_str,      &
                                          severity                )
          write(ERROR_UNIT,"(A)" ) "[ERROR]: in module_logger!!"
          write(ERROR_UNIT,*) errevent_t
!DIR$ IF (SHOW_CALLSTACK .EQ. 1)
          call TRACEBACKQQ("'TRACEBACKQQ:' has been called ",-1,STATUS=ret_stat)
          if(ret_stat .EQ. FOR$IOS_SUCCESS) then
             print*, "ret_stat=", ret_stat
          end if
!DIR$ ENDIF
          call log_error_terminate() 
    
    end subroutine
    
    !============================================50
    ! subroutine:
    !             log_error_terminate
    !============================================50
    subroutine log_error_terminate()
          ! Start of executable statements
          if(logger_stoponerror .EQ. .true.) then
             ERROR STOP '[FATAL-ERROR]: Terminating Program Execution!!'
          end if
    end subroutine
    
    !============================================50
    ! subroutine:
    !               log_set_stoponerror
    !============================================50
    subroutine log_set_stoponerror(value)
         
          logical(kind=int4), intent(in) :: value
          ! Start of executable sttements
          logger_stoponerror = value
    end subroutine
    
    !============================================50
    ! function:
    !           Returns a free logical unit.
    !============================================50  
    integer(kind=int4) function log_get_freeunit()
         
         
          ! Locals
          integer(kind=int4)            :: iunit,ios
          logical(kind=int4)            :: lopen,unit_found
          logical(kind=int4), parameter :: maxunits = 100
          ! Start of executable statements
          iunit = 0
          unit_found = .false.
          log_get_freeunit = 0
          do iunit = 1, maxunits
             if(iunit /= 5 .AND. iunit /= 6 .AND. iunit /= 9) then
                 inquire(UNIT=iunit,opened=lopen,iostat=ios)
                   if(ios == 0) then
                       if(.NOT. lopen) then
                           log_get_freeunit = iunit
                           unit_found = .true.
                           exit
                       end if
                   end if
             end if
          end do
          if(.NOT. unit_found) then
              write(ERROR_UNIT,*) "log_get_freeunit: No free logical unit available!!"
          end if
    end function 
    
    !============================================50
    !   subroutine:
    !                 logger_run
    !
    !   Purpose:
    !              Top-level wrapper subroutine
    !              for encapsulation of logger
    !              execution process.
    !============================================50
    subroutine logger_run(unit,event,filename, &
                          append,option,umsg  )
        
          integer(kind=int4),    intent(in) :: unit
          class(*),              intent(in) :: event
          character(len=*),      intent(in) :: filename
          logical(kind=int4),    intent(in) :: append
          character(len=*),      intent(in) :: option   ! Provide name of event e.g. WinError
          character(len=*),      intent(in) :: umsg
          ! Start of executable statements
          call log_startup(filename,append)
          select case (option)
          case ("UsrMsg")
              call log_UsrMsg(umsg)
          case ("PerfTimerEvent")
              call log_PerfTimerEvent(unit,event)
          case ("InvArgEvent")
              call log_InvArgEvent(unit,event)
          case ("FailAllocEvent")
              call log_FailAllocEvent(unit,event)
          case ("IndexOutBoundsEvent")
              call log_IndexOutBoundsEvent(unit,event)
          case ("FailDeallocEvent")
              call log_FailDeallocEvent(unit,event)
          case ("FileIOEvent")
              call log_FileIOEvent(unit,event)
          case ("DisasocPtrEvent")
              call log_DisassocPtrEvent(unit,event)
          case ("FPTrapUndEvent")
              call log_FPTrapUndEvent(unit,event)
          case ("FPTrapOvfEvent")
              call log_FPTrapOvfEvent(unit,event)
          case ("FPTrapDiv0Event")
              call log_FPTrapDiv0Event(unit,event)
          case ("FPTrapInvEvent")
              call log_FPTrapInvEvent(unit,event)
          case ("FPAbruptUndEvent")
              call log_FPAbruptUndEvent(unit,event)
          case ("FPAbruptOvfEvent")
              call log_FPAbruptOvfEvent(unit,event)
          case ("FPAbruptDiv0Event")
              call log_FPAbruptDiv0Event(unit,event)
          case ("FPAbruptInvEvent")
              call log_FPAbruptInvEvent(unit,event)
          case ("FPAbruptDmzEvent")
              call log_FPAbruptDmzEvent(unit,event)
          case ("FPPoleEvent")
              call log_FPPoleEvent(unit,event)
          case ("FPSingularEvent")
              call log_FPSingularEvent(unit,event)
          case ("FPDomErrEvent")
              call log_FPDomErrEvent(unit,event)
          case ("WinErrorEvent")
              call log_WinError(unit,event)
          case default
              call log_error("case default executed,", &
                             "procedure: logger_run", &
                             2857,2861,1)
          end select
          call log_shutdown()
          
    end subroutine
    
end module