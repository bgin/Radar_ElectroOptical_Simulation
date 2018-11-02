
#include "Config.fpp"
module mod_print_error

  !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_print_error'
 !          
 !          Purpose:
 !                    This module contains an implementation
 !                    of error printing subroutines
 !          History:
 !                        Date: 18-02-2018
 !                        Time: 09:56 GMT+2
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
 !                Nonw
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    
 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.
    
   
    use mod_kinds,    only : int4, int8
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT, &
                                stdout=>OUTPUT_UNIT
    use mod_logger,   only :    log_startup,   &
                                log_UsrMsg,    &
                                log_shutdown
    implicit none
    
    public :: print_non_fatal_error
    public :: print_fatal_error
    public :: handle_fatal_memory_error
    public :: handle_fatal_fileio_error
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
    ! Major version
    integer(kind=int4), parameter, public :: MOD_PRINT_ERROR_MAJOR = 1_I32P
    
    ! Minor version
    integer(kind=int4), parameter, public :: MOD_PRINT_ERROR_MINOR = 0_I32P
    
    ! Micro version
    integer(kind=int4), parameter, public :: MOD_PRINT_ERROR_MICRO = 0_I32P
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_PRINT_ERROR_FULLVER = 1000_int4*MOD_PRINT_ERROR_MAJOR + &
                                                                       100_int4*MOD_PRINT_ERROR_MINOR  + &
                                                                       10_int4*MOD_PRINT_ERROR_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_PRINT_ERROR_CREATE_DATE =  "18-02-2018 10:03 +00200 (SUN 18 FEB 2018 GMT+2)"
    
    ! Moudle build date (should be set after successful compilation)
    character(*),  parameter, public :: MOD_PRINT_ERROR_BUILD_DATE = " "
    
    ! Module author info
    character(*),  parameter, public :: MOD_PRINT_ERROR_AUTHOR = "Programmer: Bernard Gingold, e-mail: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_PRINT_ERROR_DESCRIPT = "This module contains error printing subroutines."
    
    contains
    
    !===================================
    !   Print non fatal error info
    !===================================
    subroutine print_non_fatal_error(header,msg,loc,fname,vaddr)
          
          character(len=*),       intent(in)              :: header, msg
          
          integer(kind=int4),     intent(in)              :: loc
          character(len=*),       intent(in)              :: fname
          integer(kind=int8),     intent(in), optional    :: vaddr
          ! Locals
          character(len=10) :: t
          character(len=8)  :: d
          ! Start of executable satatements
          call DATE_AND_TIME(date=d,time=t)
          write(stderr,*) header
          write(stderr,*) msg
          if(present(vaddr)) then
              write(stderr,*) "!!!<Non-Fatal Error>!!! in: -- File: ", fname, "at loc:", loc, "address: ", vaddr,  &
                              " occured at: ",  &
                                     d(1:4),"-",d(5:6),"-",d(7:8), " ", &
                                     t(1:2),":",t(3:4),":",t(5:6)
          else 
              write(stderr,*) "!!!<Non-Fatal Error>!!! in: -- File: ", fname, "at loc:", loc,  &
                              " occured at: ",  &
                                     d(1:4),"-",d(5:6),"-",d(7:8), " ", &
                                     t(1:2),":",t(3:4),":",t(5:6)
          end if
          write(stderr,*) header
          
    end subroutine print_non_fatal_error
    
    !====================================
    !  Print fatal error info
    !====================================
    subroutine print_fatal_error(header,msg,smsg,fname,loc,vaddr)
          
          
          character(len=*),        intent(in)              :: header, msg
          character(len=256),      intent(in)              :: smsg
          character(len=*),        intent(in)              :: fname
          integer(kind=int4),      intent(in)              :: loc
          integer(kind=int8),      intent(in), optional    :: vaddr
          ! Locals
          character(len=10) :: stime
          character(len=8)  :: sdate
          ! Start of executable statements
          call DATE_AND_TIME(date=sdate,time=stime)
          write(stderr,*) header
          write(stderr,*) "User message:   ",  msg
          write(stderr,*) "System message: ",  smsg
          if(present(vaddr)) then
              write(stderr,*) "!!!<Fatal Error>!!! at: -- Loc: ", loc, "address: ", vaddr,  &
                          " occured at: ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6)
          else
              write(stderr,*) "!!!<Fatal Error>!!! in file: ", fname,  "at: -- Loc: ", loc,   &
                          " occured at: ",  &
                                     sdate(1:4),"-",sdate(5:6),"-",sdate(7:8), " ", &
                                     stime(1:2),":",stime(3:4),":",stime(5:6) 
          end if
          write(stderr,*) header
          
    end subroutine
    
    !=====================================
    !  Allocation/Deallocation fatal error
    !  handler
    !=====================================
    subroutine handle_fatal_memory_error(logging,verbose,append,fname,lmsg,umsg,emsg,line)
          use IFPORT, only :  TRACEBACKQQ
          logical(kind=int4),    intent(in) :: logging, verbose,append
          character(len=*),      intent(in) :: fname
          character(len=*),      intent(in) :: lmsg ! message to logger
          
          character(len=*),      intent(in) :: umsg ! user message 
          character(len=*),      intent(in) :: emsg ! system message
          integer(kind=int4),    intent(in) :: line
          ! Locals
          character(len=10) ::  stime
          character(len=8)  ::  sdate
          ! Start of executable statements
          if(logging == .true.) then
              call log_startup(fname,append)
              call log_UsrMsg(lmsg)
              call log_shutdown()
          else if (verbose == .true. ) then
              call print_fatal_error( "========================= FATAL =========================", &
                                      umsg, emsg, sdate, stime, __LINE__ )
          end if
          call TRACEBACKQQ(STRING="MEMORY-ALLOC/DEALLOC-ERROR", USER_EXIT_CODE= -1)
          ERROR STOP umsg    
       end subroutine handle_fatal_mem_error
          
    !=====================================
    !  File I/O fatal error
    !  handler
    !=====================================                               
    subroutine handle_fatal_fileio_error(logging,verbose,append,fname,filerr,lmsg,umsg,emsg,line)
         use IFPORT, only :  TRACEBACKQQ
         logical(kind=int4),    intent(in) :: logging,verbose,append
         character(len=*),      intent(in) :: fname ! name file to written (logger)
         character(len=*),      intent(in) :: filerr ! name of file which failed to be opened
         character(len=*),      intent(in) :: lmsg ! message to logger
         character(len=*),      intent(in) :: umsg ! user message 
         character(len=*),      intent(in) :: emsg ! system message
         integer(kind=int4),    intent(in) :: line 
         ! Locals
         character(len=10) ::  stime
         character(len=8)  ::  sdate
         ! Start of executable statements
         if(logging == .true.) then
              call log_startup(fname,append)
              call log_UsrMsg(lmsg)
              call log_shutdown()
         else if (verbose == .true. ) then
              call print_fatal_error( "========================= FATAL =========================", &
                                      umsg, emsg, sdate, stime, __LINE__ )
         end if
         call TRACEBACKQQ(STRING="File: " // trim(filerr) // " I/O Error", USER_EXIT_CODE= -1)
         ERROR STOP umsg // trim(filerr) 
    end subroutine handle_fatal_fileio_error
    
end module mod_print_error