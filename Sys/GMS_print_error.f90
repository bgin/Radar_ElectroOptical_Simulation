

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
 !                      Minor: 1
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
    
   
    use mod_kinds,    only : i4,i8
    use ISO_FORTRAN_ENV, only : stderr=>ERROR_UNIT, &
                                stdout=>OUTPUT_UNIT
   
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
    integer(kind=i4), parameter, public :: MOD_PRINT_ERROR_MAJOR = 1
    
    ! Minor version
    integer(kind=i4), parameter, public :: MOD_PRINT_ERROR_MINOR = 1
    
    ! Micro version
    integer(kind=i4), parameter, public :: MOD_PRINT_ERROR_MICRO = 0
    
    ! Module full version
    integer(kind=int4), parameter, public :: MOD_PRINT_ERROR_FULLVER = 1000*MOD_PRINT_ERROR_MAJOR + &
                                                                       100*MOD_PRINT_ERROR_MINOR  + &
                                                                       10*MOD_PRINT_ERROR_MICRO
    
    ! Module creation date
    character(*),  parameter, public :: MOD_PRINT_ERROR_CREATE_DATE =  "18-02-2018 10:03 +00200 (SUN 18 FEB 2018 GMT+2)"
    
   
    character(*),  parameter, public :: MOD_PRINT_ERROR_BUILD_DATE = __DATE__ " " __TIME__
    
    ! Module author info
    character(*),  parameter, public :: MOD_PRINT_ERROR_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
    
    ! Module short description
    character(*),  parameter, public :: MOD_PRINT_ERROR_DESCRIPT = "This module contains error printing subroutines."
    
    contains
    
    !===================================
    !   Print non fatal error info
    !===================================
    subroutine print_non_fatal_error(header,msg,loc,fname,vaddr)
          
          character(len=*),       intent(in)                :: header, msg
          
          integer(kind=i4),       intent(in)                :: loc
          character(len=*),       intent(in)                :: fname
          integer(kind=i8),       intent(in), optional      :: vaddr
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
          integer(kind=i4),        intent(in)              :: loc
          integer(kind=i8),        intent(in), optional    :: vaddr
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
    subroutine handle_fatal_memory_error(logging,lmsg)
#if defined(__INTEL_COMPILER) || defined(__ICC)   
      use IFCORE, only :  TRACEBACKQQ
#endif
          use M_journal, only : journal
        
          logical(kind=i4),    intent(in) :: logging
        
          character(len=*),      intent(in) :: lmsg ! message to logger
          
       
          
         
          ! Start of executable statements
          if(logging == .true.) then
             call journal('%','%Y-%M-%DT%h:%m:%s.%x%u:%b')
             call journal('t',lmsg)
          end if
#if defined(__INTEL_COMPILER) || defined(__ICC)   
           call TRACEBACKQQ(STRING="MEMORY-ALLOC/DEALLOC-ERROR", USER_EXIT_CODE= -1)
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
           call backtrace()
#endif
          ERROR STOP   
       end subroutine handle_fatal_memory_error
          
    !=====================================
    !  File I/O fatal error
    !  handler
    !=====================================                               
       subroutine handle_fatal_fileio_error(logging,lmsg)
#if defined(__INTEL_COMPILER) || defined(__ICC)   
      use IFCORE, only :  TRACEBACKQQ
#endif         
         use M_journal, only : journal
        
         logical(kind=i4),    intent(in) :: logging
        
         character(len=*),      intent(in) :: lmsg ! message to logger
       
         ! Start of executable statements
         if(logging == .true.) then
             call journal('%','%Y-%M-%DT%h:%m:%s.%x%u:%b')
             call journal('t',lmsg)
         end if
#if defined(__INTEL_COMPILER) || defined(__ICC)              
         call TRACEBACKQQ(STRING="File: " // trim(filerr) // " I/O Error", USER_EXIT_CODE= -1)
#elif defined __GFORTRAN__ && (!defined(__ICC) || !defined(__INTEL_COMPILER))
         call backtrace()
#endif         
         ERROR STOP 
    end subroutine handle_fatal_fileio_error
    
end module mod_print_error
