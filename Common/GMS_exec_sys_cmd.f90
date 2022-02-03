
module exec_sys_cmd 

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_linux_api_wrappers'
 !          
 !          Purpose:
  !                         Calling Linux command line programms from Fortran
 !                          environment.
 !                       
 !          History:
 !                        Date: 21-09-2018
 !                        Time: 13:21 GMT+2
 !                       
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 1
 !                      Micro: 0
 !
 !          Author:  
 !                      Bernard Gingold
 !          
 !                 
 !          References:
 !         
 !                          None
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

     use mod_kinds,    only : i4
#if defined(__ICC) || defined(__INTEL_COMPILER)  
     use IFCORE,       only : TRACEBACKQQ
     use IFPORT,       only : SYSTEMQQ,GETLASTERRORQQ
#else
#error "Intel Fortran Compiler required, but none has been found!!"
#endif
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
     integer(kind=i4), parameter, public :: MOD_LINUX_API_WRAPPERS_MAJOR = 1
     integer(kind=i4), parameter, public :: MOD_LINUX_API_WRAPPERS_MINOR = 1
     integer(kind=i4), parameter, public :: MOD_LINUX_API_WRAPPERS_MICRO = 0
     integer(kind=i4), parameter, public :: MOD_LINUX_API_WRAPPERS_FULLVER = 1000*MOD_LINUX_API_WRAPPERS_MAJOR + &
                                                                               100*MOD_LINUX_API_WRAPPERS_MINOR  + &
                                                                               10*MOD_LINUX_API_WRAPPERS_MICRO
     character(*),       parameter, public :: MOD_LINUX_API_WRAPPERS_CREATE_DATE = "21-09-2019 13:21 +00200 (SAT 21 SEP 2019 GMT+2)"
     character(*),       parameter, public :: MOD_LINUX_API_WRAPPERS_BUILD_DATE  = __DATE__ ":" __TIME__
     character(*),       parameter, public :: MOD_LINUX_API_WRAPPERS_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     character(*),       parameter, public :: MOD_LINUX_API_WRAPPERS_SYNOPSIS    = "Calling Linux command line programms from Fortran environment"

     contains

       subroutine ExecSysCmd(res,ret_val,verbose,command)
!DIR$  ATTRIBUTES CODE_ALIGN : 32 :: ExecSysCmd
           logical(kind=i4),    intent(inout) :: res
           integer(kind=i4),    intent(inout) :: ret_val
           logical(kind=i4),    intent(in)    :: verbose
           character(len=1024), intent(in)    :: command
           ! Exec code ....
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) then
                 print*, "ExecSysCmd: requested command to run:", command
                 print*, "ExecSysCmd: running command error:  " , ret_val
           end if   
       end subroutine ExecSysCmd

     
end module exec_sys_cmd 
