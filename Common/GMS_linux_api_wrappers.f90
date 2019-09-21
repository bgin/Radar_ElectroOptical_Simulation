
module mod_linux_api_wrappers  

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
 !                      Minor: 0
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

     use mod_kinds,    only : int4
     use IFCORE,       only : TRACEBACKQQ
     use IFPORT,       only : SYSTEMQQ,GETLASTERRORQQ
    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
     integer(kind=int4), parameter, public :: MOD_LINUX_API_WRAPPERS_MAJOR = 1
     integer(kind=int4), parameter, public :: MOD_LINUX_API_WRAPPERS_MINOR = 0
     integer(kind=int4), parameter, public :: MOD_LINUX_API_WRAPPERS_MICRO = 0
     integer(kind=int4), parameter, public :: MOD_LINUX_API_WRAPPERS_FULLVER = 1000*MOD_LINUX_API_WRAPPERS_MAJOR + &
                                                                               100*MOD_LINUX_API_WRAPPERS_MINOR  + &
                                                                               10*MOD_LINUX_API_WRAPPERS_MICRO
     character(*),       parameter, public :: MOD_LINUX_API_WRAPPERS_CREATE_DATE = "21-09-2019 13:21 +00200 (SAT 21 SEP 2019 GMT+2)"
     character(*),       parameter, public :: MOD_LINUX_API_WRAPPERS_BUILD_DATE  = "00-00-0000 00:00"
     character(*),       parameter, public :: MOD_LINUX_API_WRAPPERS_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     character(*),       parameter, public :: MOD_LINUX_API_WRAPPERS_SYNOPSIS    = "Calling Linux command line programms from Fortran environment"

     contains

       subroutine Execute_chcpu(res,ret_val,verbose,command)
!DIR$  ATTRIBUTES CODE_ALIGN : 32 :: Execute_chcpu
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ....
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_chcpu: SYSTEMQQ failed to execute chcpu command -- reason: ",ret_val
           end if   
       end subroutine Execute_chcpu

       subroutine Execute_lscpu(res,ret_val,verbose,command)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: Execute_lscpu
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ....
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_lscpu: SYSTEMQQ failed to execute lscpu command -- reason: ",ret_val
           end if
       end subroutine Execute_lscpu

       subroutine Execute_cpudynd(res,ret_val,verbose,command)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: Execute_cpudynd
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ...
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_cpudynd: SYSTEMQQ failed to execute cpudynd command -- reason: ",ret_val
           end if
       end subroutine Execute_cpudynd

       subroutine Execute_cpufreq(res,ret_val,verbose,command)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: Execute_cpufreq
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ...
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_cpufreq: SYSTEMQQ failed to execute cpufreq command -- reason: ",ret_val
           end if
       end subroutine Execute_cpufreq

       subroutine Execute_cpuspeed(res,ret_val,verbose,command)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: Execute_cpuspeed
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ...
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_cpuspeed: SYSTEMQQ failed to execute cpuspeed command -- reason: ",ret_val
           end if
        end subroutine Execute_cpuspeed

        subroutine Execute_cpufreq_info(res,ret_val,verbose,command)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: Execute_cpufreq_info
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ...
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_cpufreq_info: SYSTEMQQ failed to execute cpudfreq-info command -- reason: ",ret_val
           end if 
        end subroutine Execute_cpufreq_info

        subroutine Execute_cpufreq_set(res,ret_val,verbose,command)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: Execute_cpufreq_set
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ...
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_cpufreq_set: SYSTEMQQ failed to execute cpufreq-set command -- reason: ",ret_val
           end if 
       end subroutine Execute_cpufreq_set

       subroutine Execute_cpuid(res,ret_val,verbose,command)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: Execute_cpuid
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ...
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_cpuid: SYSTEMQQ failed to execute cpuid command -- reason: ",ret_val
           end if  
       end subroutine Execute_cpuid

       subroutine Execute_cpupower(res,ret_val,verbose,command)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: Execute_cpupower
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ...
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_cpupower: SYSTEMQQ failed to execute cpupower command -- reason: ",ret_val
           end if 
       end subroutine Execute_cpupower

       subroutine Execute_cpupower_frequency_info(res,ret_val,verbose,command)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: Execute_cpupower_frequency_info
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ...
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_cpupower_frequency_info: SYSTEMQQ failed to execute cpupower-frequency-info command -- reason: ",ret_val
           end if 
       end subroutine Execute_cpupower_frequency_info

       subroutine Execute_cpupower_info(res,ret_val,verbose,command)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: Execute_cpupower_info
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ...
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_cpupower_info: SYSTEMQQ failed to execute cpupower-info command -- reason: ",ret_val
           end if
       end subroutine Execute_cpupower_info

       subroutine Execute_lshw(res,ret_val,verbose,command)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: Execute_lshw
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ...
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_lshw: SYSTEMQQ failed to execute lshw command -- reason: ",ret_val
           end if  
        end subroutine Execute_lshw

        subroutine Execute_pidstat(res,ret_val,verbose,command)
!DIR$ ATTRIBUTES CODE_ALIGN : 32 :: Execute_pidstat
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ...
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_pidstat: SYSTEMQQ failed to execute pidstat command -- reason: ",ret_val
           end if 
        end subroutine Execute_pidstat

        subroutine Execute_pmap(res,ret_val,verbose,command)
!DIR$  ATTRIBUTES CODE_ALIGN : 32 :: Execute_pmap
           logical(kind=int4),    intent(inout) :: res
           integer(kind=int4),    intent(inout) :: ret_val
           logical(kind=int4),    intent(in)    :: verbose
           character(len=128),    intent(in)    :: command
           ! Exec code ...
           res = SYSTEMQQ(command)
           if(res == .false.) then
              ret_val = GETLASTERRORQQ()
              if(verbose) print*, "Execute_pmap: SYSTEMQQ failed to execute pmap command -- reason: ",ret_val
           end if 
        end subroutine Execute_pmap
     
end module mod_linux_api_wrappers  
