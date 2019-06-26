
module mod_zen_msrtools_wrapper


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         mod_zen_msrtools_wrapper
 !          
 !          Purpose:
 !                    This module contains a wrapper subroutines
 !                    around msr-tool package for the Zen CPU.
 !                   
 !                     
 !          History:
 !                        Date: 24-06-2019
 !                        Time: 17:12 GMT+2
 !          
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !    
 !          Author: 
 !                    Bernard Gingold
 !          
  !          References:
 !                          AMD Zen Processor's manuals
 !                         ( AMD 'Open-Source Register Reference')
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

     use mod_kinds, only : int1,int2, int4, int8b
     use mod_zen_msr
     implicit none

     character(*), parameter, private :: rmsr = "rdmsr"
     character(*), parameter, private :: wmsr = "wrmsr"
     character(*), parameter, public  :: reset_val = "0x000000000000000"
     contains

     subroutine AccessMSR_TSC_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_TSC_ZEN
       type(MSR_TSC_ZEN),    intent(in) :: reg
       character(len=*),     intent(in)    :: command
       logical(kind=int1),   intent(in)    :: reset
       character(len=*),     intent(in)    :: fname
       character(len=2),     intent(in)    :: hwth ! HW thread number
       integer(kind=int2),   intent(inout) :: ier
       ! Locals
       character(len=128), automatic :: string
       integer(kind=int2), automatic :: stat
       ! Exec code ....
       if(.not. reset) then
          string = command//hwth//reg.addr_hex//fname
          stat = RUNQQ(rmsr,string)
          if(stat == -1) then
             ier = stat
          end if
       else
          string = command//hwth//reg.addr_hex//reset_val
          stat = RUNQQ(wmsr,string)
          if(stat == -1) then
             ier = stat
          end if
       end if
     end subroutine AccessMSR_TSC_ZEN

     subroutine AccessMSR_APIC_BAR_ZEN(reg,command,fname,core,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_APIC_BAR_ZEN
           type(MSR_APIC_BAR_ZEN),       intent(in) :: reg
           character(len=*),             intent(in) :: command
           character(len=*),             intent(in) :: fname
           character(len=2),             intent(in) :: core
           integer(kind=int2),           intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
                ier = stat
           end if
     end subroutine AccessMSR_APIC_BAR_ZEN

     subroutine AccessMSR_MPERF_ZEN(reg,command,reset,hwth,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MPERF_ZEN
           type(MSR_MPERF_ZEN),      intent(in) :: reg
           character(len=*),         intent(in) :: command
           logical(kind=int1),       intent(in) :: reset
           character(len=2),         intent(in) :: hwth
           character(len=*),         intent(in) :: fname
           integer(kind=int2),       intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//hwth//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//hwth//reg.addr_hex//reset_val
              stat   = RUNQQ(wrmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           end if
     end subroutine AccessMSR_MPERF_ZEN
     
     subroutine AccessMSR_APERF_ZEN(reg,command,reset,hwth,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_APERF_ZEN
           type(MSR_APERF_ZEN),      intent(in) :: reg
           character(len=*),         intent(in) :: command
           logical(kind=int1),       intent(in) :: reset
           character(len=2),         intent(in) :: hwth
           character(len=*),         intent(in) :: fname
           integer(kind=int2),       intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//hwth//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//hwth//reg.addr_hex//reset_val
              stat   = RUNQQ(wrmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           end if
     end subroutine AccessMSR_APERF_ZEN

     subroutine AccessMSR_MTRR_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_ZEN
           type(MSR_MTRR_ZEN),      intent(in) :: reg
           character(len=*),        intent(in) :: command
           character(len=2),        intent(in) :: core
           character(len=*),        intent(in) :: fname
           integer(kind=int2),      intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_MTRR_ZEN

     subroutine AccessMSR_MCG_CAP_ZEN(reg,command,hwth,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MCG_CAP_ZEN
           type(MSR_MCG_CAP_ZEN),     intent(in) :: reg
           character(len=*),          intent(in) :: command
           character(len=2),          intent(in) :: hwth
           character(len=*),          intent(in) :: fname
           integer(kind=int2),        intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//hwth//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_MCG_CAP_ZEN

     subroutine AccessMSR_MCG_STAT_ZEN(reg,command,hwth,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MCG_STAT_ZEN
           type(MSR_MCG_STAT_ZEN),    intent(in) :: reg
           character(len=*),          intent(in) :: command
           character(len=2),          intent(in) :: hwth
           character(len=*),          intent(in) :: fname
           integer(kind=int2),        intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//hwth//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
       
     end subroutine AccessMSR_MCG_STAT_ZEN

     subroutine AccessMSR_MCG_CTL_ZEN(reg,command,hwth,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MCG_CTL_ZEN
           type(MSR_MCG_CTL_ZEN),     intent(in) :: reg
           character(len=*),          intent(in) :: command
           character(len=2),          intent(in) :: hwth
           character(len=*),          intent(in) :: fname
           integer(kind=int2),        intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//hwth//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_MCG_CTL_ZEN   

     subroutine AccessMSR_DBG_CTL_ZEN(reg,command,hwth,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_DBG_CTL_ZEN
           type(MSR_DBG_CTL_ZEN),       intent(in) :: reg
           character(len=*),            intent(in) :: command
           character(len=2),            intent(in) :: hwth
           logical(kind=int1),          intent(in) :: reset
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//hwth//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//hwth//reg.addr_hex//reset_val
              stat   = RUNQQ(wrmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           end if
     end subroutine AccessMSR_DBG_CTL_ZEN

     subroutine AccessMSR_BR_FROM_IP_ZEN(reg,command,hwth,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_BR_FROM_IP_ZEN
           type(MSR_BR_FROM_IP_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
           character(len=2),            intent(in) :: hwth
           logical(kind=int1),          intent(in) :: reset
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//hwth//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//hwth//reg.addr_hex//reset_val
              stat   = RUNQQ(wrmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           end if
     end subroutine AccessMSR_BR_FROM_IP_ZEN

     subroutine AccessMSR_BR_TO_IP_ZEN(reg,command,hwth,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_BR_TO_IP_ZEN
           type(MSR_BR_TO_IP_ZEN),      intent(in) :: reg
           character(len=*),            intent(in) :: command
           character(len=2),            intent(in) :: hwth
           logical(kind=int1),          intent(in) :: reset
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//hwth//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//hwth//reg.addr_hex//reset_val
              stat   = RUNQQ(wrmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           end if
     end subroutine AccessMSR_BR_TO_IP_ZEN

     subroutine AccessMSR_LAST_EXP_FROM_IP_ZEN(reg,command,hwth,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_LAST_EXP_FROM_IP_ZEN
           type(MSR_LAST_EXP_FROM_IP_ZEN),    intent(in) :: reg
           character(len=*),                  intent(in) :: command
           character(len=2),                  intent(in) :: hwth
           logical(kind=int1),                intent(in) :: reset
           character(len=*),                  intent(in) :: fname
           integer(kind=int2),                intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//hwth//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//hwth//reg.addr_hex//reset_val
              stat   = RUNQQ(wrmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           end if
     end subroutine AccessMSR_LAST_EXP_FROM_IP_ZEN

     subroutine AccessMSR_LAST_EXP_TO_IP_ZEN(reg,command,hwth,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_LAST_EXP_TO_IP_ZEN
           type(MSR_LAST_EXP_TO_IP_ZEN),       intent(in) :: REG
           character(len=*),                   intent(in) :: command
           character(len=2),                   intent(in) :: hwth
           logical(kind=int1),                 intent(in) :: reset
           character(len=*),                   intent(in) :: fname
           integer(kind=int2),                 intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//hwth//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//hwth//reg.addr_hex//reset_val
              stat   = RUNQQ(wrmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           end if
     end subroutine AccessMSR_LAST_EXP_TO_IP_ZEN

     subroutine AccessMSR_MTRR_FIXED_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED_ZEN
           type(MSR_MTRR_FIXED_ZEN),   intent(in) :: reg
           character(len=*),           intent(in) :: command
           character(len=2),           intent(in) :: core
           character(len=*),           intent(in) :: fname
           integer(kind=int2),         intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_MTRR_FIXED_ZEN

     
     


end module mod_zen_msrtools_wrapper
