

module mod_msr_tools_wrapper


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         mod_msr_tools_wrapper
 !          
 !          Purpose:
 !                    This module contains a wrapper subroutines
 !                    around msr-tool package.
 !                   
 !                     
 !          History:
 !                        Date: 26-05-2019
 !                        Time: 12:39 GMT+2
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
 !                           Intel Processor's manuals
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

     use mod_kinds, only : int2, int4, int8b
     use mod_msr_architectural
     implicit none

     ! For calling msr-tools functions -- rdmsr and wrmsr
     character(*), parameter, public :: rd_cmd = "rdmsr"
     character(*), parameter, public :: wr_cmd = "wrmsr"
     !  

     contains

     subroutine AccessIA32_P5_MC_ADDR(reg,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_P5_MC_ADDR
           type(MSR_IA32_P5_MC_ADDR),  intent(in)    :: reg
           character(len=*),           intent(in)    :: command
           character(len=*),           intent(in)    :: fname
           integer(kind=int2),         intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat = RUNQQ(rd_cmd,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessIA32_P5_MC_ADDR

     subroutine AccessIA32_P5_TYPE(reg,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_P5_TYPE
           type(MSR_IA32_P5_TYPE),   intent(in)    :: reg
           character(len=*),         intent(in)    :: command
           character(len=*),         intent(in)    :: fname
           integer(kind=int2),       intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat = RUNQQ(rd_cmd,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessIA32_P5_TYPE

     subroutine AccessIA32_MONITOR_FILTER_SIZE(reg,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_MONITOR_FILTER_SIZE
           type(MSR_IA32_MONITOR_FILTER_SIZE),   intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
           stat = RUNQQ(rd_cmd,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessIA32_MONITOR_FILTER_SIZE
       
     subroutine AccessIA32_PLATFORM_ID(reg,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PLATFORM_ID
           type(MSR_IA32_PLATFORM_ID),           intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat = RUNQQ(rd_cmd,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessIA32_PLATFORM_ID

     subroutine AccessIA32_APIC_BASE(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_APIC_BASE
           type(MSR_IA32_APIC_BASE),     intent(in) :: reg
           character(len=5),             intent(in) :: op
           character(len=*),             intent(in) :: command
           character(len=*),             intent(in) :: fname
           integer(kind=int2),           intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
         
AccessMSR:    select case(op)
                  case("read")
                     string = command//reg.addr_hex//fname
                     stat = RUNQQ(rd_cmd,string)
                     if(stat == -1) then
                        ier = stat
                     end if
                  case("write")
                     string = command//reg.addr_hex//reg.msrw_hex
                     stat   = RUNQQ(wr_cmd,string)
                     if(stat == -1) then
                        ier = stat
                     end if
                  case default
                     print*, "AccessIA32_APIC_BASE: -- Invalid switch argument!"
                     return
              end select AccessMSR
     end subroutine AccessIA32_APIC_BASE

     subroutine AccessIA32_FEATURE_CONTROL(reg,op,command,fname,ier)
       !DIR$  ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_FEATURE_CONTROL
           type(MSR_IA32_FEATURE_CONTROL),       intent(in) :: reg
           character(len=5),                     intent(in) :: op
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
AccessMSR:   select case(op)
                case("read")
                   string = command//reg.addr_hex//fname
                   stat = RUNQQ(rd_cmd,string)
                   if(stat == -1) then
                      ier = stat
                   end if
                case("write")
                   string = command//reg.addr_hex//reg.msrw_hex
                   stat = RUNQQ(wr_cmd,string)
                   if(stat == -1) then
                      ier = stat
                   end if
                case default
                   print*, "AccessIA32_FEATURE_CONTROL: -- Invalid switch argument!!"
                   return
              end select AccessMSR
     end subroutine AccessIA32_FEATURE_CONTROL

     subroutine AccessIA32_TSC_ADJUST(reg,op,command,fname,hwth,val,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_TSC_ADJUST
           type(MSR_IA32_TSC_ADJUST),      intent(in) :: reg
           character(len=5),               intent(in) :: op
           character(len=*),               intent(in) :: command
           character(len=*),               intent(in) :: fname
           character(len=2),               intent(in) :: hwth
           character(len=16),              intent(in) :: val
           integer(kind=int2),             intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! EXec code
          
AccessMSR:     select case(op)
                   case("read")
                      string = command//reg.addr_hex//hwth//fname
                      stat = RUNQQ(rd_cmd,string)
                      if(stat == -1) then
                         ier = stat
                      end if
                   case("write")
                      string = command//hwth//reg.addr_hex//val
                      stat = RUNQQ(wr_cmd,string)
                      if(stat == -1) then
                         ier = stat
                      end if
                   case default
                      print*, "AccessIA32_TSC_ADJUST: -- Invalid switch argument!!"
                      return
                end select AccessMSR
     end subroutine AccessIA32_TSC_ADJUST

     subroutine AccessIA32_SPEC_CTRL(reg,op,command,fname,ier)
       !DIR$  ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_SPEC_CTRL
           type(MSR_IA32_SPEC_CTRL),       intent(in) :: reg
           character(len=5),               intent(in) :: op
           character(len=*),               intent(in) :: command
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
        
AccessMSR:    select case(op)
                  case("read")
                     string = command//reg.addr_hex//fname
                     stat = RUNQQ(rd_cmd,string)
                     if(stat == -1) then
                        ier = stat
                     end if
                  case("write")
                     string = command//reg.addr_hex/reg.msrw_hex
                     stat   = RUNQQ(wr_cmd,string)
                     if(stat == -1) then
                        ier = stat
                     end if
                  case default
                     print*, "AccessIA32_SPEC_CTRL: -- Invalid switch argument!!"
                     return
                  end select AccessMSR
     end subroutine AccessIA32_SPEC_CTRL

     subroutine AccessIA32_PRED_CMD(reg,op,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PRED_CMD
           type(MSR_IA32_PRED_CMD),         intent(in) :: reg
           character(len=5),                intent(in) :: op
           character(len=*),                intent(in) :: command
           character(len=*),                intent(in) :: fname
           integer(kind=int2),              intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
         
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PRED_CMD: -- Invalid switch argument!!"
                       return
               end select AccessMSR
     end subroutine AccessIA32_PRED_CMD

     subroutine AccessIA32_BIOS_UPDT_TRIG(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_BIOS_UPDT_TRIG
           type(MSR_IA32_BIOS_UPDT_TRIG),          intent(in) :: reg
           character(len=5),                       intent(in) :: op
           character(len=*),                       intent(in) :: command
           character(len=*),                       intent(in) :: fname
           integer(kind=int2),                     intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
          
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_BIOS_UPDT_TRIG: -- Invalid switch argument!!"
                       return
               end select AccessMSR           
     end subroutine AccessIA32_BIOS_UPDT_TRIG

     subroutine AccessIA32_BIOS_SIGN_ID(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_BIOS_SIGN_ID
           type(MSR_IA32_BIOS_SIGN_ID),          intent(in) :: reg
           character(len=5),                     intent(in) :: op
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           integer(kind=int2),                   intent(inout) :: ier
           ! Locls
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
      
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_BIOS_SIGN_ID: -- Invalid switch argument!!"
                       return
               end select AccessMSR     
     end subroutine AccessIA32_BIOS_SIGN_ID

     subroutine AccessIA32_SGXLEPUBKEYHASH0(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_SGXLEPUBKEYHASH0
           type(MSR_IA32_SGXLEPUBKEYHASH0),      intent(in) :: reg
           character(len=5),                     intent(in) :: op
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           integer(kind=int2),                   intent(inout) :: ier
           ! Local
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
         
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_SGXLEPUBKEYHASH0: -- Invalid switch argument!!"
                       return
               end select AccessMSR     
     end subroutine AccessIA32_SGXLEPUBKEYHASH0

     subroutine AccessIA32_SGXLEPUBKEYHASH1(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_SGXLEPUBKEYHASH1
           type(MSR_IA32_SGXLEPUBKEYHASH1),         intent(in) :: reg
           character(len=5),                        intent(in) :: op
           character(len=*),                        intent(in) :: command
           character(len=*),                        intent(in) :: fname
           integer(kind=int2),                      intent(inout) :: ier
           ! LOcals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
       
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_SGXLEPUBKEYHASH1: -- Invalid switch argument!!"
                       return
               end select AccessMSR               
     end subroutine AccessIA32_SGXLEPUBKEYHASH1

     subroutine AccessIA32_SGXLEPUBKEYHASH2(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_SGXLEPUBKEYHASH2
           type(MSR_IA32_SGXLEPUBKEYHASH2),       intent(in) :: reg
           character(len=5),                      intent(in) :: op
           character(len=*),                      intent(in) :: command
           character(len=*),                      intent(in) :: fname
           integer(kind=int2),                    intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_SGXLEPUBKEYHASH2: -- Invalid switch argument!!"
                       return
               end select AccessMSR      
     end subroutine AccessIA32_SGXLEPUBKEYHASH2

     subroutine AccessIA32_SGXLEPUBKEYHASH3(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_SGXLEPUBKEYHASH3
           type(MSR_IA32_SGXLEPUBKEYHASH3),       intent(in) :: reg
           character(len=5),                      intent(in) :: op
           character(len=*),                      intent(in) :: command
           character(len=*),                      intent(in) :: fname
           integer(kind=int2),                    intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_SGXLEPUBKEYHASH3: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_SGXLEPUBKEYHASH3

     subroutine  AccesIA32_SMM_MONITOR_CTL(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_SMM_MONITOR_CTL
           type(MSR_IA32_SMM_MONITOR_CTL),         intent(in) :: reg
           character(len=5),                       intent(in) :: op
           character(len=*),                       intent(in) :: command
           character(len=*),                       intent(in) :: fname
           integer(kind=int2),                     intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_SMM_MONITOR_CTL: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccesIA32_SMM_MONITOR_CTL

     subroutine AccessIA32_PMC0(reg,op,command,val,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC0
           type(MSR_IA32_PMC0),            intent(in) :: reg
           character(len=5),               intent(in) :: op
           character(len=*),               intent(in) :: command
           character(len=16),              intent(in) :: val
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
           ! LOcals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC0: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC0

     subroutine AccessIA32_PMC1(reg,op,command,val,fname,ier)
       !DIR$  ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC1
           type(MSR_IA32_PMC1),            intent(in)  :: reg
           character(len=5),               intent(in)  :: op
           character(len=*),               intent(in)  :: command
           character(len=16),              intent(in)  :: val
           character(len=*),               intent(in)  :: fname
           integer(kind=int2),             intent(inout) :: ier
           ! LOcals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC1: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC1

     subroutine AccessIA32_PMC2(reg,op,command,val,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC2
           type(MSR_IA32_PMC2),            intent(in) :: reg
           character(len=5),               intent(in) :: op
           character(len=*),               intent(in) :: command
           character(len=16),              intent(in) :: val
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC2: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC2

     subroutine AccessIA32_PMC3(reg,op,command,val,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC3
           type(MSR_IA32_PMC3),         intent(in) :: reg
           character(len=5),            intent(in) :: op
           character(len=*),            intent(in) :: command
           character(len=16),           intent(in) :: val
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC3: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC3

     subroutine AccessIA32_PMC4(reg,op,command,val,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC4
           type(MSR_IA32_PMC4),         intent(in) :: reg
           character(len=5),            intent(in) :: op
           character(len=*),            intent(in) :: command
           character(len=16),           intent(in) :: val
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC4: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC4

     subroutine AccessIA32_PMC5(reg,op,command,val,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC5
           type(MSR_IA32_PMC5),           intent(in) :: reg
           character(len=5),              intent(in) :: op
           character(len=*),              intent(in) :: command
           character(len=16),             intent(in) :: val
           character(len=*),              intent(in) :: fname
           integer(kind=int2),            intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC5: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
      end subroutine AccessIA32_PMC5

      subroutine AccessIA32_PMC6(reg,op,command,val,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC6
           type(MSR_IA32_PMC6),            intent(in) :: reg
           character(len=5),               intent(in) :: op
           character(len=*),               intent(in) :: command
           character(len=16),              intent(in) :: val
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(in) :: ier
           ! LOcals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC6: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
      end subroutine AccessIA32_PMC6

      subroutine AccessIA32_PMC7(reg,op,command,val,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC7
           type(MSR_IA32_PMC7),              intent(in) :: reg
           character(len=5),                 intent(in) :: op
           character(len=*),                 intent(in) :: command
           character(len=16),                intent(in) :: val
           character(len=*),                 intent(in) :: fname
           integer(kind=int2),               intent(inout) :: ier
           ! LOcals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC7: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC7

     subroutine AccessIA32_UMWAIT_CONTROL(reg,op,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_UMWAIT_CONTROL
           type(MSR_IA32_UMWAIT_CONTROL),           intent(in) :: reg
           character(len=5),                        intent(in) :: op
           character(len=*),                        intent(in) :: command
           character(len=*),                        intent(in) :: fname
           integer(kind=int2),                      intent(inout) :: ier
            ! LOcals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_UMWAIT_CONTROL: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_UMWAIT_CONTROL

     subroutine AccessIA32_MTRRCAP(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_MTRRCAP
           type(MSR_IA32_MTRRCAP),          intent(in) :: reg
           character(len=*),                intent(in) :: command
           character(len=*),                intent(in) :: fname
           integer(kind=int2),              intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rd_cmd,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessIA32_MTRRCAP

     subroutine AccessIA32_ARCH_CAPABILITIES(reg,command,fname,ier)
       !DIR$  ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_ARCH_CAPABILITIES
           type(MSR_IA32_ARCH_CAPABILITIES),          intent(in) :: reg
           character(len=*),                          intent(in) :: command
           character(len=*),                          intent(in) :: fname
           integer(kind=int2),                        intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rd_cmd,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessIA32_ARCH_CAPABILITIES

     subroutine AccessIA32_FLUSH_CMD(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_FLUSH_CMD
           type(MSR_IA32_FLUSH_CMD),           intent(in) :: reg
           character(len=5),                   intent(in) :: op
           character(len=*),                   intent(in) :: command
           character(len=*),                   intent(in) :: fname
           integer(kind=int2),                 intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
  ! Exec code ....
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_FLUSH_CMD: -- Invalid switch argument!!"
                       return
               end select AccessMSR            
     end subroutine AccessIA32_FLUSH_CMD




end module mod_msr_tools_wrapper
