

module mod_intel_msrtool_wrappers


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

       !==================================================================================================================120
     ! File and module data
       integer(kind=int4),  parameter :: MOD_INTEL_MSRTOOL_WRAPPERS_MAJOR = 1
       integer(kind=int4),  parameter :: MOD_INTEL_MSRTOOL_WRAPPERS_MINOR = 0
       integer(kind=int4),  parameter :: MOD_INTEL_MSRTOOL_WRAPPERS_MICRO = 0
       integer(kind=int4),  parameter :: MOD_INTEL_MSRTOOL_WRAPPERS_FULLVER = 1000*MOD_INTEL_MSRTOOL_WRAPPERS_MAJOR + &
                                                                              100*MOD_INTEL_MSRTOOL_WRAPPERS_MINOR  + &
                                                                              10*MOD_INTEL_MSRTOOL_WRAPPERS_MICRO
       character(*),        parameter :: MOD_INTEL_MSRTOOL_WRAPPERS_CREATION_DATE = "05-08-2019 10:43 +00200 (MON 05 AUG 2019 GMT+2)"
       character(*),        parameter :: MOD_INTEL_MSRTOOL_WRAPPERS_BUILD_DATE    = "00-00-0000 00:00"
       character(*),        parameter :: MOD_INTEL_MSRTOOL_WRAPPERS_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
       character(*),        parameter :: MOD_INTEL_MSRTOOL_WRAPPERS_SYNOPSIS      = "Wrapper subroutines around msr-tools (Intel Architectural MSRs)"
     !==================================================================================================================120


     
     character(*), parameter, public  :: reset_val = "0x000000000000000"
     integer(kind=int8b), parameter, public :: init_val = Z"FFFFFFFFFFFFFFF"
     integer(kind=int8b), parameter, public :: zero_val = Z"000000000000000"
     character(*), parameter, public  :: init_valh      = "0xFFFFFFFFFFFFFFFF"

     ! For calling msr-tools functions -- rdmsr and wrmsr
     character(*), parameter, public :: rd_cmd = "rdmsr"
     character(*), parameter, public :: wr_cmd = "wrmsr"
     !  

     contains

      !DIR$ ATTRIBUTES INLINE :: initIA32_P5_MC_ADDR
     subroutine initIA32_P5_MC_ADDR(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_P5_MC_ADDR
           type(MSR_IA32_P5_MC_ADDR),   intent(inout) :: msr
           ! Exec code ....
           msr.msr_read = zero_val
     end subroutine initIA32_P5_MC_ADDR

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

     subroutine ReadIA32_P5_MC_ADDR(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_P5_MC_ADDR
           type(MSR_IA32_P5_MC_ADDR),    intent(inout) :: msr
           integer(kind=int4),           intent(in)    :: iounit
           character(len=*),             intent(in)    :: fname
           integer(kind=int2),           intent(in)    :: status
           integer(kind=int4),           intent(in)    :: err
           character(len=256),           intent(in)    :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read
           if(ioerr > 0 .or. ioerr < 0) goto 9999
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadIA32_P5_MC_ADDR         

      !DIR$ ATTRIBUTES INLINE :: initIA32_P5_TYPE
     subroutine initIA32_P5_TYPE(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_P5_TYPE
           type(MSR_IA32_P5_TYPE),   intent(inout) :: msr
           ! Exec code .....
           msr.msr_read = zero_val
     end subroutine initIA32_P5_TYPE    

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

     subroutine ReadIA32_P5_TYPE(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_P5_TYPE
           type(MSR_IA32_P5_TYPE),       intent(inout) :: msr
           integer(kind=int4),           intent(in)    :: iounit
           character(len=*),             intent(in)    :: fname
           integer(kind=int2),           intent(in)    :: status
           integer(kind=int4),           intent(in)    :: err
           character(len=256),           intent(in)    :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read
           if(ioerr > 0 .or. ioerr < 0) goto 9999
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadIA32_P5_TYPE

      !DIR$ ATTRIBUTES INLINE :: initIA32_MONITOR_FILTER_SIZE
     subroutine initIA32_MONITOR_FILTER_SIZE(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_MONITOR_FILTER_SIZE
           type(MSR_IA32_MONITOR_FILTER_SIZE),   intent(inout) :: msr
           ! Exec code ....
           msr.msr_read = zero_val
     end subroutine    

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

     subroutine ReadIA32_MONITOR_FILTER_SIZE(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_MONITOR_FILTER_SIZE
           type(MSR_IA32_MONITOR_FILTER_SIZE),     intent(inout) :: msr
           integer(kind=int4),                     intent(in)    :: iounit
           character(len=*),                       intent(in)    :: fname
           integer(kind=int2),                     intent(in)    :: status
           integer(kind=int4),                     intent(in)    :: err
           character(len=256),                     intent(in)    :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read
           if(ioerr > 0 .or. ioerr < 0) goto 9999
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadIA32_MONITOR_FILTER_SIZE

      !DIR$ ATTRIBUTES INLINE :: initIA32_PLATFORM_ID
     subroutine initIA32_PLATFORM_ID(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_PLATFORM_ID
           type(MSR_IA32_PLATFORM_ID),     intent(inout) :: msr
           ! Exec code ,....
           msr.msr_read = zero_val
     end subroutine initIA32_PLATFORM_ID    
       
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

     subroutine ReadIA32_PLATFORM_ID(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_PLATFORM_ID
           type(MSR_IA32_PLATFORM_ID),   intent(inout) :: msr
           integer(kind=int4),           intent(in)    :: iounit
           character(len=*),             intent(in)    :: fname
           integer(kind=int2),           intent(in)    :: status
           integer(kind=int4),           intent(in)    :: err
           character(len=256),           intent(in)    :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read
           if(ioerr > 0 .or. ioerr < 0) goto 9999
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadIA32_PLATFORM_ID

     !DIR$ ATTRIBUTES INLINE :: initIA32_APIC_BASE
     subroutine initIA32_APIC_BASE(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_APIC_BASE
           type(MSR_IA32_APIC_BASE),      intent(inout) :: msr
           ! Exec code ....
           msr.msr_read = zero_val
     end subroutine initIA32_APIC_BASE    

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

     subroutine ReadIA32_APIC_BASE(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_APIC_BASE
           type(MSR_IA32_APIC_BASE),     intent(inout) :: msr
           integer(kind=int4),           intent(in)    :: iounit
           character(len=*),             intent(in)    :: fname
           integer(kind=int2),           intent(in)    :: status
           integer(kind=int4),           intent(in)    :: err
           character(len=256),           intent(in)    :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read
           if(ioerr > 0 .or. ioerr < 0) goto 9999
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadIA32_APIC_BASE

     !DIR$ ATTRIBUTES INLINE :: initIA32_FEATURE_CONTROL
     subroutine initIA32_FEATURE_CONTROL(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_FEATURE_CONTROL
           type(MSR_IA32_FEATURE_CONTROL),     intent(inout) :: msr
           ! Exec code
           msr.msr_read = zero_val
     end subroutine initIA32_FEATURE_CONTROL

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

     subroutine ReadIA32_FEATURE_CONTROL(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_FEATURE_CONTROL
           type(MSR_IA32_FEATURE_CONTROL),        intent(inout) :: msr
           integer(kind=int4),                    intent(in)    :: iounit
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(in)    :: err
           character(len=256),                    intent(in)    :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read
           if(ioerr > 0 .or. ioerr < 0) goto 9999
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadIA32_FEATURE_CONTROL

     !DIR$ ATTRIBUTES INLINE :: initIA32_TSC_ADJUST
     subroutine initIA32_TSC_ADJUST(msr)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_TSC_ADJUST
           type(MSR_IA32_TSC_ADJUST),        intent(inout) :: msr
       ! EXec code ....
           msr.msr_read   = zero_val
           msr.msr_write  = zero_val
           msr.msrw_hex   = init_valh
     end subroutine initIA32_TSC_ADJUST     
           
    

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

     subroutine ReadIA32_TSC_ADJUST(msr,iounit,fname,status,err,ermsg)
 !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_TSC_ADJUST
           type(MSR_IA32_TSC_ADJUST),             intent(inout) :: msr
           integer(kind=int4),                    intent(in)    :: iounit
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(in)    :: err
           character(len=256),                    intent(in)    :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=1, 8
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')            
     end subroutine ReadIA32_TSC_ADJUST

     !DIR$ ATTRIBUTES INLINE :: initIA32_SPEC_CTRL
     subroutine initIA32_SPEC_CTRL(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_SPEC_CTRL
           type(MSR_IA32_SPEC_CTRL),      intent(inout) :: msr
           ! Exec code ....
           msr.msr_read   = zero_val
           msr.msr_write  = zero_val
           msr.msrw_hex   = init_valh
     end subroutine initIA32_SPEC_CTRL

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

     subroutine ReadIA32_SPEC_CTRL(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_SPEC_CTRL
           type(MSR_IA32_SPEC_CTRL),              intent(inout) :: msr
           integer(kind=int4),                    intent(in)    :: iounit
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(in)    :: err
           character(len=256),                    intent(in)    :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read
           if(ioerr > 0 .or. ioerr < 0) goto 9999
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadIA32_SPEC_CTRL

     !DIR$ ATTRIBUTES INLINE :: initIA32_PRED_CMD
     subroutine initIA32_PRED_CMD(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_PRED_CMD
           type(MSR_IA32_PRED_CMD),      intent(inout) :: msr
           ! Exec code ....
           msr.msr_read   = zero_val
           msr.msr_write  = zero_val
           msr.msrw_hex   = init_valh
     end subroutine initIA32_PRED_CMD
       

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

     subroutine ReadIA32_PRED_CMD(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRTIBUTES CODE_ALIGN:32 :: ReadIA32_PRED_CMD
           type(MSR_IA32_PRED_CMD),               intent(inout) :: msr
           integer(kind=int4),                    intent(in)    :: iounit
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(in)    :: err
           character(len=256),                    intent(in)    :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read
           if(ioerr > 0 .or. ioerr < 0) goto 9999
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadIA32_PRED_CMD

     !DIR$ ATTRIBUTES INLINE :: initIA32_BIOS_UPDT_TRIG
     subroutine initIA32_BIOS_UPDT_TRIG(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_BIOS_UPDT_TRIG
           type(MSR_IA32_BIOS_UPDT_TRIG),    intent(inout) :: msr
           ! Exec code .....
           msr.msr_read   = zero_val
           msr.msr_write  = zero_val
           msr.msrw_hex   = init_valh
     end subroutine initIA32_BIOS_UPDT_TRIG

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

     subroutine ReadIA32_BIOS_UPDT_TRIG(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_BIOS_UPDT_TRIG
           type(MSR_IA32_BIOS_UPDT_TRIG),         intent(inout) :: msr
           integer(kind=int4),                    intent(in)    :: iounit
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(in)    :: err
           character(len=256),                    intent(in)    :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read
           if(ioerr > 0 .or. ioerr < 0) goto 9999
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadIA32_BIOS_UPDT_TRIG

         

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

     subroutine initIA32_PMC0(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_PMC0
           type(MSR_IA32_PMC0),      intent(inout) :: msr
           ! Exec code ....
           msr.msr_read = zero_val
     end subroutine initIA32_PMC0

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

     subroutine ReadIA32_PMC0(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_PMC0
           type(MSR_IA32_PMC0),                   intent(inout) :: msr
           integer(kind=int4),                    intent(in)    :: iounit
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(in)    :: err
           character(len=256),                    intent(in)    :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=1, 1000
                  read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read(i)
                  if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')     
     end subroutine ReadIA32_PMC0

     subroutine initIA32_PMC1(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_PMC1
           type(MSR_IA32_PMC1),     intent(inout) :: msr
           ! Exec code ....
           msr.msr_read = zero_val
     end subroutine initIA32_PMC1

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

     subroutine ReadIA32_PMC1(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_PMC1
           type(MSR_IA32_PMC1),                   intent(inout) :: msr
           integer(kind=int4),                    intent(in)    :: iounit
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(in)    :: err
           character(len=256),                    intent(in)    :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -1
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=1, 1000
                  read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msr.msr_read(i)
                  if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           err = 999
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')  
     end subroutine ReadIA32_PMC1

     subroutine initIA32_PMC2(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_PMC2
           type(MSR_IA32_PMC2),        intent(inout) :: msr
           ! Exec code ...
           msr.msr_read = zero_val
     end subroutine initIA32_PMC2

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
