

module mod_intel_msrtool_wrappers

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         mod_zen_msrtools_wrappers
 !          
 !          Purpose:
 !                    This module contains a wrapper subroutines
 !                    around msr-tool package for the Intel architectural MSRs.
 !                   
 !                     
 !          History:
 !                        Date: 05-08-2019
 !                        Time: 10:16 GMT+2
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
 !                          Intel  Processor's manuals
 !                          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

     use mod_kinds, only : int1, int2, int4, int8b, sp
     use mod_msr_architectural
     implicit none
     public
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


     character(*), parameter, private :: rmsr = "rdmsr"
     character(*), parameter, private :: wmsr = "wrmsr"
     character(*), parameter, public  :: reset_val = "0x000000000000000"
     integer(kind=int8b), parameter, public :: init_val = Z"FFFFFFFFFFFFFFF"
     integer(kind=int8b), parameter, public :: zero_val = Z"000000000000000"
     character(*), parameter, public  :: init_valh      = "0xFFFFFFFFFFFFFFFF"

     contains
     
     !DIR$ ATTRIBUTES INLINE :: initIA32_P5_MC_ADDR
     subroutine initIA32_P5_MC_ADDR(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_P5_MC_ADDR
           type(MSR_IA32_P5_MC_ADDR),   intent(inout) :: reg
           ! Exec code ....
           msr.msr_read = zero_val
     end subroutine initIA32_P5_MC_ADDR

     subroutine AccessIA32_P5_MC_ADDR(msr,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_P5_MC_ADDR
           type(MSR_IA32_P5_MC_ADDR),   intent(in) :: msr
           character(len=*),            intent(in) :: command
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(-1 == stat) then
              ier = stat
           end if
           ier = 0
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

     subroutine AccessIA32_P5_TYPE(msr,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_P5_TYPE
           type(MSR_IA32_P5_TYPE),      intent(in) :: msr
           character(len=*),            intent(in) :: command
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(-1 == stat) then
              ier = stat
           end if
           ier = 0
     end subroutine AccessIA32_P5_TYPE

     subroutine ReadIA32_P5_TYPE(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_P5_TYPE
           type(MSR_IA32_P5_TYPE),      intent(inout) :: msr
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
     
     subroutine AccessIA32_MONITOR_FILTER_SIZE(msr,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_MONITOR_FILTER_SIZE
           type(MSR_IA32_MONITOR_FILTER_SIZE),     intent(in) :: msr
           character(len=*),                       intent(in) :: command
           character(len=*),                       intent(in) :: fname
           integer(kind=int2),                     intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(-1 == stat) then
              ier = stat
           end if
           ier = 0
      end subroutine AccessIA32_MONITOR_FILTER_SIZE

      subroutine ReadIA32_MONITOR_FILTER_SIZE(msr,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadIA32_MONITOR_FILTER_SIZE
           type(MSR_IA32_MONITOR_FILTER_SIZE),     intent(inout) :: msr
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
     end subroutine ReadIA32_MONITOR_FILTER_SIZE

     !DIR$ ATTRIBUTES INLINE :: initIA32_PLATFORM_ID
     subroutine initIA32_PLATFORM_ID(msr)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initIA32_PLATFORM_ID
           type(MSR_IA32_PLATFORM_ID),     intent(inout) :: msr
           ! Exec code ,....
           msr.msr_read = zero_val
     end subroutine initIA32_PLATFORM_ID

     subroutine AccessIA32_PLATFORM_ID(msr,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PLATFORM_ID
           type(MSR_IA32_PLATFORM_ID),             intent(in)    :: msr
           character(len=*),                       intent(in) :: command
           character(len=*),                       intent(in) :: fname
           integer(kind=int2),                     intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(-1 == stat) then
              ier = stat
           end if
           ier = 0
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

     subroutine AccessIA32_APIC_BASE(msr,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_APIC_BASE
           type(MSR_IA32_APIC_BASE),               intent(in) :: msr
           character(len=*),                       intent(in) :: command
           character(len=*),                       intent(in) :: fname
           integer(kind=int2),                     intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(-1 == stat) then
              ier = stat
           end if
           ier = 0
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

         
         
end module mod_intel_msrtool_wrappers
