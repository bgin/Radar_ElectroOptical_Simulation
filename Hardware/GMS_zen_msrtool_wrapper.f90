
 
  
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

     use mod_kinds, only : int1,int2, int4, int8b, dp
     use mod_zen_msr
     implicit none

     character(*), parameter, private :: rmsr = "rdmsr"
     character(*), parameter, private :: wmsr = "wrmsr"
     character(*), parameter, public  :: reset_val = "0x000000000000000"
     integer(kind=int8b), parameter, public :: init_val = Z"FFFFFFFFFFFFFFF"
     integer(kind=int8b), parameter, public :: zero_val = Z"000000000000000"
     character(*), parameter, public  :: init_valh      = "0xFFFFFFFFFFFFFFFF"
     contains
!DIR$ ATTRIBUTES INLINE :: initMSR_TSC_ZEN
     subroutine initMSR_TSC_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_TSC_ZEN
           type(MSR_TSC_ZEN),      intent(inout) :: reg
          !
           ! Exec code ....
           reg.samp_delta = 0.0_dp
           reg.msr_write  = init_val
           reg.msrw_hex   = init_valh
           reg.msr_read   = init_val ! Inlining up to call to intel_malloc
           
     end subroutine initMSR_TSC_ZEN

     subroutine AccessMSR_TSC_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_TSC_ZEN
     
       type(MSR_TSC_ZEN),    intent(in)    :: reg
       character(len=*),     intent(in)    :: command
       logical(kind=int1),   intent(in)    :: reset
       character(len=*),     intent(in)    :: fname
       character(len=2),     intent(in)    :: hwth ! HW thread number
       !
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

     subroutine ReadMSR_TSC_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_TSC_ZEN
          
           type(MSR_TSC_ZEN),          intent(inout) :: reg
           integer(kind=int4),         intent(in)    :: iounit
           integer(kind=int4),         intent(in)    :: nth
           character(len=*),           intent(in)    :: fname
           integer(kind=int2),         intent(in)    :: status
           integer(kind=int4),         intent(inout) :: err
           character(len=256),         intent(inout) :: ermsg
         
           ! Locals
           integer(kind=int4), automatic :: j,i,ioerr
           logical(kind=int4), automatic :: present
           ! Exec code ...
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status == -1) then
              err = -9999
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr > 0) then
              err = ioerr
              return
           end if
!....
           do j=0, nth
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) reg.samp_delta(j)
              if(ioerr > 0.or.ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i,j)
                 if(ioerr > 0.or.ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
           
     end subroutine ReadMSR_TSC_ZEN         
    
     
!DIR$ ATTRIBUTES INLINE :: initMSR_APIC_BAR_ZEN
     subroutine initMSR_APIC_BAR_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_APIC_BAR_ZEN
           type(MSR_APIC_BAR_ZEN),        intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_APIC_BAR_zen

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

      subroutine ReadMSR_APIC_BAR_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_APIC_BAR_ZEN
           type(MSR_APIC_BAR_ZEN),      intent(inout) :: reg
           integer(kind=int4),          intent(in)    :: iounit
           integer(kind=int4),          intent(in)    :: nth
           character(len=*),            intent(in)    :: fname
           integer(kind=int2),          intent(in)    :: status
           integer(kind=int4),          intent(inout) :: err
           character(len=256),          intent(inout) :: ermsg
           ! LOcals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
              read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
           !              
      end subroutine ReadMSR_APIC_BAR_ZEN
         
!DIR$ ATTRIBUTES INLINE :: initMSR_MPERF_ZEN
     subroutine initMSR_MPERF_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MPERF_ZEN
           type(MSR_MPERF_ZEN),        intent(inout) :: reg
           ! Exec code ....
           reg.msr_write  = init_val
           reg.msrw_hex   = init_valh
           reg.samp_delta = 0.0_dp
           reg.msr_read   = init_val
     end subroutine initMSR_MPERF_ZEN

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

     subroutine ReadMSR_MPERF_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MPERF_ZEN
           type(MSR_MPERF_ZEN),      intent(inout) :: reg
           integer(kind=int4),       intent(in)    :: iounit
           integer(kind=int4),       intent(in)    :: nth
           character(len=*),         intent(in)    :: fname
           integer(kind=int2),       intent(in)    :: status
           integer(kind=int4),       intent(inout) :: err
           character(len=256),       intent(inout) :: ermsg
           ! Locals
           integer(kind=int4), automatic :: j,i,ioerr
           logical(kind=int4), automatic :: present
           ! Exec code ....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status == -1) then
              err = -9999
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr > 0) then
              err = ioerr
              return
           end if
           do j=0, nth
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) reg.samp_delta(i)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i,j)
                 if(ioerr > 0 .or. ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_MPERF_ZEN
         
!DIR$ ATTRIBUTES INLINE :: initMSR_APERF_ZEN
     subroutine initMSR_APERF_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_APERF_ZEN
           type(MSR_APERF_ZEN),      intent(inout) :: reg
           ! Exec code ...
           reg.msr_write  = init_val
           reg.msrw_hex   = init_valh
           reg.samp_delta = 0.0_dp
           reg.msr_read   = init_val
     end subroutine initMSR_APERF_ZEN
     
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

     subroutine ReadMSR_APERF_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_APERF_ZEN
           type(MSR_APERF_ZEN),        intent(inout) :: reg
           integer(kind=int4),         intent(in)    :: iounit
           integer(kind=int4),         intent(in)    :: nth
           character(len=*),           intent(in)    :: fname
           integer(kind=int2),         intent(in)    :: status
           integer(kind=int4),         intent(inout) :: err
           character(len=256),         intent(inout) :: ermsg
           ! Locals
           integer(kind=int4), automatic :: j,i,ioerr
           logical(kind=int4), automatic :: present
           ! Exec code ......
           present = .false.
           !
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status == -1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr > 0) then
              err = ioerr
              return
           end if
           do j=0, nth
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) reg.samp_delta(i)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i,j)
                 if(ioerr > 0 .or. ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_APERF_ZEN
     
!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_ZEN
     subroutine initMSR_MTRR_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_ZEN
           type(MSR_MTRR_ZEN),   intent(inout) :: reg
           ! Exec code .....
           reg.msr_read = init_val
     end subroutine initMSR_MTRR_ZEN

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

      subroutine ReadMSR_MTRR_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_ZEN
           type(MSR_MTRR_ZEN),       intent(inout) :: reg
           integer(kind=int4),       intent(in)    :: iounit
           integer(kind=int4),       intent(in)    :: ncores
           character(len=*),         intent(in)    :: fname
           integer(kind=int2),       intent(in)    :: status
           integer(kind=int4),       intent(inout) :: err
           character(len=256),       intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
              read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
      end subroutine ReadMSR_MTRR_ZEN
         
!DIR$ ATTRIBUTES INLINE :: initMSR_MCG_CAP_ZEN
     subroutine initMSR_MCG_CAP_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MCG_CAP_ZEN
           type(MSR_MCG_CAP_ZEN),   intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_MCG_CAP_ZEN

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

     subroutine ReadMSR_MCG_CAP_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MCG_CAP_ZEN
           type(MSR_MCG_CAP_ZEN),    intent(inout) :: reg
           integer(kind=int4),       intent(in)    :: iounit
           integer(kind=int4),       intent(in)    :: nth
           character(len=*),         intent(in)    :: fname
           integer(kind=int2),       intent(in)    :: status
           integer(kind=int4),       intent(inout) :: err
           character(len=256),       intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
              read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_MCG_CAP_ZEN
         
!DIR$ ATTRIBUTES INLINE :: initMSR_MCG_STAT_ZEN
     subroutine initMSR_MCG_STAT_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MCG_STAT_ZEN
           type(MSR_MCG_STAT_ZEN),       intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = init_val
           reg.msr_write = init_val
           reg.msrw_hex  = init_valh 
     end subroutine initMSR_MCG_STAT_ZEN
       
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

      subroutine ReadMSR_MCG_STAT_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MCG_STAT_ZEN
           type(MSR_MCG_STAT_ZEN),   intent(inout) :: reg
           integer(kind=int4),       intent(in)    :: iounit
           integer(kind=int4),       intent(in)    :: nth
           character(len=*),         intent(in)    :: fname
           integer(kind=int2),       intent(in)    :: status
           integer(kind=int4),       intent(inout) :: err
           character(len=256),       intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
              read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
      end subroutine ReadMSR_MCG_STAT_ZEN
      
!DIR$ ATTRIBUTES INLINE initMSR_MCG_CTL_ZEN
     subroutine initMSR_MCG_CTL_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MCG_CTL_ZEN
           type(MSR_MCG_CTL_ZEN),      intent(inout) :: reg
           ! Exec code ...
           reg.msr_read  = init_val
           reg.msr_write = init_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_MCG_CTL_ZEN

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

     subroutine ReadMSR_MCG_CTL_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MCG_CTL_ZEN
           type(MCR_MCG_CTL_ZEN),    intent(inout) :: reg
           integer(kind=int4),       intent(in)    :: iounit
           integer(kind=int4),       intent(in)    :: nth
           character(len=*),         intent(in)    :: fname
           integer(kind=int2),       intent(in)    :: status
           integer(kind=int4),       intent(inout) :: err
           character(len=256),       intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_MCG_CTL_ZEN

         
!DIR$ ATTRIBUTES INLINE :: initMSR_DBG_CTL_ZEN
     subroutine initMSR_DBG_CTL_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_DBG_CTL_ZEN
           type(MSR_DBG_CTL_ZEN),      intent(inout) :: reg
           ! Exec code ...
           reg.msr_read = init_val
           
     end subroutine initMSR_DBG_CTL_ZEN

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

     subroutine ReadMSR_DBG_CTL_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_DBG_CTL_ZEN
           type(MSR_DBG_CTL_ZEN),    intent(inout) :: reg
           integer(kind=int4),       intent(in)    :: iounit
           integer(kind=int4),       intent(in)    :: nth
           character(len=*),         intent(in)    :: fname
           integer(kind=int2),       intent(in)    :: status
           integer(kind=int4),       intent(inout) :: err
           character(len=256),       intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
              read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_DBG_CTL_ZEN
         
!DIR$ ATTRIBUTES INLINE :: initMSR_BR_FROM_IP_ZEN
     subroutine initMSR_BR_FROM_IP_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_BR_FROM_IP_ZEN
           type(MSR_BR_FROM_IP_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_BR_FROM_IP_ZEN

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

     subroutine ReadMSR_BR_FROM_IP_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_BR_FROM_IP_ZEN
           type(MSR_BR_FROM_IP_ZEN), intent(inout) :: reg
           integer(kind=int4),       intent(in)    :: iounit
           integer(kind=int4),       intent(in)    :: nth
           character(len=*),         intent(in)    :: fname
           integer(kind=int2),       intent(in)    :: status
           integer(kind=int4),       intent(inout) :: err
           character(len=256),       intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_BR_FROM_IP_ZEN
         
!DIR$ ATTRIBUTES INLINE :: initMSR_BR_TO_IP_ZEN
     subroutine initMSR_BR_TO_IP_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_BR_TO_IP_ZEN
           type(MSR_BR_TO_IP_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_BR_TO_IP_ZEN

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

     subroutine ReadMSR_BR_TO_IP_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_BR_TO_IP_ZEN
           type(MSR_BR_TO_IP_ZEN),   intent(inout) :: reg
           integer(kind=int4),       intent(in)    :: iounit
           integer(kind=int4),       intent(in)    :: nth
           character(len=*),         intent(in)    :: fname
           integer(kind=int2),       intent(in)    :: status
           integer(kind=int4),       intent(inout) :: err
           character(len=256),       intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_BR_TO_IP_ZEN

         
!DIR$ ATTRIBUTES INLINE :: initMSR_LAST_EXP_FROM_IP_ZEN
     subroutine initMSR_LAST_EXP_FROM_IP_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_LAST_EXP_FROM_IP_ZEN
           type(MSR_LAST_EXP_FROM_IP_ZEN),   intent(inout) :: reg
           ! Exec code .....
           reg.msr_read = init_val
     end subroutine initMSR_LAST_EXP_FROM_IP_ZEN

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

     subroutine ReadMSR_LAST_EXP_FROM_IP_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_LAST_EXP_FROM_IP_ZEN
           type(MSR_LAST_EXP_FROM_IP_ZEN),     intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: nth
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_LAST_EXP_FROM_IP_ZEN
         
!DIR$ ATTRIBUTES INLINE :: initMSR_LAST_EXP_TO_IP_ZEN
     subroutine initMSR_LAST_EXP_TO_IP_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_LAST_EXP_TO_IP_ZEN
           type(MSR_LAST_EXP_TO_IP_ZEN),    intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_LAST_EXP_TO_IP_ZEN

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

     subroutine ReadMSR_LAST_EXP_TO_IP_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_LAST_EXP_TO_IP_ZEN
           type(MSR_LAST_EXP_TO_IP_ZEN),       intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: nth
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_LAST_EXP_TO_IP_ZEN
         
!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_FIXED_ZEN
     subroutine initMSR_MTRR_FIXED_ZEN(reg)
!DIR$  ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_FIXED_ZEN
           type(MSR_MTRR_FIXED_ZEN),   intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_MTRR_FIXED_ZEN

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

     subroutine ReadMSR_MTRR_FIXED_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_FIXED_ZEN
           type(MSR_MTRR_FIXED_ZEN),           intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: nth
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_MTRR_FIXED_ZEN

         
!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_FIXED16K_ZEN
     subroutine initMSR_MTRR_FIXED16K_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_FIXED16K_ZEN
           type(MSR_MTRR_FIXED16K_ZEN),   intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_MTRR_FIXED16K_ZEN

     subroutine AccessMSR_MTRR_FIXED16K_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED16K_ZEN
           type(MSR_MTRR_FIXED16K_ZEN),    intent(in) :: reg
           character(len=*),               intent(in) :: command
           character(len=2),               intent(in) :: core
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_MTRR
     
         
     subroutine ReadMSR_MTRR_FIXED16K_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_FIXED16K_ZEN
           type(MSR_MTRR_FIXED16K_ZEN),        intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_MTRR_FIXED16K_ZEN
         
!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_FIXED16K1_ZEN
     subroutine initMSR_MTRR_FIXED16K1_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_FIXED16K1_ZEN
           type(MSR_MTRR_FIXED16K1_ZEN),  intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_MTRR_FIXED16K1_ZEN

         
     subroutine AccessMSR_MTRR_FIXED16K1_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED16K1_ZEN
           type(MSR_MTRR_FIXED16K1_ZEN),   intent(in) :: reg
           character(len=*),               intent(in) :: command
           character(len=2),               intent(in) :: core
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_MTRR_FIXED16K1_ZEN
         

     subroutine ReadMSR_MTRR_FIXED16K1_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_FIXED16K1_ZEN
           type(MSR_MTRR_FIXED16K1_ZEN),       intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_MTRR_FIXED16K1_ZEN
     
         
!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_FIXED4K_ZEN     
     subroutine initMSR_MTRR_FIXED4K_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_FIXED4K_ZEN
           type(MSR_MTRR_FIXED4K_ZEN),   intent(inout) :: reg
           ! Exec code ...
           reg.msr_read = init_val
     end subroutine initMSR_MTRR_FIXED4K_ZEN

         
     subroutine AccessMSR_MTRR_FIXED4K_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K_ZEN
           type(MSR_MTRR_FIXED4K_ZEN),   intent(in) :: reg
           character(len=*),               intent(in) :: command
           character(len=2),               intent(in) :: core
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_MTRR_FIXED4K_ZEN
         

     subroutine ReadMSR_MTRR_FIXED4K_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_FIXED4K_ZEN
           type(MSR_MTRR_FIXED4K_ZEN),         intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
              read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_MTRR_FIXED4K_ZEN

         
!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_FIXED4K1_ZEN
     subroutine initMSR_MTRR_FIXED4K1_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_FIXED4K1_ZEN
           type(MSR_MTRR_FIXED4K1_ZEN),   intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_MTRR_FIXED4K1_ZEN

     subroutine AccessMSR_MTRR_FIXED4K1_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K1_ZEN
           type(MSR_MTRR_FIXED4K1_ZEN),   intent(in) :: reg
           character(len=*),              intent(in) :: command
           character(len=2),              intent(in) :: core
           character(len=*),              intent(in) :: fname
           integer(kind=int2),            intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_MTRR_FIXED4K1_ZEN

     subroutine ReadMSR_MTRR_FIXED4K1_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_FIXED4K1_ZEN
           type(MSR_MTRR_FIXED4K1_ZEN),        intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
              read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
              if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_MTRR_FIXED4K1_ZEN  

         
!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_FIXED4K2_ZEN
     subroutine initMSR_MTRR_FIXED4K2_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_FIXED4K2_ZEN
           type(MSR_MTRR_FIXED4K2_ZEN),   intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_MTRR_FIXED4K2_ZEN

     subroutine AccessMSR_MTRR_FIXED4K2_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K2_ZEN
           type(MSR_MTRR_FIXED4K2_ZEN),   intent(in) :: reg
           character(len=*),              intent(in) :: command
           character(len=2),              intent(in) :: core
           character(len=*),              intent(in) :: fname
           integer(kind=int2),            intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_MTRR_FIXED4K2_ZEN

     subroutine ReadMSR_MTRR_FIXED4K2_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_FIXED4K2_ZEN
           type(MSR_MTRR_FIXED4K2_ZEN),        intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_MTRR_FIXED4K2_ZEN

         
!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_FIXED4K3_ZEN
     subroutine initMSR_MTRR_FIXED4K3_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_FIXED4K3
           type(MSR_MTRR_FIXED4K3_ZEN),   intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
      end subroutine initMSR_MTRR_FIXED4K3_ZEN

      subroutine AccessMSR_MTRR_FIXED4K3_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K3_ZEN
           type(MSR_MTRR_FIXED4K3_ZEN),   intent(in) :: reg
           character(len=*),              intent(in) :: command
           character(len=2),              intent(in) :: core
           character(len=*),              intent(in) :: fname
           integer(kind=int2),            intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
      end subroutine AccessMSR_MTRR_FIXED4K3_ZEN

     subroutine ReadMSR_MTRR_FIXED4K3_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_FIXED4K3_ZEN
           type(MSR_MTRR_FIXED4K3_ZEN),        intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_MTRR_FIXED4K3_ZEN

         
!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_FIXED4K4_ZEN
     subroutine initMSR_MTRR_FIXED4K4_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_FIXED4K4_ZEN
           type(MSR_MTRR_FIXED4K4_ZEN),   intent(inout) :: reg
           ! Exec code ...
           reg.msr_read = init_val
     end subroutine initMSR_MTRR_FIXED4K4_ZEN

     subroutine AccessMSR_MTRR_FIXED4K4_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K4_ZEN
           type(MSR_MTRR_FIXED4K4_ZEN),   intent(in) :: reg
           character(len=*),              intent(in) :: command
           character(len=2),              intent(in) :: core
           character(len=*),              intent(in) :: fname
           integer(kind=int2),            intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_MTRR_FIXED4K4_ZEN

     subroutine ReadMSR_MTRR_FIXED4K4_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_FIXED4K4_ZEN
           type(MSR_MTRR_FIXED4K4_ZEN),        intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_MTRR_FIXED4K4_ZEN

         
!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_FIXED4K5_ZEN     
     subroutine initMSR_MTRR_FIXED4K5_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_FIXED4K5_ZEN
           type(MSR_MTRR_FIXED4K5_ZEN),   intent(inout) :: reg
           ! Exec code ...
           reg.msr_read = init_val
     end subroutine initMSR_MTRR_FIXED4K5_ZEN

     subroutine AccessMSR_MTRR_FIXED4K5_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K5_ZEN
           type(MSR_MTRR_FIXED4K5_ZEN),   intent(in) :: reg
           character(len=*),              intent(in) :: command
           character(len=2),              intent(in) :: core
           character(len=*),              intent(in) :: fname
           integer(kind=int2),            intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_MTRR_FIXED4K5_ZEN

     subroutine ReadMSR_MTRR_FIXED4K5_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_FIXED4K5_ZEN
           type(MSR_MTRR_FIXED4K5_ZEN),        intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
      end subroutine ReadMSR_MTRR_FIXED4K5_ZEN

!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_FIXED4K6_ZEN
     subroutine initMSR_MTRR_FIXED4K6_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_FIXED4K6_ZEN
           type(MSR_MTRR_FIXED4K6_MSR),   intent(inout) :: reg
           ! Exec code ...
           reg.msr_read = init_val
     end subroutine initMSR_MTRR_FIXED4K6_ZEN

     subroutine AccessMSR_MTRR_FIXED4K6_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K6_ZEN
           type(MSR_MTRR_FIXED4K6_ZEN),    intent(in) :: reg
           character(len=*),               intent(in) :: command
           character(len=2),               intent(in) :: core
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_MTRR_FIXED4K6_ZEN

     subroutine ReadMSR_MTRR_FIXED4K6_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
 !DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_FIXED4K6_ZEN
           type(MSR_MTRR_FIXED4K6_ZEN),        intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_MTRR_FIXED4K6_ZEN
         
!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_FIXED4K7_ZEN
     subroutine initMSR_MTRR_FIXED4K7_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_FIXED4K7_ZEN
           type(MSR_MTRR_FIXED4K7_ZEN),   intent(inout) :: reg
           ! Exec code ...
           reg.msr_read = init_val
     end subroutine initMSR_MTRR_FIXED4K7_ZEN

     subroutine AccessMSR_MTRR_FIXED4K7_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K7_ZEN
           type(MSR_MTRR_FIXED4K7_ZEN),    intent(in)  :: reg
           character(len=*),               intent(in) :: command
           character(len=2),               intent(in) :: core
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_MTRR_FIXED4K7_ZEN

     subroutine ReadMSR_MTRR_FIXED4K7_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_FIXED4K7_ZEN
           type(MSR_MTRR_FIXED4K7_ZEN),        intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_MTRR_FIXED4K7_ZEN
         
!DIR$ ATTRIBUTES INLINE :: initMSR_PAT_ZEN
     subroutine initMSR_PAT_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PAT_ZEN
           type(MSR_PAT_ZEN),              intent(inout) :: reg
           ! Code exec .....
           reg.msr_read = init_val
     end subroutine initMSR_PAT_ZEN

     subroutine AccessMSR_PAT_ZEN(reg,command,hwth,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PAT_ZEN
           type(MSR_PAT_ZEN),              intent(in) :: reg
           character(len=*),               intent(in) :: command
           character(len=2),               intent(in) :: hwth
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//hwth//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_PAT_ZEN

     subroutine ReadMSR_PAT_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PAT_ZEN
           type(MSR_PAT_ZEN),                  intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: nth
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_PAT_ZEN

         
!DIR$ ATTRIBUTES INLINE :: initMSR_MTRR_DEFTYPE_ZEN
     subroutine initMSR_MTRR_DEFTYPE_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MTRR_DEFTYPE_ZEN
           type(MSR_MTRR_DEFTYPE_ZEN),     intent(inout) :: reg
           ! Exec code ...
           reg.msr_read = init_val
     end subroutine initMSR_MTRR_DEFTYPE_ZEN

     subroutine AccessMSR_MTRR_DEFTYPE_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_DEFTYPE_ZEN
           type(MSR_MTRR_DEFTYPE_ZEN),     intent(in) :: reg
           character(len=*),               intent(in) :: command
           character(len=2),               intent(in) :: core
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_MTRR_DEFTYPE_ZEN

     subroutine ReadMSR_MTRR_DEFTYPE_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MTRR_DEFTYPE_ZEN
           type(MSR_MTRR_DEFTYPE_ZEN),         intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')  
     end subroutine ReadMSR_MTRR_DEFTYPE_ZEN
     
         
!DIR$  ATTRIBUTES INLINE :: initMSR_EFER_ZEN
     subroutine initMSR_EFER_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_EFER_ZEN
           type(MSR_EFER_ZEN),             intent(inout) :: reg
           ! Exec code ...
           reg.msr_read = init_val
     end subroutine initMSR_EFER_ZEN

     subroutine AccessMSR_EFER_ZEN(reg,command,hwth,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_EFER_ZEN
           type(MSR_EFER_ZEN),             intent(in) :: reg
           character(len=*),               intent(in) :: command
           character(len=2),               intent(in) :: hwth
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//hwth//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if  
     end subroutine AccessMSR_EFER_ZEN

     subroutine ReadMSR_EFER_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATRIBUTES CODE_ALIGN:32 :: ReadMSR_EFER_ZEN
           type(MSR_EFER_ZEN),                 intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: nth
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')  
     end subroutine ReadMSR_EFER_ZEN

     subroutine initMSR_MPERF_READONLY_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MPERF_READONLY_ZEN
           type(MSR_MPERF_READONLY_ZEN),   intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = zero_val
     end subroutine initMSR_MPERF_READONLY_ZEN

         
     subroutine AccessMSR_MPERF_READONLY_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MPERF_READONLY_ZEN
           type(MSR_MPERF_READONLY_ZEN),    intent(in) :: reg
           character(len=*),     intent(in)    :: command
           logical(kind=int1),   intent(in)    :: reset
           character(len=*),     intent(in)    :: fname
           character(len=2),     intent(in)    :: hwth ! HW thread number
           !
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
     end subroutine AccessMSR_MPERF_READONLY_ZEN
         

     subroutine ReadMSR_MPERF_READONLY_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MPERF_READONLY_ZEN
           type(MSR_MPERF_READONLY_ZEN),   intent(inout) :: reg
           integer(kind=int4),             intent(in)    :: iounit
           integer(kind=int4),             intent(in)    :: nth
           character(len=*),               intent(in)    :: fname
           integer(kind=int2),             intent(in)    :: status
           integer(kind=int4),             intent(inout) :: err
           character(len=256),             intent(inout) :: ermsg
         
           ! Locals
           integer(kind=int4), automatic :: j,i,ioerr
           logical(kind=int4), automatic :: present
           ! Exec code ...
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status == -1) then
              err = -9999
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr > 0) then
              err = ioerr
              return
           end if
!....
           do j=0, nth
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) reg.samp_delta(j)
              if(ioerr >0.or.ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i,j)
                 if(ioerr > 0.or.ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_MPERF_READONLY_ZEN

     subroutine initMSR_APERF_READONLY_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_APERF_READONLY_ZEN
           type(MSR_APERF_READONLY_ZEN),   intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = zero_val
     end subroutine initMSR_APERF_READONLY_ZEN

     subroutine AccessMSR_APERF_READONLY_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_APERF_READONLY_ZEN
           type(MSR_APERF_READONLY_ZEN),      intent(in) :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           character(len=2),                  intent(in)    :: hwth ! HW thread number
           !
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
     end subroutine AccessMSR_MPERF_READONLY_ZEN

     subroutine ReadMSR_APERF_READONLY_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_APERF_READONLY_ZEN
           type(MSR_APERF_READONLY_ZEN),   intent(inout) :: reg
           integer(kind=int4),             intent(in)    :: iounit
           integer(kind=int4),             intent(in)    :: nth
           character(len=*),               intent(in)    :: fname
           integer(kind=int2),             intent(in)    :: status
           integer(kind=int4),             intent(inout) :: err
           character(len=256),             intent(inout) :: ermsg
         
           ! Locals
           integer(kind=int4), automatic :: j,i,ioerr
           logical(kind=int4), automatic :: present
           ! Exec code ...
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status == -1) then
              err = -9999
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr > 0) then
              err = ioerr
              return
           end if
!....
           do j=0, nth
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) reg.samp_delta(j)
              if(ioerr >0.or.ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i,j)
                 if(ioerr > 0.or.ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_APERF_READONLY_ZEN  

     subroutine initMSR_IRPERF_COUNT_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IRPERF_COUNT_ZEN
           type(MSR_IRPERF_COUNT_ZEN),     intent(inout) :: reg
           ! Exec code ...
           reg.msr_read = zero_val
     end subroutine initMSR_IRPERF_COUNT_ZEN

     subroutine AccessMSR_IPERF_COUNT_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IPERF_COUNT_ZEN
           type(MSR_IPERF_COUNT_ZEN),         intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           character(len=2),                  intent(in)    :: hwth ! HW thread number
          
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
     end subroutine AccessMSR_IPERF_COUNT_ZEN

     subroutine ReadMSR_IPERF_COUNT_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IPERF_COUNT_ZEN
           type(MSR_IPERF_COUNT_ZEN),      intent(inout) :: reg
           integer(kind=int4),             intent(in)    :: iounit
           integer(kind=int4),             intent(in)    :: nth
           character(len=*),               intent(in)    :: fname
           integer(kind=int2),             intent(in)    :: status
           integer(kind=int4),             intent(inout) :: err
           character(len=256),             intent(inout) :: ermsg
         
           ! Locals
           integer(kind=int4), automatic :: j,i,ioerr
           logical(kind=int4), automatic :: present
           ! Exec code ...
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status == -1) then
              err = -9999
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr > 0) then
              err = ioerr
              return
           end if
!....
           do j=0, nth
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) reg.samp_delta(j)
              if(ioerr >0.or.ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i,j)
                 if(ioerr > 0.or.ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_IPERF_COUNT_ZEN

     subroutine initMSR_TSC_AUX_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_TSC_AUX_ZEN
           type(MSR_TSC_AUX_ZEN),          intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = zero_val
     end subroutine initMSR_TSC_AUX_ZEN

     subroutine AccessMSR_TSC_AUX_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_TSC_AUX_ZEN
           type(MSR_TSC_AUX_ZEN),             intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           character(len=2),                  intent(in)    :: hwth ! HW thread number
          
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
     end subroutine AccessMSR_TSC_AUX_ZEN

     subroutine ReadMSR_TSC_AUX_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$  ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_TSC_AUX_ZEN
           type(MSR_TSC_AUX_ZEN),          intent(inout) :: reg
           integer(kind=int4),             intent(in)    :: iounit
           integer(kind=int4),             intent(in)    :: nth
           character(len=*),               intent(in)    :: fname
           integer(kind=int2),             intent(in)    :: status
           integer(kind=int4),             intent(inout) :: err
           character(len=256),             intent(inout) :: ermsg
         
           ! Locals
           integer(kind=int4), automatic :: j,i,ioerr
           logical(kind=int4), automatic :: present
           ! Exec code ...
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status == -1) then
              err = -9999
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr > 0) then
              err = ioerr
              return
           end if
!....
           do j=0, nth
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) reg.samp_delta(j)
              if(ioerr >0.or.ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i,j)
                 if(ioerr > 0.or.ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_TSC_AUX_ZEN

     subroutine initMSR_TSC_RATIO_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_TSC_RATIO_ZEN
           type(MSR_TSC_RATIO_ZEN),        intent(inout) :: reg
           ! Exec cdoe ...
           reg.samp_delta = 0.0_dp
           reg.msr_read   = zero_val
      end subroutine initMSR_TSC_RATIO_ZEN

      subroutine AccessMSR_TSC_RATIO_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_TSC_RATIO_ZEN
           type(MSR_TSC_RATIO_ZEN),           intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           character(len=2),                  intent(in)    :: hwth ! HW thread number
           integer(kind=int2),                intent(inout) :: ier
          
      
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
      end subroutine AccessMSR_TSC_RATIO_ZEN  

     subroutine ReadMSR_TSC_RATIO_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_TSC_RATIO_ZEN
           type(MSR_TSC_RATIO_ZEN),        intent(inout) :: reg
           integer(kind=int4),             intent(in)    :: iounit
           integer(kind=int4),             intent(in)    :: nth
           character(len=*),               intent(in)    :: fname
           integer(kind=int2),             intent(in)    :: status
           integer(kind=int4),             intent(inout) :: err
           character(len=256),             intent(inout) :: ermsg
         
           ! Locals
           integer(kind=int4), automatic :: j,i,ioerr
           logical(kind=int4), automatic :: present
           ! Exec code ...
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status == -1) then
              err = -9999
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr > 0) then
              err = ioerr
              return
           end if
!....
           do j=0, nth
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) reg.samp_delta(j)
              if(ioerr >0.or.ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i,j)
                 if(ioerr > 0.or.ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_TSC_RATIO_ZEN
       

!DIR$ ATTRIBUTES INLINE :: initMSR_MCA_INTR_CFG_ZEN
     subroutine initMSR_MCA_INTR_CFG_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MCA_INTR_CFG_ZEN
           type(MSR_MCA_INTR_CFG_ZEN),    intent(inout) :: reg
           ! Exec code ...
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_MCA_INTR_CFG_ZEN

!DIR$ ATTRIBUTES INLINE :: initMSR_SYS_CFG_ZEN
     subroutine initMSR_SYS_CFG_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_SYS_CFG_ZEN
           type(MSR_SYS_CFG_ZEN),         intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_SYS_CFG_ZEN

     subroutine AccessMSR_SYS_CFG_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_SYS_CFG_ZEN
           type(MSR_SYS_CFG_ZEN),          intent(in) :: reg
           character(len=*),               intent(in) :: command
           character(len=2),               intent(in) :: core
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_SYS_CFG_ZEN

     subroutine ReadMSR_SYS_CFG_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_SYS_CFG_ZEN
           type(MSR_SYS_CFG_ZEN),              intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')  
     end subroutine ReadMSR_SYS_CFG_ZEN
     
!DIR$ ATTRIBUTES INLINE :: initMSR_HW_CFG_ZEN
     subroutine initMSR_HW_CFG_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_HW_CFG_ZEN
           type(MSR_HW_CFG_ZEN),         intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_HW_CFG_ZEN

     subroutine AccessMSR_HW_CFG_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_HW_CFG_ZEN
           type(MSR_HW_CFG_ZEN),           intent(in) :: reg
           character(len=*),               intent(in) :: command
           character(len=2),               intent(in) :: core
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_HW_CFG_ZEN

     subroutine ReadMSR_HW_CFG_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_HW_CFG_ZEN
           type(MSR_HW_CFG_ZEN),               intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
               read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
               if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_HW_CFG_ZEN 

!DIR$ ATTRIBUTES INLINE :: initMSR_TOP_MEM_ZEN
      subroutine initMSR_TOP_MEM_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_TOP_MEM_ZEN
           type(MSR_TOP_MEM_ZEN),        intent(inout) :: reg
           ! Exec code ...
           reg.msr_read = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_TOP_MEM_ZEN

     subroutine AccessMSR_TOP_MEM_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_TOP_MEM_ZEN
           type(MSR_TOP_MEM_ZEN),          intent(in) :: reg
           character(len=*),               intent(in) :: command
           character(len=2),               intent(in) :: core
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_TOP_MEM_ZEN   

     subroutine ReadMSR_TOP_MEM_ZEN(reg,iounit,ncores,fname,status,err,emsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_TOP_MEM_ZEN
           type(MSR_TOP_MEM_ZEN),              intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                 if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
         end subroutine ReadMSR_TOP_MEM_ZEN
         
!DIR$ ATTRIBUTES INLINE :: initMSR_TOP_MEM2_ZEN
     subroutine initMSR_TOP_MEM2_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_TOP_MEM2_ZEN
           type(MSR_TOP_MEM2_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_TOP_MEM2_ZEN

     subroutine AccessMSR_TOP_MEM2_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_TOP_MEM2_ZEN
           type(MSR_TOP_MEM2_ZEN),    intent(inout) :: reg
           character(len=*),          intent(in) :: command
           character(len=2),          intent(in) :: core
           character(len=*),          intent(in) :: fname
           integer(kind=int2),        intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_TOP_MEM2_ZEN

     subroutine ReadMSR_TOP_MEM2_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_TOP_MEM2_ZEN
           type(MSR_TOP_MEM2_ZEN),             intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                 if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_TOP_MEM2_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_IORR_BASE1_ZEN
     subroutine initMSR_IORR_BASE1_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IORR_BASE1_ZEN
           type(MSR_IORR_BASE1_ZEN),    intent(inout) :: reg
           ! Exec code .....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_IORR_BASE1_ZEN

     subroutine AccessMSR_IORR_BASE1_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IORR_BASE1_ZEN
           type(MSR_IORR_BASE1_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
           character(len=2),            intent(in) :: core
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_IORR_BASE1_ZEN

     subroutine ReadMSR_IORR_BASE1_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IORR_BASE1_ZEN
           type(MSR_IORR_BASE1_ZEN),           intent(in)    :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                 if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_IORR_BASE1_ZEN

       !DIR$ ATTRIBUTES INLINE :: initMSR_IORR_BASE2_ZEN
     subroutine initMSR_IORR_BASE2_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IORR_BASE2_ZEN
           type(MSR_IORR_BASE2_ZEN),    intent(inout) :: reg
           ! Exec code .....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_IORR_BASE2_ZEN

     subroutine AccessMSR_IORR_BASE2_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IORR_BASE2_ZEN
           type(MSR_IORR_BASE2_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
           character(len=2),            intent(in) :: core
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_IORR_BASE2_ZEN

     subroutine ReadMSR_IORR_BASE2_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IORR_BASE2_ZEN
           type(MSR_IORR_BASE2_ZEN),           intent(in)    :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                 if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_IORR_BASE2_ZEN

       !DIR$ ATTRIBUTES INLINE :: initMSR_IORR_BASE3_ZEN
     subroutine initMSR_IORR_BASE3_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IORR_BASE3_ZEN
           type(MSR_IORR_BASE3_ZEN),    intent(inout) :: reg
           ! Exec code .....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_IORR_BASE3_ZEN

     subroutine AccessMSR_IORR_BASE1_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IORR_BASE3_ZEN
           type(MSR_IORR_BASE3_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
           character(len=2),            intent(in) :: core
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_IORR_BASE3_ZEN

     subroutine ReadMSR_IORR_BASE3_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IORR_BASE3_ZEN
           type(MSR_IORR_BASE1_ZEN),           intent(in)    :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                 if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_IORR_BASE3_ZEN

       !DIR$ ATTRIBUTES INLINE :: initMSR_IORR_MASK1_ZEN
     subroutine initMSR_IORR_MASK1_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IORR_MASK1_ZEN
           type(MSR_IORR_MASK1_ZEN),    intent(inout) :: reg
           ! Exec code .....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_IORR_MASK1_ZEN

     subroutine AccessMSR_IORR_MASK1_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IORR_MASK1_ZEN
           type(MSR_IORR_MASK1_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
           character(len=2),            intent(in) :: core
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_IORR_MASK1_ZEN

     subroutine ReadMSR_IORR_MASK1_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IORR_MASK1_ZEN
           type(MSR_IORR_MASK1_ZEN),           intent(in)    :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                 if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_IORR_MASK1_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_IORR_MASK2_ZEN
     subroutine initMSR_IORR_MASK2_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IORR_MASK2_ZEN
           type(MSR_IORR_MASK2_ZEN),    intent(inout) :: reg
           ! Exec code .....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_IORR_MASK2_ZEN

     subroutine AccessMSR_IORR_MASK2_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IORR_MASK2_ZEN
           type(MSR_IORR_MASK2_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
           character(len=2),            intent(in) :: core
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_IORR_MASK2_ZEN

     subroutine ReadMSR_IORR_MASK2_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IORR_MASK2_ZEN
           type(MSR_IORR_MASK1_ZEN),           intent(in)    :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                 if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_IORR_MASK2_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_IORR_MASK3_ZEN
     subroutine initMSR_IORR_MASK3_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IORR_MASK3_ZEN
           type(MSR_IORR_MASK3_ZEN),    intent(inout) :: reg
           ! Exec code .....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_IORR_MASK3_ZEN

     subroutine AccessMSR_IORR_MASK3_ZEN(reg,command,core,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IORR_MASK3_ZEN
           type(MSR_IORR_MASK3_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
           character(len=2),            intent(in) :: core
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//core//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_IORR_MASK3_ZEN

     subroutine ReadMSR_IORR_MASK3_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IORR_MASK3_ZEN
           type(MSR_IORR_MASK1_ZEN),           intent(in)    :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: ncores
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, ncores
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                 if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_IORR_MASK3_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PERF_LEGACY_CTL0_ZEN    
     subroutine initMSR_PERF_LEGACY_CTL0_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PERF_LEGACY_CTL0_ZEN
           type(MSR_PERF_LEGACY_CTL0_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_PERF_LEGACY_CTL0_ZEN
      
     subroutine AccessMSR_PERF_LEGACY_CTL0_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTL0_ZEN
           type(MSR_PERF_LEGACY_CTL0_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           character(len=2),                  intent(in)    :: hwth ! HW thread number
           integer(kind=int2),                intent(inout) :: ier
          
      
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
     end subroutine AccessMSR_PERF_LEGACY_CTL0_ZEN

     subroutine ReadMSR_PERF_LEGACY_CTL0_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PERF_LEGACY_CTL0_ZEN
           type(MSR_PERF_LEGACY_CTL0_ZEN),     intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: nth
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')  
     end subroutine ReadMSR_PERF_LEGACY_CTL0_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PERF_LEGACY_CTL1_ZEN    
     subroutine initMSR_PERF_LEGACY_CTL1_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PERF_LEGACY_CTL1_ZEN
           type(MSR_PERF_LEGACY_CTL1_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_PERF_LEGACY_CTL1_ZEN
      
     subroutine AccessMSR_PERF_LEGACY_CTL1_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTL1_ZEN
           type(MSR_PERF_LEGACY_CTL1_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           character(len=2),                  intent(in)    :: hwth ! HW thread number
           integer(kind=int2),                intent(inout) :: ier
          
      
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
     end subroutine AccessMSR_PERF_LEGACY_CTL1_ZEN

     subroutine ReadMSR_PERF_LEGACY_CTL1_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PERF_LEGACY_CTL1_ZEN
           type(MSR_PERF_LEGACY_CTL1_ZEN),     intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: nth
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')  
     end subroutine ReadMSR_PERF_LEGACY_CTL1_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PERF_LEGACY_CTL2_ZEN    
     subroutine initMSR_PERF_LEGACY_CTL2_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PERF_LEGACY_CTL2_ZEN
           type(MSR_PERF_LEGACY_CTL2_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_PERF_LEGACY_CTL2_ZEN
      
     subroutine AccessMSR_PERF_LEGACY_CTL2_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTL2_ZEN
           type(MSR_PERF_LEGACY_CTL2_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           character(len=2),                  intent(in)    :: hwth ! HW thread number
           integer(kind=int2),                intent(inout) :: ier
          
      
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
     end subroutine AccessMSR_PERF_LEGACY_CTL2_ZEN

     subroutine ReadMSR_PERF_LEGACY_CTL2_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PERF_LEGACY_CTL2_ZEN
           type(MSR_PERF_LEGACY_CTL2_ZEN),     intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: nth
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')  
     end subroutine ReadMSR_PERF_LEGACY_CTL2_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PERF_LEGACY_CTL3_ZEN    
     subroutine initMSR_PERF_LEGACY_CTL3_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PERF_LEGACY_CTL3_ZEN
           type(MSR_PERF_LEGACY_CTL3_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_PERF_LEGACY_CTL3_ZEN
      
     subroutine AccessMSR_PERF_LEGACY_CTL3_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTL3_ZEN
           type(MSR_PERF_LEGACY_CTL3_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           character(len=2),                  intent(in)    :: hwth ! HW thread number
           integer(kind=int2),                intent(inout) :: ier
          
      
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
     end subroutine AccessMSR_PERF_LEGACY_CTL3_ZEN

     subroutine ReadMSR_PERF_LEGACY_CTL3_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PERF_LEGACY_CTL3_ZEN
           type(MSR_PERF_LEGACY_CTL3_ZEN),     intent(inout) :: reg
           integer(kind=int4),                 intent(in)    :: iounit
           integer(kind=int4),                 intent(in)    :: nth
           character(len=*),                   intent(in)    :: fname
           integer(kind=int2),                 intent(in)    :: status
           integer(kind=int4),                 intent(inout) :: err
           character(len=256),                 intent(inout) :: ermsg
           ! Locals
           logical(kind=int4), automatic :: present
           integer(kind=int4), automatic :: i,ioerr
           ! Exec code .....
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status ==-1) then
              err = -9999
              return
           end if
           ioerr = 0
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr >  0) then
              err = ioerr
              return
           end if
           do i=0, nth
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')  
     end subroutine ReadMSR_PERF_LEGACY_CTL3_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PERF_LEGACY_CTR0_ZEN
     subroutine initMSR_PERF_LEGACY_CTR0_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PERF_LEGACY_CTR0_ZEN
           type(MSR_PERF_LEGACY_CTR0_ZEN),    intent(inout) :: reg
           ! Exec code ....
           reg.samp_delta = 0.0_dp
           reg.msr_read   = zero_val
     end subroutine initMSR_PERF_LEGACY_CTR0_ZEN
    
     subroutine AccessMSR_PERF_LEGACY_CTR0_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTR0_ZEN
           type(MSR_PERF_LEGACY_CTR0_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           character(len=2),                  intent(in)    :: hwth ! HW thread number
           integer(kind=int2),                intent(inout) :: ier
          
      
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
     end subroutine AccessMSR_PERF_LEGACY_CTR0_ZEN
    
     subroutine ReadMSR_PERF_LEGACY_CTR0_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PERF_LEGACY_CTR0_ZEN
           type(MSR_PERF_LEGACY_CTR0_ZEN),      intent(inout) :: reg
           integer(kind=int4),                  intent(in)    :: iounit
           integer(kind=int4),                  intent(in)    :: nth
           character(len=*),                    intent(in)    :: fname
           integer(kind=int2),                  intent(in)    :: status
           integer(kind=int4),                  intent(inout) :: err
           character(len=256),                  intent(inout) :: ermsg
         
           ! Locals
           integer(kind=int4), automatic :: j,i,ioerr
           logical(kind=int4), automatic :: present
           ! Exec code ...
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status == -1) then
              err = -9999
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr > 0) then
              err = ioerr
              return
           end if
!....
           do j=0, nth
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) reg.samp_delta(j)
              if(ioerr >0.or.ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i,j)
                 if(ioerr > 0.or.ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_PERF_LEGACY_CTR0_ZEN

       !DIR$ ATTRIBUTES INLINE :: initMSR_PERF_LEGACY_CTR1_ZEN
     subroutine initMSR_PERF_LEGACY_CTR1_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PERF_LEGACY_CTR1_ZEN
           type(MSR_PERF_LEGACY_CTR1_ZEN),    intent(inout) :: reg
           ! Exec code ....
           reg.samp_delta = 0.0_dp
           reg.msr_read   = zero_val
     end subroutine initMSR_PERF_LEGACY_CTR1_ZEN
    
     subroutine AccessMSR_PERF_LEGACY_CTR1_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTR1_ZEN
           type(MSR_PERF_LEGACY_CTR1_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           character(len=2),                  intent(in)    :: hwth ! HW thread number
           integer(kind=int2),                intent(inout) :: ier
          
      
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
     end subroutine AccessMSR_PERF_LEGACY_CTR1_ZEN
    
     subroutine ReadMSR_PERF_LEGACY_CTR1_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PERF_LEGACY_CTR1_ZEN
           type(MSR_PERF_LEGACY_CTR1_ZEN),      intent(inout) :: reg
           integer(kind=int4),                  intent(in)    :: iounit
           integer(kind=int4),                  intent(in)    :: nth
           character(len=*),                    intent(in)    :: fname
           integer(kind=int2),                  intent(in)    :: status
           integer(kind=int4),                  intent(inout) :: err
           character(len=256),                  intent(inout) :: ermsg
         
           ! Locals
           integer(kind=int4), automatic :: j,i,ioerr
           logical(kind=int4), automatic :: present
           ! Exec code ...
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status == -1) then
              err = -9999
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr > 0) then
              err = ioerr
              return
           end if
!....
           do j=0, nth
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) reg.samp_delta(j)
              if(ioerr >0.or.ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i,j)
                 if(ioerr > 0.or.ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_PERF_LEGACY_CTR1_ZEN

       !DIR$ ATTRIBUTES INLINE :: initMSR_PERF_LEGACY_CTR2_ZEN
     subroutine initMSR_PERF_LEGACY_CTR2_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PERF_LEGACY_CTR2_ZEN
           type(MSR_PERF_LEGACY_CTR2_ZEN),    intent(inout) :: reg
           ! Exec code ....
           reg.samp_delta = 0.0_dp
           reg.msr_read   = zero_val
     end subroutine initMSR_PERF_LEGACY_CTR2_ZEN
    
     subroutine AccessMSR_PERF_LEGACY_CTR2_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTR2_ZEN
           type(MSR_PERF_LEGACY_CTR2_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           character(len=2),                  intent(in)    :: hwth ! HW thread number
           integer(kind=int2),                intent(inout) :: ier
          
      
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
     end subroutine AccessMSR_PERF_LEGACY_CTR2_ZEN
    
     subroutine ReadMSR_PERF_LEGACY_CTR2_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PERF_LEGACY_CTR2_ZEN
           type(MSR_PERF_LEGACY_CTR2_ZEN),      intent(inout) :: reg
           integer(kind=int4),                  intent(in)    :: iounit
           integer(kind=int4),                  intent(in)    :: nth
           character(len=*),                    intent(in)    :: fname
           integer(kind=int2),                  intent(in)    :: status
           integer(kind=int4),                  intent(inout) :: err
           character(len=256),                  intent(inout) :: ermsg
         
           ! Locals
           integer(kind=int4), automatic :: j,i,ioerr
           logical(kind=int4), automatic :: present
           ! Exec code ...
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status == -1) then
              err = -9999
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr > 0) then
              err = ioerr
              return
           end if
!....
           do j=0, nth
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) reg.samp_delta(j)
              if(ioerr >0.or.ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i,j)
                 if(ioerr > 0.or.ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_PERF_LEGACY_CTR2_ZEN

         
       !DIR$ ATTRIBUTES INLINE :: initMSR_PERF_LEGACY_CTR3_ZEN
     subroutine initMSR_PERF_LEGACY_CTR3_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PERF_LEGACY_CTR3_ZEN
           type(MSR_PERF_LEGACY_CTR3_ZEN),    intent(inout) :: reg
           ! Exec code ....
           reg.samp_delta = 0.0_dp
           reg.msr_read   = zero_val
     end subroutine initMSR_PERF_LEGACY_CTR3_ZEN
    
     subroutine AccessMSR_PERF_LEGACY_CTR3_ZEN(reg,command,reset,fname,hwth,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTR3_ZEN
           type(MSR_PERF_LEGACY_CTR3_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           character(len=2),                  intent(in)    :: hwth ! HW thread number
           integer(kind=int2),                intent(inout) :: ier
          
      
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
     end subroutine AccessMSR_PERF_LEGACY_CTR3_ZEN
    
     subroutine ReadMSR_PERF_LEGACY_CTR3_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PERF_LEGACY_CTR3_ZEN
           type(MSR_PERF_LEGACY_CTR3_ZEN),      intent(inout) :: reg
           integer(kind=int4),                  intent(in)    :: iounit
           integer(kind=int4),                  intent(in)    :: nth
           character(len=*),                    intent(in)    :: fname
           integer(kind=int2),                  intent(in)    :: status
           integer(kind=int4),                  intent(inout) :: err
           character(len=256),                  intent(inout) :: ermsg
         
           ! Locals
           integer(kind=int4), automatic :: j,i,ioerr
           logical(kind=int4), automatic :: present
           ! Exec code ...
           present = .false.
           inquire(FILE=trim(fname),EXIST=present)
           if(.not.present .or. status == -1) then
              err = -9999
              return
           end if
           open(UNIT=iounit,FILE=trim(fname),ACTION='READ',STATUS='OLD',IOMSG=ermsg,IOSTAT=ioerr)
           if(ioerr > 0) then
              err = ioerr
              return
           end if
!....
           do j=0, nth
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) reg.samp_delta(j)
              if(ioerr >0.or.ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i,j)
                 if(ioerr > 0.or.ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_PERF_LEGACY_CTR3_ZEN

    
    
         
     
end module mod_zen_msrtools_wrapper
