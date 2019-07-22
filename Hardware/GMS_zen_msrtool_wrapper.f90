

module mod_zen_msrtools_wrappers


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
     public
     ! File and module data
     integer(kind=int4),  parameter, public :: MOD_ZEN_MSRTOOL_WRAPPER_MAJOR = 1
     integer(kind=int4),  parameter, public :: MOD_ZEN_MSRTOOL_WRAPPER_MINOR = 0
     integer(kind=int4),  parameter, public :: MOD_ZEN_MSRTOOL_WRAPPER_MICRO = 0
     integer(kind=int4),  parameter, public :: MOD_ZEN_MSRTOOL_WRAPPER_FULLVER = 1000*MOD_ZEN_MSRTOOL_WRAPPER_MAJOR + &
                                                                                 100*MOD_ZEN_MSRTOOL_WRAPPER_MINOR  + &
                                                                                 10*MOD_ZEN_MSRTOOL_WRAPPER_MICRO
     character(*),        parameter, public :: MOD_ZEN_MSRTOOL_WRAPPER_CREATION_DATE = "20-07-2019 10:51 +00200 (SAT 20 JUL 2019 GMT+2)"
     character(*),        parameter, public :: MOD_ZEN_MSRTOOL_WRAPPER_BUILD_DATE    = "00-00-0000 00:00"
     character(*),        parameter, public :: MOD_ZEN_MSRTOOL_WRAPPER_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     character(*),        parameter, public :: MOD_ZEN_MSRTOOL_WRAPPER_SYNOPSIS      = "Fortran wrappers around msr-tools program."

    ! Usage: rdmsr [options] regno
    !--help         -h  Print this help
    !--version      -V  Print current version
    ! --hexadecimal  -x  Hexadecimal output (lower case)
    ! --capital-hex  -X  Hexadecimal output (upper case)
    !--decimal      -d  Signed decimal output
    !--unsigned     -u  Unsigned decimal output
    !--octal        -o  Octal output
    !--c-language   -c  Format output as a C language constant
    !--zero-pad     -0  Output leading zeroes
    !--raw          -r  Raw binary output
    !--all          -a  all processors  <------- USE ALWAYS THIS OPTION
    !--processor #  -p  Select processor number (default 0)
    !--bitfield h:l -f  Output bits [h:l] only


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
     
     subroutine AccessMSR_TSC_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_TSC_ZEN
     
       type(MSR_TSC_ZEN),    intent(in)    :: reg
       character(len=*),     intent(in)    :: command
       logical(kind=int1),   intent(in)    :: reset
       character(len=*),     intent(in)    :: fname
       integer(kind=int2),   intent(inout) :: ier
       
      
      
       ! Locals
       character(len=128), automatic :: string
       integer(kind=int2), automatic :: stat
       
       ! Exec code ....
       ! Always dump all cores
       if(.not. reset) then
          string = command//reg.addr_hex//fname
          stat = RUNQQ(rmsr,string)
          if(stat == -1) then
             ier = stat
          end if
        
       else
          string = command//reg.addr_hex//reset_val
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

     subroutine AccessMSR_APIC_BAR_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_APIC_BAR_ZEN
           type(MSR_APIC_BAR_ZEN),       intent(in) :: reg
           character(len=*),             intent(in) :: command
           character(len=*),             intent(in) :: fname
          
           integer(kind=int2),           intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_MPERF_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MPERF_ZEN
           type(MSR_MPERF_ZEN),      intent(in) :: reg
           character(len=*),         intent(in) :: command
           logical(kind=int1),       intent(in) :: reset
           !
           character(len=*),         intent(in) :: fname
           integer(kind=int2),       intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//reg.addr_hex//reset_val
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
     
     subroutine AccessMSR_APERF_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_APERF_ZEN
           type(MSR_APERF_ZEN),      intent(in) :: reg
           character(len=*),         intent(in) :: command
           logical(kind=int1),       intent(in) :: reset
           !
           character(len=*),         intent(in) :: fname
           integer(kind=int2),       intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//reg.addr_hex//reset_val
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

     subroutine AccessMSR_MTRR_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_ZEN
           type(MSR_MTRR_ZEN),      intent(in) :: reg
           character(len=*),        intent(in) :: command
           !
           character(len=*),        intent(in) :: fname
           integer(kind=int2),      intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_MCG_CAP_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MCG_CAP_ZEN
           type(MSR_MCG_CAP_ZEN),     intent(in) :: reg
           character(len=*),          intent(in) :: command
           !
           character(len=*),          intent(in) :: fname
           integer(kind=int2),        intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
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
       
     subroutine AccessMSR_MCG_STAT_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MCG_STAT_ZEN
           type(MSR_MCG_STAT_ZEN),    intent(in) :: reg
           character(len=*),          intent(in) :: command
           !
           character(len=*),          intent(in) :: fname
           integer(kind=int2),        intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_MCG_CTL_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MCG_CTL_ZEN
           type(MSR_MCG_CTL_ZEN),     intent(in) :: reg
           character(len=*),          intent(in) :: command
           !
           character(len=*),          intent(in) :: fname
           integer(kind=int2),        intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_DBG_CTL_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_DBG_CTL_ZEN
           type(MSR_DBG_CTL_ZEN),       intent(in) :: reg
           character(len=*),            intent(in) :: command
           !
           logical(kind=int1),          intent(in) :: reset
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//reg.addr_hex//reset_val
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

     subroutine AccessMSR_BR_FROM_IP_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_BR_FROM_IP_ZEN
           type(MSR_BR_FROM_IP_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
           !
           logical(kind=int1),          intent(in) :: reset
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//reg.addr_hex//reset_val
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

     subroutine AccessMSR_BR_TO_IP_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_BR_TO_IP_ZEN
           type(MSR_BR_TO_IP_ZEN),      intent(in) :: reg
           character(len=*),            intent(in) :: command
           !
           logical(kind=int1),          intent(in) :: reset
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//reg.addr_hex//reset_val
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

     subroutine AccessMSR_LAST_EXP_FROM_IP_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_LAST_EXP_FROM_IP_ZEN
           type(MSR_LAST_EXP_FROM_IP_ZEN),    intent(in) :: reg
           character(len=*),                  intent(in) :: command
           
           logical(kind=int1),                intent(in) :: reset
           character(len=*),                  intent(in) :: fname
           integer(kind=int2),                intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//reg.addr_hex//reset_val
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

     subroutine AccessMSR_LAST_EXP_TO_IP_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_LAST_EXP_TO_IP_ZEN
           type(MSR_LAST_EXP_TO_IP_ZEN),       intent(in) :: REG
           character(len=*),                   intent(in) :: command
           !
           logical(kind=int1),                 intent(in) :: reset
           character(len=*),                   intent(in) :: fname
           integer(kind=int2),                 intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           if(.not. reset) then
              string = command//reg.addr_hex//fname
              stat   = RUNQQ(rmsr,string)
              if(stat == -1) then
                 ier = stat
              end if
           else
              string = command//reg.addr_hex//reset_val
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

     subroutine AccessMSR_MTRR_FIXED_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED_ZEN
           type(MSR_MTRR_FIXED_ZEN),   intent(in) :: reg
           character(len=*),           intent(in) :: command
           !
           character(len=*),           intent(in) :: fname
           integer(kind=int2),         intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_MTRR_FIXED16K_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED16K_ZEN
           type(MSR_MTRR_FIXED16K_ZEN),    intent(in) :: reg
           character(len=*),               intent(in) :: command
           !
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

         
     subroutine AccessMSR_MTRR_FIXED16K1_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED16K1_ZEN
           type(MSR_MTRR_FIXED16K1_ZEN),   intent(in) :: reg
           character(len=*),               intent(in) :: command
           !
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

         
     subroutine AccessMSR_MTRR_FIXED4K_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K_ZEN
           type(MSR_MTRR_FIXED4K_ZEN),   intent(in) :: reg
           character(len=*),               intent(in) :: command
           !
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_MTRR_FIXED4K1_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K1_ZEN
           type(MSR_MTRR_FIXED4K1_ZEN),   intent(in) :: reg
           character(len=*),              intent(in) :: command
           !
           character(len=*),              intent(in) :: fname
           integer(kind=int2),            intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_MTRR_FIXED4K2_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K2_ZEN
           type(MSR_MTRR_FIXED4K2_ZEN),   intent(in) :: reg
           character(len=*),              intent(in) :: command
           !
           character(len=*),              intent(in) :: fname
           integer(kind=int2),            intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

      subroutine AccessMSR_MTRR_FIXED4K3_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K3_ZEN
           type(MSR_MTRR_FIXED4K3_ZEN),   intent(in) :: reg
           character(len=*),              intent(in) :: command
           !
           character(len=*),              intent(in) :: fname
           integer(kind=int2),            intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_MTRR_FIXED4K4_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K4_ZEN
           type(MSR_MTRR_FIXED4K4_ZEN),   intent(in) :: reg
           character(len=*),              intent(in) :: command
           !
           character(len=*),              intent(in) :: fname
           integer(kind=int2),            intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_MTRR_FIXED4K5_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K5_ZEN
           type(MSR_MTRR_FIXED4K5_ZEN),   intent(in) :: reg
           character(len=*),              intent(in) :: command
           !
           character(len=*),              intent(in) :: fname
           integer(kind=int2),            intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_MTRR_FIXED4K6_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K6_ZEN
           type(MSR_MTRR_FIXED4K6_ZEN),    intent(in) :: reg
           character(len=*),               intent(in) :: command
           !
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_MTRR_FIXED4K7_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_FIXED4K7_ZEN
           type(MSR_MTRR_FIXED4K7_ZEN),    intent(in)  :: reg
           character(len=*),               intent(in) :: command
           !
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_PAT_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PAT_ZEN
           type(MSR_PAT_ZEN),              intent(in) :: reg
           character(len=*),               intent(in) :: command
           !
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_MTRR_DEFTYPE_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MTRR_DEFTYPE_ZEN
           type(MSR_MTRR_DEFTYPE_ZEN),     intent(in) :: reg
           character(len=*),               intent(in) :: command
           !
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_EFER_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_EFER_ZEN
           type(MSR_EFER_ZEN),             intent(in) :: reg
           character(len=*),               intent(in) :: command
           !
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

         
     subroutine AccessMSR_MPERF_READONLY_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MPERF_READONLY_ZEN
           type(MSR_MPERF_READONLY_ZEN),    intent(in) :: reg
           character(len=*),     intent(in)    :: command
           logical(kind=int1),   intent(in)    :: reset
           character(len=*),     intent(in)    :: fname
           !
           !
           integer(kind=int2),   intent(inout) :: ier
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
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

     subroutine AccessMSR_APERF_READONLY_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_APERF_READONLY_ZEN
           type(MSR_APERF_READONLY_ZEN),      intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           !
           !
           integer(kind=int2),   intent(inout) :: ier
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
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

     subroutine AccessMSR_IPERF_COUNT_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IPERF_COUNT_ZEN
           type(MSR_IPERF_COUNT_ZEN),         intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           !
          
           integer(kind=int2),   intent(inout) :: ier
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
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

     subroutine AccessMSR_TSC_AUX_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_TSC_AUX_ZEN
           type(MSR_TSC_AUX_ZEN),             intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           !
          
           integer(kind=int2),   intent(inout) :: ier
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
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

      subroutine AccessMSR_TSC_RATIO_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_TSC_RATIO_ZEN
           type(MSR_TSC_RATIO_ZEN),           intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           !
           integer(kind=int2),                intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
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

     subroutine AccessMSR_SYS_CFG_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_SYS_CFG_ZEN
           type(MSR_SYS_CFG_ZEN),          intent(in) :: reg
           character(len=*),               intent(in) :: command
         
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_HW_CFG_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_HW_CFG_ZEN
           type(MSR_HW_CFG_ZEN),           intent(in) :: reg
           character(len=*),               intent(in) :: command
         
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_TOP_MEM_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_TOP_MEM_ZEN
           type(MSR_TOP_MEM_ZEN),          intent(in) :: reg
           character(len=*),               intent(in) :: command
           !
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
             ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_TOP_MEM2_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_TOP_MEM2_ZEN
           type(MSR_TOP_MEM2_ZEN),    intent(inout) :: reg
           character(len=*),          intent(in) :: command
          
           character(len=*),          intent(in) :: fname
           integer(kind=int2),        intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_IORR_BASE1_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IORR_BASE1_ZEN
           type(MSR_IORR_BASE1_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
          
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_IORR_BASE2_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IORR_BASE2_ZEN
           type(MSR_IORR_BASE2_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
        
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_IORR_BASE1_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IORR_BASE3_ZEN
           type(MSR_IORR_BASE3_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
         
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_IORR_MASK1_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IORR_MASK1_ZEN
           type(MSR_IORR_MASK1_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
         
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_IORR_MASK2_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IORR_MASK2_ZEN
           type(MSR_IORR_MASK2_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
     
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
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

     subroutine AccessMSR_IORR_MASK3_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IORR_MASK3_ZEN
           type(MSR_IORR_MASK3_ZEN),    intent(in) :: reg
           character(len=*),            intent(in) :: command
          
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
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
      
     subroutine AccessMSR_PERF_LEGACY_CTL0_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTL0_ZEN
           type(MSR_PERF_LEGACY_CTL0_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
         
           integer(kind=int2),                intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
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
      
     subroutine AccessMSR_PERF_LEGACY_CTL1_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTL1_ZEN
           type(MSR_PERF_LEGACY_CTL1_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
         
           integer(kind=int2),                intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
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
      
     subroutine AccessMSR_PERF_LEGACY_CTL2_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTL2_ZEN
           type(MSR_PERF_LEGACY_CTL2_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
         
           integer(kind=int2),                intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
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
      
     subroutine AccessMSR_PERF_LEGACY_CTL3_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTL3_ZEN
           type(MSR_PERF_LEGACY_CTL3_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
          
           integer(kind=int2),                intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
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
    
     subroutine AccessMSR_PERF_LEGACY_CTR0_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTR0_ZEN
           type(MSR_PERF_LEGACY_CTR0_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           !
           integer(kind=int2),                intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
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
    
     subroutine AccessMSR_PERF_LEGACY_CTR1_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTR1_ZEN
           type(MSR_PERF_LEGACY_CTR1_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
            !
           integer(kind=int2),                intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
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
    
     subroutine AccessMSR_PERF_LEGACY_CTR2_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTR2_ZEN
           type(MSR_PERF_LEGACY_CTR2_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           !
           integer(kind=int2),                intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
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
    
     subroutine AccessMSR_PERF_LEGACY_CTR3_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_LEGACY_CTR3_ZEN
           type(MSR_PERF_LEGACY_CTR3_ZEN),    intent(in)    :: reg
           character(len=*),                  intent(in)    :: command
           logical(kind=int1),                intent(in)    :: reset
           character(len=*),                  intent(in)    :: fname
           !
           integer(kind=int2),                intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
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

     !DIR$ ATTRIBUTES INLINE :: initMSR_MC_EXP_REDIR_ZEN
     subroutine initMSR_MC_EXP_REDIR_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MC_EXP_REDIR_ZEN
           type(MSR_MC_EXP_REDIR_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_MC_EXP_REDIR_ZEN

     subroutine AccessMSR_MC_EXP_REDIR_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MC_EXP_REDIR_ZEN
           type(MSR_MC_EXP_REDIR_ZEN),      intent(in) :: reg
           character(len=*),                intent(in) :: command
           character(len=*),                intent(in) :: fname
        
           integer(kind=int2),              intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
      end subroutine AccessMSR_MC_EXP_REDIR_ZEN

      subroutine ReadMSR_MC_EXP_REDIR_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MC_EXP_REDIR_ZEN
           type(MSR_MC_EXP_REDIR_ZEN),         intent(inout) :: reg
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
     end subroutine ReadMSR_MC_EXP_REDIR_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PROC_NAME_STRING0_ZEN    
     subroutine initMSR_PROC_NAME_STRING0_ZEN(reg)
 !DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PROC_NAME_STRING0_ZEN
           type(MSR_PROC_NAME_STRING0_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_PROC_NAME_STRING0_ZEN
    
     subroutine AccessMSR_PROC_NAME_STRING0_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PROC_NAME_STRING0_ZEN
           type(MSR_PROC_NAME_STRING0_ZEN),     intent(in) :: reg
           character(len=*),                    intent(in) :: command
           character(len=*),                    intent(in) :: fname
          
           integer(kind=int2),                  intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_PROC_NAME_STRING0_ZEN

     subroutine ReadMSR_PROC_NAME_STRING0_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PROC_NAME_STRING0_ZEN
           type(MSR_PROC_NAME_STRING0_ZEN),     intent(inout) :: reg
           integer(kind=int4),                  intent(in)    :: iounit
           integer(kind=int4),                  intent(in)    :: ncores
           character(len=*),                    intent(in)    :: fname
           integer(kind=int2),                  intent(in)    :: status
           integer(kind=int4),                  intent(inout) :: err
           character(len=256),                  intent(inout) :: ermsg
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
     end subroutine ReadMSR_PROC_NAME_STRING0_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PROC_NAME_STRING1_ZEN    
     subroutine initMSR_PROC_NAME_STRING1_ZEN(reg)
 !DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PROC_NAME_STRING1_ZEN
           type(MSR_PROC_NAME_STRING1_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_PROC_NAME_STRING1_ZEN
    
     subroutine AccessMSR_PROC_NAME_STRING1_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PROC_NAME_STRING1_ZEN
           type(MSR_PROC_NAME_STRING1_ZEN),     intent(in) :: reg
           character(len=*),                    intent(in) :: command
           character(len=*),                    intent(in) :: fname
         
           integer(kind=int2),                  intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_PROC_NAME_STRING1_ZEN

     subroutine ReadMSR_PROC_NAME_STRING1_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PROC_NAME_STRING1_ZEN
           type(MSR_PROC_NAME_STRING1_ZEN),     intent(inout) :: reg
           integer(kind=int4),                  intent(in)    :: iounit
           integer(kind=int4),                  intent(in)    :: ncores
           character(len=*),                    intent(in)    :: fname
           integer(kind=int2),                  intent(in)    :: status
           integer(kind=int4),                  intent(inout) :: err
           character(len=256),                  intent(inout) :: ermsg
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
     end subroutine ReadMSR_PROC_NAME_STRING1_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PROC_NAME_STRING2_ZEN    
     subroutine initMSR_PROC_NAME_STRING2_ZEN(reg)
 !DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PROC_NAME_STRING2_ZEN
           type(MSR_PROC_NAME_STRING2_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_PROC_NAME_STRING2_ZEN
    
     subroutine AccessMSR_PROC_NAME_STRING2_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PROC_NAME_STRING2_ZEN
           type(MSR_PROC_NAME_STRING2_ZEN),     intent(in) :: reg
           character(len=*),                    intent(in) :: command
           character(len=*),                    intent(in) :: fname
          
           integer(kind=int2),                  intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_PROC_NAME_STRING2_ZEN

     subroutine ReadMSR_PROC_NAME_STRING2_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PROC_NAME_STRING2_ZEN
           type(MSR_PROC_NAME_STRING2_ZEN),     intent(inout) :: reg
           integer(kind=int4),                  intent(in)    :: iounit
           integer(kind=int4),                  intent(in)    :: ncores
           character(len=*),                    intent(in)    :: fname
           integer(kind=int2),                  intent(in)    :: status
           integer(kind=int4),                  intent(inout) :: err
           character(len=256),                  intent(inout) :: ermsg
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
     end subroutine ReadMSR_PROC_NAME_STRING2_ZEN

         
     !DIR$ ATTRIBUTES INLINE :: initMSR_PROC_NAME_STRING3_ZEN    
     subroutine initMSR_PROC_NAME_STRING3_ZEN(reg)
 !DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PROC_NAME_STRING3_ZEN
           type(MSR_PROC_NAME_STRING3_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_PROC_NAME_STRING3_ZEN
    
     subroutine AccessMSR_PROC_NAME_STRING3_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PROC_NAME_STRING3_ZEN
           type(MSR_PROC_NAME_STRING3_ZEN),     intent(in) :: reg
           character(len=*),                    intent(in) :: command
           character(len=*),                    intent(in) :: fname
          
           integer(kind=int2),                  intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_PROC_NAME_STRING3_ZEN

     subroutine ReadMSR_PROC_NAME_STRING3_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PROC_NAME_STRING3_ZEN
           type(MSR_PROC_NAME_STRING3_ZEN),     intent(inout) :: reg
           integer(kind=int4),                  intent(in)    :: iounit
           integer(kind=int4),                  intent(in)    :: ncores
           character(len=*),                    intent(in)    :: fname
           integer(kind=int2),                  intent(in)    :: status
           integer(kind=int4),                  intent(inout) :: err
           character(len=256),                  intent(inout) :: ermsg
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
     end subroutine ReadMSR_PROC_NAME_STRING3_ZEN

        !DIR$ ATTRIBUTES INLINE :: initMSR_PROC_NAME_STRING4_ZEN    
     subroutine initMSR_PROC_NAME_STRING4_ZEN(reg)
 !DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PROC_NAME_STRING4_ZEN
           type(MSR_PROC_NAME_STRING4_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_PROC_NAME_STRING4_ZEN
    
     subroutine AccessMSR_PROC_NAME_STRING4_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PROC_NAME_STRING4_ZEN
           type(MSR_PROC_NAME_STRING4_ZEN),     intent(in) :: reg
           character(len=*),                    intent(in) :: command
           character(len=*),                    intent(in) :: fname
          
           integer(kind=int2),                  intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_PROC_NAME_STRING4_ZEN

     subroutine ReadMSR_PROC_NAME_STRING4_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PROC_NAME_STRING4_ZEN
           type(MSR_PROC_NAME_STRING4_ZEN),     intent(inout) :: reg
           integer(kind=int4),                  intent(in)    :: iounit
           integer(kind=int4),                  intent(in)    :: ncores
           character(len=*),                    intent(in)    :: fname
           integer(kind=int2),                  intent(in)    :: status
           integer(kind=int4),                  intent(inout) :: err
           character(len=256),                  intent(inout) :: ermsg
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
     end subroutine ReadMSR_PROC_NAME_STRING4_ZEN

        !DIR$ ATTRIBUTES INLINE :: initMSR_PROC_NAME_STRING5_ZEN    
     subroutine initMSR_PROC_NAME_STRING5_ZEN(reg)
 !DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PROC_NAME_STRING5_ZEN
           type(MSR_PROC_NAME_STRING5_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_PROC_NAME_STRING5_ZEN
    
     subroutine AccessMSR_PROC_NAME_STRING5_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PROC_NAME_STRING5_ZEN
           type(MSR_PROC_NAME_STRING5_ZEN),     intent(in) :: reg
           character(len=*),                    intent(in) :: command
           character(len=*),                    intent(in) :: fname
          
           integer(kind=int2),                  intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessMSR_PROC_NAME_STRING5_ZEN

     subroutine ReadMSR_PROC_NAME_STRING5_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PROC_NAME_STRING5_ZEN
           type(MSR_PROC_NAME_STRING3_ZEN),     intent(inout) :: reg
           integer(kind=int4),                  intent(in)    :: iounit
           integer(kind=int4),                  intent(in)    :: ncores
           character(len=*),                    intent(in)    :: fname
           integer(kind=int2),                  intent(in)    :: status
           integer(kind=int4),                  intent(inout) :: err
           character(len=256),                  intent(inout) :: ermsg
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
     end subroutine ReadMSR_PROC_NAME_STRING5_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_MMIO_CFG_BASE_ADDR_ZEN    
     subroutine initMSR_MMIO_CFG_BASE_ADDR_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_MMIO_CFG_BASE_ADDR_ZEN
           type(MSR_MMIO_CFG_BASE_ADDR_ZEN),    intent(inout) :: reg
           ! Exec code ...
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_MMIO_CFG_BASE_ADDR_ZEN
             
     subroutine AccessMSR_MMIO_CFG_BASE_ADDR_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_MMIO_CFG_BASE_ADDR_ZEN
           type(MSR_MMIO_CFG_BASE_ADDR_ZEN),     intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
         
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if
      end subroutine AccessMSR_MMIO_CFG_BASE_ADDR_ZEN

      subroutine ReadMSR_MMIO_CFG_BASE_ADDR_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_MMIO_CFG_BASE_ADDR_ZEN
           type(MSR_MMIO_CFG_BASE_ADDR_ZEN),     intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: ncores
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_MMIO_CFG_BASE_ADDR_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_INT_PENDING_ZEN    
     subroutine initMSR_INT_PENDING_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_INT_PENDING_ZEN
           type(MSR_INT_PENDING_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_INT_PENDING_ZEN

     subroutine AccessMSR_INT_PENDING_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_INT_PENDING_ZEN
           type(MSR_INT_PENDING_ZEN),            intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
          
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_INT_PENDING_ZEN

     subroutine ReadMSR_INT_PENDING_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_INT_PENDING_ZEN
           type(MSR_INT_PENDING_ZEN),            intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: ncores
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_INT_PENDING_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_TRIG_IO_CYCLE_ZEN    
     subroutine initMSR_TRIG_IO_CYCLE_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_TRIG_IO_CYCLE_ZEN
            type(MSR_TRIG_IO_CYCLE_ZEN),    intent(inout) :: reg
            ! Exec code .....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_TRIG_IO_CYCLE_ZEN

     subroutine AccessMSR_TRIG_IO_CYCLE_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_TRIG_IO_CYCLE_ZEN
           type(MSR_TRIG_IO_CYCLE_ZEN),          intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
          
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_TRIG_IO_CYCLE_ZEN

     subroutine ReadMSR_TRIG_IO_CYCLE_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_TRIG_IO_CYCLE_ZEN
           type(MSR_TRIG_IO_CYCLE_ZEN),          intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: nth
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_TRIG_IO_CYCLE_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PSTATE_CUR_LIMIT_ZEN    
     subroutine initMSR_PSTATE_CUR_LIMIT_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PSTATE_CUR_LIMIT_ZEN
           type(MSR_PSTATE_CUR_LIMIT_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_PSTATE_CUR_LIMIT_ZEN

     subroutine AccessMSR_PSTATE_CUR_LIMIT_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PSTATE_CUR_LIMIT_ZEN
           type(MSR_PSTATE_CUR_LIMIT_ZEN),       intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
         
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_PSTATE_CUR_LIMIT_ZEN

     subroutine ReadMSR_PSTATE_CUR_LIMIT_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PSTATE_CUR_LIMIT_ZEN
           type(MSR_PSTATE_CUR_LIMIT_ZEN),       intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: ncores
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_PSTATE_CUR_LIMIT_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PSTATE_CTL_ZEN    
     subroutine initMSR_PSTATE_CTL_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PSTATE_CTL_ZEN
           type(MSR_PSTATE_CTL_ZEN),       intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_PSTATE_CTL_ZEN

     subroutine AccessMSR_PSTATE_CTL_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PSTATE_CTL_ZEN
           type(MSR_PSTATE_CTL_ZEN),       intent(in) :: reg
           character(len=*),               intent(in) :: command
           character(len=*),               intent(in) :: fname
         
           integer(kind=int2),             intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if  
     end subroutine AccessMSR_PSTATE_CTL_ZEN

     subroutine ReadMSR_PSTATE_CTL_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PSTATE_CTL_ZEN
           type(MSR_PSTATE_CTL_ZEN),             intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: nth
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_PSTATE_CTL_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PSTATE_STAT_ZEN
     subroutine initMSR_PSTATE_STAT_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PSTATE_STAT_ZEN
           type(MSR_PSTATE_STAT_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_PSTATE_STAT_ZEN

     subroutine AccessMSR_PSTATE_STAT_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PSTATE_STAT_ZEN
           type(MSR_PSTATE_STAT_ZEN),      intent(in) :: reg
           character(len=*),               intent(in) :: command
           character(len=*),               intent(in) :: fname
         
           integer(kind=int2),             intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
      end subroutine AccessMSR_PSTATE_STAT_ZEN

      subroutine ReadMSR_PSTATE_STAT_ZEN(reg,iounit,ncores,fname,stat,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PSTATE_STAT_ZEN
           type(MSR_PSTATE_STAT_ZEN),            intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: ncores
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_PSTATE_STAT_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PSTATE_DEFX_ZEN    
     subroutine initMSR_PSTATE_DEFX_ZEN(msrrd,msrwr,msrwh,length)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PSTATE_DEFX_ZEN
           integer(kind=int8b), dimension(0:length),  intent(inout) :: msrrd
           integer(kind=int8b), dimension(0:length),  intent(inout) :: msrwr
           character(len=16),   dimension(0:length),  intent(inout) :: msrwh
           integer(kind=int4),                        intent(in)    :: length
           ! Exec code .....
           msrrd = zero_val
           msrwr = zero_val
           msrwh = init_valh
      end subroutine initMSR_PSTATE_DEFX_ZEN

      subroutine AccessMSR_PSTATE_DEFX_ZEN(haddr,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PSTATE_DEFX_ZEN
           character(len=10),              intent(in) :: haddr
           character(len=*),               intent(in) :: command
           character(len=*),               intent(in) :: core
         
           integer(kind=int2),             intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//haddr//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
      end subroutine AccessMSR_PSTATE_DEFX_ZEN

      subroutine ReadMSR_PSTATE_DEFX_ZEN(msrr,nth,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PSTATE_DEFX_ZEN
           integer(kind=int8b),   dimension(0:nth),   intent(inout) :: msrr
           integer(kind=int4),                        intent(in)    :: nth
           integer(kind=int4),                        intent(in)    :: iounit
           character(len=*),                          intent(in)    :: fname
           integer(kind=int2),                        intent(in)    :: status
           integer(kind=int4),                        intent(inout) :: err
           character(len=256),                        intent(inout) :: ermsg
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
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msrr(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_PSTATE_DEFX_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_CSTATE_BASE_ADDRESS_ZEN
     subroutine initMSR_CSTATE_BASE_ADDRESS_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_CSTATE_BASE_ADDRESS_ZEN
           type(MSR_CSTATE_BASE_ADDRESS_ZEN),   intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_CSTATE_BASE_ADDRESS_ZEN

     subroutine AccessMSR_CSTATE_BASE_ADDRESS_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_CSTATE_BASE_ADDRESS_ZEN
           type(MSR_CSTATE_BASE_ADDRESS_ZEN),    intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
          
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
           
     end subroutine AccessMSR_CSTATE_BASE_ADDRESS_ZEN

     subroutine ReadMSR_CSTATE_BASE_ADDRESS_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_CSTATE_BASE_ADDRESS_ZEN
           type(MSR_CSTATE_BASE_ADDRESS_ZEN),    intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: ncores
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_CSTATE_BASE_ADDRESS_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_CPU_WDT_CFG_ZEN
     subroutine initMSR_CPU_WDT_CFG_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_CPU_WDT_CFG_ZEN
           type(MSR_CPU_WDT_CFG_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_CPU_WDT_CFG_ZEN

     subroutine AccessMSR_CPU_WDT_CFG_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_CPU_WDT_CFG_ZEN
           type(MSR_CPU_WDT_CFG_ZEN),            intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
         
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_CPU_WDT_CFG_ZEN

     subroutine ReadMSR_CPU_WDT_CFG_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_CPU_WDT_CFG_ZEN
           type(MSR_CPU_WDT_CFG_ZEN),            intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: ncores
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_CPU_WDT_CFG_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_SMM_BASE_ZEN
     subroutine initMSR_SMM_BASE_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_SMM_BASE_ZEN
           type(MSR_SMM_BASE_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = init_val
     end subroutine initMSR_SMM_BASE_ZEN

     subroutine AccessMSR_SMM_BASE_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_SMM_BASE_ZEN
           type(MSR_SMM_BASE_ZEN),               intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
         
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_SMM_BASE_ZEN

     subroutine ReadMSR_SMM_BASE_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_SMM_BASE_ZEN
           type(MSR_SMM_BASE_ZEN),               intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: ncores
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_SMM_BASE_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_SMM_CTL_ZEN
     subroutine initMSR_SMM_CTL_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_SMM_CTL_ZEN
           type(MSR_SMM_CTL_ZEN),       intent(inout) :: reg
           ! Exec code .....
           reg.msr_read = init_val
     end subroutine initMSR_SMM_CTL_ZEN

     subroutine AccessMSR_SMM_CTL_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_SMM_CTL_ZEN
           type(MSR_SMM_CTL_ZEN),                intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
         
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_SMM_CTL_ZEN

     subroutine ReadMSR_SMM_CTL_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_SMM_CTL_ZEN
           type(MSR_SMM_CTL_ZEN),                intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: nth
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_SMM_CTL_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_LOCAL_SMI_STAT_ZEN
     subroutine initMSR_LOCAL_SMI_STAT_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_LOCAL_SMI_STAT_ZEN
           type(MSR_LOCAL_SMI_STAT_ZEN),        intent(inout) :: reg
           ! Exec code .....
           reg.msr_read = init_val
     end subroutine initMSR_LOCAL_SMI_STAT_ZEN

     subroutine AccessMSR_LOCAL_SMI_STAT_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_LOCAL_SMI_STAT_ZEN
           type(MSR_LOCAL_SMI_STAT_ZEN),         intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
         
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_LOCAL_SMI_STAT_ZEN

     subroutine ReadMSR_LOCAL_SMI_STAT_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_LOCAL_SMI_STAT_ZEN
           type(MSR_LOCAL_SMI_STAT_ZEN),         intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: nth
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_LOCAL_SMI_STAT_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_PERF_CTLX_ZEN
     subroutine initMSR_PERF_CTLX_ZEN(msrr,msrw,msrwh,nth)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PERF_CTLX_ZEN
           integer(kind=int8b),  dimension(0:nth),  intent(inout) :: msrr
           integer(kind=int8b),  dimension(0:nth),  intent(inout) :: msrw
           character(len=16),    dimension(0:nth),  intent(inout) :: msrwh
           integer(kind=int4),                      intent(in)    :: nth
           ! Exec code ....
           msrr  = zero_val
           msrw  = zero_val
           msrwh = init_valh
     end subroutine initMSR_PERF_CTLX_ZEN

     subroutine AccessMSR_PERF_CTLX_ZEN(haddr,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_CTLX_ZEN
           character(len=10),              intent(in) :: haddr
           character(len=*),               intent(in) :: command
           logical(kind=int1),             intent(in) :: reset
           character(len=*),               intent(in) :: fname
        
           integer(kind=int2),             intent(inout) :: ier
           
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//haddr//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//haddr//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_PERF_CTLX_ZEN

     subroutine ReadMSR_PERF_CTLX_ZEN(msrr,nth,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PERF_CTLX_ZEN
           integer(kind=int8b),   dimension(0:nth),   intent(inout) :: msrr
           integer(kind=int4),                        intent(in)    :: nth
           integer(kind=int4),                        intent(in)    :: iounit
           character(len=*),                          intent(in)    :: fname
           integer(kind=int2),                        intent(in)    :: status
           integer(kind=int4),                        intent(inout) :: err
           character(len=256),                        intent(inout) :: ermsg
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
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msrr(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_PERF_CTLX_ZEN

     subroutine initMSR_PERF_CTRX_ZEN(sampd,msrr,nth)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PERF_CTRX_ZEN
           real(kind=dp),       dimension(0:nth),        intent(inout) :: sampd
           integer(kind=int8b), dimension(NSAMP,0:nth),  intent(inout) :: msrr
           integer(kind=int4),                           intent(in)    :: nth
           ! Exec code ....
           sampd = 0.0_dp
           msrr  = zero_val
     end subroutine initMSR_PERF_CTRX_ZEN

     subroutine AccessMSR_PERF_CTRX_ZEN(hadrr,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PERF_CTRX_ZEN
           character(len=10),              intent(in) :: haddr
           character(len=*),               intent(in) :: command
           logical(kind=int1),             intent(in) :: reset
           character(len=*),               intent(in) :: fname
        
           integer(kind=int2),             intent(inout) :: ier
           
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//haddr//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//haddr//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if  
      end subroutine AccessMSR_PERF_CTRX_ZEN

      subroutine ReadMSR_PERF_CTRX_ZEN(sampd,msrr,nth,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PERF_CTRX_ZEN
           real(kind=dp),       dimension(0:nth),       intent(inout) :: sampd
           integer(kind=int8b), dimension(NSAMP,0:nth), intent(inout) :: msrr
           integer(kind=int4),                          intent(in)    :: nth
           integer(kind=int4),                          intent(in)    :: iounit
           character(len=*),                            intent(in)    :: fname
           integer(kind=int2),                          intent(in)    :: status
           integer(kind=int4),                          intent(inout) :: err
           character(len=256),                          intent(inout) :: ermsg
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
              read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) sampd(j)
              if(ioerr >0.or.ioerr < 0) goto 9999
              do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msrr(i,j)
                 if(ioerr > 0.or.ioerr < 0) goto 9999
              end do
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_PERF_CTRX_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_L3_PMCX_ZEN
     subroutine initMSR_L3_PMCX_ZEN(sdelta,msrr)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_L3_PMCX_ZEN
           real(kind=dp),                           intent(inout) :: sdelta
           integer(kind=int8b),   dimension(NSAMP), intent(inout) :: msrr
           ! Exec code ....
           sdelta = 0.0_dp
           msrr = zero_val
      end subroutine initMSR_L3_PMCX_ZEN

      subroutine AccessMSR_L3_PMCX_ZEN(hadrr,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_L3_PMCX_ZEN
           character(len=10),              intent(in) :: haddr
           character(len=*),               intent(in) :: command
           logical(kind=int1),             intent(in) :: reset
           character(len=*),               intent(in) :: fname
           !
           integer(kind=int2),             intent(inout) :: ier
           
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//haddr//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//haddr//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if  
     end subroutine AccessMSR_L3_PMCX_ZEN

     subroutine ReadMSR_L3_PMCX_ZEN(sdelta,msrr,iounit,fname,status,err,ermsg)       
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_L3_PMCX_ZEN
           real(kind=dp),                               intent(inout) :: sdelta
           integer(kind=int8b),    dimension(NSAMP),    intent(inout) :: msrr
           integer(kind=int4),                          intent(in)    :: iounit
           character(len=*),                            intent(in)    :: fname
           integer(kind=int2),                          intent(in)    :: status
           integer(kind=int4),                          intent(inout) :: err
           character(len=256),                          intent(inout) :: ermsg
           ! Locals
           integer(kind=int4), automatic :: i,ioerr
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
!....
           read(iounit,'(F22.15)',IOMSG=ermsg,IOSTAT=ioerr) sdelta
           if(ioerr >0.or.ioerr < 0) goto 9999
           do i=1, NSAMP
                 read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) msrr(i)
                 if(ioerr > 0.or.ioerr < 0) goto 9999
           end do
             
          
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP')
     end subroutine ReadMSR_L3_PMCX_ZEN

     subroutine initMSR_CORE_ENERGY_STAT_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_CORE_ENERGY_ZEN
           type(MSR_CORE_ENERGY_STAT_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.samp_delta = 0.0_dp
           reg.msr_read   = zero_val
     end subroutine initMSR_CORE_ENERGY_STAT_ZEN

     subroutine AccessMSR_CORE_ENERGY_STAT_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_CORE_ENERGY_STAT_ZEN
           type(MSR_CORE_ENERGY_STAT_ZEN),     intent(in)    :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
          
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_CORE_ENERGY_STAT_ZEN

     subroutine ReadMSR_CORE_ENERGY_STAT_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_CORE_ENERGY_STAT_ZEN
           type(MSR_CORE_ENERGY_STAT_ZEN),       intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: ncores
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
         
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
           do j=0, ncores
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
     end subroutine ReadMSR_CORE_ENERGY_STAT_ZEN

     subroutine initMSR_PKG_ENERGY_STAT_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_PKG_ENERGY_STAT_ZEN
           type(MSR_PKG_ENERGY_STAT_ZEN),    intent(inout) :: reg
           ! Exec code ....
           reg.msr_read = zero_val
     end subroutine initMSR_PKG_ENERGY_STAT_ZEN

     subroutine AccessMSR_PKG_ENERGY_STAT_ZEN(reg,iounit,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_PKG_ENERGY_STAT_ZEN
           type(MSR_PKG_ENERGY_STAT_ZEN),        intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           !
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_PKG_ENERGY_STAT_ZEN

     subroutine ReadMSR_PKG_ENERGY_STAT_ZEN(reg,iounit,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_PKG_ENERGY_STAT_ZEN
           type(MSR_PKG_ENERGY_STAT_ZEN),             intent(inout) :: reg
           integer(kind=int4),                        intent(in)    :: iounit
           character(len=*),                          intent(in)    :: fname
           integer(kind=int2),                        intent(in)    :: status
           integer(kind=int4),                        intent(inout) :: err
           character(len=256),                        intent(inout) :: ermsg
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
           do i=0, NSAMP
                read(iounit,'(Z16.16)',IOMSG=ermsg,IOSTAT=ioerr) reg.msr_read(i)
                if(ioerr > 0 .or. ioerr < 0) goto 9999
           end do
           close(UNIT=iounit,STATUS='KEEP')
           return
9999       err = ioerr          
           close(UNIT=iounit,STATUS='KEEP') 
     end subroutine ReadMSR_PKG_ENERGY_STAT_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_CPUID_7_FEATURES_ZEN
     subroutine initMSR_CPUID_7_FEATURES_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_CPUID_7_FEATURES_ZEN
           type(MSR_CPUID_7_FEATURES_ZEN),      intent(inout) :: reg
           ! EXec code .....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_CPUID_7_FEATURES_ZEN

     subroutine AccessMSR_CPUID_7_FEATURES_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_CPUID_7_FEATURES_ZEN
           type(MSR_CPUID_7_FEATURES_ZEN),       intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
         
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
      end subroutine AccessMSR_CPUID_7_FEATURES_ZEN

      subroutine ReadMSR_CPUID_7_FEATURES_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_CPUID_7_FEATURES_ZEN
           type(MSR_CPUID_7_FEATURES_ZEN),       intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: nth
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_CPUID_7_FEATURES_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_CPUID_PWR_THERM_ZEN
     subroutine initMSR_CPUID_PWR_THERM_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_CPUID_PWR_THERM_ZEN
           type(MSR_CPUID_PWR_THERM_ZEN),     intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_CPUID_PWR_THERM_ZEN

     subroutine AccessMSR_CPUID_PWR_THERM_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_CPUID_PWR_THERM_ZEN
           type(MSR_CPUID_PWR_THERM_ZEN),        intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
       
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
      end subroutine AccessMSR_CPUID_PWR_THERM_ZEN

      subroutine ReadMSR_CPUID_PWR_THERM_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_CPUID_PWR_THERM_ZEN
           type(MSR_CPUID_PWR_THERM_ZEN),        intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: nth
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_CPUID_PWR_THERM_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_CPUID_FEATURES_ZEN
     subroutine initMSR_CPUID_FEATURES_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_CPUID_FEATURES_ZEN
           type(MSR_CPUID_FEATURES_ZEN),     intent(inout) :: reg
           ! EXec code .....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_CPUID_FEATURES_ZEN

     subroutine AccessMSR_CPUID_FEATURES_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_CPUID_FEATURES_ZEN
           type(MSR_CPUID_FEATURES_ZEN),         intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
      
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
     end subroutine AccessMSR_CPUID_FEATURES_ZEN

     subroutine ReadMSR_CPUID_FEATURES_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_CPUID_FEATURES_ZEN
           type(MSR_CPUID_FEATURES_ZEN),         intent(inout) :: reg
           integer(kind=int4),                   intent(in)    :: iounit
           integer(kind=int4),                   intent(in)    :: nth
           character(len=*),                     intent(in)    :: fname
           integer(kind=int2),                   intent(in)    :: status
           integer(kind=int4),                   intent(inout) :: err
           character(len=256),                   intent(inout) :: ermsg
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
     end subroutine ReadMSR_CPUID_FEATURES_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_CPUID_EXT_FEATURES_ZEN
     subroutine initMSR_CPUID_EXT_FEATURES_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: MSR_CPUID_EXT_FEATURES_ZEN
           type(MSR_CPUID_EXT_FEATURES_ZEN),     intent(inout) :: reg
           ! Exec code ...
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_CPUID_EXT_FEATURES_ZEN

     subroutine AccessMSR_CPUID_EXT_FEATURES_ZEN(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_CPUID_EXT_FEATURES_ZEN
           type(MSR_CPUID_EXT_FEATURES_ZEN),     intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
         
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rmsr,string)
           if(stat == -1) then
              ier = stat
           end if 
      end subroutine AccessMSR_CPUID_EXT_FEATURES_ZEN

      subroutine ReadMSR_CPUID_EXT_FEATURES_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_CPUID_EXT_FEATURES_ZEN
           type(MSR_CPUID_EXT_FEATURES_ZEN),      intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: nth
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_CPUID_EXT_FEATURES_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_IBS_FETCH_CTL_ZEN
     subroutine initMSR_IBS_FETCH_CTL_ZEN(reg)
!DIR$  ATTRIBUTES CODE_ALIGN:32 :: initMSR_IBS_FETCH_CTL_ZEN
           type(MSR_IBS_FETCH_CTL_ZEN),    intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_IBS_FETCH_CTL_ZEN

     subroutine AccessMSR_IBS_FETCH_CTL_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IBS_FETCH_CTL_ZEN
           type(MSR_IBS_FETCH_CTL_ZEN),        intent(inout) :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
      
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_IBS_FETCH_CTL_ZEN

     subroutine ReadMSR_IBS_FETCH_CTL_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IBS_FETCH_CTL_ZEN
           type(MSR_IBS_FETCH_CTL_ZEN),           intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: ncores
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_IBS_FETCH_CTL_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_IBS_FETCH_LINADDR_ZEN
     subroutine initMSR_IBS_FETCH_LINADDR_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IBS_FETCH_LINADDR_ZEN
           type(MSR_IBS_FETCH_LINADDR_ZEN),    intent(inout) :: reg
           ! Exec code .....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_IBS_FETCH_LINADDR_ZEN

     subroutine AccessMSR_IBS_FETCH_LINADDR_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IBS_FETCH_LINADDR_ZEN
           type(MSR_IBS_FETCH_LINADDR_ZEN),    intent(in)    :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
      
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_IBS_FETCH_LINADDR_ZEN

     subroutine ReadMSR_IBS_FETCH_LINADDR_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IBS_FETCH_LINADDR_ZEN
           type(MSR_IBS_FETCH_LINADDR_ZEN),       intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: ncores
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_IBS_FETCH_LINADDR_ZEN

      !DIR$ ATTRIBUTES INLINE :: initMSR_IBS_FETCH_PHYSADDR_ZEN
     subroutine initMSR_IBS_FETCH_PHYSADDR_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IBS_FETCH_PHYSADDR_ZEN
           type(MSR_IBS_FETCH_PHYSADDR_ZEN),    intent(inout) :: reg
           ! Exec code .....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
     end subroutine initMSR_IBS_FETCH_PHYSADDR_ZEN

     subroutine AccessMSR_IBS_FETCH_PHYSADDR_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IBS_FETCH_PHYSADDR_ZEN
           type(MSR_IBS_FETCH_PHYSADDR_ZEN),   intent(in)    :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
         
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_IBS_FETCH_PHYSADDR_ZEN

     subroutine ReadMSR_IBS_FETCH_PHYSADDR_ZEN(reg,iounit,ncores,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IBS_FETCH_PHYSADDR_ZEN
           type(MSR_IBS_FETCH_PHYSADDR_ZEN),      intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: ncores
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_IBS_FETCH_PHYSADDR_ZEN

     !DIR$ ATTRIBUTES INLINE :: initMSR_IBS_OP_CTL_ZEN    
     subroutine initMSR_IBS_OP_CTL_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IBS_OP_CTL_ZEN
           type(MSR_IBS_OP_CTL_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
      end subroutine initMSR_IBS_OP_CTL_ZEN

      subroutine AccessMSR_IBS_OP_CTL_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IBS_OP_CTL_ZEN
           type(MSR_IBS_OP_CTL_ZEN),           intent(inout) :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
         
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_IBS_OP_CTL_ZEN

     subroutine ReadMSR_IBS_OP_CTL_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IBS_OP_CTL_ZEN
           type(MSR_IBS_OP_CTL_ZEN),              intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: nth
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_IBS_OP_CTL_ZEN

       !DIR$ ATTRIBUTES INLINE :: initMSR_IBS_OP_RIP_ZEN    
     subroutine initMSR_IBS_OP_RIP_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IBS_OP_RIP_ZEN
           type(MSR_IBS_OP_RIP_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
      end subroutine initMSR_IBS_OP_RIP_ZEN

      subroutine AccessMSR_IBS_OP_RIP_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IBS_OP_RIP_ZEN
           type(MSR_IBS_OP_RIP_ZEN),           intent(inout) :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
      
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_IBS_OP_RIP_ZEN

     subroutine ReadMSR_IBS_OP_RIP_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IBS_OP_RIP_ZEN
           type(MSR_IBS_OP_RIP_ZEN),              intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: nth
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_IBS_OP_RIP_ZEN

       !DIR$ ATTRIBUTES INLINE :: initMSR_IBS_OP_DATA_ZEN    
     subroutine initMSR_IBS_OP_DATA_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IBS_OP_DATA_ZEN
           type(MSR_IBS_OP_DATA_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
      end subroutine initMSR_IBS_OP_DATA_ZEN

      subroutine AccessMSR_IBS_OP_DATA_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IBS_OP_DATA_ZEN
           type(MSR_IBS_OP_DATA_ZEN),          intent(inout) :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
         
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_IBS_OP_DATA_ZEN

     subroutine ReadMSR_IBS_OP_DATA_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IBS_OP_DATA_ZEN
           type(MSR_IBS_OP_DATA_ZEN),              intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: nth
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_IBS_OP_DATA_ZEN
    
!DIR$ ATTRIBUTES INLINE :: initMSR_IBS_OP_DATA2_ZEN    
     subroutine initMSR_IBS_OP_DATA2_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IBS_OP_DATA2_ZEN
           type(MSR_IBS_OP_DATA2_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
      end subroutine initMSR_IBS_OP_DATA2_ZEN

      subroutine AccessMSR_IBS_OP_DATA2_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IBS_OP_DATA2_ZEN
           type(MSR_IBS_OP_CTL_ZEN),           intent(inout) :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
        
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_IBS_OP_DATA2_ZEN

     subroutine ReadMSR_IBS_OP_DATA2_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IBS_OP_DATA2_ZEN
           type(MSR_IBS_OP_CTL_ZEN),              intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: nth
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_IBS_OP_DATA2_ZEN

!DIR$ ATTRIBUTES INLINE :: initMSR_IBS_OP_DATA3_ZEN    
     subroutine initMSR_IBS_OP_DATA3_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IBS_OP_DATA3_ZEN
           type(MSR_IBS_OP_DATA3_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
      end subroutine initMSR_IBS_OP_DATA3_ZEN

      subroutine AccessMSR_IBS_OP_DATA3_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IBS_OP_DATA3_ZEN
           type(MSR_IBS_OP_DATA3_ZEN),           intent(inout) :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
         
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_IBS_OP_DATA3_ZEN

     subroutine ReadMSR_IBS_OP_DATA3_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IBS_OP_DATA3_ZEN
           type(MSR_IBS_OP_CTL_ZEN),              intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: nth
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_IBS_OP_DATA3_ZEN
    
     !DIR$ ATTRIBUTES INLINE :: initMSR_IBS_DC_LINADDR_ZEN    
     subroutine initMSR_IBS_DC_LINADDR_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IBS_DC_LINADDR_ZEN
           type(MSR_IBS_DC_LINADDR_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
      end subroutine initMSR_IBS_DC_LINADDR_ZEN

      subroutine AccessMSR_IBS_DC_LINADDR_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IBS_DC_LINADDR_ZEN
           type(MSR_IBS_DC_LINADDR_ZEN),       intent(inout) :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
        
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_IBS_DC_LINADDR_ZEN

     subroutine ReadMSR_IBS_DC_LINADDR_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IBS_DC_LINADDR_ZEN
           type(MSR_IBS_OP_CTL_ZEN),              intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: nth
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_IBS_DC_LINADDR_ZEN

       !DIR$ ATTRIBUTES INLINE :: initMSR_IBS_DC_PHYSADDR_ZEN    
     subroutine initMSR_IBS_DC_PHYSADDR_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IBS_OP_CTL_ZEN
           type(MSR_IBS_DC_PHYSADR_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
      end subroutine initMSR_IBS_DC_PHYSADDR_ZEN

      subroutine AccessMSR_IBS_DC_PHYSADDR_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IBS_DC_PHYSADDR_ZEN
           type(MSR_IBS_DC_PHYSADDR_ZEN),      intent(inout) :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
        
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_IBS_DC_PHYSADDR_ZEN

     subroutine ReadMSR_IBS_DC_PHYSADDR_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IBS_DC_PHYSADDR_ZEN
           type(MSR_IBS_DC_PHYSADDR_ZEN),         intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: nth
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_IBS_DC_PHYSADDR_ZEN

       !DIR$ ATTRIBUTES INLINE :: initMSR_IBS_CTL_ZEN    
     subroutine initMSR_IBS_CTL_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IBS_CTL_ZEN
           type(MSR_IBS_CTL_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           
      end subroutine initMSR_IBS_CTL_ZEN

      subroutine AccessMSR_IBS_CTL_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IBS_CTL_ZEN
           type(MSR_IBS_CTL_ZEN),           intent(inout) :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
         
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_IBS_CTL_ZEN

     subroutine ReadMSR_IBS_CTL_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IBS_CTL_ZEN
           type(MSR_IBS_CTL_ZEN),                 intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: nth
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_IBS_CTL_ZEN

       !DIR$ ATTRIBUTES INLINE :: initMSR_BP_IBSTGT_RIP_ZEN    
     subroutine initMSR_BP_IBSTGT_RIP_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_BP_IBSTGT_RIP_ZEN
           type(MSR_BP_IBSTGT_RIP_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           reg.msr_write = zero_val
           reg.msrw_hex  = init_valh
      end subroutine initMSR_BP_IBSTGT_RIP_ZEN

      subroutine AccessMSR_BP_IBSTGT_RIP_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_BP_IBSTGT_RIP_ZEN
           type(MSR_BP_IBSTGT_RIP_ZEN),        intent(inout) :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
         
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_BP_IBSTGT_RIP_ZEN

     subroutine ReadMSR_BP_IBSTGT_RIP_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_BP_IBSTGT_RIP_ZEN
           type(MSR_BP_IBSTGT_RIP_ZEN),           intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: nth
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_BP_IBSTGT_RIP_ZEN
    
       !DIR$ ATTRIBUTES INLINE :: initMSR_IC_IBS_EXTD_CTL_ZEN    
     subroutine initMSR_IC_IBS_EXTD_CTL_ZEN(reg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initMSR_IC_IBS_EXTD_CTL_ZEN
           type(MSR_IBS_OP_CTL_ZEN),      intent(inout) :: reg
           ! Exec code ....
           reg.msr_read  = zero_val
           
      end subroutine initMSR_IC_IBS_EXTD_CTL_ZEN

      subroutine AccessMSR_IC_IBS_EXTD_CTL_ZEN(reg,command,reset,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessMSR_IC_IBS_EXTD_CTL_ZEN
           type(MSR_IC_IBS_EXTD_CTL_ZEN),      intent(inout) :: reg
           character(len=*),                   intent(in)    :: command
           logical(kind=int1),                 intent(in)    :: reset
           character(len=*),                   intent(in)    :: fname
          
           integer(kind=int2),                 intent(inout) :: ier
          
      
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
       
           ! Exec code ....
           if(.not. reset) then
               string = command//reg.addr_hex//fname
               stat = RUNQQ(rmsr,string)
               if(stat == -1) then
                  ier = stat
               end if
        
           else
               string = command//reg.addr_hex//reset_val
               stat = RUNQQ(wmsr,string)
               if(stat == -1) then
                   ier = stat
               end if
           end if 
     end subroutine AccessMSR_IC_IBS_EXTD_CTL_ZEN

     subroutine ReadMSR_IC_IBS_EXTD_CTL_ZEN(reg,iounit,nth,fname,status,err,ermsg)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: ReadMSR_IC_IBS_EXTD_CTL_ZEN
           type(MSR_IC_IBS_EXTD_CTL_ZEN),         intent(inout) :: reg
           integer(kind=int4),                    intent(in)    :: iounit
           integer(kind=int4),                    intent(in)    :: nth
           character(len=*),                      intent(in)    :: fname
           integer(kind=int2),                    intent(in)    :: status
           integer(kind=int4),                    intent(inout) :: err
           character(len=256),                    intent(inout) :: ermsg
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
     end subroutine ReadMSR_IC_IBS_EXTD_CTL_ZEN

     


     
end module mod_zen_msrtools_wrappers
