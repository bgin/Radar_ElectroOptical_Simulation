

module mod_zen_msr_perf

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         mod_zen_msr_perf
 !          
 !          Purpose:
 !                    This module aggregates derived types representing  various Zen
 !                    performance measurement MSRs
 !                   
 !                     
 !          History:
 !                        Date: 03-08-2019
 !                        Time: 10:11 GMT+2
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

     use mod_kinds, only : int1, int2, int4, int8b, dp
     use mod_zen_msr
     use mod_zen_msrtools_wrapper
     implicit none
     public

     ! File and module metadata
     !==================================================================================================================120
       integer(kind=int4),   parameter, public :: MOD_ZEN_MSR_PERF_MAJOR = 1
       integer(kind=int4),   parameter, public :: MOD_ZEN_MSR_PERF_MINOR = 0
       integer(kind=int4),   parameter, public :: MOD_ZEN_MSR_PERF_MICRO = 0
       integer(kind=int4),   parameter, public :: MOD_ZEN_MSR_PERF_FULLVER = 1000*MOD_ZEN_MSR_PERF_MAJOR + &
                                                                             100*MOD_ZEN_MSR_PERF_MINOR  + &
                                                                             10*MOD_ZEN_MSR_PERF_MICRO
       character(*),         parameter, public :: MOD_ZEN_MSR_PERF_CREATION_DATE = "03-08-2019 10:22 +00200 (SAT 03 AUG 2019 GMT+2)"
       character(*),         parameter, public :: MOD_ZEN_MSR_PERF_BUILD_DATE    = "00-00-0000 00:00"
       character(*),         parameter, public :: MOD_ZEN_MSR_PERF_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
       character(*),         parameter, public :: MOD_ZEN_MSR_PERF_SYNOPSIS      = "Aggregating data type for the peformance MSRs"
      !==================================================================================================================120

     integer(kind=int8b),   parameter, private :: rfail = -1  

     type, public :: ZenPerfMSR_t
        public
        character(len=32)              :: cpu_fname ! friendly name
        character(len=32)              :: cpu_cname ! hex coded name
        integer(kind=int4)             :: ncores
        integer(kind=int4)             :: nthreads
        integer(kind=int4)             :: ordinal ! n-th state sample (1...nth)
        logical(kind=int4)             :: to_screen ! output to screen, beside to file output
        type(MSR_TSC_ZEN)              :: tsc
        type(MSR_MPERF_ZEN)            :: mperf
        type(MSR_APERF_ZEN)            :: aperf
        type(MSR_MPERF_READONLY_ZEN)   :: mperf_ro
        type(MSR_APERF_READONLY_ZEN)   :: aperf_ro
        type(MSR_IRPERF_COUNT_ZEN)     :: irperf_count
        type(MSR_TSC_AUX_ZEN)          :: tsc_aux
        type(MSR_TSC_RATIO_ZEN)        :: tsc_ratio
        type(MSR_PERF_LEGACY_CTR0_ZEN) :: perf_lctr0
        type(MSR_PERF_LEGACY_CTR1_ZEN) :: perf_lctr1
        type(MSR_PERF_LEGACY_CTR2_ZEN) :: perf_lctr2
        type(MSR_PERF_LEGACY_CTR3_ZEN) :: perf_lctr3
        type(MSR_PERF_CTR0_ZEN)        :: perf_ctr0
        type(MSR_PERF_CTR2_ZEN)        :: perf_ctr2
        type(MSR_PERF_CTR4_ZEN)        :: perf_ctr4
        type(MSR_PERF_CTR6_ZEN)        :: perf_ctr6
        type(MSR_PERF_CTR8_ZEN)        :: perf_ctr8
        type(MSR_PERF_CTR10_ZEN)       :: perf_ctr10
        type(MSR_CORE_ENERGY_STAT_ZEN) :: core_enerstat
        type(MSR_PKG_ENERGY_STAT_ZEN)  :: pkg_enerstat
     end type ZenPerfMSR_t

     contains

     subroutine initZenPerfMSR(zcpu,fname,cname,ncores,nthreads,ordinal,to_screen)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: initZenPerfMSR
           type(ZenPerfMSR_t),       intent(inout) :: zcpu
           character(len=*),         intent(in)    :: fname
           character(len=*),         intent(in)    :: cname
           integer(kind=int4),       intent(in)    :: ncores
           integer(kind=int4),       intent(in)    :: nthreads
           integer(kind=int4),       intent(in)    :: ordinal
           logical(kind=int4),       intent(in)    :: to_screen
           ! Exec code .....
           zcpu.fname     = fname
           zcpu.cname     = cname
           zcpu.ncores    = ncores
           zcpu.nthreads  = nthreads
           zcpu.ordinal   = ordinal
           zcpu.to_screen = to_screen
           call initMSR_TSC_ZEN(zcpu.tsc)
           call initMSR_MPERF_ZEN(zcpu.mperf)
           call initMSR_APERF_ZEN(zcpu.aperf)
           call initMSR_MPERF_READONLY_ZEN(zcpu.mperf_ro)
           call initMSR_APERF_READONLY_ZEN(zcpu.aperf_ro)
           call initMSR_IRPERF_COUNT_ZEN(zcpu.irperf_count)
           call initMSR_TSC_AUX_ZEN(zcpu.tsc_aux)
           call initMSR_TSC_RATIO_ZEN(zcpu.tsc_ratio)
           call initMSR_PERF_LEGACY_CTR0_ZEN(zcpu.perf_lctr0)
           call initMSR_PERF_LEGACY_CTR1_ZEN(zcpu.perf_lctr1)
           call initMSR_PERF_LEGACY_CTR2_ZEN(zcpu.perf_lctr2)
           call initMSR_PERF_LEGACY_CTR3_ZEN(zcpu.perf_lctr3)
           call initMSR_PERF_CTRX_ZEN(perf_ctr0.samp_delta,perf_ctr0.msr_read, &
                                      perf_ctr0.nthreads)
           call initMSR_PERF_CTRX_ZEN(perf_ctr2.samp_delta,perf_ctr2.msr_read, &
                                      perf_ctr2.nthreads)
           call initMSR_PERF_CTRX_ZEN(perf_ctr4.samp_delta,perf_ctr4.msr_read, &
                                      perf_ctr4.nthreads)
           call initMSR_PERF_CTRX_ZEN(perf_ctr6.samp_delta,perf_ctr6.msr_read, &
                                      perf_ctr6.nthreads)
           call initMSR_PERF_CTRX_ZEN(perf_ctr8.samp_delta,perf_ctr8.msr_read, &
                                      perf_ctr8.nthreads)
           call initMSR_PERF_CTRX_ZEN(perf_ctr10.samp_delta,perf_ctr10.msr_read, &
                                      perf_ctr10.nthreads)
           call initMSR_CORE_ENERGY_STAT_ZEN(zcpu.core_enerstat)
           call initMSR_PKG_ENERGY_STAT_ZEN(zcpu.pkg_enerstat)
     end subroutine initZenPerfMSR

     subroutine collectMSR_TSC(zcpu,command,filename,reset,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_TSC
           type(ZenPerfMSR_t),         intent(inout) :: zcpu
           character(len=64),          intent(in)    :: command
           character(len=48),          intent(in)    :: filename
           logical(kind=int4),         intent(in)    :: reset
           integer(kind=int4),         intent(inout) :: nfails
           logical(kind=int1),         intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 200
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_TSC_ZEN(zcpu.tsc,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1 ! inform about the number of failed calls to rmsr
              end if
           end do
           rfail = .false.
           call ReadMSR_TSC_ZEN(zcpu.tsc,iounit,zcpu.nthreads,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_TSC: ReadMSR_TSC_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
           end if
     end subroutine collectMSR_TSC

     subroutine collectMSR_MPER(zcpu,command,filename,reset,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_MPERF
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 201
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_MPERF_ZEN(zcpu.mperf,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_MPERF_ZEN(zcpu.mperf,iounit,zcpu.nthreads,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_MPERF: ReadMSR_MPERF_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
           end if
     end subroutine collectMSR_MPER

     subroutine collectMSR_APERF(zcpu,command,filename,reset,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_APERF
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 202
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_APERF_ZEN(zcpu.aperf,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_APERF_ZEN(zcpu.aperf,iounit,zcpu.nthreads,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_APERF: ReadMSR_MPERF_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
           end if
     end subroutine collectMSR_APERF

     subroutine collectMSR_MPERF_RO(zcpu,command,filename,reset,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_MPERF_RO
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 203
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_MPERF_READONLY_ZEN(zcpu.mperf_ro,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_MPERF_READONLY_ZEN(zcpu.mperf_ro,iounit,zcpu.nthreads,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_MPERF_RO: ReadMSR_MPERF_READONLY_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
           end if
     end subroutine collectMSR_MPERF_RO

     subroutine collectMSR_APERF_RO(zcpu,command,filename,reset,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_APERF_RO
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 204
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_APERF_READONLY_ZEN(zcpu.aperf_ro,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_APERF_READONLY_ZEN(zcpu.aperf_ro,iounit,zcpu.nthreads,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_APERF_RO: ReadMSR_APERF_READONLY_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
           end if
     end subroutine collectMSR_APERF_RO
     
     subroutine collectMSR_IRPERF_COUNT(zcpu,command,filename,reset,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_IRPERF_COUNT
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 205
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_IRPERF_COUNT_ZEN(zcpu.irperf_count,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_IRPERF_COUNT_ZEN(zcpu.irperf_count,iounit,zcpu.nthreads,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_IRPERF_COUNT: ReadMSR_IRPERF_COUNT_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
           end if
     end subroutine collectMSR_IRPERF_COUNT

     subroutine collectMSR_TSC_AUX(zcpu,command,filename,reset,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_TSC_AUX
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 206
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_TSC_AUX_ZEN(zcpu.tsc_aux,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_TSC_AUX_ZEN(zcpu.tsc_aux,iounit,zcpu.nthreads,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_TSC_AUX: ReadMSR_TSC_AUX_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
           end if
     end subroutine collectMSR_TSC_AUX

     subroutine collectMSR_TSC_RATIO(zcpu,command,filename,reset,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_TSC_RATIO
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 207
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_TSC_RATIO_ZEN(zcpu.tsc_ratio,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_TSC_RATIO_ZEN(zcpu.zcpu.tsc_ratio,iounit,zcpu.nthreads,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_TSC_RATIO: ReadMSR_TSC_RATIO_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
           end if
     end subroutine collectMSR_TSC_RATIO

     subroutine collectMSR_PERF_LCTR0(zcpu,command,filename,reset,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_PERF_LCTR0
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 208
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_PERF_LEGACY_CTR0_ZEN(zcpu.perf_lctr0,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_PERF_LEGACY_CTR0_ZEN(zcpu.perf_lctr0,iounit,zcpu.nthreads,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_PERF_LCTR0: ReadMSR_PERF_LEGACY_CTR0_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
           end if
     end subroutine collectMSR_PERF_LCTR0

     subroutine collectMSR_PERF_LCTR1(zcpu,command,filename,reset,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_PERF_LCTR1
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 209
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_PERF_LEGACY_CTR1_ZEN(zcpu.perf_lctr1,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_PERF_LEGACY_CTR1_ZEN(zcpu.perf_lctr1,iounit,zcpu.nthreads,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_PERF_LCTR1: ReadMSR_PERF_LEGACY_CTR1_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
           end if
     end subroutine collectMSR_PERF_LCTR1

     subroutine collectMSR_PERF_LCTR2(zcpu,command,filename,reset,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_PERF_LCTR2
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 210
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_PERF_LEGACY_CTR2_ZEN(zcpu.perf_lctr2,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_PERF_LEGACY_CTR2_ZEN(zcpu.perf_lctr2,iounit,zcpu.nthreads,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_PERF_LCTR2: ReadMSR_PERF_LEGACY_CTR2_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
           end if
     end subroutine collectMSR_PERF_LCTR2
    
     subroutine collectMSR_PERF_LCTR3(zcpu,command,filename,reset,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_PERF_LCTR3
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 211
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_PERF_LEGACY_CTR3_ZEN(zcpu.perf_lctr3,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_PERF_LEGACY_CTR3_ZEN(zcpu.perf_lctr3,iounit,zcpu.nthreads,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_PERF_LCTR3: ReadMSR_PERF_LEGACY_CTR3_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
           end if
     end subroutine collectMSR_PERF_LCTR3

     subroutine collectMSR_PERF_CTR0(zcpu,command,reset,filename,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_PERF_CTR0
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 213
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_PERF_CTRX_ZEN(zcpu.perf_ctr0.addr_hex,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_PERF_CTRX_ZEN(zcpu.perf_ctr0.samp_delta,    &
                                      zcpu.perf_ctr0.msr_read,      &
                                      zcpu.nthreads,                &
                                      iounit,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_PERF_CTR0: ReadMSR_PERF_CTRX_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
          end if 
     end subroutine collectMSR_PERF_CTR0

     subroutine collectMSR_PERF_CTR2(zcpu,command,reset,filename,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_PERF_CTR2
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 214
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_PERF_CTRX_ZEN(zcpu.perf_ctr2.addr_hex,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_PERF_CTRX_ZEN(zcpu.perf_ctr2.samp_delta,    &
                                      zcpu.perf_ctr2.msr_read,      &
                                      zcpu.nthreads,                &
                                      iounit,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_PERF_CTR2: ReadMSR_PERF_CTRX_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
          end if 
     end subroutine collectMSR_PERF_CTR2

     subroutine collectMSR_PERF_CTR4(zcpu,command,reset,filename,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_PERF_CTR4
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 215
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_PERF_CTRX_ZEN(zcpu.perf_ctr4.addr_hex,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_PERF_CTRX_ZEN(zcpu.perf_ctr4.samp_delta,    &
                                      zcpu.perf_ctr4.msr_read,      &
                                      zcpu.nthreads,                &
                                      iounit,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_PERF_CTR4: ReadMSR_PERF_CTRX_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
          end if 
     end subroutine collectMSR_PERF_CTR4

     subroutine collectMSR_PERF_CTR6(zcpu,command,reset,filename,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_PERF_CTR6
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 216
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_PERF_CTRX_ZEN(zcpu.perf_ctr6.addr_hex,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_PERF_CTRX_ZEN(zcpu.perf_ctr6.samp_delta,    &
                                      zcpu.perf_ctr6.msr_read,      &
                                      zcpu.nthreads,                &
                                      iounit,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_PERF_CTR6: ReadMSR_PERF_CTRX_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
          end if 
     end subroutine collectMSR_PERF_CTR6

     subroutine collectMSR_PERF_CTR8(zcpu,command,reset,filename,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_PERF_CTR8
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 217
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_PERF_CTRX_ZEN(zcpu.perf_ctr8.addr_hex,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_PERF_CTRX_ZEN(zcpu.perf_ctr8.samp_delta,    &
                                      zcpu.perf_ctr8.msr_read,      &
                                      zcpu.nthreads,                &
                                      iounit,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_PERF_CTR8: ReadMSR_PERF_CTRX_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
          end if 
     end subroutine collectMSR_PERF_CTR8

     subroutine collectMSR_PERF_CTR10(zcpu,command,reset,filename,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_PERF_CTR10
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 218
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_PERF_CTRX_ZEN(zcpu.perf_ctr10.addr_hex,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_PERF_CTRX_ZEN(zcpu.perf_ctr10.samp_delta,    &
                                      zcpu.perf_ctr10.msr_read,      &
                                      zcpu.nthreads,                &
                                      iounit,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_PERF_CTR10: ReadMSR_PERF_CTRX_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
          end if 
     end subroutine collectMSR_PERF_CTR10

     subroutine collectMSR_CORE_ENERGY_STAT(zcpu,command,reset,filename,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_CORE_ENERGY_STAT
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           logical(kind=int4),            intent(in)    :: reset
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 219
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_CORE_ENERGY_STAT_ZEN(zcpu.core_enerstat,command,reset,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_CORE_ENERGY_STAT_ZEN(zcpu.core_enerstat,iounit,zcpu.ncores,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_CORE_ENERGY_STAT: ReadMSR_CORE_ENERGY_STAT_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
          end if 
     end subroutine collectMSR_CORE_ENERGY_STAT

     subroutine collectMSR_PKG_ENERGY_STAT(zcpu,command,filename,nfails,rfail)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectMSR_PKG_ENERGY_STAT
           type(ZenPerfMSR_t),            intent(inout) :: zcpu
           character(len=64),             intent(in)    :: command
           character(len=48),             intent(in)    :: filename
           !
           integer(kind=int4),            intent(inout) :: nfails
           logical(kind=int1),            intent(inout) :: rfail
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: sample
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code ....
           ermsg    = " "
           err      = 0
           iounit   = 220
           ier      = -2
           nfails   = 0
           do sample = 1, NSAMP
              call AccessMSR_PKG_ENERGY_STAT_ZEN(zcpu.pkg_enerstat,command,filename,ier)
              if(-1 == ier) then
                 nfails = nfails + 1
              end if
           end do
           rfail = .false.
           call ReadMSR_PKG_ENERGY_STAT_ZEN(zcpu.pkg_enerstat,iounit,filename,ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "collectMSR_PKG_ENERGY_STAT: ReadMSR_PKG_ENERGY_STAT_ZEN -- failed with an error:",ermsg
              rfail = .true.
              !
          end if 
     end subroutine collectMSR_PKG_ENERGY_STAT


end module mod_zen_msr_perf
