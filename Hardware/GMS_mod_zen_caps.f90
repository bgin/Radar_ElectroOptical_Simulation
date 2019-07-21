
module mod_zen_caps

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         mod_zen_caps
 !          
 !          Purpose:
 !                    This module aggregates derived types data about the various Zen
 !                    CPU capabilities.
 !                   
 !                     
 !          History:
 !                        Date: 20-07-2019
 !                        Time: 10:56 GMT+2
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
     use mod_zen_msrtools_wrappers
     implicit none
     public
     ! File and module data
     !=====================================================================================================================================!
     integer(kind=int4),   parameter,  public :: MOD_ZEN_CAPS_MAJOR = 1
     integer(kind=int4),   parameter,  public :: MOD_ZEN_CAPS_MINOR = 0
     integer(kind=int4),   parameter,  public :: MOD_ZEN_CAPS_MICRO = 0
     integer(kind=int4),   parameter,  public :: MOD_ZEN_CAPS_FULLVER = 1000*MOD_ZEN_CAPS_MAJOR + &
                                                                        100*MOD_ZEN_CAPS_MINOR  + &
                                                                        10*MOD_ZEN_CAPS_MICRO
     character(*),         parameter,  public :: MOD_ZEN_CAPS_CREATION_DATE = "20-07-2019 11:28 +00200 (SAT 20 JUL 2019 GMT+2)"
     character(*),         parameter,  public :: MOD_ZEN_CAPS_BUILD_DATE    = "00-00-0000 00:00"
     character(*),         parameter,  public :: MOD_ZEN_CAPS_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     character(*),         parameter,  public :: MOD_ZEN_CAPS_SYNOPSIS      = "Aggregator data type  of the Zen CPU capabilities."

     !=======================================================================================================================================!

     !=======================================================================================================================================!
     character(*),        parameter,  private :: DESC_APIC_BAR         =  "MSR0000_001B [APIC Base Address]"
     character(*),        parameter,  private :: DESC_MTRR             =  "MSR0000_00FE [MTRR Capabilities]"
     character(*),        parameter,  private :: DESC_MCG_CAP          =  "MSR0000_0179 [Global Machine Check Capabilities]"
     character(*),        parameter,  private :: DESC_MCG_STAT         =  "MSR0000_017A [Global Machine Check Status]"
     character(*),        parameter,  private :: DESC_MCG_CTL          =  "MSR0000_017B [Global Machine Check Exception Reporting Control]"
     character(*),        parameter,  private :: DESC_DBG_CTL          =  "MSR0000_01D9 [Debug Control]"
     character(*),        parameter,  private :: DESC_BR_FROM_IP       =  "MSR0000_01DB [Last Branch From IP]"
     character(*),        parameter,  private :: DESC_BR_TO_IP         =  "MSR0000_01DC [Last Branch To IP]"
     character(*),        parameter,  private :: DESC_LAST_EXP_FROM_IP =  "MSR0000_01DD [Last Exception From IP]"
     character(*),        parameter,  private :: DESC_LAST_EXP_TO_IP   =  "MSR0000_01DE [Last Exception To IP]"
     character(*),        parameter,  private :: DESC_MTRR_FIXED       =  "MSR0000_0250 [Fixed-Size MTRR]"
     character(*),        parameter,  private :: DESC_MTRR_FIXED16K    =  "MSR0000_0250 [Fixed-Size MTRR_16K]"
     character(*),        parameter,  private :: DESC_MTRR_FIXED16K1   =  "MSR0000_0259 [Fixed-Size MTRR_16K1]"
     character(*),        parameter,  private :: DESC_MTRR_FIXED4K     =  "MSR0000_0268 [Fixed-Size MTRR_4K]"
     character(*),        parameter,  private :: DESC_MTRR_FIXED4K1    =  "MSR0000_0269 [Fixed-Size MTRR_4K1]"
     character(*),        parameter,  private :: DESC_MTRR_FIXED4K2    =  "MSR0000_026A [Fixed-Size MTRR_4K2]"
     character(*),        parameter,  private :: DESC_MTRR_FIXED4K3    =  "MSR0000_026B [Fixed-Size MTRR_4K3]"
     character(*),        parameter,  private :: DESC_MTRR_FIXED4K4    =  "MSR0000_026C [Fixed-Size MTRR_4K4]"
     character(*),        parameter,  private :: DESC_MTRR_FIXED4K5    =  "MSR0000_026D [Fixed-Size MTRR_4K5]"
     character(*),        parameter,  private :: DESC_MTRR_FIXED4K6    =  "MSR0000_026E [Fixed-Size MTRR_4K6]"
     character(*),        parameter,  private :: DESC_MTRR_FIXED4K7    =  "MSR0000_026F [Fixed-Size MTRR_4k7]"
     character(*),        parameter,  private :: DESC_PAT              =  "MSR0000_0277 [Page Attribute Table]"
     character(*),        parameter,  private :: DESC_MTRR_DEFTYPE     =  "MSR0000_02FF [MTRR Default Memory Type]"
     character(*),        parameter,  private :: DESC_EFER             =  "MSRC000_0080 [Extended Feature Enable]"
     character(*),        parameter,  private :: DESC_SYS_CFG          =  "MSRC001_0010 [System Configuration]"
     character(*),        parameter,  private :: DESC_HW_CFG           =  "MSRC001_0015 [Hardware Configuration]"
     character(*),        parameter,  private :: DESC_TOP_MEM          =  "MSRC001_001A [Top Of Memory]"
     character(*),        parameter,  private :: DESC_TOP_MEM2         =  "MSRC001_001D [Top Of Memory 2]"
     character(*),        parameter,  private :: DESC_IORR_BASE1       =  "MSRC001_001[6] [IO Range Base]"
     character(*),        parameter,  private :: DESC_IORR_BASE2       =  "MSRC001_001[7] [IO Range Base]"
     character(*),        parameter,  private :: DESC_IORR_BASE3       =  "MSRC001_001[8] [IO Range Base]"
     character(*),        parameter,  private :: DESC_IORR_MASK1       =  "MSRC001_001[7] [IO Range Mask]"
     character(*),        parameter,  private :: DESC_IORR_MASK2       =  "MSRC001_001[8] [IO Range Mask]"
     character(*),        parameter,  private :: DESC_IORR_MASK3       =  "MSRC001_001[9] [IO Range Mask]"
     character(*),        parameter,  private :: DESC_PERF_LEGACY_CTL0 =  "MSRC001_000[0...3] [Performance Event Select [0]]"
     character(*),        parameter,  private :: DESC_PERF_LEGACY_CTL1 =  "MSRC001_000[0...3] [Performance Event Select [1]]"
     character(*),        parameter,  private :: DESC_PERF_LEGACY_CTL2 =  "MSRC001_000[0...3] [Performance Event Select [2]]"
     character(*),        parameter,  private :: DESC_PERF_LEGACY_CTL3  =  "MSRC001_000[0...3] [Performance Event Select [3]]"
     character(*),        parameter,  private :: DESC_MX_EXP_REDIR      =  "MSRC001_0022 [Machine Check Exception Redirection]"
     character(*),        parameter,  private :: DESC_PROC_NAME_STRING0 = "MSRC001_003[0] [Processor Name String0]"
     character(*),        parameter,  private :: DESC_PROC_NAME_STRING1 = "MSRC001_003[1] [Processor Name String1]"
     character(*),        parameter,  private :: DESC_PROC_NAME_STRING2 = "MSRC001_003[2] [Processor Name String2]"
     character(*),        parameter,  private :: DESC_PROC_NAME_STRING3 = "MSRC001_003[3] [Processor Name String3]"
     character(*),        parameter,  private :: DESC_PROC_NAME_STRING4 = "MSRC001_003[4] [Processor Name String4]"
     character(*),        parameter,  private :: DESC_PROC_NAME_STRING5 = "MSRC001_003[5] [Processor Name String5]"
     character(*),        parameter,  private :: DESC_MMIO_CFG_BADDR    = "MSRC001_0058 [MMIO Configuration Base Address]"
     character(*),        parameter,  private :: DESC_INT_PENDING       = "MSRC001_0055 [Reserved.]"
     character(*),        parameter,  private :: DESC_TRIG_IO_CYCLE     = "MSRC001_0056 [SMI Trigger IO Cycle]"
     character(*),        parameter,  private :: DESC_PSTATE_CUR_LIMIT  = "MSRC001_0061 [P-state Current Limit]"
     character(*),        parameter,  private :: DESC_PSTATE_CTL        = "MSRC001_0062 [P-state Control]"
     character(*),        parameter,  private :: DESC_PSTATE_STAT       = "MSRC001_0063 [P-state Status]"
     character(*),        parameter,  private :: DESC_PSTATE_DEF0       = "MSRC001_006[4] [P-state [7:0]]"
     character(*),        parameter,  private :: DESC_PSTATE_DEF1       = "MSRC001_006[5] [P-state [7:0]]"
     character(*),        parameter,  private :: DESC_PSTATE_DEF2       = "MSRC001_006[6] [P-state [7:0]]"
     character(*),        parameter,  private :: DESC_PSTATE_DEF3       = "MSRC001_006[7] [P-state [7:0]]"
     character(*),        parameter,  private :: DESC_PSTATE_DEF4       = "MSRC001_006[8] [P-state [7:0]]"
     character(*),        parameter,  private :: DESC_PSTATE_DEF5       = "MSRC001_006[9] [P-state [7:0]]"
     character(*),        parameter,  private :: DESC_PSTATE_DEF6       = "MSRC001_006[A] [P-state [7:0]]"
     character(*),        parameter,  private :: DESC_PSTATE_DEF7       = "MSRC001_006[B] [P-state [7:0]]"
     character(*),        parameter,  private :: DESC_CSTATE_BADDR      = "MSRC001_0073 [C-state Base Address]"
     character(*),        parameter,  private :: DESC_CPU_WDT_CFG       = "MSRC001_0074 [CPU Watchdog Timer]"
     character(*),        parameter,  private :: DESC_SMM_BASE          = "MSRC001_0111 [SMM Base Address]"
     character(*),        parameter,  private :: DESC_SMM_ADDR          = "MSRC001_0112 [SMM TSeg Base Address]"
     character(*),        parameter,  private :: DESC_SMM_MASK          = "MSRC001_0113 [SMM TSeg Mask]"
     character(*),        parameter,  private :: DESC_SMM_CTL           = "MSRC001_0116 [SMM Control]"
     character(*),        parameter,  private :: DESC_LOCAL_SMI_STAT    = "MSRC001_011A [Local SMI Status]"
     character(*),        parameter,  private :: DESC_PERF_CTL0         = "MSRC001_020[0] [Performance Event Select [5:0]]"
     character(*),        parameter,  private :: DESC_PERF_CTL2         = "MSRC001_020[2] [Performance Event Select [5:0]]"
     character(*),        parameter,  private :: DESC_PERF_CTL4         = "MSRC001_020[4] [Performance Event Select [5:0]]"
     character(*),        parameter,  private :: DESC_PERF_CTL6         = "MSRC001_020[6] [Performance Event Select [5:0]]"
     character(*),        parameter,  private :: DESC_PERF_CTL8         = "MSRC001_020[8] [Performance Event Select [5:0]]"
     character(*),        parameter,  private :: DESC_PERF_CTL10        = "MSRC001_020[A] [Performance Event Select [5:0]]"
     character(*),        parameter,  private :: DESC_L3_PMC_CFG0       = "MSRC001_023[0] [L3 Performance Event Select [5:0]]"
     character(*),        parameter,  private :: DESC_L3_PMC_CFG2       = "MSRC001_023[2] [L3 Performance Event Select [5:0]]"
     character(*),        parameter,  private :: DESC_L3_PMC_CFG4       = "MSRC001_023[4] [L3 Performance Event Select [5:0]]"
     character(*),        parameter,  private :: DESC_L3_PMC_CFG6       = "MSRC001_023[6] [L3 Performance Event Select [5:0]]"
     character(*),        parameter,  private :: DESC_L3_PMC_CFG8       = "MSRC001_023[8] [L3 Performance Event Select [5:0]]"
     character(*),        parameter,  private :: DESC_L3_PMC_CFG10      = "MSRC001_023[A] [L3 Performance Event Select [5:0]]"
     character(*),        parameter,  private :: DESC_RAPL_UNIT         = "MSRC001_0299 [RAPL Power Unit]"
     character(*),        parameter,  private :: DESC_CPUID_FEAT7       = "MSRC001_1002 [CPUID Features for CPUID Fn00000007_E[A,B]X]"
     character(*),        parameter,  private :: DESC_CPUID_PWR_THERM   = "MSRC001_1003 [Thermal and Power Management CPUID Features]"
     
     !=======================================================================================================================================!
     
     type, public :: ZenCPU_t
        public
        character(len=128)             :: filename
        character(len=32)              :: fname ! friendly name
        character(len=32)              :: cname ! hex coded name
        integer(kind=int4)             :: ncores
        integer(kind=int4)             :: nthreads
        integer(kind=int4)             :: ordinal ! n-th state sample (1...nth)
        logical(kind=int4)             :: to_screen ! output to screen, beside to file output
        type(MSR_APIC_BAR_ZEN)         :: apic_bar
        type(MSR_MTRR_ZEN)             :: mtrr
        type(MSR_MCG_CAP_ZEN)          :: mcg_cap
        type(MSR_MCG_STAT_ZEN)         :: mcg_stat
        type(MSR_MCG_CTL_ZEN)          :: mcg_ctl
        type(MSR_DBG_CTL_ZEN)          :: dbg_ctl
        type(MSR_BR_FROM_IP_ZEN)       :: br_fromip
        type(MSR_BR_TO_IP_ZEN)         :: br_toip
        type(MSR_LAST_EXP_FROM_IP_ZEN) :: lexp_fromip
        type(MSR_LAST_EXP_TO_IP_ZEN)   :: lexp_toip
        type(MSR_MTRR_FIXED_ZEN)       :: mtrr_fixed
        type(MSR_MTRR_FIXED16K_ZEN)    :: mtrr_fixed16k
        type(MSR_MTRR_FIXED16K1_ZEN)   :: mtrr_fixed16k1
        type(MSR_MTRR_FIXED4K_ZEN)     :: mtrr_fixed4k
        type(MSR_MTRR_FIXED4K1_ZEN)    :: mtrr_fixed4k1
        type(MSR_MTRR_FIXED4K3_ZEN)    :: mtrr_fixed4k3
        type(MSR_MTRR_FIXED4K4_ZEN)    :: mtrr_fixed4k4
        type(MSR_MTRR_FIXED4K5_ZEN)    :: mtrr_fixed4k5
        type(MSR_MTRR_FIXED4K6_ZEN)    :: mtrr_fixed4k6
        type(MSR_MTRR_FIXED4K7_ZEN)    :: mtrr_fixed4k7
        type(MSR_PAT_ZEN)              :: pat
        type(MSR_MTRR_DEFTYPE_ZEN)     :: mtrr_deftype
        type(MSR_EFER_ZEN)             :: efer
        type(MSR_SYS_CFG_ZEN)          :: sys_cfg
        type(MSR_HW_CFG_ZEN)           :: hw_cfg
        type(MSR_TOP_MEM_ZEN)          :: top_mem
        type(MSR_TOP_MEM2_ZEN)         :: top_mem2
        type(MSR_IORR_BASE1_ZEN)       :: iorr_base1
        type(MSR_IORR_BASE2_ZEN)       :: iorr_base2
        type(MSR_IORR_BASE3_ZEN)       :: iorr_base3
        type(MSR_IORR_MASK1_ZEN)       :: iorr_mask1
        type(MSR_IORR_MASK2_ZEN)       :: iorr_mask2
        type(MSR_IORR_MASK3_ZEN)        :: iorr_mask3
        type(MSR_PERF_LEGACY_CTL0_ZEN)  :: perf_legctl0
        type(MSR_PERF_LEGACY_CTL1_ZEN)  :: perf_legctl1
        type(MSR_PERF_LEGACY_CTL2_ZEN)  :: perf_legctl2
        type(MSR_PERF_LEGACY_CTL3_ZEN)  :: perf_legctl3
        type(MSR_MC_EXP_REDIR_ZEN)      :: mc_expredir
        type(MSR_PROC_NAME_STRING0_ZEN) :: proc_name0
        type(MSR_PROC_NAME_STRING1_ZEN) :: proc_name1
        type(MSR_PROC_NAME_STRING2_ZEN) :: proc_name2
        type(MSR_PROC_NAME_STRING3_ZEN) :: proc_name3
        type(MSR_PROC_NAME_STRING4_ZEN) :: proc_name4
        type(MSR_PROC_NAME_STRING5_ZEN) :: proc_name5
        type(MSR_MMIO_CFG_BASE_ADDR_ZEN) :: mmio_cfgbase
        type(MSR_INT_PENDING_ZEN)        :: int_pending
        type(MSR_TRIG_IO_CYCLE_ZEN)      :: trig_iocycle
        
     end type ZenCPU_t

     

end module mod_zen_caps
