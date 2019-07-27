
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

    
   
     
     type, public :: ZenCPU_t
        public
        !
        character(len=32)              :: cpu_fname ! friendly name
        character(len=32)              :: cpu_cname ! hex coded name
        integer(kind=int4)             :: ncores
        integer(kind=int4)             :: nthreads
        integer(kind=int4)             :: ordinal ! n-th state sample (1...nth)
        logical(kind=int4)             :: to_screen ! output to screen, beside to file output
        !
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
        type(MSR_PROC_NAME_STRING3_ZEN)      :: proc_name3
        type(MSR_PROC_NAME_STRING4_ZEN)      :: proc_name4
        type(MSR_PROC_NAME_STRING5_ZEN)      :: proc_name5
        type(MSR_MMIO_CFG_BASE_ADDR_ZEN)     :: mmio_cfgbase
        type(MSR_INT_PENDING_ZEN)            :: int_pending
        type(MSR_TRIG_IO_CYCLE_ZEN)          :: trig_iocycle
        type(MSR_PSTATE_CUR_LIMIT_ZEN)       :: pstate_curlimit
        type(MSR_PSTATE_CTL_ZEN)             :: pstate_ctl
        type(MSR_PSTATE_STAT_ZEN)            :: pstate_stat
        type(MSR_PSTATE_DEF0_ZEN)            :: pstate_def0
        type(MSR_PSTATE_DEF1_ZEN)            :: pstate_def1
        type(MSR_PSTATE_DEF2_ZEN)            :: pstate_def2
        type(MSR_PSTATE_DEF3_ZEN)            :: pstate_def3
        type(MSR_PSTATE_DEF4_ZEN)            :: pstate_def4
        type(MSR_PSTATE_DEF5_ZEN)            :: pstate_def5
        type(MSR_PSTATE_DEF6_ZEN)            :: pstate_def6
        type(MSR_PSTATE_DEF7_ZEN)            :: pstate_def7
        type(MSR_CSTATE_BASE_ADDR_ZEN)       :: cstate_baseaddr
        type(MSR_CPU_WDT_CFG_ZEN)            :: cpu_wdt_cfg
        type(MSR_SMM_BASE_ZEN)               :: smm_base
        type(MSR_SMM_ADDR_ZEN)               :: smm_addr
        type(MSR_SMM_CTL_ZEN)                :: smm_ctl
        type(MSR_LOCAL_SMI_STAT_ZEN)         :: local_smi_stat
        type(MSR_PERF_CTL0_ZEN)              :: perf_ctl0
        type(MSR_PERF_CTL2_ZEN)              :: perf_ctl2
        type(MSR_PERF_CTL4_ZEN)              :: perf_ctl4
        type(MSR_PERF_CTL6_ZEN)              :: perf_ctl6
        type(MSR_PERF_CTL8_ZEN)              :: perf_ctl8
        type(MSR_PERF_CTL10_ZEN)             :: perf_ctl10
        type(MSR_CORE_ENERGY_STAT_ZEN)       :: core_enerstat
        type(MSR_PKG_ENERGY_STAT_ZEN)        :: pkg_enerstat
        type(MSR_CPUID_7_FEATURES_ZEN)       :: cpuid7_features
        type(MSR_CPUID_PWR_THERM_ZEN)        :: cpuid_pwrtherm
        type(MSR_CPUID_FEATURES_ZEN)         :: cpuid_features
        type(MSR_CPUID_EXT_FEATURES_ZEN)     :: cpuid_extfeat
        type(MSR_IBS_OP_DATA_ZEN)            :: ibs_opdata
        type(MSR_IBS_OP_DATA2_ZEN)           :: ibs_opdata2
        type(MSR_IBS_OP_DATA3_ZEN)           :: ibs_opdata3
        type(MSR_IBS_FETCH_CTL_ZEN)          :: ibs_fetchctl
        type(MSR_IBS_FETCH_LINADDR_ZEN)      :: ibs_fetch_laddr
        type(MSR_IBS_FETCH_PHYSADDR_ZEN)     :: ibs_fetch_phyaddr
        type(MSR_IBS_CTL_ZEN)                :: ibs_ctl
        type(MSR_BP_IBSTGT_RIP_ZEN)          :: bp_ibstgt_rip
        type(MSR_IC_IBS_EXTD_CTL_ZEN)        :: ic_ibs_extdctl ! 85 type members
     end type ZenCPU_t

     contains

       subroutine initZenCPU(zcpu,fname,cname,ncores,nthreads,    &
                             ordinal,to_screen)
                           
!DIR$  ATTRIBUTES CODE_ALIGN:32 :: initZenCPU
           type(ZenCPU_t),                                intent(inout) :: zcpu
           !
           character(len=*),                              intent(in)    :: fname
           character(len=*),                              intent(in)    :: cname
           integer(kind=int4),                            intent(in)    :: ncores
           integer(kind=int4),                            intent(in)    :: nthreads
           integer(kind=int4),                            intent(in)    :: ordinal
           logical(kind=int4),                            intent(in)    :: to_screen
           ! Exec code .....
           zcpu.fname     = fname
           zcpu.cname     = cname
           zcpu.ncores    = ncores
           zcpu.nthreads  = nthreads
           zcpu.ordinal   = ordinal
           zcpu.to_screen = to_screen
           call initMSR_APIC_BAR_ZEN(zcpu.apic_bar)
           call initMSR_MTRR_ZEN(zcpu.mtrr)
           call initMSR_MCG_CAP_ZEN(zcpu.mcg_cap)
           call initMSR_MCG_STAT_ZEN(zcpu.mcg_stat)
           call initMSR_MCG_CTL_ZEN(zcpu.mcg_ctl)
           call initMSR_DBG_CTL_ZEN(zcpu.dbg_ctl)
           call initMSR_BR_FROM_IP_ZEN(zcpu.br_fromip)
           call initMSR_BR_TO_IP_ZEN(zcpu.br_toip)
           call initMSR_LAST_EXP_FROM_IP_ZEN(zcpu.lexp_fromip)
           call initMSR_LAST_EXP_TO_IP_ZEN(zcpu.lexp_toip)
           call initMSR_MTRR_FIXED_ZEN(zcpu.mtrr_fixed)
           call initMSR_MTRR_FIXED16K_ZEN(zcpu.mtrr_fixed16k)
           call initMSR_MTRR_FIXED16K1_ZEN(zcpu.mtrr_fixed16k1)
           call initMSR_MTRR_FIXED4K_ZEN(zcpu.mtrr_fixed4k)
           call initMSR_MTRR_FIXED4K1_ZEN(zcpu.mtrr_fixed4k1)
           call initMSR_MTRR_FIXED4K2_ZEN(zcpu.mtrr_fixed4k2)
           call initMSR_MTRR_FIXED4K3_ZEN(zcpu.mtrr_fixed4k3)
           call initMSR_MTRR_FIXED4K4_ZEN(zcpu.mtrr_fixed4k4)
           call initMSR_MTRR_FIXED4K5_ZEN(zcpu.mtrr_fixed4k5)
           call initMSR_MTRR_FIXED4K6_ZEN(zcpu.mtrr_fixed4k6)
           call initMSR_MTRR_FIXED4K7_ZEN(zcpu.mtrr_fixed4k7)
           call initMSR_PAT_ZEN(zcpu.pat)
           call initMSR_MTRR_DEFTYPE_ZEN(zcpu.mtrr_deftype)
           call initMSR_EFER_ZEN(zcpu.efer)
           call initMSR_SYS_CFG_ZEN(zcpu.sys_cfg)
           call initMSR_HW_CFG_ZEN(zcpu.hw_cfg)
           call initMSR_TOP_MEM_ZEN(zcpu.top_mem)
           call initMSR_TOP_MEM2_ZEN(zcpu.top_mem2)
           call initMSR_IORR_BASE1_ZEN(zcpu.iorr_base1)
           call initMSR_IORR_BASE2_ZEN(zcpu.iorr_base2)
           call initMSR_IORR_BASE3_ZEN(zcpu.iorr_base3)
           call initMSR_IORR_MASK1_ZEN(zcpu.iorr_mask1)
           call initMSR_IORR_MASK2_ZEN(zcpu.iorr_mask2)
           call initMSR_IORR_MASK3_ZEN(zcpu.iorr_mask3)
           call initMSR_PERF_LEGACY_CTL0_ZEN(zcpu.perf_legctl0)
           call initMSR_PERF_LEGACY_CTL1_ZEN(zcpu.perf_legctl1)
           call initMSR_PERF_LEGACY_CTL2_ZEN(zcpu.perf_legctl2)
           call initMSR_PERF_LEGACY_CTL3_ZEN(zcpu.perf_legctl3)
           call initMSR_MC_EXP_REDIR_ZEN(zcpu.mc_expredir)
           call initMSR_PROC_NAME_STRING0_ZEN(zcpu.proc_name0)
           call initMSR_PROC_NAME_STRING1_ZEN(zcpu.proc_name1)
           call initMSR_PROC_NAME_STRING2_ZEN(zcpu.proc_name2)
           call initMSR_PROC_NAME_STRING3_ZEN(zcpu.proc_name3)
           call initMSR_PROC_NAME_STRING4_ZEN(zcpu.proc_name4)
           call initMSR_PROC_NAME_STRING5_ZEN(zcpu.proc_name5)
           call initMSR_MMIO_CFG_BASE_ADDR_ZEN(zcpu.mmio_cfgbase)
           call initMSR_INT_PENDING_ZEN(zcpu.int_pending)
           call initMSR_TRIG_IO_CYCLE_ZEN(zcpu.trig_iocycle)
           call initMSR_PSTATE_CUR_LIMIT_ZEN(zcpu.pstate_curlimit)
           call initMSR_PSTATE_CTL_ZEN(zcpu.pstate_ctl)
           call initMSR_PSTATE_STAT_ZEN(zcpu.pstate_stat)
           call initMSR_PSTATE_DEFX_ZEN(zcpu.pstate_def0.msr_read, &
                                        zcpu.pstate_def0.msr_write,&
                                        zcpu.pstate_def0.msrw_hex,zcpu.nthreads)
           call initMSR_PSTATE_DEFX_ZEN(zcpu.pstate_def1.msr_read, &
                                        zcpu.pstate_def1.msr_write,&
                                        zcpu.pstate_def1.msrw_hex,zcpu.nthreads)
           call initMSR_PSTATE_DEFX_ZEN(zcpu.pstate_def2.msr_read, &
                                        zcpu.pstate_def2.msr_write,&
                                        zcpu.pstate_def2.msrw_hex,zcpu.nthreads)
           call initMSR_PSTATE_DEFX_ZEN(zcpu.pstate_def3.msr_read, &
                                        zcpu.pstate_def3.msr_write,&
                                        zcpu.pstate_def3.msrw_hex,zcpu.nthreads)
           call initMSR_PSTATE_DEFX_ZEN(zcpu.pstate_def4.msr_read, &
                                        zcpu.pstate_def4.msr_write,&
                                        zcpu.pstate_def4.msrw_hex,zcpu.nthreads)
           call initMSR_PSTATE_DEFX_ZEN(zcpu.pstate_def5.msr_read, &
                                        zcpu.pstate_def5.msr_write,&
                                        zcpu.pstate_def5.msrw_hex,zcpu.nthreads)
           call initMSR_PSTATE_DEFX_ZEN(zcpu.pstate_def6.msr_read, &
                                        zcpu.pstate_def6.msr_write,&
                                        zcpu.pstate_def6.msrw_hex,zcpu.nthreads)
           call initMSR_PSTATE_DEFX_ZEN(zcpu.pstate_def7.msr_read, &
                                        zcpu.pstate_def7.msr_write,&
                                        zcpu.pstate_def7.msrw_hex,zcpu.nthreads)
           call initMSR_PERF_CTLX_ZEN(zcpu.perf_ctl0.msr_read,     &
                                      zcpu.perf_ctl0.msr_write,    &
                                      zcpu.perf_ctl0.msrw_hex,zcpu.nthreads)
           call initMSR_PERF_CTLX_ZEN(zcpu.perf_ctl2.msr_read,     &
                                      zcpu.perf_ctl2.msr_write,    &
                                      zcpu.perf_ctl2.msrw_hex,zcpu.nthreads)
           call initMSR_PERF_CTLX_ZEN(zcpu.perf_ctl4.msr_read,     &
                                      zcpu.perf_ctl4.msr_write,    &
                                      zcpu.perf_ctl4.msrw_hex,zcpu.nthreads)
           call initMSR_PERF_CTLX_ZEN(zcpu.perf_ctl6.msr_read,     &
                                      zcpu.perf_ctl6.msr_write,    &
                                      zcpu.perf_ctl6.msrw_hex,zcpu.nthreads)
           call initMSR_PERF_CTLX_ZEN(zcpu.perf_ctl8.msr_read,     &
                                      zcpu.perf_ctl8.msr_write,    &
                                      zcpu.perf_ctl8.msrw_hex,zcpu.nthreads)
           call initMSR_PERF_CTLX_ZEN(zcpu.perf_ctl10.msr_read,     &
                                      zcpu.perf_ctl10.msr_write,    &
                                      zcpu.perf_ctl10.msrw_hex,zcpu.nthreads)
           call initMSR_CSTATE_BASE_ADDRESS_ZEN(zcpu.cstate_baseaddr)
           call initMSR_CPU_WDT_CFG_ZEN(zcpu.cpu_wdt_cfg)
           call initMSR_SMM_BASE_ZEN(zcpu.smm_base)
           call initMSR_SMM_CTL_ZEN(zcpu.smm_ctl)
           call initMSR_LOCAL_SMI_STAT_ZEN(zcpu.local_smi_stat)
           call initMSR_CORE_ENERGY_STAT_ZEN(zcpu.core_enerstat)
           call initMSR_PKG_ENERGY_STAT_ZEN(zcpu.pkg_enerstat)
           call initMSR_CPUID_7_FEATURES_ZEN(zcpu.cpuid7_features)
           call initMSR_CPUID_PWR_THERM_ZEN(zcpu.pwrtherm)
           call initMSR_CPUID_FEATURES_ZEN(zcpu.cpuid_features)
           call initMSR_CPUID_EXT_FEATURES_ZEN(zcpu.cpuid_extfeat)
           call initMSR_IBS_OP_DATA_ZEN(zcpu.ibs_opdata)
           call initMSR_IBS_OP_DATA2_ZEN(zcpu.ibs_opdata2)
           call initMSR_IBS_OP_DATA3_ZEN(zcpu.ibs_opdata3)
           call initMSR_IBS_FETCH_CTL_ZEN(zcpu.ibs_fetchctl)
           call initMSR_IBS_FETCH_LINADDR_ZEN(zcpu.fetch_laddr)
           call initMSR_IBS_FETCH_PHYSADDR_ZEN(zcpu.fetch_phyaddr)
           call initMSR_IBS_CTL_ZEN(zcpu.ibs_ctl)
           call initMSR_BP_IBSTGT_RIP_ZEN(zcpu.bp_ibstgt_rip)
           call initMSR_IC_IBS_EXTD_CTL_ZEN(zcpu.ic_ibs_extdctl)
     end subroutine initZenCPU

     subroutine collectZenCaps(zcpu,commands,filenames,reset)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: collectZenCaps
           type(ZenCPU_t),                       intent(inout) :: zcpu
           character(len=62),    dimension(85),  intent(in)    :: commands
           character(len=48),    dimension(85),  intent(in)    :: filenames
           logical(kind=int1),   dimension(22),  intent(in)    :: reset
           ! Locals
           character(len=256), automatic :: ermsg
           integer(kind=int4), automatic :: err
           integer(kind=int4), automatic :: iounit
           integer(kind=int2), automatic :: ier
           ! Exec code .....
           ermsg  = " "
           err    = 0
           iounit = 100
           ier    = -2
           call AccessMSR_APIC_BAR_ZEN(zcpu.apic_bar,commands(1),filenames(1),ier)
           call ReadMSR_APIC_BAR_ZEN(zcpu.apic_bar,iounit,zcpu.nthreads,filenames(1),ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "ReadMSR_APIC_BAR_ZEN --  failed with an error:", ermsg
           end if
           ermsg  = " "
           err    = 0
           iounit = 101
           ier    = -2
           call AccessMSR_MTRR_ZEN(zcpu.mtrr,commands(2),filenames(2),ier)
           call ReadMSR_MTRR_ZEN(zcpu.mtrr,iounit,zcpu.ncores,filenames(2),ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "ReadMSR_MTRR_ZEN -- failed with an error:", ermsg
           end if
           ermsg  = " "
           err    = 0
           iounit = 102
           ier    = -2
           call AccessMSR_MCG_CAP_ZEN(zcpu.mcg_cap,commands(3),filenames(3),ier)
           call ReadMSR_MCG_CAP_ZEN(zcpu.mcg_cap,iounit,zcpu.nthreads,filenames(3),ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "ReadMSR_MCG_CAP_ZEN -- failed with an error:",ermsg)
           end if
           ermsg  = " "
           err    = 0
           iounit = 103
           ier    = -2
           call AccessMSR_MCG_STAT_ZEN(zcpu.mcg_stat,commands(4),filenames(4),ier)
           call ReadMSR_MCG_STAT_ZEN(zcpu.mcg_stat,iounit,zcpu.nthreads,filenames(4),ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "ReadMSR_MCG_STAT_ZEN -- failed with an error:",ermsg
           end if
           ermsg  = " "
           err    = 0
           iounit = 104
           ier    = -2
           call AccessMSR_MCG_CTL_ZEN(zcpu.mcg_ctl,commands(5),filenames(5),ier)
           call ReadMSR_MCG_CTL_ZEN(zcpu.mcg_ctl,iounit,zcpu.nthreads,filenames(5),ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "ReadMSR_MCG_CTL_ZEN -- failed with an error:",ermsg
           end if
           ermsg  = " "
           err    = 0
           iounit = 105
           ier    = -2
           call AccessMSR_DBG_CTL_ZEN(zcpu.dbg_ctl,commands(6),filenames(6),ier)
           call ReadMSR_DBG_CTL_ZEN(zcpu.dbg_ctl,iounit,zcpu.nthreads,filenames(6),ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "ReadMSR_DBG_CTL_ZEN -- failed with an error:",ermsg
           end if
           ermsg  = " "
           err    = 0
           iounit = 106
           ier    = -2
           call AccessMSR_BR_FROM_IP_ZEN(zcpu.br_fromip,commands(7),reset(1),filenames(7),ier)
           call ReadMSR_BR_FROM_IP_ZEN(zcpu.br_fromip,iounit,zcpu.nthreads,filenames(7),ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "ReadMSR_BR_FROM_IP_ZEN -- failed with an error:",ermsg
           end if
           ermsg  = " "
           err    = 0
           iounit = 107
           ier    = -2
           call AccessMSR_BR_TO_IP_ZEN(zcpu.br_toip,commands(8),reset(2),filenames(8),ier)
           call ReadMSR_BR_TO_IP_ZEN(zcpu.br_toip,iounit,zcpu.nthreads,filenames(8),ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "ReadMSR_BR_TO_IP_ZEN -- failed with an error:",ermsg
           end if
           ermsg  = " "
           err    = 0
           iounit = 108
           ier    = -2
           call AccessMSR_LAST_EXP_FROM_IP_ZEN(zcpu.lexp_fromip,commands(9),reset(3),filenames(9),ier)
           call ReadMSR_LAST_EXP_FROM_IP_ZEN(zcpu.lexp_fromip,iounit,zcpu.nthreads,filenames(9),ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "ReadMSR_LAST_EXP_FROM_IP_ZEN -- failed with an error:",ermsg
           end if
           ermsg  = " "
           err    = 0
           iounit = 109
           ier    = -2
           call AccessMSR_LAST_EXP_TO_IP_ZEN(zcpu.lexp_toip,commands(10),reset(4),filenames(10),ier)
           call ReadMSR_LAST_EXP_TO_IP_ZEN(zcpu.lexp_toip,iounit,zcpu.nthreads,filenames(10),ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "ReadMSR_LAST_EXP_TO_IP_ZEN -- failed with an error:",ermsg
           end if
           ermsg  = " "
           err    = 0
           iounit = 110
           ier    = -2
           call AccessMSR_MTRR_FIXED_ZEN(zcpu.mtrr_fixed,commands(11),filenames(11),ier)
           call ReadMSR_MTRR_FIXED_ZEN(zcpu.mtrr_fixed,iounit,zcpu.nthreads,filenames(11),ier,err,ermsg)
           if(err == -1 .or. err > 0) then
              print*, "ReadMSR_MTRR_FIXED_ZEN -- failed with an error:",ermsg
           end if
           ermsg  = " "
           err    = 0
           iounit = 111
           ier    = -2
           
     end subroutine collectZenCaps

end module mod_zen_caps
