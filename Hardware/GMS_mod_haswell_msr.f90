
module mod_haswell_msr


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         mod_haswell_msr
 !          
 !          Purpose:
  !                   This module contains a derived data types
  !                    describing Haswell CPU-architecture MSR registers.
  !                   CPUID: DisplayFamily_DisplayModel signature 06_3CH/06_45H/06_46H
 !                   
 !                     
 !          History:
 !                        Date: 29-05-2019
 !                        Time: 18:16 GMT+2
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

     use mod_kinds, only : int4, int8b
     implicit none

     ! Haswell  CPUID: -- 06_3CH/06_45H/06_46H

     type, public :: MSR_IA32_TSC_ADJUST_HASWELL
        public
        integer(kind=int4)    :: addr_dec = 59
        character(len=4)      :: addr_hex = "0x3B"
        character(len=16)     :: msr_name = "IA32_TSC_ADJUST"
        integer(kind=int8b), dimension(0:7)  msr_read
        integer(kind=int8b), dimension(0:7) :: msr_write
        character(len=16),   dimension(0:7) :: msrw_hex
        ! Per-Logical-Processor TSC ADJUST (R/W)
     end type MSR_IA32_TSC_ADJUST_HASWELL

     type, public :: MSR_PLATFORM_INFO_HASWELL
        public
        integer(kind=int4)     :: addr_dec = 206
        character(len=4)       :: addr_hex = "0xCE"
        integer(kind=int8b), dimension(0:1) :: msr_read
        character(len=17)      :: msr_name = "MSR_PLATFORM_INFO"
        ! MSR_PLATFORM_INFO
        ! Package
        ! Platform Information
     end type MSR_PLATFORM_INFO_HASWELL

     type, public :: MSR_IA32_PERFEVTSEL0_HASWELL
        public
        integer(kind=int4)     :: addr_dec = 390
        character(len=5)       :: addr_hex = "0x186"
        character(len=16)      :: msr_name = "IA32_PERFEVTSEL0"
        integer(kind=int4), dimension(0:7) :: msr_read
        integer(kind=int4), dimension(0:7) :: msr_write
        character(len=8),   dimension(0:7) :: msrw_hex
        ! Thread
        ! Performance Event Select for Counter 0 (R/W)
     end type MSR_IA32_PERFEVTSEL0_HASWELL

     type, public :: MSR_IA32_PERFEVTSEL1_HASWELL
        public
        integer(kind=int4)     :: addr_dec = 391
        character(len=5)       :: addr_hex = "0x187"
        character(len=16)      :: msr_name = "IA32_PERFEVTSEL1"
        integer(kind=int4), dimension(0:7) :: msr_read
        integer(kind=int4), dimension(0:7) :: msr_write
        character(len=8),   dimension(0:7) :: msrw_hex
        ! Thread
        ! Performance Event Select for Counter 1 (R/W)
     end type MSR_IA32_PERFEVTSEL1_HASWELL

     type, public :: MSR_IA32_PERFEVTSEL2_HASWELL
        public
        integer(kind=int4)     :: addr_dec = 392
        character(len=5)       :: addr_hex = "0x188"
        character(len=16)      :: msr_name = "IA32_PERFEVTSEL2"
        integer(kind=int4), dimension(0:55) :: msr_read
        integer(kind=int4), dimension(0:55) :: msr_write
        character(len=8),   dimension(0:55) :: msrw_hex
        ! Thread
        ! Performance Event Select for Counter 2 (R/W)
     end type MSR_IA32_PERFEVTSEL2_HASWELL

     type, public :: MSR_IA32_PERFEVTSEL3_HASWELL
        public
        integer(kind=int4)     :: addr_dec = 393
        character(len=5)       :: addr_hex = "0x189"
        character(len=16)      :: msr_name = "IA32_PERFEVTSEL3"
        integer(kind=int4), dimension(0:7) :: msr_read
        integer(kind=int4), dimension(0:7) :: msr_write
        character(len=8),   dimension(0:7) :: msrw_hex
        ! Thread
        ! Performance Event Select for Counter 3 (R/W)
     end type MSR_IA32_PERFEVTSEL3_HASWELL

     type, public :: MSR_LBR_SELECT_HASWELL
        public
        integer(kind=int4)      :: addr_dec = 456
        character(len=5)        :: addr_hex = "0x1C8"
        character(len=14)       :: msr_name = "MSR_LBR_SELECT"
        integer(kind=int8b), dimension(0:7) :: msr_read
        integer(kind=int8b), dimension(0:7) :: msr_write
        character(len=16),   dimension(0:7) :: msrw_hex
        ! Last Branch Record Filtering Select Register (R/W)
     end type MSR_LBR_SELECT_HASWELL

     type, public :: MSR_IA32_DEBUGCTL_HASWELL
        public
        integer(kind=int4)      :: addr_dec = 473
        character(len=5)        :: addr_hex = "0x1D9"
        character(len=13)       :: msr_name = "IA32_DEBUGCTL"
        integer(kind=int8b), dimension(0:7) :: msr_read
        integer(kind=int8b), dimension(0:7) :: msr_write
        character(len=16),   dimension(0:7) :: msrw_hex
        ! Debug Control (R/W)
     end type MSR_IA32_DEBUGCTL_HASWELL

     type, public :: MSR_PKGC_IRTL1_HASWELL
        public
        integer(kind=int4)      :: addr_dec = 1548
        character(len=5)        :: addr_hex = "0x60B"
        integer(kind=int8b), dimension(0:1)     :: msr_read
        integer(kind=int8b), dimension(0:1)     :: msr_write
        character(len=16),   dimension(0:1)     :: msrw_hex
        character(len=14)       :: msr_name = "MSR_PKGC_IRTL1"
        ! Package C6/C7 Interrupt Response Limit 1 (R/W)
     end type MSR_PKGC_IRTL1_HASWELL

     type, public :: MSR_PKGC_IRTL2_HASWELL
        public
        integer(kind=int4)      :: addr_dec = 1549
        character(len=5)        :: addr_hex = "0x60C"
        integer(kind=int8b), dimension(0:1)     :: msr_read
        integer(kind=int8b), dimension(0:1)     :: msr_write
        character(len=16),   dimension(0:1)     :: msrw_hex
        character(len=14)       :: msr_name = "MSR_PKGC_IRTL2"
        ! Package C6/C7 Interrupt Response Limit 2 (R/W)
     end type MSR_PKGC_IRTL2_HASWELL

     type, public :: MSR_PKG_PERF_STATUS_HASWELL
        public
        integer(kind=int4)     :: addr_dec = 1555
        character(len=5)       :: addr_hex = "0x613"
        integer(kind=int8b), dimension(0:1)    :: msr_read
        character(len=19)      :: msr_name = "MSR_PKG_PERF_STATUS"
        ! PKG Perf Status (R/O)
     end type MSR_PKG_PERF_STATUS_HASWELL

     type, public :: MSR_DRAM_ENERGY_STATUS_HASWELL
        public
        integer(kind=int4)     :: addr_dec = 1561
        character(len=5)       :: addr_hex = "0x619"
        integer(kind=int8b), dimension(0:1)    :: msr_read
        character(len=24)      :: msr_name = "MSR_DRAM_ENERGY_STATUS"
        ! DRAM Energy Status (R/O)
     end type MSR_DRAM_ENERGY_STATUS_HASWELL

     type, public :: MSR_DRAM_PERF_STATUS_HASWELL
        public
        integer(kind=int4)     :: addr_dec = 1563
        character(len=5)       :: addr_hex = "0x61B"
        integer(kind=int8b), dimension(0:1)    :: msr_read
        character(len=22)      :: msr_name = "MSR_DRAM_PERF_STATUS"
        ! DRAM Performance Throttling Status (R/O)
     end type MSR_DRAM_PERF_STATUS_HASWELL

     type, public :: MSR_CONFIG_TDP_NOMINAL_HASWELL
        public
        integer(kind=int4)     :: addr_dec = 1608
        character(len=5)       :: addr_hex = "0x648"
        integer(kind=int8b), dimension(0:1)    :: msr_read
        character(len=25)      :: msr_name = "MSR_CONFIG_TDP_NOMINAL"
        ! Base TDP Ratio (R/O)
     end type MSR_CONFIG_TDP_NOMINAL_HASWELL

     type, public :: MSR_CONFIG_TDP_LEVEL1_HASWELL
        public
        integer(kind=int4)    :: addr_dec = 1609
        character(len=5)      :: addr_hex = "0x649"
        integer(kind=int8b), dimension(0:1)   :: msr_read
        character(len=23)     :: msr_name = "MSR_CONFIG_TDP_LEVEL1"
        ! ConfigTDP Level 1 Ratio and Power Level (R/O)
     end type MSR_CONFIG_TDP_LEVEL1_HASWELL

      type, public :: MSR_CONFIG_TDP_LEVEL2_HASWELL
        public
        integer(kind=int4)    :: addr_dec = 1610
        character(len=5)      :: addr_hex = "0x64A"
        integer(kind=int8b)   :: msr_read
        character(len=23)     :: msr_name = "MSR_CONFIG_TDP_LEVEL2"
        ! ConfigTDP Level 2 Ratio and Power Level (R/O)
     end type MSR_CONFIG_TDP_LEVEL2_HASWELL

     type, public :: MSR_CONFIG_TDP_CONTROL_HASWELL
        public
        integer(kind=int4)    :: addr_dec = 1611
        character(len=5)      :: addr_hex = "0x64B"
        integer(kind=int8b), dimension(0:1)   :: msr_read
        integer(kind=int8b), dimension(0:1)   :: msr_write
        character(len=16),   dimension(0:1)     :: msrw_hex
        character(len=22)     :: msr_name = "MSR_CONFIG_TDP_CONTROL"
        ! ConfigTDP Control (R/W)
     end type MSR_CONFIG_TDP_CONTROL_HASWELL

     type, public :: MSR_TURBO_ACTIVATION_RATIO_HASWELL
        public
        integer(kind=int4)    :: addr_dec = 1612
        character(len=5)      :: addr_hex = "0x64C"
        integer(kind=int8b), dimension(0:1)   :: msr_read
        integer(kind=int8b), dimension(0:1)   :: msr_write
        character(len=16),   dimension(0:1)     :: msrw_hex
        character(len=26)     :: msr_name = "MSR_TURBO_ACTIVATION_RATIO"
        ! ConfigTDP Control (R/W)
     end type MSR_TURBO_ACTIVATION_RATIO_HASWELL

     type, public :: MSR_PKG_CST_CONFIG_CONTROL_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 226
        character(len=4)     :: addr_hex = "0xE2"
        character(len=24)    :: msr_name = "MSR_PKG_CST_CONFIG_CONTROL"
        integer(kind=int8b), dimension(28) :: msr_read
        integer(kind=int8b), dimension(28) :: msr_write
        character(len=16),   dimension(28) :: msrw_hex
        ! C-State Configuration Control (R/W)
     end type MSR_PKG_CST_CONFIG_CONTROL_HASWELL

     type, public :: MSR_TURBO_RATIO_LIMIT_HASWELL
        public
        integer(kind=int4)  :: addr_dec = 429
        character(len=5)    :: addr_hex = "0x1AD"
        integer(kind=int8b), dimension(0:1)   :: msr_read
        integer(kind=int8b), dimension(0:1)   :: msr_write
        character(len=16),   dimension(0:1)   :: msrw_hex
        character(len=21)     :: msr_name = "MSR_TURBO_RATIO_LIMIT"
        ! Maximum Ratio Limit of Turbo Mode
     end type MSR_TURBO_RATIO_LIMIT_HASWELL

     type, public :: MSR_UNC_PERF_GLOBAL_CTRL_HASWELL
        public
        integer(kind=int4)  :: addr_dec = 913
        character(len=5)    :: addr_hex = "0x391"
        character(len=24)   :: msr_name = "MSR_UNC_PERF_GLOBAL_CTRL"
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore PMU Global Control
     end type MSR_UNC_PERF_GLOBAL_CTRL_HASWELL

     type, public :: MSR_UNC_PERF_GLOBAL_STATUS_HASWELL
        public
        integer(kind=int4)  :: addr_dec = 914
        character(len=5)    :: addr_hex = "0x392"
        character(len=26)   :: msr_name = "MSR_UNC_PERF_GLOBAL_STATUS"
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore PMU Main Status
     end type MSR_UNC_PERF_GLOBAL_STATUS_HASWELL

     type, public :: MSR_UNC_PERF_FIXED_CTRL_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 916
        character(len=5)     :: addr_hex = "0x394"
        character(len=21)    :: msr_name = "MSR_UNC_PERF_FIXED_CTRL"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! Uncore Fixed Counter Control (R/W)
     end type MSR_UNC_PERF_FIXED_CTRL_HASWELL

     type, public :: MSR_UNC_PERF_FIXED_CTR_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 917
        character(len=5)     :: addr_hex = "0x395"
        character(len=20)    :: msr_name = "MSR_UNC_PERF_FIXED_CTR"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! MSR_UNC_PERF_FIXED_CTR
     end type MSR_UNC_PERF_FIXED_CTR_HASWELL

     type, public :: MSR_UNC_CBO_CONFIG_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 918
        character(len=5)     :: addr_hex = "0x396"
        integer(kind=int8b)  :: msr_read
        character(len=18)    :: msr_name = "MSR_UNC_CBO_CONFIG"
        ! Uncore C-Box Configuration Information (R/O)
     end type MSR_UNC_CBO_CONFIG_HASWELL

     type, public :: MSR_UNC_ARB_PERFCTR0_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 946
        character(len=5)     :: addr_hex = "0x3B0"
        character(len=19)    :: msr_name = "MSR_UNC_ARB_PERFCTR0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore Arb Unit, Performance Counter 0
     end type MSR_UNC_ARB_PERFCTR0_HASWELL

     type, public :: MSR_UNC_ARB_PERFCTR1_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 947
        character(len=5)     :: addr_hex = "0x3B1"
        character(len=19)    :: msr_name = "MSR_UNC_ARB_PERFCTR1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore Arb Unit, Performance Counter 1
     end type MSR_UNC_ARB_PERFCTR1_HASWELL

     type, public :: MSR_UNC_ARB_PERFEVTSEL0_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 944
        character(len=5)     :: addr_hex = "0x3B2"
        character(len=21)    :: msr_name = "MSR_UNC_ARB_PERFEVTSEL0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore Arb Unit, Counter 0 Event Select MSR
     end type MSR_UNC_ARB_PERFEVTSEL0_HASWELL

     type, public :: MSR_UNC_ARB_PERFEVTSEL1_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 945
        character(len=5)     :: addr_hex = "0x3B3"
        character(len=21)    :: msr_name = "MSR_UNC_ARB_PERFEVTSEL1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore Arb Unit, Counter 1 Event Select MSR
     end type MSR_UNC_ARB_PERFEVTSEL1_HASWELL

     type, public :: MSR_UNC_PERF_GLOBAL_CTRL_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 913
        character(len=5)     :: addr_hex = "0x391"
        character(len=24)    :: msr_name = "MSR_UNC_PERF_GLOBAL_CTRL"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! Uncore PMU Global Control
     end type MSR_UNC_PERF_GLOBAL_CTRL_HASWELL

     type, public :: MSR_RAPL_POWER_UNIT_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1542
        character(len=5)     :: addr_hex = "0x606"
        character(len=19)    :: msr_name = "MSR_RAPL_POWER_UNIT"
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Unit Multipliers Used in RAPL Interfaces (R/O)
     end type MSR_RAPL_POWER_UNIT_HASWELL

     type, public :: MSR_PP0_ENERGY_STATUS_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1593
        character(len=5)     :: addr_hex = "0x639"
        character(len=21)    :: msr_name = "MSR_PP0_ENERGY_STATUS"
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! PP0 Energy Status (R/O)
     end type MSR_PP0_ENERGY_STATUS_HASWELL

     type, public :: MSR_PP1_ENERGY_STATUS_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1601
        character(len=5)     :: addr_hex = "0x641"
        character(len=21)    :: msr_name = "MSR_PP1_ENERGY_STATUS"
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! PP1 Energy Status (R/O)
     end type MSR_PP1_ENERGY_STATUS_HASWELL

     type, public :: MSR_PP1_POWER_LIMIT_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1600
        character(len=5)     :: addr_hex = "0x640"
        character(len=19)    :: msr_name = "MSR_PP1_POWER_LIMIT"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! PP1 RAPL Power Limit Control (R/W)
     end type MSR_PP1_POWER_LIMIT_HASWELL

     type, public :: MSR_PP1_POLICY_HSWELL
        public
        integer(kind=int4)   :: addr_dec = 1602
        character(len=5)     :: addr_hex = "0x642"
        character(len=14)    :: msr_name = "MSR_PP1_POLICY"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! PP1 Balance Policy (R/W)
     end type MSR_PP1_POLICY_HSWELL

     type, public :: MSR_CORE_PERF_LIMIT_REASONS_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1680
        character(len=5)     :: addr_hex = "0x690"
        character(len=27)    :: msr_name = "MSR_CORE_PERF_LIMIT_REASONS"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! Indicator of Frequency Clipping in Processor Cores
        ! (R/W)
     end type MSR_CORE_PERF_LIMIT_REASONS_HASWELL

     type, public :: MSR_RING_PERF_LIMIT_REASONS_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1713
        character(len=5)     :: addr_hex = "0x6B1"
        character(len=27)    :: msr_name = "MSR_RING_PERF_LIMIT_REASONS"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! Indicator of Frequency Clipping in the Ring
        ! Interconnect (R/W)
     end type MSR_RING_PERF_LIMIT_REASONS_HASWELL

     type, public :: MSR_UNC_CBO_0_PERFEVTSEL0_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1792
        character(len=5)     :: addr_hex = "0x700"
        character(len=24)    :: msr_name = "MSR_UNC_CBO_0_PERFEVTSEL0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 0, Counter 0 Event Select MSR
     end type MSR_UNC_CBO_0_PERFEVTSEL0_HASWELL

     type, public :: MSR_UNC_CBO_0_PERFEVTSEL1_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1793
        character(len=5)     :: addr_hex = "0x701"
        character(len=24)    :: msr_name = "MSR_UNC_CBO_0_PERFEVTSEL1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 0, Counter 1 Event Select MSR
     end type MSR_UNC_CBO_0_PERFEVTSEL1_HASWELL

     type, public :: MSR_UNC_CBO_0_PERFCTR0_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1798
        character(len=5)     :: addr_hex = "0x706"
        character(len=21)    :: msr_name = "MSR_UNC_CBO_0_PERFCTR0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 0, Performance Counter 0
     end type MSR_UNC_CBO_0_PERFCTR0_HASWELL

     type, public :: MSR_UNC_CBO_0_PERFCTR1_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1799
        character(len=5)     :: addr_hex = "0x707"
        character(len=21)    :: msr_name = "MSR_UNC_CBO_0_PERFCTR1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 0, Performance Counter 1
     end type MSR_UNC_CBO_0_PERFCTR1_HASWELL

     type, public :: MSR_UNC_CBO_1_PERFEVTSEL0_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1808
        character(len=5)     :: addr_hex = "0x710"
        character(len=24)    :: msr_name = "MSR_UNC_CBO_1_PERFEVTSEL0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 1, Counter 0 Event Select MSR
     end type MSR_UNC_CBO_1_PERFEVTSEL0_HASWELL

     type, public :: MSR_UNC_CBO_1_PERFEVTSEL1_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1809
        character(len=5)     :: addr_hex = "0x711"
        character(len=24)    :: msr_name = "MSR_UNC_CBO_1_PERFEVTSEL1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 1, Counter 1 Event Select MSR
     end type MSR_UNC_CBO_1_PERFEVTSEL1_HASWELL

     type, public :: MSR_UNC_CBO_1_PERFCTR0_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1814
        character(len=5)     :: addr_hex = "0x716"
        character(len=21)    :: msr_name = "MSR_UNC_CBO_1_PERFCTR0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 1, Performance Counter 0
     end type MSR_UNC_CBO_1_PERFCTR0_HASWELL

     type, public :: MSR_UNC_CBO_1_PERFCTR1_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1815
        character(len=5)     :: addr_hex = "0x717"
        character(len=21)    :: msr_name = "MSR_UNC_CBO_1_PERFCTR1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 1, Performance Counter 1
     end type MSR_UNC_CBO_1_PERFCTR1_HASWELL

     type, public :: MSR_UNC_CBO_2_PERFEVTSEL0_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1824
        character(len=5)     :: addr_hex = "0x720"
        character(len=24)    :: msr_name = "MSR_UNC_CBO_2_PERFEVTSEL0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 2, Counter 0 Event Select MSR
     end type MSR_UNC_CBO_2_PERFEVTSEL0_HASWELL

     type, public :: MSR_UNC_CBO_2_PERFEVTSEL1_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1825
        character(len=5)     :: addr_hex = "0x721"
        character(len=24)    :: msr_name = "MSR_UNC_CBO_2_PERFEVTSEL1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 2, Counter 1 Event Select MSR
     end type MSR_UNC_CBO_2_PERFEVTSEL1_HASWELL

     type, public :: MSR_UNC_CBO_2_PERFCTR0_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1830
        character(len=5)     :: addr_hex = "0x726"
        character(len=21)    :: msr_name = "MSR_UNC_CBO_2_PERFCTR0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 2, Performance Counter 0
     end type MSR_UNC_CBO_2_PERFCTR0_HASWELL

     type, public :: MSR_UNC_CBO_2_PERFCTR1_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1831
        character(len=5)     :: addr_hex = "0x727"
        character(len=21)    :: msr_name = "MSR_UNC_CBO_2_PERFCTR1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 2, Performance Counter 1
     end type MSR_UNC_CBO_2_PERFCTR1_HASWELL

     type, public :: MSR_UNC_CBO_3_PERFEVTSEL0_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1840
        character(len=5)     :: addr_hex = "0x730"
        character(len=24)    :: msr_name = "MSR_UNC_CBO_3_PERFEVTSEL0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 3, Counter 0 Event Select MSR
     end type MSR_UNC_CBO_3_PERFEVTSEL0_HASWELL

     type, public :: MSR_UNC_CBO_3_PERFEVTSEL1_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1841
        character(len=5)     :: addr_hex = "0x731"
        character(len=24)    :: msr_name = "MSR_UNC_CBO_3_PERFEVTSEL1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 3, Counter 1 Event Select MSR
     end type MSR_UNC_CBO_3_PERFEVTSEL1_HASWELL

     type, public :: MSR_UNC_CBO_3_PERFCTR0_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1846
        character(len=5)     :: addr_hex = "0x736"
        character(len=21)    :: msr_name = "MSR_UNC_CBO_3_PERFCTR0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 3, Performance Counter 0
     end type MSR_UNC_CBO_3_PERFCTR0_HASWELL

     type, public :: MSR_UNC_CBO_3_PERFCTR1_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1847
        character(len=5)     :: addr_hex = "0x737"
        character(len=21)    :: msr_name = "MSR_UNC_CBO_3_PERFCTR1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore C-Box 2, Performance Counter 1
     end type MSR_UNC_CBO_3_PERFCTR1_HASWELL

     type, public :: MSR_PKG_CST_CONFIG_CONTROL_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 226
        character(len=4)     :: addr_hex = "0xE2"
        character(len=26)    :: msr_name = "MSR_PKG_CST_CONFIG_CONTROL"
        integer(kind=int8b), dimension(0:3) :: msr_read
        integer(kind=int8b), dimension(0:3) :: msr_write
        character(len=16),   dimension(0:3) :: msrw_hex
        ! C-State Configuration Control (R/W)
     end type MSR_PKG_CST_CONFIG_CONTROL_HASWELL

     type, public :: MSR_PKG_C8_RESIDENCY_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1584
        character(len=5)     :: addr_hex = "0x630"
        character(len=19)    :: msr_name = "MSR_PKG_C8_RESIDENCY"
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! C-state values are processor specific C-state code
        ! names, unrelated to MWAIT extension C-state
        ! parameters or ACPI C-States.
     end type MSR_PKG_C8_RESIDENCY_HASWELL

     type, public :: MSR_PKG_C9_RESIDENCY_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1585
        character(len=5)     :: addr_hex = "0x631"
        character(len=19)    :: msr_name = "MSR_PKG_C9_RESIDENCY"
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! C-state values are processor specific C-state code
        ! names, unrelated to MWAIT extension C-state
        ! parameters or ACPI C-States.
     end type MSR_PKG_C9_RESIDENCY_HASWELL

     type, public :: MSR_PKG_C10_RESIDENCY_HASWELL
        public
        integer(kind=int4)   :: addr_dec = 1586
        character(len=5)     :: addr_hex = "0x632"
        character(len=20)    :: msr_name = "MSR_PKG_C10_RESIDENCY"
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! C-state values are processor specific C-state code
        ! names, unrelated to MWAIT extension C-state
        ! parameters or ACPI C-States.
     end type MSR_PKG_C10_RESIDENCY_HASWELL



end module mod_haswell_msr
