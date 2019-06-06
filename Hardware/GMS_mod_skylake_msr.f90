
module mod_skylake_msr


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         mod_skylake_msr
 !          
 !          Purpose:
  !                   This module contains a derived data types
  !                    describing Skylake CPU-architecture MSR registers.
  !                   CPUID: DisplayFamily_DisplayModel signature 06_4EH, 06_5EH, and 06_55H
 !                   
 !                     
 !          History:
 !                        Date: 01-06-2019
 !                        Time: 16:16 GMT+2
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

     type, public :: MSR_IA32_FEATURE_CONTROL_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 58
        character(len=4)     :: addr_hex = "0x3A"
        character(len=18)    :: msr_name = "IA32_FEATURE_CONTROL"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Control Features in Intel 64 Processor (R/W)
     end type MSR_IA32_FEATURE_CONTROL_SKYLAKE

     type, public :: MSR_IA32_MTRRCAP_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 254
        character(len=4)     :: addr_hex = "0xFE"
        character(len=11)    :: msr_name = "IA32_MTRRCAP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        ! MTRR Capability (RO, Architectural)
     end type MSR_IA32_MTRRCAP_SKYLAKE

     type, public :: MSR_IA32_THERM_STATUS_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 412
        character(len=5)     :: addr_hex = "0x19C"
        character(len=16)    :: msr_name = "IA32_THERM_STATUS"
        integer(kind=int8b), dimension(0:27) :: msr_read
        integer(kind=int8b), dimension(0:27) :: msr_write
        character(len=16),   dimension(0:27) :: msrw_hex
        ! Thermal Monitor Status (R/W)
     end type MSR_IA32_THERM_STATUS_SKYLAKE

     type, public :: MSR_TURBO_RATIO_LIMIT_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 429
        character(len=5)     :: addr_hex = "0x1AD"
        character(len=22)    :: msr_name = "MSR_TURBO_RATIO_LIMIT"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! Maximum Ratio Limit of Turbo Mode
        ! RO if MSR_PLATFORM_INFO.[28] = 0,
        ! RW if MSR_PLATFORM_INFO.[28] = 1
     end type MSR_TURBO_RATIO_LIMIT_SKYLAKE

     type, public :: MSR_LASTBRANCH_TOS_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec =  457
        character(len=5)      :: addr_hex = "0x1C9"
        character(len=16)     :: msr_name = "MSR_LASTBRANCH_TOS"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record Stack TOS (R/W)
        ! Contains an index (bits 0-4) that points to the MSR
        ! containing the most recent branch record.
     end type MSR_LASTBRANCH_TOS_SKYLAKE

     type, public :: MSR_POWER_CTL_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 508
        character(len=5)     :: addr_hex = "0x1FC"
        character(len=13)    :: msr_name = "MSR_POWER_CTL"
        integer(kind=int8b), dimension(0:27) :: msr_read
        integer(kind=int8b), dimension(0:27) :: msr_write
        character(len=16),   dimension(0:27) :: msrw_hex
        ! Power Control Register
     end type MSR_POWER_CTL_SKYLAKE

     type, public :: MSR_SGXOWNEREPOCH0_SKYLAKE
        public
        integer(kind=int4)  :: addr_dec = 768
        character(len=5)    :: addr_hex = "0x300"
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! Lower 64 Bit CR_SGXOWNEREPOCH (W)
     end type MSR_SGXOWNEREPOCH0_SKYLAKE

     type, public :: MSR_SGXOWNEREPOCH1_SKYLAKE
        public
        integer(kind=int4)  :: addr_dec = 769
        character(len=5)    :: addr_hex = "0x301"
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! Lower 64 Bit CR_SGXOWNEREPOCH (W)
     end type MSR_SGXOWNEREPOCH1_SKYLAKE

     type, public :: MSR_IA32_PERF_GLOBAL_STATUS_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 910
        character(len=5)     :: addr_hex = "0x38E"
        character(len=23)    :: msr_name = "IA32_PERF_GLOBAL_STATUS"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! 
     end type MSR_IA32_PERF_GLOBAL_STATUS_SKYLAKE

     type, public :: MSR_IA32_PERF_GLOBAL_STATUS_SET_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 913
        character(len=5)     :: addr_hex = "0x391"
        character(len=24)    :: msr_name = "IA32_PERF_GLOBAL_STATUS_SET"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
     end type MSR_IA32_PERF_GLOBAL_STATUS_SET_SKYLAKE

     type, public :: MSR_PEBS_FRONTEND_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 1015
        character(len=5)     :: addr_hex = "0x3F7"
        character(len=15)    :: msr_name = "MSR_PEBS_FRONTEND"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! FrontEnd Precise Event Condition Select (R/W)
     end type MSR_PEBS_FRONTEND_SKYLAKE

     type, public :: MSR_SGX_SVN_STATUS_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 1280
        character(len=5)     :: addr_hex = "0x500"
        character(len=18)    :: msr_name = "MSR_SGX_SVN_STATUS"
        integer(kind=int8b), dimension(0:55) :: msr_read
        ! Status and SVN Threshold of SGX Support for ACM (RO)
     end type MSR_SGX_SVN_STATUS_SKYLAKE

     type, public :: MSR_IA32_RTIT_OUTPUT_BASE_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 1376
        character(len=5)     :: addr_hex = "0x560"
        character(len=21)    :: msr_name = "IA32_RTIT_OUTPUT_BASE"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Trace Output Base Register (R/W)
     end type MSR_IA32_RTIT_OUTPUT_BASE_SKYLAKE

     type, public :: MSR_IA32_RTIT_OUTPUT_MASK_PTRS_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 1377
        character(len=5)     :: addr_hex = "0x561"
        character(len=24)    :: msr_name = "IA32_RTIT_OUTPUT_MASK_PTRS"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Trace Output Mask Pointers Register (R/W)
     end type MSR_IA32_RTIT_OUTPUT_MASK_PTRS_SKYLAKE

     type, public :: MSR_IA32_RTIT_CTL_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1392
        character(len=5)      :: addr_hex = "0x570"
        character(len=13)     :: msr_name = "IA32_RTIT_CTL"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Trace Control Register (R/W)
     end type MSR_IA32_RTIT_CTL_SKYLAKE

     type, public :: MSR_IA32_RTIT_STATUS_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1393
        character(len=5)      :: addr_hex = "0x571"
        character(len=16)     :: msr_name = "IA32_RTIT_STATUS"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Tracing Status Register (R/W)
     end type MSR_IA32_RTIT_STATUS_SKYLAKE

     type, public :: MSR_IA32_RTIT_CR3_MATCH_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1394
        character(len=5)      :: addr_hex = "0x572"
        character(len=19)     :: msr_name = "IA32_RTIT_CR3_MATCH"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Trace Filter CR3 Match Register (R/W)
     end type MSR_IA32_RTIT_CR3_MATCH_SKYLAKE

     type, public :: MSR_IA32_RTIT_ADDR0_A_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1408
        character(len=5)      :: addr_hex = "0x580"
        character(len=18)     :: msr_name = "IA32_RTIT_ADDR0_A"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Region 0 Start Address (R/W)
     end type MSR_IA32_RTIT_ADDR0_A_SKYLAKE

     type, public :: MSR_IA32_RTIT_ADDR0_B_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1409
        character(len=5)      :: addr_hex = "0x581"
        character(len=18)     :: msr_name = "IA32_RTIT_ADDR0_B"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Region 0 End Address (R/W)
     end type MSR_IA32_RTIT_ADDR0_B_SKYLAKE

     type, public :: MSR_IA32_RTIT_ADDR1_A_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1410
        character(len=5)      :: addr_hex = "0x582"
        character(len=18)     :: msr_name = "IA32_RTIT_ADDR1_A"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Region 1 Start Address (R/W)
     end type MSR_IA32_RTIT_ADDR1_A_SKYLAKE

     type, public :: MSR_IA32_RTIT_ADDR1_B_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1411
        character(len=5)      :: addr_hex = "0x583"
        character(len=18)     :: msr_name = "IA32_RTIT_ADDR1_B"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Region 1 End Address (R/W)
     end type MSR_IA32_RTIT_ADDR1_B_SKYLAKE

     type, public :: MSR_PP0_ENERGY_STATUS_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1593
        character(len=5)      :: addr_hex = "0x639"
        character(len=21)     :: msr_name = "MSR_PP0_ENERGY_STATUS"
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! PP0 Energy Status (R/O)
     end type MSR_PP0_ENERGY_STATUS_SKYLAKE

     type, public :: MSR_PLATFORM_ENERGY_COUNTER_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1613
        character(len=5)      :: addr_hex = "0x64D"
        character(len=26)     :: msr_name = "MSR_PLATFORM_ENERGY_COUNTER"
        integer(kind=int8b), dimension(1000) :: msr_read
        ! Platform Energy Counter (R/O)
     end type MSR_PLATFORM_ENERGY_COUNTER_SKYLAKE

     type, public :: MSR_PPERF_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1614
        character(len=5)      :: addr_hex = "0x64E"
        character(len=9)      :: msr_name = "MSR_PPERF"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:55) :: msr_read
        ! Productive Performance Count (R/O)
     end type MSR_PPERF_SKYLAKE

     type, public :: MSR_CORE_PERF_LIMIT_REASONS_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1615
        character(len=5)      :: addr_hex = "0x64F"
        character(len=27)     :: msr_name = "MSR_CORE_PERF_LIMIT_REASONS"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! Indicator of Frequency Clipping in Processor Cores
        ! (R/W)
        ! (Frequency refers to processor core frequency.)
     end type MSR_CORE_PERF_LIMIT_REASONS_SKYLAKE

     type, public :: MSR_PKG_HDC_CONFIG_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1618
        character(len=5)      :: addr_hex = "0x652"
        character(len=17)     :: msr_name = "MSR_PKG_HDC_CONFIG"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! HDC Configuration (R/W)
     end type MSR_PKG_HDC_CONFIG_SKYLAKE

     type, public :: MSR_CORE_HDC_RESIDENCY_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1619
        character(len=5)      :: addr_hex = "0x653"
        character(len=21)     :: msr_name = "MSR_CORE_HDC_RESIDENCY"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:27) :: msr_read
        ! Core HDC Idle Residency (R/O)
     end type MSR_CORE_HDC_RESIDENCY_SKYLAKE

     type, public :: MSR_PKG_HDC_SHALLOW_RESIDENCY_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1621
        character(len=5)      :: addr_hex = "0x655"
        character(len=28)     :: msr_name = "MSR_PKG_HDC_SHALLOW_RESIDENCY"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Accumulate the cycles the package was in C2 state and
        ! at least one logical processor was in forced idle (R/O)
     end type MSR_PKG_HDC_SHALLOW_RESIDENCY_SKYLAKE

     type, public :: MSR_PKG_HDC_DEEP_RESIDENCY_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1622
        character(len=5)      :: addr_hex = "0x656"
        character(len=24)     :: msr_name = "MSR_PKG_HDC_DEEP_RESIDENCY"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Package Cx HDC Idle Residency (R/O)
     end type MSR_PKG_HDC_DEEP_RESIDENCY_SKYLAKE

     type, public :: MSR_WEIGHTED_CORE_C0_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1624
        character(len=5)      :: addr_hex = "0x658"
        character(len=19)     :: msr_name = "MSR_WEIGHTED_CORE_C0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Core-count Weighted C0 Residency (R/O)
     end type MSR_WEIGHTED_CORE_C0_SKYLAKE

     type, public :: MSR_ANY_CORE_C0_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1625
        character(len=5)      :: addr_hex = "0x659"
        character(len=15)     :: msr_name = "MSR_ANY_CORE_C0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Any Core C0 Residency (R/O)
     end type MSR_ANY_CORE_C0_SKYLAKE

     type, public :: MSR_PLATFORM_POWER_LIMIT_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1628
        character(len=5)      :: addr_hex = "0x65C"
        character(len=23)     :: msr_name = "MSR_PLATFORM_POWER_LIMIT"
        integer(kind=int8b)   :: msr_read
        integer(kind=int8b)   :: msr_write
        character(len=16)     :: msrw_hex
        ! Platform Power Limit Control (R/W-L)
        ! Allows platform BIOS to limit power consumption of the
        ! platform devices to the specified values.
     end type MSR_PLATFORM_POWER_LIMIT_SKYLAKE

     type, public :: MSR_LASTBRANCH_16_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1680
        character(len=5)      :: addr_hex = "0x690"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_16_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 16 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_16_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_17_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1681
        character(len=5)      :: addr_hex = "0x691"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_17_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 17 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_17_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_18_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1682
        character(len=5)      :: addr_hex = "0x692"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_18_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 18 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_18_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_19_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1683
        character(len=5)      :: addr_hex = "0x693"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_19_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 19 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_19_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_20_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1684
        character(len=5)      :: addr_hex = "0x694"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_20_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 20 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_20_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_21_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1685
        character(len=5)      :: addr_hex = "0x695"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_21_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 21 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_21_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_22_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1686
        character(len=5)      :: addr_hex = "0x696"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_22_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 20 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_22_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_23_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1687
        character(len=5)      :: addr_hex = "0x697"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_23_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 23 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_23_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_24_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1688
        character(len=5)      :: addr_hex = "0x698"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_24_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 24 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_24_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_25_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1689
        character(len=5)      :: addr_hex = "0x699"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_25_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 25 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_25_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_26_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1690
        character(len=5)      :: addr_hex = "0x69A"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_26_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 26 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_26_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_27_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1691
        character(len=5)      :: addr_hex = "0x69B"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_27_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 27 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_27_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_28_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1692
        character(len=5)      :: addr_hex = "0x69C"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_28_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 28 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_28_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_29_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1693
        character(len=5)      :: addr_hex = "0x69D"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_29_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 29 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_29_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_30_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1694
        character(len=5)      :: addr_hex = "0x69E"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_30_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 30 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_30_FROM_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_31_FROM_IP_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1695
        character(len=5)      :: addr_hex = "0x69F"
        character(len=25)     :: msr_name = "MSR_LASTBRANCH_31_FROM_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 31 From IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the source instruction
     end type MSR_LASTBRANCH_31_FROM_IP_SKYLAKE

     type, public :: MSR_RING_PERF_LIMIT_REASONS_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 1713
        character(len=5)     :: addr_hex = "0x6B1"
        character(len=29)    :: msr_name = "MSR_RING_PERF_LIMIT_REASONS"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! Indicator of Frequency Clipping in the Ring Interconnect
        ! (R/W)
        ! (Frequency refers to ring interconnect in the uncore.)
     end type MSR_RING_PERF_LIMIT_REASONS_SKYLAKE

     type, public :: MSR_LASTBRANCH_16_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1744
        character(len=5)   :: addr_hex = "0x6D0"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_16_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 16 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_16_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_17_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1745
        character(len=5)   :: addr_hex = "0x6D1"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_17_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 17 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_17_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_18_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1746
        character(len=5)   :: addr_hex = "0x6D2"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_18_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 18 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_18_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_19_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1747
        character(len=5)   :: addr_hex = "0x6D3"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_19_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 19 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_19_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_20_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1748
        character(len=5)   :: addr_hex = "0x6D4"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_20_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 20 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_20_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_21_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1749
        character(len=5)   :: addr_hex = "0x6D5"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_21_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 21 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_21_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_22_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1750
        character(len=5)   :: addr_hex = "0x6D6"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_22_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 22 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_22_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_23_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1751
        character(len=5)   :: addr_hex = "0x6D7"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_23_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 23 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_23_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_24_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1752
        character(len=5)   :: addr_hex = "0x6D8"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_24_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 24 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_24_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_25_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1753
        character(len=5)   :: addr_hex = "0x6D9"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_25_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 25 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_25_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_25_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1753
        character(len=5)   :: addr_hex = "0x6D9"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_25_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 25 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_25_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_26_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1754
        character(len=5)   :: addr_hex = "0x6DA"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_26_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 26 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_26_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_27_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1755
        character(len=5)   :: addr_hex = "0x6DB"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_27_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 27 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_27_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_28_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1756
        character(len=5)   :: addr_hex = "0x6DC"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_28_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 28 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_28_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_29_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1757
        character(len=5)   :: addr_hex = "0x6DD"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_29_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 29 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_29_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_30_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1758
        character(len=5)   :: addr_hex = "0x6DE"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_30_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 30 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_30_TO_IP_SKYLAKE

     type, public :: MSR_LASTBRANCH_31_TO_IP_SKYLAKE
        public
        integer(kind=int4) :: addr_dec = 1759
        character(len=5)   :: addr_hex = "0x6DF"
        character(len=22)  :: msr_name = "MSR_LASTBRANCH_31_TO_IP"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 25 To IP (R/W)
        ! One of 32 triplets of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! pointers to the destination instruction
     end type MSR_LASTBRANCH_31_TO_IP_SKYLAKE

     type, public :: MSR_IA32_PM_ENABLE_SKYLAKE
        public
        integer(kind=int4)  :: addr_dec = 1904
        character(len=5)    :: addr_hex = "0x770"
        character(len=14)   :: msr_name = "IA32_PM_ENABLE"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
     end type MSR_IA32_PM_ENABLE_SKYLAKE

     type, public :: MSR_IA32_HWP_CAPABILITIES_SKYLAKE
        public
        integer(kind=int4)  :: addr_dec = 1905
        character(len=5)    :: addr_hex = "0x771"
        character(len=20)   :: msr_name = "IA32_HWP_CAPABILITIES"
        integer(kind=int8b), dimension(0:55) :: msr_read
        !
     end type MSR_IA32_HWP_CAPABILITIES_SKYLAKE

     type, public :: MSR_IA32_HWP_REQUEST_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 1908
        character(len=5)     :: addr_hex = "0x774"
        character(len=16)    :: msr_name = "IA32_HWP_REQUEST"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! 
     end type MSR_IA32_HWP_REQUEST_SKYLAKE

     type, public :: MSR_IA32_HWP_STATUS_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 1911
        character(len=5)     :: addr_hex = "0x777"
        character(len=15)    :: msr_name = "IA32_HWP_STATUS"
        integer(kind=int8b), dimension(0:55) :: msr_read
     end type MSR_IA32_HWP_STATUS_SKYLAKE

     type, public :: MSR_IA32_PKG_HDC_CTL_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3504
        character(len=5)     :: addr_hex = "0xDB0"
        character(len=18)    :: msr_name = "IA32_PKG_HDC_CTL"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
     end type MSR_IA32_PKG_HDC_CTL_SKYLAKE

     type, public :: MSR_IA32_PM_CTL1_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3505
        character(len=5)     :: addr_hex = "0xDB1"
        character(len=12)    :: msr_name = "IA32_PM_CTL1"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
     end type MSR_IA32_PM_CTL1_SKYLAKE

     type, public :: MSR_IA32_THREAD_STALL_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3506
        character(len=5)     :: addr_hex = "0xDB2"
        character(len=17)    :: msr_name = "IA32_THREAD_STALL"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
     end type MSR_IA32_THREAD_STALL_SKYLAKE

     type, public :: MSR_LBR_INFO_0_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3520
        character(len=5)     :: addr_hex = "0xDC0"
        character(len=13)    :: msr_name = "MSR_LBR_INFO_0"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 0 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_0_SKYLAKE

     type, public :: MSR_LBR_INFO_1_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3521
        character(len=5)     :: addr_hex = "0xDC1"
        character(len=13)    :: msr_name = "MSR_LBR_INFO_1"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 1 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_1_SKYLAKE

     type, public :: MSR_LBR_INFO_2_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3522
        character(len=5)     :: addr_hex = "0xDC2"
        character(len=13)    :: msr_name = "MSR_LBR_INFO_2"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 2 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_2_SKYLAKE

     type, public :: MSR_LBR_INFO_3_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3523
        character(len=5)     :: addr_hex = "0xDC3"
        character(len=13)    :: msr_name = "MSR_LBR_INFO_3"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 3 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_3_SKYLAKE

     type, public :: MSR_LBR_INFO_4_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3524
        character(len=5)     :: addr_hex = "0xDC4"
        character(len=13)    :: msr_name = "MSR_LBR_INFO_4"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 4 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_4_SKYLAKE

     type, public :: MSR_LBR_INFO_5_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3525
        character(len=5)     :: addr_hex = "0xDC4"
        character(len=13)    :: msr_name = "MSR_LBR_INFO_5"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 5 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_5_SKYLAKE

     type, public :: MSR_LBR_INFO_6_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3526
        character(len=5)     :: addr_hex = "0xDC6"
        character(len=13)    :: msr_name = "MSR_LBR_INFO_6"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 6  Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_6_SKYLAKE

     type, public :: MSR_LBR_INFO_7_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3527
        character(len=5)     :: addr_hex = "0xDC7"
        character(len=13)    :: msr_name = "MSR_LBR_INFO_7"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 7 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_7_SKYLAKE

     type, public :: MSR_LBR_INFO_8_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3528
        character(len=5)     :: addr_hex = "0xDC8"
        character(len=13)    :: msr_name = "MSR_LBR_INFO_8"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 8 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_8_SKYLAKE

     type, public :: MSR_LBR_INFO_9_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3529
        character(len=5)     :: addr_hex = "0xDC9"
        character(len=13)    :: msr_name = "MSR_LBR_INFO_9"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 9 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_9_SKYLAKE

     type, public :: MSR_LBR_INFO_10_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3530
        character(len=5)     :: addr_hex = "0xDCA"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_10"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 10 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_10_SKYLAKE

     type, public :: MSR_LBR_INFO_11_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3531
        character(len=5)     :: addr_hex = "0xDCB"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_11"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 11 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_11_SKYLAKE

     type, public :: MSR_LBR_INFO_12_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3532
        character(len=5)     :: addr_hex = "0xDCC"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_12"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 12 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_12_SKYLAKE

     type, public :: MSR_LBR_INFO_13_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3533
        character(len=5)     :: addr_hex = "0xDCD"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_13"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 13 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_13_SKYLAKE

     type, public :: MSR_LBR_INFO_14_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3534
        character(len=5)     :: addr_hex = "0xDCE"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_14"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 14 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_14_SKYLAKE

     type, public :: MSR_LBR_INFO_15_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3535
        character(len=5)     :: addr_hex = "0xDCF"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_15"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 15 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_15_SKYLAKE

     type, public :: MSR_LBR_INFO_16_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3536
        character(len=5)     :: addr_hex = "0xDD0"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_16"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 16 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_16_SKYLAKE

     type, public :: MSR_LBR_INFO_17_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3537
        character(len=5)     :: addr_hex = "0xDD1"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_17"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 17 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_17_SKYLAKE

     type, public :: MSR_LBR_INFO_18_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3538
        character(len=5)     :: addr_hex = "0xDD2"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_18"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 18 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_18_SKYLAKE

     type, public :: MSR_LBR_INFO_19_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3539
        character(len=5)     :: addr_hex = "0xDD3"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_19"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 19 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_19_SKYLAKE

     type, public :: MSR_LBR_INFO_20_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3540
        character(len=5)     :: addr_hex = "0xDD4"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_20"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 20 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_20_SKYLAKE

     type, public :: MSR_LBR_INFO_21_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3541
        character(len=5)     :: addr_hex = "0xDD5"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_21"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 21 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_21_SKYLAKE

     type, public :: MSR_LBR_INFO_22_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3542
        character(len=5)     :: addr_hex = "0xDD6"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_22"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 22 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_22_SKYLAKE

     type, public :: MSR_LBR_INFO_23_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3543
        character(len=5)     :: addr_hex = "0xDD7"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_23"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 23 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_23_SKYLAKE

     type, public :: MSR_LBR_INFO_24_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3544
        character(len=5)     :: addr_hex = "0xDD8"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_24"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 24 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_24_SKYLAKE

     type, public :: MSR_LBR_INFO_25_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3545
        character(len=5)     :: addr_hex = "0xDD9"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_25"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 25 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_25_SKYLAKE

     type, public :: MSR_LBR_INFO_26_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3546
        character(len=5)     :: addr_hex = "0xDDA"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_26"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 26 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_26_SKYLAKE

     type, public :: MSR_LBR_INFO_27_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3547
        character(len=5)     :: addr_hex = "0xDDB"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_27"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 27 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_27_SKYLAKE

     type, public :: MSR_LBR_INFO_28_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3548
        character(len=5)     :: addr_hex = "0xDDC"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_28"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 28 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_28_SKYLAKE

     type, public :: MSR_LBR_INFO_29_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3549
        character(len=5)     :: addr_hex = "0xDDD"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_29"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 29 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_29_SKYLAKE

     type, public :: MSR_LBR_INFO_30_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3550
        character(len=5)     :: addr_hex = "0xDDE"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_30"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 30 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_30_SKYLAKE

     type, public :: MSR_LBR_INFO_31_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 3551
        character(len=5)     :: addr_hex = "0xDDF"
        character(len=14)    :: msr_name = "MSR_LBR_INFO_31"
        integer(kind=int8b), dimension(0:55) :: msr_read
        integer(kind=int8b), dimension(0:55) :: msr_write
        character(len=16),   dimension(0:55) :: msrw_hex
        ! Last Branch Record 31 Additional Information (R/W)
        ! One of 32 triplet of last branch record registers on the
        ! last branch record stack. This part of the stack contains
        ! flag, TSX-related and elapsed cycle information
     end type MSR_LBR_INFO_31_SKYLAKE

     type, public :: MSR_UNC_PERF_FIXED_CTRL_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 916
        character(len=5)     :: addr_hex = "0x394"
        character(len=23)    :: msr_name = "MSR_UNC_PERF_FIXED_CTRL"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! Uncore Fixed Counter Control (R/W)
     end type MSR_UNC_PERF_FIXED_CTRL_SKYLAKE

     type, public :: MSR_UNC_PERF_FIXED_CTR_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 917
        character(len=5)     :: addr_hex = "0x395"
        character(len=22)    :: msr_name = "MSR_UNC_PERF_FIXED_CTR"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:1) :: msr_read
        ! Uncore Fixed Counter
     end type MSR_UNC_PERF_FIXED_CTR_SKYLAKE

     type, public :: MSR_UNC_CB0_CONFIG_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 918
        character(len=5)     :: addr_hex = "0x396"
        character(len=17)    :: msr_name = "MSR_UNC_CB0_CONFIG"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! Uncore C-Box Configuration Information (R/O)
     end type MSR_UNC_CB0_CONFIG_SKYLAKE
     
     type, public :: MSR_UNC_ARB_PERFCTR0_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 946
        character(len=5)     :: addr_hex = "0x3B0"
        character(len=20)    :: msr_name = "MSR_UNC_ARB_PERFCTR0"
      
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore Arb Unit, Performance Counter 0
     end type MSR_UNC_ARB_PERFCTR0_SKYLAKE

     type, public :: MSR_UNC_ARB_PERFCTR1_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 947
        character(len=5)     :: addr_hex = "0x3B1"
        character(len=20)    :: msr_name = "MSR_UNC_ARB_PERFCTR1"
       
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore Arb Unit, Performance Counter 1
     end type MSR_UNC_ARB_PERFCTR1_SKYLAKE

     type, public :: MSR_UNC_ARB_PERFEVTSEL0_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 944
        character(len=5)     :: addr_hex = "0x3B2"
        character(len=21)    :: msr_name = "MSR_UNC_ARB_PERFEVTSEL0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore Arb Unit, Counter 0 Event Select MSR
     end type MSR_UNC_ARB_PERFEVTSEL0_SKYLAKE

     type, public :: MSR_UNC_ARB_PERFEVTSEL1_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 945
        character(len=5)     :: addr_hex = "0x3B3"
        character(len=21)    :: msr_name = "MSR_UNC_ARB_PERFEVTSEL1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore Arb Unit, Counter 0 Event Select MSR
     end type MSR_UNC_ARB_PERFEVTSEL1_SKYLAKE

     type, public :: MSR_UNC_CBO_0_PERFEVTSEL0_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 1792
        character(len=5)     :: addr_hex = "0x700"
        character(len=25)    :: msr_name = "MSR_UNC_CBO_0_PERFEVTSEL0"
     
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore C-Box 0, Counter 0 Event Select MSR
     end type MSR_UNC_CBO_0_PERFEVTSEL0_SKYLAKE

     type, public :: MSR_UNC_CBO_0_PERFEVTSEL1_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 1793
        character(len=5)     :: addr_hex = "0x701"
        character(len=25)    :: msr_name = "MSR_UNC_CBO_0_PERFEVTSEL1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore C-Box 0, Counter 0 Event Select MSR
     end type MSR_UNC_CBO_0_PERFEVTSEL1_SKYLAKE

     type, public :: MSR_UNC_CBO_0_PERFCTR0_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 1798
        character(len=5)     :: addr_hex = "0x706"
        character(len=21)    :: msr_name = "MSR_UNC_CB0_0_PERFCTR0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore C-Box 0, Performance Counter 0
     end type MSR_UNC_CB0_0_PERFCTR0_SKYLAKE

     type, public :: MSR_UNC_CBO_0_PERFCTR1_SKYLAKE
        public
        integer(kind=int4)   :: addr_dec = 1799
        character(len=5)     :: addr_hex = "0x707"
        character(len=21)    :: msr_name = "MSR_UNC_CBO_0_PERFCTR1"
      
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore C-Box 0, Performance Counter 1
     end type MSR_UNC_CBO_0_PERFCTR1_SKYLAKE

     type, public :: MSR_UNC_CBO_1_PERFEVTSEL0_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1808
        character(len=5)      :: addr_hex = "0x710"
        character(len=25)     :: msr_name = "MSR_UNC_CBO_1_PERFEVTSEL0"
       
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore C-Box 1, Counter 0 Event Select MSR
     end type MSR_UNC_CBO_1_PERFEVTSEL0_SKYLAKE

     type, public :: MSR_UNC_CBO_1_PERFEVTSEL1_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1809
        character(len=5)      :: addr_hex = "0x711"
        character(len=25)     :: msr_name = "MSR_UNC_CBO_1_PERFEVTSEL1"
       
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore C-Box 1, Counter 0 Event Select MSR
     end type MSR_UNC_CBO_1_PERFEVTSEL1_SKYLAKE
     
     type, public :: MSR_UNC_CBO_1_PERFCTR0_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1814
        character(len=5)      :: addr_hex = "0x716"
        character(len=21)     :: msr_name = "MSR_UNC_CBO_1_PERFCTR0"
         
        integer(kind=int8b), dimension(0:1) :: msr_read
     end type MSR_UNC_CBO_1_PERFCTR0_SKYLAKE

     type, public :: MSR_UNC_CBO_1_PERFCTR1_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 1814
        character(len=5)      :: addr_hex = "0x716"
        character(len=21)     :: msr_name = "MSR_UNC_CBO_1_PERFCTR1"
         
        integer(kind=int8b), dimension(0:1) :: msr_read
     end type MSR_UNC_CBO_1_PERFCTR1_SKYLAKE

     type, public :: MSR_UNC_PERF_GLOBAL_CTRL_SKYLAKE
        public
        integer( kind=int4)   :: addr_dec = 3585
        character(len=5)      :: addr_hex = "0xE01"
        character(len=24)     :: msr_name = "MSR_UNC_PERF_GLOBAL_CTRL"
        integer(kind=int8b), dimension(0:1) :: msr_read
        integer(kind=int8b), dimension(0:1) :: msr_write
        character(len=16),   dimension(0:1) :: msrw_hex
        ! Uncore PMU Global Control
     end type MSR_UNC_PERF_GLOBAL_CTRL_SKYLAKE

     type, public :: MSR_UNC_PERF_GLOBAL_STATUS_SKYLAKE
        public
        integer(kind=int4)    :: addr_dec = 3586
        character(len=5)      :: addr_hex = "0xE02"
        character(len=26)     :: msr_name = "MSR_UNC_PERF_GLOBAL_STATUS"
        integer(kind=int8b), dimension(0:1) :: msr_read
        ! Uncore PMU Main Status
     end type MSR_UNC_PERF_GLOBAL_STATUS_SKYLAKE

end module mod_skylake_msr
