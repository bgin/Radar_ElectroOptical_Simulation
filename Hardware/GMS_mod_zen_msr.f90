

module  mod_zen_msr


 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         mod_zen_msr
 !          
 !          Purpose:
 !                   This module contains a derived data types
 !                    describing AMD Zen family 17h CPU-architecture MSR registers.
 !                   CPUID: AMD Processor models 00h-2Fh
 !                   
 !                     
 !          History:
 !                        Date: 10-06-2019
 !                        Time: 17:33 GMT+2
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
 !                           AMD 'Open-Source Register Reference'
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


     type, public :: MSR_TSC_ZEN
        public
        integer(kind=int4)    :: addr_dec = 10
        character(len=4)      :: addr_hex = "0x10"
        character(len=3)      :: msr_name = "TSC"
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! TSC: time stamp counter. Read-write,Volatile. Reset: 0000_0000_0000_0000h.
     end type MSR_TSC_ZEN

     type, public :: MSR_APIC_BAR_ZEN
        public
        integer(kind=int4)   :: addr_dec = 27
        character(len=4)     :: addr_hex = "0x1B"
        character(len=8)     :: msr_name = "APIC_BAR"
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_001B [APIC Base Address] (Core::X86::Msr::APIC_BAR)
     end type MSR_APIC_BAR_ZEN

     type, public :: MSR_MPERF_ZEN
        public
        integer(kind=int4)   :: addr_dec = 231
        character(len=4)     :: addr_hex = "0xE7"
        character(len=5)     :: msr_name = "MPERF"
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MPERF: maximum core clocks counter. Read-write,Volatile. Reset: 0000_0000_0000_0000h
     end type MSR_MPERF_ZEN

     type, public :: MSR_APERF_ZEN
        public
        integer(kind=int4)   :: addr_dec = 232
        character(len=4)     :: addr_hex = "0xE8"
        character(len=5)     :: msr_name = "APERF"
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! APERF: actual core clocks counter. Read-write,Volatile. Reset: 0000_0000_0000_0000h
     end type MSR_APERF_ZEN

     type, public :: MSR_MTRR_ZEN
        public
        integer(kind=int4)   :: addr_dec = 254
        character(len=4)     :: addr_hex = "0xFE"
        character(len=4)     :: msr_name = "MTRR"
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_00FE [MTRR Capabilities] (Core::X86::Msr::MTRRcap)
     end type MSR_MTRR_ZEN

     type, public :: MSR_MCG_CAP_ZEN
        public
        integer(kind=int4)   :: addr_dec = 377
        character(len=5)     :: addr_hex = "0x179"
        character(len=7)     :: msr_name = "MCG_CAP"
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_0179 [Global Machine Check Capabilities] (Core::X86::Msr::MCG_CAP)
     end type MSR_MCG_CAP_ZEN

     type, public :: MSR_MCG_STAT_ZEN
        public
        integer(kind=int4)   :: addr_dec = 378
        character(len=5)     :: addr_hex = "0x17A"
        character(len=8)     :: msr_name = "MCG_STAT"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSR0000_017A [Global Machine Check Status] (Core::X86::Msr::MCG_STAT)
     end type MSR_MCG_STAT_ZEN

     type, public :: MSR_MCG_CTL_ZEN
        public
        integer(kind=int4)   :: addr_dec = 379
        character(len=5)     :: addr_hex = "0x17B"
        character(len=7)     :: msr_name = "MCG_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSR0000_017B [Global Machine Check Exception Reporting Control] (Core::X86::Msr::MCG_CTL)
     end type MSR_MCG_CTL_ZEN

     type, public :: MSR_DBG_CTL_ZEN
        public
        integer(kind=int4)   :: addr_dec = 473
        character(len=5)     :: addr_hex = "0x1D9"
        character(len=7)     :: msr_name = "DGB_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        !MSR0000_01D9 [Debug Control] (Core::X86::Msr::DBG_CTL_MSR)
     end type MSR_DBG_CTL_ZEN

     type, public :: MSR_BR_FROM_ZEN
        public
        integer(kind=int4)   :: addr_dec = 475
        character(len=5)     :: addr_hex = "0x1DB"
        character(len=7)     :: msr_name = "BR_FROM"
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_01DB [Last Branch From IP] (Core::X86::Msr::BR_FROM)
     end type MSR_BR_FROM_ZEN

     type, public :: MSR_BR_TO_ZEN
        public
        integer(kind=int4)   :: addr_dec = 476
        character(len=5)     :: addr_hex = "0x1DC"
        character(len=5)     :: msr_name = "BR_TO"
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_01DC [Last Branch To IP] (Core::X86::Msr::BR_TO)
     end type MSR_BR_TO_ZEN

     type, public :: MSR_LAST_EXP_FROM_ZEN
        public
        integer(kind=int4)   :: addr_dec = 477
        character(len=5)     :: addr_hex = "0x1DD"
        character(len=16)    :: msr_name = "LAST_EXP_FROM_IP"
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_01DD [Last Exception From IP] (Core::X86::Msr::LastExcpFromIp)
     end type MSR_LAST_EXP_FROM_ZEN

     type, public :: MSR_LAST_EXP_TO_ZEN
        public
        integer(kind=int4)   :: addr_dec = 478
        character(len=5)     :: addr_hex = "0x1DE"
        character(len=14)    :: msr_name = "LAST_EXP_TO_IP"
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_01DE [Last Exception To IP] (Core::X86::Msr::LastExcpToIp)
     end type MSR_LAST_EXP_TO_ZEN

     type, public :: MSR_MTRR_FIXED_ZEN
        public
        integer(kind=int4)   :: addr_dec = 592
        character(len=5)     :: addr_hex = "0x250"
        character(len=14)    :: msr_name = "MTRR_FIXED"
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_0250 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_64K)
     end type MSR_MTRR_FIXED_ZEN

     type, public :: MSR_MTRR_FIXED16K_ZEN
        public
        integer(kind=int4)    :: addr_dec = 600
        character(len=5)      :: addr_hex = "0x258"
        character(len=13)     :: msr_name = "MTRR_FIXED16K"
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_0258 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_16K_0)
     end type MSR_MTRR_FIXED16K_ZEN

     type, public :: MSR_MTRR_FIXED16K1_ZEN
        public
        integer(kind=int4)    :: addr_dec = 601
        character(len=5)      :: addr_hex = "0x259"
        character(len=14)     :: msr_name = "MTRR_FIXED16K1"
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_0259 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_16K_1)
     end type MSR_MTRR_FIXED16K1_ZEN

     type, public :: MSR_MTRR_FIXED4K_ZEN
        public
        integer(kind=int4)    :: addr_dec = 616
        character(len=5)      :: addr_hex = "0x268"
        character(len=12)     :: msr_name = "MTRR_FIXED4K"
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_0268 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_0)
     end type MSR_MTRR_FIXED4K_ZEN

     type, public :: MSR_MTRR_FIXED4K1_ZEN
        public
        integer(kind=int4)    :: addr_dec = 617
        character(len=5)      :: addr_hex = "0x269"
        character(len=13)     :: msr_name = "MTRR_FIXED4K1"
        integer(kind=int8b), dimension(0:31) :: msr_read
       ! MSR0000_0269 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_1)
     end type MSR_MTRR_FIXED4K1_ZEN

     type, public :: MSR_MTRR_FIXED4K2_ZEN
        public
        integer(kind=int4)    :: addr_dec = 618
        character(len=5)      :: addr_hex = "0x26A"
        character(len=13)     :: msr_name = "MTRR_FIXED4K2"
        integer(kind=int8b), dimension(0:31) :: msr_read
       !MSR0000_026A [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_2)
     end type MSR_MTRR_FIXED4K2_ZEN

     type, public :: MSR_MTRR_FIXED4K3_ZEN
        public
        integer(kind=int4)    :: addr_dec = 619
        character(len=5)      :: addr_hex = "0x26B"
        character(len=13)     :: msr_name = "MTRR_FIXED4K3"
        integer(kind=int8b), dimension(0:31) :: msr_read
       !MSR0000_026B [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_3)
     end type MSR_MTRR_FIXED4K3_ZEN

     type, public :: MSR_MTRR_FIXED4K4_ZEN
        public
        integer(kind=int4)    :: addr_dec = 620
        character(len=5)      :: addr_hex = "0x26C"
        character(len=13)     :: msr_name = "MTRR_FIXED4K4"
        integer(kind=int8b), dimension(0:31) :: msr_read
       ! MSR0000_026C [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_4)
     end type MSR_MTRR_FIXED4K4_ZEN

     type, public :: MSR_MTRR_FIXED4K5_ZEN
        public
        integer(kind=int4)    :: addr_dec = 621
        character(len=5)      :: addr_hex = "0x26D"
        character(len=13)     :: msr_name = "MTRR_FIXED4K5"
        integer(kind=int8b), dimension(0:31) :: msr_read
       ! MSR0000_026D [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_5)
     end type MSR_MTRR_FIXED4K5_ZEN

     type, public :: MSR_MTRR_FIXED4K6_ZEN
        public
        integer(kind=int4)    :: addr_dec = 622
        character(len=5)      :: addr_hex = "0x26E"
        character(len=13)     :: msr_name = "MTRR_FIXED4K6"
        integer(kind=int8b), dimension(0:31) :: msr_read
       ! MSR0000_026E [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_6)
     end type MSR_MTRR_FIXED4K6_ZEN

     type, public :: MSR_MTRR_FIXED4K7_ZEN
        public
        integer(kind=int4)    :: addr_dec = 623
        character(len=5)      :: addr_hex = "0x26F"
        character(len=13)     :: msr_name = "MTRR_FIXED4K7"
        integer(kind=int8b), dimension(0:31) :: msr_read
       ! MSR0000_026F [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_7)
     end type MSR_MTRR_FIXED4K7_ZEN

     type, public :: MSR_PAT_ZEN
        public
        integer(kind=int4)    :: addr_dec = 631
        character(len=5)      :: addr_hex = "0x277"
        character(len=3)      :: msr_name = "PAT"
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_0277 [Page Attribute Table] (Core::X86::Msr::PAT)
        ! This register specifies the memory type based on the PAT, PCD, and PWT bits in the virtual address page tables.
     end type MSR_PAT_ZEN

     type, public :: MSR_MTRR_DEFTYPE_ZEN
        public
        integer(kind=int4)    :: addr_dec = 767
        character(len=5)      :: addr_hex = "0x2FF"
        character(len=12)     :: msr_name = "MTRR_DEFTYPE"
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_02FF [MTRR Default Memory Type] (Core::X86::Msr::MTRRdefType)
     end type MSR_MTRR_DEFTYPE_ZEN

     type, public :: MSR_EFER_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0000080"
        character(len=4)      :: msr_name = "EFER"
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSRC000_0080 [Extended Feature Enable] (Core::X86::Msr::EFER)
     end type MSR_EFER_ZEN

     type, public :: MSR_MPERF_READONLY_ZEN
        public
        character(len=10)     :: addr_hex = "0xC00000E7"
        character(len=14)     :: msr_name = "MPERF_READONLY"
        !DIR$ ATTRIBUTES ALIGN ; 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC000_00E7 [Read-Only Max Performance Frequency Clock Count] (Core::X86::Msr::MPerfReadOnly)
        ! This register
        ! increments in proportion to the actual number of core clocks cycles while the core is in P0
     end type MSR_MPERF_READONLY_ZEN

     type, public :: MSR_APERF_READONLY_ZEN
        public
        character(len=10)     :: addr_hex = "0xC00000E8"
        character(len=14)     :: msr_name = "APERF_READONLY"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC000_00E8 [Read-Only Actual Performance Frequency Clock Count] (Core::X86::Msr::APerfReadOnly)
        ! This register
        ! increments in proportion to the actual number of core clocks cycles while the core is in C0
     end type MSR_APERF_READONLY_ZEN

     type, public :: MSR_IRPERF_COUNT_ZEN
        public
        character(len=10)     :: addr_hex = "0xC00000E9"
        character(len=12)     :: msr_name = "IRPERF_COUNT"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC000_00E9 [Instructions Retired Performance Count] (Core::X86::Msr::IRPerfCount)
     end type MSR_IRPERF_COUNT_ZEN

     type, public :: MSR_TSC_AUX_ZEN
        public
        integer(kind=int4)    :: addr_dec = 257
        character(len=5)      :: addr_hex = "0x101"
        character(len=7)      :: msr_name = "TSC_AUX"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC000_0103 [Auxiliary Time Stamp Counter] (Core::X86::Msr::TSC_AUX) 
     end type MSR_TSC_AUX_ZEN

     type, public :: MSR_TSC_RATIO_ZEN
        public
        integer(kind=int4)    :: addr_dec = 260
        character(len=5)      :: addr_hex = "0x104"
        character(len=9)      :: msr_name = "TSC_RATIO"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC000_0104 [Time Stamp Counter Ratio] (Core::X86::Msr::TscRateMsr)
     end type MSR_TSC_RATIO_ZEN

     type, public :: MSR_MCA_INTR_CFG_ZEN
        public
        character(len=8)      :: addr_hex = "0xC0000410"
        character(len=12)     :: msr_name = "MCA_INTR_CFG"
        integer(kind=int8b)   :: msr_read
        integer(kind=int8b)   :: msr_write
        character(len=16)     :: msrw_hex
        ! MSRC000_0410 [MCA Interrupt Configuration] (Core::X86::Msr::McaIntrCfg)
     end type MSR_MCA_INTR_CFG_ZEN

     type, public :: MSR_SYS_CFG_ZEN
        public
        character(len=8)     :: addr_hex = "0xC0010010"
        character(len=7)     :: msr_name = "SYS_CFG"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_0010 [System Configuration] (Core::X86::Msr::SYS_CFG)
     end type MSR_SYS_CFG_ZEN

     type, public :: MSR_HW_CFG_ZEN
        public
        character(len=8)     :: addr_hex = "0xC0010015"
        character(len=6)     :: msr_name = "HW_CFG"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_0015 [Hardware Configuration] (Core::X86::Msr::HWCR)
     end type MSR_HW_CFG_ZEN

     type, public :: MSR_TOP_MEM_ZEN
        public
        character(len=8)     :: addr_hex = "0xC001001A"
        character(len=7)     :: msr_name = "TOP_MEM"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_001A [Top Of Memory] (Core::X86::Msr::TOP_MEM)
     end type MSR_TOP_MEM_ZEN

     type, public :: MSR_TOP_MEM2_ZEN
        public
        character(len=10)     :: addr_hex = "0xC001001D"
        character(len=8)     :: msr_name = "TOP_MEM2"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_001D [Top Of Memory 2] (Core::X86::Msr::TOM2)
     end type MSR_TOP_MEM2_ZEN

     type, public :: MSR_IORR_BASE1_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010016"
        character(len=10)     :: msr_name = "IORR_BASE1"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_001[6...8] [IO Range Base] (Core::X86::Msr::IORR_BASE)
     end type MSR_IORR_BASE1_ZEN

     type, public :: MSR_IORR_BASE2_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010017"
        character(len=10)     :: msr_name = "IORR_BASE2"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        !MSRC001_001[6...8] [IO Range Base] (Core::X86::Msr::IORR_BASE)
     end type MSR_IORR_BASE2_ZEN

     type, public :: MSR_IORR_BASE3_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010018"
        character(len=10)     :: msr_name = "IORR_BASE3"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        !MSRC001_001[6...8] [IO Range Base] (Core::X86::Msr::IORR_BASE)
     end type MSR_IORR_BASE3_ZEN

     type, public :: MSR_IORR_MASK1_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010017"
        character(len=10)     :: msr_name = "IORR_MASK1"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_001[7...9] [IO Range Mask] (Core::X86::Msr::IORR_MASK)
     end type MSR_IORR_MASK1_ZEN

     type, public :: MSR_IORR_MASK2_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010018"
        character(len=10)     :: msr_name = "IORR_MASK2"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_001[7...9] [IO Range Mask] (Core::X86::Msr::IORR_MASK)
     end type MSR_IORR_MASK2_ZEN

     type, public :: MSR_IORR_MASK3_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010019"
        character(len=10)     :: msr_name = "IORR_MASK3"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_001[7...9] [IO Range Mask] (Core::X86::Msr::IORR_MASK)
     end type MSR_IORR_MASK3_ZEN
     



end module mod_zen_msr
