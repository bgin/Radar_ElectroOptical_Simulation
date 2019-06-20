
  include 'GMS_config.fpp'
  
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
    
     use mod_kinds, only : int1, int4, int8b, dp  
     implicit none


     type, public :: MSR_TSC_ZEN
        public
        integer(kind=int4)    :: addr_dec = 10
        character(len=4)      :: addr_hex = "0x10"
        !
        character(len=3)      :: msr_name = "TSC"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        real(kind=dp),       dimension(0:31) :: samp_delta ! time interval in micro-seconds
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:31) :: msr_read
        ! TSC: time stamp counter. Read-write,Volatile. Reset: 0000_0000_0000_0000h.
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_write
        character(len=16),   dimension(0:47) :: msrw_hex
        real(kind=dp),       dimension(0:47) :: samp_delta ! time interval in micro-seconds
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:47) :: msr_read
        ! TSC: time stamp counter. Read-write,Volatile. Reset: 0000_0000_0000_0000h.
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        real(kind=dp),       dimension(0:63) :: samp_delta ! time interval in micro-seconds
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! TSC: time stamp counter. Read-write,Volatile. Reset: 0000_0000_0000_0000h.
#endif
     end type MSR_TSC_ZEN

     type, public :: MSR_APIC_BAR_ZEN
        public
        integer(kind=int4)   :: addr_dec = 27
        character(len=4)     :: addr_hex = "0x1B"
        character(len=8)     :: msr_name = "APIC_BAR"
#if (ZEN_16_CORE)  == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_001B [APIC Base Address] (Core::X86::Msr::APIC_BAR)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        ! MSR0000_001B [APIC Base Address] (Core::X86::Msr::APIC_BAR)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_001B [APIC Base Address] (Core::X86::Msr::APIC_BAR)
#endif
     end type MSR_APIC_BAR_ZEN

     type, public :: MSR_MPERF_ZEN
        public
        integer(kind=int4)   :: addr_dec = 231
        character(len=4)     :: addr_hex = "0xE7"
        character(len=5)     :: msr_name = "MPERF"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        real(kind=dp),       dimension(0:31) :: samp_delta 
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:31) :: msr_read
        !
        ! MPERF: maximum core clocks counter. Read-write,Volatile. Reset: 0000_0000_0000_0000h
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_write
        character(len=16),   dimension(0:47) :: msrw_hex
        real(kind=dp),       dimension(0:47) :: samp_delta 
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:47) :: msr_read
        !
        ! MPERF: maximum core clocks counter. Read-write,Volatile. Reset: 0000_0000_0000_0000h
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        real(kind=dp),       dimension(0:63) :: samp_delta 
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        !
        ! MPERF: maximum core clocks counter. Read-write,Volatile. Reset: 0000_0000_0000_0000h
#endif  
     end type MSR_MPERF_ZEN

     type, public :: MSR_APERF_ZEN
        public
        integer(kind=int4)   :: addr_dec = 232
        character(len=4)     :: addr_hex = "0xE8"
        character(len=5)     :: msr_name = "APERF"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        real(kind=dp),       dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:31) :: msr_read
        ! APERF: actual core clocks counter. Read-write,Volatile. Reset: 0000_0000_0000_0000h
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_write
        character(len=16),   dimension(0:47) :: msrw_hex
        real(kind=dp),       dimension(0:47) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:47) :: msr_read
        ! APERF: actual core clocks counter. Read-write,Volatile. Reset: 0000_0000_0000_0000h
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        real(kind=dp),       dimension(0:63) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! APERF: actual core clocks counter. Read-write,Volatile. Reset: 0000_0000_0000_0000h
#endif
     end type MSR_APERF_ZEN

     type, public :: MSR_MTRR_ZEN
        public
        integer(kind=int4)   :: addr_dec = 254
        character(len=4)     :: addr_hex = "0xFE"
        character(len=4)     :: msr_name = "MTRR"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:16) :: msr_read
        ! MSR0000_00FE [MTRR Capabilities] (Core::X86::Msr::MTRRcap)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:24) :: msr_read
         ! MSR0000_00FE [MTRR Capabilities] (Core::X86::Msr::MTRRcap)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
          ! MSR0000_00FE [MTRR Capabilities] (Core::X86::Msr::MTRRcap)
#endif
     end type MSR_MTRR_ZEN

     type, public :: MSR_MCG_CAP_ZEN
        public
        integer(kind=int4)   :: addr_dec = 377
        character(len=5)     :: addr_hex = "0x179"
        character(len=7)     :: msr_name = "MCG_CAP"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_0179 [Global Machine Check Capabilities] (Core::X86::Msr::MCG_CAP)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        ! MSR0000_0179 [Global Machine Check Capabilities] (Core::X86::Msr::MCG_CAP)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_0179 [Global Machine Check Capabilities] (Core::X86::Msr::MCG_CAP)
#endif
     end type MSR_MCG_CAP_ZEN

     type, public :: MSR_MCG_STAT_ZEN
        public
        integer(kind=int4)   :: addr_dec = 378
        character(len=5)     :: addr_hex = "0x17A"
        character(len=8)     :: msr_name = "MCG_STAT"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSR0000_017A [Global Machine Check Status] (Core::X86::Msr::MCG_STAT)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        integer(kind=int8b), dimension(0:47) :: msr_write
        character(len=16),   dimension(0:47) :: msrw_hex
        ! MSR0000_017A [Global Machine Check Status] (Core::X86::Msr::MCG_STAT)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSR0000_017A [Global Machine Check Status] (Core::X86::Msr::MCG_STAT)
#endif
     end type MSR_MCG_STAT_ZEN

     type, public :: MSR_MCG_CTL_ZEN
        public
        integer(kind=int4)   :: addr_dec = 379
        character(len=5)     :: addr_hex = "0x17B"
        character(len=7)     :: msr_name = "MCG_CTL"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSR0000_017B [Global Machine Check Exception Reporting Control] (Core::X86::Msr::MCG_CTL)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        integer(kind=int8b), dimension(0:47) :: msr_write
        character(len=16),   dimension(0:47) :: msrw_hex
        ! MSR0000_017B [Global Machine Check Exception Reporting Control] (Core::X86::Msr::MCG_CTL)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSR0000_017B [Global Machine Check Exception Reporting Control] (Core::X86::Msr::MCG_CTL)
#endif 
     end type MSR_MCG_CTL_ZEN

     type, public :: MSR_DBG_CTL_ZEN
        public
        integer(kind=int4)   :: addr_dec = 473
        character(len=5)     :: addr_hex = "0x1D9"
        character(len=7)     :: msr_name = "DGB_CTL"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        !MSR0000_01D9 [Debug Control] (Core::X86::Msr::DBG_CTL_MSR)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        !MSR0000_01D9 [Debug Control] (Core::X86::Msr::DBG_CTL_MSR)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        !MSR0000_01D9 [Debug Control] (Core::X86::Msr::DBG_CTL_MSR)
#endif
     end type MSR_DBG_CTL_ZEN

     type, public :: MSR_BR_FROM_ZEN
        public
        integer(kind=int4)   :: addr_dec = 475
        character(len=5)     :: addr_hex = "0x1DB"
        character(len=7)     :: msr_name = "BR_FROM"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_01DB [Last Branch From IP] (Core::X86::Msr::BR_FROM)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        ! MSR0000_01DB [Last Branch From IP] (Core::X86::Msr::BR_FROM)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_01DB [Last Branch From IP] (Core::X86::Msr::BR_FROM)
#endif
     end type MSR_BR_FROM_ZEN

     type, public :: MSR_BR_TO_ZEN
        public
        integer(kind=int4)   :: addr_dec = 476
        character(len=5)     :: addr_hex = "0x1DC"
        character(len=5)     :: msr_name = "BR_TO"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_01DC [Last Branch To IP] (Core::X86::Msr::BR_TO)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        ! MSR0000_01DC [Last Branch To IP] (Core::X86::Msr::BR_TO)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_01DC [Last Branch To IP] (Core::X86::Msr::BR_TO)
#endif
     end type MSR_BR_TO_ZEN

     type, public :: MSR_LAST_EXP_FROM_ZEN
        public
        integer(kind=int4)   :: addr_dec = 477
        character(len=5)     :: addr_hex = "0x1DD"
        character(len=16)    :: msr_name = "LAST_EXP_FROM_IP"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_01DD [Last Exception From IP] (Core::X86::Msr::LastExcpFromIp)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        ! MSR0000_01DD [Last Exception From IP] (Core::X86::Msr::LastExcpFromIp)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_01DD [Last Exception From IP] (Core::X86::Msr::LastExcpFromIp)
#endif        
     end type MSR_LAST_EXP_FROM_ZEN

     type, public :: MSR_LAST_EXP_TO_ZEN
        public
        integer(kind=int4)   :: addr_dec = 478
        character(len=5)     :: addr_hex = "0x1DE"
        character(len=14)    :: msr_name = "LAST_EXP_TO_IP"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_01DE [Last Exception To IP] (Core::X86::Msr::LastExcpToIp)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        ! MSR0000_01DE [Last Exception To IP] (Core::X86::Msr::LastExcpToIp)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_01DE [Last Exception To IP] (Core::X86::Msr::LastExcpToIp)
#endif
     end type MSR_LAST_EXP_TO_ZEN

     type, public :: MSR_MTRR_FIXED_ZEN
        public
        integer(kind=int4)   :: addr_dec = 592
        character(len=5)     :: addr_hex = "0x250"
        character(len=14)    :: msr_name = "MTRR_FIXED"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSR0000_0250 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_64K)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSR0000_0250 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_64K)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_0250 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_64K)
#endif
     end type MSR_MTRR_FIXED_ZEN

     type, public :: MSR_MTRR_FIXED16K_ZEN
        public
        integer(kind=int4)    :: addr_dec = 600
        character(len=5)      :: addr_hex = "0x258"
        character(len=13)     :: msr_name = "MTRR_FIXED16K"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSR0000_0258 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_16K_0)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSR0000_0258 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_16K_0)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_0258 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_16K_0)
#endif
     end type MSR_MTRR_FIXED16K_ZEN

     type, public :: MSR_MTRR_FIXED16K1_ZEN
        public
        integer(kind=int4)    :: addr_dec = 601
        character(len=5)      :: addr_hex = "0x259"
        character(len=14)     :: msr_name = "MTRR_FIXED16K1"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSR0000_0259 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_16K_1)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSR0000_0259 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_16K_1)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_0259 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_16K_1)
#endif
     end type MSR_MTRR_FIXED16K1_ZEN

     type, public :: MSR_MTRR_FIXED4K_ZEN
        public
        integer(kind=int4)    :: addr_dec = 616
        character(len=5)      :: addr_hex = "0x268"
        character(len=12)     :: msr_name = "MTRR_FIXED4K"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSR0000_0268 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_0)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSR0000_0268 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_0)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_0268 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_0)
#endif
     end type MSR_MTRR_FIXED4K_ZEN

     type, public :: MSR_MTRR_FIXED4K1_ZEN
        public
        integer(kind=int4)    :: addr_dec = 617
        character(len=5)      :: addr_hex = "0x269"
        character(len=13)     :: msr_name = "MTRR_FIXED4K1"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSR0000_0269 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_1)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSR0000_0269 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_1)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_0269 [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_1)
#endif
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
#if (ZEN_16_CORE) == 1        
        integer(kind=int8b), dimension(0:15) :: msr_read
        !MSR0000_026B [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_3)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        !MSR0000_026B [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_3)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        !MSR0000_026B [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_3)
#elif
     end type MSR_MTRR_FIXED4K3_ZEN

     type, public :: MSR_MTRR_FIXED4K4_ZEN
        public
        integer(kind=int4)    :: addr_dec = 620
        character(len=5)      :: addr_hex = "0x26C"
        character(len=13)     :: msr_name = "MTRR_FIXED4K4"
#if (ZEN_16_CORE) == 1        
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSR0000_026C [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_4)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSR0000_026C [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_4)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_026C [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_4)
#endif       
     end type MSR_MTRR_FIXED4K4_ZEN

     type, public :: MSR_MTRR_FIXED4K5_ZEN
        public
        integer(kind=int4)    :: addr_dec = 621
        character(len=5)      :: addr_hex = "0x26D"
        character(len=13)     :: msr_name = "MTRR_FIXED4K5"
#if (ZEN_16_CORE) == 1        
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSR0000_026D [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_5)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSR0000_026D [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_5)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_026D [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_5)
#endif        
     end type MSR_MTRR_FIXED4K5_ZEN

     type, public :: MSR_MTRR_FIXED4K6_ZEN
        public
        integer(kind=int4)    :: addr_dec = 622
        character(len=5)      :: addr_hex = "0x26E"
        character(len=13)     :: msr_name = "MTRR_FIXED4K6"
#if (ZEN_16_CORE) == 1        
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSR0000_026E [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_6)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSR0000_026E [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_6)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_026E [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_6)
#endif        
     end type MSR_MTRR_FIXED4K6_ZEN

     type, public :: MSR_MTRR_FIXED4K7_ZEN
        public
        integer(kind=int4)    :: addr_dec = 623
        character(len=5)      :: addr_hex = "0x26F"
        character(len=13)     :: msr_name = "MTRR_FIXED4K7"
#if (ZEN_16_CORE) == 1        
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSR0000_026F [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_7)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSR0000_026F [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_7)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_026F [Fixed-Size MTRRs] (Core::X86::Msr::MtrrFix_4K_7)
#endif        
     end type MSR_MTRR_FIXED4K7_ZEN

     type, public :: MSR_PAT_ZEN
        public
        integer(kind=int4)    :: addr_dec = 631
        character(len=5)      :: addr_hex = "0x277"
        character(len=3)      :: msr_name = "PAT"
#if (ZEN_16_CORE) == 1        
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_0277 [Page Attribute Table] (Core::X86::Msr::PAT)
        ! This register specifies the memory type based on the PAT, PCD, and PWT bits in the virtual address page tables.
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        ! MSR0000_0277 [Page Attribute Table] (Core::X86::Msr::PAT)
        ! This register specifies the memory type based on the PAT, PCD, and PWT bits in the virtual address page tables.
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSR0000_0277 [Page Attribute Table] (Core::X86::Msr::PAT)
        ! This register specifies the memory type based on the PAT, PCD, and PWT bits in the virtual address page tables.
#endif        
     end type MSR_PAT_ZEN

     type, public :: MSR_MTRR_DEFTYPE_ZEN
        public
        integer(kind=int4)    :: addr_dec = 767
        character(len=5)      :: addr_hex = "0x2FF"
        character(len=12)     :: msr_name = "MTRR_DEFTYPE"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSR0000_02FF [MTRR Default Memory Type] (Core::X86::Msr::MTRRdefType)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSR0000_02FF [MTRR Default Memory Type] (Core::X86::Msr::MTRRdefType)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSR0000_02FF [MTRR Default Memory Type] (Core::X86::Msr::MTRRdefType)
#endif        
     end type MSR_MTRR_DEFTYPE_ZEN

     type, public :: MSR_EFER_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0000080"
        character(len=4)      :: msr_name = "EFER"
#if (ZEN_16_CORE) == 1        
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC000_0080 [Extended Feature Enable] (Core::X86::Msr::EFER)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        ! MSRC000_0080 [Extended Feature Enable] (Core::X86::Msr::EFER)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSRC000_0080 [Extended Feature Enable] (Core::X86::Msr::EFER)
#endif        
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
#if (ZEN_16_CORE) == 1
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:31) :: msr_read
        ! MSRC000_00E8 [Read-Only Actual Performance Frequency Clock Count] (Core::X86::Msr::APerfReadOnly)
        ! This register
        ! increments in proportion to the actual number of core clocks cycles while the core is in C0
#elif (ZEN_24_CORE) == 1
         !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:47) :: msr_read
        ! MSRC000_00E8 [Read-Only Actual Performance Frequency Clock Count] (Core::X86::Msr::APerfReadOnly)
        ! This register
        ! increments in proportion to the actual number of core clocks cycles while the core is in C0
#elif (ZEN_32_CORE) == 1
         !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC000_00E8 [Read-Only Actual Performance Frequency Clock Count] (Core::X86::Msr::APerfReadOnly)
        ! This register
        ! increments in proportion to the actual number of core clocks cycles while the core is in C0
#endif
     end type MSR_APERF_READONLY_ZEN

     type, public :: MSR_IRPERF_COUNT_ZEN
        public
        character(len=10)     :: addr_hex = "0xC00000E9"
        character(len=12)     :: msr_name = "IRPERF_COUNT"
#if (ZEN_16_CORE) == 1
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:31) :: msr_read
        ! MSRC000_00E9 [Instructions Retired Performance Count] (Core::X86::Msr::IRPerfCount)
#elif (ZEN_24_CORE) === 1
         !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:47) :: msr_read
        ! MSRC000_00E9 [Instructions Retired Performance Count] (Core::X86::Msr::IRPerfCount)
#elif (ZEN_32_CORE) == 1
         !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC000_00E9 [Instructions Retired Performance Count] (Core::X86::Msr::IRPerfCount)
#endif
     end type MSR_IRPERF_COUNT_ZEN

     type, public :: MSR_TSC_AUX_ZEN
        public
        integer(kind=int4)    :: addr_dec = 257
        character(len=5)      :: addr_hex = "0x101"
        character(len=7)      :: msr_name = "TSC_AUX"
#if (ZEN_16_CORE) == 1
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:31) :: msr_read
        ! MSRC000_0103 [Auxiliary Time Stamp Counter] (Core::X86::Msr::TSC_AUX)
#elif (ZEN_24_CORE) == 1
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:47) :: msr_read
        ! MSRC000_0103 [Auxiliary Time Stamp Counter] (Core::X86::Msr::TSC_AUX)
#elif (ZEN_32_CORE) == 1
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC000_0103 [Auxiliary Time Stamp Counter] (Core::X86::Msr::TSC_AUX)
#endif        
     end type MSR_TSC_AUX_ZEN

     type, public :: MSR_TSC_RATIO_ZEN
        public
        integer(kind=int4)    :: addr_dec = 260
        character(len=5)      :: addr_hex = "0x104"
        character(len=9)      :: msr_name = "TSC_RATIO"
#if (ZEN_16_CORE) == 1
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:31) :: msr_read
        ! MSRC000_0104 [Time Stamp Counter Ratio] (Core::X86::Msr::TscRateMsr)
#elif (ZEN_24_CORE) == 1
        real(kind=dp), dimension(0:47) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:47) :: msr_read
        ! MSRC000_0104 [Time Stamp Counter Ratio] (Core::X86::Msr::TscRateMsr)
#elif (ZEN_32_CORE) == 1
        real(kind=dp), dimension(0:63) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC000_0104 [Time Stamp Counter Ratio] (Core::X86::Msr::TscRateMsr)
#endif
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
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        integer(kind=int8b), dimension(0:15) :: msr_write
        character(len=16),   dimension(0:15) :: msrw_hex
        ! MSRC001_0010 [System Configuration] (Core::X86::Msr::SYS_CFG)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        integer(kind=int8b), dimension(0:23) :: msr_write
        character(len=16),   dimension(0:23) :: msrw_hex
        ! MSRC001_0010 [System Configuration] (Core::X86::Msr::SYS_CFG)
#elif (ZEN_32_CORE)
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_0010 [System Configuration] (Core::X86::Msr::SYS_CFG)
#endif        
     end type MSR_SYS_CFG_ZEN

     type, public :: MSR_HW_CFG_ZEN
        public
        character(len=8)     :: addr_hex = "0xC0010015"
        character(len=6)     :: msr_name = "HW_CFG"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        integer(kind=int8b), dimension(0:15) :: msr_write
        character(len=16),   dimension(0:15) :: msrw_hex
        ! MSRC001_0015 [Hardware Configuration] (Core::X86::Msr::HWCR)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        integer(kind=int8b), dimension(0:23) :: msr_write
        character(len=16),   dimension(0:23) :: msrw_hex
        ! MSRC001_0015 [Hardware Configuration] (Core::X86::Msr::HWCR)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_0015 [Hardware Configuration] (Core::X86::Msr::HWCR)
#endif
     end type MSR_HW_CFG_ZEN

     type, public :: MSR_TOP_MEM_ZEN
        public
        character(len=8)     :: addr_hex = "0xC001001A"
        character(len=7)     :: msr_name = "TOP_MEM"
#if (ZEN_16_CORE) == 1     
        integer(kind=int8b), dimension(0:15) :: msr_read
        integer(kind=int8b), dimension(0:15) :: msr_write
        character(len=16),   dimension(0:15) :: msrw_hex
        ! MSRC001_001A [Top Of Memory] (Core::X86::Msr::TOP_MEM)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        integer(kind=int8b), dimension(0:23) :: msr_write
        character(len=16),   dimension(0:23) :: msrw_hex
        ! MSRC001_001A [Top Of Memory] (Core::X86::Msr::TOP_MEM)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_001A [Top Of Memory] (Core::X86::Msr::TOP_MEM)
#endif
     end type MSR_TOP_MEM_ZEN

     type, public :: MSR_TOP_MEM2_ZEN
        public
        character(len=10)     :: addr_hex = "0xC001001D"
        character(len=8)     :: msr_name = "TOP_MEM2"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        integer(kind=int8b), dimension(0:15) :: msr_write
        character(len=16),   dimension(0:15) :: msrw_hex
        ! MSRC001_001D [Top Of Memory 2] (Core::X86::Msr::TOM2)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        integer(kind=int8b), dimension(0:23) :: msr_write
        character(len=16),   dimension(0:23) :: msrw_hex
        ! MSRC001_001D [Top Of Memory 2] (Core::X86::Msr::TOM2)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_001D [Top Of Memory 2] (Core::X86::Msr::TOM2)
#endif        
     end type MSR_TOP_MEM2_ZEN

     type, public :: MSR_IORR_BASE1_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010016"
        character(len=10)     :: msr_name = "IORR_BASE1"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        integer(kind=int8b), dimension(0:15) :: msr_write
        character(len=16),   dimension(0:15) :: msrw_hex
        ! MSRC001_001[6...8] [IO Range Base] (Core::X86::Msr::IORR_BASE)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        integer(kind=int8b), dimension(0:23) :: msr_write
        character(len=16),   dimension(0:23) :: msrw_hex
        ! MSRC001_001[6...8] [IO Range Base] (Core::X86::Msr::IORR_BASE)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_001[6...8] [IO Range Base] (Core::X86::Msr::IORR_BASE)
#endif        
     end type MSR_IORR_BASE1_ZEN

     type, public :: MSR_IORR_BASE2_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010017"
        character(len=10)     :: msr_name = "IORR_BASE2"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        integer(kind=int8b), dimension(0:15) :: msr_write
        character(len=16),   dimension(0:15) :: msrw_hex
        !MSRC001_001[6...8] [IO Range Base] (Core::X86::Msr::IORR_BASE)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        integer(kind=int8b), dimension(0:23) :: msr_write
        character(len=16),   dimension(0:23) :: msrw_hex
        !MSRC001_001[6...8] [IO Range Base] (Core::X86::Msr::IORR_BASE)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        !MSRC001_001[6...8] [IO Range Base] (Core::X86::Msr::IORR_BASE)
#endif
     end type MSR_IORR_BASE2_ZEN

     type, public :: MSR_IORR_BASE3_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010018"
        character(len=10)     :: msr_name = "IORR_BASE3"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        integer(kind=int8b), dimension(0:15) :: msr_write
        character(len=16),   dimension(0:15) :: msrw_hex
        !MSRC001_001[6...8] [IO Range Base] (Core::X86::Msr::IORR_BASE)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        integer(kind=int8b), dimension(0:23) :: msr_write
        character(len=16),   dimension(0:23) :: msrw_hex
        !MSRC001_001[6...8] [IO Range Base] (Core::X86::Msr::IORR_BASE)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        !MSRC001_001[6...8] [IO Range Base] (Core::X86::Msr::IORR_BASE)
#endif        
     end type MSR_IORR_BASE3_ZEN

     type, public :: MSR_IORR_MASK1_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010017"
        character(len=10)     :: msr_name = "IORR_MASK1"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        integer(kind=int8b), dimension(0:15) :: msr_write
        character(len=16),   dimension(0:15) :: msrw_hex
        ! MSRC001_001[7...9] [IO Range Mask] (Core::X86::Msr::IORR_MASK)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        integer(kind=int8b), dimension(0:23) :: msr_write
        character(len=16),   dimension(0:23) :: msrw_hex
        ! MSRC001_001[7...9] [IO Range Mask] (Core::X86::Msr::IORR_MASK)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_001[7...9] [IO Range Mask] (Core::X86::Msr::IORR_MASK)
#endif
     end type MSR_IORR_MASK1_ZEN

     type, public :: MSR_IORR_MASK2_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010018"
        character(len=10)     :: msr_name = "IORR_MASK2"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        integer(kind=int8b), dimension(0:15) :: msr_write
        character(len=16),   dimension(0:15) :: msrw_hex
        ! MSRC001_001[7...9] [IO Range Mask] (Core::X86::Msr::IORR_MASK)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        integer(kind=int8b), dimension(0:23) :: msr_write
        character(len=16),   dimension(0:23) :: msrw_hex
        ! MSRC001_001[7...9] [IO Range Mask] (Core::X86::Msr::IORR_MASK)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_001[7...9] [IO Range Mask] (Core::X86::Msr::IORR_MASK)
#endif
     end type MSR_IORR_MASK2_ZEN

     type, public :: MSR_IORR_MASK3_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010019"
        character(len=10)     :: msr_name = "IORR_MASK3"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        integer(kind=int8b), dimension(0:15) :: msr_write
        character(len=16),   dimension(0:15) :: msrw_hex
        ! MSRC001_001[7...9] [IO Range Mask] (Core::X86::Msr::IORR_MASK)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        integer(kind=int8b), dimension(0:23) :: msr_write
        character(len=16),   dimension(0:23) :: msrw_hex
        ! MSRC001_001[7...9] [IO Range Mask] (Core::X86::Msr::IORR_MASK)
#elif (ZEN_32_CORES) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_001[7...9] [IO Range Mask] (Core::X86::Msr::IORR_MASK)
#endif        
     end type MSR_IORR_MASK3_ZEN

     type, public :: MSR_PERF_LEGACY_CTL0_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010000"
        character(len=14)     :: msr_name = "PERF_LEGACY_CTL"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_000[0...3] [Performance Event Select [3:0]] (Core::X86::Msr::PERF_LEGACY_CTL)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        integer(kind=int8b), dimension(0:47) :: msr_write
        character(len=16),   dimension(0:47) :: msrw_hex
        ! MSRC001_000[0...3] [Performance Event Select [3:0]] (Core::X86::Msr::PERF_LEGACY_CTL)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_000[0...3] [Performance Event Select [3:0]] (Core::X86::Msr::PERF_LEGACY_CTL)
#endif
     end type MSR_PERF_LEGACY_CTL0_ZEN

     type, public :: MSR_PERF_LEGACY_CTL1_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010001"
        character(len=14)     :: msr_name = "PERF_LEGACY_CTL"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_000[0...3] [Performance Event Select [3:0]] (Core::X86::Msr::PERF_LEGACY_CTL)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        integer(kind=int8b), dimension(0:47) :: msr_write
        character(len=16),   dimension(0:47) :: msrw_hex
        ! MSRC001_000[0...3] [Performance Event Select [3:0]] (Core::X86::Msr::PERF_LEGACY_CTL)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_000[0...3] [Performance Event Select [3:0]] (Core::X86::Msr::PERF_LEGACY_CTL)
#endif
     end type MSR_PERF_LEGACY_CTL1_ZEN

     type, public :: MSR_PERF_LEGACY_CTL2_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010002"
        character(len=14)     :: msr_name = "PERF_LEGACY_CTL"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_000[0...3] [Performance Event Select [3:0]] (Core::X86::Msr::PERF_LEGACY_CTL)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        integer(kind=int8b), dimension(0:47) :: msr_write
        character(len=16),   dimension(0:47) :: msrw_hex
        ! MSRC001_000[0...3] [Performance Event Select [3:0]] (Core::X86::Msr::PERF_LEGACY_CTL)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_000[0...3] [Performance Event Select [3:0]] (Core::X86::Msr::PERF_LEGACY_CTL)
#endif        
     end type MSR_PERF_LEGACY_CTL2_ZEN

     type, public :: MSR_PERF_LEGACY_CTL3_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010003"
        character(len=14)     :: msr_name = "PERF_LEGACY_CTL"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_000[0...3] [Performance Event Select [3:0]] (Core::X86::Msr::PERF_LEGACY_CTL)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        integer(kind=int8b), dimension(0:47) :: msr_write
        character(len=16),   dimension(0:47) :: msrw_hex
        ! MSRC001_000[0...3] [Performance Event Select [3:0]] (Core::X86::Msr::PERF_LEGACY_CTL)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_000[0...3] [Performance Event Select [3:0]] (Core::X86::Msr::PERF_LEGACY_CTL)
#endif
     end type MSR_PERF_LEGACY_CTL3_ZEN

     type, public :: MSR_PERF_LEGACY_CTR0_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010004"
        character(len=14)     :: msr_name = "PER_LEGACY_CTR"
#if (ZEN_16_CORE) == 1        
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:31) :: msr_read
        ! MSRC001_000[4...7] [Performance Event Counter [3:0]] (Core::X86::Msr::PERF_LEGACY_CTR)
#elif (ZEN_24_CORE) == 1
        real(kind=dp), dimension(0:47) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:47) :: msr_read
        ! MSRC001_000[4...7] [Performance Event Counter [3:0]] (Core::X86::Msr::PERF_LEGACY_CTR)
#elif (ZEN_32_CORE) == 1
        real(kind=dp), dimension(0:63) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC001_000[4...7] [Performance Event Counter [3:0]] (Core::X86::Msr::PERF_LEGACY_CTR)
#endif
     end type MSR_PERF_LEGACY_CTR0_ZEN

     type, public :: MSR_PERF_LEGACY_CTR1_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010005"
        character(len=14)     :: msr_name = "PER_LEGACY_CTR"
#if (ZEN_16_CORE) == 1
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:31) :: msr_read
        ! MSRC001_000[4...7] [Performance Event Counter [3:0]] (Core::X86::Msr::PERF_LEGACY_CTR)
#elif (ZEN_24_CORE) == 1
        real(kind=dp), dimension(0:47) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:47) :: msr_read
        ! MSRC001_000[4...7] [Performance Event Counter [3:0]] (Core::X86::Msr::PERF_LEGACY_CTR)
#elif (ZEN_32_CORE) == 1
        real(kind=dp), dimension(0:63) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC001_000[4...7] [Performance Event Counter [3:0]] (Core::X86::Msr::PERF_LEGACY_CTR)
#endif
     end type MSR_PERF_LEGACY_CTR1_ZEN

     type, public :: MSR_PERF_LEGACY_CTR2_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010006"
        character(len=14)     :: msr_name = "PER_LEGACY_CTR"
#if (ZEN_16_CORE) == 1
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:31) :: msr_read
        ! MSRC001_000[4...7] [Performance Event Counter [3:0]] (Core::X86::Msr::PERF_LEGACY_CTR)
#elif (ZEN_24_CORE) == 1
        real(kind=dp), dimension(0:47) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:47) :: msr_read
        ! MSRC001_000[4...7] [Performance Event Counter [3:0]] (Core::X86::Msr::PERF_LEGACY_CTR)
#elif (ZEN_32_CORE) == 1
        real(kind=dp), dimension(0:63) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC001_000[4...7] [Performance Event Counter [3:0]] (Core::X86::Msr::PERF_LEGACY_CTR)
#endif
     end type MSR_PERF_LEGACY_CTR2_ZEN

     type, public :: MSR_PERF_LEGACY_CTR3_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010007"
        character(len=14)     :: msr_name = "PER_LEGACY_CTR"
#if (ZEN_16_CORE) == 1
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:31) :: msr_read
        ! MSRC001_000[4...7] [Performance Event Counter [3:0]] (Core::X86::Msr::PERF_LEGACY_CTR)
#elif (ZEN_24_CORE) == 1
        real(kind=dp), dimension(0:47) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:47) :: msr_read
        ! MSRC001_000[4...7] [Performance Event Counter [3:0]] (Core::X86::Msr::PERF_LEGACY_CTR)
#elif (ZEN_32_CORE) == 1
        real(kind=dp), dimension(0:63) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC001_000[4...7] [Performance Event Counter [3:0]] (Core::X86::Msr::PERF_LEGACY_CTR)
#endif
     end type MSR_PERF_LEGACY_CTR3_ZEN

     type, public :: MSR_MC_EXP_REDIR_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010022"
        character(len=11)    :: msr_name = "MC_EXP_REDIR"
#if (ZEN_16_CORE) == 1        
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_0022 [Machine Check Exception Redirection] (Core::X86::Msr::McExcepRedir)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        integer(kind=int8b), dimension(0:47) :: msr_write
        character(len=16),   dimension(0:47) :: msrw_hex
        ! MSRC001_0022 [Machine Check Exception Redirection] (Core::X86::Msr::McExcepRedir)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_0022 [Machine Check Exception Redirection] (Core::X86::Msr::McExcepRedir)
#endif
     end type MSR_MC_EXP_REDIR_ZEN

     type, public :: MSR_PROC_NAME_STRING0_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010030"
        character(len=16)    :: msr_name = "PROC_NAME_STRING"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#endif
     end type MSR_PROC_NAME_STRING0_ZEN

     type, public :: MSR_PROC_NAME_STRING1_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010031"
        character(len=16)    :: msr_name = "PROC_NAME_STRING"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#endif
     end type MSR_PROC_NAME_STRING1_ZEN

     type, public :: MSR_PROC_NAME_STRING2_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010032"
        character(len=16)    :: msr_name = "PROC_NAME_STRING"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#endif
     end type MSR_PROC_NAME_STRING2_ZEN

     type, public :: MSR_PROC_NAME_STRING3_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010033"
        character(len=16)    :: msr_name = "PROC_NAME_STRING"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#endif
     end type MSR_PROC_NAME_STRING3_ZEN

     type, public :: MSR_PROC_NAME_STRING4_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010034"
        character(len=16)    :: msr_name = "PROC_NAME_STRING"
#if (ZEN_16_CORE) == 1        
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#endif
     end type MSR_PROC_NAME_STRING4_ZEN

     type, public :: MSR_PROC_NAME_STRING5_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010035"
        character(len=16)    :: msr_name = "PROC_NAME_STRING"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC001_003[0...5] [Processor Name String] (Core::X86::Msr::ProcNameString)
#endif
     end type MSR_PROC_NAME_STRING5_ZEN

     type, public :: MSR_MMIO_CFG_BASE_ADDR_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010058"
        character(len=18)    :: msr_name = "MMIO_CFG_BASE_ADDR"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        integer(kind=int8b), dimension(0:15) :: msr_write
        character(len=16),   dimension(0:15) :: msrw_hex
        ! MSRC001_0058 [MMIO Configuration Base Address] (Core::X86::Msr::MmioCfgBaseAddr)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        integer(kind=int8b), dimension(0:23) :: msr_write
        character(len=16),   dimension(0:23) :: msrw_hex
        ! MSRC001_0058 [MMIO Configuration Base Address] (Core::X86::Msr::MmioCfgBaseAddr)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_0058 [MMIO Configuration Base Address] (Core::X86::Msr::MmioCfgBaseAddr)
#endif
     end type MSR_MMIO_CFG_BASE_ADDR_ZEN

     type, public :: MSR_INT_PENDING_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010055"
        character(len=10)    :: msr_name = "INT_PENDING"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:15) :: msr_read
        ! MSRC001_0055 [Reserved.] (Core::X86::Msr::IntPend)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:23) :: msr_read
        ! MSRC001_0055 [Reserved.] (Core::X86::Msr::IntPend)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC001_0055 [Reserved.] (Core::X86::Msr::IntPend)
#endif
     end type MSR_INT_PENDING_ZEN

     type, public :: MSR_TRIG_IO_CYCLE_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0010056"
        character(len=13)     :: msr_name = "TRIG_IO_CYCLE"
#if (ZEN_16_CORE) == 1
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_0056 [SMI Trigger IO Cycle] (Core::X86::Msr::SmiTrigIoCycle)
#elif (ZEN_24_CORE) == 1
        integer(kind=int8b), dimension(0:47) :: msr_read
        integer(kind=int8b), dimension(0:47) :: msr_write
        character(len=16),   dimension(0:47) :: msrw_hex
        ! MSRC001_0056 [SMI Trigger IO Cycle] (Core::X86::Msr::SmiTrigIoCycle)
#elif (ZEN_32_CORE) == 1
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_0056 [SMI Trigger IO Cycle] (Core::X86::Msr::SmiTrigIoCycle)
#endif
     end type MSR_TRIG_IO_CYCLE_ZEN

     type, public :: MSR_MMIO_CFG_BASE_ADDR_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010058"
        character(len=18)    :: msr_name = "MMIO_CFG_BASE_ADDR"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_0058 [MMIO Configuration Base Address] (Core::X86::Msr::MmioCfgBaseAddr)
     end type MSR_MMIO_CFG_BASE_ADDR_ZEN

     type, public :: MSR_PSTATE_CUR_LIMIT_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010061"
        character(len=15)    :: msr_name = "PSTATE_CUR_LIMIT"
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC001_0061 [P-state Current Limit] (Core::X86::Msr::PStateCurLim)
     end type MSR_PSTATE_CUR_LIMIT_ZEN

     type, public :: MSR_PSTATE_CTL_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010062"
        character(len=10)    :: msr_name = "PSTATE_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_0062 [P-state Control] (Core::X86::Msr::PStateCtl)
     end type MSR_PSTATE_CTL_ZEN

     type, public :: MSR_PSTATE_STAT_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010063"
        character(len=11)    :: msr_name = "PSTATE_STAT"
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC001_0063 [P-state Status] (Core::X86::Msr::PStateStat)
     end type MSR_PSTATE_STAT_ZEN

     type, public :: MSR_PSTATE_DEF0_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010064"
        character(len=10)    :: msr_name = "PSTATE_DEF"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_006[4...B] [P-state [7:0]] (Core::X86::Msr::PStateDef)
     end type MSR_PSTATE_DEF0_ZEN

     type, public :: MSR_PSTATE_DEF1_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010065"
        character(len=10)    :: msr_name = "PSTATE_DEF"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_006[4...B] [P-state [7:0]] (Core::X86::Msr::PStateDef)
     end type MSR_PSTATE_DEF1_ZEN

     type, public :: MSR_PSTATE_DEF2_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010066"
        character(len=10)    :: msr_name = "PSTATE_DEF"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_006[4...B] [P-state [7:0]] (Core::X86::Msr::PStateDef)
     end type MSR_PSTATE_DEF2_ZEN

     type, public :: MSR_PSTATE_DEF3_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010067"
        character(len=10)    :: msr_name = "PSTATE_DEF"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_006[4...B] [P-state [7:0]] (Core::X86::Msr::PStateDef)
     end type MSR_PSTATE_DEF3_ZEN

     type, public :: MSR_PSTATE_DEF4_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010068"
        character(len=10)    :: msr_name = "PSTATE_DEF"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_006[4...B] [P-state [7:0]] (Core::X86::Msr::PStateDef)
     end type MSR_PSTATE_DEF4_ZEN

     type, public :: MSR_PSTATE_DEF5_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010069"
        character(len=10)    :: msr_name = "PSTATE_DEF"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_006[4...B] [P-state [7:0]] (Core::X86::Msr::PStateDef)
     end type MSR_PSTATE_DEF5_ZEN

     type, public :: MSR_PSTATE_DEF6_ZEN
        public
        character(len=10)    :: addr_hex = "0xC001006A"
        character(len=10)    :: msr_name = "PSTATE_DEF"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_006[4...B] [P-state [7:0]] (Core::X86::Msr::PStateDef)
     end type MSR_PSTATE_DEF6_ZEN

     type, public :: MSR_PSTATE_DEF7_ZEN
        public
        character(len=10)    :: addr_hex = "0xC001006B"
        character(len=10)    :: msr_name = "PSTATE_DEF"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_006[4...B] [P-state [7:0]] (Core::X86::Msr::PStateDef)
     end type MSR_PSTATE_DEF7_ZEN

     type, public :: MSR_CSTATE_BASE_ADDRESS_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010073"
        character(len=19)    :: msr_name = "CSTATE_BASE_ADDRESS"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_0073 [C-state Base Address] (Core::X86::Msr::CStateBaseAddr)
     end type MSR_CSTATE_BASE_ADDRESS_ZEN

     type, public :: MSR_CPU_WDT_CFG_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010074"
        character(len=11)    :: msr_name = "CPU_WDT_CFG"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_0074 [CPU Watchdog Timer] (Core::X86::Msr::CpuWdtCfg)
     end type MSR_CPU_WDT_CFG_ZEN

     type, public :: MSR_SMM_BASE_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010111"
        character(len=8)     :: msr_name = "SMM_BASE"
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC001_0111 [SMM Base Address] (Core::X86::Msr::SMM_BASE)
     end type MSR_SMM_BASE_ZEN

     type, public :: MSR_SMM_ADDR_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010112"
        character(len=8)     :: msr_name = "SMM_ADDR"
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC001_0112 [SMM TSeg Base Address] (Core::X86::Msr::SMMAddr)
     end type MSR_SMM_ADDR_ZEN

     type, public :: MSR_SMM_MASK_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010113"
        character(len=8)     :: msr_name = "SMM_MASK"
        integer(kind=int8b), dimension(0:31) :: msr_read
        ! MSRC001_0113 [SMM TSeg Mask] (Core::X86::Msr::SMMMask)
     end type MSR_SMM_MASK_ZEN

     type, public :: MSR_SMM_CTL_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010116"
        character(len=7)     :: msr_name = "SMM_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSRC001_0116 [SMM Control] (Core::X86::Msr::SMM_CTL)
     end type MSR_SMM_CTL_ZEN

     type, public :: MSR_LOCAL_SMI_STAT_ZEN
        public
        character(len=10)    :: addr_hex = "0xC001011A"
        character(len=13)    :: msr_name = "LOCAL_SMI_STAT"
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSRC001_011A [Local SMI Status] (Core::X86::Msr::LocalSmiStatus)
     end type MSR_LOCAL_SMI_STAT_ZEN

     type, public :: MSR_PERF_CTL0_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010200"
        character(len=8)     :: msr_name = "PERF_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_020[0...A] [Performance Event Select [5:0]] (Core::X86::Msr::PERF_CTL)
     end type MSR_PERF_CTL0_ZEN

     type, public :: MSR_PERF_CTL2_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010202"
        character(len=8)     :: msr_name = "PERF_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_020[0...A] [Performance Event Select [5:0]] (Core::X86::Msr::PERF_CTL)
     end type MSR_PERF_CTL2_ZEN

     type, public :: MSR_PERF_CTL4_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010204"
        character(len=8)     :: msr_name = "PERF_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_020[0...A] [Performance Event Select [5:0]] (Core::X86::Msr::PERF_CTL)
     end type MSR_PERF_CTL4_ZEN

     type, public :: MSR_PERF_CTL6_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010206"
        character(len=8)     :: msr_name = "PERF_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_020[0...A] [Performance Event Select [5:0]] (Core::X86::Msr::PERF_CTL)
     end type MSR_PERF_CTL6_ZEN

     type, public :: MSR_PERF_CTL8_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010208"
        character(len=8)     :: msr_name = "PERF_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_020[0...A] [Performance Event Select [5:0]] (Core::X86::Msr::PERF_CTL)
     end type MSR_PERF_CTL8_ZEN

     type, public :: MSR_PERF_CTL10_ZEN
        public
        character(len=10)    :: addr_hex = "0xC001020A"
        character(len=8)     :: msr_name = "PERF_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_020[0...A] [Performance Event Select [5:0]] (Core::X86::Msr::PERF_CTL)
     end type MSR_PERF_CTL10_ZEN

     type, public :: MSR_PERF_CTR1_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010201"
        character(len=8)     :: msr_name = "PERF_CTR"
         real(kind=dp), dimension(0:63) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC001_020[1...B] [Performance Event Counter [5:0]] (Core::X86::Msr::PERF_CTR)
     end type MSR_PERF_CTR1_ZEN

     type, public :: MSR_PERF_CTR3_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010203"
        character(len=8)     :: msr_name = "PERF_CTR"
         real(kind=dp), dimension(0:63) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC001_020[1...B] [Performance Event Counter [5:0]] (Core::X86::Msr::PERF_CTR)
     end type MSR_PERF_CTR3_ZEN

     type, public :: MSR_PERF_CTR5_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010205"
        character(len=8)     :: msr_name = "PERF_CTR"
         real(kind=dp), dimension(0:63) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC001_020[1...B] [Performance Event Counter [5:0]] (Core::X86::Msr::PERF_CTR)
     end type MSR_PERF_CTR5_ZEN

     type, public :: MSR_PERF_CTR7_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010207"
        character(len=8)     :: msr_name = "PERF_CTR"
         real(kind=dp), dimension(0:63) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:63) :: msr_read
        ! MSRC001_020[1...B] [Performance Event Counter [5:0]] (Core::X86::Msr::PERF_CTR)
     end type MSR_PERF_CTR7_ZEN

     type, public :: MSR_L3_PMC_CFG0_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010230"
        character(len=9)     :: msr_name = "L3_PMC_CFG"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        ! MSRC001_023[0...A] [L3 Performance Event Select [5:0]] (Core::X86::Msr::ChL3PmcCfg)
     end type MSR_L3_PMC_CFG0_ZEN

     type, public :: MSR_L3_PMC_CFG2_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010232"
        character(len=9)     :: msr_name = "L3_PMC_CFG"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        ! MSRC001_023[0...A] [L3 Performance Event Select [5:0]] (Core::X86::Msr::ChL3PmcCfg)
     end type MSR_L3_PMC_CFG2_ZEN

     type, public :: MSR_L3_PMC_CFG4_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010234"
        character(len=9)     :: msr_name = "L3_PMC_CFG"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        ! MSRC001_023[0...A] [L3 Performance Event Select [5:0]] (Core::X86::Msr::ChL3PmcCfg)
     end type MSR_L3_PMC_CFG4_ZEN

     type, public :: MSR_L3_PMC_CFG6_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010236"
        character(len=9)     :: msr_name = "L3_PMC_CFG"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        ! MSRC001_023[0...A] [L3 Performance Event Select [5:0]] (Core::X86::Msr::ChL3PmcCfg)
     end type MSR_L3_PMC_CFG6_ZEN

     type, public :: MSR_L3_PMC_CFG8_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010238"
        character(len=9)     :: msr_name = "L3_PMC_CFG"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        ! MSRC001_023[0...A] [L3 Performance Event Select [5:0]] (Core::X86::Msr::ChL3PmcCfg)
     end type MSR_L3_PMC_CFG8_ZEN

     type, public :: MSR_L3_PMC_CFG10_ZEN
        public
        character(len=10)    :: addr_hex = "0xC001023A"
        character(len=9)     :: msr_name = "L3_PMC_CFG"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        ! MSRC001_023[0...A] [L3 Performance Event Select [5:0]] (Core::X86::Msr::ChL3PmcCfg)
     end type MSR_L3_PMC_CFG10_ZEN

     type, public :: MSR_L3_PMC1_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010231"
        character(len=6)     :: msr_name = "L3_PMC"
         real(kind=dp)       :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000) :: msr_read
        ! MSRC001_023[1...B] [L3 Performance Event Counter [5:0]] (Core::X86::Msr::ChL3Pmc)
     end type MSR_L3_PMC1_ZEN

     type, public :: MSR_L3_PMC3_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010233"
        character(len=6)     :: msr_name = "L3_PMC"
         real(kind=dp)       :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000) :: msr_read
        ! MSRC001_023[1...B] [L3 Performance Event Counter [5:0]] (Core::X86::Msr::ChL3Pmc)
     end type MSR_L3_PMC3_ZEN

     type, public :: MSR_L3_PMC5_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010235"
        character(len=6)     :: msr_name = "L3_PMC"
         real(kind=dp)       :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000) :: msr_read
        ! MSRC001_023[1...B] [L3 Performance Event Counter [5:0]] (Core::X86::Msr::ChL3Pmc)
     end type MSR_L3_PMC5_ZEN

     type, public :: MSR_L3_PMC7_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010237"
        character(len=6)     :: msr_name = "L3_PMC"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000) :: msr_read
        ! MSRC001_023[1...B] [L3 Performance Event Counter [5:0]] (Core::X86::Msr::ChL3Pmc)
     end type MSR_L3_PMC7_ZEN

     type, public :: MSR_L3_PMC9_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010239"
        character(len=6)     :: msr_name = "L3_PMC"
         real(kind=dp)       :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000) :: msr_read
        ! MSRC001_023[1...B] [L3 Performance Event Counter [5:0]] (Core::X86::Msr::ChL3Pmc)
     end type MSR_L3_PMC9_ZEN

     type, public :: MSR_L3_PMC11_ZEN
        public
        character(len=10)    :: addr_hex = "0xC001023B"
        character(len=6)     :: msr_name = "L3_PMC"
         real(kind=dp)       :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000) :: msr_read
        ! MSRC001_023[1...B] [L3 Performance Event Counter [5:0]] (Core::X86::Msr::ChL3Pmc)
     end type MSR_L3_PMC11_ZEN

     type, public :: MSR_RAPL_PWR_UNIT_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0010299"
        character(len=13)    :: msr_name = "RAPL_PWR_UNIT"
        integer(kind=int8b)  :: msr_read
        ! MSRC001_0299 [RAPL Power Unit] (Core::X86::Msr::RAPL_PWR_UNIT)
     end type MSR_RAPL_PWR_UNIT_ZEN

     type, public :: MSR_CORE_ENERGY_STAT_ZEN
        public
        character(len=10)    :: addr_hex = "0xC001029A"
        character(len=16)    :: msr_name = "CORE_ENERGY_STAT"
         real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000,0:31) :: msr_read
        ! MSRC001_029A [Core Energy Status] (Core::X86::Msr::CORE_ENERGY_STAT)
     end type MSR_CORE_ENERGY_STAT_ZEN

     type, public :: MSR_PKG_ENERGY_STAT_ZEN
        public
        character(len=10)    :: addr_hex = "0xC001029B"
        character(len=15)    :: msr_name = "PKG_ENERGY_STAT"
         real(kind=dp)       :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_read
        integer(kind=int8b), dimension(1000) :: msr_read
        ! MSRC001_029B [Package Energy Status] (Core::X86::Msr::PKG_ENERGY_STAT)
     end type MSR_PKG_ENERGY_STAT_ZEN

     type, public :: MSR_CPUID_7_FEATURES_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0011002"
        character(len=16)    :: msr_name = "CPUID_7_FEATURES"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1002 [CPUID Features for CPUID Fn00000007_E[A,B]X] (Core::X86::Msr::CPUID_7_Features)
     end type MSR_CPUID_7_FEATURES_ZEN

     type, public :: MSR_CPUID_PWR_THERM_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0011003"
        character(len=15)    :: msr_name = "CPUID_PWR_THERM"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1003 [Thermal and Power Management CPUID Features] (Core::X86::Msr::CPUID_PWR_THERM)
     end type MSR_CPUID_PWR_THERM_ZEN

     type, public :: MSR_CPUID_FEATURES_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0011004"
        character(len=14)    :: msr_name = "CPUID_FEATURES"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1004 [CPUID Features for CPUID Fn00000001_E[C,D]X] (Core::X86::Msr::CPUID_Features)
     end type MSR_CPUID_FEATURES_ZEN

     type, public :: MSR_CPUID_EXT_FEATURES_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0011005"
        character(len=17)     :: msr_name = "CPUID_EXT_FEATURES"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1005 [CPUID Features for CPUID Fn80000001_E[C,D]X] (Core::X86::Msr::CPUID_ExtFeatures)
     end type MSR_CPUID_EXT_FEATURES_ZEN

     type, public :: MSR_DR1_ADDR_MASK_ZEN
        public
        character(len=10)     :: addr_hex = "0xC00011019"
        character(len=13)     :: msr_name = "DR1_ADDR_MASK"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1019 [Address Mask For DR1 Breakpoint] (Core::X86::Msr::DR1_ADDR_MASK)
     end type MSR_DR1_ADDR_MASK_ZEN

     type, public :: MSR_DR2_ADDR_MASK_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0001101A"
        character(len=13)     :: msr_name = "DR2_ADDR_MASK"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1019 [Address Mask For DR1 Breakpoint] (Core::X86::Msr::DR1_ADDR_MASK)
     end type MSR_DR2_ADDR_MASK_ZEN

     type, public :: MSR_DR3_ADDR_MASK_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0001101B"
        character(len=13)     :: msr_name = "DR1_ADDR_MASK"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1019 [Address Mask For DR1 Breakpoint] (Core::X86::Msr::DR1_ADDR_MASK)
     end type MSR_DR3_ADDR_MASK_ZEN

     type, public :: MSR_TW_CFG_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0011023"
        character(len=6)      :: msr_name = "TW_CFG"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_1023 [Table Walker Configuration] (Core::X86::Msr::TW_CFG)
     end type MSR_TW_CFG_ZEN

     type, public :: MSR_IBS_FETCH_CTL_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0011030"
        character(len=14)     :: msr_name = "IBS_FETCH_CTL"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        !MSRC001_1030 [IBS Fetch Control] (Core::X86::Msr::IBS_FETCH_CTL)
     end type MSR_IBS_FETCH_CTL_ZEN

     type, public :: MSR_IBS_FETCH_LINADDR_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0011031"
        character(len=16)     :: msr_name = "IBS_FETCH_LINADDR"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_1031 [IBS Fetch Linear Address] (Core::X86::Msr::IBS_FETCH_LINADDR)
     end type MSR_IBS_FETCH_LINADDR_ZEN

     type, public :: MSR_IBS_FETCH_PHYSADDR_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0011032"
        character(len=17)     :: msr_name = "IBS_FETCH_PHYSADDR"
        integer(kind=int8b), dimension(0:31) :: msr_read
        integer(kind=int8b), dimension(0:31) :: msr_write
        character(len=16),   dimension(0:31) :: msrw_hex
        ! MSRC001_1032 [IBS Fetch Physical Address] (Core::X86::Msr::IBS_FETCH_PHYSADDR)
     end type MSR_IBS_FETCH_PHYSADDR_ZEN

     type, public :: MSR_IBS_OP_CTL_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0011033"
        character(len=10)     :: msr_name = "IBS_OP_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        !MSRC001_1033 [IBS Execution Control] (Core::X86::Msr::IBS_OP_CTL)
     end type MSR_IBS_OP_CTL_ZEN

     type, public :: MSR_IBS_OP_RIP_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0011034"
        character(len=10)     :: msr_name = "IBS_OP_RIP"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1034 [IBS Op Logical Address] (Core::X86::Msr::IBS_OP_RIP)
     end type MSR_IBS_OP_RIP_ZEN

     type, public :: MSR_IBS_OP_DATA_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0011035"
        character(len=11)    :: msr_name = "IBS_OP_DATA"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1035 [IBS Op Data] (Core::X86::Msr::IBS_OP_DATA)
     end type MSR_IBS_OP_DATA_ZEN

     type, public :: MSR_IBS_OP_DATA2_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0011036"
        character(len=12)    :: msr_name = "IBS_OP_DATA2"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1035 [IBS Op Data] (Core::X86::Msr::IBS_OP_DATA)
     end type MSR_IBS_OP_DATA2_ZEN

     type, public :: MSR_IBS_OP_DATA3_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0011037"
        character(len=11)    :: msr_name = "IBS_OP_DATA3"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1035 [IBS Op Data] (Core::X86::Msr::IBS_OP_DATA)
     end type MSR_IBS_OP_DATA3_ZEN

     type, public :: MSR_IBS_DC_LINADDR_ZEN
        public
        character(len=10)    :: addr_hex = "0xC0011038"
        character(len=15)    :: msr_name = "IBS_DC_LINADDR"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1038 [IBS DC Linear Address] (Core::X86::Msr::IBS_DC_LINADDR)
     end type MSR_IBS_DC_LINADDR_ZEN

     type, public :: MSR_IBS_DC_PHYSADDR_ZEN
        public
        character(len=10)     :: addr_hex = "0xC0011039"
        character(len=15)     :: msr_name = "IBS_DC_PHYSADDR"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_1039 [IBS DC Physical Address] (Core::X86::Msr::IBS_DC_PHYSADDR)
     end type MSR_IBS_DC_PHYSADDR_ZEN

     type, public :: MSR_IBS_CTL_ZEN
        public
        character(len=10)     :: addr_hex = "0xC001103A"
        character(len=7)      :: msr_name = "IBS_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSRC001_103A [IBS Control] (Core::X86::Msr::IBS_CTL)
     end type MSR_IBS_CTL_ZEN

     type, public :: MSR_BP_IBSTGT_RIP_ZEN
        public
        character(len=10)     :: addr_hex = "0xC001103B"
        character(len=14)     :: msr_name = "BP_IBSTGT_RIP"
        integer(kind=int8b), dimension(0:63) :: msr_read
        integer(kind=int8b), dimension(0:63) :: msr_write
        character(len=16),   dimension(0:63) :: msrw_hex
        ! MSRC001_103B [IBS Branch Target Address] (Core::X86::Msr::BP_IBSTGT_RIP)
     end type MSR_BP_IBSTGT_RIP_ZEN

     type, public :: MSR_IC_IBS_EXTD_CTL_ZEN
        public
        character(len=10)     :: addr_hex = "0xC001103C"
        character(len=15)     :: msr_name = "IC_IBS_EXTD_CTL"
        integer(kind=int8b), dimension(0:63) :: msr_read
        ! MSRC001_103C [IBS Fetch Control Extended] (Core::X86::Msr::IC_IBS_EXTD_CTL)
     end type MSR_IC_IBS_EXTD_CTL_ZEN

     type, public :: PMC_FPU_PIPE_ASSIGNMENT_ZEN
        public
        character(len=5)      :: event = "0x000"
        character(len=20)     :: event_name = "FPU_PIPE_ASSIGNMENT"
         real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        !PMCx000 [FPU Pipe Assignment] (Core::X86::Pmc::Core::FpuPipeAssignment)
     end type PMC_FPU_PIPE_ASSIGNMENT_ZEN

     type, public :: PMC_FP_SCHED_EMPTY_ZEN
        public
        character(len=5)      :: event = "0x001"
        character(len=14)     :: event_name = "FP_SCHED_EMPTY"
         real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        !PMCx001 [FP Scheduler Empty] (Core::X86::Pmc::Core::FpSchedEmpty)
     end type PMC_FP_SCHED_EMPTY_ZEN

     type, public :: PMC_FP_RET_X86_FPOPS_ZEN
        public
        character(len=5)      :: event = "0x002"
        character(len=16)     :: event_name = "FP_RET_X86_FPOPS"
         real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx002 [Retired x87 Floating Point Operations] (Core::X86::Pmc::Core::FpRetx87FpOps)
     end type PMC_FP_RET_X86_FPOPS_ZEN

     type, public :: PMC_FP_RET_SSE_AVX_OPS_ZEN
        public
        character(len=5)      :: event = "0x003"
        character(len=18)     :: event_name = "FP_RET_SSE_AVX_OPS"
         real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx003 [Retired SSE/AVX Operations] (Core::X86::Pmc::Core::FpRetSseAvxOps)
     end type PMC_FP_RET_SSE_AVX_OPS_ZEN

     type, public :: PMC_FP_NUM_MOV_ELIM_SCALOP_ZEN
        public
        character(len=5)       :: event = "0x004"
        character(len=22)      :: event_name = "FP_NUM_MOV_ELIM_SCALOP"
         real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx004 [Number of Move Elimination and Scalar Op Optimization]
        !(Core::X86::Pmc::Core::FpNumMovElimScalOp)
     end type PMC_FP_NUM_MOV_ELIM_SCALOP_ZEN

     type, public :: PMC_FP_RETIRE_SEROPS_ZEN
        public
        character(len=5)       :: event = "0x005"
        character(len=16)      :: event_name = "FP_RETIRE_SEROPS"
         real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        !PMCx005 [Retired Serializing Ops] (Core::X86::Pmc::Core::FpRetiredSerOps)
     end type PMC_FP_RETIRE_SEROPS_ZEN

     type, public :: PMC_LS_BAD_STATUS2_ZEN
        public
        character(len=5)       :: event = "0x24"
        character(len=14)      :: event_name = "LS_BAD_STATUS2"
         real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx024 [Bad Status 2] (Core::X86::Pmc::Core::LsBadStatus2)
     end type PMC_LS_BAD_STATUS2_ZEN

     type, public :: PMC_LS_LOCKS_ZEN
        public
        character(len=5)       :: event = "0x25"
        character(len=8)       :: event_name = "LS_LOCKS"
         real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx025 [Locks] (Core::X86::Pmc::Core::LsLocks)
     end type PMC_LS_LOCKS_ZEN

     type, public :: PMC_LS_RET_CLFLUSH_ZEN
        public
        character(len=5)       :: event = "0x26"
        character(len=14)      :: event_name = "LS_RET_CLFLUSH"
         real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx026 [Retired CLFLUSH Instructions] (Core::X86::Pmc::Core::LsRetClClush)
     end type PMC_LS_RET_CLFLUSH_ZEN

     type, public :: PMC_LS_RET_CPUID_ZEN
        public
        character(len=5)       :: event = "0x27"
        character(len=12)      :: event_name = "LS_RET_CPUID"
         real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! MCx027 [Retired CPUID Instructions] (Core::X86::Pmc::Core::LsRetCpuid)
     end type PMC_LS_RET_CPUID_ZEN

     type, public :: PMC_LS_DISPATCH_ZEN
        public
        character(len=5)       :: event = "0x29"
        character(len=11)      :: event_name = "LS_DISPATCH"
         real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        !PMCx029 [LS Dispatch] (Core::X86::Pmc::Core::LsDispatch)
     end type PMC_LS_DISPATCH_ZEN

     type, public :: PMC_LS_SMI_RX_ZEN
        public
        character(len=5)       :: event = "0x2B"
        character(len=9)       :: event_name = "LS_SMI_RX"
         real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx02B [SMIs Received] (Core::X86::Pmc::Core::LsSmiRx)
     end type PMC_LS_SMI_RX_ZEN

     type, public :: PMC_LS_STLF_ZEN
        public
        character(len=5)       :: event = "0x35"
        character(len=7)       :: event_name = "LS_STLF"
         real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx035 [Store to Load Forward] (Core::X86::Pmc::Core::LsSTLF)
     end type PMC_LS_STLF_ZEN

     type, public :: PMC_LS_ST_COMMIT_CANCELS2_ZEN
        public
        character(len=5)       :: event = "0x37"
        character(len=21)      :: event_name = "LS_ST_COMMIT_CANCLES2"
         real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx037 [Store Commit Cancels 2] (Core::X86::Pmc::Core::LsStCommitCancel2)
     end type PMC_LS_ST_COMMIT_CANCELS2_ZEN

     type, public :: PMC_LS_DC_ACCESSES_ZEN
        public
        character(len=5)       :: event = "0x40"
        character(len=14)      :: event_name = "LS_DC_ACCESSES"
         real(kind=dp), dimension(0:31) :: samp_delta
          !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx040 [Data Cache Accesses] (Core::X86::Pmc::Core::LsDcAccesses)
     end type PMC_LS_DC_ACCESSES_ZEN

     type, public :: PMC_LS_REFILLS_FROM_SYS_ZEN
        public
        character(len=5)       :: event = "0x43"
        character(len=18)      :: event_name = "LS_REFILLS_FROM_SYS"
         real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx043 [Data Cache Refills from System] (Core::X86::Pmc::Core::LsRefillsFromSys)
     end  type  PMC_LS_REFILLS_FROM_SYS_ZEN

     type, public :: PMC_LS_L1_DTLB_MISS_ZEN
        public
        character(len=5)      :: event = "0x45"
        character(len=14)     :: event_name = "LS_L1_DTLB_MISS"
         real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        !PMCx045 [L1 DTLB Miss] (Core::X86::Pmc::Core::LsL1DTlbMiss)
     end type PMC_LS_L1_DTLB_MISS_ZEN

     type, public :: PMC_LS_TABLE_WALKER_ZEN
        public
        character(len=5)      :: event = "0x46"
        character(len=15)     :: event_name = "LS_TABLE_WALKER"
         real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        !PMCx046 [Tablewalker allocation] (Core::X86::Pmc::Core::LsTablewalker)
     end type PMC_LS_TABLE_WALKER_ZEN

     type, public :: PMC_LS_MISALIGN_ACCESS_ZEN
        public
        character(len=5)      :: event = "0x47"
        character(len=16)     :: event_name = "LS_MISALIGN_ACCESS"
         real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx047 [Misaligned loads] (Core::X86::Pmc::Core::LsMisalAccesses)
     end type PMC_LS_MISALIGN_ACCESS_ZEN

     type, public :: PMC_LS_PREF_INSTR_DISPATCH_ZEN
        public
        character(len=5)      :: event = "0x4B"
        character(len=21)     :: event_name = "LS_PERF_INSTR_DISPATCH"
         real(kind=dp), dimension(0:31) :: samp_delta
          !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx04B [Prefetch Instructions Dispatched] (Core::X86::Pmc::Core::LsPrefInstrDisp)
     end type PMC_LS_PREF_INSTR_DISPATCH_ZEN

     type, public :: PMC_LS_INEF_SW_PREF_ZEN
        public
        character(len=5)       :: event = "0x52"
        character(len=15)      :: event_name = "LS_INEF_SW_PREF"
         real(kind=dp), dimension(0:31) :: samp_delta
          !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx052 [Ineffective Software Prefetchs] (Core::X86::Pmc::Core::LsInefSwPref)
     end type PMC_LS_INEF_SW_PREF_ZEN

     type, public :: PMC_LS_SW_PREF_DC_FILLS_ZEN
        public
        character(len=5)       :: event = "0x59"
        character(len=19)      :: event_name = "LS_SW_PREF_DC_FILLS"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx059 [Software Prefetch Data Cache Fills] (Core::X86::Pmc::Core::LsSwPfDcFills)
     end type PMC_LS_SW_PREF_DC_FILLS_ZEN

     type, public :: PMC_LS_HW_PF_DC_FILLS_ZEN
        public
        character(len=5)       :: event = "0x5A"
        character(len=17)      :: event_name = "LS_HW_PF_DC_FILLS"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx05A [Hardware Prefetch Data Cache Fills] (Core::X86::Pmc::Core::LsHwPfDcFills)
     end type PMC_LS_HW_PF_DC_FILLS_ZEN

     type, public :: PMC_TW_DC_FILLS_ZEN
        public
        character(len=5)        :: event = "0x5B"
        character(len=11)       :: event_name = "TW_DC_FILLS"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx05B [Table Walker Data Cache Fills by Data Source] (Core::X86::Pmc::Core::LsTwDcFills)
     end type PMC_TW_DC_FILLS_ZEN

     type, public :: PMC_LS_NOT_HALTED_CYC_ZEN
        public
        character(len=5)        :: event = "0x76"
        character(len=16)       :: event_name = "LS_NOT_HALTED_CYC"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx076 [Cycles not in Halt] (Core::X86::Pmc::Core::LsNotHaltedCyc)
     end type PMC_LS_NOT_HALTED_CYC_ZEN

     type, public :: PMC_IC_FW32_ZEN
        public
        character(len=5)        :: event = "0x80"
        character(len=7)        :: event_name = "IC_FW32"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx080 [32 Byte Instruction Cache Fetch] (Core::X86::Pmc::Core::IcFw32)
     end type PMC_IC_FW32_ZEN

     type, public :: PMC_IC_FW32_MISS_ZEN
        public
        character(len=5)        :: event = "0x81"
        character(len=12)       :: event_name = "IC_FW32_MISS"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx081 [32 Byte Instruction Cache Misses] (Core::X86::Pmc::Core::IcFw32Miss)
     end type PMC_IC_FW32_MISS_ZEN

     type, public :: PMC_IC_CACHE_FILL2_ZEN
        public
        character(len=5)        :: event = "0x82"
        character(len=14)       :: event_name = "IC_CACHE_FILL2"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx082 [Instruction Cache Refills from L2] (Core::X86::Pmc::Core::IcCacheFillL2)
     end type PMC_IC_CACHE_FILL2_ZEN

     type, public :: PMC_IC_REFILL_SYS_ZEN
        public
        character(len=4)        :: event = "0x83"
        character(len=13)       :: event_name = "IC_REFILL_SYS"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx083 [Instruction Cache Refills from System] (Core::X86::Pmc::Core::IcCacheFillSys)
     end type PMC_IC_REFILL_SYS_ZEN

     type, public :: PMC_L1_ITLB_MISS_L2_ITLB_HIT_ZEN
        public
        character(len=4)         :: event = "0x84"
        character(len=24)        :: event_name = "L1_ITLB_MISS_L2_ITLB_HIT"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx084 [L1 ITLB Miss, L2 ITLB Hit] (Core::X86::Pmc::Core::BpL1TlbMissL2Hit)
     end type PMC_L1_ITLB_MISS_L2_ITLB_HIT_ZEN

      type, public :: PMC_L1_ITLB_MISS_L2_ITLB_MISS_ZEN
        public
        character(len=4)         :: event = "0x85"
        character(len=25)        :: event_name = "L1_ITLB_MISS_L2_ITLB_MISS"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx084 [L1 ITLB Miss, L2 ITLB Hit] (Core::X86::Pmc::Core::BpL1TlbMissL2Hit)
     end type PMC_L1_ITLB_MISS_L2_ITLB_MISS_ZEN

     type, public :: PMC_IC_FETCH_STALL_ZEN
        public
        character(len=4)         :: event = "0x87"
        character(len=14)        :: event_name = "IC_FETCH_STALL"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx087 [Instruction Pipe Stall] (Core::X86::Pmc::Core::IcFetchStall)
     end type PMC_IC_FETCH_STALL_ZEN

     type, public :: PMC_L1_BTB_CORRECT_ZEN
        public
        character(len=4)        :: event = "0x8A"
        character(len=13)       :: event_name = "L1_BTB_CORRECT"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx08A [L1 BTB Correction] (Core::X86::Pmc::Core::BpL1BTBCorrect)
     end type PMC_L1_BTB_CORRECT_ZEN

     type, public :: PMC_L2_BTB_CORRECT_ZEN
        public
        character(len=4)        :: event = "0x8B"
        character(len=13)       :: event_name = "L2_BTB_CORRECT"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        !PMCx08B [L2 BTB Correction] (Core::X86::Pmc::Core::BpL2BTBCorrect)
     end type PMC_L2_BTB_CORRECT_ZEN

     type, public :: PMC_IC_LINES_INVALIDATED_ZEN
        public
        character(len=4)        :: event = "0x8C"
        character(len=19)       :: event_name = "IC_LINES_INVALIDATED"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx08C [Instruction Cache Lines Invalidated] (Core::X86::Pmc::Core::IcCacheInval)
     end type PMC_IC_LINES_INVALIDATED_ZEN

     type, public :: PMC_ITLB_RELOADS_ZEN
        public
        character(len=4)         :: event = "0x99"
        character(len=12)        :: event_name = "ITLB_RELOADS"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx099 [ITLB Reloads] (Core::X86::Pmc::Core::BpTlbRel)
     end type PMC_ITLB_RELOADS_ZEN

     type, public :: PMC_IC_OC_MODE_SWITCH_ZEN
        public
        character(len=5)         :: event = "0x28A"
        character(len=16)        :: event_name = "IC_OC_MODE_SWITCH"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
     end type PMC_IC_OC_MODE_SWITCH_ZEN

     type, public :: PMC_DE_DISPATCH_TOKEN_STALLS0_ZEN
        public
        character(len=4)         :: event = "0xAF"
        character(len=23)        :: event_name = "DE_DISPATCH_TOKEN_STALLS0"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx0AF [Dynamic Tokens Dispatch Stall Cycles 0] (Core::X86::Pmc::Core::DeDisDispatchTokenStalls0)
     end type PMC_DE_DISPATCH_TOKEN_STALLS0_ZEN

     type, public :: PMC_RET_INSTR_ZEN
        public
        character(len=4)         :: event = "0xC0"
        character(len=9)         :: event_name = "RET_INSTR"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx0C0 [Retired Instructions] (Core::X86::Pmc::Core::ExRetInstr)
     end type PMC_RET_INSTR_ZEN
     
     type, public :: PMC_RETIRED_UOPS_ZEN
        public
        character(len=4)         :: event = "0xC1"
        character(len=12)        :: event_name = "RETIRED_UOPS"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx0C1 [Retired Uops] (Core::X86::Pmc::Core::ExRetCops)
     end type PMC_RETIRED_UOPS_ZEN
      
     type, public :: PMC_RETIRED_BRANCH_INSTR_ZEN
        public
        character(len=4)         :: event = "0xC2"
        character(len=20)        :: event_name = "RETIRED_BRANCH_INSTR"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx0C2 [Retired Branch Instructions] (Core::X86::Pmc::Core::ExRetBrn)
     end type PMC_RETIRED_BRANCH_INSTR_ZEN

     type, public :: PMC_RETIRED_BRANCH_INSTR_MISPRED_ZEN
        public
        character(len=4)          :: event = "0xC3"
        character(len=27)         :: event_name = "RETIRED_BRANCH_INSTR_MISPRED"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx0C3 [Retired Branch Instructions Mispredicted] (Core::X86::Pmc::Core::ExRetBrnMisp)
     end type PMC_RETIRED_BRANCH_INSTR_MISPRED_ZEN

     type, public :: PMC_RETIRED_TAKEN_BRANCH_INSTR_ZEN
        public
        character(len=4)           :: event = "0xC4"
        character(len=25)          :: event_name = "RETIRED_TAKEN_BRANCH_INSTR"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx0C4 [Retired Taken Branch Instructions] (Core::X86::Pmc::Core::ExRetBrnTkn)
     end type PMC_RETIRED_TAKEN_BRANCH_INSTR_ZEN

     type, public :: PMC_RETIRED_TAKEN_BRANCH_INSTR_MISPRED_ZEN
        public
        character(len=4)           :: event = "0xC5"
        character(len=32)          :: event_name = "RETIRED_TAKEN_BRANCH_INSTR_MISPRED"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx0C5 [Retired Taken Branch Instructions Mispredicted] (Core::X86::Pmc::Core::ExRetBrnTknMisp)
     end type PMC_RETIRED_TAKEN_BRANCH_INSTR_MISPRED_ZEN

     type, public :: PMC_RETIRED_FAR_CTRL_TRANSFERS_ZEN
        public
        character(len=4)           :: event = "0xC6"
        character(len=25)          :: event_name = "RETIRED_FAR_CTRL_TRANSFERS"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
     end type PMC_RETIRED_FAR_CTRL_TRANSFERS_ZEN

     type, public :: PMC_RETIRED_BRANCH_RESYNCS_ZEN
        public
        character(len=4)           :: event = "0xC7"
        character(len=22)          :: event_name = "RETIRED_BRANCH_RESYNCS"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx0C7 [Retired Branch Resyncs] (Core::X86::Pmc::Core::ExRetBrnResync)
     end type PMC_RETIRED_BRANCH_RESYNCS_ZEN

     type, public :: PMC_RETIRED_NEAR_RET_ZEN
        public
        character(len=4)           :: event = "0xC8"
        character(len=16)          :: event_name = "RETIRED_NEAR_RET"
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx0C8 [Retired Near Returns] (Core::X86::Pmc::Core::ExRetNearRet)
     end type PMC_RETIRED_NEAR_RET_ZEN

     

end module mod_zen_msr
