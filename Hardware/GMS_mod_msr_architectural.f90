

module mod_msr_architectural


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         mod_msr_architectural
 !          
 !          Purpose:
  !                   This module contains a derived data types
 !                    describing various processors MSR registers
 !                   
 !                     
 !          History:
 !                        Date: 15-05-2019
 !                        Time: 18:24 GMT+2
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

     use mod_kinds, only : int2,int4, int8b
     use IFPORT
     implicit none

     type, public :: MSR_IA32_P5_MC_ADDR
        public
        integer(kind=int4)  :: addr_dec  = 0
        character(len=3)    :: addr_hex = "0x0"
        integer(kind=int8b) :: msr_read
        character(len=20)   :: msr_name = "IA32_P5_MC_ADDR"
     end type MSR_IA32_PS_MC_ADDR

     type, public :: MSR_IA32_P5_TYPE
        public
        integer(kind=int4)  :: addr_dec = 1
        character(len=3)    :: addr_hex = "0x1"
        integer(kind=int8b) :: msr_read
        character(len=20)   :: msr_name = "IA32_P5_MC_TYPE"
     end type MSR_IA32_P5_TYPE

     type, public :: MSR_IA32_MONITOR_FILTER_SIZE
        public
        integer(kind=int4)  :: addr_dec = 6
        character(len=3)    :: addr_hex = "0x6"
        integer(kind=int8b) :: msr_read
        character(len=32)   :: msr_name = "IA32_MONITOR_FILTER_SIZE"
     end type MSR_IA32_MONITOR_FILTER_SIZE

     ! TSC MSR ommitted
     type, public :: MSR_IA32_PLATFORM_ID
        public
        integer(kind=int4)  :: addr_dec = 23
        character(len=4)    :: addr_hex = "0x17"
        integer(kind=int8b) :: msr_read
        ! 49:0 -- reserved
        ! 52:50 -- Platform Id (RO)
        character(len=20)   :: msr_name = "IA32_PLATFORM_ID"
     end type MSR_IA32_PLATFORM_ID

     type, public :: MSR_IA32_APIC_BASE
        public
        integer(kind=int4)  :: addr_dec = 27
        character(len=4)    :: addr_hex = "0x1B"
        integer(kind=int8b) :: msr_read !read value
        !7:0 -- reserved
        !8   -- BSP flag (R/W)
        !9   -- reserved
        !10  -- Enable x2APIC mode
        !11  -- APIC Global Enable (R/W)
        !(MAXPHYADDR - 1):12 -- APIC Base (R/W)
        !63:MAXPHYADDR -- reserved
        integer(kind=int8b) :: msr_write
        character(len=20)   :: msr_name = "IA32_APIC_BASE"
        character(len=16)   :: msrw_hex
     end type MSR_IA32_APIC_BASE

     type, public :: MSR_IA32_FEATURE_CONTROL
        public
        integer(kind=int4) :: addr_dec = 58
        character(len=4)   :: addr_hex = "0x3A"
        integer(kind=int8b) :: msr_read
        ! 0 -- Lock bit (R/WO): (1 = locked). When set,
        !  locks this MSR from being written; writes
        ! to this bit will result in GP(0).
        ! 1 -- Enable VMX inside SMX operation (R/WL):
        !  This bit enables a system executive to use
        !  VMX in conjunction with SMX to support
        ! Intel® Trusted Execution Technology.
        ! 2 -- Enable VMX outside SMX operation (R/WL):
        !      This bit enables VMX for a system
        !      executive that does not require SMX.
        ! 7:3 -- reserved
        ! 14:8 -- SENTER Local Function Enables (R/WL):
        !         When set, each bit in the field represents
        !         an enable control for a corresponding
        !         SENTER function. This field is supported
        !         only if CPUID.1:ECX.[bit 6] is set.
        ! 15 -- SENTER Global Enable (R/WL): This bit must
        !       be set to enable SENTER leaf functions.
        !       This bit is supported only if
        !       CPUID.1:ECX.[bit 6] is set.
        ! 16 -- reserved
        ! 17 -- SGX Launch Control Enable (R/WL): This bit
        !       must be set to enable runtime re-
        !       configuration of SGX Launch Control via the
        !       IA32_SGXLEPUBKEYHASHn MSR.
        ! 18 -- SGX Global Enable (R/WL): This bit must be
        !       set to enable SGX leaf functions.
        ! 19 -- reserved
        ! 20 -- LMCE On (R/WL): When set, system
        !       software can program the MSRs associated
        !       with LMCE to configure delivery of some
        !       machine check exceptions to a single logical
        !       processor.
        ! 63:21 -- reserved
        integer(kind=int8b)  :: msr_write
        character(len=20)    :: msr_name = "IA32_FEATURE_CONTROL"
        character(len=16)    :: msrw_hex
     end type MSR_IA32_FEATURE_CONTROL

     type, public :: MSR_IA32_TSC_ADJUST
        public
        integer(kind=int4)  :: addr_dec = 59
        character(len=4)    :: addr_hex = "0x3B"
        integer(kind=int8b), dimension(64) :: msr_read ! Per HW thread
        ! 63:0 -- Local offset value of the IA32_TSC for a
        !         logical processor. Reset value is zero. A
        !         write to IA32_TSC will modify the local
        !         offset in IA32_TSC_ADJUST and the
        !         content of IA32_TSC, but does not affect
        !         the internal invariant TSC hardware.
        !         THREAD_ADJUST
        integer(kind=int8b), dimension(64) :: msr_write ! Per HW thread
        character(len=16),   dimension(64) :: msrw_hex
        character(len=20)  :: msr_name = "IA32_TSC_ADJUST"
     end type MSR_IA32_TSC_ADJUST

     type, public :: MSR_IA32_SPEC_CTRL
        public
        integer(kind=int4)  :: addr_dec = 72
        character(len=4)    :: addr_hex = "0x48"
        integer(kind=int8b) :: msr_read
        ! The MSR bits are defined as logical
        ! processor scope. On some core
        ! implementations, the bits may impact
        ! sibling logical processors on the same core.
        ! Speculation Control (R/W)
        ! 0 -- Indirect Branch Restricted Speculation
        ! (IBRS). Restricts speculation of indirect
        ! branch.
        ! 1 -- Single Thread Indirect Branch Predictors
        ! (STIBP). Prevents indirect branch
        ! predictions on all logical processors on the
        ! core from being controlled by any sibling
        ! logical processor in the same core.
        ! 2 -- Speculative Store Bypass Disable (SSBD)
        ! delays speculative execution of a load until
        ! the addresses for all older stores are
        ! known.
        ! 63:3 -- reserved
        integer(kind=int8b)  :: msr_write
        character(len=20)    :: msr_name = "IA32_SPEC_CTRL"
        character(len=16)    :: msrw_hex 
     end type MSR_IA32_SPEC_CTRL
       
     type, public :: MSR_IA32_PRED_CMD
        public
        integer(kind=int4)   :: addr_dec = 73
        character(len=4)     :: addr_hex = "0x49"
        integer(kind=int8b)  :: msr_read
        ! Prediction Command (WO)
        ! Gives software a way to issue commands
        ! that affect the state of predictors.
        ! 0 -- indirect Branch Prediction Barrier (IBPB).
        ! 62:1 -- reserved
        integer(kind=int8b)  :: msr_write
        character(len=20)    :: msr_name = "IA32_PRED_CMD"
        character(len=16)    :: msrw_hex
     end type MSR_IA32_PRED_CMD

     type, public :: MSR_IA32_BIOS_UPDT_TRIG
        public
        integer(kind=int4)   :: addr_dec = 121
        character(len=5)     :: addr_hex = "0x79"
        integer(kind=int8b)  :: msr_read
        ! BIOS Update Trigger (W)
        integer(kind=int8b)  :: msr_write
        character(len=20)    :: msr_name = "IA32_BIOS_UPDT_TRIG"
        character(len=16)    :: msrw_hex
     end type MSR_IA32_BIOS_UPDT_TRIG

     type, public :: MSR_IA32_BIOS_SIGN_ID
        public
        integer(kind=int4)   :: addr_dec = 139
        character(len=4)     :: addr_hex = "0x8B"
        integer(kind=int8b)  :: msr_read
        ! BIOS Update Signature (RO)
        ! Returns the microcode update signature
        ! following the execution of CPUID.01H.
        ! 31:0 -- reserved
        ! 63:32 --  is recommended that this field be pre-
        !     loaded with zero prior to executing CPUID.
        !     If the field remains zero following the
        !     execution of CPUID, this indicates that no
        !     microcode update is loaded. Any non-zero
        !     value is the microcode update signature. 
        integer(kind=int8b)  :: msr_write
        character(len=20)    :: msr_name = "IA32_BIOS_SIGN_ID"
        character(len=16)    :: msrw_hex
     end type MSR_IA32_BIOS_SIGN_ID

     type, public :: MSR_IA32_SGXLEPUBKEYHASH0
        public
        integer(kind=int4)   :: addr_dec = 140
        character(len=4)     :: addr_hex = "0x8C"
        integer(kind=int8b)  :: msr_read
        ! IA32_SGXLEPUBKEYHASH[63:0] (R/W)
        ! Bits 63:0 of the SHA256 digest of the
        ! SIGSTRUCT.MODULUS for SGX Launch
        ! Enclave. On reset, the default value is the
        ! digest of Intel’s signing key.
        integer(kind=int8b)  :: msr_write
        character(len=20)    :: msr_name =  "IA32_SGXLEPUBKEYHASH0"
        character(len=16)    :: msrw_hex
     end type MSR_IA32_SGXLEPUBKEYHASH0

     type, public :: MSR_IA32_SGXLEPUBKEYHASH1
        public
        integer(kind=int4)   :: addr_dec = 141
        character(len=4)     :: addr_hex = "0x8D"
        integer(kind=int8b)  :: msr_read
        ! IA32_SGXLEPUBKEYHASH[127:64] (R/W)
        ! Bits 127:64 of the SHA256 digest of the
        ! SIGSTRUCT.MODULUS for SGX Launch
        ! Enclave. On reset, the default value is the
        ! digest of Intel’s signing key.
        integer(kind=int8b)  :: msr_write
        character(len=20)    :: msr_name = "IA32_SGXLEPUBKEYHASH1"
     end type MSR_IA32_SGXLEPUBKEYHASH1

     type, public :: MSR_IA32_SGXLEPUBKEYHASH2
        public
        integer(kind=int4)   :: addr_dec = 142
        character(len=4)     :: addr_hex = "0x8E"
        integer(kind=int8b)  :: msr_read
        ! IA32_SGXLEPUBKEYHASH[191:128] (R/W)
        ! Bits 191:128 of the SHA256 digest of the
        ! SIGSTRUCT.MODULUS for SGX Launch
        ! Enclave. On reset, the default value is the
        ! digest of Intel’s signing key.
        integer(kind=int8b)  :: msr_write
        character(len=20)    :: msr_name = "IA32_SGXLEPUBKEYHASH2"
     end type MSR_IA32_SGXLEPUBKEYHASH2

     type, public :: MSR_IA32_SGXLEPUBKEYHASH3
        public
        integer(kind=int4)   :: addr_dec = 143
        character(len=4)     :: addr_hex = "0x8F"
        integer(kind=int8b)  :: msr_read
        ! A32_SGXLEPUBKEYHASH[255:192] (R/W)
        ! Bits 255:192 of the SHA256 digest of the
        ! SIGSTRUCT.MODULUS for SGX Launch
        ! Enclave. On reset, the default value is the
        ! digest of Intel’s signing key.
        integer(kind=int8b)  :: msr_write
        character(len=20)    :: msr_name = "IA32_SGXLEPUBKEYHAS3"
     end type MSR_IA32_SGXLEPUBKEYHASH3

     type, public :: MSR_IA32_SMM_MONITOR_CTL
        public
        integer(kind=int4)   :: addr_dec = 155
        character(len=4)     :: addr_hex = "0x9B"
        integer(kind=int8b)  :: msr_read
        !  SMM Monitor Configuration (R/W)
        !  0 -- Valid (R/W)
        !  1 -- Reserved
        !  2 -- Controls SMI unblocking by VMXOFF
        !  11:3 -- Reserved
        !  31:12 -- MSEG Base (R/W)
        !  63:32 -- Reserved
        integer(kind=int8b)  :: msr_write
        character(len=20)    :: msr_name = "IA32_SMM_MONITOR_CTL"
        character(len=16)    :: msrw_hex
     end type MSR_IA32_SMM_MONITOR_CTL
        
     type, public :: MSR_IA32_SMBASE
        public
        integer(kind=int4)   :: addr_dec = 158
        character(len=4)     :: addr_hex = "0x9E"
        integer(kind=int8b)  :: msr_read
        ! Base address of the logical processor’s
        ! SMRAM image (RO, SMM only).
        integer(kind=int8b)  :: msr_write
        character(len=20)    :: msr_name = "IA32_SMBASE"
     end type MSR_IA32_SMBASE

     type, public :: MSR_IA32_PMC0
        public
        integer(kind=int4)   :: addr_dec = 193
        character(len=4)     :: addr_hex = "0xC1"
        ! Scalar values
        integer(kind=int8b)  :: msr_read
        ! General Performance Counter 0 (R/W)
        integer(kind=int8b)  :: msr_write
        character(len=9)     :: msr_name = "IA32_PMC0"
        ! Arrays of read out values
        !DIR$ ATTRIBUTES ALIGN : 64 :: read_values
        integer(kind=int8b), dimension(1000) :: msr_reads
        ! Array of write values
        !DIR$ ATTRIBUTES ALIGN : 64 :: write_values
        integer(kind=int8b), dimension(1000) :: msr_writes
        ! General Performance Counter 0 (R/W)
        character(len=16),   dimension(1000) :: msrw_hex
     end type MSR_IA32_PMC0

     type, public :: MSR_IA32_PMC1
        public
        integer(kind=int4)  :: addr_dec = 194
        character(len=4)    :: addr_hex = "0xC2"
        ! General Performance Counter 1 (R/W)
        ! Scalar values
        integer(kind=int8b) :: msr_read
        integer(kind=int8b) :: msr_write
        character(len=9)    :: msr_name = "IA32_PMC1"
        ! Arrays of values
        !DIR$ ATTRIBUTES ALIGN : 64 :: read_values
        integer(kind=int8b), dimension(1000) :: msr_reads
        !DIR$ ATTRIBUTES ALIGN : 64 :: write_values
        integer(kind=int8b), dimension(1000) :: msr_writes
        ! General Performance Counter 0 (R/W)
        character(len=16),   dimension(1000) :: msrw_hex
     end type MSR_IA32_PMC1

     type, public :: MSR_IA32_PMC2
        public
        integer(kind=int4)  :: addr_dec = 195
        character(len=4)    :: addr_hex = "0xC3"
        integer(kind=int8b) :: msr_read
        integer(kind=int8b) :: msr_write
        character(len=9)    :: msr_name = "IA32_PMC2"
        !DIR$ ATTRIBUTES ALIGN : 64 :: read_values
        integer(kind=int8b), dimension(1000) :: msr_reads
        !DIR$ ATTRIBUTES ALIGN : 64 :: write_values
        integer(kind=int8b), dimension(1000) :: msr_writes
        ! General Performance Counter 0 (R/W)
        character(len=16),   dimension(1000) :: msrw_hex
     end type MSR_IA32_PMC2

     type, public :: MSR_IA32_PMC3
        public
        integer(kind=int4)  :: addr_dec = 196
        character(len=4)    :: addr_hex = "0xC4"
        integer(kind=int8b) :: msr_read
        integer(kind=int8b) :: msr_write
        character(len=9)    :: msr_name = "IA32_PMC3"
        !DIR$ ATTRIBUTES ALIGN : 64 :: read_values
        integer(kind=int8b), dimension(1000) :: msr_reads
        !DIR$ ATTRIBUTES ALIGN : 64 :: write_values
        integer(kind=int8b), dimension(1000) :: msr_writes
     end type MSR_IA32_PMC3

     type, public :: MSR_IA32_PMC4
        public
        integer(kind=int4)  :: addr_dec = 197
        character(len=4)    :: addr_hex = "0xC5"
        integer(kind=int8b) :: msr_read
        integer(kind=int8b) :: msr_write
        character(len=9)    :: msr_name = "IA32_PMC4"
        !DIR$ ATTRIBUTES ALIGN : 64 :: read_values
        integer(kind=int8b), dimension(1000) :: msr_reads
        !DIR$ ATTRIBUTES ALIGN : 64 :: write_values
        integer(kind=int8b), dimension(1000) :: msr_writes
        ! General Performance Counter 0 (R/W)
        character(len=16),   dimension(1000) :: msrw_hex
     end type MSR_IA32_PMC4

     type, public :: MSR_IA32_PMC5
        public
        integer(kind=int4)  :: addr_dec = 198
        character(len=4)    :: addr_hex = "0xC6"
        integer(kind=int8b) :: msr_read
        integer(kind=int8b) :: msr_write
        character(len=9)    :: msr_name = "IA32_PMC5"
        !DIR$ ATTRIBUTES ALIGN : 64 :: read_values
        integer(kind=int8b), dimension(1000) :: msr_reads
        !DIR$ ATTRIBUTES ALIGN : 64 :: write_values
        integer(kind=int8b), dimension(1000) :: msr_writes
        ! General Performance Counter 0 (R/W)
        character(len=16),   dimension(1000) :: msrw_hex
     end type MSR_IA32_PMC5

     type, public :: MSR_IA32_PMC6
        public
        integer(kind=int4)  :: addr_dec = 199
        character(len=9)    :: addr_hex = "0xC7"
        integer(kind=int8b) :: msr_read
        integer(kind=int8b) :: msr_write
        character(len=9)    :: msr_name = "IA32_PMC6"
        !DIR$ ATTRIBUTES ALIGN : 64 :: read_values
        integer(kind=int8b), dimension(1000) :: msr_reads
        !DIR$ ATTRIBUTES ALIGN : 64 :: write_values
        integer(kind=int8b), dimension(1000) :: msr_writes
        ! General Performance Counter 0 (R/W)
        character(len=16),   dimension(1000) :: msrw_hex
     end type MSR_IA32_PMC6

     type, public :: MSR_IA32_PMC7
        public
        integer(kind=int4)  :: addr_dec = 200
        character(len=4)    :: addr_hex = "0xC8"
        integer(kind=int8b) :: msr_read
        integer(kind=int8b) :: msr_write
        character(len=9)    :: msr_name = "IA32_PMC7"
        !DIR$ ATTRIBUTES ALIGN : 64 :: read_values
        integer(kind=int8b), dimension(1000) :: msr_reads
        !DIR$ ATTRIBUTES ALIGN : 64 :: write_values
        integer(kind=int8b), dimension(1000) :: msr_writes
        ! General Performance Counter 0 (R/W)
        character(len=16),   dimension(1000) :: msrw_hex
     end type MSR_IA32_PMC7

     type, public :: MSR_IA32_UMWAIT_CONTROL
        public
        integer(kind=int4)  :: addr_dec = 225
        character(len=4)    :: addr_hex = "0xE1"
        integer(kind=int4)  :: msr_read
        integer(kind=int4)  :: msr_write
        character(len=17)   :: msr_name = "IA32_UMWAIT_CONTROL"
        character(len=8)    :: msrw_hex
        ! 0 -- C0.2 is not allowed by the OS. Value of “1”
        ! means all C0.2 requests revert to C0.1.
        ! 1 -- RESERVED
        ! 31:2 -- Determines the maximum time in TSC-
        !  quanta that the processor can reside in
        !  either C0.1 or C0.2. A zero value indicates
        !  no maximum time. The maximum time
        !  value is a 32-bit value where the upper 30
        !  bits come from this field and the lower two
        !  bits are zero.
     end type MSR_IA32_UMWAIT_CONTROL

     type, public :: MSR_IA32_MTRRCAP
        public
        integer(kind=int4)  :: addr_dec = 254
        character(len=4)    :: addr_hex = "0xFE"
        integer(kind=int8b) :: msr_read
        character(len=9)    :: msr_name = "IA32_MTRRCAP"
        ! 7:0 -- VCNT: The number of variable memory
        !        type ranges in the CPU
        ! 8   -- Fixed range MTRRs are supported when set
        ! 9   -- reserved
        ! 10  -- WC Supported when set.
        ! 11  -- SMRR Supported when set.
        ! 12  -- PRMRR supported when set.
        ! 63:13 -- reserved
     end type MSR_IA32_MTRRCAP

     type, public :: MSR_IA32_ARCH_CAPABILITIES
        public
        integer(kind=int4)  :: addr_dec = 266
        character(len=5)    :: addr_hex = "0x10A"
        integer(kind=int8b) :: msr_read
        character(len=24)   :: msr_name = "IA32_ARCH_CAPABILITIES"
        ! 0 RDCL_NO: The processor is not susceptible
        !   to Rogue Data Cache Load (RDCL).
        ! 1 IBRS_ALL: The processor supports
        !   enhanced IBRS.
        ! 2 RSBA: The processor supports RSB
        !   Alternate. Alternative branch predictors
        !   may be used by RET instructions when the
        !   RSB is empty. SW using retpoline may be
        !   affected by this behavior.
        ! 3 SKIP_L1DFL_VMENTRY: A value of 1
        !   indicates the hypervisor need not flush the
        !   L1D on VM entry.
        ! 4 SSB_NO: Processor is not susceptibled to Speculative Store Bypass
        ! 63:5 -- reserved
     end  type MSR_IA32_ARCH_CAPABILITIES

     type, public :: MSR_IA32_FLUSH_CMD
        public
        integer(kind=int4)  :: addr_dec = 267
        character(len=5)    :: addr_hex = "0x10B"
        integer(kind=int8b) :: msr_write
        character(len=14)   :: msr_name = "IA32_FLUSH_CMD"
        character(len=16)   :: msrw_hex
        ! Gives software a way to invalidate
        ! structures with finer granularity than other
        ! architectural methods.
        ! 0 L1D_FLUSH: Writeback and invalidate the
        ! L1 data cache. If CPUID.(EAX=07H,
        !    ECX=0):EDX[28]=1
        ! 63:1 Reserved
     end type MSR_IA32_FLUSH_CMD

     type, public :: MSR_IA32_MCG_CAP
        public
        integer(kind=int4)   :: addr_dec = 377
        character(len=5)     :: addr_hex = "0x179"
        integer(kind=int8b)  :: msr_read
        character(len=12)    :: msr_name = "IA32_MCG_CAP"
        !Global Machine Check Capability (RO)
        ! 7:0 Count: Number of reporting banks.
        ! 8       MCG_CTL_P: IA32_MCG_CTL is present if
        !         this bit is set.
        ! 9       MCG_EXT_P: Extended machine check
        !         state registers are present if this bit is set.
        ! 10      MCP_CMCI_P: Support for corrected MC
        !         error event is present.
        !         06_01H
        ! 11      MCG_TES_P: Threshold-based error status
        !         register are present if this bit is set.
        ! 15:12   Reserved
        ! 23:16   MCG_EXT_CNT: Number of extended
        !         machine check state registers present.
        ! 24      MCG_SER_P: The processor supports
        !         software error recovery if this bit is set.
        ! 25      Reserved
        ! 26      MCG_ELOG_P: Indicates that the processor
        !         allows platform firmware to be invoked
        !         when an error is detected so that it may
        !         provide additional platform specific
        !         information in an ACPI format “Generic
        !         Error Data Entry” that augments the data
        !         included in machine check bank registers. 06_3EH
        ! 27      MCG_LMCE_P: Indicates that the processor
        !         supports extended state in
        !         IA32_MCG_STATUS and associated MSR
        !         necessary to configure Local Machine
        !         Check Exception (LMCE).
        !63:28    Reserved
     end type MSR_IA32_MCG_CAP

     type, public :: MSR_IA32_MCG_STATUS
        public
        integer(kind=int4)  :: addr_dec = 378
        character(len=5)    :: addr_hex = "0x17A"
        integer(kind=int8b) :: msr_write
        integer(kind=int8b) :: msr_read
        character(len=16)   :: msrw_hex
        character(len=18)   :: msr_name = "IA32_MCG_STATUS"
        ! Global Machine Check Status (R/W0)
        ! 0 RIPV. Restart IP valid. 06_01H
        ! 1 EIPV. Error IP valid. 06_01H
        ! 2 MCIP. Machine check in progress. 06_01H
        !3 LMCE_S If
        !  IA32_MCG_CAP.LMCE_P[2
        !  7] =1
        ! 63:4 Reserved
     end type MSR_IA32_MCG_STATUS

     type, public :: MSR_IA32_PERFEVTSEL0
        public
        integer(kind=int4)   :: addr_dec = 390
        character(len=5)     :: addr_hex = "0x186"
        integer(kind=int8b)  :: msr_write
        integer(kind=int8b)  :: msr_read
        character(len=16)    :: msrw_hex
        character(len=17)    :: msr_name = "IA32_PERFEVTSEL0"
        ! Performance Event Select Register 0 (R/W)
        ! 7:0 Event Select: Selects a performance event
        !     logic unit.
        ! 15:8 UMask: Qualifies the microarchitectural
        !      condition to detect on the selected event
        !      logic.
        ! 16   USR: Counts while in privilege level is not
        !      ring 0.
        ! 17   OS: Counts while in privilege level is ring 0.
        ! 18   Edge: Enables edge detection if set.
        ! 19   PC: Enables pin control.
        ! 20   INT: Enables interrupt on counter overflow.
        ! 21   AnyThread: When set to 1, it enables
        !      counting the associated event conditions
        !      occurring across all logical processors
        !      sharing a processor core. When set to 0, the
        !      counter only increments the associated
        !      event conditions occurring in the logical
        !      processor which programmed the MSR.
        !  22  EN: Enables the corresponding performance
        !      counter to commence counting when this
        !      bit is set.
        !  23  INV: Invert the CMASK.
        !  31:24 CMASK: When CMASK is not zero, the
        !      corresponding performance counter
        !      increments each cycle if the event count is
        !      greater than or equal to the CMASK.
        !  63:32 Reserved IA32_PERFEVTSEL1 Performance
     end type MSR_IA32_PERFEVTSEL0

     type, public :: MSR_IA32_PERFEVTSEL1
        public
        integer(kind=int4)   :: addr_dec = 391
        character(len=5)     :: addr_hex = "0x187"
        integer(kind=int8b)  :: msr_write
        integer(kind=int8b)  :: msr_read
        character(len=16)    :: msrw_hex
        character(len=21)    :: msr_name = "MSR_IA32_PERFEVTSEL1"
        ! Performance Event Select Register 1 (R/W)
     end type MSR_IA32_PERFEVTSEL1

     type, public :: MSR_IA32_PERFEVTSEL2
        public
        integer(kind=int4)   :: addr_dec = 392
        character(len=5)     :: addr_hex = "0x188"
        integer(kind=int8b)  :: msr_write
        integer(kind=int8b)  :: msr_read
        character(len=16)    :: msrw_hex
        character(len=17)    :: msr_name = "IA32_PERFEVTSEL2"
        ! Performance Event Select Register 2 (R/W)
     end type MSR_IA32_PERFEVTSEL2

     type, public :: MSR_IA32_PERFEVTSEL3
        public
        integer(kind=int4)   :: addr_dec = 393
        character(len=5)     :: addr_hex = "0x189"
        integer(kind=int8b)  :: msr_write
        integer(kind=int8b)  :: msr_read
        character(len=16)    :: msrw_hex
        character(len=17)    :: msr_name = "IA32_PERFEVTSEL3"
        ! Performance Event Select Register 3 (R/W)
     end type MSR_IA32_PERFEVTSEL3

     type, public :: MSR_IA32_PERF_STATUS
        public
        integer(kind=int4)   :: addr_dec = 408
        character(len=5)     :: addr_hex = "0x198"
        integer(kind=int8b)  :: msr_read
        character(len=16)    :: msr_name = "IA32_PER_STATUS"
        ! Current Performance Status (RO)
        ! 15:0 Current performance State Value.
        ! 63:16 Reserved
     end type MSR_IA32_PERF_STATUS

     type, public :: MSR_IA32_PERF_CTL
        public
        integer(kind=int4)   :: addr_dec = 409
        character(len=5)     :: addr_hex = "0x199"
        integer(kind=int8b)  :: msr_write
        integer(kind=int8b)  :: msr_read
        character(len=16)    :: msrw_hex
        character(len=13)    :: msr_name = "IA32_PERF_CTL"
        ! Performance Control MSR (R/W)
        ! 15:0 Target performance State Value.
        ! 31:16 Reserved
        ! 32   IDA Engage (R/W) 06_0FH (Mobile only)
        !       When set to 1: disengages IDA.
        ! 63:33 reserved
        !
     end type MSR_IA32_PERF_CTL

     type, public :: MSR_IA32_CLOCK_MODULATION
        public
        integer(kind=int4)    :: addr_dec = 410
        character(len=5)      :: addr_hex = "0x19A"
        integer(kind=int8b)   :: msr_write
        integer(kind=int8b)   :: msr_read
        character(len=16)     :: msrw_hex
        character(len=17)     :: msr_name = "IA32_CLOCK_MODULATION"
        ! Thermal Interrupt Control (R/W)
        ! 0 High-Temperature Interrupt Enable If CPUID.01H:EDX[22] = 1
        ! 1 Low-Temperature Interrupt Enable If CPUID.01H:EDX[22] = 1
        ! 2 PROCHOT# Interrupt Enable If CPUID.01H:EDX[22] = 1
        ! 3 FORCEPR# Interrupt Enable If CPUID.01H:EDX[22] = 1
        ! 4 Critical Temperature Interrupt Enable If CPUID.01H:EDX[22] = 1
        ! 7:5 Reserved
        ! 14:8 Threshold #1 Value If CPUID.01H:EDX[22] = 1
        ! 15 Threshold #1 Interrupt Enable If CPUID.01H:EDX[22] = 1
        ! 22:16 Threshold #2 Value If CPUID.01H:EDX[22] = 1
        ! 23 Threshold #2 Interrupt Enable If CPUID.01H:EDX[22] = 1
        ! 24 Power Limit Notification Enable If CPUID.06H:EAX[4] = 1
        ! 63:25 Reserved
     end type MSR_IA32_CLOCK_MODULATION

     type, public :: MSR_IA32_THERM_STATUS
        public
        integer(kind=int4)   :: addr_dec = 412
        character(len=5)     :: addr_hex = "0x19C"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        character(len=17)    :: msr_name = "IA32_THERM_STATUS"
        
        ! Thermal Status Information (RO)
        ! 0 Thermal Status (RO) If CPUID.01H:EDX[22] = 1
        ! 1 Thermal Status Log (R/W) If CPUID.01H:EDX[22] = 1
        ! 2 PROCHOT # or FORCEPR# event (RO) If CPUID.01H:EDX[22] = 1
        ! 3 PROCHOT # or FORCEPR# log (R/WC0) If CPUID.01H:EDX[22] = 1
        ! 4 Critical Temperature Status (RO) If CPUID.01H:EDX[22] = 1
        ! 5 Critical Temperature Status log (R/WC0) If CPUID.01H:EDX[22] = 1
        ! 6 Thermal Threshold #1 Status (RO) If CPUID.01H:ECX[8] = 1
        ! 7 Thermal Threshold #1 log (R/WC0) If CPUID.01H:ECX[8] = 1
        ! 8 Thermal Threshold #2 Status (RO) If CPUID.01H:ECX[8] = 1
        ! 9 Thermal Threshold #2 log (R/WC0) If CPUID.01H:ECX[8] = 1
        ! 10 Power Limitation Status (RO) If CPUID.06H:EAX[4] = 1
        ! 11 Power Limitation log (R/WC0) If CPUID.06H:EAX[4] = 1
        ! 12 Current Limit Status (RO) If CPUID.06H:EAX[7] = 1
        ! 13 Current Limit log (R/WC0) If CPUID.06H:EAX[7] = 1
        ! 14 Cross Domain Limit Status (RO) If CPUID.06H:EAX[7] = 1
        ! 15 Cross Domain Limit log (R/WC0) If CPUID.06H:EAX[7] = 1
        ! 22:16 Digital Readout (RO) If CPUID.06H:EAX[0] = 1
        ! 26:23 Reserved 30:27 Resolution in Degrees Celsius (RO) If CPUID.06H:EAX[0] = 1
        ! 31 Reading Valid (RO) If CPUID.06H:EAX[0] = 1
        ! 63:32 RESERVED
     end type MSR_IA32_THERM_STATUS

     type, public :: MSR_IA32_MISC_ENABLE
        public
        integer(kind=int4)   :: addr_dec = 416
        character(len=5)     :: addr_hex = "0x1A0"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        character(len=17)    :: msr_name = "IA32_MISC_ENABLE"
        ! Enable Misc. Processor Features (R/W)
        ! 0  Fast-Strings Enable  0F_0H
        !     When set, the fast-strings feature (for REP
        !             MOVS and REP STORS) is enabled (default).
        !             When clear, fast-strings are disabled.
        ! 2:1 Reserved
        ! 3 Automatic Thermal Control Circuit Enable (r/w) 0F_0H
        ! 1 =
        !    Setting this bit enables the thermal
        !    control circuit (TCC) portion of the
        !    Intel Thermal Monitor feature. This
        !    allows the processor to automatically
        !    reduce power consumption in
        !    response to TCC activation.
        !  0 = Disabled.
        !      Note: In some products clearing this bit
        !      might be ignored in critical thermal
        !      conditions, and TM1, TM2 and adaptive
        !      thermal throttling will still be activated.
        !      The default value of this field varies with
        !      product . See respective tables where
        !      default value is listed.
        ! 6:4 Reserved
        ! 7 Performance Monitoring Available (R)
        !   0F_0H
        !   1 = Performance monitoring enabled.
        !   0 = Performance monitoring disabled.
        !   10:8 RESERVED
        ! 11 Branch Trace Storage Unavailable (RO)
        !    0F_0H
        !   1 = Processor doesn’t support branch
        !       trace storage (BTS).
        !   0 = BTS is supported.
        ! 12
        !   Processor Event Based Sampling (PEBS)
        !   Unavailable (RO)
        !   06_0FH
        !   1 = PEBS is not supported.
        !   0 = PEBS is supported.
        ! 15:13 Reserved
        ! 16 Enhanced Intel SpeedStep Technology
        !    Enable (R/W)
        !    If CPUID.01H: ECX[7] =1
        !   0=
        !         Enhanced Intel SpeedStep
        !         Technology disabled.
        !   1 = Enhanced Intel SpeedStep
        ! 18 ENABLE MONITOR FSM (R/W)
        ! 21:19 reserved
        ! 22 Limit CPUID Maxval (R/W)
        ! 23 xTPR Message Disable (R/W)
        ! 33:24 Reserved
        ! 34 XD Bit Disable (R/W)
        ! 63:35 Reserved
     end type MSR_IA32_MISC_ENABLE

     type, public :: MSR_IA32_ENERGY_PERF_BIAS
        public
        integer(kind=int4)   :: addr_dec = 432
        character(len=5)     :: addr_hex = "0x1B0"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        character(len=21)    :: msr_name = "IA32_ENERGY_PERF_BIAS"
        ! Performance Energy Bias Hint (R/W)
        ! 3:0 Power Policy Preference:
        ! If CPUID.6H:ECX[3] = 1
        ! 0 indicates preference to highest
        ! performance.
        ! 15 indicates preference to maximize
        ! energy saving.
        ! 63:4 reserved
      
     end type MSR_IA32_ENERGY_PERF_BIAS

     type, public :: MSR_IA32_PACKAGE_THERM_STATUS
        public
        integer(kind=int4)    :: addr_dec = 433
        character(len=5)      :: addr_hex = "0x1B1"
        integer(kind=int8b)   :: msr_read
        character(len=22)     :: msr_name = "IA32_PACKAGE_THERM_STATUS"
        ! Package Thermal Status Information (RO)
        !   ! 0 Pkg Thermal Status (RO)
        ! 1 Pkg Thermal Status Log (R/W)
        ! 2 Pkg PROCHOT # event (RO)
        ! 3 Pkg PROCHOT # log (R/WC0)
        ! 4 Pkg Critical Temperature Status (RO)
        ! 5 Pkg Critical Temperature Status Log
        !(R/WC0)
        ! 6 Pkg Thermal Threshold #1 Status (RO)
        ! 7 Pkg Thermal Threshold #1 log (R/WC0)
        ! 8 Pkg Thermal Threshold #2 Status (RO)
        ! 9 Pkg Thermal Threshold #1 log (R/WC0)
        ! 10 Pkg Power Limitation Status (RO)
        ! 11 Pkg Power Limitation log (R/WC0)
        ! 15:12 Reserved
        ! 22:16 Pkg Digital Readout (RO)
        ! 63:23 Reserved
     end type MSR_IA32_PACKAGE_THERM_STATUS

     type, public :: MSR_IA32_PACKAGE_THERM_INTERRUPT
        public
        integer(kind=int4)    :: addr_dec = 434
        character(len=5)      :: addr_hex = "0x1B2"
        integer(kind=int8b)   :: msr_read
        integer(kind=int8b)   :: msr_write
        character(len=16)     :: msrw_hex
        character(len=28)     :: msr_name = "IA32_PACKAGE_THERM_INTERRUPT"
        ! Pkg Thermal Interrupt Control (R/W)
        ! Enables and disables the generation of an
        ! interrupt on temperature transitions
        ! detected with the package’s thermal sensor
        ! 0 Pkg High-Temperature Interrupt Enable
        ! 1 Pkg Low-Temperature Interrupt Enable
        ! 2 Pkg PROCHOT# Interrupt Enable
        ! 3 Reserved
        ! 4 Pkg Overheat Interrupt Enable
        ! 7:5 Reserved
        ! 14:8 Pkg Threshold #1 Value
        ! 15 Pkg Threshold #1 Interrupt Enable
        ! 22:16 Pkg Threshold #2 Value
        ! 23 Pkg Threshold #2 Interrupt Enable
        ! 24 Pkg Power Limit Notification Enable
        ! 63:25 Reserved 
     end type MSR_IA32_PACKAGE_THERM_INTERRUPT

     type, public :: MSR_IA32_DEBUGCTL
        public
        integer(kind=int4)   :: addr_dec = 473
        character(len=5)     :: addr_hex = "0x1D9"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        character(len=13)    :: msr_name = "IA32_DEBUGCTL"
        ! Trace/Profile Resource Control (R/W)
        ! Description: Vol 4 2-17
     end type MSR_IA32_DEBUGCTL

     type, public :: MSR_IA32_FIXED_CTR0
        public
        integer(kind=int4)   :: addr_dec = 777
        character(len=5)     :: addr_hex = "0x309"
        integer(kind=int8b)  :: msr_write
        integer(kind=int8b)  :: msr_read
        character(len=16)    :: msrw_hex
        character(len=15)    :: msr_name = "IA32_FIXED_CTR0"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_reads
        integer(kind=int8b), dimension(1000) :: msr_reads
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_writes
        integer(kind=int8b), dimension(1000) :: msr_writes
        !DIR$ ATTIRBUTES ALIGN : 64 :: msrws_hex
        character(len=16),   dimension(1000) :: msrws_hex
        ! Fixed-Function Performance Counter 0
        ! (R/W): Counts Instr_Retired.Any.
     end type MSR_IA32_FIXED_CTR0

     type, public :: MSR_IA32_FIXED_CTR1
        public
        integer(kind=int4)   :: addr_dec = 778
        character(len=5)     :: addr_hex = "0x30A"
        integer(kind=int8b)  :: msr_write
        integer(kind=int8b)  :: msr_read
        character(len=16)    :: msrw_hex
        character(len=15)    :: msr_name = "IA32_FIXED_CTR1"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_reads
        integer(kind=int8b), dimension(1000) :: msr_reads
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_writes
        integer(kind=int8b), dimension(1000) :: msr_writes
        !DIR$ ATTIRBUTES ALIGN : 64 :: msrws_hex
        character(len=16),   dimension(1000) :: msrws_hex
        ! Fixed-Function Performance Counter 1
        ! (R/W): Counts CPU_CLK_Unhalted.Core
     end type MSR_IA32_FIXED_CTR1

     type, public :: MSR_IA32_FIXED_CTR2
        public
        integer(kind=int4)   :: addr_dec = 779
        character(len=5)     :: addr_hex = "0x30B"
        integer(kind=int8b)  :: msr_write
        integer(kind=int8b)  :: msr_read
        character(len=16)    :: msrw_hex
        character(len=15)    :: msr_name = "IA32_FIXED_CTR2"
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_reads
        integer(kind=int8b), dimension(1000) :: msr_reads
        !DIR$ ATTRIBUTES ALIGN : 64 :: msr_writes
        integer(kind=int8b), dimension(1000) :: msr_writes
        !DIR$ ATTIRBUTES ALIGN : 64 :: msrws_hex
        character(len=16),   dimension(1000) :: msrws_hex
        ! Fixed-Function Performance Counter 2
        ! (R/W): Counts CPU_CLK_Unhalted.Ref.
     end type MSR_IA32_FIXED_CTR2

     type, public :: MSR_IA32_PERF_CAPABILITIES
        public
        integer(kind=int4)   :: addr_dec = 837
        character(len=5)     :: addr_hex = "0x345"
        integer(kind=int8b)  :: msr_read
        character(len=21)    :: msr_name = "IA32_PERF_CAPABILITIES"
        ! Read Only MSR that enumerates the
        ! existence of performance monitoring
        ! features. (RO)
        !
     end type MSR_IA32_PERF_CAPABILITIES

     type, public :: MSR_IA32_FIXED_CTR_CTRL
        public
        integer(kind=int4)   :: addr_dec = 909
        character(len=5)     :: addr_hex = "0x38D"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        character(len=16)    :: msr_name = "IA32_FIXED_CTR_CTRL"
        ! Fixed-Function Performance Counter
        ! Control (R/W)
     end type MSR_IA32_FIXED_CTR_CTRL

     type, public :: MSR_IA32_PERF_GLOBAL_STATUS
        public
        integer(kind=int4)   :: addr_dec = 910
        character(len=5)     :: addr_hex = "0x38E"
        integer(kind=int8b)  :: msr_read
        character(len=23)    :: msr_name = "IA32_PERF_GLOBAL_STATUS"
        ! Global Performance Counter Status (RO)
     end type MSR_IA32_PERF_GLOBAL_STATUS

     type, public :: MSR_IA32_PERF_GLOBAL_CTRL
        public
        integer(kind=int4)   :: addr_dec = 911
        character(len=5)     :: addr_hex = "0x38F"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        character(len=21)    :: msr_name = "IA32_PERF_GLOBAL_CTRL"
        ! Global Performance Counter Control (R/W)
     end type MSR_IA32_PERF_GLOBAL_CTRL

     type, public :: MSR_IA32_PERF_GLOBAL_OVF_CTRL
        public
        integer(kind=int4)   :: addr_dec = 912
        character(len=5)     :: addr_hex = "0x390"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        character(len=25)    :: msr_name = "IA32_PERF_GLOBAL_OVF_CTRL"
        ! Global Performance Counter Overflow
        ! Control (R/W)
     end type MSR_IA32_PERF_GLOBAL_OVF_CTRL

     type, public :: MSR_IA32_PERF_GLOBAL_STATUS_RESET
        public
        integer(kind=int4)   :: addr_dec = 912
        character(len=5)     :: addr_hex = "0x390"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        character(len=25)    :: msr_name = "IA32_PERF_GLOBAL_STATUS_RESET"
        ! Global Performance Counter Overflow
        ! Reset Control (R/W)
     end type MSR_IA32_PERF_GLOBAL_STATUS_RESET

     type, public :: MSR_IA32_PERF_GLOBAL_STATUS_SET
        public
        integer(kind=int4)   :: addr_dec = 913
        character(len=5)     :: addr_hex = "0x391"
        integer(kind=int8b)  :: msr_read
        integer(kind=int8b)  :: msr_write
        character(len=16)    :: msrw_hex
        character(len=23)    :: msr_name = "IA32_PERF_GLOBAL_STATUS_SET"
        ! Global Performance Counter Overflow Set
        ! Control (R/W)
     end type MSR_IA32_PERF_GLOBAL_STATUS_SET

     type, public :: MSR_IA32_PERF_GLOBAL_INUSE
        public
        integer(kind=int4)   :: addr_dec = 914
        character(len=5)     :: addr_hex = "0x392"
        integer(kind=int8b)  :: msr_read
        !
        character(len=23)    :: msr_name = "IA32_PERF_GLOBAL_INUSE"
        ! Indicator that core perfmon interface is in
        ! use. (RO)
     end type MSR_IA32_PERF_GLOBAL_INUSE

     
     ! For calling msr-tools functions -- rdmsr and wrmsr
     character(*), parameter, public :: rd_cmd = "rdmsr"
     character(*), parameter, public :: wr_cmd = "wrmsr"
     ! 
     contains

     subroutine AccessIA32_P5_MC_ADDR(reg,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_P5_MC_ADDR
           type(MSR_IA32_P5_MC_ADDR),  intent(in)    :: reg
           character(len=*),           intent(in)    :: command
           character(len=*),           intent(in)    :: fname
           integer(kind=int2),         intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat = RUNQQ(rd_cmd,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessIA32_P5_MC_ADDR

     subroutine AccessIA32_P5_TYPE(reg,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_P5_TYPE
           type(MSR_IA32_P5_TYPE),   intent(in)    :: reg
           character(len=*),         intent(in)    :: command
           character(len=*),         intent(in)    :: fname
           integer(kind=int2),       intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat = RUNQQ(rd_cmd,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessIA32_P5_TYPE

     subroutine AccessIA32_MONITOR_FILTER_SIZE(reg,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_MONITOR_FILTER_SIZE
           type(MSR_IA32_MONITOR_FILTER_SIZE),   intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
           stat = RUNQQ(rd_cmd,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessIA32_MONITOR_FILTER_SIZE
       
     subroutine AccessIA32_PLATFORM_ID(reg,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PLATFORM_ID
           type(MSR_IA32_PLATFORM_ID),           intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat = RUNQQ(rd_cmd,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessIA32_PLATFORM_ID

     subroutine AccessIA32_APIC_BASE(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_APIC_BASE
           type(MSR_IA32_APIC_BASE),     intent(in) :: reg
           character(len=5),             intent(in) :: op
           character(len=*),             intent(in) :: command
           character(len=*),             intent(in) :: fname
           integer(kind=int2),           intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
         
AccessMSR:    select case(op)
                  case("read")
                     string = command//reg.addr_hex//fname
                     stat = RUNQQ(rd_cmd,string)
                     if(stat == -1) then
                        ier = stat
                     end if
                  case("write")
                     string = command//reg.addr_hex//reg.msrw_hex
                     stat   = RUNQQ(wr_cmd,string)
                     if(stat == -1) then
                        ier = stat
                     end if
                  case default
                     print*, "AccessIA32_APIC_BASE: -- Invalid switch argument!"
                     return
              end select AccessMSR
     end subroutine AccessIA32_APIC_BASE

     subroutine AccessIA32_FEATURE_CONTROL(reg,op,command,fname,ier)
       !DIR$  ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_FEATURE_CONTROL
           type(MSR_IA32_FEATURE_CONTROL),       intent(in) :: reg
           character(len=5),                     intent(in) :: op
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           integer(kind=int2),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
AccessMSR:   select case(op)
                case("read")
                   string = command//reg.addr_hex//fname
                   stat = RUNQQ(rd_cmd,string)
                   if(stat == -1) then
                      ier = stat
                   end if
                case("write")
                   string = command//reg.addr_hex//reg.msrw_hex
                   stat = RUNQQ(wr_cmd,string)
                   if(stat == -1) then
                      ier = stat
                   end if
                case default
                   print*, "AccessIA32_FEATURE_CONTROL: -- Invalid switch argument!!"
                   return
              end select AccessMSR
     end subroutine AccessIA32_FEATURE_CONTROL

     subroutine AccessIA32_TSC_ADJUST(reg,op,command,fname,hwth,val,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_TSC_ADJUST
           type(MSR_IA32_TSC_ADJUST),      intent(in) :: reg
           character(len=5),               intent(in) :: op
           character(len=*),               intent(in) :: command
           character(len=*),               intent(in) :: fname
           character(len=2),               intent(in) :: hwth
           character(len=16),              intent(in) :: val
           integer(kind=int2),             intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! EXec code
          
AccessMSR:     select case(op)
                   case("read")
                      string = command//reg.addr_hex//hwth//fname
                      stat = RUNQQ(rd_cmd,string)
                      if(stat == -1) then
                         ier = stat
                      end if
                   case("write")
                      string = command//hwth//reg.addr_hex//val
                      stat = RUNQQ(wr_cmd,string)
                      if(stat == -1) then
                         ier = stat
                      end if
                   case default
                      print*, "AccessIA32_TSC_ADJUST: -- Invalid switch argument!!"
                      return
                end select AccessMSR
     end subroutine AccessIA32_TSC_ADJUST

     subroutine AccessIA32_SPEC_CTRL(reg,op,command,fname,ier)
       !DIR$  ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_SPEC_CTRL
           type(MSR_IA32_SPEC_CTRL),       intent(in) :: reg
           character(len=5),               intent(in) :: op
           character(len=*),               intent(in) :: command
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
        
AccessMSR:    select case(op)
                  case("read")
                     string = command//reg.addr_hex//fname
                     stat = RUNQQ(rd_cmd,string)
                     if(stat == -1) then
                        ier = stat
                     end if
                  case("write")
                     string = command//reg.addr_hex/reg.msrw_hex
                     stat   = RUNQQ(wr_cmd,string)
                     if(stat == -1) then
                        ier = stat
                     end if
                  case default
                     print*, "AccessIA32_SPEC_CTRL: -- Invalid switch argument!!"
                     return
                  end select AccessMSR
     end subroutine AccessIA32_SPEC_CTRL

     subroutine AccessIA32_PRED_CMD(reg,op,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PRED_CMD
           type(MSR_IA32_PRED_CMD),         intent(in) :: reg
           character(len=5),                intent(in) :: op
           character(len=*),                intent(in) :: command
           character(len=*),                intent(in) :: fname
           integer(kind=int2),              intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
         
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PRED_CMD: -- Invalid switch argument!!"
                       return
               end select AccessMSR
     end subroutine AccessIA32_PRED_CMD

     subroutine AccessIA32_BIOS_UPDT_TRIG(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_BIOS_UPDT_TRIG
           type(MSR_IA32_BIOS_UPDT_TRIG),          intent(in) :: reg
           character(len=5),                       intent(in) :: op
           character(len=*),                       intent(in) :: command
           character(len=*),                       intent(in) :: fname
           integer(kind=int2),                     intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
          
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_BIOS_UPDT_TRIG: -- Invalid switch argument!!"
                       return
               end select AccessMSR           
     end subroutine AccessIA32_BIOS_UPDT_TRIG

     subroutine AccessIA32_BIOS_SIGN_ID(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_BIOS_SIGN_ID
           type(MSR_IA32_BIOS_SIGN_ID),          intent(in) :: reg
           character(len=5),                     intent(in) :: op
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           integer(kind=int2),                   intent(inout) :: ier
           ! Locls
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
      
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_BIOS_SIGN_ID: -- Invalid switch argument!!"
                       return
               end select AccessMSR     
     end subroutine AccessIA32_BIOS_SIGN_ID

     subroutine AccessIA32_SGXLEPUBKEYHASH0(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_SGXLEPUBKEYHASH0
           type(MSR_IA32_SGXLEPUBKEYHASH0),      intent(in) :: reg
           character(len=5),                     intent(in) :: op
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           integer(kind=int2),                   intent(inout) :: ier
           ! Local
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code .....
         
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_SGXLEPUBKEYHASH0: -- Invalid switch argument!!"
                       return
               end select AccessMSR     
     end subroutine AccessIA32_SGXLEPUBKEYHASH0

     subroutine AccessIA32_SGXLEPUBKEYHASH1(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_SGXLEPUBKEYHASH1
           type(MSR_IA32_SGXLEPUBKEYHASH1),         intent(in) :: reg
           character(len=5),                        intent(in) :: op
           character(len=*),                        intent(in) :: command
           character(len=*),                        intent(in) :: fname
           integer(kind=int2),                      intent(inout) :: ier
           ! LOcals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
       
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_SGXLEPUBKEYHASH1: -- Invalid switch argument!!"
                       return
               end select AccessMSR               
     end subroutine AccessIA32_SGXLEPUBKEYHASH1

     subroutine AccessIA32_SGXLEPUBKEYHASH2(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_SGXLEPUBKEYHASH2
           type(MSR_IA32_SGXLEPUBKEYHASH2),       intent(in) :: reg
           character(len=5),                      intent(in) :: op
           character(len=*),                      intent(in) :: command
           character(len=*),                      intent(in) :: fname
           integer(kind=int2),                    intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_SGXLEPUBKEYHASH2: -- Invalid switch argument!!"
                       return
               end select AccessMSR      
     end subroutine AccessIA32_SGXLEPUBKEYHASH2

     subroutine AccessIA32_SGXLEPUBKEYHASH3(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_SGXLEPUBKEYHASH3
           type(MSR_IA32_SGXLEPUBKEYHASH3),       intent(in) :: reg
           character(len=5),                      intent(in) :: op
           character(len=*),                      intent(in) :: command
           character(len=*),                      intent(in) :: fname
           integer(kind=int2),                    intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_SGXLEPUBKEYHASH3: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_SGXLEPUBKEYHASH3

     subroutine  AccesIA32_SMM_MONITOR_CTL(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_SMM_MONITOR_CTL
           type(MSR_IA32_SMM_MONITOR_CTL),         intent(in) :: reg
           character(len=5),                       intent(in) :: op
           character(len=*),                       intent(in) :: command
           character(len=*),                       intent(in) :: fname
           integer(kind=int2),                     intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_SMM_MONITOR_CTL: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccesIA32_SMM_MONITOR_CTL

     subroutine AccessIA32_PMC0(reg,op,command,val,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC0
           type(MSR_IA32_PMC0),            intent(in) :: reg
           character(len=5),               intent(in) :: op
           character(len=*),               intent(in) :: command
           character(len=16),              intent(in) :: val
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
           ! LOcals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC0: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC0

     subroutine AccessIA32_PMC1(reg,op,command,val,fname,ier)
       !DIR$  ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC1
           type(MSR_IA32_PMC1),            intent(in)  :: reg
           character(len=5),               intent(in)  :: op
           character(len=*),               intent(in)  :: command
           character(len=16),              intent(in)  :: val
           character(len=*),               intent(in)  :: fname
           integer(kind=int2),             intent(inout) :: ier
           ! LOcals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC1: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC1

     subroutine AccessIA32_PMC2(reg,op,command,val,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC2
           type(MSR_IA32_PMC2),            intent(in) :: reg
           character(len=5),               intent(in) :: op
           character(len=*),               intent(in) :: command
           character(len=16),              intent(in) :: val
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC2: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC2

     subroutine AccessIA32_PMC3(reg,op,command,val,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC3
           type(MSR_IA32_PMC3),         intent(in) :: reg
           character(len=5),            intent(in) :: op
           character(len=*),            intent(in) :: command
           character(len=16),           intent(in) :: val
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
         
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC3: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC3

     subroutine AccessIA32_PMC4(reg,op,command,val,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC4
           type(MSR_IA32_PMC4),         intent(in) :: reg
           character(len=5),            intent(in) :: op
           character(len=*),            intent(in) :: command
           character(len=16),           intent(in) :: val
           character(len=*),            intent(in) :: fname
           integer(kind=int2),          intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC4: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC4

     subroutine AccessIA32_PMC5(reg,op,command,val,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC5
           type(MSR_IA32_PMC5),           intent(in) :: reg
           character(len=5),              intent(in) :: op
           character(len=*),              intent(in) :: command
           character(len=16),             intent(in) :: val
           character(len=*),              intent(in) :: fname
           integer(kind=int2),            intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC5: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
      end subroutine AccessIA32_PMC5

      subroutine AccessIA32_PMC6(reg,op,command,val,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC6
           type(MSR_IA32_PMC6),            intent(in) :: reg
           character(len=5),               intent(in) :: op
           character(len=*),               intent(in) :: command
           character(len=16),              intent(in) :: val
           character(len=*),               intent(in) :: fname
           integer(kind=int2),             intent(in) :: ier
           ! LOcals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC6: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
      end subroutine AccessIA32_PMC6

      subroutine AccessIA32_PMC7(reg,op,command,val,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PMC7
           type(MSR_IA32_PMC7),              intent(in) :: reg
           character(len=5),                 intent(in) :: op
           character(len=*),                 intent(in) :: command
           character(len=16),                intent(in) :: val
           character(len=*),                 intent(in) :: fname
           integer(kind=int2),               intent(inout) :: ier
           ! LOcals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_PMC7: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC7

     subroutine AccessIA32_UMWAIT_CONTROL(reg,op,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_UMWAIT_CONTROL
           type(MSR_IA32_UMWAIT_CONTROL),           intent(in) :: reg
           character(len=5),                        intent(in) :: op
           character(len=*),                        intent(in) :: command
           character(len=*),                        intent(in) :: fname
           integer(kind=int2),                      intent(inout) :: ier
            ! LOcals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_UMWAIT_CONTROL: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_UMWAIT_CONTROL

     subroutine AccessIA32_MTRRCAP(reg,command,fname,ier)
!DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_MTRRCAP
           type(MSR_IA32_MTRRCAP),          intent(in) :: reg
           character(len=*),                intent(in) :: command
           character(len=*),                intent(in) :: fname
           integer(kind=int2),              intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rd_cmd,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessIA32_MTRRCAP

     subroutine AccessIA32_ARCH_CAPABILITIES(reg,command,fname,ier)
       !DIR$  ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_ARCH_CAPABILITIES
           type(MSR_IA32_ARCH_CAPABILITIES),          intent(in) :: reg
           character(len=*),                          intent(in) :: command
           character(len=*),                          intent(in) :: fname
           integer(kind=int2),                        intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat   = RUNQQ(rd_cmd,string)
           if(stat == -1) then
              ier = stat
           end if
     end subroutine AccessIA32_ARCH_CAPABILITIES

     subroutine AccessIA32_FLUSH_CMD(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_FLUSH_CMD
           type(MSR_IA32_FLUSH_CMD),           intent(in) :: reg
           character(len=5),                   intent(in) :: op
           character(len=*),                   intent(in) :: command
           character(len=*),                   intent(in) :: fname
           integer(kind=int2),                 intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int2), automatic :: stat
           ! Exec code ....
  ! Exec code ....
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = RUNQQ(rd_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = RUNQQ(wr_cmd,string)
                       if(stat == -1) then
                          ier = stat
                       end if
                    case default
                       print*, "AccessIA32_FLUSH_CMD: -- Invalid switch argument!!"
                       return
               end select AccessMSR            
     end subroutine AccessIA32_FLUSH_CMD

             
      
end module mod_msr_architectural
