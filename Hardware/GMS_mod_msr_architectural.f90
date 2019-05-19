

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

     use mod_kinds, only : int4, int8b
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
     end type MSR_IA32_PMC7

     type, public :: MSR_IA32_UMWAIT_CONTROL
        public
        integer(kind=int4)  :: addr_dec = 225
        character(len=4)    :: addr_hex = "0xE1"
        integer(kind=int4)  :: msr_read
        integer(kind=int4)  :: msr_write
        character(len=17)   :: msr_name = "IA32_UMWAIT_CONTROL"
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
        character(len=4)    :: addr_hex = "0x10B"
        integer(kind=int8b) :: msr_write
        character(len=14)   :: msr_name = "IA32_FLUSH_CMD"
        ! Gives software a way to invalidate
        ! structures with finer granularity than other
        ! architectural methods.
        ! 0 L1D_FLUSH: Writeback and invalidate the
        ! L1 data cache. If CPUID.(EAX=07H,
        !    ECX=0):EDX[28]=1
        ! 63:1 Reserved
     end type MSR_IA32_FLUSH_CMD

     contains

     subroutine AccessIA32_P5_MC_ADDR(reg,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_P5_MC_ADDR
           type(MSR_IA32_P5_MC_ADDR),  intent(in)    :: reg
           character(len=*),           intent(in)    :: command
           character(len=*),           intent(in)    :: fname
           integer(kind=int4),         intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat = SYSTEM(string)
           if(stat == -1) then
              ier = ierrno()
           end if
     end subroutine AccessIA32_P5_MC_ADDR

     subroutine AccessIA32_P5_TYPE(reg,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_P5_TYPE
           type(MSR_IA32_P5_TYPE),   intent(in)    :: reg
           character(len=*),         intent(in)    :: command
           character(len=*),         intent(in)    :: fname
           integer(kind=int4),       intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat = SYSTEM(string)
           if(stat == -1) then
              ier = ierrno()
           end if
     end subroutine AccessIA32_P5_TYPE

     subroutine AccessIA32_MONITOR_FILTER_SIZE(reg,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_MONITOR_FILTER_SIZE
           type(MSR_IA32_MONITOR_FILTER_SIZE),   intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           integer(kind=int4),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code ...
           string = command//reg.addr_hex//fname
           stat = SYSTEM(string)
           if(stat == -1) then
              ier = ierrno()
           end if
     end subroutine AccessIA32_MONITOR_FILTER_SIZE
       
     subroutine AccessIA32_PLATFORM_ID(reg,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_PLATFORM_ID
           type(MSR_IA32_PLATFORM_ID),           intent(in) :: reg
           character(len=*),                     intent(in) :: command
           character(len=*),                     intent(in) :: fname
           integer(kind=int4),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code ....
           string = command//reg.addr_hex//fname
           stat = SYSTEM(string)
           if(stat == -1) then
              ier = ierrno()
           end if
     end subroutine AccessIA32_PLATFORM_ID

     subroutine AccessIA32_APIC_BASE(reg,op,command,fname,ier)
       !DIR$ ATTRIBUTES CODE_ALIGN:32 :: AccessIA32_APIC_BASE
           type(MSR_IA32_APIC_BASE),     intent(in) :: reg
           character(len=5),             intent(in) :: op
           character(len=*),             intent(in) :: command
           character(len=*),             intent(in) :: fname
           integer(kind=int4),           intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           if(LEN(fname) >= 64) then
              return
           end if
AccessMSR:    select case(op)
                  case("read")
                     string = command//reg.addr_hex//fname
                     stat = SYSTEM(string)
                     if(stat == -1) then
                        ier = ierrno()
                     end if
                  case("write")
                     string = command//reg.addr_hex//reg.msrw_hex
                     if(stat == -1) then
                        ier = ierrno()
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
           integer(kind=int4),                   intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code ....
           if(LEN(fname) >= 64) then
              return
           end if
AccessMSR:   select case(op)
                case("read")
                   string = command//reg.addr_hex//fname
                   stat = SYSTEM(string)
                   if(stat == -1) then
                      ier = ierrno()
                   end if
                case("write")
                   string = command//reg.addr_hex//reg.msrw_hex
                   stat = SYSTEM(string)
                   if(stat == -1) then
                      ier = ierrno()
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
           integer(kind=int4),             intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! EXec code
           if(LEN(fname) >= 64) then
              return
           end if
AccessMSR:     select case(op)
                   case("read")
                      string = command//reg.addr_hex//hwth//fname
                      stat = SYSTEM(string)
                      if(stat == -1) then
                         ier = ierrno()
                      end if
                   case("write")
                      string = command//hwth//reg.addr_hex//val
                      stat = SYSTEM(string)
                      if(stat == -1) then
                         ier = ierrno()
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
           integer(kind=int4),             intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code .....
           if(LEN(fname) >= 64) then
              return
           end if
AccessMSR:    select case(op)
                  case("read")
                     string = command//reg.addr_hex//fname
                     stat = SYSTEM(string)
                     if(stat == -1) then
                        ier = ierrno()
                     end if
                  case("write")
                     string = command//reg.addr_hex/reg.msrw_hex
                     stat   = SYSTEM(string)
                     if(stat == -1) then
                        ier = ierrno()
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
           integer(kind=int4),              intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code .....
           if(LEN(fname) >= 64) then
              return
           end if
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
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
           integer(kind=int4),                     intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code .....
           if(LEN(fname) >= 64) then
              return
           end if
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
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
           integer(kind=int4),                   intent(inout) :: ier
           ! Locls
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code .....
           if(LEN(fname) >= 64) then
              return
           end if
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
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
           integer(kind=int4),                   intent(inout) :: ier
           ! Local
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code .....
           if(LEN(fname) >= 64) then
              return
           end if
AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
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
           integer(kind=int4),                      intent(inout) :: ier
           ! LOcals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code ....
           if(LEN(fname) >= 64) then
              return
           end if
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
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
           integer(kind=int4),                    intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code ....
           if(LEN(fname) >= 64) then
              return
           end if
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
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
           integer(kind=int4),                    intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code ....
           if(LEN(fname) >= 64) then
              return
           end if
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
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
           integer(kind=int4),                     intent(inout) :: ier
           ! Locals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code ....
           if(LEN(fname) >= 64) then
              return
           end if
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
                       end if
                    case("write")
                       string = command//reg.addr_hex//reg.msrw_hex
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
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
           integer(kind=int4),             intent(inout) :: ier
           ! LOcals
           character(len=128), automatic :: string
           integer(kind=int4), automatic :: stat
           ! Exec code ....
           if(LEN(fname) >= 64) then
              return
           end if
 AccessMSR:     select case(op)
                    case("read")
                       string = command//reg.addr_hex//fname
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
                       end if
                    case("write")
                       string = command//reg.addr_hex//val
                       stat   = SYSTEM(string)
                       if(stat == -1) then
                          ier = ierrno()
                       end if
                    case default
                       print*, "AccessIA32_PMC0: -- Invalid switch argument!!"
                       return
               end select AccessMSR 
     end subroutine AccessIA32_PMC0
     
end module mod_msr_architectural
