
  include 'GMS_config.fpp'

  module mod_zen_pmc

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         mod_zen_pmc
 !          
 !          Purpose:
 !                   This module contains a derived data types
 !                   describing AMD Zen family 17h CPU-architecture PMC counters.
 !                   CPUID: AMD Processor models 00h-2Fh
 !                   
 !                     
 !          History:
 !                        Date: 19-06-2019
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
    

    use mod_kinds, only : int1, int4, dp
    implicit none

    integer(kind=int4), parameter :: MAX_SAMP = 100

       type, public :: PMC_FPU_PIPE_ASSIGNMENT_ZEN
           public
        
        character(len=5)      :: event = "0x000"
        character(len=20)     :: event_name = "FPU_PIPE_ASSIGNMENT"
        
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num ! number of samples per core
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        !PMCx000 [FPU Pipe Assignment] (Core::X86::Pmc::Core::FpuPipeAssignment)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        !PMCx000 [FPU Pipe Assignment] (Core::X86::Pmc::Core::FpuPipeAssignment)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        !PMCx000 [FPU Pipe Assignment] (Core::X86::Pmc::Core::FpuPipeAssignment)
#endif       
     end type PMC_FPU_PIPE_ASSIGNMENT_ZEN

     type, public :: PMC_FP_SCHED_EMPTY_ZEN
        public
        character(len=5)      :: event = "0x001"
        character(len=14)     :: event_name = "FP_SCHED_EMPTY"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        !PMCx001 [FP Scheduler Empty] (Core::X86::Pmc::Core::FpSchedEmpty)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        !PMCx001 [FP Scheduler Empty] (Core::X86::Pmc::Core::FpSchedEmpty)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        !PMCx001 [FP Scheduler Empty] (Core::X86::Pmc::Core::FpSchedEmpty)
#endif
     end type PMC_FP_SCHED_EMPTY_ZEN

     type, public :: PMC_FP_RET_X86_FPOPS_ZEN
        public
        character(len=5)      :: event = "0x002"
        character(len=16)     :: event_name = "FP_RET_X86_FPOPS"
#if (ZEN_16_CORE) == 1
         integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx002 [Retired x87 Floating Point Operations] (Core::X86::Pmc::Core::FpRetx87FpOps)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx002 [Retired x87 Floating Point Operations] (Core::X86::Pmc::Core::FpRetx87FpOps)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx002 [Retired x87 Floating Point Operations] (Core::X86::Pmc::Core::FpRetx87FpOps)
#endif
     end type PMC_FP_RET_X86_FPOPS_ZEN

     type, public :: PMC_FP_RET_SSE_AVX_OPS_ZEN
        public
        character(len=5)      :: event = "0x003"
        character(len=18)     :: event_name = "FP_RET_SSE_AVX_OPS"
#if (ZEN_16_CORE)
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx003 [Retired SSE/AVX Operations] (Core::X86::Pmc::Core::FpRetSseAvxOps)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx003 [Retired SSE/AVX Operations] (Core::X86::Pmc::Core::FpRetSseAvxOps)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx003 [Retired SSE/AVX Operations] (Core::X86::Pmc::Core::FpRetSseAvxOps)
#endif
     end type PMC_FP_RET_SSE_AVX_OPS_ZEN

     type, public :: PMC_FP_NUM_MOV_ELIM_SCALOP_ZEN
        public
        character(len=5)       :: event = "0x004"
        character(len=22)      :: event_name = "FP_NUM_MOV_ELIM_SCALOP"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx004 [Number of Move Elimination and Scalar Op Optimization]
        !(Core::X86::Pmc::Core::FpNumMovElimScalOp)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx004 [Number of Move Elimination and Scalar Op Optimization]
        !(Core::X86::Pmc::Core::FpNumMovElimScalOp)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx004 [Number of Move Elimination and Scalar Op Optimization]
        !(Core::X86::Pmc::Core::FpNumMovElimScalOp)
#endif
     end type PMC_FP_NUM_MOV_ELIM_SCALOP_ZEN

     type, public :: PMC_FP_RETIRE_SEROPS_ZEN
        public
        character(len=5)       :: event = "0x005"
        character(len=16)      :: event_name = "FP_RETIRE_SEROPS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        !PMCx005 [Retired Serializing Ops] (Core::X86::Pmc::Core::FpRetiredSerOps)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        !PMCx005 [Retired Serializing Ops] (Core::X86::Pmc::Core::FpRetiredSerOps)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        !PMCx005 [Retired Serializing Ops] (Core::X86::Pmc::Core::FpRetiredSerOps)
#endif
     end type PMC_FP_RETIRE_SEROPS_ZEN

     type, public :: PMC_LS_BAD_STATUS2_ZEN
        public
        character(len=5)       :: event = "0x24"
        character(len=14)      :: event_name = "LS_BAD_STATUS2"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx024 [Bad Status 2] (Core::X86::Pmc::Core::LsBadStatus2)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
         real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx024 [Bad Status 2] (Core::X86::Pmc::Core::LsBadStatus2)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx024 [Bad Status 2] (Core::X86::Pmc::Core::LsBadStatus2)
#endif
     end type PMC_LS_BAD_STATUS2_ZEN

     type, public :: PMC_LS_LOCKS_ZEN
        public
        character(len=5)       :: event = "0x25"
        character(len=8)       :: event_name = "LS_LOCKS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx025 [Locks] (Core::X86::Pmc::Core::LsLocks)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx025 [Locks] (Core::X86::Pmc::Core::LsLocks)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx025 [Locks] (Core::X86::Pmc::Core::LsLocks)
#endif
     end type PMC_LS_LOCKS_ZEN

     type, public :: PMC_LS_RET_CLFLUSH_ZEN
        public
        character(len=5)       :: event = "0x26"
        character(len=14)      :: event_name = "LS_RET_CLFLUSH"
#if (ZEN_16_CORE) == 1        
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx026 [Retired CLFLUSH Instructions] (Core::X86::Pmc::Core::LsRetClClush)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx026 [Retired CLFLUSH Instructions] (Core::X86::Pmc::Core::LsRetClClush)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx026 [Retired CLFLUSH Instructions] (Core::X86::Pmc::Core::LsRetClClush)
#endif
     end type PMC_LS_RET_CLFLUSH_ZEN

     type, public :: PMC_LS_RET_CPUID_ZEN
        public
        character(len=5)       :: event = "0x27"
        character(len=12)      :: event_name = "LS_RET_CPUID"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! MCx027 [Retired CPUID Instructions] (Core::X86::Pmc::Core::LsRetCpuid)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! MCx027 [Retired CPUID Instructions] (Core::X86::Pmc::Core::LsRetCpuid)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! MCx027 [Retired CPUID Instructions] (Core::X86::Pmc::Core::LsRetCpuid)
#endif
     end type PMC_LS_RET_CPUID_ZEN

     type, public :: PMC_LS_DISPATCH_ZEN
        public
        character(len=5)       :: event = "0x29"
        character(len=11)      :: event_name = "LS_DISPATCH"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        !PMCx029 [LS Dispatch] (Core::X86::Pmc::Core::LsDispatch)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        !PMCx029 [LS Dispatch] (Core::X86::Pmc::Core::LsDispatch)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        !PMCx029 [LS Dispatch] (Core::X86::Pmc::Core::LsDispatch)
#endif
     end type PMC_LS_DISPATCH_ZEN

     type, public :: PMC_LS_SMI_RX_ZEN
        public
        character(len=5)       :: event = "0x2B"
        character(len=9)       :: event_name = "LS_SMI_RX"
#if (ZEN_16_CORE) == 1        
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        ! PMCx02B [SMIs Received] (Core::X86::Pmc::Core::LsSmiRx)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        ! PMCx02B [SMIs Received] (Core::X86::Pmc::Core::LsSmiRx)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx02B [SMIs Received] (Core::X86::Pmc::Core::LsSmiRx)
#endif
     end type PMC_LS_SMI_RX_ZEN

     type, public :: PMC_LS_STLF_ZEN
        public
        character(len=5)       :: event = "0x35"
        character(len=7)       :: event_name = "LS_STLF"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        ! PMCx035 [Store to Load Forward] (Core::X86::Pmc::Core::LsSTLF)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        ! PMCx035 [Store to Load Forward] (Core::X86::Pmc::Core::LsSTLF)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx035 [Store to Load Forward] (Core::X86::Pmc::Core::LsSTLF)
#endif
     end type PMC_LS_STLF_ZEN

     type, public :: PMC_LS_ST_COMMIT_CANCELS2_ZEN
        public
        character(len=5)       :: event = "0x37"
        character(len=21)      :: event_name = "LS_ST_COMMIT_CANCLES2"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        ! PMCx037 [Store Commit Cancels 2] (Core::X86::Pmc::Core::LsStCommitCancel2)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        ! PMCx037 [Store Commit Cancels 2] (Core::X86::Pmc::Core::LsStCommitCancel2)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx037 [Store Commit Cancels 2] (Core::X86::Pmc::Core::LsStCommitCancel2)
#endif
     end type PMC_LS_ST_COMMIT_CANCELS2_ZEN

     type, public :: PMC_LS_DC_ACCESSES_ZEN
        public
        character(len=5)       :: event = "0x40"
        character(len=14)      :: event_name = "LS_DC_ACCESSES"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
          !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        ! PMCx040 [Data Cache Accesses] (Core::X86::Pmc::Core::LsDcAccesses)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
          !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        ! PMCx040 [Data Cache Accesses] (Core::X86::Pmc::Core::LsDcAccesses)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
          !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx040 [Data Cache Accesses] (Core::X86::Pmc::Core::LsDcAccesses)
#endif
     end type PMC_LS_DC_ACCESSES_ZEN

     type, public :: PMC_LS_REFILLS_FROM_SYS_ZEN
        public
        character(len=5)       :: event = "0x43"
        character(len=18)      :: event_name = "LS_REFILLS_FROM_SYS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        ! PMCx043 [Data Cache Refills from System] (Core::X86::Pmc::Core::LsRefillsFromSys)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        ! PMCx043 [Data Cache Refills from System] (Core::X86::Pmc::Core::LsRefillsFromSys)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx043 [Data Cache Refills from System] (Core::X86::Pmc::Core::LsRefillsFromSys)
#endif
     end  type  PMC_LS_REFILLS_FROM_SYS_ZEN

     type, public :: PMC_LS_L1_DTLB_MISS_ZEN
        public
        character(len=5)      :: event = "0x45"
        character(len=14)     :: event_name = "LS_L1_DTLB_MISS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        !PMCx045 [L1 DTLB Miss] (Core::X86::Pmc::Core::LsL1DTlbMiss)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        !PMCx045 [L1 DTLB Miss] (Core::X86::Pmc::Core::LsL1DTlbMiss)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        !PMCx045 [L1 DTLB Miss] (Core::X86::Pmc::Core::LsL1DTlbMiss)
#endif
     end type PMC_LS_L1_DTLB_MISS_ZEN

     type, public :: PMC_LS_TABLE_WALKER_ZEN
        public
        character(len=5)      :: event = "0x46"
        character(len=15)     :: event_name = "LS_TABLE_WALKER"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        !PMCx046 [Tablewalker allocation] (Core::X86::Pmc::Core::LsTablewalker)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        !PMCx046 [Tablewalker allocation] (Core::X86::Pmc::Core::LsTablewalker)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        !PMCx046 [Tablewalker allocation] (Core::X86::Pmc::Core::LsTablewalker)
#endif
     end type PMC_LS_TABLE_WALKER_ZEN

     type, public :: PMC_LS_MISALIGN_ACCESS_ZEN
        public
        character(len=5)      :: event = "0x47"
        character(len=16)     :: event_name = "LS_MISALIGN_ACCESS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        ! PMCx047 [Misaligned loads] (Core::X86::Pmc::Core::LsMisalAccesses)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        ! PMCx047 [Misaligned loads] (Core::X86::Pmc::Core::LsMisalAccesses)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
         !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
       integer(kind=int1), dimension(1000,0:31) :: pmc_read
       ! PMCx047 [Misaligned loads] (Core::X86::Pmc::Core::LsMisalAccesses)
#endif
     end type PMC_LS_MISALIGN_ACCESS_ZEN

     type, public :: PMC_LS_PREF_INSTR_DISPATCH_ZEN
        public
        character(len=5)      :: event = "0x4B"
        character(len=21)     :: event_name = "LS_PERF_INSTR_DISPATCH"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
         real(kind=dp), dimension(0:15) :: samp_delta
          !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        ! PMCx04B [Prefetch Instructions Dispatched] (Core::X86::Pmc::Core::LsPrefInstrDisp)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
          !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        ! PMCx04B [Prefetch Instructions Dispatched] (Core::X86::Pmc::Core::LsPrefInstrDisp)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
          !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx04B [Prefetch Instructions Dispatched] (Core::X86::Pmc::Core::LsPrefInstrDisp)
#endif
     end type PMC_LS_PREF_INSTR_DISPATCH_ZEN

     type, public :: PMC_LS_INEF_SW_PREF_ZEN
        public
        character(len=5)       :: event = "0x52"
        character(len=15)      :: event_name = "LS_INEF_SW_PREF"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
          !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        ! PMCx052 [Ineffective Software Prefetchs] (Core::X86::Pmc::Core::LsInefSwPref)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
          !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        ! PMCx052 [Ineffective Software Prefetchs] (Core::X86::Pmc::Core::LsInefSwPref)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
          !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx052 [Ineffective Software Prefetchs] (Core::X86::Pmc::Core::LsInefSwPref)
#endif
     end type PMC_LS_INEF_SW_PREF_ZEN

     type, public :: PMC_LS_SW_PREF_DC_FILLS_ZEN
        public
        character(len=5)       :: event = "0x59"
        character(len=19)      :: event_name = "LS_SW_PREF_DC_FILLS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        ! PMCx059 [Software Prefetch Data Cache Fills] (Core::X86::Pmc::Core::LsSwPfDcFills)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        ! PMCx059 [Software Prefetch Data Cache Fills] (Core::X86::Pmc::Core::LsSwPfDcFills)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx059 [Software Prefetch Data Cache Fills] (Core::X86::Pmc::Core::LsSwPfDcFills)
#endif
     end type PMC_LS_SW_PREF_DC_FILLS_ZEN

     type, public :: PMC_LS_HW_PF_DC_FILLS_ZEN
        public
        character(len=5)       :: event = "0x5A"
        character(len=17)      :: event_name = "LS_HW_PF_DC_FILLS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        ! PMCx05A [Hardware Prefetch Data Cache Fills] (Core::X86::Pmc::Core::LsHwPfDcFills)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        ! PMCx05A [Hardware Prefetch Data Cache Fills] (Core::X86::Pmc::Core::LsHwPfDcFills)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx05A [Hardware Prefetch Data Cache Fills] (Core::X86::Pmc::Core::LsHwPfDcFills)
#endif
     end type PMC_LS_HW_PF_DC_FILLS_ZEN

     type, public :: PMC_TW_DC_FILLS_ZEN
        public
        character(len=5)        :: event = "0x5B"
        character(len=11)       :: event_name = "TW_DC_FILLS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:15) :: pmc_read
        ! PMCx05B [Table Walker Data Cache Fills by Data Source] (Core::X86::Pmc::Core::LsTwDcFills)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:23) :: pmc_read
        ! PMCx05B [Table Walker Data Cache Fills by Data Source] (Core::X86::Pmc::Core::LsTwDcFills)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(1000,0:31) :: pmc_read
        ! PMCx05B [Table Walker Data Cache Fills by Data Source] (Core::X86::Pmc::Core::LsTwDcFills)
#endif
     end type PMC_TW_DC_FILLS_ZEN

     type, public :: PMC_LS_NOT_HALTED_CYC_ZEN
        public
        character(len=5)        :: event = "0x76"
        character(len=16)       :: event_name = "LS_NOT_HALTED_CYC"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx076 [Cycles not in Halt] (Core::X86::Pmc::Core::LsNotHaltedCyc)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx076 [Cycles not in Halt] (Core::X86::Pmc::Core::LsNotHaltedCyc)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx076 [Cycles not in Halt] (Core::X86::Pmc::Core::LsNotHaltedCyc)
#endif
     end type PMC_LS_NOT_HALTED_CYC_ZEN

     type, public :: PMC_IC_FW32_ZEN
        public
        character(len=5)        :: event = "0x80"
        character(len=7)        :: event_name = "IC_FW32"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx080 [32 Byte Instruction Cache Fetch] (Core::X86::Pmc::Core::IcFw32)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx080 [32 Byte Instruction Cache Fetch] (Core::X86::Pmc::Core::IcFw32)
#endif
     end type PMC_IC_FW32_ZEN

     type, public :: PMC_IC_FW32_MISS_ZEN
        public
        character(len=5)        :: event = "0x81"
        character(len=12)       :: event_name = "IC_FW32_MISS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx081 [32 Byte Instruction Cache Misses] (Core::X86::Pmc::Core::IcFw32Miss)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx081 [32 Byte Instruction Cache Misses] (Core::X86::Pmc::Core::IcFw32Miss)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx081 [32 Byte Instruction Cache Misses] (Core::X86::Pmc::Core::IcFw32Miss)
#endif
     end type PMC_IC_FW32_MISS_ZEN

     type, public :: PMC_IC_CACHE_FILL2_ZEN
        public
        character(len=5)        :: event = "0x82"
        character(len=14)       :: event_name = "IC_CACHE_FILL2"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx082 [Instruction Cache Refills from L2] (Core::X86::Pmc::Core::IcCacheFillL2)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx082 [Instruction Cache Refills from L2] (Core::X86::Pmc::Core::IcCacheFillL2)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx082 [Instruction Cache Refills from L2] (Core::X86::Pmc::Core::IcCacheFillL2)
#endif
     end type PMC_IC_CACHE_FILL2_ZEN

     type, public :: PMC_IC_REFILL_SYS_ZEN
        public
        character(len=4)        :: event = "0x83"
        character(len=13)       :: event_name = "IC_REFILL_SYS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx083 [Instruction Cache Refills from System] (Core::X86::Pmc::Core::IcCacheFillSys)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx083 [Instruction Cache Refills from System] (Core::X86::Pmc::Core::IcCacheFillSys)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx083 [Instruction Cache Refills from System] (Core::X86::Pmc::Core::IcCacheFillSys)
#endif
      
     end type PMC_IC_REFILL_SYS_ZEN

     type, public :: PMC_L1_ITLB_MISS_L2_ITLB_HIT_ZEN
        public
        character(len=4)         :: event = "0x84"
        character(len=24)        :: event_name = "L1_ITLB_MISS_L2_ITLB_HIT"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
         ! PMCx084 [L1 ITLB Miss, L2 ITLB Hit] (Core::X86::Pmc::Core::BpL1TlbMissL2Hit)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
         ! PMCx084 [L1 ITLB Miss, L2 ITLB Hit] (Core::X86::Pmc::Core::BpL1TlbMissL2Hit)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx084 [L1 ITLB Miss, L2 ITLB Hit] (Core::X86::Pmc::Core::BpL1TlbMissL2Hit)
#endif        
       
       
     end type PMC_L1_ITLB_MISS_L2_ITLB_HIT_ZEN

      type, public :: PMC_L1_ITLB_MISS_L2_ITLB_MISS_ZEN
        public
        character(len=4)         :: event = "0x85"
        character(len=25)        :: event_name = "L1_ITLB_MISS_L2_ITLB_MISS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
         ! PMCx085 [L1 ITLB Miss, L2 ITLB Miss] (Core::X86::Pmc::Core::BpL1TlbMissL2Hit)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
         ! PMCx085 [L1 ITLB Miss, L2 ITLB Miss] (Core::X86::Pmc::Core::BpL1TlbMissL2Hit)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx085 [L1 ITLB Miss, L2 ITLB Miss] (Core::X86::Pmc::Core::BpL1TlbMissL2Hit)
#endif        
       
       
     end type PMC_L1_ITLB_MISS_L2_ITLB_MISS_ZEN

     type, public :: PMC_IC_FETCH_STALL_ZEN
        public
        character(len=4)         :: event = "0x87"
        character(len=14)        :: event_name = "IC_FETCH_STALL"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
          ! PMCx087 [Instruction Pipe Stall] (Core::X86::Pmc::Core::IcFetchStall)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
          ! PMCx087 [Instruction Pipe Stall] (Core::X86::Pmc::Core::IcFetchStall)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
          ! PMCx087 [Instruction Pipe Stall] (Core::X86::Pmc::Core::IcFetchStall)
#endif              
       
     end type PMC_IC_FETCH_STALL_ZEN

     type, public :: PMC_L1_BTB_CORRECT_ZEN
        public
        character(len=4)        :: event = "0x8A"
        character(len=13)       :: event_name = "L1_BTB_CORRECT"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx08A [L1 BTB Correction] (Core::X86::Pmc::Core::BpL1BTBCorrect)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
         ! PMCx08A [L1 BTB Correction] (Core::X86::Pmc::Core::BpL1BTBCorrect)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
         ! PMCx08A [L1 BTB Correction] (Core::X86::Pmc::Core::BpL1BTBCorrect)
#endif          
       
        
     end type PMC_L1_BTB_CORRECT_ZEN

     type, public :: PMC_L2_BTB_CORRECT_ZEN
        public
        character(len=4)        :: event = "0x8B"
        character(len=13)       :: event_name = "L2_BTB_CORRECT"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
       !PMCx08B [L2 BTB Correction] (Core::X86::Pmc::Core::BpL2BTBCorrect)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        !PMCx08B [L2 BTB Correction] (Core::X86::Pmc::Core::BpL2BTBCorrect)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        !PMCx08B [L2 BTB Correction] (Core::X86::Pmc::Core::BpL2BTBCorrect)
#endif              
       
        
     end type PMC_L2_BTB_CORRECT_ZEN

     type, public :: PMC_IC_LINES_INVALIDATED_ZEN
        public
        character(len=4)        :: event = "0x8C"
        character(len=19)       :: event_name = "IC_LINES_INVALIDATED"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
         ! PMCx08C [Instruction Cache Lines Invalidated] (Core::X86::Pmc::Core::IcCacheInval)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx08C [Instruction Cache Lines Invalidated] (Core::X86::Pmc::Core::IcCacheInval)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx08C [Instruction Cache Lines Invalidated] (Core::X86::Pmc::Core::IcCacheInval)
#endif              
               
      
     end type PMC_IC_LINES_INVALIDATED_ZEN

     type, public :: PMC_ITLB_RELOADS_ZEN
        public
        character(len=4)         :: event = "0x99"
        character(len=12)        :: event_name = "ITLB_RELOADS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx099 [ITLB Reloads] (Core::X86::Pmc::Core::BpTlbRel)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx099 [ITLB Reloads] (Core::X86::Pmc::Core::BpTlbRel)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx099 [ITLB Reloads] (Core::X86::Pmc::Core::BpTlbRel)
#endif         
       
       
     end type PMC_ITLB_RELOADS_ZEN

     type, public :: PMC_IC_OC_MODE_SWITCH_ZEN
        public
        character(len=5)         :: event = "0x28A"
        character(len=16)        :: event_name = "IC_OC_MODE_SWITCH"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
       
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
       
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
       
#endif           
       
     end type PMC_IC_OC_MODE_SWITCH_ZEN

     type, public :: PMC_DE_DISPATCH_TOKEN_STALLS0_ZEN
        public
        character(len=4)         :: event = "0xAF"
        character(len=23)        :: event_name = "DE_DISPATCH_TOKEN_STALLS0"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
         ! PMCx0AF [Dynamic Tokens Dispatch Stall Cycles 0] (Core::X86::Pmc::Core::DeDisDispatchTokenStalls0)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx0AF [Dynamic Tokens Dispatch Stall Cycles 0] (Core::X86::Pmc::Core::DeDisDispatchTokenStalls0)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx0AF [Dynamic Tokens Dispatch Stall Cycles 0] (Core::X86::Pmc::Core::DeDisDispatchTokenStalls0)
#endif                  
       
       
     end type PMC_DE_DISPATCH_TOKEN_STALLS0_ZEN

     type, public :: PMC_RET_INSTR_ZEN
        public
        character(len=4)         :: event = "0xC0"
        character(len=9)         :: event_name = "RET_INSTR"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
         ! PMCx0C0 [Retired Instructions] (Core::X86::Pmc::Core::ExRetInstr)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx0C0 [Retired Instructions] (Core::X86::Pmc::Core::ExRetInstr)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx0C0 [Retired Instructions] (Core::X86::Pmc::Core::ExRetInstr)
#endif            
        
       
     end type PMC_RET_INSTR_ZEN
     
     type, public :: PMC_RETIRED_UOPS_ZEN
        public
        character(len=4)         :: event = "0xC1"
        character(len=12)        :: event_name = "RETIRED_UOPS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
          ! PMCx0C1 [Retired Uops] (Core::X86::Pmc::Core::ExRetCops)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
         ! PMCx0C1 [Retired Uops] (Core::X86::Pmc::Core::ExRetCops)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
         ! PMCx0C1 [Retired Uops] (Core::X86::Pmc::Core::ExRetCops)
#endif            
       
      
     end type PMC_RETIRED_UOPS_ZEN
      
     type, public :: PMC_RETIRED_BRANCH_INSTR_ZEN
        public
        character(len=4)         :: event = "0xC2"
        character(len=20)        :: event_name = "RETIRED_BRANCH_INSTR"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
          ! PMCx0C2 [Retired Branch Instructions] (Core::X86::Pmc::Core::ExRetBrn)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
         ! PMCx0C2 [Retired Branch Instructions] (Core::X86::Pmc::Core::ExRetBrn)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
         ! PMCx0C2 [Retired Branch Instructions] (Core::X86::Pmc::Core::ExRetBrn)
#endif          
       
       
     end type PMC_RETIRED_BRANCH_INSTR_ZEN

     type, public :: PMC_RETIRED_BRANCH_INSTR_MISPRED_ZEN
        public
        character(len=4)          :: event = "0xC3"
        character(len=27)         :: event_name = "RETIRED_BRANCH_INSTR_MISPRED"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        ! PMCx0C3 [Retired Branch Instructions Mispredicted] (Core::X86::Pmc::Core::ExRetBrnMisp)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
         ! PMCx0C3 [Retired Branch Instructions Mispredicted] (Core::X86::Pmc::Core::ExRetBrnMisp)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
         ! PMCx0C3 [Retired Branch Instructions Mispredicted] (Core::X86::Pmc::Core::ExRetBrnMisp)
#endif          
      
       
     end type PMC_RETIRED_BRANCH_INSTR_MISPRED_ZEN

     type, public :: PMC_RETIRED_TAKEN_BRANCH_INSTR_ZEN
        public
        character(len=4)           :: event = "0xC4"
        character(len=25)          :: event_name = "RETIRED_TAKEN_BRANCH_INSTR"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
         ! PMCx0C4 [Retired Taken Branch Instructions] (Core::X86::Pmc::Core::ExRetBrnTkn)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
         ! PMCx0C4 [Retired Taken Branch Instructions] (Core::X86::Pmc::Core::ExRetBrnTkn)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
         ! PMCx0C4 [Retired Taken Branch Instructions] (Core::X86::Pmc::Core::ExRetBrnTkn)
#endif          
        
       
     end type PMC_RETIRED_TAKEN_BRANCH_INSTR_ZEN

     type, public :: PMC_RETIRED_TAKEN_BRANCH_INSTR_MISPRED_ZEN
        public
        character(len=4)           :: event = "0xC5"
        character(len=32)          :: event_name = "RETIRED_TAKEN_BRANCH_INSTR_MISPRED"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
         ! PMCx0C5 [Retired Taken Branch Instructions Mispredicted] (Core::X86::Pmc::Core::ExRetBrnTknMisp)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx0C5 [Retired Taken Branch Instructions Mispredicted] (Core::X86::Pmc::Core::ExRetBrnTknMisp)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
         ! PMCx0C5 [Retired Taken Branch Instructions Mispredicted] (Core::X86::Pmc::Core::ExRetBrnTknMisp)
#endif 
       
       
     end type PMC_RETIRED_TAKEN_BRANCH_INSTR_MISPRED_ZEN

     type, public :: PMC_RETIRED_FAR_CTRL_TRANSFERS_ZEN
        public
        character(len=4)           :: event = "0xC6"
        character(len=25)          :: event_name = "RETIRED_FAR_CTRL_TRANSFERS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
        
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
      
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        
#endif 
      
     end type PMC_RETIRED_FAR_CTRL_TRANSFERS_ZEN

     type, public :: PMC_RETIRED_BRANCH_RESYNCS_ZEN
        public
        character(len=4)           :: event = "0xC7"
        character(len=22)          :: event_name = "RETIRED_BRANCH_RESYNCS"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
         ! PMCx0C7 [Retired Branch Resyncs] (Core::X86::Pmc::Core::ExRetBrnResync)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx0C7 [Retired Branch Resyncs] (Core::X86::Pmc::Core::ExRetBrnResync)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
         ! PMCx0C7 [Retired Branch Resyncs] (Core::X86::Pmc::Core::ExRetBrnResync)
#endif         
       
       
     end type PMC_RETIRED_BRANCH_RESYNCS_ZEN

     type, public :: PMC_RETIRED_NEAR_RET_ZEN
        public
        character(len=4)           :: event = "0xC8"
        character(len=16)          :: event_name = "RETIRED_NEAR_RET"
#if (ZEN_16_CORE) == 1
        integer(kind=int4), dimension(0:15) :: samp_num
        real(kind=dp), dimension(0:15) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:15) :: pmc_read
         ! PMCx0C8 [Retired Near Returns] (Core::X86::Pmc::Core::ExRetNearRet)
#elif (ZEN_24_CORE) == 1
        integer(kind=int4), dimension(0:23) :: samp_num
        real(kind=dp), dimension(0:23) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:23) :: pmc_read
        ! PMCx0C8 [Retired Near Returns] (Core::X86::Pmc::Core::ExRetNearRet)
#elif (ZEN_32_CORE) == 1
        integer(kind=int4), dimension(0:31) :: samp_num
        real(kind=dp), dimension(0:31) :: samp_delta
        !DIR$ ATTRIBUTES ALIGN : 64 :: pmc_read
        integer(kind=int1), dimension(MAX_SAMP,0:31) :: pmc_read
        ! PMCx0C8 [Retired Near Returns] (Core::X86::Pmc::Core::ExRetNearRet)
#endif          
       
       
     end type PMC_RETIRED_NEAR_RET_ZEN










  end module mod_zen_pmc
