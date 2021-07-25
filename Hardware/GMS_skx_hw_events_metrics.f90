

module skx_hw_events_metrics

!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'skx_hw_events_metrics'
 !          
 !          Purpose:
 !                   Skylake-X hardware performance events metrics formulae.
 !          History:
 !                        Date: 25/07/2021
 !                        Time: 09:23 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !         
 !
 !          Author:  
 !                  Bernard Gingold
 !                 
 !          References:
 !         
 !                 Intel processsor performance events documentation.
 !    
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.


     use module, only : i4, dp
     use omp_lib 
     implicit none
     public

    !=====================================================59
    !  File and module information:
    !  version,creation and build date, author,description
    !=====================================================59
    
     ! Major version
     integer(i4), parameter :: SKX_HW_EVENTS_METRICS_MAJOR = 1
     ! Minor version
     integer(i4), parameter :: SKX_HW_EVENTS_METRICS_MINOR = 0
     ! Micro version
     integer(i4), parameter :: SKX_HW_EVENTS_METRICS_MICRO = 0
     ! Full version
     integer(i4), parameter :: SKX_HW_EVENTS_METRICS_FULLVER = 1000*SKX_HW_EVENTS_METRICS_MAJOR+ &
                                                               100*SKX_HW_EVENTS_METRICS_MINOR+  &
                                                               10*SKX_HW_EVENTS_METRICS_MICRO
     ! Module creation date
     character(*), parameter :: SKX_HW_EVENTS_METRICS_CREATE_DATE = "25-07-2021 09:23 +00200 (SUN 25 JUN 2021 GMT+2)"
     ! Module build date
     character(*), parameter :: SKX_HW_EVENTS_METRICS_BUILD_DATE = __DATE__ ":" __TIME__
     ! Module author info
     character(*), parameter :: SKX_HW_EVENTS_METRICS_AUTHOR = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Module short description
     character(*), parameter :: SKX_HW_EVENTS_METRICS_INFO  = "Skylake-X hardware performance events metrics formulae."

     ! Module constants

     real(kind=dp), parameter, private :: HUNDRED = 100.0_dp
     real(kind=dp), parameter, private :: ONE     = 1.0_dp
     real(kind=dp), parameter, private :: TWO     = 2.0_dp
     real(kind=dp), parameter, private :: ZERO    = 0.0_dp
     real(kind=dp), parameter, private :: HALF    = 0.5_dp
     real(kind=dp), parameter, private :: BILLION = 1000000000.0_dp

     contains
     
       !
       !  CPU operating frequency (in GHz)
       !
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))     
     real(kind=dp) function skx_cpu_freq(CPU_CLK_UNHALTED_THREAD, &
                                         CPU_CLK_UNHALTED_REF_TSC,&
                                         TSC_FREQ) !GCC$ ATTRIBUTES inline :: skx_cpu_freq !GCC$ ATTRIBUTES aligned(32) :: skx_cpu_freq
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     real(kind=dp) function skx_cpu_freq(CPU_CLK_UNHALTED_THREAD, &
                                         CPU_CLK_UNHALTED_REF_TSC,&
                                         TSC_FREQ)
         !DIR$ ATTRIBUTES FORCEINLINE :: skx_cpu_freq
         !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: skx_cpu_freq
         !DIR$ OPTIMIZE : 3
#endif
       !$OMP DECLARE SIMD(skx_cpu_freq)
       real(kind=dp),   intent(in) :: CPU_CLK_UNHALTED_THREAD
       real(kind=dp),   intent(in) :: CPU_CLK_UNHALTED_REF_TSC
       real(kind=dp),   intent(in) :: TSC_FREQ
       ! Exec statements
       skx_cpu_freq = (CPU_CLK_UNHALTED_THREAD/
                       CPU_CLK_UNHALTED_REF_TSC*TSC_FREQ)/ &
                       BILLION
     end function skx_cpu_freq
                     
       !
       !  CPU utilization (percentage) all cores.
       !
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     real(kind=dp) function skx_cpu_util(CPU_CLK_UNHALTED_THREAD, &
                                         TIME_STAMP_CYCLES) !GCC$ ATTRIBUTES inline :: skx_cpu_util !GCC$ ATTRIBUTES aligned(32) :: skx_cpu_util
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     real(kind=dp) function skx_cpu_util(CPU_CLK_UNHALTED_THREAD, &
                                         TIME_STAMP_CYCLES)
           !DIR$ ATTRIBUTES FORCEINLINE :: skx_cpu_util
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: skx_cpu_util
           !DIR$ OPTIMIZE : 3
#endif
       !$OMP DECLARE SIMD(skx_cpu_util)
       real(kind=dp),   intent(in) :: CPU_CLK_UNHALTED_THREAD
       real(kind=dp),   intent(in) :: TIME_STAMP_CYCLES
       !Exec
       skx_cpu_util = HUNDRED*CPU_CLK_UNHALTED_THREAD/ &
                      TIME_STAMP_CYCLES
     end function

      !               
      ! CPU utilization (percentage) in kernel mode (all cores).
      !
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     real(kind=dp) function skx_cpu_util_ring0(CPU_CLK_UNHALTED_REF_TSC_SUP, &
                                               TIME_STAMP_CYCLES) !GCC$ ATTRIBUTES inline :: skx_cpu_util_ring0 !GCC$ ATTRIBUTES aligned(32) :: skx_cpu_util_ring0
#elif defined(__INTEL_COMPILER) || defined(__ICC)
     real(kind=dp) function skx_cpu_util_ring0(CPU_CLK_UNHALTED_REF_TSC_SUP, &
                                               TIME_STAMP_CYCLES)
           !DIR$ ATTRIBUTES FORCEINLINE :: skx_cpu_util_ring0
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: skx_cpu_util_ring0
           !DIR$ OPTIMIZE : 3
#endif
       !$OMP DECLARE SIMD(skx_cpu_util_ring0)
           real(kind=dp),   intent(in) :: CPU_CLK_UNHALTED_REF_TSC_SUP
           real(kind=dp),   intent(in) :: TIME_STAMP_CYCLES
           !Exec
           skx_cpu_util_ring0 = HUNDRED*CPU_CLK_UNHALTED_REF_TSC_SUP/ &
                            TIME_STAMP_CYCLES

     end function 

      !
      !  Cycles Per Instruction (CPI).
      !
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     real(kind=dp) function skx_cpi(CPU_CLK_UNHALTED_THREAD, &
                                   INST_RETIRED_ANY) !GCC$ ATTRIBUTES inline :: skx_cpi !GCC$ ATTRIBUTES aligned(32) :: skx_cpi
#elif defined(__INTEL_COMPILER) || defined(__ICC)       
     real(kind=dp) function skx_cpi(CPU_CLK_UNHALTED_THREAD, &
                                   INST_RETIRED_ANY)
           !DIR$ ATTRIBUTES FORCEINLINE :: skx_cpi
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: skx_cpi
           !DIR$ OPTIMIZE : 3
#endif
       !$OMP DECLARE SIMD(skx_cpi)
            real(kind=dp),   intent(in) :: CPU_CLK_UNHALTED_THREAD
            real(kind=dp),   intent(in) :: INST_RETIRED_ANY
            !Exec
            skx_cpi = CPU_CLK_UNHALTED_THREAD/ &
                      INST_RETIRED_ANY
     end function 

      !
      !   Cycles Per Instruction (CPI) kernel mode.
      !
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     real(kind=dp) function skx_cpi_ring0(CPU_CLK_UNHALTED_THREAD_SUP, &
                                   INST_RETIRED_ANY) !GCC$ ATTRIBUTES inline :: skx_cpi_ring0 !GCC$ ATTRIBUTES aligned(32) :: skx_cpi_ring0
#elif defined(__INTEL_COMPILER) || defined(__ICC)       
     real(kind=dp) function skx_cpi_ring0(CPU_CLK_UNHALTED_THREAD_SUP, &
                                   INST_RETIRED_ANY)
           !DIR$ ATTRIBUTES FORCEINLINE :: skx_cpi_ring0
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: skx_cpi_ring0
           !DIR$ OPTIMIZE : 3
#endif
       !$OMP DECLARE SIMD(skx_cpi_ring0)
            real(kind=dp),   intent(in) :: CPU_CLK_UNHALTED_THREAD_SUP
            real(kind=dp),   intent(in) :: INST_RETIRED_ANY
            !Exec
            skx_cpi = CPU_CLK_UNHALTED_THREAD_SUP/ &
                      INST_RETIRED_ANY
     end function 

      !
      !  EMON event multiplexing reliability (>95% --- means satisfying ratio).
      !
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     real(kind=dp) function skx_mux_reliable(CPU_CLK_UNHALTED_THREAD_P, &
                                             CPU_CLK_UNHALTED_THREAD) !GCC$ ATTRIBUTES inline :: skx_mux_reliable !GCC$ ATTRIBUTES aligned(32) :: skx_mux_reliable
#elif defined(__INTEL_COMPILER) || defined(__ICC)          
     real(kind=dp) function skx_mux_reliable(CPU_CLK_UNHALTED_THREAD_P, &
                                             CPU_CLK_UNHALTED_THREAD)
           !DIR$ ATTRIBUTES FORCEINLINE :: skx_mux_reliable
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: skx_mux_reliable
           !DIR$ OPTIMIZE : 3  
#endif
        !$OMP DECLARE SIMD(skx_mux_reliable)
           real(kind=dp),     intent(in) :: CPU_CLK_UNHALTED_THREAD_P
           real(kind=dp),     intent(in) :: CPU_CLK_UNHALTED_THREAD
           !Exec
           if(CPU_CLK_UNHALTED_THREAD_P- &
              CPU_CLK_UNHALTED_THREAD < ZERO) then
                      skx_mux_reliable = CPU_CLK_UNHALTED_THREAD_P/ &
                                         CPU_CLK_UNHALTED_THREAD
           else
                      skx_mux_reliable = CPU_CLK_UNHALTED_THREAD/ &
                                         CPU_CLK_UNHALTED_THREAD_P
           endif           
     end function

         !
         !  Branch mispredict ratio. 
         !
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     real(kind=dp) function skx_branch_mispred(BR_MISP_RETIRED_ALL_BRANCHES, &
                                               BR_INST_RETIRED_ALL_BRANCHES) !GCC$ ATTRIBUTES inline :: skx_branch_mispred !GCC$ ATTRIBUTES aligned(32) :: skx_branch_mispred
#elif defined(__INTEL_COMPILER) || defined(__ICC)  
     real(kind=dp) function skx_branch_mispred(BR_MISP_RETIRED_ALL_BRANCHES, &
                                               BR_INST_RETIRED_ALL_BRANCHES)
           !DIR$ ATTRIBUTES FORCEINLINE :: skx_branch_mispred
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: skx_branch_mispred
           !DIR$ OPTIMIZE : 3    
#endif
           !$OMP DECLARE SIMD(skx_branch_mispred)
           real(kind=dp),     intent(in) :: BR_MISP_RETIRED_ALL_BRANCHES
           real(kind=dp),     intent(in) :: BR_INST_RETIRED_ALL_BRANCHES
           ! Exec
           skx_branch_mispred =  BR_MISP_RETIRED_ALL_BRANCHES/ &
                                 BR_INST_RETIRED_ALL_BRANCHES
     end function

         !
         !    Loads per instruction.
         !
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     real(kind=dp) function skx_loads_per_inst(MEM_INST_RETIRED_ALL_LOADS, &
                                               INST_RETIRED_ANY) !GCC$ ATTRIBUTES inline :: skx_loads_per_inst !GCC$ ATTRIBUTES aligned(32) :: skx_loads_per_inst
#elif defined(__INTEL_COMPILER) || defined(__ICC)        
      real(kind=dp) function skx_loads_per_inst(MEM_INST_RETIRED_ALL_LOADS, &
                                               INST_RETIRED_ANY)
           !DIR$ ATTRIBUTES FORCEINLINE :: skx_loads_per_inst
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: skx_loads_per_inst
           !DIR$ OPTIMIZE : 3  
#endif
           !$OMP DECLARE SIMD(skx_loads_per_inst)
           real(kind=dp),     intent(in) :: MEM_INST_RETIRED_ALL_LOADS
           real(kind=dp),     intent(in) :: INST_RETIRED_ANY
           !EXEC
           skx_loads_per_inst = MEM_INST_RETIRED_ALL_LOADS/ &
                                INST_RETIRED_ANY
      end function

         !
         ! Stores per inst
         !
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     real(kind=dp) function skx_stores_per_inst(MEM_INST_RETIRED_ALL_STORES, &
                                               INST_RETIRED_ANY) !GCC$ ATTRIBUTES inline :: skx_stores_per_inst !GCC$ ATTRIBUTES aligned(32) :: skx_stores_per_inst
#elif defined(__INTEL_COMPILER) || defined(__ICC)        
      real(kind=dp) function skx_stores_per_inst(MEM_INST_RETIRED_ALL_STORES, &
                                               INST_RETIRED_ANY)
           !DIR$ ATTRIBUTES FORCEINLINE :: skx_stores_per_inst
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: skx_stores_per_inst
           !DIR$ OPTIMIZE : 3  
#endif
           !$OMP DECLARE SIMD(skx_loads_per_inst)
           real(kind=dp),     intent(in) :: MEM_INST_RETIRED_ALL_STORES
           real(kind=dp),     intent(in) :: INST_RETIRED_ANY
           !EXEC
           skx_loads_per_inst = MEM_INST_RETIRED_ALL_STORES/ &
                                INST_RETIRED_ANY
      end function

         !
         !  Memory operations per instruction.
         !
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     real(kind=dp) function skx_mem_ops_inst(MEM_INST_RETIRED_ALL_LOADS, &
                                             MEM_INST_RETIRED_ALL_STORES, &
                                             INST_RETIRED_ANY) !GCC$ ATTRIBUTES inline :: skx_mem_ops_inst !GCC$ ATTRIBUTES aligned(32) :: skx_mem_ops_inst
#elif defined(__INTEL_COMPILER) || defined(__ICC)  
     real(kind=dp) function skx_mem_ops_inst(MEM_INST_RETIRED_ALL_LOADS, &
                                             MEM_INST_RETIRED_ALL_STORES, &
                                             INST_RETIRED_ANY)
           !DIR$ ATTRIBUTES FORCEINLINE :: skx_mem_ops_inst
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: skx_mem_ops_inst
           !DIR$ OPTIMIZE : 3  
#endif
           !$OMP DECLARE SIMD(skx_mem_ops_inst)
           real(kind=dp),     intent(in) :: MEM_INST_RETIRED_ALL_LOADS
           real(kind=dp),     intent(in) :: MEM_INST_RETIRED_ALL_STORES
           real(kind=dp),     intent(in) :: INST_RETIRED_ANY
           !Locals
           real(kind=dp), automatic :: t0 = 0.0_dp
           t0 =  MEM_INST_RETIRED_ALL_LOADS + &
                MEM_INST_RETIRED_ALL_STORES
           skx_mem_ops_inst = t0/INST_RETIRED_ANY
     end function 

         !
         !  Locks retired per instruction.
         !
#if defined(__GFORTRAN__) && (!defined(__ICC) || !defined(__INTEL_COMPILER))
     real(kind=dp) function skx_locks_per_inst(MEM_INST_RETIRED_LOCK_LOADS, &
                                               INST_RETIRED_ANY) !GCC$ ATTRIBUTES inline :: skx_locks_per_inst !GCC$ ATTRIBUTES aligned(32) :: skx_locks_per_inst
#elif defined(__INTEL_COMPILER) || defined(__ICC)  
     real(kind=dp) function skx_locks_per_inst(MEM_INST_RETIRED_LOCK_LOADS, &
                                               INST_RETIRED_ANY)
           !DIR$ ATTRIBUTES FORCEINLINE :: skx_locks_per_inst
           !DIR$ ATTRIBUTES CODE_ALIGN : 32 :: skx_locks_per_inst
           !DIR$ OPTIMIZE : 3   
#endif
           !$OMP DECLARE SIMD(skx_locks_per_inst)
           real(kind=dp),       intent(in) :: MEM_INST_RETIRED_LOCK_LOADS
           real(kind=dp),       intent(in) :: INST_RETIRED_ANY 
           !EXEC
           skx_locks_per_inst = MEM_INST_RETIRED_LOCK_LOADS/ &
                                INST_RETIRED_ANY
     end function





































     
     
end module skx_hw_events_metrics
