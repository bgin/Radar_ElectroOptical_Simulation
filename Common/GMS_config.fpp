


#if defined __INTEL_COMPILER
   #define GMS_COMPILED_BY_IFORT 1
#elif defined __GFORTRAN__ 
   
   #define GMS_COMPILED_BY_GFORTRAN 1
#else
   #error Unsupported Fortran Compiler detected
#endif

 #if (defined (_M_AMD64) || defined (_M_X64_) || defined (__amd64) ) \
        	&& !defined (__x86_64__)
    #define __x86_64__ 1
#else
    #define __x86_64__ 0
#endif

#if (__x86_64__) == 1
   #define GMS_HAS_FULL_GPR_SET 16
#elif (__x86_64__) == 0
   #define GMS_HAS_FULL_GPR_SET 8
#else
   #error "COMPILE_TIME_ERROR: Cannot determine 32bit or 64bit mode!"
#endif

/* Determine architectural support for full set
of SIMD registers*/
#if (__x86_64_)_ == 1
   #define GMS_HAS_FULL_SIMD_REGSET 32
#elif (__x86_64__) == 0
   #define GMS_HAS_FULL_SIMD_REGSET 16
#else
   #error "COMPILE_TIME_ERROR: Cannot determine 32bit or 64bit mode!"
#endif

/*
Compiler optimization settings.
*/
#if defined GMS_COMPILED_BY_IFORT && (GMS_COMPILED_BY_IFORT) == 1
    #define GMS_IFORT_NO_OPTIMIZATION 0
    #define GMS_IFORT_OPTIMIZATION_O1 1
    #define GMS_IFORT_OPTIMIZATION_O2 2
    #define GMS_IFORT_OPTIMIZATION_O3 3
#elif defined GMS_COMPILED_BY_GFORTRAN && (GMS_COMPILED_BY_GFORTRAN) == 1
    #define GMS_GFORTRAN_NO_OPTIMIZATION 0
    #define GMS_GFORTRAN_OPTIMIZATION_O1 1
    #define GMS_GFORTRAN_OPTIMIZATION_O2 2
    #define GMS_GFORTRAN_OPTIMIZATION_O3 3
#else
    #error Unrecognized Fortran Compiler
#endif


// OpenMP support
    


#if defined (_OPENMP)
    #define CURR_OMP_VER _OPENMP
#endif
    


#if defined (CURR_OMP_VER) || defined (_OPENMP)
    #define DERIVED_TYPE_HAS_ALLOCATABLES 1
#endif

// Performance measurement



#if !defined (USE_PROFILING)
    #define USE_PROFILING 1
#endif

#if defined USE_PROFILING
    #define SAMPLE_PMC 1
#endif

#if defined USE_PROFILING
    #define USE_PREC_CLOCK 1
#endif

#if defined USE_PROFILING
    #define CALL_RDTSCP 0
#endif


#if defined USE_PROFILING
    #define USE_COARSE_CLOCK 0
#endif
    
#if !defined (PROFILE_SUBROUTINE_CALLS)
    #define PROFILE_SUBROUTINE_CALLS 1
#endif
    
 #if !defined (CALL_C_VECTOR_INTRINSICS)
       #define CALL_C_VECTOR_INTRINSICS 0
#endif

! Optimization
    
#if defined __INTEL_COMPILER || defined GMS_COMPILED_BY_IFORT 

   #if !defined (USE_AUTOVECTOR_DIRECTIVE)
       #define USE_AUTOVECTOR_DIRECTIVE 1
   #endif

  

   #if !defined (USE_TRIP_COUNT_HINT)
       #define USE_TRIP_COUNT_HINT 1
   #endif
    
   #if !defined (USE_SOFT_PREFETCH)
       #define USE_SOFT_PREFETCH 0
   #endif
    
   #if !defined (USE_INLINING)
       #define USE_INLINING 1
   #endif
    
   #if !defined (USE_ADD_VECTOR_REDUCTION)
       #define USE_ADD_VECTOR_REDUCTION 1
   #endif

   #if !defined (USE_MUL_VECTOR_REDUCTION)
       #define USE_MUL_VECTOR_REDUCTION 1
   #endif
    
   #if !defined (USE_LOOP_UNROLLING)
      #define USE_LOOP_UNROLLING 1
   #endif
    
   #if defined USE_LOOP_UNROLLING && (USE_LOPP_UNROLLING) == 1
      #define DEFAULT_UNROLL   2
      #define MODERATE_UNROLL  4
      #define AGGRESIVE_UNROLL 8
      #define MEMCPY_UNROLL    16
   #endif
    
   #if !defined (USE_FORCEINLINE)
      #define USE_FORCEINLINE 0
   #endif
    
   #if !defined (LOOP_DISTRIBUTE_POINT)
      #define LOOP_DISTRIBUTE_POINT 0
   #endif
    
   #if !defined (LOOP_PARALLEL_ALWAYS)
      #define LOOP_PARALLEL_ALWAYS 0
   #endif
    
   #if !defined (LOOP_UNROLL_AND_JAM)
      #define LOOP_UNROLL_AND_JAM 0
   #endif
    
   #if !defined (LOOP_AUTOVECTORIZE_ALWAYS)
       #define LOOP_AUTOVECTORIZE_ALWAYS 1
   #endif
    
   #if !defined (USE_IVDEP)
       #define USE_IVDEP 0
   #endif
    
   #if !defined (USE_LOOP_BLOCKING)
      #define USE_LOOP_BLOCKING 1
   #endif
    
   #if !defined (USE_LOOP_STRIP_MINING)
      #define USE_LOOP_STRIP_MINING 1
   #endif

#else

#error Implementation for Gfortran not-exisiting  yet.

#endif

#if defined GMS_COMPILED_BY_IFORT && (GMS_COMPILED_BY_IFORT) == 1
    #define USE_MKL 1
#endif

    
 //! Debugging

#if defined _DEBUG
    #define GMS_DEBUG_ON 1
#endif

#if defined GMS_DEBUG_ON && (GMS_DEBUG_ON) == 1
    #define GMS_DEBUG_VERBOSE 1
#endif

#if defined GMS_DEBUG_VERBOSE && (GMS_DEBUG_VERBOSE) == 1
    #define PRINT_CALLSTACK 1
#endif
    
#if defined GMS_DEBUG_ON && (GMS_DEBUG_ON) == 1
    #define CHECK_FP_CONSISTENCY 1
#endif
    
#if !defined (USE_LOGGING)
    #define USE_LOGGING 1
#endif
    
#if defined GMS_DEBUG_ON && (GMS_DEBUG_ON) == 1
    #define USE_CADNA_DIAGNOSTIC 1
#endif
    
#if defined GMS_DEBUG_ON && (GMS_DEBUG_ON) == 1
    #define IEEE_EXCEPT_FLAGS_NOISY 1
#endif



//! Smith's algorithm for complex division.
#if !defined (USE_SAFE_COMPLEX_DIVISION)
    #define USE_SAFE_COMPLEX_DIVISION 1
#endif

//! Use Intel Intrinsics C-wrapper for vector masked comparison
#if !defined (USE_INTRINSIC_VECTOR_COMPARE)
    #define USE_INTRINSIC_VECTOR_COMPARE 1
#endif


#if !defined (USE_STRUCT_PADDING)
    #define USE_STRUCT_PADDING 1
#endif

#if !defined (BREAK_DATA_INTO_HOT_COLD_SECTIONS)
    #define BREAK_DATA_INTO_HOT_COLD_SECTIONS 1
#endif

#if !defined (AGGREGATE_AUTOMATIC_VARIABLES)
    #define AGGREGATE_AUTOMATIC_VARIABLES 1
#endif
 
//  Globally
#if defined GMS_DEBUG_ON && (GMS_DEBUG_ON) == 1
    #if !defined (USE_IEEE_EXCEPTION_HANDLING)
        #define USE_IEEE_EXCEPTION_HANDLING 1
    #endif
#endif

#if !defined (ADD_PADDING_VECTORIZE_REMAINDER)
    #define ADD_PADDING_VECTORIZE_REMAINDER 1
#endif
    

    

    
#if (__x86_64__) == 1 && defined __AVX512F__
    #define GMS_COUNT_SIMD_REGSISTERS 32
#else
    #define GMS_COUNT_SIMD_REGISTERS 16
#endif

#if (GMS_COMPILED_BY_IFORT) == 1  || (GMS_COMPILED_BY_GFORTRAN) == 1

    #if defined __AVX512F__
        #define GMS_HAS_AVX512F 1
    else
        #define GMS_HAS_AVX512F 0
    #endif
    
    #if defined __AVX512BW__
        #define GMS_HAS_AVX512BW 1
    #else
        #define GMS_HAS_AVX512BW 0
    #endif

    #if defined __AVX512CD__
        #define GMS_HAS_AVX512CD 1
    #else
        #define GMS_HAS_AVX512CD 0
    #endif

    #if defined __AVX512DQ__
        #define GMS_HAS_AVX512DQ 1
    #else
        #define GMS_HAS_AVX512DQ 0
    #endif

    #if defined __AVX512ER__
        #define GMS_HAS_AVX512ER 1
    #else
        #define GMS_HAS_AVX512ER 0
    #endif

    #if defined __AVX512PF__
        #define GMS_HAS_AVX512PF 1
    #else
        #define GMS_HAS_AVX512PF 0
    #endif

    #if defined __AVX512VL__
        #define GMS_HAS_AVX512PF 1
    #else
        #define GMS_HAS_AVX512PF 0
    #endif
#endif
    

#if !defined(USE_STL_MESH)
    #define USE_STL_MESH 1
#endif    

#if !defined(USE_OBJ_MESH)
    #define USE_OBJ_MESH 0
#endif

#if !defined(USE_DEFAULT_MESH)
    #define USE_DEFAULT_MESH 0
#endif
    
#ifndef CACHE_LINE_SIZE
   #define CACHE_LINE_SIZE 64
#endif
    
#ifndef PAD_TO_CACHE_LINE
  #define PAD_TO_CACHE_LINE(var_size) integer(kind=1), dimension(((CACHE_LINE_SIZE)-SIZEOF(var_size))) :: _pad_cache_line_
#endif

#ifndef PAD_TO
  #define PAD_TO(ordinal,size) integer(kind=1), dimension((size)) :: pad##ordinal
#endif

#ifndef MSR_TOOLS_WRAPPERS_SHORT_VERSION
  #define MSR_TOOLS_WRAPPERS_SHORT_VERSION 1
#endif

#if !defined (CURRENT_PROCESSOR_ARCH_NAME)
    #define CURRENT_PROCESSOR_ARCH_NAME 1 // for Haswell family, 2 for Cascade-Lake, 3 for Zen family
#endif

#if defined CURRENT_PROCESSOR_ARCH_NAME && (CURRENT_PROCESSOR_ARCH_NAME) == 1
   #ifndef HASWELL_4_CORE
      define HASWELL_4_CORE 1
   #endif  
#elif defined CURRENT_PROCESSOR_ARCH_NAME && (CURRENT_PROCESSOR_ARCH_NAME) == 2
   #if !defined CASCADE_LAKE_18_CORE
      #define CASCADE_LAKE_18_CORE 0
   #endif
#elif defined CURRENT_PROCESSOR_ARCH_NAME && (CURRENT_PROCESSOR_ARCH_NAME) == 3
    #ifndef ZEN_16_CORE
     #define ZEN_16_CORE 0
   #endif

   #ifndef ZEN_24_CORE
     #define ZEN_24_CORE 0
   #endif

   #ifndef ZEN_32_CORE
     #define ZEN_32_CORE 0
   #endif
#else
   #error Unrecognized Processor family encoding
#endif

#if !defined (ADD_PADDING_FOR_LOOP_PEEL)
    #define ADD_PADDING_FOR_LOOP_PEEL 1
#endif

#if (ADD_PADDING_FOR_LOOP_PEEL) == 1
     #define LOOP_PEEL_PAD 64
#endif


#if !defined (SHOW_CALLSTACK)
    #define SHOW_CALLSTACK 1
#endif

#if !defined (GATHER_PMC_DATA)
    #define GATHER_PMC_DATA 1
#endif

#if defined GATHER_PMC_DATA && (GATHER_PMC_DATA) == 1
    #if !defined (USE_PERF_PROFILER)
        #define USE_PERF_PROFILER 1
    #endif
#endif

#if !defined (USE_PERF_EVENTS_WRAPPER)
    #define USE_PERF_EVENTS_WRAPPER 1
#endif
   




    
    

    
    
    

    


    
