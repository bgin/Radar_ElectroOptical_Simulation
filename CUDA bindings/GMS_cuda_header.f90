
module mod_cuda_header
    !===========================================!
    ! Fortran wrappers to 'cuda.h' header file  !
    !===========================================!
    
    use, intrinsic :: ISO_C_BINDING
    use mod_cuda_types
    
    implicit none
    
    integer(c_int), parameter, public :: CU_IPC_HANDLE_SIZE = 64
    
    ! Start of Interoperable data-types
    !
    ! Enums
    !

    !
    !   Context creation flags
    !   cuda.h -->  typedef enum CUctx_flags_enum
    enum, bind(c) 
        enumerator :: CU_CTX_SCHED_AUTO         = 0
        enumerator :: CU_CTX_SCHED_SPIN         = 1
        enumerator :: CU_CTX_SCHED_YIELD        = 2
        enumerator :: CU_CTX_SCHED_MASK         = 3
        enumerator :: CU_CTX_BLOCKING_SYNC      = 4
        enumerator :: CU_CTX_MAP_HOST           = 8
        enumerator :: CU_CTX_LMEM_RESIZE_TO_MAX = 16
        enumerator :: CU_CTX_FLAGS_MASK         = 31
    end enum  
    
    ! cuda.h -->   typedef enum CUsharedconfig_enum
    enum, bind(c)
         enumerator :: CU_SHARED_MEM_CONFIG_DEFAULT_BANK_SIZE    = 0  !/**< set default shared memory bank size */
         enumerator :: CU_SHARED_MEM_CONFIG_FOUR_BYTE_BANK_SIZE  = 1 !/**< set shared memory bank width to four bytes */
         enumerator :: CU_SHARED_MEM_CONFIG_EIGHT_BYTE_BANK_SIZE = 2  !/**< set shared memory bank width to eight bytes */
    end enum
    
    ! cuda.h --> typedef enum CUevent_flags_enum
    enum, bind(c)
       enumerator :: CU_EVENT_DEFAULT       = 0
       enumerator :: CU_EVENT_BLOCKING_SYNC = 1
    end enum
    
    ! cuda.h --> typedef enum CUarray_format_enum
    enum, bind(c)
       enumerator :: CU_AD_FORMAT_UNSIGNED_INT8   = 1
       enumerator :: CU_AD_FORMAT_UNSIGNED_INT16  = 2
       enumerator :: CU_AD_FORMAT_UNSIGNED_INT32  = 3
       enumerator :: CU_AD_FORMAT_SIGNED_INT8     = 8
       enumerator :: CU_AD_FORMAT_SIGNED_INT16    = 9
       enumerator :: CU_AD_FORMAT_SIGNED_INT32    = 10
       enumerator :: CU_AD_FORMAT_HALF            = 16
       enumerator :: CU_AD_FORMAT_FLOAT           = 32
    end enum
    
    ! cuda.h --> typedef enum CUaddress_mode_enum
    enum, bind(c)
       enumerator :: CU_TR_ADDRESS_MODE_WRAP    = 0
       enumerator :: CU_TR_ADDRESS_MODE_CLAMP   = 1
       enumerator :: CU_TR_ADDRESS_MODE_MIRROR  = 2
    end enum
    
    ! cuda.h --> typedef enum CUfilter_mode_enum
    enum, bind(c)
       enumerator :: CU_TR_FILTER_MODE_POINT    = 0
       enumerator :: CU_TR_FILTER_MODE_LINEAR   = 1
    end enum
    
    ! cuda.h --> typedef enum CUdevice_attribute_enum
    enum, bind(c)
       enumerator :: CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK  = 1
       enumerator :: CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X        = 2
       enumerator :: CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y        = 3
       enumerator :: CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z        = 4
       enumerator :: CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X         = 5
       enumerator :: CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y         = 6
       enumerator :: CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z         = 7
       enumerator :: CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK = 8    !///< Maximum shared memory available per block in bytes
       enumerator :: CU_DEVICE_ATTRIBUTE_SHARED_MEMORY_PER_BLOCK = 8        !///< Deprecated, use CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK
       enumerator :: CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY = 9          !///< Memory available on device for __constant__ variables in a CUDA C kernel in bytes
       enumerator :: CU_DEVICE_ATTRIBUTE_WARP_SIZE = 10                     !///< Warp size in threads
       enumerator :: CU_DEVICE_ATTRIBUTE_MAX_PITCH = 11                     !///< Maximum pitch in bytes allowed by memory copies
       enumerator :: CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK = 12       !///< Maximum number of 32-bit registers available per block
       enumerator :: CU_DEVICE_ATTRIBUTE_REGISTERS_PER_BLOCK = 12           ! ///< Deprecated, use CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK
       enumerator :: CU_DEVICE_ATTRIBUTE_CLOCK_RATE = 13                    ! ///< Peak clock frequency in kilohertz
       enumerator :: CU_DEVICE_ATTRIBUTE_TEXTURE_ALIGNMENT = 14             ! ///< Alignment requirement for textures

       enumerator :: CU_DEVICE_ATTRIBUTE_GPU_OVERLAP = 15                   !///< Device can possibly copy memory and execute a kernel concurrently
       enumerator :: CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT = 16          !///< Number of multiprocessors on device
       enumerator :: CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT = 17           !///< Specifies whether there is a run time limit on kernels
       enumerator :: CU_DEVICE_ATTRIBUTE_INTEGRATED = 18                    !///< Device is integrated with host memory
       enumerator :: CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY = 19           !///< Device can map host memory into CUDA address space
       enumerator :: CU_DEVICE_ATTRIBUTE_COMPUTE_MODE = 20                  !///< Compute mode (See ::CUcomputemode for details)
       enumerator :: CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE1D_WIDTH = 21       ! ///< Maximum 1D texture width
       enumerator :: CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_WIDTH = 22       !///< Maximum 2D texture width
       enumerator :: CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_HEIGHT = 23      !///< Maximum 2D texture height
       enumerator :: CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_WIDTH = 24       !///< Maximum 3D texture width
       enumerator :: CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_HEIGHT = 25      !///< Maximum 3D texture height
       enumerator :: CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE3D_DEPTH = 26       ! ///< Maximum 3D texture depth
       enumerator :: CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_ARRAY_WIDTH = 27  !///< Maximum texture array width
       enumerator :: CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_ARRAY_HEIGHT = 28 !///< Maximum texture array height
       enumerator :: CU_DEVICE_ATTRIBUTE_MAXIMUM_TEXTURE2D_ARRAY_NUMSLICES = 29 ! ///< Maximum slices in a texture array
       enumerator :: CU_DEVICE_ATTRIBUTE_SURFACE_ALIGNMENT = 30                 ! ///< Alignment requirement for surfaces
       enumerator :: CU_DEVICE_ATTRIBUTE_CONCURRENT_KERNELS = 31                ! ///< Device can possibly execute multiple kernels concurrently
       enumerator :: CU_DEVICE_ATTRIBUTE_ECC_ENABLED = 32                       ! ///< Device has ECC support enabled
       enumerator :: CU_DEVICE_ATTRIBUTE_PCI_BUS_ID = 33                        ! ////< PCI bus ID of the device
       enumerator :: CU_DEVICE_ATTRIBUTE_PCI_DEVICE_ID = 34                     !////< PCI device ID of the device
    end enum
    
    ! cuda.h -->   typedef enum CUfunction_attribute_enum
    enum, bind(c)
        ! /**
        !    * The number of threads beyond which a launch of the function would fail.
        !    * This number depends on both the function and the device on which the
        !     * function is currently loaded.
        ! */
       enumerator ::  CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK = 0
        ! /**
        !        * The size in bytes of statically-allocated shared memory required by
        !        * this function. This does not include dynamically-allocated shared
        !        * memory requested by the user at runtime.
        ! */
       enumerator ::  CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES = 1

        !/**
        !     * The size in bytes of user-allocated constant memory required by this
        !     * function.
        ! */
       enumerator :: CU_FUNC_ATTRIBUTE_CONST_SIZE_BYTES = 2

        ! /**
        !        * The size in bytes of thread local memory used by this function.
        !*/
       enumerator :: CU_FUNC_ATTRIBUTE_LOCAL_SIZE_BYTES = 3

        !/**
        !       * The number of registers used by each thread of this function.
        !*/
       enumerator :: CU_FUNC_ATTRIBUTE_NUM_REGS = 4

        !/**
        !    * The PTX virtual architecture version for which the function was compiled.
        !*/
       enumerator :: CU_FUNC_ATTRIBUTE_PTX_VERSION = 5

        !/**
        !    * The binary version for which the function was compiled.
        !*/
       enumerator :: CU_FUNC_ATTRIBUTE_BINARY_VERSION = 6

       enumerator :: CU_FUNC_ATTRIBUTE_MAX
    end enum
    
    ! cuda.h -->   typedef enum CUfunc_cache_enum
    enum, bind(c)
        enumerator :: CU_FUNC_CACHE_PREFER_NONE   = 0
        enumerator :: CU_FUNC_CACHE_PREFER_SHARED = 1
        enumerator :: CU_FUNC_CACHE_PREFER_L1     = 2
        enumerator :: CU_FUNC_CACHE_PREFER_EQUAL  = 3
    end enum
    
    ! cuda.h -->   typedef enum CUmemorytype_enum
    enum, bind(c)
        enumerator :: CU_MEMORYTYPE_HOST   = 1
        enumerator :: CU_MEMORYTYPE_DEVICE = 2
        enumerator :: CU_MEMORYTYPE_ARRAY  = 3
    end enum
    
    ! cuda.h -->  typedef enum CUcomputemode_enum
    enum, bind(c)
        enumerator :: CU_COMPUTEMODE_DEFAULT    = 0
        enumerator :: CU_COMPUTEMODE_EXCLUSIVE  = 1
        enumerator :: CU_COMPUTEMODE_PROHIBITED = 2
    end enum
    
    ! cuda.h -->  typedef enum CUjit_option_enum
    enum, bind(c)
          ! /**
          !       * Max number of registers that a thread may use.\n
          !       * Option type: unsigned int
          ! */
        enumerator :: CU_JIT_MAX_REGISTERS            = 0

          !  /**
          !        * IN: Specifies minimum number of threads per block to target compilation
          !        * for\n
          !        * OUT: Returns the number of threads the compiler actually targeted.
          !        * This restricts the resource utilization fo the compiler (e.g. max
          !        * registers) such that a block with the given number of threads should be
          !        * able to launch based on register limitations. Note, this option does not
          !        * currently take into account any other resource limitations, such as
          !        * shared memory utilization.\n
          !        * Option type: unsigned int
          !   */
        enumerator :: CU_JIT_THREADS_PER_BLOCK

          !  !/**
          !       * Returns a float value in the option of the wall clock time, in
          !       * milliseconds, spent creating the cubin\n
          !       * Option type: float
          !   */
        enumerator :: CU_JIT_WALL_TIME

          !  /**
          !      * Pointer to a buffer in which to print any log messsages from PTXAS
          !      * that are informational in nature (the buffer size is specified via
          !      * option ::CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES) \n
          !      * Option type: char*
          !  */
        enumerator :: CU_JIT_INFO_LOG_BUFFER

          !  /**
          !      * IN: Log buffer size in bytes.  Log messages will be capped at this size
          !      * (including null terminator)\n
          !      * OUT: Amount of log buffer filled with messages\n
          !      * Option type: unsigned int
          !  */
        enumerator :: CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES

          !  /**
          !      * Pointer to a buffer in which to print any log messages from PTXAS that
          !      * reflect errors (the buffer size is specified via option
          !      * ::CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES)\n
          !      * Option type: char*
          !      */
        enumerator :: CU_JIT_ERROR_LOG_BUFFER

          !  /**
          !      * IN: Log buffer size in bytes.  Log messages will be capped at this size
          !      * (including null terminator)\n
          !      * OUT: Amount of log buffer filled with messages\n
          !      * Option type: unsigned int
          !  */
        enumerator :: CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES

          !  /**
          !      * Level of optimizations to apply to generated code (0 - 4), with 4
          !      * being the default and highest level of optimizations.\n
          !      * Option type: unsigned int
          !  */
        enumerator :: CU_JIT_OPTIMIZATION_LEVEL

          !  /**
          !      * No option value required. Determines the target based on the current
          !      * attached context (default)\n
          !      * Option type: No option value needed
          !  */
        enumerator :: CU_JIT_TARGET_FROM_CUCONTEXT

          !  /**
          !      * Target is chosen based on supplied ::CUjit_target_enum.\n
          !      * Option type: unsigned int for enumerated type ::CUjit_target_enum
          !  */
        enumerator :: CU_JIT_TARGET

          !  /**
          !      * Specifies choice of fallback strategy if matching cubin is not found.
          !      * Choice is based on supplied ::CUjit_fallback_enum.\n
          !      * Option type: unsigned int for enumerated type ::CUjit_fallback_enum
          !  */
        enumerator :: CU_JIT_FALLBACK_STRATEGY
    end enum
    
    ! cuda.h --> typedef enum CUjit_target_enum
    enum, bind(c)
        enumerator ::  CU_TARGET_COMPUTE_10 = 10       !/**< Compute device class 1.0 */
        enumerator ::  CU_TARGET_COMPUTE_11 = 11       !/**< Compute device class 1.1 */
        enumerator ::  CU_TARGET_COMPUTE_12 = 12       !/**< Compute device class 1.2 */
        enumerator ::  CU_TARGET_COMPUTE_13 = 13       !/**< Compute device class 1.3 */
        enumerator ::  CU_TARGET_COMPUTE_20 = 20       !/**< Compute device class 2.0 */
        enumerator ::  CU_TARGET_COMPUTE_21 = 21       !/**< Compute device class 2.1 */
        enumerator ::  CU_TARGET_COMPUTE_30 = 30       !/**< Compute device class 3.0 */
        enumerator ::  CU_TARGET_COMPUTE_32 = 32       !/**< Compute device class 3.2 */
        enumerator ::  CU_TARGET_COMPUTE_35 = 35       !/**< Compute device class 3.5 */
        enumerator ::  CU_TARGET_COMPUTE_37 = 37       !/**< Compute device class 3.7 */
        enumerator ::  CU_TARGET_COMPUTE_50 = 50       !/**< Compute device class 5.0 */
        enumerator ::  CU_TARGET_COMPUTE_52 = 52       !/**< Compute device class 5.2 */
        enumerator ::  CU_TARGET_COMPUTE_53 = 53       !/**< Compute device class 5.3 */
        enumerator ::  CU_TARGET_COMPUTE_60 = 60       !/**< Compute device class 6.0. This must be removed for CUDA 7.0 toolkit. See bug 1518217. */
        enumerator ::  CU_TARGET_COMPUTE_61 = 61       !/**< Compute device class 6.1. This must be removed for CUDA 7.0 toolkit.*/
        enumerator ::  CU_TARGET_COMPUTE_62 = 62       ! /**< Compute device class 6.2. This must be removed for CUDA 7.0 toolkit.*/
    end enum
    
    ! cuda.h -->   typedef enum CUjit_fallback_enum
    enum, bind(c)
       enumerator :: CU_PREFER_PTX  = 0
       enumerator :: CU_PREFER_BINARY
    end enum
    
    ! cuda.h -->   typedef enum CUjit_cacheMode_enum
    enum, bind(c)
       enumerator ::  CU_JIT_CACHE_OPTION_NONE = 0  !/**< Compile with no -dlcm flag specified */
       enumerator ::  CU_JIT_CACHE_OPTION_CG        !/**< Compile with L1 cache disabled */
       enumerator ::  CU_JIT_CACHE_OPTION_CA        !/**< Compile with L1 cache enabled */
    end enum
    
    ! cuda.h -->   typedef enum CUjitInputType_enum
    enum, bind(c)
         !/**
         !   * Compiled device-class-specific device code\n
         !   * Applicable options: none
         !*/
       enumerator ::  CU_JIT_INPUT_CUBIN = 0

        !/**
        !    * PTX source code\n
        !    * Applicable options: PTX compiler options
        !*/
       enumerator :: CU_JIT_INPUT_PTX

        !/**
        !    * Bundle of multiple cubins and/or PTX of some device code\n
        !    * Applicable options: PTX compiler options, ::CU_JIT_FALLBACK_STRATEGY
        !*/
       enumerator :: CU_JIT_INPUT_FATBINARY

        !/**
        !    * Host object with embedded device code\n
        !    * Applicable options: PTX compiler options, ::CU_JIT_FALLBACK_STRATEGY
        !*/
       enumerator :: CU_JIT_INPUT_OBJECT

        !/**
        !    * Archive of host objects with embedded device code\n
        !    * Applicable options: PTX compiler options, ::CU_JIT_FALLBACK_STRATEGY
        !*/
       enumerator :: CU_JIT_INPUT_LIBRARY

       enumerator :: CU_JIT_NUM_INPUT_TYPES
    end enum
    
    ! cuda.h -->  typedef enum CUarray_cubemap_face_enum
    enum, bind(c)
       enumerator ::  CU_CUBEMAP_FACE_POSITIVE_X  = 0 !/**< Positive X face of cubemap */
       enumerator ::  CU_CUBEMAP_FACE_NEGATIVE_X  = 1 ! /**< Negative X face of cubemap */
       enumerator ::  CU_CUBEMAP_FACE_POSITIVE_Y  = 2 !/**< Positive Y face of cubemap */
       enumerator ::  CU_CUBEMAP_FACE_NEGATIVE_Y  = 3 !/**< Negative Y face of cubemap */
       enumerator ::  CU_CUBEMAP_FACE_POSITIVE_Z  = 4 !/**< Positive Z face of cubemap */
       enumerator ::  CU_CUBEMAP_FACE_NEGATIVE_Z  = 5  !/**< Negative Z face of cubemap */
    end enum
    
    ! cuda.h -->  typedef enum CUlimit_enum
    enum, bind(c)
       enumerator :: CU_LIMIT_STACK_SIZE                       = 0 !/**< GPU thread stack size */
       enumerator :: CU_LIMIT_PRINTF_FIFO_SIZE                 = 1 !/**< GPU printf FIFO size */
       enumerator :: CU_LIMIT_MALLOC_HEAP_SIZE                 = 2 ! /**< GPU malloc heap size */
       enumerator :: CU_LIMIT_DEV_RUNTIME_SYNC_DEPTH           = 3 !/**< GPU device runtime launch synchronize depth */
       enumerator :: CU_LIMIT_DEV_RUNTIME_PENDING_LAUNCH_COUNT = 4 !/**< GPU device runtime pending launch count */
       enumerator :: CU_LIMIT_MAX 
    end enum
    
    ! cuda.h -->   typedef enum CUresourcetype_enum
    enum, bind(c)
        enumerator :: CU_RESOURCE_TYPE_ARRAY           = 0 !/**< Array resoure */
        enumerator :: CU_RESOURCE_TYPE_MIPMAPPED_ARRAY = 1 ! /**< Mipmapped array resource */
        enumerator :: CU_RESOURCE_TYPE_LINEAR          = 2 !/**< Linear resource */
        enumerator :: CU_RESOURCE_TYPE_PITCH2D         = 3 !/**< Pitch 2D resource */
    end enum
    
    ! cuda.h -->  typedef enum CUpointer_attribute_enum 
    enum, bind(c)
        enumerator ::  CU_POINTER_ATTRIBUTE_CONTEXT        = 1        !/**< The ::CUcontext on which a pointer was allocated or registered */
        enumerator ::  CU_POINTER_ATTRIBUTE_MEMORY_TYPE    = 2    !/**< The ::CUmemorytype describing the physical location of a pointer */
        enumerator ::  CU_POINTER_ATTRIBUTE_DEVICE_POINTER = 3    !/**< The address at which a pointer's memory may be accessed on the device */
        enumerator ::  CU_POINTER_ATTRIBUTE_HOST_POINTER   = 4    !/**< The address at which a pointer's memory may be accessed on the host */
        enumerator ::  CU_POINTER_ATTRIBUTE_P2P_TOKENS     = 5     !/**< A pair of tokens for use with the nv-p2p.h Linux kernel interface */
        enumerator ::  CU_POINTER_ATTRIBUTE_SYNC_MEMOPS    = 6    !/**< Synchronize every synchronous memory operation initiated on this region */
        enumerator ::  CU_POINTER_ATTRIBUTE_BUFFER_ID      = 7    !  /**< A process-wide unique ID for an allocated memory region*/
        enumerator ::  CU_POINTER_ATTRIBUTE_IS_MANAGED     = 8     ! /**< Indicates if the pointer points to managed memory */
    end enum
    
    ! cuda.h -->  typedef enum CUmem_advise_enum
    enum, bind(c)
        enumerator ::  CU_MEM_ADVISE_SET_READ_MOSTLY          = 1    !/**< Data will mostly be read and only occassionally be written to */
        enumerator ::  CU_MEM_ADVISE_UNSET_READ_MOSTLY        = 2    !/**< Undo the effect of ::CU_MEM_ADVISE_SET_READ_MOSTLY */
        enumerator ::  CU_MEM_ADVISE_SET_PREFERRED_LOCATION   = 3    ! /**< Set the preferred location for the data as the specified device */
        enumerator ::  CU_MEM_ADVISE_UNSET_PREFERRED_LOCATION = 4    !/**< Clear the preferred location for the data */
        enumerator ::  CU_MEM_ADVISE_SET_ACCESSED_BY          = 5    ! /**< Data will be accessed by the specified device, so prevent page faults as much as possible */
        enumerator ::  CU_MEM_ADVISE_UNSET_ACCESSED_BY        = 6     !/**< Let the Unified Memory subsystem decide on the page faulting policy for the specified device */
    end enum
    
    ! cuda.h -->    typedef enum cudaError_enum
    enum, bind(c)
           !/**
           !     * The API call returned with no errors. In the case of query calls, this
           !     * can also mean that the operation being queried is complete (see
           !     * ::cuEventQuery() and ::cuStreamQuery()).
           ! */
       enumerator :: CUDA_SUCCESS                              = 0

            !/**
            !     * This indicates that one or more of the parameters passed to the API call
            !    * is not within an acceptable range of values.
            !*/
       enumerator :: CUDA_ERROR_INVALID_VALUE                  = 1

            !/**
            !     * The API call failed because it was unable to allocate enough memory to
            !     * perform the requested operation.
            !*/
       enumerator :: CUDA_ERROR_OUT_OF_MEMORY                  = 2

            !/**
            !    * This indicates that the CUDA driver has not been initialized with
            !    * ::cuInit() or that initialization has failed.
            !*/
       enumerator :: CUDA_ERROR_NOT_INITIALIZED                = 3

            !/**
            !     * This indicates that the CUDA driver is in the process of shutting down.
            !*/
       enumerator :: CUDA_ERROR_DEINITIALIZED                  = 4

            !/**
            !    * This indicates profiler is not initialized for this run. This can
            !    * happen when the application is running with external profiling tools
            !    * like visual profiler.
            !*/
       enumerator :: CUDA_ERROR_PROFILER_DISABLED              = 5

            !/**
           !     * \deprecated
            !    * This error return is deprecated as of CUDA 5.0. It is no longer an error
            !    * to attempt to enable/disable the profiling via ::cuProfilerStart or
            !    * ::cuProfilerStop without initialization.
            ! */
        enumerator :: CUDA_ERROR_PROFILER_NOT_INITIALIZED       = 6

            !/**
            !* \deprecated
            !* This error return is deprecated as of CUDA 5.0. It is no longer an error
            !* to call cuProfilerStart() when profiling is already enabled.
            !*/
        enumerator :: CUDA_ERROR_PROFILER_ALREADY_STARTED       = 7

            !/**
            !    * \deprecated
            !    * This error return is deprecated as of CUDA 5.0. It is no longer an error
            !    * to call cuProfilerStop() when profiling is already disabled.
            !    */
        enumerator :: CUDA_ERROR_PROFILER_ALREADY_STOPPED       = 8

            !/**
            !    * This indicates that no CUDA-capable devices were detected by the installed
            !    * CUDA driver.
            !*/
        enumerator :: CUDA_ERROR_NO_DEVICE                      = 100

            !/**
            !    * This indicates that the device ordinal supplied by the user does not
            !    * correspond to a valid CUDA device.
            !*/
        enumerator :: CUDA_ERROR_INVALID_DEVICE                 = 101


            !/**
            !    * This indicates that the device kernel image is invalid. This can also
            !    * indicate an invalid CUDA module.
            ! */
        enumerator :: CUDA_ERROR_INVALID_IMAGE                  = 200

            !/**
            !    * This most frequently indicates that there is no context bound to the
            !    * current thread. This can also be returned if the context passed to an
            !    * API call is not a valid handle (such as a context that has had
            !    * ::cuCtxDestroy() invoked on it). This can also be returned if a user
            !    * mixes different API versions (i.e. 3010 context with 3020 API calls).
            !    * See ::cuCtxGetApiVersion() for more details.
            !*/
        enumerator :: CUDA_ERROR_INVALID_CONTEXT                = 201

            !/**
            !     * This indicated that the context being supplied as a parameter to the
            !     * API call was already the active context.
            ! * \deprecated
            ! * This error return is deprecated as of CUDA 3.2. It is no longer an
            ! * error to attempt to push the active context via ::cuCtxPushCurrent().
            ! */
        enumerator :: CUDA_ERROR_CONTEXT_ALREADY_CURRENT        = 202

            ! /**
            !    * This indicates that a map or register operation has failed.
            !    */
        enumerator :: CUDA_ERROR_MAP_FAILED                     = 205

            !/**
            !    * This indicates that an unmap or unregister operation has failed.
            ! */
        enumerator :: CUDA_ERROR_UNMAP_FAILED                   = 206

            !/**
            !    * This indicates that the specified array is currently mapped and thus
            !     * cannot be destroyed.
            !*/
        enumerator :: CUDA_ERROR_ARRAY_IS_MAPPED                = 207

             !/**
             !   * This indicates that the resource is already mapped.
             !   */
        enumerator :: CUDA_ERROR_ALREADY_MAPPED                 = 208

            !/**
            !     * This indicates that there is no kernel image available that is suitable
            !     * for the device. This can occur when a user specifies code generation
            !     * options for a particular CUDA source file that do not include the
            !     * corresponding device configuration.
            !*/
        enumerator :: CUDA_ERROR_NO_BINARY_FOR_GPU              = 209

            !/**
            !     * This indicates that a resource has already been acquired.
            ! */
        enumerator :: CUDA_ERROR_ALREADY_ACQUIRED               = 210

            !/**
            !     * This indicates that a resource is not mapped.
            ! */
        enumerator ::  CUDA_ERROR_NOT_MAPPED                     = 211

            !/**
            !    * This indicates that a mapped resource is not available for access as an
            !    ! * array.
            ! */
        enumerator :: CUDA_ERROR_NOT_MAPPED_AS_ARRAY            = 212

            !/**
            !    * This indicates that a mapped resource is not available for access as a
            !    * pointer.
            ! */
        enumerator :: CUDA_ERROR_NOT_MAPPED_AS_POINTER          = 213

            !/**
            !    * This indicates that an uncorrectable ECC error was detected during
            !     * execution.
            ! */
        enumerator :: CUDA_ERROR_ECC_UNCORRECTABLE              = 214

            !/**
                !* This indicates that the ::CUlimit passed to the API call is not
                !* supported by the active device.
            !*/
        enumerator :: CUDA_ERROR_UNSUPPORTED_LIMIT              = 215

            !/**
            !     * This indicates that the ::CUcontext passed to the API call can
            !* only be bound to a single CPU thread at a time but is already 
            !* bound to a CPU thread.
            !*/
        enumerator :: CUDA_ERROR_CONTEXT_ALREADY_IN_USE         = 216

            !/**
            !* This indicates that peer access is not supported across the given
            !* devices.
            !*/
        enumerator :: CUDA_ERROR_PEER_ACCESS_UNSUPPORTED        = 217

            !/**
            !* This indicates that a PTX JIT compilation failed.
            !*/
        enumerator :: CUDA_ERROR_INVALID_PTX                    = 218

            !/**
            !* This indicates an error with OpenGL or DirectX context.
            !*/
        enumerator :: CUDA_ERROR_INVALID_GRAPHICS_CONTEXT       = 219

            !/**
            !* This indicates that an uncorrectable NVLink error was detected during the
            !* execution.
            !*/
        enumerator :: CUDA_ERROR_NVLINK_UNCORRECTABLE           = 220

            !/**
            !* This indicates that the device kernel source is invalid.
            !*/
        enumerator :: CUDA_ERROR_INVALID_SOURCE                 = 300

            !/**
            !* This indicates that the file specified was not found.
            ! */
        enumerator :: CUDA_ERROR_FILE_NOT_FOUND                 = 301

            !/**
            !* This indicates that a link to a shared object failed to resolve.
            !*/
        enumerator :: CUDA_ERROR_SHARED_OBJECT_SYMBOL_NOT_FOUND = 302

            !/**
            !* This indicates that initialization of a shared object failed.
            !*/
        enumerator :: CUDA_ERROR_SHARED_OBJECT_INIT_FAILED      = 303

            !/**
            !* This indicates that an OS call failed.
            !*/
        enumerator :: CUDA_ERROR_OPERATING_SYSTEM               = 304

            !/**
            !* This indicates that a resource handle passed to the API call was not
            !* valid. Resource handles are opaque types like ::CUstream and ::CUevent.
            !*/
        enumerator :: CUDA_ERROR_INVALID_HANDLE                 = 400

            !/**
                !* This indicates that a named symbol was not found. Examples of symbols
                !* are global/constant variable names, texture names, and surface names.
            ! */
        enumerator :: CUDA_ERROR_NOT_FOUND                      = 500

        !/**
        !* This indicates that asynchronous operations issued previously have not
        !* completed yet. This result is not actually an error, but must be indicated
        !* differently than ::CUDA_SUCCESS (which indicates completion). Calls that
        !* may return this value include ::cuEventQuery() and ::cuStreamQuery().
        !*/
        enumerator :: CUDA_ERROR_NOT_READY                      = 600

        !/**
        !* While executing a kernel, the device encountered a
        !* load or store instruction on an invalid memory address.
        !* The context cannot be used, so it must be destroyed (and a new one should be created).
        !* All existing device memory allocations from this context are invalid
        !* and must be reconstructed if the program is to continue using CUDA.
        !*/
        enumerator :: CUDA_ERROR_ILLEGAL_ADDRESS                = 700

        !/**
        !* This indicates that a launch did not occur because it did not have
        !* appropriate resources. This error usually indicates that the user has
        !* attempted to pass too many arguments to the device kernel, or the
        !* kernel launch specifies too many threads for the kernel's register
        !* count. Passing arguments of the wrong size (i.e. a 64-bit pointer
        !* when a 32-bit int is expected) is equivalent to passing too many
        !* arguments and can also result in this error.
        !*/
        enumerator :: CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES        = 701

        !/**
        !* This indicates that the device kernel took too long to execute. This can
        !* only occur if timeouts are enabled - see the device attribute
        !* ::CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT for more information. The
        !* context cannot be used (and must be destroyed similar to
        !* ::CUDA_ERROR_LAUNCH_FAILED). All existing device memory allocations from
        !* this context are invalid and must be reconstructed if the program is to
        !* continue using CUDA.
        !*/
        enumerator :: CUDA_ERROR_LAUNCH_TIMEOUT                 = 702

        !/**
        !* This error indicates a kernel launch that uses an incompatible texturing
        ! * mode.
        ! */
        enumerator :: CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING  = 703
    
        !/**
        !* This error indicates that a call to ::cuCtxEnablePeerAccess() is
        !* trying to re-enable peer access to a context which has already
        !* had peer access to it enabled.
        !*/
        enumerator :: CUDA_ERROR_PEER_ACCESS_ALREADY_ENABLED    = 704

        !/**
        !* This error indicates that ::cuCtxDisablePeerAccess() is 
        !* trying to disable peer access which has not been enabled yet 
        !* via ::cuCtxEnablePeerAccess(). 
        ! */
        enumerator :: CUDA_ERROR_PEER_ACCESS_NOT_ENABLED        = 705

        !/**
        !* This error indicates that the primary context for the specified device
        !* has already been initialized.
        !*/
        enumerator :: CUDA_ERROR_PRIMARY_CONTEXT_ACTIVE         = 708

        !/**
        !* This error indicates that the context current to the calling thread
        !* has been destroyed using ::cuCtxDestroy, or is a primary context which
        !* has not yet been initialized.
        !*/
        enumerator :: CUDA_ERROR_CONTEXT_IS_DESTROYED           = 709

        !/**
        !* A device-side assert triggered during kernel execution. The context
        !* cannot be used anymore, and must be destroyed. All existing device 
        !* memory allocations from this context are invalid and must be 
        !* reconstructed if the program is to continue using CUDA.
        !*/
        enumerator :: CUDA_ERROR_ASSERT                         = 710

        !/**
        !* This error indicates that the hardware resources required to enable
        !* peer access have been exhausted for one or more of the devices 
        !* passed to ::cuCtxEnablePeerAccess().
        !*/
        enumerator :: CUDA_ERROR_TOO_MANY_PEERS                 = 711

        !/**
        !* This error indicates that the memory range passed to ::cuMemHostRegister()
        !* has already been registered.
        !*/
        enumerator :: CUDA_ERROR_HOST_MEMORY_ALREADY_REGISTERED = 712

        !/**
        !* This error indicates that the pointer passed to ::cuMemHostUnregister()
        !* does not correspond to any currently registered memory region.
        !*/
        enumerator :: CUDA_ERROR_HOST_MEMORY_NOT_REGISTERED     = 713

        !/**
        !* While executing a kernel, the device encountered a stack error.
        !* This can be due to stack corruption or exceeding the stack size limit.
        !* The context cannot be used, so it must be destroyed (and a new one should be created).
        !* All existing device memory allocations from this context are invalid
        !* and must be reconstructed if the program is to continue using CUDA.
        !*/
        enumerator :: CUDA_ERROR_HARDWARE_STACK_ERROR           = 714

        !/**
        !* While executing a kernel, the device encountered an illegal instruction.
        ! * The context cannot be used, so it must be destroyed (and a new one should be created).
        !* All existing device memory allocations from this context are invalid
        !* and must be reconstructed if the program is to continue using CUDA.
        !*/
        enumerator :: CUDA_ERROR_ILLEGAL_INSTRUCTION            = 715

        !/**
        !* While executing a kernel, the device encountered a load or store instruction
        !* on a memory address which is not aligned.
        !* The context cannot be used, so it must be destroyed (and a new one should be created).
        !* All existing device memory allocations from this context are invalid
        !* and must be reconstructed if the program is to continue using CUDA.
        !*/
        enumerator :: CUDA_ERROR_MISALIGNED_ADDRESS             = 716

        !/**
        !* While executing a kernel, the device encountered an instruction
        !* which can only operate on memory locations in certain address spaces
        !* (global, shared, or local), but was supplied a memory address not
        !* belonging to an allowed address space.
        !* The context cannot be used, so it must be destroyed (and a new one should be created).
        !* All existing device memory allocations from this context are invalid
        !* and must be reconstructed if the program is to continue using CUDA.
        !*/
        enumerator :: CUDA_ERROR_INVALID_ADDRESS_SPACE          = 717

        !/**
        !* While executing a kernel, the device program counter wrapped its address space.
        !* The context cannot be used, so it must be destroyed (and a new one should be created).
        !* All existing device memory allocations from this context are invalid
        !* and must be reconstructed if the program is to continue using CUDA.
        !*/
        enumerator :: CUDA_ERROR_INVALID_PC                     = 718

        !/**
        !* An exception occurred on the device while executing a kernel. Common
        !* causes include dereferencing an invalid device pointer and accessing
        !* out of bounds shared memory. The context cannot be used, so it must
        !* be destroyed (and a new one should be created). All existing device
        !* memory allocations from this context are invalid and must be
        !* reconstructed if the program is to continue using CUDA.
        !*/
        enumerator :: CUDA_ERROR_LAUNCH_FAILED                  = 719


        !/**
        !* This error indicates that the attempted operation is not permitted.
        !*/
        enumerator :: CUDA_ERROR_NOT_PERMITTED                  = 800

          ! /**
          !      * This error indicates that the attempted operation is not supported
          !      * on the current system or device.
           ! */
        enumerator :: CUDA_ERROR_NOT_SUPPORTED                  = 801

            !/**
            ! * This indicates that an unknown internal error has occurred.
            ! */
        enumerator :: CUDA_ERROR_UNKNOWN                        = 999
       
    end enum
    
    ! cuda.h -->   typedef enum CUdevice_P2PAttribute_enum
    enum, bind(c)
        !     /**< A relative value indicating the performance of the link between two devices */
         enumerator ::  CU_DEVICE_P2P_ATTRIBUTE_PERFORMANCE_RANK        = 1
        !     /**< P2P Access is enable */
         enumerator ::  CU_DEVICE_P2P_ATTRIBUTE_ACCESS_SUPPORTED        = 2
        !     /**< Atomic operation over the link supported */
         enumerator ::  CU_DEVICE_P2P_ATTRIBUTE_NATIVE_ATOMIC_SUPPORTED = 3
    end enum

  !/**
  !  * If set, host memory is portable between CUDA contexts.
  !  * Flag for ::cuMemHostAlloc()
  !*/  
#define  CU_MEMHOSTALLOC_PORTABLE 1

   !/**
    !* If set, host memory is mapped into CUDA address space and
    !* ::cuMemHostGetDevicePointer() may be called on the host pointer.
    !* Flag for ::cuMemHostAlloc()
   !*/
#define   CU_MEMHOSTALLOC_DEVICEMAP 2

    !/**
        !* If set, host memory is allocated as write-combined - fast to write,
        !* faster to DMA, slow to read except via SSE4 streaming load instruction
        !* (MOVNTDQA).
        !* Flag for ::cuMemHostAlloc()
    !*/
#define    CU_MEMHOSTALLOC_WRITECOMBINED 4

    !/**
        !* If set, host memory is portable between CUDA contexts.
        !* Flag for ::cuMemHostRegister()
    !*/
#define    CU_MEMHOSTREGISTER_PORTABLE  1

    !/**
        !* If set, host memory is mapped into CUDA address space and
        !* ::cuMemHostGetDevicePointer() may be called on the host pointer.
        !* Flag for ::cuMemHostRegister()
    !*/ 
#define     CU_MEMHOSTREGISTER_DEVICEMAP 2

     !/**
        !* If set, the passed memory pointer is treated as pointing to some
        !* memory-mapped I/O space, e.g. belonging to a third-party PCIe device.
        !* On Windows the flag is a no-op.
        !* On Linux that memory is marked as non cache-coherent for the GPU and
        !* is expected to be physically contiguous. It may return
        !* CUDA_ERROR_NOT_PERMITTED if run as an unprivileged user,
        !* CUDA_ERROR_NOT_SUPPORTED on older Linux kernel versions.
        !* On all other platforms, it is not supported and CUDA_ERROR_NOT_SUPPORTED
        !* is returned.
        !* Flag for ::cuMemHostRegister()
    !*/
#define      CU_MEMHOSTREGISTER_IOMEMORY 4

 ! Alternative implememtation as a enum
    
    !enum, bind(c)
          !/**
                !  * If set, host memory is portable between CUDA contexts.
                !  * Flag for ::cuMemHostAlloc()
          !*/ 
        ! enumerator ::  CU_MEMHOSTALLOC_PORTABLE  =  1
          !/**
                !* If set, host memory is mapped into CUDA address space and
                !* ::cuMemHostGetDevicePointer() may be called on the host pointer.
                !* Flag for ::cuMemHostAlloc()
          !*/
        ! enumerator ::  CU_MEMHOSTALLOC_DEVICEMAP = 2
         
           !/**
                !* If set, host memory is allocated as write-combined - fast to write,
                !* faster to DMA, slow to read except via SSE4 streaming load instruction
                !* (MOVNTDQA).
                !* Flag for ::cuMemHostAlloc()
           !*/
         ! enumerator ::  CU_MEMHOSTALLOC_WRITECOMBINED = 4 
          
            !/**
                !* If set, host memory is portable between CUDA contexts.
                !* Flag for ::cuMemHostRegister()
            !*/
         !  enumerator ::  CU_MEMHOSTREGISTER_PORTABLE = 1
           
            !/**
                !* If set, host memory is mapped into CUDA address space and
                !* ::cuMemHostGetDevicePointer() may be called on the host pointer.
                !* Flag for ::cuMemHostRegister()
            !*/ 
          ! enumerator ::   CU_MEMHOSTREGISTER_DEVICEMAP = 2
           
             !/**
                !* If set, the passed memory pointer is treated as pointing to some
                !* memory-mapped I/O space, e.g. belonging to a third-party PCIe device.
                !* On Windows the flag is a no-op.
                !* On Linux that memory is marked as non cache-coherent for the GPU and
                !* is expected to be physically contiguous. It may return
                !* CUDA_ERROR_NOT_PERMITTED if run as an unprivileged user,
                !* CUDA_ERROR_NOT_SUPPORTED on older Linux kernel versions.
                !* On all other platforms, it is not supported and CUDA_ERROR_NOT_SUPPORTED
                !* is returned.
                !* Flag for ::cuMemHostRegister()
            !*/
          ! enumerator ::   CU_MEMHOSTREGISTER_IOMEMORY = 4
           
   ! end enum
    
    ! cuda.h --> typedef enum CUresourceViewFormat_enum
     
    enum, bind(c)
       
        enumerator ::  CU_RES_VIEW_FORMAT_NONE          = 0 !/**< No resource view format (use underlying resource format) */
        enumerator ::  CU_RES_VIEW_FORMAT_UINT_1X8      = 1 !/**< 1 channel unsigned 8-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_UINT_2X8      = 2 !/**< 2 channel unsigned 8-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_UINT_4X8      = 3 !/**< 4 channel unsigned 8-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_SINT_1X8      = 4 !/**< 1 channel signed 8-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_SINT_2X8      = 5 !/**< 2 channel signed 8-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_SINT_4X8      = 6 !/**< 4 channel signed 8-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_UINT_1X16     = 7 !/**< 1 channel unsigned 16-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_UINT_2X16     = 8 !/**< 2 channel unsigned 16-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_UINT_4X16     = 9 !/**< 4 channel unsigned 16-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_SINT_1X16     = 10 !/**< 1 channel signed 16-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_SINT_2X16     = 11 !/**< 2 channel signed 16-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_SINT_4X16     = 12 !/**< 4 channel signed 16-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_UINT_1X32     = 13 !/**< 1 channel unsigned 32-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_UINT_2X32     = 14 !/**< 2 channel unsigned 32-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_UINT_4X32     = 15 !/**< 4 channel unsigned 32-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_SINT_1X32     = 16 !/**< 1 channel signed 32-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_SINT_2X32     = 17 !/**< 2 channel signed 32-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_SINT_4X32     = 18 !/**< 4 channel signed 32-bit integers */
        enumerator ::  CU_RES_VIEW_FORMAT_FLOAT_1X16    = 19 !/**< 1 channel 16-bit floating point */
        enumerator ::  CU_RES_VIEW_FORMAT_FLOAT_2X16    = 20 !/**< 2 channel 16-bit floating point */
        enumerator ::  CU_RES_VIEW_FORMAT_FLOAT_4X16    = 21 !/**< 4 channel 16-bit floating point */
        enumerator ::  CU_RES_VIEW_FORMAT_FLOAT_1X32    = 22 !/**< 1 channel 32-bit floating point */
        enumerator ::  CU_RES_VIEW_FORMAT_FLOAT_2X32    = 23 !/**< 2 channel 32-bit floating point */
        enumerator ::  CU_RES_VIEW_FORMAT_FLOAT_4X32    = 24 ! /**< 4 channel 32-bit floating point */
        enumerator ::  CU_RES_VIEW_FORMAT_UNSIGNED_BC1  = 25 !/**< Block compressed 1 */
        enumerator ::  CU_RES_VIEW_FORMAT_UNSIGNED_BC2  = 26 ! /**< Block compressed 2 */
        enumerator ::  CU_RES_VIEW_FORMAT_UNSIGNED_BC3  = 27 ! /**< Block compressed 3 */
        enumerator ::  CU_RES_VIEW_FORMAT_UNSIGNED_BC4  = 28 ! /**< Block compressed 4 unsigned */
        enumerator ::  CU_RES_VIEW_FORMAT_SIGNED_BC4    = 29 !/**< Block compressed 4 signed */
        enumerator ::  CU_RES_VIEW_FORMAT_UNSIGNED_BC5  = 30 ! /**< Block compressed 5 unsigned */
        enumerator ::  CU_RES_VIEW_FORMAT_SIGNED_BC5    = 31 ! /**< Block compressed 5 signed */
        enumerator ::  CU_RES_VIEW_FORMAT_UNSIGNED_BC6H = 32 !/**< Block compressed 6 unsigned half-float */
        enumerator ::  CU_RES_VIEW_FORMAT_SIGNED_BC6H   = 33 !/**< Block compressed 6 signed half-float */
        enumerator ::  CU_RES_VIEW_FORMAT_UNSIGNED_BC7  = 34 ! /**< Block compressed 7 */
        
    end enum
    
     !/**
        !* 2D memory copy parameters
     !*/
    
     type, bind(c), public :: CUDA_MEMCPY2D_st
           
           integer(c_long_long)              :: srcXInBytes     !/**< Source X in bytes */
           integer(c_long_long)              :: srcY            !/**< Source Y */
           integer(kind(CU_MEMORYTYPE_HOST)) :: srcMemoryType   !/**< Source memory type (host, device, array) */
           type(c_ptr)                       :: srcHost         !/**< Source host pointer */
           integer(c_long_long)              :: srcDevice       !/**< Source device pointer */
           type(CUArray_st)                  :: srcArray        !/**< Source array reference */
           integer(c_long_long)              :: srcPitch        !/**< Source pitch (ignored when src is array) */
           integer(c_long_long)              :: dstXInBytes     ! /**< Destination X in bytes */
           integer(c_long_long)              :: dstY            ! /**< Destination Y */
           integer(kind(CU_MEMORYTYPE_HOST)) :: dstMemoryType   !/**< Destination memory type (host, device, array) */
           type(c_ptr)                       :: dstHost         !/**< Destination host pointer */
           integer(c_long_long)              :: dstDevice       ! /**< Destination device pointer */
           type(CUArray_st)                  :: dstArray        !  /**< Destination array reference */
           integer(c_long_long)              :: dstPitch        !/**< Destination pitch (ignored when dst is array) */
           integer(c_long_long)              :: WidthInBytes    !/**< Width of 2D memory copy in bytes */
           integer(c_long_long)              :: Height          ! /**< Height of 2D memory copy */
           
     end type CUDA_MEMCPY2D_st
     
     !/**
        !* 3D memory copy parameters
     !*/

     type, bind(c), public :: CUDA_MEMCPY3D_st
         
           integer(c_long_long)              :: srcXInBytes     !/**< Source X in bytes */
           integer(c_long_long)              :: srcY            ! /**< Source Y */
           integer(c_long_long)              :: srcZ            !/**< Source Z */
           integer(c_long_long)              :: srcLOD          ! /**< Source LOD */ 
           integer(kind(CU_MEMORYTYPE_HOST)) :: srcMemoryType   !/**< Source memory type (host, device, array) */
           type(c_ptr)                       :: srcHost         !/**< Source host pointer */
           integer(c_long_long)              :: srcDevice       ! /**< Source device pointer */
           type(CUArray_st)                  :: srcArray        !    /**< Source array reference */
           type(c_ptr)                       :: reserved0       !/**< Must be NULL */
           integer(c_long_long)              :: srcPitch        !/**< Source pitch (ignored when src is array) */
           integer(c_long_long)              :: srcHeight       !/**< Source height (ignored when src is array; may be 0 if Depth==1) */
           integer(c_long_long)              :: dstXInBytes     !/**< Destination X in bytes */
           integer(c_long_long)              :: dstY            ! /**< Destination Y */
           integer(c_long_long)              :: dstZ            ! /**< Destination Z */
           integer(c_long_long)              :: dstLOD          ! /**< Destination LOD */
           integer(kind(CU_MEMORYTYPE_HOST)) :: dstMemoryType   ! /**< Destination memory type (host, device, array) */
           type(c_ptr)                       :: dstHost         ! /**< Destination host pointer */
           integer(c_long_long)              :: dstDevice       ! /**< Destination device pointer */
           type(CUArray_st)                  :: dstArray        !  /**< Destination array reference */
           type(c_ptr)                       :: reserved1       !  /**< Must be NULL */
           integer(c_long_long)              :: dstPitch        ! /**< Destination pitch (ignored when dst is array) */
           integer(c_long_long)              :: dstHeight       ! /**< Destination height (ignored when dst is array; may be 0 if Depth==1) */
           integer(c_long_long)              :: WidthInBytes    ! /**< Width of 3D memory copy in bytes */
           integer(c_long_long)              :: Height          ! /**< Height of 3D memory copy */
           integer(c_long_long)              :: Depth           !    /**< Depth of 3D memory copy */
           
     end type CUDA_MEMCPY3D_st
     
     type, bind(c), public :: CUuuid_st
           integer(c_char), dimension(0:15) :: bytes            !/**< CUDA definition of UUID */
     end type CUuuid_st
     
     !  typedef struct CUipcEventHandle_st
     type, bind(c), public :: CUipcEventHandle_st
           integer(c_char), dimension(0:CU_IPC_HANDLE_SIZE-1) :: reserved
     end type CUipcEventHandle_st
     
     !   typedef struct CUipcMemHandle_st
     type, bind(c), public :: CUipcMemHandle_st
           integer(c_char), dimension(0:CU_IPC_HANDLE_SIZE-1) :: reserved
     end type CUipcMemHandle_st
     
      !/**
            !* Legacy device properties
      !*/
      type, bind(c), public :: CUdevprop_st
          
            integer(c_int)                 :: maxThreadsPerBlock   !/**< Maximum number of threads per block */
            integer(c_int), dimension(0:2) :: maxThreadsDim        !/**< Maximum size of each dimension of a block */
            integer(c_int), dimension(0:2) :: maxGridSize          ! /**< Maximum size of each dimension of a grid */
            integer(c_int)                 :: sharedMemPerBlock    ! /**< Shared memory available per block in bytes */
            integer(c_int)                 :: totalConstantMemory  !  /**< Constant memory available on device in bytes */
            integer(c_int)                 :: SIMDWidth            !  /**< Warp size in threads */
            integer(c_int)                 :: memPitch             ! /**< Maximum pitch in bytes allowed by memory copies */
            integer(c_int)                 :: regsPerBlock         ! /**< 32-bit registers available per block */
            integer(c_int)                 :: clockRate            !   /**< Clock frequency in kilohertz */
            integer(c_int)                 :: textureAlign         !  /**< Alignment requirement for textures */
            
      end type CUdevprop_st
      
      !/**
                !* 3D memory cross-context copy parameters
      !*/
      type, bind(c), public :: CUDA_MEMCPY3D_PEER_st
          
             integer(c_long_long)               :: srcXInBytes   !    /**< Source X in bytes */
             integer(c_long_long)               :: srcY          !    /**< Source Y */
             integer(c_long_long)               :: srcZ          !    /**< Source Z */
             integer(c_long_long)               :: srcLOD        !     /**< Source LOD */
             integer(kind(CU_MEMORYTYPE_HOST))  :: srcMemoryType !     /**< Source memory type (host, device, array) */
             type(c_ptr)                        :: srcHost       !      /**< Source host pointer */
             integer(c_long_long)               :: srcDevice     !     /**< Source device pointer */
             type(CUArray_st)                   :: srcArray      !      /**< Source array reference */
             type(CUctx_st)                     :: srcContext    !    /**< Source context (ignored with srcMemoryType is ::CU_MEMORYTYPE_ARRAY) */
             integer(c_long_long)               :: srcPitch      !    /**< Source pitch (ignored when src is array) */
             integer(c_long_long)               :: srcHeight     !    /**< Source height (ignored when src is array; may be 0 if Depth==1) */
             integer(c_long_long)               :: dstXInBytes     !/**< Destination X in bytes */
             integer(c_long_long)               :: dstY            ! /**< Destination Y */
             integer(c_long_long)               :: dstZ            ! /**< Destination Z */
             integer(c_long_long)               :: dstLOD          ! /**< Destination LOD */
             integer(kind(CU_MEMORYTYPE_HOST))  :: dstMemoryType   ! /**< Destination memory type (host, device, array) */
             type(c_ptr)                        :: dstHost         ! /**< Destination host pointer */
             integer(c_long_long)               :: dstDevice       ! /**< Destination device pointer */
             type(CUArray_st)                   :: dstArray        !  /**< Destination array reference */
             type(CUctx_st)                     :: dstContext      !  /**< Destination context (ignored with dstMemoryType is ::CU_MEMORYTYPE_ARRAY) */
             integer(c_long_long)               :: dstPitch        !   /**< Destination pitch (ignored when dst is array) */
             integer(c_long_long)               :: dstHeight
             integer(c_long_long)               :: WidthInBytes    !  /**< Width of 3D memory copy in bytes */
             integer(c_long_long)               :: Heigth          !   /**< Height of 3D memory copy */
             integer(c_long_long)               :: Depth           !      /**< Depth of 3D memory copy */
             
      end type CUDA_MEMCPY3D_PEER_st
      
      
      !/**
            !* Array descriptor
      !*/
      type, bind(c), public :: CUDA_ARRAY_DESCRIPTOR_st
            
            integer(c_long_long)                      :: Width          ! /**< Width of array */
            integer(c_long_long)                      :: Height         ! /**< Height of array */
            integer(kind(CU_AD_FORMAT_UNSIGNED_INT8)) :: arrayFormat    ! /**< Array format */
            integer(c_int)                            :: NumChannels
            
      end type CUDA_ARRAY_DESCRIPTOR_st
      
      !/**
            !* 3D array descriptor
      !*/
      type, bind(c), public :: CUDA_ARRAY3D_DESCRIPTOR_st
          
            integer(c_long_long)                      :: Width          ! /**< Width of array */
            integer(c_long_long)                      :: Height         ! /**< Height of array */
            integer(c_long_long)                      :: Depth
            integer(kind(CU_AD_FORMAT_UNSIGNED_INT8)) :: arrayFormat    ! /**< Array format */
            integer(c_int)                            :: NumChannels    !/**< Channels per array element */
            integer(c_int)                            :: Flags          ! /**< Flags */ 
            
      end type CUDA_ARRAY3D_DESCRIPTOR_st
      
      !/**
           ! * CUDA Resource descriptor
      !  */
      ! Will it be compatible with this C ADT
!      typedef struct CUDA_RESOURCE_DESC_st
!{
!    CUresourcetype resType;                   /**< Resource type */

!    union {
!        struct {
!            CUarray hArray;                   /**< CUDA array */
!        } array;
!        struct {
!            CUmipmappedArray hMipmappedArray; /**< CUDA mipmapped array */
!        } mipmap;
!        struct {
!            CUdeviceptr devPtr;               /**< Device pointer */
!            CUarray_format format;            /**< Array format */
!            unsigned int numChannels;         /**< Channels per array element */
!            size_t sizeInBytes;               /**< Size in bytes */
!        } linear;
!        struct {
!            CUdeviceptr devPtr;               /**< Device pointer */
!            CUarray_format format;            /**< Array format */
!            unsigned int numChannels;         /**< Channels per array element */
!            size_t width;                     /**< Width of the array in elements */
!            size_t height;                    /**< Height of the array in elements */
!            size_t pitchInBytes;              /**< Pitch between two rows in bytes */
!        } pitch2D;
!        struct {
!            int reserved[32];
!        } reserved;
 !   } res;
 !
!    unsigned int flags;                       /**< Flags (must be zero) */
!} CUDA_RESOURCE_DESC;
      
   ! To be implemented with STRUCTURE ... UNION ... MAP
      
     !/**
        !* GPU Direct v3 tokens
     !*/ 
      type, bind(c), public :: CUDA_POINTER_ATTRIBUTE_P2P_TOKENS_st
            integer(c_long_long) :: p2pToken
            integer(c_int)       :: vaSpaceToken
      end type CUDA_POINTER_ATTRIBUTE_P2P_TOKENS_st
      
      ! Two version
      !/**
        !* If set, the CUDA array is a collection of layers, where each layer is either a 1D
        !* or a 2D array and the Depth member of CUDA_ARRAY3D_DESCRIPTOR specifies the number 
        !* of layers, not the depth of a 3D array.
      !*/
#define CUDA_ARRAY3D_LAYERED  1
       
      !/**
            !* Deprecated, use CUDA_ARRAY3D_LAYERED
      !*/
#define CUDA_ARRAY3D_2DARRAY  1
      
      !/**
            !* This flag must be set in order to bind a surface reference
            !* to the CUDA array
        !*/
#define CUDA_ARRAY3D_SURFACE_LDST 2

      !/**
            !* If set, the CUDA array is a collection of six 2D arrays, representing faces of a cube. The
            !* width of such a CUDA array must be equal to its height, and Depth must be six.
            !* If ::CUDA_ARRAY3D_LAYERED flag is also set, then the CUDA array is a collection of cubemaps
            !* and Depth must be a multiple of six.
      !*/ 
#define CUDA_ARRAY3D_CUBEMAP 4

      !/**
            !* This flag must be set in order to perform texture gather operations
            !* on a CUDA array.
      !*/ 
#define CUDA_ARRAY3D_TEXTURE_GATHER 8

       !/**
            !* This flag if set indicates that the CUDA
            !* array is a DEPTH_TEXTURE.
        !*/
#define CUDA_ARRAY3D_DEPTH_TEXTURE 10

      !/**
      !      * \brief Initialize the CUDA driver API
      !      *
      !      * Initializes the driver API and must be called before any other function from
      !      * the driver API. Currently, the \p Flags parameter must be 0. If ::cuInit()
      !      * has not been called, any function from the driver API will return
      !      * ::CUDA_ERROR_NOT_INITIALIZED.
      !      *
      !      * \param Flags - Initialization flag for CUDA.
      !      *
      !      * \return
      !      * ::CUDA_SUCCESS,
      !      * ::CUDA_ERROR_INVALID_VALUE,
      !      * ::CUDA_ERROR_INVALID_DEVICE
      !      * \notefnerr
      !  */ 
    interface
        function cuInit(Flags) result(status)  &
                     bind(c,name='cuInit')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUDA_SUCCESS
                 integer(c_int), intent(in), value :: Flags
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cuInit
    end interface 
    
     !/**
        !* \brief Returns the CUDA driver version
        !*
        !* Returns in \p *driverVersion the version number of the installed CUDA
        !* driver. This function automatically returns ::CUDA_ERROR_INVALID_VALUE if
       ! * the \p driverVersion argument is NULL.
        !*
        !* \param driverVersion - Returns the CUDA driver version
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
     !*/
    interface
        function cuDriverGetVersion(driverVersion) result(status) &
                            bind(c,name='cuDriverGetVersion')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_int), intent(inout) :: driverVersion
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuDriverGetVersion
    end interface
    
    !/**
        !* \brief Returns a handle to a compute device
        !*
        !* Returns in \p *device a device handle given an ordinal in the range <b>[0,
        !* ::cuDeviceGetCount()-1]</b>.
        !*
        !* \param device  - Returned device handle
        !* \param ordinal - Device number to get handle for
       ! *
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
       ! * ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
       ! * \notefnerr
       ! *
       ! * \sa
       ! * ::cuDeviceGetAttribute,
       ! * ::cuDeviceGetCount,
       ! * ::cuDeviceGetName,
       ! * ::cuDeviceTotalMem
       ! */
    
    interface
        function cuDeviceGet(device,ordinal) result(status) &
                        bind(c,name='cuDeviceGet')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_int), intent(inout)     :: device
                    integer(c_int), intent(in), value :: ordinal
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuDeviceGet
    end interface
    
    !/**
        !* \brief Returns the number of compute-capable devices
        !*
        !* Returns in \p *count the number of devices with compute capability greater
        !* than or equal to 1.0 that are available for execution. If there is no such
        !* device, ::cuDeviceGetCount() returns 0.
        !*
        !* \param count - Returned number of compute-capable devices
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa
        !* ::cuDeviceGetAttribute,
        !* ::cuDeviceGetName,
        !* ::cuDeviceGet,
        !* ::cuDeviceTotalMem
    !*/
    
    interface
        function cuDeviceGetCount(count) result(status) &
                           bind(c,name='cuDeviceGetCount')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_int), intent(inout) :: count
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuDeviceGetCount
    end interface
    
     !/**
        !* \brief Returns an identifer string for the device
        !*
        !* Returns an ASCII string identifying the device \p dev in the NULL-terminated
        !* string pointed to by \p name. \p len specifies the maximum length of the
        !* string that may be returned.
        !*
        !* \param name - Returned identifier string for the device
        !* \param len  - Maximum length of string to store in \p name
        !* \param dev  - Device to get identifier string for
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
        !* \notefnerr
        !*
        !* \sa
        !* ::cuDeviceGetAttribute,
        !* ::cuDeviceGetCount,
        !* ::cuDeviceGet,
        !* ::cuDeviceTotalMem
    !*/
    
    interface
        function cuDeviceGetName(name,length,dev) result(status)  &
                            bind(c,name='cuDeviceGetName')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    character(len=c_char), dimension(*), intent(inout)     :: name
                    integer(c_int),                      intent(in), value :: length
                    integer(c_int),                      intent(in), value :: dev
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuDeviceGetName
    end interface
    
    !/**
        !* \brief Returns the total amount of memory on the device
        !*
        !* Returns in \p *bytes the total amount of memory available on the device
        !* \p dev in bytes.
        !*
        !* \param bytes - Returned memory available on device in bytes
        !* \param dev   - Device handle
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
       ! * ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
        !* \notefnerr
        !*
        !* \sa
        !* ::cuDeviceGetAttribute,
        !* ::cuDeviceGetCount,
        !* ::cuDeviceGetName,
        !* ::cuDeviceGet,
    !*/
    
    interface
        function cuDeviceTotalMem(bytes,dev) result(status)  &
                                 bind(c,name='cuDeviceTotalMem')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_long_long),       intent(inout)     :: bytes
                    integer(c_int),             intent(in), value :: dev
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuDeviceTotalMem
    end interface
    
    !/**
        !* \brief Returns information about the device
        !*
        !* Returns in \p *pi the integer value of the attribute \p attrib on device
        !* \p dev. The supported attributes are:
       
        !*   memory at the same virtual address as the CPU.
        !*
        !* \param pi     - Returned device attribute value
        !* \param attrib - Device attribute to query
        !* \param dev    - Device handle
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
        !* \notefnerr
        !*
        !* \sa
        !* ::cuDeviceGetCount,
        !* ::cuDeviceGetName,
        ! * ::cuDeviceGet,
        ! * ::cuDeviceTotalMem
     !*/
    
    interface
        function cuDeviceGetAttribute(pi,attrib,dev)  result(status)  &
                                bind(c,name='cuDeviceGetAttribute')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_int),     intent(inout)     :: pi
                    integer(c_int),     intent(in), value :: attrib
                    integer(c_int),     intent(in), value :: dev
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuDeviceGetAttribute
    end interface
    
    !/**
        !* \brief Returns properties for a selected device
        !*
        !* \deprecated
        !*
        !* This function was deprecated as of CUDA 5.0 and replaced by ::cuDeviceGetAttribute().
        !*
        !* Returns in \p *prop the properties of device \p dev. The ::CUdevprop
        !* structure is defined as:
        !*
        !* \code
            ! typedef struct CUdevprop_st {
            !int maxThreadsPerBlock;
            !int maxThreadsDim[3];
            !int maxGridSize[3];
            !int sharedMemPerBlock;
            !int totalConstantMemory;
            !int SIMDWidth;
            !int memPitch;
            !int regsPerBlock;
            !int clockRate;
            !int textureAlign
        !} CUdevprop;
        !* \endcode
        !* where:
        !*
        !* - ::maxThreadsPerBlock is the maximum number of threads per block;
        !* - ::maxThreadsDim[3] is the maximum sizes of each dimension of a block;
        !* - ::maxGridSize[3] is the maximum sizes of each dimension of a grid;
        !* - ::sharedMemPerBlock is the total amount of shared memory available per
        !*   block in bytes;
        !* - ::totalConstantMemory is the total amount of constant memory available on
        !*   the device in bytes;
        !* - ::SIMDWidth is the warp size;
        !* - ::memPitch is the maximum pitch allowed by the memory copy functions that
        !*   involve memory regions allocated through ::cuMemAllocPitch();
        !* - ::regsPerBlock is the total number of registers available per block;
        !* - ::clockRate is the clock frequency in kilohertz;
        !* - ::textureAlign is the alignment requirement; texture base addresses that
        !*   are aligned to ::textureAlign bytes do not need an offset applied to
        !*   texture fetches.
        !*
        !* \param prop - Returned properties of device
        !* \param dev  - Device to get properties for
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
        !* \notefnerr
        !*
        !* \sa
        !* ::cuDeviceGetAttribute,
        !* ::cuDeviceGetCount,
        !* ::cuDeviceGetName,
        !* ::cuDeviceGet,
        !* ::cuDeviceTotalMem
    !*/
    
    interface
        function cuDeviceGetProperties(prop,dev)  result(status)  &
                                bind(c,name='cuDeviceGetProperties')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUdevprop_st
                    import :: CUDA_SUCCESS
                    type(CUdevprop_st),     intent(inout)     :: prop
                    integer(c_int),         intent(in), value :: dev
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuDeviceGetProperties
    end interface
    
    !/**
        !* \brief Returns the compute capability of the device
        !*
        !* \deprecated
        !*
        !* This function was deprecated as of CUDA 5.0 and its functionality superceded
        !* by ::cuDeviceGetAttribute(). 
        !*
        !* Returns in \p *major and \p *minor the major and minor revision numbers that
        !* define the compute capability of the device \p dev.
        !*
        !* \param major - Major revision number
        !* \param minor - Minor revision number
        !* \param dev   - Device handle
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
        !* \notefnerr
        !*
        !* \sa
        !* ::cuDeviceGetAttribute,
        !* ::cuDeviceGetCount,
        !* ::cuDeviceGetName,
        !* ::cuDeviceGet,
        !* ::cuDeviceTotalMem
    !*/
    
    interface
        function cuDeviceComputeCapability(major,minor,dev)  result(status)  &
                                    bind(c,name='cuDevComputeCapability')
                     use, intrinsic :: ISO_C_BINDING
                     import :: CUDA_SUCCESS
                     integer(c_int),    intent(inout)     :: major
                     integer(c_int),    intent(inout)     :: minor
                     integer(c_int),    intent(in), value :: dev
                     integer(kind(CUDA_SUCCESS)) :: status
        end function cuDeviceComputeCapability
    end interface
    
    !/**
        !* \brief Retain the primary context on the GPU
        !*
        !* Retains the primary context on the device, creating it if necessary,
        !* increasing its usage count. The caller must call
        !* ::cuDevicePrimaryCtxRelease() when done using the context.
        !* Unlike ::cuCtxCreate() the newly created context is not pushed onto the stack.
        !*
        !* Context creation will fail with ::CUDA_ERROR_UNKNOWN if the compute mode of
        !* the device is ::CU_COMPUTEMODE_PROHIBITED.  The function ::cuDeviceGetAttribute() 
        !* can be used with ::CU_DEVICE_ATTRIBUTE_COMPUTE_MODE to determine the compute mode 
        !* of the device. 
        !* The <i>nvidia-smi</i> tool can be used to set the compute mode for
        !* devices. Documentation for <i>nvidia-smi</i> can be obtained by passing a
        !* -h option to it.
        !*
        !* Please note that the primary context always supports pinned allocations. Other
        !* flags can be specified by ::cuDevicePrimaryCtxSetFlags().
        !*
        !* \param pctx  - Returned context handle of the new context
        !* \param dev   - Device for which primary context is requested
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_DEVICE,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY,
        !* ::CUDA_ERROR_UNKNOWN
        !* \notefnerr
        !*
        !* \sa ::cuDevicePrimaryCtxRelease,
        !* ::cuDevicePrimaryCtxSetFlags,
        !* ::cuCtxCreate,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetCacheConfig,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize
    !*/
    
    interface
        function cuDevicePrimaryCtxRetain(pctx,dev)  result(status)  &
                                bind(c,name='cuDevicePrimaryCtxRetain')
                     use, intrinsic :: ISO_C_BINDING
                     import :: CUctx_st
                     import :: CUDA_SUCCESS
                     type(CUctx_st),    intent(inout)     :: pctx
                     integer(c_int),    intent(in), value :: dev
                     integer(kind(CUDA_SUCCESS)) :: status
        end function cuDevicePrimaryCtxRetain
    end interface
    
     !/**
        !* \brief Release the primary context on the GPU
        !*
        !* Releases the primary context interop on the device by decreasing the usage
        !* count by 1. If the usage drops to 0 the primary context of device \p dev
        !* will be destroyed regardless of how many threads it is current to.
        !*
        !* Please note that unlike ::cuCtxDestroy() this method does not pop the context
        !* from stack in any circumstances.
        !*
        !* \param dev - Device which primary context is released
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_DEVICE
        !* \notefnerr
        !*
        !* \sa ::cuDevicePrimaryCtxRetain,
        !* ::cuCtxDestroy,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetCacheConfig,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize
    !*/
    
    interface
        function cuDevicePrimaryCtxRelease(dev)  result(status)   &
                                bind(c,name='cuDevicePrimaryCtxRelease')
                        use, intrinsic :: ISO_C_BINDING
                        import :: CUDA_SUCCESS
                        integer(c_int),     intent(in), value :: dev
                        integer(kind(CUDA_SUCCESS)) :: status
        end function cuDevicePrimaryCtxRelease
    end interface
    
    ! * \param dev   - Device for which the primary context flags are set
    ! * \param flags - New flags for the device
    !*
    !* \return
    !* ::CUDA_SUCCESS,
    !* ::CUDA_ERROR_DEINITIALIZED,
    !* ::CUDA_ERROR_NOT_INITIALIZED,
    !* ::CUDA_ERROR_INVALID_DEVICE,
    !* ::CUDA_ERROR_INVALID_VALUE,
    !* ::CUDA_ERROR_PRIMARY_CONTEXT_ACTIVE
    !* \notefnerr
    !*
    !* \sa ::cuDevicePrimaryCtxRetain,
    !* ::cuDevicePrimaryCtxGetState,
    !* ::cuCtxCreate,
    !* ::cuCtxGetFlags
    !*/
    
    interface
        function cuDevicePrimaryCtxSetFlags(dev,flags)  result(status)  &
                                  bind(c,name='cuDevicePrimaryCtxSetFlags')
                        use, intrinsic :: ISO_C_BINDING
                        import :: CUDA_SUCCESS
                        integer(c_int),     intent(in), value :: dev
                        integer(c_int),     intent(in), value :: flags
                        integer(kind(CUDA_SUCCESS)) :: status
        end function cuDevicePrimaryCtxSetFlags
    end interface
    
    !/**
        !* \brief Get the state of the primary context
        !*
        !* Returns in \p *flags the flags for the primary context of \p dev, and in
        !* \p *active whether it is active.  See ::cuDevicePrimaryCtxSetFlags for flag
        !* values.
        !*
        !* \param dev    - Device to get primary context flags for
        !* \param flags  - Pointer to store flags
        !* \param active - Pointer to store context state; 0 = inactive, 1 = active
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
       ! * ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_DEVICE,
       ! * ::CUDA_ERROR_INVALID_VALUE,
       ! * \notefnerr
        !*
       !! * \sa ::cuDevicePrimaryCtxSetFlags,
       ! * ::cuCtxGetFlags
    !*/
    
    interface
        function cuDeviceSetPrimaryCtxGetState(dev,flags,active)  result(status)  &
                                    bind(c,name='cuDeviceSetPrimaryCtxGetState')
                        use, intrinsic :: ISO_C_BINDING
                        import :: CUDA_SUCCESS
                        integer(c_int),     intent(in), value :: dev
                        integer(c_int),     intent(inout)     :: flags
                        integer(c_int),     intent(inout)     :: active
                        integer(kind(CUDA_SUCCESS)) :: status
        end function cuDeviceSetPrimaryCtxGetState
    end interface
    
    !/**
        !* \brief Destroy all allocations and reset all state on the primary context
        !*
        !* Explicitly destroys and cleans up all resources associated with the current
        !* device in the current process.
        !*
        !* Note that it is responsibility of the calling function to ensure that no
        !* other module in the process is using the device any more. For that reason
        !* it is recommended to use ::cuDevicePrimaryCtxRelease() in most cases.
        !* However it is safe for other modules to call ::cuDevicePrimaryCtxRelease()
        !* even after resetting the device.
        !*
        !* \param dev - Device for which primary context is destroyed
        !*
       ! * \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_DEVICE,
        !* ::CUDA_ERROR_PRIMARY_CONTEXT_ACTIVE
        !* \notefnerr
        !*
        !* \sa ::cuDevicePrimaryCtxRetain,
        !* ::cuDevicePrimaryCtxRelease,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetCacheConfig,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize
        !*
    !*/
    
    interface
        function cuDevicePrimaryCtxReset(dev)  result(status)  &
                            bind(c,name='cuDevicePrimaryCtxReset')
                        use, intrinsic :: ISO_C_BINDING
                        import :: CUDA_SUCCESS
                        integer(c_int), intent(in), value :: dev
                        integer(kind(CUDA_SUCCESS)) :: status
        end function cuDevicePrimaryCtxReset
    end interface
    
    !/**
        !* \brief Create a CUDA context
    ! *
    !* \param pctx  - Returned context handle of the new context
    !* \param flags - Context creation flags
    !* \param dev   - Device to create context on
    !*
    !*   \return
    !* ::CUDA_SUCCESS,
    !* ::CUDA_ERROR_DEINITIALIZED,
    !* ::CUDA_ERROR_NOT_INITIALIZED,
    !* ::CUDA_ERROR_INVALID_CONTEXT,
    !* ::CUDA_ERROR_INVALID_DEVICE,
    !* ::CUDA_ERROR_INVALID_VALUE,
    !* ::CUDA_ERROR_OUT_OF_MEMORY,
    !* ::CUDA_ERROR_UNKNOWN
    !* \notefnerr
    !*
    !* \sa ::cuCtxDestroy,
    !* ::cuCtxGetApiVersion,
    !* ::cuCtxGetCacheConfig,
    !* ::cuCtxGetDevice,
    !* ::cuCtxGetFlags,
    !* ::cuCtxGetLimit,
    !* ::cuCtxPopCurrent,
    !* ::cuCtxPushCurrent,
    !* ::cuCtxSetCacheConfig,
    !* ::cuCtxSetLimit,
    !* ::cuCtxSynchronize
    !*/
    
    interface
        function cuCtxCreate(pctx,flags,dev)  result(status)  &
                        bind(c,name='cuCtxCreate')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUctx_st
                    import :: CUDA_SUCCESS
                    type(CUctx_st),     intent(inout)     :: pctx
                    integer(c_int),     intent(in), value :: flags
                    integer(c_int),     intent(in), value :: dev
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxCreate
    end interface
    
    !/**
        !* \brief Destroy a CUDA context
        !*
        !* Destroys the CUDA context specified by \p ctx.  The context \p ctx will be
        !* destroyed regardless of how many threads it is current to.
        !* It is the responsibility of the calling function to ensure that no API
        ! * call issues using \p ctx while ::cuCtxDestroy() is executing.
        !*
        !* If \p ctx is current to the calling thread then \p ctx will also be 
        !* popped from the current thread's context stack (as though ::cuCtxPopCurrent()
        !* were called).  If \p ctx is current to other threads, then \p ctx will
        !* remain current to those threads, and attempting to access \p ctx from
        !* those threads will result in the error ::CUDA_ERROR_CONTEXT_IS_DESTROYED.
        !*
        !* \param ctx - Context to destroy
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuCtxCreate,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetCacheConfig,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize
        !*/
    
    interface
        function cuCtxDestroy(ctx) result(status)  &
                            bind(c,name='cuCtxDestroy')
                 import :: CUDA_SUCCESS
                 import :: CUctx_st
                 type(CUctx_st),    intent(inout) :: ctx
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxDestroy
    end interface
    
    !/**
        !* \brief Pushes a context on the current CPU thread
        !*
        !* Pushes the given context \p ctx onto the CPU thread's stack of current
        !* contexts. The specified context becomes the CPU thread's current context, so
        !* all CUDA functions that operate on the current context are affected.
        !*
        !* The previous current context may be made current again by calling
        !* ::cuCtxDestroy() or ::cuCtxPopCurrent().
        !*
        !* \param ctx - Context to push
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuCtxCreate,
        !* ::cuCtxDestroy,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxSetCacheConfig,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize
    !*/
    
    interface
        function  cuCtxPushCurrent(ctx) result(status) &
                            bind(c,name='cuCtxPushCurrent')
                    import :: CUDA_SUCCESS
                    import :: CUctx_st
                    type(CUctx_st),     intent(inout) :: ctx
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxPushCurrent
    end interface
    
    !/**
        !* \brief Pops the current CUDA context from the current CPU thread.
        !*
        !* Pops the current CUDA context from the CPU thread and passes back the 
        !* old context handle in \p *pctx. That context may then be made current 
        !* to a different CPU thread by calling ::cuCtxPushCurrent().
        !*
        !* If a context was current to the CPU thread before ::cuCtxCreate() or
        !* ::cuCtxPushCurrent() was called, this function makes that context current to
        !* the CPU thread again.
        !*
        !* \param pctx - Returned new context handle
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT
        !* \notefnerr
        !*
        !* \sa ::cuCtxCreate,
        !* ::cuCtxDestroy,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetCacheConfig,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize
    !*/
    
    interface
        function cuCtxPopCurrent(ctx)  result(status) &
                            bind(c,name='cuCtxPopCurrent')
                    import :: CUDA_SUCCESS
                    import :: CUctx_st
                    type(CUctx_st),     intent(inout) :: ctx
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxPopCurrent
    end interface
    
     !/**
        !* \brief Binds the specified CUDA context to the calling CPU thread
        !*
        !* Binds the specified CUDA context to the calling CPU thread.
        !* If \p ctx is NULL then the CUDA context previously bound to the
        !* calling CPU thread is unbound and ::CUDA_SUCCESS is returned.
        !*
        !* If there exists a CUDA context stack on the calling CPU thread, this
        !* will replace the top of that stack with \p ctx.  
        !* If \p ctx is NULL then this will be equivalent to popping the top
        !* of the calling CPU thread's CUDA context stack (or a no-op if the
        !* calling CPU thread's CUDA context stack is empty).
        !*
        !* \param ctx - Context to bind to the calling CPU thread
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT
        !* \notefnerr
        !*
        !* \sa ::cuCtxGetCurrent, ::cuCtxCreate, ::cuCtxDestroy
        !*/
    
    interface
        function cuCtxSetCurrent(ctx) result(status) &
                        bind(c,name='cuCtxSetCurrent')
                    import :: CUDA_SUCCESS
                    import :: CUctx_st
                    type(CUctx_st),     intent(inout) :: ctx
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxSetCurrent
    end interface
    
     !/**
        !* \brief Returns the CUDA context bound to the calling CPU thread.
        !*
        !* Returns in \p *pctx the CUDA context bound to the calling CPU thread.
        !* If no context is bound to the calling CPU thread then \p *pctx is
        !* set to NULL and ::CUDA_SUCCESS is returned.
        !*
        !* \param pctx - Returned context handle
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* \notefnerr
        !*
        !* \sa ::cuCtxSetCurrent, ::cuCtxCreate, ::cuCtxDestroy
    !*/
    
    interface
        function cuCtxGetCurrent(ctx) result(status)  &
                        bind(c,name='cuCtxGetCurrent')
                    import :: CUDA_SUCCESS
                    import :: CUctx_st
                    type(CUctx_st),     intent(inout) :: ctx
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxGetCurrent
    end interface
    
    !/**
         !* \brief Returns the device ID for the current context
         !*
        !* Returns in \p *device the ordinal of the current context's device.
        !*
        !* \param device - Returned device ID for the current context
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* \notefnerr
        !*
        !* \sa ::cuCtxCreate,
        !* ::cuCtxDestroy,
        !*! ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetCacheConfig,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize
    !*/
    
    interface
        function cuCtxGetDevice(device) result(status) &
                        bind(c,name='cuCtxGetDevice')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_int), intent(inout) :: device
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxGetDevice
    end interface
    
    !/**
        !* \brief Returns the flags for the current context
        !*
        !* Returns in \p *flags the flags of the current context. See ::cuCtxCreate
        !* for flag values.
        !*
        !* \param flags - Pointer to store flags of current context
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* \notefnerr
        !*
        !* \sa ::cuCtxCreate,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !* ::cuCtxGetCurrent,
        !* ::cuCtxGetDevice
        !* ::cuCtxGetLimit,
        !* ::cuCtxGetSharedMemConfig,
        !* ::cuCtxGetStreamPriorityRange
        !*/
    
    interface
        function cuCtxGetFlags(flags) result(status) &
                        bind(c,name='cuCtxGetFlags')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_int),     intent(inout) :: flags
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxGetFlags
    end interface
    
    !/**
        !* \brief Block for a context's tasks to complete
        !*
        !* Blocks until the device has completed all preceding requested tasks.
        !* ::cuCtxSynchronize() returns an error if one of the preceding tasks failed.
        !* If the context was created with the ::CU_CTX_SCHED_BLOCKING_SYNC flag, the 
        !* CPU thread will block until the GPU context has finished its work.
        !*
        !* \return
        !* ::CUDA_SUCCESS,
       ! * ::CUDA_ERROR_DEINITIALIZED,
       ! * ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_CONTEXT
       ! * \notefnerr
        !*
        !* \sa ::cuCtxCreate,
       ! * ::cuCtxDestroy,
        !* ::cuCtxGetApiVersion,
       ! * ::cuCtxGetCacheConfig,
       ! * ::cuCtxGetDevice,
       ! * ::cuCtxGetFlags,
       ! * ::cuCtxGetLimit,
       ! * ::cuCtxPopCurrent,
       !!! * ::cuCtxPushCurrent,
       ! * ::cuCtxSetCacheConfig,
       ! * ::cuCtxSetLimit
    !*/
    
    interface
        function cuCtxSynchronize() result(status)  &
                        bind(c,name='cuCtxSynchronize')
                    import :: CUDA_SUCCESS
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxSynchronize
    end interface
    
    !/**
        !* \brief Set resource limits
        !*
        !* \param limit - Limit to set
        !* \param value - Size of limit
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_UNSUPPORTED_LIMIT,
        !* ::CUDA_ERROR_OUT_OF_MEMORY
        !* \notefnerr
        !*
        !* \sa ::cuCtxCreate,
        !* ::cuCtxDestroy,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetCacheConfig,
        !* ::cuCtxSynchronize
        !*/
    
    interface
        function cuCtxSetLimit(limit,val) result(status) &
                        bind(c,name='cuCtxSetLimit')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_int),     intent(in), value :: limit
                    integer(c_int),     intent(in), value :: val
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxSetLimit
    end interface
    
    !/**
        !* \brief Returns resource limits
        !*
        !* \param limit  - Limit to query
        !* \param pvalue - Returned size of limit
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_UNSUPPORTED_LIMIT
        !* \notefnerr
        !*
        !* \sa ::cuCtxCreate,
        !* ::cuCtxDestroy,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetCacheConfig,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize
    !*/
    
    interface
        function cuCtxGetLimit(pvalue,limit) result(status)  &
                        bind(c,name='cuCtxGetLimit')
                    use, intrinsic :: ISO_C_BINDING
                    !import :: CU_FUNC_CACHE_PREFER_NONE
                    import :: CUDA_SUCCESS
                    integer(c_size_t),       intent(inout)     :: pvalue
                    integer(c_int),             intent(in), value :: limit
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxGetLimit
    end interface
    
    !/**
        !* \brief Returns the preferred cache configuration for the current context.
        !*
        !* \param pconfig - Returned cache configuration
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuCtxCreate,
        !* ::cuCtxDestroy,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetCacheConfig,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize,
        !* ::cuFuncSetCacheConfig
        !*/
    
    interface
        function cuCtxGetCacheConfig(pconfig) result(status) &
                        bind(c,name='cuCtxGetCacheConfig')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CU_FUNC_CACHE_PREFER_NONE
                    import :: CUDA_SUCCESS
                    integer(kind(CU_FUNC_CACHE_PREFER_NONE)),     intent(inout) :: pconfig
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxGetCacheConfig
    end interface
    
    !/**
        !* \brief Sets the preferred cache configuration for the current context.
        !*
        !* \param config - Requested cache configuration
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuCtxCreate,
        !* ::cuCtxDestroy,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize,
        !* ::cuFuncSetCacheConfig
    !*/
    
    interface
        function cuCtxSetCacheConfig(config)  result(status) &
                            bind(c,name='cuCtxSetCacheConfig')
                     use, intrinsic :: ISO_C_BINDING
                     import :: CU_FUNC_CACHE_PREFER_NONE
                     import :: CUDA_SUCCESS
                     integer(kind(CU_FUNC_CACHE_PREFER_NONE)), intent(in), value :: config
                     integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxSetCacheConfig
    end interface
    
    !/**
        !* \brief Returns the current shared memory configuration for the current context.
        !*
        ! * \param pConfig - returned shared memory configuration
        !* \return 
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuCtxCreate,
        !* ::cuCtxDestroy,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize,
        !* ::cuCtxGetSharedMemConfig,
        !* ::cuFuncSetCacheConfig,
        !*/
    
    interface
        function cuCtxGetSharedMemConfig(pConfig) result(status) &
                                bind(c,name='cuCtxGetSharedMemConfig')
                    use, intrinsic :: ISO_C_BINDING
                    import ::  CU_SHARED_MEM_CONFIG_DEFAULT_BANK_SIZE 
                    import ::  CUDA_SUCCESS
                    integer(kind( CU_SHARED_MEM_CONFIG_DEFAULT_BANK_SIZE)), intent(inout) :: pConfig
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxGetSharedMemConfig
    end interface
    
    !/**
        !* \brief Sets the shared memory configuration for the current context.
        !*
        !* \param config - requested shared memory configuration
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuCtxCreate,
        !* ::cuCtxDestroy,
        !* ::cuCtxGetApiVersion,
        !* ::cuCtxGetCacheConfig,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize,
        !* ::cuCtxGetSharedMemConfig,
        !* ::cuFuncSetCacheConfig,
        !*/
    
    interface
        function cuCtxSetSharedMemConfig(config) result(status) &
                            bind(c,name='cuCtxSetSharedMemConfig')
                         use, intrinsic :: ISO_C_BINDING
                         import ::  CU_SHARED_MEM_CONFIG_DEFAULT_BANK_SIZE 
                         import ::  CUDA_SUCCESS
                         integer(kind( CU_SHARED_MEM_CONFIG_DEFAULT_BANK_SIZE)), intent(in), value :: Config
                         integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxSetSharedMemConfig
    end interface
    
    !/**
        !* \brief Gets the context's API version.
        !*
        !* \param ctx     - Context to check
        !* \param version - Pointer to version
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_UNKNOWN
        !* \notefnerr
        !*
        !* \sa ::cuCtxCreate,
        !* ::cuCtxDestroy,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxGetLimit,
        !* ::cuCtxPopCurrent,
        !* ::cuCtxPushCurrent,
        !* ::cuCtxSetCacheConfig,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize
        !*/
    
    interface
        function cuCtxGetApiVersion(ctx,version) result(status) &
                             bind(c,name='cuCtxGetApiVersion')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUctx_st
                    import :: CUDA_SUCCESS
                    type(CUctx_st),     intent(in)    :: ctx
                    integer(c_int),     intent(inout) :: version
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxGetApiVersion
    end interface
    
    !/**
        !* \brief Returns numerical values that correspond to the least and
        !* greatest stream priorities.
        !*
        !* Returns in \p *leastPriority and \p *greatestPriority the numerical values that correspond
        !* to the least and greatest stream priorities respectively. Stream priorities
        !* follow a convention where lower numbers imply greater priorities. The range of
        !* meaningful stream priorities is given by [\p *greatestPriority, \p *leastPriority].
        !* If the user attempts to create a stream with a priority value that is
        !* outside the meaningful range as specified by this API, the priority is
        !* automatically clamped down or up to either \p *leastPriority or \p *greatestPriority
        !* respectively. See ::cuStreamCreateWithPriority for details on creating a
        !* priority stream.
        !* A NULL may be passed in for \p *leastPriority or \p *greatestPriority if the value
        !* is not desired.
        !*
        !* This function will return '0' in both \p *leastPriority and \p *greatestPriority if
        !* the current context's device does not support stream priorities
        !* (see ::cuDeviceGetAttribute).
        !*
        !* \param leastPriority    - Pointer to an int in which the numerical value for least
        !*                           stream priority is returned
        !* \param greatestPriority - Pointer to an int in which the numerical value for greatest
        !*                           stream priority is returned
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* \notefnerr
        !*
        !* \sa ::cuStreamCreateWithPriority,
        !* ::cuStreamGetPriority,
        !* ::cuCtxGetDevice,
        !* ::cuCtxGetFlags,
        !* ::cuCtxSetLimit,
        !* ::cuCtxSynchronize
        !*/
    
    interface
        function cuCtxGetStreamPriorityRange(leastPriority,greatestPriority) result(status) &
                                bind(c,name='cuCtxGetStreamPriorityRange')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_int),     intent(inout) :: leastPriority
                    integer(c_int),     intent(inout) :: greatestPriority
                    integer(kind(CUDA_SUCCESS)) :: status
        end function
    end interface
    
    !/**
        !* \brief Loads a compute module
        !*
        !* Takes a filename \p fname and loads the corresponding module \p module into
        !* the current context. The CUDA driver API does not attempt to lazily
        !* allocate the resources needed by a module; if the memory for functions and
        !* data (constant and global) needed by the module cannot be allocated,
        !* ::cuModuleLoad() fails. The file should be a \e cubin file as output by
        !* \b nvcc, or a \e PTX file either as output by \b nvcc or handwritten, or
        !* a \e fatbin file as output by \b nvcc from toolchain 4.0 or later.
        !*
        !* \param module - Returned module
        !* \param fname  - Filename of module to load
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_PTX,
        !* ::CUDA_ERROR_NOT_FOUND,
        !* ::CUDA_ERROR_OUT_OF_MEMORY,
        !* ::CUDA_ERROR_FILE_NOT_FOUND,
        !* ::CUDA_ERROR_NO_BINARY_FOR_GPU,
        !* ::CUDA_ERROR_SHARED_OBJECT_SYMBOL_NOT_FOUND,
        !* ::CUDA_ERROR_SHARED_OBJECT_INIT_FAILED
        !* \notefnerr
        !*
        !* \sa ::cuModuleGetFunction,
        !* ::cuModuleGetGlobal,
        !* ::cuModuleGetTexRef,
        !* ::cuModuleLoadData,
        !* ::cuModuleLoadDataEx,
        !* ::cuModuleLoadFatBinary,
        !* ::cuModuleUnload
    !*/
    
    interface
        function cuModuleLoad(mod,fname)  result(status) &
                         bind(c,name='cuModuleLoad')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUmodule
                    import :: CUDA_SUCCESS
                    type(CUmodule),    intent(in) :: mod
                    character(c_char), intent(in) :: fname
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuModuleLoad
    end interface
    
    !/**
        !* \brief Load a module's data
        !*
        !* Takes a pointer \p image and loads the corresponding module \p module into
        !* the current context. The pointer may be obtained by mapping a \e cubin or
        !* \e PTX or \e fatbin file, passing a \e cubin or \e PTX or \e fatbin file
        !* as a NULL-terminated text string, or incorporating a \e cubin or \e fatbin
        !* object into the executable resources and using operating system calls such
        !* as Windows \c FindResource() to obtain the pointer.
        !*
        !* \param module - Returned module
        !* \param image  - Module data to load
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_PTX,
        !* ::CUDA_ERROR_OUT_OF_MEMORY,
        !* ::CUDA_ERROR_NO_BINARY_FOR_GPU,
        !* ::CUDA_ERROR_SHARED_OBJECT_SYMBOL_NOT_FOUND,
        !* ::CUDA_ERROR_SHARED_OBJECT_INIT_FAILED
        !* \notefnerr
        !*
        !* \sa ::cuModuleGetFunction,
        !* ::cuModuleGetGlobal,
        !* ::cuModuleGetTexRef,
        !* ::cuModuleLoad,
        !* ::cuModuleLoadDataEx,
        !* ::cuModuleLoadFatBinary,
        !* ::cuModuleUnload
    !*/
    
    interface
        function cuModuleLoadData(mod,images) result(status) &
                            bind(c,name='cuModuleLoadData')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUmodule
                    import :: CUDA_SUCCESS
                    type(CUmodule),     intent(inout) :: mod
                    type(c_ptr),        intent(in)    :: images
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuModuleLoadData
    end interface
    
    !/**
        !* \brief Unloads a module
        !*
        !* Unloads a module \p hmod from the current context.
        !*
        !* \param hmod - Module to unload
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
       ! * \sa ::cuModuleGetFunction,
       ! * ::cuModuleGetGlobal,
        !* ::cuModuleGetTexRef,
        !* ::cuModuleLoad,
        !* ::cuModuleLoadData,
        !* ::cuModuleLoadDataEx,
        !* ::cuModuleLoadFatBinary
    !*/
    
    interface
        function cuModuleUnload(hmod) result(status) &
                        bind(c,name='cuModuleUnload')
                import :: CUmodule
                import :: CUDA_SUCCESS
                type(CUmodule),     intent(in) :: hmod
                integer(kind(CUDA_SUCCESS)) :: status
        end function cuModuleUnload
    end interface
    
    !/**
        !* \brief Returns a function handle
        !*
        !* Returns in \p *hfunc the handle of the function of name \p name located in
        !* module \p hmod. If no function of that name exists, ::cuModuleGetFunction()
        !* returns ::CUDA_ERROR_NOT_FOUND.
        !*
        !* \param hfunc - Returned function handle
        !* \param hmod  - Module to retrieve function from
        !* \param name  - Name of function to retrieve
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_NOT_FOUND
        !* \notefnerr
        !*
        !* \sa ::cuModuleGetGlobal,
        !* ::cuModuleGetTexRef,
        !* ::cuModuleLoad,
        !* ::cuModuleLoadData,
        !* ::cuModuleLoadDataEx,
        !* ::cuModuleLoadFatBinary,
        !* ::cuModuleUnload
        !*/
    
    interface
        function cuModuleGetFunction(hfunc,hmod,name) result(status)   &
                           bind(c,name='cuModuleGetFunction')
                        use, intrinsic :: ISO_C_BINDING
                        import :: CUfunction
                        import :: CUmodule
                        import :: CUDA_SUCCESS
                        type(CUfunction),       intent(out) :: hfunc
                        type(CUmodule),         intent(in)  :: hmod
                        character(c_char),      intent(in)  :: name
                        integer(kind(CUDA_SUCCESS)) :: status
        end function cuModuleGetFunction
    end interface
    
    !/**
        !* \brief Returns a global pointer from a module
        !*
        !* Returns in \p *dptr and \p *bytes the base pointer and size of the
        !* global of name \p name located in module \p hmod. If no variable of that name
        !* exists, ::cuModuleGetGlobal() returns ::CUDA_ERROR_NOT_FOUND. Both
        !* parameters \p dptr and \p bytes are optional. If one of them is
        !* NULL, it is ignored.
        !*
        !* \param dptr  - Returned global device pointer
        !* \param bytes - Returned global size in bytes
        !* \param hmod  - Module to retrieve global from
        !* \param name  - Name of global to retrieve
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_NOT_FOUND
        !* \notefnerr
        !*
        !* \sa ::cuModuleGetFunction,
        !* ::cuModuleGetTexRef,
        !* ::cuModuleLoad,
        !* ::cuModuleLoadData,
        !* ::cuModuleLoadDataEx,
        !* ::cuModuleLoadFatBinary,
        !* ::cuModuleUnload
        !*/
    
    interface
        function cuModuleGetGlobal(dptr,bytes,hmod,name) result(status)   &
                          bind(c,name='cuModuleGetGlobal')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUmodule
                    import :: CUDA_SUCCESS
                    integer(c_size_t),      intent(inout) :: dptr
                    integer(c_size_t),      intent(inout) :: bytes
                    type(CUmodule),         intent(in)    :: hmod
                    character(c_char),      intent(in)    :: name
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuModuleGetGlobal
    end interface
    
    !/**
        !* \brief Returns a handle to a texture reference
        !*
        !* Returns in \p *pTexRef the handle of the texture reference of name \p name
        !* in the module \p hmod. If no texture reference of that name exists,
        !* ::cuModuleGetTexRef() returns ::CUDA_ERROR_NOT_FOUND. This texture reference
        !*       handle should not be destroyed, since it will be destroyed when the module
        !* is unloaded.
        !*
        !* \param pTexRef  - Returned texture reference
        !* \param hmod     - Module to retrieve texture reference from
        !* \param name     - Name of texture reference to retrieve
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_NOT_FOUND
        !* \notefnerr
        !*
        !* \sa ::cuModuleGetFunction,
        !* ::cuModuleGetGlobal,
        !* ::cuModuleGetSurfRef,
        !* ::cuModuleLoad,
        !*   ::cuModuleLoadData,
        !* ::cuModuleLoadDataEx,
        !* ::cuModuleLoadFatBinary,
        !* ::cuModuleUnload
        !*/
    
    interface
        function cuModuleGetTexRef(pTexRef,hmod,name) result(status) &
                            bind(c,name='cuModuleGetTexRef')
                        use, intrinsic :: ISO_C_BINDING
                        import :: CUtexref
                        import :: CUmodule
                        import :: CUDA_SUCCESS
                        type(CUtexref),     intent(inout) :: pTexRef
                        type(CUmodule),     intent(in)    :: hmod
                        character(c_char),  intent(in)    :: name
                        integer(kind(CUDA_SUCCESS)) :: status
        end function cuModuleGetTexRef
    end interface
    
    !/**
        !* \brief Returns a handle to a surface reference
        !*
        !* Returns in \p *pSurfRef the handle of the surface reference of name \p name
        !* in the module \p hmod. If no surface reference of that name exists,
        !* ::cuModuleGetSurfRef() returns ::CUDA_ERROR_NOT_FOUND.
        !*
        !* \param pSurfRef  - Returned surface reference
        !* \param hmod     - Module to retrieve surface reference from
        !* \param name     - Name of surface reference to retrieve
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_NOT_FOUND
        !* \notefnerr
        !*
        !* \sa ::cuModuleGetFunction,
        !* ::cuModuleGetGlobal,
        !* ::cuModuleGetTexRef,
        !* ::cuModuleLoad,
        !* ::cuModuleLoadData,
        !* ::cuModuleLoadDataEx,
        !* ::cuModuleLoadFatBinary,
        !* ::cuModuleUnload
        !*/
    
    interface
        function cuModuleGetSurfRef(pSurfRef,hmod,name) result(status) &
                                bind(c,name='cuModuleGetSurfRef')
                        use, intrinsic :: ISO_C_BINDING
                        import :: CUsurfref
                        import :: CUmodule
                        import :: CUDA_SUCCESS
                        type(CUsurfref),        intent(inout) :: pSurfRef
                        type(CUmodule),         intent(in)    :: hmod
                        character(c_char),      intent(in)    :: name
                        integer(kind(CUDA_SUCCESS)) :: status
        end function cuModuleGetSurfRef
    end interface
    
    !/**
        !* \brief Gets free and total memory
        !*
        !* Returns in \p *free and \p *total respectively, the free and total amount of
        !* memory available for allocation by the CUDA context, in bytes.
        !*
        !* \param free  - Returned free memory in bytes
        !* \param total - Returned total memory in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/
    
    interface
        function   cuMemGetInfo(free,total)  result(status) &
                        bind(c,name='cuMemGetInfo')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_size_t),      intent(inout) :: free
                    integer(c_size_t),      intent(inout) :: total
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemGetInfo
    end interface
    
    !/**
        !* \brief Allocates device memory
        !*
        !* Allocates \p bytesize bytes of linear memory on the device and returns in
        !* \p *dptr a pointer to the allocated memory. The allocated memory is suitably
        !* aligned for any kind of variable. The memory is not cleared. If \p bytesize
        !* is 0, ::cuMemAlloc() returns ::CUDA_ERROR_INVALID_VALUE.
        !*
        !* \param dptr     - Returned device pointer
        !* \param bytesize - Requested allocation size in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAllocHost,
       ! * ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
       ! * ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
       ! * ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
       ! */
    
    interface
        function cuMemAlloc(dptr,bytesize)  result(status) &
                        bind(c,name='cuMemAlloc')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_size_t),      intent(inout)     :: dptr
                    integer(c_size_t),      intent(in), value :: bytesize
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemAlloc
    end interface
    
    !/**
        !* \brief Allocates pitched device memory
        !*
        !* Allocates at least \p WidthInBytes * \p Height bytes of linear memory on
        !* the device and returns in \p *dptr a pointer to the allocated memory
        !* \param dptr             - Returned device pointer
        !* \param pPitch           - Returned pitch of allocation in bytes
        !* \param WidthInBytes     - Requested allocation width in bytes
        !* \param Height           - Requested allocation height in rows
        !* \param ElementSizeBytes - Size of largest reads/writes for range
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
       ! * ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
       ! */
    
    interface
        function cuMemAllocPitch(dptr,pPitch,WidthInBytes,Height,ElementSizeBytes) result(status) &
                        bind(c,name='cuMemAllocPitch')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_size_t),      intent(inout)     :: dptr
                    integer(c_size_t),      intent(inout)     :: pPitch
                    integer(c_size_t),      intent(in), value :: WidthInBytes
                    integer(c_size_t),      intent(in), value :: Height
                    integer(c_int),         intent(in), value :: ElementSizeBytes
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemAllocPitch
    end interface
    
    !/**
        !* \brief Frees device memory
        !*
        !* Frees the memory space pointed to by \p dptr, which must have been returned
        !* by a previous call to ::cuMemAlloc() or ::cuMemAllocPitch().
        !*
        !* \param dptr - Pointer to memory to free
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
       ! * ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/
    
    interface
        function cuMemFree(dptr) result(status) & 
                    bind(c,name='cuMemFree')
                use, intrinsic :: ISO_C_BINDING
                integer(c_size_t),      intent(in) :: dptr
                integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemFree
    end interface
    
    !/**
        !* \brief Get information on memory allocations
        !*
        !* Returns the base address in \p *pbase and size in \p *psize of the
        !* allocation by ::cuMemAlloc() or ::cuMemAllocPitch() that contains the input
        !* pointer \p dptr. Both parameters \p pbase and \p psize are optional. If one
        !* of them is NULL, it is ignored.
        !*
        !* \param pbase - Returned base address
        !* \param psize - Returned size of device memory allocation
        !* \param dptr  - Device pointer to query
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/
    
    interface
        function cuMemGetAddressRange(pbase,psize,dptr) result(status) &
                                bind(c,name='cuMemGetAddressRange')
                         use, intrinsic :: ISO_C_BINDING
                         import :: CUDA_SUCCESS
                         integer(c_size_t),     intent(inout)     :: pbase
                         integer(c_size_t),     intent(inout)     :: psize
                         integer(c_size_t),     intent(in), value :: dptr
                         integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemGetAddressRange
    end interface
    
    !/**
       ! * \brief Allocates page-locked host memory
        !*
        !* Allocates \p bytesize bytes of host memory that is page-locked and
        !* accessible to the device.
        !* \param pp       - Returned host pointer to page-locked memory
        !* \param bytesize - Requested allocation size in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
       ! */
    
    interface
        function cuMemAllocHost(pp,bytesize) result(status) &
                        bind(c,name='cuMemAllocHost')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    type(c_ptr),   intent(inout) :: pp
                    integer(c_size_t),  intent(in), value :: bytesize
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemAllocHost
    end interface
    
    !/**
        !* \brief Frees page-locked host memory
        !*
        !* Frees the memory space pointed to by \p p, which must have been returned by
        !* a previous call to ::cuMemAllocHost().
        !*
        !* \param p - Pointer to memory to free
        !*
        !* \return
        !*! ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
        !*/
    
    interface
        function cuMemFreeHost(p) result(status) &
                        bind(c,name='cuMemFreeHost')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    type(c_ptr),        intent(in), value :: p
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemFreeHost
    end interface
    
    !/**
        !* \brief Allocates page-locked host memory
        !*
        !* Allocates \p bytesize bytes of host memory that is page-locked and accessible
        !* to the device.
        !* \param pp       - Returned host pointer to page-locked memory
        !* \param bytesize - Requested allocation size in bytes
        !* \param Flags    - Flags for allocation request
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
        !*/
    
    interface
        function  cuMemHostAlloc(pp,bytesize,Flags) result(status)    &
                        bind(c,name='cuMemHostAlloc')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    type(c_ptr),        intent(inout) :: pp
                    integer(c_size_t),  intent(in), value :: bytesize
                    integer(c_int),     intent(in), value :: Flags
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemHostAlloc
    end interface
    
    !/**
        !* \brief Passes back device pointer of mapped pinned memory
        !*
        !* Passes back the device pointer \p pdptr corresponding to the mapped, pinned
        !* host buffer \p p allocated by ::cuMemHostAlloc.
        !* \p Flags provides for future releases. For now, it must be set to 0.
        !*
        !* \param pdptr - Returned device pointer
        !* \param p     - Host pointer
        !* \param Flags - Options (must be 0)
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
        !*/
    
    interface
        function cuMemHostGetDevicePointer(pdptr,p,Flags) result(status) &
                        bind(c,name='cuMemHostGetDevicePointer')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_size_t),      intent(inout)        :: pdptr
                    type(c_ptr),            intent(in), value :: p
                    integer(c_int),         intent(in),    value :: Flags
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemHostGetDevicePointer
    end interface
    
    !/**
       ! * \brief Passes back flags that were used for a pinned allocation
        !*
        !* Passes back the flags \p pFlags that were specified when allocating
        !* the pinned host buffer \p p allocated by ::cuMemHostAlloc.
       ! *
       ! * ::cuMemHostGetFlags() will fail if the pointer does not reside in
       ! * an allocation performed by ::cuMemAllocHost() or ::cuMemHostAlloc().
       ! *
       ! * \param pFlags - Returned flags word
       ! * \param p     - Host pointer
       ! *
       ! * \return
       ! * ::CUDA_SUCCESS,
       ! * ::CUDA_ERROR_DEINITIALIZED,
       ! * ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_CONTEXT,
       ! * ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuMemAllocHost, ::cuMemHostAlloc
        !*/
    
    interface
        function cuMemHostGetFlags(pFlags,p) result(status)  &
                    bind(c,name='cuMemHostGetFlags')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_int),     intent(inout) :: pFlags
                    type(c_ptr),        intent(in), value :: p
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemHostGetFlags
    end interface
    
    !/**
        !* \brief Allocates memory that will be automatically managed by the Unified Memory system
        !*
        !* Allocates \p bytesize bytes of managed memory on the device and returns in
        !* \p *dptr a pointer to the allocated memory.
        !* \param dptr     - Returned device pointer
        !* \param bytesize - Requested allocation size in bytes
        !* \param flags    - Must be one of ::CU_MEM_ATTACH_GLOBAL or ::CU_MEM_ATTACH_HOST
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_NOT_SUPPORTED,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
       ! * ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32,
        !!* ::cuDeviceGetAttribute, ::cuStreamAttachMemAsync
        !*/
    
    interface
        function cuMemAllocManaged(dptr,bytesize,flags) result(status) &
                        bind(c,name='cuMemAllocManaged')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_size_t),      intent(inout) :: dptr
                    integer(c_size_t),      intent(in), value :: bytesize
                    integer(c_int),         intent(in), value :: flags
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemAllocManaged
    end interface
    
    !/**
        !* \brief Returns a handle to a compute device
        !*
        !* Returns in \p *device a device handle given a PCI bus ID string.
        !*
        !* \param dev      - Returned device handle
        !*
        !* \param pciBusId - String in one of the following forms: 
        !* [domain]:[bus]:[device].[function]
        !* [domain]:[bus]:[device]
        !* [bus]:[device].[function]
        !* where \p domain, \p bus, \p device, and \p function are all hexadecimal values
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
       ! * \notefnerr
        !*
        !* \sa ::cuDeviceGet, ::cuDeviceGetAttribute, ::cuDeviceGetPCIBusId
       ! */
    
    interface
        function cuDeviceGetByPCIBusId(dev,pciBusId) result(status) &
                            bind(c,name='cuDeviceGetByPCIBusId')
                        use, intrinsic :: ISO_C_BINDING
                        import :: CUDA_SUCCESS
                        integer(c_int),     intent(inout) :: dev
                        character(c_char),  intent(in)    :: pciBusId
                        integer(kind(CUDA_SUCCESS)) :: status
        end function cuDeviceGetByPCIBusId
    end interface
    
    !/**
        !* \brief Returns a PCI Bus Id string for the device
        !*
        !* Returns an ASCII string identifying the device \p dev in the NULL-terminated
        !* string pointed to by \p pciBusId. \p len specifies the maximum length of the
        !* string that may be returned.
        !*
        !* \param pciBusId - Returned identifier string for the device in the following format
        !* [domain]:[bus]:[device].[function]
        !* where \p domain, \p bus, \p device, and \p function are all hexadecimal values.
        !* pciBusId should be large enough to store 13 characters including the NULL-terminator.
        !*
        !* \param len      - Maximum length of string to store in \p name
        !*
        !* \param dev      - Device to get identifier string for
       !!* \return
        !* ::CUDA_SUCCESS,
       ! * ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
        !* \notefnerr
        !*
        !* \sa ::cuDeviceGet, ::cuDeviceGetAttribute, ::cuDeviceGetByPCIBusId
        !*/
        
    interface
        function cuDeviceGetPCIBusId(pciBusId,length,dev) result(status) &
                            bind(c,name='cuDeviceGetPCIBusId')
                        use, intrinsic :: ISO_C_BINDING
                        import :: CUDA_SUCCESS
                        character(c_char),      intent(inout)     :: pciBusId
                        integer(c_int),         intent(in), value :: length
                        integer(c_int),         intent(in), value :: dev
                        integer(kind(CUDA_SUCCESS)) :: status
        end function cuDeviceGetPCIBusId
    end interface
    
    !/**
        !* \brief Gets an interprocess handle for a previously allocated event
        !*
        !* Takes as input a previously allocated event.
        !* \param pHandle - Pointer to a user allocated CUipcEventHandle
        !*                    in which to return the opaque event handle
        !* \param event   - Event allocated with ::CU_EVENT_INTERPROCESS and 
        !*                    ::CU_EVENT_DISABLE_TIMING flags.
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_INVALID_HANDLE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY,
        !* ::CUDA_ERROR_MAP_FAILED
        !*
        !* \sa 
        !* ::cuEventCreate, 
        !* ::cuEventDestroy, 
        !* ::cuEventSynchronize,
        !* ::cuEventQuery,
        !* ::cuStreamWaitEvent,
        !* ::cuIpcOpenEventHandle,
        !* ::cuIpcGetMemHandle,
        !* ::cuIpcOpenMemHandle,
        !* ::cuIpcCloseMemHandle
        !*/
    
    interface
        function cuIpcGetEventHandle(pHandle,event) result(status)  &
                            bind(c,name='cuIpcGetEventHandle')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUipcEventHandle_st
                    import :: CUevent
                    import :: CUDA_SUCCESS
                    type(CUipcEventHandle_st),      intent(inout) :: pHandle
                    type(CUevent),                  intent(in)    :: event
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuIpcGetEventHandle            
    end interface
    
    !/**
        !* \brief Registers an existing host memory range for use by CUDA
        !*
        !* The memory page-locked by this function must be unregistered with 
        !* ::cuMemHostUnregister().
        !*
        !* \param p        - Host pointer to memory to page-lock
        !* \param bytesize - Size in bytes of the address range to page-lock
        !* \param Flags    - Flags for allocation request
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY,
        !* ::CUDA_ERROR_HOST_MEMORY_ALREADY_REGISTERED,
        !* ::CUDA_ERROR_NOT_PERMITTED,
        !* ::CUDA_ERROR_NOT_SUPPORTED
        !* \notefnerr
        !*
        !* \sa ::cuMemHostUnregister, ::cuMemHostGetFlags, ::cuMemHostGetDevicePointer
   ! */
    
    interface
        function cuMemHostRegister(p,bytesize,Flags) result(status) &
                        bind(c,name='cuMemHostRegister')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    type(c_ptr),        intent(in), value :: p
                    integer(c_size_t),  intent(in), value :: bytesize
                    integer(c_int),     intent(in), value :: Flags
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemHostRegister
    end interface
    
    !/**
        !* \brief Unregisters a memory range that was registered with cuMemHostRegister.
        !*
        !* Unmaps the memory range whose base address is specified by \p p, and makes
        !* it pageable again.
        !*
        !* The base address must be the same one specified to ::cuMemHostRegister().
        !*
        !* \param p - Host pointer to memory to unregister
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY,
        !* ::CUDA_ERROR_HOST_MEMORY_NOT_REGISTERED,
        !* \notefnerr
        !*
        !* \sa ::cuMemHostRegister
    !*/
    
    interface
        function cuMemHostUnregister(p) result(status) &
                        bind(c,name='cuMemHostUnregister')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    type(c_ptr),        intent(in), value :: p
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemHostUnregister
    end interface
    
    !/**
        !* \brief Copies memory
        !*
        !* Copies data between two pointers. 
        !* \p dst and \p src are base pointers of the destination and source, respectively.  
        !* \p ByteCount specifies the number of bytes to copy.
        !* Note that this function infers the type of the transfer (host to host, host to 
        !*   device, device to device, or device to host) from the pointer values.  This
        !*   function is only allowed in contexts which support unified addressing.
        !*
        !* \param dst - Destination unified virtual address space pointer
        !* \param src - Source unified virtual address space pointer
        !* \param ByteCount - Size of memory copy in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/
    
    interface
        function cuMemcpy(dst,src,ByteCount) result(status)   &
                     bind(c,name='cuMemcpy')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  integer(c_size_t),    intent(in), value :: dst
                  integer(c_size_t),    intent(in),    value :: src
                  integer(c_size_t),    intent(in),    value :: ByteCount
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpy
    end interface
    
    !/**
        !* \brief Copies device memory between two contexts
        !*
        !* Copies from device memory in one context to device memory in another
        !* context. \p dstDevice is the base device pointer of the destination memory 
        !* and \p dstContext is the destination context.  \p srcDevice is the base 
        !* device pointer of the source memory and \p srcContext is the source pointer.  
        !* \p ByteCount specifies the number of bytes to copy.
        !*
        !* \param dstDevice  - Destination device pointer
        !* \param dstContext - Destination context
        !* \param srcDevice  - Source device pointer
        !* \param srcContext - Source context
        !* \param ByteCount  - Size of memory copy in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuMemcpyDtoD, ::cuMemcpy3DPeer, ::cuMemcpyDtoDAsync, ::cuMemcpyPeerAsync,
        !* ::cuMemcpy3DPeerAsync
        !*/
    
    interface
        function cuMemcpyPeer(dstDevice,dstContext,srcDevice,srcContext,ByteCount) &
                     bind(c,name='cuMemcpyPeer')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUctx_st
                    import :: CUDA_SUCCESS
                    integer(c_size_t),      intent(in), value :: dstDevice
                    type(CUctx_st),         intent(inout)        :: dstContext
                    integer(c_size_t),      intent(in),   value  :: srcDevice
                    type(CUctx_st),         intent(in)           :: srcContext
                    integer(c_size_t),      intent(in),   value  :: ByteCount
        end function cuMemcpyPeer
    end interface
    
    !/**
        !* \brief Copies memory from Host to Device
        !*
        !* Copies from host memory to device memory. \p dstDevice and \p srcHost are
        !* the base addresses of the destination and source, respectively. \p ByteCount
        !* specifies the number of bytes to copy.
        !*
        !* \param dstDevice - Destination device pointer
        !* \param srcHost   - Source host pointer
        !* \param ByteCount - Size of memory copy in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/
    
    interface
        function cuMemcpyHtoD(dstDevice,srcHost,ByteCount) result(status)  &
                      bind(c,name='cuMemcpyHtoD')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_size_t),    intent(in), value :: dstDevice
                    type(c_ptr),          intent(in),    value :: srcHost
                    integer(c_size_t),    intent(in),    value :: ByteCount
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyHtoD
    end interface
    
    !/**
        !* \brief Copies memory from Device to Host
        !*
        !* Copies from device to host memory. \p dstHost and \p srcDevice specify the
        !* base pointers of the destination and source, respectively. \p ByteCount
        !* specifies the number of bytes to copy.
        !*
        !* \param dstHost   - Destination host pointer
        !* \param srcDevice - Source device pointer
        !* \param ByteCount - Size of memory copy in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/
    
    interface
        function cuMemcpyDtoH(dstHost,srcDevice,ByteCount) result(status) &
                        bind(c,name='cuMemcpyDtoH')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    type(c_ptr),        intent(in), value :: dstHost
                    integer(c_size_t),  intent(in),    value :: srcDevice
                    integer(c_size_t),  intent(in),    value :: ByteCount
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyDtoH
    end interface
    
    !/**
        !* \brief Copies memory from Device to Device
        !*
        !* Copies from device memory to device memory. \p dstDevice and \p srcDevice
        !* are the base pointers of the destination and source, respectively.
        !* \p ByteCount specifies the number of bytes to copy.
        !*
        !* \param dstDevice - Destination device pointer
        !* \param srcDevice - Source device pointer
        !* \param ByteCount - Size of memory copy in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
        !*/
    
    interface
        function cuMemcpyDtoD(dstDevice,srcDevice,ByteCount) result(status) &
                    bind(c,name='cuMemcpyDtoD')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  integer(c_size_t),    intent(in), value :: dstDevice
                  integer(c_size_t),    intent(in),    value :: srcDevice
                  integer(c_size_t),    intent(in),    value :: ByteCount
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyDtoD
    end interface
    
     !/**
        !* \brief Copies memory from Device to Array
        !*
        !* Copies from device memory to a 1D CUDA array. \p dstArray and \p dstOffset
        !* specify the CUDA array handle and starting index of the destination data.
        !* \p srcDevice specifies the base pointer of the source. \p ByteCount
        !* specifies the number of bytes to copy.
        !*
        !* \param dstArray  - Destination array
        !* \param dstOffset - Offset in bytes of destination array
        !* \param srcDevice - Source device pointer
        !* \param ByteCount - Size of memory copy in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !*       ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/
    
    interface
        function cuMemcpyDtoA(dstArray,dstOffset,srcDevice,ByteCount) result(status) &
                        bind(c,name='cuMemcpyDtoA')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUArray_st
                    import :: CUDA_SUCCESS
                    type(CUArray_st),       intent(in), value :: dstArray
                    integer(c_size_t),      intent(in),    value :: dstOffset
                    integer(c_size_t),      intent(in),    value :: srcDevice
                    integer(c_size_t),      intent(in),    value :: ByteCount
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyDtoA
    end interface
    
    !/**
        !* \brief Copies memory from Array to Device
        !*
        !* Copies from one 1D CUDA array to device memory. \p dstDevice specifies the
        !* base pointer of the destination and must be naturally aligned with the CUDA
        !* array elements. \p srcArray and \p srcOffset specify the CUDA array handle
        !* and the offset in bytes into the array where the copy is to begin.
        !* \p ByteCount specifies the number of bytes to copy and must be evenly
        !* divisible by the array element size.
        !*
        !* \param dstDevice - Destination device pointer
        !* \param srcArray  - Source array
        !* \param srcOffset - Offset in bytes of source array
        !* \param ByteCount - Size of memory copy in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
       ! *! ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
       ! * ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
       ! * ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
       ! * ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/
    
    interface
        function cuMemcpyAtoD(dstDevice,srcArray,srcOffset,ByteCount) result(status) &
                    bind(c,name='cuMemcpyAtoD')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUArray_st
                  import :: CUDA_SUCCESS
                  integer(c_size_t),    intent(in), value :: dstDevice
                  type(CUArray_st),     intent(in),    value :: srcArray
                  integer(c_size_t),    intent(in),    value :: srcOffset
                  integer(c_size_t),    intent(in),    value :: ByteCount
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyAtoD
    end interface
    
   !/**
        !* \brief Copies memory from Host to Array
        !*
        !* Copies from host memory to a 1D CUDA array. \p dstArray and \p dstOffset
        !* specify the CUDA array handle and starting offset in bytes of the destination
        !* data.  \p pSrc specifies the base address of the source. \p ByteCount specifies
        !* the number of bytes to copy.
        !*
        !* \param dstArray  - Destination array
        !* \param dstOffset - Offset in bytes of destination array
        !* \param srcHost   - Source host pointer
        !* \param ByteCount - Size of memory copy in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/ 
    
    interface
        function cuMemcpyHtoA(dstArray,dstOffset,srcHost,ByteCount) result(status) &
                        bind(c,name='cuMemcpyHtoA')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUArray_st
                    import :: CUDA_SUCCESS
                    type(CUArray_st),     intent(in), value :: dstArray
                    integer(c_size_t),  intent(in),    value :: dstOffset
                    type(c_ptr),        intent(in),    value :: srcHost
                    integer(c_size_t),  intent(in),    value :: ByteCount
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyHtoA
    end interface
    
    !1/**
        !* \brief Copies memory from Array to Host
        !*
        !* Copies from one 1D CUDA array to host memory. \p dstHost specifies the base
        !* pointer of the destination. \p srcArray and \p srcOffset specify the CUDA
        !* array handle and starting offset in bytes of the source data.
        !* \p ByteCount specifies the number of bytes to copy.
        !*
        !* \param dstHost   - Destination device pointer
        !* \param srcArray  - Source array
       ! * \param srcOffset - Offset in bytes of source array
        !* \param ByteCount - Size of memory copy in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/
    
    interface
        function cuMemcpyAtoH(dstHost,srcArray,srcOffset,ByteCount) result(status) &
                        bind(c,name='cuMemcpyAtoH')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUArray_st
                  import :: CUDA_SUCCESS
                  type(c_ptr),      intent(in), value :: dstHost
                  type(CUArray_st), intent(in),    value :: srcArray
                  integer(c_size_t), intent(in),   value :: srcOffset
                  integer(c_size_t), intent(in),   value :: ByteCount
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyAtoH
    end interface
    
    !/**
        !* \brief Copies memory from Array to Array
        !*
        !* Copies from one 1D CUDA array to another. \p dstArray and \p srcArray
        !* specify the handles of the destination and source CUDA arrays for the copy,
        !* respectively. \p dstOffset and \p srcOffset specify the destination and
        !* source offsets in bytes into the CUDA arrays. \p ByteCount is the number of
        !* bytes to be copied. The size of the elements in the CUDA arrays need not be
        !* the same format, but the elements must be the same size; and count must be
        !* evenly divisible by that size.
        !*
        !* \param dstArray  - Destination array
        !* \param dstOffset - Offset in bytes of destination array
        !* \param srcArray  - Source array
        !* \param srcOffset - Offset in bytes of source array
        !* \param ByteCount - Size of memory copy in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
       ! * ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
       ! *
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
        !*/
    
    interface
        function cuMemcpyAtoA(dstArray,dstOffset,srcArray,srcOffset,ByteCount) result(status) &
                    bind(c,name='cuMemcpyAtoA')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUArray_st
                 import :: CUDA_SUCCESS
                 type(CUArray_st),      intent(in), value :: dstArray
                 integer(c_size_t),     intent(in),    value :: dstOffset
                 type(CUarray_st),      intent(in),    value :: srcArray
                 integer(c_size_t),     intent(in),    value :: srcOffset
                 integer(c_size_t),     intent(in),    value :: ByteCount
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyAtoA
    end interface
    
    !/**
        !* \brief Copies memory for 2D arrays
        !*
        !* Perform a 2D memory copy according to the parameters specified in \p pCopy.
        !* The ::CUDA_MEMCPY2D structure is defined as:
        !*
        !*
        !* \param pCopy - Parameters for the memory copy
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
        !*/
    
    interface
        function cuMemcpy2D(pCopy) result(status) &
                    bind(c,name='cuMemcpy2D')
                    import :: CUDA_MEMCPY2D_st
                    import :: CUDA_SUCCESS
              type(CUDA_MEMCPY2D_st),       intent(inout) :: pCopy
              integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpy2D
    end interface
    
    !/**
        !* \brief Copies memory for 2D arrays
        !*
        !* Perform a 2D memory copy according to the parameters specified in \p pCopy.
        !* The ::CUDA_MEMCPY2D structure is defined as:
        !*
        ! * \param pCopy - Parameters for the memory copy
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
        !*/
    
    interface
        function cuMemcpy2DUnaligned(pCopy) result(status) &
                        bind(c,name='cuMemcpy2DUnaligned')
                     import :: CUDA_MEMCPY2D_st
                     import :: CUDA_SUCCESS
                     type(CUDA_MEMCPY2D_st),    intent(inout) :: pCopy
                     integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpy2DUnaligned
    end interface
    
    !/**
        !* \brief Copies memory for 3D arrays
        !*
        !* Perform a 3D memory copy according to the parameters specified in
        !* \p pCopy. The ::CUDA_MEMCPY3D structure is defined as:
        !*
        !* \code
        !* \param pCopy - Parameters for the memory copy
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
        !*/
    
    interface
        function  cuMemcpy3D(pCopy) result(status) &
                        bind(c,name='cuMemcpy3D')
                    import :: CUDA_MEMCPY3D_st
                    import :: CUDA_SUCCESS
                    type(CUDA_MEMCPY3D_st),     intent(inout) :: pCopy
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpy3D
    end interface
    
    !/**
        !* \brief Copies memory between contexts
        !*
        !* Perform a 3D memory copy according to the parameters specified in
        !* \p pCopy.  See the definition of the ::CUDA_MEMCPY3D_PEER structure
        !* for documentation of its parameters.
        !*
        !* \param pCopy - Parameters for the memory copy
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_sync
        !*
        !* \sa ::cuMemcpyDtoD, ::cuMemcpyPeer, ::cuMemcpyDtoDAsync, ::cuMemcpyPeerAsync,
        !* ::cuMemcpy3DPeerAsync
    !*/  
    
    interface
        function cuMemcpy3DPeer(pCopy) result(status)  &
                     bind(c,name='cuMemcpy3DPeer')
                    import :: CUDA_MEMCPY3D_PEER_st
                    import :: CUDA_SUCCESS
                    type(CUDA_MEMCPY3D_PEER_st),    intent(inout) :: pCopy
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpy3DPeer
    end interface
    
    !/**
        !* \brief Copies memory asynchronously
        !*
        !* Copies data between two pointers. 
        !* \p dst and \p src are base pointers of the destination and source, respectively.  
        !* \p ByteCount specifies the number of bytes to copy.
        !* Note that this function infers the type of the transfer (host to host, host to 
        !*   device, device to device, or device to host) from the pointer values.  This
        !*   function is only allowed in contexts which support unified addressing.
        !*
        !* \param dst       - Destination unified virtual address space pointer
        !* \param src       - Source unified virtual address space pointer
        !* \param ByteCount - Size of memory copy in bytes
        !* \param hStream   - Stream identifier
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_async
        !* \note_null_stream
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
        !* ::cuMemsetD32, ::cuMemsetD32Async
    
    interface
        function cuMemcpyAsync(dst,src,ByteCount,hStream)  result(status) &
                        bind(c,name='cuMemcpyAsync')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    integer(c_size_t),      intent(in), value :: dst
                    integer(c_size_t),      intent(in),    value :: src
                    integer(c_size_t),      intent(in),    value :: ByteCount
                    type(CUstream),         intent(in),    value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyAsync
    end interface
    
    !/**
        !* \brief Copies device memory between two contexts asynchronously.
        !*
        !* Copies from device memory in one context to device memory in another
        !* context. \p dstDevice is the base device pointer of the destination memory 
        !* and \p dstContext is the destination context.  \p srcDevice is the base 
        !* device pointer of the source memory and \p srcContext is the source pointer.  
        !* \p ByteCount specifies the number of bytes to copy.
        !*
        !* \param dstDevice  - Destination device pointer
        !* \param dstContext - Destination context
        !* \param srcDevice  - Source device pointer
        !* \param srcContext - Source context
        !* \param ByteCount  - Size of memory copy in bytes
        !* \param hStream    - Stream identifier
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_async
        !* \note_null_stream
        !*
        !* \sa ::cuMemcpyDtoD, ::cuMemcpyPeer, ::cuMemcpy3DPeer, ::cuMemcpyDtoDAsync, 
       ! * ::cuMemcpy3DPeerAsync
       ! */
    
    interface
        function cuMemcpyPeerAsync(dstDevice,dstContext,srcDevice,srcContext,ByteCount,hStream) result(status) &
                        bind(c,name='cuMemcpyPeerAsync')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUctx_st
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    integer(c_size_t),      intent(in), value :: dstDevice
                    type(CUctx_st),         intent(in), value :: dstContext
                    integer(c_size_t),      intent(in),    value :: srcDevice
                    type(CUctx_st),         intent(in),    value :: srcContext
                    integer(c_size_t),      intent(in),    value :: ByteCount
                    type(CUstream),         intent(in),    value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyPeerAsync
    end interface
    
     !/**
        !* \brief Copies memory from Host to Device
        !*
        !* Copies from host memory to device memory. \p dstDevice and \p srcHost are
        !* the base addresses of the destination and source, respectively. \p ByteCount
        !* specifies the number of bytes to copy.
        !*
        !* \param dstDevice - Destination device pointer
        !* \param srcHost   - Source host pointer
        !* \param ByteCount - Size of memory copy in bytes
        !* \param hStream   - Stream identifier
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_async
        !* \note_null_stream
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
       ! * ::cuMemcpyHtoD, ::cuMemFree, ::cuMemFreeHost,
       ! * ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
       ! * ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
       ! * ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
       ! * ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
        !* ::cuMemsetD32, ::cuMemsetD32Async
    !*/
    
    interface
        function cuMemcpyHtoDAsync(dstDevice,srcHost,ByteCount,hStream) result(status)  &
                     bind(c,name='cuMemcpyHtoDAsync')
                use, intrinsic :: ISO_C_BINDING
                import :: CUstream
                import :: CUDA_SUCCESS
                integer(c_size_t),      intent(in), value :: dstDevice
                type(c_ptr),            intent(in),    value :: srcHost
                integer(c_size_t),      intent(in),    value :: ByteCount
                type(CUstream),         intent(in),    value :: hStream
                integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyHtoDAsync
    end interface
    
    !/**
        !* \brief Copies memory from Device to Host
        !*
        !* Copies from device to host memory. \p dstHost and \p srcDevice specify the
        !* base pointers of the destination and source, respectively. \p ByteCount
       ! * specifies the number of bytes to copy.
        !*
       ! * \param dstHost   - Destination host pointer
        !* \param srcDevice - Source device pointer
        !* \param ByteCount - Size of memory copy in bytes
        !* \param hStream   - Stream identifier
       ! *
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_CONTEXT,
       ! * ::CUDA_ERROR_INVALID_VALUE
        !! * \note_async
        !* \note_null_stream
       ! *
       ! * \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
       ! * ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
       ! * ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
       ! * ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
       ! * ::cuMemsetD32, ::cuMemsetD32Async
        
    interface
        function cuMemcpyDtoHAsync(dstHost,srcDevice,ByteCount,hStream) result(status) &
                        bind(c,name='cuMemcpyDtoHAsync')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(c_ptr),            intent(in), value :: dstHost
                    integer(c_size_t),      intent(in),    value :: srcDevice
                    integer(c_size_t),      intent(in),    value :: ByteCount
                    type(CUstream),         intent(in),    value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyDtoHAsync
    end interface
    
    !/**
        !* \brief Copies memory from Device to Device
        !*
        !* Copies from device memory to device memory. \p dstDevice and \p srcDevice
        !* are the base pointers of the destination and source, respectively.
        !* \p ByteCount specifies the number of bytes to copy.
        !*
        !* \param dstDevice - Destination device pointer
        !* \param srcDevice - Source device pointer
        !* \param ByteCount - Size of memory copy in bytes
        !* \param hStream   - Stream identifier
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_async
        !* \note_null_stream
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
        !* ::cuMemsetD32, ::cuMemsetD32Async
   ! */
    
    interface
        function cuMemcpyDtoDAsync(dstDevice,srcDevice,ByteCount,hStream) result(status)    &
                       bind(c,name='cuMemcpyDtoDAsync')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    integer(c_size_t),      intent(in), value :: dstDevice
                    integer(c_size_t),      intent(in),    value :: srcDevice
                    integer(c_size_t),      intent(in),    value :: ByteCount
                    type(CUstream),         intent(in),    value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function
    end interface
    
    !/**
        !* \brief Copies memory from Host to Array
        !*
        !* Copies from host memory to a 1D CUDA array. \p dstArray and \p dstOffset
        !* specify the CUDA array handle and starting offset in bytes of the
        !* destination data. \p srcHost specifies the base address of the source.
        !* \p ByteCount specifies the number of bytes to copy.
        !*
        !* \param dstArray  - Destination array
        !* \param dstOffset - Offset in bytes of destination array
        !* \param srcHost   - Source host pointer
        !* \param ByteCount - Size of memory copy in bytes
        !* \param hStream   - Stream identifier
       ! *
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
       ! * \notefnerr
        !* \note_async
        !* \note_null_stream
       !! *
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
       ! * ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
       ! * ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
       ! * ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
       ! * ::cuMemsetD32, ::cuMemsetD32Async
       ! */
    
    interface
        function cuMemcpyHtoAAsync(dstArray,dstOffset,srcHost,ByteCount,hStream) result(status) &
                        bind(c,name='cuMemcpyHtoAAsync')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUstream
                  import :: CUArray_st
                  import :: CUDA_SUCCESS
                  type(CUArray_st),     intent(in), value :: dstArray
                  integer(c_size_t),    intent(in),    value :: dstOffset
                  type(c_ptr),          intent(in),    value :: srcHost
                  integer(c_size_t),    intent(in),    value :: ByteCount
                  type(CUstream),       intent(in),    value :: hStream
                  integer(kind(CUDA_SUCCESS)) :: status
         end function cuMemcpyHtoAAsync
    end interface
    
    !/**
        !* \brief Copies memory from Array to Host
        !*
        !* Copies from one 1D CUDA array to host memory. \p dstHost specifies the base
        !* pointer of the destination. \p srcArray and \p srcOffset specify the CUDA
        !* array handle and starting offset in bytes of the source data.
        !* \p ByteCount specifies the number of bytes to copy.
        !*
        !* \param dstHost   - Destination pointer
        !* \param srcArray  - Source array
        !* \param srcOffset - Offset in bytes of source array
        !* \param ByteCount - Size of memory copy in bytes
        !* \param hStream   - Stream identifier
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_async
        !* \note_null_stream
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
        !* ::cuMemsetD32, ::cuMemsetD32Async
    !*/
    
    interface
        function cuMemcpyAtoHAsync(dstHost,srcArray,srcOffset,ByteCount,hStream) result(status) &
                        bind(c,name='cuMemcpyAtoHAsync')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUArray_st
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(c_ptr),        intent(in), value :: dstHost
                    type(CUArray_st),   intent(in),    value :: srcArray
                    integer(c_size_t),  intent(in),    value :: srcOffset
                    integer(c_size_t),  intent(in),    value :: ByteCount
                    type(CUstream),     intent(in),    value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpyAtoHAsync
    end interface
    
    !/**
        !* \brief Copies memory for 2D arrays
        !*
        !* Perform a 2D memory copy according to the parameters specified in \p pCopy.
        !* The ::CUDA_MEMCPY2D structure is defined as:
        !*
        !* \param pCopy   - Parameters for the memory copy
        !* \param hStream - Stream identifier
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_async
        !* \note_null_stream
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
        !* ::cuMemsetD32, ::cuMemsetD32Async
   ! */   
    
    interface
        function cuMemcpy2DAsync(pCopy,hStream) result(status) &
                        bind(c,name='cuMemcpy2DAsync') 
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_MEMCPY2D_st
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(CUDA_MEMCPY2D_st),     intent(inout)        :: pCopy
                    type(CUstream),             intent(in), value    :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpy2DAsync
    end interface
    
    !/**
        !* \brief Copies memory for 3D arrays
        !*
        !* Perform a 3D memory copy according to the parameters specified in
        !* \p pCopy. The ::CUDA_MEMCPY3D structure is defined as:
        !*
        !* \param pCopy - Parameters for the memory copy
        !* \param hStream - Stream identifier
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_async
        !* \note_null_stream
        !*!
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
        !* ::cuMemsetD32, ::cuMemsetD32Async
    !*/
    
    interface
        function cuMemcpy3DAsync(pCopy,hStream) result(status) &
                        bind(c,name='cuMemcpy3DAsync')
                    import :: CUDA_MEMCPY3D_st
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(CUDA_MEMCPY3D_st),     intent(inout)        :: pCopy
                    type(CUstream),             intent(in), value    :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpy3DAsync
    end interface
    
    !/**
        !* \brief Copies memory between contexts asynchronously.
        !*
        !* Perform a 3D memory copy according to the parameters specified in
        !* \p pCopy.  See the definition of the ::CUDA_MEMCPY3D_PEER structure
        !* for documentation of its parameters.
       ! *
        !* \param pCopy - Parameters for the memory copy
        !* \param hStream - Stream identifier
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_async
        !* \note_null_stream
        !*
        !* \sa ::cuMemcpyDtoD, ::cuMemcpyPeer, ::cuMemcpyDtoDAsync, ::cuMemcpyPeerAsync,
        !* ::cuMemcpy3DPeerAsync
        !*/
    
    interface
        function cuMemcpy3DPeerAsync(pCopy,hStream) result(status) &
                        bind(c,name='cuMemcpy3DPeerAsync')
                    import :: CUDA_MEMCPY3D_PEER_st
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(CUDA_MEMCPY3D_PEER_st),        intent(inout)        :: pCopy
                    type(CUstream),                     intent(in), value    :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemcpy3DPeerAsync
    end interface
    
    !/**
        !* \brief Initializes device memory
        !*
        !* Sets the memory range of \p N 8-bit values to the specified value
        !* \p uc.
        !*
        !* \param dstDevice - Destination device pointer
        !* \param uc        - Value to set
        !* \param N         - Number of elements
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_memset
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
       ! * ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
       ! * ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
        !* ::cuMemsetD32, ::cuMemsetD32Async
        !*/
    
    interface
        function cuMemsetD8(dstDevice,uc,N) result(status) &
                        bind(c,name='cuMemsetD8')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  integer(c_size_t),        intent(in), value :: dstDevice
                  integer(c_signed_char),        intent(in),    value :: uc
                  integer(c_size_t),        intent(in),    value :: N
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemsetD8
    end interface
    
    !/**
        !* \brief Initializes device memory
        !*
        !* Sets the memory range of \p N 16-bit values to the specified value
        !* \p us. The \p dstDevice pointer must be two byte aligned.
        !*
        !* \param dstDevice - Destination device pointer
        !* \param us        - Value to set
        !* \param N         - Number of elements
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !*   ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_memset
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !*   ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !*   ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16Async,
        !* ::cuMemsetD32, ::cuMemsetD32Async
        !*/
    
    interface
        function cuMemsetD16(dstDevice,us,N) result(status) &
                    bind(c,name='cuMemsetD16')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUDA_SUCCESS
                 integer(c_size_t),     intent(in), value :: dstDevice
                 integer(c_short),      intent(in),    value :: us
                 integer(c_size_t),     intent(in),    value :: N
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemsetD16
    end interface
    
    !/**
        !* \brief Initializes device memory
        !*
        !* Sets the memory range of \p N 32-bit values to the specified value
        !* \p ui. The \p dstDevice pointer must be four byte aligned.
        !*
        !* \param dstDevice - Destination device pointer
        !* \param ui        - Value to set
        !* \param N         - Number of elements
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_memset
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
       !  * ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
       ! * ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
        !* ::cuMemsetD32Async
        !*/
    
    interface
        function cuMemsetD32(dstDevice,ui,N) result(status) &
                    bind(c,name='cuMemsetD32')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  integer(c_size_t),        intent(in), value :: dstDevice
                  integer(c_int),           intent(in),    value :: ui
                  integer(c_size_t),        intent(in),    value :: N
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemsetD32
    end interface
    
    !/**
        !* \brief Initializes device memory
        !*
        !* Sets the 2D memory range of \p Width 8-bit values to the specified value
        !* \p uc. \p Height specifies the number of rows to set, and \p dstPitch
        !* specifies the number of bytes between each row. This function performs
        !* fastest when the pitch is one that has been passed back by
        !* ::cuMemAllocPitch().
        !*
        !* \param dstDevice - Destination device pointer
        !* \param dstPitch  - Pitch of destination device pointer
        !* \param uc        - Value to set
        !* \param Width     - Width of row
        !* \param Height    - Number of rows
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_memset
        !*
        !!* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8Async,
        !!* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
        !* ::cuMemsetD32, ::cuMemsetD32Async
        !*/
    
    interface
        function cuMemsetD2D8(dstDevice,dstPitch,uc,Width,Height) result(status) &
                    bind(c,name='cuMemsetD2D8')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  integer(c_size_t),        intent(in), value :: dstDevice
                  integer(c_size_t),        intent(in),    value :: dstPitch
                  integer(c_char),          intent(in),    value :: uc
                  integer(c_size_t),        intent(in),    value :: Width
                  integer(c_size_t),        intent(in),    value :: Height
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemsetD2D8
    end interface
    
    !/**
        !* \brief Initializes device memory
        !*
        !* Sets the 2D memory range of \p Width 16-bit values to the specified value
        !* \p us. \p Height specifies the number of rows to set, and \p dstPitch
        !* specifies the number of bytes between each row. The \p dstDevice pointer
        !* and \p dstPitch offset must be two byte aligned. This function performs
        !* fastest when the pitch is one that has been passed back by
        !* ::cuMemAllocPitch().
        !*
        !* \param dstDevice - Destination device pointer
        !* \param dstPitch  - Pitch of destination device pointer
        !* \param us        - Value to set
        !* \param Width     - Width of row
        !* \param Height    - Number of rows
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_memset
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
       ! * ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
       ! * ::cuMemsetD32, ::cuMemsetD32Async
       ! */
    
    interface
        function cuMemsetD2D16(dstDevice,dstPitch,us,Width,Height) result(status) &
                    bind(c,name='cuMemsetD2D16')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUDA_SUCCESS
                 integer(c_size_t),         value :: dstDevice
                 integer(c_size_t),         value :: dstPitch
                 integer(c_short),          value :: us
                 integer(c_size_t),         value :: Width
                 integer(c_size_t),         value :: Height
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemsetD2D16
    end interface
    
    !/**
        !* \brief Initializes device memory
        !*
        !* Sets the 2D memory range of \p Width 32-bit values to the specified value
        !* \p ui. \p Height specifies the number of rows to set, and \p dstPitch
        !* specifies the number of bytes between each row. The \p dstDevice pointer
        !* and \p dstPitch offset must be four byte aligned. This function performs
        !* fastest when the pitch is one that has been passed back by
        !* ::cuMemAllocPitch().
        !*
        !* \param dstDevice - Destination device pointer
        !* \param dstPitch  - Pitch of destination device pointer
        !* \param ui        - Value to set
        !* \param Width     - Width of row
        !* \param Height    - Number of rows
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_memset
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
        !* ::cuMemsetD32, ::cuMemsetD32Async
    !*/
    
    interface
        function cuMemsetD2D32(dstDevice,dstPitch,ui,Width,Height) result(status) &
                    bind(c,name='cuMemsetD3D32')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  integer(c_size_t),            value :: dstDevice
                  integer(c_size_t),            value :: dstPitch
                  integer(c_int),               value :: ui
                  integer(c_size_t),            value :: Width
                  integer(c_size_t),            value :: Height
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemsetD2D32
    end interface
    
    !/**
        !* \brief Sets device memory
        !*
        !* Sets the memory range of \p N 8-bit values to the specified value
        !* \p uc.
        !*
        !* \param dstDevice - Destination device pointer
        !* \param uc        - Value to set
        !* \param N         - Number of elements
        !* \param hStream   - Stream identifier
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_memset
        !* \note_null_stream
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD16Async,
        !* ::cuMemsetD32, ::cuMemsetD32Async
   ! */
    
    interface
        function cuMemsetD8Async(dstDevice,uc,N,hStream) result(status) &
                        bind(c,name='cuMemsetD8Async')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    integer(c_size_t),        value :: dstDevice
                    integer(c_char),          value :: uc
                    integer(c_size_t),        value :: N
                    type(CUstream),           value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemsetD8Async
    end interface
    
    !/**
        !* \brief Sets device memory
        !*
        !* Sets the memory range of \p N 16-bit values to the specified value
        !* \p us. The \p dstDevice pointer must be two byte aligned.
        !*
        !* \param dstDevice - Destination device pointer
        !* \param us        - Value to set
        !* \param N         - Number of elements
        !* \param hStream   - Stream identifier
        !*
        !* \return
        ! * ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_memset
        !* \note_null_stream
        !*
       ! * \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16,
        !* ::cuMemsetD32, ::cuMemsetD32Async
        !*/
    
    interface
        function cuMemsetD16Async(dstDevice,us,N,hStream) result(status) &
                        bind(c,name='cuMemsetD16Async')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    integer(c_size_t),         value :: dstDevice
                    integer(c_short),          value :: us
                    integer(c_size_t),         value :: N
                    type(CUstream),            value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemsetD16Async
    end interface
    
    !/**
        !* \brief Sets device memory
        !*
        !* Sets the memory range of \p N 32-bit values to the specified value
        !* \p ui. The \p dstDevice pointer must be four byte aligned.
        !*
        !* \param dstDevice - Destination device pointer
        !* \param ui        - Value to set
        !* \param N         - Number of elements
        !* \param hStream   - Stream identifier
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_memset
        !* \note_null_stream
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
        !* ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async, ::cuMemsetD32
    !*/
    
    interface
        function cuMemsetD32Async(dstDevice,ui,N,hStream) result(status) &
                        bind(c,name='cuMemsetD32Async')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    integer(c_size_t),         value :: dstDevice
                    integer(c_int),            value :: ui
                    integer(c_size_t),         value :: N
                    type(CUstream),             value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemsetD32Async
    end interface
    
    !/**
        !* \brief Sets device memory
        !*
        !* Sets the 2D memory range of \p Width 8-bit values to the specified value
        !* \p uc. \p Height specifies the number of rows to set, and \p dstPitch
        !* specifies the number of bytes between each row. This function performs
        !* fastest when the pitch is one that has been passed back by
        !* ::cuMemAllocPitch().
        !*
        !* \param dstDevice - Destination device pointer
        !* \param dstPitch  - Pitch of destination device pointer
        !* \param uc        - Value to set
        !* \param Width     - Width of row
        !* \param Height    - Number of rows
        !* \param hStream   - Stream identifier
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_memset
        !* \note_null_stream
        !*
       ! * \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
       ! * ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
       !  * ::cuMemsetD32, ::cuMemsetD32Async
      !   */
    
    interface
        function cuMemsetD2D8Async(dstDevice,dstPitch,uc,Width,Height,hStream) result(status) &
                        bind(c,name='cuMemsetD2D8Async')
                    use,intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    integer(c_size_t),         value :: dstDevice
                    integer(c_size_t),         value :: dstPitch
                    integer(c_char),           value :: uc
                    integer(c_size_t),         value :: Width
                    integer(c_size_t),         value :: Height
                    type(CUstream),            value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemsetD2D8Async
    end interface
    
    !/**
        !* \brief Sets device memory
        !*
        !* Sets the 2D memory range of \p Width 16-bit values to the specified value
        !* \p us. \p Height specifies the number of rows to set, and \p dstPitch
        !* specifies the number of bytes between each row. The \p dstDevice pointer 
        !* and \p dstPitch offset must be two byte aligned. This function performs
        !* fastest when the pitch is one that has been passed back by
        !* ::cuMemAllocPitch().
        !*
        !* \param dstDevice - Destination device pointer
        !* \param dstPitch  - Pitch of destination device pointer
        !* \param us        - Value to set
        !* \param Width     - Width of row
        !* \param Height    - Number of rows
        !* \param hStream   - Stream identifier
       ! *
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
       ! * \note_memset
        !* \note_null_stream
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
       ! * ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
       ! * ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
       ! * ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D32, ::cuMemsetD2D32Async,
       ! * ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
       ! * ::cuMemsetD32, ::cuMemsetD32Async
    !*/
    
    interface
        function cuMemsetD2D16Async(dstDevice,dstPitch,us,Width,Height,hStream) result(status) &
                        bind(c,name='cuMemsetD2D16Async')
                    use,intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    integer(c_size_t),          value :: dstDevice
                    integer(c_size_t),          value :: dstPitch
                    integer(c_short),           value :: us
                    integer(c_size_t),          value :: Width
                    integer(c_size_t),          value :: Height
                    type(CUstream),             value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemsetD2D16Async
    end interface
    
    !/**
        !* \brief Sets device memory
        !*
        !* Sets the 2D memory range of \p Width 32-bit values to the specified value
        !* \p ui. \p Height specifies the number of rows to set, and \p dstPitch
        !* specifies the number of bytes between each row. The \p dstDevice pointer
        !* and \p dstPitch offset must be four byte aligned. This function performs
        !* fastest when the pitch is one that has been passed back by
        !* ::cuMemAllocPitch().
        !*
        !* \param dstDevice - Destination device pointer
        !* \param dstPitch  - Pitch of destination device pointer
        !* \param ui        - Value to set
        !* \param Width     - Width of row
        !* \param Height    - Number of rows
        !* \param hStream   - Stream identifier
        !*
        !* \return
        !*! ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !* \note_memset
        !* \note_null_stream
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D8Async,
        !* ::cuMemsetD2D16, ::cuMemsetD2D16Async, ::cuMemsetD2D32,
        !* ::cuMemsetD8, ::cuMemsetD8Async, ::cuMemsetD16, ::cuMemsetD16Async,
        !* ::cuMemsetD32, ::cuMemsetD32Async
        !*/
    
    interface
        function cuMemsetD2D32Async(dstDevice,dstPitch,ui,Width,Height,hStream) result(status) &
                        bind(c,name='cuMemsetD2D32Async')
                    use,intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    integer(c_size_t),          value :: dstDevice
                    integer(c_size_t),          value :: dstPitch
                    integer(c_int),             value :: ui
                    integer(c_size_t),          value :: Width
                    integer(c_size_t),          value :: Height
                    type(CUstream),             value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemsetD2D32Async
    end interface
    
    !/**
        !* \brief Creates a 1D or 2D CUDA array
        !*
        !* Creates a CUDA array according to the ::CUDA_ARRAY_DESCRIPTOR structure
        !* \p pAllocateArray and returns a handle to the new CUDA array in \p *pHandle.
        !* The ::CUDA_ARRAY_DESCRIPTOR is defined as:
        !*
        !*
        !* \param pHandle        - Returned array
        !* \param pAllocateArray - Array descriptor
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY,
       ! * ::CUDA_ERROR_UNKNOWN
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
       ! * ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
       ! * ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/
    
    interface
        function cuArrayCreate(pHandle,pAllocateArray) result(status) &
                        bind(c,name='cuArrayCreate')
                    import :: CUArray_st
                    import :: CUDA_ARRAY_DESCRIPTOR_st
                    import :: CUDA_SUCCESS
                    type(CUArray_st),               intent(inout) :: pHandle
                    type(CUDA_ARRAY_DESCRIPTOR_st), intent(in)    :: pAllocateArray
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuArrayCreate
    end interface
    
    !/**
        !* \brief Get a 1D or 2D CUDA array descriptor
        !*
        !* Returns in \p *pArrayDescriptor a descriptor containing information on the
        !* format and dimensions of the CUDA array \p hArray. It is useful for
        !* subroutines that have been passed a CUDA array, but need to know the CUDA
        !* array parameters for validation or other purposes.
        !*
        !* \param pArrayDescriptor - Returned array descriptor
        !* \param hArray           - Array to get descriptor of
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_HANDLE
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
        !*/
        
    interface
        function cuArrayGetDescriptor(pArrayDescriptor,hArray) result(status) &
                        bind(c,name='cuArrayGetDescriptor')
                    import :: CUDA_ARRAY_DESCRIPTOR_st
                    import :: CUArray_st
                    import :: CUDA_SUCCESS
                    type(CUDA_ARRAY_DESCRIPTOR_st),     intent(inout)     :: pArrayDescriptor
                    type(CUArray_st),                   intent(in), value :: hArray
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuArrayGetDescriptor
    end interface
    
    !/**
        !* \brief Destroys a CUDA array
        !*
        !* Destroys the CUDA array \p hArray.
        !*
        !* \param hArray - Array to destroy
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_HANDLE,
        !* ::CUDA_ERROR_ARRAY_IS_MAPPED
        !* \notefnerr
        !*
        !* \sa ::cuArray3DCreate, ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
        !* ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/
    
    interface       
        function cuArrayDestroy(hArray) result(status) &
                    bind(c,name='cuArrayDestroy')
                  import :: CUArray_st
                  import :: CUDA_SUCCESS
                  type(CUArray_st),     value :: hArray
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuArrayDestroy
    end interface
    
    !/**
        !* \brief Creates a 3D CUDA array
        !*
        !* Creates a CUDA array according to the ::CUDA_ARRAY3D_DESCRIPTOR structure
        !* \p pAllocateArray and returns a handle to the new CUDA array in \p *pHandle.
        !* The ::CUDA_ARRAY3D_DESCRIPTOR is defined as:
        !*
        !* \param pHandle        - Returned array
        !* \param pAllocateArray - 3D array descriptor
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY,
        !* ::CUDA_ERROR_UNKNOWN
        !* \notefnerr
        !*
        !* \sa ::cuArray3DGetDescriptor, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
       ! * ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
        !* ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
        !* ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
        !* ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
        !* ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
        !* ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
    !*/
    
    interface
        function cuArray3DCreate(pHandle,pAllocateArray) result(status) &
                        bind(c,name='cuArray3DCreate')
                    import :: CUArray_st
                    import :: CUDA_ARRAY3D_DESCRIPTOR_st
                    import :: CUDA_SUCCESS
                    type(CUArray_st),                   intent(in)    :: pHandle
                    type(CUDA_ARRAY3D_DESCRIPTOR_st),   intent(inout) :: pAllocateArray
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuArray3DCreate
    end interface
    
    !/**
        !* \brief Get a 3D CUDA array descriptor
        !*
        !* Returns in \p *pArrayDescriptor a descriptor containing information on the
        !* format and dimensions of the CUDA array \p hArray. It is useful for
        !* subroutines that have been passed a CUDA array, but need to know the CUDA
        !* array parameters for validation or other purposes.
        !*
        !* This function may be called on 1D and 2D arrays, in which case the \p Height
        !* and/or \p Depth members of the descriptor struct will be set to 0.
        !*
        !* \param pArrayDescriptor - Returned 3D array descriptor
        !* \param hArray           - 3D array to get descriptor of
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
       ! * ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_CONTEXT,
       ! * ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_HANDLE
        !* \notefnerr
        !*
       ! * \sa ::cuArray3DCreate, ::cuArrayCreate,
        !* ::cuArrayDestroy, ::cuArrayGetDescriptor, ::cuMemAlloc, ::cuMemAllocHost,
        !* ::cuMemAllocPitch, ::cuMemcpy2D, ::cuMemcpy2DAsync, ::cuMemcpy2DUnaligned,
        !* ::cuMemcpy3D, ::cuMemcpy3DAsync, ::cuMemcpyAtoA, ::cuMemcpyAtoD,
       ! * ::cuMemcpyAtoH, ::cuMemcpyAtoHAsync, ::cuMemcpyDtoA, ::cuMemcpyDtoD, ::cuMemcpyDtoDAsync,
       ! * ::cuMemcpyDtoH, ::cuMemcpyDtoHAsync, ::cuMemcpyHtoA, ::cuMemcpyHtoAAsync,
       !     *   ::cuMemcpyHtoD, ::cuMemcpyHtoDAsync, ::cuMemFree, ::cuMemFreeHost,
       ! * ::cuMemGetAddressRange, ::cuMemGetInfo, ::cuMemHostAlloc,
       ! * ::cuMemHostGetDevicePointer, ::cuMemsetD2D8, ::cuMemsetD2D16,
       ! * ::cuMemsetD2D32, ::cuMemsetD8, ::cuMemsetD16, ::cuMemsetD32
       ! */
    
    interface
        function  cuArray3DGetDescriptor(pArrayDescriptor,hArray) &
                        bind(c,name='cuArray3DGetDescriptor')
                    import :: CUDA_ARRAY3D_DESCRIPTOR_st
                    import :: CUArray_st
                    import :: CUDA_SUCCESS
                    type(CUDA_ARRAY3D_DESCRIPTOR_st),   intent(inout) :: pArrayDescriptor
                    type(CUArray_st),                   intent(in)    :: hArray
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuArray3DGetDescriptor
    end interface
    
    !*
        !* \param data      - Returned pointer attribute value
        !* \param attribute - Pointer attribute to query
        !* \param ptr       - Pointer
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
        !* \notefnerr
        !*
        !* \sa cuPointerSetAttribute,
        !* ::cuMemAlloc,
        !* ::cuMemFree,
        !* ::cuMemAllocHost,
        !* ::cuMemFreeHost,
        !* ::cuMemHostAlloc,
       ! * ::cuMemHostRegister,
        ! * ::cuMemHostUnregister
    !*/
    
    interface
        function cuPointerGetAttribute(datum,attribute,ptr) result(status) &
                        bind(c,name='cuPointerGetAttribute')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CU_POINTER_ATTRIBUTE_CONTEXT
                    import :: CUDA_SUCCESS
                    type(c_ptr),         value    :: datum
                    integer(kind(CU_POINTER_ATTRIBUTE_CONTEXT)) :: attribute
                    integer(c_size_t),   value    :: ptr
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuPointerGetAttribute
    end interface
    
    !/**
        !* \brief Prefetches memory to the specified destination device
        !*
        !* \param devPtr    - Pointer to be prefetched
        !* \param count     - Size in bytes
        !* \param dstDevice - Destination device to prefetch to
        !* \param hStream    - Stream to enqueue prefetch operation
        !*!
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
        !* \notefnerr
        !* \note_async
        !* \note_null_stream
        !*
        !* \sa ::cuMemcpy, ::cuMemcpyPeer, ::cuMemcpyAsync,
        !* ::cuMemcpy3DPeerAsync, ::cuMemAdvise
        !*/
    
    interface
        function cuMemPrefetchAsync(devPtr,count,dstDevice,hStream) result(status) &
                        bind(c,name='cuMemPrefetchAsync')
                    use, intrinsic :: ISO_C_BINDING
                    import ::  CUstream
                    import ::  CUDA_SUCCESS
                    integer(c_size_t),          value :: devPtr
                    integer(c_size_t),          value :: count
                    integer(c_int),             value :: dstDevice
                    type(CUstream),             value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemPrefetchAsync
    end interface
    
    !/**
        !* \brief Advise about the usage of a given memory range
        !*
        !* Advise the Unified Memory subsystem about the usage pattern for the memory range
        !* starting at \p devPtr with a size of \p count bytes.
        !* \param devPtr - Pointer to memory to set the advice for
        !* \param count  - Size in bytes of the memory range
        !* \param advice - Advice to be applied for the specified memory range
        !* \param device - Device to apply the advice for
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
        !* \notefnerr
        !* \note_async
        !* \note_null_stream
        !*
        !* \sa ::cuMemcpy, ::cuMemcpyPeer, ::cuMemcpyAsync,
        !* ::cuMemcpy3DPeerAsync, ::cuMemPrefetchAsync
    !*/
    
    interface
        function cuMemAdvise(devPtr,count,advice,device) result(status) &
                    bind(c,name='cuMemAdvise')
                 use, intrinsic :: ISO_C_BINDING
                 !import :: CU_MEM_ADVISE_SET_READ_MOSTLY
                 import :: CUDA_SUCCESS
                 integer(c_size_t),       value :: devPtr
                 integer(c_size_t),       value :: count
                 integer(c_int),          value :: advice
                 integer(c_int),          value :: device
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemAdvise
    end interface
    
    !/**
        !* \brief Query an attribute of a given memory range
        !* 
        !* Query an attribute about the memory range starting at \p devPtr with a size of \p count bytes. The
        !   * memory range must refer to managed memory allocated via ::cuMemAllocManaged or declared via
        !* __managed__ variables.
        !*
        ! * \param data      - A pointers to a memory location where the result
        !*                    of each attribute query will be written to.
        !* \param dataSize  - Array containing the size of data
        !* \param attribute - The attribute to query
        !* \param devPtr    - Start of the range to query
        !* \param count     - Size of the range to query
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
        !* \notefnerr
       ! * \note_async
        !* \note_null_stream
        !*
        !* \sa ::cuMemRangeGetAttributes, ::cuMemPrefetchAsync,
       ! * ::cuMemAdvise
    !*/
        
    interface
        function cuMemRangeGetAttribute(datum,dataSize,attribute,devPtr,count) result(status) &
                        bind(c,name='cuMemRangeGetAttribute')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  type(c_ptr),              value :: datum
                  integer(c_size_t),        value :: dataSize
                  integer(c_int),           value :: attribute
                  integer(c_size_t),        value :: devPtr
                  integer(c_size_t),        value :: count
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemRangeGetAttribute
    end interface
    
    !/**
        !* \brief Query attributes of a given memory range.
        !*
        !* Query attributes of the memory range starting at \p devPtr with a size of \p count bytes. The
        !* memory range must refer to managed memory allocated via ::cuMemAllocManaged or declared via
        !* __managed__ variables.
        !* \param data          - A two-dimensional array containing pointers to memory
        !*                        locations where the result of each attribute query will be written to.
        !* \param dataSizes     - Array containing the sizes of each result
        !* \param attributes    - An array of attributes to query
        !*                        (numAttributes and the number of attributes in this array should match)
        !* \param numAttributes - Number of attributes to query
        !* \param devPtr        - Start of the range to query
        !* \param count         - Size of the range to query
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_DEVICE
        !* \notefnerr
        !*
        !* \sa ::cuMemRangeGetAttribute, ::cuMemAdvise
        !* ::cuMemPrefetchAsync
    !*/
    
    interface
        function cuMemRangeGetAttributes(datum,dataSizes,attributes,numAttributes,devPtr,count) result(status) &
                        bind(c,name='cuMemRangeGetAttributes')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    type(c_ptr),        dimension(*)       :: datum
                    integer(c_size_t),  dimension(*)       :: dataSizes
                    integer(c_int),     dimension(*)       :: attributes
                    integer(c_size_t),               value :: numAttributes
                    integer(c_size_t),               value :: devPtr
                    integer(c_size_t),               value :: count
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuMemRangeGetAttributes
    end interface
    
    !/**
        !* \brief Set attributes on a previously allocated memory region
        !*
        !* \param value     - Pointer to memory containing the value to be set
        !* \param attribute - Pointer attribute to set
        !* \param ptr       - Pointer to a memory region allocated using CUDA memory allocation APIs
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
       ! * ::CUDA_ERROR_INVALID_DEVICE
       ! * \notefnerr
       ! *
       ! * \sa ::cuPointerGetAttribute,
       ! * ::cuPointerGetAttributes,
       ! * ::cuMemAlloc,
       ! * ::cuMemFree,
       ! * ::cuMemAllocHost,
       ! * ::cuMemFreeHost,
       ! * ::cuMemHostAlloc,
       ! * ::cuMemHostRegister,
       ! * ::cuMemHostUnregister
   ! */
    
    interface
        function cuPointerSetAttribute(val,attribute,ptr) result(status) &
                        bind(c,name='cuPointerSetAttribute')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  type(c_ptr),          value :: val
                  integer(c_int),       value :: attribute
                  integer(c_size_t),    value :: ptr
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuPointerSetAttribute
    end interface
    
    !/**
        !* \brief Create a stream
        !*
        !* Creates a stream and returns a handle in \p phStream.  The \p Flags argument
        !* determines behaviors of the stream.  Valid values for \p Flags are:
        !* - ::CU_STREAM_DEFAULT: Default stream creation flag.
        !* - ::CU_STREAM_NON_BLOCKING: Specifies that work running in the created 
        !*   stream may run concurrently with work in stream 0 (the NULL stream), and that
        !*   the created stream should perform no implicit synchronization with stream 0.
        !*
        !* \param phStream - Returned newly created stream
        !* \param Flags    - Parameters for stream creation
        !*
       ! * \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY
        !* \notefnerr
       ! *
        !* \sa ::cuStreamDestroy,
        !* ::cuStreamCreateWithPriority,
        !* ::cuStreamGetPriority,
        !* ::cuStreamGetFlags,
        !* ::cuStreamWaitEvent,
        !* ::cuStreamQuery,
        !* ::cuStreamSynchronize,
       ! * ::cuStreamAddCallback
   ! */
    
    interface
        function cuStreamCreate(phStream,Flags) result(status) &
                        bind(c,name='cuStreamCreate')
                    use,intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(CUstream),     intent(inout) :: phStream
                    integer(c_int),     intent(in), value :: Flags
                    integer(kind(CUDA_SUCCESS)) ::  status
        end function cuStreamCreate
    end interface
    
    !/**
        !* \brief Create a stream with the given priority
        !*
        !* Creates a stream with the specified priority and returns a handle in \p phStream.
        !* This API alters the scheduler priority of work in the stream. Work in a higher
        !* priority stream may preempt work already executing in a low priority stream.
        !*
        !* \p priority follows a convention where lower numbers represent higher priorities.
       ! * '0' represents default priority. The range of meaningful numerical priorities can
        !* be queried using ::cuCtxGetStreamPriorityRange. If the specified priority is
        !* outside the numerical range returned by ::cuCtxGetStreamPriorityRange,
       ! * it will automatically be clamped to the lowest or the highest number in the range.
       ! *
        !* \param phStream    - Returned newly created stream
        !* \param flags       - Flags for stream creation. See ::cuStreamCreate for a list of
       ! *                      valid flags
        !* \param priority    - Stream priority. Lower numbers represent higher priorities.
       ! *                      See ::cuCtxGetStreamPriorityRange for more information about
        !*                      meaningful stream priorities that can be passed.
        !*
       ! * \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
       ! * ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY
        !* \notefnerr
        !*
        !* \note Stream priorities are supported only on Quadro and Tesla GPUs
        !* with compute capability 3.5 or higher.
        !*
        !* \note In the current implementation, only compute kernels launched in
        !* priority streams are affected by the stream's priority. Stream priorities have
        !* no effect on host-to-device and device-to-host memory operations.
        !*
        !* \sa ::cuStreamDestroy,
        !* ::cuStreamCreate,
       ! * ::cuStreamGetPriority,
        !* ::cuCtxGetStreamPriorityRange,
       ! * ::cuStreamGetFlags,
        !* ::cuStreamWaitEvent,
        !* ::cuStreamQuery,
        !* ::cuStreamSynchronize,
       ! * ::cuStreamAddCallback
    !*/
    
    interface
        function cuStreamCreateWithPriority(phStream,flags,priority) result(status) &
                        bind(c,name='cuStreamCreateWithPriority')
                    use,intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(CUstream),     intent(inout) :: phStream
                    integer(c_int),     intent(in), value :: flags
                    integer(c_int),     intent(in), value :: priority
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuStreamCreateWithPriority
    end interface
    
    !/**
        !* \brief Query the priority of a given stream
        !*
        !* Query the priority of a stream created using ::cuStreamCreate or ::cuStreamCreateWithPriority
        !* and return the priority in \p priority. Note that if the stream was created with a
        !* priority outside the numerical range returned by ::cuCtxGetStreamPriorityRange,
        !* this function returns the clamped priority.
        !* See ::cuStreamCreateWithPriority for details about priority clamping.
       ! *
        !* \param hStream    - Handle to the stream to be queried
        !* \param priority   - Pointer to a signed integer in which the stream's priority is returned
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_HANDLE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY
        !* \notefnerr
        !*
        !* \sa ::cuStreamDestroy,
       ! * ::cuStreamCreate,
       ! * ::cuStreamCreateWithPriority,
       ! * ::cuCtxGetStreamPriorityRange,
        !* ::cuStreamGetFlags
    !*/
    
    interface
        function cuStreamGetPriority(hStream,priority) result(status) &
                        bind(c,name='cuStreamGetPriority')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(CUstream),        value :: hStream
                    integer(c_int)              :: priority
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuStreamGetPriority
    end interface
    
    !/**
        !* \brief Query the flags of a given stream
        !*
        !* Query the flags of a stream created using ::cuStreamCreate or ::cuStreamCreateWithPriority
        !* and return the flags in \p flags.
        !*
        !* \param hStream    - Handle to the stream to be queried
        !* \param flags      - Pointer to an unsigned integer in which the stream's flags are returned
        !*                     The value returned in \p flags is a logical 'OR' of all flags that
        !*                     were used while creating this stream. See ::cuStreamCreate for the list
        !*                     of valid flags
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_INVALID_HANDLE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY
        !* \notefnerr
        !*
        !* \sa ::cuStreamDestroy,
        !* ::cuStreamCreate,
       ! * ::cuStreamGetPriority
    !*/
    
    interface
        function cuStreamGetFlags(hStream,flags) result(status) &
                    bind(c,name='cuStreamGetFlags')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUstream
                 import :: CUDA_SUCCESS
                 type(CUstream),     value :: hStream
                 integer(c_int)           :: flags
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cuStreamGetFlags
    end interface
    
    !/**
        !* \brief Make a compute stream wait on an event
        !*
        !* \param hStream - Stream to wait
        !* \param hEvent  - Event to wait on (may not be NULL)
        !* \param Flags   - Parameters for the operation (must be 0)
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_HANDLE,
        !* \note_null_stream
        !* \notefnerr
        !*
        !* \sa ::cuStreamCreate,
        !* ::cuEventRecord,
        ! * ::cuStreamQuery,
        !* ::cuStreamSynchronize,
        !* ::cuStreamAddCallback,
        !* ::cuStreamDestroy
    !*/
    
    interface
        function cuStreamWaitEvent(hStream,hEvent,flags) result(status)  &
                     bind(c,name='cuStreamWaitEvent')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUstream
                  import :: CUevent
                  import :: CUDA_SUCCESS
                  type(CUstream),      value :: hStream
                  type(CUevent),       value :: hEvent
                  integer(c_int),      value :: flags
        end function cuStreamWaitEvent
    end interface
    
    !/**
        !* \brief Attach memory to a stream asynchronously
        !*
        !* Enqueues an operation in \p hStream to specify stream association of
        !* \p length bytes of memory starting from \p dptr
        !* \param hStream - Stream in which to enqueue the attach operation
        !* \param dptr    - Pointer to memory (must be a pointer to managed memory)
        !* \param length  - Length of memory (must be zero)
        !* \param flags   - Must be one of ::CUmemAttach_flags
        !*
       ! * \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_HANDLE,
        !* ::CUDA_ERROR_NOT_SUPPORTED
        !* \note_null_stream
        !* \notefnerr
       ! *
        !* \sa ::cuStreamCreate,
        !* ::cuStreamQuery,
       ! * ::cuStreamSynchronize,
       ! * ::cuStreamWaitEvent,
       ! * ::cuStreamDestroy,
       ! * ::cuMemAllocManaged
    !*/
    
    interface
        function cuStreamAttachMemAsync(hStream,dptr,length,flags) result(status) &
                    bind(c,name='cuStreamAttachMemAsync')
                use, intrinsic :: ISO_C_BINDING
                import :: CUstream
                import :: CUDA_SUCCESS
                type(CUstream),         value :: hStream
                integer(c_size_t),      value :: dptr
                integer(c_size_t),      value :: length
                integer(c_int),         value :: flags
                integer(kind(CUDA_SUCCESS)) :: status
        end function cuStreamAttachMemAsync
    end interface
    
    !/**
        !* \brief Determine status of a compute stream
        !*
        !* Returns ::CUDA_SUCCESS if all operations in the stream specified by
        !* \p hStream have completed, or ::CUDA_ERROR_NOT_READY if not.
        !*
        !* For the purposes of Unified Memory, a return value of ::CUDA_SUCCESS
       ! * is equivalent to having called ::cuStreamSynchronize().
        !*
        !* \param hStream - Stream to query status of
       ! *
        !* \return
        !* ::CUDA_SUCCESS,
       ! * ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_HANDLE,
        !* ::CUDA_ERROR_NOT_READY
        !* \note_null_stream
        !* \notefnerr
        !*
        !* \sa ::cuStreamCreate,
        !* ::cuStreamWaitEvent,
        !* ::cuStreamDestroy,
       ! * ::cuStreamSynchronize,
       ! * ::cuStreamAddCallback
       ! */
    
    interface
        function cuStreamQuery(hStream) result(status) &
                   bind(c,name='cuStreamQuery')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUstream
                 import :: CUDA_SUCCESS
                 type(CUstream),   value :: hStream
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cuStreamQuery
    end interface
    
    !/**
        !* \brief Wait until a stream's tasks are completed
        !*
        !* Waits until the device has completed all operations in the stream specified
        !* by \p hStream. If the context was created with the 
        !* ::CU_CTX_SCHED_BLOCKING_SYNC flag, the CPU thread will block until the
        !* stream is finished with all of its tasks.
        !*
        !* \param hStream - Stream to wait for
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
       ! * ::CUDA_ERROR_INVALID_HANDLE
        !* \note_null_stream
        !* \notefnerr
        !*
        !* \sa ::cuStreamCreate,
        !* ::cuStreamDestroy,
        !* ::cuStreamWaitEvent,
        !* ::cuStreamQuery,
        !* ::cuStreamAddCallback
   ! */
    
    interface
        function cuStreamSynchronize(hStream) result(status) &
                    bind(c,name='cuStreamSynchronize')
                  import :: CUstream
                  import :: CUDA_SUCCESS
                  type(CUstream),    value :: hStream
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuStreamSynchronize
    end interface
    
    !/**
        !* \brief Destroys a stream
        !*
        !* Destroys the stream specified by \p hStream.  
        !*
        !*! In case the device is still doing work in the stream \p hStream
        !* when ::cuStreamDestroy() is called, the function will return immediately 
        !* and the resources associated with \p hStream will be released automatically 
        !* once the device has completed all work in \p hStream.
        !*
        !* \param hStream - Stream to destroy
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
        !* \sa ::cuStreamCreate,
        !* ::cuStreamWaitEvent,
        !* ::cuStreamQuery,
        !* ::cuStreamSynchronize,
        !* ::cuStreamAddCallback
        !*/
    
    interface
        function cuStreamDestroy(hStream) result(status) &
                    bind(c,name='cuStreamDestroy')
                  import :: CUstream
                  type(CUstream),      value :: hStream
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuStreamDestroy
    end interface
    
    !/**
        !* \brief Creates an event
        !*
        !* Creates an event *phEvent with the flags specified via \p Flags.
        !* \param phEvent - Returns newly created event
        !* \param Flags   - Event creation flags
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_OUT_OF_MEMORY
       ! * \notefnerr
        !*
        !* \sa
        !* ::cuEventRecord,
        !* ::cuEventQuery,
        !* ::cuEventSynchronize,
        !* ::cuEventDestroy,
        !* ::cuEventElapsedTime
        !*/
    
    interface
        function cuEventCreate(phEvent,Flags) result(status) &
                    bind(c,name='cuEventCreate')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUevent
                 import :: CUDA_SUCCESS
                 type(CUevent),     intent(inout)       :: phEvent
                 integer(c_int),    intent(in), value   :: Flags
                 integer(kind(CUDA_SUCCESS)) ::  status
        end function cuEventCreate
    end interface
    
    !/**
        !* \brief Records an event
        !*
        !* Records an event. See note on NULL stream behavior. Since operation is
        !* asynchronous, ::cuEventQuery or ::cuEventSynchronize() must be used
        !* to determine when the event has actually been recorded.
        !* \param hEvent  - Event to record
        !* \param hStream - Stream to record event for
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_HANDLE,
        !* ::CUDA_ERROR_INVALID_VALUE
        !* \note_null_stream
        !* \notefnerr
        !*
        !* \sa ::cuEventCreate,
        !* ::cuEventQuery,
        !* ::cuEventSynchronize,
        !* ::cuStreamWaitEvent,
        !* ::cuEventDestroy,
        !* ::cuEventElapsedTime
        !*/
    
    interface
        function cuEventRecord(hEvent,hStream) result(status) &
                    bind(c,name='cuEventRecord')
                    import :: CUevent
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(CUevent),      value :: hEvent
                    type(CUstream),     value :: hStream
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuEventRecord
    end interface
    
    !/**
        !* \brief Queries an event's status
        !*
        !* Query the status of all device work preceding the most recent
        !* call to ::cuEventRecord() (in the appropriate compute streams,
        !!*
        !* \param hEvent - Event to query
       ! *
        !* \return
       ! * ::CUDA_SUCCESS,
       !! * ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_HANDLE,
       ! * ::CUDA_ERROR_INVALID_VALUE,
       !! * ::CUDA_ERROR_NOT_READY
       ! * \notefnerr
        !*
       !! * ::cuEventRecord,
       ! * ::cuEventSynchronize,
       ! * ::cuEventDestroy,
       ! * ::cuEventElapsedTime
    !*/
    
    interface
        function cuEventQuery(hEvent) result(status) &
                    bind(c,name='cuEventQuery')
                   import :: CUevent
                   import :: CUDA_SUCCESS
                   type(CUevent),        value :: hEvent
                   integer(kind(CUDA_SUCCESS)) :: status
        end function cuEventQuery
    end interface
    
    !**
        !* \brief Waits for an event to complete
        !*
        !* Wait until the completion of all device work preceding the most recent
        !* call to ::cuEventRecord() (in the appropriate compute streams, as specified
        !* by the arguments to ::cuEventRecord()).
        !*
        !* \param hEvent - Event to wait for
        !*
        !* \return
        !    * ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_HANDLE
        !* \notefnerr
       ! *
       ! * \sa ::cuEventCreate,
        !* ::cuEventRecord,
       ! * ::cuEventQuery,
        !* ::cuEventDestroy,
        !* ::cuEventElapsedTime
       ! */
    
    interface
        function cuEventSynchronize(hEvent) result(status) &
                    bind(c,name='cuEventSynchronize')
                 import :: CUevent
                 import :: CUDA_SUCCESS
                 type(CUevent),        value :: hEvent
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cuEventSynchronize
    end interface
    
    !/**
        !* \brief Destroys an event
        !*
        !* Destroys the event specified by \p hEvent.
        !*
        !* In case \p hEvent has been recorded but has not yet been completed
        !* when ::cuEventDestroy() is called, the function will return immediately and 
        !* the resources associated with \p hEvent will be released automatically once
        !* the device has completed \p hEvent.
        !*
        !* \param hEvent - Event to destroy
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_HANDLE
        !* \notefnerr
       !! * \sa ::cuEventCreate,
        !* ::cuEventRecord,
       ! * ::cuEventQuery,
       !     * ::cuEventSynchronize,
       ! * ::cuEventElapsedTime
    !*/
    
    interface
        function cuEventDestroy(hEvent) result(status) &
                    bind(c,name='cuEventDestroy')
                 import :: CUevent
                 import :: CUDA_SUCCESS
                 type(CUevent),      value :: hEvent
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cuEventDestroy
    end interface
    
    !/**
        !* \brief Computes the elapsed time between two events
        !*
        !* Computes the elapsed time between two events (in milliseconds with a
        !* resolution of around 0.5 microseconds).
        !*
        !* \param pMilliseconds - Time between \p hStart and \p hEnd in ms
        !* \param hStart        - Starting event
        !* \param hEnd          - Ending event
       ! *
       ! * \return
        !* ::CUDA_SUCCESS,
        !    * ::CUDA_ERROR_DEINITIALIZED,
       !!* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_HANDLE,
       ! * ::CUDA_ERROR_NOT_READY
       ! * \notefnerr
       ! *
       ! * \sa ::cuEventCreate,
      !  * ::cuEventRecord,
       ! * ::cuEventQuery,
      !!  * ::cuEventSynchronize,
      !  * ::cuEventDestroy
      !  */
    
    interface
        function cuEventElapsedTime(pMiliseconds,hStart,hEnd) result(status) &
                        bind(c,name='cuEventElapsedTime')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUevent
                    import :: CUDA_SUCCESS
                    real(c_float),      intent(inout) :: pMiliseconds
                    type(CUevent),      intent(in), value :: hStart
                    type(CUevent),      intent(in), value :: hEnd
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuEventElapsedTime
    end interface
    
    !/**
       ! * \brief Wait on a memory location
       ! *
       ! * Enqueues a synchronization of the stream on the given memory location. Work
       ! * ordered after the operation will block until the given condition on the
       ! * memory is satisfied.
       ! * \param stream The stream to synchronize on the memory location.
       ! * \param addr The memory location to wait on.
       ! * \param value The value to compare with the memory location.
       ! * \param flags See ::CUstreamWaitValue_flags.
       ! *
       ! * \return
       ! * ::CUDA_SUCCESS,
       !! * ::CUDA_ERROR_INVALID_VALUE,
       ! * ::CUDA_ERROR_NOT_SUPPORTED
    !* \notefnerr
       ! *
       ! * \sa ::cuStreamWriteValue32,
       ! * ::cuStreamBatchMemOp,
        !* ::cuMemHostRegister,
       ! * ::cuStreamWaitEvent
        !*/
    
    interface
        function cuStreamWaitValue32(stream,addr,val,flags) result(status) &
                    bind(c,name='cuStreamWaitValue32')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUstream
                  import :: CUDA_SUCCESS
                  type(CUstream),           value :: stream
                  integer(c_size_t),        value :: addr
                  integer(c_int),           value :: val
                  integer(c_int),           value :: flags
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuStreamWaitValue32
    end interface
    
    !/**
        !* \brief Write a value to memory
        !    *
        !* Write a value to memory. Unless the ::CU_STREAM_WRITE_VALUE_NO_MEMORY_BARRIER
        !* flag is passed, the write is preceded by a system-wide memory fence,
        !* equivalent to a __threadfence_system() but scoped to the stream
        !* rather than a CUDA thread
        !* \param stream The stream to do the write in.
        !* \param addr The device address to write to.
        !* \param value The value to write.
        !* \param flags See ::CUstreamWriteValue_flags.
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_NOT_SUPPORTED
        !* \notefnerr
        !*
        !* \sa ::cuStreamWaitValue32,
        !* ::cuStreamBatchMemOp,
        !* ::cuMemHostRegister,
        !* ::cuEventRecord
        !*/
    
    interface
        function cuStreamWriteValue32(stream,addr,val,flags) result(status) &
                    bind(c,name='cuStreamWriteValue32')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUstream
                  import :: CUDA_SUCCESS
                  type(CUstream),           value :: stream
                  integer(c_size_t),        value :: addr
                  integer(c_int),           value :: val
                  integer(c_int),           value :: flags
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuStreamWriteValue32
    end interface
    
     !/**
        !* \brief Returns information about a function
        !*
        !!* Returns in \p *pi the integer value of the attribute \p attrib on the kernel
       ! * given by \p hfunc
        !* \param pi     - Returned attribute value
       ! * \param attrib - Attribute requested
       ! * \param hfunc  - Function to query attribute of
       ! *
       ! * \return
       ! * ::CUDA_SUCCESS,
       ! * ::CUDA_ERROR_DEINITIALIZED,
       ! * ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_CONTEXT,
       ! * ::CUDA_ERROR_INVALID_HANDLE,
       ! * ::CUDA_ERROR_INVALID_VALUE
       ! * \notefnerr
       ! *
       !! * ::cuCtxSetCacheConfig,
       ! * ::cuFuncSetCacheConfig,
       ! * ::cuLaunchKernel
       ! */
    
    interface
        function cuFuncGetAttribute(pi,attrib,hfunc) result(status) &
                    bind(c,name='cuFuncGetAttribute')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUfunction
                  import :: CUDA_SUCCESS
                  integer(c_int),       intent(inout)      :: pi
                  integer(c_int),       intent(in),  value :: attrib
                  type(CUfunction),     intent(in),  value :: hfunc
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuFuncGetAttribute
    end interface
    
    !/**
        !* \brief Sets the preferred cache configuration for a device function
        !*
        !* \param hfunc  - Kernel to configure cache for
        !* \param config - Requested cache configuration
        !*
       ! * \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_DEINITIALIZED,
       ! * ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_CONTEXT
       ! * \notefnerr
       !  *
       !  * \sa ::cuCtxGetCacheConfig,
       ! * ::cuCtxSetCacheConfig,
       ! * ::cuFuncGetAttribute,
       ! * ::cuLaunchKernel
       ! */
    
    interface
        function cuFuncSetCacheConfig(hfunc,config) result(status) &
                     bind(c,name='cuFuncSetCacheConfig')
                   use, intrinsic :: ISO_C_BINDING
                   import :: CUfunction
                   import :: CUDA_SUCCESS
                   type(CUfunction),     value :: hfunc
                   integer(c_int),       value :: config
                   integer(kind(CUDA_SUCCESS)) :: status
        end function cuFuncSetCacheConfig
    end interface
    
     !/**
        !* \brief Returns occupancy of a function
        !*
        !* Returns in \p *numBlocks the number of the maximum active blocks per
        !* streaming multiprocessor.
        !*
        !* \param numBlocks       - Returned occupancy
        !* \param func            - Kernel for which occupancy is calculated
        !* \param blockSize       - Block size the kernel is intended to be launched with
        !* \param dynamicSMemSize - Per-block dynamic shared memory usage intended, in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
        !* ::CUDA_ERROR_UNKNOWN
        !* \notefnerr
        !*
   ! */
    
    interface
        function   cuOccupancyMaxActiveBlocksPerMultiprocessor(numBlocks,func,blockSize,dynamicSMemSize) result(status) &
                        bind(c,name='cuOccupancyMaxActiveBlocksPerMultiprocessor')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUfunction
                    import :: CUDA_SUCCESS
                    integer(c_int),     intent(inout) :: numBlocks
                    type(CUfunction),   intent(in), value :: func
                    integer(c_int),     intent(in), value :: blockSize
                    integer(c_size_t),  intent(in), value :: dynamicSMemSize
                    integer(kind(CUDA_SUCCESS)) :: status
        end function  cuOccupancyMaxActiveBlocksPerMultiprocessor
    end interface
    
    !/**
        !* \brief Returns occupancy of a function
        !*
        !* Returns in \p *numBlocks the number of the maximum active blocks per
        !* streaming multiprocessor.
        !*
        !* \param numBlocks       - Returned occupancy
        !* \param func            - Kernel for which occupancy is calculated
        !* \param blockSize       - Block size the kernel is intended to be launched with
        !!* \param dynamicSMemSize - Per-block dynamic shared memory usage intended, in bytes
        !* \param flags           - Requested behavior for the occupancy calculator
       ! *
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE,
       ! * ::CUDA_ERROR_UNKNOWN
        !* \notefnerr
        !*
    !*/
    
    interface
        function  cuOccupancyMaxActiveBlocksPerMultiprocessorWithFlags(numBlocks,func,blockSize,dynamicSMemSize,flags) result(status) &
                        bind(c,name='cuOccupancyMaxActiveBlocksPerMultiprocessorWithFlags' )
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUfunction
                    import :: CUDA_SUCCESS
                    integer(c_int),     intent(inout) :: numBlocks
                    type(CUfunction),   intent(in), value :: func
                    integer(c_int),     intent(in), value :: blockSize
                    integer(c_size_t),  intent(in), value :: dynamicSMemSize
                    integer(c_int),     intent(in), value :: flags
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuOccupancyMaxActiveBlocksPerMultiprocessorWithFlags
    end interface
    
    !/**
        !* \brief Binds an array as a texture reference
        !*
        !* Binds the CUDA array \p hArray to the texture reference \p hTexRef. Any
        !* previous address or CUDA array state associated with the texture reference
        !* is superseded by this function. \p Flags must be set to
        !* ::CU_TRSA_OVERRIDE_FORMAT. Any CUDA array previously bound to \p hTexRef is
        !* unbound.
        !*
        !* \param hTexRef - Texture reference to bind
        !!* \param Flags   - Options (must be ::CU_TRSA_OVERRIDE_FORMAT)
       ! *
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !*
        !* \sa ::cuTexRefSetAddress,
        !* ::cuTexRefSetAddress2D, ::cuTexRefSetAddressMode,
        !* ::cuTexRefSetFilterMode, ::cuTexRefSetFlags, ::cuTexRefSetFormat,
        !* ::cuTexRefGetAddress, ::cuTexRefGetAddressMode, ::cuTexRefGetArray,
        !* ::cuTexRefGetFilterMode, ::cuTexRefGetFlags, ::cuTexRefGetFormat
 !*/
    
    interface
        function cuTexRefSetArray(hTexRef,hArray,Flags) result(status) &
                        bind(c,name='cuTexRefSetArray')
                     use, intrinsic :: ISO_C_BINDING
                     import :: CUtexref
                     import :: CUArray_st
                     import :: CUDA_SUCCESS
                     type(CUtexref),           value :: hTexRef
                     type(CUArray_st),         value :: hArray
                     integer(c_int),           value :: Flags
                     integer(kind(CUDA_SUCCESS)) :: status
        end function cuTexRefSetArray
    end interface
    
    !/**
        !* \brief Binds an address as a texture reference
        !*
        !* Binds a linear address range to the texture reference \p hTexRef. Any
        !* previous address or CUDA array state associated with the texture reference
        !* is superseded by this function. Any memory previously bound to \p hTexRef
        !* is unbound.
        !* \param ByteOffset - Returned byte offset
        !!* \param dptr       - Device pointer to bind
        !* \param bytes      - Size of memory to bind in bytes
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_INVALID_VALUE
        !*
        !* \sa ::cuTexRefSetAddress2D, ::cuTexRefSetAddressMode, ::cuTexRefSetArray,
        !* ::cuTexRefSetFilterMode, ::cuTexRefSetFlags, ::cuTexRefSetFormat,
        !* ::cuTexRefGetAddress, ::cuTexRefGetAddressMode, ::cuTexRefGetArray,
        !* ::cuTexRefGetFilterMode, ::cuTexRefGetFlags, ::cuTexRefGetFormat
    !*/
    
    interface
        function cuTexRefSetAddress(ByteOffset,hTexRef,dptr,bytes) result(status) &
                    bind(c,name='cuTexRefSetAddress')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUtexref
                  import :: CUDA_SUCCESS
                  integer(c_size_t),    intent(inout)     :: ByteOffset
                  type(CUtexref),       intent(in), value :: hTexRef
                  integer(c_size_t),    intent(in), value :: dptr
                  integer(c_size_t),    intent(in), value :: bytes
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cuTexRefSetAddress
    end interface
    
    !/**
        !* \brief Queries if a device may directly access a peer device's memory.
        !*
        !* Returns in \p *canAccessPeer a value of 1 if contexts on \p dev are capable of
        !* directly accessing memory from contexts on \p peerDev and 0 otherwise.
        !* If direct access of \p peerDev from \p dev is possible, then access may be
        !* enabled on two specific contexts by calling ::cuCtxEnablePeerAccess().
        !*
        !* \param canAccessPeer - Returned access capability
        !* \param dev           - Device from which allocations on \p peerDev are to
        !*                        be directly accessed.
        !* \param peerDev       - Device on which the allocations to be directly accessed 
       ! *                        by \p dev reside.
       ! *
       ! * \return
       ! * ::CUDA_SUCCESS,
       !! * ::CUDA_ERROR_DEINITIALIZED,
       ! * ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_INVALID_DEVICE
       ! * \notefnerr
        !*
        !* \sa ::cuCtxEnablePeerAccess,
       ! * ::cuCtxDisablePeerAccess
    !*/
    
    interface
        function cuDeviceCanAccessPeer(canAccessPeer,dev,peerDev) result(status) &
                    bind(c,name='cuDeviceCanAccessPeer')
                 use, intrinsic :: ISO_C_BINDING
                 import ::  CUDA_SUCCESS
                 integer(c_int),    intent(inout) :: canAccessPeer
                 integer(c_int),    intent(in), value :: dev
                 integer(c_int),    intent(in), value ::peerDev
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cuDeviceCanAccessPeer
    end interface
    
    !/**
        !* \brief Queries attributes of the link between two devices.
        !*
        !* Returns in \p *value the value of the requested attribute \p attrib of the
        !* link between \p srcDevice and \p dstDevice.
        !* \param value         - Returned value of the requested attribute
        !* \param attrib        - The requested attribute of the link between \p srcDevice and \p dstDevice.
        !* \param srcDevice     - The source device of the target link.
        !* \param dstDevice     - The destination device of the target link.
        !*
        !* \return
        !* ::CUDA_SUCCESS,
        !* ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
        !* ::CUDA_ERROR_INVALID_DEVICE,
        !* ::CUDA_ERROR_INVALID_VALUE
       ! * \notefnerr
       ! *
        !* \sa ::cuCtxEnablePeerAccess,
       ! * ::cuCtxDisablePeerAccess,
       ! * ::cuCtxCanAccessPeer
    !*/
    
    interface
        function  cuDeviceGetP2PAttribute(val,attrib,srcDevice,dstDevice) result(status) &
                    bind(c,name='cuDeviceGetP2PAttribute' )
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUDA_SUCCESS
                 integer(c_int),        intent(inout) :: val
                 integer(c_int),        intent(in), value :: attrib
                 integer(c_int),        intent(in), value :: srcDevice
                 integer(c_int),        intent(in), value :: dstDevice
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cuDeviceGetP2PAttribute
    end interface
    
    !/**
        !* \brief Enables direct access to memory allocations in a peer context.
        !*
        !* \param peerContext - Peer context to enable direct access to from the current context
        !* \param Flags       - Reserved for future use and must be set to 0
        !*
        !* \return
        !* ::CUDA_SUCCESS,
       ! * ::CUDA_ERROR_DEINITIALIZED,
        !* ::CUDA_ERROR_NOT_INITIALIZED,
       ! * ::CUDA_ERROR_PEER_ACCESS_ALREADY_ENABLED,
       !     * ::CUDA_ERROR_TOO_MANY_PEERS,
       ! * ::CUDA_ERROR_INVALID_CONTEXT,
        !* ::CUDA_ERROR_PEER_ACCESS_UNSUPPORTED,
      ! ! * ::CUDA_ERROR_INVALID_VALUE
        !* \notefnerr
        !*
       ! * \sa ::cuDeviceCanAccessPeer,
       ! * ::cuCtxDisablePeerAccess
    !*/
    
    interface
        function  cuCtxEnablePeerAccess(peerContext,Flags) result(status) &
                        bind(c,name='cuCtxEnablePeerAccess')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUctx_st
                    import :: CUDA_SUCCESS
                    type(CUctx_st),       value :: peerContext
                    integer(c_int),       value :: Flags
                    integer(kind(CUDA_SUCCESS)) :: status
        end function cuCtxEnablePeerAccess
    end interface
    
    
        
    
end module mod_cuda_header