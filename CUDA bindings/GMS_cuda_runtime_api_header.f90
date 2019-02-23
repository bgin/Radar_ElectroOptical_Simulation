
module mod_cuda_runtime_api_header

  

 !===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         'mod_cuda_runtime_api_header'
 !          
 !          Purpose:
 !                    
 !                         !======================================================!
 !                         ! Fortran wrappers to 'cuda_runtime_api.' header file  !
 !                         !======================================================!
 !                     
 !          History:
 !                        Date: 10-02-2018
 !                        Time: 11:03 GMT+2
 !
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  Bernard Gingold
 !                 
 !          
 !         
 !          
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
 !==================================================================================85

 ! Tab:5 col - Type and etc.. definitions
 ! Tab:10,11 col - Type , function and subroutine code blocks.

    use, intrinsic :: ISO_C_BINDING
    use mod_cuda_types
    use mod_cuda_header
    implicit none
    
#define cudaHostAllocDefault        0 ! /**< Default page-locked allocation flag */
#define cudaHostAllocPortable       1 ! /**< Pinned memory accessible by all CUDA contexts */
#define cudaHostAllocMapped         2 ! /**< Map allocation into device space */
#define cudaHostAllocWriteCombined  4 ! /**< Write-combined memory */

#define cudaHostRegisterDefault     0 ! /**< Default host memory registration flag */
#define cudaHostRegisterPortable    1 ! /**< Pinned memory accessible by all CUDA contexts */
#define cudaHostRegisterMapped      2 ! /**< Map registered memory into device space */
#define cudaHostRegisterIOMemory    4 !  /**< Memory-mapped I/O space */

#define cudaPeerAccessDefault       0 ! /**< Default peer addressing enable flag */

#define cudaStreamDefault           0 ! /**< Default stream flag */
#define cudaStreamNonBlocking       1 ! /**< Stream does not synchronize with stream 0 (the NULL stream) */

    !
    !    /**
    !       * Per-thread stream handle
    !       *
    !       * Stream handle that can be passed as a cudaStream_t to use an implicit stream
    !       * with per-thread synchronization behavior.
    !       *
    !       * See details of the \link_sync_behavior
    !   */
    !

#define cudaEventDefault                0 ! /**< Default event flag */
#define cudaEventBlockingSync           1 ! /**< Event uses blocking synchronization */
#define cudaEventDisableTiming          2 !  /**< Event will not record timing data */
#define cudaEventInterprocess           4 !   /**< Event is suitable for interprocess use. cudaEventDisableTiming must be set */

#define cudaDeviceScheduleAuto          0 !  /**< Device flag - Automatic scheduling */
#define cudaDeviceScheduleSpin          1 !  /**< Device flag - Spin default scheduling */
#define cudaDeviceScheduleYield         2 !  /**< Device flag - Yield default scheduling */
#define cudaDeviceScheduleBlockingSync  4 !  /**< Device flag - Use blocking synchronization */
#define cudaDeviceScheduleMask          7 !  /**< Device schedule flags mask */
#define cudaDeviceMapHost               8 !  /**< Device flag - Support mapped pinned allocations */
#define cudaDeviceLmemResizeToMax       10 ! /**< Device flag - Keep local memory allocation after launch */
#define cudaDeviceMask                  31 ! /**< Device flags mask */

#define cudaArrayDefault                0  !/**< Default CUDA array allocation flag */
#define cudaArrayLayered                1  !/**< Must be set in cudaMalloc3DArray to create a layered CUDA array */
#define cudaArraySurfaceLoadStore       2  !/**< Must be set in cudaMallocArray or cudaMalloc3DArray in order to bind surfaces to the CUDA array */
#define cudaArrayCubemap                4  !/**< Must be set in cudaMalloc3DArray to create a cubemap CUDA array */
#define cudaArrayTextureGather          8  !/**< Must be set in cudaMallocArray or cudaMalloc3DArray in order to perform texture gather operations on the CUDA array */

#define cudaIpcMemLazyEnablePeerAccess  1  !/**< Automatically enable peer access between remote devices as needed */

#define cudaMemAttachGlobal             1  !/**< Memory can be accessed by any stream on any device*/
#define cudaMemAttachHost               2  !/**< Memory cannot be accessed by any stream on any device */
#define cudaMemAttachSingle             4  !/**< Memory can only be accessed by a single stream on the associated device */

#define cudaOccupancyDefault            0  !/**< Default behavior */
#define cudaOccupancyDisableCachingOverride 1  !/**< Assume global caching is enabled and cannot be automatically turned off */

    !/**
        !* Channel format kind
    !*/

    ! enum cudaChannelFormatKind
    enum , bind(c)
       enumerator   :: cudaChannelFormatKindSigned   = 0    ! /**< Signed channel format */
       enumerator   :: cudaChannelFormatKindUnsigned = 1    ! /**< Unsigned channel format */
       enumerator   :: cudaChannelFormatKindFloat    = 2    !  /**< Float channel format */
       enumerator   :: cudaChannelFormatKindNone     = 3    !   /**< No channel format */
    end enum
    
    !  /**
        !* CUDA Channel format descriptor
    !   */
    
   
    
    !/**
        !* CUDA memory types
    !*/
    
    !  enum __device_builtin__ cudaMemoryType
    enum, bind(c)
       enumerator :: cudaMemoryTypeHost     = 1 ! /**< Host memory */
       enumerator :: cudaMemoryTypeDevice   = 2 !  /**< Device memory */
    end enum
    
    !/**
        !* CUDA memory copy types
    !*/
    
    !   enum __device_builtin__ cudaMemcpyKind
    enum, bind(c)
       enumerator :: cudaMemcpyHostToHost     = 0 !     /**< Host   -> Host */
       enumerator :: cudaMemcpyHostToDevice   = 1 !     /**< Host   -> Device */
       enumerator :: cudaMemcpyDeviceToHost   = 2 !     /**< Device -> Host */
       enumerator :: cudaMemcpyDeviceToDevice = 3 !     /**< Device -> Device */ 
       enumerator :: cudaMemcpyDefault        = 4 !     /**< Direction of the transfer is inferred from the pointer values. Requires unified virtual addressing */
    end enum
    
     !   enum __device_builtin__ cudaGraphicsRegisterFlags
    enum, bind(c)
        enumerator  :: cudaGraphicsRegisterFlagsNone             = 0 !  /**< Default */
        enumerator  :: cudaGraphicsRegisterFlagsReadOnly         = 1 !  /**< CUDA will not write to this resource */ 
        enumerator  :: cudaGraphicsRegisterFlagsWriteDiscard     = 2 !  /**< CUDA will only write to and will not read from this resource */
        enumerator  :: cudaGraphicsRegisterFlagsSurfaceLoadStore = 4 !  /**< CUDA will bind this resource to a surface reference */
        enumerator  :: cudaGraphicsRegisterFlagsTextureGather    = 8 !  /**< CUDA will perform texture gather operations on this resource */ 
    end enum
    
    !/**
        !* CUDA graphics interop map flags
    !*/
    
    ! enum  enum __device_builtin__ cudaGraphicsMapFlags
    enum, bind(c)
        enumerator :: cudaGraphicsMapFlagsNone         = 0  !  /**< Default; Assume resource can be read/written */
        enumerator :: cudaGraphicsMapFlagsReadOnly     = 1  !  /**< CUDA will not write to this resource */
        enumerator :: cudaGraphicsMapFlagsWriteDiscard = 2  ! /**< CUDA will only write to and will not read from this resource */ 
    end enum
    
    !/**
        !* CUDA resource types
    !*/
    
    ! enum   enum __device_builtin__ cudaResourceType
    enum, bind(c)
        enumerator :: cudaResourceTypeArray          = 0 ! /**< Array resource */
        enumerator :: cudaResourceTypeMipmappedArray = 1 ! /**< Mipmapped array resource */
        enumerator :: cudaResourceTypeLinear         = 2 ! /**< Linear resource */
        enumerator :: cudaResourceTypePitch2D        = 3 ! /**< Pitch 2D resource */
    end enum
    
    !/**
        !* CUDA function cache configurations
    !*/
    
    ! enum   __device_builtin__ cudaFuncCache
    enum, bind(c)
       enumerator :: cudaFuncCachePreferNone   = 0    !/**< Default function cache configuration, no preference */
       enumerator :: cudaFuncCachePreferShared = 1    !    /**< Prefer larger shared memory and smaller L1 cache  */
       enumerator :: cudaFuncCachePreferL1     = 2    !    /**< Prefer larger L1 cache and smaller shared memory */
       enumerator :: cudaFuncCachePreferEqual  = 3    ! /**< Prefer equal size L1 cache and shared memory */ 
    end enum 
    
    !/**
        !* CUDA shared memory configuration
    !*/
    
    ! enum enum __device_builtin__ cudaSharedMemConfig
    enum, bind(c)
       enumerator ::  cudaSharedMemBankSizeDefault   = 0
       enumerator ::  cudaSharedMemBankSizeFourByte  = 1
       enumerator ::  cudaSharedMemBankSizeEightByte = 2 
    end enum
    
    !/**
        !* CUDA device compute modes
    !*/
    
    !   enum __device_builtin__ cudaComputeMode
    enum, bind(c)
      enumerator :: cudaComputeModeDefault          = 0  !/**< Default compute mode (Multiple threads can use ::cudaSetDevice() with this device) */
      enumerator :: cudaComputeModeExclusive        = 1  !/**< Compute-exclusive-thread mode (Only one thread in one process will be able to use ::cudaSetDevice() with this device) */
      enumerator :: cudaComputeModeProhibited       = 2  !/**< Compute-prohibited mode (No threads can use ::cudaSetDevice() with this device) */
      enumerator :: cudaComputeModeExclusiveProcess = 3  ! /**< Compute-exclusive-process mode (Many threads in one process will be able to use ::cudaSetDevice() with this device) */  
    end enum
    
    !/**
        !* CUDA Limits
    !*/
    
    !  enum __device_builtin__ cudaLimit
    enum, bind(c)
        enumerator :: cudaLimitStackSize                    = 0 !/**< GPU thread stack size */
        enumerator :: cudaLimitPrintfFifoSize               = 1 ! /**< GPU printf/fprintf FIFO size */
        enumerator :: cudaLimitMallocHeapSize               = 2  !/**< GPU malloc heap size */
        enumerator :: cudaLimitDevRuntimeSyncDepth          = 3  !/**< GPU device runtime synchronize depth */
        enumerator :: cudaLimitDevRuntimePendingLaunchCount = 4  ! /**< GPU device runtime pending launch count */ 
    end enum
    
    !/**
        !* CUDA Memory Advise values
    !*/
    
    !  enum __device_builtin__ cudaMemoryAdvise
    enum, bind(c)
        enumerator :: cudaMemAdviseSetReadMostly          = 1 ! /**< Data will mostly be read and only occassionally be written to */
        enumerator :: cudaMemAdviseUnsetReadMostly        = 2 ! /**< Undo the effect of ::cudaMemAdviseSetReadMostly */
        enumerator :: cudaMemAdviseSetPreferredLocation   = 3 ! /**< Set the preferred location for the data as the specified device */
        enumerator :: cudaMemAdviseUnsetPreferredLocation = 4 ! /**< Clear the preferred location for the data */
        enumerator :: cudaMemAdviseSetAccessedBy          = 5 ! /**< Data will be accessed by the specified device, so prevent page faults as much as possible */
        enumerator :: cudaMemAdviseUnsetAccessedBy        = 6 ! /**< Let the Unified Memory subsystem decide on the page faulting policy for the specified device */ 
    end enum
    
    !/**
        !* CUDA range attributes
    !*/
    
    !  enum __device_builtin__ cudaMemRangeAttribute
    enum, bind(c)
        enumerator :: cudaMemRangeAttributeReadMostly           = 1  !, /**< Whether the range will mostly be read and only occassionally be written to */
        enumerator :: cudaMemRangeAttributePreferredLocation    = 2  !, /**< The preferred location of the range */
        enumerator :: cudaMemRangeAttributeAccessedBy           = 3  !, /**< Memory range has ::cudaMemAdviseSetAccessedBy set for specified device */
        enumerator :: cudaMemRangeAttributeLastPrefetchLocation = 4  !/**< The last location to which the range was prefetched */ 
    end enum
    
    !/**
        !* CUDA device attributes
    !*/
    
    !  enum __device_builtin__ cudaDeviceAttr
    enum, bind(c)
        enumerator :: cudaDevAttrMaxThreadsPerBlock             = 1 !,  /**< Maximum number of threads per block */
        enumerator :: cudaDevAttrMaxBlockDimX                   = 2 !,  /**< Maximum block dimension X */
        enumerator :: cudaDevAttrMaxBlockDimY                   = 3 !,  /**< Maximum block dimension Y */
        enumerator :: cudaDevAttrMaxBlockDimZ                   = 4 !,  /**< Maximum block dimension Z */
        enumerator :: cudaDevAttrMaxGridDimX                    = 5 !,  /**< Maximum grid dimension X */
        enumerator :: cudaDevAttrMaxGridDimY                    = 6 !,  /**< Maximum grid dimension Y */
        enumerator :: cudaDevAttrMaxGridDimZ                    = 7 !,  /**< Maximum grid dimension Z */
        enumerator :: cudaDevAttrMaxSharedMemoryPerBlock        = 8 !,  /**< Maximum shared memory available per block in bytes */
        enumerator :: cudaDevAttrTotalConstantMemory            = 9 !,  /**< Memory available on device for __constant__ variables in a CUDA C kernel in bytes */
        enumerator :: cudaDevAttrWarpSize                       = 10 !, /**< Warp size in threads */
        enumerator :: cudaDevAttrMaxPitch                       = 11 !, /**< Maximum pitch in bytes allowed by memory copies */
        enumerator :: cudaDevAttrMaxRegistersPerBlock           = 12 !, /**< Maximum number of 32-bit registers available per block */
        enumerator :: cudaDevAttrClockRate                      = 13 !, /**< Peak clock frequency in kilohertz */
        enumerator :: cudaDevAttrTextureAlignment               = 14 !, /**< Alignment requirement for textures */
        enumerator :: cudaDevAttrGpuOverlap                     = 15 !, /**< Device can possibly copy memory and execute a kernel concurrently */
        enumerator :: cudaDevAttrMultiProcessorCount            = 16 !, /**< Number of multiprocessors on device */
        enumerator :: cudaDevAttrKernelExecTimeout              = 17 !, /**< Specifies whether there is a run time limit on kernels */
        enumerator :: cudaDevAttrIntegrated                     = 18 !, /**< Device is integrated with host memory */
        enumerator :: cudaDevAttrCanMapHostMemory               = 19 !, /**< Device can map host memory into CUDA address space */
        enumerator :: cudaDevAttrComputeMode                    = 20 !, /**< Compute mode (See ::cudaComputeMode for details) */
        enumerator :: cudaDevAttrMaxTexture1DWidth              = 21    !, /**< Maximum 1D texture width */
        enumerator :: cudaDevAttrMaxTexture2DWidth              = 22 !, /**< Maximum 2D texture width */
        enumerator :: cudaDevAttrMaxTexture2DHeight             = 23 !, /**< Maximum 2D texture height */
        enumerator :: cudaDevAttrMaxTexture3DWidth              = 24 !, /**< Maximum 3D texture width */
        enumerator :: cudaDevAttrMaxTexture3DHeight             = 25 !, /**< Maximum 3D texture height */
        enumerator :: cudaDevAttrMaxTexture3DDepth              = 26 !, /**< Maximum 3D texture depth */
        enumerator :: cudaDevAttrMaxTexture2DLayeredWidth       = 27 !, /**< Maximum 2D layered texture width */
        enumerator :: cudaDevAttrMaxTexture2DLayeredHeight      = 28 !, /**< Maximum 2D layered texture height */
        enumerator :: cudaDevAttrMaxTexture2DLayeredLayers      = 29 !, /**< Maximum layers in a 2D layered texture */
        enumerator :: cudaDevAttrSurfaceAlignment               = 30 !, /**< Alignment requirement for surfaces */
        enumerator :: cudaDevAttrConcurrentKernels              = 31 !, /**< Device can possibly execute multiple kernels concurrently */
        enumerator :: cudaDevAttrEccEnabled                     = 32 !, /**< Device has ECC support enabled */
        enumerator :: cudaDevAttrPciBusId                       = 33!, /**< PCI bus ID of the device */
        enumerator :: cudaDevAttrPciDeviceId                    = 34!, /**< PCI device ID of the device */
        enumerator :: cudaDevAttrTccDriver                      = 35!, /**< Device is using TCC driver model */
        enumerator :: cudaDevAttrMemoryClockRate                = 36!, /**< Peak memory clock frequency in kilohertz */
        enumerator :: cudaDevAttrGlobalMemoryBusWidth           = 37!, /**< Global memory bus width in bits */
        enumerator :: cudaDevAttrL2CacheSize                    = 38!, /**< Size of L2 cache in bytes */
        enumerator :: cudaDevAttrMaxThreadsPerMultiProcessor    = 39!, /**< Maximum resident threads per multiprocessor */
        enumerator :: cudaDevAttrAsyncEngineCount               = 40!, /**< Number of asynchronous engines */
        enumerator :: cudaDevAttrUnifiedAddressing              = 41!, /**< Device shares a unified address space with the host */    
        enumerator :: cudaDevAttrMaxTexture1DLayeredWidth       = 42!, /**< Maximum 1D layered texture width */
        enumerator :: cudaDevAttrMaxTexture1DLayeredLayers      = 43!, /**< Maximum layers in a 1D layered texture */
        enumerator :: cudaDevAttrMaxTexture2DGatherWidth        = 45!, /**< Maximum 2D texture width if cudaArrayTextureGather is set */
        enumerator :: cudaDevAttrMaxTexture2DGatherHeight       = 46!, /**< Maximum 2D texture height if cudaArrayTextureGather is set */
        enumerator :: cudaDevAttrMaxTexture3DWidthAlt           = 47!, /**< Alternate maximum 3D texture width */
        enumerator :: cudaDevAttrMaxTexture3DHeightAlt          = 48!, /**< Alternate maximum 3D texture height */
        enumerator :: cudaDevAttrMaxTexture3DDepthAlt           = 49!, /**< Alternate maximum 3D texture depth */
        enumerator :: cudaDevAttrPciDomainId                    = 50!, /**< PCI domain ID of the device */
        enumerator :: cudaDevAttrTexturePitchAlignment          = 51!, /**< Pitch alignment requirement for textures */
        enumerator :: cudaDevAttrMaxTextureCubemapWidth         = 52!, /**< Maximum cubemap texture width/height */
        enumerator :: cudaDevAttrMaxTextureCubemapLayeredWidth  = 53! , /**< Maximum cubemap layered texture width/height */
        enumerator :: cudaDevAttrMaxTextureCubemapLayeredLayers = 54!, /**< Maximum layers in a cubemap layered texture */
        enumerator :: cudaDevAttrMaxSurface1DWidth              = 55!, /**< Maximum 1D surface width */
        enumerator :: cudaDevAttrMaxSurface2DWidth              = 56!, /**< Maximum 2D surface width */
        enumerator :: cudaDevAttrMaxSurface2DHeight             = 57!, /**< Maximum 2D surface height */
        enumerator :: cudaDevAttrMaxSurface3DWidth              = 58!, /**< Maximum 3D surface width */
        enumerator :: cudaDevAttrMaxSurface3DHeight             = 59 !, /**< Maximum 3D surface height */
        enumerator :: cudaDevAttrMaxSurface3DDepth              = 60!, /**< Maximum 3D surface depth */
        enumerator :: cudaDevAttrMaxSurface1DLayeredWidth       = 61!, /**< Maximum 1D layered surface width */
        enumerator :: cudaDevAttrMaxSurface1DLayeredLayers      = 62 !, /**< Maximum layers in a 1D layered surface */
        enumerator :: cudaDevAttrMaxSurface2DLayeredWidth       = 63 !, /**< Maximum 2D layered surface width */
        enumerator :: cudaDevAttrMaxSurface2DLayeredHeight      = 64 !, /**< Maximum 2D layered surface height */
        enumerator :: cudaDevAttrMaxSurface2DLayeredLayers      = 65 !, /**< Maximum layers in a 2D layered surface */
        enumerator :: cudaDevAttrMaxSurfaceCubemapWidth         = 66 !, /**< Maximum cubemap surface width */
        enumerator :: cudaDevAttrMaxSurfaceCubemapLayeredWidth  = 67!, /**< Maximum cubemap layered surface width */
        enumerator :: cudaDevAttrMaxSurfaceCubemapLayeredLayers = 68!, /**< Maximum layers in a cubemap layered surface */
        enumerator :: cudaDevAttrMaxTexture1DLinearWidth        = 69!, /**< Maximum 1D linear texture width */
        enumerator :: cudaDevAttrMaxTexture2DLinearWidth        = 70!, /**< Maximum 2D linear texture width */
        enumerator :: cudaDevAttrMaxTexture2DLinearHeight       = 71!, /**< Maximum 2D linear texture height */
        enumerator :: cudaDevAttrMaxTexture2DLinearPitch        = 72!, /**< Maximum 2D linear texture pitch in bytes */
        enumerator :: cudaDevAttrMaxTexture2DMipmappedWidth     = 73!, /**< Maximum mipmapped 2D texture width */
        enumerator :: cudaDevAttrMaxTexture2DMipmappedHeight    = 74!, /**< Maximum mipmapped 2D texture height */
        enumerator :: cudaDevAttrComputeCapabilityMajor         = 75!, /**< Major compute capability version number */ 
        enumerator :: cudaDevAttrComputeCapabilityMinor         = 76!, /**< Minor compute capability version number */
        enumerator :: cudaDevAttrMaxTexture1DMipmappedWidth     = 77!, /**< Maximum mipmapped 1D texture width */
        enumerator :: cudaDevAttrStreamPrioritiesSupported      = 78!, /**< Device supports stream priorities */
        enumerator :: cudaDevAttrGlobalL1CacheSupported         = 79!, /**< Device supports caching globals in L1 */
        enumerator :: cudaDevAttrLocalL1CacheSupported          = 80!, /**< Device supports caching locals in L1 */
        enumerator :: cudaDevAttrMaxSharedMemoryPerMultiprocessor = 81!, /**< Maximum shared memory available per multiprocessor in bytes */
        enumerator :: cudaDevAttrMaxRegistersPerMultiprocessor  = 82    !, /**< Maximum number of 32-bit registers available per multiprocessor */
        enumerator :: cudaDevAttrManagedMemory                  = 83 !, /**< Device can allocate managed memory on this system */
        enumerator :: cudaDevAttrIsMultiGpuBoard                = 84 !, /**< Device is on a multi-GPU board */
        enumerator :: cudaDevAttrMultiGpuBoardGroupID           = 85 !, /**< Unique identifier for a group of devices on the same multi-GPU board */
        enumerator :: cudaDevAttrHostNativeAtomicSupported      = 86 !, /**< Link between the device and the host supports native atomic operations */
        enumerator :: cudaDevAttrSingleToDoublePrecisionPerfRatio = 87 !, /**< Ratio of single precision performance (in floating-point operations per second) to double precision performance */
        enumerator :: cudaDevAttrPageableMemoryAccess           = 88 !, /**< Device supports coherently accessing pageable memory without calling cudaHostRegister on it */
        enumerator :: cudaDevAttrConcurrentManagedAccess        = 89 !, /**< Device can coherently access managed memory concurrently with the CPU */
        enumerator :: cudaDevAttrComputePreemptionSupported     = 90 !, /**< Device supports Compute Preemption */
        enumerator :: cudaDevAttrCanUseHostPointerForRegisteredMem = 91 !/**< Device can access host registered memory at the same virtual address as the CPU */  
    end enum
    
    !/**
        !* CUDA device P2P attributes
   ! */
    
    ! enum __device_builtin__ cudaDeviceP2PAttr
    enum, bind(c)
        enumerator :: cudaDevP2PAttrPerformanceRank              = 1 !, /**< A relative value indicating the performance of the link between two devices */
        enumerator :: cudaDevP2PAttrAccessSupported              = 2 !, /**< Peer access is enabled */
        enumerator :: cudaDevP2PAttrNativeAtomicSupported        = 3 ! /**< Native atomic operation over the link supported */
    end enum
    
     !/**
        !* CUDA device properties
    !*/
    
    !   struct __device_builtin__ cudaDeviceProp
    type, bind(c), public :: cudaDeviceProp
          character(c_char), dimension(256) :: name                 !  /**< ASCII string identifying device */
          integer(c_size_t)                 :: totalGlobalMem       !/**< Global memory available on device in bytes */
          integer(c_size_t)                 :: sharedMemPerBlock    !     /**< Shared memory available per block in bytes */
          integer(c_int)                    :: regsPerBlock         !  /**< 32-bit registers available per block */
          integer(c_int)                    :: warpSize             !  /**< Warp size in threads */
          integer(c_size_t)                 :: memPitch             !     /**< Maximum pitch in bytes allowed by memory copies */
          integer(c_int)                    :: maxThreadsPerBlock   !  /**< Maximum number of threads per block */
          integer(c_int), dimension(3)      :: maxThreadsDim        !  /**< Maximum size of each dimension of a block */
          integer(c_int), dimension(3)      :: maxGridSize          !  /**< Maximum size of each dimension of a grid */
          integer(c_int)                    :: clockRate            !   /**< Clock frequency in kilohertz */
          integer(c_size_t)                 :: totalConstMem        !    /**< Constant memory available on device in bytes */
          integer(c_int)                    :: major                !    /**< Major compute capability */
          integer(c_int)                    :: minor                !   /**< Minor compute capability */
          integer(c_size_t)                 :: textureAlignment     !    /**< Alignment requirement for textures */
          integer(c_size_t)                 :: texturePitchAlignment !   /**< Pitch alignment requirement for texture references bound to pitched memory */
          integer(c_int)                    :: deviceOverlap         !  /**< Device can concurrently copy memory and execute a kernel. Deprecated. Use instead asyncEngineCount. */
          integer(c_int)                    :: multiProcessorCount   !    /**< Number of multiprocessors on device */
          integer(c_int)                    :: kernelExecTimeoutEnabled  !/**< Specified whether there is a run time limit on kernels */
          integer(c_int)                    :: integrated               ! /**< Device is integrated as opposed to discrete */
          integer(c_int)                    :: canMapHostMemory         ! /**< Device can map host memory with cudaHostAlloc/cudaHostGetDevicePointer */
          integer(c_int)                    :: computeMode              !  /**< Compute mode (See ::cudaComputeMode) */
          integer(c_int)                    :: maxTexture1D             !   /**< Maximum 1D texture size */
          integer(c_int)                    :: maxTexture1DMipmap       !    /**< Maximum 1D mipmapped texture size */
          integer(c_int)                    :: maxTexture1DLinear
          integer(c_int), dimension(2)      :: maxTexture2D
          integer(c_int), dimension(2)      :: maxTexture2DMipmap
          integer(c_int), dimension(3)      :: maxTexture2DLinear
          integer(c_int), dimension(2)      :: maxTexture2DGather
          integer(c_int), dimension(3)      :: maxTexture3D
          integer(c_int), dimension(3)      :: maxTexture3DAlt
          integer(c_int)                    :: maxTextureCubemap
          integer(c_int), dimension(2)      :: maxTexture1DLayered
          integer(c_int), dimension(3)      :: maxTexture2DLayered
          integer(c_int), dimension(2)      :: maxTextureCubemapLayered
          integer(c_int)                    :: maxSurface1D
          integer(c_int), dimension(2)      :: maxSurface2D
          integer(c_int), dimension(3)      :: maxSurface3D
          integer(c_int), dimension(2)      :: maxSurface1DLayered
          integer(c_int), dimension(3)      :: maxSurface2DLayered
          integer(c_int)                    :: maxSurfaceCubemap
          integer(c_int), dimension(2)      :: maxSurfaceCubemapLayered
          integer(c_size_t)                 :: surfaceAlignment
          integer(c_int)                    :: concurrentKernels
          integer(c_int)                    :: ECCEnabled
          integer(c_int)                    :: pciBusID
          integer(c_int)                    :: pciDeviceID
          integer(c_int)                    :: pciDomainID
          integer(c_int)                    :: tccDriver
          integer(c_int)                    :: asyncEngineCount
          integer(c_int)                    :: unifiedAddressing
          integer(c_int)                    :: memoryClockRate
          integer(c_int)                    :: memoryBusWidth
          integer(c_int)                    :: l2CacheSize
          integer(c_int)                    :: maxThreadsPerMultiProcessor
          integer(c_int)                    :: streamPrioritySupported
          integer(c_int)                    :: globalL1CacheSupported
          integer(c_int)                    :: localL1CacheSupported
          integer(c_size_t)                 :: sharedMemPerMultiprocessor
          integer(c_int)                    :: regPerMultiProcessor
          integer(c_int)                    :: managedMemory
          integer(c_int)                    :: isMultiGpuBoard
          integer(c_int)                    :: multiGpuBoardGroupID
          integer(c_int)                    :: hostNativeAtomicSupported
          integer(c_int)                    :: singleToDoublePrecisionPerfRatio
          integer(c_int)                    :: pageableMemoryAccess
          integer(c_int)                    :: concurrentManagedAccess
    end type cudaDeviceProp
    
    !/**
        !* CUDA IPC event handle
    !*/
    
    !   struct __device_builtin__ cudaIpcEventHandle_st
    type, bind(c), public :: cudaIpcEventHandle_st
          character(c_char), dimension(64) :: reserved
    end type cudaIpcEventHandle_st
    
    !/**
        !* CUDA IPC memory handle
    !*/
    
    !   struct __device_builtin__ cudaIpcMemHandle_st 
    type, bind(c), public :: cudaIpcMemHandle_st
          character(c_char), dimension(64) :: reserved
    end type cudaIpcMemHandle_st
    
    !   struct __device_builtin__ char1
    type, bind(c), public :: char1
          character(c_signed_char) :: x
    end type char1
    
    ! struct __device_builtin__ __align__(2) char2
    type, bind(c), public :: char2
          character(c_signed_char) :: x
          character(c_signed_char) :: y
    end type char2
    
    !  struct __device_builtin__ char3
    type, bind(c), public :: char3
          character(c_signed_char) :: x
          character(c_signed_char) :: y
          character(c_signed_char) :: z
    end type char3
    
    !  struct __device_builtin__ int1
    type, bind(c), public :: int1
          integer(c_int) :: x
    end type int1
    
    !  struct __device_builtin__ int3
    type, bind(c), public :: int3
          integer(c_int)  :: x
          integer(c_int)  :: y
          integer(c_int)  :: z
    end type int3
    
    ! struct __device_builtin__ __builtin_align__(16) int4
    type, bind(c), public :: int4
          integer(c_int)  :: x
          integer(c_int)  :: y
          integer(c_int)  :: z
          integer(c_int)  :: w
    end type int4
    
    !  struct __device_builtin__ longlong1
    type, bind(c), public :: longlong1
          integer(c_int64_t) :: x
    end type longlong1
    
    !   struct __device_builtin__ __builtin_align__(16) longlong2
    type, bind(c), public :: longlong2
          integer(c_int64_t) :: x
          integer(c_int64_t) :: y
    end type longlong2
    
    !       struct __device_builtin__ longlong3
    type, bind(c), public :: longlong3
          integer(c_int64_t) :: x
          integer(c_int64_t) :: y
          integer(c_int64_t) :: z
    end type longlong3
    
    !  struct __device_builtin__ __builtin_align__(16) longlong4
    type, bind(c), public :: longlong4
          integer(c_int64_t) :: x
          integer(c_int64_t) :: y
          integer(c_int64_t) :: z
          integer(c_int64_t) :: w
    end type longlong4
    
    !    struct __device_builtin__ float1
    type, bind(c), public :: float1
          real(c_float)   :: x
    end type float1
    
    !    struct __device_builtin__ float3
    type, bind(c), public :: float3
          real(c_float)   :: x
          real(c_float)   :: y
          real(c_float)   :: z
    end type float3
    
    !    struct __device_builtin__ __builtin_align__(16) float4
    type, bind(c), public :: float4
          real(c_float)   :: x
          real(c_float)   :: y
          real(c_float)   :: z
          real(c_float)   :: w
    end type float4
    
    
    
    ! struct __device_builtin__ double1
    type, bind(c), public :: double1
          real(c_double)  :: x
    end type double1
    
    ! struct __device_builtin__ double2
    type, bind(c), public :: double2
          real(c_double)  :: x
          real(c_double)  :: y
    end type double2
    
    ! struct __device_builtin__ double3
    type, bind(c), public :: double3
          real(c_double)  :: x
          real(c_double)  :: y
          real(c_double)  :: z
    end type double3
    
    !  struct __device_builtin__ double4
    type, bind(c), public :: double4
          real(c_double)  :: x
          real(c_double)  :: y
          real(c_double)  :: z
          real(c_double)  :: w
    end type double4
    
    ! struct __device_builtin__ dim3
    type, bind(c), public :: dim3
          integer(c_int)  :: x
          integer(c_int)  :: y
          integer(c_int)  :: z
    end type dim3
    
    !/**
        !* CUDA Pitched memory pointer
        !*
        !* \sa ::make_cudaPitchedPtr
    !*/
    
     !  struct __device_builtin__ cudaChannelFormatDesc
    type, bind(c), public :: cudaChannelFormatDesc
          integer(c_int)  :: x
          integer(c_int)  :: y
          integer(c_int)  :: z
          integer(c_int)  :: w
          integer(kind(cudaChannelFormatKindSigned)) ::  f
    end type cudaChannelFormatDesc
    
    !  struct __device_builtin__ cudaPitchedPtr
    type, bind(c), public :: cudaPitchedPtr
       type(c_ptr)        :: ptr !  /**< Pointer to allocated memory */
       integer(c_size_t)  :: pitch  ! /**< Pitch of allocated memory in bytes */
       integer(c_size_t)  :: xsize  !  /**< Logical width of allocation in elements */
       integer(c_size_t)  :: ysize  !   /**< Logical height of allocation in elements */
    end type cudaPitchedPtr
    
    !/**
        !* CUDA extent
        !*
        !* \sa ::make_cudaExtent
    !*/
    
    !  struct __device_builtin__ cudaExtent
    type, bind(c), public :: cudaExtent
          integer(c_size_t)     :: width  !   /**< Width in elements when referring to array memory, in bytes when referring to linear memory */
          integer(c_size_t)     :: height !    /**< Height in elements */
          integer(c_size_t)     :: depth  !    /**< Depth in elements */  
    end type cudaExtent
    
    !/**
        !* CUDA 3D position
        !*
        !* \sa ::make_cudaPos
    !*/
    
    !   struct __device_builtin__ cudaPos
    type, bind(c), public :: cudaPos
          integer(c_size_t) :: x
          integer(c_size_t) :: y
          integer(c_size_t) :: z
    end type cudaPos
    
    
    
    !/**
        !* CUDA 3D memory copying parameters
    !*/
    
    ! struct __device_builtin__ cudaMemcpy3DParms
    type, bind(c), public :: cudaMemcpy3DParms
          type(c_ptr)                         :: srcArray  ! /**< Source memory address */
          type(cudaPos)                       :: srcPos    ! /**< Source position offset */
          type(cudaPitchedPtr)                :: srcPtr    !  /**< Pitched source memory address */
          type(c_ptr)                         :: dstArray  !  /**< Destination memory address */
          type(cudaPos)                       :: dstPos    !  /**< Destination position offset */
          type(cudaPitchedPtr)                :: dstPtr    !  /**< Pitched destination memory address */
          type(cudaExtent)                    :: extent    !   /**< Requested memory copy size */
          integer(kind(cudaMemcpyHostToHost)) :: kind      !    /**< Type of transfer */
    end type cudaMemcpy3DParms
    
    !/**
        !* CUDA 3D cross-device memory copying parameters
    !*/
    
    !   struct __device_builtin__ cudaMemcpy3DPeerParms
    type, bind(c), public :: cudaMemcpy3DPeerParms
          type(c_ptr)           :: srcArray !    /**< Source memory address */
          type(cudaPos)         :: srcPos   !    /**< Source position offset */
          type(cudaPitchedPtr)  :: srcPtr   !    /**< Pitched source memory address */
          integer(c_int)        :: srcDevice !   /**< Source device */
          type(c_ptr)           :: dstArray  !   /**< Destination memory address */
          type(cudaPos)         :: dstPos    !   /**< Destination position offset */
          type(cudaPitchedPtr)  :: dstPtr    !    /**< Pitched destination memory address */
          integer(c_int)        :: dstDevice !    /**< Destination device */
          type(cudaExtent)      :: extent    !    /**< Requested memory copy size */
    end type cudaMemcpy3DPeerParms
    
    !/**
        !* CUDA pointer attributes
    !*/
    
    ! struct __device_builtin__ cudaPointerAttributes
    type, bind(c), public :: cudaPointerAttributes
          !/**
            !* The physical location of the memory, ::cudaMemoryTypeHost or 
            !* ::cudaMemoryTypeDevice.
         !*/
        integer(kind(cudaMemoryTypeHost)) :: memoryType
         !/** 
            !* The device against which the memory was allocated or registered.
            !* If the memory type is ::cudaMemoryTypeDevice then this identifies 
            !* the device on which the memory referred physically resides.  If
            !* the memory type is ::cudaMemoryTypeHost then this identifies the 
            !* device which was current when the memory was allocated or registered
            !* (and if that device is deinitialized then this allocation will vanish
            !* with that device's state).
        !*/
        integer(c_int)                    :: device
         !/**
            !* The address which may be dereferenced on the current device to access 
            !* the memory or NULL if no such address exists.
         !*/
        type(c_ptr)                       :: devicePointer
         !/**
            !* The address which may be dereferenced on the host to access the
            !* memory or NULL if no such address exists.
        !*/
        type(c_ptr)                       :: hostPointer
         !/**
            !* Indicates if this pointer points to managed memory
         !*/
        integer(c_int)                    :: isManaged
    end type cudaPointerAttributes
    
    !/**
        !* CUDA function attributes
    !*/
    
    !  struct __device_builtin__ cudaFuncAttributes
    type, bind(c), public :: cudaFuncAttributes
          !/**
            !* The size in bytes of statically-allocated shared memory per block
            !* required by this function. This does not include dynamically-allocated
            !* shared memory requested by the user at runtime.
         !*/
        integer(c_size_t)           :: sharedSizeBytes
          !/**
            !* The size in bytes of user-allocated constant memory required by this
            !* function.
          !*/
        integer(c_size_t)           :: constSizeBytes
          !/**
            !* The size in bytes of local memory used by each thread of this function.
           ! */
        integer(c_size_t)           :: localSizeBytes
           !/**
                !* The maximum number of threads per block, beyond which a launch of the
                !* function would fail. This number depends on both the function and the
                !* device on which the function is currently loaded.
            !*/
        integer(c_int)              :: maxThreadsPerBlock
            !/**
                !* The number of registers used by each thread of this function.
            !*/
        integer(c_int)              :: numRegs
             !/**
                !* The PTX virtual architecture version for which the function was
                !* compiled. This value is the major PTX version * 10 + the minor PTX
                !* version, so a PTX version 1.3 function would return the value 13.
            !*/
        integer(c_int)              :: ptxVersion
            !/**
                !* The binary architecture version for which the function was compiled.
                !* This value is the major binary version * 10 + the minor binary version,
                !* so a binary version 1.3 function would return the value 13.
            !*
        integer(c_int)              :: binaryVersion
            !/**
                !* The attribute to indicate whether the function has been compiled with 
                !* user specified option "-Xptxas --dlcm=ca" set.
            !*/
        integer(c_int)              :: cacheModeCA
    end type cudaFuncAttributes
    
    type, bind(c), public :: dim3
          integer(c_int)  :: x
          integer(c_int)  :: y
          integer(c_int)  :: z
    end type dim3
    
     !/**
        !* \brief Destroy all allocations and reset all state on the current device
        !* in the current process.
        !*
        !* Explicitly destroys and cleans up all resources associated with the current
        !* device in the current process.  Any subsequent API call to this device will 
        !* reinitialize the device.
        !*
        !* Note that this function will reset the device immediately.  It is the caller's
        !* responsibility to ensure that the device is not being accessed by any 
        !* other host threads from the process when this function is called.
        !*
        !* \return
        !* ::cudaSuccess
        !* \notefnerr
        !*
       ! * \sa ::cudaDeviceSynchronize
   ! */
    
    interface
        function cudaDeviceReset() result(status) &
                    bind(c,name='cudaDeviceReset')
                 import :: CUDA_SUCCESS
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaDeviceReset
    end interface
    
    !/**
        !* \brief Wait for compute device to finish
        !*
        !!* Blocks until the device has completed all preceding requested tasks.
        !* ::cudaDeviceSynchronize() returns an error if one of the preceding tasks
        !* has failed. If the ::cudaDeviceScheduleBlockingSync flag was set for 
        !* this device, the host thread will block until the device has finished 
        !* its work.
        !*
        !* \return
        !* ::cudaSuccess
        !* \notefnerr
        !*
        !* \sa ::cudaDeviceReset
       ! */
    
    interface
        function cudaDeviceSynchronize() result(status) &
                    bind(c,name='cudaDeviceSynchronize')
                 import :: CUDA_SUCCESS
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaDeviceSynchronize
    end interface
    
    !/**
        !* \brief Set resource limits
        !*
        !* Setting \p limit to \p value is a request by the application to update
        !* the current limit maintained by the device.  The driver is free to
        !* modify the requested value to meet h/w requirements (this could be
        !* clamping to minimum or maximum values, rounding up to nearest element
        !* size, etc).  The application can use ::cudaDeviceGetLimit() to find out
!        /* exactly what the limit has been set to.
    
    interface
        function cudaDeviceSetLimit(limit,val) result(status) &
                    bind(c,name='cudaDeviceSetLimit')
                use, intrinsic :: ISO_C_BINDING
                import :: cudaLimitStackSize
                import :: CUDA_SUCCESS
                integer(kind(cudaLimitStackSize )), value :: limit
                integer(c_size_t),      value      :: val
                integer(kind(CUDA_SUCCESS))        :: status
        end function cudaDeviceSetLimit
    end interface
    
   ! /**
        !* \brief Returns resource limits
        !*
        !* Returns in \p *pValue the current size of \p limit.
    
    interface
        function cudaDeviceGetLimit(pValue,limit) result(status) &
                    bind(c,name='cudaDeviceGetLimit')
                 use, intrinsic :: ISO_C_BINDING
                 import :: cudaLimitStackSize
                 import :: CUDA_SUCCESS
                 integer(c_size_t)                 :: pValue
                 integer(kind(cudaLimitStackSize)), value :: limit
                 integer(kind(CUDA_SUCCESS))       :: status
        end function cudaDeviceGetLimit
    end interface

    !/**
        !* \brief Returns the preferred cache configuration for the current device.
        !*
        !* On devices where the L1 cache and shared memory use the same hardware
        !* resources, this returns through \p pCacheConfig the preferred cache
        !* configuration for the current device. This is only a preference. The
        !* runtime will use the requested configuration if possible, but it is free to
        !* choose a different configuration if required to execute functions.
    
    interface
        function cudaDeviceGetCacheConfig(pCacheConfig) result(status) &
                    bind(c,name='cudaDeviceGetCacheConfig')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                integer(c_int)              :: pCacheConfig
                integer(kind(CUDA_SUCCESS)) :: status
        end function cudaDeviceGetCacheConfig
    end interface
    
    !/**
    !* \brief Returns numerical values that correspond to the least and
    !* greatest stream priorities.
    !*
    
    interface
        function cudaDeviceGetStreamPriorityRange(leastPriority,greatestPriority) result(status) &
                    bind(c,name='cudaDeviceGetStreamPriorityRange')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUDA_SUCCESS
                 integer(c_int)         :: leastPriority
                 integer(c_int)         :: greatesPriority
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaDeviceGetStreamPriorityRange
    end interface
    
    !/**
        !* \brief Sets the preferred cache configuration for the current device.
        !*
        !* On devices where the L1 cache and shared memory use the same hardware
        !* resources, this sets through \p cacheConfig the preferred cache
        !* configuration for the current device. This is only a preference. The
        !* runtime will use the requested configuration if possible, but it is free to
       ! * choose a different configuration if required to execute the function.
    
    interface
        function cudaDeviceSetCacheConfig(cacheConfig) result(status) &
                    bind(c,name='cudaDeviceSetCacheConfig')
                  import :: cudaFuncCachePreferNone 
                  import :: CUDA_SUCCESS
                  integer(kind(cudaFuncCachePreferNone)), value :: cacheConfig
                  integer(kind(CUDA_SUCCESS))            :: status
        end function cudaDeviceSetCacheConfig
    end interface
    
    !/**
        !*! \brief Returns the shared memory configuration for the current device.
                   *
        !* This function will return in \p pConfig the current size of shared memory banks
        !* on the current device. On devices with configurable shared memory banks, 
        !* ::cudaDeviceSetSharedMemConfig can be used to change this setting, so that all 
        !* subsequent kernel launches will by default use the new bank size. When 
        !* ::cudaDeviceGetSharedMemConfig is called on devices without configurable shared 
        !* memory, it will return the fixed bank size of the hardware.
    
    interface
        function cudaDeviceGetSharedMemConfig(pConfig) result(status) &
                    bind(c,name='cudaDeviceGetSharedMemConfig')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                integer(c_int)              :: pConfig
                integer(kind(CUDA_SUCCESS))            :: status
        end function cudaDeviceGetSharedMemConfig
    end interface
    
    interface
        function cudaDeviceSetSharedMemConfig(config) result(status) &
                    bind(c,name='cudDeviceSetSharedMemConfig')
                 import ::   cudaSharedMemBankSizeDefault 
                 import ::   CUDA_SUCCESS
                 integer(kind(cudaSharedMemBankSizeDefault)), value  :: config
                 integer(kind(CUDA_SUCCESS))                  :: status
        end function cudaDeviceSetSharedMemConfig
    end interface
    
    !/**
        !* \brief Returns a handle to a compute device
        !*
        !* Returns in \p *device a device ordinal given a PCI bus ID string.
        !*
        !* \param device   - Returned device ordinal
        !*
        !* \param pciBusId - String in one of the following forms: 
        !* [domain]:[bus]:[device].[function]
        !* [domain]:[bus]:[device]
        !* [bus]:[device].[function]
        !* where \p domain, \p bus, \p device, and \p function are all hexadecimal values
        !*
        !* \return
        !* ::cudaSuccess,
        !* ::cudaErrorInvalidValue,
        !* ::cudaErrorInvalidDevice
       ! * \notefnerr
        !*
        !* \sa ::cudaDeviceGetPCIBusId
    !*/
    
    interface
        function cudaDeviceGetByPCIBusId(device,pciBusId) result(status) &
                    bind(c,name='cudaDeviceGetByPCIBusId')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                integer(c_int)                  :: device
                character(c_char), dimension(*) :: pciBusId
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaDeviceGetByPCIBusId
    end interface
    
    !/**
        !* \brief Returns a PCI Bus Id string for the device
        !*
        !* Returns an ASCII string identifying the device \p dev in the NULL-terminated
        !* string pointed to by \p pciBusId. \p len specifies the maximum length of the
        !* string that may be returned.
        !*
        !* \param pciBusId - Returned identifier string for the device in the following format
       ! * [domain]:[bus]:[device].[function]
        !* where \p domain, \p bus, \p device, and \p function are all hexadecimal values.
        !* pciBusId should be large enough to store 13 characters including the NULL-terminator.
        !*
        !* \param len      - Maximum length of string to store in \p name
        !*
        !* \param device   - Device to get identifier string for
        !*  
        !* \return
        !* ::cudaSuccess,
        !* ::cudaErrorInvalidValue,
        !* ::cudaErrorInvalidDevice
        !* \notefnerr
        !*
        !* \sa ::cudaDeviceGetByPCIBusId
        
        !*/
    
    interface
        function cudaDeviceGetPCIBusId(pciBusId,length,device) result(status) &
                    bind(c,name='cudaDeviceGetPCIBusId')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                character(c_char),  dimension(*)  :: pciBusId
                integer(c_int),     value         :: length
                integer(c_int),     value         :: device
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaDeviceGetPCIBusId
    end interface
    
    !/**
        !* \brief Gets an interprocess handle for a previously allocated event
        !*
        !* Takes as input a previously allocated event. This event must have been 
        !* created with the ::cudaEventInterprocess and ::cudaEventDisableTiming
        !* flags set. This opaque handle may be copied into other processes and
        !* opened with ::cudaIpcOpenEventHandle to allow efficient hardware
       ! * synchronization between GPU work in different processes.
    
    interface
        function cudaIpcGetEventHandle(handle,event) result(status) &
                    bind(c,name='cudaIpcGetEventHandle')
                  import :: cudaIpcEventHandle_st
                  import :: CUevent
                  import :: CUDA_SUCCESS
                  type(cudaIpcEventHandle_st)       :: handle
                  type(CUevent),        value       :: event
                  integer(kind(CUDA_SUCCESS))       :: status
        end function cudaIpcGetEventHandle
    end interface
    
    !/**
       ! * \brief Gets an interprocess memory handle for an existing device memory
       ! *          allocation
       ! *
       ! * Takes a pointer to the base of an existing device memory allocation created 
       ! * with ::cudaMalloc and exports it for use in another process. This is a 
       ! * lightweight operation and may be called multiple times on an allocation
      !  * without adverse effects. 
    
    interface
        function cudaIpcGetMemHandle(handle,devPtr) result(status) &
                    bind(c,name='cudaIpcGetMemHandle')
                 import :: cudaIpcMemHandle_st
                 import :: CUDA_SUCCESS
                 type(cudaIpcMemHandle_st)          :: handle
                 type(c_ptr),   value               :: devPtr
                 integer(kind(CUDA_SUCCESS))       :: status
        end function cudaIpcGetMemHandle
    end interface
    
   ! /**
        !* \brief Opens an interprocess memory handle exported from another process
        !*          and returns a device pointer usable in the local process.
        !*
        !* Maps memory exported from another process with ::cudaIpcGetMemHandle into
        !* the current device address space. For contexts on different devices 
        !* ::cudaIpcOpenMemHandle can attempt to enable peer access between the
        !* devices as if the user called ::cudaDeviceEnablePeerAccess. This behavior is 
        !* controlled by the ::cudaIpcMemLazyEnablePeerAccess flag. 
        !* ::cudaDeviceCanAccessPeer can determine if a mapping is possible.
    
    interface
        function cudaIpcOpenMemHandle(devPtr,handle,flags) result(status) &
                    bind(c,name='cudaIpcOpenMemHandle')
                 use, intrinsic :: ISO_C_BINDING
                 import ::  cudaIpcMemHandle_st
                 import ::  CUDA_SUCCESS
                 type(c_ptr)                    :: devPtr
                 type(cudaIpcMemHandle_st)      :: handle
                 integer(c_int),        value   :: flags
                 integer(kind(CUDA_SUCCESS))       :: status
        end function cudaIpcOpenMemHandle
    end interface
    
    !**
        !* \brief Close memory mapped with cudaIpcOpenMemHandle
        !    * 
       ! * Unmaps memory returnd by ::cudaIpcOpenMemHandle. The original allocation
        ! * in the exporting process as well as imported mappings in other processes
       ! * will be unaffected.
    
    interface
        function cudaIpcCloseMemHandle(devPtr) result(status) &
                    bind(c,name='cudaIpcCloseMemHandle')
                import :: CUDA_SUCCESS
                type(c_ptr),    value             :: devPtr
                integer(kind(CUDA_SUCCESS))       :: status
        end function cudaIpcCloseMemHandle
    end interface
    
    !/**
    !* \brief Returns the last error from a runtime call
   ! *
    !* Returns the last error that has been produced by any of the runtime calls
    !* in the same host thread and resets it to ::cudaSuccess.
    !*
    !* \return
    !* ::cudaSuccess,
    !* ::cudaErrorMissingConfiguration,
    !* ::cudaErrorMemoryAllocation,
    !* ::cudaErrorInitializationError,
   ! * ::cudaErrorLaunchFailure,
    !* ::cudaErrorLaunchTimeout,
    !* ::cudaErrorLaunchOutOfResources,
    !* ::cudaErrorInvalidDeviceFunction,
    !* ::cudaErrorInvalidConfiguration,
    !* ::cudaErrorInvalidDevice,
    !* ::cudaErrorInvalidValue,
    !* ::cudaErrorInvalidPitchValue,
    !* ::cudaErrorInvalidSymbol,
    !* ::cudaErrorUnmapBufferObjectFailed,
    !* ::cudaErrorInvalidHostPointer,
    !* ::cudaErrorInvalidDevicePointer,
    !* ::cudaErrorInvalidTexture,
    !* ::cudaErrorInvalidTextureBinding,
    !* ::cudaErrorInvalidChannelDescriptor,
    !* ::cudaErrorInvalidMemcpyDirection,
    !* ::cudaErrorInvalidFilterSetting,
    !* ::cudaErrorInvalidNormSetting,
    !* ::cudaErrorUnknown,
    !* ::cudaErrorInvalidResourceHandle,
    !* ::cudaErrorInsufficientDriver,
    !* ::cudaErrorSetOnActiveProcess,
    !* ::cudaErrorStartupFailure,
    !* \notefnerr
    !*
   ! * \sa ::cudaPeekAtLastError, ::cudaGetErrorName, ::cudaGetErrorString, ::cudaError
   ! */
    
    interface
        function cudaGetLastError() result(status) &
                    bind(c,name='cudaGetLastError')
                 import :: CUDA_SUCCESS
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaGetLastError
    end interface
    
    !/**
        !* \brief Returns the last error from a runtime call
        !*
        !* Returns the last error that has been produced by any of the runtime calls
        !* in the same host thread. Note that this call does not reset the error to
        !* ::cudaSuccess like ::cudaGetLastError().
    
    interface
        function cudaPeekAtLastError() result(status) &
                    bind(c,name='cudaPeekAtLastError')
                 import :: CUDA_SUCCESS
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaPeekAtLastError
    end interface
    
    !/**
        !* \brief Returns the number of compute-capable devices
        !*
        !* Returns in \p *count the number of devices with compute capability greater
        !* or equal to 2.0 that are available for execution.  If there is no such
        !* device then ::cudaGetDeviceCount() will return ::cudaErrorNoDevice.
        !* If no driver can be loaded to determine if any such devices exist then
        !* ::cudaGetDeviceCount() will return ::cudaErrorInsufficientDriver.
        !*
        !* \param count - Returns the number of devices with compute capability
        !* greater or equal to 2.0
        !*
        !* \return
        !* ::cudaSuccess,
        !* ::cudaErrorNoDevice,
        !* ::cudaErrorInsufficientDriver
        !* \notefnerr
        !*
        !* \sa ::cudaGetDevice, ::cudaSetDevice, ::cudaGetDeviceProperties,
        !* ::cudaChooseDevice
    !*  /
    
    interface
        function cudaGetDeviceCount(count) result(status) &
                    bind(c,name='cudaGetDeviceCount')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUDA_SUCCESS
                 integer(c_int)     :: count
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaGetDeviceCount
    end interface
    
    !/**
        !* \brief Returns information about the compute-device
        !*
        !* Returns in \p *prop the properties of device \p dev. The ::cudaDeviceProp
        !* structure is defined as:
    
    interface
        function cudaGetDeviceProperties(prop,device) result(status) &
                    bind(c,name='cudaGetDeviceProperties')
                use, intrinsic :: ISO_C_BINDING
                import :: cudaDeviceProp
                import :: CUDA_SUCCESS
                type(cudaDeviceProp)        :: prop
                integer(c_int),     value   :: device
                integer(kind(CUDA_SUCCESS)) :: status
        end function cudaGetDeviceProperties
    end interface
    
    !/**
        !* \brief Returns information about the device
       ! *
       !  * Returns in \p *value the integer value of the attribute \p attr on device
       !  * \p device.
    
    interface
        function  cudaDeviceGetAttribute(val,attr,device) result(status) &
                    bind(c,name='cudaDeviceGetAttribute')
                use, intrinsic :: ISO_C_BINDING
                import :: cudaDevAttrMaxThreadsPerBlock
                import :: CUDA_SUCCESS
                integer(c_int)                               :: val
                integer(kind(cudaDevAttrMaxThreadsPerBlock)), value :: attr
                integer(c_int),     value                    :: device
                integer(kind(CUDA_SUCCESS))                  :: status
        end function cudaDeviceGetAttribute
    end interface
    
    interface
        function cudaDeviceGetP2PAttribute(val,attr,srcDevice,dstDevice) result(status) &
                     bind(c,name='cudaDeviceGetP2PAttribute')
                 use, intrinsic :: ISO_C_BINDING
                 import :: cudaDevP2PAttrPerformanceRank   
                 import :: CUDA_SUCCESS
                 integer(c_int)                               :: val
                 integer(kind(cudaDevP2PAttrPerformanceRank)), value :: attr
                 integer(c_int),        value                 :: srcDevice
                 integer(c_int),        value                 :: dstDevice
                 integer(kind(CUDA_SUCCESS))                  :: status
        end function
    end interface
    
   ! /**
       ! * \brief Select compute-device which best matches criteria
       ! *
       ! * Returns in \p *device the device which has properties that best match
       ! * \p *prop.
        !*
       ! * \param device - Device with best match
       ! * \param prop   - Desired device properties
       ! *
       ! * \return
       ! * ::cudaSuccess,
       ! * ::cudaErrorInvalidValue
       ! * \notefnerr
       ! *
       ! * \sa ::cudaGetDeviceCount, ::cudaGetDevice, ::cudaSetDevice,
       ! * ::cudaGetDeviceProperties
    !*/
    
    interface
        function cudaChooseDevice(device,prop) result(status) &
                    bind(c,name='cudaChooseDevice')
                 use, intrinsic :: ISO_C_BINDING
                 import :: cudaDeviceProp
                 import :: CUDA_SUCCESS
                 integer(c_int)              :: device
                 type(cudaDeviceProp)        :: prop
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaChooseDevice
    end interface
    
    !/**
        !* \brief Set device to be used for GPU executions
        !*
        !* Sets \p device as the current device for the calling host thread.
        !* Valid device id's are 0 to (::cudaGetDeviceCount() - 1).
        !*
    
    interface
        function cudaSetDevice(device) result(status) &
                    bind(c,name='cudaSetDevice')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                integer(c_int), value       :: device
                integer(kind(CUDA_SUCCESS)) :: status
        end function cudaSetDevice
    end interface
    
    !/**
       ! * \brief Returns which device is currently being used
        !*
        !* Returns in \p *device the current device for the calling host thread.
        !*
        !* \param device - Returns the device on which the active host thread
        !* executes the device code.
        !*
        !* \return
        !* ::cudaSuccess
        !* \notefnerr
        !*
        !* \sa ::cudaGetDeviceCount, ::cudaSetDevice, ::cudaGetDeviceProperties,
        !* ::cudaChooseDevice
        !*/
    
    interface
        function cudaGetDevice(device) result(status) &
                    bind(c,name='cudaGetDevice')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  integer(c_int)              :: device
                  integer(kind(CUDA_SUCCESS)) :: status
        end function cudaGetDevice
    end interface
    
    !/**
        !* \brief Set a list of devices that can be used for CUDA
        !*
        !* Sets a list of devices for CUDA execution in priority order using
        !* \p device_arr. The parameter \p len specifies the number of elements in the
        !* list.  CUDA will try devices from the list sequentially until it finds one
        !* that works. 
    
    interface
        function cudaSetValidDevices(device_arr,length) result(status) &
                    bind(c,name='cudaSetValidDevices')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  integer(c_int), dimension(*)      :: device_arr
                  integer(c_int),   value           :: length
                  integer(kind(CUDA_SUCCESS))       :: status
        end function cudaSetValidDevices
    end interface
    
    !/**
        !* \brief Sets flags to be used for device executions
        !*
        !* Records \p flags as the flags to use when initializing the current 
        !* device.  If no device has been made current to the calling thread,
        !* then \p flags will be applied to the initialization of any device
       ! * initialized by the calling host thread, unless that device has had
       ! * its initialization flags set explicitly by this or any host thread.
        !*
    
    interface
        function cudaSetDeviceFlags(flags) result(status) &
                    bind(c,name='cudaSetDeviceFlags')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  integer(c_int),       value       :: flags
                  integer(kind(CUDA_SUCCESS))       :: status
        end function cudaSetDeviceFlags
    end interface
    
    !/**
        !* \brief Gets the flags for the current device
        !*
        !* Returns in \p flags the flags for the current device.  If there is a
       ! * current device for the calling thread, and the device has been initialized
        !* or flags have been set on that device specifically, the flags for the
       ! * device are returned.  If there is no current device, but flags have been
       ! * set for the thread with ::cudaSetDeviceFlags, the thread flags are returned.
       ! * Finally, if there is no current device and no thread flags, the flags for
        !* the first device are returned, which may be the default flags.  Compare
       ! * to the behavior of ::cudaSetDeviceFlags.
        
   interface
        function cudaGetDeviceFlags(flags) result(status) &
                    bind(c,name='cudaGetDeviceFlags')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  integer(c_int)                    :: flags
                  integer(kind(CUDA_SUCCESS))       :: status
        end function cudaGetDeviceFlags
   end interface
   
   !/**
        !* \brief Create an asynchronous stream
        !*
       ! * Creates a new asynchronous stream.
        !    *
        !* \param pStream - Pointer to new stream identifier
       ! *
       ! * \return
        !* ::cudaSuccess,
        !* ::cudaErrorInvalidValue
       ! * \notefnerr
       !  *
   
   interface
        function cudaStreamCreate(pStream) result(status) &
                    bind(c,name='cudaStreamCreate')
                 !use, intrinsic :: ISO_C_BINDING
                 import :: CUstream
                 import :: CUDA_SUCCESS
                 type(CUstream)              :: pStream
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaStreamCreate
   end interface
   
   !/**
        !* \brief Create an asynchronous stream
        !*
        !* Creates a new asynchronous stream.  The \p flags argument determines the 
        !* behaviors of the stream.  Valid values for \p flags are
        !* - ::cudaStreamDefault: Default stream creation flag.
        !* - ::cudaStreamNonBlocking: Specifies that work running in the created 
        !*   stream may run concurrently with work in stream 0 (the NULL stream), and that
       ! *   the created stream should perform no implicit synchronization with stream 0.
        !*
   
   interface
        function cudaStreamCreateWithFlags(pStream,flags) result(status) &
                    bind(c,name='cudaStreamCreateWithFlags')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUstream
                 import :: CUDA_SUCCESS
                 type(CUstream)              :: pStream
                 integer(c_int),    value    :: flags
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaStreamCreateWithFlags
   end interface
   
   !/**
        !* \brief Create an asynchronous stream with the specified priority
       ! *
        !* Creates a stream with the specified priority and returns a handle in \p pStream.
       ! * This API alters the scheduler priority of work in the stream. Work in a higher
       ! * priority stream may preempt work already executing in a low priority stream.
       ! *
       ! * \p priority follows a convention where lower numbers represent higher priorities.
       !! * '0' represents default priority. The range of meaningful numerical priorities can
       ! * be queried using ::cudaDeviceGetStreamPriorityRange. If the specified priority is
       ! * outside the numerical range returned by ::cudaDeviceGetStreamPriorityRange,
      !   * it will automatically be clamped to the lowest or the highest number in the range.
   
   interface
        function  cudaStreamCreateWithPriority(pStream,flags,priority) result(status) &
                    bind(c,name='cudaStreamCreateWithPriority')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUstream
                 import :: CUDA_SUCCESS
                 type(CUstream)              :: pStream
                 integer(c_int),    value    :: flags
                 integer(c_int),    value    :: priority
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaStreamCreateWithPriority
   end interface
   
   !/**
        !* \brief Query the priority of a stream
        !*
        !* Query the priority of a stream. The priority is returned in in \p priority.
        !* Note that if the stream was created with a priority outside the meaningful
        !* numerical range returned by ::cudaDeviceGetStreamPriorityRange,
        !* this function returns the clamped priority.
        !* See ::cudaStreamCreateWithPriority for details about priority clamping.
        !*
   
   interface
        function cudaStreamGetPriority(hStream,priority) result(status) &
                    bind(c,name='cudaStreamGetPriority')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUstream
                 import :: CUDA_SUCCESS 
                 type(CUstream), value       :: hStream
                 integer(c_int)              :: priority
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaStreamGetPriority
   end interface
   
   !/**
        !* \brief Query the flags of a stream
        !*
        !* Query the flags of a stream. The flags are returned in \p flags.
        !* See ::cudaStreamCreateWithFlags for a list of valid flags.
        !*
        !* \param hStream - Handle to the stream to be queried
        !* \param flags   - Pointer to an unsigned integer in which the stream's flags are returned
        !*
       ! * \return
   
   interface
        function cudaStreamGetFlags(hStream,flags) result(status) &
                    bind(c,name='cudaStreamGetFlags')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUstream
                 import :: CUDA_SUCCESS 
                 type(CUstream), value       :: hStream
                 integer(c_int)              :: flags
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaStreamGetFlags
   end interface
   
   !/**
        ! * \brief Destroys and cleans up an asynchronous stream
       ! *
        !* Destroys and cleans up the asynchronous stream specified by \p stream.
        !*
        !* In case the device is still doing work in the stream \p stream
        !* when ::cudaStreamDestroy() is called, the function will return immediately 
       ! !* and the resources associated with \p stream will be released automatically 
       ! * once the device has completed all work in \p stream.
   
    interface
        function cudaStreamDestroy(stream) result(status) &
                    bind(c,name='cudaStreamDestroy')
                  !use, intrinsic :: ISO_C_BINDING
                 import :: CUstream
                 import :: CUDA_SUCCESS 
                 type(CUstream), value       :: stream 
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaStreamDestroy
    end interface
    
    !/**
        !* \brief Make a compute stream wait on an event
        !*
        !* Makes all future work submitted to \p stream wait until \p event reports
        !* completion before beginning execution.  This synchronization will be
        !* performed efficiently on the device.  The event \p event may
        !* be from a different context than \p stream, in which case this function
       ! * will perform cross-device synchronization.
    
    interface
        function cudaStreamWaitEvent(stream,event,flags) result(status) &
                    bind(c,name='cudaStreamWaitEvent')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUstream
                 import :: CUevent
                 import :: CUDA_SUCCESS
                 type(CUstream),    value       :: stream
                 type(CUevent),     value       :: event
                 integer(c_int),    value       :: flags
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaStreamWaitEvent
    end interface
    
    !/**
        !* \brief Waits for stream tasks to complete
        !*
        !* Blocks until \p stream has completed all operations. If the
        !* ::cudaDeviceScheduleBlockingSync flag was set for this device, 
        !* the host thread will block until the stream is finished with 
        !* all of its tasks.
        !*
        !* \param stream - Stream identifier
        !*
        !* \return
        !* ::cudaSuccess,
        !* ::cudaErrorInvalidResourceHandle
        !* \notefnerr
        !*
        !* \sa ::cudaStreamCreate, ::cudaStreamCreateWithFlags, ::cudaStreamQuery, ::cudaStreamWaitEvent, ::cudaStreamAddCallback, ::cudaStreamDestroy
        !*/
    
    interface
        function cudaStreamSynchronize(stream) result(status) &
                    bind(c,name='cudaStreamSynchronize')
                 import :: CUstream
                 import :: CUDA_SUCCESS
                 type(CUstream),  value      :: stream
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaStreamSynchronize
    end interface
    
    !/**
       ! * \brief Queries an asynchronous stream for completion status
        !*
        !* Returns ::cudaSuccess if all operations in \p stream have
        !* completed, or ::cudaErrorNotReady if not.
        !*
        !* For the purposes of Unified Memory, a return value of ::cudaSuccess
        !* is equivalent to having called ::cudaStreamSynchronize().
        !*
       ! * \param stream - Stream identifier
       ! *
       ! * \return
      !  * ::cudaSuccess,
      !  * ::cudaErrorNotReady,
      !  * ::cudaErrorInvalidResourceHandle
      !  *   \notefnerr
      !   *
      !  * \sa ::cudaStreamCreate, ::cudaStreamCreateWithFlags, ::cudaStreamWaitEvent, ::cudaStreamSynchronize, ::cudaStreamAddCallback, ::cudaStreamDestroy
      !  */
    
    interface
        function cudaStreamQuery(stream)  result(status) &
                    bind(c,name='cudaStreamQuery')
                 import :: CUstream
                 import :: CUDA_SUCCESS
                 type(CUstream),  value      :: stream
                 integer(kind(CUDA_SUCCESS)) :: status
        end function cudaStreamQuery
    end interface
    
    !/**
        !* \brief Attach memory to a stream asynchronously
        !*
        !* Enqueues an operation in \p stream to specify stream association of
        !* \p length bytes of memory starting from \p devPtr. This function is a
        !* stream-ordered operation, meaning that it is dependent on, and will
        !* only take effect when, previous work in stream has completed. Any
        !* previous association is automatically replaced.
    
    interface
        function   cudaStreamAttachMemAsync(stream,devPtr,length,flags) result(status) &
                        bind(c,name='cudaStreamAttachMemAsync')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(CUstream),     value       :: stream
                    type(c_ptr),        value       :: devPtr
                    integer(c_int),     value       :: length
                    integer(c_int),     value       :: flags
                    integer(kind(CUDA_SUCCESS))     :: status
        end function cudaStreamAttachMemAsync
    end interface
    
    !/**
        !* \brief Creates an event object
        !*
        !* Creates an event object using ::cudaEventDefault.
        !*
        !* \param event - Newly created event
        !*
        !* \return
        !* ::cudaSuccess,
        !* ::cudaErrorInitializationError,
        !* ::cudaErrorInvalidValue,
        !* ::cudaErrorLaunchFailure,
        !* ::cudaErrorMemoryAllocation
        !* \notefnerr
        !*
        !* \sa \ref ::cudaEventCreate(cudaEvent_t*, unsigned int) "cudaEventCreate (C++ API)",
       ! * ::cudaEventCreateWithFlags, ::cudaEventRecord, ::cudaEventQuery,
        !* ::cudaEventSynchronize, ::cudaEventDestroy, ::cudaEventElapsedTime,
       ! * ::cudaStreamWaitEvent
    !*/
    
    interface
        function cudaEventCreate(event) result(status) &
                    bind(c,name='cudaEventCreate')
                  import :: CUevent
                  import :: CUDA_SUCCESS
                  type(CUevent),    value         :: event
                  integer(kind(CUDA_SUCCESS))     :: status
        end function cudaEventCreate
    end interface
    
    !/**
        !* \brief Creates an event object with the specified flags
        !*
        !* Creates an event object with the specified flags. Valid flags include:
       ! * - ::cudaEventDefault: Default event creation flag.
       ! * - ::cudaEventBlockingSync: Specifies that event should use blocking
       ! *   synchronization. A host thread that uses ::cudaEventSynchronize() to wait
       ! *   on an event created with this flag will block until the event actually
       ! *   completes.
    
    interface
        function cudaEventCreateWithFlags(event,flags) result(status) &
                    bind(c,name='cudaEventCreateWithFlags')
                use, intrinsic :: ISO_C_BINDING
                import :: CUevent
                import :: CUDA_SUCCESS
                type(CUevent)                   :: event
                integer(c_int), value           :: flags
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaEventCreateWithFlags
    end interface
    
    !/**
        !* \brief Records an event
        !*
        !* Records an event. See note about NULL stream behavior. Since operation
        !* is asynchronous, ::cudaEventQuery() or ::cudaEventSynchronize() must
        !* be used to determine when the event has actually been recorded.
        !*
    
    interface
        function cudaEventRecord(event,stream) result(status) &
                    bind(c,name='cudaEventRecord')
                 import :: CUevent
                 import :: CUstream
                 import :: CUDA_SUCCESS
                 type(CUevent),     value        :: event
                 type(CUstream),    value        :: stream
                 integer(kind(CUDA_SUCCESS))     :: status
        end function cudaEventRecord
    end interface
    
    !/**
        !* \brief Queries an event's status
       ! *
        !* Query the status of all device work preceding the most recent call to
        !* ::cudaEventRecord() (in the appropriate compute streams, as specified by the
       ! * arguments to ::cudaEventRecord()).
       ! *
    
    interface
        function cudaEventQuery(event) result(status) &
                    bind(c,name='cudaEventQuery')
                import :: CUevent
                import :: CUDA_SUCCESS
                type(CUevent),      value   :: event
                integer(kind(CUDA_SUCCESS)) :: status
        end function cudaEventQuery
    end interface
    
    !**
        !* \brief Waits for an event to complete
        !*
        !* Wait until the completion of all device work preceding the most recent
        !* call to ::cudaEventRecord() (in the appropriate compute streams, as specified
       ! * by the arguments to ::cudaEventRecord()).
        !*
       ! * If ::cudaEventRecord() has not been called on \p event, ::cudaSuccess is
        !* returned immediately.
    
    interface
        function cudaEventSynchronize(event) result(status) &
                    bind(c,name='cudaEventSynchronize')
                import :: CUevent
                import :: CUDA_SUCCESS
                type(CUevent),      value   :: event
                integer(kind(CUDA_SUCCESS)) :: status
        end function cudaEventSynchronize
    end interface
    
    !/**
        !* \brief Destroys an event object
        !*
        !* Destroys the event specified by \p event.
        !*
        !* In case \p event has been recorded but has not yet been completed
        !* when ::cudaEventDestroy() is called, the function will return immediately and 
       ! * the resources associated with \p event will be released automatically once
       ! * the device has completed \p event.
    
    interface
        function cudaEventDestroy(event) result(status) &
                    bind(c,name='cudaEventDestroy')
                import :: CUevent
                import :: CUDA_SUCCESS
                type(CUevent),      value   :: event
                integer(kind(CUDA_SUCCESS)) :: status
        end function cudaEventDestroy
    end interface
    
   ! /**
        !* \brief Computes the elapsed time between events
        !*
        !!* resolution of around 0.5 microseconds).
       ! *
        !* If either event was last recorded in a non-NULL stream, the resulting time
       ! * may be greater than expected (even if both used the same stream handle). This
       ! * happens because the ::cudaEventRecord() operation takes place asynchronously
       ! * and there is no guarantee that the measured latency is actually just between
       ! * the two events. Any number of other different stream operations could execute
        !* in between the two measured events, thus altering the timing in a significant
       ! * way.
    
    interface
        function cudaEventElapsedTime(ms,start,en)  result(status) &
                    bind(c,name='cudaEventElapsedTime')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUevent
                 import :: CUDA_SUCCESS
                 real(c_float)                   :: ms
                 type(CUevent),     value        :: start
                 type(CUevent),     value        :: en
                 integer(kind(CUDA_SUCCESS))     :: status
        end function cudaEventElapsedTime
    end interface
    
    interface
        function cudaGetErrorString(error) result(c_res) &
                    bind(c,name='cudaGetErrorString')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                integer(kind(CUDA_SUCCESS)), value :: error
                type(c_ptr)                        ::  c_res
        end function cudaGetErrorString
    end interface
    
    !/**
        !* \brief Sets the preferred cache configuration for a device function
        !*
        !* On devices where the L1 cache and shared memory use the same hardware
        !* resources, this sets through \p cacheConfig the preferred cache configuration
       ! * for the function specified via \p func. This is only a preference. The
        !* runtime will use the requested configuration if possible, but it is free to
        !* choose a different configuration if required to execute \p func.
    
    interface
        function cudaFuncSetCacheConfig(func,cacheConfig) result(status) &
                    bind(c,name='cudaFuncSetCacheConfig')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUDA_SUCCESS
                  type(c_ptr),      value         :: func
                  integer(c_int),   value         :: cacheConfig
                  integer(kind(CUDA_SUCCESS))     :: status
        end function cudaFuncSetCacheConfig
    end interface
    
    !**
        !* \brief Sets the shared memory configuration for a device function
        !*
        !* On devices with configurable shared memory banks, this function will 
        !* force all subsequent launches of the specified device function to have
        !* the given shared memory bank size configuration. On any given launch of the
        !* function, the shared memory configuration of the device will be temporarily
        !* changed if needed to suit the function's preferred configuration. Changes in
        !* shared memory configuration between subsequent launches of functions, 
        !* may introduce a device side synchronization point.
    
    interface
        function cudaFuncSetSharedMemConfig(func,config) result(status) &
                    bind(c,name='cudaFuncSetSharedMemConfig')
                import ::   cudaSharedMemBankSizeDefault
                import ::   CUDA_SUCCESS
                type(c_ptr),        value                           :: func
                integer(kind(cudaSharedMemBankSizeDefault)), value  :: config
                integer(kind(CUDA_SUCCESS))                         :: value
        end function cudaFuncSetSharedMemConfig
    end interface
    
    !/**
        !* \brief Find out attributes for a given function
        !*
        !* This function obtains the attributes of a function specified via \p func.
        !* \p func is a device function symbol and must be declared as a
       ! * \c __global__ function. The fetched attributes are placed in \p attr.
        !* If the specified function does not exist, then
       ! * ::cudaErrorInvalidDeviceFunction is returned. For templated functions, pass
      !   * the function symbol as follows: func_name<template_arg_0,...,template_arg_N>
      !   *
    
    interface
        function cudaFuncGetAttributes(attr,func) result(status) &
                    bind(c,name='cudaFuncGetAttributes')
                 import :: cudaFuncAttributes
                 import :: CUDA_SUCCESS
                 type(cudaFuncAttributes)           :: attr
                 type(c_ptr),   value               :: func
                 integer(kind(CUDA_SUCCESS))        :: status
        end function cudaFuncGetAttributes
    end interface
    
    !/**
        !* \brief Returns occupancy for a device function
        !*
        !* Returns in \p *numBlocks the maximum number of active blocks per
        !* streaming multiprocessor for the device function.
        !*
        !* \param numBlocks       - Returned occupancy
        !* \param func            - Kernel function for which occupancy is calculated
        !* \param blockSize       - Block size the kernel is intended to be launched with
        !* \param dynamicSMemSize - Per-block dynamic shared memory usage intended, in bytes
    
    interface
        function cudaOccupancyMaxActiveBlocksPerMultiprocessor(numBlocks,func,blockSize,dynamicSMemSize) result(status) &
                    bind(c,name='cudaOccupancyMaxActiveBlocksPerMultiprocessor')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                integer(c_int)              :: numBlocks
                type(c_ptr),        value   :: func
                integer(c_int),     value   :: blockSize
                integer(c_size_t), value    :: dynamicSMemSize
                integer(kind(CUDA_SUCCESS))        :: status
        end function  cudaOccupancyMaxActiveBlocksPerMultiprocessor
    end interface
    
    !/**
        !* \brief Returns occupancy for a device function with the specified flags
        ! *
        ! * Returns in \p *numBlocks the maximum number of active blocks per
        !* streaming multiprocessor for the device function.
    
    interface
        function  cudaOccupancyMaxActiveBlocksPerMultiprocessorWithFlags(numBlocks,func,blockSize,dynamicSMemSize,flags) result(status) &
                    bind(c,name='cudaOccupancyMaxActiveBlocksPerMultiprocessorWithFlags')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                integer(c_int)              :: numBlocks
                type(c_ptr),        value   :: func
                integer(c_int),     value   :: blockSize
                integer(c_size_t), value    :: dynamicSMemSize
                integer(c_int),     value   :: flags
                integer(kind(CUDA_SUCCESS)) :: status
        end function  cudaOccupancyMaxActiveBlocksPerMultiprocessorWithFlags
    end interface
    
    !/**
        !* \brief Allocates memory that will be automatically managed by the Unified Memory system
        !*
        !* Allocates \p size bytes of managed memory on the device and returns in
        !* \p *devPtr a pointer to the allocated memory. If the device doesn't support
        !* allocating managed memory, ::cudaErrorNotSupported is returned. Support
        !* for managed memory can be queried using the device attribute
        !* ::cudaDevAttrManagedMemory. The allocated memory is suitably
       ! * aligned for any kind of variable. The memory is not cleared. If \p size
        !* is 0, ::cudaMallocManaged returns ::cudaErrorInvalidValue. The pointer
        !* is valid on the CPU and on all GPUs in the system that support managed memory.
        !* All accesses to this pointer must obey the Unified Memory programming model.
 
    interface
        function cudaMallocManaged(devPtr,size,flags) result(status) &
                    bind(c,name='cudaMallocManaged')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                type(c_ptr)                     ::      devPtr
                integer(c_size_t),      value   ::      size
                integer(c_int),         value   ::      flags
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMallocManaged
    end interface
    
    !/**
        !* \brief Allocate memory on the device
        !*
        !* Allocates \p size bytes of linear memory on the device and returns in
        !* \p *devPtr a pointer to the allocated memory. The allocated memory is
        !* suitably aligned for any kind of variable. The memory is not cleared.
        !* ::cudaMalloc() returns ::cudaErrorMemoryAllocation in case of failure.
        !*
        !* The device version of ::cudaFree cannot be used with a \p *devPtr
        !* allocated using the host API, and vice versa.
        !*
        !* \param devPtr - Pointer to allocated device memory
        !* \param size   - Requested allocation size in bytes
        !*
        !* \return
        !* ::cudaSuccess,
       ! * ::cudaErrorMemoryAllocation
    
    interface
        function cudaMalloc(devPtr,size) result(status) &
                    bind(c,name='cudaMalloc')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                type(c_ptr)                     ::  devPtr
                integer(c_size_t),  value       :: size
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMalloc
    end interface
    
    !/**
        !* \brief Allocates page-locked memory on the host
        !*
        !* Allocates \p size bytes of host memory that is page-locked and accessible
        !* to the device. The driver tracks the virtual memory ranges allocated with
       ! * this function and automatically accelerates calls to functions such as
        ! * ::cudaMemcpy*(). Since the memory can be accessed directly by the device,
        !* it can be read or written with much higher bandwidth than pageable memory
        !* obtained with functions such as ::malloc(). Allocating excessive amounts of
       ! * memory with ::cudaMallocHost() may degrade system performance, since it
       ! * reduces the amount of memory available to the system for paging. As a
       ! * result, this function is best used sparingly to allocate staging areas for
       ! * data exchange between host and device.
       ! *
       ! * \param ptr  - Pointer to allocated host memory
       ! * \param size - Requested allocation size in bytes
    
    interface
        function cudaMallocHost(ptr,size) result(status) &
                    bind(c,name='cudaMallocHost')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                type(c_ptr)                     :: ptr
                integer(c_size_t), value        :: size
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMallocHost
    end interface
    
   ! /**
        !* \brief Allocates pitched memory on the device
        !*
       ! * Allocates at least \p width (in bytes) * \p height bytes of linear memory
       ! * on the device and returns in \p *devPtr a pointer to the allocated memory.
       ! * The function may pad the allocation to ensure that corresponding pointers
       ! * in any given row will continue to meet the alignment requirements for
       ! * coalescing as the address is updated from row to row. The pitch returned in
       ! * \p *pitch by ::cudaMallocPitch() is the width in bytes of the allocation.
       ! * The intended usage of \p pitch is as a separate parameter of the allocation,
       ! * used to compute addresses within the 2D array. Given the row and column of
       ! * an array element of type \p T, the address is computed as:
       !! * \code
       !  T* pElement = (T*)((char*)BaseAddress + Row * pitch) + Column;
    ! \endcode
         
    interface
        function   cudaMallocPitch(devPtr,pitch,width,height) result(status) &
                        bind(c,name='cudaMallocPitch')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    type(c_ptr)                 :: devPtr
                    integer(c_size_t)           :: pitch
                    integer(c_size_t), value    :: width
                    integer(c_size_t), value    :: height
                    integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMallocPitch
    end interface
    
    !/**
        !* \brief Allocate an array on the device
       ! *
       ! * Allocates a CUDA array according to the ::cudaChannelFormatDesc structure
       ! * \p desc and returns a handle to the new CUDA array in \p *array.
       ! *
       ! * The ::cudaChannelFormatDesc is defined as:
       ! * \code
       !      struct cudaChannelFormatDesc {
       !               int x, y, z, w;
       !          enum cudaChannelFormatKind f;
       !      };
       !  \endcode
    
    interface
        function   cudaMallocArray(array,desc,width,height,flags) result(status) &
                        bind(c,name='cudaMallocArray')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUArray_st
                    import :: cudaChannelFormatDesc
                    import :: CUDA_SUCCESS
                    type(CUArray_st)                :: array
                    type(cudaChannelFormatDesc)     :: desc
                    integer(c_size_t), value        :: width
                    integer(c_size_t), value        :: height
                    integer(c_int),    value        :: flags
                    integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMallocArray
    end interface
    
    !/!**
        !* \brief Frees memory on the device
       ! *
        !* Frees the memory space pointed to by \p devPtr, which must have been
       ! * returned by a previous call to ::cudaMalloc() or ::cudaMallocPitch().
        !* Otherwise, or if ::cudaFree(\p devPtr) has already been called before,
       ! * an error is returned. If \p devPtr is 0, no operation is performed.
       ! * ::cudaFree() returns ::cudaErrorInvalidDevicePointer in case of failure.
       ! *
       ! * The device version of ::cudaFree cannot be used with a \p *devPtr
       ! *   allocated using the host API, and vice versa.
    
    interface
        function cudaFree(devPtr) result(status) &
                    bind(c,name='cudaFree')
                import :: CUDA_SUCCESS
                type(c_ptr),        value :: devPtr
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaFree
    end interface
    
    !/**
       ! * \brief Frees page-locked memory
       !  *
       ! * Frees the memory space pointed to by \p hostPtr, which must have been
       ! * returned by a previous call to ::cudaMallocHost() or ::cudaHostAlloc().
       ! *
       ! * \param ptr - Pointer to memory to free
       ! *
    
    interface
        function cudaFreeHost(ptr) result(status) &
                    bind(c,name='cudaFreeHost')
                import :: CUDA_SUCCESS
                type(c_ptr),        value :: devPtr
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaFreeHost
    end interface
    
    !/**
        !* \brief Frees an array on the device
        !*
        !* Frees the CUDA array \p array, which must have been * returned by a
        !* previous call to ::cudaMallocArray(). If ::cudaFreeArray(\p array) has
        !* already been called before, ::cudaErrorInvalidValue is returned. If
        !* \p devPtr is 0, no operation is performed.
        !*
        !* \param array - Pointer to array to free
    
    interface
        function   cudaFreeArray(array) result(status) &
                        bind(c,name='cudaFreeArray')
                    import :: CUArray_st
                    import :: CUDA_SUCCESS
                    type(CUArray_st),   value   :: array
                    integer(kind(CUDA_SUCCESS))     :: status
        end function cudaFreeArray
    end interface
    
    !/**
        !* \brief Allocates page-locked memory on the host
        !*
        !* Allocates \p size bytes of host memory that is page-locked and accessible
       ! * to the device. The driver tracks the virtual memory ranges allocated with
        !* this function and automatically accelerates calls to functions such as
       ! * ::cudaMemcpy(). Since the memory can be accessed directly by the device, it
       ! * can be read or written with much higher bandwidth than pageable memory
        !* obtained with functions such as ::malloc(). Allocating excessive amounts of
        !* pinned memory may degrade system performance, since it reduces the amount
       ! * of memory available to the system for paging. As a result, this function is
       ! * best used sparingly to allocate staging areas for data exchange between host
       ! * and device.
    
    interface
        function cudaHostAlloc(pHost,size,flags) result(status) &
                    bind(c,name='cudaHostAlloc')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                type(c_ptr)                     :: pHost
                integer(c_size_t), value        :: size
                integer(c_int),    value        :: flags
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaHostAlloc
    end interface
    
    !/**
        !* \brief Registers an existing host memory range for use by CUDA
        !*
        !* Page-locks the memory range specified by \p ptr and \p size and maps it
        !* for the device(s) as specified by \p flags. This memory range also is added
        !* to the same tracking mechanism as ::cudaHostAlloc() to automatically accelerate
        !* calls to functions such as ::cudaMemcpy(). Since the memory can be accessed 
        !* directly by the device, it can be read or written with much higher bandwidth 
        !* than pageable memory that has not been registered.  Page-locking excessive
        !* amounts of memory may degrade system performance, since it reduces the amount
       ! * of memory available to the system for paging. As a result, this function is
        !* best used sparingly to register staging areas for data exchange between
        !* host and device.
    
    interface
        function cudaHostRegister(ptr,size,flags) result(status) &
                    bind(c,name='cudaHostRegister')
                 use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                type(c_ptr),       value        :: pHost
                integer(c_size_t), value        :: size
                integer(c_int),    value        :: flags
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaHostRegister
    end interface
    
    !/**
       ! * \brief Unregisters a memory range that was registered with cudaHostRegister
       ! *
       ! * Unmaps the memory range whose base address is specified by \p ptr, and makes
        !* it pageable again.
        !*
       ! * The base address must be the same one specified to ::cudaHostRegister().
       ! *
       ! * \param ptr - Host pointer to memory to unregister
    
    interface
        function cudaHostUnregister(ptr)    result(status) &
                    bind(c,name='cudaHostUnregister')
                   import :: CUDA_SUCCESS
                   type(c_ptr),     value   :: ptr
                   integer(kind(CUDA_SUCCESS))     :: status
        end function cudaHostUnregister
    end interface
    
    !/**
        !* \brief Passes back device pointer of mapped host memory allocated by
        !* cudaHostAlloc or registered by cudaHostRegister
        !*
        !* Passes back the device pointer corresponding to the mapped, pinned host
        !* buffer allocated by ::cudaHostAlloc() or registered by ::cudaHostRegister().
        !*
        !* ::cudaHostGetDevicePointer() will fail if the ::cudaDeviceMapHost flag was
       ! * not specified before deferred context creation occurred, or if called on a
        !* device that does not support mapped, pinned memory.
    
    interface
        function   cudaHostGetDevicePtr(pDevice,pHost,flags) result(status) &
                        bind(c,name='cudaHostGetDevicePtr')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    type(c_ptr)             :: pDevice
                    type(c_ptr), value      :: pHost
                    integer(c_int), value   :: flags
                    integer(kind(CUDA_SUCCESS))     :: status
        end function cudaHostGetDevicePtr
    end interface
    
    !/**
       ! * \brief Passes back flags used to allocate pinned host memory allocated by
        !* cudaHostAlloc
       ! *
        !* ::cudaHostGetFlags() will fail if the input pointer does not
       ! * reside in an address range allocated by ::cudaHostAlloc().
       ! *
       ! * \param pFlags - Returned flags word
       ! * \param pHost - Host pointer
       ! *
    
    interface
        function cudaHostGetFlags(pFlags,pHost) result(status) &
                    bind(c,name='cudaHostGetFlags')
                    use, intrinsic :: ISO_C_BINDING
                    import :: CUDA_SUCCESS
                    integer(c_int)      :: pFlags
                    type(c_ptr), value  :: pHost
                    integer(kind(CUDA_SUCCESS))     :: status
        end function cudaHostGetFlags
    end interface
    
   ! /**
        !* \brief Allocates logical 1D, 2D, or 3D memory objects on the device
        ! *
        ! * Allocates at least \p width * \p height * \p depth bytes of linear memory
        ! * on the device and returns a ::cudaPitchedPtr in which \p ptr is a pointer
        !* to the allocated memory. The function may pad the allocation to ensure
        !* hardware alignment requirements are met. The pitch returned in the \p pitch
        !* field of \p pitchedDevPtr is the width in bytes of the allocation.
    
    interface
        function cudaMalloc3D(pitchedDevPtr,extent) result(status) &
                    bind(c,name='cudaMalloc3D')
                import ::  cudaPitchedPtr
                import ::  cudaExtent
                import ::  CUDA_SUCCESS
                type(cudaPitchedPtr)        :: pitchedDevPtr
                type(cudaExtent), value     :: extent
                integer(kind(CUDA_SUCCESS))     :: status
        end function  cudaMalloc3D
    end interface
    
    !/**
        !* \brief Allocate an array on the device
        !*
        !* Allocates a CUDA array according to the ::cudaChannelFormatDesc structure
        !* \p desc and returns a handle to the new CUDA array in \p *array.
       ! *
      !  * The ::cudaChannelFormatDesc is defined as:
       ! * \code
      !    struct cudaChannelFormatDesc {
      !  int x, y, z, w;
      !  enum cudaChannelFormatKind f;
     !};
    
    interface
        function cudaMalloc3DArray(array,desc,extent,flags) result(status) &
                    bind(c,name='cudaMalloc3DArray')
                use, intrinsic :: ISO_C_BINDING
                import :: CUArray_st
                import :: cudaChannelFormatDesc
                import :: cudaExtent
                import :: CUDA_SUCCESS
                type(CUArray_st)                :: array
                type(cudaChannelFormatDesc)     :: desc
                type(cudaExtent), value         :: extent
                integer(c_int), value           :: flags
                 integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMalloc3DArray
    end interface
    
    interface
        function cudaMemcpy3D(p) result(status) &
                    bind(c,name='cudaMemcpy3D')
                 import :: cudaMemcpy3DParms
                 import :: CUDA_SUCCESS
                 type(cudaMemcpy3DParms)            :: p
                 integer(kind(CUDA_SUCCESS))        :: status
        end function cudaMemcpy3D
    end interface
    
    interface
        function cudaMemcpy3DPeer(p) result(status) &
                    bind(c,name='cudaMemcpy3DPeer')
                import :: cudaMemcpy3DPeerParms
                import :: CUDA_SUCCESS
                type(cudaMemcpy3DPeerParms)     :: p
                integer(kind(CUDA_SUCCESS))        :: status
        end function  cudaMemcpy3DPeer
    end interface
    
    !/**
    !    * \brief Copies data between 3D objects
    
    interface
        function cudaMemcpy3DAsync(p, stream) result(status) &
                    bind(c,name='cudaMemcpy3DAsync')
                import :: cudaMemcpy3DParams
                import :: CUstream
                import :: CUDA_SUCCESS
                type(cudaMemcpy3DParams)        :: p
                type(CUstream), value           :: stream
                integer(kind(CUDA_SUCCESS))        :: status
        end function cudaMemcpy3DAsync
    end interface
    
    !/**
        !* \brief Copies data between host and device
        !*
        !* Copies \p count bytes from the memory area pointed to by \p src to the
        !* CUDA array \p dst starting at the upper left corner
        !* (\p wOffset, \p hOffset), where \p kind specifies the
       ! * direction of the copy, and must be one of ::cudaMemcpyHostToHost,
       ! * ::cudaMemcpyHostToDevice, ::cudaMemcpyDeviceToHost,
       ! * ::cudaMemcpyDeviceToDevice, or ::cudaMemcpyDefault. Passing
       ! * ::cudaMemcpyDefault is recommended, in which case the type of transfer is
       ! * inferred from the pointer values. However, ::cudaMemcpyDefault is only
       ! * allowed on systems that support unified virtual addressing.
      !  *
    
    interface
        function cudaMemcpyToArrayAsync(dst,wOffset,hOffset,src,count,kin,stream) result(status) &
                    bind(c,name='cudaMemcpyToArrayAsync')
                use, intrinsic :: ISO_C_BINDING
                import :: CUArray_st
                import :: cudaMemcpyHostToHost 
                import :: CUStream
                import :: CUDA_SUCCESS
                type(CUArray_st),  value    :: dst
                integer(c_size_t), value    :: wOffset
                integer(c_size_t), value    :: hOffset
                type(c_ptr),       value    :: src
                integer(c_size_t), value    :: count
                integer(kind(cudaMemcpyHostToHost)), value :: kin
                type(CUstream),    value    :: stream
                integer(kind(CUDA_SUCCESS))  :: status
        end function cudaMemcpyToArrayAsync
    end interface
    
    !/**
        !* \brief Copies data between host and device
        !*
        !* Copies \p count bytes from the CUDA array \p src starting at the upper
        !* left corner (\p wOffset, hOffset) to the memory area pointed to by \p dst,
        !* where \p kind specifies the direction of the copy, and must be one of
        !* ::cudaMemcpyHostToHost, ::cudaMemcpyHostToDevice, ::cudaMemcpyDeviceToHost,
        !* ::cudaMemcpyDeviceToDevice, or ::cudaMemcpyDefault. Passing
        !* ::cudaMemcpyDefault is recommended, in which case the type of transfer is
        !* inferred from the pointer values. However, ::cudaMemcpyDefault is only
        !* allowed on systems that support unified virtual addressing.
    
    interface
        function cudaMemcpyFromArrayAsync(dst,src,wOffset,hOffset,count,kin,stream)  result(status)   &
                     bind(c,name='cudaMemcpyFromArrayAsync')
                use, intrinsic :: ISO_C_BINDING
                import :: CUArray_st
                import :: cudaMemcpyHostToHost 
                import :: CUstream
                import :: CUDA_SUCCESS
                type(c_ptr),        value       :: dst
                type(CUArray_st),   value       :: src
                integer(c_size_t),  value       :: wOffset
                integer(c_size_t),  value       :: hOffset
                
                integer(c_size_t),  value       :: count
                integer(kind(cudaMemcpyHostToHost)), value :: kin
                type(CUstream),     value       :: stream
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemcpyFromArrayAsync
    end interface
    
    !/**
        !* \brief Copies data between host and device
        !*
        !* Copies a matrix (\p height rows of \p width bytes each) from the memory
        !* area pointed to by \p src to the memory area pointed to by \p dst, where
       ! * \p kind specifies the direction of the copy, and must be one of
       ! * ::cudaMemcpyHostToHost, ::cudaMemcpyHostToDevice, ::cudaMemcpyDeviceToHost,
       ! * ::cudaMemcpyDeviceToDevice, or ::cudaMemcpyDefault.
    
    interface
        function cudaMemcpy2DAsync(dst,dpitch,src,spitch,width,height,kin,stream) result(status) &
                      bind(c,name='cudaMemcpy2DAsync')
                    use, intrinsic :: ISO_C_BINDING
                    import :: cudaMemcpyHostToHost
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(c_ptr),        value       :: dst
                    integer(c_size_t),  value       :: dpitch
                    type(c_ptr),        value       :: src
                    integer(c_size_t),  value       :: spitch
                    integer(c_size_t),  value       :: width
                    integer(c_size_t),  value       :: height
                    integer(kind(cudaMemcpyHostToHost)), value  :: kin
                    type(CUstream),     value       :: stream
                    integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemcpy2DAsync
    end interface
    
    !/**
        !* \brief Copies data between host and device
        !*
        !* Copies a matrix (\p height rows of \p width bytes each) from the memory
        !* area pointed to by \p src to the CUDA array \p dst starting at the
        !* upper left corner (\p wOffset, \p hOffset) where \p kind specifies the
        !* direction of the copy, and must be one of ::cudaMemcpyHostToHost,
        !* ::cudaMemcpyHostToDevice, ::cudaMemcpyDeviceToHost,
        !* ::cudaMemcpyDeviceToDevice, or ::cudaMemcpyDefault.
    
    interface
        function cudaMemcpy2DToArrayAsync(dst,wOffset,hOffset,src,spitch,width,height,kin,stream) result(status) &
                    bind(c,name='cudaMemcpy2DToArrayAsync')
                    use, intrinsic ::  ISO_C_BINDING
                    import :: CUArray_st
                    import :: cudaMemcpyHostToHost
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(CUArray_st),   value       :: dst
                    integer(c_size_t),  value       :: wOffset
                    integer(c_size_t),  value       :: hOffset
                    type(c_ptr),        value       :: src
                    integer(c_size_t),  value       :: spitch
                    integer(c_size_t),  value       :: width
                    integer(c_size_t),  value       :: height
                    integer(kind(cudaMemcpyHostToHost)), value  :: kin
                    type(CUstream),     value       :: stream
                    integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemcpy2DToArrayAsync
    end interface
    
    !/**
        !* \brief Copies data between host and device
        !*
        !* Copies a matrix (\p height rows of \p width bytes each) from the CUDA
        !* array \p srcArray starting at the upper left corner
        !* (\p wOffset, \p hOffset) to the memory area pointed to by \p dst, where
        !* \p kind specifies the direction of the copy, and must be one of
        !* ::cudaMemcpyHostToHost, ::cudaMemcpyHostToDevice, ::cudaMemcpyDeviceToHost,
        !* ::cudaMemcpyDeviceToDevice, or ::cudaMemcpyDefault.
    
    interface
        function   cudaMemcpy2DFromArrayAsync(dst,dpitch,src,wOffset,hOffset,width,height,kin,stream) result(status) &
                        bind(c,name='cudaMemcpy2DFromArrayAsync')
                     use, intrinsic :: ISO_C_BINDING
                     import :: CUArray_st 
                    import :: cudaMemcpyHostToHost
                    import :: CUstream
                    import :: CUDA_SUCCESS
                    type(c_ptr),        value       :: dst
                    integer(c_size_t),  value       :: dpitch
                    type(CUArray_st),   value       :: src
                    integer(c_size_t),  value       :: wOffset
                    integer(c_size_t),  value       :: hOffset
                    integer(c_size_t),  value       :: width
                    integer(c_size_t),  value       :: height
                    integer(kind(cudaMemcpyHostToHost)), value  :: kin
                    type(CUstream),     value       :: stream
                    integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemcpy2DFromArrayAsync
    end interface
    
     !\brief Initializes or sets device memory to a value
        !*
        !* Fills the first \p count bytes of the memory area pointed to by \p devPtr
        !* with the constant byte value \p value.
        !*
        !* Note that this function is asynchronous with respect to the host unless
        !* \p devPtr refers to pinned host memory.
        !*
        !* \param devPtr - Pointer to device memory
        !* \param value  - Value to set for each byte of specified memory
        !* \param count  - Size in bytes to set
        !*
        !* \return
        !* ::cudaSuccess,
        !* ::cudaErrorInvalidValue,
        !* ::cudaErrorInvalidDevicePointer
        !* \notefnerr
        !* \note_memset
    
    interface
        function  cudaMemset(devPtr,val,count) result(status) &
                    bind(c,name='cudaMemset')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                type(c_ptr),       value    :: devPtr
                integer(c_int),    value    :: val
                integer(c_size_t), value    :: count
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemset
    end interface
    
     !*
        !* \brief Initializes or sets device memory to a value
        !*
       ! *! Sets to the specified value \p value a matrix (\p height rows of \p width
        !* bytes each) pointed to by \p dstPtr. \p pitch is the width in bytes of the
        !* 2D array pointed to by \p dstPtr, including any padding added to the end
        !* of each row. This function performs fastest when the pitch is one that has
        !* been passed back by ::cudaMallocPitch().
        !*
        !* Note that this function is asynchronous with respect to the host unless
        !* \p devPtr refers to pinned host memory.
        !*
        !* \param devPtr - Pointer to 2D device memory
        !* \param pitch  - Pitch in bytes of 2D device memory
        !* \param value  - Value to set for each byte of specified memory
        !* \param width  - Width of matrix set (columns in bytes)
        !* \param height - Height of matrix set (rows)
    
    interface
        function cudaMemset2D(devPtr,pitch,val,width,height) result(status) &
                    bind(c,name='cudaMemset2D')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUDA_SUCCESS
                 type(c_ptr),       value   :: devPtr
                 integer(c_size_t), value   :: pitch
                 integer(c_int),    value   :: val
                 integer(c_size_t), value   :: width
                 integer(c_size_t), value   :: height
                 integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemset2D
    end interface
    
    !*
        !* \brief Initializes or sets device memory to a value
        ! *
        ! * Initializes each element of a 3D array to the specified value \p value.
        !* The object to initialize is defined by \p pitchedDevPtr. The \p pitch field
        !* of \p pitchedDevPtr is the width in memory in bytes of the 3D array pointed
        !* to by \p pitchedDevPtr, including any padding added to the end of each row.
        !* The \p xsize field specifies the logical width of each row in bytes, while
       ! * the \p ysize field specifies the height of each 2D slice in rows.
    
    interface
        function cudaMemset3D(pitchedDevPtr,val,extent) result(status) &
                    bind(c,name='cudaMemset3D')
                 use, intrinsic :: ISO_C_BINDING
                 import :: cudaPitchedPtr
                 import :: cudaExtent
                 import :: CUDA_SUCCESS
                 type(cudaPitchedPtr),  value ::  pitchedDevPtr
                 integer(c_int),        value ::  val
                 type(cudaExtent),      value :: extent
                 integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemset3D
    end interface
    
    !*
        !* \brief Initializes or sets device memory to a value
        !*
        !* Fills the first \p count bytes of the memory area pointed to by \p devPtr
        !* with the constant byte value \p value.
        !*
        !* ::cudaMemsetAsync() is asynchronous with respect to the host, so
        !* the call may return before the memset is complete. The operation can optionally
        !* be associated to a stream by passing a non-zero \p stream argument.
        !* If \p stream is non-zero, the operation may overlap with operations in other streams.
        !*
        !* The device version of this function only handles device to device copies and
        !* cannot be given local or shared pointers.
        !*
        !* \param devPtr - Pointer to device memory
        !* \param value  - Value to set for each byte of specified memory
        !* \param count  - Size in bytes to set
       ! * \param stream - Stream identifier
    
    interface
        function cudaMemsetAsync(devPtr,val,count,stream) result(status) &
                    bind(c,name='cudaMemsetAsync')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUstream
                 import :: CUDA_SUCCESS
                 type(c_ptr),       value   :: devPtr
                 integer(c_int),    value   :: val
                 integer(c_size_t), value   :: count
                 type(CUstream),    value   :: stream
                 integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemsetAsync
    end interface
    
    !*
        !* \brief Initializes or sets device memory to a value
        !*
        !*! Sets to the specified value \p value a matrix (\p height rows of \p width
        !* bytes each) pointed to by \p dstPtr. \p pitch is the width in bytes of the
        !* 2D array pointed to by \p dstPtr, including any padding added to the end
        !* of each row. This function performs fastest when the pitch is one that has
        !* been passed back by ::cudaMallocPitch().
    
    interface
        function cudaMemset2DAsync(devPtr,pitch,val,width,height,stream) result(status) &
                    bind(c,name='cudaMemset2DAsync')
                  use, intrinsic :: ISO_C_BINDING
                  import :: CUstream
                  import :: CUDA_SUCCESS
                  type(c_ptr),       value   :: devPtr
                  integer(c_size_t), value  :: pitch
                  integer(c_int),    value  :: val
                  integer(c_size_t), value  :: width
                  integer(c_size_t), value  :: height
                  type(CUstream),    value  :: stream
                  integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemset2DAsync
    end interface
    
    !/**
        !* \brief Initializes or sets device memory to a value
        !*
        !* Initializes each element of a 3D array to the specified value \p value.
        !* The object to initialize is defined by \p pitchedDevPtr. The \p pitch field
        !* of \p pitchedDevPtr is the width in memory in bytes of the 3D array pointed
        !* to by \p pitchedDevPtr, including any padding added to the end of each row.
        !* The \p xsize field specifies the logical width of each row in bytes, while
       ! * the \p ysize field specifies the height of each 2D slice in rows.
    
    interface
        function cudaMemset3DAsync(pitchedDevPtr,val,extent,stream) result(status) &
                    bind(c,name='cudaMemset3DAsync')
                 use, intrinsic :: ISO_C_BINDING
                 import :: cudaPitchedPtr
                 import :: cudaExtent
                 import :: CUstream
                 import :: CUDA_SUCCESS
                 type(cudaPitchedPtr), value    :: pitchedDevPtr
                 integer(c_int),       value    :: val
                 type(cudaExtent),     value    :: extent
                 type(CUstream),       value    :: stream
                 integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemset3DAsync
    end interface
    
    !/**
        !* \brief Finds the address associated with a CUDA symbol
        !*
        !* Returns in \p *devPtr the address of symbol \p symbol on the device.
        !* \p symbol is a variable that resides in global or constant memory space.
       ! * If \p symbol cannot be found, or if \p symbol is not declared in the
        !* global or constant memory space, \p *devPtr is unchanged and the error
       ! * ::cudaErrorInvalidSymbol is returned.
    
    interface
        function cudaGetSymbolAddress(devPtr,symbol) result(status) &
                    bind(c,name='cudaGetSymbolAddress')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUDA_SUCCESS
                 type(c_ptr)        :: devPtr
                 type(c_ptr), value :: symbol
                 integer(kind(CUDA_SUCCESS))     :: status
        end function cudaGetSymbolAddress
    end interface
    
     !/**
        !* \brief Finds the size of the object associated with a CUDA symbol
        !*
        !* Returns in \p *size the size of symbol \p symbol. \p symbol is a variable that
        !* resides in global or constant memory space. If \p symbol cannot be found, or
        !* if \p symbol is not declared in global or constant memory space, \p *size is
        !* unchanged and the error ::cudaErrorInvalidSymbol is returned.
       ! *
       ! * \param size   - Size of object associated with symbol
        !* \param symbol - Device symbol address
    
    interface
        function cudaGetSymbolSize(size,symbol) result(status) &
                    bind(c,name='cudaGetSymbolSize')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                integer(c_size_t)       :: size
                type(c_ptr), value      :: symbol
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaGetSymbolSize
    end interface
    
    !/**
       ! * \brief Prefetches memory to the specified destination device
        !*
       ! * Prefetches memory to the specified destination device.  \p devPtr is the 
       ! * base device pointer of the memory to be prefetched and \p dstDevice is the 
       ! * destination device. \p count specifies the number of bytes to copy. \p stream
       ! * is the stream in which the operation is enqueued. The memory range must refer
       ! * to managed memory allocated via ::cudaMallocManaged or declared via __managed__ variables.
    
    interface
        function cudaMemPrefetchAsync(devPtr,count,dstDevice,stream) result(status) &
                    bind(c,name='cudaMemPrefetchAsync')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUstream
                 import :: CUDA_SUCCESS
                 type(c_ptr),       value   :: devPtr
                 integer(c_size_t), value   :: count
                 integer(c_int),    value   :: dstDevice
                 type(CUstream),    value   :: stream
                 integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemPrefetchAsync
    end interface
    
    !/**
        !* \brief Advise about the usage of a given memory range
        !*
        !!* Advise the Unified Memory subsystem about the usage pattern for the memory range
        !* starting at \p devPtr with a size of \p count bytes. The start address and end address of the memory
        !* range will be rounded down and rounded up respectively to be aligned to CPU page size before the
        !* advice is applied. The memory range must refer to managed memory allocated via ::cudaMallocManaged
        !* or declared via __managed__ variables.
    
    interface
        function cudaMemAdvise(devPtr,count,advice,device) result(status) &
                    bind(c,name='cudaMemAdvise')
                use, intrinsic :: ISO_C_BINDING
                import :: cudaMemAdviseSetReadMostly
                import :: CUDA_SUCCESS
                type(c_ptr),    value   :: devPtr
                integer(c_size_t), value    :: count
                integer(kind(cudaMemAdviseSetReadMostly)), value :: advice
                integer(c_int), value   :: device
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemAdvise
    end interface
    
    !/**
        !* \brief Query an attribute of a given memory range
        !*
        !* Query an attribute about the memory range starting at \p devPtr with a size of \p count bytes. The
        !* memory range must refer to managed memory allocated via ::cudaMallocManaged or declared via
        !* __managed__ variables.
    
    interface
        function cudaMemRangeGetAttribute(datum,dataSize,attribute,devPtr,count) result(status) &
                    bind(c,name='cudaMemRangeGetAttribute')
                use, intrinsic :: ISO_C_BINDING
                import ::  cudaMemRangeAttributeReadMostly 
                import :: CUDA_SUCCESS
                type(c_ptr),    value   :: datum
                integer(c_size_t),  value   :: dataSize
                integer(kind(cudaMemRangeAttributeReadMostly)), value   :: attribute
                type(c_ptr),   value    :: devPtr
                integer(c_size_t), value    :: count
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemRangeGetAttribute
    end interface
    
    !/**
        !* \brief Query attributes of a given memory range.
        !*
        !* Query attributes of the memory range starting at \p devPtr with a size of \p count bytes. The
       ! * memory range must refer to managed memory allocated via ::cudaMallocManaged or declared via
       ! * __managed__ variables. The \p attributes array will be interpreted to have \p numAttributes
       !! * entries. The \p dataSizes array will also be interpreted to have \p numAttributes entries.
       ! * The results of the query will be stored in \p data.
    
    interface
        function  cudaMemRangeGetAttributes(datum,dataSize,attributes,numAttributes,devPtr,count) result(status) &
                    bind(c,name='cudaMemRangeGetAttributes')
                 use, intrinsic :: ISO_C_BINDING
                 import :: CUDA_SUCCESS
                 type(c_ptr)        :: datum
                 integer(c_size_t), value   :: dataSize
                 integer(c_int), dimension(*) :: attributes
                 integer(c_size_t), value     :: numAttributes
                 type(c_ptr),       value     :: devPtr
                 integer(c_size_t), value     :: count
                 integer(kind(CUDA_SUCCESS))     :: status
        end function cudaMemRangeGetAttributes
    end interface
    
   ! /**
        !* \brief Queries if a device may directly access a peer device's memory.
        !*
        !* Returns in \p *canAccessPeer a value of 1 if device \p device is capable of
        !* directly accessing memory from \p peerDevice and 0 otherwise.  If direct
        !* access of \p peerDevice from \p device is possible, then access may be
        !* enabled by calling ::cudaDeviceEnablePeerAccess().
    
    interface
        function cudaDeviceCanAccessPeer(canAccessPeer,device,peerDevice) result(status) &
                    bind(c,name='cudaDeviceCanAccessPeer')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                integer(c_int)          :: canAccessPeer
                integer(c_int), value   :: device
                integer(c_int), value   :: peerDevice
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaDeviceCanAccessPeer
    end interface
    
   ! /**
         !* \brief Enables direct access to memory allocations on a peer device.
        !*
        !* On success, all allocations from \p peerDevice will immediately be accessible by
        !* the current device.  They will remain accessible until access is explicitly
        !* disabled using ::cudaDeviceDisablePeerAccess() or either device is reset using
        !* ::cudaDeviceReset().
    
    interface
        function cudaDeviceEnablePeerAccess(peerDevice,flags) result(status) &
                    bind(c,name='cudaDeviceEnablePeerAccess')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                integer(c_int), value   :: peerDevice
                integer(c_int), value   :: flags
                integer(kind(CUDA_SUCCESS))     :: status
        end function cudaDeviceEnablePeerAccess
    end interface
    
    !/**
       ! * \brief Disables direct access to memory allocations on a peer device.
       ! *
       ! * Returns ::cudaErrorPeerAccessNotEnabled if direct access to memory on
       ! * \p peerDevice has not yet been enabled from the current device.
       ! *
       ! * \param peerDevice - Peer device to disable direct access to
    
    interface
        function cudaDeviceDisablePeerAccess(peerDevice) result(status) &
                    bind(c,name='cudaDeviceDisablePeerAccess')
                use, intrinsic :: ISO_C_BINDING
                import :: CUDA_SUCCESS
                integer(c_int), value :: peerDevice
                 integer(kind(CUDA_SUCCESS))     :: status
        end function cudaDeviceDisablePeerAccess
    end interface
    
    
    
end module mod_cuda_runtime_api_header