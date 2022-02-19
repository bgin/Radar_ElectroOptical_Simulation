
#ifndef __GMS_CUDA_MEMOPS_CUH__
#define __GMS_CUDA_MEMOPS_CUH__




#include <stddef.h>
#include <cstdint>
#include "GMS_gpu_config.cuh"











//
// Copy int32_t array  (linearized) from CPU to GPU.
//
extern "C"
void copy_int32_cpu_to_gpu(int32_t * __restrict, 
                           int32_t * __restrict, 
			   const size_t, 
                           int32_t * 

                )   __attribute__((cold))
		                         __attribute__ ((alloc_size(1)))
				         __attribute__ ((malloc))
					 __attribute__ ((returns_nonnull));



//
// Copy float array  (linearized) from CPU to GPU
//
extern "C"
void copy_float_cpu_to_gpu(float * __restrict, 
                           float * __restrict, 
                           const size_t, 
                           int32_t * 

)   __attribute__((cold))
		                         __attribute__ ((alloc_size(1)))
				         __attribute__ ((malloc))
					 __attribute__ ((returns_nonnull));


//
// Copy double array  (linearized) from CPU to GPU.
//
extern "C"
void copy_double_cpu_to_gpu(double * __restrict, 
                            double * __restrict,
			    const size_t, 
                            int32_t *

 )  __attribute__((cold))
		                         __attribute__ ((alloc_size(1)))
				         __attribute__ ((malloc))
					 __attribute__ ((returns_nonnull));
//
// Copy cuComplex array from CPU to GPU
//
extern "C"
void copy_complex4_cpu_to_gpu(cuComplex * __restrict,
                              cuComplex * __restrict,
                              const size_t,
                              int32_t *

) __attribute__((cold))
		                         __attribute__ ((alloc_size(1)))
				         __attribute__ ((malloc))
					 __attribute__ ((returns_nonnull));

//
// Allocate array  of type int32_t on GPU.
//
extern "C"
void alloc_int32_gpu(int32_t * __restrict, 
                     const size_t, 
                     int32_t * 

)   __attribute__((cold))
		                   __attribute__ ((alloc_size(1)))
				   __attribute__ ((malloc))
		                   __attribute__ ((returns_nonnull));



//
// Allocate array  of type float on GPU.
//
extern "C"
void alloc_float_gpu(float * __restrict, 
                     const size_t, 
                     int32_t * 

)         __attribute__((cold))
		                         __attribute__ ((alloc_size(1)))
				         __attribute__ ((malloc))
					 __attribute__ ((returns_nonnull));


//
// Allocate array of type double on GPU.
//
extern "C"
void alloc_double_gpu(double * __restrict, 
                      const size_t, 
                      int32_t * 

)   __attribute__((cold))
		                    __attribute__ ((alloc_size(1)))
				    __attribute__ ((malloc))
				    __attribute__ ((returns_nonnull));

//
// Allocate float complex array on the GPU
//
extern "C"
void alloc_complex4_gpu(cuComplex * __restrict,
                        const size_t,
                        int32_t *

)   __attribute__((cold))
		                    __attribute__ ((alloc_size(1)))
				    __attribute__ ((malloc))
				    __attribute__ ((returns_nonnull));

//
// GPU to CPU memory copy routines
//

//
// Copy array  of int32_t from GPU to CPU.
//
extern "C"
void copy1D_int32_gpu_to_cpu(int32_t * __restrict, 
                             int32_t * __restrict,
			     const size_t, 
                             int32_t * 

) __attribute__((cold))
		                         __attribute__ ((alloc_size(1)))
				         __attribute__ ((malloc))
					 __attribute__ ((returns_nonnull));



//
// Copy array  of type float from GPU to CPU.
//
extern "C"
void copy_float_gpu_to_cpu(float * __restrict, 
                           float * __restrict, 
			   const size_t, 
                           int32_t * 

) __attribute__((cold))
		                       __attribute__ ((alloc_size(1)))
				       __attribute__ ((malloc))
				       __attribute__ ((returns_nonnull));



//
// Copy array  of type double from GPU to CPU.
//
extern "C"
void copy_double_gpu_to_cpu(double * __restrict, 
                            double * __restrict,
			    const size_t, 
                            int32_t * 

)  __attribute__((cold))
		                         __attribute__ ((alloc_size(1)))
				         __attribute__ ((malloc))
					 __attribute__ ((returns_nonnull));




//
// Copy array of type cuComplex from GPU to CPU
//
extern "C"
void copy_complex4_gpu_to_cpu(cuComplex * __restrict,
                              cuComplex * __restrict,
                              const size_t,
                              int32_t

)   __attribute__((cold))
		                         __attribute__ ((alloc_size(1)))
				         __attribute__ ((malloc))
					 __attribute__ ((returns_nonnull));


//
// Cuda memset wrappers.
//
extern "C"
void gpu_int32_memset(int32_t * __restrict,
                      int32_t,
                      size_t,
                      int32_t * __restrict) __attribute__((cold));

extern "C"
void gpu_float_memset(float * __restrict,
                      int32_t,
                      size_t,
                      int32_t * __restrict) __attribute__((cold));

extern "C"
void gpu_double_memset(double * __restrict,
                       int32_t,
                       size_t,
                       int32_t * __restrict) __attribute__((cold));

extern "C"
void gpu_complex4_memset(cuComplex * __restrict,
                         int32_t,
                         size_t,
                         int32_t * __restrict) __attribute__((cold));


//
// Cuda host allocation device accessible.
//
extern "C"
void host_alloc_int32(int32_t * __restrict,
                      size_t,
                      uint32_t,
                      int32_t * __restrict)__attribute__((cold))
		                         __attribute__ ((alloc_size(1)))
				         __attribute__ ((malloc))
					 __attribute__ ((returns_nonnull));


extern "C"
void host_alloc_float(float * __restrict,
                      size_t,
                      uint32_t,
                      int32_t * __restrict)__attribute__((cold))
		                         __attribute__ ((alloc_size(1)))
				         __attribute__ ((malloc))
					 __attribute__ ((returns_nonnull));


extern "C"
void host_alloc_double(double * __restrict,
                      size_t,
                      uint32_t,
                      int32_t * __restrict)__attribute__((cold))
		                         __attribute__ ((alloc_size(1)))
				         __attribute__ ((malloc))
					 __attribute__ ((returns_nonnull));


extern "C"
void host_alloc_complex4(cuComplex * __restrict,
                         size_t,
                         uint32_t,
                         int32_t * __restrict)__attribute__((cold))
		                         __attribute__ ((alloc_size(1)))
				         __attribute__ ((malloc))
					 __attribute__ ((returns_nonnull));







#endif /*__GMS_CUDA_MEMOPS_CUH__*/
