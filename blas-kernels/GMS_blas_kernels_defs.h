
#ifndef __GMS_BLAS_KERNELS_DEFS_H__
#define __GMS_BLAS_KERNELS_DEFS_H__

#include <immintrin.h>

#if !defined(GMS_INTERLEAVE_SIMD_OPS_SCHEDULE)
    #define GMS_INTERLEAVE_SIMD_OPS_SCHEDULE 1
#endif

#if !defined(__ATTR_PURE__)
    #define __ATTR_PURE__ __attribute__((pure))
#endif

#if !defined(__ATTR_ALWAYS_INLINE__)
    #define __ATTR_ALWAYS_INLINE__ __attribute__((always_inline))
#endif

#if !defined(__ATTR_TCLONE_AVX_AVX512__)
     #define __ATTR_TCLONE_AVX_AVX512__ __attribute__ ((target_clones("avx","avx512")))
#endif

#if !defined(__ATTR_REGCALL__)
    #define __ATTR_REGCALL__ __attribute__((regcall))
#endif

#if !defined(__ATTR_VECTORCALL__)
    #define __ATTR_VECTORCALL__ __attribute__((vectorcall))
#endif

#if !defined(__ATTR_HOT__)
    #define  __ATTR_HOT__  __attribute__ ((hot))
#endif

#if !defined(__ATTR_COLD__)
    #define __ATTR_COLD__ __attribute__ ((cold))
#endif

#if !defined(__ATTR_ALIGN__)
    #define __ATTR_ALIGN__(n) __attribute__ ((aligned((n))))
#endif

#if !defined(__ATTR_TARGET_DEFAULT__)
    #define __ATTR_TARGET_DEFAULT __attribute__ ((target ("default")))
#endif

#if !defined(__ATTR_TARGET_SSE4__)
    #define  __ATTR_TARGET_SSE4__ __attribute__ ((target ("sse4")))
#endif

#if !defined(__ATTR_TARGET_AVX__)
    #define  __ATTR_TARGET_AVX__ __attribute__ ((target ("avx")))
#endif

#if !defined(__ATTR_TARGET_AVX2__)
    #define  __ATTR_TARGET_AVX2__ __attribute__ ((target ("avx2")))
#endif

#if !defined(__ATTR_TARGET_AVX512F__)
    #define  __ATTR_TARGET_AVX512F__ __attribute__ ((target ("avx512f")))
#endif

#if !defined(__ATTR_TARGET_CLDEMOTE__)
    #define __ATTR_TARGET_CLDEMOTE__ __attribute__ ((target ("cldemote")))
#endif

#if !defined(__ATTR_TARGET_FMA4__)
    #define __ATTR_TARGET_FMA4__   __attribute__ ((target ("fma4")))
#endif

#if !defined(__ATTR_TARGET_NO_FANCY_MATH_387__)
    #define __ATTR_TARGET_NO_FANCY_MATH_387__ __attribute__ ((target ("no-fancy-math-387")))
#endif


                  typedef union {

                      __m256 v;
                      __ATTR_ALIGN__(32) float f[8];
                  } ymm8r4_t;

                  typedef union {
                      
                       __m256d v;
                       __ATTR_ALIGN__(32) double f[4];
                  } ymm4r8_t;


                  typedef union {

                       __m512 v;
		       __ATTR_ALIGN__(64) float f[16];
		       
		   } zmm16r4_t;

		   typedef union {

		       __m512d v;
		       __ATTR_ALIGN__(64) double f[8];
		   } zmm8r8_t;









#endif /*__GMS_BLAS_KERNELS_DEFS_H__*/
