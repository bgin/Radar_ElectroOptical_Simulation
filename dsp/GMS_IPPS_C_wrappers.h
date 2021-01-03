

#ifndef __GMS_IPPS_C_WRAPPERS_H__
#define __GMS_IPPS_C_WRAPPERS_H__



/*
     Intel (R) IPPS C-wrappers Fortran callable interface.
*/


#include <stdint.h>
#include <stdbool.h>



void gms_ippGetLibVersion() __attribute__((cold));

Ipp32f *
gms_ippsMalloc_32f(int32_t len) __attribute__((cold)) 
                            __attribute__ ((malloc))
                            __attribute__ ((returns_nonnull))
                            __attribute__ ((assume_aligned(64)))
                            __attribute__ ((alloc_size(1)));


Ipp64f *
gms_ippsMalloc_32f(int32_t len) __attribute__((cold)) 
                            __attribute__ ((malloc))
                            __attribute__ ((returns_nonnull))
                            __attribute__ ((assume_aligned(64)))
                            __attribute__ ((alloc_size(1)));


Ipp32fc *
gms_ippsMalloc_32fc(int32_t len) __attribute__((cold)) 
                            __attribute__ ((malloc))
                            __attribute__ ((returns_nonnull))
                            __attribute__ ((assume_aligned(64)))
                            __attribute__ ((alloc_size(1)));

Ipp64fc *
gms_ippsMalloc_64fc(int32_t len) __attribute__((cold)) 
                            __attribute__ ((malloc))
                            __attribute__ ((returns_nonnull))
                            __attribute__ ((assume_aligned(64)))
                            __attribute__ ((alloc_size(1)));

IppStatus
gms_ippsCopy_32f(const Ipp32f , Ipp32f *, int32_t) __attribute__((cold))
                                                   __attribute__((nonnull (1, 2)))
						   __attribute__ ((assume_aligned(64)));
						   
IppStatus
gms_ippsCopy_64f(const Ipp64f , Ipp64f *, int32_t) __attribute__((cold))
                                                   __attribute__((nonnull (1, 2)))
                                                   __attribute__ ((assume_aligned(64)));


IppStatus
gms_ippsCopy_32fc(const Ipp32fc , Ipp32fc *, int32_t) __attribute__((cold))
                                                   __attribute__((nonnull (1, 2)))
                                                   __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsCopy_64fc(const Ipp64fc , Ipp64fc *, int32_t) __attribute__((cold))
                                                   __attribute__((nonnull (1, 2)))
                                                   __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsCopy_8u(const Ipp8u *, Ipp8u *, int32_t) __attribute__((cold))
                                                   __attribute__((nonnull (1, 2)))
                                                   __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsCopyLE_1u(const Ipp8u *, int32_t ,
                  Ipp8u * , int32_t , int32_t) __attribute__((cold))
                                                   __attribute__((nonnull (1, 2)))
                                                   __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsCopyBE_1u(const Ipp8u *, int32_t ,
                            Ipp8u * , int32_t, int32_t) __attribute__((cold))
                                                   __attribute__((nonnull (1, 2)))
                                                   __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsMove_8u(const Ipp8u *, Ipp8u *, int32_t) __attribute__((cold))
                                                   __attribute__((nonnull (1, 2)))
                                                   __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsMove_32f(const Ipp32f *, Ipp32f *, int32_t) __attribute__((cold))
                                                   __attribute__((nonnull (1, 2)))
                                                   __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsMove_64f(const Ipp64f *, Ipp64f *, int32_t) __attribute__((cold))
                                                   __attribute__((nonnull (1, 2)))
                                                   __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsMove_32fc(const Ipp32fc *, Ipp32fc *, int32_t) __attribute__((cold))
                                                   __attribute__((nonnull (1, 2)))
                                                   __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsMove_64fc(const Ipp64fc *, Ipp64fc *, int32_t)  __attribute__((cold))
                                                   __attribute__((nonnull (1, 2)))
                                                   __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippSet_8u(Ipp8u, Ipp8u *, int32_t)__attribute__((cold))
                                      __attribute__((nonnull (1)))
                                      __attribute__ ((assume_aligned(64)));


IppStatus
gms_ippSet_32f(Ipp32f, Ipp32f *, int32_t)__attribute__((cold))
                                      __attribute__((nonnull (1)))
                                      __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippSet_64f(Ipp64f, Ipp64f *, int32_t)__attribute__((cold))
                                      __attribute__((nonnull (1)))
                                      __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippSet_32fc(Ipp32fc, Ipp32fc *, int32_t)__attribute__((cold))
                                      __attribute__((nonnull (1)))
                                      __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippSet_64fc(Ipp64fc, Ipp64fc * , int32_t)__attribute__((cold))
                                      __attribute__((nonnull (1)))
                                      __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsZero_8u(Ipp8u * , int32_t) __attribute__((cold))
                                      __attribute__((nonnull (1)))
                                      __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsZero_32f(Ipp32f *, int32_t) __attribute__((cold))
                                      __attribute__((nonnull (1)))
                                      __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsZero_64f(Ipp64f *, int32_t)__attribute__((cold))
                                      __attribute__((nonnull (1)))
                                      __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsZero_32fc(Ipp32fc *, int32_t)__attribute__((cold))
                                      __attribute__((nonnull (1)))
                                      __attribute__ ((assume_aligned(64)));

IppStatus
gms_ippsZero_64fc(Ipp64fc *, int32_t)__attribute__((cold))
                                      __attribute__((nonnull (1)))
                                      __attribute__ ((assume_aligned(64)));

void
gms_ippsFree() __attribute__((cold));

bool
gms_ippGetCacheParams() __attribute__((cold));

Ipp64u
gms_ippGetCpuClocks() __attribute__((pure))
                      __attribute__((hot));

IppStatus
gms_ippGetCpuFreqMhz(int32_t *) __attribute__((cold));

IppStatus
gms_ippGetL2CacheSize(int32_t *) __attribute__((cold));

IppStatus
gms_ippGetCpuFeatures(Ipp64u *,
                      Ipp32u *) __attribute__((cold));

Ipp64u
gms_ippGetEnabledCpuFeatures() __attribute__((pure))
                               __attribute__((cold));

IppStatus
gms_ippGetMaxCacheSizeB(int32_t *) __attribute__((cold));

IppStatus
gms_ippSetCpuFeatures(Ipp64u) __attribute__((cold));

IppStatus
gms_ippSetFlushToZero(int32_t,
                      uint32_t *) __attribute__((cold));

IppStatus
gms_ippSetDenormAreZeros(int32_t) __attribute__((cold));

IppStatus
gms_ippSetNumThreads(int32_t) __attribute__((cold))

IppStatus
gms_ippGetNumThreads(int32_t *) __attribute__((cold));


IppStatus
gms_ippsTone_64fc(Ipp64fc *, int32_t , Ipp64f , Ipp64f ,
                           Ipp64f * , IppHintAlgorithm) __attribute__((hot))
                                                        __attribute__((nonnull (1, 2)))
                                                        __attribute__((assume_aligned(64)));

IppStatus
gms_ippsTone_32f(Ipp32f *, int32_t, Ipp32f , Ipp32f ,
                           Ipp32f *, IppHintAlgorithm) __attribute__((hot))
                                                        __attribute__((nonnull (1, 2)))
                                                        __attribute__((assume_aligned(64)));

IppStatus
gms_ippsTone_64f(Ipp64f * , int32_t, Ipp64f, Ipp64f,
                           Ipp64f *, IppHintAlgorithm) __attribute__((hot))
                                                        __attribute__((nonnull (1, 2)))
                                                        __attribute__((assume_aligned(64)));

IppStatus
gms_ippsTone_32fc(Ipp32fc *, int32_t, Ipp32f, Ipp32f ,
                           Ipp32f *, IppHintAlgorithm) __attribute__((hot))
                                                        __attribute__((nonnull (1, 2)))
                                                        __attribute__((assume_aligned(64)));

IppStatus
gms_ippsTriangle_32f(Ipp32f *, int32_t, Ipp32f, Ipp32f,
                               Ipp32f, Ipp32f *) __attribute__((hot))
                                                 __attribute__((nonnull (1, 2)))
                                                 __attribute__((assume_aligned(64)));

IppStatus
gms_ippsTriangle_64f(Ipp64f *, int32_t, Ipp64f, Ipp64f,
                               Ipp64f, Ipp64f *)__attribute__((hot))
                                                 __attribute__((nonnull (1, 2)))
                                                 __attribute__((assume_aligned(64)));

IppStatus
gms_ippsTriangle_32fc(Ipp32fc *, int32_t, Ipp32f, Ipp32f,
                               Ipp32f, Ipp32f *) __attribute__((hot))
                                                 __attribute__((nonnull (1, 2)))
                                                 __attribute__((assume_aligned(64)));

IppStatus
gms_ippsTriangle_64fc(Ipp64fc *, int32_t, Ipp64f, Ipp64f,
                               Ipp64f, Ipp64f *) __attribute__((hot))
                                                 __attribute__((nonnull (1, 2)))
                                                 __attribute__((assume_aligned(64)));

IppStatus
gms_ippsRandUniformInit_32f(IppsRandUniState_32f *, Ipp32f,
                                  Ipp32f, uint32_t)__attribute__((hot))
                                                 __attribute__((nonnull (1, 2)))
                                                 __attribute__((assume_aligned(64)));

IppStatus
gms_ippsRandUniformInit_64f(IppsRandUniState_64f *, Ipp64f ,
                            Ipp64f, uint32_t ) __attribute__((hot))
                                                 __attribute__((nonnull (1, 2)))
                                                 __attribute__((assume_aligned(64)));

IppStatus
gms_ippsRandUniformGetSize_32f(int *) __attribute__((hot));
                                     
                                    

IppStatus
gms_ippsRandUniformGetSize_64f(int *) __attribute__((hot));
                                     
                                    

IppStatus
gms_ippsRandUniform_32f(Ipp32f *, int32_t, IppsRandUniState_32f *) __attribute__((hot))
                                                 __attribute__((nonnull (1, 2)))
                                                 __attribute__((assume_aligned(64)));

IppStatus
gms_ippsRandUniform_64f(Ipp64f *, int32_t, IppsRandUniState_64f *)  __attribute__((hot))
                                                 __attribute__((nonnull (1, 2)))
                                                 __attribute__((assume_aligned(64)));

bool
vecf32_fill_ippsRandUniform_32f(Ipp32f * __restrict, int32_t,
                                          Ipp32f, Ipp32f) __attribute__((hot))
                                      __attribute__((nonnull (1)))
                                      __attribute__((assume_aligned(64)));

bool
vecf64_fill_ippsRandUniform_64f(Ipp64f * __restrict , int32_t,
                                          Ipp64f, Ipp64f )__attribute__((hot))
                                      __attribute__((nonnull (1)))
                                      __attribute__((assume_aligned(64)));

IppStatus
gms_ippsRandGaussInit_32f(IppsRandGaussState_32f *, Ipp32f,
                                    Ipp32f, uint32_t) __attribute__((hot))
                                      __attribute__((nonnull (1)))
                                      __attribute__((assume_aligned(64)));

IppStatus
gms_ippsRandGaussInit_64f(IppsRandGaussState_64f *, Ipp64f,
                                    Ipp64f, uint32_t) __attribute__((hot))
                                      __attribute__((nonnull (1)))
                                      __attribute__((assume_aligned(64)));

IppStatus
gms_ippsRandGaussGetSize_32f(int32_t *)__attribute__((hot));
                                      
                                    

IppStatus
gms_ippsRandGaussGetSize_64f(int32_t *)
                                      __attribute__((hot));

IppStatus
gms_ippsRandGauss_32f(Ipp32f *, int32_t,
                      IppsRandGaussState_32f *) __attribute__((hot))
                                                __attribute__((nonnull (1))
                                                __attribute__((assume_aligned(64)));

IppStatus
gms_ippsRandGauss_64f(Ipp64f *, int32_t,
                      IppsRandGaussState_64f *) __attribute__((hot))
                                                __attribute__((nonnull (1))
                                                __attribute__((assume_aligned(64)));

bool
gms_vecf32_fill_ippsRandGauss_f32(Ipp32f *, int32_t,
                                  Ipp32f, Ipp32f)__attribute__((hot))
                                                __attribute__((nonnull (1))
                                                __attribute__((assume_aligned(64)));

bool
gms_vecf64_fill_ippsRandGauss_f64(Ipp64f *, int32_t,
                                  Ipp64f,Ipp64f)__attribute__((hot))
                                                __attribute__((nonnull (1))
                                                __attribute__((assume_aligned(64)));

IppStatus
gms_ippsVectorJaehne_32f(Ipp32f *, int32_t, Ipp32f) __attribute__((hot))
                                                __attribute__((nonnull (1))
                                                __attribute__((assume_aligned(64)));

IppStatus
gms_ippsVectorJaehne_64f(Ipp64f *, int32_t, Ipp64f) __attribute__((hot))
                                                __attribute__((nonnull (1))
                                                __attribute__((assume_aligned(64)));

IppStatus
gms_ippsVectorSlope_32f(Ippf32 *, int32_t, Ipp32f, Ipp32f) __attribute__((hot))
                                                __attribute__((nonnull (1))
                                                __attribute__((assume_aligned(64)));

IppStatus
gms_ippsVectorSlope_64f(Ippf64 *, int32_t, Ipp64f, Ipp64f)__attribute__((hot))
                                                __attribute__((nonnull (1))
                                                __attribute__((assume_aligned(64)));

IppStatus
gms_ippsAddC_32f(const Ipp32f *, Ipp32f, Ipp32f *, int32_t)__attribute__((hot))
                                                __attribute__((nonnull (1,2))
                                                __attribute__((assume_aligned(64)));

        



IppStatus
gms_ippsAddC_64f(const Ipp64f *, Ipp64f, Ipp64f *, int32_t)__attribute__((hot))
                                                __attribute__((nonnull (1,2))
                                                __attribute__((assume_aligned(64)));

       



IppStatus
gms_ippsAddC_32fc(const Ipp32fc *, Ipp32fc, Ipp32fc *, int32_t)__attribute__((hot))
                                                __attribute__((nonnull (1,2))
                                                __attribute__((assume_aligned(64)));

         


IppStatus
gms_ippsAddC_64fc(const Ipp64fc *, Ipp64fc, Ipp64fc *, int32_t) __attribute__((hot))
                                                __attribute__((nonnull (1,2))
                                                __attribute__((assume_aligned(64)));



IppStatus
gms_ippsAddC_32f_I(Ipp32f, Ipp32f *, int32_t) __attribute__((hot))
                                                __attribute__((nonnull (1))
                                                __attribute__((assume_aligned(64)));




IppStatus gms_ippsAddC_64f_I(Ipp64f, Ipp64f *, int32_t)  __attribute__((hot))
                                                __attribute__((nonnull (1))
                                                __attribute__((assume_aligned(64)));



IppStatus
gms_ippsAddC_32fc_I(Ipp32fc, Ipp32fc *, int32_t) __attribute__((hot))
                                                __attribute__((nonnull (1))
                                                __attribute__((assume_aligned(64)));



IppStatus
gms_ippsAddC_64fc_I(Ipp64fc, Ipp64fc *, int32_t) __attribute__((hot))
                                                __attribute__((nonnull (1))
                                                __attribute__((assume_aligned(64)));


IppStatus
gms_ippsAdd_32f(const Ipp32f *, const Ipp32f *, Ipp32f *, int32_t)  __attribute__((hot))
                                                __attribute__((nonnull (1,2,3))
                                                __attribute__((assume_aligned(64)));



IppStatus
gms_ippsAdd_64f(const Ipp64f *, const Ipp64f *, Ipp64f *, int32_t) __attribute__((hot))
                                                __attribute__((nonnull (1,2,3))
                                                __attribute__((assume_aligned(64)));




IppStatus
gms_ippsAdd_32fc(const Ipp32fc *, const Ipp32fc *, Ipp32fc *, int32_t)  __attribute__((hot))
                                                __attribute__((nonnull (1,2,3))
                                                __attribute__((assume_aligned(64)));


IppStatus
gms_ippsAdd_64fc(const Ipp64fc *, const Ipp64fc *, Ipp64fc *, int32_t) __attribute__((hot))
                                                __attribute__((nonnull (1,2,3))
                                                __attribute__((assume_aligned(64)));



IppStatus
gms_ippsAdd_32f_I(const Ipp32f * pSrc, Ipp32f * pSrcDst, int32_t len)  __attribute__((hot))
                                                __attribute__((nonnull (1,2))
                                                __attribute__((assume_aligned(64)));



IppStatus
gms_ippsAdd_64f_I(const Ipp64f *, Ipp64f *, int32_t)  __attribute__((hot))
                                                __attribute__((nonnull (1,2))
                                                __attribute__((assume_aligned(64)));


IppStatus
gms_ippsAdd_32fc_I(const Ipp32fc *, Ipp32fc *, int32_t)__attribute__((hot))
                                                __attribute__((nonnull (1,2))
                                                __attribute__((assume_aligned(64)));

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsAdd_64fc_I(const Ipp64fc * pSrc, Ipp64fc * pSrcDst, int32_t len) {

          return (ippsAdd_64fc_I(pSrc,pSrcDst,len));
}

/*
        AddProductC
Adds product of a vector and a constant to the
accumulator vector
*/

	      
__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsAddProdC_32f(const Ipp32f * pSrc, const Ipp32f val, Ipp32f * pSrcDst, int32_t len) {

          return (ippsAddProdC_32f(pSrc,val,pSrcDst,len));
}


__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsAddProdC_64f(const Ipp64f * pSrc, const Ipp64f val, Ipp64f * pSrcDst, int32_t len) {

          return (ippsAddProdC_64f(pSrc,val,pSrcDst,len));
}


__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2,3))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsAddProduct_32f(const Ipp32f * pSrc1, const Ipp32f * pSrc2, Ipp32f * pSrcDst, int32_t len) {

          return (ippsAddProduct_32f(pSrc1,pSrc2,pSrcDst,len));
}


__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2,3))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsAddProduct_64f(const Ipp64f * pSrc1, const Ipp64f * pSrc2, Ipp64f * pSrcDst, int32_t len) {

          return (ippsAddProduct_64f(pSrc1,pSrc2,pSrcDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2,3))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsAddProduct_32fc(const Ipp32fc * pSrc1, const Ipp32fc * pSrc2, Ipp32fc * pSrcDst, int32_t len) {

          return (ippsAddProduct_32fc(pSrc1,pSrc2,pSrcDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2,3))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsAddProduct_64fc(const Ipp64fc * pSrc1, const Ipp64fc * pSrc2, Ipp64fc * pSrcDst, int32_t len) {

          return (ippsAddProduct_64fc(pSrc1,pSrc2,pSrcDst,len));
}

/*
     MulC
Multiplies each element of a vector by a constant
value.
*/

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMulC_32f(const Ipp32f * pSrc, Ipp32f val, Ipp32f * pDst, int32_t len) {

          return (ippsMulC_32f(pSrc,val,pDst,len));
}


__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMulC_64f(const Ipp64f * pSrc, Ipp64f val, Ipp64f * pDst, int32_t len) {

          return (ippsMulC_64f(pSrc,val,pDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMulC_32fc(const Ipp32fc * pSrc, Ipp32fc val, Ipp32fc * pDst, int32_t len) {

          return (ippsMulC_32fc(pSrc,val,pDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMulC_64fc(const Ipp64fc * pSrc, Ipp64fc val, Ipp64fc * pDst, int32_t len) {

          return (ippsMulC_64fc(pSrc,val,pDst,len));
}

/*
      In-place operations without scaling.
*/


__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMulC_32f_I(Ipp32f val, Ipp32f * pSrcDst, int32_t len) {
 
          return (ippsMulC_32f_I(val,pSrcDst,len));
}


__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMulC_64f_I(Ipp64f val, Ipp64f * pSrcDst, int32_t len) {
 
          return (ippsMulC_64f_I(val,pSrcDst,len));
}


__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMulC_32fc_I(Ipp32fc val, Ipp32fc * pSrcDst, int32_t len) {
 
          return (ippsMulC_32fc_I(val,pSrcDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMulC_64fc_I(Ipp64fc val, Ipp64fc * pSrcDst, int32_t len) {
 
          return (ippsMulC_64fc_I(val,pSrcDst,len));
}


/*
       Mul
Multiplies the elements of two vectors.
*/

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2,3))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMul_32f(const Ipp32f * pSrc1, const Ipp32f * pSrc2, Ipp32f * pDst, int32_t len) {

          return (ippsMul_32f(pSrc1,pSrc2,pDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2,3))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMul_64f(const Ipp64f * pSrc1, const Ipp64f * pSrc2, Ipp64f * pDst, int32_t len) {

          return (ippsMul_64f(pSrc1,pSrc2,pDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2,3))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMul_32fc(const Ipp32fc * pSrc1, const Ipp32fc * pSrc2, Ipp32fc * pDst, int32_t len) {

          return (ippsMul_32fc(pSrc1,pSrc2,pDst,len));
}


__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2,3))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMul_64fc(const Ipp64fc * pSrc1, const Ipp64fc * pSrc2, Ipp64fc * pDst, int32_t len) {

          return (ippsMul_64fc(pSrc1,pSrc2,pDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2,3))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMul_32f32fc(const Ipp32f * pSrc1, const Ipp32fc * pSrc2, Ipp32fc * pDst, int32_t len) {

          return (ippsMul_32f32fc(pSrc1,pSrc2,pDst,len));
}



/*
    In-place operations on floating point and integer data without scaling
*/

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMul_32f_I(const Ipp32f * pSrc, Ipp32f * pSrcDst, int32_t len) {
 
          return (ippsMul_32f_I(pSrc,pSrcDst,len));
}


__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMul_64f_I(const Ipp64f * pSrc, Ipp64f * pSrcDst, int32_t len) {
 
          return (ippsMul_64f_I(pSrc,pSrcDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMul_32fc_I(const Ipp32fc * pSrc, Ipp32fc * pSrcDst, int32_t len) {
 
          return (ippsMul_32fc_I(pSrc,pSrcDst,len));
}


__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsMul_64fc_I(const Ipp64fc * pSrc, Ipp64fc * pSrcDst, int32_t len) {
 
          return (ippsMul_64fc_I(pSrc,pSrcDst,len));
}


/*
   SubC
Subtracts a constant value from each element of a
vector.
*/

/*
     Not-in-place operations on floating point data.
*/


__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsSubC_32f(const Ipp32f * pSrc, Ipp32f val, Ipp32f * pDst, int32_t len) {
  
          return (ippsSubC_32f(pSrc,val,pDst,len));
}


__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsSubC_64f(const Ipp64f * pSrc, Ipp64f val, Ipp64f * pDst, int32_t len) {
  
          return (ippsSubC_64f(pSrc,val,pDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsSubC_32fc(const Ipp32fc * pSrc, Ipp32fc val, Ipp32fc * pDst, int32_t len) {
  
          return (ippsSubC_32fc(pSrc,val,pDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsSubC_64fc(const Ipp64fc * pSrc, Ipp64fc val, Ipp64fc * pDst, int32_t len) {
  
          return (ippsSubC_64fc(pSrc,val,pDst,len));
}

/*
     In-place operations on floating point data.
*/

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsSubC_32f_I(Ipp32f val, Ipp32f * pSrcDst, int32_t len) {

          return (ippsSubC_32f(val,pSrcDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsSubC_64f_I(Ipp64f val, Ipp64f * pSrcDst, int32_t len) {

          return (ippsSubC_64f(val,pSrcDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsSubC_32fc_I(Ipp32fc val, Ipp32fc * pSrcDst, int32_t len) {

          return (ippsSubC_32fc(val,pSrcDst,len));
}

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsSubC_64fc_I(Ipp64fc val, Ipp64fc * pSrcDst, int32_t len) {

          return (ippsSubC_64fc(val,pSrcDst,len));
}

/*
   SubCRev
Subtracts each element of a vector from a constant
value.
*/

__ATTR_ALWAYS_INLINE__
__ATTR_HOT__
__attribute__((nonnull (1,2))
__attribute__((assume_aligned(64)))
static inline
IppStatus gms_ippsSubCRev_32f(const Ipp32f * pSrc, Ipp32f val, Ipp32f * pDst, int32_t len) {

          return (ippsSubCRev_32f(pSrc,val,pDst,len));
}



IppStatus
gms_ippsSubCRev_64f(const Ipp64f * pSrc, Ipp64f val, Ipp64f * pDst, int32_t len)__attribute__((hot))
                                                                __attribute__((nonnull (1,2))
                                                               __attribute__((assume_aligned(64)));

         


IppStatus
gms_ippsSubCRev_32fc(const Ipp32fc *, Ipp32fc, Ipp32fc *, int32_t)__attribute__((hot))
                                                                __attribute__((nonnull (1,2))
                                                               __attribute__((assume_aligned(64)));




IppStatus
gms_ippsSubCRev_64fc(const Ipp64fc *, Ipp64fc, Ipp64fc *, int32_t)__attribute__((hot))
                                                                __attribute__((nonnull (1,2))
                                                               __attribute__((assume_aligned(64)));


/*
    In-place operations on floating point data.
*/



IppStatus
gms_ippsSubCRev_32f_I(Ipp32f, Ipp32f *, int32_t)__attribute__((hot))
                                                                __attribute__((nonnull (1))
                                                               __attribute__((assume_aligned(64)));



IppStatus
gms_ippsSubCRev_64f_I(Ipp64f, Ipp64f *, int32_t)__attribute__((hot))
                                                                __attribute__((nonnull (1))
                                                               __attribute__((assume_aligned(64)));



IppStatus
gms_ippsSubCRev_32fc_I(Ipp32fc, Ipp32fc *, int32_t) __attribute__((hot))
                                                                __attribute__((nonnull (1))
                                                               __attribute__((assume_aligned(64)));

        



IppStatus
gms_ippsSubCRev_64fc_I(Ipp64fc, Ipp64fc *, int32_t)  __attribute__((hot))
                                                                __attribute__((nonnull (1,2,3))
                                                               __attribute__((assume_aligned(64)));

/*
    Sub
Subtracts the elements of two vectors.
*/


IppStatus
gms_ippsSub_32f(const Ipp32f *, const Ipp32f *, Ipp32f *, int32_t)  __attribute__((hot))
                                                                __attribute__((nonnull (1,2,3))
                                                               __attribute__((assume_aligned(64)));

         


IppStatus
gms_ippsSub_64f(const Ipp64f *, const Ipp64f *, Ipp64f *, int32_t)  __attribute__((hot))
                                                                __attribute__((nonnull (1,2,3))
                                                               __attribute__((assume_aligned(64)));

          


IppStatus
gms_ippsSub_32fc(const Ipp32fc *, const Ipp32fc *, Ipp32fc *, int32_t)  __attribute__((hot))
                                                                __attribute__((nonnull (1,2,3))
                                                               __attribute__((assume_aligned(64)));

         


IppStatus
gms_ippsSub_64fc(const Ipp64fc *, const Ipp64fc *, Ipp64fc *, int32_t) __attribute__((hot))
                                                                __attribute__((nonnull (1,2,3))
                                                               __attribute__((assume_aligned(64)));

         

/*
    In-place operations on floating point data and integer data without scaling.
*/


IppStatus
gms_ippsSub_32f_I(const Ipp32f * pSrc, Ipp32f * pSrcDst, int32_t len) __attribute__((hot))
                                                                __attribute__((nonnull (1,2))
                                                               __attribute__((assume_aligned(64)));

         




IppStatus
gms_ippsSub_64f_I(const Ipp64f *, Ipp64f *, int32_t) __attribute__((hot))
                                                                __attribute__((nonnull (1,2))
                                                               __attribute__((assume_aligned(64)));

         


IppStatus
gms_ippsSub_32fc_I(const Ipp32fc *, Ipp32fc *, int32_t)  __attribute__((hot))
                                                                __attribute__((nonnull (1,2))
                                                               __attribute__((assume_aligned(64)));

       


IppStatus
gms_ippsSub_64fc_I(const Ipp64fc *, Ipp64fc *, int32_t) __attribute__((hot))
                                                                __attribute__((nonnull (1,2))
                                                               __attribute__((assume_aligned(64)));

       


/*
   DivC
Divides each element of a vector by a constant value
*/


IppStatus gms_ippsDivC_32f(const Ipp32f *, Ipp32f, Ipp32f *, int32_t) __attribute__((hot))
                                                                __attribute__((nonnull (1,2))
                                                               __attribute__((assume_aligned(64)));

        



IppStatus
gms_ippsDivC_64f(const Ipp64f *, Ipp64f, Ipp64f *, int32_t) __attribute__((hot))
                                                                __attribute__((nonnull (1,2))
                                                               __attribute__((assume_aligned(64)));

       

							       
IppStatus
gms_ippsDivC_32fc(const Ipp32fc *, Ipp32fc, Ipp32fc *, int32_t) __attribute__((hot))
                                                                __attribute__((nonnull (1,2))
                                                               __attribute__((assume_aligned(64)));

    
				

IppStatus
gms_ippsDivC_64fc(const Ipp64fc *, Ipp64fc, Ipp64fc *, int32_t) __attribute__((hot))
                                                                __attribute__((nonnull (1,2))
                                                                __attribute__((assume_aligned(64)));

          

IppStatus
gms_ippsDivC_32f_I(Ipp32f, Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsDivC_64f_I(Ipp64f, Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

					      
IppStatus
gms_ippsDivC_32fc_I(Ipp32fc, Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsDivC_64fc_I(Ipp64fc, Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsDivCRev_32f(const Ipp32f*, Ipp32f, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsDivCRev_32f_I( Ipp32f, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsDiv_32f(const Ipp32f*, const Ipp32f*, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsDiv_64f(const Ipp64f*, const Ipp64f*, Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsDiv_32fc(const Ipp32fc*, const Ipp32fc*, Ipp32fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsDiv_64fc(const Ipp64fc*, const Ipp64fc*, Ipp64fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsDiv_32f_I(const Ipp32f*, Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsDiv_64f_I(const Ipp64f*, Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsDiv_32fc_I(const Ipp32fc*, Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsDiv_64fc_I(const Ipp64fc*, Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsAbs_32f(const Ipp32f*, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsAbs_64f(const Ipp64f*, Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsAbs_32f_I(Ipp32f*, int32_t)  __attribute__((hot))
                                     __attribute__((nonnull (1))
                                     __attribute__((assume_aligned(64)));

IppStatus
gms_ippsAbs_64f_I(Ipp64f*, int32_t)  __attribute__((hot))
                                     __attribute__((nonnull (1))
                                     __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSqr_32f(const Ipp32f*, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSqr_64f(const Ipp64f*, Ipp364*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSqr_32fc(const Ipp32fc*, Ipp32fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSqr_64cf(const Ipp64fc*, Ipp64fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus					      
gms_ippsSqr_32f_I(Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus					      
gms_ippsSqr_64f_I(Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus					      
gms_ippsSqr_32fc_I(Ipp32fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus					      
gms_ippsSqr_64fc_I(Ipp64fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus			
gms_ippsSqrt_32f(const Ipp32f*, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus			
gms_ippsSqrt_64f(const Ipp64f*, Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus			
gms_ippsSqrt_32fc(const Ipp32fc*, Ipp32fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus			
gms_ippsSqrt_64fc(const Ipp64fc*, Ipp64fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSqrt_32f_I(Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSqrt_64f_I(Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSqrt_32fc_I(Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSqrt_64fc_I(Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsCubrt_32f(const Ipp32f*, Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsExp_32f(const Ipp32f*, Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsExp_64f(const Ipp64f*, Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsExp_32f_I(Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsExp_64f_I(Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsLn_32f(const Ipp32f*, Ipp32f*, int32_t)__attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsLn_64f(const Ipp64f*, Ipp64f*, int32_t)__attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsLn_32f_I(Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsLn_64f_I(Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSumLn_32f(const Ipp32f*, int32_t, Ipp32f*) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSumLn_64f(const Ipp64f*, int32_t, Ipp64f*) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSumLn_32f64f(const Ipp32f*, int32_t, Ipp64f*) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsArctan_32f(const Ipp32f*, Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsArctan_64f(const Ipp64f*, Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsArctan_32f_I(Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsArctan_64f_I(Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsNormalize_32f(const Ipp32f*, Ipp32f*, int32_t,
                      Ipp32f, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsNormalize_64f(const Ipp64f*, Ipp64f*, int32_t,
                      Ipp64f, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsNormalize_32fc(const Ipp32fc*, Ipp32fc*, int32_t,
                      Ipp32fc, Ipp32fc)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsNormalize_64fc(const Ipp64fc*, Ipp64fc*, int32_t,
                      Ipp64fc, Ipp64fc)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsNormalize_32f_I(Ipp32f*, int32_t, Ipp32f, Ipp32f) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsNormalize_64f_I(Ipp64f*, int32_t, Ipp64f, Ipp64f) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsNormalize_32fc_I(Ipp32fc*, int32_t, Ipp32fc, Ipp32fc) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsNormalize_64fc_I(Ipp64fc*, int32_t, Ipp64fc, Ipp64fc) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSortAscend_32f_I(Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSortAscend_64f_I(Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSortDescend_32f_I(Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSortDescend_64f_I(Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsConvert32f64f(const Ipp32f*, Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsConvert64f32f(const Ipp64f*, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsConj_32fc(const Ipp32fc*, Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsConj_64fc(const Ipp64fc*, Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsConj_32fc_I(Ipp32fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsConj_64fc_I(Ipp64fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsConjFlip_32fc(const Ipp32fc * ,Ipp32fc* , int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsConjFlip_64fc(const Ipp64fc * ,Ipp64fc* , int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMagnitude_32f(const Ipp32f*, const Ipp32f*, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMagnitude_64f(const Ipp64f*, const Ipp64f*, Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMagnitude_32fc(const Ipp32fc*, Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMagnitude_64fc(const Ipp64fc*, Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsPhase_64fc(const Ipp64fc*, Ipp64fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsPhase_32fc(const Ipp32fc*, Ipp32fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippPhase_32f(const Ipp32f*, const Ipp32f*, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippPhase_64f(const Ipp64f*, const Ipp64f*, Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsPowerSpectrum_32fc(const Ipp32fc*, Ipp32fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsPowerSpectrum_64fc(const Ipp64fc*, Ipp64fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsPowerSpectrum_32f(const Ipp32f*, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsPowerSpectrum_64f(const Ipp64f*, Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsReal_32fc(const Ipp32fc*, Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

					      
IppStatus
gms_ippsReal_64fc(const Ipp64fc*, Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsImag_32fc(const Ipp32fc*, Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

					      
IppStatus
gms_ippsImag_64fc(const Ipp64fc*, Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsRealToCplx_32f(const Ipp32f*, const Ipp32f*, Ipp32fc*, int32_t)__attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsRealToCplx_64f(const Ipp64f*, const Ipp64f*, Ipp64fc*, int32_t)__attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsCplxToReal_32f(const Ipp32fc*, Ipp32f*, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsCplxToReal_64f(const Ipp64fc*, Ipp64f*, Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_32f(const Ipp32f*, Ipp32f*, int32_t, Ipp32f,
                      IppCompOp)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_64f(const Ipp64f*, Ipp64f*, int32_t, Ipp64f,
                      IppCompOp)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_32fc(const Ipp32fc*, Ipp32fc*, int32_t, Ipp32f,
                      IppCompOp)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_64fc(const Ipp64fc*, Ipp64fc*, int32_t, Ipp64f,
                      IppCompOp)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LT_32f(const Ipp32f*, Ipp32f*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LT_64f(const Ipp64f*, Ipp64f*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LT_32fc(const Ipp32fc*, Ipp32fc*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LT_64fc(const Ipp64fc*, Ipp64fc*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GT_32f(const Ipp32f*, Ipp32f*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GT_64f(const Ipp64f*, Ipp64f*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GT_32fc(const Ipp32fc*, Ipp32fc*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GT_64fc(const Ipp64fc*, Ipp64fc*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GT_32f_I(Ipp32f*, int32_t Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GT_64f_I(Ipp64f*, int32_t Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GT_32fc_I(Ipp32fc*, int32_t Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GT_64fc_I(Ipp64fc*, int32_t Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LT_32f_I(Ipp32f*, int32_t Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LT_64f_I(Ipp64f*, int32_t Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LT_32fc_I(Ipp32fc*, int32_t Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LT_64fc_I(Ipp64fc*, int32_t Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsLTAbs_32f(const Ipp32f*, Ipp32f*, int32_t len, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsLTAbs_64f(const Ipp64f*, Ipp64f*, int32_t len, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsGTAbs_32f(const Ipp32f*, Ipp32f*, int32_t len, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsGTAbs_64f(const Ipp64f*, Ipp64f*, int32_t len, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsGTAbs_32f_I(Ipp32f*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsGTAbs_64f_I(Ipp64f*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsLTAbs_32f_I(Ipp32f*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsLTAbs_64f_I(Ipp64f*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTAbsVal_32f(const Ipp32f*, Ipp32f*, int32_t,
                                Ipp32f, Ipp32f) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTAbsVal_64f(const Ipp64f*, Ipp64f*, int32_t,
                                Ipp64f, Ipp64f) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTAbsVal_32f_I(Ipp32f*, int32_t, Ipp32f, Ipp32f) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTAbsVal_64f_I(Ipp64f*, int32_t, Ipp64f, Ipp64f) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTVal_32f(const Ipp32f*, Ipp32f*, int32_t,
                            Ipp32f, Ipp32f) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTVal_64f(const Ipp64f*, Ipp64f*, int32_t,
                            Ipp64f, Ipp64f) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsThreshold_LTVal_32fc(const Ipp32fc*, Ipp32fc*, int32_t,
                            Ipp32f, Ipp32f) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTVal_64fc(const Ipp64fc*, Ipp64fc*, int32_t,
                            Ipp64f, Ipp64f) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GTVal_32fc(const Ipp32fc*, Ipp32fc*, int32_t,
                            Ipp32f, Ipp32f) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GTVal_64fc(const Ipp64fc*, Ipp64fc*, int32_t,
                            Ipp64f, Ipp64f) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTVal_32f_I(Ipp32f*, int32_t, Ipp32f, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTVal_64f_I(Ipp64f*, int32_t, Ipp64f, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsThreshold_LTVal_32fc_I(Ipp32fc*, int32_t, Ipp32f, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTVal_64fc_I(Ipp64fc*, int32_t, Ipp64f, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GTVal_32f_I(Ipp32f*, int32_t, Ipp32f, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GTVal_64f_I(Ipp64f*, int32_t, Ipp64f, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsThreshold_GTVal_32fc_I(Ipp32fc*, int32_t, Ipp32f, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_GTVal_64fc_I(Ipp64fc*, int32_t, Ipp64f, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTInv_32f(const Ipp32f*, Ipp32f*, int32_t, Ipp32f) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTInv_64f(const Ipp64f*, Ipp64f*, int32_t, Ipp64f) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

					      
IppStatus
gms_ippsThreshold_LTInv_32fc(const Ipp32fc*, Ipp32fc*, int32_t, Ipp32f) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTInv_64fc(const Ipp64fc*, Ipp64fc*, int32_t, Ipp64f) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTInv_32f_I(Ipp32f*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTInv_64f_I(Ipp64f*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsThreshold_LTInv_32fc_I(Ipp32fc*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsThreshold_LTInv_64fc_I(Ipp64fc*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsCartToPolar_32f(const Ipp32f*, const Ipp32f*, Ipp32f*,
                        Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2,3,4))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsCartToPolar_64f(const Ipp64f*, const Ipp64f*, Ipp64f*,
                        Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2,3,4))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsCartToPolar_32fc(const Ipp32fc*, Ipp32f*, Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsCartToPolar_64fc(const Ipp64fc*, Ipp64f*, Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsPolarToCart_32f(const Ipp32f*, const Ipp32f*, Ipp32f*,
                        Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2,3,4))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsPolarToCart_64f(const Ipp64f*, const Ipp64f*, Ipp64f*,
                        Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2,3,4))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsPolarToCart_32fc(const Ipp32f*, const Ipp32f*, Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsPolarToCart_64fc(const Ipp64f*, const Ipp64f*, Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMaxOrder_32f(const Ipp32f*, int32_t, int32_t*) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMaxOrder_64f(const Ipp64f*, int32_t, int32_t*) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsFlip_32f(const Ipp32f*, Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsFlip_64f(const Ipp64f*, Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsFlip_32fc(const Ipp32fc*, Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsFlip_64fc(const Ipp64fc*, Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsFlip_32f_I(Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsFlip_64f_I(Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsFlip_32fc_I(Ipp32fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsFlip_64fc_I(Ipp64fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBartlett_32f(const Ipp32f*, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBartlett_64f(const Ipp64f*, Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBartlett_32fc(const Ipp32fc*, Ipp32fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBartlett_64fc(const Ipp64fc*, Ipp64fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBartlett_32f_I(Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBartlett_64f_I(Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBartlett_32fc_I(Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBartlett_64fc_I(Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackman_32f(const Ipp32f*, Ipp32f*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsWinBlackman_64f(const Ipp64f*, Ipp64f*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsWinBlackman_32fc(const Ipp32fc*, Ipp32fc*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsWinBlackman_64fc(const Ipp64fc*, Ipp64fc*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanStd_32f(const Ipp32f*, Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanStd_32fc(const Ipp32fc*, Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanStd_64f(const Ipp64f*, Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanStd_64fc(const Ipp64fc*, Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanOpt_32f(const Ipp32f*, Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanOpt_32fc(const Ipp32fc*, Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanOpt_64f(const Ipp64f*, Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanOpt_64fc(const Ipp64fc*, Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackman_32f_I(Ipp32f*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackman_32cf_I(Ipp32fc*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackman_64f_I(Ipp64f*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackman_64fc_I(Ipp64fc*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanOpt_32f_I(Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanOpt_32cf_I(Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanOpt_64f_I(Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanOpt_64fc_I(Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanStd_32f_I(Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanStd_32cf_I(Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanStd_64f_I(Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinBlackmanStd_64fc_I(Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinHammimg_32f(const Ipp32f*, Ipp32f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinHammimg_64f(const Ipp64f*, Ipp64f*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinHammimg_32fc(const Ipp32fc*, Ipp32fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinHammimg_64fc(const Ipp64fc*, Ipp64fc*, int32_t) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinHamming_32f_I(Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinHamming_64f_I(Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinHamming_32cf_I(Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinHamming_64fc_I(Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinHann_32f(const Ipp32f*, Ipp32f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsWinHann_64f(const Ipp64f*, Ipp64f*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinHann_32fc(const Ipp32fc*, Ipp32fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinHann_64fc(const Ipp64fc*, Ipp64fc*, int32_t)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinKaiser_32f(const Ipp32f*, Ipp32f*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinKaiser_64f(const Ipp64f*, Ipp64f*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinKaiser_32fc(const Ipp32fc*, Ipp32fc*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinKaiser_64fc(const Ipp64fc*, Ipp64fc*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinKaiser_32f_I(Ipp32f*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinKaiser_64f_I(Ipp64f*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinKaiser_32fc_I(Ipp32fc*, int32_t, Ipp32f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsWinKaiser_64fc_I(Ipp64fc*, int32_t, Ipp64f)  __attribute__((hot))
                                              __attribute__((nonnull (1))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSum_32f(const Ipp32f*, int32_t, Ipp32f*, IppHintAlgorithm) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSum_64f(const Ipp64f*, int32_t, Ipp64f*, IppHintAlgorithm) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSum_32fc(const Ipp32fc*, int32_t, Ipp32fc*, IppHintAlgorithm) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsSum_64fc(const Ipp64fc*, int32_t, Ipp64fc*, IppHintAlgorithm) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMax_32f(const Ipp32f*, int32_t, Ipp32f*)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMax_64f(const Ipp64f*, int32_t, Ipp64f*)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMaxIndx_32f(const Ipp32f*, int32_t, Ipp32f*, int32_t*) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMaxIndx_64f(const Ipp64f*, int32_t, Ipp64f*, int32_t*) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMaxAbs_32f(const Ipp32f*, int32_t, Ipp32f*)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMaxAbs_64f(const Ipp64f*, int32_t, Ipp64f*)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMaxAbsIndx_32f(const Ipp32f*, int32_t, Ipp32f*, int32_t*) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMaxAbsIndx_64f(const Ipp64f*, int32_t, Ipp64f*, int32_t*) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMin_32f(const Ipp32f*, int32_t, Ipp32f*) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMin_64f(const Ipp64f*, int32_t, Ipp64f*) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMinIndx_32f(const Ipp32f*, int32_t, Ipp32f*, int32_t*) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));


IppStatus
gms_ippsMinIndx_64f(const Ipp64f*, int32_t, Ipp64f*, int32_t*) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMinAbs_32f(const Ipp32f*, int32_t, Ipp32f*) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMinAbs_64f(const Ipp64f*, int32_t, Ipp64f*) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMinAbsIndx_32f(const Ipp32f*, int32_t, Ipp32f*, int32_t*) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMinAbsIndx_64f(const Ipp64f*, int32_t, Ipp64f*, int32_t*) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMinMax_32f(const Ipp32f*, int32_t, Ipp32f*, Ipp32f*)  __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMinMax_64f(const Ipp64f*, int32_t, Ipp64f*, Ipp64f*)  __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMinMaxIndx_32f(const Ipp32f*, int32_t, Ipp32f*, int32_t*,
                       Ipp32f*, int32_t*)   __attribute__((hot))
                                              __attribute__((nonnull (1,2,3,4,5))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMinMaxIndx_64f(const Ipp64f*, int32_t, Ipp64f*, int32_t*,
                       Ipp64f*, int32_t*)   __attribute__((hot))
                                              __attribute__((nonnull (1,2,3,4,5))
                                              __attribute__((assume_aligned(64)));
IppStatus
gms_ippsMean_32f(const Ipp32f*, int32_t, Ipp32f*, IppHintAlgorithm) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMean_32fc(const Ipp32fc*, int32_t, Ipp32fc*, IppHintAlgorithm) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMean_64f(const Ipp64f*, int32_t, Ipp64f*, IppHintAlgorithm) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMean_64fc(const Ipp64fc*, int32_t, Ipp64fc*, IppHintAlgorithm) __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsStdDev_32f(const Ipp32f*, int32_t, Ipp32f*, IppHintAlgorithm)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsStdDev_32f(const Ipp32f*, int32_t, Ipp32f*)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMeanStdDev_32f(const Ipp32f*, int32_t, Ipp32f*, Ipp32f*,
                       IppHintAlgorithm) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsMeanStdDev_64f(const Ipp64f*, int32_t, Ipp64f*, Ipp64f*,
                       IppHintAlgorithm) __attribute__((hot))
                                              __attribute__((nonnull (1,2,3))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsNorm_Inf_32f(const Ipp32f*, int32_t, Ipp32f*)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsNorm_Inf_64f(const Ipp64f*, int32_t, Ipp64f*)  __attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsNorm_Inf_32fc32f(const Ipp32fc*, int32_t, Ipp32f*)__attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

IppStatus
gms_ippsNorm_Inf_64fc64f(const Ipp64fc*, int32_t, Ipp64f*)__attribute__((hot))
                                              __attribute__((nonnull (1,2))
                                              __attribute__((assume_aligned(64)));

#endif /*__GMS_IPPS_C_WRAPPERS_H__*/
