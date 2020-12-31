

#ifndef __GMS_IPPS_C_WRAPPERS_H__
#define __GMS_IPPS_C_WRAPPERS_H__



/*
     Intel (R) IPPS C-wrappers Fortran callable interface.
*/


#include <stdint.h>
#include <stdbool.h>
#include <ipps.h>
#include <ipp.h>
#include <ippcore.h>


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



#endif /*__GMS_IPPS_C_WRAPPERS_H__*/
