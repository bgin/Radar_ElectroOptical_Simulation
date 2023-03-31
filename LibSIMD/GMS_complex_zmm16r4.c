
#include "GMS_complex_zmm16r4.h"


                    void cadd_zmm16r4_u(const float * __restrict xre,
                                       const float * __restrict xim,
                                       const float * __restrict yre,
                                       const float * __restrict yim,
                                       float *       __restrict zre,
                                       float *       __restrict zim)  {

                        register __m512 zmm0,zmm1,zmm2,zmm3;
                        zmm0  = _mm512_loadu_ps(&xre[0]);
                        zmm1  = _mm512_loadu_ps(&yre[0]);
                        _mm512_storeu_ps(&zre[0], _mm512_add_ps(zmm0,zmm1));
                        zmm2  = _mm512_loadu_ps(&xim[0]);
                        zmm3  = _mm512_loadu_ps(&yim[0]);
                        _mm512_storeu_ps(&zim[0], _mm512_add_ps(zmm2,zmm3)); 
                   }


                    void cadd_zmm16r4_a(const float * __restrict __attribute__((aligned(64))) xre,
                                       const float * __restrict __attribute__((aligned(64))) xim,
                                       const float * __restrict __attribute__((aligned(64))) yre,
                                       const float * __restrict __attribute__((aligned(64))) yim,
                                       float *       __restrict __attribute__((aligned(64))) zre,
                                       float *       __restrict __attribute__((aligned(64))) zim) {

                        register __m512 zmm0,zmm1,zmm2,zmm3;
                        zmm0  = _mm512_load_ps(&xre[0]);
                        zmm1  = _mm512_load_ps(&yre[0]);
                        _mm512_store_ps(&zre[0], _mm512_add_ps(zmm0,zmm1));
                        zmm2  = _mm512_load_ps(&xim[0]);
                        zmm3  = _mm512_load_ps(&yim[0]);
                        _mm512_store_ps(&zim[0], _mm512_add_ps(zmm2,zmm3));  
                   }


                  void cadd_zmm16r4( const __m512 xre,
                                     const __m512 xim,
                                     const __m512 yre,
                                     const __m512 yim,
                                     __m512 * __restrict zre,
                                     __m512 * __restrict zim) {

                        register __m512 zmm0,zmm1;
                        zmm0  = _mm512_add_ps(xre,yre);
                        *zre  = zmm0;
                        zmm1  = _mm512_add_ps(xim,yim);
                        *zim  = zmm1; 
                  }
