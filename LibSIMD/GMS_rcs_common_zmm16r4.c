

#include "GMS_rcs_common_zmm16r4.h"
#include "GMS_complex_zmm16r4.h"
#include "GMS_simd_utils.h"
#include "GMS_sleefsimdsp.h"





                void k_zmm16r4(const __m512 mur,
                                  const __m512 mui
                                  const __m512 epsr,
                                  const __m512 epsi,
                                  const __m512 om,
                                  __m512 * __restrict kr,
                                  __m512 * __restrict ki) {

                        __m512 sqrr,sqri;
                        __m512 t0r,t0i;
                        __m512 wrkc = _mm512_setzero_ps();
                        cmul_zmm16r4(mur,mui,epsr,epsi,&t0r,&t0i);
                        csqrt_zmm16r4(t0r,t0i,&wrkc,&sqrr,&sqri);
                        *kr = _mm512_mul_ps(om,sqrr);
                        *ki = _mm512_mul_ps(om,sqri);
               }   
