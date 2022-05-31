

#include "GMS_coord_conv_avx512pd.h"
#if (COORD_CONV__AVX512PD_SLEEF_LIB) == 1
#include "GMS_sleefsimddp.hpp"
#endif
#include "GMS_simd_utils.hpp"


void ENU_axis_dir_zmm8r8(__m512d * __restrict __attribute__((aligned(64))) u_0, //2-element array of __m512d
                         __m512d * __restrict __attribute__((aligned(64))) u_1, //2-element array of __m512d
                         __m512d * __restrict __attribute__((aligned(64))) u_2, //2-element array of __m512d
                         const __m512d lat,
                         const __m512d lon) {

#if (COORD_CONV__AVX512PD_SLEEF_LIB) == 1
                         const __m512d sinp = xsin(lat);
                         const __m512d cosp = xcos(lat);
                         const __m512d sinl = xsin(lon);
                         const __m512d cosl = xcos(lon);
#else
                         const __m512d sinp = _mm512_sin_pd(lat);
                         const __m512d cosp = _mm512_cos_pd(lat);
                         const __m512d sinl = _mm512_sin_pd(lon);
                         const __m512d cosl = _mm512_cos_pd(lon);
#endif
                         const __m512d nsinl = zmm8r8_negate(sinl);
                         const __m512d ncosl = zmm8r8_negate(cosl);
                         // dr/dlambda, normalized (East) -- first element
                         u_0[0] = nsin;
                         u_1[0] = cosl;
                         u_2[0] = _mm512_setzero_pd();
                         // dr/dphi, normalized (North)  -- second element
                         u_0[1] = _mm512_mul_pd(ncosl,sinp);
                         u_1[1] = _mm512_mul_pd(nsinl,sinp);
                         u_2[1] = cosp;
}


void
ENU_axis_dir_mag_zmm8r8(__m512d * __restrict __attribute__((aligned(64))) u_0, //3-element array of __m512d
                        __m512d * __restrict __attribute__((aligned(64))) u_1, //3-element array of __m512d
                        __m512d * __restrict __attribute__((aligned(64))) u_2, //3-element array of __m512d
                        __m512d * __restrict __attribute__((aligned(64))) c ,   //3-element array of __m512d
                        const __m512d lat,
                        const __m512d lon,
                        const __m512d h,
                        const __m512d a,
                        const __m512d f) {
                         const __m512d _2   = _mm512_set1_pd(2.0);
                         const __m512d _0_02= _mm512_set1_pd(1.0e-2);
                         const __m512d _1   = _mm512_set1_pd(1.0);
#if (COORD_CONV__AVX512PD_SLEEF_LIB) == 1
                         const __m512d sinp = xsin(lat);
                         const __m512d cosp = xcos(lat);
                         const __m512d sinl = xsin(lon);
                         const __m512d cosl = xcos(lon);
#else
                         const __m512d sinp = _mm512_sin_pd(lat);
                         const __m512d cosp = _mm512_cos_pd(lat);
                         const __m512d sinl = _mm512_sin_pd(lon);
                         const __m512d cosl = _mm512_cos_pd(lon);
#endif
                         const __m512d e2   = _mm512_sub_pd(_mm512_mul_pd(_2,f),
                                                            _mm512_mul_pd(f,f));
                         const __m512d sarg = _mm512_sqrt_pd(_mm512_mul_pd(_0_02,
                                                             _mm512_mul_pd(sinp,sinp));
                         const __m512d Ne   = _mm512_div_pd(a,sarg);
                         //The derivative of the normal radius of curvature with respect to
                         //phi.
                         const __m512d sarg3= _mm512_mul_pd(sarg,_mm512_mul_pd(sarg,sarg));
                         __m512d t0   = _mm512_mul_pd(_mm512_mul_pd(a,e2),
                                                            _mm512_mul_pd(cosp,sinp));
                         const __m512d dNedPhi = _mm512_div_pd(t0,sarg3);
                          __m512d t1      = _mm512_mul_pd(_mm512_add_pd(Ne,h),sinp);
                          __m512d t2      = _mm512_mul_pd(cosp,dNedPhi);
                         const __m512d temp    = _mm512_sub_pd(t2,t1);
                         // u1  dr/dlambda, normalized (East).
                         u_0[0]  = zmm8r8_negate(sinl);
                         u_1[0]  = cosl;
                         u_2[0]  = _mm512_setzero_pd();
                         // mangitude of the East vector.
                         const __m512d Neh = _mm512_add_pd(Ne,h);
                         const __m512d ca  = _mm512_mul_pd(zmm8r8_negate(Neh),
                                                           _mm512_mul_pd(cosp,sinl));
                         const __m512d cb  = _mm512_mul_pd(Neh,
                                                           _mm512_mul_pd(cosp,cosl)); 
                         t0 = _mm512_add_pd(_mm512_mul_pd(ca,ca),
                                            _mm512_mul_pd(cb,cb));
                         c[0]              = _mm512_sqrt_pd(t0);
                         // u2 dr/dphi, normalized (North)
                         u_0[1] = _mm512_mul_pd(zmm8r8_negate(cosl),sinp);
                         u_1[1] = _mm512_mul_pd(zmm8r8_negate(sinl),sinp); 
                         u_2[1] = cosp;
                         //magnitude of the North vector.
                         const __m512d ca2 = _mm512_mul_pd(temp,cosl);
                         const __m512d cb2 = _mm512_mul_pd(temp,sinl);
                         c[2] = _1;
                         t1   = _mm512_mul_pd(_mm512_mul_pd(Ne,
                                              _mm512_add_pd(_0_02,h)),cosp);
                         t2   = _mm512_mul_pd(_0_02,_mm512_mul_pd(dNedPhi,sinp));
                         const __m512d cc = _mm512_add_pd(t1,t2);
                         const __m512d cc2 = _mm512_mul_pd(cc,cc);
                         const __m512d t3  = _mm512_add_pd(_mm512_mul_pd(ca2,ca2),
                                             _mm512_mul_pd(cb2,cb2));
                         c[1] = _mm512_sqrt_pd(_mm512_add_pd(t3,cc2));
                         // u3 dr/dh (Up)
                         u_0[2] = _mm512_mul_pd(cosp,cosl);
                         u_1[2] = _mm512_mul_pd(cosp,sinl);
                         u_2[2] = sinp;

}

