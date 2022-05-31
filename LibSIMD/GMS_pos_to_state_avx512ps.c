

#include "GMS_pos_to_state_avx512ps.h"
#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
#include "GMS_sleefsimdsp.hpp"
#endif
#include "GMS_simd_utils.hpp"


void
const_velocity_zmm16r4(const __m512 xDot,
                      const __m512 yDot,
		      __m512 * __restrict __attribute__((aligned(64))) s_a,
		      __m512 * __restrict __attribute__((aligned(64))) s_b) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1					    
                         *s_a = atan2kf(yDot,xDot);
#else
                         *s_a = _mm512_atan2_ps(yDot,xDot);
#endif
                         *s_b = _mm512_fmadd_ps(yDot,yDot,_mm512_mul_ps(xDot,xDot));
}


void
const_velocity_zmm16r4_a(const __m512 xDot,
                        const __m512 yDot,
		        float * __restrict __attribute__((aligned(64))) s_a,
		        float * __restrict __attribute__((aligned(64))) s_b) {
#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1					    
                         _mm512_store_ps(&s_a[0],atan2kf(yDot,xDot));
#else
                         _mm512_store_ps(&s_a[0],_mm512_atan2_ps(yDot,xDot));
#endif
                         _mm512_store_ps(&s_b[0],_mm512_fmadd_ps(yDot,yDot,_mm512_mul_ps(xDot,xDot)));

}


void
const_velocity_zmm16r4_u(const __m512 xDot,
                        const __m512 yDot,
		        float * __restrict  s_a,
		        float * __restrict  s_b) {
#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1					    
                         _mm512_storeu_ps(&s_a[0],atan2kf(yDot,xDot));
#else
                         _mm512_storeu_ps(&s_a[0],_mm512_atan2_ps(yDot,xDot));
#endif
                         _mm512_storeu_ps(&s_b[0],_mm512_fmadd_ps(yDot,yDot,_mm512_mul_ps(xDot,xDot)));

}


void
const_acceleration_zmm16r4(const __m512 xDot,
                          const __m512 yDot,
			  const __m512 xDdot,
			  const __m512 yDdot,
			  __m512 * __restrict  __attribute__((aligned(64))) s_a,
			  __m512 * __restrict  __attribute__((aligned(64))) s_b,
			  __m512 * __restrict  __attribute__((aligned(64))) s_c) {

  __m512 theta,costh,sinth,diff1,diff2,vDot,t0,t1,t2,t3;
			 __mmask16 m = 0x0;
#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         theta = atan2kf(yDot,xDot);
			 *s_a  = theta;
			 costh = xcosf(theta);
			 sinth = xsinf(theta);
#else
                         theta = _mm512_atan2_ps(yDot,xDot);
			 *s_a  = theta;
			 costh = _mm512_cos_ps(theta);
			 sinth = _mm512_sin_ps(theta);
#endif
                         *s_b  = _mm512_fmadd_ps(yDot,yDot,_mm512_mul_ps(xDot,xDot));
                         vDot  = _mm512_sqrt_ps(_mm512_fmadd_ps(yDdot,yDdot,
			                                    _mm512_mul_ps(xDdot,xDdot)));
			 t0    = _mm512_sub_ps(_mm512_mul_ps(vDot,costh),xDdot);
			 t1    = _mm512_sub_ps(_mm512_mul_ps(vDot,sinth),yDdot);
			 t2    = _mm512_sub_ps(_mm512_mul_ps(zmm16r4_negate(vDot),costh),xDdot);
			 t3    = _mm512_sub_ps(_mm512_mul_ps(zmm16r4_negate(vDot),sinth),yDdot);
			 diff1 = _mm512_fmadd_ps(t0,t0,_mm512_mul_ps(t1,t1));
			 diff2 = _mm512_fmadd_ps(t2,t2,_mm512_mul_ps(t3,t3));
			 m     = _mm512_cmp_ps_mask(diff1,diff2,_CMP_LT_OQ);
			 vdot  = _mm512_mask_blend_ps(m,vDot,zmm16r4_negate(vDot));
			 *s_c  = vdot;
}


void
const_acceleration_zmm16r4_a(const __m512 xDot,
                            const __m512 yDot,
			    const __m512 xDdot,
			    const __m512 yDdot,
			    float * __restrict __attribute__((aligned(64))) s_a,
			    float * __restrict __attribute__((aligned(64))) s_b,
			    float * __restrict __attribute__((aligned(64))) s_c) {

  __m512 theta,costh,sinth,diff1,diff2,vDot,t0,t1,t2,t3;
			 __mmask16 m = 0x0;
#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         theta = atan2kf(yDot,xDot);
			 _mm512_store_ps(&s_a[0],theta);
			 costh = xcosf(theta);
			 sinth = xsinf(theta);
#else
                         theta = _mm512_atan2_ps(yDot,xDot);
			 _mm512_store_ps(&s_a[0],theta);
			 costh = _mm512_cos_ps(theta);
			 sinth = _mm512_sin_ps(theta);
#endif
                         _mm512_store_ps(&s_b[0],_mm512_fmadd_ps(yDot,yDot,_mm512_mul_ps(xDot,xDot)));
                         vDot  = _mm512_sqrt_ps(_mm512_fmadd_ps(yDdot,yDdot,
			                                    _mm512_mul_ps(xDdot,xDdot)));
			 t0    = _mm512_sub_ps(_mm512_mul_ps(vDot,costh),xDdot);
			 t1    = _mm512_sub_ps(_mm512_mul_ps(vDot,sinth),yDdot);
			 t2    = _mm512_sub_ps(_mm512_mul_ps(zmm16r4_negate(vDot),costh),xDdot);
			 t3    = _mm512_sub_ps(_mm512_mul_ps(zmm16r4_negate(vDot),sinth),yDdot);
			 diff1 = _mm512_fmadd_ps(t0,t0,_mm512_mul_ps(t1,t1));
			 diff2 = _mm512_fmadd_ps(t2,t2,_mm512_mul_ps(t3,t3));
			 m     = _mm512_cmp_ps_mask(diff1,diff2,_CMP_LT_OQ);
			 vdot  = _mm512_mask_blend_ps(m,vDot,zmm16r4_negate(vDot));
			 _mm512_store_ps(&s_c[0],vdot);
}


void
const_acceleration_zmm16r4_u(const __m512 xDot,
                            const __m512 yDot,
			    const __m512 xDdot,
			    const __m512 yDdot,
			    float * __restrict  s_a,
			    float * __restrict  s_b,
			    float * __restrict  s_c) {

                         __m512 theta,costh,sinth,diff1,diff2,vDot,t0,t1,t2,t3;
			 __mmask16 m = 0x0;
#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         theta = atan2kf(yDot,xDot);
			 _mm512_storeu_ps(&s_a[0],theta);
			 costh = xcosf(theta);
			 sinth = xsinf(theta);
#else
                         theta = _mm512_atan2_ps(yDot,xDot);
			 _mm512_storeu_ps(&s_a[0],theta);
			 costh = _mm512_cos_ps(theta);
			 sinth = _mm512_sin_ps(theta);
#endif
                         _mm512_storeu_ps(&s_b[0],_mm512_fmadd_ps(yDot,yDot,_mm512_mul_ps(xDot,xDot)));
                         vDot  = _mm512_sqrt_ps(_mm512_fmadd_ps(yDdot,yDdot,
			                                    _mm512_mul_ps(xDdot,xDdot)));
			 t0    = _mm512_sub_ps(_mm512_mul_ps(vDot,costh),xDdot);
			 t1    = _mm512_sub_ps(_mm512_mul_ps(vDot,sinth),yDdot);
			 t2    = _mm512_sub_ps(_mm512_mul_ps(zmm16r4_negate(vDot),costh),xDdot);
			 t3    = _mm512_sub_ps(_mm512_mul_ps(zmm16r4_negate(vDot),sinth),yDdot);
			 diff1 = _mm512_fmadd_ps(t0,t0,_mm512_mul_ps(t1,t1));
			 diff2 = _mm512_fmadd_ps(t2,t2,_mm512_mul_ps(t3,t3));
			 m     = _mm512_cmp_ps_mask(diff1,diff2,_CMP_LT_OQ);
			 vdot  = _mm512_mask_blend_ps(m,vDot,zmm16r4_negate(vDot));
			 _mm512_storeu_ps(&s_c[0],vdot);
}


void
const_turn_zmm16r4(const __m512 xDot,
                  const __m512 yDot,
		  const __m512 xDdot,
		  const __m512 yDdot,
		  __m512 * __restrict __attribute__((aligned(64))) a,
		  __m512 * __restrict __attribute__((aligned(64))) s,
		  __m512 * __restrict __attribute__((aligned(64))) omega) {


                          const __m512 t0 = _mm512_fmsub_ps(xDot,yDot,
			                                 _mm512_mul_ps(yDot,xDot));
			  const __m512 t1 = _mm512_fmadd_ps(xDot,xDot,
			                                 _mm512_mul_ps(yDot,yDot));
			  *omega           = _mm512_div_ps(t0,t1);
#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                          *a               = atan2kf(yDot,xDot);
#else
                          *a               = _mm512_atan2_ps(yDot,xDot);
#endif
                          *s               = _mm512_sqrt_ps(_mm512_fmadd_ps(yDot,yDot,
			                                                _mm512_mul_ps(xDot,xDot)));
}


void
const_turn_zmm16r4_a(const __m512 xDot,
                    const __m512 yDot,
		    const __m512 xDdot,
		    const __m512 yDdot,
		    float * __restrict  __attribute__((aligned(64))) a,
		    float * __restrict  __attribute__((aligned(64))) s,
		    float * __restrict  __attribute__((aligned(64))) omega) {

  const __m512 t0 = _mm512_fmsub_ps(xDot,yDot,
			                                 _mm512_mul_ps(yDot,xDot));
			  const __m512 t1 = _mm512_fmadd_ps(xDot,xDot,
			                                 _mm512_mul_ps(yDot,yDot));
			  _mm512_store_ps(&omega[0],_mm512_div_ps(t0,t1));
#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                          _mm512_store_ps(&a[0],atan2kf(yDot,xDot));
#else
                          _mm512_store_ps(&a[0],_mm512_atan2_ps(yDot,xDot));
#endif
                          _mm512_store_ps(&s[0],_mm512_sqrt_ps(_mm512_fmadd_ps(yDot,yDot,
			                                                _mm512_mul_ps(xDot,xDot))));
}


void
const_turn_zmm16r4_u(const __m512 xDot,
                    const __m512 yDot,
		    const __m512 xDdot,
		    const __m512 yDdot,
		    float * __restrict a,
		    float * __restrict s,
		    float * __restrict omega) {

  const __m512 t0 = _mm512_fmsub_ps(xDot,yDot,
			                                 _mm512_mul_ps(yDot,xDot));
			  const __m512 t1 = _mm512_fmadd_ps(xDot,xDot,
			                                 _mm512_mul_ps(yDot,yDot));
			  _mm512_storeu_ps(&omega[0],_mm512_div_ps(t0,t1));
#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                          _mm512_storeu_ps(&a[0],atan2kf(yDot,xDot));
#else
                          _mm512_storeu_ps(&a[0],_mm512_atan2_ps(yDot,xDot));
#endif
                          _mm512_storeu_ps(&s[0],_mm512_sqrt_ps(_mm512_fmadd_ps(yDot,yDot,
			                                                _mm512_mul_ps(xDot,xDot))));
}


void
turn_accelerate_zmm16r4(const __m512 xDot,
                       const __m512 yDot,
		       const __m512 xDdot,
		       const __m512 yDdot,
		       __m512 * __restrict  __attribute__((aligned(64))) theta,
		       __m512 * __restrict  __attribute__((aligned(64))) v,
		       __m512 * __restrict  __attribute__((aligned(64))) omega,
		       __m512 * __restrict  __attribute__((aligned(64))) vDot) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         const __m512 th    = atan2kf(yDot,xDot);
                         const __m512 costh = xcos(th);
			 const __m512 sinth = xsin(th);
#else
                         const __m512 th    = _mm512_atan2_ps(yDot,xDot);
                         const __m512 costh = _mm512_cos_ps(th);
			 const __m512 sinth = _mm512_sin_ps(th);
#endif
                         *theta              = th;
			 *v                  = _mm512_sqrt_ps(_mm512_fmadd_ps(yDot,yDot,
			                                               _mm512_mul_ps(xDot,xDot)));
			 *omega              = _mm512_div_ps(_mm512_fmsub_ps(yDot,costh,
			                                               _mm512_mul_ps(xDot,sinth)),*v);
			 *vDot               = _mm512_fmadd_ps(xDot,costh,
			                                               _mm512_mul_ps(yDot,sinth));
}


void
turn_accelerate_zmm16r4_a(const __m512 xDot,
                         const __m512 yDot,
		         const __m512 xDdot,
		         const __m512 yDdot,
		         float * __restrict __attribute__((aligned(64))) theta,
		         float * __restrict __attribute__((aligned(64))) v,
		         float * __restrict __attribute__((aligned(64))) omega,
		         float * __restrict __attribute__((aligned(64))) vDot) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         const __m512 th    = atan2kf(yDot,xDot);
                         const __m512 costh = xcosf(th);
			 const __m512 sinth = xsinf(th);
#else
                         const __m512 th    = _mm512_atan2_ps(yDot,xDot);
                         const __m512 costh = _mm512_cos_ps(th);
			 const __m512 sinth = _mm512_sin_ps(th);
#endif
                         _mm512_store_ps(&theta[0],th);
			 _mm512_store_ps(&v[0],_mm512_sqrt_ps(_mm512_fmadd_ps(yDot,yDot,
			                                               _mm512_mul_ps(xDot,xDot))));
			 _mm512_store_ps(&omega[0],_mm512_div_ps(_mm512_fmsub_ps(yDot,costh,
			                                               _mm512_mul_ps(xDot,sinth)),
								              _mm512_load_ps(&v[0])));
			 _mm512_store_ps(&vDot[0],_mm512_fmadd_ps(xDot,costh,
			                                               _mm512_mul_ps(yDot,sinth)));
}


void
turn_accelerate_zmm16r4_u(const __m512 xDot,
                         const __m512 yDot,
		         const __m512 xDdot,
		         const __m512 yDdot,
		         float * __restrict  theta,
		         float * __restrict  v,
		         float * __restrict  omega,
		         float * __restrict  vDot) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         const __m512 th    = atan2kf(yDot,xDot);
                         const __m512 costh = xcosf(th);
			 const __m512 sinth = xsinf(th);
#else
                         const __m512 th    = _mm512_atan2_ps(yDot,xDot);
                         const __m512 costh = _mm512_cos_ps(th);
			 const __m512 sinth = _mm512_sin_ps(th);
#endif
                         _mm512_storeu_ps(&theta[0],th);
			 _mm512_storeu_ps(&v[0],_mm512_sqrt_ps(_mm512_fmadd_ps(yDot,yDot,
			                                               _mm512_mul_ps(xDot,xDot))));
			 _mm512_storeu_ps(&omega[0],_mm512_div_ps(_mm512_fmsub_ps(yDot,costh,
			                                               _mm512_mul_ps(xDot,sinth)),
								              _mm512_loadu_ps(&v[0])));
			 _mm512_storeu_ps(&vDot[0],_mm512_fmadd_ps(xDot,costh,
			                                               _mm512_mul_ps(yDot,sinth)));
}


void
const_pol_vel_zmm16r4(const __m512 theta,
                     const __m512 v,
		     __m512 * __restrict  __attribute__((aligned(64))) vcth,
		     __m512 * __restrict  __attribute__((aligned(64))) vsth) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         const __m512 cth = xcosf(theta);
			 const __m512 sth = xsinf(theta);
#else
                         const __m512 cth = _mm512_cos_ps(theta);
			 const __m512 sth = _mm512_sin_ps(theta);
#endif
                         *vcth = _mm512_mul_ps(v,cth);
			 *vsth = _mm512_mul_ps(v,sth);
}


void
const_pol_vel_zmm16r4_a(const __m512 theta,
                       const __m512 v,
		       float * __restrict __attribute__((aligned(64))) vcth,
		       float * __restrict __attribute__((aligned(64))) vsth) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         const __m512 cth = xcosf(theta);
			 const __m512 sth = xsinf(theta);
#else
                         const __m512 cth = _mm512_cos_ps(theta);
			 const __m512 sth = _mm512_sin_ps(theta);
#endif
                         _mm512_store_ps(&vcth[0],_mm512_mul_ps(v,cth));
			 _mm512_store_ps(&vsth[0],_mm512_mul_ps(v,sth));
}


void
const_pol_vel_zmm16r4_u(const __m512 theta,
                       const __m512 v,
		       float * __restrict  vcth,
		       float * __restrict  vsth) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         const __m512 cth = xcosf(theta);
			 const __m512 sth = xsinf(theta);
#else
                         const __m512 cth = _mm512_cos_ps(theta);
			 const __m512 sth = _mm512_sin_ps(theta);
#endif
                         _mm512_storeu_ps(&vcth[0],_mm512_mul_ps(v,cth));
			 _mm512_storeu_ps(&vsth[0],_mm512_mul_ps(v,sth));
}


void
const_pol_accel_zmm16r4(const __m512 theta,
                       const __m512 v,
		       const __m512 vDot,
		       __m512 * __restrict __attribute__((aligned(64))) vcth,
		       __m512 * __restrict __attribute__((aligned(64))) vsth,
		       __m512 * __restrict __attribute__((aligned(64))) vdcth,
		       __m512 * __restrict __attribute__((aligned(64))) vdsth) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         const __m512 cth = xcosf(theta);
			 const __m512 sth = xsinf(theta);
#else
                         const __m512 cth = _mm512_cos_ps(theta);
			 const __m512 sth = _mm512_sin_ps(theta);
#endif
                         *vcth              = _mm512_mul_ps(v,cth);
			 *vsth              = _mm512_mul_ps(v,sth);
			 *vdcth             = _mm512_mul_ps(vDot,cth);
			 *vdsth             = _mm512_mul_ps(vDot,sth);
		   }
}


void
const_pol_accel_zmm16r4_a(const __m512 theta,
                         const __m512 v,
		         const __m512 vDot,
		         float * __restrict __attribute__((aligned(64))) vcth,
		         float * __restrict __attribute__((aligned(64))) vsth,
		         float * __restrict __attribute__((aligned(64))) vdcth,
		         float * __restrict __attribute__((aligned(64))) vdsth) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         const __m512 cth = xcosf(theta);
			 const __m512 sth = xsinf(theta);
#else
                         const __m512 cth = _mm512_cos_ps(theta);
			 const __m512 sth = _mm512_sin_ps(theta);
#endif
                         _mm512_store_ps(&vcth[0],_mm512_mul_ps(v,cth));
			 _mm512_store_ps(&vsth[0],_mm512_mul_ps(v,sth));
			 _mm512_store_ps(&vdcth[0],_mm512_mul_ps(vDot,cth));
			 _mm512_store_ps(&vdsth[0],_mm512_mul_ps(vDot,sth));
}


void
const_pol_accel_zmm16r4_u(const __m512 theta,
                         const __m512 v,
		         const __m512 vDot,
		         float * __restrict vcth,
		         float * __restrict vsth,
		         float * __restrict vdcth,
		         float * __restrict vdsth) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         const __m512 cth = xcosf(theta);
			 const __m512 sth = xsinf(theta);
#else
                         const __m512 cth = _mm512_cos_ps(theta);
			 const __m512 sth = _mm512_sin_ps(theta);
#endif
                         _mm512_storeu_ps(&vcth[0],_mm512_mul_ps(v,cth));
			 _mm512_storeu_ps(&vsth[0],_mm512_mul_ps(v,sth));
			 _mm512_storeu_ps(&vdcth[0],_mm512_mul_ps(vDot,cth));
			 _mm512_storeu_ps(&vdsth[0],_mm512_mul_ps(vDot,sth));
}


void
const_pol_turn_zmm16r4( const __m512 theta,
                       const __m512 v,
		       const __m512 omega, // turn-rate
		       __m512 * __restrict __attribute__((aligned(64))) vcth,
		       __m512 * __restrict __attribute__((aligned(64))) vsth,
		       __m512 * __restrict __attribute__((aligned(64))) vomsth,
		       __m512 * __restrict __attribute__((aligned(64))) vomcth) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         const __m512 cth = xcosf(theta);
			 const __m512 sth = xsinf(theta);
#else
                         const __m512 cth = _mm512_cos_ps(theta);
			 const __m512 sth = _mm512_sin_ps(theta);
#endif
                         *vcth              = _mm512_mul_ps(v,cth);
			 *vsth              = _mm512_mul_ps(v,sth);
                         *vomsth            = _mm512_mul_ps(zmm16r4_negate(v),
			                               _mm512_mul_ps(omega,sth));
			 *vomcth            = _mm512_mul_ps(v,_mm512_mul_ps(omega,cth));
}


void
const_pol_turn_zmm16r4_a( const __m512 theta,
                         const __m512 v,
		         const __m512 omega,
		         float * __restrict __attribute__((aligned(64))) vcth,
		         float * __restrict __attribute__((aligned(64))) vsth,
		         float * __restrict __attribute__((aligned(64))) vomsth,
		         float * __restrict __attribute__((aligned(64))) vomcth) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         const __m512 cth = xcosf(theta);
			 const __m512 sth = xsinf(theta);
#else
                         const __m512 cth = _mm512_cos_ps(theta);
			 const __m512 sth = _mm512_sin_ps(theta);
#endif
                         _mm512_store_ps(&vcth[0],_mm512_mul_ps(v,cth));
			 _mm512_store_ps(&vsth[0],_mm512_mul_ps(v,sth));
                         _mm512_store_ps(&vomsth[0],_mm512_mul_ps(zmm16r4_negate(v),
			                               _mm512_mul_ps(omega,sth)));
			 _mm512_store_ps(&vomcth[0],_mm512_mul_ps(v,_mm512_mul_ps(omega,cth)));
}


void
const_pol_turn_zmm16r4_u( const __m512 theta,
                         const __m512 v,
		         const __m512 omega,
		         float * __restrict  vcth,
		         float * __restrict  vsth,
		         float * __restrict  vomsth,
		         float * __restrict  vomcth) {

#if (POS_TO_STATE_AVX512PS_SLEEF_LIB) == 1
                         const __m512 cth = xcosf(theta);
			 const __m512 sth = xsinf(theta);
#else
                         const __m512 cth = _mm512_cos_ps(theta);
			 const __m512 sth = _mm512_sin_ps(theta);
#endif
                         _mm512_storeu_ps(&vcth[0],_mm512_mul_ps(v,cth));
			 _mm512_storeu_ps(&vsth[0],_mm512_mul_ps(v,sth));
                         _mm512_storeu_ps(&vomsth[0],_mm512_mul_ps(zmm16r4_negate(v),
			                               _mm512_mul_ps(omega,sth)));
			 _mm512_storeu_ps(&vomcth[0],_mm512_mul_ps(v,_mm512_mul_ps(omega,cth)));
}
