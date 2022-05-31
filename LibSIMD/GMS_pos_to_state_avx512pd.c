

#include "GMS_pos_to_state_avx512pd.h"
#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
#include "GMS_sleefsimddp.hpp"
#endif
#include "GMS_simd_utils.hpp"


void
const_velocity_zmm8r8(const __m512d xDot,
                      const __m512d yDot,
		      __m512d * __restrict __attribute__((aligned(64))) s_a,
		      __m512d * __restrict __attribute__((aligned(64))) s_b) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1					    
                         *s_a = atan2k(yDot,xDot);
#else
                         *s_a = _mm512_atan2_pd(yDot,xDot);
#endif
                         *s_b = _mm512_fmadd_pd(yDot,yDot,_mm512_mul_pd(xDot,xDot));
}


void
const_velocity_zmm8r8_a(const __m512d xDot,
                        const __m512d yDot,
		        double * __restrict __attribute__((aligned(64))) s_a,
		        double * __restrict __attribute__((aligned(64))) s_b) {
#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1					    
                         _mm512_store_pd(&s_a[0],atan2k(yDot,xDot));
#else
                         _mm512_store_pd(&s_a[0],_mm512_atan2_pd(yDot,xDot));
#endif
                         _mm512_store_pd(&s_b[0],_mm512_fmadd_pd(yDot,yDot,_mm512_mul_pd(xDot,xDot)));

}


void
const_velocity_zmm8r8_u(const __m512d xDot,
                        const __m512d yDot,
		        double * __restrict  s_a,
		        double * __restrict  s_b) {
#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1					    
                         _mm512_storeu_pd(&s_a[0],atan2k(yDot,xDot));
#else
                         _mm512_storeu_pd(&s_a[0],_mm512_atan2_pd(yDot,xDot));
#endif
                         _mm512_storeu_pd(&s_b[0],_mm512_fmadd_pd(yDot,yDot,_mm512_mul_pd(xDot,xDot)));

}


void
const_acceleration_zmm8r8(const __m512d xDot,
                          const __m512d yDot,
			  const __m512d xDdot,
			  const __m512d yDdot,
			  __m512d * __restrict  __attribute__((aligned(64))) s_a,
			  __m512d * __restrict  __attribute__((aligned(64))) s_b,
			  __m512d * __restrict  __attribute__((aligned(64))) s_c) {

  __m512d theta,costh,sinth,diff1,diff2,vDot,t0,t1,t2,t3;
			 __mmask8 m = 0x0;
#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         theta = atan2k(yDot,xDot);
			 *s_a  = theta;
			 costh = xcos(theta);
			 sinth = xsin(theta);
#else
                         theta = _mm512_atan2_pd(yDot,xDot);
			 *s_a  = theta;
			 costh = _mm512_cos_pd(theta);
			 sinth = _mm512_sin_pd(theta);
#endif
                         *s_b  = _mm512_fmadd_pd(yDot,yDot,_mm512_mul_pd(xDot,xDot));
                         vDot  = _mm512_sqrt_pd(_mm512_fmadd_pd(yDdot,yDdot,
			                                    _mm512_mul_pd(xDdot,xDdot)));
			 t0    = _mm512_sub_pd(_mm512_mul_pd(vDot,costh),xDdot);
			 t1    = _mm512_sub_pd(_mm512_mul_pd(vDot,sinth),yDdot);
			 t2    = _mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(vDot),costh),xDdot);
			 t3    = _mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(vDot),sinth),yDdot);
			 diff1 = _mm512_fmadd_pd(t0,t0,_mm512_mul_pd(t1,t1));
			 diff2 = _mm512_fmadd_pd(t2,t2,_mm512_mul_pd(t3,t3));
			 m     = _mm512_cmp_pd_mask(diff1,diff2,_CMP_LT_OQ);
			 vdot  = _mm512_mask_blend_pd(m,vDot,zmm8r8_negate(vDot));
			 *s_c  = vdot;
}


void
const_acceleration_zmm8r8_a(const __m512d xDot,
                            const __m512d yDot,
			    const __m512d xDdot,
			    const __m512d yDdot,
			    double * __restrict __attribute__((aligned(64))) s_a,
			    double * __restrict __attribute__((aligned(64))) s_b,
			    double * __restrict __attribute__((aligned(64))) s_c) {

  __m512d theta,costh,sinth,diff1,diff2,vDot,t0,t1,t2,t3;
			 __mmask8 m = 0x0;
#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         theta = atan2k(yDot,xDot);
			 _mm512_store_pd(&s_a[0],theta);
			 costh = xcos(theta);
			 sinth = xsin(theta);
#else
                         theta = _mm512_atan2_pd(yDot,xDot);
			 _mm512_store_pd(&s_a[0],theta);
			 costh = _mm512_cos_pd(theta);
			 sinth = _mm512_sin_pd(theta);
#endif
                         _mm512_store_pd(&s_b[0],_mm512_fmadd_pd(yDot,yDot,_mm512_mul_pd(xDot,xDot)));
                         vDot  = _mm512_sqrt_pd(_mm512_fmadd_pd(yDdot,yDdot,
			                                    _mm512_mul_pd(xDdot,xDdot)));
			 t0    = _mm512_sub_pd(_mm512_mul_pd(vDot,costh),xDdot);
			 t1    = _mm512_sub_pd(_mm512_mul_pd(vDot,sinth),yDdot);
			 t2    = _mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(vDot),costh),xDdot);
			 t3    = _mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(vDot),sinth),yDdot);
			 diff1 = _mm512_fmadd_pd(t0,t0,_mm512_mul_pd(t1,t1));
			 diff2 = _mm512_fmadd_pd(t2,t2,_mm512_mul_pd(t3,t3));
			 m     = _mm512_cmp_pd_mask(diff1,diff2,_CMP_LT_OQ);
			 vdot  = _mm512_mask_blend_pd(m,vDot,zmm8r8_negate(vDot));
			 _mm512_store_pd(&s_c[0],vdot);
}


void
const_acceleration_zmm8r8_u(const __m512d xDot,
                            const __m512d yDot,
			    const __m512d xDdot,
			    const __m512d yDdot,
			    double * __restrict  s_a,
			    double * __restrict  s_b,
			    double * __restrict  s_c) {

                         __m512d theta,costh,sinth,diff1,diff2,vDot,t0,t1,t2,t3;
			 __mmask8 m = 0x0;
#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         theta = atan2k(yDot,xDot);
			 _mm512_storeu_pd(&s_a[0],theta);
			 costh = xcos(theta);
			 sinth = xsin(theta);
#else
                         theta = _mm512_atan2_pd(yDot,xDot);
			 _mm512_storeu_pd(&s_a[0],theta);
			 costh = _mm512_cos_pd(theta);
			 sinth = _mm512_sin_pd(theta);
#endif
                         _mm512_storeu_pd(&s_b[0],_mm512_fmadd_pd(yDot,yDot,_mm512_mul_pd(xDot,xDot)));
                         vDot  = _mm512_sqrt_pd(_mm512_fmadd_pd(yDdot,yDdot,
			                                    _mm512_mul_pd(xDdot,xDdot)));
			 t0    = _mm512_sub_pd(_mm512_mul_pd(vDot,costh),xDdot);
			 t1    = _mm512_sub_pd(_mm512_mul_pd(vDot,sinth),yDdot);
			 t2    = _mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(vDot),costh),xDdot);
			 t3    = _mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(vDot),sinth),yDdot);
			 diff1 = _mm512_fmadd_pd(t0,t0,_mm512_mul_pd(t1,t1));
			 diff2 = _mm512_fmadd_pd(t2,t2,_mm512_mul_pd(t3,t3));
			 m     = _mm512_cmp_pd_mask(diff1,diff2,_CMP_LT_OQ);
			 vdot  = _mm512_mask_blend_pd(m,vDot,zmm8r8_negate(vDot));
			 _mm512_storeu_pd(&s_c[0],vdot);
}


void
const_turn_zmm8r8(const __m512d xDot,
                  const __m512d yDot,
		  const __m512d xDdot,
		  const __m512d yDdot,
		  __m512d * __restrict __attribute__((aligned(64))) a,
		  __m512d * __restrict __attribute__((aligned(64))) s,
		  __m512d * __restrict __attribute__((aligned(64))) omega) {


                          const __m512d t0 = _mm512_fmsub_pd(xDot,yDot,
			                                 _mm512_mul_pd(yDot,xDot));
			  const __m512d t1 = _mm512_fmadd_pd(xDot,xDot,
			                                 _mm512_mul_pd(yDot,yDot));
			  *omega           = _mm512_div_pd(t0,t1);
#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                          *a               = atan2kf(yDot,xDot);
#else
                          *a               = _mm512_atan2_pd(yDot,xDot);
#endif
                          *s               = _mm512_sqrt_pd(_mm512_fmadd_pd(yDot,yDot,
			                                                _mm512_mul_pd(xDot,xDot)));
}


void
const_turn_zmm8r8_a(const __m512d xDot,
                    const __m512d yDot,
		    const __m512d xDdot,
		    const __m512d yDdot,
		    double * __restrict  __attribute__((aligned(64))) a,
		    double * __restrict  __attribute__((aligned(64))) s,
		    double * __restrict  __attribute__((aligned(64))) omega) {

  const __m512d t0 = _mm512_fmsub_pd(xDot,yDot,
			                                 _mm512_mul_pd(yDot,xDot));
			  const __m512d t1 = _mm512_fmadd_pd(xDot,xDot,
			                                 _mm512_mul_pd(yDot,yDot));
			  _mm512_store_pd(&omega[0],_mm512_div_pd(t0,t1));
#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                          _mm512_store_pd(&a[0],atan2kf(yDot,xDot));
#else
                          _mm512_store_pd(&a[0],_mm512_atan2_pd(yDot,xDot));
#endif
                          _mm512_store_pd(&s[0],_mm512_sqrt_pd(_mm512_fmadd_pd(yDot,yDot,
			                                                _mm512_mul_pd(xDot,xDot))));
}


void
const_turn_zmm8r8_u(const __m512d xDot,
                    const __m512d yDot,
		    const __m512d xDdot,
		    const __m512d yDdot,
		    double * __restrict a,
		    double * __restrict s,
		    double * __restrict omega) {

  const __m512d t0 = _mm512_fmsub_pd(xDot,yDot,
			                                 _mm512_mul_pd(yDot,xDot));
			  const __m512d t1 = _mm512_fmadd_pd(xDot,xDot,
			                                 _mm512_mul_pd(yDot,yDot));
			  _mm512_storeu_pd(&omega[0],_mm512_div_pd(t0,t1));
#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                          _mm512_storeu_pd(&a[0],atan2kf(yDot,xDot));
#else
                          _mm512_storeu_pd(&a[0],_mm512_atan2_pd(yDot,xDot));
#endif
                          _mm512_storeu_pd(&s[0],_mm512_sqrt_pd(_mm512_fmadd_pd(yDot,yDot,
			                                                _mm512_mul_pd(xDot,xDot))));
}


void
turn_accelerate_zmm8r8(const __m512d xDot,
                       const __m512d yDot,
		       const __m512d xDdot,
		       const __m512d yDdot,
		       __m512d * __restrict  __attribute__((aligned(64))) theta,
		       __m512d * __restrict  __attribute__((aligned(64))) v,
		       __m512d * __restrict  __attribute__((aligned(64))) omega,
		       __m512d * __restrict  __attribute__((aligned(64))) vDot) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         const __m512d th    = atan2k(yDot,xDot);
                         const __m512d costh = xcos(th);
			 const __m512d sinth = xsin(th);
#else
                         const __m512d th    = _mm512_atan2_pd(yDot,xDot);
                         const __m512d costh = _mm512_cos_pd(th);
			 const __m512d sinth = _mm512_sin_pd(th);
#endif
                         *theta              = th;
			 *v                  = _mm512_sqrt_pd(_mm512_fmadd_pd(yDot,yDot,
			                                               _mm512_mul_pd(xDot,xDot)));
			 *omega              = _mm512_div_pd(_mm512_fmsub_pd(yDot,costh,
			                                               _mm512_mul_pd(xDot,sinth)),*v);
			 *vDot               = _mm512_fmadd_pd(xDot,costh,
			                                               _mm512_mul_pd(yDot,sinth));
}


void
turn_accelerate_zmm8r8_a(const __m512d xDot,
                         const __m512d yDot,
		         const __m512d xDdot,
		         const __m512d yDdot,
		         double * __restrict __attribute__((aligned(64))) theta,
		         double * __restrict __attribute__((aligned(64))) v,
		         double * __restrict __attribute__((aligned(64))) omega,
		         double * __restrict __attribute__((aligned(64))) vDot) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         const __m512d th    = atan2k(yDot,xDot);
                         const __m512d costh = xcos(th);
			 const __m512d sinth = xsin(th);
#else
                         const __m512d th    = _mm512_atan2_pd(yDot,xDot);
                         const __m512d costh = _mm512_cos_pd(th);
			 const __m512d sinth = _mm512_sin_pd(th);
#endif
                         _mm512_store_pd(&theta[0],th);
			 _mm512_store_pd(&v[0],_mm512_sqrt_pd(_mm512_fmadd_pd(yDot,yDot,
			                                               _mm512_mul_pd(xDot,xDot))));
			 _mm512_store_pd(&omega[0],_mm512_div_pd(_mm512_fmsub_pd(yDot,costh,
			                                               _mm512_mul_pd(xDot,sinth)),
								              _mm512_load_pd(&v[0])));
			 _mm512_store_pd(&vDot[0],_mm512_fmadd_pd(xDot,costh,
			                                               _mm512_mul_pd(yDot,sinth)));
}


void
turn_accelerate_zmm8r8_u(const __m512d xDot,
                         const __m512d yDot,
		         const __m512d xDdot,
		         const __m512d yDdot,
		         double * __restrict  theta,
		         double * __restrict  v,
		         double * __restrict  omega,
		         double * __restrict  vDot) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         const __m512d th    = atan2k(yDot,xDot);
                         const __m512d costh = xcos(th);
			 const __m512d sinth = xsin(th);
#else
                         const __m512d th    = _mm512_atan2_pd(yDot,xDot);
                         const __m512d costh = _mm512_cos_pd(th);
			 const __m512d sinth = _mm512_sin_pd(th);
#endif
                         _mm512_storeu_pd(&theta[0],th);
			 _mm512_storeu_pd(&v[0],_mm512_sqrt_pd(_mm512_fmadd_pd(yDot,yDot,
			                                               _mm512_mul_pd(xDot,xDot))));
			 _mm512_storeu_pd(&omega[0],_mm512_div_pd(_mm512_fmsub_pd(yDot,costh,
			                                               _mm512_mul_pd(xDot,sinth)),
								              _mm512_loadu_pd(&v[0])));
			 _mm512_storeu_pd(&vDot[0],_mm512_fmadd_pd(xDot,costh,
			                                               _mm512_mul_pd(yDot,sinth)));
}


void
const_pol_vel_zmm8r8(const __m512d theta,
                     const __m512d v,
		     __m512d * __restrict  __attribute__((aligned(64))) vcth,
		     __m512d * __restrict  __attribute__((aligned(64))) vsth) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         const __m512d cth = xcos(theta);
			 const __m512d sth = xsin(theta);
#else
                         const __m512d cth = _mm512_cos_pd(theta);
			 const __m512d sth = _mm512_sin_pd(theta);
#endif
                         *vcth = _mm512_mul_pd(v,cth);
			 *vsth = _mm512_mul_pd(v,sth);
}


void
const_pol_vel_zmm8r8_a(const __m512d theta,
                       const __m512d v,
		       double * __restrict __attribute__((aligned(64))) vcth,
		       double * __restrict __attribute__((aligned(64))) vsth) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         const __m512d cth = xcos(theta);
			 const __m512d sth = xsin(theta);
#else
                         const __m512d cth = _mm512_cos_pd(theta);
			 const __m512d sth = _mm512_sin_pd(theta);
#endif
                         _mm512_store_pd(&vcth[0],_mm512_mul_pd(v,cth));
			 _mm512_store_pd(&vsth[0],_mm512_mul_pd(v,sth));
}


void
const_pol_vel_zmm8r8_u(const __m512d theta,
                       const __m512d v,
		       double * __restrict  vcth,
		       double * __restrict  vsth) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         const __m512d cth = xcos(theta);
			 const __m512d sth = xsin(theta);
#else
                         const __m512d cth = _mm512_cos_pd(theta);
			 const __m512d sth = _mm512_sin_pd(theta);
#endif
                         _mm512_storeu_pd(&vcth[0],_mm512_mul_pd(v,cth));
			 _mm512_storeu_pd(&vsth[0],_mm512_mul_pd(v,sth));
}


void
const_pol_accel_zmm8r8(const __m512d theta,
                       const __m512d v,
		       const __m512d vDot,
		       __m512d * __restrict __attribute__((aligned(64))) vcth,
		       __m512d * __restrict __attribute__((aligned(64))) vsth,
		       __m512d * __restrict __attribute__((aligned(64))) vdcth,
		       __m512d * __restrict __attribute__((aligned(64))) vdsth) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         const __m512d cth = xcos(theta);
			 const __m512d sth = xsin(theta);
#else
                         const __m512d cth = _mm512_cos_pd(theta);
			 const __m512d sth = _mm512_sin_pd(theta);
#endif
                         *vcth              = _mm512_mul_pd(v,cth);
			 *vsth              = _mm512_mul_pd(v,sth);
			 *vdcth             = _mm512_mul_pd(vDot,cth);
			 *vdsth             = _mm512_mul_pd(vDot,sth);
		   }
}


void
const_pol_accel_zmm8r8_a(const __m512d theta,
                         const __m512d v,
		         const __m512d vDot,
		         double * __restrict __attribute__((aligned(64))) vcth,
		         double * __restrict __attribute__((aligned(64))) vsth,
		         double * __restrict __attribute__((aligned(64))) vdcth,
		         double * __restrict __attribute__((aligned(64))) vdsth) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         const __m512d cth = xcos(theta);
			 const __m512d sth = xsin(theta);
#else
                         const __m512d cth = _mm512_cos_pd(theta);
			 const __m512d sth = _mm512_sin_pd(theta);
#endif
                         _mm512_store_pd(&vcth[0],_mm512_mul_pd(v,cth));
			 _mm512_store_pd(&vsth[0],_mm512_mul_pd(v,sth));
			 _mm512_store_pd(&vdcth[0],_mm512_mul_pd(vDot,cth));
			 _mm512_store_pd(&vdsth[0],_mm512_mul_pd(vDot,sth));
}


void
const_pol_accel_zmm8r8_u(const __m512d theta,
                         const __m512d v,
		         const __m512d vDot,
		         double * __restrict vcth,
		         double * __restrict vsth,
		         double * __restrict vdcth,
		         double * __restrict vdsth) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         const __m512d cth = xcos(theta);
			 const __m512d sth = xsin(theta);
#else
                         const __m512d cth = _mm512_cos_pd(theta);
			 const __m512d sth = _mm512_sin_pd(theta);
#endif
                         _mm512_storeu_pd(&vcth[0],_mm512_mul_pd(v,cth));
			 _mm512_storeu_pd(&vsth[0],_mm512_mul_pd(v,sth));
			 _mm512_storeu_pd(&vdcth[0],_mm512_mul_pd(vDot,cth));
			 _mm512_storeu_pd(&vdsth[0],_mm512_mul_pd(vDot,sth));
}


void
const_pol_turn_zmm8r8( const __m512d theta,
                       const __m512d v,
		       const __m512d omega, // turn-rate
		       __m512d * __restrict __attribute__((aligned(64))) vcth,
		       __m512d * __restrict __attribute__((aligned(64))) vsth,
		       __m512d * __restrict __attribute__((aligned(64))) vomsth,
		       __m512d * __restrict __attribute__((aligned(64))) vomcth) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         const __m512d cth = xcos(theta);
			 const __m512d sth = xsin(theta);
#else
                         const __m512d cth = _mm512_cos_pd(theta);
			 const __m512d sth = _mm512_sin_pd(theta);
#endif
                         *vcth              = _mm512_mul_pd(v,cth);
			 *vsth              = _mm512_mul_pd(v,sth);
                         *vomsth            = _mm512_mul_pd(zmm8r8_negate(v),
			                               _mm512_mul_pd(omega,sth));
			 *vomcth            = _mm512_mul_pd(v,_mm512_mul_pd(omega,cth));
}


void
const_pol_turn_zmm8r8_a( const __m512d theta,
                         const __m512d v,
		         const __m512d omega,
		         double * __restrict __attribute__((aligned(64))) vcth,
		         double * __restrict __attribute__((aligned(64))) vsth,
		         double * __restrict __attribute__((aligned(64))) vomsth,
		         double * __restrict __attribute__((aligned(64))) vomcth) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         const __m512d cth = xcos(theta);
			 const __m512d sth = xsin(theta);
#else
                         const __m512d cth = _mm512_cos_pd(theta);
			 const __m512d sth = _mm512_sin_pd(theta);
#endif
                         _mm512_store_pd(&vcth[0],_mm512_mul_pd(v,cth));
			 _mm512_store_pd(&vsth[0],_mm512_mul_pd(v,sth));
                         _mm512_store_pd(&vomsth[0],_mm512_mul_pd(zmm8r8_negate(v),
			                               _mm512_mul_pd(omega,sth)));
			 _mm512_store_pd(&vomcth[0],_mm512_mul_pd(v,_mm512_mul_pd(omega,cth)));
}


void
const_pol_turn_zmm8r8_u( const __m512d theta,
                         const __m512d v,
		         const __m512d omega,
		         double * __restrict  vcth,
		         double * __restrict  vsth,
		         double * __restrict  vomsth,
		         double * __restrict  vomcth) {

#if (POS_TO_STATE_AVX512PD_SLEEF_LIB) == 1
                         const __m512d cth = xcos(theta);
			 const __m512d sth = xsin(theta);
#else
                         const __m512d cth = _mm512_cos_pd(theta);
			 const __m512d sth = _mm512_sin_pd(theta);
#endif
                         _mm512_storeu_pd(&vcth[0],_mm512_mul_pd(v,cth));
			 _mm512_storeu_pd(&vsth[0],_mm512_mul_pd(v,sth));
                         _mm512_storeu_pd(&vomsth[0],_mm512_mul_pd(zmm8r8_negate(v),
			                               _mm512_mul_pd(omega,sth)));
			 _mm512_storeu_pd(&vomcth[0],_mm512_mul_pd(v,_mm512_mul_pd(omega,cth)));
}
