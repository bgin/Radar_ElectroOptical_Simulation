
#include "GMS_rotation_kernels_avx512.h"


                       RotM9x16v16
                       q4x16_to_rmat9x16_zmm16r4(const __m512 q_x,
		                                 const __m512 q_y,
						 const __m512 q_z,
						 const __m512 q_w) {
                            RotM9x16v16 rmat{};
			    __m512 t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10;
			    t0  = _mm512_mul_ps(q_x,q_x);
			    t1  = _mm512_add_ps(_mm512_add_ps(q_x,q_y),q_w);
			    t2  = _mm512_mul_ps(t1,t1);
			    t3  = _mm512_sub_ps(t0,t2); //qq
			    t5  = _mm512_mul_ps(q_y,q_z);
			    t6  = _mm512_mul_ps(q_z,q_w);
			    t7  = _mm512_mul_ps(q_w,q_y);
			    t8  = _mm512_mul_ps(q_x,q_w);
			    t9  = _mm512_mul_ps(q_x,q_y);
			    t10 = _mm512_mul_ps(q_x,q_z);
			    t4 = _mm512_mul_ps(q_y,q_y);
                            rmat.m_vRow1 = _mm512_fmadd_ps(v16_2,t4,t3);
			    rmat.m_vRow2 = _mm512_mul_ps(v16_2,_mm512_sub_ps(t5,t8));
			    rmat.m_vRow3 = _mm512_mul_ps(v16_2,_mm512_add_ps(t7,t10));
			    rmat.m_vRow4 = _mm512_mul_ps(v16_2,_mm512_add_ps(t5,t8));
			    rmat.m_vRow5 = _mm512_fmadd_ps(v16_2,_mm512_mul_ps(q_z,q_z),t3);
			    rmat.m_vRow6 = _mm512_mul_ps(v16_2,_mm512_sub_ps(t6,t9));
			    rmat.m_vRow7 = _mm512_mul_ps(v16_2,_mm512_sub_ps(t7,t8));
			    rmat.m_vRow8 = _mm512_mul_ps(v16_2,_mm512_add_ps(t6,t9));
			    rmat.m_vRow9 = _mm512_fmadd_ps(v16_2,_mm512_mul_ps(q_w,q_w),t3);
			    return (rmat);
		      }


                       RotM9x8v8
                       q4x8_to_rmat9x8_zmm8r8(const __m512d q_x,
		                              const __m512d q_y,
					      const __m512d q_z,
					      const __m512d q_w) {
                         
                            RotM9x8v8 rmat{};
			    __m512d t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10;
			    t0  = _mm512_mul_pd(q_x,q_x);
			    t1  = _mm512_add_pd(_mm512_add_pd(q_x,q_y),q_w);
			    t2  = _mm512_mul_pd(t1,t1);
			    t3  = _mm512_sub_pd(t0,t2); //qq
			    t5  = _mm512_mul_pd(q_y,q_z);
			    t6  = _mm512_mul_pd(q_z,q_w);
			    t7  = _mm512_mul_pd(q_w,q_y);
			    t8  = _mm512_mul_pd(q_x,q_w);
			    t9  = _mm512_mul_pd(q_x,q_y);
			    t10 = _mm512_mul_pd(q_x,q_z);
			    t4 = _mm512_mul_pd(q_y,q_y);
                            rmat.m_vRow1 = _mm512_fmadd_pd(v16f32_2,t4,t3);
			    rmat.m_vRow2 = _mm512_mul_pd(v16f32_2,_mm512_sub_pd(t5,t8));
			    rmat.m_vRow3 = _mm512_mul_pd(v16f32_2,_mm512_add_pd(t7,t10));
			    rmat.m_vRow4 = _mm512_mul_pd(v16f32_2,_mm512_add_pd(t5,t8));
			    rmat.m_vRow5 = _mm512_fmadd_pd(v16f32_2,_mm512_mul_pd(q_z,q_z),t3);
			    rmat.m_vRow6 = _mm512_mul_pd(v16f32_2,_mm512_sub_pd(t6,t9));
			    rmat.m_vRow7 = _mm512_mul_pd(v16f32_2,_mm512_sub_pd(t7,t8));
			    rmat.m_vRow8 = _mm512_mul_pd(v16f32_2,_mm512_add_pd(t6,t9));
			    rmat.m_vRow9 = _mm512_fmadd_pd(v16f32_2,_mm512_mul_pd(q_w,q_w),t3);
			    return (rmat);
		    }


                  EA3x16v16
                  q4x16_to_ea3x16_zmm16r4(    const __m512 q_x,
		                              const __m512 q_y,
					      const __m512 q_z,
					      const __m512 q_w) {

		         EA3x16v16 ea;
                         const __m512 qxw = _mm512_fmadd_ps(q_x,q_x,_mm512_mul_ps(q_w,q_w));
			 const __m512 qyz = _mm512_fmadd_ps(q_y,q_y,_mm512_mul_ps(q_z,q_z));
			 const __m512 chi = _mm512_sqrt_ps(_mm512_mul_ps(qxw,qyz));
			 __m512 alpha_c;
			 __m512 beta_c;
			 __m512 gamma_c;
			 __mmask16 qyz0 = 0x0;
			 __mmask16 qxw0 = 0x0;
			 __mmask16 k1   = 0x0;
			 __mmask16 k2   = 0x0;
			 __mmask16 k3   = 0x0;
			 qyz0 = _mm512_cmp_ps_mask(qyz0,v16_0,_CMP_EQ_OQ);
			 qxw0 = _mm512_cmp_ps_mask(qxw0,v16_0,_CMP_EQ_OQ);
			 if(1==qyz0) {
			    ea.alpha = _mm512_atan2_ps(_mm512_mul_ps(v16_n2,_mm512_mul_ps(q_x,q_w)),
			                              _mm512_fmsub_ps(q_x,q_x,_mm512_mul_ps(q_w,q_w)));
			    ea.beta  = v16_0;
			    ea.gamma = v16_0;
			 } else if(1==qxw0) {
                            ea.alpha = _mm512_atan2_ps(_mm512_mul_ps(v16_2,_mm512_mul_ps(q_y,q_z)),
			                              _mm512_fmsub_ps(q_y,q_y,_mm512_mul_ps(q_z,q_z)));
			    ea.beta = v16_pi;
			    ea.gamma = v16_0;
			 }
			 else {
			    const __m512 t0 = _mm512_mul_ps(q_y,q_w);
			    const __m512 c0 = _mm512_fmadd_ps(q_x,q_z,t0);
			    const __m512 t1 = _mm512_mul_ps(q_z,q_w);
			    const __m512 c1 = _mm512_fmsub_ps(q_x,q_y,t1);
			 
			    ea.alpha = _mm512_atan2_ps(_mm512_mul_ps(chi,_mm512_mul_ps(c0,v16_n1)),
			                            _mm512_mul_ps(chi,_mm512_mul_ps(c1,v16_n1)));
			    ea.beta  = _mm512_atan2_ps(_mm512_mul_ps(v16_2,chi),_mm512_sub_ps(qxw,qyz));
			    const __m512 c2 = _mm512_fmadd_ps(q_x,_qy,t1);
			    ea.gamma = _mm512_atan2_ps(_mm512_mul_ps(chi,_mm512_mul_ps(c0,v16_1)),
			                            _mm512_mul_ps(chi,_mm512_mul_ps(c2,v16_n1)));
			 }
			 alpha_c = ea.alpha;
			 const __m512 tmp0 = _mm512_fmadd_ps(v16_2,v16_pi,alpha_c);
			 k1 = _mm512_cmp_ps_mask(alpha_c,v16_0,_CMP_LT_OQ);
			 ea.alpha = _mm512_mask_mov_ps(alpha_c,k1,fmod_zmm16r4(tmp0,v16_2pi));
			 beta_c = ea.beta;
			 const _mm512 tmp1 = _mm512_fmadd_ps(v16_2,v16_pi,beta_c);
			 k2 = _mm512_cmp_ps_mask(beta_c,v16_0,k2,_CMP_LT_OQ);
			 ea.beta = _mm512_mask_mov_ps(beta_c,k2,fmod_zmm16r4(tmp1,v16_pi));
			 gamma_c = ea.gamma;
			 const __m512 tmp2 = _mm512_fmadd_ps(v16_2,v16_pi,gamma_c);
			 k3 = _mm512_cmp_ps_mask(gamma_c,v16_0,k3,_CMP_LT_OQ);
			 ea.gamma = _mm512_mask_mov_ps(gamma_c,k3,fmod_zmm16r4(tmp2,v16_2pi));
			 return (ea);
		}


                EA3x8v8
                q4x8_to_ea3x8_zmm8r8(     const __m512d q_x,
		                          const __m512d q_y,
					  const __m512d q_z,
					  const __m512d q_w) {
					     
		         EA3x8v8 ea;
                         const __m512d qxw = _mm512_fmadd_pd(q_x,q_x,_mm512_mul_pd(q_w,q_w));
			 const __m512d qyz = _mm512_fmadd_pd(q_y,q_y,_mm512_mul_pd(q_z,q_z));
			 const __m512d chi = _mm512_sqrt_pd(_mm512_mul_pd(qxw,qyz));
			 __m512d alpha_c;
			 __m512d beta_c;
			 __m512d gamma_c;
			 __mmask8 qyz0 = 0x0;
			 __mmask8 qxw0 = 0x0;
			 __mmask8 k1   = 0x0;
			 __mmask8 k2   = 0x0;
			 __mmask8 k3   = 0x0;
			 qyz0 = _mm512_cmp_pd_mask(qyz0,v8_0,_CMP_EQ_OQ);
			 qxw0 = _mm512_cmp_pd_mask(qxw0,v8_0,_CMP_EQ_OQ);
			 if(__builtin_expect(1==qyz0,0)) {
			    ea.alpha = _mm512_atan2_ps(_mm512_mul_pd(v8_n2,_mm512_mul_pd(q_x,q_w)),
			                              _mm512_fmsub_pd(q_x,q_x,_mm512_mul_pd(q_w,q_w)));
			    ea.beta  = v8_0;
			    ea.gamma = v8_0;
			 } else if(__builtin_expect(1==qxw0,0)) {
                            ea.alpha = _mm512_atan2_pd(_mm512_mul_pd(v16_2,_mm512_mul_pd(q_y,q_z)),
			                              _mm512_fmsub_pd(q_y,q_y,_mm512_mul_pd(q_z,q_z)));
			    ea.beta = v8_pi;
			    ea.gamma = v8_0;
			 }
			 else {
			    const __m512d t0 = _mm512_mul_pd(q_y,q_w);
			    const __m512d c0 = _mm512_fmadd_pd(q_x,q_z,t0);
			    const __m512d t1 = _mm512_mul_pd(q_z,q_w);
			    const __m512d c1 = _mm512_fmsub_pd(q_x,q_y,t1);
			 
			    ea.alpha = _mm512_atan2_pd(_mm512_mul_pd(chi,_mm512_mul_pd(c0,v8_n1)),
			                            _mm512_mul_pd(chi,_mm512_mul_ps(c1,v8_n1)));
			    ea.beta  = _mm512_atan2_pd(_mm512_mul_pd(v8_2,chi),_mm512_sub_pd(qxw,qyz));
			    const __m512 c2 = _mm512_fmadd_pd(q_x,_qy,t1);
			    ea.gamma = _mm512_atan2_pd(_mm512_mul_pd(chi,_mm512_mul_pd(c0,v8_1)),
			                            _mm512_mul_pd(chi,_mm512_mul_pd(c2,v8_n1)));
			 }
			 alpha_c = ea.alpha;
			 const __m512d tmp0 = _mm512_fmadd_pd(v8_2,v16_pi,alpha_c);
			 k1 = _mm512_cmp_pd_mask(alpha_c,v8_0,_CMP_LT_OQ);
			 ea.alpha = _mm512_mask_mov_ps(alpha_c,k1,fmod_zmm8r8(tmp0,v8_2pi));
			 beta_c = ea.beta;
			 const _mm512d tmp1 = _mm512_fmadd_ps(v8_2,v8_pi,beta_c);
			 k2 = _mm512_cmp_pd_mask(beta_c,v8_0,k2,_CMP_LT_OQ);
			 ea.beta = _mm512_mask_mov_pd(beta_c,k2,fmod_zmm8r8(tmp1,v8_pi));
			 gamma_c = ea.gamma;
			 const __m512d tmp2 = _mm512_fmadd_pd(v8_2,v8_pi,gamma_c);
			 k3 = _mm512_cmp_pd_mask(gamma_c,v8_0,k3,_CMP_LT_OQ);
			 ea.gamma = _mm512_mask_mov_pd(gamma_c,k3,fmod_zmm8r8(tmp2,v8_2pi));
			 return (ea);
		}
