
#include "GMS_rotation_kernels_avx512.h"
#include "GMS_rotations_avx512_helpers.h"

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

                    /*
                       Convert unit quaternion to axis angle pair
                    */
                AX4x16v16
		q4x16_to_ax4x16_zmm16r4( const __m512 q_x,
		                         const __m512 q_y,
					 const __m512 q_z,
					 const __m512 q_w) {
		          AX4x16v16 aa;
		          const register __m512 t0 = _mm512_mul_ps(q_y,q_y);
			  const register __m512 t1 = _mm512_mul_ps(q_z,q_z);
			  const register __m512 t2 = _mm512_mul_ps(q_w,q_w);
			  const register __m512 v0 = _mm512_add_ps(t0,_mm512_add_ps(t1,t2));
			  __m512 c0 = v16_0;
			  __m512 c1 = v16_0;
			  __mmask16 k1 = 0x0;
			  __mmask16 k2 = 0x0;
			  k1 = _mm512_cmp_ps_mask(v0,v16_0,_CMP_EQ_OQ);
			  if(__builtin_expect(1==k1,0)) {
                             aa.ax_1 = v16_0;
			     aa.ax_2 = v16_0;
			     aa.ax_3 = v16_1;
			     aa.ax_4 = v16_0;
			     return (aa);
			  }
			  k2 = _mm512_cmp_ps_mask(q_x,_v16_0,_CMP_NEQ_OQ);
			  if(1==k2) {
                             const register __m512 s = _mm512_div_ps(
			                          zmm16r4_sign_zmm16r4(v16_1,q_x),
						  norm2_zmm16r4(q_y,q_z,q_w));
			     aa.ax_1 = _mm512_mul_ps(q_y,s);
			     aa.ax_2 = _mm512_mul_ps(q_z,s);
			     aa.ax_2 = _mm512_mul_ps(q_w,s);
			     const register __m512 omega = _mm512_mul_ps(v16_2,
			                              _mm512_acos_ps(clip_zmm16r4(q_x,v16_n1,v16_1)));
			     aa.ax_4 = omega;
			  }
			  else {
                             aa.ax_1 = q_y;
			     aa.ax_2 = q_z;
			     aa.ax_3 = q_w;
			     aa.ax_4 = v16_pi;
			     
			  }
			  return (aa);
               }


	        AX4x8v8
		q4x8_to_ax4x8_zmm8r8(   const __m512d q_x,
		                        const __m512d q_y,
					const __m512d q_z,
					const __m512d q_w) {
			  AX4x8v8 aa;
                          const register __m512d t0 = _mm512_mul_pd(q_y,q_y);
			  const register __m512d t1 = _mm512_mul_pd(q_z,q_z);
			  const register __m512d t2 = _mm512_mul_pd(q_w,q_w);
			  const register __m512d v0 = _mm512_add_pd(t0,_mm512_add_pd(t1,t2));
			  __mmask8 k1 = 0x0;
			  __mmask8 k2 = 0x0;
			  k1 = _mm512_cmp_pd_mask(v0,v8_0,_CMP_EQ_OQ);
			  if(__builtin_expect(1==k1,0)) {
                             aa.ax_1 = v8_0;
			     aa.ax_2 = v8_0;
			     aa.ax_3 = v8_1;
			     aa.ax_4 = v8_0;
			     return (aa);
			  }
			  k2 = _mm512_cmp_pd_mask(q_x,_v8_0,_CMP_NEQ_OQ);
			  if(1==k2) {
                             const register __m512 s = _mm512_div_pd(
			                          zmm8r8_sign_zmm8r8(v8_1,q_x),
						  norm2_zmm8r8(q_y,q_z,q_w));
			     aa.ax_1 = _mm512_mul_pd(q_y,s);
			     aa.ax_2 = _mm512_mul_pd(q_z,s);
			     aa.ax_2 = _mm512_mul_pd(q_w,s);
			     const register __m512 omega = _mm512_mul_pd(v16_2,
			                              _mm512_acos_pd(clip_zmm8r8(q_x,v8_n1,v8_1)));
			     aa.ax_4 = omega;
			  }
			  else {
                             aa.ax_1 = q_y;
			     aa.ax_2 = q_z;
			     aa.ax_3 = q_w;
			     aa.ax_4 = v8_pi;
			     
			  }
			  return (aa);

	       }


	          RV4x16v16
		  q4x16_to_rv4x16_zmm16r4( const __m512 q_x,
		                           const __m512 q_y,
					   const __m512 q_w,
					   const __m512 q_z) {
                          RV4x16v16 rv;
                          const register __m512 thr = _mm512_set1_ps(1.0e-8);
			  register __m512 t0 = v16_0;
			  register __m512 s  = v16_0;
			  __mmask16 k1 = 0x0;
			  __mmask16 k2 = 0x0;
			  k1 = _mm512_cmp_ps_mask(_mm512_abs_ps(q_x),thr,_CMP_LT_OQ);
			  if(__builtin_expect(1==k1,0)) {
                             r_x = q_y;
			     r_y = q_z;
			     r_z = q_w;
			     r_w = v16_inf;
			  }
			  else {
                               s   = norm2_zmm16r4(q_y,q_z,q_w);
			       k2  = _mm512_cmp_ps_mask(s,thr,_CMP_LT_OQ);
			       r_x = _mm512_mask_blend_ps(k2,_mm512_div_ps(q_y,s),v16_0);
			       t0  = _mm512_acos_ps(clip_zmm16r4(q_x,v16_n1,v16_1));
			       r_y = _mm512_mask_blend_ps(k2,_mm512_div_ps(q_z,s),v16_0);
			       r_z = _mm512_mask_blend_ps(k2,_mm512_div_ps(q_w,s),v16_n1);
			       r_w = _mm512_mask_blend_ps(k2,_mm512_tan_ps(t0),v16_0);
			  }
			  return (rv);
		}



		 RV4x8v8
		 q4x8_to_rv4x8_zmm8r8(  const __m512d q_x,
		                        const __m512d q_y,
					const __m512d q_z,
					const __m512d q_w) {

			  RV4x8v8 rv;
                          const register __m512d thr = _mm512_set1_pd(1.0e-8);
			  register __m512d t0 = v8_0;
			  register __m512d s  = v8_0;
			  __mmask8 k1 = 0x0;
			  __mmask8 k2 = 0x0;
			  k1 = _mm512_cmp_pd_mask(_mm512_abs_pd(q_x),thr,_CMP_LT_OQ);
			  if(__builtin_expect(1==k1,0)) {
                             rv.r_x = q_y;
			     rv.r_y = q_z;
			     rv.r_z = q_w;
			     rv.r_w = v8_inf;
			  }
			  else {
                               s   = norm2_zmm8r8(q_y,q_z,q_w);
			       k2  = _mm512_cmp_pd_mask(s,thr,_CMP_LT_OQ);
			       rv.r_x = _mm512_mask_blend_pd(k2,_mm512_div_pd(q_y,s),v8_0);
			       t0  = _mm512_acos_pd(clip_zmm8r8(q_x,v8_n1,v8_1));
			       rv.r_y = _mm512_mask_blend_pd(k2,_mm512_div_pd(q_z,s),v8_0);
			       rv.r_z = _mm512_mask_blend_pd(k2,_mm512_div_pd(q_w,s),v8_n1);
			       rv.r_w = _mm512_mask_blend_pd(k2,_mm512_tan_pd(t0),v8_0);
			  }

			  return (rv);
		}


		 /*
                           Orientation i.e. (Direct Cosine Matrix)  matrix to Euler angles.
                       */


	         EA3x16v16
		 rmat9x16_to_ea3x16_zmm16r4(const RotM9x16v16 rm) {

		           EA3x16v16 ea;
                           const    __m512  thr  = _mm512_set1_ps(1.0e-8);
			   register __m512  t0   = v16_0;
			   register __m512  t1   = v16_0;
			   register __m512  t2   = v16_0;
			   register __m512  zeta = v16_0;
			   register __m512  al_c = v16_0;
			   register __m512  be_c = v16_0;
			   register __m512  ga_c = v16_0;
			   
			   __mmask16 k1 = 0x0;
			   __mmask16 k2 = 0x0;
			   __mmask16 k3 = 0x0;
			   __mmask16 k4 = 0x0;
			   k1 = _mm512_cmp_ps_mask(rm.m_vRow9,v16_1,_CMP_NEQ_OQ);
			   t0 = _mm512_mul_ps(rm.m_vRow9,rm.m_vRow9);
			   zeta = _mm512_div_ps(_mm512_sqrt_ps(_mm512_sub_ps(v16_1,t0)));
			   t0 = _mm512_sub_ps(v16_0,rm.vRow8);
			   ea.alpha = _mm512_mask_blend_ps(k1,_mm512_atan2_ps(rm.vRow2,rm.vRow1),
			                                   _mm512_atan2_ps(_mm512_mul_ps(rm.vRow7,zeta),
			   				                   _mm512_mul_ps(t0,zeta)));
			   t1    = _mm512_mul_ps(v16_1o2,_mm512_mul_ps(v16_pi,_mm512_sub_ps(v16_1,rm.vRow9)));
			   ea.beta  = _mm512_mask_blend_ps(k1,t1,_mm512_acos_ps(rm.vRow9));
			   ea.gamma = _mm512_mask_blend_ps(k1,v16_0,_mm512_atan_ps(_mm512_mul_ps(rm.vRow3,zeta),
			                                                        _mm512_mul_ps(rm.vRow6,zeta)));
			   al_c = ea.alpha;
			   be_c = ea.beta;
			   ga_c = ea.gamma;
			   k2 = _mm512_cmp_ps_mask(_mm512_abs_ps(ea.alpha),thr,_CMP_LT_OQ);
			   ea.alpha = _mm512_mask_mov_ps(al_c,k2,v16_0);
			   k3 = _mm512_cmp_ps_mask(_mm512_abs_ps(ea.beta),thr,_CMP_LT_OQ);
			   ea.beta = _mm512_mask_mov_ps(be_c,k3,v16_0);
			   k4 = _mm512_cmp_ps_mask(_mm512_abs_ps(ea.gamma),thr,_CMP_LT_OQ);
			   ea.gamma = _mm512_mask_mov_ps(ga_c,k4,v16_0);
			   //al_c = alpha;
			   t0 = _mm512_add_ps(ea.alpha,v16_2pi);
			   k2 = _mm512_cmp_ps_mask(ea.alpha,v16_0,_CMP_LT_OQ);
			   ea.alpha = _mm512_mask_mov_ps(al_c,k2,fmod_zmm16r4(t0,v16_2pi));
			   //be_c = beta;
			   t1 = _mm512_add_ps(ea.beta,v16_2pi);
			   k3 = _mm512_cmp_ps_mask(ea.beta,v16_0,_CMP_LT_OQ);
			   ea.beta = _mm512_mask_mov_ps(be_c,k3,fmod_zmm16r4(t1,v16_pi));
			   //ga_c = gamma;
			   t2 = _mm512_add_ps(ea.gamma,v16_2pi);
			   k4 = _mm512_cmp_ps_mask(ea.gamma,v16_0,_CMP_LT_OQ);
			   ea.gamma = _mm512_mask_mov_ps(ga_c,k4,fmod_zmm16r4(t2,v16_2pi));
			   return (ea);
		 }



		  EA3x8v8
                  rmat9x8_to_ea3x8_zmm8r8(const RotM9x8v8 rm) {

		           EA3x8v8 ea;
                           const    __m512d  thr  = _mm512_set1_pd(1.0e-8);
			   register __m512d  t0   = v8_0;
			   register __m512d  t1   = v8_0;
			   register __m512d  t2   = v8_0;
			   register __m512d  zeta = v8_0;
			   register __m512d  al_c = v8_0;
			   register __m512d  be_c = v8_0;
			   register __m512d  ga_c = v8_0;
			   
			   __mmask8 k1 = 0x0;
			   __mmask8 k2 = 0x0;
			   __mmask8 k3 = 0x0;
			   __mmask8 k4 = 0x0;
			   k1 = _mm512_cmp_pd_mask(rm.m_vRow9,v8_1,_CMP_NEQ_OQ);
			   t0 = _mm512_mul_pd(rm.m_vRow9,rm.m_vRow9);
			   zeta = _mm512_div_pd(_mm512_sqrt_pd(_mm512_sub_pd(v8_1,t0)));
			   t0 = _mm512_sub_pd(v8_0,rm.vRow8);
			   ea.alpha = _mm512_mask_blend_pd(k1,_mm512_atan2_pd(rm.vRow2,rm.vRow1),
			                                   _mm512_atan2_pd(_mm512_mul_pd(rm.vRow7,zeta),
			   				                   _mm512_mul_pd(t0,zeta)));
			   t1    = _mm512_mul_pd(v8_1o2,_mm512_mul_pd(v8_pi,_mm512_sub_pd(v8_1,rm.vRow9)));
			   ea.beta  = _mm512_mask_blend_pd(k1,t1,_mm512_acos_pd(rm.vRow9));
			   ea.gamma = _mm512_mask_blend_pd(k1,v8_0,_mm512_atan_pd(_mm512_mul_pd(rm.vRow3,zeta),
			                                                        _mm512_mul_pd(rm.vRow6,zeta)));
			   al_c = ea.alpha;
			   be_c = ea.beta;
			   ga_c = ea.gamma;
			   k2 = _mm512_cmp_pd_mask(_mm512_abs_pd(ea.alpha),thr,_CMP_LT_OQ);
			   ea.alpha = _mm512_mask_mov_pd(al_c,k2,v8_0);
			   k3 = _mm512_cmp_pd_mask(_mm512_abs_pd(ea.beta),thr,_CMP_LT_OQ);
			   ea.beta = _mm512_mask_mov_pd(be_c,k3,v8_0);
			   k4 = _mm512_cmp_pd_mask(_mm512_abs_pd(ea.gamma),thr,_CMP_LT_OQ);
			   ea.gamma = _mm512_mask_mov_pd(ga_c,k4,v8_0);
			   //al_c = alpha;
			   t0 = _mm512_add_pd(ea.alpha,v8_2pi);
			   k2 = _mm512_cmp_pd_mask(ea.alpha,v8_0,_CMP_LT_OQ);
			   ea.alpha = _mm512_mask_mov_pd(al_c,k2,fmod_zmm8r8(t0,v8_2pi));
			   //be_c = beta;
			   t1 = _mm512_add_pd(ea.beta,v8_2pi);
			   k3 = _mm512_cmp_pd_mask(ea.beta,v8_0,_CMP_LT_OQ);
			   ea.beta = _mm512_mask_mov_pd(be_c,k3,fmod_zmm8r8(t1,v8_pi));
			   //ga_c = gamma;
			   t2 = _mm512_add_pd(ea.gamma,v8_2pi);
			   k4 = _mm512_cmp_pd_mask(ea.gamma,v8_0,_CMP_LT_OQ);
			   ea.gamma = _mm512_mask_mov_pd(ga_c,k4,fmod_zmm8r8(t2,v8_2pi));
			   return (ea);
		  }
