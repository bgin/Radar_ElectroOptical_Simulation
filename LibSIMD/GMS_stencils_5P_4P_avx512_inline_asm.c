

#include "GMS_stencils_5P_4P_avx512_inline_asm.h"



                        __m512d
                        stencil_5P_zmm8r8(__m512d (*f) (__m512d),
                                          const __m512d vx,
		                          const __m512d vh,
                                          __m512d * __restrict verr_ro,
		                          __m512d * __restrict verr_tr) {

                             
                             
                               const __m512d vn0   = _mm512_setzero_pd(); // 0x0000000000000000
                               const __m512d vn0_5 = _mm512_set1_pd(0.5); // 0x3FE0000000000000
			       const __m512d vn1_3 = _mm512_set1_pd(0.33333333333333333333333333); // 0x3FD5555555555555 
			       const __m512d vn4_3 = _mm512_set1_pd(1.33333333333333333333333333); // 0x3FF5555555555555 
			       const __m512d vn2   = _mm512_set1_pd(2.0); // 0x4000000000000000
			       const __m512d veps  = _mm512_set1_pd(2.2204460492503131e-16); // 0x3CB0000000000000
			       const __m512d mask  = _mm512_set1_pd(0x7FFFFFFFFFFFFFF);
			       __m512d vp1;
			       __m512d vp2;
			       __m512d vp1h;
			       __m512d vp2h;
			       __m512d vt0;
			       __m512d vt1;
			       __m512d vt2;
			       __m512d vt3;
			       __m512d vt4;
			       __m512d vtmp0;
			       __m512d vtmp1;
			       __m512d vtmp2;
			       __m512d vtmp3;
			       asm {
			        
			             vmovups ZMMWORD PTR [192+rsp], zmm0
			             mov r13, rdx
			             vmovups ZMMWORD PTR [256+rsp], zmm1
			             mov r12, rsi
			             vsubpd   zmm0, zmm0, zmm1
			             call rbx
			             vmovups  ZMMWORD PTR [64+rsp], zmm0
			             vmovups  zmm16, ZMMWORD PTR [256+rsp]
			             vmovups  zmm0,  zmm16, ZMMWORD PTR [192+rsp]
			             call rbx
			             vmovups  ZMMWORD PTR [rsp], zmm0
			             vmovups  zmm16, ZMMWORD PTR [256+rsp]
			             vmovups  zmm17, ZMMWORD PTR [192+rsp]
			             vmulpd   zmm18, zmm16, vn0_5
			             vsubpd   zmm0,  zmm17, zmm18
			             vmovups  ZMMWORD PTR [128+rsp], zmm18
			             call rbx
			             vmovups  zmm25, zmm0
			             vmovups  zmm16, ZMMWORD PTR [192+rsp]
			             vaddpd   zmm0,  zmm16, ZMMWORD PTR [128+rsp]
			             vmovups  ZMMWORD PTR [320+rsp], zmm25
			             call rbx
			             vmovups  zmm25, ZMMWORD PTR [320+rsp]
			             vbroadcastsd zmm29, veps
			             vmovups  zmm19, ZMMWORD PTR [rsp]
			             vmovups  zmm20, ZMMWORD PTR [64+rsp]
			             vmovups  zmm16, ZMMWORD PTR [192+rsp]
			             vsubpd   zmm18, zmm0,  zmm25
			             vmulpd   zmm24, zmm18, vn4_3
			             vpandq   zmm21, zmm19, mask
			             vpandq   zmm22, zmm20, mask
			             vmulpd   zmm27, zmm29, zmm23
			             vpandq   zmm17, zmm16, mask
			             vmovups  zmm16, ZMMWORD PTR [256+rsp]
			             vpandq   zmm26, zmm0,  mask
			             vdivpd   zmm30, zmm17, zmm16
			             vpandq   zmm25, zmm25, mask
			             vdivpd   zmm17, zmm24, zmm16
			             vmulpd   zmm25, zmm26, zmm25
			             vdivpd   zmm28, zmm27, zmm16
			             vmaxpd   zmm31, zmm17, zmm28
			             vfmadd213pd zmm25, zmm29, zmm27
			             vmulpd   zmm27, zmm29, zmm30
			             vdivpd   zmm16, zmm25, zmm16
			             vmovups  zmm0,  zmm17
			             vmulpd   zmm26, zmm31, zmm27
			             vpandq   zmm16, zmm16, mask
			             vmulpd   zmm16, zmm16, zmm26
			             vmovups  ZMMWORD PTR [r12], zmm16
			             vpandq   zmm16, zmm17, mask
			             vmovups  ZMMWORD PTR [r13], zmm16
			       }
		           }



                        __m512d
			stencil_5P_central_zmm8r8_optim(__m512d (*f) (__m512d),
			                                const __m512d vx,
						        const __m512d vh,
						        __m512d * __restrict vabserr) {
                            const __m512d vn0   = _mm512_setzero_pd();
                            const __m512d vn2   = _mm512_set1_pd(2.0);
			    const __m512d vn1_3 = _mm512_set1_pd(0.33333333333333333333333333333333);
			    const __m512d vn4   = _mm512_set1_pd(4.0);
			    __m512d vx0     = vn0;
			    __m512d verr_ro = vn0;
			    __m512d verr_tr = vn0;
			    __m512d verr    = vn0;
			    __mmask8 vb1    = 0;
			    __mmask8 vb2    = 0;
			    __mmask8 vb3    = 0;
			    __mmask8 vb4    = 0;
			    vx0 = stencil_5P_zmm8r8(f,vx,vh,&verr_ro,&verr_tr);
			    verr = _mm512_add_pd(verr_ro,verr_tr);
			    vb1  = _mm512_cmp_pd_mask(verr_ro,vn0,_CMP_GT_OQ) && 
			           _mm512_cmp_pd_mask(verr_tr,vn0,_CMP_GT_OQ);
			    vb2  = _mm512_cmp_pd_mask(verr_ro,verr_tr,_CMP_LT_OQ);
			    if(vb1 && vb2) {
                                __m512d vx02     = vn0;
			        __m512d verr_ro2 = vn0;
			        __m512d verr_tr2 = vn0;
			        __m512d verr2    = vn0;
				__m512d vh2      = vn0;
				__m512d vt0;
				__m512d tmp0;
				vt0   = _mm512_div_pd(verr_r0,
				                       _mm512_add_pd(verr_ro,verr_tr));

				vh2   = _mm512_mul_pd(vh,_mm512_pow_pd(vt0,vn1_3));
				vx02  = stencil_5P_zmm8r8(f,vx,vh,&verr_ro2,&verr_tr2);
				verr2 = _mm512_add_pd(verr_ro2,verr_tr2);
				vb3   = _mm512_cmp_pd_mask(verr2,verr,_CMP_LT_OQ);
				tmp0  = _mm512_abs_pd(_mm512_sub_pd(vx02,vx0));
				vb4   = _mm512_cmp_pd_mask(tmp0,
				                     _mm512_mul_pd(vn4,verr),_CMP_LT_OQ);
				if(vb3 && vb4) {
                                   vx0 = vx02;
				   verr = verr2;
				}
			    }
			    *vabserr = err;
			    return (vx0);
			}



                         __m512d
			 stencil_4P_zmm8r8(__m512d (*f)(__m512d),
			                   const __m512d vx,
					   const __m512d vh,
					   __m512d * __restrict verr_ro,
					   __m512d * __restrict verr_tr) {
                                 const __m512d vn0   = _mm512_setzero_pd();
				 const __m512d vn1_4  = _mm512_set1_pd(0.25);
				 const __m512d vn1_2  = _mm512_set1_pd(0.5);
				 const __m512d vn3_4  = _mm512_set1_pd(0.75);
				 const __m512d vn22_3 = _mm512_set1_pd(7.3333333333333333333333);
				 const __m512d vn62_3 = _mm512_set1_pd(20.6666666666666666666667);
				 const __m512d vn52_3 = _mm512_set1_pd(17.3333333333333333333333);
				 const __m512d vn2    = _mm512_set1_pd(2.0);
				 const __m512d vn4134 = _mm512_set1_pd(41.34);
				 const __m512d veps  = _mm512_set1_pd(2.2204460492503131e-16);
				 __m512d dydx = vn0;
				 __m512d vp1  = vn0;
				 __m512d vp2  = vn0;
				 __m512d vp3  = vn0;
				 __m512d vp4  = vn0;
				 __m512d vt0  = vn0;
				 __m512d vt1  = vn0;
				 __m512d vt2  = vn0;
				 __m512d vt3  = vn0;
				 __m512d vtmp0 = vn0;
				 __m512d vtmp1 = vn0;
				 __m512d vtmp2 = vn0;
				 __m512d vtmp3 = vn0;
				 __m512d vtmp4 = vn0;
				 __m512d vtmp5 = vn0;
				 __m512d vtmp6 = vn0;
				 __m512d vtmp7 = vn0;
				 vp1 = f(_mm512_fmadd_pd(vh,vn1_4,vx));
				 vtmp7 = _mm512_div_pd(vx,vh);
				 vp2 = f(_mm512_fmadd_pd(vh,vn1_2,vx));
				 vtmp0 = _mm512_mul_pd(vv52_3,_mm512_sub_pd(vp2,vp1));
				 vp3 = f(_mm512_fmadd_pd(vh,vn3_4,vx));
				 vtmp1 = _mm512_mul_pd(vn62_3,_mm512_sub_pd(vp3,vp2));
				 vp4 = f(_mm512_add_pd(vx,vh));
				 vtmp2 = _mm512_mul_pd(vn22_3,_mm512_sub_pd(vp4,vp3));
				 vt0 = _mm512_mul_pd(vn2,_mm512_sub_pd(vp4,vp2));
				 vtmp5 = _mm512_div_pd(vt0,vh);
				 vt1 = _mm512_add_pd(_mm512_sub_pd(vtmp2,vtmp1),vtmp0);
				 vtmp6 = _mm512_div_pd(vt1,vh);
				 vtmp0 = _mm512_abs_pd(vp4);
				 vtmp1 = _mm512_abs_pd(vp3);
				 vtmp2 = _mm512_abs_pd(vp2);
				 vtmp3 = _mm512_abs_pd(vp1);
				 vtmp4 = _mm512_add_pd(_mm512_add_pd(vtmp0,vtmp1),
				                             _mm512_add_pd(vtmp2,vtmp3));
				 vt2   = _mm512_mul_pd(vn4134,
				                           _mm512_mul_pd(vtmp4,veps));
				 vtmp0 = _mm512_max_pd(_mm512_abs_pd(vtmp7),
				                              _mm512_abs_pd(vtmp6));
				 vt3   = _mm512_mul_pd(vtmp0,
				                     _mm512_mul_pd(vtmp7,veps));
				 dydx  = vtmp6;
				 *verr_tr = _mm512_abs_pd(_mm512_div_pd(
				                                _mm512_sub_pd(vt2,vt0),vh));
				 *verr_ro = _mm512_add_pd(_mm512_abs_pd(
				                                _mm512_div_pd(vt2,vh),vt3));
				 return (dydx);

			  }



                        __m512d
			stencil_4P_forward_zmm8r8_optim(__m512d (*f) (__m512d),
			                                const __m512d vx,
							const __m512d vh,
							__m512d * __restrict vabserr) {

                                 const __m512d vn0   = _mm512_setzero_pd();
				 const __m512d v1_2  = _mm512_set1_pd(0.5);
				 const __m512d vn4   = _mm512_set1_pd(4.0);
				 __m512d vx0     = vn0;
			         __m512d verr_ro = vn0;
			         __m512d verr_tr = vn0;
			         __m512d verr    = vn0;
			         __mmask8 vb1    = 0;
			         __mmask8 vb2    = 0;
			         __mmask8 vb3    = 0;
			         __mmask8 vb4    = 0;
			         vx0 = stencil_4P_zmm8r8(f,vx,vh,&verr_ro,&verr_tr);
			         verr = _mm512_add_pd(verr_ro,verr_tr);
			         vb1  = _mm512_cmp_pd_mask(verr_ro,vn0,_CMP_GT_OQ) && 
			                _mm512_cmp_pd_mask(verr_tr,vn0,_CMP_GT_OQ);
			         vb2  = _mm512_cmp_pd_mask(verr_ro,verr_tr,_CMP_LT_OQ);
			         if(vb1 && vb2) {
                                    __m512d vx02     = vn0;
			            __m512d verr_ro2 = vn0;
			            __m512d verr_tr2 = vn0;
			            __m512d verr2    = vn0;
				    __m512d vh2      = vn0;
				    __m512d vt0;
				    __m512d vtmp0;
				    __m512d vtmp1;
				    vtmp0 = _mm512_div_pd(verr_ro,verr_tr);
				    vh2   = _mm512_mul_pd(vh,tmp0);
				    vx02  = stencil_4P_zmm8r8(f,vx,vh,&verr_r02,&verr_tr2);
                                    verr2 = _mm512_add_pd(verr_ro2,verr_tr2);
				    vb3   = _mm512_cmp_pd_mask(verr2,verr,_CMP_LT_OQ);
				    vtmp1 = _mm512_abs_pd(_mm512_sub_pd(vx02,vx0));
				    vb4   = _mm512_cmp_pd_mask(vtmp1,_mm512_mul_pd(vn4,verr),_CMP_LT_OQ);
				    if(vb3 && vb4) {
                                       vx0 = vx02;
				       verr = verr2;
				    }
				}
				*vabserr = verr;
				return (vx0);
		        }



                        __m512d
			stencil_4P_backward_zmm8r8_optim(__m512d (*f) (__m512d),
			                                 const __m512d vx,
							 const __m512d vh,
							 __m512d * __restrict vabserr) {
                              const __m512d vxhn = _mm512_sub_pd(_mm512_setzero_pd(),vh);
			      return (stencil_4P_forward_zmm8r8_optim(f,vx,vxhn,vabserr));
			}



