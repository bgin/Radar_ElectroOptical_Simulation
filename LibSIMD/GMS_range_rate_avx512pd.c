

#include "GMS_range_rate_avx512pd.h"
#include "GMS_simd_utils.h"

/*LICENSE:
*
*The source code is in the public domain and not licensed or under
*copyright. The information and software may be used freely by the public.
*As required by 17 U.S.C. 403, third parties producing copyrighted works
*consisting predominantly of the material produced by U.S. government
*agencies must provide notice with such work(s) identifying the U.S.
*Government material incorporated and stating that such material is not
*subject to copyright protection.
*
*Derived works shall not identify themselves in a manner that implies an
*endorsement by or an affiliation with the Naval Research Laboratory.
*
*RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF THE
*SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY THE NAVAL
*RESEARCH LABORATORY FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE ACTIONS
*OF RECIPIENT IN THE USE OF THE SOFTWARE.
@@Modified by Bernard Gingold, on 14-05-2022 11:11 +00200 (SAT 14 MAY 2022 11:11 GMT+2)
  contact: beniekg@gmail.com
*/

/*MIT License
Copyright (c) 2020 Bernard Gingold
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/



           __m512d
	   range_rate_2d_zmm8r8(                   const __m512d tar_x,
		                                   const __m512d tar_y,
						   const __m512d tar_xD,
						   const __m512d tar_yD,
						   const __m512d tx_x,
						   const __m512d tx_y,
						   const __m512d tx_xD,
						   const __m512d tx_yD,
						   const __m512d rx_x,
						   const __m512d rx_y,
						   const __m512d rx_xD,
						   const __m512d rx_yD,
						   const bool useHalfRange) {

                        __m512d rr = _mm512_setzero_pd(); // return value i.e. rate-range
			const __m512d half = _mm512_set1_pd(0.5);
			__m512d dtr0,dtr1,dtl0,dtl1,mag;
			__m512d t0,t1,t2,t3,t4;
			dtr0 = _mm512_sub_pd(tar_x,rx_x);
			dtr1 = _mm512_sub_pd(tar_y,rx_x);
			mag  = _mm512_sqrt_pd(_mm512_add_pd(_mm512_mul_pd(dtr0,dtr0),
			                                    _mm512_mul_pd(dtr1,dtr1)));
		        dtr0 = _mm512_div_pd(dtr0,mag);
			dtl0 = _mm512_sub_pd(tar_x,tx_x);
			dtl1 = _mm512_sub_pd(tar_y,tx_x);
			dtr1 = _mm512_div_pd(dtr1,mag);
			mag  = _mm512_sqrt_pd(_mm512_add_pd(_mm512_mul_pd(dtl0,dtl0),
			                                    _mm512_mul_pd(dtl1,dtl1)));
		        dtl0 = _mm512_div_pd(dtl0,mag);
			dtl1 = _mm512_div_pd(dtl1,mag);
			t0   = _mm512_sub_pd(_mm512_mul_pd(dtr0,rx_xD),
			                     _mm512_mul_pd(dtr1,rx_yD));
			t1   = _mm512_sub_pd(_mm512_mul_pd(dtl0,tx_xD),
			                     _mm512_mul_pd(dtl1,tx_yD));
			t2   = _mm512_mul_pd(_mm512_add_pd(dtr0,dtl0),tar_xD);
			t3   = _mm512_mul_pd(_mm512_add_pd(dtr1,dtl1),tar_yD);
			t4   = _mm512_add_pd(t2,t3);
			rr   = _mm512_sub_ps(t4,_mm512_sub_pd(t1,t0));
		        if(useHalfRange) {
                           rr = _mm512_mul_pd(rr,half);
			}
			return (rr);
		 }


		  __m512d
		  range_rate_3d_zmm8r8(            const __m512d tar_x,
		                                   const __m512d tar_y,
						   const __m512d tar_z,
						   const __m512d tar_xD,
						   const __m512d tar_yD,
						   const __m512d tar_zD,
						   const __m512d tx_x,
						   const __m512d tx_y,
						   const __m512d tx_z,
						   const __m512d tx_xD,
						   const __m512d tx_yD,
						   const __m512d tx_zD,
						   const __m512d rx_x,
						   const __m512d rx_y,
						   const __m512d rx_z,
						   const __m512d rx_xD,
						   const __m512d rx_yD,
						   const __m512d rx_zD,
						   const bool useHalfRange) {

                          __m512d rr = _mm512_setzero_pd(); // return value i.e. rate-range
			  const __m512d half = _mm512_set1_pd(0.5);
			  __m512d dtr0,dtr1,dtr2,dtl0,dtl1,dtl2,mag;
		          __m512d t0,t1,t2,t3,t4,t5;
			  dtr0 = _mm512_sub_pd(tar_x,rx_x);
			  dtr1 = _mm512_sub_pd(tar_y,rx_y);
			  dtr2 = _mm512_sub_pd(tar_z,rx_z);
			  // Normalization
			  mag = _mm512_fmadd_pd(dtr0,dtr0,
			                        _mm512_fmadd_pd(dtr1,dtr1,
						_mm512_fmadd_pd(dtr2,dtr2)));
			  dtr0 = _mm512_div_pd(dtr0,mag);
			  dtl0 = _mm512_sub_pd(tar_x,tx_x);
			  dtr1 = _mm512_div_pd(dtr1,mag);
			  dtl1 = _mm512_sub_pd(tar_y,tx_y);
			  dtl2 = _mm512_sub_pd(tar_z,tx_z);
			  dtr2 = _mm512_div_pd(dtr2,mag);
			  // Normalization
			  mag  = _mm512_fmadd_pd(dtl0,dtl0,
			                        _mm512_fmadd_pd(dtl1,dtl1,
						_mm512_fmadd_pd(dlr2,dtl2)));
			  dtl0 = _mm512_div_pd(dtl0,mag);
			  t0   = _mm512_fmsub_pd(dtr0,rx_xD,
			                         _mm512_fmsub_pd(dtr1,rx_yD,
						 _mm512_fmsub_pd(dtr2,rx_zD)));
			  dtl1 = _mm512_div_pd(dtl1,mag);
			  t1   = _mm512_fmsub_pd(dtl0,tx_xD,
			                         _mm512_fmsub_pd(dtl1,tx_yD,
						 _mm512_fmsub_pd(dtl2,tx_zD)));
			  dtl2 = _mm512_div_pd(dtl2,mag);
			  t2   = _mm512_mul_pd(_mm512_add_pd(dtr0,dtl0),tar_xD);
			  t3   = _mm512_mul_pd(_mm512_add_pd(dtr1,dtl1),tar_yD);
			  t4   = _mm512_mul_pd(_mm512_add_pd(dtr2,dtl2),tar_zD);
			  t5   = _mm512_add_pd(t2,_mm512_add_pd(t3,t4));
			  rr   = _mm512_sub_pd(t5,_mm512_sub_pd(t1,t0));
			  if(useHalfRange) {
                               rr = _mm512_mul_pd(rr,half);
			  }
			  return (rr);
		    }


		     __m512d range_grad_zmm8r8( const __m512d p,
					        const __m512d Tx,
					        const __m512d Rx,
					        const bool useHalfRange) {

			  const __m512d half = _mm512_set1_pd(0.5);
			  __m512d temp,vnorm,J;
			  double norm        = 0.0;
			  //deltaTx=x-lTx;
			  temp  = _mm512_sub_pd(p,Tx);
			  norm  = _mm512_reduce_add_pd(_mm512_mul_pd(temp,temp));
			  vnorm = _mm512_set1_pd(norm);
			  vnorm = _mm512_sqrt_pd(vnorm);
			  //deltaTx.'/norm(deltaTx)
			  J     = _mm512_div_pd(temp,vnorm);
			  // eltaRx=x-lRx;
			  temp  = _mm512_sub_pd(p,Rx);
			  norm  = 0.0;
			  vnorm = _mm512_setzero_pd();
			  norm  = _mm512_reduce_add_pd(_mm512_mul_pd(temp,temp));
			  vnorm = _mm512_set1_pd(norm);
			  vnorm = _mm512_sqrt_pd(vnorm);
			  ////deltaTx=x-lTx;
			  J     = _mm512_add_pd(J,_mm512_div_pd(temp,vnorm));
			  if(useHalfRange) {
                             J  = _mm512_mul_pd(J,half);
			  }
			  return (J);
		    }


		     __m512d range_hessian_1d_zmm8r8() {
                         __m512d H = _mm512_setzero_pd();
			 return (H);
		     }


		      void range_hessian_2d_zmm8r8_a(double * __restrict __attribute__((aligned(64))) H_0,
		                                     double * __restrict __attribute__((aligned(64))) H_1,
						     double * __restrict __attribute__((aligned(64))) H_2,
						     double * __restrict __attribute__((aligned(64))) H_3,
						     const __m512d x_0,
						     const __m512d x_1,
						     const bool useHalfRange) {

			   const __m512d _2   = _mm512_set1_pd(2.0);
			   const __m512d _1   = _mm512_set1_pd(1.0);
			   const __m512d xC2  = _mm512_mul_pd(x_0,x_0);
			   const __m512d yC2  = _mm512_mul_pd(x_1,x_1);
			   const __m512d r    = _mm512_sqrt_pd(_mm512_add_pd(xC2,yC2));
			   const __m512d r3   = _mm512_mul_pd(r,_mm512_mul_pd(r,r));
			   const __m512d invr = _mm512_div_pd(_1,r);
			   _mm512_store_pd(&H_0[0],_mm512_add_pd(_mm512_div_pd(zmm8r8_negate(xC2),
			                                                    r3),invr));
			   _mm512_store_pd(&H_1[0],_mm512_div_pd(_mm512_mul_pd(zmm8r8_negate(x_0),
			                                                    x_1),r3));
			   _mm512_store_pd(&H_2[0],_mm512_load_pd(&H1[0]));
			   _mm512_store_pd(&H_3[0],_mm512_add_pd(_mm512_div_pd(zmm8r8_negate(yC2),
			                                                    r3),invr));
			   if(useHalfRange) {
                              _mm512_store_pd(&H_0[0],_mm512_mul_pd(_mm512_load_pd(&H_0[0],_2)));
			      _mm512_store_pd(&H_1[0],_mm512_mul_pd(_mm512_load_pd(&H_1[0],_2)));
			      _mm512_store_pd(&H_2[0],_mm512_mul_pd(_mm512_load_pd(&H_2[0],_2)));
			      _mm512_store_pd(&H_3[0],_mm512_mul_pd(_mm512_load_pd(&H_3[0],_2)));
			   }
		 }


		  void range_hessian_2d_zmm8r8_u(    double * __restrict H_0,
		                                     double * __restrict H_1,
						     double * __restrict H_2,
						     double * __restrict H_3,
						     const __m512d x_0,
						     const __m512d x_1,
						     const bool useHalfRange) {

			   const __m512d _2   = _mm512_set1_pd(2.0);
			   const __m512d _1   = _mm512_set1_pd(1.0);
			   const __m512d xC2  = _mm512_mul_pd(x_0,x_0);
			   const __m512d yC2  = _mm512_mul_pd(x_1,x_1);
			   const __m512d r    = _mm512_sqrt_pd(_mm512_add_pd(xC2,yC2));
			   const __m512d r3   = _mm512_mul_pd(r,_mm512_mul_pd(r,r));
			   const __m512d invr = _mm512_div_pd(_1,r);
			   _mm512_storeu_pd(&H_0[0],_mm512_add_pd(_mm512_div_pd(zmm8r8_negate(xC2),
			                                                    r3),invr));
			   _mm512_storeu_pd(&H_1[0],_mm512_div_pd(_mm512_mul_pd(zmm8r8_negate(x_0),
			                                                    x_1),r3));
			   _mm512_storeu_pd(&H_2[0],_mm512_loadu_pd(&H1[0]));
			   _mm512_storeu_pd(&H_3[0],_mm512_add_pd(_mm512_div_pd(zmm8r8_negate(yC2),
			                                                    r3),invr));
			   if(useHalfRange) {
                              _mm512_storeu_pd(&H_0[0],_mm512_mul_pd(_mm512_loadu_pd(&H_0[0],_2)));
			      _mm512_storeu_pd(&H_1[0],_mm512_mul_pd(_mm512_loadu_pd(&H_1[0],_2)));
			      _mm512_storeu_pd(&H_2[0],_mm512_mul_pd(_mm512_loadu_pd(&H_2[0],_2)));
			      _mm512_storeu_pd(&H_3[0],_mm512_mul_pd(_mm512_loadu_pd(&H_3[0],_2)));
			   }
		}


		 void range_hessian_3d_zmm8r8_a(     double * __restrict __attribute__((aligned(64))) H_0,
		                                     double * __restrict __attribute__((aligned(64))) H_1,
						     double * __restrict __attribute__((aligned(64))) H_2,
						     double * __restrict __attribute__((aligned(64))) H_3,
						     double * __restrict __attribute__((aligned(64))) H_4,
						     double * __restrict __attribute__((aligned(64))) H_5,
						     double * __restrict __attribute__((aligned(64))) H_6,
						     double * __restrict __attribute__((aligned(64))) H_7,
						     double * __restrict __attribute__((aligned(64))) H_8,
						     const __m512d x_0,
						     const __m512d x_1,
						     const __m512d x_2,
						     const bool useHalfRange) {
                           __m512d invr,invr3;
                           const __m512d _1   = _mm512_set1_pd(1.0);
			   const __m512d xC2  = _mm512_mul_pd(x_0,x_0);
			   const __m512d yC2  = _mm512_mul_pd(x_1,x_1);
			   const __m512d zC2  = _mm512_mul_pd(x_2,x_2);
			   const __m512d r    = _mm512_sqrt_pd(
			                              _mm512_add_pd(xC2,_mm512_add_pd(yC2,zC2)));
			   const __m512d r3   = _mm512_mul_pd(r,_mm512_mul_pd(r,r));
			   invr               = _mm512_div_pd(_1,r);
			   invr3              = _mm512_div_pd(_1,r3);
			   _mm512_store_pd(&H_0[0],_mm512_fmadd_pd(zmm8r8_negate(xC2),invr3,invr));
			   _mm512_store_pd(&H_1[0],_mm512_mul_pd(zmm8r8_negate(x_0),
			                                      _mm512_mul_pd(x_1,invr3)));
			   _mm512_store_pd(&H_2[0],_mm512_mul_pd(zmm8r8_negate(x_0),
			                                      _mm512_mul_pd(x_2,invr3)));
			   _mm512_store_pd(&H_3[0],_mm512_load_pd(&H_1[0]));
			   _mm512_store_pd(&H_4[0],_mm512_fmadd_pd(zmm8r8_negate(yC2),invr3,invr));
			   _mm512_store_pd(&H_5[0],_mm512_mul_pd(zmm8r8_negate(x_1),
			                                      _mm512_mul_pd(x_2,invr3)));
			   _mm512_store_pd(&H_6[0],_mm512_load_pd(&H_2[0]));
			   _mm512_store_pd(&H_7[0],_mm512_load_pd(&H_5[0]));
			   _mm512_store_pd(&H_8[0],_mm512_fmadd_pd(zmm8r8_negate(zC2),invr3,invr));
			   if(useHalfRange) {
			       const __m512d t0 = _mm512_load_pd(&H_0[0]);
                               _mm512_store_pd(&H_0[0],_mm512_add_pd(t0,t0));
			       const __m512d t1 = _mm512_load_pd(&H_1[0]);
			      _mm512_store_pd(&H_1[0],_mm512_add_pd(t1,t1));
			       const __m512d t2 = _mm512_load_pd(&H_2[0]);
			      _mm512_store_pd(&H_2[0],_mm512_add_pd(t2,t2));
			       const __m512d t3 = _mm512_load_pd(&H_3[0]);
			      _mm512_store_pd(&H_3[0],_mm512_add_pd(t3,t3));
			       const __m512d t4 = _mm512_load_pd(&H_4[0]);
			      _mm512_store_pd(&H_4[0],_mm512_add_pd(t4,t4));
			       const __m512d t5 = _mm512_load_pd(&H_5[0]);
			      _mm512_store_pd(&H_5[0],_mm512_add_pd(t5,t5));
			       const __m512d t6 = _mm512_load_pd(&H_6[0]);
			      _mm512_store_pd(&H_6[0],_mm512_add_pd(t6,t6));
			       const __m512d t7 = _mm512_load_pd(&H_7[0]);
			      _mm512_store_pd(&H_7[0],_mm512_add_pd(t7,t7));
			       const __m512d t8 = _mm512_load_pd(&H_8[0]);
			      _mm512_store_pd(&H_8[0],_mm512_add_pd(t8,t8));
			   }
		    }


		     void range_hessian_3d_zmm8r8_u(double * __restrict H_0,
		                                     double * __restrict H_1,
						     double * __restrict H_2,
						     double * __restrict H_3,
						     double * __restrict H_4,
						     double * __restrict H_5,
						     double * __restrict H_6,
						     double * __restrict H_7,
						     double * __restrict H_8,
						   const __m512d x_0,
						   const __m512d x_1,
						   const __m512d x_2,
						   const bool useHalfRange) {
                           __m512d invr,invr3;
                           const __m512d _1   = _mm512_set1_pd(1.0);
			   const __m512d xC2  = _mm512_mul_pd(x_0,x_0);
			   const __m512d yC2  = _mm512_mul_pd(x_1,x_1);
			   const __m512d zC2  = _mm512_mul_pd(x_2,x_2);
			   const __m512d r    = _mm512_sqrt_pd(
			                              _mm512_add_pd(xC2,_mm512_add_pd(yC2,zC2)));
			   const __m512d r3   = _mm512_mul_pd(r,_mm512_mul_pd(r,r));
			   invr               = _mm512_div_pd(_1,r);
			   invr3              = _mm512_div_pd(_1,r3);
			   _mm512_storeu_pd(&H_0[0],_mm512_fmadd_pd(zmm8r8_negate(xC2),invr3,invr));
			   _mm512_storeu_pd(&H_1[0],_mm512_mul_pd(zmm8r8_negate(x_0),
			                                      _mm512_mul_pd(x_1,invr3)));
			   _mm512_storeu_pd(&H_2[0],_mm512_mul_pd(zmm8r8_negate(x_0),
			                                      _mm512_mul_pd(x_2,invr3)));
			   _mm512_storeu_pd(&H_3[0],_mm512_load_pd(&H_1[0]));
			   _mm512_storeu_pd(&H_4[0],_mm512_fmadd_pd(zmm8r8_negate(yC2),invr3,invr));
			   _mm512_storeu_pd(&H_5[0],_mm512_mul_pd(zmm8r8_negate(x_1),
			                                      _mm512_mul_pd(x_2,invr3)));
			   _mm512_storeu_pd(&H_6[0],_mm512_load_pd(&H_2[0]));
			   _mm512_storeu_pd(&H_7[0],_mm512_load_pd(&H_5[0]));
			   _mm512_storeu_pd(&H_8[0],_mm512_fmadd_pd(zmm8r8_negate(zC2),invr3,invr));
			   if(useHalfRange) {
			       const __m512d t0 = _mm512_loadu_pd(&H_0[0]);
                               _mm512_storeu_pd(&H_0[0],_mm512_add_pd(t0,t0));
			       const __m512d t1 = _mm512_loadu_pd(&H_1[0]);
			      _mm512_storeu_pd(&H_1[0],_mm512_add_pd(t1,t1));
			       const __m512d t2 = _mm512_loadu_pd(&H_2[0]);
			      _mm512_storeu_pd(&H_2[0],_mm512_add_pd(t2,t2));
			       const __m512d t3 = _mm512_loadu_pd(&H_3[0]);
			      _mm512_storeu_pd(&H_3[0],_mm512_add_pd(t3,t3));
			       const __m512d t4 = _mm512_loadu_pd(&H_4[0]);
			      _mm512_storeu_pd(&H_4[0],_mm512_add_pd(t4,t4));
			       const __m512d t5 = _mm512_loadu_pd(&H_5[0]);
			      _mm512_storeu_pd(&H_5[0],_mm512_add_pd(t5,t5));
			       const __m512d t6 = _mm512_loadu_pd(&H_6[0]);
			      _mm512_storeu_pd(&H_6[0],_mm512_add_pd(t6,t6));
			       const __m512d t7 = _mm512_loadu_pd(&H_7[0]);
			      _mm512_storeu_pd(&H_7[0],_mm512_add_pd(t7,t7));
			       const __m512d t8 = _mm512_loadu_pd(&H_8[0]);
			      _mm512_storeu_pd(&H_8[0],_mm512_add_pd(t8,t8));
			   }
		    }


		    void range_hessian_3d_zmm8r8(__m512d * __restrict __attribute__((aligned(64))) H,
		                                   const __m512d x_0,
						   const __m512d x_1,
						   const __m512d x_2,
						   const bool useHalfRange) {
                           __m512d invr,invr3;
                           const __m512d _1   = _mm512_set1_pd(1.0);
			   const __m512d xC2  = _mm512_mul_pd(x_0,x_0);
			   const __m512d yC2  = _mm512_mul_pd(x_1,x_1);
			   const __m512d zC2  = _mm512_mul_pd(x_2,x_2);
			   const __m512d r    = _mm512_sqrt_pd(
			                              _mm512_add_pd(xC2,_mm512_add_pd(yC2,zC2)));
			   const __m512d r3   = _mm512_mul_pd(r,_mm512_mul_pd(r,r));
			   invr               = _mm512_div_pd(_1,r);
			   invr3              = _mm512_div_pd(_1,r3);
			   H[0]                = _mm512_fmadd_pd(zmm8r8_negate(xC2),invr3,invr);
			   H[1]                = _mm512_mul_pd(zmm8r8_negate(x_0),
			                                      _mm512_mul_pd(x_1,invr3));
			   H[2]                = _mm512_mul_pd(zmm8r8_negate(x_0),
			                                      _mm512_mul_pd(x_2,invr3));
			   H[3]                = H[1];
			   H[4]                = _mm512_fmadd_pd(zmm8r8_negate(yC2),invr3,invr);
			   H[5]                = _mm512_mul_pd(zmm8r8_negate(x_1),
			                                      _mm512_mul_pd(x_2,invr3));
			   H[6]                = H[2];
			   H[7]                = H[5];
			   H[8]                = _mm512_fmadd_pd(zmm8r8_negate(zC2),invr3,invr);
			   if(useHalfRange) {
                               H[0] = _mm512_add_pd(H[0],H[0]);
			       H[1] = _mm512_add_pd(H[1],H[1]);
			       H[2] = _mm512_add_pd(H[2],H[2]);
			       H[3] = _mm512_add_pd(H[3],H[3]);
			       H[4] = _mm512_add_pd(H[4],H[4]);
			       H[5] = _mm512_add_pd(H[5],H[5]);
			       H[6] = _mm512_add_pd(H[6],H[6]);
			       H[7] = _mm512_add_pd(H[7],H[7]);
			       H[8] = _mm512_add_pd(H[8],H[8]);
			   }
		    }


		     __m512d range_hess_gen_1d_zmm8r8() {

		             return (_mm512_setzero_pd());
		      }


		    void range_hess_gen_2d_zmm8r8(  __m512d  * __restrict H_0,
		                                    __m512d  * __restrict H_1,
						    __m512d  * __restrict H_2,
						    __m512d  * __restrict H_3,
						    const __m512d x_0,
						    const __m512d x_1,
						    const __m512d rx_0,
						    const __m512d rx_1,
						    const __m512d tx_0,
						    const __m512d tx_1,
						    const bool useHalfRange) {

			  __m512d inv1,inv2,inv3,inv4;
                          const __m512d _1     = _mm512_set1_pd(1.0);
			  const __m512d _0_5   = _mm512_set1_pd(0.5);
			  const __m512d dRxx   = _mm512_sub_pd(x_0,rx_0);
			  const __m512d dRxx2  = _mm512_mul_pd(dRxx,dRxx);
			  const __m512d dRxy   = _mm512_sub_pd(x_1,rx_1);
			  const __m512d dRxy2  = _mm512_mul_pd(dRxy2,dRxy2);
			  const __m512d nrmdRx = _mm512_sqrt_pd(_mm512_add_pd(dRxx2,dRxy2));
			  inv1                 = _mm512_div_pd(_1,nrmdRx);
			  const __m512d nrmdRx3= _mm512_mul_pd(nrmdRx,_mm512_mul_pd(nrmdRx,nrmdRx));
			  inv3                 = _mm512_div_pd(_1,nrmdRx3);
			  const __m512d dTxx   = _mm512_sub_pd(x_0,tx_0);
			  const __m512d dTxx2  = _mm512_mul_pd(dTxx,dTxx);
			  const __m512d dTxy   = _mm512_sub_pd(x_1,tx_1);
			  const __m512d dTxy2  = _mm512_mul_pd(dTxy2,dTxy2);
			  const __m512d nrmdTx = _mm512_sqrt_pd(_mm512_add_pd(dTxx2,dTxy2));
			  inv2                 = _mm512_div_pd(_1,nrmdTx);
			  const __m512d nrmdTx3= _mm512_mul_pd(nrmdTx,_mm512_mul_pd(nrmdTx,nrmdTx));
			  inv4                 = _mm512_div_pd(_1,nrmdTx3);
			  *H_0                  = _mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxx2),inv3,inv1),
			                                       _mm512_fmadd_pd(dTxx2,inv4,inv2));
			  *H_1                  = _mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(dRxx),
			                                       _mm512_mul_pd(dRxy,inv3)),
							       _mm512_mul_pd(dTxx,
							       _mm512_mul_pd(dTxy,inv4)));
			  *H_2                  = *H_1;
			  *H_3                  = _mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxy2),inv3,inv1),
			                                       _mm512_fmadd_pd(dTxy2,inv4,inv2));
			  if(useHalfRange) {
                             *H_0 = _mm512_mul_pd(*H_0,_0_5);
			     *H_1 = _mm512_mul_pd(*H_1,_0_5);
			     *H_2 = _mm512_mul_pd(*H_2,_0_5);
			     *H_3 = _mm512_mul_pd(*H_3,_0_5);
			  }
			                                                     
		    }


		    void range_hess_gen_2d_zmm8r8_a(  double * __restrict __attribute__((aligned(64))) H_0,
		                                      double * __restrict __attribute__((aligned(64))) H_1,
						      double * __restrict __attribute__((aligned(64))) H_2,
						      double * __restrict __attribute__((aligned(64))) H_3,
						      const __m512d x_0,
						      const __m512d x_1,
						      const __m512d rx_0,
						      const __m512d rx_1,
						      const __m512d tx_0,
						      const __m512d tx_1,
						      const bool useHalfRange) {

			  __m512d inv1,inv2,inv3,inv4;
                          const __m512d _1     = _mm512_set1_pd(1.0);
			  const __m512d _0_5   = _mm512_set1_pd(0.5);
			  const __m512d dRxx   = _mm512_sub_pd(x_0,rx_0);
			  const __m512d dRxx2  = _mm512_mul_pd(dRxx,dRxx);
			  const __m512d dRxy   = _mm512_sub_pd(x_1,rx_1);
			  const __m512d dRxy2  = _mm512_mul_pd(dRxy2,dRxy2);
			  const __m512d nrmdRx = _mm512_sqrt_pd(_mm512_add_pd(dRxx2,dRxy2));
			  inv1                 = _mm512_div_pd(_1,nrmdRx);
			  const __m512d nrmdRx3= _mm512_mul_pd(nrmdRx,_mm512_mul_pd(nrmdRx,nrmdRx));
			  inv3                 = _mm512_div_pd(_1,nrmdRx3);
			  const __m512d dTxx   = _mm512_sub_pd(x_0,tx_0);
			  const __m512d dTxx2  = _mm512_mul_pd(dTxx,dTxx);
			  const __m512d dTxy   = _mm512_sub_pd(x_1,tx_1);
			  const __m512d dTxy2  = _mm512_mul_pd(dTxy2,dTxy2);
			  const __m512d nrmdTx = _mm512_sqrt_pd(_mm512_add_pd(dTxx2,dTxy2));
			  inv2                 = _mm512_div_pd(_1,nrmdTx);
			  const __m512d nrmdTx3= _mm512_mul_pd(nrmdTx,_mm512_mul_pd(nrmdTx,nrmdTx));
			  inv4                 = _mm512_div_pd(_1,nrmdTx3);
			  _mm512_store_pd(&H_0[0],_mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxx2),inv3,inv1),
			                                       _mm512_fmadd_pd(dTxx2,inv4,inv2)));
			  _mm512_store_pd(&H_1[0],_mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(dRxx),
			                                       _mm512_mul_pd(dRxy,inv3)),
							       _mm512_mul_pd(dTxx,
							       _mm512_mul_pd(dTxy,inv4))));
			  _mm512_store_pd(&H_2[0],_mm512_load_pd(&H_1[0]));
			  _mm512_store_pd(&H_3[0],_mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxy2),inv3,inv1),
			                                       _mm512_fmadd_pd(dTxy2,inv4,inv2)));
			  if(useHalfRange) {
                             _mm512_store_pd(&H_0[0],_mm512_mul_pd(_mm512_load_pd(&H_0[0],_0_5)));
			     _mm512_store_pd(&H_1[0],_mm512_mul_pd(_mm512_load_pd(&H_1[0],_0_5)));
			     _mm512_store_pd(&H_2[0],_mm512_mul_pd(_mm512_load_pd(&H_2[0],_0_5)));
			     _mm512_store_pd(&H_3[0],_mm512_mul_pd(_mm512_load_pd(&H_3[0],_0_5)));
			  }
			                                                     
		    }


		     void range_hess_gen_2d_zmm8r8_u( double * __restrict H_0,
		                                      double * __restrict H_1,
						      double * __restrict H_2,
						      double * __restrict H_3,
						      const __m512d x_0,
						      const __m512d x_1,
						      const __m512d rx_0,
						      const __m512d rx_1,
						      const __m512d tx_0,
						      const __m512d tx_1,
						      const bool useHalfRange) {

			  __m512d inv1,inv2,inv3,inv4;
                          const __m512d _1     = _mm512_set1_pd(1.0);
			  const __m512d _0_5   = _mm512_set1_pd(0.5);
			  const __m512d dRxx   = _mm512_sub_pd(x_0,rx_0);
			  const __m512d dRxx2  = _mm512_mul_pd(dRxx,dRxx);
			  const __m512d dRxy   = _mm512_sub_pd(x_1,rx_1);
			  const __m512d dRxy2  = _mm512_mul_pd(dRxy2,dRxy2);
			  const __m512d nrmdRx = _mm512_sqrt_pd(_mm512_add_pd(dRxx2,dRxy2));
			  inv1                 = _mm512_div_pd(_1,nrmdRx);
			  const __m512d nrmdRx3= _mm512_mul_pd(nrmdRx,_mm512_mul_pd(nrmdRx,nrmdRx));
			  inv3                 = _mm512_div_pd(_1,nrmdRx3);
			  const __m512d dTxx   = _mm512_sub_pd(x_0,tx_0);
			  const __m512d dTxx2  = _mm512_mul_pd(dTxx,dTxx);
			  const __m512d dTxy   = _mm512_sub_pd(x_1,tx_1);
			  const __m512d dTxy2  = _mm512_mul_pd(dTxy2,dTxy2);
			  const __m512d nrmdTx = _mm512_sqrt_pd(_mm512_add_pd(dTxx2,dTxy2));
			  inv2                 = _mm512_div_pd(_1,nrmdTx);
			  const __m512d nrmdTx3= _mm512_mul_pd(nrmdTx,_mm512_mul_pd(nrmdTx,nrmdTx));
			  inv4                 = _mm512_div_pd(_1,nrmdTx3);
			  _mm512_storeu_pd(&H_0[0],_mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxx2),inv3,inv1),
			                                       _mm512_fmadd_pd(dTxx2,inv4,inv2)));
			  _mm512_storeu_pd(&H_1[0],_mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(dRxx),
			                                       _mm512_mul_pd(dRxy,inv3)),
							       _mm512_mul_pd(dTxx,
							       _mm512_mul_pd(dTxy,inv4))));
			  _mm512_storeu_pd(&H_2[0],_mm512_load_pd(&H_1[0]));
			  _mm512_storeu_pd(&H_3[0],_mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxy2),inv3,inv1),
			                                       _mm512_fmadd_pd(dTxy2,inv4,inv2)));
			  if(useHalfRange) {
                             _mm512_storeu_pd(&H_0[0],_mm512_mul_pd(_mm512_loadu_pd(&H_0[0],_0_5)));
			     _mm512_storeu_pd(&H_1[0],_mm512_mul_pd(_mm512_loadu_pd(&H_1[0],_0_5)));
			     _mm512_storeu_pd(&H_2[0],_mm512_mul_pd(_mm512_loadu_pd(&H_2[0],_0_5)));
			     _mm512_storeu_pd(&H_3[0],_mm512_mul_pd(_mm512_loadu_pd(&H_3[0],_0_5)));
			  }
			                                                     
		    }


		     void range_hess_3d_zmm8r8( __m512d * __restrict H_0,
		                                __m512d * __restrict H_1,
						__m512d * __restrict H_2,
						__m512d * __restrict H_3,
						__m512d * __restrict H_4,
						__m512d * __restrict H_5,
						__m512d * __restrict H_6,
						__m512d * __restrict H_7,
						__m512d * __restrict H_8,
						const __m512d x_0,
						const __m512d x_1,
						const __m512d x_2,
						const __m512d rx_0,
						const __m512d rx_1,
						const __m512d rx_2,
						const __m512d tx_0,
						const __m512d tx_1,
						const __m512d tx_2,
						const bool useHalfRange) {


			  const __m512d _1      = _mm512_set1_pd(1.0);
			  const __m512d _0_5    = _mm512_set1_pd(0.5);
			  const __m512d dRxx    = _mm512_sub_pd(x_0,rx_0);
			  const __m512d dRxx2   = _mm512_mul_pd(dRxx,dRxx);
			  const __m512d dRxy    = _mm512_sub_pd(x_1,rx_1);
			  const __m512d dRxy2   = _mm512_mul_pd(dRxy,dRxy);
			  const __m512d dRxz    = _mm512_sub_pd(x_2,rx_2);
			  const __m512d dRxz2   = _mm512_mul_pd(dRxz,dRxz);
			  const __m512d nrmdRx  = _mm512_sqrt_pd(_mm512_mul_pd(dRxx2,
			                                                       _mm512_mul_pd(dRxy2,dRxz2)));
			  const __m512d inv0    = _mm512_div_pd(_1,nrmdRx);						      
			  const __m512d nrmdRx3 = _mm512_mul_pd(nrmdRx,_mm512_mul_pd(nrmdRx3,nrmdRx3));
			  const __m512d inv1    = _mm512_div_pd(_1,nrmdRx3);
			  const __m512d dTxx    = _mm512_sub_pd(x_0,tx_0);
			  const __m512d dTxx2   = _mm512_mul_pd(dTxx,dTxx);
			  const __m512d dTxy    = _mm512_sub_pd(x_1,tx_1);
			  const __m512d dTxy2   = _mm512_mul_pd(dTxy,dTxy);
			  const __m512d dTxz    = _mm512_sub_pd(x_2,tx_2);
			  const __m512d dTxz2   = _mm512_mul_pd(dTxz,dTxz);
			  const __m512d nrmdTx  = _mm512_sqrt_pd(_mm512_mul_pd(dTxx2,
			                                                       _mm512_mul_pd(dTxy2,dTxz2)));
			  const __m512d inv3    = _mm512_div_pd(_1,nrmdTx);
			  const __m512d nrmdTx3 = _mm512_mul_pd(nrmdTx,_mm512_mul_pd(nrmdTx3,nrmdTx3));
			  const __m512d inv2    = _mm512_div_pd(_1,nrmdTx3);
			  *H_0                   = _mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxx2),inv1,inv0),
			                                        _mm512_fmadd_pd(dTxx2,inv2,inv3));
			  *H_1                   = _mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(dRxx),
			                                        _mm512_mul_pd(dRxy,inv1)),
								_mm512_mul_pd(dTxx,
								_mm512_mul_pd(dTxy,inv2)));
			  *H_2                   = _mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(dRxx),
			                                        _mm512_mul_pd(dRxz,inv1)),
								_mm512_mul_pd(dTxx,
								_mm512_mul_pd(dTxz,inv2)));
			  *H_3                   = *H_1;
			  *H_4                   = _mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxy2),inv1,inv0),
			                                        _mm512_fmadd_pd(dTxy2,inv2,inv3));
			  *H_5                   = _mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(dRxy),
			                                        _mm512_mul_pd(dRxz,inv1)),
								_mm512_mul_pd(dTxy,
								_mm512_mul_pd(dTxz,inv2)));
			  *H_6                   = *H_2;
			  *H_7                   = *H_5;
			  *H_8                   = _mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxz2),inv1,inv0),
			                                        _mm512_fmadd_pd(dTxz2,inv2,inv3));
			  if(useHalfRange) {
                             *H_0 = _mm512_mul_pd(*H_0,_0_5);
			     *H_1 = _mm512_mul_pd(*H_1,_0_5);
			     *H_2 = _mm512_mul_pd(*H_2,_0_5);
			     *H_3 = _mm512_mul_pd(*H_3,_0_5);
			     *H_4 = _mm512_mul_pd(*H_4,_0_5);
			     *H_5 = _mm512_mul_pd(*H_5,_0_5);
			     *H_6 = _mm512_mul_pd(*H_6,_0_5);
			     *H_7 = _mm512_mul_pd(*H_7,_0_5);
			     *H_8 = _mm512_mul_pd(*H_8,_0_5);
			  }
		    }


		     void range_hess_3d_zmm8r8_a(  double * __restrict __attribute__((aligned(64))) H_0,
		                                   double * __restrict __attribute__((aligned(64))) H_1,
						   double * __restrict __attribute__((aligned(64))) H_2,
						   double * __restrict __attribute__((aligned(64))) H_3,
						   double * __restrict __attribute__((aligned(64))) H_4,
						   double * __restrict __attribute__((aligned(64))) H_5,
						   double * __restrict __attribute__((aligned(64))) H_6,
						   double * __restrict __attribute__((aligned(64))) H_7,
						   double * __restrict __attribute__((aligned(64))) H_8,
						   const __m512d x_0,
						   const __m512d x_1,
						   const __m512d x_2,
						   const __m512d rx_0,
						   const __m512d rx_1,
						   const __m512d rx_2,
						   const __m512d tx_0,
						   const __m512d tx_1,
						   const __m512d tx_2,
						   const bool useHalfRange) {


			  const __m512d _1      = _mm512_set1_pd(1.0);
			  const __m512d _0_5    = _mm512_set1_pd(0.5);
			  const __m512d dRxx    = _mm512_sub_pd(x_0,rx_0);
			  const __m512d dRxx2   = _mm512_mul_pd(dRxx,dRxx);
			  const __m512d dRxy    = _mm512_sub_pd(x_1,rx_1);
			  const __m512d dRxy2   = _mm512_mul_pd(dRxy,dRxy);
			  const __m512d dRxz    = _mm512_sub_pd(x_2,rx_2);
			  const __m512d dRxz2   = _mm512_mul_pd(dRxz,dRxz);
			  const __m512d nrmdRx  = _mm512_sqrt_pd(_mm512_mul_pd(dRxx2,
			                                                       _mm512_mul_pd(dRxy2,dRxz2)));
			  const __m512d inv0    = _mm512_div_pd(_1,nrmdRx);						      
			  const __m512d nrmdRx3 = _mm512_mul_pd(nrmdRx,_mm512_mul_pd(nrmdRx3,nrmdRx3));
			  const __m512d inv1    = _mm512_div_pd(_1,nrmdRx3);
			  const __m512d dTxx    = _mm512_sub_pd(x_0,tx_0);
			  const __m512d dTxx2   = _mm512_mul_pd(dTxx,dTxx);
			  const __m512d dTxy    = _mm512_sub_pd(x_1,tx_1);
			  const __m512d dTxy2   = _mm512_mul_pd(dTxy,dTxy);
			  const __m512d dTxz    = _mm512_sub_pd(x_2,tx_2);
			  const __m512d dTxz2   = _mm512_mul_pd(dTxz,dTxz);
			  const __m512d nrmdTx  = _mm512_sqrt_pd(_mm512_mul_pd(dTxx2,
			                                                       _mm512_mul_pd(dTxy2,dTxz2)));
			  const __m512d inv3    = _mm512_div_pd(_1,nrmdTx);
			  const __m512d nrmdTx3 = _mm512_mul_pd(nrmdTx,_mm512_mul_pd(nrmdTx3,nrmdTx3));
			  const __m512d inv2    = _mm512_div_pd(_1,nrmdTx3);
			  _mm512_store_pd(&H_0[0],_mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxx2),inv1,inv0),
			                                        _mm512_fmadd_pd(dTxx2,inv2,inv3)));
			  _mm512_store_pd(&H_1[0],_mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(dRxx),
			                                        _mm512_mul_pd(dRxy,inv1)),
								_mm512_mul_pd(dTxx,
								_mm512_mul_pd(dTxy,inv2))));
			  _mm512_store_pd(&H_2[0],_mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(dRxx),
			                                        _mm512_mul_pd(dRxz,inv1)),
								_mm512_mul_pd(dTxx,
								_mm512_mul_pd(dTxz,inv2))));
			  _mm512_store_pd(&H_3[0],_mm512_load_pd(&H_1[0]));
			  _mm512_store_pd(&H_4[0],_mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxy2),inv1,inv0),
			                                        _mm512_fmadd_pd(dTxy2,inv2,inv3)));
			  _mm512_store_pd(&H_5[0],_mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(dRxy),
			                                        _mm512_mul_pd(dRxz,inv1)),
								_mm512_mul_pd(dTxy,
								_mm512_mul_pd(dTxz,inv2))));
			  _mm512_store_pd(&H_6[0],_mm512_load_pd(&H_2[0]));
			  _mm512_store_pd(&H_7[0],_mm512_load_pd(&H_5[0]));
			  _mm512_store_pd(&H_8[0],_mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxz2),inv1,inv0),
			                                        _mm512_fmadd_pd(dTxz2,inv2,inv3)));
			  if(useHalfRange) {
			     _mm512_store_pd(&H_0[0],_mm512_mul_pd(_mm512_load_pd(&H_0[0],_0_5)));
			     _mm512_store_pd(&H_1[0],_mm512_mul_pd(_mm512_load_pd(&H_1[0],_0_5)));
			     _mm512_store_pd(&H_2[0],_mm512_mul_pd(_mm512_load_pd(&H_2[0],_0_5)));
			     _mm512_store_pd(&H_3[0],_mm512_mul_pd(_mm512_load_pd(&H_3[0],_0_5)));
			     _mm512_store_pd(&H_4[0],_mm512_mul_pd(_mm512_load_pd(&H_4[0],_0_5)));
			     _mm512_store_pd(&H_5[0],_mm512_mul_pd(_mm512_load_pd(&H_5[0],_0_5)));
			     _mm512_store_pd(&H_6[0],_mm512_mul_pd(_mm512_load_pd(&H_6[0],_0_5)));
			     _mm512_store_pd(&H_7[0],_mm512_mul_pd(_mm512_load_pd(&H_7[0],_0_5)));
			     _mm512_store_pd(&H_8[0],_mm512_mul_pd(_mm512_load_pd(&H_8[0],_0_5)));
			  }
		    }


		      void range_hess_3d_zmm8r8_u(  double * __restrict  H_0,
		                                   double * __restrict   H_1,
						   double * __restrict   H_2,
						   double * __restrict   H_3,
						   double * __restrict   H_4,
						   double * __restrict   H_5,
						   double * __restrict   H_6,
						   double * __restrict   H_7,
						   double * __restrict   H_8,
						   const __m512d x_0,
						   const __m512d x_1,
						   const __m512d x_2,
						   const __m512d rx_0,
						   const __m512d rx_1,
						   const __m512d rx_2,
						   const __m512d tx_0,
						   const __m512d tx_1,
						   const __m512d tx_2,
						   const bool useHalfRange) {


			  const __m512d _1      = _mm512_set1_pd(1.0);
			  const __m512d _0_5    = _mm512_set1_pd(0.5);
			  const __m512d dRxx    = _mm512_sub_pd(x_0,rx_0);
			  const __m512d dRxx2   = _mm512_mul_pd(dRxx,dRxx);
			  const __m512d dRxy    = _mm512_sub_pd(x_1,rx_1);
			  const __m512d dRxy2   = _mm512_mul_pd(dRxy,dRxy);
			  const __m512d dRxz    = _mm512_sub_pd(x_2,rx_2);
			  const __m512d dRxz2   = _mm512_mul_pd(dRxz,dRxz);
			  const __m512d nrmdRx  = _mm512_sqrt_pd(_mm512_mul_pd(dRxx2,
			                                                       _mm512_mul_pd(dRxy2,dRxz2)));
			  const __m512d inv0    = _mm512_div_pd(_1,nrmdRx);						      
			  const __m512d nrmdRx3 = _mm512_mul_pd(nrmdRx,_mm512_mul_pd(nrmdRx3,nrmdRx3));
			  const __m512d inv1    = _mm512_div_pd(_1,nrmdRx3);
			  const __m512d dTxx    = _mm512_sub_pd(x_0,tx_0);
			  const __m512d dTxx2   = _mm512_mul_pd(dTxx,dTxx);
			  const __m512d dTxy    = _mm512_sub_pd(x_1,tx_1);
			  const __m512d dTxy2   = _mm512_mul_pd(dTxy,dTxy);
			  const __m512d dTxz    = _mm512_sub_pd(x_2,tx_2);
			  const __m512d dTxz2   = _mm512_mul_pd(dTxz,dTxz);
			  const __m512d nrmdTx  = _mm512_sqrt_pd(_mm512_mul_pd(dTxx2,
			                                                       _mm512_mul_pd(dTxy2,dTxz2)));
			  const __m512d inv3    = _mm512_div_pd(_1,nrmdTx);
			  const __m512d nrmdTx3 = _mm512_mul_pd(nrmdTx,_mm512_mul_pd(nrmdTx3,nrmdTx3));
			  const __m512d inv2    = _mm512_div_pd(_1,nrmdTx3);
			  _mm512_storeu_pd(&H_0[0],_mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxx2),inv1,inv0),
			                                        _mm512_fmadd_pd(dTxx2,inv2,inv3)));
			  _mm512_storeu_pd(&H_1[0],_mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(dRxx),
			                                        _mm512_mul_pd(dRxy,inv1)),
								_mm512_mul_pd(dTxx,
								_mm512_mul_pd(dTxy,inv2))));
			  _mm512_storeu_pd(&H_2[0],_mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(dRxx),
			                                        _mm512_mul_pd(dRxz,inv1)),
								_mm512_mul_pd(dTxx,
								_mm512_mul_pd(dTxz,inv2))));
			  _mm512_storeu_pd(&H_3[0],_mm512_load_pd(&H_1[0]));
			  _mm512_storeu_pd(&H_4[0],_mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxy2),inv1,inv0),
			                                        _mm512_fmadd_pd(dTxy2,inv2,inv3)));
			  _mm512_storeu_pd(&H_5[0],_mm512_sub_pd(_mm512_mul_pd(zmm8r8_negate(dRxy),
			                                        _mm512_mul_pd(dRxz,inv1)),
								_mm512_mul_pd(dTxy,
								_mm512_mul_pd(dTxz,inv2))));
			  _mm512_storeu_pd(&H_6[0],_mm512_loadu_pd(&H_2[0]));
			  _mm512_storeu_pd(&H_7[0],_mm512_loadu_pd(&H_5[0]));
			  _mm512_storeu_pd(&H_8[0],_mm512_sub_pd(_mm512_fmadd_pd(zmm8r8_negate(dRxz2),inv1,inv0),
			                                        _mm512_fmadd_pd(dTxz2,inv2,inv3)));
			  if(useHalfRange) {
			     _mm512_storeu_pd(&H_0[0],_mm512_mul_pd(_mm512_loadu_pd(&H_0[0],_0_5)));
			     _mm512_storeu_pd(&H_1[0],_mm512_mul_pd(_mm512_loadu_pd(&H_1[0],_0_5)));
			     _mm512_storeu_pd(&H_2[0],_mm512_mul_pd(_mm512_loadu_pd(&H_2[0],_0_5)));
			     _mm512_storeu_pd(&H_3[0],_mm512_mul_pd(_mm512_loadu_pd(&H_3[0],_0_5)));
			     _mm512_storeu_pd(&H_4[0],_mm512_mul_pd(_mm512_loadu_pd(&H_4[0],_0_5)));
			     _mm512_storeu_pd(&H_5[0],_mm512_mul_pd(_mm512_loadu_pd(&H_5[0],_0_5)));
			     _mm512_storeu_pd(&H_6[0],_mm512_mul_pd(_mm512_loadu_pd(&H_6[0],_0_5)));
			     _mm512_storeu_pd(&H_7[0],_mm512_mul_pd(_mm512_loadu_pd(&H_7[0],_0_5)));
			     _mm512_storeu_pd(&H_8[0],_mm512_mul_pd(_mm512_loadu_pd(&H_8[0],_0_5)));
			  }
		    }


		     void cart_to_ruv_zmm8r8( __m512d  *__restrict r,
		                              __m512d * __restrict u,
					      __m512d * __restrict v,
					      __m512d * __restrict w,
					      const __m512d C_x,
					      const __m512d C_y,
					      const __m512d C_z,
					      const __m512d T_x,
					      const __m512d T_y,
					      const __m512d T_z,
					      const __m512d R_x,
					      const __m512d R_y,
					      const __m512d R_z,
					      const __m512d * __restrict __attribute__((aligned(64)))  M, //flattened 3x3 matrix
					      const bool useHalfRange) {

                          __m512d CL0,CL1,CL2,TxL0,TxL1,TxL2;
			  __m512d r1,r2;
			  __m512d diff0,diff1,diff2,diff3,diff4,diff5;
			  const __m512d M0 = M[0];
			  //Compute the target location in the receiver's coordinate system.
			  diff0 = _mm512_sub_pd(C_x,R_x);
			  const __m512d M1 = M[1];
			  diff1 = _mm512_sub_pd(C_y,R_y);
			  const __m512d M2 = M[2];
			  diff2 = _mm512_sub_pd(C_z,R_z);
			  const __m512d M3 = M[3];
			  diff3 = _mm512_sub_pd(T_x,R_x);
			  const __m512d M4 = M[4];
			  diff4 = _mm512_sub_pd(T_y,R_y);
			  const __m512d M5 = M[5];
			  diff5 = _mm512_sub_pd(T_z,R_z);
			  const __m512d M6 = M[6];
			   //Compute the transmitter location in the receiver's local coordinate
                          //system.
			  TxL0  = _mm512_fmadd_pd(M0,diff3,_mm512_fmadd_pd(M3,diff4,_mm512_mul_pd(M6,diff5)));
			  CL0   = _mm512_fmadd_pd(M0,diff0,_mm512_fmadd_pd(M3,diff1,_mm512_mul_pd(M6,diff2)));
			  const __m512d M7 = M[7];
			  CL1   = _mm512_fmadd_pd(M1,diff0,_mm512_fmadd_pd(M4,diff1,_mm512_mul_pd(M7,diff2)));
			  TxL1  = _mm512_fmadd_pd(M1,diff3,_mm512_fmadd_pd(M4,diff4,_mm512_mul_pd(M7,diff5)));
			  const __m512d M8 = M[8];
			  CL2   = _mm512_fmadd_pd(M2,diff0,_mm512_fmadd_pd(M5,diff1,_mm512_mul_pd(M8,diff2)));
			  TxL2  = _mm512_fmadd_pd(M2,diff3,_mm512_fmadd_pd(M5,diff4,_mm512_mul_pd(M8,diff5)));
			  ///Receiver to target.
			  __m512d sarg = _mm512_fmadd_pd(CL0,CL0,_mm512_fmadd_pd(CL1,CL1,_mm512_mul_pd(CL2,CL2)));
			  r1    = _mm512_sqrt_pd(sarg);
			  diff0 = _mm512_sub_pd(CL0,TxL0);
			  diff1 = _mm512_sub_pd(CL1,TxL1);
			  *u     = _mm512_div_pd(CL0,r1);
			  diff2 = _mm512_sub_pd(CL2,TxL2);
			  *w     = _mm512_div_pd(CL2,r1);
			  //Target to transmitter
			  sarg  = _mm512_fmadd_pd(diff0,diff0,_mm512_fmadd_pd(diff1,diff1,_mm512_mul_pd(diff2,diff2)));
			  r2    = _mm512_sqrt_pd(sarg);
			  *r     = _mm512_add_pd(r1,r2);
			  *v     = _mm512_div_pd(CL1,r1);
			  if(useHalfRange) {
                             const __m512d _0_5 = _mm512_set1_pd(0.5);
			     *r  = _mm512_mul_pd(*r,_0_5);
			  }
		     }


		     void cart_to_ruv_zmm8r8_a( double * __restrict  __attribute__((aligned(64))) r,
		                                double * __restrict  __attribute__((aligned(64))) u,
					        double * __restrict  __attribute__((aligned(64))) v,
					        double * __restrict  __attribute__((aligned(64))) w,
					      const __m512d C_x,
					      const __m512d C_y,
					      const __m512d C_z,
					      const __m512d T_x,
					      const __m512d T_y,
					      const __m512d T_z,
					      const __m512d R_x,
					      const __m512d R_y,
					      const __m512d R_z,
					      const __m512d * __restrict __ATTR_ALIGN__(64) M, //flattened 3x3 matrix
					      const bool useHalfRange) {

                          __m512d CL0,CL1,CL2,TxL0,TxL1,TxL2;
			  __m512d r1,r2;
			  __m512d diff0,diff1,diff2,diff3,diff4,diff5;
			  const __m512d M0 = M[0];
			  //Compute the target location in the receiver's coordinate system.
			  diff0 = _mm512_sub_pd(C_x,R_x);
			  const __m512d M1 = M[1];
			  diff1 = _mm512_sub_pd(C_y,R_y);
			  const __m512d M2 = M[2];
			  diff2 = _mm512_sub_pd(C_z,R_z);
			  const __m512d M3 = M[3];
			  diff3 = _mm512_sub_pd(T_x,R_x);
			  const __m512d M4 = M[4];
			  diff4 = _mm512_sub_pd(T_y,R_y);
			  const __m512d M5 = M[5];
			  diff5 = _mm512_sub_pd(T_z,R_z);
			  const __m512d M6 = M[6];
			   //Compute the transmitter location in the receiver's local coordinate
                          //system.
			  TxL0  = _mm512_fmadd_pd(M0,diff3,_mm512_fmadd_pd(M3,diff4,_mm512_mul_pd(M6,diff5)));
			  CL0   = _mm512_fmadd_pd(M0,diff0,_mm512_fmadd_pd(M3,diff1,_mm512_mul_pd(M6,diff2)));
			  const __m512d M7 = M[7];
			  CL1   = _mm512_fmadd_pd(M1,diff0,_mm512_fmadd_pd(M4,diff1,_mm512_mul_pd(M7,diff2)));
			  TxL1  = _mm512_fmadd_pd(M1,diff3,_mm512_fmadd_pd(M4,diff4,_mm512_mul_pd(M7,diff5)));
			  const __m512d M8 = M[8];
			  CL2   = _mm512_fmadd_pd(M2,diff0,_mm512_fmadd_pd(M5,diff1,_mm512_mul_pd(M8,diff2)));
			  TxL2  = _mm512_fmadd_pd(M2,diff3,_mm512_fmadd_pd(M5,diff4,_mm512_mul_pd(M8,diff5)));
			  ///Receiver to target.
			  __m512d sarg = _mm512_fmadd_pd(CL0,CL0,_mm512_fmadd_pd(CL1,CL1,_mm512_mul_pd(CL2,CL2)));
			  r1    = _mm512_sqrt_pd(sarg);
			  diff0 = _mm512_sub_pd(CL0,TxL0);
			  diff1 = _mm512_sub_pd(CL1,TxL1);
			  _mm512_store_pd(&u[0],_mm512_div_pd(CL0,r1));
			  diff2 = _mm512_sub_pd(CL2,TxL2);
			  _mm512_store_pd(&w[0],_mm512_div_pd(CL2,r1));
			  //Target to transmitter
			  sarg  = _mm512_fmadd_pd(diff0,diff0,_mm512_fmadd_pd(diff1,diff1,_mm512_mul_pd(diff2,diff2)));
			  r2    = _mm512_sqrt_pd(sarg);
			  _mm512_store_pd(&r[0],_mm512_add_pd(r1,r2));
			  _mm512_store_pd(&v[0],_mm512_div_pd(CL1,r1));
			  if(useHalfRange) {
                             const __m512d _0_5 = _mm512_set1_pd(0.5);
			     _mm512_store_pd(&r[0],_mm512_mul_pd(_mm512_load_pd(&r[0],_0_5)));
			  }
		     }


		       void cart_to_ruv_zmm8r8_u(double * __restrict r,
		                                double * __restrict u,
					        double * __restrict v,
					        double * __restrict w,
					      const __m512d C_x,
					      const __m512d C_y,
					      const __m512d C_z,
					      const __m512d T_x,
					      const __m512d T_y,
					      const __m512d T_z,
					      const __m512d R_x,
					      const __m512d R_y,
					      const __m512d R_z,
					      const __m512d * __restrict __ATTR_ALIGN__(64) M, //flattened 3x3 matrix
					      const bool useHalfRange) {

                          __m512d CL0,CL1,CL2,TxL0,TxL1,TxL2;
			  __m512d r1,r2;
			  __m512d diff0,diff1,diff2,diff3,diff4,diff5;
			  const __m512d M0 = M[0];
			  //Compute the target location in the receiver's coordinate system.
			  diff0 = _mm512_sub_pd(C_x,R_x);
			  const __m512d M1 = M[1];
			  diff1 = _mm512_sub_pd(C_y,R_y);
			  const __m512d M2 = M[2];
			  diff2 = _mm512_sub_pd(C_z,R_z);
			  const __m512d M3 = M[3];
			  diff3 = _mm512_sub_pd(T_x,R_x);
			  const __m512d M4 = M[4];
			  diff4 = _mm512_sub_pd(T_y,R_y);
			  const __m512d M5 = M[5];
			  diff5 = _mm512_sub_pd(T_z,R_z);
			  const __m512d M6 = M[6];
			   //Compute the transmitter location in the receiver's local coordinate
                          //system.
			  TxL0  = _mm512_fmadd_pd(M0,diff3,_mm512_fmadd_pd(M3,diff4,_mm512_mul_pd(M6,diff5)));
			  CL0   = _mm512_fmadd_pd(M0,diff0,_mm512_fmadd_pd(M3,diff1,_mm512_mul_pd(M6,diff2)));
			  const __m512d M7 = M[7];
			  CL1   = _mm512_fmadd_pd(M1,diff0,_mm512_fmadd_pd(M4,diff1,_mm512_mul_pd(M7,diff2)));
			  TxL1  = _mm512_fmadd_pd(M1,diff3,_mm512_fmadd_pd(M4,diff4,_mm512_mul_pd(M7,diff5)));
			  const __m512d M8 = M[8];
			  CL2   = _mm512_fmadd_pd(M2,diff0,_mm512_fmadd_pd(M5,diff1,_mm512_mul_pd(M8,diff2)));
			  TxL2  = _mm512_fmadd_pd(M2,diff3,_mm512_fmadd_pd(M5,diff4,_mm512_mul_pd(M8,diff5)));
			  ///Receiver to target.
			  __m512d sarg = _mm512_fmadd_pd(CL0,CL0,_mm512_fmadd_pd(CL1,CL1,_mm512_mul_pd(CL2,CL2)));
			  r1    = _mm512_sqrt_pd(sarg);
			  diff0 = _mm512_sub_pd(CL0,TxL0);
			  diff1 = _mm512_sub_pd(CL1,TxL1);
			  _mm512_storeu_pd(&u[0],_mm512_div_pd(CL0,r1));
			  diff2 = _mm512_sub_pd(CL2,TxL2);
			  _mm512_storeu_pd(&w[0],_mm512_div_pd(CL2,r1));
			  //Target to transmitter
			  sarg  = _mm512_fmadd_pd(diff0,diff0,_mm512_fmadd_pd(diff1,diff1,_mm512_mul_pd(diff2,diff2)));
			  r2    = _mm512_sqrt_pd(sarg);
			  _mm512_storeu_pd(&r[0],_mm512_add_pd(r1,r2));
			  _mm512_storeu_pd(&v[0],_mm512_div_pd(CL1,r1));
			  if(useHalfRange) {
                             const __m512d _0_5 = _mm512_set1_pd(0.5);
			     _mm512_storeu_pd(&r[0],_mm512_mul_pd(_mm512_loadu_pd(&r[0],_0_5)));
			  }
		     }


		      void cart_to_sphere_zmm8r8(__m512d * __restrict range,
		                                 __m512d * __restrict az,
						 __m512d * __restrict elev,
						 const __m512d C_x,
						 const __m512d C_y,
						 const __m512d C_z,
						 const __m512d T_x,
						 const __m512d T_y,
						 const __m512d T_z,
						 const __m512d R_x,
						 const __m512d R_y,
						 const __m512d R_z,
						 const __m512d * __restrict __ATTR_ALIGN__(64) M,
						 const int sysType,
						 const bool useHalfRange) {

			  const __m512d _0   = _mm512_setzero_pd();
			  const __m512d _0_5 = _mm512_set1_pd(0.5);
                          __m512d CL0,CL1,CL2,TxL0,TxL1,TxL2;
			  __m512d diff0,diff1,diff2,diff3,diff4,diff5;
			  __m512d r1,r2;
			  const __m512d M0 = M[0];
			  //Compute the target location in the receiver's coordinate system.
			  diff0 = _mm512_sub_pd(C_x,R_x);
			  const __m512d M1 = M[1];
			  diff1 = _mm512_sub_pd(C_y,R_y);
			  const __m512d M2 = M[2];
			  diff2 = _mm512_sub_pd(C_z,R_z);
			  const __m512d M3 = M[3];
			  diff3 = _mm512_sub_pd(T_x,R_x);
			  const __m512d M4 = M[4];
			  diff4 = _mm512_sub_pd(T_y,R_y);
			  const __m512d M5 = M[5];
			  diff5 = _mm512_sub_pd(T_z,R_z);
			  const __m512d M6 = M[6];
			   //Compute the transmitter location in the receiver's local coordinate
                          //system.
			  TxL0  = _mm512_fmadd_pd(M0,diff3,_mm512_fmadd_pd(M3,diff4,_mm512_mul_pd(M6,diff5)));
			  CL0   = _mm512_fmadd_pd(M0,diff0,_mm512_fmadd_pd(M3,diff1,_mm512_mul_pd(M6,diff2)));
			  const __m512d M7 = M[7];
			  CL1   = _mm512_fmadd_pd(M1,diff0,_mm512_fmadd_pd(M4,diff1,_mm512_mul_pd(M7,diff2)));
			  TxL1  = _mm512_fmadd_pd(M1,diff3,_mm512_fmadd_pd(M4,diff4,_mm512_mul_pd(M7,diff5)));
			  const __m512d M8 = M[8];
			  CL2   = _mm512_fmadd_pd(M2,diff0,_mm512_fmadd_pd(M5,diff1,_mm512_mul_pd(M8,diff2)));
			  TxL2  = _mm512_fmadd_pd(M2,diff3,_mm512_fmadd_pd(M5,diff4,_mm512_mul_pd(M8,diff5)));
			  ///Receiver to target.
			  __m512d sarg = _mm512_fmadd_pd(CL0,CL0,_mm512_fmadd_pd(CL1,CL1,_mm512_mul_pd(CL2,CL2)));
			  r1    = _mm512_sqrt_pd(sarg);
			  diff0 = _mm512_sub_pd(CL0,TxL0);
			  diff1 = _mm512_sub_pd(CL1,TxL1);
			  diff2 = _mm512_sub_pd(CL2,TxL2);
			  //Target to transmitter
			  sarg  = _mm512_fmadd_pd(diff0,diff0,_mm512_fmadd_pd(diff1,diff1,_mm512_mul_pd(diff2,diff2)));
			  r2    = _mm512_sqrt_pd(sarg);
			  *range = _mm512_add_pd(r1,r2);
			  if(sysType==0||sysType==2||sysType==3) {
                             __mmask8 m1,m2;
			     m1 = _mm512_cmp_pd_mask(CL1,_0,_CMP_EQ_OQ);
			     m2 = _mm512_cmp_pd_mask(CL0,_0,_CMP_EQ_OQ);
			     if(m1 && m2) {
                                *az = _0;
			     }
			     else {
                                *az = _mm512_atan2_pd(CL1,CL0);
			     }
			     elev = _mm512_atan2_pd(CL2,_mm512_hypot_pd(CL0,CL1));
			     if(sysType==2) {
                                const __m512d pi2 = _mm512_set1_pd(1.5707963267948966192313);
				*elev              = _mm512_sub_pd(pi2,*elev);
			     }
			     else if(sysType==3) {
                                const __m512d pi2 = _mm512_set1_pd(1.5707963267948966192313);
				*az                = _mm512_sub_pd(pi2,*az);
			     }
			  }
			  else {
                              __mmask8 m1,m2;
			      m1 = _mm512_cmp_pd_mask(CL2,_0,_CMP_EQ_OQ);
			      m2 = _mm512_cmp_pd_mask(CL0,_0,_CMP_EQ_OQ);
			      if(m1 && m2) {
                                 *az = _0;
			      }
			      else {
                                 *az = _mm512_atan2_pd(CL0,CL2);
			      }
			      *elev = _mm512_atan2_pd(CL1,_mm512_hypot_pd(CL2,CL0));
			  }

			  if(useHalfRange) {
                             *range = _mm512_mul_pd(*range,_0_5);
			  }
		     }


		       void cart_to_sphere_zmm8r8_a(double * __restrict __attribute__((aligned(64))) range,
		                                    double * __restrict __attribute__((aligned(64))) az,
						    double * __restrict __attribute__((aligned(64))) elev,
						   const __m512d C_x,
						   const __m512d C_y,
						   const __m512d C_z,
						   const __m512d T_x,
						   const __m512d T_y,
						   const __m512d T_z,
						   const __m512d R_x,
						   const __m512d R_y,
						   const __m512d R_z,
						   const __m512d * __restrict __ATTR_ALIGN__(64) M,
						   const int sysType,
						   const bool useHalfRange) {

			  const __m512d _0   = _mm512_setzero_pd();
			  const __m512d _0_5 = _mm512_set1_pd(0.5);
                          __m512d CL0,CL1,CL2,TxL0,TxL1,TxL2;
			  __m512d diff0,diff1,diff2,diff3,diff4,diff5;
			  __m512d r1,r2;
			  const __m512d M0 = M[0];
			  //Compute the target location in the receiver's coordinate system.
			  diff0 = _mm512_sub_pd(C_x,R_x);
			  const __m512d M1 = M[1];
			  diff1 = _mm512_sub_pd(C_y,R_y);
			  const __m512d M2 = M[2];
			  diff2 = _mm512_sub_pd(C_z,R_z);
			  const __m512d M3 = M[3];
			  diff3 = _mm512_sub_pd(T_x,R_x);
			  const __m512d M4 = M[4];
			  diff4 = _mm512_sub_pd(T_y,R_y);
			  const __m512d M5 = M[5];
			  diff5 = _mm512_sub_pd(T_z,R_z);
			  const __m512d M6 = M[6];
			   //Compute the transmitter location in the receiver's local coordinate
                          //system.
			  TxL0  = _mm512_fmadd_pd(M0,diff3,_mm512_fmadd_pd(M3,diff4,_mm512_mul_pd(M6,diff5)));
			  CL0   = _mm512_fmadd_pd(M0,diff0,_mm512_fmadd_pd(M3,diff1,_mm512_mul_pd(M6,diff2)));
			  const __m512d M7 = M[7];
			  CL1   = _mm512_fmadd_pd(M1,diff0,_mm512_fmadd_pd(M4,diff1,_mm512_mul_pd(M7,diff2)));
			  TxL1  = _mm512_fmadd_pd(M1,diff3,_mm512_fmadd_pd(M4,diff4,_mm512_mul_pd(M7,diff5)));
			  const __m512d M8 = M[8];
			  CL2   = _mm512_fmadd_pd(M2,diff0,_mm512_fmadd_pd(M5,diff1,_mm512_mul_pd(M8,diff2)));
			  TxL2  = _mm512_fmadd_pd(M2,diff3,_mm512_fmadd_pd(M5,diff4,_mm512_mul_pd(M8,diff5)));
			  ///Receiver to target.
			  __m512d sarg = _mm512_fmadd_pd(CL0,CL0,_mm512_fmadd_pd(CL1,CL1,_mm512_mul_pd(CL2,CL2)));
			  r1    = _mm512_sqrt_pd(sarg);
			  diff0 = _mm512_sub_pd(CL0,TxL0);
			  diff1 = _mm512_sub_pd(CL1,TxL1);
			  diff2 = _mm512_sub_pd(CL2,TxL2);
			  //Target to transmitter
			  sarg  = _mm512_fmadd_pd(diff0,diff0,_mm512_fmadd_pd(diff1,diff1,_mm512_mul_pd(diff2,diff2)));
			  r2    = _mm512_sqrt_pd(sarg);
			  _mm512_store_pd(&range[0],_mm512_add_pd(r1,r2));
			  if(sysType==0||sysType==2||sysType==3) {
                             __mmask8 m1,m2;
			     m1 = _mm512_cmp_pd_mask(CL1,_0,_CMP_EQ_OQ);
			     m2 = _mm512_cmp_pd_mask(CL0,_0,_CMP_EQ_OQ);
			     if(m1 && m2) {
                                _mm512_store_pd(&az[0],_0);
			     }
			     else {
                                _mm512_store_pd(&az[0],_mm512_atan2_pd(CL1,CL0));
			     }
			     _mm512_store_pd(&elev[0],_mm512_atan2_pd(CL2,_mm512_hypot_pd(CL0,CL1)));
			     if(sysType==2) {
                                const __m512d pi2 = _mm512_set1_pd(1.5707963267948966192313);
				_mm512_store_pd(&elev[0],_mm512_sub_pd(pi2,_mm512_load_pd(&elev[0])));
			     }
			     else if(sysType==3) {
                                const __m512d pi2 = _mm512_set1_pd(1.5707963267948966192313);
				_mm512_store_pd(&az[0],_mm512_sub_pd(pi2,_mm512_load_pd(&az[0])));
			     }
			  }
			  else {
                              __mmask8 m1,m2;
			      m1 = _mm512_cmp_pd_mask(CL2,_0,_CMP_EQ_OQ);
			      m2 = _mm512_cmp_pd_mask(CL0,_0,_CMP_EQ_OQ);
			      if(m1 && m2) {
                                 _mm512_store_pd(&az[0],_0);
			      }
			      else {
                                 _mm512_store_pd(&az[0],_mm512_atan2_pd(CL0,CL2));
			      }
			      _mm512_store_pd(&elev[0],_mm512_atan2_pd(CL1,_mm512_hypot_pd(CL2,CL0)));
			  }

			  if(useHalfRange) {
                             _mm512_store_pd(&range[0],_mm512_mul_pd(_mm512_load_pd(&range[0],_0_5)));
			  }
		     }


		      void cart_to_sphere_zmm8r8_u(double * __restrict  range,
		                                   double * __restrict  az,
						   double * __restrict  elev,
						   const __m512d C_x,
						   const __m512d C_y,
						   const __m512d C_z,
						   const __m512d T_x,
						   const __m512d T_y,
						   const __m512d T_z,
						   const __m512d R_x,
						   const __m512d R_y,
						   const __m512d R_z,
						   const __m512d * __restrict __ATTR_ALIGN__(64) M,
						   const int sysType,
						   const bool useHalfRange) {

			  const __m512d _0   = _mm512_setzero_pd();
			  const __m512d _0_5 = _mm512_set1_pd(0.5);
                          __m512d CL0,CL1,CL2,TxL0,TxL1,TxL2;
			  __m512d diff0,diff1,diff2,diff3,diff4,diff5;
			  __m512d r1,r2;
			  const __m512d M0 = M[0];
			  //Compute the target location in the receiver's coordinate system.
			  diff0 = _mm512_sub_pd(C_x,R_x);
			  const __m512d M1 = M[1];
			  diff1 = _mm512_sub_pd(C_y,R_y);
			  const __m512d M2 = M[2];
			  diff2 = _mm512_sub_pd(C_z,R_z);
			  const __m512d M3 = M[3];
			  diff3 = _mm512_sub_pd(T_x,R_x);
			  const __m512d M4 = M[4];
			  diff4 = _mm512_sub_pd(T_y,R_y);
			  const __m512d M5 = M[5];
			  diff5 = _mm512_sub_pd(T_z,R_z);
			  const __m512d M6 = M[6];
			   //Compute the transmitter location in the receiver's local coordinate
                          //system.
			  TxL0  = _mm512_fmadd_pd(M0,diff3,_mm512_fmadd_pd(M3,diff4,_mm512_mul_pd(M6,diff5)));
			  CL0   = _mm512_fmadd_pd(M0,diff0,_mm512_fmadd_pd(M3,diff1,_mm512_mul_pd(M6,diff2)));
			  const __m512d M7 = M[7];
			  CL1   = _mm512_fmadd_pd(M1,diff0,_mm512_fmadd_pd(M4,diff1,_mm512_mul_pd(M7,diff2)));
			  TxL1  = _mm512_fmadd_pd(M1,diff3,_mm512_fmadd_pd(M4,diff4,_mm512_mul_pd(M7,diff5)));
			  const __m512d M8 = M[8];
			  CL2   = _mm512_fmadd_pd(M2,diff0,_mm512_fmadd_pd(M5,diff1,_mm512_mul_pd(M8,diff2)));
			  TxL2  = _mm512_fmadd_pd(M2,diff3,_mm512_fmadd_pd(M5,diff4,_mm512_mul_pd(M8,diff5)));
			  ///Receiver to target.
			  __m512d sarg = _mm512_fmadd_pd(CL0,CL0,_mm512_fmadd_pd(CL1,CL1,_mm512_mul_pd(CL2,CL2)));
			  r1    = _mm512_sqrt_pd(sarg);
			  diff0 = _mm512_sub_pd(CL0,TxL0);
			  diff1 = _mm512_sub_pd(CL1,TxL1);
			  diff2 = _mm512_sub_pd(CL2,TxL2);
			  //Target to transmitter
			  sarg  = _mm512_fmadd_pd(diff0,diff0,_mm512_fmadd_pd(diff1,diff1,_mm512_mul_pd(diff2,diff2)));
			  r2    = _mm512_sqrt_pd(sarg);
			  _mm512_storeu_pd(&range[0],_mm512_add_pd(r1,r2));
			  if(sysType==0||sysType==2||sysType==3) {
                             __mmask8 m1,m2;
			     m1 = _mm512_cmp_pd_mask(CL1,_0,_CMP_EQ_OQ);
			     m2 = _mm512_cmp_pd_mask(CL0,_0,_CMP_EQ_OQ);
			     if(m1 && m2) {
                                _mm512_storeu_pd(&az[0],_0);
			     }
			     else {
                                _mm512_storeu_pd(&az[0],_mm512_atan2_pd(CL1,CL0));
			     }
			     _mm512_storeu_pd(&elev[0],_mm512_atan2_pd(CL2,_mm512_hypot_pd(CL0,CL1)));
			     if(sysType==2) {
                                const __m512d pi2 = _mm512_set1_pd(1.5707963267948966192313);
				_mm512_storeu_pd(&elev[0],_mm512_sub_pd(pi2,_mm512_loadu_pd(&elev[0])));
			     }
			     else if(sysType==3) {
                                const __m512d pi2 = _mm512_set1_pd(1.5707963267948966192313);
				_mm512_storeu_pd(&az[0],_mm512_sub_pd(pi2,_mm512_loadu_pd(&az[0])));
			     }
			  }
			  else {
                              __mmask8 m1,m2;
			      m1 = _mm512_cmp_pd_mask(CL2,_0,_CMP_EQ_OQ);
			      m2 = _mm512_cmp_pd_mask(CL0,_0,_CMP_EQ_OQ);
			      if(m1 && m2) {
                                 _mm512_storeu_pd(&az[0],_0);
			      }
			      else {
                                 _mm512_storeu_pd(&az[0],_mm512_atan2_pd(CL0,CL2));
			      }
			      _mm512_storeu_pd(&elev[0],_mm512_atan2_pd(CL1,_mm512_hypot_pd(CL2,CL0)));
			  }

			  if(useHalfRange) {
                             _mm512_storeu_pd(&range[0],_mm512_mul_pd(_mm512_loadu_pd(&range[0],_0_5)));
			  }
		     }






 









  





