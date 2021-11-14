

#ifndef __GMS_ROTATION_KERNELS_AVX512_H__
#define __GMS_ROTATION_KERNELS_AVX512_H__ 121120210945



const unsigned int gGMS_ROTATION_KERNELS_AVX512_MAJOR = 1U;
const unsigned int gGMS_ROTATION_KERNELS_AVX512_MINOR = 0U;
const unsigned int gGMS_ROTATION_KERNELS_AVX512_MICRO = 0U;
const unsigned int gGMS_ROTATION_KERNELS_AVX512_FULLVER =
       1000U*gGMS_ROTATION_KERNELS_AVX512_MAJOR+
       100U*gGMS_ROTATION_KERNELS_AVX512_MINOR +
       10U*gGMS_ROTATION_KERNELS_AVX512_MICRO;
const char * const pgGMS_ROTATION_KERNELS_AVX512_CREATION_DATE = "12-11-2021 09:45 PM +00200 (FRI 12 NOV 2021 GMT+2)";
const char * const pgGMS_ROTATION_KERNELS_AVX512_BUILD_DATE    = __DATE__ ":" __TIME__;
const char * const pgGMS_ROTATION_KERNELS_AVX512_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
const char * const pgGMS_ROTATION_KERNELS_AVX512_DESCRIPTION   = "AVX512 vectorized basic rotation operations.";


     /*
                                 This version is *loosely based on the Fortran 90 "rotation.f90" source code
                                 implementation.
                                 
                                 *Many optimizations were applied (precomputation of common subexpression,
                                 constants folding,  AVX512 vectorization.)
                                 The original authors copyright statement
                                 Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
                                 Modified      2017-2020, Martin Diehl/Max-Planck-Institut für Eisenforschung GmbH
                                 All rights reserved.
                         */


#include <immintrin.h>


// Interoperable with corresponding Fortran structure.
   // Direct Cosine Matrix
   typedef struct __attribute__((aligned(64))) RotM9x16v16 {

           __m512 row0;
	   __m512 row1;
	   __m512 row2;
	   __m512 row3;
	   __m512 row4;
	   __m512 row5;
	   __m512 row6;
	   __m512 row7;
	   __m512 row8;

   }RotM9x16v16;

   // Direct Cosine Matrix
   typedef struct __attribute__((aligned(64))) RotM9x8v8 {

           __m512d row0;
	   __m512d row1;
	   __m512d row2;
	   __m512d row3;
	   __m512d row4;
	   __m512d row5;
	   __m512d row6;
	   __m512d row7;
	   __m512d row8;
   }RotM9x8v8;

   // Euler Angles
   typedef struct __attribute__((aligned(64))) EA3x16v16 {

           __m512 alpha;
	   __m512 beta;
	   __m512 gamma;
   }EA3x16v16;

   // Euler Angles
   typedef struct __attribute__((aligned(64))) EA3x8v8 {

           __m512d alpha;
	   __m512d beta;
	   __m512d gamma;
   }EA3x8v8;

   // Axis-angles pair
   typedef struct __attribute__((aligned(64))) AX4x16v16 {

           __m512 ax_1;
	   __m512 ax_2;
	   __m512 ax_3;
	   __m512 ax_4;
   }AX4x16v16;


   // Axis-angles pair
   typedef struct __attribute__((aligned(64))) AX4x8v8 {

           __m512d ax_1;
	   __m512d ax_2;
	   __m512d ax_3;
	   __m512d ax_4;
   }AX4x8v8;


   // Rodrigues Vector
   typedef struct __attribute__((aligned(64))) RV4x16v16 {

           __m512 r_x;
	   __m512 r_y;
	   __m512 r_z;
	   __m512 r_w;
   }RV4x16v16;


   // Rodrigues Vector
   typedef struct __attribute__((aligned(64))) RV4x8v8 {

           __m512d r_x;
	   __m512d r_y;
	   __m512d r_z;
	   __m512d r_w;
   }RV4x8v8;

                                static const __m512 v16_1      = _mm512_set1_ps(1.0F);
                                static const __m512 v16_inf    = _mm512_div_ps(v16_1,v16_0);
                                static const __m512 v16_0      = _mm512_set1_ps(0.0F);
		                static const __m512 v16_2      = _mm512_set1_ps(2.0f);
                                static const __m512 v16_n1     = _mm512_set1_ps(-1.0f);
			        static const __m512 v16_n2     = _mm512_set1_ps(-2.0F);
			        static const __m512 v16_spi    = _mm512_set1_ps(1.7724538509055160272982F);
			        static  const __m512 v16_s6pi   = _mm512_set1_ps(1.381976597885341917061F);
			        static  const __m512 v16_a      = _mm512_set1_ps(1.9257490199582527754939F);
			        static const __m512 v16_ap     = _mm512_set1_ps(2.1450293971110256000775F);
			        static const __m512 v16_sc     = _mm512_set1_ps(0.8977727869612861128953F);
			        static const __m512 v16_beta   = _mm512_set1_ps(0.9628745099791263877469F);
			        static const __m512 v16_r1     = _mm512_set1_ps(1.3306700394914687909256F);
			        static const __m512 v16_r2     = _mm512_set1_ps(1.4142135623730950488017F);
			        static const __m512 v16_pi12   = _mm512_set1_ps(0.2617993877991494365386F);
			        static const __m512 v16_prek   = _mm512_set1_ps(1.6434564029725030125017F);
			        static const __m512 v16_pi     = _mm512_set1_pd(3.1415926535897932384626F);
			        static const __m512 v16_2pi    = _mm512_set1_pd(6.2831853071795864769253F);
			        
                                static const __m512 v16_n1     = _mm512_set1_pd(-1.0);
			        static const __m512 v16_n2     = _mm512_set1_pd(-2.0)
				static const __m512 v8_2       = _mm512_set1_pd(2.0);
				static const __m512d v8_1      = _mm512_set1_pd(1.0);
				static const __m512d v8_0      = _mm512_setzero_pd();
				static const __m512d v8_inf    = _mm512_div_pd(v8_1,v8_0);
			        static const __m512d v8_n1     = _mm512_set1_pd(-1.0);
			        static const __m512d  v8_pi     = _mm512_set1_pd(3.1415926535897932384626);
			        static const __m512d  v8_2pi    = _mm512_set1_pd(6.2831853071795864769253);
			        static const __m512d v8_spi    = _mm512_set1_pd(1.7724538509055160272982);
			        static const __m512d v8_s6pi   = _mm512_set1_pd(1.381976597885341917061);
			        static const __m512d v8_a      = _mm512_set1_pd(1.9257490199582527754939);
			        static const __m512d v8_ap     = _mm512_set1_pd(2.1450293971110256000775);
			        static const __m512d v8_sc     = _mm512_set1_pd(0.8977727869612861128953);
			        static const __m512d v8_beta   = _mm512_set1_pd(0.9628745099791263877469);
			        static const __m512d v8_r1     = _mm512_set1_pd(1.3306700394914687909256);
			        static const __m512d v8_r2     = _mm512_set1_pd(1.4142135623730950488017);
			        static const __m512d v8_pi12   = _mm512_set1_pd(0.2617993877991494365386);
			        static const __m512d v8f_prek   = _mm512_set1_pd(1.6434564029725030125017);



                      __attribute__((regcall))
		      __attribute__((always_inline))
		      __attribute__((aligned(32)))
                      static inline
		      __m512 fmod_zmm16r4(const __m512 a,
		                          const __m512 b) {

                          __m512 v = _mm512_sub_ps(a,_mm512_mul_ps(
			             _mm512_div_round_ps(a,b,_MM_FROUND_TO_ZERO|_MM_FROUND_NO_EXEC),b));
			  return (v);
			  
		     }


		      __attribute__((regcall))
		      __attribute__((always_inline))
		      __attribute__((aligned(32)))
		      static inline
		      __m512d fmod_zmm8r8(const __m512d a,
		                          const __m512d b) {

                          __m512d v = _mm512_sub_pd(a,_mm512_mul_pd(
			             _mm512_div_round_pd(a,b,_MM_FROUND_TO_ZERO|_MM_FROUND_NO_EXEC),b));
			  return (v);
			  
		     }

		      __attribute__((regcall))
		      __attribute__((always_inline))
		      __attribute__((aligned(32)))
	              static inline
		      __m512d
		      zmm8r8_sign_zmm8r8(const __m512d va,
				         const __m512d vb) {
				       
			   register __m512d vret = _0;
			   register __m512d t0   = _mm512_abs_pd(va);
                           __mmask8 gez = 0x0;
			   gez  = _mm512_cmp_pd_mask(vb,v8_0,_CMP_GE_OQ); // Lat=3refc,Thr=1refc
			   vret = _mm512_mask_blend_pd(gez,t0,_mm512_sub_pd(v8_0,t0)); //Lat=1refc,Thr=0.5refc,Lat=4refc,Thr=1refc
			   return (vret);
				                                       
		    }


		     __attribute__((regcall))
		     __attribute__((always_inline))
		     __attribute__((aligned(32)))
	            static inline
		    __m512
		    zmm16r4_sign_zmm16r4(const __m512 va,
				         const __m512 vb) {
				       
			  register __m512 vret = _0;
			  register __m512 t0   = _mm512_abs_ps(va);
                          __mmask8 gez = 0x0;
			  gez  = _mm512_cmp_ps_mask(vb,v16_0,_CMP_GE_OQ); // Lat=3refc,Thr=1refc
			  vret = _mm512_mask_blend_ps(gez,t0,_mm512_sub_ps(v16_0,t0)); //Lat=1refc,Thr=0.5refc,Lat=4refc,Thr=1refc
			  return (vret);
				                                       
		   }


		      __attribute__((regcall))
		      __attribute__((always_inline))
		      __attribute__((aligned(32)))
		      static inline
		      __m512d norm2_zmm8r8(const __m512d y,
		                          const __m512d z,
					  const __m512d w) {

                            const __m512d t0 = _mm512_mul_pd(y,y);
			    const __m512d t1 = _mm512_mul_pd(z,z);
			    const __m512d t2 = _mm512_mul_pd(w,w);
			    const __m512d v  = _mm512_add_pd(t0,_mm512_add_pd(t1,t2));
			    return (_mm512_sqrt_pd(v));
			    
		      }


		      __attribute__((regcall))
		      __attribute__((always_inline))
		      __attribute__((aligned(32)))
		      static inline
		      __m512d norm2_zmm16r4(const __m512 y,
		                            const __m512 z,
					    const __m512 w) {

                            const __m512 t0 = _mm512_mul_ps(y,y);
			    const __m512 t1 = _mm512_mul_ps(z,z);
			    const __m512 t2 = _mm512_mul_ps(w,w);
			    const __m512 v  = _mm512_add_ps(t0,_mm512_add_ps(t1,t2));
			    return (_mm512_sqrt_ps(v));
			    
		      }


		      __attribute__((regcall))
		      __attribute__((always_inline))
		      __attribute__((aligned(32)))
		      static inline
		      __m512 clip_zmm16r4(const __m512 x,
		                          const __m512 lo,
					  const __m512 hi) {

                           return (_mm512_max_ps(lo,_mm512_min_ps(x,hi)));
		      }


		      __attribute__((regcall))
		      __attribute__((always_inline))
		      __attribute__((aligned(32)))
		      static inline
		      __m512d clip_zmm8r8(const __m512d x,
		                          const __m512d lo,
					  const __m512d hi) {

                           return (_mm512_max_pd(lo,_mm512_min_pd(x,hi)));
		      }

			    

                    RotM9x16v16
		    q4x16_to_rmat9x16_zmm16r4(  const __m512,
		                                const __m512,
						const __m512,
						const __m512)     __attribute__((regcall))
						                  __attribute__((hot))
								  __attribute__((noinline))
								  __attribute__((aligned(32)));

                    RotM9x8v8
		    q4x8_to_rmat9x8_zmm8r8(  const __m512d,
		                             const __m512d,
					     const __m512d,
					     const __m512d)       __attribute__((regcall))
						                  __attribute__((hot))
								  __attribute__((noinline))
								  __attribute__((aligned(32)));

                    EA3x16v16
		    q4x16_to_ea3x16_zmm16r4(  const __m512,
		                              const __m512,
					      const __m512,
					      const __m512)       __attribute__((regcall))
						                  __attribute__((hot))
								  __attribute__((noinline))
								  __attribute__((aligned(32)));

                     EA3x8v8
		     q4x8_to_ea3x8_zmm8r8(    const __m512d,
		                              const __m512d,
					      const __m512d,
					      const __m512d)      __attribute__((regcall))
						                  __attribute__((hot))
								  __attribute__((noinline))
								  __attribute__((aligned(32)));


		     AX4x16v16
		     q4x16_to_ax4x16_zmm16r4( const __m512,
		                              const __m512,
					      const __m512,
					      const __m512)       __attribute__((regcall))
						                  __attribute__((hot))
								  __attribute__((noinline))
								  __attribute__((aligned(32)));


		     AX4x8v8
		     q4x8_to_ax4x8_zmm8r8(    const __m512d,
		                              const __m512d,
					      const __m512d,
					      const __m512d)      __attribute__((regcall))
						                  __attribute__((hot))
								  __attribute__((noinline))
								  __attribute__((aligned(32)));


		     RV4x16v16
		     q4x16_to_rv4x16_zmm16r4( const __m512,
		                              const __m512,
					      const __m512,
					      const __m512)       __attribute__((regcall))
						                  __attribute__((hot))
								  __attribute__((noinline))
								  __attribute__((aligned(32)));


		     RV4x8v8
		     q4x8_to_rv4x8_zmm8r8(    const __m512d,
		                              const __m512d,
					      const __m512d,
					      const __m512d)      __attribute__((regcall))
						                  __attribute__((hot))
								  __attribute__((noinline))
								  __attribute__((aligned(32)));


		     EA3x16v16
		     rmat9x16_to_ea3x16_zmm16r4(const RotM9x16v16)  __attribute__((regcall))
						                    __attribute__((hot))
								    __attribute__((noinline))
								    __attribute__((aligned(32)));


		     EA3x8v8
                     rmat9x8_to_ea3x8_zmm8r8(const RotM9x8v8)       __attribute__((regcall))
						                    __attribute__((hot))
								    __attribute__((noinline))
								    __attribute__((aligned(32)));

		     

                     

		    







#endif /*__GMS_ROTATION_KERNELS_AVX512_H__*/
