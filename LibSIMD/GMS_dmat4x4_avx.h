

#ifndef __GMS_DMAT4X4_AVX_H__
#define __GMS_DMAT4X4_AVX_H__


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


#if !defined (DMAT4X4_AVX_MAJOR)
#define DMAT4X4_AVX_MAJOR 1
#endif

#if !defined (DMAT4X4_AVX_MINOR)
#define  DMAT4X4_AVX_MINOR 0
#endif

#if !defined (DMAT4X4_AVX_MICRO)
#define  DMAT4X4_AVX_MICRO 0
#endif

#if !defined (DMAT4X4_AVX_FULLVER) 
#define  DMAT4X4_AVX_FULLVER 1000*(DMAT4X4_AVX_MAJOR)+100*(DMAT4X4_AVX_MINOR)+10*(DMAT4X4_AVX_MICRO)
#endif

#if !defined (DMAT4X4_AVX_CREATE_DATE)
#define  DMAT4X4_AVX_CREATE_DATE  "02-07-2022 09:05 + 00200 (SAT 02 JUL 2022 GMT+2)"
#endif

#if !defined (DMAT4X4_AVX_BUILD_DATE)
#define  DMAT4X4_AVX_BUILD_DATE __DATE__":"__TIME__;
#endif

#if !defined (DMAT4X4_AVX_AUTHOR)
#define  MAT4X4_AVX_AUTHOR "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
#endif

#if !defined (DMAT4X4_AVX_SYNOPSIS)
#define  DMAT4X4_AVX_SYNOPSIS "AVX accelerated 4x4 Matrix-Matrix, Matrix-Vector operations -- Fortran callable."
#endif

// Interoperable with the corresponding Fortran derived type.
typedef struct __attribute__((aligned(32))) M4x4f64 {
           double row0[4];
	   double row1[4];
	   double row2[4];
           double row3[4];
	  
}M4x4f64;


typedef struct __attribute__((aligned(32))) V1x4f64 {
           double row0[4];
          
}V1x4f64;


// Fortran callable void functions

void
M4x4f64_set0(struct M4x4f64 * __restrict)   __attribute__((noinline))
			                    __attribute__((hot))
				            __attribute__((regcall))
				            __attribute__((aligned(32)));

void
M4x4f64_set0_nt(struct M4x4f64 * __restrict)    __attribute__((noinline))
			                        __attribute__((hot))
				                __attribute__((regcall))
				                __attribute__((aligned(32)));


void
M4x4f64_set1(struct M4x4f64 * __restrict,
             const double)                  __attribute__((noinline))
			                    __attribute__((hot))
				            __attribute__((regcall))
				            __attribute__((aligned(32)));

void
M4x4f64_set1_nt(struct M4x4f64 * __restrict,
                const double)               __attribute__((noinline))
			                    __attribute__((hot))
				            __attribute__((regcall))
				            __attribute__((aligned(32)));


void
M4x4f64_set16s(struct M4x4f64 * __restrict,
               const double, const double,
	       const double, const double,
	       const double, const double,
	       const double, const double,
               const double, const double,
	       const double, const double,
	       const double, const double,
	       const double, const double)  __attribute__((noinline))
			                    __attribute__((hot))
				            __attribute__((regcall))
				            __attribute__((aligned(32)));


void
M4x4f64_set16s_nt(struct M4x4f64 * __restrict,
                  const double, const double,
	          const double, const double,
	          const double, const double,
	          const double, const double,
                  const double, const double,
	          const double, const double,
	          const double, const double,
	          const double, const double)  __attribute__((noinline))
			                       __attribute__((hot))
				               __attribute__((regcall))
				               __attribute__((aligned(32)));


void
M4x4f64_set8a(struct M4x4f64 * __restrict,
              const double * __restrict,
	      const double * __restrict,
	      const double * __restrict,
	      const double * __restrict,
	      const double * __restrict,
	      const double * __restrict,
	      const double * __restrict,
	      const double * __restrict)       __attribute__((noinline))
			                       __attribute__((hot))
				               __attribute__((regcall))
				               __attribute__((aligned(32)));

void
M4x4f64_set8a_nt(struct M4x4f64 * __restrict,
                 const double * __restrict,
	         const double * __restrict,
	         const double * __restrict,
	         const double * __restrict,
	         const double * __restrict,
	         const double * __restrict,
	         const double * __restrict,
	         const double * __restrict)    __attribute__((noinline))
			                       __attribute__((hot))
				               __attribute__((regcall))
				               __attribute__((aligned(32)));

void
M4x4f64_set1a(struct M4x4f64 * __restrict,
              const double * __restrict)       __attribute__((noinline))
			                       __attribute__((hot))
				               __attribute__((regcall))
				               __attribute__((aligned(32)));


void
M4x4f64_set1a_nt(struct M4x4f64 * __restrict,
                 const double * __restrict)    __attribute__((noinline))
			                       __attribute__((hot))
				               __attribute__((regcall))
				               __attribute__((aligned(32)));


void
M4x4f64_set_M4x4f64(struct M4x4f64 * __restrict,
                    const struct M4x4f64 * __restrict)  __attribute__((noinline))
			                                __attribute__((hot))
				                        __attribute__((regcall))
				                        __attribute__((aligned(32)));

void
M4x4f64_set_M4x4f64_nt(struct M4x4f64 * __restrict,
                       const struct M4x4f64 * __restrict)  __attribute__((noinline))
			                                __attribute__((hot))
				                        __attribute__((regcall))
				                        __attribute__((aligned(32)));


void
M4x4f64_identity(struct M4x4f64 * __restrict)           __attribute__((noinline))
			                                __attribute__((hot))
				                        __attribute__((regcall))
				                        __attribute__((aligned(32)));


void
M4x4f64_add_M4x4f64(struct M4x4f64 * __restrict,
		    const struct M4x4f64 *  __restrict,
		    const struct M4x4f64 * __restrict)   __attribute__((noinline))
			                                 __attribute__((hot))
				                         __attribute__((regcall))
				                         __attribute__((aligned(32)));

void
M4x4f64_add_M4x4f64_ip(struct M4x4f64 * __restrict,
		       const struct M4x4f64 * __restrict)  __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_sub_M4x4f64(struct M4x4f64 * __restrict,
		    const struct M4x4f64 * __restrict,
		    const struct M4x4f64 * __restrict)     __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_sub_M4x4f64_ip(struct M4x4f64 * __restrict,
		       const struct M4x4f64 * __restrict)   __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));


// Matrix 4x4 - mul - Matrix 4x4
void
M4x4f64_mul_M4x4f64(struct M4x4f64 * __restrict , 
		    const struct M4x4f64 * __restrict , 
		    const struct M4x4f64 * __restrict )    __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_mul_M4x4f64_ip(struct M4x4f64 * __restrict,
		       const struct M4x4f64 * __restrict)  __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_mul_1s(struct M4x4f64 * __restrict,
	       const struct M4x4f64 * __restrict,
	       const double)                               __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));                             

void
M4x4f64_mul_1s_ip(struct M4x4f64 * __restrict,
		  const double)                            __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));    

void
M4x4f64_negate(struct M4x4f64 * __restrict,
	       const struct M4x4f64 * __restrict)          __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));    

void
M4x4f64_transpose(struct M4x4f64 * __restrict)             __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));


void
M4x4f64_rotateX(struct M4x4f64 * __restrict,
                const double)                              __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_rotateY(struct M4x4f64 * __restrict,
		const double)                              __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_rotateZ(struct M4x4f64 * __restrict,
		const double)                              __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_translate_xyz(struct M4x4f64 * __restrict,
		      const double,
		      const double,
		      const double)                        __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_scale_xyz(struct M4x4f64 * __restrict,
		  const double,
		  const double,
		  const double)                            __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_scale_s(struct M4x4f64 * __restrict,
		const double)                              __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_minimum(double * ,
		const struct M4x4f64 * __restrict)         __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_maximum(double * ,
		const struct M4x4f64 * __restrict)         __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_det(double * ,
	    const struct M4x4f64 * __restrict)             __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
M4x4f64_inverse(double * ,
		const struct M4x4f64 * __restrict)         __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_set0(struct V1x4f64 * __restrict)                __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_set0_nt(struct V1x4f64 * __restrict)               __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_set1(struct V1x4f64 * __restrict,
	     const double)                                 __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_set1_nt(struct V1x4f64 * __restrict,
		    const double)                           __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_set4s(struct V1x4f64 * __restrict,
	       const double, const double,
	       const double, const double)                 __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_set4s_nt(struct V1x4f64 * __restrict,
		  const double, const double,
		  const double, const double)              __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_seta(struct V1x4f64 * __restrict,
	      const double * __restrict)                   __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_seta_nt(struct V1x4f64 * __restrict,
		const double * __restrict)                 __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_set_V1x4f64(struct V1x4f64 * __restrict,
		    const struct V1x4f64 * __restrict)     __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_set_V1x4f64_nt(struct V1x4f64 * __restrict,
		       const struct V1x4f64 * __restrict)   __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_set_M4x4f64(struct V1x4f64 * __restrict,
		    const struct M4x4f64 * __restrict,
		    const int)                             __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_set_M4x4f64_nt(struct V1x4f64 * __restrict,
		       const struct M4x4f64 * __restrict,
		       const int)                           __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_mul_M4x4f64(struct V1x4f64 * __restrict,
		    const struct V1x4f64 * __restrict ,
		    const struct M4x4f64 * __restrict)     __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_mul_M4x4f64_ip(struct V1x4f64 * __restrict,
		       const struct M4x4f64 * __restrict)   __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_product(struct V1x4f64 * __restrict,
		const struct V1x4f64 * __restrict,
		const struct V1x4f64 * __restrict)         __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_product_ip(struct V1x4f64 * __restrict,
	           const struct V1x4f64 * __restrict)       __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_mul_1s(struct V1x4f64 * __restrict ,
		   const struct V1x4f64 * __restrict,
		   const double)                           __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_add_V1x4f64(struct V1x4f64 * __restrict,
		    const struct V1x4f64 * __restrict,
		    const struct V1x4f64 * __restrict)    __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_add_V1x4f64_inplace(struct V1x4f64 * __restrict,
			    const struct V1x4f64 * __restrict)   __attribute__((noinline))
			                                         __attribute__((hot))
				                                 __attribute__((regcall))
				                                 __attribute__((aligned(32)));

void
V1x4f64_sub_V1x4f64(struct V1x4f64 * __restrict,
		    const struct V1x4f64 * __restrict,
		    const struct V1x4f64 * __restrict)     __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_sub_V1x4f64_inplace(struct V1x4f64 * __restrict,
			    const struct V1x4f64 * __restrict)  __attribute__((noinline))
			                                        __attribute__((hot))
				                                __attribute__((regcall))
				                                __attribute__((aligned(32)));

void
V1x4f64_dot(double * ,
	    const struct V1x4f64 * __restrict,
	    const struct V1x4f64 * __restrict)             __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));

void
V1x4f64_cross(struct V1x4f64 * __restrict,
	      const struct V1x4f64 * __restrict,
	      const struct V1x4f64 * __restrict)            __attribute__((noinline))
			                                   __attribute__((hot))
				                           __attribute__((regcall))
				                           __attribute__((aligned(32)));





#endif /*__GMS_DMAT4X4_AVX_H__*/
