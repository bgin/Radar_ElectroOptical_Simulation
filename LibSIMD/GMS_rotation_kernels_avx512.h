

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
