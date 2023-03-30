

#ifndef __GMS_COMPLEX_ZMM16R4_H__
#define __GMS_COMPLEX_ZMM16R4_H__ 251220220954


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


    const unsigned int GMS_COMPLEX_ZMM16R4_MAJOR = 1U;
    const unsigned int GMS_COMPLEX_ZMM16R4_MINOR = 0U;
    const unsigned int GMS_COMPLEX_ZMM16R4_MICRO = 0U;
    const unsigned int GMS_COMPLEX_ZMM16R4_FULLVER =
      1000U*GMS_COMPLEX_ZMM16R4_MAJOR+
      100U*GMS_COMPLEX_ZMM16R4_MINOR+
      10U*GMS_COMPLEX_ZMM16R4_MICRO;
    const char * const GMS_COMPLEX_ZMM16R4_CREATION_DATE = "25-12-2022 09:54 AM +00200 (SUN 25 DEC 2022 GMT+2)";
    const char * const GMS_COMPLEX_ZMM16R4_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_COMPLEX_ZMM16R4_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_COMPLEX_ZMM16R4_DESCRIPTION   = "AVX512 optimized complex number implementation."


#include <cstdint>
#include <immintrin.h>


                   void cadd_zmm16r4_u(const float * __restrict xre,
                                       const float * __restrict xim,
                                       const float * __restrict yre,
                                       const float * __restrict yim,
                                       float *       __restrict zre,
                                       float *       __restrict zim)  
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                   void cadd_zmm16r4_a(const float * __restrict __attribute__((aligned(64))) xre,
                                       const float * __restrict __attribute__((aligned(64))) xim,
                                       const float * __restrict __attribute__((aligned(64))) yre,
                                       const float * __restrict __attribute__((aligned(64))) yim,
                                       float *       __restrict __attribute__((aligned(64))) zre,
                                       float *       __restrict __attribute__((aligned(64))) zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));   


                  void cadd_zmm16r4( const __m512 xre,
                                     const __m512 xim,
                                     const __m512 yre,
                                     const __m512 yim,
                                     __m512 * __restrict zre,
                                     __m512 * __restrict zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  void cadd_zmm16r4( const __m512 xre,
                                     const __m512 xim,
                                     const __m512 s,
                                     __m512 * __restrict     zre,
                                     __m512 * __restrict     zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                 void cadd_zmm16r4_uip(  const float * __restrict xre,
                                         const float * __restrict xim,
                                         float *       __restrict zre,
                                         float *       __restrict zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                 void cadd_zmm16r4_aip(  const float * __restrict __attribute__((aligned(64))) xre,
                                         const float * __restrict __attribute__((aligned(64))) xim,
                                         float *       __restrict __attribute__((aligned(64))) zre,
                                         float *       __restrict __attribute__((aligned(64))) zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                     ////////////////////////////////////////////////////////////////////


                 void csub_zmm16r4_u( const float * __restrict xre,
                                       const float * __restrict xim,
                                       const float * __restrict yre,
                                       const float * __restrict yim,
                                       float *       __restrict zre,
                                       float *       __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                 void csub_zmm16r4_a(  const float * __restrict __attribute__((aligned(64)))  xre,
                                       const float * __restrict __attribute__((aligned(64)))  xim,
                                       const float * __restrict __attribute__((aligned(64)))  yre,
                                       const float * __restrict __attribute__((aligned(64)))  yim,
                                       float *       __restrict __attribute__((aligned(64)))  zre,
                                       float *       __restrict __attribute__((aligned(64)))  zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  void csub_zmm16r4( const __m512 xre,
                                     const __m512 xim,
                                     const __m512 yre,
                                     const __m512 yim,
                                     __m512 * __restrict     zre,
                                     __m512 * __restrict     zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  void csub_zmm16r4( const __m512 xre,
                                     const __m512 xim,
                                     const __m512 s,
                                     __m512 * __restrict     zre,
                                     __m512 * __restrict     zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  void csub_zmm16r4_uip( const float * __restrict xre,
                                         const float * __restrict xim,
                                         float *       __restrict zre,
                                         float *       __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  void csub_zmm16r4_aip( const float * __restrict __attribute__((aligned(64)))  xre,
                                         const float * __restrict __attribute__((aligned(64)))  xim,
                                         float *       __restrict __attribute__((aligned(64)))  zre,
                                         float *       __restrict __attribute__((aligned(64)))  zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  ////////////////////////////////////////////////////////////////////////


                 void cmul_zmm16r4_u(  const float * __restrict xre,
                                       const float * __restrict xim,
                                       const float * __restrict yre,
                                       const float * __restrict yim,
                                       float *       __restrict zre,
                                       float *       __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                 void cmul_zmm16r4_a(  const float * __restrict __attribute__((aligned(64)))  xre,
                                       const float * __restrict __attribute__((aligned(64)))  xim,
                                       const float * __restrict __attribute__((aligned(64)))  yre,
                                       const float * __restrict __attribute__((aligned(64)))  yim,
                                       float *       __restrict __attribute__((aligned(64)))  zre,
                                       float *       __restrict __attribute__((aligned(64)))  zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                 void cmul_zmm16r4(  const __m512 xre,
                                     const __m512 xim,
                                     const __m512 yre,
                                     const __m512 yim,
                                     __m512 * __restrict     zre,
                                     __m512 * __restrict     zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                 void cmul_zmm16r4(  const __m512 xre,
                                     const __m512 xim,
                                     const __m512 s,
                                     __m512 * __restrict   zre,
                                     __m512 * __restrict   zim)
                                                        __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                 void cmul_zmm16r4_uip(  const float * __restrict xre,
                                         const float * __restrict xim,
                                         float *       __restrict zre,
                                         float *       __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  

 
                 void cmul_zmm16r4_aip(  const float * __restrict __attribute__((aligned(64))) xre,
                                         const float * __restrict __attribute__((aligned(64))) xim,
                                         float *       __restrict __attribute__((aligned(64))) zre,
                                         float *       __restrict __attribute__((aligned(64))) zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


#endif /*__GMS_COMPLEX_ZMM16R4_H__*/
