

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


                  ////////////////////////////////////////////////////////////////////////////


                  void cdiv_zmm16r4_u( const float * __restrict xre,
                                       const float * __restrict xim,
                                       const float * __restrict yre,
                                       const float * __restrict yim,
                                       float *       __restrict zre,
                                       float *       __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  void cdiv_zmm16r4_a(const float  * __restrict __attribute__((aligned(64))) xre,
                                       const float * __restrict __attribute__((aligned(64))) xim,
                                       const float * __restrict __attribute__((aligned(64))) yre,
                                       const float * __restrict __attribute__((aligned(64))) yim,
                                       float *       __restrict __attribute__((aligned(64))) zre,
                                       float *       __restrict __attribute__((aligned(64))) zim)                  
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                  void cdiv_zmm16r4( const __m512 xre,
                                     const __m512 xim,
                                     const __m512 yre,
                                     const __m512 yim,
                                     __m512 * __restrict zre,
                                     __m512 * __restrict zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                 void cdiv_zmm16r4(  const __m512 xre,
                                     const __m512 xim,
                                     const __m512 s,
                                     __m512 * __restrict zre,
                                     __m512 * __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                void cdiv_zmm16r4_uip(   const float * __restrict xre,
                                         const float * __restrict xim,
                                         float *       __restrict zre,
                                         float *       __restrict zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                void cdiv_zmm16r4_aip(   const float * __restrict __attribute__((aligned(64))) xre,
                                         const float * __restrict __attribute__((aligned(64))) xim,
                                         float *       __restrict __attribute__((aligned(64))) zre,
                                         float *       __restrict __attribute__((aligned(64))) zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

                
                void cdiv_smith_zmm16r4_u(   const float * __restrict xre,
                                             const float * __restrict xim,
                                             const float * __restrict yre,
                                             const float * __restrict yim,
                                             float *       __restrict zre,
                                             float *       __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                void cdiv_smith_zmm16r4_a(   const float * __restrict __attribute__((aligned(64))) xre,
                                             const float * __restrict __attribute__((aligned(64))) xim,
                                             const float * __restrict __attribute__((aligned(64))) yre,
                                             const float * __restrict __attribute__((aligned(64))) yim,
                                             float *       __restrict __attribute__((aligned(64))) zre,
                                             float *       __restrict __attribute__((aligned(64))) zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                void cdiv_smith_zmm16r4(   const __m512 xre,
                                           const __m512 xim,
                                           const __m512 yre,
                                           const __m512 yim,
                                           __m512 * __restrict zre,
                                           __m512 * __restrict zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                ///////////////////////////////////////////////////////////////////////////


               void cabs_zmm16r4_u(    const float * __restrict re,
                                       const float * __restrict im,
                                       float * __restrict  cabs)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


               void cabs_zmm16r4_a(    const float * __restrict __attribute__((aligned(64))) re,
                                       const float * __restrict __attribute__((aligned(64))) im,
                                       float * __restrict  __attribute__((aligned(64))) cabs)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


               __m512 cabs_zmm16r4(    const __m512 re,
                                       const __m512 im)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             ////////////////////////////////////////////////////////////////////////////


             void  carg_zmm16r4_u(     const float * __restrict re,
                                       const float * __restrict im,
                                       float * __restrict  carg )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             void carg_zmm16r4_a(      const float * __restrict __attribute__((aligned(64))) re,
                                       const float * __restrict __attribute__((aligned(64))) im,
                                       float * __restrict  __attribute__((aligned(64))) carg)  
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             __m512 carg_zmm16r4(      const __m512 re,
                                       const __m512 im)
                                                        __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

        
            //////////////////////////////////////////////////////////////////////////////


             void cconj_zmm16r4_u(      const float * __restrict im,
                                        float * __restrict  conj)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             void cconj_zmm16r4_a(      const float * __restrict __attribute__((aligned(64))) im,
                                        float * __restrict  __attribute__((aligned(64))) conj)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


              __m512 cconj_zmm16r4(const __m512 im) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


           /////////////////////////////////////////////////////////////////////////////////


            void ccos_zmm16r4_u(       const float * __restrict xre,
                                       const float * __restrict xim,
                                       float * __restrict  csre,
                                       float * __restrict  csim)  
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


            void ccos_zmm16r4_a(       const float * __restrict __attribute__((aligned(64))) xre,
                                       const float * __restrict __attribute__((aligned(64))) xim,
                                       float * __restrict  __attribute__((aligned(64))) csre,
                                       float * __restrict  __attribute__((aligned(64))) csim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


            void ccos_zmm16r4(       const __m512 xre,
                                     const __m512 xim,
                                     __m512 * __restrict csre,
                                     __m512 * __restrict csim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


            //////////////////////////////////////////////////////////////////////////////


            void ccosh_zmm16r4_u(const float * __restrict xre,
                                       const float * __restrict xim,
                                       float * __restrict  csre,
                                       float * __restrict  csim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


             void ccosh_zmm16r4_a(const float * __restrict  __attribute__((aligned(64))) xre,
                                       const float * __restrict __attribute__((aligned(64))) xim,
                                       float * __restrict  __attribute__((aligned(64))) csre,
                                       float * __restrict  __attribute__((aligned(64))) csim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             void ccosh_zmm16r4(     const __m512 xre,
                                     const __m512 xim,
                                     __m512 * __restrict csre,
                                     __m512 * __restrict csim) 
                                                          __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             /////////////////////////////////////////////////////////////////////////////


             void ceq_zmm16r4_u(      const float * __restrict xre,
                                      const float * __restrict xim,
                                      const float * __restrict yre,
                                      const float * __restrict yim,
                                      __mmask16 * __restrict eqr,
                                      __mmask16 * __restrict eqi )
                                                          __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            void ceq_zmm16r4_a(       const float * __restrict __attribute__((aligned(64))) xre,
                                      const float * __restrict __attribute__((aligned(64))) xim,
                                      const float * __restrict __attribute__((aligned(64))) yre,
                                      const float * __restrict __attribute__((aligned(64))) yim,
                                      __mmask16 * __restrict  eqr,
                                      __mmask16 * __restrict  eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

             void ceq_zmm16r4(      const __m512 xre,
                                    const __m512 xim,
                                    const __m512 yre,
                                    const __m512 yim,
                                    __mmask16 * __restrict eqr,
                                    __mmask16 * __restrict eqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            //////////////////////////////////////////////////////////////////////////////////


            void cgt_zmm16r4_u(       const float * __restrict xre,
                                      const float * __restrict xim,
                                      const float * __restrict yre,
                                      const float * __restrict yim,
                                      __mmask16 * __restrict eqr,
                                      __mmask16 * __restrict eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            void cgt_zmm16r4_a(       const float * __restrict __attribute__((aligned(64))) xre,
                                      const float * __restrict __attribute__((aligned(64))) xim,
                                      const float * __restrict __attribute__((aligned(64))) yre,
                                      const float * __restrict __attribute__((aligned(64))) yim,
                                      __mmask16 * __restrict  eqr,
                                      __mmask16 * __restrict  eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

             void cgt_zmm16r4(      const __m512 xre,
                                    const __m512 xim,
                                    const __m512 yre,
                                    const __m512 yim,
                                    __mmask16 * __restrict eqr,
                                    __mmask16 * __restrict eqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

            ///////////////////////////////////////////////////////////////////////////////////////


            void clt_zmm16r4_u(       const float * __restrict xre,
                                      const float * __restrict xim,
                                      const float * __restrict yre,
                                      const float * __restrict yim,
                                      __mmask16 * __restrict eqr,
                                      __mmask16 * __restrict eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            void clt_zmm16r4_a(       const float * __restrict __attribute__((aligned(64))) xre,
                                      const float * __restrict __attribute__((aligned(64))) xim,
                                      const float * __restrict __attribute__((aligned(64))) yre,
                                      const float * __restrict __attribute__((aligned(64))) yim,
                                      __mmask16 * __restrict  eqr,
                                      __mmask16 * __restrict  eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

             void clt_zmm16r4(      const __m512 xre,
                                    const __m512 xim,
                                    const __m512 yre,
                                    const __m512 yim,
                                    __mmask16 * __restrict eqr,
                                    __mmask16 * __restrict eqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             //////////////////////////////////////////////////////////////////////////////////////////


              void cneq_zmm16r4_u(       const float * __restrict xre,
                                      const float * __restrict xim,
                                      const float * __restrict yre,
                                      const float * __restrict yim,
                                      __mmask16 * __restrict eqr,
                                      __mmask16 * __restrict eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            void cneq_zmm16r4_a(       const float * __restrict __attribute__((aligned(64))) xre,
                                      const float * __restrict __attribute__((aligned(64))) xim,
                                      const float * __restrict __attribute__((aligned(64))) yre,
                                      const float * __restrict __attribute__((aligned(64))) yim,
                                      __mmask16 * __restrict  eqr,
                                      __mmask16 * __restrict  eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

             void cneq_zmm16r4(      const __m512 xre,
                                    const __m512 xim,
                                    const __m512 yre,
                                    const __m512 yim,
                                    __mmask16 * __restrict eqr,
                                    __mmask16 * __restrict eqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            //////////////////////////////////////////////////////////////////////////////////////////


            void cexp_zmm16r4_u(       const float * __restrict xre,
                                       const float * __restrict xim,
                                       float * __restrict cexpr,
                                       float * __restrict cexpi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            void cexp_zmm16r4_a(       const float * __restrict __attribute__((aligned(64))) xre,
                                       const float * __restrict __attribute__((aligned(64))) xim,
                                       float * __restrict __attribute__((aligned(64))) cexpr,
                                       float * __restrict __attribute__((aligned(64))) cexpi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            void cexp_zmm16r4(       const __m512 xre,
                                     const __m512 xim,
                                     __m512 * __restrict cexpr,
                                     __m512 * __restrict cexpi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));    

           
            //////////////////////////////////////////////////////////////////////////////////////


            void cpolar_zmm16r4_u(       const float * __restrict rho,
                                         const float * __restrict tht,
                                         float * __restrict  re,
                                         float * __restrict  im)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


           void cpolar_zmm16r4_a(        const float * __restrict __attribute__((aligned(64))) rho,
                                         const float * __restrict __attribute__((aligned(64))) tht,
                                         float * __restrict  __attribute__((aligned(64))) re,
                                         float * __restrict  __attribute__((aligned(64))) im)   
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


           void cpolar_zmm16r4(        const __m512 rho,
                                       const __m512 tht,
                                       __m512 * __restrict re,
                                       __m512 * __restrict im)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


         /////////////////////////////////////////////////////////////////////////////////////////////

 
         void csqrt_zmm16r4_u(         const float * __restrict xre,
                                       const float * __restrict xim,
                                       float * __restrict wrkc,
                                       float * __restrict csqr,
                                       float * __restrict csqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));

          
          void csqrt_zmm16r4_a(        const float * __restrict __attribute__((aligned(64)))  xre,
                                       const float * __restrict __attribute__((aligned(64)))  xim,
                                       float * __restrict __attribute__((aligned(64)))  wrkc,
                                       float * __restrict __attribute__((aligned(64)))  csqr,
                                       float * __restrict __attribute__((aligned(64)))  csqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


          void csqrt_zmm16r4(         const __m512 xre,
                                      const __m512 xim,
                                      __m512 * __restrict wrkc,
                                      __m512 * __restrict csqr,
                                      __m512 * __restrict csqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


         ///////////////////////////////////////////////////////////////////////////////////////////


          void cnorm_prod_zmm16r4_u(         const float * __restrict xre,
                                             const float * __restrict xim,
                                             const float * __restrict yre,
                                             const float * __restrict yim,
                                             float * __restrict zre,
                                             float * __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


         void cnorm_prod_zmm16r4_a(          const float * __restrict __attribute__((aligned(64)))  xre,
                                             const float * __restrict __attribute__((aligned(64)))  xim,
                                             const float * __restrict __attribute__((aligned(64)))  yre,
                                             const float * __restrict __attribute__((aligned(64)))  yim,
                                             float * __restrict __attribute__((aligned(64)))  zre,
                                             float * __restrict __attribute__((aligned(64)))  zim) 
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


        void cnorm_prod_zmm16r4(             const __m512  xre,
                                             const __m512  xim,
                                             const __m512  yre,
                                             const __m512  yim,
                                             __m512 * __restrict zre,
                                             __m512 * __restrict zim) 
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


        ////////////////////////////////////////////////////////////////////////////////////


         void cmean_prod_zmm16r4_u(          const float * __restrict xre,
                                             const float * __restrict xim,
                                             const float * __restrict yre,
                                             const float * __restrict yim,
                                             float * __restrict mre,
                                             float * __restrict mim)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


         void cmean_prod_zmm16r4_a(          const float * __restrict __attribute__((aligned(64)))  xre,
                                             const float * __restrict __attribute__((aligned(64)))  xim,
                                             const float * __restrict __attribute__((aligned(64)))  yre,
                                             const float * __restrict __attribute__((aligned(64)))  yim,
                                             float * __restrict mre,
                                             float * __restrict mim)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));

         
         void cmean_prod_zmm16r4(          const __m512 xre,
                                           const __m512 xim,
                                           const __m512 yre,
                                           const __m512 yim,
                                           float * __restrict mre,
                                           float * __restrict min)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));

         //////////////////////////////////////////////////////////////////////////////////////


         void cmean_quot_zmm16r4_u(          const float * __restrict xre,
                                             const float * __restrict xim,
                                             const float * __restrict yre,
                                             const float * __restrict yim,
                                             float * __restrict mre,
                                             float * __restrict mim)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


         void cmean_quot_zmm16r4_a(          const float * __restrict __attribute__((aligned(64)))  xre,
                                             const float * __restrict __attribute__((aligned(64)))  xim,
                                             const float * __restrict __attribute__((aligned(64)))  yre,
                                             const float * __restrict __attribute__((aligned(64)))  yim,
                                             float * __restrict mre,
                                             float * __restrict mim) 
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


         void cmean_quot_zmm16r4(            const __m512 xre,
                                             const __m512 xim,
                                             const __m512 yre,
                                             const __m512 yim,
                                             float * __restrict mre,
                                             float * __restrict mim) 
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));

 
         //////////////////////////////////////////////////////////////////////////////////////////////


         void cnorm_cprod_zmm16r4_u(          const float * __restrict xre,
                                              const float * __restrict xim,
                                              const float * __restrict yre,
                                              const float * __restrict yim,
                                              float * __restrict mre,
                                              float * __restrict mim )
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


          void cnorm_cprod_zmm16r4_a(         const float * __restrict __attribute__((aligned(64))) xre,
                                              const float * __restrict __attribute__((aligned(64))) xim,
                                              const float * __restrict __attribute__((aligned(64))) yre,
                                              const float * __restrict __attribute__((aligned(64))) yim,
                                              float * __restrict __attribute__((aligned(64))) mre,
                                              float * __restrict __attribute__((aligned(64))) mim )
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


          void cnorm_cprod_zmm16r4(         const __m512 xre,
                                            const __m512 xim,
                                            const __m512 yre,
                                            const __m512 yim,
                                            __m512 * __restrict mre,
                                            __m512 * __restrict mim)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


          /////////////////////////////////////////////////////////////////////////////////////////////


          void cmean_cprod_zmm16r4_u(         const float * __restrict xre,
                                              const float * __restrict xim,
                                              const float * __restrict yre,
                                              const float * __restrict yim,
                                              float * __restrict mre,
                                              float * __restrict mim)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


           void cmean_cprod_zmm16r4_a(        const float * __restrict __attribute__((aligned(64)))  xre,
                                              const float * __restrict __attribute__((aligned(64)))  xim,
                                              const float * __restrict __attribute__((aligned(64)))  yre,
                                              const float * __restrict __attribute__((aligned(64)))  yim,
                                              float * __restrict mre,
                                              float * __restrict mim)  
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


            void cmean_cprod_zmm16r4(       const __m512 xre,
                                            const __m512 xim,
                                            const __m512 yre,
                                            const __m512 yim,
                                            float * __restrict mre,
                                            float * __restrict min)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 














#endif /*__GMS_COMPLEX_ZMM16R4_H__*/
