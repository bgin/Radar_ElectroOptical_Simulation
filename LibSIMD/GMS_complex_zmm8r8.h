

#ifndef __GMS_COMPLEX_ZMM8R8_H__
#define __GMS_COMPLEX_ZMM8R8_H__ 251220220954


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


    const unsigned int GMS_COMPLEX_ZMM8R8_MAJOR = 1U;
    const unsigned int GMS_COMPLEX_ZMM8R8_MINOR = 0U;
    const unsigned int GMS_COMPLEX_ZMM8R8_MICRO = 0U;
    const unsigned int GMS_COMPLEX_ZMM8R8_FULLVER =
      1000U*GMS_COMPLEX_ZMM8R8_MAJOR+
      100U*GMS_COMPLEX_ZMM8R8_MINOR+
      10U*GMS_COMPLEX_ZMM8R8_MICRO;
    const char * const GMS_COMPLEX_ZMM8R8_CREATION_DATE = "01-04-2023 15:30  +00200 (SAT 01 APR 2023 GMT+2)";
    const char * const GMS_COMPLEX_ZMM8R8_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_COMPLEX_ZMM8R8_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_COMPLEX_ZMM8R8_DESCRIPTION   = "AVX512 optimized complex number implementation."



#include <immintrin.h>


                   void cadd_zmm8r8_u(const double * __restrict xre,
                                       const double * __restrict xim,
                                       const double * __restrict yre,
                                       const double * __restrict yim,
                                       double *       __restrict zre,
                                       double *       __restrict zim)  
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                   void cadd_zmm8r8_a(const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       const double * __restrict __attribute__((aligned(64))) yre,
                                       const double * __restrict __attribute__((aligned(64))) yim,
                                       double *       __restrict __attribute__((aligned(64))) zre,
                                       double *       __restrict __attribute__((aligned(64))) zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));   


                  void cadd_zmm8r8( const __m512d xre,
                                     const __m512d xim,
                                     const __m512d yre,
                                     const __m512d yim,
                                     __m512d * __restrict zre,
                                     __m512d * __restrict zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  void cadd_zmm8r8( const __m512d xre,
                                     const __m512d xim,
                                     const __m512d s,
                                     __m512d * __restrict     zre,
                                     __m512d * __restrict     zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                 void cadd_zmm8r8_uip(  const double * __restrict xre,
                                         const double * __restrict xim,
                                         double *       __restrict zre,
                                         double *       __restrict zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                 void cadd_zmm8r8_aip(  const double * __restrict __attribute__((aligned(64))) xre,
                                         const double * __restrict __attribute__((aligned(64))) xim,
                                         double *       __restrict __attribute__((aligned(64))) zre,
                                         double *       __restrict __attribute__((aligned(64))) zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                     ////////////////////////////////////////////////////////////////////


                 void csub_zmm8r8_u( const double * __restrict xre,
                                       const double * __restrict xim,
                                       const double * __restrict yre,
                                       const double * __restrict yim,
                                       double *       __restrict zre,
                                       double *       __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                 void csub_zmm8r8_a(  const double * __restrict __attribute__((aligned(64)))  xre,
                                       const double * __restrict __attribute__((aligned(64)))  xim,
                                       const double * __restrict __attribute__((aligned(64)))  yre,
                                       const double * __restrict __attribute__((aligned(64)))  yim,
                                       double *       __restrict __attribute__((aligned(64)))  zre,
                                       double *       __restrict __attribute__((aligned(64)))  zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  void csub_zmm8r8( const __m512d xre,
                                     const __m512d xim,
                                     const __m512d yre,
                                     const __m512d yim,
                                     __m512d * __restrict     zre,
                                     __m512d * __restrict     zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  void csub_zmm8r8( const __m512d xre,
                                     const __m512d xim,
                                     const __m512d s,
                                     __m512d * __restrict     zre,
                                     __m512d * __restrict     zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  void csub_zmm8r8_uip( const double * __restrict xre,
                                         const double * __restrict xim,
                                         double *       __restrict zre,
                                         double *       __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  void csub_zmm8r8_aip( const double * __restrict __attribute__((aligned(64)))  xre,
                                         const double * __restrict __attribute__((aligned(64)))  xim,
                                         double *       __restrict __attribute__((aligned(64)))  zre,
                                         double *       __restrict __attribute__((aligned(64)))  zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  ////////////////////////////////////////////////////////////////////////


                 void cmul_zmm8r8_u(  const double * __restrict xre,
                                       const double * __restrict xim,
                                       const double * __restrict yre,
                                       const double * __restrict yim,
                                       double *       __restrict zre,
                                       double *       __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                 void cmul_zmm8r8_a(  const double * __restrict __attribute__((aligned(64)))  xre,
                                       const double * __restrict __attribute__((aligned(64)))  xim,
                                       const double * __restrict __attribute__((aligned(64)))  yre,
                                       const double * __restrict __attribute__((aligned(64)))  yim,
                                       double *       __restrict __attribute__((aligned(64)))  zre,
                                       double *       __restrict __attribute__((aligned(64)))  zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                 void cmul_zmm8r8(  const __m512d xre,
                                     const __m512d xim,
                                     const __m512d yre,
                                     const __m512d yim,
                                     __m512d * __restrict     zre,
                                     __m512d * __restrict     zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                 void cmul_zmm8r8(  const __m512d xre,
                                     const __m512d xim,
                                     const __m512d s,
                                     __m512d * __restrict   zre,
                                     __m512d * __restrict   zim)
                                                        __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                 void cmul_zmm8r8_uip(  const double * __restrict xre,
                                         const double * __restrict xim,
                                         double *       __restrict zre,
                                         double *       __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  

 
                 void cmul_zmm8r8_aip(  const double * __restrict __attribute__((aligned(64))) xre,
                                         const double * __restrict __attribute__((aligned(64))) xim,
                                         double *       __restrict __attribute__((aligned(64))) zre,
                                         double *       __restrict __attribute__((aligned(64))) zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  ////////////////////////////////////////////////////////////////////////////


                  void cdiv_zmm8r8_u( const double * __restrict xre,
                                       const double * __restrict xim,
                                       const double * __restrict yre,
                                       const double * __restrict yim,
                                       double *       __restrict zre,
                                       double *       __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


                  void cdiv_zmm8r8_a(const double  * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       const double * __restrict __attribute__((aligned(64))) yre,
                                       const double * __restrict __attribute__((aligned(64))) yim,
                                       double *       __restrict __attribute__((aligned(64))) zre,
                                       double *       __restrict __attribute__((aligned(64))) zim)                  
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                  void cdiv_zmm8r8( const __m512d xre,
                                     const __m512d xim,
                                     const __m512d yre,
                                     const __m512d yim,
                                     __m512d * __restrict zre,
                                     __m512d * __restrict zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                 void cdiv_zmm8r8(  const __m512d xre,
                                     const __m512d xim,
                                     const __m512d s,
                                     __m512d * __restrict zre,
                                     __m512d * __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                void cdiv_zmm8r8_uip(   const double * __restrict xre,
                                         const double * __restrict xim,
                                         double *       __restrict zre,
                                         double *       __restrict zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                void cdiv_zmm8r8_aip(   const double * __restrict __attribute__((aligned(64))) xre,
                                         const double * __restrict __attribute__((aligned(64))) xim,
                                         double *       __restrict __attribute__((aligned(64))) zre,
                                         double *       __restrict __attribute__((aligned(64))) zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

                
                void cdiv_smith_zmm8r8_u(   const double * __restrict xre,
                                             const double * __restrict xim,
                                             const double * __restrict yre,
                                             const double * __restrict yim,
                                             double *       __restrict zre,
                                             double *       __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                void cdiv_smith_zmm8r8_a(   const double * __restrict __attribute__((aligned(64))) xre,
                                             const double * __restrict __attribute__((aligned(64))) xim,
                                             const double * __restrict __attribute__((aligned(64))) yre,
                                             const double * __restrict __attribute__((aligned(64))) yim,
                                             double *       __restrict __attribute__((aligned(64))) zre,
                                             double *       __restrict __attribute__((aligned(64))) zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                void cdiv_smith_zmm8r8(   const __m512d xre,
                                           const __m512d xim,
                                           const __m512d yre,
                                           const __m512d yim,
                                           __m512d * __restrict zre,
                                           __m512d * __restrict zim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


                ///////////////////////////////////////////////////////////////////////////


               void cabs_zmm8r8_u(    const double * __restrict re,
                                       const double * __restrict im,
                                       double * __restrict  cabs)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


               void cabs_zmm8r8_a(    const double * __restrict __attribute__((aligned(64))) re,
                                       const double * __restrict __attribute__((aligned(64))) im,
                                       double * __restrict  __attribute__((aligned(64))) cabs)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


               __m512d cabs_zmm8r8(    const __m512d re,
                                       const __m512d im)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             ////////////////////////////////////////////////////////////////////////////


             void  carg_zmm8r8_u(     const double * __restrict re,
                                       const double * __restrict im,
                                       double * __restrict  carg )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             void carg_zmm8r8_a(      const double * __restrict __attribute__((aligned(64))) re,
                                       const double * __restrict __attribute__((aligned(64))) im,
                                       double * __restrict  __attribute__((aligned(64))) carg)  
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             __m512d carg_zmm8r8(      const __m512d re,
                                       const __m512d im)
                                                        __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

        
            //////////////////////////////////////////////////////////////////////////////


             void cconj_zmm8r8_u(      const double * __restrict im,
                                        double * __restrict  conj)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             void cconj_zmm8r8_a(      const double * __restrict __attribute__((aligned(64))) im,
                                        double * __restrict  __attribute__((aligned(64))) conj)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


              __m512d cconj_zmm8r8(const __m512d im) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


           /////////////////////////////////////////////////////////////////////////////////


            void ccos_zmm8r8_u(       const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict  csre,
                                       double * __restrict  csim)  
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


            void ccos_zmm8r8_a(       const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict  __attribute__((aligned(64))) csre,
                                       double * __restrict  __attribute__((aligned(64))) csim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


            void ccos_zmm8r8(       const __m512d xre,
                                     const __m512d xim,
                                     __m512d * __restrict csre,
                                     __m512d * __restrict csim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


            //////////////////////////////////////////////////////////////////////////////


             void csin_zmm8r8_u(      const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict  csre,
                                       double * __restrict  csim)  
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


             void csin_zmm8r8_a(      const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict  __attribute__((aligned(64))) csre,
                                       double * __restrict  __attribute__((aligned(64))) csim) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


             void csin_zmm8r8(      const __m512d xre,
                                     const __m512d xim,
                                     __m512d * __restrict csre,
                                     __m512d * __restrict csim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


           //////////////////////////////////////////////////////////////////////////////


            void csinh_zmm8r8_u(const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict  csre,
                                       double * __restrict  csim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


             void csinh_zmm8r8_a(const double * __restrict  __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict  __attribute__((aligned(64))) csre,
                                       double * __restrict  __attribute__((aligned(64))) csim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             void csinh_zmm8r8(     const __m512d xre,
                                     const __m512d xim,
                                     __m512d * __restrict csre,
                                     __m512d * __restrict csim) 
                                                          __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 
           


           ////////////////////////////////////////////////////////////////////////////////


            void ccosh_zmm8r8_u(      const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict  csre,
                                       double * __restrict  csim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


             void ccosh_zmm8r8_a(     const double * __restrict  __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict  __attribute__((aligned(64))) csre,
                                       double * __restrict  __attribute__((aligned(64))) csim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             void ccosh_zmm8r8(     const __m512d xre,
                                     const __m512d xim,
                                     __m512d * __restrict csre,
                                     __m512d * __restrict csim) 
                                                          __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             /////////////////////////////////////////////////////////////////////////////


            
             void cpow_zmm8r8_u(   const double * __restrict xre,
                                    const double * __restrict xim,
                                    const double n,
                                    double * __restrict powr,
                                    double * __restrict powi) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


             void cpow_zmm8r8_a(   const double * __restrict __attribute__((aligned(64))) xre,
                                    const double * __restrict __attribute__((aligned(64))) xim,
                                    const double n,
                                    double * __restrict __attribute__((aligned(64))) powr,
                                    double * __restrict __attribute__((aligned(64))) powi) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


             void cpow_zmm8r8(const __m512d xre,
                               const __m512d xim,
                               const double n,
                               __m512d * __restrict powr,
                               __m512d * __restrict powi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  

             ////////////////////////////////////////////////////////////////////////////


              void clog_zmm8r8_u(  const double * __restrict xre,
                                    const double * __restrict xim,
                                    double * __restrict logr,
                                    double * __restrict logi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             void clog_zmm8r8_a(   const double * __restrict __attribute__((aligned(64))) xre,
                                    const double * __restrict __attribute__((aligned(64)))xim,
                                    double * __restrict __attribute__((aligned(64))) logr,
                                    double * __restrict __attribute__((aligned(64))) logi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             void clog_zmm8r8(    const __m512d xre,
                                   const __m512d xim,
                                   __m512d * __restrict logr,
                                   __m512d * __restrict logi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            //////////////////////////////////////////////////////////////////////////////


              void ctan_zmm8r8_u( const double * __restrict xre,
                                   const double * __restrict xim,
                                   double * __restrict tanr,
                                   double * __restrict tani) 
                                                        __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


              void ctan_zmm8r8_a( const double * __restrict __attribute__((aligned(64))) xre,
                                   const double * __restrict __attribute__((aligned(64))) xim,
                                   double * __restrict tanr,
                                   double * __restrict tani) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));   


              void ctan_zmm8r8(   const __m512d xre,
                                   const __m512d xim,
                                   __m512d * __restrict tanr,
                                   __m512d * __restrict tani)  
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));   



            ////////////////////////////////////////////////////////////////////////////


              void ctanh_zmm8r8_u( const double * __restrict xre,
                                    const double * __restrict xim,
                                    double * __restrict tanr,
                                    double * __restrict tani) 
                                                        __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


              void ctanh_zmm8r8_a( const double * __restrict __attribute__((aligned(64))) xre,
                                    const double * __restrict __attribute__((aligned(64))) xim,
                                    double * __restrict tanr,
                                    double * __restrict tani) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));   


              void ctanh_zmm8r8(   const __m512d xre,
                                    const __m512d xim,
                                    __m512d * __restrict tanr,
                                    __m512d * __restrict tani)  
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));    


           /////////////////////////////////////////////////////////////////////////////



             void ceq_zmm8r8_u(      const double * __restrict xre,
                                      const double * __restrict xim,
                                      const double * __restrict yre,
                                      const double * __restrict yim,
                                      __mmask16 * __restrict eqr,
                                      __mmask16 * __restrict eqi )
                                                          __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            void ceq_zmm8r8_a(       const double * __restrict __attribute__((aligned(64))) xre,
                                      const double * __restrict __attribute__((aligned(64))) xim,
                                      const double * __restrict __attribute__((aligned(64))) yre,
                                      const double * __restrict __attribute__((aligned(64))) yim,
                                      __mmask16 * __restrict  eqr,
                                      __mmask16 * __restrict  eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

             void ceq_zmm8r8(      const __m512d xre,
                                    const __m512d xim,
                                    const __m512d yre,
                                    const __m512d yim,
                                    __mmask16 * __restrict eqr,
                                    __mmask16 * __restrict eqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            //////////////////////////////////////////////////////////////////////////////////


            void cgt_zmm8r8_u(       const double * __restrict xre,
                                      const double * __restrict xim,
                                      const double * __restrict yre,
                                      const double * __restrict yim,
                                      __mmask16 * __restrict eqr,
                                      __mmask16 * __restrict eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            void cgt_zmm8r8_a(       const double * __restrict __attribute__((aligned(64))) xre,
                                      const double * __restrict __attribute__((aligned(64))) xim,
                                      const double * __restrict __attribute__((aligned(64))) yre,
                                      const double * __restrict __attribute__((aligned(64))) yim,
                                      __mmask16 * __restrict  eqr,
                                      __mmask16 * __restrict  eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

             void cgt_zmm8r8(      const __m512d xre,
                                    const __m512d xim,
                                    const __m512d yre,
                                    const __m512d yim,
                                    __mmask16 * __restrict eqr,
                                    __mmask16 * __restrict eqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

            ///////////////////////////////////////////////////////////////////////////////////////


            void clt_zmm8r8_u(       const double * __restrict xre,
                                      const double * __restrict xim,
                                      const double * __restrict yre,
                                      const double * __restrict yim,
                                      __mmask16 * __restrict eqr,
                                      __mmask16 * __restrict eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            void clt_zmm8r8_a(       const double * __restrict __attribute__((aligned(64))) xre,
                                      const double * __restrict __attribute__((aligned(64))) xim,
                                      const double * __restrict __attribute__((aligned(64))) yre,
                                      const double * __restrict __attribute__((aligned(64))) yim,
                                      __mmask16 * __restrict  eqr,
                                      __mmask16 * __restrict  eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

             void clt_zmm8r8(      const __m512d xre,
                                    const __m512d xim,
                                    const __m512d yre,
                                    const __m512d yim,
                                    __mmask16 * __restrict eqr,
                                    __mmask16 * __restrict eqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


             //////////////////////////////////////////////////////////////////////////////////////////


              void cneq_zmm8r8_u(       const double * __restrict xre,
                                      const double * __restrict xim,
                                      const double * __restrict yre,
                                      const double * __restrict yim,
                                      __mmask16 * __restrict eqr,
                                      __mmask16 * __restrict eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            void cneq_zmm8r8_a(       const double * __restrict __attribute__((aligned(64))) xre,
                                      const double * __restrict __attribute__((aligned(64))) xim,
                                      const double * __restrict __attribute__((aligned(64))) yre,
                                      const double * __restrict __attribute__((aligned(64))) yim,
                                      __mmask16 * __restrict  eqr,
                                      __mmask16 * __restrict  eqi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 

             void cneq_zmm8r8(      const __m512d xre,
                                    const __m512d xim,
                                    const __m512d yre,
                                    const __m512d yim,
                                    __mmask16 * __restrict eqr,
                                    __mmask16 * __restrict eqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            //////////////////////////////////////////////////////////////////////////////////////////


            void cexp_zmm8r8_u(       const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict cexpr,
                                       double * __restrict cexpi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            void cexp_zmm8r8_a(       const double * __restrict __attribute__((aligned(64))) xre,
                                       const double * __restrict __attribute__((aligned(64))) xim,
                                       double * __restrict __attribute__((aligned(64))) cexpr,
                                       double * __restrict __attribute__((aligned(64))) cexpi )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32))); 


            void cexp_zmm8r8(       const __m512d xre,
                                     const __m512d xim,
                                     __m512d * __restrict cexpr,
                                     __m512d * __restrict cexpi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));    

           
            //////////////////////////////////////////////////////////////////////////////////////


            void cpolar_zmm8r8_u(       const double * __restrict rho,
                                         const double * __restrict tht,
                                         double * __restrict  re,
                                         double * __restrict  im)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


           void cpolar_zmm8r8_a(        const double * __restrict __attribute__((aligned(64))) rho,
                                         const double * __restrict __attribute__((aligned(64))) tht,
                                         double * __restrict  __attribute__((aligned(64))) re,
                                         double * __restrict  __attribute__((aligned(64))) im)   
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));  


           void cpolar_zmm8r8(        const __m512d rho,
                                       const __m512d tht,
                                       __m512d * __restrict re,
                                       __m512d * __restrict im)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


         /////////////////////////////////////////////////////////////////////////////////////////////

 
         void csqrt_zmm8r8_u(         const double * __restrict xre,
                                       const double * __restrict xim,
                                       double * __restrict wrkc,
                                       double * __restrict csqr,
                                       double * __restrict csqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));

          
          void csqrt_zmm8r8_a(        const double * __restrict __attribute__((aligned(64)))  xre,
                                       const double * __restrict __attribute__((aligned(64)))  xim,
                                       double * __restrict __attribute__((aligned(64)))  wrkc,
                                       double * __restrict __attribute__((aligned(64)))  csqr,
                                       double * __restrict __attribute__((aligned(64)))  csqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


          void csqrt_zmm8r8(         const __m512d xre,
                                      const __m512d xim,
                                      __m512d * __restrict wrkc,
                                      __m512d * __restrict csqr,
                                      __m512d * __restrict csqi)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


         ///////////////////////////////////////////////////////////////////////////////////////////


          void cnorm_prod_zmm8r8_u(         const double * __restrict xre,
                                             const double * __restrict xim,
                                             const double * __restrict yre,
                                             const double * __restrict yim,
                                             double * __restrict zre,
                                             double * __restrict zim)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


         void cnorm_prod_zmm8r8_a(          const double * __restrict __attribute__((aligned(64)))  xre,
                                             const double * __restrict __attribute__((aligned(64)))  xim,
                                             const double * __restrict __attribute__((aligned(64)))  yre,
                                             const double * __restrict __attribute__((aligned(64)))  yim,
                                             double * __restrict __attribute__((aligned(64)))  zre,
                                             double * __restrict __attribute__((aligned(64)))  zim) 
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


        void cnorm_prod_zmm8r8(             const __m512d  xre,
                                             const __m512d  xim,
                                             const __m512d  yre,
                                             const __m512d  yim,
                                             __m512d * __restrict zre,
                                             __m512d * __restrict zim) 
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


        ////////////////////////////////////////////////////////////////////////////////////


         void cmean_prod_zmm8r8_u(          const double * __restrict xre,
                                             const double * __restrict xim,
                                             const double * __restrict yre,
                                             const double * __restrict yim,
                                             double * __restrict mre,
                                             double * __restrict mim)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


         void cmean_prod_zmm8r8_a(          const double * __restrict __attribute__((aligned(64)))  xre,
                                             const double * __restrict __attribute__((aligned(64)))  xim,
                                             const double * __restrict __attribute__((aligned(64)))  yre,
                                             const double * __restrict __attribute__((aligned(64)))  yim,
                                             double * __restrict mre,
                                             double * __restrict mim)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));

         
         void cmean_prod_zmm8r8(          const __m512d xre,
                                           const __m512d xim,
                                           const __m512d yre,
                                           const __m512d yim,
                                           double * __restrict mre,
                                           double * __restrict min)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));

         //////////////////////////////////////////////////////////////////////////////////////


         void cmean_quot_zmm8r8_u(          const double * __restrict xre,
                                             const double * __restrict xim,
                                             const double * __restrict yre,
                                             const double * __restrict yim,
                                             double * __restrict mre,
                                             double * __restrict mim)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


         void cmean_quot_zmm8r8_a(          const double * __restrict __attribute__((aligned(64)))  xre,
                                             const double * __restrict __attribute__((aligned(64)))  xim,
                                             const double * __restrict __attribute__((aligned(64)))  yre,
                                             const double * __restrict __attribute__((aligned(64)))  yim,
                                             double * __restrict mre,
                                             double * __restrict mim) 
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


         void cmean_quot_zmm8r8(            const __m512d xre,
                                             const __m512d xim,
                                             const __m512d yre,
                                             const __m512d yim,
                                             double * __restrict mre,
                                             double * __restrict mim) 
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));

 
         //////////////////////////////////////////////////////////////////////////////////////////////


         void cnorm_cprod_zmm8r8_u(          const double * __restrict xre,
                                              const double * __restrict xim,
                                              const double * __restrict yre,
                                              const double * __restrict yim,
                                              double * __restrict mre,
                                              double * __restrict mim )
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


          void cnorm_cprod_zmm8r8_a(         const double * __restrict __attribute__((aligned(64))) xre,
                                              const double * __restrict __attribute__((aligned(64))) xim,
                                              const double * __restrict __attribute__((aligned(64))) yre,
                                              const double * __restrict __attribute__((aligned(64))) yim,
                                              double * __restrict __attribute__((aligned(64))) mre,
                                              double * __restrict __attribute__((aligned(64))) mim )
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


          void cnorm_cprod_zmm8r8(         const __m512d xre,
                                            const __m512d xim,
                                            const __m512d yre,
                                            const __m512d yim,
                                            __m512d * __restrict mre,
                                            __m512d * __restrict mim)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32)));


          /////////////////////////////////////////////////////////////////////////////////////////////


          void cmean_cprod_zmm8r8_u(         const double * __restrict xre,
                                              const double * __restrict xim,
                                              const double * __restrict yre,
                                              const double * __restrict yim,
                                              double * __restrict mre,
                                              double * __restrict mim)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


           void cmean_cprod_zmm8r8_a(        const double * __restrict __attribute__((aligned(64)))  xre,
                                              const double * __restrict __attribute__((aligned(64)))  xim,
                                              const double * __restrict __attribute__((aligned(64)))  yre,
                                              const double * __restrict __attribute__((aligned(64)))  yim,
                                              double * __restrict mre,
                                              double * __restrict mim)  
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


            void cmean_cprod_zmm8r8(       const __m512d xre,
                                            const __m512d xim,
                                            const __m512d yre,
                                            const __m512d yim,
                                            double * __restrict mre,
                                            double * __restrict min)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


           /////////////////////////////////////////////////////////////////////////////


           void arith_cmean_zmm8r8_u(        const double * __restrict xre,
                                              const double * __restrict xim,
                                              double * __restrict mre,
                                              double * __restrict min)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


           void arith_cmean_zmm8r8_a(        const double * __restrict __attribute__((aligned(64))) xre,
                                              const double * __restrict __attribute__((aligned(64))) xim,
                                              double * __restrict mre,
                                              double * __restrict min)
                                                              __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


            void arith_cmean_zmm8r8(        const __m512d xre,
                                              const __m512d xim,
                                              double * __restrict mre,
                                              double * __restrict min)
                                                              __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


           ///////////////////////////////////////////////////////////////////////////////////////


            void cnormalize_zmm8r8_u(        const double * __restrict xre,
                                              const double * __restrict xim,
                                              const double * __restrict yre,
                                              const double * __restrict yim,
                                              double * __restrict mre,
                                              double * __restrict mim )
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


            void cnormalize_zmm8r8_a(        const double * __restrict __attribute__((aligned(64)))  xre,
                                              const double * __restrict __attribute__((aligned(64)))  xim,
                                              const double * __restrict __attribute__((aligned(64)))  yre,
                                              const double * __restrict __attribute__((aligned(64)))  yim,
                                              double * __restrict __attribute__((aligned(64)))  mre,
                                              double * __restrict __attribute__((aligned(64)))  mim )
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


             void cnormalize_zmm8r8(       const __m512d xre,
                                            const __m512d xim,
                                            const __m512d yre,
                                            const __m512d yim,
                                            __m512d * __restrict mre,
                                            __m512d * __restrict mim )
                                                               __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


            //////////////////////////////////////////////////////////////////////////////////


             void cmagnitude_zmm8r8_u(       const double * __restrict xre,
                                              const double * __restrict xim,
                                              const double * __restrict yre,
                                              const double * __restrict yim,
                                              double * __restrict mre)
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


              void cmagnitude_zmm8r8_a(      const double * __restrict __attribute__((aligned(64)))  xre,
                                              const double * __restrict __attribute__((aligned(64)))  xim,
                                              const double * __restrict __attribute__((aligned(64)))  yre,
                                              const double * __restrict __attribute__((aligned(64)))  yim,
                                              double * __restrict __attribute__((aligned(64)))  mre) 
                                                             __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 


              void cmagnitude_zmm8r8(        const __m512d xre,
                                              const __m512d xim,
                                              const __m512d yre,
                                              const __m512d yim,
                                              __m512d * __restrict  mre)
                                                              __attribute__((vectorcall))
                                                             __attribute__((noinline))
							     __attribute__((hot))
                                                             __attribute__((aligned(32))); 




#endif /*__GMS_COMPLEX_ZMM8R8_H__*/
