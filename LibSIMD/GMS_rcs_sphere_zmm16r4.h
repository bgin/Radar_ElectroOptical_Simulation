


#ifndef __GMS_RCS_SPHERE_ZMM16R4_H__
#define __GMS_RCS_SPHERE_ZMM16R4_H__ 040120231245



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



    const unsigned int GMS_RCS_SPHERE_ZMM16R4_MAJOR = 1U;
    const unsigned int GMS_RCS_SPHERE_ZMM16R4_MINOR = 0U;
    const unsigned int GMS_RCS_SPHERE_ZMM16R4_MICRO = 0U;
    const unsigned int GMS_RCS_SPHERE_ZMM16R4_FULLVER =
      1000U*GMS_RCS_SPHERE_ZMM16R4_MAJOR+
      100U*GMS_RCS_SPHERE_ZMM16R4_MINOR+
      10U*GMS_RCS_SPHERE_ZMM16R4_MICRO;
    const char * const GMS_RCS_SPHERE_ZMM16R4_CREATION_DATE = "04-01-2023 12:45 AM +00200 (WED 04 JAN 2023 GMT+2)";
    const char * const GMS_RCS_SPHERE_ZMM16R4_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_RCS_SPHERE_ZMM16R4_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_RCS_SPHERE_ZMM16R4_DESCRIPTION   = "AVX512 optimized Sphere Radar Cross Section (analytic) functionality."


#include <immintrin.h>




              /*
                       Radar Cross Section Handbook 1, page 147, formula 3.2-4
                       Backscattering function ,resonance region 0.4 .le. k0a .le. 20.0
                       Theta = 0, far-field
                       Valid for k0a < 1 only!!
                   */


                     void Fth_f324_zmm16r4(   const __m512 k0a, // size of sphere expressed in wavenumber units
                                               __m512 * __restrict F0r, // the results
                                               __m512 * __restrict F0i)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                     void Fth_f324_zmm16r4_a(  const float * __restrict __attribute__((aligned(64))) pk0a, // size of sphere expressed in wavenumber units
                                               __m512 * __restrict F0r, // the results
                                               __m512 * __restrict F0i)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                     void Fth_f324_zmm16r4_u(  const float * __restrict  pk0a, // size of sphere expressed in wavenumber units
                                               __m512 * __restrict F0r, // the results
                                               __m512 * __restrict F0i) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));



        //////////////////////////////////////////////////////////////////////////////////////////////


                      /*
                        Radar Cross Section Handbook 1, page 147, formula 3.2-5
                        Backscattering cross section
                        
                    */


                    __m512 rcs_f325_zmm16r4(const __m512 k0,
                                            const __m512 a ) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                   __m512 rcs_f325_zmm16r4_a(const float * __restrict __attribute__((aligned(64))) pk0,
                                             const float * __restrict __attribute__((aligned(64))) pa )
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                   __m512 rcs_f325_zmm16r4_u(const float * __restrict pk0,
                                             const float * __restrict pa )
                                                          __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                /*
                        Creeping wave term, F c(0) at the upper end of the resonance region.
                        Formula 3.2-8
                    */

          /////////////////////////////////////////////////////////////////////////////////////////////


                   void Fc_f328_zmm16r4( const __m512 x,//k0a
                                         __m512 * __restrict Fc0r,
                                         __m512 * __restrict Fc0i)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                  void Fc_f328_zmm16r4_a( const float * __restrict __attribute__((aligned(64))) px,//k0a
                                           float * __restrict __attribute__((aligned(64))) Fc0r,
                                           float * __restrict __attribute__((aligned(64))) Fc0i) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                   void Fc_f328_zmm16r4_u(const float * __restrict px,//k0a
                                           float * __restrict  Fc0r,
                                           float * __restrict  Fc0i) 
                                                          __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                  /*
                       The complex scattering amplitudes near the lower end of the resonance
                       region (say, 0.4 < k^a < 1) 
                       E-plane approximation
                       These equations are not valid at all for k0a > 1. They are
                       valid for all theta angles.
                   */


             ///////////////////////////////////////////////////////////////////////////////////////


                    void S1_f3213_zmm146r4( const __m512 k0a,
                                            const __m512 tht,
                                           __m512 * __restrict S1r,
                                           __m512 * __restrict S1i)    
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));

                    void S1_f3213_zmm146r4_a( const float * __restrict __attribute__((aligned(64))) pk0a,
                                              const float * __restrict __attribute__((aligned(64))) ptht,
                                              float * __restrict __attribute__((aligned(64))) S1r,
                                              float * __restrict __attribute__((aligned(64))) S1i)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                     void S1_f3213_zmm146r4_u( const float * __restrict  pk0a,
                                               const float * __restrict  ptht,
                                               float * __restrict S1r,
                                               float * __restrict S1i)        
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                       /*
                       The complex scattering amplitudes near the lower end of the resonance
                       region (say, 0.4 < k^a < 1) 
                       H-plane approximation
                       These equations are not valid at all for k0a > 1. They are
                       valid for all theta angles.
                   */


              //////////////////////////////////////////////////////////////////////////////////////////


                     void S2_f3214_zmm146r4( const __m512 k0a,
                                             const __m512 tht,
                                             __m512 * __restrict S2r,
                                             __m512 * __restrict S2i)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                     void S2_f3214_zmm146r4_a( const float * __restrict __attribute__((aligned(64))) pk0a,
                                               const float * __restrict __attribute__((aligned(64))) ptht,
                                               float * __restrict __attribute__((aligned(64))) S2r,
                                               float * __restrict __attribute__((aligned(64))) S2i)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                     void S2_f3214_zmm146r4_u( const float * __restrict  pk0a,
                                               const float * __restrict  ptht,
                                               float * __restrict  S2r,
                                               float * __restrict  S2i) 
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                 /*
                       Formula 3.2-16, optics contribution at upper end of resonance region
                   */


            //////////////////////////////////////////////////////////////////////////////////////////////


                    void S1_f3216_zmm16r4(const __m512 k0a,
                                          const __m512 tht,
                                          __m512 * __restrict S1r,
                                          __m512 * __restrict S1i)    
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                    void S1_f3216_zmm16r4_a(const float * __restrict __attribute__((aligned(64))) pk0a,
                                            const float * __restrict __attribute__((aligned(64))) ptht,
                                            float * __restrict __attribute__((aligned(64))) S1r,
                                            float * __restrict __attribute__((aligned(64))) S1i)
                                                         __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));


                    void S1_f3216_zmm16r4_u(const float * __restrict  pk0a,
                                            const float * __restrict  ptht,
                                            float * __restrict  S1r,
                                            float * __restrict  S1i)
                                                          __attribute__((vectorcall))
                                                         __attribute__((noinline))
							 __attribute__((hot))
                                                         __attribute__((aligned(32)));



#endif /*__GMS_RCS_SPHERE_ZMM16R4_H__*/
