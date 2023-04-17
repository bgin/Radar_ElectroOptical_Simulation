

#ifndef __GMS_RCS_ELLIPS_OGIVE_ZMM16R4_H__
#define __GMS_RCS_ELLIPS_OGIVE_ZMM16R4_H__ 010320230919


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



    const unsigned int GMS_RCS_ELLIPS_OGIVE_ZMM16R4_MAJOR = 1U;
    const unsigned int GMS_RCS_ELLIPS_OGIVE_ZMM16R4_MINOR = 0U;
    const unsigned int GMS_RCS_ELLIPS_OGIVE_ZMM16R4_MICRO = 0U;
    const unsigned int GMS_RCS_ELLIPS_OGIVE_ZMM16R4_FULLVER =
      1000U*GMS_RCS_ELLIPS_OGIVE_ZMM16R4_MAJOR+
      100U*GMS_RCS_ELLIPS_OGIVE_ZMM16R4_MINOR+
      10U*GMS_RCS_ELLIPS_OGIVE_ZMM16R4_MICRO;
    const char * const GMS_RCS_ELLIPS_OGIVE_ZMM16R4_CREATION_DATE = "01-03-2023 09:18 AM +00200 (WED 01 MAR 2023 GMT+2)";
    const char * const GMS_RCS_ELLIPS_OGIVE_ZMM16R4_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_RCS_ELLIPS_OGIVE_ZMM16R4_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_RCS_ELLIPS_OGIVE_ZMM16R4_DESCRIPTION   = "AVX512 optimized Ellipsoid and Ogive Radar Cross Section (analytic) functionality.";



#include <stdint.h>
#include <immintrin.h>
#include "GMS_kernel_config.h"





                      /*
                            High-frequency cross-section of perfectly
                            conducting ellipsoid.
                            Bistatic case.
                            Formula 5.1.54
                        */


                 
                   __m512 rcs_f5154_zmm16r4(const __m512 a,
                                            const __m512 b,
                                            const __m512 c,
                                            const __m512 th1,
                                            const __m512 phi1,
                                            const __m512 th2,
                                            const __m512 phi2) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f5154_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pb,
                                            const float * __restrict __ATTR_ALIGN__(64) pc,
                                            const float * __restrict __ATTR_ALIGN__(64) pth1,
                                            const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                            const float * __restrict __ATTR_ALIGN__(64) pth2,
                                            const float * __restrict __ATTR_ALIGN__(64) pphi2) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f5154_zmm16r4_u(const float * __restrict  pa,
                                            const float * __restrict  pb,
                                            const float * __restrict  pc,
                                            const float * __restrict  pth1,
                                            const float * __restrict  pphi1,
                                            const float * __restrict  pth2,
                                            const float * __restrict  pphi2) FUNC_ATTRIBUTES;


                       /*
                            High-frequency cross-section of perfectly
                            conducting ellipsoid.
                            Backscattering case.
                            Formula 5.1.55
                        */ 


                   __m512 rcs_f5155_zmm16r4(const __m512 a,
                                            const __m512 b,
                                            const __m512 c,
                                            const __m512 th,
                                            const __m512 phi) FUNC_ATTRIBUTES;


               
                   __m512 rcs_f5155_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pc,
                                              const float * __restrict __ATTR_ALIGN__(64) pth,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


              
                   __m512 rcs_f5155_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict  pc,
                                              const float * __restrict  pth,
                                              const float * __restrict  pphi) FUNC_ATTRIBUTES;


                    /*
                           High frequency solutions.
                           Perfectly conducting spheroids.
                           Bistatic case.
                           Formula 5.1-67
                        */

               
                   __m512 rcs_f5167_zmm16r4(const __m512 a,
                                            const __m512 b,
                                            const __m512 th1,
                                            const __m512 phi1,
                                            const __m512 th2,
                                            const __m512 phi2) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f5167_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pth1,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const float * __restrict __ATTR_ALIGN__(64) pth2,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi2) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f5167_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict  pth1,
                                              const float * __restrict  pphi1,
                                              const float * __restrict  pth2,
                                              const float * __restrict  pphi2) FUNC_ATTRIBUTES;


                  /*
                           High frequency solutions.
                           Perfectly conducting spheroids.
                           Backscatter case.
                           Formula 5.1-68
                     */


               
                   __m512 rcs_f5168_zmm16r4(const __m512 a,
                                            const __m512 b,
                                            const __m512 phi) FUNC_ATTRIBUTES;

               
                   __m512 rcs_f5168_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pb,
                                            const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;

               
                   __m512 rcs_f5168_zmm16r4_u(const float * __restrict  pa,
                                            const float * __restrict  pb,
                                            const float * __restrict  pphi) FUNC_ATTRIBUTES;


                    /*
                           High frequency solutions.
                           Perfectly conducting spheroids.
                           Incidence along a the major axis -- the backscatter RCS.
                           Formula 5.1-69
                     */


                
                   __m512 rcs_f5169_zmm16r4(const __m512 a,
                                            const __m512 b,
                                            const __m512 k0) FUNC_ATTRIBUTES;


              
                   __m512 rcs_f5169_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pb,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


                  
                   __m512 rcs_f5169_zmm16r4_u(const float * __restrict  pa,
                                            const float * __restrict pb,
                                            const float * __restrict  pk0) FUNC_ATTRIBUTES;


                  /*
                           High frequency solutions.
                           Perfectly conducting spheroids.
                           Incidence along a the minor axis -- the backscatter RCS.
                           Formula 5.1-70

                    */


               
                   __m512 rcs_f5170_zmm16r4(const __m512 a,
                                            const __m512 b,
                                            const __m512 k0) FUNC_ATTRIBUTES;


                   __m512 rcs_f5170_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pb,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f5170_zmm16r4_u(const float * __restrict  pa,
                                            const float * __restrict  pb,
                                            const float * __restrict  pk0) FUNC_ATTRIBUTES;


                   /*
                         Oblate spheroids.
                         Low frequency solutions.
                         Helper parameters: Ia,Ib
                         Formula 5.1-91
                     */


                
                   __m512 IaIb_f5191_zmm16r4(const __m512 a,
                                             const __m512 c) FUNC_ATTRIBUTES;


                
                   __m512 IaIb_f5191_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                               const float * __restrict __ATTR_ALIGN__(64) pc) FUNC_ATTRIBUTES;


                
                   __m512 IaIb_f5191_zmm16r4_u(const float * __restrict  pa,
                                               const float * __restrict  pc) FUNC_ATTRIBUTES;

                     /*
                         Oblate spheroids.
                         Low frequency solutions.
                         Helper parameters: Ia,Ib
                         Formula 5.1-92
                     */


                 
                   __m512 Ic_f5192_zmm16r4(const __m512 a,
                                             const __m512 c) FUNC_ATTRIBUTES;


                
                   __m512 Ic_f5192_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) pc) FUNC_ATTRIBUTES;


                  
                   __m512 Ic_f5192_zmm16r4_u(const float * __restrict  pa,
                                           const float * __restrict  pc) FUNC_ATTRIBUTES;


                   /*
                         Oblate spheroids (perfectly conducting);
                         Low frequency solutions.
                         Backscatter RCS.
                         Formula 5.1-89
                     */


              
                   __m512 rcs_f5189_zmm16r4(const __m512 a,
                                            const __m512 c,
                                            const __m512 tht,
                                            const __m512 k0) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f5189_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pc,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0 ) FUNC_ATTRIBUTES;


                   __m512 rcs_f5189_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pc,
                                              const float * __restrict  ptht,
                                              const float * __restrict  pk0 ) FUNC_ATTRIBUTES;


                     /*
                         Oblate spheroids (perfectly conducting);
                         Low frequency solutions.
                         Backscatter RCS.
                         Formula 5.1-90
                     */


               
                   __m512 rcs_f5190_zmm16r4(const __m512 a,
                                            const __m512 c,
                                            const __m512 tht,
                                            const __m512 k0) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f5190_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pc,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


                   /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          E-field (theta), perpendicular, formula 5.1-83
                    */

               
                   void ESth_f5183_zmm16r4(const __m512 k0,
                                          const __m512 r,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          const __m512 a,
                                          const __m512 c,
                                          const __m512 tht1,
                                          const __m512 tht2,
                                          const __m512 phi2,
                                          __m512 * __restrict ESr,
                                          __m512 * __restrict ESi) FUNC_ATTRIBUTES;

             
                   void ESth_f5183_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                          const float * __restrict __ATTR_ALIGN__(64) pr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          const float * __restrict __ATTR_ALIGN__(64) pa,
                                          const float * __restrict __ATTR_ALIGN__(64) pc,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                          float * __restrict __ATTR_ALIGN__(64) ESr,
                                          float * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

                
                   void ESth_f5183_zmm16r4_u(const float * __restrict  pk0,
                                          const float * __restrict  pr,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict  pmur,
                                          const float * __restrict  pmui,
                                          const float * __restrict  pa,
                                          const float * __restrict  pc,
                                          const float * __restrict  ptht1,
                                          const float * __restrict  ptht2,
                                          const float * __restrict  pphi2,
                                          float * __restrict  ESr,
                                          float * __restrict  ESi) FUNC_ATTRIBUTES;

                  /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          H-field (phi), perpendicular formula 5.1-83
                    */


                
                   void HSph_f5183_zmm16r4(const __m512 k0,
                                          const __m512 r,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          const __m512 a,
                                          const __m512 c,
                                          const __m512 tht1,
                                          const __m512 tht2,
                                          const __m512 phi2,
                                          __m512 * __restrict HSr,
                                          __m512 * __restrict HSi) FUNC_ATTRIBUTES;

                   void HSph_f5183_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                          const float * __restrict __ATTR_ALIGN__(64) pr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          const float * __restrict __ATTR_ALIGN__(64) pa,
                                          const float * __restrict __ATTR_ALIGN__(64) pc,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                          float * __restrict __ATTR_ALIGN__(64) HSr,
                                          float * __restrict __ATTR_ALIGN__(64) HSi) FUNC_ATTRIBUTES;


             
                   void HSph_f5183_zmm16r4_u(const float * __restrict  pk0,
                                          const float * __restrict  pr,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict pmur,
                                          const float * __restrict  pmui,
                                          const float * __restrict  pa,
                                          const float * __restrict  pc,
                                          const float * __restrict  ptht1,
                                          const float * __restrict  ptht2,
                                          const float * __restrict  pphi2,
                                          float * __restrict  HSr,
                                          float * __restrict  HSi) FUNC_ATTRIBUTES;

                 /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          E-field (phi), perpendicular formula 5.1-84

                    */


                   void ESph_f5184_zmm16r4(const __m512 k0,
                                          const __m512 r,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          const __m512 a,
                                          const __m512 c,
                                          const __m512 tht1,
                                          const __m512 tht2,
                                          const __m512 phi2,
                                          __m512 * __restrict ESr,
                                          __m512 * __restrict ESi) FUNC_ATTRIBUTES;

               
                   void ESph_f5184_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                          const float * __restrict __ATTR_ALIGN__(64) pr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          const float * __restrict __ATTR_ALIGN__(64) pa,
                                          const float * __restrict __ATTR_ALIGN__(64) pc,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                          float * __restrict __ATTR_ALIGN__(64) ESr,
                                          float * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

                
                   void ESph_f5184_zmm16r4_u(const float * __restrict  pk0,
                                          const float * __restrict pr,
                                          const float * __restrict pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict  pmur,
                                          const float * __restrict  pmui,
                                          const float * __restrict  pa,
                                          const float * __restrict  pc,
                                          const float * __restrict ptht1,
                                          const float * __restrict  ptht2,
                                          const float * __restrict  pphi2,
                                          float * __restrict  ESr,
                                          float * __restrict  ESi) FUNC_ATTRIBUTES;

                  /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          H-field (theta), perpendicular formula 5.1-84
                    */


                  
                   void HSth_f5184_zmm16r4(const __m512 k0,
                                          const __m512 r,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          const __m512 a,
                                          const __m512 c,
                                          const __m512 tht1,
                                          const __m512 tht2,
                                          const __m512 phi2,
                                          __m512 * __restrict HSr,
                                          __m512 * __restrict HSi) FUNC_ATTRIBUTES;

                   void HSph_f5184_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                          const float * __restrict __ATTR_ALIGN__(64) pr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          const float * __restrict __ATTR_ALIGN__(64) pa,
                                          const float * __restrict __ATTR_ALIGN__(64) pc,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                          float * __restrict __ATTR_ALIGN__(64) HSr,
                                          float * __restrict __ATTR_ALIGN__(64) HSi) FUNC_ATTRIBUTES;


                
                   void HSph_f5184_zmm16r4_u(const float * __restrict pk0,
                                          const float * __restrict  pr,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict  pmur,
                                          const float * __restrict  pmui,
                                          const float * __restrict  pa,
                                          const float * __restrict  pc,
                                          const float * __restrict  ptht1,
                                          const float * __restrict  ptht2,
                                          const float * __restrict  pphi2,
                                          float * __restrict  HSr,
                                          float * __restrict  HSi) FUNC_ATTRIBUTES;


                     /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          E-field (theta), parallel, formula 5.1-85

                    */


                
                   void ESth_f5185_zmm16r4(const __m512 k0,
                                          const __m512 r,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          const __m512 a,
                                          const __m512 c,
                                          const __m512 tht1,
                                          const __m512 tht2,
                                          const __m512 phi2,
                                          __m512 * __restrict ESr,
                                          __m512 * __restrict ESi) FUNC_ATTRIBUTES;

                 
                   void ESth_f5185_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                          const float * __restrict __ATTR_ALIGN__(64) pr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          const float * __restrict __ATTR_ALIGN__(64) pa,
                                          const float * __restrict __ATTR_ALIGN__(64) pc,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                          float * __restrict __ATTR_ALIGN__(64) ESr,
                                          float * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

             
                   void ESth_f5185_zmm16r4_u(const float * __restrict pk0,
                                          const float * __restrict  pr,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict  pmur,
                                          const float * __restrict  pmui,
                                          const float * __restrict  pa,
                                          const float * __restrict  pc,
                                          const float * __restrict  ptht1,
                                          const float * __restrict  ptht2,
                                          const float * __restrict  pphi2,
                                          float * __restrict  ESr,
                                          float * __restrict  ESi) FUNC_ATTRIBUTES;

                   /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          H-field (phi), parallel, formula 5.1-85

                    */



                
                   void HSph_f5185_zmm16r4(const __m512 k0,
                                          const __m512 r,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          const __m512 a,
                                          const __m512 c,
                                          const __m512 tht1,
                                          const __m512 tht2,
                                          const __m512 phi2,
                                          __m512 * __restrict HSr,
                                          __m512 * __restrict HSi) FUNC_ATTRIBUTES;

                
                   void HSph_f5185_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pr,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                             const float * __restrict __ATTR_ALIGN__(64) pmur,
                                             const float * __restrict __ATTR_ALIGN__(64) pmui,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) pc,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                             const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                             float * __restrict __ATTR_ALIGN__(64) HSr,
                                             float * __restrict __ATTR_ALIGN__(64) HSi) FUNC_ATTRIBUTES;


               
                   void HSph_f5185_zmm16r4_u(const float * __restrict pk0,
                                             const float * __restrict  pr,
                                             const float * __restrict  pepsr,
                                             const float * __restrict  pepsi,
                                             const float * __restrict  pmur,
                                             const float * __restrict  pmui,
                                             const float * __restrict  pa,
                                             const float * __restrict  pc,
                                             const float * __restrict  ptht1,
                                             const float * __restrict  ptht2,
                                             const float * __restrict  pphi2,
                                             float * __restrict  HSr,
                                             float * __restrict  HSi) FUNC_ATTRIBUTES;


                  /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          E-field (phi), parallel, formula 5.1-86


                     */


                   void ESph_f5186_zmm16r4(const __m512 k0,
                                          const __m512 r,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          const __m512 a,
                                          const __m512 c,
                                          const __m512 tht1,
                                          const __m512 tht2,
                                          const __m512 phi2,
                                          __m512 * __restrict ESr,
                                          __m512 * __restrict ESi) FUNC_ATTRIBUTES;

             
                   void ESph_f5186_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                          const float * __restrict __ATTR_ALIGN__(64) pr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          const float * __restrict __ATTR_ALIGN__(64) pa,
                                          const float * __restrict __ATTR_ALIGN__(64) pc,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                          float * __restrict __ATTR_ALIGN__(64) ESr,
                                          float * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

             
                   void ESph_f5186_zmm16r4_u(const float * __restrict  pk0,
                                          const float * __restrict  pr,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict  pmur,
                                          const float * __restrict  pmui,
                                          const float * __restrict pa,
                                          const float * __restrict  pc,
                                          const float * __restrict  ptht1,
                                          const float * __restrict  ptht2,
                                          const float * __restrict  pphi2,
                                          float * __restrict  ESr,
                                          float * __restrict _ ESi) FUNC_ATTRIBUTES;

                      /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          H-field (theta), parallel, formula 5.1-86


                     */


               
                   void HSth_f5186_zmm16r4(const __m512 k0,
                                          const __m512 r,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          const __m512 a,
                                          const __m512 c,
                                          const __m512 tht1,
                                          const __m512 tht2,
                                          const __m512 phi2,
                                          __m512 * __restrict HSr,
                                          __m512 * __restrict HSi) FUNC_ATTRIBUTES;


               
                   void HSth_f5186_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                          const float * __restrict __ATTR_ALIGN__(64) pr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          const float * __restrict __ATTR_ALIGN__(64) pa,
                                          const float * __restrict __ATTR_ALIGN__(64) pc,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                          float * __restrict __ATTR_ALIGN__(64) HSr,
                                          float * __restrict __ATTR_ALIGN__(64) HSi) FUNC_ATTRIBUTES;


              
                   void HSth_f5186_zmm16r4_u(const float * __restrict pk0,
                                          const float * __restrict  pr,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict  pmur,
                                          const float * __restrict  pmui,
                                          const float * __restrict  pa,
                                          const float * __restrict  pc,
                                          const float * __restrict  ptht1,
                                          const float * __restrict  ptht2,
                                          const float * __restrict  pphi2,
                                          float * __restrict  HSr,
                                          float * __restrict  HSi) FUNC_ATTRIBUTES;


                    /*
                          Low-frequency oblate spheroid Rayleigh
                          Backscattered fields.
                          E-field (phi), perpendicular, formula 5.1-87         
 
                     */


               
                   void ESph_f5187_zmm16r4(const __m512 k0,
                                          const __m512 r,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          const __m512 a,
                                          const __m512 c,
                                          const __m512 tht,
                                          __m512 * __restrict ESr,
                                          __m512 * __restrict ESi) FUNC_ATTRIBUTES;

               
                   void ESph_f5187_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                          const float * __restrict __ATTR_ALIGN__(64) pr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          const float * __restrict __ATTR_ALIGN__(64) pa,
                                          const float * __restrict __ATTR_ALIGN__(64) pc,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht,
                                          float * __restrict __ATTR_ALIGN__(64) ESr,
                                          float * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

              
                
                   void ESph_f5187_zmm16r4_u(const float * __restrict  pk0,
                                          const float * __restrict  pr,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict pmur,
                                          const float * __restrict  pmui,
                                          const float * __restrict  pa,
                                          const float * __restrict  pc,
                                          const float * __restrict  ptht,
                                          float * __restrict  ESr,
                                          float * __restrict  ESi) FUNC_ATTRIBUTES;


                   /*
                          Low-frequency oblate spheroid Rayleigh
                          Backscattered fields.
                          E-field (theta), parallel, formula 5.1-88         
 
                     */


                
                   void ESth_f5188_zmm16r4(const __m512 k0,
                                          const __m512 r,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          const __m512 a,
                                          const __m512 c,
                                          const __m512 tht,
                                          __m512 * __restrict ESr,
                                          __m512 * __restrict ESi) FUNC_ATTRIBUTES;

                
                   void ESth_f5188_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                          const float * __restrict __ATTR_ALIGN__(64) pr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          const float * __restrict __ATTR_ALIGN__(64) pa,
                                          const float * __restrict __ATTR_ALIGN__(64) pc,
                                          const float * __restrict __ATTR_ALIGN__(64) ptht,
                                          float * __restrict __ATTR_ALIGN__(64) ESr,
                                          float * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

                  
                   void ESth_f5188_zmm16r4_u(const float * __restrict  pk0,
                                          const float * __restrict  pr,
                                          const float * __restrict  pepsr,
                                          const float * __restrict pepsi,
                                          const float * __restrict  pmur,
                                          const float * __restrict pmui,
                                          const float * __restrict  pa,
                                          const float * __restrict  pc,
                                          const float * __restrict  ptht,
                                          float * __restrict  ESr,
                                          float * __restrict  ESi) FUNC_ATTRIBUTES;

                    /*
                          High-frequency solutions.
                          Bistatic case RCS of oblate spheroid.
                          Formula 5.1-93
                      */


               
                   __m512 rcs_f5193_zmm16r4(const __m512 a,
                                            const __m512 c,
                                            const __m512 tht1,
                                            const __m512 tht2,
                                            const __m512 phi2) FUNC_ATTRIBUTES;


               
                   __m512 rcs_f5193_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) pc,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                             const float * __restrict __ATTR_ALIGN__(64) pphi2) FUNC_ATTRIBUTES;


              
                   __m512 rcs_f5193_zmm16r4_u( const float * __restrict  pa,
                                             const float * __restrict  pc,
                                             const float * __restrict  ptht1,
                                             const float * __restrict  ptht2,
                                             const float * __restrict  pphi2) FUNC_ATTRIBUTES;


                   /*
                          High-frequency solutions.
                          Backscatter case RCS of oblate spheroid.
                          Formula 5.1-94
                      */


                
                   __m512 rcs_f5194_zmm16r4(const __m512 a,
                                            const __m512 c,
                                            const __m512 tht) FUNC_ATTRIBUTES;

                
                   __m512 rcs_f5194_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) pc,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f5194_zmm16r4_u(const float * __restrict  pa,
                                             const float * __restrict  pc,
                                             const float * __restrict  ptht) FUNC_ATTRIBUTES;


                    /*
                           Prolate spheroid.
                           Axial incidence "Physical Optics" backscatter RCS.
                           Formula 5.1-95
                      */

 
                   __m512 rcs_f5195_zmm16r4(const __m512 a,
                                            const __m512 c,
                                            const __m512 k0) FUNC_ATTRIBUTES;


              
                   __m512 rcs_f5195_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pc,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f5195_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pc,
                                              const float * __restrict  pk0) FUNC_ATTRIBUTES;


                  /*
                           Prolate spheroid.
                           Incidence perpendicular to the axis. 
                           "Physical Optics" backscatter RCS.
                           Formula 5.1-96
                      */


                 
                   __m512 rcs_f5196_zmm16r4(const __m512 a,
                                            const __m512 c,
                                            const __m512 k0) FUNC_ATTRIBUTES;

                  
                   __m512 rcs_f5196_zmm16r4_a(  const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pc,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f5196_zmm16r4_u(  const float * __restrict  pa,
                                              const float * __restrict  pc,
                                              const float * __restrict  pk0) FUNC_ATTRIBUTES;


                   /*
                        Perfectly conducting ogive.
                        Backscatter RCS, axial incidence.
                        Parabolic ogive.
                        Formula 5.2-1
                    */


                 
	        
                   __m512 rcs_f521_zmm16r4(const __m512 a,
                                           const __m512 b,
                                           const __m512 k0) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f521_zmm16r4_a(   const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


               
                   __m512 rcs_f521_zmm16r4_u(   const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict  pk0) FUNC_ATTRIBUTES;


                  /*
                        Perfectly conducting ogive.
                        Backscatter RCS, axial incidence.
                        Circular ogive.
                        Formula 5.2-4
                    */


                
                   __m512 rcs_f524_zmm16r4(const __m512 alp,
                                           const __m512 r0,
                                           const __m512 k0) FUNC_ATTRIBUTES;


               
                   __m512 rcs_f524_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) palp,
                                              const float * __restrict __ATTR_ALIGN__(64) pr0,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


                   __m512 rcs_f524_zmm16r4_u( const float * __restrict  palp,
                                              const float * __restrict  pr0,
                                              const float * __restrict  pk0) FUNC_ATTRIBUTES;

                 /*
                        Circular ogive.
                        Axial incidence -- RCS.
                        Formula 5.2-9
                   */


                  
                   __m512 rcs_f529_zmm16r4(const __m512 gam0,
                                           const __m512 alp,
                                           const __m512 k0a) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f529_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const float * __restrict __ATTR_ALIGN__(64) palp,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f529_zmm16r4_u( const float * __restrict pgam0,
                                              const float * __restrict  palp,
                                              const float * __restrict  pk0a) FUNC_ATTRIBUTES;


                   /*
                           Circular ogive.
                           RCS as function of theta angle, i.e. (0<<theta<<(90-alpha))
                           Formula 5.2-6
                     */

                
                   __m512 rcs_f526_zmm16r4(const __m512 gam0,
                                           const __m512 alp,
                                           const __m512 tht) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f526_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const float * __restrict __ATTR_ALIGN__(64) palp,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f526_zmm16r4_u(const float * __restrict  pgam0,
                                              const float * __restrict  palp,
                                              const float * __restrict  ptht) FUNC_ATTRIBUTES;


                    /*
                           Circular ogive.
                           RCS as function of theta angle, i.e. theta = 90-alpha
                           Formula 5.2-7
                     */


                 
                   __m512 rcs_f527_zmm16r4(const __m512 b,
                                           const __m512 alp) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f527_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) palp) FUNC_ATTRIBUTES;


                  
                   __m512 rcs_f527_zmm16r4_u(const float * __restrict  pb,
                                              const float * __restrict  palp) FUNC_ATTRIBUTES;


                   /*
                           Circular ogive.
                           RCS as function of theta angle, i.e. theta = (90-alpha) < theta << 90
                           Formula 5.2-8
                      */


                  
                   __m512 rcs_f528_zmm16r4(const __m512 r0,
                                           const __m512 b,
                                           const __m512 tht) FUNC_ATTRIBUTES;

                    
             
                   __m512 rcs_f528_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pb,
                                           const float * __restrict __ATTR_ALIGN__(64) pr0,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht ) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f528_zmm16r4_u(const float * __restrict  pb,
                                           const float * __restrict  pr0,
                                           const float * __restrict  ptht ) FUNC_ATTRIBUTES;


                  /*
                       Dispatch kernel for Circular ogive RCS.
                       Formulae: 5.2-6, 5.2-7, 5.2-8
                    */


               
                   __m512 rcs_f52678_zmm16r4(const __m512 gam0,
                                             const __m512 alp,
                                             const __m512 tht,
                                             const __m512 b,
                                             const __m512 r0) FUNC_ATTRIBUTES;


                   __m512 rcs_f52678_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                               const float * __restrict __ATTR_ALIGN__(64) palp,
                                               const float * __restrict __ATTR_ALIGN__(64) ptht,
                                               const float * __restrict __ATTR_ALIGN__(64) pb,
                                               const float * __restrict __ATTR_ALIGN__(64) pr0) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f52678_zmm16r4_u(const float * __restrict  pgam0,
                                               const float * __restrict  palp,
                                               const float * __restrict  ptht,
                                               const float * __restrict  pb,
                                               const float * __restrict  pr0) FUNC_ATTRIBUTES;

                  /*
                      Long thin bodies of revolution.
                      Axial-incidence backscatter RCS.
                      Long thin parabolic ogive - MISSILE NOISE CONE.
                      Formula 5.2-10
                  */

                  
                   __m512 rcs_f5210_zmm16r4(const __m512 gam0,
                                            const __m512 alp,
                                            const __m512 k0a) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f5210_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                               const float * __restrict __ATTR_ALIGN__(64) palp,
                                               const float * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f5210_zmm16r4_u( const float * __restrict  pgam0,
                                               const float * __restrict  palp,
                                               const float * __restrict  pk0a,) FUNC_ATTRIBUTES;


                 /*
                      Long thin bodies of revolution.
                      Axial-incidence backscatter RCS.
                      High-frequency limit.
                      Long thin parabolic ogive - MISSILE NOISE CONE.
                      Formula 5.2-12
                  */


                  
                   __m512 rcs_f5212_zmm16r4(const __m512 gam0,
                                            const __m512 alp,
                                            const __m512 k0a) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f5212_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                               const float * __restrict __ATTR_ALIGN__(64) palp,
                                               const float * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f5212_zmm16r4_u(const float * __restrict  pgam0,
                                               const float * __restrict palp,
                                               const float * __restrict  pk0a) FUNC_ATTRIBUTES;


                    /*
                         Circular ogive nose-on RCS (en emprical solution).
                         Formula 5.2-14
                     */


                 
                   __m512 rcs_f5214_zmm16r4(const __m512 gam0,
                                            const __m512 alp,
                                            const __m512 k0a) FUNC_ATTRIBUTES;


                   __m512 rcs_f5214_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const float * __restrict __ATTR_ALIGN__(64) palp,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


               
                   __m512 rcs_f5214_zmm16r4_u(const float * __restrict pgam0,
                                              const float * __restrict  palp,
                                              const float * __restrict  pk0a) FUNC_ATTRIBUTES;

                 



                  
















#endif /*__GMS_RCS_ELLIPS_OGIVE_ZMM16R4_H__*/
