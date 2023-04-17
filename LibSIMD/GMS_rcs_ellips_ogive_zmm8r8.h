

#ifndef __GMS_RCS_ELLIPS_OGIVE_ZMM8R8_H__
#define __GMS_RCS_ELLIPS_OGIVE_ZMM8R8_H__ 010320230919


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



    const unsigned int GMS_RCS_ELLIPS_OGIVE_ZMM8R8_MAJOR = 1U;
    const unsigned int GMS_RCS_ELLIPS_OGIVE_ZMM8R8_MINOR = 0U;
    const unsigned int GMS_RCS_ELLIPS_OGIVE_ZMM8R8_MICRO = 0U;
    const unsigned int GMS_RCS_ELLIPS_OGIVE_ZMM8R8_FULLVER =
      1000U*GMS_RCS_ELLIPS_OGIVE_ZMM8R8_MAJOR+
      100U*GMS_RCS_ELLIPS_OGIVE_ZMM8R8_MINOR+
      10U*GMS_RCS_ELLIPS_OGIVE_ZMM8R8_MICRO;
    const char * const GMS_RCS_ELLIPS_OGIVE_ZMM8R8_CREATION_DATE = "01-03-2023 09:18 AM +00200 (WED 01 MAR 2023 GMT+2)";
    const char * const GMS_RCS_ELLIPS_OGIVE_ZMM8R8_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_RCS_ELLIPS_OGIVE_ZMM8R8_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_RCS_ELLIPS_OGIVE_ZMM8R8_DESCRIPTION   = "AVX512 optimized Ellipsoid and Ogive Radar Cross Section (analytic) functionality.";



#include <stdint.h>
#include <immintrin.h>
#include "GMS_kernel_config.h"





                      /*
                            High-frequency cross-section of perfectly
                            conducting ellipsoid.
                            Bistatic case.
                            Formula 5.1.54
                        */


                 
                   __m512d rcs_f5154_zmm8r8(const __m512d a,
                                            const __m512d b,
                                            const __m512d c,
                                            const __m512d th1,
                                            const __m512d phi1,
                                            const __m512d th2,
                                            const __m512d phi2) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f5154_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) pb,
                                            const double * __restrict __ATTR_ALIGN__(64) pc,
                                            const double * __restrict __ATTR_ALIGN__(64) pth1,
                                            const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                            const double * __restrict __ATTR_ALIGN__(64) pth2,
                                            const double * __restrict __ATTR_ALIGN__(64) pphi2) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f5154_zmm8r8_u(const double * __restrict  pa,
                                            const double * __restrict  pb,
                                            const double * __restrict  pc,
                                            const double * __restrict  pth1,
                                            const double * __restrict  pphi1,
                                            const double * __restrict  pth2,
                                            const double * __restrict  pphi2) FUNC_ATTRIBUTES;


                       /*
                            High-frequency cross-section of perfectly
                            conducting ellipsoid.
                            Backscattering case.
                            Formula 5.1.55
                        */ 


                   __m512d rcs_f5155_zmm8r8(const __m512d a,
                                            const __m512d b,
                                            const __m512d c,
                                            const __m512d th,
                                            const __m512d phi) FUNC_ATTRIBUTES;


               
                   __m512d rcs_f5155_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pc,
                                              const double * __restrict __ATTR_ALIGN__(64) pth,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


              
                   __m512d rcs_f5155_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pb,
                                              const double * __restrict  pc,
                                              const double * __restrict  pth,
                                              const double * __restrict  pphi) FUNC_ATTRIBUTES;


                    /*
                           High frequency solutions.
                           Perfectly conducting spheroids.
                           Bistatic case.
                           Formula 5.1-67
                        */

               
                   __m512d rcs_f5167_zmm8r8(const __m512d a,
                                            const __m512d b,
                                            const __m512d th1,
                                            const __m512d phi1,
                                            const __m512d th2,
                                            const __m512d phi2) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f5167_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pth1,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const double * __restrict __ATTR_ALIGN__(64) pth2,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi2) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f5167_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pb,
                                              const double * __restrict  pth1,
                                              const double * __restrict  pphi1,
                                              const double * __restrict  pth2,
                                              const double * __restrict  pphi2) FUNC_ATTRIBUTES;


                  /*
                           High frequency solutions.
                           Perfectly conducting spheroids.
                           Backscatter case.
                           Formula 5.1-68
                     */


               
                   __m512d rcs_f5168_zmm8r8(const __m512d a,
                                            const __m512d b,
                                            const __m512d phi) FUNC_ATTRIBUTES;

               
                   __m512d rcs_f5168_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) pb,
                                            const double * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;

               
                   __m512d rcs_f5168_zmm8r8_u(const double * __restrict  pa,
                                            const double * __restrict  pb,
                                            const double * __restrict  pphi) FUNC_ATTRIBUTES;


                    /*
                           High frequency solutions.
                           Perfectly conducting spheroids.
                           Incidence along a the major axis -- the backscatter RCS.
                           Formula 5.1-69
                     */


                
                   __m512d rcs_f5169_zmm8r8(const __m512d a,
                                            const __m512d b,
                                            const __m512d k0) FUNC_ATTRIBUTES;


              
                   __m512d rcs_f5169_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) pb,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


                  
                   __m512d rcs_f5169_zmm8r8_u(const double * __restrict  pa,
                                            const double * __restrict pb,
                                            const double * __restrict  pk0) FUNC_ATTRIBUTES;


                  /*
                           High frequency solutions.
                           Perfectly conducting spheroids.
                           Incidence along a the minor axis -- the backscatter RCS.
                           Formula 5.1-70

                    */


               
                   __m512d rcs_f5170_zmm8r8(const __m512d a,
                                            const __m512d b,
                                            const __m512d k0) FUNC_ATTRIBUTES;


                   __m512d rcs_f5170_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) pb,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f5170_zmm8r8_u(const double * __restrict  pa,
                                            const double * __restrict  pb,
                                            const double * __restrict  pk0) FUNC_ATTRIBUTES;


                   /*
                         Oblate spheroids.
                         Low frequency solutions.
                         Helper parameters: Ia,Ib
                         Formula 5.1-91
                     */


                
                   __m512d IaIb_f5191_zmm8r8(const __m512d a,
                                             const __m512d c) FUNC_ATTRIBUTES;


                
                   __m512d IaIb_f5191_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                               const double * __restrict __ATTR_ALIGN__(64) pc) FUNC_ATTRIBUTES;


                
                   __m512d IaIb_f5191_zmm8r8_u(const double * __restrict  pa,
                                               const double * __restrict  pc) FUNC_ATTRIBUTES;

                     /*
                         Oblate spheroids.
                         Low frequency solutions.
                         Helper parameters: Ia,Ib
                         Formula 5.1-92
                     */


                 
                   __m512d Ic_f5192_zmm8r8(const __m512d a,
                                             const __m512d c) FUNC_ATTRIBUTES;


                
                   __m512d Ic_f5192_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                           const double * __restrict __ATTR_ALIGN__(64) pc) FUNC_ATTRIBUTES;


                  
                   __m512d Ic_f5192_zmm8r8_u(const double * __restrict  pa,
                                           const double * __restrict  pc) FUNC_ATTRIBUTES;


                   /*
                         Oblate spheroids (perfectly conducting);
                         Low frequency solutions.
                         Backscatter RCS.
                         Formula 5.1-89
                     */


              
                   __m512d rcs_f5189_zmm8r8(const __m512d a,
                                            const __m512d c,
                                            const __m512d tht,
                                            const __m512d k0) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f5189_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pc,
                                              const double * __restrict __ATTR_ALIGN__(64) ptht,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0 ) FUNC_ATTRIBUTES;


                   __m512d rcs_f5189_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pc,
                                              const double * __restrict  ptht,
                                              const double * __restrict  pk0 ) FUNC_ATTRIBUTES;


                     /*
                         Oblate spheroids (perfectly conducting);
                         Low frequency solutions.
                         Backscatter RCS.
                         Formula 5.1-90
                     */


               
                   __m512d rcs_f5190_zmm8r8(const __m512d a,
                                            const __m512d c,
                                            const __m512d tht,
                                            const __m512d k0) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f5190_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pc,
                                              const double * __restrict __ATTR_ALIGN__(64) ptht,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


                   /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          E-field (theta), perpendicular, formula 5.1-83
                    */

               
                   void ESth_f5183_zmm8r8(const __m512d k0,
                                          const __m512d r,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          const __m512d a,
                                          const __m512d c,
                                          const __m512d tht1,
                                          const __m512d tht2,
                                          const __m512d phi2,
                                          __m512d * __restrict ESr,
                                          __m512d * __restrict ESi) FUNC_ATTRIBUTES;

             
                   void ESth_f5183_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                          const double * __restrict __ATTR_ALIGN__(64) pr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          const double * __restrict __ATTR_ALIGN__(64) pa,
                                          const double * __restrict __ATTR_ALIGN__(64) pc,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                          double * __restrict __ATTR_ALIGN__(64) ESr,
                                          double * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

                
                   void ESth_f5183_zmm8r8_u(const double * __restrict  pk0,
                                          const double * __restrict  pr,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict  pmur,
                                          const double * __restrict  pmui,
                                          const double * __restrict  pa,
                                          const double * __restrict  pc,
                                          const double * __restrict  ptht1,
                                          const double * __restrict  ptht2,
                                          const double * __restrict  pphi2,
                                          double * __restrict  ESr,
                                          double * __restrict  ESi) FUNC_ATTRIBUTES;

                  /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          H-field (phi), perpendicular formula 5.1-83
                    */


                
                   void HSph_f5183_zmm8r8(const __m512d k0,
                                          const __m512d r,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          const __m512d a,
                                          const __m512d c,
                                          const __m512d tht1,
                                          const __m512d tht2,
                                          const __m512d phi2,
                                          __m512d * __restrict HSr,
                                          __m512d * __restrict HSi) FUNC_ATTRIBUTES;

                   void HSph_f5183_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                          const double * __restrict __ATTR_ALIGN__(64) pr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          const double * __restrict __ATTR_ALIGN__(64) pa,
                                          const double * __restrict __ATTR_ALIGN__(64) pc,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                          double * __restrict __ATTR_ALIGN__(64) HSr,
                                          double * __restrict __ATTR_ALIGN__(64) HSi) FUNC_ATTRIBUTES;


             
                   void HSph_f5183_zmm8r8_u(const double * __restrict  pk0,
                                          const double * __restrict  pr,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict pmur,
                                          const double * __restrict  pmui,
                                          const double * __restrict  pa,
                                          const double * __restrict  pc,
                                          const double * __restrict  ptht1,
                                          const double * __restrict  ptht2,
                                          const double * __restrict  pphi2,
                                          double * __restrict  HSr,
                                          double * __restrict  HSi) FUNC_ATTRIBUTES;

                 /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          E-field (phi), perpendicular formula 5.1-84

                    */


                   void ESph_f5184_zmm8r8(const __m512d k0,
                                          const __m512d r,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          const __m512d a,
                                          const __m512d c,
                                          const __m512d tht1,
                                          const __m512d tht2,
                                          const __m512d phi2,
                                          __m512d * __restrict ESr,
                                          __m512d * __restrict ESi) FUNC_ATTRIBUTES;

               
                   void ESph_f5184_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                          const double * __restrict __ATTR_ALIGN__(64) pr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          const double * __restrict __ATTR_ALIGN__(64) pa,
                                          const double * __restrict __ATTR_ALIGN__(64) pc,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                          double * __restrict __ATTR_ALIGN__(64) ESr,
                                          double * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

                
                   void ESph_f5184_zmm8r8_u(const double * __restrict  pk0,
                                          const double * __restrict pr,
                                          const double * __restrict pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict  pmur,
                                          const double * __restrict  pmui,
                                          const double * __restrict  pa,
                                          const double * __restrict  pc,
                                          const double * __restrict ptht1,
                                          const double * __restrict  ptht2,
                                          const double * __restrict  pphi2,
                                          double * __restrict  ESr,
                                          double * __restrict  ESi) FUNC_ATTRIBUTES;

                  /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          H-field (theta), perpendicular formula 5.1-84
                    */


                  
                   void HSth_f5184_zmm8r8(const __m512d k0,
                                          const __m512d r,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          const __m512d a,
                                          const __m512d c,
                                          const __m512d tht1,
                                          const __m512d tht2,
                                          const __m512d phi2,
                                          __m512d * __restrict HSr,
                                          __m512d * __restrict HSi) FUNC_ATTRIBUTES;

                   void HSph_f5184_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                          const double * __restrict __ATTR_ALIGN__(64) pr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          const double * __restrict __ATTR_ALIGN__(64) pa,
                                          const double * __restrict __ATTR_ALIGN__(64) pc,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                          double * __restrict __ATTR_ALIGN__(64) HSr,
                                          double * __restrict __ATTR_ALIGN__(64) HSi) FUNC_ATTRIBUTES;


                
                   void HSph_f5184_zmm8r8_u(const double * __restrict pk0,
                                          const double * __restrict  pr,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict  pmur,
                                          const double * __restrict  pmui,
                                          const double * __restrict  pa,
                                          const double * __restrict  pc,
                                          const double * __restrict  ptht1,
                                          const double * __restrict  ptht2,
                                          const double * __restrict  pphi2,
                                          double * __restrict  HSr,
                                          double * __restrict  HSi) FUNC_ATTRIBUTES;


                     /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          E-field (theta), parallel, formula 5.1-85

                    */


                
                   void ESth_f5185_zmm8r8(const __m512d k0,
                                          const __m512d r,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          const __m512d a,
                                          const __m512d c,
                                          const __m512d tht1,
                                          const __m512d tht2,
                                          const __m512d phi2,
                                          __m512d * __restrict ESr,
                                          __m512d * __restrict ESi) FUNC_ATTRIBUTES;

                 
                   void ESth_f5185_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                          const double * __restrict __ATTR_ALIGN__(64) pr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          const double * __restrict __ATTR_ALIGN__(64) pa,
                                          const double * __restrict __ATTR_ALIGN__(64) pc,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                          double * __restrict __ATTR_ALIGN__(64) ESr,
                                          double * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

             
                   void ESth_f5185_zmm8r8_u(const double * __restrict pk0,
                                          const double * __restrict  pr,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict  pmur,
                                          const double * __restrict  pmui,
                                          const double * __restrict  pa,
                                          const double * __restrict  pc,
                                          const double * __restrict  ptht1,
                                          const double * __restrict  ptht2,
                                          const double * __restrict  pphi2,
                                          double * __restrict  ESr,
                                          double * __restrict  ESi) FUNC_ATTRIBUTES;

                   /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          H-field (phi), parallel, formula 5.1-85

                    */



                
                   void HSph_f5185_zmm8r8(const __m512d k0,
                                          const __m512d r,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          const __m512d a,
                                          const __m512d c,
                                          const __m512d tht1,
                                          const __m512d tht2,
                                          const __m512d phi2,
                                          __m512d * __restrict HSr,
                                          __m512d * __restrict HSi) FUNC_ATTRIBUTES;

                
                   void HSph_f5185_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                             const double * __restrict __ATTR_ALIGN__(64) pr,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                             const double * __restrict __ATTR_ALIGN__(64) pmur,
                                             const double * __restrict __ATTR_ALIGN__(64) pmui,
                                             const double * __restrict __ATTR_ALIGN__(64) pa,
                                             const double * __restrict __ATTR_ALIGN__(64) pc,
                                             const double * __restrict __ATTR_ALIGN__(64) ptht1,
                                             const double * __restrict __ATTR_ALIGN__(64) ptht2,
                                             const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                             double * __restrict __ATTR_ALIGN__(64) HSr,
                                             double * __restrict __ATTR_ALIGN__(64) HSi) FUNC_ATTRIBUTES;


               
                   void HSph_f5185_zmm8r8_u(const double * __restrict pk0,
                                             const double * __restrict  pr,
                                             const double * __restrict  pepsr,
                                             const double * __restrict  pepsi,
                                             const double * __restrict  pmur,
                                             const double * __restrict  pmui,
                                             const double * __restrict  pa,
                                             const double * __restrict  pc,
                                             const double * __restrict  ptht1,
                                             const double * __restrict  ptht2,
                                             const double * __restrict  pphi2,
                                             double * __restrict  HSr,
                                             double * __restrict  HSi) FUNC_ATTRIBUTES;


                  /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          E-field (phi), parallel, formula 5.1-86


                     */


                   void ESph_f5186_zmm8r8(const __m512d k0,
                                          const __m512d r,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          const __m512d a,
                                          const __m512d c,
                                          const __m512d tht1,
                                          const __m512d tht2,
                                          const __m512d phi2,
                                          __m512d * __restrict ESr,
                                          __m512d * __restrict ESi) FUNC_ATTRIBUTES;

             
                   void ESph_f5186_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                          const double * __restrict __ATTR_ALIGN__(64) pr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          const double * __restrict __ATTR_ALIGN__(64) pa,
                                          const double * __restrict __ATTR_ALIGN__(64) pc,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                          double * __restrict __ATTR_ALIGN__(64) ESr,
                                          double * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

             
                   void ESph_f5186_zmm8r8_u(const double * __restrict  pk0,
                                          const double * __restrict  pr,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict  pmur,
                                          const double * __restrict  pmui,
                                          const double * __restrict pa,
                                          const double * __restrict  pc,
                                          const double * __restrict  ptht1,
                                          const double * __restrict  ptht2,
                                          const double * __restrict  pphi2,
                                          double * __restrict  ESr,
                                          double * __restrict _ ESi) FUNC_ATTRIBUTES;

                      /*
                          Low-frequency oblate spheroid Rayleigh
                          bistatic scattered fields.
                          H-field (theta), parallel, formula 5.1-86


                     */


               
                   void HSth_f5186_zmm8r8(const __m512d k0,
                                          const __m512d r,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          const __m512d a,
                                          const __m512d c,
                                          const __m512d tht1,
                                          const __m512d tht2,
                                          const __m512d phi2,
                                          __m512d * __restrict HSr,
                                          __m512d * __restrict HSi) FUNC_ATTRIBUTES;


               
                   void HSth_f5186_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                          const double * __restrict __ATTR_ALIGN__(64) pr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          const double * __restrict __ATTR_ALIGN__(64) pa,
                                          const double * __restrict __ATTR_ALIGN__(64) pc,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht1,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht2,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                          double * __restrict __ATTR_ALIGN__(64) HSr,
                                          double * __restrict __ATTR_ALIGN__(64) HSi) FUNC_ATTRIBUTES;


              
                   void HSth_f5186_zmm8r8_u(const double * __restrict pk0,
                                          const double * __restrict  pr,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict  pmur,
                                          const double * __restrict  pmui,
                                          const double * __restrict  pa,
                                          const double * __restrict  pc,
                                          const double * __restrict  ptht1,
                                          const double * __restrict  ptht2,
                                          const double * __restrict  pphi2,
                                          double * __restrict  HSr,
                                          double * __restrict  HSi) FUNC_ATTRIBUTES;


                    /*
                          Low-frequency oblate spheroid Rayleigh
                          Backscattered fields.
                          E-field (phi), perpendicular, formula 5.1-87         
 
                     */


               
                   void ESph_f5187_zmm8r8(const __m512d k0,
                                          const __m512d r,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          const __m512d a,
                                          const __m512d c,
                                          const __m512d tht,
                                          __m512d * __restrict ESr,
                                          __m512d * __restrict ESi) FUNC_ATTRIBUTES;

               
                   void ESph_f5187_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                          const double * __restrict __ATTR_ALIGN__(64) pr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          const double * __restrict __ATTR_ALIGN__(64) pa,
                                          const double * __restrict __ATTR_ALIGN__(64) pc,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht,
                                          double * __restrict __ATTR_ALIGN__(64) ESr,
                                          double * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

              
                
                   void ESph_f5187_zmm8r8_u(const double * __restrict  pk0,
                                          const double * __restrict  pr,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict pmur,
                                          const double * __restrict  pmui,
                                          const double * __restrict  pa,
                                          const double * __restrict  pc,
                                          const double * __restrict  ptht,
                                          double * __restrict  ESr,
                                          double * __restrict  ESi) FUNC_ATTRIBUTES;


                   /*
                          Low-frequency oblate spheroid Rayleigh
                          Backscattered fields.
                          E-field (theta), parallel, formula 5.1-88         
 
                     */


                
                   void ESth_f5188_zmm8r8(const __m512d k0,
                                          const __m512d r,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          const __m512d a,
                                          const __m512d c,
                                          const __m512d tht,
                                          __m512d * __restrict ESr,
                                          __m512d * __restrict ESi) FUNC_ATTRIBUTES;

                
                   void ESth_f5188_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                          const double * __restrict __ATTR_ALIGN__(64) pr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          const double * __restrict __ATTR_ALIGN__(64) pa,
                                          const double * __restrict __ATTR_ALIGN__(64) pc,
                                          const double * __restrict __ATTR_ALIGN__(64) ptht,
                                          double * __restrict __ATTR_ALIGN__(64) ESr,
                                          double * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

                  
                   void ESth_f5188_zmm8r8_u(const double * __restrict  pk0,
                                          const double * __restrict  pr,
                                          const double * __restrict  pepsr,
                                          const double * __restrict pepsi,
                                          const double * __restrict  pmur,
                                          const double * __restrict pmui,
                                          const double * __restrict  pa,
                                          const double * __restrict  pc,
                                          const double * __restrict  ptht,
                                          double * __restrict  ESr,
                                          double * __restrict  ESi) FUNC_ATTRIBUTES;

                    /*
                          High-frequency solutions.
                          Bistatic case RCS of oblate spheroid.
                          Formula 5.1-93
                      */


               
                   __m512d rcs_f5193_zmm8r8(const __m512d a,
                                            const __m512d c,
                                            const __m512d tht1,
                                            const __m512d tht2,
                                            const __m512d phi2) FUNC_ATTRIBUTES;


               
                   __m512d rcs_f5193_zmm8r8_a( const double * __restrict __ATTR_ALIGN__(64) pa,
                                             const double * __restrict __ATTR_ALIGN__(64) pc,
                                             const double * __restrict __ATTR_ALIGN__(64) ptht1,
                                             const double * __restrict __ATTR_ALIGN__(64) ptht2,
                                             const double * __restrict __ATTR_ALIGN__(64) pphi2) FUNC_ATTRIBUTES;


              
                   __m512d rcs_f5193_zmm8r8_u( const double * __restrict  pa,
                                             const double * __restrict  pc,
                                             const double * __restrict  ptht1,
                                             const double * __restrict  ptht2,
                                             const double * __restrict  pphi2) FUNC_ATTRIBUTES;


                   /*
                          High-frequency solutions.
                          Backscatter case RCS of oblate spheroid.
                          Formula 5.1-94
                      */


                
                   __m512d rcs_f5194_zmm8r8(const __m512d a,
                                            const __m512d c,
                                            const __m512d tht) FUNC_ATTRIBUTES;

                
                   __m512d rcs_f5194_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                             const double * __restrict __ATTR_ALIGN__(64) pc,
                                             const double * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f5194_zmm8r8_u(const double * __restrict  pa,
                                             const double * __restrict  pc,
                                             const double * __restrict  ptht) FUNC_ATTRIBUTES;


                    /*
                           Prolate spheroid.
                           Axial incidence "Physical Optics" backscatter RCS.
                           Formula 5.1-95
                      */

 
                   __m512d rcs_f5195_zmm8r8(const __m512d a,
                                            const __m512d c,
                                            const __m512d k0) FUNC_ATTRIBUTES;


              
                   __m512d rcs_f5195_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pc,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f5195_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pc,
                                              const double * __restrict  pk0) FUNC_ATTRIBUTES;


                  /*
                           Prolate spheroid.
                           Incidence perpendicular to the axis. 
                           "Physical Optics" backscatter RCS.
                           Formula 5.1-96
                      */


                 
                   __m512d rcs_f5196_zmm8r8(const __m512d a,
                                            const __m512d c,
                                            const __m512d k0) FUNC_ATTRIBUTES;

                  
                   __m512d rcs_f5196_zmm8r8_a(  const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pc,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f5196_zmm8r8_u(  const double * __restrict  pa,
                                              const double * __restrict  pc,
                                              const double * __restrict  pk0) FUNC_ATTRIBUTES;


                   /*
                        Perfectly conducting ogive.
                        Backscatter RCS, axial incidence.
                        Parabolic ogive.
                        Formula 5.2-1
                    */


                 
	        
                   __m512d rcs_f521_zmm8r8(const __m512d a,
                                           const __m512d b,
                                           const __m512d k0) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f521_zmm8r8_a(   const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


               
                   __m512d rcs_f521_zmm8r8_u(   const double * __restrict  pa,
                                              const double * __restrict  pb,
                                              const double * __restrict  pk0) FUNC_ATTRIBUTES;


                  /*
                        Perfectly conducting ogive.
                        Backscatter RCS, axial incidence.
                        Circular ogive.
                        Formula 5.2-4
                    */


                
                   __m512d rcs_f524_zmm8r8(const __m512d alp,
                                           const __m512d r0,
                                           const __m512d k0) FUNC_ATTRIBUTES;


               
                   __m512d rcs_f524_zmm8r8_a( const double * __restrict __ATTR_ALIGN__(64) palp,
                                              const double * __restrict __ATTR_ALIGN__(64) pr0,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0) FUNC_ATTRIBUTES;


                   __m512d rcs_f524_zmm8r8_u( const double * __restrict  palp,
                                              const double * __restrict  pr0,
                                              const double * __restrict  pk0) FUNC_ATTRIBUTES;

                 /*
                        Circular ogive.
                        Axial incidence -- RCS.
                        Formula 5.2-9
                   */


                  
                   __m512d rcs_f529_zmm8r8(const __m512d gam0,
                                           const __m512d alp,
                                           const __m512d k0a) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f529_zmm8r8_a( const double * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const double * __restrict __ATTR_ALIGN__(64) palp,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f529_zmm8r8_u( const double * __restrict pgam0,
                                              const double * __restrict  palp,
                                              const double * __restrict  pk0a) FUNC_ATTRIBUTES;


                   /*
                           Circular ogive.
                           RCS as function of theta angle, i.e. (0<<theta<<(90-alpha))
                           Formula 5.2-6
                     */

                
                   __m512d rcs_f526_zmm8r8(const __m512d gam0,
                                           const __m512d alp,
                                           const __m512d tht) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f526_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const double * __restrict __ATTR_ALIGN__(64) palp,
                                              const double * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f526_zmm8r8_u(const double * __restrict  pgam0,
                                              const double * __restrict  palp,
                                              const double * __restrict  ptht) FUNC_ATTRIBUTES;


                    /*
                           Circular ogive.
                           RCS as function of theta angle, i.e. theta = 90-alpha
                           Formula 5.2-7
                     */


                 
                   __m512d rcs_f527_zmm8r8(const __m512d b,
                                           const __m512d alp) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f527_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) palp) FUNC_ATTRIBUTES;


                  
                   __m512d rcs_f527_zmm8r8_u(const double * __restrict  pb,
                                              const double * __restrict  palp) FUNC_ATTRIBUTES;


                   /*
                           Circular ogive.
                           RCS as function of theta angle, i.e. theta = (90-alpha) < theta << 90
                           Formula 5.2-8
                      */


                  
                   __m512d rcs_f528_zmm8r8(const __m512d r0,
                                           const __m512d b,
                                           const __m512d tht) FUNC_ATTRIBUTES;

                    
             
                   __m512d rcs_f528_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pb,
                                           const double * __restrict __ATTR_ALIGN__(64) pr0,
                                           const double * __restrict __ATTR_ALIGN__(64) ptht ) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f528_zmm8r8_u(const double * __restrict  pb,
                                           const double * __restrict  pr0,
                                           const double * __restrict  ptht ) FUNC_ATTRIBUTES;


                  /*
                       Dispatch kernel for Circular ogive RCS.
                       Formulae: 5.2-6, 5.2-7, 5.2-8
                    */


               
                   __m512d rcs_f52678_zmm8r8(const __m512d gam0,
                                             const __m512d alp,
                                             const __m512d tht,
                                             const __m512d b,
                                             const __m512d r0) FUNC_ATTRIBUTES;


                   __m512d rcs_f52678_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam0,
                                               const double * __restrict __ATTR_ALIGN__(64) palp,
                                               const double * __restrict __ATTR_ALIGN__(64) ptht,
                                               const double * __restrict __ATTR_ALIGN__(64) pb,
                                               const double * __restrict __ATTR_ALIGN__(64) pr0) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f52678_zmm8r8_u(const double * __restrict  pgam0,
                                               const double * __restrict  palp,
                                               const double * __restrict  ptht,
                                               const double * __restrict  pb,
                                               const double * __restrict  pr0) FUNC_ATTRIBUTES;

                  /*
                      Long thin bodies of revolution.
                      Axial-incidence backscatter RCS.
                      Long thin parabolic ogive - MISSILE NOISE CONE.
                      Formula 5.2-10
                  */

                  
                   __m512d rcs_f5210_zmm8r8(const __m512d gam0,
                                            const __m512d alp,
                                            const __m512d k0a) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f5210_zmm8r8_a( const double * __restrict __ATTR_ALIGN__(64) pgam0,
                                               const double * __restrict __ATTR_ALIGN__(64) palp,
                                               const double * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f5210_zmm8r8_u( const double * __restrict  pgam0,
                                               const double * __restrict  palp,
                                               const double * __restrict  pk0a,) FUNC_ATTRIBUTES;


                 /*
                      Long thin bodies of revolution.
                      Axial-incidence backscatter RCS.
                      High-frequency limit.
                      Long thin parabolic ogive - MISSILE NOISE CONE.
                      Formula 5.2-12
                  */


                  
                   __m512d rcs_f5212_zmm8r8(const __m512d gam0,
                                            const __m512d alp,
                                            const __m512d k0a) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f5212_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam0,
                                               const double * __restrict __ATTR_ALIGN__(64) palp,
                                               const double * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f5212_zmm8r8_u(const double * __restrict  pgam0,
                                               const double * __restrict palp,
                                               const double * __restrict  pk0a) FUNC_ATTRIBUTES;


                    /*
                         Circular ogive nose-on RCS (en emprical solution).
                         Formula 5.2-14
                     */


                 
                   __m512d rcs_f5214_zmm8r8(const __m512d gam0,
                                            const __m512d alp,
                                            const __m512d k0a) FUNC_ATTRIBUTES;


                   __m512d rcs_f5214_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const double * __restrict __ATTR_ALIGN__(64) palp,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


               
                   __m512d rcs_f5214_zmm8r8_u(const double * __restrict pgam0,
                                              const double * __restrict  palp,
                                              const double * __restrict  pk0a) FUNC_ATTRIBUTES;

                 



                  
















#endif /*__GMS_RCS_ELLIPS_OGIVE_ZMM16R4_H__*/
