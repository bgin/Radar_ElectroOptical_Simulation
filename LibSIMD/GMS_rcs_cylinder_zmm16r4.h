
#ifndef __GMS_RCS_CYLINDER_ZMM16R4_H__
#define __GMS_RCS_CYLINDER_ZMM16R4_H__ 120420231641


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



    const unsigned int GMS_RCS_CYLINDER_ZMM16R4_MAJOR = 1U;
    const unsigned int GMS_RCS_CYLINDER_ZMM16R4_MINOR = 0U;
    const unsigned int GMS_RCS_CYLINDER_ZMM16R4_MICRO = 0U;
    const unsigned int GMS_RCS_CYLINDER_ZMM16R4_FULLVER =
      1000U*GMS_RCS_CYLINDER_ZMM16R4_MAJOR+
      100U*GMS_RCS_CYLINDER_ZMM16R4_MINOR+
      10U*GMS_RCS_CYLINDER_ZMM16R4_MICRO;
    const char * const GMS_RCS_CYLINDER_ZMM16R4_CREATION_DATE = "12-04-2023 16:41  +00200 (WED 12 APR 2023 GMT+2)";
    const char * const GMS_RCS_CYLINDER_ZMM16R4_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_RCS_CYLINDER_ZMM16R4_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_RCS_CYLINDER_ZMM16R4_DESCRIPTION   = "AVX512 optimized Cylinder Radar Cross Section (analytic) functionality.";



#include <stdint.h>
#include <stdbool.h>
#include <immintrin.h>
#include "GMS_kernel_config.h"







              
                 

               


                   /* 
                         Low frequency scattering widths (k0a << 1).
                         Backscatter scattering width for E-field 
                         cylinder-parallel,formula 4.1-19
                    */
               
                   __m512 rcs_f419_zmm16r4(const __m512 a,
                                           const __m512 k0a)  FUNC_ATTRIBUTES;
                                                     


                  
                   __m512 rcs_f419_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0a)  FUNC_ATTRIBUTES;

                
                   __m512 rcs_f419_zmm16r4_u(const float * __restrict  pa,
                                             const float * __restrict  pk0a)  FUNC_ATTRIBUTES;


                /* 
                         Low frequency scattering widths (k0a << 1).
                         Backscatter scattering width for H-field 
                         cylinder-parallel,formula 4.1-20
                    */

                 
                   __m512 rcs_f4120_zmm16r4(const __m512 a,
                                            const __m512 k0a) FUNC_ATTRIBUTES;

                 
                   __m512 rcs_f4120_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0a)  FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f4120_zmm16r4_u(const float * __restrict  pa,
                                            const float * __restrict  pk0a)  FUNC_ATTRIBUTES;


                /*
                        Bistatic scattering widths, E-field cylinder axis-parallel
                        Formula 4.1-21
                   */

                
                   __m512 rcs_f4121_zmm16r4(const __m512 a,
                                            const __m512 k0a)  FUNC_ATTRIBUTES;


               
                   __m512 rcs_f4121_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0a)  FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f4121_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pk0a)  FUNC_ATTRIBUTES;



                 /*
                        Bistatic scattering widths, H-field cylinder axis-parallel
                        Formula 4.1-22
                   */

                
                   __m512 rcs_f4122_zmm16r4(const __m512 phi,
                                            const __m512 a,
                                            const __m512 k0a)  FUNC_ATTRIBUTES;


                
                   __m512 rcs_f4122_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pphi,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a)  FUNC_ATTRIBUTES;


               
                   __m512 rcs_f4122_zmm16r4_u(const float * __restrict  pphi,
                                              const float * __restrict  pa,
                                              const float * __restrict  pk0a)  FUNC_ATTRIBUTES;


                   /*
                       Forward scattering widths, E-field.
                       Formula 4.1-23
                   */
 
                 
                   __m512 rcs_f4123_zmm16r4(const __m512 a,
                                            const __m512 k0a)  FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f4123_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0a)  FUNC_ATTRIBUTES;

                  
                   __m512 rcs_f4123_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pk0a)  FUNC_ATTRIBUTES;


                  /*
                       Forward scattering widths, H-field.
                       Formula 4.1-24
                   */

                 
                   __m512 rcs_f4124_zmm16r4(const __m512 a,
                                            const __m512 k0a)  FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f4124_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a)  FUNC_ATTRIBUTES;


                  
                   __m512 rcs_f4124_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pk0a)  FUNC_ATTRIBUTES;

                    /*
                          Surface currents (k0a << 1), for long cylinder (wire).
                          E-field cylinder axis parallel.
                          Formula 4.1-25
                       */

                 
                   void Kz_f4125_zmm16r4(const __m512 eps0,
                                         const __m512 mu0,
                                         const __m512 Er,
                                         const __m512 Ei,
                                         const __m512 k0a,
                                         __m512 * __restrict Kzr,
                                         __m512 * __restrict Kzi)  FUNC_ATTRIBUTES;


                 
                   void Kz_f4125_zmm16r4_a(const  float * __restrict __ATTR_ALIGN__(64) peps0,
                                           const  float * __restrict __ATTR_ALIGN__(64) pmu0,
                                           const   float * __restrict __ATTR_ALIGN__(64) pEr,
                                           const   float * __restrict __ATTR_ALIGN__(64) pEi,
                                           const   float * __restrict __ATTR_ALIGN__(64) pk0a,
                                           float * __restrict __ATTR_ALIGN__(64) Kzr,
                                           float * __restrict __ATTR_ALIGN__(64) Kzi)  FUNC_ATTRIBUTES;


                 
                   void Kz_f4125_zmm16r4_u(const  float * __restrict  peps0,
                                           const  float * __restrict  pmu0,
                                           const   float * __restrict  pEr,
                                           const   float * __restrict  pEi,
                                           const   float * __restrict  pk0a,
                                           float * __restrict  Kzr,
                                           float * __restrict  Kzi)  FUNC_ATTRIBUTES;


                  /*
                          Surface currents (k0a << 1), for long cylinder (wire).
                          H-field cylinder axis parallel.
                          Formula 4.1-26
                   */

                  
                   void Kph_f4126_zmm16r4(const __m512 Hr,
                                          const __m512 Hi,
                                          __m512 * __restrict Kphr,
                                          __m512 * __restrict Kphi)  FUNC_ATTRIBUTES;

                 
                   void Kph_f4126_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) Hr,
                                            const float * __restrict __ATTR_ALIGN__(64) Hi,
                                           float * __restrict __ATTR_ALIGN__(64) Kphr,
                                          float * __restrict __ATTR_ALIGN__(64) Kphi)  FUNC_ATTRIBUTES;


                 
                   void Kph_f4126_zmm16r4_u(const float * __restrict  Hr,
                                            const float * __restrict  Hi,
                                           float * __restrict  Kphr,
                                          float * __restrict Kphi)  FUNC_ATTRIBUTES;


                   /*
                        The toal current along the wire.
                        Formula 4.1-27 

                    */

                  
                   void Iz_f4127_zmm16r4(const __m512 eps0,
                                         const __m512 mu0,
                                         const __m512 Er,
                                         const __m512 Ei,
                                         const __m512 k0a,
                                         const __m512 k0,
                                         __m512 * __restrict Izr,
                                         __m512 * __restrict Izi)  FUNC_ATTRIBUTES;


                 
                   void Iz_f4127_zmm16r4_a(const  float * __restrict __ATTR_ALIGN__(64) peps0,
                                           const  float * __restrict __ATTR_ALIGN__(64) pmu0,
                                           const   float * __restrict __ATTR_ALIGN__(64) pEr,
                                           const   float * __restrict __ATTR_ALIGN__(64) pEi,
                                           const   float * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const   float * __restrict __ATTR_ALIGN__(64) pk0,
                                           float * __restrict __ATTR_ALIGN__(64) Izr,
                                           float * __restrict __ATTR_ALIGN__(64) Izi)  FUNC_ATTRIBUTES;

                
                   void Iz_f4127_zmm16r4_u(const  float * __restrict  peps0,
                                           const  float * __restrict  pmu0,
                                           const   float * __restrict  pEr,
                                           const   float * __restrict  pEi,
                                           const   float * __restrict  pk0a,
                                           const   float * __restrict  pk0,
                                           float * __restrict  Izr,
                                           float * __restrict  Izi)  FUNC_ATTRIBUTES;


                   /*
                        Approximation for upper-middle and high-frequency region
                        (k0a > 2).
                        Bistatic creeping wave approximation for resonance region
                        (0<<phi<pi/2, k0a > 2)
                        Electric-field.
                    */

                 
                   void EO_f4129_zmm16r4(const __m512 phi2,
                                         const __m512 a,
                                         const __m512 r,
                                         const __m512 k0,
                                         const __m512 k0a,
                                         const __m512 Er,
                                         const __m512 Ei,
                                         __m512 * __restrict EOr,
                                         __m512 * __restrict EOi)  FUNC_ATTRIBUTES;

                
                   void EO_f4129_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                           const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const float * __restrict __ATTR_ALIGN__(64) pEr,
                                           const float * __restrict __ATTR_ALIGN__(64) pEi,
                                           float * __restrict __ATTR_ALIGN__(64) EOr,
                                           float * __restrict __ATTR_ALIGN__(64) EOi)  FUNC_ATTRIBUTES;

                 
                   void EO_f4129_zmm16r4_u(const float * __restrict  pphi2,
                                           const float * __restrict  pa,
                                           const float * __restrict  pr,
                                           const float * __restrict  pk0,
                                           const float * __restrict  pk0a,
                                           const float * __restrict  pEr,
                                           const float * __restrict  pEi,
                                           float * __restrict  EOr,
                                           float * __restrict  EOi)  FUNC_ATTRIBUTES;

                     /*
                        Approximation for upper-middle and high-frequency region
                        (k0a > 2).
                        Bistatic creeping wave approximation for resonance region
                        (0<<phi<pi/2, k0a > 2)
                        Magnetic-field.
                    */

               
                   void HO_f4131_zmm16r4(const __m512 phi2,
                                         const __m512 a,
                                         const __m512 r,
                                         const __m512 k0,
                                         const __m512 k0a,
                                         const __m512 Hr,
                                         const __m512 Hi,
                                         __m512 * __restrict HOr,
                                         __m512 * __restrict HOi)  FUNC_ATTRIBUTES;


                  
                   void HO_f4131_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                           const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const float * __restrict __ATTR_ALIGN__(64) pHr,
                                           const float * __restrict __ATTR_ALIGN__(64) pHi,
                                           float * __restrict __ATTR_ALIGN__(64) HOr,
                                           float * __restrict __ATTR_ALIGN__(64) HOi)  FUNC_ATTRIBUTES;

                 
                   void HO_f4131_zmm16r4_u(const float * __restrict  pphi2,
                                           const float * __restrict  pa,
                                           const float * __restrict  pr,
                                           const float * __restrict  pk0,
                                           const float * __restrict  pk0a,
                                           const float * __restrict  pHr,
                                           const float * __restrict  pHi,
                                           float * __restrict  HOr,
                                           float * __restrict  HOi) FUNC_ATTRIBUTES;

                 
                 /*
                        Approximation for upper-middle and high-frequency region
                        (k0a > 2).
                        Bistatic creeping wave approximation for resonance region
                        (0<<phi<pi/2, k0a > 2)
                        Electric-field.
                        Formula 4.1-30
                    */

                 
                   void EC_f4130_zmm16r4(const __m512 Er,
                                         const __m512 Ei,
                                         const __m512 a,
                                         const __m512 r,
                                         const __m512 k0,
                                         const __m512 k0a,
                                         const __m512 phi,
                                         __m512 * __restrict ECr,
                                         __m512 * __restrict ECi) FUNC_ATTRIBUTES;

                 
                   void EC_f4130_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pEr,
                                           const float * __restrict __ATTR_ALIGN__(64) pEi,
                                           const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi,
                                           float * __restrict __ATTR_ALIGN__(64) ECr,
                                           float * __restrict __ATTR_ALIGN__(64) ECi) FUNC_ATTRIBUTES;

                
                   void EC_f4130_zmm16r4_a(const float * __restrict  pEr,
                                           const float * __restrict  pEi,
                                           const float * __restrict  pa,
                                           const float * __restrict  pr,
                                           const float * __restrict  pk0,
                                           const float * __restrict  pk0a,
                                           const float * __restrict  pphi,
                                           float * __restrict  ECr,
                                           float * __restrict ECi) FUNC_ATTRIBUTES;

                    /*
                        Approximation for upper-middle and high-frequency region
                        (k0a > 2).
                        Bistatic creeping wave approximation for resonance region
                        valid only for (0<<phi<pi/2, k0a > 2)
                        Magnetic-field.
                        Formula 4.1-32
                    */


                
                   void HC_f4132_zmm16r4(const __m512 Hr,
                                         const __m512 Hi,
                                         const __m512 a,
                                         const __m512 r,
                                         const __m512 k0,
                                         const __m512 k0a,
                                         const __m512 phi,
                                         __m512 * __restrict HCr,
                                         __m512 * __restrict HCi) FUNC_ATTRIBUTES;

                  
                   void HC_f4132_zmm16r4_a(const  float * __restrict __ATTR_ALIGN__(64)  pHr,
                                           const  float * __restrict __ATTR_ALIGN__(64)  pHi,
                                           const  float * __restrict __ATTR_ALIGN__(64)  pa,
                                           const  float * __restrict __ATTR_ALIGN__(64)  pr,
                                           const  float * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const  float * __restrict __ATTR_ALIGN__(64)  pk0a,
                                           const  float * __restrict __ATTR_ALIGN__(64)  pphi,
                                           float * __restrict __ATTR_ALIGN__(64)  HCr,
                                           float * __restrict __ATTR_ALIGN__(64)  HCi) FUNC_ATTRIBUTES;

                
                   void HC_f4132_zmm16r4_u(const  float * __restrict  pHr,
                                           const  float * __restrict  pHi,
                                           const  float * __restrict  pa,
                                           const  float * __restrict  pr,
                                           const  float * __restrict  pk0,
                                           const  float * __restrict  pk0a,
                                           const  float * __restrict  pphi,
                                           float * __restrict   HCr,
                                           float * __restrict   HCi) FUNC_ATTRIBUTES;

                   /*

                       Backscattering creeping-wave approximation for resonance region
                       (phi == 0, k0a > 2).
                       Optical wave component e-field, formula 4.1-33
                   */

               
                   void EO_f4133_zmm16r4(const __m512 Er,
                                         const __m512 Ei,
                                         const __m512 a,
                                         const __m512 r,
                                         const __m512 k0a,
                                         __m512 * __restrict EOr,
                                         __m512 * __restrict EOi) FUNC_ATTRIBUTES;


                
                   void EO_f4133_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pEr,
                                           const float * __restrict __ATTR_ALIGN__(64) pEi,
                                           const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                           float * __restrict __ATTR_ALIGN__(64) EOr,
                                           float * __restrict __ATTR_ALIGN__(64) EOi) FUNC_ATTRIBUTES;

                 
                   void EO_f4133_zmm16r4_u(const float * __restrict  pEr,
                                           const float * __restrict  pEi,
                                           const float * __restrict  pa,
                                           const float * __restrict  pr,
                                           const float * __restrict  pk0a,
                                           float * __restrict  EOr,
                                           float * __restrict  EOi) FUNC_ATTRIBUTES;

                     /*

                       Backscattering creeping-wave approximation for resonance region
                       (phi == 0, k0a > 2).
                       Optical wave component h-field, formula 4.1-35
                   */

                 
                   void HO_f4135_zmm16r4(const __m512 Hr,
                                         const __m512 Hi,
                                         const __m512 a,
                                         const __m512 r,
                                         const __m512 k0a,
                                         __m512 * __restrict HOr,
                                         __m512 * __restrict HOi) FUNC_ATTRIBUTES;

                
                   void HO_f4135_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pHr,
                                           const float * __restrict __ATTR_ALIGN__(64) pHi,
                                           const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                           float * __restrict __ATTR_ALIGN__(64) HOr,
                                           float * __restrict __ATTR_ALIGN__(64) HOi) FUNC_ATTRIBUTES;

                 
                   void HO_f4135_zmm16r4_u(const float * __restrict  pHr,
                                           const float * __restrict  pHi,
                                           const float * __restrict  pa,
                                           const float * __restrict  pr,
                                           const float * __restrict  pk0a,
                                           float * __restrict  HOr,
                                           float * __restrict  HOi) FUNC_ATTRIBUTES;

                   /*

                       Backscattering creeping-wave approximation for resonance region
                       (phi == 0, k0a > 2).
                       Creeping wave component e-field, formula 4.1-34
                   */

                
                   void EC_f4134_zmm16r4(const __m512 Er,
                                         const __m512 Ei,
                                         const __m512 a,
                                         const __m512 r,
                                         const __m512 k0,
                                         __m512 * __restrict ECr,
                                         __m512 * __restrict ECi) FUNC_ATTRIBUTES;

               
                   void EC_f4134_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pEr,
                                         const float * __restrict __ATTR_ALIGN__(64) pEi,
                                         const float * __restrict __ATTR_ALIGN__(64) pa,
                                         const float * __restrict __ATTR_ALIGN__(64) pr,
                                         const float * __restrict __ATTR_ALIGN__(64) pk0,
                                         float * __restrict __ATTR_ALIGN__(64) ECr,
                                         float * __restrict __ATTR_ALIGN__(64) ECi) FUNC_ATTRIBUTES;

                  
                 
                   void EC_f4134_zmm16r4_u(const float * __restrict  pEr,
                                           const float * __restrict  pEi,
                                           const float * __restrict  pa,
                                           const float * __restrict  pr,
                                           const float * __restrict  pk0,
                                           float * __restrict  ECr,
                                           float * __restrict  ECi) FUNC_ATTRIBUTES;


                   /*

                       Backscattering creeping-wave approximation for resonance region
                       (phi == 0, k0a > 2).
                       Creeping wave component h-field, formula 4.1-36
                   */


                 
                   void HC_f4136_zmm16r4(const __m512 Hr,
                                         const __m512 Hi,
                                         const __m512 a,
                                         const __m512 r,
                                         const __m512 k0,
                                         __m512 * __restrict HCr,
                                         __m512 * __restrict HCi) FUNC_ATTRIBUTES;

                
                   void HC_f4136_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pHr,
                                           const float * __restrict __ATTR_ALIGN__(64) pHi,
                                           const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           float * __restrict __ATTR_ALIGN__(64) HCr,
                                           float * __restrict __ATTR_ALIGN__(64) HCi) FUNC_ATTRIBUTES;

                  
                   void HC_f4136_zmm16r4_u(const float * __restrict  pHr,
                                           const float * __restrict  pHi,
                                           const float * __restrict  pa,
                                           const float * __restrict  pr,
                                           const float * __restrict  pk0,
                                           float * __restrict  HCr,
                                           float * __restrict  HCi) FUNC_ATTRIBUTES;

                  /*
                        Bistatic scattering width in high frequency limit (k0a > 20)
                        for |PI-phi| > k0a^0.3
                        Formula 4.1-37
                    */

                
                   __m512 rcs_f4137_zmm16r4(const __m512 a,
                                            const __m512 phi2) FUNC_ATTRIBUTES;


              
                   __m512 rcs_f4137_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi2) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f4137_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pphi2) FUNC_ATTRIBUTES;


                    /*
                          Backscattering Width in High-Frequency Limit (k0a > 20)
                          Formula 4.1-38
                     */

                   
                
                   __m512 rcs_f4138_zmm16r4(const __m512 a) FUNC_ATTRIBUTES;

                
                   __m512 rcs_f4138_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f4138_zmm16r4_u(const float * __restrict  pa) FUNC_ATTRIBUTES;


                   /*
                         Forward scattering widths and pattern in high-frequency limit
                         (k0a>20.0)
                         Formula 4.1-40, RCS.
                     */

                
                   __m512 rcs_f4140_zmm16r4(const __m512 k0a,
                                            const __m512 alpha) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f4140_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) palpha) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f4140_zmm16r4_u(const float * __restrict  pk0a,
                                              const float * __restrict  palpha) FUNC_ATTRIBUTES;


                     /*
                         Forward scattering widths and pattern in high-frequency limit
                         (k0a>20.0), forward scattered (diffracted) e-field
                         Formula 4.1-39.

                       */


                 
                   void Es_f4139_zmm16r4(const __m512 Er,
                                         const __m512 Ei,
                                         const __m512 r,
                                         const __m512 k0,
                                         const __m512 alp
                                         const __m512 k0a,
                                         __m512 * __restrict Esr,
                                         __m512 * __restrict Esi) FUNC_ATTRIBUTES;


                 
                   void Es_f4139_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pEr,
                                           const float * __restrict __ATTR_ALIGN__(64) pEi,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) palp
                                           const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                           float * __restrict __ATTR_ALIGN__(64) Esr,
                                           float * __restrict __ATTR_ALIGN__(64) Esi) FUNC_ATTRIBUTES;


                  
                   void Es_f4139_zmm16r4_u(const float * __restrict  pEr,
                                           const float * __restrict  pEi,
                                           const float * __restrict  pr,
                                           const float * __restrict  pk0,
                                           const float * __restrict  palp
                                           const float * __restrict  pk0a,
                                           float * __restrict  Esr,
                                           float * __restrict  Esi) FUNC_ATTRIBUTES;


                  /*
                         Forward scattering widths and pattern in high-frequency limit
                         (k0a>20.0), constant angle (alpha=0)
                         Formula 4.1-41, RCS.
                     */

             
                   __m512 rcs_f4141_zmm16r4(const __m512 k0a) FUNC_ATTRIBUTES;

                

                 
                   __m512 rcs_f4141_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  pk0a) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f4141_zmm16r4_u(const float * __restrict   pk0a) FUNC_ATTRIBUTES;


                   /*
                        Approximations for the low frequency region (k0a<<1,k1a<<1)
                        Scattered far-zone e-field, formula 4.1-45
                    */

                 
                   void Es_f4145_zmm16r4(const __m512 EIr,
                                         const __m512 EIi,
                                         const __m512 r,
                                         const __m512 k0,
                                         const __m512 k0a,
                                         const __m512 phi,
                                         const __m512 eps0,
                                         const __m512 eps1,
                                         const __m512 mu0,
                                         const __m512 mu1,
                                         __m512 * __restrict ESr,
                                         __m512 * __restrict ESi) FUNC_ATTRIBUTES;

               
                   void Es_f4145_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pEIr,
                                           const float * __restrict __ATTR_ALIGN__(64) pEIi,
                                           const float * __restrict __ATTR_ALIGN__(64)  pr,
                                           const float * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const float * __restrict __ATTR_ALIGN__(64)  pk0a,
                                           const float * __restrict __ATTR_ALIGN__(64)  pphi,
                                           const float * __restrict __ATTR_ALIGN__(64)  peps0,
                                           const float * __restrict __ATTR_ALIGN__(64)  peps1,
                                           const float * __restrict __ATTR_ALIGN__(64)  pmu0,
                                           const float * __restrict __ATTR_ALIGN__(64)  pmu1,
                                           float * __restrict __ATTR_ALIGN__(64)  ESr,
                                           float * __restrict __ATTR_ALIGN__(64)  ESi) FUNC_ATTRIBUTES;

                  
                   void Es_f4145_zmm16r4_u(const float * __restrict  pEIr,
                                           const float * __restrict  pEIi,
                                           const float * __restrict  pr,
                                           const float * __restrict   pk0,
                                           const float * __restrict  pk0a,
                                           const float * __restrict   pphi,
                                           const float * __restrict   peps0,
                                           const float * __restrict  peps1,
                                           const float * __restrict   pmu0,
                                           const float * __restrict   pmu1,
                                           float * __restrict   ESr,
                                           float * __restrict   ESi) FUNC_ATTRIBUTES;

                  /*
                        Approximations for the low frequency region (k0a<<1,k1a<<1)
                        Scattered far-zone h-field, formula 4.1-46
                    */


               
                   void Hs_f4146_zmm16r4(const __m512 HIr,
                                         const __m512 HIi,
                                         const __m512 r,
                                         const __m512 k0,
                                         const __m512 k0a,
                                         const __m512 phi,
                                         const __m512 eps0,
                                         const __m512 eps1,
                                         const __m512 mu0,
                                         const __m512 mu1,
                                         __m512 * __restrict HSr,
                                         __m512 * __restrict HSi) FUNC_ATTRIBUTES;

               
                   void Hs_f4146_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pHIr,
                                           const float * __restrict __ATTR_ALIGN__(64) pHIi,
                                           const float * __restrict __ATTR_ALIGN__(64)  pr,
                                           const float * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const float * __restrict __ATTR_ALIGN__(64)  pk0a,
                                           const float * __restrict __ATTR_ALIGN__(64)  pphi,
                                           const float * __restrict __ATTR_ALIGN__(64)  peps0,
                                           const float * __restrict __ATTR_ALIGN__(64)  peps1,
                                           const float * __restrict __ATTR_ALIGN__(64)  pmu0,
                                           const float * __restrict __ATTR_ALIGN__(64)  pmu1,
                                           float * __restrict __ATTR_ALIGN__(64)  HSr,
                                           float * __restrict __ATTR_ALIGN__(64)  HSi) FUNC_ATTRIBUTES;

               
                   void Hs_f4146_zmm16r4_u(const float * __restrict   pHIr,
                                           const float * __restrict   pHIi,
                                           const float * __restrict   pr,
                                           const float * __restrict   pk0,
                                           const float * __restrict   pk0a,
                                           const float * __restrict   pphi,
                                           const float * __restrict   peps0,
                                           const float * __restrict   peps1,
                                           const float * __restrict   pmu0,
                                           const float * __restrict   pmu1,
                                           float * __restrict  HSr,
                                           float * __restrict   HSi) FUNC_ATTRIBUTES;

                 /*
                      Bistatic scattering width (k0a<<1, k1a<<1) at the angle 'phi'
                      Formula 4.1-47

                   */

                
                   __m512 rcs_f4147_zmm16r4(const __m512 a,
                                            const __m512 k0a,
                                            const __m512 phi,
                                            const __m512 eps1,
                                            const __m512 eps0,
                                            const __m512 mu1,
                                            const __m512 mu0) FUNC_ATTRIBUTES;


                   
               
                   __m512 rcs_f4147_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                            const float * __restrict __ATTR_ALIGN__(64) pphi,
                                            const float * __restrict __ATTR_ALIGN__(64) peps1,
                                            const float * __restrict __ATTR_ALIGN__(64) peps0,
                                            const float * __restrict __ATTR_ALIGN__(64) pmu1,
                                            const float * __restrict __ATTR_ALIGN__(64) pmu0) FUNC_ATTRIBUTES;


               
                   __m512 rcs_f4147_zmm16r4_u(const float * __restrict  pa,
                                            const float * __restrict  pk0a,
                                            const float * __restrict  pphi,
                                            const float * __restrict  peps1,
                                            const float * __restrict  peps0,
                                            const float * __restrict  pmu1,
                                            const float * __restrict  pmu0) FUNC_ATTRIBUTES;


                    /*
                      Bistatic scattering width (k0a<<1, k1a<<1) at the angle 'phi'
                      Formula 4.1-48

                   */   

                 
                   __m512 rcs_f4148_zmm16r4(const __m512 a,
                                            const __m512 k0a,
                                            const __m512 phi,
                                            const __m512 eps1,
                                            const __m512 eps0,
                                            const __m512 mu1,
                                            const __m512 mu0) FUNC_ATTRIBUTES;

               
                   __m512 rcs_f4148_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                            const float * __restrict __ATTR_ALIGN__(64) pphi,
                                            const float * __restrict __ATTR_ALIGN__(64) peps1,
                                            const float * __restrict __ATTR_ALIGN__(64) peps0,
                                            const float * __restrict __ATTR_ALIGN__(64) pmu1,
                                            const float * __restrict __ATTR_ALIGN__(64) pmu0) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f4148_zmm16r4_u(const float * __restrict  pa,
                                            const float * __restrict pk0a,
                                            const float * __restrict  pphi,
                                            const float * __restrict  peps1,
                                            const float * __restrict  peps0,
                                            const float * __restrict  pmu1,
                                            const float * __restrict  pmu0) FUNC_ATTRIBUTES;


                   /*
                         Backscattering width (k0a<<1,k1a<<1), when phi = 0
                         Formula 4.1-49
                    */
                 
                 
                   __m512 rcs_f4149_zmm16r4(const __m512 a,
                                            const __m512 k0a,
                                            const __m512 eps1,
                                            const __m512 eps0,
                                            const __m512 mu1,
                                            const __m512 mu0) FUNC_ATTRIBUTES;

                
                   __m512 rcs_f4149_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) peps1,
                                              const float * __restrict __ATTR_ALIGN__(64) peps0,
                                              const float * __restrict __ATTR_ALIGN__(64) pmu1,
                                              const float * __restrict __ATTR_ALIGN__(64) pmu0) FUNC_ATTRIBUTES;


              
                   __m512 rcs_f4149_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pk0a,
                                              const float * __restrict  peps1,
                                              const float * __restrict  peps0,
                                              const float * __restrict  pmu1,
                                              const float * __restrict  pmu0) FUNC_ATTRIBUTES;

                     /*
                         Backscattering width (k0a<<1,k1a<<1), when phi = 0
                         Formula 4.1-50
                    */

                 
                   __m512 rcs_f4150_zmm16r4(const __m512 a,
                                            const __m512 k0a,
                                            const __m512 eps1,
                                            const __m512 eps0,
                                            const __m512 mu1,
                                            const __m512 mu0) FUNC_ATTRIBUTES;

                
                   __m512 rcs_f4150_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) peps1,
                                              const float * __restrict __ATTR_ALIGN__(64) peps0,
                                              const float * __restrict __ATTR_ALIGN__(64) pmu1,
                                              const float * __restrict __ATTR_ALIGN__(64) pmu0) FUNC_ATTRIBUTES;


             
                   __m512 rcs_f4150_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pk0a,
                                              const float * __restrict  peps1,
                                              const float * __restrict  peps0,
                                              const float * __restrict  pmu1,
                                              const float * __restrict  pmu0) FUNC_ATTRIBUTES;


                    /*
                         Forward scattering width (k0a<<1, k1a<<1), phi = pi
                         Formula 4.1-51
                     */

                  
                   __m512 rcs_f4151_zmm16r4(const __m512 a,
                                            const __m512 k0a,
                                            const __m512 eps1,
                                            const __m512 eps0,
                                            const __m512 mu1,
                                            const __m512 mu0) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f4151_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) peps1,
                                              const float * __restrict __ATTR_ALIGN__(64) peps0,
                                              const float * __restrict __ATTR_ALIGN__(64) pmu1,
                                              const float * __restrict __ATTR_ALIGN__(64) pmu0) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f4151_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pk0a,
                                              const float * __restrict  peps1,
                                              const float * __restrict  peps0,
                                              const float * __restrict  pmu1,
                                              const float * __restrict  pmu0) FUNC_ATTRIBUTES;


                      /*
                         Forward scattering width (k0a<<1, k1a<<1), phi = pi
                         Formula 4.1-52
                     */

                 
                   __m512 rcs_f4152_zmm16r4(const __m512 a,
                                            const __m512 k0a,
                                            const __m512 eps1,
                                            const __m512 eps0,
                                            const __m512 mu1,
                                            const __m512 mu0) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f4152_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) peps1,
                                              const float * __restrict __ATTR_ALIGN__(64) peps0,
                                              const float * __restrict __ATTR_ALIGN__(64) pmu1,
                                              const float * __restrict __ATTR_ALIGN__(64) pmu0) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f4152_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pk0a,
                                              const float * __restrict  peps1,
                                              const float * __restrict  peps0,
                                              const float * __restrict  pmu1,
                                              const float * __restrict  pmu0) FUNC_ATTRIBUTES;

                     /*
                           Fresnel reflection and transmission coefficients
                           Formula 4.1-72
                       */

                
                   void Tin_f4172_zmm16r4(const __m512 mur,
                                          const __m512 mui,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 psi,
                                          __m512 * __restrict Tinr,
                                          __m512 * __restrict Tini) FUNC_ATTRIBUTES;


                
                   void Tin_f4172_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pmur,
                                            const float * __restrict __ATTR_ALIGN__(64) pmui,
                                            const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                            const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                            const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                            float * __restrict __ATTR_ALIGN__(64) Tinr,
                                            float * __restrict __ATTR_ALIGN__(64) Tini) FUNC_ATTRIBUTES;


                 
                   void Tin_f4172_zmm16r4_u(const float * __restrict  pmur,
                                            const float * __restrict  pmui,
                                            const float * __restrict  pepsr,
                                            const float * __restrict  pepsi,
                                            const float * __restrict  ppsi,
                                            float * __restrict  Tinr,
                                            float * __restrict  Tini) FUNC_ATTRIBUTES;


                     /*
                           Fresnel reflection and transmission coefficients
                           Formula 4.1-73
                       */

                   void Tin_f4173_zmm16r4(const __m512 mur,
                                          const __m512 mui,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 psi,
                                          __m512 * __restrict Tinr,
                                          __m512 * __restrict Tini) FUNC_ATTRIBUTES;

                  
                   void Tin_f4173_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pmur,
                                            const float * __restrict __ATTR_ALIGN__(64) pmui,
                                            const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                            const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                            const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                            float * __restrict __ATTR_ALIGN__(64) Tinr,
                                            float * __restrict __ATTR_ALIGN__(64) Tini) FUNC_ATTRIBUTES;


                 
                   void Tin_f4173_zmm16r4_u(const float * __restrict  pmur,
                                            const float * __restrict  pmui,
                                            const float * __restrict  pepsr,
                                            const float * __restrict  pepsi,
                                            const float * __restrict  ppsi,
                                            float * __restrict  Tinr,
                                            float * __restrict  Tini) FUNC_ATTRIBUTES;


                    /*
                           Fresnel reflection and transmission coefficients
                           Formula 4.1-74
                       */

                
                   void Tout_f4174_zmm16r4(const __m512 mur,
                                          const __m512 mui,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 psi,
                                          __m512 * __restrict Toutr,
                                          __m512 * __restrict Touti) FUNC_ATTRIBUTES;


                
                   void Tout_f4174_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pmur,
                                            const float * __restrict __ATTR_ALIGN__(64) pmui,
                                            const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                            const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                            const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                            float * __restrict __ATTR_ALIGN__(64) Toutr,
                                            float * __restrict __ATTR_ALIGN__(64) Touti) FUNC_ATTRIBUTES;


               
                   void Tout_f4174_zmm16r4_u(const float * __restrict pmur,
                                            const float * __restrict  pmui,
                                            const float * __restrict  pepsr,
                                            const float * __restrict  pepsi,
                                            const float * __restrict  ppsi,
                                            float * __restrict  Toutr,
                                            float * __restrict  Touti) FUNC_ATTRIBUTES;


                   /*
                           Fresnel reflection and transmission coefficients
                           Formula 4.1-75
                       */


               
                   void Tout_f4175_zmm16r4(const __m512 mur,
                                           const __m512 mui,
                                           const __m512 epsr,
                                           const __m512 epsi,
                                           const __m512 psi,
                                           __m512 * __restrict Toutr,
                                           __m512 * __restrict Touti) FUNC_ATTRIBUTES;


               
                   void Tout_f4175_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pmur,
                                            const float * __restrict __ATTR_ALIGN__(64) pmui,
                                            const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                            const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                            const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                            float * __restrict __ATTR_ALIGN__(64) Toutr,
                                            float * __restrict __ATTR_ALIGN__(64) Touti) FUNC_ATTRIBUTES;

                
                   void Tout_f4175_zmm16r4_u(const float * __restrict  pmur,
                                            const float * __restrict  pmui,
                                            const float * __restrict  pepsr,
                                            const float * __restrict  pepsi,
                                            const float * __restrict  ppsi,
                                            float * __restrict  Toutr,
                                            float * __restrict  Touti) FUNC_ATTRIBUTES;


                   /*
                           Fresnel reflection and transmission coefficients
                           Formula 4.1-76
                    */

                 
                   void Rin_f4176_zmm16r4( const __m512 mur,
                                           const __m512 mui,
                                           const __m512 epsr,
                                           const __m512 epsi,
                                           const __m512 psi,
                                           __m512 * __restrict Rinr,
                                           __m512 * __restrict Rini) FUNC_ATTRIBUTES;

             
                   void Rin_f4176_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) pmur,
                                             const float * __restrict __ATTR_ALIGN__(64) pmui,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                             const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                             float * __restrict __ATTR_ALIGN__(64) Rinr,
                                             float * __restrict __ATTR_ALIGN__(64) Rini) FUNC_ATTRIBUTES;


                 
                   void Rin_f4176_zmm16r4_u( const float * __restrict  pmur,
                                             const float * __restrict  pmui,
                                             const float * __restrict  pepsr,
                                             const float * __restrict  pepsi,
                                             const float * __restrict  ppsi,
                                             float * __restrict  Rinr,
                                             float * __restrict  Rini) FUNC_ATTRIBUTES;


                    /*
                           Fresnel reflection and transmission coefficients
                           Formula 4.1-77
                    */


                
                   void Rin_f4177_zmm16r4( const __m512 mur,
                                           const __m512 mui,
                                           const __m512 epsr,
                                           const __m512 epsi,
                                           const __m512 psi,
                                           __m512 * __restrict Rinr,
                                           __m512 * __restrict Rini) FUNC_ATTRIBUTES;

                  
                   void Rin_f4177_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) pmur,
                                             const float * __restrict __ATTR_ALIGN__(64) pmui,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                             const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                             float * __restrict __ATTR_ALIGN__(64) Rinr,
                                             float * __restrict __ATTR_ALIGN__(64) Rini ) FUNC_ATTRIBUTES;


                  
                   void Rin_f4177_zmm16r4_u( const float * __restrict  pmur,
                                             const float * __restrict  pmui,
                                             const float * __restrict  pepsr,
                                             const float * __restrict  pepsi,
                                             const float * __restrict  ppsi,
                                             float * __restrict  Rinr,
                                             float * __restrict  Rini ) FUNC_ATTRIBUTES;


                  /*
                          Specular rays reflection
                          Formula 4.1-64
                      */

                
                   void Rext_f4164_zmm16r4(const __m512 mur,
                                           const __m512 mui,
                                           const __m512 epsr,
                                           const __m512 epsi,
                                           __m512 * __restrict Rexr,
                                           __m512 * __restrict Rexi) FUNC_ATTRIBUTES;


                 
                   void Rext_f4164_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pmur,
                                             const float * __restrict __ATTR_ALIGN__(64) pmui,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                             float * __restrict __ATTR_ALIGN__(64) Rexr,
                                             float * __restrict __ATTR_ALIGN__(64) Rexi) FUNC_ATTRIBUTES;

                  
                  
                   void Rext_f4164_zmm16r4_u(const float * __restrict  pmur,
                                             const float * __restrict  pmui,
                                             const float * __restrict  pepsr,
                                             const float * __restrict  pepsi,
                                             float * __restrict  Rexr,
                                             float * __restrict  Rexi) FUNC_ATTRIBUTES;


                  /*

                         Axial rays, when phi = 0
                         Formula 4.1-67
                    */

                  
                
                   void Tin_f4167_zmm16r4( const __m512 mur,
                                           const __m512 mui,
                                           const __m512 epsr,
                                           const __m512 epsi,
                                           __m512 * __restrict Tinr,
                                           __m512 * __restrict Tini) FUNC_ATTRIBUTES;

              
                   void Tin_f4167_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) pmur,
                                             const float * __restrict __ATTR_ALIGN__(64) pmui,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                             float * __restrict __ATTR_ALIGN__(64) Tinr,
                                             float * __restrict __ATTR_ALIGN__(64) Tini ) FUNC_ATTRIBUTES;


            
                   void Tin_f4167_zmm16r4_u( const float * __restrict  pmur,
                                             const float * __restrict  pmui,
                                             const float * __restrict  pepsr,
                                             const float * __restrict  pepsi,
                                             float * __restrict  Tinr,
                                             float * __restrict  Tini ) FUNC_ATTRIBUTES;

                  /*
                          Axial rays, when phi = 0
                          Formula 4.1-68
                   */


               
                   void Tout_f4168_zmm16r4( const __m512 mur,
                                           const __m512 mui,
                                           const __m512 epsr,
                                           const __m512 epsi,
                                           __m512 * __restrict Toutr,
                                           __m512 * __restrict Touti) FUNC_ATTRIBUTES;

               
                   void Tout_f4168_zmm16r4_a(  const float * __restrict __ATTR_ALIGN__(64) pmur,
                                             const float * __restrict __ATTR_ALIGN__(64) pmui,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                             float * __restrict __ATTR_ALIGN__(64) Toutr,
                                             float * __restrict __ATTR_ALIGN__(64) Touti ) FUNC_ATTRIBUTES;


               
                   void Tout_f4168_zmm16r4_u(  const float * __restrict  pmur,
                                             const float * __restrict  pmui,
                                             const float * __restrict  pepsr,
                                             const float * __restrict  pepsi,
                                             float * __restrict  Toutr,
                                             float * __restrict Touti ) FUNC_ATTRIBUTES;


                  /*
                          Axial rays, when phi = 0
                          Formula 4.1-69
                   */


                
                   void Rint_f4169_zmm16r4(const __m512 mur,
                                           const __m512 mui,
                                           const __m512 epsr,
                                           const __m512 epsi,
                                           __m512 * __restrict Rintr,
                                           __m512 * __restrict Rinti) FUNC_ATTRIBUTES;


               
                   void Rint_f4169_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) pmur,
                                             const float * __restrict __ATTR_ALIGN__(64) pmui,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                             float * __restrict __ATTR_ALIGN__(64) Rintr,
                                             float * __restrict __ATTR_ALIGN__(64) Rinti) FUNC_ATTRIBUTES;


              
                   void Rint_f4169_zmm16r4_u( const float * __restrict pmur,
                                              const float * __restrict  pmui,
                                              const float * __restrict  pepsr,
                                              const float * __restrict  pepsi,
                                              float * __restrict  Rintr,
                                              float * __restrict  Rinti) FUNC_ATTRIBUTES;


                   /*
                       Backscatter widths in high-frequency limit.
                       Phi = 0, formula 4.1-91,for k1a>5.
                    */

                 
                   __m512 rcs_f4191_zmm16r4(const __m512 a,
                                            const __m512 mur,
                                            const __m512 mui,
                                            const __m512 epsr,
                                            const __m512 epsi ) FUNC_ATTRIBUTES;

               
                   __m512 rcs_f4191_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pmur,
                                              const float * __restrict __ATTR_ALIGN__(64) pmui,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsi, ) FUNC_ATTRIBUTES;


              
                   __m512 rcs_f4191_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pmur,
                                              const float * __restrict  pmui,
                                              const float * __restrict  pepsr,
                                              const float * __restrict  pepsi, ) FUNC_ATTRIBUTES;


                    /*
                         Bistatic scattering width (k0a0<<1, k1a0<<1), function of phi angle.
                         Formula 4.1-104
                      */

                 
                   __m512 rcs_f41104_zmm16r4(const __m512 a0,
                                             const __m512 a1,
                                             const __m512 k0a0,
                                             const __m512 phi,
                                             const __m512 mu1r,
                                             const __m512 mu1i,
                                             const __m512 mu0r,
                                             const __m512 mu0i,
                                             const __m512 eps1r,
                                             const __m512 eps1i,
                                             const __m512 eps0r,
                                             const __m512 eps0i) FUNC_ATTRIBUTES;


                  
                   __m512 rcs_f41104_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa0,
                                               const float * __restrict __ATTR_ALIGN__(64) pa1,
                                               const float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                               const float * __restrict __ATTR_ALIGN__(64) pphi,
                                               const float * __restrict __ATTR_ALIGN__(64) pmu1r,
                                               const float * __restrict __ATTR_ALIGN__(64) pmu1i,
                                               const float * __restrict __ATTR_ALIGN__(64) pmu0r,
                                               const float * __restrict __ATTR_ALIGN__(64) pmu0i,
                                               const float * __restrict __ATTR_ALIGN__(64) peps1r,
                                               const float * __restrict __ATTR_ALIGN__(64) peps1i,
                                               const float * __restrict __ATTR_ALIGN__(64) peps0r,
                                               const float * __restrict __ATTR_ALIGN__(64) peps0i) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f41104_zmm16r4_u(const float * __restrict  pa0,
                                               const float * __restrict  pa1,
                                               const float * __restrict  pk0a0,
                                               const float * __restrict  pphi,
                                               const float * __restrict  pmu1r,
                                               const float * __restrict  pmu1i,
                                               const float * __restrict  pmu0r,
                                               const float * __restrict  pmu0i,
                                               const float * __restrict  peps1r,
                                               const float * __restrict  peps1i,
                                               const float * __restrict  peps0r,
                                               const float * __restrict  peps0i) FUNC_ATTRIBUTES;

                /*
                         Backscattering  width (k0a0<<1, k1a0<<1), phi = 0
                         Formula 4.1-105
                  */

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f41105_zmm16r4(const __m512 a0,
                                             const __m512 a1,
                                             const __m512 k0a0,
                                             const __m512 mu1r,
                                             const __m512 mu1i,
                                             const __m512 mu0r,
                                             const __m512 mu0i,
                                             const __m512 eps1r,
                                             const __m512 eps1i,
                                             const __m512 eps0r,
                                             const __m512 eps0i) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f41105_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa0,
                                               const float * __restrict __ATTR_ALIGN__(64) pa1,
                                               const float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                               const float * __restrict __ATTR_ALIGN__(64) pmu1r,
                                               const float * __restrict __ATTR_ALIGN__(64) pmu1i,
                                               const float * __restrict __ATTR_ALIGN__(64) pmu0r,
                                               const float * __restrict __ATTR_ALIGN__(64) pmu0i,
                                               const float * __restrict __ATTR_ALIGN__(64) peps1r,
                                               const float * __restrict __ATTR_ALIGN__(64) peps1i,
                                               const float * __restrict __ATTR_ALIGN__(64) peps0r,
                                               const float * __restrict __ATTR_ALIGN__(64) peps0i) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f41105_zmm16r4_u(const float * __restrict  pa0,
                                               const float * __restrict  pa1,
                                               const float * __restrict  pk0a0,
                                               const float * __restrict  pmu1r,
                                               const float * __restrict  pmu1i,
                                               const float * __restrict  pmu0r,
                                               const float * __restrict  pmu0i,
                                               const float * __restrict  peps1r,
                                               const float * __restrict  peps1i,
                                               const float * __restrict  peps0r,
                                               const float * __restrict  peps0i) FUNC_ATTRIBUTES;

                /*
                      Forward scattering width (k0a0<<1, k1a0<<1), phi = pi.
                      Formula 4.1-106
                 */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f41106_zmm16r4(const __m512 a0,
                                             const __m512 a1,
                                             const __m512 k0a0,
                                             const __m512 mu1r,
                                             const __m512 mu1i,
                                             const __m512 mu0r,
                                             const __m512 mu0i,
                                             const __m512 eps1r,
                                             const __m512 eps1i,
                                             const __m512 eps0r,
                                             const __m512 eps0i) FUNC_ATTRIBUTES;


                  
                   __m512 rcs_f41106_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa0,
                                               const float * __restrict __ATTR_ALIGN__(64) pa1,
                                               const float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                               const float * __restrict __ATTR_ALIGN__(64) pmu1r,
                                               const float * __restrict __ATTR_ALIGN__(64) pmu1i,
                                               const float * __restrict __ATTR_ALIGN__(64) pmu0r,
                                               const float * __restrict __ATTR_ALIGN__(64) pmu0i,
                                               const float * __restrict __ATTR_ALIGN__(64) peps1r,
                                               const float * __restrict __ATTR_ALIGN__(64) peps1i,
                                               const float * __restrict __ATTR_ALIGN__(64) peps0r,
                                               const float * __restrict __ATTR_ALIGN__(64) peps0i) FUNC_ATTRIBUTES;


                 /*
                       Hollow cylindrical shell.
                       Approximations for the low frequency region
                       (k0a0<<1, k1a0<<1).
                       Formula 4.1-124
                  */


                
                   void A0_f41124_zmm16r4(const __m512 a1,
                                          const __m512 a0,
                                          const __m512 k0a0,
                                          const __m512 eps1r,
                                          const __m512 eps1i,
                                          const __m512 eps0r,
                                          const __m512 eps0i,
                                          __m512 * __restrict A0r,
                                          __m512 * __restrict A0i) FUNC_ATTRIBUTES;


                   void A0_f41124_zmm16r4_a(const  float * __restrict __ATTR_ALIGN__(64) pa1,
                                            const  float * __restrict __ATTR_ALIGN__(64) pa0,
                                            const  float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                            const  float * __restrict __ATTR_ALIGN__(64) peps1r,
                                            const  float * __restrict __ATTR_ALIGN__(64) peps1i,
                                            const  float * __restrict __ATTR_ALIGN__(64) peps0r,
                                            const  float * __restrict __ATTR_ALIGN__(64) peps0i,
                                            float * __restrict __ATTR_ALIGN__(64) A0r,
                                            float * __restrict __ATTR_ALIGN__(64) A0i) FUNC_ATTRIBUTES;

               
                   void A0_f41124_zmm16r4_u(const  float * __restrict  pa1,
                                          const  float * __restrict pa0,
                                          const  float * __restrict  pk0a0,
                                          const  float * __restrict  peps1r,
                                          const  float * __restrict  peps1i,
                                          const  float * __restrict  peps0r,
                                          const  float * __restrict  peps0i,
                                          float * __restrict  A0r,
                                          float * __restrict  A0i) FUNC_ATTRIBUTES;

                  /*

                       Hollow cylindrical shell.
                       Approximations for the low frequency region
                       (k0a0<<1, k1a0<<1).
                       Formula 4.1-126
                   */


                
                   void B0_f41126_zmm16r4(const __m512 a1,
                                          const __m512 a0,
                                          const __m512 k0a0,
                                          const __m512 mu1r,
                                          const __m512 mu1i,
                                          const __m512 mu0r,
                                          const __m512 mu0i,
                                          __m512 * __restrict B0r,
                                          __m512 * __restrict B0i) FUNC_ATTRIBUTES;


                
                   void B0_f41126_zmm16r4_a(const  float * __restrict __ATTR_ALIGN__(64) pa1,
                                            const  float * __restrict __ATTR_ALIGN__(64) pa0,
                                            const  float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                            const  float * __restrict __ATTR_ALIGN__(64) pmu1r,
                                            const  float * __restrict __ATTR_ALIGN__(64) pmu1i,
                                            const  float * __restrict __ATTR_ALIGN__(64) pmu0r,
                                            const  float * __restrict __ATTR_ALIGN__(64) pmu0i,
                                            float * __restrict __ATTR_ALIGN__(64) B0r,
                                            float * __restrict __ATTR_ALIGN__(64) B0i) FUNC_ATTRIBUTES;


                 
                   void B0_f41126_zmm16r4_u(const  float * __restrict  pa1,
                                            const  float * __restrict  pa0,
                                            const  float * __restrict  pk0a0,
                                            const  float * __restrict  mu1r,
                                            const  float * __restrict  mu1i,
                                            const  float * __restrict  mu0r,
                                            const  float * __restrict  mu0i,
                                            float * __restrict  B0r,
                                            float * __restrict  B0i) FUNC_ATTRIBUTES;


                  /*

                          Hollow cylindrical shell.
                          Approximations for the low frequency region
                          (k0a0<<1, k1a0<<1).
                           Formula 4.1-125
                    */

                
                   void A1_f41125_zmm16r4(const __m512 a1,
                                          const __m512 a0,
                                          const __m512 k0a0,
                                          const __m512 mu1r,
                                          const __m512 mu1i,
                                          const __m512 mu0r,
                                          const __m512 mu0i,
                                          __m512 * __restrict A1r,
                                          __m512 * __restrict A1i) FUNC_ATTRIBUTES;

                
                   void A1_f41125_zmm16r4_a(const  float * __restrict __ATTR_ALIGN__(64) pa1,
                                            const  float * __restrict __ATTR_ALIGN__(64) pa0,
                                            const  float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                            const  float * __restrict __ATTR_ALIGN__(64) pmu1r,
                                            const  float * __restrict __ATTR_ALIGN__(64) pmu1i,
                                            const  float * __restrict __ATTR_ALIGN__(64) pmu0r,
                                            const  float * __restrict __ATTR_ALIGN__(64) pmu0i,
                                            float * __restrict __ATTR_ALIGN__(64) A1r,
                                            float * __restrict __ATTR_ALIGN__(64) A1i) FUNC_ATTRIBUTES;

                
                   void A1_f41125_zmm16r4_u(const  float * __restrict  pa1,
                                            const  float * __restrict  pa0,
                                            const  float * __restrict  pk0a0,
                                            const  float * __restrict  pmu1r,
                                            const  float * __restrict  pmu1i,
                                            const  float * __restrict  pmu0r,
                                            const  float * __restrict  pmu0i,
                                            float * __restrict  A1r,
                                            float * __restrict A1i) FUNC_ATTRIBUTES;

                 /*

                          Hollow cylindrical shell.
                          Approximations for the low frequency region
                          (k0a0<<1, k1a0<<1).
                           Formula 4.1-127
                    */


               
                   void B1_f41127_zmm16r4(const __m512 a1,
                                          const __m512 a0,
                                          const __m512 k0a0,
                                          const __m512 eps1r,
                                          const __m512 eps1i,
                                          const __m512 eps0r,
                                          const __m512 eps0i,
                                          __m512 * __restrict B1r,
                                          __m512 * __restrict B1i) FUNC_ATTRIBUTES;


                
                   void B1_f41127_zmm16r4_a(const  float * __restrict __ATTR_ALIGN__(64) pa1,
                                            const  float * __restrict __ATTR_ALIGN__(64) pa0,
                                            const  float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                            const  float * __restrict __ATTR_ALIGN__(64) peps1r,
                                            const  float * __restrict __ATTR_ALIGN__(64) peps1i,
                                            const  float * __restrict __ATTR_ALIGN__(64) peps0r,
                                            const  float * __restrict __ATTR_ALIGN__(64) peps0i,
                                            float * __restrict __ATTR_ALIGN__(64) B1r,
                                            float * __restrict __ATTR_ALIGN__(64) B1i) FUNC_ATTRIBUTES;

                 
                   void B1_f41127_zmm16r4_u(const  float * __restrict  pa1,
                                            const  float * __restrict  pa0,
                                            const  float * __restrict  pk0a0,
                                            const  float * __restrict  peps1r,
                                            const  float * __restrict  peps1i,
                                            const  float * __restrict  peps0r,
                                            const  float * __restrict  peps0i,
                                            float * __restrict  B1r,
                                            float * __restrict  B1i) FUNC_ATTRIBUTES;

                    /*

                          Low-frequncy approximations (k0a<0.2)
                          Cylindrical Luneberg lens (k0a<0.2).
                          Formula 4.1-162
                     */

                    

                
                   void A0_f41162_zmm16r4(const __m512 k0a,
                                          __m512 * __restrict A0r,
                                          __m512 * __restrict A0i) FUNC_ATTRIBUTES;


                
                   void A0_f41162_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                            float * __restrict __ATTR_ALIGN__(64) A0r,
                                            float * __restrict __ATTR_ALIGN__(64) A0i) FUNC_ATTRIBUTES;

                
                   void A0_f41162_zmm16r4_u(const float * __restrict  pk0a,
                                            float * __restrict  A0r,
                                            float * __restrict  A0i) FUNC_ATTRIBUTES;


                 

                 
                   void B0_f41162_zmm16r4(__m512 * __restrict B0r,
                                          __m512 * __restrict B0i) FUNC_ATTRIBUTES;


                 
                   void B0_f41162_zmm16r4_a(float * __restrict __ATTR_ALIGN__(64) B0r,
                                          float * __restrict __ATTR_ALIGN__(64) B0i) FUNC_ATTRIBUTES;


                 
                   void B0_f41162_zmm16r4_u(float * __restrict  B0r,
                                          float * __restrict  B0i) FUNC_ATTRIBUTES;


                 
                   void A1_f41162_zmm16r4(__m512 * __restrict A1r,
                                          __m512 * __restrict A1i) FUNC_ATTRIBUTES;


                
                   void A1_f41162_zmm16r4_a(float * __restrict __ATTR_ALIGN__(64) A1r,
                                          float * __restrict __ATTR_ALIGN__(64) A1i) FUNC_ATTRIBUTES;


                
                   void A1_f41162_zmm16r4_u(float * __restrict  A1r,
                                            float * __restrict  A1i) FUNC_ATTRIBUTES;


                 
                   void B1_f41162_zmm16r4(const __m512 k0a,
                                          __m512 * __restrict B1r,
                                          __m512 * __restrict B1i) FUNC_ATTRIBUTES;


                
                   void B1_f41162_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                            float * __restrict __ATTR_ALIGN__(64) B1r,
                                            float * __restrict __ATTR_ALIGN__(64) B1i) FUNC_ATTRIBUTES;


                 
                   void B1_f41162_zmm16r4_u(const float * __restrict  pk0a,
                                            float * __restrict  B1r,
                                            float * __restrict  B1i) FUNC_ATTRIBUTES;


                   /*
                          Low-frequncy approximations (k0a<0.2)
                          Cylindrical Luneberg lens (k0a<0.2).  
                          Scattering widths.
                          Formula 4.1-163
                      */

                
                   __m512 rcs_f41163_zmm16r4(const __m512 a,
                                             const __m512 k0a) FUNC_ATTRIBUTES;


              
                   __m512 rcs_f41163_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f41163_zmm16r4_u(const float * __restrict  pa,
                                             const float * __restrict  pk0a) FUNC_ATTRIBUTES;


                    /*
                          Low-frequncy approximations (k0a<0.2)
                          Cylindrical Luneberg lens (k0a<0.2).  
                          Scattering widths.
                          Formula 4.1-164
                      */


                  
                   __m512 rcs_f41164_zmm16r4(const __m512 a,
                                             const __m512 k0a,
                                             const __m512 phi) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f41164_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                               const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                               const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f41164_zmm16r4_u(const float * __restrict  pa,
                                               const float * __restrict  pk0a,
                                               const float * __restrict  pphi) FUNC_ATTRIBUTES;


                  /*

                      Cylindrical Eaton-Lippman Lens, (k0a<0.2)
                      Formulae 4.1-165
                  */


                
                   void A0_f41165_zmm16r4(const __m512 k0a,
                                          __m512 * __restrict A0r,
                                          __m512 * __restrict A0i) FUNC_ATTRIBUTES;


              
                   void A0_f41165_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                            float * __restrict __ATTR_ALIGN__(64) A0r,
                                            float * __restrict __ATTR_ALIGN__(64) A0i) FUNC_ATTRIBUTES;


                
                   void A0_f41165_zmm16r4_u(const float * __restrict  pk0a,
                                            float * __restrict  A0r,
                                            float * __restrict  A0i) FUNC_ATTRIBUTES;

                 
                   void A1_f41165_zmm16r4(__m512 * __restrict A0r,
                                          __m512 * __restrict A0i) FUNC_ATTRIBUTES;


                  
               
                   void A1_f41165_zmm16r4_a(float * __restrict __ATTR_ALIGN__(64) A0r,
                                            float * __restrict __ATTR_ALIGN__(64) A0i) FUNC_ATTRIBUTES;

                 
                   void A1_f41165_zmm16r4_u(float * __restrict  A0r,
                                            float * __restrict  A0i) FUNC_ATTRIBUTES;

               
                   void B0_f41165_zmm16r4(__m512 * __restrict B0r,
                                          __m512 * __restrict B0i) FUNC_ATTRIBUTES;


                  
               
                   void B0_f41165_zmm16r4_a(float * __restrict __ATTR_ALIGN__(64) B0r,
                                            float * __restrict __ATTR_ALIGN__(64) B0i) FUNC_ATTRIBUTES;


                
                   void B0_f41165_zmm16r4_u(float * __restrict  B0r,
                                            float * __restrict  B0i) FUNC_ATTRIBUTES;


             
                   void B1_f41165_zmm16r4(const __m512 k0a,
                                          __m512 * __restrict B1r,
                                          __m512 * __restrict B1i) FUNC_ATTRIBUTES;


              
                   void B1_f41165_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                          float * __restrict __ATTR_ALIGN__(64) B1r,
                                          float * __restrict __ATTR_ALIGN__(64) B1i) FUNC_ATTRIBUTES;


                 
                   void B1_f41165_zmm16r4_u(const float * __restrict  pk0a,
                                          float * __restrict  B1r,
                                          float * __restrict  B1i) FUNC_ATTRIBUTES;


                  /*

                       Cylindrical Eaton-Lippman Lens, (k0a<0.2) 
                       Scattering widths.
                       Formula: 1.4-166
                   */

                  
                   __m512 rcs_f14166_zmm16r4(const __m512 a,
                                             const __m512 k0a) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f14166_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                               const float * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


             
                   __m512 rcs_f14166_zmm16r4_u(const float * __restrict pa,
                                               const float * __restrict  pk0a) FUNC_ATTRIBUTES;


                  /*

                       Cylindrical Eaton-Lippman Lens, (k0a<0.2) 
                       Scattering widths.
                       Formula: 1.4-167
                   */
                 
 
               
                   __m512 rcs_f14167_zmm16r4(const __m512 a,
                                             const __m512 k0a,
                                             const __m512 phi) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f14167_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                               const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                               const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f14167_zmm16r4_u(const float * __restrict  pa,
                                               const float * __restrict  pk0a,
                                               const float * __restrict  pphi) FUNC_ATTRIBUTES;



                  /*

                        Infinitely long cylinder.
                        Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                        TM-incident E-field.
                        Formula 4.2-48
                    */


                
                   void Ez_f4248_zmm16r4(const __m512 E0r,
                                         const __m512 E0i,
                                         const __m512 psi,
                                         const __m512 phi,
                                         const __m512 k0,
                                         const __m512 z,
                                         const __m512 r,
                                         const __m512 a0,
                                         const __m512 epsr,
                                         const __m512 epsi,
                                         const __m512 mur,
                                         const __m512 mui,
                                         __m512 * __restrict Ezr,
                                         __m512 * __restrict Ezi) FUNC_ATTRIBUTES;


               
                   void Ez_f4248_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pE0r,
                                           const float * __restrict __ATTR_ALIGN__(64) pE0i,
                                           const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pz,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) pa0,
                                           const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                           const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                           const float * __restrict __ATTR_ALIGN__(64) pmur,
                                           const float * __restrict __ATTR_ALIGN__(64) pmui,
                                           float * __restrict __ATTR_ALIGN__(64) Ezr,
                                           float * __restrict __ATTR_ALIGN__(64) Ezi) FUNC_ATTRIBUTES;


                
                   void Ez_f4248_zmm16r4_u(const float * __restrict  pE0r,
                                           const float * __restrict  pE0i,
                                           const float * __restrict  ppsi,
                                           const float * __restrict  pphi,
                                           const float * __restrict  pk0,
                                           const float * __restrict  pz,
                                           const float * __restrict  pr,
                                           const float * __restrict  pa0,
                                           const float * __restrict  pepsr,
                                           const float * __restrict  pepsi,
                                           const float * __restrict  pmur,
                                           const float * __restrict  pmui,
                                           float * __restrict  Ezr,
                                           float * __restrict  Ezi) FUNC_ATTRIBUTES;


               

                       /*

                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TM-incident H-field.
                         Formula 4.2-51
                    */

                   void Hp_f4251_zmm16r4(const __m512 E0r,
                                         const __m512 E0i,
                                         const __m512 psi,
                                         const __m512 phi,
                                         const __m512 k0,
                                         const __m512 z,
                                         const __m512 r,
                                         const __m512 a0,
                                         const __m512 epsr,
                                         const __m512 epsi,
                                         const __m512 mur,
                                         const __m512 mui,
                                         __m512 * __restrict Hpr,
                                         __m512 * __restrict Hpi) FUNC_ATTRIBUTES;


                   void Hp_f4251_zmm16r4_a(  const float * __restrict __ATTR_ALIGN__(64) pE0r,
                                           const float * __restrict __ATTR_ALIGN__(64) pE0i,
                                           const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pz,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) pa0,
                                           const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                           const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                           const float * __restrict __ATTR_ALIGN__(64) pmur,
                                           const float * __restrict __ATTR_ALIGN__(64) pmui,
                                           float * __restrict __ATTR_ALIGN__(64) Hpr,
                                           float * __restrict __ATTR_ALIGN__(64) Hpi) FUNC_ATTRIBUTES;


                
                   void Hp_f4251_zmm16r4_u(  const float * __restrict  pE0r,
                                           const float * __restrict  pE0i,
                                           const float * __restrict  ppsi,
                                           const float * __restrict pphi,
                                           const float * __restrict  pk0,
                                           const float * __restrict  pz,
                                           const float * __restrict  pr,
                                           const float * __restrict  pa0,
                                           const float * __restrict  pepsr,
                                           const float * __restrict  pepsi,
                                           const float * __restrict  pmur,
                                           const float * __restrict  pmui,
                                           float * __restrict  Hpr,
                                           float * __restrict  Hpi) FUNC_ATTRIBUTES;


               
                     /*

                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TM-incident E-field.
                         Formula 4.2-49
                    */

                  
                   void Eph_f4249_zmm16r4(const __m512 E0r,
                                          const __m512 E0i,
                                          const __m512 k0z,
                                          const __m512 k0r,
                                          const __m512 k0a0,
                                          const __m512 psi,
                                          const __m512 phi,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          __m512 * __restrict Ephr,
                                          __m512 * __restrict Ephi) FUNC_ATTRIBUTES;

                   


                 
                   void Eph_f4249_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pE0r,
                                          const float * __restrict __ATTR_ALIGN__(64) pE0i,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0z,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0r,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                          const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          float * __restrict __ATTR_ALIGN__(64) Ephr,
                                          float * __restrict __ATTR_ALIGN__(64) Ephi) FUNC_ATTRIBUTES;


                 
                   void Eph_f4249_zmm16r4_u(const float * __restrict pE0r,
                                          const float * __restrict  pE0i,
                                          const float * __restrict pk0z,
                                          const float * __restrict  pk0r,
                                          const float * __restrict  pk0a0,
                                          const float * __restrict  ppsi,
                                          const float * __restrict  pphi,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict  pmur,
                                          const float * __restrict  pmui,
                                          float * __restrict Ephr,
                                          float * __restrict  Ephi) FUNC_ATTRIBUTES;



                 /*
                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TM-incident H-field.
                         Formula 4.2-50

                  */


                 
                   void Hz_f4250_zmm16r4( const __m512 E0r,
                                          const __m512 E0i,
                                          const __m512 k0z,
                                          const __m512 k0r,
                                          const __m512 k0a0,
                                          const __m512 psi,
                                          const __m512 phi,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          __m512 * __restrict Hzr,
                                          __m512 * __restrict Hzi) FUNC_ATTRIBUTES;


                  
                  
                   void Hz_f4250_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pE0r,
                                          const float * __restrict __ATTR_ALIGN__(64) pE0i,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0z,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0r,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                          const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          float * __restrict __ATTR_ALIGN__(64) Hzr,
                                          float * __restrict __ATTR_ALIGN__(64) Hzi ) FUNC_ATTRIBUTES;


               
                   void Hz_f4250_zmm16r4_u(const float * __restrict  pE0r,
                                          const float * __restrict pE0i,
                                          const float * __restrict  pk0z,
                                          const float * __restrict  pk0r,
                                          const float * __restrict  pk0a0,
                                          const float * __restrict  ppsi,
                                          const float * __restrict  pphi,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict  pmur,
                                          const float * __restrict  pmui,
                                          float * __restrict  Hzr,
                                          float * __restrict  Hzi ) FUNC_ATTRIBUTES;


                 /*
                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TE-incident H-field.
                         Formula 4.2-52

                   */


               
                   void Hz_f4252_zmm16r4(const __m512 H0r,
                                         const __m512 H0i,
                                         const __m512 psi,
                                         const __m512 phi,
                                         const __m512 k0r,
                                         const __m512 k0z,
                                         const __m512 k0a0,
                                         const __m512 epsr,
                                         const __m512 epsi,
                                         const __m512 mur,
                                         const __m512 mui,
                                         __m512 * __restrict Hzr,
                                         __m512 * __restrict Hzi) FUNC_ATTRIBUTES;


               
                   void Hz_f4252_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pH0r,
                                          const float * __restrict __ATTR_ALIGN__(64) pH0i,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0z,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0r,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                          const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          float * __restrict __ATTR_ALIGN__(64) Hzr,
                                          float * __restrict __ATTR_ALIGN__(64) Hzi) FUNC_ATTRIBUTES;


              
                   void Hz_f4252_zmm16r4_u(const float * __restrict  pH0r,
                                          const float * __restrict  pH0i,
                                          const float * __restrict  pk0z,
                                          const float * __restrict  pk0r,
                                          const float * __restrict  pk0a0,
                                          const float * __restrict  ppsi,
                                          const float * __restrict  pphi,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict pmur,
                                          const float * __restrict  pmui,
                                          float * __restrict  Hzr,
                                          float * __restrict Hzi) FUNC_ATTRIBUTES;


                    /*
                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TE-incident E-field.
                         Formula 4.2-55

                   */



                   void Eph_f4255_zmm16r4(const __m512 H0r,
                                         const __m512 H0i,
                                         const __m512 psi,
                                         const __m512 phi,
                                         const __m512 k0r,
                                         const __m512 k0z,
                                         const __m512 k0a0,
                                         const __m512 epsr,
                                         const __m512 epsi,
                                         const __m512 mur,
                                         const __m512 mui,
                                         __m512 * __restrict Epr,
                                         __m512 * __restrict Epi) FUNC_ATTRIBUTES;



                 
                   void Eph_f4255_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pH0r,
                                          const float * __restrict __ATTR_ALIGN__(64) pH0i,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0z,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0r,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                          const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          float * __restrict __ATTR_ALIGN__(64) Epr,
                                          float * __restrict __ATTR_ALIGN__(64) Epi) FUNC_ATTRIBUTES;


               
                   void Eph_f4255_zmm16r4_u(const float * __restrict pH0r,
                                          const float * __restrict  pH0i,
                                          const float * __restrict  pk0z,
                                          const float * __restrict  pk0r,
                                          const float * __restrict  pk0a0,
                                          const float * __restrict  ppsi,
                                          const float * __restrict  pphi,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict  pmur,
                                          const float * __restrict  pmui,
                                          float * __restrict  Epr,
                                          float * __restrict  Epi) FUNC_ATTRIBUTES;


                    /*
                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TE-incident H-field.
                         Formula 4.2-53

                   */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void Hph_f4253_zmm16r4(const __m512 H0r,
                                          const __m512 H0i,
                                          const __m512 k0z,
                                          const __m512 k0r,
                                          const __m512 psi,
                                          const __m512 phi,
                                          const __m512 k0a0,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          __m512 * __restrict Hpr,
                                          __m512 * __restrict Hpi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void Hph_f4253_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pH0r,
                                          const float * __restrict __ATTR_ALIGN__(64) pH0i,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0z,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0r,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                          const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          float * __restrict __ATTR_ALIGN__(64) Hpr,
                                          float * __restrict __ATTR_ALIGN__(64) Hpi) FUNC_ATTRIBUTES;

                     __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void Hph_f4253_zmm16r4_u(const float * __restrict  pH0r,
                                          const float * __restrict pH0i,
                                          const float * __restrict  pk0z,
                                          const float * __restrict  pk0r,
                                          const float * __restrict  pk0a0,
                                          const float * __restrict  ppsi,
                                          const float * __restrict  pphi,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict  pmur,
                                          const float * __restrict  pmui,
                                          float * __restrict  Hpr,
                                          float * __restrict  Hpi) FUNC_ATTRIBUTES;

                   /*
                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TE-incident E-field.
                         Formula 4.2-54

                   */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void Ez_f4254_zmm16r4( const __m512 H0r,
                                          const __m512 H0i,
                                          const __m512 k0z,
                                          const __m512 k0r,
                                          const __m512 psi,
                                          const __m512 phi,
                                          const __m512 k0a0,
                                          const __m512 epsr,
                                          const __m512 epsi,
                                          const __m512 mur,
                                          const __m512 mui,
                                          __m512 * __restrict Ezr,
                                          __m512 * __restrict Ezi) FUNC_ATTRIBUTES;


                    __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void Ez_f4254_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pH0r,
                                          const float * __restrict __ATTR_ALIGN__(64) pH0i,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0z,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0r,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                          const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui,
                                          float * __restrict __ATTR_ALIGN__(64) Hzr,
                                          float * __restrict __ATTR_ALIGN__(64) Hzi ) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void Ez_f4254_zmm16r4_u(const float * __restrict  pH0r,
                                          const float * __restrict pH0i,
                                          const float * __restrict  pk0z,
                                          const float * __restrict  pk0r,
                                          const float * __restrict  pk0a0,
                                          const float * __restrict  ppsi,
                                          const float * __restrict  pphi,
                                          const float * __restrict  pepsr,
                                          const float * __restrict  pepsi,
                                          const float * __restrict  pmur,
                                          const float * __restrict  pmui,
                                          float * __restrict  Hzr,
                                          float * __restrict  Hzi ) FUNC_ATTRIBUTES;


                  /*
                     Bistatic scattering width for (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                     Infinitely long cylinder.
                     TM-incident.
                     Formula 4.2-56
                 */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4256_zmm16r4(const __m512 a0,
                                            const __m512 k0a0,
                                            const __m512 psi,
                                            const __m512 phi,
                                            const __m512 epsr,
                                            const __m512 epsi,
                                            const __m512 mur,
                                            const __m512 mui) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4256_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa0,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const float * __restrict __ATTR_ALIGN__(64) pmur,
                                              const float * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4256_zmm16r4_u(const float * __restrict  pa0,
                                              const float * __restrict  pk0a0,
                                              const float * __restrict  ppsi,
                                              const float * __restrict  pphi,
                                              const float * __restrict  pepsr,
                                              const float * __restrict  pepsi,
                                              const float * __restrict  pmur,
                                              const float * __restrict  pmui) FUNC_ATTRIBUTES;

                  /*
                     Bistatic scattering width for (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                     Infinitely long cylinder.
                     TE-incident.
                     Formula 4.2-58
                 */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4258_zmm16r4(const __m512 a0,
                                            const __m512 k0a0,
                                            const __m512 psi,
                                            const __m512 phi,
                                            const __m512 epsr,
                                            const __m512 epsi,
                                            const __m512 mur,
                                            const __m512 mui) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4258_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa0,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const float * __restrict __ATTR_ALIGN__(64) pmur,
                                              const float * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;

                    __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4258_zmm16r4_u(const float * __restrict  pa0,
                                              const float * __restrict  pk0a0,
                                              const float * __restrict  ppsi,
                                              const float * __restrict  pphi,
                                              const float * __restrict  pepsr,
                                              const float * __restrict  pepsi,
                                              const float * __restrict  pmur,
                                              const float * __restrict  pmui) FUNC_ATTRIBUTES;

                   /*

                           Bistatic scattering width for (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                           Infinitely long cylinder.
                           TM-incident.
                           Formula 4.2-57
                     */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4257_zmm16r4(const __m512 a0,
                                            const __m512 k0a0,
                                            const __m512 psi,
                                            const __m512 phi,
                                            const __m512 epsr,
                                            const __m512 epsi,
                                            const __m512 mur,
                                            const __m512 mui) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4257_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa0,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a0,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const float * __restrict __ATTR_ALIGN__(64) pmur,
                                              const float * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4257_zmm16r4_u(const float * __restrict  pa0,
                                              const float * __restrict  pk0a0,
                                              const float * __restrict  ppsi,
                                              const float * __restrict  pphi,
                                              const float * __restrict  pepsr,
                                              const float * __restrict  pepsi,
                                              const float * __restrict  pmur,
                                              const float * __restrict  pmui) FUNC_ATTRIBUTES;


                   /*
                       Circular cylinders of finite length.
                       Cylinder radius small (k0a<1.0)
                       Wire limit of cylinder (h>>a).
                       E-field
                       Formula 4.3-9
                   */

                    
                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f439_zmm16r4(const __m512 EIr,
                                        const __m512 EIi,
                                        const __m512 r,
                                        const __m512 k0,
                                        const __m512 psii,
                                        const __m512 psis,
                                        const __m512 h,
                                        const __m512 ln4ha,
                                        __m512 * __restrict ESr,
                                        __m512 * __restrict ESi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f439_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pEIr,
                                          const float * __restrict __ATTR_ALIGN__(64) pEIi,
                                          const float * __restrict __ATTR_ALIGN__(64) pr,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0,
                                          const float * __restrict __ATTR_ALIGN__(64) ppsii,
                                          const float * __restrict __ATTR_ALIGN__(64) ppsis,
                                          const float * __restrict __ATTR_ALIGN__(64) ph,
                                          const float * __restrict __ATTR_ALIGN__(64) pln4ha,
                                          float * __restrict __ATTR_ALIGN__(64) ESr,
                                          float * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f439_zmm16r4_u(const float * __restrict  pEIr,
                                          const float * __restrict  pEIi,
                                          const float * __restrict  pr,
                                          const float * __restrict  pk0,
                                          const float * __restrict  ppsii,
                                          const float * __restrict  ppsis,
                                          const float * __restrict  ph,
                                          const float * __restrict  pln4ha,
                                          float * __restrict  ESr,
                                          float * __restrict  ESi) FUNC_ATTRIBUTES;


                  /*
                       Circular cylinders of finite length.
                       Cylinder radius small (k0a<1.0)
                       Wire limit of cylinder (h>>a).
                       RCS.
                       Formula 4.3-10

                    */
                  
                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4310_zmm16r4(const __m512 k0,
                                            const __m512 h,
                                            const __m512 psii,
                                            const __m512 psis,
                                            const __m512 ln4h) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4310_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  pk0,
                                              const float * __restrict __ATTR_ALIGN__(64)  ph,
                                              const float * __restrict __ATTR_ALIGN__(64)  ppsii,
                                              const float * __restrict __ATTR_ALIGN__(64)  ppsis,
                                              const float * __restrict __ATTR_ALIGN__(64)  pln4h) FUNC_ATTRIBUTES;


                    __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4310_zmm16r4_u(const float * __restrict   pk0,
                                              const float * __restrict   ph,
                                              const float * __restrict   ppsii,
                                              const float * __restrict   ppsis,
                                              const float * __restrict   pln4h) FUNC_ATTRIBUTES;


                  /*
                         The average dipole scattering RCS when the incidence
                         and scattered polarization direction coincide.
                         Formula 4.3-11
                    */

                   
                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4311_zmm16r4(const __m512 k0,
                                            const __m512 h,
                                            const __m512 ln4h) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4311_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) ph,
                                              const float * __restrict __ATTR_ALIGN__(64) pln4h) FUNC_ATTRIBUTES;


                  __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4311_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  ph,
                                              const float * __restrict  pln4h) FUNC_ATTRIBUTES;


                  /*
                           Disc limit of cylinder (h<<a).
                           Scattered fields from the cylinder in the disc limit
                           Formula 4.3-18
                      */

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f4318_zmm16r4(const __m512 EIr,
                                         const __m512 EIi,
                                         const __m512 k0,
                                         const __m512 r,
                                         const __m512 psii,
                                         const __m512 psis,
                                         const __m512 phi,
                                         const __m512 a,
                                         __m512 * __restrict ESr,
                                         __m512 * __restrict ESi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f4318_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  pEIr,
                                           const float * __restrict __ATTR_ALIGN__(64)  pEIi,
                                           const float * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const float * __restrict __ATTR_ALIGN__(64)  pr,
                                           const float * __restrict __ATTR_ALIGN__(64)  ppsii,
                                           const float * __restrict __ATTR_ALIGN__(64)  ppsis,
                                           const float * __restrict __ATTR_ALIGN__(64)  pphi,
                                           const float * __restrict __ATTR_ALIGN__(64)  pa,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64)  ESi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f4318_zmm16r4_u(const float * __restrict   pEIr,
                                           const float * __restrict   pEIi,
                                           const float * __restrict   pk0,
                                           const float * __restrict   pr,
                                           const float * __restrict   ppsii,
                                           const float * __restrict   ppsis,
                                           const float * __restrict   pphi,
                                           const float * __restrict   pa,
                                           float * __restrict  ESr,
                                           float * __restrict   ESi) FUNC_ATTRIBUTES;

                   /*
                           Disc limit of cylinder (h<<a).
                           Scattered fields from the cylinder in the disc limit
                           Formula 4.3-19
                      */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f4319_zmm16r4(const __m512 EIr,
                                         const __m512 EIi,
                                         const __m512 k0,
                                         const __m512 r,
                                         const __m512 psii,
                                         const __m512 phi,
                                         const __m512 a,
                                         __m512 * __restrict ESr,
                                         __m512 * __restrict ESi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f4319_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  pEIr,
                                           const float * __restrict __ATTR_ALIGN__(64)  pEIi,
                                           const float * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const float * __restrict __ATTR_ALIGN__(64)  pr,
                                           const float * __restrict __ATTR_ALIGN__(64)  ppsii,
                                           const float * __restrict __ATTR_ALIGN__(64)  pphi,
                                           const float * __restrict __ATTR_ALIGN__(64)  pa,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64)  ESi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f4319_zmm16r4_u(const float * __restrict   pEIr,
                                           const float * __restrict   pEIi,
                                           const float * __restrict   pk0,
                                           const float * __restrict   pr,
                                           const float * __restrict   ppsii,
                                           const float * __restrict   pphi,
                                           const float * __restrict   pa,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi) FUNC_ATTRIBUTES;


                 /*
                           Disc limit of cylinder (h<<a).
                           Scattered fields from the cylinder in the disc limit
                           Formula 4.3-20
                   */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f4320_zmm16r4(const __m512 EIr,
                                         const __m512 EIi,
                                         const __m512 k0,
                                         const __m512 r,
                                         const __m512 psis,
                                         const __m512 phi,
                                         const __m512 a,
                                         __m512 * __restrict ESr,
                                         __m512 * __restrict ESi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f4320_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  pEIr,
                                           const float * __restrict __ATTR_ALIGN__(64)  pEIi,
                                           const float * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const float * __restrict __ATTR_ALIGN__(64)  pr,
                                           const float * __restrict __ATTR_ALIGN__(64)  ppsis,
                                           const float * __restrict __ATTR_ALIGN__(64)  pphi,
                                           const float * __restrict __ATTR_ALIGN__(64)  pa,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64)  ESi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f4320_zmm16r4_u(const float * __restrict   pEIr,
                                           const float * __restrict   pEIi,
                                           const float * __restrict   pk0,
                                           const float * __restrict   pr,
                                           const float * __restrict   ppsis,
                                           const float * __restrict   pphi,
                                           const float * __restrict   pa,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi) FUNC_ATTRIBUTES;


                   /*
                           Disc limit of cylinder (h<<a).
                           Scattered fields from the cylinder in the disc limit
                           Formula 4.3-21
                   */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f4321_zmm16r4(const __m512 EIr,
                                         const __m512 EIi,
                                         const __m512 k0,
                                         const __m512 r,
                                         const __m512 psii,
                                         const __m512 psis,
                                         const __m512 phi,
                                         const __m512 a,
                                         __m512 * __restrict ESr,
                                         __m512 * __restrict ESi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f4321_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  pEIr,
                                           const float * __restrict __ATTR_ALIGN__(64)  pEIi,
                                           const float * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const float * __restrict __ATTR_ALIGN__(64)  pr,
                                           const float * __restrict __ATTR_ALIGN__(64)  ppsii,
                                           const float * __restrict __ATTR_ALIGN__(64)  ppsis,
                                           const float * __restrict __ATTR_ALIGN__(64)  pphi,
                                           const float * __restrict __ATTR_ALIGN__(64)  pa,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64)  ESi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void ES_f4321_zmm16r4_u(const float * __restrict   pEIr,
                                           const float * __restrict   pEIi,
                                           const float * __restrict   pk0,
                                           const float * __restrict   pr,
                                           const float * __restrict   ppsii,
                                           const float * __restrict   ppsis,
                                           const float * __restrict   pphi,
                                           const float * __restrict   pa,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi) FUNC_ATTRIBUTES;


                 /*
                           Disc limit of cylinder (h<<a).
                           Bistatic scattering RCS for cylinder in the disc limit
                           Formula 4.3-22
                      */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4322_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 psii,
                                            const __m512 psis,
                                            const __m512 phi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4322_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsis,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4322_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  ppsii,
                                              const float * __restrict  ppsis,
                                              const float * __restrict  pphi) FUNC_ATTRIBUTES;

                    /*
                           Disc limit of cylinder (h<<a).
                           Bistatic scattering RCS for cylinder in the disc limit
                           Formula 4.3-23
                      */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4323_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 psii,
                                            const __m512 phi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4323_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4323_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  ppsii,
                                              const float * __restrict  pphi) FUNC_ATTRIBUTES;

                  /*
                           Disc limit of cylinder (h<<a).
                           Bistatic scattering RCS for cylinder in the disc limit
                           Formula 4.3-24
                      */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4324_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 psis,
                                            const __m512 phi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4324_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsis,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4324_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  ppsis,
                                              const float * __restrict  pphi) FUNC_ATTRIBUTES;


                  /*
                           Disc limit of cylinder (h<<a).
                           Bistatic scattering RCS for cylinder in the disc limit
                           Formula 4.3-25
                   */

  
                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4325_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 psii,
                                            const __m512 psis,
                                            const __m512 phi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4325_zmm16r4_a(  const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsis,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                  __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4325_zmm16r4_u(  const float * __restrict  pk0,
                                                const float * __restrict  pa,
                                                const float * __restrict  ppsis,
                                                const float * __restrict  ppsii,
                                                const float * __restrict  pphi) FUNC_ATTRIBUTES;

                   /*
                          Backscattering RCS for perfectly conducting wire.
                          (2*h>gamma/4)
                          Formula 4.3-29

                     */

                     /*
                          Parameter a1,a2,a3 of equation 4.3-29
                          Formula 4.3-30
                      */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 a1_f4330_zmm16r4(const __m512 k0h,
                                           const __m512 psi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 a1_f4330_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const float * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 a1_f4330_zmm16r4_u(const float * __restrict  pk0h,
                                             const float * __restrict  ppsi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 a2_f4330_zmm16r4(const __m512 k0h,
                                           const __m512 psi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 a2_f4330_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const float * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 a2_f4330_zmm16r4_u(const float * __restrict  pk0h,
                                             const float * __restrict  ppsi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 a3_f4330_zmm16r4(const __m512 k0h,
                                           const __m512 psi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 a3_f4330_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const float * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 a3_f4330_zmm16r4_u(const float * __restrict  pk0h,
                                             const float * __restrict  ppsi) FUNC_ATTRIBUTES;


                    /*
                          Parameter F1,F2 of equation 4.3-29
                          Formula 4.3-31
                      */

                    __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 F1_f4331_zmm16r4(const __m512 k0a) FUNC_ATTRIBUTES;


                    __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 F1_f4331_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 F1_f4331_zmm16r4_u(const float * __restrict  pk0a) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 F2_f4331_zmm16r4(const __m512 k0a) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 F2_f4331_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                  __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 F2_f4331_zmm16r4_u(const float * __restrict  pk0a) FUNC_ATTRIBUTES;

                     /*
                          Parameter (helper) Lambda of equation 4.3-29
                          Formula 4.3-34
                      */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 L_f4334_zmm16r4(const __m512 k0h,
                                          const __m512 k0a) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 L_f4334_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 L_f4334_zmm16r4_u(const float * __restrict pk0h,
                                            const float * __restrict pk0a) FUNC_ATTRIBUTES;


                   /*
                          Parameter (helper) Sigma of equation 4.3-29
                          Formula 4.3-35
                      */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 S_f4335_zmm16r4(const __m512 k0a,
                                          const __m512 k0h) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 S_f4335_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0h) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 S_f4335_zmm16r4_u(const float * __restrict  pk0a,
                                            const float * __restrict  pk0h) FUNC_ATTRIBUTES;


                  /*

                           Parameter G1,G2 of equation 4.3-29
                           Formula 4.3-32
                    */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 G2_f4332_zmm16r4(const __m512 k0h,
                                           const __m512 k0a) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 G2_f4332_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 G2_f4332_zmm16r4_u(const float * __restrict  pk0h,
                                             const float * __restrict  pk0a) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 G1_f4332_zmm16r4(const __m512 k0h,
                                           const __m512 k0a) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 G1_f4332_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const float * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 G1_f4332_zmm16r4_u(const float * __restrict  pk0h,
                                             const float * __restrict  pk0a) FUNC_ATTRIBUTES;

                     /*

                           Parameter H1,H2 of equation 4.3-29
                           Formula 4.3-33
                    */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 H2_f4333_zmm16r4(const __m512 k0h,
                                           const __m512 k0a) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 H2_f4333_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const float * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 H2_f4333_zmm16r4_u(const float * __restrict  pk0h,
                                             const float * __restrict  pk0a) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 H1_f4333_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                             const float * __restrict __ATTR_ALIGN__(64) pk0h) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 H1_f4333_zmm16r4_u(const float * __restrict  pk0a,
                                             const float * __restrict  pk0h) FUNC_ATTRIBUTES;


                 /*
                          Backscattering RCS for perfectly conducting wire.
                          (2*h>gamma/4)
                          Formula 4.3-29

                     */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4329_zmm16r4(const __m512 k0,
                                            const __m512 gami,
                                            const __m512 gams,
                                            const __m512 k0h,
                                            const __m512 k0a,
                                            const __m512 psi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4329_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pgami,
                                              const float * __restrict __ATTR_ALIGN__(64) pgams,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                    __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4329_zmm16r4_u(const float * __restrict pk0,
                                              const float * __restrict  pgami,
                                              const float * __restrict  pgams,
                                              const float * __restrict  pk0h,
                                              const float * __restrict  pk0a,
                                              const float * __restrict  ppsi) FUNC_ATTRIBUTES;

                  /*

                         Simplified back and bistatic scattering RCS for
                         half and full-wave dipole (2*h == gam0/2, and gam0)
                         gam0 -- wavelength.
                    */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4337_zmm16r4(const __m512 gammi,
                                            const __m512 gamms,
                                            const __m512 psii,
                                            const __m512 psis,
                                            const __m512 g0 )  FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4337_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgammi,
                                              const float * __restrict __ATTR_ALIGN__(64) pgamms,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsis,
                                              const float * __restrict __ATTR_ALIGN__(64) pg0 )  FUNC_ATTRIBUTES;
      


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4337_zmm16r4_u(const float * __restrict  pgammi,
                                              const float * __restrict  pgamms,
                                              const float * __restrict  ppsii,
                                              const float * __restrict  ppsis,
                                              const float * __restrict  pg0 )  FUNC_ATTRIBUTES;

                   /*

                         Simplified back and bistatic scattering RCS for
                         Full-wave dipole (2*h == gam0)
                         gam0 -- wavelength.
                    */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4340_zmm16r4(const __m512 gammi,
                                            const __m512 gamms,
                                            const __m512 psii,
                                            const __m512 psis,
                                            const __m512 g0 )  FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4340_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgammi,
                                              const float * __restrict __ATTR_ALIGN__(64) pgamms,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsis,
                                              const float * __restrict __ATTR_ALIGN__(64) pg0 )  FUNC_ATTRIBUTES;

                    __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4340_zmm16r4_u(const float * __restrict  pgammi,
                                              const float * __restrict  pgamms,
                                              const float * __restrict  ppsii,
                                              const float * __restrict  ppsis,
                                              const float * __restrict  pg0 ) FUNC_ATTRIBUTES;


                     /*
                           Cylinder length much greater then wavelength (h>>gamma).
                           Biscattering RCS, formula 4.3-43
                      */

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4343_zmm16r4(const __m512 rcs_inf, // rcs of inifnitely long cylinder (section 4.2)
                                            const __m512 k0,
                                            const __m512 h,
                                            const __m512 psis,
                                            const __m512 psii) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4343_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) prcs_inf, // rcs of inifnitely long cylinder (section 4.2)
                                              const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) ph,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsis,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsii) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4343_zmm16r4_u(const float * __restrict  prcs_inf, // rcs of inifnitely long cylinder (section 4.2)
                                              const float * __restrict  pk0,
                                              const float * __restrict  ph,
                                              const float * __restrict  ppsis,
                                              const float * __restrict  ppsii) FUNC_ATTRIBUTES;


                  /*
                         General bistatic scattering RCS from long thin wire.
                         Formula 4.3-44
                    */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4344_zmm16r4(const __m512 h,
                                            const __m512 k0,
                                            const __m512 k0a,
                                            const __m512 psii,
                                            const __m512 psis,
                                            const __m512 gams,
                                            const __m512 gami) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4344_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  ph,
                                              const float * __restrict __ATTR_ALIGN__(64)  pk0,
                                              const float * __restrict __ATTR_ALIGN__(64)  pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64)  ppsii,
                                              const float * __restrict __ATTR_ALIGN__(64)  ppsis,
                                              const float * __restrict __ATTR_ALIGN__(64)  pgams,
                                              const float * __restrict __ATTR_ALIGN__(64)  pgami) FUNC_ATTRIBUTES;



                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4344_zmm16r4_u(const float * __restrict   ph,
                                              const float * __restrict   pk0,
                                              const float * __restrict   pk0a,
                                              const float * __restrict   ppsii,
                                              const float * __restrict   ppsis,
                                              const float * __restrict   pgams,
                                              const float * __restrict   pgami) FUNC_ATTRIBUTES;


                    /*

                          General backscatter (only) scattering RCS from long thin wire.
                          Formula 4.3-45
                     */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4345_zmm16r4(const __m512 psi,
                                            const __m512 k0a,
                                            const __m512 gami,
                                            const __m512 gams,
                                            const __m512 k0,
                                            const __m512 h) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4345_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  ph,
                                              const float * __restrict __ATTR_ALIGN__(64)  pk0,
                                              const float * __restrict __ATTR_ALIGN__(64)  pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64)  pgams,
                                              const float * __restrict __ATTR_ALIGN__(64)  pgami,
                                              const float * __restrict __ATTR_ALIGN__(64)  ppsi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4345_zmm16r4_u(const float * __restrict   ph,
                                              const float * __restrict   pk0,
                                              const float * __restrict   pk0a,
                                              const float * __restrict   pgams,
                                              const float * __restrict   pgami,
                                              const float * __restrict   ppsi) FUNC_ATTRIBUTES;

                  /*
                        Backscattering From a Perfectly Conducting Cylinder With Flat Ends.
                        Helper functions, M1,M2 for the main formula 4.3-48
                        Formula 4.3-50

                   */

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 M1_f4350_zmm16r4(const __m512 psi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 M1_f4350_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 M1_f4350_zmm16r4_u(const float * __restrict  ppsi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 M2_f4350_zmm16r4(const __m512 psi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 M2_f4350_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 M2_f4350_zmm16r4_u(const float * __restrict  ppsi) FUNC_ATTRIBUTES;


                    /*
                        Backscattering From a Perfectly Conducting Cylinder With Flat Ends.
                        Helper functions, M1,M2 for the main formula 4.3-48
                        Formula 4.3-51

                   */

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 N1_f4351_zmm16r4(const __m512 psi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 N1_f4351_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 N1_f4351_zmm16r4_u(const float * __restrict  ppsi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 N2_f4351_zmm16r4(const __m512 psi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 N2_f4351_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 N2_f4351_zmm16r4_u(const float * __restrict  ppsi) FUNC_ATTRIBUTES;


                   /*
                        Backscattering From a Perfectly Conducting Cylinder With Flat Ends.
                        Helper functions, M1,M2 for the main formula 4.3-48
                        Formula 4.3-52

                   */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 G_f4352_zmm16r4(const __m512 psi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 G_f4352_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 G_f4352_zmm16r4_u(const float * __restrict  ppsi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 F_f4352_zmm16r4(const __m512 psi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 F_f4352_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 F_f4352_zmm16r4_u(const float * __restrict ppsi) FUNC_ATTRIBUTES;

                    /*
                           Scattering From Cylinder Near the Specular Direction.
                           Formula 4.3-53
                      */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4353_zmm16r4(const __m512 k0a,
                                            const __m512 k0,
                                            const __m512 h,
                                            const __m512 phi,
                                            const __m512 psii,
                                            const __m512 psis) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4353_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) ph,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsis) FUNC_ATTRIBUTES;


                    __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4353_zmm16r4_u(const float * __restrict  pk0a,
                                              const float * __restrict  pk0,
                                              const float * __restrict  ph,
                                              const float * __restrict  pphi,
                                              const float * __restrict  ppsii,
                                              const float * __restrict  ppsis) FUNC_ATTRIBUTES;


                   /*

                            Specular direction -- RCS.
                            Formula 4.3-54
                       */

                    __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4354_zmm16r4(const __m512 k0a,
                                            const __m512 h,
                                            const __m512 psii,
                                            const __m512 phi) FUNC_ATTRIBUTES;


                     __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4354_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) ph,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4354_zmm16r4_u(const float * __restrict  pk0a,
                                              const float * __restrict  ph,
                                              const float * __restrict  ppsii,
                                              const float * __restrict  pphi) FUNC_ATTRIBUTES;

                  /*

                         Backscattering direction -- RCS for incidence angles
                         near broadside.
                         Formula 4.3-54
                     */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4354_zmm16r4(const __m512 k0a,
                                            const __m512 h,
                                            const __m512 k0,
                                            const __m512 psii) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4354_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) ph,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) ppsii) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4354_zmm16r4_u(const float * __restrict  pk0a,
                                              const float * __restrict  ph,
                                              const float * __restrict  pk0,
                                              const float * __restrict  ppsii) FUNC_ATTRIBUTES;

                 /*

                        Broadside (psi == 0) RCS.
                        Formula 4.3-56
                   */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4356_zmm16r4(const __m512 k0a,
                                            const __m512 h) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4356_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                            const float * __restrict __ATTR_ALIGN__(64) ph) FUNC_ATTRIBUTES;

                  __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4356_zmm16r4_u(const float * __restrict  pk0a,
                                            const float * __restrict  ph) {

                          register __m512  k0a  = _mm512_loadu_ps(&pk0a[0]);
                          register __m512  h    = _mm512_loadu_ps(&ph[0]);
                          const __m512 _4 = _mm512_set1_ps(4.0f);
                          register __m512 rcs,h2;
                          h2 = _mm512_mul_ps(h,h);
                          rcs = _mm512_mul_ps(_4,_mm512_mul_ps(k0a,h2));
                          return (rcs); 
                }


                  /*
                       Elliptical cylinders.
                   */


                   /*
                         Low-frequency approximations (k0a<0.5, k0b<0.5)
                         TM-case,formula 4.4-11
                    */

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TM_f4411_zmm16r4(const __m512 a,
                                         const __m512 b,
                                         const __m512 k0,
                                         __m512 * __restrict TMr,
                                         __m512 * __restrict TMi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TM_f4411_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                         const float * __restrict __ATTR_ALIGN__(64) pb,
                                         const float * __restrict __ATTR_ALIGN__(64) pk0,
                                         float * __restrict __ATTR_ALIGN__(64) TMr,
                                         float * __restrict __ATTR_ALIGN__(64) TMi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TM_f4411_zmm16r4_u(const float * __restrict  pa,
                                           const float * __restrict  pb,
                                           const float * __restrict  pk0,
                                           float * __restrict  TMr,
                                           float * __restrict  TMi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TE_f4412_zmm16r4(const __m512 k0a,
                                         const __m512 a,
                                         const __m512 b,
                                         const __m512 phi1,
                                         const __m512 phi2,
                                         __m512 * __restrict TEr,
                                         __m512 * __restrict TEi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TE_f4412_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) pb,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                           float * __restrict __ATTR_ALIGN__(64) TEr,
                                           float * __restrict __ATTR_ALIGN__(64) TEi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TE_f4412_zmm16r4_u(const float * __restrict  pk0a,
                                           const float * __restrict  pa,
                                           const float * __restrict  pb,
                                           const float * __restrict  pphi1,
                                           const float * __restrict  pphi2,
                                           __m512 * __restrict TEr,
                                           __m512 * __restrict TEi) FUNC_ATTRIBUTES;


                 /*
                       TM-case, RCS.
                       Formula 4.4-13
                  */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4413_zmm16r4(const __m512 a,
                                            const __m512 b,
                                            const __m512 k0) FUNC_ATTRIBUTES;
                                            


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4413_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  pa,
                                              const float * __restrict __ATTR_ALIGN__(64)  pb,
                                              const float * __restrict __ATTR_ALIGN__(64)  pk0) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4413_zmm16r4_u(const float * __restrict   pa,
                                              const float * __restrict   pb,
                                              const float * __restrict   pk0) FUNC_ATTRIBUTES;


                    /*
                         High frequency approximations (k0a>5, k0b>5)
                         TM-case, formula 4.4-15
                      */



                    /*
                        Helper function for testing the condition of high-frequency limit.
                        Page. 322.

                     */

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __mmask16 
                   TM_f4415_helper_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 phi1,
                                           const __m512 phi2,
                                           const __m512 b) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TM_f4415_zmm16r4(const __m512 phi1,
                                         const __m512 phi2,
                                         const __m512 a,
                                         const __m512 b,
                                         const __m512 k0,
                                         __m512 * __restrict TMr,
                                         __m512 * __restrict TMi,
                                         bool & status) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TM_f4415_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                         const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                         const float * __restrict __ATTR_ALIGN__(64) pa,
                                         const float * __restrict __ATTR_ALIGN__(64) pb,
                                         const float * __restrict __ATTR_ALIGN__(64) pk0,
                                         float * __restrict __ATTR_ALIGN__(64) TMr,
                                         float * __restrict __ATTR_ALIGN__(64) TMi,
                                         bool & status) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TM_f4415_zmm16r4_u(const float * __restrict  pphi1,
                                         const float * __restrict  pphi2,
                                         const float * __restrict  pa,
                                         const float * __restrict  pb,
                                         const float * __restrict  pk0,
                                         float * __restrict  TMr,
                                         float * __restrict  TMi,
                                         bool & status) FUNC_ATTRIBUTES;

                    /*
                         High frequency approximations (k0a>5, k0b>5)
                         TE-case, formula 4.4-16
                      */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TE_f4416_zmm16r4(const __m512 phi1,
                                         const __m512 phi2,
                                         const __m512 a,
                                         const __m512 b,
                                         const __m512 k0,
                                         __m512 * __restrict TEr,
                                         __m512 * __restrict TEi,
                                         bool & status) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TE_f4416_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                         const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                         const float * __restrict __ATTR_ALIGN__(64) pa,
                                         const float * __restrict __ATTR_ALIGN__(64) pb,
                                         const float * __restrict __ATTR_ALIGN__(64) pk0,
                                         float * __restrict __ATTR_ALIGN__(64) TEr,
                                         float * __restrict __ATTR_ALIGN__(64) TEi,
                                         bool & status) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TE_f4416_zmm16r4_u(const float * __restrict  pphi1,
                                         const float * __restrict  pphi2,
                                         const float * __restrict  pa,
                                         const float * __restrict  pb,
                                         const float * __restrict  pk0,
                                         float * __restrict  TEr,
                                         float * __restrict  TEi,
                                         bool & status) FUNC_ATTRIBUTES;

                 /*

                        Bistatic scattering width.
                        Formula 4.4-19
                   */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4419_zmm16r4(const __m512 phi1,
                                            const __m512 phi2,
                                            const __m512 a,
                                            const __m512 b) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4419_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb) FUNC_ATTRIBUTES;


                     __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4419_zmm16r4_u(const float * __restrict  pphi1,
                                              const float * __restrict  pphi2,
                                              const float * __restrict  pa,
                                              const float * __restrict  pb) FUNC_ATTRIBUTES;


                   /*

                          Backscattering width, for phi2 == phi1.
                          Formula 4.4-20
                      */


                     
                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4420_zmm16r4(const __m512 a,
                                            const __m512 b,
                                            const __m512 phi) FUNC_ATTRIBUTES;


                    __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4420_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4420_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict  pphi) FUNC_ATTRIBUTES;


                   /*
                        Forward scattering pattern and width.
                        Formula 4.4-23 a scattering amplitude

                    */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __mmask16 
                   T_f4423_helper_zmm16r4( const __m512 k0,
                                           const __m512 a,
                                           const __m512 phi1,
                                           const __m512 phi2,
                                           const __m512 b) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 T_f4423_zmm16r4(const __m512 a,
                                          const __m512 b,
                                          const __m512 phi1,
                                          const __m512 phi2,
                                          const __m512 k0,
                                          bool & status) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 T_f4423_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                          const float * __restrict __ATTR_ALIGN__(64) pb,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                          const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                          const float * __restrict __ATTR_ALIGN__(64) pk0,
                                          bool & status) FUNC_ATTRIBUTES;


                     __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 T_f4423_zmm16r4_u(const float * __restrict pa,
                                          const float * __restrict  pb,
                                          const float * __restrict  pphi1,
                                          const float * __restrict  pphi2,
                                          const float * __restrict  pk0,
                                          bool & status) FUNC_ATTRIBUTES;


                   /*
                          Scattering width near the forward direction.
                          Formula 4.4-24

                     */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4424_zmm16r4(const __m512 a,
                                          const __m512 b,
                                          const __m512 phi1,
                                          const __m512 phi2,
                                          const __m512 k0,
                                          bool & status) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4424_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              bool & status) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4424_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict  pphi1,
                                              const float * __restrict  pphi2,
                                              const float * __restrict  pk0,
                                              bool & status) FUNC_ATTRIBUTES;


                   /*
                         Scattering width in the exact forward direction (alpha == 0).
                         Formula 4.4-25
                     */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4425_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 b,
                                            const __m512 phi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4425_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  pk0,
                                              const float * __restrict __ATTR_ALIGN__(64)  pa,
                                              const float * __restrict __ATTR_ALIGN__(64)  pb,
                                              const float * __restrict __ATTR_ALIGN__(64)  pphi) FUNC_ATTRIBUTES;


                    __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4425_zmm16r4_u(const float * __restrict   pk0,
                                              const float * __restrict   pa,
                                              const float * __restrict   pb,
                                              const float * __restrict  pphi) FUNC_ATTRIBUTES;


                    /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          TM-case, formula 4.4-26
                     */

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TM_f4426_zmm16r4(const __m512 k0,
                                         const __m512 a,
                                         const __m512 b,
                                         const __m512 phi1,
                                         const __m512 phi2,
                                         const __m512 epsr,
                                         const __m512 epsi,
                                         const __m512 mur,
                                         const __m512 mui,
                                         __m512 * __restrict TMr,
                                         __m512 * __restrict TMi) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TM_f4426_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                         const float * __restrict __ATTR_ALIGN__(64) pa,
                                         const float * __restrict __ATTR_ALIGN__(64) pb,
                                         const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                         const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                         const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                         const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                         const float * __restrict __ATTR_ALIGN__(64) pmur,
                                         const float * __restrict __ATTR_ALIGN__(64) pmui,
                                         float * __restrict __ATTR_ALIGN__(64) TMr,
                                         float * __restrict __ATTR_ALIGN__(64) TMi) FUNC_ATTRIBUTES;

                  __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TM_f4426_zmm16r4_u(const float * __restrict  pk0,
                                         const float * __restrict  pa,
                                         const float * __restrict  pb,
                                         const float * __restrict  pphi1,
                                         const float * __restrict pphi2,
                                         const float * __restrict  pepsr,
                                         const float * __restrict  pepsi,
                                         const float * __restrict  pmur,
                                         const float * __restrict  pmui,
                                         float * __restrict  TMr,
                                         float * __restrict  TMi) FUNC_ATTRIBUTES;

                   /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          TE-case, formula 4.4-27
                     */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TE_f4427_zmm16r4(const __m512 k0,
                                         const __m512 a,
                                         const __m512 b,
                                         const __m512 phi1,
                                         const __m512 phi2,
                                         const __m512 epsr,
                                         const __m512 epsi,
                                         const __m512 mur,
                                         const __m512 mui,
                                         __m512 * __restrict TEr,
                                         __m512 * __restrict TEi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TE_f4427_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                         const float * __restrict __ATTR_ALIGN__(64) pa,
                                         const float * __restrict __ATTR_ALIGN__(64) pb,
                                         const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                         const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                         const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                         const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                         const float * __restrict __ATTR_ALIGN__(64) pmur,
                                         const float * __restrict __ATTR_ALIGN__(64) pmui,
                                         float * __restrict __ATTR_ALIGN__(64) TEr,
                                         float * __restrict __ATTR_ALIGN__(64) TEi) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   void TE_f4427_zmm16r4_u(const float * __restrict  pk0,
                                         const float * __restrict  pa,
                                         const float * __restrict  pb,
                                         const float * __restrict  pphi1,
                                         const float * __restrict pphi2,
                                         const float * __restrict  pepsr,
                                         const float * __restrict  pepsi,
                                         const float * __restrict pmur,
                                         const float * __restrict  pmui,
                                         float * __restrict  TEr,
                                         float * __restrict  TEi) FUNC_ATTRIBUTES;


                  /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          Bistatic scattering width (RCS).
                          TM-case.
                          Formula 4.4-28
                    */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4428_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 b,
                                            const __m512 phi1,
                                            const __m512 phi2,
                                            const __m512 epsr,
                                            const __m512 epsi,
                                            const __m512 mur,
                                            const __m512 mui) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4428_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const float * __restrict __ATTR_ALIGN__(64) pmur,
                                              const float * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4428_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict  pphi1,
                                              const float * __restrict  pphi2,
                                              const float * __restrict  pepsr,
                                              const float * __restrict  pepsi,
                                              const float * __restrict  pmur,
                                              const float * __restrict  pmui) FUNC_ATTRIBUTES;

                  /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          Bistatic scattering width (RCS).
                          TE-case.
                          Formula 4.4-29

                    */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4429_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 b,
                                         const __m512 phi1,
                                         const __m512 phi2,
                                         const __m512 epsr,
                                         const __m512 epsi,
                                         const __m512 mur,
                                         const __m512 mui) FUNC_ATTRIBUTES;


                    __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4429_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const float * __restrict __ATTR_ALIGN__(64) pmur,
                                              const float * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4429_zmm16r4_u(const float * __restrict pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict pb,
                                              const float * __restrict pphi1,
                                              const float * __restrict  pphi2,
                                              const float * __restrict  pepsr,
                                              const float * __restrict  pepsi,
                                              const float * __restrict  pmur,
                                              const float * __restrict  pmui) FUNC_ATTRIBUTES;

                    /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          Backscattering  width (RCS).
                          TM-case.
                          Formula 4.4-30
                    */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4430_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 b,
                                            const __m512 phi1,
                                            const __m512 epsr,
                                            const __m512 epsi,
                                            const __m512 mur,
                                            const __m512 mui) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4430_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const float * __restrict __ATTR_ALIGN__(64) pmur,
                                              const float * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;

                  __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4430_zmm16r4_u(const float * __restrict pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict pphi1,
                                              const float * __restrict pepsr,
                                              const float * __restrict  pepsi,
                                              const float * __restrict  pmur,
                                              const float * __restrict  pmui) FUNC_ATTRIBUTES;

                  /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          Backscattering  width (RCS).
                          TE-case.
                          Formula 4.4-31
                    */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4431_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 b,
                                            const __m512 phi1,
                                            const __m512 epsr,
                                            const __m512 epsi,
                                            const __m512 mur,
                                            const __m512 mui) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4431_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const float * __restrict __ATTR_ALIGN__(64) pmur,
                                              const float * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4431_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict  pphi1,
                                              const float * __restrict  pepsr,
                                              const float * __restrict  pepsi,
                                              const float * __restrict  pmur,
                                              const float * __restrict  pmui) FUNC_ATTRIBUTES;

                 /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          Forward scattering (phi2 = pi+phi1)  width (RCS).
                          TM-case.
                          Formula 4.4-32
                    */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4432_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 b,
                                            const __m512 phi1,
                                            const __m512 epsr,
                                            const __m512 epsi,
                                            const __m512 mur,
                                            const __m512 mui) FUNC_ATTRIBUTES;
                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4432_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const float * __restrict __ATTR_ALIGN__(64) pmur,
                                              const float * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;

                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4432_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict  pphi1,
                                              const float * __restrict  pepsr,
                                              const float * __restrict  pepsi,
                                              const float * __restrict  pmur,
                                              const float * __restrict  pmui) FUNC_ATTRIBUTES;

                 /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          Forward scattering (phi2 = pi+phi1)  width (RCS).
                          TE-case.
                          Formula 4.4-33

                   */


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4433_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 b,
                                            const __m512 phi1,
                                            const __m512 epsr,
                                            const __m512 epsi,
                                            const __m512 mur,
                                            const __m512 mui) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4433_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const float * __restrict __ATTR_ALIGN__(64) pmur,
                                              const float * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;


                   __ATTR_ALWAYS_INLINE__
                   __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f4433_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict  pphi1,
                                              const float * __restrict  pepsr,
                                              const float * __restrict  pepsi,
                                              const float * __restrict  pmur,
                                              const float * __restrict  pmui) FUNC_ATTRIBUTES;


                








                  

 



                   






                   

 




     









#endif /*__GMS_RCS_CYLINDER_ZMM16R4_H__*/
