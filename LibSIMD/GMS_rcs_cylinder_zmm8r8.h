
#ifndef __GMS_RCS_CYLINDER_ZMM8R8_H__
#define __GMS_RCS_CYLINDER_ZMM8R8_H__ 140420231641


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



    const unsigned int GMS_RCS_CYLINDER_ZMM8R8_MAJOR = 1U;
    const unsigned int GMS_RCS_CYLINDER_ZMM8R8_MINOR = 0U;
    const unsigned int GMS_RCS_CYLINDER_ZMM8R8_MICRO = 0U;
    const unsigned int GMS_RCS_CYLINDER_ZMM8R8_FULLVER =
      1000U*GMS_RCS_CYLINDER_ZMM8R8_MAJOR+
      100U*GMS_RCS_CYLINDER_ZMM8R8_MINOR+
      10U*GMS_RCS_CYLINDER_ZMM8R8_MICRO;
    const char * const GMS_RCS_CYLINDER_ZMM8R8_CREATION_DATE = "14-04-2023 09:14  +00200 (WED 14 APR 2023 GMT+2)";
    const char * const GMS_RCS_CYLINDER_ZMM8R8_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_RCS_CYLINDER_ZMM8R8_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_RCS_CYLINDER_ZMM8R8_DESCRIPTION   = "AVX512 optimized Cylinder Radar Cross Section (analytic) functionality.";



#include <stdint.h>
#include <stdbool.h>
#include <immintrin.h>
#include "GMS_kernel_config.h"







              
                 

               


                   /* 
                         Low frequency scattering widths (k0a << 1).
                         Backscatter scattering width for E-field 
                         cylinder-parallel,formula 4.1-19
                    */
               
                   __m512d rcs_f419_zmm8r8(const __m512d a,
                                           const __m512d k0a)  FUNC_ATTRIBUTES;
                                                     


                  
                   __m512d rcs_f419_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0a)  FUNC_ATTRIBUTES;

                
                   __m512d rcs_f419_zmm8r8_u(const double * __restrict  pa,
                                             const double * __restrict  pk0a)  FUNC_ATTRIBUTES;


                /* 
                         Low frequency scattering widths (k0a << 1).
                         Backscatter scattering width for H-field 
                         cylinder-parallel,formula 4.1-20
                    */

                 
                   __m512d rcs_f4120_zmm8r8(const __m512d a,
                                            const __m512d k0a) FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f4120_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0a)  FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4120_zmm8r8_u(const double * __restrict  pa,
                                            const double * __restrict  pk0a)  FUNC_ATTRIBUTES;


                /*
                        Bistatic scattering widths, E-field cylinder axis-parallel
                        Formula 4.1-21
                   */

                
                   __m512d rcs_f4121_zmm8r8(const __m512d a,
                                            const __m512d k0a)  FUNC_ATTRIBUTES;


               
                   __m512d rcs_f4121_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0a)  FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4121_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pk0a)  FUNC_ATTRIBUTES;



                 /*
                        Bistatic scattering widths, H-field cylinder axis-parallel
                        Formula 4.1-22
                   */

                
                   __m512d rcs_f4122_zmm8r8(const __m512d phi,
                                            const __m512d a,
                                            const __m512d k0a)  FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4122_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pphi,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0a)  FUNC_ATTRIBUTES;


               
                   __m512d rcs_f4122_zmm8r8_u(const double * __restrict  pphi,
                                              const double * __restrict  pa,
                                              const double * __restrict  pk0a)  FUNC_ATTRIBUTES;


                   /*
                       Forward scattering widths, E-field.
                       Formula 4.1-23
                   */
 
                 
                   __m512d rcs_f4123_zmm8r8(const __m512d a,
                                            const __m512d k0a)  FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4123_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0a)  FUNC_ATTRIBUTES;

                  
                   __m512d rcs_f4123_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pk0a)  FUNC_ATTRIBUTES;


                  /*
                       Forward scattering widths, H-field.
                       Formula 4.1-24
                   */

                 
                   __m512d rcs_f4124_zmm8r8(const __m512d a,
                                            const __m512d k0a)  FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4124_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0a)  FUNC_ATTRIBUTES;


                  
                   __m512d rcs_f4124_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pk0a)  FUNC_ATTRIBUTES;

                    /*
                          Surface currents (k0a << 1), for long cylinder (wire).
                          E-field cylinder axis parallel.
                          Formula 4.1-25
                       */

                 
                   void Kz_f4125_zmm8r8(const __m512d eps0,
                                         const __m512d mu0,
                                         const __m512d Er,
                                         const __m512d Ei,
                                         const __m512d k0a,
                                         __m512d * __restrict Kzr,
                                         __m512d * __restrict Kzi)  FUNC_ATTRIBUTES;


                 
                   void Kz_f4125_zmm8r8_a(const  double * __restrict __ATTR_ALIGN__(64) peps0,
                                           const  double * __restrict __ATTR_ALIGN__(64) pmu0,
                                           const   double * __restrict __ATTR_ALIGN__(64) pEr,
                                           const   double * __restrict __ATTR_ALIGN__(64) pEi,
                                           const   double * __restrict __ATTR_ALIGN__(64) pk0a,
                                           double * __restrict __ATTR_ALIGN__(64) Kzr,
                                           double * __restrict __ATTR_ALIGN__(64) Kzi)  FUNC_ATTRIBUTES;


                 
                   void Kz_f4125_zmm8r8_u(const  double * __restrict  peps0,
                                           const  double * __restrict  pmu0,
                                           const   double * __restrict  pEr,
                                           const   double * __restrict  pEi,
                                           const   double * __restrict  pk0a,
                                           double * __restrict  Kzr,
                                           double * __restrict  Kzi)  FUNC_ATTRIBUTES;


                  /*
                          Surface currents (k0a << 1), for long cylinder (wire).
                          H-field cylinder axis parallel.
                          Formula 4.1-26
                   */

                  
                   void Kph_f4126_zmm8r8(const __m512d Hr,
                                          const __m512d Hi,
                                          __m512d * __restrict Kphr,
                                          __m512d * __restrict Kphi)  FUNC_ATTRIBUTES;

                 
                   void Kph_f4126_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) Hr,
                                            const double * __restrict __ATTR_ALIGN__(64) Hi,
                                           double * __restrict __ATTR_ALIGN__(64) Kphr,
                                          double * __restrict __ATTR_ALIGN__(64) Kphi)  FUNC_ATTRIBUTES;


                 
                   void Kph_f4126_zmm8r8_u(const double * __restrict  Hr,
                                            const double * __restrict  Hi,
                                           double * __restrict  Kphr,
                                          double * __restrict Kphi)  FUNC_ATTRIBUTES;


                   /*
                        The toal current along the wire.
                        Formula 4.1-27 

                    */

                  
                   void Iz_f4127_zmm8r8(const __m512d eps0,
                                         const __m512d mu0,
                                         const __m512d Er,
                                         const __m512d Ei,
                                         const __m512d k0a,
                                         const __m512d k0,
                                         __m512d * __restrict Izr,
                                         __m512d * __restrict Izi)  FUNC_ATTRIBUTES;


                 
                   void Iz_f4127_zmm8r8_a(const  double * __restrict __ATTR_ALIGN__(64) peps0,
                                           const  double * __restrict __ATTR_ALIGN__(64) pmu0,
                                           const   double * __restrict __ATTR_ALIGN__(64) pEr,
                                           const   double * __restrict __ATTR_ALIGN__(64) pEi,
                                           const   double * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const   double * __restrict __ATTR_ALIGN__(64) pk0,
                                           double * __restrict __ATTR_ALIGN__(64) Izr,
                                           double * __restrict __ATTR_ALIGN__(64) Izi)  FUNC_ATTRIBUTES;

                
                   void Iz_f4127_zmm8r8_u(const  double * __restrict  peps0,
                                           const  double * __restrict  pmu0,
                                           const   double * __restrict  pEr,
                                           const   double * __restrict  pEi,
                                           const   double * __restrict  pk0a,
                                           const   double * __restrict  pk0,
                                           double * __restrict  Izr,
                                           double * __restrict  Izi)  FUNC_ATTRIBUTES;


                   /*
                        Approximation for upper-middle and high-frequency region
                        (k0a > 2).
                        Bistatic creeping wave approximation for resonance region
                        (0<<phi<pi/2, k0a > 2)
                        Electric-field.
                    */

                 
                   void EO_f4129_zmm8r8(const __m512d phi2,
                                         const __m512d a,
                                         const __m512d r,
                                         const __m512d k0,
                                         const __m512d k0a,
                                         const __m512d Er,
                                         const __m512d Ei,
                                         __m512d * __restrict EOr,
                                         __m512d * __restrict EOi)  FUNC_ATTRIBUTES;

                
                   void EO_f4129_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                           const double * __restrict __ATTR_ALIGN__(64) pa,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const double * __restrict __ATTR_ALIGN__(64) pEr,
                                           const double * __restrict __ATTR_ALIGN__(64) pEi,
                                           double * __restrict __ATTR_ALIGN__(64) EOr,
                                           double * __restrict __ATTR_ALIGN__(64) EOi)  FUNC_ATTRIBUTES;

                 
                   void EO_f4129_zmm8r8_u(const double * __restrict  pphi2,
                                           const double * __restrict  pa,
                                           const double * __restrict  pr,
                                           const double * __restrict  pk0,
                                           const double * __restrict  pk0a,
                                           const double * __restrict  pEr,
                                           const double * __restrict  pEi,
                                           double * __restrict  EOr,
                                           double * __restrict  EOi)  FUNC_ATTRIBUTES;

                     /*
                        Approximation for upper-middle and high-frequency region
                        (k0a > 2).
                        Bistatic creeping wave approximation for resonance region
                        (0<<phi<pi/2, k0a > 2)
                        Magnetic-field.
                    */

               
                   void HO_f4131_zmm8r8(const __m512d phi2,
                                         const __m512d a,
                                         const __m512d r,
                                         const __m512d k0,
                                         const __m512d k0a,
                                         const __m512d Hr,
                                         const __m512d Hi,
                                         __m512d * __restrict HOr,
                                         __m512d * __restrict HOi)  FUNC_ATTRIBUTES;


                  
                   void HO_f4131_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                           const double * __restrict __ATTR_ALIGN__(64) pa,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const double * __restrict __ATTR_ALIGN__(64) pHr,
                                           const double * __restrict __ATTR_ALIGN__(64) pHi,
                                           double * __restrict __ATTR_ALIGN__(64) HOr,
                                           double * __restrict __ATTR_ALIGN__(64) HOi)  FUNC_ATTRIBUTES;

                 
                   void HO_f4131_zmm8r8_u(const double * __restrict  pphi2,
                                           const double * __restrict  pa,
                                           const double * __restrict  pr,
                                           const double * __restrict  pk0,
                                           const double * __restrict  pk0a,
                                           const double * __restrict  pHr,
                                           const double * __restrict  pHi,
                                           double * __restrict  HOr,
                                           double * __restrict  HOi) FUNC_ATTRIBUTES;

                 
                 /*
                        Approximation for upper-middle and high-frequency region
                        (k0a > 2).
                        Bistatic creeping wave approximation for resonance region
                        (0<<phi<pi/2, k0a > 2)
                        Electric-field.
                        Formula 4.1-30
                    */

                 
                   void EC_f4130_zmm8r8(const __m512d Er,
                                         const __m512d Ei,
                                         const __m512d a,
                                         const __m512d r,
                                         const __m512d k0,
                                         const __m512d k0a,
                                         const __m512d phi,
                                         __m512d * __restrict ECr,
                                         __m512d * __restrict ECi) FUNC_ATTRIBUTES;

                 
                   void EC_f4130_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pEr,
                                           const double * __restrict __ATTR_ALIGN__(64) pEi,
                                           const double * __restrict __ATTR_ALIGN__(64) pa,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const double * __restrict __ATTR_ALIGN__(64) pphi,
                                           double * __restrict __ATTR_ALIGN__(64) ECr,
                                           double * __restrict __ATTR_ALIGN__(64) ECi) FUNC_ATTRIBUTES;

                
                   void EC_f4130_zmm8r8_a(const double * __restrict  pEr,
                                           const double * __restrict  pEi,
                                           const double * __restrict  pa,
                                           const double * __restrict  pr,
                                           const double * __restrict  pk0,
                                           const double * __restrict  pk0a,
                                           const double * __restrict  pphi,
                                           double * __restrict  ECr,
                                           double * __restrict ECi) FUNC_ATTRIBUTES;

                    /*
                        Approximation for upper-middle and high-frequency region
                        (k0a > 2).
                        Bistatic creeping wave approximation for resonance region
                        valid only for (0<<phi<pi/2, k0a > 2)
                        Magnetic-field.
                        Formula 4.1-32
                    */


                
                   void HC_f4132_zmm8r8(const __m512d Hr,
                                         const __m512d Hi,
                                         const __m512d a,
                                         const __m512d r,
                                         const __m512d k0,
                                         const __m512d k0a,
                                         const __m512d phi,
                                         __m512d * __restrict HCr,
                                         __m512d * __restrict HCi) FUNC_ATTRIBUTES;

                  
                   void HC_f4132_zmm8r8_a(const  double * __restrict __ATTR_ALIGN__(64)  pHr,
                                           const  double * __restrict __ATTR_ALIGN__(64)  pHi,
                                           const  double * __restrict __ATTR_ALIGN__(64)  pa,
                                           const  double * __restrict __ATTR_ALIGN__(64)  pr,
                                           const  double * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const  double * __restrict __ATTR_ALIGN__(64)  pk0a,
                                           const  double * __restrict __ATTR_ALIGN__(64)  pphi,
                                           double * __restrict __ATTR_ALIGN__(64)  HCr,
                                           double * __restrict __ATTR_ALIGN__(64)  HCi) FUNC_ATTRIBUTES;

                
                   void HC_f4132_zmm8r8_u(const  double * __restrict  pHr,
                                           const  double * __restrict  pHi,
                                           const  double * __restrict  pa,
                                           const  double * __restrict  pr,
                                           const  double * __restrict  pk0,
                                           const  double * __restrict  pk0a,
                                           const  double * __restrict  pphi,
                                           double * __restrict   HCr,
                                           double * __restrict   HCi) FUNC_ATTRIBUTES;

                   /*

                       Backscattering creeping-wave approximation for resonance region
                       (phi == 0, k0a > 2).
                       Optical wave component e-field, formula 4.1-33
                   */

               
                   void EO_f4133_zmm8r8(const __m512d Er,
                                         const __m512d Ei,
                                         const __m512d a,
                                         const __m512d r,
                                         const __m512d k0a,
                                         __m512d * __restrict EOr,
                                         __m512d * __restrict EOi) FUNC_ATTRIBUTES;


                
                   void EO_f4133_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pEr,
                                           const double * __restrict __ATTR_ALIGN__(64) pEi,
                                           const double * __restrict __ATTR_ALIGN__(64) pa,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                           double * __restrict __ATTR_ALIGN__(64) EOr,
                                           double * __restrict __ATTR_ALIGN__(64) EOi) FUNC_ATTRIBUTES;

                 
                   void EO_f4133_zmm8r8_u(const double * __restrict  pEr,
                                           const double * __restrict  pEi,
                                           const double * __restrict  pa,
                                           const double * __restrict  pr,
                                           const double * __restrict  pk0a,
                                           double * __restrict  EOr,
                                           double * __restrict  EOi) FUNC_ATTRIBUTES;

                     /*

                       Backscattering creeping-wave approximation for resonance region
                       (phi == 0, k0a > 2).
                       Optical wave component h-field, formula 4.1-35
                   */

                 
                   void HO_f4135_zmm8r8(const __m512d Hr,
                                         const __m512d Hi,
                                         const __m512d a,
                                         const __m512d r,
                                         const __m512d k0a,
                                         __m512d * __restrict HOr,
                                         __m512d * __restrict HOi) FUNC_ATTRIBUTES;

                
                   void HO_f4135_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pHr,
                                           const double * __restrict __ATTR_ALIGN__(64) pHi,
                                           const double * __restrict __ATTR_ALIGN__(64) pa,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                           double * __restrict __ATTR_ALIGN__(64) HOr,
                                           double * __restrict __ATTR_ALIGN__(64) HOi) FUNC_ATTRIBUTES;

                 
                   void HO_f4135_zmm8r8_u(const double * __restrict  pHr,
                                           const double * __restrict  pHi,
                                           const double * __restrict  pa,
                                           const double * __restrict  pr,
                                           const double * __restrict  pk0a,
                                           double * __restrict  HOr,
                                           double * __restrict  HOi) FUNC_ATTRIBUTES;

                   /*

                       Backscattering creeping-wave approximation for resonance region
                       (phi == 0, k0a > 2).
                       Creeping wave component e-field, formula 4.1-34
                   */

                
                   void EC_f4134_zmm8r8(const __m512d Er,
                                         const __m512d Ei,
                                         const __m512d a,
                                         const __m512d r,
                                         const __m512d k0,
                                         __m512d * __restrict ECr,
                                         __m512d * __restrict ECi) FUNC_ATTRIBUTES;

               
                   void EC_f4134_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pEr,
                                         const double * __restrict __ATTR_ALIGN__(64) pEi,
                                         const double * __restrict __ATTR_ALIGN__(64) pa,
                                         const double * __restrict __ATTR_ALIGN__(64) pr,
                                         const double * __restrict __ATTR_ALIGN__(64) pk0,
                                         double * __restrict __ATTR_ALIGN__(64) ECr,
                                         double * __restrict __ATTR_ALIGN__(64) ECi) FUNC_ATTRIBUTES;

                  
                 
                   void EC_f4134_zmm8r8_u(const double * __restrict  pEr,
                                           const double * __restrict  pEi,
                                           const double * __restrict  pa,
                                           const double * __restrict  pr,
                                           const double * __restrict  pk0,
                                           double * __restrict  ECr,
                                           double * __restrict  ECi) FUNC_ATTRIBUTES;


                   /*

                       Backscattering creeping-wave approximation for resonance region
                       (phi == 0, k0a > 2).
                       Creeping wave component h-field, formula 4.1-36
                   */


                 
                   void HC_f4136_zmm8r8(const __m512d Hr,
                                         const __m512d Hi,
                                         const __m512d a,
                                         const __m512d r,
                                         const __m512d k0,
                                         __m512d * __restrict HCr,
                                         __m512d * __restrict HCi) FUNC_ATTRIBUTES;

                
                   void HC_f4136_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pHr,
                                           const double * __restrict __ATTR_ALIGN__(64) pHi,
                                           const double * __restrict __ATTR_ALIGN__(64) pa,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           double * __restrict __ATTR_ALIGN__(64) HCr,
                                           double * __restrict __ATTR_ALIGN__(64) HCi) FUNC_ATTRIBUTES;

                  
                   void HC_f4136_zmm8r8_u(const double * __restrict  pHr,
                                           const double * __restrict  pHi,
                                           const double * __restrict  pa,
                                           const double * __restrict  pr,
                                           const double * __restrict  pk0,
                                           double * __restrict  HCr,
                                           double * __restrict  HCi) FUNC_ATTRIBUTES;

                  /*
                        Bistatic scattering width in high frequency limit (k0a > 20)
                        for |PI-phi| > k0a^0.3
                        Formula 4.1-37
                    */

                
                   __m512d rcs_f4137_zmm8r8(const __m512d a,
                                            const __m512d phi2) FUNC_ATTRIBUTES;


              
                   __m512d rcs_f4137_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi2) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4137_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pphi2) FUNC_ATTRIBUTES;


                    /*
                          Backscattering Width in High-Frequency Limit (k0a > 20)
                          Formula 4.1-38
                     */

                   
                
                   __m512d rcs_f4138_zmm8r8(const __m512d a) FUNC_ATTRIBUTES;

                
                   __m512d rcs_f4138_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4138_zmm8r8_u(const double * __restrict  pa) FUNC_ATTRIBUTES;


                   /*
                         Forward scattering widths and pattern in high-frequency limit
                         (k0a>20.0)
                         Formula 4.1-40, RCS.
                     */

                
                   __m512d rcs_f4140_zmm8r8(const __m512d k0a,
                                            const __m512d alpha) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4140_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const double * __restrict __ATTR_ALIGN__(64) palpha) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4140_zmm8r8_u(const double * __restrict  pk0a,
                                              const double * __restrict  palpha) FUNC_ATTRIBUTES;


                     /*
                         Forward scattering widths and pattern in high-frequency limit
                         (k0a>20.0), forward scattered (diffracted) e-field
                         Formula 4.1-39.

                       */


                 
                   void Es_f4139_zmm8r8(const __m512d Er,
                                         const __m512d Ei,
                                         const __m512d r,
                                         const __m512d k0,
                                         const __m512d alp
                                         const __m512d k0a,
                                         __m512d * __restrict Esr,
                                         __m512d * __restrict Esi) FUNC_ATTRIBUTES;


                 
                   void Es_f4139_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pEr,
                                           const double * __restrict __ATTR_ALIGN__(64) pEi,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) palp
                                           const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                           double * __restrict __ATTR_ALIGN__(64) Esr,
                                           double * __restrict __ATTR_ALIGN__(64) Esi) FUNC_ATTRIBUTES;


                  
                   void Es_f4139_zmm8r8_u(const double * __restrict  pEr,
                                           const double * __restrict  pEi,
                                           const double * __restrict  pr,
                                           const double * __restrict  pk0,
                                           const double * __restrict  palp
                                           const double * __restrict  pk0a,
                                           double * __restrict  Esr,
                                           double * __restrict  Esi) FUNC_ATTRIBUTES;


                  /*
                         Forward scattering widths and pattern in high-frequency limit
                         (k0a>20.0), constant angle (alpha=0)
                         Formula 4.1-41, RCS.
                     */

             
                   __m512d rcs_f4141_zmm8r8(const __m512d k0a) FUNC_ATTRIBUTES;

                

                 
                   __m512d rcs_f4141_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64)  pk0a) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4141_zmm8r8_u(const double * __restrict   pk0a) FUNC_ATTRIBUTES;


                   /*
                        Approximations for the low frequency region (k0a<<1,k1a<<1)
                        Scattered far-zone e-field, formula 4.1-45
                    */

                 
                   void Es_f4145_zmm8r8(const __m512d EIr,
                                         const __m512d EIi,
                                         const __m512d r,
                                         const __m512d k0,
                                         const __m512d k0a,
                                         const __m512d phi,
                                         const __m512d eps0,
                                         const __m512d eps1,
                                         const __m512d mu0,
                                         const __m512d mu1,
                                         __m512d * __restrict ESr,
                                         __m512d * __restrict ESi) FUNC_ATTRIBUTES;

               
                   void Es_f4145_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pEIr,
                                           const double * __restrict __ATTR_ALIGN__(64) pEIi,
                                           const double * __restrict __ATTR_ALIGN__(64)  pr,
                                           const double * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const double * __restrict __ATTR_ALIGN__(64)  pk0a,
                                           const double * __restrict __ATTR_ALIGN__(64)  pphi,
                                           const double * __restrict __ATTR_ALIGN__(64)  peps0,
                                           const double * __restrict __ATTR_ALIGN__(64)  peps1,
                                           const double * __restrict __ATTR_ALIGN__(64)  pmu0,
                                           const double * __restrict __ATTR_ALIGN__(64)  pmu1,
                                           double * __restrict __ATTR_ALIGN__(64)  ESr,
                                           double * __restrict __ATTR_ALIGN__(64)  ESi) FUNC_ATTRIBUTES;

                  
                   void Es_f4145_zmm8r8_u(const double * __restrict  pEIr,
                                           const double * __restrict  pEIi,
                                           const double * __restrict  pr,
                                           const double * __restrict   pk0,
                                           const double * __restrict  pk0a,
                                           const double * __restrict   pphi,
                                           const double * __restrict   peps0,
                                           const double * __restrict  peps1,
                                           const double * __restrict   pmu0,
                                           const double * __restrict   pmu1,
                                           double * __restrict   ESr,
                                           double * __restrict   ESi) FUNC_ATTRIBUTES;

                  /*
                        Approximations for the low frequency region (k0a<<1,k1a<<1)
                        Scattered far-zone h-field, formula 4.1-46
                    */


               
                   void Hs_f4146_zmm8r8(const __m512d HIr,
                                         const __m512d HIi,
                                         const __m512d r,
                                         const __m512d k0,
                                         const __m512d k0a,
                                         const __m512d phi,
                                         const __m512d eps0,
                                         const __m512d eps1,
                                         const __m512d mu0,
                                         const __m512d mu1,
                                         __m512d * __restrict HSr,
                                         __m512d * __restrict HSi) FUNC_ATTRIBUTES;

               
                   void Hs_f4146_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pHIr,
                                           const double * __restrict __ATTR_ALIGN__(64) pHIi,
                                           const double * __restrict __ATTR_ALIGN__(64)  pr,
                                           const double * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const double * __restrict __ATTR_ALIGN__(64)  pk0a,
                                           const double * __restrict __ATTR_ALIGN__(64)  pphi,
                                           const double * __restrict __ATTR_ALIGN__(64)  peps0,
                                           const double * __restrict __ATTR_ALIGN__(64)  peps1,
                                           const double * __restrict __ATTR_ALIGN__(64)  pmu0,
                                           const double * __restrict __ATTR_ALIGN__(64)  pmu1,
                                           double * __restrict __ATTR_ALIGN__(64)  HSr,
                                           double * __restrict __ATTR_ALIGN__(64)  HSi) FUNC_ATTRIBUTES;

               
                   void Hs_f4146_zmm8r8_u(const double * __restrict   pHIr,
                                           const double * __restrict   pHIi,
                                           const double * __restrict   pr,
                                           const double * __restrict   pk0,
                                           const double * __restrict   pk0a,
                                           const double * __restrict   pphi,
                                           const double * __restrict   peps0,
                                           const double * __restrict   peps1,
                                           const double * __restrict   pmu0,
                                           const double * __restrict   pmu1,
                                           double * __restrict  HSr,
                                           double * __restrict   HSi) FUNC_ATTRIBUTES;

                 /*
                      Bistatic scattering width (k0a<<1, k1a<<1) at the angle 'phi'
                      Formula 4.1-47

                   */

                
                   __m512d rcs_f4147_zmm8r8(const __m512d a,
                                            const __m512d k0a,
                                            const __m512d phi,
                                            const __m512d eps1,
                                            const __m512d eps0,
                                            const __m512d mu1,
                                            const __m512d mu0) FUNC_ATTRIBUTES;


                   
               
                   __m512d rcs_f4147_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                            const double * __restrict __ATTR_ALIGN__(64) pphi,
                                            const double * __restrict __ATTR_ALIGN__(64) peps1,
                                            const double * __restrict __ATTR_ALIGN__(64) peps0,
                                            const double * __restrict __ATTR_ALIGN__(64) pmu1,
                                            const double * __restrict __ATTR_ALIGN__(64) pmu0) FUNC_ATTRIBUTES;


               
                   __m512d rcs_f4147_zmm8r8_u(const double * __restrict  pa,
                                            const double * __restrict  pk0a,
                                            const double * __restrict  pphi,
                                            const double * __restrict  peps1,
                                            const double * __restrict  peps0,
                                            const double * __restrict  pmu1,
                                            const double * __restrict  pmu0) FUNC_ATTRIBUTES;


                    /*
                      Bistatic scattering width (k0a<<1, k1a<<1) at the angle 'phi'
                      Formula 4.1-48

                   */   

                 
                   __m512d rcs_f4148_zmm8r8(const __m512d a,
                                            const __m512d k0a,
                                            const __m512d phi,
                                            const __m512d eps1,
                                            const __m512d eps0,
                                            const __m512d mu1,
                                            const __m512d mu0) FUNC_ATTRIBUTES;

               
                   __m512d rcs_f4148_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                            const double * __restrict __ATTR_ALIGN__(64) pphi,
                                            const double * __restrict __ATTR_ALIGN__(64) peps1,
                                            const double * __restrict __ATTR_ALIGN__(64) peps0,
                                            const double * __restrict __ATTR_ALIGN__(64) pmu1,
                                            const double * __restrict __ATTR_ALIGN__(64) pmu0) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4148_zmm8r8_u(const double * __restrict  pa,
                                            const double * __restrict pk0a,
                                            const double * __restrict  pphi,
                                            const double * __restrict  peps1,
                                            const double * __restrict  peps0,
                                            const double * __restrict  pmu1,
                                            const double * __restrict  pmu0) FUNC_ATTRIBUTES;


                   /*
                         Backscattering width (k0a<<1,k1a<<1), when phi = 0
                         Formula 4.1-49
                    */
                 
                 
                   __m512d rcs_f4149_zmm8r8(const __m512d a,
                                            const __m512d k0a,
                                            const __m512d eps1,
                                            const __m512d eps0,
                                            const __m512d mu1,
                                            const __m512d mu0) FUNC_ATTRIBUTES;

                
                   __m512d rcs_f4149_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const double * __restrict __ATTR_ALIGN__(64) peps1,
                                              const double * __restrict __ATTR_ALIGN__(64) peps0,
                                              const double * __restrict __ATTR_ALIGN__(64) pmu1,
                                              const double * __restrict __ATTR_ALIGN__(64) pmu0) FUNC_ATTRIBUTES;


              
                   __m512d rcs_f4149_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pk0a,
                                              const double * __restrict  peps1,
                                              const double * __restrict  peps0,
                                              const double * __restrict  pmu1,
                                              const double * __restrict  pmu0) FUNC_ATTRIBUTES;

                     /*
                         Backscattering width (k0a<<1,k1a<<1), when phi = 0
                         Formula 4.1-50
                    */

                 
                   __m512d rcs_f4150_zmm8r8(const __m512d a,
                                            const __m512d k0a,
                                            const __m512d eps1,
                                            const __m512d eps0,
                                            const __m512d mu1,
                                            const __m512d mu0) FUNC_ATTRIBUTES;

                
                   __m512d rcs_f4150_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const double * __restrict __ATTR_ALIGN__(64) peps1,
                                              const double * __restrict __ATTR_ALIGN__(64) peps0,
                                              const double * __restrict __ATTR_ALIGN__(64) pmu1,
                                              const double * __restrict __ATTR_ALIGN__(64) pmu0) FUNC_ATTRIBUTES;


             
                   __m512d rcs_f4150_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pk0a,
                                              const double * __restrict  peps1,
                                              const double * __restrict  peps0,
                                              const double * __restrict  pmu1,
                                              const double * __restrict  pmu0) FUNC_ATTRIBUTES;


                    /*
                         Forward scattering width (k0a<<1, k1a<<1), phi = pi
                         Formula 4.1-51
                     */

                  
                   __m512d rcs_f4151_zmm8r8(const __m512d a,
                                            const __m512d k0a,
                                            const __m512d eps1,
                                            const __m512d eps0,
                                            const __m512d mu1,
                                            const __m512d mu0) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4151_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const double * __restrict __ATTR_ALIGN__(64) peps1,
                                              const double * __restrict __ATTR_ALIGN__(64) peps0,
                                              const double * __restrict __ATTR_ALIGN__(64) pmu1,
                                              const double * __restrict __ATTR_ALIGN__(64) pmu0) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4151_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pk0a,
                                              const double * __restrict  peps1,
                                              const double * __restrict  peps0,
                                              const double * __restrict  pmu1,
                                              const double * __restrict  pmu0) FUNC_ATTRIBUTES;


                      /*
                         Forward scattering width (k0a<<1, k1a<<1), phi = pi
                         Formula 4.1-52
                     */

                 
                   __m512d rcs_f4152_zmm8r8(const __m512d a,
                                            const __m512d k0a,
                                            const __m512d eps1,
                                            const __m512d eps0,
                                            const __m512d mu1,
                                            const __m512d mu0) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4152_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const double * __restrict __ATTR_ALIGN__(64) peps1,
                                              const double * __restrict __ATTR_ALIGN__(64) peps0,
                                              const double * __restrict __ATTR_ALIGN__(64) pmu1,
                                              const double * __restrict __ATTR_ALIGN__(64) pmu0) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4152_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pk0a,
                                              const double * __restrict  peps1,
                                              const double * __restrict  peps0,
                                              const double * __restrict  pmu1,
                                              const double * __restrict  pmu0) FUNC_ATTRIBUTES;

                     /*
                           Fresnel reflection and transmission coefficients
                           Formula 4.1-72
                       */

                
                   void Tin_f4172_zmm8r8(const __m512d mur,
                                          const __m512d mui,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d psi,
                                          __m512d * __restrict Tinr,
                                          __m512d * __restrict Tini) FUNC_ATTRIBUTES;


                
                   void Tin_f4172_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pmur,
                                            const double * __restrict __ATTR_ALIGN__(64) pmui,
                                            const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                            const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                            const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                            double * __restrict __ATTR_ALIGN__(64) Tinr,
                                            double * __restrict __ATTR_ALIGN__(64) Tini) FUNC_ATTRIBUTES;


                 
                   void Tin_f4172_zmm8r8_u(const double * __restrict  pmur,
                                            const double * __restrict  pmui,
                                            const double * __restrict  pepsr,
                                            const double * __restrict  pepsi,
                                            const double * __restrict  ppsi,
                                            double * __restrict  Tinr,
                                            double * __restrict  Tini) FUNC_ATTRIBUTES;


                     /*
                           Fresnel reflection and transmission coefficients
                           Formula 4.1-73
                       */

                   void Tin_f4173_zmm8r8(const __m512d mur,
                                          const __m512d mui,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d psi,
                                          __m512d * __restrict Tinr,
                                          __m512d * __restrict Tini) FUNC_ATTRIBUTES;

                  
                   void Tin_f4173_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pmur,
                                            const double * __restrict __ATTR_ALIGN__(64) pmui,
                                            const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                            const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                            const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                            double * __restrict __ATTR_ALIGN__(64) Tinr,
                                            double * __restrict __ATTR_ALIGN__(64) Tini) FUNC_ATTRIBUTES;


                 
                   void Tin_f4173_zmm8r8_u(const double * __restrict  pmur,
                                            const double * __restrict  pmui,
                                            const double * __restrict  pepsr,
                                            const double * __restrict  pepsi,
                                            const double * __restrict  ppsi,
                                            double * __restrict  Tinr,
                                            double * __restrict  Tini) FUNC_ATTRIBUTES;


                    /*
                           Fresnel reflection and transmission coefficients
                           Formula 4.1-74
                       */

                
                   void Tout_f4174_zmm8r8(const __m512d mur,
                                          const __m512d mui,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d psi,
                                          __m512d * __restrict Toutr,
                                          __m512d * __restrict Touti) FUNC_ATTRIBUTES;


                
                   void Tout_f4174_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pmur,
                                            const double * __restrict __ATTR_ALIGN__(64) pmui,
                                            const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                            const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                            const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                            double * __restrict __ATTR_ALIGN__(64) Toutr,
                                            double * __restrict __ATTR_ALIGN__(64) Touti) FUNC_ATTRIBUTES;


               
                   void Tout_f4174_zmm8r8_u(const double * __restrict pmur,
                                            const double * __restrict  pmui,
                                            const double * __restrict  pepsr,
                                            const double * __restrict  pepsi,
                                            const double * __restrict  ppsi,
                                            double * __restrict  Toutr,
                                            double * __restrict  Touti) FUNC_ATTRIBUTES;


                   /*
                           Fresnel reflection and transmission coefficients
                           Formula 4.1-75
                       */


               
                   void Tout_f4175_zmm8r8(const __m512d mur,
                                           const __m512d mui,
                                           const __m512d epsr,
                                           const __m512d epsi,
                                           const __m512d psi,
                                           __m512d * __restrict Toutr,
                                           __m512d * __restrict Touti) FUNC_ATTRIBUTES;


               
                   void Tout_f4175_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pmur,
                                            const double * __restrict __ATTR_ALIGN__(64) pmui,
                                            const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                            const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                            const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                            double * __restrict __ATTR_ALIGN__(64) Toutr,
                                            double * __restrict __ATTR_ALIGN__(64) Touti) FUNC_ATTRIBUTES;

                
                   void Tout_f4175_zmm8r8_u(const double * __restrict  pmur,
                                            const double * __restrict  pmui,
                                            const double * __restrict  pepsr,
                                            const double * __restrict  pepsi,
                                            const double * __restrict  ppsi,
                                            double * __restrict  Toutr,
                                            double * __restrict  Touti) FUNC_ATTRIBUTES;


                   /*
                           Fresnel reflection and transmission coefficients
                           Formula 4.1-76
                    */

                 
                   void Rin_f4176_zmm8r8( const __m512d mur,
                                           const __m512d mui,
                                           const __m512d epsr,
                                           const __m512d epsi,
                                           const __m512d psi,
                                           __m512d * __restrict Rinr,
                                           __m512d * __restrict Rini) FUNC_ATTRIBUTES;

             
                   void Rin_f4176_zmm8r8_a( const double * __restrict __ATTR_ALIGN__(64) pmur,
                                             const double * __restrict __ATTR_ALIGN__(64) pmui,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                             const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                             double * __restrict __ATTR_ALIGN__(64) Rinr,
                                             double * __restrict __ATTR_ALIGN__(64) Rini) FUNC_ATTRIBUTES;


                 
                   void Rin_f4176_zmm8r8_u( const double * __restrict  pmur,
                                             const double * __restrict  pmui,
                                             const double * __restrict  pepsr,
                                             const double * __restrict  pepsi,
                                             const double * __restrict  ppsi,
                                             double * __restrict  Rinr,
                                             double * __restrict  Rini) FUNC_ATTRIBUTES;


                    /*
                           Fresnel reflection and transmission coefficients
                           Formula 4.1-77
                    */


                
                   void Rin_f4177_zmm8r8( const __m512d mur,
                                           const __m512d mui,
                                           const __m512d epsr,
                                           const __m512d epsi,
                                           const __m512d psi,
                                           __m512d * __restrict Rinr,
                                           __m512d * __restrict Rini) FUNC_ATTRIBUTES;

                  
                   void Rin_f4177_zmm8r8_a( const double * __restrict __ATTR_ALIGN__(64) pmur,
                                             const double * __restrict __ATTR_ALIGN__(64) pmui,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                             const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                             double * __restrict __ATTR_ALIGN__(64) Rinr,
                                             double * __restrict __ATTR_ALIGN__(64) Rini ) FUNC_ATTRIBUTES;


                  
                   void Rin_f4177_zmm8r8_u( const double * __restrict  pmur,
                                             const double * __restrict  pmui,
                                             const double * __restrict  pepsr,
                                             const double * __restrict  pepsi,
                                             const double * __restrict  ppsi,
                                             double * __restrict  Rinr,
                                             double * __restrict  Rini ) FUNC_ATTRIBUTES;


                  /*
                          Specular rays reflection
                          Formula 4.1-64
                      */

                
                   void Rext_f4164_zmm8r8(const __m512d mur,
                                           const __m512d mui,
                                           const __m512d epsr,
                                           const __m512d epsi,
                                           __m512d * __restrict Rexr,
                                           __m512d * __restrict Rexi) FUNC_ATTRIBUTES;


                 
                   void Rext_f4164_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pmur,
                                             const double * __restrict __ATTR_ALIGN__(64) pmui,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                             double * __restrict __ATTR_ALIGN__(64) Rexr,
                                             double * __restrict __ATTR_ALIGN__(64) Rexi) FUNC_ATTRIBUTES;

                  
                  
                   void Rext_f4164_zmm8r8_u(const double * __restrict  pmur,
                                             const double * __restrict  pmui,
                                             const double * __restrict  pepsr,
                                             const double * __restrict  pepsi,
                                             double * __restrict  Rexr,
                                             double * __restrict  Rexi) FUNC_ATTRIBUTES;


                  /*

                         Axial rays, when phi = 0
                         Formula 4.1-67
                    */

                  
                
                   void Tin_f4167_zmm8r8( const __m512d mur,
                                           const __m512d mui,
                                           const __m512d epsr,
                                           const __m512d epsi,
                                           __m512d * __restrict Tinr,
                                           __m512d * __restrict Tini) FUNC_ATTRIBUTES;

              
                   void Tin_f4167_zmm8r8_a( const double * __restrict __ATTR_ALIGN__(64) pmur,
                                             const double * __restrict __ATTR_ALIGN__(64) pmui,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                             double * __restrict __ATTR_ALIGN__(64) Tinr,
                                             double * __restrict __ATTR_ALIGN__(64) Tini ) FUNC_ATTRIBUTES;


            
                   void Tin_f4167_zmm8r8_u( const double * __restrict  pmur,
                                             const double * __restrict  pmui,
                                             const double * __restrict  pepsr,
                                             const double * __restrict  pepsi,
                                             double * __restrict  Tinr,
                                             double * __restrict  Tini ) FUNC_ATTRIBUTES;

                  /*
                          Axial rays, when phi = 0
                          Formula 4.1-68
                   */


               
                   void Tout_f4168_zmm8r8( const __m512d mur,
                                           const __m512d mui,
                                           const __m512d epsr,
                                           const __m512d epsi,
                                           __m512d * __restrict Toutr,
                                           __m512d * __restrict Touti) FUNC_ATTRIBUTES;

               
                   void Tout_f4168_zmm8r8_a(  const double * __restrict __ATTR_ALIGN__(64) pmur,
                                             const double * __restrict __ATTR_ALIGN__(64) pmui,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                             double * __restrict __ATTR_ALIGN__(64) Toutr,
                                             double * __restrict __ATTR_ALIGN__(64) Touti ) FUNC_ATTRIBUTES;


               
                   void Tout_f4168_zmm8r8_u(  const double * __restrict  pmur,
                                             const double * __restrict  pmui,
                                             const double * __restrict  pepsr,
                                             const double * __restrict  pepsi,
                                             double * __restrict  Toutr,
                                             double * __restrict Touti ) FUNC_ATTRIBUTES;


                  /*
                          Axial rays, when phi = 0
                          Formula 4.1-69
                   */


                
                   void Rint_f4169_zmm8r8(const __m512d mur,
                                           const __m512d mui,
                                           const __m512d epsr,
                                           const __m512d epsi,
                                           __m512d * __restrict Rintr,
                                           __m512d * __restrict Rinti) FUNC_ATTRIBUTES;


               
                   void Rint_f4169_zmm8r8_a( const double * __restrict __ATTR_ALIGN__(64) pmur,
                                             const double * __restrict __ATTR_ALIGN__(64) pmui,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                             const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                             double * __restrict __ATTR_ALIGN__(64) Rintr,
                                             double * __restrict __ATTR_ALIGN__(64) Rinti) FUNC_ATTRIBUTES;


              
                   void Rint_f4169_zmm8r8_u( const double * __restrict pmur,
                                              const double * __restrict  pmui,
                                              const double * __restrict  pepsr,
                                              const double * __restrict  pepsi,
                                              double * __restrict  Rintr,
                                              double * __restrict  Rinti) FUNC_ATTRIBUTES;


                   /*
                       Backscatter widths in high-frequency limit.
                       Phi = 0, formula 4.1-91,for k1a>5.
                    */

                 
                   __m512d rcs_f4191_zmm8r8(const __m512d a,
                                            const __m512d mur,
                                            const __m512d mui,
                                            const __m512d epsr,
                                            const __m512d epsi ) FUNC_ATTRIBUTES;

               
                   __m512d rcs_f4191_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pmur,
                                              const double * __restrict __ATTR_ALIGN__(64) pmui,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsi, ) FUNC_ATTRIBUTES;


              
                   __m512d rcs_f4191_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pmur,
                                              const double * __restrict  pmui,
                                              const double * __restrict  pepsr,
                                              const double * __restrict  pepsi, ) FUNC_ATTRIBUTES;


                    /*
                         Bistatic scattering width (k0a0<<1, k1a0<<1), function of phi angle.
                         Formula 4.1-104
                      */

                 
                   __m512d rcs_f41104_zmm8r8(const __m512d a0,
                                             const __m512d a1,
                                             const __m512d k0a0,
                                             const __m512d phi,
                                             const __m512d mu1r,
                                             const __m512d mu1i,
                                             const __m512d mu0r,
                                             const __m512d mu0i,
                                             const __m512d eps1r,
                                             const __m512d eps1i,
                                             const __m512d eps0r,
                                             const __m512d eps0i) FUNC_ATTRIBUTES;


                  
                   __m512d rcs_f41104_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa0,
                                               const double * __restrict __ATTR_ALIGN__(64) pa1,
                                               const double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                               const double * __restrict __ATTR_ALIGN__(64) pphi,
                                               const double * __restrict __ATTR_ALIGN__(64) pmu1r,
                                               const double * __restrict __ATTR_ALIGN__(64) pmu1i,
                                               const double * __restrict __ATTR_ALIGN__(64) pmu0r,
                                               const double * __restrict __ATTR_ALIGN__(64) pmu0i,
                                               const double * __restrict __ATTR_ALIGN__(64) peps1r,
                                               const double * __restrict __ATTR_ALIGN__(64) peps1i,
                                               const double * __restrict __ATTR_ALIGN__(64) peps0r,
                                               const double * __restrict __ATTR_ALIGN__(64) peps0i) FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f41104_zmm8r8_u(const double * __restrict  pa0,
                                               const double * __restrict  pa1,
                                               const double * __restrict  pk0a0,
                                               const double * __restrict  pphi,
                                               const double * __restrict  pmu1r,
                                               const double * __restrict  pmu1i,
                                               const double * __restrict  pmu0r,
                                               const double * __restrict  pmu0i,
                                               const double * __restrict  peps1r,
                                               const double * __restrict  peps1i,
                                               const double * __restrict  peps0r,
                                               const double * __restrict  peps0i) FUNC_ATTRIBUTES;

                /*
                         Backscattering  width (k0a0<<1, k1a0<<1), phi = 0
                         Formula 4.1-105
                  */

                  
                   __m512d rcs_f41105_zmm8r8(const __m512d a0,
                                             const __m512d a1,
                                             const __m512d k0a0,
                                             const __m512d mu1r,
                                             const __m512d mu1i,
                                             const __m512d mu0r,
                                             const __m512d mu0i,
                                             const __m512d eps1r,
                                             const __m512d eps1i,
                                             const __m512d eps0r,
                                             const __m512d eps0i) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f41105_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa0,
                                               const double * __restrict __ATTR_ALIGN__(64) pa1,
                                               const double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                               const double * __restrict __ATTR_ALIGN__(64) pmu1r,
                                               const double * __restrict __ATTR_ALIGN__(64) pmu1i,
                                               const double * __restrict __ATTR_ALIGN__(64) pmu0r,
                                               const double * __restrict __ATTR_ALIGN__(64) pmu0i,
                                               const double * __restrict __ATTR_ALIGN__(64) peps1r,
                                               const double * __restrict __ATTR_ALIGN__(64) peps1i,
                                               const double * __restrict __ATTR_ALIGN__(64) peps0r,
                                               const double * __restrict __ATTR_ALIGN__(64) peps0i) FUNC_ATTRIBUTES;

                
                   __m512d rcs_f41105_zmm8r8_u(const double * __restrict  pa0,
                                               const double * __restrict  pa1,
                                               const double * __restrict  pk0a0,
                                               const double * __restrict  pmu1r,
                                               const double * __restrict  pmu1i,
                                               const double * __restrict  pmu0r,
                                               const double * __restrict  pmu0i,
                                               const double * __restrict  peps1r,
                                               const double * __restrict  peps1i,
                                               const double * __restrict  peps0r,
                                               const double * __restrict  peps0i) FUNC_ATTRIBUTES;

                /*
                      Forward scattering width (k0a0<<1, k1a0<<1), phi = pi.
                      Formula 4.1-106
                 */


                
                   __m512d rcs_f41106_zmm8r8(const __m512d a0,
                                             const __m512d a1,
                                             const __m512d k0a0,
                                             const __m512d mu1r,
                                             const __m512d mu1i,
                                             const __m512d mu0r,
                                             const __m512d mu0i,
                                             const __m512d eps1r,
                                             const __m512d eps1i,
                                             const __m512d eps0r,
                                             const __m512d eps0i) FUNC_ATTRIBUTES;


                  
                   __m512d rcs_f41106_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa0,
                                               const double * __restrict __ATTR_ALIGN__(64) pa1,
                                               const double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                               const double * __restrict __ATTR_ALIGN__(64) pmu1r,
                                               const double * __restrict __ATTR_ALIGN__(64) pmu1i,
                                               const double * __restrict __ATTR_ALIGN__(64) pmu0r,
                                               const double * __restrict __ATTR_ALIGN__(64) pmu0i,
                                               const double * __restrict __ATTR_ALIGN__(64) peps1r,
                                               const double * __restrict __ATTR_ALIGN__(64) peps1i,
                                               const double * __restrict __ATTR_ALIGN__(64) peps0r,
                                               const double * __restrict __ATTR_ALIGN__(64) peps0i) FUNC_ATTRIBUTES;


                 /*
                       Hollow cylindrical shell.
                       Approximations for the low frequency region
                       (k0a0<<1, k1a0<<1).
                       Formula 4.1-124
                  */


                
                   void A0_f41124_zmm8r8(const __m512d a1,
                                          const __m512d a0,
                                          const __m512d k0a0,
                                          const __m512d eps1r,
                                          const __m512d eps1i,
                                          const __m512d eps0r,
                                          const __m512d eps0i,
                                          __m512d * __restrict A0r,
                                          __m512d * __restrict A0i) FUNC_ATTRIBUTES;


                   void A0_f41124_zmm8r8_a(const  double * __restrict __ATTR_ALIGN__(64) pa1,
                                            const  double * __restrict __ATTR_ALIGN__(64) pa0,
                                            const  double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                            const  double * __restrict __ATTR_ALIGN__(64) peps1r,
                                            const  double * __restrict __ATTR_ALIGN__(64) peps1i,
                                            const  double * __restrict __ATTR_ALIGN__(64) peps0r,
                                            const  double * __restrict __ATTR_ALIGN__(64) peps0i,
                                            double * __restrict __ATTR_ALIGN__(64) A0r,
                                            double * __restrict __ATTR_ALIGN__(64) A0i) FUNC_ATTRIBUTES;

               
                   void A0_f41124_zmm8r8_u(const  double * __restrict  pa1,
                                          const  double * __restrict pa0,
                                          const  double * __restrict  pk0a0,
                                          const  double * __restrict  peps1r,
                                          const  double * __restrict  peps1i,
                                          const  double * __restrict  peps0r,
                                          const  double * __restrict  peps0i,
                                          double * __restrict  A0r,
                                          double * __restrict  A0i) FUNC_ATTRIBUTES;

                  /*

                       Hollow cylindrical shell.
                       Approximations for the low frequency region
                       (k0a0<<1, k1a0<<1).
                       Formula 4.1-126
                   */


                
                   void B0_f41126_zmm8r8(const __m512d a1,
                                          const __m512d a0,
                                          const __m512d k0a0,
                                          const __m512d mu1r,
                                          const __m512d mu1i,
                                          const __m512d mu0r,
                                          const __m512d mu0i,
                                          __m512d * __restrict B0r,
                                          __m512d * __restrict B0i) FUNC_ATTRIBUTES;


                
                   void B0_f41126_zmm8r8_a(const  double * __restrict __ATTR_ALIGN__(64) pa1,
                                            const  double * __restrict __ATTR_ALIGN__(64) pa0,
                                            const  double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                            const  double * __restrict __ATTR_ALIGN__(64) pmu1r,
                                            const  double * __restrict __ATTR_ALIGN__(64) pmu1i,
                                            const  double * __restrict __ATTR_ALIGN__(64) pmu0r,
                                            const  double * __restrict __ATTR_ALIGN__(64) pmu0i,
                                            double * __restrict __ATTR_ALIGN__(64) B0r,
                                            double * __restrict __ATTR_ALIGN__(64) B0i) FUNC_ATTRIBUTES;


                 
                   void B0_f41126_zmm8r8_u(const  double * __restrict  pa1,
                                            const  double * __restrict  pa0,
                                            const  double * __restrict  pk0a0,
                                            const  double * __restrict  mu1r,
                                            const  double * __restrict  mu1i,
                                            const  double * __restrict  mu0r,
                                            const  double * __restrict  mu0i,
                                            double * __restrict  B0r,
                                            double * __restrict  B0i) FUNC_ATTRIBUTES;


                  /*

                          Hollow cylindrical shell.
                          Approximations for the low frequency region
                          (k0a0<<1, k1a0<<1).
                           Formula 4.1-125
                    */

                
                   void A1_f41125_zmm8r8(const __m512d a1,
                                          const __m512d a0,
                                          const __m512d k0a0,
                                          const __m512d mu1r,
                                          const __m512d mu1i,
                                          const __m512d mu0r,
                                          const __m512d mu0i,
                                          __m512d * __restrict A1r,
                                          __m512d * __restrict A1i) FUNC_ATTRIBUTES;

                
                   void A1_f41125_zmm8r8_a(const  double * __restrict __ATTR_ALIGN__(64) pa1,
                                            const  double * __restrict __ATTR_ALIGN__(64) pa0,
                                            const  double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                            const  double * __restrict __ATTR_ALIGN__(64) pmu1r,
                                            const  double * __restrict __ATTR_ALIGN__(64) pmu1i,
                                            const  double * __restrict __ATTR_ALIGN__(64) pmu0r,
                                            const  double * __restrict __ATTR_ALIGN__(64) pmu0i,
                                            double * __restrict __ATTR_ALIGN__(64) A1r,
                                            double * __restrict __ATTR_ALIGN__(64) A1i) FUNC_ATTRIBUTES;

                
                   void A1_f41125_zmm8r8_u(const  double * __restrict  pa1,
                                            const  double * __restrict  pa0,
                                            const  double * __restrict  pk0a0,
                                            const  double * __restrict  pmu1r,
                                            const  double * __restrict  pmu1i,
                                            const  double * __restrict  pmu0r,
                                            const  double * __restrict  pmu0i,
                                            double * __restrict  A1r,
                                            double * __restrict A1i) FUNC_ATTRIBUTES;

                 /*

                          Hollow cylindrical shell.
                          Approximations for the low frequency region
                          (k0a0<<1, k1a0<<1).
                           Formula 4.1-127
                    */


               
                   void B1_f41127_zmm8r8(const __m512d a1,
                                          const __m512d a0,
                                          const __m512d k0a0,
                                          const __m512d eps1r,
                                          const __m512d eps1i,
                                          const __m512d eps0r,
                                          const __m512d eps0i,
                                          __m512d * __restrict B1r,
                                          __m512d * __restrict B1i) FUNC_ATTRIBUTES;


                
                   void B1_f41127_zmm8r8_a(const  double * __restrict __ATTR_ALIGN__(64) pa1,
                                            const  double * __restrict __ATTR_ALIGN__(64) pa0,
                                            const  double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                            const  double * __restrict __ATTR_ALIGN__(64) peps1r,
                                            const  double * __restrict __ATTR_ALIGN__(64) peps1i,
                                            const  double * __restrict __ATTR_ALIGN__(64) peps0r,
                                            const  double * __restrict __ATTR_ALIGN__(64) peps0i,
                                            double * __restrict __ATTR_ALIGN__(64) B1r,
                                            double * __restrict __ATTR_ALIGN__(64) B1i) FUNC_ATTRIBUTES;

                 
                   void B1_f41127_zmm8r8_u(const  double * __restrict  pa1,
                                            const  double * __restrict  pa0,
                                            const  double * __restrict  pk0a0,
                                            const  double * __restrict  peps1r,
                                            const  double * __restrict  peps1i,
                                            const  double * __restrict  peps0r,
                                            const  double * __restrict  peps0i,
                                            double * __restrict  B1r,
                                            double * __restrict  B1i) FUNC_ATTRIBUTES;

                    /*

                          Low-frequncy approximations (k0a<0.2)
                          Cylindrical Luneberg lens (k0a<0.2).
                          Formula 4.1-162
                     */

                    

                
                   void A0_f41162_zmm8r8(const __m512d k0a,
                                          __m512d * __restrict A0r,
                                          __m512d * __restrict A0i) FUNC_ATTRIBUTES;


                
                   void A0_f41162_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                            double * __restrict __ATTR_ALIGN__(64) A0r,
                                            double * __restrict __ATTR_ALIGN__(64) A0i) FUNC_ATTRIBUTES;

                
                   void A0_f41162_zmm8r8_u(const double * __restrict  pk0a,
                                            double * __restrict  A0r,
                                            double * __restrict  A0i) FUNC_ATTRIBUTES;


                 

                 
                   void B0_f41162_zmm8r8(__m512d * __restrict B0r,
                                          __m512d * __restrict B0i) FUNC_ATTRIBUTES;


                 
                   void B0_f41162_zmm8r8_a(double * __restrict __ATTR_ALIGN__(64) B0r,
                                          double * __restrict __ATTR_ALIGN__(64) B0i) FUNC_ATTRIBUTES;


                 
                   void B0_f41162_zmm8r8_u(double * __restrict  B0r,
                                          double * __restrict  B0i) FUNC_ATTRIBUTES;


                 
                   void A1_f41162_zmm8r8(__m512d * __restrict A1r,
                                          __m512d * __restrict A1i) FUNC_ATTRIBUTES;


                
                   void A1_f41162_zmm8r8_a(double * __restrict __ATTR_ALIGN__(64) A1r,
                                          double * __restrict __ATTR_ALIGN__(64) A1i) FUNC_ATTRIBUTES;


                
                   void A1_f41162_zmm8r8_u(double * __restrict  A1r,
                                            double * __restrict  A1i) FUNC_ATTRIBUTES;


                 
                   void B1_f41162_zmm8r8(const __m512d k0a,
                                          __m512d * __restrict B1r,
                                          __m512d * __restrict B1i) FUNC_ATTRIBUTES;


                
                   void B1_f41162_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                            double * __restrict __ATTR_ALIGN__(64) B1r,
                                            double * __restrict __ATTR_ALIGN__(64) B1i) FUNC_ATTRIBUTES;


                 
                   void B1_f41162_zmm8r8_u(const double * __restrict  pk0a,
                                            double * __restrict  B1r,
                                            double * __restrict  B1i) FUNC_ATTRIBUTES;


                   /*
                          Low-frequncy approximations (k0a<0.2)
                          Cylindrical Luneberg lens (k0a<0.2).  
                          Scattering widths.
                          Formula 4.1-163
                      */

                
                   __m512d rcs_f41163_zmm8r8(const __m512d a,
                                             const __m512d k0a) FUNC_ATTRIBUTES;


              
                   __m512d rcs_f41163_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                             const double * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f41163_zmm8r8_u(const double * __restrict  pa,
                                             const double * __restrict  pk0a) FUNC_ATTRIBUTES;


                    /*
                          Low-frequncy approximations (k0a<0.2)
                          Cylindrical Luneberg lens (k0a<0.2).  
                          Scattering widths.
                          Formula 4.1-164
                      */


                  
                   __m512d rcs_f41164_zmm8r8(const __m512d a,
                                             const __m512d k0a,
                                             const __m512d phi) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f41164_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                               const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                               const double * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f41164_zmm8r8_u(const double * __restrict  pa,
                                               const double * __restrict  pk0a,
                                               const double * __restrict  pphi) FUNC_ATTRIBUTES;


                  /*

                      Cylindrical Eaton-Lippman Lens, (k0a<0.2)
                      Formulae 4.1-165
                  */


                
                   void A0_f41165_zmm8r8(const __m512d k0a,
                                          __m512d * __restrict A0r,
                                          __m512d * __restrict A0i) FUNC_ATTRIBUTES;


              
                   void A0_f41165_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                            double * __restrict __ATTR_ALIGN__(64) A0r,
                                            double * __restrict __ATTR_ALIGN__(64) A0i) FUNC_ATTRIBUTES;


                
                   void A0_f41165_zmm8r8_u(const double * __restrict  pk0a,
                                            double * __restrict  A0r,
                                            double * __restrict  A0i) FUNC_ATTRIBUTES;

                 
                   void A1_f41165_zmm8r8(__m512d * __restrict A0r,
                                          __m512d * __restrict A0i) FUNC_ATTRIBUTES;


                  
               
                   void A1_f41165_zmm8r8_a(double * __restrict __ATTR_ALIGN__(64) A0r,
                                            double * __restrict __ATTR_ALIGN__(64) A0i) FUNC_ATTRIBUTES;

                 
                   void A1_f41165_zmm8r8_u(double * __restrict  A0r,
                                            double * __restrict  A0i) FUNC_ATTRIBUTES;

               
                   void B0_f41165_zmm8r8(__m512d * __restrict B0r,
                                          __m512d * __restrict B0i) FUNC_ATTRIBUTES;


                  
               
                   void B0_f41165_zmm8r8_a(double * __restrict __ATTR_ALIGN__(64) B0r,
                                            double * __restrict __ATTR_ALIGN__(64) B0i) FUNC_ATTRIBUTES;


                
                   void B0_f41165_zmm8r8_u(double * __restrict  B0r,
                                            double * __restrict  B0i) FUNC_ATTRIBUTES;


             
                   void B1_f41165_zmm8r8(const __m512d k0a,
                                          __m512d * __restrict B1r,
                                          __m512d * __restrict B1i) FUNC_ATTRIBUTES;


              
                   void B1_f41165_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                          double * __restrict __ATTR_ALIGN__(64) B1r,
                                          double * __restrict __ATTR_ALIGN__(64) B1i) FUNC_ATTRIBUTES;


                 
                   void B1_f41165_zmm8r8_u(const double * __restrict  pk0a,
                                          double * __restrict  B1r,
                                          double * __restrict  B1i) FUNC_ATTRIBUTES;


                  /*

                       Cylindrical Eaton-Lippman Lens, (k0a<0.2) 
                       Scattering widths.
                       Formula: 1.4-166
                   */

                  
                   __m512d rcs_f14166_zmm8r8(const __m512d a,
                                             const __m512d k0a) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f14166_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                               const double * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


             
                   __m512d rcs_f14166_zmm8r8_u(const double * __restrict pa,
                                               const double * __restrict  pk0a) FUNC_ATTRIBUTES;


                  /*

                       Cylindrical Eaton-Lippman Lens, (k0a<0.2) 
                       Scattering widths.
                       Formula: 1.4-167
                   */
                 
 
               
                   __m512d rcs_f14167_zmm8r8(const __m512d a,
                                             const __m512d k0a,
                                             const __m512d phi) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f14167_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                               const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                               const double * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f14167_zmm8r8_u(const double * __restrict  pa,
                                               const double * __restrict  pk0a,
                                               const double * __restrict  pphi) FUNC_ATTRIBUTES;



                  /*

                        Infinitely long cylinder.
                        Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                        TM-incident E-field.
                        Formula 4.2-48
                    */


                
                   void Ez_f4248_zmm8r8(const __m512d E0r,
                                         const __m512d E0i,
                                         const __m512d psi,
                                         const __m512d phi,
                                         const __m512d k0,
                                         const __m512d z,
                                         const __m512d r,
                                         const __m512d a0,
                                         const __m512d epsr,
                                         const __m512d epsi,
                                         const __m512d mur,
                                         const __m512d mui,
                                         __m512d * __restrict Ezr,
                                         __m512d * __restrict Ezi) FUNC_ATTRIBUTES;


               
                   void Ez_f4248_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pE0r,
                                           const double * __restrict __ATTR_ALIGN__(64) pE0i,
                                           const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                           const double * __restrict __ATTR_ALIGN__(64) pphi,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pz,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) pa0,
                                           const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                           const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                           const double * __restrict __ATTR_ALIGN__(64) pmur,
                                           const double * __restrict __ATTR_ALIGN__(64) pmui,
                                           double * __restrict __ATTR_ALIGN__(64) Ezr,
                                           double * __restrict __ATTR_ALIGN__(64) Ezi) FUNC_ATTRIBUTES;


                
                   void Ez_f4248_zmm8r8_u(const double * __restrict  pE0r,
                                           const double * __restrict  pE0i,
                                           const double * __restrict  ppsi,
                                           const double * __restrict  pphi,
                                           const double * __restrict  pk0,
                                           const double * __restrict  pz,
                                           const double * __restrict  pr,
                                           const double * __restrict  pa0,
                                           const double * __restrict  pepsr,
                                           const double * __restrict  pepsi,
                                           const double * __restrict  pmur,
                                           const double * __restrict  pmui,
                                           double * __restrict  Ezr,
                                           double * __restrict  Ezi) FUNC_ATTRIBUTES;


               

                       /*

                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TM-incident H-field.
                         Formula 4.2-51
                    */

                   void Hp_f4251_zmm8r8(const __m512d E0r,
                                         const __m512d E0i,
                                         const __m512d psi,
                                         const __m512d phi,
                                         const __m512d k0,
                                         const __m512d z,
                                         const __m512d r,
                                         const __m512d a0,
                                         const __m512d epsr,
                                         const __m512d epsi,
                                         const __m512d mur,
                                         const __m512d mui,
                                         __m512d * __restrict Hpr,
                                         __m512d * __restrict Hpi) FUNC_ATTRIBUTES;


                   void Hp_f4251_zmm8r8_a(  const double * __restrict __ATTR_ALIGN__(64) pE0r,
                                           const double * __restrict __ATTR_ALIGN__(64) pE0i,
                                           const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                           const double * __restrict __ATTR_ALIGN__(64) pphi,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0,
                                           const double * __restrict __ATTR_ALIGN__(64) pz,
                                           const double * __restrict __ATTR_ALIGN__(64) pr,
                                           const double * __restrict __ATTR_ALIGN__(64) pa0,
                                           const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                           const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                           const double * __restrict __ATTR_ALIGN__(64) pmur,
                                           const double * __restrict __ATTR_ALIGN__(64) pmui,
                                           double * __restrict __ATTR_ALIGN__(64) Hpr,
                                           double * __restrict __ATTR_ALIGN__(64) Hpi) FUNC_ATTRIBUTES;


                
                   void Hp_f4251_zmm8r8_u(  const double * __restrict  pE0r,
                                           const double * __restrict  pE0i,
                                           const double * __restrict  ppsi,
                                           const double * __restrict pphi,
                                           const double * __restrict  pk0,
                                           const double * __restrict  pz,
                                           const double * __restrict  pr,
                                           const double * __restrict  pa0,
                                           const double * __restrict  pepsr,
                                           const double * __restrict  pepsi,
                                           const double * __restrict  pmur,
                                           const double * __restrict  pmui,
                                           double * __restrict  Hpr,
                                           double * __restrict  Hpi) FUNC_ATTRIBUTES;


               
                     /*

                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TM-incident E-field.
                         Formula 4.2-49
                    */

                  
                   void Eph_f4249_zmm8r8(const __m512d E0r,
                                          const __m512d E0i,
                                          const __m512d k0z,
                                          const __m512d k0r,
                                          const __m512d k0a0,
                                          const __m512d psi,
                                          const __m512d phi,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          __m512d * __restrict Ephr,
                                          __m512d * __restrict Ephi) FUNC_ATTRIBUTES;

                   


                 
                   void Eph_f4249_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pE0r,
                                          const double * __restrict __ATTR_ALIGN__(64) pE0i,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0z,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0r,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                          const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          double * __restrict __ATTR_ALIGN__(64) Ephr,
                                          double * __restrict __ATTR_ALIGN__(64) Ephi) FUNC_ATTRIBUTES;


                 
                   void Eph_f4249_zmm8r8_u(const double * __restrict pE0r,
                                          const double * __restrict  pE0i,
                                          const double * __restrict pk0z,
                                          const double * __restrict  pk0r,
                                          const double * __restrict  pk0a0,
                                          const double * __restrict  ppsi,
                                          const double * __restrict  pphi,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict  pmur,
                                          const double * __restrict  pmui,
                                          double * __restrict Ephr,
                                          double * __restrict  Ephi) FUNC_ATTRIBUTES;



                 /*
                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TM-incident H-field.
                         Formula 4.2-50

                  */


                 
                   void Hz_f4250_zmm8r8( const __m512d E0r,
                                          const __m512d E0i,
                                          const __m512d k0z,
                                          const __m512d k0r,
                                          const __m512d k0a0,
                                          const __m512d psi,
                                          const __m512d phi,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          __m512d * __restrict Hzr,
                                          __m512d * __restrict Hzi) FUNC_ATTRIBUTES;


                  
                  
                   void Hz_f4250_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pE0r,
                                          const double * __restrict __ATTR_ALIGN__(64) pE0i,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0z,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0r,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                          const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          double * __restrict __ATTR_ALIGN__(64) Hzr,
                                          double * __restrict __ATTR_ALIGN__(64) Hzi ) FUNC_ATTRIBUTES;


               
                   void Hz_f4250_zmm8r8_u(const double * __restrict  pE0r,
                                          const double * __restrict pE0i,
                                          const double * __restrict  pk0z,
                                          const double * __restrict  pk0r,
                                          const double * __restrict  pk0a0,
                                          const double * __restrict  ppsi,
                                          const double * __restrict  pphi,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict  pmur,
                                          const double * __restrict  pmui,
                                          double * __restrict  Hzr,
                                          double * __restrict  Hzi ) FUNC_ATTRIBUTES;


                 /*
                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TE-incident H-field.
                         Formula 4.2-52

                   */


               
                   void Hz_f4252_zmm8r8(const __m512d H0r,
                                         const __m512d H0i,
                                         const __m512d psi,
                                         const __m512d phi,
                                         const __m512d k0r,
                                         const __m512d k0z,
                                         const __m512d k0a0,
                                         const __m512d epsr,
                                         const __m512d epsi,
                                         const __m512d mur,
                                         const __m512d mui,
                                         __m512d * __restrict Hzr,
                                         __m512d * __restrict Hzi) FUNC_ATTRIBUTES;


               
                   void Hz_f4252_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pH0r,
                                          const double * __restrict __ATTR_ALIGN__(64) pH0i,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0z,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0r,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                          const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          double * __restrict __ATTR_ALIGN__(64) Hzr,
                                          double * __restrict __ATTR_ALIGN__(64) Hzi) FUNC_ATTRIBUTES;


              
                   void Hz_f4252_zmm8r8_u(const double * __restrict  pH0r,
                                          const double * __restrict  pH0i,
                                          const double * __restrict  pk0z,
                                          const double * __restrict  pk0r,
                                          const double * __restrict  pk0a0,
                                          const double * __restrict  ppsi,
                                          const double * __restrict  pphi,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict pmur,
                                          const double * __restrict  pmui,
                                          double * __restrict  Hzr,
                                          double * __restrict Hzi) FUNC_ATTRIBUTES;


                    /*
                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TE-incident E-field.
                         Formula 4.2-55

                   */



                   void Eph_f4255_zmm8r8(const __m512d H0r,
                                         const __m512d H0i,
                                         const __m512d psi,
                                         const __m512d phi,
                                         const __m512d k0r,
                                         const __m512d k0z,
                                         const __m512d k0a0,
                                         const __m512d epsr,
                                         const __m512d epsi,
                                         const __m512d mur,
                                         const __m512d mui,
                                         __m512d * __restrict Epr,
                                         __m512d * __restrict Epi) FUNC_ATTRIBUTES;



                 
                   void Eph_f4255_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pH0r,
                                          const double * __restrict __ATTR_ALIGN__(64) pH0i,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0z,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0r,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                          const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          double * __restrict __ATTR_ALIGN__(64) Epr,
                                          double * __restrict __ATTR_ALIGN__(64) Epi) FUNC_ATTRIBUTES;


               
                   void Eph_f4255_zmm8r8_u(const double * __restrict pH0r,
                                          const double * __restrict  pH0i,
                                          const double * __restrict  pk0z,
                                          const double * __restrict  pk0r,
                                          const double * __restrict  pk0a0,
                                          const double * __restrict  ppsi,
                                          const double * __restrict  pphi,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict  pmur,
                                          const double * __restrict  pmui,
                                          double * __restrict  Epr,
                                          double * __restrict  Epi) FUNC_ATTRIBUTES;


                    /*
                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TE-incident H-field.
                         Formula 4.2-53

                   */


                 
                   void Hph_f4253_zmm8r8(const __m512d H0r,
                                          const __m512d H0i,
                                          const __m512d k0z,
                                          const __m512d k0r,
                                          const __m512d psi,
                                          const __m512d phi,
                                          const __m512d k0a0,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          __m512d * __restrict Hpr,
                                          __m512d * __restrict Hpi) FUNC_ATTRIBUTES;

                  
                   void Hph_f4253_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pH0r,
                                          const double * __restrict __ATTR_ALIGN__(64) pH0i,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0z,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0r,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                          const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          double * __restrict __ATTR_ALIGN__(64) Hpr,
                                          double * __restrict __ATTR_ALIGN__(64) Hpi) FUNC_ATTRIBUTES;

                   void Hph_f4253_zmm8r8_u(const double * __restrict  pH0r,
                                          const double * __restrict pH0i,
                                          const double * __restrict  pk0z,
                                          const double * __restrict  pk0r,
                                          const double * __restrict  pk0a0,
                                          const double * __restrict  ppsi,
                                          const double * __restrict  pphi,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict  pmur,
                                          const double * __restrict  pmui,
                                          double * __restrict  Hpr,
                                          double * __restrict  Hpi) FUNC_ATTRIBUTES;

                   /*
                         Infinitely long cylinder.
                         Scattered fields (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                         TE-incident E-field.
                         Formula 4.2-54

                   */


               
                   void Ez_f4254_zmm8r8( const __m512d H0r,
                                          const __m512d H0i,
                                          const __m512d k0z,
                                          const __m512d k0r,
                                          const __m512d psi,
                                          const __m512d phi,
                                          const __m512d k0a0,
                                          const __m512d epsr,
                                          const __m512d epsi,
                                          const __m512d mur,
                                          const __m512d mui,
                                          __m512d * __restrict Ezr,
                                          __m512d * __restrict Ezi) FUNC_ATTRIBUTES;


                 
                   void Ez_f4254_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pH0r,
                                          const double * __restrict __ATTR_ALIGN__(64) pH0i,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0z,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0r,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                          const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                          const double * __restrict __ATTR_ALIGN__(64) pmur,
                                          const double * __restrict __ATTR_ALIGN__(64) pmui,
                                          double * __restrict __ATTR_ALIGN__(64) Hzr,
                                          double * __restrict __ATTR_ALIGN__(64) Hzi ) FUNC_ATTRIBUTES;

                 
                   void Ez_f4254_zmm8r8_u(const double * __restrict  pH0r,
                                          const double * __restrict pH0i,
                                          const double * __restrict  pk0z,
                                          const double * __restrict  pk0r,
                                          const double * __restrict  pk0a0,
                                          const double * __restrict  ppsi,
                                          const double * __restrict  pphi,
                                          const double * __restrict  pepsr,
                                          const double * __restrict  pepsi,
                                          const double * __restrict  pmur,
                                          const double * __restrict  pmui,
                                          double * __restrict  Hzr,
                                          double * __restrict  Hzi ) FUNC_ATTRIBUTES;


                  /*
                     Bistatic scattering width for (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                     Infinitely long cylinder.
                     TM-incident.
                     Formula 4.2-56
                 */


                 
                   __m512d rcs_f4256_zmm8r8(const __m512d a0,
                                            const __m512d k0a0,
                                            const __m512d psi,
                                            const __m512d phi,
                                            const __m512d epsr,
                                            const __m512d epsi,
                                            const __m512d mur,
                                            const __m512d mui) FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f4256_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa0,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const double * __restrict __ATTR_ALIGN__(64) pmur,
                                              const double * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;

               
                   __m512d rcs_f4256_zmm8r8_u(const double * __restrict  pa0,
                                              const double * __restrict  pk0a0,
                                              const double * __restrict  ppsi,
                                              const double * __restrict  pphi,
                                              const double * __restrict  pepsr,
                                              const double * __restrict  pepsi,
                                              const double * __restrict  pmur,
                                              const double * __restrict  pmui) FUNC_ATTRIBUTES;

                  /*
                     Bistatic scattering width for (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                     Infinitely long cylinder.
                     TE-incident.
                     Formula 4.2-58
                 */


                 
                   __m512d rcs_f4258_zmm8r8(const __m512d a0,
                                            const __m512d k0a0,
                                            const __m512d psi,
                                            const __m512d phi,
                                            const __m512d epsr,
                                            const __m512d epsi,
                                            const __m512d mur,
                                            const __m512d mui) FUNC_ATTRIBUTES;

                
                   __m512d rcs_f4258_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa0,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const double * __restrict __ATTR_ALIGN__(64) pmur,
                                              const double * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;

                  
                   __m512d rcs_f4258_zmm8r8_u(const double * __restrict  pa0,
                                              const double * __restrict  pk0a0,
                                              const double * __restrict  ppsi,
                                              const double * __restrict  pphi,
                                              const double * __restrict  pepsr,
                                              const double * __restrict  pepsi,
                                              const double * __restrict  pmur,
                                              const double * __restrict  pmui) FUNC_ATTRIBUTES;

                   /*

                           Bistatic scattering width for (k0a0 sqrt(epsr*mur-sin^2(Psi) < 0.5)
                           Infinitely long cylinder.
                           TM-incident.
                           Formula 4.2-57
                     */


                
                   __m512d rcs_f4257_zmm8r8(const __m512d a0,
                                            const __m512d k0a0,
                                            const __m512d psi,
                                            const __m512d phi,
                                            const __m512d epsr,
                                            const __m512d epsi,
                                            const __m512d mur,
                                            const __m512d mui) FUNC_ATTRIBUTES;


                   __m512d rcs_f4257_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa0,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0a0,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const double * __restrict __ATTR_ALIGN__(64) pmur,
                                              const double * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f4257_zmm8r8_u(const double * __restrict  pa0,
                                              const double * __restrict  pk0a0,
                                              const double * __restrict  ppsi,
                                              const double * __restrict  pphi,
                                              const double * __restrict  pepsr,
                                              const double * __restrict  pepsi,
                                              const double * __restrict  pmur,
                                              const double * __restrict  pmui) FUNC_ATTRIBUTES;


                   /*
                       Circular cylinders of finite length.
                       Cylinder radius small (k0a<1.0)
                       Wire limit of cylinder (h>>a).
                       E-field
                       Formula 4.3-9
                   */

                    
                
                   void ES_f439_zmm8r8(const __m512d EIr,
                                        const __m512d EIi,
                                        const __m512d r,
                                        const __m512d k0,
                                        const __m512d psii,
                                        const __m512d psis,
                                        const __m512d h,
                                        const __m512d ln4ha,
                                        __m512d * __restrict ESr,
                                        __m512d * __restrict ESi) FUNC_ATTRIBUTES;


                 
                   void ES_f439_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pEIr,
                                          const double * __restrict __ATTR_ALIGN__(64) pEIi,
                                          const double * __restrict __ATTR_ALIGN__(64) pr,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0,
                                          const double * __restrict __ATTR_ALIGN__(64) ppsii,
                                          const double * __restrict __ATTR_ALIGN__(64) ppsis,
                                          const double * __restrict __ATTR_ALIGN__(64) ph,
                                          const double * __restrict __ATTR_ALIGN__(64) pln4ha,
                                          double * __restrict __ATTR_ALIGN__(64) ESr,
                                          double * __restrict __ATTR_ALIGN__(64) ESi) FUNC_ATTRIBUTES;

                   void ES_f439_zmm8r8_u(const double * __restrict  pEIr,
                                          const double * __restrict  pEIi,
                                          const double * __restrict  pr,
                                          const double * __restrict  pk0,
                                          const double * __restrict  ppsii,
                                          const double * __restrict  ppsis,
                                          const double * __restrict  ph,
                                          const double * __restrict  pln4ha,
                                          double * __restrict  ESr,
                                          double * __restrict  ESi) FUNC_ATTRIBUTES;


                  /*
                       Circular cylinders of finite length.
                       Cylinder radius small (k0a<1.0)
                       Wire limit of cylinder (h>>a).
                       RCS.
                       Formula 4.3-10

                    */
                  
                  
                   __m512d rcs_f4310_zmm8r8(const __m512d k0,
                                            const __m512d h,
                                            const __m512d psii,
                                            const __m512d psis,
                                            const __m512d ln4h) FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f4310_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64)  pk0,
                                              const double * __restrict __ATTR_ALIGN__(64)  ph,
                                              const double * __restrict __ATTR_ALIGN__(64)  ppsii,
                                              const double * __restrict __ATTR_ALIGN__(64)  ppsis,
                                              const double * __restrict __ATTR_ALIGN__(64)  pln4h) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4310_zmm8r8_u(const double * __restrict   pk0,
                                              const double * __restrict   ph,
                                              const double * __restrict   ppsii,
                                              const double * __restrict   ppsis,
                                              const double * __restrict   pln4h) FUNC_ATTRIBUTES;


                  /*
                         The average dipole scattering RCS when the incidence
                         and scattered polarization direction coincide.
                         Formula 4.3-11
                    */

                   
                
                   __m512d rcs_f4311_zmm8r8(const __m512d k0,
                                            const __m512d h,
                                            const __m512d ln4h) FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f4311_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) ph,
                                              const double * __restrict __ATTR_ALIGN__(64) pln4h) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4311_zmm8r8_u(const double * __restrict  pk0,
                                              const double * __restrict  ph,
                                              const double * __restrict  pln4h) FUNC_ATTRIBUTES;


                  /*
                           Disc limit of cylinder (h<<a).
                           Scattered fields from the cylinder in the disc limit
                           Formula 4.3-18
                      */

               
                   void ES_f4318_zmm8r8(const __m512d EIr,
                                         const __m512d EIi,
                                         const __m512d k0,
                                         const __m512d r,
                                         const __m512d psii,
                                         const __m512d psis,
                                         const __m512d phi,
                                         const __m512d a,
                                         __m512d * __restrict ESr,
                                         __m512d * __restrict ESi) FUNC_ATTRIBUTES;

                
                   void ES_f4318_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64)  pEIr,
                                           const double * __restrict __ATTR_ALIGN__(64)  pEIi,
                                           const double * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const double * __restrict __ATTR_ALIGN__(64)  pr,
                                           const double * __restrict __ATTR_ALIGN__(64)  ppsii,
                                           const double * __restrict __ATTR_ALIGN__(64)  ppsis,
                                           const double * __restrict __ATTR_ALIGN__(64)  pphi,
                                           const double * __restrict __ATTR_ALIGN__(64)  pa,
                                           double * __restrict __ATTR_ALIGN__(64) ESr,
                                           double * __restrict __ATTR_ALIGN__(64)  ESi) FUNC_ATTRIBUTES;


                 
                   void ES_f4318_zmm8r8_u(const double * __restrict   pEIr,
                                           const double * __restrict   pEIi,
                                           const double * __restrict   pk0,
                                           const double * __restrict   pr,
                                           const double * __restrict   ppsii,
                                           const double * __restrict   ppsis,
                                           const double * __restrict   pphi,
                                           const double * __restrict   pa,
                                           double * __restrict  ESr,
                                           double * __restrict   ESi) FUNC_ATTRIBUTES;

                   /*
                           Disc limit of cylinder (h<<a).
                           Scattered fields from the cylinder in the disc limit
                           Formula 4.3-19
                      */


                  
                   void ES_f4319_zmm8r8(const __m512d EIr,
                                         const __m512d EIi,
                                         const __m512d k0,
                                         const __m512d r,
                                         const __m512d psii,
                                         const __m512d phi,
                                         const __m512d a,
                                         __m512d * __restrict ESr,
                                         __m512d * __restrict ESi) FUNC_ATTRIBUTES;


               
                   void ES_f4319_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64)  pEIr,
                                           const double * __restrict __ATTR_ALIGN__(64)  pEIi,
                                           const double * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const double * __restrict __ATTR_ALIGN__(64)  pr,
                                           const double * __restrict __ATTR_ALIGN__(64)  ppsii,
                                           const double * __restrict __ATTR_ALIGN__(64)  pphi,
                                           const double * __restrict __ATTR_ALIGN__(64)  pa,
                                           double * __restrict __ATTR_ALIGN__(64) ESr,
                                           double * __restrict __ATTR_ALIGN__(64)  ESi) FUNC_ATTRIBUTES;

                
                   void ES_f4319_zmm8r8_u(const double * __restrict   pEIr,
                                           const double * __restrict   pEIi,
                                           const double * __restrict   pk0,
                                           const double * __restrict   pr,
                                           const double * __restrict   ppsii,
                                           const double * __restrict   pphi,
                                           const double * __restrict   pa,
                                           double * __restrict  ESr,
                                           double * __restrict  ESi) FUNC_ATTRIBUTES;


                 /*
                           Disc limit of cylinder (h<<a).
                           Scattered fields from the cylinder in the disc limit
                           Formula 4.3-20
                   */


                 
                   void ES_f4320_zmm8r8(const __m512d EIr,
                                         const __m512d EIi,
                                         const __m512d k0,
                                         const __m512d r,
                                         const __m512d psis,
                                         const __m512d phi,
                                         const __m512d a,
                                         __m512d * __restrict ESr,
                                         __m512d * __restrict ESi) FUNC_ATTRIBUTES;

                 
                   void ES_f4320_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64)  pEIr,
                                           const double * __restrict __ATTR_ALIGN__(64)  pEIi,
                                           const double * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const double * __restrict __ATTR_ALIGN__(64)  pr,
                                           const double * __restrict __ATTR_ALIGN__(64)  ppsis,
                                           const double * __restrict __ATTR_ALIGN__(64)  pphi,
                                           const double * __restrict __ATTR_ALIGN__(64)  pa,
                                           double * __restrict __ATTR_ALIGN__(64) ESr,
                                           double * __restrict __ATTR_ALIGN__(64)  ESi) FUNC_ATTRIBUTES;

                 
                   void ES_f4320_zmm8r8_u(const double * __restrict   pEIr,
                                           const double * __restrict   pEIi,
                                           const double * __restrict   pk0,
                                           const double * __restrict   pr,
                                           const double * __restrict   ppsis,
                                           const double * __restrict   pphi,
                                           const double * __restrict   pa,
                                           double * __restrict  ESr,
                                           double * __restrict  ESi) FUNC_ATTRIBUTES;


                   /*
                           Disc limit of cylinder (h<<a).
                           Scattered fields from the cylinder in the disc limit
                           Formula 4.3-21
                   */


                   void ES_f4321_zmm8r8(const __m512d EIr,
                                         const __m512d EIi,
                                         const __m512d k0,
                                         const __m512d r,
                                         const __m512d psii,
                                         const __m512d psis,
                                         const __m512d phi,
                                         const __m512d a,
                                         __m512d * __restrict ESr,
                                         __m512d * __restrict ESi) FUNC_ATTRIBUTES;


                 
                   void ES_f4321_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64)  pEIr,
                                           const double * __restrict __ATTR_ALIGN__(64)  pEIi,
                                           const double * __restrict __ATTR_ALIGN__(64)  pk0,
                                           const double * __restrict __ATTR_ALIGN__(64)  pr,
                                           const double * __restrict __ATTR_ALIGN__(64)  ppsii,
                                           const double * __restrict __ATTR_ALIGN__(64)  ppsis,
                                           const double * __restrict __ATTR_ALIGN__(64)  pphi,
                                           const double * __restrict __ATTR_ALIGN__(64)  pa,
                                           double * __restrict __ATTR_ALIGN__(64) ESr,
                                           double * __restrict __ATTR_ALIGN__(64)  ESi) FUNC_ATTRIBUTES;


                  
                   void ES_f4321_zmm8r8_u(const double * __restrict   pEIr,
                                           const double * __restrict   pEIi,
                                           const double * __restrict   pk0,
                                           const double * __restrict   pr,
                                           const double * __restrict   ppsii,
                                           const double * __restrict   ppsis,
                                           const double * __restrict   pphi,
                                           const double * __restrict   pa,
                                           double * __restrict  ESr,
                                           double * __restrict  ESi) FUNC_ATTRIBUTES;


                 /*
                           Disc limit of cylinder (h<<a).
                           Bistatic scattering RCS for cylinder in the disc limit
                           Formula 4.3-22
                      */


                 
                   __m512d rcs_f4322_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d psii,
                                            const __m512d psis,
                                            const __m512d phi) FUNC_ATTRIBUTES;

               
                   __m512d rcs_f4322_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsis,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                  
                   __m512d rcs_f4322_zmm8r8_u(const double * __restrict  pk0,
                                              const double * __restrict  pa,
                                              const double * __restrict  ppsii,
                                              const double * __restrict  ppsis,
                                              const double * __restrict  pphi) FUNC_ATTRIBUTES;

                    /*
                           Disc limit of cylinder (h<<a).
                           Bistatic scattering RCS for cylinder in the disc limit
                           Formula 4.3-23
                      */


                 
                   __m512d rcs_f4323_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d psii,
                                            const __m512d phi) FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f4323_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4323_zmm8r8_u(const double * __restrict  pk0,
                                              const double * __restrict  pa,
                                              const double * __restrict  ppsii,
                                              const double * __restrict  pphi) FUNC_ATTRIBUTES;

                  /*
                           Disc limit of cylinder (h<<a).
                           Bistatic scattering RCS for cylinder in the disc limit
                           Formula 4.3-24
                      */


                
                   __m512d rcs_f4324_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d psis,
                                            const __m512d phi) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4324_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsis,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4324_zmm8r8_u(const double * __restrict  pk0,
                                              const double * __restrict  pa,
                                              const double * __restrict  ppsis,
                                              const double * __restrict  pphi) FUNC_ATTRIBUTES;


                  /*
                           Disc limit of cylinder (h<<a).
                           Bistatic scattering RCS for cylinder in the disc limit
                           Formula 4.3-25
                   */

  
                 
                   __m512d rcs_f4325_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d psii,
                                            const __m512d psis,
                                            const __m512d phi) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4325_zmm8r8_a(  const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsis,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4325_zmm8r8_u(  const double * __restrict  pk0,
                                                const double * __restrict  pa,
                                                const double * __restrict  ppsis,
                                                const double * __restrict  ppsii,
                                                const double * __restrict  pphi) FUNC_ATTRIBUTES;

                   /*
                          Backscattering RCS for perfectly conducting wire.
                          (2*h>gamma/4)
                          Formula 4.3-29

                     */

                     /*
                          Parameter a1,a2,a3 of equation 4.3-29
                          Formula 4.3-30
                      */


                 
                   __m512d a1_f4330_zmm8r8(const __m512d k0h,
                                           const __m512d psi) FUNC_ATTRIBUTES;

                  
                   __m512d a1_f4330_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const double * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                
                   __m512d a1_f4330_zmm8r8_u(const double * __restrict  pk0h,
                                             const double * __restrict  ppsi) FUNC_ATTRIBUTES;

                  
                   __m512d a2_f4330_zmm8r8(const __m512d k0h,
                                           const __m512d psi) FUNC_ATTRIBUTES;


                   __m512d a2_f4330_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const double * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                 
                   __m512d a2_f4330_zmm8r8_u(const double * __restrict  pk0h,
                                             const double * __restrict  ppsi) FUNC_ATTRIBUTES;

                 
                   __m512d a3_f4330_zmm8r8(const __m512d k0h,
                                           const __m512d psi) FUNC_ATTRIBUTES;


                 
                   __m512d a3_f4330_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const double * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                
                   __m512d a3_f4330_zmm8r8_u(const double * __restrict  pk0h,
                                             const double * __restrict  ppsi) FUNC_ATTRIBUTES;


                    /*
                          Parameter F1,F2 of equation 4.3-29
                          Formula 4.3-31
                      */

                  
                   __m512d F1_f4331_zmm8r8(const __m512d k0a) FUNC_ATTRIBUTES;


                
                   __m512d F1_f4331_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;

                
                   __m512d F1_f4331_zmm8r8_u(const double * __restrict  pk0a) FUNC_ATTRIBUTES;

                 
                   __m512d F2_f4331_zmm8r8(const __m512d k0a) FUNC_ATTRIBUTES;

                  
                   __m512d F2_f4331_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                 
                   __m512d F2_f4331_zmm8r8_u(const double * __restrict  pk0a) FUNC_ATTRIBUTES;

                     /*
                          Parameter (helper) Lambda of equation 4.3-29
                          Formula 4.3-34
                      */


                 
                   __m512d L_f4334_zmm8r8(const __m512d k0h,
                                          const __m512d k0a) FUNC_ATTRIBUTES;


                  
                   __m512d L_f4334_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0h,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                 
                   __m512d L_f4334_zmm8r8_u(const double * __restrict pk0h,
                                            const double * __restrict pk0a) FUNC_ATTRIBUTES;


                   /*
                          Parameter (helper) Sigma of equation 4.3-29
                          Formula 4.3-35
                      */


                
                   __m512d S_f4335_zmm8r8(const __m512d k0a,
                                          const __m512d k0h) FUNC_ATTRIBUTES;

                 
                   __m512d S_f4335_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                            const double * __restrict __ATTR_ALIGN__(64) pk0h) FUNC_ATTRIBUTES;


                 
                   __m512d S_f4335_zmm8r8_u(const double * __restrict  pk0a,
                                            const double * __restrict  pk0h) FUNC_ATTRIBUTES;


                  /*

                           Parameter G1,G2 of equation 4.3-29
                           Formula 4.3-32
                    */


                 
                   __m512d G2_f4332_zmm8r8(const __m512d k0h,
                                           const __m512d k0a) FUNC_ATTRIBUTES;


                 
                   __m512d G2_f4332_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0h,
                                           const double * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                 
                   __m512d G2_f4332_zmm8r8_u(const double * __restrict  pk0h,
                                             const double * __restrict  pk0a) FUNC_ATTRIBUTES;


                  
                   __m512d G1_f4332_zmm8r8(const __m512d k0h,
                                           const __m512d k0a) FUNC_ATTRIBUTES;

                  
                   __m512d G1_f4332_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const double * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                  
                   __m512d G1_f4332_zmm8r8_u(const double * __restrict  pk0h,
                                             const double * __restrict  pk0a) FUNC_ATTRIBUTES;

                     /*

                           Parameter H1,H2 of equation 4.3-29
                           Formula 4.3-33
                    */


                  
                   __m512d H2_f4333_zmm8r8(const __m512d k0h,
                                           const __m512d k0a) FUNC_ATTRIBUTES;


                 
                   __m512d H2_f4333_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const double * __restrict __ATTR_ALIGN__(64) pk0a) FUNC_ATTRIBUTES;


                  
                   __m512d H2_f4333_zmm8r8_u(const double * __restrict  pk0h,
                                             const double * __restrict  pk0a) FUNC_ATTRIBUTES;


               
                   __m512d H1_f4333_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                             const double * __restrict __ATTR_ALIGN__(64) pk0h) FUNC_ATTRIBUTES;


                 
                   __m512d H1_f4333_zmm8r8_u(const double * __restrict  pk0a,
                                             const double * __restrict  pk0h) FUNC_ATTRIBUTES;


                 /*
                          Backscattering RCS for perfectly conducting wire.
                          (2*h>gamma/4)
                          Formula 4.3-29

                     */


                
                   __m512d rcs_f4329_zmm8r8(const __m512d k0,
                                            const __m512d gami,
                                            const __m512d gams,
                                            const __m512d k0h,
                                            const __m512d k0a,
                                            const __m512d psi) FUNC_ATTRIBUTES;


               
                   __m512d rcs_f4329_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pgami,
                                              const double * __restrict __ATTR_ALIGN__(64) pgams,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0h,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4329_zmm8r8_u(const double * __restrict pk0,
                                              const double * __restrict  pgami,
                                              const double * __restrict  pgams,
                                              const double * __restrict  pk0h,
                                              const double * __restrict  pk0a,
                                              const double * __restrict  ppsi) FUNC_ATTRIBUTES;

                  /*

                         Simplified back and bistatic scattering RCS for
                         half and full-wave dipole (2*h == gam0/2, and gam0)
                         gam0 -- wavelength.
                    */


               
                   __m512d rcs_f4337_zmm8r8(const __m512d gammi,
                                            const __m512d gamms,
                                            const __m512d psii,
                                            const __m512d psis,
                                            const __m512d g0 )  FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f4337_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgammi,
                                              const double * __restrict __ATTR_ALIGN__(64) pgamms,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsis,
                                              const double * __restrict __ATTR_ALIGN__(64) pg0 )  FUNC_ATTRIBUTES;
      


                
                   __m512d rcs_f4337_zmm8r8_u(const double * __restrict  pgammi,
                                              const double * __restrict  pgamms,
                                              const double * __restrict  ppsii,
                                              const double * __restrict  ppsis,
                                              const double * __restrict  pg0 )  FUNC_ATTRIBUTES;

                   /*

                         Simplified back and bistatic scattering RCS for
                         Full-wave dipole (2*h == gam0)
                         gam0 -- wavelength.
                    */


                
                   __m512d rcs_f4340_zmm8r8(const __m512d gammi,
                                            const __m512d gamms,
                                            const __m512d psii,
                                            const __m512d psis,
                                            const __m512d g0 )  FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4340_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgammi,
                                              const double * __restrict __ATTR_ALIGN__(64) pgamms,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsis,
                                              const double * __restrict __ATTR_ALIGN__(64) pg0 )  FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f4340_zmm8r8_u(const double * __restrict  pgammi,
                                              const double * __restrict  pgamms,
                                              const double * __restrict  ppsii,
                                              const double * __restrict  ppsis,
                                              const double * __restrict  pg0 ) FUNC_ATTRIBUTES;


                     /*
                           Cylinder length much greater then wavelength (h>>gamma).
                           Biscattering RCS, formula 4.3-43
                      */

                
                   __m512d rcs_f4343_zmm8r8(const __m512d rcs_inf, // rcs of inifnitely long cylinder (section 4.2)
                                            const __m512d k0,
                                            const __m512d h,
                                            const __m512d psis,
                                            const __m512d psii) FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f4343_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) prcs_inf, // rcs of inifnitely long cylinder (section 4.2)
                                              const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) ph,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsis,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsii) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4343_zmm8r8_u(const double * __restrict  prcs_inf, // rcs of inifnitely long cylinder (section 4.2)
                                              const double * __restrict  pk0,
                                              const double * __restrict  ph,
                                              const double * __restrict  ppsis,
                                              const double * __restrict  ppsii) FUNC_ATTRIBUTES;


                  /*
                         General bistatic scattering RCS from long thin wire.
                         Formula 4.3-44
                    */


                 
                   __m512d rcs_f4344_zmm8r8(const __m512d h,
                                            const __m512d k0,
                                            const __m512d k0a,
                                            const __m512d psii,
                                            const __m512d psis,
                                            const __m512d gams,
                                            const __m512d gami) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4344_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64)  ph,
                                              const double * __restrict __ATTR_ALIGN__(64)  pk0,
                                              const double * __restrict __ATTR_ALIGN__(64)  pk0a,
                                              const double * __restrict __ATTR_ALIGN__(64)  ppsii,
                                              const double * __restrict __ATTR_ALIGN__(64)  ppsis,
                                              const double * __restrict __ATTR_ALIGN__(64)  pgams,
                                              const double * __restrict __ATTR_ALIGN__(64)  pgami) FUNC_ATTRIBUTES;



                
                   __m512d rcs_f4344_zmm8r8_u(const double * __restrict   ph,
                                              const double * __restrict   pk0,
                                              const double * __restrict   pk0a,
                                              const double * __restrict   ppsii,
                                              const double * __restrict   ppsis,
                                              const double * __restrict   pgams,
                                              const double * __restrict   pgami) FUNC_ATTRIBUTES;


                    /*

                          General backscatter (only) scattering RCS from long thin wire.
                          Formula 4.3-45
                     */


                 
                   __m512d rcs_f4345_zmm8r8(const __m512d psi,
                                            const __m512d k0a,
                                            const __m512d gami,
                                            const __m512d gams,
                                            const __m512d k0,
                                            const __m512d h) FUNC_ATTRIBUTES;


                  
                   __m512d rcs_f4345_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64)  ph,
                                              const double * __restrict __ATTR_ALIGN__(64)  pk0,
                                              const double * __restrict __ATTR_ALIGN__(64)  pk0a,
                                              const double * __restrict __ATTR_ALIGN__(64)  pgams,
                                              const double * __restrict __ATTR_ALIGN__(64)  pgami,
                                              const double * __restrict __ATTR_ALIGN__(64)  ppsi) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4345_zmm8r8_u(const double * __restrict   ph,
                                              const double * __restrict   pk0,
                                              const double * __restrict   pk0a,
                                              const double * __restrict   pgams,
                                              const double * __restrict   pgami,
                                              const double * __restrict   ppsi) FUNC_ATTRIBUTES;

                  /*
                        Backscattering From a Perfectly Conducting Cylinder With Flat Ends.
                        Helper functions, M1,M2 for the main formula 4.3-48
                        Formula 4.3-50

                   */

                  
                   __m512d M1_f4350_zmm8r8(const __m512d psi) FUNC_ATTRIBUTES;


                 
                   __m512d M1_f4350_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                 
                   __m512d M1_f4350_zmm8r8_u(const double * __restrict  ppsi) FUNC_ATTRIBUTES;


                 
                   __m512d M2_f4350_zmm8r8(const __m512d psi) FUNC_ATTRIBUTES;


                 
                   __m512d M2_f4350_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                
                   __m512d M2_f4350_zmm8r8_u(const double * __restrict  ppsi) FUNC_ATTRIBUTES;


                    /*
                        Backscattering From a Perfectly Conducting Cylinder With Flat Ends.
                        Helper functions, M1,M2 for the main formula 4.3-48
                        Formula 4.3-51

                   */

                   __m512d N1_f4351_zmm8r8(const __m512d psi) FUNC_ATTRIBUTES;


                 
                   __m512d N1_f4351_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                 
                   __m512d N1_f4351_zmm8r8_u(const double * __restrict  ppsi) FUNC_ATTRIBUTES;


                  
                   __m512d N2_f4351_zmm8r8(const __m512d psi) FUNC_ATTRIBUTES;


                   
                   __m512d N2_f4351_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;


                  
                   __m512d N2_f4351_zmm8r8_u(const double * __restrict  ppsi) FUNC_ATTRIBUTES;


                   /*
                        Backscattering From a Perfectly Conducting Cylinder With Flat Ends.
                        Helper functions, M1,M2 for the main formula 4.3-48
                        Formula 4.3-52

                   */


                
                   __m512d G_f4352_zmm8r8(const __m512d psi) FUNC_ATTRIBUTES;


                 
                   __m512d G_f4352_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;

                 
                   __m512d G_f4352_zmm8r8_u(const double * __restrict  ppsi) FUNC_ATTRIBUTES;


                  
                   __m512d F_f4352_zmm8r8(const __m512d psi) FUNC_ATTRIBUTES;


                  
                   __m512d F_f4352_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) ppsi) FUNC_ATTRIBUTES;

                 
                   __m512d F_f4352_zmm8r8_u(const double * __restrict ppsi) FUNC_ATTRIBUTES;

                    /*
                           Scattering From Cylinder Near the Specular Direction.
                           Formula 4.3-53
                      */


                 
                   __m512d rcs_f4353_zmm8r8(const __m512d k0a,
                                            const __m512d k0,
                                            const __m512d h,
                                            const __m512d phi,
                                            const __m512d psii,
                                            const __m512d psis) FUNC_ATTRIBUTES;


                  
                   __m512d rcs_f4353_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) ph,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsis) FUNC_ATTRIBUTES;


                   __m512d rcs_f4353_zmm8r8_u(const double * __restrict  pk0a,
                                              const double * __restrict  pk0,
                                              const double * __restrict  ph,
                                              const double * __restrict  pphi,
                                              const double * __restrict  ppsii,
                                              const double * __restrict  ppsis) FUNC_ATTRIBUTES;


                   /*

                            Specular direction -- RCS.
                            Formula 4.3-54
                       */

                  
                   __m512d rcs_f4354_zmm8r8(const __m512d k0a,
                                            const __m512d h,
                                            const __m512d psii,
                                            const __m512d phi) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4354_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const double * __restrict __ATTR_ALIGN__(64) ph,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsii,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4354_zmm8r8_u(const double * __restrict  pk0a,
                                              const double * __restrict  ph,
                                              const double * __restrict  ppsii,
                                              const double * __restrict  pphi) FUNC_ATTRIBUTES;

                  /*

                         Backscattering direction -- RCS for incidence angles
                         near broadside.
                         Formula 4.3-54
                     */


                
                   __m512d rcs_f4354_zmm8r8(const __m512d k0a,
                                            const __m512d h,
                                            const __m512d k0,
                                            const __m512d psii) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4354_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const double * __restrict __ATTR_ALIGN__(64) ph,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) ppsii) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4354_zmm8r8_u(const double * __restrict  pk0a,
                                              const double * __restrict  ph,
                                              const double * __restrict  pk0,
                                              const double * __restrict  ppsii) FUNC_ATTRIBUTES;

                 /*

                        Broadside (psi == 0) RCS.
                        Formula 4.3-56
                   */


                
                   __m512d rcs_f4356_zmm8r8(const __m512d k0a,
                                            const __m512d h) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4356_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                            const double * __restrict __ATTR_ALIGN__(64) ph) FUNC_ATTRIBUTES;

                
                   __m512d rcs_f4356_zmm8r8_u(const double * __restrict  pk0a,
                                            const double * __restrict  ph) FUNC_ATTRIBUTES;


                  /*
                       Elliptical cylinders.
                   */


                   /*
                         Low-frequency approximations (k0a<0.5, k0b<0.5)
                         TM-case,formula 4.4-11
                    */

                 
                   void TM_f4411_zmm8r8(const __m512d a,
                                         const __m512d b,
                                         const __m512d k0,
                                         __m512d * __restrict TMr,
                                         __m512d * __restrict TMi) FUNC_ATTRIBUTES;


                 
                   void TM_f4411_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                         const double * __restrict __ATTR_ALIGN__(64) pb,
                                         const double * __restrict __ATTR_ALIGN__(64) pk0,
                                         double * __restrict __ATTR_ALIGN__(64) TMr,
                                         double * __restrict __ATTR_ALIGN__(64) TMi) FUNC_ATTRIBUTES;


                   void TM_f4411_zmm8r8_u(const double * __restrict  pa,
                                           const double * __restrict  pb,
                                           const double * __restrict  pk0,
                                           double * __restrict  TMr,
                                           double * __restrict  TMi) FUNC_ATTRIBUTES;


               
                   void TE_f4412_zmm8r8(const __m512d k0a,
                                         const __m512d a,
                                         const __m512d b,
                                         const __m512d phi1,
                                         const __m512d phi2,
                                         __m512d * __restrict TEr,
                                         __m512d * __restrict TEi) FUNC_ATTRIBUTES;


                 
                   void TE_f4412_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const double * __restrict __ATTR_ALIGN__(64) pa,
                                           const double * __restrict __ATTR_ALIGN__(64) pb,
                                           const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                           const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                           double * __restrict __ATTR_ALIGN__(64) TEr,
                                           double * __restrict __ATTR_ALIGN__(64) TEi) FUNC_ATTRIBUTES;


                 
                   void TE_f4412_zmm8r8_u(const double * __restrict  pk0a,
                                           const double * __restrict  pa,
                                           const double * __restrict  pb,
                                           const double * __restrict  pphi1,
                                           const double * __restrict  pphi2,
                                           __m512d * __restrict TEr,
                                           __m512d * __restrict TEi) FUNC_ATTRIBUTES;


                 /*
                       TM-case, RCS.
                       Formula 4.4-13
                  */


               
                   __m512d rcs_f4413_zmm8r8(const __m512d a,
                                            const __m512d b,
                                            const __m512d k0) FUNC_ATTRIBUTES;
                                            


                   __m512d rcs_f4413_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64)  pa,
                                              const double * __restrict __ATTR_ALIGN__(64)  pb,
                                              const double * __restrict __ATTR_ALIGN__(64)  pk0) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4413_zmm8r8_u(const double * __restrict   pa,
                                              const double * __restrict   pb,
                                              const double * __restrict   pk0) FUNC_ATTRIBUTES;


                    /*
                         High frequency approximations (k0a>5, k0b>5)
                         TM-case, formula 4.4-15
                      */



                    /*
                        Helper function for testing the condition of high-frequency limit.
                        Page. 322.

                     */

             
                   __mmask16 
                   TM_f4415_helper_zmm8r8(const __m512d k0,
                                           const __m512d a,
                                           const __m512d phi1,
                                           const __m512d phi2,
                                           const __m512d b) FUNC_ATTRIBUTES;

                   void TM_f4415_zmm8r8(const __m512d phi1,
                                         const __m512d phi2,
                                         const __m512d a,
                                         const __m512d b,
                                         const __m512d k0,
                                         __m512d * __restrict TMr,
                                         __m512d * __restrict TMi,
                                         bool & status) FUNC_ATTRIBUTES;

                 
                   void TM_f4415_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                         const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                         const double * __restrict __ATTR_ALIGN__(64) pa,
                                         const double * __restrict __ATTR_ALIGN__(64) pb,
                                         const double * __restrict __ATTR_ALIGN__(64) pk0,
                                         double * __restrict __ATTR_ALIGN__(64) TMr,
                                         double * __restrict __ATTR_ALIGN__(64) TMi,
                                         bool & status) FUNC_ATTRIBUTES;


                
                   void TM_f4415_zmm8r8_u(const double * __restrict  pphi1,
                                         const double * __restrict  pphi2,
                                         const double * __restrict  pa,
                                         const double * __restrict  pb,
                                         const double * __restrict  pk0,
                                         double * __restrict  TMr,
                                         double * __restrict  TMi,
                                         bool & status) FUNC_ATTRIBUTES;

                    /*
                         High frequency approximations (k0a>5, k0b>5)
                         TE-case, formula 4.4-16
                      */


                
                   void TE_f4416_zmm8r8(const __m512d phi1,
                                         const __m512d phi2,
                                         const __m512d a,
                                         const __m512d b,
                                         const __m512d k0,
                                         __m512d * __restrict TEr,
                                         __m512d * __restrict TEi,
                                         bool & status) FUNC_ATTRIBUTES;

                  
                   void TE_f4416_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                         const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                         const double * __restrict __ATTR_ALIGN__(64) pa,
                                         const double * __restrict __ATTR_ALIGN__(64) pb,
                                         const double * __restrict __ATTR_ALIGN__(64) pk0,
                                         double * __restrict __ATTR_ALIGN__(64) TEr,
                                         double * __restrict __ATTR_ALIGN__(64) TEi,
                                         bool & status) FUNC_ATTRIBUTES;


                 
                   void TE_f4416_zmm8r8_u(const double * __restrict  pphi1,
                                         const double * __restrict  pphi2,
                                         const double * __restrict  pa,
                                         const double * __restrict  pb,
                                         const double * __restrict  pk0,
                                         double * __restrict  TEr,
                                         double * __restrict  TEi,
                                         bool & status) FUNC_ATTRIBUTES;

                 /*

                        Bistatic scattering width.
                        Formula 4.4-19
                   */


                
                   __m512d rcs_f4419_zmm8r8(const __m512d phi1,
                                            const __m512d phi2,
                                            const __m512d a,
                                            const __m512d b) FUNC_ATTRIBUTES;


                  
                   __m512d rcs_f4419_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4419_zmm8r8_u(const double * __restrict  pphi1,
                                              const double * __restrict  pphi2,
                                              const double * __restrict  pa,
                                              const double * __restrict  pb) FUNC_ATTRIBUTES;


                   /*

                          Backscattering width, for phi2 == phi1.
                          Formula 4.4-20
                      */


               
                   __m512d rcs_f4420_zmm8r8(const __m512d a,
                                            const __m512d b,
                                            const __m512d phi) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4420_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4420_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pb,
                                              const double * __restrict  pphi) FUNC_ATTRIBUTES;


                   /*
                        Forward scattering pattern and width.
                        Formula 4.4-23 a scattering amplitude

                    */


                 
                   __mmask16 
                   T_f4423_helper_zmm8r8( const __m512d k0,
                                           const __m512d a,
                                           const __m512d phi1,
                                           const __m512d phi2,
                                           const __m512d b) FUNC_ATTRIBUTES;


                   __m512d T_f4423_zmm8r8(const __m512d a,
                                          const __m512d b,
                                          const __m512d phi1,
                                          const __m512d phi2,
                                          const __m512d k0,
                                          bool & status) FUNC_ATTRIBUTES;


                 
                   __m512d T_f4423_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                          const double * __restrict __ATTR_ALIGN__(64) pb,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                          const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                          const double * __restrict __ATTR_ALIGN__(64) pk0,
                                          bool & status) FUNC_ATTRIBUTES;


                  
                   __m512d T_f4423_zmm8r8_u(const double * __restrict pa,
                                          const double * __restrict  pb,
                                          const double * __restrict  pphi1,
                                          const double * __restrict  pphi2,
                                          const double * __restrict  pk0,
                                          bool & status) FUNC_ATTRIBUTES;


                   /*
                          Scattering width near the forward direction.
                          Formula 4.4-24

                     */


               
                   __m512d rcs_f4424_zmm8r8(const __m512d a,
                                          const __m512d b,
                                          const __m512d phi1,
                                          const __m512d phi2,
                                          const __m512d k0,
                                          bool & status) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4424_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              bool & status) FUNC_ATTRIBUTES;


                
                   __m512d rcs_f4424_zmm8r8_u(const double * __restrict  pa,
                                              const double * __restrict  pb,
                                              const double * __restrict  pphi1,
                                              const double * __restrict  pphi2,
                                              const double * __restrict  pk0,
                                              bool & status) FUNC_ATTRIBUTES;


                   /*
                         Scattering width in the exact forward direction (alpha == 0).
                         Formula 4.4-25
                     */


               
                   __m512d rcs_f4425_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d b,
                                            const __m512d phi) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4425_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64)  pk0,
                                              const double * __restrict __ATTR_ALIGN__(64)  pa,
                                              const double * __restrict __ATTR_ALIGN__(64)  pb,
                                              const double * __restrict __ATTR_ALIGN__(64)  pphi) FUNC_ATTRIBUTES;


                   
                   __m512d rcs_f4425_zmm8r8_u(const double * __restrict   pk0,
                                              const double * __restrict   pa,
                                              const double * __restrict   pb,
                                              const double * __restrict  pphi) FUNC_ATTRIBUTES;


                    /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          TM-case, formula 4.4-26
                     */

                 
                   void TM_f4426_zmm8r8(const __m512d k0,
                                         const __m512d a,
                                         const __m512d b,
                                         const __m512d phi1,
                                         const __m512d phi2,
                                         const __m512d epsr,
                                         const __m512d epsi,
                                         const __m512d mur,
                                         const __m512d mui,
                                         __m512d * __restrict TMr,
                                         __m512d * __restrict TMi) FUNC_ATTRIBUTES;


                
                   void TM_f4426_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                         const double * __restrict __ATTR_ALIGN__(64) pa,
                                         const double * __restrict __ATTR_ALIGN__(64) pb,
                                         const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                         const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                         const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                         const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                         const double * __restrict __ATTR_ALIGN__(64) pmur,
                                         const double * __restrict __ATTR_ALIGN__(64) pmui,
                                         double * __restrict __ATTR_ALIGN__(64) TMr,
                                         double * __restrict __ATTR_ALIGN__(64) TMi) FUNC_ATTRIBUTES;

                   void TM_f4426_zmm8r8_u(const double * __restrict  pk0,
                                         const double * __restrict  pa,
                                         const double * __restrict  pb,
                                         const double * __restrict  pphi1,
                                         const double * __restrict pphi2,
                                         const double * __restrict  pepsr,
                                         const double * __restrict  pepsi,
                                         const double * __restrict  pmur,
                                         const double * __restrict  pmui,
                                         double * __restrict  TMr,
                                         double * __restrict  TMi) FUNC_ATTRIBUTES;

                   /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          TE-case, formula 4.4-27
                     */


              
                   void TE_f4427_zmm8r8(const __m512d k0,
                                         const __m512d a,
                                         const __m512d b,
                                         const __m512d phi1,
                                         const __m512d phi2,
                                         const __m512d epsr,
                                         const __m512d epsi,
                                         const __m512d mur,
                                         const __m512d mui,
                                         __m512d * __restrict TEr,
                                         __m512d * __restrict TEi) FUNC_ATTRIBUTES;

                   void TE_f4427_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                         const double * __restrict __ATTR_ALIGN__(64) pa,
                                         const double * __restrict __ATTR_ALIGN__(64) pb,
                                         const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                         const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                         const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                         const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                         const double * __restrict __ATTR_ALIGN__(64) pmur,
                                         const double * __restrict __ATTR_ALIGN__(64) pmui,
                                         double * __restrict __ATTR_ALIGN__(64) TEr,
                                         double * __restrict __ATTR_ALIGN__(64) TEi) FUNC_ATTRIBUTES;

                 
                   void TE_f4427_zmm8r8_u(const double * __restrict  pk0,
                                         const double * __restrict  pa,
                                         const double * __restrict  pb,
                                         const double * __restrict  pphi1,
                                         const double * __restrict pphi2,
                                         const double * __restrict  pepsr,
                                         const double * __restrict  pepsi,
                                         const double * __restrict pmur,
                                         const double * __restrict  pmui,
                                         double * __restrict  TEr,
                                         double * __restrict  TEi) FUNC_ATTRIBUTES;


                  /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          Bistatic scattering width (RCS).
                          TM-case.
                          Formula 4.4-28
                    */


                 
                   __m512d rcs_f4428_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d b,
                                            const __m512d phi1,
                                            const __m512d phi2,
                                            const __m512d epsr,
                                            const __m512d epsi,
                                            const __m512d mur,
                                            const __m512d mui) FUNC_ATTRIBUTES;

                  
                   __m512d rcs_f4428_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const double * __restrict __ATTR_ALIGN__(64) pmur,
                                              const double * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;

                  
                   __m512d rcs_f4428_zmm8r8_u(const double * __restrict  pk0,
                                              const double * __restrict  pa,
                                              const double * __restrict  pb,
                                              const double * __restrict  pphi1,
                                              const double * __restrict  pphi2,
                                              const double * __restrict  pepsr,
                                              const double * __restrict  pepsi,
                                              const double * __restrict  pmur,
                                              const double * __restrict  pmui) FUNC_ATTRIBUTES;

                  /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          Bistatic scattering width (RCS).
                          TE-case.
                          Formula 4.4-29

                    */


                
                   __m512d rcs_f4429_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d b,
                                         const __m512d phi1,
                                         const __m512d phi2,
                                         const __m512d epsr,
                                         const __m512d epsi,
                                         const __m512d mur,
                                         const __m512d mui) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4429_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const double * __restrict __ATTR_ALIGN__(64) pmur,
                                              const double * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;


                  
                   __m512d rcs_f4429_zmm8r8_u(const double * __restrict pk0,
                                              const double * __restrict  pa,
                                              const double * __restrict pb,
                                              const double * __restrict pphi1,
                                              const double * __restrict  pphi2,
                                              const double * __restrict  pepsr,
                                              const double * __restrict  pepsi,
                                              const double * __restrict  pmur,
                                              const double * __restrict  pmui) FUNC_ATTRIBUTES;

                    /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          Backscattering  width (RCS).
                          TM-case.
                          Formula 4.4-30
                    */


                 
                   __m512d rcs_f4430_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d b,
                                            const __m512d phi1,
                                            const __m512d epsr,
                                            const __m512d epsi,
                                            const __m512d mur,
                                            const __m512d mui) FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f4430_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const double * __restrict __ATTR_ALIGN__(64) pmur,
                                              const double * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;

                
                   __m512d rcs_f4430_zmm8r8_u(const double * __restrict pk0,
                                              const double * __restrict  pa,
                                              const double * __restrict  pb,
                                              const double * __restrict pphi1,
                                              const double * __restrict pepsr,
                                              const double * __restrict  pepsi,
                                              const double * __restrict  pmur,
                                              const double * __restrict  pmui) FUNC_ATTRIBUTES;

                  /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          Backscattering  width (RCS).
                          TE-case.
                          Formula 4.4-31
                    */


                  
                   __m512d rcs_f4431_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d b,
                                            const __m512d phi1,
                                            const __m512d epsr,
                                            const __m512d epsi,
                                            const __m512d mur,
                                            const __m512d mui) FUNC_ATTRIBUTES;

                  
                   __m512d rcs_f4431_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const double * __restrict __ATTR_ALIGN__(64) pmur,
                                              const double * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4431_zmm8r8_u(const double * __restrict  pk0,
                                              const double * __restrict  pa,
                                              const double * __restrict  pb,
                                              const double * __restrict  pphi1,
                                              const double * __restrict  pepsr,
                                              const double * __restrict  pepsi,
                                              const double * __restrict  pmur,
                                              const double * __restrict  pmui) FUNC_ATTRIBUTES;

                 /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          Forward scattering (phi2 = pi+phi1)  width (RCS).
                          TM-case.
                          Formula 4.4-32
                    */


                 
                   __m512d rcs_f4432_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d b,
                                            const __m512d phi1,
                                            const __m512d epsr,
                                            const __m512d epsi,
                                            const __m512d mur,
                                            const __m512d mui) FUNC_ATTRIBUTES;
                   
                   __m512d rcs_f4432_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const double * __restrict __ATTR_ALIGN__(64) pmur,
                                              const double * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;

                 
                   __m512d rcs_f4432_zmm8r8_u(const double * __restrict  pk0,
                                              const double * __restrict  pa,
                                              const double * __restrict  pb,
                                              const double * __restrict  pphi1,
                                              const double * __restrict  pepsr,
                                              const double * __restrict  pepsi,
                                              const double * __restrict  pmur,
                                              const double * __restrict  pmui) FUNC_ATTRIBUTES;

                 /*
                          Infinitely long homogenous cylinder at normal
                          incidence.
                          Low frequency approximation (k0a<0.5,k0b<0.5,k1a<0.5,k1b<0.5)
                          Forward scattering (phi2 = pi+phi1)  width (RCS).
                          TE-case.
                          Formula 4.4-33

                   */


                 
                   __m512d rcs_f4433_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d b,
                                            const __m512d phi1,
                                            const __m512d epsr,
                                            const __m512d epsi,
                                            const __m512d mur,
                                            const __m512d mui) FUNC_ATTRIBUTES;


                  
                   __m512d rcs_f4433_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsr,
                                              const double * __restrict __ATTR_ALIGN__(64) pepsi,
                                              const double * __restrict __ATTR_ALIGN__(64) pmur,
                                              const double * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;


                 
                   __m512d rcs_f4433_zmm8r8_u(const double * __restrict  pk0,
                                              const double * __restrict  pa,
                                              const double * __restrict  pb,
                                              const double * __restrict  pphi1,
                                              const double * __restrict  pepsr,
                                              const double * __restrict  pepsi,
                                              const double * __restrict  pmur,
                                              const double * __restrict  pmui) FUNC_ATTRIBUTES;


                








                  

 



                   






                   

 




     









#endif /*__GMS_RCS_CYLINDER_ZMM8R8_H__*/
