

#ifndef __GMS_RCS_PLANAR_SURF_ZMM16R4_H__
#define __GMS_RCS_PLANAR_SURF_ZMM16R4_H__ 180420231543



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



    const unsigned int GMS_RCS_PLANAR_SURF_ZMM16R4_MAJOR = 1U;
    const unsigned int GMS_RCS_PLANAR_SURF_ZMM16R4_MINOR = 0U;
    const unsigned int GMS_RCS_PLANAR_SURF_ZMM16R4_MICRO = 0U;
    const unsigned int GMS_RCS_PLANAR_SURF_ZMM16R4_FULLVER =
      1000U*GMS_RCS_PLANAR_SURF_ZMM16R4_MAJOR+
      100U*GMS_RCS_PLANAR_SURF_ZMM16R4_MINOR+
      10U*GMS_RCS_PLANAR_SURF_ZMM16R4_MICRO;
    const char * const GMS_RCS_PLANAR_SURF_ZMM16R4_CREATION_DATE = "18-04-2023 15:43 PM +00200 (TUE 18 APR 2023 GMT+2)";
    const char * const GMS_RCS_PLANAR_SURF_ZMM16R4_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_RCS_PLANAR_SURF_ZMM16R4_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_RCS_PLANAR_SURF_ZMM16R4_DESCRIPTION   = "AVX512 optimized Planar Surfaces Radar Cross Section (analytic) functionality.";




#include <immintrin.h>
#include "GMS_kernel_config.h"






                 /*
                       Complex impedances.
                       Formula 7.1-6
                   */


                 
                   void zi_f716_zmm16r4(const __m512 tht,
                                        const __m512 mur,
                                        const __m512 mui,
                                        const __m512 epsr,
                                        const __m512 epsi,
                                        __m512 * __restrict zr,
                                        __m512 * __restrict zi)  FUNC_ATTRIBUTES;


                  
                   void zi_f716_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                        const float * __restrict __ATTR_ALIGN__(64) pmur,
                                        const float * __restrict __ATTR_ALIGN__(64) pmui,
                                        const float * __restrict __ATTR_ALIGN__(64) pepsr,
                                        const float * __restrict __ATTR_ALIGN__(64) pepsi,
                                        float * __restrict __ATTR_ALIGN__(64) zr,
                                        float * __restrict __ATTR_ALIGN__(64)  zi)  FUNC_ATTRIBUTES;


                
                   void zi_f716_zmm16r4_u(const float * __restrict  ptht,
                                        const float * __restrict  pmur,
                                        const float * __restrict  pmui,
                                        const float * __restrict  pepsr,
                                        const float * __restrict  pepsi,
                                        float * __restrict  zr,
                                        float * __restrict   zi)  FUNC_ATTRIBUTES;


                 /*
                          Equivalent complex impedances.
                          Formula 7.1-4
                     */


                 
                   void R_f714_zmm16r4( const __m512 tht1,
                                        const __m512 mur1,
                                        const __m512 mui1,
                                        const __m512 epsr1,
                                        const __m512 epsi1,
                                        const __m512 tht2,
                                        const __m512 mur2,
                                        const __m512 mui2,
                                        const __m512 epsr2,
                                        const __m512 epsi2,
                                        __m512 * __restrict Rr,
                                        __m512 * __restrict Ri)  FUNC_ATTRIBUTES;


                 
                   void R_f714_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                        const float * __restrict __ATTR_ALIGN__(64) pmur1,
                                        const float * __restrict __ATTR_ALIGN__(64) pmui1,
                                        const float * __restrict __ATTR_ALIGN__(64) pepsr1,
                                        const float * __restrict __ATTR_ALIGN__(64) pepsi1,
                                        const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                        const float * __restrict __ATTR_ALIGN__(64) pmur2,
                                        const float * __restrict __ATTR_ALIGN__(64) pmui2,
                                        const float * __restrict __ATTR_ALIGN__(64) pepsr2,
                                        const float * __restrict __ATTR_ALIGN__(64) pepsi2,
                                        float * __restrict __ATTR_ALIGN__(64) Rr,
                                        float * __restrict __ATTR_ALIGN__(64) Ri)  FUNC_ATTRIBUTES;


                 
                   void R_f714_zmm16r4_u( const float * __restrict  ptht1,
                                        const float * __restrict  pmur1,
                                        const float * __restrict  pmui1,
                                        const float * __restrict  pepsr1,
                                        const float * __restrict  pepsi1,
                                        const float * __restrict  ptht2,
                                        const float * __restrict  pmur2,
                                        const float * __restrict  pmui2,
                                        const float * __restrict  pepsr2,
                                        const float * __restrict  pepsi2,
                                        float * __restrict  Rr,
                                        float * __restrict  Ri)  FUNC_ATTRIBUTES;


                  /*
                        Transmission coefficient components.
                        Formula 7.1-5
                    */


                
                   void T_f715_zmm16r4( const __m512 tht1,
                                        const __m512 mur1,
                                        const __m512 mui1,
                                        const __m512 epsr1,
                                        const __m512 epsi1,
                                        const __m512 tht2,
                                        const __m512 mur2,
                                        const __m512 mui2,
                                        const __m512 epsr2,
                                        const __m512 epsi2,
                                        __m512 * __restrict Tr,
                                        __m512 * __restrict Ti)  FUNC_ATTRIBUTES;


                /*
                        Reflection coefficient special cases:
                        1) k1<k2, eps1,eps2 (real), mu1 = m2 = mu0
                        Formula 7.1-17
                   */


               
                   __m512 R_f7117_zmm16r4(const __m512 tht,
                                          const __m512 eps1,
                                          const __m512 eps2)  FUNC_ATTRIBUTES;


                
                   __m512 R_f7117_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                            const float * __restrict __ATTR_ALIGN__(64) peps1,
                                            const float * __restrict __ATTR_ALIGN__(64) peps2)  FUNC_ATTRIBUTES;


                 
                   __m512 R_f7117_zmm16r4_u(const float * __restrict  ptht,
                                            const float * __restrict  peps1,
                                            const float * __restrict  peps2)  FUNC_ATTRIBUTES;


                   /*
                        Reflection coefficient special cases:
                        1) k1<k2, eps1,eps2 (real), mu1 = m2 = mu0
                        Formula 7.1-18
                   */


               
                   __m512 R_f7118_zmm16r4(const __m512 tht,
                                          const __m512 eps1,
                                          const __m512 eps2)  FUNC_ATTRIBUTES;


                 
                   __m512 R_f7118_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                            const float * __restrict __ATTR_ALIGN__(64) peps1,
                                            const float * __restrict __ATTR_ALIGN__(64) peps2)  FUNC_ATTRIBUTES;


                
                   __m512 R_f7118_zmm16r4_u(const float * __restrict  ptht,
                                            const float * __restrict  peps1,
                                            const float * __restrict  peps2)  FUNC_ATTRIBUTES;



                    /*
                        Reflection coefficient special cases:
                        2) k2<k1, eps1,eps2 (real), mu1 = mu2 = mu0
                        Formula 7.1-23
                   */


                  
                   void R_f7123_zmm16r4(const __m512 tht,
                                        const __m512 eps2,
                                        const __m512 eps1,
                                        __m512 * __restrict Rr,
                                        __m512 * __restrict Ri)  FUNC_ATTRIBUTES;


              
                   void R_f7123_zmm16r4_a(  const float * __restrict __ATTR_ALIGN__(64) ptht,
                                            const float * __restrict __ATTR_ALIGN__(64) peps1,
                                            const float * __restrict __ATTR_ALIGN__(64) peps2
                                            float * __restrict __ATTR_ALIGN__(64) Rr,
                                            float * __restrict __ATTR_ALIGN__(64) Ri)  FUNC_ATTRIBUTES;


               
                   void R_f7123_zmm16r4_u(  const float * __restrict  ptht,
                                            const float * __restrict  peps1,
                                            const float * __restrict  peps2
                                            float * __restrict  Rr,
                                            float * __restrict  Ri)  FUNC_ATTRIBUTES;


                    /*
                        Reflection coefficient special cases:
                        2) k2<k1, eps1,eps2 (real), mu1 = mu2 = mu0
                        Formula 7.1-24
                   */


                  
                   void R_f7124_zmm16r4(const __m512 tht,
                                        const __m512 eps2,
                                        const __m512 eps1,
                                        __m512 * __restrict Rr,
                                        __m512 * __restrict Ri)  FUNC_ATTRIBUTES;


                 
                   void R_f7124_zmm16r4_a(  const float * __restrict __ATTR_ALIGN__(64) ptht,
                                            const float * __restrict __ATTR_ALIGN__(64) peps1,
                                            const float * __restrict __ATTR_ALIGN__(64) peps2
                                            float * __restrict __ATTR_ALIGN__(64) Rr,
                                            float * __restrict __ATTR_ALIGN__(64) Ri)  FUNC_ATTRIBUTES;



               
                   void R_f7124_zmm16r4_u(  const float * __restrict  ptht,
                                            const float * __restrict  peps1,
                                            const float * __restrict  peps2
                                            float * __restrict  Rr,
                                            float * __restrict  Ri)  FUNC_ATTRIBUTES;


                  /*
                       Lateral displacement of the incident ray.
                       Formula 7.1-27
                   */

                  
                   __m512 D_f7127_zmm16r4(    const __m512 gam0,
                                              const __m512 tht,
                                              const __m512 eps2,
                                              const __m512 eps1)  FUNC_ATTRIBUTES;


                
                   __m512 D_f7127_zmm16r4_a(    const float * __restrict __ATTR_ALIGN__(64)  pgam0,
                                                const float * __restrict __ATTR_ALIGN__(64)  ptht,
                                                const float * __restrict __ATTR_ALIGN__(64)  peps2,
                                                const float * __restrict __ATTR_ALIGN__(64)  peps1)  FUNC_ATTRIBUTES;


                  
                   __m512 D_f7127_zmm16r4_u(    const float * __restrict   pgam0,
                                                const float * __restrict   ptht,
                                                const float * __restrict   peps2,
                                                const float * __restrict   peps1)  FUNC_ATTRIBUTES;


                    /*
                       Lateral displacement of the incident ray.
                       Formula 7.1-28
                   */


                
                   __m512 D_f7128_zmm16r4(    const __m512 gam0,
                                              const __m512 tht,
                                              const __m512 eps2,
                                              const __m512 eps1)  FUNC_ATTRIBUTES;


                 
                   __m512 D_f7128_zmm16r4_a(    const float * __restrict __ATTR_ALIGN__(64)  pgam0,
                                                const float * __restrict __ATTR_ALIGN__(64)  ptht,
                                                const float * __restrict __ATTR_ALIGN__(64)  peps2,
                                                const float * __restrict __ATTR_ALIGN__(64)  peps1)  FUNC_ATTRIBUTES;


                
                   __m512 D_f7128_zmm16r4_u(    const float * __restrict   pgam0,
                                                const float * __restrict   ptht,
                                                const float * __restrict   peps2,
                                                const float * __restrict   peps1)  FUNC_ATTRIBUTES;

                      /*
                             For (k1/k2)^2*sin^2(theta)<<1 (Simplification
                             of formulae 7.1-9 and 7.1-10).
                             Formula 7.1-29
                        */


                    
                 
                   void R_f7129_zmm16r4(const __m512 tht,
                                        const __m512 mur1,
                                        const __m512 mui1,
                                        const __m512 epsr1,
                                        const __m512 epsi1,
                                        const __m512 mur2,
                                        const __m512 mui2,
                                        const __m512 epsr2,
                                        const __m512 epsi2,
                                        __m512 * __restrict Rr,
                                        __m512 * __restrict Ri)  FUNC_ATTRIBUTES;


                
                   void R_f7129_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur1,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui1,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr1,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi1,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur2,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui2,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr2,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi2,
                                          float * __restrict __ATTR_ALIGN__(64) Rr,
                                          float * __restrict __ATTR_ALIGN__(64) Ri)  FUNC_ATTRIBUTES;


                 
                   void R_f7129_zmm16r4_u(const float * __restrict  ptht,
                                          const float * __restrict  pmur1,
                                          const float * __restrict  pmui1,
                                          const float * __restrict  pepsr1,
                                          const float * __restrict  pepsi1,
                                          const float * __restrict  pmur2,
                                          const float * __restrict  pmui2,
                                          const float * __restrict  pepsr2,
                                          const float * __restrict  pepsi2,
                                          float * __restrict  Rr,
                                          float * __restrict  Ri)  FUNC_ATTRIBUTES;


                  /*
                             For (k1/k2)^2*sin^2(theta)<<1 (Simplification
                             of formulae 7.1-9 and 7.1-10).
                             Formula 7.1-30

                     */

   
                
                   void R_f7130_zmm16r4(const __m512 tht,
                                        const __m512 mur1,
                                        const __m512 mui1,
                                        const __m512 epsr1,
                                        const __m512 epsi1,
                                        const __m512 mur2,
                                        const __m512 mui2,
                                        const __m512 epsr2,
                                        const __m512 epsi2,
                                        __m512 * __restrict Rr,
                                        __m512 * __restrict Ri)  FUNC_ATTRIBUTES;


               
                   void R_f7130_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur1,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui1,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr1,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi1,
                                          const float * __restrict __ATTR_ALIGN__(64) pmur2,
                                          const float * __restrict __ATTR_ALIGN__(64) pmui2,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsr2,
                                          const float * __restrict __ATTR_ALIGN__(64) pepsi2,
                                          float * __restrict __ATTR_ALIGN__(64) Rr,
                                          float * __restrict __ATTR_ALIGN__(64) Ri)  FUNC_ATTRIBUTES;


                 
                   void R_f7130_zmm16r4_u(const float * __restrict  ptht,
                                          const float * __restrict  pmur1,
                                          const float * __restrict  pmui1,
                                          const float * __restrict  pepsr1,
                                          const float * __restrict  pepsi1,
                                          const float * __restrict  pmur2,
                                          const float * __restrict  pmui2,
                                          const float * __restrict  pepsr2,
                                          const float * __restrict  pepsi2,
                                          float * __restrict  Rr,
                                          float * __restrict  Ri)  FUNC_ATTRIBUTES;

                 /*
                       Reflection coefficients for (alpha<cos^2(theta)).
                       Formula 7.2-15
                  */


               
                   __m512 R_f7215_f7216_zmm16r4(const __m512 d,
                                                const __m512 k0,
                                                const __m512 alp,
                                                const __m512 tht)  FUNC_ATTRIBUTES;


               
                   __m512 R_f7215_f7216_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pd,
                                                  const float * __restrict __ATTR_ALIGN__(64) pk0,
                                                  const float * __restrict __ATTR_ALIGN__(64) palp,
                                                  const float * __restrict __ATTR_ALIGN__(64) ptht)  FUNC_ATTRIBUTES;

               
                   __m512 R_f7215_f7216_zmm16r4_u(const float * __restrict  pd,
                                                  const float * __restrict  pk0,
                                                  const float * __restrict  palp,
                                                  const float * __restrict  ptht)  FUNC_ATTRIBUTES;

                    /*
                            Infinite strips, low frequency region.
                            E-field (scattered) along 'z'.
                            Formula 7.4-1
                        */


            
                   void Esz_f741_zmm16r4(const __m512 k0,
                                         const __m512 r,
                                         const __m512 a,
                                         const __m512 tht,
                                         const __m512 Eir,
                                         const __m512 Eii,
                                         __m512 * __restrict Esr,
                                         __m512 * __restrict Esi)  FUNC_ATTRIBUTES;

               
                   void Esz_f741_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           const float * __restrict __ATTR_ALIGN__(64) pEir,
                                           const float * __restrict __ATTR_ALIGN__(64) pEii,
                                           float * __restrict __ATTR_ALIGN__(64) Esr,
                                           float * __restrict __ATTR_ALIGN__(64) Esi)  FUNC_ATTRIBUTES;

                
                   void Esz_f741_zmm16r4_u(const float * __restrict  pk0,
                                           const float * __restrict  pr,
                                           const float * __restrict  pa,
                                           const float * __restrict  ptht,
                                           const float * __restrict  pEir,
                                           const float * __restrict  pEii,
                                           float * __restrict  Esr,
                                           float * __restrict  Esi)  FUNC_ATTRIBUTES;


                     
                    /*
                            Infinite strips, low frequency region.
                            H-field (scattered) along 'z'.
                            Formula 7.4-2
                        */


                
                   void Hsz_f742_zmm16r4(const __m512 k0a,
                                         const __m512 k0r,
                                         const __m512 tht,
                                         const __m512 Hir,
                                         const __m512 Hii,
                                         __m512 * __restrict Hsr,
                                         __m512 * __restrict Hsi)  FUNC_ATTRIBUTES;


                 
                   void Hsz_f742_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0r,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           const float * __restrict __ATTR_ALIGN__(64) pHir,
                                           const float * __restrict __ATTR_ALIGN__(64) pHii,
                                           float * __restrict __ATTR_ALIGN__(64) Hsr,
                                           float * __restrict __ATTR_ALIGN__(64) Hsi)  FUNC_ATTRIBUTES;


               
                   void Hsz_f742_zmm16r4_u(const float * __restrict  pk0a,
                                           const float * __restrict  pk0r,
                                           const float * __restrict  ptht,
                                           const float * __restrict  pHir,
                                           const float * __restrict  pHii,
                                           float * __restrict  Hsr,
                                           float * __restrict  Hsi)  FUNC_ATTRIBUTES;


                  /*
                       The resultant backscatter RCS of perpendicular
                       polarization.
                       Formula 7.4-3
                    */


                  
                   __m512 rcs_f743_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 tht)  FUNC_ATTRIBUTES;

                
                   __m512 rcs_f743_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht)  FUNC_ATTRIBUTES;


             
                   __m512 rcs_f743_zmm16r4_u(const float * __restrict  pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict  ptht)  FUNC_ATTRIBUTES;

                  /*
                        
                       The resultant backscatter RCS of parallel
                       polarization.
                       Formula 7.4-4  

                     */


                  
                   __m512 rcs_f744_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 tht) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f744_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;


             
                   __m512 rcs_f744_zmm16r4_u(const float * __restrict  pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict  ptht) FUNC_ATTRIBUTES;


                  /*
                          General bistatic case.
                          The Rayleigh scattering results.
                          Plane-perpendicular.
                          Formula 7.4-5
                     */


                
                   __m512 rcs_f745_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 tht,
                                           const __m512 tht2) FUNC_ATTRIBUTES;


               
                   __m512 rcs_f745_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht2) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f745_zmm16r4_u(const float * __restrict  pk0,
                                             const float * __restrict pa,
                                             const float * __restrict  ptht,
                                             const float * __restrict  ptht2) FUNC_ATTRIBUTES;

                   /*
                          General bistatic case.
                          The Rayleigh scattering results.
                          Plane-parallel.
                          Formula 7.4-6
                     */


                
                   __m512 rcs_f746_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 tht,
                                           const __m512 tht2) FUNC_ATTRIBUTES;

                 
                   __m512 rcs_f746_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht2) FUNC_ATTRIBUTES;


               
                   __m512 rcs_f746_zmm16r4_u(const float * __restrict  pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict  ptht,
                                             const float * __restrict  ptht2) FUNC_ATTRIBUTES;

                  /*
                         High Frequency Region.
                         For k0a>>1, PO solution of backscatter RCS.
                         Formula 7.4-7
                     */


               
                   __m512 rcs_f747_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 tht) FUNC_ATTRIBUTES;


                
                   __m512 rcs_f747_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f747_zmm16r4_u(const float * __restrict  pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict  ptht) FUNC_ATTRIBUTES;


                 /*
                       Backscattered fields from the edges of strips.
                       Helper function for the formula 7.4-9
                       Electric-field (over z).
                       Formula 7.4-15
                  */


                 
                   void coefg12_f7415_zmm16r4(  const __m512 k0a,
                                                const __m512 tht,
                                                __m512 * __restrict gamm1,
                                                __m512 * __restrict gamm2) FUNC_ATTRIBUTES;

                 
                   void coefg12_f7415_zmm16r4_a(  const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                                const float * __restrict __ATTR_ALIGN__(64) ptht,
                                                float * __restrict __ATTR_ALIGN__(64) gamm1,
                                                float * __restrict __ATTR_ALIGN__(64) gamm2) FUNC_ATTRIBUTES;


                 
                   void coefg12_f7415_zmm16r4_u(  const float * __restrict  pk0a,
                                                const float * __restrict  ptht,
                                                float * __restrict  gamm1,
                                                float * __restrict  gamm2) FUNC_ATTRIBUTES;


                      /*
                       Backscattered fields from the edges of strips.
                       Helper function for the formula 7.4-9
                       Electric-field (over z).
                       Formula 7.4-13
                  */



                
                   void coefA12_f7413_zmm16r4(const __m512 k0a,
                                              const __m512 tht,
                                              __m512 * __restrict A1r,
                                              __m512 * __restrict A1i,
                                              __m512 * __restrict A2r,
                                              __m512 * __restrict A2i) FUNC_ATTRIBUTES;
                                              
                                              
                 
                   void coefA12_f7413_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              float * __restrict __ATTR_ALIGN__(64) A1r,
                                              float * __restrict __ATTR_ALIGN__(64) A1i,
                                              float * __restrict __ATTR_ALIGN__(64) A2r,
                                              float * __restrict __ATTR_ALIGN__(64) A2i) FUNC_ATTRIBUTES;


               
                   void coefA12_f7413_zmm16r4_u(const float * __restrict  pk0a,
                                                const float * __restrict  ptht,
                                                float * __restrict  A1r,
                                                float * __restrict  A1i,
                                                float * __restrict  A2r,
                                                float * __restrict  A2i) FUNC_ATTRIBUTES;


                   /*
                       Backscattered fields from the edges of strips.
                       Helper function for the formula 7.4-9
                       Electric-field (over z).
                       Formula 7.4-14
                  */


                
                   void coefB12_f7414_zmm16r4(const __m512 k0a,
                                              const __m512 tht,
                                              __m512 * __restrict B1r,
                                              __m512 * __restrict B1i,
                                              __m512 * __restrict B2r,
                                              __m512 * __restrict B2i) FUNC_ATTRIBUTES;
                                              

                  
                   void coefB12_f7414_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  pk0a,
                                                const float * __restrict __ATTR_ALIGN__(64)  ptht,
                                                float * __restrict __ATTR_ALIGN__(64) B1r,
                                                float * __restrict __ATTR_ALIGN__(64) B1i,
                                                float * __restrict __ATTR_ALIGN__(64) B2r,
                                                float * __restrict __ATTR_ALIGN__(64) B2i) FUNC_ATTRIBUTES;


                
                   void coefB12_f7414_zmm16r4_u(const float * __restrict   pk0a,
                                                const float * __restrict   ptht,
                                                float * __restrict  B1r,
                                                float * __restrict  B1i,
                                                float * __restrict  B2r,
                                                float * __restrict  B2i) FUNC_ATTRIBUTES;


                  /*
                       Very Important!!
                       Backscattered fields from the edges of strips.
                       Ufimtsev derivation.
                       Electric-field (over z).
                       Formula 7.4-9
                 */


                 
                   void Esz_f749_zmm16r4(const __m512 tht,
                                         const __m512 k0a,
                                         const __m512 k0r,
                                         const __m512 Eir,
                                         const __m512 Eii,
                                         __m512 * __restrict Esr,
                                         __m512 * __restrict Esi) FUNC_ATTRIBUTES;


                 
                   void Esz_f749_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0r,
                                           const float * __restrict __ATTR_ALIGN__(64) pEir,
                                           const float * __restrict __ATTR_ALIGN__(64) pEii,
                                           float * __restrict __ATTR_ALIGN__(64)  Esr,
                                           float * __restrict __ATTR_ALIGN__(64)  Esi) FUNC_ATTRIBUTES;
                                           

               
                   void Esz_f749_zmm16r4_u(const float * __restrict  ptht,
                                           const float * __restrict  pk0a,
                                           const float * __restrict  pk0r,
                                           const float * __restrict  pEir,
                                           const float * __restrict  pEii,
                                           float * __restrict   Esr,
                                           float * __restrict   Esi) FUNC_ATTRIBUTES;

                    /*
                       Very Important!!
                       Backscattered fields from the edges of strips.
                       Ufimtsev derivation.
                       Electric-field (over z).
                       Formula 7.4-10
                 */


                 
                   void Hsz_f749_zmm16r4(const __m512 tht,
                                         const __m512 k0a,
                                         const __m512 k0r,
                                         const __m512 Hir,
                                         const __m512 Hii,
                                         __m512 * __restrict Hsr,
                                         __m512 * __restrict Hsi) FUNC_ATTRIBUTES;


                  
                   void Hsz_f749_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0r,
                                           const float * __restrict __ATTR_ALIGN__(64) pHir,
                                           const float * __restrict __ATTR_ALIGN__(64) pHii,
                                           float * __restrict __ATTR_ALIGN__(64)  Hsr,
                                           float * __restrict __ATTR_ALIGN__(64)  Hsi) FUNC_ATTRIBUTES;

                
                   void Hsz_f749_zmm16r4_u(const float * __restrict  ptht,
                                           const float * __restrict  pk0a,
                                           const float * __restrict  pk0r,
                                           const float * __restrict  pHir,
                                           const float * __restrict  pHii,
                                           float * __restrict __ATTR_ALIGN__(64)  Hsr,
                                           float * __restrict __ATTR_ALIGN__(64)  Hsi) FUNC_ATTRIBUTES;
                       /*
                       Very Important!!
                       Resultant RCS of backscattered fields from the edges of strips.
                       Perpendicular RCS.
                       See: formula 7.4-10, 7.4-9   
                       Formula: 7.4-11
                 */


                 


                    
                   __m512 rcs_f7411_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 tht) FUNC_ATTRIBUTES;

 
                   
                   __m512 rcs_f7411_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                            const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f7411_zmm16r4_u(const float * __restrict pk0,
                                            const float * __restrict  pa,
                                            const float * __restrict  ptht) FUNC_ATTRIBUTES;


                   
                       /*
                       Very Important!!
                       Resultant RCS of backscattered fields from the edges of strips.
                       Parallel RCS.
                       See: formula 7.4-10, 7.4-9   
                       Formula: 7.4-12
                 */


                   
                   __m512 rcs_f7412_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 tht) FUNC_ATTRIBUTES;


                    
                   __m512 rcs_f7412_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f7412_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  ptht) FUNC_ATTRIBUTES;


                   /*
                          
                       Very Important!!
                       Resultant RCS of backscattered fields from the edges of strips.
                       Incident angle very close to zero.
                       See: formula 7.4-10, 7.4-9   
                       Formula: 7.4-16, 7.4-17
              
                     */

                    
                
                   __m512 rcs_f7416_f7417_zmm16r4(const __m512 k0,
                                                  const __m512 a,
                                                  const __m512 tht) FUNC_ATTRIBUTES;
                  


                
                   __m512 rcs_f7416_f7417_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                                    const float * __restrict __ATTR_ALIGN__(64) pa,
                                                    const float * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;


                  
                   __m512 rcs_f7416_f7417_zmm16r4_u(const float * __restrict  pk0,
                                                    const float * __restrict  pa,
                                                    const float * __restrict  ptht) FUNC_ATTRIBUTES;


                       /*
                          
                       Very Important!!
                       Resultant RCS of backscattered fields from the edges of strips.
                       Incident angle at theta = PI/2
                       See: formula 7.4-10, 7.4-9   
                       Formula: 7.4-18 (perpendicula)
              
                     */


               
                   __m512 rcs_f7418_zmm16r4(const __m512 k0,
                                            const __m512 a) FUNC_ATTRIBUTES;


                   
                   __m512 rcs_f7418_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa) FUNC_ATTRIBUTES;

                   
                   __m512 rcs_f7418_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  pa) FUNC_ATTRIBUTES;


                    /*
                          
                       Very Important!!
                       Resultant RCS of backscattered fields from the edges of strips.
                       Incident angle at theta = PI/2
                       See: formula 7.4-10, 7.4-9   
                       Formula: 7.4-19 (parallel)
              
                     */


                 
                   __m512 rcs_f7419_zmm16r4() FUNC_ATTRIBUTES;


                    /*
                          
                       Very Important!!
                       Resultant RCS of backscattered fields from the edges of strips.
                       Incident angle at theta near PI/2
                       See: formula 7.4-10, 7.4-9   
                       Formula: 7.4-20 (perpendicular)
              
                     */


                
                   __m512 rcs_f7420_zmm16r4(const __m512 k0,
                                            const __m512 tht) FUNC_ATTRIBUTES;

                
                   __m512 rcs_f7420_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f7420_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  ptht) FUNC_ATTRIBUTES;


                      /*
                          
                       Very Important!!
                       Resultant RCS of backscattered fields from the edges of strips.
                       Incident angle at theta near PI/2
                       See: formula 7.4-10, 7.4-9   
                       Formula: 7.4-21 (parallel)
              
                     */


                  
                   __m512 rcs_f7421_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 tht) FUNC_ATTRIBUTES;

                
                   __m512 rcs_f7421_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;


              
                   __m512 rcs_f7421_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  ptht) FUNC_ATTRIBUTES;


                   /*
                          Bistatic RCS at high frequencies.
                          Approximation by Sommerfeld-MacDonald technique.
                          Case of parallel RCS.
                          Formula 7.4-22

                      */


                  
                   void rcs_f7422_zmm16r4(const __m512 tht1,
                                          const __m512 tht2,
                                          const __m512 k0,
                                          const __m512 a,
                                          __m512 * __restrict rcs1,
                                          __m512 * __restrict rcs2) FUNC_ATTRIBUTES;

                  
                   void rcs_f7422_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                            const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0,
                                            const float * __restrict __ATTR_ALIGN__(64) pa,
                                            float * __restrict __ATTR_ALIGN__(64) rcs1,
                                            float * __restrict __ATTR_ALIGN__(64) rcs2) FUNC_ATTRIBUTES;

               
                   void rcs_f7422_zmm16r4_u(const float * __restrict  ptht1,
                                            const float * __restrict  ptht2,
                                            const float * __restrict  pk0,
                                            const float * __restrict pa,
                                            float * __restrict  rcs1,
                                            float * __restrict  rcs2) FUNC_ATTRIBUTES;


                  
                   /*
                          Bistatic RCS at high frequencies.
                          Approximation by Sommerfeld-MacDonald technique.
                          Case of perpendicular RCS.
                          Formula 7.4-23

                      */


               
                   void rcs_f7423_zmm16r4(const __m512 tht1,
                                          const __m512 tht2,
                                          const __m512 k0,
                                          const __m512 a,
                                          __m512 * __restrict rcs1,
                                          __m512 * __restrict rcs2) FUNC_ATTRIBUTES;

                  
                   void rcs_f7423_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                            const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0,
                                            const float * __restrict __ATTR_ALIGN__(64) pa,
                                            float * __restrict __ATTR_ALIGN__(64) rcs1,
                                            float * __restrict __ATTR_ALIGN__(64) rcs2) FUNC_ATTRIBUTES;

               
                   void rcs_f7423_zmm16r4_u(const float * __restrict  ptht1,
                                            const float * __restrict  ptht2,
                                            const float * __restrict  pk0,
                                            const float * __restrict  pa,
                                            float * __restrict  rcs1,
                                            float * __restrict  rcs2) FUNC_ATTRIBUTES;

              
                   void rcs_f7422_f7423_zmm16r4(const __m512 tht1,
                                                const __m512 tht2,
                                                const __m512 k0,
                                                const __m512 a,
                                                __m512 * __restrict rcsx1,
                                                __m512 * __restrict rcsx2,
                                                __m512 * __restrict rcsy1,
                                                __m512 * __restrict rcsy2) FUNC_ATTRIBUTES;
                    /*
                            RCS forward direction scattering.
                            Perpendicular and parallel.
                            Formula 7.4-24
                       */

                   /*
                                 Argument checking removed!!
                         */
                
                   __m512 rcs_f7424_zmm16r4(const __m512 tht1,
                                            const __m512 tht2,
                                            const __m512 k0,
                                            const __m512 a) FUNC_ATTRIBUTES;


            
                   __m512 rcs_f7424_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa) FUNC_ATTRIBUTES;

                  
                   __m512 rcs_f7424_zmm16r4_u(const float * __restrict  ptht1,
                                              const float * __restrict  ptht2,
                                              const float * __restrict  pk0,
                                              const float * __restrict  pa) FUNC_ATTRIBUTES;


                   /*
                           Backward hemisphere RCS. for |theta2| < PI/2
                           theta1 != PI/2 (edges interaction neglected).
                           Formula 7.4-27
                     */

                 
                   __m512 rcs_f7427_zmm16r4(const __m512 tht1,
                                            const __m512 tht2,
                                            const __m512 k0,
                                            const __m512 a) FUNC_ATTRIBUTES;

               
                   __m512 rcs_f7427_zmm16r4_a(  const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa) FUNC_ATTRIBUTES;


                    
                   __m512 rcs_f7427_zmm16r4_u(  const float * __restrict  ptht1,
                                                const float * __restrict  ptht2,
                                                const float * __restrict  pk0,
                                                const float * __restrict  pa) FUNC_ATTRIBUTES;


                     /*
                             Flat plates approximations.
                             Low-frequency approximations for k0a<<1,
                             plane wave unit amplitude xz-plane propagating
                             at angle 'theta'.
                             Perpendicular polarization.
                             Theta component.
                             Formula 7.5-1
                        */


             
                   void Eth_f751_zmm16r4(const __m512 tht,
                                         const __m512 phi,
                                         const __m512 k0,
                                         const __m512 r,
                                         const __m512 a,
                                         __m512 * __restrict Esr,
                                         __m512 * __restrict Esi) FUNC_ATTRIBUTES;


               
                   void Eth_f751_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                         const float * __restrict __ATTR_ALIGN__(64) pphi,
                                         const float * __restrict __ATTR_ALIGN__(64) pk0,
                                         const float * __restrict __ATTR_ALIGN__(64) pr,
                                         const float * __restrict __ATTR_ALIGN__(64) pa,
                                         float * __restrict __ATTR_ALIGN__(64) Esr,
                                         float * __restrict __ATTR_ALIGN__(64) Esi) FUNC_ATTRIBUTES;



               
                   void Eth_f751_zmm16r4_u(const float * __restrict ptht,
                                         const float * __restrict  pphi,
                                         const float * __restrict  pk0,
                                         const float * __restrict  pr,
                                         const float * __restrict  pa,
                                         float * __restrict  Esr,
                                         float * __restrict  Esi) FUNC_ATTRIBUTES;


                   /*
                             Flat plates approximations.
                             Low-frequency approximations for k0a<<1,
                             plane wave unit amplitude xz-plane propagating
                             at angle 'theta'.
                             Perpendicular polarization.
                             Phi component.
                             Formula 7.5-1
                      */


                 
                   void Eph_f751_zmm16r4(const __m512 tht,
                                         const __m512 tht2
                                         const __m512 phi,
                                         const __m512 k0,
                                         const __m512 r,
                                         const __m512 a,
                                         __m512 * __restrict Esr,
                                         __m512 * __restrict Esi) FUNC_ATTRIBUTES;


              
                   void Eph_f751_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                         const float * __restrict __ATTR_ALIGN__(64) ptht2
                                         const float * __restrict __ATTR_ALIGN__(64) pphi,
                                         const float * __restrict __ATTR_ALIGN__(64) pk0,
                                         const float * __restrict __ATTR_ALIGN__(64) pr,
                                         const float * __restrict __ATTR_ALIGN__(64) pa,
                                         float * __restrict __ATTR_ALIGN__(64) Esr,
                                         float * __restrict __ATTR_ALIGN__(64) Esi) FUNC_ATTRIBUTES;


                   void Eph_f751_zmm16r4_u(const float * __restrict  ptht,
                                         const float * __restrict  ptht2
                                         const float * __restrict  pphi,
                                         const float * __restrict  pk0,
                                         const float * __restrict  pr,
                                         const float * __restrict  pa,
                                         float * __restrict  Esr,
                                         float * __restrict  Esi) FUNC_ATTRIBUTES;
                 
                 
                    /*
                             Flat plates approximations.
                             Low-frequency approximations for k0a<<1,
                             plane wave unit amplitude xz-plane propagating
                             at angle 'theta'.
                             Parallel polarization.
                             Theta component.
                             Formula 7.5-2
                        */


                  
                   void Eth_f752_zmm16r4(const __m512 tht,
                                         const __m512 tht2,
                                         const __m512 phi,
                                         const __m512 k0,
                                         const __m512 r,
                                         const __m512 a,
                                         __m512 * __restrict Esr,
                                         __m512 * __restrict Esi) FUNC_ATTRIBUTES;
                 
             
                   void Eth_f752_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                         const float * __restrict __ATTR_ALIGN__(64) ptht2
                                         const float * __restrict __ATTR_ALIGN__(64) pphi,
                                         const float * __restrict __ATTR_ALIGN__(64) pk0,
                                         const float * __restrict __ATTR_ALIGN__(64) pr,
                                         const float * __restrict __ATTR_ALIGN__(64) pa,
                                         float * __restrict __ATTR_ALIGN__(64) Esr,
                                         float * __restrict __ATTR_ALIGN__(64) Esi) FUNC_ATTRIBUTES;
                 
                 
                 
                 
                   void Eth_f752_zmm16r4_u(const float * __restrict ptht,
                                         const float * __restrict ptht2
                                         const float * __restrict  pphi,
                                         const float * __restrict  pk0,
                                         const float * __restrict  pr,
                                         const float * __restrict  pa,
                                         float * __restrict Esr,
                                         float * __restrict  Esi) FUNC_ATTRIBUTES;
                 
                 
                 /*
                             Flat plates approximations.
                             Low-frequency approximations for k0a<<1,
                             plane wave unit amplitude xz-plane propagating
                             at angle 'phi'.
                             Parallel polarization.
                             Theta component.
                             Formula 7.5-2
                        */
                        
                        
                 
                   void Eph_f752_zmm16r4(const __m512 tht,
                                         const __m512 phi2,
                                         const __m512 k0,
                                         const __m512 r,
                                         const __m512 a,
                                         __m512 * __restrict Esr,
                                         __m512 * __restrict Esi) FUNC_ATTRIBUTES;
                 
                 
                 
                   void Eph_f752_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) pa,
                                           float * __restrict __ATTR_ALIGN__(64) Esr,
                                           float * __restrict __ATTR_ALIGN__(64) Esi) FUNC_ATTRIBUTES;
                 
                 
                
                   void Eph_f752_zmm16r4_u(const float * __restrict  ptht,
                                           const float * __restrict  pphi2,
                                           const float * __restrict  pk0,
                                           const float * __restrict  pr,
                                           const float * __restrict pa,
                                           float * __restrict Esr,
                                           float * __restrict Esi) FUNC_ATTRIBUTES;
                 
                 
                 /*
                         Backscatter RCS (of 7.5-,7.5-2)
                         Formula: 7.5-3 (perpendicular)
                 */
                 
                 
                 
                   __m512 rcs_f753_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 tht) FUNC_ATTRIBUTES;
                 
                 
                
                   __m512 rcs_f753_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;
                 
                 
                
                   __m512 rcs_f753_zmm16r4_u(const float * __restrict pk0,
                                             const float * __restrict pa,
                                             const float * __restrict  ptht) FUNC_ATTRIBUTES;
                 
                   /*
                         Backscatter RCS (of 7.5-,7.5-2)
                         Formula: 7.5-4 (paralell)
                 */
                 
                 
                
                   __m512 rcs_f754_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 tht) FUNC_ATTRIBUTES;
               
               
              
                   __m512 rcs_f754_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;
               
               
                 
                   __m512 rcs_f754_zmm16r4_u(const float * __restrict  pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict  ptht) FUNC_ATTRIBUTES;
               
               
               /*
                    Normal incidence backscatter RCS.
                    Formula: 7.5-5
               */
               
               
                   __m512 rcs_f755_zmm16r4(const __m512 k0,
                                           const __m512 a) FUNC_ATTRIBUTES;
                 
                 
                
                   __m512 rcs_f755_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa) FUNC_ATTRIBUTES;
                 
                 
               
                   __m512 rcs_f755_zmm16r4_u(const float * __restrict  pk0,
                                             const float * __restrict pa) FUNC_ATTRIBUTES;
                 
                 
                 /*
                       Wedge-on incidence.
                       RCS (perpendicular)
                       Formula: 7.5-6
                 */
                 
                 
               
                   __m512 rcs_f756_zmm16r4(const __m512 k0,
                                           const __m512 a) FUNC_ATTRIBUTES;
                  
                   
                   __m512 rcs_f756_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa) FUNC_ATTRIBUTES;
                  
                  

                    
                
                   __m512 rcs_f756_zmm16r4_u(const float * __restrict  pk0,
                                             const float * __restrict  pa) FUNC_ATTRIBUTES;
                  
                    /*
                       Wedge-on incidence.
                       RCS (parallel)
                       Formula: 7.5-7
                  */
                  
                  
              
                   __m512 rcs_f757_zmm16r4() FUNC_ATTRIBUTES;
                   
                   
                  /*
                          Low-frequency forward scatter RCS for theta2=PI-theta1
                          phi2=PI, perpendicular
                          Formula 7.5-8
                  */
                  
               
                   __m512 rcs_f758_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 tht2) FUNC_ATTRIBUTES;
                                           
                 
                 
                   __m512 rcs_f758_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  pk0,
                                             const float * __restrict __ATTR_ALIGN__(64)  pa,
                                             const float * __restrict __ATTR_ALIGN__(64)  ptht2) FUNC_ATTRIBUTES;
                 
                 
                 
                 
                   __m512 rcs_f758_zmm16r4_u(const float * __restrict  pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict  ptht2) FUNC_ATTRIBUTES;
                 
                 
                 
                   /*
                          Low-frequency forward scatter RCS for theta2=PI-theta1
                          phi2=PI, parallel
                          Formula 7.5-9
                  */ 
                  
                  
                
                   __m512 rcs_f759_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 tht2) FUNC_ATTRIBUTES;
                 
                 
                 
               
                   __m512 rcs_f759_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64)  pk0,
                                             const float * __restrict __ATTR_ALIGN__(64)  pa,
                                             const float * __restrict __ATTR_ALIGN__(64)  ptht2) FUNC_ATTRIBUTES;
                 
                 
                
                   __m512 rcs_f759_zmm16r4_u(const float * __restrict   pk0,
                                             const float * __restrict   pa,
                                             const float * __restrict   ptht2) FUNC_ATTRIBUTES;
                 
                 
                 /*
                       High frequency approximations.
                       Perfectly conducting disc.
                       Geometrical Diifraction backscatter RCS,
                       for 0<|theta|<PI.
                       !!NOT VALID FOR: theta==0, theta==PI/2!!
                       Formula: 7.5-13
                 */
                 
                 
                  // Input argument surpessed for this kernel.
                 
                   __m512 rcs_f7513_zmm16r4(const __m512 k0a,
                                            const __m512 tht) FUNC_ATTRIBUTES;
                 
                 
                  
                 
                 
                 
                   __m512 rcs_f7513_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht) FUNC_ATTRIBUTES;
                 
                 
               
                   __m512 rcs_f7513_zmm16r4_u(const float * __restrict  pk0a,
                                              const float * __restrict  ptht) FUNC_ATTRIBUTES;
                 
                 
                 /*
                     Rectangular plates.
                     Backscatter RCS of perfectly conducting square.
                     Normal incidence.
                     Formula: 7.5-31
                 */
                 
               
                   __m512 rcs_f7531_zmm16r4(const __m512 a, // side length of plate
                                            const __m512 gam0) FUNC_ATTRIBUTES;
                 
                 
                
                   __m512 rcs_f7531_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa, // side length of plate
                                              const float * __restrict __ATTR_ALIGN__(64)  pgam0) FUNC_ATTRIBUTES;
                 
                 
             
                   __m512 rcs_f7531_zmm16r4_u(const float * __restrict  pa, // side length of plate
                                              const float * __restrict  pgam0) FUNC_ATTRIBUTES;
                 
                 
                 
                 /*
                     
                     Arbitrary shape (no-circular) plates high frequency.
                     Backscatter RCS.
                     Normal incidence.
                     Formula: 7.5-32
                 */
                 
                 
                 
                   __m512 rcs_f7532_zmm16r4(const __m512 A,
                                            const __m512 gam0) FUNC_ATTRIBUTES;
                
                
                
                
                   __m512 rcs_f7532_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pA,
                                            const  float * __restrict __ATTR_ALIGN__(64)  gam0) FUNC_ATTRIBUTES;
                
                
                
                
                   __m512 rcs_f7532_zmm16r4_u(const float * __restrict  pA,
                                            const  float * __restrict  gam0) FUNC_ATTRIBUTES;
                
                
                /*
                       Rectangular plate perfectly conducting.
                       Physical Optics backascatter RCS at theta angle.
                       Formula: 7.5-33
                
                */
                
                 
                   __m512 rcs_f7533_zmm16r4(const __m512 A,
                                            const __m512 k0a,
                                            const __m512 k0b,
                                            const __m512 tht,
                                            const __m512 phi,
                                            const __m512 gam0) FUNC_ATTRIBUTES;
                
               
                   __m512 rcs_f7533_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pA,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0b,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi,
                                              const float * __restrict __ATTR_ALIGN__(64) pgam0) FUNC_ATTRIBUTES;
                                              
                
                
                
                   __m512 rcs_f7533_zmm16r4_u(const float * __restrict  pA,
                                              const float * __restrict pk0a,
                                              const float * __restrict pk0b,
                                              const float * __restrict  ptht,
                                              const float * __restrict  pphi,
                                              const float * __restrict  pgam0) FUNC_ATTRIBUTES;
                
                /*
                      Triangular plates.
                      High-frequency backscatter RCS by methods of 
                      Physical Optics.
                      Formula 7.5-59
                */
                
                
                   __m512 rcs_f7559_zmm16r4(const __m512 a,
                                            const __m512 b,
                                            const __m512 gam0,
                                            const __m512 k0a,
                                            const __m512 tht,
                                            const __m512 k0b,
                                            const __m512 phi) FUNC_ATTRIBUTES;
                  
                  
                 
                   __m512 rcs_f7559_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0b,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;
                  
                  
                
                   __m512 rcs_f7559_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict  pgam0,
                                              const float * __restrict  pk0a,
                                              const float * __restrict  ptht,
                                              const float * __restrict  pk0b,
                                              const float * __restrict  pphi) FUNC_ATTRIBUTES;
                  
                  
                   /*
                      Triangular plates.
                      High-frequency backscatter RCS by methods of 
                      Physical Optics (wave incident in plane phi = 0).
                      Formula 7.5-60
                 */
                 
                 
               
                   __m512 rcs_f7560_zmm16r4(const __m512 a,
                                            const __m512 b,
                                            const __m512 gam0,
                                            const __m512 tht,
                                            const __m512 phi,
                                            const __m512 k0a) FUNC_ATTRIBUTES;
                
                
                   __m512 rcs_f7560_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;
                
                
                
                   __m512 rcs_f7560_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict  pgam0,
                                              const float * __restrict  pk0a,
                                              const float * __restrict  ptht,
                                              const float * __restrict  pphi) FUNC_ATTRIBUTES;
                
                
                /*
                      Triangular plates.
                      High-frequency backscatter RCS by methods of 
                      Physical Optics (wave incident in plane phi = PI/2).
                      Formula 7.5-61 
                */
                
                
                 
                   __m512 rcs_f7561_zmm16r4(const __m512 a,
                                            const __m512 b,
                                            const __m512 k0b,
                                            const __m512 tht,
                                            const __m512 phi,
                                            const __m512 gam0) FUNC_ATTRIBUTES;
               
               
               
                
                   __m512 rcs_f7561_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0b,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              const float * __restrict __ATTR_ALIGN__(64) pphi) FUNC_ATTRIBUTES;
               
               
               
                   __m512 rcs_f7561_zmm16r4_u(const float * __restrict  pa,
                                              const float * __restrict  pb,
                                              const float * __restrict  pgam0,
                                              const float * __restrict  pk0b,
                                              const float * __restrict  ptht,
                                              const float * __restrict  pphi) FUNC_ATTRIBUTES;
               
                                            
                  
                



























#endif /*__GMS_RCS_PLANAR_SURF_ZMM16R4_H__*/
