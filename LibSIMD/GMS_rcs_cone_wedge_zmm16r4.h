

#ifndef __GMS_RCS_CONE_WEDGE_ZMM16R4_H__
#define __GMS_RCS_CONE_WEDGE_ZMM16R4_H__ 130320231230


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



    const unsigned int GMS_RCS_CONE_WEDGE_ZMM16R4_MAJOR = 1U;
    const unsigned int GMS_RCS_CONE_WEDGE_ZMM16R4_MINOR = 0U;
    const unsigned int GMS_RCS_CONE_WEDGE_ZMM16R4_MICRO = 0U;
    const unsigned int GMS_RCS_CONE_WEDGE_ZMM16R4_FULLVER =
      1000U*GMS_RCS_CONE_WEDGE_ZMM16R4_MAJOR+
      100U*GMS_RCS_CONE_WEDGE_ZMM16R4_MINOR+
      10U*GMS_RCS_CONE_WEDGE_ZMM16R4_MICRO;
    const char * const GMS_RCS_CONE_WEDGE_ZMM16R4_CREATION_DATE = "13-03-2023 12:30 PM +00200 (MON 13 MAR 2023 GMT+2)";
    const char * const GMS_RCS_CONE_WEDGE_ZMM16R4_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_RCS_CONE_WEDGE_ZMM16R4_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_RCS_CONE_WEDGE_ZMM16R4_DESCRIPTION   = "AVX512 optimized Cones and Wedges Radar Cross Section (analytic) functionality.";



#include <stdint.h>
#include <stdbool.h>
#include <immintrin.h>
#include "GMS_kernel_config.h"




                   /*
                       Small-angle cone (alpha ~ 0).
                       Backscattered RCS.
                       Formula 6.2-12
                   */


                 
                   __m512 rcs_f6212_zmm16r4(const __m512 gam0,
                                            const __m512 alp) 
                                                         FUNC_ATTRIBUTES;

                 
                   __m512 rcs_f6212_zmm16r4_a(const float * __restrict __attribute__((aligned(64))) pgam0,
                                              const float * __restrict __attribute__((aligned(64))) palp) 
                                                         FUNC_ATTRIBUTES;

                  
                   __m512 rcs_f6212_zmm16r4_u(const float * __restrict  pgam0,
                                              const float * __restrict  palp)
                                                         FUNC_ATTRIBUTES;


                     /*
                           Small-angle cone (alpha ~ pi/2).
                           Backscattered RCS.
                           Formula 6.2-13
                       */

                 
                   __m512 rcs_f6213_zmm16r4(const __m512 gam0,
                                            const __m512 alp) 
                                                          FUNC_ATTRIBUTES;


                 
                   __m512 rcs_f6213_zmm16r4_a(const float * __restrict __attribute__((aligned(64))) pgam0,
                                              const float * __restrict __attribute__((aligned(64))) palp) 
                                                          FUNC_ATTRIBUTES;
 

                  
                   __m512 rcs_f6213_zmm16r4_u(const float * __restrict  pgam0,
                                              const float * __restrict  palp) 
                                                          FUNC_ATTRIBUTES;
 

                   /*
                         Backscattering case.
                         E-field scattered for (phi component).
                         Formula 6.2-16
    
                     */


                 
                   void ESph_f6216_zmm16r4(const __m512 k0,
                                           const __m512 r,
                                           const __m512 alp,
                                           const __m512 tht,
                                           __m512 * __restrict ESr,
                                           __m512 * __restrict ESi) 
                                                        FUNC_ATTRIBUTES;

               
                   void ESph_f6216_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) palp,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64) ESi) 
                                                                FUNC_ATTRIBUTES;

                 
                   void ESph_f6216_zmm16r4_u(const float * __restrict  pk0,
                                           const float * __restrict  pr,
                                           const float * __restrict  palp,
                                           const float * __restrict  ptht,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi)
                                                                   FUNC_ATTRIBUTES;

                   /*
                         Bistatic RCS case.
                         E-field scattered for (theta component).
                         Formula 6.2-14
                    */


                
                   void ESth_f6214_zmm16r4(const __m512 k0,
                                           const __m512 r,
                                           const __m512 alp,
                                           const __m512 tht1, //inc
                                           const __m512 tht2  //scat
                                           const __m512 phi1, //inc
                                           const __m512 phi2, //scat
                                           __m512 * __restrict ESr,
                                           __m512 * __restrict ESi) 
                                                           FUNC_ATTRIBUTES;

                 
                   void ESth_f6214_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) palp,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64) ESi) 
                                                             FUNC_ATTRIBUTES;

                
                   void ESth_f6214_zmm16r4_u(const float * __restrict  pk0,
                                           const float * __restrict  pr,
                                           const float * __restrict  palp,
                                           const float * __restrict  ptht1,
                                           const float * __restrict  ptht2,
                                           const float * __restrict  pphi1,
                                           const float * __restrict  pphi2,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi) 
                                                           FUNC_ATTRIBUTES;
                    /*
                           Bistatic RCS case.
                           E-field scattered for (phi component).
                           Formula 6.2-15
                      */


                 
                   void ESph_f6215_zmm16r4(const __m512 k0,
                                           const __m512 r,
                                           const __m512 alp,
                                           const __m512 tht1, //inc
                                           const __m512 tht2  //scat
                                           const __m512 phi1, //inc
                                           const __m512 phi2, //scat
                                           __m512 * __restrict ESr,
                                           __m512 * __restrict ESi) 
                                                           FUNC_ATTRIBUTES;

                 
                   void ESph_f6215_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) palp,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64) ESi) 
                                                                  FUNC_ATTRIBUTES;
                 
                   void ESph_f6215_zmm16r4_u(const float * __restrict  pk0,
                                           const float * __restrict  pr,
                                           const float * __restrict  palp,
                                           const float * __restrict  ptht1,
                                           const float * __restrict  ptht2,
                                           const float * __restrict  pphi1,
                                           const float * __restrict  pphi2,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi) 
                                                                  FUNC_ATTRIBUTES;
                  /*
                           Bistatic RCS case -- Physical Optics approximated.
                           E-field scattered for (theta component).
                           Formula 6.2-17
                    */



                   void ESth_f6217_zmm16r4(const __m512 k0,
                                           const __m512 r,
                                           const __m512 alp,
                                           const __m512 tht,
                                           const __m512 phi,
                                           __m512 * __restrict ESr,
                                           __m512 * __restrict ESi) 
                                                                 FUNC_ATTRIBUTES;


                 
                   void ESth_f6217_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) palp,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64) ESi) 
                                                                  FUNC_ATTRIBUTES;

                
                   void ESth_f6217_zmm16r4_u(const float * __restrict  pk0,
                                           const float * __restrict  pr,
                                           const float * __restrict  palp,
                                           const float * __restrict  ptht,
                                           const float * __restrict  pphi,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi) 
                                                                  FUNC_ATTRIBUTES;   

                    /*
                           Bistatic RCS case -- Physical Optics approximated.
                           E-field scattered for (phi component).
                           Formula 6.2-18
                      */


                
                   void ESph_f6218_zmm16r4(const __m512 k0,
                                           const __m512 r,
                                           const __m512 alp,
                                           const __m512 tht,
                                           const __m512 phi,
                                           __m512 * __restrict ESr,
                                           __m512 * __restrict ESi) 
                                                                  FUNC_ATTRIBUTES;


                 
                   void ESph_f6218_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) palp,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64) ESi) 
                                                                    FUNC_ATTRIBUTES;


                
                   void ESph_f6218_zmm16r4_u(const float * __restrict  pk0,
                                           const float * __restrict  pr,
                                           const float * __restrict  palp,
                                           const float * __restrict  ptht,
                                           const float * __restrict  pphi,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi) 
                                                                    FUNC_ATTRIBUTES;


                    /*
                           Physical-Optics axial-incidence bistatic RCS
                           function of theta for (0 << theta < PI-2*alpha)
                           Formula 6.2-20
                      */


                 
                   __m512 rcs_f6220_zmm16r4(const __m512 gam0,
                                            const __m512 alp,
                                            const __m512 tht) 
                                                                  FUNC_ATTRIBUTES;

                  
                   __m512 rcs_f6220_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                            const float * __restrict __ATTR_ALIGN__(64) palp,
                                            const float * __restrict __ATTR_ALIGN__(64) ptht)
                                                                   FUNC_ATTRIBUTES;   

                  
                   __m512 rcs_f6220_zmm16r4_u(const float * __restrict  pgam0,
                                            const float * __restrict  palp,
                                            const float * __restrict  ptht)
                                                                    FUNC_ATTRIBUTES;   

                     /*
                           Axial incidence backscattering (theta = 0)
                           RCS (Spencer equation).
                           Formula 6.2-22
                       */

                
                   __m512 rcs_f6222_zmm16r4(const __m512 gam0,
                                        const __m512 alp)
                                                                    FUNC_ATTRIBUTES;   

                 
                   __m512 rcs_f6222_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                            const float * __restrict __ATTR_ALIGN__(64) palp)
                                                                     FUNC_ATTRIBUTES;   

                
                   __m512 rcs_f6222_zmm16r4_u(const float * __restrict  pgam0,
                                            const float * __restrict  palp) 
                                                                      FUNC_ATTRIBUTES;   

                    /*
                           Narrow-angle cone.
                           Scattered E-field.
                           Formula 6.2-24
                       */


                 
                   void ES_f6224_zmm16r4(const __m512 k0,
                                         const __m512 z,
                                         const __m512 alp,
                                         const __m512 x,
                                         const __m512 y,
                                         const __m512 z,
                                         __m512 * __restrict xre,
                                         __m512 * __restrict xim,
                                         __m512 * __restrict yre,
                                         __m512 * __restrict yim,
                                         __m512 * __restrict zre,
                                         __m512 * __restrict zim) 
                                                                 FUNC_ATTRIBUTES;   

                
                   void ES_f6224_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pz,
                                           const float * __restrict __ATTR_ALIGN__(64) palp,
                                           const float * __restrict __ATTR_ALIGN__(64) px,
                                           const float * __restrict __ATTR_ALIGN__(64) py,
                                           const float * __restrict __ATTR_ALIGN__(64) pz,
                                           float * __restrict __ATTR_ALIGN__(64) xre,
                                           float * __restrict __ATTR_ALIGN__(64) xim,
                                           float * __restrict __ATTR_ALIGN__(64) yre,
                                           float * __restrict __ATTR_ALIGN__(64) yim,
                                           float * __restrict __ATTR_ALIGN__(64) zre,
                                           float * __restrict __ATTR_ALIGN__(64) zim) 
                                                                  FUNC_ATTRIBUTES;   

                 
                   void ES_f6224_zmm16r4_u(const float * __restrict  pk0,
                                           const float * __restrict pz,
                                           const float * __restrict palp,
                                           const float * __restrict  px,
                                           const float * __restrict  py,
                                           const float * __restrict  pz,
                                           float * __restrict  xre,
                                           float * __restrict  xim,
                                           float * __restrict  yre,
                                           float * __restrict  yim,
                                           float * __restrict  zre,
                                           float * __restrict  zim)
                                                                   FUNC_ATTRIBUTES;   

                     /*
                           Wide-angle cone.
                           Scattered E-field.
                           Formula 6.2-25
                       */


                
                   void ES_f6225_zmm16r4(const __m512 k0,
                                         const __m512 z,
                                         const __m512 alp,
                                         const __m512 x,
                                         const __m512 y,
                                         const __m512 z,
                                         __m512 * __restrict xre,
                                         __m512 * __restrict xim,
                                         __m512 * __restrict yre,
                                         __m512 * __restrict yim,
                                         __m512 * __restrict zre,
                                         __m512 * __restrict zim)
                                                                FUNC_ATTRIBUTES;   

                 
                   void ES_f6225_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pz,
                                           const float * __restrict __ATTR_ALIGN__(64) palp,
                                           const float * __restrict __ATTR_ALIGN__(64) px,
                                           const float * __restrict __ATTR_ALIGN__(64) py,
                                           const float * __restrict __ATTR_ALIGN__(64) pz,
                                           float * __restrict __ATTR_ALIGN__(64) xre,
                                           float * __restrict __ATTR_ALIGN__(64) xim,
                                           float * __restrict __ATTR_ALIGN__(64) yre,
                                           float * __restrict __ATTR_ALIGN__(64) yim,
                                           float * __restrict __ATTR_ALIGN__(64) zre,
                                           float * __restrict __ATTR_ALIGN__(64) zim) 
                                                                FUNC_ATTRIBUTES;   

                
                   void ES_f6225_zmm16r4_u(const float * __restrict  pk0,
                                           const float * __restrict  pz,
                                           const float * __restrict  palp,
                                           const float * __restrict  px,
                                           const float * __restrict  py,
                                           const float * __restrict  pz,
                                           float * __restrict  xre,
                                           float * __restrict  xim,
                                           float * __restrict  yre,
                                           float * __restrict  yim,
                                           float * __restrict  zre,
                                           float * __restrict  zim) 
                                                               FUNC_ATTRIBUTES;      


                   /*
                           Wide-angle cone.
                           Radar Cross Section (angle = 0)
                           Formula 6.2-26
                     */

                
                   __m512 rcs_f6226_zmm16r4(const __m512 gam2,
                                            const __m512 alp) 
                                                                FUNC_ATTRIBUTES;   


                 
                   __m512 rcs_f6226_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam2,
                                            const float * __restrict __ATTR_ALIGN__(64) palp) 
                                                                  FUNC_ATTRIBUTES;   


                
                   __m512 rcs_f6226_zmm16r4_u(const float * __restrict  pgam2,
                                            const float * __restrict  palp)
                                                                   FUNC_ATTRIBUTES;   


                    /*
                          The concave-tip axial-incidence, Physical-Optics RCS.
                          Formula 6.2-29

                      */


                
                   __m512 rcs_f6229_zmm16r4(const __m512 k0,
                                            const __m512 alp,
                                            const __m512 R) 
                                                                     FUNC_ATTRIBUTES;    

                  
                   __m512 rcs_f6229_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) palp,
                                              const float * __restrict __ATTR_ALIGN__(64) pR)
                                                                       FUNC_ATTRIBUTES;   


                 
                   __m512 rcs_f6229_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  palp,
                                              const float * __restrict  pR)
                                                                         FUNC_ATTRIBUTES;   


                    /*
                           RCS of pointed cone and flat plate of radius b.
                           Formula 6.2-30
                      */


                 
                   __m512 rcs_f6230_zmm16r4(const __m512 b,
                                            const __m512 k0) 
                                                                FUNC_ATTRIBUTES;   


                
                   __m512 rcs_f6230_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0)
                                                                 FUNC_ATTRIBUTES;     


                 
                   __m512 rcs_f6230_zmm16r4_u(const float * __restrict  pb,
                                              const float * __restrict  pk0)
                                                                   FUNC_ATTRIBUTES;   


                  /*
                       Physical-Optics approximation of rounded-tip cone,
                       for axial incidence.
                       Formula 6.2-31
                   */


                
                   __m512 rcs_f6231_zmm16r4(const __m512 alp,
                                            const __m512 k0,
                                            const __m512 b) 
                                                                  FUNC_ATTRIBUTES;   


                 
                   __m512 rcs_f6231_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) palp,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0,
                                            const float * __restrict __ATTR_ALIGN__(64) pb) 
                                                                   FUNC_ATTRIBUTES;   


                
                   __m512 rcs_f6231_zmm16r4_u(const float * __restrict  palp,
                                            const float * __restrict  pk0,
                                            const float * __restrict  pb)
                                                                    FUNC_ATTRIBUTES;   


                   /*
                          Not nose-one incidence, for theta << alpha -- 
                          PO approximation for rounded-tip cones.
                          Formula 6.2-23
                      */


                
                   __m512 rcs_f6223_zmm16r4(const __m512 tht,
                                            const __m512 k0,
                                            const __m512 b,
                                            const __m512 a,
                                            const __m512 alp) 
                                                                       FUNC_ATTRIBUTES;   

               
                   __m512 rcs_f6223_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0,
                                            const float * __restrict __ATTR_ALIGN__(64) pb,
                                            const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) palp) 
                                                                        FUNC_ATTRIBUTES;   

              
                   __m512 rcs_f6223_zmm16r4_u(const float * __restrict  ptht,
                                            const float * __restrict  pk0,
                                            const float * __restrict  pb,
                                            const float * __restrict  pa,
                                            const float * __restrict  palp) 
                                                                      FUNC_ATTRIBUTES;   

                  /*
                         Finite cones, approximated solutions.
                         Rayleigh region.
                         RCS.
                         Formula 6.3-4
                     */



                  
              
                   __m512 rcs_f634_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 h) 
                                                            FUNC_ATTRIBUTES;   


              
                   __m512 rcs_f634_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) ph) 
                                                                FUNC_ATTRIBUTES;     


                
                   __m512 rcs_f634_zmm16r4_u(const float * __restrict pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict ph)
                                                              FUNC_ATTRIBUTES;   


                   /*
                          Rayleigh RCS of cone-hemispheroid.
                          Formula 6.3-5
                     */


                 
                   __m512 rcs_f635_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 b,
                                           const __m512 h)
                                                              FUNC_ATTRIBUTES;   


                
                   __m512 rcs_f635_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) pb,
                                             const float * __restrict __ATTR_ALIGN__(64) ph) 
                                                                FUNC_ATTRIBUTES;   


                   __m512 rcs_f635_zmm16r4_u(const float * __restrict  pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict  pb,
                                             const float * __restrict  ph) 
                                                                  FUNC_ATTRIBUTES;   

                  /*
                          Rayleigh RCS of cone-hemispheroid (b == a)
                          Formula 6.3-6
                      */

                
                   __m512 rcs_f636_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 h) 
                                                                  FUNC_ATTRIBUTES;   

                 
                   __m512 rcs_f636_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) ph) 
                                                                   FUNC_ATTRIBUTES;   


               
                   __m512 rcs_f636_zmm16r4_u(const float * __restrict  pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict  ph)
                                                                   FUNC_ATTRIBUTES;   


                   /*
                         Flat-back cone, backscatter RCS.
                         Formula 6.3-9
                   */


               
                   __m512 rcs_f639_zmm16r4(const __m512 gam0,
                                           const __m512 alp,
                                           const __m512 k0,
                                           const __m512 h) 
                                                                    FUNC_ATTRIBUTES;   

                 
                   __m512 rcs_f639_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam0, 
                                             const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) palp,
                                             const float * __restrict __ATTR_ALIGN__(64) ph)
                                                                     FUNC_ATTRIBUTES;   


                
                   __m512 rcs_f639_zmm16r4_u(const float * __restrict  pgam0, 
                                             const float * __restrict  pk0,
                                             const float * __restrict  palp,
                                             const float * __restrict  ph) 
                                                                      FUNC_ATTRIBUTES;   


                   /*
                        Cone tip scattering RCS.
                        Formula 6.3-10
                    */


              
                   __m512 rcs_f6310_zmm16r4(const __m512 gam0,
                                            const __m512 alp) 
                                                                      FUNC_ATTRIBUTES;   

                
                   __m512 rcs_f6310_zmm16r4_a(const float * __restrict  __ATTR_ALIGN__(64) pgam0, 
                                              const float * __restrict  __ATTR_ALIGN__(64) palp) 
                                                                       FUNC_ATTRIBUTES;   


                  
                   __m512 rcs_f6310_zmm16r4_u(const float * __restrict  pgam0, 
                                              const float * __restrict  palp) 
                                                                        FUNC_ATTRIBUTES;   

                   /*
                         Case of flat-back cone joined by the sphere.
                         Axial-incidence backscatter RCS.
                         Formula 6.3-11
                    */

              
                   __m512 rcs_f6311_zmm16r4(const __m512 gam0,
                                            const __m512 alp,
                                            const __m512 k0,
                                            const __m512 h,
                                            const __m512 z1) 
                                                              FUNC_ATTRIBUTES;   


                 
                   __m512 rcs_f6311_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam0, 
                                             const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) palp,
                                             const float * __restrict __ATTR_ALIGN__(64) ph,
                                             const float * __restrict __ATTR_ALIGN__(64) pz1) 
                                                              FUNC_ATTRIBUTES;   

               
                   __m512 rcs_f6311_zmm16r4_u(const float * __restrict  pgam0, 
                                             const float * __restrict  pk0,
                                             const float * __restrict  palp,
                                             const float * __restrict  ph,
                                             const float * __restrict  pz1) 
                                                                   FUNC_ATTRIBUTES;   

                    /*
                          Flat-back cone backscattering RCS.
                          Formula 6.3-14
                     */

              
                   __m512 rcs_f6314_zmm16r4(const __m512 a,
                                            const __m512 alp)
                                                                     FUNC_ATTRIBUTES;     


                   __m512 rcs_f6314_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) palp) 
                                                                      FUNC_ATTRIBUTES;   


                
                   __m512 rcs_f6314_zmm16r4_u( const float * __restrict  pa,
                                             const float * __restrict  palp) 
                                                                        FUNC_ATTRIBUTES;   

                   /*
                          Scattering from a thick cylinder of length
                          being a slant height of the cone of radius
                          (4/9*a*sec(alpha)), i.e. RCS(PI/2-alpha).
                          Formula 6.3-18
                      */


               
                   __m512 rcs_f6318_zmm16r4(const __m512 gam0,
                                            const __m512 k0h,
                                            const __m512 alp) 
                                                                  FUNC_ATTRIBUTES;   

                 
                   __m512 rcs_f6318_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam0, 
                                             const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const float * __restrict __ATTR_ALIGN__(64) palp) 
                                                                   FUNC_ATTRIBUTES;   
                                            



                 
                   __m512 rcs_f6318_zmm16r4_u(const float * __restrict  pgam0, 
                                             const float * __restrict  pk0h,
                                             const float * __restrict  palp) 
                                                                   FUNC_ATTRIBUTES;   


                 /*
                     Fresnel integral component.
                     Helper kernel for formula 6.3-15
                     Formula 6.3-16
                  */


                 
                   void Frho_f6316_zmm16r4(const __m512 xxa,
                                           const __m512 rho,
                                           __m512 * __restrict ssa,
                                           __m512 * __restrict cca) 
                                                                    FUNC_ATTRIBUTES;   


               
                   void Frho_f6316_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pxxa,
                                             const float * __restrict __ATTR_ALIGN__(64) prho,
                                             float * __restrict __ATTR_ALIGN__(64) ssa,
                                             float * __restrict __ATTR_ALIGN__(64) cca)
                                                                     FUNC_ATTRIBUTES;   

                
                   void Frho_f6316_zmm16r4_u(const float * __restrict  pxxa,
                                             const float * __restrict  prho,
                                             float * __restrict __ATTR_ALIGN__(64) ssa,
                                             float * __restrict __ATTR_ALIGN__(64) cca) 
                                                                     FUNC_ATTRIBUTES;   


                   /*
                          Incidence near broadside to the cone - backscatter RCS.
                          Formula 6.3-15
                      */


                
                   __m512 rcs_f6315_zmm16r4(const __m512 rho,
                                            const __m512 gam0,
                                            const __m512 k0h,
                                            const __m512 tht,
                                            const __m512 alp) 
                                                         FUNC_ATTRIBUTES;   

                 
                   __m512 rcs_f6315_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) prho,
                                              const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              const float * __restrict __ATTR_ALIGN__(64) palp)
                                                            FUNC_ATTRIBUTES;   

                
                   __m512 rcs_f6315_zmm16r4_u(const float * __restrict  prho,
                                              const float * __restrict  pgam0,
                                              const float * __restrict  pk0h,
                                              const float * __restrict  ptht,
                                              const float * __restrict  palp) 
                                                             FUNC_ATTRIBUTES;   

                      /*
                          Specular return (RCS) of equivalent cylinder
                          to cone sphere.
                          Formula 6.3-19
                      */


                
                   __m512 rcs_f6319_zmm16r4(const __m512 k0h,
                                            const __m512 alp,
                                            const __m512 a) 
                                                            FUNC_ATTRIBUTES;   

                   __m512 rcs_f6319_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) palp)
                                                             FUNC_ATTRIBUTES;   


                
                   __m512 rcs_f6319_zmm16r4_u(const float * __restrict  pk0h,
                                              const float * __restrict  pa,
                                              const float * __restrict  palp) 
                                                              FUNC_ATTRIBUTES;    


                  /*
                          Width of specular lobe of cylinder formula 6.3-19
                          Formula 6.3-20 
                     */

                    
                  
                   __m512 dpsi_f6320_zmm16r4(const __m512 gam0,
                                             const __m512 h,
                                             const __m512 alp) 
                                                              FUNC_ATTRIBUTES;   

                
                   __m512 dpsi_f6320_zmm16r4_a(const float * __ATTR_ALIGN__(64) pgam0,
                                               const float * __ATTR_ALIGN__(64) ph,
                                               const float * __ATTR_ALIGN__(64) palp) 
                                                               FUNC_ATTRIBUTES;   


                
                   __m512 dpsi_f6320_zmm16r4_u(const float *  pgam0,
                                               const float *  ph,
                                               const float *  palp)
                                                               FUNC_ATTRIBUTES;   


                 


                  /*
                          Helper argument K1 for calculation
                          of 6.3-41.
                          Formula 6.3-42
                     */


                 
                   __m512 K1_f6342_zmm16r4(const __m512 k0a,
                                           const __m512 tht,
                                           const __m512 alp) 
                                                              FUNC_ATTRIBUTES;   


                
                   __m512 K1_f6342_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht,
                                             const float * __restrict __ATTR_ALIGN__(64) palp) 
                                                               FUNC_ATTRIBUTES;   


               
                   __m512 K1_f6342_zmm16r4_u(const float * __restrict  pk0a,
                                             const float * __restrict  ptht,
                                             const float * __restrict  palp)
                                                                FUNC_ATTRIBUTES;   


                   /*
                          Helper argument K1 for calculation
                          of 6.3-41.
                          Formula 6.3-43
                     */


               
                   __m512 K2_f6343_zmm16r4(const __m512 k0,
                                           const __m512 a,
                                           const __m512 Ls,
                                           const __m512 tht,
                                           const __m512 alp)
                                                               FUNC_ATTRIBUTES;   

                
                   __m512 K2_f6343_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) pLs,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht,
                                             const float * __restrict __ATTR_ALIGN__(64) palp) 
                                                               FUNC_ATTRIBUTES;   


               
                   __m512 K2_f6343_zmm16r4_u(const float * __restrict  pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict  pLs,
                                             const float * __restrict  ptht,
                                             const float * __restrict  palp)
                                                              FUNC_ATTRIBUTES;   


                    /*
                          Cylindrical current approximation of
                          flat based truncated cone RCS for
                          perpendicular polarization.
                          Formula 6.3-41 
                    */


                 
                   __m512 rcs_f6341_zmm16r4(const __m512 gam0,
                                            const __m512 tht,
                                            const __m512 alp,
                                            const __m512 Ls,
                                            const __m512 k0,
                                            const __m512 a,
                                            const __m512 a1) 
                                                              FUNC_ATTRIBUTES;   

                 
                   __m512 rcs_f6341_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              const float * __restrict __ATTR_ALIGN__(64) palp,
                                              const float * __restrict __ATTR_ALIGN__(64) pLs,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pa1)
                                                               FUNC_ATTRIBUTES;   

              
                   __m512 rcs_f6341_zmm16r4_u(const float * __restrict  pgam0,
                                              const float * __restrict  ptht,
                                              const float * __restrict  palp,
                                              const float * __restrict  pLs,
                                              const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  pa1) 
                                                              FUNC_ATTRIBUTES;   

                   /*
                          Cylindrical current approximation of
                          flat based pointed cone RCS for
                          perpendicular polarization.
                          Formula 6.3-44
                     */


                  
                   __m512 rcs_f6344_zmm16r4(const __m512 gam0,
                                            const __m512 a,
                                            const __m512 tht,
                                            const __m512 alp,
                                            const __m512 k0) 
                                                      FUNC_ATTRIBUTES;   


                
                   __m512 rcs_f6344_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                            const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) ptht,
                                            const float * __restrict __ATTR_ALIGN__(64) palp,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0)
                                                        FUNC_ATTRIBUTES;   

                   /*
                        Geometrical Diffraction.
                        Flat-backed cone, backscattered RCS.
                        Simplified by neglecting a second order terms of 6.3-49
                        Formula 6.3-50
                    */


                 
                   __m512 rcs_f6350_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 alp)
                                                       FUNC_ATTRIBUTES;   

                   __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f6350_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) palp)
                                                        FUNC_ATTRIBUTES;   

               
                   __m512 rcs_f6350_zmm16r4_u(const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  palp)
                                                         FUNC_ATTRIBUTES;   


                  /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series convergence.
                         Formula 6.3-56
                    */


                  /*
                         Complex 'j' phase associated with each 'j' amplitude.
                         Formula 6.3-58, 6.3-56 (complex exponent term only).
                    */


                   void expj_f6358_zmm16r4(const __m512 k0,
                                         const __m512 beta,
                                         const __m512 a,
                                         const __m512 h,
                                         const __m512 tht,
                                       __m512 * __restrict cer,
                                       __m512 * __restrict cei) 
                                                   FUNC_ATTRIBUTES;   

                  
                   void expj_f6358_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pbeta,
                                           const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) ph,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           float * __restrict __ATTR_ALIGN__(64) cer,
                                           float * __restrict __ATTR_ALIGN__(64) cei) 
                                                   FUNC_ATTRIBUTES;   


                  
                   void expj_f6358_zmm16r4_u(const float * __restrict  pk0,
                                           const float * __restrict  pbeta,
                                           const float * __restrict  pa,
                                           const float * __restrict  ph,
                                           const float * __restrict  ptht,
                                           float * __restrict  cer,
                                           float * __restrict  cei)
                                                    FUNC_ATTRIBUTES;   


                   /*
                         Axially asymetric edge diffraction amplitudes.
                         Part of kernel 6.3-56
                         Formula 6.3-57
                     */


                 
                   __m512 rcs_f6357_zmm16r4(const __m512 alp,
                                            const __m512 tht,
                                            const __m512 beta,
                                            const __m512 beta1,
                                            const __m512 a,
                                            const __m512 k0,
                                            const bool ver ) 
                                                      FUNC_ATTRIBUTES;   


                 
                   __m512 rcs_f6357_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) palp,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              const float * __restrict __ATTR_ALIGN__(64) pbeta,
                                              const float * __restrict __ATTR_ALIGN__(64) pbeta1,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const bool ver) 
                                                       FUNC_ATTRIBUTES;   


                      __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f6357_zmm16r4_u(const float * __restrict  palp,
                                              const float * __restrict  ptht,
                                              const float * __restrict  pbeta,
                                              const float * __restrict  pbeta1,
                                              const float * __restrict  pa,
                                              const float * __restrict  pk0,
                                              const bool ver) 
                                                     FUNC_ATTRIBUTES;   

                       /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- single term only
                    */


                
                   __m512 rcs_f6356_term1_zmm16r4(const __m512 alp,
                                              const __m512 h,
                                              const __m512 beta,
                                              const __m512 beta1,
                                              const __m512 a,
                                              const __m512 k0,
                                              const __m512 tht,
                                              const bool ver) 
                                                    FUNC_ATTRIBUTES;   

                
                   __m512 rcs_f6356_term1_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) palp,
                                                  const float * __restrict __ATTR_ALIGN__(64) ph,
                                                  const float * __restrict __ATTR_ALIGN__(64) pbeta,
                                                  const float * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                  const float * __restrict __ATTR_ALIGN__(64) pa,
                                                  const float * __restrict __ATTR_ALIGN__(64) pk0,
                                                  const float * __restrict __ATTR_ALIGN__(64) ptht,
                                                  const bool ver)
                                                    FUNC_ATTRIBUTES;   


                   __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
                   __m512 rcs_f6356_term1_zmm16r4_u(const float * __restrict  palp,
                                                  const float * __restrict  ph,
                                                  const float * __restrict  pbeta,
                                                  const float * __restrict  pbeta1,
                                                  const float * __restrict  pa,
                                                  const float * __restrict  pk0,
                                                  const float * __restrict  ptht,
                                                  const bool ver) 
                                                     FUNC_ATTRIBUTES;   


                     /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms.
                    */


               
                   __m512 rcs_f6356_nterm_zmm16r4(  const __m512 * __restrict __ATTR_ALIGN__(64) palp,
                                                    const __m512 h,
                                                    const __m512 * __restrict __ATTR_ALIGN__(64) pbeta,
                                                    const __m512 * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                    const __m512 a,
                                                    const __m512 k0,
                                                    const __m512 * __restrict __ATTR_ALIGN__(64) ptht,
                                                    const int32_t n,
                                                    const bool ver) 
                                                          FUNC_ATTRIBUTES;   


                    /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms, unrolled 16x.
                    */

                
                   float rcs_f6356_nterm_u16x_zmm16r4(    const __m512 * __restrict __ATTR_ALIGN__(64) palp,
                                                         const __m512 h,
                                                         const __m512 * __restrict __ATTR_ALIGN__(64) pbeta,
                                                         const __m512 * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                         const __m512 a,
                                                         const __m512 k0,
                                                         const __m512 * __restrict __ATTR_ALIGN__(64) ptht,
                                                         const int32_t n,
                                                         const bool ver) 
                                                     FUNC_ATTRIBUTES;   


                   /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms, unrolled 8x.
                    */


                 
                   float rcs_f6356_nterm_u8x_zmm16r4(    const __m512 * __restrict __ATTR_ALIGN__(64) palp,
                                                         const __m512 h,
                                                         const __m512 * __restrict __ATTR_ALIGN__(64) pbeta,
                                                         const __m512 * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                         const __m512 a,
                                                         const __m512 k0,
                                                         const __m512 * __restrict __ATTR_ALIGN__(64) ptht,
                                                         const int32_t n,
                                                         const bool ver) 
                                                        FUNC_ATTRIBUTES;   

              

                      /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms, unrolled 4x.
                    */


                 
                   float rcs_f6356_nterm_u4x_zmm16r4(    const __m512 * __restrict __ATTR_ALIGN__(64) palp,
                                                         const __m512 h,
                                                         const __m512 * __restrict __ATTR_ALIGN__(64) pbeta,
                                                         const __m512 * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                         const __m512 a,
                                                         const __m512 k0,
                                                         const __m512 * __restrict __ATTR_ALIGN__(64) ptht,
                                                         const int32_t n,
                                                         const bool ver) 
                                                         FUNC_ATTRIBUTES;   

                     /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms, unrolled 2x.
                    */


                 
                   float rcs_f6356_nterm_u2x_zmm16r4(    const __m512 * __restrict __ATTR_ALIGN__(64) palp,
                                                         const __m512 h,
                                                         const __m512 * __restrict __ATTR_ALIGN__(64) pbeta,
                                                         const __m512 * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                         const __m512 a,
                                                         const __m512 k0,
                                                         const __m512 * __restrict __ATTR_ALIGN__(64) ptht,
                                                         const int32_t n,
                                                         const bool ver) 
                                                             FUNC_ATTRIBUTES;   
                       

                    

                   /*
                          Large loop k0a >> 1 , for theta = 0 degree.
                          Formula 6.4-10
                      */


                
                   __m512 rcs_f6410_zmm16r4(const __m512 k0,
                                            const __m512 a,
                                            const __m512 b) 
                                                             FUNC_ATTRIBUTES;   


                   __m512 rcs_f6410_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                            const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pb)
                                                             FUNC_ATTRIBUTES;     


                
                   __m512 rcs_f6410_zmm16r4_u(const float * __restrict  pk0,
                                            const float * __restrict  pa,
                                            const float * __restrict  pb) 
                                                              FUNC_ATTRIBUTES;   

                     /*
                         THe far-zone amplitude for TE-case.
                         For small loop k0a<<1
                         Formula 6.4-11
                      */

                       
                 
                   void Uv_f6411_zmm16r4(const __m512 a,
                                         const __m512 k0,
                                         const __m512 b,
                                         const __m512 tht,
                                         __m512 *  __restrict Uvr,
                                         __m512 *  __restrict Uvi) 
                                                              FUNC_ATTRIBUTES;   


                
                   void Uv_f6411_u16x_zmm16r4(const __m512 a,
                                              const __m512 k0,
                                              const __m512 b,
                                              const __m512 * __restrict __ATTR_ALIGN__(64) tht,
                                              __m512 * __restrict __ATTR_ALIGN__(64) Uvr,
                                              __m512 * __restrict __ATTR_ALIGN__(64) Uvi,
                                              const int32_t n) 
                                                               FUNC_ATTRIBUTES;   

                
                   void Uv_f6411_u8x_zmm16r4( const __m512 a,
                                              const __m512 k0,
                                              const __m512 b,
                                              const __m512 * __restrict __ATTR_ALIGN__(64) tht,
                                              __m512 * __restrict __ATTR_ALIGN__(64) Uvr,
                                              __m512 * __restrict __ATTR_ALIGN__(64) Uvi,
                                              const int32_t n) 
                                                                FUNC_ATTRIBUTES;   


                  
                   void Uv_f6411_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pb,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           float * __restrict __ATTR_ALIGN__(64) Uvr,
                                           float * __restrict __ATTR_ALIGN__(64) Uvi) 
                                                                 FUNC_ATTRIBUTES;   


                
                   void Uv_f6411_zmm16r4_u(const float * __restrict  pa,
                                           const float * __restrict  pk0,
                                           const float * __restrict  pb,
                                           const float * __restrict  ptht,
                                           float * __restrict  Uvr,
                                           float * __restrict  Uvi) 
                                                                   FUNC_ATTRIBUTES;   

                  
                   __m512 rcs_f6412_zmm16r4(const __m512 a,
                                          const __m512 b,
                                          const __m512 k0,
                                          const __m512 tht) 
                                                                    FUNC_ATTRIBUTES;   

                 
                   __m512 rcs_f6412_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pb,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0,
                                            const float * __restrict __ATTR_ALIGN__(64) ptht)
                                                                     FUNC_ATTRIBUTES;    


                 
                   __m512 rcs_f6412_zmm16r4_u(const float * __restrict  pa,
                                            const float * __restrict  pb,
                                            const float * __restrict  pk0,
                                            const float * __restrict  ptht) 
                                                                      FUNC_ATTRIBUTES;   

                 
                                          

                    


                    





                  



















#endif  /*__GMS_RCS_CONE_WEDGE_ZMM16R4_H__*/
