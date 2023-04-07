

#include "GMS_rcs_cone_wedge_zmm8r8.h"
#include "GMS_sleefsimddp.h"
#include "GMS_complex_zmm8r8.h"






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









                   /*
                       Small-angle cone (alpha ~ 0).
                       Backscattered RCS.
                       Formula 6.2-12
                   */


                   __m512d rcs_f6212_zmm8r8(const __m512d gam0,
                                            const __m512d alp) {

                          const __m512d _4pi  = _mm512_set1_pd(12.566370614359172953850573533118);
                          const __m512d _1    = _mm512_set1_pd(1.0f);
                          const __m512d _3    = _mm512_set1_pd(3.0f);
                          register __m512d rcs,gam2,calp,trm1,trm2,trm3,x0;
                          gam2  = _mm512_mul_pd(gam0,gam0);
                          calp  = xcos(alp);
                          trm1  = _mm512_div_pd(gam2,_4pi);
                          x0    = _mm512_sub_pd(_1,calp);
                          trm3  = _mm512_fmadd_pd(_3,x0,_1);
                          trm2  = _mm512_mul_pd(x0,x0);
                          rcs   = _mm512_mul_pd(trm1,_mm512_mul_pd(trm2,trm3));
                          return (rcs);
                  }


              
                   __m512d rcs_f6212_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const float * __restrict __ATTR_ALIGN__(64) palp) {

                          register __m512d gam0 = _mm512_load_pd(&pgam0[0]);
                          register __m512d alp  = _mm512_load_pd(&palp[0]);
                          const __m512d _4pi  = _mm512_set1_pd(12.566370614359172953850573533118);
                          const __m512d _1    = _mm512_set1_pd(1.0f);
                          const __m512d _3    = _mm512_set1_pd(3.0f);
                          register __m512d rcs,gam2,calp,trm1,trm2,trm3,x0;
                          gam2  = _mm512_mul_pd(gam0,gam0);
                          calp  = xcos(alp);
                          trm1  = _mm512_div_pd(gam2,_4pi);
                          x0    = _mm512_sub_pd(_1,calp);
                          trm3  = _mm512_fmadd_pd(_3,x0,_1);
                          trm2  = _mm512_mul_pd(x0,x0);
                          rcs   = _mm512_mul_pd(trm1,_mm512_mul_pd(trm2,trm3));
                          return (rcs);
                  }


                
                   __m512d rcs_f6212_zmm8r8_u(const float * __restrict  pgam0,
                                              const float * __restrict  palp) {

                          register __m512d gam0 = _mm512_loadu_pd(&pgam0[0]);
                          register __m512d alp  = _mm512_loadu_pd(&palp[0]);
                          const __m512d _4pi  = _mm512_set1_pd(12.566370614359172953850573533118);
                          const __m512d _1    = _mm512_set1_pd(1.0f);
                          const __m512d _3    = _mm512_set1_pd(3.0f);
                          register __m512d rcs,gam2,calp,trm1,trm2,trm3,x0;
                          gam2  = _mm512_mul_pd(gam0,gam0);
                          calp  = xcos(alp);
                          trm1  = _mm512_div_pd(gam2,_4pi);
                          x0    = _mm512_sub_pd(_1,calp);
                          trm3  = _mm512_fmadd_pd(_3,x0,_1);
                          trm2  = _mm512_mul_pd(x0,x0);
                          rcs   = _mm512_mul_pd(trm1,_mm512_mul_pd(trm2,trm3));
                          return (rcs);
                  }


                     /*
                           Small-angle cone (alpha ~ pi/2).
                           Backscattered RCS.
                           Formula 6.2-13
                       */

                
                   __m512d rcs_f6213_zmm8r8(const __m512d gam0,
                                            const __m512d alp) {

                          const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                          const __m512d _1    = _mm512_set1_pd(1.0f);
                          register __m512d rcs,calp,calp2,calp4,gam2,trm1,trm2,x0;
                          gam2 = _mm512_mul_pd(gam0,gam0);
                          calp = xcos(alp);
                          x0   = _mm512_div_pd(gam2,_16pi);
                          calp2= _mm512_mul_pd(calp,calp);
                          trm2 = _mm512_sub_pd(_1,_mm512_add_pd(calp2,calp2));
                          calp4= _mm512_mul_pd(calp2,calp2);
                          trm1 = _mm512_mul_pd(x0,calp4);
                          rcs  = _mm512_mul_pd(trm1,trm2);
                          return (rcs);
                  }


                 
                   __m512d rcs_f6213_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const float * __restrict __ATTR_ALIGN__(64) palp) {

                          register __m512d gam0 = _mm512_load_pd(&pgam0[0]);
                          register __m512d alp  = _mm512_load_pd(&palp[0]);

                          const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                          const __m512d _1    = _mm512_set1_pd(1.0f);
                          register __m512d rcs,calp,calp2,calp4,gam2,trm1,trm2,x0;
                          gam2 = _mm512_mul_pd(gam0,gam0);
                          calp = xcos(alp);
                          x0   = _mm512_div_pd(gam2,_16pi);
                          calp2= _mm512_mul_pd(calp,calp);
                          trm2 = _mm512_sub_pd(_1,_mm512_add_pd(calp2,calp2));
                          calp4= _mm512_mul_pd(calp2,calp2);
                          trm1 = _mm512_mul_pd(x0,calp4);
                          rcs  = _mm512_mul_pd(trm1,trm2);
                          return (rcs);
                  }


                
                   __m512d rcs_f6213_zmm8r8_u(const float * __restrict  pgam0,
                                              const float * __restrict  palp) {

                          register __m512d gam0 = _mm512_loadu_pd(&pgam0[0]);
                          register __m512d alp  = _mm512_loadu_pd(&palp[0]);

                          const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                          const __m512d _1    = _mm512_set1_pd(1.0f);
                          register __m512d rcs,calp,calp2,calp4,gam2,trm1,trm2,x0;
                          gam2 = _mm512_mul_pd(gam0,gam0);
                          calp = xcos(alp);
                          x0   = _mm512_div_pd(gam2,_16pi);
                          calp2= _mm512_mul_pd(calp,calp);
                          trm2 = _mm512_sub_pd(_1,_mm512_add_pd(calp2,calp2));
                          calp4= _mm512_mul_pd(calp2,calp2);
                          trm1 = _mm512_mul_pd(x0,calp4);
                          rcs  = _mm512_mul_pd(trm1,trm2);
                          return (rcs);
                  }


                   /*
                         Backscattering case.
                         E-field scattered for (phi component).
                         Formula 6.2-16
    
                     */


                 
                   void ESph_f6216_zmm8r8(const __m512d k0,
                                           const __m512d r,
                                           const __m512d alp,
                                           const __m512d tht,
                                           __m512d * __restrict ESr,
                                           __m512d * __restrict ESi) {
  
                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        const __m512d _3  = _mm512_set1_pd(3.0f);
                        const __m512d _4  = _mm512_set1_pd(4.0f);
                        register __m512d k0r,inv,alph,alphs,ctht,t0r,t0i;
                        register __m512d ctht2,num,den,ear,eai,cer,cei,rat;
                        k0r  = _mm512_mul_pd(k0,r);
                        ctht = xcos(tht);
                        alph = _mm512_mul_pd(alp,hlf);
                        ctht2= _mm512_mul_pd(ctht,ctht);
                        ear  = _mm512_setzero_pd();
                        ctht3= _mm512_mul_pd(ctht2,ctht);
                        eai  = k0r;
                        num  = _mm512_add_pd(_3,ctht2);
                        inv  = _mm512_rcp14_pd(k0r);
                        cexp_zmm8r8(ear,eai,&cer,&cei);
                        den  = _mm512_mul_pd(_4,ctht3);
                        t0r  = _mm512_mul_pd(cer,inv);
                        rat  = _mm512_div_pd(num,den);
                        t0i  = _mm512_mul_pd(cei,inv);
                        alphs= _mm512_mul_pd(_mm512_mul_pd(alph,alph),rat);
                        *ESr = _mm512_mul_pd(t0r,alphs);
                        *ESi = _mm512_mul_pd(t0i,alphs);
                 }


                 
                   void ESph_f6216_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) palp,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64) ESi) {
  
                        register __m512d k0  = _mm512_load_pd(&pk0[0]);
                        register __m512d r   = _mm512_load_pd(&pr[0]);
                        register __m512d alp = _mm512_load_pd(&palp[0]);
                        register __m512d tht = _mm512_load_pd(&ptht[0]);
                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        const __m512d _3  = _mm512_set1_pd(3.0f);
                        const __m512d _4  = _mm512_set1_pd(4.0f);
                        register __m512d k0r,inv,alph,alphs,ctht,t0r,t0i;
                        register __m512d ctht2,num,den,ear,eai,cer,cei,rat;
                        k0r  = _mm512_mul_pd(k0,r);
                        ctht = xcos(tht);
                        alph = _mm512_mul_pd(alp,hlf);
                        ctht2= _mm512_mul_pd(ctht,ctht);
                        ear  = _mm512_setzero_pd();
                        ctht3= _mm512_mul_pd(ctht2,ctht);
                        eai  = k0r;
                        num  = _mm512_add_pd(_3,ctht2);
                        inv  = _mm512_rcp14_pd(k0r);
                        cexp_zmm8r8(ear,eai,&cer,&cei);
                        den  = _mm512_mul_pd(_4,ctht3);
                        t0r  = _mm512_mul_pd(cer,inv);
                        rat  = _mm512_div_pd(num,den);
                        t0i  = _mm512_mul_pd(cei,inv);
                        alphs= _mm512_mul_pd(_mm512_mul_pd(alph,alph),rat);
                        _mm512_store_pd(&ESr[0] ,_mm512_mul_pd(t0r,alphs));
                        _mm512_store_pd(&ESi[0] ,_mm512_mul_pd(t0i,alphs));
                 }


               
                   void ESph_f6216_zmm8r8_u(const float * __restrict  pk0,
                                           const float * __restrict  pr,
                                           const float * __restrict  palp,
                                           const float * __restrict  ptht,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi) {
  
                        register __m512d k0  = _mm512_loadu_pd(&pk0[0]);
                        register __m512d r   = _mm512_loadu_pd(&pr[0]);
                        register __m512d alp = _mm512_loadu_pd(&palp[0]);
                        register __m512d tht = _mm512_loadu_pd(&ptht[0]);
                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        const __m512d _3  = _mm512_set1_pd(3.0f);
                        const __m512d _4  = _mm512_set1_pd(4.0f);
                        register __m512d k0r,inv,alph,alphs,ctht,t0r,t0i;
                        register __m512d ctht2,num,den,ear,eai,cer,cei,rat;
                        k0r  = _mm512_mul_pd(k0,r);
                        ctht = xcos(tht);
                        alph = _mm512_mul_pd(alp,hlf);
                        ctht2= _mm512_mul_pd(ctht,ctht);
                        ear  = _mm512_setzero_pd();
                        ctht3= _mm512_mul_pd(ctht2,ctht);
                        eai  = k0r;
                        num  = _mm512_add_pd(_3,ctht2);
                        inv  = _mm512_rcp14_pd(k0r);
                        cexp_zmm8r8(ear,eai,&cer,&cei);
                        den  = _mm512_mul_pd(_4,ctht3);
                        t0r  = _mm512_mul_pd(cer,inv);
                        rat  = _mm512_div_pd(num,den);
                        t0i  = _mm512_mul_pd(cei,inv);
                        alphs= _mm512_mul_pd(_mm512_mul_pd(alph,alph),rat);
                        _mm512_storeu_pd(&ESr[0] ,_mm512_mul_pd(t0r,alphs));
                        _mm512_storeu_pd(&ESi[0] ,_mm512_mul_pd(t0i,alphs));
                 }


                   /*
                         Bistatic RCS case.
                         E-field scattered for (theta component).
                         Formula 6.2-14
                    */


                 
                   void ESth_f6214_zmm8r8(const __m512d k0,
                                           const __m512d r,
                                           const __m512d alp,
                                           const __m512d tht1, //inc
                                           const __m512d tht2  //scat
                                           const __m512d phi1, //inc
                                           const __m512d phi2, //scat
                                           __m512d * __restrict ESr,
                                           __m512d * __restrict ESi) {

                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        const __m512d _2  = _mm512_set1_pd(2.0f);
                        register __m512d k0r,htht1,htht2,phid,ear,eai,cer,cei;
                        register __m512d sphid,t0r,t0i,num1,den1,num2,den2,inv;
                        register __m512d ctht1,ctht2,sect1,sect2,x0,x1,alp2;
                        register __m512d stht1,stht2,chtht1,chtht2,rat1,rat2,cx0,cx1;
                        phid   = _mm512_sub_pd(phi1,phi2);
                        k0r    = _mm512_mul_pd(k0,r);
                        htht1  = _mm512_mul_pd(tht1,hlf);
                        sphid  = xsin(phid);
                        htht2  = _mm512_mul_pd(tht2,hlf);
                        inv    = _mm512_rcp14_pd(k0r);
                        ear    = _mm512_setzero_pd();
                        ctht1  = xcos(tht1);
                        cx1    = _mm512_mul_pd(ctht1);
                        x0     = _mm512_mul_pd(alp,hlf);
                        ctht2  = xcos(tht2);
                        cx2    = _mm512_mul_pd(ctht2,ctht2);
                        alp2   = _mm512_mul_pd(x0,x0);
                        chtht1 = xcos(htht1);
                        eai    = k0r;
                        cexp_zmm8r8(ear,eai,&cer,&cei);
                        sect1  = _mm512_rcp14_pd(chtht1);
                        den1   = _mm512_add_pd(ctht1,ctht2);
                        stht1  = xsin(tht1);
                        t0r    = _mm512_mul_pd(cer,inv);
                        chtht2 = xcos(htht2);
                        t0i    = _mm512_mul_pd(cei,inv);
                        sect2  = _mm512_rcp14_pd(chtht2);
                        stht2  = xsin(tht2);
                        num2   = _mm512_fmadd_pd(stht1,stht1,_mm512_mul_pd(stht2,stht2));
                        num1   = _mm512_fmadd_pd(sect1,sect1,_mm512_mul_pd(sect2,sect2));
                        x0     = _mm512_mul_pd(sphid,alp2);
                        rat1   = _mm512_div_pd(num1,den1);
                        x1     = _mm512_mul_pd(_2,_mm512_mul_pd(cx1,cx2));
                        sect1  = _mm512_add_pd(ctht1,ctht2);
                        sect2  = _mm512_mul_pd(sect1,sect1);
                        cer    = _mm512_mul_pd(t0r,x0);
                        den2   = _mm512_mul_pd(x1,sect2);
                        cei    = _mm512_mul_pd(t0i,x0);
                        rat2   = _mm512_div_pd(num2,den2);
                        *ESr   = _mm512_fmadd_pd(cer,rat1,rat2);
                        *ESi   = _mm512_fmadd_pd(cei,rat1,rat2);
                }


                
                   void ESth_f6214_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) palp,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64) ESi) {

                        register __m512d k0   = _mm512_load_pd(&pk0[0]);
                        register __m512d r    = _mm512_load_pd(&pr[0]);
                        register __m512d alp  = _mm512_load_pd(&palp[0]);
                        register __m512d tht1 = _mm512_load_pd(&ptht1[0]);
                        register __m512d tht2 = _mm512_load_pd(&ptht2[0]);
                        register __m512d phi1 = _mm512_load_pd(&pphi1[0]);
                        register __m512d phi2 = _mm512_load_pd(&pphi2[0]);
                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        const __m512d _2  = _mm512_set1_pd(2.0f);
                        register __m512d k0r,htht1,htht2,phid,ear,eai,cer,cei;
                        register __m512d sphid,t0r,t0i,num1,den1,num2,den2,inv;
                        register __m512d ctht1,ctht2,sect1,sect2,x0,x1,alp2;
                        register __m512d stht1,stht2,chtht1,chtht2,rat1,rat2,cx0,cx1;
                        phid   = _mm512_sub_pd(phi1,phi2);
                        k0r    = _mm512_mul_pd(k0,r);
                        htht1  = _mm512_mul_pd(tht1,hlf);
                        sphid  = xsin(phid);
                        htht2  = _mm512_mul_pd(tht2,hlf);
                        inv    = _mm512_rcp14_pd(k0r);
                        ear    = _mm512_setzero_pd();
                        ctht1  = xcos(tht1);
                        cx1    = _mm512_mul_pd(ctht1);
                        x0     = _mm512_mul_pd(alp,hlf);
                        ctht2  = xcos(tht2);
                        cx2    = _mm512_mul_pd(ctht2,ctht2);
                        alp2   = _mm512_mul_pd(x0,x0);
                        chtht1 = xcos(htht1);
                        eai    = k0r;
                        cexp_zmm8r8(ear,eai,&cer,&cei);
                        sect1  = _mm512_rcp14_pd(chtht1);
                        den1   = _mm512_add_pd(ctht1,ctht2);
                        stht1  = xsin(tht1);
                        t0r    = _mm512_mul_pd(cer,inv);
                        chtht2 = xcos(htht2);
                        t0i    = _mm512_mul_pd(cei,inv);
                        sect2  = _mm512_rcp14_pd(chtht2);
                        stht2  = xsin(tht2);
                        num2   = _mm512_fmadd_pd(stht1,stht1,_mm512_mul_pd(stht2,stht2));
                        num1   = _mm512_fmadd_pd(sect1,sect1,_mm512_mul_pd(sect2,sect2));
                        x0     = _mm512_mul_pd(sphid,alp2);
                        rat1   = _mm512_div_pd(num1,den1);
                        x1     = _mm512_mul_pd(_2,_mm512_mul_pd(cx1,cx2));
                        sect1  = _mm512_add_pd(ctht1,ctht2);
                        sect2  = _mm512_mul_pd(sect1,sect1);
                        cer    = _mm512_mul_pd(t0r,x0);
                        den2   = _mm512_mul_pd(x1,sect2);
                        cei    = _mm512_mul_pd(t0i,x0);
                        rat2   = _mm512_div_pd(num2,den2);
                        _mm512_store_pd(&ESr[0] ,_mm512_fmadd_pd(cer,rat1,rat2));
                        _mm512_store_pd(&ESi[0] ,_mm512_fmadd_pd(cei,rat1,rat2));
                }


                
                   void ESth_f6214_zmm8r8_u(const float * __restrict  pk0,
                                           const float * __restrict  pr,
                                           const float * __restrict  palp,
                                           const float * __restrict  ptht1,
                                           const float * __restrict  ptht2,
                                           const float * __restrict  pphi1,
                                           const float * __restrict  pphi2,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi) {

                        register __m512d k0   = _mm512_loadu_pd(&pk0[0]);
                        register __m512d r    = _mm512_loadu_pd(&pr[0]);
                        register __m512d alp  = _mm512_loadu_pd(&palp[0]);
                        register __m512d tht1 = _mm512_loadu_pd(&ptht1[0]);
                        register __m512d tht2 = _mm512_loadu_pd(&ptht2[0]);
                        register __m512d phi1 = _mm512_loadu_pd(&pphi1[0]);
                        register __m512d phi2 = _mm512_loadu_pd(&pphi2[0]);
                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        const __m512d _2  = _mm512_set1_pd(2.0f);
                        register __m512d k0r,htht1,htht2,phid,ear,eai,cer,cei;
                        register __m512d sphid,t0r,t0i,num1,den1,num2,den2,inv;
                        register __m512d ctht1,ctht2,sect1,sect2,x0,x1,alp2;
                        register __m512d stht1,stht2,chtht1,chtht2,rat1,rat2,cx0,cx1;
                        phid   = _mm512_sub_pd(phi1,phi2);
                        k0r    = _mm512_mul_pd(k0,r);
                        htht1  = _mm512_mul_pd(tht1,hlf);
                        sphid  = xsin(phid);
                        htht2  = _mm512_mul_pd(tht2,hlf);
                        inv    = _mm512_rcp14_pd(k0r);
                        ear    = _mm512_setzero_pd();
                        ctht1  = xcos(tht1);
                        cx1    = _mm512_mul_pd(ctht1);
                        x0     = _mm512_mul_pd(alp,hlf);
                        ctht2  = xcos(tht2);
                        cx2    = _mm512_mul_pd(ctht2,ctht2);
                        alp2   = _mm512_mul_pd(x0,x0);
                        chtht1 = xcos(htht1);
                        eai    = k0r;
                        cexp_zmm8r8(ear,eai,&cer,&cei);
                        sect1  = _mm512_rcp14_pd(chtht1);
                        den1   = _mm512_add_pd(ctht1,ctht2);
                        stht1  = xsin(tht1);
                        t0r    = _mm512_mul_pd(cer,inv);
                        chtht2 = xcos(htht2);
                        t0i    = _mm512_mul_pd(cei,inv);
                        sect2  = _mm512_rcp14_pd(chtht2);
                        stht2  = xsin(tht2);
                        num2   = _mm512_fmadd_pd(stht1,stht1,_mm512_mul_pd(stht2,stht2));
                        num1   = _mm512_fmadd_pd(sect1,sect1,_mm512_mul_pd(sect2,sect2));
                        x0     = _mm512_mul_pd(sphid,alp2);
                        rat1   = _mm512_div_pd(num1,den1);
                        x1     = _mm512_mul_pd(_2,_mm512_mul_pd(cx1,cx2));
                        sect1  = _mm512_add_pd(ctht1,ctht2);
                        sect2  = _mm512_mul_pd(sect1,sect1);
                        cer    = _mm512_mul_pd(t0r,x0);
                        den2   = _mm512_mul_pd(x1,sect2);
                        cei    = _mm512_mul_pd(t0i,x0);
                        rat2   = _mm512_div_pd(num2,den2);
                        _mm512_storeu_pd(&ESr[0] ,_mm512_fmadd_pd(cer,rat1,rat2));
                        _mm512_storeu_pd(&ESi[0] ,_mm512_fmadd_pd(cei,rat1,rat2));
                }


                    /*
                           Bistatic RCS case.
                           E-field scattered for (phi component).
                           Formula 6.2-15
                      */


                 
                   void ESph_f6215_zmm8r8(const __m512d k0,
                                           const __m512d r,
                                           const __m512d alp,
                                           const __m512d tht1, //inc
                                           const __m512d tht2  //scat
                                           const __m512d phi1, //inc
                                           const __m512d phi2, //scat
                                           __m512d * __restrict ESr,
                                           __m512d * __restrict ESi) {

                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        const __m512d _2  = _mm512_set1_pd(2.0f);
                        const __m512d _16 = _mm512_set1_pd(16.0f);
                        register __m512d k0r,inv,alph,ear,eai,cer,cei;
                        register __m512d t0r,t0i,trm1,trm2,trm3,trm4;
                        register __m512d stht1,stht2,ctht1,ctht2;
                        register __m512d htht1,htht2,chtht1,chtht2;
                        register __m512d shtht1,shtht2,x0,x1,phid,cphid;
                        register __m512d ctht12,sect1,sect2,x2,x3,cthtp3;
                        k0r   = _mm512_mul_pd(k0,r);
                        stht1 = xsin(tht1);
                        alph  = _mm512_mul_pd(alp,hlf);
                        ctht1 = xcos(tht1);
                        phid  = _mm512_sub_pd(phi1,phi2);
                        stht2 = xsin(tht2);
                        htht1 = _mm512_mul_pd(tht1,hlf);
                        ctht2 = xcos(tht2);
                        ear   = _mm512_setzero_pd();
                        htht2 = _mm512_mul_pd(tht2);
                        eai   = k0r;
                        inv   = _mm512_rcp14_pd(k0r);
                        ctht12= _mm512_add_pd(ctht1,ctht2);
                        cexp_zmm8r8(ear,eai,&t0r,&t0i);
                        x0    = _mm512_fmadd_pd(stht2,stht1,stht1);
                        chtht1= xcos(htht1);
                        cer   = _mm512_mul_pd(_mm512_mul_pd(t0r,inv),alph);
                        cthtp3= _mm512_mul_pd(ctht12,_mm512_mul_pd(ctht12,ctht12));
                        cei   = _mm512_mul_pd(_mm512_mul_pd(t0i,inv),alph);
                        chtht2= xcos(htht2);
                        trm1  = _mm512_div_pd(x0,cthtp3);
                        x2    = _mm512_rcp14_pd(chtht1);
                        shtht1= xsin(htht1);
                        x3    = _mm512_rcp14_pd(chtht2);
                        shtht2= xsin(htht2);
                        x0    = _mm512_fmadd_pd(x2,x2,_mm512_mul_pd(x3,x3));
                        trm2  = _mm512_div_pd(x0,ctht12);
                        x1    = _mm512_fmadd_pd(stht1,stht1,_mm512_mul_pd(stht2,stht2));
                        x2    = _mm512_mul_pd(_2,_mm512_mul_pd(_mm512_mul_pd(chtht1,chtht1),
                                              _mm512_mul_pd(chtht2,chtht2)));
                        x3    = _mm512_mul_pd(ctht12,ctht12);
                        t0r   = _mm512_mul_pd(x2,x3);
                        trm3  = _mm512_div_pd(x1,t0r);
                        x0    = _mm512_mul_pd(_16,_mm512_mul_pd(shtht1,shtht1));
                        cphid = xcos(phid);
                        x1    = _mm512_mul_pd(shtht2,shtht2);
                        x2    = _mm512_mul_pd(x0,x1);
                        trm4  = _mm512_div_pd(x2,cthtp3);
                        x3    = _mm512_add_pd(trm1,cphid);
                        x4    = _mm512_add_pd(trm2,_mm512_add_pd(trm3,trm4));
                        x0    = _mm512_mul_pd(x3,x4);
                        *ESr  = _mm512_mul_pd(cer,x0);
                        *ESi  = _mm512_mul_pd(cei,x0);
                                                
                }


                 
                   void ESph_f6215_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) palp,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht1,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht2,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64) ESi) {

                        register __m512d k0   = _mm512_load_pd(&pk0[0]);
                        register __m512d r    = _mm512_load_pd(&pr[0]);
                        register __m512d alp  = _mm512_load_pd(&palp[0]);
                        register __m512d tht1 = _mm512_load_pd(&ptht1[0]);
                        register __m512d tht2 = _mm512_load_pd(&ptht2[0]);
                        register __m512d phi1 = _mm512_load_pd(&pphi1[0]);
                        register __m512d phi2 = _mm512_load_pd(&pphi2[0]);
                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        const __m512d _2  = _mm512_set1_pd(2.0f);
                        const __m512d _16 = _mm512_set1_pd(16.0f);
                        register __m512d k0r,inv,alph,ear,eai,cer,cei;
                        register __m512d t0r,t0i,trm1,trm2,trm3,trm4;
                        register __m512d stht1,stht2,ctht1,ctht2;
                        register __m512d htht1,htht2,chtht1,chtht2;
                        register __m512d shtht1,shtht2,x0,x1,phid,cphid;
                        register __m512d ctht12,sect1,sect2,x2,x3,cthtp3;
                        k0r   = _mm512_mul_pd(k0,r);
                        stht1 = xsin(tht1);
                        alph  = _mm512_mul_pd(alp,hlf);
                        ctht1 = xcos(tht1);
                        phid  = _mm512_sub_pd(phi1,phi2);
                        stht2 = xsin(tht2);
                        htht1 = _mm512_mul_pd(tht1,hlf);
                        ctht2 = xcos(tht2);
                        ear   = _mm512_setzero_pd();
                        htht2 = _mm512_mul_pd(tht2);
                        eai   = k0r;
                        inv   = _mm512_rcp14_pd(k0r);
                        ctht12= _mm512_add_pd(ctht1,ctht2);
                        cexp_zmm8r8(ear,eai,&t0r,&t0i);
                        x0    = _mm512_fmadd_pd(stht2,stht1,stht1);
                        chtht1= xcos(htht1);
                        cer   = _mm512_mul_pd(_mm512_mul_pd(t0r,inv),alph);
                        cthtp3= _mm512_mul_pd(ctht12,_mm512_mul_pd(ctht12,ctht12));
                        cei   = _mm512_mul_pd(_mm512_mul_pd(t0i,inv),alph);
                        chtht2= xcos(htht2);
                        trm1  = _mm512_div_pd(x0,cthtp3);
                        x2    = _mm512_rcp14_pd(chtht1);
                        shtht1= xsin(htht1);
                        x3    = _mm512_rcp14_pd(chtht2);
                        shtht2= xsin(htht2);
                        x0    = _mm512_fmadd_pd(x2,x2,_mm512_mul_pd(x3,x3));
                        trm2  = _mm512_div_pd(x0,ctht12);
                        x1    = _mm512_fmadd_pd(stht1,stht1,_mm512_mul_pd(stht2,stht2));
                        x2    = _mm512_mul_pd(_2,_mm512_mul_pd(_mm512_mul_pd(chtht1,chtht1),
                                              _mm512_mul_pd(chtht2,chtht2)));
                        x3    = _mm512_mul_pd(ctht12,ctht12);
                        t0r   = _mm512_mul_pd(x2,x3);
                        trm3  = _mm512_div_pd(x1,t0r);
                        x0    = _mm512_mul_pd(_16,_mm512_mul_pd(shtht1,shtht1));
                        cphid = xcos(phid);
                        x1    = _mm512_mul_pd(shtht2,shtht2);
                        x2    = _mm512_mul_pd(x0,x1);
                        trm4  = _mm512_div_pd(x2,cthtp3);
                        x3    = _mm512_add_pd(trm1,cphid);
                        x4    = _mm512_add_pd(trm2,_mm512_add_pd(trm3,trm4));
                        x0    = _mm512_mul_pd(x3,x4);
                        _mm512_store_pd(&ESr[0] ,_mm512_mul_pd(cer,x0));
                        _mm512_store_pd(&ESi[0] ,_mm512_mul_pd(cei,x0));
                                                
                }


                 
                   void ESph_f6215_zmm8r8_u(const float * __restrict  pk0,
                                           const float * __restrict  pr,
                                           const float * __restrict  palp,
                                           const float * __restrict  ptht1,
                                           const float * __restrict  ptht2,
                                           const float * __restrict  pphi1,
                                           const float * __restrict  pphi2,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi) {

                        register __m512d k0   = _mm512_loadu_pd(&pk0[0]);
                        register __m512d r    = _mm512_loadu_pd(&pr[0]);
                        register __m512d alp  = _mm512_loadu_pd(&palp[0]);
                        register __m512d tht1 = _mm512_loadu_pd(&ptht1[0]);
                        register __m512d tht2 = _mm512_loadu_pd(&ptht2[0]);
                        register __m512d phi1 = _mm512_loadu_pd(&pphi1[0]);
                        register __m512d phi2 = _mm512_loadu_pd(&pphi2[0]);
                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        const __m512d _2  = _mm512_set1_pd(2.0f);
                        const __m512d _16 = _mm512_set1_pd(16.0f);
                        register __m512d k0r,inv,alph,ear,eai,cer,cei;
                        register __m512d t0r,t0i,trm1,trm2,trm3,trm4;
                        register __m512d stht1,stht2,ctht1,ctht2;
                        register __m512d htht1,htht2,chtht1,chtht2;
                        register __m512d shtht1,shtht2,x0,x1,phid,cphid;
                        register __m512d ctht12,sect1,sect2,x2,x3,cthtp3;
                        k0r   = _mm512_mul_pd(k0,r);
                        stht1 = xsin(tht1);
                        alph  = _mm512_mul_pd(alp,hlf);
                        ctht1 = xcos(tht1);
                        phid  = _mm512_sub_pd(phi1,phi2);
                        stht2 = xsin(tht2);
                        htht1 = _mm512_mul_pd(tht1,hlf);
                        ctht2 = xcos(tht2);
                        ear   = _mm512_setzero_pd();
                        htht2 = _mm512_mul_pd(tht2);
                        eai   = k0r;
                        inv   = _mm512_rcp14_pd(k0r);
                        ctht12= _mm512_add_pd(ctht1,ctht2);
                        cexp_zmm8r8(ear,eai,&t0r,&t0i);
                        x0    = _mm512_fmadd_pd(stht2,stht1,stht1);
                        chtht1= xcos(htht1);
                        cer   = _mm512_mul_pd(_mm512_mul_pd(t0r,inv),alph);
                        cthtp3= _mm512_mul_pd(ctht12,_mm512_mul_pd(ctht12,ctht12));
                        cei   = _mm512_mul_pd(_mm512_mul_pd(t0i,inv),alph);
                        chtht2= xcos(htht2);
                        trm1  = _mm512_div_pd(x0,cthtp3);
                        x2    = _mm512_rcp14_pd(chtht1);
                        shtht1= xsin(htht1);
                        x3    = _mm512_rcp14_pd(chtht2);
                        shtht2= xsin(htht2);
                        x0    = _mm512_fmadd_pd(x2,x2,_mm512_mul_pd(x3,x3));
                        trm2  = _mm512_div_pd(x0,ctht12);
                        x1    = _mm512_fmadd_pd(stht1,stht1,_mm512_mul_pd(stht2,stht2));
                        x2    = _mm512_mul_pd(_2,_mm512_mul_pd(_mm512_mul_pd(chtht1,chtht1),
                                              _mm512_mul_pd(chtht2,chtht2)));
                        x3    = _mm512_mul_pd(ctht12,ctht12);
                        t0r   = _mm512_mul_pd(x2,x3);
                        trm3  = _mm512_div_pd(x1,t0r);
                        x0    = _mm512_mul_pd(_16,_mm512_mul_pd(shtht1,shtht1));
                        cphid = xcos(phid);
                        x1    = _mm512_mul_pd(shtht2,shtht2);
                        x2    = _mm512_mul_pd(x0,x1);
                        trm4  = _mm512_div_pd(x2,cthtp3);
                        x3    = _mm512_add_pd(trm1,cphid);
                        x4    = _mm512_add_pd(trm2,_mm512_add_pd(trm3,trm4));
                        x0    = _mm512_mul_pd(x3,x4);
                        _mm512_storeu_pd(&ESr[0] ,_mm512_mul_pd(cer,x0));
                        _mm512_storeu_pd(&ESi[0] ,_mm512_mul_pd(cei,x0));
                                                
                }


                  /*
                           Bistatic RCS case -- Physical Optics approximated.
                           E-field scattered for (theta component).
                           Formula 6.2-17
                    */

#include "GMS_simd_utils.h"

                 
                   void ESth_f6217_zmm8r8(const __m512d k0,
                                           const __m512d r,
                                           const __m512d alp,
                                           const __m512d tht,
                                           const __m512d phi,
                                           __m512d * __restrict ESr,
                                           __m512d * __restrict ESi) {

                         const __m512d hlf = _mm512_set1_pd(0.5f);
                         const __m512d _4  = _mm512_set1_pd(4.0f);
                         const __m512d c0  = _mm512_set1_pd(1.5f);
                         register __m512d k0r,ear,eai,cer,cei,t0r,t0i,inv,x0,chtht;
                         register __m512d L,cphi,cosa,sina,htht,argm,argp,num,den;
                         k0r   = _mm512_mul_pd(k0,r);
                         cphi  = xcos(phi);
                         htht  = _mm512_mul_pd(tht,hlf);
                         cosa  = xcos(alp);
                         ear   = _mm512_setzero_pd();
                         inv   = _mm512_rcp14_pd(k0r);
                         eai   = k0r;
                         argm  = _mm512_add_pd(alp,htht);
                         cexp_zmm8r8(ear,eai,&t0r,&t0i);
                         argp  = _mm512_sub_pd(alp,htht);
                         sina  = xsin(alp);
                         x0    = _mm512_mul_pd(sina,sina);
                         chtht = xcos(htht);
                         cer   = _mm512_mul_pd(t0r,inv);
                         num   = _mm512_mul_pd(negate_zmm8r8(x0),cosa);
                         cei   = _mm512_mul_pd(t0i,inv);
                         ear   = xcos(argp);
                         eai   = xcos(argm);
                         x0    = _mm512_pow_pd(_mm512_add_pd(ear,eai),c0);
                         den   = _mm512_mul_pd(_mm512_mul_pd(_4,chtht),x0);
                         L     = _mm512_div_pd(num,den);
                         t0i   = _mm512_mul_pd(L,cphi);
                         *ESr  = _mm512_mul_pd(cer,t0i);
                         *ESi  = _mm512_mul_pd(cei,t0i);
                }


                
                   void ESth_f6217_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) palp,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64) ESi) {

                         register __m512d k0   = _mm512_load_pd(&pk0[0]);
                         register __m512d r    = _mm512_load_pd(&pr[0]);
                         register __m512d alp  = _mm512_load_pd(&palp[0]);
                         register __m512d tht  = _mm512_load_pd(&ptht[0]);
                         register __m512d phi = _mm512_load_pd(&pphi[0]);
                         const __m512d hlf = _mm512_set1_pd(0.5f);
                         const __m512d _4  = _mm512_set1_pd(4.0f);
                         const __m512d c0  = _mm512_set1_pd(1.5f);
                         register __m512d k0r,ear,eai,cer,cei,t0r,t0i,inv,x0,chtht;
                         register __m512d L,cphi,cosa,sina,htht,argm,argp,num,den;
                         k0r   = _mm512_mul_pd(k0,r);
                         cphi  = xcos(phi);
                         htht  = _mm512_mul_pd(tht,hlf);
                         cosa  = xcos(alp);
                         ear   = _mm512_setzero_pd();
                         inv   = _mm512_rcp14_pd(k0r);
                         eai   = k0r;
                         argm  = _mm512_add_pd(alp,htht);
                         cexp_zmm8r8(ear,eai,&t0r,&t0i);
                         argp  = _mm512_sub_pd(alp,htht);
                         sina  = xsin(alp);
                         x0    = _mm512_mul_pd(sina,sina);
                         chtht = xcos(htht);
                         cer   = _mm512_mul_pd(t0r,inv);
                         num   = _mm512_mul_pd(negate_zmm8r8(x0),cosa);
                         cei   = _mm512_mul_pd(t0i,inv);
                         ear   = xcos(argp);
                         eai   = xcos(argm);
                         x0    = _mm512_pow_pd(_mm512_add_pd(ear,eai),c0);
                         den   = _mm512_mul_pd(_mm512_mul_pd(_4,chtht),x0);
                         L     = _mm512_div_pd(num,den);
                         t0i   = _mm512_mul_pd(L,cphi);
                         _mm512_store_pd(&ESr[0] ,_mm512_mul_pd(cer,t0i));
                         _mm512_store_pd(&ESi[0] ,_mm512_mul_pd(cei,t0i));
                }


                  
                   void ESth_f6217_zmm8r8_u(const float * __restrict  pk0,
                                           const float * __restrict  pr,
                                           const float * __restrict  palp,
                                           const float * __restrict  ptht,
                                           const float * __restrict  pphi,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi) {

                         register __m512d k0   = _mm512_loadu_pd(&pk0[0]);
                         register __m512d r    = _mm512_loadu_pd(&pr[0]);
                         register __m512d alp  = _mm512_loadu_pd(&palp[0]);
                         register __m512d tht  = _mm512_loadu_pd(&ptht[0]);
                         register __m512d phi  = _mm512_loadu_pd(&pphi[0]);
                         const __m512d hlf = _mm512_set1_pd(0.5f);
                         const __m512d _4  = _mm512_set1_pd(4.0f);
                         const __m512d c0  = _mm512_set1_pd(1.5f);
                         register __m512d k0r,ear,eai,cer,cei,t0r,t0i,inv,x0,chtht;
                         register __m512d L,cphi,cosa,sina,htht,argm,argp,num,den;
                         k0r   = _mm512_mul_pd(k0,r);
                         cphi  = xcos(phi);
                         htht  = _mm512_mul_pd(tht,hlf);
                         cosa  = xcos(alp);
                         ear   = _mm512_setzero_pd();
                         inv   = _mm512_rcp14_pd(k0r);
                         eai   = k0r;
                         argm  = _mm512_add_pd(alp,htht);
                         cexp_zmm8r8(ear,eai,&t0r,&t0i);
                         argp  = _mm512_sub_pd(alp,htht);
                         sina  = xsin(alp);
                         x0    = _mm512_mul_pd(sina,sina);
                         chtht = xcos(htht);
                         cer   = _mm512_mul_pd(t0r,inv);
                         num   = _mm512_mul_pd(negate_zmm8r8(x0),cosa);
                         cei   = _mm512_mul_pd(t0i,inv);
                         ear   = xcos(argp);
                         eai   = xcos(argm);
                         x0    = _mm512_pow_pd(_mm512_add_pd(ear,eai),c0);
                         den   = _mm512_mul_pd(_mm512_mul_pd(_4,chtht),x0);
                         L     = _mm512_div_pd(num,den);
                         t0i   = _mm512_mul_pd(L,cphi);
                         _mm512_storeu_pd(&ESr[0] ,_mm512_mul_pd(cer,t0i));
                         _mm512_storeu_pd(&ESi[0] ,_mm512_mul_pd(cei,t0i));
                }


                    /*
                           Bistatic RCS case -- Physical Optics approximated.
                           E-field scattered for (phi component).
                           Formula 6.2-18
                      */


                 
                   void ESph_f6218_zmm8r8(const __m512d k0,
                                           const __m512d r,
                                           const __m512d alp,
                                           const __m512d tht,
                                           const __m512d phi,
                                           __m512d * __restrict ESr,
                                           __m512d * __restrict ESi) {
                        
                         const __m512d hlf = _mm512_set1_pd(0.5f);
                         const __m512d _4  = _mm512_set1_pd(4.0f);
                         const __m512d c0  = _mm512_set1_pd(1.5f);
                         const __m512d n1  = _mm512_set1_pd(-1.0f);
                         register __m512d k0r,ear,eai,cer,cei,t0r,t0i,inv,x0,chtht;
                         register __m512d L,sphi,cosa,sina,htht,argm,argp,num,den;
                         k0r   = _mm512_mul_pd(k0,r);
                         sphi  = xsin(phi);
                         htht  = _mm512_mul_pd(tht,hlf);
                         cosa  = xcos(alp);
                         ear   = _mm512_setzero_pd();
                         inv   = _mm512_rcp14_pd(k0r);
                         eai   = k0r;
                         argm  = _mm512_add_pd(alp,htht);
                         cexp_zmm8r8(ear,eai,&t0r,&t0i);
                         argp  = _mm512_sub_pd(alp,htht);
                         sina  = xsin(alp);
                         x0    = _mm512_mul_pd(sina,sina);
                         chtht = xcos(htht);
                         cer   = _mm512_mul_pd(t0r,inv);
                         num   = _mm512_mul_pd(negate_zmm8r8(x0),cosa);
                         cei   = _mm512_mul_pd(t0i,inv);
                         ear   = xcos(argp);
                         eai   = xcos(argm);
                         x0    = _mm512_pow_pd(_mm512_add_pd(ear,eai),c0);
                         den   = _mm512_mul_pd(_mm512_mul_pd(_4,chtht),x0);
                         L     = _mm512_div_pd(num,den);
                         t0i   = _mm512_mul_pd(L,cphi);
                         *ESr  = _mm512_mul_pd(n1,_mm512_mul_pd(cer,t0i)));
                         *ESi  = _mm512_mul_pd(n1,_mm512_mul_pd(cei,t0i)));
                }


                 
                   void ESph_f6218_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pr,
                                           const float * __restrict __ATTR_ALIGN__(64) palp,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           const float * __restrict __ATTR_ALIGN__(64) pphi,
                                           float * __restrict __ATTR_ALIGN__(64) ESr,
                                           float * __restrict __ATTR_ALIGN__(64) ESi) {

                         register __m512d k0   = _mm512_load_pd(&pk0[0]);
                         register __m512d r    = _mm512_load_pd(&pr[0]);
                         register __m512d alp  = _mm512_load_pd(&palp[0]);
                         register __m512d tht  = _mm512_load_pd(&ptht[0]);
                         register __m512d phi = _mm512_load_pd(&pphi[0]);
                         const __m512d hlf = _mm512_set1_pd(0.5f);
                         const __m512d _4  = _mm512_set1_pd(4.0f);
                         const __m512d c0  = _mm512_set1_pd(1.5f);
                         const __m512d n1  = _mm512_set1_pd(-1.0f);
                         register __m512d k0r,ear,eai,cer,cei,t0r,t0i,inv,x0,chtht;
                         register __m512d L,sphi,cosa,sina,htht,argm,argp,num,den;
                         k0r   = _mm512_mul_pd(k0,r);
                         sphi  = xsin(phi);
                         htht  = _mm512_mul_pd(tht,hlf);
                         cosa  = xcos(alp);
                         ear   = _mm512_setzero_pd();
                         inv   = _mm512_rcp14_pd(k0r);
                         eai   = k0r;
                         argm  = _mm512_add_pd(alp,htht);
                         cexp_zmm8r8(ear,eai,&t0r,&t0i);
                         argp  = _mm512_sub_pd(alp,htht);
                         sina  = xsin(alp);
                         x0    = _mm512_mul_pd(sina,sina);
                         chtht = xcos(htht);
                         cer   = _mm512_mul_pd(t0r,inv);
                         num   = _mm512_mul_pd(negate_zmm8r8(x0),cosa);
                         cei   = _mm512_mul_pd(t0i,inv);
                         ear   = xcos(argp);
                         eai   = xcos(argm);
                         x0    = _mm512_pow_pd(_mm512_add_pd(ear,eai),c0);
                         den   = _mm512_mul_pd(_mm512_mul_pd(_4,chtht),x0);
                         L     = _mm512_div_pd(num,den);
                         t0i   = _mm512_mul_pd(L,cphi);
                         _mm512_store_pd(&ESr[0]  ,_mm512_mul_pd(n1,_mm512_mul_pd(cer,t0i)));
                         _mm512_store_pd(&ESi[0]  ,_mm512_mul_pd(n1,_mm512_mul_pd(cei,t0i)));
                }


                  
                   void ESph_f6218_zmm8r8_u(const float * __restrict  pk0,
                                           const float * __restrict  pr,
                                           const float * __restrict  palp,
                                           const float * __restrict  ptht,
                                           const float * __restrict  pphi,
                                           float * __restrict  ESr,
                                           float * __restrict  ESi) {

                         register __m512d k0   = _mm512_loadu_pd(&pk0[0]);
                         register __m512d r    = _mm512_loadu_pd(&pr[0]);
                         register __m512d alp  = _mm512_loadu_pd(&palp[0]);
                         register __m512d tht  = _mm512_loadu_pd(&ptht[0]);
                         register __m512d phi  = _mm512_loadu_pd(&pphi[0]);
                         const __m512d hlf = _mm512_set1_pd(0.5f);
                         const __m512d _4  = _mm512_set1_pd(4.0f);
                         const __m512d c0  = _mm512_set1_pd(1.5f);
                         const __m512d n1  = _mm512_set1_pd(-1.0f);
                         register __m512d k0r,ear,eai,cer,cei,t0r,t0i,inv,x0,chtht;
                         register __m512d L,sphi,cosa,sina,htht,argm,argp,num,den;
                         k0r   = _mm512_mul_pd(k0,r);
                         sphi  = xsin(phi);
                         htht  = _mm512_mul_pd(tht,hlf);
                         cosa  = xcos(alp);
                         ear   = _mm512_setzero_pd();
                         inv   = _mm512_rcp14_pd(k0r);
                         eai   = k0r;
                         argm  = _mm512_add_pd(alp,htht);
                         cexp_zmm8r8(ear,eai,&t0r,&t0i);
                         argp  = _mm512_sub_pd(alp,htht);
                         sina  = xsin(alp);
                         x0    = _mm512_mul_pd(sina,sina);
                         chtht = xcos(htht);
                         cer   = _mm512_mul_pd(t0r,inv);
                         num   = _mm512_mul_pd(negate_zmm8r8(x0),cosa);
                         cei   = _mm512_mul_pd(t0i,inv);
                         ear   = xcos(argp);
                         eai   = xcos(argm);
                         x0    = _mm512_pow_pd(_mm512_add_pd(ear,eai),c0);
                         den   = _mm512_mul_pd(_mm512_mul_pd(_4,chtht),x0);
                         L     = _mm512_div_pd(num,den);
                         t0i   = _mm512_mul_pd(L,cphi);
                         _mm512_storeu_pd(&ESr[0]  ,_mm512_mul_pd(n1,_mm512_mul_pd(cer,t0i)));
                         _mm512_storeu_pd(&ESi[0]  ,_mm512_mul_pd(n1,_mm512_mul_pd(cei,t0i)));
                }



                    /*
                           Physical-Optics axial-incidence bistatic RCS
                           function of theta for (0 << theta < PI-2*alpha)
                           Formula 6.2-20
                      */


                 
                   __m512d rcs_f6220_zmm8r8(const __m512d gam0,
                                            const __m512d alp,
                                            const __m512d tht) {

                           const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                           const __m512d _1    = _mm512_set1_pd(1.0f);
                           register __m512d rcs,gam2,tana,tan4,x0,x1,trm1;
                           register __m512d alp2,calp2,ctht,trm2,num,den,x2;
                           gam2  = _mm512_mul_pd(gam0,gam0);
                           tana  = xtan(alp);
                           alp2  = _mm512_add_pd(alp,alp);
                           calp2 = xcos(alp2);
                           x0    = _mm512_mul_pd(tana,tana);
                           x1    = _mm512_add_pd(_1,calp2);
                           x2    = _mm512_mul_pd(x0,x0);
                           ctht  = xcos(tht);
                           trm1  = _mm512_div_pd(_mm512_mul_pd(gam2,x2),_16pi);
                           x0    = _mm512_add_pd(_1,ctht);
                           x2    = _mm512_mul_pd(x1,_mm512_mul_pd(x1,x1));
                           num   = _mm512_add_pd(x2,x2);
                           x1    = _mm512_add_pd(ctht,calp2);
                           x2    = _mm512_mul_pd(x1,_mm512_mul_pd(x1,x1));
                           den   = _mm512_mul_pd(x0,x2);
                           trm2  = _mm512_div_pd(num,den);
                           rcs   = _mm512_mul_pd(trm1,trm2);
                           return (rcs);
                  }


                
                   __m512d rcs_f6220_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                            const float * __restrict __ATTR_ALIGN__(64) palp,
                                            const float * __restrict __ATTR_ALIGN__(64) ptht) {

                           register __m512d  gam0  = _mm512_load_pd(&pgam0[0]);
                           register __m512d  alp   = _mm512_load_pd(&palp[0]);
                           register __m512d  tht   = _mm512_load_pd(&ptht[0]);
                           const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                           const __m512d _1    = _mm512_set1_pd(1.0f);
                           register __m512d rcs,gam2,tana,tan4,x0,x1,trm1;
                           register __m512d alp2,calp2,ctht,trm2,num,den,x2;
                           gam2  = _mm512_mul_pd(gam0,gam0);
                           tana  = xtan(alp);
                           alp2  = _mm512_add_pd(alp,alp);
                           calp2 = xcos(alp2);
                           x0    = _mm512_mul_pd(tana,tana);
                           x1    = _mm512_add_pd(_1,calp2);
                           x2    = _mm512_mul_pd(x0,x0);
                           ctht  = xcos(tht);
                           trm1  = _mm512_div_pd(_mm512_mul_pd(gam2,x2),_16pi);
                           x0    = _mm512_add_pd(_1,ctht);
                           x2    = _mm512_mul_pd(x1,_mm512_mul_pd(x1,x1));
                           num   = _mm512_add_pd(x2,x2);
                           x1    = _mm512_add_pd(ctht,calp2);
                           x2    = _mm512_mul_pd(x1,_mm512_mul_pd(x1,x1));
                           den   = _mm512_mul_pd(x0,x2);
                           trm2  = _mm512_div_pd(num,den);
                           rcs   = _mm512_mul_pd(trm1,trm2);
                           return (rcs);
                  }


                  
                   __m512d rcs_f6220_zmm8r8_u(const float * __restrict  pgam0,
                                            const float * __restrict  palp,
                                            const float * __restrict  ptht) {

                           register __m512d  gam0  = _mm512_loadu_pd(&pgam0[0]);
                           register __m512d  alp   = _mm512_loadu_pd(&palp[0]);
                           register __m512d  tht   = _mm512_loadu_pd(&ptht[0]);
                           const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                           const __m512d _1    = _mm512_set1_pd(1.0f);
                           register __m512d rcs,gam2,tana,tan4,x0,x1,trm1;
                           register __m512d alp2,calp2,ctht,trm2,num,den,x2;
                           gam2  = _mm512_mul_pd(gam0,gam0);
                           tana  = xtan(alp);
                           alp2  = _mm512_add_pd(alp,alp);
                           calp2 = xcos(alp2);
                           x0    = _mm512_mul_pd(tana,tana);
                           x1    = _mm512_add_pd(_1,calp2);
                           x2    = _mm512_mul_pd(x0,x0);
                           ctht  = xcos(tht);
                           trm1  = _mm512_div_pd(_mm512_mul_pd(gam2,x2),_16pi);
                           x0    = _mm512_add_pd(_1,ctht);
                           x2    = _mm512_mul_pd(x1,_mm512_mul_pd(x1,x1));
                           num   = _mm512_add_pd(x2,x2);
                           x1    = _mm512_add_pd(ctht,calp2);
                           x2    = _mm512_mul_pd(x1,_mm512_mul_pd(x1,x1));
                           den   = _mm512_mul_pd(x0,x2);
                           trm2  = _mm512_div_pd(num,den);
                           rcs   = _mm512_mul_pd(trm1,trm2);
                           return (rcs);
                  }


                     /*
                           Axial incidence backscattering (theta = 0)
                           RCS (Spencer equation).
                           Formula 6.2-22
                       */

               
                   __m512d rcs_f6222_zmm8r8(const __m512d gam0,
                                        const __m512d alp) {

                          const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                          register __m512d rcs,gam2,trm1,tana,x0,x1;
                          gam2 = _mm512_mul_pd(gam0,gam00;
                          tana = xtan(alp);
                          trm1 = _mm512_div_pd(gam2,_16pi);
                          x0   = _mm512_mul_pd(tana,tana);
                          x1   = _mm512_mul_pd(x0,x0);
                          rcs  = _mm512_mul_pd(trm1,x1);
                          return (rcs);
                 }


                 
                   __m512d rcs_f6222_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                            const float * __restrict __ATTR_ALIGN__(64) palp) {

                          register __m512d  gam0  = _mm512_load_pd(&pgam0[0]);
                          register __m512d  alp   = _mm512_load_pd(&palp[0]);
                          const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                          register __m512d rcs,gam2,trm1,tana,x0,x1;
                          gam2 = _mm512_mul_pd(gam0,gam00;
                          tana = xtan(alp);
                          trm1 = _mm512_div_pd(gam2,_16pi);
                          x0   = _mm512_mul_pd(tana,tana);
                          x1   = _mm512_mul_pd(x0,x0);
                          rcs  = _mm512_mul_pd(trm1,x1);
                          return (rcs);
                 }


                  
                   __m512d rcs_f6222_zmm8r8_u(const float * __restrict  pgam0,
                                            const float * __restrict  palp) {

                          register __m512d  gam0  = _mm512_loadu_pd(&pgam0[0]);
                          register __m512d  alp   = _mm512_loadu_pd(&palp[0]);
                          const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                          register __m512d rcs,gam2,trm1,tana,x0,x1;
                          gam2 = _mm512_mul_pd(gam0,gam00;
                          tana = xtan(alp);
                          trm1 = _mm512_div_pd(gam2,_16pi);
                          x0   = _mm512_mul_pd(tana,tana);
                          x1   = _mm512_mul_pd(x0,x0);
                          rcs  = _mm512_mul_pd(trm1,x1);
                          return (rcs);
                 }


                    /*
                           Narrow-angle cone.
                           Scattered E-field.
                           Formula 6.2-24
                       */


                 
                   void ES_f6224_zmm8r8(const __m512d k0,
                                         const __m512d z,
                                         const __m512d alp,
                                         const __m512d x,
                                         const __m512d y,
                                         const __m512d z,
                                         __m512d * __restrict xre,
                                         __m512d * __restrict xim,
                                         __m512d * __restrict yre,
                                         __m512d * __restrict yim,
                                         __m512d * __restrict zre,
                                         __m512d * __restrict zim) {

                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        const __m512d cr  = _mm512_set1_pd(0.0f);
                        const __m512d ci  = _mm512_set1_pd(1.0f);
                        register __m512d alph,x0,k0z,inv,ear,eai,cer,cei;
                        register __m512d t0r,t0i;
                        
                        alph = _mm512_mul_pd(alp,hlf);
                        k0z  = _mm512_mul_pd(k0,z);
                        ear  = _mm512_setzero_pd();
                        inv  = _mm512_rcp14_pd(k0z);
                        eai  = k0z;
                        x0   = _mm512_mul_pd(alph,alph);
                        cexp_zmm8r8(ear,eai,&t0r,&t0i);
                        cmul_zmm8r8(t0r,t0i,cr,ci,&cer,&cei);
                        cer  = _mm512_mul_pd(x0,_mm512_mul_pd(cer,inv));
                        cei  = _mm512_mul_pd(x0,_mm512_mul_pd(cei,inv));
                        *xre  = _mm512_mul_pd(x,cer);
                        *xim  = _mm512_mul_pd(x,cei);
                        *yre  = _mm512_mul_pd(y,cer);
                        *yim  = _mm512_mul_pd(y,cei);
                        *zre  = _mm512_mul_pd(z,cer);
                        *zim  = _mm512_mul_pd(z,cei);
                 }


                 
                   void ES_f6224_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
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
                                           float * __restrict __ATTR_ALIGN__(64) zim) {

                        register __m512d k0  = _mm512_load_pd(&pk0[0]);
                        register __m512d z   = _mm512_load_pd(&pz[0]);
                        register __m512d alp = _mm512_load_pd(&palp[0]);
                        register __m512d x   = _mm512_load_pd(&px[0]);
                        register __m512d y   = _mm512_load_pd(&py[0]);
                        register __m512d z   = _mm512_load_pd(&pz[0]);
                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        const __m512d cr  = _mm512_set1_pd(0.0f);
                        const __m512d ci  = _mm512_set1_pd(1.0f);
                        register __m512d alph,x0,k0z,inv,ear,eai,cer,cei;
                        register __m512d t0r,t0i;
                        
                        alph = _mm512_mul_pd(alp,hlf);
                        k0z  = _mm512_mul_pd(k0,z);
                        ear  = _mm512_setzero_pd();
                        inv  = _mm512_rcp14_pd(k0z);
                        eai  = k0z;
                        x0   = _mm512_mul_pd(alph,alph);
                        cexp_zmm8r8(ear,eai,&t0r,&t0i);
                        cmul_zmm8r8(t0r,t0i,cr,ci,&cer,&cei);
                        cer  = _mm512_mul_pd(x0,_mm512_mul_pd(cer,inv));
                        cei  = _mm512_mul_pd(x0,_mm512_mul_pd(cei,inv));
                        _mm512_store_pd(&xre[0] ,_mm512_mul_pd(x,cer));
                        _mm512_store_pd(&xim[0] ,_mm512_mul_pd(x,cei));
                        _mm512_store_pd(&yre[0] ,_mm512_mul_pd(y,cer));
                        _mm512_store_pd(&yim[0] ,_mm512_mul_pd(y,cei));
                        _mm512_store_pd(&zre[0] ,_mm512_mul_pd(z,cer));
                        _mm512_store_pd(&zim[0] ,_mm512_mul_pd(z,cei));
                 }


                 
                   void ES_f6224_zmm8r8_u(const float * __restrict  pk0,
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
                                           float * __restrict  zim) {

                        register __m512d k0  = _mm512_loadu_pd(&pk0[0]);
                        register __m512d z   = _mm512_loadu_pd(&pz[0]);
                        register __m512d alp = _mm512_loadu_pd(&palp[0]);
                        register __m512d x   = _mm512_loadu_pd(&px[0]);
                        register __m512d y   = _mm512_loadu_pd(&py[0]);
                        register __m512d z   = _mm512_loadu_pd(&pz[0]);
                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        const __m512d cr  = _mm512_set1_pd(0.0f);
                        const __m512d ci  = _mm512_set1_pd(1.0f);
                        register __m512d alph,x0,k0z,inv,ear,eai,cer,cei;
                        register __m512d t0r,t0i;
                        
                        alph = _mm512_mul_pd(alp,hlf);
                        k0z  = _mm512_mul_pd(k0,z);
                        ear  = _mm512_setzero_pd();
                        inv  = _mm512_rcp14_pd(k0z);
                        eai  = k0z;
                        x0   = _mm512_mul_pd(alph,alph);
                        cexp_zmm8r8(ear,eai,&t0r,&t0i);
                        cmul_zmm8r8(t0r,t0i,cr,ci,&cer,&cei);
                        cer  = _mm512_mul_pd(x0,_mm512_mul_pd(cer,inv));
                        cei  = _mm512_mul_pd(x0,_mm512_mul_pd(cei,inv));
                        _mm512_storeu_pd(&xre[0] ,_mm512_mul_pd(x,cer));
                        _mm512_storeu_pd(&xim[0] ,_mm512_mul_pd(x,cei));
                        _mm512_storeu_pd(&yre[0] ,_mm512_mul_pd(y,cer));
                        _mm512_storeu_pd(&yim[0] ,_mm512_mul_pd(y,cei));
                        _mm512_storeu_pd(&zre[0] ,_mm512_mul_pd(z,cer));
                        _mm512_storeu_pd(&zim[0] ,_mm512_mul_pd(z,cei));
                 }


                     /*
                           Wide-angle cone.
                           Scattered E-field.
                           Formula 6.2-25
                       */


               
                   void ES_f6225_zmm8r8(const __m512d k0,
                                         const __m512d z,
                                         const __m512d alp,
                                         const __m512d x,
                                         const __m512d y,
                                         const __m512d z,
                                         __m512d * __restrict xre,
                                         __m512d * __restrict xim,
                                         __m512d * __restrict yre,
                                         __m512d * __restrict yim,
                                         __m512d * __restrict zre,
                                         __m512d * __restrict zim) {

                        const __m512d pi2 = _mm512_set1_pd(1.57079632679489661923132169164);
                        const __m512d _4  = _mm512_set1_pd(4.0f);
                        const __m512d cr  = _mm512_set1_pd(0.0f);
                        const __m512d ci  = _mm512_set1_pd(1.0f);
                        register __m512d pima,x0,pimas,k0z,inv,ear,eai,cer,cei;
                        register __m512d t0r,t0i;
                        
                        pima = _mm512_sub_pd(pi2,alp);
                        k0z  = _mm512_mul_pd(k0,z);
                        pimas= _mm512_mul_pd(pima,pima);
                        ear  = _mm512_setzero_pd();
                        x0   = _mm512_mul_pd(_mm512_mul_pd(_4,k0z),pimas);
                        inv  = _mm512_rcp14_pd(x0);
                        eai  = k0z;
                        cexp_zmm8r8(ear,eai,&t0r,&t0i);
                        cmul_zmm8r8(t0r,t0i,cr,ci,&cer,&cei);
                        cer  = _mm512_mul_pd(alph,_mm512_mul_pd(cer,inv));
                        cei  = _mm512_mul_pd(alph,_mm512_mul_pd(cei,inv));
                        *xre  = _mm512_mul_pd(x,cer);
                        *xim  = _mm512_mul_pd(x,cei);
                        *yre  = _mm512_mul_pd(y,cer);
                        *yim  = _mm512_mul_pd(y,cei);
                        *zre  = _mm512_mul_pd(z,cer);
                        *zim  = _mm512_mul_pd(z,cei);
                 }


                 
                   void ES_f6225_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
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
                                           float * __restrict __ATTR_ALIGN__(64) zim) {

                        register __m512d k0  = _mm512_load_pd(&pk0[0]);
                        register __m512d z   = _mm512_load_pd(&pz[0]);
                        register __m512d alp = _mm512_load_pd(&palp[0]);
                        register __m512d x   = _mm512_load_pd(&px[0]);
                        register __m512d y   = _mm512_load_pd(&py[0]);
                        register __m512d z   = _mm512_load_pd(&pz[0]);
                        const __m512d pi2 = _mm512_set1_pd(1.57079632679489661923132169164);
                        const __m512d _4  = _mm512_set1_pd(4.0f);
                        const __m512d cr  = _mm512_set1_pd(0.0f);
                        const __m512d ci  = _mm512_set1_pd(1.0f);
                        register __m512d pima,x0,pimas,k0z,inv,ear,eai,cer,cei;
                        register __m512d t0r,t0i;
                        
                        pima = _mm512_sub_pd(pi2,alp);
                        k0z  = _mm512_mul_pd(k0,z);
                        pimas= _mm512_mul_pd(pima,pima);
                        ear  = _mm512_setzero_pd();
                        x0   = _mm512_mul_pd(_mm512_mul_pd(_4,k0z),pimas);
                        inv  = _mm512_rcp14_pd(x0);
                        eai  = k0z;
                        cexp_zmm8r8(ear,eai,&t0r,&t0i);
                        cmul_zmm8r8(t0r,t0i,cr,ci,&cer,&cei);
                        cer  = _mm512_mul_pd(alph,_mm512_mul_pd(cer,inv));
                        cei  = _mm512_mul_pd(alph,_mm512_mul_pd(cei,inv));
                        _mm512_store_pd(&xre[0] ,_mm512_mul_pd(x,cer));
                        _mm512_store_pd(&xim[0] ,_mm512_mul_pd(x,cei));
                        _mm512_store_pd(&yre[0] ,_mm512_mul_pd(y,cer));
                        _mm512_store_pd(&yim[0] ,_mm512_mul_pd(y,cei));
                        _mm512_store_pd(&zre[0] ,_mm512_mul_pd(z,cer));
                        _mm512_store_pd(&zim[0] ,_mm512_mul_pd(z,cei));
                 }


                 
                   void ES_f6225_zmm8r8_u(const float * __restrict  pk0,
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
                                           float * __restrict  zim) {

                        register __m512d k0  = _mm512_loadu_pd(&pk0[0]);
                        register __m512d z   = _mm512_loadu_pd(&pz[0]);
                        register __m512d alp = _mm512_loadu_pd(&palp[0]);
                        register __m512d x   = _mm512_loadu_pd(&px[0]);
                        register __m512d y   = _mm512_loadu_pd(&py[0]);
                        register __m512d z   = _mm512_loadu_pd(&pz[0]);
                        const __m512d pi2 = _mm512_set1_pd(1.57079632679489661923132169164);
                        const __m512d _4  = _mm512_set1_pd(4.0f);
                        const __m512d cr  = _mm512_set1_pd(0.0f);
                        const __m512d ci  = _mm512_set1_pd(1.0f);
                        register __m512d pima,x0,pimas,k0z,inv,ear,eai,cer,cei;
                        register __m512d t0r,t0i;
                        
                        pima = _mm512_sub_pd(pi2,alp);
                        k0z  = _mm512_mul_pd(k0,z);
                        pimas= _mm512_mul_pd(pima,pima);
                        ear  = _mm512_setzero_pd();
                        x0   = _mm512_mul_pd(_mm512_mul_pd(_4,k0z),pimas);
                        inv  = _mm512_rcp14_pd(x0);
                        eai  = k0z;
                        cexp_zmm8r8(ear,eai,&t0r,&t0i);
                        cmul_zmm8r8(t0r,t0i,cr,ci,&cer,&cei);
                        cer  = _mm512_mul_pd(alph,_mm512_mul_pd(cer,inv));
                        cei  = _mm512_mul_pd(alph,_mm512_mul_pd(cei,inv));
                        _mm512_storeu_pd(&xre[0] ,_mm512_mul_pd(x,cer));
                        _mm512_storeu_pd(&xim[0] ,_mm512_mul_pd(x,cei));
                        _mm512_storeu_pd(&yre[0] ,_mm512_mul_pd(y,cer));
                        _mm512_storeu_pd(&yim[0] ,_mm512_mul_pd(y,cei));
                        _mm512_storeu_pd(&zre[0] ,_mm512_mul_pd(z,cer));
                        _mm512_storeu_pd(&zim[0] ,_mm512_mul_pd(z,cei));
                 }


                   /*
                           Wide-angle cone.
                           Radar Cross Section (angle = 0)
                           Formula 6.2-26
                     */

                 
                   __m512d rcs_f6226_zmm8r8(const __m512d gam2,
                                            const __m512d alp) {

                         const __m512d pi2   = _mm512_set1_pd(1.57079632679489661923132169164);
                         const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                         register __m512d rcs,gam2,trm1,inv,pim,x0,pim4;
                         gam2 = _mm512_mul_pd(gam0,gam0);
                         pim  = _mm512_sub_pd(pi2,alp);
                         trm1 = _mm512_div_pd(gam2,_16pi);
                         x0   = _mm512_mul_pd(pim,pim);
                         pim4 = _mm512_mul_pd(x0,x0);
                         inv  = _mm512_rcp14_pd(pim4);
                         rcs  = _mm512_mul_pd(trm1,inv);
                         return (rcs);
                 } 


                   __m512d rcs_f6226_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pgam2,
                                            const float * __restrict __ATTR_ALIGN__(64) palp) {

                         register __m512d gam2   = _mm512_load_pd(&pgam2[0]);
                         register __m512d alp = _mm512_load_pd(&palp[0]);
                         const __m512d pi2   = _mm512_set1_pd(1.57079632679489661923132169164);
                         const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                         register __m512d rcs,gam2,trm1,inv,pim,x0,pim4;
                         gam2 = _mm512_mul_pd(gam0,gam0);
                         pim  = _mm512_sub_pd(pi2,alp);
                         trm1 = _mm512_div_pd(gam2,_16pi);
                         x0   = _mm512_mul_pd(pim,pim);
                         pim4 = _mm512_mul_pd(x0,x0);
                         inv  = _mm512_rcp14_pd(pim4);
                         rcs  = _mm512_mul_pd(trm1,inv);
                         return (rcs);
                 } 


                
                   __m512d rcs_f6226_zmm8r8_u(const float * __restrict  pgam2,
                                            const float * __restrict  palp) {

                         register __m512d gam2   = _mm512_loadu_pd(&pgam2[0]);
                         register __m512d alp = _mm512_loadu_pd(&palp[0]);
                         const __m512d pi2   = _mm512_set1_pd(1.57079632679489661923132169164);
                         const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                         register __m512d rcs,gam2,trm1,inv,pim,x0,pim4;
                         gam2 = _mm512_mul_pd(gam0,gam0);
                         pim  = _mm512_sub_pd(pi2,alp);
                         trm1 = _mm512_div_pd(gam2,_16pi);
                         x0   = _mm512_mul_pd(pim,pim);
                         pim4 = _mm512_mul_pd(x0,x0);
                         inv  = _mm512_rcp14_pd(pim4);
                         rcs  = _mm512_mul_pd(trm1,inv);
                         return (rcs);
                 } 


                    /*
                          The concave-tip axial-incidence, Physical-Optics RCS.
                          Formula 6.2-29

                      */


                 
                   __m512d rcs_f6229_zmm8r8(const __m512d k0,
                                            const __m512d alp,
                                            const __m512d R) {

                           const __m512d _4pi = _mm512_set1_pd(12.566370614359172953850573533118);
                           const __m512d _2   = _mm512_set1_pd(2.0f);
                           const __m512d _1   = _mm512_set1_pd(1.0f);
                           register __m512d rcs,trm1,ear1,eai1,ear2,eai2;
                           register __m512d cer1,cei1,cer2,cei2,inv1,inv2;
                           register __m512d t0r,t0i,x0,x1,k0R,R2,calp,x2,x3,t1r,t1i,cabs;
                           R2   = _m512_mul_pd(R,R);
                           x1   = _mm512_mul_pd(k0,k0);
                           calp = xcos(alp);
                           k0R  = _mm512_mul_pd(k0,R);
                           x0   = _mm512_mul_pd(R2,R2);
                           ear1 = _mm512_setzero_pd();
                           trm1 = _mm512_mul_pd(_mm512_mul_pd(_4pi,x1),x0);
                           eai1 = _mm512_add_pd(k0R,k0R);
                           x0   = _mm512_mul_pd(eai1,eai1);
                           x1   = _mm512_sub_pd(eai1,_1);
                           inv1 = _mm512_rcp14_pd(x0);
                           cexp_zmm8r8(ear1,eai1,t0r,t0i);
                           x2   = _mm512_sub_pd(calp,_1);
                           cmul_zmm8r8(ear1,x1,t0r,t0i,&cer1,&cei1);
                           cer1 = _mm512_mul_pd(cer1,inv1);
                           cei1 = _mm512_mul_pd(cei1,inv1);
                           ear2 = ear1;
                           eai2 = _mm512_mul_pd(eai1,calp);
                           inv2 = _mm512_rcp14_pd(_mm512_mul_pd(eai2,eai2));
                           x3   = _mm512_mul_pd(eai1,x2); 
                           cexp_zmm8r8(ear2,eai2,t0r,t0i);
                           cmul_zmm8r8(ear2,x3,t0r,t0i,&cer2,&cei2);
                           cer2 = _mm512_mul_pd(cer2,inv2);
                           cei2 = _mm512_mul_pd(cei2,inv2);
                           t1r  = _mm512_sub_pd(cer1,cer2);
                           t1i  = _mm512_sub_pd(cei1,cei2);
                           cabs = cabs_zmm8r8(t1r,t1i);
                           rcs  = _mm512_mul_pd(trm1,cabs);
                           return (rcs);
                 }


                  
                   __m512d rcs_f6229_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) palp,
                                              const float * __restrict __ATTR_ALIGN__(64) pR) {

                           register __m512d k0     = _mm512_load_pd(&pk0[0]);
                           register __m512d alp    = _mm512_load_pd(&palp[0]);
                           register __m512d R      = _mm512_load_pd(&pR[0]);
                           const __m512d _4pi = _mm512_set1_pd(12.566370614359172953850573533118);
                           const __m512d _2   = _mm512_set1_pd(2.0f);
                           const __m512d _1   = _mm512_set1_pd(1.0f);
                           register __m512d rcs,trm1,ear1,eai1,ear2,eai2;
                           register __m512d cer1,cei1,cer2,cei2,inv1,inv2;
                           register __m512d t0r,t0i,x0,x1,k0R,k0R2,R2,calp,x2,x3,t1r,t1i,cabs;
                           R2   = _m512_mul_pd(R,R);
                           x1   = _mm512_mul_pd(k0,k0);
                           calp = xcos(alp);
                           k0R  = _mm512_mul_pd(k0,R);
                           x0   = _mm512_mul_pd(R2,R2);
                           ear1 = _mm512_setzero_pd();
                           trm1 = _mm512_mul_pd(_mm512_mul_pd(_4pi,x1),x0);
                           eai1 = _mm512_add_pd(k0R,k0R);
                           x0   = _mm512_mul_pd(eai1,eai1);
                           x1   = _mm512_sub_pd(eai1,_1);
                           inv1 = _mm512_rcp14_pd(x0);
                           cexp_zmm8r8(ear1,eai1,t0r,t0i);
                           x2   = _mm512_sub_pd(calp,_1);
                           cmul_zmm8r8(ear1,x1,t0r,t0i,&cer1,&cei1);
                           cer1 = _mm512_mul_pd(cer1,inv1);
                           cei1 = _mm512_mul_pd(cei1,inv1);
                           ear2 = ear1;
                           eai2 = _mm512_mul_pd(eai1,calp);
                           inv2 = _mm512_rcp14_pd(_mm512_mul_pd(eai2,eai2));
                           x3   = _mm512_mul_pd(eai1,x2); 
                           cexp_zmm8r8(ear2,eai2,t0r,t0i);
                           cmul_zmm8r8(ear2,x3,t0r,t0i,&cer2,&cei2);
                           cer2 = _mm512_mul_pd(cer2,inv2);
                           cei2 = _mm512_mul_pd(cei2,inv2);
                           t1r  = _mm512_sub_pd(cer1,cer2);
                           t1i  = _mm512_sub_pd(cei1,cei2);
                           cabs = cabs_zmm8r8(t1r,t1i);
                           rcs  = _mm512_mul_pd(trm1,cabs);
                           return (rcs);
                 }


                
                   __m512d rcs_f6229_zmm8r8_u(const float * __restrict  pk0,
                                              const float * __restrict  palp,
                                              const float * __restrict  pR) {

                           register __m512d k0     = _mm512_loadu_pd(&pk0[0]);
                           register __m512d alp    = _mm512_loadu_pd(&palp[0]);
                           register __m512d R      = _mm512_loadu_pd(&pR[0]);
                           const __m512d _4pi = _mm512_set1_pd(12.566370614359172953850573533118);
                           const __m512d _2   = _mm512_set1_pd(2.0f);
                           const __m512d _1   = _mm512_set1_pd(1.0f);
                           register __m512d rcs,trm1,ear1,eai1,ear2,eai2;
                           register __m512d cer1,cei1,cer2,cei2,inv1,inv2;
                           register __m512d t0r,t0i,x0,x1,k0R,k0R2,R2,calp,x2,x3,t1r,t1i,cabs;
                           R2   = _m512_mul_pd(R,R);
                           x1   = _mm512_mul_pd(k0,k0);
                           calp = xcos(alp);
                           k0R  = _mm512_mul_pd(k0,R);
                           x0   = _mm512_mul_pd(R2,R2);
                           ear1 = _mm512_setzero_pd();
                           trm1 = _mm512_mul_pd(_mm512_mul_pd(_4pi,x1),x0);
                           eai1 = _mm512_add_pd(k0R,k0R);
                           x0   = _mm512_mul_pd(eai1,eai1);
                           x1   = _mm512_sub_pd(eai1,_1);
                           inv1 = _mm512_rcp14_pd(x0);
                           cexp_zmm8r8(ear1,eai1,t0r,t0i);
                           x2   = _mm512_sub_pd(calp,_1);
                           cmul_zmm8r8(ear1,x1,t0r,t0i,&cer1,&cei1);
                           cer1 = _mm512_mul_pd(cer1,inv1);
                           cei1 = _mm512_mul_pd(cei1,inv1);
                           ear2 = ear1;
                           eai2 = _mm512_mul_pd(eai1,calp);
                           inv2 = _mm512_rcp14_pd(_mm512_mul_pd(eai2,eai2));
                           x3   = _mm512_mul_pd(eai1,x2); 
                           cexp_zmm8r8(ear2,eai2,t0r,t0i);
                           cmul_zmm8r8(ear2,x3,t0r,t0i,&cer2,&cei2);
                           cer2 = _mm512_mul_pd(cer2,inv2);
                           cei2 = _mm512_mul_pd(cei2,inv2);
                           t1r  = _mm512_sub_pd(cer1,cer2);
                           t1i  = _mm512_sub_pd(cei1,cei2);
                           cabs = cabs_zmm8r8(t1r,t1i);
                           rcs  = _mm512_mul_pd(trm1,cabs);
                           return (rcs);
                 }


                    /*
                           RCS of pointed cone and flat plate of radius b.
                           Formula 6.2-30
                      */


                 
                   __m512d rcs_f6230_zmm8r8(const __m512d b,
                                            const __m512d k0) {

                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d _4 = _mm512_set1_pd(4.0f);
                          register __m512d b4,k02,k0b2,b2;
                          register __m512d trm1,trm1,rcs,x0;
                          b2  = _mm512_mul_pd(b,b);
                          k02 = _mm512_mul_pd(k0,k0);
                          b4  = _mm512_mul_pd(b2,b2);
                          k0b2= _mm512_mul_pd(k02,k02);
                          trm2= _mm512_mul_pd(k0b2,_mm512_mul_pd(pi,b2));
                          x0  = _mm512_mul_pd(_4,k02);
                          trm1= _mm512_div_pd(_mm512_mul_pd(pi,b4),x0);
                          rcs = _mm512_add_pd(trm1,trm2);
                          return (rcs);
                }


                  
                   __m512d rcs_f6230_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0) {

                          register __m512d b  = _mm512_load_pd(&pb[0]);
                          register __m512d k0 = _mm512_load_pd(&pk0[0]);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d _4 = _mm512_set1_pd(4.0f);
                          register __m512d b4,k02,k0b2,b2;
                          register __m512d trm1,trm1,rcs,x0;
                          b2  = _mm512_mul_pd(b,b);
                          k02 = _mm512_mul_pd(k0,k0);
                          b4  = _mm512_mul_pd(b2,b2);
                          k0b2= _mm512_mul_pd(k02,k02);
                          trm2= _mm512_mul_pd(k0b2,_mm512_mul_pd(pi,b2));
                          x0  = _mm512_mul_pd(_4,k02);
                          trm1= _mm512_div_pd(_mm512_mul_pd(pi,b4),x0);
                          rcs = _mm512_add_pd(trm1,trm2);
                          return (rcs);
                }


                
                   __m512d rcs_f6230_zmm8r8_u(const float * __restrict  pb,
                                              const float * __restrict  pk0) {

                          register __m512d b  = _mm512_loadu_pd(&pb[0]);
                          register __m512d k0 = _mm512_loadu_pd(&pk0[0]);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d _4 = _mm512_set1_pd(4.0f);
                          register __m512d b4,k02,k0b2,b2;
                          register __m512d trm1,trm1,rcs,x0;
                          b2  = _mm512_mul_pd(b,b);
                          k02 = _mm512_mul_pd(k0,k0);
                          b4  = _mm512_mul_pd(b2,b2);
                          k0b2= _mm512_mul_pd(k02,k02);
                          trm2= _mm512_mul_pd(k0b2,_mm512_mul_pd(pi,b2));
                          x0  = _mm512_mul_pd(_4,k02);
                          trm1= _mm512_div_pd(_mm512_mul_pd(pi,b4),x0);
                          rcs = _mm512_add_pd(trm1,trm2);
                          return (rcs);
                }


                  /*
                       Physical-Optics approximation of rounded-tip cone,
                       for axial incidence.
                       Formula 6.2-31
                   */


                 
                   __m512d rcs_f6231_zmm8r8(const __m512d alp,
                                            const __m512d k0,
                                            const __m512d b) {

                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          const __m512d _4 = _mm512_set1_pd(4.0f);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          register __m512d b2,k0b,_2k0b,k0b2,arg;
                          register __m512d sarg,carg,salp,calp,calp2,calp4;
                          register __m512d rcs,trm1,trm2,trm3,x0,x1;
                          b2   = _mm512_mul_pd(b,b);
                          calp = xcos(alp);
                          k0b  = _mm512_mul_pd(k0,b);
                          calp2= _mm512_mul_pd(calp,calp);
                          _2k0b= _mm512_add_pd(k0b,k0b);
                          calp4= _mm512_mul_pd(calp2,calp2);
                          k0b2 = _mm512_mul_pd(k0b,k0b);
                          salp = xsin(alp);
                          arg  = _mm512_mul_pd(_2k0b,_mm512_sub_pd(_1,salp));
                          x0   = _mm512_mul_pd(k0b,calp2);
                          sarg = xsin(arg);
                          x1   = _mm512_add_pd(_1,calp4);
                          trm1 = _mm512_div_pd(sarg,x0);
                          x0   = _mm512_mul_pd(_mm512_mul_pd(_4,k0b2),calp4);
                          trm2 = _mm512_div_pd(x1,x0);
                          carg = xcos(arg);
                          x0   = _mm512_mul_pd(_mm512_add_pd(k0b2,k0b2),calp2);
                          trm3 = _mm512_div_pd(carg,x0);
                          x1   = _mm512_mul_pd(pi,b2);
                          x0   = _mm512_add_pd(_mm512_sub_pd(_1,trm1),trm2);
                          rcs  = _mm512_mul_pd(x0,_mm512_sub_pd(x0,trm3));
                          return (rcs);
                 }


                 
                   __m512d rcs_f6231_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) palp,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0,
                                            const float * __restrict __ATTR_ALIGN__(64) pb) {

                          register __m512d alp= _mm512_load_pd(&palp[0]);
                          register __m512d b  = _mm512_load_pd(&pb[0]);
                          register __m512d k0 = _mm512_load_pd(&pk0[0]);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          const __m512d _4 = _mm512_set1_pd(4.0f);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          register __m512d b2,k0b,_2k0b,k0b2,arg;
                          register __m512d sarg,carg,salp,calp,calp2,calp4;
                          register __m512d rcs,trm1,trm2,trm3,x0,x1;
                          b2   = _mm512_mul_pd(b,b);
                          calp = xcos(alp);
                          k0b  = _mm512_mul_pd(k0,b);
                          calp2= _mm512_mul_pd(calp,calp);
                          _2k0b= _mm512_add_pd(k0b,k0b);
                          calp4= _mm512_mul_pd(calp2,calp2);
                          k0b2 = _mm512_mul_pd(k0b,k0b);
                          salp = xsin(alp);
                          arg  = _mm512_mul_pd(_2k0b,_mm512_sub_pd(_1,salp));
                          x0   = _mm512_mul_pd(k0b,calp2);
                          sarg = xsin(arg);
                          x1   = _mm512_add_pd(_1,calp4);
                          trm1 = _mm512_div_pd(sarg,x0);
                          x0   = _mm512_mul_pd(_mm512_mul_pd(_4,k0b2),calp4);
                          trm2 = _mm512_div_pd(x1,x0);
                          carg = xcos(arg);
                          x0   = _mm512_mul_pd(_mm512_add_pd(k0b2,k0b2),calp2);
                          trm3 = _mm512_div_pd(carg,x0);
                          x1   = _mm512_mul_pd(pi,b2);
                          x0   = _mm512_add_pd(_mm512_sub_pd(_1,trm1),trm2);
                          rcs  = _mm512_mul_pd(x0,_mm512_sub_pd(x0,trm3));
                          return (rcs);
                 }


                  
                   __m512d rcs_f6231_zmm8r8_u(const float * __restrict  palp,
                                            const float * __restrict  pk0,
                                            const float * __restrict  pb) {

                          register __m512d alp= _mm512_loadu_pd(&palp[0]);
                          register __m512d b  = _mm512_loadu_pd(&pb[0]);
                          register __m512d k0 = _mm512_loadu_pd(&pk0[0]);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          const __m512d _4 = _mm512_set1_pd(4.0f);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          register __m512d b2,k0b,_2k0b,k0b2,arg;
                          register __m512d sarg,carg,salp,calp,calp2,calp4;
                          register __m512d rcs,trm1,trm2,trm3,x0,x1;
                          b2   = _mm512_mul_pd(b,b);
                          calp = xcos(alp);
                          k0b  = _mm512_mul_pd(k0,b);
                          calp2= _mm512_mul_pd(calp,calp);
                          _2k0b= _mm512_add_pd(k0b,k0b);
                          calp4= _mm512_mul_pd(calp2,calp2);
                          k0b2 = _mm512_mul_pd(k0b,k0b);
                          salp = xsin(alp);
                          arg  = _mm512_mul_pd(_2k0b,_mm512_sub_pd(_1,salp));
                          x0   = _mm512_mul_pd(k0b,calp2);
                          sarg = xsin(arg);
                          x1   = _mm512_add_pd(_1,calp4);
                          trm1 = _mm512_div_pd(sarg,x0);
                          x0   = _mm512_mul_pd(_mm512_mul_pd(_4,k0b2),calp4);
                          trm2 = _mm512_div_pd(x1,x0);
                          carg = xcos(arg);
                          x0   = _mm512_mul_pd(_mm512_add_pd(k0b2,k0b2),calp2);
                          trm3 = _mm512_div_pd(carg,x0);
                          x1   = _mm512_mul_pd(pi,b2);
                          x0   = _mm512_add_pd(_mm512_sub_pd(_1,trm1),trm2);
                          rcs  = _mm512_mul_pd(x0,_mm512_sub_pd(x0,trm3));
                          return (rcs);
                 }


                   /*
                          Not nose-one incidence, for theta << alpha -- 
                          PO approximation for rounded-tip cones.
                          Formula 6.2-23
                      */


                   __m512d rcs_f6223_zmm8r8(const __m512d tht,
                                            const __m512d k0,
                                            const __m512d b,
                                            const __m512d a,
                                            const __m512d alp) {

                          const __m512d hlf= _mm512_set1_pd(0.5f);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          const __m512d _4 = _mm512_set1_pd(4.0f);
                          const __m512d n4 = _mm512_set1_pd(-4.0f);
                          const __m512d _3 = _mm512_set1_pd(3.0f);
                          const __m512d _2 = _mm512_set1_pd(2.0f);
                          const __m512d n2 = _mm512_set1_pd(-2.0f);
                          const __m512d _6 = _mm512_set1_pd(6.0f);
                          const __m512d _8 = _mm512_set1_pd(8.0f);
                          const __m512d _13= _mm512_set1_pd(13.0f);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          register __m512d k0b,salp,arg,_2k0,b2,sarg,carg,ctht,a2,salp1,tht3;
                          register __m512d trm1,trm2,trm3;
                          register __m512d rcs,A1,A2,A3;
                          __m512d alp2,tht2,alp4,tht4,k0b2,k0b3,k0b4,a2,k0bt,k0a;
                          __m512d t0,t1,t2,t3,t4,t5,t6,t7,t8,t9;
                          __m512d t10,t11,t12,t13,t14,t15,t16,t17,t18,t19;
                          __m512d x0,x1,x2,x3,x4;
                          k0b  = _mm512_mul_pd(k0,b);
                          tht2 = _mm512_mul_pd(tht,tht);
                          _2k0 = _mm512_add_pd(k0,k0);
                          salp = xsin(alp);
                          k0b2 = _mm512_mul_pd(k0b,k0b);
                          ctht = xcos(tht);
                          a2   = _mm512_mul_pd(a,a);
                          salp1= _mm512_sub_pd(_1,salp);
                          k0a  = _mm512_mul_pd(k0,a);
                          alp2 = _mm512_mul_pd(alp,alp);
                          tht3 = _mm512_mul_pd(tht2,tht);
                          arg  = _mm512_mul_pd(_2k0,_mm512_mul_pd(calp,salp1));
                          tht4 = _mm512_mul_pd(tht2,tht2);
                          alp4 = _mm512_mul_pd(alp2,alp2);
                          k0b3 = _mm512_mul_pd(k0b2,k0b);
                          k0b4 = _mm512_mul_pd(k0b2,k0b2);
                          trm1 = _mm512_div_pd(_mm512_add_pd(_1,tht2),
                                               _mm512_mul_pd(_4,k0b2));
                          a2   = _mm512_mul_pd(a,a);
                          k0bt = _mm512_mul_pd(k0b,tht);
                          t0   = _mm512_add_pd(alp2,alp2); //2alp^2
                          t1   = _mm512_add_pd(tht2,tht2); //2tht^2
                          t2   = _mm512_mul_pd(alp2,tht2);//alp2*tht2
                          t3   = _mm512_mul_pd(tht4,hlf); //tht4*0.5
                          t4   = _mm512_mul_pd(_mm512_mul_pd(_2,alp4),tht2);//2*alp4*tht2
                          t5   = _mm512_mul_pd(_4,k0b2);//4*k0b2
                          t6   = _mm512_mul_pd(_mm512_mul_pd(_2,k0b2),tht2);//2*k0b2*tht2
                          t7   = _mm512_mul_pd(_mm512_mul_pd(_8,k0b3),tht2);//8*k0b3*tht2
                          t8   = _mm512_mul_pd(k0b2,tht4);//k0b2*tht4
                          t9   = _mm512_mul_pd(_mm512_mul_pd(_6,k0b2),
                                               _mm512_mul_pd(a2,tht2));//6*k0b2*a2*tht2
                          t10  = _mm512_mul_pd(_mm512_mul_pd(_8,k0b3),tht4);//8*k0b3*tht4
                          t11  = _mm512_mul_pd(_mm512_mul_pd(_13,k0b4),tht4);//13*k0b4*tht4
                          sarg = xsin(arg);
                          x0   = _mm512_sub_pd(_mm512_add_pd(_2,t0),
                                               _mm512_add_pd(t1,alp4));
                          x1   = _mm512_add_pd(_mm512_add_pd(t2,t3),
                                               _mm512_add_pd(t4,t5));
                          x2   = _mm512_add_pd(_mm512_sub_pd(t6,t7),
                                               _mm512_add_pd(t8,t9));
                          x3   = _mm512_add_pd(t10,t11);
                          x4   = _mm512_sub_pd(x0,x1);
                          carg = xcos(arg);
                          t9   = _mm512_mul_pd(_mm512_mul_pd(_6,k0b2),tht2);
                          A1   = _mm512_sub_pd(_mm512_sub_pd(x0,x1),
                                               _mm512_add_pd(x2,x3));
                          t7   = _mm512_mul_pd(_mm512_mul_pd(_8,k0b4),tht3);
                          x0   = _mm512_sub_pd(_mm512_sub_pd(n2,t0),
                                               _mm512_add_pd(t1,t2));
                          t8   = _mm512_mul_pd(_mm512_mul_pd(_3,k0b2),tht4);
                          x1   = _mm512_add_pd(_mm512_sub_pd(t3,t9),
                                               _mm512_add_pd(t7,t8));
                          A2   = _mm512_sub_pd(x0,x1);
                          t11  = _mm512_mul_pd(k0bt,k0bt);
                          t10  = _mm512_mul_pd(t11,k0bt);
                          t9   = _mm512_mul_pd(k0b,tht2);
                          t8   = _mm512_mul_pd(_3,_mm512_mul_pd(k0a,k0a));
                          x0   = _mm512_sub_pd(_mm512_add_pd(_1,alp2),
                                               _mm512_add_pd(t2,t8));
                          x1   = _mm512_sub_pd(k0b,_mm512_sub_pd(t9,t11));
                          A3   = _mm512_mul_pd(_mm512_mul_pd(n4,x0),
                                               _mm512_sub_pd(x1,_mm512_mul_pd(_4,t10)));
                          trm2 = _mm512_fmadd_pd(carg,A2,A1);
                          trm3 = _mm512_fmadd_pd(sarg,A3,trm2);
                          rcs  = _mm512_mul_pd(trm1,trm3);
                          return (rcs);
                 }


                 
                   __m512d rcs_f6223_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) ptht,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0,
                                            const float * __restrict __ATTR_ALIGN__(64) pb,
                                            const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) palp) {

                          register __m512d tht  = _mm512_load_pd(&ptht[0]);
                          register __m512d k0   = _mm512_load_pd(&pk0[0]);
                          register __m512d b    = _mm512_load_pd(&pb[0]);
                          register __m512d a    = _mm512_load_pd(&pa[0]);
                          register __m512d alp  = _mm512_load_pd(&palp[0]);
                          const __m512d hlf= _mm512_set1_pd(0.5f);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          const __m512d _4 = _mm512_set1_pd(4.0f);
                          const __m512d n4 = _mm512_set1_pd(-4.0f);
                          const __m512d _3 = _mm512_set1_pd(3.0f);
                          const __m512d _2 = _mm512_set1_pd(2.0f);
                          const __m512d n2 = _mm512_set1_pd(-2.0f);
                          const __m512d _6 = _mm512_set1_pd(6.0f);
                          const __m512d _8 = _mm512_set1_pd(8.0f);
                          const __m512d _13= _mm512_set1_pd(13.0f);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          register __m512d k0b,salp,arg,_2k0,b2,sarg,carg,ctht,a2,salp1,tht3;
                          register __m512d trm1,trm2,trm3;
                          register __m512d rcs,A1,A2,A3;
                          __m512d alp2,tht2,alp4,tht4,k0b2,k0b3,k0b4,a2,k0bt,k0a;
                          __m512d t0,t1,t2,t3,t4,t5,t6,t7,t8,t9;
                          __m512d t10,t11,t12,t13,t14,t15,t16,t17,t18,t19;
                          __m512d x0,x1,x2,x3,x4;
                          k0b  = _mm512_mul_pd(k0,b);
                          tht2 = _mm512_mul_pd(tht,tht);
                          _2k0 = _mm512_add_pd(k0,k0);
                          salp = xsin(alp);
                          k0b2 = _mm512_mul_pd(k0b,k0b);
                          ctht = xcos(tht);
                          a2   = _mm512_mul_pd(a,a);
                          salp1= _mm512_sub_pd(_1,salp);
                          k0a  = _mm512_mul_pd(k0,a);
                          alp2 = _mm512_mul_pd(alp,alp);
                          tht3 = _mm512_mul_pd(tht2,tht);
                          arg  = _mm512_mul_pd(_2k0,_mm512_mul_pd(calp,salp1));
                          tht4 = _mm512_mul_pd(tht2,tht2);
                          alp4 = _mm512_mul_pd(alp2,alp2);
                          k0b3 = _mm512_mul_pd(k0b2,k0b);
                          k0b4 = _mm512_mul_pd(k0b2,k0b2);
                          trm1 = _mm512_div_pd(_mm512_add_pd(_1,tht2),
                                               _mm512_mul_pd(_4,k0b2));
                          a2   = _mm512_mul_pd(a,a);
                          k0bt = _mm512_mul_pd(k0b,tht);
                          t0   = _mm512_add_pd(alp2,alp2); //2alp^2
                          t1   = _mm512_add_pd(tht2,tht2); //2tht^2
                          t2   = _mm512_mul_pd(alp2,tht2);//alp2*tht2
                          t3   = _mm512_mul_pd(tht4,hlf); //tht4*0.5
                          t4   = _mm512_mul_pd(_mm512_mul_pd(_2,alp4),tht2);//2*alp4*tht2
                          t5   = _mm512_mul_pd(_4,k0b2);//4*k0b2
                          t6   = _mm512_mul_pd(_mm512_mul_pd(_2,k0b2),tht2);//2*k0b2*tht2
                          t7   = _mm512_mul_pd(_mm512_mul_pd(_8,k0b3),tht2);//8*k0b3*tht2
                          t8   = _mm512_mul_pd(k0b2,tht4);//k0b2*tht4
                          t9   = _mm512_mul_pd(_mm512_mul_pd(_6,k0b2),
                                               _mm512_mul_pd(a2,tht2));//6*k0b2*a2*tht2
                          t10  = _mm512_mul_pd(_mm512_mul_pd(_8,k0b3),tht4);//8*k0b3*tht4
                          t11  = _mm512_mul_pd(_mm512_mul_pd(_13,k0b4),tht4);//13*k0b4*tht4
                          sarg = xsin(arg);
                          x0   = _mm512_sub_pd(_mm512_add_pd(_2,t0),
                                               _mm512_add_pd(t1,alp4));
                          x1   = _mm512_add_pd(_mm512_add_pd(t2,t3),
                                               _mm512_add_pd(t4,t5));
                          x2   = _mm512_add_pd(_mm512_sub_pd(t6,t7),
                                               _mm512_add_pd(t8,t9));
                          x3   = _mm512_add_pd(t10,t11);
                          x4   = _mm512_sub_pd(x0,x1);
                          carg = xcos(arg);
                          t9   = _mm512_mul_pd(_mm512_mul_pd(_6,k0b2),tht2);
                          A1   = _mm512_sub_pd(_mm512_sub_pd(x0,x1),
                                               _mm512_add_pd(x2,x3));
                          t7   = _mm512_mul_pd(_mm512_mul_pd(_8,k0b4),tht3);
                          x0   = _mm512_sub_pd(_mm512_sub_pd(n2,t0),
                                               _mm512_add_pd(t1,t2));
                          t8   = _mm512_mul_pd(_mm512_mul_pd(_3,k0b2),tht4);
                          x1   = _mm512_add_pd(_mm512_sub_pd(t3,t9),
                                               _mm512_add_pd(t7,t8));
                          A2   = _mm512_sub_pd(x0,x1);
                          t11  = _mm512_mul_pd(k0bt,k0bt);
                          t10  = _mm512_mul_pd(t11,k0bt);
                          t9   = _mm512_mul_pd(k0b,tht2);
                          t8   = _mm512_mul_pd(_3,_mm512_mul_pd(k0a,k0a));
                          x0   = _mm512_sub_pd(_mm512_add_pd(_1,alp2),
                                               _mm512_add_pd(t2,t8));
                          x1   = _mm512_sub_pd(k0b,_mm512_sub_pd(t9,t11));
                          A3   = _mm512_mul_pd(_mm512_mul_pd(n4,x0),
                                               _mm512_sub_pd(x1,_mm512_mul_pd(_4,t10)));
                          trm2 = _mm512_fmadd_pd(carg,A2,A1);
                          trm3 = _mm512_fmadd_pd(sarg,A3,trm2);
                          rcs  = _mm512_mul_pd(trm1,trm3);
                          return (rcs);
                 }


                   
                   __m512d rcs_f6223_zmm8r8_u(const float * __restrict  ptht,
                                            const float * __restrict  pk0,
                                            const float * __restrict  pb,
                                            const float * __restrict  pa,
                                            const float * __restrict  palp) {

                          register __m512d tht  = _mm512_loadu_pd(&ptht[0]);
                          register __m512d k0   = _mm512_loadu_pd(&pk0[0]);
                          register __m512d b    = _mm512_loadu_pd(&pb[0]);
                          register __m512d a    = _mm512_loadu_pd(&pa[0]);
                          register __m512d alp  = _mm512_loadu_pd(&palp[0]);
                          const __m512d hlf= _mm512_set1_pd(0.5f);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          const __m512d _4 = _mm512_set1_pd(4.0f);
                          const __m512d n4 = _mm512_set1_pd(-4.0f);
                          const __m512d _3 = _mm512_set1_pd(3.0f);
                          const __m512d _2 = _mm512_set1_pd(2.0f);
                          const __m512d n2 = _mm512_set1_pd(-2.0f);
                          const __m512d _6 = _mm512_set1_pd(6.0f);
                          const __m512d _8 = _mm512_set1_pd(8.0f);
                          const __m512d _13= _mm512_set1_pd(13.0f);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          register __m512d k0b,salp,arg,_2k0,b2,sarg,carg,ctht,a2,salp1,tht3;
                          register __m512d trm1,trm2,trm3;
                          register __m512d rcs,A1,A2,A3;
                          __m512d alp2,tht2,alp4,tht4,k0b2,k0b3,k0b4,a2,k0bt,k0a;
                          __m512d t0,t1,t2,t3,t4,t5,t6,t7,t8,t9;
                          __m512d t10,t11,t12,t13,t14,t15,t16,t17,t18,t19;
                          __m512d x0,x1,x2,x3,x4;
                          k0b  = _mm512_mul_pd(k0,b);
                          tht2 = _mm512_mul_pd(tht,tht);
                          _2k0 = _mm512_add_pd(k0,k0);
                          salp = xsin(alp);
                          k0b2 = _mm512_mul_pd(k0b,k0b);
                          ctht = xcos(tht);
                          a2   = _mm512_mul_pd(a,a);
                          salp1= _mm512_sub_pd(_1,salp);
                          k0a  = _mm512_mul_pd(k0,a);
                          alp2 = _mm512_mul_pd(alp,alp);
                          tht3 = _mm512_mul_pd(tht2,tht);
                          arg  = _mm512_mul_pd(_2k0,_mm512_mul_pd(calp,salp1));
                          tht4 = _mm512_mul_pd(tht2,tht2);
                          alp4 = _mm512_mul_pd(alp2,alp2);
                          k0b3 = _mm512_mul_pd(k0b2,k0b);
                          k0b4 = _mm512_mul_pd(k0b2,k0b2);
                          trm1 = _mm512_div_pd(_mm512_add_pd(_1,tht2),
                                               _mm512_mul_pd(_4,k0b2));
                          a2   = _mm512_mul_pd(a,a);
                          k0bt = _mm512_mul_pd(k0b,tht);
                          t0   = _mm512_add_pd(alp2,alp2); //2alp^2
                          t1   = _mm512_add_pd(tht2,tht2); //2tht^2
                          t2   = _mm512_mul_pd(alp2,tht2);//alp2*tht2
                          t3   = _mm512_mul_pd(tht4,hlf); //tht4*0.5
                          t4   = _mm512_mul_pd(_mm512_mul_pd(_2,alp4),tht2);//2*alp4*tht2
                          t5   = _mm512_mul_pd(_4,k0b2);//4*k0b2
                          t6   = _mm512_mul_pd(_mm512_mul_pd(_2,k0b2),tht2);//2*k0b2*tht2
                          t7   = _mm512_mul_pd(_mm512_mul_pd(_8,k0b3),tht2);//8*k0b3*tht2
                          t8   = _mm512_mul_pd(k0b2,tht4);//k0b2*tht4
                          t9   = _mm512_mul_pd(_mm512_mul_pd(_6,k0b2),
                                               _mm512_mul_pd(a2,tht2));//6*k0b2*a2*tht2
                          t10  = _mm512_mul_pd(_mm512_mul_pd(_8,k0b3),tht4);//8*k0b3*tht4
                          t11  = _mm512_mul_pd(_mm512_mul_pd(_13,k0b4),tht4);//13*k0b4*tht4
                          sarg = xsin(arg);
                          x0   = _mm512_sub_pd(_mm512_add_pd(_2,t0),
                                               _mm512_add_pd(t1,alp4));
                          x1   = _mm512_add_pd(_mm512_add_pd(t2,t3),
                                               _mm512_add_pd(t4,t5));
                          x2   = _mm512_add_pd(_mm512_sub_pd(t6,t7),
                                               _mm512_add_pd(t8,t9));
                          x3   = _mm512_add_pd(t10,t11);
                          x4   = _mm512_sub_pd(x0,x1);
                          carg = xcos(arg);
                          t9   = _mm512_mul_pd(_mm512_mul_pd(_6,k0b2),tht2);
                          A1   = _mm512_sub_pd(_mm512_sub_pd(x0,x1),
                                               _mm512_add_pd(x2,x3));
                          t7   = _mm512_mul_pd(_mm512_mul_pd(_8,k0b4),tht3);
                          x0   = _mm512_sub_pd(_mm512_sub_pd(n2,t0),
                                               _mm512_add_pd(t1,t2));
                          t8   = _mm512_mul_pd(_mm512_mul_pd(_3,k0b2),tht4);
                          x1   = _mm512_add_pd(_mm512_sub_pd(t3,t9),
                                               _mm512_add_pd(t7,t8));
                          A2   = _mm512_sub_pd(x0,x1);
                          t11  = _mm512_mul_pd(k0bt,k0bt);
                          t10  = _mm512_mul_pd(t11,k0bt);
                          t9   = _mm512_mul_pd(k0b,tht2);
                          t8   = _mm512_mul_pd(_3,_mm512_mul_pd(k0a,k0a));
                          x0   = _mm512_sub_pd(_mm512_add_pd(_1,alp2),
                                               _mm512_add_pd(t2,t8));
                          x1   = _mm512_sub_pd(k0b,_mm512_sub_pd(t9,t11));
                          A3   = _mm512_mul_pd(_mm512_mul_pd(n4,x0),
                                               _mm512_sub_pd(x1,_mm512_mul_pd(_4,t10)));
                          trm2 = _mm512_fmadd_pd(carg,A2,A1);
                          trm3 = _mm512_fmadd_pd(sarg,A3,trm2);
                          rcs  = _mm512_mul_pd(trm1,trm3);
                          return (rcs);
                 }


                  /*
                         Finite cones, approximated solutions.
                         Rayleigh region.
                         RCS.
                         Formula 6.3-4
                     */

#include "GMS_simd_utils.h"   

                  
              
                   __m512d rcs_f634_zmm8r8(const __m512d k0,
                                           const __m512d a,
                                           const __m512d h) {

                         
                          const __m512d c0 = _mm512_set1_pd(1.396263401595463661538952614791);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          const __m512d _4 = _mm512_set1_pd(4.0f);
                          register __m512d k04,a4,h2,_4a,arg,earg,pih,x0,x1,nh;
                          register __m512d rcs,trm1,trm2;
                          h2  = _mm512_mul_pd(h,h);
                          x0  = _mm512_mul_pd(k0,k0);
                          x1  = _mm512_mul_pd(a,a);
                          _4a = _mm512_mul_pd(_4,a);
                          pih = _mm512_mul_pd(_mm512_set1_pd(3.14159265358979323846264338328),h);
                          nh  = negate_zmm8r8(h);
                          k04 = _mm512_mul_pd(x0,x0);
                          arg = _mm512_div_pd(nh,_4a);
                          a4  = _mm512_mul_pd(x1,x1);
                          earg= xexpf(arg);
                          trm1= _mm512_mul_pd(_mm512_mul_pd(k04,a4),h2);
                          x0  = _mm512_div_pd(_mm512_mul_pd(_4,earg),pih);
                          trm1= _mm512_mul_pd(c0,trm1);
                          x1  = _mm512_add_pd(_1,x0);
                          trm2= _mm512_mul_pd(x1,x1);
                          rcs = _mm512_mul_pd(trm1,trm2);
                          return (rcs);
                 }


                  
                   __m512d rcs_f634_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) ph) {

                        
                          register __m512d k0  = _mm512_load_pd(&pk0[0]);
                          register __m512d a   = _mm512_load_pd(&pa[0]);
                          register __m512d h   = _mm512_load_pd(&ph[0]);
                          const __m512d c0 = _mm512_set1_pd(1.396263401595463661538952614791);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          const __m512d _4 = _mm512_set1_pd(4.0f);
                          register __m512d k04,a4,h2,_4a,arg,earg,pih,x0,x1,nh;
                          register __m512d rcs,trm1,trm2;
                          h2  = _mm512_mul_pd(h,h);
                          x0  = _mm512_mul_pd(k0,k0);
                          x1  = _mm512_mul_pd(a,a);
                          _4a = _mm512_mul_pd(_4,a);
                          pih = _mm512_mul_pd(_mm512_set1_pd(3.14159265358979323846264338328),h);
                          nh  = negate_zmm8r8(h);
                          k04 = _mm512_mul_pd(x0,x0);
                          arg = _mm512_div_pd(nh,_4a);
                          a4  = _mm512_mul_pd(x1,x1);
                          earg= xexpf(arg);
                          trm1= _mm512_mul_pd(_mm512_mul_pd(k04,a4),h2);
                          x0  = _mm512_div_pd(_mm512_mul_pd(_4,earg),pih);
                          trm1= _mm512_mul_pd(c0,trm1);
                          x1  = _mm512_add_pd(_1,x0);
                          trm2= _mm512_mul_pd(x1,x1);
                          rcs = _mm512_mul_pd(trm1,trm2);
                          return (rcs);
                 }


                 
                   __m512d rcs_f634_zmm8r8_u(const float * __restrict pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict ph) {

                          
                          register __m512d k0  = _mm512_loadu_pd(&pk0[0]);
                          register __m512d a   = _mm512_loadu_pd(&pa[0]);
                          register __m512d h   = _mm512_loadu_pd(&ph[0]);
                          const __m512d c0 = _mm512_set1_pd(1.396263401595463661538952614791);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          const __m512d _4 = _mm512_set1_pd(4.0f);
                          register __m512d k04,a4,h2,_4a,arg,earg,pih,x0,x1,nh;
                          register __m512d rcs,trm1,trm2;
                          h2  = _mm512_mul_pd(h,h);
                          x0  = _mm512_mul_pd(k0,k0);
                          x1  = _mm512_mul_pd(a,a);
                          _4a = _mm512_mul_pd(_4,a);
                          pih = _mm512_mul_pd(_mm512_set1_pd(3.14159265358979323846264338328),h);
                          nh  = negate_zmm8r8(h);
                          k04 = _mm512_mul_pd(x0,x0);
                          arg = _mm512_div_pd(nh,_4a);
                          a4  = _mm512_mul_pd(x1,x1);
                          earg= xexpf(arg);
                          trm1= _mm512_mul_pd(_mm512_mul_pd(k04,a4),h2);
                          x0  = _mm512_div_pd(_mm512_mul_pd(_4,earg),pih);
                          trm1= _mm512_mul_pd(c0,trm1);
                          x1  = _mm512_add_pd(_1,x0);
                          trm2= _mm512_mul_pd(x1,x1);
                          rcs = _mm512_mul_pd(trm1,trm2);
                          return (rcs);
                 }


                   /*
                          Rayleigh RCS of cone-hemispheroid.
                          Formula 6.3-5
                     */


                 
                   __m512d rcs_f635_zmm8r8(const __m512d k0,
                                           const __m512d a,
                                           const __m512d b,
                                           const __m512d h) {

                        
                          const __m512d _4pi = _mm512_set1_pd(1.27323954473516268615107010698);
                          const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d c0   = _mm512_set1_pd(0.33333333333333333333333333333);
                          const __m512d c1   = _mm512_set1_pd(0.666666666666666666666666666667);
                          const __m512d _4   = _mm512_set1_pd(4.0f);
                          const __m512d _2   = _mm512_set1_pd(2.0f);
                          const __m512d _1   = _mm512_set1_pd(1.0f);
                          register __m512d k04,a2,_4a,h2b,nh2b,pia2,arg,earg,x0,x1,x2;
                          register __m512d rcs,trm1,trm2,trm3;
                          x0  = _mm512_mul_pd(k0,k0);
                          a2  = _mm512_mul_pd(a,a);
                          h2b = _mm512_fmadd_pd(b,_2,h);
                          k04 = _mm512_mul_pd(x0,x0);
                          _4a = _mm512_mul_pd(_4,a);
                          trm1= _mm512_mul_pd(_4pi,k04);
                          pia2= _mm512_mul_pd(pi,a2);
                          nh2b= negate_zmm8r8(h2b);
                          x0  = _mm512_mul_pd(c0,_mm512_mul_pd(pia2,h));
                          arg = _mm512_div_pd(nh2b,_4a);
                          x1  = _mm512_mul_pd(c1,_mm512_mul_pd(pia2,b));
                          earg= xexpf(arg);
                          x2  = _mm512_add_pd(x0,x1);
                          trm2= _mm512_mul_pd(x2,x2);
                          x0  = _mm512_div_pd(_mm512_mul_pd(pi,h2b),_4a);
                          x1  = _mm512_add_pd(_1, _mm512_div_pd(earg,x0));
                          trm2= _mm512_mul_pd(x1,x1);
                          rcs = _mm512_mul_pd(trm1,_mm512_mul_pd(trm2,trm3));
                          return (rcs);
                }


                 
                   __m512d rcs_f635_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) pb,
                                             const float * __restrict __ATTR_ALIGN__(64) ph) {
                                             
                        
                          register __m512d k0  = _mm512_load_pd(&pk0[0]);
                          register __m512d a   = _mm512_load_pd(&pa[0]);
                          register __m512d b   = _mm512_load_pd(&pb[0]);
                          register __m512d h   = _mm512_load_pd(&ph[0]);
                          const __m512d _4pi = _mm512_set1_pd(1.27323954473516268615107010698);
                          const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d c0   = _mm512_set1_pd(0.33333333333333333333333333333);
                          const __m512d c1   = _mm512_set1_pd(0.666666666666666666666666666667);
                          const __m512d _4   = _mm512_set1_pd(4.0f);
                          const __m512d _2   = _mm512_set1_pd(2.0f);
                          const __m512d _1   = _mm512_set1_pd(1.0f);
                          register __m512d k04,a2,_4a,h2b,nh2b,pia2,arg,earg,x0,x1,x2;
                          register __m512d rcs,trm1,trm2,trm3;
                          x0  = _mm512_mul_pd(k0,k0);
                          a2  = _mm512_mul_pd(a,a);
                          h2b = _mm512_fmadd_pd(b,_2,h);
                          k04 = _mm512_mul_pd(x0,x0);
                          _4a = _mm512_mul_pd(_4,a);
                          trm1= _mm512_mul_pd(_4pi,k04);
                          pia2= _mm512_mul_pd(pi,a2);
                          nh2b= negate_zmm8r8(h2b);
                          x0  = _mm512_mul_pd(c0,_mm512_mul_pd(pia2,h));
                          arg = _mm512_div_pd(nh2b,_4a);
                          x1  = _mm512_mul_pd(c1,_mm512_mul_pd(pia2,b));
                          earg= xexpf(arg);
                          x2  = _mm512_add_pd(x0,x1);
                          trm2= _mm512_mul_pd(x2,x2);
                          x0  = _mm512_div_pd(_mm512_mul_pd(pi,h2b),_4a);
                          x1  = _mm512_add_pd(_1, _mm512_div_pd(earg,x0));
                          trm2= _mm512_mul_pd(x1,x1);
                          rcs = _mm512_mul_pd(trm1,_mm512_mul_pd(trm2,trm3));
                          return (rcs);
                }


                 
                   __m512d rcs_f635_zmm8r8_u(const float * __restrict  pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict  pb,
                                             const float * __restrict  ph) {
                                             
                         
                          register __m512d k0  = _mm512_loadu_pd(&pk0[0]);
                          register __m512d a   = _mm512_loadu_pd(&pa[0]);
                          register __m512d b   = _mm512_loadu_pd(&pb[0]);
                          register __m512d h   = _mm512_loadu_pd(&ph[0]);
                          const __m512d _4pi = _mm512_set1_pd(1.27323954473516268615107010698);
                          const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d c0   = _mm512_set1_pd(0.33333333333333333333333333333);
                          const __m512d c1   = _mm512_set1_pd(0.666666666666666666666666666667);
                          const __m512d _4   = _mm512_set1_pd(4.0f);
                          const __m512d _2   = _mm512_set1_pd(2.0f);
                          const __m512d _1   = _mm512_set1_pd(1.0f);
                          register __m512d k04,a2,_4a,h2b,nh2b,pia2,arg,earg,x0,x1,x2;
                          register __m512d rcs,trm1,trm2,trm3;
                          x0  = _mm512_mul_pd(k0,k0);
                          a2  = _mm512_mul_pd(a,a);
                          h2b = _mm512_fmadd_pd(b,_2,h);
                          k04 = _mm512_mul_pd(x0,x0);
                          _4a = _mm512_mul_pd(_4,a);
                          trm1= _mm512_mul_pd(_4pi,k04);
                          pia2= _mm512_mul_pd(pi,a2);
                          nh2b= negate_zmm8r8(h2b);
                          x0  = _mm512_mul_pd(c0,_mm512_mul_pd(pia2,h));
                          arg = _mm512_div_pd(nh2b,_4a);
                          x1  = _mm512_mul_pd(c1,_mm512_mul_pd(pia2,b));
                          earg= xexpf(arg);
                          x2  = _mm512_add_pd(x0,x1);
                          trm2= _mm512_mul_pd(x2,x2);
                          x0  = _mm512_div_pd(_mm512_mul_pd(pi,h2b),_4a);
                          x1  = _mm512_add_pd(_1, _mm512_div_pd(earg,x0));
                          trm2= _mm512_mul_pd(x1,x1);
                          rcs = _mm512_mul_pd(trm1,_mm512_mul_pd(trm2,trm3));
                          return (rcs);
                }


                  /*
                          Rayleigh RCS of cone-hemispheroid (b == a)
                          Formula 6.3-6
                      */

                 
                   __m512d rcs_f636_zmm8r8(const __m512d k0,
                                           const __m512d a,
                                           const __m512d h) {

                        
                          const __m512d _4pi = _mm512_set1_pd(1.27323954473516268615107010698);
                          const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d c0   = _mm512_set1_pd(0.33333333333333333333333333333);
                          const __m512d c1   = _mm512_set1_pd(0.666666666666666666666666666667);
                          const __m512d _4   = _mm512_set1_pd(4.0f);
                          const __m512d _2   = _mm512_set1_pd(2.0f);
                          const __m512d _1   = _mm512_set1_pd(1.0f);
                          register __m512d k04,a2,_4a,a3,h2a,nh2a,pia2,pia3,arg,earg,x0,x1,x2;
                          register __m512d rcs,trm1,trm2,trm3;
                          x0  = _mm512_mul_pd(k0,k0);
                          a2  = _mm512_mul_pd(a,a);
                          h2a = _mm512_fmadd_pd(a,_2,h);
                          a3  = _mm512_mul_pd(a2,a);
                          k04 = _mm512_mul_pd(x0,x0);
                          _4a = _mm512_mul_pd(_4,a);
                          trm1= _mm512_mul_pd(_4pi,k04);
                          pia2= _mm512_mul_pd(pi,a2);
                          nh2a= negate_zmm8r8(h2a);
                          x0  = _mm512_mul_pd(c0,_mm512_mul_pd(pia2,h));
                          pia3= _mm512_mul_pd(pi,a3);
                          arg = _mm512_div_pd(nh2a,_4a);
                          x1  = _mm512_mul_pd(c1,pia3);
                          earg= xexpf(arg);
                          x2  = _mm512_add_pd(x0,x1);
                          trm2= _mm512_mul_pd(x2,x2);
                          x0  = _mm512_div_pd(_mm512_mul_pd(pi,h2a),_4a);
                          x1  = _mm512_add_pd(_1, _mm512_div_pd(earg,x0));
                          trm2= _mm512_mul_pd(x1,x1);
                          rcs = _mm512_mul_pd(trm1,_mm512_mul_pd(trm2,trm3));
                          return (rcs);
                }


                
                   __m512d rcs_f636_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) ph) {

                        
                          register __m512d k0  = _mm512_load_pd(&pk0[0]);
                          register __m512d a   = _mm512_load_pd(&pa[0]);
                          register __m512d h   = _mm512_load_pd(&ph[0]);
                          const __m512d _4pi = _mm512_set1_pd(1.27323954473516268615107010698);
                          const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d c0   = _mm512_set1_pd(0.33333333333333333333333333333);
                          const __m512d c1   = _mm512_set1_pd(0.666666666666666666666666666667);
                          const __m512d _4   = _mm512_set1_pd(4.0f);
                          const __m512d _2   = _mm512_set1_pd(2.0f);
                          const __m512d _1   = _mm512_set1_pd(1.0f);
                          register __m512d k04,a2,_4a,a3,h2a,nh2a,pia2,pia3,arg,earg,x0,x1,x2;
                          register __m512d rcs,trm1,trm2,trm3;
                          x0  = _mm512_mul_pd(k0,k0);
                          a2  = _mm512_mul_pd(a,a);
                          h2a = _mm512_fmadd_pd(a,_2,h);
                          a3  = _mm512_mul_pd(a2,a);
                          k04 = _mm512_mul_pd(x0,x0);
                          _4a = _mm512_mul_pd(_4,a);
                          trm1= _mm512_mul_pd(_4pi,k04);
                          pia2= _mm512_mul_pd(pi,a2);
                          nh2a= negate_zmm8r8(h2a);
                          x0  = _mm512_mul_pd(c0,_mm512_mul_pd(pia2,h));
                          pia3= _mm512_mul_pd(pi,a3);
                          arg = _mm512_div_pd(nh2a,_4a);
                          x1  = _mm512_mul_pd(c1,pia3);
                          earg= xexpf(arg);
                          x2  = _mm512_add_pd(x0,x1);
                          trm2= _mm512_mul_pd(x2,x2);
                          x0  = _mm512_div_pd(_mm512_mul_pd(pi,h2a),_4a);
                          x1  = _mm512_add_pd(_1, _mm512_div_pd(earg,x0));
                          trm2= _mm512_mul_pd(x1,x1);
                          rcs = _mm512_mul_pd(trm1,_mm512_mul_pd(trm2,trm3));
                          return (rcs);
                }


                
                   __m512d rcs_f636_zmm8r8_u(const float * __restrict  pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict  ph) {

                        
                          register __m512d k0  = _mm512_loadu_pd(&pk0[0]);
                          register __m512d a   = _mm512_loadu_pd(&pa[0]);
                          register __m512d h   = _mm512_loadu_pd(&ph[0]);
                          const __m512d _4pi = _mm512_set1_pd(1.27323954473516268615107010698);
                          const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d c0   = _mm512_set1_pd(0.33333333333333333333333333333);
                          const __m512d c1   = _mm512_set1_pd(0.666666666666666666666666666667);
                          const __m512d _4   = _mm512_set1_pd(4.0f);
                          const __m512d _2   = _mm512_set1_pd(2.0f);
                          const __m512d _1   = _mm512_set1_pd(1.0f);
                          register __m512d k04,a2,_4a,a3,h2a,nh2a,pia2,pia3,arg,earg,x0,x1,x2;
                          register __m512d rcs,trm1,trm2,trm3;
                          x0  = _mm512_mul_pd(k0,k0);
                          a2  = _mm512_mul_pd(a,a);
                          h2a = _mm512_fmadd_pd(a,_2,h);
                          a3  = _mm512_mul_pd(a2,a);
                          k04 = _mm512_mul_pd(x0,x0);
                          _4a = _mm512_mul_pd(_4,a);
                          trm1= _mm512_mul_pd(_4pi,k04);
                          pia2= _mm512_mul_pd(pi,a2);
                          nh2a= negate_zmm8r8(h2a);
                          x0  = _mm512_mul_pd(c0,_mm512_mul_pd(pia2,h));
                          pia3= _mm512_mul_pd(pi,a3);
                          arg = _mm512_div_pd(nh2a,_4a);
                          x1  = _mm512_mul_pd(c1,pia3);
                          earg= xexpf(arg);
                          x2  = _mm512_add_pd(x0,x1);
                          trm2= _mm512_mul_pd(x2,x2);
                          x0  = _mm512_div_pd(_mm512_mul_pd(pi,h2a),_4a);
                          x1  = _mm512_add_pd(_1, _mm512_div_pd(earg,x0));
                          trm2= _mm512_mul_pd(x1,x1);
                          rcs = _mm512_mul_pd(trm1,_mm512_mul_pd(trm2,trm3));
                          return (rcs);
                }


                   /*
                         Flat-back cone, backscatter RCS.
                         Formula 6.3-9
                   */


                 
                   __m512d rcs_f639_zmm8r8(const __m512d gam0,
                                           const __m512d alp,
                                           const __m512d k0,
                                           const __m512d h) {

                         
                          const __m512d c0 = _mm512_set1_pd(-0.25f);
                          const __m512d _0 = _mm512_set1_pd(-0.0f);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          const __m512d n1 = _mm512_set1_pd(-1.0f);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          register __m512d gam2,talp,k0h,x0,ear,eai,cer,cei;
                          register __m512d t0r,t0i,trm2r,trm2i,trm1r,trm1i,t1r,t1i;
                          gam2  = _mm512_div_pd(_mm512_mul_pd(gam0,gam0),pi);
                          talp  = xtan(alp);
                          k0h   = _mm512_mul_pd(k0,h);
                          x0    = _mm512_mul_pd(talp,talp);
                          ear   = _mm512_setzero_pd();
                          t0r   = n1
                          eai   = _mm512_add_pd(k0h,k0h);
                          t0i   = _mm512_sub_pd(eai,_1);
                          cexp_zmm8r8(eai,ear,&cer,&cei);
                          trm1r = ear;
                          trm1i = negate_zmm8r8(x0);
                          cmul_zmm8r8(t0r,t0i,cer,cei,&trm2r,&trm2i);
                          trm2r = _mm512_add_pd(_1,trm2r);
                          trm2i = _mm512_add_pd(_1,trm2i);
                          cmul_zmm8r8(trm1r,trm1i,trm2r,&t0r,&t0i);
                          cabs = cabs_zmm8r8(t0r,t0i);
                          rcs  = _mm512_mul_pd(gam2,cabs);
                          return (rcs);
                 }


                   
                   __m512d rcs_f639_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pgam0, 
                                             const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) palp,
                                             const float * __restrict __ATTR_ALIGN__(64) ph) {

                         
                          register __m512d gam0= _mm512_load_pd(&pgam0[0]);
                          register __m512d k0  = _mm512_load_pd(&pk0[0]);
                          register __m512d alp = _mm512_load_pd(&palp[0]);
                          register __m512d h   = _mm512_load_pd(&ph[0]);
                          const __m512d c0 = _mm512_set1_pd(-0.25f);
                          const __m512d _0 = _mm512_set1_pd(-0.0f);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          const __m512d n1 = _mm512_set1_pd(-1.0f);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          register __m512d gam2,talp,k0h,x0,ear,eai,cer,cei;
                          register __m512d t0r,t0i,trm2r,trm2i,trm1r,trm1i,t1r,t1i;
                          gam2  = _mm512_div_pd(_mm512_mul_pd(gam0,gam0),pi);
                          talp  = xtan(alp);
                          k0h   = _mm512_mul_pd(k0,h);
                          x0    = _mm512_mul_pd(talp,talp);
                          ear   = _mm512_setzero_pd();
                          t0r   = n1
                          eai   = _mm512_add_pd(k0h,k0h);
                          t0i   = _mm512_sub_pd(eai,_1);
                          cexp_zmm8r8(eai,ear,&cer,&cei);
                          trm1r = ear;
                          trm1i = negate_zmm8r8(x0);
                          cmul_zmm8r8(t0r,t0i,cer,cei,&trm2r,&trm2i);
                          trm2r = _mm512_add_pd(_1,trm2r);
                          trm2i = _mm512_add_pd(_1,trm2i);
                          cmul_zmm8r8(trm1r,trm1i,trm2r,&t0r,&t0i);
                          cabs = cabs_zmm8r8(t0r,t0i);
                          rcs  = _mm512_mul_pd(gam2,cabs);
                          return (rcs);
                 }


                
                   __m512d rcs_f639_zmm8r8_u(const float * __restrict  pgam0, 
                                             const float * __restrict  pk0,
                                             const float * __restrict  palp,
                                             const float * __restrict  ph) {

                         
                          register __m512d gam0= _mm512_loadu_pd(&pgam0[0]);
                          register __m512d k0  = _mm512_loadu_pd(&pk0[0]);
                          register __m512d alp = _mm512_loadu_pd(&palp[0]);
                          register __m512d h   = _mm512_loadu_pd(&ph[0]);
                          const __m512d c0 = _mm512_set1_pd(-0.25f);
                          const __m512d _0 = _mm512_set1_pd(-0.0f);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          const __m512d n1 = _mm512_set1_pd(-1.0f);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          register __m512d gam2,talp,k0h,x0,ear,eai,cer,cei;
                          register __m512d t0r,t0i,trm2r,trm2i,trm1r,trm1i,t1r,t1i;
                          gam2  = _mm512_div_pd(_mm512_mul_pd(gam0,gam0),pi);
                          talp  = xtan(alp);
                          k0h   = _mm512_mul_pd(k0,h);
                          x0    = _mm512_mul_pd(talp,talp);
                          ear   = _mm512_setzero_pd();
                          t0r   = n1
                          eai   = _mm512_add_pd(k0h,k0h);
                          t0i   = _mm512_sub_pd(eai,_1);
                          cexp_zmm8r8(eai,ear,&cer,&cei);
                          trm1r = ear;
                          trm1i = negate_zmm8r8(x0);
                          cmul_zmm8r8(t0r,t0i,cer,cei,&trm2r,&trm2i);
                          trm2r = _mm512_add_pd(_1,trm2r);
                          trm2i = _mm512_add_pd(_1,trm2i);
                          cmul_zmm8r8(trm1r,trm1i,trm2r,&t0r,&t0i);
                          cabs = cabs_zmm8r8(t0r,t0i);
                          rcs  = _mm512_mul_pd(gam2,cabs);
                          return (rcs);
                 }


                   /*
                        Cone tip scattering RCS.
                        Formula 6.3-10
                    */


                
                   __m512d rcs_f6310_zmm8r8(const __m512d gam0,
                                            const __m512d alp) {

                          const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                          register __m512d gam2,tana,x0,tan4,trm1;
                          gam2 = _mm512_mul_pd(gam0,gam0);
                          tana = xtan(alp);
                          trm1 = _mm512_div_pd(gam2,_16pi);
                          x0   = _mm512_mul_pd(tana,tana);
                          tan4 = _mm512_mul_pd(x0,x0);
                          rcs  = _mm512_mul_pd(trm1,tan4);
                          return (rcs);
                 }


                
                   __m512d rcs_f6310_zmm8r8_a(const float * __restrict  __ATTR_ALIGN__(64) pgam0, 
                                              const float * __restrict  __ATTR_ALIGN__(64) palp) {

                          register __m512d gam0= _mm512_load_pd(&pgam0[0]);
                          register __m512d alp = _mm512_load_pd(&palp[0]);
                          const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                          register __m512d gam2,tana,x0,tan4,trm1;
                          gam2 = _mm512_mul_pd(gam0,gam0);
                          tana = xtan(alp);
                          trm1 = _mm512_div_pd(gam2,_16pi);
                          x0   = _mm512_mul_pd(tana,tana);
                          tan4 = _mm512_mul_pd(x0,x0);
                          rcs  = _mm512_mul_pd(trm1,tan4);
                          return (rcs);
                 }


                
                   __m512d rcs_f6310_zmm8r8_u(const float * __restrict  pgam0, 
                                              const float * __restrict  palp) {

                          register __m512d gam0= _mm512_loadu_pd(&pgam0[0]);
                          register __m512d alp = _mm512_loadu_pd(&palp[0]);
                          const __m512d _16pi = _mm512_set1_pd(50.265482457436691815402294132472);
                          register __m512d gam2,tana,x0,tan4,trm1;
                          gam2 = _mm512_mul_pd(gam0,gam0);
                          tana = xtan(alp);
                          trm1 = _mm512_div_pd(gam2,_16pi);
                          x0   = _mm512_mul_pd(tana,tana);
                          tan4 = _mm512_mul_pd(x0,x0);
                          rcs  = _mm512_mul_pd(trm1,tan4);
                          return (rcs);
                 }


                   /*
                         Case of flat-back cone joined by the sphere.
                         Axial-incidence backscatter RCS.
                         Formula 6.3-11
                    */

                   
                 
                   __m512d rcs_f6311_zmm8r8(const __m512d gam0,
                                            const __m512d alp,
                                            const __m512d k0,
                                            const __m512d h,
                                            const __m512d z1) {

                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d c0 = _mm512_set1_pd(0.25f);
                          const __m512d c1 = _mm512_set1_pd(-0.25f);
                          register __m512d gam2,tana,tana2,cosa,seca,k0h,z1h,kz1h,i4r,ir4;
                          register __m512d ear1,eai1,cer1,cei1,ear2,eai2,cer2,cei2;
                          register __m512d t0r,t0i,t1r,t1i,x0,x1,cabs;
                          k0h  = _mm512_mul_pd(k0,h);
                          tana = xtan(alp);
                          z1h  = _mm512_add_pd(z1,h);
                          tana2= _mm512_mul_pd(tana,tana);
                          kz1h = _mm512_mul_pd(k0,z1h);
                          gam2 = _mm512_div_pd(_mm512_mul_pd(gam0,gam0),pi);
                          i4r  = _mm512_setzero_pd();
                          i4i  = c1;
                          cosa = xcos(alp);
                          ear1 = i4r;
                          eai1 = _mm512_add_pd(k0h,k0h);
                          seca = _mm512_mul_pd(cosa,cosa);
                          t0r  = i4r;
                          x0   = _mm512_fmsub_pd(h,tana2,z1);
                          t0i  = _mm512_mul_pd(tana2,i4i);
                          cexp_zmm8r8(ear1,eai1,&cer1,&cei1);
                          x1   = _mm512_sub_pd(x0,seca);
                          t1r  = i4r;
                          ear2 = i4r;
                          t1i  = _mm512_sub_pd(t0i,_mm512_mul_pd(c0,x1));
                          eai2 = _mm512_add_pd(kz1h,kz1h);
                          cexp_zmm8r8(ear2,eai2,&cer2,&cei2);
                          cer2 = i4r;
                          cmul_zmm8r8(t1r,t1i,cer1,cei1,&t0r,&t0i);
                          cei2 = _mm512_mul_pd(cei2,c0);
                          x0   = _mm512_sub_pd(t0r,cer2);
                          x1   = _mm512_sub_pd(t0i,cei2);
                          cabs = cabs_zmm8r8(x0,x1);
                          rcs  = _mm512_mul_pd(gam2,cabs);
                          return (rcs);
                  }


                
                   __m512d rcs_f6311_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pgam0, 
                                             const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) palp,
                                             const float * __restrict __ATTR_ALIGN__(64) ph,
                                             const float * __restrict __ATTR_ALIGN__(64) pz1) {

                          register __m512d gam0= _mm512_load_pd(&pgam0[0]);
                          register __m512d k0  = _mm512_load_pd(&pk0[0]);
                          register __m512d alp = _mm512_load_pd(&palp[0]);
                          register __m512d h   = _mm512_load_pd(&ph[0]);
                          register __m512d z1  = _mm512_load_pd(&pz1[0]);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d c0 = _mm512_set1_pd(0.25f);
                          const __m512d c1 = _mm512_set1_pd(-0.25f);
                          register __m512d gam2,tana,tana2,cosa,seca,k0h,z1h,kz1h,i4r,ir4;
                          register __m512d ear1,eai1,cer1,cei1,ear2,eai2,cer2,cei2;
                          register __m512d t0r,t0i,t1r,t1i,x0,x1,cabs;
                          k0h  = _mm512_mul_pd(k0,h);
                          tana = xtan(alp);
                          z1h  = _mm512_add_pd(z1,h);
                          tana2= _mm512_mul_pd(tana,tana);
                          kz1h = _mm512_mul_pd(k0,z1h);
                          gam2 = _mm512_div_pd(_mm512_mul_pd(gam0,gam0),pi);
                          i4r  = _mm512_setzero_pd();
                          i4i  = c1;
                          cosa = xcos(alp);
                          ear1 = i4r;
                          eai1 = _mm512_add_pd(k0h,k0h);
                          seca = _mm512_mul_pd(cosa,cosa);
                          t0r  = i4r;
                          x0   = _mm512_fmsub_pd(h,tana2,z1);
                          t0i  = _mm512_mul_pd(tana2,i4i);
                          cexp_zmm8r8(ear1,eai1,&cer1,&cei1);
                          x1   = _mm512_sub_pd(x0,seca);
                          t1r  = i4r;
                          ear2 = i4r;
                          t1i  = _mm512_sub_pd(t0i,_mm512_mul_pd(c0,x1));
                          eai2 = _mm512_add_pd(kz1h,kz1h);
                          cexp_zmm8r8(ear2,eai2,&cer2,&cei2);
                          cer2 = i4r;
                          cmul_zmm8r8(t1r,t1i,cer1,cei1,&t0r,&t0i);
                          cei2 = _mm512_mul_pd(cei2,c0);
                          x0   = _mm512_sub_pd(t0r,cer2);
                          x1   = _mm512_sub_pd(t0i,cei2);
                          cabs = cabs_zmm8r8(x0,x1);
                          rcs  = _mm512_mul_pd(gam2,cabs);
                          return (rcs);
                  }


                 
                   __m512d rcs_f6311_zmm8r8_u(const float * __restrict  pgam0, 
                                             const float * __restrict  pk0,
                                             const float * __restrict  palp,
                                             const float * __restrict  ph,
                                             const float * __restrict  pz1) {

                          register __m512d gam0= _mm512_loadu_pd(&pgam0[0]);
                          register __m512d k0  = _mm512_loadu_pd(&pk0[0]);
                          register __m512d alp = _mm512_loadu_pd(&palp[0]);
                          register __m512d h   = _mm512_loadu_pd(&ph[0]);
                          register __m512d z1  = _mm512_loadu_pd(&pz1[0]);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d c0 = _mm512_set1_pd(0.25f);
                          const __m512d c1 = _mm512_set1_pd(-0.25f);
                          register __m512d gam2,tana,tana2,cosa,seca,k0h,z1h,kz1h,i4r,ir4;
                          register __m512d ear1,eai1,cer1,cei1,ear2,eai2,cer2,cei2;
                          register __m512d t0r,t0i,t1r,t1i,x0,x1,cabs;
                          k0h  = _mm512_mul_pd(k0,h);
                          tana = xtan(alp);
                          z1h  = _mm512_add_pd(z1,h);
                          tana2= _mm512_mul_pd(tana,tana);
                          kz1h = _mm512_mul_pd(k0,z1h);
                          gam2 = _mm512_div_pd(_mm512_mul_pd(gam0,gam0),pi);
                          i4r  = _mm512_setzero_pd();
                          i4i  = c1;
                          cosa = xcos(alp);
                          ear1 = i4r;
                          eai1 = _mm512_add_pd(k0h,k0h);
                          seca = _mm512_mul_pd(cosa,cosa);
                          t0r  = i4r;
                          x0   = _mm512_fmsub_pd(h,tana2,z1);
                          t0i  = _mm512_mul_pd(tana2,i4i);
                          cexp_zmm8r8(ear1,eai1,&cer1,&cei1);
                          x1   = _mm512_sub_pd(x0,seca);
                          t1r  = i4r;
                          ear2 = i4r;
                          t1i  = _mm512_sub_pd(t0i,_mm512_mul_pd(c0,x1));
                          eai2 = _mm512_add_pd(kz1h,kz1h);
                          cexp_zmm8r8(ear2,eai2,&cer2,&cei2);
                          cer2 = i4r;
                          cmul_zmm8r8(t1r,t1i,cer1,cei1,&t0r,&t0i);
                          cei2 = _mm512_mul_pd(cei2,c0);
                          x0   = _mm512_sub_pd(t0r,cer2);
                          x1   = _mm512_sub_pd(t0i,cei2);
                          cabs = cabs_zmm8r8(x0,x1);
                          rcs  = _mm512_mul_pd(gam2,cabs);
                          return (rcs);
                  }


                    /*
                          Flat-back cone backscattering RCS.
                          Formula 6.3-14
                     */

                  
                   __m512d rcs_f6314_zmm8r8(const __m512d a,
                                            const __m512d alp) {

                          const __m512d pi3   = _mm512_set1_pd(31.006276680299820175476315067101);
                          const __m512d _4pi2 = _mm512_set1_pd(39.478417604357434475337963999505);
                          const __m512d _3pi  = _mm512_set1_pd(9.424777960769379715387930149839);
                          const __m512d _3pi2 = _mm512_set1_pd(4.712388980384689857693965074919)
                          register __m512d rcs,a2,num1,den1,arg,sarg,csc,x0,alp2;
                          a2   = _mm512_mul_pd(a,a);
                          alp2 = _mm512_add_pd(alp,alp);
                          num1 = _mm512_mul_pd(pi3,a2);
                          arg  = _mm512_div_pd(_4pi2,_mm512_add_pd(_3pi,alp2));
                          x0   = _mm512_add_pd(alp,_3pi2);
                          sarg = xsin(arg);
                          den1 = _mm512_mul_pd(x0,x0);
                          csc  = _mm512_rcp14_pd(sarg);
                          x0   = _mm512_mul_pd(csc,csc);
                          arg  = _mm512_div_pd(num1,den1);
                          rcs  = _mm512_mul_pd(arg,x0);
                          return (rcs);
                }


                
                   __m512d rcs_f6314_zmm8r8_a( const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) palp) {

                          register __m512d a   = _mm512_load_pd(&pa[0]);
                          register __m512d alp = _mm512_load_pd(&palp[0]);
                          const __m512d pi3   = _mm512_set1_pd(31.006276680299820175476315067101);
                          const __m512d _4pi2 = _mm512_set1_pd(39.478417604357434475337963999505);
                          const __m512d _3pi  = _mm512_set1_pd(9.424777960769379715387930149839);
                          const __m512d _3pi2 = _mm512_set1_pd(4.712388980384689857693965074919)
                          register __m512d rcs,a2,num1,den1,arg,sarg,csc,x0,alp2;
                          a2   = _mm512_mul_pd(a,a);
                          alp2 = _mm512_add_pd(alp,alp);
                          num1 = _mm512_mul_pd(pi3,a2);
                          arg  = _mm512_div_pd(_4pi2,_mm512_add_pd(_3pi,alp2));
                          x0   = _mm512_add_pd(alp,_3pi2);
                          sarg = xsin(arg);
                          den1 = _mm512_mul_pd(x0,x0);
                          csc  = _mm512_rcp14_pd(sarg);
                          x0   = _mm512_mul_pd(csc,csc);
                          arg  = _mm512_div_pd(num1,den1);
                          rcs  = _mm512_mul_pd(arg,x0);
                          return (rcs);
                }


                 
                   __m512d rcs_f6314_zmm8r8_u( const float * __restrict  pa,
                                             const float * __restrict  palp) {

                          register __m512d a   = _mm512_loadu_pd(&pa[0]);
                          register __m512d alp = _mm512_loadu_pd(&palp[0]);
                          const __m512d pi3   = _mm512_set1_pd(31.006276680299820175476315067101);
                          const __m512d _4pi2 = _mm512_set1_pd(39.478417604357434475337963999505);
                          const __m512d _3pi  = _mm512_set1_pd(9.424777960769379715387930149839);
                          const __m512d _3pi2 = _mm512_set1_pd(4.712388980384689857693965074919)
                          register __m512d rcs,a2,num1,den1,arg,sarg,csc,x0,alp2;
                          a2   = _mm512_mul_pd(a,a);
                          alp2 = _mm512_add_pd(alp,alp);
                          num1 = _mm512_mul_pd(pi3,a2);
                          arg  = _mm512_div_pd(_4pi2,_mm512_add_pd(_3pi,alp2));
                          x0   = _mm512_add_pd(alp,_3pi2);
                          sarg = xsin(arg);
                          den1 = _mm512_mul_pd(x0,x0);
                          csc  = _mm512_rcp14_pd(sarg);
                          x0   = _mm512_mul_pd(csc,csc);
                          arg  = _mm512_div_pd(num1,den1);
                          rcs  = _mm512_mul_pd(arg,x0);
                          return (rcs);
                }


                   /*
                          Scattering from a thick cylinder of length
                          being a slant height of the cone of radius
                          (4/9*a*sec(alpha)), i.e. RCS(PI/2-alpha).
                          Formula 6.3-18
                      */


                
                   __m512d rcs_f6318_zmm8r8(const __m512d gam0,
                                            const __m512d k0h,
                                            const __m512d alp) {

                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d c0 = _mm512_set1_pd(0.33333333333333333333333333333);
                          register __m512d rcs,gam2,k0h3,x0,salp,calp,seca,sqr,x1,sabs;
                          x0   = _mm512_mul_pd(gam0,gam0);
                          k0h3 = _mm512_mul_pd(k0h,c0);
                          gam2 = _mm512_div_pd(x0,pi);
                          salp = xsin(alp);
                          calp = xcos(alp);
                          x1   = _mm512_mul_pd(k0h,salp);
                          x0   = _mm512_div_pd(x1,pi);
                          seca = _mm512_rcp14_pd(calp);
                          sqr  = _mm512_sqrt_pd(x0);
                          x1   = _mm512_mul_pd(seca,seca);
                          x0   = _mm512_mul_pd(k0h3,_mm512_mul_pd(sqr,x1));
                          sabs = _mm512_abs_pd(x0);
                          x1   = _mm512_mul_pd(sabs,sabs);
                          rcs  = _mm512_mul_pd(gam2,x1);
                          return (rcs);
                 }


                 
                   __m512d rcs_f6318_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pgam0, 
                                             const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                             const float * __restrict __ATTR_ALIGN__(64) palp) {

                          register __m512d gam0 = _mm512_load_pd(&pgam0[0]);
                          register __m512d k0h  = _mm512_load_pd(&pk0[0]);
                          register __m512d alp  = _mm512_load_pd(&palp[0]);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d c0 = _mm512_set1_pd(0.33333333333333333333333333333);
                          register __m512d rcs,gam2,k0h3,x0,salp,calp,seca,sqr,x1,sabs;
                          x0   = _mm512_mul_pd(gam0,gam0);
                          k0h3 = _mm512_mul_pd(k0h,c0);
                          gam2 = _mm512_div_pd(x0,pi);
                          salp = xsin(alp);
                          calp = xcos(alp);
                          x1   = _mm512_mul_pd(k0h,salp);
                          x0   = _mm512_div_pd(x1,pi);
                          seca = _mm512_rcp14_pd(calp);
                          sqr  = _mm512_sqrt_pd(x0);
                          x1   = _mm512_mul_pd(seca,seca);
                          x0   = _mm512_mul_pd(k0h3,_mm512_mul_pd(sqr,x1));
                          sabs = _mm512_abs_pd(x0);
                          x1   = _mm512_mul_pd(sabs,sabs);
                          rcs  = _mm512_mul_pd(gam2,x1);
                          return (rcs);
                 }
                                            



                
                   __m512d rcs_f6318_zmm8r8_u(const float * __restrict  pgam0, 
                                             const float * __restrict  pk0h,
                                             const float * __restrict  palp) {

                          register __m512d gam0 = _mm512_loadu_pd(&pgam0[0]);
                          register __m512d k0h  = _mm512_loadu_pd(&pk0[0]);
                          register __m512d alp  = _mm512_loadu_pd(&palp[0]);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d c0 = _mm512_set1_pd(0.33333333333333333333333333333);
                          register __m512d rcs,gam2,k0h3,x0,salp,calp,seca,sqr,x1,sabs;
                          x0   = _mm512_mul_pd(gam0,gam0);
                          k0h3 = _mm512_mul_pd(k0h,c0);
                          gam2 = _mm512_div_pd(x0,pi);
                          salp = xsin(alp);
                          calp = xcos(alp);
                          x1   = _mm512_mul_pd(k0h,salp);
                          x0   = _mm512_div_pd(x1,pi);
                          seca = _mm512_rcp14_pd(calp);
                          sqr  = _mm512_sqrt_pd(x0);
                          x1   = _mm512_mul_pd(seca,seca);
                          x0   = _mm512_mul_pd(k0h3,_mm512_mul_pd(sqr,x1));
                          sabs = _mm512_abs_pd(x0);
                          x1   = _mm512_mul_pd(sabs,sabs);
                          rcs  = _mm512_mul_pd(gam2,x1);
                          return (rcs);
                 }

#include "GMS_rcs_common_zmm8r8.h"
                 /*
                     Fresnel integral component.
                     Helper kernel for formula 6.3-15
                     Formula 6.3-16
                  */


                 
                   void Frho_f6316_zmm8r8(const __m512d xxa,
                                           const __m512d rho,
                                           __m512d * __restrict ssa,
                                           __m512d * __restrict cca) {

                        const __m512d n1 = _mm512_set1_pd(-1.0f);
                        register __m512d irho,rho2,ear,eai,cer,cei,resr,resi;
                        rho2 = _mm512_mul_pd(rho,rho);
                        ear  = _mm512_setzero_pd();
                        irho = _mm512_rcp14_pd(rho);
                        eai  = _mm512_mul_pd(n1,rho2);
                        cexp_zmm8r8(ear,eai,&cer,&cei);
                        fresnel_zmm8r8(xxa,&resr,&resi);
                        cer = _mm512_mul_pd(cer,irho);
                        cei = _mm512_mul_pd(cei,irho);
                        cmul_zmm8r8(cer,cei,resr,resi,*ssa,*cca);
                }


              
                   void Frho_f6316_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pxxa,
                                             const float * __restrict __ATTR_ALIGN__(64) prho,
                                             float * __restrict __ATTR_ALIGN__(64) ssa,
                                             float * __restrict __ATTR_ALIGN__(64) cca) {

                        register __m512d xxa = _mm512_load_pd(&pxxa[0]);
                        register __m512d rho = _mm512_load_pd(&prho[0]);
                        const __m512d n1 = _mm512_set1_pd(-1.0f);
                        register __m512d irho,rho2,ear,eai,cer,cei,t0r,t0i,resr,resi;
                        rho2 = _mm512_mul_pd(rho,rho);
                        ear  = _mm512_setzero_pd();
                        irho = _mm512_rcp14_pd(rho);
                        eai  = _mm512_mul_pd(n1,rho2);
                        cexp_zmm8r8(ear,eai,&cer,&cei);
                        fresnel_zmm8r8(xxa,&resr,&resi);
                        cer = _mm512_mul_pd(cer,irho);
                        cei = _mm512_mul_pd(cei,irho);
                        cmul_zmm8r8(cer,cei,resr,resi,&t0r,&t0i);
                        _mm512_store_pd(&ssa[0], t0i);
                        _mm512_store_pd(&cca[0], t0r);
                }


                
                   void Frho_f6316_zmm8r8_u(const float * __restrict  pxxa,
                                             const float * __restrict  prho,
                                             float * __restrict __ATTR_ALIGN__(64) ssa,
                                             float * __restrict __ATTR_ALIGN__(64) cca) {

                        register __m512d xxa = _mm512_loadu_pd(&pxxa[0]);
                        register __m512d rho = _mm512_loadu_pd(&prho[0]);
                        const __m512d n1 = _mm512_set1_pd(-1.0f);
                        register __m512d irho,rho2,ear,eai,cer,cei,t0r,t0i,resr,resi;
                        rho2 = _mm512_mul_pd(rho,rho);
                        ear  = _mm512_setzero_pd();
                        irho = _mm512_rcp14_pd(rho);
                        eai  = _mm512_mul_pd(n1,rho2);
                        cexp_zmm8r8(ear,eai,&cer,&cei);
                        fresnel_zmm8r8(xxa,&resr,&resi);
                        cer = _mm512_mul_pd(cer,irho);
                        cei = _mm512_mul_pd(cei,irho);
                        cmul_zmm8r8(cer,cei,resr,resi,&t0r,&t0i);
                        _mm512_storeu_pd(&ssa[0], t0i);
                        _mm512_storeu_pd(&cca[0], t0r);
                }


                   /*
                          Incidence near broadside to the cone - backscatter RCS.
                          Formula 6.3-15
                      */


               
                   __m512d rcs_f6315_zmm8r8(const __m512d rho,
                                            const __m512d gam0,
                                            const __m512d k0h,
                                            const __m512d tht,
                                            const __m512d alp) {

                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d pi4= _mm512_set1_pd(0.78539816339744830961566084582);
                          const __m512d c0 = _mm512_set1_pd(0.25f);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          register __m512d ssa,cca,gam2,ear,eai,cer,cei,thalp,ir,ii,num,den,rat;
                          register __m512d rcs,tana,tanta,costa,xxa,_2k0h,cosa,x0,x1,sth,seca,trm1;
                          register __m512d t0r,t0i,t1r,t1i,cabs;
                          thalp = _mm512_add_pd(tht,alp);
                          cosa   = xcos(alp);
                          ir    = _mm512_setzero_pd();
                          gam2  = _mm512_div_pd(_mm512_mul_pd(gam0,gam0),pi);
                          ii    = pi4;
                          seca  = _mm512_rcp14_pd(cosa);
                          tanta = xtan(thalp);
                          _2k0h = _mm512_add_pd(k0h,k0h);
                          tana  = xtan(alp);
                          costa = xcos(thalp);
                          x0    = _mm512_mul_pd(_2k0h,_mm512_mul_pd(costa,seca));
                          sth   = xsin(tht);
                          ear   = ir;
                          num   = _mm512_mul_pd(k0h,tana);
                          xxa   = _mm512_sqrt_pd(x0);
                          den   = _mm512_mul_pd(pi,sth);
                          Frho_f6316_zmm8r8(rho,xxa,&ssa,&cca);
                          eai   = _mm512_add_pd(ii,x0);
                          trm1  = _mm512_mul_pd(_mm512_sqrt_pd(den),tanta);
                          t0r   = _mm512_sub_pd(_1,ssa);
                          t0i   = _mm512_sub_pd(_1,cca);
                          cexp_zmm8r8(ear,eai,&cer,&cei);
                          cer   = _mm512_mul_pd(cer,c0);
                          cei   = _mm512_mul_pd(cei,c0);
                          x0    = _mm512_mul_pd(trm1,t0r);
                          x1    = _mm512_mul_pd(trm1,t0i);
                          cmul_zmm8r8(cer,cei,t0r,t0i,&t1r,&t1i);
                          cabs  = cabs_zmm8r8(t1r,t1i);
                          rcs   = _mm512_mul_pd(gam2,cabs);
                          return (rcs);
                   }


                  
                   __m512d rcs_f6315_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) prho,
                                              const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              const float * __restrict __ATTR_ALIGN__(64) palp) {

                          register __m512d rho  = _mm512_load_pd(&prho[0]);
                          register __m512d gam0 = _mm512_load_pd(&pgam0[0]);
                          register __m512d k0h  = _mm512_load_pd(&pk0h[0]);
                          register __m512d tht  = _mm512_load_pd(&ptht[0]);
                          register __m512d alp  = _mm512_load_pd(&palp[0]);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d pi4= _mm512_set1_pd(0.78539816339744830961566084582);
                          const __m512d c0 = _mm512_set1_pd(0.25f);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          register __m512d ssa,cca,gam2,ear,eai,cer,cei,thalp,ir,ii,num,den,rat;
                          register __m512d rcs,tana,tanta,costa,xxa,_2k0h,cosa,x0,x1,sth,seca,trm1;
                          register __m512d t0r,t0i,t1r,t1i,cabs;
                          thalp = _mm512_add_pd(tht,alp);
                          cosa   = xcos(alp);
                          ir    = _mm512_setzero_pd();
                          gam2  = _mm512_div_pd(_mm512_mul_pd(gam0,gam0),pi);
                          ii    = pi4;
                          seca  = _mm512_rcp14_pd(cosa);
                          tanta = xtan(thalp);
                          _2k0h = _mm512_add_pd(k0h,k0h);
                          tana  = xtan(alp);
                          costa = xcos(thalp);
                          x0    = _mm512_mul_pd(_2k0h,_mm512_mul_pd(costa,seca));
                          sth   = xsin(tht);
                          ear   = ir;
                          num   = _mm512_mul_pd(k0h,tana);
                          xxa   = _mm512_sqrt_pd(x0);
                          den   = _mm512_mul_pd(pi,sth);
                          Frho_f6316_zmm8r8(rho,xxa,&ssa,&cca);
                          eai   = _mm512_add_pd(ii,x0);
                          trm1  = _mm512_mul_pd(_mm512_sqrt_pd(den),tanta);
                          t0r   = _mm512_sub_pd(_1,ssa);
                          t0i   = _mm512_sub_pd(_1,cca);
                          cexp_zmm8r8(ear,eai,&cer,&cei);
                          cer   = _mm512_mul_pd(cer,c0);
                          cei   = _mm512_mul_pd(cei,c0);
                          x0    = _mm512_mul_pd(trm1,t0r);
                          x1    = _mm512_mul_pd(trm1,t0i);
                          cmul_zmm8r8(cer,cei,t0r,t0i,&t1r,&t1i);
                          cabs  = cabs_zmm8r8(t1r,t1i);
                          rcs   = _mm512_mul_pd(gam2,cabs);
                          return (rcs);
                   }


               
                   __m512d rcs_f6315_zmm8r8_u(const float * __restrict  prho,
                                              const float * __restrict  pgam0,
                                              const float * __restrict  pk0h,
                                              const float * __restrict  ptht,
                                              const float * __restrict  palp) {

                          register __m512d rho  = _mm512_loadu_pd(&prho[0]);
                          register __m512d gam0 = _mm512_loadu_pd(&pgam0[0]);
                          register __m512d k0h  = _mm512_loadu_pd(&pk0h[0]);
                          register __m512d tht  = _mm512_loadu_pd(&ptht[0]);
                          register __m512d alp  = _mm512_loadu_pd(&palp[0]);
                          const __m512d pi = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d pi4= _mm512_set1_pd(0.78539816339744830961566084582);
                          const __m512d c0 = _mm512_set1_pd(0.25f);
                          const __m512d _1 = _mm512_set1_pd(1.0f);
                          register __m512d ssa,cca,gam2,ear,eai,cer,cei,thalp,ir,ii,num,den,rat;
                          register __m512d rcs,tana,tanta,costa,xxa,_2k0h,cosa,x0,x1,sth,seca,trm1;
                          register __m512d t0r,t0i,t1r,t1i,cabs;
                          thalp = _mm512_add_pd(tht,alp);
                          cosa   = xcos(alp);
                          ir    = _mm512_setzero_pd();
                          gam2  = _mm512_div_pd(_mm512_mul_pd(gam0,gam0),pi);
                          ii    = pi4;
                          seca  = _mm512_rcp14_pd(cosa);
                          tanta = xtan(thalp);
                          _2k0h = _mm512_add_pd(k0h,k0h);
                          tana  = xtan(alp);
                          costa = xcos(thalp);
                          x0    = _mm512_mul_pd(_2k0h,_mm512_mul_pd(costa,seca));
                          sth   = xsin(tht);
                          ear   = ir;
                          num   = _mm512_mul_pd(k0h,tana);
                          xxa   = _mm512_sqrt_pd(x0);
                          den   = _mm512_mul_pd(pi,sth);
                          Frho_f6316_zmm8r8(rho,xxa,&ssa,&cca);
                          eai   = _mm512_add_pd(ii,x0);
                          trm1  = _mm512_mul_pd(_mm512_sqrt_pd(den),tanta);
                          t0r   = _mm512_sub_pd(_1,ssa);
                          t0i   = _mm512_sub_pd(_1,cca);
                          cexp_zmm8r8(ear,eai,&cer,&cei);
                          cer   = _mm512_mul_pd(cer,c0);
                          cei   = _mm512_mul_pd(cei,c0);
                          x0    = _mm512_mul_pd(trm1,t0r);
                          x1    = _mm512_mul_pd(trm1,t0i);
                          cmul_zmm8r8(cer,cei,t0r,t0i,&t1r,&t1i);
                          cabs  = cabs_zmm8r8(t1r,t1i);
                          rcs   = _mm512_mul_pd(gam2,cabs);
                          return (rcs);
                   }


                      /*
                          Specular return (RCS) of equivalent cylinder
                          to cone sphere.
                          Formula 6.3-19
                      */


                   __m512d rcs_f6319_zmm8r8(const __m512d k0h,
                                            const __m512d alp,
                                            const __m512d a) {

                          const __m512d c0 = _mm512_set1_pd(0.333333333333333333333333333333333);
                          register __m512d k0h2,a2o3,cosa,seca,trm1,trm2,rcs,x0; 
                          a2o3 = _mm512_mul_pd(_mm512_mul_pd(a,a),c0);
                          cosa = xcos(alp);
                          k0h2 = _mm512_mul_pd(k0h,k0h);
                          seca = _mm512_rcp14_pd(cosa);
                          x0   = _mm512_mul_pd(a2o3,cosa);
                          trm1 = _mm512_mul_pd(k0h2,_mm512_mul_pd(seca,seca));
                          trm2 = _mm512_sqrt_pd(x0);
                          rcs  = _mm512_mul_pd(trm1,trm2);
                          return (rcs);
                 }


                  
                   __m512d rcs_f6319_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0h,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) palp) {

                          register __m512d k0h  = _mm512_load_pd(&pk0h[0]);
                          register __m512d a    = _mm512_load_pd(&pa[0]);
                          register __m512d alp  = _mm512_load_pd(&palp[0]);
                          const __m512d c0 = _mm512_set1_pd(0.333333333333333333333333333333333);
                          register __m512d k0h2,a2o3,cosa,seca,trm1,trm2,rcs,x0; 
                          a2o3 = _mm512_mul_pd(_mm512_mul_pd(a,a),c0);
                          cosa = xcos(alp);
                          k0h2 = _mm512_mul_pd(k0h,k0h);
                          seca = _mm512_rcp14_pd(cosa);
                          x0   = _mm512_mul_pd(a2o3,cosa);
                          trm1 = _mm512_mul_pd(k0h2,_mm512_mul_pd(seca,seca));
                          trm2 = _mm512_sqrt_pd(x0);
                          rcs  = _mm512_mul_pd(trm1,trm2);
                          return (rcs);
                 }


                  
                   __m512d rcs_f6319_zmm8r8_u(const float * __restrict  pk0h,
                                              const float * __restrict  pa,
                                              const float * __restrict  palp) {

                          register __m512d k0h  = _mm512_loadu_pd(&pk0h[0]);
                          register __m512d a    = _mm512_loadu_pd(&pa[0]);
                          register __m512d alp  = _mm512_loadu_pd(&palp[0]);
                          const __m512d c0 = _mm512_set1_pd(0.333333333333333333333333333333333);
                          register __m512d k0h2,a2o3,cosa,seca,trm1,trm2,rcs,x0; 
                          a2o3 = _mm512_mul_pd(_mm512_mul_pd(a,a),c0);
                          cosa = xcos(alp);
                          k0h2 = _mm512_mul_pd(k0h,k0h);
                          seca = _mm512_rcp14_pd(cosa);
                          x0   = _mm512_mul_pd(a2o3,cosa);
                          trm1 = _mm512_mul_pd(k0h2,_mm512_mul_pd(seca,seca));
                          trm2 = _mm512_sqrt_pd(x0);
                          rcs  = _mm512_mul_pd(trm1,trm2);
                          return (rcs);
                 }


                  /*
                          Width of specular lobe of cylinder formula 6.3-19
                          Formula 6.3-20 
                     */

                    
                 
                   __m512d dpsi_f6320_zmm8r8(const __m512d gam0,
                                             const __m512d h,
                                             const __m512d alp) {

                          register __m512d dpsi,cosa,seca;
                          cosa = xcos(alp);
                          seca = _mm512_rcp14_pd(cosa);
                          dpsi = _mm512_div_pd(gam0,_mm512_mul_pd(h,seca));
                          return (dpsi);
                }


                 
                   __m512d dpsi_f6320_zmm8r8_a(const float * __ATTR_ALIGN__(64) pgam0,
                                               const float * __ATTR_ALIGN__(64) ph,
                                               const float * __ATTR_ALIGN__(64) palp) {

                          register __m512d gam0 = _mm512_load_pd(&pgam0[0]);
                          register __m512d h    = _mm512_load_pd(&ph[0]);
                          register __m512d alp  = _mm512_load_pd(&palp[0]);
                          register __m512d dpsi,cosa,seca;
                          cosa = xcos(alp);
                          seca = _mm512_rcp14_pd(cosa);
                          dpsi = _mm512_div_pd(gam0,_mm512_mul_pd(h,seca));
                          return (dpsi);
                }


                
                   __m512d dpsi_f6320_zmm8r8_u(const float *  pgam0,
                                               const float *  ph,
                                               const float *  palp) {

                          register __m512d gam0 = _mm512_loadu_pd(&pgam0[0]);
                          register __m512d h    = _mm512_loadu_pd(&ph[0]);
                          register __m512d alp  = _mm512_loadu_pd(&palp[0]);
                          register __m512d dpsi,cosa,seca;
                          cosa = xcos(alp);
                          seca = _mm512_rcp14_pd(cosa);
                          dpsi = _mm512_div_pd(gam0,_mm512_mul_pd(h,seca));
                          return (dpsi);
                }


                 


                  /*
                          Helper argument K1 for calculation
                          of 6.3-41.
                          Formula 6.3-42
                     */


               
                   __m512d K1_f6342_zmm8r8(const __m512d k0a,
                                           const __m512d tht,
                                           const __m512d alp) {
                          
                          const __m512d _4opi = _mm512_set1_pd(1.27323954473516268615107010698);
                          register __m512d K1,sth,cth,cota,trm1,trm2,calp,salp;
                          sth = xsin(tht);
                          calp= xcos(alp);
                          cth = xcos(tht);
                          salp= xsin(alp);
                          cota= _mm512_div_pd(calp,salp);
                          trm2= _mm512_sub_pd(sth,_mm512_mul_pd(cota,cth));
                          trm1= _mm512_mul_pd(k0a,trm2);
                          K1  = _mm512_mul_pd(_4opi,trm1);
                          return (K1);
                  }


                 
                   __m512d K1_f6342_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0a,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht,
                                             const float * __restrict __ATTR_ALIGN__(64) palp) {
                          
                          register __m512d k0a = _mm512_load_pd(&pk0a[0]);
                          register __m512d tht = _mm512_load_pd(&ptht[0]);
                          register __m512d alp = _mm512_load_pd(&palp[0]);
                          const __m512d _4opi = _mm512_set1_pd(1.27323954473516268615107010698);
                          register __m512d K1,sth,cth,cota,trm1,trm2,calp,salp;
                          sth = xsin(tht);
                          calp= xcos(alp);
                          cth = xcos(tht);
                          salp= xsin(alp);
                          cota= _mm512_div_pd(calp,salp);
                          trm2= _mm512_sub_pd(sth,_mm512_mul_pd(cota,cth));
                          trm1= _mm512_mul_pd(k0a,trm2);
                          K1  = _mm512_mul_pd(_4opi,trm1);
                          return (K1);
                  }


                  
                   __m512d K1_f6342_zmm8r8_u(const float * __restrict  pk0a,
                                             const float * __restrict  ptht,
                                             const float * __restrict  palp) {
                          
                          register __m512d k0a = _mm512_loadu_pd(&pk0a[0]);
                          register __m512d tht = _mm512_loadu_pd(&ptht[0]);
                          register __m512d alp = _mm512_loadu_pd(&palp[0]);
                          const __m512d _4opi = _mm512_set1_pd(1.27323954473516268615107010698);
                          register __m512d K1,sth,cth,cota,trm1,trm2,calp,salp;
                          sth = xsin(tht);
                          calp= xcos(alp);
                          cth = xcos(tht);
                          salp= xsin(alp);
                          cota= _mm512_div_pd(calp,salp);
                          trm2= _mm512_sub_pd(sth,_mm512_mul_pd(cota,cth));
                          trm1= _mm512_mul_pd(k0a,trm2);
                          K1  = _mm512_mul_pd(_4opi,trm1);
                          return (K1);
                  }


                   /*
                          Helper argument K1 for calculation
                          of 6.3-41.
                          Formula 6.3-43
                     */


                 
                   __m512d K2_f6343_zmm8r8(const __m512d k0,
                                           const __m512d a,
                                           const __m512d Ls,
                                           const __m512d tht,
                                           const __m512d alp) {
                          
                          const __m512d _4opi = _mm512_set1_pd(1.27323954473516268615107010698);
                          register __m512d sth,cth,cota,trm1,trm2,calp,salp;
                          register __m512d tana,x0,K2;
                          sth = xsin(tht);
                          calp= xcos(alp);
                          cth = xcos(tht);
                          tana= xtan(alp);
                          salp= xsin(alp);
                          cota= _mm512_div_pd(calp,salp);
                          trm2= _mm512_sub_pd(sth,_mm512_mul_pd(cota,cth));
                          trm1= _mm512_sub_pd(a,_mm512_mul_pd(Ls,tana));
                          x0  = _mm512_mul_pd(k0,trm1);
                          K2  = _mm512_mul_pd(_4opi,_mm512_mul_pd(trm1,trm2));
                          return (K2);
                  }


                 
                   __m512d K2_f6343_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) pLs,
                                             const float * __restrict __ATTR_ALIGN__(64) ptht,
                                             const float * __restrict __ATTR_ALIGN__(64) palp) {
                          
                          register __m512d k0a = _mm512_load_pd(&pk0a[0]);
                          register __m512d a   = _mm512_load_pd(&pa[0]);
                          register __m512d Ls  = _mm512_load_pd(&pLs[0]);
                          register __m512d tht = _mm512_load_pd(&ptht[0]);
                          register __m512d alp = _mm512_load_pd(&palp[0]);
                          const __m512d _4opi = _mm512_set1_pd(1.27323954473516268615107010698);
                          register __m512d sth,cth,cota,trm1,trm2,calp,salp;
                          register __m512d tana,x0,K2;
                          sth = xsin(tht);
                          calp= xcos(alp);
                          cth = xcos(tht);
                          tana= xtan(alp);
                          salp= xsin(alp);
                          cota= _mm512_div_pd(calp,salp);
                          trm2= _mm512_sub_pd(sth,_mm512_mul_pd(cota,cth));
                          trm1= _mm512_sub_pd(a,_mm512_mul_pd(Ls,tana));
                          x0  = _mm512_mul_pd(k0,trm1);
                          K2  = _mm512_mul_pd(_4opi,_mm512_mul_pd(trm1,trm2));
                          return (K2);
                  }


                  
                   __m512d K2_f6343_zmm8r8_u(const float * __restrict  pk0,
                                             const float * __restrict  pa,
                                             const float * __restrict  pLs,
                                             const float * __restrict  ptht,
                                             const float * __restrict  palp) {
                          
                          register __m512d k0a = _mm512_loadu_pd(&pk0a[0]);
                          register __m512d a   = _mm512_loadu_pd(&pa[0]);
                          register __m512d Ls  = _mm512_loadu_pd(&pLs[0]);
                          register __m512d tht = _mm512_loadu_pd(&ptht[0]);
                          register __m512d alp = _mm512_loadu_pd(&palp[0]);
                          const __m512d _4opi = _mm512_set1_pd(1.27323954473516268615107010698);
                          register __m512d sth,cth,cota,trm1,trm2,calp,salp;
                          register __m512d tana,x0,K2;
                          sth = xsin(tht);
                          calp= xcos(alp);
                          cth = xcos(tht);
                          tana= xtan(alp);
                          salp= xsin(alp);
                          cota= _mm512_div_pd(calp,salp);
                          trm2= _mm512_sub_pd(sth,_mm512_mul_pd(cota,cth));
                          trm1= _mm512_sub_pd(a,_mm512_mul_pd(Ls,tana));
                          x0  = _mm512_mul_pd(k0,trm1);
                          K2  = _mm512_mul_pd(_4opi,_mm512_mul_pd(trm1,trm2));
                          return (K2);
                  }


                    /*
                          Cylindrical current approximation of
                          flat based truncated cone RCS for
                          perpendicular polarization.
                          Formula 6.3-41 
                    */


                  
                   __m512d rcs_f6341_zmm8r8(const __m512d gam0,
                                            const __m512d tht,
                                            const __m512d alp,
                                            const __m512d Ls,
                                            const __m512d k0,
                                            const __m512d a,
                                            const __m512d a1) {

                          const __m512d pi  = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d pi2 = _mm512_set1_pd(-1.57079632679489661923132169164);
                          const __m512d _16 = _mm512_set1_pd(16.0f);
                          const __m512d hlf = _mm512_set1_pd(0.5f);
                          const __m512d n1  = _mm512_set1_pd(-1.0f);
                          const __m512d pi2a= _mm512_sub_pd(pi2,alp);
                          register __m512d sth,cosa,cosa2,sina,k02,k0a;
                          register __m512d thta,cthta,cthta3,num,den,trm1;
                          register __m512d K1,K2,CK1,SK1,CK2,SK2,x0,x1;
                          register __m512d eai,ear,cer1,cei1,cer2,cei2,t0r,t0i;
                          register __m512d rcs,cabs;

                          if(_mm512_cmp_pd_mask(tht,pi2a,_CMP_EQ_OQ)) {
                              const __m512d _4o9 = _mm512_set1_pd(0.444444444444444444444444444444);
                              register __m512d a32,a132,sqr,cota,cotas,diff;
                              x0   = _mm512_mul_pd(_4o9,k0);
                              cosa = xcos(alp);
                              a32  = _mm512_pow_pd(a,_mm512_set1_pd(1.5f);
                              sina = xsin(alp);
                              a321 = _mm512_pow_pd(a1,_mm512_set1_pd(1.5f);
                              cota = _mm512_div_pd(cosa,sina);
                              diff = _mm512_sub_pd(a32,a321);
                              cotas= _mm512_mul_pd(cota,cota);
                              x0   = _mm512_mul_pd(x0,cosa);
                              x1   = _mm512_mul_pd(cotas,_mm512_mul_pd(diff,diff));
                              rcs  = _mm512_mul_pd(x0,x1);
                              return (rcs);
                          }
                          else if(_mm512_cmp_pd_mask(a1,
                                            _mm512_setzero_pd(),_CMP_EQ_OQ)) {
                                  const __m512d _8pi = _mm512_set1_pd(25.132741228718345907701147066236);
                                  const __m512d _9   = _mm512_set1_pd(9.0f);
                                  register __m512d rat,a3,num,den,cosa,sina,cota,cotas;
                                  a3   = _mm512_mul_pd(a,_mm512_mul_pd(a,a))'
                                  cosa = xcos(alp);
                                  sina = xsin(alp);
                                  num  = _mm512_mul_pd(_8pi,a3);
                                  cota = _mm512_div_pd(cosa,sina); 
                                  den  = _mm512_mul_pd(_9,gam0);
                                  cotas= _mm512_mul_pd(cota,cota);
                                  rat  = _mm512_div_pd(num,den);
                                  rcs  = _mm512_mul_pd(rat,_mm512_mul_pd(cosa,cotas));
                                  return (rcs);
                          }

                          k02  = _mm512_mul_pd(_16,_mm512_mul_pd(k0,k0));
                          k0a  = _mm512_mul_pd(k0,a);
                          sth  = xsin(tht);
                          thta = _mm512_add_pd(tht,a);
                          sina = xsin(alp);
                          ear  = _mm512_setzero_pd();
                          cosa = xcos(tht);
                          eai  = pi2;
                          x0   = _mm512_mul_pd(pi,sina);
                          cthta= xcos(thta);
                          x1   = _mm512_mul_pd(cosa,cosa);
                          K1   = K1_f6342_zmm8r8(k0a,tht,alp);
                          num  = _mm512_mul_pd(x0,_mm512_mul_pd(x1,sina));
                          K2   = K2_f6343_zmm8r8(K0,a,Ls,tht,alp);
                          x0   = _mm512_mul_pd(cthta,_mm512_mul_pd(cthta,cthta));
                          den  = _mm512_mul_pd(k02,x0);
                          fresnel_zmm8r8(K1,&CK1,&SK1);
                          trm1 = _mm512_div_pd(num,den);
                          eai  = _mm512_mul_pd(eai,_mm512_mul_pd(K1,K1));
                          cexp_zmm8r8(ear,eai,&cer1,&cei1);
                          fresnel_zmm8r8(K2,&CK2,&SK2);
                          cer1 = _mm512_mul_pd(cer1,K1);
                          cei1 = _mm512_mul_pd(cei1,K1);
                          eai  = _mm512_mul_pd(pi2,_mm512_mul_pd(K2,K2));
                          cexp_zmm8r8(ear,eai,&cer2,&cei2);
                          cer2 = _mm512_mul_pd(cer2,K2);
                          cei2 = _mm512_mul_pd(cei2,K2);
                          t0r  = _mm512_sub_pd(cer1,cer2);
                          t0i  = _mm512_sub_pd(cei1,cei2);
                          x0   = _mm512_add_pd(CK1,CK2);
                          x1   = _mm512_add_pd(SK1,SK2);
                          ear  = _mm512_sub_pd(t0r,x0);
                          eai  = _mm512_sub_pd(t0i,x1);
                          cabs = cabs_zmm8r8(ear,eai);
                          rcs  = _mm512_mul_pd(trm1,cabs);
                          return (rcs);
                 }


                 
                   __m512d rcs_f6341_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              const float * __restrict __ATTR_ALIGN__(64) palp,
                                              const float * __restrict __ATTR_ALIGN__(64) pLs,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pa1) {

                          register __m512d gam0= _mm512_load_pd(&pgam0[0]);
                          register __m512d tht = _mm512_load_pd(&ptht[0]);
                          register __m512d alp = _mm512_load_pd(&palp[0]);
                          register __m512d Ls  = _mm512_load_pd(&pLs[0]);
                          register __m512d k0  = _mm512_load_pd(&pk0[0]);
                          register __m512d a   = _mm512_load_pd(&pa[0]);
                          register __m512d a   = _mm512_load_pd(&pa1[0]);
                          const __m512d pi  = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d pi2 = _mm512_set1_pd(-1.57079632679489661923132169164);
                          const __m512d _16 = _mm512_set1_pd(16.0f);
                          const __m512d hlf = _mm512_set1_pd(0.5f);
                          const __m512d n1  = _mm512_set1_pd(-1.0f);
                          register __m512d sth,cosa,cosa2,sina,k02,k0a;
                          register __m512d thta,cthta,cthta3,num,den,trm1;
                          register __m512d K1,K2,CK1,SK1,CK2,SK2,x0,x1;
                          register __m512d eai,ear,cer1,cei1,cer2,cei2,t0r,t0i;
                          register __m512d rcs,cabs;

                          if(_mm512_cmp_pd_mask(tht,pi2a,_CMP_EQ_OQ)) {
                              const __m512d _4o9 = _mm512_set1_pd(0.444444444444444444444444444444);
                              register __m512d a32,a132,sqr,cota,cotas,diff;
                              x0   = _mm512_mul_pd(_4o9,k0);
                              cosa = xcos(alp);
                              a32  = _mm512_pow_pd(a,_mm512_set1_pd(1.5f);
                              sina = xsin(alp);
                              a321 = _mm512_pow_pd(a1,_mm512_set1_pd(1.5f);
                              cota = _mm512_div_pd(cosa,sina);
                              diff = _mm512_sub_pd(a32,a321);
                              cotas= _mm512_mul_pd(cota,cota);
                              x0   = _mm512_mul_pd(x0,cosa);
                              x1   = _mm512_mul_pd(cotas,_mm512_mul_pd(diff,diff));
                              rcs  = _mm512_mul_pd(x0,x1);
                              return (rcs);
                          } 
                          else if(_mm512_cmp_pd_mask(a1,
                                            _mm512_setzero_pd(),_CMP_EQ_OQ)) {
                                  const __m512d _8pi = _mm512_set1_pd(25.132741228718345907701147066236);
                                  const __m512d _9   = _mm512_set1_pd(9.0f);
                                  register __m512d rat,a3,num,den,cosa,sina,cota,cotas;
                                  a3   = _mm512_mul_pd(a,_mm512_mul_pd(a,a))'
                                  cosa = xcos(alp);
                                  sina = xsin(alp);
                                  num  = _mm512_mul_pd(_8pi,a3);
                                  cota = _mm512_div_pd(cosa,sina); 
                                  den  = _mm512_mul_pd(_9,gam0);
                                  cotas= _mm512_mul_pd(cota,cota);
                                  rat  = _mm512_div_pd(num,den);
                                  rcs  = _mm512_mul_pd(rat,_mm512_mul_pd(cosa,cotas));
                                  return (rcs);
                          }

                          k02  = _mm512_mul_pd(_16,_mm512_mul_pd(k0,k0));
                          k0a  = _mm512_mul_pd(k0,a);
                          sth  = xsin(tht);
                          thta = _mm512_add_pd(tht,a);
                          sina = xsin(alp);
                          ear  = _mm512_setzero_pd();
                          cosa = xcos(tht);
                          eai  = pi2;
                          x0   = _mm512_mul_pd(pi,sina);
                          cthta= xcos(thta);
                          x1   = _mm512_mul_pd(cosa,cosa);
                          K1   = K1_f6342_zmm8r8(k0a,tht,alp);
                          num  = _mm512_mul_pd(x0,_mm512_mul_pd(x1,sina));
                          K2   = K2_f6343_zmm8r8(K0,a,Ls,tht,alp);
                          x0   = _mm512_mul_pd(cthta,_mm512_mul_pd(cthta,cthta));
                          den  = _mm512_mul_pd(k02,x0);
                          fresnel_zmm8r8(K1,&CK1,&SK1);
                          trm1 = _mm512_div_pd(num,den);
                          eai  = _mm512_mul_pd(eai,_mm512_mul_pd(K1,K1));
                          cexp_zmm8r8(ear,eai,&cer1,&cei1);
                          fresnel_zmm8r8(K2,&CK2,&SK2);
                          cer1 = _mm512_mul_pd(cer1,K1);
                          cei1 = _mm512_mul_pd(cei1,K1);
                          eai  = _mm512_mul_pd(pi2,_mm512_mul_pd(K2,K2));
                          cexp_zmm8r8(ear,eai,&cer2,&cei2);
                          cer2 = _mm512_mul_pd(cer2,K2);
                          cei2 = _mm512_mul_pd(cei2,K2);
                          t0r  = _mm512_sub_pd(cer1,cer2);
                          t0i  = _mm512_sub_pd(cei1,cei2);
                          x0   = _mm512_add_pd(CK1,CK2);
                          x1   = _mm512_add_pd(SK1,SK2);
                          ear  = _mm512_sub_pd(t0r,x0);
                          eai  = _mm512_sub_pd(t0i,x1);
                          cabs = cabs_zmm8r8(ear,eai);
                          rcs  = _mm512_mul_pd(trm1,cabs);
                          return (rcs);
                 }


                  
                   __m512d rcs_f6341_zmm8r8_u(const float * __restrict  pgam0,
                                              const float * __restrict  ptht,
                                              const float * __restrict  palp,
                                              const float * __restrict  pLs,
                                              const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  pa1) {

                          register __m512d gam0= _mm512_loadu_pd(&pgam0[0]); 
                          register __m512d tht = _mm512_loadu_pd(&ptht[0]);
                          register __m512d alp = _mm512_loadu_pd(&palp[0]);
                          register __m512d Ls  = _mm512_loadu_pd(&pLs[0]);
                          register __m512d k0  = _mm512_loadu_pd(&pk0[0]);
                          register __m512d a   = _mm512_loadu_pd(&pa[0]);
                          register __m512d a1  = _mm512_loadu_pd(&pa1[0]);
                          const __m512d pi  = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d pi2 = _mm512_set1_pd(-1.57079632679489661923132169164);
                          const __m512d _16 = _mm512_set1_pd(16.0f);
                          const __m512d hlf = _mm512_set1_pd(0.5f);
                          const __m512d n1  = _mm512_set1_pd(-1.0f);
                          register __m512d sth,cosa,cosa2,sina,k02,k0a;
                          register __m512d thta,cthta,cthta3,num,den,trm1;
                          register __m512d K1,K2,CK1,SK1,CK2,SK2,x0,x1;
                          register __m512d eai,ear,cer1,cei1,cer2,cei2,t0r,t0i;
                          register __m512d rcs,cabs;

                          if(_mm512_cmp_pd_mask(tht,pi2a,_CMP_EQ_OQ)) {
                              const __m512d _4o9 = _mm512_set1_pd(0.444444444444444444444444444444);
                              register __m512d a32,a132,sqr,cota,cotas,diff;
                              x0   = _mm512_mul_pd(_4o9,k0);
                              cosa = xcos(alp);
                              a32  = _mm512_pow_pd(a,_mm512_set1_pd(1.5f);
                              sina = xsin(alp);
                              a321 = _mm512_pow_pd(a1,_mm512_set1_pd(1.5f);
                              cota = _mm512_div_pd(cosa,sina);
                              diff = _mm512_sub_pd(a32,a321);
                              cotas= _mm512_mul_pd(cota,cota);
                              x0   = _mm512_mul_pd(x0,cosa);
                              x1   = _mm512_mul_pd(cotas,_mm512_mul_pd(diff,diff));
                              rcs  = _mm512_mul_pd(x0,x1);
                              return (rcs);
                          } 
                          else if(_mm512_cmp_pd_mask(a1,
                                            _mm512_setzero_pd(),_CMP_EQ_OQ)) {
                                  const __m512d _8pi = _mm512_set1_pd(25.132741228718345907701147066236);
                                  const __m512d _9   = _mm512_set1_pd(9.0f);
                                  register __m512d rat,a3,num,den,cosa,sina,cota,cotas;
                                  a3   = _mm512_mul_pd(a,_mm512_mul_pd(a,a))'
                                  cosa = xcos(alp);
                                  sina = xsin(alp);
                                  num  = _mm512_mul_pd(_8pi,a3);
                                  cota = _mm512_div_pd(cosa,sina); 
                                  den  = _mm512_mul_pd(_9,gam0);
                                  cotas= _mm512_mul_pd(cota,cota);
                                  rat  = _mm512_div_pd(num,den);
                                  rcs  = _mm512_mul_pd(rat,_mm512_mul_pd(cosa,cotas));
                                  return (rcs);
                          }

                          k02  = _mm512_mul_pd(_16,_mm512_mul_pd(k0,k0));
                          k0a  = _mm512_mul_pd(k0,a);
                          sth  = xsin(tht);
                          thta = _mm512_add_pd(tht,a);
                          sina = xsin(alp);
                          ear  = _mm512_setzero_pd();
                          cosa = xcos(tht);
                          eai  = pi2;
                          x0   = _mm512_mul_pd(pi,sina);
                          cthta= xcos(thta);
                          x1   = _mm512_mul_pd(cosa,cosa);
                          K1   = K1_f6342_zmm8r8(k0a,tht,alp);
                          num  = _mm512_mul_pd(x0,_mm512_mul_pd(x1,sina));
                          K2   = K2_f6343_zmm8r8(K0,a,Ls,tht,alp);
                          x0   = _mm512_mul_pd(cthta,_mm512_mul_pd(cthta,cthta));
                          den  = _mm512_mul_pd(k02,x0);
                          fresnel_zmm8r8(K1,&CK1,&SK1);
                          trm1 = _mm512_div_pd(num,den);
                          eai  = _mm512_mul_pd(eai,_mm512_mul_pd(K1,K1));
                          cexp_zmm8r8(ear,eai,&cer1,&cei1);
                          fresnel_zmm8r8(K2,&CK2,&SK2);
                          cer1 = _mm512_mul_pd(cer1,K1);
                          cei1 = _mm512_mul_pd(cei1,K1);
                          eai  = _mm512_mul_pd(pi2,_mm512_mul_pd(K2,K2));
                          cexp_zmm8r8(ear,eai,&cer2,&cei2);
                          cer2 = _mm512_mul_pd(cer2,K2);
                          cei2 = _mm512_mul_pd(cei2,K2);
                          t0r  = _mm512_sub_pd(cer1,cer2);
                          t0i  = _mm512_sub_pd(cei1,cei2);
                          x0   = _mm512_add_pd(CK1,CK2);
                          x1   = _mm512_add_pd(SK1,SK2);
                          ear  = _mm512_sub_pd(t0r,x0);
                          eai  = _mm512_sub_pd(t0i,x1);
                          cabs = cabs_zmm8r8(ear,eai);
                          rcs  = _mm512_mul_pd(trm1,cabs);
                          return (rcs);
                 }


                   /*
                          Cylindrical current approximation of
                          flat based pointed cone RCS for
                          perpendicular polarization.
                          Formula 6.3-44
                     */


                
                   __m512d rcs_f6344_zmm8r8(const __m512d gam0,
                                            const __m512d a,
                                            const __m512d tht,
                                            const __m512d alp,
                                            const __m512d k0) {

                          const __m512d pi2  = _mm512_set1_pd(1.57079632679489661923132169164);
                          const __m512d _4opi= _mm512_set1_pd(1.27323954473516268615107010698)
                          const __m512d _1   = _mm512_set1_pd(1.0f);
                          const __m512d _2   = _mm512_set1_pd(2.0f);
                          const __m512d _8pi = _mm512_set1_pd(25.132741228718345907701147066236);
                          register __m512d gama,sint,tana,cost,sina1,cosa1,cott,cota1,den,x0,x1;
                          register __m512d C2,S2,rat,arg,carg,sarg,num,trm1,trm2,ksi,ksi2,x2;
                          k0a   = _mm512_mul_pd(k0,a);
                          x0    = _mm512_sub_pd(alp,_1);
                          sint  = xsin(tht);
                          gama  = _mm512_mul_pd(gam0,a);
                          cost  = xcos(tht);
                          cott  = _mm512_div_pd(cost,sint);
                          tana  = xtan(alp);
                          sina1 = xsin(x0);
                          cosa1 = xcos(x0);
                          cota1 = _mm512_div_pd(cosa1,sina1);
                          x2    = _mm512_mul_pd(cott,cota1);
                          x1    = _mm512_mul_pd(k0a,_4opi);
                          ksi2  = _mm512_mul_pd(x1,_mm512_mul_pd(sint,x2));
                          arg   = _mm512_mul_pd(pi2,ksi2);
                          ksi   = _mm512_sqrt_pd(ksi2);
                          fresnel_zmm8r8(ksi,&C2,&S2);
                          carg  = xcos(arg);
                          sarg  = xsin(arg);
                          x0    = _mm512_mul_pd(tana,tana);
                          x1    = _mm512_mul_pd(_8pi,sint);
                          den   = _mm512_mul_pd(_mm512_mul_pd(x0,x1),x2);
                          trm1  = _mm512_div_pd(_mm512_fmadd_pd(C2,C2,
                                                            _mm512_mul_pd(S2,S2)),ksi2);
                          trm1  = _mm512_add_pd(_1,trm1);
                          x1    = _mm512_fmadd_pd(C2,carg,_mm512_mul_pd(S2,sarg));
                          x0    = _mm512_div_pd(_2,ksi);
                          num   = _mm512_sub_pd(trm1,_mm512_mul_pd(x0,x1));
                          rat   = _mm512_div_pd(num,den);
                          rcs   = _mm512_mul_pd(gama,rat);
                          return (rcs);
                }


                 
                   __m512d rcs_f6344_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pgam0,
                                            const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) ptht,
                                            const float * __restrict __ATTR_ALIGN__(64) palp,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0) {

                          register __m512d gam0 = _mm512_load_pd(&pgam0[0]);
                          register __m512d a    = _mm512_load_pd(&pa[0]);
                          register __m512d tht  = _mm512_load_pd(&ptht[0]);
                          register __m512d alp  = _mm512_load_pd(&palp[0]);
                          register __m512d k0   = _mm512_load_pd(&pk0[0]);
                          const __m512d pi2  = _mm512_set1_pd(1.57079632679489661923132169164);
                          const __m512d _4opi= _mm512_set1_pd(1.27323954473516268615107010698)
                          const __m512d _1   = _mm512_set1_pd(1.0f);
                          const __m512d _2   = _mm512_set1_pd(2.0f);
                          const __m512d _8pi = _mm512_set1_pd(25.132741228718345907701147066236);
                          register __m512d gama,sint,tana,cost,sina1,cosa1,cott,cota1,den,x0,x1;
                          register __m512d C2,S2,rat,arg,carg,sarg,num,trm1,trm2,ksi,ksi2,x2;
                          k0a   = _mm512_mul_pd(k0,a);
                          x0    = _mm512_sub_pd(alp,_1);
                          sint  = xsin(tht);
                          gama  = _mm512_mul_pd(gam0,a);
                          cost  = xcos(tht);
                          cott  = _mm512_div_pd(cost,sint);
                          tana  = xtan(alp);
                          sina1 = xsin(x0);
                          cosa1 = xcos(x0);
                          cota1 = _mm512_div_pd(cosa1,sina1);
                          x2    = _mm512_mul_pd(cott,cota1);
                          x1    = _mm512_mul_pd(k0a,_4opi);
                          ksi2  = _mm512_mul_pd(x1,_mm512_mul_pd(sint,x2));
                          arg   = _mm512_mul_pd(pi2,ksi2);
                          ksi   = _mm512_sqrt_pd(ksi2);
                          fresnel_zmm8r8(ksi,&C2,&S2);
                          carg  = xcos(arg);
                          sarg  = xsin(arg);
                          x0    = _mm512_mul_pd(tana,tana);
                          x1    = _mm512_mul_pd(_8pi,sint);
                          den   = _mm512_mul_pd(_mm512_mul_pd(x0,x1),x2);
                          trm1  = _mm512_div_pd(_mm512_fmadd_pd(C2,C2,
                                                            _mm512_mul_pd(S2,S2)),ksi2);
                          trm1  = _mm512_add_pd(_1,trm1);
                          x1    = _mm512_fmadd_pd(C2,carg,_mm512_mul_pd(S2,sarg));
                          x0    = _mm512_div_pd(_2,ksi);
                          num   = _mm512_sub_pd(trm1,_mm512_mul_pd(x0,x1));
                          rat   = _mm512_div_pd(num,den);
                          rcs   = _mm512_mul_pd(gama,rat);
                          return (rcs);
                }


                 
                   __m512d rcs_f6344_zmm8r8_u(const float * __restrict  pgam0,
                                            const float * __restrict  pa,
                                            const float * __restrict  ptht,
                                            const float * __restrict  palp,
                                            const float * __restrict  pk0) {

                          register __m512d gam0 = _mm512_loadu_pd(&pgam0[0]);
                          register __m512d a    = _mm512_loadu_pd(&pa[0]);
                          register __m512d tht  = _mm512_loadu_pd(&ptht[0]);
                          register __m512d alp  = _mm512_loadu_pd(&palp[0]);
                          register __m512d k0   = _mm512_loadu_pd(&pk0[0]);
                          const __m512d pi2  = _mm512_set1_pd(1.57079632679489661923132169164);
                          const __m512d _4opi= _mm512_set1_pd(1.27323954473516268615107010698)
                          const __m512d _1   = _mm512_set1_pd(1.0f);
                          const __m512d _2   = _mm512_set1_pd(2.0f);
                          const __m512d _8pi = _mm512_set1_pd(25.132741228718345907701147066236);
                          register __m512d gama,sint,tana,cost,sina1,cosa1,cott,cota1,den,x0,x1;
                          register __m512d C2,S2,rat,arg,carg,sarg,num,trm1,trm2,ksi,ksi2,x2;
                          k0a   = _mm512_mul_pd(k0,a);
                          x0    = _mm512_sub_pd(alp,_1);
                          sint  = xsin(tht);
                          gama  = _mm512_mul_pd(gam0,a);
                          cost  = xcos(tht);
                          cott  = _mm512_div_pd(cost,sint);
                          tana  = xtan(alp);
                          sina1 = xsin(x0);
                          cosa1 = xcos(x0);
                          cota1 = _mm512_div_pd(cosa1,sina1);
                          x2    = _mm512_mul_pd(cott,cota1);
                          x1    = _mm512_mul_pd(k0a,_4opi);
                          ksi2  = _mm512_mul_pd(x1,_mm512_mul_pd(sint,x2));
                          arg   = _mm512_mul_pd(pi2,ksi2);
                          ksi   = _mm512_sqrt_pd(ksi2);
                          fresnel_zmm8r8(ksi,&C2,&S2);
                          carg  = xcos(arg);
                          sarg  = xsin(arg);
                          x0    = _mm512_mul_pd(tana,tana);
                          x1    = _mm512_mul_pd(_8pi,sint);
                          den   = _mm512_mul_pd(_mm512_mul_pd(x0,x1),x2);
                          trm1  = _mm512_div_pd(_mm512_fmadd_pd(C2,C2,
                                                            _mm512_mul_pd(S2,S2)),ksi2);
                          trm1  = _mm512_add_pd(_1,trm1);
                          x1    = _mm512_fmadd_pd(C2,carg,_mm512_mul_pd(S2,sarg));
                          x0    = _mm512_div_pd(_2,ksi);
                          num   = _mm512_sub_pd(trm1,_mm512_mul_pd(x0,x1));
                          rat   = _mm512_div_pd(num,den);
                          rcs   = _mm512_mul_pd(gama,rat);
                          return (rcs);
                }


                   /*
                        Geometrical Diffraction.
                        Flat-backed cone, backscattered RCS.
                        Simplified by neglecting a second order terms of 6.3-49
                        Formula 6.3-50
                    */


                 
                   __m512d rcs_f6350_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d alp) {

                          const __m512d ipi = _mm512_set1_pd(0.318309886183790671537767526745);
                          const __m512d pi  = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d _3pi= _mm512_set1_pd(9.424777960769379715387930149839);
                          register __m512d k0a,n,arg,sarg,carg,arg2,carg2;
                          register __m512d rcs,x0,x1,inv2;
                          k0a  = _mm512_mul_pd(k0,a);
                          n    = _mm512_add_pd(_mm512_set1_pd(1.5f),
                                               _mm512_div_pd(alp,pi));
                          arg  = _mm512_div_pd(pi,n);
                          sarg = xsin(arg);
                          x0   = _mm512_div_pd(_mm512_mul_pd(k0a,sarg),n);
                          carg = xcos(arg);
                          arg2 = _mm512_div_pd(_3pi,n);
                          x1   = _mm512_mul_pd(x0,x0);
                          carg2= xcos(arg2);
                          x0   = _mm512_sub_pd(carg,carg2);
                          inv2 = _mm512_rcp14_pd(_mm512_mul_pd(x0,x0));
                          rcs  = _mm512_mul_pd(ipi,_mm512_mul_pd(x0,inv2));
                          return (rcs);
                 }


                 
                   __m512d rcs_f6350_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) palp) {

                          register __m512d k0  = _mm512_load_pd(&pk0[0]);
                          register __m512d a   = _mm512_load_pd(&pa[0]);
                          register __m512d alp = _mm512_load_pd(&palp[0]);
                          const __m512d ipi = _mm512_set1_pd(0.318309886183790671537767526745);
                          const __m512d pi  = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d _3pi= _mm512_set1_pd(9.424777960769379715387930149839);
                          register __m512d k0a,n,arg,sarg,carg,arg2,carg2;
                          register __m512d rcs,x0,x1,inv2;
                          k0a  = _mm512_mul_pd(k0,a);
                          n    = _mm512_add_pd(_mm512_set1_pd(1.5f),
                                               _mm512_div_pd(alp,pi));
                          arg  = _mm512_div_pd(pi,n);
                          sarg = xsin(arg);
                          x0   = _mm512_div_pd(_mm512_mul_pd(k0a,sarg),n);
                          carg = xcos(arg);
                          arg2 = _mm512_div_pd(_3pi,n);
                          x1   = _mm512_mul_pd(x0,x0);
                          carg2= xcos(arg2);
                          x0   = _mm512_sub_pd(carg,carg2);
                          inv2 = _mm512_rcp14_pd(_mm512_mul_pd(x0,x0));
                          rcs  = _mm512_mul_pd(ipi,_mm512_mul_pd(x0,inv2));
                          return (rcs);
                 }


                   
                   __m512d rcs_f6350_zmm8r8_u(const float * __restrict  pk0,
                                              const float * __restrict  pa,
                                              const float * __restrict  palp) {

                          register __m512d k0  = _mm512_loadu_pd(&pk0[0]);
                          register __m512d a   = _mm512_loadu_pd(&pa[0]);
                          register __m512d alp = _mm512_loadu_pd(&palp[0]);
                          const __m512d ipi = _mm512_set1_pd(0.318309886183790671537767526745);
                          const __m512d pi  = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d _3pi= _mm512_set1_pd(9.424777960769379715387930149839);
                          register __m512d k0a,n,arg,sarg,carg,arg2,carg2;
                          register __m512d rcs,x0,x1,inv2;
                          k0a  = _mm512_mul_pd(k0,a);
                          n    = _mm512_add_pd(_mm512_set1_pd(1.5f),
                                               _mm512_div_pd(alp,pi));
                          arg  = _mm512_div_pd(pi,n);
                          sarg = xsin(arg);
                          x0   = _mm512_div_pd(_mm512_mul_pd(k0a,sarg),n);
                          carg = xcos(arg);
                          arg2 = _mm512_div_pd(_3pi,n);
                          x1   = _mm512_mul_pd(x0,x0);
                          carg2= xcos(arg2);
                          x0   = _mm512_sub_pd(carg,carg2);
                          inv2 = _mm512_rcp14_pd(_mm512_mul_pd(x0,x0));
                          rcs  = _mm512_mul_pd(ipi,_mm512_mul_pd(x0,inv2));
                          return (rcs);
                 }


                  /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series convergence.
                         Formula 6.3-56
                    */


                  /*
                         Complex 'j' phase associated with each 'j' amplitude.
                         Formula 6.3-58, 6.3-56 (complex exponent term only).
                    */


                  
                   void expj_f6358_zmm8r8(const __m512d k0,
                                         const __m512d beta,
                                         const __m512d a,
                                         const __m512d h,
                                         const __m512d tht,
                                       __m512d * __restrict cer,
                                       __m512d * __restrict cei) {

                       
                        const __m512d pi4 = _mm512_set1_pd(0.78539816339744830961566084582);
                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        register __m512d _2k0,b2,sint,cost,cosb,negt,x0;
                        register __m512d ear,eai;
                        b2   = _mm512_mul_pd(beta,hlf);
                        sint = xsin(tht);
                        _2k0 = _mm512_add_pd(k0,k0);
                        cost = xcos(tht);
                        ear  = _mm512_setzero_pd();
                        cosb = xcos(b2);
                        negt = negate_zmm8r8( _mm512_mul_pd(_2k0,cosb));
                        x0   = _mm512_fmadd_pd(a,sint,_mm512_mul_pd(h,cost));
                        eai  = _mm512_fmadd_pd(negt,x0,pi4);
                        cexp_zmm8r8(ear,eai,*cer,*cei);
                 }


                  
                   void expj_f6358_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pbeta,
                                           const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) ph,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           float * __restrict __ATTR_ALIGN__(64) cer,
                                           float * __restrict __ATTR_ALIGN__(64) cei) {

                        
                        register __m512d k0   = _mm512_load_pd(&pk0[0]);
                        register __m512d beta = _mm512_load_pd(&pbeta[0]);
                        register __m512d a    = _mm512_load_pd(&pa[0]);
                        register __m512d h    = _mm512_load_pd(&ph[0]);
                        register __m512d tht  = _mm512_load_pd(&ptht[0]);
                        const __m512d pi4 = _mm512_set1_pd(0.78539816339744830961566084582);
                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        register __m512d _2k0,b2,sint,cost,cosb,negt,x0;
                        register __m512d ear,eai,resr,resi;
                        b2   = _mm512_mul_pd(beta,hlf);
                        sint = xsin(tht);
                        _2k0 = _mm512_add_pd(k0,k0);
                        cost = xcos(tht);
                        ear  = _mm512_setzero_pd();
                        cosb = xcos(b2);
                        negt = negate_zmm8r8( _mm512_mul_pd(_2k0,cosb));
                        x0   = _mm512_fmadd_pd(a,sint,_mm512_mul_pd(h,cost));
                        eai  = _mm512_fmadd_pd(negt,x0,pi4);
                        cexp_zmm8r8(ear,eai,&resr,&resi);
                        _mm512_store_pd(&cer[0], resr);
                        _mm512_store_pd(&cei[0], resi);
                 }


                  
                   void expj_f6358_zmm8r8_u(const float * __restrict  pk0,
                                           const float * __restrict  pbeta,
                                           const float * __restrict  pa,
                                           const float * __restrict  ph,
                                           const float * __restrict  ptht,
                                           float * __restrict  cer,
                                           float * __restrict  cei) {

                       
                        register __m512d k0   = _mm512_loadu_pd(&pk0[0]);
                        register __m512d beta = _mm512_loadu_pd(&pbeta[0]);
                        register __m512d a    = _mm512_loadu_pd(&pa[0]);
                        register __m512d h    = _mm512_loadu_pd(&ph[0]);
                        register __m512d tht  = _mm512_loadu_pd(&ptht[0]);
                        const __m512d pi4 = _mm512_set1_pd(0.78539816339744830961566084582);
                        const __m512d hlf = _mm512_set1_pd(0.5f);
                        register __m512d _2k0,b2,sint,cost,cosb,negt,x0;
                        register __m512d ear,eai,resr,resi;
                        b2   = _mm512_mul_pd(beta,hlf);
                        sint = xsin(tht);
                        _2k0 = _mm512_add_pd(k0,k0);
                        cost = xcos(tht);
                        ear  = _mm512_setzero_pd();
                        cosb = xcos(b2);
                        negt = negate_zmm8r8( _mm512_mul_pd(_2k0,cosb));
                        x0   = _mm512_fmadd_pd(a,sint,_mm512_mul_pd(h,cost));
                        eai  = _mm512_fmadd_pd(negt,x0,pi4);
                        cexp_zmm8r8(ear,eai,&resr,&resi);
                        _mm512_storeu_pd(&cer[0], resr);
                        _mm512_storeu_pd(&cei[0], resi);
                 }


                   /*
                         Axially asymetric edge diffraction amplitudes.
                         Part of kernel 6.3-56
                         Formula 6.3-57
                     */


                 
                   __m512d rcs_f6357_zmm8r8(const __m512d alp,
                                            const __m512d tht,
                                            const __m512d beta,
                                            const __m512d beta1,
                                            const __m512d a,
                                            const __m512d k0,
                                            const bool ver ) { // addition (true) or subtraction (false) of cosine terms in 6.3-57

                          const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d _3o2 = _mm512_set1_pd(1.5f);
                          const __m512d hlf  = _mm512_set1_pd(0.5f);
                          const __m512d _2   = _mm512_set1_pd(2.0f);
                          register __m512d b1,n,arg1,psi1,sarg1,carg1,arg2,carg2,x2;
                          register __m512d rcs,invn,cosb2,x0,x1,trm1,trm2,trm3,sint,trm4;
                          n    = _mm512_add_pd(_3o2,_mm512_div_pd(alp,pi));
                          arg2 = _mm512_fmadd_pd(_2,beta,pi);
                          b1   = _mm512_mul_pd(beta1,hlf);
                          arg2 = _mm512_div_pd(arg2,n);
                          arg1 = _mm512_div_pd(pi,n);
                          psi1 = _mm512_add_pd(tht,b1);
                          sarg1= xsin(arg1);
                          sint = xsin(psi1);
                          invn = _mm512_rcp14_pd(n);
                          cosb2= xcos(_mm512_mul_pd(beta,hlf));
                          trm1 = _mm512_mul_pd(sarg1,invn);
                          x2   = _mm512_div_pd(beta1,n);
                          carg1= xcos(arg1);
                          x0   = _mm512_mul_pd(a,_mm512_rcp14_pd(sint));
                          carg2= xcos(arg2);
                          x1   = _mm512_mul_pd(k0,cosb2);
                          sarg1= xcos(x2);
                          trm2 = _mm512_div_pd(x0,x1);
                          trm3 = _mm512_rcp14_pd(_mm512_sub_pd(carg1,carg2));
                          trm4 = _mm512_rcp14_pd(_mm512_sub_pd(carg1,sarg1));
                          x0   = _mm512_sqrt_pd(trm2);
                          (ver == true) ? x1   = _mm512_add_pd(trm3,trm4) : 
                                          x1   = _mm512_sub_pd(trm3,trm4);
                          rcs  = _mm512_mul_pd(_mm512_mul_pd(trm1,x0),x1);
                          return (rcs);
                 }


                  
                   __m512d rcs_f6357_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) palp,
                                              const float * __restrict __ATTR_ALIGN__(64) ptht,
                                              const float * __restrict __ATTR_ALIGN__(64) pbeta,
                                              const float * __restrict __ATTR_ALIGN__(64) pbeta1,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const bool ver) { // addition (true) or subtraction (false) of cosine terms in 6.3-57

                          register __m512d alp   = _mm512_load_pd(&palp[0]);
                          register __m512d tht   = _mm512_load_pd(&ptht[0]);
                          register __m512d beta  = _mm512_load_pd(&pbeta[0]);
                          register __m512d beta1 = _mm512_load_pd(&pbeta1[0]);
                          register __m512d a     = _mm512_load_pd(&pa[0]);
                          register __m512d k0    = _mm512_load_pd(&pk0[0]);
                          const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d _3o2 = _mm512_set1_pd(1.5f);
                          const __m512d hlf  = _mm512_set1_pd(0.5f);
                          const __m512d _2   = _mm512_set1_pd(2.0f);
                          register __m512d b1,n,arg1,psi1,sarg1,carg1,arg2,carg2,x2;
                          register __m512d rcs,invn,cosb2,x0,x1,trm1,trm2,trm3,sint,trm4;
                          n    = _mm512_add_pd(_3o2,_mm512_div_pd(alp,pi));
                          arg2 = _mm512_fmadd_pd(_2,beta,pi);
                          b1   = _mm512_mul_pd(beta1,hlf);
                          arg2 = _mm512_div_pd(arg2,n);
                          arg1 = _mm512_div_pd(pi,n);
                          psi1 = _mm512_add_pd(tht,b1);
                          sarg1= xsin(arg1);
                          sint = xsin(psi1);
                          invn = _mm512_rcp14_pd(n);
                          cosb2= xcos(_mm512_mul_pd(beta,hlf));
                          trm1 = _mm512_mul_pd(sarg1,invn);
                          x2   = _mm512_div_pd(beta1,n);
                          carg1= xcos(arg1);
                          x0   = _mm512_mul_pd(a,_mm512_rcp14_pd(sint));
                          carg2= xcos(arg2);
                          x1   = _mm512_mul_pd(k0,cosb2);
                          sarg1= xcos(x2);
                          trm2 = _mm512_div_pd(x0,x1);
                          trm3 = _mm512_rcp14_pd(_mm512_sub_pd(carg1,carg2));
                          trm4 = _mm512_rcp14_pd(_mm512_sub_pd(carg1,sarg1));
                          x0   = _mm512_sqrt_pd(trm2);
                          (ver == true) ? x1   = _mm512_add_pd(trm3,trm4) : 
                                          x1   = _mm512_sub_pd(trm3,trm4);
                          rcs  = _mm512_mul_pd(_mm512_mul_pd(trm1,x0),x1);
                          return (rcs);
                 }


                   
                   __m512d rcs_f6357_zmm8r8_u(const float * __restrict  palp,
                                              const float * __restrict  ptht,
                                              const float * __restrict  pbeta,
                                              const float * __restrict  pbeta1,
                                              const float * __restrict  pa,
                                              const float * __restrict  pk0,
                                              const bool ver) { // addition (true) or subtraction (false) of cosine terms in 6.3-57

                          register __m512d alp   = _mm512_loadu_pd(&palp[0]);
                          register __m512d tht   = _mm512_loadu_pd(&ptht[0]);
                          register __m512d beta  = _mm512_loadu_pd(&pbeta[0]);
                          register __m512d beta1 = _mm512_loadu_pd(&pbeta1[0]);
                          register __m512d a     = _mm512_loadu_pd(&pa[0]);
                          register __m512d k0    = _mm512_loadu_pd(&pk0[0]);
                          const __m512d pi   = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d _3o2 = _mm512_set1_pd(1.5f);
                          const __m512d hlf  = _mm512_set1_pd(0.5f);
                          const __m512d _2   = _mm512_set1_pd(2.0f);
                          register __m512d b1,n,arg1,psi1,sarg1,carg1,arg2,carg2,x2;
                          register __m512d rcs,invn,cosb2,x0,x1,trm1,trm2,trm3,sint,trm4;
                          n    = _mm512_add_pd(_3o2,_mm512_div_pd(alp,pi));
                          arg2 = _mm512_fmadd_pd(_2,beta,pi);
                          b1   = _mm512_mul_pd(beta1,hlf);
                          arg2 = _mm512_div_pd(arg2,n);
                          arg1 = _mm512_div_pd(pi,n);
                          psi1 = _mm512_add_pd(tht,b1);
                          sarg1= xsin(arg1);
                          sint = xsin(psi1);
                          invn = _mm512_rcp14_pd(n);
                          cosb2= xcos(_mm512_mul_pd(beta,hlf));
                          trm1 = _mm512_mul_pd(sarg1,invn);
                          x2   = _mm512_div_pd(beta1,n);
                          carg1= xcos(arg1);
                          x0   = _mm512_mul_pd(a,_mm512_rcp14_pd(sint));
                          carg2= xcos(arg2);
                          x1   = _mm512_mul_pd(k0,cosb2);
                          sarg1= xcos(x2);
                          trm2 = _mm512_div_pd(x0,x1);
                          trm3 = _mm512_rcp14_pd(_mm512_sub_pd(carg1,carg2));
                          trm4 = _mm512_rcp14_pd(_mm512_sub_pd(carg1,sarg1));
                          x0   = _mm512_sqrt_pd(trm2);
                          (ver == true) ? x1   = _mm512_add_pd(trm3,trm4) : 
                                          x1   = _mm512_sub_pd(trm3,trm4);
                          rcs  = _mm512_mul_pd(_mm512_mul_pd(trm1,x0),x1);
                          return (rcs);
                 }


                       /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- single term only
                    */


                 
                   __m512d rcs_f6356_term1_zmm8r8(const __m512d alp,
                                              const __m512d h,
                                              const __m512d beta,
                                              const __m512d beta1,
                                              const __m512d a,
                                              const __m512d k0,
                                              const __m512d tht,
                                              const bool ver) {

                          register __m512d cer,cei,rcs6357,t0r,t0i,cabs,rcs;
                          expj_f6358_zmm8r8(k0,beta,a,h,tht,&cer,&cei);
                          rcs6357 = rcs_f6357_zmm8r8(alp,tht,beta,beta1,
                                                      a,k0,ver);
                          t0r = _mm512_mul_pd(cer,rcs6357);
                          t0i = _mm512_mul_pd(cei,rcs6357);
                          cabs= cabs_zmm8r8(t0r,t0i);
                          rcs = cabs;
                          return (rcs);
                 } 


                 
                   __m512d rcs_f6356_term1_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) palp,
                                                  const float * __restrict __ATTR_ALIGN__(64) ph,
                                                  const float * __restrict __ATTR_ALIGN__(64) pbeta,
                                                  const float * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                  const float * __restrict __ATTR_ALIGN__(64) pa,
                                                  const float * __restrict __ATTR_ALIGN__(64) pk0,
                                                  const float * __restrict __ATTR_ALIGN__(64) ptht,
                                                  const bool ver) {

                          register __m512d alp   = _mm512_load_pd(&palp[0]);
                          register __m512d h     = _mm512_load_pd(&ph[0]);
                          register __m512d beta  = _mm512_load_pd(&pbeta[0]);
                          register __m512d beta1 = _mm512_load_pd(&pbeta1[0]);
                          register __m512d a     = _mm512_load_pd(&pa[0]);
                          register __m512d k0    = _mm512_load_pd(&pk0[0]);
                          register __m512d tht   = _mm512_load_pd(&ptht[0]);
                          register __m512d cer,cei,rcs6357,t0r,t0i,cabs,rcs;
                          expj_f6358_zmm8r8(k0,beta,a,h,tht,&cer,&cei);
                          rcs6357 = rcs_f6357_zmm8r8(alp,tht,beta,beta1,
                                                      a,k0,ver);
                          t0r = _mm512_mul_pd(cer,rcs6357);
                          t0i = _mm512_mul_pd(cei,rcs6357);
                          cabs= cabs_zmm8r8(t0r,t0i);
                          rcs = cabs;
                          return (rcs);
                 } 


               
                   __m512d rcs_f6356_term1_zmm8r8_u(const float * __restrict  palp,
                                                  const float * __restrict  ph,
                                                  const float * __restrict  pbeta,
                                                  const float * __restrict  pbeta1,
                                                  const float * __restrict  pa,
                                                  const float * __restrict  pk0,
                                                  const float * __restrict  ptht,
                                                  const bool ver) {

                          register __m512d alp   = _mm512_loadu_pd(&palp[0]);
                          register __m512d h     = _mm512_loadu_pd(&ph[0]);
                          register __m512d beta  = _mm512_loadu_pd(&pbeta[0]);
                          register __m512d beta1 = _mm512_loadu_pd(&pbeta1[0]);
                          register __m512d a     = _mm512_loadu_pd(&pa[0]);
                          register __m512d k0    = _mm512_loadu_pd(&pk0[0]);
                          register __m512d tht   = _mm512_loadu_pd(&ptht[0]);
                          register __m512d cer,cei,rcs6357,t0r,t0i,cabs,rcs;
                          expj_f6358_zmm8r8(k0,beta,a,h,tht,&cer,&cei);
                          rcs6357 = rcs_f6357_zmm8r8(alp,tht,beta,beta1,
                                                      a,k0,ver);
                          t0r = _mm512_mul_pd(cer,rcs6357);
                          t0i = _mm512_mul_pd(cei,rcs6357);
                          cabs= cabs_zmm8r8(t0r,t0i);
                          rcs = cabs;
                          return (rcs);
                 } 


                     /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms.
                    */


                   __m512d rcs_f6356_nterm_zmm8r8(  const __m512d * __restrict __ATTR_ALIGN__(64) palp,
                                                    const __m512d h,
                                                    const __m512d * __restrict __ATTR_ALIGN__(64) pbeta,
                                                    const __m512d * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                    const __m512d a,
                                                    const __m512d k0,
                                                    const __m512d * __restrict __ATTR_ALIGN__(64) ptht,
                                                    const int32_t n,
                                                    const bool ver) {

                       

                         if(__builtin_expect(n<=0,0)) { return _mm512_setzero_pd();}
                         if(__builtin_expect(n==1,0)) { 
                             return (rcs_f6356_term1_zmm8r8(palp[0],h,pbeta[0],pbeta1[0],
                                                            a,k0,ptht[0],ver);)
                         }
                         
                         register __m512d cer,cei,rcs6357,t0r,t0i,cabs,rcs;
                         register __m512d accr,acci;
                         int32_t j; 
                         accr = _mm512_setzero_pd();
                         acci = accr;
                         for(j = 0; j != n; ++j) {
                             register __m512d al = palp[j];
                             register __m512d b = pbeta[j];
                             register __m512d b1= pbeta1[j];
                             register __m512d t = ptht[j];
                             expj_f6358_zmm8r8(k0,b,a,h,t,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al,t,b,b1,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                         }
                         cabs = cabs_zmm8r8(accr,acci);
                         rcs  = cabs;
                         return (rcs);
                 }


                    /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms, unrolled 16x.
                    */

              
                   float rcs_f6356_nterm_u16x_zmm8r8(    const __m512d * __restrict __ATTR_ALIGN__(64) palp,
                                                         const __m512d h,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                         const __m512d a,
                                                         const __m512d k0,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) ptht,
                                                         const int32_t n,
                                                         const bool ver) {

                       

                         if(__builtin_expect(n<=0,0)) { return _mm512_setzero_pd();}
                                                  
                         __m512d cer,cei,rcs6357,t0r,t0i,cabs;
                         float rcs = 0.0f;
                         register __m512d al0,al1,al2,al3,al4,al5,al6,al7
                         register __m512d b0,b1,b2,b3,b4,b,b6,b7;
                         register __m512d b10,b11,b12,b13,b14,b1,b16,b17;
                         register __m512d t0,t1,t2,t3,t4,t5,t6,t7;
                         register __m512d accr,acci;
                         int32_t j,m,m1; 
                         accr = _mm512_setzero_pd();
                         acci = accr;
                         m = n%16;
                         if(m != 0) {
                            for(j = 0; j != m; ++j) {
                                register __m512d al = palp[j];
                                register __m512d b = pbeta[j];
                                register __m512d b1= pbeta1[j];
                                register __m512d t = ptht[j];
                                expj_f6358_zmm8r8(k0,b,a,h,t,&cer,&cei);
                                rcs6357 = rcs_f6357_zmm8r8(al,t,b,b1,
                                                         a,k0,ver);
                                t0r     = _mm512_mul_pd(cer,rcs6357);
                                accr    = _mm512_add_pd(accr,t0r);
                                t0i     = _mm512_mul_pd(cei,rcs6357);
                                acci    = _mm512_add_pd(acci,t0i);
                            }
                             if(n<16) {
                                cabs = cabs_zmm8r8(accr,acci);
                                rcs  = _mm512_reduce_add_pd(cabs);
                                return (rcs);
                             }
                         }
                         m1 = m+1;
                         for(j = m1; j != n; j += 16) {
                             al0 = palp[j+0];
                             b0 = pbeta[j+0];
                             b10= pbeta1[j+0];
                             t0 = ptht[j+0];
                             expj_f6358_zmm8r8(k0,b0,a,h,t0,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al0,t0,b0,b10,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al1 = palp[j+1];
                             b1 = pbeta[j+1];
                             b11= pbeta1[j+1];
                             t1 = ptht[j+1];
                             expj_f6358_zmm8r8(k0,b1,a,h,t1,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al1,t1,b1,b11,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al2 = palp[j+2];
                             b2 = pbeta[j+2];
                             b12= pbeta1[j+2];
                             t2 = ptht[j+2];
                             expj_f6358_zmm8r8(k0,b2,a,h,t2,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al2,t2,b2,b12,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al3 = palp[j+3];
                             b3 = pbeta[j+3];
                             b13= pbeta1[j+3];
                             t3 = ptht[j+3];
                             expj_f6358_zmm8r8(k0,b3,a,h,t3,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al3,t3,b3,b13,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al4 = palp[j+4];
                             b4 = pbeta[j+4];
                             b14= pbeta1[j+4];
                             t4 = ptht[j+4];
                             expj_f6358_zmm8r8(k0,b4,a,h,t4,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al4,t4,b4,b14,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al5 = palp[j+5];
                             b5 = pbeta[j+5];
                             b15= pbeta1[j+5];
                             t5 = ptht[j+5];
                             expj_f6358_zmm8r8(k0,b5,a,h,t5,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al5,t5,b5,b15,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al6 = palp[j+6];
                             b6 = pbeta[j+6];
                             b16= pbeta1[j+6];
                             t6 = ptht[j+6];
                             expj_f6358_zmm8r8(k0,b6,a,h,t6,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al6,t6,b6,b16,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al7 = palp[j+7];
                             b7 = pbeta[j+7];
                             b17= pbeta1[j+7];
                             t7 = ptht[j+7];
                             expj_f6358_zmm8r8(k0,b7,a,h,t7,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al7,t7,b7,b17,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al0 = palp[j+8];
                             b0 = pbeta[j+8];
                             b10= pbeta1[j+8];
                             t0 = ptht[j+8];
                             expj_f6358_zmm8r8(k0,b0,a,h,t0,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al0,t0,b0,b10,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al1 = palp[j+9];
                             b1 = pbeta[j+9];
                             b11= pbeta1[j+9];
                             t1 = ptht[j+9];
                             expj_f6358_zmm8r8(k0,b1,a,h,t1,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al1,t1,b1,b11,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al2 = palp[j+10];
                             b2 = pbeta[j+10];
                             b12= pbeta1[j+10];
                             t2 = ptht[j+10];
                             expj_f6358_zmm8r8(k0,b2,a,h,t2,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al2,t2,b2,b12,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al3 = palp[j+11];
                             b3 = pbeta[j+11];
                             b13= pbeta1[j+11];
                             t3 = ptht[j+11];
                             expj_f6358_zmm8r8(k0,b3,a,h,t3,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al3,t3,b3,b13,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al4 = palp[j+12];
                             b4 = pbeta[j+12];
                             b14= pbeta1[j+12];
                             t4 = ptht[j+12];
                             expj_f6358_zmm8r8(k0,b4,a,h,t4,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al4,t4,b4,b14,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al5 = palp[j+13];
                             b5 = pbeta[j+13];
                             b15= pbeta1[j+13];
                             t5 = ptht[j+13];
                             expj_f6358_zmm8r8(k0,b5,a,h,t5,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al5,t5,b5,b15,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al6 = palp[j+14];
                             b6 = pbeta[j+14];
                             b16= pbeta1[j+14];
                             t6 = ptht[j+14];
                             expj_f6358_zmm8r8(k0,b6,a,h,t6,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al6,t6,b6,b16,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             al7 = palp[j+15];
                             b7 = pbeta[j+15];
                             b17= pbeta1[j+15];
                             t7 = ptht[j+15];
                             expj_f6358_zmm8r8(k0,b7,a,h,t7,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al7,t7,b7,b17,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                         }
                         cabs = cabs_zmm8r8(accr,acci);
                         rcs  = _mm512_reduce_add_pd(cabs);
                         return (rcs);
                 }



                   /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms, unrolled 8x.
                    */


                
                   float rcs_f6356_nterm_u8x_zmm8r8(    const __m512d * __restrict __ATTR_ALIGN__(64) palp,
                                                         const __m512d h,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                         const __m512d a,
                                                         const __m512d k0,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) ptht,
                                                         const int32_t n,
                                                         const bool ver) {

                       

                         if(__builtin_expect(n<=0,0)) { return _mm512_setzero_pd();}
                                                  
                         __m512d cer,cei,rcs6357,t0r,t0i,cabs;
                         float rcs = 0.0f;
                        // register __m512d al0,al1,al2,al3,al4,al5,al6,al7
                        // register __m512d b0,b1,b2,b3,b4,b,b6,b7;
                        // register __m512d b10,b11,b12,b13,b14,b1,b16,b17;
                        // register __m512d t0,t1,t2,t3,t4,t5,t6,t7;
                        // register __m512d accr,acci;
                         int32_t j,m,m1; 
                         accr = _mm512_setzero_pd();
                         acci = accr;
                         m = n%8;
                         if(m != 0) {
                            for(j = 0; j != m; ++j) {
                                register __m512d al = palp[j];
                                register __m512d b = pbeta[j];
                                register __m512d b1= pbeta1[j];
                                register __m512d t = ptht[j];
                                expj_f6358_zmm8r8(k0,b,a,h,t,&cer,&cei);
                                rcs6357 = rcs_f6357_zmm8r8(al,t,b,b1,
                                                         a,k0,ver);
                                t0r     = _mm512_mul_pd(cer,rcs6357);
                                accr    = _mm512_add_pd(accr,t0r);
                                t0i     = _mm512_mul_pd(cei,rcs6357);
                                acci    = _mm512_add_pd(acci,t0i);
                            }
                             if(n<8) {
                                cabs = cabs_zmm8r8(accr,acci);
                                rcs  = _mm512_reduce_add_pd(cabs);
                                return (rcs);
                             }
                         }
                         m1 = m+1;
                         for(j = m1; j != n; j += 8) {
                             register __m512d al0 = palp[j+0];
                             register __m512d b0 = pbeta[j+0];
                             register __m512d b10= pbeta1[j+0];
                             register __m512d t0 = ptht[j+0];
                             expj_f6358_zmm8r8(k0,b0,a,h,t0,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al0,t0,b0,b10,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             register __m512d al1 = palp[j+1];
                             register __m512d b1 = pbeta[j+1];
                             register __m512d b11= pbeta1[j+1];
                             register __m512d t1 = ptht[j+1];
                             expj_f6358_zmm8r8(k0,b1,a,h,t1,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al1,t1,b1,b11,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             register __m512d al2 = palp[j+2];
                             register __m512d b2 = pbeta[j+2];
                             register __m512d b12= pbeta1[j+2];
                             register __m512d t2 = ptht[j+2];
                             expj_f6358_zmm8r8(k0,b2,a,h,t2,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al2,t2,b2,b12,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             register __m512d al3 = palp[j+3];
                             register __m512d b3 = pbeta[j+3];
                             register __m512d b13= pbeta1[j+3];
                             register __m512d t3 = ptht[j+3];
                             expj_f6358_zmm8r8(k0,b3,a,h,t3,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al3,t3,b3,b13,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             register __m512d al4 = palp[j+4];
                             register __m512d b4 = pbeta[j+4];
                             register __m512d b14= pbeta1[j+4];
                             register __m512d t4 = ptht[j+4];
                             expj_f6358_zmm8r8(k0,b4,a,h,t4,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al4,t4,b4,b14,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             register __m512d al5 = palp[j+5];
                             register __m512d b5 = pbeta[j+5];
                             register __m512d b15= pbeta1[j+5];
                             register __m512d t5 = ptht[j+5];
                             expj_f6358_zmm8r8(k0,b5,a,h,t5,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al5,t5,b5,b15,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             register __m512d al6 = palp[j+6];
                             register __m512d b6 = pbeta[j+6];
                             register __m512d b16= pbeta1[j+6];
                             register __m512d t6 = ptht[j+6];
                             expj_f6358_zmm8r8(k0,b6,a,h,t6,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al6,t6,b6,b16,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             register __m512d al7 = palp[j+7];
                             register __m512d b7 = pbeta[j+7];
                             register __m512d b17= pbeta1[j+7];
                             register __m512d t7 = ptht[j+7];
                             expj_f6358_zmm8r8(k0,b7,a,h,t7,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al7,t7,b7,b17,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                         }
                         cabs = cabs_zmm8r8(accr,acci);
                         rcs  = _mm512_reduce_add_pd(cabs);
                         return (rcs);
                 }


              

                      /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms, unrolled 4x.
                    */


                
                   float rcs_f6356_nterm_u4x_zmm8r8(    const __m512d * __restrict __ATTR_ALIGN__(64) palp,
                                                         const __m512d h,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                         const __m512d a,
                                                         const __m512d k0,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) ptht,
                                                         const int32_t n,
                                                         const bool ver) {

                       

                         if(__builtin_expect(n<=0,0)) { return _mm512_setzero_pd();}
                                                  
                         __m512d cer,cei,rcs6357,t0r,t0i,cabs;
                         float rcs = 0.0f;
                        // register __m512d al0,al1,al2,al3,al4,al5,al6,al7
                        // register __m512d b0,b1,b2,b3,b4,b,b6,b7;
                        // register __m512d b10,b11,b12,b13,b14,b1,b16,b17;
                        // register __m512d t0,t1,t2,t3,t4,t5,t6,t7;
                        // register __m512d accr,acci;
                         int32_t j,m,m1; 
                         accr = _mm512_setzero_pd();
                         acci = accr;
                         m = n%4;
                         if(m != 0) {
                            for(j = 0; j != m; ++j) {
                                register __m512d al = palp[j];
                                register __m512d b = pbeta[j];
                                register __m512d b1= pbeta1[j];
                                register __m512d t = ptht[j];
                                expj_f6358_zmm8r8(k0,b,a,h,t,&cer,&cei);
                                rcs6357 = rcs_f6357_zmm8r8(al,t,b,b1,
                                                         a,k0,ver);
                                t0r     = _mm512_mul_pd(cer,rcs6357);
                                accr    = _mm512_add_pd(accr,t0r);
                                t0i     = _mm512_mul_pd(cei,rcs6357);
                                acci    = _mm512_add_pd(acci,t0i);
                            }
                             if(n<4) {
                                cabs = cabs_zmm8r8(accr,acci);
                                rcs  = _mm512_reduce_add_pd(cabs);
                                return (rcs);
                             }
                         }
                         m1 = m+1;
                         for(j = m1; j != n; j += 4) {
                             register __m512d al0 = palp[j+0];
                             register __m512d b0 = pbeta[j+0];
                             register __m512d b10= pbeta1[j+0];
                             register __m512d t0 = ptht[j+0];
                             expj_f6358_zmm8r8(k0,b0,a,h,t0,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al0,t0,b0,b10,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             register __m512d al1 = palp[j+1];
                             register __m512d b1 = pbeta[j+1];
                             register __m512d b11= pbeta1[j+1];
                             register __m512d t1 = ptht[j+1];
                             expj_f6358_zmm8r8(k0,b1,a,h,t1,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al1,t1,b1,b11,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             register __m512d al2 = palp[j+2];
                             register __m512d b2 = pbeta[j+2];
                             register __m512d b12= pbeta1[j+2];
                             register __m512d t2 = ptht[j+2];
                             expj_f6358_zmm8r8(k0,b2,a,h,t2,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al2,t2,b2,b12,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             register __m512d al3 = palp[j+3];
                             register __m512d b3 = pbeta[j+3];
                             register __m512d b13= pbeta1[j+3];
                             register __m512d t3 = ptht[j+3];
                             expj_f6358_zmm8r8(k0,b3,a,h,t3,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al3,t3,b3,b13,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                           
                         }
                         cabs = cabs_zmm8r8(accr,acci);
                         rcs  = _mm512_reduce_add_pd(cabs);
                         return (rcs);
                 }


                     /*
                         Geometrical Theory of Diffraction
                         RCS by amplitude series summing convergence.
                         Formula 6.3-56 -- multiple terms, unrolled 2x.
                    */


                 
                   float rcs_f6356_nterm_u2x_zmm8r8(    const __m512d * __restrict __ATTR_ALIGN__(64) palp,
                                                         const __m512d h,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) pbeta1,
                                                         const __m512d a,
                                                         const __m512d k0,
                                                         const __m512d * __restrict __ATTR_ALIGN__(64) ptht,
                                                         const int32_t n,
                                                         const bool ver) {

                       

                         if(__builtin_expect(n<=0,0)) { return _mm512_setzero_pd();}
                                                  
                         __m512d cer,cei,rcs6357,t0r,t0i,cabs;
                         float rcs = 0.0f;
                        // register __m512d al0,al1,al2,al3,al4,al5,al6,al7
                        // register __m512d b0,b1,b2,b3,b4,b,b6,b7;
                        // register __m512d b10,b11,b12,b13,b14,b1,b16,b17;
                        // register __m512d t0,t1,t2,t3,t4,t5,t6,t7;
                        // register __m512d accr,acci;
                         int32_t j,m,m1; 
                         accr = _mm512_setzero_pd();
                         acci = accr;
                         m = n%2;
                         if(m != 0) {
                            for(j = 0; j != m; ++j) {
                                register __m512d al = palp[j];
                                register __m512d b = pbeta[j];
                                register __m512d b1= pbeta1[j];
                                register __m512d t = ptht[j];
                                expj_f6358_zmm8r8(k0,b,a,h,t,&cer,&cei);
                                rcs6357 = rcs_f6357_zmm8r8(al,t,b,b1,
                                                         a,k0,ver);
                                t0r     = _mm512_mul_pd(cer,rcs6357);
                                accr    = _mm512_add_pd(accr,t0r);
                                t0i     = _mm512_mul_pd(cei,rcs6357);
                                acci    = _mm512_add_pd(acci,t0i);
                            }
                             if(n<2) {
                                cabs = cabs_zmm8r8(accr,acci);
                                rcs  = _mm512_reduce_add_pd(cabs);
                                return (rcs);
                             }
                         }
                         m1 = m+1;
                         for(j = m1; j != n; j += 2) {
                             register __m512d al0 = palp[j+0];
                             register __m512d b0 = pbeta[j+0];
                             register __m512d b10= pbeta1[j+0];
                             register __m512d t0 = ptht[j+0];
                             expj_f6358_zmm8r8(k0,b0,a,h,t0,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al0,t0,b0,b10,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                             register __m512d al1 = palp[j+1];
                             register __m512d b1 = pbeta[j+1];
                             register __m512d b11= pbeta1[j+1];
                             register __m512d t1 = ptht[j+1];
                             expj_f6358_zmm8r8(k0,b1,a,h,t1,&cer,&cei);
                             rcs6357 = rcs_f6357_zmm8r8(al1,t1,b1,b11,
                                                         a,k0,ver);
                             t0r     = _mm512_mul_pd(cer,rcs6357);
                             accr    = _mm512_add_pd(accr,t0r);
                             t0i     = _mm512_mul_pd(cei,rcs6357);
                             acci    = _mm512_add_pd(acci,t0i);
                                                      
                         }
                         cabs = cabs_zmm8r8(accr,acci);
                         rcs  = _mm512_reduce_add_pd(cabs);
                         return (rcs);
                 }


                   /*
                          Large loop k0a >> 1 , for theta = 0 degree.
                          Formula 6.4-10
                      */


                
                   __m512d rcs_f6410_zmm8r8(const __m512d k0,
                                            const __m512d a,
                                            const __m512d b) {

                          const __m512d pi  = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d _2pi= _mm512_set1_pd(9.869604401089358618834490999876);
                          const __m512d _2  = _mm512_set1_pd(2.0f);
                          register __m512d num,k0a,k0a2,k0b,arg,larg,den,rcs,x0;
                          k0a = _mm512_mul_pd(k0,a);
                          k0b = _mm512_mul_pd(k0,b);
                          k0a2= _mm512_mul_pd(k0a,k0a);
                          arg = _mm512_div_pd(_2,k0b);
                          num = _mm512_mul_pd(pi,k0a2);
                          larg= xlog(arg);
                          x0  = _mm512_add_pd(larg,larg);
                          den = _mm512_fmadd_pd(x0,x0,pi2);
                          rcs = _mm512_div_pd(num,den);
                          return (rcs);
                   }


                
                   __m512d rcs_f6410_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                            const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pb) {

                          register __m512d k0 = _mm512_load_pd(&pk0[0]);
                          register __m512d a  = _mm512_load_pd(&pa[0]);
                          register __m512d b  = _mm512_load_pd(&pb[0]);
                          const __m512d pi  = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d _2pi= _mm512_set1_pd(9.869604401089358618834490999876);
                          const __m512d _2  = _mm512_set1_pd(2.0f);
                          register __m512d num,k0a,k0a2,k0b,arg,larg,den,rcs,x0;
                          k0a = _mm512_mul_pd(k0,a);
                          k0b = _mm512_mul_pd(k0,b);
                          k0a2= _mm512_mul_pd(k0a,k0a);
                          arg = _mm512_div_pd(_2,k0b);
                          num = _mm512_mul_pd(pi,k0a2);
                          larg= xlog(arg);
                          x0  = _mm512_add_pd(larg,larg);
                          den = _mm512_fmadd_pd(x0,x0,pi2);
                          rcs = _mm512_div_pd(num,den);
                          return (rcs);
                   }


                 
                   __m512d rcs_f6410_zmm8r8_u(const float * __restrict  pk0,
                                            const float * __restrict  pa,
                                            const float * __restrict  pb) {

                          register __m512d k0 = _mm512_loadu_pd(&pk0[0]);
                          register __m512d a  = _mm512_loadu_pd(&pa[0]);
                          register __m512d b  = _mm512_loadu_pd(&pb[0]);
                          const __m512d pi  = _mm512_set1_pd(3.14159265358979323846264338328);
                          const __m512d _2pi= _mm512_set1_pd(9.869604401089358618834490999876);
                          const __m512d _2  = _mm512_set1_pd(2.0f);
                          register __m512d num,k0a,k0a2,k0b,arg,larg,den,rcs,x0;
                          k0a = _mm512_mul_pd(k0,a);
                          k0b = _mm512_mul_pd(k0,b);
                          k0a2= _mm512_mul_pd(k0a,k0a);
                          arg = _mm512_div_pd(_2,k0b);
                          num = _mm512_mul_pd(pi,k0a2);
                          larg= xlog(arg);
                          x0  = _mm512_add_pd(larg,larg);
                          den = _mm512_fmadd_pd(x0,x0,pi2);
                          rcs = _mm512_div_pd(num,den);
                          return (rcs);
                   }


                     /*
                         THe far-zone amplitude for TE-case.
                         For small loop k0a<<1
                         Formula 6.4-11
                      */

                       
                
                   void Uv_f6411_zmm8r8(const __m512d a,
                                         const __m512d k0,
                                         const __m512d b,
                                         const __m512d tht,
                                         __m512d *  __restrict Uvr,
                                         __m512d *  __restrict Uvi) {

                        const __m512d pis = _mm512_set1_pd(9.869604401089358618834490999876);
                        const __m512d _2  = _mm512_set1_pd(2.0f);
                        const __m512d _8  = _mm512_set1_pd(8.0f);
                        const __m512d sme0= _mm512_set1_pd(376.991118430775188623669955550061);
                        register __m512d ir,ii,a2,k0a,sint,num,x0;
                        register __m512d arg,larg,den;
                        ir   = _mm512_setzero_pd();
                        a2   = _mm512_mul_pd(a,a);
                        sint = xsin(tht);
                        k0a  = _mm512_mul_pd(a2,_mm512_mul_pd(k0,a));
                        arg  = _mm512_mul_pd(_8,_mm512_div_pd(a,b));
                        x0   = _mm512_fmadd_pd(sint,sint,_2);
                        larg = xlog(arg);
                        num  = _mm512_mul_pd(pis,_mm512_mul_pd(k0a,x0));
                        den  = _mm512_mul_pd(sme0,_mm512_sub_pd(larg,_2));
                        ii   = num;
                        *Uvr = ir;
                        *Uvi = _mm512_div_pd(ii,den); 
                 }


               
                   void Uv_f6411_u16x_zmm8r8(const __m512d a,
                                              const __m512d k0,
                                              const __m512d b,
                                              const __m512d * __restrict __ATTR_ALIGN__(64) tht,
                                              __m512d * __restrict __ATTR_ALIGN__(64) Uvr,
                                              __m512d * __restrict __ATTR_ALIGN__(64) Uvi,
                                              const int32_t n) {

                        if(__builtin_expect(n<=0,0)) { return;}
                        register __m512d t0,t1,t2,t3,t4,t5,t6,t7;
                        register __m512d t8,t9,t10,t11,t12,t13,t14,t15;
                        register __m512d resr,resi;
                        int32_t j,m1,m;
                        m = n%16;

                        if(m != 0) {
                           for(j = 0; j != m; ++j) {
                               t0 = tht[j];
                               Uv_f6411_zmm8r8(a,k0,b,t0,&resr,&resi);
                               _mm512_store_pd(&Uvr[j],resr);
                               _mm512_store_pd(&Uvi[j],resi);
                           }
                           if(n<16) { return;}
                        }

                        m1 = m+1;
                        for(j = m1; j != n; j += 16) {
                             t0 = tht[j+0];
                             Uv_f6411_zmm8r8(a,k0,b,t0,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+0],resr);
                             _mm512_store_pd(&Uvi[j+0],resi);
                             t1 = tht[j+1];
                             Uv_f6411_zmm8r8(a,k0,b,t1,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+1],resr);
                             _mm512_store_pd(&Uvi[j+1],resi);
                             t2 = tht[j+2];
                             Uv_f6411_zmm8r8(a,k0,b,t2,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+2],resr);
                             _mm512_store_pd(&Uvi[j+2],resi);
                             t3 = tht[j+3];
                             Uv_f6411_zmm8r8(a,k0,b,t3,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+3],resr);
                             _mm512_store_pd(&Uvi[j+3],resi);
                             t4 = tht[j+4];
                             Uv_f6411_zmm8r8(a,k0,b,t4,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+4],resr);
                             _mm512_store_pd(&Uvi[j+4],resi);
                             t5 = tht[j+5];
                             Uv_f6411_zmm8r8(a,k0,b,t5,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+5],resr);
                             _mm512_store_pd(&Uvi[j+5],resi);
                             t6 = tht[j+6];
                             Uv_f6411_zmm8r8(a,k0,b,t6,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+6],resr);
                             _mm512_store_pd(&Uvi[j+6],resi);
                             t7 = tht[j+7];
                             Uv_f6411_zmm8r8(a,k0,b,t7,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+7],resr);
                             _mm512_store_pd(&Uvi[j+7],resi);
                             t8 = tht[j+8];
                             Uv_f6411_zmm8r8(a,k0,b,t8,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+8],resr);
                             _mm512_store_pd(&Uvi[j+8],resi);
                             t9 = tht[j+9];
                             Uv_f6411_zmm8r8(a,k0,b,t9,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+9],resr);
                             _mm512_store_pd(&Uvi[j+9],resi);
                             t10 = tht[j+10];
                             Uv_f6411_zmm8r8(a,k0,b,t10,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+10],resr);
                             _mm512_store_pd(&Uvi[j+10],resi);
                             t11 = tht[j+11];
                             Uv_f6411_zmm8r8(a,k0,b,t11,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+11],resr);
                             _mm512_store_pd(&Uvi[j+11],resi);
                             t12 = tht[j+12];
                             Uv_f6411_zmm8r8(a,k0,b,t12,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+12],resr);
                             _mm512_store_pd(&Uvi[j+12],resi); 
                             t13 = tht[j+13];
                             Uv_f6411_zmm8r8(a,k0,b,t13,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+13],resr);
                             _mm512_store_pd(&Uvi[j+13],resi);
                             t14 = tht[j+14];
                             Uv_f6411_zmm8r8(a,k0,b,t14,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+14],resr);
                             _mm512_store_pd(&Uvi[j+14],resi);
                             t15 = tht[j+15];
                             Uv_f6411_zmm8r8(a,k0,b,t15,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+15],resr);
                             _mm512_store_pd(&Uvi[j+15],resi);
                        }
                 }


                  
                   void Uv_f6411_u8x_zmm8r8( const __m512d a,
                                              const __m512d k0,
                                              const __m512d b,
                                              const __m512d * __restrict __ATTR_ALIGN__(64) tht,
                                              __m512d * __restrict __ATTR_ALIGN__(64) Uvr,
                                              __m512d * __restrict __ATTR_ALIGN__(64) Uvi,
                                              const int32_t n) {

                        if(__builtin_expect(n<=0,0)) { return;}
                        register __m512d t0,t1,t2,t3,t4,t5,t6,t7;
                        register __m512d resr,resi;
                        int32_t j,m1,m;
                        m = n%8;

                        if(m != 0) {
                           for(j = 0; j != m; ++j) {
                               t0 = tht[j];
                               Uv_f6411_zmm8r8(a,k0,b,t0,&resr,&resi);
                               _mm512_store_pd(&Uvr[j],resr);
                               _mm512_store_pd(&Uvi[j],resi);
                           }
                           if(n<8) { return;}
                        }

                        m1 = m+1;
                        for(j = m1; j != n; j += 8) {
                             t0 = tht[j+0];
                             Uv_f6411_zmm8r8(a,k0,b,t0,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+0],resr);
                             _mm512_store_pd(&Uvi[j+0],resi);
                             t1 = tht[j+1];
                             Uv_f6411_zmm8r8(a,k0,b,t1,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+1],resr);
                             _mm512_store_pd(&Uvi[j+1],resi);
                             t2 = tht[j+2];
                             Uv_f6411_zmm8r8(a,k0,b,t2,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+2],resr);
                             _mm512_store_pd(&Uvi[j+2],resi);
                             t3 = tht[j+3];
                             Uv_f6411_zmm8r8(a,k0,b,t3,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+3],resr);
                             _mm512_store_pd(&Uvi[j+3],resi);
                             t4 = tht[j+4];
                             Uv_f6411_zmm8r8(a,k0,b,t4,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+4],resr);
                             _mm512_store_pd(&Uvi[j+4],resi);
                             t5 = tht[j+5];
                             Uv_f6411_zmm8r8(a,k0,b,t5,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+5],resr);
                             _mm512_store_pd(&Uvi[j+5],resi);
                             t6 = tht[j+6];
                             Uv_f6411_zmm8r8(a,k0,b,t6,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+6],resr);
                             _mm512_store_pd(&Uvi[j+6],resi);
                             t7 = tht[j+7];
                             Uv_f6411_zmm8r8(a,k0,b,t7,&resr,&resi);
                             _mm512_store_pd(&Uvr[j+7],resr);
                             _mm512_store_pd(&Uvi[j+7],resi);
                      }
                 }



                 
                   void Uv_f6411_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                           const float * __restrict __ATTR_ALIGN__(64) pk0,
                                           const float * __restrict __ATTR_ALIGN__(64) pb,
                                           const float * __restrict __ATTR_ALIGN__(64) ptht,
                                           float * __restrict __ATTR_ALIGN__(64) Uvr,
                                           float * __restrict __ATTR_ALIGN__(64) Uvi) {
  
                        register __m512d a   = _mm512_load_pd(&pa[0]);
                        register __m512d k0  = _mm512_load_pd(&pk0[0]);
                        register __m512d b   = _mm512_load_pd(&pb[0]);
                        register __m512d tht = _mm512_load_pd(&ptht[0]);
                        
                        const __m512d pis = _mm512_set1_pd(9.869604401089358618834490999876);
                        const __m512d _2  = _mm512_set1_pd(2.0f);
                        const __m512d _8  = _mm512_set1_pd(8.0f);
                        const __m512d sme0= _mm512_set1_pd(376.991118430775188623669955550061);
                        register __m512d ir,ii,a2,k0a,sint,num,x0;
                        register __m512d arg,larg,den;
                        ir   = _mm512_setzero_pd();
                        a2   = _mm512_mul_pd(a,a);
                        sint = xsin(tht);
                        k0a  = _mm512_mul_pd(a2,_mm512_mul_pd(k0,a));
                        arg  = _mm512_mul_pd(_8,_mm512_div_pd(a,b));
                        x0   = _mm512_fmadd_pd(sint,sint,_2);
                        larg = xlog(arg);
                        num  = _mm512_mul_pd(pis,_mm512_mul_pd(k0a,x0));
                        den  = _mm512_mul_pd(sme0,_mm512_sub_pd(larg,_2));
                        ii   = num;
                        _mm512_store_pd(&Uvr[0] ,ir);
                        _mm512_store_pd(&Uvi[0] ,_mm512_div_pd(ii,den)); 
                 }


                 
                   void Uv_f6411_zmm8r8_u(const float * __restrict  pa,
                                           const float * __restrict  pk0,
                                           const float * __restrict  pb,
                                           const float * __restrict  ptht,
                                           float * __restrict  Uvr,
                                           float * __restrict  Uvi) {
  
                        register __m512d a   = _mm512_loadu_pd(&pa[0]);
                        register __m512d k0  = _mm512_loadu_pd(&pk0[0]);
                        register __m512d b   = _mm512_loadu_pd(&pb[0]);
                        register __m512d tht = _mm512_loadu_pd(&ptht[0]);
                        
                        const __m512d pis = _mm512_set1_pd(9.869604401089358618834490999876);
                        const __m512d _2  = _mm512_set1_pd(2.0f);
                        const __m512d _8  = _mm512_set1_pd(8.0f);
                        const __m512d sme0= _mm512_set1_pd(376.991118430775188623669955550061);
                        register __m512d ir,ii,a2,k0a,sint,num,x0;
                        register __m512d arg,larg,den;
                        ir   = _mm512_setzero_pd();
                        a2   = _mm512_mul_pd(a,a);
                        sint = xsin(tht);
                        k0a  = _mm512_mul_pd(a2,_mm512_mul_pd(k0,a));
                        arg  = _mm512_mul_pd(_8,_mm512_div_pd(a,b));
                        x0   = _mm512_fmadd_pd(sint,sint,_2);
                        larg = xlog(arg);
                        num  = _mm512_mul_pd(pis,_mm512_mul_pd(k0a,x0));
                        den  = _mm512_mul_pd(sme0,_mm512_sub_pd(larg,_2));
                        ii   = num;
                        _mm512_storeu_pd(&Uvr[0] ,ir);
                        _mm512_storeu_pd(&Uvi[0] ,_mm512_div_pd(ii,den)); 
                 }


                  
                   __m512d rcs_f6412_zmm8r8(const __m512d a,
                                          const __m512d b,
                                          const __m512d k0,
                                          const __m512d tht) {

                        const __m512d pi34 = _mm512_set1_pd(7.751569170074955043869078766775);
                        const __m512d _2   = _mm512_set1_pd(2.0f);
                        const __m512d _8   = _mm512_set1_pd(8.0f);
                        register __m512d a2,k0a4,x0,arg,larg;
                        register __m512d rcs,slarg,x1,sint;
                        a2   = _mm512_mul_pd(a,a);
                        arg  = _mm512_div_pd(_8,_mm512_mul_pd(a,a));
                        sint = _mm512_add_pd(_2,xsin(tht));
                        x0   = _mm512_mul_pd(k0a,k0a);
                        larg = _mm512_sub_pd(xlog(arg),_2);
                        k0a4 = _mm512_mul_pd(x0,x0);
                        x1   = _mm512_sqrt_pd(larg);
                        x0   = _mm512_mul_pd(pi34,_mm512_mul_pd(a2,k0a4));
                        slarg= _mm512_rcp14_pd(x1);
                        a2   = _mm512_mul_pd(sint,sint);
                        rcs  = _mm512_mul_pd(x0,_mm512_mul_pd(a2,slarg));
                        return (rcs);
                }


                 
                   __m512d rcs_f6412_zmm8r8_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                            const float * __restrict __ATTR_ALIGN__(64) pb,
                                            const float * __restrict __ATTR_ALIGN__(64) pk0,
                                            const float * __restrict __ATTR_ALIGN__(64) ptht) {

                        register __m512d a  = _mm512_load_pd(&pa[0]);
                        register __m512d b  = _mm512_load_pd(&pb[0]);
                        register __m512d k0 = _mm512_load_pd(&pk0[0]);
                        register __m512d tht= _mm512_load_pd(&ptht[0]);
                        const __m512d pi34 = _mm512_set1_pd(7.751569170074955043869078766775);
                        const __m512d _2   = _mm512_set1_pd(2.0f);
                        const __m512d _8   = _mm512_set1_pd(8.0f);
                        register __m512d a2,k0a4,x0,arg,larg;
                        register __m512d rcs,slarg,x1,sint;
                        a2   = _mm512_mul_pd(a,a);
                        arg  = _mm512_div_pd(_8,_mm512_mul_pd(a,a));
                        sint = _mm512_add_pd(_2,xsin(tht));
                        x0   = _mm512_mul_pd(k0a,k0a);
                        larg = _mm512_sub_pd(xlog(arg),_2);
                        k0a4 = _mm512_mul_pd(x0,x0);
                        x1   = _mm512_sqrt_pd(larg);
                        x0   = _mm512_mul_pd(pi34,_mm512_mul_pd(a2,k0a4));
                        slarg= _mm512_rcp14_pd(x1);
                        a2   = _mm512_mul_pd(sint,sint);
                        rcs  = _mm512_mul_pd(x0,_mm512_mul_pd(a2,slarg));
                        return (rcs);
                }


                 
                   __m512d rcs_f6412_zmm8r8_u(const float * __restrict  pa,
                                            const float * __restrict  pb,
                                            const float * __restrict  pk0,
                                            const float * __restrict  ptht) {

                        register __m512d a  = _mm512_loadu_pd(&pa[0]);
                        register __m512d b  = _mm512_loadu_pd(&pb[0]);
                        register __m512d k0 = _mm512_loadu_pd(&pk0[0]);
                        register __m512d tht= _mm512_loadu_pd(&ptht[0]);
                        const __m512d pi34 = _mm512_set1_pd(7.751569170074955043869078766775);
                        const __m512d _2   = _mm512_set1_pd(2.0f);
                        const __m512d _8   = _mm512_set1_pd(8.0f);
                        register __m512d a2,k0a4,x0,arg,larg;
                        register __m512d rcs,slarg,x1,sint;
                        a2   = _mm512_mul_pd(a,a);
                        arg  = _mm512_div_pd(_8,_mm512_mul_pd(a,a));
                        sint = _mm512_add_pd(_2,xsin(tht));
                        x0   = _mm512_mul_pd(k0a,k0a);
                        larg = _mm512_sub_pd(xlog(arg),_2);
                        k0a4 = _mm512_mul_pd(x0,x0);
                        x1   = _mm512_sqrt_pd(larg);
                        x0   = _mm512_mul_pd(pi34,_mm512_mul_pd(a2,k0a4));
                        slarg= _mm512_rcp14_pd(x1);
                        a2   = _mm512_mul_pd(sint,sint);
                        rcs  = _mm512_mul_pd(x0,_mm512_mul_pd(a2,slarg));
                        return (rcs);
                }


                 
                                          

                    


                    





                  


       


















