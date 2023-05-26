

#ifndef __GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_H__
#define __GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_H__

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



    const unsigned int GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_MAJOR = 1U;
    const unsigned int GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_MINOR = 0U;
    const unsigned int GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_MICRO = 0U;
    const unsigned int GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_FULLVER =
      1000U*GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_MAJOR+
      100U*GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_MINOR+
      10U*GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_MICRO;
    const char * const GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_CREATION_DATE = "11-05-2023 10:53 PM +00200 (THR 11 MAY 2023 GMT+2)";
    const char * const GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_DESCRIPTION   = "AVX512 optimized Complex Surfaces Radar Cross Section functionality.";




#include <immintrin.h>
#include "GMS_kernel_config.h"






         
         
               /*
                   Work (input) arrays for kernel rcs_f8162_zmm16r4_2t_u and
                   rcs_f8162_zmm16r4_2t_a.
               */
               __ATTR_ALIGN__(64) struct RCS_F8162_DATA {
               
                       float * __restrict  Ya1; 
                       float * __restrict  Ya2; 
                       float * __restrict  Ya3; 
                       float * __restrict  Ea;  
                       float * __restrict  WRKa; 
                       float * __restrict  Yb1;
                       float * __restrict  Yb2; 
                       float * __restrict  Yb3; 
                       float * __restrict  Eb; 
                       float * __restrict  WRKb;  
               };
         
              
              /*
                    Surface discontinuities.
                    General perfectly conducting convex edge.
                    Backscatter diffraction coefficient 'D'.
                    Formula: 8.1-21
              */     
              
              
               
                   void coef_D12_f8121_zmm16r4(const __m512 gam,
                                             const __m512 phi,
                                             const __m512 k0,
                                             __m512 * __restrict D1r,
                                             __m512 * __restrict D1i,
                                            __m512 * __restrict D2r,
                                            __m512 * __restrict D2i) FUNC_ATTRIBUTES;
                
                
                
                 
                   void coef_D12_f8121_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam,
                                               const float * __restrict __ATTR_ALIGN__(64) pphi,
                                               const float * __restrict __ATTR_ALIGN__(64) pk0,
                                               float * __restrict __ATTR_ALIGN__(64) D1r,
                                               float * __restrict __ATTR_ALIGN__(64) D1i,
                                               float * __restrict __ATTR_ALIGN__(64) D2r,
                                               float * __restrict __ATTR_ALIGN__(64) D2i) FUNC_ATTRIBUTES;
                     
                
                
                
                 
                   void coef_D12_f8121_zmm16r4_u(const float * __restrict  pgam,
                                               const float * __restrict  pphi,
                                               const float * __restrict  pk0,
                                               float * __restrict  D1r,
                                               float * __restrict  D1i,
                                               float * __restrict  D2r,
                                               float * __restrict  D2i) FUNC_ATTRIBUTES;
                
                
                /*
                    Surface discontinuities.
                    General perfectly conducting convex edge.
                    Backscatter singly diffracted far-zone fields (E,H).
                    Formula: 8.1-19, 8.1-20
                
                */
                
                
                 
                   void EsHs_f811920_zmm16r4(    const __m512 betai,
                                                 const __m512 betas,
                                                 const __m512 gam,
                                                 const __m512 phi,
                                                 const __m512 k0,
                                                 const __m512 r,
                                                 const __m512 rho,
                                                 const __m512 psi,
                                                 __m512 * __restrict Esr,
                                                 __m512 * __restrict Esi,
                                                 __m512 * __restrict Hsr,
                                                 __m512 * __restrict Hsi) FUNC_ATTRIBUTES;
            
            
               
                   void EsHs_f811920_zmm16r4_a(  const float * __restrict __ATTR_ALIGN__(64) pbetai,
                                                 const float * __restrict __ATTR_ALIGN__(64) pbetas,
                                                 const float * __restrict __ATTR_ALIGN__(64) pgam,
                                                 const float * __restrict __ATTR_ALIGN__(64) pphi,
                                                 const float * __restrict __ATTR_ALIGN__(64) pk0,
                                                 const float * __restrict __ATTR_ALIGN__(64) pr,
                                                 const float * __restrict __ATTR_ALIGN__(64) prho,
                                                 const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                                 float * __restrict __ATTR_ALIGN__(64) Esr,
                                                 float * __restrict __ATTR_ALIGN__(64) Esi,
                                                 float * __restrict __ATTR_ALIGN__(64) Hsr,
                                                 float * __restrict __ATTR_ALIGN__(64) Hsi) FUNC_ATTRIBUTES;
            
                
                   void EsHs_f811920_zmm16r4_u(  const float * __restrict  pbetai,
                                                 const float * __restrict  pbetas,
                                                 const float * __restrict  pgam,
                                                 const float * __restrict  pphi,
                                                 const float * __restrict  pk0,
                                                 const float * __restrict  pr,
                                                 const float * __restrict  prho,
                                                 const float * __restrict ppsi,
                                                 float * __restrict  Esr,
                                                 float * __restrict  Esi,
                                                 float * __restrict  Hsr,
                                                 float * __restrict  Hsi) FUNC_ATTRIBUTES;
            
            
            /*
                    Surface discontinuities.
                    General perfectly conducting convex edge.
                    Backscatter diffraction coefficient 'D'.
                    Ray normal-incidence to one of edge faces.
                    Formula: 8.1-24
            */
            
                 
                   void coef_D12_f8124_zmm16r4(const __m512 k0,
                                               const __m512 gam,
                                               __m512 * __restrict D1r,
                                               __m512 * __restrict D1i,
                                               __m512 * __restrict D2r,
                                               __m512 * __restrict D2i) FUNC_ATTRIBUTES;
                
                
                
                   void coef_D12_f8124_zmm16r4_a(const  float * __restrict __ATTR_ALIGN__(64) pk0,
                                                 const  float * __restrict __ATTR_ALIGN__(64) pgam,
                                                 float * __restrict __ATTR_ALIGN__(64) D1r,
                                                 float * __restrict __ATTR_ALIGN__(64) D1i,
                                                 float * __restrict __ATTR_ALIGN__(64) D2r,
                                                 float * __restrict __ATTR_ALIGN__(64) D2i) FUNC_ATTRIBUTES;
                
                
                 
                   void coef_D12_f8124_zmm16r4_u(const  float * __restrict  pk0,
                                                 const  float * __restrict pgam,
                                                 float * __restrict  D1r,
                                                 float * __restrict  D1i,
                                                 float * __restrict  D2r,
                                                 float * __restrict  D2i) FUNC_ATTRIBUTES;
                
                /*
                    Surface discontinuities.
                    General perfectly conducting convex edge.
                    Backscatter diffraction coefficient 'D'.
                    Backscatter direction axial caustic (for slightly diffracted rays).
                    Formula: 8.1-26
                */
                
                
                
                   void coef_Ddiff_f8126_zmm16r4(const __m512 gam,
                                             const __m512 phi,
                                             const __m512 k0,
                                             __m512 * __restrict Dr,
                                             __m512 * __restrict Di) FUNC_ATTRIBUTES;
                
                
               
                   void coef_Ddiff_f8126_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pgam,
                                               const float * __restrict __ATTR_ALIGN__(64) pphi,
                                               const float * __restrict __ATTR_ALIGN__(64) pk0,
                                               float * __restrict __ATTR_ALIGN__(64)  Dr,
                                               float * __restrict __ATTR_ALIGN__(64)  Di) FUNC_ATTRIBUTES;
                
               
                   void coef_Ddiff_f8126_zmm16r4_u(const float * __restrict  pgam,
                                                   const float * __restrict pphi,
                                                   const float * __restrict  pk0,
                                                   float * __restrict  Dr,
                                                   float * __restrict  Di) FUNC_ATTRIBUTES;
                
                
                   /*
                    Surface discontinuities.
                    General perfectly conducting convex edge.
                    Backscatter diffraction coefficient 'D'.
                    Backscatter direction axial caustic (for slightly diffracted rays).
                    Scattered Electric and Magnetic fields.
                    Formula: 8.1-25
                */
                
                
           
                   void EsHs_f8125_zmm16r4(const __m512 a,
                                           const __m512 k0,
                                           const __m512 r,
                                           const __m512 gam,
                                           const __m512 phi,
                                           const __m512 psi,
                                           __m512 * __restrict Esr,
                                           __m512 * __restrict Esi,
                                           __m512 * __restrict Hsr,
                                           __m512 * __restrict Hsi) FUNC_ATTRIBUTES;
                
           
                   void EsHs_f8125_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pa,
                                             const float * __restrict __ATTR_ALIGN__(64) pk0,
                                             const float * __restrict __ATTR_ALIGN__(64) pr,
                                             const float * __restrict __ATTR_ALIGN__(64) pgam,
                                             const float * __restrict __ATTR_ALIGN__(64) pphi,
                                             const float * __restrict __ATTR_ALIGN__(64) ppsi,
                                             float * __restrict __ATTR_ALIGN__(64) Esr,
                                             float * __restrict __ATTR_ALIGN__(64) Esi,
                                             float * __restrict __ATTR_ALIGN__(64) Hsr,
                                             float * __restrict __ATTR_ALIGN__(64) Hsi) FUNC_ATTRIBUTES;
                
            
              
                   void EsHs_f8125_zmm16r4_u(const float * __restrict  pa,
                                             const float * __restrict  pk0,
                                             const float * __restrict  pr,
                                             const float * __restrict pgam,
                                             const float * __restrict  pphi,
                                             const float * __restrict  ppsi,
                                             float * __restrict  Esr,
                                             float * __restrict  Esi,
                                             float * __restrict  Hsr,
                                             float * __restrict  Hsi) FUNC_ATTRIBUTES;
                
                
                /*
                    Surface discontinuities.
                    General perfectly conducting convex edge.
                    Backscatter diffraction coefficient 'D'.
                    Doubly and high order diffracted rays --
                    bistatic diffraction coefficients.
                    Formula: 8.1-27  
                
                */
                
                
                
                   void coef_D12_f8127_zmm16r4(const __m512 k0,
                                               const __m512 gam,
                                               const __m512 phi1,
                                               const __m512 phi2,
                                               __m512 * __restrict D1r,
                                               __m512 * __restrict D1i,
                                               __m512 * __restrict D2r,
                                               __m512 * __restrict D2i) FUNC_ATTRIBUTES;
                
                
                 
                   void coef_D12_f8127_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                                 const float * __restrict __ATTR_ALIGN__(64) pgam,
                                                 const float * __restrict __ATTR_ALIGN__(64) pphi1,
                                                 const float * __restrict __ATTR_ALIGN__(64) pphi2,
                                                 float * __restrict __ATTR_ALIGN__(64)  D1r,
                                                 float * __restrict __ATTR_ALIGN__(64)  D1i,
                                                 float * __restrict __ATTR_ALIGN__(64)  D2r,
                                                 float * __restrict __ATTR_ALIGN__(64)  D2i) FUNC_ATTRIBUTES;
                
                
                
                   void coef_D12_f8127_zmm16r4_u(const float * __restrict  pk0,
                                                 const float * __restrict  pgam,
                                                 const float * __restrict  pphi1,
                                                 const float * __restrict  pphi2,
                                                 float * __restrict   D1r,
                                                 float * __restrict  D1i,
                                                 float * __restrict   D2r,
                                                 float * __restrict  D2i) FUNC_ATTRIBUTES;
                
                /*
                       Adachi expression for axial-incidence
                       of backscatter RCS for entire scatterer length.
                       Shall be used in case of thin long axially symetric 
                       bodies e.g. 'ogives,double-cones, etc.,'
                       Vectorization of an integrand.
                       Formula 8.1-62
                */


                 
               
                   float rcs_f8162_zmm16r4_u(const float * __restrict pdAdl,
                                             const float *  __restrict pdl,
                                             const float   k0,
                                             const float   l) FUNC_ATTRIBUTES;
                  
                  
              
                   float rcs_f8162_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pdAdl,
                                             const float * __restrict __ATTR_ALIGN__(64) pdl,
                                             const float   k0,
                                             const float   l) FUNC_ATTRIBUTES;
                  
                  
                   /*
                       Adachi expression for axial-incidence
                       of backscatter RCS for entire scatterer length.
                       Shall be used in case of thin long axially symetric 
                       bodies e.g. 'ogives,double-cones, etc.,'
                       Vectorization of an integrand.
                       Case of small integrand. 
                       Integrator 'avint' i.e. irregular abscissas
                       Formula 8.1-62
                */
                
               
                   float rcs_f8162_zmm16r4_avint_u(const float * __restrict pdAdl,
                                                   const float *  __restrict pdl,
                                                   const float   k0,
                                                   const float   l,
                                                   int32_t * __restrict ierr,
                                                   int32_t * __restrict ieri) FUNC_ATTRIBUTES;
                  
                  
                
                   float rcs_f8162_zmm16r4_avint_a(const float * __restrict __ATTR_ALIGN__(64) pdAdl,
                                                   const float * __restrict __ATTR_ALIGN__(64) pdl,
                                                   const float   k0,
                                                   const float   l,
                                                   int32_t * __restrict ierr,
                                                   int32_t * __restrict ieri) FUNC_ATTRIBUTES;
                  
                  
                  
                  
                  
                   /*
                       Adachi expression for axial-incidence
                       of backscatter RCS for entire scatterer length.
                       Shall be used in case of thin long axially symetric 
                       bodies e.g. 'ogives,double-cones, etc.,'
                       Vectorization of an integrand.
                       Case of small integrand -- single-threaded execution.
                       Formula 8.1-62
                */
                
                
               
                   float rcs_f8162_zmm16r4_u(const float * __restrict  pdAdl,
                                             const float * __restrict  pdl,
                                             float * __restrict  intr,
                                             float * __restrict  inti,
                                             float * __restrict  Y1,
                                             float * __restrict  Y2,
                                             float * __restrict  Y3,
                                             float * __restrict  E,
                                             float * __restrict  WRK
                                             const float   k0,
                                             const float   l,
                                             const int32_t NTAB) FUNC_ATTRIBUTES;
               
                 
                   float rcs_f8162_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pdAdl,
                                             const float * __restrict __ATTR_ALIGN__(64) pdl,
                                             float * __restrict __ATTR_ALIGN__(64) intr,
                                             float * __restrict __ATTR_ALIGN__(64) inti,
                                             float * __restrict __ATTR_ALIGN__(64) Y1,
                                             float * __restrict __ATTR_ALIGN__(64) Y2,
                                             float * __restrict __ATTR_ALIGN__(64) Y3,
                                             float * __restrict __ATTR_ALIGN__(64) E,
                                             float * __restrict __ATTR_ALIGN__(64) WRK
                                             const float   k0,
                                             const float   l,
                                             const int32_t NTAB) FUNC_ATTRIBUTES;
               
                   /*
                       Adachi expression for axial-incidence
                       of backscatter RCS for entire scatterer length.
                       Shall be used in case of thin long axially symetric 
                       bodies e.g. 'ogives,double-cones, etc.,'
                       Vectorization of an integrand.
                       Case of large integrand - single thread.
                       Integrator 'avint' i.e. irregular abscissas
                       Formula 8.1-62
                */
                
                
                
                   float rcs_f8162_zmm16r4_avint_u(const float * __restrict  pdAdl,
                                                   const float * __restrict  pdl,
                                                   float * __restrict  intr,
                                                   float * __restrict  inti,
                                                   const float   k0,
                                                   const float   l,
                                                   int32_t * __restrict ierr,
                                                   int32_t * __restrict ieri,
                                                   const int32_t NTAB) FUNC_ATTRIBUTES;
               
                   float rcs_f8162_zmm16r4_avint_a(const float * __restrict __ATTR_ALIGN__(64) pdAdl,
                                                   const float * __restrict __ATTR_ALIGN__(64) pdl,
                                                   float * __restrict __ATTR_ALIGN__(64) intr,
                                                   float * __restrict __ATTR_ALIGN__(64) inti,
                                                   const float   k0,
                                                   const float   l,
                                                   int32_t * __restrict ierr,
                                                   int32_t * __restrict ieri,
                                                   const int32_t NTAB) FUNC_ATTRIBUTES;
               
               
               
                 /*
                       Adachi expression for axial-incidence
                       of backscatter RCS for entire scatterer length.
                       Shall be used in case of thin long axially symetric 
                       bodies e.g. 'ogives,double-cones, etc.,'
                       Vectorization of an integrand.
                       Case of large integrand -- two-threaded execution of integrator.
                       Integrator 'cspint'
                       Formula 8.1-62
                */
                

    
                
                
                   float rcs_f8162_zmm16r4_cspint2t_u(const float * __restrict  pdAdl,
                                                     const float * __restrict  pdl,
                                                     float * __restrict  intr,
                                                     float * __restrict  inti,
                                                     struct RCS_F8162_DATA w,
                                                     const float   k0,
                                                     const float   l,
                                                     const int32_t NTAB) FUNC_ATTRIBUTES;
               
               
                
                   float rcs_f8162_zmm16r4_cspint2t_a(const float * __restrict __ATTR_ALIGN__(64) pdAdl,
                                                     const float * __restrict __ATTR_ALIGN__(64) pdl,
                                                     float * __restrict __ATTR_ALIGN__(64) intr,
                                                     float * __restrict __ATTR_ALIGN__(64) inti,
                                                     struct RCS_F8162_DATA w,
                                                     const float   k0,
                                                     const float   l,
                                                     const int32_t NTAB) FUNC_ATTRIBUTES;
               
                /*
                       Adachi expression for axial-incidence
                       of backscatter RCS for entire scatterer length.
                       Shall be used in case of thin long axially symetric 
                       bodies e.g. 'ogives,double-cones, etc.,'
                       Vectorization of an integrand.
                       Case of large integrand -- two-threaded execution of integrator.
                       Integrator 'avint' (irregular abscissas).
                       Formula 8.1-62
                */
                
                
               
                   float rcs_f8162_zmm16r4_avint2t_u(const float * __restrict  pdAdl,
                                                     const float * __restrict  pdl,
                                                     float * __restrict  intr,
                                                     float * __restrict  inti,
                                                     int32_t * __restrict ierr,
                                                     int32_t * __restrict ieri,
                                                     const float   k0,
                                                     const float   l,
                                                     const int32_t NTAB) FUNC_ATTRIBUTES;
               
                
                   float rcs_f8162_zmm16r4_avint2t_a(const float * __restrict __ATTR_ALIGN__(64) pdAdl,
                                                     const float * __restrict __ATTR_ALIGN__(64) pdl,
                                                     float * __restrict __ATTR_ALIGN__(64) intr,
                                                     float * __restrict __ATTR_ALIGN__(64) inti,
                                                     int32_t * __restrict ierr,
                                                     int32_t * __restrict ieri,
                                                     const float   k0,
                                                     const float   l,
                                                     const int32_t NTAB) 
               
               
               
               
               
               /*
                     High frequency approximations.
                     Rounded-tip cone total nose-on
                     backscatter RCS.
                     Formula 8.1-93
               */
               
               
               
                   __m512 rcs_f8193_zmm16r4(const __m512 b,
                                            const __m512 a,
                                            const __m512 k0,
                                            const __m512 alp,
                                            const __m512 l) FUNC_ATTRIBUTES;
                 
                 
                 
               
                   __m512 rcs_f8193_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pb,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) palp,
                                              const float * __restrict __ATTR_ALIGN__(64) pl) FUNC_ATTRIBUTES;
                 
                 
             
                   __m512 rcs_f8193_zmm16r4_u(const float * __restrict  pb,
                                              const float * __restrict  pa,
                                              const float * __restrict  pk0,
                                              const float * __restrict  palp,
                                              const float * __restrict _pl) FUNC_ATTRIBUTES;
                 
                 
                 /*
                     High frequency approximations.
                     Backscatter RCS of conical frustum
                     for |theta| = PI/2-alpha
                     Formula 8.1-96
                 */
                 
                 
                 
                   __m512 rcs_f8196_zmm16r4(const __m512 k0,
                                            const __m512 alp,
                                            const __m512 a,
                                            const __m512 b) FUNC_ATTRIBUTES;
                 
                 
               
                   __m512 rcs_f8196_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
                                              const float * __restrict __ATTR_ALIGN__(64) palp,
                                              const float * __restrict __ATTR_ALIGN__(64) pa,
                                              const float * __restrict __ATTR_ALIGN__(64) pb) FUNC_ATTRIBUTES;
                 
                 
              
                   __m512 rcs_f8196_zmm16r4_u(const float * __restrict pk0,
                                              const float * __restrict palp,
                                              const float * __restrict pa,
                                              const float * __restrict pb) FUNC_ATTRIBUTES;
                 
                 
                 /*
                     High frequency approximations.
                     Backscatter RCS of conical frustum
                     for 0<|theta|<alpha
                     Perpendicular RCS.
                     Formula 8.1-94
                 */
                 
                 
             /*      __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
	           __m512 rcs_perpendic_f8194_zmm16r4(const __m512 h,
	                                        const __m512 l,
	                                        const __m512 b,
	                                        const __m512 a,
	                                        const __m512 k0,
	                                        const __m512 tht,
	                                        const __m512 alp) {
	                                 
	                                  
	                 const __m512 C314159265358979323846264338328  = 
                                                     _mm512_set1_ps(3.14159265358979323846264338328f); 
                         const __m512 C1772453850905516027298167483341 = 
                                                     _mm512_set1_ps(1.772453850905516027298167483341f);
                         const __m512 C078539816339744830961566084582  = 
                                                     _mm512_set1_ps(0.78539816339744830961566084582f);
                         const __m512 C10                              = 
                                                     _mm512_set1_ps(1.0f);  
                         const __m512 C15                              = 
                                                     _mm512_set1_ps(1.5f); 
                         const __m512 C05                              = 
                                                     _mm512_set1_ps(0.5f);
                         const __m512 C20                              =
                                                     _mm512_set1_ps(2.0f);
                         register __m512 pin,n,invn,spin,cos1,k02,cos2,sint,cost;
                         register __m512 ear,eai1,eai2,eai3,cer1,cei1,sk02,sacs;
                         register __m512 cer2,cei2,cer3,cei3,x0,x1,x2,x3,atant;
                         register __m512 cpin1,cpin2,trm1,trm2,rcs;
                         __m512 t0r,t0i,t1r,t1i,a2;
                         hlb  = _mm512_sub_ps(h,_mm512_add_ps(l,b));
                         sint = xsinf(tht);
                         k02  = _mm512_add_ps(k0,k0);
                         n    = _mm512_mul_ps(C15,_mm512_div_ps(alp,  
                                                           C314159265358979323846264338328));   
                         csct = _mm512_rcp14_ps(sint);
                         a2   = _mm512_mul_ps(a,C05);
                         ear  = _mm512_setzero_ps();
                         sk02 = _mm512_sqrt_ps(_mm512_mul_ps(k0,C05));
                         x0   = _mm512_mul_ps(hlb,_mm512_sub_ps(cost,b));
                         invn = _mm512_rcp14_ps(n);
                         //x2   = _mm512_mul_ps(a,C05);
                         eai  = _mm512_mul_ps(k02,x0);
                         tant = xtanf(tht);
                         pin  = _mm512_mul_ps(C314159265358979323846264338328,invn);
                         sacs = _mm512_sqrt_ps(_mm512_mul_ps(a2,csct));
                         atant= _mm512_mul_ps(a,tant);
                         cost = xcosf(tht);
                         x0   = _mm512_mul_ps(b,C1772453850905516027298167483341);
                         cexp_zmm16r4(ear,eai,&cer1,&cei1);
                         cer1 = _mm512_mul_ps(x0,cer1);
                         spin = xsinf(pin);
                         cei1 = _mm512_mul_ps(x0,cei1);
                         cpin = xcosf(pin);
                         x1   = _mm512_mul_ps(_mm512_sub_ps(h,atant),cost);
                         eai2 = _mm512_fmadd_ps(k02,x1,C078539816339744830961566084582);
                         cexp_zmm16r4(ear,eai2,&cer2,&cei);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_mul_ps(x0,sacs);
                         cer2 = _mm512_mul_ps(cer2,x1);
                         cei2 = _mm512_mul_ps(cei2,x1);
                         cpin1= _mm512_rcp14_ps(_mm512_sub_ps(cpin,C10));
                         x2   = _mm512_mul_ps(C20,_mm512_add_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x2,invn));
                         x3   = _mm512_rcp14_ps(_mm512_sub_ps(cpin,cpin2));
                         trm1 = _mm512_sub_ps(cpin1,x3);
                         cmul_zmm16r4(cer1,cei1,cer2,cei2,&t0r,&t0i);
                         t0r  = _mm512_mul_ps(t0r,trm1);
                         t0i  = _mm512_mul_ps(t0i,trm1);
                         x0   = _mm512_mul_ps(C20,_mm512_sub_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x0,invn));
                         x1   = _mm512_rcp14_ps(cpin2);
                         trm2 = _mm512_sub_ps(cpin1,x1);
                         x2   = _mm512_fmadd_ps(cost,_mm512_mul_ps(k02,
                                                               _mm512_add_ps(h,atant)));
                         eai3 = _mm512_add_ps(C078539816339744830961566084582,x2);
                         cexp_zmm16r4(ear,ea3,&cer3,&cei3);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_sqrt_ps(_mm512_mul_ps(gms::math::
                                                                  negate_zmm16r4(a2),csct));
                         x2   = _mm512_mul_ps(x0,x1);
                         cer3 = _mm512_mul_ps(_mm512_mul_ps(cer3,x2),trm2);
                         cei3 = _mm512_mul_ps(_mm512_mul_ps(cei3,x2),trm2);
                         cmul_zmm16r4(t0r,t0i,cer3,cei3,&t1r,&t1i);
                         rcs  = cabs_zmm16r4(t1r,t1i);
                         return (rcs);
	        }
	        
	        
	        
	                                  
                    __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
	           __m512 rcs_perpendic_f8194_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) ph,
	                                         const float * __restrict __ATTR_ALIGN__(64) pl,
	                                         const float * __restrict __ATTR_ALIGN__(64) pb,
	                                         const float * __restrict __ATTR_ALIGN__(64) pa,
	                                         const float * __restrict __ATTR_ALIGN__(64) pk0,
	                                         const float * __restrict __ATTR_ALIGN__(64) ptht,
	                                         const float * __restrict __ATTR_ALIGN__(64) palp) {
	                                 
	                  
	                 register __m512 h  = _mm512_load_ps(&ph[0]);
	                 register __m512 l  = _mm512_load_ps(&pl[0]); 
	                 register __m512 b  = _mm512_load_ps(&pb[0]);   
	                 register __m512 a  = _mm512_load_ps(&pa[0]);  
	                 register __m512 k0 = _mm512_load_ps(&pk0[0]);
	                 register __m512 tht= _mm512_load_ps(&ptht[0]); 
	                 register __m512 alp= _mm512_load_ps(&palp[0]);        
	                 const __m512 C314159265358979323846264338328  = 
                                                     _mm512_set1_ps(3.14159265358979323846264338328f); 
                         const __m512 C1772453850905516027298167483341 = 
                                                     _mm512_set1_ps(1.772453850905516027298167483341f);
                         const __m512 C078539816339744830961566084582  = 
                                                     _mm512_set1_ps(0.78539816339744830961566084582f);
                         const __m512 C10                              = 
                                                     _mm512_set1_ps(1.0f);  
                         const __m512 C15                              = 
                                                     _mm512_set1_ps(1.5f); 
                         const __m512 C05                              = 
                                                     _mm512_set1_ps(0.5f);
                         const __m512 C20                              =
                                                     _mm512_set1_ps(2.0f);
                         __m512 pin,n,invn,spin,cos1,k02,cos2,sint,cost;
                         register __m512 ear,eai1,eai2,eai3,cer1,cei1,sk02,sacs;
                         register __m512 cer2,cei2,cer3,cei3,x0,x1,x2,x3,atant;
                         register __m512 cpin1,cpin2,trm1,trm2,rcs;
                         __m512 t0r,t0i,t1r,t1i,a2;
                         hlb  = _mm512_sub_ps(h,_mm512_add_ps(l,b));
                         sint = xsinf(tht);
                         k02  = _mm512_add_ps(k0,k0);
                         n    = _mm512_mul_ps(C15,_mm512_div_ps(alp,  
                                                           C314159265358979323846264338328));   
                         csct = _mm512_rcp14_ps(sint);
                         a2   = _mm512_mul_ps(a,C05);
                         ear  = _mm512_setzero_ps();
                         sk02 = _mm512_sqrt_ps(_mm512_mul_ps(k0,C05));
                         x0   = _mm512_mul_ps(hlb,_mm512_sub_ps(cost,b));
                         invn = _mm512_rcp14_ps(n);
                         //x2   = _mm512_mul_ps(a,C05);
                         eai  = _mm512_mul_ps(k02,x0);
                         tant = xtanf(tht);
                         pin  = _mm512_mul_ps(C314159265358979323846264338328,invn);
                         sacs = _mm512_sqrt_ps(_mm512_mul_ps(a2,csct));
                         atant= _mm512_mul_ps(a,tant);
                         cost = xcosf(tht);
                         x0   = _mm512_mul_ps(b,C1772453850905516027298167483341);
                         cexp_zmm16r4(ear,eai,&cer1,&cei1);
                         cer1 = _mm512_mul_ps(x0,cer1);
                         spin = xsinf(pin);
                         cei1 = _mm512_mul_ps(x0,cei1);
                         cpin = xcosf(pin);
                         x1   = _mm512_mul_ps(_mm512_sub_ps(h,atant),cost);
                         eai2 = _mm512_fmadd_ps(k02,x1,C078539816339744830961566084582);
                         cexp_zmm16r4(ear,eai2,&cer2,&cei);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_mul_ps(x0,sacs);
                         cer2 = _mm512_mul_ps(cer2,x1);
                         cei2 = _mm512_mul_ps(cei2,x1);
                         cpin1= _mm512_rcp14_ps(_mm512_sub_ps(cpin,C10));
                         x2   = _mm512_mul_ps(C20,_mm512_add_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x2,invn));
                         x3   = _mm512_rcp14_ps(_mm512_sub_ps(cpin,cpin2));
                         trm1 = _mm512_sub_ps(cpin1,x3);
                         cmul_zmm16r4(cer1,cei1,cer2,cei2,&t0r,&t0i);
                         t0r  = _mm512_mul_ps(t0r,trm1);
                         t0i  = _mm512_mul_ps(t0i,trm1);
                         x0   = _mm512_mul_ps(C20,_mm512_sub_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x0,invn));
                         x1   = _mm512_rcp14_ps(cpin2);
                         trm2 = _mm512_sub_ps(cpin1,x1);
                         x2   = _mm512_fmadd_ps(cost,_mm512_mul_ps(k02,
                                                               _mm512_add_ps(h,atant)));
                         eai3 = _mm512_add_ps(C078539816339744830961566084582,x2);
                         cexp_zmm16r4(ear,ea3,&cer3,&cei3);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_sqrt_ps(_mm512_mul_ps(gms::math::
                                                                  negate_zmm16r4(a2),csct));
                         x2   = _mm512_mul_ps(x0,x1);
                         cer3 = _mm512_mul_ps(_mm512_mul_ps(cer3,x2),trm2);
                         cei3 = _mm512_mul_ps(_mm512_mul_ps(cei3,x2),trm2);
                         cmul_zmm16r4(t0r,t0i,cer3,cei3,&t1r,&t1i);
                         rcs  = cabs_zmm16r4(t1r,t1i);
                         return (rcs);
	        }
	        
	        
	        
	           __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
	           __m512 rcs_perpendic_f8194_zmm16r4_u(    const float * __restrict  ph,
	                                                    const float * __restrict  pl,
	                                                    const float * __restrict  pb,
	                                                    const float * __restrict  pa,
	                                                    const float * __restrict  pk0,
	                                                    const float * __restrict  ptht,
	                                                    const float * __restrict  palp) {
	                                 
	                  
	                 register __m512 h  = _mm512_loadu_ps(&ph[0]);
	                 register __m512 l  = _mm512_loadu_ps(&pl[0]); 
	                 register __m512 b  = _mm512_loadu_ps(&pb[0]);   
	                 register __m512 a  = _mm512_loadu_ps(&pa[0]);  
	                 register __m512 k0 = _mm512_loadu_ps(&pk0[0]);
	                 register __m512 tht= _mm512_loadu_ps(&ptht[0]); 
	                 register __m512 alp= _mm512_loadu_ps(&palp[0]);        
	                 const __m512 C314159265358979323846264338328  = 
                                                     _mm512_set1_ps(3.14159265358979323846264338328f); 
                         const __m512 C1772453850905516027298167483341 = 
                                                     _mm512_set1_ps(1.772453850905516027298167483341f);
                         const __m512 C078539816339744830961566084582  = 
                                                     _mm512_set1_ps(0.78539816339744830961566084582f);
                         const __m512 C10                              = 
                                                     _mm512_set1_ps(1.0f);  
                         const __m512 C15                              = 
                                                     _mm512_set1_ps(1.5f); 
                         const __m512 C05                              = 
                                                     _mm512_set1_ps(0.5f);
                         const __m512 C20                              =
                                                     _mm512_set1_ps(2.0f);
                         __m512 pin,n,invn,spin,cos1,k02,cos2,sint,cost;
                         register __m512 ear,eai1,eai2,eai3,cer1,cei1,sk02,sacs;
                         register __m512 cer2,cei2,cer3,cei3,x0,x1,x2,x3,atant;
                         register __m512 cpin1,cpin2,trm1,trm2,rcs;
                         __m512 t0r,t0i,t1r,t1i,a2;
                         hlb  = _mm512_sub_ps(h,_mm512_add_ps(l,b));
                         sint = xsinf(tht);
                         k02  = _mm512_add_ps(k0,k0);
                         n    = _mm512_mul_ps(C15,_mm512_div_ps(alp,  
                                                           C314159265358979323846264338328));   
                         csct = _mm512_rcp14_ps(sint);
                         a2   = _mm512_mul_ps(a,C05);
                         ear  = _mm512_setzero_ps();
                         sk02 = _mm512_sqrt_ps(_mm512_mul_ps(k0,C05));
                         x0   = _mm512_mul_ps(hlb,_mm512_sub_ps(cost,b));
                         invn = _mm512_rcp14_ps(n);
                         //x2   = _mm512_mul_ps(a,C05);
                         eai  = _mm512_mul_ps(k02,x0);
                         tant = xtanf(tht);
                         pin  = _mm512_mul_ps(C314159265358979323846264338328,invn);
                         sacs = _mm512_sqrt_ps(_mm512_mul_ps(a2,csct));
                         atant= _mm512_mul_ps(a,tant);
                         cost = xcosf(tht);
                         x0   = _mm512_mul_ps(b,C1772453850905516027298167483341);
                         cexp_zmm16r4(ear,eai,&cer1,&cei1);
                         cer1 = _mm512_mul_ps(x0,cer1);
                         spin = xsinf(pin);
                         cei1 = _mm512_mul_ps(x0,cei1);
                         cpin = xcosf(pin);
                         x1   = _mm512_mul_ps(_mm512_sub_ps(h,atant),cost);
                         eai2 = _mm512_fmadd_ps(k02,x1,C078539816339744830961566084582);
                         cexp_zmm16r4(ear,eai2,&cer2,&cei);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_mul_ps(x0,sacs);
                         cer2 = _mm512_mul_ps(cer2,x1);
                         cei2 = _mm512_mul_ps(cei2,x1);
                         cpin1= _mm512_rcp14_ps(_mm512_sub_ps(cpin,C10));
                         x2   = _mm512_mul_ps(C20,_mm512_add_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x2,invn));
                         x3   = _mm512_rcp14_ps(_mm512_sub_ps(cpin,cpin2));
                         trm1 = _mm512_sub_ps(cpin1,x3);
                         cmul_zmm16r4(cer1,cei1,cer2,cei2,&t0r,&t0i);
                         t0r  = _mm512_mul_ps(t0r,trm1);
                         t0i  = _mm512_mul_ps(t0i,trm1);
                         x0   = _mm512_mul_ps(C20,_mm512_sub_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x0,invn));
                         x1   = _mm512_rcp14_ps(cpin2);
                         trm2 = _mm512_sub_ps(cpin1,x1);
                         x2   = _mm512_fmadd_ps(cost,_mm512_mul_ps(k02,
                                                               _mm512_add_ps(h,atant)));
                         eai3 = _mm512_add_ps(C078539816339744830961566084582,x2);
                         cexp_zmm16r4(ear,ea3,&cer3,&cei3);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_sqrt_ps(_mm512_mul_ps(gms::math::
                                                                  negate_zmm16r4(a2),csct));
                         x2   = _mm512_mul_ps(x0,x1);
                         cer3 = _mm512_mul_ps(_mm512_mul_ps(cer3,x2),trm2);
                         cei3 = _mm512_mul_ps(_mm512_mul_ps(cei3,x2),trm2);
                         cmul_zmm16r4(t0r,t0i,cer3,cei3,&t1r,&t1i);
                         rcs  = cabs_zmm16r4(t1r,t1i);
                         return (rcs);
	        }*/
	        
	        
	        
	         /*
                     High frequency approximations.
                     Backscatter RCS of conical frustum
                     for 0<|theta|<alpha
                     Parallel RCS.
                     Formula 8.1-94
                 */
                 
                 
             /*      __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
	           __m512 rcs_parallel_f8194_zmm16r4(const __m512 h,
	                                             const __m512 l,
	                                             const __m512 b,
	                                             const __m512 a,
	                                             const __m512 k0,
	                                             const __m512 tht,
	                                             const __m512 alp) {
	                                 
	                                  
	                 const __m512 C314159265358979323846264338328  = 
                                                     _mm512_set1_ps(3.14159265358979323846264338328f); 
                         const __m512 C1772453850905516027298167483341 = 
                                                     _mm512_set1_ps(1.772453850905516027298167483341f);
                         const __m512 C078539816339744830961566084582  = 
                                                     _mm512_set1_ps(0.78539816339744830961566084582f);
                         const __m512 C10                              = 
                                                     _mm512_set1_ps(1.0f);  
                         const __m512 C15                              = 
                                                     _mm512_set1_ps(1.5f); 
                         const __m512 C05                              = 
                                                     _mm512_set1_ps(0.5f);
                         const __m512 C20                              =
                                                     _mm512_set1_ps(2.0f);
                         register __m512 pin,n,invn,spin,cos1,k02,cos2,sint,cost;
                         register __m512 ear,eai1,eai2,eai3,cer1,cei1,sk02,sacs;
                         register __m512 cer2,cei2,cer3,cei3,x0,x1,x2,x3,atant;
                         register __m512 cpin1,cpin2,trm1,trm2,rcs;
                         __m512 t0r,t0i,t1r,t1i,a2;
                         hlb  = _mm512_sub_ps(h,_mm512_add_ps(l,b));
                         sint = xsinf(tht);
                         k02  = _mm512_add_ps(k0,k0);
                         n    = _mm512_mul_ps(C15,_mm512_div_ps(alp,  
                                                           C314159265358979323846264338328));   
                         csct = _mm512_rcp14_ps(sint);
                         a2   = _mm512_mul_ps(a,C05);
                         ear  = _mm512_setzero_ps();
                         sk02 = _mm512_sqrt_ps(_mm512_mul_ps(k0,C05));
                         x0   = _mm512_mul_ps(hlb,_mm512_sub_ps(cost,b));
                         invn = _mm512_rcp14_ps(n);
                         //x2   = _mm512_mul_ps(a,C05);
                         eai  = _mm512_mul_ps(k02,x0);
                         tant = xtanf(tht);
                         pin  = _mm512_mul_ps(C314159265358979323846264338328,invn);
                         sacs = _mm512_sqrt_ps(_mm512_mul_ps(a2,csct));
                         atant= _mm512_mul_ps(a,tant);
                         cost = xcosf(tht);
                         x0   = _mm512_mul_ps(b,C1772453850905516027298167483341);
                         cexp_zmm16r4(ear,eai,&cer1,&cei1);
                         cer1 = _mm512_mul_ps(x0,cer1);
                         spin = xsinf(pin);
                         cei1 = _mm512_mul_ps(x0,cei1);
                         cpin = xcosf(pin);
                         x1   = _mm512_mul_ps(_mm512_sub_ps(h,atant),cost);
                         eai2 = _mm512_fmadd_ps(k02,x1,C078539816339744830961566084582);
                         cexp_zmm16r4(ear,eai2,&cer2,&cei);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_mul_ps(x0,sacs);
                         cer2 = _mm512_mul_ps(cer2,x1);
                         cei2 = _mm512_mul_ps(cei2,x1);
                         cpin1= _mm512_rcp14_ps(_mm512_sub_ps(cpin,C10));
                         x2   = _mm512_mul_ps(C20,_mm512_add_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x2,invn));
                         x3   = _mm512_rcp14_ps(_mm512_sub_ps(cpin,cpin2));
                         trm1 = _mm512_add_ps(cpin1,x3);
                         cmul_zmm16r4(cer1,cei1,cer2,cei2,&t0r,&t0i);
                         t0r  = _mm512_mul_ps(t0r,trm1);
                         t0i  = _mm512_mul_ps(t0i,trm1);
                         x0   = _mm512_mul_ps(C20,_mm512_sub_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x0,invn));
                         x1   = _mm512_rcp14_ps(cpin2);
                         trm2 = _mm512_add_ps(cpin1,x1);
                         x2   = _mm512_fmadd_ps(cost,_mm512_mul_ps(k02,
                                                               _mm512_add_ps(h,atant)));
                         eai3 = _mm512_add_ps(C078539816339744830961566084582,x2);
                         cexp_zmm16r4(ear,ea3,&cer3,&cei3);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_sqrt_ps(_mm512_mul_ps(gms::math::
                                                                  negate_zmm16r4(a2),csct));
                         x2   = _mm512_mul_ps(x0,x1);
                         cer3 = _mm512_mul_ps(_mm512_mul_ps(cer3,x2),trm2);
                         cei3 = _mm512_mul_ps(_mm512_mul_ps(cei3,x2),trm2);
                         cmul_zmm16r4(t0r,t0i,cer3,cei3,&t1r,&t1i);
                         rcs  = cabs_zmm16r4(t1r,t1i);
                         return (rcs);
	        }
	        */
	        
	        
	        /*
	             Model 9B4 (Peake's Model)
	             Model resembling many natural grass-like structures like
	             forests,grass,wheat fields, etc.
	             Helper formula coefficient B(gamma).
	             Formula 9.1-37
	        */
	        
	        
	        
	         
	           __m512 coef_Bg_f9137_zmm16r4(const __m512 A,
	                                        const __m512 N,
	                                        const __m512 k0,
	                                        const __m512 epsr,
	                                        const __m512 epsi,
	                                        const __m512 thti,
	                                        const __m512 thts,
	                                        const __m512 phis,
	                                        const int pol) FUNC_ATTRIBUTES;
	       
	       
	           __m512 coef_Bg_f9137_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pA,
	                                          const float * __restrict __ATTR_ALIGN__(64) pN,
	                                          const float * __restrict __ATTR_ALIGN__(64) pk0,
	                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
	                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
	                                          const float * __restrict __ATTR_ALIGN__(64) pthti,
	                                          const float * __restrict __ATTR_ALIGN__(64) pthts,
	                                          const float * __restrict __ATTR_ALIGN__(64) pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	       
	      
	           __m512 coef_Bg_f9137_zmm16r4_u(const float * __restrict  pA,
	                                          const float * __restrict  pN,
	                                          const float * __restrict  pk0,
	                                          const float * __restrict  pepsr,
	                                          const float * __restrict  pepsi,
	                                          const float * __restrict  pthti,
	                                          const float * __restrict  pthts,
	                                          const float * __restrict  pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	       
	       /*
	             Model 9B4 (Peake's Model)
	             Model resembling many natural grass-like structures like
	             forests,grass,wheat fields, etc.
	             Bistatic RCS (hh) polarized per unit surface area.
	             Formula 9.1-33
	       
	       */
	       
	       
	      
	           __m512 rcs_hh_f9133_zmm16r4( const __m512 A,
	                                        const __m512 N,
	                                        const __m512 k0,
	                                        const __m512 epsr,
	                                        const __m512 epsi,
	                                        const __m512 thti,
	                                        const __m512 thts,
	                                        const __m512 phis,
	                                        const int pol) FUNC_ATTRIBUTES;
	       
	       
	       
	      
	           __m512 rcs_hh_f9133_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) pA,
	                                          const float * __restrict __ATTR_ALIGN__(64) pN,
	                                          const float * __restrict __ATTR_ALIGN__(64) pk0,
	                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
	                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
	                                          const float * __restrict __ATTR_ALIGN__(64) pthti,
	                                          const float * __restrict __ATTR_ALIGN__(64) pthts,
	                                          const float * __restrict __ATTR_ALIGN__(64) pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	       
	       
	       
	        
	           __m512 rcs_hh_f9133_zmm16r4_u( const float * __restrict pA,
	                                          const float * __restrict pN,
	                                          const float * __restrict pk0,
	                                          const float * __restrict pepsr,
	                                          const float * __restrict pepsi,
	                                          const float * __restrict pthti,
	                                          const float * __restrict pthts,
	                                          const float * __restrict pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	       
	       
	        
	       /*
	             Model 9B4 (Peake's Model)
	             Model resembling many natural grass-like structures like
	             forests,grass,wheat fields, etc.
	             Bistatic RCS (vh) polarized per unit surface area.
	             Formula 9.1-34
	       
	       */
	       
	       
	         
	           __m512 rcs_vh_f9134_zmm16r4( const __m512 A,
	                                        const __m512 N,
	                                        const __m512 k0,
	                                        const __m512 epsr,
	                                        const __m512 epsi,
	                                        const __m512 thti,
	                                        const __m512 thts,
	                                        const __m512 phis,
	                                        const int pol) FUNC_ATTRIBUTES;
	         
	         
	         
	      
	           __m512 rcs_vh_f9134_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) pA,
	                                          const float * __restrict __ATTR_ALIGN__(64) pN,
	                                          const float * __restrict __ATTR_ALIGN__(64) pk0,
	                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
	                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
	                                          const float * __restrict __ATTR_ALIGN__(64) pthti,
	                                          const float * __restrict __ATTR_ALIGN__(64) pthts,
	                                          const float * __restrict __ATTR_ALIGN__(64) pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	         
	         
	           __m512 rcs_vh_f9134_zmm16r4_u( const float * __restrict  pA,
	                                          const float * __restrict  pN,
	                                          const float * __restrict  pk0,
	                                          const float * __restrict  pepsr,
	                                          const float * __restrict  pepsi,
	                                          const float * __restrict  pthti,
	                                          const float * __restrict  pthts,
	                                          const float * __restrict  pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	         
	           /*
	             Model 9B4 (Peake's Model)
	             Model resembling many natural grass-like structures like
	             forests,grass,wheat fields, etc.
	             Bistatic RCS (hv) polarized per unit surface area.
	             Formula 9.1-35
	       
	       */
	       
	       
	        
	           __m512 rcs_hv_f9135_zmm16r4( const __m512 A,
	                                        const __m512 N,
	                                        const __m512 k0,
	                                        const __m512 epsr,
	                                        const __m512 epsi,
	                                        const __m512 thti,
	                                        const __m512 thts,
	                                        const __m512 phis,
	                                        const int pol) FUNC_ATTRIBUTES;
	         
	         
	         
	     
	           __m512 rcs_hv_f9135_zmm16r4_a(  const float * __restrict __ATTR_ALIGN__(64) pA,
	                                          const float * __restrict __ATTR_ALIGN__(64) pN,
	                                          const float * __restrict __ATTR_ALIGN__(64) pk0,
	                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
	                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
	                                          const float * __restrict __ATTR_ALIGN__(64) pthti,
	                                          const float * __restrict __ATTR_ALIGN__(64) pthts,
	                                          const float * __restrict __ATTR_ALIGN__(64) pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	         
	         
	           __m512  rcs_hv_f9135_zmm16r4_u(const float * __restrict pA,
	                                          const float * __restrict  pN,
	                                          const float * __restrict  pk0,
	                                          const float * __restrict  pepsr,
	                                          const float * __restrict  pepsi,
	                                          const float * __restrict  pthti,
	                                          const float * __restrict  pthts,
	                                          const float * __restrict  pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	         
	            /*
	             Model 9B4 (Peake's Model)
	             Model resembling many natural grass-like structures like
	             forests,grass,wheat fields, etc.
	             Bistatic RCS (vv) polarized per unit surface area.
	             Formula 9.1-36
	       
	       */
	       
	       
	        
	           __m512 rcs_vv_f9136_zmm16r4( const __m512 A,
	                                        const __m512 N,
	                                        const __m512 k0,
	                                        const __m512 epsr,
	                                        const __m512 epsi,
	                                        const __m512 thti,
	                                        const __m512 thts,
	                                        const __m512 phis,
	                                        const int pol) FUNC_ATTRIBUTES;
	         
	         
	         
	  
	           __m512 rcs_vv_f9136_zmm16r4_a( const float * __restrict __ATTR_ALIGN__(64) pA,
	                                          const float * __restrict __ATTR_ALIGN__(64) pN,
	                                          const float * __restrict __ATTR_ALIGN__(64) pk0,
	                                          const float * __restrict __ATTR_ALIGN__(64) pepsr,
	                                          const float * __restrict __ATTR_ALIGN__(64) pepsi,
	                                          const float * __restrict __ATTR_ALIGN__(64) pthti,
	                                          const float * __restrict __ATTR_ALIGN__(64) pthts,
	                                          const float * __restrict __ATTR_ALIGN__(64) pphis,
	                                          const int pol ) FUNC_ATTRIBUTES;
	         
	         
	       
	           __m512 rcs_vv_f9136_zmm16r4_u( const float * __restrict  pA,
	                                          const float * __restrict  pN,
	                                          const float * __restrict  pk0,
	                                          const float * __restrict  pepsr,
	                                          const float * __restrict  pepsi,
	                                          const float * __restrict  pthti,
	                                          const float * __restrict  pthts,
	                                          const float * __restrict  pphis,
	                                          const int pol ) FUNC_ATTRIBUTES;
	         
	         
	         
	        /*
	            Gaussian surface-height correlation
	            coefficient of average backscattering RCS 
	            per unit area.
	            RCS (vv) polarization.
	            Formula 9.1-74
	        */
	        
	         
	           __m512 rcs_vv_f9174_zmm16r4(const __m512 k0,
	                                       const __m512 h,
	                                       const __m512 l,
	                                       const __m512 thti,
	                                       const __m512 epsr,
	                                       const __m512 epsi,
	                                       const __m512 mur,
	                                       const __m512 mui) FUNC_ATTRIBUTES;
	       
	       
	           __m512 rcs_vv_f9174_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
	                                         const float * __restrict __ATTR_ALIGN__(64) ph,
	                                         const float * __restrict __ATTR_ALIGN__(64) pl,
	                                         const float * __restrict __ATTR_ALIGN__(64) pthti,
	                                         const float * __restrict __ATTR_ALIGN__(64) pepsr,
	                                         const float * __restrict __ATTR_ALIGN__(64) pepsi,
	                                         const float * __restrict __ATTR_ALIGN__(64) pmur,
	                                         const float * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;
	       
	       
	       
	           __m512 rcs_vv_f9174_zmm16r4_u(const float * __restrict  pk0,
	                                         const float * __restrict  ph,
	                                         const float * __restrict  pl,
	                                         const float * __restrict  pthti,
	                                         const float * __restrict  pepsr,
	                                         const float * __restrict  pepsi,
	                                         const float * __restrict  pmur,
	                                         const float * __restrict  pmui) FUNC_ATTRIBUTES;
	       
	        /*
	            Gaussian surface-height correlation
	            coefficient of average backscattering RCS 
	            per unit area.
	            RCS (hh) polarization.
	            Formula 9.1-75
	        */
	        
	        
	         
	           __m512 rcs_hh_f9175_zmm16r4(const __m512 k0,
	                                       const __m512 h,
	                                       const __m512 l,
	                                       const __m512 thti,
	                                       const __m512 epsr,
	                                       const __m512 epsi,
	                                       const __m512 mur,
	                                       const __m512 mui) FUNC_ATTRIBUTES;
	       
	         
	           __m512 rcs_hh_f9175_zmm16r4_a(  const float * __restrict __ATTR_ALIGN__(64) pk0,
	                                         const float * __restrict __ATTR_ALIGN__(64) ph,
	                                         const float * __restrict __ATTR_ALIGN__(64) pl,
	                                         const float * __restrict __ATTR_ALIGN__(64) pthti,
	                                         const float * __restrict __ATTR_ALIGN__(64) pepsr,
	                                         const float * __restrict __ATTR_ALIGN__(64) pepsi,
	                                         const float * __restrict __ATTR_ALIGN__(64) pmur,
	                                         const float * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;
	       
	         
	           __m512 rcs_hh_f9175_zmm16r4_u(  const float * __restrict  pk0,
	                                         const float * __restrict  ph,
	                                         const float * __restrict  pl,
	                                         const float * __restrict  pthti,
	                                         const float * __restrict  pepsr,
	                                         const float * __restrict  pepsi,
	                                         const float * __restrict  pmur,
	                                         const float * __restrict  pmui) FUNC_ATTRIBUTES;
	       
	         /*
	            Gaussian surface-height correlation
	            coefficient of average backscattering RCS 
	            per unit area.
	            RCS (hv) polarization.
	            Formula 9.1-76
	        */
	        
	        
	       
	           __m512 rcs_hv_f9176_zmm16r4() FUNC_ATTRIBUTES;
	         
	         
	         
	         /*
	            Exponential surface-height correlation
	            coefficient of average backscattering RCS 
	            per unit area.
	            RCS (vv) polarization.
	            Formula 9.1-77
	        */
	        
	        
	          
	           __m512 rcs_vv_f9177_zmm16r4(const __m512 k0,
	                                       const __m512 h,
	                                       const __m512 l,
	                                       const __m512 thti,
	                                       const __m512 epsr,
	                                       const __m512 epsi,
	                                       const __m512 mur,
	                                       const __m512 mui) FUNC_ATTRIBUTES;
	       
	       
	    
	           __m512 rcs_vv_f9177_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
	                                         const float * __restrict __ATTR_ALIGN__(64) ph,
	                                         const float * __restrict __ATTR_ALIGN__(64) pl,
	                                         const float * __restrict __ATTR_ALIGN__(64) pthti,
	                                         const float * __restrict __ATTR_ALIGN__(64) pepsr,
	                                         const float * __restrict __ATTR_ALIGN__(64) pepsi,
	                                         const float * __restrict __ATTR_ALIGN__(64) pmur,
	                                         const float * __restrict __ATTR_ALIGN__(64) pmui) 
	       
	       
	       
	        
	           __m512 rcs_vv_f9177_zmm16r4_u(const float * __restrict  pk0,
	                                         const float * __restrict  ph,
	                                         const float * __restrict  pl,
	                                         const float * __restrict  pthti,
	                                         const float * __restrict  pepsr,
	                                         const float * __restrict  pepsi,
	                                         const float * __restrict  pmur,
	                                         const float * __restrict  pmui) FUNC_ATTRIBUTES;
	       
	       
	         /*
	            Exponential surface-height correlation
	            coefficient of average backscattering RCS 
	            per unit area.
	            RCS (hh) polarization.
	            Formula 9.1-78
	        */
	        
	        
	        

	           __m512 rcs_hh_f9178_zmm16r4(const __m512 k0,
	                                       const __m512 h,
	                                       const __m512 l,
	                                       const __m512 thti,
	                                       const __m512 epsr,
	                                       const __m512 epsi,
	                                       const __m512 mur,
	                                       const __m512 mui) FUNC_ATTRIBUTES;
	       
	         
	           __m512 rcs_hh_f9178_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
	                                         const float * __restrict __ATTR_ALIGN__(64) ph,
	                                         const float * __restrict __ATTR_ALIGN__(64) pl,
	                                         const float * __restrict __ATTR_ALIGN__(64) pthti,
	                                         const float * __restrict __ATTR_ALIGN__(64) pepsr,
	                                         const float * __restrict __ATTR_ALIGN__(64) pepsi,
	                                         const float * __restrict __ATTR_ALIGN__(64) pmur,
	                                         const float * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;
	       
	       
	       
	           __m512 rcs_hh_f9178_zmm16r4_u(const float * __restrict  pk0,
	                                         const float * __restrict  ph,
	                                         const float * __restrict  pl,
	                                         const float * __restrict  pthti,
	                                         const float * __restrict  pepsr,
	                                         const float * __restrict  pepsi,
	                                         const float * __restrict  pmur,
	                                         const float * __restrict  pmui) FUNC_ATTRIBUTES;
	       
	         /*
	            Gaussian surface-height correlation
	            coefficient of average backscattering RCS 
	            per unit area.
	            RCS (hv) polarization.
	            Formula 9.1-79
	        */
	        
	        
	       
	           __m512 rcs_hv_f9179_zmm16r4() FUNC_ATTRIBUTES;
	         
	         
	         /*
	                Scattering matrix elements for
	                a perfectly conducting surface.
	                Formula: 9.1-80
	         */
	         
	         
	        
	           __m512 a_vv_f9180_zmm16r4(const __m512 thti,
	                                         const __m512 thts,
	                                         const __m512 phis) FUNC_ATTRIBUTES;
	          
	          
	          
	          
	           __m512 a_vv_f9180_zmm16r4_a(  const float * __restrict __ATTR_ALIGN__(64) pthti,
	                                         const float * __restrict __ATTR_ALIGN__(64) pthts,
	                                         const float * __restrict __ATTR_ALIGN__(64) pphis) FUNC_ATTRIBUTES;
	          
	          
	         
	           __m512 a_vv_f9180_zmm16r4_u(  const float * __restrict  pthti,
	                                         const float * __restrict  pthts,
	                                         const float * __restrict  pphis) FUNC_ATTRIBUTES;
	          
	          /*
	                Scattering matrix elements for
	                a perfectly conducting surface.
	                Formula: 9.1-81
	         */
	         
	         
	        
	           __m512 a_hv_f9181_zmm16r4(const __m512 phis,
	                                     const __m512 thti) FUNC_ATTRIBUTES;
	         
	         
	        
	           __m512 a_hv_f9181_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pphis,
	                                     const float * __restrict __ATTR_ALIGN__(64) pthti) FUNC_ATTRIBUTES;
	          
	       
	        
	     
	           __m512 a_hv_f9181_zmm16r4_u(const float * __restrict pphis,
	                                     const float * __restrict  pthti) FUNC_ATTRIBUTES;
	         
	         
	            /*
	                Scattering matrix elements for
	                a perfectly conducting surface.
	                Formula: 9.1-82
	         */
	         
	         
	       
	           __m512 a_hh_f9182_zmm16r4(const __m512 phis) FUNC_ATTRIBUTES;
	          
	          
	         
	           __m512 a_hh_f9182_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pphis) FUNC_ATTRIBUTES;
	        
	       
	        
	           __m512 a_hh_f9182_zmm16r4_u(const float * __restrict  pphis) FUNC_ATTRIBUTES;
	          
	          
	             /*
	                Scattering matrix elements for
	                a perfectly conducting surface.
	                Formula: 9.1-83
	         */
	         
	         
	      
	           __m512 a_vh_f9183_zmm16r4(const __m512 phis,
	                                     const __m512 thts) FUNC_ATTRIBUTES;
	         
	         
	         
	         
	           __m512 a_vh_f9183_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pphis,
	                                       const float * __restrict __ATTR_ALIGN__(64) pthts) FUNC_ATTRIBUTES;
	         
	       
	           __m512 a_vh_f9183_zmm16r4_u(const float * __restrict  pphis,
	                                       const float * __restrict pthts) FUNC_ATTRIBUTES;
	         
	         
	         /*
	              Backscattering from a perfectly conducting surface
	              Theta (inc) == theta (scat) , phi (scat) = 180 (grad).
	              Formula: 9.1-84 
	         */
	         
	         
	       
	           __m512 a_vv_f9184_zmm16r4(const __m512 thti) FUNC_ATTRIBUTES;
	           
	           
	      
	           __m512 a_vv_f9184_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pthti) FUNC_ATTRIBUTES;
	           
	           
	        
	           __m512 a_vv_f9184_zmm16r4_u(const float * __restrict  pthti) FUNC_ATTRIBUTES;
	           
	           
	           
	         /*
	              Backscattering from a perfectly conducting surface
	              Theta (inc) == theta (scat) , phi (scat) = 180 (grad).
	              Formula: 9.1-85
	         */                  
	         
	         
	      
	           __m512 a_hh_f9185_zmm16r4() FUNC_ATTRIBUTES;
	           
	           
	            /*
	              Backscattering from a perfectly conducting surface
	              Theta (inc) == theta (scat) , phi (scat) = 180 (grad).
	              Formula: 9.1-86
	         */   
	         
	         
	      
	           __m512 a_vh_f9186_zmm16r4() FUNC_ATTRIBUTES;
	           
	           
	         /*
	              Backscattering from a perfectly conducting surface
	              Theta (inc) == theta (scat) , phi (scat) = 180 (grad).
	              Average backscattering RCS per unit area.
	              Gaussian surface height correlation coefficient.
	              Formula: 9.1-87
	         */
	         
	         
	         
	       
	           __m512 rcs_vv_f9187_zmm16r4(const __m512 k0,
	                                       const __m512 h,
	                                       const __m512 l,
	                                       const __m512 thti) FUNC_ATTRIBUTES;
	         
	           
	           
	           
	        
	           __m512 rcs_vv_f9187_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
	                                       const float * __restrict __ATTR_ALIGN__(64) ph,
	                                       const float * __restrict __ATTR_ALIGN__(64) pl,
	                                       const float * __restrict __ATTR_ALIGN__(64) pthti) FUNC_ATTRIBUTES;
	           
	            
	       
	           __m512 rcs_vv_f9187_zmm16r4_u(const float * __restrict  pk0,
	                                         const float * __restrict  ph,
	                                         const float * __restrict  pl,
	                                         const float * __restrict  pthti) FUNC_ATTRIBUTES;
	         
	         
	          /*
	              Backscattering from a perfectly conducting surface
	              Theta (inc) == theta (scat) , phi (scat) = 180 (grad).
	              Average backscattering RCS per unit area.
	              Gaussian surface height correlation coefficient.
	              Formula: 9.1-88
	         */
	         
	         
	      
	           __m512 rcs_hh_f9188_zmm16r4(const __m512 k0,
	                                       const __m512 h,
	                                       const __m512 l,
	                                       const __m512 thti) FUNC_ATTRIBUTES;
	         
	         
	        
	           __m512 rcs_hh_f9188_zmm16r4_a(const float * __restrict __ATTR_ALIGN__(64) pk0,
	                                       const float * __restrict __ATTR_ALIGN__(64) ph,
	                                       const float * __restrict __ATTR_ALIGN__(64) pl,
	                                       const float * __restrict __ATTR_ALIGN__(64) pthti) FUNC_ATTRIBUTES;
	         }
	         
	         
	       
	           __m512 rcs_hh_f9188_zmm16r4_u(const float * __restrict  pk0,
	                                       const float * __restrict  ph,
	                                       const float * __restrict  pl,
	                                       const float * __restrict  pthti) FUNC_ATTRIBUTES;
	         
	         
	         /*
	              
	              Backscattering from a perfectly conducting surface
	              Theta (inc) == theta (scat) , phi (scat) = 180 (grad).
	              Average backscattering RCS per unit area.
	              Gaussian surface height correlation coefficient.
	              Formula: 9.1-89
	           
	         */
	         
	         
	        
	           __m512 rcs_vhhv_f9189_zmm16r4() FUNC_ATTRIBUTES;
	          
	          
	          
	           
	      
	                                    
	        
                 
                 
                 
               
               
         
    




















#endif /*__GMS_RCS_COMPLEX_OBJECTS_ZMM16R4_H__*/
