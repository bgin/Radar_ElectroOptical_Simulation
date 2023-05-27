

#ifndef __GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_H__
#define __GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_H__

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



    const unsigned int GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_MAJOR = 1U;
    const unsigned int GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_MINOR = 0U;
    const unsigned int GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_MICRO = 0U;
    const unsigned int GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_FULLVER =
      1000U*GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_MAJOR+
      100U*GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_MINOR+
      10U*GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_MICRO;
    const char * const GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_CREATION_DATE = "11-05-2023 10:53 PM +00200 (THR 11 MAY 2023 GMT+2)";
    const char * const GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_BUILD_DATE    = __DATE__ ":" __TIME__;
    const char * const GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_AUTHOR        = "Programmer: Bernard Gingold, contact: beniekg@gmail.com";
    const char * const GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_DESCRIPTION   = "AVX512 optimized Complex Surfaces Radar Cross Section functionality.";




#include <immintrin.h>
#include "GMS_kernel_config.h"






         
         
               /*
                   Work (input) arrays for kernel rcs_f8162_zmm8r8_2t_u and
                   rcs_f8162_zmm8r8_2t_a.
               */
               __ATTR_ALIGN__(64) struct RCS_F8162_DATA {
               
                       double * __restrict  Ya1; 
                       double * __restrict  Ya2; 
                       double * __restrict  Ya3; 
                       double * __restrict  Ea;  
                       double * __restrict  WRKa; 
                       double * __restrict  Yb1;
                       double * __restrict  Yb2; 
                       double * __restrict  Yb3; 
                       double * __restrict  Eb; 
                       double * __restrict  WRKb;  
               };
         
              
              /*
                    Surface discontinuities.
                    General perfectly conducting convex edge.
                    Backscatter diffraction coefficient 'D'.
                    Formula: 8.1-21
              */     
              
              
               
                   void coef_D12_f8121_zmm8r8(const __m512d gam,
                                             const __m512d phi,
                                             const __m512d k0,
                                             __m512d * __restrict D1r,
                                             __m512d * __restrict D1i,
                                            __m512d * __restrict D2r,
                                            __m512d * __restrict D2i) FUNC_ATTRIBUTES;
                
                
                
                 
                   void coef_D12_f8121_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam,
                                               const double * __restrict __ATTR_ALIGN__(64) pphi,
                                               const double * __restrict __ATTR_ALIGN__(64) pk0,
                                               double * __restrict __ATTR_ALIGN__(64) D1r,
                                               double * __restrict __ATTR_ALIGN__(64) D1i,
                                               double * __restrict __ATTR_ALIGN__(64) D2r,
                                               double * __restrict __ATTR_ALIGN__(64) D2i) FUNC_ATTRIBUTES;
                     
                
                
                
                 
                   void coef_D12_f8121_zmm8r8_u(const double * __restrict  pgam,
                                               const double * __restrict  pphi,
                                               const double * __restrict  pk0,
                                               double * __restrict  D1r,
                                               double * __restrict  D1i,
                                               double * __restrict  D2r,
                                               double * __restrict  D2i) FUNC_ATTRIBUTES;
                
                
                /*
                    Surface discontinuities.
                    General perfectly conducting convex edge.
                    Backscatter singly diffracted far-zone fields (E,H).
                    Formula: 8.1-19, 8.1-20
                
                */
                
                
                 
                   void EsHs_f811920_zmm8r8(    const __m512d betai,
                                                 const __m512d betas,
                                                 const __m512d gam,
                                                 const __m512d phi,
                                                 const __m512d k0,
                                                 const __m512d r,
                                                 const __m512d rho,
                                                 const __m512d psi,
                                                 __m512d * __restrict Esr,
                                                 __m512d * __restrict Esi,
                                                 __m512d * __restrict Hsr,
                                                 __m512d * __restrict Hsi) FUNC_ATTRIBUTES;
            
            
               
                   void EsHs_f811920_zmm8r8_a(  const double * __restrict __ATTR_ALIGN__(64) pbetai,
                                                 const double * __restrict __ATTR_ALIGN__(64) pbetas,
                                                 const double * __restrict __ATTR_ALIGN__(64) pgam,
                                                 const double * __restrict __ATTR_ALIGN__(64) pphi,
                                                 const double * __restrict __ATTR_ALIGN__(64) pk0,
                                                 const double * __restrict __ATTR_ALIGN__(64) pr,
                                                 const double * __restrict __ATTR_ALIGN__(64) prho,
                                                 const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                                 double * __restrict __ATTR_ALIGN__(64) Esr,
                                                 double * __restrict __ATTR_ALIGN__(64) Esi,
                                                 double * __restrict __ATTR_ALIGN__(64) Hsr,
                                                 double * __restrict __ATTR_ALIGN__(64) Hsi) FUNC_ATTRIBUTES;
            
                
                   void EsHs_f811920_zmm8r8_u(  const double * __restrict  pbetai,
                                                 const double * __restrict  pbetas,
                                                 const double * __restrict  pgam,
                                                 const double * __restrict  pphi,
                                                 const double * __restrict  pk0,
                                                 const double * __restrict  pr,
                                                 const double * __restrict  prho,
                                                 const double * __restrict ppsi,
                                                 double * __restrict  Esr,
                                                 double * __restrict  Esi,
                                                 double * __restrict  Hsr,
                                                 double * __restrict  Hsi) FUNC_ATTRIBUTES;
            
            
            /*
                    Surface discontinuities.
                    General perfectly conducting convex edge.
                    Backscatter diffraction coefficient 'D'.
                    Ray normal-incidence to one of edge faces.
                    Formula: 8.1-24
            */
            
                 
                   void coef_D12_f8124_zmm8r8(const __m512d k0,
                                               const __m512d gam,
                                               __m512d * __restrict D1r,
                                               __m512d * __restrict D1i,
                                               __m512d * __restrict D2r,
                                               __m512d * __restrict D2i) FUNC_ATTRIBUTES;
                
                
                
                   void coef_D12_f8124_zmm8r8_a(const  double * __restrict __ATTR_ALIGN__(64) pk0,
                                                 const  double * __restrict __ATTR_ALIGN__(64) pgam,
                                                 double * __restrict __ATTR_ALIGN__(64) D1r,
                                                 double * __restrict __ATTR_ALIGN__(64) D1i,
                                                 double * __restrict __ATTR_ALIGN__(64) D2r,
                                                 double * __restrict __ATTR_ALIGN__(64) D2i) FUNC_ATTRIBUTES;
                
                
                 
                   void coef_D12_f8124_zmm8r8_u(const  double * __restrict  pk0,
                                                 const  double * __restrict pgam,
                                                 double * __restrict  D1r,
                                                 double * __restrict  D1i,
                                                 double * __restrict  D2r,
                                                 double * __restrict  D2i) FUNC_ATTRIBUTES;
                
                /*
                    Surface discontinuities.
                    General perfectly conducting convex edge.
                    Backscatter diffraction coefficient 'D'.
                    Backscatter direction axial caustic (for slightly diffracted rays).
                    Formula: 8.1-26
                */
                
                
                
                   void coef_Ddiff_f8126_zmm8r8(const __m512d gam,
                                             const __m512d phi,
                                             const __m512d k0,
                                             __m512d * __restrict Dr,
                                             __m512d * __restrict Di) FUNC_ATTRIBUTES;
                
                
               
                   void coef_Ddiff_f8126_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pgam,
                                               const double * __restrict __ATTR_ALIGN__(64) pphi,
                                               const double * __restrict __ATTR_ALIGN__(64) pk0,
                                               double * __restrict __ATTR_ALIGN__(64)  Dr,
                                               double * __restrict __ATTR_ALIGN__(64)  Di) FUNC_ATTRIBUTES;
                
               
                   void coef_Ddiff_f8126_zmm8r8_u(const double * __restrict  pgam,
                                                   const double * __restrict pphi,
                                                   const double * __restrict  pk0,
                                                   double * __restrict  Dr,
                                                   double * __restrict  Di) FUNC_ATTRIBUTES;
                
                
                   /*
                    Surface discontinuities.
                    General perfectly conducting convex edge.
                    Backscatter diffraction coefficient 'D'.
                    Backscatter direction axial caustic (for slightly diffracted rays).
                    Scattered Electric and Magnetic fields.
                    Formula: 8.1-25
                */
                
                
           
                   void EsHs_f8125_zmm8r8(const __m512d a,
                                           const __m512d k0,
                                           const __m512d r,
                                           const __m512d gam,
                                           const __m512d phi,
                                           const __m512d psi,
                                           __m512d * __restrict Esr,
                                           __m512d * __restrict Esi,
                                           __m512d * __restrict Hsr,
                                           __m512d * __restrict Hsi) FUNC_ATTRIBUTES;
                
           
                   void EsHs_f8125_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pa,
                                             const double * __restrict __ATTR_ALIGN__(64) pk0,
                                             const double * __restrict __ATTR_ALIGN__(64) pr,
                                             const double * __restrict __ATTR_ALIGN__(64) pgam,
                                             const double * __restrict __ATTR_ALIGN__(64) pphi,
                                             const double * __restrict __ATTR_ALIGN__(64) ppsi,
                                             double * __restrict __ATTR_ALIGN__(64) Esr,
                                             double * __restrict __ATTR_ALIGN__(64) Esi,
                                             double * __restrict __ATTR_ALIGN__(64) Hsr,
                                             double * __restrict __ATTR_ALIGN__(64) Hsi) FUNC_ATTRIBUTES;
                
            
              
                   void EsHs_f8125_zmm8r8_u(const double * __restrict  pa,
                                             const double * __restrict  pk0,
                                             const double * __restrict  pr,
                                             const double * __restrict pgam,
                                             const double * __restrict  pphi,
                                             const double * __restrict  ppsi,
                                             double * __restrict  Esr,
                                             double * __restrict  Esi,
                                             double * __restrict  Hsr,
                                             double * __restrict  Hsi) FUNC_ATTRIBUTES;
                
                
                /*
                    Surface discontinuities.
                    General perfectly conducting convex edge.
                    Backscatter diffraction coefficient 'D'.
                    Doubly and high order diffracted rays --
                    bistatic diffraction coefficients.
                    Formula: 8.1-27  
                
                */
                
                
                
                   void coef_D12_f8127_zmm8r8(const __m512d k0,
                                               const __m512d gam,
                                               const __m512d phi1,
                                               const __m512d phi2,
                                               __m512d * __restrict D1r,
                                               __m512d * __restrict D1i,
                                               __m512d * __restrict D2r,
                                               __m512d * __restrict D2i) FUNC_ATTRIBUTES;
                
                
                 
                   void coef_D12_f8127_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                                 const double * __restrict __ATTR_ALIGN__(64) pgam,
                                                 const double * __restrict __ATTR_ALIGN__(64) pphi1,
                                                 const double * __restrict __ATTR_ALIGN__(64) pphi2,
                                                 double * __restrict __ATTR_ALIGN__(64)  D1r,
                                                 double * __restrict __ATTR_ALIGN__(64)  D1i,
                                                 double * __restrict __ATTR_ALIGN__(64)  D2r,
                                                 double * __restrict __ATTR_ALIGN__(64)  D2i) FUNC_ATTRIBUTES;
                
                
                
                   void coef_D12_f8127_zmm8r8_u(const double * __restrict  pk0,
                                                 const double * __restrict  pgam,
                                                 const double * __restrict  pphi1,
                                                 const double * __restrict  pphi2,
                                                 double * __restrict   D1r,
                                                 double * __restrict  D1i,
                                                 double * __restrict   D2r,
                                                 double * __restrict  D2i) FUNC_ATTRIBUTES;
                
                /*
                       Adachi expression for axial-incidence
                       of backscatter RCS for entire scatterer length.
                       Shall be used in case of thin long axially symetric 
                       bodies e.g. 'ogives,double-cones, etc.,'
                       Vectorization of an integrand.
                       Formula 8.1-62
                */


                 
               
                   double rcs_f8162_zmm8r8_u(const double * __restrict pdAdl,
                                             const double *  __restrict pdl,
                                             const double   k0,
                                             const double   l) FUNC_ATTRIBUTES;
                  
                  
              
                   double rcs_f8162_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pdAdl,
                                             const double * __restrict __ATTR_ALIGN__(64) pdl,
                                             const double   k0,
                                             const double   l) FUNC_ATTRIBUTES;
                  
                  
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
                
               
                   double rcs_f8162_zmm8r8_avint_u(const double * __restrict pdAdl,
                                                   const double *  __restrict pdl,
                                                   const double   k0,
                                                   const double   l,
                                                   int32_t * __restrict ierr,
                                                   int32_t * __restrict ieri) FUNC_ATTRIBUTES;
                  
                  
                
                   double rcs_f8162_zmm8r8_avint_a(const double * __restrict __ATTR_ALIGN__(64) pdAdl,
                                                   const double * __restrict __ATTR_ALIGN__(64) pdl,
                                                   const double   k0,
                                                   const double   l,
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
                
                
               
                   double rcs_f8162_zmm8r8_u(const double * __restrict  pdAdl,
                                             const double * __restrict  pdl,
                                             double * __restrict  intr,
                                             double * __restrict  inti,
                                             double * __restrict  Y1,
                                             double * __restrict  Y2,
                                             double * __restrict  Y3,
                                             double * __restrict  E,
                                             double * __restrict  WRK
                                             const double   k0,
                                             const double   l,
                                             const int32_t NTAB) FUNC_ATTRIBUTES;
               
                 
                   double rcs_f8162_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pdAdl,
                                             const double * __restrict __ATTR_ALIGN__(64) pdl,
                                             double * __restrict __ATTR_ALIGN__(64) intr,
                                             double * __restrict __ATTR_ALIGN__(64) inti,
                                             double * __restrict __ATTR_ALIGN__(64) Y1,
                                             double * __restrict __ATTR_ALIGN__(64) Y2,
                                             double * __restrict __ATTR_ALIGN__(64) Y3,
                                             double * __restrict __ATTR_ALIGN__(64) E,
                                             double * __restrict __ATTR_ALIGN__(64) WRK
                                             const double   k0,
                                             const double   l,
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
                
                
                
                   double rcs_f8162_zmm8r8_avint_u(const double * __restrict  pdAdl,
                                                   const double * __restrict  pdl,
                                                   double * __restrict  intr,
                                                   double * __restrict  inti,
                                                   const double   k0,
                                                   const double   l,
                                                   int32_t * __restrict ierr,
                                                   int32_t * __restrict ieri,
                                                   const int32_t NTAB) FUNC_ATTRIBUTES;
               
                   double rcs_f8162_zmm8r8_avint_a(const double * __restrict __ATTR_ALIGN__(64) pdAdl,
                                                   const double * __restrict __ATTR_ALIGN__(64) pdl,
                                                   double * __restrict __ATTR_ALIGN__(64) intr,
                                                   double * __restrict __ATTR_ALIGN__(64) inti,
                                                   const double   k0,
                                                   const double   l,
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
                

    
                
                
                   double rcs_f8162_zmm8r8_cspint2t_u(const double * __restrict  pdAdl,
                                                     const double * __restrict  pdl,
                                                     double * __restrict  intr,
                                                     double * __restrict  inti,
                                                     struct RCS_F8162_DATA w,
                                                     const double   k0,
                                                     const double   l,
                                                     const int32_t NTAB) FUNC_ATTRIBUTES;
               
               
                
                   double rcs_f8162_zmm8r8_cspint2t_a(const double * __restrict __ATTR_ALIGN__(64) pdAdl,
                                                     const double * __restrict __ATTR_ALIGN__(64) pdl,
                                                     double * __restrict __ATTR_ALIGN__(64) intr,
                                                     double * __restrict __ATTR_ALIGN__(64) inti,
                                                     struct RCS_F8162_DATA w,
                                                     const double   k0,
                                                     const double   l,
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
                
                
               
                   double rcs_f8162_zmm8r8_avint2t_u(const double * __restrict  pdAdl,
                                                     const double * __restrict  pdl,
                                                     double * __restrict  intr,
                                                     double * __restrict  inti,
                                                     int32_t * __restrict ierr,
                                                     int32_t * __restrict ieri,
                                                     const double   k0,
                                                     const double   l,
                                                     const int32_t NTAB) FUNC_ATTRIBUTES;
               
                
                   double rcs_f8162_zmm8r8_avint2t_a(const double * __restrict __ATTR_ALIGN__(64) pdAdl,
                                                     const double * __restrict __ATTR_ALIGN__(64) pdl,
                                                     double * __restrict __ATTR_ALIGN__(64) intr,
                                                     double * __restrict __ATTR_ALIGN__(64) inti,
                                                     int32_t * __restrict ierr,
                                                     int32_t * __restrict ieri,
                                                     const double   k0,
                                                     const double   l,
                                                     const int32_t NTAB) 
               
               
               
               
               
               /*
                     High frequency approximations.
                     Rounded-tip cone total nose-on
                     backscatter RCS.
                     Formula 8.1-93
               */
               
               
               
                   __m512d rcs_f8193_zmm8r8(const __m512d b,
                                            const __m512d a,
                                            const __m512d k0,
                                            const __m512d alp,
                                            const __m512d l) FUNC_ATTRIBUTES;
                 
                 
                 
               
                   __m512d rcs_f8193_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pb,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) palp,
                                              const double * __restrict __ATTR_ALIGN__(64) pl) FUNC_ATTRIBUTES;
                 
                 
             
                   __m512d rcs_f8193_zmm8r8_u(const double * __restrict  pb,
                                              const double * __restrict  pa,
                                              const double * __restrict  pk0,
                                              const double * __restrict  palp,
                                              const double * __restrict _pl) FUNC_ATTRIBUTES;
                 
                 
                 /*
                     High frequency approximations.
                     Backscatter RCS of conical frustum
                     for |theta| = PI/2-alpha
                     Formula 8.1-96
                 */
                 
                 
                 
                   __m512d rcs_f8196_zmm8r8(const __m512d k0,
                                            const __m512d alp,
                                            const __m512d a,
                                            const __m512d b) FUNC_ATTRIBUTES;
                 
                 
               
                   __m512d rcs_f8196_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
                                              const double * __restrict __ATTR_ALIGN__(64) palp,
                                              const double * __restrict __ATTR_ALIGN__(64) pa,
                                              const double * __restrict __ATTR_ALIGN__(64) pb) FUNC_ATTRIBUTES;
                 
                 
              
                   __m512d rcs_f8196_zmm8r8_u(const double * __restrict pk0,
                                              const double * __restrict palp,
                                              const double * __restrict pa,
                                              const double * __restrict pb) FUNC_ATTRIBUTES;
                 
                 
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
	           __m512d rcs_perpendic_f8194_zmm8r8(const __m512d h,
	                                        const __m512d l,
	                                        const __m512d b,
	                                        const __m512d a,
	                                        const __m512d k0,
	                                        const __m512d tht,
	                                        const __m512d alp) {
	                                 
	                                  
	                 const __m512d C314159265358979323846264338328  = 
                                                     _mm512_set1_ps(3.14159265358979323846264338328f); 
                         const __m512d C1772453850905516027298167483341 = 
                                                     _mm512_set1_ps(1.772453850905516027298167483341f);
                         const __m512d C078539816339744830961566084582  = 
                                                     _mm512_set1_ps(0.78539816339744830961566084582f);
                         const __m512d C10                              = 
                                                     _mm512_set1_ps(1.0f);  
                         const __m512d C15                              = 
                                                     _mm512_set1_ps(1.5f); 
                         const __m512d C05                              = 
                                                     _mm512_set1_ps(0.5f);
                         const __m512d C20                              =
                                                     _mm512_set1_ps(2.0f);
                         register __m512d pin,n,invn,spin,cos1,k02,cos2,sint,cost;
                         register __m512d ear,eai1,eai2,eai3,cer1,cei1,sk02,sacs;
                         register __m512d cer2,cei2,cer3,cei3,x0,x1,x2,x3,atant;
                         register __m512d cpin1,cpin2,trm1,trm2,rcs;
                         __m512d t0r,t0i,t1r,t1i,a2;
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
                         cexp_zmm8r8(ear,eai,&cer1,&cei1);
                         cer1 = _mm512_mul_ps(x0,cer1);
                         spin = xsinf(pin);
                         cei1 = _mm512_mul_ps(x0,cei1);
                         cpin = xcosf(pin);
                         x1   = _mm512_mul_ps(_mm512_sub_ps(h,atant),cost);
                         eai2 = _mm512_fmadd_ps(k02,x1,C078539816339744830961566084582);
                         cexp_zmm8r8(ear,eai2,&cer2,&cei);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_mul_ps(x0,sacs);
                         cer2 = _mm512_mul_ps(cer2,x1);
                         cei2 = _mm512_mul_ps(cei2,x1);
                         cpin1= _mm512_rcp14_ps(_mm512_sub_ps(cpin,C10));
                         x2   = _mm512_mul_ps(C20,_mm512_add_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x2,invn));
                         x3   = _mm512_rcp14_ps(_mm512_sub_ps(cpin,cpin2));
                         trm1 = _mm512_sub_ps(cpin1,x3);
                         cmul_zmm8r8(cer1,cei1,cer2,cei2,&t0r,&t0i);
                         t0r  = _mm512_mul_ps(t0r,trm1);
                         t0i  = _mm512_mul_ps(t0i,trm1);
                         x0   = _mm512_mul_ps(C20,_mm512_sub_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x0,invn));
                         x1   = _mm512_rcp14_ps(cpin2);
                         trm2 = _mm512_sub_ps(cpin1,x1);
                         x2   = _mm512_fmadd_ps(cost,_mm512_mul_ps(k02,
                                                               _mm512_add_ps(h,atant)));
                         eai3 = _mm512_add_ps(C078539816339744830961566084582,x2);
                         cexp_zmm8r8(ear,ea3,&cer3,&cei3);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_sqrt_ps(_mm512_mul_ps(gms::math::
                                                                  negate_zmm8r8(a2),csct));
                         x2   = _mm512_mul_ps(x0,x1);
                         cer3 = _mm512_mul_ps(_mm512_mul_ps(cer3,x2),trm2);
                         cei3 = _mm512_mul_ps(_mm512_mul_ps(cei3,x2),trm2);
                         cmul_zmm8r8(t0r,t0i,cer3,cei3,&t1r,&t1i);
                         rcs  = cabs_zmm8r8(t1r,t1i);
                         return (rcs);
	        }
	        
	        
	        
	                                  
                    __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
	           __m512d rcs_perpendic_f8194_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) ph,
	                                         const double * __restrict __ATTR_ALIGN__(64) pl,
	                                         const double * __restrict __ATTR_ALIGN__(64) pb,
	                                         const double * __restrict __ATTR_ALIGN__(64) pa,
	                                         const double * __restrict __ATTR_ALIGN__(64) pk0,
	                                         const double * __restrict __ATTR_ALIGN__(64) ptht,
	                                         const double * __restrict __ATTR_ALIGN__(64) palp) {
	                                 
	                  
	                 register __m512d h  = _mm512_load_ps(&ph[0]);
	                 register __m512d l  = _mm512_load_ps(&pl[0]); 
	                 register __m512d b  = _mm512_load_ps(&pb[0]);   
	                 register __m512d a  = _mm512_load_ps(&pa[0]);  
	                 register __m512d k0 = _mm512_load_ps(&pk0[0]);
	                 register __m512d tht= _mm512_load_ps(&ptht[0]); 
	                 register __m512d alp= _mm512_load_ps(&palp[0]);        
	                 const __m512d C314159265358979323846264338328  = 
                                                     _mm512_set1_ps(3.14159265358979323846264338328f); 
                         const __m512d C1772453850905516027298167483341 = 
                                                     _mm512_set1_ps(1.772453850905516027298167483341f);
                         const __m512d C078539816339744830961566084582  = 
                                                     _mm512_set1_ps(0.78539816339744830961566084582f);
                         const __m512d C10                              = 
                                                     _mm512_set1_ps(1.0f);  
                         const __m512d C15                              = 
                                                     _mm512_set1_ps(1.5f); 
                         const __m512d C05                              = 
                                                     _mm512_set1_ps(0.5f);
                         const __m512d C20                              =
                                                     _mm512_set1_ps(2.0f);
                         __m512d pin,n,invn,spin,cos1,k02,cos2,sint,cost;
                         register __m512d ear,eai1,eai2,eai3,cer1,cei1,sk02,sacs;
                         register __m512d cer2,cei2,cer3,cei3,x0,x1,x2,x3,atant;
                         register __m512d cpin1,cpin2,trm1,trm2,rcs;
                         __m512d t0r,t0i,t1r,t1i,a2;
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
                         cexp_zmm8r8(ear,eai,&cer1,&cei1);
                         cer1 = _mm512_mul_ps(x0,cer1);
                         spin = xsinf(pin);
                         cei1 = _mm512_mul_ps(x0,cei1);
                         cpin = xcosf(pin);
                         x1   = _mm512_mul_ps(_mm512_sub_ps(h,atant),cost);
                         eai2 = _mm512_fmadd_ps(k02,x1,C078539816339744830961566084582);
                         cexp_zmm8r8(ear,eai2,&cer2,&cei);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_mul_ps(x0,sacs);
                         cer2 = _mm512_mul_ps(cer2,x1);
                         cei2 = _mm512_mul_ps(cei2,x1);
                         cpin1= _mm512_rcp14_ps(_mm512_sub_ps(cpin,C10));
                         x2   = _mm512_mul_ps(C20,_mm512_add_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x2,invn));
                         x3   = _mm512_rcp14_ps(_mm512_sub_ps(cpin,cpin2));
                         trm1 = _mm512_sub_ps(cpin1,x3);
                         cmul_zmm8r8(cer1,cei1,cer2,cei2,&t0r,&t0i);
                         t0r  = _mm512_mul_ps(t0r,trm1);
                         t0i  = _mm512_mul_ps(t0i,trm1);
                         x0   = _mm512_mul_ps(C20,_mm512_sub_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x0,invn));
                         x1   = _mm512_rcp14_ps(cpin2);
                         trm2 = _mm512_sub_ps(cpin1,x1);
                         x2   = _mm512_fmadd_ps(cost,_mm512_mul_ps(k02,
                                                               _mm512_add_ps(h,atant)));
                         eai3 = _mm512_add_ps(C078539816339744830961566084582,x2);
                         cexp_zmm8r8(ear,ea3,&cer3,&cei3);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_sqrt_ps(_mm512_mul_ps(gms::math::
                                                                  negate_zmm8r8(a2),csct));
                         x2   = _mm512_mul_ps(x0,x1);
                         cer3 = _mm512_mul_ps(_mm512_mul_ps(cer3,x2),trm2);
                         cei3 = _mm512_mul_ps(_mm512_mul_ps(cei3,x2),trm2);
                         cmul_zmm8r8(t0r,t0i,cer3,cei3,&t1r,&t1i);
                         rcs  = cabs_zmm8r8(t1r,t1i);
                         return (rcs);
	        }
	        
	        
	        
	           __ATTR_ALWAYS_INLINE__
	           __ATTR_HOT__
	           __ATTR_ALIGN__(32)
                   __ATTR_VECTORCALL__
	           static inline
	           __m512d rcs_perpendic_f8194_zmm8r8_u(    const double * __restrict  ph,
	                                                    const double * __restrict  pl,
	                                                    const double * __restrict  pb,
	                                                    const double * __restrict  pa,
	                                                    const double * __restrict  pk0,
	                                                    const double * __restrict  ptht,
	                                                    const double * __restrict  palp) {
	                                 
	                  
	                 register __m512d h  = _mm512_loadu_ps(&ph[0]);
	                 register __m512d l  = _mm512_loadu_ps(&pl[0]); 
	                 register __m512d b  = _mm512_loadu_ps(&pb[0]);   
	                 register __m512d a  = _mm512_loadu_ps(&pa[0]);  
	                 register __m512d k0 = _mm512_loadu_ps(&pk0[0]);
	                 register __m512d tht= _mm512_loadu_ps(&ptht[0]); 
	                 register __m512d alp= _mm512_loadu_ps(&palp[0]);        
	                 const __m512d C314159265358979323846264338328  = 
                                                     _mm512_set1_ps(3.14159265358979323846264338328f); 
                         const __m512d C1772453850905516027298167483341 = 
                                                     _mm512_set1_ps(1.772453850905516027298167483341f);
                         const __m512d C078539816339744830961566084582  = 
                                                     _mm512_set1_ps(0.78539816339744830961566084582f);
                         const __m512d C10                              = 
                                                     _mm512_set1_ps(1.0f);  
                         const __m512d C15                              = 
                                                     _mm512_set1_ps(1.5f); 
                         const __m512d C05                              = 
                                                     _mm512_set1_ps(0.5f);
                         const __m512d C20                              =
                                                     _mm512_set1_ps(2.0f);
                         __m512d pin,n,invn,spin,cos1,k02,cos2,sint,cost;
                         register __m512d ear,eai1,eai2,eai3,cer1,cei1,sk02,sacs;
                         register __m512d cer2,cei2,cer3,cei3,x0,x1,x2,x3,atant;
                         register __m512d cpin1,cpin2,trm1,trm2,rcs;
                         __m512d t0r,t0i,t1r,t1i,a2;
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
                         cexp_zmm8r8(ear,eai,&cer1,&cei1);
                         cer1 = _mm512_mul_ps(x0,cer1);
                         spin = xsinf(pin);
                         cei1 = _mm512_mul_ps(x0,cei1);
                         cpin = xcosf(pin);
                         x1   = _mm512_mul_ps(_mm512_sub_ps(h,atant),cost);
                         eai2 = _mm512_fmadd_ps(k02,x1,C078539816339744830961566084582);
                         cexp_zmm8r8(ear,eai2,&cer2,&cei);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_mul_ps(x0,sacs);
                         cer2 = _mm512_mul_ps(cer2,x1);
                         cei2 = _mm512_mul_ps(cei2,x1);
                         cpin1= _mm512_rcp14_ps(_mm512_sub_ps(cpin,C10));
                         x2   = _mm512_mul_ps(C20,_mm512_add_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x2,invn));
                         x3   = _mm512_rcp14_ps(_mm512_sub_ps(cpin,cpin2));
                         trm1 = _mm512_sub_ps(cpin1,x3);
                         cmul_zmm8r8(cer1,cei1,cer2,cei2,&t0r,&t0i);
                         t0r  = _mm512_mul_ps(t0r,trm1);
                         t0i  = _mm512_mul_ps(t0i,trm1);
                         x0   = _mm512_mul_ps(C20,_mm512_sub_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x0,invn));
                         x1   = _mm512_rcp14_ps(cpin2);
                         trm2 = _mm512_sub_ps(cpin1,x1);
                         x2   = _mm512_fmadd_ps(cost,_mm512_mul_ps(k02,
                                                               _mm512_add_ps(h,atant)));
                         eai3 = _mm512_add_ps(C078539816339744830961566084582,x2);
                         cexp_zmm8r8(ear,ea3,&cer3,&cei3);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_sqrt_ps(_mm512_mul_ps(gms::math::
                                                                  negate_zmm8r8(a2),csct));
                         x2   = _mm512_mul_ps(x0,x1);
                         cer3 = _mm512_mul_ps(_mm512_mul_ps(cer3,x2),trm2);
                         cei3 = _mm512_mul_ps(_mm512_mul_ps(cei3,x2),trm2);
                         cmul_zmm8r8(t0r,t0i,cer3,cei3,&t1r,&t1i);
                         rcs  = cabs_zmm8r8(t1r,t1i);
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
	           __m512d rcs_parallel_f8194_zmm8r8(const __m512d h,
	                                             const __m512d l,
	                                             const __m512d b,
	                                             const __m512d a,
	                                             const __m512d k0,
	                                             const __m512d tht,
	                                             const __m512d alp) {
	                                 
	                                  
	                 const __m512d C314159265358979323846264338328  = 
                                                     _mm512_set1_ps(3.14159265358979323846264338328f); 
                         const __m512d C1772453850905516027298167483341 = 
                                                     _mm512_set1_ps(1.772453850905516027298167483341f);
                         const __m512d C078539816339744830961566084582  = 
                                                     _mm512_set1_ps(0.78539816339744830961566084582f);
                         const __m512d C10                              = 
                                                     _mm512_set1_ps(1.0f);  
                         const __m512d C15                              = 
                                                     _mm512_set1_ps(1.5f); 
                         const __m512d C05                              = 
                                                     _mm512_set1_ps(0.5f);
                         const __m512d C20                              =
                                                     _mm512_set1_ps(2.0f);
                         register __m512d pin,n,invn,spin,cos1,k02,cos2,sint,cost;
                         register __m512d ear,eai1,eai2,eai3,cer1,cei1,sk02,sacs;
                         register __m512d cer2,cei2,cer3,cei3,x0,x1,x2,x3,atant;
                         register __m512d cpin1,cpin2,trm1,trm2,rcs;
                         __m512d t0r,t0i,t1r,t1i,a2;
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
                         cexp_zmm8r8(ear,eai,&cer1,&cei1);
                         cer1 = _mm512_mul_ps(x0,cer1);
                         spin = xsinf(pin);
                         cei1 = _mm512_mul_ps(x0,cei1);
                         cpin = xcosf(pin);
                         x1   = _mm512_mul_ps(_mm512_sub_ps(h,atant),cost);
                         eai2 = _mm512_fmadd_ps(k02,x1,C078539816339744830961566084582);
                         cexp_zmm8r8(ear,eai2,&cer2,&cei);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_mul_ps(x0,sacs);
                         cer2 = _mm512_mul_ps(cer2,x1);
                         cei2 = _mm512_mul_ps(cei2,x1);
                         cpin1= _mm512_rcp14_ps(_mm512_sub_ps(cpin,C10));
                         x2   = _mm512_mul_ps(C20,_mm512_add_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x2,invn));
                         x3   = _mm512_rcp14_ps(_mm512_sub_ps(cpin,cpin2));
                         trm1 = _mm512_add_ps(cpin1,x3);
                         cmul_zmm8r8(cer1,cei1,cer2,cei2,&t0r,&t0i);
                         t0r  = _mm512_mul_ps(t0r,trm1);
                         t0i  = _mm512_mul_ps(t0i,trm1);
                         x0   = _mm512_mul_ps(C20,_mm512_sub_ps(alp,tht));
                         cpin2= xcosf(_mm512_mul_ps(x0,invn));
                         x1   = _mm512_rcp14_ps(cpin2);
                         trm2 = _mm512_add_ps(cpin1,x1);
                         x2   = _mm512_fmadd_ps(cost,_mm512_mul_ps(k02,
                                                               _mm512_add_ps(h,atant)));
                         eai3 = _mm512_add_ps(C078539816339744830961566084582,x2);
                         cexp_zmm8r8(ear,ea3,&cer3,&cei3);
                         x0   = _mm512_div_ps(spin,_mm512_mul_ps(n,sk02));
                         x1   = _mm512_sqrt_ps(_mm512_mul_ps(gms::math::
                                                                  negate_zmm8r8(a2),csct));
                         x2   = _mm512_mul_ps(x0,x1);
                         cer3 = _mm512_mul_ps(_mm512_mul_ps(cer3,x2),trm2);
                         cei3 = _mm512_mul_ps(_mm512_mul_ps(cei3,x2),trm2);
                         cmul_zmm8r8(t0r,t0i,cer3,cei3,&t1r,&t1i);
                         rcs  = cabs_zmm8r8(t1r,t1i);
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
	        
	        
	        
	         
	           __m512d coef_Bg_f9137_zmm8r8(const __m512d A,
	                                        const __m512d N,
	                                        const __m512d k0,
	                                        const __m512d epsr,
	                                        const __m512d epsi,
	                                        const __m512d thti,
	                                        const __m512d thts,
	                                        const __m512d phis,
	                                        const int pol) FUNC_ATTRIBUTES;
	       
	       
	           __m512d coef_Bg_f9137_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pA,
	                                          const double * __restrict __ATTR_ALIGN__(64) pN,
	                                          const double * __restrict __ATTR_ALIGN__(64) pk0,
	                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
	                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
	                                          const double * __restrict __ATTR_ALIGN__(64) pthti,
	                                          const double * __restrict __ATTR_ALIGN__(64) pthts,
	                                          const double * __restrict __ATTR_ALIGN__(64) pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	       
	      
	           __m512d coef_Bg_f9137_zmm8r8_u(const double * __restrict  pA,
	                                          const double * __restrict  pN,
	                                          const double * __restrict  pk0,
	                                          const double * __restrict  pepsr,
	                                          const double * __restrict  pepsi,
	                                          const double * __restrict  pthti,
	                                          const double * __restrict  pthts,
	                                          const double * __restrict  pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	       
	       /*
	             Model 9B4 (Peake's Model)
	             Model resembling many natural grass-like structures like
	             forests,grass,wheat fields, etc.
	             Bistatic RCS (hh) polarized per unit surface area.
	             Formula 9.1-33
	       
	       */
	       
	       
	      
	           __m512d rcs_hh_f9133_zmm8r8( const __m512d A,
	                                        const __m512d N,
	                                        const __m512d k0,
	                                        const __m512d epsr,
	                                        const __m512d epsi,
	                                        const __m512d thti,
	                                        const __m512d thts,
	                                        const __m512d phis,
	                                        const int pol) FUNC_ATTRIBUTES;
	       
	       
	       
	      
	           __m512d rcs_hh_f9133_zmm8r8_a( const double * __restrict __ATTR_ALIGN__(64) pA,
	                                          const double * __restrict __ATTR_ALIGN__(64) pN,
	                                          const double * __restrict __ATTR_ALIGN__(64) pk0,
	                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
	                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
	                                          const double * __restrict __ATTR_ALIGN__(64) pthti,
	                                          const double * __restrict __ATTR_ALIGN__(64) pthts,
	                                          const double * __restrict __ATTR_ALIGN__(64) pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	       
	       
	       
	        
	           __m512d rcs_hh_f9133_zmm8r8_u( const double * __restrict pA,
	                                          const double * __restrict pN,
	                                          const double * __restrict pk0,
	                                          const double * __restrict pepsr,
	                                          const double * __restrict pepsi,
	                                          const double * __restrict pthti,
	                                          const double * __restrict pthts,
	                                          const double * __restrict pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	       
	       
	        
	       /*
	             Model 9B4 (Peake's Model)
	             Model resembling many natural grass-like structures like
	             forests,grass,wheat fields, etc.
	             Bistatic RCS (vh) polarized per unit surface area.
	             Formula 9.1-34
	       
	       */
	       
	       
	         
	           __m512d rcs_vh_f9134_zmm8r8( const __m512d A,
	                                        const __m512d N,
	                                        const __m512d k0,
	                                        const __m512d epsr,
	                                        const __m512d epsi,
	                                        const __m512d thti,
	                                        const __m512d thts,
	                                        const __m512d phis,
	                                        const int pol) FUNC_ATTRIBUTES;
	         
	         
	         
	      
	           __m512d rcs_vh_f9134_zmm8r8_a( const double * __restrict __ATTR_ALIGN__(64) pA,
	                                          const double * __restrict __ATTR_ALIGN__(64) pN,
	                                          const double * __restrict __ATTR_ALIGN__(64) pk0,
	                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
	                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
	                                          const double * __restrict __ATTR_ALIGN__(64) pthti,
	                                          const double * __restrict __ATTR_ALIGN__(64) pthts,
	                                          const double * __restrict __ATTR_ALIGN__(64) pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	         
	         
	           __m512d rcs_vh_f9134_zmm8r8_u( const double * __restrict  pA,
	                                          const double * __restrict  pN,
	                                          const double * __restrict  pk0,
	                                          const double * __restrict  pepsr,
	                                          const double * __restrict  pepsi,
	                                          const double * __restrict  pthti,
	                                          const double * __restrict  pthts,
	                                          const double * __restrict  pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	         
	           /*
	             Model 9B4 (Peake's Model)
	             Model resembling many natural grass-like structures like
	             forests,grass,wheat fields, etc.
	             Bistatic RCS (hv) polarized per unit surface area.
	             Formula 9.1-35
	       
	       */
	       
	       
	        
	           __m512d rcs_hv_f9135_zmm8r8( const __m512d A,
	                                        const __m512d N,
	                                        const __m512d k0,
	                                        const __m512d epsr,
	                                        const __m512d epsi,
	                                        const __m512d thti,
	                                        const __m512d thts,
	                                        const __m512d phis,
	                                        const int pol) FUNC_ATTRIBUTES;
	         
	         
	         
	     
	           __m512d rcs_hv_f9135_zmm8r8_a(  const double * __restrict __ATTR_ALIGN__(64) pA,
	                                          const double * __restrict __ATTR_ALIGN__(64) pN,
	                                          const double * __restrict __ATTR_ALIGN__(64) pk0,
	                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
	                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
	                                          const double * __restrict __ATTR_ALIGN__(64) pthti,
	                                          const double * __restrict __ATTR_ALIGN__(64) pthts,
	                                          const double * __restrict __ATTR_ALIGN__(64) pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	         
	         
	           __m512d  rcs_hv_f9135_zmm8r8_u(const double * __restrict pA,
	                                          const double * __restrict  pN,
	                                          const double * __restrict  pk0,
	                                          const double * __restrict  pepsr,
	                                          const double * __restrict  pepsi,
	                                          const double * __restrict  pthti,
	                                          const double * __restrict  pthts,
	                                          const double * __restrict  pphis,
	                                          const int pol) FUNC_ATTRIBUTES;
	         
	            /*
	             Model 9B4 (Peake's Model)
	             Model resembling many natural grass-like structures like
	             forests,grass,wheat fields, etc.
	             Bistatic RCS (vv) polarized per unit surface area.
	             Formula 9.1-36
	       
	       */
	       
	       
	        
	           __m512d rcs_vv_f9136_zmm8r8( const __m512d A,
	                                        const __m512d N,
	                                        const __m512d k0,
	                                        const __m512d epsr,
	                                        const __m512d epsi,
	                                        const __m512d thti,
	                                        const __m512d thts,
	                                        const __m512d phis,
	                                        const int pol) FUNC_ATTRIBUTES;
	         
	         
	         
	  
	           __m512d rcs_vv_f9136_zmm8r8_a( const double * __restrict __ATTR_ALIGN__(64) pA,
	                                          const double * __restrict __ATTR_ALIGN__(64) pN,
	                                          const double * __restrict __ATTR_ALIGN__(64) pk0,
	                                          const double * __restrict __ATTR_ALIGN__(64) pepsr,
	                                          const double * __restrict __ATTR_ALIGN__(64) pepsi,
	                                          const double * __restrict __ATTR_ALIGN__(64) pthti,
	                                          const double * __restrict __ATTR_ALIGN__(64) pthts,
	                                          const double * __restrict __ATTR_ALIGN__(64) pphis,
	                                          const int pol ) FUNC_ATTRIBUTES;
	         
	         
	       
	           __m512d rcs_vv_f9136_zmm8r8_u( const double * __restrict  pA,
	                                          const double * __restrict  pN,
	                                          const double * __restrict  pk0,
	                                          const double * __restrict  pepsr,
	                                          const double * __restrict  pepsi,
	                                          const double * __restrict  pthti,
	                                          const double * __restrict  pthts,
	                                          const double * __restrict  pphis,
	                                          const int pol ) FUNC_ATTRIBUTES;
	         
	         
	         
	        /*
	            Gaussian surface-height correlation
	            coefficient of average backscattering RCS 
	            per unit area.
	            RCS (vv) polarization.
	            Formula 9.1-74
	        */
	        
	         
	           __m512d rcs_vv_f9174_zmm8r8(const __m512d k0,
	                                       const __m512d h,
	                                       const __m512d l,
	                                       const __m512d thti,
	                                       const __m512d epsr,
	                                       const __m512d epsi,
	                                       const __m512d mur,
	                                       const __m512d mui) FUNC_ATTRIBUTES;
	       
	       
	           __m512d rcs_vv_f9174_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
	                                         const double * __restrict __ATTR_ALIGN__(64) ph,
	                                         const double * __restrict __ATTR_ALIGN__(64) pl,
	                                         const double * __restrict __ATTR_ALIGN__(64) pthti,
	                                         const double * __restrict __ATTR_ALIGN__(64) pepsr,
	                                         const double * __restrict __ATTR_ALIGN__(64) pepsi,
	                                         const double * __restrict __ATTR_ALIGN__(64) pmur,
	                                         const double * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;
	       
	       
	       
	           __m512d rcs_vv_f9174_zmm8r8_u(const double * __restrict  pk0,
	                                         const double * __restrict  ph,
	                                         const double * __restrict  pl,
	                                         const double * __restrict  pthti,
	                                         const double * __restrict  pepsr,
	                                         const double * __restrict  pepsi,
	                                         const double * __restrict  pmur,
	                                         const double * __restrict  pmui) FUNC_ATTRIBUTES;
	       
	        /*
	            Gaussian surface-height correlation
	            coefficient of average backscattering RCS 
	            per unit area.
	            RCS (hh) polarization.
	            Formula 9.1-75
	        */
	        
	        
	         
	           __m512d rcs_hh_f9175_zmm8r8(const __m512d k0,
	                                       const __m512d h,
	                                       const __m512d l,
	                                       const __m512d thti,
	                                       const __m512d epsr,
	                                       const __m512d epsi,
	                                       const __m512d mur,
	                                       const __m512d mui) FUNC_ATTRIBUTES;
	       
	         
	           __m512d rcs_hh_f9175_zmm8r8_a(  const double * __restrict __ATTR_ALIGN__(64) pk0,
	                                         const double * __restrict __ATTR_ALIGN__(64) ph,
	                                         const double * __restrict __ATTR_ALIGN__(64) pl,
	                                         const double * __restrict __ATTR_ALIGN__(64) pthti,
	                                         const double * __restrict __ATTR_ALIGN__(64) pepsr,
	                                         const double * __restrict __ATTR_ALIGN__(64) pepsi,
	                                         const double * __restrict __ATTR_ALIGN__(64) pmur,
	                                         const double * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;
	       
	         
	           __m512d rcs_hh_f9175_zmm8r8_u(  const double * __restrict  pk0,
	                                         const double * __restrict  ph,
	                                         const double * __restrict  pl,
	                                         const double * __restrict  pthti,
	                                         const double * __restrict  pepsr,
	                                         const double * __restrict  pepsi,
	                                         const double * __restrict  pmur,
	                                         const double * __restrict  pmui) FUNC_ATTRIBUTES;
	       
	         /*
	            Gaussian surface-height correlation
	            coefficient of average backscattering RCS 
	            per unit area.
	            RCS (hv) polarization.
	            Formula 9.1-76
	        */
	        
	        
	       
	           __m512d rcs_hv_f9176_zmm8r8() FUNC_ATTRIBUTES;
	         
	         
	         
	         /*
	            Exponential surface-height correlation
	            coefficient of average backscattering RCS 
	            per unit area.
	            RCS (vv) polarization.
	            Formula 9.1-77
	        */
	        
	        
	          
	           __m512d rcs_vv_f9177_zmm8r8(const __m512d k0,
	                                       const __m512d h,
	                                       const __m512d l,
	                                       const __m512d thti,
	                                       const __m512d epsr,
	                                       const __m512d epsi,
	                                       const __m512d mur,
	                                       const __m512d mui) FUNC_ATTRIBUTES;
	       
	       
	    
	           __m512d rcs_vv_f9177_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
	                                         const double * __restrict __ATTR_ALIGN__(64) ph,
	                                         const double * __restrict __ATTR_ALIGN__(64) pl,
	                                         const double * __restrict __ATTR_ALIGN__(64) pthti,
	                                         const double * __restrict __ATTR_ALIGN__(64) pepsr,
	                                         const double * __restrict __ATTR_ALIGN__(64) pepsi,
	                                         const double * __restrict __ATTR_ALIGN__(64) pmur,
	                                         const double * __restrict __ATTR_ALIGN__(64) pmui) 
	       
	       
	       
	        
	           __m512d rcs_vv_f9177_zmm8r8_u(const double * __restrict  pk0,
	                                         const double * __restrict  ph,
	                                         const double * __restrict  pl,
	                                         const double * __restrict  pthti,
	                                         const double * __restrict  pepsr,
	                                         const double * __restrict  pepsi,
	                                         const double * __restrict  pmur,
	                                         const double * __restrict  pmui) FUNC_ATTRIBUTES;
	       
	       
	         /*
	            Exponential surface-height correlation
	            coefficient of average backscattering RCS 
	            per unit area.
	            RCS (hh) polarization.
	            Formula 9.1-78
	        */
	        
	        
	        

	           __m512d rcs_hh_f9178_zmm8r8(const __m512d k0,
	                                       const __m512d h,
	                                       const __m512d l,
	                                       const __m512d thti,
	                                       const __m512d epsr,
	                                       const __m512d epsi,
	                                       const __m512d mur,
	                                       const __m512d mui) FUNC_ATTRIBUTES;
	       
	         
	           __m512d rcs_hh_f9178_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
	                                         const double * __restrict __ATTR_ALIGN__(64) ph,
	                                         const double * __restrict __ATTR_ALIGN__(64) pl,
	                                         const double * __restrict __ATTR_ALIGN__(64) pthti,
	                                         const double * __restrict __ATTR_ALIGN__(64) pepsr,
	                                         const double * __restrict __ATTR_ALIGN__(64) pepsi,
	                                         const double * __restrict __ATTR_ALIGN__(64) pmur,
	                                         const double * __restrict __ATTR_ALIGN__(64) pmui) FUNC_ATTRIBUTES;
	       
	       
	       
	           __m512d rcs_hh_f9178_zmm8r8_u(const double * __restrict  pk0,
	                                         const double * __restrict  ph,
	                                         const double * __restrict  pl,
	                                         const double * __restrict  pthti,
	                                         const double * __restrict  pepsr,
	                                         const double * __restrict  pepsi,
	                                         const double * __restrict  pmur,
	                                         const double * __restrict  pmui) FUNC_ATTRIBUTES;
	       
	         /*
	            Gaussian surface-height correlation
	            coefficient of average backscattering RCS 
	            per unit area.
	            RCS (hv) polarization.
	            Formula 9.1-79
	        */
	        
	        
	       
	           __m512d rcs_hv_f9179_zmm8r8() FUNC_ATTRIBUTES;
	         
	         
	         /*
	                Scattering matrix elements for
	                a perfectly conducting surface.
	                Formula: 9.1-80
	         */
	         
	         
	        
	           __m512d a_vv_f9180_zmm8r8(const __m512d thti,
	                                         const __m512d thts,
	                                         const __m512d phis) FUNC_ATTRIBUTES;
	          
	          
	          
	          
	           __m512d a_vv_f9180_zmm8r8_a(  const double * __restrict __ATTR_ALIGN__(64) pthti,
	                                         const double * __restrict __ATTR_ALIGN__(64) pthts,
	                                         const double * __restrict __ATTR_ALIGN__(64) pphis) FUNC_ATTRIBUTES;
	          
	          
	         
	           __m512d a_vv_f9180_zmm8r8_u(  const double * __restrict  pthti,
	                                         const double * __restrict  pthts,
	                                         const double * __restrict  pphis) FUNC_ATTRIBUTES;
	          
	          /*
	                Scattering matrix elements for
	                a perfectly conducting surface.
	                Formula: 9.1-81
	         */
	         
	         
	        
	           __m512d a_hv_f9181_zmm8r8(const __m512d phis,
	                                     const __m512d thti) FUNC_ATTRIBUTES;
	         
	         
	        
	           __m512d a_hv_f9181_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pphis,
	                                     const double * __restrict __ATTR_ALIGN__(64) pthti) FUNC_ATTRIBUTES;
	          
	       
	        
	     
	           __m512d a_hv_f9181_zmm8r8_u(const double * __restrict pphis,
	                                     const double * __restrict  pthti) FUNC_ATTRIBUTES;
	         
	         
	            /*
	                Scattering matrix elements for
	                a perfectly conducting surface.
	                Formula: 9.1-82
	         */
	         
	         
	       
	           __m512d a_hh_f9182_zmm8r8(const __m512d phis) FUNC_ATTRIBUTES;
	          
	          
	         
	           __m512d a_hh_f9182_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pphis) FUNC_ATTRIBUTES;
	        
	       
	        
	           __m512d a_hh_f9182_zmm8r8_u(const double * __restrict  pphis) FUNC_ATTRIBUTES;
	          
	          
	             /*
	                Scattering matrix elements for
	                a perfectly conducting surface.
	                Formula: 9.1-83
	         */
	         
	         
	      
	           __m512d a_vh_f9183_zmm8r8(const __m512d phis,
	                                     const __m512d thts) FUNC_ATTRIBUTES;
	         
	         
	         
	         
	           __m512d a_vh_f9183_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pphis,
	                                       const double * __restrict __ATTR_ALIGN__(64) pthts) FUNC_ATTRIBUTES;
	         
	       
	           __m512d a_vh_f9183_zmm8r8_u(const double * __restrict  pphis,
	                                       const double * __restrict pthts) FUNC_ATTRIBUTES;
	         
	         
	         /*
	              Backscattering from a perfectly conducting surface
	              Theta (inc) == theta (scat) , phi (scat) = 180 (grad).
	              Formula: 9.1-84 
	         */
	         
	         
	       
	           __m512d a_vv_f9184_zmm8r8(const __m512d thti) FUNC_ATTRIBUTES;
	           
	           
	      
	           __m512d a_vv_f9184_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pthti) FUNC_ATTRIBUTES;
	           
	           
	        
	           __m512d a_vv_f9184_zmm8r8_u(const double * __restrict  pthti) FUNC_ATTRIBUTES;
	           
	           
	           
	         /*
	              Backscattering from a perfectly conducting surface
	              Theta (inc) == theta (scat) , phi (scat) = 180 (grad).
	              Formula: 9.1-85
	         */                  
	         
	         
	      
	           __m512d a_hh_f9185_zmm8r8() FUNC_ATTRIBUTES;
	           
	           
	            /*
	              Backscattering from a perfectly conducting surface
	              Theta (inc) == theta (scat) , phi (scat) = 180 (grad).
	              Formula: 9.1-86
	         */   
	         
	         
	      
	           __m512d a_vh_f9186_zmm8r8() FUNC_ATTRIBUTES;
	           
	           
	         /*
	              Backscattering from a perfectly conducting surface
	              Theta (inc) == theta (scat) , phi (scat) = 180 (grad).
	              Average backscattering RCS per unit area.
	              Gaussian surface height correlation coefficient.
	              Formula: 9.1-87
	         */
	         
	         
	         
	       
	           __m512d rcs_vv_f9187_zmm8r8(const __m512d k0,
	                                       const __m512d h,
	                                       const __m512d l,
	                                       const __m512d thti) FUNC_ATTRIBUTES;
	         
	           
	           
	           
	        
	           __m512d rcs_vv_f9187_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
	                                       const double * __restrict __ATTR_ALIGN__(64) ph,
	                                       const double * __restrict __ATTR_ALIGN__(64) pl,
	                                       const double * __restrict __ATTR_ALIGN__(64) pthti) FUNC_ATTRIBUTES;
	           
	            
	       
	           __m512d rcs_vv_f9187_zmm8r8_u(const double * __restrict  pk0,
	                                         const double * __restrict  ph,
	                                         const double * __restrict  pl,
	                                         const double * __restrict  pthti) FUNC_ATTRIBUTES;
	         
	         
	          /*
	              Backscattering from a perfectly conducting surface
	              Theta (inc) == theta (scat) , phi (scat) = 180 (grad).
	              Average backscattering RCS per unit area.
	              Gaussian surface height correlation coefficient.
	              Formula: 9.1-88
	         */
	         
	         
	      
	           __m512d rcs_hh_f9188_zmm8r8(const __m512d k0,
	                                       const __m512d h,
	                                       const __m512d l,
	                                       const __m512d thti) FUNC_ATTRIBUTES;
	         
	         
	        
	           __m512d rcs_hh_f9188_zmm8r8_a(const double * __restrict __ATTR_ALIGN__(64) pk0,
	                                       const double * __restrict __ATTR_ALIGN__(64) ph,
	                                       const double * __restrict __ATTR_ALIGN__(64) pl,
	                                       const double * __restrict __ATTR_ALIGN__(64) pthti) FUNC_ATTRIBUTES;
	         }
	         
	         
	       
	           __m512d rcs_hh_f9188_zmm8r8_u(const double * __restrict  pk0,
	                                       const double * __restrict  ph,
	                                       const double * __restrict  pl,
	                                       const double * __restrict  pthti) FUNC_ATTRIBUTES;
	         
	         
	         /*
	              
	              Backscattering from a perfectly conducting surface
	              Theta (inc) == theta (scat) , phi (scat) = 180 (grad).
	              Average backscattering RCS per unit area.
	              Gaussian surface height correlation coefficient.
	              Formula: 9.1-89
	           
	         */
	         
	         
	        
	           __m512d rcs_vhhv_f9189_zmm8r8() FUNC_ATTRIBUTES;
	          
	          
	          
	           
	      
	                                    
	        
                 
                 
                 
               
               
         
    




















#endif /*__GMS_RCS_COMPLEX_OBJECTS_ZMM8R8_H__*/
