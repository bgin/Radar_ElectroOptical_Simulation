
#include "GMS_config.fpp"

!/*MIT License
!Copyright (c) 2020 Bernard Gingold
!Permission is hereby granted, free of charge, to any person obtaining a copy
!of this software and associated documentation files (the "Software"), to deal
!in the Software without restriction, including without limitation the rights
!to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!copies of the Software, and to permit persons to whom the Software is
!furnished to do so, subject to the following conditions:
!The above copyright notice and this permission notice shall be included in all
!copies or substantial portions of the Software.
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!SOFTWARE.
!*/

module atmos_refraction_xmm4r4

!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         atmos_refraction_xmm4r4
 !          
 !          Purpose:
 !                        Calculation of  EM wave refraction in the Earth atmopshere.
 !                        SSE-based PAOS manual vectorization (single-precision).
 !                        Various characteristics and formulae of atmospheric refraction (radio waves and visible light/IR wavelengths)  
 !                        Based mainly on      Колосов М.А., Шабельников А.В. - Рефракция электромагнитных волн в атмосферах Земли, Венеры и Марса-Советское Радио (1976)    
 !                       
 !          History:
 !                        Date: 19-04-2025
 !                        Time: 07:01AM GMT+2
 !                        
 !          Version:
 !
 !                      Major: 1
 !                      Minor: 0
 !                      Micro: 0
 !
 !          Author:  
 !                      Bernard Gingold
 !          
 !                 
 !          References:
 !         
 !                       Колосов М.А., Шабельников А.В. 
 !                       "Рефракция электромагнитных волн в атмосферах Земли, Венеры и Марса-Советское Радио (1976)"   
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
   
   use mod_kinds,    only : i4,sp
   use mod_vectypes, only : XMM4r4_t 
#if defined(__GNUC__) && !defined(__INTEL_COMPILER)
   use omp_lib
#endif 

   public
   implicit none

     ! Major version
     integer(kind=i4),  parameter :: ATMOS_REFRACTION_XMM4R4_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: ATMOS_REFRACTION_XMM4R4_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: ATMOS_REFRACTION_XMM4R4_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: ATMOS_REFRACTION_XMM4R4_FULLVER =   &
            1000*ATMOS_REFRACTION_XMM4R4_MAJOR+100*ATMOS_REFRACTION_XMM4R4_MINOR+10*ATMOS_REFRACTION_XMM4R4_MICRO
     ! Module creation date
     character(*),        parameter :: ATMOS_REFRACTION_XMM4R4_CREATE_DATE = "19-04-2025 07:01AM +00200 (SAT 19 APR 2025 GMT+2)"
     ! Module build date
     character(*),        parameter :: ATMOS_REFRACTION_XMM4R4_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: ATMOS_REFRACTION_XMM4R4_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: ATMOS_REFRACTION_XMM4R4_SYNOPSIS    = "Calculation of EM Wave atmospheric refraction - SSE (single) vectorized."

     
     ! IRI model output arrays
     type, public :: ionosphere_state_xmm4r4_t
           
           integer(kind=i4)                          :: n_4x32vecs
           type(XMM4r4_t), allocatable, dimension(:) :: elec_dens    ! electron density in m-3
           type(XMM4r4_t), allocatable, dimension(:) :: neut_tmp     ! neutral temperature in K
           type(XMM4r4_t), allocatable, dimension(:) :: ion_tmp      ! ion temperature in K
           type(XMM4r4_t), allocatable, dimension(:) :: elec_tmp     ! electron temperature in K
           type(XMM4r4_t), allocatable, dimension(:) :: O_ion_d      ! O+ ion density in % or in m-3 
           type(XMM4r4_t), allocatable, dimension(:) :: H_ion_d      ! H+ ion density in % or in m-3 
           type(XMM4r4_t), allocatable, dimension(:) :: He_ion_d     ! He+ ion density in % or in m-3
           type(XMM4r4_t), allocatable, dimension(:) :: O2_ion_d     ! O2+ ion density in % or in m-3 
           type(XMM4r4_t), allocatable, dimension(:) :: NO_ion_d     ! NO+ ion density in % or in m-3
           type(XMM4r4_t), allocatable, dimension(:) :: ion_dens     ! Cluster ion density in % or in m-3
           type(XMM4r4_t), allocatable, dimension(:) :: N_ion_d      ! N+ ion density in % or in m-3 
     end type ionosphere_state_xmm4r4_t

     contains

     ! Formula 2.43, page 46
     pure function n_refract_tht_f243_xmm4r4(n,n0,z,z0,r,R0,phi,phi0) result(n_o_tht)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_refract_tht_f243_xmm4r4
            !dir$ attributes forceinline :: n_refract_tht_f243_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: n_refract_tht_f243_xmm4r4
#endif   
            use mod_vecconsts,    only : v4r4_1
            type(XMM4r4_t),       intent(in) :: n 
            type(XMM4r4_t),       intent(in) :: n0 
            type(XMM4r4_t),       intent(in) :: z 
            type(XMM4r4_t),       intent(in) :: z0 
            type(XMM4r4_t),       intent(in) :: r 
            type(XMM4r4_t),       intent(in) :: R0 
            type(XMM4r4_t),       intent(in) :: phi 
            type(XMM4r4_t),       intent(in) :: phi0 
            type(XMM4r4_t)                   :: n_o_tht 
            ! Locals
            type(XMM4r4_t), automatic :: tgz
            type(XMM4r4_t), automatic :: tgz0 
            type(XMM4r4_t), automatic :: tgphi 
            type(XMM4r4_t), automatic :: tgphi0 
            type(XMM4r4_t), automatic :: num_d 
            type(XMM4r4_t), automatic :: num_s
            type(XMM4r4_t), automatic :: den_d 
            type(XMM4r4_t), automatic :: den_s 
            type(XMM4r4_t), automatic :: rat_s 
            type(XMM4r4_t), automatic :: rat_d 
            type(XMM4r4_t), automatic :: stgz 
            type(XMM4r4_t), automatic :: stgphi
            type(XMM4r4_t), automatic :: stgz0
            type(XMM4r4_t), automatic :: stgphi0
            !dir$ attributes align : 16 :: tgz 
            !dir$ attributes align : 16 :: tgz0 
            !dir$ attributes align : 16 :: tgphi 
            !dir$ attributes align : 16 :: tgphi0 
            !dir$ attributes align : 16 :: num_d 
            !dir$ attributes align : 16 :: num_s 
            !dir$ attributes align : 16 :: den_d 
            !dir$ attributes align : 16 :: den_s 
            !dir$ attributes align : 16 :: rat_s 
            !dir$ attributes align : 16 :: rat_d 
            !dir$ attributes align : 16 :: stgz 
            !dir$ attributes align : 16 :: stgphi 
            !dir$ attributes align : 16 :: stgz0 
            !dir$ attributes align : 16 :: stgphi0

#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
              !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3   
                tgz.v(j)    = tan(z.v(j))
                stgz.v(j)   = tgz.v(j)*tgz.v(j)
                tgz0.v(j)   = tan(z0.v(j))
                stgz0.v(j)  = tgz0.v(j)*tgz0.v(j)
                num_d.v(j)  = n.v(j)*r.v(j)*tgz.v(j) 
                tgphi.v(j)  = tan(phi.v(j))
                stgphi.v(j) = tgphi.v(j)*tgphi.v(j) 
                tgphi0.v(j) = tan(phi0.v(j))
                stgphi0.v(j)= tgphi0.v(j)*tgphi0.v(j)
                num_s.v(j)  = n0.v(j)*R0.v(j)*tgz0.v(j) 
                den_d.v(j)  = sqrt(v4r4_1.v(j)+stgz.v(j)+stgphi.v(j)) 
                den_s.v(j)  = sqrt(v4r4_1.v(j)+stgz0.v(j)+stgphi0.v(j))
                rat_s.v(j)  = num_s.v(j)/den_s.v(j) 
                rat_d.v(j)  = num_d.v(j)/den_d.v(j) 
                n_o_tht.v(j)= rat_d.v(j)-rat_s.v(j) 
             end do 
#else 
                tgz.v    = tan(z.v)
                stgz.v   = tgz.v*tgz.v
                tgz0.v   = tan(z0.v)
                stgz0.v = tgz0.v*tgz0.v
                num_d.v  = n.v*r.v*tgz.v 
                tgphi.v  = tan(phi.v)
                stgphi.v = tgphi.v*tgphi.v 
                tgphi0.v = tan(phi0.v)
                stgphi0.v= tgphi0.v*tgphi0.v
                num_s.v  = n0.v*R0.v*tgz0.v
                den_d.v  = sqrt(v4r4_1.v+stgz.v+stgphi.v) 
                den_s.v  = sqrt(v4r4_1.v+stgz0.v+stgphi0.v)
                rat_s.v  = num_s.v/den_s.v 
                rat_d.v  = num_d.v/den_d.v 
                n_o_tht.v= rat_d.v-rat_s.v    
#endif 
     end function n_refract_tht_f243_xmm4r4

       pure function n_refract_phi_f243_xmm4r4(n,n0,z,z0,r,R0,phi,phi0) result(n_o_phi)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_refract_phi_f243_xmm4r4
            !dir$ attributes forceinline :: n_refract_phi_f243_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: n_refract_phi_f243_xmm4r4
#endif   
            use mod_vecconsts,    only : v4r4_1
            type(XMM4r4_t),       intent(in) :: n 
            type(XMM4r4_t),       intent(in) :: n0 
            type(XMM4r4_t),       intent(in) :: z 
            type(XMM4r4_t),       intent(in) :: z0 
            type(XMM4r4_t),       intent(in) :: r 
            type(XMM4r4_t),       intent(in) :: R0 
            type(XMM4r4_t),       intent(in) :: phi 
            type(XMM4r4_t),       intent(in) :: phi0 
            type(XMM4r4_t), automatic :: tgz
            type(XMM4r4_t), automatic :: tgz0 
            type(XMM4r4_t), automatic :: tgphi 
            type(XMM4r4_t), automatic :: tgphi0 
            type(XMM4r4_t), automatic :: num_d 
            type(XMM4r4_t), automatic :: num_s
            type(XMM4r4_t), automatic :: den_d 
            type(XMM4r4_t), automatic :: den_s 
            type(XMM4r4_t), automatic :: rat_s 
            type(XMM4r4_t), automatic :: rat_d 
            type(XMM4r4_t), automatic :: stgz 
            type(XMM4r4_t), automatic :: stgphi
            type(XMM4r4_t), automatic :: stgz0
            type(XMM4r4_t), automatic :: stgphi0
            !dir$ attributes align : 16 :: tgz 
            !dir$ attributes align : 16 :: tgz0 
            !dir$ attributes align : 16 :: tgphi 
            !dir$ attributes align : 16 :: tgphi0 
            !dir$ attributes align : 16 :: num_d 
            !dir$ attributes align : 16 :: num_s 
            !dir$ attributes align : 16 :: den_d 
            !dir$ attributes align : 16 :: den_s 
            !dir$ attributes align : 16 :: rat_s 
            !dir$ attributes align : 16 :: rat_d 
            !dir$ attributes align : 16 :: stgz 
            !dir$ attributes align : 16 :: stgphi 
            !dir$ attributes align : 16 :: stgz0 
            !dir$ attributes align : 16 :: stgphi0

#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                tgz.v(j)        = tan(z.v(j))
                stgz.v(j)       = tgz.v(j)*tgz.v(j)
                tgz0.v(j)       = tan(z0.v(j))
                stgz0.v(j)      = tgz0.v(j)*tgz0.v(j)
                tgphi.v(j)      = tan(phi.v(j))
                stgphi.v(j)     = tgphi.v(j)*tgphi.v(j) 
                tgphi0.v(j)     = tan(phi0.v(j))
                stgphi0.v(j)    = tgphi0.v(j)*tgphi0.v(j)
                num_d.v(j)      = n.v(j)*r.v(j)*tgphi.v(j) 
                num_s.v(j)      = n0.v(j)*R0.v(j)*tgphi0.v(j) 
                den_d.v(j)      = sqrt(v4r4_1.v(j)+stgz.v(j)+stgphi.v(j)) 
                den_s.v(j)      = sqrt(v4r4_1.v(j)+stgz0.v(j)+stgphi0.v(j))
                rat_s.v(j)      = num_s.v(j)/den_s.v(j) 
                rat_d.v(j)      = num_d.v(j)/den_d.v(j) 
                n_o_phi.v(j)    = rat_d.v(j)-rat_s.v(j) 
             end do 
#else
            tgz.v        = tan(z.v)
            stgz.v       = tgz.v*tgz.v
            tgz0.v       = tan(z0.v)
            stgz0.v      = tgz0.v*tgz0.v
            tgphi.v      = tan(phi.v)
            stgphi.v     = tgphi.v*tgphi.v 
            tgphi0.v     = tan(phi0.v)
            stgphi0.v    = tgphi0.v*tgphi0.v
            num_d.v      = n.v*r.v*tgphi.v 
            num_s.v      = n0.v*R0.v*tgphi0.v 
            den_d.v      = sqrt(v4r4_1.v+stgz.v+stgphi.v) 
            den_s.v      = sqrt(v4r4_1.v+stgz0.v+stgphi0.v)
            rat_s.v      = num_s.v/den_s.v 
            rat_d.v      = num_d.v/den_d.v 
            n_o_phi.v    = rat_d.v-rat_s.v 
#endif
      end function n_refract_phi_f243_xmm4r4

      !Радиус кривизны траектории луча, formula 2.51, page: 47
      pure function rad_ray_curvature_f251_xmm4r4(n,z,dndr) result(rho)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: rad_ray_curvature_f251_xmm4r4
            !dir$ attributes forceinline :: rad_ray_curvature_f251_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rad_ray_curvature_f251_xmm4r4
#endif 
            type(XMM4r4_t),       intent(in) :: n 
            type(XMM4r4_t),       intent(in) :: z 
            type(XMM4r4_t),       intent(in) :: dndr 
            type(XMM4r4_t)                   :: rho 
            type(XMM4r4_t),       automatic  :: t0 
            type(XMM4r4_t),       automatic  :: sinz 
            !dir$ attributes align : 16 :: t0 
            !dir$ attributes align : 16 :: sinz 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                 sinz.v(j) = sin(z.v(j))
                 t0.v(j)   = -n.v(j)/sinz.v(j) 
                 rho.v(j)  = t0.v(j)*dndr.v(j) 
             end do 
#else 
                 sinz.v = sin(z.v)
                 t0.v   = -n.v/sinz.v
                 rho.v  = t0.v*dndr.v
#endif
      end function rad_ray_curvature_f251_xmm4r4

     !относителыную кривизну по-1
     !верхности Земли и траектории волны, formula: 2.54, page: 48
     pure function k_relative_f254_xmm4r4(n,z,dndr) result(k_rel)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: k_relative_f254_xmm4r4
            !dir$ attributes forceinline :: k_relative_f254_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: k_relative_f254_xmm4r4
#endif 
            use mod_vecconsts,    only : v4r4_1
            type(XMM4r4_t),       intent(in) :: n 
            type(XMM4r4_t),       intent(in) :: z 
            type(XMM4r4_t),       intent(in) :: dndr 
            type(XMM4r4_t)                   :: k_rel 
            type(XMM4r4_t),       parameter  :: C000015678896205707118218877391 = &
                                                XMM4r4_t(0.00015678896205707118218877391_sp)
            type(XMM4r4_t),       automatic  :: inv_rho 
             !dir$ attributes align : 16 :: C000015678896205707118218877391
             !dir$ attributes align : 16 :: inv_rho 
             !dir$ attributes align : 16 :: t0 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#endif 
             t0    =    rad_ray_curvature_f251_xmm4r4(n,z,dndr)
#if (GMS_EXPLICIT_VECTORIZE) == 1
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                inv_rho.v(j) = v4r4_1.v(j)/t0.v(j)
                k_rel.v(j)   = C000015678896205707118218877391.v(j)* &
                               inv_rho.v(j)
             end do 
#else 
                inv_rho.v = v4r4_1.v/t0.v
                k_rel.v   = C000015678896205707118218877391.v* &
                               inv_rho.v 
#endif 
      end function k_relative_f254_xmm4r4

      ! отношения радиуса кривизны траекторий
     ! луча к радиусу Земли:, formula 2.67, page: 52 
      pure function rho_to_a_f267_xmm4r4(dndh) result(R)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: rho_to_a_f267_xmm4r4
            !dir$ attributes forceinline :: rho_to_a_f267_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rho_to_a_f267_xmm4r4
#endif 
            type(XMM4r4_t),      intent(in) :: dndh 
            type(XMM4r4_t)                  :: R 
            type(XMM4r4_t),       parameter :: C000015678896205707118218877391 = &
                                                XMM4r4_t(0.00015678896205707118218877391_sp)  
            !dir$ attributes align : 16 ::   C000015678896205707118218877391 
            R.v  =  C000015678896205707118218877391.v*dndh.v       
      end function rho_to_a_f267_xmm4r4 

      !Усредненная зависимость показателя преломления от 
      !высоты, formula: 1.45, page 29
      pure function n_avg_h_f145_xmm4r4(dn0,beta,h) result(nah)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_avg_h_f145_xmm4r4
            !dir$ attributes forceinline :: n_avg_h_f145_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: n_avg_h_f145_xmm4r4
#endif 
            use mod_vecconsts,    only : v4r4_1
            type(XMM4r4_t),      intent(in) :: dn0 
            type(XMM4r4_t),      intent(in) :: beta 
            type(XMM4r4_t),      intent(in) :: h 
            type(XMM4r4_t)                  :: nah 
            type(XMM4r4_t),      automatic  :: earg 
            type(XMM4r4_t),      automatic  :: t0 
            !dir$ attributes align : 16 :: earg 
            !dir$ attributes align : 16 :: t0 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                t0.v(j)   = v4r4_1.v(j)+dn0.v(j) 
                earg.v(j) = -beta.v(j)*h.v(j) 
                nah.v(j)  = t0.v(j)*exp(earg.v(j)) 
             end do 
#else 
              t0.v   = v4r4_1.v+dn0.v 
              earg.v = -beta.v*h.v 
              nah.v  = t0.v*exp(earg.v) 
#endif 
      end function n_avg_h_f145_xmm4r4

      !связь между величинами dn0 , beta, formula 1.46, page: 29
      pure function approx_beta_coeff_f146_xmm4r4(dn0) result(beta)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: approx_beta_coeff_f146_xmm4r4
            !dir$ attributes forceinline :: approx_beta_coeff_f146_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: approx_beta_coeff_f146_xmm4r4
#endif  
            type(XMM4r4_t),        intent(in) :: dn0 
            type(XMM4r4_t)                    :: beta 
            type(XMM4r4_t),        parameter  :: C000000732 = &
                                                   XMM4r4_t(0.00000732_sp)
            type(XMM4r4_t),        parameter  :: C5577      = &
                                                   XMM4r4_t(5577.0_sp)
            type(XMM4r4_t),        automatic  :: t0 
            type(XMM4r4_t),        automatic  :: earg 
            !dir$ attributes align : 16 :: C000000732
            !dir$ attributes align : 16 :: C5577
            !dir$ attributes align : 16 :: earg 
            !dir$ attributes align : 16 :: t0  
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                t0.v(j)   = C000000732.v(j)/dn0.v(j) 
                earg.v(j) = C5577.v(j)*dn0.v(j) 
                beta.v(j) = t0.v(j)*exp(earg.v(j))  
             end do 
#else 
                t0.v   = C000000732.v/dn0.v 
                earg.v = C5577.v*dn0.v
                beta.v = t0.v*exp(earg.v)  
#endif
      end function approx_beta_coeff_f146_xmm4r4

      pure function prob_integral_xmm4r4(x) result(int)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: prob_integral_xmm4r4
            !dir$ attributes forceinline :: prob_integral_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: prob_integral_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_1
            type(XMM4r4_t),        intent(in) :: x 
            type(XMM4r4_t)                    :: int 
            type(XMM4r4_t),        parameter  :: C0707106781186547524400844362105 = &
                                                   XMM4r4_t(0.707106781186547524400844362105_sp)
            type(XMM4r4_t),        parameter  :: C05 = & 
                                                   XMM4r4_t(0.5_sp)
            type(XMM4r4_t),        automatic  ::  t0 
            !dir$ attributes align : 16 :: C0707106781186547524400844362105
            !dir$ attributes align : 16 :: C05 
            !dir$ attributes align : 16 :: t0 
            t0.v  = erf(x.v*C0707106781186547524400844362105.v)
            int.v = C05.v*(v4r4_1.v+t0.v)
      end function prob_integral_xmm4r4

       !формулу (3.35) для расчета регулярной
       !рефракции оптических волн в земной атмосфере.
       ! formula 3.37, page: 68
      pure function analytic_sol_L1_f337_xmm4r4(beta,dn0,z0,H) result(L1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_f337_xmm4r4
            !dir$ attributes forceinline :: analytic_sol_L1_f337_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: analytic_sol_L1_f337_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_1
            type(XMM4r4_t),      intent(in) :: beta 
            type(XMM4r4_t),      intent(in) :: dn0 
            type(XMM4r4_t),      intent(in) :: z0 
            type(XMM4r4_t),      intent(in) :: H 
            type(XMM4r4_t)                  :: L1 
            type(XMM4r4_t),      parameter  :: C6378 = XMM4r4_t(6378.0_sp)
            type(XMM4r4_t),      parameter  :: Cn20  = XMM4r4_t(-2.0_sp)
            type(XMM4r4_t),      parameter  :: C20   = XMM4r4_t(2.0_sp)
            type(XMM4r4_t),      automatic  :: cosz0,ctgz0,ea1
            type(XMM4r4_t),      automatic  :: ea2,exp1,exp2,num2
            type(XMM4r4_t),      automatic  :: den2,num1,den1,sdn0
            type(XMM4r4_t),      automatic  :: stgz0,rat1,rat2 
             !dir$ attributes align : 16 :: C6378 
             !dir$ attributes align : 16 :: Cn20
             !dir$ attributes align : 16 :: C20 
             !dir$ attributes align : 16 :: cosz0 
             !dir$ attributes align : 16 :: ctgz0 
             !dir$ attributes align : 16 :: ea1 
             !dir$ attributes align : 16 :: ea2
             !dir$ attributes align : 16 :: exp1 
             !dir$ attributes align : 16 :: exp2 
             !dir$ attributes align : 16 :: num2 
             !dir$ attributes align : 16 :: den2 
             !dir$ attributes align : 16 :: num1 
             !dir$ attributes align : 16 :: den1 
             !dir$ attributes align : 16 :: sdn0 
             !dir$ attributes align : 16 :: stgz0 
             !dir$ attributes align : 16 :: rat1 
             !dir$ attributes align : 16 :: rat2 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                 ea1.v(j)   = Cn20.v(j)*beta.v(j)*H.v(j)
                 ea2.v(j)   = -beta.v(j)*H.v(j) 
                 ctgz0.v(j) = v4r4_1.v(j)/tan(z0.v(j))
                 sdn0.v(j)  = dn0.v(j)*dn0.v(j) 
                 exp1.v(j)  = exp(ea1.v(j))
                 num1.v(j)  = beta.v(j)*C6378.v(j)*sdn0.v(j)*ctgz0.v(j)
                 cosz0.v(j) = cos(z0.v(j))
                 den1.v(j)  = cosz0.v(j)*cosz0.v(j) 
                 exp2.v(j)  = exp(ea2.v(j))
                 rat1.v(j)  = num1.v(j)/den1.v(j) 
                 stgz0.v(j) = C20.v(j)*(tgz0.v(j)*tgz0.v(j)) 
                 den2.v(j)  = sqrt(v4r4_1.v(j)+stgz0.v(j)*(H.v(j)/C6378.v(j)))
                 num2.v(j)  = exp1.v(j)-exp2.v(j) 
                 rat2.v(j)  = num2.v(j)/den2.v(j) 
                 L1.v(j)    = rat1.v(j)*rat2.v(j) 
             end do 
#else 
                 ea1.v   = Cn20.v*beta.v*H.v
                 ea2.v   = -beta.v*H.v
                 ctgz0.v = v4r4_1.v/tan(z0.v)
                 sdn0.v  = dn0.v*dn0.v
                 exp1.v  = exp(ea1.v)
                 num1.v = beta.v*C6378.v*sdn0.v*ctgz0.v
                 cosz0.v = cos(z0.v)
                 den1.v  = cosz0.v*cosz0.v 
                 exp2.v  = exp(ea2.v)
                 rat1.v  = num1.v/den1.v 
                 stgz0.v = C20.v*(tgz0.v*tgz0.v) 
                 den2.v  = sqrt(v4r4_1.v+stgz0.v*(H.v/C6378.v))
                 num2.v  = exp1.v-exp2.v 
                 rat2.v  = num2.v/den2.v 
                 L1.v    = rat1.v*rat2.v 
#endif
      end function analytic_sol_L1_f337_xmm4r4

       !формулa (3.35) для расчета регулярной
       !рефракции оптических волн в земной атмосфере.
       ! formula 3.41, page: 68
      pure function analytic_sol_L2_f341_xmm4r4(dn0,beta,z0,H) result(L2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L2_f341_xmm4r4
            !dir$ attributes forceinline :: analytic_sol_L2_f341_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: analytic_sol_L2_f341_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_1
            type(XMM4r4_t),      intent(in) :: beta 
            type(XMM4r4_t),      intent(in) :: dn0 
            type(XMM4r4_t),      intent(in) :: z0 
            type(XMM4r4_t),      intent(in) :: H 
            type(XMM4r4_t)                  :: L2 
            type(XMM4r4_t),      parameter  :: C6378 = XMM4r4_t(6378.0_sp)
            type(XMM4r4_t),      parameter  :: C20   = XMM4r4_t(2.0_sp)
            type(XMM4r4_t),      parameter  :: C1253314137315500251207882642406 = &
                                                       XMM4r4_t(1.253314137315500251207882642406_sp)
            type(XMM4r4_t),      parameter  :: C05   = XMM4r4_t(0.5_sp)                                           
            type(XMM4r4_t), automatic :: sba, ctgz0, ba 
            type(XMM4r4_t), automatic :: sctgz0, tbh, phi1, phi2 
            type(XMM4r4_t), automatic :: exp1, bactgz0, t0, t1  
             !dir$ attributes align : 16 :: C6378 
             !dir$ attributes align : 16 :: C20 
             !dir$ attributes align : 16 :: sba 
             !dir$ attributes align : 16 :: ctgz0 
             !dir$ attributes align : 16 :: ba 
             !dir$ attributes align : 16 :: sctgz0 
             !dir$ attributes align : 16 :: tbh 
             !dir$ attributes align : 16 :: phi1 
             !dir$ attributes align : 16 :: phi2 
             !dir$ attributes align : 16 :: exp1 
             !dir$ attributes align : 16 :: bactgz0 
             !dir$ attributes align : 16 :: t0 
             !dir$ attributes align : 16 :: t1 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                 sba.v(j)    = sqrt(beta.v(j)*C6378.v(j))
                 ctgz0.v(j)  = v4r4_1.v(j)/tan(z0.v(j))
                 sctgz0.v(j) = ctgz0.v(j)*ctgz0.v(j)  
                 bactgz0.v(j)= beta.v(j)*C6378.v(j)*sctgz0.v(j)  
                 tbH.v(j)    = c20.v(j)*beta.v(j)*H.v(j)  
                 t0.v(j)     = dn0.v(j)*sqrt(beta.v(j)*C6378.v(j)*ctgz0.v(j))
                 exp1.v(j)   = exp(sctgz0.v(j)*C05.v(j))* &
                                 C1253314137315500251207882642406.v(j)
                 phi1.v(j)   = prob_integral_xmm4r4(sqrt(bactgz0.v(j)*tbH.v(j)))
                 phi2.v(j)   = prob_integral_xmm4r4(sqrt(bactgz0.v(j)))
                 t1.v(j)     = phi1.v(j)-phi2.v(j) 
                 L2.v(j)     = t0.v(j)*exp1.v(j)*t1.v(j) 
             end do 
#else 
                 sba.v    = sqrt(beta.v*C6378.v)
                 ctgz0.v  = v4r4_1.v/tan(z0.v)
                 sctgz0.v = ctgz0.v*ctgz0.v 
                 bactgz0.v= beta.v*C6378.v*sctgz0.v 
                 tbH.v    = c20.v*beta.v*H.v 
                 t0.v     = dn0.v*sqrt(beta.v*C6378.v*ctgz0.v)
                 exp1.v   = exp(sctgz0.v*C05.v)* &
                                 C1253314137315500251207882642406.v
                 phi1.v   = prob_integral_xmm4r4(sqrt(bactgz0.v*tbH.v))
                 phi2.v   = prob_integral_xmm4r4(sqrt(bactgz0.v))
                 t1.v     = phi1.v-phi2.v
                 L2.v     = t0.v*exp1.v*t1.v
#endif
      end function analytic_sol_L2_f341_xmm4r4

       !формулa (3.35) для расчета регулярной
       !рефракции оптических волн в земной атмосфере.
       ! formula 3.42, page: 68
      pure function analytic_sol_L3_f342_xmm4r4(dn0,beta,z0,H) result(L3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L3_f342_xmm4r4
            !dir$ attributes forceinline :: analytic_sol_L3_f342_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: analytic_sol_L3_f342_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_1
            type(XMM4r4_t),      intent(in) :: beta 
            type(XMM4r4_t),      intent(in) :: dn0 
            type(XMM4r4_t),      intent(in) :: z0 
            type(XMM4r4_t),      intent(in) :: H 
            type(XMM4r4_t)                  :: L2 
            type(XMM4r4_t),      parameter  :: C6378 = XMM4r4_t(6378.0_sp)
            type(XMM4r4_t),      parameter  :: C20   = XMM4r4_t(2.0_sp)
            type(XMM4r4_t),      parameter  :: C1253314137315500251207882642406 = &
                                                       XMM4r4_t(1.253314137315500251207882642406_sp)
            type(XMM4r4_t),      parameter  :: C05   = XMM4r4_t(0.5_sp)  
            type(XMM4r4_t),      parameter  :: C40   = XMM4r4_t(4.0_sp)                                           
            type(XMM4r4_t), automatic :: sba, ctgz0, ba 
            type(XMM4r4_t), automatic :: sctgz0, tbh, phi1, phi2 
            type(XMM4r4_t), automatic :: exp1, bactgz0, t0, t1  
             !dir$ attributes align : 16 :: C6378 
             !dir$ attributes align : 16 :: C20 
             !dir$ attributes align : 16 :: sba 
             !dir$ attributes align : 16 :: ctgz0 
             !dir$ attributes align : 16 :: ba 
             !dir$ attributes align : 16 :: sctgz0 
             !dir$ attributes align : 16 :: tbh 
             !dir$ attributes align : 16 :: phi1 
             !dir$ attributes align : 16 :: phi2 
             !dir$ attributes align : 16 :: exp1 
             !dir$ attributes align : 16 :: bactgz0 
             !dir$ attributes align : 16 :: t0 
             !dir$ attributes align : 16 :: t1 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                 sba.v(j)    = sqrt(C20.v(j)*beta.v(j)*C6378.v(j))
                 ctgz0.v(j)  = v4r4_1.v(j)/tan(z0.v(j))
                 sctgz0.v(j) = ctgz0.v(j)*ctgz0.v(j)  
                 bactgz0.v(j)= C20.v(j)*beta.v(j)*C6378.v(j)*sctgz0.v(j)  
                 tbH.v(j)    = c40.v(j)*beta.v(j)*H.v(j)  
                 t0.v(j)     = dn0.v(j)*sqrt(beta.v(j)*C6378.v(j)*ctgz0.v(j))
                 exp1.v(j)   = exp(sctgz0.v(j))* &
                                 C1253314137315500251207882642406.v(j)
                 phi1.v(j)   = prob_integral_xmm4r4(sqrt(bactgz0.v(j)+tbH.v(j)))
                 phi2.v(j)   = prob_integral_xmm4r4(sqrt(bactgz0.v(j)))
                 t1.v(j)     = phi1.v(j)-phi2.v(j) 
                 L2.v(j)     = t0.v(j)*exp1.v(j)*t1.v(j) 
             end do 
#else 
                 sba.v    = sqrt(C20.v*beta.v*C6378.v)
                 ctgz0.v  = v4r4_1.v/tan(z0.v)
                 sctgz0.v = ctgz0.v*ctgz0.v 
                 bactgz0.v= C20.v*beta.v*C6378.v*sctgz0.v 
                 tbH.v    = c40.v*beta.v*H.v 
                 t0.v     = dn0.v*sqrt(beta.v*C6378.v*ctgz0.v)
                 exp1.v   = exp(sctgz0.v)* &
                                 C1253314137315500251207882642406.v
                 phi1.v   = prob_integral_xmm4r4(sqrt(bactgz0.v+tbH.v))
                 phi2.v   = prob_integral_xmm4r4(sqrt(bactgz0.v))
                 t1.v     = phi1.v-phi2.v
                 L2.v     = t0.v*exp1.v*t1.v
#endif
      end function analytic_sol_L3_f342_xmm4r4
    
       !Формула' (3.35) справедлива во всем диапазоне 
       !изменения зенитных углов (0 < z0 <90°) при любых 
       !зависимостях n(h).
       ! The angle of refraction.
      pure function refraction_angle_f345_xmm4r4(n0,nh,z0,dn0,beta,H) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_f345_xmm4r4
            !dir$ attributes forceinline :: refraction_angle_f345_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refraction_angle_f345_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_1
            type(XMM4r4_t),     intent(in) :: n0 
            type(XMM4r4_t),     intent(in) :: nh 
            type(XMM4r4_t),     intent(in) :: z0 
            type(XMM4r4_t),     intent(in) :: dn0 
            type(XMM4r4_t),     intent(in) :: beta 
            type(XMM4r4_t),     intent(in) :: H 
            type(XMM4r4_t)                 :: alpha 
            type(XMM4r4_t),     parameter  :: a = XMM4r4_t(6378.0_sp)
            type(XMM4r4_t),     automatic  :: ctgz0, ln0nh
            type(XMM4r4_t),     automatic  :: ssecz0,badn0 
            type(XMM4r4_t),     automatic  :: ctgzsec0, t0 
            type(XMM4r4_t),     automatic  :: t1,       t2 
            type(XMM4r4_t),     automatic  :: L1,       L2 
            type(XMM4r4_t),     automatic  :: L3 
             !dir$ attributes align : 16 :: a
             !dir$ attributes align : 16 :: ctgz0 
             !dir$ attributes align : 16 :: ln0nh 
             !dir$ attributes align : 16 :: ssecz0 
             !dir$ attributes align : 16 :: badn0 
             !dir$ attributes align : 16 :: ctgzsec0 
             !dir$ attributes align : 16 :: t0 
             !dir$ attributes align : 16 :: t1 
             !dir$ attributes align : 16 :: t2 
             !dir$ attributes align : 16 :: L1 
             !dir$ attributes align : 16 :: L2 
             !dir$ attributes align : 16 :: L3 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#endif 
             L1  =   analytic_sol_L1_f337_xmm4r4(dn0,beta,z0,H)   
             L2  =   analytic_sol_L2_f341_xmm4r4(dn0,beta,z0,H)
             L3  =   analytic_sol_L3_f342_xmm4r4(dn0,beta,z0,H)   
#if (GMS_EXPLICIT_VECTORIZE) == 1
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3      
                 badn0.v(j)    = beta.v(j)*a.v(j)*dn0.v(j) 
                 ctgz0.v(j)    = v4r4_1.v(j)/tan(z0.v(j))
                 ln0nh.v(j)    = log(n0.v(j)/nh.v(j)) 
                 t0.v(j)       = v4r4_1.v(j)/sin(z0.v(j))
                 ssecz0.v(j)   = t0.v(j)*t0.v(j) 
                 t0.v(j)       = -ctgz0.v(j)*ln0nh.v(j)+L1.v(j) 
                 ctgzsec0.v(j) = ctgz0.v(j)*ssecz0.v(j)
                 t1.v(j)       = ctgzsec0.v(j)*L2.v(j)
                 t2.v(j)       = badn0.v(j)*ctgzsec0.v(j)*(L3.v(j)-L2.v(j))
                 alpha.v(j)    = t0.v(j)+t1.v(j)+t2.v(j) 
             end do 
#else 
                 badn0.v    = beta.v*a.v*dn0.v 
                 ctgz0.v   = v4r4_1.v/tan(z0.v)
                 ln0nh.v    = log(n0.v/nh.v) 
                 t0.v       = v4r4_1.v/sin(z0.v)
                 ssecz0.v   = t0.v*t0.v
                 t0.v       = -ctgz0.v*ln0nh.v+L1.v
                 ctgzsec0.v = ctgz0.v*ssecz0.v
                 t1.v      = ctgzsec0.v*L2.v
                 t2.v       = badn0.v*ctgzsec0.v*(L3.v-L2.v)
                 alpha.v   = t0.v+t1.v+t2.v 
#endif
      end function refraction_angle_f345_xmm4r4

      ! z0 близко к 90°.
       ! The angle of arrival close to horizon.
       ! formula 3.51, page: 70
       ! analytic solution L2 for angle near 90 (deg)
      pure function analytic_sol_n90_L2_f351_xmm4r4(dn0,beta,z0) result(L2) 
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_n90_L2_f351_xmm4r4
            !dir$ attributes forceinline :: analytic_sol_n90_L2_f351_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: analytic_sol_n90_L2_f351_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_1, v4r4_2 
            type(XMM4r4_t),     intent(in) :: dn0 
            type(XMM4r4_t),     intent(in) :: beta 
            type(XMM4r4_t),     intent(in) :: z0 
            type(XMM4r4_t)                 :: L2 
            type(XMM4r4_t),     parameter  :: a = XMM4r4_t(6378.0_sp)
            type(XMM4r4_t),     parameter :: C1253314137315500251207882642406 =  &
                                                    XMM4r4_t(1.253314137315500251207882642406_sp) 
            type(XMM4r4_t),     parameter :: C0318309886183790671537767526745 =  &
                                                    XMM4r4_t(0.318309886183790671537767526745_sp)
            type(XMM4r4_t),     automatic :: sba, tgz0
            type(XMM4r4_t),     automatic :: stgz0, ctgz0 
            type(XMM4r4_t),     automatic :: earg, exp1
            type(XMM4r4_t),     automatic :: t0,   t1 
            type(XMM4r4_t),     automatic :: strm 
             !dir$ attributes align : 16 :: a 
             !dir$ attributes align : 16 :: C1253314137315500251207882642406
             !dir$ attributes align : 16 :: C0318309886183790671537767526745 
             !dir$ attributes align : 16 :: sba 
             !dir$ attributes align : 16 :: tgz0 
             !dir$ attributes align : 16 :: stgz0 
             !dir$ attributes align : 16 :: ctgz0 
             !dir$ attributes align : 16 :: earg 
             !dir$ attributes align : 16 :: exp1 
             !dir$ attributes align : 16 :: t0 
             !dir$ attributes align : 16 :: t1 
             !dir$ attributes align : 16 :: strm 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                 sba.v(j)  = sqrt(beta.v(j)*a.v(j))
                 tgz0.v(j) = tan(z0.v(j))
                 ctgz0.v(j)= v4r4_1.v(j)/tan(z0.v(j))
                 earg.v(j) = beta.v(j)*a.v(j)/(v4r4_2.v(j)*tgz0.v(j)*tgz0.v(j))
                 exp1.v(j) = exp(earg.v(j))
                 t0.v(j)   = dn0.v(j)*(sba.v(j)/tgz0.v(j))*exp1.v(j) 
                 strm.v(j) = sqrt(v4r4_2.v(j)*beta.v(j)*a.v(j)*C0318309886183790671537767526745.v(j))
                 t1.v(j)   = C1253314137315500251207882642406.v(j)* &
                             (v4r4_1.v(j)-strm.v(j)*ctgz0.v(j))
                 L2.v(j)   = t0.v(j)*t1.v(j) 
             end do 
#else 
                 sba.v  = sqrt(beta.v*a.v)
                 tgz0.v = tan(z0.v)
                 ctgz0.v= v4r4_1.v/tan(z0.v)
                 earg.v = beta.v*a.v/(v4r4_2.v*tgz0.v*tgz0.v)
                 exp1.v = exp(earg.v)
                 t0.v   = dn0.v*(sba.v/tgz0.v)*exp1.v
                 strm.v = sqrt(v4r4_2.v*beta.v*a.v*C0318309886183790671537767526745.v)
                 t1.v   = C1253314137315500251207882642406.v* &
                             (v4r4_1.v-strm.v*ctgz0.v)
                 L2.v   = t0.v*t1.v 
#endif
      end function analytic_sol_n90_L2_f351_xmm4r4

       ! z0 близко к 90°.
       ! The angle of arrival close to horizon.
       ! formula 3.51, page: 70
       ! analytic solution L3 for angle near 90 (deg)
       pure function analytic_sol_n90_L3_f351_xmm4r4(dn0,beta,z0) result(L2) 
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_n90_L3_f351_xmm4r4
            !dir$ attributes forceinline :: analytic_sol_n90_L3_f351_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: analytic_sol_n90_L3_f351_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_1, v4r4_2, v4r4_4 
            type(XMM4r4_t),     intent(in) :: dn0 
            type(XMM4r4_t),     intent(in) :: beta 
            type(XMM4r4_t),     intent(in) :: z0 
            type(XMM4r4_t)                 :: L2 
            type(XMM4r4_t),     parameter  :: a = XMM4r4_t(6378.0_sp)
            type(XMM4r4_t),     parameter :: C1253314137315500251207882642406 =  &
                                                    XMM4r4_t(1.253314137315500251207882642406_sp) 
            type(XMM4r4_t),     parameter :: C0318309886183790671537767526745 =  &
                                                    XMM4r4_t(0.318309886183790671537767526745_sp)
            type(XMM4r4_t),     automatic :: sba, tgz0
            type(XMM4r4_t),     automatic :: stgz0, ctgz0 
            type(XMM4r4_t),     automatic :: earg, exp1
            type(XMM4r4_t),     automatic :: t0,   t1 
            type(XMM4r4_t),     automatic :: strm 
             !dir$ attributes align : 16 :: a 
             !dir$ attributes align : 16 :: C1253314137315500251207882642406
             !dir$ attributes align : 16 :: C0318309886183790671537767526745 
             !dir$ attributes align : 16 :: sba 
             !dir$ attributes align : 16 :: tgz0 
             !dir$ attributes align : 16 :: stgz0 
             !dir$ attributes align : 16 :: ctgz0 
             !dir$ attributes align : 16 :: earg 
             !dir$ attributes align : 16 :: exp1 
             !dir$ attributes align : 16 :: t0 
             !dir$ attributes align : 16 :: t1 
             !dir$ attributes align : 16 :: strm 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                 sba.v(j)  = sqrt(v4r4_2.v(j)*beta.v(j)*a.v(j))
                 tgz0.v(j) = tan(z0.v(j))
                 ctgz0.v(j)= v4r4_1.v(j)/tan(z0.v(j))
                 earg.v(j) = beta.v(j)*a.v(j)/(tgz0.v(j)*tgz0.v(j))
                 exp1.v(j) = exp(earg.v(j))
                 t0.v(j)   = dn0.v(j)*(sba.v(j)/tgz0.v(j))*exp1.v(j) 
                 strm.v(j) = sqrt(v4r4_4.v(j)*beta.v(j)*a.v(j)*C0318309886183790671537767526745.v(j))
                 t1.v(j)   = C1253314137315500251207882642406.v(j)* &
                             (v4r4_1.v(j)-strm.v(j)*ctgz0.v(j))
                 L2.v(j)   = t0.v(j)*t1.v(j) 
             end do 
#else 
                 sba.v  = sqrt(v4r4_2.v*beta.v*a.v)
                 tgz0.v = tan(z0.v)
                 ctgz0.v= v4r4_1.v/tan(z0.v)
                 earg.v = beta.v*a.v/(v4r4_2.v*tgz0.v*tgz0.v)
                 exp1.v = exp(earg.v)
                 t0.v   = dn0.v*(sba.v/tgz0.v)*exp1.v
                 strm.v = sqrt(v4r4_4.v*beta.v*a.v*C0318309886183790671537767526745.v)
                 t1.v   = C1253314137315500251207882642406.v* &
                             (v4r4_1.v-strm.v*ctgz0.v)
                 L2.v   = t0.v*t1.v 
#endif
      end function analytic_sol_n90_L3_f351_xmm4r4

       ! z0 близко к 90°.
       ! The angle of arrival close to horizon.
       ! formula 3.51, page: 70
       ! The whole solution for angle alpha near 90 (deg)
       pure function refraction_angle_n90_f351_xmm4r4(dn0,beta,z0) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_n90_f351_xmm4r4
            !dir$ attributes forceinline :: refraction_angle_n90_f351_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refraction_angle_n90_f351_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_1
            type(XMM4r4_t),     intent(in) :: dn0 
            type(XMM4r4_t),     intent(in) :: beta 
            type(XMM4r4_t),     intent(in) :: z0 
            type(XMM4r4_t)                 :: alpha 
            type(XMM4r4_t),     parameter  :: a = XMM4r4_t(6378.0_sp)
            type(XMM4r4_t),     automatic  :: ctgz0, badn0
            type(XMM4r4_t),     automatic  :: cosz0, scosz0 
            type(XMM4r4_t),     automatic  :: L2, L3 
            type(XMM4r4_t),     automatic  :: t0, t1 
            type(XMM4r4_t),     automatic  :: rat 
            !dir$ attributes align : 16 :: a 
            !dir$ attributes align : 16 :: ctgz0 
            !dir$ attributes align : 16 :: badn0 
            !dir$ attributes align : 16 :: cosz0 
            !dir$ attributes align : 16 :: scosz0 
            !dir$ attributes align : 16 :: L2 
            !dir$ attributes align : 16 :: L3 
            !dir$ attributes align : 16 :: t0 
            !dir$ attributes align : 16 :: t1 
            !dir$ attributes align : 16 :: rat 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#endif 
             L2 = analytic_sol_n90_L2_f351_xmm4r4(dn0,beta,z0)
             L3 = analytic_sol_n90_L3_f351_xmm4r4(dn0,beta,z0)
#if (GMS_EXPLICIT_VECTORIZE) == 1
 #if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3
                 cosz0.v(j) = cos(z0.v(j))
                 badn0.v(j) = beta.v(j)*dn0.v(j)*a.v(j) 
                 ctgz0.v(j) = v4r4_1.v(j)/tan(z0.v(j))
                 scosz0.v(j)= cosz0.v(j)*cosz0.v(j) 
                 rat.v(j)   = ctgz0.v(j)/scosz0.v(j) 
                 t0.v(j)    = -dn0.v(j)*ctgz0.v(j)+(v4r4_1.v(j)-badn0.v(j)) 
                 t1.v(j)    = rat.v(j)*L2.v(j)+badn0.v(j)*rat.v(j)*L3.v(j) 
                 alpha.v(j) = t0.v(j)*t1.v(j) 
             end do 
#else 
                 cosz0.v = cos(z0.v)
                 badn0.v = beta.v*dn0.v*a.v
                 ctgz0.v = v4r4_1.v/tan(z0.v)
                 scosz0.v= cosz0.v*cosz0.v
                 rat.v   = ctgz0.v/scosz0.v
                 t0.v    = -dn0.v*ctgz0.v+(v4r4_1.v-badn0.v) 
                 t1.v    = rat.v*L2.v+badn0.v*rat.v*L3.v
                 alpha.v = t0.v*t1.v
#endif 
       end function refraction_angle_n90_f351_xmm4r4

        !z0 = 90° формула (3.51) упрощается.
       ! formula: 3.52, page: 71
       pure function refraction_angle_at90_f352_xmm4r4(dn0,beta) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_at90_f352_xmm4r4
            !dir$ attributes forceinline :: refraction_angle_at90_f352_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refraction_angle_at90_f352_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_1, v4r4_1over2
            type(XMM4r4_t),     intent(in) :: dn0 
            type(XMM4r4_t),     intent(in) :: beta
            type(XMM4r4_t)                 :: alpha 
            type(XMM4r4_t),     parameter  :: a =  XMM4r4_t(6378.0_sp)
            type(XMM4r4_t),     parameter :: C041421356237309504880168872421 =                  &
                                                   XMM4r4_t(0.41421356237309504880168872421_sp) 
            type(XMM4r4_t),     parameter :: C314159265358979323846264338328 =                  &
                                                   XMM4r4_t(3.14159265358979323846264338328_sp)
            type(XMM4r4_t),     automatic  :: t0, t1, t2 
            !dir$ attributes align : 16 :: a 
            !dir$ attributes align : 16 :: C041421356237309504880168872421
            !dir$ attributes align : 16 :: C314159265358979323846264338328 
            !dir$ attributes align : 16 :: t0 
            !dir$ attributes align : 16 :: t1 
            !dir$ attributes align : 16 :: t2 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                 t0.v(j) = dn0.v(j)*sqrt((C314159265358979323846264338328.v(j)*beta.v(j)*a.v(j))*v4r4_1over2.v(j))
                 t1.v(j) = v4r4_1.v(j)+C041421356237309504880168872421.v(j)*beta.v(j)*a.v(j)*dn0.v(j) 
                 alpha.v(j) = t0.v(j)*t1.v(j) 
             end do 
#else 
                 t0.v = dn0.v*sqrt((C314159265358979323846264338328.v*beta.v*a.v)*v4r4_1over2.v)
                 t1.v = v4r4_1.v+C041421356237309504880168872421.v*beta.v*a.v*dn0.v
                 alpha.v = t0.v*t1.v  
#endif 
       end function refraction_angle_at90_f352_xmm4r4 

       !угол радиорефракции I типа в 
       !земной атмосфере для длин волн, меньших 5 см
       ! formula: 4.2, page 73.
       pure function analytic_sol_L1_gl5cm_f42_xmm4r4(dn0,beta,z0,H) result(L1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_gl5cm_f42_xmm4r4
            !dir$ attributes forceinline :: analytic_sol_L1_gl5cm_f42_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: analytic_sol_L1_gl5cm_f42_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_1, v4r4_2, v4r4_n2 
            type(XMM4r4_t),     intent(in) :: dn0 
            type(XMM4r4_t),     intent(in) :: beta 
            type(XMM4r4_t),     intent(in) :: z0 
            type(XMM4r4_t),     intent(in) :: H 
            type(XMM4r4_t)                 :: L1 
            type(XMM4r4_t),     parameter  :: a =  XMM4r4_t(6378.0_sp)
            type(XMM4r4_t),     automatic  :: ctgz0, secz0
            type(XMM4r4_t),     automatic  :: tgz0, betaH  
            type(XMM4r4_t),     automatic  :: t0,   t1 
            type(XMM4r4_t),     automatic  :: earg, exp1 
            type(XMM4r4_t),     automatic  :: exp2, sdn0ba 
            type(XMM4r4_t),     automatic  :: trm1,  trm2 
            type(XMM4r4_t),     automatic  :: trm3 
            !dir$ attributes align : 16 :: a 
            !dir$ attributes align : 16 :: ctgz0 
            !dir$ attributes align : 16 :: secz0 
            !dir$ attributes align : 16 :: tgz0 
            !dir$ attributes align : 16 :: betaH
            !dir$ attributes align : 16 :: t0 
            !dir$ attributes align : 16 :: t1 
            !dir$ attributes align : 16 :: earg 
            !dir$ attributes align : 16 :: exp1 
            !dir$ attributes align : 16 :: exp2 
            !dir$ attributes align : 16 :: sdn0ba 
            !dir$ attributes align : 16 :: trm1 
            !dir$ attributes align : 16 :: trm2 
            !dir$ attributes align : 16 :: trm3 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                 betaH.v(j)  = beta.v(j)*H.v(j) 
                 ctgz0.v(j)  = v4r4_1.v(j)/tan(z0.v(j))
                 sdn0ba.v(j) = -dn0.v(j)*dn0.v(j)*beta.v(j)*a.v(j) 
                 t0.v(j)     = tan(z0.v(j)) 
                 tgz0.v(j)   = t0.v(j)*t0.v(j) 
                 t1.v(j)     = v4r4_1.v(j)/cos(z0.v(j)) 
                 secz0.v(j)  = t1.v(j)*t1.v(j) 
                 exp1.v(j)   = exp(-betaH.v(j))
                 ctgz0.v(j)  = v4r4_1.v(j)/t0.v(j) 
                 exp2.v(j)   = exp(v4r4_n2.v(j)*betaH.v(j))
                 trm1.v(j)   = sdn0ba.v(j)*ctgz0.v(j)*secz0.v(j) 
                 trm2.v(j)   = exp1.v(j)-exp2.v(j) 
                 trm3.v(j)   = sqrt(v4r4_1.v(j)+v4r4_2.v(j)*tgz0.v(j)*(H.v(j)/a.v(j)))
                 L1.v(j)    = trm1.v(j)*trm2.v(j)*trm3.v(j) 
             end do 
#else 
                 betaH.v  = beta.v*H.v
                 ctgz0.v  = v4r4_1.v/tan(z0.v)
                 sdn0ba.v = -dn0.v*dn0.v*beta.v*a.v
                 t0.v     = tan(z0.v) 
                 tgz0.v   = t0.v*t0.v 
                 t1.v     = v4r4_1.v/cos(z0.v) 
                 secz0.v  = t1.v*t1.v
                 exp1.v   = exp(-betaH.v)
                 ctgz0.v  = v4r4_1.v/t0.v
                 exp2.v   = exp(v4r4_n2.v*betaH.v)
                 trm1.v   = sdn0ba.v*ctgz0.v*secz0.v
                 trm2.v   = exp1.v-exp2.v 
                 trm3.v   = sqrt(v4r4_1.v+v4r4_2.v*tgz0.v*(H.v/a.v))
                 L1.v    = trm1.v*trm2.v*trm3.v
#endif
      end function analytic_sol_L1_gl5cm_f42_xmm4r4

      pure function analytic_sol_L2_gl5cm_f43_xmm4r4(dn0,beta,z0,H) result(L2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L2_gl5cm_f43_xmm4r4
            !dir$ attributes forceinline :: analytic_sol_L2_gl5cm_f43_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: analytic_sol_L2_gl5cm_f43_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_1, v4r4_2, v4r4_1over2 
            type(XMM4r4_t),     intent(in) :: dn0 
            type(XMM4r4_t),     intent(in) :: beta 
            type(XMM4r4_t),     intent(in) :: z0 
            type(XMM4r4_t),     intent(in) :: H 
            type(XMM4r4_t)                 :: L2
            type(XMM4r4_t),     parameter  :: a =  XMM4r4_t(6378.0_sp)
            type(XMM4r4_t),     parameter  :: C314159265358979323846264338328 =   &
                                                          XMM4r4_t(3.14159265358979323846264338328_sp)
            type(XMM4r4_t), automatic :: piba2, ctgz0, 
            type(XMM4r4_t), automatic :: bactgz0, exp1 
            type(XMM4r4_t), automatic :: t0, t1 
            type(XMM4r4_t), automatic :: trm1, trm2, trm3 
            !dir$ attributes align : 16 :: a 
            !dir$ attributes align : 16 :: C314159265358979323846264338328
            !dir$ attributes align : 16 :: piba2 
            !dir$ attributes align : 16 :: ctgz0 
            !dir$ attributes align : 16 :: bactgz0 
            !dir$ attributes align : 16 :: exp1 
            !dir$ attributes align : 16 :: t0 
            !dir$ attributes align : 16 :: t1 
            !dir$ attributes align : 16 :: trm1 
            !dir$ attributes align : 16 :: trm2 
            !dir$ attributes align : 16 :: trm3 
            piba2.v  = sqrt((C314159265358979323846264338328.v*beta.v*a.v)*v4r4_1over2.v)
            ctgz0.v  = v4r4_1.v/tan(z0.v)
            bactgz0.v= beta.v*a.v*ctgz0.v*ctgz0.v 
            exp1.v   = exp(bactgz0.v*v4r4_1over2.v)
            trm1.v   = dn0.v*sqrt(piba2.v)*ctgz0.v 
            t0       = prob_integral_r4(sqrt(bactgz0.v+v4r4_2.v*beta.v*H.v))
            t1       = prob_integral_r4(sqrt(bactgz0.v))
            trm3.v   = t0.v-t1.v 
            trm2.v   = trm1.v*exp1.v 
            L2.v     = trm2.v*trm3.v        
      end function analytic_sol_L2_gl5cm_f43_xmm4r4

      pure function analytic_sol_L3_gl5cm_f43_xmm4r4(dn0,beta,z0,H) result(L3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)            
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L3_gl5cm_f43_xmm4r4
            !dir$ attributes forceinline :: analytic_sol_L3_gl5cm_f43_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: analytic_sol_L3_gl5cm_f43_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_1, v4r4_2
            type(XMM4r4_t),     intent(in) :: dn0 
            type(XMM4r4_t),     intent(in) :: beta 
            type(XMM4r4_t),     intent(in) :: z0 
            type(XMM4r4_t),     intent(in) :: H 
            type(XMM4r4_t)                 :: L1 
            type(XMM4r4_t),     parameter  :: a =  XMM4r4_t(6378.0_sp)
            type(XMM4r4_t),     parameter  :: C314159265358979323846264338328 =   &
                                                          XMM4r4_t(3.14159265358979323846264338328_sp)
            type(XMM4r4_t), automatic :: piba2, ctgz0, 
            type(XMM4r4_t), automatic :: bactgz0, exp1 
            type(XMM4r4_t), automatic :: t0, t1 
            type(XMM4r4_t), automatic :: trm1, trm2, trm3 
            !dir$ attributes align : 16 :: a 
            !dir$ attributes align : 16 :: C314159265358979323846264338328
            !dir$ attributes align : 16 :: piba2 
            !dir$ attributes align : 16 :: ctgz0 
            !dir$ attributes align : 16 :: bactgz0 
            !dir$ attributes align : 16 :: exp1 
            !dir$ attributes align : 16 :: t0 
            !dir$ attributes align : 16 :: t1 
            !dir$ attributes align : 16 :: trm1 
            !dir$ attributes align : 16 :: trm2 
            !dir$ attributes align : 16 :: trm3 
            piba2.v  = sqrt(C314159265358979323846264338328.v*beta.v*a.v)
            ctgz0.v  = v4r4_1.v/tan(z0.v)
            bactgz0.v= beta.v*a.v*ctgz0.v*ctgz0.v 
            exp1.v   = exp(bactgz0.v)
            trm1.v   = dn0.v*sqrt(piba2.v)*ctgz0.v 
            t0       = prob_integral_r4(sqrt(v4r4_2.v*bactgz0.v+v4r4_4.v*beta.v*H.v))
            t1       = prob_integral_r4(sqrt(v4r4_2.v*bactgz0.v))
            trm3.v   = t0.v-t1.v 
            trm2.v   = trm1.v*exp1.v 
            L2.v     = trm2.v*trm3.v        
      end function analytic_sol_L3_gl5cm_f43_xmm4r4

      pure function refraction_angle_for_gl5cm_f41_xmm4r4(n0,nh,z0,beta,dn0,H) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_for_gl5cm_f41_xmm4r4
            !dir$ attributes forceinline :: refraction_angle_for_gl5cm_f41_xmm4r4
             !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refraction_angle_for_gl5cm_f41_xmm4r4
#endif  
             use mod_vecconsts, only : v4r4_1 
             type(XMM4r4_t),  intent(in) :: n0 
             type(XMM4r4_t),  intent(in) :: nh 
             type(XMM4r4_t),  intent(in) :: z0 
             type(XMM4r4_t),  intent(in) :: beta 
             type(XMM4r4_t),  intent(in) :: dn0 
             type(XMM4r4_t),  intent(in) :: H 
             type(XMM4r4_t)              :: alpha 
             type(XMM4r4_t), parameter   :: a = XMM4r4_t(6378.0_sp)
             type(XMM4r4_t), automatic   :: L1, L2, L3 
             type(XMM4r4_t), automatic   :: ctgz0, lnn0nh
             type(XMM4r4_t), automatic   :: ssecz, badn0 
             type(XMM4r4_t), automatic   :: t0, t1
             type(XMM4r4_t), automatic   :: trm1, trm2, trm3 
             !dir$ attributes align : 16 :: a 
             !dir$ attributes align : 16 :: L1 
             !dir$ attributes align : 16 :: L2 
             !dir$ attributes align : 16 :: L3 
             !dir$ attributes align : 16 :: ctgz0 
             !dir$ attributes align : 16 :: lnn0nh 
             !dir$ attributes align : 16 :: ssecz 
             !dir$ attributes align : 16 :: badn0 
             !dir$ attributes align : 16 :: t0 
             !dir$ attributes align : 16 :: t1 
             !dir$ attributes align : 16 :: trm1 
             !dir$ attributes align : 16 :: trm2 
             !dir$ attributes align : 16 :: trm3 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j    
#endif 
             L1  =  analytic_sol_L1_gl5cm_f42_xmm4r4(dn0,beta,z0,H)  
             L2  =  analytic_sol_L2_gl5cm_f43_xmm4r4(dn0,beta,z0,H)   
             L3  =  analytic_sol_L3_gl5cm_f43_xmm4r4(dn0,beta,z0,H)  
#if (GMS_EXPLICIT_VECTORIZE) == 1
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                 badn0.v(j)  = beta.v(j)*a.v(j)*dn0.v(j) 
                 ctgz0.v(j)  = v4r4_1.v(j)/tan(z0.v(j))
                 lnn0nh.v(j) = log(n0.v(j)/nh.v(j))
                 t0.v(j)     = v4r4_1.v(j)/cos(z0.v(j))
                 ssecz.v(j)  = t0.v(j)*t0.v(j) 
                 t1.v(j)     = ctgz0.v(j)*ssecz.v(j) 
                 trm1.v(j)   = -ctgz0.v(j)*lnn0nh.v(j)+L1.v(j) 
                 trm2.v(j)   = t1.v(j)*L2.v(j) 
                 trm3.v(j)   = badn0.v(j)*t1.v(j)*(L3.v(j)-L2.v(j))
                 alpha.v(j)  = trm1.v(j)+trm2.v(j)+trm3.v(j) 
             end do 
#else 
                 badn0.v  = beta.v*a.v*dn0.v 
                 ctgz0.v  = v4r4_1.v/tan(z0.v)
                 lnn0nh.v = log(n0.v/nh.v)
                 t0.v     = v4r4_1.v/cos(z0.v)
                 ssecz.v  = t0.v*t0.v 
                 t1.v     = ctgz0.v*ssecz.v 
                 trm1.v   = -ctgz0.v*lnn0nh.v+L1.v 
                 trm2.v   = t1.v*L2.v 
                 trm3.v   = badn0.v*t1.v*(L3.v-L2.v)
                 alpha.v  = trm1.v+trm2.v+trm3.v 
#endif 
      end function refraction_angle_for_gl5cm_f41_xmm4r4

        !показатель преломления ионосферы в среднем
      pure function refractive_idx_lo_ionosphere_f412_xmm4r4(h,d,f,Nmf) result(n)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractive_idx_lo_ionosphere_f412_xmm4r4
            !dir$ attributes forceinline :: refractive_idx_lo_ionosphere_f412_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refractive_idx_lo_ionosphere_f412_xmm4r4
#endif  
             use mod_vecconsts, only : v4r4_1, v4r4_2 
             type(XMM4r4_t),        intent(in) :: h 
             type(XMM4r4_t),        intent(in) :: d 
             type(XMM4r4_t),        intent(in) :: f 
             type(XMM4r4_t),        intent(in) :: Nmf 
             type(XMM4r4_t)                    :: n 
             type(XMM4r4_t),        parameter  :: C808 = XMM4r4_t(80.0_sp)
             type(XMM4r4_t),        automatic  :: dnm, hd 
             type(XMM4r4_t),        automatic  :: hhdd, fcr 
             !dir$ attributes align : 16 :: dnm 
             !dir$ attributes align : 16 :: hd 
             !dir$ attributes align : 16 :: hhdd 
             !dir$ attributes align : 16 :: fcr 
#if (GMS_EXPLICIT_VECTORIZE) == 1
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#endif             
             do j=0,3  
                 fcr.v(j) = sqrt(C808.v(j)*Nmf.v(j))
                 hd.v(j)  = h.v(j)/d.v(j) 
                 dnm.v(j) = fcr.v(j)*fcr.v(j)/(v4r4_2.v(j)*f.v(j)*f.v(j))
                 hhdd.v(j)= hd.v(j)*hd.v(j) 
                 n.v(j)   = v4r4_1.v(j)-dnm.v(j)*(v4r4_2.v(j)*hd.v(j)-hhdd.v(j))
             end do 
#else 
                 fcr.v = sqrt(C808.v*Nmf.v)
                 hd.v  = h.v/d.v 
                 dnm.v = fcr.v*fcr.v/(v4r4_2.v*f.v*f.v)
                 hhdd.v= hd.v*hd.v 
                 n.v   = v4r4_1.v-dnm.v*(v4r4_2.v*hd.v-hhdd.v)
#endif
      end function refractive_idx_lo_ionosphere_f412_xmm4r4

      pure function refractive_idx_hi_ionosphere_f413_xmm4r4(h,d,f,Nmf,beta) result(n)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractive_idx_hi_ionosphere_f413_xmm4r4
            !dir$ attributes forceinline :: refractive_idx_hi_ionosphere_f413_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refractive_idx_hi_ionosphere_f413_xmm4r4
#endif  
             use mod_vecconsts, only : v4r4_1, v4r4_2 
             type(XMM4r4_t),        intent(in) :: h 
             type(XMM4r4_t),        intent(in) :: d 
             type(XMM4r4_t),        intent(in) :: f 
             type(XMM4r4_t),        intent(in) :: Nmf 
             type(XMM4r4_t),        intent(in) :: beta
             type(XMM4r4_t)                    :: n 
             type(XMM4r4_t),        parameter  :: C808 = XMM4r4_t(80.0_sp)
             type(XMM4r4_t),        automatic  :: dnm, fcr 
             type(XMM4r4_t),        automatic  :: earg, exp1  
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)   
             !dir$ attributes align : 16 :: dnm 
             !dir$ attributes align : 16 :: fcr
             !dir$ attributes align : 16 :: earg 
             !dir$ attributes align : 16 :: exp1
#endif
             integer(kind=i4) :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#elif defined(__GNUC__) && !defined(__INTEL_COMPILER)
             !$omp simd simdlen(4) linear(j:1)
#endif
             do j=0,3  
                 fcr.v(j) = sqrt(C808.v(j)*Nmf.v(j))
                 dnm.v(j) = fcr.v(j)*fcr.v(j)/(v4r4_2.v(j)*f.v(j)*f.v(j))
                 earg.v(j)= -beta.v(j)*(h.v(j)-d.v(j))
                 exp1.v(j)= exp(earg.v(j))
                 n.v(j)   = v4r4_1.v(j)-dnm.v(j)*exp1.v(j) 
             end do 
      end function refractive_idx_hi_ionosphere_f413_xmm4r4

        ! Compute `delta-nM` value, formula 4.14, page: 77
      pure function compute_delnM_f414_xmm4r4(fc,Nmf) result(dnM)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_delnM_f414_xmm4r4
            !dir$ attributes forceinline :: compute_delnM_f414_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_delnM_f414_xmm4r4
#endif  
            use mod_vecconsts, only : v4r4_2 
            type(XMM4r4_t),      intent(in) :: fc 
            type(XMM4r4_t),      intent(in) :: Nmf 
            type(XMM4r4_t)                  :: dnM 
            type(XMM4r4_t),      parameter  :: C808 = XMM4r4_t(80.0_sp)
            type(XMM4r4_t),      automatic  :: fcr, sfc 
            integer(kind=i4)                :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)   
             !dir$ attributes align : 16 :: C808
             !dir$ attributes align : 16 :: sfc
             !dir$ attributes align : 16 :: fcr
#endif 
            
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#elif defined(__GNUC__) && !defined(__INTEL_COMPILER)
             !$omp simd simdlen(4) linear(j:1)
#endif
             do j=0,3  
                 sfc.v(j) = v4r4_2.v(j)*fc.v(j)*fc.v(j) 
                 fcr.v(j) = sqrt(C808.v(j)*Nmf.v(j))
                 dnM.v(j) = fcr.v(j)*fcr.v(j)/sfc.v(j)  
             end do 
      end function compute_delnM_f414_xmm4r4

      pure function compute_delnEps_f421_xmm4r4(fc,Nmf,beta,d) result(dnE)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_delnEps_f421_xmm4r4
            !dir$ attributes forceinline :: compute_delnEps_f421_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_delnEps_f421_xmm4r4
#endif  
            type(XMM4r4_t),      intent(in) :: fc 
            type(XMM4r4_t),      intent(in) :: Nmf 
            type(XMM4r4_t),      intent(in) :: beta 
            type(XMM4r4_t),      intent(in) :: d 
            type(XMM4r4_t)                  :: dnE 
            type(XMM4r4_t),      automatic  :: dnM, earg 
            type(XMM4r4_t),      automatic  :: exp1 
            integer(kind=i4)                :: j 
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)   
             !dir$ attributes align : 16 :: dnm
             !dir$ attributes align : 16 :: earg
             !dir$ attributes align : 16 :: exp1
#endif 
             dnM.v = compute_delnM_f414_xmm4r4(fc,Nmf)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#elif defined(__GNUC__) && !defined(__INTEL_COMPILER)
             !$omp simd simdlen(4) linear(j:1)
#endif
             do j=0,3 
                 earg.v(j) = beta.v(j)*d.v(j) 
                 exp1.v(j) = exp(earg.v(j))
                 dnE.v(j)  = dnM.v(j)*exp1.v(j)
             end do  
      end function compute_delnEps_f421_xmm4r4

      pure function analytic_sol_L1_lo_ionosphere_f418_xmm4r4(fc,Nmf,z0,d,R0) result(L1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_lo_ionosphere_f418_xmm4r4
            !dir$ attributes forceinline :: analytic_sol_L1_lo_ionosphere_f418_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: analytic_sol_L1_lo_ionosphere_f418_xmm4r4
#endif  
            use mod_vecconsts,   only : v4r4_1, v4r4_n2, v4r4_2, v4r4_3, v4r4_5, &
                                        v4r4_10, v4r4_19 
            type(XMM4r4_t),      intent(in) :: fc 
            type(XMM4r4_t),      intent(in) :: Nmf 
            type(XMM4r4_t),      intent(in) :: z0 
            type(XMM4r4_t),      intent(in) :: d 
            type(XMM4r4_t),      intent(in) :: R0 
            type(XMM4r4_t)                  :: L1 
            type(XMM4r4_t),      automatic  :: delnM, m
            type(XMM4r4_t),      automatic  :: c2mm, c12m 
            type(XMM4r4_t),      automatic  :: ctgz0, cos2z0
            type(XMM4r4_t),      automatic  :: c3m, tgz0 
            type(XMM4r4_t),      automatic  :: c5mm, sqr 
            type(XMM4r4_t),      automatic  :: t0, t1 
            type(XMM4r4_t),      automatic  :: t2, t3 
            type(XMM4r4_t),      automatic  :: trm1, trm2
            type(XMM4r4_t),      automatic  :: trm3, trm4 
            integer(kind=i4)                :: j
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)   
             !dir$ attributes align : 16 :: delnM
             !dir$ attributes align : 16 :: m 
             !dir$ attributes align : 16 :: c2mm
             !dir$ attributes align : 16 :: c12m 
             !dir$ attributes align : 16 :: ctgz0
             !dir$ attributes align : 16 :: cos2z0 
             !dir$ attributes align : 16 :: c3m
             !dir$ attributes align : 16 :: tgz0 
             !dir$ attributes align : 16 :: c5mm
             !dir$ attributes align : 16 :: sqr 
             !dir$ attributes align : 16 :: t0 
             !dir$ attributes align : 16 :: t1 
             !dir$ attributes align : 16 :: t2 
             !dir$ attributes align : 16 :: t3 
             !dir$ attributes align : 16 :: trm1 
             !dir$ attributes align : 16 :: trm2 
             !dir$ attributes align : 16 :: trm3 
             !dir$ attributes align : 16 :: trm4
#endif       
             dnM.v = compute_delnM_f414_xmm4r4(fc,Nmf)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)                  
             !dir$ loop_count(4)
             !dir$ vector aligned
             !dir$ vector vectorlength(4)
             !dir$ vector always
#elif defined(__GNUC__) && !defined(__INTEL_COMPILER)
             !$omp simd simdlen(4) linear(j:1)
#endif
             do j=0,3 
                  tgz0.v(j)  = tan(z0.v(j))
                  ctgz0.v(j) = v4r4_1.v(j)/tgz0.v(j) 
                  m.v(j)     = (tgz0.v(j)*tgz0.v(j)*d.v(j))/R0.v(j)
                  sqr.v(j)   = sqrt(v4r4_1.v(j)+v4r4_2.v(j)*m.v(j))
                  t0.v(j)    = cos(z0.v(j))
                  cos2z0.v(j)= t0.v(j)*t0.v(j) 
                  t1.v(j)    = ctgz0.v(j)/cos2z0.v(j)
                  t2.v(j)    = (v4r4_n2.v(j)*delNm.v(j))/m.v(j)
                  c5mm.v(j)  = v4r4_5.v(j)*m.v(j)*m.v(j) 
                  trm1.v(j)  = t2.v(j)*t1.v(j) 
                  c3m.v(j)   = v4r4_3.v(j)*m.v(j)
                  trm2.v(j)  = (v4r4_1.v(j)-sqr.v(j))+(v4r4_1.v(j)/c3m.v(j))*  &
                                m.v(j)-v4r4_1.v(j)*sqr.v(j)+v4r4_1.v(j)
                  t2.v(j)    = (v4r4_2.v(j)*delnM.v(j)*delnM.v(j))/c5mm.v(j)
                  t3.v(j)    = tgz0.v(j)/cos2z0.v(j) 
                  trm3.v(j)  = t2.v(j)*t3.v(j) 
                  c2mm.v(j)  = v4r4_2.v(j)/(m.v(j)*m.v(j))
                  c12m.v(j)  = v4r4_12.v(j)/m.v(j) 
                  t1.v(j)    = c2mm.v(j)+c12m.v(j)+v4r4_12.v(j)+v4r4_6.v(j)*m.v(j) 
                  t2.v(j)    = c2mm.v(j)+(v4r4_10.v(j)/m.v(j))+v4r4_10.v(j) 
                  trm4.v(j)  = (v4r4_1.v(j)/sqr.v(j))*t1.v(j)-t2.v(j)
                  L1.v(j)    = trm1.v(j)*trm2.v(j)+trm3.v(j)*trm4.v(j) 
             end do      
      end function analytic_sol_L1_lo_ionosphere_f418_xmm4r4







end module atmos_refraction_xmm4r4