
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
    
















end module atmos_refraction_xmm4r4