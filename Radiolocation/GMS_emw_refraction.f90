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

module emw_refraction


!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         emw_refraction
 !          
 !          Purpose:
 !                        Calculation of  EM wave refraction in the Earth atmopshere.
 !                        Various characteristics and formulae of atmospheric refraction (radio waves and visible light/IR wavelengths)  
 !                        Based mainly on      Колосов М.А., Шабельников А.В. - Рефракция электромагнитных волн в атмосферах Земли, Венеры и Марса-Советское Радио (1976)    
 !                       
 !          History:
 !                        Date: 29-12-2024
 !                        Time: 13:11 GMT+2
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
   
   use mod_kinds,    only : i4,sp,dp

   public
   implicit none

     ! Major version
     integer(kind=i4),  parameter :: EMW_REFRACTION_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: EMW_REFRACTION_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: EMW_REFRACTION_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: EMW_REFRACTION_FULLVER =   &
            1000*EMW_REFRACTION_MAJOR+100*EMW_REFRACTION_MINOR+10*EMW_REFRACTION_MICRO
     ! Module creation date
     character(*),        parameter :: EMW_REFRACTION_CREATE_DATE = "29-12-2024 13:13 +00200 (SUN 29 DEC 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: EMW_REFRACTION_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: EMW_REFRACTION_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: EMW_REFRACTION_SYNOPSIS    = "Calculation of EM Wave atmospheric refraction."

     
     ! IRI model output arrays
     type, public :: ionosphere_state_t
           
           integer(kind=i4)                         :: n_values
           real(kind=sp), allocatable, dimension(:) :: elec_dens    ! electron density in m-3
           real(kind=sp), allocatable, dimension(:) :: neut_tmp     ! neutral temperature in K
           real(kind=sp), allocatable, dimension(:) :: ion_tmp      ! ion temperature in K
           real(kind=sp), allocatable, dimension(:) :: elec_tmp     ! electron temperature in K
           real(kind=sp), allocatable, dimension(:) :: O_ion_d      ! O+ ion density in % or in m-3 
           real(kind=sp), allocatable, dimension(:) :: H_ion_d      ! H+ ion density in % or in m-3 
           real(kind=sp), allocatable, dimension(:) :: He_ion_d     ! He+ ion density in % or in m-3
           real(kind=sp), allocatable, dimension(:) :: O2_ion_d     ! O2+ ion density in % or in m-3 
           real(kind=sp), allocatable, dimension(:) :: NO_ion_d     ! NO+ ion density in % or in m-3
           real(kind=sp), allocatable, dimension(:) :: ion_dens     ! Cluster ion density in % or in m-3
           real(kind=sp), allocatable, dimension(:) :: N_ion_d      ! N+ ion density in % or in m-3 
     end type ionosphere_state_t
    
     
     contains

     ! Formula 2.43, page 46
     elemental function n_refract_tht_f243_r4(n,n0,z,z0,r,R0,phi,phi0) result(n_over_tht)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_refract_tht_f243_r4
            !dir$ attributes forceinline :: n_refract_tht_f243_r4
           
#endif
!$omp declare simd(n_refract_tht_f243_r4)
            real(kind=sp),     intent(in) :: n    ! refractive index at dest (observation point)
            real(kind=sp),     intent(in) :: n0   ! refractive index at source
            real(kind=sp),     intent(in) :: z    ! 'z' angle at dest
            real(kind=sp),     intent(in) :: z0   ! 'z' angle at source
            real(kind=sp),     intent(in) :: r    ! upper limit of integration (radius)
            real(kind=sp),     intent(in) :: R0   ! lower limit of integration (radius)
            real(kind=sp),     intent(in) :: phi  ! 'phi' angle at dest
            real(kind=sp),     intent(in) :: phi0 
            real(kind=sp) :: n_over_tht 
            ! Locals
            real(kind=sp), automatic :: tgz, tgz0, tgphi, tgphi0 
            real(kind=sp), automatic :: num_d, num_s, den_d, den_s 
            real(kind=sp), automatic :: rat_s, rat_d 
            real(kind=sp), automatic :: stgz, stgphi, stgz0, stgphi0
            tgz    = tan(z)
            stgz   = tgz*tgz
            tgz0   = tan(z0)
            stgz0  = tgz0*tgz0
            num_d  = n*r*tgz 
            tgphi  = tan(phi)
            stgphi = tgphi*tgphi 
            tgphi0 = tan(phi0)
            stgphi0= tgphi0*tgphi0
            num_s  = n0*R0*tgz0 
            den_d  = sqrt(1.0_sp+stgz+stgphi) 
            den_s  = sqrt(1.0_sp+stgz0+stgphi0)
            rat_s  = num_s/den_s 
            rat_d  = num_d/den_d 
            n_over_tht = rat_d-rat_s 
     end function n_refract_tht_f243_r4
    
     
     elemental function n_refract_tht_f243_r8(n,n0,z,z0,r,R0,phi,phi0) result(n_over_tht)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_refract_tht_f243_r8
            !dir$ attributes forceinline :: n_refract_tht_f243_r8
            
#endif
!$omp declare simd(n_refract_tht_f243_r8)
            real(kind=dp),     intent(in) :: n    ! refractive index at dest (observation point)
            real(kind=dp),     intent(in) :: n0   ! refractive index at source
            real(kind=dp),     intent(in) :: z    ! 'z' angle at dest
            real(kind=dp),     intent(in) :: z0   ! 'z' angle at source
            real(kind=dp),     intent(in) :: r    ! upper limit of integration (radius)
            real(kind=dp),     intent(in) :: R0   ! lower limit of integration (radius)
            real(kind=dp),     intent(in) :: phi  ! 'phi' angle at dest
            real(kind=dp),     intent(in) :: phi0 
            real(kind=dp) :: n_over_tht 
            ! Locals
            real(kind=dp), automatic :: tgz, tgz0, tgphi, tgphi0 
            real(kind=dp), automatic :: num_d, num_s, den_d, den_s 
            real(kind=dp), automatic :: rat_s, rat_d 
            real(kind=dp), automatic :: stgz, stgphi, stgz0, stgphi0
            tgz    = tan(z)
            stgz   = tgz*tgz
            tgz0   = tan(z0)
            stgz0  = tgz0*tgz0
            num_d  = n*r*tgz 
            tgphi  = tan(phi)
            stgphi = tgphi*tgphi 
            tgphi0 = tan(phi0)
            stgphi0= tgphi0*tgphi0
            num_s  = n0*R0*tgz0 
            den_d  = sqrt(1.0_dp+stgz+stgphi) 
            den_s  = sqrt(1.0_dp+stgz0+stgphi0)
            rat_s  = num_s/den_s 
            rat_d  = num_d/den_d 
            n_over_tht = rat_d-rat_s 
     end function n_refract_tht_f243_r8

     elemental function n_refract_phi_f243_r4(n,n0,z,z0,r,R0,phi,phi0) result(n_over_phi)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_refract_phi_f243_r4
            !dir$ attributes forceinline :: n_refract_phi_f243_r4
          
#endif
!$omp declare simd(n_refract_phi_f243_r4)
            real(kind=sp),     intent(in) :: n    ! refractive index at dest (observation point)
            real(kind=sp),     intent(in) :: n0   ! refractive index at source
            real(kind=sp),     intent(in) :: z    ! 'z' angle at dest
            real(kind=sp),     intent(in) :: z0   ! 'z' angle at source
            real(kind=sp),     intent(in) :: r    ! upper limit of integration (radius)
            real(kind=sp),     intent(in) :: R0   ! lower limit of integration (radius)
            real(kind=sp),     intent(in) :: phi  ! 'phi' angle at dest
            real(kind=sp),     intent(in) :: phi0 
            real(kind=sp) :: n_over_phi 
            ! Locals
            real(kind=sp), automatic :: tgz, tgz0, tgphi, tgphi0 
            real(kind=sp), automatic :: num_d, num_s, den_d, den_s 
            real(kind=sp), automatic :: rat_s, rat_d 
            real(kind=sp), automatic :: stgz, stgphi, stgz0, stgphi0
            tgz        = tan(z)
            stgz       = tgz*tgz
            tgz0       = tan(z0)
            stgz0      = tgz0*tgz0
            tgphi      = tan(phi)
            stgphi     = tgphi*tgphi 
            tgphi0     = tan(phi0)
            stgphi0    = tgphi0*tgphi0
            num_d      = n*r*tgphi 
            num_s      = n0*R0*tgphi0 
            den_d      = sqrt(1.0_sp+stgz+stgphi) 
            den_s      = sqrt(1.0_sp+stgz0+stgphi0)
            rat_s      = num_s/den_s 
            rat_d      = num_d/den_d 
            n_over_phi = rat_d-rat_s 
     end function n_refract_phi_f243_r4

     elemental function n_refract_phi_f243_r8(n,n0,z,z0,r,R0,phi,phi0) result(n_over_phi)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_refract_phi_f243_r8
            !dir$ attributes forceinline :: n_refract_phi_f243_r8
           
#endif
!$omp declare simd(n_refract_phi_f243_r8)
            real(kind=dp),     intent(in) :: n    ! refractive index at dest (observation point)
            real(kind=dp),     intent(in) :: n0   ! refractive index at source
            real(kind=dp),     intent(in) :: z    ! 'z' angle at dest
            real(kind=dp),     intent(in) :: z0   ! 'z' angle at source
            real(kind=dp),     intent(in) :: r    ! upper limit of integration (radius)
            real(kind=dp),     intent(in) :: R0   ! lower limit of integration (radius)
            real(kind=dp),     intent(in) :: phi  ! 'phi' angle at dest
            real(kind=dp),     intent(in) :: phi0 
            real(kind=dp) :: n_over_phi 
            ! Locals
            real(kind=dp), automatic :: tgz, tgz0, tgphi, tgphi0 
            real(kind=dp), automatic :: num_d, num_s, den_d, den_s 
            real(kind=dp), automatic :: rat_s, rat_d 
            real(kind=dp), automatic :: stgz, stgphi, stgz0, stgphi0
            tgz        = tan(z)
            stgz       = tgz*tgz
            tgz0       = tan(z0)
            stgz0      = tgz0*tgz0
            tgphi      = tan(phi)
            stgphi     = tgphi*tgphi 
            tgphi0     = tan(phi0)
            stgphi0    = tgphi0*tgphi0
            num_d      = n*r*tgphi 
            num_s      = n0*R0*tgphi0 
            den_d      = sqrt(1.0_dp+stgz+stgphi) 
            den_s      = sqrt(1.0_dp+stgz0+stgphi0)
            rat_s      = num_s/den_s 
            rat_d      = num_d/den_d 
            n_over_phi = rat_d-rat_s 
     end function n_refract_phi_f243_r8


     !Радиус кривизны траектории луча, formula 2.51, page: 47
     
     elemental function rad_ray_curvature_f251_r4(n,z,dndr) result(rho)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: rad_ray_curvature_f251_r4
            !dir$ attributes forceinline :: rad_ray_curvature_f251_r4
            
#endif
!$omp declare simd(rad_ray_curvature_f251_r4)
            real(kind=sp),  intent(in) :: n ! refractive index
            real(kind=sp),  intent(in) :: z ! angle
            real(kind=sp),  intent(in) :: dndr ! derivative of refractive index at r
            real(kind=sp) :: rho 
            ! Locals
            real(kind=sp), automatic :: t0,sinz 
            sinz = sin(z)
            t0   = -n/sinz 
            rho  = t0*dndr 
     end function rad_ray_curvature_f251_r4

     elemental function rad_ray_curvature_f251_r8(n,z,dndr) result(rho)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: rad_ray_curvature_f251_r8
            !dir$ attributes forceinline :: rad_ray_curvature_f251_r8
           
#endif
!$omp declare simd(rad_ray_curvature_f251_r8)
            real(kind=dp),  intent(in) :: n ! refractive index
            real(kind=dp),  intent(in) :: z ! angle
            real(kind=dp),  intent(in) :: dndr ! derivative of refractive index at r
            real(kind=dp) :: rho 
            ! Locals
            real(kind=dp), automatic :: t0,sinz 
            sinz = sin(z)
            t0   = -n/sinz 
            rho  = t0*dndr 
     end function rad_ray_curvature_f251_r8

     !относителыную кривизну по-1
     !верхности Земли и траектории волны, formula: 2.54, page: 48
     
     elemental function k_relative_f254_r4(n,z,dndr) result(k_rel)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: k_relative_f254_r4
            !dir$ attributes forceinline :: k_relative_f254_r4
          
#endif   
!$omp declare simd(k_relative_f254_r4)
            real(kind=sp),  intent(in) :: n ! refractive index
            real(kind=sp),  intent(in) :: z ! angle
            real(kind=sp),  intent(in) :: dndr ! derivative of refractive index at r
            real(kind=sp) :: k_rel 
            real(kind=sp), parameter :: inv_erad = 0.00015678896205707118218877391_sp
            ! Locals
            real(kind=sp), automatic :: inv_rho
            inv_rho = 1.0_sp/rad_ray_curvature_f251_r4(n,z,dndr)
            k_rel   = inv_erad*inv_rho
     end function k_relative_f254_r4
 
     elemental function k_relative_f254_r8(n,z,dndr) result(k_rel)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: k_relative_f254_r8
            !dir$ attributes forceinline :: k_relative_f254_r8
            
#endif   
!$omp declare simd(k_relative_f254_r8)
            real(kind=dp),  intent(in) :: n ! refractive index
            real(kind=dp),  intent(in) :: z ! angle
            real(kind=dp),  intent(in) :: dndr ! derivative of refractive index at r
            real(kind=dp) :: k_rel 
            real(kind=dp), parameter :: inv_erad = 0.00015678896205707118218877391_dp
            ! Locals
            real(kind=dp), automatic :: inv_rho
            inv_rho = 1.0_dp/rad_ray_curvature_f251_r8(n,z,dndr)
            k_rel   = inv_erad*inv_rho
     end function k_relative_f254_r8

     ! отношения радиуса кривизны траекторий
     ! луча к радиусу Земли:, formula 2.67, page: 52 
     elemental function rho_to_a_f267_r4(dndh) result(R)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: rho_to_a_f267_r4
            !dir$ attributes forceinline :: rho_to_a_f267_r4
           
#endif  
!$omp declare simd(rho_to_a_f267_r4)
            real(kind=sp),   intent(in) :: dndh ! derivative of refractive index
            real(kind=sp) :: R 
            real(kind=sp), parameter :: inv_erad = -0.00015678896205707118218877391_sp 
            R = inv_erad*dndh 
     end function rho_to_a_f267_r4

       elemental function rho_to_a_f267_r8(dndh) result(R)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: rho_to_a_f267_r8
            !dir$ attributes forceinline :: rho_to_a_f267_r8
          
#endif  
!$omp declare simd(rho_to_a_f267_r8)
            real(kind=dp),   intent(in) :: dndh ! derivative of refractive index
            real(kind=dp) :: R 
            real(kind=dp), parameter :: inv_erad = -0.00015678896205707118218877391_dp 
            R = inv_erad*dndh 
     end function rho_to_a_f267_r8 
   
!Усредненная зависимость показателя преломления от 
!высоты, formula: 1.45, page 29

       elemental function n_avg_h_f145_r4(dn0,beta,h) result(nah)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_avg_h_f145_r4
            !dir$ attributes forceinline :: n_avg_h_f145_r4
           
#endif  
!$omp declare simd(n_avg_h_f145_r4)
            real(kind=sp),  intent(in) :: dn0  ! coefficient of refreaction near the Earth surface i.e. dn0 = (240*10e-6->380*10e-6)
            real(kind=sp),  intent(in) :: beta ! coefficient describing the diminishing of 'n' as function of height, i.e. 0.10->0.14 1/km
            real(kind=sp),  intent(in) :: h    
            real(kind=sp) :: nah 
            real(kind=sp), automatic :: earg,t0 
            t0   = 1.0_sp+dn0 
            earg = -beta*h 
            nah  = t0*exp(earg) 
       end function n_avg_h_f145_r4

       elemental function n_avg_h_f145_r8(dn0,beta,h) result(nah)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_avg_h_f145_r8
            !dir$ attributes forceinline :: n_avg_h_f145_r8
            
#endif 
!$omp declare simd(n_avg_h_f145_r8)
            real(kind=dp),  intent(in) :: dn0  ! coefficient of refreaction near the Earth surface i.e. dn0 = (240*10e-6->380*10e-6)
            real(kind=dp),  intent(in) :: beta ! coefficient describing the diminishing of 'n' as function of height, i.e. 0.10->0.14 1/km
            real(kind=dp),  intent(in) :: h    
            real(kind=dp) :: nah 
            real(kind=dp), automatic :: earg,t0 
            t0   = 1.0_dp+dn0 
            earg = -beta*h 
            nah  = t0*exp(earg) 
       end function n_avg_h_f145_r8

       !связь между величинами dn0 , beta, formula 1.46, page: 29
       elemental function approx_beta_coeff_f146_r4(dn0) result(beta)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: approx_beta_coeff_f146_r4
            !dir$ attributes forceinline :: approx_beta_coeff_f146_r4
            
#endif 
!$omp declare simd(approx_beta_coeff_f146_r4) 
            real(kind=sp),  intent(in) :: dn0  ! coefficient of refreaction near the Earth surface i.e. dn0 = (240*10e-6->380*10e-6)
            real(kind=sp) :: beta 
            real(kind=sp), automatic :: t0, earg 
            t0   = 0.00000732_sp/dn0 
            earg = 5577.0_sp*dn0 
            beta = t0*exp(earg)  
       end function approx_beta_coeff_f146_r4

    !связь между величинами dn0 , beta, formula 1.46, page: 29
       elemental function approx_beta_coeff_f146_r8(dn0) result(beta)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: approx_beta_coeff_f146_r8
            !dir$ attributes forceinline :: approx_beta_coeff_f146_r8
           
#endif 
!$omp declare simd(approx_beta_coeff_f146_r8)  
            real(kind=dp),  intent(in) :: dn0  ! coefficient of refreaction near the Earth surface i.e. dn0 = (240*10e-6->380*10e-6)
            real(kind=dp) :: beta 
            real(kind=dp), automatic :: t0, earg 
            t0   = 0.00000732_dp/dn0 
            earg = 5577.0_dp*dn0 
            beta = t0*exp(earg)  
       end function approx_beta_coeff_f146_r8

        elemental function prob_integral_r4(x) result(res)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: prob_integral_r4
            !dir$ attributes forceinline :: prob_integral_r4
#endif
!$omp declare simd(prob_integral_r4)
             real(kind=sp), intent(in) :: x 
             real(kind=sp) :: res 
             real(kind=sp), parameter :: C0707106781186547524400844362105 = 0.707106781186547524400844362105_sp
             res = 0.0_sp 
             res = 0.5_sp*(1.0_sp+erf(x*C0707106781186547524400844362105))
       end function prob_integral_r4

       elemental function prob_integral_r8(x) result(res)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: prob_integral_r8
            !dir$ attributes forceinline :: prob_integral_r8
#endif
!$omp declare simd(prob_integral_r8)
             real(kind=dp), intent(in) :: x 
             real(kind=dp) :: res 
             real(kind=dp), parameter :: C0707106781186547524400844362105 = 0.707106781186547524400844362105_dp
             res = 0.0_dp 
             res = 0.5_dp*(1.0_dp+erf(x*C0707106781186547524400844362105))
       end function prob_integral_r8

       !формулу (3.35) для расчета регулярной
       !рефракции оптических волн в земной атмосфере.
       ! formula 3.37, page: 68
       elemental function analytic_sol_L1_f337_r4(beta,dn0,z0,H) result(L1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_f337_r4
            !dir$ attributes forceinline :: analytic_sol_L1_f337_r4
#endif 
!$omp declare simd(analytic_sol_L1_f337_r4) 
            real(kind=sp),  intent(in) :: beta 
            real(kind=sp),  intent(in) :: dn0 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: H 
            real(kind=sp) :: L1 
            real(kind=sp), parameter :: a = 6378.0_sp
            real(kind=sp), automatic :: cosz0,ctgz0,ea1
            real(kind=sp), automatic :: ea2,exp1,exp2,num2
            real(kind=sp), automatic :: den2,num1,den1,sdn0
            real(kind=sp), automatic :: stgz0,rat1,rat2 
            ea1   = -2.0_sp*beta*H 
            ea2   = -beta*H 
            ctgz0 = 1.0_sp/tan(z0)
            sdn0  = dn0*dn0 
            exp1  = exp(ea1)
            num1  = beta*a*sdn0*ctgz0
            cosz0 = cos(z0)
            den1  = cosz0*cosz0 
            exp2  = exp(ea2)
            rat1  = num1/den1 
            stgz0 = 2.0_sp*(tgz0*tgz0) 
            den2  = sqrt(1.0_sp+stgz0*(H/a))
            num2  = exp1-exp2 
            rat2  = num2/den2 
            L1    = rat1*rat2 
       end function analytic_sol_L1_f337_r4

       elemental function analytic_sol_L1_f337_r8(beta,dn0,z0,H) result(L1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_f337_r8
            !dir$ attributes forceinline :: analytic_sol_L1_f337_r8
#endif  
!$omp declare simd(analytic_sol_L1_f337_r8) 
            real(kind=dp),  intent(in) :: beta 
            real(kind=dp),  intent(in) :: dn0 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: H 
            real(kind=dp) :: L1 
            real(kind=dp), parameter :: a = 6378.0_dp
            real(kind=dp), automatic :: cosz0,ctgz0,ea1
            real(kind=dp), automatic :: ea2,exp1,exp2,num2
            real(kind=dp), automatic :: den2,num1,den1,sdn0
            real(kind=dp), automatic :: stgz0,rat1,rat2 
            ea1   = -2.0_dp*beta*H 
            ea2   = -beta*H 
            ctgz0 = 1.0_dp/tan(z0)
            sdn0 = dn0*dn0 
            exp1 = exp(ea1)
            num1 = beta*a*sdn0*ctgz0
            cosz0= cos(z0)
            den1 = cosz0*cosz0 
            exp2 = exp(ea2)
            rat1 = num1/den1 
            stgz0= 2.0_dp*(tgz0*tgz0) 
            den2 = sqrt(1.0_dp+stgz0*(H/a))
            num2 = exp1-exp2 
            rat2 = num2/den2 
            L1   = rat1*rat2 
       end function analytic_sol_L1_f337_r8

       !формулa (3.35) для расчета регулярной
       !рефракции оптических волн в земной атмосфере.
       ! formula 3.41, page: 68
       elemental function analytic_sol_L2_f341_r4(dn0,beta,z0,H) result(L2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L2_f341_r4
            !dir$ attributes forceinline :: analytic_sol_L2_f341_r4
#endif  
!$omp declare simd(analytic_sol_L2_f341_r4)
            real(kind=sp),  intent(in) :: dn0 
            real(kind=sp),  intent(in) :: beta 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: H 
            real(kind=sp) :: L2 
            real(kind=sp), parameter :: C1253314137315500251207882642406 = 1.253314137315500251207882642406_sp
            real(kind=sp), parameter :: a = 6378.0_sp
            real(kind=sp), automatic :: sba, ctgz0, ba 
            real(kind=sp), automatic :: sctgz0, tbh, phi1, phi2 
            real(kind=sp), automatic :: exp1, bactgz0, t0, t1  
            sba    = sqrt(beta*a)
            ctgz0  = 1.0_sp/tan(z0)
            sctgz0 = ctgz0*ctgz0 
            bactgz0= beta*a*sctgz0 
            tbH    = 2.0_sp*beta*H 
            t0     = dn0*sqrt(beta*a*ctgz0)
            exp1   = exp(sctgz0*0.5_sp)* &
                     C1253314137315500251207882642406
            phi1   = prob_integral_r4(sqrt(bactgz0*tbH))
            phi2   = prob_integral_r4(sqrt(bactgz0))
            t1     = phi1-phi2 
            L2     = t0*exp1*t1 
       end function analytic_sol_L2_f341_r4

       elemental function analytic_sol_L2_f341_r8(dn0,beta,z0,H) result(L2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L2_f341_r8
            !dir$ attributes forceinline :: analytic_sol_L2_f341_r8
#endif  
!$omp declare simd(analytic_sol_L2_f341_r8)
            real(kind=dp),  intent(in) :: dn0 
            real(kind=dp),  intent(in) :: beta 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: H 
            real(kind=dp) :: L2 
            real(kind=dp), parameter :: C1253314137315500251207882642406 = 1.253314137315500251207882642406_dp
            real(kind=dp), parameter :: a = 6378.0_dp
            real(kind=dp), automatic :: sba, ctgz0, ba 
            real(kind=dp), automatic :: sctgz0, tbh, phi1, phi2 
            real(kind=dp), automatic :: exp1, bactgz0, t0, t1  
            sba    = sqrt(beta*a)
            ctgz0  = 1.0_dp/tan(z0)
            sctgz0 = ctgz0*ctgz0 
            bactgz0= beta*a*sctgz0 
            tbH    = 2.0_dp*beta*H 
            t0     = dn0*sqrt(beta*a*ctgz0)
            exp1   = exp(sctgz0*0.5_dp)* &
                     C1253314137315500251207882642406
            phi1   = prob_integral_r8(sqrt(bactgz0*tbH))
            phi2   = prob_integral_r8(sqrt(bactgz0))
            t1     = phi1-phi2 
            L2     = t0*exp1*t1 
       end function analytic_sol_L2_f341_r8

        !формулa (3.35) для расчета регулярной
       !рефракции оптических волн в земной атмосфере.
       ! formula 3.42, page: 68
       elemental function analytic_sol_L3_f342_r4(dn0,beta,z0,H) result(L2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L3_f342_r4
            !dir$ attributes forceinline :: analytic_sol_L3_f342_r4
#endif  
!$omp declare simd(analytic_sol_L3_f342_r4)
            real(kind=sp),  intent(in) :: dn0   ! refractive index near to earth surface
            real(kind=sp),  intent(in) :: beta  ! beta coefficient
            real(kind=sp),  intent(in) :: z0    ! angle of ray incoming to receiver
            real(kind=sp),  intent(in) :: H     ! height of raditaing source over the earth surface
            real(kind=sp) :: L2 
            real(kind=sp), parameter :: C1253314137315500251207882642406 = 1.253314137315500251207882642406_sp
            real(kind=sp), parameter :: a = 6378.0_sp
            real(kind=sp), automatic :: sba, ctgz0, ba 
            real(kind=sp), automatic :: sctgz0, tbh, phi1, phi2 
            real(kind=sp), automatic :: exp1, bactgz0, t0, t1  
            sba    = sqrt(2.0_sp*beta*a)
            ctgz0  = 1.0_sp/tan(z0)
            sctgz0 = ctgz0*ctgz0 
            bactgz0= 2.0_sp*beta*a*sctgz0 
            tbH    = 4.0_sp*beta*H 
            t0     = dn0*sqrt(beta*a*ctgz0)
            exp1   = exp(sctgz0)* &
                     C1253314137315500251207882642406
            phi1   = prob_integral_r4(sqrt(bactgz0+tbH))
            phi2   = prob_integral_r4(sqrt(bactgz0))
            t1     = phi1-phi2 
            L2     = t0*exp1*t1 
       end function analytic_sol_L3_f342_r4

         !формулa (3.35) для расчета регулярной
       !рефракции оптических волн в земной атмосфере.
       ! formula 3.42, page: 68
       elemental function analytic_sol_L3_f342_r8(dn0,beta,z0,H) result(L2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L3_f342_r8
            !dir$ attributes forceinline :: analytic_sol_L3_f342_r8
#endif 
!$omp declare simd(analytic_sol_L3_f342_r8) 
            real(kind=dp),  intent(in) :: dn0   ! refractive index near to earth surface
            real(kind=dp),  intent(in) :: beta  ! beta coefficient
            real(kind=dp),  intent(in) :: z0    ! angle of ray incoming to receiver
            real(kind=dp),  intent(in) :: H     ! height of raditaing source over the earth surface
            real(kind=dp) :: L2 
            real(kind=dp), parameter :: C1253314137315500251207882642406 = 1.253314137315500251207882642406_dp
            real(kind=dp), parameter :: a = 6378.0_dp
            real(kind=dp), automatic :: sba, ctgz0, ba 
            real(kind=dp), automatic :: sctgz0, tbh, phi1, phi2 
            real(kind=dp), automatic :: exp1, bactgz0, t0, t1  
            sba    = sqrt(2.0_dp*beta*a)
            ctgz0  = 1.0_dp/tan(z0)
            sctgz0 = ctgz0*ctgz0 
            bactgz0= 2.0_dp*beta*a*sctgz0 
            tbH    = 4.0_dp*beta*H 
            t0     = dn0*sqrt(beta*a*ctgz0)
            exp1   = exp(sctgz0)* &
                     C1253314137315500251207882642406
            phi1   = prob_integral_r8(sqrt(bactgz0+tbH))
            phi2   = prob_integral_r8(sqrt(bactgz0))
            t1     = phi1-phi2 
            L2     = t0*exp1*t1 
       end function analytic_sol_L3_f342_r8

       !Формула' (3.35) справедлива во всем диапазоне 
       !изменения зенитных углов (0 < z0 <90°) при любых 
       !зависимостях n(h).
       ! The angle of refraction.
       elemental function refraction_angle_f345_r4(n0,nh,z0,dn0,beta,H) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_f345_r4
            !dir$ attributes forceinline :: refraction_angle_f345_r4
#endif  
!$omp declare simd(refraction_angle_f345_r4)
            real(kind=sp),  intent(in) :: n0 
            real(kind=sp),  intent(in) :: nh 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: dn0 
            real(kind=sp),  intent(in) :: beta 
            real(kind=sp),  intent(in) :: H 
            real(kind=sp) :: alpha 
            real(kind=sp), parameter :: a = 6378.0_sp
            real(kind=sp), automatic :: ctgz0, ln0nh, ssecz0,badn0, ctgzsec0
            real(kind=sp), automatic :: t0, t1, t2 
            real(kind=sp), automatic :: L1, L2, L3 
            badn0    = beta*a*dn0 
            L1       = 0.0_sp 
            ctgz0    = 1.0_sp/tan(z0)
            L2       = 0.0_sp 
            ln0nh    = log(n0/nh) 
            L3       = 0.0_sp 
            t0       = 1.0_sp/sin(z0)
            ssecz0   = t0*t0 
            L1       = analytic_sol_L1_f337_r4(dn0,beta,z0,H) 
            t0       = -ctgz0*ln0nh+L1 
            ctgzsec0 = ctgz0*ssecz0
            L2       = analytic_sol_L2_f341_r4(dn0,beta,z0,H)
            t1       = ctgzsec0*L2 
            L3       = analytic_sol_L3_f342_r4(dn0,beta,z0,H)
            t2       = badn0*ctgzsec0*(L3-L2)
            alpha    = t0+t1+t2 
       end function refraction_angle_f345_r4

         elemental function refraction_angle_f345_r8(n0,nh,z0,dn0,beta,H) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_f345_r8
            !dir$ attributes forceinline :: refraction_angle_f345_r8
#endif  
!$omp declare simd(refraction_angle_f345_r8)
            real(kind=dp),  intent(in) :: n0 
            real(kind=dp),  intent(in) :: nh 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: dn0 
            real(kind=dp),  intent(in) :: beta 
            real(kind=dp),  intent(in) :: H 
            real(kind=dp) :: alpha 
            real(kind=dp), parameter :: a = 6378.0_dp
            real(kind=dp), automatic :: ctgz0, ln0nh, ssecz0,badn0, ctgzsec0
            real(kind=dp), automatic :: t0, t1, t2 
            real(kind=dp), automatic :: L1, L2, L3 
            badn0    = beta*a*dn0 
            L1       = 0.0_dp 
            ctgz0    = 1.0_dp/tan(z0)
            L2       = 0.0_dp 
            ln0nh    = log(n0/nh) 
            L3       = 0.0_dp 
            t0       = 1.0_dp/sin(z0)
            ssecz0   = t0*t0 
            L1       = analytic_sol_L1_f337_r8(dn0,beta,z0,H) 
            t0       = -ctgz0*ln0nh+L1 
            ctgzsec0 = ctgz0*ssecz0
            L2       = analytic_sol_L2_f341_r8(dn0,beta,z0,H)
            t1       = ctgzsec0*L2 
            L3       = analytic_sol_L3_f342_r8(dn0,beta,z0,H)
            t2       = badn0*ctgzsec0*(L3-L2)
            alpha    = t0+t1+t2 
       end function refraction_angle_f345_r8



       ! z0 близко к 90°.
       ! The angle of arrival close to horizon.
       ! formula 3.51, page: 70
       ! analytic solution L2 for angle near 90 (deg)
       elemental function analytic_sol_n90_L2_f351_r4(dn0,beta,z0) result(L2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_n90_L2_f351_r4
            !dir$ attributes forceinline :: analytic_sol_n90_L2_f351_r4
#endif  
!$omp declare simd(analytic_sol_n90_L2_f351_r4)
            real(kind=sp), intent(in) :: dn0 
            real(kind=sp), intent(in) :: beta 
            real(kind=sp), intent(in) :: z0 
            real(kind=sp) :: L2 
            real(kind=sp), parameter :: a = 6378.0_sp
            real(kind=sp), parameter :: C1253314137315500251207882642406 = 1.253314137315500251207882642406_sp 
            real(kind=sp), parameter :: C0318309886183790671537767526745 = 0.318309886183790671537767526745_sp
            real(kind=sp), automatic :: sba, tgz0, stgz0, ctgz0 
            real(kind=sp), automatic :: earg, exp1, t0, t1, strm 
            sba  = sqrt(beta*a)
            tgz0 = tan(z0)
            ctgz0= 1.0_sp/tan(z0)
            earg = beta*a/(2.0_sp*tgz0*tgz0)
            exp1 = exp(earg)
            t0   = dn0*(sba/tgz0)*exp1 
            strm = sqrt(2.0_sp*beta*a*C0318309886183790671537767526745)
            t1   = C1253314137315500251207882642406* &
                   (1.0_sp-strm*ctgz0)
            L2   = t0*t1 
       end function analytic_sol_n90_L2_f351_r4

       elemental function analytic_sol_n90_L2_f351_r8(dn0,beta,z0) result(L2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_n90_L2_f351_r8
            !dir$ attributes forceinline :: analytic_sol_n90_L2_f351_r8
#endif  
!$omp declare simd(analytic_sol_n90_L2_f351_r8)
            real(kind=dp), intent(in) :: dn0 
            real(kind=dp), intent(in) :: beta 
            real(kind=dp), intent(in) :: z0 
            real(kind=dp) :: L2 
            real(kind=dp), parameter :: a = 6378.0_dp
            real(kind=dp), parameter :: C1253314137315500251207882642406 = 1.253314137315500251207882642406_dp 
            real(kind=dp), parameter :: C0318309886183790671537767526745 = 0.318309886183790671537767526745_dp
            real(kind=dp), automatic :: sba, tgz0, stgz0, ctgz0 
            real(kind=dp), automatic :: earg, exp1, t0, t1, strm 
            sba  = sqrt(beta*a)
            tgz0 = tan(z0)
            ctgz0= 1.0_dp/tan(z0)
            earg = beta*a/(2.0_dp*tgz0*tgz0)
            exp1 = exp(earg)
            t0   = dn0*(sba/tgz0)*exp1 
            strm = sqrt(2.0_dp*beta*a*C0318309886183790671537767526745)
            t1   = C1253314137315500251207882642406* &
                   (1.0_dp-strm*ctgz0)
            L2   = t0*t1 
       end function analytic_sol_n90_L2_f351_r8

        ! z0 близко к 90°.
       ! The angle of arrival close to horizon.
       ! formula 3.51, page: 70
       ! analytic solution L3 for angle near 90 (deg)
       elemental function analytic_sol_n90_L3_f351_r4(dn0,beta,z0) result(L3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_n90_L3_f351_r4
            !dir$ attributes forceinline :: analytic_sol_n90_L3_f351_r4
#endif  
!$omp declare simd(analytic_sol_n90_L3_f351_r4)
            real(kind=sp), intent(in) :: dn0 
            real(kind=sp), intent(in) :: beta 
            real(kind=sp), intent(in) :: z0 
            real(kind=sp) :: L3 
            real(kind=sp), parameter :: a = 6378.0_sp
            real(kind=sp), parameter :: C1253314137315500251207882642406 = 1.253314137315500251207882642406_sp 
            real(kind=sp), parameter :: C0318309886183790671537767526745 = 0.318309886183790671537767526745_sp
            real(kind=sp), automatic :: sba, tgz0, stgz0, ctgz0 
            real(kind=sp), automatic :: earg, exp1, t0, t1, strm 
            sba  = sqrt(2.0_sp*beta*a)
            tgz0 = tan(z0)
            ctgz0= 1.0_sp/tan(z0)
            earg = beta*a/(tgz0*tgz0)
            exp1 = exp(earg)
            t0   = dn0*(sba/tgz0)*exp1 
            strm = sqrt(4.0_sp*beta*a*C0318309886183790671537767526745)
            t1   = C1253314137315500251207882642406* &
                   (1.0_sp-strm*ctgz0)
            L3   = t0*t1 
       end function analytic_sol_n90_L3_f351_r4

       elemental function analytic_sol_n90_L3_f351_r8(dn0,beta,z0) result(L3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_n90_L3_f351_r8
            !dir$ attributes forceinline :: analytic_sol_n90_L3_f351_r8
#endif  
!$omp declare simd(analytic_sol_n90_L3_f351_r8)
            real(kind=dp), intent(in) :: dn0 
            real(kind=dp), intent(in) :: beta 
            real(kind=dp), intent(in) :: z0 
            real(kind=dp) :: L3 
            real(kind=dp), parameter :: a = 6378.0_dp
            real(kind=dp), parameter :: C1253314137315500251207882642406 = 1.253314137315500251207882642406_dp 
            real(kind=dp), parameter :: C0318309886183790671537767526745 = 0.318309886183790671537767526745_dp
            real(kind=dp), automatic :: sba, tgz0, stgz0, ctgz0 
            real(kind=dp), automatic :: earg, exp1, t0, t1, strm 
            sba  = sqrt(2.0_dp*beta*a)
            tgz0 = tan(z0)
            ctgz0= 1.0_dp/tan(z0)
            earg = beta*a/(tgz0*tgz0)
            exp1 = exp(earg)
            t0   = dn0*(sba/tgz0)*exp1 
            strm = sqrt(4.0_dp*beta*a*C0318309886183790671537767526745)
            t1   = C1253314137315500251207882642406* &
                   (1.0_dp-strm*ctgz0)
            L3   = t0*t1 
       end function analytic_sol_n90_L3_f351_r8

         ! z0 близко к 90°.
       ! The angle of arrival close to horizon.
       ! formula 3.51, page: 70
       ! The whole solution for angle alpha near 90 (deg)
       elemental function refraction_angle_n90_f351_r4(dn0,beta,z0) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_n90_f351_r4
            !dir$ attributes forceinline :: refraction_angle_n90_f351_r4
#endif
!$omp declare simd(refraction_angle_n90_f351_r4)  
            real(kind=sp), intent(in) :: dn0 
            real(kind=sp), intent(in) :: beta 
            real(kind=sp), intent(in) :: z0 
            real(kind=sp) :: alpha  
            real(kind=sp), parameter :: a = 6378.0_sp
            real(kind=sp), automatic :: ctgz0, badn0, cosz0, scosz0
            real(kind=sp), automatic :: L2, L3, t0, t1, rat 
            
            cosz0 = cos(z0)
            badn0 = beta*dn0*a 
            ctgz0 = 1.0_sp/tan(z0)
            scosz0= cosz0*cosz0 
            L2    = analytic_sol_n90_L2_f351_r4(dn0,beta,z0)
            rat   = ctgz0/scosz0 
            t0    = -dn0*ctgz0+(1.0_sp-badn0) 
            L3    = analytic_sol_n90_L3_f351_r4(dn0,beta,z0)
            t1    = rat*L2+badn0*rat*L3 
            alpha = t0*t1 
       end function refraction_angle_n90_f351_r4

       elemental function refraction_angle_n90_f351_r8(dn0,beta,z0) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_n90_f351_r8
            !dir$ attributes forceinline :: refraction_angle_n90_f351_r8
#endif  
!$omp declare simd(refraction_angle_n90_f351_r8) 
            real(kind=dp), intent(in) :: dn0 
            real(kind=dp), intent(in) :: beta 
            real(kind=dp), intent(in) :: z0 
            real(kind=dp) :: alpha  
            real(kind=dp), parameter :: a = 6378.0_dp
            real(kind=dp), automatic :: ctgz0, badn0, cosz0, scosz0
            real(kind=dp), automatic :: L2, L3, t0, t1, rat 
            
            cosz0 = cos(z0)
            badn0 = beta*dn0*a 
            ctgz0 = 1.0_dp/tan(z0)
            scosz0= cosz0*cosz0 
            L2    = analytic_sol_n90_L2_f351_r8(dn0,beta,z0)
            rat   = ctgz0/scosz0 
            t0    = -dn0*ctgz0+(1.0_dp-badn0) 
            L3    = analytic_sol_n90_L3_f351_r8(dn0,beta,z0)
            t1    = rat*L2+badn0*rat*L3 
            alpha = t0*t1 
       end function refraction_angle_n90_f351_r8

       !z0 = 90° формула (3.51) упрощается.
       ! formula: 3.52, page: 71
       elemental function refraction_angle_at90_f352_r4(dn0,beta) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_at90_f352_r4
            !dir$ attributes forceinline :: refraction_angle_at90_f352_r4
#endif
!$omp declare simd(refraction_angle_at90_f352_r4)
            real(kind=sp), intent(in) :: dn0 
            real(kind=sp), intent(in) :: beta 
            real(kind=sp) :: alpha  
            real(kind=sp), parameter :: a = 6378.0_sp
            real(kind=sp), parameter :: C041421356237309504880168872421 = 0.41421356237309504880168872421_sp 
            real(kind=sp), parameter :: C314159265358979323846264338328 = 3.14159265358979323846264338328_sp
            real(kind=sp), automatic :: t0, t1, t2 
            t0 = dn0*sqrt((C314159265358979323846264338328*beta*a)*0.5_sp)
            t1 = 1.0_sp+C041421356237309504880168872421*beta*a*dn0 
            alpha = t0*t1 
       end function refraction_angle_at90_f352_r4

       elemental function refraction_angle_at90_f352_r8(dn0,beta) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_at90_f352_r8
            !dir$ attributes forceinline :: refraction_angle_at90_f352_r8
#endif
!$omp declare simd(refraction_angle_at90_f352_r8)
            real(kind=dp), intent(in) :: dn0 
            real(kind=dp), intent(in) :: beta 
            real(kind=dp) :: alpha  
            real(kind=dp), parameter :: a = 6378.0_dp
            real(kind=dp), parameter :: C041421356237309504880168872421 = 0.41421356237309504880168872421_dp 
            real(kind=dp), parameter :: C314159265358979323846264338328 = 3.14159265358979323846264338328_dp
            real(kind=dp), automatic :: t0, t1, t2 
            t0 = dn0*sqrt((C314159265358979323846264338328*beta*a)*0.5_dp)
            t1 = 1.0_dp+C041421356237309504880168872421*beta*a*dn0 
            alpha = t0*t1 
       end function refraction_angle_at90_f352_r8

       !угол радиорефракции I типа в 
       !земной атмосфере для длин волн, меньших 5 см
       ! formula: 4.2, page 73.
       elemental function analytic_sol_L1_gl5cm_f42_r4(dn0,beta,z0,H) result(L1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_gl5cm_f42_r4
            !dir$ attributes forceinline :: analytic_sol_L1_gl5cm_f42_r4
#endif  
!$omp declare simd(analytic_sol_L1_gl5cm_f42_r4)
            real(kind=sp), intent(in) :: dn0 
            real(kind=sp), intent(in) :: beta 
            real(kind=sp), intent(in) :: z0 
            real(kind=sp), intent(in) :: H 
            real(kind=sp) :: L1 
            real(kind=sp), parameter :: a = 6378.0_sp
            real(kind=sp), automatic :: ctgz0, secz0, tgz0, betaH 
            real(kind=sp), automatic :: t0, t1, earg, exp1, exp2 
            real(kind=sp), automatic :: sdn0ba, trm1, trm2, trm3 
            betaH  = beta*H 
            ctgz0  = 1.0_sp/tan(z0)
            sdn0ba = -dn0*dn0*beta*a 
            t0     = tan(z0) 
            tgz0   = t0*t0 
            t1     = 1.0_sp/cos(z0) 
            secz0  = t1*t1 
            exp1   = exp(-betaH)
            ctgz0  = 1.0_sp/t0 
            exp2   = exp(-2.0_sp*betaH)
            trm1   = sdn0ba*ctgz0*secz0 
            trm2   = exp1-exp2 
            trm3   = sqrt(1.0_sp+2.0_sp*tgz0*(H/a))
            L1     = trm1*trm2*trm3 
       end function analytic_sol_L1_gl5cm_f42_r4

       elemental function analytic_sol_L1_gl5cm_f42_r8(dn0,beta,z0,H) result(L1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_gl5cm_f42_r8
            !dir$ attributes forceinline :: analytic_sol_L1_gl5cm_f42_r8
#endif  
!$omp declare simd(analytic_sol_L1_gl5cm_f42_r8)
            real(kind=dp), intent(in) :: dn0 
            real(kind=dp), intent(in) :: beta 
            real(kind=dp), intent(in) :: z0 
            real(kind=dp), intent(in) :: H 
            real(kind=dp) :: L1 
            real(kind=dp), parameter :: a = 6378.0_dp
            real(kind=dp), automatic :: ctgz0, secz0, tgz0, betaH 
            real(kind=dp), automatic :: t0, t1, earg, exp1, exp2 
            real(kind=dp), automatic :: sdn0ba, trm1, trm2, trm3 
            betaH  = beta*H 
            ctgz0  = 1.0_dp/tan(z0)
            sdn0ba = -dn0*dn0*beta*a 
            t0     = tan(z0) 
            tgz0   = t0*t0 
            t1     = 1.0_dp/cos(z0) 
            secz0  = t1*t1 
            exp1   = exp(-betaH)
            ctgz0  = 1.0_dp/t0 
            exp2   = exp(-2.0_dp*betaH)
            trm1   = sdn0ba*ctgz0*secz0 
            trm2   = exp1-exp2 
            trm3   = sqrt(1.0_dp+2.0_dp*tgz0*(H/a))
            L1     = trm1*trm2*trm3 
       end function analytic_sol_L1_gl5cm_f42_r8

       elemental function analytic_sol_L2_gl5cm_f43_r4(dn0,beta,z0,H) result(L2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L2_gl5cm_f43_r4
            !dir$ attributes forceinline :: analytic_sol_L2_gl5cm_f43_r4
#endif  
!$omp declare simd(analytic_sol_L2_gl5cm_f43_r4)
            real(kind=sp), intent(in) :: dn0 
            real(kind=sp), intent(in) :: beta 
            real(kind=sp), intent(in) :: z0 
            real(kind=sp), intent(in) :: H 
            real(kind=sp) :: L2 
            real(kind=sp), parameter :: a = 6378.0_sp
            real(kind=sp), parameter :: C314159265358979323846264338328 = 3.14159265358979323846264338328_sp
            real(kind=sp), automatic :: piba2, ctgz0, bactgz0, exp1 
            real(kind=sp), automatic :: t0, t1, trm1, trm2, trm3 
            piba2  = sqrt((C314159265358979323846264338328*beta*a)/2)
            ctgz0  = 1.0_sp/tan(z0)
            bactgz0= beta*a*ctgz0*ctgz0 
            exp1   = exp(bactgz0*0.5_sp)
            trm1   = dn0*sqrt(piba2)*ctgz0 
            t0     = prob_integral_r4(sqrt(bactgz0+2.0_sp*beta*H))
            t1     = prob_integral_r4(sqrt(bactgz0))
            trm3   = t0-t1 
            trm2   = trm1*exp1 
            L2     = trm2*trm3 
       end function analytic_sol_L2_gl5cm_f43_r4

       elemental function analytic_sol_L2_gl5cm_f43_r8(dn0,beta,z0,H) result(L2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L2_gl5cm_f43_r8
            !dir$ attributes forceinline :: analytic_sol_L2_gl5cm_f43_r8
#endif  
!$omp declare simd(analytic_sol_L2_gl5cm_f43_r8)
            real(kind=dp), intent(in) :: dn0 
            real(kind=dp), intent(in) :: beta 
            real(kind=dp), intent(in) :: z0 
            real(kind=dp), intent(in) :: H 
            real(kind=dp) :: L2 
            real(kind=dp), parameter :: a = 6378.0_dp
            real(kind=dp), parameter :: C314159265358979323846264338328 = 3.14159265358979323846264338328_dp
            real(kind=dp), automatic :: piba2, ctgz0, bactgz0, exp1 
            real(kind=dp), automatic :: t0, t1, trm1, trm2, trm3 
            piba2  = sqrt((C314159265358979323846264338328*beta*a)/2)
            ctgz0  = 1.0_dp/tan(z0)
            bactgz0= beta*a*ctgz0*ctgz0 
            exp1   = exp(bactgz0*0.5_dp)
            trm1   = dn0*sqrt(piba2)*ctgz0 
            t0     = prob_integral_r8(sqrt(bactgz0+2.0_dp*beta*H))
            t1     = prob_integral_r8(sqrt(bactgz0))
            trm3   = t0-t1 
            trm2   = trm1*exp1 
            L2     = trm2*trm3 
       end function analytic_sol_L2_gl5cm_f43_r8

       elemental function analytic_sol_L3_gl5cm_f43_r4(dn0,beta,z0,H) result(L3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L3_gl5cm_f43_r4
            !dir$ attributes forceinline :: analytic_sol_L3_gl5cm_f43_r4
#endif  
!$omp declare simd(analytic_sol_L3_gl5cm_f43_r4)
            real(kind=sp), intent(in) :: dn0 
            real(kind=sp), intent(in) :: beta 
            real(kind=sp), intent(in) :: z0 
            real(kind=sp), intent(in) :: H 
            real(kind=sp) :: L2 
            real(kind=sp), parameter :: a = 6378.0_sp
            real(kind=sp), parameter :: C314159265358979323846264338328 = 3.14159265358979323846264338328_sp
            real(kind=sp), automatic :: piba, ctgz0, bactgz0, exp1 
            real(kind=sp), automatic :: t0, t1, trm1, trm2, trm3 
            piba  = sqrt(C314159265358979323846264338328*beta*a)
            ctgz0  = 1.0_sp/tan(z0)
            bactgz0= beta*a*ctgz0*ctgz0 
            exp1   = exp(bactgz0)
            trm1   = dn0*sqrt(piba)*ctgz0 
            t0     = prob_integral_r4(sqrt(2.0_sp*bactgz0+4.0_sp*beta*H))
            t1     = prob_integral_r4(sqrt(2.0_sp*bactgz0))
            trm3   = t0-t1 
            trm2   = trm1*exp1 
            L2     = trm2*trm3 
       end function analytic_sol_L3_gl5cm_f43_r4

         elemental function analytic_sol_L3_gl5cm_f43_r8(dn0,beta,z0,H) result(L3)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L3_gl5cm_f43_r8
            !dir$ attributes forceinline :: analytic_sol_L3_gl5cm_f43_r8
#endif  
!$omp declare simd(analytic_sol_L3_gl5cm_f43_r8)
            real(kind=dp), intent(in) :: dn0 
            real(kind=dp), intent(in) :: beta 
            real(kind=dp), intent(in) :: z0 
            real(kind=dp), intent(in) :: H 
            real(kind=dp) :: L2 
            real(kind=dp), parameter :: a = 6378.0_dp
            real(kind=dp), parameter :: C314159265358979323846264338328 = 3.14159265358979323846264338328_dp
            real(kind=dp), automatic :: piba, ctgz0, bactgz0, exp1 
            real(kind=dp), automatic :: t0, t1, trm1, trm2, trm3 
            piba  = sqrt(C314159265358979323846264338328*beta*a)
            ctgz0  = 1.0_dp/tan(z0)
            bactgz0= beta*a*ctgz0*ctgz0 
            exp1   = exp(bactgz0)
            trm1   = dn0*sqrt(piba)*ctgz0 
            t0     = prob_integral_r8(sqrt(2.0_dp*bactgz0+4.0_dp*beta*H))
            t1     = prob_integral_r8(sqrt(2.0_dp*bactgz0))
            trm3   = t0-t1 
            trm2   = trm1*exp1 
            L2     = trm2*trm3 
       end function analytic_sol_L3_gl5cm_f43_r8

       elemental function refraction_angle_for_gl5cm_f41_r4(n0,nh,z0,beta,dn0,H) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_for_gl5cm_f41_r4
            !dir$ attributes forceinline :: refraction_angle_for_gl5cm_f41_r4
#endif  
!$omp declare simd(refraction_angle_for_gl5cm_f41_r4)
             real(kind=sp),  intent(in) :: n0 
             real(kind=sp),  intent(in) :: nh 
             real(kind=sp),  intent(in) :: z0 
             real(kind=sp),  intent(in) :: beta 
             real(kind=sp),  intent(in) :: dn0 
             real(kind=sp),  intent(in) :: H 
             real(kind=sp) :: alpha 
             real(kind=sp), parameter :: a = 6378.0_sp
             real(kind=sp), automatic :: L1, L2, L3 
             real(kind=sp), automatic :: ctgz0, lnn0nh, ssecz, badn0 
             real(kind=sp), automatic :: t0, t1, trm1, trm2, trm3 
             badn0  = beta*a*dn0 
             ctgz0  = 1.0_sp/tan(z0)
             lnn0nh = log(n0/nh)
             L1     = analytic_sol_L1_gl5cm_f42_r4(dn0,beta,z0,H)
             t0     = 1.0_sp/cos(z0)
             ssecz  = t0*t0 
             t1     = ctgz0*ssecz 
             L2     = analytic_sol_L2_gl5cm_f43_r4(dn0,beta,z0,H)
             trm1   = -ctgz0*lnn0nh+L1 
             L3     = analytic_sol_L3_gl5cm_f43_r4(dn0,beta,z0,H)
             trm2   = t1*L2 
             trm3   = badn0*t1*(L3-L2)
             alpha  = trm1+trm2+trm3 
       end function refraction_angle_for_gl5cm_f41_r4

       elemental function refraction_angle_for_gl5cm_f41_r8(n0,nh,z0,beta,dn0,H) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_for_gl5cm_f41_r8
            !dir$ attributes forceinline :: refraction_angle_for_gl5cm_f41_r8
#endif  
!$omp declare simd(refraction_angle_for_gl5cm_f41_r8)
             real(kind=dp),  intent(in) :: n0 
             real(kind=dp),  intent(in) :: nh 
             real(kind=dp),  intent(in) :: z0 
             real(kind=dp),  intent(in) :: beta 
             real(kind=dp),  intent(in) :: dn0 
             real(kind=dp),  intent(in) :: H 
             real(kind=dp) :: alpha 
             real(kind=dp), parameter :: a = 6378.0_dp
             real(kind=dp), automatic :: L1, L2, L3 
             real(kind=dp), automatic :: ctgz0, lnn0nh, ssecz, badn0 
             real(kind=dp), automatic :: t0, t1, trm1, trm2, trm3 
             badn0  = beta*a*dn0 
             ctgz0  = 1.0_dp/tan(z0)
             lnn0nh = log(n0/nh)
             L1     = analytic_sol_L1_gl5cm_f42_r8(dn0,beta,z0,H)
             t0     = 1.0_dp/cos(z0)
             ssecz  = t0*t0 
             t1     = ctgz0*ssecz 
             L2     = analytic_sol_L2_gl5cm_f43_r8(dn0,beta,z0,H)
             trm1   = -ctgz0*lnn0nh+L1 
             L3     = analytic_sol_L3_gl5cm_f43_r8(dn0,beta,z0,H)
             trm2   = t1*L2 
             trm3   = badn0*t1*(L3-L2)
             alpha  = trm1+trm2+trm3 
       end function refraction_angle_for_gl5cm_f41_r8

       !показатель преломления ионосферы в среднем
       elemental function refractive_idx_lo_ionosphere_f412_r4(h,d,f,Nmf) result(n)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractive_idx_lo_ionosphere_f412_r4
            !dir$ attributes forceinline :: refractive_idx_lo_ionosphere_f412_r4
#endif 
!$omp declare simd(refractive_idx_lo_ionosphere_f412_r4) 
            real(kind=sp), intent(in) :: h     ! height 
            real(kind=sp), intent(in) :: d     ! height a maximum of layer F2
            real(kind=sp), intent(in) :: f     ! center signal frequency
            real(kind=sp), intent(in) :: Nmf   ! electron density in layer F2
            real(kind=sp) :: n 
            real(kind=sp), automatic :: dnm, hd, hhdd, fcr 
            fcr = sqrt(80.8_sp*Nmf)
            hd  = h/d 
            dnm = fcr*fcr/(2.0_sp*f*f)
            hhdd= hd*hd 
            n   = 1.0_sp-dnm*(2.0_sp*hd-hhdd)
       end function refractive_idx_lo_ionosphere_f412_r4

        elemental function refractive_idx_lo_ionosphere_f412_r8(h,d,f,Nmf) result(n)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractive_idx_lo_ionosphere_f412_r8
            !dir$ attributes forceinline :: refractive_idx_lo_ionosphere_f412_r8
#endif  
!$omp declare simd(refractive_idx_lo_ionosphere_f412_r8) 
            real(kind=dp), intent(in) :: h     ! height 
            real(kind=dp), intent(in) :: d     ! height a maximum of layer F2
            real(kind=dp), intent(in) :: f     ! center signal frequency
            real(kind=dp), intent(in) :: Nmf   ! electron density in layer F2
            real(kind=dp) :: n 
            real(kind=dp), automatic :: dnm, hd, hhdd, fcr 
            fcr = sqrt(80.8_dp*Nmf)
            hd  = h/d 
            dnm = fcr*fcr/(2.0_dp*f*f)
            hhdd= hd*hd 
            n   = 1.0_dp-dnm*(2.0_dp*hd-hhdd)
       end function refractive_idx_lo_ionosphere_f412_r8

       elemental function refractive_idx_hi_ionosphere_f413_r4(h,d,f,Nmf,beta) result(n)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractive_idx_hi_ionosphere_f413_r4
            !dir$ attributes forceinline :: refractive_idx_hi_ionosphere_f413_r4
#endif 
!$omp declare simd(refractive_idx_hi_ionosphere_f413_r4) 
            real(kind=sp), intent(in) :: h     ! height 
            real(kind=sp), intent(in) :: d     ! height a maximum of layer F2
            real(kind=sp), intent(in) :: f     ! center signal frequency
            real(kind=sp), intent(in) :: Nmf   ! electron density in layer F2
            real(kind=sp), intent(in) :: beta  ! diminishing speed of electron concentration in layer F2
            real(kind=sp) :: n 
            real(kind=sp), automatic :: dnm, fcr, earg, exp1 
            fcr = sqrt(80.8_sp*Nmf)
            dnm = fcr*fcr/(2.0_sp*f*f)
            earg= -beta*(h-d)
            exp1= exp(earg)
            n   = 1.0_sp-dnm*exp1 
       end function refractive_idx_hi_ionosphere_f413_r4

       elemental function refractive_idx_hi_ionosphere_f413_r8(h,d,f,Nmf,beta) result(n)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractive_idx_hi_ionosphere_f413_r8
            !dir$ attributes forceinline :: refractive_idx_hi_ionosphere_f413_r8
#endif 
!$omp declare simd(refractive_idx_hi_ionosphere_f413_r8)  
            real(kind=dp), intent(in) :: h     ! height 
            real(kind=dp), intent(in) :: d     ! height a maximum of layer F2
            real(kind=dp), intent(in) :: f     ! center signal frequency
            real(kind=dp), intent(in) :: Nmf   ! electron density in layer F2
            real(kind=dp), intent(in) :: beta  ! diminishing speed of electron concentration in layer F2
            real(kind=dp) :: n 
            real(kind=dp), automatic :: dnm, fcr, earg, exp1 
            fcr = sqrt(80.8_dp*Nmf)
            dnm = fcr*fcr/(2.0_dp*f*f)
            earg= -beta*(h-d)
            exp1= exp(earg)
            n   = 1.0_dp-dnm*exp1 
       end function refractive_idx_hi_ionosphere_f413_r8

       ! Compute `delta-nM` value, formula 4.14, page: 77
       elemental function compute_delnM_f414_r4(fc,Nmf) result(dnM)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_delnM_f414_r4
            !dir$ attributes forceinline :: compute_delnM_f414_r4
#endif 
!$omp declare simd(compute_delnM_f414_r4)
            real(kind=sp), intent(in) :: fc 
            real(kind=sp), intent(in) :: Nmf 
            real(kind=sp) :: dnM 
            real(kind=sp), automatic :: fcr, sfc 
            sfc = 2.0_sp*fc*fc 
            fcr = sqrt(80.8_sp*Nmf)
            dnM = fcr*fcr/sfc  
       end function compute_delnM_f414_r4

       elemental function compute_delnM_f414_r8(fc,Nmf) result(dnM)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_delnM_f414_r8
            !dir$ attributes forceinline :: compute_delnM_f414_r8
#endif 
!$omp declare simd(compute_delnM_f414_r8)
            real(kind=dp), intent(in) :: fc 
            real(kind=dp), intent(in) :: Nmf 
            real(kind=dp) :: dnM 
            real(kind=dp), automatic :: fcr, sfc 
            sfc = 2.0_dp*fc*fc 
            fcr = sqrt(80.8_dp*Nmf)
            dnM = fcr*fcr/sfc  
       end function compute_delnM_f414_r8

       elemental function compute_delnEps_f421_r4(fc,Nmf,beta,d) result(dnE)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_delnEps_f421_r4
            !dir$ attributes forceinline :: compute_delnEps_f421_r4
#endif 
!$omp declare simd(compute_delnEps_f421_r4)
            real(kind=sp), intent(in) :: fc 
            real(kind=sp), intent(in) :: Nmf 
            real(kind=sp), intent(in) :: beta 
            real(kind=sp), intent(in) :: d 
            real(kind=sp) :: dnE 
            real(kind=sp), automatic :: dnM, earg, exp1 
            earg = beta*d 
            dnM  = compute_delnM_f414_r4(fc,Nmf)
            exp1 = exp(earg)
            dnE  = dnM*exp1 
       end function compute_delnEps_f421_r4

       elemental function compute_delnEps_f421_r8(fc,Nmf,beta,d) result(dnE)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_delnEps_f421_r8
            !dir$ attributes forceinline :: compute_delnEps_f421_r8
#endif 
!$omp declare simd(compute_delnEps_f421_r8)
            real(kind=dp), intent(in) :: fc 
            real(kind=dp), intent(in) :: Nmf 
            real(kind=dp), intent(in) :: beta 
            real(kind=dp), intent(in) :: d 
            real(kind=dp) :: dnE 
            real(kind=dp), automatic :: dnM, earg, exp1 
            earg = beta*d 
            dnM  = compute_delnM_f414_r8(fc,Nmf)
            exp1 = exp(earg)
            dnE  = dnM*exp1 
       end function compute_delnEps_f421_r8

      ! An analytic solution of `L1` component integral 
      elemental function analytic_sol_L1_lo_ionosphere_f418_r4(fc,Nmf,z0,d,R0) result(L1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_lo_ionosphere_f418_r4
            !dir$ attributes forceinline :: analytic_sol_L1_lo_ionosphere_f418_r4
#endif 
!$omp declare simd(analytic_sol_L1_lo_ionosphere_f418_r4)
            real(kind=sp),  intent(in) :: fc 
            real(kind=sp),  intent(in) :: Nmf 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: d 
            real(kind=sp),  intent(in) :: R0 
            real(kind=sp) :: L1 
            real(kind=sp),   parameter :: C10 = 10.0_sp 
            real(kind=sp),   parameter :: C19 = 19.0_sp
            real(kind=sp),   parameter :: C1  = 1.0_sp
            real(kind=sp), automatic :: delnM, m, c2mm, c12m 
            real(kind=sp), automatic :: ctgz0, cos2z0, c3m, tgz0 
            real(kind=sp), automatic :: c5mm, sqr, t0, t1, t2, t3 
            real(kind=sp), automatic :: trm1, trm2, trm3, trm4 
            delnM = compute_delnM_f414_r4(fc,Nmf)
            tgz0  = tan(z0)
            ctgz0 = C1/tgz0 
            m     = (tgz0*tgz0*d)/R0
            sqr   = sqrt(C1+2.0_sp*m)
            t0    = cos(z0)
            cos2z0= t0*t0 
            t1    = ctgz0/cos2z0
            t2    = (-2.0_sp*delNm)/m
            c5mm  = 5.0_dp*m*m 
            trm1  = t2*t1 
            c3m   = 3.0_sp*m
            trm2  = (C1-sqr)+(C1/c3m)*m-C1*sqr+C1
            t2    = (2.0_sp*delnM*delnM)/c5mm
            t3    = tgz0/cos2z0 
            trm3  = t2*t3 
            c2mm  = 2.0_sp/(m*m)
            c12m  = 12.0_sp/m 
            t1    = c2mm+c12m+C19+6.0_sp*m 
            t2    = c2mm+(C10/m)+C10 
            trm4  = (C1/sqr)*t1-t2
            L1    = trm1*trm2+trm3*trm4 
      end function analytic_sol_L1_lo_ionosphere_f418_r4

       elemental function analytic_sol_L1_lo_ionosphere_f418_r8(fc,Nmf,z0,d,R0) result(L1)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_lo_ionosphere_f418_r8
            !dir$ attributes forceinline :: analytic_sol_L1_lo_ionosphere_f418_r8
#endif 
!$omp declare simd(analytic_sol_L1_lo_ionosphere_f418_r8)
            real(kind=dp),  intent(in) :: fc 
            real(kind=dp),  intent(in) :: Nmf 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: d 
            real(kind=dp),  intent(in) :: R0 
            real(kind=dp) :: L1 
            real(kind=dp),   parameter :: C10 = 10.0_dp 
            real(kind=dp),   parameter :: C19 = 19.0_dp
            real(kind=dp),   parameter :: C1  = 1.0_dp
            real(kind=dp), automatic :: delnM, m, c2mm, c12m 
            real(kind=dp), automatic :: ctgz0, cos2z0, c3m, tgz0 
            real(kind=dp), automatic :: c5mm, sqr, t0, t1, t2, t3 
            real(kind=dp), automatic :: trm1, trm2, trm3, trm4 
            delnM = compute_delnM_f414_r8(fc,Nmf)
            tgz0  = tan(z0)
            ctgz0 = C1/tgz0 
            m     = (tgz0*tgz0*d)/R0
            sqr   = sqrt(C1+2.0_dp*m)
            t0    = cos(z0)
            cos2z0= t0*t0 
            t1    = ctgz0/cos2z0
            t2    = (-2.0_dp*delNm)/m
            c5mm  = 5.0_dp*m*m 
            trm1  = t2*t1 
            c3m   = 3.0_dp*m
            trm2  = (C1-sqr)+(C1/c3m)*m-C1*sqr+C1
            t2    = (2.0_dp*delnM*delnM)/c5mm
            t3    = tgz0/cos2z0 
            trm3  = t2*t3 
            c2mm  = 2.0_dp/(m*m)
            c12m  = 12.0_dp/m 
            t1    = c2mm+c12m+C19+6.0_dp*m 
            t2    = c2mm+(C10/m)+C10 
            trm4  = (C1/sqr)*t1-t2
            L1    = trm1*trm2+trm3*trm4 
      end function analytic_sol_L1_lo_ionosphere_f418_r8

      ! formula: 4.22, page: 78

      elemental function analytic_sol_L01_hi_ionosphere_f422_r4(fc,Nmf,beta,d,R0,z0,D1) result(L01)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L01_hi_ionosphere_f422_r4
            !dir$ attributes forceinline :: analytic_sol_L01_hi_ionosphere_f422_r4
#endif 
!$omp declare simd(analytic_sol_L01_hi_ionosphere_f422_r4)
            real(kind=sp),  intent(in) :: fc 
            real(kind=sp),  intent(in) :: Nmf 
            real(kind=sp),  intent(in) :: beta 
            real(kind=sp),  intent(in) :: d 
            real(kind=sp),  intent(in) :: R0 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: D1 
            real(kind=sp)  :: L01 
            real(kind=sp), automatic :: sdnE, ctgz0, sec2z0, bd
            real(kind=sp), automatic :: bD1,  strm,  exp1,  exp2 
            real(kind=sp), automatic :: exp3, exp4, trm1, trm2 
            real(kind=sp), automatic :: t0, t1, tg2z0, trm3  
            t0    = compute_delnEps_f421_r4(fc,Nmf,beta,d)
            bd    = beta*d 
            bD1   = beta*D1 
            ctgz0 = 1.0_sp/tan(z0)
            sdnE  = t0*t0 
            t1    = 1.0_sp/cos(z0)
            sec2z0= t1*t1 
            t0    = tan(z0)
            tg2z0 = t0*t0 
            strm  = sqrt((1.0_sp+2.0_sp*tg2z0*d)/R0)
            trm1  = sdnE*beta*R0*ctgz0*sec2z0
            exp1  = exp(-bd)
            exp2  = exp(-2.0_sp*bd)
            exp3  = exp(-bD1)
            exp4  = exp(-2.0_sp*bD1)
            trm2  = (exp1-exp2)*strm
            trm3  = (exp3-exp4)*strm
            L01   = trm1*(trm2-trm3)
      end function analytic_sol_L01_hi_ionosphere_f422_r4

      elemental function analytic_sol_L01_hi_ionosphere_f422_r8(fc,Nmf,beta,d,R0,z0,D1) result(L01)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L01_hi_ionosphere_f422_r8
            !dir$ attributes forceinline :: analytic_sol_L01_hi_ionosphere_f422_r8
#endif 
!$omp declare simd(analytic_sol_L01_hi_ionosphere_f422_r8)
            real(kind=dp),  intent(in) :: fc 
            real(kind=dp),  intent(in) :: Nmf 
            real(kind=dp),  intent(in) :: beta 
            real(kind=dp),  intent(in) :: d 
            real(kind=dp),  intent(in) :: R0 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: D1 
            real(kind=dp)  :: L01 
            real(kind=dp), automatic :: sdnE, ctgz0, sec2z0, bd
            real(kind=dp), automatic :: bD1,  strm,  exp1,  exp2 
            real(kind=dp), automatic :: exp3, exp4, trm1, trm2 
            real(kind=dp), automatic :: t0, t1, tg2z0, trm3  
            t0    = compute_delnEps_f421_r8(fc,Nmf,beta,d)
            bd    = beta*d 
            bD1   = beta*D1 
            ctgz0 = 1.0_dp/tan(z0)
            sdnE  = t0*t0 
            t1    = 1.0_dp/cos(z0)
            sec2z0= t1*t1 
            t0    = tan(z0)
            tg2z0 = t0*t0 
            strm  = sqrt((1.0_dp+2.0_dp*tg2z0*d)/R0)
            trm1  = sdnE*beta*R0*ctgz0*sec2z0
            exp1  = exp(-bd)
            exp2  = exp(-2.0_dp*bd)
            exp3  = exp(-bD1)
            exp4  = exp(-2.0_dp*bD1)
            trm2  = (exp1-exp2)*strm
            trm3  = (exp3-exp4)*strm
            L01   = trm1*(trm2-trm3)
      end function analytic_sol_L01_hi_ionosphere_f422_r8

      ! formula 4.23, page: 78
      elemental function analytic_sol_L02_hi_ionosphere_f423_r4(fc,Nmf,beta,d,R0,z0,D1) result(L02)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L02_hi_ionosphere_f423_r4
            !dir$ attributes forceinline :: analytic_sol_L02_hi_ionosphere_f423_r4
#endif 
!$omp declare simd(analytic_sol_L02_hi_ionosphere_f423_r4)
            real(kind=sp),  intent(in) :: fc 
            real(kind=sp),  intent(in) :: Nmf 
            real(kind=sp),  intent(in) :: beta 
            real(kind=sp),  intent(in) :: d 
            real(kind=sp),  intent(in) :: R0 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: D1 
            real(kind=sp) :: L02
            real(kind=sp), parameter :: C314159265358979323846264338328 = 3.14159265358979323846264338328_sp
            real(kind=sp), automatic :: dnEps, ctgz0, sctgz0, sqr
            real(kind=sp), automatic :: bRctgz0, sqr1, sqr2, exp1 
            real(kind=sp), automatic :: prob1, prob2, trm1, trm2 
            sqr     = sqrt(C314159265358979323846264338328*beta*R0*0.5_sp)
            ctgz0   = 1.0_sp/tan(z0)
            dnEps   = compute_delnEps_f421_r4(fc,Nmf,beta,d)
            sctgz0  = ctgz0*ctgz0 
            bRctgz0 = beta*R0*sctgz0
            exp1    = exp(bRctgz0*0.5_sp) 
            sqr1    = sqrt(bRctgz0+2.0_sp*beta*D1)
            sqr2    = sqrt(bRctgz0+2.0_sp*beta*d)
            trm1    = dnEps*sqr*ctgz0*exp1 
            prob1   = prob_integral_r4(sqr1)
            prob2   = prob_integral_r4(sqr2)
            trm2    = prob1-prob2 
            L02     = trm1*trm2 
      end function analytic_sol_L02_hi_ionosphere_f423_r4

      elemental function analytic_sol_L02_hi_ionosphere_f423_r8(fc,Nmf,beta,d,R0,z0,D1) result(L02)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L02_hi_ionosphere_f423_r8
            !dir$ attributes forceinline :: analytic_sol_L02_hi_ionosphere_f423_r8
#endif 
!$omp declare simd(analytic_sol_L02_hi_ionosphere_f423_r8)
            real(kind=dp),  intent(in) :: fc 
            real(kind=dp),  intent(in) :: Nmf 
            real(kind=dp),  intent(in) :: beta 
            real(kind=dp),  intent(in) :: d 
            real(kind=dp),  intent(in) :: R0 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: D1 
            real(kind=dp) :: L02
            real(kind=dp), parameter :: C314159265358979323846264338328 = 3.14159265358979323846264338328_dp
            real(kind=dp), automatic :: dnEps, ctgz0, sctgz0, sqr
            real(kind=dp), automatic :: bRctgz0, sqr1, sqr2, exp1 
            real(kind=dp), automatic :: prob1, prob2, trm1, trm2 
            sqr     = sqrt(C314159265358979323846264338328*beta*R0*0.5_dp)
            ctgz0   = 1.0_dp/tan(z0)
            dnEps   = compute_delnEps_f421_r8(fc,Nmf,beta,d)
            sctgz0  = ctgz0*ctgz0 
            bRctgz0 = beta*R0*sctgz0
            exp1    = exp(bRctgz0*0.5_dp) 
            sqr1    = sqrt(bRctgz0+2.0_dp*beta*D1)
            sqr2    = sqrt(bRctgz0+2.0_dp*beta*d)
            trm1    = dnEps*sqr*ctgz0*exp1 
            prob1   = prob_integral_r8(sqr1)
            prob2   = prob_integral_r8(sqr2)
            trm2    = prob1-prob2 
            L02     = trm1*trm2 
      end function analytic_sol_L02_hi_ionosphere_f423_r8

      ! formula 4.24, page: 78
      elemental function analytic_sol_L03_hi_ionosphere_f424_r4(fc,Nmf,beta,d,R0,z0,D1) result(L03)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L03_hi_ionosphere_f424_r4
            !dir$ attributes forceinline :: analytic_sol_L03_hi_ionosphere_f424_r4
#endif 
!$omp declare simd(analytic_sol_L03_hi_ionosphere_f424_r4)
            real(kind=sp),  intent(in) :: fc 
            real(kind=sp),  intent(in) :: Nmf 
            real(kind=sp),  intent(in) :: beta 
            real(kind=sp),  intent(in) :: d 
            real(kind=sp),  intent(in) :: R0 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: D1 
            real(kind=sp) :: L02
            real(kind=sp), parameter :: C314159265358979323846264338328 = 3.14159265358979323846264338328_sp
            real(kind=sp), automatic :: dnEps, ctgz0, sctgz0, sqr
            real(kind=sp), automatic :: bRctgz0, sqr1, sqr2, exp1 
            real(kind=sp), automatic :: prob1, prob2, trm1, trm2 
            sqr     = sqrt(C314159265358979323846264338328*beta*R0)
            ctgz0   = 1.0_sp/tan(z0)
            dnEps   = compute_delnEps_f421_r4(fc,Nmf,beta,d)
            sctgz0  = ctgz0*ctgz0 
            bRctgz0 = beta*R0*sctgz0
            exp1    = exp(bRctgz0) 
            sqr1    = sqrt(2.0_sp*bRctgz0+4.0_sp*beta*D1)
            sqr2    = sqrt(2.0_sp*bRctgz0+4.0_sp*beta*d)
            trm1    = dnEps*sqr*ctgz0*exp1 
            prob1   = prob_integral_r4(sqr1)
            prob2   = prob_integral_r4(sqr2)
            trm2    = prob1-prob2 
            L03     = trm1*trm2 
      end function analytic_sol_L03_hi_ionosphere_f424_r4

       elemental function analytic_sol_L03_hi_ionosphere_f424_r8(fc,Nmf,beta,d,R0,z0,D1) result(L03)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L03_hi_ionosphere_f424_r8
            !dir$ attributes forceinline :: analytic_sol_L03_hi_ionosphere_f424_r8
#endif 
!$omp declare simd(analytic_sol_L03_hi_ionosphere_f424_r8)
            real(kind=dp),  intent(in) :: fc 
            real(kind=dp),  intent(in) :: Nmf 
            real(kind=dp),  intent(in) :: beta 
            real(kind=dp),  intent(in) :: d 
            real(kind=dp),  intent(in) :: R0 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: D1 
            real(kind=dp) :: L02
            real(kind=dp), parameter :: C314159265358979323846264338328 = 3.14159265358979323846264338328_dp
            real(kind=dp), automatic :: dnEps, ctgz0, sctgz0, sqr
            real(kind=dp), automatic :: bRctgz0, sqr1, sqr2, exp1 
            real(kind=dp), automatic :: prob1, prob2, trm1, trm2 
            sqr     = sqrt(C314159265358979323846264338328*beta*R0)
            ctgz0   = 1.0_dp/tan(z0)
            dnEps   = compute_delnEps_f421_r8(fc,Nmf,beta,d)
            sctgz0  = ctgz0*ctgz0 
            bRctgz0 = beta*R0*sctgz0
            exp1    = exp(bRctgz0) 
            sqr1    = sqrt(2.0_dp*bRctgz0+4.0_dp*beta*D1)
            sqr2    = sqrt(2.0_dp*bRctgz0+4.0_dp*beta*d)
            trm1    = dnEps*sqr*ctgz0*exp1 
            prob1   = prob_integral_r8(sqr1)
            prob2   = prob_integral_r8(sqr2)
            trm2    = prob1-prob2 
            L03     = trm1*trm2 
      end function analytic_sol_L03_hi_ionosphere_f424_r8

      ! formula: 4.20, page: 78
      elemental function analytic_sol_L2_hi_ionosphere_f420_r4(fc,Nmf,beta,d,R0,z0,D1) result(L2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L2_hi_ionosphere_f420_r4
            !dir$ attributes forceinline :: analytic_sol_L2_hi_ionosphere_f420_r4
#endif 
!$omp declare simd(analytic_sol_L2_hi_ionosphere_f420_r4)
            real(kind=sp),  intent(in) :: fc 
            real(kind=sp),  intent(in) :: Nmf 
            real(kind=sp),  intent(in) :: beta 
            real(kind=sp),  intent(in) :: d 
            real(kind=sp),  intent(in) :: R0 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: D1 
            real(kind=sp) :: L2 
            real(kind=sp), automatic :: L01, L02, L03 
            real(kind=sp), automatic :: dnEps, ctgz0, i2cosz0, ssecz0 
            real(kind=sp), automatic :: trm1, trm2, trm3, t0, t1  
            ctgz0  = 1.0_sp/tan(z0) 
            dnEps  = compute_delnEps_f421_r4(fc,Nmf,beta,d)
            t0     = cos(z0)
            t1     = 1.0_sp/t0 
            ssecz0 = t1*t1 
            i2cosz0= 1.0_sp/(t0*t0) 
            L01    = analytic_sol_L01_hi_ionosphere_f422_r4(fc,Nmf,beta,d,R0,z0,D1)
            trm1   = L01+(1.0_sp-beta*R0*dnEps)
            L02    = analytic_sol_L02_hi_ionosphere_f423_r4(fc,Nmf,beta,d,R0,z0,D1)
            trm2   = ctgz0*ssecz0*L02 
            L03    = analytic_sol_L03_hi_ionosphere_f424_r4(fc,Nmf,beta,d,R0,z0,D1)
            trm3   = dnEps*beta*R0*ctgz0*i2cosz0*L03 
            L2     = trm1+trm2+trm3 
      end function analytic_sol_L2_hi_ionosphere_f420_r4

      elemental function analytic_sol_L2_hi_ionosphere_f420_r8(fc,Nmf,beta,d,R0,z0,D1) result(L2)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L2_hi_ionosphere_f420_r8
            !dir$ attributes forceinline :: analytic_sol_L2_hi_ionosphere_f420_r8
#endif 
!$omp declare simd(analytic_sol_L2_hi_ionosphere_f420_r8)
            real(kind=dp),  intent(in) :: fc 
            real(kind=dp),  intent(in) :: Nmf 
            real(kind=dp),  intent(in) :: beta 
            real(kind=dp),  intent(in) :: d 
            real(kind=dp),  intent(in) :: R0 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: D1 
            real(kind=dp) :: L2 
            real(kind=dp), automatic :: L01, L02, L03 
            real(kind=dp), automatic :: dnEps, ctgz0, i2cosz0, ssecz0 
            real(kind=dp), automatic :: trm1, trm2, trm3, t0, t1  
            ctgz0  = 1.0_sp/tan(z0) 
            dnEps  = compute_delnEps_f421_r8(fc,Nmf,beta,d)
            t0     = cos(z0)
            t1     = 1.0_dp/t0 
            ssecz0 = t1*t1 
            i2cosz0= 1.0_dp/(t0*t0) 
            L01    = analytic_sol_L01_hi_ionosphere_f422_r8(fc,Nmf,beta,d,R0,z0,D1)
            trm1   = L01+(1.0_dp-beta*R0*dnEps)
            L02    = analytic_sol_L02_hi_ionosphere_f423_r8(fc,Nmf,beta,d,R0,z0,D1)
            trm2   = ctgz0*ssecz0*L02 
            L03    = analytic_sol_L03_hi_ionosphere_f424_r8(fc,Nmf,beta,d,R0,z0,D1)
            trm3   = dnEps*beta*R0*ctgz0*i2cosz0*L03 
            L2     = trm1+trm2+trm3 
      end function analytic_sol_L2_hi_ionosphere_f420_r8

      ! угол рефракции в ионосфере
      ! L1 — величина угла рефракции в нижней 
      ! ионосфере; L2 — величина угла рефракции в верхней ионосфере;
      ! formula: 4.15, page: 77
      elemental function refraction_angle_in_ionosphere_f415_r4(fc,Nmf,beta,d,R0,z0,D1) result(L)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_in_ionosphere_f415_r4
            !dir$ attributes forceinline :: refraction_angle_in_ionosphere_f415_r4
#endif 
!$omp declare simd(refraction_angle_in_ionosphere_f415_r4)
            real(kind=sp),  intent(in) :: fc 
            real(kind=sp),  intent(in) :: Nmf 
            real(kind=sp),  intent(in) :: beta 
            real(kind=sp),  intent(in) :: d 
            real(kind=sp),  intent(in) :: R0 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: D1 
            real(kind=sp) :: L
            real(kind=sp), automatic :: L1, L2 
            L1 = analytic_sol_L1_lo_ionosphere_f418_r4(fc,Nmf,z0,d,R0)
            L2 = analytic_sol_L2_hi_ionosphere_f420_r4(fc,Nmf,beta,d,R0,z0,D1)
            L  = L1+L2 
      end function refraction_angle_in_ionosphere_f415_r4

      elemental function refraction_angle_in_ionosphere_f415_r8(fc,Nmf,beta,d,R0,z0,D1) result(L)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_in_ionosphere_f415_r8
            !dir$ attributes forceinline :: refraction_angle_in_ionosphere_f415_r8
#endif 
!$omp declare simd(refraction_angle_in_ionosphere_f415_r8)
            real(kind=dp),  intent(in) :: fc 
            real(kind=dp),  intent(in) :: Nmf 
            real(kind=dp),  intent(in) :: beta 
            real(kind=dp),  intent(in) :: d 
            real(kind=dp),  intent(in) :: R0 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: D1 
            real(kind=dp) :: L
            real(kind=dp), automatic :: L1, L2 
            L1 = analytic_sol_L1_lo_ionosphere_f418_r8(fc,Nmf,z0,d,R0)
            L2 = analytic_sol_L2_hi_ionosphere_f420_r8(fc,Nmf,beta,d,R0,z0,D1)
            L  = L1+L2 
      end function refraction_angle_in_ionosphere_f415_r8

      ! частные случаи общей формулы (4.10).
      ! 1. m<t 1 и z0 <60°.
      ! formula: 4.25, page: 79
      elemental function refraction_angle_ionosphere_z0le60_f425_r4(fc,Nmf,d,R0,z0) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_ionosphere_z0le60_f425_r4
            !dir$ attributes forceinline :: refraction_angle_ionosphere_z0le60_f425_r4
#endif 
!$omp declare simd(refraction_angle_ionosphere_z0le60_f425_r4)
             real(kind=sp),  intent(in) :: fc 
             real(kind=sp),  intent(in) :: Nmf 
             real(kind=sp),  intent(in) :: d 
             real(kind=sp),  intent(in) :: R0 
             real(kind=sp),  intent(in) :: z0 
             real(kind=sp) :: alpha 
             real(kind=sp), parameter :: C0666666666666666666666666666667 = 0.666666666666666666666666666667_sp
             real(kind=sp), automatic :: delnM, dR0, tgz0, scosz0
             real(kind=sp), automatic :: trm1, trm2, t0 
             dR0    = d/R0 
             tgz0   = tan(z0) 
             t0     = cos(z0)
             scosz0 = t0*t0 
             delnM  = compute_delnM_f414_r4(fc,Nmf)
             trm2   = tgz0/scosz0
             t0     = delNm*0.5_sp 
             trm1   = C0666666666666666666666666666667*delnM*dR0 
             alpha  = (trm1+t0)*trm2
      end function refraction_angle_ionosphere_z0le60_f425_r4

       elemental function refraction_angle_ionosphere_z0le60_f425_r8(fc,Nmf,d,R0,z0) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_ionosphere_z0le60_f425_r8
            !dir$ attributes forceinline :: refraction_angle_ionosphere_z0le60_f425_r8
#endif 
!$omp declare simd(refraction_angle_ionosphere_z0le60_f425_r8)
             real(kind=dp),  intent(in) :: fc 
             real(kind=dp),  intent(in) :: Nmf 
             real(kind=dp),  intent(in) :: d 
             real(kind=dp),  intent(in) :: R0 
             real(kind=dp),  intent(in) :: z0 
             real(kind=dp) :: alpha 
             real(kind=dp), parameter :: C0666666666666666666666666666667 = 0.666666666666666666666666666667_dp
             real(kind=dp), automatic :: delnM, dR0, tgz0, scosz0
             real(kind=dp), automatic :: trm1, trm2, t0 
             dR0    = d/R0 
             tgz0   = tan(z0) 
             t0     = cos(z0)
             scosz0 = t0*t0 
             delnM  = compute_delnM_f414_r8(fc,Nmf)
             trm2   = tgz0/scosz0
             t0     = delNm*0.5_dp 
             trm1   = C0666666666666666666666666666667*delnM*dR0 
             alpha  = (trm1+t0)*trm2
      end function refraction_angle_ionosphere_z0le60_f425_r8

      ! m > 1 и z0=90°.
      ! formula: 4.28, page: 79
      elemental function refraction_angle_ionosphere_z0eq90_f428_r4(fc,Nmf,d,R0,z0) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_ionosphere_z0eq90_f428_r4
            !dir$ attributes forceinline :: refraction_angle_ionosphere_z0eq90_f428_r4
#endif 
!$omp declare simd(refraction_angle_ionosphere_z0eq90_f428_r4)
             real(kind=sp),  intent(in) :: fc 
             real(kind=sp),  intent(in) :: Nmf 
             real(kind=sp),  intent(in) :: d 
             real(kind=sp),  intent(in) :: R0 
             real(kind=sp),  intent(in) :: z0 
             real(kind=sp) :: alpha 
             real(kind=sp), parameter :: C1666666666666666666666666666667 = 1.666666666666666666666666666667_sp
             real(kind=sp), parameter :: C48                              = 4.8_sp 
             real(kind=sp), automatic :: delnM, R02d, sqr, sqrp3
             real(kind=sp), automatic :: t0, trm1, trm2 
             R02d   = R0/(d+d)
             delnM  = compute_delnM_f414_r4(fc,Nmf)
             sqr    = sqrt(R02d)
             trm1   = C1666666666666666666666666666667*delnM*sqr 
             sqrp3  = sqr*sqr*sqr 
             trm2   = C48*delnM*delnM*sqrp3 
             angle  = trm1+trm2 
      end function refraction_angle_ionosphere_z0eq90_f428_r4

       elemental function refraction_angle_ionosphere_z0eq90_f428_r8(fc,Nmf,d,R0,z0) result(alpha)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_ionosphere_z0eq90_f428_r8
            !dir$ attributes forceinline :: refraction_angle_ionosphere_z0eq90_f428_r8
#endif 
!$omp declare simd(refraction_angle_ionosphere_z0eq90_f428_r8)
             real(kind=dp),  intent(in) :: fc 
             real(kind=dp),  intent(in) :: Nmf 
             real(kind=dp),  intent(in) :: d 
             real(kind=dp),  intent(in) :: R0 
             real(kind=dp),  intent(in) :: z0 
             real(kind=dp) :: alpha 
             real(kind=dp), parameter :: C1666666666666666666666666666667 = 1.666666666666666666666666666667_dp
             real(kind=dp), parameter :: C48                              = 4.8_dp 
             real(kind=dp), automatic :: delnM, R02d, sqr, sqrp3
             real(kind=dp), automatic :: t0, trm1, trm2 
             R02d   = R0/(d+d)
             delnM  = compute_delnM_f414_r8(fc,Nmf)
             sqr    = sqrt(R02d)
             trm1   = C1666666666666666666666666666667*delnM*sqr 
             sqrp3  = sqr*sqr*sqr 
             trm2   = C48*delnM*delnM*sqrp3 
             angle  = trm1+trm2 
      end function refraction_angle_ionosphere_z0eq90_f428_r8
     
      ! усредненный
      ! показатель преломления атмосферы меняется.
      ! 0<=h<=H1
      ! formula: 4.29, page: 80
      elemental function n_avg_0_h_H1_f429_r4(deln0,beta,h) result(n)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_avg_0_h_H1_f429_r4
            !dir$ attributes forceinline :: n_avg_0_h_H1_f429_r4
#endif 
!$omp declare simd(n_avg_0_h_H1_f429_r4)
            real(kind=sp), intent(in) :: deln0 
            real(kind=sp), intent(in) :: beta 
            real(kind=sp), intent(in) :: h 
            real(kind=sp) :: n 
            real(kind=sp), automatic :: bh, exp1 
            bh  = -beta*h 
            exp1= exp(bh)
            n   = 1.0_sp+deln0*exp1 
      end function n_avg_0_h_H1_f429_r4

      elemental function n_avg_0_h_H1_f429_r8(deln0,beta,h) result(n)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_avg_0_h_H1_f429_r8
            !dir$ attributes forceinline :: n_avg_0_h_H1_f429_r8
#endif 
!$omp declare simd(n_avg_0_h_H1_f429_r8)
            real(kind=dp), intent(in) :: deln0 
            real(kind=dp), intent(in) :: beta 
            real(kind=dp), intent(in) :: h 
            real(kind=dp) :: n 
            real(kind=dp), automatic :: bh, exp1 
            bh  = -beta*h 
            exp1= exp(bh)
            n   = 1.0_dp+deln0*exp1 
      end function n_avg_0_h_H1_f429_r8

      ! усредненный
      ! показатель преломления атмосферы меняется.
      ! H1<=h<=H2
      ! formula: 4.30, page: 80
       elemental function n_avg_H1_h_H2_f430_r4(fc,Nmf,h,H1,H2) result(n)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_avg_H1_h_H2_f430_r4
            !dir$ attributes forceinline :: n_avg_H1_h_H2_f430_r4
#endif 
!$omp declare simd(n_avg_H1_h_H2_f430_r4)
            real(kind=sp),  intent(in) :: fc 
            real(kind=sp),  intent(in) :: Nmf 
            real(kind=sp),  intent(in) :: h 
            real(kind=sp),  intent(in) :: H1 
            real(kind=sp),  intent(in) :: H2 
            real(kind=sp),  :: n 
            real(kind=sp), automatic :: delNm, rat1, sqr1, sqr2
            real(kind=sp), automatic :: rat2, trm1, trm2, t0, t1  
            t0   = h-H1 
            t1   = H2-H1 
            rat1 = t0/t1 
            delNm= compute_delnM_f414_r4(fc,Nmf)
            rat2 = (t0*t0)/(t1*t1)
            trm1 = 1.0_sp-delNm 
            trm2 = 2.0_sp*rat1-rat2 
            n    = trm1*trm2  
       end function n_avg_H1_h_H2_f430_r4

       elemental function n_avg_H1_h_H2_f430_r8(fc,Nmf,h,H1,H2) result(n)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_avg_H1_h_H2_f430_r8
            !dir$ attributes forceinline :: n_avg_H1_h_H2_f430_r8
#endif 
!$omp declare simd(n_avg_H1_h_H2_f430_r8)
            real(kind=dp),  intent(in) :: fc 
            real(kind=dp),  intent(in) :: Nmf 
            real(kind=dp),  intent(in) :: h 
            real(kind=dp),  intent(in) :: H1 
            real(kind=dp),  intent(in) :: H2 
            real(kind=dp),  :: n 
            real(kind=dp), automatic :: delNm, rat1, sqr1, sqr2
            real(kind=dp), automatic :: rat2, trm1, trm2, t0, t1  
            t0   = h-H1 
            t1   = H2-H1 
            rat1 = t0/t1 
            delNm= compute_delnM_f414_r8(fc,Nmf)
            rat2 = (t0*t0)/(t1*t1)
            trm1 = 1.0_dp-delNm 
            trm2 = 2.0_dp*rat1-rat2 
            n    = trm1*trm2  
       end function n_avg_H1_h_H2_f430_r8

      ! усредненный
      ! показатель преломления атмосферы меняется.
      ! H2<=h<=H3
      ! formula: 4.31, page: 80
      elemental function n_avg_H2_h_H3_f431_r4(fc,Nmf,h,H2) result(n)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_avg_H2_h_H3_f431_r4
            !dir$ attributes forceinline :: n_avg_H2_h_H3_f431_r4
#endif 
!$omp declare simd(n_avg_H2_h_H3_f431_r4)
            real(kind=sp),  intent(in) :: fc 
            real(kind=sp),  intent(in) :: Nmf 
            real(kind=sp),  intent(in) :: h 
            real(kind=sp),  intent(in) :: H2 
            real(kind=sp),  :: n 
            real(kind=sp), automatic :: hH2, earg, exp1, delnM
            hH2  =  h-H2 
            earg = -beta*hH2
            delnM=  compute_delnM_f414_r4(fc,Nmf)
            exp1 =  exp(earg)
            n    - 1.0_sp-delnM*exp1
      end function n_avg_H2_h_H3_f431_r4

      elemental function n_avg_H2_h_H3_f431_r8(fc,Nmf,h,H2) result(n)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_avg_H2_h_H3_f431_r8
            !dir$ attributes forceinline :: n_avg_H2_h_H3_f431_r8
#endif 
!$omp declare simd(n_avg_H2_h_H3_f431_r8)
            real(kind=dp),  intent(in) :: fc 
            real(kind=dp),  intent(in) :: Nmf 
            real(kind=dp),  intent(in) :: h 
            real(kind=dp),  intent(in) :: H2 
            real(kind=dp),  :: n 
            real(kind=dp), automatic :: hH2, earg, exp1, delnM
            hH2  =  h-H2 
            earg = -beta*hH2
            delnM=  compute_delnM_f414_r8(fc,Nmf)
            exp1 =  exp(earg)
            n    - 1.0_dp-delnM*exp1
      end function n_avg_H2_h_H3_f431_r8

      !Так к-ак усредненная зависимость показателя 
      !преломления атмосферы определяется тремя 
      !соотношениями (4.29), (4.30) и (4.31), то (4.33) целесообразно 
      !разбить на три слагаемых
      !L=L1+L2+L3

      elemental function analytic_sol_L11_lo_ionosphere_f439_r4(deln0,beta,a,z0,H1) result(L11)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L11_lo_ionosphere_f439_r4
            !dir$ attributes forceinline :: analytic_sol_L11_lo_ionosphere_f439_r4
#endif 
!$omp declare simd(analytic_sol_L11_lo_ionosphere_f439_r4)
            real(kind=sp),  intent(in) :: deln0 
            real(kind=sp),  intent(in) :: beta 
            real(kind=sp),  intent(in) :: a 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: H1 
            real(kind=sp) :: L11 
            real(kind=sp), automatic :: ctgz0, ssecz0, stgz0, delba
            real(kind=sp), automatic :: bH1, sqr, sqrtrm, t0, t1 
            real(kind=sp), automatic :: exp1, exp2, trm1, trm2 
            bH1    = beta*H1 
            exp1   = exp(-bH1)
            t0     = 1.0_sp/cos(z0)
            ssecz0 = t0*t0 
            delba  = deln0*deln0*beta*a 
            t0     = tan(z0)
            stgz0  = t0*t0 
            t1     = 1.0_sp/t0 
            ctgz0  = t1*t1 
            trm1   = delba*ctgz0*ssecz0 
            exp2   = exp1(-2.0_sp*bH1)
            sqrtrm = 1.0_sp+(2.0_sp*stgz0*H1)/a 
            sqr    = sqrt(sqrtrm)
            trm2   = (exp1-exp2)*sqr 
            L11    = trm1*trm2
      end function analytic_sol_L11_lo_ionosphere_f439_r4

      elemental function analytic_sol_L11_lo_ionosphere_f439_r8(deln0,beta,a,z0,H1) result(L11)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L11_lo_ionosphere_f439_r8
            !dir$ attributes forceinline :: analytic_sol_L11_lo_ionosphere_f439_r8
#endif 
!$omp declare simd(analytic_sol_L11_lo_ionosphere_f439_r8)
            real(kind=dp),  intent(in) :: deln0 
            real(kind=dp),  intent(in) :: beta 
            real(kind=dp),  intent(in) :: a 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: H1 
            real(kind=dp) :: L11 
            real(kind=dp), automatic :: ctgz0, ssecz0, stgz0, delba
            real(kind=dp), automatic :: bH1, sqr, sqrtrm, t0, t1 
            real(kind=dp), automatic :: exp1, exp2, trm1, trm2 
            bH1    = beta*H1 
            exp1   = exp(-bH1)
            t0     = 1.0_dp/cos(z0)
            ssecz0 = t0*t0 
            delba  = deln0*deln0*beta*a 
            t0     = tan(z0)
            stgz0  = t0*t0 
            t1     = 1.0_dp/t0 
            ctgz0  = t1*t1 
            trm1   = delba*ctgz0*ssecz0 
            exp2   = exp1(-2.0_dp*bH1)
            sqrtrm = 1.0_dp+(2.0_dp*stgz0*H1)/a 
            sqr    = sqrt(sqrtrm)
            trm2   = (exp1-exp2)*sqr 
            L11    = trm1*trm2
      end function analytic_sol_L11_lo_ionosphere_f439_r8

      elemental function analytic_sol_L12_lo_ionosphere_f440_r4(deln0,beta,a,z0,H1) result(L12)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L12_lo_ionosphere_f440_r4
            !dir$ attributes forceinline :: analytic_sol_L12_lo_ionosphere_f440_r4
#endif 
!$omp declare simd(analytic_sol_L12_lo_ionosphere_f440_r4)
            real(kind=sp),  intent(in) :: deln0 
            real(kind=sp),  intent(in) :: beta 
            real(kind=sp),  intent(in) :: a 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: H1 
            real(kind=sp)              :: L12 
            real(kind=sp),  parameter  :: C314159265358979323846264338328 = 3.14159265358979323846264338328_sp
            real(kind=sp),  automatic  :: deln0, ctgz0, piba, bactgz0, sctgz0  
            real(kind=sp),  automatic  :: prob1, prob2, sqr1, sqr2 
            real(kind=sp),  automatic  :: trm1, trm2, exp1, earg, t0, t1 
            piba    = C314159265358979323846264338328*beta*a*0.5_sp 
            ctgz0   = 1.0_sp/tan(z0)
            sctgz0  = ctgz0*ctgz0 
            bactgz0 = beta*a*sctgz0
            exp1    = exp(bactgz0*0.5_sp)
            trm1    = deln0*sqrt(piba)*ctgz0*exp1 
            t0      = sqrt(bactgz0+2.0_sp*beta*H1)
            t1      = sqrt(bactgz0)
            prob1   = prob_integral_r4(t0)
            prob2   = prob_integral_r4(t1)
            trm2    = prob1-prob2 
            L12     = trm1*trm2 
      end function analytic_sol_L12_lo_ionosphere_f440_r4

     elemental function analytic_sol_L12_lo_ionosphere_f440_r8(deln0,beta,a,z0,H1) result(L12)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L12_lo_ionosphere_f440_r8
            !dir$ attributes forceinline :: analytic_sol_L12_lo_ionosphere_f440_r8
#endif 
!$omp declare simd(analytic_sol_L12_lo_ionosphere_f440_r8)
            real(kind=dp),  intent(in) :: deln0 
            real(kind=dp),  intent(in) :: beta 
            real(kind=dp),  intent(in) :: a 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: H1 
            real(kind=dp)              :: L12 
            real(kind=dp),  parameter  :: C314159265358979323846264338328 = 3.14159265358979323846264338328_dp
            real(kind=dp),  automatic  :: deln0, ctgz0, piba, bactgz0, sctgz0  
            real(kind=dp),  automatic  :: prob1, prob2, sqr1, sqr2 
            real(kind=dp),  automatic  :: trm1, trm2, exp1, earg, t0, t1 
            piba    = C314159265358979323846264338328*beta*a*0.5_dp 
            ctgz0   = 1.0_dp/tan(z0)
            sctgz0  = ctgz0*ctgz0 
            bactgz0 = beta*a*sctgz0
            exp1    = exp(bactgz0*0.5_dp)
            trm1    = deln0*sqrt(piba)*ctgz0*exp1 
            t0      = sqrt(bactgz0+2.0_dp*beta*H1)
            t1      = sqrt(bactgz0)
            prob1   = prob_integral_r8(t0)
            prob2   = prob_integral_r8(t1)
            trm2    = prob1-prob2 
            L12     = trm1*trm2 
      end function analytic_sol_L12_lo_ionosphere_f440_r8

      elemental function analytic_sol_L13_lo_ionosphere_f441_r4(deln0,beta,a,z0,H1) result(L12)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L13_lo_ionosphere_f441_r4
            !dir$ attributes forceinline :: analytic_sol_L13_lo_ionosphere_f441_r4
#endif 
!$omp declare simd(analytic_sol_L13_lo_ionosphere_f441_r4)
            real(kind=sp),  intent(in) :: deln0 
            real(kind=sp),  intent(in) :: beta 
            real(kind=sp),  intent(in) :: a 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: H1 
            real(kind=sp)              :: L12 
            real(kind=sp),  parameter  :: C314159265358979323846264338328 = 3.14159265358979323846264338328_sp
            real(kind=sp),  automatic  :: deln0, ctgz0, piba, bactgz0, sctgz0  
            real(kind=sp),  automatic  :: prob1, prob2, sqr1, sqr2 
            real(kind=sp),  automatic  :: trm1, trm2, exp1, earg, t0, t1 
            piba    = C314159265358979323846264338328*beta*a
            ctgz0   = 1.0_sp/tan(z0)
            sctgz0  = ctgz0*ctgz0 
            bactgz0 = beta*a*sctgz0
            exp1    = exp(bactgz0)
            trm1    = deln0*sqrt(piba)*ctgz0*exp1 
            t0      = sqrt((2.0_sp*bactgz0)+4.0_sp*beta*H1)
            t1      = sqrt(2.0_sp*bactgz0)
            prob1   = prob_integral_r4(t0)
            prob2   = prob_integral_r4(t1)
            trm2    = prob1-prob2 
            L12     = trm1*trm2 
      end function analytic_sol_L13_lo_ionosphere_f441_r4

      elemental function analytic_sol_L13_lo_ionosphere_f441_r8(deln0,beta,a,z0,H1) result(L12)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L13_lo_ionosphere_f441_r8
            !dir$ attributes forceinline :: analytic_sol_L13_lo_ionosphere_f441_r8
#endif 
!$omp declare simd(analytic_sol_L13_lo_ionosphere_f441_r8)
            real(kind=dp),  intent(in) :: deln0 
            real(kind=dp),  intent(in) :: beta 
            real(kind=dp),  intent(in) :: a 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: H1 
            real(kind=dp)              :: L12 
            real(kind=dp),  parameter  :: C314159265358979323846264338328 = 3.14159265358979323846264338328_dp
            real(kind=dp),  automatic  :: deln0, ctgz0, piba, bactgz0, sctgz0  
            real(kind=dp),  automatic  :: prob1, prob2, sqr1, sqr2 
            real(kind=dp),  automatic  :: trm1, trm2, exp1, earg, t0, t1 
            piba    = C314159265358979323846264338328*beta*a
            ctgz0   = 1.0_dp/tan(z0)
            sctgz0  = ctgz0*ctgz0 
            bactgz0 = beta*a*sctgz0
            exp1    = exp(bactgz0)
            trm1    = deln0*sqrt(piba)*ctgz0*exp1 
            t0      = sqrt((2.0_dp*bactgz0)+4.0_dp*beta*H1)
            t1      = sqrt(2.0_dp*bactgz0)
            prob1   = prob_integral_r8(t0)
            prob2   = prob_integral_r8(t1)
            trm2    = prob1-prob2 
            L12     = trm1*trm2 
      end function analytic_sol_L13_lo_ionosphere_f441_r8

      ! refraction angle whole atmosphere (lower part).
      ! formula: 4.38, page: 82
      elemental function refraction_angle_atmos_L1_lo_f438_r4(deln0,beta,a,z0,H1) resilt(L1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_atmos_L1_lo_f438_r4
            !dir$ attributes forceinline :: refraction_angle_atmos_L1_lo_f438_r4
#endif 
!$omp declare simd(refraction_angle_atmos_L1_lo_f438_r4)
            real(kind=sp),  intent(in) :: deln0 
            real(kind=sp),  intent(in) :: beta 
            real(kind=sp),  intent(in) :: a 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: H1 
            real(kind=sp) :: L1 
            real(kind=sp),  automatic :: L11, L12, L13 
            real(kind=sp),  automatic :: badln0, ctgz0, ssecz0 
            real(kind=sp),  automatic :: trm1, trm2, trm3, t0  
            badln0 = beta*a*deln0 
            ctgz0  = 1.0_sp/tan(z0)
            t0     = 1.0_sp/cos(z0)
            ssecz0 = t0*t0 
            L11    = analytic_sol_L11_lo_ionosphere_f439_r4(deln0,beta,a,z0,H1)
            trm1   = L11+(1.0_sp-badln0)
            L12    = analytic_sol_L12_lo_ionosphere_f440_r4(deln0,beta,a,z0,H1)
            trm2   = ctgz0*ssecz0*L12 
            L13    = analytic_sol_L13_lo_ionosphere_f441_r4(deln0,beta,a,z0,H1)
            trm3   = badln0*ctgz0*ssecz0*L13 
            L1     = L11*L12+L13 
      end function refraction_angle_atmos_L1_lo_f438_r4

      elemental function refraction_angle_atmos_L1_lo_f438_r8(deln0,beta,a,z0,H1) resilt(L1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_atmos_L1_lo_f438_r8
            !dir$ attributes forceinline :: refraction_angle_atmos_L1_lo_f438_r8
#endif 
!$omp declare simd(refraction_angle_atmos_L1_lo_f438_r8)
            real(kind=dp),  intent(in) :: deln0 
            real(kind=dp),  intent(in) :: beta 
            real(kind=dp),  intent(in) :: a 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: H1 
            real(kind=dp) :: L1 
            real(kind=dp),  automatic :: L11, L12, L13 
            real(kind=dp),  automatic :: badln0, ctgz0, ssecz0 
            real(kind=dp),  automatic :: trm1, trm2, trm3, t0  
            badln0 = beta*a*deln0 
            ctgz0  = 1.0_dp/tan(z0)
            t0     = 1.0_dp/cos(z0)
            ssecz0 = t0*t0 
            L11    = analytic_sol_L11_lo_ionosphere_f439_r8(deln0,beta,a,z0,H1)
            trm1   = L11+(1.0_dp-badln0)
            L12    = analytic_sol_L12_lo_ionosphere_f440_r8(deln0,beta,a,z0,H1)
            trm2   = ctgz0*ssecz0*L12 
            L13    = analytic_sol_L13_lo_ionosphere_f441_r8(deln0,beta,a,z0,H1)
            trm3   = badln0*ctgz0*ssecz0*L13 
            L1     = L11*L12+L13 
      end function refraction_angle_atmos_L1_lo_f438_r8

      ! formula: 4.43, page: 82
      elemental function analytic_sol_L21_med_ionosphere_f443_r4(fc,Nmf,H1,H2,a,z0) result(L21)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L21_med_ionosphere_f443_r4
            !dir$ attributes forceinline :: analytic_sol_L21_med_ionosphere_f443_r4
#endif 
!$omp declare simd(analytic_sol_L21_med_ionosphere_f443_r4)
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: H1 
            real(kind=sp),   intent(in) :: H2 
            real(kind=sp),   intent(in) :: a 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp) :: L21 
            real(kind=sp),   automatic :: delnM, ctgz0, stgz0, issinz0 
            real(kind=sp),   automatic :: sqrtrm1, sqrtrm2, t0, t1, athrd 
            real(kind=sp),   automatic :: trm1, trm2, trm3, trm4, trm5, stgz0 
            ctgz0  = 1.0_sp/tan(z0)
            t0     = sin(z0)
            issinz0= 1.0_sp/(t0*t0)
            athrd  = a*0.3333333333333333333333333333333333333333_sp
            t1     = tan(z0)
            stgz0  = t1*t1 
            delnM  = compute_delnM_f414_r4(fc,Nmf)
            t0     = 1.0_sp/((H2-H1)*(H2-H1))
            trm1   = 2.0_sp*delnM*a*t0*ctgz0*issinz0
            sqrtrm1= stgz0*(H1)/a
            sqrtrm2= stgz0*(H2)/a 
            t0     = sqrt(1.0_sp+2.0_sp*sqrtrm2)
            t1     = sqrt(1.0_sp+2.0_sp*sqrtrm1)
            trm2   = H2*(t0-t1)
            trm4   = 1.0_sp-sqrtrm1*t1 
            trm3   = 1.0_sp-sqrtrm2*t0 
            trm5   = ctgz0*ctgz0*(trm3-trm4)*athrd 
            L12    = trm1*trm2+trm5
      end function analytic_sol_L21_med_ionosphere_f443_r4

       elemental function analytic_sol_L21_med_ionosphere_f443_r8(fc,Nmf,H1,H2,a,z0) result(L21)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L21_med_ionosphere_f443_r8
            !dir$ attributes forceinline :: analytic_sol_L21_med_ionosphere_f443_r8
#endif 
!$omp declare simd(analytic_sol_L21_med_ionosphere_f443_r8)
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: H1 
            real(kind=dp),   intent(in) :: H2 
            real(kind=dp),   intent(in) :: a 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp) :: L21 
            real(kind=dp),   automatic :: delnM, ctgz0, stgz0, issinz0 
            real(kind=dp),   automatic :: sqrtrm1, sqrtrm2, t0, t1, athrd 
            real(kind=dp),   automatic :: trm1, trm2, trm3, trm4, trm5, stgz0 
            ctgz0  = 1.0_sp/tan(z0)
            t0     = sin(z0)
            issinz0= 1.0_sp/(t0*t0)
            athrd  = a*0.3333333333333333333333333333333333333333_dp
            t1     = tan(z0)
            stgz0  = t1*t1 
            delnM  = compute_delnM_f414_r8(fc,Nmf)
            t0     = 1.0_dp/((H2-H1)*(H2-H1))
            trm1   = 2.0_dp*delnM*a*t0*ctgz0*issinz0
            sqrtrm1= stgz0*(H1)/a
            sqrtrm2= stgz0*(H2)/a 
            t0     = sqrt(1.0_dp+2.0_dp*sqrtrm2)
            t1     = sqrt(1.0_dp+2.0_dp*sqrtrm1)
            trm2   = H2*(t0-t1)
            trm4   = 1.0_dp-sqrtrm1*t1 
            trm3   = 1.0_dp-sqrtrm2*t0 
            trm5   = ctgz0*ctgz0*(trm3-trm4)*athrd 
            L12    = trm1*trm2+trm5
      end function analytic_sol_L21_med_ionosphere_f443_r8

      ! formula: 4.44, page: 82
      elemental function analytic_sol_L22_med_ionosphere_f444_r4(deln0,fc,Nmf,H1,H2,a,z0) result(L22)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L22_med_ionosphere_f444_r4
            !dir$ attributes forceinline :: analytic_sol_L22_med_ionosphere_f444_r4
#endif 

            real(kind=sp),   intent(in) :: deln0
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: H1 
            real(kind=sp),   intent(in) :: H2 
            real(kind=sp),   intent(in) :: a 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp) :: L22 
            real(kind=sp),   automatic :: stgz0, delnM, scosz0, b4, tgz0  
            real(kind=sp),   automatic :: b2, b3, H1s, H2s 
            real(kind=sp),   automatic :: p, q, g, b, H2H1p4 
            real(kind=sp),   automatic :: trm1, trm2, lrat, rrat 
            real(kind=sp),   automatic :: t0, t1, t2, t3, cosz  
            tgz0    = tan(z0)
            stgz0   = tgz0*tgz0 
            delnM   = compute_delnM_f414_r4(fc,Nmf)
            b       = (2.0_sp*stgz0)/a
            H2s     = H2*H2 
            b4      = b*b*b*b 
            H1s     = H1*H1 
            t1      = (H2-H1)*(H2-H1)
            H2H1p4  = t1*t1 
            g       = H2s-t1*(1.0_sp+deln0/delnM)
            b2      = b*b 
            b3      = b2*b 
            t0      = 8.0_sp+24.0_sp*b*H2
            t1      = 19.0_sp*b2*H2s 
            t2      = 3.0_sp*b3*H2s*H2 
            cosz    = cos(z0) 
            t3      = 5.0_sp*b2*g 
            t4      = 1.0_sp+b*H2 
            p       = t0+t1+t2+t3+t4 
            t0      = 8.0_sp+4.0_sp*b*H1-b2*H2s  
            t1      = b3*(H2s*H2)*0.5_sp+20._sp*b*H2
            t2      = 10.0_sp*b2*H2s+10.0_sp*b2*H1*H2 
            t3      = 5.0_sp*b3*H1s*H2*0.5_sp+5.0_sp*b3*H1*H2s 
            t4      = 5.0_sp*b2*g+5.0_sp*b3*g*(H1+H2)*0.5_sp 
            q       = t0+t1+t2-t3+t4 
            lrat    = p/sqrt(1.0_sp+b*H2)
            rrat    = q/sqrt(1.0_sp+b*H1)
            trm2    = lrat-rrat 
            t0      = 8.0_sp*tgz0*delnM*delnM 
            t1      = 5.0_sp*cosz*cosz*b4*H2H1p4 
            trm1    = t0/t1 
            L22     = trm1*trm2 
      end function analytic_sol_L22_med_ionosphere_f444_r4

      elemental function analytic_sol_L22_med_ionosphere_f444_r8(deln0,fc,Nmf,H1,H2,a,z0) result(L22)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L22_med_ionosphere_f444_r8
            !dir$ attributes forceinline :: analytic_sol_L22_med_ionosphere_f444_r8
#endif 

            real(kind=dp),   intent(in) :: deln0
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: H1 
            real(kind=dp),   intent(in) :: H2 
            real(kind=dp),   intent(in) :: a 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp) :: L22 
            real(kind=dp),   automatic :: stgz0, delnM, scosz0, b4, tgz0  
            real(kind=dp),   automatic :: b2, b3, H1s, H2s 
            real(kind=dp),   automatic :: p, q, g, b, H2H1p4 
            real(kind=dp),   automatic :: trm1, trm2, lrat, rrat 
            real(kind=dp),   automatic :: t0, t1, t2, t3, cosz  
            tgz0    = tan(z0)
            stgz0   = tgz0*tgz0 
            delnM   = compute_delnM_f414_r8(fc,Nmf)
            b       = (2.0_dp*stgz0)/a
            H2s     = H2*H2 
            b4      = b*b*b*b 
            H1s     = H1*H1 
            t1      = (H2-H1)*(H2-H1)
            H2H1p4  = t1*t1 
            g       = H2s-t1*(1.0_dp+deln0/delnM)
            b2      = b*b 
            b3      = b2*b 
            t0      = 8.0_dp+24.0_dp*b*H2
            t1      = 19.0_dp*b2*H2s 
            t2      = 3.0_dp*b3*H2s*H2 
            cosz    = cos(z0) 
            t3      = 5.0_dp*b2*g 
            t4      = 1.0_dp+b*H2 
            p       = t0+t1+t2+t3+t4 
            t0      = 8.0_dp+4.0_dp*b*H1-b2*H2s  
            t1      = b3*(H2s*H2)*0.5_dp+20._sp*b*H2
            t2      = 10.0_dp*b2*H2s+10.0_dp*b2*H1*H2 
            t3      = 5.0_dp*b3*H1s*H2*0.5_dp+5.0_dp*b3*H1*H2s 
            t4      = 5.0_dp*b2*g+5.0_dp*b3*g*(H1+H2)*0.5_dp 
            q       = t0+t1+t2-t3+t4 
            lrat    = p/sqrt(1.0_dp+b*H2)
            rrat    = q/sqrt(1.0_dp+b*H1)
            trm2    = lrat-rrat 
            t0      = 8.0_dp*tgz0*delnM*delnM 
            t1      = 5.0_dp*cosz*cosz*b4*H2H1p4 
            trm1    = t0/t1 
            L22     = trm1*trm2 
      end function analytic_sol_L22_med_ionosphere_f444_r8

      ! refraction angle whole atmosphere (medium part).
      ! formula: 4.42, page: 82
      elemental function refraction_angle_atmos_L2_med_f442_r4(deln0,fc,Nmf,H1,H2,a,z0) result(L2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_atmos_L2_med_f442_r4
            !dir$ attributes forceinline :: refraction_angle_atmos_L2_med_f442_r4
#endif 

            real(kind=sp),   intent(in) :: deln0
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: H1 
            real(kind=sp),   intent(in) :: H2 
            real(kind=sp),   intent(in) :: a 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp) :: L2
            real(kind=sp),   automatic :: L21, L22 
            L21  = analytic_sol_L21_med_ionosphere_f443_r4(fc,Nmf,H1,H2,a,z0)
            L22  = analytic_sol_L22_med_ionosphere_f444_r4(deln0,fc,Nmf,H1,H2,a,z0)
            L2   = L21+L22 
      end function refraction_angle_atmos_L2_med_f442_r4

      elemental function refraction_angle_atmos_L2_med_f442_r8(deln0,fc,Nmf,H1,H2,a,z0) result(L2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_atmos_L2_med_f442_r8
            !dir$ attributes forceinline :: refraction_angle_atmos_L2_med_f442_r8
#endif 

            real(kind=dp),   intent(in) :: deln0
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: H1 
            real(kind=dp),   intent(in) :: H2 
            real(kind=dp),   intent(in) :: a 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp) :: L2
            real(kind=dp),   automatic :: L21, L22 
            L21  = analytic_sol_L21_med_ionosphere_f443_r8(fc,Nmf,H1,H2,a,z0)
            L22  = analytic_sol_L22_med_ionosphere_f444_r8(deln0,fc,Nmf,H1,H2,a,z0)
            L2   = L21+L22 
      end function refraction_angle_atmos_L2_med_f442_r8
     
      ! Analytic solution upper ionosphere.
      ! Formula: 4.46, page: 83
       elemental function analytic_sol_L31_up_ionosphere_f446_r4(fc,Nmf,H2,H3,beta,a,z0) result(L31)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L31_up_ionosphere_f446_r4
            !dir$ attributes forceinline :: analytic_sol_L31_up_ionosphere_f446_r4
#endif 
!$omp declare simd(analytic_sol_L31_up_ionosphere_f446_r4)
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: H2 
            real(kind=sp),   intent(in) :: H3 
            real(kind=sp),   intent(in) :: beta
            real(kind=sp),   intent(in) :: a 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp)  :: L31 
            real(kind=sp), automatic :: delNm, stgz0, ctgz0, earg 
            real(kind=sp), automatic :: trm1, trm2, exp1, ssecz0 
            real(kind=sp), automatic :: sqrtrm1, sqrtrm2, t0, t1 
            earg   = -2.0_sp*beta*(H3-H2)
            delnNm = compute_delnM_f414_r4(fc,Nmf)
            t0     = tan(z0)
            stgz0  = t0*t0 
            ctgz0  = 1.0_sp/t0 
            t0     = cos(z0)
            t1     = 1.0_sp/t0 
            ssecz0 = t1*t1 
            exp1   = exp(earg)
            trm1   = -delNm*delNm*beta*ctgz0*ssecz0 
            sqrtrm1= 1.0_sp+2.0_sp*stgz0*(H2/a)
            sqrtrm2= 1.0_sp+2.0_sp*stgz0*(H3/a)
            t0     = sqrt(sqrtrm1)
            t1     = sqrt(sqrtrm2)
            trm2   = t0-exp1*t1 
            L31    = trm1*trm2 
        end function analytic_sol_L31_up_ionosphere_f446_r4

        elemental function analytic_sol_L31_up_ionosphere_f446_r8(fc,Nmf,H2,H3,beta,a,z0) result(L31)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L31_up_ionosphere_f446_r8
            !dir$ attributes forceinline :: analytic_sol_L31_up_ionosphere_f446_r8
#endif 
!$omp declare simd(analytic_sol_L31_up_ionosphere_f446_r8)
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: H2 
            real(kind=dp),   intent(in) :: H3 
            real(kind=dp),   intent(in) :: beta
            real(kind=dp),   intent(in) :: a 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp)  :: L31 
            real(kind=dp), automatic :: delNm, stgz0, ctgz0, earg 
            real(kind=dp), automatic :: trm1, trm2, exp1, ssecz0 
            real(kind=dp), automatic :: sqrtrm1, sqrtrm2, t0, t1 
            earg   = -2.0_dp*beta*(H3-H2)
            delnNm = compute_delnM_f414_r8(fc,Nmf)
            t0     = tan(z0)
            stgz0  = t0*t0 
            ctgz0  = 1.0_dp/t0 
            t0     = cos(z0)
            t1     = 1.0_dp/t0 
            ssecz0 = t1*t1 
            exp1   = exp(earg)
            trm1   = -delNm*delNm*beta*ctgz0*ssecz0 
            sqrtrm1= 1.0_dp+2.0_dp*stgz0*(H2/a)
            sqrtrm2= 1.0_dp+2.0_dp*stgz0*(H3/a)
            t0     = sqrt(sqrtrm1)
            t1     = sqrt(sqrtrm2)
            trm2   = t0-exp1*t1 
            L31    = trm1*trm2 
        end function analytic_sol_L31_up_ionosphere_f446_r8

        ! Formula: 4.47, page: 83
        elemental function analytic_sol_L32_up_ionosphere_f447_r4(fc,Nmf,H2,H3,beta,a,z0) result(L32)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L32_up_ionosphere_f447_r4
            !dir$ attributes forceinline :: analytic_sol_L32_up_ionosphere_f447_r4
#endif 
!$omp declare simd(analytic_sol_L32_up_ionosphere_f447_r4)
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: H2 
            real(kind=sp),   intent(in) :: H3 
            real(kind=sp),   intent(in) :: beta
            real(kind=sp),   intent(in) :: a 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp) :: L32 
            real(kind=sp),  parameter  :: C314159265358979323846264338328 = 3.14159265358979323846264338328_sp
            real(kind=sp),   automatic :: delNm, piba, earg, bactgz 
            real(kind=sp),   automatic :: prob1, prob2, trm1, trm2 
            real(kind=sp),   automatic :: ctgz0, sctgz0, exp1, t0, t1
            piba  = C314159265358979323846264338328*beta*(a*0.5_sp)
            t0    = tan(z0)
            ctgz0 = 1.0_sp/t0 
            sctgz0= ctgz0*ctgz0 
            delNm = compute_delnM_f414_r4(fc,Nmf)
            bactgz0 = beta*a*sctgz0
            trm1    = -delnM*sqrt(piba)*ctgz0 
            earg    = beta*(H2+a*(sctgz0*0.5_sp))
            exp1    = exp(earg)
            t0      = sqrt(bactgz0+2.0_sp*beta*H3)
            t1      = sqrt(bactgz0+2.0_sp*beta*H2)
            prob1   = prob_integral_r4(t0)
            prob2   = prob_integral_r4(t1)
            trm2    = exp1*(prob1-prob2)
            L32     = trm1*trm2 
       end function analytic_sol_L32_up_ionosphere_f447_r4

       elemental function analytic_sol_L32_up_ionosphere_f447_r8(fc,Nmf,H2,H3,beta,a,z0) result(L32)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L32_up_ionosphere_f447_r8
            !dir$ attributes forceinline :: analytic_sol_L32_up_ionosphere_f447_r8
#endif 
!$omp declare simd(analytic_sol_L32_up_ionosphere_f447_r8)
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: H2 
            real(kind=dp),   intent(in) :: H3 
            real(kind=dp),   intent(in) :: beta
            real(kind=dp),   intent(in) :: a 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp) :: L32 
            real(kind=dp),  parameter  :: C314159265358979323846264338328 = 3.14159265358979323846264338328_dp
            real(kind=dp),   automatic :: delNm, piba, earg, bactgz 
            real(kind=dp),   automatic :: prob1, prob2, trm1, trm2 
            real(kind=dp),   automatic :: ctgz0, sctgz0, exp1, t0, t1
            piba  = C314159265358979323846264338328*beta*(a*0.5_sp)
            t0    = tan(z0)
            ctgz0 = 1.0_dp/t0 
            sctgz0= ctgz0*ctgz0 
            delNm = compute_delnM_f414_r8(fc,Nmf)
            bactgz0 = beta*a*sctgz0
            trm1    = -delnM*sqrt(piba)*ctgz0 
            earg    = beta*(H2+a*(sctgz0*0.5_dp))
            exp1    = exp(earg)
            t0      = sqrt(bactgz0+2.0_dp*beta*H3)
            t1      = sqrt(bactgz0+2.0_dp*beta*H2)
            prob1   = prob_integral_r8(t0)
            prob2   = prob_integral_r8(t1)
            trm2    = exp1*(prob1-prob2)
            L32     = trm1*trm2 
       end function analytic_sol_L32_up_ionosphere_f447_r8

       ! Formula: 4.48, page: 83
        elemental function analytic_sol_L33_up_ionosphere_f448_r4(fc,Nmf,H2,H3,beta,a,z0) result(L33)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L33_up_ionosphere_f448_r4
            !dir$ attributes forceinline :: analytic_sol_L33_up_ionosphere_f448_r4
#endif 
!$omp declare simd(analytic_sol_L33_up_ionosphere_f448_r4)
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: H2 
            real(kind=sp),   intent(in) :: H3 
            real(kind=sp),   intent(in) :: beta
            real(kind=sp),   intent(in) :: a 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp) :: L32 
            real(kind=sp),  parameter  :: C314159265358979323846264338328 = 3.14159265358979323846264338328_sp
            real(kind=sp),   automatic :: delNm, piba, earg, bactgz 
            real(kind=sp),   automatic :: prob1, prob2, trm1, trm2 
            real(kind=sp),   automatic :: ctgz0, sctgz0, exp1, t0, t1
            piba  = C314159265358979323846264338328*beta*a
            t0    = tan(z0)
            ctgz0 = 1.0_sp/t0 
            sctgz0= ctgz0*ctgz0 
            delNm = compute_delnM_f414_r4(fc,Nmf)
            bactgz0 = beta*a*sctgz0
            trm1    = -delnM*sqrt(piba)*ctgz0 
            earg    = beta*(H2+a*sctgz0)
            exp1    = exp(earg)
            t0      = sqrt(2.0_sp*bactgz0+4.0_sp*beta*H3)
            t1      = sqrt(2.0_sp*bactgz0+4.0_sp*beta*H2)
            prob1   = prob_integral_r4(t0)
            prob2   = prob_integral_r4(t1)
            trm2    = exp1*(prob1-prob2)
            L32     = trm1*trm2 
       end function analytic_sol_L33_up_ionosphere_f448_r4

       elemental function analytic_sol_L33_up_ionosphere_f448_r8(fc,Nmf,H2,H3,beta,a,z0) result(L33)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L33_up_ionosphere_f448_r8
            !dir$ attributes forceinline :: analytic_sol_L33_up_ionosphere_f448_r8
#endif 
!$omp declare simd(analytic_sol_L33_up_ionosphere_f448_r8)
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: H2 
            real(kind=dp),   intent(in) :: H3 
            real(kind=dp),   intent(in) :: beta
            real(kind=dp),   intent(in) :: a 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp) :: L33 
            real(kind=dp),  parameter  :: C314159265358979323846264338328 = 3.14159265358979323846264338328_dp
            real(kind=dp),   automatic :: delNm, piba, earg, bactgz 
            real(kind=dp),   automatic :: prob1, prob2, trm1, trm2 
            real(kind=dp),   automatic :: ctgz0, sctgz0, exp1, t0, t1
            piba    = C314159265358979323846264338328*beta*a
            t0      = tan(z0)
            ctgz0   = 1.0_dp/t0 
            sctgz0  = ctgz0*ctgz0 
            delNm   = compute_delnM_f414_r8(fc,Nmf)
            bactgz0 = beta*a*sctgz0
            trm1    = -delnM*sqrt(piba)*ctgz0 
            earg    = beta*(H2+a*sctgz0)
            exp1    = exp(earg)
            t0      = sqrt(2.0_dp*bactgz0+4.0_dp*beta*H3)
            t1      = sqrt(2.0_dp*bactgz0+4.0_dp*beta*H2)
            prob1   = prob_integral_r8(t0)
            prob2   = prob_integral_r8(t1)
            trm2    = exp1*(prob1-prob2)
            L32     = trm1*trm2 
       end function analytic_sol_L33_up_ionosphere_f448_r8

       ! Formula: 4.49, page: 83
       elemental function analytic_sol_L34_up_ionosphere_f449_r4(deln0,fc,Nmf,H2,H3,beta,a,z0) result(L34)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L34_up_ionosphere_f449_r4
            !dir$ attributes forceinline :: analytic_sol_L34_up_ionosphere_f449_r4
#endif 
!$omp declare simd(analytic_sol_L34_up_ionosphere_f449_r4)
            real(kind=sp),   intent(in) :: deln0 
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: H2 
            real(kind=sp),   intent(in) :: H3 
            real(kind=sp),   intent(in) :: beta
            real(kind=sp),   intent(in) :: a 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp)  :: L34 
            real(kind=sp), automatic :: delNm, stgz0, ctgz0, earg 
            real(kind=sp), automatic :: trm1, trm2, exp1, ssecz0 
            real(kind=sp), automatic :: sqrtrm1, sqrtrm2, t0, t1 
            earg   = beta*(H3-H2)
            delnNm = compute_delnM_f414_r4(fc,Nmf)
            t0     = tan(z0)
            stgz0  = t0*t0 
            ctgz0  = 1.0_sp/t0 
            t0     = cos(z0)
            t1     = 1.0_sp/t0 
            ssecz0 = t1*t1 
            exp1   = exp(earg)
            trm1   = -deln0*delNm*beta*ctgz0*ssecz0 
            sqrtrm1= 1.0_sp+2.0_sp*stgz0*(H2/a)
            sqrtrm2= 1.0_sp+2.0_sp*stgz0*(H3/a)
            t0     = sqrt(sqrtrm1)
            t1     = sqrt(sqrtrm2)
            trm2   = t0-exp1*t1 
            L31    = trm1*trm2 
        end function analytic_sol_L34_up_ionosphere_f449_r4

         elemental function analytic_sol_L34_up_ionosphere_f449_r8(deln0,fc,Nmf,H2,H3,beta,a,z0) result(L34)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L34_up_ionosphere_f449_r8
            !dir$ attributes forceinline :: analytic_sol_L34_up_ionosphere_f449_r8
#endif 
!$omp declare simd(analytic_sol_L34_up_ionosphere_f449_r8)
            real(kind=dp),   intent(in) :: deln0 
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: H2 
            real(kind=dp),   intent(in) :: H3 
            real(kind=dp),   intent(in) :: beta
            real(kind=dp),   intent(in) :: a 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp)  :: L34
            real(kind=dp), automatic :: delNm, stgz0, ctgz0, earg 
            real(kind=dp), automatic :: trm1, trm2, exp1, ssecz0 
            real(kind=dp), automatic :: sqrtrm1, sqrtrm2, t0, t1 
            earg   = beta*(H3-H2)
            delnNm = compute_delnM_f414_r8(fc,Nmf)
            t0     = tan(z0)
            stgz0  = t0*t0 
            ctgz0  = 1.0_dp/t0 
            t0     = cos(z0)
            t1     = 1.0_dp/t0 
            ssecz0 = t1*t1 
            exp1   = exp(earg)
            trm1   = -deln0*delNm*beta*ctgz0*ssecz0 
            sqrtrm1= 1.0_dp+2.0_dp*stgz0*(H2/a)
            sqrtrm2= 1.0_dp+2.0_dp*stgz0*(H3/a)
            t0     = sqrt(sqrtrm1)
            t1     = sqrt(sqrtrm2)
            trm2   = t0-exp1*t1 
            L31    = trm1*trm2 
        end function analytic_sol_L34_up_ionosphere_f449_r8

       ! refraction angle whole atmosphere (upper part).
       ! formula: 4.49, page: 83
      elemental function refraction_angle_atmos_L3_upper_f445_r4(deln0,fc,Nmf,H2,H3,beta,a,z0) result(L3)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_atmos_L3_upper_f445_r4
            !dir$ attributes forceinline :: refraction_angle_atmos_L3_upper_f445_r4
#endif 

            real(kind=sp),   intent(in) :: deln0
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: H2 
            real(kind=sp),   intent(in) :: H3 
            real(kind=sp),   intent(in) :: beta 
            real(kind=sp),   intent(in) :: a 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp)  :: L3 
            real(kind=sp),   automatic :: L31, L32, L33, L34 
            real(kind=sp),   automatic :: delNm, ctgz0, ssecz0, exp1 
            real(kind=sp),   automatic :: t0, t1, trm1, trm2, trm3   
            L32   = analytic_sol_L32_up_ionosphere_f447_r4(fc,Nmf,H2,H3,beta,a,z0) 
            exp1  =  exp(beta*H2)
            t0    =  tan(z0)
            ctgz0 = 1.0_sp/t0 
            t1    = 1.0_sp/cos(z0)
            ssecz0= t1*t1 
            delNm = compute_delnM_f414_r4(fc,Nmf)
            trm2  = delNm*beta*a*exp1 
            L31   = analytic_sol_L31_up_ionosphere_f446_r4(fc,Nmf,H2,H3,beta,a,z0) 
            trm1  = L31+(1.0_sp-deln0*beta*a)*ctgz0*ssecz0*L32 
            L33   = analytic_sol_L33_up_ionosphere_f448_r4(fc,Nmf,H2,H3,beta,a,z0) 
            L34   = analytic_sol_L34_up_ionosphere_f449_r4(deln0,fc,Nmf,H2,H3,beta,a,z0)
            trm3  = ctgz0*ssecz0*L33+L34 
            L3    = trm1-trm2*trm3 
       end function refraction_angle_atmos_L3_upper_f445_r4

       elemental function refraction_angle_atmos_L3_upper_f445_r8(deln0,fc,Nmf,H2,H3,beta,a,z0) result(L3)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_atmos_L3_upper_f445_r8
            !dir$ attributes forceinline :: refraction_angle_atmos_L3_upper_f445_r8
#endif 

            real(kind=dp),   intent(in) :: deln0
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: H2 
            real(kind=dp),   intent(in) :: H3 
            real(kind=dp),   intent(in) :: beta 
            real(kind=dp),   intent(in) :: a 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp)  :: L3 
            real(kind=dp),   automatic :: L31, L32, L33, L34 
            real(kind=dp),   automatic :: delNm, ctgz0, ssecz0, exp1 
            real(kind=dp),   automatic :: t0, t1, trm1, trm2, trm3   
            L32   = analytic_sol_L32_up_ionosphere_f447_r8(fc,Nmf,H2,H3,beta,a,z0) 
            exp1  =  exp(beta*H2)
            t0    =  tan(z0)
            ctgz0 = 1.0_dp/t0 
            t1    = 1.0_dp/cos(z0)
            ssecz0= t1*t1 
            delNm = compute_delnM_f414_r8(fc,Nmf)
            trm2  = delNm*beta*a*exp1 
            L31   = analytic_sol_L31_up_ionosphere_f446_r8(fc,Nmf,H2,H3,beta,a,z0) 
            trm1  = L31+(1.0_dp-deln0*beta*a)*ctgz0*ssecz0*L32 
            L33   = analytic_sol_L33_up_ionosphere_f448_r8(fc,Nmf,H2,H3,beta,a,z0) 
            L34   = analytic_sol_L34_up_ionosphere_f449_r8(deln0,fc,Nmf,H2,H3,beta,a,z0)
            trm3  = ctgz0*ssecz0*L33+L34 
            L3    = trm1-trm2*trm3 
       end function refraction_angle_atmos_L3_upper_f445_r8

       !характеризующего величину угла 
       !радиорефракции в земной атмосфере.
       ! 2*tg^2(z0)*H2/a«1, z0<60°.
       ! Formula: 4.50, page: 84
       elemental function refraction_angle_z0le60_med_atmos_f450_r4(fc,Nmf,z0,deln0,g,H1,H2) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_z0le60_med_atmos_f450_r4
            !dir$ attributes forceinline :: refraction_angle_z0le60_med_atmos_f450_r4
#endif 
!$omp declare simd(refraction_angle_z0le60_med_atmos_f450_r4)
            real(kind=sp),  intent(in) :: fc 
            real(kind=sp),  intent(in) :: Nmf 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: deln0 
            real(kind=sp),  intent(in) :: g 
            real(kind=sp),  intent(in) :: H1 
            real(kind=sp),  intent(in) :: H2 
            real(kind=sp)  :: alpha 
            real(kind=sp), parameter  :: inva = 0.000156985871271585557299843014_sp
            real(kind=sp),  automatic :: delnNm, tgz0, scosz0, rat1
            real(kind=sp),  automatic :: H1s, H2s, rat2, rat3 
            real(kind=sp),  automatic :: ghlf, trm1, trm2, trm3 
            real(kind=sp),  automatic :: t0, t1, t2, t3 
            H1s    = H1*H1 
            H2s    = H2*H2 
            tgz0   = tan(z0)
            ghlf   = g*0.5_sp 
            t0     = cos(z0)
            scosz0 = t0*t0 
            delnNm = compute_delnM_f414_r4(fc,Nmf)
            rat1   = tgz0/scosz0
            trm1   = deln0*tgz0+delnNm*rat1*inva 
            t1     = 2.0_sp*H2s+2.0_sp*H2*H1-H2s 
            t0     = 3.0_sp*(H2-H1)
            rat2   = H2-(t1/t0)
            t2     = (H2-H1)
            t3     = t2*t2*t2*t2 
            trm1   = trm1*rat2 
            t0     = 2.0_sp*(delnNm*delNm)
            trm2   = t0/t3*rat1 
            t1     = (H2s*H2s)*0.25_sp-ghlf*H2s 
            t2     = (H2s*H2s)*0.25_sp-H2*H1s*H1+H2s*H1s
            t3     = ghlf*H1s-g*H2*H1 
            trm3   = t1-t2+t3 
            alpha  = trm1+trm2*trm3
       end function refraction_angle_z0le60_med_atmos_f450_r4

       elemental function refraction_angle_z0le60_med_atmos_f450_r8(fc,Nmf,z0,deln0,g,H1,H2) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_z0le60_med_atmos_f450_r8
            !dir$ attributes forceinline :: refraction_angle_z0le60_med_atmos_f450_r8
#endif 
!$omp declare simd(refraction_angle_z0le60_med_atmos_f450_r8)
            real(kind=dp),  intent(in) :: fc 
            real(kind=dp),  intent(in) :: Nmf 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: deln0 
            real(kind=dp),  intent(in) :: g 
            real(kind=dp),  intent(in) :: H1 
            real(kind=dp),  intent(in) :: H2 
            real(kind=dp)  :: alpha 
            real(kind=dp), parameter  :: inva = 0.000156985871271585557299843014_dp
            real(kind=dp),  automatic :: delnNm, tgz0, scosz0, rat1
            real(kind=dp),  automatic :: H1s, H2s, rat2, rat3 
            real(kind=dp),  automatic :: ghlf, trm1, trm2, trm3 
            real(kind=dp),  automatic :: t0, t1, t2, t3 
            H1s    = H1*H1 
            H2s    = H2*H2 
            tgz0   = tan(z0)
            ghlf   = g*0.5_dp 
            t0     = cos(z0)
            scosz0 = t0*t0 
            delnNm = compute_delnM_f414_r8(fc,Nmf)
            rat1   = tgz0/scosz0
            trm1   = deln0*tgz0+delnNm*rat1*inva 
            t1     = 2.0_dp*H2s+2.0_dp*H2*H1-H2s 
            t0     = 3.0_dp*(H2-H1)
            rat2   = H2-(t1/t0)
            t2     = (H2-H1)
            t3     = t2*t2*t2*t2 
            trm1   = trm1*rat2 
            t0     = 2.0_dp*(delnNm*delNm)
            trm2   = t0/t3*rat1 
            t1     = (H2s*H2s)*0.25_dp-ghlf*H2s 
            t2     = (H2s*H2s)*0.25_dp-H2*H1s*H1+H2s*H1s
            t3     = ghlf*H1s-g*H2*H1 
            trm3   = t1-t2+t3 
            alpha  = trm1+trm2*trm3
       end function refraction_angle_z0le60_med_atmos_f450_r8

        !характеризующего величину угла 
       !радиорефракции в земной атмосфере.
       ! 2*tgz^2(z0)*H2/a >> 1, z0~90°.
       ! 
       ! Formula: 4.51, page: 84
       elemental function refraction_angle_z0eq90_med_atmos_f451_r4(fc,Nmf,z0,deln0,g,H1,H2) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_z0eq90_med_atmos_f451_r4
            !dir$ attributes forceinline :: refraction_angle_z0eq90_med_atmos_f451_r4
#endif 
!$omp declare simd(refraction_angle_z0eq90_med_atmos_f451_r4)
            real(kind=sp),  intent(in) :: fc 
            real(kind=sp),  intent(in) :: Nmf 
            real(kind=sp),  intent(in) :: z0 
            real(kind=sp),  intent(in) :: deln0 
            real(kind=sp),  intent(in) :: g 
            real(kind=sp),  intent(in) :: H1 
            real(kind=sp),  intent(in) :: H2 
            real(kind=sp)  :: alpha 
            real(kind=sp), parameter :: a = 6370.0_sp 
            real(kind=sp), parameter :: C314159265358979323846264338328       = 3.14159265358979323846264338328_sp
            real(kind=sp), parameter :: C141421356237309504880168872421       = 1.41421356237309504880168872421_sp
            real(kind=sp), parameter :: C112871608476179695133132585224253    = &
                                                                          112.871608476179695133132585224253_sp
            real(kind=sp), parameter :: C508404222051705624896764260500215822 = &
                                                                          508404.222051705624896764260500215822_sp
            real(kind=sp), automatic :: H2s, H1s, piba, sH2H1
            real(kind=sp), automatic :: sqrH1, sqrH2, t0, t1
            real(kind=sp), automatic :: t2, t3, trm1, trm2 
            
            H1s  = H1*H1 
            piba = sqrt((C314159265358979323846264338328*beta*a)*0.5_sp)
            H2s  = H2*H2 
            sqrH1= sqrt(H1)
            delNm= compute_delnM_f414_r4(fc,Nmf)
            sqrH2= sqrt(H2)
            trm1 = deln0*piba*(1.0_sp*(C141421356237309504880168872421-1)*deln0*beta*a)
            sH2H1= (H2-H1)*(H2-H1)
            t0   = 2.0_sp*delNm*C112871608476179695133132585224253/ &
                   sH2H1
            t1   = H2*(sqrH2-sqrH1)-0.3333333333333333333333_sp* &
                   (H2*sqrH2-H1*sqrH1)
            trm1 = t0*t1 
            t2   = C141421356237309504880168872421*delnNm*delNm* &
                   C508404222051705624896764260500215822/(sH2H1*sH2H1)
            t3   = 1.0_sp/sqrH2*(1.2_sp*H2s*H2+2.0_sp*g*H2)
            t0   = 1.0_sp/sqrH1*((H1s*H1)*0.2_sp-H2*H1s+ &
                   2.0_sp*H2s*H1+g*H1+g*H2)
            t1   = delNm*sqrt(a/(2.0_sp*H2)
            trm2 = t2*t3-t1 
            alpha= trm1+trm2 
       end function refraction_angle_z0eq90_med_atmos_f451_r4

        elemental function refraction_angle_z0eq90_med_atmos_f451_r8(fc,Nmf,z0,deln0,g,H1,H2) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_z0eq90_med_atmos_f451_r8
            !dir$ attributes forceinline :: refraction_angle_z0eq90_med_atmos_f451_r8
#endif 
!$omp declare simd(refraction_angle_z0eq90_med_atmos_f451_r8)
            real(kind=dp),  intent(in) :: fc 
            real(kind=dp),  intent(in) :: Nmf 
            real(kind=dp),  intent(in) :: z0 
            real(kind=dp),  intent(in) :: deln0 
            real(kind=dp),  intent(in) :: g 
            real(kind=dp),  intent(in) :: H1 
            real(kind=dp),  intent(in) :: H2 
            real(kind=dp)  :: alpha 
            real(kind=dp), parameter :: a = 6370.0_dp 
            real(kind=dp), parameter :: C314159265358979323846264338328       = 3.14159265358979323846264338328_dp
            real(kind=dp), parameter :: C141421356237309504880168872421       = 1.41421356237309504880168872421_dp
            real(kind=dp), parameter :: C112871608476179695133132585224253    = &
                                                                          112.871608476179695133132585224253_dp
            real(kind=dp), parameter :: C508404222051705624896764260500215822 = &
                                                                          508404.222051705624896764260500215822_dp
            real(kind=dp), automatic :: H2s, H1s, piba, sH2H1
            real(kind=dp), automatic :: sqrH1, sqrH2, t0, t1
            real(kind=dp), automatic :: t2, t3, trm1, trm2 
            
            H1s  = H1*H1 
            piba = sqrt((C314159265358979323846264338328*beta*a)*0.5_dp)
            H2s  = H2*H2 
            sqrH1= sqrt(H1)
            delNm= compute_delnM_f414_r8(fc,Nmf)
            sqrH2= sqrt(H2)
            trm1 = deln0*piba*(1.0_dp*(C141421356237309504880168872421-1.0_dp)*deln0*beta*a)
            sH2H1= (H2-H1)*(H2-H1)
            t0   = 2.0_sp*delNm*C112871608476179695133132585224253/ &
                   sH2H1
            t1   = H2*(sqrH2-sqrH1)-0.3333333333333333333333_dp* &
                   (H2*sqrH2-H1*sqrH1)
            trm1 = t0*t1 
            t2   = C141421356237309504880168872421*delnNm*delNm* &
                   C508404222051705624896764260500215822/(sH2H1*sH2H1)
            t3   = 1.0_dp/sqrH2*(1.2_dp*H2s*H2+2.0_dp*g*H2)
            t0   = 1.0_dp/sqrH1*((H1s*H1)*0.2_dp-H2*H1s+ &
                   2.0_dp*H2s*H1+g*H1+g*H2)
            t1   = delNm*sqrt(a/(2.0_dp*H2)
            trm2 = t2*t3-t1 
            alpha= trm1+trm2 
       end function refraction_angle_z0eq90_med_atmos_f451_r8

       !Рефракция электромагнитных волн (gamma (wavelength) < 5 см)
       !в земной атмосфере при различных высотах
       !излучателя и приемника.
       ! Formula: 5.4, page: 93
     
       elemental function analytic_sol_L1_troposph_wvle5cm_f54_r4(beta,R0,delnA,z0,Hc0) result(L1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_troposph_wvle5cm_f54_r4
            !dir$ attributes forceinline :: analytic_sol_L1_troposph_wvle5cm_f54_r4
#endif 
!$omp declare simd(analytic_sol_L1_troposph_wvle5cm_f54_r4)
             real(kind=sp),    intent(in) :: beta 
             real(kind=sp),    intent(in) :: R0 
             real(kind=sp),    intent(in) :: delnA 
             real(kind=sp),    intent(in) :: z0 
             real(kind=sp),    intent(in) :: Hc0 
             real(kind=sp)   :: L1 
             real(kind=sp),    parameter :: a = 6370.0_sp 
             real(kind=sp),    automatic :: stgz0, ctgz0, btHc0, scosz0 
             real(kind=sp),    automatic :: rat1, rat2, exp1, exp2 
             real(kind=sp),    automatic :: t0, t1
             t0     = tan(z0)
             t1     = cos(z0)
             ctgz0  = 1.0_sp/t0 
             scosz0 = t1*t1 
             stgz0  = t0*t0 
             btHc0  = beta*Hc0 
             exp1   = exp(-2.0_sp*btHc0) 
             rat1   = (beta*R0*delnA*delnA*ctgz0)/scosz0
             exp2   = exp(-btHc0)
             t0     = (1.0_sp+2.0_sp*stgz0*Hc0)/R0
             t1     = sqrt(t0)
             rat2   = (exp1-exp2)/t1 
             L1     = rat1*rat2 
        end function analytic_sol_L1_troposph_wvle5cm_f54_r4

         elemental function analytic_sol_L1_troposph_wvle5cm_f54_r8(beta,R0,delnA,z0,Hc0) result(L1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_troposph_wvle5cm_f54_r8
            !dir$ attributes forceinline :: analytic_sol_L1_troposph_wvle5cm_f54_r8
#endif 
!$omp declare simd(analytic_sol_L1_troposph_wvle5cm_f54_r8)
             real(kind=dp),    intent(in) :: beta 
             real(kind=dp),    intent(in) :: R0 
             real(kind=dp),    intent(in) :: delnA 
             real(kind=dp),    intent(in) :: z0 
             real(kind=dp),    intent(in) :: Hc0 
             real(kind=dp)   :: L1 
             real(kind=dp),    parameter :: a = 6370.0_dp 
             real(kind=dp),    automatic :: stgz0, ctgz0, btHc0, scosz0 
             real(kind=dp),    automatic :: rat1, rat2, exp1, exp2 
             real(kind=dp),    automatic :: t0, t1
             t0     = tan(z0)
             t1     = cos(z0)
             ctgz0  = 1.0_dp/t0 
             scosz0 = t1*t1 
             stgz0  = t0*t0 
             btHc0  = beta*Hc0 
             exp1   = exp(-2.0_dp*btHc0) 
             rat1   = (beta*R0*delnA*delnA*ctgz0)/scosz0
             exp2   = exp(-btHc0)
             t0     = (1.0_dp+2.0_dp*stgz0*Hc0)/R0
             t1     = sqrt(t0)
             rat2   = (exp1-exp2)/t1 
             L1     = rat1*rat2 
        end function analytic_sol_L1_troposph_wvle5cm_f54_r8

        elemental function analytic_sol_L2_troposph_wvle5cm_f55_r4(beta,R0,delnA,z0,Hc0) result(L2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L2_troposph_wvle5cm_f55_r4
            !dir$ attributes forceinline :: analytic_sol_L2_troposph_wvle5cm_f55_r4
#endif 
!$omp declare simd(analytic_sol_L2_troposph_wvle5cm_f55_r4)
             real(kind=sp),    intent(in) :: beta 
             real(kind=sp),    intent(in) :: R0 
             real(kind=sp),    intent(in) :: delnA 
             real(kind=sp),    intent(in) :: z0 
             real(kind=sp),    intent(in) :: Hc0 
             real(kind=sp)   :: L1 
             real(kind=sp),    parameter :: C157079632679489661923132169164 = 1.57079632679489661923132169164_sp
             real(kind=sp),    automatic :: tgz0, stgz0, sctgz0, btR0 
             real(kind=sp),    automatic :: prob1, prob2, exp1, bRctgz0 
             real(kind=sp),    automatic :: t0, t1, trm1, trm2 
             btR0   = beta*R0 
             tgz0   = tan(z0)
             stgz0  = tgz0*tgz0 
             t0     = 1.0_sp/tgz0 
             sctgz0 = t0*t0 
             exp1   = (btR0/(2._sp*stgz0))
             t1     = delnA*sqrt(btR0/tgz0)   
             bRctgz0= btR0*sctgz0  
             trm1   = t1*exp1*C157079632679489661923132169164
             t0     = sqrt(bRctgz0+2.0_sp*beta*Hc0)
             t1     = sqrt(bRctgz0)
             prob1  = prob_integral_r4(t0)
             prob2  = prob_integral_r4(t1)
             trm2   = prob1-prob2 
             L2     = trm1*trm2 
        end function analytic_sol_L2_troposph_wvle5cm_f55_r4

        elemental function analytic_sol_L2_troposph_wvle5cm_f55_r8(beta,R0,delnA,z0,Hc0) result(L2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L2_troposph_wvle5cm_f55_r8
            !dir$ attributes forceinline :: analytic_sol_L2_troposph_wvle5cm_f55_r8
#endif 
!$omp declare simd(analytic_sol_L2_troposph_wvle5cm_f55_r8)
             real(kind=dp),    intent(in) :: beta 
             real(kind=dp),    intent(in) :: R0 
             real(kind=dp),    intent(in) :: delnA 
             real(kind=dp),    intent(in) :: z0 
             real(kind=dp),    intent(in) :: Hc0 
             real(kind=dp)   :: L1 
             real(kind=dp),    parameter :: C157079632679489661923132169164 = 1.57079632679489661923132169164_dp
             real(kind=dp),    automatic :: tgz0, stgz0, sctgz0, btR0 
             real(kind=dp),    automatic :: prob1, prob2, exp1, bRctgz0 
             real(kind=dp),    automatic :: t0, t1, trm1, trm2 
             btR0   = beta*R0 
             tgz0   = tan(z0)
             stgz0  = tgz0*tgz0 
             t0     = 1.0_dp/tgz0 
             sctgz0 = t0*t0 
             exp1   = (btR0/(2._dp*stgz0))
             t1     = delnA*sqrt(btR0/tgz0)   
             bRctgz0= btR0*sctgz0  
             trm1   = t1*exp1*C157079632679489661923132169164
             t0     = sqrt(bRctgz0+2.0_dp*beta*Hc0)
             t1     = sqrt(bRctgz0)
             prob1  = prob_integral_r8(t0)
             prob2  = prob_integral_r8(t1)
             trm2   = prob1-prob2 
             L2     = trm1*trm2 
        end function analytic_sol_L2_troposph_wvle5cm_f55_r8

        elemental function analytic_sol_L3_troposph_wvle5cm_f56_r4(beta,R0,delnA,z0,Hc0) result(L3)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L3_troposph_wvle5cm_f56_r4
            !dir$ attributes forceinline :: analytic_sol_L3_troposph_wvle5cm_f56_r4
#endif 
!$omp declare simd(analytic_sol_L3_troposph_wvle5cm_f56_r4)
             real(kind=sp),    intent(in) :: beta 
             real(kind=sp),    intent(in) :: R0 
             real(kind=sp),    intent(in) :: delnA 
             real(kind=sp),    intent(in) :: z0 
             real(kind=sp),    intent(in) :: Hc0 
             real(kind=sp)   :: L1 
             real(kind=sp),    parameter :: C157079632679489661923132169164 = 1.57079632679489661923132169164_sp
             real(kind=sp),    automatic :: tgz0, stgz0, sctgz0, btR0 
             real(kind=sp),    automatic :: prob1, prob2, exp1, bRctgz0 
             real(kind=sp),    automatic :: t0, t1, trm1, trm2 
             btR0   = beta*R0 
             tgz0   = tan(z0)
             stgz0  = tgz0*tgz0 
             t0     = 1.0_sp/tgz0 
             sctgz0 = t0*t0 
             exp1   = (btR0/stgz0)
             t1     = delnA*sqrt((2.0_sp*btR0)/tgz0)   
             bRctgz0= btR0*sctgz0  
             trm1   = t1*exp1*C157079632679489661923132169164
             t0     = sqrt(2.0_sp*bRctgz0+4.0_sp*beta*Hc0)
             t1     = sqrt(2.0_sp*bRctgz0)
             prob1  = prob_integral_r4(t0)
             prob2  = prob_integral_r4(t1)
             trm2   = prob1-prob2 
             L2     = trm1*trm2 
        end function analytic_sol_L3_troposph_wvle5cm_f56_r4

        elemental function analytic_sol_L3_troposph_wvle5cm_f56_r8(beta,R0,delnA,z0,Hc0) result(L2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L3_troposph_wvle5cm_f56_r8
            !dir$ attributes forceinline :: analytic_sol_L3_troposph_wvle5cm_f56_r8
#endif 
!$omp declare simd(analytic_sol_L3_troposph_wvle5cm_f56_r8)
             real(kind=dp),    intent(in) :: beta 
             real(kind=dp),    intent(in) :: R0 
             real(kind=dp),    intent(in) :: delnA 
             real(kind=dp),    intent(in) :: z0 
             real(kind=dp),    intent(in) :: Hc0 
             real(kind=dp)   :: L1 
             real(kind=dp),    parameter :: C157079632679489661923132169164 = 1.57079632679489661923132169164_dp
             real(kind=dp),    automatic :: tgz0, stgz0, sctgz0, btR0 
             real(kind=dp),    automatic :: prob1, prob2, exp1, bRctgz0 
             real(kind=dp),    automatic :: t0, t1, trm1, trm2 
             btR0   = beta*R0 
             tgz0   = tan(z0)
             stgz0  = tgz0*tgz0 
             t0     = 1.0_dp/tgz0 
             sctgz0 = t0*t0 
             exp1   = (btR0/stgz0)
             t1     = delnA*sqrt((2.0_dp*btR0)/tgz0)   
             bRctgz0= btR0*sctgz0  
             trm1   = t1*exp1*C157079632679489661923132169164
             t0     = sqrt(2.0_dp*bRctgz0+4.0_dp*beta*Hc0)
             t1     = sqrt(2.0_dp*bRctgz0)
             prob1  = prob_integral_r8(t0)
             prob2  = prob_integral_r8(t1)
             trm2   = prob1-prob2 
             L2     = trm1*trm2 
        end function analytic_sol_L3_troposph_wvle5cm_f56_r8

        ! Formula 5.3, page: 93
        ! An angle of atmospheric (troposheric) refraction for wavelength <= 5cm (different TX,RX height)
        elemental function refraction_angle_tropo_wvle5cm_f53_r4(na,nc,beta,R0,delnA,z0,Hc0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_tropo_wvle5cm_f53_r4
            !dir$ attributes forceinline :: refraction_angle_tropo_wvle5cm_f53_r4
#endif 
!$omp declare simd(refraction_angle_tropo_wvle5cm_f53_r4)
             real(kind=sp),    intent(in) :: na 
             real(kind=sp),    intent(in) :: nc 
             real(kind=sp),    intent(in) :: beta 
             real(kind=sp),    intent(in) :: R0 
             real(kind=sp),    intent(in) :: delnA 
             real(kind=sp),    intent(in) :: z0 
             real(kind=sp),    intent(in) :: Hc0 
             real(kind=sp)   :: alpha 
             real(kind=sp),    automatic  :: lnanc, ctgz0, L1, ctgz0 
             real(kind=sp),    automatic  :: scosz0, btRdna, rat1, L2 
             real(kind=sp),    automatic  :: t0, t1, L3, trm1, trm2  
             t0     = cos(z0)
             scosz0 = t0*t0 
             btRdna = beta*R0*delnA 
             L1     = analytic_sol_L1_troposph_wvle5cm_f54_r4(beta,R0,delnA,z0,Hc0)
             t1     = tan(z0)
             ctgz0  = 1.0_sp/t1 
             lnanc  = -log(na/nc)
             L2     = analytic_sol_L2_troposph_wvle5cm_f56_r4(beta,R0,delnA,z0,Hc0)
             rat1   = ctgz0/scosz0
             trm1   = lnanc*ctgz0+L1+rat1 
             L3     = analytic_sol_L3_troposph_wvle5cm_f56_r4(beta,R0,delnA,z0,Hc0)
             trm2   = btRdna*rat1*(L3-L2)
             alpha  = trm1+trm2 
        end function refraction_angle_tropo_wvle5cm_f53_r4

        elemental function refraction_angle_tropo_wvle5cm_f53_r8(na,nc,beta,R0,delnA,z0,Hc0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_tropo_wvle5cm_f53_r8
            !dir$ attributes forceinline :: refraction_angle_tropo_wvle5cm_f53_r8
#endif 
!$omp declare simd(refraction_angle_tropo_wvle5cm_f53_r8)
             real(kind=dp),    intent(in) :: na 
             real(kind=dp),    intent(in) :: nc 
             real(kind=dp),    intent(in) :: beta 
             real(kind=dp),    intent(in) :: R0 
             real(kind=dp),    intent(in) :: delnA 
             real(kind=dp),    intent(in) :: z0 
             real(kind=dp),    intent(in) :: Hc0 
             real(kind=dp)   :: alpha 
             real(kind=dp),    automatic  :: lnanc, ctgz0, L1, ctgz0 
             real(kind=dp),    automatic  :: scosz0, btRdna, rat1, L2 
             real(kind=dp),    automatic  :: t0, t1, L3, trm1, trm2  
             t0     = cos(z0)
             scosz0 = t0*t0 
             btRdna = beta*R0*delnA 
             L1     = analytic_sol_L1_troposph_wvle5cm_f54_r8(beta,R0,delnA,z0,Hc0)
             t1     = tan(z0)
             ctgz0  = 1.0_dp/t1 
             lnanc  = -log(na/nc)
             L2     = analytic_sol_L2_troposph_wvle5cm_f56_r8(beta,R0,delnA,z0,Hc0)
             rat1   = ctgz0/scosz0
             trm1   = lnanc*ctgz0+L1+rat1 
             L3     = analytic_sol_L3_troposph_wvle5cm_f56_r8(beta,R0,delnA,z0,Hc0)
             trm2   = btRdna*rat1*(L3-L2)
             alpha  = trm1+trm2 
        end function refraction_angle_tropo_wvle5cm_f53_r4

        !Представим (5.15) в виде двух слагаемых, учитывая,
        !что: 1/n~1, z=z0-theta+alpha=z-gamma, (gamm<<1)
        !i.e. formula: 5.16, page: 95
        !рассчитать угол истинной атмосферной рёф-;
        !ракции б в диапазоне видимых зенитных угловч 0° <•
        !<г0<88° при условии, что показатель преломлений
        !атмосферы меняется с высотой по закону (1.45)

        ! formula: 5.22, page: 96
        elemental function analytic_sol_tropo_del1_wvle5cm_deg0_80_f522_r4(delnA,z0,beta,Hc0) result(del1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del1_wvle5cm_deg0_80_f522_r4
            !dir$ attributes forceinline :: analytic_sol_tropo_del1_wvle5cm_deg0_80_f522_r4
#endif 
!$omp declare simd(analytic_sol_tropo_del1_wvle5cm_deg0_80_f522_r4)
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: Hc0 
            real(kind=sp)  :: del1 
            real(kind=sp),    automatic  :: tgz0, btHc0, exp1, rat 
            btHc0  = beta*Hc0 
            tgz0   = tan(z0)
            exp1   = exp(-btHc0)
            rat    = (1.0_sp-exp1)/btHc0
            del1   = delnA*tgz0*(1.0_sp-rat)
        end function analytic_sol_tropo_del1_wvle5cm_deg0_80_f522_r4

        elemental function analytic_sol_tropo_del1_wvle5cm_deg0_80_f522_r8(delnA,z0,beta,Hc0) result(del1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del1_wvle5cm_deg0_80_f522_r8
            !dir$ attributes forceinline :: analytic_sol_tropo_del1_wvle5cm_deg0_80_f522_r8
#endif 
!$omp declare simd(analytic_sol_tropo_del1_wvle5cm_deg0_80_f522_r8)
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: Hc0 
            real(kind=dp)  :: del1 
            real(kind=dp),    automatic  :: tgz0, btHc0, exp1, rat 
            btHc0  = beta*Hc0 
            tgz0   = tan(z0)
            exp1   = exp(-btHc0)
            rat    = (1.0_dp-exp1)/btHc0
            del1   = delnA*tgz0*(1.0_dp-rat)
        end function analytic_sol_tropo_del1_wvle5cm_deg0_80_f522_r8

        !formula: 5.24, page: 97
        elemental function analytic_sol_tropo_del21_wvle5cm_deg0_80_f524_r4(delnA,z0,beta,Hc0) result(del21)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del21_wvle5cm_deg0_80_f524_r4
            !dir$ attributes forceinline :: analytic_sol_tropo_del21_wvle5cm_deg0_80_f524_r4
#endif 
!$omp declare simd(analytic_sol_tropo_del21_wvle5cm_deg0_80_f524_r4)
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: Hc0 
            real(kind=sp)  :: del21 
            real(kind=sp),    automatic  :: ctgz0, scosz0, btHc0, exp1, rat 
            real(kind=sp),    automatic  :: t0 
            btHc0  = beta*Hc0 
            t0     = tan(z0)
            ctgz0  = 1.0_sp/t0 
            exp1   = exp(-btHc0)
            t0     = cos(z0)
            scosz0 = t0*t0 
            rat    = (1.0_sp-exp1)/btHc0
            del21   = -delnA*(tgz0/scosz0)*(1.0_sp-rat)
        end function analytic_sol_tropo_del21_wvle5cm_deg0_80_f524_r4

        elemental function analytic_sol_tropo_del21_wvle5cm_deg0_80_f524_r8(delnA,z0,beta,Hc0) result(del21)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del21_wvle5cm_deg0_80_f524_r8
            !dir$ attributes forceinline :: analytic_sol_tropo_del21_wvle5cm_deg0_80_f524_r8
#endif 
!$omp declare simd(analytic_sol_tropo_del21_wvle5cm_deg0_80_f524_r8)
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: Hc0 
            real(kind=dp)  :: del21 
            real(kind=dp),    automatic  :: ctgz0, scosz0, btHc0, exp1, rat 
            real(kind=dp),    automatic  :: t0 
            btHc0  = beta*Hc0 
            t0     = tan(z0)
            ctgz0  = 1.0_dp/t0 
            exp1   = exp(-btHc0)
            t0     = cos(z0)
            scosz0 = t0*t0 
            rat    = (1.0_dp-exp1)/btHc0
            del21   = -delnA*(tgz0/scosz0)*(1.0_dp-rat)
        end function analytic_sol_tropo_del21_wvle5cm_deg0_80_f524_r8

        ! formula: 5.25, page: 97
        elemental function analytic_sol_tropo_del22_wvle5cm_deg0_80_f525_r4(delnA,z0,beta,Hc0,R0) result(del22)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del22_wvle5cm_deg0_80_f525_r4
            !dir$ attributes forceinline :: analytic_sol_tropo_del22_wvle5cm_deg0_80_f525_r4
#endif 

            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: Hc0 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp)  :: del22 
            real(kind=sp),    parameter  :: C1253314137315500251207882642406 = & 
                                                   1.253314137315500251207882642406_sp
            real(kind=sp),    automatic  :: ctgz0, ssinz0, exp1, exp2, stgz0  
            real(kind=sp),    automatic  :: ps, q, btR0, btHc0 
            real(kind=sp),    automatic  :: prob1, prob2, t0, t1
            real(kind=sp),    automatic  :: trm1, trm2, trm3, trm4, sqr2q 
            t0     = tan(z0)
            btR0   = beta*R0 
            btHc0  = beta*Hc0 
            t1     = sin(z0)
            stgz0  = t0*t0 
            ps     = 1.0_sp+2.0_sp*stgz0*(Hc0/R0)
            q      = (btR0*0.5_sp)*stgz0 
            ssinz0 = t1*t1 
            exp1   = exp(q)
            ctgz0  = 1.0_sp/t0 
            sqr2q  = sqrt(q+q)
            trm1   = delnA*(ctgz0/ssinz0)*exp1 
            t1     = 1.0_sp+(q/btHc0)-1.0_sp/(2.0_sp*btHc0)
            trm2   = t1*(1.0_sp/sqr2q)
            prob1  = prob_integral_r4(ps*sqr2q)
            prob2  = prob_integral_r4(sqr2q)
            trm3   = C1253314137315500251207882642406*(prob1-prob2)
            t0     = exp1/(btHc0+btHc0)
            exp2   = exp(q-q*ps)-1.0_sp
            trm4   = t0*exp2 
            del22  = trm1*(trm2*trm3+trm4)
        end function analytic_sol_tropo_del22_wvle5cm_deg0_80_f525_r4

         elemental function analytic_sol_tropo_del22_wvle5cm_deg0_80_f525_r8(delnA,z0,beta,Hc0,R0) result(del22)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del22_wvle5cm_deg0_80_f525_r8
            !dir$ attributes forceinline :: analytic_sol_tropo_del22_wvle5cm_deg0_80_f525_r8
#endif 

            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: Hc0 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp)  :: del22 
            real(kind=sp),    parameter  :: C1253314137315500251207882642406 = & 
                                                   1.253314137315500251207882642406_dp
            real(kind=dp),    automatic  :: ctgz0, ssinz0, exp1, exp2, stgz0  
            real(kind=dp),    automatic  :: ps, q, btR0, btHc0 
            real(kind=dp),    automatic  :: prob1, prob2, t0, t1
            real(kind=dp),    automatic  :: trm1, trm2, trm3, trm4, sqr2q 
            t0     = tan(z0)
            btR0   = beta*R0 
            btHc0  = beta*Hc0 
            t1     = sin(z0)
            stgz0  = t0*t0 
            ps     = 1.0_dp+2.0_dp*stgz0*(Hc0/R0)
            q      = (btR0*0.5_dp)*stgz0 
            ssinz0 = t1*t1 
            exp1   = exp(q)
            ctgz0  = 1.0_dp/t0 
            sqr2q  = sqrt(q+q)
            trm1   = delnA*(ctgz0/ssinz0)*exp1 
            t1     = 1.0_dp+(q/btHc0)-1.0_dp/(2.0_dp*btHc0)
            trm2   = t1*(1.0_dp/sqr2q)
            prob1  = prob_integral_r8(ps*sqr2q)
            prob2  = prob_integral_r8(sqr2q)
            trm3   = C1253314137315500251207882642406*(prob1-prob2)
            t0     = exp1/(btHc0+btHc0)
            exp2   = exp(q-q*ps)-1.0_dp
            trm4   = t0*exp2 
            del22  = trm1*(trm2*trm3+trm4)
        end function analytic_sol_tropo_del22_wvle5cm_deg0_80_f525_r8

          ! formula: 5.27, page: 97
        elemental function analytic_sol_tropo_del231_wvle5cm_deg0_80_f527_r4(delnA,z0,beta,Hc0,R0) result(del231)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del231_wvle5cm_deg0_80_f527_r4
            !dir$ attributes forceinline :: analytic_sol_tropo_del231_wvle5cm_deg0_80_f527_r4
#endif 
!$omp declare simd(analytic_sol_tropo_del231_wvle5cm_deg0_80_f527_r4)
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: Hc0 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp)                :: del231 
            real(kind=sp),    parameter  :: C1253314137315500251207882642406 = & 
                                                   1.253314137315500251207882642406_sp
            real(kind=sp),    automatic  :: stgz0, btHc0, exp1, sqr2q 
            real(kind=sp),    automatic  :: prob1, prob2, trm1, trm2 
            real(kind=sp),    automatic  :: t0, t1, ps2, q, exp2, trm3    
            btHc0   = beta*Hc0 
            t0      = tan(z0)
            stgz0   = t0*t0 
            ps      = 1.0_sp+2.0_sp*stgz0*(Hc0/R0)
            q       = (beta*R0*0.5_sp)*stgz0 
            exp1    = exp(q-q*ps)
            t0      = 1.0_sp+(q/btHc0)
            trm1    = t0*(1.0_sp-exp1/p) 
            sqr2q   = sqrt(q+q)
            prob1   = prob_integral_r4(p*sqr2q)
            prob2   = prob_integral_r4(sqr2q)
            exp2    = exp(q)
            t0      = 2.0_sp*q+q/btHc0+(2.0_sp*q*q)/btHc0
            t1      = exp2/sqr2q*C1253314137315500251207882642406
            trm2    = t0*t1 
            trm3    = prob1-prob2 
            del231  = trm1-trm2*trm3 
        end function analytic_sol_tropo_del231_wvle5cm_deg0_80_f527_r4

         elemental function analytic_sol_tropo_del231_wvle5cm_deg0_80_f527_r8(delnA,z0,beta,Hc0,R0) result(del231)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del231_wvle5cm_deg0_80_f527_r8
            !dir$ attributes forceinline :: analytic_sol_tropo_del231_wvle5cm_deg0_80_f527_r8
#endif 
!$omp declare simd(analytic_sol_tropo_del231_wvle5cm_deg0_80_f527_r8)
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: Hc0 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp)                :: del231 
            real(kind=dp),    parameter  :: C1253314137315500251207882642406 = & 
                                                   1.253314137315500251207882642406_dp
            real(kind=dp),    automatic  :: stgz0, btHc0, exp1, sqr2q 
            real(kind=dp),    automatic  :: prob1, prob2, trm1, trm2 
            real(kind=dp),    automatic  :: t0, t1, ps2, q, exp2, trm3    
            btHc0   = beta*Hc0 
            t0      = tan(z0)
            stgz0   = t0*t0 
            ps      = 1.0_dp+2.0_dp*stgz0*(Hc0/R0)
            q       = (beta*R0*0.5_dp)*stgz0 
            exp1    = exp(q-q*ps)
            t0      = 1.0_dp+(q/btHc0)
            trm1    = t0*(1.0_dp-exp1/p) 
            sqr2q   = sqrt(q+q)
            prob1   = prob_integral_r8(p*sqr2q)
            prob2   = prob_integral_r8(sqr2q)
            exp2    = exp(q)
            t0      = 2.0_dp*q+q/btHc0+(2.0_dp*q*q)/btHc0
            t1      = exp2/sqr2q*C1253314137315500251207882642406
            trm2    = t0*t1 
            trm3    = prob1-prob2 
            del231  = trm1-trm2*trm3 
        end function analytic_sol_tropo_del231_wvle5cm_deg0_80_f527_r8

        ! formula: 5.28, page: 97
        elemental function analytic_sol_tropo_del232_wvle5cm_deg0_80_f528_r4(delnA,z0,beta,Hc0,R0) result(del232)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del232_wvle5cm_deg0_80_f528_r4
            !dir$ attributes forceinline :: analytic_sol_tropo_del232_wvle5cm_deg0_80_f528_r4
#endif 
!$omp declare simd(analytic_sol_tropo_del232_wvle5cm_deg0_80_f528_r4)
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: Hc0 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp)                :: del231 
            real(kind=sp),    parameter  :: C1253314137315500251207882642406 = & 
                                                   1.253314137315500251207882642406_sp
            real(kind=sp),    automatic  :: stgz0, btHc0, exp1, sqr2q 
            real(kind=sp),    automatic  :: prob1, prob2, trm1, trm2 
            real(kind=sp),    automatic  :: t0, t1, ps2, q, exp2, trm3    
            btHc0   = beta*Hc0 
            t0      = tan(z0)
            stgz0   = t0*t0 
            ps      = 1.0_sp+2.0_sp*stgz0*(Hc0/R0)
            q       = (beta*R0*0.5_sp)*stgz0 
            exp1    = exp(2.0_sp*q-2.0_sp*q*ps)
            t0      = 1.0_sp+(q/btHc0)
            trm1    = t0*(1.0_sp-exp1/p) 
            sqr2q   = sqrt(4.0_sp*q)
            prob1   = prob_integral_r4(p*sqr2q)
            prob2   = prob_integral_r4(sqr2q)
            exp2    = exp(q)
            t0      = 4.0_sp*q+q/btHc0+(4.0_sp*q*q)/btHc0
            t1      = exp2/sqr2q*C1253314137315500251207882642406
            trm2    = t0*t1 
            trm3    = prob1-prob2 
            del231  = trm1-trm2*trm3 
        end function analytic_sol_tropo_del232_wvle5cm_deg0_80_f528_r4

        elemental function analytic_sol_tropo_del232_wvle5cm_deg0_80_f528_r8(delnA,z0,beta,Hc0,R0) result(del232)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del232_wvle5cm_deg0_80_f528_r8
            !dir$ attributes forceinline :: analytic_sol_tropo_del232_wvle5cm_deg0_80_f528_r8
#endif 
!$omp declare simd(analytic_sol_tropo_del232_wvle5cm_deg0_80_f528_r8)
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: Hc0 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp)                :: del231 
            real(kind=dp),    parameter  :: C1253314137315500251207882642406 = & 
                                                   1.253314137315500251207882642406_dp
            real(kind=dp),    automatic  :: stgz0, btHc0, exp1, sqr2q 
            real(kind=dp),    automatic  :: prob1, prob2, trm1, trm2 
            real(kind=dp),    automatic  :: t0, t1, ps2, q, exp2, trm3    
            btHc0   = beta*Hc0 
            t0      = tan(z0)
            stgz0   = t0*t0 
            ps      = 1.0_dp+2.0_dp*stgz0*(Hc0/R0)
            q       = (beta*R0*0.5_dp)*stgz0 
            exp1    = exp(2.0_dp*q-2.0_dp*q*ps)
            t0      = 1.0_dp+(q/btHc0)
            trm1    = t0*(1.0_dp-exp1/p) 
            sqr2q   = sqrt(4.0_dp*q)
            prob1   = prob_integral_r8(p*sqr2q)
            prob2   = prob_integral_r8(sqr2q)
            exp2    = exp(q)
            t0      = 4.0_dp*q+q/btHc0+(4.0_sp*q*q)/btHc0
            t1      = exp2/sqr2q*C1253314137315500251207882642406
            trm2    = t0*t1 
            trm3    = prob1-prob2 
            del231  = trm1-trm2*trm3 
        end function analytic_sol_tropo_del232_wvle5cm_deg0_80_f528_r8

        ! formula: 5.256, page: 97
        elemental function analytic_sol_tropo_del23_wvle5cm_deg0_80_f526_r4(delnA,z0,beta,Hc0,R0) result(del23)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del23_wvle5cm_deg0_80_f526_r4
            !dir$ attributes forceinline :: analytic_sol_tropo_del23_wvle5cm_deg0_80_f526_r4
#endif 

            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: Hc0 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp)                :: del23 
            real(kind=sp),    automatic  :: ctgz0, scosz0, del231, del232 
            real(kind=sp),    automatic  :: sdelnA, t0, btR0, rat  
            btR0    = beta*R0 
            sdelnA  = delnA*delnA 
            t0      = tan(z0)
            ctgz0   = 1.0_sp/t0 
            del231  = analytic_sol_tropo_del231_wvle5cm_deg0_80_f527_r4(delnA,z0,beta,Hc0,R0)
            t0      = cos(z0)
            scosz0  = t0*t0 
            del232  = analytic_sol_tropo_del232_wvle5cm_deg0_80_f528_r4(delnA,z0,beta,Hc0,R0)
            rat     = ctgz0/scosz0 
            t0      = del231-del232 
            del23   = sdelnA*btR0*rat*t0 
       end function analytic_sol_tropo_del23_wvle5cm_deg0_80_f526_r4

       elemental function analytic_sol_tropo_del23_wvle5cm_deg0_80_f526_r8(delnA,z0,beta,Hc0,R0) result(del23)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del23_wvle5cm_deg0_80_f526_r8
            !dir$ attributes forceinline :: analytic_sol_tropo_del23_wvle5cm_deg0_80_f526_r8
#endif 

            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: Hc0 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp)                :: del23 
            real(kind=dp),    automatic  :: ctgz0, scosz0, del231, del232 
            real(kind=dp),    automatic  :: sdelnA, t0, btR0, rat  
            btR0    = beta*R0 
            sdelnA  = delnA*delnA 
            t0      = tan(z0)
            ctgz0   = 1.0_dp/t0 
            del231  = analytic_sol_tropo_del231_wvle5cm_deg0_80_f527_r8(delnA,z0,beta,Hc0,R0)
            t0      = cos(z0)
            scosz0  = t0*t0 
            del232  = analytic_sol_tropo_del232_wvle5cm_deg0_80_f528_r8(delnA,z0,beta,Hc0,R0)
            rat     = ctgz0/scosz0 
            t0      = del231-del232 
            del23   = sdelnA*btR0*rat*t0 
       end function analytic_sol_tropo_del23_wvle5cm_deg0_80_f526_r8

        ! formula: 5.23, page: 96
        elemental function analytic_sol_tropo_del2_wvle5cm_deg0_80_f523_r4(delnA,z0,beta,Hc0,R0) result(del2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del2_wvle5cm_deg0_80_f523_r4
            !dir$ attributes forceinline :: analytic_sol_tropo_del2_wvle5cm_deg0_80_f523_r4
#endif 

            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: Hc0 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp)                :: del2
            real(kind=sp),    automatic  :: del21, del22, del23 
            del21   =  analytic_sol_tropo_del21_wvle5cm_deg0_80_f524_r4(delnA,z0,beta,Hc0) 
            del22   =  analytic_sol_tropo_del22_wvle5cm_deg0_80_f525_r4(delnA,z0,beta,Hc0,R0)
            del23   =  analytic_sol_tropo_del23_wvle5cm_deg0_80_f526_r4(delnA,z0,beta,Hc0,R0) 
            del2    = del21+del22+del23 
        end function analytic_sol_tropo_del2_wvle5cm_deg0_80_f523_r4

        elemental function analytic_sol_tropo_del2_wvle5cm_deg0_80_f523_r48(delnA,z0,beta,Hc0,R0) result(del2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del2_wvle5cm_deg0_80_f523_r8
            !dir$ attributes forceinline :: analytic_sol_tropo_del2_wvle5cm_deg0_80_f523_r8
#endif 

            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: Hc0 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp)                :: del2
            real(kind=dp),    automatic  :: del21, del22, del23 
            del21   =  analytic_sol_tropo_del21_wvle5cm_deg0_80_f524_r8(delnA,z0,beta,Hc0) 
            del22   =  analytic_sol_tropo_del22_wvle5cm_deg0_80_f525_r8(delnA,z0,beta,Hc0,R0)
            del23   =  analytic_sol_tropo_del23_wvle5cm_deg0_80_f526_r8(delnA,z0,beta,Hc0,R0) 
            del2    = del21+del22+del23 
        end function analytic_sol_tropo_del2_wvle5cm_deg0_80_f523_r8

        ! Final calculation of refractive angle.
        ! Formula 5.17, page: 95
        elemental function refraction_angle_tropo_wvle5cm_f517_r4(delnA,z0,beta,Hc0,R0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_tropo_wvle5cm_f517_r4
            !dir$ attributes forceinline :: refraction_angle_tropo_wvle5cm_f517_r4
#endif 

            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: Hc0 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp)   :: alpha
            real(kind=sp),    automatic  :: del1, del2 
            del1  =  analytic_sol_tropo_del1_wvle5cm_deg0_80_f522_r4(delnA,z0,beta,Hc0)
            del2  =  analytic_sol_tropo_del2_wvle5cm_deg0_80_f523_r4(delnA,z0,beta,Hc0,R0)
            alpha = del1+del2 
        end function refraction_angle_tropo_wvle5cm_f517_r4

        elemental function refraction_angle_tropo_wvle5cm_f517_r8(delnA,z0,beta,Hc0,R0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_tropo_wvle5cm_f517_r8
            !dir$ attributes forceinline :: refraction_angle_tropo_wvle5cm_f517_r8
#endif 

            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: Hc0 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp)   :: alpha
            real(kind=dp),    automatic  :: del1, del2 
            del1  =  analytic_sol_tropo_del1_wvle5cm_deg0_80_f522_r8(delnA,z0,beta,Hc0)
            del2  =  analytic_sol_tropo_del2_wvle5cm_deg0_80_f523_r8(delnA,z0,beta,Hc0,R0)
            alpha = del1+del2 
        end function refraction_angle_tropo_wvle5cm_f517_r8

        ! For: z0<=80(deg), p*sqrt(2*q) >= 1, sqrt(2*q) >= 1, ***can be used instead of 5.23***
        ! Formula: 5.29, page: 95
         elemental function analytic_sol_tropo_del2_wvle5cm_deg0_80_f529_r4(delnA,z0,beta,Hc0,R0) result(del2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del2_wvle5cm_deg0_80_f529_r4
            !dir$ attributes forceinline :: analytic_sol_tropo_del2_wvle5cm_deg0_80_f529_r4
#endif 
!$omp declare simd(analytic_sol_tropo_del2_wvle5cm_deg0_80_f529_r4)
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: Hc0 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp)                :: del2
            real(kind=sp),    automatic  :: tgz0, scosz0, btHc0, ebtHc0
            real(kind=sp),    automatic  :: rat1, rat2, rat3, rat4 
            real(kind=sp),    automatic  :: t0, t1, t2 
            real(kind=sp),    automatic  :: trm1, trm2, trm3  
            btHc0  = beta*Hc0 
            tgz0   = tan(z0)
            t0     = cos(z0)
            ebtHc0 = exp(-btHc0)
            scosz0 = t0*t0 
            trm1   = delnA*(tgz0/scosz0)
            t0     = (Hc0/R0)*ebtHc0
            t1     = 1.0_sp-ebtHc0/(2.0_sp*beta*beta*Hc0*R0)
            t2     = 1.0_sp+ebtHc0/(beta*R0)
            trm2   = t0+t1-t2 
            t0     = 0.5_sp+1.0_sp-ebtHc0/(2.0_sp*btHc0)
            t2     = 8.0_sp*btHc0
            t1     = 1.0_sp-exp(-2.0_sp*btHc0)/t2 
            trm3   = delnA*(t0-t1)
            del2   = trm1*(trm2+trm3)
         end function analytic_sol_tropo_del2_wvle5cm_deg0_80_f529_r4

         elemental function analytic_sol_tropo_del2_wvle5cm_deg0_80_f529_r8(delnA,z0,beta,Hc0,R0) result(del2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_tropo_del2_wvle5cm_deg0_80_f529_r8
            !dir$ attributes forceinline :: analytic_sol_tropo_del2_wvle5cm_deg0_80_f529_r8
#endif 
!$omp declare simd(analytic_sol_tropo_del2_wvle5cm_deg0_80_f529_r8)
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: Hc0 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp)                :: del2
            real(kind=dp),    automatic  :: tgz0, scosz0, btHc0, ebtHc0
            real(kind=dp),    automatic  :: rat1, rat2, rat3, rat4 
            real(kind=dp),    automatic  :: t0, t1, t2 
            real(kind=dp),    automatic  :: trm1, trm2, trm3  
            btHc0  = beta*Hc0 
            tgz0   = tan(z0)
            t0     = cos(z0)
            ebtHc0 = exp(-btHc0)
            scosz0 = t0*t0 
            trm1   = delnA*(tgz0/scosz0)
            t0     = (Hc0/R0)*ebtHc0
            t1     = 1.0_dp-ebtHc0/(2.0_dp*beta*beta*Hc0*R0)
            t2     = 1.0_dp+ebtHc0/(beta*R0)
            trm2   = t0+t1-t2 
            t0     = 0.5_dp+1.0_dp-ebtHc0/(2.0_dp*btHc0)
            t2     = 8.0_dp*btHc0
            t1     = 1.0_dp-exp(-2.0_dp*btHc0)/t2 
            trm3   = delnA*(t0-t1)
            del2   = trm1*(trm2+trm3)
         end function analytic_sol_tropo_del2_wvle5cm_deg0_80_f529_r8

         ! Рефракция электромагнитных волн (Х<5 см)
         ! в земной атмосфере при близких или равных
         ! высотах излучателя и приемника.

         ! Уравнение траектории луча в сферически 
         ! неоднородной атмосфере.
         ! позволяет установить связь между
         ! радиусом-вектором 'r' и геоцентрическим углом 'theta'. С 
         ! учетом малости угла 'theta' эта зависимость имеет вид
         elemental function ray_traj_inhomogenous_atmos_f531_r4(n,na,R0,z0,tht) result(r)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: ray_traj_inhomogenous_atmos_f531_r4
            !dir$ attributes forceinline :: ray_traj_inhomogenous_atmos_f531_r4
#endif 
!$omp declare simd(ray_traj_inhomogenous_atmos_f531_r4)
            real(kind=sp),   intent(in) :: n
            real(kind=sp),   intent(in) :: na 
            real(kind=sp),   intent(in) :: R0 ! a+H0
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp),   intent(in) :: tht 
            real(kind=sp)    :: r 
            real(kind=sp),   automatic  :: ctgz0, sctgz0, trm1, trm2 
            real(kind=sp),   automatic  :: stht, t0, t1  
            trm1   = (na*R0)/n 
            ctgz0  = 1.0_sp/tan(z0)
            sctgz0 = ctgz0*ctgz0 
            t1     = 1.0_sp+ctgz0*tht 
            stht   = tht*tht 
            t0     = (sctgz0+0.5_sp)*stht
            trm2   = t1+t0 
            r      = trm1*trm2 
         end function ray_traj_inhomogenous_atmos_f531_r4

         elemental function ray_traj_inhomogenous_atmos_f531_r8(n,na,R0,z0,tht) result(r)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: ray_traj_inhomogenous_atmos_f531_r8
            !dir$ attributes forceinline :: ray_traj_inhomogenous_atmos_f531_r8
#endif 
!$omp declare simd(ray_traj_inhomogenous_atmos_f531_r8)
            real(kind=dp),   intent(in) :: n
            real(kind=dp),   intent(in) :: na 
            real(kind=dp),   intent(in) :: R0 ! a+H0
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp),   intent(in) :: tht 
            real(kind=dp)    :: r 
            real(kind=dp),   automatic  :: ctgz0, sctgz0, trm1, trm2 
            real(kind=dp),   automatic  :: stht, t0, t1  
            trm1   = (na*R0)/n 
            ctgz0  = 1.0_dp/tan(z0)
            sctgz0 = ctgz0*ctgz0 
            t1     = 1.0_dp+ctgz0*tht 
            stht   = tht*tht 
            t0     = (sctgz0+0.5_dp)*stht
            trm2   = t1+t0 
            r      = trm1*trm2 
         end function ray_traj_inhomogenous_atmos_f531_r8

         ! Рефракция электромагнитных волн (Х<5 см)
         ! в земной атмосфере при близких или равных
         ! высотах излучателя и приемника.
         ! Formula: 5.34, page: 100
         elemental function analytic_sol_L_atmos_wvle5cm_f534_r4(z0,beta,R0,thtc) result(L)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L_atmos_wvle5cm_f534_r4
            !dir$ attributes forceinline :: analytic_sol_L_atmos_wvle5cm_f534_r4
#endif 
!$omp declare simd(analytic_sol_L_atmos_wvle5cm_f534_r4)
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp),   intent(in) :: beta 
            real(kind=sp),   intent(in) :: R0 
            real(kind=sp),   intent(in) :: thtc 
            real(kind=sp)               :: L 
            real(kind=sp), automatic    :: ctgz0, sctgz0, btR0
            real(kind=sp), automatic    :: p1, q1, sp1
            real(kind=sp), automatic    :: t0, t1
            real(kind=sp), automatic    :: exp1, exp2 
            real(kind=sp), automatic    :: rat1, rat2 
            real(kind=sp), automatic    :: tbtR0, trm1, trm2  
            btR0    = beta*R0 
            tbtR0   = btR0+btR0 
            ctgz0   = 1.0_sp/tan(z0)
            sctgz0  = ctgz0*ctgz0 
            q1      = sqrt(0.5_sp*sctgz0)
            t0      = sqrt(0.5_sp+sctgz0)
            p1      = ctgz0/(2.0_sp+t0)
            sp1     = p1*p1 
            rat1    = p1/tbtR0 
            exp1    = exp(-btR0*sp1)
            t1      = (p1+q1*thtc)/tbtR0 
            t0      = (p1+q1*thtc)*(p1+q1*thtc)
            exp2    = exp(-btR0*t0)
            trm1    = rat1*exp1 
            trm2    = t1*exp2 
            L       = trm1-trm2 
         end function analytic_sol_L_atmos_wvle5cm_f534_r4

         elemental function analytic_sol_L_atmos_wvle5cm_f534_r8(z0,beta,R0,thtc) result(L)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L_atmos_wvle5cm_f534_r8
            !dir$ attributes forceinline :: analytic_sol_L_atmos_wvle5cm_f534_r8
#endif 
!$omp declare simd(analytic_sol_L_atmos_wvle5cm_f534_r8)
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp),   intent(in) :: beta 
            real(kind=dp),   intent(in) :: R0 
            real(kind=dp),   intent(in) :: thtc 
            real(kind=dp)               :: L 
            real(kind=dp), automatic    :: ctgz0, sctgz0, btR0
            real(kind=dp), automatic    :: p1, q1, sp1
            real(kind=dp), automatic    :: t0, t1
            real(kind=dp), automatic    :: exp1, exp2 
            real(kind=dp), automatic    :: rat1, rat2 
            real(kind=dp), automatic    :: tbtR0, trm1, trm2  
            btR0    = beta*R0 
            tbtR0   = btR0+btR0 
            ctgz0   = 1.0_dp/tan(z0)
            sctgz0  = ctgz0*ctgz0 
            q1      = sqrt(0.5_dp*sctgz0)
            t0      = sqrt(0.5_dp+sctgz0)
            p1      = ctgz0/(2.0_dp+t0)
            sp1     = p1*p1 
            rat1    = p1/tbtR0 
            exp1    = exp(-btR0*sp1)
            t1      = (p1+q1*thtc)/tbtR0 
            t0      = (p1+q1*thtc)*(p1+q1*thtc)
            exp2    = exp(-btR0*t0)
            trm1    = rat1*exp1 
            trm2    = t1*exp2 
            L       = trm1-trm2 
         end function analytic_sol_L_atmos_wvle5cm_f534_r8

       !Рассмотрим несколько частных случаев соотношения
       !(5.33), наиболее типичных для практических 
       !приложений.
       !А. Высоты излучателя и приемника близки .друг к
       !другу, что равносильно выполнению неравенств u2 < 1
       !И u1 < 1.
       !Formula: 5.35, page: 101
       elemental function refraction_angle_atmos_wvle5cm_f535_r4(delnA,beta,R0,thtc,z0,  &
                                                                 Rc,nc,na) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_atmos_wvle5cm_f535_r4
            !dir$ attributes forceinline :: refraction_angle_atmos_wvle5cm_f535_r4
#endif 
!$omp declare simd(refraction_angle_atmos_wvle5cm_f535_r4)
            real(kind=sp),     intent(in) :: delnA 
            real(kind=sp),     intent(in) :: beta 
            real(kind=sp),     intent(in) :: R0 
            real(kind=sp),     intent(in) :: thtc 
            real(kind=sp),     intent(in) :: z0 
            real(kind=sp),     intent(in) :: Rc ! (a+Hc) distance of emmiter from the earth center.
            real(kind=sp),     intent(in) :: nc ! refractive index at emmiter vicinity
            real(kind=sp),     intent(in) :: na 
            real(kind=sp)                 :: alpha
            real(kind=sp),     automatic  :: p1, ctgz0, sctgz0
            real(kind=sp),     automatic  :: sp1, rat, sqr 
            real(kind=sp),     automatic  :: t0, t1, exp1 
            real(kind=sp),     automatic  :: trm1, trm2 
            t0     = delnA*beta*R0*thtc 
            ctgz0  = 1.0_sp/tan(z0)
            sctgz0 = ctgz0*ctgz0 
            p1     = ctgz0/(2.0_sp*sqrt(0.5_sp+sctgz0))
            sp1    = p1*p1
            t1     = exp(beta*R0*sp1)
            trm1   = t0*t1 
            rat    = (Rc*nc)/(R0*na)
            t0     = 1.0_sp+(p1*0.5_sp)*sqrt(sp1+rat-1.0_sp)
            t1     = 0.5_sp*(rat-1.0_sp)
            trm2   = t0+t1
            alpha  = trm1*trm2 
       end function refraction_angle_atmos_wvle5cm_f535_r4

       elemental function refraction_angle_atmos_wvle5cm_f535_r8(delnA,beta,R0,thtc,z0,  &
                                                                 Rc,nc,na) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_atmos_wvle5cm_f535_r8
            !dir$ attributes forceinline :: refraction_angle_atmos_wvle5cm_f535_r8
#endif 
!$omp declare simd(refraction_angle_atmos_wvle5cm_f535_r8)
            real(kind=dp),     intent(in) :: delnA 
            real(kind=dp),     intent(in) :: beta 
            real(kind=dp),     intent(in) :: R0 
            real(kind=dp),     intent(in) :: thtc 
            real(kind=dp),     intent(in) :: z0 
            real(kind=dp),     intent(in) :: Rc ! (a+Hc) distance of emmiter from the earth center.
            real(kind=dp),     intent(in) :: nc ! refractive index at emmiter vicinity
            real(kind=dp),     intent(in) :: na 
            real(kind=dp)                 :: alpha 
            real(kind=dp),     automatic  :: p1, ctgz0, sctgz0
            real(kind=dp),     automatic  :: sp1, rat, sqr 
            real(kind=dp),     automatic  :: t0, t1, exp1 
            real(kind=dp),     automatic  :: trm1, trm2 
            t0     = delnA*beta*R0*thtc 
            ctgz0  = 1.0_dp/tan(z0)
            sctgz0 = ctgz0*ctgz0 
            p1     = ctgz0/(2.0_dp*sqrt(0.5_dp+sctgz0))
            sp1    = p1*p1
            t1     = exp(beta*R0*sp1)
            trm1   = t0*t1 
            rat    = (Rc*nc)/(R0*na)
            t0     = 1.0_dp+(p1*0.5_dp)*sqrt(sp1+rat-1.0_dp)
            t1     = 0.5_dp*(rat-1.0_dp)
            trm2   = t0+t1
            alpha  = trm1*trm2 
       end function refraction_angle_atmos_wvle5cm_f535_r8

       !При одинаковых высотах излучателя и приемника и
       !ри z0 = 90°
       !!Formula: 5.36, page: 101
       elemental function refraction_angle_atmos_wvle5cm_z0eq90_f536_r4(delnA,beta,R0,thtc) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_atmos_wvle5cm_z0eq90_f536_r4
            !dir$ attributes forceinline :: refraction_angle_atmos_wvle5cm_z0eq90_f536_r4
#endif 
!$omp declare simd(refraction_angle_atmos_wvle5cm_z0eq90_f536_r4) 
            real(kind=sp),     intent(in) :: delnA 
            real(kind=sp),     intent(in) :: beta 
            real(kind=sp),     intent(in) :: R0 
            real(kind=sp),     intent(in) :: thtc 
            real(kind=sp)                 :: alpha 
            alpha  = delnA*beta*R0*thtc 
       end function refraction_angle_atmos_wvle5cm_z0eq90_f536_r4

       elemental function refraction_angle_atmos_wvle5cm_z0eq90_f536_r8(delnA,beta,R0,thtc) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_atmos_wvle5cm_z0eq90_f536_r8
            !dir$ attributes forceinline :: refraction_angle_atmos_wvle5cm_z0eq90_f536_r8
#endif 
!$omp declare simd(refraction_angle_atmos_wvle5cm_z0eq90_f536_r8) 
            real(kind=dp),     intent(in) :: delnA 
            real(kind=dp),     intent(in) :: beta 
            real(kind=dp),     intent(in) :: R0 
            real(kind=dp),     intent(in) :: thtc 
            real(kind=dp)                 :: alpha 
            alpha  = delnA*beta*R0*thtc 
       end function refraction_angle_atmos_wvle5cm_z0eq90_f536_r8

       !Высоты излучателя и лриемнйка значительно 
       !отличаются друг от друга, т. е. выполняется условие
       !u2 > 1 и и u1 > 1.
       !Formula: 5.37, page: 101
       elemental function refraction_angle_atmos_wvle5cm_f537_r4(delnA,z0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_atmos_wvle5cm_f537_r4
            !dir$ attributes forceinline :: refraction_angle_atmos_wvle5cm_f537_r4
#endif 
!$omp declare simd(refraction_angle_atmos_wvle5cm_f537_r4) 
            real(kind=sp),     intent(in) :: delnA 
            real(kind=sp),     intent(in) :: z0 
            real(kind=sp)                 :: alpha 
            real(kind=sp),     automatic  :: tgz0 
            tgz0  = tan(z0)
            alpha = delNa*tgz0 
       end function refraction_angle_atmos_wvle5cm_f537_r4

       elemental function refraction_angle_atmos_wvle5cm_f537_r8(delnA,z0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_atmos_wvle5cm_f537_r8
            !dir$ attributes forceinline :: refraction_angle_atmos_wvle5cm_f537_r8
#endif 
!$omp declare simd(refraction_angle_atmos_wvle5cm_f537_r8) 
            real(kind=dp),     intent(in) :: delnA 
            real(kind=dp),     intent(in) :: z0 
            real(kind=dp)                 :: alpha 
            real(kind=dp),     automatic  :: tgz0 
            tgz0  = tan(z0)
            alpha = delNa*tgz0 
       end function refraction_angle_atmos_wvle5cm_f537_r8

       !Излучатель находится за пределами атмосферы, а
       !приемник — вблизи поверхности Земли, причем 
       !видимый'зенитный угол близок к 90° (астрономическая 
       !рефракция вблизи горизонта).
       !В этом случае имеют месте) соотношения u2 > 1, u1 < 1
       elemental function refraction_angle_astronomical_wvle5cm_f538_r4(delnA,beta,R0,z0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_astronomical_wvle5cm_f538_r4
            !dir$ attributes forceinline :: refraction_angle_astronomical_wvle5cm_f538_r4
#endif 
!$omp declare simd(refraction_angle_astronomical_wvle5cm_f538_r4) 
            real(kind=sp),     intent(in) :: delnA 
            real(kind=sp),     intent(in) :: beta
            real(kind=sp),     intent(in) :: R0 
            real(kind=sp),     intent(in) :: z0 
            real(kind=sp)                 :: alpha 
            real(kind=sp),     parameter  :: C314159265358979323846264338328 = & 
                                                3.14159265358979323846264338328_sp 
            real(kind=sp),     automatic  :: p1, q1 
            real(kind=sp),     automatic  :: ctgz0, sctgz0 
            real(kind=sp),     automatic  :: btR0, sqr 
            real(kind=sp),     automatic  :: trm1, trm2 
            real(kind=sp),     automatic  :: t0, t1 
            ctgz0  = 1.0_sp/tan(z0)
            btR0   = beta*R0 
            sctgz0 = ctgz0 
            p1     = ctgz0/(2.0_sp*sqrt(0.5_sp+sctgz0))
            q1     = sqrt(0.5_sp*sctgz0)
            sqr    = sqrt(C314159265358979323846264338328/btR0)
            trm1   = delnA*(btR0/q1)
            t0     = 0.5_sp*sqr 
            t1     = (1.0_sp+(1.0_sp/(2.0_sp*btR0)))-p1 
            trm2   = t0*t1 
            alpha  = trm1*trm2 
       end function refraction_angle_astronomical_wvle5cm_f538_r4

       elemental function refraction_angle_astronomical_wvle5cm_f538_r8(delnA,beta,R0,z0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_astronomical_wvle5cm_f538_r8
            !dir$ attributes forceinline :: refraction_angle_astronomical_wvle5cm_f538_r8
#endif 
!$omp declare simd(refraction_angle_astronomical_wvle5cm_f538_r8) 
            real(kind=dp),     intent(in) :: delnA 
            real(kind=dp),     intent(in) :: beta
            real(kind=dp),     intent(in) :: R0 
            real(kind=dp),     intent(in) :: z0 
            real(kind=dp)                 :: alpha 
            real(kind=dp),     parameter  :: C314159265358979323846264338328 = & 
                                                3.14159265358979323846264338328_dp 
            real(kind=dp),     automatic  :: p1, q1 
            real(kind=dp),     automatic  :: ctgz0, sctgz0 
            real(kind=dp),     automatic  :: btR0, sqr 
            real(kind=dp),     automatic  :: trm1, trm2 
            real(kind=dp),     automatic  :: t0, t1 
            ctgz0  = 1.0_dp/tan(z0)
            btR0   = beta*R0 
            sctgz0 = ctgz0 
            p1     = ctgz0/(2.0_dp*sqrt(0.5_dp+sctgz0))
            q1     = sqrt(0.5_dp*sctgz0)
            sqr    = sqrt(C314159265358979323846264338328/btR0)
            trm1   = delnA*(btR0/q1)
            t0     = 0.5_dp*sqr 
            t1     = (1.0_dp+(1.0_dp/(2.0_dp*btR0)))-p1 
            trm2   = t0*t1 
            alpha  = trm1*trm2 
       end function refraction_angle_astronomical_wvle5cm_f538_r8

       !Для астрономической рефракции на горизонте z0 == 90 deg.
       elemental function refraction_angle_astronomical_wvle5cm_z0eq90_f539_r4(delnA,beta,R0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_astronomical_wvle5cm_z0eq90_f539_r4
            !dir$ attributes forceinline :: refraction_angle_astronomical_wvle5cm_z0eq90_f539_r4
#endif 
!$omp declare simd(refraction_angle_astronomical_wvle5cm_z0eq90_f539_r4) 
            real(kind=sp),     intent(in) :: delnA 
            real(kind=sp),     intent(in) :: beta
            real(kind=sp),     intent(in) :: R0 
            real(kind=sp)                 :: alpha 
            real(kind=sp),     parameter  :: C314159265358979323846264338328 = & 
                                                3.14159265358979323846264338328_sp 
            real(kind=sp),     automatic  :: pibtR0, sqr 
            real(kind=sp),     automatic  :: inv 
            pibtR0  = C314159265358979323846264338328*beta*R0 
            sqr     = sqrt(0.5_sp*pibtR0)
            inv     = 1.0_sp/(2.0_sp*beta*R0)
            alpha   = delnA*sqr*(1.0_sp+inv)
       end function refraction_angle_astronomical_wvle5cm_z0eq90_f539_r4

       elemental function refraction_angle_astronomical_wvle5cm_z0eq90_f539_r8(delnA,beta,R0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_astronomical_wvle5cm_z0eq90_f539_r8
            !dir$ attributes forceinline :: refraction_angle_astronomical_wvle5cm_z0eq90_f539_r8
#endif 
!$omp declare simd(refraction_angle_astronomical_wvle5cm_z0eq90_f539_r8) 
            real(kind=dp),     intent(in) :: delnA 
            real(kind=dp),     intent(in) :: beta
            real(kind=dp),     intent(in) :: R0 
            real(kind=dp)                 :: alpha 
            real(kind=dp),     parameter  :: C314159265358979323846264338328 = & 
                                                3.14159265358979323846264338328_dp 
            real(kind=dp),     automatic  :: pibtR0, sqr 
            real(kind=dp),     automatic  :: inv 
            pibtR0  = C314159265358979323846264338328*beta*R0 
            sqr     = sqrt(0.5_dp*pibtR0)
            inv     = 1.0_dp/(2.0_dp*beta*R0)
            alpha   = delnA*sqr*(1.0_dp+inv)
       end function refraction_angle_astronomical_wvle5cm_z0eq90_f539_r8

       ! Рефракция радиоволн (5 см < X < 3 м)
       ! в земной атмосфере при различных высотах
       ! излучателя и приемника

       ! Приемник помещен в нейтросфере в точке А
       ! на высоте Н0 над поверхностью Земли, а 
       ! излучатель—'в нижней ионосфере, в точке С на высоте
       ! Нс над земной поверхностью.

       ! Formula: 5.41, page: 103
       elemental function refractive_idx_lo_ionosphere_wv5cm3m_f541_r4(delnA,beta,h) result(n) 
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractive_idx_lo_ionosphere_wv5cm3m_f541_r4
            !dir$ attributes forceinline :: refractive_idx_lo_ionosphere_wv5cm3m_f541_r4
#endif 
!$omp declare simd(refractive_idx_lo_ionosphere_wv5cm3m_f541_r4) 
            real(kind=sp),     intent(in) :: delnA ! refractive index at point `A`
            real(kind=sp),     intent(in) :: beta
            real(kind=sp),     intent(in) :: h ! 0<=h<=H10 (H10==H1-H0)
            real(kind=sp)                 :: n 
            real(kind=sp),     automatic  :: bth, exp1
            real(kind=sp),     automatic  :: t0 
            t0   = 1.0_sp+delnA 
            bth  = -beta*h 
            exp1 = exp(bth)
            n    = t0*exp1 
       end function refractive_idx_lo_ionosphere_wv5cm3m_f541_r4

       elemental function refractive_idx_lo_ionosphere_wv5cm3m_f541_r8(delnA,beta,h) result(n) 
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractive_idx_lo_ionosphere_wv5cm3m_f541_r8
            !dir$ attributes forceinline :: refractive_idx_lo_ionosphere_wv5cm3m_f541_r8
#endif 
!$omp declare simd(refractive_idx_lo_ionosphere_wv5cm3m_f541_r8) 
            real(kind=dp),     intent(in) :: delnA ! refractive index at point `A`
            real(kind=dp),     intent(in) :: beta
            real(kind=dp),     intent(in) :: h ! 0<=h<=H10 (H10==H1-H0)
            real(kind=dp)                 :: n 
            real(kind=dp),     automatic  :: bth, exp1
            real(kind=dp),     automatic  :: t0 
            t0   = 1.0_dp+delnA 
            bth  = -beta*h 
            exp1 = exp(bth)
            n    = t0*exp1 
       end function refractive_idx_lo_ionosphere_wv5cm3m_f541_r8

       ! Formula 5.42, page: 103
       elemental function refractive_idx_lo_ionosphere_wv5cm3m_f542_r4(fc,Nmf,h,H10,H20) result(n) 
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractive_idx_lo_ionosphere_wv5cm3m_f542_r4
            !dir$ attributes forceinline :: refractive_idx_lo_ionosphere_wv5cm3m_f542_r4
#endif 
!$omp declare simd(refractive_idx_lo_ionosphere_wv5cm3m_f542_r4) 
            real(kind=sp),    intent(in) :: fc 
            real(kind=sp),    intent(in) :: Nmf 
            real(kind=sp),    intent(in) :: h     !H10<=h<=H20
            real(kind=sp),    intent(in) :: H10 
            real(kind=sp),    intent(in) :: H20 
            real(kind=sp)                :: n 
            real(kind=sp),    automatic  :: delnM, hH10 
            real(kind=sp),    automatic  :: H20H10, rat1 
            real(kind=sp),    automatic  :: rat2,   trm1 
            real(kind=sp),    automatic  :: trm2 
            hH10  = h-H10 
            delnM = compute_delnM_f414_r4(fc,Nmf)
            H20H10= H20-H10 
            rat1  = hH10/H20H10 
            trm1  = 1.0_sp-delnM 
            rat2  = (hH10*hH10)/(H20H10*H20H10)
            trm2  = 2.0_sp*(rat1-rat2)
            n     = trm1*trm2 
       end function refractive_idx_lo_ionosphere_wv5cm3m_f542_r4

       elemental function refractive_idx_lo_ionosphere_wv5cm3m_f542_r8(fc,Nmf,h,H10,H20) result(n) 
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractive_idx_lo_ionosphere_wv5cm3m_f542_r8
            !dir$ attributes forceinline :: refractive_idx_lo_ionosphere_wv5cm3m_f542_r8
#endif 
!$omp declare simd(refractive_idx_lo_ionosphere_wv5cm3m_f542_r8) 
            real(kind=dp),    intent(in) :: fc 
            real(kind=dp),    intent(in) :: Nmf 
            real(kind=dp),    intent(in) :: h     !H10<=h<=H20
            real(kind=dp),    intent(in) :: H10 
            real(kind=dp),    intent(in) :: H20 
            real(kind=dp)                :: n 
            real(kind=dp),    automatic  :: delnM, hH10 
            real(kind=dp),    automatic  :: H20H10, rat1 
            real(kind=dp),    automatic  :: rat2,   trm1 
            real(kind=dp),    automatic  :: trm2 
            hH10  = h-H10 
            delnM = compute_delnM_f414_r8(fc,Nmf)
            H20H10= H20-H10 
            rat1  = hH10/H20H10 
            trm1  = 1.0_dp-delnM 
            rat2  = (hH10*hH10)/(H20H10*H20H10)
            trm2  = 2.0_dp*(rat1-rat2)
            n     = trm1*trm2 
       end function refractive_idx_lo_ionosphere_wv5cm3m_f542_r8

       ! угол полной атмосферной
       ! рефракции а определяется соотношением
       ! Formula: 5.49, page: 105 
       elemental function analytic_sol_L11_whole_atmosphere_f549_r4(beta,R0,delnA,z0,H10) result(L11)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L11_whole_atmosphere_f549_r4
            !dir$ attributes forceinline :: analytic_sol_L11_whole_atmosphere_f549_r4
#endif 
!$omp declare simd(analytic_sol_L11_whole_atmosphere_f549_r4) 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: H10 
            real(kind=sp)                :: L11 
            real(kind=sp),    automatic  :: btH10, exp1 
            real(kind=sp),    automatic  :: exp2,  siz0 
            real(kind=sp),    automatic  :: cosz0, stgz0 
            real(kind=sp),    automatic  :: trm1, trm2 
            real(kind=sp),    automatic  :: t0,   t1 
            t0    = beta*R0*delnA*delnA 
            btH10 = beta*H10 
            siz0  = sin(z0) 
            cosz0 = cos(z0) 
            t1    = siz0*cosz0 
            rat1  = t0/t1 
            exp1  = exp(-btH10) 
            t0    = tan(z0)
            stgz0 = t0*t0 
            exp2  = exp(-2.0_sp*btH10)
            t0    = H10/R0 
            t1    = sqrt(1.0_sp+2.0_sp*stgz0*t0) 
            rat2  = (exp1-exp2)/t1 
            L11   = -rat1*rat2 
       end function analytic_sol_L11_whole_atmosphere_f549_r4

       elemental function analytic_sol_L11_whole_atmosphere_f549_r8(beta,R0,delnA,z0,H10) result(L11)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L11_whole_atmosphere_f549_r8
            !dir$ attributes forceinline :: analytic_sol_L11_whole_atmosphere_f549_r8
#endif 
!$omp declare simd(analytic_sol_L11_whole_atmosphere_f549_r8) 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: H10 
            real(kind=dp)                :: L11 
            real(kind=dp),    automatic  :: btH10, exp1 
            real(kind=dp),    automatic  :: exp2,  siz0 
            real(kind=dp),    automatic  :: cosz0, stgz0 
            real(kind=dp),    automatic  :: trm1, trm2 
            real(kind=dp),    automatic  :: t0,   t1 
            t0    = beta*R0*delnA*delnA 
            btH10 = beta*H10 
            siz0  = sin(z0) 
            cosz0 = cos(z0) 
            t1    = siz0*cosz0 
            rat1  = t0/t1 
            exp1  = exp(-btH10) 
            t0    = tan(z0)
            stgz0 = t0*t0 
            exp2  = exp(-2.0_dp*btH10)
            t0    = H10/R0 
            t1    = sqrt(1.0_dp+2.0_dp*stgz0*t0) 
            rat2  = (exp1-exp2)/t1 
            L11   = -rat1*rat2 
       end function analytic_sol_L11_whole_atmosphere_f549_r8

        ! угол полной атмосферной
       ! рефракции а определяется соотношением
       ! Formula: 5.50, page: 105 
       elemental function analytic_sol_L12_whole_atmosphere_f550_r4(beta,R0,delnA,z0,H10) result(L12)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L12_whole_atmosphere_f550_r4
            !dir$ attributes forceinline :: analytic_sol_L12_whole_atmosphere_f550_r4
#endif 
!$omp declare simd(analytic_sol_L12_whole_atmosphere_f550_r4) 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: H10 
            real(kind=sp)                :: L12 
            real(kind=sp),    parameter  :: C157079632679489661923132169164 = & 
                                              1.57079632679489661923132169164_sp 
            real(kind=sp),    automatic  :: btR0,  tgz0 
            real(kind=sp),    automatic  :: stgz0, sctgz0 
            real(kind=sp),    automatic  :: exp1,  rat1 
            real(kind=sp),    automatic  :: prob1, prob2 
            real(kind=sp),    automatic  :: trm1, trm2 
            real(kind=sp),    automatic  :: btR0scz0, t0 
            real(kind=sp),    automatic  :: t1 
            tgz0     = tan(z0)
            btR0     = beta*R0 
            stgz0    = tgz0*tgz0 
            t0       = 1.0_sp/tgz0 
            sctgz0   = t0*t0 
            btR0scz0 = btR0*sctgz0 
            exp1     = exp(btR0/(sctgz0+sctgz0))
            t1       = sqrt(btR0/tgz0)
            trm1     = delnA*t1*exp1*C157079632679489661923132169164 
            t0       = sqrt(btR0scz0+2.0_sp*beta*H10)
            prob1    = prob_integral_r4(t0)
            t1       = sqrt(btR0scz0)
            prob2    = prob_integral_r4(t1)
            trm2     = prob1-prob2 
            L12      = trm1*trm2 
       end function analytic_sol_L12_whole_atmosphere_f550_r4

       elemental function analytic_sol_L12_whole_atmosphere_f550_r8(beta,R0,delnA,z0,H10) result(L12)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L12_whole_atmosphere_f550_r8
            !dir$ attributes forceinline :: analytic_sol_L12_whole_atmosphere_f550_r8
#endif 
!$omp declare simd(analytic_sol_L12_whole_atmosphere_f550_r8) 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: H10 
            real(kind=dp)                :: L12 
            real(kind=dp),    parameter  :: C157079632679489661923132169164 = & 
                                              1.57079632679489661923132169164_dp 
            real(kind=dp),    automatic  :: btR0,  tgz0 
            real(kind=dp),    automatic  :: stgz0, sctgz0 
            real(kind=dp),    automatic  :: exp1,  rat1 
            real(kind=dp),    automatic  :: prob1, prob2 
            real(kind=dp),    automatic  :: trm1, trm2 
            real(kind=dp),    automatic  :: btR0scz0, t0 
            real(kind=dp),    automatic  :: t1 
            tgz0     = tan(z0)
            btR0     = beta*R0 
            stgz0    = tgz0*tgz0 
            t0       = 1.0_dp/tgz0 
            sctgz0   = t0*t0 
            btR0scz0 = btR0*sctgz0 
            exp1     = exp(btR0/(sctgz0+sctgz0))
            t1       = sqrt(btR0/tgz0)
            trm1     = delnA*t1*exp1*C157079632679489661923132169164 
            t0       = sqrt(btR0scz0+2.0_dp*beta*H10)
            prob1    = prob_integral_r8(t0)
            t1       = sqrt(btR0scz0)
            prob2    = prob_integral_r8(t1)
            trm2     = prob1-prob2 
            L12      = trm1*trm2 
       end function analytic_sol_L12_whole_atmosphere_f550_r8

       ! угол полной атмосферной
       ! рефракции а определяется соотношением
       ! Formula: 5.51, page: 105 
       elemental function analytic_sol_L13_whole_atmosphere_f551_r4(beta,R0,delnA,z0,H10) result(L13)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L13_whole_atmosphere_f551_r4
            !dir$ attributes forceinline :: analytic_sol_L13_whole_atmosphere_f551_r4
#endif 
!$omp declare simd(analytic_sol_L13_whole_atmosphere_f551_r4) 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: H10 
            real(kind=sp)                :: L13 
            real(kind=sp),    parameter  :: C157079632679489661923132169164 = & 
                                              1.57079632679489661923132169164_sp 
            real(kind=sp),    automatic  :: btR0,  tgz0 
            real(kind=sp),    automatic  :: stgz0, sctgz0 
            real(kind=sp),    automatic  :: exp1,  rat1 
            real(kind=sp),    automatic  :: prob1, prob2 
            real(kind=sp),    automatic  :: trm1, trm2 
            real(kind=sp),    automatic  :: btR0scz0, t0 
            real(kind=sp),    automatic  :: t1 
            tgz0     = tan(z0)
            btR0     = beta*R0 
            stgz0    = tgz0*tgz0 
            t0       = 1.0_sp/tgz0 
            sctgz0   = t0*t0 
            btR0scz0 = btR0*sctgz0 
            exp1     = exp(btR0/sctgz0)
            t1       = sqrt((2.0_sp*btR0)/tgz0)
            trm1     = delnA*t1*exp1*C157079632679489661923132169164 
            t0       = sqrt(2.0_sp*btR0scz0+4.0_sp*beta*H10)
            prob1    = prob_integral_r4(t0)
            t1       = sqrt(2.0_sp*btR0scz0)
            prob2    = prob_integral_r4(t1)
            trm2     = prob1-prob2 
            L12      = trm1*trm2 
       end function analytic_sol_L13_whole_atmosphere_f551_r4

       elemental function analytic_sol_L13_whole_atmosphere_f551_r8(beta,R0,delnA,z0,H10) result(L13)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L13_whole_atmosphere_f551_r8
            !dir$ attributes forceinline :: analytic_sol_L13_whole_atmosphere_f551_r8
#endif 
!$omp declare simd(analytic_sol_L13_whole_atmosphere_f551_r8) 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: H10 
            real(kind=dp)                :: L13 
            real(kind=dp),    parameter  :: C157079632679489661923132169164 = & 
                                              1.57079632679489661923132169164_dp 
            real(kind=dp),    automatic  :: btR0,  tgz0 
            real(kind=dp),    automatic  :: stgz0, sctgz0 
            real(kind=dp),    automatic  :: exp1,  rat1 
            real(kind=dp),    automatic  :: prob1, prob2 
            real(kind=dp),    automatic  :: trm1, trm2 
            real(kind=dp),    automatic  :: btR0scz0, t0 
            real(kind=dp),    automatic  :: t1 
            tgz0     = tan(z0)
            btR0     = beta*R0 
            stgz0    = tgz0*tgz0 
            t0       = 1.0_dp/tgz0 
            sctgz0   = t0*t0 
            btR0scz0 = btR0*sctgz0 
            exp1     = exp(btR0/sctgz0)
            t1       = sqrt((2.0_dp*btR0)/tgz0)
            trm1     = delnA*t1*exp1*C157079632679489661923132169164 
            t0       = sqrt(2.0_dp*btR0scz0+4.0_dp*beta*H10)
            prob1    = prob_integral_r8(t0)
            t1       = sqrt(2.0_dp*btR0scz0)
            prob2    = prob_integral_r8(t1)
            trm2     = prob1-prob2 
            L12      = trm1*trm2 
       end function analytic_sol_L13_whole_atmosphere_f551_r8

       !Formula: 5.48, page: 104
       elemental function analytic_sol_L1_whole_atmosphere_f548_r4(beta,R0,delnA,z0,H10) result(L1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_whole_atmosphere_f548_r4
            !dir$ attributes forceinline :: analytic_sol_L1_whole_atmosphere_f548_r4
#endif 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: H10 
            real(kind=sp)                :: L1
            real(kind=sp),    automatic  :: ctgz0, scosz0 
            real(kind=sp),    automatic  :: btR0dln,rat 
            real(kind=sp),    automatic  :: trm1, trm2 
            real(kind=sp),    automatic  :: L11, L12
            real(kind=sp),    automatic  :: t0, L13 
            btR0dln  = beta*R0*delnA 
            L11      = analytic_sol_L11_whole_atmosphere_f549_r4(beta,R0,delnA,z0,H10)
            ctgz0    = 1.0_sp/tan(z0)
            t0       = cos(z0)
            scosz0   = t0*t0 
            rat      = ctgz0/scosz0 
            L12      = analytic_sol_L12_whole_atmosphere_f550_r4(beta,R0,delnA,z0,H10)
            L13      = analytic_sol_L13_whole_atmosphere_f551_r4(beta,R0,delnA,z0,H10)
            trm1     = L11+rat*L12 
            trm2     = btR0dln*rat*(L13-L12)
            L1       = trm1+trm2 
       end function analytic_sol_L1_whole_atmosphere_f548_r4

       elemental function analytic_sol_L1_whole_atmosphere_f548_r8(beta,R0,delnA,z0,H10) result(L1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L1_whole_atmosphere_f548_r8
            !dir$ attributes forceinline :: analytic_sol_L1_whole_atmosphere_f548_r8
#endif 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: H10 
            real(kind=dp)                :: L1
            real(kind=dp),    automatic  :: ctgz0, scosz0 
            real(kind=dp),    automatic  :: btR0dln,rat 
            real(kind=dp),    automatic  :: trm1, trm2 
            real(kind=dp),    automatic  :: L11, L12
            real(kind=dp),    automatic  :: t0, L13 
            btR0dln  = beta*R0*delnA 
            L11      = analytic_sol_L11_whole_atmosphere_f549_r8(beta,R0,delnA,z0,H10)
            ctgz0    = 1.0_dp/tan(z0)
            t0       = cos(z0)
            scosz0   = t0*t0 
            rat      = ctgz0/scosz0 
            L12      = analytic_sol_L12_whole_atmosphere_f550_r8(beta,R0,delnA,z0,H10)
            L13      = analytic_sol_L13_whole_atmosphere_f551_r8(beta,R0,delnA,z0,H10)
            trm1     = L11+rat*L12 
            trm2     = btR0dln*rat*(L13-L12)
            L1       = trm1+trm2 
       end function analytic_sol_L1_whole_atmosphere_f548_r8

       ! Formula: 5.53, page: 105
       elemental function analytic_sol_L21_whole_atmosphere_f553_r4(fc,Nmf,beta,R0,delnA,        &
                                                                    z0,H0,H1,H2,Hc) result(L21)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L21_whole_atmosphere_f553_r4
            !dir$ attributes forceinline :: analytic_sol_L21_whole_atmosphere_f553_r4
#endif 
!$omp declare simd(analytic_sol_L21_whole_atmosphere_f553_r4) 
            real(kind=sp),    intent(in) :: fc
            real(kind=sp),    intent(in) :: Nmf 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: H0 
            real(kind=sp),    intent(in) :: H1 
            real(kind=sp),    intent(in) :: H2 
            real(kind=sp),    intent(in) :: Hc 
            real(kind=sp)                :: L21 
            real(kind=sp),    automatic  :: delnM, ctgz0 
            real(kind=sp),    automatic  :: ssinz0,H10 
            real(kind=sp),    automatic  :: H20,Hc0 
            real(kind=sp),    automatic  :: stgz0, Hc0R0
            real(kind=sp),    automatic  :: sqr1, sqr2 
            real(kind=sp),    automatic  :: t0, t1 
            real(kind=sp),    automatic  :: trm1, trm2 
            real(kind=sp),    automatic  :: trm3, sqr3 
            real(kind=sp),    automatic  ::  t2
            Hc0   = Hc-H0 
            H20   = H2-H0 
            H10   = H1-H0 
            t0    = sin(z0)
            ssinz0= t0*t0 
            t0    = tan(z0)
            ctgz0 = 1.0_sp/t0 
            stgz0 = t0*t0 
            delnNm= compute_delnM_f414_r4(fc,Nmf)
            t0    = 1.0_sp+2.0_sp*stgz0*(H10/R0)
            sqr1  = sqrt(t0)
            t1    = 1.0_sp+2.0_sp*stgz0*(Hc0/R0)
            sqr2  = sqrt(t1)
            sqr3  = 1.0_sp-stgz0*(Hc0/R0)
            t1    = R0/((H2-H1)*(H2-H1))
            t2    = ctgz0/ssinz0 
            trm1  = (delnM+delnM)*t1*t2
            t3    = R0/(3.0_sp*stgz0)
            trm2  = H20*(sqr2-sqr1)+t3 
            trm3  = sqr3*sqr2-sqr3*sqr2 
            L21   = trm1*trm2*trm3 
       end function analytic_sol_L21_whole_atmosphere_f553_r4

        elemental function analytic_sol_L21_whole_atmosphere_f553_r8(fc,Nmf,beta,R0,delnA,        &
                                                                    z0,H0,H1,H2,Hc) result(L21)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L21_whole_atmosphere_f553_r8
            !dir$ attributes forceinline :: analytic_sol_L21_whole_atmosphere_f553_r8
#endif 
!$omp declare simd(analytic_sol_L21_whole_atmosphere_f553_r8) 
            real(kind=dp),    intent(in) :: fc
            real(kind=dp),    intent(in) :: Nmf 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: H0 
            real(kind=dp),    intent(in) :: H1 
            real(kind=dp),    intent(in) :: H2 
            real(kind=dp),    intent(in) :: Hc 
            real(kind=dp)                :: L21 
            real(kind=dp),    automatic  :: delnM, ctgz0 
            real(kind=dp),    automatic  :: ssinz0,H10 
            real(kind=dp),    automatic  :: H20,Hc0 
            real(kind=dp),    automatic  :: stgz0, Hc0R0
            real(kind=dp),    automatic  :: sqr1, sqr2 
            real(kind=dp),    automatic  :: t0, t1 
            real(kind=dp),    automatic  :: trm1, trm2 
            real(kind=dp),    automatic  :: trm3, sqr3 
            real(kind=dp),    automatic  ::  t2
            Hc0   = Hc-H0 
            H20   = H2-H0 
            H10   = H1-H0 
            t0    = sin(z0)
            ssinz0= t0*t0 
            t0    = tan(z0)
            ctgz0 = 1.0_dp/t0 
            stgz0 = t0*t0 
            delnNm= compute_delnM_f414_r8(fc,Nmf)
            t0    = 1.0_dp+2.0_dp*stgz0*(H10/R0)
            sqr1  = sqrt(t0)
            t1    = 1.0_dp+2.0_dp*stgz0*(Hc0/R0)
            sqr2  = sqrt(t1)
            sqr3  = 1.0_dp-stgz0*(Hc0/R0)
            t1    = R0/((H2-H1)*(H2-H1))
            t2    = ctgz0/ssinz0 
            trm1  = (delnM+delnM)*t1*t2
            t3    = R0/(3.0_dp*stgz0)
            trm2  = H20*(sqr2-sqr1)+t3 
            trm3  = sqr3*sqr2-sqr3*sqr2 
            L21   = trm1*trm2*trm3 
       end function 
       
       ! Formula: 5.53a, page: 105 
       elemental function analytic_sol_L22_whole_atmosphere_f553a_r4(fc,Nmf,beta,R0,delnA,        &
                                                                    z0,H0,H1,H2,Hc) result(L22)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L22_whole_atmosphere_f553a_r4
            !dir$ attributes forceinline :: analytic_sol_L22_whole_atmosphere_f553a_r4
#endif 
!$omp declare simd(analytic_sol_L22_whole_atmosphere_f553a_r4) 
            real(kind=sp),    intent(in) :: fc
            real(kind=sp),    intent(in) :: Nmf 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: H0 
            real(kind=sp),    intent(in) :: H1 
            real(kind=sp),    intent(in) :: H2 
            real(kind=sp),    intent(in) :: Hc 
            real(kind=sp)                :: L22 
            real(kind=sp),    automatic  :: p2,q2
            real(kind=sp),    automatic  :: b, m1 
            real(kind=sp),    automatic  :: m2, g 
            real(kind=sp),    automatic  :: delnM, tgz0 
            real(kind=sp),    automatic  :: scosz0, H10 
            real(kind=sp),    automatic  :: H20,Hc0 
            real(kind=sp),    automatic  :: H2H1p4, sqr1 
            real(kind=sp),    automatic  :: sqr2, t0 
            real(kind=sp),    automatic  :: t1, t2 
            real(kind=sp),    automatic  :: t3, trm1 
            real(kind=sp),    automatic  :: trm2, trm3 
            real(kind=sp),    automatic  :: m1p3, m1p2 
            real(kind=sp),    automatic  :: m2p3, m1p2 
            real(kind=sp),    automatic  :: bp3, stgz0 
            real(kind=sp),    automatic  :: bp2, c0 
            real(kind=sp),    automatic  :: c1  
            Hc0   = Hc-H0 
            delnM = compute_delnM_f414_r4(fc,Nmf)
            H10   = H1-H0 
            H20   = H2-H0 
            tgz0  = tan(z0)
            m1    = 1.0_sp+beta*Hc0 
            m2    = 1.0_sp+beta*H10 
            stgz0 = tgz0*tgz0 
            t0    = cos(z0)
            scosz0= t0*t0 
            b     = 2.0_sp*(stgz0/R0)
            t1    = (H2-H1)*(H2-H1)
            t0    = 1.0_sp+(delnA/delnM)
            g     = H20*H20-t1*t0 
            m1p3  = m1*m1*m1 
            m1p2  = m1*m1 
            bp3   = b*b*b 
            m2p3  = m2*m2*m2 
            m2p2  = m2*m2 
            t0    = m1p3*0.2_sp-m1p2+3.0_sp*m1+1.0_sp 
            bp2   = b*b 
            c0    = bp2*(2.0_sp*H20*H20+g)
            c1    = bp3*H20*g 
            t1    = 3.0_sp*H20*b*(m1p2*0.33333333333_sp-2.0_sp*m1-1.0_sp)
            t2    = c0*(m1+1.0_sp)+c1 
            p2    = t0-t1+t2 
            t0    = m2p3*0.2_sp-m2p2+3.0_sp*m2+1.0_sp
            t1    = 3.0_sp*H20*b*(m2p2*0.33333333333_sp-2.0_sp*m2-1.0_sp)
            t2    = c0*(m2+1.0_sp)+c1 
            q2    = t0-t1+t2 
            t0    = (H2-H1)*(H2-H1)
            t1    = t0*t0 
            trm1  = 4.0_sp/(t1*bp2*bp2)
            sqr1  = p2/sqrt(1.0_sp+beta*Hc0)
            trm2  = tgz0/scosz0
            sqr2  = q2/sqrt(1.0_sp+beta*H10)
            trm3  = sqr1-sqr2 
            L22   = trm1*trm2*trm3 
       end function analytic_sol_L22_whole_atmosphere_f553a_r4

       elemental function analytic_sol_L22_whole_atmosphere_f553a_r8(fc,Nmf,beta,R0,delnA,        &
                                                                    z0,H0,H1,H2,Hc) result(L22)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L22_whole_atmosphere_f553a_r8
            !dir$ attributes forceinline :: analytic_sol_L22_whole_atmosphere_f553a_r8
#endif 
!$omp declare simd(analytic_sol_L22_whole_atmosphere_f553a_r8) 
            real(kind=dp),    intent(in) :: fc
            real(kind=dp),    intent(in) :: Nmf 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: H0 
            real(kind=dp),    intent(in) :: H1 
            real(kind=dp),    intent(in) :: H2 
            real(kind=dp),    intent(in) :: Hc 
            real(kind=dp)                :: L22 
            real(kind=dp),    automatic  :: p2,q2
            real(kind=dp),    automatic  :: b, m1 
            real(kind=dp),    automatic  :: m2, g 
            real(kind=dp),    automatic  :: delnM, tgz0 
            real(kind=dp),    automatic  :: scosz0, H10 
            real(kind=dp),    automatic  :: H20,Hc0 
            real(kind=dp),    automatic  :: H2H1p4, sqr1 
            real(kind=dp),    automatic  :: sqr2, t0 
            real(kind=dp),    automatic  :: t1, t2 
            real(kind=dp),    automatic  :: t3, trm1 
            real(kind=dp),    automatic  :: trm2, trm3 
            real(kind=dp),    automatic  :: m1p3, m1p2 
            real(kind=dp),    automatic  :: m2p3, m1p2 
            real(kind=dp),    automatic  :: bp3, stgz0 
            real(kind=dp),    automatic  :: bp2, c0 
            real(kind=dp),    automatic  :: c1  
            Hc0   = Hc-H0 
            delnM = compute_delnM_f414_r8(fc,Nmf)
            H10   = H1-H0 
            H20   = H2-H0 
            tgz0  = tan(z0)
            m1    = 1.0_dp+beta*Hc0 
            m2    = 1.0_dp+beta*H10 
            stgz0 = tgz0*tgz0 
            t0    = cos(z0)
            scosz0= t0*t0 
            b     = 2.0_dp*(stgz0/R0)
            t1    = (H2-H1)*(H2-H1)
            t0    = 1.0_dp+(delnA/delnM)
            g     = H20*H20-t1*t0 
            m1p3  = m1*m1*m1 
            m1p2  = m1*m1 
            bp3   = b*b*b 
            m2p3  = m2*m2*m2 
            m2p2  = m2*m2 
            t0    = m1p3*0.2_dp-m1p2+3.0_dp*m1+1.0_dp 
            bp2   = b*b 
            c0    = bp2*(2.0_dp*H20*H20+g)
            c1    = bp3*H20*g 
            t1    = 3.0_dp*H20*b*(m1p2*0.33333333333333333333_dp-2.0_sp*m1-1.0_dp)
            t2    = c0*(m1+1.0_dp)+c1 
            p2    = t0-t1+t2 
            t0    = m2p3*0.2_dp-m2p2+3.0_dp*m2+1.0_dp
            t1    = 3.0_dp*H20*b*(m2p2*0.33333333333333333333_dp-2.0_dp*m2-1.0_dp)
            t2    = c0*(m2+1.0_dp)+c1 
            q2    = t0-t1+t2 
            t0    = (H2-H1)*(H2-H1)
            t1    = t0*t0 
            trm1  = 4.0_dp/(t1*bp2*bp2)
            sqr1  = p2/sqrt(1.0_dp+beta*Hc0)
            trm2  = tgz0/scosz0
            sqr2  = q2/sqrt(1.0_dp+beta*H10)
            trm3  = sqr1-sqr2 
            L22   = trm1*trm2*trm3 
       end function analytic_sol_L22_whole_atmosphere_f553a_r8

       !Formula: 5.52, page: 105
       elemental function analytic_sol_L2_whole_atmosphere_f552_r4(fc,Nmf,beta,R0,delnA,        &
                                                                    z0,H0,H1,H2,Hc) result(L2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L2_whole_atmosphere_f552_r4
            !dir$ attributes forceinline :: analytic_sol_L2_whole_atmosphere_f552_r4
#endif 

            real(kind=sp),    intent(in) :: fc
            real(kind=sp),    intent(in) :: Nmf 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: H0 
            real(kind=sp),    intent(in) :: H1 
            real(kind=sp),    intent(in) :: H2 
            real(kind=sp),    intent(in) :: Hc 
            real(kind=sp)                :: L2 
            real(kind=sp),    automatic  :: L21, L22 
            L21   = analytic_sol_L21_whole_atmosphere_f553_r4(fc,Nmf,beta,R0,delnA,        &
                                                                    z0,H0,H1,H2,Hc)
            L22   = analytic_sol_L22_whole_atmosphere_f553a_r4(fc,Nmf,beta,R0,delnA,        &
                                                                    z0,H0,H1,H2,Hc)
            L2    = L21+L22 
       end function analytic_sol_L2_whole_atmosphere_f552_r4

       elemental function analytic_sol_L2_whole_atmosphere_f552_r8(fc,Nmf,beta,R0,delnA,        &
                                                                    z0,H0,H1,H2,Hc) result(L2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L2_whole_atmosphere_f552_r8
            !dir$ attributes forceinline :: analytic_sol_L2_whole_atmosphere_f552_r8
#endif 

            real(kind=dp),    intent(in) :: fc
            real(kind=dp),    intent(in) :: Nmf 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: H0 
            real(kind=dp),    intent(in) :: H1 
            real(kind=dp),    intent(in) :: H2 
            real(kind=dp),    intent(in) :: Hc 
            real(kind=dp)                :: L2 
            real(kind=dp),    automatic  :: L21, L22 
            L21   = analytic_sol_L21_whole_atmosphere_f553_r8(fc,Nmf,beta,R0,delnA,        &
                                                                    z0,H0,H1,H2,Hc)
            L22   = analytic_sol_L22_whole_atmosphere_f553a_r8(fc,Nmf,beta,R0,delnA,        &
                                                                    z0,H0,H1,H2,Hc)
            L2    = L21+L22 
       end function analytic_sol_L2_whole_atmosphere_f552_r8

       ! Formula: 5.43, page: 104
       elemental function refraction_angle_whole_atmos_vw5cm3m_f543_r4(na,nc,fc,Nmf,beta,R0,delnA,        &
                                                              z0,H0,H1,H2,Hc) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_whole_atmos_vw5cm3m_f543_r4
            !dir$ attributes forceinline :: refraction_angle_whole_atmos_vw5cm3m_f543_r4
#endif 
            real(kind=sp),    intent(in) :: na 
            real(kind=sp),    intent(in) :: nc 
            real(kind=sp),    intent(in) :: fc
            real(kind=sp),    intent(in) :: Nmf 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: H0 
            real(kind=sp),    intent(in) :: H1 
            real(kind=sp),    intent(in) :: H2 
            real(kind=sp),    intent(in) :: Hc                     
            real(kind=sp)                :: alpha 
            real(kind=sp),   automatic   :: H10, ctgz0 
            real(kind=sp),   automatic   :: L1, L2
            real(kind=sp),   automatic   :: L  
            H10   = H1-H0 
            ctgz0 = 1.0_sp/tan(z0) 
            L1    = analytic_sol_L1_whole_atmosphere_f548_r4(beta,R0,delnA,z0,H10)
            L2    = analytic_sol_L2_whole_atmosphere_f552_r4(fc,Nmf,beta,R0,delnA,        &
                                                                    z0,H0,H1,H2,Hc)
            L     = L1+L2 
            alpha = -log(na/nc)*ctgz0+L                                                        
       end function refraction_angle_whole_atmos_vw5cm3m_f543_r4

       elemental function refraction_angle_whole_atmos_vw5cm3m_f543_r8(na,nc,fc,Nmf,beta,R0,delnA,        &
                                                              z0,H0,H1,H2,Hc) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_whole_atmos_vw5cm3m_f543_r8
            !dir$ attributes forceinline :: refraction_angle_whole_atmos_vw5cm3m_f543_r8
#endif 
            real(kind=dp),    intent(in) :: na 
            real(kind=dp),    intent(in) :: nc 
            real(kind=dp),    intent(in) :: fc
            real(kind=dp),    intent(in) :: Nmf 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: H0 
            real(kind=dp),    intent(in) :: H1 
            real(kind=dp),    intent(in) :: H2 
            real(kind=dp),    intent(in) :: Hc                     
            real(kind=dp)                :: alpha 
            real(kind=dp),   automatic   :: H10, ctgz0 
            real(kind=dp),   automatic   :: L1, L2
            real(kind=dp),   automatic   :: L  
            H10   = H1-H0 
            ctgz0 = 1.0_sp/tan(z0) 
            L1    = analytic_sol_L1_whole_atmosphere_f548_r8(beta,R0,delnA,z0,H10)
            L2    = analytic_sol_L2_whole_atmosphere_f552_r8(fc,Nmf,beta,R0,delnA,        &
                                                                    z0,H0,H1,H2,Hc)
            L     = L1+L2 
            alpha = -log(na/nc)*ctgz0+L                                                        
       end function refraction_angle_whole_atmos_vw5cm3m_f543_r8

       ! Formula: 5.59, page: 106
       elemental function analytic_sol_del11_whole_atmos_f559_r4(delnA,z0,H10,Hc0,beta) result(del11)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del11_whole_atmos_f559_r4
            !dir$ attributes forceinline :: analytic_sol_del11_whole_atmos_f559_r4
#endif 
!$omp declare simd(analytic_sol_del11_whole_atmos_f559_r4) 
            real(kind=sp),   intent(in) :: delnA 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp),   intent(in) :: H10 
            real(kind=sp),   intent(in) :: Hc0 
            real(kind=sp),   intent(in) :: beta 
            real(kind=sp)               :: del11 
            real(kind=sp),   automatic  :: tgz0, H10Hc0
            real(kind=sp),   automatic  :: btH10, exp1 
            real(kind=sp),   automatic  :: btHc0, rat 
            real(kind=sp),   automatic  :: trm1,  trm2 
            real(kind=sp),   automatic  :: exp2
            btH10  = beta*H10 
            H10Hc0 = H10/Hc0 
            tgz0   = tan(z0)
            btHc0  = beta*Hc0 
            exp1   = exp(-btH10)
            exp2   = (1.0_sp-exp1)/btHc0
            rat    = 1.0_sp*(1.0_sp-H10Hc0)
            trm1   = delnA*tgz0
            trm2   = rat*exp1-exp2
            del11  = trm1*trm2 
       end function analytic_sol_del11_whole_atmos_f559_r4
 
       elemental function analytic_sol_del11_whole_atmos_f559_r8(delnA,z0,H10,Hc0,beta) result(del11)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del11_whole_atmos_f559_r8
            !dir$ attributes forceinline :: analytic_sol_del11_whole_atmos_f559_r8
#endif 
!$omp declare simd(analytic_sol_del11_whole_atmos_f559_r8) 
            real(kind=dp),   intent(in) :: delnA 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp),   intent(in) :: H10 
            real(kind=dp),   intent(in) :: Hc0 
            real(kind=dp),   intent(in) :: beta 
            real(kind=dp)               :: del11 
            real(kind=dp),   automatic  :: tgz0, H10Hc0
            real(kind=dp),   automatic  :: btH10, exp1 
            real(kind=dp),   automatic  :: btHc0, rat 
            real(kind=dp),   automatic  :: trm1,  trm2 
            real(kind=dp),   automatic  :: exp2
            btH10  = beta*H10 
            H10Hc0 = H10/Hc0 
            tgz0   = tan(z0)
            btHc0  = beta*Hc0 
            exp1   = exp(-btH10)
            exp2   = (1.0_dp-exp1)/btHc0
            rat    = 1.0_dp*(1.0_dp-H10Hc0)
            trm1   = delnA*tgz0
            trm2   = rat*exp1-exp2
            del11  = trm1*trm2 
       end function analytic_sol_del11_whole_atmos_f559_r8

       !Formula: 5.60, page: 107
       elemental function analytic_sol_del12_whole_atmos_f560_r4(fc,Nmf,z0,H10,Hc0,beta,d) result(del12)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del12_whole_atmos_f560_r4
            !dir$ attributes forceinline :: analytic_sol_del12_whole_atmos_f560_r4
#endif 
!$omp declare simd(analytic_sol_del12_whole_atmos_f560_r4) 
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp),   intent(in) :: H10 
            real(kind=sp),   intent(in) :: Hc0 
            real(kind=sp),   intent(in) :: beta 
            real(kind=sp),   intent(in) :: d 
            real(kind=sp)               :: del12 
            real(kind=sp),   automatic  :: delnM, HHc0
            real(kind=sp),   automatic  :: HH10, tgz0 
            real(kind=sp),   automatic  :: rat1, rat2 
            real(kind=sp),   automatic  :: rat3, rat4 
            real(kind=sp),   automatic  :: t0, t1 
            real(kind=sp),   automatic  :: trm1, trm2 
            real(kind=sp),   automatic  :: trm3, trm4  
            HH10  = H10*H10 
            delnM = compute_delnM_f414_r4(fc,Nmf)
            tgz0  = tan(z0)
            rat1  = (Hc0-H10)/d 
            HHc0  = Hc0*Hc0 
            rat2  = 1.0_sp+(H10/d)
            rat3  = 1.0_sp+(H10/Hc0)
            t0    = HHc0+Hc0*H10+HH10 
            rat4  = 1.0_sp+((Hc0+H10)/d)
            t1    = 2.0_sp/(3.0_sp*Hc0*d)
            trm1  = delnM*tgz0*rat1
            trm2  = 2.0_sp*rat1 
            trm3  = rat3*rat4 
            trm4  = t1*t0 
            del12 = trm1*trm2-trm3+trm4 
       end function analytic_sol_del12_whole_atmos_f560_r4

       elemental function analytic_sol_del12_whole_atmos_f560_r8(fc,Nmf,z0,H10,Hc0,beta,d) result(del12)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del12_whole_atmos_f560_r8
            !dir$ attributes forceinline :: analytic_sol_del12_whole_atmos_f560_r8
#endif 
!$omp declare simd(analytic_sol_del12_whole_atmos_f560_r8) 
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp),   intent(in) :: H10 
            real(kind=dp),   intent(in) :: Hc0 
            real(kind=dp),   intent(in) :: beta 
            real(kind=dp),   intent(in) :: d 
            real(kind=dp)               :: del12 
            real(kind=dp),   automatic  :: delnM, HHc0
            real(kind=dp),   automatic  :: HH10, tgz0 
            real(kind=dp),   automatic  :: rat1, rat2 
            real(kind=dp),   automatic  :: rat3, rat4 
            real(kind=dp),   automatic  :: t0, t1 
            real(kind=dp),   automatic  :: trm1, trm2 
            real(kind=dp),   automatic  :: trm3, trm4  
            HH10  = H10*H10 
            delnM = compute_delnM_f414_r8(fc,Nmf)
            tgz0  = tan(z0)
            rat1  = (Hc0-H10)/d 
            HHc0  = Hc0*Hc0 
            rat2  = 1.0_dp+(H10/d)
            rat3  = 1.0_dp+(H10/Hc0)
            t0    = HHc0+Hc0*H10+HH10 
            rat4  = 1.0_dp+((Hc0+H10)/d)
            t1    = 2.0_dp/(3.0_dp*Hc0*d)
            trm1  = delnM*tgz0*rat1
            trm2  = 2.0_dp*rat1 
            trm3  = rat3*rat4 
            trm4  = t1*t0 
            del12 = trm1*trm2-trm3+trm4 
       end function analytic_sol_del12_whole_atmos_f560_r8

       !Formula: 5.55, page: 106
       elemental function analytic_sol_del1_whole_atmos_f555_r4(fc,Nmf,delnA,z0,H10,Hc0,beta,d) result(del1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del1_whole_atmos_f555_r4
            !dir$ attributes forceinline :: analytic_sol_del1_whole_atmos_f555_r4
#endif 

            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: delnA 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp),   intent(in) :: H10 
            real(kind=sp),   intent(in) :: Hc0 
            real(kind=sp),   intent(in) :: beta 
            real(kind=sp),   intent(in) :: d 
            real(kind=sp)               :: del1 
            real(kind=sp),   automatic  :: del11, del12 
            del11  = analytic_sol_del11_whole_atmos_f559_r4(delnA,z0,H10,Hc0,beta)
            del12  = analytic_sol_del12_whole_atmos_f560_r4(fc,Nmf,z0,H10,Hc0,beta,d)
            del1   = del11+del12 
       end function analytic_sol_del1_whole_atmos_f555_r4

       elemental function analytic_sol_del1_whole_atmos_f555_r8(fc,Nmf,delnA,z0,H10,Hc0,beta,d) result(del1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del1_whole_atmos_f555_r8
            !dir$ attributes forceinline :: analytic_sol_del1_whole_atmos_f555_r8
#endif 

            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: delnA 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp),   intent(in) :: H10 
            real(kind=dp),   intent(in) :: Hc0 
            real(kind=dp),   intent(in) :: beta 
            real(kind=dp),   intent(in) :: d 
            real(kind=dp)               :: del1 
            real(kind=dp),   automatic  :: del11, del12 
            del11  = analytic_sol_del11_whole_atmos_f559_r8(delnA,z0,H10,Hc0,beta)
            del12  = analytic_sol_del12_whole_atmos_f560_r8(fc,Nmf,z0,H10,Hc0,beta,d)
            del1   = del11+del12 
       end function analytic_sol_del1_whole_atmos_f555_r8

       !Formula: 5.69, page: 108
       elemental function analytic_sol_del221_whole_atmos_f569_r4(fc,Nmf,z0,H10,Hc0,beta,d) result(del221)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del221_whole_atmos_f569_r4
            !dir$ attributes forceinline :: analytic_sol_del221_whole_atmos_f569_r4
#endif 
!$omp declare simd(analytic_sol_del221_whole_atmos_f569_r4) 
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp),   intent(in) :: H10 
            real(kind=sp),   intent(in) :: Hc0 
            real(kind=sp),   intent(in) :: beta 
            real(kind=sp),   intent(in) :: d 
            real(kind=sp)               :: del12 
            real(kind=sp),   automatic  :: delnM, HHc0
            real(kind=sp),   automatic  :: HH10, ctgz0, scosz0 
            real(kind=sp),   automatic  :: rat1, rat2 
            real(kind=sp),   automatic  :: rat3, rat4 
            real(kind=sp),   automatic  :: t0, t1 
            real(kind=sp),   automatic  :: trm1, trm2 
            real(kind=sp),   automatic  :: trm3, trm4  
            HH10  = H10*H10 
            delnM = compute_delnM_f414_r4(fc,Nmf)
            ctgz0 = 1.0_sp/tan(z0)
            t0    = cos(z0)
            scosz0= t0*t0 
            rat1  = (Hc0-H10)/d 
            HHc0  = Hc0*Hc0 
            rat2  = 1.0_sp+(H10/d)
            rat3  = 1.0_sp+(H10/Hc0)
            t0    = HHc0+Hc0*H10+HH10 
            rat4  = 1.0_sp+((Hc0+H10)/d)
            t1    = 2.0_sp/(3.0_sp*Hc0*d)
            trm1  = -delnM*(ctgz0/scosz0)*rat1
            trm2  = 2.0_sp*rat1 
            trm3  = rat3*rat4 
            trm4  = t1*t0 
            del12 = trm1*trm2-trm3+trm4 
       end function analytic_sol_del221_whole_atmos_f569_r4

        elemental function analytic_sol_del221_whole_atmos_f569_r8(fc,Nmf,z0,H10,Hc0,beta,d) result(del221)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del221_whole_atmos_f569_r8
            !dir$ attributes forceinline :: analytic_sol_del221_whole_atmos_f569_r8
#endif 
!$omp declare simd(analytic_sol_del221_whole_atmos_f569_r8) 
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp),   intent(in) :: H10 
            real(kind=dp),   intent(in) :: Hc0 
            real(kind=dp),   intent(in) :: beta 
            real(kind=dp),   intent(in) :: d 
            real(kind=dp)               :: del12 
            real(kind=dp),   automatic  :: delnM, HHc0
            real(kind=dp),   automatic  :: HH10, ctgz0, scosz0 
            real(kind=dp),   automatic  :: rat1, rat2 
            real(kind=dp),   automatic  :: rat3, rat4 
            real(kind=dp),   automatic  :: t0, t1 
            real(kind=dp),   automatic  :: trm1, trm2 
            real(kind=dp),   automatic  :: trm3, trm4  
            HH10  = H10*H10 
            delnM = compute_delnM_f414_r8(fc,Nmf)
            ctgz0 = 1.0_dp/tan(z0)
            t0    = cos(z0)
            scosz0= t0*t0 
            rat1  = (Hc0-H10)/d 
            HHc0  = Hc0*Hc0 
            rat2  = 1.0_dp+(H10/d)
            rat3  = 1.0_dp+(H10/Hc0)
            t0    = HHc0+Hc0*H10+HH10 
            rat4  = 1.0_dp+((Hc0+H10)/d)
            t1    = 2.0_dp/(3.0_dp*Hc0*d)
            trm1  = -delnM*(ctgz0/scosz0)*rat1
            trm2  = 2.0_dp*rat1 
            trm3  = rat3*rat4 
            trm4  = t1*t0 
            del12 = trm1*trm2-trm3+trm4 
       end function analytic_sol_del221_whole_atmos_f569_r8

       !Function: 5.70, page: 108
       elemental function analytic_sol_del222_whole_atmos_f570_r4(fc,Nmf,z0,H10,Hc0,beta,d,h,R0) result(del222)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del222_whole_atmos_f570_r4
            !dir$ attributes forceinline :: analytic_sol_del222_whole_atmos_f570_r4
#endif 
!$omp declare simd(analytic_sol_del222_whole_atmos_f570_r4) 
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp),   intent(in) :: H10 
            real(kind=sp),   intent(in) :: Hc0 
            real(kind=sp),   intent(in) :: beta 
            real(kind=sp),   intent(in) :: d 
            real(kind=sp),   intent(in) :: h 
            real(kind=sp),   intent(in) :: R0  
            real(kind=sp)               :: del222 
            real(kind=sp),   automatic  :: delnM, ctgz0 
            real(kind=sp),   automatic  :: scosz0, M1 
            real(kind=sp),   automatic  :: M2, M3 
            real(kind=sp),   automatic  :: t1, t2 
            real(kind=sp),   automatic  :: t3, x 
            real(kind=sp),   automatic  :: b, stgz0 
            real(kind=sp),   automatic  :: trm1, trm2 
            real(kind=sp),   automatic  :: trm3
            real(kind=sp),   automatic  :: c0, c1 
            real(kind=sp),   automatic  :: c2, c3 
            real(kind=sp),   automatic  :: sqrx 
            c0     = tan(z0)
            delN   = compute_delnM_f414_r4(fc,Nmf)
            ctgz0  = 1.0_sp/c0
            stgz0  = c0*c0 
            b      = 2.0_sp*(stgz0/R0)
            x      = 1.0_sp+b*h 
            c1     = cos(z0)
            scosz0 = c1*c1 
            trm1   = 2.0_sp*(delnM/d)*(ctgz0/scosz0) ! T2 
            t1     = 1.0_sp+(H10/d)
            c2     = 1.0_sp/Hc0 
            sqrx   = sqrt(x)
            c3     = 1.0_sp/d 
            c0     = H10/Hc0 
            t2     = c2+c3*c0+c3 
            t3     = 1.0_sp/(Hc0*d)
            M1     = t1*(2.0_sp/b)*sqrx 
            c0     = 0.333333333_sp*x-1.0_sp
            c1     = 2.0_sp/(b*b)*sqrx
            M2     = t2*c0*c1 
            c2     = 0.2_sp*x*x 
            c3     = 0.66666666666666666667_sp*x+1.0_sp 
            c1     = 2.0_sp/(b*b*b)*sqrx 
            M3     = t3*(c2-c3)*c1 
            trm2   = M1*Hc0-M2*Hc0+M3*Hc0
            trm3   = M1*H10-M2*H10+M3*H10 
            del222 = trm1*(trm2-trm3)
       end function analytic_sol_del222_whole_atmos_f570_r4

        elemental function analytic_sol_del222_whole_atmos_f570_r8(fc,Nmf,z0,H10,Hc0,beta,d,h,R0) result(del222)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del222_whole_atmos_f570_r8
            !dir$ attributes forceinline :: analytic_sol_del222_whole_atmos_f570_r8
#endif 
!$omp declare simd(analytic_sol_del222_whole_atmos_f570_r8) 
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp),   intent(in) :: H10 
            real(kind=dp),   intent(in) :: Hc0 
            real(kind=dp),   intent(in) :: beta 
            real(kind=dp),   intent(in) :: d 
            real(kind=dp),   intent(in) :: h 
            real(kind=dp),   intent(in) :: R0  
            real(kind=dp)               :: del222 
            real(kind=dp),   automatic  :: delnM, ctgz0 
            real(kind=dp),   automatic  :: scosz0, M1 
            real(kind=dp),   automatic  :: M2, M3 
            real(kind=dp),   automatic  :: t1, t2 
            real(kind=dp),   automatic  :: t3, x 
            real(kind=dp),   automatic  :: b, stgz0 
            real(kind=dp),   automatic  :: trm1, trm2 
            real(kind=dp),   automatic  :: trm3
            real(kind=dp),   automatic  :: c0, c1 
            real(kind=dp),   automatic  :: c2, c3 
            real(kind=dp),   automatic  :: sqrx 
            c0     = tan(z0)
            delN   = compute_delnM_f414_r8(fc,Nmf)
            ctgz0  = 1.0_dp/c0
            stgz0  = c0*c0 
            b      = 2.0_dp*(stgz0/R0)
            x      = 1.0_dp+b*h 
            c1     = cos(z0)
            scosz0 = c1*c1 
            trm1   = 2.0_dp*(delnM/d)*(ctgz0/scosz0) ! T2 
            t1     = 1.0_dp+(H10/d)
            c2     = 1.0_dp/Hc0 
            sqrx   = sqrt(x)
            c3     = 1.0_dp/d 
            c0     = H10/Hc0 
            t2     = c2+c3*c0+c3 
            t3     = 1.0_dp/(Hc0*d)
            M1     = t1*(2.0_dp/b)*sqrx 
            c0     = 0.333333333_dp*x-1.0_dp
            c1     = 2.0_dp/(b*b)*sqrx
            M2     = t2*c0*c1 
            c2     = 0.2_dp*x*x 
            c3     = 0.66666666666666666667_dp*x+1.0_dp 
            c1     = 2.0_sp/(b*b*b)*sqrx 
            M3     = t3*(c2-c3)*c1 
            trm2   = M1*Hc0-M2*Hc0+M3*Hc0
            trm3   = M1*H10-M2*H10+M3*H10 
            del222 = trm1*(trm2-trm3)
       end function analytic_sol_del222_whole_atmos_f570_r8

        !Function: 5.71, page: 108
       elemental function analytic_sol_del223_whole_atmos_f571_r4(delnA,fc,Nmf,z0,H10,Hc0,beta,d,h,R0) result(del223)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del223_whole_atmos_f571_r4
            !dir$ attributes forceinline :: analytic_sol_del223_whole_atmos_f571_r4
#endif 
!$omp declare simd(analytic_sol_del223_whole_atmos_f571_r4) 
            real(kind=sp),   intent(in) :: delnA 
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp),   intent(in) :: H10 
            real(kind=sp),   intent(in) :: Hc0 
            real(kind=sp),   intent(in) :: beta 
            real(kind=sp),   intent(in) :: d 
            real(kind=sp),   intent(in) :: h 
            real(kind=sp),   intent(in) :: R0  
            real(kind=sp)               :: del223 
            real(kind=sp),   automatic  :: tgz0, scosz0 
            real(kind=sp),   automatic  :: isqrx, tb
            real(kind=sp),   automatic  :: tbb, tbbb 
            real(kind=sp),   automatic  :: delnM, x 
            real(kind=sp),   automatic  :: b, v1
            real(kind=sp),   automatic  :: u1, u2 
            real(kind=sp),   automatic  :: u3, u4 
            real(kind=sp),   automatic  :: f0, f1 
            real(kind=sp),   automatic  :: f2, f3 
            real(kind=sp),   automatic  :: f4, s1 
            real(kind=sp),   automatic  :: s2, s3 
            real(kind=sp),   automatic  :: T3, K1
            real(kind=sp),   automatic  :: K2, K3
            real(kind=sp),   automatic  :: K4, K5 
            real(kind=sp),   automatic  :: c0, c1
            real(kind=sp),   automatic  :: c2, c3 
            real(kind=sp),   automatic  :: xx, xxx 
            real(kind=sp),   automatic  :: xxxx 
            real(kind=sp),   automatic  :: trm1, trm2 
            real(kind=sp),   automatic  :: trm3, trm4 
            tgz0    = tan(z0)
            c0      = cos(z0)
            stgz0   = tgz0*tgz0 
            scosz0  = c0*c0 
            b       = 2.0_sp*(stgz0/R0)
            x       = 1.0_sp+b*h 
            v1      = 1.0_sp/Hc0 
            u1      = 1.0_sp+(H10/d)
            delnM   = compute_delnM_f414_r4(fc,Nmf)
            u2      = 1.0_sp/d 
            c0      = H10/(d*d)
            xx      = x*x 
            c1      = d+d+H10
            s1      = delnA-delnM*c0*c1 
            c2      = (d+H10)/(d*d)
            s2      = delnM+delnM*c2 
            xxx     = xx*x 
            s3      = delnM/(d*d)
            f0      = u2*s1 
            f1      = u1*s2-u2*s1-v1*u1*s1 
            f2      = u1*s3+u2*s2+v1*u1*s2 
            xxxx    = xxx*x 
            f3      = u2*s3+v1*u1*s3+v1*u2*s2 
            f4      = v1*u2*s3 
            isqrx   = 1.0_sp/sqrt(x) 
            tb      = 2.0_sp/b*isqrx
            K1      = -f0*tb 
            tbb     = 2.0_sp/(b*b)*isqrx
            K2      = f1*(x+1.0_sp)*tb
            tbbb    = 2.0_sp/(b*b*b)*isqrx
            c0      = 0.33333333333333333_sp*x*x 
            c1      = x+x-1.0_sp
            K3      = -f2*(c0-c1)*tbbb 
            tbbbb   = 2.0_sp/(b*b*b*b)*isqrx
            c2      = 0.2*xxx 
            c3      = xx+3.0_sp*x+1.0_sp 
            K4      = f3*(c2-c3)*tbbbb 
            tbbbbb  = 2.0_sp/(b*b*b*b*b)*isqrx
            c0      = 0.142857142857142857142857142857_sp* &
                      xxxx
            c1      = 0.8*xxx+2.0_sp*xx-4.0_sp*x-1.0_sp 
            K5      = -f4*(c0-c1)*isqrx 
            c2      = tgz0/scosz0
            T3      = 2.0_sp*(delnM/d)*c2 
            trm1    = K1*Hc0+K2*Hc0+K3*Hc0 
            trm2    = K4*Hc0+K5*Hc0 
            trm3    = K1*H10+K2*H10+K3*H10 
            trm4    = K4*H10+K5*H10 
            del223  = T3*(trm1+trm2-trm3+trm4)
       end function analytic_sol_del223_whole_atmos_f571_r4

        elemental function analytic_sol_del223_whole_atmos_f571_r8(delnA,fc,Nmf,z0,H10,Hc0,beta,d,h,R0) result(del223)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del223_whole_atmos_f571_r8
            !dir$ attributes forceinline :: analytic_sol_del223_whole_atmos_f571_r8
#endif 
!$omp declare simd(analytic_sol_del223_whole_atmos_f571_r8) 
            real(kind=dp),   intent(in) :: delnA 
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp),   intent(in) :: H10 
            real(kind=dp),   intent(in) :: Hc0 
            real(kind=dp),   intent(in) :: beta 
            real(kind=dp),   intent(in) :: d 
            real(kind=dp),   intent(in) :: h 
            real(kind=dp),   intent(in) :: R0  
            real(kind=dp)               :: del223 
            real(kind=dp),   automatic  :: tgz0, scosz0 
            real(kind=dp),   automatic  :: isqrx, tb
            real(kind=dp),   automatic  :: tbb, tbbb 
            real(kind=dp),   automatic  :: delnM, x 
            real(kind=d),   automatic  :: b, v1
            real(kind=dp),   automatic  :: u1, u2 
            real(kind=dp),   automatic  :: u3, u4 
            real(kind=dp),   automatic  :: f0, f1 
            real(kind=dp),   automatic  :: f2, f3 
            real(kind=dp),   automatic  :: f4, s1 
            real(kind=dp),   automatic  :: s2, s3 
            real(kind=dp),   automatic  :: T3, K1
            real(kind=dp),   automatic  :: K2, K3
            real(kind=dp),   automatic  :: K4, K5 
            real(kind=dp),   automatic  :: c0, c1
            real(kind=dp),   automatic  :: c2, c3 
            real(kind=dp),   automatic  :: xx, xxx 
            real(kind=dp),   automatic  :: xxxx 
            real(kind=dp),   automatic  :: trm1, trm2 
            real(kind=dp),   automatic  :: trm3, trm4 
            tgz0    = tan(z0)
            c0      = cos(z0)
            stgz0   = tgz0*tgz0 
            scosz0  = c0*c0 
            b       = 2.0_dp*(stgz0/R0)
            x       = 1.0_dp+b*h 
            v1      = 1.0_dp/Hc0 
            u1      = 1.0_dp+(H10/d)
            delnM   = compute_delnM_f414_r8(fc,Nmf)
            u2      = 1.0_dp/d 
            c0      = H10/(d*d)
            xx      = x*x 
            c1      = d+d+H10
            s1      = delnA-delnM*c0*c1 
            c2      = (d+H10)/(d*d)
            s2      = delnM+delnM*c2 
            xxx     = xx*x 
            s3      = delnM/(d*d)
            f0      = u2*s1 
            f1      = u1*s2-u2*s1-v1*u1*s1 
            f2      = u1*s3+u2*s2+v1*u1*s2 
            xxxx    = xxx*x 
            f3      = u2*s3+v1*u1*s3+v1*u2*s2 
            f4      = v1*u2*s3 
            isqrx   = 1.0_dp/sqrt(x) 
            tb      = 2.0_dp/b*isqrx
            K1      = -f0*tb 
            tbb     = 2.0_dp/(b*b)*isqrx
            K2      = f1*(x+1.0_dp)*tb
            tbbb    = 2.0_dp/(b*b*b)*isqrx
            c0      = 0.33333333333333333_dp*x*x 
            c1      = x+x-1.0_sp
            K3      = -f2*(c0-c1)*tbbb 
            tbbbb   = 2.0_dp/(b*b*b*b)*isqrx
            c2      = 0.2_dp*xxx 
            c3      = xx+3.0_dp*x+1.0_dp 
            K4      = f3*(c2-c3)*tbbbb 
            tbbbbb  = 2.0_dp/(b*b*b*b*b)*isqrx
            c0      = 0.142857142857142857142857142857_dp* &
                      xxxx
            c1      = 0.8_dp*xxx+2.0_dp*xx-4.0_dp*x-1.0_dp 
            K5      = -f4*(c0-c1)*isqrx 
            c2      = tgz0/scosz0
            T3      = 2.0_dp*(delnM/d)*c2 
            trm1    = K1*Hc0+K2*Hc0+K3*Hc0 
            trm2    = K4*Hc0+K5*Hc0 
            trm3    = K1*H10+K2*H10+K3*H10 
            trm4    = K4*H10+K5*H10 
            del223  = T3*(trm1+trm2-trm3+trm4)
       end function analytic_sol_del223_whole_atmos_f571_r8

       !Formula: 5.65, page: 108
       elemental function analytic_sol_del22_whole_atmos_f565_r4(delnA,fc,Nmf,z0,H10,          &
                                                                 Hc0,beta,d,h,R0) result(del22)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del22_whole_atmos_f565_r4
            !dir$ attributes forceinline :: analytic_sol_del22_whole_atmos_f565_r4
#endif 
 
            real(kind=sp),   intent(in) :: delnA 
            real(kind=sp),   intent(in) :: fc 
            real(kind=sp),   intent(in) :: Nmf 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp),   intent(in) :: H10 
            real(kind=sp),   intent(in) :: Hc0 
            real(kind=sp),   intent(in) :: beta 
            real(kind=sp),   intent(in) :: d 
            real(kind=sp),   intent(in) :: h 
            real(kind=sp),   intent(in) :: R0  
            real(kind=sp)               :: del22
            real(kind=sp),   automatic  :: del221, del222, del223 
            del221  = analytic_sol_del221_whole_atmos_f569_r4(fc,Nmf,z0,H10,Hc0,beta,d)
            del222  = analytic_sol_del222_whole_atmos_f570_r4(fc,Nmf,z0,H10,Hc0,beta,d,h,R0)
            del223  = analytic_sol_del223_whole_atmos_f571_r4(delnA,fc,Nmf,z0,H10,Hc0,beta,d,h,R0)
            del22   = del221+del222+del223 
       end function analytic_sol_del22_whole_atmos_f565_r4

       elemental function analytic_sol_del22_whole_atmos_f565_r8(delnA,fc,Nmf,z0,H10,          &
                                                                 Hc0,beta,d,h,R0) result(del22)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del22_whole_atmos_f565_r8
            !dir$ attributes forceinline :: analytic_sol_del22_whole_atmos_f565_r8
#endif 
 
            real(kind=dp),   intent(in) :: delnA 
            real(kind=dp),   intent(in) :: fc 
            real(kind=dp),   intent(in) :: Nmf 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp),   intent(in) :: H10 
            real(kind=dp),   intent(in) :: Hc0 
            real(kind=dp),   intent(in) :: beta 
            real(kind=dp),   intent(in) :: d 
            real(kind=dp),   intent(in) :: h 
            real(kind=dp),   intent(in) :: R0  
            real(kind=dp)               :: del22
            real(kind=dp),   automatic  :: del221, del222, del223 
            del221  = analytic_sol_del221_whole_atmos_f569_r8(fc,Nmf,z0,H10,Hc0,beta,d)
            del222  = analytic_sol_del222_whole_atmos_f570_r8(fc,Nmf,z0,H10,Hc0,beta,d,h,R0)
            del223  = analytic_sol_del223_whole_atmos_f571_r8(delnA,fc,Nmf,z0,H10,Hc0,beta,d,h,R0)
            del22   = del221+del222+del223 
       end function analytic_sol_del22_whole_atmos_f565_r8

        ! Formula: 5.62, page: 107
       elemental function analytic_sol_del211_whole_atmos_f562_r4(delnA,z0,H10,Hc0,beta) result(del11)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del211_whole_atmos_f562_r4
            !dir$ attributes forceinline :: analytic_sol_del211_whole_atmos_f562_r4
#endif 
!$omp declare simd(analytic_sol_del211_whole_atmos_f562_r4) 
            real(kind=sp),   intent(in) :: delnA 
            real(kind=sp),   intent(in) :: z0 
            real(kind=sp),   intent(in) :: H10 
            real(kind=sp),   intent(in) :: Hc0 
            real(kind=sp),   intent(in) :: beta 
            real(kind=sp)               :: del11 
            real(kind=sp),   automatic  :: ctgz0, H10Hc0
            real(kind=sp),   automatic  :: btH10, exp1 
            real(kind=sp),   automatic  :: btHc0, rat 
            real(kind=sp),   automatic  :: trm1,  trm2 
            real(kind=sp),   automatic  :: exp2,  ssecz0
            real(kind=sp),   automatic  :: t0  
            
            btH10  = beta*H10 
            t0     = 1.0_sp/cos(z0)
            H10Hc0 = H10/Hc0 
            ctgz0  = 1.0_sp/tan(z0)
            ssecz0 = t0*t0 
            btHc0  = beta*Hc0 
            exp1   = exp(-btH10)
            exp2   = (1.0_sp-exp1)/btHc0
            t0     = -ctgz0*ssecz0
            rat    = 1.0_sp*(1.0_sp-H10Hc0)
            trm1   = delnA*t0 
            trm2   = rat*exp1-exp2
            del11  = trm1*trm2 
       end function analytic_sol_del211_whole_atmos_f562_r4

       elemental function analytic_sol_del211_whole_atmos_f562_r8(delnA,z0,H10,Hc0,beta) result(del11)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del211_whole_atmos_f562_r8
            !dir$ attributes forceinline :: analytic_sol_del211_whole_atmos_f562_r8
#endif 
!$omp declare simd(analytic_sol_del211_whole_atmos_f562_r8) 
            real(kind=dp),   intent(in) :: delnA 
            real(kind=dp),   intent(in) :: z0 
            real(kind=dp),   intent(in) :: H10 
            real(kind=dp),   intent(in) :: Hc0 
            real(kind=dp),   intent(in) :: beta 
            real(kind=dp)               :: del11 
            real(kind=dp),   automatic  :: ctgz0, H10Hc0
            real(kind=dp),   automatic  :: btH10, exp1 
            real(kind=dp),   automatic  :: btHc0, rat 
            real(kind=dp),   automatic  :: trm1,  trm2 
            real(kind=dp),   automatic  :: exp2,  ssecz0
            real(kind=dp),   automatic  :: t0  
            
            btH10  = beta*H10 
            t0     = 1.0_dp/cos(z0)
            H10Hc0 = H10/Hc0 
            ctgz0  = 1.0_dp/tan(z0)
            ssecz0 = t0*t0 
            btHc0  = beta*Hc0 
            exp1   = exp(-btH10)
            exp2   = (1.0_dp-exp1)/btHc0
            t0     = -ctgz0*ssecz0
            rat    = 1.0_dp*(1.0_dp-H10Hc0)
            trm1   = delnA*t0 
            trm2   = rat*exp1-exp2
            del11  = trm1*trm2 
       end function analytic_sol_del211_whole_atmos_f562_r8

       ! Formula: 5.63, page: 107
        elemental function analytic_sol_del212_whole_atmos_f563_r4(delnA,z0,beta,H10,R0) result(del212)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del212_whole_atmos_f563_r4
            !dir$ attributes forceinline :: analytic_sol_del212_whole_atmos_f563_r4
#endif 
            
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: H10 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp)  :: del212
            del212  =  analytic_sol_tropo_del22_wvle5cm_deg0_80_f525_r4(delnA,z0,beta,H10,R0)
        end function analytic_sol_del212_whole_atmos_f563_r4

        elemental function analytic_sol_del212_whole_atmos_f563_r8(delnA,z0,beta,H10,R0) result(del212)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del212_whole_atmos_f563_r8
            !dir$ attributes forceinline :: analytic_sol_del212_whole_atmos_f563_r8
#endif 
            
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: H10 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp)  :: del212
            del212  =  analytic_sol_tropo_del22_wvle5cm_deg0_80_f525_r8(delnA,z0,beta,H10,R0)
        end function analytic_sol_del212_whole_atmos_f563_r8

        !Formula: 5.64, pagr: 107
       elemental function analytic_sol_del213_whole_atmos_f564_r4(delnA,z0,beta,H10,R0) result(del213)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del213_whole_atmos_f564_r4
            !dir$ attributes forceinline :: analytic_sol_del213_whole_atmos_f564_r4
#endif 
            
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: H10 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp)                :: del213 
            del213   = analytic_sol_tropo_del23_wvle5cm_deg0_80_f526_r4(delnA,z0,beta,H10,R0)
        end function analytic_sol_del213_whole_atmos_f564_r4

       elemental function analytic_sol_del213_whole_atmos_f564_r8(delnA,z0,beta,H10,R0) result(del213)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del213_whole_atmos_f564_r8
            !dir$ attributes forceinline :: analytic_sol_del213_whole_atmos_f564_r8
#endif 
            
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: H10 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp)                :: del213 
            del213   = analytic_sol_tropo_del23_wvle5cm_deg0_80_f526_r8(delnA,z0,bta,H10,R0)
        end function analytic_sol_del213_whole_atmos_f564_r8

       ! Formula: 5.65, page: 108
       elemental function analytic_sol_del21_whole_atmos_f565_r4(delnA,z0,beta,H10,R0) result(del21)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del21_whole_atmos_f565_r4
            !dir$ attributes forceinline :: analytic_sol_del21_whole_atmos_f565_r4
#endif 
            
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: H10 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp)                :: del21 
            real(kind=sp),    automatic  :: del211, del212, del213 
            del211  = analytic_sol_del211_whole_atmos_f562_r4(delnA,z0,H10,Hc0,beta)
            del212  = analytic_sol_del212_whole_atmos_f563_r4(delnA,z0,beta,H10,R0)
            del213  = analytic_sol_del213_whole_atmos_f564_r4(delnA,z0,beta,H10,R0)
            del21   = del211+del212+del213 
       end function analytic_sol_del21_whole_atmos_f565_r4

       elemental function analytic_sol_del21_whole_atmos_f565_r8(delnA,z0,beta,H10,R0) result(del21)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del21_whole_atmos_f565_r8
            !dir$ attributes forceinline :: analytic_sol_del21_whole_atmos_f565_r8
#endif 
            
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: H10 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp)                :: del21 
            real(kind=dp),    automatic  :: del211, del212, del213 
            del211  = analytic_sol_del211_whole_atmos_f562_r8(delnA,z0,H10,Hc0,beta)
            del212  = analytic_sol_del212_whole_atmos_f563_r8(delnA,z0,beta,H10,R0)
            del213  = analytic_sol_del213_whole_atmos_f564_r8(delnA,z0,beta,H10,R0)
            del21   = del211+del212+del213 
       end function analytic_sol_del21_whole_atmos_f565_r8

        !Formula: 5.58, page: 106
       elemental function analytic_sol_del2_whole_atmos_wv5cm3m_f558_r4(fc,Nmf,delnA,z0,H10,           &
                                                                        Hc0,beta,d,h,R0) result(del2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del2_whole_atmos_wv5cm3m_f558_r4
            !dir$ attributes forceinline :: analytic_sol_del2_whole_atmos_wv5cm3m_f558_r4
#endif 
            real(kind=sp),    intent(in) :: fc 
            real(kind=sp),    intent(in) :: Nmf 
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: H10 
            real(kind=sp),    intent(in) :: Hc0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: d 
            real(kind=sp),    intent(in) :: h 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp)                :: del2 
            real(kind=sp),    automatic  :: del21, del22 
            del21   = analytic_sol_del21_whole_atmos_f565_r4(delnA,z0,beta,H10,R0)
            del22   = analytic_sol_del22_whole_atmos_f565_r4(delnA,fc,Nmf,z0,H10,          &
                                                                 Hc0,beta,d,h,R0)
       end function analytic_sol_del2_whole_atmos_wv5cm3m_f558_r4

       elemental function analytic_sol_del2_whole_atmos_wv5cm3m_f558_r8(fc,Nmf,delnA,z0,H10,           &
                                                                        Hc0,beta,d,h,R0) result(del2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_del2_whole_atmos_wv5cm3m_f558_r8
            !dir$ attributes forceinline :: analytic_sol_del2_whole_atmos_wv5cm3m_f558_r8
#endif 
            real(kind=dp),    intent(in) :: fc 
            real(kind=dp),    intent(in) :: Nmf 
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: H10 
            real(kind=dp),    intent(in) :: Hc0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: d 
            real(kind=dp),    intent(in) :: h 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp)                :: del2 
            real(kind=dp),    automatic  :: del21, del22 
            del21   = analytic_sol_del21_whole_atmos_f565_r8(delnA,z0,beta,H10,R0)
            del22   = analytic_sol_del22_whole_atmos_f565_r8(delnA,fc,Nmf,z0,H10,          &
                                                                 Hc0,beta,d,h,R0)
       end function analytic_sol_del2_whole_atmos_wv5cm3m_f558_r8

       ! Formule: 5.54, page: 106
       elemental function refraction_angle_whole_atmos_vw5cm3m_f554_r4(fc,Nmf,delnA,z0,H10,           &
                                                                        Hc0,beta,d,h,R0)   result(angle)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_whole_atmos_vw5cm3m_f554_r4
            !dir$ attributes forceinline :: refraction_angle_whole_atmos_vw5cm3m_f554_r4
#endif 
            real(kind=sp),    intent(in) :: fc 
            real(kind=sp),    intent(in) :: Nmf 
            real(kind=sp),    intent(in) :: delnA 
            real(kind=sp),    intent(in) :: z0 
            real(kind=sp),    intent(in) :: H10 
            real(kind=sp),    intent(in) :: Hc0 
            real(kind=sp),    intent(in) :: beta 
            real(kind=sp),    intent(in) :: d 
            real(kind=sp),    intent(in) :: h 
            real(kind=sp),    intent(in) :: R0 
            real(kind=sp)                :: angle 
            real(kind=sp),    automatic  :: del1, del2 
            del1  = analytic_sol_del1_whole_atmos_f555_r4(fc,Nmf,delnA,z0,H10,Hc0,beta,d)
            del2  = analytic_sol_del2_whole_atmos_wv5cm3m_f558_r4(fc,Nmf,delnA,z0,H10,           &
                                                                        Hc0,beta,d,h,R0)
            angle = del1+del2 
       end function refraction_angle_whole_atmos_vw5cm3m_f554_r4

       elemental function refraction_angle_whole_atmos_vw5cm3m_f554_r8(fc,Nmf,delnA,z0,H10,           &
                                                                        Hc0,beta,d,h,R0)   result(angle)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_whole_atmos_vw5cm3m_f554_r8
            !dir$ attributes forceinline :: refraction_angle_whole_atmos_vw5cm3m_f554_r8
#endif 
            real(kind=dp),    intent(in) :: fc 
            real(kind=dp),    intent(in) :: Nmf 
            real(kind=dp),    intent(in) :: delnA 
            real(kind=dp),    intent(in) :: z0 
            real(kind=dp),    intent(in) :: H10 
            real(kind=dp),    intent(in) :: Hc0 
            real(kind=dp),    intent(in) :: beta 
            real(kind=dp),    intent(in) :: d 
            real(kind=dp),    intent(in) :: h 
            real(kind=dp),    intent(in) :: R0 
            real(kind=dp)                :: angle 
            real(kind=dp),    automatic  :: del1, del2 
            del1  = analytic_sol_del1_whole_atmos_f555_r8(fc,Nmf,delnA,z0,H10,Hc0,beta,d)
            del2  = analytic_sol_del2_whole_atmos_wv5cm3m_f558_r8(fc,Nmf,delnA,z0,H10,           &
                                                                        Hc0,beta,d,h,R0)
            angle = del1+del2 
       end function 
       
       !высотная зависимость показателя преломления в 
       ! верхней ионосфере имеет вид
       ! Formula: 5.72, page: 110
       elemental function refractive_idx_hi_ionosphere_approx_f572_r4(fc,Nmf,beta,h,H20) result(n)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractive_idx_hi_ionosphere_approx_f572_r4
            !dir$ attributes forceinline :: refractive_idx_hi_ionosphere_approx_f572_r4
#endif 
!$omp declare simd(refractive_idx_hi_ionosphere_approx_f572_r4) 
            real(kind=sp),      intent(in) :: fc 
            real(kind=sp),      intent(in) :: Nmf 
            real(kind=sp),      intent(in) :: beta 
            real(kind=sp),      intent(in) :: h 
            real(kind=sp),      intent(in) :: H20 
            real(kind=sp)                  :: n 
            real(kind=sp),      automatic  :: delNm, hH20 
            real(kind=sp),      automatic  :: exp1 
            hH20  = -beta*(h-H20)
            delNm = compute_delnM_f414_r4(fc,Nmf)
            exp1  = exp(hH20)
            n     = 1.0_sp-delNm*exp1 
       end function refractive_idx_hi_ionosphere_approx_f572_r4

       elemental function refractive_idx_hi_ionosphere_approx_f572_r8(fc,Nmf,beta,h,H20) result(n)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractive_idx_hi_ionosphere_approx_f572_r8
            !dir$ attributes forceinline :: refractive_idx_hi_ionosphere_approx_f572_r8
#endif 
!$omp declare simd(refractive_idx_hi_ionosphere_approx_f572_r8) 
            real(kind=dp),      intent(in) :: fc 
            real(kind=dp),      intent(in) :: Nmf 
            real(kind=dp),      intent(in) :: beta 
            real(kind=dp),      intent(in) :: h 
            real(kind=dp),      intent(in) :: H20 
            real(kind=dp)                  :: n 
            real(kind=dp),      automatic  :: delNm, hH20 
            real(kind=dp),      automatic  :: exp1 
            hH20  = -beta*(h-H20)
            delNm = compute_delnM_f414_r8(fc,Nmf)
            exp1  = exp(hH20)
            n     = 1.0_dp-delNm*exp1 
       end function refractive_idx_hi_ionosphere_approx_f572_r8

      !Приемник находится в нейтроофере в точке А на
      !высоте H0, а излучатель — в верхней ионосфере в точке С
      !на высоте Нс над поверхностью Земли. 
      !Formula for 'L31' sub-component: 5.76, page: 111
      elemental function analytic_sol_L31_whole_atmos_wv5cm3m_f576_r4(fc,Nmf,beta,R0,z0,H20,Hc0,Hc,H2) result(L31)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L31_whole_atmos_wv5cm3m_f576_r4
            !dir$ attributes forceinline :: analytic_sol_L31_whole_atmos_wv5cm3m_f576_r4
#endif 
!$omp declare simd(analytic_sol_L31_whole_atmos_wv5cm3m_f576_r4) 
            real(kind=sp),      intent(in) :: fc 
            real(kind=sp),      intent(in) :: Nmf 
            real(kind=sp),      intent(in) :: beta 
            real(kind=sp),      intent(in) ::  R0 
            real(kind=sp),      intent(in) :: z0 
            real(kind=sp),      intent(in) :: H20 
            real(kind=sp),      intent(in) :: Hc0 
            real(kind=sp),      intent(in) :: Hc 
            real(kind=sp),      intent(in) ::  H2
            real(kind=sp)                  :: L31 
            real(kind=sp),      automatic  :: t0, t1 
            real(kind=sp),      automatic  :: stgz0, ctgz0 
            real(kind=sp),      automatic  :: scosz0, earg 
            real(kind=sp),      automatic  :: exp1, sqr1 
            real(kind=sp),      automatic  :: trm1, trm2 
            real(kind=sp),      automatic  :: trm3, sdelNm
            real(kind=sp),      automatic  :: sqr2 
            earg  = -2.0_sp*beta*(Hc-H2)
            t0    = compute_delnM_f414_r4(fc,Nmf)
            sdelNm= t0*t0 
            t1    = tan(z0)
            ctgz0 = 1.0_sp/t1
            stgz0 = t1*t1 
            t0    = cos(z0)
            scosz0= t0*t0 
            trm1  = -sdelNm*beta*R0*(ctgz0/scosz0)
            t0    = H20/R0 
            t1    = 2.0_sp*stgz0 
            sqr1  = sqrt(1.0_sp+t1*t0)
            trm2  = 1.0_sp/sqr1
            exp1  = exp(earg)
            t0    = Hc0/R0
            sqr2  = sqrt(1.0_sp+t1*t0) 
            trm3  = exp1/sqr2 
            L13   = trm1*(trm2-trm3)
      end function analytic_sol_L31_whole_atmos_wv5cm3m_f576_r4

      elemental function analytic_sol_L31_whole_atmos_wv5cm3m_f576_r8(fc,Nmf,beta,R0,z0,H20,Hc0,Hc,H2) result(L31)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L31_whole_atmos_wv5cm3m_f576_r8
            !dir$ attributes forceinline :: analytic_sol_L31_whole_atmos_wv5cm3m_f576_r8
#endif 
!$omp declare simd(analytic_sol_L31_whole_atmos_wv5cm3m_f576_r8) 
            real(kind=dp),      intent(in) :: fc 
            real(kind=dp),      intent(in) :: Nmf 
            real(kind=dp),      intent(in) :: beta 
            real(kind=dp),      intent(in) ::  R0 
            real(kind=dp),      intent(in) :: z0 
            real(kind=dp),      intent(in) :: H20 
            real(kind=dp),      intent(in) :: Hc0 
            real(kind=dp),      intent(in) :: Hc 
            real(kind=dp),      intent(in) ::  H2
            real(kind=dp)                  :: L31 
            real(kind=dp),      automatic  :: t0, t1 
            real(kind=dp),      automatic  :: stgz0, ctgz0 
            real(kind=dp),      automatic  :: scosz0, earg 
            real(kind=dp),      automatic  :: exp1, sqr1 
            real(kind=dp),      automatic  :: trm1, trm2 
            real(kind=dp),      automatic  :: trm3, sdelNm
            real(kind=dp),      automatic  :: sqr2 
            earg  = -2.0_dp*beta*(Hc-H2)
            t0    = compute_delnM_f414_r8(fc,Nmf)
            sdelNm= t0*t0 
            t1    = tan(z0)
            ctgz0 = 1.0_dp/t1
            stgz0 = t1*t1 
            t0    = cos(z0)
            scosz0= t0*t0 
            trm1  = -sdelNm*beta*R0*(ctgz0/scosz0)
            t0    = H20/R0 
            t1    = 2.0_dp*stgz0 
            sqr1  = sqrt(1.0_dp+t1*t0)
            trm2  = 1.0_dp/sqr1
            exp1  = exp(earg)
            t0    = Hc0/R0
            sqr2  = sqrt(1.0_dp+t1*t0) 
            trm3  = exp1/sqr2 
            L13   = trm1*(trm2-trm3)
      end function analytic_sol_L31_whole_atmos_wv5cm3m_f576_r8

      !Formula: 5.77, page: 111
      elemental function analytic_sol_L32_whole_atmos_wv5cm3m_f577_r4(fc,Nmf,beta,R0,z0,H20,Hc0) result(L32)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L32_whole_atmos_wv5cm3m_f577_r4
            !dir$ attributes forceinline :: analytic_sol_L32_whole_atmos_wv5cm3m_f577_r4
#endif 
!$omp declare simd(analytic_sol_L32_whole_atmos_wv5cm3m_f577_r4) 
            real(kind=sp),      intent(in) :: fc 
            real(kind=sp),      intent(in) :: Nmf 
            real(kind=sp),      intent(in) :: beta 
            real(kind=sp),      intent(in) :: R0 
            real(kind=sp),      intent(in) :: z0 
            real(kind=sp),      intent(in) :: H20 
            real(kind=sp),      intent(in) :: Hc0 
            real(kind=sp)                  :: L32 
            real(kind=sp),      parameter  :: C1253314137315500251207882642406 = & 
                                                  1.253314137315500251207882642406_sp
            real(kind=sp),      automatic  :: delNm, rat1 
            real(kind=sp),      automatic  :: earg, exp1 
            real(kind=sp),      automatic  :: tgz0, stgz0 
            real(kind=sp),      automatic  :: prob1, prob2 
            real(kind=sp),      automatic  :: p1arg, p2arg 
            real(kind=sp),      automatic  :: t0, t1 
            real(kind=sp),      automatic  :: sctgz0, trm1 
            real(kind=sp),      automatic  :: trm2
            tgz0   = tan(z0)
            stgz0  = tgz0*tgz0 
            t0     = 1.0_sp/tgz0 
            sctgz0 = t0*t0 
            delnNm = compute_delnM_f414_r4(fc,Nmf)
            t1     = R0/(stgz0+stgz0) 
            earg   = beta*(t1+H20)
            exp1   = exp(earg)
            t0     = beta*R0*sctgz0
            p1arg  = sqrt(t0+beta+beta*Hc0)
            p2arg  = sqrt(t0+beta+beta*H20)
            t1     = sqrt(beta*R0)/tgz0 
            trm1   = -delNm*t1*exp1 
            prob1  = prob_integral_r4(p1arg)
            prob2  = prob_integral_r4(p2arg)
            trm2   = C1253314137315500251207882642406* &
                     (prob1-prob2)
            L32    = trm1*trm2 
       end function analytic_sol_L32_whole_atmos_wv5cm3m_f577_r4

       elemental function analytic_sol_L32_whole_atmos_wv5cm3m_f577_r8(fc,Nmf,beta,R0,z0,H20,Hc0) result(L32)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L32_whole_atmos_wv5cm3m_f577_r8
            !dir$ attributes forceinline :: analytic_sol_L32_whole_atmos_wv5cm3m_f577_r8
#endif 
!$omp declare simd(analytic_sol_L32_whole_atmos_wv5cm3m_f577_r8) 
            real(kind=dp),      intent(in) :: fc 
            real(kind=dp),      intent(in) :: Nmf 
            real(kind=dp),      intent(in) :: beta 
            real(kind=dp),      intent(in) :: R0 
            real(kind=dp),      intent(in) :: z0 
            real(kind=dp),      intent(in) :: H20 
            real(kind=dp),      intent(in) :: Hc0 
            real(kind=dp)                  :: L32 
            real(kind=dp),      parameter  :: C1253314137315500251207882642406 = & 
                                                  1.253314137315500251207882642406_dp
            real(kind=dp),      automatic  :: delNm, rat1 
            real(kind=dp),      automatic  :: earg, exp1 
            real(kind=dp),      automatic  :: tgz0, stgz0 
            real(kind=dp),      automatic  :: prob1, prob2 
            real(kind=dp),      automatic  :: p1arg, p2arg 
            real(kind=dp),      automatic  :: t0, t1 
            real(kind=dp),      automatic  :: sctgz0, trm1 
            real(kind=dp),      automatic  :: trm2
            tgz0   = tan(z0)
            stgz0  = tgz0*tgz0 
            t0     = 1.0_dp/tgz0 
            sctgz0 = t0*t0 
            delnNm = compute_delnM_f414_r8(fc,Nmf)
            t1     = R0/(stgz0+stgz0) 
            earg   = beta*(t1+H20)
            exp1   = exp(earg)
            t0     = beta*R0*sctgz0
            p1arg  = sqrt(t0+beta+beta*Hc0)
            p2arg  = sqrt(t0+beta+beta*H20)
            t1     = sqrt(beta*R0)/tgz0 
            trm1   = -delNm*t1*exp1 
            prob1  = prob_integral_r8(p1arg)
            prob2  = prob_integral_r8(p2arg)
            trm2   = C1253314137315500251207882642406* &
                     (prob1-prob2)
            L32    = trm1*trm2 
       end function analytic_sol_L32_whole_atmos_wv5cm3m_f577_r8

       !Formula: 5.78, page: 111
       elemental function analytic_sol_L33_whole_atmos_wv5cm3m_f578_r4(fc,Nmf,beta,R0,z0,H20,Hc0) result(L33)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L33_whole_atmos_wv5cm3m_f578_r4
            !dir$ attributes forceinline :: analytic_sol_L33_whole_atmos_wv5cm3m_f578_r4
#endif 
!$omp declare simd(analytic_sol_L33_whole_atmos_wv5cm3m_f578_r4) 
            real(kind=sp),      intent(in) :: fc 
            real(kind=sp),      intent(in) :: Nmf 
            real(kind=sp),      intent(in) :: beta 
            real(kind=sp),      intent(in) :: R0 
            real(kind=sp),      intent(in) :: z0 
            real(kind=sp),      intent(in) :: H20 
            real(kind=sp),      intent(in) :: Hc0 
            real(kind=sp)                  :: L32 
            real(kind=sp),      parameter  :: C1253314137315500251207882642406 = & 
                                                  1.253314137315500251207882642406_sp
            real(kind=sp),      automatic  :: delNm, rat1 
            real(kind=sp),      automatic  :: earg, exp1 
            real(kind=sp),      automatic  :: tgz0, stgz0 
            real(kind=sp),      automatic  :: prob1, prob2 
            real(kind=sp),      automatic  :: p1arg, p2arg 
            real(kind=sp),      automatic  :: t0, t1 
            real(kind=sp),      automatic  :: sctgz0, trm1 
            real(kind=sp),      automatic  :: trm2
            tgz0   = tan(z0)
            stgz0  = tgz0*tgz0 
            t0     = 1.0_sp/tgz0 
            sctgz0 = t0*t0 
            delnNm = compute_delnM_f414_r4(fc,Nmf)
            t1     = R0/stgz0 
            earg   = beta*(t1+H20)
            exp1   = exp(earg)
            t0     = (beta+beta)*R0*sctgz0
            p1arg  = sqrt(t0+4.0_sp*Hc0)
            p2arg  = sqrt(t0+4.0_sp*H20)
            t1     = sqrt((beta+beta)*R0)/tgz0 
            trm1   = -delNm*t1*exp1 
            prob1  = prob_integral_r4(p1arg)
            prob2  = prob_integral_r4(p2arg)
            trm2   = C1253314137315500251207882642406* &
                     (prob1-prob2)
            L32    = trm1*trm2 
       end function analytic_sol_L33_whole_atmos_wv5cm3m_f578_r4

       elemental function analytic_sol_L33_whole_atmos_wv5cm3m_f578_r8(fc,Nmf,beta,R0,z0,H20,Hc0) result(L33)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L33_whole_atmos_wv5cm3m_f578_r8
            !dir$ attributes forceinline :: analytic_sol_L33_whole_atmos_wv5cm3m_f578_r8
#endif 
!$omp declare simd(analytic_sol_L33_whole_atmos_wv5cm3m_f578_r8) 
            real(kind=dp),      intent(in) :: fc 
            real(kind=dp),      intent(in) :: Nmf 
            real(kind=dp),      intent(in) :: beta 
            real(kind=dp),      intent(in) :: R0 
            real(kind=dp),      intent(in) :: z0 
            real(kind=dp),      intent(in) :: H20 
            real(kind=dp),      intent(in) :: Hc0 
            real(kind=dp)                  :: L33 
            real(kind=dp),      parameter  :: C1253314137315500251207882642406 = & 
                                                  1.253314137315500251207882642406_dp
            real(kind=dp),      automatic  :: delNm, rat1 
            real(kind=dp),      automatic  :: earg, exp1 
            real(kind=dp),      automatic  :: tgz0, stgz0 
            real(kind=dp),      automatic  :: prob1, prob2 
            real(kind=dp),      automatic  :: p1arg, p2arg 
            real(kind=dp),      automatic  :: t0, t1 
            real(kind=dp),      automatic  :: sctgz0, trm1 
            real(kind=dp),      automatic  :: trm2
            tgz0   = tan(z0)
            stgz0  = tgz0*tgz0 
            t0     = 1.0_dp/tgz0 
            sctgz0 = t0*t0 
            delnNm = compute_delnM_f414_r8(fc,Nmf)
            t1     = R0/stgz0 
            earg   = beta*(t1+H20)
            exp1   = exp(earg)
            t0     = (beta+beta)*R0*sctgz0
            p1arg  = sqrt(t0+4.0_dp*Hc0)
            p2arg  = sqrt(t0+4.0_dp*H20)
            t1     = sqrt((beta+beta)*R0)/tgz0 
            trm1   = -delNm*t1*exp1 
            prob1  = prob_integral_r8(p1arg)
            prob2  = prob_integral_r8(p2arg)
            trm2   = C1253314137315500251207882642406* &
                     (prob1-prob2)
            L33    = trm1*trm2 
       end function analytic_sol_L33_whole_atmos_wv5cm3m_f578_r8

       !Formula: 5.79, page: 111
       elemental function analytic_sol_L34_whole_atmos_wv5cm3m_f579_r4(delnA,fc,Nmf,beta,R0,z0,H20,Hc0,Hc,H2) result(L31)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L34_whole_atmos_wv5cm3m_f579_r4
            !dir$ attributes forceinline :: analytic_sol_L34_whole_atmos_wv5cm3m_f579_r4
#endif 
!$omp declare simd(analytic_sol_L34_whole_atmos_wv5cm3m_f579_r4) 
            real(kind=sp),      intent(in) :: delnA 
            real(kind=sp),      intent(in) :: fc 
            real(kind=sp),      intent(in) :: Nmf 
            real(kind=sp),      intent(in) :: beta 
            real(kind=sp),      intent(in) ::  R0 
            real(kind=sp),      intent(in) :: z0 
            real(kind=sp),      intent(in) :: H20 
            real(kind=sp),      intent(in) :: Hc0 
            real(kind=sp),      intent(in) :: Hc 
            real(kind=sp),      intent(in) ::  H2
            real(kind=sp)                  :: L31 
            real(kind=sp),      automatic  :: t0, t1 
            real(kind=sp),      automatic  :: stgz0, ctgz0 
            real(kind=sp),      automatic  :: scosz0, earg 
            real(kind=sp),      automatic  :: exp1, sqr1 
            real(kind=sp),      automatic  :: trm1, trm2 
            real(kind=sp),      automatic  :: trm3, delNm
            real(kind=sp),      automatic  :: sqr2 
            earg  = beta*(Hc-H2)
            delNm = compute_delnM_f414_r4(fc,Nmf)
            t1    = tan(z0)
            ctgz0 = 1.0_sp/t1
            stgz0 = t1*t1 
            t0    = cos(z0)
            scosz0= t0*t0 
            trm1  = delnA*delNm*beta*R0*(ctgz0/scosz0)
            t0    = H20/R0 
            t1    = 2.0_sp*stgz0 
            sqr1  = sqrt(1.0_sp+t1*t0)
            trm2  = 1.0_sp/sqr1
            exp1  = exp(earg)
            t0    = Hc0/R0
            sqr2  = sqrt(1.0_sp+t1*t0) 
            trm3  = exp1/sqr2 
            L13   = trm1*(trm2-trm3)
      end function analytic_sol_L34_whole_atmos_wv5cm3m_f579_r4

      elemental function analytic_sol_L34_whole_atmos_wv5cm3m_f579_r8(delnA,fc,Nmf,beta,R0,z0,H20,Hc0,Hc,H2) result(L31)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_L34_whole_atmos_wv5cm3m_f579_r8
            !dir$ attributes forceinline :: analytic_sol_L34_whole_atmos_wv5cm3m_f579_r4
#endif 
!$omp declare simd(analytic_sol_L34_whole_atmos_wv5cm3m_f579_r4) 
            real(kind=dp),      intent(in) :: delnA 
            real(kind=dp),      intent(in) :: fc 
            real(kind=dp),      intent(in) :: Nmf 
            real(kind=dp),      intent(in) :: beta 
            real(kind=dp),      intent(in) ::  R0 
            real(kind=dp),      intent(in) :: z0 
            real(kind=dp),      intent(in) :: H20 
            real(kind=dp),      intent(in) :: Hc0 
            real(kind=dp),      intent(in) :: Hc 
            real(kind=dp),      intent(in) ::  H2
            real(kind=dp)                  :: L31 
            real(kind=dp),      automatic  :: t0, t1 
            real(kind=dp),      automatic  :: stgz0, ctgz0 
            real(kind=dp),      automatic  :: scosz0, earg 
            real(kind=dp),      automatic  :: exp1, sqr1 
            real(kind=dp),      automatic  :: trm1, trm2 
            real(kind=dp),      automatic  :: trm3, delNm
            real(kind=dp),      automatic  :: sqr2 
            earg  = beta*(Hc-H2)
            delNm = compute_delnM_f414_r8(fc,Nmf)
            t1    = tan(z0)
            ctgz0 = 1.0_dp/t1
            stgz0 = t1*t1 
            t0    = cos(z0)
            scosz0= t0*t0 
            trm1  = delnA*delNm*beta*R0*(ctgz0/scosz0)
            t0    = H20/R0 
            t1    = 2.0_dp*stgz0 
            sqr1  = sqrt(1.0_dp+t1*t0)
            trm2  = 1.0_dp/sqr1
            exp1  = exp(earg)
            t0    = Hc0/R0
            sqr2  = sqrt(1.0_dp+t1*t0) 
            trm3  = exp1/sqr2 
            L13   = trm1*(trm2-trm3)
      end function analytic_sol_L34_whole_atmos_wv5cm3m_f579_r8

      !Рефракция радиоволн (5 см < X < 3 м)
      !в земной атмосфере
      !при близких или равных высотах
      !излучателя и приемника.
      !Излучатель и приемник находятся в нижней 
      !ионосфере, причем п (h) меняется с высотой по формуле
      !(4.30).
      !Formula: 5.90, page: 115
      elemental function refraction_angle_lo_ionospere_wv5cm3m_f590_r4(fc,Nmf,R0,thtc,z0,H2,H1) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_lo_ionospere_wv5cm3m_f590_r4
            !dir$ attributes forceinline :: refraction_angle_lo_ionospere_wv5cm3m_f590_r4
#endif 
!$omp declare simd(refraction_angle_lo_ionospere_wv5cm3m_f590_r4)
            real(kind=sp),     intent(in)  :: fc 
            real(kind=sp),     intent(in)  :: Nmf 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: thtc 
            real(kind=sp),     intent(in)  :: z0 
            real(kind=sp),     intent(in)  :: H2 
            real(kind=sp),     intent(in)  :: H1 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     automatic   :: delNm, sH2H1
            real(kind=sp),     automatic   :: thtc2, thtc4 
            real(kind=sp),     automatic   :: t0, t1. t2  
            real(kind=sp),     automatic   :: ctgz0, sctgz0
            real(kind=sp),     automatic   :: sctgz0h, trm1 
            real(kind=sp),     automatic   :: trm2, trm3 
            real(kind=sp),     automatic   :: R2 
            t0     = H2-H1 
            sH2H1  = t0*t0 
            t1     = tan(z0)
            R2     = 6397.0_sp+H2 
            ctgz0  = 1.0_sp/t1 
            thtc2  = tht*tht 
            sctgz0 = ctgz0*ctgz0 
            thtc4  = thtc2*thtc2 
            t0     = compute_delnM_f414_r4(fc,Nmf)
            delNm  = t0*t0 
            t1     = R0*thtc 
            trm1   = ((delNm+delNm)/sH2H1)*t1 
            sctgz0h= sctgz0+0.5_sp 
            t0     = 1.0_sp+ctgz0*(0.5_sp*thtc)
            t1     = sctgz0h*(thtc2*0.333333333333333333333333333_sp)
            trm2   = R2*(t0+t1)
            t2     = 1.0_sp+ctgz0+thtc 
            t1     = (sctgz0+0.333333333333333333333333_sp)*thtc2
            t0     = sctgz0+sctgz0*sctgz0h 
            trm2   = t2+t1+t0 
            trm3   = trm2+(sctgz0h*sctgz0h)*(tht4*0.2_sp)
            alpha  = R2*trm3  
      end function refraction_angle_lo_ionospere_wv5cm3m_f590_r4

  elemental function refraction_angle_lo_ionospere_wv5cm3m_f590_r8(fc,Nmf,R0,thtc,z0,H2,H1) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_lo_ionospere_wv5cm3m_f590_r8
            !dir$ attributes forceinline :: refraction_angle_lo_ionospere_wv5cm3m_f590_r8
#endif 
!$omp declare simd(refraction_angle_lo_ionospere_wv5cm3m_f590_r8)
            real(kind=dp),     intent(in)  :: fc 
            real(kind=dp),     intent(in)  :: Nmf 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: thtc 
            real(kind=dp),     intent(in)  :: z0 
            real(kind=dp),     intent(in)  :: H2 
            real(kind=dp),     intent(in)  :: H1 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     automatic   :: delNm, sH2H1
            real(kind=dp),     automatic   :: thtc2, thtc4 
            real(kind=dp),     automatic   :: t0, t1. t2  
            real(kind=dp),     automatic   :: ctgz0, sctgz0
            real(kind=dp),     automatic   :: sctgz0h, trm1 
            real(kind=dp),     automatic   :: trm2, trm3 
            real(kind=dp),     automatic   :: R2 
            t0     = H2-H1 
            sH2H1  = t0*t0 
            t1     = tan(z0)
            R2     = 6397.0_dp+H2 
            ctgz0  = 1.0_dp/t1 
            thtc2  = tht*tht 
            sctgz0 = ctgz0*ctgz0 
            thtc4  = thtc2*thtc2 
            t0     = compute_delnM_f414_r8(fc,Nmf)
            delNm  = t0*t0 
            t1     = R0*thtc 
            trm1   = ((delNm+delNm)/sH2H1)*t1 
            sctgz0h= sctgz0+0.5_dp 
            t0     = 1.0_dp+ctgz0*(0.5_dp*thtc)
            t1     = sctgz0h*(thtc2*0.333333333333333333333333333_dp)
            trm2   = R2*(t0+t1)
            t2     = 1.0_dp+ctgz0+thtc 
            t1     = (sctgz0+0.333333333333333333333333_dp)*thtc2
            t0     = sctgz0+sctgz0*sctgz0h 
            trm2   = t2+t1+t0 
            trm3   = trm2+(sctgz0h*sctgz0h)*(tht4*0.2_dp)
            alpha  = R2*trm3  
      end function refraction_angle_lo_ionospere_wv5cm3m_f590_r8

      ! При равных высотах излучателя и приемника 
      !величины, стоящие в квадратных скобках (5.90), близки к 1,
      !и в этом случае угол рефракции 'а определяется 
      !соотношением
      !Formula: 5.91, page: 115
      elemental function refraction_angle_lo_ionospere_wv5cm3m_f591_r4(fc,Nmf,H2,H1,thtc,R0,R2) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_lo_ionospere_wv5cm3m_f591_r4
            !dir$ attributes forceinline :: refraction_angle_lo_ionospere_wv5cm3m_f591_r4
#endif 
!$omp declare simd(refraction_angle_lo_ionospere_wv5cm3m_f591_r4)
            real(kind=sp),     intent(in)  :: fc 
            real(kind=sp),     intent(in)  :: Nmf 
            real(kind=sp),     intent(in)  :: H2 
            real(kind=sp),     intent(in)  :: H1 
            real(kind=sp),     intent(in)  :: thtc 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: R2 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     automatic   :: delNm, sH2H1 
            real(kind=sp),     automatic   :: R2R0 
            real(kind=sp),     automatic   :: L, G0 
            real(kind=sp),     automatic   :: t0
            t0   = H2-H1
            sH2H1= t0*t0 
            R2R0 = R2-R0 
            delNm= compute_delnM_f414_r4(fc,Nmf)
            L    = thtc*R0 
            G0   = ((delNm+delNm)/sH2H1)*R2R0
            alpha= G0*L 
      end function refraction_angle_lo_ionospere_wv5cm3m_f591_r4

       elemental function refraction_angle_lo_ionospere_wv5cm3m_f591_r8(fc,Nmf,H2,H1,thtc,R0,R2) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_lo_ionospere_wv5cm3m_f591_r8
            !dir$ attributes forceinline :: refraction_angle_lo_ionospere_wv5cm3m_f591_r8
#endif 
!$omp declare simd(refraction_angle_lo_ionospere_wv5cm3m_f591_r8)
            real(kind=dp),     intent(in)  :: fc 
            real(kind=dp),     intent(in)  :: Nmf 
            real(kind=dp),     intent(in)  :: H2 
            real(kind=dp),     intent(in)  :: H1 
            real(kind=dp),     intent(in)  :: thtc 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: R2 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     automatic   :: delNm, sH2H1 
            real(kind=dp),     automatic   :: R2R0 
            real(kind=dp),     automatic   :: L, G0 
            real(kind=dp),     automatic   :: t0
            t0   = H2-H1
            sH2H1= t0*t0 
            R2R0 = R2-R0 
            delNm= compute_delnM_f414_r8(fc,Nmf)
            L    = thtc*R0 
            G0   = ((delNm+delNm)/sH2H1)*R2R0
            alpha= G0*L 
      end function refraction_angle_lo_ionospere_wv5cm3m_f591_r8

      !Приемник и излучатель радиоволн помещены в
      !верхней ионосфере в точках А и С (рис. 5.1). 
      !Показатель преломления верхней ионосферы 
      !экспоненциально возрастает с высотой по закону (4.31).
      !Formula 5.93, page: 116
      elemental function refraction_angle_lo_ionospere_wv5cm3m_f593_r4(fc,Nmf,beta,R2,R0,z0,thtc) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_lo_ionospere_wv5cm3m_f593_r4
            !dir$ attributes forceinline :: refraction_angle_lo_ionospere_wv5cm3m_f593_r4
#endif 
!$omp declare simd(refraction_angle_lo_ionospere_wv5cm3m_f593_r4)
            real(kind=sp),     intent(in)  :: fc 
            real(kind=sp),     intent(in)  :: Nmf 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: R2
            real(kind=sp),     intent(in)  :: z0 
            real(kind=sp),     intent(in)  :: thtc 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     parameter   :: C314159265358979323846264338328 = & 
                                                 3.14159265358979323846264338328_sp  
            real(kind=sp),     automatic   :: L,p 
            real(kind=sp),     automatic   :: q,u1 
            real(kind=sp),     automatic   :: u2, ctgz0 
            real(kind=sp),     automatic   :: sctgz0 
            real(kind=sp),     automatic   :: btR0p, delNma 
            real(kind=sp),     automatic   :: t0, t1 
            real(kind=sp),     automatic   :: t2, t3 
            real(kind=sp),     automatic   :: trm1, trm2 
            real(kind=sp),     automatic   :: prob1, prob2 
            real(kind=sp),     automatic   :: tbtR0, sqr1  
            real(kind=sp),     automatic   :: sqr2, exp1, exp2  
            tbtR0    = 2.0_sp*beta*R0 
            t0       = compute_delnM_f414_r4(fc,Nmf)
            delNma   = t0*exp(beta*(R2-R0))
            t1       = tan(z0)
            ctgz0    = 1.0_sp/t1 
            sctgz0   = ctgz0*ctgz0 
            sqr1      = sqrt(0.5_sp+sctgz0)
            q        = sqr 
            p        = ctgz0/(sqr1+sqr1)
            btR0p    = beta*R0*p*p 
            t0       = (delNma*beta*R0)/q
            t1       = exp(btR0p)
            trm1     = t0*t1 
            sqr2     = sqrt((beta+beta)*R0)
            u1       = p*sqr2 
            u2       = sqr2*(p+thtc*q)
            prob1    = prob_integral_r4(u1)
            prob2    = prob_integral_r4(u2)
            t2       = C314159265358979323846264338328/(beta*R0)
            t1       = 1.0_sp/((beta+beta)*R0)
            t3       = 1.0_sp-p*p
            trm2     = t3+t1*t2*(prob2-prob1)
            t0       = p/tbtR0 
            exp1     = exp(-btR0p)
            t1       = p+q*thtc
            t2       = t1/tbtR0 
            exp2     = exp(-beta*R0*t1*t1)
            L        = t0*exp1-t2*exp2 
            alpha    = trm1*(L+0.5_sp)*trm2
      end function refraction_angle_lo_ionospere_wv5cm3m_f593_r4 

        elemental function refraction_angle_lo_ionospere_wv5cm3m_f593_r8(fc,Nmf,beta,R2,R0,z0,thtc) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_lo_ionospere_wv5cm3m_f593_r8
            !dir$ attributes forceinline :: refraction_angle_lo_ionospere_wv5cm3m_f593_r8
#endif 
!$omp declare simd(refraction_angle_lo_ionospere_wv5cm3m_f593_r8)
            real(kind=dp),     intent(in)  :: fc 
            real(kind=dp),     intent(in)  :: Nmf 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: R2
            real(kind=dp),     intent(in)  :: z0 
            real(kind=dp),     intent(in)  :: thtc 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     parameter   :: C314159265358979323846264338328 = & 
                                                 3.14159265358979323846264338328_dp  
            real(kind=dp),     automatic   :: L,p 
            real(kind=dp),     automatic   :: q,u1 
            real(kind=dp),     automatic   :: u2, ctgz0 
            real(kind=dp),     automatic   :: sctgz0 
            real(kind=dp),     automatic   :: btR0p, delNma 
            real(kind=dp),     automatic   :: t0, t1 
            real(kind=dp),     automatic   :: t2, t3 
            real(kind=dp),     automatic   :: trm1, trm2 
            real(kind=dp),     automatic   :: prob1, prob2 
            real(kind=dp),     automatic   :: tbtR0, sqr1  
            real(kind=dp),     automatic   :: sqr2, exp1, exp2  
            tbtR0    = 2.0_dp*beta*R0 
            t0       = compute_delnM_f414_r8(fc,Nmf)
            delNma   = t0*exp(beta*(R2-R0))
            t1       = tan(z0)
            ctgz0    = 1.0_dp/t1 
            sctgz0   = ctgz0*ctgz0 
            sqr1      = sqrt(0.5_dp+sctgz0)
            q        = sqr 
            p        = ctgz0/(sqr1+sqr1)
            btR0p    = beta*R0*p*p 
            t0       = (delNma*beta*R0)/q
            t1       = exp(btR0p)
            trm1     = t0*t1 
            sqr2     = sqrt((beta+beta)*R0)
            u1       = p*sqr2 
            u2       = sqr2*(p+thtc*q)
            prob1    = prob_integral_r8(u1)
            prob2    = prob_integral_r8(u2)
            t2       = C314159265358979323846264338328/(beta*R0)
            t1       = 1.0_dp/((beta+beta)*R0)
            t3       = 1.0_dp-p*p
            trm2     = t3+t1*t2*(prob2-prob1)
            t0       = p/tbtR0 
            exp1     = exp(-btR0p)
            t1       = p+q*thtc
            t2       = t1/tbtR0 
            exp2     = exp(-beta*R0*t1*t1)
            L        = t0*exp1-t2*exp2 
            alpha    = trm1*(L+0.5_dp)*trm2
      end function refraction_angle_lo_ionospere_wv5cm3m_f593_r8

      !При равных .высотах излучателя и приемника соот->
      !ношение (5.93) упрощается (см. § 5.2) и принимает:
      !Formula: 5.95, page: 116
      elemental function refraction_angle_lo_ionospere_wv5cm3m_f595_r4(delnA,beta,R0,thtc) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_lo_ionospere_wv5cm3m_f595_r4
            !dir$ attributes forceinline :: refraction_angle_lo_ionospere_wv5cm3m_f595_r4
#endif 
!$omp declare simd(refraction_angle_lo_ionospere_wv5cm3m_f595_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: thtc 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     automatic   :: t0, t1 
            t0   = delnA*beta 
            t1   = R0*thtc
            alpha= t0*t1 
      end function refraction_angle_lo_ionospere_wv5cm3m_f595_r4

      elemental function refraction_angle_lo_ionospere_wv5cm3m_f595_r8(delnA,beta,R0,thtc) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_lo_ionospere_wv5cm3m_f595_r8
            !dir$ attributes forceinline :: refraction_angle_lo_ionospere_wv5cm3m_f595_r8
#endif 
!$omp declare simd(refraction_angle_lo_ionospere_wv5cm3m_f595_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: thtc 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     automatic   :: t0, t1 
            t0   = delnA*beta 
            t1   = R0*thtc
            alpha= t0*t1 
      end function refraction_angle_lo_ionospere_wv5cm3m_f595_r8

      !Рассмотрим метод расчета угла планетной рефракции а
      !(рис. 6.1) для электромагнитных волн диапазона X <;
      !< 5 см.
      !Formula: 6.3, page: 119
      elemental function analytic_sol_LB1_whole_atmos_wvl5cm_f63_r4(delnA,beta,R0,HB,H0) result(LB1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_LB1_whole_atmos_wvl5cm_f63_r4
            !dir$ attributes forceinline :: analytic_sol_LB1_whole_atmos_wvl5cm_f63_r4
#endif 
!$omp declare simd(analytic_sol_LB1_whole_atmos_wvl5cm_f63_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HB 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: LB1 
            real(kind=sp),     automatic   :: sdelnA, HB0 
            real(kind=sp),     automatic   :: btHb0, exp1 
            real(kind=sp),     automatic   :: exp2, sqr 
            real(kind=sp),     automatic   :: t0, t1 
            real(kind=sp),     automatic   :: trm1, trm2 
            HB0    = HB-H0 
            sdelnA = delnA*delnA 
            t0     = HB0+HB0 
            t1     = R0/t0 
            btHb0  = bt*HB0 
            sqr    = sqrt(t1)
            trm1   = -sdelnA*beta*R0 
            exp1   = exp(-btHb0)
            exp2   = exp(-(btHb0+btHb0))
            trm2   = sqr*(exp1-exp2)
            LB1    = trm1*trm2  
      end function analytic_sol_LB1_whole_atmos_wvl5cm_f63_r4

      elemental function analytic_sol_LB1_whole_atmos_wvl5cm_f63_r8(delnA,beta,R0,HB,H0) result(LB1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_LB1_whole_atmos_wvl5cm_f63_r8
            !dir$ attributes forceinline :: analytic_sol_LB1_whole_atmos_wvl5cm_f63_r8
#endif 
!$omp declare simd(analytic_sol_LB1_whole_atmos_wvl5cm_f63_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 ! R0=a + H0 — расстояние точки А от центра zemli.
            real(kind=dp),     intent(in)  :: HB 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: LB1 
            real(kind=dp),     automatic   :: sdelnA, HB0 
            real(kind=dp),     automatic   :: btHb0, exp1 
            real(kind=dp),     automatic   :: exp2, sqr 
            real(kind=dp),     automatic   :: t0, t1 
            real(kind=dp),     automatic   :: trm1, trm2 
            HB0    = HB-H0 
            sdelnA = delnA*delnA 
            t0     = HB0+HB0 
            t1     = R0/t0 
            btHb0  = bt*HB0 
            sqr    = sqrt(t1)
            trm1   = -sdelnA*beta*R0 
            exp1   = exp(-btHb0)
            exp2   = exp(-(btHb0+btHb0))
            trm2   = sqr*(exp1-exp2)
            LB1    = trm1*trm2  
      end function analytic_sol_LB1_whole_atmos_wvl5cm_f63_r8

      !Formula: 6.4, page: 120
       elemental function analytic_sol_LB2_whole_atmos_wvl5cm_f64_r4(delnA,beta,R0,HB,H0) result(LB2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_LB2_whole_atmos_wvl5cm_f64_r4
            !dir$ attributes forceinline :: analytic_sol_LB2_whole_atmos_wvl5cm_f64_r4
#endif 
!$omp declare simd(analytic_sol_LB2_whole_atmos_wvl5cm_f64_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HB 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: LB2 
            real(kind=sp),     parameter   :: C314159265358979323846264338328 = &
                                                 3.14159265358979323846264338328_sp
            real(kind=sp),     automatic   :: btR0, HBH0 
            real(kind=sp),     automatic   :: sqr,  prob 
            real(kind=sp),     automatic   :: t0,   t1 
            HBH0  = HB-H0 
            btR0  = beta*R0 
            t0    = beta+HBH0 
            t1    = 0.5_sp*(C314159265358979323846264338328*btR0)
            sqr   = sqrt(t1)
            prob  = prob_integral_r4(sqrt(t0+t0))
            LB2   = delnA*sqr*prob 
       end function analytic_sol_LB2_whole_atmos_wvl5cm_f64_r4

       elemental function analytic_sol_LB2_whole_atmos_wvl5cm_f64_r8(delnA,beta,R0,HB,H0) result(LB2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_LB2_whole_atmos_wvl5cm_f64_r8
            !dir$ attributes forceinline :: analytic_sol_LB2_whole_atmos_wvl5cm_f64_r8
#endif 
!$omp declare simd(analytic_sol_LB2_whole_atmos_wvl5cm_f64_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HB 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: LB2 
            real(kind=dp),     parameter   :: C314159265358979323846264338328 = &
                                                 3.14159265358979323846264338328_dp
            real(kind=dp),     automatic   :: btR0, HBH0 
            real(kind=dp),     automatic   :: sqr,  prob 
            real(kind=dp),     automatic   :: t0,   t1 
            HBH0  = HB-H0 
            btR0  = beta*R0 
            t0    = beta+HBH0 
            t1    = 0.5_dp*(C314159265358979323846264338328*btR0)
            sqr   = sqrt(t1)
            prob  = prob_integral_r4(sqrt(t0+t0))
            LB2   = delnA*sqr*prob 
       end function analytic_sol_LB2_whole_atmos_wvl5cm_f64_r8

       !Formula: 6.5, page: 120
       elemental function analytic_sol_LB3_whole_atmos_wvl5cm_f65_r4(delnA,beta,R0,HB,H0) result(LB3)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_LB3_whole_atmos_wvl5cm_f65_r4
            !dir$ attributes forceinline :: analytic_sol_LB3_whole_atmos_wvl5cm_f65_r4
#endif 
!$omp declare simd(analytic_sol_LB3_whole_atmos_wvl5cm_f65_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HB 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: LB3 
            real(kind=sp),     parameter   :: C314159265358979323846264338328 = &
                                                 3.14159265358979323846264338328_sp
            real(kind=sp),     parameter   :: C141421356237309504880168872421 = &
                                                 1.41421356237309504880168872421_sp
            real(kind=sp),     automatic   :: btR0, HBH0 
            real(kind=sp),     automatic   :: sqr,  prob1 
            real(kind=sp),     automatic   :: t0,   t1 
            real(kind=sp),     automatic   :: prob2,sdelnA
            real(kind=sp),     automatic   :: trm1, trm2 
            HBH0  = HB-H0 
            btR0  = beta*R0 
            sdelnA= delnA*delnA 
            t1    = 0.5_sp*(C314159265358979323846264338328*btR0)
            sqr   = sqrt(t1)
            trm1  = sdelnA*btR0*t1 
            t0    = sqrt(4.0_sp*(beta*HBH0))
            t1    = sqrt(2.0_sp*(beta*HBH0))
            prob1 = prob_integral_r4(t0)
            prob2 = prob_integral_r4(t1)
            trm2  = C141421356237309504880168872421*(prob1-prob2)
            LB3   = trm1*trm2 
       end function analytic_sol_LB3_whole_atmos_wvl5cm_f65_r4

        elemental function analytic_sol_LB3_whole_atmos_wvl5cm_f65_r8(delnA,beta,R0,HB,H0) result(LB3)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_LB3_whole_atmos_wvl5cm_f65_r8
            !dir$ attributes forceinline :: analytic_sol_LB3_whole_atmos_wvl5cm_f65_r8
#endif 
!$omp declare simd(analytic_sol_LB3_whole_atmos_wvl5cm_f65_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HB 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: LB3 
            real(kind=dp),     parameter   :: C314159265358979323846264338328 = &
                                                 3.14159265358979323846264338328_dp
            real(kind=dp),     parameter   :: C141421356237309504880168872421 = &
                                                 1.41421356237309504880168872421_dp
            real(kind=dp),     automatic   :: btR0, HBH0 
            real(kind=dp),     automatic   :: sqr,  prob1 
            real(kind=dp),     automatic   :: t0,   t1 
            real(kind=dp),     automatic   :: prob2,sdelnA
            real(kind=dp),     automatic   :: trm1, trm2 
            HBH0  = HB-H0 
            btR0  = beta*R0 
            sdelnA= delnA*delnA 
            t1    = 0.5_dp*(C314159265358979323846264338328*btR0)
            sqr   = sqrt(t1)
            trm1  = sdelnA*btR0*t1 
            t0    = sqrt(4.0_dp*(beta*HBH0))
            t1    = sqrt(2.0_dp*(beta*HBH0))
            prob1 = prob_integral_r8(t0)
            prob2 = prob_integral_r8(t1)
            trm2  = C141421356237309504880168872421*(prob1-prob2)
            LB3   = trm1*trm2 
       end function analytic_sol_LB3_whole_atmos_wvl5cm_f65_r8

       !Formula: 6.2, page: 119
       elemental function refraction_angle_B_whole_atmos_vwl5cm_f62_r4(delnA,beta,R0,HB,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_B_whole_atmos_vwl5cm_f62_r4
            !dir$ attributes forceinline :: refraction_angle_B_whole_atmos_vwl5cm_f62_r4
#endif 
!$omp declare simd(refraction_angle_B_whole_atmos_vwl5cm_f62_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HB 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     automatic   :: LB1, LB2, LB3 
            LB1  = analytic_sol_LB1_whole_atmos_wvl5cm_f63_r4(delnA,beta,R0,HB,H0)
            LB2  = analytic_sol_LB2_whole_atmos_wvl5cm_f64_r4(delnA,beta,R0,HB,H0)
            LB3  = analytic_sol_LB3_whole_atmos_wvl5cm_f65_r4(delnA,beta,R0,HB,H0)
            alpha= LB1+LB2+LB3 
       end function refraction_angle_B_whole_atmos_vwl5cm_f62_r4

       elemental function refraction_angle_B_whole_atmos_vwl5cm_f62_r8(delnA,beta,R0,HB,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_B_whole_atmos_vwl5cm_f62_r8
            !dir$ attributes forceinline :: refraction_angle_B_whole_atmos_vwl5cm_f62_r8
#endif 
!$omp declare simd(refraction_angle_B_whole_atmos_vwl5cm_f62_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HB 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     automatic   :: LB1, LB2, LB3 
            LB1  = analytic_sol_LB1_whole_atmos_wvl5cm_f63_r8(delnA,beta,R0,HB,H0)
            LB2  = analytic_sol_LB2_whole_atmos_wvl5cm_f64_r8(delnA,beta,R0,HB,H0)
            LB3  = analytic_sol_LB3_whole_atmos_wvl5cm_f65_r8(delnA,beta,R0,HB,H0)
            alpha= LB1+LB2+LB3 
       end function refraction_angle_B_whole_atmos_vwl5cm_f62_r8

       !Formula: 6.7, page: 120
        elemental function analytic_sol_LC1_whole_atmos_wvl5cm_f67_r4(delnA,beta,R0,HC,H0) result(LC1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_LC1_whole_atmos_wvl5cm_f67_r4
            !dir$ attributes forceinline :: analytic_sol_LC1_whole_atmos_wvl5cm_f67_r4
#endif 
!$omp declare simd(analytic_sol_LC1_whole_atmos_wvl5cm_f63_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HC 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: LC1 
            real(kind=sp),     automatic   :: sdelnA, HCH0 
            real(kind=sp),     automatic   :: btHb0, exp1 
            real(kind=sp),     automatic   :: exp2, sqr 
            real(kind=sp),     automatic   :: t0, t1 
            real(kind=sp),     automatic   :: trm1, trm2 
            HCH0    = HC-H0 
            sdelnA = delnA*delnA 
            t0     = HCH0+HCH0 
            t1     = R0/t0 
            btHb0  = bt*HB0 
            sqr    = sqrt(t1)
            trm1   = -sdelnA*beta*R0 
            exp1   = exp(-btHb0)
            exp2   = exp(-(btHb0+btHb0))
            trm2   = sqr*(exp1-exp2)
            LC1    = trm1*trm2  
      end function analytic_sol_LC1_whole_atmos_wvl5cm_f67_r4

       elemental function analytic_sol_LC1_whole_atmos_wvl5cm_f67_r8(delnA,beta,R0,HC,H0) result(LC1)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_LC1_whole_atmos_wvl5cm_f67_r8
            !dir$ attributes forceinline :: analytic_sol_LC1_whole_atmos_wvl5cm_f67_r8
#endif 
!$omp declare simd(analytic_sol_LC1_whole_atmos_wvl5cm_f63_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HC 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: LC1 
            real(kind=dp),     automatic   :: sdelnA, HCH0 
            real(kind=dp),     automatic   :: btHb0, exp1 
            real(kind=dp),     automatic   :: exp2, sqr 
            real(kind=dp),     automatic   :: t0, t1 
            real(kind=dp),     automatic   :: trm1, trm2 
            HCH0    = HC-H0 
            sdelnA = delnA*delnA 
            t0     = HCH0+HCH0 
            t1     = R0/t0 
            btHb0  = bt*HCH0 
            sqr    = sqrt(t1)
            trm1   = -sdelnA*beta*R0 
            exp1   = exp(-btHb0)
            exp2   = exp(-(btHb0+btHb0))
            trm2   = sqr*(exp1-exp2)
            LC1    = trm1*trm2  
      end function analytic_sol_LC1_whole_atmos_wvl5cm_f67_r8

      !Formula: 6.8, page: 120
        elemental function analytic_sol_LC2_whole_atmos_wvl5cm_f68_r4(delnA,beta,R0,HC,H0) result(LC2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_LC2_whole_atmos_wvl5cm_f68_r4
            !dir$ attributes forceinline :: analytic_sol_LC2_whole_atmos_wvl5cm_f68_r4
#endif 
!$omp declare simd(analytic_sol_LC2_whole_atmos_wvl5cm_f68_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HC
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: LC2 
            real(kind=sp),     parameter   :: C314159265358979323846264338328 = &
                                                 3.14159265358979323846264338328_sp
            real(kind=sp),     automatic   :: btR0, HCH0 
            real(kind=sp),     automatic   :: sqr,  prob 
            real(kind=sp),     automatic   :: t0,   t1 
            HCH0  = HC-H0 
            btR0  = beta*R0 
            t0    = beta+HCH0 
            t1    = 0.5_sp*(C314159265358979323846264338328*btR0)
            sqr   = sqrt(t1)
            prob  = prob_integral_r4(sqrt(t0+t0))
            LB2   = delnA*sqr*prob 
       end function analytic_sol_LC2_whole_atmos_wvl5cm_f68_r4

       elemental function analytic_sol_LC2_whole_atmos_wvl5cm_f68_r8(delnA,beta,R0,HC,H0) result(LC2)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_LC2_whole_atmos_wvl5cm_f68_r8
            !dir$ attributes forceinline :: analytic_sol_LC2_whole_atmos_wvl5cm_f68_r8
#endif 
!$omp declare simd(analytic_sol_LC2_whole_atmos_wvl5cm_f68_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HC
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: LC2 
            real(kind=dp),     parameter   :: C314159265358979323846264338328 = &
                                                 3.14159265358979323846264338328_dp
            real(kind=dp),     automatic   :: btR0, HCH0 
            real(kind=dp),     automatic   :: sqr,  prob 
            real(kind=dp),     automatic   :: t0,   t1 
            HCH0  = HC-H0 
            btR0  = beta*R0 
            t0    = beta+HCH0 
            t1    = 0.5_dp*(C314159265358979323846264338328*btR0)
            sqr   = sqrt(t1)
            prob  = prob_integral_r8(sqrt(t0+t0))
            LB2   = delnA*sqr*prob 
       end function analytic_sol_LC2_whole_atmos_wvl5cm_f68_r8

       !Formula: 6.9, page: 120
       elemental function analytic_sol_LC3_whole_atmos_wvl5cm_f69_r4(delnA,beta,R0,HC,H0) result(LC3)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_LC3_whole_atmos_wvl5cm_f69_r4
            !dir$ attributes forceinline :: analytic_sol_LC3_whole_atmos_wvl5cm_f69_r4
#endif 
!$omp declare simd(analytic_sol_LC3_whole_atmos_wvl5cm_f69_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HC 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: LB3 
            real(kind=sp),     parameter   :: C314159265358979323846264338328 = &
                                                 3.14159265358979323846264338328_sp
            real(kind=sp),     parameter   :: C141421356237309504880168872421 = &
                                                 1.41421356237309504880168872421_sp
            real(kind=sp),     automatic   :: btR0, HCH0 
            real(kind=sp),     automatic   :: sqr,  prob1 
            real(kind=sp),     automatic   :: t0,   t1 
            real(kind=sp),     automatic   :: prob2,sdelnA
            real(kind=sp),     automatic   :: trm1, trm2 
            HCH0  = HC-H0 
            btR0  = beta*R0 
            sdelnA= delnA*delnA 
            t1    = 0.5_sp*(C314159265358979323846264338328*btR0)
            sqr   = sqrt(t1)
            trm1  = sdelnA*btR0*t1 
            t0    = sqrt(4.0_sp*(beta*HCH0))
            t1    = sqrt(2.0_sp*(beta*HCH0))
            prob1 = prob_integral_r4(t0)
            prob2 = prob_integral_r4(t1)
            trm2  = C141421356237309504880168872421*(prob1-prob2)
            LB3   = trm1*trm2 
       end function analytic_sol_LC3_whole_atmos_wvl5cm_f69_r4

        elemental function analytic_sol_LC3_whole_atmos_wvl5cm_f69_r8(delnA,beta,R0,HC,H0) result(LC3)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: analytic_sol_LC3_whole_atmos_wvl5cm_f69_r8
            !dir$ attributes forceinline :: analytic_sol_LC3_whole_atmos_wvl5cm_f69_r8
#endif 
!$omp declare simd(analytic_sol_LC3_whole_atmos_wvl5cm_f69_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HC 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: LB3 
            real(kind=dp),     parameter   :: C314159265358979323846264338328 = &
                                                 3.14159265358979323846264338328_dp
            real(kind=dp),     parameter   :: C141421356237309504880168872421 = &
                                                 1.41421356237309504880168872421_dp
            real(kind=dp),     automatic   :: btR0, HCH0 
            real(kind=dp),     automatic   :: sqr,  prob1 
            real(kind=dp),     automatic   :: t0,   t1 
            real(kind=dp),     automatic   :: prob2,sdelnA
            real(kind=dp),     automatic   :: trm1, trm2 
            HCH0  = HC-H0 
            btR0  = beta*R0 
            sdelnA= delnA*delnA 
            t1    = 0.5_dp*(C314159265358979323846264338328*btR0)
            sqr   = sqrt(t1)
            trm1  = sdelnA*btR0*t1 
            t0    = sqrt(4.0_dp*(beta*HCH0))
            t1    = sqrt(2.0_dp*(beta*HCH0))
            prob1 = prob_integral_r8(t0)
            prob2 = prob_integral_r8(t1)
            trm2  = C141421356237309504880168872421*(prob1-prob2)
            LB3   = trm1*trm2 
       end function analytic_sol_LC3_whole_atmos_wvl5cm_f69_r8

       !Formula: 6.6, page: 120
        elemental function refraction_angle_C_whole_atmos_vwl5cm_f66_r4(delnA,beta,R0,HC,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_C_whole_atmos_vwl5cm_f66_r4
            !dir$ attributes forceinline :: refraction_angle_C_whole_atmos_vwl5cm_f66_r4
#endif 
!$omp declare simd(refraction_angle_C_whole_atmos_vwl5cm_f66_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HC 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     automatic   :: LC1, LC2, LC3 
            LC1  = analytic_sol_LC1_whole_atmos_wvl5cm_f63_r4(delnA,beta,R0,HC,H0)
            LC2  = analytic_sol_LC2_whole_atmos_wvl5cm_f64_r4(delnA,beta,R0,HC,H0)
            LC3  = analytic_sol_LC3_whole_atmos_wvl5cm_f65_r4(delnA,beta,R0,HC,H0)
            alpha= LC1+LC2+LC3 
       end function refraction_angle_C_whole_atmos_vwl5cm_f66_r4

       elemental function refraction_angle_C_whole_atmos_vwl5cm_f66_r8(delnA,beta,R0,HC,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_C_whole_atmos_vwl5cm_f66_r8
            !dir$ attributes forceinline :: refraction_angle_C_whole_atmos_vwl5cm_f66_r8
#endif 
!$omp declare simd(refraction_angle_C_whole_atmos_vwl5cm_f66_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HC 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     automatic   :: LC1, LC2, LC3 
            LC1  = analytic_sol_LC1_whole_atmos_wvl5cm_f63_r8(delnA,beta,R0,HC,H0)
            LC2  = analytic_sol_LC2_whole_atmos_wvl5cm_f64_r8(delnA,beta,R0,HC,H0)
            LC3  = analytic_sol_LC3_whole_atmos_wvl5cm_f65_r8(delnA,beta,R0,HC,H0)
            alpha= LC1+LC2+LC3 
       end function refraction_angle_C_whole_atmos_vwl5cm_f66_r8

       !Formula 6.1, page: 119
       elemental function refraction_angle_whole_atmos_vwl5cm_f61_r4(delnA,beta,R0,HB,HC,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_whole_atmos_vwl5cm_f61_r4
            !dir$ attributes forceinline :: refraction_angle_whole_atmos_vwl5cm_f61_r4
#endif 
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HB 
            real(kind=sp),     intent(in)  :: HC 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     automatic   :: alpha_b, alpha_c
            alpha_b = refraction_angle_B_whole_atmos_vwl5cm_f62_r4(delnA,beta,R0,HB,H0)
            alpha_c = refraction_angle_C_whole_atmos_vwl5cm_f66_r4(delnA,beta,R0,HC,H0)
            alpha   = alpha_b+alpha_c 
       end function refraction_angle_whole_atmos_vwl5cm_f61_r4

         elemental function refraction_angle_whole_atmos_vwl5cm_f61_r8(delnA,beta,R0,HB,HC,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_whole_atmos_vwl5cm_f61_r8
            !dir$ attributes forceinline :: refraction_angle_whole_atmos_vwl5cm_f61_r8
#endif 
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HB 
            real(kind=dp),     intent(in)  :: HC 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     automatic   :: alpha_b, alpha_c
            alpha_b = refraction_angle_B_whole_atmos_vwl5cm_f62_r8(delnA,beta,R0,HB,H0)
            alpha_c = refraction_angle_C_whole_atmos_vwl5cm_f66_r8(delnA,beta,R0,HC,H0)
            alpha   = alpha_b+alpha_c 
       end function refraction_angle_whole_atmos_vwl5cm_f61_r8

       !А. Излучатель и приемник находятся на таком рас-.;
       !стоянии друг от друга, что выполняются условия
       !SQRT(2*beta*HB0) and SQRT(2*beta*HC0) <= 1
       !Formula: 6.10, page: 120 
       elemental function refraction_angle_B_whole_atmos_vwl5cm_f610_r4(delnA,beta,R0,HB,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_B_whole_atmos_vwl5cm_f610_r4
            !dir$ attributes forceinline :: refraction_angle_B_whole_atmos_vwl5cm_f610_r4
#endif 
!$omp declare simd(refraction_angle_B_whole_atmos_vwl5cm_f610_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HB 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     automatic   :: sdelnA,sbt 
            real(kind=sp),     automatic   :: RHB0, sqr1 
            real(kind=sp),     automatic   :: sqr2, trm1 
            real(kind=sp),     automatic   :: trm2, t0 
            real(kind=sp),     automatic   :: HBH0 
            HBH0  = HB-H0 
            sbt   = beta*beta 
            RHB0  = R0*HBH0 
            sqr2  = sqrt(0.5_sp*RHB0)
            sdelnA= delnA*delnA 
            t0    = sdelnA*bt*R0 
            trm2  = t0*sqr2 
            sqr1  = sqrt(RHB0+RHB0)
            trm1  = delnA*beta*sqr1 
            alpha = trm1*trm2 
       end function refraction_angle_B_whole_atmos_vwl5cm_f610_r4

         elemental function refraction_angle_B_whole_atmos_vwl5cm_f610_r8(delnA,beta,R0,HB,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_B_whole_atmos_vwl5cm_f610_r8
            !dir$ attributes forceinline :: refraction_angle_B_whole_atmos_vwl5cm_f610_r8
#endif 
!$omp declare simd(refraction_angle_B_whole_atmos_vwl5cm_f610_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HB 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     automatic   :: sdelnA,sbt 
            real(kind=dp),     automatic   :: RHB0, sqr1 
            real(kind=dp),     automatic   :: sqr2, trm1 
            real(kind=dp),     automatic   :: trm2, t0 
            real(kind=dp),     automatic   :: HBH0 
            HBH0  = HB-H0 
            sbt   = beta*beta 
            RHB0  = R0*HBH0 
            sqr2  = sqrt(0.5_dp*RHB0)
            sdelnA= delnA*delnA 
            t0    = sdelnA*bt*R0 
            trm2  = t0*sqr2 
            sqr1  = sqrt(RHB0+RHB0)
            trm1  = delnA*beta*sqr1 
            alpha = trm1*trm2 
       end function refraction_angle_B_whole_atmos_vwl5cm_f610_r8

       !Formula: 6.11, page: 120
        elemental function refraction_angle_C_whole_atmos_vwl5cm_f611_r4(delnA,beta,R0,HC,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_C_whole_atmos_vwl5cm_f611_r4
            !dir$ attributes forceinline :: refraction_angle_C_whole_atmos_vwl5cm_f611_r4
#endif 
!$omp declare simd(refraction_angle_C_whole_atmos_vwl5cm_f611_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HC 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     automatic   :: sdelnA,sbt 
            real(kind=sp),     automatic   :: RHC0, sqr1 
            real(kind=sp),     automatic   :: sqr2, trm1 
            real(kind=sp),     automatic   :: trm2, t0 
            real(kind=sp),     automatic   :: HCH0 
            HCH0  = HC-H0 
            sbt   = beta*beta 
            RHC0  = R0*HCH0 
            sqr2  = sqrt(0.5_sp*RHC0)
            sdelnA= delnA*delnA 
            t0    = sdelnA*bt*R0 
            trm2  = t0*sqr2 
            sqr1  = sqrt(RHC0+RHC0)
            trm1  = delnA*beta*sqr1 
            alpha = trm1*trm2 
       end function refraction_angle_C_whole_atmos_vwl5cm_f611_r4

       elemental function refraction_angle_C_whole_atmos_vwl5cm_f611_r8(delnA,beta,R0,HC,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_C_whole_atmos_vwl5cm_f611_r8
            !dir$ attributes forceinline :: refraction_angle_C_whole_atmos_vwl5cm_f611_r8
#endif 
!$omp declare simd(refraction_angle_C_whole_atmos_vwl5cm_f611_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HC 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     automatic   :: sdelnA,sbt 
            real(kind=dp),     automatic   :: RHC0, sqr1 
            real(kind=dp),     automatic   :: sqr2, trm1 
            real(kind=dp),     automatic   :: trm2, t0 
            real(kind=dp),     automatic   :: HCH0 
            HCH0  = HC-H0 
            sbt   = beta*beta 
            RHC0  = R0*HCH0 
            sqr2  = sqrt(0.5_dp*RHC0)
            sdelnA= delnA*delnA 
            t0    = sdelnA*bt*R0 
            trm2  = t0*sqr2 
            sqr1  = sqrt(RHC0+RHC0)
            trm1  = delnA*beta*sqr1 
            alpha = trm1*trm2 
       end function refraction_angle_C_whole_atmos_vwl5cm_f611_r8

       !Formula: 6.1b, page: 119
       elemental function refraction_angle_whole_atmos_vwl5cm_f61b_r4(delnA,beta,R0,HB,HC,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_whole_atmos_vwl5cm_f61b_r4
            !dir$ attributes forceinline :: refraction_angle_whole_atmos_vwl5cm_f61b_r4
#endif 
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HB 
            real(kind=sp),     intent(in)  :: HC 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     automatic   :: alpha_b, alpha_c
            alpha_b = refraction_angle_B_whole_atmos_vwl5cm_f610_r4(delnA,beta,R0,HB,H0)
            alpha_c = refraction_angle_C_whole_atmos_vwl5cm_f611_r4(delnA,beta,R0,HC,H0)
            alpha   = alpha_b+alpha_c 
       end function refraction_angle_whole_atmos_vwl5cm_f61b_r4

       elemental function refraction_angle_whole_atmos_vwl5cm_f61b_r8(delnA,beta,R0,HB,HC,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_whole_atmos_vwl5cm_f61b_r8
            !dir$ attributes forceinline :: refraction_angle_whole_atmos_vwl5cm_f61b_r8
#endif 
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HB 
            real(kind=dp),     intent(in)  :: HC 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     automatic   :: alpha_b, alpha_c
            alpha_b = refraction_angle_B_whole_atmos_vwl5cm_f610_r8(delnA,beta,R0,HB,H0)
            alpha_c = refraction_angle_C_whole_atmos_vwl5cm_f611_r8(delnA,beta,R0,HC,H0)
            alpha   = alpha_b+alpha_c 
       end function refraction_angle_whole_atmos_vwl5cm_f61b_r8

       !Б. Излучатель и приемник удалены друг от друга
       !   на такое расстояние, что выполняются неравенства) SQRT(2*beta*HB0) >> 1 , SQRT(2*beta*HC0) >> 1
       !Formula: 6.12, page: 121
        elemental function refraction_angle_B_whole_atmos_vwl5cm_f612_r4(delnA,beta,R0,HB,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_B_whole_atmos_vwl5cm_f612_r4
            !dir$ attributes forceinline :: refraction_angle_B_whole_atmos_vwl5cm_f612_r4
#endif 
!$omp declare simd(refraction_angle_B_whole_atmos_vwl5cm_f612_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HB 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     parameter   :: C314159265358979323846264338328 = &
                                                 3.14159265358979323846264338328_sp
            real(kind=sp),     parameter   :: C041421356237309504880168872421 = &
                                                 0.41421356237309504880168872421_sp
            real(kind=sp),     automatic   :: HBH0, btHBH0 
            real(kind=sp),     automatic   :: btR0, sqr1
            real(kind=sp),     automatic   :: sqr2, exp1 
            real(kind=sp),     automatic   :: t0, t1 
            real(kind=sp),     automatic   :: trm1, trm2 
            HBH0   = HB-H0 
            btR0   = beta*R0 
            btHBH0 = beta*HBH0
            t0     = C314159265358979323846264338328*btR0
            sqr1   = sqrt(0.5_sp*t0)
            t1     = C314159265358979323846264338328*btHBH0 
            sqr2   = sqrt(t1)
            trm1   = delnA*sqr1 
            exp1   = exp(-btHBH0)/sqr2
            t0     = 1.0_sp+C041421356237309504880168872421* &
                     delnA*btR0
            trm2   = t0-exp1 
            alpha  = trm1*trm2 
        end function refraction_angle_B_whole_atmos_vwl5cm_f612_r4

       elemental function refraction_angle_B_whole_atmos_vwl5cm_f612_r8(delnA,beta,R0,HB,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_B_whole_atmos_vwl5cm_f612_r8
            !dir$ attributes forceinline :: refraction_angle_B_whole_atmos_vwl5cm_f612_r8
#endif 
!$omp declare simd(refraction_angle_B_whole_atmos_vwl5cm_f612_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HB 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     parameter   :: C314159265358979323846264338328 = &
                                                 3.14159265358979323846264338328_dp
            real(kind=dp),     parameter   :: C041421356237309504880168872421 = &
                                                 0.41421356237309504880168872421_dp
            real(kind=dp),     automatic   :: HBH0, btHBH0 
            real(kind=dp),     automatic   :: btR0, sqr1
            real(kind=dp),     automatic   :: sqr2, exp1 
            real(kind=dp),     automatic   :: t0, t1 
            real(kind=dp),     automatic   :: trm1, trm2 
            HBH0   = HB-H0 
            btR0   = beta*R0 
            btHBH0 = beta*HBH0
            t0     = C314159265358979323846264338328*btR0
            sqr1   = sqrt(0.5_dp*t0)
            t1     = C314159265358979323846264338328*btHBH0 
            sqr2   = sqrt(t1)
            trm1   = delnA*sqr1 
            exp1   = exp(-btHBH0)/sqr2
            t0     = 1.0_dp+C041421356237309504880168872421* &
                     delnA*btR0
            trm2   = t0-exp1 
            alpha  = trm1*trm2 
        end function refraction_angle_B_whole_atmos_vwl5cm_f612_r8

        !Formula: 6.13, page: 121
       elemental function refraction_angle_C_whole_atmos_vwl5cm_f613_r4(delnA,beta,R0,HC,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_C_whole_atmos_vwl5cm_f613_r4
            !dir$ attributes forceinline :: refraction_angle_C_whole_atmos_vwl5cm_f613_r4
#endif 
!$omp declare simd(refraction_angle_C_whole_atmos_vwl5cm_f613_r4)
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HC
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     parameter   :: C314159265358979323846264338328 = &
                                                 3.14159265358979323846264338328_sp
            real(kind=sp),     parameter   :: C041421356237309504880168872421 = &
                                                 0.41421356237309504880168872421_sp
            real(kind=sp),     automatic   :: HCH0, btHCH0 
            real(kind=sp),     automatic   :: btR0, sqr1
            real(kind=sp),     automatic   :: sqr2, exp1 
            real(kind=sp),     automatic   :: t0, t1 
            real(kind=sp),     automatic   :: trm1, trm2 
            HCH0   = HC-H0 
            btR0   = beta*R0 
            btHCH0 = beta*HCH0
            t0     = C314159265358979323846264338328*btR0
            sqr1   = sqrt(0.5_sp*t0)
            t1     = C314159265358979323846264338328*btHCH0 
            sqr2   = sqrt(t1)
            trm1   = delnA*sqr1 
            exp1   = exp(-btHCH0)/sqr2
            t0     = 1.0_sp+C041421356237309504880168872421* &
                     delnA*btR0
            trm2   = t0-exp1 
            alpha  = trm1*trm2 
        end function refraction_angle_C_whole_atmos_vwl5cm_f613_r4

         elemental function refraction_angle_C_whole_atmos_vwl5cm_f613_r8(delnA,beta,R0,HC,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_C_whole_atmos_vwl5cm_f613_r8
            !dir$ attributes forceinline :: refraction_angle_C_whole_atmos_vwl5cm_f613_r8
#endif 
!$omp declare simd(refraction_angle_C_whole_atmos_vwl5cm_f613_r8)
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HC
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     parameter   :: C314159265358979323846264338328 = &
                                                 3.14159265358979323846264338328_dp
            real(kind=dp),     parameter   :: C041421356237309504880168872421 = &
                                                 0.41421356237309504880168872421_dp
            real(kind=dp),     automatic   :: HCH0, btHCH0 
            real(kind=dp),     automatic   :: btR0, sqr1
            real(kind=dp),     automatic   :: sqr2, exp1 
            real(kind=dp),     automatic   :: t0, t1 
            real(kind=dp),     automatic   :: trm1, trm2 
            HCH0   = HC-H0 
            btR0   = beta*R0 
            btHCH0 = beta*HCH0
            t0     = C314159265358979323846264338328*btR0
            sqr1   = sqrt(0.5_dp*t0)
            t1     = C314159265358979323846264338328*btHCH0 
            sqr2   = sqrt(t1)
            trm1   = delnA*sqr1 
            exp1   = exp(-btHCH0)/sqr2
            t0     = 1.0_dp+C041421356237309504880168872421* &
                     delnA*btR0
            trm2   = t0-exp1 
            alpha  = trm1*trm2 
        end function refraction_angle_C_whole_atmos_vwl5cm_f613_r8

       !Formula: 6.1b, page: 119
       elemental function refraction_angle_whole_atmos_case_B_vwl5cm_f61b_r4(delnA,beta,R0,HB,HC,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_whole_atmos_case_B_vwl5cm_f61b_r4
            !dir$ attributes forceinline :: refraction_angle_whole_atmos_case_B_vwl5cm_f61b_r4
#endif 
            real(kind=sp),     intent(in)  :: delnA 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: HB 
            real(kind=sp),     intent(in)  :: HC 
            real(kind=sp),     intent(in)  :: H0 
            real(kind=sp)                  :: alpha 
            real(kind=sp),     automatic   :: alpha_b, alpha_c
            alpha_b = refraction_angle_B_whole_atmos_vwl5cm_f612_r4(delnA,beta,R0,HB,H0)
            alpha_c = refraction_angle_C_whole_atmos_vwl5cm_f613_r4(delnA,beta,R0,HC,H0)
            alpha   = alpha_b+alpha_c 
       end function refraction_angle_whole_atmos_case_B_vwl5cm_f61b_r4

       elemental function refraction_angle_whole_atmos_case_B_vwl5cm_f61b_r8(delnA,beta,R0,HB,HC,H0) result(alpha)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refraction_angle_whole_atmos_case_B_vwl5cm_f61b_r8
            !dir$ attributes forceinline :: refraction_angle_whole_atmos_case_B_vwl5cm_f61b_r8
#endif 
            real(kind=dp),     intent(in)  :: delnA 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: HB 
            real(kind=dp),     intent(in)  :: HC 
            real(kind=dp),     intent(in)  :: H0 
            real(kind=dp)                  :: alpha 
            real(kind=dp),     automatic   :: alpha_b, alpha_c
            alpha_b = refraction_angle_B_whole_atmos_vwl5cm_f612_r8(delnA,beta,R0,HB,H0)
            alpha_c = refraction_angle_C_whole_atmos_vwl5cm_f613_r8(delnA,beta,R0,HC,H0)
            alpha   = alpha_b+alpha_c 
       end function refraction_angle_whole_atmos_case_B_vwl5cm_f61b_r8

       !Formula: 6.19, page: 122
       elemental function deriv_alpha_over_R0_f619_r4(deln0,beta,R0) result(dadR0)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: deriv_alpha_over_R0_f619_r4
            !dir$ attributes forceinline :: deriv_alpha_over_R0_f619_r4
#endif 
!$omp declare simd(deriv_alpha_over_R0_f619_r4)
            real(kind=sp),     intent(in)  :: deln0 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp)                  :: dadR0 
            real(kind=sp),     parameter   :: C6283185307179586476925286766559 = & 
                                                    6.283185307179586476925286766559_sp
            real(kind=sp),     parameter   :: C041421356237309504880168872421 = &
                                                 0.41421356237309504880168872421_sp
            real(kind=sp),     automatic   :: m1, m2 
            real(kind=sp),     automatic   :: bta, dexp1 
            real(kind=sp),     automatic   :: btR0, exp1 
            real(kind=sp),     automatic   :: sqr1, sqr2 
            real(kind=sp),     automatic   :: t0,   t1 
            real(kind=sp),     automatic   :: trm1, trm2 
            bta   = beta*6378.0_sp 
            dexp1 = exp(bta)
            t0    = C6283185307179586476925286766559*beta 
            sqr1  = sqrt(t0)
            m1    = deln0*dexp1*sqr1 
            t1    = beta*dexp1 
            m2    = C041421356237309504880168872421*deln0*t1 
            btR0  = beta*R0 
            sqr2  = sqrt(R0)
            exp1  = exp(-btR0)
            t0    = -m1*beta*sqr2 
            trm1  = t0*exp1 
            t1    = 1.0_sp+m2+m2 
            t0    = R0*exp1 
            trm2  = t1*t0 
            dadR0 = trm1*trm2 
       end function deriv_alpha_over_R0_f619_r4

        elemental function deriv_alpha_over_R0_f619_r8(deln0,beta,R0) result(dadR0)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: deriv_alpha_over_R0_f619_r8
            !dir$ attributes forceinline :: deriv_alpha_over_R0_f619_r8
#endif 
!$omp declare simd(deriv_alpha_over_R0_f619_r8)
            real(kind=dp),     intent(in)  :: deln0 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp)                  :: dadR0 
            real(kind=dp),     parameter   :: C6283185307179586476925286766559 = & 
                                                    6.283185307179586476925286766559_dp
            real(kind=dp),     parameter   :: C041421356237309504880168872421 = &
                                                 0.41421356237309504880168872421_dp
            real(kind=dp),     automatic   :: m1, m2 
            real(kind=dp),     automatic   :: bta, dexp1 
            real(kind=dp),     automatic   :: btR0, exp1 
            real(kind=dp),     automatic   :: sqr1, sqr2 
            real(kind=dp),     automatic   :: t0,   t1 
            real(kind=dp),     automatic   :: trm1, trm2 
            bta   = beta*6378.0_dp 
            dexp1 = exp(bta)
            t0    = C6283185307179586476925286766559*beta 
            sqr1  = sqrt(t0)
            m1    = deln0*dexp1*sqr1 
            t1    = beta*dexp1 
            m2    = C041421356237309504880168872421*deln0*t1 
            btR0  = beta*R0 
            sqr2  = sqrt(R0)
            exp1  = exp(-btR0)
            t0    = -m1*beta*sqr2 
            trm1  = t0*exp1 
            t1    = 1.0_dp+m2+m2 
            t0    = R0*exp1 
            trm2  = t1*t0 
            dadR0 = trm1*trm2 
       end function deriv_alpha_over_R0_f619_r8

       !Formula: 6.18, page: 122
       elemental function refracted_signal_weakening_Vp_f618_r4(deln0,beta,R0,gamma,Lc,Lb) result(Vp)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refracted_signal_weakening_Vp_f618_r4
            !dir$ attributes forceinline :: refracted_signal_weakening_Vp_f618_r4
#endif 
!$omp declare simd(refracted_signal_weakening_Vp_f618_r4)
            real(kind=sp),     intent(in)  :: deln0 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: gamma 
            real(kind=sp),     intent(in)  :: Lc 
            real(kind=sp),     intent(in)  :: Lb 
            real(kind=sp)                  :: Vp 
            real(kind=sp),     automatic   :: LcLb, cosg 
            real(kind=sp),     automatic   :: scosg, dadR0 
            real(kind=sp),     automatic   :: num, denom 
            real(kind=sp),     automatic   :: t0, t1 
            LcLb   = Lc+Lb 
            cosg   = cos(gamma)
            dadR0  = deriv_alpha_over_R0_f619_r4(deln0,beta,R0)
            num    = cosg*LcLb 
            scosg  = cosg*cosg 
            t0     = (scosg/Lc)-dadR0
            t1     = 1.0_sp+Lb*t0 
            denom  = Lc*t1 
            Vp     = num/denom 
       end function refracted_signal_weakening_Vp_f618_r4
        
       elemental function refracted_signal_weakening_Vp_f618_r8(deln0,beta,R0,gamma,Lc,Lb) result(Vp)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refracted_signal_weakening_Vp_f618_r8
            !dir$ attributes forceinline :: refracted_signal_weakening_Vp_f618_r8
#endif 
!$omp declare simd(refracted_signal_weakening_Vp_f618_r8)
            real(kind=dp),     intent(in)  :: deln0 
            real(kind=dp),     intent(in)  :: beta 
            real(kind=dp),     intent(in)  :: R0 
            real(kind=dp),     intent(in)  :: gamma 
            real(kind=dp),     intent(in)  :: Lc 
            real(kind=dp),     intent(in)  :: Lb 
            real(kind=dp)                  :: Vp 
            real(kind=dp),     automatic   :: LcLb, cosg 
            real(kind=dp),     automatic   :: scosg, dadR0 
            real(kind=dp),     automatic   :: num, denom 
            real(kind=dp),     automatic   :: t0, t1 
            LcLb   = Lc+Lb 
            cosg   = cos(gamma)
            dadR0  = deriv_alpha_over_R0_f619_r8(deln0,beta,R0)
            num    = cosg*LcLb 
            scosg  = cosg*cosg 
            t0     = (scosg/Lc)-dadR0
            t1     = 1.0_dp+Lb*t0 
            denom  = Lc*t1 
            Vp     = num/denom 
       end function refracted_signal_weakening_Vp_f618_r8

       !Formula: 6.20, page: 122
       elemental function refracted_signal_weakening_case1_Vp_f620_r4(deln0,beta,R0,Lb) result(Vp)
if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refracted_signal_weakening_case1_Vp_f620_r4
            !dir$ attributes forceinline :: refracted_signal_weakening_case1_Vp_f620_r4
#endif 
!$omp declare simd(refracted_signal_weakening_case1_Vp_f620_r4)
            real(kind=sp),     intent(in)  :: deln0 
            real(kind=sp),     intent(in)  :: beta 
            real(kind=sp),     intent(in)  :: R0 
            real(kind=sp),     intent(in)  :: Lb 
            real(kind=sp)                  :: Vp 
            real(kind=sp),     automatic   :: dadR0, trm1 
            dadR0  = deriv_alpha_over_R0_f619_r4(deln0,beta,R0)
            trm1   = 1.0_sp-Lb*dadR0
            Vp     = 1.0_sp/trm1 
       end function refracted_signal_weakening_case1_Vp_f620_r4

       

end module emw_refraction
