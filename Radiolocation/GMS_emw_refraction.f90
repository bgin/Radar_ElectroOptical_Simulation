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

end module emw_refraction
