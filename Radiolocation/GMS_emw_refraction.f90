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

     interface n_refract_tht_f243
         module procedure n_refract_tht_f243_r4
         module procedure n_refract_tht_f243_r8
     end interface n_refract_tht_f243

     interface n_refract_phi_f243
         module procedure n_refract_phi_f243_r4
         module procedure n_refract_phi_f243_r8
     end interface n_refract_phi_f243

     interface rad_ray_curvature_f251
         module procedure rad_ray_curvature_f251_r4 
         module procedure rad_ray_curvature_f251_r8 
     end interface rad_ray_curvature_f251

     interface k_relative_f254
         module procedure  k_relative_f254_r4
         module procedure  k_relative_f254_r8 
     end interface k_relative_f254

     ! Constants:

     real(kind=sp), parameter, private :: L    = 6.02214076e+23_sp ! (mol-1), avogadro constant
     real(kind=sp), parameter, private :: K1h  = 0.77607_sp+0.0013_sp ! K/Pa
     real(kind=sp), parameter, private :: K1l  = 0.77607_sp-0.0013_sp ! K/Pa
     real(kind=sp), parameter, private :: K2h  = 0.716_sp+0.085_sp    ! K/Pa
     real(kind=sp), parameter, private :: K2l  = 0.716_sp-0.085_sp    ! K/Pa
     real(kind=sp), parameter, private :: K3h  = 3747.0_sp+31.0_sp    ! K^2/Pa
     real(kind=sp), parameter, private :: K3l  = 3747.0_sp-31.0_sp    ! K^2/Pa
     real(kind=sp), parameter, private :: K4h  = 1.2934_sp+0.0002_sp  ! K/Pa
     real(kind=sp), parameter, private :: K4l  = 1.2934_sp-0.0002_sp  ! K/Pa
     
     contains

     ! Formula 2.43, page 46
     pure function n_refract_tht_f243_r4(n,n0,z,z0,r,R0,phi,phi0) result(n_over_tht)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_refract_tht_f243_r4
            !dir$ attributes forceinline :: n_refract_tht_f243_r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: n_refract_tht_f243_r4
#endif
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
    
     
     pure function n_refract_tht_f243_r8(n,n0,z,z0,r,R0,phi,phi0) result(n_over_tht)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_refract_tht_f243_r8
            !dir$ attributes forceinline :: n_refract_tht_f243_r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: n_refract_tht_f243_r8
#endif
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

     pure function n_refract_phi_f243_r4(n,n0,z,z0,r,R0,phi,phi0) result(n_over_phi)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_refract_phi_f243_r4
            !dir$ attributes forceinline :: n_refract_phi_f243_r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: n_refract_phi_f243_r4
#endif
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

     pure function n_refract_phi_f243_r8(n,n0,z,z0,r,R0,phi,phi0) result(n_over_phi)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: n_refract_phi_f243_r8
            !dir$ attributes forceinline :: n_refract_phi_f243_r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: n_refract_phi_f243_r8
#endif
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
     
     pure function rad_ray_curvature_f251_r4(n,z,dndr) result(rho)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: rad_ray_curvature_f251_r4
            !dir$ attributes forceinline :: rad_ray_curvature_f251_r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rad_ray_curvature_f251_r4
#endif
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

     pure function rad_ray_curvature_f251_r8(n,z,dndr) result(rho)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: rad_ray_curvature_f251_r8
            !dir$ attributes forceinline :: rad_ray_curvature_f251_r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rad_ray_curvature_f251_r8
#endif
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
     
     pure function k_relative_f254_r4(n,z,dndr) result(k_rel)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: k_relative_f254_r4
            !dir$ attributes forceinline :: k_relative_f254_r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: k_relative_f254_r4
#endif   
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
 
     pure function k_relative_f254_r8(n,z,dndr) result(k_rel)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: k_relative_f254_r8
            !dir$ attributes forceinline :: k_relative_f254_r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: k_relative_f254_r8
#endif   
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
     pure function rho_to_a_f267_r4(dndh) result(R)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: rho_to_a_f267_r4
            !dir$ attributes forceinline :: rho_to_a_f267_r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rho_to_a_f267_r4
#endif  
            real(kind=sp),   intent(in) :: dndh ! derivative of refractive index
            real(kind=sp) :: R 
            real(kind=sp), parameter :: inv_erad = -0.00015678896205707118218877391_sp 
            R = inv_erad*dndh 
     end function rho_to_a_f267_r4

       pure function rho_to_a_f267_r8(dndh) result(R)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: rho_to_a_f267_r8
            !dir$ attributes forceinline :: rho_to_a_f267_r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: rho_to_a_f267_r8
#endif  
            real(kind=dp),   intent(in) :: dndh ! derivative of refractive index
            real(kind=dp) :: R 
            real(kind=dp), parameter :: inv_erad = -0.00015678896205707118218877391_dp 
            R = inv_erad*dndh 
     end function rho_to_a_f267_r8 

 




end module emw_refraction