


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


module radio_refractivity_ref 








!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name: 
 !                         RADIO_REFRACTIVITY_REF_ref
 !          
 !          Purpose:
 !                      ITU-R P.453-11 - radio refractivity index calculation
 !                        
 !          History:
 !                        Date: 21-12-2024
 !                        Time: 11:35PM GMT+2
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
 !          References:
 !         
 !                          RECOMMENDATION ITU-R P.453-11
 !                          The radio refractive index: its formula and refractivity data
 !                          (Question ITU-R 201/3)
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.

    use mod_kinds,     only : i4,sp,dp
    


    public
    implicit none
    
    
      ! Major version
     integer(kind=i4),  parameter :: RADIO_REFRACTIVITY_REF_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: RADIO_REFRACTIVITY_REF_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: RADIO_REFRACTIVITY_REF_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: RADIO_REFRACTIVITY_REF_FULLVER =   &
            1000*RADIO_REFRACTIVITY_REF_MAJOR+100*RADIO_REFRACTIVITY_REF_MINOR+10*RADIO_REFRACTIVITY_REF_MICRO
     ! Module creation date
     character(*),        parameter :: RADIO_REFRACTIVITY_REF_CREATE_DATE = "21-12-2024 11:38AM +00200 (SAT 21 DEC 2024 GMT+2)"
     ! Module build date
     character(*),        parameter :: RADIO_REFRACTIVITY_REF_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: RADIO_REFRACTIVITY_REF_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: RADIO_REFRACTIVITY_REF_SYNOPSIS  = "ITU-R P.453-11 - radio refractivity index calculation" 
     
     
     contains



     pure function water_vapour_pressure_e_r4(t,P,H,water_or_ice) result(e)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: water_vapour_pressure_e_r4
            !dir$ attributes forceinline :: water_vapour_pressure_e_r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: water_vapour_pressure_e_r4
#endif
            real(kind=sp),   intent(in) :: t ! temperature (C)
            real(kind=sp),   intent(in) :: P ! pressure (hPa)
            real(kind=sp),   intent(in) :: H ! relative humidity (%)
            integer(kind=i4),intent(in) :: water_or_ice ! 0 for water, 1 for ice
            real(kind=sp)  :: e 
            real(kind=sp), parameter    :: a_wat = 6.1121_sp
            real(kind=sp), parameter    :: b_wat = 18.678_sp
            real(kind=sp), parameter    :: c_wat = 257.14_sp
            real(kind=sp), parameter    :: d_wat = 234.5_sp
            ! Ice constants
            real(kind=sp), parameter    :: a_ice = 6.1125_sp
            real(kind=sp), parameter    :: b_ice = 23.036_sp
            real(kind=sp), parameter    :: c_ice = 279.82_sp
            real(kind=sp), parameter    :: d_ice = 333.7_sp
            real(kind=sp), parameter    :: C1    = 1.0_sp
            real(kind=sp), parameter    :: C0001 = 0.0001_sp
            real(kind=sp), automatic    :: ef_wat, ef_ice, num, den
            real(kind=sp), automatic    :: tt, td, es
            tt     = t*t 
            td     = t/d 
            num    = (b-td)*t
            den    = t+c 
            if(water_or_ice == 0) then 
               ef_wat = C1+C0001*(7.2_sp+P*(0.00320_sp+0.0000059_sp*tt))
               es      = ef_wat*a*exp(num/den)
               e      = H*es*0.01_sp

            else if(water_or_ice == 1) then 
               ef_ice = C1+C0001*(2.2_sp+P*(0.00382_sp+0.0000064_sp*tt))
               es      = ef_ice*a*exp(num/den)
               e      = H*es*0.01_sp 
            end if 
      end function water_vapour_pressure_e_r4

       pure function water_vapour_pressure_e_r8(t,P,H,water_or_ice) result(e)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: water_vapour_pressure_e_r8
            !dir$ attributes forceinline :: water_vapour_pressure_e_r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: water_vapour_pressure_e_r8
#endif
            real(kind=dp),   intent(in) :: t ! temperature (C)
            real(kind=dp),   intent(in) :: P ! pressure (hPa)
            real(kind=dp),   intent(in) :: H ! relative humidity (%)
            integer(kind=i4),intent(in) :: water_or_ice ! 0 for water, 1 for ice
            real(kind=dp)  :: e 
            real(kind=dp), parameter    :: a_wat = 6.1121_dp
            real(kind=dp), parameter    :: b_wat = 18.678_dp
            real(kind=dp), parameter    :: c_wat = 257.14_dp
            real(kind=dp), parameter    :: d_wat = 234.5_dp
            ! Ice constants
            real(kind=dp), parameter    :: a_ice = 6.1125_dp
            real(kind=dp), parameter    :: b_ice = 23.036_dp
            real(kind=dp), parameter    :: c_ice = 279.82_dp
            real(kind=dp), parameter    :: d_ice = 333.7_dp
            real(kind=dp), parameter    :: C1    = 1.0_dp
            real(kind=dp), parameter    :: C0001 = 0.0001_dp
            real(kind=dp), automatic    :: ef_wat, ef_ice, num, den
            real(kind=dp), automatic    :: tt, td, es
            tt     = t*t 
            td     = t/d 
            num    = (b-td)*t
            den    = t+c 
            if(water_or_ice == 0) then 
               ef_wat = C1+C0001*(7.2_dp+P*(0.00320_dp+0.0000059_dp*tt))
               es      = ef_wat*a*exp(num/den)
               e      = H*es*0.01_dp

            else if(water_or_ice == 1) then 
               ef_ice = C1+C0001*(2.2_dp+P*(0.00382_dp+0.0000064_dp*tt))
               es      = ef_ice*a*exp(num/den)
               e      = H*es*0.01_dp 
            end if 
      end function water_vapour_pressure_e_r8


      pure function refractivity_index_n_r4(tc,Tk,Pd,P,H,water_or_ice) result(n)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractivity_index_n_r4
            !dir$ attributes forceinline :: refractivity_index_n_r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refractivity_index_n_r4
#endif
            real(kind=sp),   intent(in) :: tc ! temperature (C)
            real(kind=sp),   intent(in) :: Tk ! temperatur (K)
            real(kind=sp),   intent(in) :: Pd ! dry atmospheric pressure  (hPa)
            real(kind=sp),   intent(in) :: P ! pressure (hPa)
            real(kind=sp),   intent(in) :: H ! relative humidity (%)
            integer(kind=i4),intent(in) :: water_or_ice ! 0 for water, 1 for ice
            real(kind=sp) :: n
            real(kind=sp), automatic :: PdT, eT, eTT, e, n_tmp
            e   = water_vapour_pressure_e_r4(tc,P,H,water_or_ice)
            PdT = 76.6_sp*PdT/Tk 
            eT  = 72.0_sp*e/Tk 
            eTT = 3750000.0_sp*(e/(Tk*Tk)) 
            n_tmp   = PdT+eT+eTT 
            n   = 1.0_sp+n_tmp*0.000001_sp
      end function refractivity_index_n_r4
       

       pure function refractivity_index_n_r8(tc,Tk,Pd,P,H,water_or_ice) result(n)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractivity_index_n_r8
            !dir$ attributes forceinline :: refractivity_index_n_r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refractivity_index_n_r8
#endif
            real(kind=dp),   intent(in) :: tc ! temperature (C)
            real(kind=dp),   intent(in) :: Tk ! temperatur (K)
            real(kind=dp),   intent(in) :: Pd ! dry atmospheric pressure  (hPa)
            real(kind=dp),   intent(in) :: P ! pressure (hPa)
            real(kind=dp),   intent(in) :: H ! relative humidity (%)
            integer(kind=i4),intent(in) :: water_or_ice ! 0 for water, 1 for ice
            real(kind=dp) :: n
            real(kind=dp), automatic :: PdT, eT, eTT, e, n_tmp 
            e   = water_vapour_pressure_e_r8(tc,P,H,water_or_ice)
            PdT = 76.6_dp*PdT/Tk 
            eT  = 72.0_dp*e/Tk 
            eTT = 3750000.0_dp*(e/(Tk*Tk)) 
            n_tmp   = PdT+eT+eTT 
            n   = 1.0_dp+n_tmp*0.000001_dp
      end function refractivity_index_n_r8 
     
      ! Function of height
      pure function refractivity_index_nh_r4(N0,h,h0) result(nh)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractivity_index_nh_r4
            !dir$ attributes forceinline :: refractivity_index_nh_r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refractivity_index_nh_r4
#endif
            real(kind=sp),  intent(in) :: N0 
            real(kind=sp),  intent(in) :: h 
            real(kind=sp),  intent(in) :: h0 
            real(kind=sp) :: nh 
            real(kind=sp), automatic :: earg, eres 
            nh = 0.0_sp 
            earg = -h/h0 
            eres = exp(earg)
            nh   = 1.0_sp+N0*0.000001_sp*eres 
      end function refractivity_index_nh_r4

       ! Function of height
      pure function refractivity_index_nh_r8(N0,h,h0) result(nh)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractivity_index_nh_r8
            !dir$ attributes forceinline :: refractivity_index_nh_r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refractivity_index_nh_r8
#endif
            real(kind=dp),  intent(in) :: N0 
            real(kind=dp),  intent(in) :: h 
            real(kind=dp),  intent(in) :: h0 
            real(kind=dp) :: nh 
            real(kind=dp), automatic :: earg, eres 
            nh = 0.0_dp 
            earg = -h/h0 
            eres = exp(earg)
            nh   = 1.0_dp+N0*0.000001_dp*eres 
      end function refractivity_index_nh_r8


      ! Dry term of refractivity
        pure function refractivity_index_dry_r4(Pd,T) result(Ndry)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractivity_index_dry_r4
            !dir$ attributes forceinline :: refractivity_index_dry_r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refractivity_index_dry_r4
#endif
            real(kind=sp), intent(in) :: Pd ! dry atmos pressure (hPa)
            real(kind=sp), intent(in) :: T  ! absolute temp (K)
            real(kind=sp) :: Ndry 
            real(kind=sp), automatic :: rat 
            Ndry = 0.0_sp 
            rat  = Pd/T 
            Ndry = 77.6_sp*rat 
        end function refractivity_index_dry_r4

         ! Dry term of refractivity
        pure function refractivity_index_dry_r8(Pd,T) result(Ndry)
#if defined(__INTEL_COMPILER) && !defined(__GNUC__)           
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: refractivity_index_dry_r8
            !dir$ attributes forceinline :: refractivity_index_dry_r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refractivity_index_dry_r8
#endif
            real(kind=dp), intent(in) :: Pd ! dry atmos pressure (hPa)
            real(kind=dp), intent(in) :: T  ! absolute temp (K)
            real(kind=dp) :: Ndry 
            real(kind=dp), automatic :: rat 
            Ndry = 0.0_dp 
            rat  = Pd/T 
            Ndry = 77.6_dp*rat 
        end function refractivity_index_dry_r8
     
end module radio_refractivity_ref

