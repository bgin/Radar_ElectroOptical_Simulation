

module eos_sensor_simd


!======================================================!
! Various characteristics of Electro-Optical Systems   !
! Based mainly on Miroshenko M.M book (rus):           !
! "Mathematical Theory of Electro-Optical Sensors"     !
!======================================================!
!===================================================================================85
 !---------------------------- DESCRIPTION ------------------------------------------85
 !
 !
 !
 !          Module  name:
 !                         eos_sensor_simd
 !          
 !          Purpose:
 !                        Various characteristics of Electro-Optical Sensors   
 !                        Based mainly on Based mainly on Miroshenko M.M book (rus):          
 !                        "Mathematical Theory of Electro-Optical Sensors".
 !                        This module contains only explicitly vectorized (SIMD)
 !                        versions of many function implemented in eos_sensor (scalar)
 !                        module
 !          History:
 !                        Date: 09-24-2022
 !                        Time: 17:16 GMT+2
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
 !                       Miroshenko M.M book (rus):          
 !                      "Mathematical Theory of Electro-Optical Sensors"     
 !         
 !          E-mail:
 !                  
 !                      beniekg@gmail.com
!==================================================================================85
    ! Tab:5 col - Type and etc.. definitions
    ! Tab:10,11 col - Type , function and subroutine code blocks.
   
   use mod_kinds,    only : i4,sp,dp
   use mod_vectypes, only : YMM8r4_t, YMM4r8_t, ZMM16r4_t, ZMM8r8_t
   public
   implicit none

     ! Major version
     integer(kind=i4),  parameter :: EOS_SENSOR_SIMD_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: EOS_SENSOR_SIMD_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: EOS_SENSOR_SIMD_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: EOS_SENSOR_SIMD_FULLVER =   &
            1000*EOS_SENSOR_SIMD_MAJOR+100*EOS_SENSOR_SIMD_MINOR+10*EOS_SENSOR_SIMD_MICRO
     ! Module creation date
     character(*),        parameter :: EOS_SENSOR_SIMD_CREATE_DATE = "24-08-2022 17:17 +00200 (WED 24 AUG 2022 GMT+2)"
     ! Module build date
     character(*),        parameter :: EOS_SENSOR_SIMD_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: EOS_SENSOR_SIMD_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: EOS_SENSOR_SIMD_SYNOPSIS    = "EO Sensors characteristics and models explicitly vectorized (SIMD)."

     
     contains


     subroutine const_flux_spectr_unroll_16x_ymm8r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_16x_ymm8r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_16x_ymm8r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_16x_ymm8r4
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(YMM8r4_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(YMM8r4_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(YMM8r4_t), parameter :: twopi = YMM8r4_t(6.283185307179586476925286766559_sp)
           type(YMM8r4_r), parameter :: half  = YMM8r4_t(0.5_sp)
           type(YMM8r4_t), parameter :: one   = YMM8r4_t(1.0_sp)
           type(YMM8r4_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 32 :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(YMM8r4_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           !dir$ attributes align : 32 :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(YMM8r4_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 32 :: f0,f1,f2,f3,f4,f5,f6,f7
           type(YMM8r4_t) :: f8,f9,f10,f11,f12,f13,f14,f15
           !dir$ attributes align : 32 :: f8,f9,f10,f11,f12,f13,f14,f15
           type(YMM8r4_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 32 :: c0,c1,c2,c3,c4,c5,c6,c7
           type(YMM8r4_t) :: c8,c9,c10,c11,c12,c13,c14,c15
           !dir$ attributes align : 32 :: c8,c9,c10,c11,c12,c13,c14,c15
           type(YMM8r4_t) :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           !dir$ attributes align : 32 :: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           type(YMM8r4_t) :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           !dir$ attributes align : 32 :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           type(YMM8r4_t) :: hT,Phi0T
           !dir$ attributes align : 32 :: hT,Phi0t
           real(kind=sp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<8) then
              return
           else if(n==8) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>8 .and. n<=128) then
              ! rolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(7)),8
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,7
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v*f0.v*hT.v
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>128) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:64
              !dir$ assume_aligned freq:64
              do i=1,iand(n,not(7)),128
                  call mm_prefetch(freq(i*128),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,7
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v*f0.v*hT.v
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v-(f0.v*hT.v*f0.v*hT.v)
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+8+ii)
                    c1.v(ii)       = twopi.v*f1.v*hT.v
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v-(f1.v*hT.v*f1.v*hT.v)
                    Phi0f(i+8+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+16+ii)
                    c2.v(ii)       = twopi.v*f2.v*hT.v
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+24+ii)
                    c3.v(ii)       = twopi.v*f3.v*hT.v
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v-(f3.v*hT.v*f3.v*hT.v)
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+32+ii)
                    c4.v(ii)       = twopi.v*f4.v*hT.v
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v-(f4.v*hT.v*f4.v*hT.v)
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+40+ii)
                    c5.v(ii)       = twopi.v*f5.v*hT.v
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v-(f5.v*hT.v*f5.v*hT.v)
                    Phi0f(i+40+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+48+ii)
                    c6.v(ii)       = twopi.v*f6.v*hT.v
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+ii)
                    c7.v(ii)       = twopi.v*f7.v*hT.v
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v-(f7.v*hT.v*f7.v*hT.v)
                    Phi0f(i+56+ii) = Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                    f8.v(ii)       = freq(i+64+ii)
                    c8.v(ii)       = twopi.v*f8.v*hT.v
                    sa8.v(ii)      = sin(c8.v(ii))/c8.v(ii)
                    arg8.v(ii)     = one.v-(f8.v*hT.v*f8.v*hT.v)
                    Phi0f(i+64+ii) = Phi0T.v(ii)*(sa8.v(ii)/arg8.v(ii))
                    f9.v(ii)       = freq(i+72+ii)
                    c9.v(ii)       = twopi.v*f9.v*hT.v
                    sa9.v(ii)      = sin(c9.v(ii))/c9.v(ii)
                    arg9.v(ii)     = one.v-(f9.v*hT.v*f9.v*hT.v)
                    Phi0f(i+72+ii) = Phi0T.v(ii)*(sa9.v(ii)/arg9.v(ii)) 
                    f10.v(ii)      = freq(i+80+ii)
                    c10.v(ii)      = twopi.v*f10.v*hT.v
                    sa10.v(ii)     = sin(c10.v(ii))/c10.v(ii)
                    arg10.v(ii)    = one.v-(f10.v*hT.v*f10.v*hT.v)
                    Phi0f(i+80+ii) = Phi0T.v(ii)*(sa10.v(ii)/arg10.v(ii))
                    f11.v(ii)      = freq(i+88+ii)
                    c11.v(ii)      = twopi.v*f11.v*hT.v
                    sa11.v(ii)     = sin(c11.v(ii))/c11.v(ii)
                    arg11.v(ii)    = one.v-(f11.v*hT.v*f11.v*hT.v)
                    Phi0f(i+88+ii) = Phi0T.v(ii)*(sa11.v(ii)/arg11.v(ii))
                    f12.v(ii)      = freq(i+96+ii)
                    c12.v(ii)      = twopi.v*f12.v*hT.v
                    sa12.v(ii)     = sin(c12.v(ii))/c12.v(ii)
                    arg12.v(ii)    = one.v-(f12.v*hT.v*f12.v*hT.v)
                    Phi0f(i+96+ii) = Phi0T.v(ii)*(sa12.v(ii)/arg12.v(ii))
                    f13.v(ii)      = freq(i+104+ii)
                    c13.v(ii)      = twopi.v*f13.v*hT.v
                    sa13.v(ii)     = sin(c13.v(ii))/c13.v(ii)
                    arg13.v(ii)    = one.v-(f13.v*hT.v*f13.v*hT.v)
                    Phi0f(i+104+ii)= Phi0T.v(ii)*(sa13.v(ii)/arg13.v(ii))
                    f14.v(ii)      = freq(i+112+ii)
                    c14.v(ii)      = twopi.v*f14.v*hT.v
                    sa14.v(ii)     = sin(c14.v(ii))/c14.v(ii)
                    arg14.v(ii)    = one.v-(f14.v*hT.v*f14.v*hT.v)
                    Phi0f(i+112+ii)= Phi0T.v(ii)*(sa14.v(ii)/arg14.v(ii))
                    f15.v(ii)      = freq(i+120+ii)
                    c15.v(ii)      = twopi.v*f15.v*hT.v
                    sa15.v(ii)     = sin(c15.v(ii))/c15.v(ii)
                    arg15.v(ii)    = one.v-(f15.v*hT.v*f15.v*hT.v)
                    Phi0f(i+120+ii)= Phi0T.v(ii)*(sa15.v(ii)/arg15.v(ii))
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=8,min=1,avg=4
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_16x_ymm8r4













end module eos_sensor_simd
