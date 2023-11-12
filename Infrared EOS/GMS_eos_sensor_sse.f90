
module eos_sensor_sse

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
 !                         eos_sensor_sse
 !          
 !          Purpose:
 !                        Various characteristics of Electro-Optical Sensors   
 !                        Based mainly on Based mainly on Miroshenko M.M book (rus):          
 !                        "Mathematical Theory of Electro-Optical Sensors".
 !                        This module contains only explicitly vectorized (SIMD)
 !                        versions of many function implemented in eos_sensor (scalar)
 !                        module
 !          History:
 !                        Date: 10-11-2023
 !                        Time: 08:45 GMT+2
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
   use mod_vectypes, only : XMM2r8_t,XMM4r4_t
   public
   implicit none

     ! Major version
     integer(kind=i4),  parameter :: EOS_SENSOR_SSE_MAJOR = 1
     ! Minor version
     integer(kind=i4),  parameter :: EOS_SENSOR_SSE_MINOR = 0
     ! Micro version
     integer(kind=i4),  parameter :: EOS_SENSOR_SSE_MICRO = 0
     ! Full version
     integer(kind=i4),  parameter :: EOS_SENSOR_SSE_FULLVER =   &
            1000*EOS_SENSOR_SSE_MAJOR+100*EOS_SENSOR_SSE_MINOR+10*EOS_SENSOR_SSE_MICRO
     ! Module creation date
     character(*),        parameter :: EOS_SENSOR_SSE_CREATE_DATE = "24-08-2022 17:17 +00200 (WED 24 AUG 2022 GMT+2)"
     ! Module build date
     character(*),        parameter :: EOS_SENSOR_SSE_BUILD_DATE  = __DATE__ " " __TIME__
     ! Module author info
     character(*),        parameter :: EOS_SENSOR_SSE_AUTHOR      = "Programmer: Bernard Gingold, contact: beniekg@gmail.com"
     ! Short description
     character(*),        parameter :: EOS_SENSOR_SSE_SYNOPSIS    = "EO Sensors characteristics and models explicitly vectorized (SSE)."


     
     
     contains
     
     pure function compute_SN_xmm4r4(R,phi,gamma) result(SN)

        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_xmm4r4
        !dir$ attributes forceinline :: compute_SN_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  intent(in), optional :: phi
        type(XMM4r4_t),  intent(in), optional :: gamma
        type(XMM4r4_t) :: SN
        if(present(phi)) then
            SN.v = R.v*sin(phi.v)
        else if(present(gamma)) then
            SN.v = R.v*cos(gamma.v)
        end if
     end function compute_SN_xmm4r4
     
     
     subroutine compute_SN_unroll_16x_xmm4r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_16x_xmm4r4
        !dir$ attributes forceinline :: compute_SN_unroll_16x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM4r4_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(XMM4r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm4r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_xmm4r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_xmm4r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_xmm4r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_xmm4r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_xmm4r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_xmm4r4(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SN(i+8) = compute_SN_xmm4r4(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SN(i+9) = compute_SN_xmm4r4(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SN(i+1) = compute_SN_xmm4r4(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SN(i+1) = compute_SN_xmm4r4(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SN(i+12)= compute_SN_xmm4r4(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SN(i+13)= compute_SN_xmm4r4(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SN(i+14)= compute_SN_xmm4r4(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SN(i+15)= compute_SN_xmm4r4(R,p15,g15)
        end do
     end subroutine compute_SN_unroll_16x_xmm4r4
     
     
     subroutine compute_SN_unroll_16x_omp_xmm4r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_16x_omp_xmm4r4
        !dir$ attributes forceinline :: compute_SN_unroll_16x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_omp_xmm4r4
        use omp_lib
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM4r4_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(XMM4r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)  &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7,p8,g8)             &
        !$omp private(p9,g9,p10,g10,p11,g11,p12,g12,p13)               &
        !$omp private(g13,p14,g14,p15,g15)                             &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm4r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_xmm4r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_xmm4r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_xmm4r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_xmm4r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_xmm4r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_xmm4r4(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SN(i+8) = compute_SN_xmm4r4(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SN(i+9) = compute_SN_xmm4r4(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SN(i+1) = compute_SN_xmm4r4(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SN(i+1) = compute_SN_xmm4r4(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SN(i+12)= compute_SN_xmm4r4(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SN(i+13)= compute_SN_xmm4r4(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SN(i+14)= compute_SN_xmm4r4(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SN(i+15)= compute_SN_xmm4r4(R,p15,g15)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_16x_omp_xmm4r4
     
     
     subroutine compute_SN_unroll_8x_xmm4r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_8x_xmm4r4
        !dir$ attributes forceinline :: compute_SN_unroll_8x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm4r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_xmm4r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_xmm4r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_xmm4r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_xmm4r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_xmm4r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_xmm4r4(R,p7,g7)
        end do
     end subroutine compute_SN_unroll_8x_xmm4r4
     
     
     subroutine compute_SN_unroll_8x_omp_xmm4r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_8x_omp_xmm4r4
        !dir$ attributes forceinline :: compute_SN_unroll_8x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_omp_xmm4r4
        use omp_lib
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)  &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7)                   &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm4r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_xmm4r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_xmm4r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_xmm4r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_xmm4r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_xmm4r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_xmm4r4(R,p7,g7)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_8x_omp_xmm4r4
     
     
     subroutine compute_SN_unroll_4x_xmm4r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_4x_xmm4r4
        !dir$ attributes forceinline :: compute_SN_unroll_4x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1,p2,p3
        type(XMM4r4_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm4r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_xmm4r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_xmm4r4(R,p3,g3)
        end do
     end subroutine compute_SN_unroll_4x_xmm4r4
     
     
     subroutine compute_SN_unroll_4x_omp_xmm4r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_4x_omp_xmm4r4
        !dir$ attributes forceinline :: compute_SN_unroll_4x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_omp_xmm4r4
        use omp_lib
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1,p2,p3
        type(XMM4r4_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)  &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3)                                           &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm4r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_xmm4r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_xmm4r4(R,p3,g3)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_4x_omp_xmm4r4
     
     
     subroutine compute_SN_unroll_2x_xmm4r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_2x_xmm4r4
        !dir$ attributes forceinline :: compute_SN_unroll_2x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1
        type(XMM4r4_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm4r4(R,p1,g1)
        end do
     end subroutine compute_SN_unroll_2x_xmm4r4
     
     
     subroutine compute_SN_unroll_2x_omp_xmm4r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_2x_omp_xmm4r4
        !dir$ attributes forceinline :: compute_SN_unroll_2x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_omp_xmm4r4
        use omp_lib
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1
        type(XMM4r4_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1)            &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm4r4(R,p1,g1)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_2x_omp_xmm4r4
     
     
     subroutine compute_SN_rolled_xmm4r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_rolled_xmm4r4
        !dir$ attributes forceinline :: compute_SN_rolled_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0
        type(XMM4r4_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm4r4(R,p0,g0)
        end do
     end subroutine compute_SN_rolled_xmm4r4
     
     
     subroutine compute_SN_rolled_omp_xmm4r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_rolled_omp_xmm4r4
        !dir$ attributes forceinline :: compute_SN_rolled_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_omp_xmm4r4
        use omp_lib
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0
        type(XMM4r4_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp private(i)  shared(n,phi,gamma,SN)      
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm4r4(R,p0,g0)
        end do
        !$omp end parallel do
     end subroutine compute_SN_rolled_omp_xmm4r4
     
     
     subroutine compute_SN_dispatch_xmm4r4(R,phi,gamma,SN,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 16:: compute_SN_dispatch_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        integer(kind=i4),                 intent(in) :: unroll_cnt
        logical(kind=i4),                 intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
              case (16)
                call compute_SN_unroll_16x_omp_xmm4r4(R,phi,gamma,SN,n)
              case (8)
                call compute_SN_unroll_8x_omp_xmm4r4(R,phi,gamma,SN,n) 
              case (4)
                call compute_SN_unroll_4x_omp_xmm4r4(R,phi,gamma,SN,n)
              case (2)
                call compute_SN_unroll_2x_omp_xmm4r4(R,phi,gamma,SN,n)
              case (0)
                call compute_SN_rolled_omp_xmm4r4(R,phi,gamma,SN,n)
              case default
                return
            end select
         else
            select case (unroll_cnt)
              case (16)
                call compute_SN_unroll_16x_xmm4r4(R,phi,gamma,SN,n)
              case (8)
                call compute_SN_unroll_8x_xmm4r4(R,phi,gamma,SN,n) 
              case (4)
                call compute_SN_unroll_4x_xmm4r4(R,phi,gamma,SN,n)
              case (2)
                call compute_SN_unroll_2x_xmm4r4(R,phi,gamma,SN,n)
              case (0)
                call compute_SN_rolled_xmm4r4(R,phi,gamma,SN,n)
              case default
                return
            end select
         end if
     end subroutine compute_SN_dispatch_xmm4r4


! //////////////////////////////////////////////////////////////////////////////////// !
! //////////////////////////////////////////////////////////////////////////////////// ! 

    
     pure function compute_SN_xmm2r8(R,phi,gamma) result(SN)

        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_xmm2r8
        !dir$ attributes forceinline :: compute_SN_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  intent(in), optional :: phi
        type(XMM2r8_t),  intent(in), optional :: gamma
        type(XMM2r8_t) :: SN
        if(present(phi)) then
            SN.v = R.v*sin(phi.v)
        else if(present(gamma)) then
            SN.v = R.v*cos(gamma.v)
        end if
     end function compute_SN_xmm2r8
     
     
     subroutine compute_SN_unroll_16x_xmm2r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_16x_xmm2r8
        !dir$ attributes forceinline :: compute_SN_unroll_16x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM2r8_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(XMM2r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm2r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_xmm2r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_xmm2r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_xmm2r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_xmm2r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_xmm2r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_xmm2r8(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SN(i+8) = compute_SN_xmm2r8(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SN(i+9) = compute_SN_xmm2r8(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SN(i+1) = compute_SN_xmm2r8(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SN(i+1) = compute_SN_xmm2r8(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SN(i+12)= compute_SN_xmm2r8(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SN(i+13)= compute_SN_xmm2r8(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SN(i+14)= compute_SN_xmm2r8(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SN(i+15)= compute_SN_xmm2r8(R,p15,g15)
        end do
     end subroutine compute_SN_unroll_16x_xmm2r8
     
     
     subroutine compute_SN_unroll_16x_omp_xmm2r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_16x_omp_xmm2r8
        !dir$ attributes forceinline :: compute_SN_unroll_16x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_16x_omp_xmm2r8
        use omp_lib
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM2r8_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(XMM2r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7,p8,g8)             &
        !$omp private(p9,g9,p10,g10,p11,g11,p12,g12,p13)               &
        !$omp private(g13,p14,g14,p15,g15)                             &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm2r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_xmm2r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_xmm2r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_xmm2r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_xmm2r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_xmm2r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_xmm2r8(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SN(i+8) = compute_SN_xmm2r8(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SN(i+9) = compute_SN_xmm2r8(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SN(i+1) = compute_SN_xmm2r8(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SN(i+1) = compute_SN_xmm2r8(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SN(i+12)= compute_SN_xmm2r8(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SN(i+13)= compute_SN_xmm2r8(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SN(i+14)= compute_SN_xmm2r8(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SN(i+15)= compute_SN_xmm2r8(R,p15,g15)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_16x_omp_xmm2r8
     
     
     subroutine compute_SN_unroll_8x_xmm2r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_8x_xmm2r8
        !dir$ attributes forceinline :: compute_SN_unroll_8x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm2r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_xmm2r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_xmm2r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_xmm2r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_xmm2r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_xmm2r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_xmm2r8(R,p7,g7)
        end do
     end subroutine compute_SN_unroll_8x_xmm2r8
     
     
     subroutine compute_SN_unroll_8x_omp_xmm2r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_8x_omp_xmm2r8
        !dir$ attributes forceinline :: compute_SN_unroll_8x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_8x_omp_xmm2r8
        use omp_lib
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7)                   &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm2r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_xmm2r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_xmm2r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SN(i+4) = compute_SN_xmm2r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SN(i+5) = compute_SN_xmm2r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SN(i+6) = compute_SN_xmm2r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SN(i+7) = compute_SN_xmm2r8(R,p7,g7)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_8x_omp_xmm2r8
     
     
     subroutine compute_SN_unroll_4x_xmm2r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_4x_xmm2r8
        !dir$ attributes forceinline :: compute_SN_unroll_4x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1,p2,p3
        type(XMM2r8_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm2r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_xmm2r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_xmm2r8(R,p3,g3)
        end do
     end subroutine compute_SN_unroll_4x_xmm2r8
     
     
     subroutine compute_SN_unroll_4x_omp_xmm2r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_4x_omp_xmm2r8
        !dir$ attributes forceinline :: compute_SN_unroll_4x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_4x_omp_xmm2r8
        use omp_lib
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1,p2,p3
        type(XMM2r8_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SN:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3)                                           &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm2r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SN(i+2) = compute_SN_xmm2r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SN(i+3) = compute_SN_xmm2r8(R,p3,g3)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_4x_omp_xmm2r8
     
     
     subroutine compute_SN_unroll_2x_xmm2r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_2x_xmm2r8
        !dir$ attributes forceinline :: compute_SN_unroll_2x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1
        type(XMM2r8_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm2r8(R,p1,g1)
        end do
     end subroutine compute_SN_unroll_2x_xmm2r8
     
     
     subroutine compute_SN_unroll_2x_omp_xmm2r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_unroll_2x_omp_xmm2r8
        !dir$ attributes forceinline :: compute_SN_unroll_2x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_unroll_2x_omp_xmm2r8
        use omp_lib
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1
        type(XMM2r8_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SN(i) = compute_SN_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1)            &
        !$omp shared(n,phi,gamma,SN)
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SN(i+1) = compute_SN_xmm2r8(R,p1,g1)
        end do
        !$omp end parallel do
     end subroutine compute_SN_unroll_2x_omp_xmm2r8


     subroutine compute_SN_rolled_xmm2r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_rolled_xmm2r8
        !dir$ attributes forceinline :: compute_SN_rolled_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0
        type(XMM2r8_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(48
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm2r8(R,p0,g0)
        end do
     end subroutine compute_SN_rolled_xmm2r8


     subroutine compute_SN_rolled_omp_xmm2r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SN_rolled_omp_xmm2r8
        !dir$ attributes forceinline :: compute_SN_rolled_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SN_rolled_omp_xmm2r8
        use omp_lib
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0
        type(XMM2r8_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp private(i)  shared(n,phi,gamma,SN)      
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SN(i)   = compute_SN_xmm2r8(R,p0,g0)
        end do
        !$omp end parallel do
     end subroutine compute_SN_rolled_omp_xmm2r8

     
     subroutine compute_SN_dispatch_xmm2r8(R,phi,gamma,SN,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 16:: compute_SN_dispatch_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SN
        integer(kind=i4),                 intent(in) :: n
        integer(kind=i4),                 intent(in) :: unroll_cnt
        logical(kind=i4),                 intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
              case (16)
                call compute_SN_unroll_16x_omp_xmm2r8(R,phi,gamma,SN,n)
              case (8)
                call compute_SN_unroll_8x_omp_xmm2r8(R,phi,gamma,SN,n) 
              case (4)
                call compute_SN_unroll_4x_omp_xmm2r8(R,phi,gamma,SN,n)
              case (2)
                call compute_SN_unroll_2x_omp_xmm2r8(R,phi,gamma,SN,n)
              case (0)
                call compute_SN_rolled_omp_xmm2r8(R,phi,gamma,SN,n)
              case default
                return
            end select
         else
            select case (unroll_cnt)
              case (16)
                call compute_SN_unroll_16x_xmm2r8(R,phi,gamma,SN,n)
              case (8)
                call compute_SN_unroll_8x_xmm2r8(R,phi,gamma,SN,n) 
              case (4)
                call compute_SN_unroll_4x_xmm2r8(R,phi,gamma,SN,n)
              case (2)
                call compute_SN_unroll_2x_xmm2r8(R,phi,gamma,SN,n)
              case (0)
                call compute_SN_rolled_xmm2r8(R,phi,gamma,SN,n)
              case default
                return
            end select
         end if
     end subroutine compute_SN_dispatch_xmm2r8
     
     
!/////////////////////////////////////////////////////////////////////////////////////!
!/////////////////////////////////////////////////////////////////////////////////////!


    pure function compute_SM_xmm4r4(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_xmm4r4
        !dir$ attributes forceinline :: compute_SM_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  intent(in), optional :: phi
        type(XMM4r4_t),  intent(in), optional :: gamma
        type(XMM4r4_t) :: SM
        type(XMM4r4_t), automatic :: SN
        !dir$ attributes align : 16:: SN
        SN = compute_SN_xmm4r4(R,phi,gamma)
        SM = 2.0_sp*SN.v
     end function compute_SM_xmm4r4


     subroutine compute_SM_unroll_16x_xmm4r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_16x_xmm4r4
        !dir$ attributes forceinline :: compute_SM_unroll_16x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM4r4_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(XMM4r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SN_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SN_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SN_xmm4r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SN_xmm4r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SN_xmm4r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SN_xmm4r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SN_xmm4r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SN_xmm4r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SN_xmm4r4(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SM(i+8) = compute_SN_xmm4r4(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SM(i+9) = compute_SN_xmm4r4(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SM(i+1) = compute_SN_xmm4r4(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SM(i+1) = compute_SN_xmm4r4(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SM(i+12)= compute_SN_xmm4r4(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SM(i+13)= compute_SN_xmm4r4(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SM(i+14)= compute_SN_xmm4r4(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SM(i+15)= compute_SN_xmm4r4(R,p15,g15)
        end do
     end subroutine compute_SM_unroll_16x_xmm4r4


     subroutine compute_SM_unroll_16x_omp_xmm4r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_16x_omp_xmm4r4
        !dir$ attributes forceinline :: compute_SM_unroll_16x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_omp_xmm4r4
        use omp_lib
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM4r4_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(XMM4r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)  &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7,p8,g8)             &
        !$omp private(p9,g9,p10,g10,p11,g11,p12,g12,p13)               &
        !$omp private(g13,p14,g14,p15,g15)                             &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm4r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_xmm4r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_xmm4r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_xmm4r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_xmm4r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_xmm4r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_xmm4r4(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SM(i+8) = compute_SM_xmm4r4(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SM(i+9) = compute_SM_xmm4r4(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SM(i+1) = compute_SM_xmm4r4(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SM(i+1) = compute_SM_xmm4r4(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SM(i+12)= compute_SM_xmm4r4(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SM(i+13)= compute_SM_xmm4r4(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SM(i+14)= compute_SM_xmm4r4(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SM(i+15)= compute_SM_xmm4r4(R,p15,g15)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_16x_omp_xmm4r4


     subroutine compute_SM_unroll_8x_xmm4r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_8x_xmm4r4
        !dir$ attributes forceinline :: compute_SM_unroll_8x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm4r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_xmm4r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_xmm4r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_xmm4r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_xmm4r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_xmm4r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_xmm4r4(R,p7,g7)
        end do
     end subroutine compute_SM_unroll_8x_xmm4r4
     
     
     subroutine compute_SM_unroll_8x_omp_xmm4r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_8x_omp_xmm4r4
        !dir$ attributes forceinline :: compute_SM_unroll_8x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_omp_xmm4r4
        use omp_lib
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)  &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7)                   &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm4r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_xmm4r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_xmm4r4(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_xmm4r4(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_xmm4r4(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_xmm4r4(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_xmm4r4(R,p7,g7)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_8x_omp_xmm4r4


     subroutine compute_SM_unroll_4x_xmm4r4(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_4x_xmm4r4
        !dir$ attributes forceinline :: compute_SM_unroll_4x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1,p2,p3
        type(XMM4r4_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm4r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_xmm4r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_xmm4r4(R,p3,g3)
        end do
     end subroutine compute_SM_unroll_4x_xmm4r4


     subroutine compute_SM_unroll_4x_omp_xmm4r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_4x_omp_xmm4r4
        !dir$ attributes forceinline :: compute_SM_unroll_4x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_omp_xmm4r4
        use omp_lib
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1,p2,p3
        type(XMM4r4_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)  &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3)                                           &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm4r4(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_xmm4r4(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_xmm4r4(R,p3,g3)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_4x_omp_xmm4r4


     subroutine compute_SM_unroll_2x_xmm4r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_2x_xmm4r4
        !dir$ attributes forceinline :: compute_SM_unroll_2x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1
        type(XMM4r4_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm4r4(R,p1,g1)
        end do
     end subroutine compute_SM_unroll_2x_xmm4r4


     subroutine compute_SM_unroll_2x_omp_xmm4r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_2x_omp_xmm4r4
        !dir$ attributes forceinline :: compute_SM_unroll_2x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_omp_xmm4r4
        use omp_lib
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0,p1
        type(XMM4r4_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm4r4(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:64
        !dir$ assume_aligned gamma:64
        !dir$ assume_aligned SM:64
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1)            &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm4r4(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm4r4(R,p1,g1)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_2x_omp_xmm4r4
     
     
     subroutine compute_SM_rolled_xmm4r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_rolled_xmm4r4
        !dir$ attributes forceinline :: compute_SM_rolled_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0
        type(XMM4r4_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm4r4(R,p0,g0)
        end do
     end subroutine compute_SM_rolled_xmm4r4


     subroutine compute_SM_rolled_omp_xmm4r4(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_rolled_omp_xmm4r4
        !dir$ attributes forceinline :: compute_SM_rolled_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_omp_xmm4r4
        use omp_lib
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM4r4_t), automatic :: p0
        type(XMM4r4_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp private(i)  shared(n,phi,gamma,SM)      
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm4r4(R,p0,g0)
        end do
        !$omp end parallel do
     end subroutine compute_SM_rolled_omp_xmm4r4

     
     subroutine compute_SM_dispatch_xmm4r4(R,phi,gamma,SM,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 16:: compute_SM_dispatch_xmm4r4
        type(XMM4r4_t),  intent(in) :: R
        type(XMM4r4_t),  dimension(1:n), intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n), intent(in) :: gamma
        type(XMM4r4_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        integer(kind=i4),                 intent(in) :: unroll_cnt
        logical(kind=i4),                 intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
              case (16)
                call compute_SM_unroll_16x_omp_xmm4r4(R,phi,gamma,SM,n)
              case (8)
                call compute_SM_unroll_8x_omp_xmm4r4(R,phi,gamma,SM,n) 
              case (4)
                call compute_SM_unroll_4x_omp_xmm4r4(R,phi,gamma,SM,n)
              case (2)
                call compute_SM_unroll_2x_omp_xmm4r4(R,phi,gamma,SM,n)
              case (0)
                call compute_SM_rolled_omp_xmm4r4(R,phi,gamma,SM,n)
              case default
                return
            end select
         else
            select case (unroll_cnt)
              case (16)
                call compute_SM_unroll_16x_xmm4r4(R,phi,gamma,SM,n)
              case (8)
                call compute_SM_unroll_8x_xmm4r4(R,phi,gamma,SM,n) 
              case (4)
                call compute_SM_unroll_4x_xmm4r4(R,phi,gamma,SM,n)
              case (2)
                call compute_SM_unroll_2x_xmm4r4(R,phi,gamma,SM,n)
              case (0)
                call compute_SM_rolled_xmm4r4(R,phi,gamma,SM,n)
              case default
                return
            end select
         end if
     end subroutine compute_SM_dispatch_xmm4r4
     
     
! ///////////////////////////////////////////////////////////////////////////////////!
! ///////////////////////////////////////////////////////////////////////////////////!


    pure function compute_SM_xmm2r8(R,phi,gamma) result(SM)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_xmm2r8
        !dir$ attributes forceinline :: compute_SM_ymmyr8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  intent(in), optional :: phi
        type(XMM2r8_t),  intent(in), optional :: gamma
        type(XMM2r8_t) :: SM
        type(XMM2r8_t), automatic :: SN
        !dir$ attributes align : 16:: SN
        SN = compute_SN_xmm2r8(R,phi,gamma)
        SM = 2.0_dp*SN.v
     end function compute_SM_xmm2r8


     subroutine compute_SM_unroll_16x_xmm2r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_16x_xmm2r8
        !dir$ attributes forceinline :: compute_SM_unroll_16x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM2r8_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(XMM2r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm2r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_xmm2r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_xmm2r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_xmm2r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_xmm2r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_xmm2r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_xmm2r8(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SM(i+8) = compute_SM_xmm2r8(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SM(i+9) = compute_SM_xmm2r8(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SM(i+1) = compute_SM_xmm2r8(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SM(i+1) = compute_SM_xmm2r8(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SM(i+12)= compute_SM_xmm2r8(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SM(i+13)= compute_SM_xmm2r8(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SM(i+14)= compute_SM_xmm2r8(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SM(i+15)= compute_SM_xmm2r8(R,p15,g15)
        end do
     end subroutine compute_SM_unroll_16x_xmm2r8


     subroutine compute_SM_unroll_16x_omp_xmm2r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_16x_omp_xmm2r8
        !dir$ attributes forceinline :: compute_SM_unroll_16x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_16x_omp_xmm2r8
        use omp_lib
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM2r8_t), automatic :: p8,p9,p10,p11,p12,p13,p14,p15
        type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        type(XMM2r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)  &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7,p8,g8)             &
        !$omp private(p9,g9,p10,g10,p11,g11,p12,g12,p13)               &
        !$omp private(g13,p14,g14,p15,g15)                             &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,16
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm2r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_xmm2r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_xmm2r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_xmm2r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_xmm2r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_xmm2r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_xmm2r8(R,p7,g7)
           p8      = phi(i+8)
           g8      = gamma(i+8)
           SM(i+8) = compute_SM_xmm2r8(R,p8,g8)
           p9      = phi(i+9)
           g9      = gamma(i+9)
           SM(i+9) = compute_SM_xmm2r8(R,p9,g9)
           p10     = phi(i+10)
           g10     = gamma(i+10)
           SM(i+1) = compute_SM_xmm2r8(R,p10,g10)
           p11     = phi(i+11)
           g11     = gamma(i+11)
           SM(i+1) = compute_SM_xmm2r8(R,p11,g11)
           p12     = phi(i+12)
           g12     = gamma(i+12)
           SM(i+12)= compute_SM_xmm2r8(R,p12,g12)
           p13     = phi(i+13)
           g13     = gamma(i+13)
           SM(i+13)= compute_SM_xmm2r8(R,p13,g13)
           p14     = phi(i+14)
           g14     = gamma(i+14)
           SM(i+14)= compute_SM_xmm2r8(R,p14,g14)
           p15     = phi(i+15)
           g15     = gamma(i+15)
           SM(i+15)= compute_SM_xmm2r8(R,p15,g15)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_16x_omp_xmm2r8


     subroutine compute_SM_unroll_8x_xmm2r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_8x_xmm2r8
        !dir$ attributes forceinline :: compute_SM_unroll_8x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm2r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_xmm2r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_xmm2r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_xmm2r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_xmm2r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_xmm2r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_xmm2r8(R,p7,g7)
        end do
     end subroutine compute_SM_unroll_8x_xmm2r8


     subroutine compute_SM_unroll_8x_omp_xmm2r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_8x_omp_xmm2r8
        !dir$ attributes forceinline :: compute_SM_unroll_8x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_8x_omp_xmm2r8
        use omp_lib
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1,p2,p3,p4,p5,p6,p7
        type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3,p4,g4,p5,g5,p6,g6,p7,g7)                   &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,8
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm2r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_xmm2r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_xmm2r8(R,p3,g3)
           p4      = phi(i+4)
           g4      = gamma(i+4)
           SM(i+4) = compute_SM_xmm2r8(R,p4,g4)
           p5      = phi(i+5)
           g5      = gamma(i+5)
           SM(i+5) = compute_SM_xmm2r8(R,p5,g5)
           p6      = phi(i+6)
           g6      = gamma(i+6)
           SM(i+6) = compute_SM_xmm2r8(R,p6,g6)  
           p7      = phi(i+7)
           g7      = gamma(i+7)
           SM(i+7) = compute_SM_xmm2r8(R,p7,g7)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_8x_omp_xmm2r8


     subroutine compute_SM_unroll_4x_xmm2r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_4x_xmm2r8
        !dir$ attributes forceinline :: compute_SM_unroll_4x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1,p2,p3
        type(XMM2r8_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm2r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_xmm2r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_xmm2r8(R,p3,g3)
        end do
     end subroutine compute_SM_unroll_4x_xmm2r8


     subroutine compute_SM_unroll_4x_omp_xmm2r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_4x_omp_xmm2r8
        !dir$ attributes forceinline :: compute_SM_unroll_4x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_4x_omp_xmm2r8
        use omp_lib
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1,p2,p3
        type(XMM2r8_t), automatic :: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1,p2,g2)            &
        !$omp private(p3,g3)                                           &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,4
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm2r8(R,p1,g1)
           p2      = phi(i+2)
           g2      = gamma(i+2)
           SM(i+2) = compute_SM_xmm2r8(R,p2,g2)
           p3      = phi(i+3)
           g3      = gamma(i+3)
           SM(i+3) = compute_SM_xmm2r8(R,p3,g3)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_4x_omp_xmm2r8


     subroutine compute_SM_unroll_2x_xmm2r8(R,phi,gamma,SN,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_2x_xmm2r8
        !dir$ attributes forceinline :: compute_SM_unroll_2x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1
        type(XMM2r8_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm2r8(R,p1,g1)
        end do
     end subroutine compute_SM_unroll_2x_xmm2r8


     subroutine compute_SM_unroll_2x_omp_xmm2r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_unroll_2x_omp_xmm2r8
        !dir$ attributes forceinline :: compute_SM_unroll_2x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_unroll_2x_omp_xmm2r8
        use omp_lib
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0,p1
        type(XMM2r8_t), automatic :: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              SM(i) = compute_SM_xmm2r8(R,phi(i),gamma(i))
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp firstprivate(m1) private(i,p0,g0,p1,g1)            &
        !$omp shared(n,phi,gamma,SM)
        do i=m1,n,2
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm2r8(R,p0,g0)
           p1      = phi(i+1)
           g1      = gamma(i+1)
           SM(i+1) = compute_SM_xmm2r8(R,p1,g1)
        end do
        !$omp end parallel do
     end subroutine compute_SM_unroll_2x_omp_xmm2r8


     subroutine compute_SM_rolled_xmm2r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_rolled_xmm2r8
        !dir$ attributes forceinline :: compute_SM_rolled_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0
        type(XMM2r8_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SN:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector multiple_gather_scatter_by_shuffles 
        !dir$ vector always
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm2r8(R,p0,g0)
        end do
     end subroutine compute_SM_rolled_xmm2r8


     subroutine compute_SM_rolled_omp_xmm2r8(R,phi,gamma,SM,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: compute_SM_rolled_omp_xmm2r8
        !dir$ attributes forceinline :: compute_SM_rolled_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: compute_SM_rolled_omp_xmm2r8
        use omp_lib
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        type(XMM2r8_t), automatic :: p0
        type(XMM2r8_t), automatic :: g0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned gamma:16
        !dir$ assume_aligned SM:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024)   &
        !$omp private(i)  shared(n,phi,gamma,SN)      
        do i=1,n
           p0      = phi(i)
           g0      = gamma(i)
           SM(i)   = compute_SM_xmm2r8(R,p0,g0)
        end do
        !$omp end parallel do
     end subroutine compute_SM_rolled_omp_xmm2r8

     
     subroutine compute_SM_dispatch_xmm2r8(R,phi,gamma,SM,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 16:: compute_SM_dispatch_xmm2r8
        type(XMM2r8_t),  intent(in) :: R
        type(XMM2r8_t),  dimension(1:n), intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n), intent(in) :: gamma
        type(XMM2r8_t),  dimension(1:n), intent(out):: SM
        integer(kind=i4),                 intent(in) :: n
        integer(kind=i4),                 intent(in) :: unroll_cnt
        logical(kind=i4),                 intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
              case (16)
                call compute_SM_unroll_16x_omp_xmm2r8(R,phi,gamma,SM,n)
              case (8)
                call compute_SM_unroll_8x_omp_xmm2r8(R,phi,gamma,SM,n) 
              case (4)
                call compute_SM_unroll_4x_omp_xmm2r8(R,phi,gamma,SM,n)
              case (2)
                call compute_SM_unroll_2x_omp_xmm2r8(R,phi,gamma,SM,n)
              case (0)
                call compute_SM_rolled_omp_xmm2r8(R,phi,gamma,SM,n)
              case default
                return
            end select
         else
            select case (unroll_cnt)
              case (16)
                call compute_SM_unroll_16x_xmm2r8(R,phi,gamma,SM,n)
              case (8)
                call compute_SM_unroll_8x_xmm2r8(R,phi,gamma,SM,n) 
              case (4)
                call compute_SM_unroll_4x_xmm2r8(R,phi,gamma,SM,n)
              case (2)
                call compute_SM_unroll_2x_xmm2r8(R,phi,gamma,SM,n)
              case (0)
                call compute_SM_rolled_xmm2r8(R,phi,gamma,SM,n)
              case default
                return
            end select
         end if
     end subroutine compute_SM_dispatch_xmm2r8

!/////////////////////////////////////////////////////////////////////////////!
!/////////////////////////////////////////////////////////////////////////////!


     !Сканирующее зеркало для обеспечения осмотра всего поля
     !обзора ф необходимо повернуть на угол, обеспечивающий 
     !совмещение края изображения источника излучения с отверстием 
     !диафрагмы а, находящимся в центре поля. Для этого необходимо 
     !повернуть изображение светящейся точки S на угол ф/2
     ! Formula 1, p. 56


     pure function ratio_FH_xmm4r4(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_xmm4r4
        !dir$ attributes forceinline :: ratio_FH_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_xmm4r4
        type(XMM4r4_t),  intent(in) :: psi
        type(XMM4r4_t),  intent(in) :: phi
        type(XMM4r4_t) :: FH
        type(XMM4r4_t), parameter :: half = XMM4r4_t(0.5_sp)
        type(XMM4r4_t), automatic :: hpsi,hphi
        hpsi = half.v*psi.v
        hphi = half.v*phi.v
        FH   = tan(hpsi.v)/tan(hphi.v)
     end function ratio_FH_xmm4r4


     subroutine ratio_FH_unroll_16x_xmm4r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_16x_xmm4r4
        !dir$ attributes forceinline :: ratio_FH_unroll_16x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_16x_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM4r4_t), automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 16:: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(XMM4r4_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(XMM4r4_t), automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 16:: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm4r4(ps0,ph0)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,16
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm4r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm4r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_xmm4r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_xmm4r4(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_xmm4r4(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_xmm4r4(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_xmm4r4(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_xmm4r4(ps7,ph7)
            ps8     = psi(i+8)
            ph8     = phi(i+8)
            FH(i+8) = ratio_FH_xmm4r4(ps8,ph8)
            ps9     = psi(i+9)
            ph9     = phi(i+9)
            FH(i+9) = ratio_FH_xmm4r4(ps9,ph9)
            ps10    = psi(i+10)
            ph10    = phi(i+10)
            FH(i+10)= ratio_FH_xmm4r4(ps10,ph10)
            ps11    = psi(i+11)
            ph11    = phi(i+11)
            FH(i+11)= ratio_FH_xmm4r4(ps11,ph11)
            ps12    = psi(i+12)
            ph12    = phi(i+12)
            FH(i+12)= ratio_FH_xmm4r4(ps12,ph12)
            ps13    = psi(i+13)
            ph13    = phi(i+13)
            FH(i+13)= ratio_FH_xmm4r4(ps13,ph13)
            ps14    = psi(i+14)
            ph14    = phi(i+14)
            FH(i+14)= ratio_FH_xmm4r4(ps14,ph14) 
            ps15    = psi(i+15)
            ph15    = phi(i+15)
            FH(i+15)= ratio_FH_xmm4r4(ps15,ph15)
        end do
     end subroutine ratio_FH_unroll_16x_xmm4r4


     subroutine ratio_FH_unroll_16x_omp_xmm4r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_16x_omp_xmm4r4
        !dir$ attributes forceinline :: ratio_FH_unroll_16x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_16x_omp_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM4r4_t), automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 16:: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(XMM4r4_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(XMM4r4_t), automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 16:: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm4r4(ps0,ph0)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3,ps4,ph4,ps5,ph5,ps6,ph6,ps7,ph7)       &
        !$omp private(ps8,ph8,ps9,ph9,ps10,ph10,ps11,ph11,ps12,ph12) &
        !$omp private(ps13,ph13,ps14,ph14,ps15,ph15)                 &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,16
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm4r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm4r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_xmm4r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_xmm4r4(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_xmm4r4(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_xmm4r4(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_xmm4r4(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_xmm4r4(ps7,ph7)
            ps8     = psi(i+8)
            ph8     = phi(i+8)
            FH(i+8) = ratio_FH_xmm4r4(ps8,ph8)
            ps9     = psi(i+9)
            ph9     = phi(i+9)
            FH(i+9) = ratio_FH_xmm4r4(ps9,ph9)
            ps10    = psi(i+10)
            ph10    = phi(i+10)
            FH(i+10)= ratio_FH_xmm4r4(ps10,ph10)
            ps11    = psi(i+11)
            ph11    = phi(i+11)
            FH(i+11)= ratio_FH_xmm4r4(ps11,ph11)
            ps12    = psi(i+12)
            ph12    = phi(i+12)
            FH(i+12)= ratio_FH_xmm4r4(ps12,ph12)
            ps13    = psi(i+13)
            ph13    = phi(i+13)
            FH(i+13)= ratio_FH_xmm4r4(ps13,ph13)
            ps14    = psi(i+14)
            ph14    = phi(i+14)
            FH(i+14)= ratio_FH_xmm4r4(ps14,ph14) 
            ps15    = psi(i+15)
            ph15    = phi(i+15)
            FH(i+15)= ratio_FH_xmm4r4(ps15,ph15)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_16x_omp_xmm4r4


     subroutine ratio_FH_unroll_8x_xmm4r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_8x_xmm4r4
        !dir$ attributes forceinline :: ratio_FH_unroll_8x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_8x_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM4r4_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm4r4(ps0,ph0)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,8
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm4r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm4r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_xmm4r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_xmm4r4(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_xmm4r4(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_xmm4r4(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_xmm4r4(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_xmm4r4(ps7,ph7)
         end do
     end subroutine ratio_FH_unroll_8x_xmm4r4


     subroutine ratio_FH_unroll_8x_omp_xmm4r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_8x_omp_xmm4r4
        !dir$ attributes forceinline :: ratio_FH_unroll_8x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_8x_omp_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM4r4_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm4r4(ps0,ph0)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3,ps4,ph4,ps5,ph5,ps6,ph6,ps7,ph7)       &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,8
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm4r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm4r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_xmm4r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_xmm4r4(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_xmm4r4(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_xmm4r4(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_xmm4r4(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_xmm4r4(ps7,ph7)
         end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_8x_omp_xmm4r4


     subroutine ratio_FH_unroll_4x_xmm4r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_4x_xmm4r4
        !dir$ attributes forceinline :: ratio_FH_unroll_4x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_4x_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3
        type(XMM4r4_t), automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm4r4(ps0,ph0)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,4
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm4r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm4r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_xmm4r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_xmm4r4(ps3,ph3)
         end do
     end subroutine ratio_FH_unroll_4x_xmm4r4


     subroutine ratio_FH_unroll_4x_omp_xmm4r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_4x_omp_xmm4r4
        !dir$ attributes forceinline :: ratio_FH_unroll_4x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_4x_omp_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3
        type(XMM4r4_t), automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm4r4(ps0,ph0)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3)                                       &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,4
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm4r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm4r4(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_xmm4r4(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_xmm4r4(ps3,ph3)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_4x_omp_xmm4r4



       subroutine ratio_FH_unroll_2x_xmm4r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_2x_xmm4r4
        !dir$ attributes forceinline :: ratio_FH_unroll_2x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_2x_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: ps0,ps1
        !dir$ attributes align : 16:: ps0,ps1
        type(XMM4r4_t), automatic :: ph0,ph1
        !dir$ attributes align : 16:: ph0,ph1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm4r4(ps0,ph0)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,2
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm4r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm4r4(ps1,ph1)
        end do
     end subroutine ratio_FH_unroll_2x_xmm4r4


     subroutine ratio_FH_unroll_2x_omp_xmm4r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_2x_omp_xmm4r4
        !dir$ attributes forceinline :: ratio_FH_unroll_2x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_2x_omp_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: ps0,ps1
        !dir$ attributes align : 16:: ps0,ps1
        type(XMM4r4_t), automatic :: ph0,ph1
        !dir$ attributes align : 16:: ph0,ph1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm4r4(ps0,ph0)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1)            &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,2
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm4r4(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm4r4(ps1,ph1)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_2x_omp_xmm4r4



      subroutine ratio_FH_rolled_xmm4r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_rolled_xmm4r4
        !dir$ attributes forceinline :: ratio_FH_rolled_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_rolled_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: ps0
        !dir$ attributes align : 16:: ps0
        type(XMM4r4_t), automatic :: ph0
        !dir$ attributes align : 16:: ph0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=1,n
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm4r4(ps0,ph0)
        end do
     end subroutine ratio_FH_rolled_xmm4r4


     subroutine ratio_FH_rolled_omp_xmm4r4(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_rolled_omp_xmm4r4
        !dir$ attributes forceinline :: ratio_FH_rolled_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_rolled_omp_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: ps0
        !dir$ attributes align : 16:: ps0
        type(XMM4r4_t), automatic :: ph0
        !dir$ attributes align : 16:: ph0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(i,ps0,ph0)                    &
        !$omp shared(n,psi,phi,FH)
        do i=1,n
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm4r4(ps0,ph0)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_rolled_omp_xmm4r4



     subroutine ratio_FH_dispatch_xmm4r4(psi,phi,FH,n,unroll_cnt,omp_ver)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_dispatch_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in) :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        integer(kind=i4),                  intent(in) :: unroll_cnt
        logical(kind=i4),                  intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
               case (16)
                  call ratio_FH_unroll_16x_omp_xmm4r4(psi,phi,FH,n)
               case (8)
                  call ratio_FH_unroll_8x_omp_xmm4r4(psi,phi,FH,n) 
               case (4)
                  call ratio_FH_unroll_4x_omp_xmm4r4(psi,phi,FH,n)
               case (2)
                  call ratio_FH_unroll_2x_omp_xmm4r4(psi,phi,FH,n)
               case (0)
                  call ratio_FH_rolled_omp_xmm4r4(psi,phi,FH,n)
               case default
                  return
            end select
         else
            select case (unroll_cnt)
               case (16)
                  call ratio_FH_unroll_16x_xmm4r4(psi,phi,FH,n)
               case (8)
                  call ratio_FH_unroll_8x_xmm4r4(psi,phi,FH,n) 
               case (4)
                  call ratio_FH_unroll_4x_xmm4r4(psi,phi,FH,n)
               case (2)
                  call ratio_FH_unroll_2x_xmm4r4(psi,phi,FH,n)
               case (0)
                  call ratio_FH_rolled_xmm4r4(psi,phi,FH,n)
               case default
                  return
            end select
         end if
     end subroutine ratio_FH_dispatch_xmm4r4
      
!/////////////////////////////////////////////////////////////////////!
!/////////////////////////////////////////////////////////////////////!

     
     pure function ratio_FH_xmm2r8(psi,phi) result(FH)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_xmm2r8
        !dir$ attributes forceinline :: ratio_FH_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_xmm2r8
        type(XMM2r8_t),  intent(in) :: psi
        type(XMM2r8_t),  intent(in) :: phi
        type(XMM2r8_t) :: FH
        type(XMM2r8_t), parameter :: half = XMM2r8_t(0.5_dp)
        type(XMM2r8_t), automatic :: hpsi,hphi
        hpsi = half.v*psi.v
        hphi = half.v*phi.v
        FH   = tan(hpsi.v)/tan(hphi.v)
     end function ratio_FH_xmm2r8


     subroutine ratio_FH_unroll_16x_xmm2r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_16x_xmm2r8
        !dir$ attributes forceinline :: ratio_FH_unroll_16x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_16x_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM2r8_t), automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 16:: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(XMM2r8_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(XMM2r8_t), automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 16:: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm2r8(ps0,ph0)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,16
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm2r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm2r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_xmm2r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_xmm2r8(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_xmm2r8(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_xmm2r8(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_xmm2r8(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_xmm2r8(ps7,ph7)
            ps8     = psi(i+8)
            ph8     = phi(i+8)
            FH(i+8) = ratio_FH_xmm2r8(ps8,ph8)
            ps9     = psi(i+9)
            ph9     = phi(i+9)
            FH(i+9) = ratio_FH_xmm2r8(ps9,ph9)
            ps10    = psi(i+10)
            ph10    = phi(i+10)
            FH(i+10)= ratio_FH_xmm2r8(ps10,ph10)
            ps11    = psi(i+11)
            ph11    = phi(i+11)
            FH(i+11)= ratio_FH_xmm2r8(ps11,ph11)
            ps12    = psi(i+12)
            ph12    = phi(i+12)
            FH(i+12)= ratio_FH_xmm2r8(ps12,ph12)
            ps13    = psi(i+13)
            ph13    = phi(i+13)
            FH(i+13)= ratio_FH_xmm2r8(ps13,ph13)
            ps14    = psi(i+14)
            ph14    = phi(i+14)
            FH(i+14)= ratio_FH_xmm2r8(ps14,ph14) 
            ps15    = psi(i+15)
            ph15    = phi(i+15)
            FH(i+15)= ratio_FH_xmm2r8(ps15,ph15)
        end do
     end subroutine ratio_FH_unroll_16x_xmm2r8


     subroutine ratio_FH_unroll_16x_omp_xmm2r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_16x_omp_xmm2r8
        !dir$ attributes forceinline :: ratio_FH_unroll_16x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_16x_omp_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM2r8_t), automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 16:: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(XMM2r8_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(XMM2r8_t), automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 16:: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm2r8(ps0,ph0)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3,ps4,ph4,ps5,ph5,ps6,ph6,ps7,ph7)       &
        !$omp private(ps8,ph8,ps9,ph9,ps10,ph10,ps11,ph11,ps12,ph12) &
        !$omp private(ps13,ph13,ps14,ph14,ps15,ph15)                 &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,16
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm2r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm2r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_xmm2r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_xmm2r8(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_xmm2r8(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_xmm2r8(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_xmm2r8(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_xmm2r8(ps7,ph7)
            ps8     = psi(i+8)
            ph8     = phi(i+8)
            FH(i+8) = ratio_FH_xmm2r8(ps8,ph8)
            ps9     = psi(i+9)
            ph9     = phi(i+9)
            FH(i+9) = ratio_FH_xmm2r8(ps9,ph9)
            ps10    = psi(i+10)
            ph10    = phi(i+10)
            FH(i+10)= ratio_FH_xmm2r8(ps10,ph10)
            ps11    = psi(i+11)
            ph11    = phi(i+11)
            FH(i+11)= ratio_FH_xmm2r8(ps11,ph11)
            ps12    = psi(i+12)
            ph12    = phi(i+12)
            FH(i+12)= ratio_FH_xmm2r8(ps12,ph12)
            ps13    = psi(i+13)
            ph13    = phi(i+13)
            FH(i+13)= ratio_FH_xmm2r8(ps13,ph13)
            ps14    = psi(i+14)
            ph14    = phi(i+14)
            FH(i+14)= ratio_FH_xmm2r8(ps14,ph14) 
            ps15    = psi(i+15)
            ph15    = phi(i+15)
            FH(i+15)= ratio_FH_xmm2r8(ps15,ph15)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_16x_omp_xmm2r8


     subroutine ratio_FH_unroll_8x_xmm2r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_8x_xmm2r8
        !dir$ attributes forceinline :: ratio_FH_unroll_8x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_8x_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM2r8_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm2r8(ps0,ph0)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,8
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm2r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm2r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_xmm2r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_xmm2r8(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_xmm2r8(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_xmm2r8(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_xmm2r8(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_xmm2r8(ps7,ph7)
         end do
     end subroutine ratio_FH_unroll_8x_xmm2r8


     subroutine ratio_FH_unroll_8x_omp_xmm2r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_8x_omp_xmm2r8
        !dir$ attributes forceinline :: ratio_FH_unroll_8x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_8x_omp_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM2r8_t), automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm2r8(ps0,ph0)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3,ps4,ph4,ps5,ph5,ps6,ph6,ps7,ph7)       &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,8
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm2r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm2r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_xmm2r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_xmm2r8(ps3,ph3)
            ps4     = psi(i+4)
            ph4     = phi(i+4)
            FH(i+4) = ratio_FH_xmm2r8(ps4,ph4) 
            ps5     = psi(i+5)
            ph5     = phi(i+5)
            FH(i+1) = ratio_FH_xmm2r8(ps5,ph5)
            ps6     = psi(i+6)
            ph6     = phi(i+6)
            FH(i+6) = ratio_FH_xmm2r8(ps6,ph6)
            ps7     = psi(i+7)
            ph7     = phi(i+7)
            FH(i+7) = ratio_FH_xmm2r8(ps7,ph7)
         end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_8x_omp_xmm2r8


     subroutine ratio_FH_unroll_4x_xmm2r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_4x_xmm2r8
        !dir$ attributes forceinline :: ratio_FH_unroll_4x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_4x_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3
        type(XMM2r8_t), automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm2r8(ps0,ph0)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,4
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm2r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm2r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_xmm2r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_xmm2r8(ps3,ph3)
         end do
     end subroutine ratio_FH_unroll_4x_xmm2r8


     subroutine ratio_FH_unroll_4x_omp_xmm2r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_4x_omp_xmm2r8
        !dir$ attributes forceinline :: ratio_FH_unroll_4x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_4x_omp_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3
        type(XMM2r8_t), automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm2r8(ps0,ph0)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1,ps2,ph2)    &
        !$omp private(ps3,ph3)                                       &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,4
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm2r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm2r8(ps1,ph1)
            ps2     = psi(i+2)
            ph2     = phi(i+2)
            FH(i+2) = ratio_FH_xmm2r8(ps2,ph2)
            ps3     = psi(i+3)
            ph3     = phi(i+3)
            FH(i+3) = ratio_FH_xmm2r8(ps3,ph3)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_4x_omp_xmm2r8
     
     
     subroutine ratio_FH_unroll_2x_xmm2r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_2x_xmm2r8
        !dir$ attributes forceinline :: ratio_FH_unroll_2x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_2x_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: ps0,ps1
        !dir$ attributes align : 16:: ps0,ps1
        type(XMM2r8_t), automatic :: ph0,ph1
        !dir$ attributes align : 16:: ph0,ph1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm2r8(ps0,ph0)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,2
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm2r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm2r8(ps1,ph1)
        end do
     end subroutine ratio_FH_unroll_2x_xmm2r8


     subroutine ratio_FH_unroll_2x_omp_xmm2r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_unroll_2x_omp_xmm2r8
        !dir$ attributes forceinline :: ratio_FH_unroll_2x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_unroll_2x_omp_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: ps0,ps1
        !dir$ attributes align : 16:: ps0,ps1
        type(XMM2r8_t), automatic :: ph0,ph1
        !dir$ attributes align : 16:: ph0,ph1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              ps0   = psi(i)
              ph0   = phi(i)
              FH(i) = ratio_FH_xmm2r8(ps0,ph0)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(i,ps0,ph0,ps1,ph1)            &
        !$omp shared(n,psi,phi,FH)
        do i=m1,n,2
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm2r8(ps0,ph0)
            ps1     = psi(i+1)
            ph1     = phi(i+1)
            FH(i+1) = ratio_FH_xmm2r8(ps1,ph1)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_unroll_2x_omp_xmm2r8



      subroutine ratio_FH_rolled_xmm2r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_rolled_xmm2r8
        !dir$ attributes forceinline :: ratio_FH_rolled_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_rolled_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: ps0
        !dir$ attributes align : 16:: ps0
        type(XMM2r8_t), automatic :: ph0
        !dir$ attributes align : 16:: ph0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=1,n
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm2r8(ps0,ph0)
        end do
     end subroutine ratio_FH_rolled_xmm2r8


     subroutine ratio_FH_rolled_omp_xmm2r8(psi,phi,FH,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_rolled_omp_xmm2r8
        !dir$ attributes forceinline :: ratio_FH_rolled_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: ratio_FH_rolled_omp_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: ps0
        !dir$ attributes align : 16:: ps0
        type(XMM2r8_t), automatic :: ph0
        !dir$ attributes align : 16:: ph0
        integer(kind=i4) :: i
      
        !dir$ assume_aligned phi:16
        !dir$ assume_aligned psi:16
        !dir$ assume_aligned FH:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(i,ps0,ph0)                    &
        !$omp shared(n,psi,phi,FH)
        do i=1,n
            ps0     = psi(i)
            ph0     = phi(i)
            FH(i)   = ratio_FH_xmm2r8(ps0,ph0)
        end do
        !$omp end parallel do
     end subroutine ratio_FH_rolled_omp_xmm2r8



     subroutine ratio_FH_dispatch_xmm2r8(psi,phi,FH,n,unroll_cnt,omp_ver)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: ratio_FH_dispatch_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in) :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out):: FH
        integer(kind=i4),                  intent(in) :: n
        integer(kind=i4),                  intent(in) :: unroll_cnt
        logical(kind=i4),                  intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
               case (16)
                  call ratio_FH_unroll_16x_omp_xmm2r8(psi,phi,FH,n)
               case (8)
                  call ratio_FH_unroll_8x_omp_xmm2r8(psi,phi,FH,n) 
               case (4)
                  call ratio_FH_unroll_4x_omp_xmm2r8(psi,phi,FH,n)
               case (2)
                  call ratio_FH_unroll_2x_omp_xmm2r8(psi,phi,FH,n)
               case (0)
                  call ratio_FH_rolled_omp_xmm2r8(psi,phi,FH,n)
               case default
                  return
            end select
         else
            select case (unroll_cnt)
               case (16)
                  call ratio_FH_unroll_16x_xmm2r8(psi,phi,FH,n)
               case (8)
                  call ratio_FH_unroll_8x_xmm2r8(psi,phi,FH,n) 
               case (4)
                  call ratio_FH_unroll_4x_xmm2r8(psi,phi,FH,n)
               case (2)
                  call ratio_FH_unroll_2x_xmm2r8(psi,phi,FH,n)
               case (0)
                  call ratio_FH_rolled_xmm2r8(psi,phi,FH,n)
               case default
                  return
            end select
         end if
     end subroutine ratio_FH_dispatch_xmm2r8
     
     
!///////////////////////////////////////////////////////////////////////////////////!
!///////////////////////////////////////////////////////////////////////////////////!


     ! следовательно, угол установки сканирующего зеркала
     ! Formula 4, p. 56
     
     
     pure function scan_mirror_ang_xmm4r4(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: scan_mirror_ang_xmm4r4
        !dir$ attributes forceinline :: scan_mirror_ang_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_xmm4r4
        type(XMM4r4_t),  intent(in) :: gam0
        type(XMM4r4_t),  intent(in) :: psi
        type(XMM4r4_t),  intent(in) :: phi
        character(len=3), intent(in) :: dir
        type(XMM4r4_t) :: gamma
        type(XMM4r4_t), parameter :: half = XMM4r4_t(0.5_sp)
        type(XMM4r4_t), automatic :: t0,t1
        if(dir=="pos") then
            t0 = gam0.v+half.v*phi.v*half.v
            t1 = ratio_FH_xmm4r4(psi,phi)
            gamma = t0.v*t1.v
        else if(dir=="neg") then
            t0 = gam0.v-half.v*phi.v*half.v
            t1 = ratio_FH_xmm4r4(psi,phi)
            gamma = t0.v*t1.v
        end if
     end function scan_mirror_ang_xmm4r4


     subroutine scan_mirror_ang_unroll_16x_xmm4r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: scan_mirror_ang_unroll_16x_xmm4r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_16x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_16x_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM4r4_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM4r4_t)   , automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(XMM4r4_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(XMM4r4_t)   , automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        type(XMM4r4_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        type(XMM4r4_t)   , automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir)  
           end do
           if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,16
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm4r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_xmm4r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_xmm4r4(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_xmm4r4(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_xmm4r4(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_xmm4r4(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_xmm4r4(g7,ps7,ph7,dir)  
            g8         = gam0(i+8)
            ps8        = psi(i+8)
            ph8        = phi(i+8)
            gamma(i+8) = scan_mirror_ang_xmm4r4(g8,ps8,ph8,dir)    
            g9         = gam0(i+9)
            ps9        = psi(i+9)
            ph9        = phi(i+9)
            gamma(i+9) = scan_mirror_ang_xmm4r4(g9,ps9,ph9,dir)  
            g10        = gam0(i+1)
            ps10       = psi(i+1)
            ph10       = phi(i+1)
            gamma(i+10)= scan_mirror_ang_xmm4r4(g10,ps10,ph10,dir) 
            g11        = gam0(i+11)
            ps11       = psi(i+11)
            ph11       = phi(i+11)
            gamma(i+11)= scan_mirror_ang_xmm4r4(g11,ps11,ph12,dir) 
            g12        = gam0(i+12)
            ps12       = psi(i+12)
            ph12       = phi(i+12)
            gamma(i+12)= scan_mirror_ang_xmm4r4(g12,ps12,ph12,dir)  
            g13        = gam0(i+13)
            ps13       = psi(i+13)
            ph13       = phi(i+13)
            gamma(i+13)= scan_mirror_ang_xmm4r4(g13,ps13,ph13,dir)
            g14        = gam0(i+14)
            ps14       = psi(i+14)
            ph14       = phi(i+14)
            gamma(i+14)= scan_mirror_ang_xmm4r4(g14,ps14,ph14,dir) 
            g15        = gam0(i+15)
            ps15       = psi(i+15)
            ph155      = phi(i+15)
            gamma(i+15)= scan_mirror_ang_xmm4r4(g15,ps15,ph15,dir)                  
         end do
     end subroutine scan_mirror_ang_unroll_16x_xmm4r4


     
     subroutine scan_mirror_ang_unroll_16x_omp_xmm4r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: scan_mirror_ang_unroll_16x_omp_xmm4r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_16x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_16x_omp_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM4r4_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM4r4_t)   , automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(XMM4r4_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(XMM4r4_t)   , automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        type(XMM4r4_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        type(XMM4r4_t)   , automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir)  
           end do
           if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7) &
         !$omp private(ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7)                  &
         !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                          &
         !$omp private(ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15)            &
         !$omp private(ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15)            &
         !$omp private(g8,g9,g10,g11,g12,g13,g14,g15)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,16
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm4r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_xmm4r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_xmm4r4(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_xmm4r4(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_xmm4r4(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_xmm4r4(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_xmm4r4(g7,ps7,ph7,dir)  
            g8         = gam0(i+8)
            ps8        = psi(i+8)
            ph8        = phi(i+8)
            gamma(i+8) = scan_mirror_ang_xmm4r4(g8,ps8,ph8,dir)    
            g9         = gam0(i+9)
            ps9        = psi(i+9)
            ph9        = phi(i+9)
            gamma(i+9) = scan_mirror_ang_xmm4r4(g9,ps9,ph9,dir)  
            g10        = gam0(i+1)
            ps10       = psi(i+1)
            ph10       = phi(i+1)
            gamma(i+10)= scan_mirror_ang_xmm4r4(g10,ps10,ph10,dir) 
            g11        = gam0(i+11)
            ps11       = psi(i+11)
            ph11       = phi(i+11)
            gamma(i+11)= scan_mirror_ang_xmm4r4(g11,ps11,ph12,dir) 
            g12        = gam0(i+12)
            ps12       = psi(i+12)
            ph12       = phi(i+12)
            gamma(i+12)= scan_mirror_ang_xmm4r4(g12,ps12,ph12,dir)  
            g13        = gam0(i+13)
            ps13       = psi(i+13)
            ph13       = phi(i+13)
            gamma(i+13)= scan_mirror_ang_xmm4r4(g13,ps13,ph13,dir)
            g14        = gam0(i+14)
            ps14       = psi(i+14)
            ph14       = phi(i+14)
            gamma(i+14)= scan_mirror_ang_xmm4r4(g14,ps14,ph14,dir) 
            g15        = gam0(i+15)
            ps15       = psi(i+15)
            ph155      = phi(i+15)
            gamma(i+15)= scan_mirror_ang_xmm4r4(g15,ps15,ph15,dir)                  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_16x_xmm4r4


     subroutine scan_mirror_ang_unroll_8x_xmm4r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: scan_mirror_ang_unroll_8x_xmm4r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_8x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_8x_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM4r4_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM4r4_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(XMM4r4_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir)  
           end do
           if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,8
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm4r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_xmm4r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_xmm4r4(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_xmm4r4(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_xmm4r4(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_xmm4r4(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_xmm4r4(g7,ps7,ph7,dir)  
        end do
     end subroutine scan_mirror_ang_unroll_8x_xmm4r4


     
     subroutine scan_mirror_ang_unroll_8x_omp_xmm4r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: scan_mirror_ang_unroll_8x_omp_xmm4r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_8x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_8x_omp_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM4r4_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM4r4_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(XMM4r4_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir)  
           end do
           if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7) &
         !$omp private(ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7)                  &
         !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                          &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,8
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm4r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_xmm4r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_xmm4r4(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_xmm4r4(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_xmm4r4(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_xmm4r4(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_xmm4r4(g7,ps7,ph7,dir)  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_8x_xmm4r4


     subroutine scan_mirror_ang_unroll_4x_xmm4r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: scan_mirror_ang_unroll_4x_xmm4r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_4x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_4x_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM4r4_t)   , automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3
        type(XMM4r4_t)   , automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3
        type(XMM4r4_t)   , automatic :: g0,g1,g2,g3
        !dir$ attributes align : 16:: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir)  
           end do
           if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,4
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm4r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_xmm4r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_xmm4r4(g3,ps3,ph3,dir)  
        end do
     end subroutine scan_mirror_ang_unroll_4x_xmm4r4


     
     subroutine scan_mirror_ang_unroll_4x_omp_xmm4r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: scan_mirror_ang_unroll_4x_omp_xmm4r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_4x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_4x_omp_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM4r4_t)   , automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3
        type(XMM4r4_t)   , automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3
        type(XMM4r4_t)   , automatic :: g0,g1,g2,g3
        !dir$ attributes align : 16:: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir)  
           end do
           if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3) &
         !$omp private(ph0,ph1,ph2,ph3)                  &
         !$omp private(g0,g1,g2,g3)                      &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,4
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm4r4(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_xmm4r4(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_xmm4r4(g3,ps3,ph3,dir)  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_4x_xmm4r4


    subroutine scan_mirror_ang_unroll_2x_xmm4r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: scan_mirror_ang_unroll_2x_xmm4r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_2x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_2x_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM4r4_t)   , automatic :: ps0,ps1
        !dir$ attributes align : 16:: ps0,ps1
        type(XMM4r4_t)   , automatic :: ph0,ph1
        !dir$ attributes align : 16:: ph0,ph1
        type(XMM4r4_t)   , automatic :: g0,g1
        !dir$ attributes align : 16:: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir)  
           end do
           if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,2
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm4r4(g1,ps1,ph1,dir)   
         end do
     end subroutine scan_mirror_ang_unroll_2x_xmm4r4


     
     subroutine scan_mirror_ang_unroll_2x_omp_xmm4r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: scan_mirror_ang_unroll_2x_omp_xmm4r4
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_2x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_2x_omp_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM4r4_t)   , automatic :: ps0,ps1
        !dir$ attributes align : 16:: ps0,ps1
        type(XMM4r4_t)   , automatic :: ph0,ph1
        !dir$ attributes align : 16:: ph0,ph1
        type(XMM4r4_t)   , automatic :: g0,g1
        !dir$ attributes align : 16:: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir)  
           end do
           if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(ps0,ps1) &
         !$omp private(ph0,ph1)                  &
         !$omp private(g0,g1)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,2
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm4r4(g1,ps1,ph1,dir)   
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_2x_xmm4r4



     subroutine scan_mirror_ang_rolled_xmm4r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: scan_mirror_ang_rolled_xmm4r4
        !dir$ attributes forceinline :: scan_mirror_ang_rolled_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_rolled_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM4r4_t)   , automatic :: ps0
        !dir$ attributes align : 16:: ps0
        type(XMM4r4_t)   , automatic :: ph0
        !dir$ attributes align : 16:: ph0
        type(XMM4r4_t)   , automatic :: g0
        !dir$ attributes align : 16:: g0
        integer(kind=i4) :: i
      
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=1,n
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir) 
         end do
     end subroutine scan_mirror_ang_rolled_xmm4r4


     
     subroutine scan_mirror_ang_rolled_omp_xmm4r4(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 16:: scan_mirror_ang_rolled_omp_xmm4r4
        !dir$ attributes forceinline :: scan_mirror_ang_rolled_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_rolled_omp_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM4r4_t)   , automatic :: ps0
        !dir$ attributes align : 16:: ps0
        type(XMM4r4_t)   , automatic :: ph0
        !dir$ attributes align : 16:: ph0
        type(XMM4r4_t)   , automatic :: g0
        !dir$ attributes align : 16:: g0
        integer(kind=i4) :: i
      
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp  private(i,ps0) &
         !$omp private(ph0)                  &
         !$omp private(g0)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=1,n
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm4r4(g0,ps0,ph0,dir) 
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_rolled_xmm4r4



     subroutine scan_mirror_ang_dispatch_xmm4r4(gam0,psi,phi,dir,gamma,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 16:: scan_mirror_ang_dispatch_xmm4r4
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM4r4_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM4r4_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        integer(kind=i4),                  intent(in)  :: unroll_cnt
        logical(kind=i4),                  intent(in)  :: omp_ver
        if(omp_ver) then
            select case (unroll_cnt)
               case (16)
                  call scan_mirror_ang_unroll_16x_omp_xmm4r4(gam0,psi,phi,dir,gamma,n)
               case (8)
                  call scan_mirror_ang_unroll_8x_omp_xmm4r4(gam0,psi,phi,dir,gamma,n)
               case (4)
                  call scan_mirror_ang_unroll_4x_omp_xmm4r4(gam0,psi,phi,dir,gamma,n)
               case (2)
                  call scan_mirror_ang_unroll_2x_omp_xmm4r4(gam0,psi,phi,dir,gamma,n)
               case (0)
                  call scan_mirror_ang_rolled_omp_xmm4r4(gam0,psi,phi,dir,gamma,n)
               case default
                  return
             end select
        else
             select case (unroll_cnt)
               case (16)
                  call scan_mirror_ang_unroll_16x_xmm4r4(gam0,psi,phi,dir,gamma,n)
               case (8)
                  call scan_mirror_ang_unroll_8x_xmm4r4(gam0,psi,phi,dir,gamma,n)
               case (4)
                  call scan_mirror_ang_unroll_4x_xmm4r4(gam0,psi,phi,dir,gamma,n)
               case (2)
                  call scan_mirror_ang_unroll_2x_xmm4r4(gam0,psi,phi,dir,gamma,n)
               case (0)
                  call scan_mirror_ang_rolled_xmm4r4(gam0,psi,phi,dir,gamma,n)
               case default
                  return
             end select
        end if
    end subroutine scan_mirror_ang_dispatch_xmm4r4
    
    
!///////////////////////////////////////////////////////////////////////////////////////////////


    pure function scan_mirror_ang_xmm2r8(gam0,psi,phi,dir) result(gamma)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_xmm2r8
        !dir$ attributes forceinline :: scan_mirror_ang_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_xmm2r8
        type(XMM2r8_t),  intent(in) :: gam0
        type(XMM2r8_t),  intent(in) :: psi
        type(XMM2r8_t),  intent(in) :: phi
        character(len=3), intent(in) :: dir
        type(XMM2r8_t) :: gamma
        type(XMM2r8_t), parameter :: half = XMM2r8_t(0.5_dp)
        type(XMM2r8_t), automatic :: t0,t1
        if(dir=="pos") then
            t0 = gam0.v+half.v*phi.v*half.v
            t1 = ratio_FH_xmm2r8(psi,phi)
            gamma = t0.v*t1.v
        else if(dir=="neg") then
            t0 = gam0.v-half.v*phi.v*half.v
            t1 = ratio_FH_xmm2r8(psi,phi)
            gamma = t0.v*t1.v
        end if
     end function scan_mirror_ang_xmm2r8


     subroutine scan_mirror_ang_unroll_16x_xmm2r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_16x_xmm2r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_16x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_16x_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM2r8_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM2r8_t)   , automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(XMM2r8_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(XMM2r8_t)   , automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        type(XMM2r8_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        type(XMM2r8_t)   , automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir)  
           end do
           if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,16
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm2r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_xmm2r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_xmm2r8(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_xmm2r8(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_xmm2r8(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_xmm2r8(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_xmm2r8(g7,ps7,ph7,dir)  
            g8         = gam0(i+8)
            ps8        = psi(i+8)
            ph8        = phi(i+8)
            gamma(i+8) = scan_mirror_ang_xmm2r8(g8,ps8,ph8,dir)    
            g9         = gam0(i+9)
            ps9        = psi(i+9)
            ph9        = phi(i+9)
            gamma(i+9) = scan_mirror_ang_xmm2r8(g9,ps9,ph9,dir)  
            g10        = gam0(i+1)
            ps10       = psi(i+1)
            ph10       = phi(i+1)
            gamma(i+10)= scan_mirror_ang_xmm2r8(g10,ps10,ph10,dir) 
            g11        = gam0(i+11)
            ps11       = psi(i+11)
            ph11       = phi(i+11)
            gamma(i+11)= scan_mirror_ang_xmm2r8(g11,ps11,ph12,dir) 
            g12        = gam0(i+12)
            ps12       = psi(i+12)
            ph12       = phi(i+12)
            gamma(i+12)= scan_mirror_ang_xmm2r8(g12,ps12,ph12,dir)  
            g13        = gam0(i+13)
            ps13       = psi(i+13)
            ph13       = phi(i+13)
            gamma(i+13)= scan_mirror_ang_xmm2r8(g13,ps13,ph13,dir)
            g14        = gam0(i+14)
            ps14       = psi(i+14)
            ph14       = phi(i+14)
            gamma(i+14)= scan_mirror_ang_xmm2r8(g14,ps14,ph14,dir) 
            g15        = gam0(i+15)
            ps15       = psi(i+15)
            ph155      = phi(i+15)
            gamma(i+15)= scan_mirror_ang_xmm2r8(g15,ps15,ph15,dir)                  
         end do
     end subroutine scan_mirror_ang_unroll_16x_xmm2r8
     
     
     subroutine scan_mirror_ang_unroll_16x_omp_xmm2r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_16x_omp_xmm2r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_16x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_16x_omp_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM2r8_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM2r8_t)   , automatic :: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15
        type(XMM2r8_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(XMM2r8_t)   , automatic :: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15
        type(XMM2r8_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        type(XMM2r8_t)   , automatic :: g8,g9,g10,g11,g12,g13,g14,g15
        !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir)  
           end do
           if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7) &
         !$omp private(ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7)                  &
         !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                          &
         !$omp private(ps8,ps9,ps10,ps11,ps12,ps13,ps14,ps15)            &
         !$omp private(ph8,ph9,ph10,ph11,ph12,ph13,ph14,ph15)            &
         !$omp private(g8,g9,g10,g11,g12,g13,g14,g15)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,16
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm2r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_xmm2r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_xmm2r8(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_xmm2r8(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_xmm2r8(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_xmm2r8(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_xmm2r8(g7,ps7,ph7,dir)  
            g8         = gam0(i+8)
            ps8        = psi(i+8)
            ph8        = phi(i+8)
            gamma(i+8) = scan_mirror_ang_xmm2r8(g8,ps8,ph8,dir)    
            g9         = gam0(i+9)
            ps9        = psi(i+9)
            ph9        = phi(i+9)
            gamma(i+9) = scan_mirror_ang_xmm2r8(g9,ps9,ph9,dir)  
            g10        = gam0(i+1)
            ps10       = psi(i+1)
            ph10       = phi(i+1)
            gamma(i+10)= scan_mirror_ang_xmm2r8(g10,ps10,ph10,dir) 
            g11        = gam0(i+11)
            ps11       = psi(i+11)
            ph11       = phi(i+11)
            gamma(i+11)= scan_mirror_ang_xmm2r8(g11,ps11,ph12,dir) 
            g12        = gam0(i+12)
            ps12       = psi(i+12)
            ph12       = phi(i+12)
            gamma(i+12)= scan_mirror_ang_xmm2r8(g12,ps12,ph12,dir)  
            g13        = gam0(i+13)
            ps13       = psi(i+13)
            ph13       = phi(i+13)
            gamma(i+13)= scan_mirror_ang_xmm2r8(g13,ps13,ph13,dir)
            g14        = gam0(i+14)
            ps14       = psi(i+14)
            ph14       = phi(i+14)
            gamma(i+14)= scan_mirror_ang_xmm2r8(g14,ps14,ph14,dir) 
            g15        = gam0(i+15)
            ps15       = psi(i+15)
            ph155      = phi(i+15)
            gamma(i+15)= scan_mirror_ang_xmm2r8(g15,ps15,ph15,dir)                  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_16x_xmm2r8


     subroutine scan_mirror_ang_unroll_8x_xmm2r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_8x_xmm2r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_8x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_8x_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM2r8_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM2r8_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(XMM2r8_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir)  
           end do
           if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,8
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm2r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_xmm2r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_xmm2r8(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_xmm2r8(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_xmm2r8(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_xmm2r8(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_xmm2r8(g7,ps7,ph7,dir)  
        end do
     end subroutine scan_mirror_ang_unroll_8x_xmm2r8


     
     subroutine scan_mirror_ang_unroll_8x_omp_xmm2r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_8x_omp_xmm2r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_8x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_8x_omp_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM2r8_t)   , automatic :: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7
        type(XMM2r8_t)   , automatic :: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7
        type(XMM2r8_t)   , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
        !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir)  
           end do
           if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3,ps4,ps5,ps6,ps7) &
         !$omp private(ph0,ph1,ph2,ph3,ph4,ph5,ph6,ph7)                  &
         !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                          &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,8
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm2r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_xmm2r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_xmm2r8(g3,ps3,ph3,dir)  
            g4         = gam0(i+4)
            ps4        = psi(i+4)
            ph4        = phi(i+4)
            gamma(i+4) = scan_mirror_ang_xmm2r8(g4,ps4,ph4,dir) 
            g5         = gam0(i+5)
            ps5        = psi(i+5)
            ph5        = phi(i+5)
            gamma(i+5) = scan_mirror_ang_xmm2r8(g5,ps5,ph5,dir)    
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+6) = scan_mirror_ang_xmm2r8(g6,ps6,ph6,dir)  
            g7         = gam0(i+7)
            ps7        = psi(i+7)
            ph7        = phi(i+7)
            gamma(i+7) = scan_mirror_ang_xmm2r8(g7,ps7,ph7,dir)  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_8x_xmm2r8


     subroutine scan_mirror_ang_unroll_4x_xmm2r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_4x_xmm2r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_4x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_4x_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM2r8_t)   , automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3
        type(XMM2r8_t)   , automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3
        type(XMM2r8_t)   , automatic :: g0,g1,g2,g3
        !dir$ attributes align : 16:: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir)  
           end do
           if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,4
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm2r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_xmm2r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_xmm2r8(g3,ps3,ph3,dir)  
        end do
     end subroutine scan_mirror_ang_unroll_4x_xmm2r8


     
     subroutine scan_mirror_ang_unroll_4x_omp_xmm2r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_4x_omp_xmm2r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_4x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_4x_omp_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM2r8_t)   , automatic :: ps0,ps1,ps2,ps3
        !dir$ attributes align : 16:: ps0,ps1,ps2,ps3
        type(XMM2r8_t)   , automatic :: ph0,ph1,ph2,ph3
        !dir$ attributes align : 16:: ph0,ph1,ph2,ph3
        type(XMM2r8_t)   , automatic :: g0,g1,g2,g3
        !dir$ attributes align : 16:: g0,g1,g2,g3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir)  
           end do
           if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(ps0,ps1,ps2,ps3) &
         !$omp private(ph0,ph1,ph2,ph3)                  &
         !$omp private(g0,g1,g2,g3)                      &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,4
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm2r8(g1,ps1,ph1,dir)   
            g2         = gam0(i+2)
            ps2        = psi(i+2)
            ph2        = phi(i+2)
            gamma(i+2) = scan_mirror_ang_xmm2r8(g2,ps2,ph2,dir)  
            g3         = gam0(i+3)
            ps3        = psi(i+3)
            ph3        = phi(i+3)
            gamma(i+3) = scan_mirror_ang_xmm2r8(g3,ps3,ph3,dir)  
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_4x_xmm2r8


    subroutine scan_mirror_ang_unroll_2x_xmm2r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_2x_xmm2r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_2x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_2x_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM2r8_t)   , automatic :: ps0,ps1
        !dir$ attributes align : 16:: ps0,ps1
        type(XMM2r8_t)   , automatic :: ph0,ph1
        !dir$ attributes align : 16:: ph0,ph1
        type(XMM2r8_t)   , automatic :: g0,g1
        !dir$ attributes align : 16:: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir)  
           end do
           if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,2
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm2r8(g1,ps1,ph1,dir)   
         end do
     end subroutine scan_mirror_ang_unroll_2x_xmm2r8


     
     subroutine scan_mirror_ang_unroll_2x_omp_xmm2r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_unroll_2x_omp_xmm2r8
        !dir$ attributes forceinline :: scan_mirror_ang_unroll_2x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_unroll_2x_omp_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM2r8_t)   , automatic :: ps0,ps1
        !dir$ attributes align : 16:: ps0,ps1
        type(XMM2r8_t)   , automatic :: ph0,ph1
        !dir$ attributes align : 16:: ph0,ph1
        type(XMM2r8_t)   , automatic :: g0,g1
        !dir$ attributes align : 16:: g0,g1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              g0       = gam0(i)
              ps0      = psi(i)
              ph0      = phi(i)
              gamma(i) = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir)  
           end do
           if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(ps0,ps1) &
         !$omp private(ph0,ph1)                  &
         !$omp private(g0,g1)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=m1,n,2
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir) 
            g1         = gam0(i+1)
            ps1        = psi(i+1)
            ph1        = phi(i+1)
            gamma(i+1) = scan_mirror_ang_xmm2r8(g1,ps1,ph1,dir)   
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_unroll_2x_xmm2r8



     subroutine scan_mirror_ang_rolled_xmm2r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_rolled_xmm2r8
        !dir$ attributes forceinline :: scan_mirror_ang_rolled_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_rolled_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(YMM8r8_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: phi
        type(YMMr8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM2r8_t)   , automatic :: ps0
        !dir$ attributes align : 16:: ps0
        type(XMM2r8_t)   , automatic :: ph0
        !dir$ attributes align : 16:: ph0
        type(XMM2r8_t)   , automatic :: g0
        !dir$ attributes align : 16:: g0
        integer(kind=i4) :: i
      
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=1,n
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir) 
         end do
     end subroutine scan_mirror_ang_rolled_xmm2r8


     
     subroutine scan_mirror_ang_rolled_omp_xmm2r8(gam0,psi,phi,dir,gamma,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_mirror_ang_rolled_omp_xmm2r8
        !dir$ attributes forceinline :: scan_mirror_ang_rolled_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_mirror_ang_rolled_omp_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        type(XMM2r8_t)   , automatic :: ps0
        !dir$ attributes align : 16:: ps0
        type(XMM2r8_t)   , automatic :: ph0
        !dir$ attributes align : 16:: ph0
        type(XMM2r8_t)   , automatic :: g0
        !dir$ attributes align : 16:: g0
        integer(kind=i4) :: i
      
           !dir$ assume_aligned gam0:16
           !dir$ assume_aligned phi:16
           !dir$ assume_aligned psi:16
           !dir$ assume_aligned gamma:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp  private(i,ps0) &
         !$omp private(ph0)                  &
         !$omp private(g0)                    &
         !$omp shared(n,gamma,gam0,psi,phi)
         do i=1,n
            g0         = gam0(i)
            ps0        = psi(i)
            ph0        = phi(i)
            gamma(i)   = scan_mirror_ang_xmm2r8(g0,ps0,ph0,dir) 
         end do
         !$omp end parallel do
     end subroutine scan_mirror_ang_rolled_xmm2r8



     subroutine scan_mirror_ang_dispatch_xmm2r8(gam0,psi,phi,dir,gamma,n,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: scan_mirror_ang_dispatch_xmm2r8
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: gam0
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: psi
        type(XMM2r8_t),  dimension(1:n),  intent(in)  :: phi
        type(XMM2r8_t),  dimension(1:n),  intent(out) :: gamma
        integer(kind=i4),                  intent(in)  :: n
        character(len=3),                  intent(in)  :: dir
        integer(kind=i4),                  intent(in)  :: unroll_cnt
        logical(kind=i4),                  intent(in)  :: omp_ver
        if(omp_ver) then
            select case (unroll_cnt)
               case (16)
                  call scan_mirror_ang_unroll_16x_omp_xmm2r8(gam0,psi,phi,dir,gamma,n)
               case (8)
                  call scan_mirror_ang_unroll_8x_omp_xmm2r8(gam0,psi,phi,dir,gamma,n)
               case (4)
                  call scan_mirror_ang_unroll_4x_omp_xmm2r8(gam0,psi,phi,dir,gamma,n)
               case (2)
                  call scan_mirror_ang_unroll_2x_omp_xmm2r8(gam0,psi,phi,dir,gamma,n)
               case (0)
                  call scan_mirror_ang_rolled_omp_xmm2r8(gam0,psi,phi,dir,gamma,n)
               case default
                  return
             end select
        else
             select case (unroll_cnt)
               case (16)
                  call scan_mirror_ang_unroll_16x_xmm2r8(gam0,psi,phi,dir,gamma,n)
               case (8)
                  call scan_mirror_ang_unroll_8x_xmm2r8(gam0,psi,phi,dir,gamma,n)
               case (4)
                  call scan_mirror_ang_unroll_4x_xmm2r8(gam0,psi,phi,dir,gamma,n)
               case (2)
                  call scan_mirror_ang_unroll_2x_xmm2r8(gam0,psi,phi,dir,gamma,n)
               case (0)
                  call scan_mirror_ang_rolled_xmm2r8(gam0,psi,phi,dir,gamma,n)
               case default
                  return
             end select
        end if
    end subroutine scan_mirror_ang_dispatch_xmm2r8
    
    
    !////////////////////////////////////////////////////////////////////////////////////!
    !////////////////////////////////////////////////////////////////////////////////////!
    
    
     !величина расфокусировки
      !Formula 1, p. 59
    
   
    pure function defocus_cof_xmm4r4(l2,alpha,O,inf) result(dc)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_xmm4r4
        !dir$ attributes forceinline :: defocus_cof_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_xmm4r4
        type(XMM4r4_t),  intent(in) :: l2
        type(XMM4r4_t),  intent(in) :: alpha
        type(XMM4r4_t),  intent(in) :: O
        logical(kind=i4), intent(in) :: inf
        type(XMM4r4_t) :: dc
        type(XMM4r4_t), automatic :: cos2a,icos
        type(XMM4r4_t), parameter :: one = XMM4r4_t(1.0_sp)
        cos2a = cos(alpha.v+alpha.v)
        icos  = one.v/cos2a.v
        if(inf) then
           df    = l2.v/(icos.v-one.v)*O
        else
           df    = l2.v/(icos.v-one.v)
        end if
    end function defocus_cof_xmm4r4


    subroutine defocus_cof_unroll_16x_xmm4r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_16x_xmm4r4
        !dir$ attributes forceinline :: defocus_cof_unroll_16x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_16x_xmm4r4
        type(XMM4r4_t),                 intent(in) :: l2
        type(XMM4r4_t), dimension(1:n), intent(in) :: alpha
        type(XMM4r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM4r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        type(XMM4r4_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm4r4(l2,a0,O,inf)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,16
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm4r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm4r4(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_xmm4r4(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_xmm4r4(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_xmm4r4(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_xmm4r4(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_xmm4r4(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_xmm4r4(l2,a7,O,inf)
            a8      =  alpha(i+8)
            dc(i+8) =  defocus_cof_xmm4r4(l2,a8,O,inf)
            a9      =  alpha(i+9)
            dc(i+9) =  defocus_cof_xmm4r4(l2,a9,O,inf)
            a10     =  alpha(i+10)
            dc(i+10)=  defocus_cof_xmm4r4(l2,a10,O,inf)
            a11     =  alpha(i+11)
            dc(i+11)=  defocus_cof_xmm4r4(l2,a11,O,inf)
            a12     =  alpha(i+12)
            dc(i+12)=  defocus_cof_xmm4r4(l2,a12,O,inf)
            a13     =  alpha(i+13)
            dc(i+13)=  defocus_cof_xmm4r4(l2,a13,O,inf)
            a14      =  alpha(i+1)
            dc(i+14) =  defocus_cof_xmm4r4(l2,a14,O,inf) 
            a15      =  alpha(i+15)
            dc(i+15) =  defocus_cof_xmm4r4(l2,a15,O,inf) 
        end do
    end subroutine defocus_cof_unroll_16x_xmm4r4


    subroutine defocus_cof_unroll_16x_omp_xmm4r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_16x_omp_xmm4r4
        !dir$ attributes forceinline :: defocus_cof_unroll_16x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_16x_omp_xmm4r4
        type(XMM4r4_t),                 intent(in) :: l2
        type(XMM4r4_t), dimension(1:n), intent(in) :: alpha
        type(XMM4r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM4r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        type(XMM4r4_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm4r4(l2,a0,O,inf)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)
        !$omp private(a8,a9,a10,a11,a12,a13,a14,a15)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,16
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm4r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm4r4(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_xmm4r4(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_xmm4r4(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_xmm4r4(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_xmm4r4(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_xmm4r4(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_xmm4r4(l2,a7,O,inf)
            a8      =  alpha(i+8)
            dc(i+8) =  defocus_cof_xmm4r4(l2,a8,O,inf)
            a9      =  alpha(i+9)
            dc(i+9) =  defocus_cof_xmm4r4(l2,a9,O,inf)
            a10     =  alpha(i+10)
            dc(i+10)=  defocus_cof_xmm4r4(l2,a10,O,inf)
            a11     =  alpha(i+11)
            dc(i+11)=  defocus_cof_xmm4r4(l2,a11,O,inf)
            a12     =  alpha(i+12)
            dc(i+12)=  defocus_cof_xmm4r4(l2,a12,O,inf)
            a13     =  alpha(i+13)
            dc(i+13)=  defocus_cof_xmm4r4(l2,a13,O,inf)
            a14      =  alpha(i+1)
            dc(i+14) =  defocus_cof_xmm4r4(l2,a14,O,inf) 
            a15      =  alpha(i+15)
            dc(i+15) =  defocus_cof_xmm4r4(l2,a15,O,inf) 
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_16x_xmm4r4


    subroutine defocus_cof_unroll_8x_xmm4r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_8x_xmm4r4
        !dir$ attributes forceinline :: defocus_cof_unroll_8x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_8x_xmm4r4
        type(XMM4r4_t),                 intent(in) :: l2
        type(XMM4r4_t), dimension(1:n), intent(in) :: alpha
        type(XMM4r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM4r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm4r4(l2,a0,O,inf)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,8
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm4r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm4r4(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_xmm4r4(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_xmm4r4(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_xmm4r4(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_xmm4r4(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_xmm4r4(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_xmm4r4(l2,a7,O,inf)
         end do
    end subroutine defocus_cof_unroll_8x_xmm4r4


    subroutine defocus_cof_unroll_8x_omp_xmm4r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_8x_omp_xmm4r4
        !dir$ attributes forceinline :: defocus_cof_unroll_8x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_8x_omp_xmm4r4
        type(XMM4r4_t),                 intent(in) :: l2
        type(XMM4r4_t), dimension(1:n), intent(in) :: alpha
        type(XMM4r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM4r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm4r4(l2,a0,O,inf)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,8
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm4r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm4r4(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_xmm4r4(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_xmm4r4(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_xmm4r4(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_xmm4r4(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_xmm4r4(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_xmm4r4(l2,a7,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_8x_xmm4r4


     subroutine defocus_cof_unroll_4x_xmm4r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_4x_xmm4r4
        !dir$ attributes forceinline :: defocus_cof_unroll_4x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_4x_xmm4r4
        type(XMM4r4_t),                 intent(in) :: l2
        type(XMM4r4_t), dimension(1:n), intent(in) :: alpha
        type(XMM4r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM4r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1,a2,a3
        !dir$ attributes align : 16:: a0,a1,a2,a3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm4r4(l2,a0,O,inf)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,4
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm4r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm4r4(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_xmm4r4(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_xmm4r4(l2,a3,O,inf)
        end do
    end subroutine defocus_cof_unroll_4x_xmm4r4


    subroutine defocus_cof_unroll_4x_omp_xmm4r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_4x_omp_xmm4r4
        !dir$ attributes forceinline :: defocus_cof_unroll_4x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_4x_omp_xmm4r4
        type(XMM4r4_t),                 intent(in) :: l2
        type(XMM4r4_t), dimension(1:n), intent(in) :: alpha
        type(XMM4r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM4r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1,a2,a3
        !dir$ attributes align : 16:: a0,a1,a2,a3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm4r4(l2,a0,O,inf)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,4
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm4r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm4r4(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_xmm4r4(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_xmm4r4(l2,a3,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_4x_xmm4r4

    
    subroutine defocus_cof_unroll_2x_xmm4r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_2x_xmm4r4
        !dir$ attributes forceinline :: defocus_cof_unroll_2x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_2x_xmm4r4
        type(XMM4r4_t),                 intent(in) :: l2
        type(XMM4r4_t), dimension(1:n), intent(in) :: alpha
        type(XMM4r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM4r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1
        !dir$ attributes align : 16:: a0,a1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm4r4(l2,a0,O,inf)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=m1,n,2
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm4r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm4r4(l2,a1,O,inf)
        end do
    end subroutine defocus_cof_unroll_2x_xmm4r4


    


    subroutine defocus_cof_unroll_2x_omp_xmm4r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_2x_omp_xmm4r4
        !dir$ attributes forceinline :: defocus_cof_unroll_2x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_2x_omp_xmm4r4
        type(XMM4r4_t),                 intent(in) :: l2
        type(XMM4r4_t), dimension(1:n), intent(in) :: alpha
        type(XMM4r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM4r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1
        !dir$ attributes align : 16:: a0,a1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm4r4(l2,a0,O,inf)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,2
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm4r4(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm4r4(l2,a1,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_2x_xmm4r4


    subroutine defocus_cof_rolled_xmm4r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_rolled_xmm4r4
        !dir$ attributes forceinline :: defocus_cof_rolled_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_rolled_xmm4r4
        type(XMM4r4_t),                 intent(in) :: l2
        type(XMM4r4_t), dimension(1:n), intent(in) :: alpha
        type(XMM4r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM4r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM4r4_t), automatic :: a0
        !dir$ attributes align : 16:: a0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        do i=1,n
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm4r4(l2,a0,O,inf)
        end do
    end subroutine defocus_cof_rolled_xmm4r4


    subroutine defocus_cof_rolled_omp_xmm4r4(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_rolled_omp_xmm4r4
        !dir$ attributes forceinline :: defocus_cof_rolled_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_rolled_omp_xmm4r4
        type(XMM4r4_t),                 intent(in) :: l2
        type(XMM4r4_t), dimension(1:n), intent(in) :: alpha
        type(XMM4r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM4r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM4r4_t), automatic :: a0
        !dir$ attributes align : 16:: a0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0)
        !$omp shared(n,alpha,dc,O,inf)
        do i=1,n
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm4r4(l2,a0,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_rolled_xmm4r4


    subroutine defocus_cof_dispatch_xmm4r4(l2,alpha,O,inf,dc,n,unroll_cnt,omp_ver)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_dispatch_xmm4r4
        type(XMM4r4_t),                 intent(in) :: l2
        type(XMM4r4_t), dimension(1:n), intent(in) :: alpha
        type(XMM4r4_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM4r4_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        integer(kind=i4),                intent(in) :: unroll_cnt
        logical(kind=i4),                intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
               case (16)
                  call defocus_cof_unroll_16x_omp_xmm4r4(l2,alpha,O,inf,dc,n)
               case (8)
                  call defocus_cof_unroll_8x_omp_xmm4r4(l2,alpha,O,inf,dc,n)
               case (4)
                  call defocus_cof_unroll_4x_omp_xmm4r4(l2,alpha,O,inf,dc,n)
               case (2)
                  call defocus_cof_unroll_2x_omp_xmm4r4(l2,alpha,O,inf,dc,n)
               case (0)
                  call defocus_cof_rolled_omp_xmm4r4(l2,alpha,O,inf,dc,n)
               case default
                  return
           end select
        else
           select case (unroll_cnt)
               case (16)
                  call defocus_cof_unroll_16x_xmm4r4(l2,alpha,O,inf,dc,n)
               case (8)
                  call defocus_cof_unroll_8x_xmm4r4(l2,alpha,O,inf,dc,n)
               case (4)
                  call defocus_cof_unroll_4x_xmm4r4(l2,alpha,O,inf,dc,n)
               case (2)
                  call defocus_cof_unroll_2x_xmm4r4(l2,alpha,O,inf,dc,n)
               case (0)
                  call defocus_cof_rolled_xmm4r4(l2,alpha,O,inf,dc,n)
               case default
                  return
           end select 
        end if
    end subroutine defocus_cof_dispatch_xmm4r4
    
    
    !/////////////////////////////////////////////////////////////////////////!
    !/////////////////////////////////////////////////////////////////////////!
    
    pure function defocus_cof_ym4r8(l2,alpha,O,inf) result(dc)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_xmm2r8
        !dir$ attributes forceinline :: defocus_cof_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_xmm2r8
        type(XMM2r8_t),   intent(in) :: l2
        type(XMM2r8_t),   intent(in) :: alpha
        type(XMM2r8_t),   intent(in) :: O
        logical(kind=i4), intent(in) :: inf
        type(XMM2r8_t) :: dc
        type(XMM2r8_t), automatic :: cos2a,icos
        type(XMM2r8_t), parameter :: one = XMM2r8_t(1.0_dp)
        cos2a = cos(alpha.v+alpha.v)
        icos  = one.v/cos2a.v
        if(inf) then
           df    = l2.v/(icos.v-one.v)*O
        else
           df    = l2.v/(icos.v-one.v)
        end if
    end function defocus_cof_xmm2r8


    subroutine defocus_cof_unroll_16x_xmm2r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_16x_xmm2r8
        !dir$ attributes forceinline :: defocus_cof_unroll_16x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_16x_xmm2r8
        type(XMM2r8_t),                 intent(in) :: l2
        type(XMM2r8_t), dimension(1:n), intent(in) :: alpha
        type(XMM2r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM2r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        type(XMM2r8_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm2r8(l2,a0,O,inf)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,16
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm2r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm2r8(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_xmm2r8(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_xmm2r8(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_xmm2r8(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_xmm2r8(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_xmm2r8(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_xmm2r8(l2,a7,O,inf)
            a8      =  alpha(i+8)
            dc(i+8) =  defocus_cof_xmm2r8(l2,a8,O,inf)
            a9      =  alpha(i+9)
            dc(i+9) =  defocus_cof_xmm2r8(l2,a9,O,inf)
            a10     =  alpha(i+10)
            dc(i+10)=  defocus_cof_xmm2r8(l2,a10,O,inf)
            a11     =  alpha(i+11)
            dc(i+11)=  defocus_cof_xmm2r8(l2,a11,O,inf)
            a12     =  alpha(i+12)
            dc(i+12)=  defocus_cof_xmm2r8(l2,a12,O,inf)
            a13     =  alpha(i+13)
            dc(i+13)=  defocus_cof_xmm2r8(l2,a13,O,inf)
            a14      =  alpha(i+1)
            dc(i+14) =  defocus_cof_xmm2r8(l2,a14,O,inf) 
            a15      =  alpha(i+15)
            dc(i+15) =  defocus_cof_xmm2r8(l2,a15,O,inf) 
        end do
    end subroutine defocus_cof_unroll_16x_xmm2r8


    subroutine defocus_cof_unroll_16x_omp_xmm2r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_16x_omp_xmm2r8
        !dir$ attributes forceinline :: defocus_cof_unroll_16x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_16x_omp_xmm2r8
        type(XMM2r8_t),                 intent(in) :: l2
        type(XMM2r8_t), dimension(1:n), intent(in) :: alpha
        type(XMM2r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM2r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        type(XMM2r8_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm2r8(l2,a0,O,inf)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)
        !$omp private(a8,a9,a10,a11,a12,a13,a14,a15)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,16
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm2r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm2r8(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_xmm2r8(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_xmm2r8(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_xmm2r8(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_xmm2r8(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_xmm2r8(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_xmm2r8(l2,a7,O,inf)
            a8      =  alpha(i+8)
            dc(i+8) =  defocus_cof_xmm2r8(l2,a8,O,inf)
            a9      =  alpha(i+9)
            dc(i+9) =  defocus_cof_xmm2r8(l2,a9,O,inf)
            a10     =  alpha(i+10)
            dc(i+10)=  defocus_cof_xmm2r8(l2,a10,O,inf)
            a11     =  alpha(i+11)
            dc(i+11)=  defocus_cof_xmm2r8(l2,a11,O,inf)
            a12     =  alpha(i+12)
            dc(i+12)=  defocus_cof_xmm2r8(l2,a12,O,inf)
            a13     =  alpha(i+13)
            dc(i+13)=  defocus_cof_xmm2r8(l2,a13,O,inf)
            a14      =  alpha(i+1)
            dc(i+14) =  defocus_cof_xmm2r8(l2,a14,O,inf) 
            a15      =  alpha(i+15)
            dc(i+15) =  defocus_cof_xmm2r8(l2,a15,O,inf) 
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_16x_xmm2r8


    subroutine defocus_cof_unroll_8x_xmm2r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_8x_xmm2r8
        !dir$ attributes forceinline :: defocus_cof_unroll_8x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_8x_xmm2r8
        type(XMM2r8_t),                 intent(in) :: l2
        type(XMM2r8_t), dimension(1:n), intent(in) :: alpha
        type(XMM2r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM2r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm2r8(l2,a0,O,inf)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,8
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm2r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm2r8(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_xmm2r8(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_xmm2r8(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_xmm2r8(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_xmm2r8(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_xmm2r8(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_xmm2r8(l2,a7,O,inf)
         end do
    end subroutine defocus_cof_unroll_8x_xmm2r8


    subroutine defocus_cof_unroll_8x_omp_xmm2r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_8x_omp_xmm2r8
        !dir$ attributes forceinline :: defocus_cof_unroll_8x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_8x_omp_xmm2r8
        type(XMM2r8_t),                 intent(in) :: l2
        type(XMM2r8_t), dimension(1:n), intent(in) :: alpha
        type(XMM2r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM2r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm2r8(l2,a0,O,inf)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,8
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm2r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm2r8(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_xmm2r8(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_xmm2r8(l2,a3,O,inf)
            a4      =  alpha(i+4)
            dc(i+4) =  defocus_cof_xmm2r8(l2,a4,O,inf)
            a5      =  alpha(i+5)
            dc(i+5) =  defocus_cof_xmm2r8(l2,a5,O,inf)
            a6      =  alpha(i+6)
            dc(i+6) =  defocus_cof_xmm2r8(l2,a6,O,inf)
            a7      =  alpha(i+7)
            dc(i+7) =  defocus_cof_xmm2r8(l2,a7,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_8x_xmm2r8


     subroutine defocus_cof_unroll_4x_xmm2r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_4x_xmm2r8
        !dir$ attributes forceinline :: defocus_cof_unroll_4x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_4x_xmm2r8
        type(XMM2r8_t),                 intent(in) :: l2
        type(XMM2r8_t), dimension(1:n), intent(in) :: alpha
        type(XMM2r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM2r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1,a2,a3
        !dir$ attributes align : 16:: a0,a1,a2,a3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm2r8(l2,a0,O,inf)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,4
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm2r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm2r8(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_xmm2r8(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_xmm2r8(l2,a3,O,inf)
        end do
    end subroutine defocus_cof_unroll_4x_xmm2r8


    subroutine defocus_cof_unroll_4x_omp_xmm2r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_4x_omp_xmm2r8
        !dir$ attributes forceinline :: defocus_cof_unroll_4x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_4x_omp_xmm2r8
        type(XMM2r8_t),                 intent(in) :: l2
        type(XMM2r8_t), dimension(1:n), intent(in) :: alpha
        type(XMM2r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM2r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1,a2,a3
        !dir$ attributes align : 16:: a0,a1,a2,a3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm2r8(l2,a0,O,inf)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,4
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm2r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm2r8(l2,a1,O,inf)
            a2      =  alpha(i+2)
            dc(i+2) =  defocus_cof_xmm2r8(l2,a2,O,inf)
            a3      =  alpha(i+3)
            dc(i+3) =  defocus_cof_xmm2r8(l2,a3,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_4x_xmm2r8

    
    subroutine defocus_cof_unroll_2x_xmm2r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_2x_xmm2r8
        !dir$ attributes forceinline :: defocus_cof_unroll_2x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_2x_xmm2r8
        type(XMM2r8_t),                 intent(in) :: l2
        type(XMM2r8_t), dimension(1:n), intent(in) :: alpha
        type(XMM2r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM2r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1
        !dir$ attributes align : 16:: a0,a1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm2r8(l2,a0,O,inf)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=m1,n,2
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm2r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm2r8(l2,a1,O,inf)
        end do
    end subroutine defocus_cof_unroll_2x_xmm2r8


    


    subroutine defocus_cof_unroll_2x_omp_xmm2r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_unroll_2x_omp_xmm2r8
        !dir$ attributes forceinline :: defocus_cof_unroll_2x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_unroll_2x_omp_xmm2r8
        type(XMM2r8_t),                 intent(in) :: l2
        type(XMM2r8_t), dimension(1:n), intent(in) :: alpha
        type(XMM2r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM2r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1
        !dir$ attributes align : 16:: a0,a1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              a0     =  alpha(i)
              dc(i)  =  defocus_cof_xmm2r8(l2,a0,O,inf)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1)
        !$omp shared(n,alpha,dc,O,inf)
        do i=m1,n,2
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm2r8(l2,a0,O,inf)
            a1      =  alpha(i+1)
            dc(i+1) =  defocus_cof_xmm2r8(l2,a1,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_unroll_2x_xmm2r8


    subroutine defocus_cof_rolled_xmm2r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_rolled_xmm2r8
        !dir$ attributes forceinline :: defocus_cof_rolled_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_rolled_xmm2r8
        type(XMM2r8_t),                 intent(in) :: l2
        type(XMM2r8_t), dimension(1:n), intent(in) :: alpha
        type(XMM2r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM2r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM2r8_t), automatic :: a0
        !dir$ attributes align : 16:: a0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        do i=1,n
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm2r8(l2,a0,O,inf)
        end do
    end subroutine defocus_cof_rolled_xmm2r8


    subroutine defocus_cof_rolled_omp_xmm2r8(l2,alpha,O,inf,dc,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_rolled_omp_xmm2r8
        !dir$ attributes forceinline :: defocus_cof_rolled_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: defocus_cof_rolled_omp_xmm2r8
        type(XMM2r8_t),                 intent(in) :: l2
        type(XMM2r8_t), dimension(1:n), intent(in) :: alpha
        type(XMM2r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM2r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        type(XMM2r8_t), automatic :: a0
        !dir$ attributes align : 16:: a0
        integer(kind=i4) :: i
       
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned dc:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0)
        !$omp shared(n,alpha,dc,O,inf)
        do i=1,n
            a0      =  alpha(i)
            dc(i)   =  defocus_cof_xmm2r8(l2,a0,O,inf)
        end do
        !$omp end parallel do
    end subroutine defocus_cof_rolled_xmm2r8


    subroutine defocus_cof_dispatch_xmm2r8(l2,alpha,O,inf,dc,n,unroll_cnt,omp_ver)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: defocus_cof_dispatch_xmm2r8
        type(XMM2r8_t),                 intent(in) :: l2
        type(XMM2r8_t), dimension(1:n), intent(in) :: alpha
        type(XMM2r8_t),                 intent(in) :: O
        logical(kind=i4),                intent(in) :: inf
        type(XMM2r8_t), dimension(1:n), intent(out):: dc
        integer(kind=i4),                intent(in) :: n
        integer(kind=i4),                intent(in) :: unroll_cnt
        logical(kind=i4),                intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
               case (16)
                  call defocus_cof_unroll_16x_omp_xmm2r8(l2,alpha,O,inf,dc,n)
               case (8)
                  call defocus_cof_unroll_8x_omp_xmm2r8(l2,alpha,O,inf,dc,n)
               case (4)
                  call defocus_cof_unroll_4x_omp_xmm2r8(l2,alpha,O,inf,dc,n)
               case (2)
                  call defocus_cof_unroll_2x_omp_xmm2r8(l2,alpha,O,inf,dc,n)
               case (0)
                  call defocus_cof_rolled_omp_xmm2r8(l2,alpha,O,inf,dc,n)
               case default
                  return
           end select
        else
           select case (unroll_cnt)
               case (16)
                  call defocus_cof_unroll_16x_xmm2r8(l2,alpha,O,inf,dc,n)
               case (8)
                  call defocus_cof_unroll_8x_xmm2r8(l2,alpha,O,inf,dc,n)
               case (4)
                  call defocus_cof_unroll_4x_xmm2r8(l2,alpha,O,inf,dc,n)
               case (2)
                  call defocus_cof_unroll_2x_xmm2r8(l2,alpha,O,inf,dc,n)
               case (0)
                  call defocus_cof_rolled_xmm2r8(l2,alpha,O,inf,dc,n)
               case default
                  return
           end select 
        end if
    end subroutine defocus_cof_dispatch_xmm2r8
    
    
    !//////////////////////////////////////////////////////////////////////////////!
    !//////////////////////////////////////////////////////////////////////////////!
    
    
      ! Диаметр кружка рассеяния р
    ! Formula 3, p.59
   



     
     pure function circle_dispersion_xmm4r4(d,l1,l2,alpha,O,inf) result(rho)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_xmm4r4
        !dir$ attributes forceinline :: circle_dispersion_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_xmm4r4
        type(XMM4r4_t),  intent(in) :: d
        type(XMM4r4_t),  intent(in) :: l1
        type(XMM4r4_t),  intent(in) :: l2
        type(XMM4r4_t),  intent(in) :: alpha
        type(XMM4r4_t),  intent(in) :: O
        logical(kind=i4), intent(in) :: inf
        type(XMM4r4_t) :: rho
        type(XMM4r4_t), automatic :: t0,t1
        !dir$ attributes align : 16:: t0,t1
        t0 = d.v/(l1.v+l2.v)
        t1 = defocus_cof_xmm4r4(l2,alpha,O,inf)
        rho= t0.v*t1.v
    end function circle_dispersion_xmm4r4


    subroutine circle_dispersion_unroll_16x_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_16x_xmm4r4
        !dir$ attributes forceinline :: circle_dispersion_unroll_16x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_16x_xmm4r4
        type(XMM4r4_t),                   intent(in) :: d
        type(XMM4r4_t),                   intent(in) :: l1
        type(XMM4r4_t),                   intent(in) :: l2
        type(XMM4r4_t), dimension(1:n),   intent(in) :: alpha
        type(XMM4r4_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM4r4_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        type(XMM4r4_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always 
        do i=m1,n,16
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm4r4(d,l1,l2,a1,O,inf)
             a2        = alpha(i+2)
             rho(i+2)  = circle_dispersion_xmm4r4(d,l1,l2,a2,O,inf)
             a3        = alpha(i+3)
             rho(i+3)  = circle_dispersion_xmm4r4(d,l1,l2,a3,O,inf)
             a4        = alpha(i+1)
             rho(i+4)  = circle_dispersion_xmm4r4(d,l1,l2,a4,O,inf)
             a5        = alpha(i+5)
             rho(i+5)  = circle_dispersion_xmm4r4(d,l1,l2,a5,O,inf)
             a6        = alpha(i+6)
             rho(i+6)  = circle_dispersion_xmm4r4(d,l1,l2,a6,O,inf)
             a7        = alpha(i+7)
             rho(i+7)  = circle_dispersion_xmm4r4(d,l1,l2,a7,O,inf)
             a8        = alpha(i+8)
             rho(i+8)  = circle_dispersion_xmm4r4(d,l1,l2,a8,O,inf)  
             a9        = alpha(i+9)
             rho(i+9)  = circle_dispersion_xmm4r4(d,l1,l2,a9,O,inf)
             a10       = alpha(i+10)
             rho(i+10) = circle_dispersion_xmm4r4(d,l1,l2,a10,O,inf)
             a11       = alpha(i+1)
             rho(i+11) = circle_dispersion_xmm4r4(d,l1,l2,a11,O,inf)
             a12       = alpha(i+12)
             rho(i+12) = circle_dispersion_xmm4r4(d,l1,l2,a12,O,inf)
             a13       = alpha(i+13)
             rho(i+13) = circle_dispersion_xmm4r4(d,l1,l2,a13,O,inf)
             a14       = alpha(i+14)
             rho(i+14) = circle_dispersion_xmm4r4(d,l1,l2,a14,O,inf) 
             a15       = alpha(i+15)
             rho(i+15) = circle_dispersion_xmm4r4(d,l1,l2,a15,O,inf)
         end do
    end subroutine circle_dispersion_unroll_16x_xmm4r4


    subroutine circle_dispersion_unroll_16x_omp_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_16x_omp_xmm4r4
        !dir$ attributes forceinline :: circle_dispersion_unroll_16x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_16x_omp_xmm4r4
        type(XMM4r4_t),                   intent(in) :: d
        type(XMM4r4_t),                   intent(in) :: l1
        type(XMM4r4_t),                   intent(in) :: l2
        type(XMM4r4_t), dimension(1:n),   intent(in) :: alpha
        type(XMM4r4_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM4r4_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        type(XMM4r4_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always 
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)      &
        !$omp private(a8,a9,a10,a11,a12,a13,a14,a15)                 &
        !$omp shared(n,d,l1,l2,O,inf,rho,alpha)
        do i=m1,n,16
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm4r4(d,l1,l2,a1,O,inf)
             a2        = alpha(i+2)
             rho(i+2)  = circle_dispersion_xmm4r4(d,l1,l2,a2,O,inf)
             a3        = alpha(i+3)
             rho(i+3)  = circle_dispersion_xmm4r4(d,l1,l2,a3,O,inf)
             a4        = alpha(i+1)
             rho(i+4)  = circle_dispersion_xmm4r4(d,l1,l2,a4,O,inf)
             a5        = alpha(i+5)
             rho(i+5)  = circle_dispersion_xmm4r4(d,l1,l2,a5,O,inf)
             a6        = alpha(i+6)
             rho(i+6)  = circle_dispersion_xmm4r4(d,l1,l2,a6,O,inf)
             a7        = alpha(i+7)
             rho(i+7)  = circle_dispersion_xmm4r4(d,l1,l2,a7,O,inf)
             a8        = alpha(i+8)
             rho(i+8)  = circle_dispersion_xmm4r4(d,l1,l2,a8,O,inf)  
             a9        = alpha(i+9)
             rho(i+9)  = circle_dispersion_xmm4r4(d,l1,l2,a9,O,inf)
             a10       = alpha(i+10)
             rho(i+10) = circle_dispersion_xmm4r4(d,l1,l2,a10,O,inf)
             a11       = alpha(i+1)
             rho(i+11) = circle_dispersion_xmm4r4(d,l1,l2,a11,O,inf)
             a12       = alpha(i+12)
             rho(i+12) = circle_dispersion_xmm4r4(d,l1,l2,a12,O,inf)
             a13       = alpha(i+13)
             rho(i+13) = circle_dispersion_xmm4r4(d,l1,l2,a13,O,inf)
             a14       = alpha(i+14)
             rho(i+14) = circle_dispersion_xmm4r4(d,l1,l2,a14,O,inf) 
             a15       = alpha(i+15)
             rho(i+15) = circle_dispersion_xmm4r4(d,l1,l2,a15,O,inf)
         end do
         !$omp end parallel do
    end subroutine circle_dispersion_unroll_16x_omp_xmm4r4
    
    
    subroutine circle_dispersion_unroll_8x_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_8x_xmm4r4
        !dir$ attributes forceinline :: circle_dispersion_unroll_8x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_8x_xmm4r4
        type(XMM4r4_t),                   intent(in) :: d
        type(XMM4r4_t),                   intent(in) :: l1
        type(XMM4r4_t),                   intent(in) :: l2
        type(XMM4r4_t), dimension(1:n),   intent(in) :: alpha
        type(XMM4r4_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM4r4_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always 
        do i=m1,n,8
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm4r4(d,l1,l2,a1,O,inf)
             a2        = alpha(i+2)
             rho(i+2)  = circle_dispersion_xmm4r4(d,l1,l2,a2,O,inf)
             a3        = alpha(i+3)
             rho(i+3)  = circle_dispersion_xmm4r4(d,l1,l2,a3,O,inf)
             a4        = alpha(i+1)
             rho(i+4)  = circle_dispersion_xmm4r4(d,l1,l2,a4,O,inf)
             a5        = alpha(i+5)
             rho(i+5)  = circle_dispersion_xmm4r4(d,l1,l2,a5,O,inf)
             a6        = alpha(i+6)
             rho(i+6)  = circle_dispersion_xmm4r4(d,l1,l2,a6,O,inf)
             a7        = alpha(i+7)
             rho(i+7)  = circle_dispersion_xmm4r4(d,l1,l2,a7,O,inf)
        end do
    end subroutine circle_dispersion_unroll_8x_xmm4r4


    subroutine circle_dispersion_unroll_8x_omp_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_8x_omp_xmm4r4
        !dir$ attributes forceinline :: circle_dispersion_unroll_8x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_8x_omp_xmm4r4
        type(XMM4r4_t),                   intent(in) :: d
        type(XMM4r4_t),                   intent(in) :: l1
        type(XMM4r4_t),                   intent(in) :: l2
        type(XMM4r4_t), dimension(1:n),   intent(in) :: alpha
        type(XMM4r4_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM4r4_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always 
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)      &
        !$omp shared(n,d,l1,l2,O,inf,rho,alpha)
        do i=m1,n,8
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm4r4(d,l1,l2,a1,O,inf)
             a2        = alpha(i+2)
             rho(i+2)  = circle_dispersion_xmm4r4(d,l1,l2,a2,O,inf)
             a3        = alpha(i+3)
             rho(i+3)  = circle_dispersion_xmm4r4(d,l1,l2,a3,O,inf)
             a4        = alpha(i+1)
             rho(i+4)  = circle_dispersion_xmm4r4(d,l1,l2,a4,O,inf)
             a5        = alpha(i+5)
             rho(i+5)  = circle_dispersion_xmm4r4(d,l1,l2,a5,O,inf)
             a6        = alpha(i+6)
             rho(i+6)  = circle_dispersion_xmm4r4(d,l1,l2,a6,O,inf)
             a7        = alpha(i+7)
             rho(i+7)  = circle_dispersion_xmm4r4(d,l1,l2,a7,O,inf)
         end do
         !$omp end parallel do
    end subroutine circle_dispersion_unroll_8x_omp_xmm4r4


    subroutine circle_dispersion_unroll_4x_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_4x_xmm4r4
        !dir$ attributes forceinline :: circle_dispersion_unroll_4x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_4x_xmm4r4
        type(XMM4r4_t),                   intent(in) :: d
        type(XMM4r4_t),                   intent(in) :: l1
        type(XMM4r4_t),                   intent(in) :: l2
        type(XMM4r4_t), dimension(1:n),   intent(in) :: alpha
        type(XMM4r4_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM4r4_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1,a2,a3
        !dir$ attributes align : 16:: a0,a1,a2,a3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always 
        do i=m1,n,4
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm4r4(d,l1,l2,a1,O,inf)
             a2        = alpha(i+2)
             rho(i+2)  = circle_dispersion_xmm4r4(d,l1,l2,a2,O,inf)
             a3        = alpha(i+3)
             rho(i+3)  = circle_dispersion_xmm4r4(d,l1,l2,a3,O,inf)
        end do
    end subroutine circle_dispersion_unroll_4x_xmm4r4


    subroutine circle_dispersion_unroll_4x_omp_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_4x_omp_xmm4r4
        !dir$ attributes forceinline :: circle_dispersion_unroll_4x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_4x_omp_xmm4r4
        type(XMM4r4_t),                   intent(in) :: d
        type(XMM4r4_t),                   intent(in) :: l1
        type(XMM4r4_t),                   intent(in) :: l2
        type(XMM4r4_t), dimension(1:n),   intent(in) :: alpha
        type(XMM4r4_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM4r4_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1,a2,a3
        !dir$ attributes align : 16:: a0,a1,a2,a3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always 
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3)      &
        !$omp shared(n,d,l1,l2,O,inf,rho,alpha)
        do i=m1,n,4
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm4r4(d,l1,l2,a1,O,inf)
             a2        = alpha(i+2)
             rho(i+2)  = circle_dispersion_xmm4r4(d,l1,l2,a2,O,inf)
             a3        = alpha(i+3)
             rho(i+3)  = circle_dispersion_xmm4r4(d,l1,l2,a3,O,inf)
         end do
         !$omp end parallel do
    end subroutine circle_dispersion_unroll_4x_omp_xmm4r4


    subroutine circle_dispersion_unroll_2x_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_2x_xmm4r4
        !dir$ attributes forceinline :: circle_dispersion_unroll_2x_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_2x_xmm4r4
        type(XMM4r4_t),                   intent(in) :: d
        type(XMM4r4_t),                   intent(in) :: l1
        type(XMM4r4_t),                   intent(in) :: l2
        type(XMM4r4_t), dimension(1:n),   intent(in) :: alpha
        type(XMM4r4_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM4r4_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1
        !dir$ attributes align : 16:: a0,a1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always 
        do i=m1,n,2
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm4r4(d,l1,l2,a1,O,inf)
        end do
    end subroutine circle_dispersion_unroll_2x_xmm4r4


    subroutine circle_dispersion_unroll_2x_omp_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_2x_omp_xmm4r4
        !dir$ attributes forceinline :: circle_dispersion_unroll_2x_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_2x_omp_xmm4r4
        type(XMM4r4_t),                   intent(in) :: d
        type(XMM4r4_t),                   intent(in) :: l1
        type(XMM4r4_t),                   intent(in) :: l2
        type(XMM4r4_t), dimension(1:n),   intent(in) :: alpha
        type(XMM4r4_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM4r4_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: a0,a1
        !dir$ attributes align : 16:: a0,a1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always 
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1)      &
        !$omp shared(n,d,l1,l2,O,inf,rho,alpha)
        do i=m1,n,2
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm4r4(d,l1,l2,a1,O,inf)
        end do
        !$omp end parallel do
    end subroutine circle_dispersion_unroll_2x_omp_xmm4r4


    
    subroutine circle_dispersion_rolled_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_rolled_xmm4r4
        !dir$ attributes forceinline :: circle_dispersion_rolled_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_rolled_xmm4r4
        type(XMM4r4_t),                   intent(in) :: d
        type(XMM4r4_t),                   intent(in) :: l1
        type(XMM4r4_t),                   intent(in) :: l2
        type(XMM4r4_t), dimension(1:n),   intent(in) :: alpha
        type(XMM4r4_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM4r4_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: a0
        !dir$ attributes align : 16:: a0
        integer(kind=i4) :: i
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always 
        do i=1,n
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
        end do
    end subroutine circle_dispersion_rolled_xmm4r4


    subroutine circle_dispersion_rolled_omp_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_rolled_omp_xmm4r4
        !dir$ attributes forceinline :: circle_dispersion_rolled_omp_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_rolled_omp_xmm4r4
        type(XMM4r4_t),                   intent(in) :: d
        type(XMM4r4_t),                   intent(in) :: l1
        type(XMM4r4_t),                   intent(in) :: l2
        type(XMM4r4_t), dimension(1:n),   intent(in) :: alpha
        type(XMM4r4_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM4r4_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM4r4_t), automatic :: a0
        !dir$ attributes align : 16:: a0
        integer(kind=i4) :: i
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(4)
        !dir$ vector always 
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp private(a0)      &
        !$omp shared(n,d,l1,l2,O,inf,rho,alpha)
        do i=1,n
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm4r4(d,l1,l2,a0,O,inf)
        end do
        !$omp end parallel do
    end subroutine circle_dispersion_rolled_omp_xmm4r4



    subroutine circle_dispersion_dispatch_xmm4r4(d,l1,l2,alpha,O,inf,rho,n,unroll_cnt,omp_ver)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_dispatch_xmm4r4
        type(XMM4r4_t),                   intent(in) :: d
        type(XMM4r4_t),                   intent(in) :: l1
        type(XMM4r4_t),                   intent(in) :: l2
        type(XMM4r4_t), dimension(1:n),   intent(in) :: alpha
        type(XMM4r4_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM4r4_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        integer(kind=i4),                  intent(in) :: unroll_cnt
        logical(kind=i4),                  intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
               case (16)
                  call circle_dispersion_unroll_16x_omp_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
               case (8)
                  call circle_dispersion_unroll_8x_omp_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
               case (4)
                  call circle_dispersion_unroll_4x_omp_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
               case (2)
                  call circle_dispersion_unroll_2x_omp_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
               case (0)
                  call circle_dispersion_rolled_omp_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
               case default
                  return
            end select
         else
            select case (unroll_cnt)
               case (16)
                  call circle_dispersion_unroll_16x_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
               case (8)
                  call circle_dispersion_unroll_8x_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
               case (4)
                  call circle_dispersion_unroll_4x_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
               case (2)
                  call circle_dispersion_unroll_2x_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
               case (0)
                  call circle_dispersion_rolled_xmm4r4(d,l1,l2,alpha,O,inf,rho,n)
               case default
                  return
            end select
         end if
    end subroutine circle_dispersion_dispatch_xmm4r4
    
    
    !/////////////////////////////////////////////////////////!
    !//////////////////////////////////////////////////////////!
    
    
    pure function circle_dispersion_xmm2r8(d,l1,l2,alpha,O,inf) result(rho)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_xmm2r8
        !dir$ attributes forceinline :: circle_dispersion_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_xmm2r8
        type(XMM2r8_t),  intent(in) :: d
        type(XMM2r8_t),  intent(in) :: l1
        type(XMM2r8_t),  intent(in) :: l2
        type(XMM2r8_t),  intent(in) :: alpha
        type(XMM2r8_t),  intent(in) :: O
        logical(kind=i4), intent(in) :: inf
        type(XMM2r8_t) :: rho
        type(XMM2r8_t), automatic :: t0,t1
        !dir$ attributes align : 16:: t0,t1
        t0 = d.v/(l1.v+l2.v)
        t1 = defocus_cof_xmm2r8(l2,alpha,O,inf)
        rho= t0.v*t1.v
     end function circle_dispersion_xmm2r8


     subroutine circle_dispersion_unroll_16x_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_16x_xmm2r8
        !dir$ attributes forceinline :: circle_dispersion_unroll_16x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_16x_xmm2r8
        type(XMM2r8_t),                   intent(in) :: d
        type(XMM2r8_t),                   intent(in) :: l1
        type(XMM2r8_t),                   intent(in) :: l2
        type(XMM2r8_t), dimension(1:n),   intent(in) :: alpha
        type(XMM2r8_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM2r8_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        type(XMM2r8_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always 
        do i=m1,n,16
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm2r8(d,l1,l2,a1,O,inf)
             a2        = alpha(i+2)
             rho(i+2)  = circle_dispersion_xmm2r8(d,l1,l2,a2,O,inf)
             a3        = alpha(i+3)
             rho(i+3)  = circle_dispersion_xmm2r8(d,l1,l2,a3,O,inf)
             a4        = alpha(i+1)
             rho(i+4)  = circle_dispersion_xmm2r8(d,l1,l2,a4,O,inf)
             a5        = alpha(i+5)
             rho(i+5)  = circle_dispersion_xmm2r8(d,l1,l2,a5,O,inf)
             a6        = alpha(i+6)
             rho(i+6)  = circle_dispersion_xmm2r8(d,l1,l2,a6,O,inf)
             a7        = alpha(i+7)
             rho(i+7)  = circle_dispersion_xmm2r8(d,l1,l2,a7,O,inf)
             a8        = alpha(i+8)
             rho(i+8)  = circle_dispersion_xmm2r8(d,l1,l2,a8,O,inf)  
             a9        = alpha(i+9)
             rho(i+9)  = circle_dispersion_xmm2r8(d,l1,l2,a9,O,inf)
             a10       = alpha(i+10)
             rho(i+10) = circle_dispersion_xmm2r8(d,l1,l2,a10,O,inf)
             a11       = alpha(i+1)
             rho(i+11) = circle_dispersion_xmm2r8(d,l1,l2,a11,O,inf)
             a12       = alpha(i+12)
             rho(i+12) = circle_dispersion_xmm2r8(d,l1,l2,a12,O,inf)
             a13       = alpha(i+13)
             rho(i+13) = circle_dispersion_xmm2r8(d,l1,l2,a13,O,inf)
             a14       = alpha(i+14)
             rho(i+14) = circle_dispersion_xmm2r8(d,l1,l2,a14,O,inf) 
             a15       = alpha(i+15)
             rho(i+15) = circle_dispersion_xmm2r8(d,l1,l2,a15,O,inf)
         end do
    end subroutine circle_dispersion_unroll_16x_xmm2r8


    subroutine circle_dispersion_unroll_16x_omp_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_16x_omp_xmm2r8
        !dir$ attributes forceinline :: circle_dispersion_unroll_16x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_16x_omp_xmm2r8
        type(XMM2r8_t),                   intent(in) :: d
        type(XMM2r8_t),                   intent(in) :: l1
        type(XMM2r8_t),                   intent(in) :: l2
        type(XMM2r8_t), dimension(1:n),   intent(in) :: alpha
        type(XMM2r8_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM2r8_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        type(XMM2r8_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
        integer(kind=i4) :: i,m,m1
        m = mod(n,16)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
           end do
           if(n<16) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always 
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)      &
        !$omp private(a8,a9,a10,a11,a12,a13,a14,a15)                 &
        !$omp shared(n,d,l1,l2,O,inf,rho,alpha)
        do i=m1,n,16
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm2r8(d,l1,l2,a1,O,inf)
             a2        = alpha(i+2)
             rho(i+2)  = circle_dispersion_xmm2r8(d,l1,l2,a2,O,inf)
             a3        = alpha(i+3)
             rho(i+3)  = circle_dispersion_xmm2r8(d,l1,l2,a3,O,inf)
             a4        = alpha(i+1)
             rho(i+4)  = circle_dispersion_xmm2r8(d,l1,l2,a4,O,inf)
             a5        = alpha(i+5)
             rho(i+5)  = circle_dispersion_xmm2r8(d,l1,l2,a5,O,inf)
             a6        = alpha(i+6)
             rho(i+6)  = circle_dispersion_xmm2r8(d,l1,l2,a6,O,inf)
             a7        = alpha(i+7)
             rho(i+7)  = circle_dispersion_xmm2r8(d,l1,l2,a7,O,inf)
             a8        = alpha(i+8)
             rho(i+8)  = circle_dispersion_xmm2r8(d,l1,l2,a8,O,inf)  
             a9        = alpha(i+9)
             rho(i+9)  = circle_dispersion_xmm2r8(d,l1,l2,a9,O,inf)
             a10       = alpha(i+10)
             rho(i+10) = circle_dispersion_xmm2r8(d,l1,l2,a10,O,inf)
             a11       = alpha(i+1)
             rho(i+11) = circle_dispersion_xmm2r8(d,l1,l2,a11,O,inf)
             a12       = alpha(i+12)
             rho(i+12) = circle_dispersion_xmm2r8(d,l1,l2,a12,O,inf)
             a13       = alpha(i+13)
             rho(i+13) = circle_dispersion_xmm2r8(d,l1,l2,a13,O,inf)
             a14       = alpha(i+14)
             rho(i+14) = circle_dispersion_xmm2r8(d,l1,l2,a14,O,inf) 
             a15       = alpha(i+15)
             rho(i+15) = circle_dispersion_xmm2r8(d,l1,l2,a15,O,inf)
         end do
         !$omp end parallel do
    end subroutine circle_dispersion_unroll_16x_omp_xmm2r8


    subroutine circle_dispersion_unroll_8x_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_8x_xmm2r8
        !dir$ attributes forceinline :: circle_dispersion_unroll_8x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_8x_xmm2r8
        type(XMM2r8_t),                   intent(in) :: d
        type(XMM2r8_t),                   intent(in) :: l1
        type(XMM2r8_t),                   intent(in) :: l2
        type(XMM2r8_t), dimension(1:n),   intent(in) :: alpha
        type(XMM2r8_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM2r8_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always 
        do i=m1,n,8
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm2r8(d,l1,l2,a1,O,inf)
             a2        = alpha(i+2)
             rho(i+2)  = circle_dispersion_xmm2r8(d,l1,l2,a2,O,inf)
             a3        = alpha(i+3)
             rho(i+3)  = circle_dispersion_xmm2r8(d,l1,l2,a3,O,inf)
             a4        = alpha(i+1)
             rho(i+4)  = circle_dispersion_xmm2r8(d,l1,l2,a4,O,inf)
             a5        = alpha(i+5)
             rho(i+5)  = circle_dispersion_xmm2r8(d,l1,l2,a5,O,inf)
             a6        = alpha(i+6)
             rho(i+6)  = circle_dispersion_xmm2r8(d,l1,l2,a6,O,inf)
             a7        = alpha(i+7)
             rho(i+7)  = circle_dispersion_xmm2r8(d,l1,l2,a7,O,inf)
        end do
    end subroutine circle_dispersion_unroll_8x_xmm2r8


    subroutine circle_dispersion_unroll_8x_omp_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_8x_omp_xmm2r8
        !dir$ attributes forceinline :: circle_dispersion_unroll_8x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_8x_omp_xmm2r8
        type(XMM2r8_t),                   intent(in) :: d
        type(XMM2r8_t),                   intent(in) :: l1
        type(XMM2r8_t),                   intent(in) :: l2
        type(XMM2r8_t), dimension(1:n),   intent(in) :: alpha
        type(XMM2r8_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM2r8_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
        !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
        integer(kind=i4) :: i,m,m1
        m = mod(n,8)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
           end do
           if(n<8) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always 
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)      &
        !$omp shared(n,d,l1,l2,O,inf,rho,alpha)
        do i=m1,n,8
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm2r8(d,l1,l2,a1,O,inf)
             a2        = alpha(i+2)
             rho(i+2)  = circle_dispersion_xmm2r8(d,l1,l2,a2,O,inf)
             a3        = alpha(i+3)
             rho(i+3)  = circle_dispersion_xmm2r8(d,l1,l2,a3,O,inf)
             a4        = alpha(i+1)
             rho(i+4)  = circle_dispersion_xmm2r8(d,l1,l2,a4,O,inf)
             a5        = alpha(i+5)
             rho(i+5)  = circle_dispersion_xmm2r8(d,l1,l2,a5,O,inf)
             a6        = alpha(i+6)
             rho(i+6)  = circle_dispersion_xmm2r8(d,l1,l2,a6,O,inf)
             a7        = alpha(i+7)
             rho(i+7)  = circle_dispersion_xmm2r8(d,l1,l2,a7,O,inf)
         end do
         !$omp end parallel do
    end subroutine circle_dispersion_unroll_8x_omp_xmm2r8


    subroutine circle_dispersion_unroll_4x_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_4x_xmm2r8
        !dir$ attributes forceinline :: circle_dispersion_unroll_4x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_4x_xmm2r8
        type(XMM2r8_t),                   intent(in) :: d
        type(XMM2r8_t),                   intent(in) :: l1
        type(XMM2r8_t),                   intent(in) :: l2
        type(XMM2r8_t), dimension(1:n),   intent(in) :: alpha
        type(XMM2r8_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM2r8_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1,a2,a3
        !dir$ attributes align : 16:: a0,a1,a2,a3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always 
        do i=m1,n,4
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm2r8(d,l1,l2,a1,O,inf)
             a2        = alpha(i+2)
             rho(i+2)  = circle_dispersion_xmm2r8(d,l1,l2,a2,O,inf)
             a3        = alpha(i+3)
             rho(i+3)  = circle_dispersion_xmm2r8(d,l1,l2,a3,O,inf)
        end do
    end subroutine circle_dispersion_unroll_4x_xmm2r8


    subroutine circle_dispersion_unroll_4x_omp_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_4x_omp_xmm2r8
        !dir$ attributes forceinline :: circle_dispersion_unroll_4x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_4x_omp_xmm2r8
        type(XMM2r8_t),                   intent(in) :: d
        type(XMM2r8_t),                   intent(in) :: l1
        type(XMM2r8_t),                   intent(in) :: l2
        type(XMM2r8_t), dimension(1:n),   intent(in) :: alpha
        type(XMM2r8_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM2r8_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1,a2,a3
        !dir$ attributes align : 16:: a0,a1,a2,a3
        integer(kind=i4) :: i,m,m1
        m = mod(n,4)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
           end do
           if(n<4) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always 
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1,a2,a3)      &
        !$omp shared(n,d,l1,l2,O,inf,rho,alpha)
        do i=m1,n,4
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm2r8(d,l1,l2,a1,O,inf)
             a2        = alpha(i+2)
             rho(i+2)  = circle_dispersion_xmm2r8(d,l1,l2,a2,O,inf)
             a3        = alpha(i+3)
             rho(i+3)  = circle_dispersion_xmm2r8(d,l1,l2,a3,O,inf)
         end do
         !$omp end parallel do
    end subroutine circle_dispersion_unroll_4x_omp_xmm2r8


    subroutine circle_dispersion_unroll_2x_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_2x_xmm2r8
        !dir$ attributes forceinline :: circle_dispersion_unroll_2x_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_2x_xmm2r8
        type(XMM2r8_t),                   intent(in) :: d
        type(XMM2r8_t),                   intent(in) :: l1
        type(XMM2r8_t),                   intent(in) :: l2
        type(XMM2r8_t), dimension(1:n),   intent(in) :: alpha
        type(XMM2r8_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM2r8_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1
        !dir$ attributes align : 16:: a0,a1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always 
        do i=m1,n,2
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm2r8(d,l1,l2,a1,O,inf)
        end do
    end subroutine circle_dispersion_unroll_2x_xmm2r8


    subroutine circle_dispersion_unroll_2x_omp_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_unroll_2x_omp_xmm2r8
        !dir$ attributes forceinline :: circle_dispersion_unroll_2x_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_unroll_2x_omp_xmm2r8
        type(XMM2r8_t),                   intent(in) :: d
        type(XMM2r8_t),                   intent(in) :: l1
        type(XMM2r8_t),                   intent(in) :: l2
        type(XMM2r8_t), dimension(1:n),   intent(in) :: alpha
        type(XMM2r8_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM2r8_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: a0,a1
        !dir$ attributes align : 16:: a0,a1
        integer(kind=i4) :: i,m,m1
        m = mod(n,2)
        if(m /= 0) then
           do i=1,m
              a0       = alpha(i)
              rho(i)   = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
           end do
           if(n<2) return
        end if
        m1 = m+1
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always 
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp firstprivate(m1) private(a0,a1)      &
        !$omp shared(n,d,l1,l2,O,inf,rho,alpha)
        do i=m1,n,2
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
             a1        = alpha(i+1)
             rho(i+1)  = circle_dispersion_xmm2r8(d,l1,l2,a1,O,inf)
        end do
        !$omp end parallel do
    end subroutine circle_dispersion_unroll_2x_omp_xmm2r8


    
    subroutine circle_dispersion_rolled_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_rolled_xmm2r8
        !dir$ attributes forceinline :: circle_dispersion_rolled_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_rolled_xmm2r8
        type(XMM2r8_t),                   intent(in) :: d
        type(XMM2r8_t),                   intent(in) :: l1
        type(XMM2r8_t),                   intent(in) :: l2
        type(XMM2r8_t), dimension(1:n),   intent(in) :: alpha
        type(XMM2r8_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM2r8_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: a0
        !dir$ attributes align : 16:: a0
        integer(kind=i4) :: i
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always 
        do i=1,n
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
        end do
    end subroutine circle_dispersion_rolled_xmm2r8


    subroutine circle_dispersion_rolled_omp_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_rolled_omp_xmm2r8
        !dir$ attributes forceinline :: circle_dispersion_rolled_omp_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circle_dispersion_rolled_omp_xmm2r8
        type(XMM2r8_t),                   intent(in) :: d
        type(XMM2r8_t),                   intent(in) :: l1
        type(XMM2r8_t),                   intent(in) :: l2
        type(XMM2r8_t), dimension(1:n),   intent(in) :: alpha
        type(XMM2r8_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM2r8_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        type(XMM2r8_t), automatic :: a0
        !dir$ attributes align : 16:: a0
        integer(kind=i4) :: i
        !dir$ assume_aligned alpha:16
        !dir$ assume_aligned rho:16
        !dir$ vector aligned
        !dir$ ivdep
        !dir$ vector vectorlength(8)
        !dir$ vector always 
        !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
        !$omp private(a0)      &
        !$omp shared(n,d,l1,l2,O,inf,rho,alpha)
        do i=1,n
             a0        = alpha(i)
             rho(i)    = circle_dispersion_xmm2r8(d,l1,l2,a0,O,inf)
        end do
        !$omp end parallel do
    end subroutine circle_dispersion_rolled_omp_xmm2r8



    subroutine circle_dispersion_dispatch_xmm2r8(d,l1,l2,alpha,O,inf,rho,n,unroll_cnt,omp_ver)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circle_dispersion_dispatch_xmm2r8
        type(XMM2r8_t),                   intent(in) :: d
        type(YMM8r8_t),                   intent(in) :: l1
        type(XMM2r8_t),                   intent(in) :: l2
        type(XMM2r8_t), dimension(1:n),   intent(in) :: alpha
        type(XMM2r8_t),                   intent(in) :: O
        logical(kind=i4),                  intent(in) :: inf
        type(XMM2r8_t), dimension(1:n),   intent(in) :: rho
        integer(kind=i4),                  intent(in) :: n
        integer(kind=i4),                  intent(in) :: unroll_cnt
        logical(kind=i4),                  intent(in) :: omp_ver
        if(omp_ver) then
           select case (unroll_cnt)
               case (16)
                  call circle_dispersion_unroll_16x_omp_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
               case (8)
                  call circle_dispersion_unroll_8x_omp_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
               case (4)
                  call circle_dispersion_unroll_4x_omp_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
               case (2)
                  call circle_dispersion_unroll_2x_omp_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
               case (0)
                  call circle_dispersion_rolled_omp_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
               case default
                  return
            end select
         else
            select case (unroll_cnt)
               case (16)
                  call circle_dispersion_unroll_16x_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
               case (8)
                  call circle_dispersion_unroll_8x_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
               case (4)
                  call circle_dispersion_unroll_4x_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
               case (2)
                  call circle_dispersion_unroll_2x_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
               case (0)
                  call circle_dispersion_rolled_xmm2r8(d,l1,l2,alpha,O,inf,rho,n)
               case default
                  return
            end select
         end if
    end subroutine circle_dispersion_dispatch_xmm2r8
    
    !/////////////////////////////////////////////////////////////////////////////!
    !//////////////////////////////////////////////////////////////////////////////!
    
       !Formula 2, p. 59
     
    pure function circ_dispers_diam_xmm4r4(l1,l2,alpha,O,inf) result(ratio)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circ_dispers_diam_xmm4r4
        !dir$ attributes forceinline :: circ_dispers_diam_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_xmm4r4
        type(XMM4r4_t),     intent(in) :: l1
        type(XMM4r4_t),     intent(in) :: l2
        type(XMM4r4_t),     intent(in) :: alpha
        type(XMM4r4_t),     intent(in) :: O
        logical(kind=i4),    intent(in) :: inf
        type(XMM4r4_t) :: ratio
        type(XMM4r4_t), automatic :: t0,t1
        !dir$ attributes align : 16:: t0,t1
        t0    = l1.v+l2.v
        t1    = defocus_cof_xmm4r4(l2,alpha,O,inf)
        ratio = t1.v/t0.v
     end function circ_dispers_diam_xmm4r4


     subroutine circ_dispers_diam_unroll_16x_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_16x_xmm4r4
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_16x_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_16x_xmm4r4
         type(XMM4r4_t),                     intent(in) :: l1
         type(XMM4r4_t),                     intent(in) :: l2
         type(XMM4r4_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM4r4_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM4r4_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
         type(XMM4r4_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
         !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
         !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
         integer(kind=i4) :: i,m,m1
         m = mod(n,16)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
            end do
            if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,16
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm4r4(l1,l2,a1,O,inf)
             a2         = alpha(i+2)
             ratio(i+2) = circ_dispers_diam_xmm4r4(l1,l2,a2,O,inf)
             a3         = alpha(i+3)
             ratio(i+3) = circ_dispers_diam_xmm4r4(l1,l2,a3,O,inf)
             a4         = alpha(i+4)
             ratio(i+4) = circ_dispers_diam_xmm4r4(l1,l2,a4,O,inf)
             a5         = alpha(i+5)
             ratio(i+5) = circ_dispers_diam_xmm4r4(l1,l2,a5,O,inf)
             a6         = alpha(i+6)
             ratio(i+6) = circ_dispers_diam_xmm4r4(l1,l2,a6,O,inf)
             a7         = alpha(i+7)
             ratio(i+7) = circ_dispers_diam_xmm4r4(l1,l2,a7,O,inf)
             a8         = alpha(i+8)
             ratio(i+8) = circ_dispers_diam_xmm4r4(l1,l2,a8,O,inf)
             a9         = alpha(i+9)
             ratio(i+9) = circ_dispers_diam_xmm4r4(l1,l2,a9,O,inf)
             a10        = alpha(i+10)
             ratio(i+10)= circ_dispers_diam_xmm4r4(l1,l2,a10,O,inf)
             a11        = alpha(i+11)
             ratio(i+1) = circ_dispers_diam_xmm4r4(l1,l2,a11,O,inf)
             a12        = alpha(i+12)
             ratio(i+12)= circ_dispers_diam_xmm4r4(l1,l2,a12,O,inf) 
             a13        = alpha(i+13)
             ratio(i+13)= circ_dispers_diam_xmm4r4(l1,l2,a13,O,inf)
             a14        = alpha(i+14)
             ratio(i+14)= circ_dispers_diam_xmm4r4(l1,l2,a14,O,inf)
             a15        = alpha(i+15)
             ratio(i+15)= circ_dispers_diam_xmm4r4(l1,l2,a15,O,inf)
         end do

      end subroutine circ_dispers_diam_unroll_16x_xmm4r4


      subroutine circ_dispers_diam_unroll_16x_omp_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_16x_omp_xmm4r4
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_16x_omp_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_16x_omp_xmm4r4
         type(XMM4r4_t),                     intent(in) :: l1
         type(XMM4r4_t),                     intent(in) :: l2
         type(XMM4r4_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM4r4_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM4r4_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
         type(XMM4r4_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
         !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
         !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
         integer(kind=i4) :: i,m,m1
         m = mod(n,16)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
            end do
            if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)      &
         !$omp private(a8,a9,a10,a11,a12,a13,a14,a15)                 &
         !$omp shared(n,l1,l2,alpha,O,inf,ratio)
         do i=m1,n,16
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm4r4(l1,l2,a1,O,inf)
             a2         = alpha(i+2)
             ratio(i+2) = circ_dispers_diam_xmm4r4(l1,l2,a2,O,inf)
             a3         = alpha(i+3)
             ratio(i+3) = circ_dispers_diam_xmm4r4(l1,l2,a3,O,inf)
             a4         = alpha(i+4)
             ratio(i+4) = circ_dispers_diam_xmm4r4(l1,l2,a4,O,inf)
             a5         = alpha(i+5)
             ratio(i+5) = circ_dispers_diam_xmm4r4(l1,l2,a5,O,inf)
             a6         = alpha(i+6)
             ratio(i+6) = circ_dispers_diam_xmm4r4(l1,l2,a6,O,inf)
             a7         = alpha(i+7)
             ratio(i+7) = circ_dispers_diam_xmm4r4(l1,l2,a7,O,inf)
             a8         = alpha(i+8)
             ratio(i+8) = circ_dispers_diam_xmm4r4(l1,l2,a8,O,inf)
             a9         = alpha(i+9)
             ratio(i+9) = circ_dispers_diam_xmm4r4(l1,l2,a9,O,inf)
             a10        = alpha(i+10)
             ratio(i+10)= circ_dispers_diam_xmm4r4(l1,l2,a10,O,inf)
             a11        = alpha(i+11)
             ratio(i+1) = circ_dispers_diam_xmm4r4(l1,l2,a11,O,inf)
             a12        = alpha(i+12)
             ratio(i+12)= circ_dispers_diam_xmm4r4(l1,l2,a12,O,inf) 
             a13        = alpha(i+13)
             ratio(i+13)= circ_dispers_diam_xmm4r4(l1,l2,a13,O,inf)
             a14        = alpha(i+14)
             ratio(i+14)= circ_dispers_diam_xmm4r4(l1,l2,a14,O,inf)
             a15        = alpha(i+15)
             ratio(i+15)= circ_dispers_diam_xmm4r4(l1,l2,a15,O,inf)
         end do
         !$omp end parallel do
     end subroutine circ_dispers_diam_unroll_16x_omp_xmm4r4

     
   
    subroutine circ_dispers_diam_unroll_8x_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_8x_xmm4r4
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_8x_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_8x_xmm4r4
         type(XMM4r4_t),                     intent(in) :: l1
         type(XMM4r4_t),                     intent(in) :: l2
         type(XMM4r4_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM4r4_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM4r4_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
         !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
         integer(kind=i4) :: i,m,m1
         m = mod(n,8)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
            end do
            if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,8
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm4r4(l1,l2,a1,O,inf)
             a2         = alpha(i+2)
             ratio(i+2) = circ_dispers_diam_xmm4r4(l1,l2,a2,O,inf)
             a3         = alpha(i+3)
             ratio(i+3) = circ_dispers_diam_xmm4r4(l1,l2,a3,O,inf)
             a4         = alpha(i+4)
             ratio(i+4) = circ_dispers_diam_xmm4r4(l1,l2,a4,O,inf)
             a5         = alpha(i+5)
             ratio(i+5) = circ_dispers_diam_xmm4r4(l1,l2,a5,O,inf)
             a6         = alpha(i+6)
             ratio(i+6) = circ_dispers_diam_xmm4r4(l1,l2,a6,O,inf)
             a7         = alpha(i+7)
             ratio(i+7) = circ_dispers_diam_xmm4r4(l1,l2,a7,O,inf)
         end do

      end subroutine circ_dispers_diam_unroll_8x_xmm4r4


      subroutine circ_dispers_diam_unroll_8x_omp_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_8x_omp_xmm4r4
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_8x_omp_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_8x_omp_xmm4r4
         type(XMM4r4_t),                     intent(in) :: l1
         type(XMM4r4_t),                     intent(in) :: l2
         type(XMM4r4_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM4r4_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM4r4_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
         !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
         integer(kind=i4) :: i,m,m1
         m = mod(n,8)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
            end do
            if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:1
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)      &
         !$omp shared(n,l1,l2,alpha,O,inf,ratio)
         do i=m1,n,8
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm4r4(l1,l2,a1,O,inf)
             a2         = alpha(i+2)
             ratio(i+2) = circ_dispers_diam_xmm4r4(l1,l2,a2,O,inf)
             a3         = alpha(i+3)
             ratio(i+3) = circ_dispers_diam_xmm4r4(l1,l2,a3,O,inf)
             a4         = alpha(i+4)
             ratio(i+4) = circ_dispers_diam_xmm4r4(l1,l2,a4,O,inf)
             a5         = alpha(i+5)
             ratio(i+5) = circ_dispers_diam_xmm4r4(l1,l2,a5,O,inf)
             a6         = alpha(i+6)
             ratio(i+6) = circ_dispers_diam_xmm4r4(l1,l2,a6,O,inf)
             a7         = alpha(i+7)
             ratio(i+7) = circ_dispers_diam_xmm4r4(l1,l2,a7,O,inf)
         end do
         !$omp end parallel do
     end subroutine circ_dispers_diam_unroll_8x_omp_xmm4r4



     subroutine circ_dispers_diam_unroll_4x_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_4x_xmm4r4
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_4x_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_4x_xmm4r4
         type(XMM4r4_t),                     intent(in) :: l1
         type(XMM4r4_t),                     intent(in) :: l2
         type(XMM4r4_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM4r4_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM4r4_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM4r4_t), automatic :: a0,a1,a2,a3
         !dir$ attributes align : 16:: a0,a1,a2,a3
         integer(kind=i4) :: i,m,m1
         m = mod(n,4)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,4
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm4r4(l1,l2,a1,O,inf)
             a2         = alpha(i+2)
             ratio(i+2) = circ_dispers_diam_xmm4r4(l1,l2,a2,O,inf)
             a3         = alpha(i+3)
             ratio(i+3) = circ_dispers_diam_xmm4r4(l1,l2,a3,O,inf)
         end do
      end subroutine circ_dispers_diam_unroll_4x_xmm4r4


      subroutine circ_dispers_diam_unroll_4x_omp_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_4x_omp_xmm4r4
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_4x_omp_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_4x_omp_xmm4r4
         type(XMM4r4_t),                     intent(in) :: l1
         type(XMM4r4_t),                     intent(in) :: l2
         type(XMM4r4_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM4r4_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM4r4_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM4r4_t), automatic :: a0,a1,a2,a3
         !dir$ attributes align : 16:: a0,a1,a2,a3
         integer(kind=i4) :: i,m,m1
         m = mod(n,4)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(a0,a1,a2,a3)      &
         !$omp shared(n,l1,l2,alpha,O,inf,ratio)
         do i=m1,n,4
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm4r4(l1,l2,a1,O,inf)
             a2         = alpha(i+2)
             ratio(i+2) = circ_dispers_diam_xmm4r4(l1,l2,a2,O,inf)
             a3         = alpha(i+3)
             ratio(i+3) = circ_dispers_diam_xmm4r4(l1,l2,a3,O,inf)
         end do
         !$omp end parallel do
     end subroutine circ_dispers_diam_unroll_4x_omp_xmm4r4



     subroutine circ_dispers_diam_unroll_2x_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_2x_xmm4r4
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_2x_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_2x_xmm4r4
         type(XMM4r4_t),                     intent(in) :: l1
         type(XMM4r4_t),                     intent(in) :: l2
         type(XMM4r4_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM4r4_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM4r4_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM4r4_t), automatic :: a0,a1
         !dir$ attributes align : 16:: a0,a1
         integer(kind=i4) :: i,m,m1
         m = mod(n,2)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,2
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm4r4(l1,l2,a1,O,inf)
         end do
      end subroutine circ_dispers_diam_unroll_2x_xmm4r4


      subroutine circ_dispers_diam_unroll_2x_omp_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_2x_omp_xmm4r4
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_2x_omp_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_2x_omp_xmm4r4
         type(XMM4r4_t),                     intent(in) :: l1
         type(XMM4r4_t),                     intent(in) :: l2
         type(XMM4r4_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM4r4_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM4r4_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM4r4_t), automatic :: a0,a1
         !dir$ attributes align : 16:: a0,a1
         integer(kind=i4) :: i,m,m1
         m = mod(n,2)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
            end do
            if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(a0,a1)      &
         !$omp shared(n,l1,l2,alpha,O,inf,ratio)
         do i=m1,n,2
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm4r4(l1,l2,a1,O,inf)
         end do
         !$omp end parallel do
     end subroutine circ_dispers_diam_unroll_2x_omp_xmm4r4



     subroutine circ_dispers_diam_rolled_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_rolled_xmm4r4
           !dir$ attributes forceinline :: circ_dispers_diam_rolled_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_rolled_xmm4r4
         type(XMM4r4_t),                     intent(in) :: l1
         type(XMM4r4_t),                     intent(in) :: l2
         type(XMM4r4_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM4r4_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM4r4_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM4r4_t), automatic :: a0
         !dir$ attributes align : 16:: a0
         integer(kind=i4) :: i
        
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=1,n
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
         end do
      end subroutine circ_dispers_diam_rolled_xmm4r4


      subroutine circ_dispers_diam_rolled_omp_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_rolled_omp_xmm4r4
           !dir$ attributes forceinline :: circ_dispers_diam_rolled_omp_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_rolled_omp_xmm4r4
         type(XMM4r4_t),                     intent(in) :: l1
         type(XMM4r4_t),                     intent(in) :: l2
         type(XMM4r4_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM4r4_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM4r4_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM4r4_t), automatic :: a0
         !dir$ attributes align : 16:: a0
         integer(kind=i4) :: i
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp private(a0)      &
         !$omp shared(n,l1,l2,alpha,O,inf,ratio)
         do i=1,n
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm4r4(l1,l2,a0,O,inf)
         end do
         !$omp end parallel do
     end subroutine circ_dispers_diam_rolled_omp_xmm4r4


     subroutine circ_dispers_diam_dispatch_xmm4r4(l1,l2,alpha,O,inf,ratio,n,unroll_cnt,omp_ver) 
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: circ_dispers_diam_dispatch_xmm4r4 
         type(XMM4r4_t),                     intent(in) :: l1
         type(XMM4r4_t),                     intent(in) :: l2
         type(XMM4r4_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM4r4_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM4r4_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         integer(kind=i4),                    intent(in) :: unroll_cnt
         logical(kind=i4),                    intent(in) :: omp_ver
         if(omp_ver) then
            select case (unroll_cnt)
                case (16)
                   call circ_dispers_diam_unroll_16x_omp_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
                case (8)
                   call circ_dispers_diam_unroll_8x_omp_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
                case (4)
                   call circ_dispers_diam_unroll_4x_omp_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
                case (2)
                   call circ_dispers_diam_unroll_2x_omp_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
                case (0)
                   call circ_dispers_diam_rolled_omp_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
                case default
                   return
             end select
          else
             select case (unroll_cnt)
                case (16)
                   call circ_dispers_diam_unroll_16x_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
                case (8)
                   call circ_dispers_diam_unroll_8x_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
                case (4)
                   call circ_dispers_diam_unroll_4x_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
                case (2)
                   call circ_dispers_diam_unroll_2x_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
                case (0)
                   call circ_dispers_diam_rolled_xmm4r4(l1,l2,alpha,O,inf,ratio,n)
                case default
                   return
             end select
     end subroutine  circ_dispers_diam_dispatch_xmm4r4 
     
     
     
    pure function circ_dispers_diam_xmm2r8(l1,l2,alpha,O,inf) result(ratio)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: circ_dispers_diam_xmm2r8
        !dir$ attributes forceinline :: circ_dispers_diam_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_xmm2r8
        type(XMM2r8_t),     intent(in) :: l1
        type(XMM2r8_t),     intent(in) :: l2
        type(XMM2r8_t),     intent(in) :: alpha
        type(XMM2r8_t),     intent(in) :: O
        logical(kind=i4),   intent(in) :: inf
        type(XMM2r8_t) :: ratio
        type(XMM2r8_t), automatic :: t0,t1
        !dir$ attributes align : 16:: t0,t1
        t0    = l1.v+l2.v
        t1    = defocus_cof_xmm2r8(l2,alpha,O,inf)
        ratio = t1.v/t0.v
     end function circ_dispers_diam_xmm2r8


     
     subroutine circ_dispers_diam_unroll_16x_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_16x_xmm2r8
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_16x_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_16x_xmm2r8
         type(XMM2r8_t),                     intent(in) :: l1
         type(XMM2r8_t),                     intent(in) :: l2
         type(XMM2r8_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM2r8_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM2r8_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
         type(XMM2r8_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
         !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
         !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
         integer(kind=i4) :: i,m,m1
         m = mod(n,16)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
            end do
            if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,16
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm2r8(l1,l2,a1,O,inf)
             a2         = alpha(i+2)
             ratio(i+2) = circ_dispers_diam_xmm2r8(l1,l2,a2,O,inf)
             a3         = alpha(i+3)
             ratio(i+3) = circ_dispers_diam_xmm2r8(l1,l2,a3,O,inf)
             a4         = alpha(i+4)
             ratio(i+4) = circ_dispers_diam_xmm2r8(l1,l2,a4,O,inf)
             a5         = alpha(i+5)
             ratio(i+5) = circ_dispers_diam_xmm2r8(l1,l2,a5,O,inf)
             a6         = alpha(i+6)
             ratio(i+6) = circ_dispers_diam_xmm2r8(l1,l2,a6,O,inf)
             a7         = alpha(i+7)
             ratio(i+7) = circ_dispers_diam_xmm2r8(l1,l2,a7,O,inf)
             a8         = alpha(i+8)
             ratio(i+8) = circ_dispers_diam_xmm2r8(l1,l2,a8,O,inf)
             a9         = alpha(i+9)
             ratio(i+9) = circ_dispers_diam_xmm2r8(l1,l2,a9,O,inf)
             a10        = alpha(i+10)
             ratio(i+10)= circ_dispers_diam_xmm2r8(l1,l2,a10,O,inf)
             a11        = alpha(i+11)
             ratio(i+1) = circ_dispers_diam_xmm2r8(l1,l2,a11,O,inf)
             a12        = alpha(i+12)
             ratio(i+12)= circ_dispers_diam_xmm2r8(l1,l2,a12,O,inf) 
             a13        = alpha(i+13)
             ratio(i+13)= circ_dispers_diam_xmm2r8(l1,l2,a13,O,inf)
             a14        = alpha(i+14)
             ratio(i+14)= circ_dispers_diam_xmm2r8(l1,l2,a14,O,inf)
             a15        = alpha(i+15)
             ratio(i+15)= circ_dispers_diam_xmm2r8(l1,l2,a15,O,inf)
         end do

      end subroutine circ_dispers_diam_unroll_16x_xmm2r8


      subroutine circ_dispers_diam_unroll_16x_omp_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_16x_omp_xmm2r8
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_16x_omp_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_16x_omp_xmm2r8
         type(XMM2r8_t),                     intent(in) :: l1
         type(XMM2r8_t),                     intent(in) :: l2
         type(XMM2r8_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM2r8_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM2r8_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
         type(XMM2r8_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
         !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
         !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
         integer(kind=i4) :: i,m,m1
         m = mod(n,16)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
            end do
            if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)      &
         !$omp private(a8,a9,a10,a11,a12,a13,a14,a15)                 &
         !$omp shared(n,l1,l2,alpha,O,inf,ratio)
         do i=m1,n,16
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm2r8(l1,l2,a1,O,inf)
             a2         = alpha(i+2)
             ratio(i+2) = circ_dispers_diam_xmm2r8(l1,l2,a2,O,inf)
             a3         = alpha(i+3)
             ratio(i+3) = circ_dispers_diam_xmm2r8(l1,l2,a3,O,inf)
             a4         = alpha(i+4)
             ratio(i+4) = circ_dispers_diam_xmm2r8(l1,l2,a4,O,inf)
             a5         = alpha(i+5)
             ratio(i+5) = circ_dispers_diam_xmm2r8(l1,l2,a5,O,inf)
             a6         = alpha(i+6)
             ratio(i+6) = circ_dispers_diam_xmm2r8(l1,l2,a6,O,inf)
             a7         = alpha(i+7)
             ratio(i+7) = circ_dispers_diam_xmm2r8(l1,l2,a7,O,inf)
             a8         = alpha(i+8)
             ratio(i+8) = circ_dispers_diam_xmm2r8(l1,l2,a8,O,inf)
             a9         = alpha(i+9)
             ratio(i+9) = circ_dispers_diam_xmm2r8(l1,l2,a9,O,inf)
             a10        = alpha(i+10)
             ratio(i+10)= circ_dispers_diam_xmm2r8(l1,l2,a10,O,inf)
             a11        = alpha(i+11)
             ratio(i+1) = circ_dispers_diam_xmm2r8(l1,l2,a11,O,inf)
             a12        = alpha(i+12)
             ratio(i+12)= circ_dispers_diam_xmm2r8(l1,l2,a12,O,inf) 
             a13        = alpha(i+13)
             ratio(i+13)= circ_dispers_diam_xmm2r8(l1,l2,a13,O,inf)
             a14        = alpha(i+14)
             ratio(i+14)= circ_dispers_diam_xmm2r8(l1,l2,a14,O,inf)
             a15        = alpha(i+15)
             ratio(i+15)= circ_dispers_diam_xmm2r8(l1,l2,a15,O,inf)
         end do
         !$omp end parallel do
     end subroutine circ_dispers_diam_unroll_16x_omp_xmm2r8

     
   
    subroutine circ_dispers_diam_unroll_8x_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_8x_xmm2r8
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_8x_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_8x_xmm2r8
         type(XMM2r8_t),                     intent(in) :: l1
         type(XMM2r8_t),                     intent(in) :: l2
         type(XMM2r8_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM2r8_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM2r8_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
         !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
         integer(kind=i4) :: i,m,m1
         m = mod(n,8)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
            end do
            if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,8
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm2r8(l1,l2,a1,O,inf)
             a2         = alpha(i+2)
             ratio(i+2) = circ_dispers_diam_xmm2r8(l1,l2,a2,O,inf)
             a3         = alpha(i+3)
             ratio(i+3) = circ_dispers_diam_xmm2r8(l1,l2,a3,O,inf)
             a4         = alpha(i+4)
             ratio(i+4) = circ_dispers_diam_xmm2r8(l1,l2,a4,O,inf)
             a5         = alpha(i+5)
             ratio(i+5) = circ_dispers_diam_xmm2r8(l1,l2,a5,O,inf)
             a6         = alpha(i+6)
             ratio(i+6) = circ_dispers_diam_xmm2r8(l1,l2,a6,O,inf)
             a7         = alpha(i+7)
             ratio(i+7) = circ_dispers_diam_xmm2r8(l1,l2,a7,O,inf)
         end do

      end subroutine circ_dispers_diam_unroll_8x_xmm2r8


      subroutine circ_dispers_diam_unroll_8x_omp_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_8x_omp_xmm2r8
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_8x_omp_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_8x_omp_xmm2r8
         type(XMM2r8_t),                     intent(in) :: l1
         type(XMM2r8_t),                     intent(in) :: l2
         type(XMM2r8_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM2r8_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM2r8_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
         !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
         integer(kind=i4) :: i,m,m1
         m = mod(n,8)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
            end do
            if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)      &
         !$omp shared(n,l1,l2,alpha,O,inf,ratio)
         do i=m1,n,8
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm2r8(l1,l2,a1,O,inf)
             a2         = alpha(i+2)
             ratio(i+2) = circ_dispers_diam_xmm2r8(l1,l2,a2,O,inf)
             a3         = alpha(i+3)
             ratio(i+3) = circ_dispers_diam_xmm2r8(l1,l2,a3,O,inf)
             a4         = alpha(i+4)
             ratio(i+4) = circ_dispers_diam_xmm2r8(l1,l2,a4,O,inf)
             a5         = alpha(i+5)
             ratio(i+5) = circ_dispers_diam_xmm2r8(l1,l2,a5,O,inf)
             a6         = alpha(i+6)
             ratio(i+6) = circ_dispers_diam_xmm2r8(l1,l2,a6,O,inf)
             a7         = alpha(i+7)
             ratio(i+7) = circ_dispers_diam_xmm2r8(l1,l2,a7,O,inf)
         end do
         !$omp end parallel do
     end subroutine circ_dispers_diam_unroll_8x_omp_xmm2r8



     subroutine circ_dispers_diam_unroll_4x_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_4x_xmm2r8
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_4x_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_4x_xmm2r8
         type(XMM2r8_t),                     intent(in) :: l1
         type(XMM2r8_t),                     intent(in) :: l2
         type(XMM2r8_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM2r8_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM2r8_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM2r8_t), automatic :: a0,a1,a2,a3
         !dir$ attributes align : 16:: a0,a1,a2,a3
         integer(kind=i4) :: i,m,m1
         m = mod(n,4)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,4
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm2r8(l1,l2,a1,O,inf)
             a2         = alpha(i+2)
             ratio(i+2) = circ_dispers_diam_xmm2r8(l1,l2,a2,O,inf)
             a3         = alpha(i+3)
             ratio(i+3) = circ_dispers_diam_xmm2r8(l1,l2,a3,O,inf)
         end do
      end subroutine circ_dispers_diam_unroll_4x_xmm2r8


      subroutine circ_dispers_diam_unroll_4x_omp_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_4x_omp_xmm2r8
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_4x_omp_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_4x_omp_xmm2r8
         type(XMM2r8_t),                     intent(in) :: l1
         type(XMM2r8_t),                     intent(in) :: l2
         type(XMM2r8_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM2r8_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM2r8_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM2r8_t), automatic :: a0,a1,a2,a3
         !dir$ attributes align : 16:: a0,a1,a2,a3
         integer(kind=i4) :: i,m,m1
         m = mod(n,4)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(a0,a1,a2,a3)      &
         !$omp shared(n,l1,l2,alpha,O,inf,ratio)
         do i=m1,n,4
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm2r8(l1,l2,a1,O,inf)
             a2         = alpha(i+2)
             ratio(i+2) = circ_dispers_diam_xmm2r8(l1,l2,a2,O,inf)
             a3         = alpha(i+3)
             ratio(i+3) = circ_dispers_diam_xmm2r8(l1,l2,a3,O,inf)
         end do
         !$omp end parallel do
     end subroutine circ_dispers_diam_unroll_4x_omp_xmm2r8



     subroutine circ_dispers_diam_unroll_2x_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_2x_xmm2r8
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_2x_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_2x_xmm2r8
         type(XMM2r8_t),                     intent(in) :: l1
         type(XMM2r8_t),                     intent(in) :: l2
         type(XMM2r8_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM2r8_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM2r8_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM2r8_t), automatic :: a0,a1
         !dir$ attributes align : 16:: a0,a1
         integer(kind=i4) :: i,m,m1
         m = mod(n,2)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,2
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm2r8(l1,l2,a1,O,inf)
         end do
      end subroutine circ_dispers_diam_unroll_2x_xmm2r8


      subroutine circ_dispers_diam_unroll_2x_omp_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_unroll_2x_omp_xmm2r8
           !dir$ attributes forceinline :: circ_dispers_diam_unroll_2x_omp_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_unroll_2x_omp_xmm2r8
         type(XMM2r8_t),                     intent(in) :: l1
         type(XMM2r8_t),                     intent(in) :: l2
         type(XMM2r8_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM2r8_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM2r8_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM2r8_t), automatic :: a0,a1
         !dir$ attributes align : 16:: a0,a1
         integer(kind=i4) :: i,m,m1
         m = mod(n,2)
         if(m /= 0) then
            do i=1,m
               a0   = alpha(i)
               ratio(i) = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
            end do
            if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(a0,a1)      &
         !$omp shared(n,l1,l2,alpha,O,inf,ratio)
         do i=m1,n,2
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
             a1         = alpha(i+1)
             ratio(i+1) = circ_dispers_diam_xmm2r8(l1,l2,a1,O,inf)
         end do
         !$omp end parallel do
     end subroutine circ_dispers_diam_unroll_2x_omp_xmm2r8



     subroutine circ_dispers_diam_rolled_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_rolled_xmm2r8
           !dir$ attributes forceinline :: circ_dispers_diam_rolled_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_rolled_xmm2r8
         type(XMM2r8_t),                     intent(in) :: l1
         type(XMM2r8_t),                     intent(in) :: l2
         type(XMM2r8_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM2r8_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM2r8_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM2r8_t), automatic :: a0
         !dir$ attributes align : 16:: a0
         integer(kind=i4) :: i
        
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=1,n
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
         end do
      end subroutine circ_dispers_diam_rolled_xmm2r8


      subroutine circ_dispers_diam_rolled_omp_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: circ_dispers_diam_rolled_omp_xmm2r8
           !dir$ attributes forceinline :: circ_dispers_diam_rolled_omp_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: circ_dispers_diam_rolled_omp_xmm2r8
         type(XMM2r8_t),                     intent(in) :: l1
         type(XMM2r8_t),                     intent(in) :: l2
         type(XMM2r8_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM2r8_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM2r8_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         type(XMM2r8_t), automatic :: a0
         !dir$ attributes align : 16:: a0
         integer(kind=i4) :: i
           !dir$ assume_aligned alpha:16
           !dir$ assume_aligned ratio:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp private(a0)      &
         !$omp shared(n,l1,l2,alpha,O,inf,ratio)
         do i=1,n
             a0         = alpha(i)
             ratio(i)   = circ_dispers_diam_xmm2r8(l1,l2,a0,O,inf)
         end do
         !$omp end parallel do
     end subroutine circ_dispers_diam_rolled_omp_xmm2r8


     subroutine circ_dispers_diam_dispatch_xmm2r8(l1,l2,alpha,O,inf,ratio,n,unroll_cnt,omp_ver) 
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: circ_dispers_diam_dispatch_xmm2r8 
         type(XMM2r8_t),                     intent(in) :: l1
         type(XMM2r8_t),                     intent(in) :: l2
         type(XMM2r8_t),    dimension(1:n),  intent(in) :: alpha
         type(XMM2r8_t),                     intent(in) :: O
         logical(kind=i4),                    intent(in) :: inf
         type(XMM4r4_t),    dimension(1:n),  intent(out):: ratio
         integer(kind=i4),                    intent(in) :: n
         integer(kind=i4),                    intent(in) :: unroll_cnt
         logical(kind=i4),                    intent(in) :: omp_ver
         if(omp_ver) then
            select case (unroll_cnt)
                case (16)
                   call circ_dispers_diam_unroll_16x_omp_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
                case (8)
                   call circ_dispers_diam_unroll_8x_omp_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
                case (4)
                   call circ_dispers_diam_unroll_4x_omp_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
                case (2)
                   call circ_dispers_diam_unroll_2x_omp_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
                case (0)
                   call circ_dispers_diam_rolled_omp_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
                case default
                   return
             end select
          else
             select case (unroll_cnt)
                case (16)
                   call circ_dispers_diam_unroll_16x_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
                case (8)
                   call circ_dispers_diam_unroll_8x_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
                case (4)
                   call circ_dispers_diam_unroll_4x_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
                case (2)
                   call circ_dispers_diam_unroll_2x_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
                case (0)
                   call circ_dispers_diam_rolled_xmm2r8(l1,l2,alpha,O,inf,ratio,n)
                case default
                   return
             end select
     end subroutine  circ_dispers_diam_dispatch_xmm2r8 

 
     !/////////////////////////////////////////////////////////////////////////////////////////////!
     !////////////////////////////////////////////////////////////////////////////////////////////!
     
     
       ! СКАНИРОВАНИЕ ЗЕРКАЛОМ, ВРАЩАЮЩИМСЯ
      ! ВОКРУГ ОСИ, НЕПЕРПЕНДИКУЛЯРНОЙ К НЕМУ
      ! Formula 1, p. 100

       


     
    pure function fov_x_axis_xmm4r4(H,delta,gamma) result(ax)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_x_axis_xmm4r4
         !dir$ attributes forceinline :: fov_x_axis_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_xmm4r4
         type(XMM4r4_t),   intent(in) :: H
         type(XMM4r4_t),   intent(in) :: delta
         type(XMM4r4_t),   intent(in) :: gamma
         type(XMM4r4_t) :: ax
         type(XMM4r4_t), parameter :: half = XMM4r4_t(0.5_sp)
         type(XMM4r4_t), automatic :: gamm2,tdel
         !dir$ attributes align : 16:: gamm2,tdel
         gamm2 = half.v*gamma.v
         tdel  = tan(delta.v)
         ax    = H.v*tdel.v/cos(gamm2.v)
    end function fov_x_axis_xmm4r4


    
    subroutine fov_x_axis_unroll_16x_xmm4r4(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_16x_xmm4r4
           !dir$ attributes forceinline :: fov_x_axis_unroll_16x_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_16x_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM4r4_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM4r4_t), automatic :: d8,d9,d10,d11,d12,d13,d14,d15
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d8,d9,d10,d11,d12,d13,d14,d15
         type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         type(XMM4r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
         integer(kind=i4) :: i,m,m1
         m = mod(n,16)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm4r4(H,d0,g0)
            end do
            if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:32
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,16
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm4r4(H,d1,g1)
            d2      = delta(i+2)
            g2      = gamma(i+2)
            ax(i+2) = fov_x_axis_xmm4r4(H,d2,g2)
            d3      = delta(i+3)
            g3      = gamma(i+3)
            ax(i+3) = fov_x_axis_xmm4r4(H,d3,g3)
            d4      = delta(i+4)
            g4      = gamma(i+4)
            ax(i+4) = fov_x_axis_xmm4r4(H,d4,g4)
            d5      = delta(i+5)
            g5      = gamma(i+5)
            ax(i+5) = fov_x_axis_xmm4r4(H,d5,g5)
            d6      = delta(i+6)
            g6      = gamma(i+6)
            ax(i+6) = fov_x_axis_xmm4r4(H,d6,g6)
            d7      = delta(i+7)
            g7      = gamma(i+7)
            ax(i+7) = fov_x_axis_xmm4r4(H,d7,g7)
            d8      = delta(i+8)
            g8      = gamma(i+8)
            ax(i+8) = fov_x_axis_xmm4r4(H,d8,g8)
            d9      = delta(i+9)
            g9      = gamma(i+9)
            ax(i+9) = fov_x_axis_xmm4r4(H,d9,g9)
            d10     = delta(i+10)
            g10     = gamma(i+10)
            ax(i+10)= fov_x_axis_xmm4r4(H,d10,g10)
            d11     = delta(i+11)
            g11     = gamma(i+11)
            ax(i+11)= fov_x_axis_xmm4r4(H,d11,g11)
            d12     = delta(i+12)
            g12     = gamma(i+12)
            ax(i+12)= fov_x_axis_xmm4r4(H,d12,g12)
            d13     = delta(i+13)
            g13     = gamma(i+13)
            ax(i+13)= fov_x_axis_xmm4r4(H,d13,g13)
            d14     = delta(i+14)
            g14     = gamma(i+14)
            ax(i+14)= fov_x_axis_xmm4r4(H,d14,g14)
            d15     = delta(i+15)
            g15     = gamma(i+15)
            ax(i+15)= fov_x_axis_xmm4r4(H,d15,g15)
         end do
     end subroutine fov_x_axis_unroll_16x_xmm4r4


     subroutine fov_x_axis_unroll_16x_omp_xmm4r4(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_16x_omp_xmm4r4
           !dir$ attributes forceinline :: fov_x_axis_unroll_16x_omp_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_16x_omp_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM4r4_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM4r4_t), automatic :: d8,d9,d10,d11,d12,d13,d14,d15
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d8,d9,d10,d11,d12,d13,d14,d15
         type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         type(XMM4r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
         integer(kind=i4) :: i,m,m1
         m = mod(n,16)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm4r4(H,d0,g0)
            end do
            if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1,d2,d3,d4,d5,d6,d7)      &
         !$omp private(d8,d9,d10,d11,d12,d13,d14,d15)                 &
         !$omp private(g0,g1,g2,g3,g4,g,g6,g7)                        &
         !$omp private(g8,g9,g10,g11,g12,g13,g14,g15)                 &
         !$omp shared(n,H,delta,gamma,ax)
         do i=m1,n,16
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm4r4(H,d1,g1)
            d2      = delta(i+2)
            g2      = gamma(i+2)
            ax(i+2) = fov_x_axis_xmm4r4(H,d2,g2)
            d3      = delta(i+3)
            g3      = gamma(i+3)
            ax(i+3) = fov_x_axis_xmm4r4(H,d3,g3)
            d4      = delta(i+4)
            g4      = gamma(i+4)
            ax(i+4) = fov_x_axis_xmm4r4(H,d4,g4)
            d5      = delta(i+5)
            g5      = gamma(i+5)
            ax(i+5) = fov_x_axis_xmm4r4(H,d5,g5)
            d6      = delta(i+6)
            g6      = gamma(i+6)
            ax(i+6) = fov_x_axis_xmm4r4(H,d6,g6)
            d7      = delta(i+7)
            g7      = gamma(i+7)
            ax(i+7) = fov_x_axis_xmm4r4(H,d7,g7)
            d8      = delta(i+8)
            g8      = gamma(i+8)
            ax(i+8) = fov_x_axis_xmm4r4(H,d8,g8)
            d9      = delta(i+9)
            g9      = gamma(i+9)
            ax(i+9) = fov_x_axis_xmm4r4(H,d9,g9)
            d10     = delta(i+10)
            g10     = gamma(i+10)
            ax(i+10)= fov_x_axis_xmm4r4(H,d10,g10)
            d11     = delta(i+11)
            g11     = gamma(i+11)
            ax(i+11)= fov_x_axis_xmm4r4(H,d11,g11)
            d12     = delta(i+12)
            g12     = gamma(i+12)
            ax(i+12)= fov_x_axis_xmm4r4(H,d12,g12)
            d13     = delta(i+13)
            g13     = gamma(i+13)
            ax(i+13)= fov_x_axis_xmm4r4(H,d13,g13)
            d14     = delta(i+14)
            g14     = gamma(i+14)
            ax(i+14)= fov_x_axis_xmm4r4(H,d14,g14)
            d15     = delta(i+15)
            g15     = gamma(i+15)
            ax(i+15)= fov_x_axis_xmm4r4(H,d15,g15)
         end do
         !$omp end parallel do
     end subroutine fov_x_axis_unroll_16x_omp_xmm4r4


     subroutine fov_x_axis_unroll_8x_xmm4r4(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_8x_xmm4r4
           !dir$ attributes forceinline :: fov_x_axis_unroll_8x_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_8x_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM4r4_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         integer(kind=i4) :: i,m,m1
         m = mod(n,8)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm4r4(H,d0,g0)
            end do
            if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,8
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm4r4(H,d1,g1)
            d2      = delta(i+2)
            g2      = gamma(i+2)
            ax(i+2) = fov_x_axis_xmm4r4(H,d2,g2)
            d3      = delta(i+3)
            g3      = gamma(i+3)
            ax(i+3) = fov_x_axis_xmm4r4(H,d3,g3)
            d4      = delta(i+4)
            g4      = gamma(i+4)
            ax(i+4) = fov_x_axis_xmm4r4(H,d4,g4)
            d5      = delta(i+5)
            g5      = gamma(i+5)
            ax(i+5) = fov_x_axis_xmm4r4(H,d5,g5)
            d6      = delta(i+6)
            g6      = gamma(i+6)
            ax(i+6) = fov_x_axis_xmm4r4(H,d6,g6)
            d7      = delta(i+7)
            g7      = gamma(i+7)
            ax(i+7) = fov_x_axis_xmm4r4(H,d7,g7)
         end do
     end subroutine fov_x_axis_unroll_8x_xmm4r4


     subroutine fov_x_axis_unroll_8x_omp_xmm4r4(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_8x_omp_xmm4r4
           !dir$ attributes forceinline :: fov_x_axis_unroll_8x_omp_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_8x_omp_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM4r4_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         integer(kind=i4) :: i,m,m1
         m = mod(n,8)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm4r4(H,d0,g0)
            end do
            if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1,d2,d3,d4,d5,d6,d7)      &
         !$omp private(g0,g1,g2,g3,g4,g,g6,g7)                        &
         !$omp shared(n,H,delta,gamma,ax)
         do i=m1,n,8
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm4r4(H,d1,g1)
            d2      = delta(i+2)
            g2      = gamma(i+2)
            ax(i+2) = fov_x_axis_xmm4r4(H,d2,g2)
            d3      = delta(i+3)
            g3      = gamma(i+3)
            ax(i+3) = fov_x_axis_xmm4r4(H,d3,g3)
            d4      = delta(i+4)
            g4      = gamma(i+4)
            ax(i+4) = fov_x_axis_xmm4r4(H,d4,g4)
            d5      = delta(i+5)
            g5      = gamma(i+5)
            ax(i+5) = fov_x_axis_xmm4r4(H,d5,g5)
            d6      = delta(i+6)
            g6      = gamma(i+6)
            ax(i+6) = fov_x_axis_xmm4r4(H,d6,g6)
            d7      = delta(i+7)
            g7      = gamma(i+7)
            ax(i+7) = fov_x_axis_xmm4r4(H,d7,g7)
         end do
         !$omp end parallel do
     end subroutine fov_x_axis_unroll_8x_omp_xmm4r4


     subroutine fov_x_axis_unroll_4x_xmm4r4(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_4x_xmm4r4
           !dir$ attributes forceinline :: fov_x_axis_unroll_4x_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_4x_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM4r4_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3
         !dir$ attributes align : 16:: d0,d1,d2,d3
         type(XMM4r4_t), automatic :: g0,g1,g2,g3
         !dir$ attributes align : 16:: g0,g1,g2,g3
         integer(kind=i4) :: i,m,m1
         m = mod(n,4)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm4r4(H,d0,g0)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,4
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm4r4(H,d1,g1)
            d2      = delta(i+2)
            g2      = gamma(i+2)
            ax(i+2) = fov_x_axis_xmm4r4(H,d2,g2)
            d3      = delta(i+3)
            g3      = gamma(i+3)
            ax(i+3) = fov_x_axis_xmm4r4(H,d3,g3)
         end do
     end subroutine fov_x_axis_unroll_4x_xmm4r4


     subroutine fov_x_axis_unroll_4x_omp_xmm4r4(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_4x_omp_xmm4r4
           !dir$ attributes forceinline :: fov_x_axis_unroll_4x_omp_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_4x_omp_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM4r4_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3
         !dir$ attributes align : 16:: d0,d1,d2,d3
         type(XMM4r4_t), automatic :: g0,g1,g2,g3
         !dir$ attributes align : 16:: g0,g1,g2,g3
         integer(kind=i4) :: i,m,m1
         m = mod(n,4)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm4r4(H,d0,g0)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1,d2,d3)      &
         !$omp private(g0,g1,g2,g3)                        &
         !$omp shared(n,H,delta,gamma,ax)
         do i=m1,n,4
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm4r4(H,d1,g1)
            d2      = delta(i+2)
            g2      = gamma(i+2)
            ax(i+2) = fov_x_axis_xmm4r4(H,d2,g2)
            d3      = delta(i+3)
            g3      = gamma(i+3)
            ax(i+3) = fov_x_axis_xmm4r4(H,d3,g3)
         end do
         !$omp end parallel do
     end subroutine fov_x_axis_unroll_4x_omp_xmm4r4


    subroutine fov_x_axis_unroll_2x_xmm4r4(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_2x_xmm4r4
           !dir$ attributes forceinline :: fov_x_axis_unroll_2x_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_2x_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM4r4_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1
         !dir$ attributes align : 16:: d0,d1
         type(XMM4r4_t), automatic :: g0,g1
         !dir$ attributes align : 16:: g0,g1
         integer(kind=i4) :: i,m,m1
         m = mod(n,2)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm4r4(H,d0,g0)
            end do
            if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,2
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm4r4(H,d1,g1)
         end do
     end subroutine fov_x_axis_unroll_2x_xmm4r4


     subroutine fov_x_axis_unroll_2x_omp_xmm4r4(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_2x_omp_xmm4r4
           !dir$ attributes forceinline :: fov_x_axis_unroll_2x_omp_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_2x_omp_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM4r4_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1
         !dir$ attributes align : 16:: d0,d1
         type(XMM4r4_t), automatic :: g0,g1
         !dir$ attributes align : 16:: g0,g1
         integer(kind=i4) :: i,m,m1
         m = mod(n,2)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm4r4(H,d0,g0)
            end do
            if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1)      &
         !$omp private(g0,g1)                        &
         !$omp shared(n,H,delta,gamma,ax)
         do i=m1,n,2
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm4r4(H,d1,g1)
         end do
         !$omp end parallel do
     end subroutine fov_x_axis_unroll_2x_omp_xmm4r4

     

     subroutine fov_x_axis_rolled_xmm4r4(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_rolled_xmm4r4
           !dir$ attributes forceinline :: fov_x_axis_rolled_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_rolled_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM4r4_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0
         !dir$ attributes align : 16:: d0
         type(XMM4r4_t), automatic :: g0
         !dir$ attributes align : 16:: g0
         integer(kind=i4) :: i
         
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=1,n
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm4r4(H,d0,g0)
         end do
     end subroutine fov_x_axis_rolled_xmm4r4


     subroutine fov_x_axis_rolled_omp_xmm4r4(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_rolled_omp_xmm4r4
           !dir$ attributes forceinline :: fov_x_axis_rolled_omp_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_rolled_omp_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM4r4_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0
         !dir$ attributes align : 16:: d0
         type(XMM4r4_t), automatic :: g0
         !dir$ attributes align : 16:: g0
         integer(kind=i4) :: i
       
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp  private(d0)      &
         !$omp private(g0)                        &
         !$omp shared(n,H,delta,gamma,ax)
         do i=1,n
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm4r4(H,d0,g0)
         end do
         !$omp end parallel do
     end subroutine fov_x_axis_rolled_omp_xmm4r4


     subroutine fov_x_axis_dispatch_xmm4r4(H,delta,gamma,ax,n,unroll_cnt,omp_ver)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_dispatch_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM4r4_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         integer(kind=i4),                 intent(in)  :: unroll_cnt
         logical(kind=i4),                 intent(in)  :: omp_ver
         if(omp_ver) then
            select case (unroll_cnt)
                 case (16)
                     call fov_x_axis_unroll_16x_omp_xmm4r4(H,delta,gamma,ax,n)
                 case (8)
                     call fov_x_axis_unroll_8x_omp_xmm4r4(H,delta,gamma,ax,n)
                 case (4)
                     call fov_x_axis_unroll_4x_omp_xmm4r4(H,delta,gamma,ax,n)
                 case (2)
                     call fov_x_axis_unroll_2x_omp_xmm4r4(H,delta,gamma,ax,n)
                 case (0)
                     call fov_x_axis_rolled_omp_xmm4r4(H,delta,gamma,ax,n)
                 case default
                      return
             end select
         else
              select case (unroll_cnt)
                 case (16)
                     call fov_x_axis_unroll_16x_xmm4r4(H,delta,gamma,ax,n)
                 case (8)
                     call fov_x_axis_unroll_8x_xmm4r4(H,delta,gamma,ax,n)
                 case (4)
                     call fov_x_axis_unroll_4x_xmm4r4(H,delta,gamma,ax,n)
                 case (2)
                     call fov_x_axis_unroll_2x_xmm4r4(H,delta,gamma,ax,n)
                 case (0)
                     call fov_x_axis_rolled_xmm4r4(H,delta,gamma,ax,n)
                 case default
                      return
             end select
         end if
     end subroutine fov_x_axis_dispatch_xmm4r4
     
     
     pure function fov_x_axis_xmm2r8(H,delta,gamma) result(ax)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: fov_x_axis_xmm2r8
         !dir$ attributes forceinline :: fov_x_axis_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_xmm2r8
         type(XMM2r8_t),   intent(in) :: H
         type(XMM2r8_t),   intent(in) :: delta
         type(XMM2r8_t),   intent(in) :: gamma
         type(XMM2r8_t) :: ax
         type(XMM2r8_t), parameter :: half = XMM2r8_t(0.5_dp)
         type(XMM2r8_t), automatic :: gamm2,tdel
         !dir$ attributes align : 16:: gamm2,tdel
         gamm2 = half.v*gamma.v
         tdel  = tan(delta.v)
         ax    = H.v*tdel.v/cos(gamm2.v)
      end function fov_x_axis_xmm2r8


      subroutine fov_x_axis_unroll_16x_xmm2r8(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_16x_xmm2r8
           !dir$ attributes forceinline :: fov_x_axis_unroll_16x_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_16x_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM2r8_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM2r8_t), automatic :: d8,d9,d10,d11,d12,d13,d14,d15
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d8,d9,d10,d11,d12,d13,d14,d15
         type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         type(XMM2r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
         integer(kind=i4) :: i,m,m1
         m = mod(n,16)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm2r8(H,d0,g0)
            end do
            if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,16
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm2r8(H,d1,g1)
            d2      = delta(i+2)
            g2      = gamma(i+2)
            ax(i+2) = fov_x_axis_xmm2r8(H,d2,g2)
            d3      = delta(i+3)
            g3      = gamma(i+3)
            ax(i+3) = fov_x_axis_xmm2r8(H,d3,g3)
            d4      = delta(i+4)
            g4      = gamma(i+4)
            ax(i+4) = fov_x_axis_xmm2r8(H,d4,g4)
            d5      = delta(i+5)
            g5      = gamma(i+5)
            ax(i+5) = fov_x_axis_xmm2r8(H,d5,g5)
            d6      = delta(i+6)
            g6      = gamma(i+6)
            ax(i+6) = fov_x_axis_xmm2r8(H,d6,g6)
            d7      = delta(i+7)
            g7      = gamma(i+7)
            ax(i+7) = fov_x_axis_xmm2r8(H,d7,g7)
            d8      = delta(i+8)
            g8      = gamma(i+8)
            ax(i+8) = fov_x_axis_xmm2r8(H,d8,g8)
            d9      = delta(i+9)
            g9      = gamma(i+9)
            ax(i+9) = fov_x_axis_xmm2r8(H,d9,g9)
            d10     = delta(i+10)
            g10     = gamma(i+10)
            ax(i+10)= fov_x_axis_xmm2r8(H,d10,g10)
            d11     = delta(i+11)
            g11     = gamma(i+11)
            ax(i+11)= fov_x_axis_xmm2r8(H,d11,g11)
            d12     = delta(i+12)
            g12     = gamma(i+12)
            ax(i+12)= fov_x_axis_xmm2r8(H,d12,g12)
            d13     = delta(i+13)
            g13     = gamma(i+13)
            ax(i+13)= fov_x_axis_xmm2r8(H,d13,g13)
            d14     = delta(i+14)
            g14     = gamma(i+14)
            ax(i+14)= fov_x_axis_xmm2r8(H,d14,g14)
            d15     = delta(i+15)
            g15     = gamma(i+15)
            ax(i+15)= fov_x_axis_xmm2r8(H,d15,g15)
         end do
     end subroutine fov_x_axis_unroll_16x_xmm2r8


     subroutine fov_x_axis_unroll_16x_omp_xmm2r8(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_16x_omp_xmm2r8
           !dir$ attributes forceinline :: fov_x_axis_unroll_16x_omp_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_16x_omp_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM2r8_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM2r8_t), automatic :: d8,d9,d10,d11,d12,d13,d14,d15
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d8,d9,d10,d11,d12,d13,d14,d15
         type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         type(XMM2r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
         integer(kind=i4) :: i,m,m1
         m = mod(n,16)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm2r8(H,d0,g0)
            end do
            if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1,d2,d3,d4,d5,d6,d7)      &
         !$omp private(d8,d9,d10,d11,d12,d13,d14,d15)                 &
         !$omp private(g0,g1,g2,g3,g4,g,g6,g7)                        &
         !$omp private(g8,g9,g10,g11,g12,g13,g14,g15)                 &
         !$omp shared(n,H,delta,gamma,ax)
         do i=m1,n,16
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm2r8(H,d1,g1)
            d2      = delta(i+2)
            g2      = gamma(i+2)
            ax(i+2) = fov_x_axis_xmm2r8(H,d2,g2)
            d3      = delta(i+3)
            g3      = gamma(i+3)
            ax(i+3) = fov_x_axis_xmm2r8(H,d3,g3)
            d4      = delta(i+4)
            g4      = gamma(i+4)
            ax(i+4) = fov_x_axis_xmm2r8(H,d4,g4)
            d5      = delta(i+5)
            g5      = gamma(i+5)
            ax(i+5) = fov_x_axis_xmm2r8(H,d5,g5)
            d6      = delta(i+6)
            g6      = gamma(i+6)
            ax(i+6) = fov_x_axis_xmm2r8(H,d6,g6)
            d7      = delta(i+7)
            g7      = gamma(i+7)
            ax(i+7) = fov_x_axis_xmm2r8(H,d7,g7)
            d8      = delta(i+8)
            g8      = gamma(i+8)
            ax(i+8) = fov_x_axis_xmm2r8(H,d8,g8)
            d9      = delta(i+9)
            g9      = gamma(i+9)
            ax(i+9) = fov_x_axis_xmm2r8(H,d9,g9)
            d10     = delta(i+10)
            g10     = gamma(i+10)
            ax(i+10)= fov_x_axis_xmm2r8(H,d10,g10)
            d11     = delta(i+11)
            g11     = gamma(i+11)
            ax(i+11)= fov_x_axis_xmm2r8(H,d11,g11)
            d12     = delta(i+12)
            g12     = gamma(i+12)
            ax(i+12)= fov_x_axis_xmm2r8(H,d12,g12)
            d13     = delta(i+13)
            g13     = gamma(i+13)
            ax(i+13)= fov_x_axis_xmm2r8(H,d13,g13)
            d14     = delta(i+14)
            g14     = gamma(i+14)
            ax(i+14)= fov_x_axis_xmm2r8(H,d14,g14)
            d15     = delta(i+15)
            g15     = gamma(i+15)
            ax(i+15)= fov_x_axis_xmm2r8(H,d15,g15)
         end do
         !$omp end parallel do
     end subroutine fov_x_axis_unroll_16x_omp_xmm2r8


     subroutine fov_x_axis_unroll_8x_xmm2r8(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_8x_xmm2r8
           !dir$ attributes forceinline :: fov_x_axis_unroll_8x_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_8x_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM2r8_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         integer(kind=i4) :: i,m,m1
         m = mod(n,8)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm2r8(H,d0,g0)
            end do
            if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,8
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm2r8(H,d1,g1)
            d2      = delta(i+2)
            g2      = gamma(i+2)
            ax(i+2) = fov_x_axis_xmm2r8(H,d2,g2)
            d3      = delta(i+3)
            g3      = gamma(i+3)
            ax(i+3) = fov_x_axis_xmm2r8(H,d3,g3)
            d4      = delta(i+4)
            g4      = gamma(i+4)
            ax(i+4) = fov_x_axis_xmm2r8(H,d4,g4)
            d5      = delta(i+5)
            g5      = gamma(i+5)
            ax(i+5) = fov_x_axis_xmm2r8(H,d5,g5)
            d6      = delta(i+6)
            g6      = gamma(i+6)
            ax(i+6) = fov_x_axis_xmm2r8(H,d6,g6)
            d7      = delta(i+7)
            g7      = gamma(i+7)
            ax(i+7) = fov_x_axis_xmm2r8(H,d7,g7)
         end do
     end subroutine fov_x_axis_unroll_8x_xmm2r8


     subroutine fov_x_axis_unroll_8x_omp_xmm2r8(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_8x_omp_xmm2r8
           !dir$ attributes forceinline :: fov_x_axis_unroll_8x_omp_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_8x_omp_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM2r8_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         integer(kind=i4) :: i,m,m1
         m = mod(n,8)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm2r8(H,d0,g0)
            end do
            if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1,d2,d3,d4,d5,d6,d7)      &
         !$omp private(g0,g1,g2,g3,g4,g,g6,g7)                        &
         !$omp shared(n,H,delta,gamma,ax)
         do i=m1,n,8
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm2r8(H,d1,g1)
            d2      = delta(i+2)
            g2      = gamma(i+2)
            ax(i+2) = fov_x_axis_xmm2r8(H,d2,g2)
            d3      = delta(i+3)
            g3      = gamma(i+3)
            ax(i+3) = fov_x_axis_xmm2r8(H,d3,g3)
            d4      = delta(i+4)
            g4      = gamma(i+4)
            ax(i+4) = fov_x_axis_xmm2r8(H,d4,g4)
            d5      = delta(i+5)
            g5      = gamma(i+5)
            ax(i+5) = fov_x_axis_xmm2r8(H,d5,g5)
            d6      = delta(i+6)
            g6      = gamma(i+6)
            ax(i+6) = fov_x_axis_xmm2r8(H,d6,g6)
            d7      = delta(i+7)
            g7      = gamma(i+7)
            ax(i+7) = fov_x_axis_xmm2r8(H,d7,g7)
         end do
         !$omp end parallel do
     end subroutine fov_x_axis_unroll_8x_omp_xmm2r8


     subroutine fov_x_axis_unroll_4x_xmm2r8(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_4x_xmm2r8
           !dir$ attributes forceinline :: fov_x_axis_unroll_4x_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_4x_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM2r8_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1,d2,d3
         !dir$ attributes align : 16:: d0,d1,d2,d3
         type(XMM2r8_t), automatic :: g0,g1,g2,g3
         !dir$ attributes align : 16:: g0,g1,g2,g3
         integer(kind=i4) :: i,m,m1
         m = mod(n,4)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm2r8(H,d0,g0)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,4
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm2r8(H,d1,g1)
            d2      = delta(i+2)
            g2      = gamma(i+2)
            ax(i+2) = fov_x_axis_xmm2r8(H,d2,g2)
            d3      = delta(i+3)
            g3      = gamma(i+3)
            ax(i+3) = fov_x_axis_xmm2r8(H,d3,g3)
         end do
     end subroutine fov_x_axis_unroll_4x_xmm2r8


     subroutine fov_x_axis_unroll_4x_omp_xmm2r8(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_4x_omp_xmm2r8
           !dir$ attributes forceinline :: fov_x_axis_unroll_4x_omp_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_4x_omp_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM2r8_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1,d2,d3
         !dir$ attributes align : 16:: d0,d1,d2,d3
         type(XMM2r8_t), automatic :: g0,g1,g2,g3
         !dir$ attributes align : 16:: g0,g1,g2,g3
         integer(kind=i4) :: i,m,m1
         m = mod(n,4)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm2r8(H,d0,g0)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1,d2,d3)      &
         !$omp private(g0,g1,g2,g3)                        &
         !$omp shared(n,H,delta,gamma,ax)
         do i=m1,n,4
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm2r8(H,d1,g1)
            d2      = delta(i+2)
            g2      = gamma(i+2)
            ax(i+2) = fov_x_axis_xmm2r8(H,d2,g2)
            d3      = delta(i+3)
            g3      = gamma(i+3)
            ax(i+3) = fov_x_axis_xmm2r8(H,d3,g3)
         end do
         !$omp end parallel do
     end subroutine fov_x_axis_unroll_4x_omp_xmm2r8


    subroutine fov_x_axis_unroll_2x_xmm2r8(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_2x_xmm2r8
           !dir$ attributes forceinline :: fov_x_axis_unroll_2x_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_2x_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM2r8_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1
         !dir$ attributes align : 16:: d0,d1
         type(XMM2r8_t), automatic :: g0,g1
         !dir$ attributes align : 16:: g0,g1
         integer(kind=i4) :: i,m,m1
         m = mod(n,2)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm2r8(H,d0,g0)
            end do
            if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,2
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm2r8(H,d1,g1)
         end do
     end subroutine fov_x_axis_unroll_2x_xmm2r8


     subroutine fov_x_axis_unroll_2x_omp_xmm2r8(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_unroll_2x_omp_xmm2r8
           !dir$ attributes forceinline :: fov_x_axis_unroll_2x_omp_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_unroll_2x_omp_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM2r8_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1
         !dir$ attributes align : 16:: d0,d1
         type(XMM2r8_t), automatic :: g0,g1
         !dir$ attributes align : 16:: g0,g1
         integer(kind=i4) :: i,m,m1
         m = mod(n,2)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = gamma(i)
               ax(i) = fov_x_axis_xmm2r8(H,d0,g0)
            end do
            if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1)      &
         !$omp private(g0,g1)                        &
         !$omp shared(n,H,delta,gamma,ax)
         do i=m1,n,2
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = gamma(i+1)
            ax(i+1) = fov_x_axis_xmm2r8(H,d1,g1)
         end do
         !$omp end parallel do
     end subroutine fov_x_axis_unroll_2x_omp_xmm2r8

     

     subroutine fov_x_axis_rolled_xmm2r8(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_rolled_xmm2r8
           !dir$ attributes forceinline :: fov_x_axis_rolled_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_rolled_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM2r8_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0
         !dir$ attributes align : 16:: d0
         type(XMM2r8_t), automatic :: g0
         !dir$ attributes align : 16:: g0
         integer(kind=i4) :: i
         
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=1,n
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm2r8(H,d0,g0)
         end do
     end subroutine fov_x_axis_rolled_xmm2r8


     subroutine fov_x_axis_rolled_omp_xmm2r8(H,delta,gamma,ax,n)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_rolled_omp_xmm2r8
           !dir$ attributes forceinline :: fov_x_axis_rolled_omp_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: fov_x_axis_rolled_omp_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM2r8_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0
         !dir$ attributes align : 16:: d0
         type(XMM2r8_t), automatic :: g0
         !dir$ attributes align : 16:: g0
         integer(kind=i4) :: i
       
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned ax:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp  private(d0)      &
         !$omp private(g0)                        &
         !$omp shared(n,H,delta,gamma,ax)
         do i=1,n
            d0      = delta(i)
            g0      = gamma(i)
            ax(i)   = fov_x_axis_xmm2r8(H,d0,g0)
         end do
         !$omp end parallel do
     end subroutine fov_x_axis_rolled_omp_xmm2r8


     subroutine fov_x_axis_dispatch_xmm2r8(H,delta,gamma,ax,n,unroll_cnt,omp_ver)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: fov_x_axis_dispatch_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: gamma
         type(XMM2r8_t),  dimension(1:n), intent(out) :: ax
         integer(kind=i4),                 intent(in)  :: n
         integer(kind=i4),                 intent(in)  :: unroll_cnt
         logical(kind=i4),                 intent(in)  :: omp_ver
         if(omp_ver) then
            select case (unroll_cnt)
                 case (16)
                     call fov_x_axis_unroll_16x_omp_xmm2r8(H,delta,gamma,ax,n)
                 case (8)
                     call fov_x_axis_unroll_8x_omp_xmm2r8(H,delta,gamma,ax,n)
                 case (4)
                     call fov_x_axis_unroll_4x_omp_xmm2r8(H,delta,gamma,ax,n)
                 case (2)
                     call fov_x_axis_unroll_2x_omp_xmm2r8(H,delta,gamma,ax,n)
                 case (0)
                     call fov_x_axis_rolled_omp_xmm2r8(H,delta,gamma,ax,n)
                 case default
                      return
             end select
         else
              select case (unroll_cnt)
                 case (16)
                     call fov_x_axis_unroll_16x_xmm2r8(H,delta,gamma,ax,n)
                 case (8)
                     call fov_x_axis_unroll_8x_xmm2r8(H,delta,gamma,ax,n)
                 case (4)
                     call fov_x_axis_unroll_4x_xmm2r8(H,delta,gamma,ax,n)
                 case (2)
                     call fov_x_axis_unroll_2x_xmm2r8(H,delta,gamma,ax,n)
                 case (0)
                     call fov_x_axis_rolled_xmm2r8(H,delta,gamma,ax,n)
                 case default
                      return
             end select
         end if
     end subroutine fov_x_axis_dispatch_xmm2r8
     
     
     !////////////////////////////////////////////////////////////////////!
     !////////////////////////////////////////////////////////////////////!
     
     
       !Если рабочая зона сканирования ограничена углом G, то
      !ширина захвата
      !Formula 3, p. 100

      

    
      pure function scan_width_xmm4r4(H,gamma,theta) result(B)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_width_xmm4r4
        !dir$ attributes forceinline :: scan_width_xmm4r4
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_xmm4r4
        type(XMM4r4_t),  intent(in) :: H
        type(XMM4r4_t),  intent(in) :: gamma
        type(XMM4r4_t),  intent(in) :: theta
        type(XMM4r4_t) :: B
        type(XMM4r4_t), parameter :: half = XMM4r4_t(0.5_sp)
        type(XMM4r4_t), automatic :: gam2,th2,t0,t1
        !dir$ attributes align : 16:: gam2,th2,t0,t1
        gam2  = half.v*gamma.v
        th2   = half.v*theta.v
        t0    = tan(gam2.v)
        t1    = sin(th2.v)
        B     = (H.v+H.v)*t0.v*t1.v
      end function scan_width_xmm4r4


      subroutine scan_width_unroll_16x_xmm4r4(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_16x_xmm4r4
         !dir$ attributes forceinline :: scan_width_unroll_16x_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_16x_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: theta
         type(XMM4r4_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM4r4_t), automatic :: d8,d9,d10,d11,d12,d13,d14,d15
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d8,d9,d10,d11,d12,d13,d14,d15
         type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         type(XMM4r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
         integer(kind=i4) :: i,m,m1
          m = mod(n,16)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm4r4(H,d0,g0)
            end do
            if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,16
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm4r4(H,d1,g1)
            d2      = delta(i+2)
            g2      = theta(i+2)
            B(i+2) = scan_width_xmm4r4(H,d2,g2)
            d3      = delta(i+3)
            g3      = theta(i+3)
            B(i+3) = scan_width_xmm4r4(H,d3,g3)
            d4      = delta(i+4)
            g4      = theta(i+4)
            B(i+4) = scan_width_xmm4r4(H,d4,g4)
            d5      = delta(i+5)
            g5      = theta(i+5)
            B(i+5) = scan_width_ymmm8r4(H,d5,g5)
            d6      = delta(i+6)
            g6      = theta(i+6)
            B(i+6) = scan_width_xmm4r4(H,d6,g6)
            d7      = delta(i+7)
            g7      = theta(i+7)
            B(i+7) = scan_width_xmm4r4(H,d7,g7)
            d8      = delta(i+8)
            g8      = theta(i+8)
            B(i+8) = scan_width_xmm4r4(H,d8,g8)
            d9      = delta(i+9)
            g9      = theta(i+9)
            B(i+9) = scan_width_xmm4r4(H,d9,g9)
            d10     = delta(i+10)
            g10     = theta(i+10)
            B(i+10)= scan_width_xmm4r4(H,d10,g10)
            d11     = delta(i+11)
            g11     = theta(i+11)
            B(i+11)= scan_width_xmm4r4(H,d11,g11)
            d12     = delta(i+12)
            g12     = theta(i+12)
            B(i+12)= scan_width_xmm4r4(H,d12,g12)
            d13     = delta(i+13)
            g13     = theta(i+13)
            B(i+13)= scan_width_xmm4r4(H,d13,g13)
            d14     = delta(i+14)
            g14     = theta(i+14)
            B(i+14)= scan_width_xmm4r4(H,d14,g14)
            d15     = delta(i+15)
            g15     = theta(i+15)
            B(i+15)= scan_width_xmm4r4(H,d15,g15)
         end do
      end subroutine scan_width_unroll_16x_xmm4r4


      subroutine scan_width_unroll_16x_omp_xmm4r4(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_16x_omp_xmm4r4
         !dir$ attributes forceinline :: scan_width_unroll_16x_omp_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_16x_omp_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: theta
         type(XMM4r4_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM4r4_t), automatic :: d8,d9,d10,d11,d12,d13,d14,d15
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d8,d9,d10,d11,d12,d13,d14,d15
         type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         type(XMM4r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
         integer(kind=i4) :: i,m,m1
          m = mod(n,16)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i)  = scan_width_xmm4r4(H,d0,g0)
            end do
            if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1,d2,d3,d4,d5,d6,d7)      &
         !$omp private(d8,d9,d10,d11,d12,d13,d14,d15)                 &
         !$omp private(g0,g1,g2,g3,g4,g,g6,g7)                        &
         !$omp private(g8,g9,g10,g11,g12,g13,g14,g15)                 &
         !$omp shared(n,delta,theta,B,H)
         do i=m1,n,16
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm4r4(H,d1,g1)
            d2      = delta(i+2)
            g2      = theta(i+2)
            B(i+2) = scan_width_xmm4r4(H,d2,g2)
            d3      = delta(i+3)
            g3      = theta(i+3)
            B(i+3) = scan_width_xmm4r4(H,d3,g3)
            d4      = delta(i+4)
            g4      = theta(i+4)
            B(i+4) = scan_width_xmm4r4(H,d4,g4)
            d5      = delta(i+5)
            g5      = theta(i+5)
            B(i+5) = scan_width_ymmm8r4(H,d5,g5)
            d6      = delta(i+6)
            g6      = theta(i+6)
            B(i+6) = scan_width_xmm4r4(H,d6,g6)
            d7      = delta(i+7)
            g7      = theta(i+7)
            B(i+7) = scan_width_xmm4r4(H,d7,g7)
            d8      = delta(i+8)
            g8      = theta(i+8)
            B(i+8) = scan_width_xmm4r4(H,d8,g8)
            d9      = delta(i+9)
            g9      = theta(i+9)
            B(i+9) = scan_width_xmm4r4(H,d9,g9)
            d10     = delta(i+10)
            g10     = theta(i+10)
            B(i+10)= scan_width_xmm4r4(H,d10,g10)
            d11     = delta(i+11)
            g11     = theta(i+11)
            B(i+11)= scan_width_xmm4r4(H,d11,g11)
            d12     = delta(i+12)
            g12     = theta(i+12)
            B(i+12)= scan_width_xmm4r4(H,d12,g12)
            d13     = delta(i+13)
            g13     = theta(i+13)
            B(i+13)= scan_width_xmm4r4(H,d13,g13)
            d14     = delta(i+14)
            g14     = theta(i+14)
            B(i+14)= scan_width_xmm4r4(H,d14,g14)
            d15     = delta(i+15)
            g15     = theta(i+15)
            B(i+15)= scan_width_xmm4r4(H,d15,g15)
         end do
         !$omp end parallel do
      end subroutine scan_width_unroll_16x_omp_xmm4r4


      subroutine scan_width_unroll_8x_xmm4r4(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_8x_xmm4r4
         !dir$ attributes forceinline :: scan_width_unroll_8x_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_8x_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: theta
         type(XMM4r4_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         integer(kind=i4) :: i,m,m1
         m = mod(n,8)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm4r4(H,d0,g0)
            end do
            if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,8
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm4r4(H,d1,g1)
            d2      = delta(i+2)
            g2      = theta(i+2)
            B(i+2) = scan_width_xmm4r4(H,d2,g2)
            d3      = delta(i+3)
            g3      = theta(i+3)
            B(i+3) = scan_width_xmm4r4(H,d3,g3)
            d4      = delta(i+4)
            g4      = theta(i+4)
            B(i+4) = scan_width_xmm4r4(H,d4,g4)
            d5      = delta(i+5)
            g5      = theta(i+5)
            B(i+5) = scan_width_ymmm8r4(H,d5,g5)
            d6      = delta(i+6)
            g6      = theta(i+6)
            B(i+6) = scan_width_xmm4r4(H,d6,g6)
            d7      = delta(i+7)
            g7      = theta(i+7)
            B(i+7) = scan_width_xmm4r4(H,d7,g7)
         end do
      end subroutine scan_width_unroll_8x_xmm4r4


      subroutine scan_width_unroll_8x_omp_xmm4r4(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_8x_omp_xmm4r4
         !dir$ attributes forceinline :: scan_width_unroll_8x_omp_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_8x_omp_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: theta
         type(XMM4r4_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM4r4_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         integer(kind=i4) :: i,m,m1
          m = mod(n,8)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm4r4(H,d0,g0)
            end do
            if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1,d2,d3,d4,d5,d6,d7)      &
         !$omp private(g0,g1,g2,g3,g4,g,g6,g7)                        &
         !$omp shared(n,delta,theta,B,H)
         do i=m1,n,8
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm4r4(H,d1,g1)
            d2      = delta(i+2)
            g2      = theta(i+2)
            B(i+2) = scan_width_xmm4r4(H,d2,g2)
            d3      = delta(i+3)
            g3      = theta(i+3)
            B(i+3) = scan_width_xmm4r4(H,d3,g3)
            d4      = delta(i+4)
            g4      = theta(i+4)
            B(i+4) = scan_width_xmm4r4(H,d4,g4)
            d5      = delta(i+5)
            g5      = theta(i+5)
            B(i+5) = scan_width_ymmm8r4(H,d5,g5)
            d6      = delta(i+6)
            g6      = theta(i+6)
            B(i+6) = scan_width_xmm4r4(H,d6,g6)
            d7      = delta(i+7)
            g7      = theta(i+7)
            B(i+7) = scan_width_xmm4r4(H,d7,g7)
         end do
         !$omp end parallel do
      end subroutine scan_width_unroll_8x_omp_xmm4r4


      subroutine scan_width_unroll_4x_xmm4r4(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_4x_xmm4r4
         !dir$ attributes forceinline :: scan_width_unroll_4x_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_4x_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: theta
         type(XMM4r4_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3
         !dir$ attributes align : 16:: d0,d1,d2,d3
         type(XMM4r4_t), automatic :: g0,g1,g2,g3
         !dir$ attributes align : 16:: g0,g1,g2,g3
         integer(kind=i4) :: i,m,m1
         m = mod(n,4)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm4r4(H,d0,g0)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,4
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm4r4(H,d1,g1)
            d2      = delta(i+2)
            g2      = theta(i+2)
            B(i+2) = scan_width_xmm4r4(H,d2,g2)
            d3      = delta(i+3)
            g3      = theta(i+3)
            B(i+3) = scan_width_xmm4r4(H,d3,g3)
         end do
      end subroutine scan_width_unroll_4x_xmm4r4


      subroutine scan_width_unroll_4x_omp_xmm4r4(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_4x_omp_xmm4r4
         !dir$ attributes forceinline :: scan_width_unroll_4x_omp_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_4x_omp_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: theta
         type(XMM4r4_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1,d2,d3
         !dir$ attributes align : 16:: d0,d1,d2,d3
         type(XMM4r4_t), automatic :: g0,g1,g2,g3
         !dir$ attributes align : 16:: g0,g1,g2,g3
         integer(kind=i4) :: i,m,m1
          m = mod(n,4)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm4r4(H,d0,g0)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1,d2,d3)      &
         !$omp private(g0,g1,g2,g3)                        &
         !$omp shared(n,delta,theta,B,H)
         do i=m1,n,4
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm4r4(H,d1,g1)
            d2      = delta(i+2)
            g2      = theta(i+2)
            B(i+2) = scan_width_xmm4r4(H,d2,g2)
            d3      = delta(i+3)
            g3      = theta(i+3)
            B(i+3) = scan_width_xmm4r4(H,d3,g3)
         end do
         !$omp end parallel do
     end subroutine scan_width_unroll_4x_omp_xmm4r4



     subroutine scan_width_unroll_2x_xmm4r4(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_2x_xmm4r4
         !dir$ attributes forceinline :: scan_width_unroll_2x_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_2x_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: theta
         type(XMM4r4_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1
         !dir$ attributes align : 16:: d0,d1
         type(XMM4r4_t), automatic :: g0,g1
         !dir$ attributes align : 16:: g0,g1
         integer(kind=i4) :: i,m,m1
         m = mod(n,2)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm4r4(H,d0,g0)
            end do
            if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:1
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=m1,n,2
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm4r4(H,d1,g1)
         end do
      end subroutine scan_width_unroll_2x_xmm4r4


      subroutine scan_width_unroll_2x_omp_xmm4r4(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_2x_omp_xmm4r4
         !dir$ attributes forceinline :: scan_width_unroll_2x_omp_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_2x_omp_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: theta
         type(XMM4r4_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0,d1
         !dir$ attributes align : 64 :: d0,d1
         type(XMM4r4_t), automatic :: g0,g1
         !dir$ attributes align : 64 :: g0,g1
         integer(kind=i4) :: i,m,m1
          m = mod(n,2)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm4r4(H,d0,g0)
            end do
            if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1)      &
         !$omp private(g0,g1)                        &
         !$omp shared(n,delta,theta,B,H)
         do i=m1,n,2
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm4r4(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm4r4(H,d1,g1)
         end do
         !$omp end parallel do
     end subroutine scan_width_unroll_2x_omp_xmm4r4


     subroutine scan_width_rolled_xmm4r4(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_rolled_xmm4r4
         !dir$ attributes forceinline :: scan_width_rolled_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_rolled_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: theta
         type(XMM4r4_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0
         !dir$ attributes align : 16:: d0
         type(XMM4r4_t), automatic :: g0
         !dir$ attributes align : 16:: g0
         integer(kind=i4) :: i
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         do i=1,n
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm4r4(H,d0,g0)
         end do
      end subroutine scan_width_rolled_xmm4r4


      subroutine scan_width_rolled_omp_xmm4r4(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_rolled_omp_xmm4r4
         !dir$ attributes forceinline :: scan_width_rolled_omp_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_rolled_omp_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: theta
         type(XMM4r4_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM4r4_t), automatic :: d0
         !dir$ attributes align : 16:: d0
         type(XMM4r4_t), automatic :: g0
         !dir$ attributes align : 16:: g0
         integer(kind=i4) :: i
        
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp private(d0)      &
         !$omp private(g0)                        &
         !$omp shared(n,delta,theta,B,H)
         do i=1,n
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm4r4(H,d0,g0)
         end do
         !$omp end parallel do
     end subroutine scan_width_rolled_omp_xmm4r4


     subroutine scan_width_dispatch_xmm4r4(H,delta,theta,B,n,unroll_cnt,omp_ver)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_dispatch_xmm4r4
         type(XMM4r4_t),                  intent(in) :: H
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: delta
         type(XMM4r4_t),  dimension(1:n), intent(in)  :: theta
         type(XMM4r4_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         integer(kind=i4),                 intent(in)  :: unroll_cnt
         logical(kind=i4),                 intent(in)  :: omp_ver
         if(omp_ver) then
            select case (unroll_cnt)
                case (16)
                   call scan_width_unroll_16x_omp_xmm4r4(H,delta,theta,B,n)
                case (8)
                   call scan_width_unroll_8x_omp_xmm4r4(H,delta,theta,B,n)
                case (4)
                   call scan_width_unroll_4x_omp_xmm4r4(H,delta,theta,B,n)
                case (2)
                   call scan_width_unroll_2x_omp_xmm4r4(H,delta,theta,B,n)
                case (0)
                   call scan_width_rolled_omp_xmm4r4(H,delta,theta,B,n)
                case default
                   return
             end select
          else
              select case (unroll_cnt)
                case (16)
                   call scan_width_unroll_16x_xmm4r4(H,delta,theta,B,n)
                case (8)
                   call scan_width_unroll_8x_xmm4r4(H,delta,theta,B,n)
                case (4)
                   call scan_width_unroll_4x_xmm4r4(H,delta,theta,B,n)
                case (2)
                   call scan_width_unroll_2x_xmm4r4(H,delta,theta,B,n)
                case (0)
                   call scan_width_rolled_xmm4r4(H,delta,theta,B,n)
                case default
                   return
             end select
     end subroutine scan_width_dispatch_xmm4r4
     
     
     pure function scan_width_xmm2r8(H,gamma,theta) result(B)
        !dir$ optimize:3
        !dir$ attributes code_align : 32 :: scan_width_xmm2r8
        !dir$ attributes forceinline :: scan_width_xmm2r8
        !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_xmm2r8
        type(XMM2r8_t),  intent(in) :: H
        type(XMM2r8_t),  intent(in) :: gamma
        type(XMM2r8_t),  intent(in) :: theta
        type(XMM2r8_t) :: B
        type(XMM2r8_t), parameter :: half = XMM2r8_t(0.5_dp)
        type(XMM2r8_t), automatic :: gam2,th2,t0,t1
        !dir$ attributes align : 16:: gam2,th2,t0,t1
        gam2  = half.v*gamma.v
        th2   = half.v*theta.v
        t0    = tan(gam2.v)
        t1    = sin(th2.v)
        B     = (H.v+H.v)*t0.v*t1.v
     end function scan_width_xmm2r8 


     subroutine scan_width_unroll_16x_xmm2r8(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_16x_xmm2r8
         !dir$ attributes forceinline :: scan_width_unroll_16x_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_16x_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: theta
         type(XMM2r8_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM2r8_t), automatic :: d8,d9,d10,d11,d12,d13,d14,d15
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d8,d9,d10,d11,d12,d13,d14,d15
         type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         type(XMM2r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
         integer(kind=i4) :: i,m,m1
          m = mod(n,16)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm2r8(H,d0,g0)
            end do
            if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,16
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm2r8(H,d1,g1)
            d2      = delta(i+2)
            g2      = theta(i+2)
            B(i+2) = scan_width_xmm2r8(H,d2,g2)
            d3      = delta(i+3)
            g3      = theta(i+3)
            B(i+3) = scan_width_xmm2r8(H,d3,g3)
            d4      = delta(i+4)
            g4      = theta(i+4)
            B(i+4) = scan_width_xmm2r8(H,d4,g4)
            d5      = delta(i+5)
            g5      = theta(i+5)
            B(i+5) = scan_width_ymmm4r8(H,d5,g5)
            d6      = delta(i+6)
            g6      = theta(i+6)
            B(i+6) = scan_width_xmm2r8(H,d6,g6)
            d7      = delta(i+7)
            g7      = theta(i+7)
            B(i+7) = scan_width_xmm2r8(H,d7,g7)
            d8      = delta(i+8)
            g8      = theta(i+8)
            B(i+8) = scan_width_xmm2r8(H,d8,g8)
            d9      = delta(i+9)
            g9      = theta(i+9)
            B(i+9) = scan_width_xmm2r8(H,d9,g9)
            d10     = delta(i+10)
            g10     = theta(i+10)
            B(i+10)= scan_width_xmm2r8(H,d10,g10)
            d11     = delta(i+11)
            g11     = theta(i+11)
            B(i+11)= scan_width_xmm2r8(H,d11,g11)
            d12     = delta(i+12)
            g12     = theta(i+12)
            B(i+12)= scan_width_xmm2r8(H,d12,g12)
            d13     = delta(i+13)
            g13     = theta(i+13)
            B(i+13)= scan_width_xmm2r8(H,d13,g13)
            d14     = delta(i+14)
            g14     = theta(i+14)
            B(i+14)= scan_width_xmm2r8(H,d14,g14)
            d15     = delta(i+15)
            g15     = theta(i+15)
            B(i+15)= scan_width_xmm2r8(H,d15,g15)
         end do
      end subroutine scan_width_unroll_16x_xmm2r8


      subroutine scan_width_unroll_16x_omp_xmm2r8(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_16x_omp_xmm2r8
         !dir$ attributes forceinline :: scan_width_unroll_16x_omp_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_16x_omp_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: theta
         type(XMM2r8_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM2r8_t), automatic :: d8,d9,d10,d11,d12,d13,d14,d15
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d8,d9,d10,d11,d12,d13,d14,d15
         type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         type(XMM2r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
         integer(kind=i4) :: i,m,m1
          m = mod(n,16)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i)  = scan_width_xmm2r8(H,d0,g0)
            end do
            if(n<16) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1,d2,d3,d4,d5,d6,d7)      &
         !$omp private(d8,d9,d10,d11,d12,d13,d14,d15)                 &
         !$omp private(g0,g1,g2,g3,g4,g,g6,g7)                        &
         !$omp private(g8,g9,g10,g11,g12,g13,g14,g15)                 &
         !$omp shared(n,delta,theta,B,H)
         do i=m1,n,16
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm2r8(H,d1,g1)
            d2      = delta(i+2)
            g2      = theta(i+2)
            B(i+2) = scan_width_xmm2r8(H,d2,g2)
            d3      = delta(i+3)
            g3      = theta(i+3)
            B(i+3) = scan_width_xmm2r8(H,d3,g3)
            d4      = delta(i+4)
            g4      = theta(i+4)
            B(i+4) = scan_width_xmm2r8(H,d4,g4)
            d5      = delta(i+5)
            g5      = theta(i+5)
            B(i+5) = scan_width_ymmm4r8(H,d5,g5)
            d6      = delta(i+6)
            g6      = theta(i+6)
            B(i+6) = scan_width_xmm2r8(H,d6,g6)
            d7      = delta(i+7)
            g7      = theta(i+7)
            B(i+7) = scan_width_xmm2r8(H,d7,g7)
            d8      = delta(i+8)
            g8      = theta(i+8)
            B(i+8) = scan_width_xmm2r8(H,d8,g8)
            d9      = delta(i+9)
            g9      = theta(i+9)
            B(i+9) = scan_width_xmm2r8(H,d9,g9)
            d10     = delta(i+10)
            g10     = theta(i+10)
            B(i+10)= scan_width_xmm2r8(H,d10,g10)
            d11     = delta(i+11)
            g11     = theta(i+11)
            B(i+11)= scan_width_xmm2r8(H,d11,g11)
            d12     = delta(i+12)
            g12     = theta(i+12)
            B(i+12)= scan_width_xmm2r8(H,d12,g12)
            d13     = delta(i+13)
            g13     = theta(i+13)
            B(i+13)= scan_width_xmm2r8(H,d13,g13)
            d14     = delta(i+14)
            g14     = theta(i+14)
            B(i+14)= scan_width_xmm2r8(H,d14,g14)
            d15     = delta(i+15)
            g15     = theta(i+15)
            B(i+15)= scan_width_xmm2r8(H,d15,g15)
         end do
         !$omp end parallel do
      end subroutine scan_width_unroll_16x_omp_xmm2r8


      subroutine scan_width_unroll_8x_xmm2r8(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_8x_xmm2r8
         !dir$ attributes forceinline :: scan_width_unroll_8x_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_8x_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: theta
         type(XMM2r8_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         integer(kind=i4) :: i,m,m1
         m = mod(n,8)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm2r8(H,d0,g0)
            end do
            if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,8
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm2r8(H,d1,g1)
            d2      = delta(i+2)
            g2      = theta(i+2)
            B(i+2) = scan_width_xmm2r8(H,d2,g2)
            d3      = delta(i+3)
            g3      = theta(i+3)
            B(i+3) = scan_width_xmm2r8(H,d3,g3)
            d4      = delta(i+4)
            g4      = theta(i+4)
            B(i+4) = scan_width_xmm2r8(H,d4,g4)
            d5      = delta(i+5)
            g5      = theta(i+5)
            B(i+5) = scan_width_ymmm4r8(H,d5,g5)
            d6      = delta(i+6)
            g6      = theta(i+6)
            B(i+6) = scan_width_xmm2r8(H,d6,g6)
            d7      = delta(i+7)
            g7      = theta(i+7)
            B(i+7) = scan_width_xmm2r8(H,d7,g7)
         end do
      end subroutine scan_width_unroll_8x_xmm2r8


      subroutine scan_width_unroll_8x_omp_xmm2r8(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_8x_omp_xmm2r8
         !dir$ attributes forceinline :: scan_width_unroll_8x_omp_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_8x_omp_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: theta
         type(XMM2r8_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1,d2,d3,d4,d5,d6,d7
         !dir$ attributes align : 16:: d0,d1,d2,d3,d4,d5,d6,d7
         type(XMM2r8_t), automatic :: g0,g1,g2,g3,g4,g,g6,g7
         !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g,g6,g7
         integer(kind=i4) :: i,m,m1
          m = mod(n,8)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm2r8(H,d0,g0)
            end do
            if(n<8) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1,d2,d3,d4,d5,d6,d7)      &
         !$omp private(g0,g1,g2,g3,g4,g,g6,g7)                        &
         !$omp shared(n,delta,theta,B,H)
         do i=m1,n,8
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm2r8(H,d1,g1)
            d2      = delta(i+2)
            g2      = theta(i+2)
            B(i+2) = scan_width_xmm2r8(H,d2,g2)
            d3      = delta(i+3)
            g3      = theta(i+3)
            B(i+3) = scan_width_xmm2r8(H,d3,g3)
            d4      = delta(i+4)
            g4      = theta(i+4)
            B(i+4) = scan_width_xmm2r8(H,d4,g4)
            d5      = delta(i+5)
            g5      = theta(i+5)
            B(i+5) = scan_width_xmm2r8(H,d5,g5)
            d6      = delta(i+6)
            g6      = theta(i+6)
            B(i+6) = scan_width_xmm2r8(H,d6,g6)
            d7      = delta(i+7)
            g7      = theta(i+7)
            B(i+7) = scan_width_xmm2r8(H,d7,g7)
         end do
         !$omp end parallel do
      end subroutine scan_width_unroll_8x_omp_xmm2r8


      subroutine scan_width_unroll_4x_xmm2r8(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_4x_xmm2r8
         !dir$ attributes forceinline :: scan_width_unroll_4x_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_4x_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: theta
         type(XMM2r8_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1,d2,d3
         !dir$ attributes align : 16:: d0,d1,d2,d3
         type(XMM2r8_t), automatic :: g0,g1,g2,g3
         !dir$ attributes align : 16:: g0,g1,g2,g3
         integer(kind=i4) :: i,m,m1
         m = mod(n,4)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm2r8(H,d0,g0)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,4
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm2r8(H,d1,g1)
            d2      = delta(i+2)
            g2      = theta(i+2)
            B(i+2) = scan_width_xmm2r8(H,d2,g2)
            d3      = delta(i+3)
            g3      = theta(i+3)
            B(i+3) = scan_width_xmm2r8(H,d3,g3)
         end do
      end subroutine scan_width_unroll_4x_xmm2r8


      subroutine scan_width_unroll_4x_omp_xmm2r8(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_4x_omp_xmm2r8
         !dir$ attributes forceinline :: scan_width_unroll_4x_omp_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_4x_omp_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: theta
         type(XMM2r8_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1,d2,d3
         !dir$ attributes align : 16:: d0,d1,d2,d3
         type(XMM2r8_t), automatic :: g0,g1,g2,g3
         !dir$ attributes align : 16:: g0,g1,g2,g3
         integer(kind=i4) :: i,m,m1
          m = mod(n,4)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm2r8(H,d0,g0)
            end do
            if(n<4) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1,d2,d3)      &
         !$omp private(g0,g1,g2,g3)                        &
         !$omp shared(n,delta,theta,B,H)
         do i=m1,n,4
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm2r8(H,d1,g1)
            d2      = delta(i+2)
            g2      = theta(i+2)
            B(i+2) = scan_width_xmm2r8(H,d2,g2)
            d3      = delta(i+3)
            g3      = theta(i+3)
            B(i+3) = scan_width_xmm2r8(H,d3,g3)
         end do
         !$omp end parallel do
     end subroutine scan_width_unroll_4x_omp_xmm2r8



     subroutine scan_width_unroll_2x_xmm2r8(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_2x_xmm2r8
         !dir$ attributes forceinline :: scan_width_unroll_2x_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_2x_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: theta
         type(XMM2r8_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1
         !dir$ attributes align : 16:: d0,d1
         type(XMM2r8_t), automatic :: g0,g1
         !dir$ attributes align : 16:: g0,g1
         integer(kind=i4) :: i,m,m1
         m = mod(n,2)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm2r8(H,d0,g0)
            end do
            if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=m1,n,2
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm2r8(H,d1,g1)
         end do
      end subroutine scan_width_unroll_2x_xmm2r8


      subroutine scan_width_unroll_2x_omp_xmm2r8(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_unroll_2x_omp_xmm2r8
         !dir$ attributes forceinline :: scan_width_unroll_2x_omp_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_unroll_2x_omp_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: theta
         type(XMM2r8_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0,d1
         !dir$ attributes align : 64 :: d0,d1
         type(XMM2r8_t), automatic :: g0,g1
         !dir$ attributes align : 64 :: g0,g1
         integer(kind=i4) :: i,m,m1
          m = mod(n,2)
         if(m /= 0) then
            do i=1,m
               d0    = delta(i)
               g0    = theta(i)
               B(i) = scan_width_xmm2r8(H,d0,g0)
            end do
            if(n<2) return
         end if
         m1 = m+1
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp firstprivate(m1) private(d0,d1)      &
         !$omp private(g0,g1)                        &
         !$omp shared(n,delta,theta,B,H)
         do i=m1,n,2
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm2r8(H,d0,g0)
            d1      = delta(i+1)
            g1      = theta(i+1)
            B(i+1) = scan_width_xmm2r8(H,d1,g1)
         end do
         !$omp end parallel do
     end subroutine scan_width_unroll_2x_omp_xmm2r8


     subroutine scan_width_rolled_xmm2r8(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_rolled_xmm4r4
         !dir$ attributes forceinline :: scan_width_rolled_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_rolled_xmm4r4
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: theta
         type(XMM2r8_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0
         !dir$ attributes align : 16:: d0
         type(XMM2r8_t), automatic :: g0
         !dir$ attributes align : 16:: g0
         integer(kind=i4) :: i
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         do i=1,n
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm2r8(H,d0,g0)
         end do
      end subroutine scan_width_rolled_xmm2r8


      subroutine scan_width_rolled_omp_xmm2r8(H,delta,theta,B,n)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_rolled_omp_xmm2r8
         !dir$ attributes forceinline :: scan_width_rolled_omp_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: scan_width_rolled_omp_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: theta
         type(XMM2r8_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         type(XMM2r8_t), automatic :: d0
         !dir$ attributes align : 16:: d0
         type(XMM2r8_t), automatic :: g0
         !dir$ attributes align : 16:: g0
         integer(kind=i4) :: i
        
           !dir$ assume_aligned delta:16
           !dir$ assume_aligned theta:16
           !dir$ assume_aligned B:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
         !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
         !$omp private(d0)      &
         !$omp private(g0)                        &
         !$omp shared(n,delta,theta,B,H)
         do i=1,n
            d0      = delta(i)
            g0      = theta(i)
            B(i)   = scan_width_xmm2r8(H,d0,g0)
         end do
         !$omp end parallel do
     end subroutine scan_width_rolled_omp_xmm2r8


     subroutine scan_width_dispatch_xmm2r8(H,delta,theta,B,n,unroll_cnt,omp_ver)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: scan_width_dispatch_xmm2r8
         type(XMM2r8_t),                  intent(in) :: H
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: delta
         type(XMM2r8_t),  dimension(1:n), intent(in)  :: theta
         type(XMM2r8_t),  dimension(1:n), intent(out) :: B
         integer(kind=i4),                 intent(in)  :: n
         integer(kind=i4),                 intent(in)  :: unroll_cnt
         logical(kind=i4),                 intent(in)  :: omp_ver
         if(omp_ver) then
            select case (unroll_cnt)
                case (16)
                   call scan_width_unroll_16x_omp_xmm2r8(H,delta,theta,B,n)
                case (8)
                   call scan_width_unroll_8x_omp_xmm2r8(H,delta,theta,B,n)
                case (4)
                   call scan_width_unroll_4x_omp_xmm2r8(H,delta,theta,B,n)
                case (2)
                   call scan_width_unroll_2x_omp_xmm2r8(H,delta,theta,B,n)
                case (0)
                   call scan_width_rolled_omp_xmm2r8(H,delta,theta,B,n)
                case default
                   return
             end select
          else
              select case (unroll_cnt)
                case (16)
                   call scan_width_unroll_16x_xmm2r8(H,delta,theta,B,n)
                case (8)
                   call scan_width_unroll_8x_xmm2r8(H,delta,theta,B,n)
                case (4)
                   call scan_width_unroll_4x_xmm2r8(H,delta,theta,B,n)
                case (2)
                   call scan_width_unroll_2x_xmm2r8(H,delta,theta,B,n)
                case (0)
                   call scan_width_rolled_xmm2r8(H,delta,theta,B,n)
                case default
                   return
             end select
     end subroutine scan_width_dispatch_xmm2r8
     
     
     !///////////////////////////////////////////////////////////////////////!
     !//////////////////////////////////////////////////////////////////////!
     
     
      !Плоскопараллельная пластинка, установленная за 
      !объективом, изменяет ход лучей таким образом, что изображение
      ! светящейся точки отодвигается и его положение зависит от угла у
      !между оптической осью и нормалью N к поверхности пластинки
      ! Formula 7,8 p. 106
    
    
      !AVX/AVX2 version
     pure function refract_shift_xmm4r4(i1,delta,alfa,gamma,n)  result(l)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_xmm4r4
            !dir$ attributes forceinline ::  refract_shift_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  refract_shift_xmm4r4
            use mod_fpcompare, only : xmm4r4_equalto_xmm4r4
            use mod_vectypes,  only : Mask8_t
            type(XMM4r4_t),   intent(in) :: i1
            type(XMM4r4_t),   intent(in) :: delta
            type(XMM4r4_t),   intent(in) :: alfa
            type(XMM4r4_t),   intent(in) :: gamma
            type(XMM4r4_t),   intent(in) :: n
            type(XMM4r4_t) :: l
            type(XMM4r4_t), parameter :: one = XMM4r4_t(1.0_sp)
            type(XMM4r4_t), parameter :: zer = XMM4r4_t(0.0_sp)
            type(XMM4r4_t), automatic :: ag,num,den,sin2,sag,t0,t1
            !dir$ attributes align : 16:: ag,num,den,sin2,sag,t0,t1
            type(Mask8_t), automatic :: m1,m2
            ag  = alfa.v-gamma.v
            m1  = xmm4r4_equalto_xmm4r4(i1,ag)
            m2  = xmm4r4_equalto_xmm4r4(alfa,zer) 
            if(all(m1)) then
               sag  = sin(ag.v)
               t0   = delta.v*sag.v
               sin2 = sag.v*sag.v
               num  = one.v-sag.v
               den  = n.v*n.v*sag.v
               t1   = one.v-sqrt(num.v/den.v)
               l    = t0.v*t1.v
            else if(all(m2)) then
               sag  = sin(gamma.v)
               t0   = -delta.v*sag.v
               sin2 = sag.v*sag.v
               num  = one.v-sin2.v
               den  = n.v*n.v-sin2.v
               t1   = one.v-sqrt(num.v/den.v)
               l    = t0.v*t1.v
            else
               sag  = sin(i1.v)
               t0   = delta.v*sag.v
               sin2 = sag.v*sag.v
               num  = one.v-sin2.v
               den  = n.v*n.v-sin2.v
               t1   = one.v-sqrt(num.v/den.v)
               l    = t0.v*t1.v
            end if
       end function refract_shift_xmm4r4


       subroutine refract_shift_unroll_16x_xmm4r4(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_16x_xmm4r4
            !dir$ attributes forceinline ::  refract_shift_unroll_16x_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_16x_xmm4r4
            type(XMM4r4_t),                     intent(in) :: i1
            type(XMM4r4_t),                     intent(in) :: delta
            type(XMM4r4_t), dimension(1:len),   intent(in) :: alfa
            type(XMM4r4_t), dimension(1:len),   intent(in) :: gamma
            type(XMM4r4_t), dimension(1:len),   intent(in) :: n
            type(XMM4r4_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
            type(XMM4r4_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
            !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
            !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
            type(XMM4r4_t) , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
            type(XMM4r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
            !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
            !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
            type(XMM4r4_t), automatic :: n0,n1,n2,n3,n4,n5,n6,n7
            type(XMM4r4_t), automatic :: n8,n9,n10,n11,n12,n13,n14,n15
            !dir$ attributes align : 16:: n0,n1,n2,n3,n4,n5,n6,n7
            !dir$ attributes align : 16:: n8,n9,n10,n11,n12,n13,n14,n15
            integer(kind=i4) :: i,m,m1
            m = mod(len,16)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
            end do
            if(len<16) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
           do i=m1,len,16
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm4r4(i1,delta,a1,g1,n1)
             a2     = alfa(i+2)
             g2     = gamma(i+2)
             n2     = n(i+2)
             l(i+2) = refract_shift_xmm4r4(i1,delta,a2,g2,n2)
             a3     = alfa(i+3)
             g3     = gamma(i+3)
             n3     = n(i+3)
             l(i+3) = refract_shift_xmm4r4(i1,delta,a3,g3,n3)
             a4     = alfa(i+4)
             g4     = gamma(i+4)
             n4     = n(i+4)
             l(i+4) = refract_shift_xmm4r4(i1,delta,a4,g4,n4)
             a5     = alfa(i+5)
             g5     = gamma(i+5)
             n5     = n(i+5)
             l(i+5) = refract_shift_xmm4r4(i1,delta,a5,g5,n5)
             a6     = alfa(i+6)
             g6     = gamma(i+6)
             n6     = n(i+6)
             l(i+6) = refract_shift_xmm4r4(i1,delta,a6,g6,n6)
             a7     = alfa(i+7)
             g7     = gamma(i+7)
             n7     = n(i+7)
             l(i+7) = refract_shift_xmm4r4(i1,delta,a7,g7,n7)
             a8     = alfa(i+8)
             g8     = gamma(i+8)
             n8     = n(i+8)
             l(i+8) = refract_shift_xmm4r4(i1,delta,a8,g8,n8)
             a9     = alfa(i+9)
             g9     = gamma(i+9)
             n9     = n(i+9)
             l(i+9) = refract_shift_xmm4r4(i1,delta,a9,g9,n9)
             a10    = alfa(i+10)
             g10    = gamma(i+10)
             n10    = n(i+10)
             l(i+10)= refract_shift_xmm4r4(i1,delta,a10,g10,n10)
             a11    = alfa(i+11)
             g11    = gamma(i+11)
             n11    = n(i+11)
             l(i+11)= refract_shift_xmm4r4(i1,delta,a11,g11,n11)
             a12    = alfa(i+12)
             g12    = gamma(i+12)
             n12    = n(i+12)
             l(i+12)= refract_shift_xmm4r4(i1,delta,a12,g12,n12)
             a13    = alfa(i+13)
             g13    = gamma(i+13)
             n13    = n(i+13)
             l(i+13)= refract_shift_xmm4r4(i1,delta,a13,g13,n13)
             a14    = alfa(i+14)
             g14    = gamma(i+14)
             n14    = n(i+14)
             l(i+14)= refract_shift_xmm4r4(i1,delta,a14,g14,n14)
             a15    = alfa(i+15)
             g15    = gamma(i+15)
             n15    = n(i+15)
             l(i+15)= refract_shift_xmm4r4(i1,delta,a15,g15,n15)
         end do
      end subroutine refract_shift_unroll_16x_xmm4r4


      subroutine refract_shift_unroll_16x_omp_xmm4r4(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_16x_omp_xmm4r4
            !dir$ attributes forceinline ::  refract_shift_unroll_16x_omp_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_16x_omp_xmm4r4
            type(XMM4r4_t),                     intent(in) :: i1
            type(XMM4r4_t),                     intent(in) :: delta
            type(XMM4r4_t), dimension(1:len),   intent(in) :: alfa
            type(XMM4r4_t), dimension(1:len),   intent(in) :: gamma
            type(XMM4r4_t), dimension(1:len),   intent(in) :: n
            type(XMM4r4_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
            type(XMM4r4_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
            !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
            !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
            type(XMM4r4_t) , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
            type(XMM4r4_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
            !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
            !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
            type(XMM4r4_t), automatic :: n0,n1,n2,n3,n4,n5,n6,n7
            type(XMM4r4_t), automatic :: n8,n9,n10,n11,n12,n13,n14,n15
            !dir$ attributes align : 16:: n0,n1,n2,n3,n4,n5,n6,n7
            !dir$ attributes align : 16:: n8,n9,n10,n11,n12,n13,n14,n15
            integer(kind=i4) :: i,m,m1
            m = mod(len,16)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
            end do
            if(len<16) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
           !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
           !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)      &
           !$omp private(a8,a9,a10,a11,a12,a13,a14,a15)                 &
           !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                        &
           !$omp private(g8,g9,g10,g11,g12,g13,g14,g15)                 &
           !$omp private(n0,n1,n2,n3,n4,n5,n6,n7)                       &
           !$omp private(n8,n9,n10,n11,n12,n13,n14,n15)                 &
           !$omp shared(i1,delta,len,alfa,gamma,n)
           do i=m1,len,16
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm4r4(i1,delta,a1,g1,n1)
             a2     = alfa(i+2)
             g2     = gamma(i+2)
             n2     = n(i+2)
             l(i+2) = refract_shift_xmm4r4(i1,delta,a2,g2,n2)
             a3     = alfa(i+3)
             g3     = gamma(i+3)
             n3     = n(i+3)
             l(i+3) = refract_shift_xmm4r4(i1,delta,a3,g3,n3)
             a4     = alfa(i+4)
             g4     = gamma(i+4)
             n4     = n(i+4)
             l(i+4) = refract_shift_xmm4r4(i1,delta,a4,g4,n4)
             a5     = alfa(i+5)
             g5     = gamma(i+5)
             n5     = n(i+5)
             l(i+5) = refract_shift_xmm4r4(i1,delta,a5,g5,n5)
             a6     = alfa(i+6)
             g6     = gamma(i+6)
             n6     = n(i+6)
             l(i+6) = refract_shift_xmm4r4(i1,delta,a6,g6,n6)
             a7     = alfa(i+7)
             g7     = gamma(i+7)
             n7     = n(i+7)
             l(i+7) = refract_shift_xmm4r4(i1,delta,a7,g7,n7)
             a8     = alfa(i+8)
             g8     = gamma(i+8)
             n8     = n(i+8)
             l(i+8) = refract_shift_xmm4r4(i1,delta,a8,g8,n8)
             a9     = alfa(i+9)
             g9     = gamma(i+9)
             n9     = n(i+9)
             l(i+9) = refract_shift_xmm4r4(i1,delta,a9,g9,n9)
             a10    = alfa(i+10)
             g10    = gamma(i+10)
             n10    = n(i+10)
             l(i+10)= refract_shift_xmm4r4(i1,delta,a10,g10,n10)
             a11    = alfa(i+11)
             g11    = gamma(i+11)
             n11    = n(i+11)
             l(i+11)= refract_shift_xmm4r4(i1,delta,a11,g11,n11)
             a12    = alfa(i+12)
             g12    = gamma(i+12)
             n12    = n(i+12)
             l(i+12)= refract_shift_xmm4r4(i1,delta,a12,g12,n12)
             a13    = alfa(i+13)
             g13    = gamma(i+13)
             n13    = n(i+13)
             l(i+13)= refract_shift_xmm4r4(i1,delta,a13,g13,n13)
             a14    = alfa(i+14)
             g14    = gamma(i+14)
             n14    = n(i+14)
             l(i+14)= refract_shift_xmm4r4(i1,delta,a14,g14,n14)
             a15    = alfa(i+15)
             g15    = gamma(i+15)
             n15    = n(i+15)
             l(i+15)= refract_shift_xmm4r4(i1,delta,a15,g15,n15)
          end do
          !$omp end parallel do
      end subroutine refract_shift_unroll_16x_omp_xmm4r4


      subroutine refract_shift_unroll_8x_xmm4r4(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_8x_xmm4r4
            !dir$ attributes forceinline ::  refract_shift_unroll_8x_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_8x_xmm4r4
            type(XMM4r4_t),                     intent(in) :: i1
            type(XMM4r4_t),                     intent(in) :: delta
            type(XMM4r4_t), dimension(1:len),   intent(in) :: alfa
            type(XMM4r4_t), dimension(1:len),   intent(in) :: gamma
            type(XMM4r4_t), dimension(1:len),   intent(in) :: n
            type(XMM4r4_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
            !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
            type(XMM4r4_t) , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
            !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
            type(XMM4r4_t), automatic :: n0,n1,n2,n3,n4,n5,n6,n7
            !dir$ attributes align : 16:: n0,n1,n2,n3,n4,n5,n6,n7
            integer(kind=i4) :: i,m,m1
            m = mod(len,8)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
            end do
            if(len<8) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
           do i=m1,len,8
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm4r4(i1,delta,a1,g1,n1)
             a2     = alfa(i+2)
             g2     = gamma(i+2)
             n2     = n(i+2)
             l(i+2) = refract_shift_xmm4r4(i1,delta,a2,g2,n2)
             a3     = alfa(i+3)
             g3     = gamma(i+3)
             n3     = n(i+3)
             l(i+3) = refract_shift_xmm4r4(i1,delta,a3,g3,n3)
             a4     = alfa(i+4)
             g4     = gamma(i+4)
             n4     = n(i+4)
             l(i+4) = refract_shift_xmm4r4(i1,delta,a4,g4,n4)
             a5     = alfa(i+5)
             g5     = gamma(i+5)
             n5     = n(i+5)
             l(i+5) = refract_shift_xmm4r4(i1,delta,a5,g5,n5)
             a6     = alfa(i+6)
             g6     = gamma(i+6)
             n6     = n(i+6)
             l(i+6) = refract_shift_xmm4r4(i1,delta,a6,g6,n6)
             a7     = alfa(i+7)
             g7     = gamma(i+7)
             n7     = n(i+7)
             l(i+7) = refract_shift_xmm4r4(i1,delta,a7,g7,n7)
         end do
      end subroutine refract_shift_unroll_8x_xmm4r4


      subroutine refract_shift_unroll_8x_omp_xmm4r4(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_8x_omp_xmm4r4
            !dir$ attributes forceinline ::  refract_shift_unroll_8x_omp_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_8x_omp_xmm4r4
            type(XMM4r4_t),                     intent(in) :: i1
            type(XMM4r4_t),                     intent(in) :: delta
            type(XMM4r4_t), dimension(1:len),   intent(in) :: alfa
            type(XMM4r4_t), dimension(1:len),   intent(in) :: gamma
            type(XMM4r4_t), dimension(1:len),   intent(in) :: n
            type(XMM4r4_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM4r4_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
            !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
            type(XMM4r4_t) , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
            !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
            type(XMM4r4_t), automatic :: n0,n1,n2,n3,n4,n5,n6,n7
            !dir$ attributes align : 16:: n0,n1,n2,n3,n4,n5,n6,n7
            integer(kind=i4) :: i,m,m1
            m = mod(len,8)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
            end do
            if(len<8) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
           !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
           !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)      &
           !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                        &
           !$omp private(n0,n1,n2,n3,n4,n5,n6,n7)                       &
           !$omp shared(i1,delta,len,alfa,gamma,n)
           do i=m1,len,8
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm4r4(i1,delta,a1,g1,n1)
             a2     = alfa(i+2)
             g2     = gamma(i+2)
             n2     = n(i+2)
             l(i+2) = refract_shift_xmm4r4(i1,delta,a2,g2,n2)
             a3     = alfa(i+3)
             g3     = gamma(i+3)
             n3     = n(i+3)
             l(i+3) = refract_shift_xmm4r4(i1,delta,a3,g3,n3)
             a4     = alfa(i+4)
             g4     = gamma(i+4)
             n4     = n(i+4)
             l(i+4) = refract_shift_xmm4r4(i1,delta,a4,g4,n4)
             a5     = alfa(i+5)
             g5     = gamma(i+5)
             n5     = n(i+5)
             l(i+5) = refract_shift_xmm4r4(i1,delta,a5,g5,n5)
             a6     = alfa(i+6)
             g6     = gamma(i+6)
             n6     = n(i+6)
             l(i+6) = refract_shift_xmm4r4(i1,delta,a6,g6,n6)
             a7     = alfa(i+7)
             g7     = gamma(i+7)
             n7     = n(i+7)
             l(i+7) = refract_shift_xmm4r4(i1,delta,a7,g7,n7)
          end do
          !$omp end parallel do
      end subroutine refract_shift_unroll_8x_omp_xmm4r4


      subroutine refract_shift_unroll_4x_xmm4r4(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_4x_xmm4r4
            !dir$ attributes forceinline ::  refract_shift_unroll_4x_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_4x_xmm4r4
            type(XMM4r4_t),                     intent(in) :: i1
            type(XMM4r4_t),                     intent(in) :: delta
            type(XMM4r4_t), dimension(1:len),   intent(in) :: alfa
            type(XMM4r4_t), dimension(1:len),   intent(in) :: gamma
            type(XMM4r4_t), dimension(1:len),   intent(in) :: n
            type(XMM4r4_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM4r4_t), automatic :: a0,a1,a2,a3
            !dir$ attributes align : 16:: a0,a1,a2,a3
            type(XMM4r4_t) , automatic :: g0,g1,g2,g3
            !dir$ attributes align : 16:: g0,g1,g2,g3
            type(XMM4r4_t), automatic :: n0,n1,n2,n3
            !dir$ attributes align : 16:: n0,n1,n2,n3
            integer(kind=i4) :: i,m,m1
            m = mod(len,4)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
            end do
            if(len<4) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
           do i=m1,len,4
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm4r4(i1,delta,a1,g1,n1)
             a2     = alfa(i+2)
             g2     = gamma(i+2)
             n2     = n(i+2)
             l(i+2) = refract_shift_xmm4r4(i1,delta,a2,g2,n2)
             a3     = alfa(i+3)
             g3     = gamma(i+3)
             n3     = n(i+3)
             l(i+3) = refract_shift_xmm4r4(i1,delta,a3,g3,n3)
          end do
      end subroutine refract_shift_unroll_4x_xmm4r4


      subroutine refract_shift_unroll_4x_omp_xmm4r4(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_4x_omp_xmm4r4
            !dir$ attributes forceinline ::  refract_shift_unroll_4x_omp_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_4x_omp_xmm4r4
            type(XMM4r4_t),                     intent(in) :: i1
            type(XMM4r4_t),                     intent(in) :: delta
            type(XMM4r4_t), dimension(1:len),   intent(in) :: alfa
            type(XMM4r4_t), dimension(1:len),   intent(in) :: gamma
            type(XMM4r4_t), dimension(1:len),   intent(in) :: n
            type(XMM4r4_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM4r4_t), automatic :: a0,a1,a2,a3
            !dir$ attributes align : 16:: a0,a1,a2,a3
            type(XMM4r4_t) , automatic :: g0,g1,g2,g3
            !dir$ attributes align : 16:: g0,g1,g2,g3
            type(XMM4r4_t), automatic :: n0,n1,n2,n3
            !dir$ attributes align : 16:: n0,n1,n2,n3
            integer(kind=i4) :: i,m,m1
            m = mod(len,4)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
            end do
            if(len<4) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
           !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
           !$omp firstprivate(m1) private(a0,a1,a2,a3)      &
           !$omp private(g0,g1,g2,g3)                        &
           !$omp private(n0,n1,n2,n3)                       &
           !$omp shared(i1,delta,len,alfa,gamma,n)
           do i=m1,len,4
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm4r4(i1,delta,a1,g1,n1)
             a2     = alfa(i+2)
             g2     = gamma(i+2)
             n2     = n(i+2)
             l(i+2) = refract_shift_xmm4r4(i1,delta,a2,g2,n2)
             a3     = alfa(i+3)
             g3     = gamma(i+3)
             n3     = n(i+3)
             l(i+3) = refract_shift_xmm4r4(i1,delta,a3,g3,n3)
          end do
          !$omp end parallel do
      end subroutine refract_shift_unroll_4x_omp_xmm4r4


      subroutine refract_shift_unroll_2x_xmm4r4(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_2x_xmm4r4
            !dir$ attributes forceinline ::  refract_shift_unroll_2x_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_2x_xmm4r4
            type(XMM4r4_t),                     intent(in) :: i1
            type(XMM4r4_t),                     intent(in) :: delta
            type(XMM4r4_t), dimension(1:len),   intent(in) :: alfa
            type(XMM4r4_t), dimension(1:len),   intent(in) :: gamma
            type(XMM4r4_t), dimension(1:len),   intent(in) :: n
            type(XMM4r4_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM4r4_t), automatic :: a0,a1
            !dir$ attributes align : 16:: a0,a1
            type(XMM4r4_t) , automatic :: g0,g1
            !dir$ attributes align : 16:: g0,g1
            type(XMM4r4_t), automatic :: n0,n1
            !dir$ attributes align : 16:: n0,n1
            integer(kind=i4) :: i,m,m1
            m = mod(len,2)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
            end do
            if(len<2) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
           do i=m1,len,2
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm4r4(i1,delta,a1,g1,n1)
          end do
      end subroutine refract_shift_unroll_2x_xmm4r4


      subroutine refract_shift_unroll_2x_omp_xmm4r4(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_2x_omp_xmm4r4
            !dir$ attributes forceinline ::  refract_shift_unroll_2x_omp_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_2x_omp_xmm4r4
            type(XMM4r4_t),                     intent(in) :: i1
            type(XMM4r4_t),                     intent(in) :: delta
            type(XMM4r4_t), dimension(1:len),   intent(in) :: alfa
            type(XMM4r4_t), dimension(1:len),   intent(in) :: gamma
            type(XMM4r4_t), dimension(1:len),   intent(in) :: n
            type(XMM4r4_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM4r4_t), automatic :: a0,a1
            !dir$ attributes align : 16:: a0,a1
            type(XMM4r4_t) , automatic :: g0,g1
            !dir$ attributes align : 16:: g0,g1
            type(XMM4r4_t), automatic :: n0,n1
            !dir$ attributes align : 16:: n0,n1
            integer(kind=i4) :: i,m,m1
            m = mod(len,2)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
            end do
            if(len<2) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
           !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
           !$omp firstprivate(m1) private(a0,a1,)      &
           !$omp private(g0,g1)                        &
           !$omp private(n0,n1)                       &
           !$omp shared(i1,delta,len,alfa,gamma,n)
           do i=m1,len,2
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm4r4(i1,delta,a1,g1,n1)
          end do
          !$omp end parallel do
      end subroutine refract_shift_unroll_2x_omp_xmm4r4


      subroutine refract_shift_rolled_xmm4r4(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_rolled_xmm4r4
            !dir$ attributes forceinline ::  refract_shift_rolled_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_rolled_xmm4r4
            type(XMM4r4_t),                     intent(in) :: i1
            type(XMM4r4_t),                     intent(in) :: delta
            type(XMM4r4_t), dimension(1:len),   intent(in) :: alfa
            type(XMM4r4_t), dimension(1:len),   intent(in) :: gamma
            type(XMM4r4_t), dimension(1:len),   intent(in) :: n
            type(XMM4r4_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM4r4_t), automatic :: a0
            !dir$ attributes align : 16:: a0
            type(XMM4r4_t) , automatic :: g0
            !dir$ attributes align : 16:: g0
            type(XMM4r4_t), automatic :: n0
            !dir$ attributes align : 16:: n0
            integer(kind=i4) :: i
           
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
           do i=1,len
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
           end do
      end subroutine refract_shift_rolled_xmm4r4


      subroutine refract_shift_rolled_omp_xmm4r4(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_rolled_omp_xmm4r4
            !dir$ attributes forceinline ::  refract_shift_rolled_omp_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_rolled_omp_xmm4r4
            type(XMM4r4_t),                     intent(in) :: i1
            type(XMM4r4_t),                     intent(in) :: delta
            type(XMM4r4_t), dimension(1:len),   intent(in) :: alfa
            type(XMM4r4_t), dimension(1:len),   intent(in) :: gamma
            type(XMM4r4_t), dimension(1:len),   intent(in) :: n
            type(XMM4r4_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM4r4_t), automatic :: a0
            !dir$ attributes align : 16:: a0
            type(XMM4r4_t) , automatic :: g0
            !dir$ attributes align : 16:: g0
            type(XMM4r4_t), automatic :: n0
            !dir$ attributes align : 16:: n0
            integer(kind=i4) :: i
         
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(4)
           !dir$ vector always
           !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
           !$omp private(a0)      &
           !$omp private(g0)                        &
           !$omp private(n0)                       &
           !$omp shared(i1,delta,len,alfa,gamma,n)
           do i=1,len
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm4r4(i1,delta,a0,g0,n0)
          end do
          !$omp end parallel do
      end subroutine refract_shift_rolled_omp_xmm4r4

      
      subroutine refract_shift_dispatch_xmm4r4(i1,delta,alfa,gamma,n,l,len,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: refract_shift_dispatch_xmm4r4
          type(XMM4r4_t),                     intent(in) :: i1
          type(XMM4r4_t),                     intent(in) :: delta
          type(XMM4r4_t), dimension(1:len),   intent(in) :: alfa
          type(XMM4r4_t), dimension(1:len),   intent(in) :: gamma
          type(XMM4r4_t), dimension(1:len),   intent(in) :: n
          type(XMM4r4_t), dimension(1:len),   intent(out):: l
          integer(kind=i4),                    intent(in) :: len 
          integer(kind=i4),                    intent(in) :: unroll_cnt
          logical(kind=i4),                    intent(in) :: omp_ver
          if(omp_ver) then
             select case (unroll_cnt)
                 case (16)
                    call refract_shift_unroll_16x_omp_xmm4r4(i1,delta,alfa,gamma,n,l,len)
                 case (8)
                    call refract_shift_unroll_8x_omp_xmm4r4(i1,delta,alfa,gamma,n,l,len)
                 case (4)
                    call refract_shift_unroll_4x_omp_xmm4r4(i1,delta,alfa,gamma,n,l,len)
                 case (2)
                    call refract_shift_unroll_2x_omp_xmm4r4(i1,delta,alfa,gamma,n,l,len)
                 case (0)
                    call refract_shift_rolled_omp_xmm4r4(i1,delta,alfa,gamma,n,l,len)
                 case default
                    return
             end select
          else
              select case (unroll_cnt)
                 case (16)
                    call refract_shift_unroll_16x_xmm4r4(i1,delta,alfa,gamma,n,l,len)
                 case (8)
                    call refract_shift_unroll_8x_xmm4r4(i1,delta,alfa,gamma,n,l,len)
                 case (4)
                    call refract_shift_unroll_4x_xmm4r4(i1,delta,alfa,gamma,n,l,len)
                 case (2)
                    call refract_shift_unroll_2x_xmm4r4(i1,delta,alfa,gamma,n,l,len)
                 case (0)
                    call refract_shift_rolled_xmm4r4(i1,delta,alfa,gamma,n,l,len)
                 case default
                    return
             end select
          end if
      end subroutine refract_shift_dispatch_xmm4r4


      
     pure function refract_shift_xmm2r8(i1,delta,alfa,gamma,n)  result(l)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_xmm2r8
            !dir$ attributes forceinline ::  refract_shift_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  refract_shift_xmm2r8
            use mod_fpcompare, only : xmm2r8_equalto_xmm2r8
            use mod_vectypes,  only : Mask2_t
            type(XMM2r8_t),   intent(in) :: i1
            type(XMM2r8_t),   intent(in) :: delta
            type(XMM2r8_t),   intent(in) :: alfa
            type(XMM2r8_t),   intent(in) :: gamma
            type(XMM2r8_t),   intent(in) :: n
            type(XMM2r8_t) :: l
            type(XMM2r8_t), parameter :: one = XMM2r8_t(1.0_dp)
            type(XMM2r8_t), parameter :: zer = XMM2r8_t(0.0_dp)
            type(XMM2r8_t), automatic :: ag,num,den,sin2,sag,t0,t1
            !dir$ attributes align : 16:: ag,num,den,sin2,sag,t0,t1
            type(Mask2_t), automatic :: m1,m2
            ag  = alfa.v-gamma.v
            m1  = xmm2r8_equalto_xmm2r8(i1,ag)
            m2  = xmm2r8_equalto_xmm2r8(alfa,zer) 
            if(all(m1)) then
               sag  = sin(ag.v)
               t0   = delta.v*sag.v
               sin2 = sag.v*sag.v
               num  = one.v-sag.v
               den  = n.v*n.v*sag.v
               t1   = one.v-sqrt(num.v/den.v)
               l    = t0.v*t1.v
            else if(all(m2)) then
               sag  = sin(gamma.v)
               t0   = -delta.v*sag.v
               sin2 = sag.v*sag.v
               num  = one.v-sin2.v
               den  = n.v*n.v-sin2.v
               t1   = one.v-sqrt(num.v/den.v)
               l    = t0.v*t1.v
            else
               sag  = sin(i1.v)
               t0   = delta.v*sag.v
               sin2 = sag.v*sag.v
               num  = one.v-sin2.v
               den  = n.v*n.v-sin2.v
               t1   = one.v-sqrt(num.v/den.v)
               l    = t0.v*t1.v
            end if
     end function refract_shift_xmm2r8  


     subroutine refract_shift_unroll_16x_xmm2r8(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_16x_xmm2r8
            !dir$ attributes forceinline ::  refract_shift_unroll_16x_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_16x_xmm2r8
            type(XMM2r8_t),                     intent(in) :: i1
            type(XMM2r8_t),                     intent(in) :: delta
            type(XMM2r8_t), dimension(1:len),   intent(in) :: alfa
            type(XMM2r8_t), dimension(1:len),   intent(in) :: gamma
            type(XMM2r8_t), dimension(1:len),   intent(in) :: n
            type(XMM2r8_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
            type(XMM2r8_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
            !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
            !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
            type(XMM2r8_t) , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
            type(XMM2r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
            !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
            !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
            type(XMM2r8_t), automatic :: n0,n1,n2,n3,n4,n5,n6,n7
            type(XMM2r8_t), automatic :: n8,n9,n10,n11,n12,n13,n14,n15
            !dir$ attributes align : 16:: n0,n1,n2,n3,n4,n5,n6,n7
            !dir$ attributes align : 16:: n8,n9,n10,n11,n12,n13,n14,n15
            integer(kind=i4) :: i,m,m1
            m = mod(len,16)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
            end do
            if(len<16) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
           do i=m1,len,16
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm2r8(i1,delta,a1,g1,n1)
             a2     = alfa(i+2)
             g2     = gamma(i+2)
             n2     = n(i+2)
             l(i+2) = refract_shift_xmm2r8(i1,delta,a2,g2,n2)
             a3     = alfa(i+3)
             g3     = gamma(i+3)
             n3     = n(i+3)
             l(i+3) = refract_shift_xmm2r8(i1,delta,a3,g3,n3)
             a4     = alfa(i+4)
             g4     = gamma(i+4)
             n4     = n(i+4)
             l(i+4) = refract_shift_xmm2r8(i1,delta,a4,g4,n4)
             a5     = alfa(i+5)
             g5     = gamma(i+5)
             n5     = n(i+5)
             l(i+5) = refract_shift_xmm2r8(i1,delta,a5,g5,n5)
             a6     = alfa(i+6)
             g6     = gamma(i+6)
             n6     = n(i+6)
             l(i+6) = refract_shift_xmm2r8(i1,delta,a6,g6,n6)
             a7     = alfa(i+7)
             g7     = gamma(i+7)
             n7     = n(i+7)
             l(i+7) = refract_shift_xmm2r8(i1,delta,a7,g7,n7)
             a8     = alfa(i+8)
             g8     = gamma(i+8)
             n8     = n(i+8)
             l(i+8) = refract_shift_xmm2r8(i1,delta,a8,g8,n8)
             a9     = alfa(i+9)
             g9     = gamma(i+9)
             n9     = n(i+9)
             l(i+9) = refract_shift_xmm2r8(i1,delta,a9,g9,n9)
             a10    = alfa(i+10)
             g10    = gamma(i+10)
             n10    = n(i+10)
             l(i+10)= refract_shift_xmm2r8(i1,delta,a10,g10,n10)
             a11    = alfa(i+11)
             g11    = gamma(i+11)
             n11    = n(i+11)
             l(i+11)= refract_shift_xmm2r8(i1,delta,a11,g11,n11)
             a12    = alfa(i+12)
             g12    = gamma(i+12)
             n12    = n(i+12)
             l(i+12)= refract_shift_xmm2r8(i1,delta,a12,g12,n12)
             a13    = alfa(i+13)
             g13    = gamma(i+13)
             n13    = n(i+13)
             l(i+13)= refract_shift_xmm2r8(i1,delta,a13,g13,n13)
             a14    = alfa(i+14)
             g14    = gamma(i+14)
             n14    = n(i+14)
             l(i+14)= refract_shift_xmm2r8(i1,delta,a14,g14,n14)
             a15    = alfa(i+15)
             g15    = gamma(i+15)
             n15    = n(i+15)
             l(i+15)= refract_shift_xmm2r8(i1,delta,a15,g15,n15)
         end do
      end subroutine refract_shift_unroll_16x_xmm2r8


      subroutine refract_shift_unroll_16x_omp_xmm2r8(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_16x_omp_xmm2r8
            !dir$ attributes forceinline ::  refract_shift_unroll_16x_omp_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_16x_omp_xmm2r8
            type(XMM2r8_t),                     intent(in) :: i1
            type(XMM2r8_t),                     intent(in) :: delta
            type(XMM2r8_t), dimension(1:len),   intent(in) :: alfa
            type(XMM2r8_t), dimension(1:len),   intent(in) :: gamma
            type(XMM2r8_t), dimension(1:len),   intent(in) :: n
            type(XMM2r8_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
            type(XMM2r8_t), automatic :: a8,a9,a10,a11,a12,a13,a14,a15
            !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
            !dir$ attributes align : 16:: a8,a9,a10,a11,a12,a13,a14,a15
            type(XMM2r8_t) , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
            type(XMM2r8_t), automatic :: g8,g9,g10,g11,g12,g13,g14,g15
            !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
            !dir$ attributes align : 16:: g8,g9,g10,g11,g12,g13,g14,g15
            type(XMM2r8_t), automatic :: n0,n1,n2,n3,n4,n5,n6,n7
            type(XMM2r8_t), automatic :: n8,n9,n10,n11,n12,n13,n14,n15
            !dir$ attributes align : 16:: n0,n1,n2,n3,n4,n5,n6,n7
            !dir$ attributes align : 16:: n8,n9,n10,n11,n12,n13,n14,n15
            integer(kind=i4) :: i,m,m1
            m = mod(len,16)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
            end do
            if(len<16) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
           !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
           !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)      &
           !$omp private(a8,a9,a10,a11,a12,a13,a14,a15)                 &
           !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                        &
           !$omp private(g8,g9,g10,g11,g12,g13,g14,g15)                 &
           !$omp private(n0,n1,n2,n3,n4,n5,n6,n7)                       &
           !$omp private(n8,n9,n10,n11,n12,n13,n14,n15)                 &
           !$omp shared(i1,delta,len,alfa,gamma,n)
           do i=m1,len,16
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm2r8(i1,delta,a1,g1,n1)
             a2     = alfa(i+2)
             g2     = gamma(i+2)
             n2     = n(i+2)
             l(i+2) = refract_shift_xmm2r8(i1,delta,a2,g2,n2)
             a3     = alfa(i+3)
             g3     = gamma(i+3)
             n3     = n(i+3)
             l(i+3) = refract_shift_xmm2r8(i1,delta,a3,g3,n3)
             a4     = alfa(i+4)
             g4     = gamma(i+4)
             n4     = n(i+4)
             l(i+4) = refract_shift_xmm2r8(i1,delta,a4,g4,n4)
             a5     = alfa(i+5)
             g5     = gamma(i+5)
             n5     = n(i+5)
             l(i+5) = refract_shift_xmm2r8(i1,delta,a5,g5,n5)
             a6     = alfa(i+6)
             g6     = gamma(i+6)
             n6     = n(i+6)
             l(i+6) = refract_shift_xmm2r8(i1,delta,a6,g6,n6)
             a7     = alfa(i+7)
             g7     = gamma(i+7)
             n7     = n(i+7)
             l(i+7) = refract_shift_xmm2r8(i1,delta,a7,g7,n7)
             a8     = alfa(i+8)
             g8     = gamma(i+8)
             n8     = n(i+8)
             l(i+8) = refract_shift_xmm2r8(i1,delta,a8,g8,n8)
             a9     = alfa(i+9)
             g9     = gamma(i+9)
             n9     = n(i+9)
             l(i+9) = refract_shift_xmm2r8(i1,delta,a9,g9,n9)
             a10    = alfa(i+10)
             g10    = gamma(i+10)
             n10    = n(i+10)
             l(i+10)= refract_shift_xmm2r8(i1,delta,a10,g10,n10)
             a11    = alfa(i+11)
             g11    = gamma(i+11)
             n11    = n(i+11)
             l(i+11)= refract_shift_xmm2r8(i1,delta,a11,g11,n11)
             a12    = alfa(i+12)
             g12    = gamma(i+12)
             n12    = n(i+12)
             l(i+12)= refract_shift_xmm2r8(i1,delta,a12,g12,n12)
             a13    = alfa(i+13)
             g13    = gamma(i+13)
             n13    = n(i+13)
             l(i+13)= refract_shift_xmm2r8(i1,delta,a13,g13,n13)
             a14    = alfa(i+14)
             g14    = gamma(i+14)
             n14    = n(i+14)
             l(i+14)= refract_shift_xmm2r8(i1,delta,a14,g14,n14)
             a15    = alfa(i+15)
             g15    = gamma(i+15)
             n15    = n(i+15)
             l(i+15)= refract_shift_xmm2r8(i1,delta,a15,g15,n15)
          end do
          !$omp end parallel do
      end subroutine refract_shift_unroll_16x_omp_xmm2r8


      subroutine refract_shift_unroll_8x_xmm2r8(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_8x_xmm2r8
            !dir$ attributes forceinline ::  refract_shift_unroll_8x_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_8x_xmm2r8
            type(XMM2r8_t),                     intent(in) :: i1
            type(XMM2r8_t),                     intent(in) :: delta
            type(XMM2r8_t), dimension(1:len),   intent(in) :: alfa
            type(XMM2r8_t), dimension(1:len),   intent(in) :: gamma
            type(XMM2r8_t), dimension(1:len),   intent(in) :: n
            type(XMM2r8_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
            !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
            type(XMM2r8_t) , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
            !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
            type(XMM2r8_t), automatic :: n0,n1,n2,n3,n4,n5,n6,n7
            !dir$ attributes align : 16:: n0,n1,n2,n3,n4,n5,n6,n7
            integer(kind=i4) :: i,m,m1
            m = mod(len,8)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
            end do
            if(len<8) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
           do i=m1,len,8
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm2r8(i1,delta,a1,g1,n1)
             a2     = alfa(i+2)
             g2     = gamma(i+2)
             n2     = n(i+2)
             l(i+2) = refract_shift_xmm2r8(i1,delta,a2,g2,n2)
             a3     = alfa(i+3)
             g3     = gamma(i+3)
             n3     = n(i+3)
             l(i+3) = refract_shift_xmm2r8(i1,delta,a3,g3,n3)
             a4     = alfa(i+4)
             g4     = gamma(i+4)
             n4     = n(i+4)
             l(i+4) = refract_shift_xmm2r8(i1,delta,a4,g4,n4)
             a5     = alfa(i+5)
             g5     = gamma(i+5)
             n5     = n(i+5)
             l(i+5) = refract_shift_xmm2r8(i1,delta,a5,g5,n5)
             a6     = alfa(i+6)
             g6     = gamma(i+6)
             n6     = n(i+6)
             l(i+6) = refract_shift_xmm2r8(i1,delta,a6,g6,n6)
             a7     = alfa(i+7)
             g7     = gamma(i+7)
             n7     = n(i+7)
             l(i+7) = refract_shift_xmm2r8(i1,delta,a7,g7,n7)
         end do
      end subroutine refract_shift_unroll_8x_xmm2r8


      subroutine refract_shift_unroll_8x_omp_xmm2r8(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_8x_omp_xmm2r8
            !dir$ attributes forceinline ::  refract_shift_unroll_8x_omp_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_8x_omp_xmm2r8
            type(XMM2r8_t),                     intent(in) :: i1
            type(XMM2r8_t),                     intent(in) :: delta
            type(XMM2r8_t), dimension(1:len),   intent(in) :: alfa
            type(XMM2r8_t), dimension(1:len),   intent(in) :: gamma
            type(XMM2r8_t), dimension(1:len),   intent(in) :: n
            type(XMM2r8_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM2r8_t), automatic :: a0,a1,a2,a3,a4,a5,a6,a7
            !dir$ attributes align : 16:: a0,a1,a2,a3,a4,a5,a6,a7
            type(XMM2r8_t) , automatic :: g0,g1,g2,g3,g4,g5,g6,g7
            !dir$ attributes align : 16:: g0,g1,g2,g3,g4,g5,g6,g7
            type(XMM2r8_t), automatic :: n0,n1,n2,n3,n4,n5,n6,n7
            !dir$ attributes align : 16:: n0,n1,n2,n3,n4,n5,n6,n7
            integer(kind=i4) :: i,m,m1
            m = mod(len,8)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
            end do
            if(len<8) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
           !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
           !$omp firstprivate(m1) private(a0,a1,a2,a3,a4,a5,a6,a7)      &
           !$omp private(g0,g1,g2,g3,g4,g5,g6,g7)                        &
           !$omp private(n0,n1,n2,n3,n4,n5,n6,n7)                       &
           !$omp shared(i1,delta,len,alfa,gamma,n)
           do i=m1,len,8
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm2r8(i1,delta,a1,g1,n1)
             a2     = alfa(i+2)
             g2     = gamma(i+2)
             n2     = n(i+2)
             l(i+2) = refract_shift_xmm2r8(i1,delta,a2,g2,n2)
             a3     = alfa(i+3)
             g3     = gamma(i+3)
             n3     = n(i+3)
             l(i+3) = refract_shift_xmm2r8(i1,delta,a3,g3,n3)
             a4     = alfa(i+4)
             g4     = gamma(i+4)
             n4     = n(i+4)
             l(i+4) = refract_shift_xmm2r8(i1,delta,a4,g4,n4)
             a5     = alfa(i+5)
             g5     = gamma(i+5)
             n5     = n(i+5)
             l(i+5) = refract_shift_xmm2r8(i1,delta,a5,g5,n5)
             a6     = alfa(i+6)
             g6     = gamma(i+6)
             n6     = n(i+6)
             l(i+6) = refract_shift_xmm2r8(i1,delta,a6,g6,n6)
             a7     = alfa(i+7)
             g7     = gamma(i+7)
             n7     = n(i+7)
             l(i+7) = refract_shift_xmm2r8(i1,delta,a7,g7,n7)
          end do
          !$omp end parallel do
      end subroutine refract_shift_unroll_8x_omp_xmm2r8


      subroutine refract_shift_unroll_4x_xmm2r8(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_4x_xmm2r8
            !dir$ attributes forceinline ::  refract_shift_unroll_4x_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_4x_xmm2r8
            type(XMM2r8_t),                     intent(in) :: i1
            type(XMM2r8_t),                     intent(in) :: delta
            type(XMM2r8_t), dimension(1:len),   intent(in) :: alfa
            type(XMM2r8_t), dimension(1:len),   intent(in) :: gamma
            type(XMM2r8_t), dimension(1:len),   intent(in) :: n
            type(XMM2r8_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM2r8_t), automatic :: a0,a1,a2,a3
            !dir$ attributes align : 16:: a0,a1,a2,a3
            type(XMM2r8_t) , automatic :: g0,g1,g2,g3
            !dir$ attributes align : 16:: g0,g1,g2,g3
            type(XMM2r8_t), automatic :: n0,n1,n2,n3
            !dir$ attributes align : 16:: n0,n1,n2,n3
            integer(kind=i4) :: i,m,m1
            m = mod(len,4)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
            end do
            if(len<4) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
           do i=m1,len,4
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm2r8(i1,delta,a1,g1,n1)
             a2     = alfa(i+2)
             g2     = gamma(i+2)
             n2     = n(i+2)
             l(i+2) = refract_shift_xmm2r8(i1,delta,a2,g2,n2)
             a3     = alfa(i+3)
             g3     = gamma(i+3)
             n3     = n(i+3)
             l(i+3) = refract_shift_xmm2r8(i1,delta,a3,g3,n3)
          end do
      end subroutine refract_shift_unroll_4x_xmm2r8


      subroutine refract_shift_unroll_4x_omp_xmm2r8(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_4x_omp_xmm2r8
            !dir$ attributes forceinline ::  refract_shift_unroll_4x_omp_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_4x_omp_xmm2r8
            type(XMM2r8_t),                     intent(in) :: i1
            type(XMM2r8_t),                     intent(in) :: delta
            type(XMM2r8_t), dimension(1:len),   intent(in) :: alfa
            type(XMM2r8_t), dimension(1:len),   intent(in) :: gamma
            type(XMM2r8_t), dimension(1:len),   intent(in) :: n
            type(XMM2r8_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM2r8_t), automatic :: a0,a1,a2,a3
            !dir$ attributes align : 16:: a0,a1,a2,a3
            type(XMM2r8_t) , automatic :: g0,g1,g2,g3
            !dir$ attributes align : 16:: g0,g1,g2,g3
            type(XMM2r8_t), automatic :: n0,n1,n2,n3
            !dir$ attributes align : 16:: n0,n1,n2,n3
            integer(kind=i4) :: i,m,m1
            m = mod(len,4)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
            end do
            if(len<4) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
           !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
           !$omp firstprivate(m1) private(a0,a1,a2,a3)      &
           !$omp private(g0,g1,g2,g3)                        &
           !$omp private(n0,n1,n2,n3)                       &
           !$omp shared(i1,delta,len,alfa,gamma,n)
           do i=m1,len,4
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm2r8(i1,delta,a1,g1,n1)
             a2     = alfa(i+2)
             g2     = gamma(i+2)
             n2     = n(i+2)
             l(i+2) = refract_shift_xmm2r8(i1,delta,a2,g2,n2)
             a3     = alfa(i+3)
             g3     = gamma(i+3)
             n3     = n(i+3)
             l(i+3) = refract_shift_xmm2r8(i1,delta,a3,g3,n3)
          end do
          !$omp end parallel do
      end subroutine refract_shift_unroll_4x_omp_xmm2r8


      subroutine refract_shift_unroll_2x_xmm2r8(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_2x_xmm2r8
            !dir$ attributes forceinline ::  refract_shift_unroll_2x_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_2x_xmm2r8
            type(XMM2r8_t),                     intent(in) :: i1
            type(XMM2r8_t),                     intent(in) :: delta
            type(XMM2r8_t), dimension(1:len),   intent(in) :: alfa
            type(XMM2r8_t), dimension(1:len),   intent(in) :: gamma
            type(XMM2r8_t), dimension(1:len),   intent(in) :: n
            type(XMM2r8_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM2r8_t), automatic :: a0,a1
            !dir$ attributes align : 16:: a0,a1
            type(XMM2r8_t) , automatic :: g0,g1
            !dir$ attributes align : 16:: g0,g1
            type(XMM2r8_t), automatic :: n0,n1
            !dir$ attributes align : 16:: n0,n1
            integer(kind=i4) :: i,m,m1
            m = mod(len,2)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
            end do
            if(len<2) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
           do i=m1,len,2
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm2r8(i1,delta,a1,g1,n1)
          end do
      end subroutine refract_shift_unroll_2x_xmm2r8


      subroutine refract_shift_unroll_2x_omp_xmm2r8(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_unroll_2x_omp_xmm2r8
            !dir$ attributes forceinline ::  refract_shift_unroll_2x_omp_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_unroll_2x_omp_xmm2r8
            type(XMM2r8_t),                     intent(in) :: i1
            type(XMM2r8_t),                     intent(in) :: delta
            type(XMM2r8_t), dimension(1:len),   intent(in) :: alfa
            type(XMM2r8_t), dimension(1:len),   intent(in) :: gamma
            type(XMM2r8_t), dimension(1:len),   intent(in) :: n
            type(XMM2r8_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM2r8_t), automatic :: a0,a1
            !dir$ attributes align : 16:: a0,a1
            type(XMM2r8_t) , automatic :: g0,g1
            !dir$ attributes align : 16:: g0,g1
            type(XMM2r8_t), automatic :: n0,n1
            !dir$ attributes align : 16:: n0,n1
            integer(kind=i4) :: i,m,m1
            m = mod(len,2)
            if(m /= 0) then
            do i=1,m
               a0     = alfa(i)
               g0     = gamma(i)
               n0     = n(i)
               l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
            end do
            if(len<2) return
            end if
           m1 = m+1
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
           !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
           !$omp firstprivate(m1) private(a0,a1,)      &
           !$omp private(g0,g1)                        &
           !$omp private(n0,n1)                       &
           !$omp shared(i1,delta,len,alfa,gamma,n)
           do i=m1,len,2
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
             a1     = alfa(i+1)
             g1     = gamma(i+1)
             n1     = n(i+1)
             l(i+1) = refract_shift_xmm2r8(i1,delta,a1,g1,n1)
          end do
          !$omp end parallel do
      end subroutine refract_shift_unroll_2x_omp_xmm2r8


      subroutine refract_shift_rolled_xmm2r8(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_rolled_xmm2r8
            !dir$ attributes forceinline ::  refract_shift_rolled_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_rolled_xmm2r8
            type(XMM2r8_t),                     intent(in) :: i1
            type(XMM2r8_t),                     intent(in) :: delta
            type(XMM2r8_t), dimension(1:len),   intent(in) :: alfa
            type(XMM2r8_t), dimension(1:len),   intent(in) :: gamma
            type(XMM2r8_t), dimension(1:len),   intent(in) :: n
            type(XMM2r8_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM2r8_t), automatic :: a0
            !dir$ attributes align : 16:: a0
            type(XMM2r8_t) , automatic :: g0
            !dir$ attributes align : 16:: g0
            type(XMM2r8_t), automatic :: n0
            !dir$ attributes align : 16:: n0
            integer(kind=i4) :: i
           
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
           do i=1,len
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
           end do
      end subroutine refract_shift_rolled_xmm2r8


      subroutine refract_shift_rolled_omp_xmm2r8(i1,delta,alfa,gamma,n,l,len)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 ::  refract_shift_rolled_omp_xmm2r8
            !dir$ attributes forceinline ::  refract_shift_rolled_omp_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" :: refract_shift_rolled_omp_xmm2r8
            type(XMM2r8_t),                     intent(in) :: i1
            type(XMM2r8_t),                     intent(in) :: delta
            type(XMM2r8_t), dimension(1:len),   intent(in) :: alfa
            type(XMM2r8_t), dimension(1:len),   intent(in) :: gamma
            type(XMM2r8_t), dimension(1:len),   intent(in) :: n
            type(XMM2r8_t), dimension(1:len),   intent(out):: l
            integer(kind=i4),                    intent(in) :: len
            type(XMM2r8_t), automatic :: a0
            !dir$ attributes align : 16:: a0
            type(XMM2r8_t) , automatic :: g0
            !dir$ attributes align : 16:: g0
            type(XMM2r8_t), automatic :: n0
            !dir$ attributes align : 16:: n0
            integer(kind=i4) :: i
         
           !dir$ assume_aligned alfa:16
           !dir$ assume_aligned gamma:16
           !dir$ assume_aligned n:16
           !dir$ assume_aligned l:16
           !dir$ vector aligned
           !dir$ ivdep
           !dir$ vector vectorlength(8)
           !dir$ vector always
           !$omp parallel do schedule(dynamic) default(none) if(n>=1024) &
           !$omp private(a0)      &
           !$omp private(g0)                        &
           !$omp private(n0)                       &
           !$omp shared(i1,delta,len,alfa,gamma,n)
           do i=1,len
             a0     = alfa(i)
             g0     = gamma(i)
             n0     = n(i)
             l(i)   = refract_shift_xmm2r8(i1,delta,a0,g0,n0)
          end do
          !$omp end parallel do
      end subroutine refract_shift_rolled_omp_xmm2r8

      
      subroutine refract_shift_dispatch_xmm2r8(i1,delta,alfa,gamma,n,l,len,unroll_cnt,omp_ver)
          !dir$ optimize:3
          !dir$ attributes code_align : 32 :: refract_shift_dispatch_xmm2r8
          type(XMM2r8_t),                     intent(in) :: i1
          type(XMM2r8_t),                     intent(in) :: delta
          type(XMM2r8_t), dimension(1:len),   intent(in) :: alfa
          type(XMM2r8_t), dimension(1:len),   intent(in) :: gamma
          type(XMM2r8_t), dimension(1:len),   intent(in) :: n
          type(XMM2r8_t), dimension(1:len),   intent(out):: l
          integer(kind=i4),                    intent(in) :: len 
          integer(kind=i4),                    intent(in) :: unroll_cnt
          logical(kind=i4),                    intent(in) :: omp_ver
          if(omp_ver) then
             select case (unroll_cnt)
                 case (16)
                    call refract_shift_unroll_16x_omp_xmm2r8(i1,delta,alfa,gamma,n,l,len)
                 case (8)
                    call refract_shift_unroll_8x_omp_xmm2r8(i1,delta,alfa,gamma,n,l,len)
                 case (4)
                    call refract_shift_unroll_4x_omp_xmm2r8(i1,delta,alfa,gamma,n,l,len)
                 case (2)
                    call refract_shift_unroll_2x_omp_xmm2r8(i1,delta,alfa,gamma,n,l,len)
                 case (0)
                    call refract_shift_rolled_omp_xmm2r8(i1,delta,alfa,gamma,n,l,len)
                 case default
                    return
             end select
          else
              select case (unroll_cnt)
                 case (16)
                    call refract_shift_unroll_16x_xmm2r8(i1,delta,alfa,gamma,n,l,len)
                 case (8)
                    call refract_shift_unroll_8x_xmm2r8(i1,delta,alfa,gamma,n,l,len)
                 case (4)
                    call refract_shift_unroll_4x_xmm2r8(i1,delta,alfa,gamma,n,l,len)
                 case (2)
                    call refract_shift_unroll_2x_xmm2r8(i1,delta,alfa,gamma,n,l,len)
                 case (0)
                    call refract_shift_rolled_xmm2r8(i1,delta,alfa,gamma,n,l,len)
                 case default
                    return
             end select
          end if
      end subroutine refract_shift_dispatch_xmm2r8
      
      
      !/////////////////////////////////////////////////////////////////////////////////////////!
      !/////////////////////////////////////////////////////////////////////////////////////////!
      
      
       !Formula 1, p. 108    
    
    
       
     subroutine project_xy_axis_xmm4r4(l,alpha,xl,yl)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  project_xy_axis_xmm4r4
         !dir$ attributes forceinline ::  project_xy_axis_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  project_xy_axis_xmm4r4
         type(XMM4r4_t),  intent(in) :: l
         type(XMM4r4_t),  intent(in) :: alpha
         type(XMM4r4_t),  intent(in) :: xl
         type(XMM4r4_t),  intent(in) :: yl
         type(XMM4r4_t), automatic :: absl
         !dir$ attributes align : 16:: absl
         absl = abs(l.v)
         xl   = absl.v*sin(alpha.v)
         yl   = absl.v*cos(alpha.v)
     end subroutine project_xy_axis_xmm4r4

     
     subroutine project_xy_axis_xmm2r8(l,alpha,xl,yl)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  project_xy_axis_xmm2r8
         !dir$ attributes forceinline ::  project_xy_axis_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  project_xy_axis_xmm2r8
         type(XMM2r8_t),  intent(in) :: l
         type(XMM2r8_t),  intent(in) :: alpha
         type(XMM2r8_t),  intent(in) :: xl
         type(XMM2r8_t),  intent(in) :: yl
         type(XMM2r8_t), automatic :: absl
         !dir$ attributes align : 16:: absl
         absl = abs(l.v)
         xl   = absl.v*sin(alpha.v)
         yl   = absl.v*cos(alpha.v)
     end subroutine project_xy_axis_xmm2r8 
     
     
     

   

     !AVX/AVX2 version
     pure function s_shift_xmm4r4(l,alpha,gamma) result(s)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  s_shift_xmm4r4
         !dir$ attributes forceinline ::  s_shift_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  s_shift_xmm4r4
         type(XMM4r4_t),  intent(in) :: l
         type(XMM4r4_t),  intent(in) :: alpha
         type(XMM4r4_t),  intent(in) :: gamma
         type(XMM4r4_t) :: s
         type(XMM4r4_t), automatic :: ag,sag
         !dir$ attributes align : 16:: ag,sag
         ag  = alpha.v-gamma.v
         sag = sin(ag.v)
         s   = l.v/sag.v
     end function s_shift_xmm4r4


     pure function s_shift_xmm2r8(l,alpha,gamma) result(s)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 ::  s_shift_xmm2r8
         !dir$ attributes forceinline ::  s_shift_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  s_shift_xmm2r8
         type(XMM2r8_t),  intent(in) :: l
         type(XMM2r8_t),  intent(in) :: alpha
         type(XMM2r8_t),  intent(in) :: gamma
         type(XMM2r8_t) :: s
         type(XMM2r8_t), automatic :: ag,sag
         !dir$ attributes align : 16:: ag,sag
         ag  = alpha.v-gamma.v
         sag = sin(ag.v)
         s   = l.v/sag.v
     end function s_shift_xmm2r8


      ! Проекции s на оси координат равны
      ! Formula 4, p. 108
    

      !AVX/AVX2 version
      subroutine project_s_xy_xmm4r4(s,gamma,xs,ys)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_s_xy_xmm4r4
         !dir$ attributes forceinline ::  project_s_xy_xmm4r4
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  project_s_xy_xmm4r4
         type(XMM4r4_t),   intent(in)  :: s
         type(XMM4r4_t),   intent(in)  :: gamma
         type(XMM4r4_t),   intent(out) :: xs
         type(XMM4r4_t),   intent(in)  :: ys
         xs = s.v*cos(gamma.v)
         ys = s.v*sin(gamma.v)
      end subroutine project_s_xy_xmm4r4 

      subroutine project_s_xy_xmm2r8(s,gamma,xs,ys)
         !dir$ optimize:3
         !dir$ attributes code_align : 32 :: project_s_xy_xmm2r8
         !dir$ attributes forceinline ::  project_s_xy_xmm2r8
         !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  project_s_xy_xmm2r8
         type(XMM2r8_t),   intent(in)  :: s
         type(XMM2r8_t),   intent(in)  :: gamma
         type(XMM2r8_t),   intent(out) :: xs
         type(XMM2r8_t),   intent(in)  :: ys
         xs = s.v*cos(gamma.v)
         ys = s.v*sin(gamma.v)
      end subroutine project_s_xy_xmm2r8




      ! что расстояния от начала координат О до точек
      ! пересечения лучей, образующих с горизонталью угла ±а, с 
      ! перпендикуляром к пластинке
      ! Formula 1, p. 110
     

     !AVX/AVX2 version
     pure function ray_intercept_pa_xmm4r4(delta,alpha,gamma,n) result(sp)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_pa_xmm4r4
           !dir$ attributes forceinline ::  ray_intercept_pa_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_intercept_pa_xmm4r4
           type(XMM4r4_t),  intent(in) :: delta
           type(XMM4r4_t),  intent(in) :: alpha
           type(XMM4r4_t),  intent(in) :: gamma
           type(XMM4r4_t),  intent(in) :: n
           type(XMM4r4_t) :: sp
           type(XMM4r4_t), parameter :: one = XMM4r4_t(1.0_sp)
           type(XMM4r4_t), automatic :: ag,num,den,n2,sag,sin2
           !dir$ attributes align : 16 :: ag,num,den,n2,sag,sin2
           ag  = abs(alpha.v)-gamma.v
           n2  = n.v*n.v
           num = cos(ag.v)
           sag = sin(ag.v)
           sin2= sag.v*sag.v
           den = sqrt(n2.v-sin2.v)
           sp  = delta.v*one.v-(num.v/den.v)
      end function ray_intercept_pa_xmm4r4

 
     pure function ray_intercept_pa_xmm2r8(delta,alpha,gamma,n) result(sp)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_pa_xmm2r8
           !dir$ attributes forceinline ::  ray_intercept_pa_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_intercept_pa_xmm2r8
           type(XMM2r8_t),  intent(in) :: delta
           type(XMM2r8_t),  intent(in) :: alpha
           type(XMM2r8_t),  intent(in) :: gamma
           type(XMM2r8_t),  intent(in) :: n
           type(XMM2r8_t) :: sp
           type(XMM2r8_t), parameter :: one = XMM2r8_t(1.0_dp)
           type(XMM2r8_t), automatic :: ag,num,den,n2,sag,sin2
           !dir$ attributes align : 16 :: ag,num,den,n2,sag,sin2
           ag  = abs(alpha.v)-gamma.v
           n2  = n.v*n.v
           num = cos(ag.v)
           sag = sin(ag.v)
           sin2= sag.v*sag.v
           den = sqrt(n2.v-sin2.v)
           sp  = delta.v*one.v-(num.v/den.v)
     end function ray_intercept_pa_xmm2r8


     


    
    pure function ray_intercept_na_xmm4r4(delta,alpha,gamma,n) result(sn)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_na_xmm4r4
           !dir$ attributes forceinline ::  ray_intercept_na_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_intercept_na_xmm4r4
           type(XMM4r4_t),  intent(in) :: delta
           type(XMM4r4_t),  intent(in) :: alpha
           type(XMM4r4_t),  intent(in) :: gamma
           type(XMM4r4_t),  intent(in) :: n
           type(XMM4r4_t) :: sn
           type(XMM4r4_t), parameter :: one = XMM4r4_t(1.0_sp)
           type(XMM4r4_t), automatic :: ag,num,den,n2,sag,sin2
           !dir$ attributes align : 16:: ag,num,den,n2,sag,sin2
           ag  = abs(alpha.v)+gamma.v
           n2  = n.v*n.v
           num = cos(ag.v)
           sag = sin(ag.v)
           sin2= sag.v*sag.v
           den = sqrt(n2.v-sin2.v)
           sn  = delta.v*one.v-(num.v/den.v)
    end function ray_intercept_na_xmm4r4
 
 
    pure function ray_intercept_na_xmm2r8(delta,alpha,gamma,n) result(sn)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_intercept_na_xmm2r8
           !dir$ attributes forceinline ::  ray_intercept_na_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_intercept_na_xmm2r8
           type(XMM2r8_t),  intent(in) :: delta
           type(XMM2r8_t),  intent(in) :: alpha
           type(XMM2r8_t),  intent(in) :: gamma
           type(XMM2r8_t),  intent(in) :: n
           type(XMM2r8_t) :: sn
           type(XMM2r8_t), parameter :: one = XMM2r8_t(1.0_dp)
           type(XMM2r8_t), automatic :: ag,num,den,n2,sag,sin2
           !dir$ attributes align : 16 :: ag,num,den,n2,sag,sin2
           ag  = abs(alpha.v)+gamma.v
           n2  = n.v*n.v
           num = cos(ag.v)
           sag = sin(ag.v)
           sin2= sag.v*sag.v
           den = sqrt(n2.v-sin2.v)
           sn  = delta.v*one.v-(num.v/den.v)
    end function ray_intercept_na_xmm2r8

   
    
       ! Formula 3, p. 110

  
     
      !AVX/AVX2 versions
      pure function ray_diff_xmm4r4(delta,alpha,gamma,n,u) result(ds)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_diff_xmm4r4
           !dir$ attributes forceinline ::  ray_diff_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_diff_xmm4r4
           use mod_fpcompare, only :  xmm4r4_rgt_xmm4r4
           type(XMM4r4_t),  intent(in) :: delta
           type(XMM4r4_t),  intent(in) :: alpha
           type(XMM4r4_t),  intent(in) :: gamma
           type(XMM4r4_t),  intent(in) :: n
           type(XMM4r4_t),  intent(in) :: u
           type(XMM4r4_t) :: ds
           type(XMM4r4_t), parameter :: two  = XMM4r4_t(2.0_sp)
           type(XMM4r4_t), parameter :: half = XMM4r4_t(0.5_sp)
           type(XMM4r4_t), automatic :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           !dir$ attributes align : 16:: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           type(Mask4_t), automatic :: m
           n   = n.v*n.v
           u2  = u.v*half.v
           u2g = u2.v-gamma.v
           t2  = sin(u2g.v)
           su2 = t2.v*t2.v
           m   = xmm4r4_rgt_xmm4r4(n2,su2)
           if(all(m)) then
              t3 = (-two.v*delta.v)/n.v
              t4 = sin(u2.v)
              t5 = sin(gamma.v)
              ds = t3.v*t4.v*t5.v
           else
              t0 = ray_intercept_pa_xmm4r4(delta,alpha,gamma,n)
              t1 = ray_intercept_na_xmm4r4(delta,alpha,gamma,n)
              ds = t0.v-t1.v
           end if
        end function ray_diff_xmm4r4


      pure function ray_diff_xmm2r8(delta,alpha,gamma,n,u) result(ds)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 :: ray_diff_xmm2r8
           !dir$ attributes forceinline ::  ray_diff_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  ray_diff_xmm2r8
           use mod_fpcompare, only :  xmm2r8_rgt_xmm2r8
           type(XMM2r8_t),  intent(in) :: delta
           type(XMM2r8_t),  intent(in) :: alpha
           type(XMM2r8_t),  intent(in) :: gamma
           type(XMM2r8_t),  intent(in) :: n
           type(XMM2r8_t),  intent(in) :: u
           type(XMM2r8_t) :: ds
           type(XMM2r8_t), parameter :: two  = XMM2r8_t(2.0_dp)
           type(XMM2r8_t), parameter :: half = XMM2r8_t(0.5_dp)
           type(XMM2r8_t), automatic :: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           !dir$ attributes align : 16:: t0,t1,u2,u2g,su2,sg,t2,n2,t3,t4,t5
           type(Mask2_t), automatic :: m
           n   = n.v*n.v
           u2  = u.v*half.v
           u2g = u2.v-gamma.v
           t2  = sin(u2g.v)
           su2 = t2.v*t2.v
           m   = xmm2r8_rgt_xmm2r8(n2,su2)
           if(all(m)) then
              t3 = (-two.v*delta.v)/n.v
              t4 = sin(u2.v)
              t5 = sin(gamma.v)
              ds = t3.v*t4.v*t5.v
           else
              t0 = ray_intercept_pa_xmm2r8(delta,alpha,gamma,n)
              t1 = ray_intercept_na_xmm2r8(delta,alpha,gamma,n)
              ds = t0.v-t1.v
           end if
      end function ray_diff_xmm2r8



        !Поле точек пересечения лучей, преломленных пластинкой,
        !относительно оси Ох (рис. 87) имеет симметрию, поэтому 
        !упростим обозначения и выполним расчет соответствующих 
        !координат на основании
        ! Formula 6,7, p. 111

       
     !AVX/AVX2 versions
      subroutine compute_dxdy_xmm4r4(alpha,beta,delta,gamma,n,u,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_dxdy_xmm4r4
            !dir$ attributes forceinline ::  compute_dxdy_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_dxdy_xmm4r4
            type(XMM4r4_t),   intent(in)  :: alpha
            type(XMM4r4_t),   intent(in)  :: beta
            type(XMM4r4_t),   intent(in)  :: delta
            type(XMM4r4_t),   intent(in)  :: gamma
            type(XMM4r4_t),   intent(in)  :: n
            type(XMM4r4_t),   intent(in)  :: u
            type(XMM4r4_t),   intent(out) :: dx
            type(XMM4r4_t),   intent(out) :: dy
            type(XMM4r4_t), parameter :: two = XMM4r4_t(2.0_sp)
            type(XMM4r4_t), automatic ::  ag,ds,t0,t1,t2
            ag  = alpha.v+gamma.v
            ds  = ray_diff_xmm4r4(delta,alfa,gamma,n,u)
            t0  = sin(ag.v)
            t1  = two.v*sin(alpha.v)
            t2  = two.v*cos(alpha.v)
            dx  = t0.v/t1.v*ds.v
            dy  = t0.v/t2.v*ds.v
      end subroutine compute_dxdy_xmm4r4


      subroutine compute_dxdy_xmm2r8(alpha,beta,delta,gamma,n,u,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_dxdy_xmm2r8
            !dir$ attributes forceinline ::  compute_dxdy_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_dxdy_xmm2r8
            type(XMM2r8_t),   intent(in)  :: alpha
            type(XMM2r8_t),   intent(in)  :: beta
            type(XMM2r8_t),   intent(in)  :: delta
            type(XMM2r8_t),   intent(in)  :: gamma
            type(XMM2r8_t),   intent(in)  :: n
            type(XMM2r8_t),   intent(in)  :: u
            type(XMM2r8_t),   intent(out) :: dx
            type(XMM2r8_t),   intent(out) :: dy
            type(XMM2r8_t), parameter :: two = XMM2r8_t(2.0_dp)
            type(XMM2r8_t), automatic ::  ag,ds,t0,t1,t2
            ag  = alpha.v+gamma.v
            ds  = ray_diff_xmm2r8(delta,alfa,gamma,n,u)
            t0  = sin(ag.v)
            t1  = two.v*sin(alpha.v)
            t2  = two.v*cos(alpha.v)
            dx  = t0.v/t1.v*ds.v
            dy  = t0.v/t2.v*ds.v
     end subroutine compute_dxdy_xmm2r8



    


    

    !AVX/AVX2 versions
     subroutine compute_xy_xmm4r4(alpha,beta,delta,gamma,n,u,x,y)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xy_xmm4r4
            !dir$ attributes forceinline ::  compute_xy_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_xy_xmm4r4
            type(XMM4r4_t),   intent(in)  :: alpha
            type(XMM4r4_t),   intent(in)  :: beta
            type(XMM4r4_t),   intent(in)  :: delta
            type(XMM4r4_t),   intent(in)  :: gamma
            type(XMM4r4_t),   intent(in)  :: n
            type(XMM4r4_t),   intent(in)  :: u
            type(XMM4r4_t),   intent(out) :: x
            type(XMM4r4_t),   intent(out) :: y
            type(XMM4r4_t), automatic :: sag,cag,pa,dx,dy,xs,ys
            !dir$ attributes align : 16:: sag,cag,pa,dx,dy,xs,ys
            sag  = sin(gamma.v)
            cag  = cos(gamma.v)
            pa   = ray_intercept_pa_xmm4r4(delta,alpha,gamma,n)
            xs   = pa.v*sag.v
            ys   = pa.v*cag.v
            call compute_dxdy_xmm4r4(alpha,beta,delta,gamma,n,u,dx,dy)
            x    = xs.v+dx.v
            y    = ys.v+dx.v
     end subroutine compute_xy_xmm4r4


     subroutine compute_xy_xmm2r8(alpha,beta,delta,gamma,n,u,x,y)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xy_xmm2r8
            !dir$ attributes forceinline ::  compute_xy_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_xy_xmm2r8
            type(XMM2r8_t),   intent(in)  :: alpha
            type(XMM2r8_t),   intent(in)  :: beta
            type(XMM2r8_t),   intent(in)  :: delta
            type(XMM2r8_t),   intent(in)  :: gamma
            type(XMM2r8_t),   intent(in)  :: n
            type(XMM2r8_t),   intent(in)  :: u
            type(XMM2r8_t),   intent(out) :: x
            type(XMM2r8_t),   intent(out) :: y
            type(XMM2r8_t), automatic :: sag,cag,pa,dx,dy,xs,ys
            !dir$ attributes align : 16:: sag,cag,pa,dx,dy,xs,ys
            sag  = sin(gamma.v)
            cag  = cos(gamma.v)
            pa   = ray_intercept_pa_xmm2r8(delta,alpha,gamma,n)
            xs   = pa.v*sag.v
            ys   = pa.v*cag.v
            call compute_dxdy_xmm2r8(alpha,beta,delta,gamma,n,u,dx,dy)
            x    = xs.v+dx.v
            y    = ys.v+dx.v
     end subroutine compute_xy_xmm2r8


    


     


     !AVX/AVX2 versions
     subroutine compute_xdyd_xmm4r4(gamma,u,n,xd,yd)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xdyd_xmm4r4
            !dir$ attributes forceinline ::  compute_xdyd_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_xdyd_xmm4r4
            type(XMM4r4_t),  intent(in)  :: gamma
            type(XMM4r4_t),  intent(in)  :: u
            type(XMM4r4_t),  intent(in)  :: n
            type(XMM4r4_t),  intent(out) :: xd
            type(XMM4r4_t),  intent(out) :: yd
            type(XMM4r4_t), parameter :: half = XMM4r4_t(0.5_sp)
            type(XMM4r4_t), parameter :: one  = XMM4r4_t(1.0_sp)
            type(XMM4r4_t), parameter :: four = XMM4r4_t(4.0_sp)
            type(XMM4r4_t), automatic :: cosg,sing,sin2s,sin2d,u2,u2gs,u2gd,n2,t0,t1,t2,t3,t4
            cosg = cos(gamma.v)
            sing = sin(gamma.v)
            u2   = u.v*half.v
            n2   = n.v*n.v
            u2gs = u2.v+gamma.v
            ungd = u2.v-gamma.v
            t0   = sin(u2gs.v)
            sin2s= t0.v*t0.v
            t1   = sin(u2gd.v)
            sin2d= t1.v*t1.v
            t2   = one.v/(four.v*sin(u2.v))
            t3   = sqrt(n2.v-sin2s.v)
            t0   = sin2s.v/t3.v
            t4   = sqrt(n2.v-sin2d.v)
            t1   = sin2d.v/t4.v
            dx   = cosg.v-t2.v*(t0.v+t1.v)
            t2   = one.v/(four.v*cos(u2.v)
            dy   = sing.v-t2.v*(t0.v-t1.v)
      end subroutine compute_xdyd_xmm4r4


      subroutine compute_xdyd_xmm2r8(gamma,u,n,xd,yd)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: compute_xdyd_xmm2r8
            !dir$ attributes forceinline ::  compute_xdyd_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  compute_xdyd_xmm2r8
            type(XMM2r8_t),  intent(in)  :: gamma
            type(XMM2r8_t),  intent(in)  :: u
            type(XMM2r8_t),  intent(in)  :: n
            type(XMM2r8_t),  intent(out) :: xd
            type(XMM2r8_t),  intent(out) :: yd
            type(XMM2r8_t), parameter :: half = XMM2r8_t(0.5_dp)
            type(XMM2r8_t), parameter :: one  = XMM2r8_t(1.0_dp)
            type(XMM2r8_t), parameter :: four = XMM2r8_t(4.0_dp)
            type(XMM2r8_t), automatic :: cosg,sing,sin2s,sin2d,u2,u2gs,u2gd,n2,t0,t1,t2,t3,t4
            cosg = cos(gamma.v)
            sing = sin(gamma.v)
            u2   = u.v*half.v
            n2   = n.v*n.v
            u2gs = u2.v+gamma.v
            ungd = u2.v-gamma.v
            t0   = sin(u2gs.v)
            sin2s= t0.v*t0.v
            t1   = sin(u2gd.v)
            sin2d= t1.v*t1.v
            t2   = one.v/(four.v*sin(u2.v))
            t3   = sqrt(n2.v-sin2s.v)
            t0   = sin2s.v/t3.v
            t4   = sqrt(n2.v-sin2d.v)
            t1   = sin2d.v/t4.v
            dx   = cosg.v-t2.v*(t0.v+t1.v)
            t2   = one.v/(four.v*cos(u2.v)
            dy   = sing.v-t2.v*(t0.v-t1.v)
     end subroutine compute_xdyd_xmm2r8


     !СКАНИРОВАНИЕ ВРАЩАЮЩИМИСЯ ОБЪЕКТИВАМИ
     !Formula 1, p. 121
    
       
        
    
     !AVX/AVX2 versions.
     subroutine fov_axay_xmm4r4(H,delx,dely,phi,ax,ay)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: fov_axay_xmm4r4
            !dir$ attributes forceinline ::  fov_axay_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  fov_axay_xmm4r4
            type(XMM4r4_t),  intent(in) :: H
            type(XMM4r4_t),  intent(in) :: delx
            type(XMM4r4_t),  intent(in) :: dely
            type(XMM4r4_t),  intent(in) :: phi
            type(XMM4r4_t),  intent(out):: ax
            type(XMM4r4_t),  intent(out):: ay
            type(XMM4r4_t),  parameter :: half = XMM4r4_t(0.5_sp)
            type(XMM4r4_t),  parameter :: one  = XMM4r4_t(1.0_sp)
            type(XMM4r4_t),  automatic :: sec2,phi2,t0,t1,sec
            phi2  = half.v*phi.v
            sec   = one.v/cos(phi2.v)
            sec2  = sec.v*sec.v
            ax    = H.v*delx.v*sec2.v
            ay    = H.v*dely.v*sec.v
      end subroutine fov_axay_xmm4r4
        
        
      subroutine fov_axay_xmm2r8(H,delx,dely,phi,ax,ay)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: fov_axay_xmm2r8
            !dir$ attributes forceinline ::  fov_axay_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  fov_axay_xmm2r8
            type(XMM2r8_t),  intent(in) :: H
            type(XMM2r8_t),  intent(in) :: delx
            type(XMM2r8_t),  intent(in) :: dely
            type(XMM2r8_t),  intent(in) :: phi
            type(XMM2r8_t),  intent(out):: ax
            type(XMM2r8_t),  intent(out):: ay
            type(XMM2r8_t),  parameter :: half = XMM2r8_t(0.5_dp)
            type(XMM2r8_t),  parameter :: one  = XMM2r8_t(1.0_dp)
            type(XMM2r8_t),  automatic :: sec2,phi2,t0,t1,sec
            phi2  = half.v*phi.v
            sec   = one.v/cos(phi2.v)
            sec2  = sec.v*sec.v
            ax    = H.v*delx.v*sec2.v
            ay    = H.v*dely.v*sec.v
     end subroutine fov_axay_xmm2r8


    

     !AVX/AVX2 versions
     subroutine  fov_dxdy_xmm4r4(x,y,F,phi,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: fov_dxdy_xmm4r4
            !dir$ attributes forceinline ::  fov_dxdy_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  fov_dxdy_xmm4r4
            type(YMM16r4_t),   intent(in) :: x
            type(YMM16r4_t),   intent(in) :: y
            type(YMM16r4_t),   intent(in) :: F
            type(YMM16r4_t),   intent(in) :: phi
            type(YMM16r4_t),   intent(out):: dx
            type(YMM16r4_t),   intent(out):: dy
            type(YMM16r4_t), parameter :: half = XMM4r4_t(0.5_sp)
            type(YMM16r4_t), automatic :: d0x,d0y,phi2
            !dir$ attributes align : 16:: d0x,d0y,phi2
            d0y    = y.v/F.v
            phi2   = half.v*phi.v
            d0x    = x.v/F.v
            dy     = d0y.v
            dx     = d0x.v*cos(phi2.v)
      end subroutine fov_dxdy_xmm4r4
        
        
      subroutine  fov_dxdy_xmm2r8(x,y,F,phi,dx,dy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: fov_dxdy_xmm2r8
            !dir$ attributes forceinline ::  fov_dxdy_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  fov_dxdy_xmm2r8
            type(YMM8r8_t),   intent(in) :: x
            type(YMM8r8_t),   intent(in) :: y
            type(YMM8r8_t),   intent(in) :: F
            type(YMM8r8_t),   intent(in) :: phi
            type(YMM8r8_t),   intent(out):: dx
            type(YMM8r8_t),   intent(out):: dy
            type(YMM8r8_t), parameter :: half = XMM2r8_t(0.5_dp)
            type(YMM8r8_t), automatic :: d0x,d0y,phi2
            !dir$ attributes align : 64 :: d0x,d0y,phi2
            d0y    = y.v/F.v
            phi2   = half.v*phi.v
            d0x    = x.v/F.v
            dy     = d0y.v
            dx     = d0x.v*cos(phi2.v)
     end subroutine fov_dxdy_xmm2r8

    
    

     !AVX/AVX2
     subroutine volt_impulse_uxuy_xmm4r4(u,om1,om2,t,ux,uy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: volt_impulse_uxuy_xmm4r4
            !dir$ attributes forceinline ::  volt_impulse_uxuy_xmm4r4
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  volt_impulse_uxuy_xmm4r4
            type(XMM4r4_t),   intent(in) :: u
            type(XMM4r4_t),   intent(in) :: om1
            type(XMM4r4_t),   intent(in) :: om2
            type(XMM4r4_t),   intent(in) :: t
            type(XMM4r4_t),   intent(out):: ux
            type(XMM4r4_t),   intent(out):: uy
            type(XMM4r4_t), automatic :: om1t,om2t,t0,t1
            om1t = om1.v*t.v
            om2t = om2.v*t.v
            t0   = sin(om1t.v)+sin(om2t.v)
            t1   = cos(om1t.v)+cos(om2t.v)
            ux   = u.v*t0.v
            uy   = u.v*t1.v
       end subroutine volt_impulse_uxuy_xmm4r4


       subroutine volt_impulse_uxuy_xmm2r8(u,om1,om2,t,ux,uy)
            !dir$ optimize:3
            !dir$ attributes code_align : 32 :: volt_impulse_uxuy_xmm2r8
            !dir$ attributes forceinline ::  volt_impulse_uxuy_xmm2r8
            !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  volt_impulse_uxuy_xmm2r8
            type(XMM2r8_t),   intent(in) :: u
            type(XMM2r8_t),   intent(in) :: om1
            type(XMM2r8_t),   intent(in) :: om2
            type(XMM2r8_t),   intent(in) :: t
            type(XMM2r8_t),   intent(out):: ux
            type(XMM2r8_t),   intent(out):: uy
            type(XMM2r8_t), automatic :: om1t,om2t,t0,t1
            om1t = om1.v*t.v
            om2t = om2.v*t.v
            t0   = sin(om1t.v)+sin(om2t.v)
            t1   = cos(om1t.v)+cos(om2t.v)
            ux   = u.v*t0.v
            uy   = u.v*t1.v
     end subroutine volt_impulse_uxuy_xmm2r8
     
     
     subroutine const_flux_spectr_unroll_16x_xmm4r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_16x_xmm4r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_16x_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_16x_xmm4r4
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(XMM4r4_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(XMM4r4_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(XMM4r4_t), parameter :: twopi = XMM4r4_t(6.283185307179586476925286766559_sp)
           type(YMM8r4_r), parameter :: half  = XMM4r4_t(0.5_sp)
           type(XMM4r4_t), parameter :: one   = XMM4r4_t(1.0_sp)
           type(XMM4r4_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 16:: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(XMM4r4_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           !dir$ attributes align : 16:: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(XMM4r4_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 16:: f0,f1,f2,f3,f4,f5,f6,f7
           type(XMM4r4_t) :: f8,f9,f10,f11,f12,f13,f14,f15
           !dir$ attributes align : 16:: f8,f9,f10,f11,f12,f13,f14,f15
           type(XMM4r4_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 16:: c0,c1,c2,c3,c4,c5,c6,c7
           type(XMM4r4_t) :: c8,c9,c10,c11,c12,c13,c14,c15
           !dir$ attributes align : 16:: c8,c9,c10,c11,c12,c13,c14,c15
           type(XMM4r4_t) :: sa0,sa1,sa2,sa3,sa4,sa5,sa6,sa7
           !dir$ attributes align : 16:: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           type(XMM4r4_t) :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           !dir$ attributes align : 16:: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           type(XMM4r4_t) :: hT,Phi0T
           !dir$ attributes align : 16:: hT,Phi0t
           real(kind=sp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<4) then
              return
           else if(n==4) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>4 .and. n<=64) then
              ! rolled version
              !dir$ assume_aligned Phi0f:16
              !dir$ assume_aligned freq:16
              do i=1,iand(n,not(3)),4
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>64) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:16
              !dir$ assume_aligned freq:16
              do i=1,iand(n,not(3)),64
                  call mm_prefetch(freq(i*64),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+4+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+4+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+8+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+8+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+12+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+12+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+16+ii)
                    c4.v(ii)       = twopi.v(ii)*f4.v(ii)*hT.v(ii)
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v(ii)-(f4.v(ii)*hT.v(ii)*f4.v(ii)*hT.v(ii))
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+20+ii)
                    c5.v(ii)       = twopi.v(ii)*f5.v(ii)*hT.v(ii)
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v(ii)-(f5.v(ii)*hT.v(ii)*f5.v(ii)*hT.v(ii))
                    Phi0f(i+20+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+24+ii)
                    c6.v(ii)       = twopi.v(ii)*f6.v(ii)*hT.v(ii)
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+28+ii)
                    c7.v(ii)       = twopi.v(ii)*f7.v(ii)*hT.v(ii)
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v(ii)-(f7.v(ii)*hT.v(ii)*f7.v(ii)*hT.v(ii))
                    Phi0f(i+28+ii) = Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                    f8.v(ii)       = freq(i+32+ii)
                    c8.v(ii)       = twopi.v(ii)*f8.v(ii)*hT.v(ii)
                    sa8.v(ii)      = sin(c8.v(ii))/c8.v(ii)
                    arg8.v(ii)     = one.v(ii)-(f8.v(ii)*hT.v(ii)*f8.v(ii)*hT.v(ii))
                    Phi0f(i+32+ii) = Phi0T.v(ii)*(sa8.v(ii)/arg8.v(ii))
                    f9.v(ii)       = freq(i+36+ii)
                    c9.v(ii)       = twopi.v(ii)*f9.v(ii)*hT.v(ii)
                    sa9.v(ii)      = sin(c9.v(ii))/c9.v(ii)
                    arg9.v(ii)     = one.v(ii)-(f9.v(ii)*hT.v(ii)*f9.v(ii)*hT.v(ii))
                    Phi0f(i+36+ii) = Phi0T.v(ii)*(sa9.v(ii)/arg9.v(ii)) 
                    f10.v(ii)      = freq(i+40+ii)
                    c10.v(ii)      = twopi.v(ii)*f10.v(ii)*hT.v(ii)
                    sa10.v(ii)     = sin(c10.v(ii))/c10.v(ii)
                    arg10.v(ii)    = one.v(ii)-(f10.v(ii)*hT.v(ii)*f10.v(ii)*hT.v(ii))
                    Phi0f(i+40+ii) = Phi0T.v(ii)*(sa10.v(ii)/arg10.v(ii))
                    f11.v(ii)      = freq(i+44+ii)
                    c11.v(ii)      = twopi.v(ii)*f11.v(ii)*hT.v(ii)
                    sa11.v(ii)     = sin(c11.v(ii))/c11.v(ii)
                    arg11.v(ii)    = one.v(ii)-(f11.v(ii)*hT.v(ii)*f11.v(ii)*hT.v(ii))
                    Phi0f(i+44+ii) = Phi0T.v(ii)*(sa11.v(ii)/arg11.v(ii))
                    f12.v(ii)      = freq(i+48+ii)
                    c12.v(ii)      = twopi.v(ii)*f12.v(ii)*hT.v(ii)
                    sa12.v(ii)     = sin(c12.v(ii))/c12.v(ii)
                    arg12.v(ii)    = one.v(ii)-(f12.v(ii)*hT.v(ii)*f12.v(ii)*hT.v(ii))
                    Phi0f(i+48+ii) = Phi0T.v(ii)*(sa12.v(ii)/arg12.v(ii))
                    f13.v(ii)      = freq(i+52+ii)
                    c13.v(ii)      = twopi.v(ii)*f13.v(ii)*hT.v(ii)
                    sa13.v(ii)     = sin(c13.v(ii))/c13.v(ii)
                    arg13.v(ii)    = one.v(ii)-(f13.v(ii)*hT.v(ii)*f13.v(ii)*hT.v(ii))
                    Phi0f(i+52+ii)= Phi0T.v(ii)*(sa13.v(ii)/arg13.v(ii))
                    f14.v(ii)      = freq(i+56+ii)
                    c14.v(ii)      = twopi.v(ii)*f14.v(ii)*hT.v(ii)
                    sa14.v(ii)     = sin(c14.v(ii))/c14.v(ii)
                    arg14.v(ii)    = one.v(ii)-(f14.v(ii)*hT.v(ii)*f14.v(ii)*hT.v(ii))
                    Phi0f(i+56+ii)= Phi0T.v(ii)*(sa14.v(ii)/arg14.v(ii))
                    f15.v(ii)      = freq(i+60+ii)
                    c15.v(ii)      = twopi.v(ii)*f15.v(ii)*hT.v(ii)
                    sa15.v(ii)     = sin(c15.v(ii))/c15.v(ii)
                    arg15.v(ii)    = one.v(ii)-(f15.v(ii)*hT.v(ii)*f15.v(ii)*hT.v(ii))
                    Phi0f(i+60+ii)= Phi0T.v(ii)*(sa15.v(ii)/arg15.v(ii))
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
       end subroutine const_flux_spectr_unroll_16x_xmm4r4
       
       
       subroutine const_flux_spectr_unroll_8x_xmm4r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_8x_xmm4r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_8x_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_8x_xmm4r4
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(XMM4r4_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(XMM4r4_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(XMM4r4_t), parameter :: twopi = XMM4r4_t(6.283185307179586476925286766559_sp)
           type(XMM4r4_t), parameter :: half  = XMM4r4_t(0.5_sp)
           type(XMM4r4_t), parameter :: one   = XMM4r4_t(1.0_sp)
           type(XMM4r4_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 16:: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(XMM4r4_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 16:: f0,f1,f2,f3,f4,f5,f6,f7
           type(XMM4r4_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 16:: c0,c1,c2,c3,c4,c5,c6,c7
           type(XMM4r4_t) :: sa0,sa1,sa2,sa3,sa4,sa5,sa6,sa7
           !dir$ attributes align : 16:: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           type(XMM4r4_t) :: hT,Phi0T
           !dir$ attributes align : 16:: hT,Phi0t
           real(kind=sp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<4) then
              return
           else if(n==4) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>4 .and. n<=32) then
              ! rolled version
              !dir$ assume_aligned Phi0f:16
              !dir$ assume_aligned freq:16
              do i=1,iand(n,not(3)),4
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>32) then
              ! 8x-unrolled version
              !dir$ assume_aligned Phi0f:16
              !dir$ assume_aligned freq:16
              do i=1,iand(n,not(3)),32
                  call mm_prefetch(freq(i*32),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+4+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+4+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+8+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+8+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+12+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+12+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+16+ii)
                    c4.v(ii)       = twopi.v(ii)*f4.v(ii)*hT.v(ii)
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v(ii)-(f4.v(ii)*hT.v(ii)*f4.v(ii)*hT.v(ii))
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+20+ii)
                    c5.v(ii)       = twopi.v(ii)*f5.v(ii)*hT.v(ii)
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v(ii)-(f5.v(ii)*hT.v(ii)*f5.v(ii)*hT.v(ii))
                    Phi0f(i+20+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+24+ii)
                    c6.v(ii)       = twopi.v(ii)*f6.v(ii)*hT.v(ii)
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+28+ii)
                    c7.v(ii)       = twopi.v(ii)*f7.v(ii)*hT.v(ii)
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v(ii)-(f7.v(ii)*hT.v(ii)*f7.v(ii)*hT.v(ii))
                    Phi0f(i+28+ii) = Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
              
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_8x_xmm4r4
       
       
       subroutine const_flux_spectr_unroll_4x_xmm4r4(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_4x_xmm4r4
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_4x_xmm4r4
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_4x_xmm4r4
           real(kind=sp), dimension(1:n), intent(out) :: Phi0f
           type(XMM4r4_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=sp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(XMM4r4_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(XMM4r4_t), parameter :: twopi = XMM4r4_t(6.283185307179586476925286766559_sp)
           type(XMM4r4_t), parameter :: half  = XMM4r4_t(0.5_sp)
           type(XMM4r4_t), parameter :: one   = XMM4r4_t(1.0_sp)
           type(XMM4r4_t) :: arg0,arg1,arg2,arg3
           !dir$ attributes align : 16:: arg0,arg1,arg2,arg3
           type(XMM4r4_t) :: f0,f1,f2,f3
           !dir$ attributes align : 16:: f0,f1,f2,f3
           type(XMM4r4_t) :: c0,c1,c2,c3
           !dir$ attributes align : 16:: c0,c1,c2,c3
           type(XMM4r4_t) :: sa0,sa1,sa2,sa3
           !dir$ attributes align : 16:: sa0,sa1,sa2,sa3
           type(XMM4r4_t) :: hT,Phi0T
           !dir$ attributes align : 16:: hT,Phi0t
           real(kind=sp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<4) then
              return
           else if(n==4) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>4 .and. n<=16) then
              ! rolled version
              !dir$ assume_aligned Phi0f:16
              !dir$ assume_aligned freq:16
              do i=1,iand(n,not(3)),4
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>16) then
              ! 8x-unrolled version
              !dir$ assume_aligned Phi0f:32
              !dir$ assume_aligned freq:32
              do i=1,iand(n,not(3)),16
                  call mm_prefetch(freq(i*16),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(4)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+4+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+4+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+8+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v-(f2.v*hT.v*f2.v*hT.v)
                    Phi0f(i+8+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+12+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+12+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                               
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=4,min=1,avg=2
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_4x_xmm4r4
       
       
       subroutine const_flux_spectr_unroll_16x_xmm2r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_16x_xmm2r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_16x_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_16x_xmm2r8
           real(kind=dp), dimension(1:n), intent(out) :: Phi0f
           type(XMM2r8_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=dp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(XMM2r8_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(XMM2r8_t), parameter :: twopi = XMM2r8_t(6.283185307179586476925286766559_dp)
           type(XMM2r8_t), parameter :: half  = XMM2r8_t(0.5_dp)
           type(XMM2r8_t), parameter :: one   = XMM2r8_t(1.0_dp)
           type(XMM2r8_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 16:: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(XMM2r8_t) :: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           !dir$ attributes align : 16:: arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15
           type(XMM2r8_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 16:: f0,f1,f2,f3,f4,f5,f6,f7
           type(XMM2r8_t) :: f8,f9,f10,f11,f12,f13,f14,f15
           !dir$ attributes align : 16:: f8,f9,f10,f11,f12,f13,f14,f15
           type(XMM2r8_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 16:: c0,c1,c2,c3,c4,c5,c6,c7
           type(XMM2r8_t) :: c8,c9,c10,c11,c12,c13,c14,c15
           !dir$ attributes align : 16:: c8,c9,c10,c11,c12,c13,c14,c15
           type(XMM2r8_t) :: sa0,sa1,sa2,sa3,sa4,sa5,sa6,sa7
           !dir$ attributes align : 16:: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           type(XMM2r8_t) :: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           !dir$ attributes align : 16:: sa8,sa9,sa10,sa11,sa12,sa13,sa14,sa15 
           type(XMM2r8_t) :: hT,Phi0T
           !dir$ attributes align : 16:: hT,Phi0t
           real(kind=dp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<2) then
              return
           else if(n==2) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>2 .and. n<=32) then
              ! rolled version
              !dir$ assume_aligned Phi0f:16
              !dir$ assume_aligned freq:16
              do i=1,iand(n,not(1)),2
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,1
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=2,min=1,avg=1
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>32) then
              ! 16x-unrolled version
              !dir$ assume_aligned Phi0f:16
              !dir$ assume_aligned freq:16
              do i=1,iand(n,not(1)),32
                  call mm_prefetch(freq(i*32),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,1
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+2+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+2+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+4+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v(ii)-(f2.v(ii)*hT.v(ii)*f2.v(ii)*hT.v(ii))
                    Phi0f(i+4+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+6+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+6+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+8+ii)
                    c4.v(ii)       = twopi.v(ii)*f4.v(ii)*hT.v(ii)
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v(ii)-(f4.v(ii)*hT.v(ii)*f4.v(ii)*hT.v(ii))
                    Phi0f(i+8+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+10+ii)
                    c5.v(ii)       = twopi.v(ii)*f5.v(ii)*hT.v(ii)
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v(ii)-(f5.v(ii)*hT.v(ii)*f5.v(ii)*hT.v(ii))
                    Phi0f(i+10+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+12+ii)
                    c6.v(ii)       = twopi.v(ii)*f6.v(ii)*hT.v(ii)
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+12+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+14+ii)
                    c7.v(ii)       = twopi.v(ii)*f7.v(ii)*hT.v(ii)
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v(ii)-(f7.v(ii)*hT.v(ii)*f7.v(ii)*hT.v(ii))
                    Phi0f(i+14+ii) = Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                    f8.v(ii)       = freq(i+16+ii)
                    c8.v(ii)       = twopi.v(ii)*f8.v(ii)*hT.v(ii)
                    sa8.v(ii)      = sin(c8.v(ii))/c8.v(ii)
                    arg8.v(ii)     = one.v(ii)-(f8.v(ii)*hT.v(ii)*f8.v(ii)*hT.v(ii))
                    Phi0f(i+16+ii) = Phi0T.v(ii)*(sa8.v(ii)/arg8.v(ii))
                    f9.v(ii)       = freq(i+18+ii)
                    c9.v(ii)       = twopi.v*f9.v*hT.v
                    sa9.v(ii)      = sin(c9.v(ii))/c9.v(ii)
                    arg9.v(ii)     = one.v-(f9.v*hT.v*f9.v*hT.v)
                    Phi0f(i+18+ii) = Phi0T.v(ii)*(sa9.v(ii)/arg9.v(ii)) 
                    f10.v(ii)      = freq(i+20+ii)
                    c10.v(ii)      = twopi.v(ii)*f10.v(ii)*hT.v(ii)
                    sa10.v(ii)     = sin(c10.v(ii))/c10.v(ii)
                    arg10.v(ii)    = one.v(ii)-(f10.v(ii)*hT.v(ii)*f10.v(ii)*hT.v(ii))
                    Phi0f(i+20+ii) = Phi0T.v(ii)*(sa10.v(ii)/arg10.v(ii))
                    f11.v(ii)      = freq(i+22+ii)
                    c11.v(ii)      = twopi.v(ii)*f11.v(ii)*hT.v(ii)
                    sa11.v(ii)     = sin(c11.v(ii))/c11.v(ii)
                    arg11.v(ii)    = one.v(ii)-(f11.v(ii)*hT.v(ii)*f11.v(ii)*hT.v(ii))
                    Phi0f(i+22+ii) = Phi0T.v(ii)*(sa11.v(ii)/arg11.v(ii))
                    f12.v(ii)      = freq(i+24+ii)
                    c12.v(ii)      = twopi.v(ii)*f12.v(ii)*hT.v(ii)
                    sa12.v(ii)     = sin(c12.v(ii))/c12.v(ii)
                    arg12.v(ii)    = one.v(ii)-(f12.v(ii)*hT.v(ii)*f12.v(ii)*hT.v(ii))
                    Phi0f(i+24+ii) = Phi0T.v(ii)*(sa12.v(ii)/arg12.v(ii))
                    f13.v(ii)      = freq(i+26+ii)
                    c13.v(ii)      = twopi.v(ii)*f13.v(ii)*hT.v(ii)
                    sa13.v(ii)     = sin(c13.v(ii))/c13.v(ii)
                    arg13.v(ii)    = one.v(ii)-(f13.v(ii)*hT.v(ii)*f13.v(ii)*hT.v(ii))
                    Phi0f(i+26+ii) = Phi0T.v(ii)*(sa13.v(ii)/arg13.v(ii))
                    f14.v(ii)      = freq(i+28+ii)
                    c14.v(ii)      = twopi.v(ii)*f14.v(ii)*hT.v(ii)
                    sa14.v(ii)     = sin(c14.v(ii))/c14.v(ii)
                    arg14.v(ii)    = one.v(ii)-(f14.v(ii)*hT.v(ii)*f14.v(ii)*hT.v(ii))
                    Phi0f(i+28+ii)= Phi0T.v(ii)*(sa14.v(ii)/arg14.v(ii))
                    f15.v(ii)      = freq(i+30+ii)
                    c15.v(ii)      = twopi.v(ii)*f15.v(ii)*hT.v(ii)
                    sa15.v(ii)     = sin(c15.v(ii))/c15.v(ii)
                    arg15.v(ii)    = one.v(ii)-(f15.v(ii)*hT.v(ii)*f15.v(ii)*hT.v(ii))
                    Phi0f(i+30+ii) = Phi0T.v(ii)*(sa15.v(ii)/arg15.v(ii))
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=2,min=1,avg=1
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_16x_xmm2r8
       
       
       subroutine const_flux_spectr_unroll_8x_xmm2r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_8x_xmm2r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_8x_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_8x_xmm2r8
           real(kind=dp), dimension(1:n), intent(out) :: Phi0f
           type(XMM2r8_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=dp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(XMM2r8_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(XMM2r8_t), parameter :: twopi = XMM2r8_t(6.283185307179586476925286766559_dp)
           type(YMM4r8_r), parameter :: half  = XMM2r8_t(0.5_dp)
           type(XMM2r8_t), parameter :: one   = XMM2r8_t(1.0_dp)
           type(XMM2r8_t) :: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           !dir$ attributes align : 16:: arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7
           type(XMM2r8_t) :: f0,f1,f2,f3,f4,f5,f6,f7
           !dir$ attributes align : 16:: f0,f1,f2,f3,f4,f5,f6,f7
           type(XMM2r8_t) :: c0,c1,c2,c3,c4,c5,c6,c7
           !dir$ attributes align : 16:: c0,c1,c2,c3,c4,c5,c6,c7
           type(XMM2r8_t) :: sa0,sa1,sa2,sa3,sa4,sa5,sa6,sa7
           !dir$ attributes align : 16:: sa0,sa1,sa2,sa3,sa4,sa,sa6,sa7
           type(XMM2r8_t) :: hT,Phi0T
           !dir$ attributes align : 16:: hT,Phi0t
           real(kind=dp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<2) then
              return
           else if(n==2) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>2 .and. n<=16) then
              ! rolled version
              !dir$ assume_aligned Phi0f:16
              !dir$ assume_aligned freq:16
              do i=1,iand(n,not(1)),2
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,3
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=2,min=1,avg=1
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>316) then
              ! 8x-unrolled version
              !dir$ assume_aligned Phi0f:16
              !dir$ assume_aligned freq:16
              do i=1,iand(n,not(1)),16
                  call mm_prefetch(freq(i*16),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,1
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+2+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+2+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+4+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v(ii)-(f2.v(ii)*hT.v(ii)*f2.v(ii)*hT.v(ii))
                    Phi0f(i+4+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+6+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+6+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                    f4.v(ii)       = freq(i+8+ii)
                    c4.v(ii)       = twopi.v(ii)*f4.v(ii)*hT.v(ii)
                    sa4.v(ii)      = sin(c4.v(ii))/c4.v(ii)
                    arg4.v(ii)     = one.v(ii)-(f4.v(ii)*hT.v(ii)*f4.v(ii)*hT.v(ii))
                    Phi0f(i+8+ii) = Phi0T.v(ii)*(sa4.v(ii)/arg4.v(ii))
                    f5.v(ii)       = freq(i+10+ii)
                    c5.v(ii)       = twopi.v(ii)*f5.v(ii)*hT.v(ii)
                    sa5.v(ii)      = sin(c5.v(ii))/c5.v(ii)
                    arg5.v(ii)     = one.v(ii)-(f5.v(ii)*hT.v(ii)*f5.v(ii)*hT.v(ii))
                    Phi0f(i+10+ii) = Phi0T.v(ii)*(sa5.v(ii)/arg5.v(ii))
                    f6.v(ii)       = freq(i+12+ii)
                    c6.v(ii)       = twopi.v(ii)*f6.v(ii)*hT.v(ii)
                    sa6.v(ii)      = sin(c6.v(ii))/c6.v(ii)
                    arg6.v(ii)     = one.v-(f6.v*hT.v*f6.v*hT.v)
                    Phi0f(i+12+ii) = Phi0T.v(ii)*(sa6.v(ii)/arg6.v(ii))
                    f7.v(ii)       = freq(i+14+ii)
                    c7.v(ii)       = twopi.v(ii)*f7.v(ii)*hT.v(ii)
                    sa7.v(ii)      = sin(c7.v(ii))/c7.v(ii)
                    arg7.v(ii)     = one.v(ii)-(f7.v(ii)*hT.v(ii)*f7.v(ii)*hT.v(ii))
                    Phi0f(i+14+ii) = Phi0T.v(ii)*(sa7.v(ii)/arg7.v(ii))
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=2,min=1,avg=1
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_8x_xmm2r8
       
       
       subroutine const_flux_spectr_unroll_4x_xmm2r8(Phi0f,Phi0,freq,n,T)
           !dir$ optimize:3
           !dir$ attributes code_align : 32 ::  const_flux_spectr_unroll_4x_xmm2r8
           !dir$ attributes forceinline ::   const_flux_spectr_unroll_4x_xmm2r8
           !dir$ attributes optimization_parameter:"target_arch=skylake-avx512" ::  const_flux_spectr_unroll_4x_xmm2r8
           real(kind=dp), dimension(1:n), intent(out) :: Phi0f
           type(XMM2r8_t),                intent(in)  :: Phi0   !vector enhanced i.e. broadcast
           real(kind=dp), dimension(1:n), intent(in)  :: freq
           integer(kind=i4),              intent(in)  :: n
           type(XMM2r8_t),                intent(in)  :: T      !vector enhanced i.e. broadcast
           type(XMM2r8_t), parameter :: twopi = XMM2r8_t(6.283185307179586476925286766559_dp)
           type(XMM2r8_t), parameter :: half  = XMM2r8_t(0.5_dp)
           type(XMM2r8_t), parameter :: one   = XMM2r8_t(1.0_dp)
           type(XMM2r8_t) :: arg0,arg1,arg2,arg3
           !dir$ attributes align : 16:: arg0,arg1,arg2,arg3
           type(XMM2r8_t) :: f0,f1,f2,f3
           !dir$ attributes align : 16:: f0,f1,f2,f3
           type(XMM2r8_t) :: c0,c1,c2,c3
           !dir$ attributes align : 16:: c0,c1,c2,c3
           type(XMM2r8_t) :: sa0,sa1,sa2,sa3
           !dir$ attributes align : 16:: sa0,sa1,sa2,sa3
           type(XMM2r8_t) :: hT,Phi0T
           !dir$ attributes align : 16:: hT,Phi0t
           real(kind=dp) :: f,c,sa,arg
           integer(kind=i4) :: i,ii,j
           hT.v    = half.v*T.v
           Phi0T.v = Phi0.v*T.v
           if(n<2) then
              return
           else if(n==2) then
              do i,n
                 f           = freq(i)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(i)    = Phi0T.v(0)*(sa/arg)
              end do
              return
           else if(n>2 .and. n<=8) then
              ! rolled version
              !dir$ assume_aligned Phi0f:16
              !dir$ assume_aligned freq:16
              do i=1,iand(n,not(1)),2
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,1
                    f0.v(ii)   = freq(i+ii)
                    c0.v(ii)   = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)  = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii) = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)= Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                 end do
              end do
              ! Remainder loop
              !dir$ loop_count max=2,min=1,avg=1
              do j=i,n
                 f           = freq(j)
                 c           = twopi.v(0)*f*hT.v(0)
                 sa          = sin(c)/c
                 arg         = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j)    = Phi0T.v(0)*(sa/arg)
              end do
              return
            else if(n>8) then
              ! 4-unrolled version
              !dir$ assume_aligned Phi0f:16
              !dir$ assume_aligned freq:16
              do i=1,iand(n,not(1)),8
                  call mm_prefetch(freq(i*8),FOR_K_PREFETCH_T1)
                 !dir$ vector aligned
                 !dir$ ivdep
                 !dir$ vector vectorlength(8)
                 !dir$ vector always
                 do ii=0,1
                    f0.v(ii)       = freq(i+ii)
                    c0.v(ii)       = twopi.v(ii)*f0.v(ii)*hT.v(ii)
                    sa0.v(ii)      = sin(c0.v(ii))/c0.v(ii)
                    arg0.v(ii)     = one.v(ii)-(f0.v(ii)*hT.v(ii)*f0.v(ii)*hT.v(ii))
                    Phi0f(i+ii)    = Phi0T.v(ii)*(sa0.v(ii)/arg0.v(ii))
                    f1.v(ii)       = freq(i+2+ii)
                    c1.v(ii)       = twopi.v(ii)*f1.v(ii)*hT.v(ii)
                    sa1.v(ii)      = sin(c1.v(ii))/c1.v(ii)
                    arg1.v(ii)     = one.v(ii)-(f1.v(ii)*hT.v(ii)*f1.v(ii)*hT.v(ii))
                    Phi0f(i+2+ii)  = Phi0T.v(ii)*(sa1.v(ii)/arg1.v(ii))
                    f2.v(ii)       = freq(i+4+ii)
                    c2.v(ii)       = twopi.v(ii)*f2.v(ii)*hT.v(ii)
                    sa2.v(ii)      = sin(c2.v(ii))/c2.v(ii)
                    arg2.v(ii)     = one.v(ii)-(f2.v(ii)*hT.v(ii)*f2.v(ii)*hT.v(ii))
                    Phi0f(i+4+ii) = Phi0T.v(ii)*(sa2.v(ii)/arg2.v(ii))
                    f3.v(ii)       = freq(i+6+ii)
                    c3.v(ii)       = twopi.v(ii)*f3.v(ii)*hT.v(ii)
                    sa3.v(ii)      = sin(c3.v(ii))/c3.v(ii)
                    arg3.v(ii)     = one.v(ii)-(f3.v(ii)*hT.v(ii)*f3.v(ii)*hT.v(ii))
                    Phi0f(i+6+ii) = Phi0T.v(ii)*(sa3.v(ii)/arg3.v(ii))
                  end do
               end do
              ! Remainder loop
              !dir$ loop_count max=2,min=1,avg=1
              do j=i,n
                 f        = freq(j)
                 c        = twopi.v(0)*f*hT.v(0)
                 sa       = sin(c)/c
                 arg      = one.v(0)-(f*hT.v(0)*f*hT.v(0)
                 Phi0f(j) = Phi0T.v(0)*(sa/arg)
              end do
              return
            end if
       end subroutine const_flux_spectr_unroll_4x_xmm2r8





  



     

     

    









 

    
 


    

      





 



































end module eos_sensor_sse
