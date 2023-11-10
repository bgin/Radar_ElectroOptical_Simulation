
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

    
 


    

      





 



































end module eos_sensor_sse
